#pragma once

#include <functional>
#include <memory>
#include <string>
#include <vector>
#include <deque>
#include <cassert>
#include <cctype>
#include <bitset>
#include <cstring>
#include <algorithm>
#include <type_traits>
#include <variant>

#ifndef _LEXPARSER_CORE
#define _LEXPARSER_CORE lexpsr_core
#endif // ! _LEXPARSER_CORE

#ifndef  _LEXPARSER_SHELL
#define _LEXPARSER_SHELL lexpsr_shell
#endif // ! _LEXPARSER_SHELL

#ifndef psr
#define psr(var) Parser var(#var); _PsrForward(var)
#endif // ! psr

namespace _LEXPARSER_CORE {
    template <class... Args>
    static inline void _Unused(Args&&...) {}

    struct StrRef {
        const char* data = nullptr;
        std::size_t len = 0;

        StrRef() = default;

        StrRef(const char* d, std::size_t l) : data(d), len(l) {}

        template <class Iter>
        StrRef(Iter&& begin, Iter&& end) : data(static_cast<const char*>(&*begin)), len(std::distance(begin, end)) {}

        template <std::size_t N>
        StrRef(const char(&arr)[N]) : data(arr), len(N - 1) { static_assert(N != 0, "Error"); }

        char operator[](std::size_t index) const { return data[index]; } // may crash

        std::string to_std_string() const { return std::string(data, len); }
    }; // struct StrRef

    struct Context; 
    struct ActionMaterial;
    class ActionScanner;

    enum class ScanState {
        OK = 0, Dismatch, Fatal // 进入引导词之后的失配即 Fatal
    };

    typedef std::function<ScanState(const char*, std::size_t, std::size_t&, Context&, std::string&)> ScanFunc;
    typedef std::function<bool(const ActionMaterial&, Context& ctx, std::string& err)>               Action; // 识别动作

    struct ActionMaterial {
        const StrRef         m_token;
        std::size_t          m_scanner_info = 0; // just for loop cnt
        const ActionScanner* m_action_scanner = nullptr;
    };

    struct Context {
        union {
            std::size_t     m_scanner_info = 0;
            uint64_t        m_int_temp_in_lazy; // 仅在 lazy action 中使用
        };

        std::vector<ActionMaterial>  m_lazy_action;
    }; // struct Context

    namespace details {
        struct DefaultClass {
            DefaultClass() = default;
            DefaultClass(const DefaultClass&) = default;
            DefaultClass(DefaultClass&&) = default;
            DefaultClass& operator=(const DefaultClass&) = default;
            DefaultClass& operator=(DefaultClass&&) = default;
        };
    } // namespace details;

    class Seq : public details::DefaultClass {
    public:
        using details::DefaultClass::DefaultClass;
        explicit Seq(std::initializer_list<ScanFunc> member) : m_member(member) {}

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept {
            std::size_t oldOffset = offset;
            for (const ScanFunc& sf : m_member) {
                ScanState ss = sf(data, len, offset, ctx, err);
                if (ScanState::Dismatch == ss) {
                    offset = oldOffset;
                    return ScanState::Dismatch;
                }

                if (ScanState::Fatal == ss) {
                    return ScanState::Fatal;
                }
            }

            // 允许空串
            ctx.m_scanner_info = m_member.size();
            return ScanState::OK;
        }

        Seq& AddMember(std::initializer_list<ScanFunc> member) {
            m_member.insert(m_member.end(), member.begin(), member.end());
            return *this;
        }

    private:
        std::vector<ScanFunc>  m_member;
    }; // class Seq

    class Branch : public details::DefaultClass {
    public:
        using details::DefaultClass::DefaultClass;
        explicit Branch(std::initializer_list<ScanFunc> member) : m_member(member) {}

        Branch& AddMember(std::initializer_list<ScanFunc> member) {
            m_member.insert(m_member.end(), member.begin(), member.end());
            return *this;
        }

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept {
            if (m_member.empty()) { // 允许空串
                return ScanState::OK;
            }

            std::size_t oldOffset = offset;
            for (std::size_t i = 0; i < m_member.size(); ++i) {
                const ScanFunc& sf = m_member[i];
                ScanState ss = sf(data, len, offset, ctx, err);
                if (ScanState::Dismatch != ss) {
                    assert(ScanState::OK == ss || ScanState::Fatal == ss);
                    ctx.m_scanner_info = i;
                    return ss;
                }

                offset = oldOffset;
            }

            assert(oldOffset == offset);
            return ScanState::Dismatch;
        }

    private:
        std::vector<ScanFunc>  m_member;
    }; // class Branch

    class Loop : public details::DefaultClass {
    public:
        enum : std::size_t { INF_CNT = ~static_cast<std::size_t>(0) };

    public:
        using details::DefaultClass::DefaultClass;

        template <class Scanner>
        Loop(Scanner&& member, std::size_t min, std::size_t max) 
            : m_min(min), m_max(max), m_member(std::forward<Scanner>(member)) {
            assert(m_min <= m_max);
        }

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept {
            std::size_t oldOffset = offset;
            for (std::size_t i = 0; i < m_min; ++i) { // 必要条件： 满足最小循环次数
                ScanState ss = m_member(data, len, offset, ctx, err);
                if (ScanState::Dismatch == ss) {
                    offset = oldOffset;
                    return ScanState::Dismatch;
                }

                if (ScanState::Fatal == ss) {
                    return ScanState::Fatal;
                }
            }

            std::size_t loop_cnt = m_min;
            for (; loop_cnt < m_max; ++loop_cnt) {
                oldOffset = offset;
                ScanState ss = m_member(data, len, offset, ctx, err);
                if (ScanState::OK != ss) {
                    offset = oldOffset;
                    break;
                }
                // @TODO 致命错误，也当失配处理？
            }

            ctx.m_scanner_info = loop_cnt;
            return ScanState::OK;
        }

        Loop& AddMember(const ScanFunc& member, std::size_t _min, std::size_t _max) {
            m_min = _min;
            m_max = _max;
            assert(m_min <= m_max);
            m_member = member;
            return *this;
        }

    private:
        std::size_t m_min = 0;
        std::size_t m_max = 0;
        ScanFunc    m_member;
    }; // class Loop

    // 三值布尔的逻辑非（第三态是 Fatal）。类似于负零宽断言: LogicNotScanner 不消耗字符，区别于“负字符集”，后者是消耗字符的
    class LogicNotScanner : public details::DefaultClass {
    public:
        using details::DefaultClass::DefaultClass;

        template <class Scanner, class D = typename std::decay<Scanner>::type, 
            class = typename std::enable_if<!std::is_same<D, LogicNotScanner>::value>::type>
        explicit LogicNotScanner(Scanner&& scanner) : m_scanner(std::forward<Scanner>(scanner)) {}

        template <class Scanner>
        void AddMember(Scanner&& scanner) {
            m_scanner = std::forward<Scanner>(scanner);
        }

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept {
            if (m_scanner) {
                std::size_t old_offset = offset;
                ScanState ss = m_scanner(data, len, offset, ctx, err);
                switch (ss) {
                case ScanState::OK:
                    offset = old_offset;
                    return ScanState::Dismatch;
                case ScanState::Dismatch:
                    assert(offset == old_offset); // m_scanner 内部保证
                    offset = old_offset;
                    return ScanState::OK;
                default:
                    return ScanState::Fatal;
                }
            }

            err = "logic_error: LogicNotScanner is empty";
            return ScanState::Fatal;
        }

    private:
        ScanFunc m_scanner;
    }; // class LogicNotScanner

    // 一旦匹配成功即触发 Fatal; 没有 OK 状态（不允许出现该 Scanner, 出现即 Fatal）
    // 用于及早断言错误，同样不消耗字符。（比如进入一个无二义性的引导词之后的失配）
    class FatalIf {
    public:
        template <class Scanner>
        explicit FatalIf(Scanner&& scanner, const std::string& err_msg) : m_scanner(std::forward<Scanner>(scanner)), m_err_msg(err_msg) {}

        FatalIf(const FatalIf&) = default;
        FatalIf(FatalIf&&) = default;

        FatalIf& operator=(const FatalIf&) = default;
        FatalIf& operator=(FatalIf&&) = default;

        explicit FatalIf(const std::string& err_msg = "") : m_err_msg(err_msg) {}

        template <class Scanner>
        FatalIf& AddMember(Scanner&& scanner, const std::string& err_msg) {
            m_scanner = std::forward<Scanner>(scanner);
            m_err_msg = err_msg;
            return *this;
        }

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept {
            if (m_scanner) {
                std::size_t old_offset = offset;
                ScanState ss = m_scanner(data, len, offset, ctx, err);
                if (ScanState::OK == ss) {
                    err += m_err_msg;
                    offset = old_offset;
                    return ScanState::Fatal;
                }
                return ss;
            }
            err = "logic_error: FatalIf is empty";
            return ScanState::Fatal; // 无效 Scanner, 无条件 Fatal
        }

    private:
        ScanFunc    m_scanner; // cond
        std::string m_err_msg;
    }; // class FatalIf

    class ActionScanner : public details::DefaultClass {
    public:
        using details::DefaultClass::DefaultClass;

        template <class Scanner, class ActionType>
        ActionScanner(Scanner&& scanner, ActionType&& action)
            : m_scanner(std::forward<Scanner>(scanner)), m_action(std::forward<ActionType>(action))
        {}

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept {
            if (!m_scanner) { // 纯 Action
                const char* begin = data + offset;
                ctx.m_lazy_action.emplace_back(ActionMaterial{ StrRef{begin, begin}, ctx.m_scanner_info, this });
                return ScanState::OK;
            }

            std::size_t old_lazy_cnt = ctx.m_lazy_action.size();
            const char* begin = data + offset;
            ScanState ss = m_scanner(data, len, offset, ctx, err);
            if (ScanState::OK == ss) {
                const char* end = data + offset;
                ctx.m_lazy_action.emplace_back(ActionMaterial{ StrRef{ begin, end }, ctx.m_scanner_info, this });
            }
            else if (ScanState::Dismatch == ss) {
                ctx.m_lazy_action.resize(old_lazy_cnt);
            }

            return ss; // 原样抛出
        }

        bool InvokeAction(const ActionMaterial& am, Context& ctx, std::string& err) const noexcept {
            if (m_action) {
                return m_action(am, ctx, err);
            }
            return true; // 兼容 “空 Action”
        }

    private:
        ScanFunc    m_scanner;
        Action      m_action;
    }; // class ActionScanner

    class PreparedScanner {
    public:
        PreparedScanner() : m_scanner(std::make_shared<ScanFunc>()) {}

        PreparedScanner(const PreparedScanner&) = default;
        PreparedScanner(PreparedScanner&&) = default;

        PreparedScanner& operator=(const PreparedScanner&) = default;
        PreparedScanner& operator=(PreparedScanner&&) = default;

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept {
            assert(m_scanner);
            return (*m_scanner)(data, len, offset, ctx, err);
        }

        template <class T, class... Args>
        void ReshapePlacement(Args&&... args) {
            assert(m_scanner);
            *m_scanner = T(std::forward<Args>(args)...);
        }

        template <class T>
        void Reshape(T&& scanner) {
            assert(m_scanner);
            *m_scanner = std::forward<T>(scanner);
        }

    private:
        std::shared_ptr<ScanFunc>   m_scanner;
    }; // class PreparedScanner

    struct AtMost1 : Loop {
        AtMost1() = default;
        AtMost1(const AtMost1&) = default;
        AtMost1(AtMost1&&) = default;
        explicit AtMost1(const ScanFunc& member) : Loop(member, 0, 1u) {}

        AtMost1& operator=(const AtMost1&) = default;
        AtMost1& operator=(AtMost1&&) = default;
        
        AtMost1& AddMember(const ScanFunc& member) {
            Loop::AddMember(member, 0, 1u); 
            return *this;
        }
    }; // struct AtMost1

    namespace details {
        typedef std::pair<char, char> Range;
        using  range_arg_t = void(*)(Range*);
    } // namespace details

    namespace { void range_v(details::Range*) {} }

    class CharBranch : public details::DefaultClass {
    public:
        typedef details::Range Range;

    public:
        using details::DefaultClass::DefaultClass;

        explicit CharBranch(std::initializer_list<char> member) : CharBranch(false, member) {}   // set
        CharBranch(bool negative, std::initializer_list<char> member) {                          // set
            AddMember(member);
            if (negative) { // 负字符集
                m_bitset.flip();
            }
        }

        explicit CharBranch(const std::string& member) : CharBranch(false, member) {} // set

        CharBranch(bool negative, const std::string& member) {  // set
            AddMember(member);
            if (negative) { // 负字符集
                m_bitset.flip();
            }
        }

        explicit CharBranch(details::range_arg_t, const std::vector<Range>& ranges) { // range
            for (const Range& range : ranges) {
                AddMember(range_v, range);
            }
        }

        explicit CharBranch(details::range_arg_t, const Range& range)  { // range
            AddMember(range_v, range);
        }

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context&, std::string&) const noexcept {
            if (m_bitset.none()) { // 允许空串
                return ScanState::OK;
            }

            if (offset + 1u > len) { // 至少要求一个字符
                return ScanState::Dismatch;
            }

            uint8_t c = (uint8_t)(data[offset]);
            if (m_bitset.test(c)) {
                ++offset;
                // 没有分支 id 可以被回调
                return ScanState::OK;
            }

            return ScanState::Dismatch;
        }

        template <class Member>
        void AddMember(Member&& member) { // set
            for (char c : member) {
                uint8_t pos = static_cast<uint8_t>(c);
                m_bitset.set(pos, true);
            }
        }

        void AddMember(details::range_arg_t, const Range& range) {// range
            assert(range.first <= range.second);
            if (range.first < range.second) {
                for (int v = range.first; v < (int)(range.second) + 1; ++v) {
                    AddMember(std::initializer_list<char>{ char(v) });
                }
            }
            else if (range.first == range.second) {
                AddMember(std::initializer_list<char>{range.first});
            }
        }

    private:
        std::bitset<256u> m_bitset;
    }; // class CharBranch

    class Token {
    public:
        explicit Token(const std::string& tok) : m_tok(tok) {}
        Token(const Token&) = default;
        Token(Token&&) = default;
        Token& operator=(const Token&) = default;
        Token& operator=(Token&&) = default;

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context&, std::string&) const noexcept {
            if (m_tok.size() + offset > len) {
                return ScanState::Dismatch;
            }

            //if (!m_tok.empty()) // ??? nop ?
            {
                if (0 == std::memcmp(m_tok.data(), data + offset, m_tok.size())) {
                    offset += m_tok.size();
                    return ScanState::OK;
                }
            }

            return ScanState::Dismatch;
        }

    private:
        std::string m_tok;
    }; // class Token

    struct Nop { template <class... Args> ScanState operator()(Args&&...) const noexcept { return ScanState::OK; } };

    template <class S, class D>
    struct _Debugger {
        typedef _Debugger<S, D> _Myt;

        template <class Sc, class T>
        _Debugger(Sc&& s, T&& h) : m_scanner(std::forward<Sc>(s)), m_h(std::forward<T>(h)) {}

        _Debugger(const _Myt&) = default;
        _Debugger() = default;

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept {
            m_h(); // debug pointer
            return m_scanner(data, len, offset, ctx, err);
        }

        S  m_scanner;
        D  m_h;
    }; // struct _Debugger

    namespace
    {
        template <class Scanner, class Handler,
            class S = typename std::decay<Scanner>::type,
            class H = typename std::decay<Handler>::type>
        _Debugger<S, H> MakeDebugger(Scanner&& scanner, Handler&& h = []() {}) {
            return _Debugger<S, H>(std::forward<Scanner>(scanner), std::forward<Handler>(h));
        }

        Token operator"" _t(const char* str, std::size_t) { return Token(str); }
    }
} // namespace _LEXPARSER_CORE

////////////////////////////////
namespace _LEXPARSER_SHELL
{
    namespace core = _LEXPARSER_CORE;

    typedef core::ScanFunc  Scanner;
    typedef core::Action    Action;

    namespace details
    {
        typedef std::pair<std::size_t, std::size_t> LoopCntPair;
        struct AtMost1 {};
        struct AnyCnt {};
        struct AtLeast1 {};

        using any_cnt_t    = LoopCntPair(*)(AnyCnt*);
        using at_least_1_t = LoopCntPair(*)(AtLeast1*);
        using at_most_1_t  = LoopCntPair(*)(AtMost1*);
    } // namespace details

    namespace //  free function
    {
        details::LoopCntPair loop_cnt(std::size_t c) noexcept {
            return details::LoopCntPair{ c, c };
        }
        details::LoopCntPair loop_cnt(std::size_t m, std::size_t n) noexcept { 
            return details::LoopCntPair{ m, n }; 
        }
        details::LoopCntPair any_cnt(details::AnyCnt* = nullptr) {
            return details::LoopCntPair{ 0, ~std::size_t(0) }; 
        }
        details::LoopCntPair at_least(std::size_t n) noexcept {
            return details::LoopCntPair{ n, ~std::size_t(0) };
        }
        details::LoopCntPair at_least_1(details::AtLeast1* = nullptr) { return at_least(1u); }
        details::LoopCntPair at_most(std::size_t n) noexcept { return details::LoopCntPair{ 0, n }; }
        details::LoopCntPair at_most_1(details::AtMost1* = nullptr) { return at_most(1u);}
    } // namespace free function

    struct UnbindPsr {};
    using LiteralStringPsr = core::Token;
    using SequencePsr      = core::Seq;
    using BranchPsr        = core::Branch;
    using LoopPsr          = core::Loop;
    using CharSetPsr       = core::CharBranch;
    using NopPser          = core::Nop;
    using ActionPsr        = core::ActionScanner;
    using NotPsr           = core::LogicNotScanner;
    using FatalPsr         = core::FatalIf;
    using NopPsr           = core::Nop;

    struct Parser;
    struct LambdaPsr : std::function<Parser(const Parser&)> {
        using std::function<Parser(const Parser&)>::function;
        // curry : (Parser -> Parser) -> Parser == Parser -> (Parser -> Parser)
    };
    
    struct Parser {
        typedef std::variant<
            UnbindPsr, LiteralStringPsr, SequencePsr, BranchPsr, LoopPsr, 
            CharSetPsr, ActionPsr, NotPsr, FatalPsr, Scanner, NopPsr, LambdaPsr
        > VariantParser;

    public:
        explicit Parser(const VariantParser& expr, const std::string& name = std::string())
            : m_psr(expr), m_name(name)
        {}

        explicit Parser(const std::string& name)
            : m_name(name)
        {}

        Parser(const Parser&) = default;
        Parser(Parser&&) = default;
        //Parser& operator=(const Parser&) = default;
        Parser& operator=(Parser&&) = default;

    public:
        void operator=(const std::string& expr) noexcept {
            assert(std::get_if<UnbindPsr>(&m_psr));
            m_psr = LiteralStringPsr{ expr };
        }

        template <std::size_t N>
        void operator=(const char(&arr)[N]) noexcept {
            return (*this) = std::string(arr);
        }

        template <class T, 
            class = typename std::enable_if<!std::is_same<UnbindPsr, T>::value>::type,
            class = std::void_t<decltype(std::declval<VariantParser>() = std::declval<T>())>>
        void operator=(const T& expr) noexcept {
            m_psr = expr;
        }

        void operator=(const Parser& expr) noexcept {
            if (this != &expr) {
                assert(!m_name.empty());
                m_psr = expr.m_psr;
            }
        }

        template <class... Psrs>
        Parser apply(const Parser& arg, Psrs&&... rest) const {
            assert(std::get_if<LambdaPsr>(&m_psr));
            return std::get<LambdaPsr>(m_psr)(arg, std::forward<Psrs>(rest)...);
        }

        // CharRangePsr helper function
        Parser& operator()(const std::pair<char, char>& range)
        {
            assert(std::get_if<CharSetPsr>(&m_psr));
            std::get<CharSetPsr>(m_psr).AddMember(core::range_v, range);
            return *this;
        }

        // CharRangePsr helper function
        Parser& operator()(char b, char e) {
            return (*this)(std::make_pair(b, e));
        }

        // Make LoopPsr
        Parser operator[](details::LoopCntPair pair) const {
            return Parser(LoopPsr{ *this, pair.first, pair.second });
        }

#if (__cplusplus > 202002) || (defined(_MSC_VER) && (_MSC_VER > 1934))
        Parser operator[](std::size_t _min, std::size_t _max) const { // C++ 23
            return (*this)[details::LoopCntPair{ _min , _max }];
        }
#endif

        Parser operator[](details::at_most_1_t) const { return (*this)[at_most_1()]; }
        Parser operator[](details::any_cnt_t) const { return (*this)[any_cnt()]; }
        Parser operator[](details::at_least_1_t) const { return (*this)[at_least_1()]; }

        bool Anonymous() const { return m_name.empty(); }

        core::ScanState operator()(const char*, std::size_t, std::size_t&, core::Context&, std::string&) const {
            return core::ScanState::OK;
        }

    public:
        VariantParser  m_psr;
        std::string    m_name;
    }; // struct Parser

    namespace details {
        template <class T, class Parser>
        static inline Parser _MakeSeqOrBranchPair(const Parser& a, const Parser& b) {
            if (a.Anonymous()) {
                // 左结合的连接符，-，会导致 a 有可能也是个 SequenceExpr， 如果 a 是匿名的需展开
                const T* _a = std::get_if<T>(&a.m_psr);
                if (nullptr != _a) {
                    T copy = *_a; // copy 一份
                    return Parser(copy.AddMember({ b }));
                }
            }

            return Parser(T { a, b });
        }

        // currying helper
        template <class, class = std::void_t<>>
        struct needs_unapply : std::true_type {};

        template <class T>
        struct needs_unapply<T, std::void_t<decltype(std::declval<T>()())>> : std::false_type {};
    } // namespace details

    namespace { // free function
        [[maybe_unused]] Parser operator-(const Parser& a, const Parser& b) {
            return (details::_MakeSeqOrBranchPair<SequencePsr>(a, b)); // _LXP_SEQUENCE_CONCATENATION_CHARACTER
        }

        [[maybe_unused]] Parser operator, (const Parser& a, const Parser& b) {
            return (a - b); // 序列连接兼容 ‘-’ 与 ‘,’ 只是它们的优先级不同
        }

        // | 也是与 , 类似
        [[maybe_unused]] Parser operator|(const Parser& a, const Parser& b) {
            return details::_MakeSeqOrBranchPair<BranchPsr>(a, b);
        }

        [[maybe_unused]] Parser operator<<(const std::string& str, const Action& action) {
            return Parser(ActionPsr{ Parser(LiteralStringPsr{str}), action });
        }

        template <class T>
        [[maybe_unused]] Parser operator<<(const T& expr, const Action& action) { 
            // 对与 T 类型的约束，Expr 的构造函数会出手
            return Parser(ActionPsr{ Parser(expr), action });
        }

        [[maybe_unused]] Parser operator"" _psr(const char* str, std::size_t) {
            return Parser(LiteralStringPsr{ str });
        }

        [[maybe_unused]] Parser set(const std::string& s) {
            return Parser(CharSetPsr(s));
        }

        [[maybe_unused]] Parser range(char b, char e) {
            return Parser(CharSetPsr(core::range_v, std::make_pair(b, e)));
        }

        [[maybe_unused]] Parser range(const std::pair<char, char>& pair) {
            return range(pair.first, pair.second);
        }

        [[maybe_unused]] Parser _not(const Parser& expr) { // 函数名添加 _ 前缀是为了避免与 not 操作符冲突
            return Parser(NotPsr(expr));
        }

        [[maybe_unused]] Parser fatal_if(const Parser& expr, const std::string& errMsg = "") {
            return Parser(FatalPsr(expr, errMsg));
        }

        // `nop` variable
        [[maybe_unused]] const Parser nop = Parser(NopPsr());

        [[maybe_unused]] Parser $(const Scanner& scan) {
            return Parser(scan);
        }

        template <class F>
        [[maybe_unused]] auto $curry(F&& f) {
            if constexpr (details::needs_unapply<decltype(f)>::value) {
                return Parser(LambdaPsr([=](auto&& x) {
                    return $curry(
                        [=](auto&&...xs) -> decltype(f(x, xs...)) {
                            return (f(x, xs...));
                        }
                    );
                }));
            }
            else {
                return f();
            }
        }
    } // namespace  // free function

    template <class T> static inline T& _PsrForward(T& v) { return v; }
} // namespace _LEXPARSER_SHELL
