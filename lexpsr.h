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
#include <iterator>
#include <variant>

#ifndef _LEXPARSER_CORE
#define _LEXPARSER_CORE lexpsr_core
#endif // ! _LEXPARSER_CORE

#ifndef  _LEXPARSER_SHELL
#define _LEXPARSER_SHELL lexpsr_shell
#endif // ! _LEXPARSER_SHELL

#ifndef LEXPSR_KEYWORD_PSR
#define LEXPSR_KEYWORD_PSR(var) _LEXPARSER_SHELL::Parser var(#var); _LEXPARSER_SHELL::_PsrForward(var)
#endif // ! LEXPSR_KEYWORD_PSR

#ifndef LEXPSR_KEYWORD_DECL_PSR
#define LEXPSR_KEYWORD_DECL_PSR(var) \
    _LEXPARSER_SHELL::Parser var(std::make_shared<_LEXPARSER_SHELL::Parser>(_LEXPARSER_SHELL::UnbindPsr()), #var); \
    _LEXPARSER_SHELL::_PsrForward(var)
#endif // ! LEXPSR_KEYWORD_DECL_PSR

#ifdef LEXPSR_SHORT_KEYWORDS
#ifndef psr
#define psr(var) LEXPSR_KEYWORD_PSR(var)
#endif // ! psr

#ifndef decl_psr
#define decl_psr(var) LEXPSR_KEYWORD_DECL_PSR(var)
#endif // ! decl_psr
#endif // LEXPSR_SHORT_KEYWORDS

namespace _LEXPARSER_SHELL { struct Parser; }

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

    class Context;
    struct ActionMaterial;
    class ActionScanner;

    enum class ScanState { OK = 0, Dismatch, Fatal }; // 进入引导词之后的失配即 Fatal

    typedef std::function<ScanState(const char*, std::size_t, std::size_t&, Context&, std::string&)> ScanFunc;
    typedef std::function<bool(const ActionMaterial&, Context& ctx, std::string& err)>               Action; // 识别动作

    struct ScanArgs {
        const char*  m_script = nullptr;
        std::size_t  m_length = 0;
        std::size_t& m_offset;
        Context&     m_contex;
        std::string& m_error_message;
    };

    struct ActionArgs {
        const ActionMaterial& m_action_material;
        Context&              m_contex;
        std::string&          m_error_message;
    };

    struct ActionMaterial {
#ifndef __clang__
        const  // 修饰 `StrRef m_token`。原因是 clang 的编译期检查过于严格，在 std::vector::resize() 中不会被通过，即便 vector 是只减不增
#endif // !__clang__
        StrRef               m_token;
        std::size_t          m_scanner_info = 0; // just for loop cnt
        const ActionScanner* m_action_scanner = nullptr;
    };

    struct CrimeScenes {
        enum : std::size_t { Invaild = ~static_cast<std::size_t>(0) };
        void Reset() noexcept { m_max = Invaild; }
        void OnRecord(std::size_t, std::size_t curr) noexcept {
            if (Invaild == m_max || curr > m_max) { m_max = curr; }
        }
        std::size_t m_max = Invaild;
    };

    class Context {
    public:
        union {
            std::size_t     m_scanner_info = 0;
            uint64_t        m_int_temp_in_lazy; // 仅在 lazy action 中使用
        };

        std::vector<ActionMaterial>  m_lazy_action;
        CrimeScenes                  m_crime_scenes;
    private:
        friend struct _LEXPARSER_SHELL::Parser;
        ScanFunc                     m_white_spaces;
        bool                         m_in_white_spaces = false;

    public:
        Context& Reset() {
            ResetLazyAction();
            m_crime_scenes.Reset();
            ClearWhiteSpaces();
            return *this;
        }

        Context& SetWhiteSpaces(const ScanFunc& white_spaces) {
            m_white_spaces = white_spaces;
            return *this;
        }

        Context& ClearWhiteSpaces() {
            SetWhiteSpaces(nullptr);
            return *this;
        }

        Context& ResetLazyAction() {
            m_lazy_action.clear();
            return *this;
        }

        std::size_t ErrorOffset() const { return m_crime_scenes.m_max; }
        std::string ErrorPrompts(const std::string& script) const {
            if (ErrorOffset() < script.size()) {
                auto format = [&script, this](bool lastline, std::size_t pos, std::size_t last, std::size_t line_num) {
                    const std::string line_content = script.substr(last, pos + 1u - last);
                    const std::size_t offset_in_line = ErrorOffset() - last;

                    const std::string line_num_str = " " + std::to_string(line_num);
                    return line_num_str + " | " + line_content + (lastline ? "\n" : "")
                        + std::string(line_num_str.size(), ' ') + " | " + std::string(offset_in_line, ' ') + "^";
                };
                std::size_t pos = 0;
                std::size_t last = 0;
                std::size_t line_num = 0;
                for (; pos < script.size(); pos = script.find('\n', pos), ++line_num) {
                    if (pos > ErrorOffset()) { // found
                        return format(false, pos, last, line_num);
                    }
                    last = ++pos;
                }

                if (std::string::npos == pos) { // last line
                    return format(true, pos, last, line_num);
                }
            }
            return {}; // not found
        }

        void GoBackToWithRecording(std::size_t& offset, std::size_t oldOffset) {
            m_crime_scenes.OnRecord(oldOffset, offset);
            GoBackTo(offset, oldOffset);
        }

        void GoBackTo(std::size_t& offset, std::size_t oldOffset) noexcept { offset = oldOffset; }

    private:
        const ScanFunc& IgnoreWhiteSpaces() const { return m_white_spaces; }
        bool InWhiteSpacesPsr() const { return m_in_white_spaces; }

        struct WhiteSpacesGuard {
            bool& m_white_spaces_guard;
            explicit WhiteSpacesGuard(bool& guard) : m_white_spaces_guard(guard) { m_white_spaces_guard = true; }
            ~WhiteSpacesGuard() { m_white_spaces_guard = false; }
        };

        WhiteSpacesGuard WhiteSpacesScopeGuard() { return WhiteSpacesGuard(m_in_white_spaces); }
    }; // class Context

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
                    ctx.GoBackToWithRecording(offset, oldOffset);
                    return ScanState::Dismatch;
                }

                if (ScanState::Fatal == ss) {
                    return ScanState::Fatal;
                }
            }

            ctx.m_scanner_info = m_member.size();
            return ScanState::OK; // 允许空串
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

                ctx.GoBackToWithRecording(offset, oldOffset);
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
                    ctx.GoBackToWithRecording(offset, oldOffset);
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
                if (ScanState::Dismatch == ss) {
                    ctx.GoBackTo(offset, oldOffset);
                    break;
                }
                else if (ScanState::Fatal == ss) {// @TODO 致命错误，也当失配处理？
                    return ScanState::Fatal;
                }
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

    // 三值布尔的逻辑非（第三态是 Fatal）。类似于负零宽断言: LogicNotScanner 不消耗字符，不产生任何副作用，区别于“负字符集”，后者是消耗字符的
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
                const std::size_t old_offset   = offset;
                const std::size_t old_lazy_cnt = ctx.m_lazy_action.size();
                ScanState ss = m_scanner(data, len, offset, ctx, err);
                switch (ss) {
                case ScanState::OK:       ss = ScanState::Dismatch;  break;
                case ScanState::Dismatch: ss = ScanState::OK;        break;
                default:                  ss = ScanState::Fatal;     break;
                }
                ctx.GoBackTo(offset, old_offset); // 不产生任何副作用
                ctx.m_lazy_action.resize(old_lazy_cnt);
                return ss;
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
                    ctx.GoBackToWithRecording(offset, old_offset);
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

        ScanFunc& Target() { assert(m_scanner); return *m_scanner; }
        const ScanFunc& Target() const { return const_cast<PreparedScanner*>(this)->Target(); }

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
        typedef details::Range Range; // [a, b] 左闭右闭区间

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

        explicit CharBranch(details::range_arg_t, const Range& range) { // range
            AddMember(range_v, range);
        }

        explicit CharBranch(details::range_arg_t, bool negative, const Range& range) { // negative_range
            assert(range.first <= range.second);
            if (0 < range.first) {
                AddMember(range_v, Range(0, range.first - 1));
            }
            if (range.second < (~static_cast<char>(0))) {
                AddMember(range_v, Range(range.second + 1, (~static_cast<char>(0))));
            }
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
        // 作为 lexpsr_shell::StrPsr 时，需要拿到它携带的数据 (Value)
        const std::string& Value() const { return m_tok; }

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

    typedef core::ScanFunc   Scanner;
    typedef core::ScanArgs   ScanArgs;

    typedef core::Action     Action;
    typedef core::ActionArgs ActionArgs;

    typedef core::ScanState  ScanState;

    namespace details {
        typedef std::pair<std::size_t, std::size_t> LoopCntPair;
        struct AtMost1 {};
        struct AnyCnt {};
        struct AtLeast1 {};

        using any_cnt_t    = LoopCntPair(*)(AnyCnt*);
        using at_least_1_t = LoopCntPair(*)(AtLeast1*);
        using at_most_1_t  = LoopCntPair(*)(AtMost1*);
    } // namespace details

    namespace { //  free function
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
        details::LoopCntPair at_most_1(details::AtMost1* = nullptr) { return at_most(1u); }
    } // namespace free function

    struct UnbindPsr {};
    using StrPsr           = core::Token;
    using SequencePsr      = core::Seq;
    using BranchPsr        = core::Branch;
    using LoopPsr          = core::Loop;
    using CharSetPsr       = core::CharBranch;
    using ActionPsr        = core::ActionScanner;
    using NextNotPsr       = core::LogicNotScanner;
    using FatalIfPsr       = core::FatalIf;
    using NopPsr           = core::Nop;

    struct LambdaPsr : std::function<Parser(const Parser&)> { // currying. for example, (Parser -> Parser) -> Parser == Parser -> (Parser -> Parser).
        typedef std::vector<std::shared_ptr<Parser>> Args;
#ifndef __clang__
        using std::function<Parser(const Parser&)>::function;
#else
        // clang BUG: https://github.com/llvm/llvm-project/issues/61443
        LambdaPsr() = default;
        LambdaPsr(const LambdaPsr&) = default;
        LambdaPsr(LambdaPsr&&) = default;
        LambdaPsr(const std::function<Parser(const Parser&)>& f) : std::function<Parser(const Parser&)>(f) {}
        LambdaPsr(std::function<Parser(const Parser&)>&& f) : std::function<Parser(const Parser&)>(std::move(f)) {}

        LambdaPsr& operator=(const LambdaPsr&) = default;
        LambdaPsr& operator=(LambdaPsr&&) = default;
#endif // !__clang__
        Args m_args; // 携带的实参，从该函数递归执行 apply
    };

    struct IntPsr {}; // 表示数字的 psr , 兼容丘奇数与立即数 @TODO
    using PreDeclPsr = std::shared_ptr<Parser>;

    struct Parser {
        typedef std::variant<UnbindPsr,
            StrPsr, CharSetPsr, SequencePsr, BranchPsr, LoopPsr, ActionPsr, NextNotPsr, FatalIfPsr, Scanner, NopPsr, LambdaPsr,
            PreDeclPsr
        > VariantParser;

    public:
        ScanState ScanScript(const char* script, std::size_t len, std::size_t& offset, core::Context& ctx, std::string& err) const {
            ScanState ret = (*this)(script, len, offset, ctx, err);
            if (ctx.IgnoreWhiteSpaces()) {
                ctx.IgnoreWhiteSpaces()(script, len, offset, ctx, err);
            }
            return ret;
        }

    public:
        explicit Parser(const VariantParser& expr, const std::string& name = std::string())
            : m_psr(expr), m_name(name)
        {}

        explicit Parser(const std::string& name)
            : m_name(name)
        {}

        Parser(const Parser&) = default;
        Parser(Parser&&) = default;

    public:
        void operator=(const std::string& expr) noexcept {
            unwrap() = StrPsr{ expr };
        }

        template <std::size_t N>
        void operator=(const char(&arr)[N]) noexcept {
            unwrap() = std::string(arr);
        }

        template <class T,
            class = typename std::enable_if<!std::is_same<UnbindPsr, T>::value>::type,
            class = std::void_t<decltype(std::declval<VariantParser>() = std::declval<T>())>>
        void operator=(const T& expr) noexcept {
            unwrap().m_psr = expr;
        }

        void operator=(const Parser& expr) noexcept {
            if (this != &expr) {
                assert(!m_name.empty());
                unwrap() = expr.m_psr;
                unwrap().m_slice_callback = expr.m_slice_callback;
            }
        }

        void operator=(Parser&& expr) noexcept {
            if (this != &expr) {
                assert(!m_name.empty());
                unwrap() = std::move(expr.m_psr);
                unwrap().m_slice_callback = std::move(expr.m_slice_callback);
            }
        }

        template <class... Psrs>
        Parser with_args(const Parser& arg, Psrs&&... rest) const { // 使得 LambdaPsr 携带实参
            assert(std::get_if<LambdaPsr>(&(unwrap().m_psr)));
            LambdaPsr::Args args{ std::make_shared<Parser>(arg), std::make_shared<Parser>(std::forward<Psrs>(rest))... };
            auto copy = std::get<LambdaPsr>(unwrap().m_psr);
            std::move(args.begin(), args.end(), std::back_inserter(copy.m_args));
            return Parser(copy, m_name);
        }
        const Parser& with_args() const { return *this; }

        template <class... Psrs>
        Parser apply(const Parser& arg, Psrs&&... rest) const {
            assert(std::get_if<LambdaPsr>(&(unwrap().m_psr)));
            return std::get<LambdaPsr>(unwrap().m_psr)(arg).apply(std::forward<Psrs>(rest)...);
        }
        const Parser& apply() const { return *this; }

        Parser slice_as_str_psr(Parser& out) const {
            assert(std::get_if<PreDeclPsr>(&out.m_psr)); // out 中的 shared 对象被 this.m_slice_callback 持有
            return with_slice_callback([out](core::StrRef tok) mutable {
                out = Parser(StrPsr{ tok.to_std_string() });
            });
        }

        template <class SliceHandler>
        Parser with_slice_callback(SliceHandler&& h) const {
            auto copy(*this);
            copy.m_slice_callback = std::forward<SliceHandler>(h);
            return copy;
        }

        // CharRangePsr helper function
        Parser operator()(const std::pair<char, char>& range) const
        {
            assert(std::get_if<CharSetPsr>(&unwrap().m_psr));
            auto copy = *this;
            std::get<CharSetPsr>(copy.unwrap().m_psr).AddMember(core::range_v, range);
            return copy;
        }

        // CharRangePsr helper function
        Parser operator()(char b, char e) const {
            return (*this)(std::make_pair(b, e));
        }

        // Make LoopPsr
        Parser operator[](details::LoopCntPair pair) const {
            return Parser(LoopPsr{ *this, pair.first, pair.second });
        }

#if (__cplusplus > 202300) || (defined(_MSC_VER) && (_MSC_VER > 1938))
        Parser operator[](std::size_t _min, std::size_t _max) const { // C++ 23
            return (*this)[details::LoopCntPair{ _min , _max }];
        }
#endif

        Parser operator[](details::at_most_1_t) const { return (*this)[at_most_1()]; }
        Parser operator[](details::any_cnt_t) const { return (*this)[any_cnt()]; }
        Parser operator[](details::at_least_1_t) const { return (*this)[at_least_1()]; }

        bool anonymous() const { return name().empty(); }
        const std::string& name() const { return m_name; }
        Parser& set_name(const std::string& name) {
            if (std::get_if<PreDeclPsr>(&m_psr)) { unwrap().m_name = name; }
            m_name = name; 
            return *this; 
        }

        ScanState operator()(const char* script, std::size_t len, std::size_t& offset, core::Context& ctx, std::string& err) const {
            ScanState ret = ScanState::Fatal;
            std::visit([&](auto&& _psr) -> ScanState {
                typedef typename std::decay<decltype(_psr)>::type P;
                if constexpr (std::is_same<P, UnbindPsr>::value) {
                    err = "UnbindPsr: ...";
                    assert(((void)0, false));
                    return (ret = ScanState::Fatal);
                }
                else if constexpr (std::is_same<P, LambdaPsr>::value) {
                    if (!_psr.m_args.empty()) { // 非空意味着当前 Lambda 携带实参，需执行 apply
                        Parser psr_fn = *this;  // copy
                        for (auto&& arg : _psr.m_args) {
                            assert(!!arg);
                            psr_fn = psr_fn.apply(*arg); // psr_fn.set_name(); 这里可以添加 psr 名字信息，以方便调试，比如 func_$0 func_$1 ...
                        }
                        return (ret = psr_fn(script, len, offset, ctx, err));
                    }
                    err =  (m_name.empty() ? m_name :  "`" + m_name + "` ") + "LambdaPsr: expect a parameter, but here a null is provided.";
                    assert(((void)0, false)); // 没有参数可以传递给该 lambda
                    return (ret = ScanState::Fatal);
                }
                else if constexpr (std::is_same<P, PreDeclPsr>::value) {
                    assert(nullptr != _psr);
                    return (ret = (*_psr)(script, len, offset, ctx, err));
                }
                else {
                    if (ctx.IgnoreWhiteSpaces() && (!ctx.InWhiteSpacesPsr())) {
                        auto guard = ctx.WhiteSpacesScopeGuard(); (void)guard; // 白字符不能递归下去
                        ctx.IgnoreWhiteSpaces()(script, len, offset, ctx, err);
                    }
                    std::size_t oldoffset = offset;
                    ret = _psr(script, len, offset, ctx, err);
                    if (m_slice_callback && ScanState::OK == ret) {
                        m_slice_callback(core::StrRef{script + oldoffset, script + offset}); // @TODO 暂时不考虑脚本分段情况
                    }
                    return ret;
                }
            }, m_psr);

            return ret;
        }

        Parser& unwrap() {
            if (std::get_if<PreDeclPsr>(&m_psr)) {
                auto&& inner = std::get<PreDeclPsr>(m_psr);
                assert(nullptr != inner);
                return *inner;
            }
            return *this;
        }

        const Parser& unwrap() const { return const_cast<Parser*>(this)->unwrap(); };

    public:
        // 吃掉一个白字符
        static ScanState EatWs(const char* script, std::size_t len, std::size_t& offset, 
#ifdef __clang__
            // clang BUG: https://github.com/llvm/llvm-project/issues/61444
            core::Context&, std::string&
#else
            ...
#endif // !__clang__
            ) noexcept {
            if (offset < len && (std::isspace((uint8_t)script[offset]))) {
                ++offset;
                return ScanState::OK;
            }
            return ScanState::Dismatch;
        }

        // 吃掉一批连续的白字符，总是成功
        static ScanState EatWss(const char* script, std::size_t len, std::size_t& offset, core::Context& ctx, std::string& err) noexcept {
            while (offset < len) {
                if (ScanState::OK != EatWs(script, len, offset, ctx, err)) { break; }
            }
            return ScanState::OK;
        }

    public:
        VariantParser                     m_psr;
        std::string                       m_name;
        std::function<void(core::StrRef)> m_slice_callback;
    }; // struct Parser

    namespace details {
        template <class T>
        static inline Parser _MakeSeqOrBranchPair(const Parser& a, const Parser& b) {
            if (a.anonymous()) { // 左结合的连接符，-，会导致 a 有可能也是个 SequenceExpr， 如果 a 是匿名的需展开
                const T* _a = std::get_if<T>(&a.m_psr);
                if (nullptr != _a) {
                    T copy = *_a; // copy 一份
                    return Parser(copy.AddMember({ b }));
                }
            }
            return Parser(T{ a, b });
        }

        // currying helper
        template <class, class = std::void_t<>>
        struct needs_unapply : std::true_type {};

        template <class T>
        struct needs_unapply<T, std::void_t<decltype(std::declval<T>()())>> : std::false_type {};

        typedef std::function<bool(const ActionArgs&)> action_wrap_t;
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

        // <<= 与 = 优先级相同，但是他们都是右结合
        [[maybe_unused]] Parser operator<<=(const std::string& str, const Action& action) {
            return Parser(ActionPsr{ Parser(StrPsr{str}), action });
        }

        template <class T>
        [[maybe_unused]] Parser operator<<=(const T& expr, const Action& action) {
            // 对与 T 类型的约束，Expr 的构造函数会出手
            return Parser(ActionPsr{ Parser(expr), action });
        }

        template <class T>
        [[maybe_unused]] Parser operator<<=(const T& expr, const details::action_wrap_t& action) {
            return expr <<= [action](const core::ActionMaterial& a, core::Context& c, std::string& e) -> bool {
                return action(ActionArgs{ a, c, e });
            };
        }

        [[maybe_unused]] Parser operator"" _psr(const char* str, std::size_t) {
            return Parser(StrPsr{ str });
        }

        // 字符集
        [[maybe_unused]] Parser set(const std::string& s) {
            return Parser(CharSetPsr(s));
        }

        // 负字符集
        [[maybe_unused]] Parser negative_set(const std::string& s) {
            return Parser(CharSetPsr(true, s));
        }

        [[maybe_unused]] Parser range(char b, char e) {
            return Parser(CharSetPsr(core::range_v, std::make_pair(b, e)));
        }

        [[maybe_unused]] Parser range(const std::pair<char, char>& pair) {
            return range(pair.first, pair.second);
        }

        [[maybe_unused]] Parser negative_range(char b, char e) {
            return Parser(CharSetPsr(core::range_v, true, std::make_pair(b, e)));
        }

        [[maybe_unused]] Parser negative_range(const std::pair<char, char>& pair) {
            return negative_range(pair.first, pair.second);
        }

        [[maybe_unused]] Parser next_not(const Parser& expr) { // 函数名添加 _ 前缀是为了避免与 not 操作符冲突
            return Parser(NextNotPsr(expr));
        }

        [[maybe_unused]] Parser fatal_if(const Parser& expr, const std::string& errMsg = "") {
            return Parser(FatalIfPsr(expr, errMsg));
        }

        [[maybe_unused]] Parser $(const Scanner& scan) {
            return Parser(scan);
        }

        [[maybe_unused]] Parser $(const std::function<ScanState(const ScanArgs&)>& scan) {
            return $([scan](const char* s, std::size_t l, std::size_t& o, core::Context& c, std::string& e) -> ScanState {
                return scan(ScanArgs{ s, l, o, c, e });
            });
        }

        template <class F>
        [[maybe_unused]] auto $curry(F&& f) {
            if constexpr (details::needs_unapply<decltype(f)>::value) {
                return Parser(LambdaPsr([=](auto&& x) {
                    return $curry(
                        [=](auto&&...xs) -> decltype(f(x, xs...)) {
                            return (f(x, xs...));
                        });
                }));
            }
            else {
                return f();
            }
        }

        [[maybe_unused]] const Parser  epsilon = Parser(NopPsr(), "epsilon");       // `epsilon` 永远成功，等价于 nop ???
        [[maybe_unused]] const Parser& nop     = epsilon;
        [[maybe_unused]] const Parser _false   = next_not(nop).set_name("_false");  // `_false`，永远失配
        [[maybe_unused]] const Parser ws(Scanner(Parser::EatWs), "ws");             // ws  吃掉一个白字符，失败返回失配
        [[maybe_unused]] const Parser wss(Scanner(Parser::EatWss), "wss");          // wss 吃掉一批连续的白字符，永远成功
        [[maybe_unused]] const Parser utf8bom(StrPsr{ "\xEF\xBB\xBF" }, "utf8bom"); // UTF-8 编码的 BOM 头，通常这样使用：(utf8bom | epsilon), 或 ignore_utf8bom
        [[maybe_unused]] const Parser ignore_utf8bom(utf8bom | epsilon);            // 可表达 UTF-8 BOM 头允许被忽略的 psr

        std::pair<bool, std::size_t> InvokeActions(core::Context& ctx, std::string& err) {
            std::size_t success_cnt = 0;
            const std::size_t sz = ctx.m_lazy_action.size();
            for (; success_cnt < sz; ++success_cnt) {
                auto&& f = ctx.m_lazy_action[success_cnt];
                if (!f.m_action_scanner->InvokeAction(f, ctx, err)) {
                    break;
                }
            }
            return std::make_pair(sz == success_cnt, success_cnt);
        }
    } // namespace  // free function

    template <class T> static inline T& _PsrForward(T& v) { return v; }
} // namespace _LEXPARSER_SHELL
