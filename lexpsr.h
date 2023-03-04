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

namespace _LEXPARSER_CORE
{
    template <class... Args>
    static inline void _Unused(Args&&...) {}

    struct StrRef
    {
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

    class ActionScanner;

    struct ActionMaterial
    {
        const StrRef         m_token;
        std::size_t          m_scanner_info = 0; // just for loop cnt
        const ActionScanner* m_action_scanner = nullptr;
    };

    struct Context
    {
    public:
        union {
            std::size_t     m_scanner_info = 0;
            uint64_t        m_int_temp_in_lazy; // 仅在 lazy action 中使用
        };

        std::vector<ActionMaterial>  m_lazy_action;

    public:
        virtual ~Context() {}
    }; // struct Context


    enum class ScanState
    {
        OK = 0, Dismatch, Fatal // 进入引导词之后的失配即 Fatal
    };

    typedef std::function<ScanState(const char*, std::size_t, std::size_t&, Context&, std::string&)> ScanFunc;
    typedef std::function<bool(const ActionMaterial&, Context& ctx, std::string& err)>               Action; // 识别动作

    // namespace details ????
    template <class ScanFunc>
    class SeqImpl
    {
    public:
        SeqImpl() = default;
        SeqImpl(const SeqImpl&) = default;

        explicit SeqImpl(std::initializer_list<ScanFunc> member) : m_member(member) {}

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept
        {
            std::size_t oldOffset = offset;
            for (const ScanFunc& sf : m_member)
            {
                ScanState ss = sf(data, len, offset, ctx, err);
                if (ScanState::Dismatch == ss)
                {
                    offset = oldOffset;
                    return ScanState::Dismatch;
                }

                if (ScanState::Fatal == ss)
                {
                    return ScanState::Fatal;
                }
            }

            // 允许空串
            ctx.m_scanner_info = m_member.size();
            return ScanState::OK;
        }

        void AddMember(std::initializer_list<ScanFunc> member)
        {
            m_member.insert(m_member.end(), member.begin(), member.end());
        }

    private:
        std::vector<ScanFunc>  m_member;
    }; // class SeqImpl

    class Branch
    {
    public:
        Branch() = default;
        Branch(const Branch&) = default;

        explicit Branch(std::initializer_list<ScanFunc> member) : m_member(member) {}

        void AddMember(std::initializer_list<ScanFunc> member)
        {
            m_member.insert(m_member.end(), member.begin(), member.end());
        }

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept
        {
            if (m_member.empty())
            { // 允许空串
                return ScanState::OK;
            }

            std::size_t oldOffset = offset;
            for (std::size_t i = 0; i < m_member.size(); ++i)
            {
                const ScanFunc& sf = m_member[i];
                ScanState ss = sf(data, len, offset, ctx, err);
                if (ScanState::Dismatch != ss)
                {
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

    class CharBranch
    {
    public:
        typedef std::pair<char, char> Range;

    public:
        CharBranch() = default;
        CharBranch(const CharBranch&) = default;
        explicit CharBranch(std::initializer_list<char> member) : CharBranch(false, member) {}
        CharBranch(bool negative, std::initializer_list<char> member)
        {
            AddMember(member);
            if (negative)
            { // 负字符集
                m_bitset.flip();
            }
        }

        explicit CharBranch(const std::string& member) : CharBranch(false, member) {}

        CharBranch(bool negative, const std::string& member)
        {
            AddMember(member);
            if (negative)
            { // 负字符集
                m_bitset.flip();
            }
        }

        explicit CharBranch(const std::vector<Range>& ranges)
        {
            for (const Range& range : ranges)
            {
                if (range.first < range.second)
                {
                    for (int v = range.first; v < (int)(range.second) + 1; ++v)
                    {
                        AddMember(std::initializer_list<char>{ char(v) });
                    }
                }
                else if (range.first == range.second)
                {
                    AddMember(std::initializer_list<char>{range.first});
                }
            }
        }

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept
        {
            _Unused(ctx, err);

            if (m_bitset.none())
            { // 允许空串
                return ScanState::OK;
            }

            if (offset + 1u > len)
            { // 至少要求一个字符
                return ScanState::Dismatch;
            }

            uint8_t c = (uint8_t)(data[offset]);
            if (m_bitset.test(c))
            {
                ++offset;
                // 没有分支 id 可以被回调
                return ScanState::OK;
            }

            return ScanState::Dismatch;
        }

        template <class Member>
        void AddMember(Member&& member)
        {
            for (char c : member)
            {
                uint8_t pos = static_cast<uint8_t>(c);
                m_bitset.set(pos, true);
            }
        }

    private:
        std::bitset<256u> m_bitset;
    }; // class CharBranch

    class Loop
    {
    public:
        enum : std::size_t { INF_CNT = ~static_cast<std::size_t>(0) };

    public:
        Loop() = default;
        Loop(const Loop&) = default;
        Loop(const ScanFunc& member, std::size_t min, std::size_t max) : m_min(min), m_max(max), m_member(member)
        {
            assert(m_min <= m_max);
        }

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept
        {
            std::size_t oldOffset = offset;
            for (std::size_t i = 0; i < m_min; ++i)
            { // 必要条件： 满足最小循环次数
                ScanState ss = m_member(data, len, offset, ctx, err);
                if (ScanState::Dismatch == ss)
                {
                    offset = oldOffset;
                    return ScanState::Dismatch;
                }

                if (ScanState::Fatal == ss)
                {
                    return ScanState::Fatal;
                }
            }

            std::size_t loop_cnt = m_min;
            for (; loop_cnt < m_max; ++loop_cnt)
            {
                oldOffset = offset;
                ScanState ss = m_member(data, len, offset, ctx, err);
                if (ScanState::OK != ss)
                {
                    offset = oldOffset;
                    break;
                }

                // @TODO 致命错误，也当失配处理？
            }

            ctx.m_scanner_info = loop_cnt;
            return ScanState::OK;
        }

        void AddMember(const ScanFunc& member, std::size_t _min, std::size_t _max)
        {
            m_min = _min;
            m_max = _max;
            assert(m_min <= m_max);
            m_member = member;
        }

    private:
        std::size_t m_min = 0;
        std::size_t m_max = 0;
        ScanFunc    m_member;
    }; // class Loop

    struct AtMost1 : Loop
    {
        AtMost1() = default;
        AtMost1(const AtMost1&) = default;
        explicit AtMost1(const ScanFunc& member) : Loop(member, 0, 1u) {}

        void AddMember(const ScanFunc& member)
        {
            Loop::AddMember(member, 0, 1u);
        }
    };

    class Token
    {
    public:
        explicit Token(const std::string& tok) : m_tok(tok) {}
        Token(const Token&) = default;
        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context&, std::string&) const noexcept
        {
            if (m_tok.size() + offset > len)
            {
                return ScanState::Dismatch;
            }

            //if (!m_tok.empty()) // ??? nop ?
            {
                if (0 == std::memcmp(m_tok.data(), data + offset, m_tok.size()))
                {
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

    // 三值布尔的逻辑非（第三态是 Fatal）。类似于负零宽断言: LogicNotScanner 不消耗字符，区别于“负字符集”，后者是消耗字符的
    class LogicNotScanner
    {
    public:
        LogicNotScanner() = default;

        template <class Scanner>
        explicit LogicNotScanner(Scanner&& scanner) : m_scanner(std::forward<Scanner>(scanner)) {}

        LogicNotScanner(const LogicNotScanner&) = default;

        template <class Scanner>
        void AddMember(Scanner&& scanner)
        {
            m_scanner = std::forward<Scanner>(scanner);
        }

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept
        {
            if (m_scanner)
            {
                std::size_t old_offset = offset;
                ScanState ss = m_scanner(data, len, offset, ctx, err);
                switch (ss)
                {
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
    class FatalIf
    {
    public:
        template <class Scanner>
        explicit FatalIf(Scanner&& scanner, const std::string& err_msg) : m_scanner(std::forward<Scanner>(scanner)), m_err_msg(err_msg) {}

        FatalIf(const FatalIf&) = default;

        explicit FatalIf(const std::string& err_msg = "") : m_err_msg(err_msg) {}

        template <class Scanner>
        void AddMember(Scanner&& scanner, const std::string& err_msg)
        {
            m_scanner = std::forward<Scanner>(scanner);
            m_err_msg = err_msg;
        }

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept
        {
            if (m_scanner)
            {
                std::size_t old_offset = offset;
                ScanState ss = m_scanner(data, len, offset, ctx, err);
                if (ScanState::OK == ss)
                {
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

    class ActionScanner
    {
    public:
        template <class Scanner, class ActionType>
        ActionScanner(Scanner&& scanner, ActionType&& action)
            : m_scanner(std::forward<Scanner>(scanner)), m_action(std::forward<ActionType>(action))
        {}

        ActionScanner() = default;
        ActionScanner(const ActionScanner&) = default;

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept
        {
            if (!m_scanner)
            { // 纯 Action
                const char* begin = data + offset;
                ctx.m_lazy_action.emplace_back(ActionMaterial{ StrRef{begin, begin}, ctx.m_scanner_info, this });
                return ScanState::OK;
            }

            std::size_t old_lazy_cnt = ctx.m_lazy_action.size();
            const char* begin = data + offset;
            ScanState ss = m_scanner(data, len, offset, ctx, err);
            if (ScanState::OK == ss)
            {
                const char* end = data + offset;
                ctx.m_lazy_action.emplace_back(ActionMaterial{ StrRef{ begin, end }, ctx.m_scanner_info, this });
            }
            else if (ScanState::Dismatch == ss)
            {
                ctx.m_lazy_action.resize(old_lazy_cnt);
            }

            return ss; // 原样抛出
        }

        bool InvokeAction(const ActionMaterial& am, Context& ctx, std::string& err) const noexcept
        {
            if (m_action)
            {
                return m_action(am, ctx, err);
            }
            return true; // 兼容 “空 Action”
        }

    private:
        ScanFunc    m_scanner;
        Action      m_action;
    }; // class ActionScanner

    class PreparedScanner // : public Debug
    {
    public:
        PreparedScanner() : m_scanner(std::make_shared<ScanFunc>()) {}

        PreparedScanner(const PreparedScanner&) = default;

        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept
        {
            assert(m_scanner);
            return (*m_scanner)(data, len, offset, ctx, err);
        }

        template <class T, class... Args>
        void ReshapePlacement(Args&&... args)
        {
            assert(m_scanner);
            *m_scanner = T(std::forward<Args>(args)...);
        }

        template <class T>
        void Reshape(T&& scanner)
        {
            assert(m_scanner);
            *m_scanner = std::forward<T>(scanner);
        }

    private:
        std::shared_ptr<ScanFunc>   m_scanner;
    }; // class PreparedScanner

    template <class S, class D>
    struct _Debugger
    {
        typedef _Debugger<S, D> _Myt;

        template <class Sc, class T>
        _Debugger(Sc&& s, T&& h) : m_scanner(std::forward<Sc>(s)), m_h(std::forward<T>(h)) {}

        _Debugger(const _Myt&) = default;
        _Debugger() = default;


        ScanState operator()(const char* data, std::size_t len, std::size_t& offset, Context& ctx, std::string& err) const noexcept
        {
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
            _Debugger<S, H> MakeDebugger(Scanner&& scanner, Handler&& h = []() {})
        {
            return _Debugger<S, H>(std::forward<Scanner>(scanner), std::forward<Handler>(h));
        }

        Token operator"" _t(const char* str, std::size_t) { return Token(str); }
    }


    ////////////

    typedef SeqImpl<ScanFunc> Seq;
} // namespace _LEXPARSER_CORE

////////////////////////////////

namespace _LEXPARSER_SHELL
{

    typedef _LEXPARSER_CORE::ScanFunc  Scanner;
    typedef _LEXPARSER_CORE::Action    Action;

    namespace details
    {
        typedef std::pair<std::size_t, std::size_t> LoopCntPair;
        struct AtMost1 {};
        struct AnyCnt {};
        struct AtLeast1 {};

        using any_cnt_t = LoopCntPair(*)(AnyCnt*);
        using at_least_1_t = LoopCntPair(*)(AtLeast1*);
        using at_most_1_t = LoopCntPair(*)(AtMost1*);

        //typedef std::variant<LoopCntPair, LoopCnt, AtMost1, AnyCnt, AtLeast1> LoopCntVariant;
    } // namespace details


    namespace //  free function
    {
        details::LoopCntPair loop_cnt(std::size_t c) noexcept
        {
            return details::LoopCntPair{ c, c };
        }

        details::LoopCntPair loop_cnt(std::size_t m, std::size_t n) noexcept
        {
            return details::LoopCntPair{ m, n };
        }

        details::LoopCntPair any_cnt(details::AnyCnt* = nullptr)
        {
            return details::LoopCntPair{ 0, ~std::size_t(0) };
        }

        details::LoopCntPair at_least(std::size_t n) noexcept
        {
            return details::LoopCntPair{ n, ~std::size_t(0) };
        }

        details::LoopCntPair at_least_1(details::AtLeast1* = nullptr)
        {
            return at_least(1u);
        }

        details::LoopCntPair at_most(std::size_t n) noexcept
        {
            return details::LoopCntPair{ 0, n };
        }

        details::LoopCntPair at_most_1(details::AtMost1* = nullptr)
        {
            return at_most(1u);
        }
    } // namespace free function

    //// helper type for the visitor #4
    //template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    //// explicit deduction guide (not needed as of C++20)
    //template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;
    // helper constant for the visitor
    //template<class> inline constexpr bool always_false_v = false;


    // UnbindPsr, LiteralStringPsr, SequencePsr, BranchPsr, LoopPsr, CharSetPsr, CharRangePsr, ActionPsr
    // data Parser = UnbindPsr | LiteralStringPsr [char] | SequencePsr [expr] | BranchPsr [expr] | LoopPsr expr m n | CharSetPsr | CharRangePsr | ActionPsr expr ac

    struct Parser;

    struct UnbindPsr
    {
    };

    //struct ParserPtr
    //{
    //    _LEXPARSER_CORE::ScanState  operator()(const char* data, std::size_t len, std::size_t& offset, _LEXPARSER_CORE::Context& ctx, std::string& err) const noexcept
    //    {
    //        assert(m_psr);
    //        return (*m_psr)(data, len, offset, ctx, err);
    //    }
    //
    //    std::shared_ptr<Parser>  m_psr;
    //};

    //struct LiteralStringPsr : _LEXPARSER_CORE::Token
    //{
    //    std::string m_content;
    //};

    typedef _LEXPARSER_CORE::Token LiteralStringPsr;

    struct SequencePsr : _LEXPARSER_CORE::SeqImpl<std::shared_ptr<Parser>>
    {
        SequencePsr& PushBack(const Parser& expr)
        {
            m_content.push_back(std::make_shared<Parser>(expr));
            return *this;
        }

        SequencePsr& PushBack(const std::shared_ptr<Parser>& expr)
        {
            m_content.push_back(expr);
            return *this;
        }

        std::vector<std::shared_ptr<Parser>>  m_content;
        //_LEXPARSER_CORE::Seq                  m_seq;
    };

    struct BranchPsr
    {
        BranchPsr& PushBack(const Parser& expr)
        {
            m_content.push_back(std::make_shared<Parser>(expr));
            return *this;
        }

        BranchPsr& PushBack(const std::shared_ptr<Parser>& expr)
        {
            m_content.push_back(expr);
            return *this;
        }

        std::vector<std::shared_ptr<Parser>>  m_content;
    };

    struct LoopPsr
    {
        LoopPsr(const Parser& expr, std::size_t _min, std::size_t _max)
            : m_body(std::make_shared<Parser>(expr)), m_min(_min), m_max(_max)
        {}
        LoopPsr(const std::shared_ptr<Parser>& expr, std::size_t _min, std::size_t _max)
            : m_body(expr), m_min(_min), m_max(_max)
        {}

        std::shared_ptr<Parser>  m_body;
        std::size_t            m_min = 0;
        std::size_t            m_max = 0;
    };

    struct CharSetPsr
    {
        explicit CharSetPsr(const std::string& set)
            : m_content(set)
        {}

        std::string m_content;
    };

    struct CharRangePsr
    {
        CharRangePsr() = default;
        CharRangePsr(char b, char e)
            : m_content({ std::make_pair(b, e) })
        {}

        CharRangePsr& Push(const std::pair<char, char>& pair)
        {
            m_content.emplace_back(pair.first, pair.second);
            return *this;
        }

        std::vector<std::pair<char, char>> m_content;
    };

    struct ActionPsr
    {
        ActionPsr(const Parser& expr, const Action& ac)
            : m_psr(std::make_shared<Parser>(expr)), m_ac(ac)
        {}

        ActionPsr(const std::shared_ptr<Parser>& expr, const Action& ac)
            : m_psr(expr), m_ac(ac)
        {}

        std::shared_ptr<Parser>  m_psr;
        Action                 m_ac;
    };

    struct NotPsr
    {
        explicit NotPsr(const Parser& expr)
            : m_psr(std::make_shared<Parser>(expr))
        {}

        explicit NotPsr(const std::shared_ptr<Parser>& expr)
            : m_psr(expr)
        {}

        std::shared_ptr<Parser>  m_psr;
    };

    struct FatalPsr
    {
        FatalPsr(const Parser& expr, const std::string& errMsg)
            : m_psr(std::make_shared<Parser>(expr)), m_errMsg(errMsg)
        {}

        FatalPsr(const std::shared_ptr<Parser>& expr, const std::string& errMsg)
            : m_psr(expr), m_errMsg(errMsg)
        {}

        std::shared_ptr<Parser>  m_psr;
        std::string              m_errMsg;
    };

    struct NopPsr
    {};

    struct LambdaPsr  // @TODO 尚未实现 柯里化
        : std::function<Parser(Parser)>
    {
        using std::function<Parser(Parser)>::function;
    };

    struct Parser
    {
        typedef std::variant<
            UnbindPsr, LiteralStringPsr, SequencePsr, BranchPsr, LoopPsr, CharSetPsr, CharRangePsr, ActionPsr, NotPsr, FatalPsr, Scanner, NopPsr, LambdaPsr
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

    public:
        void operator=(const std::string& expr) noexcept
        {
            assert(std::get_if<UnbindPsr>(&m_psr));
            m_psr = LiteralStringPsr{ expr };
        }

        template <std::size_t N>
        void operator=(const char(&arr)[N]) noexcept
        {
            return (*this) = std::string(arr);
        }

        template <class T, class = typename std::enable_if<!std::is_same<UnbindPsr, T>::value>::type>
        void operator=(const T& expr) noexcept
        {
            m_psr = expr;
        }

        void operator=(const Parser& expr) noexcept
        {
            if (this != &expr)
            {
                //assert(expr.m_name.empty());
                assert(!m_name.empty());
                m_psr = expr.m_psr;
            }
        }

        template <class... Psrs>
        Parser apply(const Parser& arg, Psrs&&... rest) const
        {
            assert(std::get_if<LambdaPsr>(&m_psr));
            return std::get<LambdaPsr>(m_psr)(arg, std::forward<Psrs>(rest)...);
        }

        // CharRangePsr helper function
        Parser& operator()(const std::pair<char, char>& pair)
        {
            assert(std::get_if<CharRangePsr>(&m_psr));
            std::get<CharRangePsr>(m_psr).Push(pair);
            return *this;
        }

        // CharRangePsr helper function
        Parser& operator()(char b, char e)
        {
            return (*this)(std::make_pair(b, e));
        }

        // Make LoopPsr
        Parser operator[](details::LoopCntPair pair) const
        {
            return Parser(LoopPsr{ *this, pair.first, pair.second });
        }

#if (__cplusplus > 202002) || (defined(_MSC_VER) && (_MSC_VER > 1934))
        template <class T>
        Parser operator[](std::size_t _min, std::size_t _max)  const // C++ 23 
        {
            return Parser(LoopPsr{ *this, _min, _max });
        }
#endif

        Parser operator[](details::at_most_1_t) const { return (*this)[at_most_1()]; }
        Parser operator[](details::any_cnt_t) const { return (*this)[any_cnt()]; }
        Parser operator[](details::at_least_1_t) const { return (*this)[at_least_1()]; }

        bool Anonymous() const { return m_name.empty(); }

        //operator()(xxx)
        //{
        //    std::visit([](auto&& arg) {
        //        arg(xxx)
        //        }
        //}

    public:
        VariantParser  m_psr;
        std::string    m_name;
    };


    namespace details
    {
        template <class T, class Parser>
        static inline Parser _MakeSeqOrBranchPair(const Parser& a, const Parser& b)
        {
            if (a.Anonymous())
            {// 左结合的连接符，-，会导致 a 有可能也是个 SequenceExpr， 如果 a 是匿名的需展开
                const T* _a = std::get_if<T>(&a.m_psr);
                if (nullptr != _a)
                {
                    T copy = *_a; // copy 一份
                    return Parser(copy.PushBack(b));
                }
            }

            return Parser(T{ {std::make_shared<Parser>(a), std::make_shared<Parser>(b)} });
        }

    } // namespace details

    namespace // free function
    {

        //#ifndef _LXP_SEQUENCE_CONCATENATION_CHARACTER
        //    // 序列连接符
        //#define _LXP_SEQUENCE_CONCATENATION_CHARACTER -
        //#endif // _LXP_SEQUENCE_CONCATENATION_CHARACTER

        Parser operator- (const Parser& a, const Parser& b)
        {
            return (details::_MakeSeqOrBranchPair<SequencePsr>(a, b));
        }
        Parser operator, (const Parser& a, const Parser& b)
        {
            return (a - b); // 序列连接兼容 ‘-’ 与 ‘,’ 只是它们的优先级不同
        }

        // | 也是与 , 类似
        Parser operator| (const Parser& a, const Parser& b)
        {
            return details::_MakeSeqOrBranchPair<BranchPsr>(a, b);
        }


        Parser operator<<(const std::string& str, const Action& action)
        {
            return Parser(ActionPsr{ Parser(LiteralStringPsr{str}), action });
        }

        Parser operator"" _psr(const char* str, std::size_t)
        {
            return Parser(LiteralStringPsr{ str });
        }

        template <class T>
        Parser operator<<(const T& expr, const Action& action)
        { // 对与 T 类型的约束，Expr 的构造函数会出手
            return Parser(ActionPsr{ Parser(expr), action });
        }

        Parser set(const std::string& s)
        {
            return Parser(CharSetPsr(s));
        }

        Parser range(char b, char e)
        {
            return Parser(CharRangePsr(b, e));
        }

        Parser range(const std::pair<char, char>& pair)
        {
            return range(pair.first, pair.second);
        }

        Parser _not(const Parser& expr)
        { // 函数名添加 _ 前缀是为了避免与 not 操作符冲突
            return Parser(NotPsr(expr));
        }

        Parser fatal_if(const Parser& expr, const std::string& errMsg = "")
        {
            return Parser(FatalPsr(expr, errMsg));
        }

        // nop
        Parser nop(NotPsr());

        Parser $(const Scanner& scan)
        {
            //return Parser(Scanner(std::forward<scanner_t>(scan)));
            return Parser(scan);
        }

        Parser $(const LambdaPsr& lambdaPsr)
        {
            return Parser(lambdaPsr);
        }

    } // namespace  // free function


    template <class T>
    static inline T& _PsrForward(T& v) { return v; }

} // namespace _LEXPARSER_SHELL
