/**
 * Copyright (c) 2023, Connor deel@d-l.top
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * You can modify this file as you wish, as long as you comply with
 * the terms of the MIT license.
 */


#define LEXPSR_SHORT_KEYWORDS
#include "../lexpsr.h"
#include "eNFA2DFA.h"

// valgrind --tool=memcheck --leak-check=full

namespace regex {
    enum Flag : uint32_t {
        DEFAULT     = 0,
        CASELESS    = (1 << 0), // i
        NON_GREEDY  = (1 << 2), // 非贪婪匹配
        DOT_NOT_ALL = (1 << 3), // . 不匹配所有， 与其他正则引擎不同，这里默认 ‘.’ 是匹配所有的
    };

    class RegexAST final {
        typedef _LEXPARSER_CORE::StrRef StrRef;
    public:
        enum class NodeType {
            Unknown = -1, Branch, Sequence, Loop, CharSet
        };
        struct LoopFlag {
            std::pair<std::size_t, std::size_t> m_range{ 0, 0 };
            bool                                m_less{ false };

            LoopFlag() = default;
            LoopFlag(const std::pair<std::size_t, std::size_t>& range, bool less) : m_range(range), m_less(less) {}
        };
        class CharSet;
        union UnionData {
            LoopFlag m_loop_flag;
            CharSet* m_charset;

            UnionData() : m_loop_flag(LoopFlag()) {}
        };
        struct Node {
        public:
            StrRef             m_context;
            UnionData          m_union_data;
            NodeType           m_type = NodeType::Unknown;
            std::vector<Node*> m_member;

        public:
            Node(NodeType type, const StrRef& tok, UnionData union_data = UnionData()) : m_context(tok), m_union_data(union_data), m_type(type) {}

            bool IsBranch() const { return NodeType::Branch == m_type; }
            bool IsSequence() const { return NodeType::Sequence == m_type; }
            bool IsLoop() const { return NodeType::Loop == m_type; }
            bool IsCharSet() const { return NodeType::CharSet == m_type; }

            bool IsInfiniteLoop() const { // 是否为无限循环
                if (IsLoop()) {
                    return _LEXPARSER_CORE::Loop::INF_CNT == m_union_data.m_loop_flag.m_range.second; // max_loop
                }
                return false;
            }
        }; // struct Node

        class CharSet final {
            struct _Flag;
        public:
            static void NormalConstruct(_Flag&) {}
            static void SingleCharConstruct(_Flag&, int) {}

        private:
            using _NormalType = decltype(NormalConstruct);
            using _SingleCharType = decltype(SingleCharConstruct);

        public:
            CharSet(_NormalType, bool negative_flag) : m_negative_flag(negative_flag), m_single_char_cached(false) {}

            // 单字符集构造版本
            CharSet(_SingleCharType, uint8_t single_char) : m_negative_flag(false), m_single_char(single_char), m_single_char_cached(false) {
                SetPositiveFlag();
                AddDiscrete(single_char);  // 这里会破坏 m_single_char_cached
                m_single_char_cached = true;
            }

            CharSet(const CharSet&) = default;

            CharSet& AddDiscrete(uint8_t c) noexcept {
                m_single_char_cached = false;
                m_bitset.set(c, true);
                return *this;
            }

            CharSet& AddRange(uint8_t _min, uint8_t _max) noexcept {
                m_single_char_cached = false;
                if (_min > _max) { std::swap(_min, _max); }
                for (std::size_t i = _min; i <= _max; ++i) { m_bitset.set(i, true); } // i 不能是 uint8_t 类型
                return *this;
            }

            CharSet& ToCaseless() {
                const bool v = IsPositive();
                using fn = int(*)(int);
                auto trans = [this, v](char low, char high, fn castfn) {
                    for (std::size_t i = (std::size_t)low; i <= (std::size_t)high; ++i) {
                        if (v == m_bitset.test(i)) {
                            m_bitset.set((std::size_t)castfn((int)i), v);
                            if (m_single_char_cached) { // 必定不再是 single_char 了
                                m_single_char_cached = false;
                            }
                        }
                    }
                };
                trans('A', 'Z', (std::tolower));
                trans('a', 'z', (std::toupper));
                return *this;
            }

            // 判断是否是空字符集 []
            bool IsNoneChar() const { return m_bitset.none(); }

            // 返回是否是满域字符集
            bool IsAnyChar() const {
                if (IsNegative()) {
                    return IsNoneChar();
                }
                return m_bitset.all();
            }

            bool IsSingleChar() const { return 1u == Count(); }

            bool SingleChar(uint8_t& c) const {
                if (!IsSingleChar()) { return false; }
                if (m_single_char_cached) {
                    c = m_single_char;
                }
                else { // 缓存无效
                    for (std::size_t i = 0; i < 256u; ++i) {
                        bool bit = m_bitset.test(i);
                        if (IsPositive() ? (bit) : (!bit)) {
                            uint8_t target = (uint8_t)(i & 0xffu);
                            c = target;
                            // 更新缓存？？？ 不值得再更新缓存！会使得维护复杂度上升
                            // m_single_char        = target;
                            // m_single_char_cached = true;
                            break;
                        }
                    }
                }

                // assert(m_single_char_cached); // ??? @TODO 可以注释掉
                return true;
            }
            // Count() 返回有效字符的个数
            //   返回值为 0 （存疑?）、1 和 256 都有特殊含义
            //   0:   表示空集 [], 这种情况在“词法解析”过程中就可以被无视的吃掉，但构图的时候也可以做一些兼容
            //   1:   表示字符集只有一个，可以被优化成 \xnn,
            //        进而多个连续的 \xnn 可以生成一个 StringGraph (但不太容易在 流模式 中使用)
            //        此外， ??(是否值得?)?? 短一些的 StringGraph 可以不用 memcmp (比如平铺成： len >= x && \xnn == t[0] && \xnn' == t[1] && ...)
            //   256: 表示 AnyChar
            //        注意这三个模式串的 Graph 是同构的，甚至是完全相同的：
            //        (1) /[^]*abc/
            //        (2) /.*abc/s
            //        (3) /[\x00-\xff]*abc/
            std::size_t Count() const {
                if (IsNegative()) {
                    return m_bitset.size() - m_bitset.count();
                }
                return m_bitset.count();
            }

            // 转换成正字符集
            CharSet& ToPositive() noexcept {
                if (IsNegative()) {
                    assert(false == m_single_char_cached);
                    SetPositiveFlag();
                    m_bitset.flip();
                }
                return *this;
            }

            void SetPositiveFlag() { m_negative_flag = false; }
            void SetNegativeFlag() { m_negative_flag = true; }

            bool IsPositive() const noexcept { return false == m_negative_flag; }
            bool IsNegative() const noexcept { return !IsPositive(); }

            // 合并两个字符集
            CharSet& operator|=(CharSet&& that) noexcept {
                if (this == &that) {
                    return *this;
                }

                m_single_char_cached = false; // 合并完成之后，缓存不再可靠
                if (IsPositive() && that.IsPositive()) { // 都是正字符集是计算方式是并集
                    m_bitset |= that.m_bitset;
                    return *this;
                }

                if (IsNegative() && that.IsNegative()) { // 都是负字符集是计算方式是交集 <--- 这里要注意
                    m_bitset &= that.m_bitset;
                    return *this;
                }

                if (IsNegative()) { // 仅 this 是负字符集，转正字符集后再合并
                    return ToPositive() |= std::move(that);
                }

                // 仅 that 是负字符集, 转正字符集后再合并
                return *this |= std::move(that.ToPositive());
            }

            template <class T, class V = std::nullptr_t>
            void CalcSet(T&& arr, std::size_t size, V&& next_v, V&& default_v = nullptr) const {
                assert(256u == size); (void)size;
                assert(default_v != next_v);
                auto&& true_value = IsPositive() ? next_v : default_v;
                auto&& false_value = IsPositive() ? default_v : next_v;
                for (std::size_t i = 0; i < 256u; ++i) {
                    arr[i] = m_bitset.test(i) ? true_value : false_value;
                }
            }

            template <class H>
            void PeekInsideInOrder(H&& h) const { // 按序窥视内部
                for (std::size_t i = 0; i < m_bitset.size(); ++i) {
                    const bool rawbit = m_bitset.test(i);
                    const bool bit = IsPositive() ? rawbit : (!rawbit);
                    h((uint8_t)i, bit);
                }
            }

            ~CharSet() {
                m_bitset.reset();
                m_negative_flag = false;
                m_single_char = 0;
                m_single_char_cached = false;
            }

        private:
            std::bitset<256u>  m_bitset;
            bool               m_negative_flag = false;      // 是否是“非字符集”； ^
            uint8_t            m_single_char   = 0;          // 仅作为临时缓存使用
            bool               m_single_char_cached = false; // single_char 缓存是否有效（可靠）
        }; // class CharSet

    public:
        RegexAST() = default;
        RegexAST(const RegexAST&) = delete;
        RegexAST& operator=(const RegexAST&) = delete;
        ~RegexAST() { Reset(); }

        void Reset() {
            m_root = nullptr;
            for (Node* n : m_nodes_holder) { delete n; }
            for (CharSet* cs : m_charset_holder) { delete cs; }
            m_nodes_holder.clear();
            m_charset_holder.clear();
        }

    public:
        void SetRoot(Node* node) { m_root = node; }
        const Node* Root() const { return m_root; }

        template <class... Args>
        Node* CreateNode(NodeType type, const StrRef& tok, CharSet* cs, Args&&... args) {
            UnionData ud;
            ud.m_charset = cs;
            return CreateNode(type, tok, ud, std::forward<Args>(args)...);
        }

        template <class... Args>
        Node* CreateNode(NodeType type, const StrRef& tok, LoopFlag loop_flag, Args&&... args) {
            UnionData ud;
            ud.m_loop_flag = loop_flag;
            return CreateNode(type, tok, ud, std::forward<Args>(args)...);
        }

        template <class... Args>
        Node* CreateNode(NodeType type, const StrRef& tok, Args&&... args) {
            Node* node = new Node(type, tok, std::forward<Args>(args)...);
            m_nodes_holder.push_back(node);
            return node;
        }

        template <class... Args>
        CharSet* _CreateCharSet(Args&&... args) {
            CharSet* cs = new CharSet(std::forward<Args>(args)...);
            m_charset_holder.push_back(cs);
            return cs;
        }

        CharSet* CreateSingleCharSet(uint8_t single_char) {
            return _CreateCharSet(CharSet::SingleCharConstruct, single_char);
        }

        CharSet* CreateNormalCharSet(bool negative_flag) {
            return _CreateCharSet(CharSet::NormalConstruct, negative_flag);
        }

    private:
        Node*                m_root = nullptr;
        std::deque<Node*>    m_nodes_holder;
        std::deque<CharSet*> m_charset_holder;
    }; // class RegexAST

    namespace details {
        static inline std::string DumpCharSet(RegexAST::CharSet* cs) {
            assert(nullptr != cs);

            auto x = [](uint8_t c) -> char {
                switch (c) {
                case 0: return '0';  case 1: return '1';  case 2: return '2';  case 3: return '3';
                case 4: return '4';  case 5: return '5';  case 6: return '6';  case 7: return '7';
                case 8: return '8';  case 9: return '9';  case 0x0a: return 'a'; case 0x0b: return 'b';
                case 0x0c: return 'c'; case 0x0d: return 'd'; case 0x0e: return 'e'; case 0x0f: return 'f';
                default: assert(((void)0, false)); return '\0';
                }
            };

            auto tostr = [&x](uint8_t c) {
                if (std::isalnum(uint8_t(c))) {
                    return std::string(1u, (char)c); // 数字和字母原样输出
                }
                std::string ret = "0x";
                ret.push_back(x(uint8_t(c >> 4)));
                ret.push_back(x(uint8_t(c & 0x0f)));
                return ret;
            };

            std::string ret = R"({C:")";
            uint8_t last = (uint8_t)0xff;
            bool  overhang = false;
            const std::size_t ret_origin_len = ret.size();
            cs->PeekInsideInOrder([&](uint8_t c, bool bit) {
                if (false == bit) { return; } // 只关心 true 即可
                if (0 != c) {
                    if (c == uint8_t(last + 1u)) { // 与上次连续, 更新和记录连续值
                        if (0xffu != c) {
                            overhang = true;
                        }
                        last = c;
                        return;
                    }

                    if (overhang) { // 除 0xff 外，c 是连续的最后一个
                        ret += ("-" + tostr(last) + "," + tostr(c)); // -tostr(last),tostr(c)
                    }
                    else { // 不连续
                        if (ret.size() != ret_origin_len) { ret.push_back(','); } // 已经添加过就需要一个逗号隔开
                        ret += tostr(c);
                    }
                    last = c;
                    overhang = false; // 已经落地
                    return;
                }
                last = c;        // assert(0 == c);
                ret += tostr(c); // c == 0
            }); // end cs->PeekInsideInOrder()

            if (overhang) { // last 是连续的最后一个, 包括 0xff
                ret += ("-" + tostr(last)); // -tostr(last)
            }
            return (ret += R"("})");
        } // DumpCharSet()

        static inline std::string DumpNode(const RegexAST::Node* node) {
            if (nullptr == node) { return ""; }

            auto show_seq_branch_loop = [](std::string ret, const RegexAST::Node* node) {
                assert(!node->m_member.empty());
                for (std::size_t i = 0; i < node->m_member.size(); ++i) {
                    const RegexAST::Node* mem = node->m_member[i];
                    if (0 != i) {
                        ret.push_back(',');
                    }
                    ret += DumpNode(mem);
                }
                return (ret += "]}");
            };

            switch (node->m_type) {
            case RegexAST::RegexAST::NodeType::CharSet: {
                return DumpCharSet(node->m_union_data.m_charset);
            }
            case RegexAST::RegexAST::NodeType::Branch: {
                return show_seq_branch_loop("{B:[", node);
            }
            case RegexAST::RegexAST::NodeType::Sequence: {
                return show_seq_branch_loop("{S:[", node);
            }
            case RegexAST::RegexAST::NodeType::Loop: {
                const auto& loop_flag = node->m_union_data.m_loop_flag;
                auto cnt_to_str = [](std::size_t cnt) -> std::string {
                    if (_LEXPARSER_CORE::Loop::INF_CNT == cnt) {
                        return "INF";
                    }
                    return std::to_string(cnt);
                };
                std::string min_cnt = cnt_to_str(loop_flag.m_range.first);
                std::string max_cnt = cnt_to_str(loop_flag.m_range.second);
                auto less_flag = loop_flag.m_less ? " less" : "";
                return show_seq_branch_loop(R"({"L )" + min_cnt + "," + max_cnt + less_flag + R"(":[)", node);
            }
            default:
                assert(((void)0, false));
                return "";
            }
        }

        class ToEpsilonNFAContext {
        public:
            finite_automata::State apply_new_state() {
                finite_automata::State ret = m_max_state;
                m_max_state = finite_automata::details::successor(m_max_state);
                return ret; // return m_max_state++;
            }

            finite_automata::State current_state() const {
                return m_current_state;
            }

            void current_state(finite_automata::State state) {
                m_current_state = state;
            }

        private:
            finite_automata::State m_current_state = 0;
            finite_automata::State m_max_state = 0;
        };

        static inline bool ToEpsilonNFAImpl(const RegexAST::Node* node, finite_automata::EpsilonNFA& enfa, 
            ToEpsilonNFAContext& ctx, std::string& err) {
            if (nullptr == node) { err = "invalid ast"; return false; }

            switch (node->m_type) {
            case RegexAST::RegexAST::NodeType::CharSet: {
                const RegexAST::CharSet* cs = node->m_union_data.m_charset;
                assert(nullptr != cs);

                finite_automata::State current_state = ctx.current_state();
                finite_automata::State next_state = ctx.apply_new_state();
                ctx.current_state(next_state);

                const std::vector<finite_automata::State> next_nfa_state = { next_state };
                uint8_t schar = 0;
                if (cs->SingleChar(schar)) {
                    enfa.move_func(current_state, schar, next_nfa_state);
                }
                else {
                    finite_automata::ordered_unique_vec<finite_automata::Edge> ordered_edges;
                    cs->PeekInsideInOrder([&ordered_edges](uint8_t c, bool bit) {
                        if (false == bit) { return; } // 只关心 true 即可
                        ordered_edges.push_back(c);
                    });
                    enfa.move_func(current_state, ordered_edges, next_nfa_state);
                }

                return true; // DumpCharSet(node->m_union_data.m_charset);
            }
            case RegexAST::RegexAST::NodeType::Branch: {
                assert(!node->m_member.empty());
                if (1u == node->m_member.size()) { // 当序列处理
                    return ToEpsilonNFAImpl(node->m_member.back(), enfa, ctx, err);
                }
                finite_automata::State begin = ctx.current_state();
                finite_automata::State end = ctx.apply_new_state();
                std::vector<finite_automata::State> nfa_end = {end};
                for (const RegexAST::Node* mem : node->m_member) {
                    ctx.current_state(begin);
                    if (!ToEpsilonNFAImpl(mem, enfa, ctx, err)) {
                        return false;
                    }
                    enfa.move_func(ctx.current_state(), finite_automata::epsilon, nfa_end);
                }
                ctx.current_state(end);
                return true;
            }
            case RegexAST::RegexAST::NodeType::Sequence: {
                assert(!node->m_member.empty());
                for (const RegexAST::Node* mem : node->m_member) {
                    if (!ToEpsilonNFAImpl(mem, enfa, ctx, err)) {
                        return false;
                    }
                }
                return true;
            }
            case RegexAST::RegexAST::NodeType::Loop: {
                const auto& loop_flag = node->m_union_data.m_loop_flag;
                if (loop_flag.m_less) {
                    err = "Feature not yet supported: non-greedy matching.";
                    return false;
                }
                if (1u != node->m_member.size()) {
                    err = "BUG: The number of bodies in the loop is not unique.";
                    return false;
                }

                const auto& min_cnt = loop_flag.m_range.first;
                const auto& max_cnt = loop_flag.m_range.second;
                assert(min_cnt <= max_cnt);
                if (min_cnt > 0xffffu) {
                    err = "Currently, the number of deterministic loops exceeding 65535 is not supported.";
                    return false;
                }
                const RegexAST::Node* body = node->m_member.back();
                assert(nullptr != body);

                for (std::size_t i = 0; i < min_cnt; ++i) { // 最小循环 min_cnt 次
                    if (!ToEpsilonNFAImpl(body, enfa, ctx, err)) { // 注意：这个循环可以利用 nfa + nfa + ... 进行性能优化 @TODO
                        return false;
                    }
                }
                /*
                auto need_add_new_begin_end = [&]() -> std::pair<bool, bool> {
                    if (0 == min_cnt || _LEXPARSER_CORE::Loop::INF_CNT == max_cnt) { // 只有在0次或无限循环下才需要考虑新增 begin 或 end 节点
                        return { true, true };  // <--- 细分不必要添加新状态的情况，太过困难，索性都统一加 begin end 新状态！！！

                        if (!body->IsLoop() && !body->IsCharSet()) {
                            if (body->IsBranch()) {
                                for (const RegexAST::Node* member : body->m_member) {
                                    if (member->IsInfiniteLoop()) { return { true, true }; }
                                }
                                return { false, false };
                            }

                            if (body->IsSequence() && !body->m_member.empty()) {
                                const bool first_is_infinite_loop = body->m_member.front()->IsInfiniteLoop();
                                const bool last_is_infinite_loop  = body->m_member.back()->IsInfiniteLoop();
                                if (first_is_infinite_loop || last_is_infinite_loop) {
                                    if (first_is_infinite_loop && last_is_infinite_loop) {
                                        for (auto it = std::next(body->m_member.begin()), end_iter = std::prev(body->m_member.end()); it < end_iter; ++it) {
                                            if (!(*it)->IsInfiniteLoop()) {
                                                return { first_is_infinite_loop, last_is_infinite_loop }; // case:   (a*bc*)*
                                            }
                                        }
                                        return { false, false }; // 如果序列里全是无限循环，则也不用新增节点. case (a*b*c*)*     (a+b*c+)*
                                    }
                                    return { first_is_infinite_loop, last_is_infinite_loop }; // case:   (a*b)*    (ab*)* 
                                }
                                else {
                                    assert(!first_is_infinite_loop && !last_is_infinite_loop);
                                    return { false, false }; // case   (a{0,4}b)*   (ba{0,4})*     (ab*c)*
                                }
                            }

                            return { true, true }; // 兜底保证正确性
                        }
                        else if (body->IsLoop() && !body->IsInfiniteLoop()) {
                            return { true, true }; // case  ((ba*){0,4})*     ((b*a){0,4})*   <--- fix BUG
                        } // case: ((ab)*)*
                    }

                    return { false, false };
                };
                */
                auto add_new_state = [](finite_automata::EpsilonNFA& _enfa, ToEpsilonNFAContext& _ctx) {
                    finite_automata::State new_state = _ctx.apply_new_state();
                    _enfa.move_func(_ctx.current_state(), finite_automata::epsilon, { new_state });
                    _ctx.current_state(new_state);
                };

                const finite_automata::State begin = ctx.current_state();
                if (_LEXPARSER_CORE::Loop::INF_CNT == max_cnt) { // 无限循环
                    //const auto need_add_new_state = need_add_new_begin_end();

                    //if (need_add_new_state.first) {
                    //    finite_automata::State new_state = ctx.apply_new_state();
                    //    enfa.move_func(ctx.current_state(), finite_automata::epsilon, { new_state });
                    //    ctx.current_state(new_state);
                    //}
                    add_new_state(enfa, ctx);

                    if (!ToEpsilonNFAImpl(body, enfa, ctx, err)) {
                        return false;
                    }

                    //if (need_add_new_state.second) {
                    //    finite_automata::State new_state = ctx.apply_new_state();
                    //    enfa.move_func(ctx.current_state(), finite_automata::epsilon, { new_state });
                    //    ctx.current_state(new_state);
                    //}
                    add_new_state(enfa, ctx);

                    enfa.move_func(ctx.current_state(), finite_automata::epsilon, { begin });
                    enfa.move_func(begin, finite_automata::epsilon, { ctx.current_state() });
                    return true;
                }

                if (0 == min_cnt) {
                    if (0 == max_cnt) {
                        return true; // 循环 0 次什么都不用做
                    }
                    add_new_state(enfa, ctx);
                }

                // 剩余最多循环 max_cnt - min_cnt 次
                if (min_cnt < max_cnt) {
                    for (std::size_t i = 0; i < (max_cnt - min_cnt); ++i) {
                        if (!ToEpsilonNFAImpl(body, enfa, ctx, err)) { // 注意：这个循环可以利用 nfa + nfa + ... 进行性能优化 @TODO
                            return false;
                        }
                        if (0 == min_cnt) { // case ((ab*){0,2})*
                            add_new_state(enfa, ctx);
                        }
                        enfa.move_func(begin, finite_automata::epsilon, { ctx.current_state() });
                    }
                }
                else {
                    enfa.move_func(begin, finite_automata::epsilon, { ctx.current_state() });
                }

                return true;
            }
            default:
                assert(((void)0, false));
                return false;
            }
        } // function ToEpsilonNFAImpl
    } // namespace details

    // S: Seq; B: Branch; L: Loop; C: CharSet
    static inline std::string Dump(const RegexAST& ast) {
        const RegexAST::Node* root = ast.Root();
        if (nullptr == root) { return ""; }
        return details::DumpNode(root);
    }

    static inline bool ToEpsilonNFA(const RegexAST& ast, finite_automata::EpsilonNFA& enfa, std::string& err) {
        err.clear();
        enfa.reset();
        details::ToEpsilonNFAContext ctx;
        finite_automata::State begin = ctx.apply_new_state();
        ctx.current_state(begin);
        if (details::ToEpsilonNFAImpl(ast.Root(), enfa, ctx, err)) {
            enfa.start_state(begin);
            enfa.final_states({ ctx.current_state() });
            return true;
        }
        return false;
    }
} // namespace regex

class RegexBuilder {
public:
    RegexBuilder() // : m_root(lexpsr_shell::named_v, "root") {
                    : m_root(std::make_shared<lexpsr_shell::Parser>(lexpsr_shell::UnbindPsr()), "root")
                    , m_group(std::make_shared<lexpsr_shell::Parser>(lexpsr_shell::UnbindPsr()), "group") {
       init();
    }

    bool to_ast(const std::string& regex, uint32_t modifiers, regex::RegexAST& ast, std::string& err) const {
        using namespace lexpsr_shell;
        using namespace regex;

        //RegexAST ast;
        Context ctx(ast);
        std::size_t offset = 0;
        err.clear();

        //auto reset = [&ctx, &offset, &err]() {
        //    ctx.Reset();
        //    offset = 0;
        //    err.clear();
        //};

        ctx.m_global_modifiers = modifiers;
        const std::string& script = regex;
        ScanState ss = m_root.ScanScript(script.data(), script.size(), offset, ctx, err);
        if (ScanState::OK != ss || script.size() != offset) {
            const std::string error_prompts = ctx.ErrorPrompts(offset, script);
            //std::cerr << err << std::endl;
            //std::cerr << error_prompts << std::endl;
            err += ("\r\n" + error_prompts + "\r\n");
            return false;
        }
        else {
            assert(script.size() == offset);
            auto res = InvokeActions(ctx, err);
            if (!res.first) {
                //std::cerr << err << std::endl;
                return false;
            }

            // assert(res.first);  // test !!!
        }
        assert(ScanState::OK == ss && script.size() == offset); // test !!!
        ctx.Finish();
        return true;
        //const std::string dump = regex::Dump(ast);
        //assert(dump == cs.result);  // test !!!
    }

    static bool ast_to_nfa(const regex::RegexAST& ast, finite_automata::EpsilonNFA& nfa, std::string& err) {
        return regex::ToEpsilonNFA(ast, nfa, err);
    }

private:
    typedef regex::RegexAST RegexAST;
    struct Context : lexpsr_shell::core::Context {
        explicit Context(RegexAST& ast) : m_ast(ast) {}
        void Reset() {
            lexpsr_shell::core::Context::Reset();

            m_int_num_stack.clear();
            m_negative_flag_opt_stack.clear();
            m_branch_cnt_stack.clear();
            m_charset_content_stack.clear();
            m_loop_less_opt_stack.clear();
            m_loop_flag_opt_stack.clear();
            m_loop_cnt_range.clear();
            m_ast_charset_stack.clear();
            m_ast_node_stack.clear();
            m_global_modifiers = 0;
            m_comments_cnt     = 0;
            ResetLiteralCharsetCnt();

            m_ast.Reset();
        }

        void ResetLiteralCharsetCnt() {
            m_literal_charset_cnt = 1u;       // literal 字符集个数，默认为 1
            m_literal_leading_word_bytes = 0; // literal 默认没有引导词
        }

        bool Finish() {
            if (1u == m_ast_node_stack.size()) {
                m_ast.SetRoot(m_ast_node_stack.back());
                m_ast_node_stack.pop_back();
                return true;
            }
            return false;
        }

        std::vector<std::size_t>    m_int_num_stack;
        std::vector<std::size_t>    m_negative_flag_opt_stack;
        std::vector<std::size_t>    m_branch_cnt_stack;
        std::vector<std::size_t>    m_charset_content_stack;
        std::vector<std::size_t>    m_loop_less_opt_stack; // 非贪婪标识
        std::vector<std::size_t>    m_loop_flag_opt_stack; // 是否存在显式循环次数标识
        std::vector<std::pair<std::size_t, std::size_t>> m_loop_cnt_range; // 循环次数

        std::vector<RegexAST::CharSet*>    m_ast_charset_stack;
        std::vector<RegexAST::Node*>       m_ast_node_stack;

        RegexAST&                          m_ast;
        uint32_t                           m_global_modifiers = 0;
        uint32_t                           m_comments_cnt     = 0;  // 注释的计数，用于修正 ac_seq 中的 loop_cnt
        uint32_t                           m_literal_charset_cnt        = 1;  // literal 字符（集）个数
        uint32_t                           m_literal_leading_word_bytes = 0;  // literal 引导词的长度，比如 \Q..\E 的引导词长度为 2
    }; // Context
    // static const auto Ctx = [](core::Context& ctx) -> Context& { return static_cast<Context&>(ctx); };
    static Context& Ctx(lexpsr_shell::core::Context& ctx) {
        return static_cast<Context&>(ctx);
    }


private:
    bool init() {
        using namespace lexpsr_shell;
        using namespace regex;
        ///////////////////////// actions ///////////////////
        auto ac_char = [](const ActionArgs& args) {
            const core::StrRef& tok = args.m_action_material.m_token;
            assert(1u == tok.len);
            auto&& ctx = Ctx(args.m_contex);
            ctx.m_ast_charset_stack.push_back(ctx.m_ast.CreateSingleCharSet(tok[0]));
            return true;
        };

        auto ac_hex = [](const ActionArgs& args) {
            const core::StrRef& tok = args.m_action_material.m_token;
            assert(2u == tok.len);
            uint8_t first  = tok[0];
            uint8_t second = tok[1];
            auto xx = [](uint8_t c) -> uint8_t {
                if ('0' <= c && c <= '9') { return c - '0'; }
                if ('a' <= c && c <= 'f') { return c - 'a' + 10; }
                assert('A' <= c && c <= 'F');
                return c - 'A' + 10;
            };
            uint8_t res = (xx(first) << 4 | xx(second)) & 0xff;
            auto&& ctx = Ctx(args.m_contex);
            ctx.m_ast_charset_stack.push_back(ctx.m_ast.CreateSingleCharSet(res));
            return true;
        };

        auto ac_qe_block_content = [](const ActionArgs& args) {
            const core::StrRef& tok = args.m_action_material.m_token;
            auto&& ctx = Ctx(args.m_contex);
            for (std::size_t i = 0; i < tok.len; ++i) {
                ctx.m_ast_charset_stack.push_back(ctx.m_ast.CreateSingleCharSet(tok[i]));
            }
            assert(tok.len <= UINT32_MAX);
            ctx.m_literal_charset_cnt        = (uint32_t)tok.len; // \Q...\E 的字符（集）个数
            ctx.m_literal_leading_word_bytes = 2u;                // \Q..\E 的引导词长度为 2 : '\Q' 两个字符
            return true;
        };

        auto ac_escape_char_one_alpha = [](const ActionArgs& args) {
            const core::StrRef& tok = args.m_action_material.m_token;
            auto&& ctx = Ctx(args.m_contex);
            assert(2u == tok.len);
            assert('\\' == tok[0]);
            RegexAST::CharSet* cs = ctx.m_ast.CreateNormalCharSet(false);
            assert(cs->IsPositive());
            const uint8_t escape_char = tok[1];
            switch (escape_char) { // @TODO 未与 psr 解耦
                ///// 多字符集 ////
            case 't': cs->AddDiscrete('\t'); break;
            case 'n': cs->AddDiscrete('\n'); break;
            case 'v': cs->AddDiscrete('\v'); break;
            case 'f': cs->AddDiscrete('\f'); break;
            case 'r': cs->AddDiscrete('\r'); break;
            case '0': cs->AddDiscrete('\0'); break;
            case 'D': cs->SetNegativeFlag(); // [[fallthrough]]
            case 'd': cs->AddRange('0', '9'); break;
            case 'S': cs->SetNegativeFlag(); // [[fallthrough]]
            case 's': cs->AddDiscrete('\t').AddDiscrete('\n').AddDiscrete('\v').AddDiscrete('\f').AddDiscrete('\r').AddDiscrete(' '); break;
            case 'W': cs->SetNegativeFlag(); // [[fallthrough]]
            case 'w': cs->AddDiscrete('_').AddRange('0', '9').AddRange('a', 'z').AddRange('A', 'Z'); break;
            default:
                cs->~CharSet();
                new (cs) RegexAST::CharSet(RegexAST::CharSet::SingleCharConstruct, escape_char);
                assert(cs->IsPositive() && cs->IsSingleChar());
                break;
            }
            ctx.m_ast_charset_stack.push_back(cs);
            return true;
        };

        auto ac_char_range = [](const ActionArgs& args) {
            const core::StrRef& tok = args.m_action_material.m_token;
            auto&& ctx = Ctx(args.m_contex);
            assert(ctx.m_ast_charset_stack.size() >= 2u);
            RegexAST::CharSet* right = ctx.m_ast_charset_stack.back(); ctx.m_ast_charset_stack.pop_back();
            RegexAST::CharSet* left = ctx.m_ast_charset_stack.back(); // ctx.m_ast_charset_stack.pop_back();

            uint8_t _min = 0, _max = 0;
            if (left->SingleChar(_min) && right->SingleChar(_max)) {
                if (_min > _max) {
                    args.m_error_message = "Range out of order in character class : " + tok.to_std_string();
                    return false;
                }

                if (_min != _max) {
                    left->~CharSet();
                    new (left) RegexAST::CharSet(RegexAST::CharSet::NormalConstruct, false);
                    assert(left->IsPositive());
                    left->AddRange(_min, _max);
                } // else if (_min == _max) 时等价与 [_min] 无需对 left 做任何变动
                return true;
            }
            args.m_error_message = "logic_error: " + tok.to_std_string(); // 文件要求， [left-right] 必须时 SingleChar
            return false;
        };

        auto ac_int_num = [](const ActionArgs& args) {
            const core::StrRef& tok = args.m_action_material.m_token;
            auto&& ctx = Ctx(args.m_contex);
            assert(tok.data && tok.len >= 1u);
            if (0 == tok.len) { return false; }

            uint64_t value = 0;
            auto calc = [&value](uint8_t c) {
                std::size_t old = value;
                value = value * 10u + (c - '0');
                if (value < old) {
                    return false;
                }
                return true;
            };

            for (std::size_t i = 0; i < tok.len; ++i) {
                if (!calc((uint8_t)tok[i])) {
                    args.m_error_message = "int_num overflow: " + tok.to_std_string();
                    return false;
                }
            }
            ctx.m_int_num_stack.push_back(value);
            return true;
        };

        auto ac_negative_flag_opt = [](const ActionArgs& args) {
            auto&& ctx = Ctx(args.m_contex);
            std::size_t loop_cnt = args.m_action_material.m_scanner_info;
            assert(loop_cnt <= 1u);
            ctx.m_negative_flag_opt_stack.push_back(loop_cnt);
            return true;
        };

        auto ac_not0x5d = [](const ActionArgs& args) {
            const core::StrRef& tok = args.m_action_material.m_token;
            auto&& ctx = Ctx(args.m_contex);
            assert(1u == tok.len);
            RegexAST::CharSet* cs = ctx.m_ast.CreateSingleCharSet(tok[0]);
            ctx.m_ast_charset_stack.push_back(cs);
            return true;
        };

        auto ac_charset_content = [](const ActionArgs& args) {
            Ctx(args.m_contex).m_charset_content_stack.push_back(args.m_action_material.m_scanner_info); // loop_cnt
            return true;
        };

        auto ac_charset = [](const ActionArgs& args) {
            auto&& ctx = Ctx(args.m_contex);
            assert(!ctx.m_negative_flag_opt_stack.empty());
            assert(!ctx.m_charset_content_stack.empty());

            bool negative_flag = (0 != ctx.m_negative_flag_opt_stack.back()); // 是否存在负字符集
            ctx.m_negative_flag_opt_stack.pop_back();

            std::size_t content_cnt = ctx.m_charset_content_stack.back(); ctx.m_charset_content_stack.pop_back();
            assert(content_cnt <= ctx.m_ast_charset_stack.size());
            if (0 == content_cnt) { // [] or [^]
                assert(ctx.m_ast_charset_stack.empty());
                RegexAST::CharSet* cs = ctx.m_ast.CreateNormalCharSet(negative_flag);
                ctx.m_ast_charset_stack.push_back(cs);
                return true;
            }

            assert(!ctx.m_ast_charset_stack.empty());
            std::size_t charset_stack_fitsize = ctx.m_ast_charset_stack.size() - content_cnt;
            auto iter = ctx.m_ast_charset_stack.begin();
            RegexAST::CharSet* cs = *iter++;

            for (; iter != ctx.m_ast_charset_stack.end(); ++iter) {
                *cs |= std::move(**iter);
            }
            assert(ctx.m_ast_charset_stack.size() >= charset_stack_fitsize + 1u);
            ctx.m_ast_charset_stack.resize(charset_stack_fitsize + 1u);

            if (negative_flag) {
                if (cs->IsPositive()) {
                    cs->SetNegativeFlag();
                }
                else {
                    cs->SetPositiveFlag();
                }
            }

            return true;
        };

        auto ac_dot = [](const ActionArgs& args) {
            auto&& ctx = Ctx(args.m_contex);
            RegexAST::CharSet* cs = ctx.m_ast.CreateNormalCharSet(true);
            assert(cs->IsNegative() && cs->IsAnyChar());
            if (ctx.m_global_modifiers & (uint32_t)Flag::DOT_NOT_ALL) {
                cs->AddDiscrete('\r').AddDiscrete('\n');
            }
            // 默认“点”是匹配所有字符（与其他正则引擎不同）
            ctx.m_ast_charset_stack.push_back(cs);
            return true;
        };

        auto ac_literal = [](const ActionArgs& args) {
            const core::StrRef& tok = args.m_action_material.m_token;
            auto&& ctx = Ctx(args.m_contex);
            assert(ctx.m_ast_charset_stack.size() >= ctx.m_literal_charset_cnt);
            if (1u == ctx.m_literal_charset_cnt) { // literal 字符集个数为 1
                RegexAST::CharSet* cs = ctx.m_ast_charset_stack.back(); ctx.m_ast_charset_stack.pop_back();
                if (ctx.m_global_modifiers & (uint32_t)Flag::CASELESS) {
                    cs->ToCaseless();
                }
                RegexAST::Node* node = ctx.m_ast.CreateNode(RegexAST::NodeType::CharSet, tok, cs);
                ctx.m_ast_node_stack.push_back(node);
            }
            else if (ctx.m_literal_charset_cnt > 1u){ // literal 字符集个数不为 1，（多字符）可以构成一个序列
                const std::size_t hold_cnt = ctx.m_ast_charset_stack.size() - ctx.m_literal_charset_cnt;
                RegexAST::Node* node = ctx.m_ast.CreateNode(RegexAST::NodeType::Sequence, tok);
                ctx.m_ast_node_stack.push_back(node);
                // add member
                for (std::size_t i = 0; i < ctx.m_literal_charset_cnt; ++i) {
                    RegexAST::CharSet* cs = ctx.m_ast_charset_stack[hold_cnt + i];
                    if (ctx.m_global_modifiers & (uint32_t)Flag::CASELESS) {
                        cs->ToCaseless();
                    }
                    core::StrRef _tok = { tok.data + ctx.m_literal_leading_word_bytes + i, 1u }; // 一个 CharSet 对应着 一个字符， 还要考虑移除引导词
                    assert(_tok.data < tok.data + tok.len); // 不能越界
                    RegexAST::Node* charNode = ctx.m_ast.CreateNode(RegexAST::NodeType::CharSet, _tok, cs);
                    node->m_member.push_back(charNode);
                }
                ctx.m_ast_charset_stack.resize(hold_cnt);
            } // else // 比如 \Q\E 可以构造出一共空的 literal，对于空的 literal 什么都不生成

            ctx.ResetLiteralCharsetCnt();  // 重置 literal 字符集个数
            return true;
        };

        auto ac_loop_less_opt = [](const ActionArgs& args) {
            auto&& ctx = Ctx(args.m_contex);
            std::size_t loop_cnt = args.m_action_material.m_scanner_info;
            assert(loop_cnt <= 1u);
            if (ctx.m_global_modifiers & (uint32_t)Flag::NON_GREEDY) { // 全局非贪婪匹配
                ctx.m_loop_less_opt_stack.push_back(1u);
            }
            else {
                ctx.m_loop_less_opt_stack.push_back(loop_cnt);
            }
            return true;
        };

        auto ac_loop_n = [](const ActionArgs& args) {
            auto&& ctx = Ctx(args.m_contex);
            assert(ctx.m_int_num_stack.size() >= 1u);
            std::size_t n = ctx.m_int_num_stack.back(); ctx.m_int_num_stack.pop_back();
            ctx.m_loop_cnt_range.emplace_back(n, n);
            return true;
        };
        auto ac_loop_mn = [](const ActionArgs& args) {
            auto&& ctx = Ctx(args.m_contex);
            assert(ctx.m_int_num_stack.size() >= 2u);
            std::size_t n = ctx.m_int_num_stack.back(); ctx.m_int_num_stack.pop_back();
            std::size_t m = ctx.m_int_num_stack.back(); ctx.m_int_num_stack.pop_back();
            if (m > n) { // 这个判断过程可以转移到 lexpsr 层面解决（它现在有这个表达能力）
                const core::StrRef& tok = args.m_action_material.m_token;
                args.m_error_message = tok.to_std_string() + " : numbers out of order in {} quantifier";
                return false;
            }
            ctx.m_loop_cnt_range.emplace_back(m, n);
            return true;
        };
        auto ac_loop_m_comma = [](const ActionArgs& args) {
            auto&& ctx = Ctx(args.m_contex);
            assert(ctx.m_int_num_stack.size() >= 1u);
            std::size_t m = ctx.m_int_num_stack.back(); ctx.m_int_num_stack.pop_back();
            ctx.m_loop_cnt_range.emplace_back(m, core::Loop::INF_CNT);
            return true;
        };
        auto ac_loop_comma_n = [](const ActionArgs& args) {
            auto&& ctx = Ctx(args.m_contex);
            assert(ctx.m_int_num_stack.size() >= 1u);
            std::size_t n = ctx.m_int_num_stack.back(); ctx.m_int_num_stack.pop_back();
            ctx.m_loop_cnt_range.emplace_back(0, n);
            return true;
        };
        auto ac_loop_star = [](const ActionArgs& args) {
            Ctx(args.m_contex).m_loop_cnt_range.emplace_back(0, core::Loop::INF_CNT);
            return true;
        };
        auto ac_loop_plus = [](const ActionArgs& args) {
            Ctx(args.m_contex).m_loop_cnt_range.emplace_back(1u, core::Loop::INF_CNT);
            return true;
        };
        auto ac_question_mark = [](const ActionArgs& args) {
            Ctx(args.m_contex).m_loop_cnt_range.emplace_back(0, 1u);
            return true;
        };
        auto ac_loop_flag_opt = [](const ActionArgs& args) {
            std::size_t loop_cnt = args.m_action_material.m_scanner_info;
            assert(loop_cnt <= 1u);
            auto&& ctx = Ctx(args.m_contex);
            ctx.m_loop_flag_opt_stack.push_back(loop_cnt);
            return true;
        };
        auto ac_loop = [](const ActionArgs& args) {
            auto&& ctx = Ctx(args.m_contex);
            bool less = false; // 默认贪婪
            std::pair<std::size_t, std::size_t> loop_cnt_range = { 1, 1 };
            bool implicit_cnt = (0 != ctx.m_loop_flag_opt_stack.back()); ctx.m_loop_flag_opt_stack.pop_back();
            if (implicit_cnt) { // 显式书写了循环次数
                assert(!ctx.m_loop_cnt_range.empty() && !ctx.m_loop_less_opt_stack.empty());
                less = !!(ctx.m_loop_less_opt_stack.back()); ctx.m_loop_less_opt_stack.pop_back();
                loop_cnt_range = ctx.m_loop_cnt_range.back(); ctx.m_loop_cnt_range.pop_back();
            }

            bool left_eq_right_range = loop_cnt_range.first == loop_cnt_range.second;
            // 移除了 循环中判断 group 的设计后，可以放心的将 loop 也最小化了
            if (left_eq_right_range && 1u == loop_cnt_range.first) { // 最小化 loop
                return true;
            }

            // 循环体
            if (ctx.m_ast_node_stack.empty()) {
                return true; // 兼容空的循环体，比如 (\Q\E){2} 或 (){3,4}
            }
            RegexAST::Node* body = ctx.m_ast_node_stack.back(); ctx.m_ast_node_stack.pop_back();
            RegexAST::Node* node = ctx.m_ast.CreateNode(RegexAST::NodeType::Loop, args.m_action_material.m_token);
            if (left_eq_right_range && less) { // 这四种情况是同构的： {n,n}? <=> {n}? <=> {n.n} <=> {n}
                less = false; // 取最简单的（默认状态）: {n}
            }

            node->m_union_data.m_loop_flag = RegexAST::LoopFlag(loop_cnt_range, less);
            node->m_member.push_back(body);

            ctx.m_ast_node_stack.push_back(node);
            return true;
        };
        auto ac_comment = [](const ActionArgs& args) {
            auto&& ctx = Ctx(args.m_contex);
            ++ctx.m_comments_cnt; // 对注释计数，用于修正 ac_seq 的 loop_cnt
            return true;
        };
        auto ac_seq = [](const ActionArgs& args) {
            const core::StrRef& tok = args.m_action_material.m_token;
            auto&& ctx = Ctx(args.m_contex);
            assert(ctx.m_comments_cnt <= args.m_action_material.m_scanner_info);
            const std::size_t loop_cnt = args.m_action_material.m_scanner_info - ctx.m_comments_cnt; // 要排除掉注释的影响
            ctx.m_comments_cnt = 0; // 注释计数用完立刻清零
            if (ctx.m_ast_node_stack.empty()) { // 没有有效节点
                assert(0u == loop_cnt || 1u == loop_cnt);
                return true;
            }
            assert(ctx.m_ast_node_stack.size() >= loop_cnt && loop_cnt >= 1u);
            if (1u == loop_cnt) { // 最小化
                ctx.m_ast_node_stack.back()->m_context = tok;
                return true;
            }

            assert(ctx.m_ast_node_stack.size() >= loop_cnt);
            const std::size_t hold_cnt = ctx.m_ast_node_stack.size() - loop_cnt;
            RegexAST::Node* seq = ctx.m_ast.CreateNode(RegexAST::NodeType::Sequence, tok);
            for (std::size_t i = 0; i < loop_cnt; ++i) {
                RegexAST::Node* node = ctx.m_ast_node_stack[hold_cnt + i];
                assert(nullptr != node);
                if (RegexAST::NodeType::Sequence == node->m_type) { // 合并相邻的 Seq
                    seq->m_member.insert(seq->m_member.end(), node->m_member.begin(), node->m_member.end());
                }
                else {
                    seq->m_member.push_back(node);
                }
            }
            ctx.m_ast_node_stack.resize(hold_cnt);
            ctx.m_ast_node_stack.push_back(seq);
            return true;
        };

        auto ac_branch_follow_up = [](const ActionArgs& args) {
            std::size_t loop_cnt = args.m_action_material.m_scanner_info;
            Ctx(args.m_contex).m_branch_cnt_stack.push_back(loop_cnt);
            return true;
        };

        auto ac_branch = [](const ActionArgs& args) {
            const core::StrRef& tok = args.m_action_material.m_token;
            auto&& ctx = Ctx(args.m_contex);
            assert(!ctx.m_branch_cnt_stack.empty());
            std::size_t branch_cnt = ctx.m_branch_cnt_stack.back() + 1u; ctx.m_branch_cnt_stack.pop_back();
            if (1u == branch_cnt) { // 最小化
                if (!ctx.m_ast_node_stack.empty())
                {
                    ctx.m_ast_node_stack.back()->m_context = tok;
                }
                return true;
            }

            assert(ctx.m_ast_node_stack.size() >= branch_cnt);
            const std::size_t hold_cnt = ctx.m_ast_node_stack.size() - branch_cnt;
            RegexAST::Node* branch = ctx.m_ast.CreateNode(RegexAST::NodeType::Branch, tok);
            for (std::size_t i = 0; i < branch_cnt; ++i) {
                RegexAST::Node* node = ctx.m_ast_node_stack[hold_cnt + i];
                assert(nullptr != node);
                if (RegexAST::NodeType::Branch == node->m_type) { // 合并相邻的 Branch
                    branch->m_member.insert(branch->m_member.end(), node->m_member.begin(), node->m_member.end());
                }
                else {
                    branch->m_member.push_back(node);
                }
            }
            ctx.m_ast_node_stack.resize(hold_cnt);
            ctx.m_ast_node_stack.push_back(branch);
            return true;
        };

        /////////////////////////
        // https://www.debuggex.com/cheatsheet/regex/pcre#/
        //
        //                                                   PCRE regex quick reference:
        //  [abx-z]        One character of: a, b, or the range x-z                     ^          Beginning of the string
        //  [^abx-z]       One character except: a, b, or the range x-z                 $          End of the string
        //  a|b            a or b                                                       \d         A digit (same as [0-9])
        //  a?             Zero or one a's (greedy)                                     \D         A non-digit (same as [^0-9])
        //  a??            Zero or one a's (lazy)                                       \w         A word character (same as [_a-zA-Z0-9])
        //  a*             Zero or more a's (greedy)                                    \W         A non-word character (same as [^_a-zA-Z0-9])
        //  a*?            Zero or more a's (lazy)                                      \s         A whitespace character
        //  a+             One or more a's (greedy)                                     \S         A non-whitespace character
        //  a+?            One or more a's (lazy)                                       \b         A word boundary
        //  a{4}           Exactly 4 a's                                                \B         A non-word boundary
        //  a{4,8}         Between (inclusive) 4 and 8 a's                              \n         A newline
        //  a{9,}          9 or more a's                                                \t         A tab
        //  (?>...)        An atomic group                                              \cY        The control character with the hex code Y
        //  (?=...)        A positive lookahead                                         \xYY       The character with the hex code YY
        //  (?!...)        A negative lookahead                                         \uYYYY     The character with the hex code YYYY
        //  (?<=...)       A positive lookbehind                                        .          Any character
        //  (?<!...)       A negative lookbehind                                        \Y         The Y'th captured group
        //  (?:...)        A non-capturing group                                        (?1)       Recurse into numbered group 1
        //  (...)          A capturing group                                            (?&x)      Recurse into named group x
        //  (?P<n>...)     A capturing group named n                                    (?P=n)     The captured group named 'n'
        //  \Q..\E         Remove special meaning                                       (?#...)    A comment (Escapes cannot be handled in comments)
        // 
        ///////////////////////////////////////////////////
        // 
        //  root          = branch;
        //  branch        = seq ('|' seq)*; # 目前不允许空串
        //  seq           = loop+;
        //  loop          = (group | literal) loop_flag_opt;  # /a/ 等价与 /a{1}/
        //  group         = '(' ('?' special_group | branch ')');
        //  special_group = comment | fatal("This group syntax is not yet supported")
        //  comment       = '#' (next_not(')') any_char)*  ')'
        //  loop_flag_opt = loop_flag ?;
        //  loop_flag     = ( '*' | '+' | '?' | loop_n | loop_mn | loop_m_comma | loop_comma_n ) loop_less_opt;
        //  loop_less_opt = '?'?;
        //  loop_n        = '{' int_num '}';
        //  loop_mn       = '{' int_num ',' int_num '}';
        //  loop_m_comma  = '{' int_num ',}';
        //  loop_comma_n  = '{,' int_num '}';
        //  int_num       = /[1-9][0-9]*|0/;
        //  
        //  literal       = charset | escape_char | digit | alpha | '.' | punct_char | fatal_if(loop_flag);
        //  punct_char    = set(R"punct( -<>&:!"'#%,;=@_`~}])punct");  # 非元字符的标点符号，不能出现诸如 "*+?(){[" 等元字符，但注意："}]" 不属于元字符
        //  chatset       = '[' negative_flag_opt charset_content ']'; # 注意： [^] 表示 [\x00-\xff], NoneOfEmpty; 而 [] 表示空字符集
        //  negative_flag_opt   = '^'?;
        //  charset_content     = (char_range | char_range_boundary)*;
        //  char_range_boundary = escape_char | not0x5d;      # not0x5d 必须在最后，让前面短路它
        //  not0x5d             = /[^\]]/;
        //  
        //  escape_char           = hex | qe_block | escape_char_one_alpha;
        //  escape_char_one_alpha = '\' set(
        //                            "tnvfr0dDsSwW"                # 字符集（缩写）
        //                            R"---(/\.^$*+?()[]{}|-)---"   # 原样输出
        //                           ); # @TODO 解耦写法 _r : 'r' ... 但没必要
        //  qe_block              = '\Q' (next_not('\E') .)* '\E'
        //  char_range            = char_range_boundary '-' char_range_boundary;   # 比如 [\x00-xff] 应当与 [\x00-x]|f 同构
        //  hex   = $hex();
        //  alpha = $alpha();
        //  digit = $digit();
        //
        ///////////////////////// parser ////////////////////////
        psr($digit) = range('0', '9');
        psr(digit) = $digit                                                                                        <<= ac_char;
        psr(alpha) = range('a', 'z')('A', 'Z')                                                                     <<= ac_char;
        psr(hex)   = (R"(\x)", ($digit('a', 'f')('A', 'F')[{2, 2}] | fatal_if(epsilon, "illegal hexadecimal"))     <<= ac_hex);

        psr(qe_block_content)      = (next_not(R"(\E)"), any_char)[any_cnt]                                        <<= ac_qe_block_content; // \Q..\E 内容原样输出
        psr(qe_block)              = (R"(\Q)", qe_block_content, R"(\E)"_psr[at_most_1]);
        psr(escape_char_one_alpha) = (R"(\)", (set("tnvfr0dDsSwW" R"---(/\.^$*+?()[]{}|-)---") | fatal_if(epsilon, "unsupported escape character")))   
                                                                                          <<= ac_escape_char_one_alpha; // 多字符集 & 原样输出

        decl_psr(char_range_boundary);

        psr(char_range)        = (char_range_boundary, "-", char_range_boundary)          <<= ac_char_range;
        psr(int_num)           = ((range('1', '9'), range('0', '9')[any_cnt]) | "0")      <<= ac_int_num;
        psr(negative_flag_opt) = "^"_psr[at_most_1]                                       <<= ac_negative_flag_opt; // ("^" | epsilon) 可以省掉一个 stack @TODO
        psr(punct_char)        = set(R"---( -<>&:!"'#%,;=@_`~}])---")                     <<= ac_char;
        psr(not0x5d)           = negative_set("]")                                        <<= ac_not0x5d;
        psr(escape_char)       = hex | qe_block | escape_char_one_alpha;
        char_range_boundary    = escape_char | not0x5d;

        psr(charset_content)   = (char_range | char_range_boundary)[any_cnt]              <<= ac_charset_content;
        psr(charset)           = ("[", negative_flag_opt, charset_content, "]")           <<= ac_charset;

        psr(dot) = "." <<= ac_dot;
        decl_psr(fatal_nothing2repeat); // 绑定 loop_flag

        psr(literal) = (charset | escape_char | digit | alpha | dot | punct_char | fatal_nothing2repeat) <<= ac_literal;

        psr(loop_less_opt) = "?"_psr[at_most_1]                                                          <<= ac_loop_less_opt;
        psr(loop_n)        = ("{", int_num, "}")                                                         <<= ac_loop_n;
        psr(loop_mn)       = ("{", int_num, ",", int_num, "}")                                           <<= ac_loop_mn;
        psr(loop_m_comma)  = ("{", int_num, ",}")                                                        <<= ac_loop_m_comma;
        psr(loop_comma_n)  = ("{,", int_num, "}")                                                        <<= ac_loop_comma_n;

        psr(loop_star)     = "*" <<= ac_loop_star;
        psr(loop_plus)     = "+" <<= ac_loop_plus;
        psr(question_mark) = "?" <<= ac_question_mark;

        psr(loop_flag) = ((loop_star | loop_plus | question_mark | loop_n | loop_mn | loop_m_comma | loop_comma_n), loop_less_opt);

        fatal_nothing2repeat = fatal_if(loop_flag, "nothing to repeat");
        psr(loop_flag_opt) = loop_flag[at_most_1] <<= ac_loop_flag_opt;  // 这里可以实现成  loop_flag | epsilon 这样就可以省掉一个 stack @TODO

        psr(comment)       = ("#", (next_not(")"), any_char)[any_cnt], ")")    <<= ac_comment;
        psr(special_group) = comment | fatal_if(epsilon, "This group syntax is not yet supported");

        decl_psr(group);

        psr(loop)   = ((group.weak() | literal), loop_flag_opt)                <<= ac_loop;
        psr(seq)    = loop[at_least_1]                                         <<= ac_seq;
        psr(branch) = (seq, ("|", seq)[any_cnt] <<= ac_branch_follow_up)       <<= ac_branch;

        group = ("(", (("?", special_group) | (branch, ")")));
        psr(root) = branch;

        ////////////////// end parser ///////////////////
        m_root = root;
        m_group = group;
        return true;
    }

private:
    lexpsr_shell::Parser m_root;
    lexpsr_shell::Parser m_group; 
}; // class RegexBuilder

////////////////////////////////////////////////////////////////////////////

#include <sstream>
namespace tests {

/////////////////////////////////////////////////////////////////////////////////////////////////

static inline void t_assert_abort(const char* msg, const char* file, unsigned line) {
    std::cerr << "[FATAL]: \"" << msg << "\", " << file << ":" << line << std::endl;
    std::abort();
}

static inline void t_expect(const char* msg, const char* file, unsigned line) {
    std::cerr << "[UNEXPECTED]: \"" << msg << "\", " << file << ":" << line << std::endl;
}

#undef TEST_ASSERT
#define TEST_ASSERT(expression) (void)( (!!(expression)) || (t_assert_abort((#expression), (__FILE__), (unsigned)(__LINE__)), 0) )

#undef TEST_EXPECT
#define TEST_EXPECT(expression) (void)( (!!(expression)) || (t_expect((#expression), (__FILE__), (unsigned)(__LINE__)), 0) )

namespace lexpsr {
struct Case {
	const char*    regex;
	const uint32_t flag;
	const char*    result;
};

static inline std::vector<Case> correct_expressions() {
    using namespace regex;
    // S: Seq; B: Branch; L: Loop; C: CharSet
    std::vector<Case> cases = {

        { "[abC123]",        0, R"===({C:"1-3,C,a-b"})===" },
        { "[^0-9_123]",      0, R"===({C:"0x00-0x2f,0x3a-0x5e,0x60-0xff"})===" },
        { "[a-z][^0-9_123]", 0, R"===({S:[{C:"a-z"},{C:"0x00-0x2f,0x3a-0x5e,0x60-0xff"}]})===" },
        { "[\\]]",           0, R"===({C:"0x5d"})===" },
        { "a*",              0, R"===({"L 0,INF":[{C:"a"}]})===" },
        { "a*a*",            0, R"===({S:[{"L 0,INF":[{C:"a"}]},{"L 0,INF":[{C:"a"}]}]})===" },
        { "(a*)*",           0, R"===({"L 0,INF":[{"L 0,INF":[{C:"a"}]}]})===" },       // 这里没有经过最小化
        { "a{1,2}",          0, R"===({"L 1,2":[{C:"a"}]})===" },
        { "a{1,2}?",         0, R"===({"L 1,2 less":[{C:"a"}]})===" },
        { "(abc{1}){2}",     0, R"===({"L 2,2":[{S:[{C:"a"},{C:"b"},{C:"c"}]}]})===" }, // c{1} 经过了最小化
        { "(abc{1}?){2}",    0, R"===({"L 2,2":[{S:[{C:"a"},{C:"b"},{C:"c"}]}]})===" }, // c{1} 经过了最小化
        { "c{2}?",           0, R"===({"L 2,2":[{C:"c"}]})===" },  // 这里的 less 经过了优化处理
        { "-",               0, R"===({C:"0x2d"})===" },
        { "\\r",             0, R"===({C:"0x0d"})===" },
        { "\\t\\t",          0, R"===({S:[{C:"0x09"},{C:"0x09"}]})===" },
        { "\\xab\\x12",      0, R"===({S:[{C:"0xab"},{C:"0x12"}]})===" },
        { "[a-b][1-9]",      0, R"===({S:[{C:"a-b"},{C:"1-9"}]})===" },
        { "12abc",           0, R"===({S:[{C:"1"},{C:"2"},{C:"a"},{C:"b"},{C:"c"}]})===" },
        { "ab",              0, R"===({S:[{C:"a"},{C:"b"}]})===" },
        { "\\r\\n",          0, R"===({S:[{C:"0x0d"},{C:"0x0a"}]})===" },
        { "  ",              0, R"===({S:[{C:"0x20"},{C:"0x20"}]})===" },
        { "..",              0, R"===({S:[{C:"0x00-0xff"},{C:"0x00-0xff"}]})===" },
        { "-a",              0, R"===({S:[{C:"0x2d"},{C:"a"}]})===" },
        { "--",              0, R"===({S:[{C:"0x2d"},{C:"0x2d"}]})===" },
        { "-.",              0, R"===({S:[{C:"0x2d"},{C:"0x00-0xff"}]})===" },
        { "230-.{,2000}",    0, R"===({S:[{C:"2"},{C:"3"},{C:"0"},{C:"0x2d"},{"L 0,2000":[{C:"0x00-0xff"}]}]})===" },
        { "(230-.{,2000})",  0, R"===({S:[{C:"2"},{C:"3"},{C:"0"},{C:"0x2d"},{"L 0,2000":[{C:"0x00-0xff"}]}]})===" },
        { "(230-.{,2000})?", 0, R"===({"L 0,1":[{S:[{C:"2"},{C:"3"},{C:"0"},{C:"0x2d"},{"L 0,2000":[{C:"0x00-0xff"}]}]}]})===" },
        { R"=((230-.{,2000})?(230\s).*?\r\n)=", 0,
           //R"===({S:[{"L 0,1":[{S:[{C:"2"},{C:"3"},{C:"0"},{C:"0x2d"},{"L 0,2000":[{C:"0x00-0xff"}]}]}]},{S:[{C:"2"},{C:"3"},{C:"0"},{C:"0x09-0x0d,0x20"}]},{"L 0,INF less":[{C:"0x00-0xff"}]},{C:"0x0d"},{C:"0x0a"}]})===" },
           R"===({S:[{"L 0,1":[{S:[{C:"2"},{C:"3"},{C:"0"},{C:"0x2d"},{"L 0,2000":[{C:"0x00-0xff"}]}]}]},{C:"2"},{C:"3"},{C:"0"},{C:"0x09-0x0d,0x20"},{"L 0,INF less":[{C:"0x00-0xff"}]},{C:"0x0d"},{C:"0x0a"}]})===" }, // Seq 做了合并
        { "a[/]c",           0, R"===({S:[{C:"a"},{C:"0x2f"},{C:"c"}]})===" },
        { R"=(a[\/]c)=",     0, R"===({S:[{C:"a"},{C:"0x2f"},{C:"c"}]})===" },
        { R"=([\S]\s)=",     0, R"===({S:[{C:"0x00-0x08,0x0e-0x1f,0x21-0xff"},{C:"0x09-0x0d,0x20"}]})===" },
        { R"=([^\D]\d)=",    0, R"===({S:[{C:"0-9"},{C:"0-9"}]})===" },
        { R"=(\D)=",         0, R"===({C:"0x00-0x2f,0x3a-0xff"})===" },
        { R"=(\S)=",         0, R"===({C:"0x00-0x08,0x0e-0x1f,0x21-0xff"})===" },
        { R"=([\x00-\x2f\x3a-\xff])=",                             0, R"===({C:"0x00-0x2f,0x3a-0xff"})===" },
        { R"=([\x00-\x08\x0e-\x1f\x21-\xff])=",                    0, R"===({C:"0x00-0x08,0x0e-0x1f,0x21-0xff"})===" },
        { R"=([\x00-\x2f\x3a-\xff\x00-\x08\x0e-\x1f\x21-\xff])=",  0, R"===({C:"0x00-0xff"})===" },
        { R"=([\D\S])=",     0, R"===({C:"0x00-0xff"})===" },
        { R"=([^\D\S])=",    0, R"===({C:""})===" },
        { R"=([^\Dabc])=",   0, R"===({C:"0-9"})===" },
        { R"=(\xff\x00[][^])=", 0, R"===({S:[{C:"0xff"},{C:"0x00"},{C:""},{C:"0x00-0xff"}]})===" },

        // 注意这里的循环
        { R"=(.*abc)=",           0, R"===({S:[{"L 0,INF":[{C:"0x00-0xff"}]},{C:"a"},{C:"b"},{C:"c"}]})===" },
        { R"=(.*?abc)=",          0, R"===({S:[{"L 0,INF less":[{C:"0x00-0xff"}]},{C:"a"},{C:"b"},{C:"c"}]})===" },
        { R"=(.*(abc))=",         0, R"===({S:[{"L 0,INF":[{C:"0x00-0xff"}]},{C:"a"},{C:"b"},{C:"c"}]})===" }, // Seq 做了合并
        { R"=(.{2,100}(abc))=",   0, R"===({S:[{"L 2,100":[{C:"0x00-0xff"}]},{C:"a"},{C:"b"},{C:"c"}]})===" }, // Seq 做了合并

        //// 分支
        { R"=(((abc)|[\D])*)=",   0, R"===({"L 0,INF":[{B:[{S:[{C:"a"},{C:"b"},{C:"c"}]},{C:"0x00-0x2f,0x3a-0xff"}]}]})===" },

        // 忽略大小写
        { R"=([^\x00-\x60\x7b-\xff])=",  0 | Flag::CASELESS, R"===({C:"A-Z,a-z"})===" }, // [a-z]/i
        { R"=([^\x00-\x40\x5b-\xff])=",  0 | Flag::CASELESS, R"===({C:"A-Z,a-z"})===" }, // [A-Z]/i
        { R"=([abC123]abC123)=",         0 | Flag::CASELESS, R"===({S:[{C:"1-3,A-C,a-c"},{C:"A,a"},{C:"B,b"},{C:"C,c"},{C:"1"},{C:"2"},{C:"3"}]})===" },
        { R"=([a-b]C123)=",              0 | Flag::CASELESS, R"===({S:[{C:"A-B,a-b"},{C:"C,c"},{C:"1"},{C:"2"},{C:"3"}]})===" },

        // dot 不匹配所有: 1. 字符集中的[.] 不被当做元字符处理 2. 启用 DOT_NOT_ALL 不匹配 '\r' 和 '\n' （区别于 hyperscan）
        { R"=([.].)=", 0 | Flag::DOT_NOT_ALL, R"===({S:[{C:"0x2e"},{C:"0x00-0x09,0x0b-0x0c,0x0e-0xff"}]})===" },

        // 混合 flags
        { R"=([.abC].abc)=", Flag::CASELESS | Flag::DOT_NOT_ALL, R"===({S:[{C:"0x2e,A-C,a-c"},{C:"0x00-0x09,0x0b-0x0c,0x0e-0xff"},{C:"A,a"},{C:"B,b"},{C:"C,c"}]})===" },

        // charset
        { R"=([--]])=",    0,  R"===({S:[{C:"0x2d"},{C:"0x5d"}]})===" },
        { R"=([%-9])=",    0,  R"===({C:"0x25-9"})===" },
        { R"=([\x00--])=", 0,  R"===({C:"0x00-0x2d"})===" },
        { R"=([--9])=",    0,  R"===({C:"0x2d-9"})===" },
        { R"=([---])=",    0,  R"===({C:"0x2d"})===" },

        // 漏掉一个 '\'
        { R"=([^\x00-x60\x7b-\xff])=", 0, R"===({C:"y-z"})===" },
        { R"=([\x00-xff])=",           0, R"===({C:"0x00-x"})===" },
        { R"=([\x00-x]|f)=",           0, R"===({B:[{C:"0x00-x"},{C:"f"}]})===" },

        // 特殊字符 
        { R"=(]})=",      0, R"===({S:[{C:"0x5d"},{C:"0x7d"}]})===" },
        { R"=(.{1,3}})=", 0, R"===({S:[{"L 1,3":[{C:"0x00-0xff"}]},{C:"0x7d"}]})===" },

        // 测试 Seq 合并
        { R"=(\Qabc\Ed)=", 0, R"===({S:[{C:"a"},{C:"b"},{C:"c"},{C:"d"}]})===" },
        { R"=(\Qabc\Ed\Qe\E\Qfg\E(hi))=", 0, R"===({S:[{C:"a"},{C:"b"},{C:"c"},{C:"d"},{C:"e"},{C:"f"},{C:"g"},{C:"h"},{C:"i"}]})===" },

        // 测试 Branch 合并
        { R"=(a|(b|cd)|e)=", 0, R"===({B:[{C:"a"},{C:"b"},{S:[{C:"c"},{C:"d"}]},{C:"e"}]})==="},

        // 注释 (?#  commment )
        { R"=((?#comment\)a(?#)b(?#comment)c)=", 0, R"===({S:[{C:"a"},{C:"b"},{C:"c"}]})===" },   // 注释中不能包含右括号
        { R"=((?#)(?#comment))=", 0, R"===()===" },   // 测试空正则

        // \Q..\E
        { R"=(a\Q\Q{1,2}[a-z]\n\\Eb?)=", 0,
             R"===({S:[{C:"a"},{C:"0x5c"},{C:"Q"},{C:"0x7b"},{C:"1"},{C:"0x2c"},{C:"2"},{C:"0x7d"},{C:"0x5b"},{C:"a"},{C:"0x2d"},{C:"z"},{C:"0x5d"},{C:"0x5c"},{C:"n"},{C:"0x5c"},{"L 0,1":[{C:"b"}]}]})===" },
        { R"=(\Qab.c\E)=",               0 | Flag::CASELESS,   R"===({S:[{C:"A,a"},{C:"B,b"},{C:"0x2e"},{C:"C,c"}]})===" },  // 测试忽略大小写
        { R"=(\Q.*[1-5]\x2E)=",          0,                    R"===({S:[{C:"0x2e"},{C:"0x2a"},{C:"0x5b"},{C:"1"},{C:"0x2d"},{C:"5"},{C:"0x5d"},{C:"0x5c"},{C:"x"},{C:"2"},{C:"E"}]})===" },  // 测试无 \E 右边界
        { R"=(\Q\E)=",                   0,                    R"===()===" },  // 测试 空串
        { R"=(\Q)=",                     0,                    R"===()===" },  // 测试无 \E 右边界的 空串
        { R"=((\Q\E){2,3})=",            0,                    R"===()===" },  // 测试空循环体
    };

    return cases;
} // fuction correct_expressions()

static inline std::vector<std::pair<std::string, uint32_t>> wrong_expressions() {
    // 错误的正则表达式用例
    std::vector<std::pair<std::string, uint32_t>> cases = {
        { "a**",       0 }, // nothing to repeat
        { "a{1,2}??",  0 }, // nothong to repeat
        { R"=(\x0w)=", 0 }, // 非法的十六进制 : illegal hexadecimal
        { R"=([a-z][^0-9_123]?????(230-.{,2000})?(230 ).*?\r\n)=", 0 }, // nothing to repeat
        { R"=(a{3,1})=", 0 }, // m > n : out of order
        { R"=([7-\r])=", 0 }, // m > n : out of order
    };
    return cases;
} // function wrong_expressions() 

////////////////////////////////////////////////////////////////

void test_regex2() {
    using namespace regex;

    RegexBuilder rbuilder;

    for (const Case& cs : correct_expressions()) {
        const std::string script(cs.regex);
        RegexAST ast;
        std::string err;
        if (!rbuilder.to_ast(script, cs.flag, ast, err)) {
           std::cerr << err << std::endl;
           continue;
        }

        const std::string dump = regex::Dump(ast);
        assert(dump == cs.result);  // test !!!

        finite_automata::EpsilonNFA enfa;
        if (rbuilder.ast_to_nfa(ast, enfa, err)) {
            std::cout << script << std::endl;
            std::cout << cs.flag << std::endl;
            std::cout << dump << std::endl;
            std::cout << enfa.to_mermaid("\n") << std::endl;
            std::cout << std::endl;
        }
        else {
            std::cout << script << std::endl;
            std::cout << cs.flag << std::endl;
            std::cout << dump << std::endl;
            std::cerr << err << std::endl;
            std::cout << std::endl;
        }
    }


    for (auto&& cs : wrong_expressions()) {
        auto global_modifiers = cs.second;
        std::string err;
        RegexAST ast;
        const std::string script(cs.first);
        if (!rbuilder.to_ast(script, global_modifiers, ast, err)) {
           std::cerr << err << std::endl;
        }
        else {
           assert(false && "wrong_expression");
        }
    }

    std::cout << "-----------" << std::endl;
}

void test_regex() {
    using namespace lexpsr_shell;
    using namespace regex;

    struct Context : core::Context {
        explicit Context(RegexAST& ast) : m_ast(ast) {}
        void Reset() {
            core::Context::Reset();

            m_int_num_stack.clear();
            m_negative_flag_opt_stack.clear();
            m_branch_cnt_stack.clear();
            m_charset_content_stack.clear();
            m_loop_less_opt_stack.clear();
            m_loop_flag_opt_stack.clear();
            m_loop_cnt_range.clear();
            m_ast_charset_stack.clear();
            m_ast_node_stack.clear();
            m_global_modifiers = 0;
            m_comments_cnt     = 0;
            ResetLiteralCharsetCnt();

            m_ast.Reset();
        }

        void ResetLiteralCharsetCnt() {
            m_literal_charset_cnt = 1u;       // literal 字符集个数，默认为 1
            m_literal_leading_word_bytes = 0; // literal 默认没有引导词
        }

        bool Finish() {
            if (1u == m_ast_node_stack.size()) {
                m_ast.SetRoot(m_ast_node_stack.back());
                m_ast_node_stack.pop_back();
                return true;
            }
            return false;
        }

        std::vector<std::size_t>    m_int_num_stack;
        std::vector<std::size_t>    m_negative_flag_opt_stack;
        std::vector<std::size_t>    m_branch_cnt_stack;
        std::vector<std::size_t>    m_charset_content_stack;
        std::vector<std::size_t>    m_loop_less_opt_stack; // 非贪婪标识
        std::vector<std::size_t>    m_loop_flag_opt_stack; // 是否存在显式循环次数标识
        std::vector<std::pair<std::size_t, std::size_t>> m_loop_cnt_range; // 循环次数

        std::vector<RegexAST::CharSet*>    m_ast_charset_stack;
        std::vector<RegexAST::Node*>       m_ast_node_stack;

        RegexAST&                          m_ast;
        uint32_t                           m_global_modifiers = 0;
        uint32_t                           m_comments_cnt     = 0;  // 注释的计数，用于修正 ac_seq 中的 loop_cnt
        uint32_t                           m_literal_charset_cnt        = 1;  // literal 字符（集）个数
        uint32_t                           m_literal_leading_word_bytes = 0;  // literal 引导词的长度，比如 \Q..\E 的引导词长度为 2
    }; // Context
    static const auto Ctx = [](core::Context& ctx) -> Context& { return static_cast<Context&>(ctx); };

    ///////////////////////// actions ///////////////////
    auto ac_char = [](const ActionArgs& args) {
        const core::StrRef& tok = args.m_action_material.m_token;
        assert(1u == tok.len);
        auto&& ctx = Ctx(args.m_contex);
        ctx.m_ast_charset_stack.push_back(ctx.m_ast.CreateSingleCharSet(tok[0]));
        return true;
    };

    auto ac_hex = [](const ActionArgs& args) {
        const core::StrRef& tok = args.m_action_material.m_token;
        assert(2u == tok.len);
        uint8_t first  = tok[0];
        uint8_t second = tok[1];
        auto xx = [](uint8_t c) -> uint8_t {
            if ('0' <= c && c <= '9') { return c - '0'; }
            if ('a' <= c && c <= 'f') { return c - 'a' + 10; }
            assert('A' <= c && c <= 'F');
            return c - 'A' + 10;
        };
        uint8_t res = (xx(first) << 4 | xx(second)) & 0xff;
        auto&& ctx = Ctx(args.m_contex);
        ctx.m_ast_charset_stack.push_back(ctx.m_ast.CreateSingleCharSet(res));
        return true;
    };

    auto ac_qe_block_content = [](const ActionArgs& args) {
        const core::StrRef& tok = args.m_action_material.m_token;
        auto&& ctx = Ctx(args.m_contex);
        for (std::size_t i = 0; i < tok.len; ++i) {
            ctx.m_ast_charset_stack.push_back(ctx.m_ast.CreateSingleCharSet(tok[i]));
        }
        assert(tok.len <= UINT32_MAX);
        ctx.m_literal_charset_cnt        = (uint32_t)tok.len; // \Q...\E 的字符（集）个数
        ctx.m_literal_leading_word_bytes = 2u;                // \Q..\E 的引导词长度为 2 : '\Q' 两个字符
        return true;
    };

    auto ac_escape_char_one_alpha = [](const ActionArgs& args) {
        const core::StrRef& tok = args.m_action_material.m_token;
        auto&& ctx = Ctx(args.m_contex);
        assert(2u == tok.len);
        assert('\\' == tok[0]);
        RegexAST::CharSet* cs = ctx.m_ast.CreateNormalCharSet(false);
        assert(cs->IsPositive());
        const uint8_t escape_char = tok[1];
        switch (escape_char) { // @TODO 未与 psr 解耦
            ///// 多字符集 ////
        case 't': cs->AddDiscrete('\t'); break;
        case 'n': cs->AddDiscrete('\n'); break;
        case 'v': cs->AddDiscrete('\v'); break;
        case 'f': cs->AddDiscrete('\f'); break;
        case 'r': cs->AddDiscrete('\r'); break;
        case '0': cs->AddDiscrete('\0'); break;
        case 'D': cs->SetNegativeFlag(); // [[fallthrough]]
        case 'd': cs->AddRange('0', '9'); break;
        case 'S': cs->SetNegativeFlag(); // [[fallthrough]]
        case 's': cs->AddDiscrete('\t').AddDiscrete('\n').AddDiscrete('\v').AddDiscrete('\f').AddDiscrete('\r').AddDiscrete(' '); break;
        case 'W': cs->SetNegativeFlag(); // [[fallthrough]]
        case 'w': cs->AddDiscrete('_').AddRange('0', '9').AddRange('a', 'z').AddRange('A', 'Z'); break;
        default:
            cs->~CharSet();
            new (cs) RegexAST::CharSet(RegexAST::CharSet::SingleCharConstruct, escape_char);
            assert(cs->IsPositive() && cs->IsSingleChar());
            break;
        }
        ctx.m_ast_charset_stack.push_back(cs);
        return true;
    };

    auto ac_char_range = [](const ActionArgs& args) {
        const core::StrRef& tok = args.m_action_material.m_token;
        auto&& ctx = Ctx(args.m_contex);
        assert(ctx.m_ast_charset_stack.size() >= 2u);
        RegexAST::CharSet* right = ctx.m_ast_charset_stack.back(); ctx.m_ast_charset_stack.pop_back();
        RegexAST::CharSet* left = ctx.m_ast_charset_stack.back(); // ctx.m_ast_charset_stack.pop_back();

        uint8_t _min = 0, _max = 0;
        if (left->SingleChar(_min) && right->SingleChar(_max)) {
            if (_min > _max) {
                args.m_error_message = "Range out of order in character class : " + tok.to_std_string();
                return false;
            }

            if (_min != _max) {
                left->~CharSet();
                new (left) RegexAST::CharSet(RegexAST::CharSet::NormalConstruct, false);
                assert(left->IsPositive());
                left->AddRange(_min, _max);
            } // else if (_min == _max) 时等价与 [_min] 无需对 left 做任何变动
            return true;
        }
        args.m_error_message = "logic_error: " + tok.to_std_string(); // 文件要求， [left-right] 必须时 SingleChar
        return false;
    };

    auto ac_int_num = [](const ActionArgs& args) {
        const core::StrRef& tok = args.m_action_material.m_token;
        auto&& ctx = Ctx(args.m_contex);
        assert(tok.data && tok.len >= 1u);
        if (0 == tok.len) { return false; }

        uint64_t value = 0;
        auto calc = [&value](uint8_t c) {
            std::size_t old = value;
            value = value * 10u + (c - '0');
            if (value < old) {
                return false;
            }
            return true;
        };

        for (std::size_t i = 0; i < tok.len; ++i) {
            if (!calc((uint8_t)tok[i])) {
                args.m_error_message = "int_num overflow: " + tok.to_std_string();
                return false;
            }
        }
        ctx.m_int_num_stack.push_back(value);
        return true;
    };

    auto ac_negative_flag_opt = [](const ActionArgs& args) {
        auto&& ctx = Ctx(args.m_contex);
        std::size_t loop_cnt = args.m_action_material.m_scanner_info;
        assert(loop_cnt <= 1u);
        ctx.m_negative_flag_opt_stack.push_back(loop_cnt);
        return true;
    };

    auto ac_not0x5d = [](const ActionArgs& args) {
        const core::StrRef& tok = args.m_action_material.m_token;
        auto&& ctx = Ctx(args.m_contex);
        assert(1u == tok.len);
        RegexAST::CharSet* cs = ctx.m_ast.CreateSingleCharSet(tok[0]);
        ctx.m_ast_charset_stack.push_back(cs);
        return true;
    };

    auto ac_charset_content = [](const ActionArgs& args) {
        Ctx(args.m_contex).m_charset_content_stack.push_back(args.m_action_material.m_scanner_info); // loop_cnt
        return true;
    };

    auto ac_charset = [](const ActionArgs& args) {
        auto&& ctx = Ctx(args.m_contex);
        assert(!ctx.m_negative_flag_opt_stack.empty());
        assert(!ctx.m_charset_content_stack.empty());

        bool negative_flag = (0 != ctx.m_negative_flag_opt_stack.back()); // 是否存在负字符集
        ctx.m_negative_flag_opt_stack.pop_back();

        std::size_t content_cnt = ctx.m_charset_content_stack.back(); ctx.m_charset_content_stack.pop_back();
        assert(content_cnt <= ctx.m_ast_charset_stack.size());
        if (0 == content_cnt) { // [] or [^]
            assert(ctx.m_ast_charset_stack.empty());
            RegexAST::CharSet* cs = ctx.m_ast.CreateNormalCharSet(negative_flag);
            ctx.m_ast_charset_stack.push_back(cs);
            return true;
        }

        assert(!ctx.m_ast_charset_stack.empty());
        std::size_t charset_stack_fitsize = ctx.m_ast_charset_stack.size() - content_cnt;
        auto iter = ctx.m_ast_charset_stack.begin();
        RegexAST::CharSet* cs = *iter++;

        for (; iter != ctx.m_ast_charset_stack.end(); ++iter) {
            *cs |= std::move(**iter);
        }
        assert(ctx.m_ast_charset_stack.size() >= charset_stack_fitsize + 1u);
        ctx.m_ast_charset_stack.resize(charset_stack_fitsize + 1u);

        if (negative_flag) {
            if (cs->IsPositive()) {
                cs->SetNegativeFlag();
            }
            else {
                cs->SetPositiveFlag();
            }
        }

        return true;
    };

    auto ac_dot = [](const ActionArgs& args) {
        auto&& ctx = Ctx(args.m_contex);
        RegexAST::CharSet* cs = ctx.m_ast.CreateNormalCharSet(true);
        assert(cs->IsNegative() && cs->IsAnyChar());
        if (ctx.m_global_modifiers & (uint32_t)Flag::DOT_NOT_ALL) {
            cs->AddDiscrete('\r').AddDiscrete('\n');
        }
        // 默认“点”是匹配所有字符（与其他正则引擎不同）
        ctx.m_ast_charset_stack.push_back(cs);
        return true;
    };

    auto ac_literal = [](const ActionArgs& args) {
        const core::StrRef& tok = args.m_action_material.m_token;
        auto&& ctx = Ctx(args.m_contex);
        assert(ctx.m_ast_charset_stack.size() >= ctx.m_literal_charset_cnt);
        if (1u == ctx.m_literal_charset_cnt) { // literal 字符集个数为 1
            RegexAST::CharSet* cs = ctx.m_ast_charset_stack.back(); ctx.m_ast_charset_stack.pop_back();
            if (ctx.m_global_modifiers & (uint32_t)Flag::CASELESS) {
                cs->ToCaseless();
            }
            RegexAST::Node* node = ctx.m_ast.CreateNode(RegexAST::NodeType::CharSet, tok, cs);
            ctx.m_ast_node_stack.push_back(node);
        }
        else if (ctx.m_literal_charset_cnt > 1u){ // literal 字符集个数不为 1，（多字符）可以构成一个序列
            const std::size_t hold_cnt = ctx.m_ast_charset_stack.size() - ctx.m_literal_charset_cnt;
            RegexAST::Node* node = ctx.m_ast.CreateNode(RegexAST::NodeType::Sequence, tok);
            ctx.m_ast_node_stack.push_back(node);
            // add member
            for (std::size_t i = 0; i < ctx.m_literal_charset_cnt; ++i) {
                RegexAST::CharSet* cs = ctx.m_ast_charset_stack[hold_cnt + i];
                if (ctx.m_global_modifiers & (uint32_t)Flag::CASELESS) {
                    cs->ToCaseless();
                }
                core::StrRef _tok = { tok.data + ctx.m_literal_leading_word_bytes + i, 1u }; // 一个 CharSet 对应着 一个字符， 还要考虑移除引导词
                assert(_tok.data < tok.data + tok.len); // 不能越界
                RegexAST::Node* charNode = ctx.m_ast.CreateNode(RegexAST::NodeType::CharSet, _tok, cs);
                node->m_member.push_back(charNode);
            }
            ctx.m_ast_charset_stack.resize(hold_cnt);
        } // else // 比如 \Q\E 可以构造出一共空的 literal，对于空的 literal 什么都不生成

        ctx.ResetLiteralCharsetCnt();  // 重置 literal 字符集个数
        return true;
    };

    auto ac_loop_less_opt = [](const ActionArgs& args) {
        auto&& ctx = Ctx(args.m_contex);
        std::size_t loop_cnt = args.m_action_material.m_scanner_info;
        assert(loop_cnt <= 1u);
        if (ctx.m_global_modifiers & (uint32_t)Flag::NON_GREEDY) { // 全局非贪婪匹配
            ctx.m_loop_less_opt_stack.push_back(1u);
        }
        else {
            ctx.m_loop_less_opt_stack.push_back(loop_cnt);
        }
        return true;
    };

    auto ac_loop_n = [](const ActionArgs& args) {
        auto&& ctx = Ctx(args.m_contex);
        assert(ctx.m_int_num_stack.size() >= 1u);
        std::size_t n = ctx.m_int_num_stack.back(); ctx.m_int_num_stack.pop_back();
        ctx.m_loop_cnt_range.emplace_back(n, n);
        return true;
    };
    auto ac_loop_mn = [](const ActionArgs& args) {
        auto&& ctx = Ctx(args.m_contex);
        assert(ctx.m_int_num_stack.size() >= 2u);
        std::size_t n = ctx.m_int_num_stack.back(); ctx.m_int_num_stack.pop_back();
        std::size_t m = ctx.m_int_num_stack.back(); ctx.m_int_num_stack.pop_back();
        if (m > n) { // 这个判断过程可以转移到 lexpsr 层面解决（它现在有这个表达能力）
            const core::StrRef& tok = args.m_action_material.m_token;
            args.m_error_message = tok.to_std_string() + " : numbers out of order in {} quantifier";
            return false;
        }
        ctx.m_loop_cnt_range.emplace_back(m, n);
        return true;
    };
    auto ac_loop_m_comma = [](const ActionArgs& args) {
        auto&& ctx = Ctx(args.m_contex);
        assert(ctx.m_int_num_stack.size() >= 1u);
        std::size_t m = ctx.m_int_num_stack.back(); ctx.m_int_num_stack.pop_back();
        ctx.m_loop_cnt_range.emplace_back(m, core::Loop::INF_CNT);
        return true;
    };
    auto ac_loop_comma_n = [](const ActionArgs& args) {
        auto&& ctx = Ctx(args.m_contex);
        assert(ctx.m_int_num_stack.size() >= 1u);
        std::size_t n = ctx.m_int_num_stack.back(); ctx.m_int_num_stack.pop_back();
        ctx.m_loop_cnt_range.emplace_back(0, n);
        return true;
    };
    auto ac_loop_star = [](const ActionArgs& args) {
        Ctx(args.m_contex).m_loop_cnt_range.emplace_back(0, core::Loop::INF_CNT);
        return true;
    };
    auto ac_loop_plus = [](const ActionArgs& args) {
        Ctx(args.m_contex).m_loop_cnt_range.emplace_back(1u, core::Loop::INF_CNT);
        return true;
    };
    auto ac_question_mark = [](const ActionArgs& args) {
        Ctx(args.m_contex).m_loop_cnt_range.emplace_back(0, 1u);
        return true;
    };
    auto ac_loop_flag_opt = [](const ActionArgs& args) {
        std::size_t loop_cnt = args.m_action_material.m_scanner_info;
        assert(loop_cnt <= 1u);
        auto&& ctx = Ctx(args.m_contex);
        ctx.m_loop_flag_opt_stack.push_back(loop_cnt);
        return true;
    };
    auto ac_loop = [](const ActionArgs& args) {
        auto&& ctx = Ctx(args.m_contex);
        bool less = false; // 默认贪婪
        std::pair<std::size_t, std::size_t> loop_cnt_range = { 1, 1 };
        bool implicit_cnt = (0 != ctx.m_loop_flag_opt_stack.back()); ctx.m_loop_flag_opt_stack.pop_back();
        if (implicit_cnt) { // 显式书写了循环次数
            assert(!ctx.m_loop_cnt_range.empty() && !ctx.m_loop_less_opt_stack.empty());
            less = !!(ctx.m_loop_less_opt_stack.back()); ctx.m_loop_less_opt_stack.pop_back();
            loop_cnt_range = ctx.m_loop_cnt_range.back(); ctx.m_loop_cnt_range.pop_back();
        }

        bool left_eq_right_range = loop_cnt_range.first == loop_cnt_range.second;
        // 移除了 循环中判断 group 的设计后，可以放心的将 loop 也最小化了
        if (left_eq_right_range && 1u == loop_cnt_range.first) { // 最小化 loop
            return true;
        }

        // 循环体
        if (ctx.m_ast_node_stack.empty()) {
            return true; // 兼容空的循环体，比如 (\Q\E){2} 或 (){3,4}
        }
        RegexAST::Node* body = ctx.m_ast_node_stack.back(); ctx.m_ast_node_stack.pop_back();
        RegexAST::Node* node = ctx.m_ast.CreateNode(RegexAST::NodeType::Loop, args.m_action_material.m_token);
        if (left_eq_right_range && less) { // 这四种情况是同构的： {n,n}? <=> {n}? <=> {n.n} <=> {n}
            less = false; // 取最简单的（默认状态）: {n}
        }

        node->m_union_data.m_loop_flag = RegexAST::LoopFlag(loop_cnt_range, less);
        node->m_member.push_back(body);

        ctx.m_ast_node_stack.push_back(node);
        return true;
    };
    auto ac_comment = [](const ActionArgs& args) {
        auto&& ctx = Ctx(args.m_contex);
        ++ctx.m_comments_cnt; // 对注释计数，用于修正 ac_seq 的 loop_cnt
        return true;
    };
    auto ac_seq = [](const ActionArgs& args) {
        const core::StrRef& tok = args.m_action_material.m_token;
        auto&& ctx = Ctx(args.m_contex);
        assert(ctx.m_comments_cnt <= args.m_action_material.m_scanner_info);
        const std::size_t loop_cnt = args.m_action_material.m_scanner_info - ctx.m_comments_cnt; // 要排除掉注释的影响
        ctx.m_comments_cnt = 0; // 注释计数用完立刻清零
        if (ctx.m_ast_node_stack.empty()) { // 没有有效节点
            assert(0u == loop_cnt || 1u == loop_cnt);
            return true;
        }
        assert(ctx.m_ast_node_stack.size() >= loop_cnt && loop_cnt >= 1u);
        if (1u == loop_cnt) { // 最小化
            ctx.m_ast_node_stack.back()->m_context = tok;
            return true;
        }

        assert(ctx.m_ast_node_stack.size() >= loop_cnt);
        const std::size_t hold_cnt = ctx.m_ast_node_stack.size() - loop_cnt;
        RegexAST::Node* seq = ctx.m_ast.CreateNode(RegexAST::NodeType::Sequence, tok);
        for (std::size_t i = 0; i < loop_cnt; ++i) {
            RegexAST::Node* node = ctx.m_ast_node_stack[hold_cnt + i];
            assert(nullptr != node);
            if (RegexAST::NodeType::Sequence == node->m_type) { // 合并相邻的 Seq
                seq->m_member.insert(seq->m_member.end(), node->m_member.begin(), node->m_member.end());
            }
            else {
                seq->m_member.push_back(node);
            }
        }
        ctx.m_ast_node_stack.resize(hold_cnt);
        ctx.m_ast_node_stack.push_back(seq);
        return true;
    };

    auto ac_branch_follow_up = [](const ActionArgs& args) {
        std::size_t loop_cnt = args.m_action_material.m_scanner_info;
        Ctx(args.m_contex).m_branch_cnt_stack.push_back(loop_cnt);
        return true;
    };

    auto ac_branch = [](const ActionArgs& args) {
        const core::StrRef& tok = args.m_action_material.m_token;
        auto&& ctx = Ctx(args.m_contex);
        assert(!ctx.m_branch_cnt_stack.empty());
        std::size_t branch_cnt = ctx.m_branch_cnt_stack.back() + 1u; ctx.m_branch_cnt_stack.pop_back();
        if (1u == branch_cnt) { // 最小化
            if (!ctx.m_ast_node_stack.empty())
            {
                ctx.m_ast_node_stack.back()->m_context = tok;
            }
            return true;
        }

        assert(ctx.m_ast_node_stack.size() >= branch_cnt);
        const std::size_t hold_cnt = ctx.m_ast_node_stack.size() - branch_cnt;
        RegexAST::Node* branch = ctx.m_ast.CreateNode(RegexAST::NodeType::Branch, tok);
        for (std::size_t i = 0; i < branch_cnt; ++i) {
            RegexAST::Node* node = ctx.m_ast_node_stack[hold_cnt + i];
            assert(nullptr != node);
            if (RegexAST::NodeType::Branch == node->m_type) { // 合并相邻的 Branch
                branch->m_member.insert(branch->m_member.end(), node->m_member.begin(), node->m_member.end());
            }
            else {
                branch->m_member.push_back(node);
            }
        }
        ctx.m_ast_node_stack.resize(hold_cnt);
        ctx.m_ast_node_stack.push_back(branch);
        return true;
    };

    /////////////////////////
    // https://www.debuggex.com/cheatsheet/regex/pcre#/
    //
    //                                                   PCRE regex quick reference:
    //  [abx-z]        One character of: a, b, or the range x-z                     ^          Beginning of the string
    //  [^abx-z]       One character except: a, b, or the range x-z                 $          End of the string
    //  a|b            a or b                                                       \d         A digit (same as [0-9])
    //  a?             Zero or one a's (greedy)                                     \D         A non-digit (same as [^0-9])
    //  a??            Zero or one a's (lazy)                                       \w         A word character (same as [_a-zA-Z0-9])
    //  a*             Zero or more a's (greedy)                                    \W         A non-word character (same as [^_a-zA-Z0-9])
    //  a*?            Zero or more a's (lazy)                                      \s         A whitespace character
    //  a+             One or more a's (greedy)                                     \S         A non-whitespace character
    //  a+?            One or more a's (lazy)                                       \b         A word boundary
    //  a{4}           Exactly 4 a's                                                \B         A non-word boundary
    //  a{4,8}         Between (inclusive) 4 and 8 a's                              \n         A newline
    //  a{9,}          9 or more a's                                                \t         A tab
    //  (?>...)        An atomic group                                              \cY        The control character with the hex code Y
    //  (?=...)        A positive lookahead                                         \xYY       The character with the hex code YY
    //  (?!...)        A negative lookahead                                         \uYYYY     The character with the hex code YYYY
    //  (?<=...)       A positive lookbehind                                        .          Any character
    //  (?<!...)       A negative lookbehind                                        \Y         The Y'th captured group
    //  (?:...)        A non-capturing group                                        (?1)       Recurse into numbered group 1
    //  (...)          A capturing group                                            (?&x)      Recurse into named group x
    //  (?P<n>...)     A capturing group named n                                    (?P=n)     The captured group named 'n'
    //  \Q..\E         Remove special meaning                                       (?#...)    A comment (Escapes cannot be handled in comments)
    // 
    ///////////////////////////////////////////////////
    // 
    //  root          = branch;
    //  branch        = seq ('|' seq)*; # 目前不允许空串
    //  seq           = loop+;
    //  loop          = (group | literal) loop_flag_opt;  # /a/ 等价与 /a{1}/
    //  group         = '(' ('?' special_group | branch ')');
    //  special_group = comment | fatal("This group syntax is not yet supported")
    //  comment       = '#' (next_not(')') any_char)*  ')'
    //  loop_flag_opt = loop_flag ?;
    //  loop_flag     = ( '*' | '+' | '?' | loop_n | loop_mn | loop_m_comma | loop_comma_n ) loop_less_opt;
    //  loop_less_opt = '?'?;
    //  loop_n        = '{' int_num '}';
    //  loop_mn       = '{' int_num ',' int_num '}';
    //  loop_m_comma  = '{' int_num ',}';
    //  loop_comma_n  = '{,' int_num '}';
    //  int_num       = /[1-9][0-9]*|0/;
    //  
    //  literal       = charset | escape_char | digit | alpha | '.' | punct_char | fatal_if(loop_flag);
    //  punct_char    = set(R"punct( -<>&:!"'#%,;=@_`~}])punct");  # 非元字符的标点符号，不能出现诸如 "*+?(){[" 等元字符，但注意："}]" 不属于元字符
    //  chatset       = '[' negative_flag_opt charset_content ']'; # 注意： [^] 表示 [\x00-\xff], NoneOfEmpty; 而 [] 表示空字符集
    //  negative_flag_opt   = '^'?;
    //  charset_content     = (char_range | char_range_boundary)*;
    //  char_range_boundary = escape_char | not0x5d;      # not0x5d 必须在最后，让前面短路它
    //  not0x5d             = /[^\]]/;
    //  
    //  escape_char           = hex | qe_block | escape_char_one_alpha;
    //  escape_char_one_alpha = '\' set(
    //                            "tnvfr0dDsSwW"                # 字符集（缩写）
    //                            R"---(/\.^$*+?()[]{}|-)---"   # 原样输出
    //                           ); # @TODO 解耦写法 _r : 'r' ... 但没必要
    //  qe_block              = '\Q' (next_not('\E') .)* '\E'
    //  char_range            = char_range_boundary '-' char_range_boundary;   # 比如 [\x00-xff] 应当与 [\x00-x]|f 同构
    //  hex   = $hex();
    //  alpha = $alpha();
    //  digit = $digit();
    //
    ///////////////////////// parser ////////////////////////
    psr($digit) = range('0', '9');
    psr(digit) = $digit                                                                                        <<= ac_char;
    psr(alpha) = range('a', 'z')('A', 'Z')                                                                     <<= ac_char;
    psr(hex)   = (R"(\x)", ($digit('a', 'f')('A', 'F')[{2, 2}] | fatal_if(epsilon, "illegal hexadecimal"))     <<= ac_hex);

    psr(qe_block_content)      = (next_not(R"(\E)"), any_char)[any_cnt]                                        <<= ac_qe_block_content; // \Q..\E 内容原样输出
    psr(qe_block)              = (R"(\Q)", qe_block_content, R"(\E)"_psr[at_most_1]);
    psr(escape_char_one_alpha) = (R"(\)", (set("tnvfr0dDsSwW" R"---(/\.^$*+?()[]{}|-)---") | fatal_if(epsilon, "unsupported escape character")))   
                                                                                      <<= ac_escape_char_one_alpha; // 多字符集 & 原样输出

    decl_psr(char_range_boundary);

    psr(char_range)        = (char_range_boundary, "-", char_range_boundary)          <<= ac_char_range;
    psr(int_num)           = ((range('1', '9'), range('0', '9')[any_cnt]) | "0")      <<= ac_int_num;
    psr(negative_flag_opt) = "^"_psr[at_most_1]                                       <<= ac_negative_flag_opt; // ("^" | epsilon) 可以省掉一个 stack @TODO
    psr(punct_char)        = set(R"---( -<>&:!"'#%,;=@_`~}])---")                     <<= ac_char;
    psr(not0x5d)           = negative_set("]")                                        <<= ac_not0x5d;
    psr(escape_char)       = hex | qe_block | escape_char_one_alpha;
    char_range_boundary    = escape_char | not0x5d;

    psr(charset_content)   = (char_range | char_range_boundary)[any_cnt]              <<= ac_charset_content;
    psr(charset)           = ("[", negative_flag_opt, charset_content, "]")           <<= ac_charset;

    psr(dot) = "." <<= ac_dot;
    decl_psr(fatal_nothing2repeat); // 绑定 loop_flag

    psr(literal) = (charset | escape_char | digit | alpha | dot | punct_char | fatal_nothing2repeat) <<= ac_literal;

    psr(loop_less_opt) = "?"_psr[at_most_1]                                                          <<= ac_loop_less_opt;
    psr(loop_n)        = ("{", int_num, "}")                                                         <<= ac_loop_n;
    psr(loop_mn)       = ("{", int_num, ",", int_num, "}")                                           <<= ac_loop_mn;
    psr(loop_m_comma)  = ("{", int_num, ",}")                                                        <<= ac_loop_m_comma;
    psr(loop_comma_n)  = ("{,", int_num, "}")                                                        <<= ac_loop_comma_n;

    psr(loop_star)     = "*" <<= ac_loop_star;
    psr(loop_plus)     = "+" <<= ac_loop_plus;
    psr(question_mark) = "?" <<= ac_question_mark;

    psr(loop_flag) = ((loop_star | loop_plus | question_mark | loop_n | loop_mn | loop_m_comma | loop_comma_n), loop_less_opt);

    fatal_nothing2repeat = fatal_if(loop_flag, "nothing to repeat");
    psr(loop_flag_opt) = loop_flag[at_most_1] <<= ac_loop_flag_opt;  // 这里可以实现成  loop_flag | epsilon 这样就可以省掉一个 stack @TODO

    psr(comment)       = ("#", (next_not(")"), any_char)[any_cnt], ")")    <<= ac_comment;
    psr(special_group) = comment | fatal_if(epsilon, "This group syntax is not yet supported");

    decl_psr(group);

    psr(loop)   = ((group.weak() | literal), loop_flag_opt)                <<= ac_loop;
    psr(seq)    = loop[at_least_1]                                         <<= ac_seq;
    psr(branch) = (seq, ("|", seq)[any_cnt] <<= ac_branch_follow_up)       <<= ac_branch;

    group = ("(", (("?", special_group) | (branch, ")")));
    psr(root) = branch;

    ////////////////// end parser ///////////////////

    regex::RegexAST ast;
    Context ctx(ast);
    std::size_t offset = 0;
    std::string err;

    auto reset = [&ctx, &offset, &err]() {
        ctx.Reset();
        offset = 0;
        err.clear();
    };


    for (const Case& cs : correct_expressions()) {
        reset();
        ctx.m_global_modifiers = cs.flag;
        const std::string script(cs.regex);
        ScanState ss = root.ScanScript(script.data(), script.size(), offset, ctx, err);
        if (ScanState::OK != ss || script.size() != offset) {
            std::cerr << err << std::endl;
            std::cerr << ctx.ErrorPrompts(offset, script) << std::endl;
        }
        else {
            assert(script.size() == offset);
            auto res = InvokeActions(ctx, err);
            if (!res.first) {
                std::cerr << err << std::endl;
            }

            assert(res.first);  // test !!!
        }
        assert(ScanState::OK == ss && script.size() == offset); // test !!!
        ctx.Finish();
        const std::string dump = regex::Dump(ast);
        assert(dump == cs.result);  // test !!!

        finite_automata::EpsilonNFA enfa;
        if (regex::ToEpsilonNFA(ast, enfa, err)) {
            std::cout << script << std::endl;
            std::cout << cs.flag << std::endl;
            std::cout << dump << std::endl;
            std::cout << enfa.to_mermaid("\n") << std::endl;
            std::cout << std::endl;
        }
        else {
            std::cout << script << std::endl;
            std::cout << cs.flag << std::endl;
            std::cout << dump << std::endl;
            std::cerr << err << std::endl;
            std::cout << std::endl;
        }
    }

    // 错误的正则表达式用例
    for (auto&& cs : wrong_expressions()) {
        reset();
        ctx.m_global_modifiers = cs.second;
        const std::string script(cs.first);
        ScanState ss = root.ScanScript(script.data(), script.size(), offset, ctx, err);
        if (ScanState::OK != ss || script.size() != offset) {
            std::cerr << err << std::endl;
            std::cerr << ctx.ErrorPrompts(offset, script) << std::endl;
        }
        else {
            assert(script.size() == offset);
            auto res = InvokeActions(ctx, err);
            if (!res.first) {
                std::cerr << err << std::endl;
            }

            assert(false == res.first);  // test !!!
        }
    }

    std::cout << "-----------" << std::endl;

} // fuction test_regex()
} // namespace lexpsr

namespace fa {

typedef std::unordered_set<std::string> mermaid_result_t;
static inline mermaid_result_t format_mermaid_result(const std::string& input_string) {
    mermaid_result_t ret;
    std::stringstream ss(input_string);
    std::string line;
    while (std::getline(ss, line, '\n')) {
        if (!ret.insert(std::move(line)).second) {
            throw("Duplicate state pairs have been found!");
        }
    }
    return ret;
}

using namespace finite_automata;

int test_case1() {
    EpsilonNFA eNFA;

    eNFA.move_func(0, epsilon, { 2,3 });
    eNFA.move_func(0, 'a', { 1 });

    eNFA.move_func(1, 'b', { 2 });
    eNFA.move_func(2, epsilon, { 0 });
    eNFA.move_func(2, 'c', { 3 });

    eNFA.move_func(3, epsilon, { 0 });

    eNFA.final_states({ 3 });
    eNFA.start_state(0);

    std::string mermaid_result = eNFA.to_mermaid("\n");
    std::cout << std::endl << "e-NFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    std::string expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- a --> 1;
    0(0-S)  -- ε --> 3(3-E);
    0(0-S)  -- ε --> 2;
    2  -- ε --> 0(0-S);
    3(3-E)  -- ε --> 0(0-S);
    1  -- b --> 2;
    2  -- c --> 3(3-E);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    /////////////////

    std::string err;
    DFA dfa;
    if (!eNFA.to_dfa(dfa, err))
    {
        std::cerr << err << std::endl;
        return -1;
    }

    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S-E)  -- c --> 4(4-E);
    0(0-S-E)  -- a --> 1;
    4(4-E)  -- a --> 1;
    1  -- b --> 4(4-E);
    4(4-E)  -- c --> 4(4-E);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));
    return 0;
}

int test_case2() {
    std::cout << std::endl << "-------" << std::endl << R"==(`/((ab)*c)*/`)==" << std::endl;

    EpsilonNFA eNFA; //  regex: /((ab)*c)*/

    eNFA.move_func(0, epsilon, { 1, 8 });
    eNFA.move_func(1, epsilon, { 2, 5 });

    eNFA.move_func(2, 'a', { 3 });
    eNFA.move_func(3, 'b', { 4 });

    eNFA.move_func(4, epsilon, { 5 });

    eNFA.move_func(5, epsilon, { 1, 6 });

    eNFA.move_func(6, 'c', { 7 });
    eNFA.move_func(7, epsilon, { 8 });
    eNFA.move_func(8, epsilon, { 0 });

    eNFA.final_states({ 8 });
    eNFA.start_state(0);

    std::string mermaid_result = eNFA.to_mermaid("\n");
    std::cout << std::endl << "e-NFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    std::string expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- ε --> 8(8-E);
    0(0-S)  -- ε --> 1;
    4  -- ε --> 5;
    8(8-E)  -- ε --> 0(0-S);
    1  -- ε --> 2;
    1  -- ε --> 5;
    5  -- ε --> 1;
    2  -- a --> 3;
    3  -- b --> 4;
    5  -- ε --> 6;
    6  -- c --> 7;
    7  -- ε --> 8(8-E);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    /////////////////

    std::string err;
    DFA dfa;
    if (!eNFA.to_dfa(dfa, err))
    {
        std::cerr << err << std::endl;
        return -1;
    }

    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S-E)  -- c --> 10(10-E);
    0(0-S-E)  -- a --> 3;
    9  -- c --> 10(10-E);
    9  -- a --> 3;
    3  -- b --> 9;
    10(10-E)  -- a --> 3;
    10(10-E)  -- c --> 10(10-E);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));
    return 0;
}

int test_case3() {
    std::cout << std::endl << "-------" << std::endl << R"==(`/((ab)*c)*(ab)*/`)==" << std::endl;

    EpsilonNFA eNFA; //  regex: /((ab)*c)*(ab)*/

    eNFA.move_func(0, epsilon, { 1, 8 });
    eNFA.move_func(1, epsilon, { 2, 5 });

    eNFA.move_func(2, 'a', { 3 });
    eNFA.move_func(3, 'b', { 4 });

    eNFA.move_func(4, epsilon, { 5 });

    eNFA.move_func(5, epsilon, { 1, 6 });

    eNFA.move_func(6, 'c', { 7 });
    eNFA.move_func(7, epsilon, { 8 });
    eNFA.move_func(8, epsilon, { 0, 9 });

    eNFA.move_func(9, epsilon, { 10, 13 });
    eNFA.move_func(10, 'a', { 11 });
    eNFA.move_func(11, 'b', { 12 });
    eNFA.move_func(12, epsilon, { 13 });
    eNFA.move_func(13, epsilon, { 9 });

    eNFA.final_states({ 13 });
    eNFA.start_state(0);

    std::string mermaid_result = eNFA.to_mermaid("\n");
    std::cout << std::endl << "e-NFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    std::string expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- ε --> 8;
    0(0-S)  -- ε --> 1;
    4  -- ε --> 5;
    8  -- ε --> 0(0-S);
    1  -- ε --> 2;
    1  -- ε --> 5;
    5  -- ε --> 1;
    2  -- a --> 3;
    3  -- b --> 4;
    5  -- ε --> 6;
    6  -- c --> 7;
    7  -- ε --> 8;
    8  -- ε --> 9;
    9  -- ε --> 10;
    13(13-E)  -- ε --> 9;
    9  -- ε --> 13(13-E);
    10  -- a --> 11;
    11  -- b --> 12;
    12  -- ε --> 13(13-E);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    /////////////////

    std::string err;
    DFA dfa;
    if (!eNFA.to_dfa(dfa, err))
    {
        std::cerr << err << std::endl;
        return -1;
    }

    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S-E)  -- c --> 16(16-E);
    0(0-S-E)  -- a --> 14;
    16(16-E)  -- a --> 14;
    15(15-E)  -- a --> 14;
    14  -- b --> 15(15-E);
    15(15-E)  -- c --> 16(16-E);
    16(16-E)  -- c --> 16(16-E);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    dfa.try_minimize();
    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "minimized DFA: " << std::endl << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S-E)  -- c --> 0(0-S-E);
    0(0-S-E)  -- a --> 14;
    14  -- b --> 0(0-S-E);
```)==";
    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));


    {
        EpsilonNFA eNFA2;
        eNFA2.move_func(0, 'a', { 1 });
        eNFA2.move_func(0, 'c', { 5 });

        eNFA2.move_func(5, 'c', { 5 });
        eNFA2.move_func(5, 'a', { 1 });

        eNFA2.move_func(1, 'b', { 2 });
        eNFA2.move_func(2, 'c', { 3 });
        eNFA2.move_func(2, 'a', { 1 });

        eNFA2.move_func(3, 'c', { 4 });
        eNFA2.move_func(3, 'a', { 1 });
        eNFA2.move_func(4, 'c', { 4 });
        eNFA2.move_func(4, 'a', { 1 });

        eNFA2.final_states({0,2,3,4,5});
        eNFA2.start_state(0);

        std::string s1 = eNFA2.to_mermaid();
        std::string err2;
        DFA dfa2;
        if (!eNFA2.to_dfa(dfa2, err))
        {
            std::cerr << err2 << std::endl;
            return -1;
        }
        std::string s2 = dfa2.to_mermaid();
        dfa2.try_minimize();
        std::string s3 = dfa2.to_mermaid();

        std::cout << "e-NFA: " << std::endl << s1 << std::endl;
        std::cout << "DFA: " << std::endl << s2 << std::endl;
        std::cout << "minimized DFA: " << std::endl << s3 << std::endl;

        // expected : mermaid_result == s3

        TEST_ASSERT(dfa2.is_same_shape_mut(dfa));
    }

    return 0;
}

int test_case4() {
    std::cout << std::endl << "-------" << std::endl << R"==(`/[a-c]*ab/`)==" << std::endl;

    EpsilonNFA eNFA; //

    eNFA.move_func(0, epsilon, { 1 });
    eNFA.move_func(0, 'a', { 0 });
    eNFA.move_func(0, 'b', { 0 });
    eNFA.move_func(0, 'c', { 0 });

    eNFA.move_func(1, 'a', { 2 });
    eNFA.move_func(2, 'b', { 3 });

    eNFA.final_states({ 3 });
    eNFA.start_state(0);

    std::string mermaid_result = eNFA.to_mermaid("\n");
    std::cout << std::endl << "e-NFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    std::string expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- [a-c] --> 0(0-S);
    0(0-S)  -- ε --> 1;
    1  -- a --> 2;
    2  -- b --> 3(3-E);
```)==";

    TEST_ASSERT(mermaid_result == expected_result);

    /////////////////

    std::string err;
    DFA dfa;
    if (!eNFA.to_dfa(dfa, err))
    {
        std::cerr << err << std::endl;
        return -1;
    }

    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- [b-c] --> 6;
    0(0-S)  -- a --> 4;
    6  -- [b-c] --> 6;
    4  -- a --> 4;
    4  -- b --> 5(5-E);
    5(5-E)  -- a --> 4;
    5(5-E)  -- [b-c] --> 6;
    6  -- a --> 4;
    4  -- c --> 6;
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    dfa.try_minimize();
    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "minimize DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- [b-c] --> 0(0-S);
    0(0-S)  -- a --> 4;
    4  -- c --> 0(0-S);
    4  -- a --> 4;
    5(5-E)  -- a --> 4;
    4  -- b --> 5(5-E);
    5(5-E)  -- [b-c] --> 0(0-S);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    return 0;
}

int test_case5() {
    EpsilonNFA eNFA;
    eNFA.move_func(0, 'a', { 1,2 });
    eNFA.move_func(1, 'b', { 1 });
    eNFA.move_func(1, 'd', { 3 });
    eNFA.move_func(2, 'c', { 3 });
    eNFA.move_func(3, 'e', { 4 });

    eNFA.final_states({ 4 });
    eNFA.start_state(0);
    std::string mermaid_result = eNFA.to_mermaid("\n");
    std::cout << std::endl << "e-NFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    std::string expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- a --> 2;
    0(0-S)  -- a --> 1;
    1  -- b --> 1;
    1  -- d --> 3;
    2  -- c --> 3;
    3  -- e --> 4(4-E);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    //////////

    std::string err;
    DFA dfa;
    if (!eNFA.to_dfa(dfa, err))
    {
        std::cerr << err << std::endl;
        return -1;
    }

    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- a --> 5;
    5  -- b --> 1;
    1  -- b --> 1;
    1  -- d --> 3;
    3  -- e --> 4(4-E);
    5  -- [c-d] --> 3;
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));


    dfa.try_minimize();
    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "minimize DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- a --> 5;
    5  -- b --> 1;
    1  -- b --> 1;
    1  -- d --> 3;
    3  -- e --> 4(4-E);
    5  -- [c-d] --> 3;
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    return 0;
}

int test_case6() {
    EpsilonNFA eNFA;
    eNFA.move_func(0, 'a', { 1 });
    eNFA.move_func(1, 'a', { 2 });
    eNFA.move_func(2, 'a', { 3 });
    eNFA.move_func(3, 'a', { 4 });
    eNFA.move_func(4, 'a', { 1 });
    eNFA.move_func(3, 'a', { 5 });

    eNFA.final_states({ 5 });
    eNFA.start_state(0);
    std::string mermaid_result = eNFA.to_mermaid("\n");
    std::cout << std::endl << "e-NFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    std::string expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- a --> 1;
    1  -- a --> 2;
    4  -- a --> 1;
    2  -- a --> 3;
    3  -- a --> 4;
    3  -- a --> 5(5-E);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    std::string err;
    DFA dfa;
    if (!eNFA.to_dfa(dfa, err))
    {
        std::cerr << err << std::endl;
        return -1;
    }

    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- a --> 1;
    3  -- a --> 6(6-E);
    1  -- a --> 2;
    2  -- a --> 3;
    6(6-E)  -- a --> 1;
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));


    dfa.try_minimize();
    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "minimize DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;
    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));
    return 0;
}

int test_case7() { // 测试移除死状态 (目前这个case还不能通过！！)
    EpsilonNFA eNFA;
    eNFA.move_func(0, 'a', { 1 });
    eNFA.move_func(0, 'b', { 2 });
    eNFA.move_func(2, 'a', { 3 });
    eNFA.move_func(3, 'a', { 4 });
    eNFA.move_func(4, 'a', { 5 });
    eNFA.move_func(4, 'b', { 4 });
    eNFA.move_func(5, 'c', { 3 });
    eNFA.move_func(5, 'a', { 6 });
    eNFA.move_func(7, 'a', { 0 }); // 7 状态目前可以被移除
    eNFA.move_func(7, 'a', { 1 });

    eNFA.final_states({ 1 });
    eNFA.start_state(0);
    std::string mermaid_result = eNFA.to_mermaid("\n");
    std::cout << std::endl << "e-NFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    std::string expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- b --> 2;
    0(0-S)  -- a --> 1(1-E);
    2  -- a --> 3;
    4  -- a --> 5;
    5  -- a --> 6;
    4  -- b --> 4;
    5  -- c --> 3;
    7  -- a --> 1(1-E);
    3  -- a --> 4;
    7  -- a --> 0(0-S);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    std::string err;
    DFA dfa;
    if (!eNFA.to_dfa(dfa, err))
    {
        std::cerr << err << std::endl;
        return -1;
    }

    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- a --> 1(1-E);
```)==";

    TEST_EXPECT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));
    return 0;
}

int test_case8() {
    EpsilonNFA eNFA; // (ab|c)?[abc]+
    eNFA.move_func(0, 'a', { 1 });
    eNFA.move_func(0, 'b', { 2 });

    eNFA.final_states({ 1, 2 });
    eNFA.start_state(0);
    std::string mermaid_result = eNFA.to_mermaid("\n");
    std::cout << std::endl << "e-NFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    std::string expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- a --> 1(1-E);
    0(0-S)  -- b --> 2(2-E);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    std::string err;
    DFA dfa;
    if (!eNFA.to_dfa(dfa, err))
    {
        std::cerr << err << std::endl;
        return -1;
    }

    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- a --> 1(1-E);
    0(0-S)  -- b --> 2(2-E);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    DFA::minimize_policy p;
    p.reset_target_structure(DFA::minimize_policy::target_structure::to_both);

    dfa.try_minimize(p);
    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "minimize DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- [a-b] --> 1(1-E);
```)==";
    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));
    return 0;
}

int test_case9() {
    EpsilonNFA eNFA; // (ab|c)?[abc]+
    eNFA.move_func(0, epsilon, { 1 });
    eNFA.move_func(0, 'c', { 4 });
    eNFA.move_func(0, 'a', { 2 });

    eNFA.move_func(2, 'b', { 3 });
    eNFA.move_func(4, epsilon, { 1 });
    eNFA.move_func(3, epsilon, { 1 });

    eNFA.move_func(1, std::vector<Edge>{ 'a', 'b', 'c' }, { 5 });
    eNFA.move_func(5, std::vector<Edge>{ epsilon, 'a', 'b', 'c' }, { 6 });
    eNFA.move_func(6, epsilon, { 5 });

    eNFA.final_states({ 6 });
    eNFA.start_state(0);
    std::string mermaid_result = eNFA.to_mermaid("\n");
    std::cout << std::endl << "e-NFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    std::string expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- ε --> 1;
    0(0-S)  -- c --> 4;
    0(0-S)  -- a --> 2;
    2  -- b --> 3;
    4  -- ε --> 1;
    3  -- ε --> 1;
    1  -- [a-c] --> 5;
    5  -- [εa-c] --> 6(6-E);
    6(6-E)  -- ε --> 5;
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    std::string err;
    DFA dfa;
    if (!eNFA.to_dfa(dfa, err))
    {
        std::cerr << err << std::endl;
        return -1;
    }

    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- a --> 7(7-E);
    0(0-S)  -- b --> 8(8-E);
    0(0-S)  -- c --> 10(10-E);
    10(10-E)  -- [a-c] --> 8(8-E);
    9(9-E)  -- [a-c] --> 8(8-E);
    7(7-E)  -- b --> 9(9-E);
    8(8-E)  -- [a-c] --> 8(8-E);
    7(7-E)  -- [ac] --> 8(8-E);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));


    dfa.try_minimize();
    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "minimize DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S)  -- [a-c] --> 7(7-E);
    7(7-E)  -- [a-c] --> 7(7-E);
```)==";
    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));
    return 0;
}

int run_all_tests() {
    // https://www.mermaidflow.app/editor#/
    // https://mermaid.live/edit
    int ret = 0;

    ret = test_case1(); TEST_ASSERT(0 == ret);
    ret = test_case2(); TEST_ASSERT(0 == ret);
    ret = test_case3(); TEST_ASSERT(0 == ret);
    ret = test_case4(); TEST_ASSERT(0 == ret);
    ret = test_case5(); TEST_ASSERT(0 == ret);
    ret = test_case6(); TEST_ASSERT(0 == ret);
    ret = test_case7(); TEST_EXPECT(0 == ret); /// <<-----
    ret = test_case8(); TEST_ASSERT(0 == ret);
    ret = test_case9(); TEST_ASSERT(0 == ret);
    return 0;
}

} // namespace fa
} // namespace tests




#ifdef __EMSCRIPTEN__
#include <emscripten/bind.h>
namespace to_wasm {
   static inline emscripten::val NFAToDFA(const finite_automata::EpsilonNFA& nfa, finite_automata::DFA& dfa) {
        std::string err;
        bool ret = nfa.to_dfa(dfa, err);
        emscripten::val resultObj = emscripten::val::object();
        resultObj.set("result", ret); 
        resultObj.set("error", err); 
        return resultObj; // Return the object
   }

   static inline bool TryMinimizeDFA(finite_automata::DFA& dfa) {
       return dfa.try_minimize(); // 移除掉默认参数的影响
   }


   template <class FA>
   static inline std::string ToMermaidImpl(const FA& fa) {
      constexpr const char* endline = "\r\n";
      std::string ret = fa.to_mermaid(endline);
      auto f1 = ret.find_first_of('\n'); 
      auto f2 = ret.find_last_of('\n'); 
      if (std::string::npos != f1 && std::string::npos != f2 && f1 <= f2) {
         ret = ret.substr(f1, f2 - f1);
         ret += endline;
         const std::string final_stroke = "stroke:#333,stroke-width:4px";
         const std::string both_start_final_stroke = "stroke:#fff,stroke-width:0px";
         for (finite_automata::State s : fa.get_final_states()) {
            // style 3 fill:#f9f,stroke:#333,stroke-width:4px;
            const std::string& stroke = fa.is_start_state(s) ? both_start_final_stroke : final_stroke;
            ret += "    style " + std::to_string(s) + " fill:#f9f," + stroke + ";" + endline;
         }
      }
      return ret;
   }

   static inline std::string NFAToMermaid(const finite_automata::EpsilonNFA& nfa) {
       return ToMermaidImpl(nfa);
   }

   static inline std::string DFAToMermaid(const finite_automata::DFA& dfa) {
       return ToMermaidImpl(dfa);
   }

   static inline emscripten::val RegexBuilderToAST(const RegexBuilder& builder, const std::string& regex, uint32_t modifiers, regex::RegexAST& ast) {
        std::string err;
        bool ret = builder.to_ast(regex, modifiers, ast, err);
        emscripten::val resultObj = emscripten::val::object();
        resultObj.set("result", ret);
        resultObj.set("error", err);
        return resultObj; // Return the object
   }

    // regex::ToEpsilonNFA(ast, nfa, err)
    static inline emscripten::val ASTToNFA(const regex::RegexAST& ast, finite_automata::EpsilonNFA& nfa) {
        std::string err;
        bool ret = regex::ToEpsilonNFA(ast, nfa, err);
        emscripten::val resultObj = emscripten::val::object();
        resultObj.set("result", ret);
        resultObj.set("error", err);
        return resultObj; // Return the object
    }

    static inline uint32_t CalcModifiers(bool caseless, bool non_greedy, bool dot_not_all) {
        uint32_t ret = regex::Flag::DEFAULT;
        if (caseless)    { ret |= regex::Flag::CASELESS; }
        if (non_greedy)  { ret |= regex::Flag::NON_GREEDY; }
        if (dot_not_all) { ret |= regex::Flag::DOT_NOT_ALL; }
        return ret;
	  }

} // to_wasm
EMSCRIPTEN_BINDINGS(my_module) {

    emscripten::class_<regex::RegexAST>("RegexAST")
        .constructor<>();

    emscripten::class_<finite_automata::EpsilonNFA>("EpsilonNFA")
        .constructor<>();
        //.function("to_dfa", &finite_automata::EpsilonNFA::to_dfa, emscripten::allow_raw_pointers());
        //.function("to_dfa", &::finite_automata::EpsilonNFA::to_dfa, emscripten::allow_raw_pointer<emscripten::arg<1>>());

    emscripten::class_<finite_automata::DFA>("DFA")
        .constructor<>();
        //.function("to_mermaid", &finite_automata::DFA::to_mermaid);

    emscripten::class_<RegexBuilder>("RegexBuilder")
        .constructor<>();
        //.function("to_ast", &RegexBuilder::to_ast)
        //.property("x", &MyClass::getX, &MyClass::setX)
        //.class_function("ast_to_nfa", &RegexBuilder::ast_to_nfa);

    emscripten::function("CalcModifiers", &to_wasm::CalcModifiers);
    emscripten::function("RegexBuilderToAST", &to_wasm::RegexBuilderToAST);
    emscripten::function("DumpAST", &regex::Dump);
    emscripten::function("ASTToNFA", &to_wasm::ASTToNFA);
    emscripten::function("NFAToMermaid", &to_wasm::NFAToMermaid);
    emscripten::function("NFAToDFA", &to_wasm::NFAToDFA);
    emscripten::function("DFAToMermaid", &to_wasm::DFAToMermaid);
    emscripten::function("TryMinimizeDFA", &to_wasm::TryMinimizeDFA);
}
#else
int main()
{
    tests::lexpsr::test_regex2();
    tests::lexpsr::test_regex();
    tests::fa::run_all_tests();
    return 0;
}
#endif // __EMSCRIPTEN__
