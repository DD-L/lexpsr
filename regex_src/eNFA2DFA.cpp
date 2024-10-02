﻿// eNFA2DFA.cpp : 此文件包含 "main" 函数。程序执行将在此处开始并结束。
//

#include <iostream> // for debug print
#include <cstdint>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <assert.h>
#include <algorithm>
#include <string>
#include <cctype>

#undef _DEPRECATED_ALGO
#define _DEPRECATED_ALGO

typedef uint64_t State;
typedef int32_t  Edge;

template <class T>
class ordered_unique_vec;

template <class T>
static inline ordered_unique_vec<T>& make_ordered_unique_mut_vec(std::vector<T>& vec);

template <class T>
class ordered_unique_vec : public std::vector<T> {
    typedef std::vector<T> base_type;
public:
    using base_type::vector;

    template <class V>
    bool binary_search(V&& value) const {
        return std::binary_search(base_type::begin(), base_type::end(), std::forward<V>(value));
    }

    template <class V>
    void ordered_push(V&& value) {
        base_type::push_back(std::forward<V>(value));
        make_ordered_unique_mut_vec(*this);
    }

    template <class... Args>
    void ordered_insert(Args&&... args) {
        base_type::insert(std::forward<Args>(args)...);
        make_ordered_unique_mut_vec(*this);
    }

    void ordered_insert(const base_type& vec) {
        ordered_insert(base_type::end(), vec.begin(), vec.end());
    }

    void ordered_insert(const ordered_unique_vec<T>& vec) {
        ordered_insert(static_cast<const base_type&>(vec));
    }

    template <class Vec>
    void ordered_assgin_vec(Vec&& vec) {
        static_cast<base_type&>(*this) = std::forward<Vec>(vec);
        make_ordered_unique_mut_vec(*this);
    }
}; // class ordered_unique_vec<T>

constexpr Edge  epsilon = -1;
constexpr State invalid_state = ~static_cast<State>(0);

static constexpr inline bool valid_edge(Edge e) {
    return e >= epsilon;
}

template <class T>
static inline ordered_unique_vec<T>& make_ordered_unique_mut_vec(std::vector<T>& vec) {
    if (vec.size() > 1u) {
        std::sort(vec.begin(), vec.end());
        auto last = std::unique(vec.begin(), vec.end());
        assert(vec.begin() <= last);
        const std::size_t size = std::distance(vec.begin(), last);
        vec.resize(size);
    }
    return static_cast<ordered_unique_vec<T>&>(vec);
}

namespace details {
    static constexpr inline State successor(State s) {
        return s + 1u;
    }

    struct state_hash {
        std::size_t operator()(State s) const {
            //constexpr size_t _FNV_offset_basis = (sizeof(void*) == 8) ? static_cast<size_t>(14695981039346656037ULL) : 2166136261U;
            //constexpr size_t _FNV_prime = (sizeof(void*) == 8) ? static_cast<size_t>(1099511628211ULL) : 16777619U;
            //// accumulate range [_First, _First + _Count) into partial FNV-1a hash _Val
            //std::size_t _Val = _FNV_offset_basis;
            //for (size_t _Idx = 0; _Idx < sizeof(State); ++_Idx) {
            //    _Val ^= static_cast<size_t>(reinterpret_cast<const unsigned char* const>(&s)[_Idx]);
            //    _Val *= _FNV_prime;
            //}
            //
            //return _Val;
            return std::hash<State>()(s);
        }
    };

    struct state_pair_t {
        State s1;
        State s2;

        struct hash {
            std::size_t operator()(const state_pair_t& sft) const {
                return state_hash()(sft.s1) + state_hash()(sft.s2);
            }
        };

        bool operator==(const state_pair_t& that) const { // fix g++ (GCC) 11.3.0 : map1 == map2
            return s1 == that.s1 && s2 == that.s2;
        }
    };

    typedef state_pair_t state_from_to_t;
    typedef std::unordered_map<state_from_to_t, ordered_unique_vec<Edge>, state_from_to_t::hash> state_pair_map_t;
    typedef std::unordered_map<Edge, ordered_unique_vec<State>> edge_to_states_t;
    typedef std::unordered_map<State, std::unordered_map<Edge, State>>  dfa_move_functions_t;

    class FABase {
    public:
        bool is_start_state(State state) const {
            return m_start_state == state;
        }

        bool is_final_state(State state) const {
            return m_final_states.binary_search(state);
        }

        std::string get_state_name(State s) const {
            std::string state_name(std::to_string(s));

            const bool start_state = is_start_state(s);
            const bool final_state = is_final_state(s);

            if (start_state || final_state) {
                state_name += ("(" + state_name);
                if (start_state) {
                    state_name += "-S";
                }
                if (final_state) {
                    state_name += "-E";
                }
                state_name += ")";
            }
            return state_name;
        }

        std::string get_edges_string(const ordered_unique_vec<Edge>& edges_set) const {
            if (edges_set.empty()) {
                return {};
            }

            std::string ret;
            auto iter = edges_set.begin();
            if (epsilon == edges_set.front()) {
                ret += "ε";
                ++iter;
            }

            constexpr Edge invalid_last_edge = epsilon - 1;
            Edge last_edge = invalid_last_edge;
            bool is_closed = false;
            for (; iter != edges_set.end(); ++iter) {
                Edge edge = *iter;
                assert(valid_edge(edge));
                is_closed = false;
                if (edge != last_edge + 1) {
                    if (invalid_last_edge != last_edge) {
                        ret += ("-" + get_edge_string(last_edge));
                    }
                    ret += get_edge_string(edge);
                    is_closed = true;
                }
                last_edge = edge;
            }

            if (!is_closed && valid_edge(last_edge)) {
                ret += ("-" + get_edge_string(last_edge));
            }

            return (edges_set.size() == 1u) ? ret : ("[" + ret + "]");
        }

        std::string get_edges_string(const std::unordered_set<Edge>& edges) const {
            ordered_unique_vec<Edge> edges_vec(edges.begin(), edges.end());
            return get_edges_string(edges_vec);
        }

        std::string get_edge_string(Edge edge) const {
            assert(valid_edge(edge));
            if (epsilon == edge) {
                return "ε";
            }

            if (edge <= 0xff) {
                uint8_t u8_val = uint8_t(edge & 0xff);
                if (std::isalnum(u8_val)) {
                    return std::string(1u, char(u8_val));
                }

                std::string ret = "\\x";
                ret.push_back("0123456789abcdef"[u8_val >> 4]);
                ret.push_back("0123456789abcdef"[u8_val & 0xf]);
                return ret;
            }

            std::string ret = "\\x";
            for (int i = sizeof(Edge) - 1; i >= 0; --i) {
                uint8_t u8_val = reinterpret_cast<uint8_t*>(&edge)[i];
                ret.push_back("0123456789abcdef"[u8_val >> 4]);
                ret.push_back("0123456789abcdef"[u8_val & 0xf]);
            }
            return ret;
        }

    protected:
        template <class StatePairMap, class EdgesStrHandler>
        std::string to_mermaid_impl(StatePairMap&& state_pair_map, const char* endline, EdgesStrHandler&& edges_string_handler) const {
            std::string ret;

            for (auto&& pair : state_pair_map) {
                const state_from_to_t& state_from_to = pair.first;
                auto& edges = pair.second;

                State s1 = state_from_to.s1;
                State s2 = state_from_to.s2;

                const std::string edges_str = edges_string_handler(edges);

                const std::string s1_name = get_state_name(s1);
                const std::string s2_name = (s1 == s2) ? s1_name : get_state_name(s2);


                const std::string line = "    " + s1_name + "  --" + edges_str + "--> " + s2_name + ";" + endline;

                if (is_start_state(s1)) {
                    ret = line + ret;
                }
                else {
                    ret += line;
                }
            }

            return "```mermaid" + std::string(endline) + "graph TD;" + endline + ret + "```";
        }

        void reset(State start_state, ordered_unique_vec<State>&& final_states) {
            m_start_state = start_state;
            if (&m_final_states != &final_states) { // fix g++ (GCC) https://godbolt.org/z/b4x9PhWcd
                m_final_states = std::move(final_states);
            }
        }

        void reset() {
            m_start_state = invalid_state;
            m_final_states.clear();
        }

    protected:
        State                       m_start_state = invalid_state;
        ordered_unique_vec<State>    m_final_states;
    }; // class FABase
} // namespace details

class DFA : public details::FABase
{
    typedef details::state_from_to_t       state_from_to_t;
    typedef details::state_pair_t          state_pair_t;
    typedef details::dfa_move_functions_t  move_functions_t;

    typedef details::state_pair_map_t state_pair_map_t;

    class merged_states_cache_guard_t {
    public:
        enum class cached_value {
            initial_value    = -2,
            still_on_stack   = -1,
            can_be_merged    = 0,
            cannot_be_merged = 1,
        };

        bool transfer_init_state() { // guard 的初始状态转移
            // 返回当前状态对是否是因为还在之前的函数栈上（未被求值），如果仅仅在当前的函数中，则返回 false
            if (cached_value::initial_value == m_cache_value) {
                m_cache_value = cached_value::still_on_stack;
                return true; // 刚刚成为 still_on_stack 还没开始计算，仅在当前的函数中
            }

            return !still_on_outer_layer_stack();
        }

        bool has_been_calculated() const {
            return cached_value::can_be_merged == m_cache_value || cached_value::cannot_be_merged == m_cache_value;
        }

        bool can_be_merged() const {
            assert(has_been_calculated());
            return cached_value::can_be_merged == m_cache_value;
        }

        bool return_value(bool _can_be_merged) {
            assert(still_on_outer_layer_stack());
            m_cache_value = _can_be_merged ? cached_value::can_be_merged : cached_value::cannot_be_merged;
            return _can_be_merged;
        }

    public:
        cached_value& m_cache_value;

    private:
        bool still_on_outer_layer_stack() const {
            return cached_value::still_on_stack == m_cache_value;
        }
    }; // merged_states_cache_guard_t

    struct merged_states_cache_t {
        typedef merged_states_cache_guard_t::cached_value cached_value;

        merged_states_cache_guard_t generate_guard(const state_pair_t& state_pair) {
            auto found = m_cache.find(state_pair);
            if (m_cache.end() != found) {
                return { found->second };
            }
            auto res = m_cache.insert({ state_pair, cached_value::initial_value });
            assert(res.second);
            if (res.second) {
                return { res.first->second };
                // 1. https://cplusplus.com/reference/unordered_map/unordered_map/rehash/#/
                // # Iterator validity:
                // > If a rehash happens, all iterators are invalidated, but references and pointers to individual elements remain valid.
                // 2. https://en.cppreference.com/w/cpp/container/unordered_map#/
                // # Iterator invalidation:
                // > References and pointers to either key or data stored in the container are only invalidated by erasing that element,
                // > even when the corresponding iterator is invalidated.
                // case: https://godbolt.org/z/4M159Ta46 迭代器和元素指针都暂时没有观察到失效的情况 (文档上说 rehash 后迭代器 `Always` 失效)
            }

            static cached_value _v { cached_value::initial_value };
            return  { _v }; // BUG
        }

        std::unordered_map<state_pair_t, cached_value, state_pair_t::hash> m_cache;
    }; // merged_states_cache_t

public:
    bool is_start_state(State state) const {
        return m_start_state == state;
    }

    bool is_final_state(State state) const {
        return m_final_states.binary_search(state);
    }

    std::string to_mermaid(const char* endline = "\n") const {
        return to_mermaid_impl(m_state_pair_map, endline, [this](const ordered_unique_vec<Edge>& edges) {
            return get_edges_string(edges);
        });
    }

public:
    class minimize_policy {
    public:
        enum class target_structure : char {
            to_state_pair_map = 1 << 0,
            to_move_functions = 1 << 1,
            to_both           = to_state_pair_map | to_move_functions,
        };

        enum {
            default_abandonment_threshold = 0xffffUL
        };

    public:
        explicit minimize_policy(std::size_t threshold = default_abandonment_threshold,
            target_structure ts = target_structure::to_state_pair_map)
            : m_abandonment_threshold(threshold), m_target_structure(ts)
        {}

    public:
        minimize_policy& reset_abandonment_threshold(std::size_t threshold) {
            m_abandonment_threshold = threshold;
            return *this;
        }

        minimize_policy& reset_target_structure(target_structure ts) {
            m_target_structure = ts;
            return *this;
        }

    public:
        std::size_t abandonment_threshold() const {
            return m_abandonment_threshold;
        }

        bool target_structure_is_state_pair_map() const {
            return static_cast<bool>(static_cast<char>(m_target_structure) & static_cast<char>(target_structure::to_state_pair_map));
        }

        bool target_structure_is_move_functions() const {
            return static_cast<bool>(static_cast<char>(m_target_structure) & static_cast<char>(target_structure::to_move_functions));
        }

    private:
        std::size_t         m_abandonment_threshold = default_abandonment_threshold;
        target_structure    m_target_structure      = target_structure::to_state_pair_map;
    }; // class minimize_policy

    bool try_minimize(minimize_policy policy = minimize_policy{}) {
        if (m_minimized) { return true; } // 因为极小化比较耗时，所以目前只能尝试极小化最多一次 !!!!
        const std::size_t origin_state_cnt = m_state_set.size();
        const std::size_t loop_cnt_in_algo = combination_n_choose_2(origin_state_cnt); // 此算法需要遍历的次数
        if (loop_cnt_in_algo >= policy.abandonment_threshold()) {
            return false; // too many states, give up
        }

        State new_start_state = m_start_state;
        std::unordered_map<State, State> state_replacement_table = generate_state_replacement_table(new_start_state);
        update_with_state_replacement_table(state_replacement_table, new_start_state, policy);
        m_minimized = true;
        return true;
    }

public:
    void reset(State start_state, state_pair_map_t&& state_pair_map, ordered_unique_vec<State>&& final_states,
         std::unordered_set<State>&& state_set, move_functions_t&& move_functions) {
        details::FABase::reset(start_state, std::move(final_states));
        if (&m_state_pair_map != &state_pair_map) { // fix g++ (GCC) < 11.1
            m_state_pair_map = std::move(state_pair_map);
        }
        if (&m_move_functions != &move_functions) { // fix g++ (GCC) < 11.1
            m_move_functions = std::move(move_functions);
        }
        if (&m_state_set != &state_set) { // fix g++ (GCC) < 11.1
            m_state_set = std::move(state_set);
        }
    }

    void reset() {
        details::FABase::reset();
        m_state_pair_map.clear();
    }

    bool is_same_shape_mut(DFA& that) { // 判断是否形状相同，但对状态数值没有要求 （此方法可能有副作用）
        if (m_state_set.empty() && that.m_state_set.empty()) { return true; }
        if (m_state_set.size() != that.m_state_set.size() || m_final_states.size() != that.m_final_states.size()) {
            return false;
        }

        if (!m_move_functions.empty() && !that.m_move_functions.empty()) {
            if (m_move_functions.size() != that.m_move_functions.size()) { return false; }
            if (m_move_functions == that.m_move_functions && m_final_states == that.m_final_states) { return true; }

            std::unordered_map<State, State> state_map;
            if (compare_move_functions(m_start_state, m_move_functions, that.m_start_state, that.m_move_functions, state_map)) {
                return compare_final_states(m_final_states, that.m_final_states, state_map);
            }
            return false;
        }

        if (!m_state_pair_map.empty() && !that.m_state_pair_map.empty()) {
            if (m_state_pair_map.size() != that.m_state_pair_map.size()) { return false; }
            if (m_state_pair_map == that.m_state_pair_map && m_final_states == that.m_final_states) { return true; }
        }

        gen_move_functions_from_state_pair_map(); // 有副作用
        that.gen_move_functions_from_state_pair_map(); // 有副作用

        return is_same_shape_mut(that);
    }

private:
    void update_with_state_replacement_table(const std::unordered_map<State, State>& state_replacement_table, 
        State new_start_state, const minimize_policy& policy) {

        if (state_replacement_table.empty()) { return; }

        state_pair_map_t          new_state_pair_map;
        move_functions_t          new_move_functions;

        const bool target_structure_is_state_pair_map = policy.target_structure_is_state_pair_map();
        const bool target_structure_is_move_functions = policy.target_structure_is_move_functions();

        for (auto&& pair : m_state_pair_map) {
            const state_from_to_t& from_to_state = pair.first;
            const ordered_unique_vec<Edge>& edges = pair.second;

            State new_state_1 = from_to_state.s1;
            State new_state_2 = from_to_state.s2;

            auto found_source_iter = state_replacement_table.find(from_to_state.s1);
            if (state_replacement_table.end() != found_source_iter) {
                new_state_1 = found_source_iter->second;
            }

            if (from_to_state.s1 != from_to_state.s2) {
                found_source_iter = state_replacement_table.find(from_to_state.s2);
                if (state_replacement_table.end() != found_source_iter) {
                    new_state_2 = found_source_iter->second;
                }
            }
            else {
                new_state_2 = new_state_1;
            }

            if (target_structure_is_state_pair_map) {
                new_state_pair_map.insert({ state_from_to_t{new_state_1, new_state_2}, edges });
            }

            if (target_structure_is_move_functions) {
                std::unordered_map<Edge, State>& edge_to_state = new_move_functions[new_state_1];
                for (Edge edge : edges) {
                    edge_to_state[edge] = new_state_2;
                }
            }
        }

        for (auto&& pair : state_replacement_table) {
            assert(pair.first > pair.second);
            const State source_state = pair.first; // state_replacement_table 中所有的 key 都是要被换掉的 state

            m_state_set.erase(source_state);
            auto iter = std::lower_bound(m_final_states.begin(), m_final_states.end(), source_state);
            if ((iter != m_final_states.end()) && (*iter == source_state)) {
                m_final_states.erase(iter);
            }
        }

        assert(!m_final_states.empty());
        assert(invalid_state != new_start_state);
        reset(new_start_state, std::move(new_state_pair_map), std::move(m_final_states),
            std::move(m_state_set), std::move(new_move_functions));
    }

    std::unordered_map<State, State> generate_state_replacement_table(State& new_start_state) const {
        std::unordered_map<State, State> state_replacement_table;
        merged_states_cache_t merged_states_cache;
        gen_combination_states_choose_2([&](State s1, State s2) {
            if (can_be_merged(s1, s2, merged_states_cache)) {
                if (s1 > s2) { std::swap(s1, s2); }
                assert(s1 < s2);
                auto found = state_replacement_table.find(s2);
                if (state_replacement_table.end() == found) {
                    if (is_start_state(s2)) { // s2 要被 s1 换掉，保留 s2 的起始标记
                        new_start_state = s1; // 对小值 s1 染色
                    }
                    state_replacement_table.insert({ s2, s1 }); // 大 -> 小
                }
                else {
                    State& s_target = found->second;
                    if (s1 < s_target) {
                        if (is_start_state(s_target)) { // s_target 要被 s1 换掉，保留 s_target 的起始标记
                            new_start_state = s1; // 对小值 s1 染色
                        }
                        s_target = s1;
                    }
                    // 不需要收集所有可以合并的状态对。比如：1,2,3 状态可以合并（2->1， 3->1， 3->2）
                    // 则只需保存 2->1, 3->1 即可。将 2,3 状态都转换成 1
                }
            }
        });
        return state_replacement_table;
    }

    bool can_be_merged(State s1, State s2, merged_states_cache_t& cache_result) const {
        // 找出所有不能合并的状态对（剩下的都是可以合并的）
        if (s1 == s2) { return true; } // 不必缓存相同的 state
        if (s1 > s2) { std::swap(s1, s2); }
        assert(s1 < s2);
        const state_pair_t state_pair{ s1, s2 };
        auto guard = cache_result.generate_guard(state_pair);
        if (!guard.transfer_init_state()) {
            return true; // 此状态对仍在此外层递归函数栈上，未被计算出结果，还不能决定是不能合并的状态对
            // 破环，此状态还被确定是否能合并，因为它还在 can_be_merged(...) 的外层递归函数栈上
        }

        if (guard.has_been_calculated()) {
            return guard.can_be_merged();
        }

        ///////////////!!! 从这一行开始，下面每一个 return 出口都要严格调用 return guard.return_value(...); !!! //////////////////
        const bool s1_is_final = is_final_state(s1);
        const bool s2_is_final = is_final_state(s2);
        if (s1_is_final != s2_is_final) { // 一个是终点另一个不是终点，一定不能合并
            return guard.return_value(false);
        }

        auto found_s1_iter = m_move_functions.find(s1);
        auto found_s2_iter = m_move_functions.find(s2);
        const bool found_s1 = found_s1_iter != m_move_functions.end();
        const bool found_s2 = found_s2_iter != m_move_functions.end();

        if (found_s1 != found_s2) { // 如果一个有出边，另一个没有出边，那么这两个状态一定不能合并（出边集不同）
            return guard.return_value(false);
        }

        if (found_s1 && found_s2) {
            const std::unordered_map<Edge, State>& s1_map = found_s1_iter->second;
            const std::unordered_map<Edge, State>& s2_map = found_s2_iter->second;
            if (s1_map.size() != s2_map.size()) { // 出边集个数不同，一定不能合并
                return guard.return_value(false);
            }

            for (auto&& pair1 : s1_map) { // 不能采用两个 map iter 同步迭代的方案，因为 unordered_map 的 key 集即使相同，但排列的顺序可能不同！！！
                const Edge e = pair1.first;
                auto found = s2_map.find(e);
                if (s2_map.end() == found) {
                    return guard.return_value(false); // 出边集不同，一定不能合并
                }
                State next_s1 = pair1.second;
                State next_s2 = found->second;
                if (!can_be_merged(next_s1, next_s2, cache_result)) {
                    return guard.return_value(false);
                }
            }
        }
        else {
            (void)cache_result;
            assert(!found_s1 && !found_s2); // 两个状态都没有出边, 可以合并
            assert(s1_is_final && s2_is_final); // 因为已经消除了死状态，所以没有出边一定是终止状态
            assert(!is_start_state(s1) && !is_start_state(s2)); // 起点一定有出边
        }

        return guard.return_value(true); // 可以合并
    } // function can_be_merged()

    static constexpr inline unsigned long long combination_n_choose_2(std::size_t N) {
        return (N < 2u) ? 0u // C(N, 2) is 0 for N < 2
            : (static_cast<unsigned long long>(N) * (N - 1u) / 2u);
    }

    template <class H>
    void gen_combination_states_choose_2(H&& h) const { // 循环次数可由 combination_n_choose_2() 计算得出
        for (auto s1_iter = m_state_set.begin(); s1_iter != m_state_set.end(); ++s1_iter) {
            for (auto s2_iter = std::next(s1_iter); s2_iter != m_state_set.end(); ++s2_iter) {
                State s1 = *s1_iter;
                State s2 = *s2_iter;
                assert(s1 != s2);
                h(s1, s2);
            }
        }
    }

    static bool compare_move_functions(State s1, const move_functions_t& move_funcs1,
        State s2, const move_functions_t& move_funcs2, std::unordered_map<State, State>& state_map) {

        if (!state_map.insert({ s1, s2 }).second) { return true; } // 破环

        auto&& found1 = move_funcs1.find(s1);
        auto&& found2 = move_funcs2.find(s2);

        const bool not_found1 = move_funcs1.end() == found1;
        const bool not_found2 = move_funcs2.end() == found2;
        if (not_found1 && not_found2) {
            return true;
        }
        if (not_found1 != not_found2) {
            return false;
        }

        auto&& mapped1 = found1->second;
        auto&& mapped2 = found2->second;
        if (mapped1.size() != mapped2.size()) {
            return false;
        }

        for (auto&& pair : mapped1) { // 不能采用两个 map iter 同步迭代的方案，因为 unordered_map 的 key 集即使相同，但排列的顺序可能不同！！！
            const Edge e = pair.first;
            auto found = mapped2.find(e);
            if (mapped2.end() == found) {
                return false;
            }
            State next1 = pair.second;
            State next2 = found->second;
            if (!compare_move_functions(next1, move_funcs1, next2, move_funcs2, state_map)) {
                return false;
            }
        }

        return true;
    }

    static bool compare_final_states(const ordered_unique_vec<State>& final_state1,
        const ordered_unique_vec<State>& final_state2, std::unordered_map<State, State>& state_map) noexcept {

        if (final_state1.size() != final_state2.size()) {
            return false;
        }

        for (auto iter1 = final_state1.begin(), iter2 = final_state2.begin();
            iter1 != final_state1.end(); ++iter1, ++iter2) {
            if (iter2 == final_state2.end()) {
                return false;
            }

            const State s1 = *iter1;
            const State s2 = *iter2;

            auto found = state_map.find(s1);
            if (state_map.end() == found) {
                assert(false && "BUG in DFA::compare_final_states()");
                return false;
            }

            if (found->second != s2) { return false; }
        }

        return true;
    }

    void gen_move_functions_from_state_pair_map() {
        if (!m_move_functions.empty()) { return; }
        for (auto&& p : m_state_pair_map) {
            const state_from_to_t& ft = p.first;
            const ordered_unique_vec<Edge>& edges = p.second;
            auto&& mapped = m_move_functions[ft.s1];
            for (Edge e : edges) {
                mapped[e] = ft.s2;
            }
        }
    }

private:
    state_pair_map_t          m_state_pair_map;  // for to_mermaid()
    move_functions_t          m_move_functions;  // for try_minimize()
    std::unordered_set<State> m_state_set;
    bool                      m_minimized = false;
}; // class DFA

namespace details {

    struct merged_states_hash {
        std::size_t operator()(const ordered_unique_vec<State>& state_vec) const {
            std::size_t hash_code = 0;
            for (State s : state_vec) {
                hash_code += state_hash()(s);
            }
            return hash_code;
        }
    };

    // 缓存
    typedef std::unordered_map<
        ordered_unique_vec<State>, ordered_unique_vec<State>, merged_states_hash
    > cache_eat_epsilon_star_t;

    struct cache_batch_eat_edge_t {
        struct key_t {
            ordered_unique_vec<State> from;
            Edge                     edge;
        };

        struct hash {
            std::size_t operator()(const key_t& key) const {
                return merged_states_hash()(key.from) + std::hash<Edge>()(key.edge);
            }
        };

        struct equal {
            bool operator()(const key_t& key1, const key_t& key2) const {
                return key1.from == key2.from && key1.edge == key2.edge;
            }
        };

        typedef std::unordered_map<key_t, ordered_unique_vec<State>, hash, equal> cache_t;

        template <class K>
        cache_t::const_iterator find(K&& k) const {
            return m_cache.find(std::forward<K>(k));
        }

        template <class... Args>
        void insert(Args&&... args) {
            m_cache.insert(std::forward<Args>(args)...);
        }

        cache_t::const_iterator end() const {
            return m_cache.end();
        }

        cache_t  m_cache;
    }; // struct cache_batch_eat_edge_t;

    _DEPRECATED_ALGO class DFAAux {
    public:
        void set_start_state(State state) {
            m_start_state = state;
        }

        bool move_func(State state, Edge edge, const ordered_unique_vec<State>& next_state_vec) {
            return move_func(ordered_unique_vec<State>{ state }, edge, next_state_vec);
        }

        bool move_func(const ordered_unique_vec<State>& state, Edge edge, const ordered_unique_vec<State>& next_state_vec) {
            if (state.empty() || next_state_vec.empty()) {
                return false;
            }
            m_move_functions[state][edge].ordered_insert(next_state_vec);
            return true;
        }

        bool move_func(const ordered_unique_vec<State>& state, edge_to_states_t&& merged_edge_map) {
            if (state.empty() || merged_edge_map.empty()) {
                return false;
            }

            edge_to_states_t& edge_map = m_move_functions[state];
            if (edge_map.empty()) {
                assert(&edge_map != &merged_edge_map);
                edge_map = std::move(merged_edge_map);
                return true;
            }

            for (const auto& pair : merged_edge_map) {
                edge_map[pair.first].ordered_insert(pair.second);
            }
            return true;
        }

        bool find_from_state(const ordered_unique_vec<State>& state) const {
            return m_move_functions.end() != m_move_functions.find(state);
        }

        bool is_final_state(State state) const {
            return m_final_states.binary_search(state);
        }

        bool to_dfa(DFA& dfa, std::string& err) {
            if (invalid_state == m_start_state || m_final_states.empty() || m_state_pair_map.empty()
                || m_dfa_state_set.empty() || m_dfa_move_functions.empty()) {
                err = "illegal dfa_aux";
                return false;
            }

            m_dfa_state_set.insert(m_final_states.begin(), m_final_states.end()); // 补缺，因为之前算法中只收集了 from 状态集

            dfa.reset(m_start_state, std::move(m_state_pair_map), std::move(m_final_states),
                std::move(m_dfa_state_set), std::move(m_dfa_move_functions));
            return true;
        }

    public:
        typedef std::unordered_map<ordered_unique_vec<State>, edge_to_states_t, merged_states_hash> move_functions_t;
        typedef std::unordered_set<ordered_unique_vec<State>, merged_states_hash> merged_states_temp_t;

    public:
        State                     m_start_state = invalid_state;
        move_functions_t          m_move_functions;
        state_pair_map_t          m_state_pair_map;
        ordered_unique_vec<State> m_final_states;
        std::unordered_set<State> m_dfa_state_set;
        dfa_move_functions_t      m_dfa_move_functions;
        merged_states_temp_t      m_merged_states_temp;
        // caches
        cache_eat_epsilon_star_t m_cache_eat_epsilon_star;
        cache_batch_eat_edge_t   m_cache_batch_eat_edge;
    }; // class DFAAux


    template <class dfa_aux_t>
    class dfa_aux_cache_guard_t {
    public:
        enum class cache_state {
            initial_value = -1,
            still_on_stack = 0,
            calculated_value = 1
        };

        typedef std::pair<const ordered_unique_vec<State>*, Edge> key_t;

    public:
        bool transfer_init_state() {
            if (cache_state::initial_value == m_cache_state) {
                m_cache_state = cache_state::still_on_stack;
                return true; // 刚刚成为 still_on_stack 还没开始计算，仅在当前的函数中
            }

            return !still_on_outer_layer_stack();
        }

        bool has_been_calculated() const {
            return cache_state::calculated_value == m_cache_state;
        }

        const ordered_unique_vec<State>& calculated_value() const {
            has_been_calculated();
            return m_calculated_value;
        }

        ordered_unique_vec<State>& return_value(ordered_unique_vec<State>& ret) {
            assert(still_on_outer_layer_stack());
            m_cache_state = cache_state::calculated_value;
            assert(m_calculated_value.empty());
            m_calculated_value = ret;

            generate_dfa_move_function(); // 外层确保了 return_value() 只会在可达状态下被调用
            return ret;
        }

        dfa_aux_cache_guard_t(const key_t& cached_key,
            ordered_unique_vec<State>& calculated_value,
            dfa_aux_t& dfa_aux, cache_state cstate)
            : m_cached_key(cached_key)
            , m_calculated_value(calculated_value)
            , m_dfa_aux(dfa_aux)
            , m_cache_state(cstate)
        {}

    private:
        void generate_dfa_move_function() {
            assert(has_been_calculated());
            assert(nullptr != m_cached_key.first);
            const ordered_unique_vec<State>& from = *(m_cached_key.first);
            const ordered_unique_vec<State>& to   = m_calculated_value;
            Edge edge = m_cached_key.second;
            m_dfa_aux.generate_dfa_move_function(from, edge, to);
        }

        bool still_on_outer_layer_stack() const {
            return cache_state::still_on_stack == m_cache_state;
        }

    private:
        const key_t               m_cached_key;
        ordered_unique_vec<State>& m_calculated_value;
        dfa_aux_t&                m_dfa_aux;
        cache_state               m_cache_state = cache_state::initial_value;
    }; // class dfa_aux_cache_guard_t

    struct remove_epsilon_cache_t {
        struct key_t {
            State state; // nfa state
            Edge  edge;
        };

        struct hash {
            std::size_t operator()(const key_t& key) const {
                return std::hash<State>()(key.state) * 131 + std::hash<Edge>()(key.edge);
            }
        };

        struct equal {
            bool operator()(const key_t& k1, const key_t& k2) const {
                return k1.state == k2.state && k1.edge == k2.edge;
            }
        };

        const ordered_unique_vec<State>* find(State nfa_state, Edge edge) const {
            auto found = m_cache.find(key_t{ nfa_state, edge });
            if (m_cache.end() != found) {
                return &(found->second);
            }
            return nullptr;
        }

        ordered_unique_vec<State>& update(State nfa_state, Edge edge, ordered_unique_vec<State>& cached_value) {
            assert(epsilon != edge);
            assert(nullptr == find(nfa_state, edge));
            m_cache[key_t{ nfa_state, edge }] = cached_value; // insert
            return cached_value;
        }

        // { key : e-nfa state + edge;  mapped_value: 吃掉 epsilon* edge opsilon* 之后的状态 }
        std::unordered_map<key_t, ordered_unique_vec<State>, hash, equal> m_cache;
    }; // struct remove_epsilon_cache_t

    class DFAAux2 {
    public:
        typedef dfa_aux_cache_guard_t<DFAAux2> _dfa_aux_cache_guard_t;
        typedef std::unordered_map<ordered_unique_vec<State>, edge_to_states_t, merged_states_hash> cache_move_functions_t;

    public:
        void set_start_state(State state) {
            m_start_state = state;
        }

        void set_enfa_base(const FABase* enfa_base) {
            assert(enfa_base);
            m_enfa_base = enfa_base;
        }

        void set_enfa_max_state(State state) {
            m_current_max_state = state;
        }

        _dfa_aux_cache_guard_t generate_cache_guard(const ordered_unique_vec<State>& from_state, Edge edge) {
            assert(!from_state.empty());
            assert(epsilon != edge);

            _dfa_aux_cache_guard_t::cache_state cstate = _dfa_aux_cache_guard_t::cache_state::initial_value;
            _dfa_aux_cache_guard_t::key_t cached_key;
            ordered_unique_vec<State>* cached_value = nullptr;
            auto found = m_cache_move_functions.find(from_state);
            if (m_cache_move_functions.end() == found) { // 没有 from_state 也没有 edge
                auto res = m_cache_move_functions.insert({ from_state, edge_to_states_t{} });
                assert(res.second);
                cached_key = std::make_pair(&(res.first->first), edge);

                edge_to_states_t& edge_to_states = res.first->second;
                ordered_unique_vec<State>& result = edge_to_states[edge]; // insert
                cached_value = &result;
            }
            else {
                cached_key = std::make_pair(&(found->first), edge);

                edge_to_states_t& edge_to_states = found->second;
                auto found1 = edge_to_states.find(edge);
                if (found1 == edge_to_states.end()) { // 有 from_state 但没有 edge
                    cached_value = &(edge_to_states[edge]); // insert
                }
                else { // 有 from_state 也有 edge
                    cached_value = &(found1->second);
                    cstate = _dfa_aux_cache_guard_t::cache_state::calculated_value;
                }
            }
            assert(nullptr != cached_key.first && nullptr != cached_value);
            return _dfa_aux_cache_guard_t(cached_key, *cached_value, *this, cstate);
        }

        void generate_dfa_move_function(const ordered_unique_vec<State>& from, Edge edge, const ordered_unique_vec<State>& to) {
            if (from.empty() || to.empty()) {
                return;
            }
            assert(epsilon != edge);

            State dfa_from = calc_dfa_state(from);
            State dfa_to   = calc_dfa_state(to);

            m_dfa_move_functions[dfa_from][edge] = dfa_to; // for minimize dfa

            state_from_to_t from_to { dfa_from , dfa_to };
            ordered_unique_vec<Edge>& edges = m_state_pair_map[from_to];
            if (! edges.binary_search(edge)) {
                edges.ordered_push(edge);
            }
        }

        State calc_dfa_state(const ordered_unique_vec<State>& state) {
            assert(!state.empty());
            bool was_just_inserted = false;
            const State dfa_state = m_dfa_state_map.calc_dfa_state(state, m_current_max_state, was_just_inserted);
            if (!was_just_inserted) { // 不是刚刚被插入的 map 的 state （之前已经被计算过）
                assert(invalid_state != dfa_state);
                return dfa_state;
            }

            // try_stain_final  只需染色新刚刚被插入 m_dfa_state_map 的 state 即可
            assert(nullptr != m_enfa_base);
            for (State s : state) {
                if (m_enfa_base->is_final_state(s)) { // 在原始的 e-nfa 中如果是终点，那么 dfa_state 也应该是终点
                    m_dfa_final_states.insert(dfa_state); // 终点染色(起点的染色已经在前面做过了)
                    break;
                }
            }

            return dfa_state;
        }

        bool to_dfa(DFA& dfa, std::string& err) {
            std::unordered_set<State> dfa_state_set = m_dfa_state_map.get_all_dfa_states();
            if (invalid_state == m_start_state || m_dfa_final_states.empty() || m_state_pair_map.empty()
                || dfa_state_set.empty() || m_dfa_move_functions.empty()) {
                err = "illegal dfa_aux";
                return false;
            }

            ordered_unique_vec<State> final_states(m_dfa_final_states.begin(), m_dfa_final_states.end());
            dfa.reset(m_start_state, std::move(m_state_pair_map),
                std::move(make_ordered_unique_mut_vec(final_states)),
                std::move(dfa_state_set), std::move(m_dfa_move_functions));
            return true;
        }

    public:
        State                     m_start_state    = invalid_state;
        const FABase*             m_enfa_base      = nullptr;
        State                     m_current_max_state = 0; // 当前最大的状态数

        dfa_move_functions_t      m_dfa_move_functions;
        state_pair_map_t          m_state_pair_map;

        // typedef std::unordered_map<const ordered_unique_vec<State>, State, merged_states_hash> dfa_state_map_t;
        class dfa_state_map_t { // 一表两用：1. dfa_aux_move_each_edge() 递归破环；2. calc_dfa_state() 计算 dfa 状态值
        public:
            struct mapped_value_t {
                State dfa_state        = invalid_state; // for calc_dfa_state()
                bool  has_been_reached = false;         // for dfa_aux_move_each_edge() 递归破环

                mapped_value_t() : dfa_state(invalid_state), has_been_reached(false) {}
            };

            typedef std::unordered_map<const ordered_unique_vec<State>, mapped_value_t, merged_states_hash> map_t;

            bool break_loop(const ordered_unique_vec<State>& dfa_aux_from_state) {
                assert(!dfa_aux_from_state.empty());
                auto found = m_map.find(dfa_aux_from_state);
                if (m_map.end() != found) {
                    mapped_value_t& mapped_value = found->second;
                    if (mapped_value.has_been_reached) {
                        return true; // 已经来过
                    }
                    mapped_value.has_been_reached = true; // insert
                }
                else {
                    m_map[dfa_aux_from_state].has_been_reached = true; // insert
                }

                return false; // 第一次来
            }

            State calc_dfa_state(const ordered_unique_vec<State>& dfa_aux_state, State& current_max_state, bool& was_just_inserted) {
                assert(!dfa_aux_state.empty());
                auto found = m_map.find(dfa_aux_state);
                if (m_map.end() != found) {
                    mapped_value_t& mapped_value = found->second;
                    if (invalid_state != mapped_value.dfa_state) {
                        was_just_inserted = false;
                        return mapped_value.dfa_state;
                    }

                    const State dfa_state = gen_new_dfa_state(dfa_aux_state, current_max_state);
                    was_just_inserted = true;
                    assert(invalid_state == mapped_value.dfa_state);
                    mapped_value.dfa_state = dfa_state; // insert
                    return dfa_state;
                }

                const State dfa_state = gen_new_dfa_state(dfa_aux_state, current_max_state);
                m_map[dfa_aux_state].dfa_state = dfa_state; // insert
                was_just_inserted = true;
                return dfa_state;
            }

            std::unordered_set<State> get_all_dfa_states() const {
                std::unordered_set<State> ret;
                for (auto&& pair : m_map) {
                    const mapped_value_t& mapped_value = pair.second;
                    if (invalid_state != mapped_value.dfa_state) {
                        auto res = ret.insert(mapped_value.dfa_state);
                        assert(res.second); (void)res;
                    }
                }
                return ret;
            }

        private:
            State gen_new_dfa_state(const ordered_unique_vec<State>& dfa_aux_state, State& current_max_state) const {
                assert(!dfa_aux_state.empty());
                return (dfa_aux_state.size() == 1u)
                    ? dfa_aux_state.back() : (current_max_state = successor(current_max_state));
            }

        private:
            map_t  m_map;
        }; // dfa_state_map_t

        dfa_state_map_t           m_dfa_state_map; // 一表两用
        std::unordered_set<State> m_dfa_final_states;

        // caches
        cache_eat_epsilon_star_t m_cache_eat_epsilon_star;
        cache_batch_eat_edge_t   m_cache_batch_eat_edge;
        remove_epsilon_cache_t   m_remove_epsilon_cache;
        cache_move_functions_t   m_cache_move_functions;
    }; // class DFAAux2

    _DEPRECATED_ALGO static inline void debug_print(const ordered_unique_vec<State>& states) {
        (void)states;
#ifndef NDEBUG
        if (!states.empty()) {
            std::cout << states.front();
        }

        for (auto iter = states.begin() + 1; iter != states.end(); ++iter) {
            const State& s = *iter;
            std::cout << ", " << s;
        }
#endif // NDEBUG
    }

    _DEPRECATED_ALGO static inline void debug_print(const DFAAux::move_functions_t& dfa_aux_move_functions) {
        (void)dfa_aux_move_functions;
#ifndef NDEBUG
        std::cout << std::endl << "```" << std::endl << "DFAAux::move_functions:" << std::endl;
        for (auto&& pair : dfa_aux_move_functions) {
            const ordered_unique_vec<State>& key = pair.first;
            const edge_to_states_t& mapped = pair.second;

            std::cout << "["; debug_print(key); std::cout << "] :" << std::endl;
            for (auto&& etss : mapped) {
                Edge e = etss.first;
                const ordered_unique_vec<State>& tos = etss.second;
                std::cout << "    [" << e << "] : {"; debug_print(tos); std::cout << "}" << std::endl;
            }
        }
        std::cout << std::endl << "```" << std::endl;
#endif // NDEBUG
    }
} // namespace details

class EpsilonNFA : public details::FABase
{
    typedef details::edge_to_states_t edge_to_states_t;
    typedef details::state_from_to_t  state_from_to_t;

public:
    bool move_func(State s1, Edge edge, const std::vector<State>& s2) {
        if (s2.empty() || (!valid_edge(edge)) || invalid_state == s1) {
            return false;
        }
        //if (epsilon == edge || s2.size() > 1u) {
        //    m_is_already_dfa = false; // 加速 to_dfa
        //}

        edge_to_states_t& edge_to_states = m_move_func[s1];
        ordered_unique_vec<State>& to_states = edge_to_states[edge];

        for (State s : s2) {
            if (s == s1 && epsilon == edge) {
                continue; // 排除掉 s1 -- ε --> s1
            }
            if (invalid_state == s) {
                return false;
            }
            to_states.push_back(s); // 后续再集中 make_ordered_unique
        }
        make_ordered_unique_mut_vec(to_states);

        // 收集输入的边
        if (!m_input_edges.binary_search(edge))
        {
            m_input_edges.ordered_push(edge);
        }

        // 收集输入的状态
        m_input_states.push_back(s1);
        m_input_states.insert(m_input_states.end(), s2.begin(), s2.end());
        make_ordered_unique_mut_vec(m_input_states);

        return true;
    }

    void final_states(const std::vector<State>& s) {
        m_final_states.ordered_assgin_vec(s);
    }

    void start_state(State s0) {
        assert(invalid_state != s0);
        m_start_state = s0;
    }

    bool has_epsilon() const {
        if (!m_input_edges.empty()) {
            return epsilon == m_input_edges.front();
        }
        return false;
    }

    std::string to_mermaid(const char* endline = "\n") const {
        std::string ret;

        std::unordered_map<state_from_to_t, std::vector<Edge>, state_from_to_t::hash> state_pair_map_aux;
        for (auto&& m : m_move_func) {
            const State& s1 = m.first;
            const edge_to_states_t& edge_to_states = m.second;

            for (auto&& e2ss : edge_to_states) {
                Edge edge = e2ss.first;
                const ordered_unique_vec<State>& states2 = e2ss.second;
                for (State s2 : states2) {
                    std::vector<Edge>& edges = state_pair_map_aux[state_from_to_t{ s1, s2 }];
                    edges.push_back(edge);
                    //make_ordered_unique_mut_vec(edges); // it's not necessary
                }
            }
        }

        return to_mermaid_impl(state_pair_map_aux, endline, [this](std::vector<Edge>& edges) {
            return get_edges_string(make_ordered_unique_mut_vec(edges));
        });
    }

    _DEPRECATED_ALGO typedef details::DFAAux DFAAux;
    typedef details::DFAAux2 DFAAux2;
    bool to_dfa(DFA& dfa, std::string& err) const {
        dfa.reset();

        if (invalid_state == m_start_state) {
            err = "Illegal initial state!";
            return false;
        }
        err.clear();
        DFAAux2 dfa_aux;
        if (!to_dfa_aux(dfa_aux, err)) {
            return false;
        }

        return dfa_aux.to_dfa(dfa, err);
    }

private:
    bool to_dfa_aux(DFAAux2& dfa_aux, std::string& err) const {
        dfa_aux.set_start_state(m_start_state);
        dfa_aux.set_enfa_max_state(enfa_max_input_state());
        dfa_aux.set_enfa_base(this);
        if ((!is_final_state(m_start_state)) && dfa_aux_final_reachable(m_start_state, dfa_aux)) {
            assert(m_start_state == dfa_aux.m_start_state);
            dfa_aux.m_dfa_final_states.insert(dfa_aux.m_start_state); // 对起点做终止染色
        }
        return make_dfa_transition_table(dfa_aux, err);
    } // to_dfa_aux

    bool make_dfa_transition_table(DFAAux2& dfa_aux, std::string& err) const {
        assert(invalid_state != dfa_aux.m_start_state);
        return dfa_aux_move_each_edge({ dfa_aux.m_start_state }, dfa_aux, err); // 这样递归下去的一定都是可达状态
    } // make_dfa_transition_table

    bool dfa_aux_move_each_edge(const ordered_unique_vec<State>& dfa_aux_from_state, DFAAux2& dfa_aux, std::string& err) const {
        if (dfa_aux_from_state.empty()) {
            return true; // 忽略此状态
        }

        if (dfa_aux.m_dfa_state_map.break_loop(dfa_aux_from_state)) {
            return true; // 此状态已经被计算过了(破环)
        }

        const bool has_epsilon_edge = has_epsilon();
        if (1u == dfa_aux_from_state.size()) { // 尝试加速 （此加速算法只能处理没有 epsilon 出边的原始状态）
            State s = dfa_aux_from_state.back();
            auto found = m_move_func.find(s);
            if (m_move_func.end() != found) {
                const edge_to_states_t& edge_to_states = found->second;
                const bool can_speed_up = has_epsilon_edge ? (edge_to_states.end() == edge_to_states.find(epsilon)) : true;
                if (can_speed_up) { // 当前状态的出边没有 epsilon 才可以加速！！！
                    assert(edge_to_states.size() <= m_input_edges.size());
                    for (auto&& pair : edge_to_states) {
                        const Edge edge = pair.first;
                        assert(epsilon != edge);
                        ordered_unique_vec<State> dfa_aux_next_state = dfa_aux_move_to_next(dfa_aux_from_state, edge, dfa_aux, err);
                        assert(pair.second.size() <= dfa_aux_next_state.size());
                        if (!dfa_aux_move_each_edge(dfa_aux_next_state, dfa_aux, err)) {
                            return false;
                        }
                    }
                    return true;
                } // else { 不能应用此加速算法，走朴素算法 }
            }
            else if (!m_input_states.binary_search(s)) {
                err = "invalid state: `" + std::to_string(s) + "` in the func dfa_aux_move_each_edge()";
                return false;
            }
            else { assert(is_final_state(s)); }
        }

        // 朴素算法
        auto input_edges_iter = m_input_edges.begin();
        if (has_epsilon_edge) {
            ++input_edges_iter; // skip epsilon
        }

        for (; input_edges_iter != m_input_edges.end(); ++input_edges_iter) {
            const Edge edge = *input_edges_iter;
            ordered_unique_vec<State> dfa_aux_next_state = dfa_aux_move_to_next(dfa_aux_from_state, edge, dfa_aux, err);
            if (!dfa_aux_move_each_edge(dfa_aux_next_state, dfa_aux, err)) {
                return false;
            }
        }

        return true;
    } // dfa_aux_move_each_edge

    ordered_unique_vec<State> dfa_aux_move_to_next(State state, Edge edge, DFAAux2& dfa_aux) const {
        // nfa 单值状态的转移，不一定是可达状态
        assert(epsilon != edge);
        assert(invalid_state != state);
        assert(m_input_states.binary_search(state));

        const ordered_unique_vec<State>* found = dfa_aux.m_remove_epsilon_cache.find(state, edge);
        if (nullptr != found) {
            return *found;
        }

        // remove epsilon : eat epsilon* edge epsilon*
        ordered_unique_vec<State> to_states0;
        eat_epsilon_star({ state }, to_states0, dfa_aux.m_cache_eat_epsilon_star);
        assert(!to_states0.empty()); // 至少应该包含 current_state

        ordered_unique_vec<State> to_states1 = batch_eat_edge(to_states0, edge, &dfa_aux.m_cache_batch_eat_edge);
        if (to_states1.empty()) { // 此路不通，放弃此路径
            return dfa_aux.m_remove_epsilon_cache.update(state, edge, to_states1);
        }
        ordered_unique_vec<State> to_states2;
        eat_epsilon_star(std::move(to_states1), to_states2, dfa_aux.m_cache_eat_epsilon_star);
        assert(!to_states2.empty());
        return dfa_aux.m_remove_epsilon_cache.update(state, edge, to_states2);
    }

    ordered_unique_vec<State> dfa_aux_move_to_next(const ordered_unique_vec<State>& state,
        Edge edge, DFAAux2& dfa_aux, std::string& err) const {
        // 此函数的输入 state 和 返回值 如果不为空，则它们一定是 dfa 的可达状态
        assert(epsilon != edge);
        (void)err;
        if (state.empty()) {
            return {};
        }

        auto dfa_aux_cache_guard = dfa_aux.generate_cache_guard(state, edge);
        if (!dfa_aux_cache_guard.transfer_init_state()) { // 此输入仍在外层的递归函数栈上，需要破环
            return {}; // 但是 dfa_aux_move_each_edge() 中已经做了破环，这个分支可能没有机会再进来
        }

        if (dfa_aux_cache_guard.has_been_calculated()) {
            return dfa_aux_cache_guard.calculated_value(); // 有可能不会收敛？
        }

        if (1u == state.size()) { // nfa 单值状态的转移
            ordered_unique_vec<State> ret = dfa_aux_move_to_next(state.back(), edge, dfa_aux);
            return dfa_aux_cache_guard.return_value(ret);
        }

        ordered_unique_vec<State> ret;
        for (State substate : state) {
            ordered_unique_vec<State> res = dfa_aux_move_to_next(substate, edge, dfa_aux); // 统计每个单值状态转移
            ret.insert(ret.end(), res.begin(), res.end());
        }
        return dfa_aux_cache_guard.return_value(make_ordered_unique_mut_vec(ret));
    }

    _DEPRECATED_ALGO bool to_dfa_aux(DFAAux& dfa_aux, std::string& err) const {

        dfa_aux.set_start_state(m_start_state);

        if ((!is_final_state(m_start_state)) && dfa_aux_final_reachable(m_start_state, dfa_aux)) {
            assert(m_start_state == dfa_aux.m_start_state);
            dfa_aux.m_final_states.ordered_push(dfa_aux.m_start_state); // 对起点做终止染色
        }

        if (!dfa_aux_remove_epsilons(m_start_state, dfa_aux, err)) {
            return false;
        }

        if (!dfa_aux_collect_merged_states(dfa_aux, err)) {
            return false;
        }

        debug_print(dfa_aux.m_move_functions);

        return dfa_aux_convert_to_state_pair(dfa_aux);
    }// to_dfa_aux

    void eat_epsilon_star_impl(const ordered_unique_vec<State>& states,
        ordered_unique_vec<State>& ret, std::unordered_set<State>& state_hit_map) const {

        for (State fs : states) {
            if (state_hit_map.insert(fs).second) {
                const ordered_unique_vec<State>* tos = eat_edge(fs, epsilon);
                if (nullptr != tos && !tos->empty()) {
                    ret.insert(ret.end(), tos->begin(), tos->end()); // 后续集中 make ordered unique
                    eat_epsilon_star_impl(*tos, ret, state_hit_map);
                }
            }
        }
    }

    void eat_epsilon_star(ordered_unique_vec<State>&& states, ordered_unique_vec<State>& ret, details::cache_eat_epsilon_star_t& cache) const {
        assert((!states.empty()) && ret.empty());
        if (!has_epsilon()) {
            assert(&ret != &states); // make sure it works with gcc
            ret = std::move(states);
            return;
        }

        auto found_cache = cache.find(states);
        if (found_cache != cache.end()) {
            ret = found_cache->second; // 缓存加速
            return;
        }

        ret = states;

        std::unordered_set<State> state_hit_map;
        eat_epsilon_star_impl(states, ret, state_hit_map);
        make_ordered_unique_mut_vec(ret);

        cache.insert({ states, ret }); // 更新缓存
        return;
    }

    const ordered_unique_vec<State>* eat_edge(State from, Edge edge) const {
        assert(invalid_state != from);
        assert(m_input_states.binary_search(from)); // 若断言失败说明 from 状态非法

        auto found_s1 = m_move_func.find(from);
        if (m_move_func.end() != found_s1) {
            const edge_to_states_t& edge_to_states = found_s1->second;
            auto found_s2 = edge_to_states.find(edge);
            if (edge_to_states.end() != found_s2) {
                const ordered_unique_vec<State>& all_s2 = found_s2->second;
                return &all_s2;
            }
        }
        return nullptr; // 返回 nullptr 意味着不存在此出边；或者 from 状态非法
    }

    ordered_unique_vec<State> batch_eat_edge(const ordered_unique_vec<State>& froms, Edge edge, details::cache_batch_eat_edge_t* cache = nullptr) const {
        assert((!froms.empty()));

        const bool enable_cache = (nullptr != cache) && (froms.size() > 1u);
        if (enable_cache) {
            details::cache_batch_eat_edge_t::key_t cache_key{ froms, edge };
            auto found_cache = cache->find(cache_key);
            if (found_cache != cache->end()) {
                return found_cache->second; // 缓存加速
            }
        }

        ordered_unique_vec<State> res;
        for (State s1 : froms) {
            const ordered_unique_vec<State>* tos = eat_edge(s1, edge);
            if (nullptr != tos && !tos->empty()) {
                res.insert(res.end(), tos->begin(), tos->end()); // 后续集中 make_ordered_unique
            }
        }
        make_ordered_unique_mut_vec(res);

        if (enable_cache) {
            details::cache_batch_eat_edge_t::key_t cache_key{ froms, edge };
            cache->insert(std::make_pair(cache_key, res)); // 更新缓存
        }
        return res;
    }

    _DEPRECATED_ALGO bool dfa_aux_final_reachable(State state, DFAAux& dfa_aux) const {
        if (m_move_func.end() == m_move_func.find(state)) {
            return false;
        }

        if (is_final_state(state)) {
            return true;
        }

        ordered_unique_vec<State> to_states;
        eat_epsilon_star({ state }, to_states, dfa_aux.m_cache_eat_epsilon_star);
        assert(!to_states.empty()); // 至少应该包含原始输入状态
        for (State s : to_states) {
            if (is_final_state(s)) {
                return true;
            }
        }
        return false;
    }

    bool dfa_aux_final_reachable(State state, DFAAux2& dfa_aux) const {
        if (m_move_func.end() == m_move_func.find(state)) {
            return false;
        }

        if (is_final_state(state)) {
            return true;
        }

        ordered_unique_vec<State> to_states;
        eat_epsilon_star({ state }, to_states, dfa_aux.m_cache_eat_epsilon_star);
        assert(!to_states.empty()); // 至少应该包含原始输入状态
        for (State s : to_states) {
            if (is_final_state(s)) {
                return true;
            }
        }
        return false;
    }

    _DEPRECATED_ALGO bool dfa_aux_remove_epsilons(State current_state, DFAAux& dfa_aux, std::string& err) const {
        // 职责：移除 epsilon 的同时也去除死状态，并只收集所有的 “单值 from_state”到 dfa_aux
        if (dfa_aux.find_from_state({ current_state })) {
            // 已经将此单值状态作为 from_state 收集过了
            return true;
        }

        assert(m_input_states.binary_search(current_state));
        auto input_edges_iter = m_input_edges.begin();
        if (has_epsilon()) {
            ++input_edges_iter; // skip epsilon
        }

        for (; input_edges_iter != m_input_edges.end(); ++input_edges_iter) {
            Edge edge = *input_edges_iter;
            // remove epsilon : eat epsilon* edge epsilon*
            ordered_unique_vec<State> to_states0;
            eat_epsilon_star({ current_state }, to_states0, dfa_aux.m_cache_eat_epsilon_star);
            assert(!to_states0.empty()); // 至少应该包含 current_state

            ordered_unique_vec<State> to_states1 = batch_eat_edge(to_states0, edge, &dfa_aux.m_cache_batch_eat_edge);
            if (to_states1.empty()) { // 此路不通，放弃此路径
                // 放弃此路径
                continue;
            }
            ordered_unique_vec<State> to_states2;
            eat_epsilon_star(std::move(to_states1), to_states2, dfa_aux.m_cache_eat_epsilon_star);
            assert(!to_states2.empty());

            // 所有可能的 “单值 from_state” 的 move functions  收集到 dfa_aux
            dfa_aux.move_func(current_state, edge, to_states2);

            // update dfa_aux.m_merged_states_temp
            if (to_states2.size() > 1u) {
                dfa_aux.m_merged_states_temp.insert(to_states2);
            }

            // 继续收集所有可能的 “单值 from_state”
            for (State next_state : to_states2) {
                if (!dfa_aux_remove_epsilons(next_state, dfa_aux, err)) {
                    return false;
                }
            }
        }

        return true;
    } // dfa_aux_remove_epsilons

    _DEPRECATED_ALGO bool dfa_aux_collect_merged_states(DFAAux& dfa_aux, std::string& err) const {
        const std::size_t merged_states_temp_size = dfa_aux.m_merged_states_temp.size();
        if (0 == merged_states_temp_size) { // 注意这里：状态数可能很难收敛，需要添加递归次数限制 @TODO
            return true;
        }

        {
            std::unordered_set<ordered_unique_vec<State>, details::merged_states_hash> new_merged_states_temp;
            for (const ordered_unique_vec<State>& merged_state : dfa_aux.m_merged_states_temp) {
                assert(merged_state.size() > 1u);

                if (dfa_aux.find_from_state(merged_state)) {
                    // 已经将此状态作为 from_state 收集过了
                    continue;
                }

                edge_to_states_t merged_edge_map;
                for (State s : merged_state) {
                    auto found = dfa_aux.m_move_functions.find({ s });
                    if (dfa_aux.m_move_functions.end() == found) {
                        if (m_input_states.binary_search(s)) {
                            continue; // 说明此状态没有出边（比如没有出边的终止状态），跳过
                        }

                        err = "state `" + get_state_name(s) + "` does not exist in dfa_aux.m_move_functions!";
                        // BUG: 说明上一步 dfa_aux_remove_epsilons 没有将所有可能的
                        // “单值 from_state” 的 move functions 收集到 dfa_aux 之中
                        return false;
                    }

                    const edge_to_states_t& edge_map = found->second;
                    // merged_edge_map += edge_map
                    for (auto&& pair : edge_map) {
                        if (!pair.second.empty()) {
                            ordered_unique_vec<State>& states = merged_edge_map[pair.first];
                            states.ordered_insert(pair.second);
                        }
                    }
                }

                // update new_merged_states_temp
                for (auto&& pair : merged_edge_map) {
                    if (pair.second.size() > 1u) {
                        new_merged_states_temp.insert(pair.second);
                    }
                }

                if (!merged_edge_map.empty()) {
                    dfa_aux.move_func(merged_state, std::move(merged_edge_map));
                }
            } // end for dfa_aux.m_merged_states_temp

            assert(&dfa_aux.m_merged_states_temp != &new_merged_states_temp); // make sure it works with gcc
            dfa_aux.m_merged_states_temp = std::move(new_merged_states_temp);
        }

        return dfa_aux_collect_merged_states(dfa_aux, err);  // tail call
    }

    //// 此版本的 dfa_aux_convert_to_state_pair() 可以有效的移除所有的死状态，但性能略差
    _DEPRECATED_ALGO bool dfa_aux_convert_to_state_pair(DFAAux& dfa_aux) const {
        // 职责：将 dfa_aux.m_move_functions 转换为 dfa_aux.m_state_pair_map 和 dfa_aux.m_dfa_move_functions，
        //       并标记 final state、 收集 dfa 状态和再次去除死状态
        std::unordered_map<ordered_unique_vec<State>, State, details::merged_states_hash> new_state_map;
        dfa_aux.m_dfa_state_set.clear(); // 在 dfa_aux_convert_to_state_pair_impl() 中它仅用来做破环，并不包含 next 状态集
        return dfa_aux_convert_to_state_pair_impl(dfa_aux, { dfa_aux.m_start_state }, new_state_map);
    }

    _DEPRECATED_ALGO bool dfa_aux_convert_to_state_pair_impl(DFAAux& dfa_aux, const ordered_unique_vec<State>& dfa_aux_state1,
        std::unordered_map<ordered_unique_vec<State>, State, details::merged_states_hash>& new_state_map) const {

        auto found_state = dfa_aux.m_move_functions.find(dfa_aux_state1);
        if (dfa_aux.m_move_functions.end() == found_state) {
            return true; // 有可能是没有出边的终止状态，目前并没有被放入 dfa_aux.m_dfa_state_set
        }
        const State dfa_s1 = dfa_aux_calc_dfa_state(dfa_aux_state1, dfa_aux, new_state_map);
        if (invalid_state == dfa_s1) {
            return false; // 内存不足 或者 BUG
        }

        if (!dfa_aux.m_dfa_state_set.insert(dfa_s1).second) {
            return true; // 破环，并只收集 dfa 的 from 状态集
        }

        const edge_to_states_t& edge_to_states = found_state->second;
        if (edge_to_states.empty()) {
            return true;
        }

        std::unordered_map<Edge, State>& dfa_edge_to_state_map = dfa_aux.m_dfa_move_functions[dfa_s1];
        for (auto&& e2ss : edge_to_states) {
            Edge edge = e2ss.first;
            const ordered_unique_vec<State>& dfa_aux_states2 = e2ss.second;

            const State dfa_s2 = dfa_aux_calc_dfa_state(dfa_aux_states2, dfa_aux, new_state_map);
            ordered_unique_vec<Edge>& edges = dfa_aux.m_state_pair_map[state_from_to_t{ dfa_s1, dfa_s2 }];
            edges.ordered_push(edge);

            dfa_edge_to_state_map[edge] = dfa_s2; // for dfa.try_minimize()

            if (!dfa_aux_convert_to_state_pair_impl(dfa_aux, dfa_aux_states2, new_state_map)) {
                return false;
            }
        }

        return true;
    }

    _DEPRECATED_ALGO State dfa_aux_calc_dfa_state(const ordered_unique_vec<State>& dfa_aux_state, DFAAux& dfa_aux,
        std::unordered_map<ordered_unique_vec<State>, State, details::merged_states_hash>& new_state_map) const {
        // 职责：由 dfa_aux 的状态 得到 dfa 的状态，并对终止状态进行染色
        State dfa_state = invalid_state;
        if (dfa_aux_state.size() == 1u) {
            dfa_state = dfa_aux_state.back();
            if (is_final_state(dfa_state) && (!dfa_aux.is_final_state(dfa_state))) {
                dfa_aux.m_final_states.ordered_push(dfa_state); // 染色
            }
        }
        else {
            assert(dfa_aux_state.size() > 1u);
            auto found = new_state_map.find(dfa_aux_state);
            if (new_state_map.end() != found) {
                dfa_state = found->second;
            }
            else { // 正常情况下，此分支对于同一个 dfa_aux_state 最多只会进来一次
                dfa_state = new_state_map.size() + enfa_max_input_state() + 1u;
                if (!new_state_map.insert({ dfa_aux_state, dfa_state }).second) {
                    return invalid_state; // 内存不足 或者 BUG
                }

                for (State s : dfa_aux_state) {
                    if (is_final_state(s)) {
                        dfa_aux.m_final_states.ordered_push(dfa_state); // 染色
                        break;
                    }
                }
            }
        }
        return dfa_state;
    }

    State enfa_max_input_state() const {
        return m_input_states.empty() ? 0 : m_input_states.back();
    }

private:
    std::unordered_map<State, edge_to_states_t>  m_move_func;
    ordered_unique_vec<Edge>                     m_input_edges;  // to speed up `to_dfa()`
    ordered_unique_vec<State>                    m_input_states;
    //bool                                        m_is_already_dfa = true;
}; // class EpsilonNFA

/////////////////////////////////////////////////////////////////////////////////////////////////
#include <sstream>
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

static inline void t_assert_abort(const char* msg, const char* file, unsigned line) {
    std::cerr << "[FATAL]: \"" << msg << "\", " << file << ":" << line << std::endl;
    std::abort();
}

#undef TEST_ASSERT
#define TEST_ASSERT(expression) (void)( (!!(expression)) || (t_assert_abort((#expression), (__FILE__), (unsigned)(__LINE__)), 0) )


int test_cast1() {
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
    0(0-S)  --a--> 1;
    0(0-S)  --ε--> 3(3-E);
    0(0-S)  --ε--> 2;
    2  --ε--> 0(0-S);
    3(3-E)  --ε--> 0(0-S);
    1  --b--> 2;
    2  --c--> 3(3-E);
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
    0(0-S-E)  --c--> 4(4-E);
    0(0-S-E)  --a--> 1;
    4(4-E)  --a--> 1;
    1  --b--> 4(4-E);
    4(4-E)  --c--> 4(4-E);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));
    return 0;
}

int test_cast2() {
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
    0(0-S)  --ε--> 8(8-E);
    0(0-S)  --ε--> 1;
    4  --ε--> 5;
    8(8-E)  --ε--> 0(0-S);
    1  --ε--> 2;
    1  --ε--> 5;
    5  --ε--> 1;
    2  --a--> 3;
    3  --b--> 4;
    5  --ε--> 6;
    6  --c--> 7;
    7  --ε--> 8(8-E);
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
    0(0-S-E)  --c--> 10(10-E);
    0(0-S-E)  --a--> 3;
    9  --c--> 10(10-E);
    9  --a--> 3;
    3  --b--> 9;
    10(10-E)  --a--> 3;
    10(10-E)  --c--> 10(10-E);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));
    return 0;
}

int test_cast3() {
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
    0(0-S)  --ε--> 8;
    0(0-S)  --ε--> 1;
    4  --ε--> 5;
    8  --ε--> 0(0-S);
    1  --ε--> 2;
    1  --ε--> 5;
    5  --ε--> 1;
    2  --a--> 3;
    3  --b--> 4;
    5  --ε--> 6;
    6  --c--> 7;
    7  --ε--> 8;
    8  --ε--> 9;
    9  --ε--> 10;
    13(13-E)  --ε--> 9;
    9  --ε--> 13(13-E);
    10  --a--> 11;
    11  --b--> 12;
    12  --ε--> 13(13-E);
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
    0(0-S-E)  --c--> 16(16-E);
    0(0-S-E)  --a--> 14;
    16(16-E)  --a--> 14;
    15(15-E)  --a--> 14;
    14  --b--> 15(15-E);
    15(15-E)  --c--> 16(16-E);
    16(16-E)  --c--> 16(16-E);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    dfa.try_minimize();
    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "minimized DFA: " << std::endl << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S-E)  --c--> 0(0-S-E);
    0(0-S-E)  --a--> 14;
    14  --b--> 0(0-S-E);
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
    0(0-S)  --[a-c]--> 0(0-S);
    0(0-S)  --ε--> 1;
    1  --a--> 2;
    2  --b--> 3(3-E);
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
    0(0-S)  --[b-c]--> 6;
    0(0-S)  --a--> 4;
    6  --[b-c]--> 6;
    4  --a--> 4;
    4  --b--> 5(5-E);
    5(5-E)  --a--> 4;
    5(5-E)  --[b-c]--> 6;
    6  --a--> 4;
    4  --c--> 6;
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    dfa.try_minimize();
    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "minimize DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S)  --[b-c]--> 0(0-S);
    0(0-S)  --a--> 4;
    4  --c--> 0(0-S);
    4  --a--> 4;
    5(5-E)  --a--> 4;
    4  --b--> 5(5-E);
    5(5-E)  --[b-c]--> 0(0-S);
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    return 0;
}

int test_cast5() {
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
    0(0-S)  --a--> 2;
    0(0-S)  --a--> 1;
    1  --b--> 1;
    1  --d--> 3;
    2  --c--> 3;
    3  --e--> 4(4-E);
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
    0(0-S)  --a--> 5;
    5  --b--> 1;
    1  --b--> 1;
    1  --d--> 3;
    3  --e--> 4(4-E);
    5  --[c-d]--> 3;
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));


    dfa.try_minimize();
    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "minimize DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    expected_result = R"==(```mermaid
graph TD;
    0(0-S)  --a--> 5;
    5  --b--> 1;
    1  --b--> 1;
    1  --d--> 3;
    3  --e--> 4(4-E);
    5  --[c-d]--> 3;
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

    return 0;
}

int test_cast6() {
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
    0(0-S)  --a--> 1;
    1  --a--> 2;
    4  --a--> 1;
    2  --a--> 3;
    3  --a--> 4;
    3  --a--> 5(5-E);
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
    0(0-S)  --a--> 1;
    3  --a--> 6(6-E);
    1  --a--> 2;
    2  --a--> 3;
    6(6-E)  --a--> 1;
```)==";

    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));


    dfa.try_minimize();
    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "minimize DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;
    TEST_ASSERT(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));
    return 0;
}

int main()
{
    // https://www.mermaidflow.app/editor#/
    // https://mermaid.live/edit
    int ret = 0;

    ret = test_cast1(); TEST_ASSERT(0 == ret);
    ret = test_cast2(); TEST_ASSERT(0 == ret);
    ret = test_cast3(); TEST_ASSERT(0 == ret);
    ret = test_case4(); TEST_ASSERT(0 == ret);
    ret = test_cast5(); TEST_ASSERT(0 == ret);
    ret = test_cast6(); TEST_ASSERT(0 == ret);
    return 0;
}

