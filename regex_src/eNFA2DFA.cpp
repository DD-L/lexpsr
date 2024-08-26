// eNFA2DFA.cpp : 此文件包含 "main" 函数。程序执行将在此处开始并结束。
//

#include <iostream>
#include <cstdint>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <assert.h>
#include <algorithm>
#include <string>
#include <cctype>

typedef uint64_t State;
typedef int32_t  Edge;

template <class T>
class ordered_uniqe_vec;

template <class T>
static inline ordered_uniqe_vec<T>& make_ordered_unique_mut_vec(std::vector<T>& vec);

template <class T>
class ordered_uniqe_vec : public std::vector<T> {
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

    void ordered_insert(const ordered_uniqe_vec<T>& vec) {
        ordered_insert(static_cast<const base_type&>(vec));
    }

    template <class Vec>
    void ordered_assgin_vec(Vec&& vec) {
        static_cast<base_type&>(*this) = std::forward<Vec>(vec);
        make_ordered_unique_mut_vec(*this);
    }
}; // class ordered_uniqe_vec<T>

constexpr Edge  epsilon = -1;
constexpr State invalid_state = ~static_cast<State>(0);

static constexpr inline bool valid_edge(Edge e) {
    return e >= epsilon;
}

template <class T>
static inline ordered_uniqe_vec<T>& make_ordered_unique_mut_vec(std::vector<T>& vec) {
    if (vec.size() > 1u) {
        std::sort(vec.begin(), vec.end());
        auto last = std::unique(vec.begin(), vec.end());
        assert(vec.begin() <= last);
        const std::size_t size = std::distance(vec.begin(), last);
        vec.resize(size);
    }
    return static_cast<ordered_uniqe_vec<T>&>(vec);
}

namespace details {
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

        struct equal {
            bool operator()(const state_pair_t& sft1, const state_pair_t& sft2) const {
                return sft1.s1 == sft2.s1 && sft1.s2 == sft2.s2;
            }
        };
    };

    typedef state_pair_t state_from_to_t;

    typedef std::unordered_map<
        state_from_to_t, ordered_uniqe_vec<Edge>,
        state_from_to_t::hash, state_from_to_t::equal>
    state_pair_map_t;

    typedef std::unordered_map<Edge, ordered_uniqe_vec<State>> edge_to_states_t;
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

        std::string get_edges_string(const ordered_uniqe_vec<Edge>& edges_set) const {
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
            ordered_uniqe_vec<Edge> edges_vec(edges.begin(), edges.end());
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

        void reset(State start_state, ordered_uniqe_vec<State>&& final_states) {
            m_start_state = start_state;
            m_final_states = std::move(final_states);
        }

        void reset() {
            m_start_state = invalid_state;
            m_final_states.clear();
        }

    protected:
        State                       m_start_state = invalid_state;
        ordered_uniqe_vec<State>    m_final_states;
    }; // class FABase
} // namespace details

class DFA : public details::FABase
{
    typedef details::state_from_to_t       state_from_to_t;
    typedef details::state_pair_t          state_pair_t;
    typedef details::dfa_move_functions_t  move_functions_t;

    typedef details::state_pair_map_t state_pair_map_t;
    typedef std::unordered_map<state_pair_t, bool, state_pair_t::hash, state_pair_t::equal> merged_states_cache_t;

public:
    bool is_start_state(State state) const {
        return m_start_state == state;
    }

    bool is_final_state(State state) const {
        return m_final_states.binary_search(state);
    }

    std::string to_mermaid(const char* endline = "\n") const {
        return to_mermaid_impl(m_state_pair_map, endline, [this](const ordered_uniqe_vec<Edge>& edges) {
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
    void reset(State start_state, state_pair_map_t&& state_pair_map, ordered_uniqe_vec<State>&& final_states,
         std::unordered_set<State>&& state_set, move_functions_t&& move_functions) {
        details::FABase::reset(start_state, std::move(final_states));
        m_state_pair_map = std::move(state_pair_map);
        m_move_functions = std::move(move_functions);
        m_state_set = std::move(state_set);
    }

    void reset() {
        details::FABase::reset();
        m_state_pair_map.clear();
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
            const ordered_uniqe_vec<Edge>& edges = pair.second;

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

    bool can_be_merged(State s1, State s2, merged_states_cache_t& cache) const {
        // 找出所有不能合并的状态对（剩下的都是可以合并的）
        if (s1 == s2) { return true; } // 不必缓存相同的 state
        if (s1 > s2) { std::swap(s1, s2); }
        assert(s1 < s2);
        const state_pair_t state_pair{ s1, s2 };
        auto found_cache = cache.find(state_pair);
        if (cache.end() != found_cache) {
            return found_cache->second; // 缓存加速
        }

        const bool s1_is_final = is_final_state(s1);
        const bool s2_is_final = is_final_state(s2);
        if (s1_is_final != s2_is_final) { // 一个是终点另一个不是终点，一定不能合并
            cache.insert({ state_pair, false });
            return false;
        }

        auto found_s1_iter = m_move_functions.find(s1);
        auto found_s2_iter = m_move_functions.find(s2);
        const bool found_s1 = found_s1_iter != m_move_functions.end();
        const bool found_s2 = found_s2_iter != m_move_functions.end();

        if (found_s1 != found_s2) {
            // 如果一个有出边，另一个没有出边，那么这两个状态一定不能合并（出边集不同）
            cache.insert({ state_pair, false });
            return false;
        }

        if (found_s1 && found_s2) {
            const std::unordered_map<Edge, State>& s1_map = found_s1_iter->second;
            const std::unordered_map<Edge, State>& s2_map = found_s2_iter->second;
            if (s1_map.size() != s1_map.size()) { // 出边集个数不同，一定不能合并
                cache.insert({ state_pair, false });
                return false;
            }

            for (auto&& s1_pair : s1_map) {
                Edge edge = s1_pair.first;
                auto found_s2_map_iter = s2_map.find(edge);
                if (s2_map.end() == found_s2_map_iter) { // 出边集不同，一定不能合并
                    cache.insert({ state_pair, false });
                    return false;
                }

                State next_s1 = s1_pair.second;
                State next_s2 = found_s2_map_iter->second;
                if (!can_be_merged(next_s1, next_s2, cache)) {
                    cache.insert({ state_pair, false });
                    return false;
                }
            }
        }
        else {
            (void)cache;
            assert(!found_s1 && !found_s2); // 两个状态都没有出边, 可以合并
            assert(s1_is_final && s2_is_final); // 因为已经消除了死状态，所以没有出边一定是终止状态
            assert(!is_start_state(s1) && !is_start_state(s2)); // 起点一定有出边
        }

        cache.insert({ state_pair, true });
        return true; // 可以合并
    }

    static inline unsigned long long combination_n_choose_2(std::size_t N) {
        if (N < 2u) { return 0; } // C(N, 2) is 0 for N < 2
        return static_cast<unsigned long long>(N) * (N - 1u) / 2u;
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

private:
    state_pair_map_t          m_state_pair_map;  // for to_mermaid()
    move_functions_t          m_move_functions;  // for try_minimize()
    std::unordered_set<State> m_state_set;
    bool                      m_minimized = false;
}; // class DFA

namespace details {

    struct merged_states_hash {
        std::size_t operator()(const ordered_uniqe_vec<State>& state_vec) const {
            std::size_t hash_code = 0;
            for (State s : state_vec) {
                hash_code += state_hash()(s);
            }
            return hash_code;
        }
    };

    // 缓存
    typedef std::unordered_map<
        ordered_uniqe_vec<State>, ordered_uniqe_vec<State>, merged_states_hash
    > cache_eat_epsilon_star_t;

    struct cache_batch_eat_edge_t {
        struct key_t {
            ordered_uniqe_vec<State> from;
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

        typedef std::unordered_map<key_t, ordered_uniqe_vec<State>, hash, equal> cache_t;

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

    class DFAAux {
    public:
        void set_start_state(State state) {
            m_start_state = state;
        }

        bool move_func(State state, Edge edge, const ordered_uniqe_vec<State>& next_state_vec) {
            return move_func(ordered_uniqe_vec<State>{ state }, edge, next_state_vec);
        }

        bool move_func(const ordered_uniqe_vec<State>& state, Edge edge, const ordered_uniqe_vec<State>& next_state_vec) {
            if (state.empty() || next_state_vec.empty()) {
                return false;
            }
            m_move_functions[state][edge].ordered_insert(next_state_vec);
            return true;
        }

        bool move_func(const ordered_uniqe_vec<State>& state, edge_to_states_t&& merged_edge_map) {
            if (state.empty() || merged_edge_map.empty()) {
                return false;
            }

            edge_to_states_t& edge_map = m_move_functions[state];
            if (edge_map.empty()) {
                edge_map = std::move(merged_edge_map);
                return true;
            }

            for (const auto& pair : merged_edge_map) {
                edge_map[pair.first].ordered_insert(pair.second);
            }
            return true;
        }

        bool find_from_state(const ordered_uniqe_vec<State>& state) const {
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

#ifdef TRY_TO_REMOVE_DEAD_STATES_EARLY
        bool is_redundant_state(State state) const {
            return m_redundant_states.end() != m_redundant_states.find(state);
        }
#endif
    public:
        typedef details::state_pair_map_t state_pair_map_t;
        typedef std::unordered_map<ordered_uniqe_vec<State>, edge_to_states_t, merged_states_hash> move_functions_t;
        typedef std::unordered_set<ordered_uniqe_vec<State>, merged_states_hash> merged_states_temp_t;

    public:
        State                     m_start_state = invalid_state;
        move_functions_t          m_move_functions;
        state_pair_map_t          m_state_pair_map;
        ordered_uniqe_vec<State>  m_final_states;
        std::unordered_set<State> m_dfa_state_set;
        dfa_move_functions_t      m_dfa_move_functions;
        merged_states_temp_t      m_merged_states_temp;
#ifdef TRY_TO_REMOVE_DEAD_STATES_EARLY
        std::unordered_set<State> m_redundant_states;  // 在 remove_epsilon 过程中产生的多余的单值死状态
#endif
        // caches
        cache_eat_epsilon_star_t m_cache_eat_epsilon_star;
        cache_batch_eat_edge_t   m_cache_batch_eat_edge;
    }; // class DFAAux

    static inline void debug_print(const ordered_uniqe_vec<State>& states) {
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

    static inline void debug_print(const DFAAux::move_functions_t& dfa_aux_move_functions) {
        (void)dfa_aux_move_functions;
#ifndef NDEBUG
        std::cout << std::endl << "```" << std::endl << "DFAAux::move_functions:" << std::endl;
        for (auto&& pair : dfa_aux_move_functions) {
            const ordered_uniqe_vec<State>& key = pair.first;
            const edge_to_states_t& mapped = pair.second;

            std::cout << "["; debug_print(key); std::cout << "] :" << std::endl;
            for (auto&& etss : mapped) {
                Edge e = etss.first;
                const ordered_uniqe_vec<State>& tos = etss.second;
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

        edge_to_states_t& edge_to_states = m_move_func[s1];
        ordered_uniqe_vec<State>& to_states = edge_to_states[edge];

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

        std::unordered_map<state_from_to_t, std::vector<Edge>, state_from_to_t::hash, state_from_to_t::equal> state_pair_map_aux;
        for (auto&& m : m_move_func) {
            const State& s1 = m.first;
            const edge_to_states_t& edge_to_states = m.second;

            for (auto&& e2ss : edge_to_states) {
                Edge edge = e2ss.first;
                const ordered_uniqe_vec<State>& states2 = e2ss.second;
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

    typedef details::DFAAux DFAAux;
    bool to_dfa(DFA& dfa, std::string& err) const {
        dfa.reset();

        if (invalid_state == m_start_state) {
            err = "Illegal initial state!";
            return false;
        }
        err.clear();
        DFAAux dfa_aux;
        if (!to_dfa_aux(dfa_aux, err)) {
            return false;
        }

        return dfa_aux.to_dfa(dfa, err);
    }

private:
    bool to_dfa_aux(DFAAux& dfa_aux, std::string& err) const {

        dfa_aux.set_start_state(m_start_state);

        if ((!is_final_state(m_start_state)) && dfa_aux_final_reachable(m_start_state, dfa_aux)) {
            assert(m_start_state == dfa_aux.m_start_state);
            dfa_aux.m_final_states.ordered_push(dfa_aux.m_start_state); // 对起点做终止染色
        }
#ifdef TRY_TO_REMOVE_DEAD_STATES_EARLY
        dfa_aux_init_redundant_states(dfa_aux);
#endif
        if (!dfa_aux_remove_epsilons(m_start_state, dfa_aux, err)) {
            return false;
        }

        if (!dfa_aux_collect_merged_states(dfa_aux, err)) {
            return false;
        }

        debug_print(dfa_aux.m_move_functions);

        return dfa_aux_convert_to_state_pair(dfa_aux);
    }// to_dfa_aux

    void eat_epsilon_star_impl(const ordered_uniqe_vec<State>& states,
        ordered_uniqe_vec<State>& ret, std::unordered_set<State>& state_hit_map) const {

        for (State fs : states) {
            if (state_hit_map.insert(fs).second) {
                const ordered_uniqe_vec<State>* tos = eat_edge(fs, epsilon);
                if (nullptr != tos && !tos->empty()) {
                    ret.insert(ret.end(), tos->begin(), tos->end()); // 后续集中 make ordered uniqe
                    eat_epsilon_star_impl(*tos, ret, state_hit_map);
                }
            }
        }
    }

    void eat_epsilon_star(ordered_uniqe_vec<State>&& states, ordered_uniqe_vec<State>& ret, details::cache_eat_epsilon_star_t& cache) const {
        assert((!states.empty()) && ret.empty());
        if (!has_epsilon()) {
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

    const ordered_uniqe_vec<State>* eat_edge(State from, Edge edge) const {
        assert(invalid_state != from);
        assert(m_input_states.binary_search(from)); // 若断言失败说明 from 状态非法

        auto found_s1 = m_move_func.find(from);
        if (m_move_func.end() != found_s1) {
            const edge_to_states_t& edge_to_states = found_s1->second;
            auto found_s2 = edge_to_states.find(edge);
            if (edge_to_states.end() != found_s2) {
                const ordered_uniqe_vec<State>& all_s2 = found_s2->second;
                return &all_s2;
            }
        }
        return nullptr; // 返回 nullptr 意味着不存在此出边；或者 from 状态非法
    }

    ordered_uniqe_vec<State> batch_eat_edge(const ordered_uniqe_vec<State>& froms, Edge edge, details::cache_batch_eat_edge_t& cache) const {
        assert((!froms.empty()));

        details::cache_batch_eat_edge_t::key_t cache_key{ froms, edge };
        auto found_cache = cache.find(cache_key);
        if (found_cache != cache.end()) {
            return found_cache->second; // 缓存加速
        }

        ordered_uniqe_vec<State> res;
        for (State s1 : froms) {
            const ordered_uniqe_vec<State>* tos = eat_edge(s1, edge);
            if (nullptr != tos && !tos->empty()) {
                res.insert(res.end(), tos->begin(), tos->end()); // 后续集中 make_ordered_unique
            }
        }

        make_ordered_unique_mut_vec(res);

        cache.insert(std::make_pair(cache_key, res)); // 更新缓存
        return res;
    }

    bool dfa_aux_final_reachable(State state, DFAAux& dfa_aux) const {
        if (m_move_func.end() == m_move_func.find(state)) {
            return false;
        }

        if (is_final_state(state)) {
            return true;
        }

        ordered_uniqe_vec<State> to_states;
        eat_epsilon_star({ state }, to_states, dfa_aux.m_cache_eat_epsilon_star);
        assert(!to_states.empty()); // 至少应该包含原始输入状态
        for (State s : to_states) {
            if (is_final_state(s)) {
                return true;
            }
        }
        return false;
    }

#ifdef TRY_TO_REMOVE_DEAD_STATES_EARLY
    void dfa_aux_init_redundant_states(DFAAux& dfa_aux) const {
        dfa_aux.m_redundant_states.clear();
        dfa_aux.m_redundant_states.insert(m_input_states.begin(), m_input_states.end());
        assert(invalid_state != dfa_aux.m_start_state);
        dfa_aux.m_redundant_states.erase(dfa_aux.m_start_state); // 除了起始状态，其他单值状态都有可能是多余的死状态，后续做验证再排除
    }
#endif

    bool dfa_aux_remove_epsilons(State current_state, DFAAux& dfa_aux, std::string& err) const {
        // 职责：移除 epsilon 的同时也去除死状态，并只收集所有的 “单值 from_state”到 dfa_aux
        if (dfa_aux.find_from_state({ current_state })) {
            // 已经将此单值状态作为 from_state 收集过了
            return true;
        }

        assert(m_input_states.binary_search(current_state));
        auto input_edges_iter = m_input_edges.begin();
        if (has_epsilon())
        {
            ++input_edges_iter; // skip epsilon
        }

        for (; input_edges_iter != m_input_edges.end(); ++input_edges_iter) {
            Edge edge = *input_edges_iter;
            // remove epsilon : eat epsilon* edge epsilon*
            ordered_uniqe_vec<State> to_states0;
            eat_epsilon_star({ current_state }, to_states0, dfa_aux.m_cache_eat_epsilon_star);
            assert(!to_states0.empty()); // 至少应该包含 current_state

            ordered_uniqe_vec<State> to_states1 = batch_eat_edge(to_states0, edge, dfa_aux.m_cache_batch_eat_edge);
            if (to_states1.empty()) { // 此路不通，放弃此路径
                // 放弃此路径
                continue;
            }
            ordered_uniqe_vec<State> to_states2;
            eat_epsilon_star(std::move(to_states1), to_states2, dfa_aux.m_cache_eat_epsilon_star);
            assert(!to_states2.empty());

            // 所有可能的 “单值 from_state” 的 move functions  收集到 dfa_aux
            dfa_aux.move_func(current_state, edge, to_states2);

            // update dfa_aux.m_merged_states_temp
            if (to_states2.size() > 1u) {
                dfa_aux.m_merged_states_temp.insert(to_states2);
            }
#ifdef TRY_TO_REMOVE_DEAD_STATES_EARLY
            else if (is_start_state(current_state)) {
                assert(to_states2.size() == 1u);
                //“起点”指向的“单值状态”是可达状态，从 dfa_aux.m_redundant_states 中排除
                dfa_aux.m_redundant_states.erase(to_states2.back());
            }
#endif

            // 继续收集所有可能的 “单值 from_state”
            for (State next_state : to_states2) {
                if (!dfa_aux_remove_epsilons(next_state, dfa_aux, err)) {
                    return false;
                }
            }
        }

        return true;
    } // dfa_aux_remove_epsilons

    bool dfa_aux_collect_merged_states(DFAAux& dfa_aux, std::string& err) const {
        const std::size_t merged_states_temp_size = dfa_aux.m_merged_states_temp.size();
        if (0 == merged_states_temp_size) { // 注意这里：状态数可能很难收敛，需要添加递归次数限制 @TODO
            return true;
        }

        {
            std::unordered_set<ordered_uniqe_vec<State>, details::merged_states_hash> new_merged_states_temp;
            for (const ordered_uniqe_vec<State>& merged_state : dfa_aux.m_merged_states_temp) {
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
                            ordered_uniqe_vec<State>& states = merged_edge_map[pair.first];
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
#ifdef TRY_TO_REMOVE_DEAD_STATES_EARLY
                    if (merged_state.size() == 1u) {
                        // 可达的单值状态，从 dfa_aux.m_redundant_states 中排除
                        dfa_aux.m_redundant_states.erase(merged_state.back());
                    }
#endif
                }
            } // end for dfa_aux.m_merged_states_temp

            dfa_aux.m_merged_states_temp = std::move(new_merged_states_temp);
        }

        return dfa_aux_collect_merged_states(dfa_aux, err);  // tail call
    }

#ifdef TRY_TO_REMOVE_DEAD_STATES_EARLY
    //////// 此版本的 dfa_aux_convert_to_state_pair 算法，未能实现有效的移除所有的死状态(多值)，须开启 TRY_TO_REMOVE_DEAD_STATES_EARLY，但性能略好
    bool dfa_aux_convert_to_state_pair(DFAAux& dfa_aux) const {
        // 职责：将 dfa_aux.m_move_functions 转换为不冗余的 dfa_aux.m_state_pair_map 和 dfa_aux.m_dfa_move_functions
        //       并标记 final state 和 收集 dfa 状态
        std::unordered_map<ordered_uniqe_vec<State>, State, details::merged_states_hash> new_state_map;
        for (auto&& m : dfa_aux.m_move_functions) {
            const ordered_uniqe_vec<State>& dfa_aux_state1 = m.first;
            const edge_to_states_t& edge_to_states = m.second;

            if (dfa_aux_state1.empty()) {
                return false; // BUG
            }
#ifdef TRY_TO_REMOVE_DEAD_STATES_EARLY
            if (dfa_aux_state1.size() == 1u && dfa_aux.is_redundant_state(dfa_aux_state1.back())) {
                continue; // 移除多余的单值死状态
            }
#endif
            const State dfa_s1 = dfa_aux_calc_dfa_state(dfa_aux_state1, dfa_aux, new_state_map);
            if (invalid_state == dfa_s1) {
                return false; // 内存不足 或者 BUG
            }
            dfa_aux.m_dfa_state_set.insert(dfa_s1); // 收集 dfa 状态集

            auto found_m_dfa_move_functions_iter = dfa_aux.m_dfa_move_functions.find(dfa_s1);
            for (auto&& e2ss : edge_to_states) {
                Edge edge = e2ss.first;
                const ordered_uniqe_vec<State>& dfa_aux_states2 = e2ss.second;
#ifdef TRY_TO_REMOVE_DEAD_STATES_EARLY
                if (dfa_aux_states2.size() == 1u && dfa_aux.is_redundant_state(dfa_aux_states2.back())) {
                    continue; // 移除多余的单值死状态
                }
#endif
                const State dfa_s2 = dfa_aux_calc_dfa_state(dfa_aux_states2, dfa_aux, new_state_map);
                ordered_uniqe_vec<Edge>& edges = dfa_aux.m_state_pair_map[state_from_to_t{ dfa_s1, dfa_s2 }];
                edges.ordered_push(edge);

                dfa_aux.m_dfa_state_set.insert(dfa_s2); // 收集 dfa 状态集

                // for dfa.try_minimize()
                if (dfa_aux.m_dfa_move_functions.end() != found_m_dfa_move_functions_iter) {
                    std::unordered_map<Edge, State>& dfa_edge_to_state_map = found_m_dfa_move_functions_iter->second;
                    dfa_edge_to_state_map[edge] = dfa_s2;
                }
                else {
                    auto res = dfa_aux.m_dfa_move_functions.insert({ dfa_s1, {{ edge, dfa_s2 }} });
                    if (res.second) {
                        found_m_dfa_move_functions_iter = res.first;
                    }
                }
            }
        } // end for dfa_aux.m_move_functions
        return true;
    }
#else // <```not defined TRY_TO_REMOVE_DEAD_STATES_EARLY
    //// 此版本的 dfa_aux_convert_to_state_pair() 可以有效的移除所有的死状态，但性能略差
    bool dfa_aux_convert_to_state_pair(DFAAux& dfa_aux) const {
        // 职责：将 dfa_aux.m_move_functions 转换为 dfa_aux.m_state_pair_map 和 dfa_aux.m_dfa_move_functions，
        //       并标记 final state、 收集 dfa 状态和再次去除死状态
        std::unordered_map<ordered_uniqe_vec<State>, State, details::merged_states_hash> new_state_map;
        dfa_aux.m_dfa_state_set.clear(); // 在 dfa_aux_convert_to_state_pair_impl() 中它仅用来做破环，并不包含 next 状态集
        return dfa_aux_convert_to_state_pair_impl(dfa_aux, { dfa_aux.m_start_state }, new_state_map);
    }
#endif // end defined TRY_TO_REMOVE_DEAD_STATES_EARLY

    bool dfa_aux_convert_to_state_pair_impl(DFAAux& dfa_aux, const ordered_uniqe_vec<State>& dfa_aux_state1,
        std::unordered_map<ordered_uniqe_vec<State>, State, details::merged_states_hash>& new_state_map) const {

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
            const ordered_uniqe_vec<State>& dfa_aux_states2 = e2ss.second;

            const State dfa_s2 = dfa_aux_calc_dfa_state(dfa_aux_states2, dfa_aux, new_state_map);
            ordered_uniqe_vec<Edge>& edges = dfa_aux.m_state_pair_map[state_from_to_t{ dfa_s1, dfa_s2 }];
            edges.ordered_push(edge);

            dfa_edge_to_state_map[edge] = dfa_s2; // for dfa.try_minimize()

            if (!dfa_aux_convert_to_state_pair_impl(dfa_aux, dfa_aux_states2, new_state_map)) {
                return false;
            }
        }

        return true;
    }


    State dfa_aux_calc_dfa_state(const ordered_uniqe_vec<State>& dfa_aux_state, DFAAux& dfa_aux,
        std::unordered_map<ordered_uniqe_vec<State>, State, details::merged_states_hash>& new_state_map) const {
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
    ordered_uniqe_vec<Edge>                      m_input_edges;  // to speed up `to_dfa()`
    ordered_uniqe_vec<State>                     m_input_states;
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

#ifdef _MSC_VER
#define msvc_assert(expr) assert(expr)
#else
#define msvc_assert(expr)
#endif

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

    assert(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

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

    std::string msvc_expected_result = R"==(```mermaid
graph TD;
    0(0-S-E)  --c--> 4(4-E);
    0(0-S-E)  --a--> 1;
    4(4-E)  --a--> 1;
    1  --b--> 4(4-E);
    4(4-E)  --c--> 4(4-E);
```)==";

    msvc_assert(format_mermaid_result(mermaid_result) == format_mermaid_result(msvc_expected_result));
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

    assert(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

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

    std::string msvc_expected_result = R"==(```mermaid
graph TD;
    0(0-S-E)  --c--> 10(10-E);
    0(0-S-E)  --a--> 3;
    9  --c--> 10(10-E);
    9  --a--> 3;
    3  --b--> 9;
    10(10-E)  --a--> 3;
    10(10-E)  --c--> 10(10-E);
```)==";

    msvc_assert(format_mermaid_result(mermaid_result) == format_mermaid_result(msvc_expected_result));
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

    assert(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

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

    std::string msvc_expected_result = R"==(```mermaid
graph TD;
    0(0-S-E)  --c--> 16(16-E);
    0(0-S-E)  --a--> 14;
    16(16-E)  --a--> 14;
    15(15-E)  --a--> 14;
    14  --b--> 15(15-E);
    15(15-E)  --c--> 16(16-E);
    16(16-E)  --c--> 16(16-E);
```)==";

    msvc_assert(format_mermaid_result(mermaid_result) == format_mermaid_result(msvc_expected_result));
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

    assert(mermaid_result == expected_result);

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

    std::string msvc_expected_result = R"==(```mermaid
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

    msvc_assert(format_mermaid_result(mermaid_result) == format_mermaid_result(msvc_expected_result));

    dfa.try_minimize();
    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "minimize DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    msvc_expected_result = R"==(```mermaid
graph TD;
    0(0-S)  --[b-c]--> 0(0-S);
    0(0-S)  --a--> 4;
    4  --c--> 0(0-S);
    4  --a--> 4;
    5(5-E)  --a--> 4;
    4  --b--> 5(5-E);
    5(5-E)  --[b-c]--> 0(0-S);
```)==";

    msvc_assert(format_mermaid_result(mermaid_result) == format_mermaid_result(msvc_expected_result));

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

    assert(format_mermaid_result(mermaid_result) == format_mermaid_result(expected_result));

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

    std::string msvc_expected_result = R"==(```mermaid
graph TD;
    0(0-S)  --a--> 5;
    5  --b--> 1;
    1  --b--> 1;
    1  --d--> 3;
    3  --e--> 4(4-E);
    5  --[c-d]--> 3;
```)==";

    msvc_assert(format_mermaid_result(mermaid_result) == format_mermaid_result(msvc_expected_result));


    dfa.try_minimize();
    mermaid_result = dfa.to_mermaid("\n");
    std::cout << "minimize DFA:" << std::endl;
    std::cout << mermaid_result << std::endl;

    msvc_expected_result = R"==(```mermaid
graph TD;
    0(0-S)  --a--> 1;
    1  --b--> 1;
    1  --d--> 3;
    3  --e--> 4(4-E);
```)==";
    msvc_assert(format_mermaid_result(mermaid_result) == format_mermaid_result(msvc_expected_result));

    return 0;
}

int main()
{
    // https://www.mermaidflow.app/editor#/
    // https://mermaid.live/edit
    test_cast1();
    test_cast2();
    test_cast3();
    test_case4();
    test_cast5();
    return 0;
}

