﻿#include "eNFA2DFA.h"

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

using namespace finite_automata;

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

