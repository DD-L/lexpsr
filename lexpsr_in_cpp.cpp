#include "lexpsr.h"

int test_core()
{
    using namespace lexpsr_core;

    Seq s;
    Branch b;
    CharBranch cb;
    Loop loop;
    Token t("");
    LogicNotScanner lns(b);
    FatalIf fi(b, "err");
    ActionScanner as(s, [](const ActionMaterial&, Context&, std::string&) { return true; });
    PreparedScanner ps;
    ps.Reshape(cb);

    Seq seq{ MakeDebugger(b, []() {}), "abc"_t };

    // 1 + 1 = 2
    // root : num '+' num '=' num ;
    // num  : /[1-9][0-9]*|0/
    // [1-9] : CharSet("123456789")
    // [0-9] : CharSet("0123456789")

    auto _1_9 = CharBranch("123456789");
    auto _0_9 = CharBranch("0123456789");
    auto _0_9_loop = Loop(_0_9, 0, Loop::INF_CNT);
    auto _not_0 = Seq{ _1_9, _0_9_loop };
    auto num = Branch{ _not_0, "0"_t };
    auto ws = CharBranch("\t \r\n\f\v");
    auto wss = Loop(ws, 0, Loop::INF_CNT);
    auto root = Seq{ wss, num,  wss, "+"_t, wss, num, wss, "="_t, wss, num , wss };

    std::size_t offset = 0;
    Context ctx;
    std::string err;

    std::string input = "2 + 4 = 7";
    ScanState ss = root(input.data(), input.size(), offset, ctx, err);
    assert(ScanState::OK == ss);
    (void)ss;

    return 0;
}

void test_shell0()
{
    using namespace lexpsr_shell;

    //psr(a);

    psr(b); psr(c);
    psr(d); psr(e); psr(f);
    psr(g); psr(h); psr(i);
    psr(j); psr(k); psr(l);
    psr(m); psr(n); psr(o);

    psr(a) = "abc";
    b = "bbb" <<= [](const ActionArgs&) -> bool { return true; };
    c = "ccc";
    d = "eeee";
    f = "ffff";
    g = (a - b - c | d - "xxxde"_psr | f)[{2, 3}];  // - 的优先级要比 | 高


    h = f[{ 2, 4 }];
    i = set("qazwsxwdc") <<= [](const ActionArgs&) -> bool { return true; };
    j = range('0', '9')('a', 'z')('A', 'Z')[{2, 2}];

    k = _not(h) | a;           // 逻辑非
    l = fatal_if(j, "errMsg"); // 致命错误
    m = $([](const ScanArgs&) {  return ScanState::OK; })[{2, 4}];    // 自由函数
    n = "xxx"_psr[any_cnt] | "xxxx"_psr[loop_cnt(3)] | "xx"_psr[at_least_1] | "eee"_psr[at_least(5)] | "sddd"_psr[at_most_1] | "ewrwe"_psr[at_most(5u)] | c[loop_cnt(3, 6)][{2, 3}];

    psr(abc) = "343434";

    auto fn = [&abc](Parser a, Parser b)
    {
        psr(ret) = a | b | abc;
        return (ret, abc)[at_most_1];
    };

    psr(fn_app) = fn(m, "sdfs"_psr);                // 函数应用


    psr(id_lambda) = $curry([](Parser a) { return a; }); // 高阶函数： Parser -> Parser
    psr(lambda_apply) = id_lambda.apply("xxxx"_psr);

    psr(xxxx0) = $curry([]() {return "wewe"_psr; });
    xxxx0.apply();
    psr(xxxx1) = $curry([](Parser a) {return a; });
    psr(xxxx2) = $curry([](Parser a, Parser b) {return a - b; });
    psr(xxxx3) = $curry([](Parser a, Parser b, Parser c) {return (a, b) | c; });
    psr(xxxx4) = $curry([](Parser a, Parser b, Parser c, Parser d) {return d | ((a, b) | c)[{2, 2}]; });
    psr(xxxx4_app) = xxxx4.apply(""_psr).apply("werw"_psr).apply("ewfwe"_psr).apply("wewfwef"_psr);
}

//void err()
//{
//    using namespace lexpsr_shell;
//
//    psr(a); psr(b); psr(c);
//    auto ac = [](const core::ActionMaterial&, core::Context&, std::string&) { return true; };
//    a = "abc" <<= ac <<= ac;
//}

#include <iostream>

void test_shell1()
{
    using namespace lexpsr_shell;
    // 1 + 1 = 2
    // root : num '+' num '=' num ;
    // num  : /[1-9][0-9]*|0/
    // [1-9] : CharSet("123456789")
    // [0-9] : CharSet("0123456789")

    // [1-9][0-9]*
    psr(num) = (range('1', '9') - range('0', '9')[any_cnt]) | "0"_psr
        <<=
        [](const ActionArgs& args)
    {
        std::cout << args.m_action_material.m_token.to_std_string() << std::endl;
        return true;
    };

    psr(root) = (num, "+"_psr, num, "="_psr, num);

    std::size_t offset = 0;
    core::Context ctx;
    std::string err;

    {// eat wss case:
        const std::string script = R"(

         23423 + 1032 = 24455

        )";

        ctx.SetWhiteSpaces(wss); // or `ctx.SetWhiteSpaces(Parser::EatWss);`
        core::ScanState res = root.ScanScript(script.data(), script.size(), offset, ctx, err);
        assert(core::ScanState::OK == res);
        std::pair<bool, std::size_t> ac_res = InvokeActions(ctx, err);
        assert(ac_res.first);
    }

    { // eat wss and comment case:
        const std::string script = R"(
        #werwerw
        # comment 。d
                 23423 + 1032 = 24455 # comment
        # comment
        )";

        // 中文 BUG
        //const std::string script = R"(
        //#werwerw
        //# comment 。。。d
        //         23423 + 1032 = 24455 # comment
        //# comment
        //)";

        psr(comment) = "#"_psr - negative_set("\n")[any_cnt] - "\n"_psr;
        ctx.SetWhiteSpaces((ws | comment)[any_cnt]);

        ctx.ResetLazyAction(); // 清理前一个 ctx 的事件垃圾
        core::ScanState res = root.ScanScript(script.data(), script.size(), offset, ctx, err);
        assert(core::ScanState::OK == res);
        std::pair<bool, std::size_t> ac_res = InvokeActions(ctx, err);
        assert(ac_res.first);
    }
}

void test_shell_unordered()
{
    using namespace lexpsr_shell;
    decl_psr(num);

    psr(root) = (num, "+"_psr, num, "="_psr, num);
    num = (range('1', '9'), range('0', '9')[any_cnt]) | "0"_psr;

    num <<=
        [](const ActionArgs& args)
    {
        std::cout << args.m_action_material.m_token.to_std_string() << std::endl;
        return true;
    };

    std::size_t offset = 0;
    core::Context ctx;
    std::string err;

    {// eat wss case:
        const std::string script = R"(

         23423 + 1032 = 24455

        )";

        ctx.SetWhiteSpaces(wss); // or `ctx.SetWhiteSpaces(Parser::EatWss);`
        core::ScanState res = root.ScanScript(script.data(), script.size(), offset, ctx, err);
        assert(core::ScanState::OK == res);
        std::pair<bool, std::size_t> ac_res = InvokeActions(ctx, err);
        assert(ac_res.first);
    }
}

void test_xml()
{
}

int main()
{
    test_core();
    test_shell0();
    test_shell1();
    test_shell_unordered();
    test_xml();
    return 0;
}
