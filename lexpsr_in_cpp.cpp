#define LEXPSR_SHORT_KEYWORDS
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

    k = next_not(h) | a;           // 逻辑非
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
    psr(xxxx4_app2) = xxxx4.apply(""_psr, "werw"_psr).apply("ewfwe"_psr, "wewfwef"_psr);
    psr(xxxx4_app3) = xxxx4.apply(""_psr, "werw"_psr, "ewfwe"_psr, "wewfwef"_psr);
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

void test_xml1()
{ //  忽略 xml 开始与结尾 标记的比对逻辑， 也就是说 <a>xxx</b> ，其中 a != b 将是合法的。
    const std::string script = R"(
         <x1>
             <a>a_v</a>
             <b> b_v < /b>
             <c>
                <c1> 
                    <d1> d1_v </d1>
                    <d2> d2_v </d2>
                </c1>
                <c2> c2_v </c2>
             </c>
         </x1>
    )";



    //field node_begin = string;
    //field node_end   = string;
    //field leaf       = string;
    //
    //groupfield node;
    //
    //declare node;
    //psr wss   = regex([\x0d\x0a\x20\x09\x0c\x0b]*); # 白字符
    //psr ident = regex([a-zA-Z0-9_]+); # 根据实际需要修改 ident
    //psr node_begin = ident nop;
    //psr node_end   = ident nop;
    //psr leaf   = ident nop;
    //psr value  = leaf | node;
    //psr values = value +;
    //psr node   = wss "<" wss node_begin wss ">" wss values wss "<" wss "/" wss node_end ">" wss; 
    //
    //root = node;     


    using namespace lexpsr_shell;

    struct Ctx : core::Context {
        std::size_t depth = 0;
        std::string Spaces() const {
            return std::string(depth * 2, ' ');
        }
    };

    auto node_begin_ac = [](const ActionArgs& args) {
        ((Ctx&)args.m_contex).depth++;
        std::cout << ((Ctx&)args.m_contex).Spaces() << "<" << args.m_action_material.m_token.to_std_string() << ">" << std::endl;
        return true;
    };
    auto node_end_ac = [](const ActionArgs& args) {
        std::cout << ((Ctx&)args.m_contex).Spaces() << "</" << args.m_action_material.m_token.to_std_string() << ">" << std::endl;
        ((Ctx&)args.m_contex).depth--;
        return true;
    };
    auto leaf_ac = [](const ActionArgs& args) {
        std::cout << ((Ctx&)args.m_contex).Spaces() << "  " << args.m_action_material.m_token.to_std_string() << std::endl;
        return true;
    };

    /// <summary>
    /// xml 文法
    /// </summary>
    decl_psr(node);

    psr(_) = wss; // 白字符
    psr(ident) = range('0', '9')('a', 'z')('A', 'Z')('_', '_')[at_least_1];
    psr(node_begin) = ident <<= node_begin_ac;
    psr(node_end) = ident <<= node_end_ac;
    psr(leaf) = ident <<= leaf_ac;
    psr(values) = (leaf | node)[at_least_1];
    node = (_, "<"_psr, _, node_begin, _, ">"_psr, _, values, _, "<"_psr, _, "/"_psr, _, node_end, _, ">"_psr, _);

    /////////////////////
    std::string err;
    std::size_t offset = 0;
    Ctx ctx;
    ScanState ss = node.ScanScript(script.data(), script.size(), offset, ctx, err);
    assert(ScanState::OK == ss && script.size() == offset);
    auto res = InvokeActions(ctx, err);
    assert(res.first);

    std::cout << "-----------" << std::endl;
}

void test_xml2()
{ // 将 xml 开始与结尾 标记的比对逻辑，放在自动机的栈上（在 InvokeActions() 中发现错误）
    const std::string script = R"(
         <x1>
             <a>a_v</a>
             <b> b_v < /b>
             <c>
                <c1> 
                    <d1> d1_v </d1>
                    <d2> d2_v </d2>
                </c1>
                <c2> c2_v </c2>
             </cxxx>
         </x1>
    )";

    using namespace lexpsr_shell;

    struct Ctx : core::Context {
        std::vector<std::string> m_node_names;
        std::string Spaces() const {
            return std::string(m_node_names.size() * 2, ' ');
        }
    };

    auto node_begin_ac = [](const ActionArgs& args) {
        const std::string begin = args.m_action_material.m_token.to_std_string();
        Ctx& ctx = (Ctx&)args.m_contex;
        ctx.m_node_names.push_back(begin);
        std::cout << ctx.Spaces() << "<" << begin << ">" << std::endl;
        return true;
    };
    auto node_end_ac = [](const ActionArgs& args) {
        const std::string end = args.m_action_material.m_token.to_std_string();
        Ctx& ctx = (Ctx&)args.m_contex;
        const std::string begin = ctx.m_node_names.empty() ? "" : ctx.m_node_names.back();
        if (begin != end) {  // <--- 注意这里, 可以判断 begin 与 end 是否匹配
            args.m_error_message = "The start and end of an xml node do not match. <" + begin + "> != </" + end + ">";
            return false;
        }
        std::cout << ctx.Spaces() << "</" << end << ">" << std::endl;
        ((Ctx&)args.m_contex).m_node_names.pop_back();
        return true;
    };
    auto leaf_ac = [](const ActionArgs& args) {
        std::cout << ((Ctx&)args.m_contex).Spaces() << "  " << args.m_action_material.m_token.to_std_string() << std::endl;
        return true;
    };

    /// <summary>
    /// xml 文法
    /// </summary>
    decl_psr(node);

    psr(_) = wss; // 白字符
    psr(ident) = range('0', '9')('a', 'z')('A', 'Z')('_', '_')[at_least_1];
    psr(node_begin) = ident <<= node_begin_ac;
    psr(node_end) = ident <<= node_end_ac;
    psr(leaf) = ident <<= leaf_ac;
    psr(values) = (leaf | node)[at_least_1];
    node = (_, "<"_psr, _, node_begin, _, ">"_psr, _, values, _, "<"_psr, _, "/"_psr, _, node_end, _, ">"_psr, _);

    /////////////////////
    std::string err;
    std::size_t offset = 0;
    Ctx ctx;
    ScanState ss = node.ScanScript(script.data(), script.size(), offset, ctx, err);
    assert(ScanState::OK == ss && script.size() == offset);
    auto res = InvokeActions(ctx, err); // 在这个过程中发现 begin - end 不匹配的错误
    if (!res.first) {
        std::cerr << err << std::endl;
    }
    std::cout << "-----------" << std::endl;
}


void test_xml3()
{ // 可以利用 lambda 演算系统，在 ScanScript() 阶段就能发现错误
    const std::string script = R"(
         <x1>
             <a>a_v</a>
             <b> b_v < /b>
             <c>
                <c1> 
                    <d1> d1_v </d1>
                    <d2> d2_v </d2>
                </c1>
                <c2> c2_v </c2>
             </cxxx>
         </x1>
    )";

    using namespace lexpsr_shell;

    /// <summary>
    /// xml 文法  !!!!!!!!!!!!! 未完成 ！！！！！！！！！！！！！！！！！
    /// </summary>
    psr(_) = wss; // 白字符
    psr(ident) = range('0', '9')('a', 'z')('A', 'Z')('_', '_')[at_least_1];
    psr(leaf) = ident;

    decl_psr(node) = $curry([&_, &leaf, &node](Parser ident) -> Parser {
        psr(values) = (leaf | node.apply(ident))[at_least_1];
        psr(node_name);
        return (
            _, "<"_psr, _, 
            ident // .as(node_name)   // <---- 这里未完成
            , _, ">"_psr, _, values, _, "<"_psr, _, "/"_psr, _, 
            (node_name | fatal_if(nop, "The start and end of an xml node do not match.")),
            _, ">"_psr, _
        );
    });

    
    ///////////
    std::size_t offset = 0;
    core::Context ctx;
    std::string err;
    ScanState ss = node.apply(ident).ScanScript(script.data(), script.size(), offset, ctx, err);
    if (ScanState::OK != ss) {
        std::cout << err << std::endl;
    }
    assert(ScanState::OK == ss && script.size() == offset);
    std::cout << "-----------" << std::endl;
}

int main()
{
    test_core();
    test_shell0();
    test_shell1();
    test_shell_unordered();
    test_xml1();
    test_xml2();
    //test_xml3(); // 未完成，暂时注释掉

    //auto node = []<class V, class N>(V value, N next) {
    //    return [=](bool which) {
    //        return which ? std::variant<V, N>(value) : std::variant<V, N>(next);
    //    };
    //};
    //
    //auto lst = node(1, node(2, node(3, nullptr)));
    //
    // auto value = std::get<0>(lst(true));
    // std::cout << value << std::endl;
    // auto next_node = std::get<1>(lst(false));
    // auto next_value = std::get<0>(next_node(true));
    // std::cout << next_value << std::endl;

    return 0;
}
