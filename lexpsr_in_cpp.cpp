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
    assert(ScanState::OK == ss && input.size() == offset);
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
        # comment 。。。d 这里是中文。。
                 23423 + 1032 = 24455 # comment
        # comment
        )";

        ctx.Reset(); // 清理前一个 ctx 的垃圾
        offset = 0;

        psr(comment) = "#"_psr - negative_set("\n")[any_cnt] - "\n"_psr;
        ctx.SetWhiteSpaces((ws | comment)[any_cnt]);

        core::ScanState res = root.ScanScript(script.data(), script.size(), offset, ctx, err);
        assert(core::ScanState::OK == res && script.size() == offset);
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
        assert(core::ScanState::OK == res && script.size() == offset);
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
    psr(values) = (leaf | node.weak())[at_least_1];
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
    psr(values) = (leaf | node.weak())[at_least_1];
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
             </!cxxx>
         </x1>
    )";

    using namespace lexpsr_shell;

    /// <summary>
    /// xml 文法 
    /// </summary>
    psr(_) = wss; // 白字符
    psr(ident) = range('0', '9')('a', 'z')('A', 'Z')('_', '_')[at_least_1];
    psr(leaf) = ident;

    decl_psr(node) = $curry([&_, &leaf, &node](const Parser& ident) {
    
        decl_psr(node_name);
        psr(head) = ("<"_psr, _, ident.slice_as_str_psr(node_name), _, ">"_psr);
    
        psr(tail) = ("<"_psr, _, "/"_psr, _,
            (node_name | fatal_if(nop, "The start and end of an xml node do not match.")),
            _, ">"_psr);
    
        psr(values) = (leaf | node.with_args(ident))[at_least_1];
        return (_, head, _, values,  _, tail, _);
    });

    psr(root) = (ignore_utf8bom, node.with_args(ident));
    
    ///////////
    std::size_t offset = 0;
    core::Context ctx;
    std::string err;
    ScanState ss = root.ScanScript(script.data(), script.size(), offset, ctx, err);
    if (ScanState::OK != ss || script.size() != offset) {
        std::cout << err << std::endl;
        std::cerr << ctx.ErrorPrompts(offset, script) << std::endl;
    }
    else {
        assert(ScanState::OK == ss && script.size() == offset);
        auto res = InvokeActions(ctx, err); 
        if (!res.first) {
            std::cerr << err << std::endl;
        }
    }

    std::cout << "-----------" << std::endl;
}

void test_xml4()
{ // 可以利用 lambda 演算系统，在 ScanScript() 阶段就能发现错误， 相对于 test_xml3，它将 “</name>” 放在一起做判定
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
    /// xml 文法 
    /// </summary>
    psr(_) = wss; // 白字符
    psr(ident) = range('0', '9')('a', 'z')('A', 'Z')('_', '_')[at_least_1];
    psr(leaf) = ident;

    decl_psr(node) = $curry([&_, &leaf, &node](const Parser& ident) {

        decl_psr(tail);
        psr(name_with_cb) = ident.with_slice_callback([tail, &_](core::StrRef slice) mutable {
            psr(name) = slice.to_std_string();
            assert(std::get_if<PreDeclPsr>(&tail.m_psr));
            tail = ("<"_psr, _, "/"_psr, _, name, _, ">"_psr);
        });

        psr(head) = ("<"_psr, _, name_with_cb, _, ">"_psr);
        psr(values) = (leaf | node.with_args(ident))[at_least_1];
        psr(assert_tail) = tail | fatal_if(nop, "The start and end of an xml node do not match.");

        return (_, head, _, values, _, assert_tail, _);
    });

    psr(root) = node.with_args(ident);

    ///////////
    std::size_t offset = 0;
    core::Context ctx;
    std::string err;
    ScanState ss = root.ScanScript(script.data(), script.size(), offset, ctx, err);
    if (ScanState::OK != ss || script.size() != offset) {
        std::cout << err << std::endl;
        std::cerr << ctx.ErrorPrompts(offset, script) << std::endl;
    }
    else {
        assert(ScanState::OK == ss && script.size() == offset);
        auto res = InvokeActions(ctx, err);
        if (!res.first) {
            std::cerr << err << std::endl;
        }
    }

    std::cout << "-----------" << std::endl;
}

void test_with_args()
{
    using namespace lexpsr_shell;
    psr(root) = $curry([](Parser p1, Parser p2, Parser p3) {
        psr(space) = " ";
        return (p1, space, p2, space, p3);
    });
    psr(p1) = "abc";
    psr(p2) = "def";
    psr(p3) = "g";

    std::size_t offset = 0;
    core::Context ctx;
    std::string err;

    // case 1
    std::string script = "abc def g";
    ctx.Reset();
    offset = 0;
    ScanState ss = root.with_args(p1, p2, p3).ScanScript(script.data(), script.size(), offset, ctx, err);
    assert(ScanState::OK == ss && script.size() == offset); (void)ss;

    // case 2
    script = "abc def g";
    ctx.Reset();
    offset = 0;
    ss = root.with_args(p1, p2).with_args(p3).ScanScript(script.data(), script.size(), offset, ctx, err);
    assert(ScanState::OK == ss && script.size() == offset); (void)ss;

    // case 3
    script = "abc def xyz";
    psr(p4) = "xyz";
    ctx.Reset();
    offset = 0;
    ss = root.with_args(p1).with_args(p2).with_args(p4).ScanScript(script.data(), script.size(), offset, ctx, err);
    assert(ScanState::OK == ss && script.size() == offset); (void)ss;

    // case 4
    script = "abc x g";
    ctx.Reset();
    offset = 0;
    ss = root.with_args(p1, p2, p3).ScanScript(script.data(), script.size(), offset, ctx, err);
    assert(ScanState::OK != ss && script.size() != offset); (void)ss;
    if (ScanState::OK != ss || script.size() != offset) {
        std::cerr << err << std::endl;
        std::cerr << ctx.ErrorPrompts(offset, script) << std::endl;
    }
    std::cout << "-----------" << std::endl;
}

void test_friendly_error()
{
    using namespace lexpsr_shell;
    core::Context ctx;
    std::size_t offset = 0;
    std::string err;
    std::string script;

    auto reset = [&ctx, &offset]() {
        ctx.Reset();
        ctx.SetWhiteSpaces(wss);
        offset = 0;
    };

    // 定义一种语言：
    // "a: ident -> ident;"
    // "b: ident -> 0;"
    // "c: 0 -> ident;"
    // "d: (ident, ..) -> 0;"
    // "e: 0 -> (ident, ..);"
    // "f: (ident, ..) -> ident;"
    // "g: ident -> (ident, ...);"
    // "h: (ident, ...) -> (ident, ...);"
    // 其中 “a:”、“b:”... 可以省略

    // [a-zA-Z_][a-zA-Z0-9_]*
    psr(ident_first) = range('a', 'z')('A', 'Z')('_', '_');
    psr(ident) = (ident_first, ident_first('0', '9')[any_cnt]);
    psr(identlist) = ("("_psr, ident, (","_psr, ident)[any_cnt], ")"_psr);

    auto together = [](const Parser& head, const Parser& body) {
        return (head, body) | body;
        // 注意这里：
        //   不能写成 : return (head[at_most_1], body);
        // 也不能写成 : return ((head | epsilon), body);
        //   因为它们都不能处理 "abc->dd;" 这样的语言：
        //     首字节 'a' 会被 head 成功吃掉，进而在第二个字节遇到'b'，但是它期望':'导致失败
    };

    psr(a) = together("a:"_psr, (ident, "->"_psr, ident));
    psr(b) = together("b:"_psr, (ident, "->"_psr, "0"_psr));
    psr(c) = together("c:"_psr, ("0"_psr, "->"_psr, ident));
    psr(d) = $curry(together).apply("d:"_psr, (identlist, "->"_psr, "0"_psr)); // 也可以用 lexpsr 语言内置的 $curry + apply
    psr(e) = together("e:"_psr, ("0"_psr, "->"_psr, identlist));
    psr(f) = together("f:"_psr, (identlist, "->"_psr, ident));
    psr(g) = together("g:"_psr, (ident, "->"_psr, identlist));
    psr(h) = together("h:"_psr, (identlist, "->"_psr, identlist));

    psr(expr) = ((a | b | c | d | e | f | g | h), ";"_psr);
    psr(root) = expr[at_least_1];

    // case 1
    {
        reset();
        script = R"(
             abc -> dfcwe;
        )";

        ScanState ss = root.ScanScript(script.data(), script.size(), offset, ctx, err);
        if (ScanState::OK != ss || script.size() != offset) {
            std::cerr << err << std::endl;
            std::cerr << ctx.ErrorPrompts(offset, script) << std::endl;
        }
        else {
            assert(script.size() == offset);
        }
    }

    // case 2
    {
        reset();
        script = R"(
             (abc, dd) -> 0;
        )";

        ScanState ss = root.ScanScript(script.data(), script.size(), offset, ctx, err);
        if (ScanState::OK != ss || script.size() != offset) {
            std::cerr << err << std::endl;
            std::cerr << ctx.ErrorPrompts(offset, script) << std::endl;
        }
        else {
            assert(script.size() == offset);
        }
    }

    // case 3
    {
        reset();

        // 数字 123 应当出错
        script = R"(
            abc -> dfcwe;
            a: abc -> dfcwe324;
            (abc, dd) -> 0;
            (abc, dd) -> (ds, 123);
            e: 0 -> (ident);
        )";

        ScanState ss = root.ScanScript(script.data(), script.size(), offset, ctx, err);
        if (ScanState::OK != ss || script.size() != offset) {
            std::cout << err << std::endl;
            std::cout << ctx.ErrorPrompts(offset, script) << std::endl;
        }
        else {
            assert(script.size() == offset);
        }
    }

    std::cout << "-----------" << std::endl;
}

void test_as_int() 
{
    /*

    field a = int;
    field b = int;
    struct a 1;
    struct b 1;

    psr c = a b;
    psr s = "{" c*(a!=0) "}";
    root = s;

    # 它这里 c*(a != 0) 表达的有以下两种歧义：
    #  1. 循环 c，直到条件 (a != 0) 不满足； 
    #  2. 循环 c 任意多次，且当条件 (a != 0) 不满足时，停止贪婪的循环。

    下面实现上述表达 1：
    */

    using namespace lexpsr_shell;

    auto ac_a = [](const ActionArgs& arg) {
        std::cout << "a:" << arg.m_action_material.m_token.to_std_string() << std::endl;
        return true;
    };

    auto ac_b = [](const ActionArgs& arg) {
        std::cout << "b:" << arg.m_action_material.m_token.to_std_string() << std::endl;
        return true;
    };

    decl_psr(loop) = $curry([loop = loop.weak(), ac_a, ac_b]() { // `loop = loop.weak()` or `&loop` 
        psr(a) = any_char <<= ac_a;
        psr(b) = any_char <<= ac_b;

        decl_psr(int_a);
        psr(c) = (a.slice_as_int_psr<int>(int_a), b);
        return (c, eq(int_a, 0) | loop.with_args());
    });


    psr(root) = ("{"_psr, loop.with_args(), "}"_psr);

    core::Context ctx;
    std::size_t offset = 0;
    std::string err;
    std::string script("{ABCDEF" "\x00" "I}", 10u);

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
    }
    std::cout << "-----------" << std::endl;
}

void test_var_loop_cnt() {
    // 
    // fn loop = (cnt) -> {
    //     if (cnt > 0) {
    //         do_something();
    //         loop(cnt - 1);
    //     }
    // }
    //
    using namespace lexpsr_shell;

    auto ac = [](const ActionArgs& arg) {
        std::cout << arg.m_action_material.m_token.to_std_string() << std::endl;
        return true;
    };

    decl_psr(loop) = $curry([&loop, ac](const Parser& cnt) -> Parser { // or `loop = loop.weak()`
        psr(a) = any_char <<= ac;
        auto minus = [](auto v1, auto v2) { 
            return v1 - v2; 
        };
        return le(cnt, 0) | (a, loop.with_args(int_op(minus, cnt, 1))); // 遗留： int_op 需要做出惰性 Lambda ?   ... // loop.with_args(cnt.plus(-1));
    });

    psr(ignore_to_end) = any_char[any_cnt];
    psr(root) = (loop.with_args(Parser(5)), ignore_to_end);

    core::Context ctx;
    std::size_t offset = 0;
    std::string err;
    std::string script("0123456789");

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
    }
    std::cout << "-----------" << std::endl;
}

namespace regex {
    enum Flag : uint32_t {
        DEFAULT     = 0,
        CASELESS    = (1 << 0), // i
        LESS_MATCH  = (1 << 2), // 非贪婪匹配
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

            CharSet& ToCastless() {
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
                            // 更新缓存？？？ 不值得在更新缓存！会使得维护复杂度上升
                            // m_single_char        = target;
                            // m_single_char_cached = true;
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
    }

    // S: Seq; B: Branch; L: Loop; C: CharSet
    static inline std::string Dump(const RegexAST& ast) {
        const RegexAST::Node* root = ast.Root();
        if (nullptr == root) { return ""; }
        return details::DumpNode(root);
    }
} // namespace regex

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

            m_ast.Reset();
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
        RegexAST::CharSet* cs = ctx.m_ast_charset_stack.back(); ctx.m_ast_charset_stack.pop_back();
        if (ctx.m_global_modifiers & (uint32_t)Flag::CASELESS) {
            cs->ToCastless();
        }
        RegexAST::Node* node = ctx.m_ast.CreateNode(RegexAST::NodeType::CharSet, tok, cs);
        ctx.m_ast_node_stack.push_back(node);
        return true;
    };

    auto ac_loop_less_opt = [](const ActionArgs& args) {
        auto&& ctx = Ctx(args.m_contex);
        std::size_t loop_cnt = args.m_action_material.m_scanner_info;
        assert(loop_cnt <= 1u);
        if (ctx.m_global_modifiers & (uint32_t)Flag::LESS_MATCH) { // 全局非贪婪匹配
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
    auto ac_seq = [](const ActionArgs& args) {
        const core::StrRef& tok = args.m_action_material.m_token;
        auto&& ctx = Ctx(args.m_contex);
        std::size_t loop_cnt = args.m_action_material.m_scanner_info;
        assert(ctx.m_ast_node_stack.size() >= loop_cnt && loop_cnt >= 1u);
        if (1u == loop_cnt) { // 最小化
            ctx.m_ast_node_stack.back()->m_context = tok;
            return true;
        }
        assert(ctx.m_ast_node_stack.size() >= loop_cnt);
        const std::size_t hold_cnt = ctx.m_ast_node_stack.size() - loop_cnt;
        RegexAST::Node* node = ctx.m_ast.CreateNode(RegexAST::NodeType::Sequence, tok);

        // add member
        node->m_member.insert(node->m_member.end(), ctx.m_ast_node_stack.begin() + hold_cnt, ctx.m_ast_node_stack.end());
        ctx.m_ast_node_stack.resize(hold_cnt);
        ctx.m_ast_node_stack.push_back(node);
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
            ctx.m_ast_node_stack.back()->m_context = tok;
            return true;
        }
    
        assert(ctx.m_ast_node_stack.size() >= branch_cnt);
        const std::size_t hold_cnt = ctx.m_ast_node_stack.size() - branch_cnt;
        RegexAST::Node* node = ctx.m_ast.CreateNode(RegexAST::NodeType::Branch, tok);

        // add member
        node->m_member.insert(node->m_member.end(), ctx.m_ast_node_stack.begin() + hold_cnt, ctx.m_ast_node_stack.end());
        ctx.m_ast_node_stack.resize(hold_cnt);
        ctx.m_ast_node_stack.push_back(node);
        return true;
    };

    /////////////////////////
    //   
    //  root          = branch;
    //  branch        = seq ('|' seq)*; # 目前不允许空串
    //  seq           = loop+;
    //  loop          = (group | literal) loop_flag_opt;  # /a/ 等价与 /a{1}/
    //  group         = '(' branch ')';
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
    //  escape_char           = hex | escape_char_one_alpha;
    //  escape_char_one_alpha = '\' set(
    //                            "tnvfr0dDsSwW"                # 字符集（缩写）
    //                            R"---(/\.^$*+?()[]{}|-)---"   # 原样输出
    //                           ); # @TODO 解耦写法 _r : 'r' ... 但没必要
    //  char_range            = char_range_boundary '-' char_range_boundary;   # 比如 [\x00-xff] 应当与 [\x00-x]|f 同构
    //  hex   = $hex();
    //  alpha = $alpha();
    //  digit = $digit();
    //
    ///////////////////////// parser ////////////////////////
    psr($digit) = range('0', '9');
    psr(digit) = $digit                                                                                        <<= ac_char;
    psr(alpha) = range('a', 'z')('A', 'Z')                                                                     <<= ac_char;
    psr(hex)   = (R"(\x)"_psr, ($digit('a', 'f')('A', 'F')[{2, 2}] | fatal_if(epsilon, "illegal hexadecimal")) <<= ac_hex);

    psr(escape_char_one_alpha) = (R"(\)"_psr, (set("tnvfr0dDsSwW" R"---(/\.^$*+?()[]{}|-)---") | fatal_if(epsilon, "unsupported escape character"))) // 多字符集 & 原样输出
                                 <<= ac_escape_char_one_alpha;

    decl_psr(char_range_boundary);

    psr(char_range)        = (char_range_boundary, "-"_psr, char_range_boundary)      <<= ac_char_range;
    psr(int_num)           = ((range('1', '9'), range('0', '9')[any_cnt]) | "0"_psr)  <<= ac_int_num;
    psr(negative_flag_opt) = "^"_psr[at_most_1]                                       <<= ac_negative_flag_opt; // ("^"_psr | epsilon) 可以省掉一个 stack @TODO
    psr(punct_char)        = set(R"---( -<>&:!"'#%,;=@_`~}])---")                     <<= ac_char;
    psr(not0x5d)           = negative_set("]")                                        <<= ac_not0x5d;
    psr(escape_char)       = hex | escape_char_one_alpha;
    char_range_boundary    = escape_char | not0x5d;

    psr(charset_content)   = (char_range | char_range_boundary)[any_cnt]              <<= ac_charset_content;
    psr(charset)           = ("["_psr, negative_flag_opt, charset_content, "]"_psr)   <<= ac_charset;

    psr(dot) = "." <<= ac_dot;
    decl_psr(fatal_nothing2repeat); // 绑定 loop_flag

    psr(literal) = (charset | escape_char | digit | alpha | dot | punct_char | fatal_nothing2repeat) <<= ac_literal;

    psr(loop_less_opt) = "?"_psr[at_most_1]                                                          <<= ac_loop_less_opt;
    psr(loop_n)        = ("{"_psr, int_num, "}"_psr)                                                 <<= ac_loop_n;
    psr(loop_mn)       = ("{"_psr, int_num, ","_psr, int_num, "}"_psr)                               <<= ac_loop_mn;
    psr(loop_m_comma)  = ("{"_psr, int_num, ",}"_psr)                                                <<= ac_loop_m_comma;
    psr(loop_comma_n)  = ("{,"_psr, int_num, "}"_psr)                                                <<= ac_loop_comma_n;

    psr(loop_star)     = "*" <<= ac_loop_star;
    psr(loop_plus)     = "+" <<= ac_loop_plus;
    psr(question_mark) = "?" <<= ac_question_mark;

    psr(loop_flag) = ((loop_star | loop_plus | question_mark | loop_n | loop_mn | loop_m_comma | loop_comma_n), loop_less_opt);

    fatal_nothing2repeat = fatal_if(loop_flag, "nothing to repeat");
    psr(loop_flag_opt) = loop_flag[at_most_1] <<= ac_loop_flag_opt;  // 这里可以实现成  loop_flag | epsilon 这样就可以省掉一个 stack @TODO

    decl_psr(group);

    psr(loop)   = ((group.weak() | literal), loop_flag_opt)                <<= ac_loop;
    psr(seq)    = loop[at_least_1]                                         <<= ac_seq;
    psr(branch) = (seq, ("|"_psr, seq)[any_cnt] <<= ac_branch_follow_up)   <<= ac_branch;

    group = ("("_psr, branch, ")"_psr);
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

    struct Case {
        const char*    regex;
        const uint32_t flag;
        const char*    result;
    };
    // S: Seq; B: Branch; L: Loop; C: CharSet
    std::vector<Case> correct_expressions = {
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
        R"===({S:[{"L 0,1":[{S:[{C:"2"},{C:"3"},{C:"0"},{C:"0x2d"},{"L 0,2000":[{C:"0x00-0xff"}]}]}]},{S:[{C:"2"},{C:"3"},{C:"0"},{C:"0x09-0x0d,0x20"}]},{"L 0,INF less":[{C:"0x00-0xff"}]},{C:"0x0d"},{C:"0x0a"}]})===" },
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
        { R"=(.*(abc))=",         0, R"===({S:[{"L 0,INF":[{C:"0x00-0xff"}]},{S:[{C:"a"},{C:"b"},{C:"c"}]}]})===" },
        { R"=(.{2,100}(abc))=",   0, R"===({S:[{"L 2,100":[{C:"0x00-0xff"}]},{S:[{C:"a"},{C:"b"},{C:"c"}]}]})===" },
        
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
    };

    for (const Case& cs : correct_expressions) {
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
    }

    // 错误的正则表达式用例
    std::vector<std::pair<std::string, uint32_t>> wrong_expression = {
        { "a**",       0 }, // nothing to repeat
        { "a{1,2}??",  0 }, // nothong to repeat
        { R"=(\x0w)=", 0 }, // 非法的十六进制 : illegal hexadecimal
        { R"=([a-z][^0-9_123]?????(230-.{,2000})?(230 ).*?\r\n)=", 0 }, // nothing to repeat
        { R"=(a{3,1})=", 0 }, // m > n : out of order
        { R"=([7-\r])=", 0 }, // m > n : out of order
    };

    for (auto&& cs : wrong_expression) {
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

} // test_regex

int main()
{
    test_core();
    test_shell0();
    test_shell1();
    test_shell_unordered();
    test_xml1();
    test_xml2();
    test_xml3();
    test_xml4();
    test_with_args();
    test_friendly_error();
    test_as_int();
    test_var_loop_cnt();
    test_regex();

    return 0;
}
