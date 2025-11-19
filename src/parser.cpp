/**
 * @file parser.cpp
 * @brief Parsing implementation for Scheme syntax tree to expression tree conversion
 * 
 * This file implements the parsing logic that converts syntax trees into
 * expression trees that can be evaluated.
 * primitive operations, and function applications.
 */

#include "RE.hpp"
#include "Def.hpp"
#include "syntax.hpp"
#include "value.hpp"
#include "expr.hpp"
#include <map>
#include <string>
#include <iostream>

#define mp make_pair
using std::string;
using std::vector;
using std::pair;

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;

/**
 * @brief Default parse method (should be overridden by subclasses)
 */
Expr Syntax::parse(Assoc &env) {
    throw RuntimeError("Unimplemented parse method");
}

Expr Number::parse(Assoc &env) {
    return Expr(new Fixnum(n));
}

Expr RationalSyntax::parse(Assoc &env) {
    //TODO: complete the rational parser
    return Expr(new RationalNum(numerator,denominator));
}

Expr SymbolSyntax::parse(Assoc &env) {
    return Expr(new Var(s));
}

Expr StringSyntax::parse(Assoc &env) {
    return Expr(new StringExpr(s));
}

Expr TrueSyntax::parse(Assoc &env) {
    return Expr(new True());
}

Expr FalseSyntax::parse(Assoc &env) {
    return Expr(new False());
}

Expr syntaxtoExpr(const Syntax &s,Assoc &env)
{
    if (auto num = dynamic_cast<Number*>(s.get())) {
        return Expr(new Fixnum(num->n));
    } else if (auto rat = dynamic_cast<RationalSyntax*>(s.get())) {
        return Expr(new RationalNum(rat->numerator, rat->denominator));
    } else if (auto str = dynamic_cast<StringSyntax*>(s.get())) {
        return Expr(new StringExpr(str->s));
    } else if (auto sym = dynamic_cast<SymbolSyntax*>(s.get())) {
        return Expr(new Var(sym->s));
    } else if (dynamic_cast<TrueSyntax*>(s.get())) {
        return Expr(new True());
    } else if (dynamic_cast<FalseSyntax*>(s.get())) {
        return Expr(new False());
    } else if (auto lis = dynamic_cast<List*>(s.get())) {
        return lis->parse(env);
    }
    throw RuntimeError("Invalid quoted syntax");
}
Expr List::parse(Assoc &env) {
    if (stxs.empty()) {
        return Expr(new Quote(Syntax(new List())));
    }

    //TODO: check if the first element is a symbol
    //If not, use Apply function to package to a closure;
    //If so, find whether it's a variable or a keyword;
    SymbolSyntax *id = dynamic_cast<SymbolSyntax*>(stxs[0].get());
    //检查是否为符号
    if (id == nullptr) {//若不是，就用Apply
        //TODO: TO COMPLETE THE LOGIC
        Expr rator=stxs[0]->parse(env);
        vector<Expr> args;
        for(int i=1;i<stxs.size();i++)
            args.push_back(stxs[i]->parse(env));
        return Expr(new Apply(rator,args));
    }else{
        string op = id->s;
        if (find(op, env).get() != nullptr) {
            //TODO: TO COMPLETE THE PARAMETER PARSER LOGIC
            Expr rator=Expr(new Var(op));
            vector<Expr>args;
            for(int i=1;i<stxs.size();i++)
                args.push_back(stxs[i]->parse(env));
            return Expr(new Apply(rator,args));
        }
        if (primitives.count(op) != 0) {
            vector<Expr> parameters;
            for(int i=1;i<stxs.size();i++)
                parameters.push_back(stxs[i]->parse(env));
            //TODO: TO COMPLETE THE PARAMETER PARSER LOGIC
            //函数名这一块
            ExprType op_type = primitives[op];
            if (op_type == E_PLUS) {
                if (parameters.size() == 2) {
                    return Expr(new Plus(parameters[0], parameters[1])); 
                } else {
                    return Expr(new PlusVar(parameters));
                    // throw RuntimeError("Wrong number of arguments for +");
                }
            } else if (op_type == E_MINUS) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() == 2) {
                    return Expr(new Minus(parameters[0], parameters[1])); 
                } else {
                    return Expr(new MinusVar(parameters));
                    // throw RuntimeError("Wrong number of arguments for -");
                }
            } else if (op_type == E_MUL) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() == 2) {
                    return Expr(new Mult(parameters[0], parameters[1])); 
                } else {
                    return Expr(new MultVar(parameters));
                    // throw RuntimeError("Wrong number of arguments for *");
                }
            }  else if (op_type == E_DIV) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() == 2) {
                    return Expr(new Div(parameters[0], parameters[1])); 
                } else {
                    return Expr(new DivVar(parameters));
                    // throw RuntimeError("Wrong number of arguments for /");
                }
            } else if (op_type == E_MODULO) {
                if (parameters.size() != 2) {
                    throw RuntimeError("Wrong number of arguments for modulo");
                }
                return Expr(new Modulo(parameters[0], parameters[1]));
            } else if (op_type == E_EXPT) {
                if (parameters.size() == 2) {
                    return Expr(new Expt(parameters[0], parameters[1]));
                } else {
                    throw RuntimeError("Wrong number of arguments for expt");
                }
            }else if (op_type == E_LIST) {
                return Expr(new ListFunc(parameters));
            } else if (op_type == E_LT) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() <= 1) throw RuntimeError("Wrong number of arguments for <");
                if (parameters.size() == 2) {
                    return Expr(new Less(parameters[0], parameters[1])); 
                } else {
                    return Expr(new LessVar(parameters));
                }
            } else if (op_type == E_LE) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() <= 1) throw RuntimeError("Wrong number of arguments for <=");
                if (parameters.size() == 2) {
                    return Expr(new LessEq(parameters[0], parameters[1])); 
                } else {
                    return Expr(new LessEqVar(parameters));
                }
            } else if (op_type == E_EQ) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() <= 1) throw RuntimeError("Wrong number of arguments for =");
                if (parameters.size() == 2) {
                    return Expr(new Equal(parameters[0], parameters[1])); 
                } else {
                    return Expr(new EqualVar(parameters));
                }
            } else if (op_type == E_GE) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() <= 1) throw RuntimeError("Wrong number of arguments for >=");
                if (parameters.size() == 2) {
                    return Expr(new GreaterEq(parameters[0], parameters[1])); 
                } else {
                    return Expr(new GreaterEqVar(parameters));
                }
            } else if (op_type == E_GT) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() <= 1) throw RuntimeError("Wrong number of arguments for >");
                if (parameters.size() == 2) {
                    return Expr(new Greater(parameters[0], parameters[1])); 
                } else {
                    return Expr(new GreaterVar(parameters));
                }
                return Expr(new GreaterVar(parameters));
            } else if (op_type == E_AND) {
                return Expr(new AndVar(parameters));
            } else if (op_type == E_OR) {
                return Expr(new OrVar(parameters));
            } else if (op_type == E_NOT) {
                if(parameters.size()!=1)
                    throw RuntimeError("Wrong number of not");
                return Expr(new Not(parameters[0]));
            }else if (op_type == E_CAR) {
                if (parameters.size() == 1) {
                    return Expr(new Car(parameters[0]));
                } else {
                    throw RuntimeError("Wrong number of arguments for car");
                }
            } else if (op_type == E_CDR) {
                if (parameters.size() == 1) {
                    return Expr(new Cdr(parameters[0]));
                } else {
                    throw RuntimeError("Wrong number of arguments for cdr");
                }
            } else if (op_type == E_CONS) {
                if (parameters.size() == 2) {
                    return Expr(new Cons(parameters[0], parameters[1]));
                } else {
                    throw RuntimeError("Wrong number of arguments for cons");
                }
        
            } else if (op_type == E_SETCAR) {
                if (parameters.size() != 2) {
                    throw RuntimeError("Wrong number of set-car!");
                }
                return Expr(new SetCar(parameters[0], parameters[1]));
            } else if (op_type == E_SETCDR) {
                if (parameters.size() != 2) {
                    throw RuntimeError("Wrong number of set-cdr!");
                }
                return Expr(new SetCdr(parameters[0], parameters[1]));
            } else if (op_type == E_LIST) {
                return Expr(new ListFunc(parameters));
            } else if (op_type == E_LISTQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of list?");
                }
                return Expr(new IsList(parameters[0]));
            } else if (op_type == E_BOOLQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of boolean?");
                }
                return Expr(new IsBoolean(parameters[0]));
            } else if (op_type == E_INTQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of integer?");
                }
                return Expr(new IsFixnum(parameters[0]));
            } else if (op_type == E_NULLQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of null?");
                }
                return Expr(new IsNull(parameters[0]));
            } else if (op_type == E_PAIRQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of pair?");
                }
                return Expr(new IsPair(parameters[0]));
            } else if (op_type == E_PROCQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of procedure?");
                }
                return Expr(new IsProcedure(parameters[0]));
            } else if (op_type == E_SYMBOLQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of symbol?");
                }
                return Expr(new IsSymbol(parameters[0]));
            } else if (op_type == E_STRINGQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of string?");
                }
                return Expr(new IsString(parameters[0]));
            } else if (op_type == E_EQQ) {
                if (parameters.size() != 2) {
                    throw RuntimeError("Wrong number of eq?");
                }
                return Expr(new IsEq(parameters[0], parameters[1]));
                
            // 输入输出
            } else if (op_type == E_DISPLAY) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of display");
                }
                return Expr(new Display(parameters[0]));
                
            // 特殊形式
            } else if (op_type == E_VOID) {
                if (!parameters.empty()) {
                    throw RuntimeError("Wrong number of void");
                }
                return Expr(new MakeVoid());
            } else if (op_type == E_EXIT) {
                if (!parameters.empty()) {
                    throw RuntimeError("Wrong number of exit");
                }
                return Expr(new Exit());
            } else {
                //TODO: TO COMPLETE THE LOGIC
                throw RuntimeError("Unknown primitives: "+op);
            } 
        }
        //预留关键字这一块
        if (reserved_words.count(op) != 0) {
            switch (reserved_words[op]) {
                //TODO: TO COMPLETE THE reserve_words PARSER LOGIC
                case E_IF:{
                    if(stxs.size()!=4){
                        throw RuntimeError("Wrong Grammar of If");
                    }
                    return Expr(new If(stxs[1]->parse(env),stxs[2]->parse(env),stxs[3]->parse(env)));
                }
                /*
                关于lambda的操作方式:
                实际上我们只传进来3个东西:lambda 参数表 函数体(运算结构)
                所以我们将参数表(一个字符串数组记作parms)传进一页新开的变量表new_env
                由于此时是变量所以我们用0为各变量赋初值
                然后看有几个表达式, 如果只有一个,直接算就行
                                  如果有多个,就用begin打包       
                */
                case E_LAMBDA: {
                    if(stxs.size() < 3) {
                        throw RuntimeError("Wrong number of arguments for lambda");
                    }
                    List *parmlist = dynamic_cast<List*>(stxs[1].get());
                    if (!parmlist) {
                        throw RuntimeError("Lambda parameters must be a list");
                    }
                    vector<string> parms;
                    for(auto &it : parmlist->stxs) {
                        SymbolSyntax *sym = dynamic_cast<SymbolSyntax*>(it.get());
                        if (!sym) {
                            throw RuntimeError("Lambda parameter must be a symbol");
                        }
                        parms.push_back(sym->s);
                    }
                    
                    // 创建新环境，将参数名添加到环境中
                    Assoc new_env = env;
                    for (const auto &param : parms) {
                        // 使用占位符值将参数名添加到环境
                        new_env = extend(param, IntegerV(0), new_env);
                    }
                    
                    // 使用新环境解析lambda体
                    if (stxs.size() > 3) {
                        vector<Expr> body_exprs;
                        for(size_t i = 2; i < stxs.size(); i++) {
                            body_exprs.push_back(stxs[i]->parse(new_env));
                        }
                        return Expr(new Lambda(parms, Expr(new Begin(std::move(body_exprs)))));
                    } else {
                        return Expr(new Lambda(parms, stxs[2]->parse(new_env)));
                    }
                }    
                
                case E_QUOTE:{
                    if(stxs.size()==2)return Expr(new Quote(stxs[1]));
                    else throw RuntimeError("Wrong number of arguments for quote");
                }
                case E_DEFINE: {
                    // (define <var> <expr>)
                    // (define <func> (lambda (<parm1> <parm2> ...) <expr1> <expr2> ...))
                    // (define (<func> <parm1> <parm2> ...) <expr1> <expr2> ...)

                    if (stxs.size() < 3) {
                        throw RuntimeError("define: too few arguments");
                    }
                    auto func_list = dynamic_cast<List*>(stxs[1].get());
                    // (define (<func> <parm1> <parm2> ...) <expr1> <expr2> ...)

                    /*
                    这个就是把lambda省略了,直接给到参数表和函数体,需要我们把它转出来
                                        见lambda操作即可
                                        只是多一步把这个lambda命个名
                    */

                    if (func_list != nullptr) {
                        if (func_list->stxs.empty()) {
                            throw RuntimeError("define: function name missing");
                        }
                        auto func_name = dynamic_cast<SymbolSyntax*>(func_list->stxs[0].get());
                        if (func_name == nullptr) {
                            throw RuntimeError("define: function name must be a symbol");
                        }
                        std::vector<std::string> params;
                        for (size_t i = 1; i < func_list->stxs.size(); ++i) {
                            auto param = dynamic_cast<SymbolSyntax*>(func_list->stxs[i].get());
                            if (param == nullptr) {
                                throw RuntimeError("define: parameter must be a symbol");
                            }
                            params.push_back(param->s);
                        }
                        Assoc env2 = env;
                        for (const auto& p : params) {
                            env2 = extend(p, Value(nullptr), env2);
                        }
                        std::vector<Expr> body_exprs;
                        for (size_t i = 2; i < stxs.size(); ++i) {
                            body_exprs.push_back(stxs[i]->parse(env2));
                        }
                        Expr lambda_body(nullptr);
                        if (body_exprs.empty()) {
                            lambda_body = Expr(new MakeVoid());
                        } else if (body_exprs.size() == 1){
                            lambda_body = body_exprs[0];
                        } else {
                            lambda_body = Expr(new Begin(body_exprs));
                        }

                        Expr lambda_expr = Expr(new Lambda(params, lambda_body));
                        
                        return Expr(new Define(func_name->s, lambda_expr));
                    } else {
                        /*
                        这个就是1.给变量赋个值
                               2.给函数命个名
                        */
                        auto var_name = dynamic_cast<SymbolSyntax*>(stxs[1].get());
                        if (var_name == nullptr) {
                            throw RuntimeError("define: variable of function name must be a symbol");
                        }
                        return Expr(new Define(var_name->s, stxs[2]->parse(env)));
                    }
                    
                }
                case E_BEGIN:{
                    vector<Expr> exprs;
                    for(int i=1;i<stxs.size();i++)
                        exprs.push_back(stxs[i]->parse(env));
                    return Expr(new Begin(exprs));
                }
                /*
                cond很简单:把每一行拆出来就行
                          主要还是看evaluation怎么处理
                */
                case E_COND:{
                    if(stxs.size()<=1)throw RuntimeError("Invalid cond clause");
                    vector<vector<Expr>> clauses;
                    for(int i = 1; i < stxs.size(); i++) {
                        List *clause = dynamic_cast<List*>(stxs[i].get());
                        if (!clause || clause->stxs.empty()) {
                            throw RuntimeError("Invalid cond clause");
                        }
                        vector<Expr> clause_exprs;
                        for(auto &expr : clause->stxs)
                            clause_exprs.push_back(expr->parse(env));                        
                        clauses.push_back(clause_exprs);
                    }
                        return Expr(new Cond(clauses));
                }
                /*
                let和letrec比之define就友好多了
                区别也不大:letrec允许使用未定义完毕的变量,而let不允许
                e.g. (let ((a (+ 1 2)) (b (+ 3 4)))
                        (* a b))
                      (let ((a (+ 1 2)) (b (+ a 4)))
                        (* a b))  

                下面是 let 和 letrec 的具体实现方式:
                let: 
                    先算所有变量的值,再绑定
                    1.在原环境中把绑定表达式转化为Expr型
                        并把变量名用占位符绑定到新环境中        
                    2.在新环境中执行各语句

                letrec:
                    先把变量绑定到环境里,再算值 
                    1.在新环境中绑定所有变量名
                    2.用新环境解析绑定表达式
                    3.在新环境中执行各语句
                */
                case E_LET:{
                    if(stxs.size() < 3) {
                        throw RuntimeError("Wrong number of arguments for let");
                    }
                    List *bindings = dynamic_cast<List*>(stxs[1].get());
                    if (!bindings) {
                        throw RuntimeError("Let bindings must be a list");
                    }
                    
                    // 创建新的环境，先复制当前环境
                    Assoc new_env = env;
                    vector<pair<string, Expr>> let_bindings;
                    
                    // 第一步：先解析所有绑定表达式（使用旧环境）
                    for(auto &binding : bindings->stxs) {
                        List *binding_pair = dynamic_cast<List*>(binding.get());
                        if (!binding_pair || binding_pair->stxs.size() != 2) {
                            throw RuntimeError("Invalid binding in let");
                        }
                        SymbolSyntax *var = dynamic_cast<SymbolSyntax*>(binding_pair->stxs[0].get());
                        if (!var) {
                            throw RuntimeError("Binding variable must be a symbol");
                        }
                        
                        // 使用当前环境解析绑定值
                        Expr value_expr = binding_pair->stxs[1]->parse(env);
                        let_bindings.push_back({var->s, value_expr});
                        
                        // 将变量名添加到新环境中（使用一个占位符值）
                        // 这样在解析let体时，这个符号就会被识别为变量而不是关键字
                        new_env = extend(var->s, IntegerV(0), new_env);
                    }
                    
                    // 第二步：使用新环境解析body
                    if (stxs.size() > 3) {
                        vector<Expr> body_exprs;
                        for(size_t i = 2; i < stxs.size(); i++) {
                            body_exprs.push_back(stxs[i]->parse(new_env));
                        }
                        return Expr(new Let(let_bindings, Expr(new Begin(body_exprs))));
                    } else {
                        return Expr(new Let(let_bindings, stxs[2]->parse(new_env)));
                    }
                }

                case E_LETREC:{
                    if(stxs.size() < 3) {
                        throw RuntimeError("Wrong number of arguments for letrec");
                    }
                    List *bindings = dynamic_cast<List*>(stxs[1].get());
                    if (!bindings) {
                        throw RuntimeError("Letrec bindings must be a list");
                    }
                    
                    // 创建新的环境，先复制当前环境
                    Assoc new_env = env;
                    vector<pair<string, Expr>> letrec_bindings;
                    
                    // 第一步：先将所有变量名添加到新环境中
                    for(auto &binding : bindings->stxs) {
                        List *binding_pair = dynamic_cast<List*>(binding.get());
                        if (!binding_pair || binding_pair->stxs.size() != 2) {
                            throw RuntimeError("Invalid binding in letrec");
                        }
                        SymbolSyntax *var = dynamic_cast<SymbolSyntax*>(binding_pair->stxs[0].get());
                        if (!var) {
                            throw RuntimeError("Binding variable must be a symbol");
                        }
                        // 先添加变量名到环境
                        new_env = extend(var->s, IntegerV(0), new_env);
                    }
                    
                    // 第二步：使用新环境解析绑定值
                    for(auto &binding : bindings->stxs) {
                        List *binding_pair = dynamic_cast<List*>(binding.get());
                        SymbolSyntax *var = dynamic_cast<SymbolSyntax*>(binding_pair->stxs[0].get());
                        Expr value_expr = binding_pair->stxs[1]->parse(new_env);
                        letrec_bindings.push_back({var->s, value_expr});
                    }
                    
                    // 第三步：使用新环境解析body
                    if (stxs.size() > 3) {
                        vector<Expr> body_exprs;
                        for(size_t i = 2; i < stxs.size(); i++) {
                            body_exprs.push_back(stxs[i]->parse(new_env));
                        }
                        return Expr(new Letrec(letrec_bindings, Expr(new Begin(body_exprs))));
                    } else {
                        return Expr(new Letrec(letrec_bindings, stxs[2]->parse(new_env)));
                    }
                }
                case E_SET:{
                    if(stxs.size() != 3) {
                            throw RuntimeError("Wrong number of arguments for set!");
                        }
                        SymbolSyntax *name = dynamic_cast<SymbolSyntax*>(stxs[1].get());
                        if (!name) {
                            throw RuntimeError("set! must be followed by a symbol");
                        }
                        return Expr(new Set(name->s, stxs[2]->parse(env)));
                }
                default:
                    throw RuntimeError("Unknown reserved word: " + op);
            }
        }
  
        //default: use Apply to be an expression
        //TODO: TO COMPLETE THE PARSER LOGIC
        Expr rator=stxs[0]->parse(env);
        vector<Expr>args;
        for(int i=1;i<stxs.size();i++)
            args.push_back(stxs[i]->parse(env));
        return Expr(new Apply(rator,args));
    }
}
