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

Expr List::parse(Assoc &env) {
    if (stxs.empty()) {
        return Expr(new Quote(Syntax(new List())));
    }

    //TODO: check if the first element is a symbol
    //If not, use Apply function to package to a closure;
    //If so, find whether it's a variable or a keyword;
    SymbolSyntax *id = dynamic_cast<SymbolSyntax*>(stxs[0].get());
    //检查是否为关键字
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
            } else if (op_type == E_LIST) {
                return Expr(new ListFunc(parameters));
            } else if (op_type == E_LT) {
                if (parameters.size() < 2) {
                    throw RuntimeError("Wrong number of arguments for <");
                }
                return Expr(new LessVar(parameters));
            } else if (op_type == E_LE) {
                if (parameters.size() < 2) {
                    throw RuntimeError("Wrong number of arguments for <=");
                }
                return Expr(new LessEqVar(parameters));
            } else if (op_type == E_EQ) {
                if (parameters.size() < 2) {
                    throw RuntimeError("Wrong number of arguments for =");
                }
                return Expr(new EqualVar(parameters));
            } else if (op_type == E_GE) {
                if (parameters.size() < 2) {
                    throw RuntimeError("Wrong number of arguments for >=");
                }
                return Expr(new GreaterEqVar(parameters));
            } else if (op_type == E_GT) {
                if (parameters.size() < 2) {
                    throw RuntimeError("Wrong number of arguments for >");
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
            } else if (op_type == E_CONS){
                if(parameters.size()!=2)
                    throw RuntimeError("Wrong grammar of cons");
                return Expr(new Cons(parameters[0],parameters[1]));
            } else if (op_type == E_CAR){
                if(parameters.size()!=1)
                    throw RuntimeError("Wrong number of car");
                return Expr(new Car(parameters[0]));
            } else if (op_type == E_CDR){
                if(parameters.size()!=1)
                    throw RuntimeError("Wrong number of cdr");
                return Expr(new Cdr(parameters[0]));
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
                // throw RuntimeError("没得这个函数,你再看看呢: "+op);
            } 
        }
        //预留关键字这一块
        if (reserved_words.count(op) != 0) {
            switch (reserved_words[op]) {
                //TODO: TO COMPLETE THE reserve_words PARSER LOGIC
                case E_IF:{
                    Value flag=find("if",env);
                    if(flag.get()!=nullptr){
                        vector<Expr> args;
                        for(int i=1;i<stxs.size();i++)args.push_back(stxs[i]->parse(env));
                        return Expr(new Apply(new Var("if"),args));
                    }
                    if(stxs.size()==4)Expr(new If(stxs[1]->parse(env),stxs[2]->parse(env),stxs[3]->parse(env)));
                }
                // case E_LAMBDA:{
                //     List *parmlist=dynamic_cast<List*>(stxs[1].get());
                //     vector<string>parms;
                //     for(auto &it:parmlist->stxs){
                //         SymbolSyntax *sym=dynamic_cast<SymbolSyntax*>(it.get());
                //         //注意这里可能有报错
                //         parms.push_back(sym->s);
                //     }
                //     if(stxs.size()>3){
                //         vector<Expr> body;
                //         for(int i=2;i<stxs.size();i++)
                //             body.push_back(stxs[i]->parse(env));
                //         return Expr(new Lambda(parms,Expr(new Begin(body))));
                //     }
                //     return Expr(new Lambda(parms,stxs[2]->parse(env)));
                // }    

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
                    Value flag=find("quote",env);
                    if(flag.get()!=nullptr){
                        vector<Expr> args;
                        for(int i=1;i<stxs.size();i++)args.push_back(stxs[i]->parse(env));
                        return Expr(new Apply(new Var("quote"),args));
                    }
                    if(stxs.size()==2)return Expr(new Quote(stxs[1]));
                    else throw RuntimeError("Wrong number of arguments for quote");
                }
                case E_DEFINE:{
                    if (auto name = dynamic_cast<SymbolSyntax*>(stxs[1].get())) {
                        return Expr(new Define(name->s, stxs[2]->parse(env)));
                    } else if (auto fnlist = dynamic_cast<List*>(stxs[1].get())) {
                        SymbolSyntax *fname = dynamic_cast<SymbolSyntax*>(fnlist->stxs[0].get());
                        vector<string> params;
                        for (size_t i = 1; i < fnlist->stxs.size(); ++i) {
                            SymbolSyntax *sym = dynamic_cast<SymbolSyntax*>(fnlist->stxs[i].get());
                            params.push_back(sym->s);
                        }
                        Expr body = stxs[2]->parse(env);
                        Expr lambda = Expr(new Lambda(params, body));
                        return Expr(new Define(fname->s, lambda));
                    } else {
                        throw RuntimeError("Invalid define syntax");
                    }
                }
                case E_BEGIN:{
                    vector<Expr> exprs;
                    for(int i=1;i<stxs.size();i++)
                        exprs.push_back(stxs[i]->parse(env));
                    return Expr(new Begin(exprs));
                }
                case E_COND:{
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
