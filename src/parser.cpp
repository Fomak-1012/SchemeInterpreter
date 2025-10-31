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
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() == 2) {
                    return Expr(new Less(parameters[0], parameters[1])); 
                } else {
                    return Expr(new LessVar(parameters));
                    // throw RuntimeError("Wrong number of arguments for <");
                }
            } else if (op_type == E_LE) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() == 2) {
                    return Expr(new LessEq(parameters[0], parameters[1])); 
                } else {
                    return Expr(new LessEqVar(parameters));
                    // throw RuntimeError("Wrong number of arguments for <=");
                }
            } else if (op_type == E_EQ) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() == 2) {
                    return Expr(new Equal(parameters[0], parameters[1])); 
                } else {
                    return Expr(new EqualVar(parameters));
                    // throw RuntimeError("Wrong number of arguments for =");
                }
            } else if (op_type == E_GE) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() == 2) {
                    return Expr(new GreaterEq(parameters[0], parameters[1])); 
                } else {
                    return Expr(new GreaterEqVar(parameters));
                    // throw RuntimeError("Wrong number of arguments for >=");
                }
            } else if (op_type == E_GT) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() == 2) {
                    return Expr(new Greater(parameters[0], parameters[1])); 
                } else {
                    return Expr(new GreaterVar(parameters));
                    // throw RuntimeError("Wrong number of arguments for >");
                }
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
                    if(stxs.size()!=4){
                        // throw RuntimeError("你看看你的if语法写对了吗");
                        throw RuntimeError("Wrong Grammar of If");
                    }
                    return Expr(new If(stxs[1]->parse(env),stxs[2]->parse(env),stxs[3]->parse(env)));
                }
                case E_LAMBDA:{
                    List *parmlist=dynamic_cast<List*>(stxs[1].get());
                    vector<string>parms;
                    for(auto &it:parmlist->stxs){
                        SymbolSyntax *sym=dynamic_cast<SymbolSyntax*>(it.get());
                        //注意这里可能有报错
                        parms.push_back(sym->s);
                    }
                    return Expr(new Lambda(parms,stxs[2]->parse(env)));
                }    
                case E_QUOTE:{
                    if(stxs.size()!=2){
                        throw RuntimeError("Wrong number of Quote");
                    }
                    return Expr(new Quote(stxs[1]));
                }
                case E_DEFINE:{
                    SymbolSyntax *name=dynamic_cast<SymbolSyntax*>(stxs[1].get());
                    return Expr(new Define(name->s,stxs[2]->parse(env)));
                }
                case E_BEGIN:{
                    vector<Expr> exprs;
                    for(int i=1;i<stxs.size();i++)
                        exprs.push_back(stxs[i]->parse(env));
                    return Expr(new Begin(exprs));
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
