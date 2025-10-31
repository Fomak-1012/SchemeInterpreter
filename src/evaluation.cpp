/**
 * @file evaluation.cpp
 * @brief Expression evaluation implementation for the Scheme interpreter
 * @author luke36
 * 
 * This file implements evaluation methods for all expression types in the Scheme
 * interpreter. Functions are organized according to ExprType enumeration order
 * from Def.hpp for consistency and maintainability.
 */

#include "value.hpp"
#include "expr.hpp" 
#include "RE.hpp"
#include "syntax.hpp"
#include <cstring>
#include <vector>
#include <map>
#include <climits>

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;

Value Fixnum::eval(Assoc &e) { // evaluation of a fixnum
    return IntegerV(n);
}

Value RationalNum::eval(Assoc &e) { // evaluation of a rational number
    return RationalV(numerator, denominator);
}

Value StringExpr::eval(Assoc &e) { // evaluation of a string
    return StringV(s);
}

Value True::eval(Assoc &e) { // evaluation of #t
    return BooleanV(true);
}

Value False::eval(Assoc &e) { // evaluation of #f
    return BooleanV(false);
}

Value MakeVoid::eval(Assoc &e) { // (void)
    return VoidV();
}

Value Exit::eval(Assoc &e) { // (exit)
    return TerminateV();
}

Value Unary::eval(Assoc &e) { // evaluation of single-operator primitive
    return evalRator(rand->eval(e));
}

Value Binary::eval(Assoc &e) { // evaluation of two-operators primitive
    return evalRator(rand1->eval(e), rand2->eval(e));
}

Value Variadic::eval(Assoc &e) { // evaluation of multi-operator primitive
    // TODO: TO COMPLETE THE VARIADIC CLASS
    std::vector<Value> vals;
    for(auto &r:rands)vals.push_back(r->eval(e));
    return evalRator(vals);
}

Value Var::eval(Assoc &e) { // evaluation of variable
    // TODO: TO identify the invalid variable
    // We request all valid variable just need to be a symbol,you should promise:
    //The first character of a variable name cannot be a digit or any character from the set: {.@}
    //If a string can be recognized as a number, it will be prioritized as a number. For example: 1, -1, +123, .123, +124., 1e-3
    //Variable names can overlap with primitives and reserve_words
    //Variable names can contain any non-whitespace characters except #, ', ", `, but the first character cannot be a digit
    //When a variable is not defined in the current scope, your interpreter should output RuntimeError
    
    Value matched_value = find(x, e);
    if (matched_value.get() == nullptr) {
        if (primitives.count(x)) {
             static std::map<ExprType, std::pair<Expr, std::vector<std::string>>> primitive_map = {
                    {E_VOID,     {new MakeVoid(), {}}},
                    {E_EXIT,     {new Exit(), {}}},
                    {E_BOOLQ,    {new IsBoolean(new Var("parm")), {"parm"}}},
                    {E_INTQ,     {new IsFixnum(new Var("parm")), {"parm"}}},
                    {E_NULLQ,    {new IsNull(new Var("parm")), {"parm"}}},
                    {E_PAIRQ,    {new IsPair(new Var("parm")), {"parm"}}},
                    {E_PROCQ,    {new IsProcedure(new Var("parm")), {"parm"}}},
                    {E_SYMBOLQ,  {new IsSymbol(new Var("parm")), {"parm"}}},
                    {E_STRINGQ,  {new IsString(new Var("parm")), {"parm"}}},
                    {E_DISPLAY,  {new Display(new Var("parm")), {"parm"}}},
                    {E_PLUS,     {new PlusVar({}),  {}}},
                    {E_MINUS,    {new MinusVar({}), {}}},
                    {E_MUL,      {new MultVar({}),  {}}},
                    {E_DIV,      {new DivVar({}),   {}}},
                    {E_MODULO,   {new Modulo(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
                    {E_EXPT,     {new Expt(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
                    {E_EQQ,      {new EqualVar({}), {}}},
            };

            auto it = primitive_map.find(primitives[x]);
            //TOD0:to PASS THE parameters correctly;
            //COMPLETE THE CODE WITH THE HINT IN IF SENTENCE WITH CORRECT RETURN VALUE
            if (it != primitive_map.end()) {
                //TODO
                //找到了
                return ProcedureV(it->second.second,it->second.first,e);
            }
        }
        //未定义的变量名
        throw(RuntimeError("Undefined variable"+x));
    }
    return matched_value;
}

Value Plus::evalRator(const Value &rand1, const Value &rand2) { // +
    //TODO: To complete the addition logic
    //std::cerr << "调用一次：" << __func__ << std::endl;

    if(rand1->v_type==V_INT&&rand2->v_type==V_INT){
        int p1=dynamic_cast<Integer*>(rand1.get())->n;
        int p2=dynamic_cast<Integer*>(rand2.get())->n;
        return IntegerV(p1+p2);
    }
    if(rand1->v_type==V_INT&&rand2->v_type==V_RATIONAL||
    rand2->v_type==V_INT&&rand1->v_type==V_RATIONAL|| 
    rand1->v_type==V_RATIONAL&&rand2->v_type==V_RATIONAL){
        int up1,up2,down1,down2;
        if(rand1->v_type==V_INT){
            up1=dynamic_cast<Integer*>(rand1.get())->n;
            down1=1;
        }
        if(rand2->v_type==V_INT){
            up2=dynamic_cast<Integer*>(rand2.get())->n;
            down2=1;
        }
        if(rand1->v_type==V_RATIONAL){
            Rational* v1=dynamic_cast<Rational*>(rand1.get());
            up1=v1->numerator;
            down1=v1->denominator;
        }
        if(rand2->v_type==V_RATIONAL){
            Rational* v2=dynamic_cast<Rational*>(rand2.get());
            up2=v2->numerator;
            down2=v2->denominator;
        }
        return RationalV(up1*down2+up2*down1,down1*down2);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Minus::evalRator(const Value &rand1, const Value &rand2) { // -
    //TODO: To complete the substraction logic
    //std::cerr << "调用一次：" << __func__ << std::endl;

    if(rand1->v_type==V_INT&&rand2->v_type==V_INT){
        int p1=dynamic_cast<Integer*>(rand1.get())->n;
        int p2=dynamic_cast<Integer*>(rand2.get())->n;
        return IntegerV(p1-p2);
    }
    if(rand1->v_type==V_INT&&rand2->v_type==V_RATIONAL||
    rand2->v_type==V_INT&&rand1->v_type==V_RATIONAL|| 
    rand1->v_type==V_RATIONAL&&rand2->v_type==V_RATIONAL){
        int up1,up2,down1,down2;
        if(rand1->v_type==V_INT){
            up1=dynamic_cast<Integer*>(rand1.get())->n;
            down1=1;
        }
        if(rand2->v_type==V_INT){
            up2=dynamic_cast<Integer*>(rand2.get())->n;
            down2=1;
        }
        if(rand1->v_type==V_RATIONAL){
            Rational* v1=dynamic_cast<Rational*>(rand1.get());
            up1=v1->numerator;
            down1=v1->denominator;
        }
        if(rand2->v_type==V_RATIONAL){
            Rational* v2=dynamic_cast<Rational*>(rand2.get());
            up2=v2->numerator;
            down2=v2->denominator;
        }
        return RationalV(up1*down2-up2*down1,down1*down2);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Mult::evalRator(const Value &rand1, const Value &rand2) { // *
    //TODO: To complete the Multiplication logic
    //std::cerr << "调用一次：" << __func__ << std::endl;

    if(rand1->v_type==V_INT&&rand2->v_type==V_INT){
        int p1=dynamic_cast<Integer*>(rand1.get())->n;
        int p2=dynamic_cast<Integer*>(rand2.get())->n;
        return IntegerV(p1*p2);
    }
    if(rand1->v_type==V_INT&&rand2->v_type==V_RATIONAL||
    rand2->v_type==V_INT&&rand1->v_type==V_RATIONAL|| 
    rand1->v_type==V_RATIONAL&&rand2->v_type==V_RATIONAL){
        int up1,up2,down1,down2;
        if(rand1->v_type==V_INT){
            up1=dynamic_cast<Integer*>(rand1.get())->n;
            down1=1;
        }
        if(rand2->v_type==V_INT){
            up2=dynamic_cast<Integer*>(rand2.get())->n;
            down2=1;
        }
        if(rand1->v_type==V_RATIONAL){
            Rational* v1=dynamic_cast<Rational*>(rand1.get());
            up1=v1->numerator;
            down1=v1->denominator;
        }
        if(rand2->v_type==V_RATIONAL){
            Rational* v2=dynamic_cast<Rational*>(rand2.get());
            up2=v2->numerator;
            down2=v2->denominator;
        }
        int ups=up1*up2;
        int downs=down1*down2;
        if(ups%downs==0)return IntegerV(ups/downs);
        else return RationalV(ups,downs);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Div::evalRator(const Value &rand1, const Value &rand2) { // /
    //TODO: To complete the dicision logic
    //std::cerr << "调用一次：" << __func__ << std::endl;

    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int dividend = dynamic_cast<Integer*>(rand1.get())->n;
        int divisor = dynamic_cast<Integer*>(rand2.get())->n;
        if (divisor == 0) {
            throw(RuntimeError("Division by zero"));
        }
        return IntegerV(dividend / divisor);
    }
    if(rand1->v_type==V_INT&&rand2->v_type==V_RATIONAL||
    rand2->v_type==V_INT&&rand1->v_type==V_RATIONAL|| 
    rand1->v_type==V_RATIONAL&&rand2->v_type==V_RATIONAL){
        int up1,up2,down1,down2;
        if(rand1->v_type==V_INT){
            up1=dynamic_cast<Integer*>(rand1.get())->n;
            down1=1;
        }
        if(rand2->v_type==V_INT){
            up2=dynamic_cast<Integer*>(rand2.get())->n;
            down2=1;
        }
        if(rand1->v_type==V_RATIONAL){
            Rational* v1=dynamic_cast<Rational*>(rand1.get());
            up1=v1->numerator;
            down1=v1->denominator;
        }
        if(rand2->v_type==V_RATIONAL){
            Rational* v2=dynamic_cast<Rational*>(rand2.get());
            up2=v2->numerator;
            down2=v2->denominator;
        }
        int ups=up1*down2;
        int downs=up2*down1;
        if(ups%downs==0)return IntegerV(ups/downs);
        else return RationalV(ups,downs);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Modulo::evalRator(const Value &rand1, const Value &rand2) { // modulo
    //std::cerr << "调用一次：" << __func__ << std::endl;

    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int dividend = dynamic_cast<Integer*>(rand1.get())->n;
        int divisor = dynamic_cast<Integer*>(rand2.get())->n;
        if (divisor == 0) {
            throw(RuntimeError("Division by zero"));
        }
        return IntegerV(dividend % divisor);
    }
    throw(RuntimeError("modulo is only defined for integers"));
}

Value PlusVar::evalRator(const std::vector<Value> &args) { // + with multiple args
    //TODO: To complete the addition logic
    //std::cerr << "调用一次：" << __func__ << std::endl;

    if(args.empty())throw(RuntimeError("Undefined option"));
    Value tmp=args[0];
    for(int i=1;i<args.size();i++)
        tmp=Plus(nullptr,nullptr).evalRator(tmp,args[i]);
    return tmp;
}

Value MinusVar::evalRator(const std::vector<Value> &args) { // - with multiple args
    //TODO: To complete the substraction logic
    //std::cerr << "调用一次：" << __func__ << std::endl;

    if(args.empty())throw(RuntimeError("Undefined option"));
    Value tmp=args[0];
    for(int i=1;i<args.size();i++)
        tmp=Minus(nullptr,nullptr).evalRator(tmp,args[i]);
    return tmp;
}

Value MultVar::evalRator(const std::vector<Value> &args) { // * with multiple args
    //TODO: To complete the multiplication logic
    //std::cerr << "调用一次：" << __func__ << std::endl;

    if(args.empty())throw(RuntimeError("Undefined option"));
    Value tmp=args[0];
    for(int i=1;i<args.size();i++)
        tmp=Mult(nullptr,nullptr).evalRator(tmp,args[i]);
    return tmp;
}

Value DivVar::evalRator(const std::vector<Value> &args) { // / with multiple args
    //TODO: To complete the divisor logic
    //std::cerr << "调用一次：" << __func__ << std::endl;

    if(args.empty())throw(RuntimeError("Undefined option"));
    Value tmp=args[0];
    for(int i=1;i<args.size();i++)
        tmp=Div(nullptr,nullptr).evalRator(tmp,args[i]);
    return tmp;
}


Value Expt::evalRator(const Value &rand1, const Value &rand2) { // expt
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int base = dynamic_cast<Integer*>(rand1.get())->n;
        int exponent = dynamic_cast<Integer*>(rand2.get())->n;
        
        if (exponent < 0) {
            throw(RuntimeError("Negative exponent not supported for integers"));
        }
        if (base == 0 && exponent == 0) {
            throw(RuntimeError("0^0 is undefined"));
        }
        
        long long result = 1;
        long long b = base;
        int exp = exponent;
        
        while (exp > 0) {
            if (exp % 2 == 1) {
                result *= b;
                if (result > INT_MAX || result < INT_MIN) {
                    throw(RuntimeError("Integer overflow in expt"));
                }
            }
            b *= b;
            if (b > INT_MAX || b < INT_MIN) {
                if (exp > 1) {
                    throw(RuntimeError("Integer overflow in expt"));
                }
            }
            exp /= 2;
        }
        
        return IntegerV((int)result);
    }
    throw(RuntimeError("Wrong typename"));
}

//A FUNCTION TO SIMPLIFY THE COMPARISON WITH INTEGER AND RATIONAL NUMBER
int compareNumericValues(const Value &v1, const Value &v2) {
    //std::cerr << "调用一次：" << __func__ << std::endl;

    if (v1->v_type == V_INT && v2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        return (n1 < n2) ? -1 : (n1 > n2) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        int left = r1->numerator;
        int right = n2 * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_INT && v2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = n1 * r2->denominator;
        int right = r2->numerator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = r1->numerator * r2->denominator;
        int right = r2->numerator * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    throw RuntimeError("Wrong typename in numeric comparison");
}

int mycompareNumericValues(const Value &v1, const Value &v2) {
    if (v1->v_type == V_INT && v2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        return (n1 < n2) ? -1 : (n1 > n2) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        int left = r1->numerator;
        int right = n2 * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_INT && v2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = n1 * r2->denominator;
        int right = r2->numerator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = r1->numerator * r2->denominator;
        int right = r2->numerator * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else return 2;
    // throw RuntimeError("Wrong typename in numeric comparison");
}
Value Less::evalRator(const Value &rand1, const Value &rand2) { // <
    //TODO: To complete the less logic
    int result=mycompareNumericValues(rand1,rand2);
    if(result==-1)return BooleanV(true);
    if(result==0||result==1) return BooleanV(false);
    throw(RuntimeError("Wrong typename"));
}

Value LessEq::evalRator(const Value &rand1, const Value &rand2) { // <=
    //TODO: To complete the lesseq logic
    int result=mycompareNumericValues(rand1,rand2);
    if(result==-1||result==0)return BooleanV(true);
    if(result==1) return BooleanV(false);
    throw(RuntimeError("Wrong typename"));
}

Value Equal::evalRator(const Value &rand1, const Value &rand2) { // =
    //TODO: To complete the equal logic
    int result=mycompareNumericValues(rand1,rand2);
    if(result==0)return BooleanV(true);
    if(result==1||result==-1) return BooleanV(false);
    throw(RuntimeError("Wrong typename"));
}

Value GreaterEq::evalRator(const Value &rand1, const Value &rand2) { // >=
    //TODO: To complete the greatereq logic
    int result=mycompareNumericValues(rand1,rand2);
    if(result==0||result==1)return BooleanV(true);
    if(result==-1) return BooleanV(false);
    throw(RuntimeError("Wrong typename"));
}

Value Greater::evalRator(const Value &rand1, const Value &rand2) { // >
    //TODO: To complete the greater logic
    int result=mycompareNumericValues(rand1,rand2);
    if(result==1)return BooleanV(true);
    if(result==0||result==-1) return BooleanV(false);
    throw(RuntimeError("Wrong typename"));
}

Value LessVar::evalRator(const std::vector<Value> &args) { // < with multiple args
    //TODO: To complete the less logic
    for(int i=1;i<args.size();i++){
        Value tmp=Less(nullptr,nullptr).evalRator(args[i-1],args[i]);
        if(!dynamic_cast<Boolean*>(tmp.get())->b)
            return BooleanV(false);
    }
    return BooleanV(true);
}

Value LessEqVar::evalRator(const std::vector<Value> &args) { // <= with multiple args
    //TODO: To complete the lesseq logic
    for(int i=1;i<args.size();i++){
        Value tmp=LessEq(nullptr,nullptr).evalRator(args[i-1],args[i]);
        if(!dynamic_cast<Boolean*>(tmp.get())->b)
            return BooleanV(false);
    }
    return BooleanV(true);
}

Value EqualVar::evalRator(const std::vector<Value> &args) { // = with multiple args
    //TODO: To complete the equal logic
    for(int i=1;i<args.size();i++){
        Value tmp=Equal(nullptr,nullptr).evalRator(args[i-1],args[i]);
        if(!dynamic_cast<Boolean*>(tmp.get())->b)
            return BooleanV(false);
    }
    return BooleanV(true);
}

Value GreaterEqVar::evalRator(const std::vector<Value> &args) { // >= with multiple args
    //TODO: To complete the greatereq logic
    for(int i=1;i<args.size();i++){
        Value tmp=GreaterEq(nullptr,nullptr).evalRator(args[i-1],args[i]);
        if(!dynamic_cast<Boolean*>(tmp.get())->b)
            return BooleanV(false);
    }
    return BooleanV(true);
}

Value GreaterVar::evalRator(const std::vector<Value> &args) { // > with multiple args
    //TODO: To complete the greater logic
    for(int i=1;i<args.size();i++){
        Value tmp=Greater(nullptr,nullptr).evalRator(args[i-1],args[i]);
        if(!dynamic_cast<Boolean*>(tmp.get())->b)
            return BooleanV(false);
    }
    return BooleanV(true);
}

Value Cons::evalRator(const Value &rand1, const Value &rand2) { // cons
    //TODO: To complete the cons logic
    // std::cerr<<"成功调用Cons"<<std::endl;
    return PairV(rand1,rand2);
}

Value ListFunc::evalRator(const std::vector<Value> &args) { // list function
    //TODO: To complete the list logic
    Value list=NullV();
    for(int i=args.size()-1;i>=0;i--){
        list=PairV(args[i],list);
    }
    return list;
}

Value IsList::evalRator(const Value &rand) { // list?
    //TODO: To complete the list? logic
    Value tmp=rand;
    while(tmp->v_type==V_PAIR){
        tmp=dynamic_cast<Pair*>(tmp.get())->cdr;
    }
    return BooleanV(tmp->v_type==V_NULL);
}

Value Car::evalRator(const Value &rand) { // car
    //TODO: To complete the car logic
    if(rand->v_type==V_PAIR){
        return dynamic_cast<Pair*>(rand.get())->car;
    }
    throw(RuntimeError("Wrong typename"));
}

Value Cdr::evalRator(const Value &rand) { // cdr
    //TODO: To complete the cdr logic
    if(rand->v_type==V_PAIR){
        return dynamic_cast<Pair*>(rand.get())->cdr;
    }
    throw(RuntimeError("Wrong typename"));
}

Value SetCar::evalRator(const Value &rand1, const Value &rand2) { // set-car!
    //TODO: To complete the set-car! logic
    if(rand1->v_type!=V_PAIR)throw(RuntimeError("not pair"));
    Pair *p=dynamic_cast<Pair*>(rand1.get());
    p->car=rand2;
    return VoidV();
}

Value SetCdr::evalRator(const Value &rand1, const Value &rand2) { // set-cdr!
   //TODO: To complete the set-cdr! logic
   if(rand1->v_type!=V_PAIR)throw(RuntimeError("not pair"));
    Pair *p=dynamic_cast<Pair*>(rand1.get());
    p->cdr=rand2;
    return VoidV();
}

Value IsEq::evalRator(const Value &rand1, const Value &rand2) { // eq?
    // 检查类型是否为 Integer
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) == (dynamic_cast<Integer*>(rand2.get())->n));
    }
    // 检查类型是否为 Boolean
    else if (rand1->v_type == V_BOOL && rand2->v_type == V_BOOL) {
        return BooleanV((dynamic_cast<Boolean*>(rand1.get())->b) == (dynamic_cast<Boolean*>(rand2.get())->b));
    }
    // 检查类型是否为 Symbol
    else if (rand1->v_type == V_SYM && rand2->v_type == V_SYM) {
        return BooleanV((dynamic_cast<Symbol*>(rand1.get())->s) == (dynamic_cast<Symbol*>(rand2.get())->s));
    }
    // 检查类型是否为 Null 或 Void
    else if ((rand1->v_type == V_NULL && rand2->v_type == V_NULL) ||
             (rand1->v_type == V_VOID && rand2->v_type == V_VOID)) {
        return BooleanV(true);
    } else {
        return BooleanV(rand1.get() == rand2.get());
    }
}

Value IsBoolean::evalRator(const Value &rand) { // boolean?
    return BooleanV(rand->v_type == V_BOOL);
}

Value IsFixnum::evalRator(const Value &rand) { // number?
    return BooleanV(rand->v_type == V_INT);
}

Value IsNull::evalRator(const Value &rand) { // null?
    return BooleanV(rand->v_type == V_NULL);
}

Value IsPair::evalRator(const Value &rand) { // pair?
    return BooleanV(rand->v_type == V_PAIR);
}

Value IsProcedure::evalRator(const Value &rand) { // procedure?
    return BooleanV(rand->v_type == V_PROC);
}

Value IsSymbol::evalRator(const Value &rand) { // symbol?
    return BooleanV(rand->v_type == V_SYM);
}

Value IsString::evalRator(const Value &rand) { // string?
    return BooleanV(rand->v_type == V_STRING);
}

Value Begin::eval(Assoc &e) {
    //TODO: To complete the begin logic
    Value res=VoidV();
    for(auto &it:es)
        res=it->eval(e);
    return res;
}

Value conv(const Syntax& s){
    if(auto num=dynamic_cast<Number*>(s.get())){
        return IntegerV(num->n);
    }
    else if(auto rat=dynamic_cast<RationalSyntax*>(s.get())){
        return RationalV(rat->numerator,rat->denominator);
    }
    else if(auto str=dynamic_cast<StringSyntax*>(s.get())){
        return StringV(str->s);
    }
    else if(auto sym=dynamic_cast<SymbolSyntax*>(s.get())){
        return SymbolV(sym->s);
    }
    else if(dynamic_cast<TrueSyntax*>(s.get())){
        return BooleanV(true);
    }
    else if(dynamic_cast<FalseSyntax*>(s.get())){
        return BooleanV(false);
    }
    else if(auto lis=dynamic_cast<List*>(s.get())){
        if(lis->stxs.empty())return NullV();
        Value car=conv(lis->stxs[0]);
        Value cdr=NullV();
        if(lis->stxs.size()>1){
            List* res=new List();
            for(int i=1;i<lis->stxs.size();i++){
                res->stxs.push_back(lis->stxs[i]);
            }
            cdr=conv(Syntax(res));
        }
        return PairV(car,cdr);
    }
    throw RuntimeError("Wrong quote");
}
Value Quote::eval(Assoc& e) {
    //TODO: To complete the quote logic
    return conv(s);
}

Value AndVar::eval(Assoc &e) { // and with short-circuit evaluation
    //TODO: To complete the and logic
    for(auto &it:rands){
        Value val=it->eval(e);
        if(val->v_type==V_BOOL&&!dynamic_cast<Boolean*>(val.get())->b)
            return BooleanV(false);
    }
    return BooleanV(true);
}

Value OrVar::eval(Assoc &e) { // or with short-circuit evaluation
    //TODO: To complete the or logic
    for(auto &it:rands){
        Value val=it->eval(e);
        if(val->v_type==V_BOOL&&dynamic_cast<Boolean*>(val.get())->b)
            return BooleanV(true);
    }
    return BooleanV(false);
}

Value Not::evalRator(const Value &rand) { // not
    //TODO: To complete the not logic
    if(rand->v_type==V_BOOL)
        return BooleanV(!dynamic_cast<Boolean*>(rand.get())->b);
    return BooleanV(false);//不是#f的值都是#t
}

Value If::eval(Assoc &e) {
    //TODO: To complete the if logic
    Value condi=cond->eval(e);
    bool flag= condi->v_type==V_BOOL?dynamic_cast<Boolean*>(condi.get())->b:true;
    if(flag)return conseq->eval(e);
    else return alter->eval(e);
}

Value Cond::eval(Assoc &env) {
    //TODO: To complete the cond logic
    for(auto &clause:clauses){
        if(clause.empty())continue;
        bool isElse=false;
        if(auto varx=dynamic_cast<Var*>(clause[0].get()))
            if(varx->x=="else")isElse=true;
        Value testVal=isElse?BooleanV(true):clause[0]->eval(env);
        bool condTrue=true;
        if(testVal->v_type==V_BOOL)
            condTrue=dynamic_cast<Boolean*>(testVal.get())->b;

        if(condTrue){
            Value result=VoidV();
            for(int i=1;i<clause.size();i++) {
                result=clause[i]->eval(env);
            }
            return result;
        } 
    }
    return VoidV();
}

Value Lambda::eval(Assoc &env) { 
    //TODO: To complete the lambda logic
    return ProcedureV(x,e,env);
}

Value Apply::eval(Assoc &e) {
    //std::cerr << "调用一次：" << __func__ << std::endl;
    Value proval=rator->eval(e);
    if (proval->v_type != V_PROC) {throw RuntimeError("Attempt to apply a non-procedure");}

    //TODO: TO COMPLETE THE CLOSURE LOGIC
    Procedure* clos_ptr = dynamic_cast<Procedure*>(proval.get());
    
    //TODO: TO COMPLETE THE ARGUMENT PARSER LOGIC
    std::vector<Value> args;
    for(auto it:rand)args.push_back(it->eval(e));
    if (args.size() != clos_ptr->parameters.size()) 
        throw RuntimeError("Wrong number of arguments");
    
    //TODO: TO COMPLETE THE PARAMETERS' ENVIRONMENT LOGIC
    Assoc param_env = clos_ptr->env;
    for(int i=0;i<args.size();i++)
        param_env=extend(clos_ptr->parameters[i],args[i],param_env);

    return clos_ptr->e->eval(param_env);
}


Value Define::eval(Assoc &env) {
    //TODO: To complete the define logic
    //std::cerr << "调用一次：" << __func__ << std::endl;

    Value val=e->eval(env); 
    env=extend(var,val,env);
    return VoidV();
}

Value Let::eval(Assoc &env) {
    //TODO: To complete the let logic
    Assoc newenv=env;
    for(auto &it:bind){
        Value val=it.second->eval(env);
        newenv=extend(it.first,val,newenv);
    }
    return body->eval(newenv);
}

Value Letrec::eval(Assoc &env) {
    //TODO: To complete the letrec logic
    Assoc newenv=env;
    for(auto &it:bind)
        newenv=extend(it.first,VoidV(),newenv);
    for(auto &it:bind){
        Value val=it.second->eval(env);
        modify(it.first,val,newenv);

    }
    return body->eval(newenv);
}

Value Set::eval(Assoc &env) {
    //TODO: To complete the set logic
    Value val=e->eval(env);
    modify(var,val,env);
    return VoidV();
}

Value Display::evalRator(const Value &rand) { // display function
    if (rand->v_type == V_STRING) {
        String* str_ptr = dynamic_cast<String*>(rand.get());
        std::cout << str_ptr->s;
    } else {
        rand->show(std::cout);
    }
    return VoidV();
}
