#include "s7.hpp"

// Call a Scheme-defined function from C, and get/set Scheme variable values in C
int main()
{
    s7::Scheme scheme;
    scheme["an-integer"] = 1;
    scheme.eval("(define (add1 a) (+ a 1))");
    printf("%s\n", std::format("an-integer: {}", scheme["an-integer"].to<s7_int>()).c_str());
    scheme["an-integer"] = 32;
    printf("%s\n", std::format("now an-integer: {}", scheme["an-integer"].to<s7_int>()).c_str());
    printf("%s\n", std::format("(add1 2): {}", scheme.to<s7_int>(scheme.call("add1", 2))).c_str());
}

