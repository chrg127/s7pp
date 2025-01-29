#include "s7.hpp"

// Define a function with arguments and a returned value, and a variable
void example2()
{
    s7::Scheme scheme;
    scheme.define_function("add1", "(add1 int) adds 1 to int", [](s7_int x) {
        return x + 1;
    });
    scheme["my-pi"] = 3.14159265;
    scheme.repl();
}

// Call a Scheme-defined function from C, and get/set Scheme variable values in C
void example3()
{
    s7::Scheme scheme;
    scheme["an-integer"] = 1;
    scheme.eval("(define (add1 a) (+ a 1))");
    printf("an-integer: %lld\n", scheme["an-integer"].as<s7_int>());
    scheme["an-integer"] = 32;
    printf("now an-integer: %lld\n", scheme["an-integer"].as<s7_int>());
    printf("(add1 2): %lld\n", scheme.to<s7_int>(scheme.call("add1", 2)));
}

int main()
{
    // example2();
    example3();
}
