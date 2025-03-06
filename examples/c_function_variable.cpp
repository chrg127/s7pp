#include "s7.hpp"

// Define a function with arguments and a returned value, and a variable
int main()
{
    s7::Scheme scheme;
    scheme.define_function("add1", "(add1 int) adds 1 to int", [](s7_int x) {
        return x + 1;
    });
    scheme["my-pi"] = 3.14159265;
    scheme.repl();
}

