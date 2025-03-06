#include "s7.hpp"

int main()
{
    s7::Scheme scheme;
    scheme.define_star_function("add", "a (b 1)", "doc", [](int a, int b) {
        return a + b;
    });
    scheme.repl();
}

