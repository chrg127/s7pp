#include "s7.hpp"

int main()
{
    s7::Scheme scheme;
    scheme.define_function("z+", "add 1 to complex", [](s7_complex z) { return s7_complex(z.real() + 1, z.imag()); });
    scheme.repl();
}


