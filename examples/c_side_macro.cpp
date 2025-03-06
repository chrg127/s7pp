#include "s7.hpp"

// C-side define-macro (s7_define_macro)
int main()
{
    s7::Scheme scheme;
    scheme.define_macro("plus", "plus adds its two arguments.", [&](s7_int a, s7_int b) {
        return scheme.list(scheme.sym("+"), a, b);
    });
    scheme.repl();
}

