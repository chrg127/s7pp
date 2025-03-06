#include "s7.hpp"

int main()
{
    s7::Scheme scheme;
    scheme.define_function("errs", "", [&](s7_int x) {
        if (x == 0) throw s7::errors::Error("generic-error", scheme.list("x = ~a\n", x));
        if (x == 1) throw s7::errors::WrongType { .arg = scheme.from(x), .arg_n = 1, .desc = "a nothing", .caller = "errs" };
        if (x == 2) throw s7::errors::OutOfRange { .arg = scheme.from(x), .arg_n = 1, .desc = "somehow", .caller = "errs" };
        if (x == 3) throw s7::errors::WrongArgsNumber { .args = scheme.from(scheme.list(1, 2, 3)), .caller = "errs" };
    });
    scheme.repl();
}

