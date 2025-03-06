#include "s7.hpp"

// Extend a built-in operator ("+" in this case)
// s7_method version
int main()
{
    s7::Scheme scheme;
    scheme.define_function("our-abs", "abs replacement", [&](s7_pointer x) {
        if (!scheme.is<s7_int>(x) && !scheme.is<double>(x)) {
            auto method = scheme.find_method(x, "abs");
            if (!method) {
                throw s7::errors::WrongType {
                    .arg = x, .arg_n = 1, .desc = "a real", .caller = "abs"
                };
            }
            return scheme.call(method.value(), x);
        } else {
            return scheme.from(fabs(scheme.to<double>(x)));
        }
    });
    scheme.repl();
}

