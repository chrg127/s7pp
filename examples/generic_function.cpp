#include "s7.hpp"

// define a generic function in C
int main()
{
    s7::Scheme scheme;
    scheme.define_function("plus",
        "(plus obj ...) applies obj's plus method to obj and any trailing arguments.",
        [&](s7::VarArgs<s7_pointer> args) {
            auto obj = args[0];
            auto method = scheme.find_method(obj, "plus");
            if (method) {
                return scheme.apply(method.value(), args);
            } else {
                return scheme.from(false);
            }
        }
    );
    scheme.repl();
}

