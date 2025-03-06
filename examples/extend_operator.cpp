#include "s7.hpp"

// Extend a built-in operator ("+" in this case)
int main()
{
    s7::Scheme scheme;
    auto old_add = scheme["+"].to<s7::Function>();
    auto old_string_append = scheme["string-append"].to<s7::Function>();
    scheme.define_function("+", "(+ ...) adds or appends its arguments",
        [&scheme, old_add, old_string_append](s7::VarArgs<s7_pointer> args) {
            return scheme.apply(scheme.is<std::string_view>(args[0]) ? old_string_append : old_add, args);
        }
    );
    scheme.repl();
}

