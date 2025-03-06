#include "s7.hpp"

// C and Scheme hooks
int main()
{
    s7::Scheme scheme;
    auto test_hook = scheme.eval("(make-hook 'a 'b)");
    scheme.define_const("test-hook", test_hook);
    // not quite finished here
    s7_hook_set_functions(scheme.ptr(), test_hook, scheme.list(
        scheme.make_function("my-hook-function", "my hook function", [&](s7_pointer let) -> void {
            printf("a is %s\n", scheme.to_string(s7_symbol_local_value(scheme.ptr(), scheme.sym("a"), let)).data());
        }),
        s7_hook_functions(scheme.ptr(), test_hook)
    ).ptr());
    scheme.repl();
}

