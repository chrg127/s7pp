#include "s7.hpp"

// C-side define* (s7_define_function_star)
int main()
{
    s7::Scheme scheme;
    scheme.define_star_function("plus", "(red 32) blue", "an example of define* from C",
        [](s7_int red, s7_int blue) -> s7_int { return 2 * red + blue; });
    scheme.repl();
}

