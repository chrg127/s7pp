#include "s7.hpp"

int main()
{
    s7::Scheme scheme;
    const char *s = "test";
    scheme["a-string"] = scheme.from(s);
    scheme.repl();
}

