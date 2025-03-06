#include "s7.hpp"

int main()
{
    s7::Scheme scheme;

    scheme.define_function("add", "doc", [](s7::VarArgs<int> args) -> int {
        int sum = 0;
        for (auto arg : args) {
            sum += arg;
        }
        return sum;
    });

    scheme.define_function("add-with-init", "add but with an init value", [](s7_int init, s7::VarArgs<s7_int> args) -> s7_int {
        printf("init = %lld\n", init);
        for (auto v : args) {
            init += v;
        }
        return init;
    });

    scheme.define_function("varargs-err", "", [](s7::VarArgs<s7_int> v, s7_int x) { return v[0] + x; });

    scheme.repl();
}

