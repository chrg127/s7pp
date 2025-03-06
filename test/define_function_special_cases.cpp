#include "s7.hpp"

double add_double(double a, double b) { return a + b; }
int64_t add_int(int64_t x, int64_t y) { return x + y + 1; }

std::string print_append(std::string_view s)
{
    printf("a string! %s\n", s.data());
    return std::string(s) + " blah";
}

int64_t find(std::span<s7_int> vec, int64_t obj)
{
    for (std::size_t i = 0; i < vec.size(); i++) {
        if (vec[i] == obj) {
            return i;
        }
    }
    return -1;
}

auto add2(int x) -> int { return x + 2; }

int main()
{
    s7::Scheme scheme;

    scheme.define_function("add-double", "doc", add_double);
    scheme.define_function("add-int", "doc", add_int);
    scheme.define_function("print-append", "doc", print_append);
    scheme.define_function("find-index", "doc", find);

    auto fn = [](s7_scheme *sc, s7_pointer _args) -> s7_pointer {
        auto &scheme = *reinterpret_cast<s7::Scheme *>(&sc);
        auto args = s7::List(_args);
        auto sym = s7_symbol_table_find_name(scheme.ptr(), scheme.to<const char *>(args[0]));
        printf("%p\n", static_cast<void *>(sym));
        printf("%s\n", scheme.to_string(sym).data());
        return scheme.undefined();
    };
    scheme.define_function("test-sym", "test symbols", fn);

    scheme["testfn"] = &add2;
    scheme["testfn2"] = [&]() { return 42; };
    scheme["testfn3"] = add2;

    scheme.repl();
}

