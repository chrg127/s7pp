#include <vector>
#include "s7.hpp"

struct Ps {
    std::vector<s7_pointer> ps;

    void add(s7_pointer p) { ps.push_back(p); }
};

int main()
{
    auto f = [](s7_int x) -> s7_int { return x + 1; };
    auto g = [](s7::VarArgs<s7_int> args) { return args[0]; };
    auto h = []() { return 42; };
    s7::Scheme scheme;
    scheme.make_usertype<Ps>("ps");
    printf("%s\n", scheme.to_string(scheme.make_signature(f)).data());
    printf("%s\n", scheme.to_string(scheme.make_signature(g)).data());
    printf("%s\n", scheme.to_string(scheme.make_signature(&Ps::add)).data());
    printf("%s\n", scheme.to_string(scheme.make_signature(h)).data());
    scheme.repl();
}

