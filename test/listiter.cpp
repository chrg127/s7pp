#include "s7.hpp"

int main()
{
    s7::Scheme scheme;
    auto orig = scheme.list(1, 2, 3);

    for (auto p : orig) {
        printf("obj = %lld\n", scheme.to<s7_int>(p));
    }

    for (auto l = orig; !l.empty(); l.advance()) {
        auto p = l.car();
        printf("obj = %lld\n", scheme.to<s7_int>(p));
    }
}

