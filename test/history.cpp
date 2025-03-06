#include "s7.hpp"

int main()
{
    s7::Scheme scheme;
    scheme.set_history_enabled(true);
    scheme.add_to_history(scheme.from(1));
    scheme.add_to_history(scheme.from(2));
    scheme.add_to_history(scheme.from(3));
    auto histsize = scheme.to<s7_int>(s7_starlet_ref(scheme.ptr(), scheme.sym("history-size")));
    for (auto i = histsize; i >= 0; i--) {
        printf("%s\n", scheme.to_string(scheme.history()[i]).data());
    }
}

