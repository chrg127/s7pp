#include "s7.hpp"

// Notification from Scheme that a given Scheme variable has been set
// (slightly modified to make use of make_function
int main()
{
    s7::Scheme scheme;
    auto f = scheme.make_function("notify-C", "called if notified-var is set!",
        [&](s7_pointer a, s7_pointer b) -> s7_pointer {
            printf("%s is set to %s\n", scheme.to_string(a).data(), scheme.to_string(b).data());
            return b;
        });
    // i am not sure if 'f' must be protected in this case
    // scheme.protect(f);
    scheme["notified-var"] = 0;
    scheme.set_setter(scheme.sym("notified-var"), f);
    scheme.repl();
}

