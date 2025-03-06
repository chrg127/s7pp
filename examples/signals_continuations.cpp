#include "s7.hpp"

// Signal handling and continuations
#ifdef __linux__

#include <signal.h>

struct sigaction new_act, old_act;
s7::Scheme *global_scheme;

void handle_sigint(int)
{
    printf("interrupted!\n");
    global_scheme->set("*interrupt*", global_scheme->make_continuation());
    sigaction(SIGINT, &new_act, nullptr);
    s7_quit(global_scheme->ptr());
}

void example_signals_continuations()
{
    s7::Scheme scheme;
    global_scheme = &scheme;

    scheme.define_function("sleep", "(sleep) sleeps", []() -> bool { sleep(1); return false; });
    // Scheme variable *interrupt* holds the continuation at the point of the interrupt
    scheme["*interrupt*"] = false;

    sigaction(SIGINT, nullptr, &old_act);
    if (old_act.sa_handler != SIG_IGN) {
        std::memset(&new_act, 0, sizeof(new_act));
        new_act.sa_handler = &handle_sigint;
        sigaction(SIGINT, &new_act, nullptr);
    }

    scheme.repl();
}
#else

int main()
{
    // sorry, can't replicate on windows
}

#endif

