#include "s7.hpp"

// Load a shared library
#ifdef __linux__

#include <dlfcn.h>

static void *library = nullptr;

int main()
{
    s7::Scheme scheme;
    scheme.define_function("cload", "(cload so-file-name) loads the module",
        [&](std::string_view name, std::string_view init_name) -> s7_pointer {
            library = dlopen(name.data(), RTLD_LAZY);
            if (library) {
                void *init_func = dlsym(library, init_name.data());
                if (init_func) {
                    /* call initialization function */
                    using dl_func = void (*)(s7::Scheme *);
                    ((dl_func) init_func)(&scheme);
                    return scheme.from(true);
                }
            }
            return scheme.error(s7::errors::Error {
                .type = "load-error",
                .info = scheme.list("loader error: ~S", dlerror())
            });
        }
    );

    scheme.define_function("try", "(try name num) tries to call name in the shared library with the argument num,",
        [&](std::string_view name, double num) -> s7_pointer {
            void *func = dlsym(library, name.data());
            if (func) {
                /* we'll assume double f(double) */
                using dl_func = double (*)(double);
                return scheme.from(((dl_func)func)(num));
            }
            return scheme.error(s7::errors::Error {
                .type = "can't find function",
                .info = scheme.list("loader error: ~S", dlerror())
            });
        }
    );

    scheme.repl();
}

#else

int main()
{
    // sorry, can't replicate on windows
}

#endif

