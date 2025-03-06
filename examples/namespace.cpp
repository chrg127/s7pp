#include "s7.hpp"

// Load C defined stuff into a separate namespace
int main(int argc, char *argv[])
{
    s7::Scheme scheme;
    /*
     * "func1" and "var1" will be placed in an anonymous environment,
     * accessible from Scheme via the global variable "lib-exports"
     */
    auto new_env = scheme.new_let(); // creates a let with curlet as parent
    /* make a private environment for func1 and var1 below (this is our "namespace") */
    scheme.protect(new_env);
    new_env.define_function("func1", "func1 adds 1 to its argument", [](s7_int x) { return x + 1; });
    new_env["var1"] = 32;
    /* those two symbols are now defined in the new environment */

    /* add "lib-exports" to the global environment */
    scheme["lib-exports"] = new_env.to_list();

    if (argc == 2) {
        fprintf(stderr, "load %s\n", argv[1]);
        if (!scheme.load(argv[1])) {
            fprintf(stderr, "can't find %s\n", argv[1]);
        }
    } else {
        scheme.repl();
    }
}

