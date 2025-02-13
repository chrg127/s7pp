#include "s7.hpp"

#include <iostream>
#include <signal.h>

// A simple listener
void example_repl()
{
    s7::Scheme scheme;
    for (;;) {
        printf("> ");
        char buffer[512];
        fgets(buffer, sizeof(buffer), stdin);
        if (buffer[0] != '\n' || strlen(buffer) > 1) {
            scheme.eval(std::format("(write {})", buffer));
        }
        printf("\n");
    }
}

// Define a function with arguments and a returned value, and a variable
void example_c_function_variable()
{
    s7::Scheme scheme;
    scheme.define_function("add1", "(add1 int) adds 1 to int", [](s7_int x) {
        return x + 1;
    });
    scheme["my-pi"] = 3.14159265;
    scheme.repl();
}

// Call a Scheme-defined function from C, and get/set Scheme variable values in C
void example_call_get_set_vars()
{
    s7::Scheme scheme;
    scheme["an-integer"] = 1;
    scheme.eval("(define (add1 a) (+ a 1))");
    printf("%s\n", std::format("an-integer: {}", scheme["an-integer"].as<s7_int>()).c_str());
    scheme["an-integer"] = 32;
    printf("%s\n", std::format("now an-integer: {}", scheme["an-integer"].as<s7_int>()).c_str());
    printf("%s\n", std::format("(add1 2): {}", scheme.to<s7_int>(scheme.call("add1", 2))).c_str());
}

// C++ version of the repl
void example_cpp_repl()
{
    s7::Scheme scheme;
    for (;;) {
        std::string str;
        std::cout << "s7> ";
        std::getline(std::cin, str);
        std::cout << scheme.to_string(scheme.eval(str)) << "\n";
    }
}

// Add a new Scheme type and a procedure with a setter

/* define *listener-prompt* in scheme, add two accessors for C get/set */
std::string_view listener_prompt(s7::Scheme &scheme)
{
    return scheme["*listener-prompt*"].as<std::string_view>();
}

void set_listener_prompt(s7::Scheme &scheme, std::string_view new_prompt)
{
    scheme["*listener-prompt*"] = new_prompt;
}

struct dax {
    double x;
    s7_pointer data;
};

void example_listener_dax()
{
    s7::Scheme scheme;
    scheme["*listener-prompt*"] = ">";

    scheme.make_usertype<dax>("dax",
        s7::Constructors(
            [&](double x) { return dax { .x = x, .data = scheme.nil() }; },
            [&](double x, s7_pointer p) { return dax { .x = x, .data = p }; }
        ),
        s7::Op::GcMark, [&](dax &dax) -> void { scheme.mark(dax.data); },
        s7::Op::Equal,  [&](dax &d1, dax &d2) -> bool {
            return d1.x == d2.x && s7_is_equal(scheme.ptr(), d1.data, d2.data);
        },
        s7::Op::ToString, [&](dax &dax) -> std::string {
            return std::format("<dax {} {}>", dax.x, scheme.to_string(dax.data));
        }
    );
    scheme.define_property("dax-x", "dax x field",
        [](dax &d) -> double { return d.x; },
        [](dax &d, double x) -> void { d.x = x; });
    scheme.define_property("dax-data", "dax data field",
        [](dax &d) -> s7_pointer { return d.data; },
        [](dax &d, s7_pointer data) -> void { d.data = data; });

    // i can't use scheme.repl() here
    for (;;) {
        printf("%s ", scheme["*listener-prompt*"].as<const char *>());
        char buffer[512];
        fgets(buffer, sizeof(buffer), stdin);
        if (buffer[0] != '\n' || strlen(buffer) > 1) {
            printf("%s", scheme.to_string(scheme.eval(buffer)).data());
        }
        printf("\n");
    }
}

// Redirect output (and input) to a C procedure
// (currently this example can't be fully done due to missing support for ports
// that said, this example does at least show how to mix c api function with the
// c++ class)
void example_ports_redirect()
{
    s7::Scheme scheme;
    s7_set_current_output_port(scheme.ptr(),
        s7_open_output_function(scheme.ptr(), [](s7_scheme *, uint8_t c, s7_pointer /* port */) {
            fprintf(stderr, "[%c] ", c);
        })
    );
    scheme["io-port"] = s7_open_input_function(scheme.ptr(),
        [](s7_scheme *sc, s7_read_t /*peek*/, s7_pointer /*port*/) -> s7_pointer {
            auto &scheme = *reinterpret_cast<s7::Scheme *>(&sc);
            return scheme.from((char) fgetc(stdin));
        }
    );
    scheme.repl();
}

// Extend a built-in operator ("+" in this case)
void example_add_extension()
{
    s7::Scheme scheme;
    auto old_add = scheme["+"].as<s7::Function>();
    auto old_string_append = scheme["string-append"].as<s7::Function>();
    scheme.define_varargs_function("+", "(+ ...) adds or appends its arguments",
        [&scheme, old_add, old_string_append](s7::VarArgs<s7_pointer> args) {
            return scheme.apply(scheme.is<std::string>(args[0]) ? old_string_append : old_add, args);
        }
    );
    scheme.repl();
}

// s7_method version
void example_add_extension_method()
{
    s7::Scheme scheme;
    scheme.define_function("our-abs", "abs replacement", [&](s7_pointer x) {
        if (!scheme.is<s7_int>(x) && !scheme.is<double>(x)) {
            auto method = scheme.find_method(x, "abs");
            if (!method) {
                return scheme.error(s7::errors::WrongType {
                    .arg = x, .arg_n = 1, .type = "a real", .caller = "abs"
                });
            }
            return scheme.call(method.value(), x);
        } else {
            return scheme.from(fabs(scheme.to<double>(x)));
        }
    });
    scheme.repl();
}

// C-side define* (s7_define_function_star)
void example_star_function()
{
    s7::Scheme scheme;
    scheme.define_star_function("plus", "(red 32) blue", "an example of define* from C",
        [](s7_int red, s7_int blue) -> s7_int { return 2 * red + blue; });
    scheme.repl();
}

// C-side define-macro (s7_define_macro)
void example_macro()
{
    s7::Scheme scheme;
    scheme.define_macro("plus", "plus adds its two arguments.", [&](s7_int a, s7_int b) {
        return scheme.list(scheme.sym("+"), a, b);
    });
    scheme.repl();
}

// define a generic function in C
void example_generic_function()
{
    s7::Scheme scheme;
    scheme.define_varargs_function("plus",
        "(plus obj ...) applies obj's plus method to obj and any trailing arguments.",
        [&](s7::VarArgs<s7_pointer> args) {
            auto obj = args[0];
            auto method = scheme.find_method(obj, "plus");
            if (method) {
                return scheme.apply(method.value(), args);
            } else {
                return scheme.from(false);
            }
        }
    );
    scheme.repl();
}


// Signal handling and continuations
#ifdef __linux__

#include <signal.h>

struct sigaction new_act, old_act;
s7::Scheme *global_scheme;

void handle_sigint(int ignored)
{
    printf("interrupted!\n");
    global_scheme->set("*interrupt*", global_scheme->make_continuation());
    sigaction(SIGINT, &new_act, nullptr);
    s7_quit(global_scheme->ptr());
}

void example_signals_continuations()
{
    s7::Scheme scheme;
    *global_scheme = &scheme;

    scheme.define_function("sleep", "(sleep) sleeps", []() -> bool { sleep(1); return false; });
    // Scheme variable *interrupt* holds the continuation at the point of the interrupt
    scheme["*interrupt*"] = false;

    sigaction(SIGINT, nullptr, &old_act);
    if (old_add.sa_handler != SIG_IGN) {
        std::memset(&new_act, 0, sizeof(new_act));
        new_act.sa_handler = &handle_sigint;
        sigaction(SIGINT, &new_act, nullptr);
    }

    scheme.repl();
}
#endif

// Notification from Scheme that a given Scheme variable has been set
// (slightly modified to make use of make_function
void example_notification()
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

// Load C defined stuff into a separate namespace
void example_namespace(int argc, char *argv[])
{
    s7::Scheme scheme;
    /*
     * "func1" and "var1" will be placed in an anonymous environment,
     * accessible from Scheme via the global variable "lib-exports"
     */
    auto new_env = s7_sublet(scheme.ptr(), s7_curlet(scheme.ptr()), scheme.nil());
    /* make a private environment for func1 and var1 below (this is our "namespace") */
    scheme.protect(new_env);
    s7_define(scheme.ptr(), new_env, scheme.sym("func1"),
        scheme.make_function("func1", "func1 adds 1 to its argument", [](s7_int x) {
            return x + 1;
        }).p
    );
    s7_define(scheme.ptr(), new_env, scheme.sym("var1"), scheme.from(32));
    /* those two symbols are now defined in the new environment */

    /* add "lib-exports" to the global environment */
    scheme.define("lib-exports", s7_let_to_list(scheme.ptr(), new_env));

    if (argc == 2) {
        fprintf(stderr, "load %s\n", argv[1]);
        if (!s7_load(scheme.ptr(), argv[1])) {
            fprintf(stderr, "can't find %s\n", argv[1]);
        }
    } else {
        scheme.repl();
    }
}

// Handle scheme errors in C
void example_handle_errors()
{
    s7::Scheme scheme;
    bool with_error_hook = true;
    scheme.define_function("error-handler", "out error handler", [](std::string_view error) -> bool {
        printf("error: %s\n", error.data());
        return false;
    });
    if (with_error_hook) {
        scheme.eval(R"(
            (set! (hook-functions *error-hook*)
              (list (lambda (hook)
                      (error-handler
                        (apply format #f (hook 'data)))
                      (set! (hook 'result) 'our-error)))))");
    }

    for (;;) {
        printf("> ");
        char buffer[512];
        fgets(buffer, 512, stdin);
        if (buffer[0] != '\n' || strlen(buffer) > 1) {
            /* trap error messages */
            auto old_port = s7_set_current_error_port(scheme.ptr(), s7_open_output_string(scheme.ptr()));
            s7_int gc_loc = -1;
            if (old_port != scheme.nil()) {
                gc_loc = scheme.protect(old_port);
            }

            /* eval input */
            auto result = scheme.eval(buffer);

            /* print out the value wrapped in "{}" so we can tell it from other IO paths */
            printf("{%s}", scheme.to_string(result).data());

            /* look for error messages */
            auto errmsg = s7_get_output_string(scheme.ptr(), s7_current_error_port(scheme.ptr()));

            /* if we got something, wrap it in "[]" */
            if (errmsg && *errmsg) {
                printf("[%s]", errmsg);
            }

            s7_close_output_port(scheme.ptr(), s7_current_error_port(scheme.ptr()));
            if (gc_loc != -1) {
                scheme.unprotect_at(gc_loc);
            }
        }
        printf("\n");
    }
}

// C and Scheme hooks
void example_hooks()
{
    s7::Scheme scheme;
    auto test_hook = scheme.eval("(make-hook 'a 'b)");
    scheme.define_const("test-hook", test_hook);
    // not quite finished here
    s7_hook_set_functions(scheme.ptr(), test_hook, scheme.list(
        scheme.make_function("my-hook-function", "my hook function", [&](s7_pointer let) -> void {
            printf("a is %s\n", scheme.to_string(s7_symbol_local_value(scheme.ptr(), scheme.sym("a"), let)).data());
        }),
        s7_hook_functions(scheme.ptr(), test_hook)
    ).ptr());
    scheme.repl();
}

// Load a shared library
#ifdef __linux__

#include <dlfcn.h>

static void *library = nullptr;

void example_load_library()
{
    s7::Scheme scheme;
    scheme.define_function("cload", "(cload so-file-name) loads the module",
        [&](std::string_view name, std::string_view init_name) -> s7_pointer {
            library = dlopen(name.data(), RTLD_LAZY);
            if (library) {
                void *init_func = dlsym(library, init_name.data());
                if (init_func) {
                    /* call initialization function */
                    using dl_func = void (*)(Scheme *);
                    ((dl_func) init_func)(&scheme);
                }
            }
            scheme.error(s7::errors::Error {
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
                return scheme.from(dl_func(num));
            }
            return scheme.error({
                .type = "can't find function",
                .info = scheme.list("loader error: ~S", dlerror())
            });
        }
    );

    scheme.repl();
}
#endif

// Bignums in C
// (bignums not supported)

int main(int argc, char *argv[])
{
    // example_repl();
    // example_c_function_variable();
    // example_call_get_set_vars();
    // example_cpp_repl();
    // example_listener_dax();
    // example_ports_redirect();
    // example_add_extension();
    // example_add_extension_method();
    // example_star_function();
    // example_macro();
    // example_generic_function();
    // example_signals_continuations();
    // example_notification();
    // example_namespace(argc, argv);
    example_handle_errors();
    // example_hooks();
    // example_load_library
}

