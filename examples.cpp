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
    printf("an-integer: %lld\n", scheme["an-integer"].as<s7_int>());
    scheme["an-integer"] = 32;
    printf("now an-integer: %lld\n", scheme["an-integer"].as<s7_int>());
    printf("(add1 2): %lld\n", scheme.to<s7_int>(scheme.call("add1", 2)));
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
            [&](double x) { return scheme.make_c_object(new dax { .x = x, .data = scheme.nil() }); },
            [&](double x, s7_pointer p) { return scheme.make_c_object(new dax { .x = x, .data = p }); }
        ),
        s7::Op::GcMark, [&](dax &dax) -> void { scheme.mark(dax.data); },
        s7::Op::Equal,  [&](dax &d1, dax &d2) -> bool {
            return d1.x == d2.x && s7_is_equal(scheme.ptr(), d1.data, d2.data);
        },
        s7::Op::ToString, [&](dax &dax) -> std::string {
            return std::format("<dax {} {}>", dax.x, scheme.to_string(dax.data));
        }
    );

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
s7_pointer my_read(s7_scheme *sc, s7_read_t /*peek*/, s7_pointer /*port*/)
{
    auto &scheme = *reinterpret_cast<s7::Scheme *>(&sc);
    return scheme.from<char>(fgetc(stdin));
}

void my_print(s7_scheme *, uint8_t c, s7_pointer /*port*/)
{
    fprintf(stderr, "[%c] ", c);
}

void example_ports_redirect()
{
    s7::Scheme scheme;
    s7_set_current_output_port(scheme.ptr(), s7_open_output_function(scheme.ptr(), my_print));
    scheme["io-port"] = s7_open_input_function(scheme.ptr(), my_read);
    scheme.repl();
}

// Extend a built-in operator ("+" in this case)
void example_add_extension()
{
    s7::Scheme scheme;
    auto old_add = scheme["+"].as<s7_pointer>();
    auto old_string_append = scheme["string-append"].as<s7_pointer>();
    scheme.define_varargs_function("+", "(+ ...) adds or appends its arguments",
        [&scheme, old_add, old_string_append](s7::VarArgs<s7_pointer> args) {
            return scheme.apply(scheme.is<std::string>(args[0]) ? old_string_append : old_add, args);
        }
    );
    scheme.repl();
}

// s7_method not supported yet

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
// s7_method not supported yet

// Signal handling and continuations
// This one's pretty shit, admittedly...
/*
struct sigaction new_act, old_act;
s7::Scheme *global_scheme;

void handle_sigint(int ignored)
{
    printf("interrupted!\n");
    (*global_scheme)["*interrupt*"] = s7_make_continuation(global_scheme->ptr());
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
*/

// Notification from Scheme that a given Scheme variable has been set
void example_notification()
{
    s7::Scheme scheme;
    scheme.define_function("notify-C", "called if notified-var is set!",
        [&](s7_pointer a, s7_pointer b) -> s7_pointer {
            printf("%s is set to %s\n", scheme.to_string(a).data(), scheme.to_string(b).data());
            return b;
        });
    scheme["notified-var"] = 0;
    s7_set_setter(scheme.ptr(), scheme.sym("notified-var"), scheme["notify-C"].as<s7_pointer>());
    scheme.repl();
}

// Load C defined stuff into a separate namespace

// Handle scheme errors in C

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

// Bignums in C
// bignums not supported

int main()
{
    // example_repl();
    // example_c_function_variable();
    // example_call_get_set_vars();
    // example_cpp_repl();
    // example_listener_dax();
    // example_ports_redirect();
    // example_star_function();
    // example_macro();
    // example_notification();
    // example_hooks();
}

