#include "s7.hpp"

// Add a new Scheme type and a procedure with a setter

/* define *listener-prompt* in scheme, add two accessors for C get/set */
std::string_view listener_prompt(s7::Scheme &scheme)
{
    return scheme["*listener-prompt*"].to<std::string_view>();
}

void set_listener_prompt(s7::Scheme &scheme, std::string_view new_prompt)
{
    scheme["*listener-prompt*"] = new_prompt;
}

struct dax {
    double x;
    s7_pointer data;
};

int main()
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
        printf("%s ", scheme["*listener-prompt*"].to<const char *>());
        char buffer[512];
        fgets(buffer, sizeof(buffer), stdin);
        if (buffer[0] != '\n' || strlen(buffer) > 1) {
            printf("%s", scheme.to_string(scheme.eval(buffer)).data());
        }
        printf("\n");
    }
}

// Redirect output (and input) to a C procedure
void example_ports_redirect()
{
    s7::Scheme scheme;
    scheme.set_current_output_port([](s7::Scheme &, s7::OutputPort, uint8_t c) {
        fprintf(stderr, "[%c] ", c);
    });
    scheme["io-port"] = scheme.open_input_function([](s7::Scheme &scheme, s7::InputPort /*port*/, s7_read_t /*peek*/) -> s7_pointer {
        return scheme.from((char) fgetc(stdin));
    });
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

