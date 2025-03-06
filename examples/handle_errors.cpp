#include "s7.hpp"

// Handle scheme errors in C
int main()
{
    s7::Scheme scheme;
    scheme.define_function("error-handler", "out error handler", [](std::string_view error) -> bool {
        printf("error: %s\n", error.data());
        return false;
    });
    bool with_error_hook = false;
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
            auto old_port = scheme.set_current_error_port(scheme.open_string());
            s7_int gc_loc = -1;
            if (old_port.ptr() != scheme.nil()) {
                gc_loc = scheme.protect(old_port);
            }

            /* eval input */
            auto result = scheme.eval(buffer);

            /* print out the value wrapped in "{}" so we can tell it from other IO paths */
            printf("{%s}", scheme.to_string(result).data());

            /* look for error messages */
            auto errmsg = scheme.current_error_port().get_string();

            /* if we got something, wrap it in "[]" */
            if (!errmsg.empty()) {
                printf("[%s]", errmsg.data());
            }

            scheme.current_error_port().close();
            if (gc_loc != -1) {
                scheme.unprotect(gc_loc);
            }
        }
        printf("\n");
    }
}

