#include "s7.hpp"

// A simple listener
int main()
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

