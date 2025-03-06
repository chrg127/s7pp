#include <iostream>
#include "s7.hpp"

// C++ version of the repl
int main()
{
    s7::Scheme scheme;
    for (;;) {
        std::string str;
        std::cout << "s7> ";
        std::getline(std::cin, str);
        std::cout << scheme.to_string(scheme.eval(str)) << "\n";
    }
}

