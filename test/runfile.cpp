#include <cstdio>
#include "s7.hpp"

int main(int argc, char *argv[])
{
    if (argc != 2) {
        fprintf(stderr, "not enough args\n");
        return 1;
    }
    s7::Scheme scheme;
    s7_starlet_set(scheme.ptr(), scheme.sym("stacktrace-defaults"), scheme.list(100, 100, 100, 100, true).ptr());
    scheme.load(argv[1]);
    return 0;
}

