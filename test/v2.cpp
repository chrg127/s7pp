#include "s7.hpp"

struct v2 {
    double x, y;

    const double &operator[](size_t i) const { return i == 0 ? x : y; }

    // maybe it shouldn't define set! if this isn't provided...
    double &operator[](size_t i) { return i == 0 ? x : y; }

    v2 operator+=(const v2 &v) const
    {
        return v2 { .x = x + v.x,
                    .y = y + v.y };
    }
};

v2 operator*(double x, v2 v) { return v2 { v.x * x, v.y * x }; }
v2 operator*(v2 v, double x) { return v2 { v.x * x, v.y * x }; }

struct A {};

int main()
{
    s7::Scheme scheme;
    scheme.make_usertype<v2>("v2",
        s7::Constructors("v2",
            []() -> v2 { return v2 { .x = 0, .y = 0 }; },
            [](double x, double y) -> v2 { return v2 { .x = x, .y = y }; }),
        s7::Op::ToString, [](const v2 &v) -> std::string { return std::format("v2({}, {})", v.x, v.y); },
        s7::MethodOp::Add, &v2::operator+=,
        s7::MethodOp::Sub, [](const v2 &a, const v2 &b) { return v2 { .x = a.x - b.x, .y = a.y - b.y }; },
        s7::MethodOp::Mul, s7::Overload(
            s7::resolve<v2(double, v2)>(&operator*),
            s7::resolve<v2(v2, double)>(&operator*)
        ) 
    );
    scheme.define_property("v2-x", "(v2-x v2) accesses x", [](const v2 &v) { return v.x; }, [](v2 &v, double x) { v.x = x; });
    scheme.define_property("v2-y", "(v2-y v2) accesses y", [](const v2 &v) { return v.y; }, [](v2 &v, double y) { v.y = y; });

    scheme.make_usertype<A>("a"); // for testing +...

    scheme.repl();
}

void test_type_of()
{
    s7::Scheme scheme;
    scheme.make_usertype<v2>("v2", s7::Constructors("v2", []() { return v2 { .x = 0, .y = 0 }; }));
    scheme.define_function("type-of2", "better type of", [&](s7_pointer p) { return scheme.sym(scheme.type_of(p)); });
    scheme.repl();
}

