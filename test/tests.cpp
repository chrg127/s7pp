#include <cstdio>
#include <cstdlib>
#include <unordered_set>
#include "s7.hpp"

auto add2(int x) -> int { return x + 2; };

void test_scheme_defined_function()
{
    s7::Scheme scheme;
    scheme["an-integer"] = 1;
    scheme.eval("(define (add1 a) (+ a 1))");
    auto x = scheme["an-integer"].to_opt<int64_t>();
    printf("%s\n", std::format("{}", x.value()).c_str());
    printf("%s\n", std::format("an-integer: {}", scheme["an-integer"].to<int64_t>()).c_str());
    scheme["an-integer"] = 32;
    printf("%s\n", std::format("an-integer: {}", scheme["an-integer"].to_opt<int64_t>().value()).c_str());
    int64_t res = scheme.to<int64_t>(scheme.call("add1", 2));
    printf("%s\n", std::format("(add1 2): {}", res).c_str());

    auto fn = [](s7_scheme *sc, s7_pointer _args) -> s7_pointer {
        auto &scheme = *reinterpret_cast<s7::Scheme *>(&sc);
        auto args = s7::List(_args);
        auto sym = s7_symbol_table_find_name(scheme.ptr(), scheme.to<const char *>(args[0]));
        printf("%p\n", static_cast<void *>(sym));
        printf("%s\n", scheme.to_string(sym).data());
        return scheme.undefined();
    };
    scheme.define_function("test-sym", "test symbols", s7_function(fn));

    scheme["testfn"] = &add2;
    scheme["testfn2"] = [&]() { return res; };
    scheme["testfn3"] = add2;

    scheme.repl();
}

s7_pointer add1(s7_scheme *sc, s7_pointer _args)
{
    auto &scheme = *reinterpret_cast<s7::Scheme *>(&sc);
    auto args = s7::List(_args);
    printf("add1 called\n");
    if (args.size() != 1) {
        return s7_wrong_number_of_args_error(sc, "add1", _args);
    }
    if (scheme.is<int64_t>(args[0])) {
        return scheme.from<int64_t>(scheme.to<int64_t>(args[0]) + 1);
    }
    return s7_wrong_type_arg_error(sc, "add1", 1, args[0], "an integer");
}

void test_c_defined_function()
{
    s7::Scheme scheme;
    scheme.define_function("add1", "(add1 int): adds 1 to int", add1);
    scheme.define("my-pi", 3.14159265);
    scheme.repl();
}

void test_conversion()
{
    s7::Scheme scheme;
    auto sym = scheme.define("a-list", scheme.list(1, 2, 3));
    printf("defined %s\n", scheme.to_string(sym).data());
    scheme.repl();
}

double add_double(double a, double b) { return a + b; }
int64_t add_int(int64_t x, int64_t y) { return x + y + 1; }

std::string print_append(std::string_view s)
{
    printf("a string! %s\n", s.data());
    return std::string(s) + " blah";
}

int64_t find(std::span<s7_int> vec, int64_t obj)
{
    for (std::size_t i = 0; i < vec.size(); i++) {
        if (vec[i] == obj) {
            return i;
        }
    }
    return -1;
}

void test_define_function()
{
    s7::Scheme scheme;
    scheme.define_function("add-double", "doc", add_double);
    scheme.define_function("add-int", "doc", add_int);
    scheme.define_function("print-append", "doc", print_append);
    scheme.define_function("find-index", "doc", find);
    scheme.repl();
}

struct Set {
    std::unordered_set<s7_pointer, s7::Hash, s7::Equal> set;

    explicit Set(s7::Scheme &scheme)
        : set(512, s7::Hash(scheme), s7::Equal(scheme)) {}

    Set(const Set &) = delete;
    Set & operator=(const Set &) = delete;
    Set(Set &&) = default;
    Set & operator=(Set &&) = default;

    void gc_mark(s7::Scheme &scheme) const
    {
        for (s7_pointer value : set) {
            scheme.mark(value);
        }
    }

    std::string to_string(s7::Scheme &scheme) const
    {
        std::string str = "#<set(";
        for (const s7_pointer &value : set) {
            str += std::string(scheme.to_string(value)) + ", ";
        }
        str += ")>";
        return str;
    }

    s7_pointer add(s7_pointer p) { this->set.insert(p); return p; }

    void add_all(s7::VarArgs<s7_pointer> ps)
    {
        for (auto p : ps) {
            this->set.insert(p);
        }
    }

    std::size_t the_size() const { return 42; }
};

template <typename T>
bool is_subset(const T &a, const T &b)
{
    return std::all_of(b.begin(), b.end(), [&](const auto &x) {
        return a.find(x) != a.end();
    });
}

bool operator==(const Set &a, const Set &b)
{
    return a.set.size() == b.set.size() && is_subset(a.set, b.set);
}

void test_set()
{
    s7::Scheme scheme;
    scheme.make_usertype<Set>("set",
        s7::Constructors([&]() { return Set(scheme); }),
        s7::Op::GcMark,   [&](const Set &s) { return s.gc_mark(scheme); },
        s7::Op::ToString, [&](const Set &s) { return s.to_string(scheme); },
        s7::Op::Length,   &Set::the_size
    );
    scheme.define_function("set-add!", "(set-add! set value) adds value to set",
            s7::Overload(&Set::add, &Set::add_all));
    scheme.repl();
}

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

void test_v2()
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

    scheme.define_function("test", "doc", s7::Overload([](s7_int x) { return x + 1; }, []() { return 0; }));
    scheme.make_usertype<A>("a");

    scheme.repl();
}

void test_star_fns()
{
    s7::Scheme scheme;
    scheme.define_star_function("add", "a (b 1)", "doc", [](int a, int b) {
        return a + b;
    });
    scheme.repl();
}

void test_from()
{
    s7::Scheme scheme;
    const char *s = "test";
    scheme["a-string"] = scheme.from(s);
    scheme.repl();
}

void test_varargs()
{
    s7::Scheme scheme;

    scheme.define_function("add", "doc", [](s7::VarArgs<int> args) -> int {
        int sum = 0;
        for (auto arg : args) {
            sum += arg;
        }
        return sum;
    });

    scheme.define_function("add-with-init", "add but with an init value", [](s7_int init, s7::VarArgs<s7_int> args) -> s7_int {
        printf("init = %lld\n", init);
        for (auto v : args) {
            init += v;
        }
        return init;
    });

    scheme.repl();
}

void test_sig()
{
    auto f = [](s7_int x) -> s7_int { return x + 1; };
    auto g = [](s7::VarArgs<s7_int> args) { return args[0]; };
    auto h = []() { return 42; };
    s7::Scheme scheme;
    scheme.make_usertype<Set>("set");
    printf("%s\n", scheme.to_string(scheme.make_signature(f)).data());
    printf("%s\n", scheme.to_string(scheme.make_signature(g)).data());
    printf("%s\n", scheme.to_string(scheme.make_signature(&Set::add)).data());
    printf("%s\n", scheme.to_string(scheme.make_signature(h)).data());
    scheme.repl();
}

void test_type_of()
{
    s7::Scheme scheme;
    scheme.make_usertype<v2>("v2", s7::Constructors("v2", []() { return v2 { .x = 0, .y = 0 }; }));
    scheme.define_function("type-of2", "better type of", [&](s7_pointer p) { return scheme.sym(scheme.type_of(p)); });
    scheme.repl();
}

void test_complex()
{
    s7::Scheme scheme;
    scheme.define_function("z+", "add 1 to complex", [](s7_complex z) { return s7_complex(z.real() + 1, z.imag()); });
    scheme.repl();
}

void test_history()
{
    s7::Scheme scheme;
    scheme.set_history_enabled(true);
    scheme.add_history(scheme.from(1));
    scheme.add_history(scheme.from(2));
    scheme.add_history(scheme.from(3));
    auto histsize = scheme.to<s7_int>(s7_starlet_ref(scheme.ptr(), scheme.sym("history-size")));
    for (auto i = histsize; i >= 0; i--) {
        printf("%s\n", scheme.to_string(scheme.history()[i]).data());
    }
}

void test_errors()
{
    s7::Scheme scheme;
    scheme.define_function("errs", "", [&](s7_int x) {
        if (x == 0) throw s7::errors::Error("generic-error", scheme.list("x = ~a\n", x));
        if (x == 1) throw s7::errors::WrongType { .arg = scheme.from(x), .arg_n = 1, .desc = "a nothing", .caller = "errs" };
        if (x == 2) throw s7::errors::OutOfRange { .arg = scheme.from(x), .arg_n = 1, .desc = "somehow", .caller = "errs" };
        if (x == 3) throw s7::errors::WrongArgsNumber { .args = scheme.from(scheme.list(1, 2, 3)), .caller = "errs" };
    });
    scheme.repl();
}

int main(int argc, char *argv[])
{
    // test_scheme_defined_function();
    // test_c_defined_function();
    // test_conversion();
    // test_define_function();
    // test_set();
    test_v2();
    // test_star_fns();
    // test_varargs();
    // test_sig();
    // test_type_of();
    // test_complex();
    // test_history();
    // test_errors();
}

