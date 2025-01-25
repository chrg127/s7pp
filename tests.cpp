#include <cstdio>
#include <cstdlib>
#include <unordered_set>
#include "s7.hpp"
#include "s7/s7.h"

void test_scheme_defined_function()
{
    s7::Scheme scheme;
    scheme["an-integer"] = 1;
    scheme.eval("(define (add1 a) (+ a 1))");
    auto x = scheme["an-integer"].as_opt<int64_t>();
    printf("%s\n", std::format("{}", x.value()).c_str());
    printf("%s\n", std::format("an-integer: {}", scheme["an-integer"].as<int64_t>()).c_str());
    scheme["an-integer"] = 32;
    printf("%s\n", std::format("an-integer: {}", scheme["an-integer"].as_opt<int64_t>().value()).c_str());
    int64_t res = scheme.to<int64_t>(scheme.call("add1", 2));
    printf("%s\n", std::format("(add1 2): {}", res).c_str());

    auto fn = [](s7_scheme *sc, s7_pointer _args) -> s7_pointer {
        auto &scheme = *reinterpret_cast<s7::Scheme *>(&sc);
        auto args = s7::List(_args);
        auto sym = s7_symbol_table_find_name(scheme.sc, scheme.to<const char *>(args[0]));
        printf("%p\n", static_cast<void *>(sym));
        printf("%s\n", scheme.to_string(sym).data());
        return scheme.undefined();
    };
    scheme.define_function("test-sym", "test symbols", s7_function(fn));

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

    return scheme.error(s7::errors::WrongType {
       .arg = args[0],
       .arg_n = 1,
       .type = "integer",
       .caller = "add1",
   });
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

struct Set {
    std::unordered_set<s7_pointer, s7::Hash, s7::Equal> set;

    explicit Set(s7::Scheme &scheme)
        : set(std::unordered_set<s7_pointer, s7::Hash, s7::Equal>(512, s7::Hash(scheme.sc), s7::Equal(scheme.sc))) {}

    Set(const Set &) = delete;
    Set & operator=(const Set &) = delete;

    void gc_mark(s7::Scheme &scheme)
    {
        for (s7_pointer value : set) {
            scheme.mark(value);
        }
    }

    std::string to_string(s7::Scheme &scheme)
    {
        std::string str = "#<set(";
        for (const s7_pointer &value : set) {
            str += std::string(scheme.to_string(value)) + ", ";
        }
        str += ")>";
        return str;
    }

    s7_pointer add(s7_pointer p) { this->set.insert(p); return p; }
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

s7_pointer set_add(Set &set, s7_pointer arg)
{
    set.add(arg);
    return arg;
}

void test_define_function()
{
    s7::Scheme scheme;
    scheme.define_function("add-double", "doc", add_double);
    scheme.define_function("add-int", "doc", add_int);
    scheme.define_function("str-test", "doc", print_append);
    scheme.define_function("index-of-vec", "doc", find);
    scheme.define_function("add1", "doc", add1);

    int64_t x = 0;
    scheme.define_function("inc", "doc", [&]() -> void { x++; });
    scheme.define_function("get", "doc", [&]() -> int64_t { return x; });

    auto f = [i = 0]() mutable -> int64_t {
        i++;
        printf("i = %d\n", i);
        return i;
    };

    scheme.define_function("inc2", "doc", f);

    scheme.repl();
}

void test_set()
{
    s7::Scheme scheme;
    auto to_str = [&scheme](Set &set) -> std::string {
        return set.to_string(scheme);
    };
    scheme.make_c_type<Set>("set",
        s7::Op::ToString, to_str
    );
    scheme.define_function("set-add!", "(set-add! set value) adds value to set", &Set::add);
    scheme.repl();
}

struct v2 {
    double x, y;

    double &operator[](s7_int i) { return i == 0 ? x : y; }

    std::string to_string()
    {
        return std::format("v2({}, {})", x, y);
    }
};

void test_v2()
{
    s7::Scheme scheme;
    auto ctor1 = []() -> v2 { return v2 { .x = 0, .y = 0 }; };
    auto ctor2 = [](double x, double y) -> v2 { return v2 { .x = x, .y = y }; };
    auto ctor = scheme.make_constructor(ctor1, ctor2);
    scheme.define_function("v2", "doc", ctor);

    scheme.make_c_type<v2>("v2",
        s7::Op::ToString, &v2::to_string
    );
    scheme.repl();
}

void test_star_fns()
{
    s7::Scheme scheme;
    scheme.define_star_function("add", "a (b 1)", "doc", [](s7_int a, s7_int b) {
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

int main()
{
    // test_scheme_defined_function();
    // test_c_defined_function();
    // test_conversion();
    // test_define_function();
    // test_set();
    test_v2();
    // test_star_fns();
}

