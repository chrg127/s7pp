#include "s7.hpp"
#include <unordered_set>

struct Set {
    std::unordered_set<s7_pointer, s7::Hash, s7::Equal> set;

    explicit Set(s7::Scheme &scheme)
        : set(512, s7::Hash(scheme.sc), s7::Equal(scheme.sc))
    {}

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

    s7_int the_size() const { return 42; }
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

int main()
{
    s7::Scheme scheme;

    scheme.make_c_type<Set>("set",
        s7::Op::GcMark,   [&](Set &s) { return s.gc_mark(scheme); },
        s7::Op::ToString, [&](Set &s) { return s.to_string(scheme); },
        s7::Op::Length,   &Set::the_size
    );
    scheme.define_function("set-add!", "(set-add! set value) adds value to set", &Set::add);
    scheme.repl();
    return 0;
}

