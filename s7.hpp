#include <cstdint>
#include <cstring>
#include <cassert>
#include <string>
#include <string_view>
#include <format>
#include <type_traits>
#include <memory>
#include <functional>
#include <unordered_map>
#include <optional>
#include <utility>
#include <array>
#include "function_traits.hpp"
#include "s7/s7.h"

namespace s7 {

namespace detail {
    template <typename L>
    struct LambdaTable {
        static inline std::function<typename FunctionTraits<L>::Signature> lambda;
        static inline std::unordered_map<uintptr_t, const char *> name;
    };

    template <typename T>
    struct TypeTag {
        static inline std::unordered_map<uintptr_t, s7_int> tags;
        static inline std::unordered_map<uintptr_t, const char *> names;
    };

    template <typename T>
    s7_int get_type_tag(s7_scheme *sc)
    {
        auto &m = detail::TypeTag<std::remove_cvref_t<T>>::tags;
        auto it = m.find(reinterpret_cast<uintptr_t>(sc));
        assert(it != m.end() && "missing tag for T");
        return it->second;
    }

    template <typename T>
    const char *get_type_name(s7_scheme *sc)
    {
        auto &m = detail::TypeTag<std::remove_cvref_t<T>>::names;
        auto it = m.find(reinterpret_cast<uintptr_t>(sc));
        assert(it != m.end() && "missing tag for T");
        return it->second;
    }
} // namespace detail

struct Equal {
    s7_scheme *sc;

    bool operator()(const s7_pointer &a, const s7_pointer &b) const {
        return s7_is_equal(sc, a, b);
    }
};

struct Hash {
    s7_scheme *sc;

    size_t operator()(const s7_pointer& p) const
    {
        return s7_hash_code(sc, p, s7_name_to_value(sc, "equal?"));
    }
};

class List {
    s7_pointer p;

public:
    explicit List(s7_pointer p) : p{p} {}

    s7_pointer operator[](std::size_t i) const
    {
        s7_pointer x = this->p;
        while (i-- > 0) {
            x = s7_cdr(x);
        }
        return s7_car(x);
    }

    s7_pointer car() const { return s7_car(p); }
    List cdr() const { return List(s7_cdr(p)); }
    s7_pointer ptr() const { return p; }

    s7_pointer advance() { auto tmp = s7_car(p); p = s7_cdr(p); return tmp; }

    std::size_t size() const
    {
        size_t s = 0;
        for (auto p = this->p; s7_is_pair(p); p = s7_cdr(p), s++)
            ;
        return s;
    }

    struct iterator {
        s7_pointer p = nullptr;

        using value_type = s7_pointer;

        iterator() = default;
        explicit iterator(s7_pointer p) : p{p} {}
        iterator & operator++() { p = s7_cdr(p); return *this; }
        iterator operator++(int) { auto i = *this; p = s7_cdr(p); return i; }
        s7_pointer operator*() const { return s7_car(p); }
    };

    iterator begin() { return iterator(p); }
    iterator end() { return iterator(); }
};

bool operator==(const List::iterator &a, const List::iterator &b)
{
    return ((a.p == nullptr || !s7_is_pair(a.p)) && (b.p == nullptr || !s7_is_pair(b.p)))
        || (a.p != nullptr && b.p != nullptr && s7_is_eq(a.p, b.p));
}

struct Variable;

// should we add is_rational, is_ratio and is_complex?
template <typename T>
concept InputType = std::is_same_v<T, s7_pointer>
                 || std::is_same_v<T, bool>
                 || std::is_same_v<T, s7_int>
                 || std::is_same_v<T, double>
                 || std::is_same_v<T, const char *>
                 || std::is_same_v<T, std::string_view>
                 || std::is_same_v<T, char>                     // character
                 || std::is_same_v<T, std::span<s7_pointer>>    // generic vector
                 || std::is_same_v<T, std::span<s7_int>>        // int vector
                 || std::is_same_v<T, std::span<double>>        // float vector
                 || std::is_same_v<T, std::span<uint8_t>>       // byte vector
                 || std::is_pointer_v<T>                        // c-pointer
                 || std::is_same_v<T, List>;

template <typename T>
concept OutputType = std::is_same_v<T, s7_pointer>
                  || std::is_same_v<T, bool>
                  || std::is_same_v<T, s7_int>
                  || std::is_same_v<T, int>
                  || std::is_same_v<T, short>
                  || std::is_same_v<T, long>
                  || std::is_same_v<T, float>
                  || std::is_same_v<T, double>
                  || std::is_same_v<T, const char *>
                  || std::is_same_v<std::remove_cvref_t<T>, std::string>
                  || std::is_same_v<T, std::string_view>
                  || std::is_same_v<T, char>                     // character
                  || std::is_same_v<T, std::span<s7_pointer>>    // generic vector
                  || std::is_same_v<T, std::span<s7_int>>        // int vector
                  || std::is_same_v<T, std::span<int>>
                  || std::is_same_v<T, std::span<short>>
                  || std::is_same_v<T, std::span<long>>
                  || std::is_same_v<T, std::span<double>>
                  || std::is_same_v<T, std::span<uint8_t>>
                  || std::is_same_v<T, std::vector<s7_pointer>>
                  || std::is_same_v<T, std::vector<s7_int>>
                  || std::is_same_v<T, std::vector<int>>
                  || std::is_same_v<T, std::vector<short>>
                  || std::is_same_v<T, std::vector<long>>
                  || std::is_same_v<T, std::vector<double>>
                  || std::is_same_v<T, std::vector<uint8_t>>
                  || std::is_same_v<T, std::is_pointer<T>>
                  || std::is_same_v<T, List>;

namespace errors {

struct Error {
    std::string_view type;
    List info;
};

struct WrongType {
    s7_pointer arg;
    s7_int arg_n;
    std::string_view type;
    std::string_view caller;
};

struct OutOfRange {
    s7_pointer arg;
    std::string_view type;
    s7_int arg_n;
    std::string_view caller;
};

struct WrongArgsNumber {
    s7_pointer args;
    std::string_view caller;
};

} // errors

struct FunctionOpts {
    bool unsafe_body;
    bool unsafe_arglist;
};

enum class Op {
    Equal, Equivalent, Copy, Fill, Reverse, GcMark, GcFree,
    Length, ToString, ToList, Ref, Set,
};

struct Scheme {
    s7_scheme *sc;

    Scheme() : sc(s7_init()) {}

    ~Scheme()
    {
        s7_quit(sc);
        s7_free(sc);
    }

    Scheme(const Scheme &) = delete;
    Scheme & operator=(const Scheme &) = delete;
    Scheme(Scheme &&other) { operator=(std::move(other)); }
    Scheme & operator=(Scheme &&other) { this->sc = other.sc; other.sc = nullptr; return *this; }

    s7_scheme *ptr() { return sc; }

    /* eval/repl stuff */
    s7_pointer eval(std::string_view code)
    {
        return s7_eval_c_string(sc, code.data());
    }

    void repl(
        std::function<bool(std::string_view)> quit = [](std::string_view) { return false; },
        std::function<void(std::string_view)> output = [](std::string_view s) {
            printf("%s", s.data());
        },
        std::function<std::string()> input = []() {
            char buffer[512];
            fgets(buffer, sizeof(buffer), stdin);
            return std::string(buffer);
        }
    )
    {
        for (;;) {
            output("> ");
            auto s = input();
            if (quit(s)) {
                break;
            }
            if (s[0] != '\n' || s.size() > 1) {
                output(to_string(eval(s)));
            }
            output("\n");
        }
    }

    /* gc stuff */
    void protect(s7_pointer p)
    {
        s7_gc_protect(sc, p);
    }

    void mark(s7_pointer p)
    {
        s7_mark(p);
    }

    /* constants */
    s7_pointer undefined() { return s7_undefined(sc); }
    s7_pointer nil() { return s7_nil(sc); }

    /* functions for inspecting and converting from/to scheme objects */
    template <typename T>
    bool is(s7_pointer p)
    {
             if constexpr(std::is_same_v<T, s7_pointer>) { return p; }
        else if constexpr(std::is_same_v<T, bool>) { return s7_is_boolean(p); }
        else if constexpr(std::is_same_v<T, s7_int>) { return s7_is_integer(p); }
        else if constexpr(std::is_same_v<T, double>) { return s7_is_real(p); }
        else if constexpr(std::is_same_v<T, const char *>
                       || std::is_same_v<T, std::string_view>) { return s7_is_string(p); }
        else if constexpr(std::is_same_v<T, unsigned char>) { return s7_is_character(p); }
        else if constexpr(std::is_same_v<T, std::span<s7_pointer>>) { return s7_is_vector(p); }
        else if constexpr(std::is_same_v<T, std::span<s7_int>>) { return s7_is_int_vector(p); }
        else if constexpr(std::is_same_v<T, std::span<double>>) { return s7_is_float_vector(p); }
        else if constexpr(std::is_same_v<T, std::span<uint8_t>>) { return s7_is_byte_vector(p); }
        else if constexpr(std::is_pointer_v<T>) { return s7_is_c_pointer(p); }
        else if constexpr(std::is_same_v<T, List>) { return s7_is_pair(p); }
        return s7_is_c_object(p) && s7_c_object_type(p) == detail::get_type_tag<T>(sc);
    }

    template <typename T>
    T to(s7_pointer p)
    {
        assert(is<T>(p) && "p isn't an object of type T");
             if constexpr(std::is_same_v<T, s7_pointer>) { return p; }
        else if constexpr(std::is_same_v<T, bool>) { return s7_boolean(sc, p); }
        else if constexpr(std::is_same_v<T, s7_int>) { return s7_integer(p); }
        else if constexpr(std::is_same_v<T, double>) { return s7_real(p); }
        else if constexpr(std::is_same_v<T, const char *>) { return s7_string(p); } // do not free
        else if constexpr(std::is_same_v<T, std::string_view>) { return std::string_view(s7_string(p)); } // do not free
        else if constexpr(std::is_same_v<T, char>) { return static_cast<char>(s7_character(p)); }
        else if constexpr(std::is_same_v<T, std::span<s7_pointer>>) { return std::span(s7_vector_elements(p), s7_vector_length(p)); }
        else if constexpr(std::is_same_v<T, std::span<s7_int>>) { return std::span(s7_int_vector_elements(p), s7_vector_length(p)); }
        else if constexpr(std::is_same_v<T, std::span<double>>) { return std::span(s7_float_vector_elements(p), s7_vector_length(p)); }
        else if constexpr(std::is_same_v<T, std::span<uint8_t>>) { return std::span(s7_byte_vector_elements(p), s7_vector_length(p)); }
        else if constexpr(std::is_pointer_v<T>) { return reinterpret_cast<T>(s7_c_pointer(p)); }
        else if constexpr(std::is_same_v<T, List>) { return List(p); }
        else { return *reinterpret_cast<std::remove_cvref_t<T> *>(s7_c_object_value(p)); }
    }

    template <typename T>
    s7_pointer from(const T &x)
    {
             if constexpr(std::is_same_v<T, s7_pointer>) { return x; }
        else if constexpr(std::is_same_v<T, bool>) { return s7_make_boolean(sc, x); }
        else if constexpr(std::is_same_v<T, s7_int> || std::is_same_v<T, int>
                       || std::is_same_v<T, short> || std::is_same_v<T, long>) { return s7_make_integer(sc, x); }
        else if constexpr(std::is_same_v<T, double> || std::is_same_v<T, float>) { return s7_make_real(sc, x); }
        else if constexpr(std::is_same_v<std::decay_t<std::remove_cvref_t<T>>, char *>) { return s7_make_string(sc, x); }
        else if constexpr(std::is_same_v<std::remove_cvref_t<T>, std::string>) { return s7_make_string_with_length(sc, x.c_str(), x.size()); }
        else if constexpr(std::is_same_v<T, std::string_view>) { return s7_make_string_with_length(sc, x.data(), x.size()); }
        else if constexpr(std::is_same_v<T, unsigned char>) { return s7_make_character(sc, x); }
        else if constexpr(std::is_same_v<T, std::span<s7_pointer>> || std::is_same_v<T, std::vector<s7_pointer>>) {
            auto vec = s7_make_vector(sc, x.size());
            for (size_t i = 0; i < x.size(); i++) {
                s7_vector_set(sc, vec, i, x[i]);
            }
            return vec;
        } else if constexpr(std::is_same_v<T, std::span<s7_int>> || std::is_same_v<T, std::vector<s7_int>>
                         || std::is_same_v<T, std::span<int>>    || std::is_same_v<T, std::vector<int>>
                         || std::is_same_v<T, std::span<short>>  || std::is_same_v<T, std::vector<short>>
                         || std::is_same_v<T, std::span<long>>   || std::is_same_v<T, std::vector<long>>) {
            auto vec = s7_make_int_vector(sc, x.size(), 1, nullptr);
            for (size_t i = 0; i < x.size(); i++) {
                s7_int_vector_set(vec, i, x[i]);
            }
            return vec;
        } else if constexpr(std::is_same_v<T, std::span<double>> || std::is_same_v<T, std::vector<double>>
                         || std::is_same_v<T, std::span<float>>  || std::is_same_v<T, std::vector<float>>) {
            auto vec = s7_make_float_vector(sc, x.size(), 1, nullptr);
            for (size_t i = 0; i < x.size(); i++) {
                s7_float_vector_set(vec, i, x[i]);
            }
            return vec;
        } else if constexpr(std::is_same_v<T, std::span<uint8_t>> || std::is_same_v<T, std::vector<uint8_t>>) {
            auto vec = s7_make_byte_vector(sc, x.size(), 1, nullptr);
            for (size_t i = 0; i < x.size(); i++) {
                s7_byte_vector_set(vec, i, x[i]);
            }
            return vec;
        }
        else if constexpr(std::is_pointer_v<T>) { return s7_make_c_pointer(sc, x); }
        else if constexpr(std::is_same_v<T, List>) { return x.ptr(); }
        else {
            auto tag = detail::get_type_tag<T>(sc);
            return s7_make_c_object(sc, tag, reinterpret_cast<void *>(new T(x)));
        }
    }

    template <typename T>
    std::optional<T> to_opt(s7_pointer p)
    {
        if (!is<T>(p)) {
            return std::nullopt;
        }
        return to<T>(p);
    }

    std::string_view to_string(s7_pointer p)
    {
        // avoid s7_object_to_c_string since return value must be freed
        return this->to<std::string_view>(s7_object_to_string(sc, p, true));
    }

    template <typename T>
    List list(const T &arg)
    {
        return List(s7_cons(sc, this->from<T>(arg), s7_nil(sc)));
    }

    template <typename T, typename... Args>
    List list(const T &arg, Args&&... args)
    {
        return List(s7_cons(sc, this->from<T>(arg), list(args...).ptr()));
    }

    List list() { return s7::List(s7_nil(sc)); }

    /* errors */
    template <typename T>
    s7_pointer error(T data)
    {
        if constexpr(std::is_same_v<T, errors::Error>)      { return s7_error(sc, s7_make_symbol(sc, data.type.data()), data.info.ptr()); }
        if constexpr(std::is_same_v<T, errors::WrongType>)  { return s7_wrong_type_arg_error(sc, data.caller.data(), data.arg_n, data.arg, data.type.data()); }
        if constexpr(std::is_same_v<T, errors::OutOfRange>) { return s7_out_of_range_error(sc, data.caller.data(), data.arg_n, data.arg, data.type.data()); }
        if constexpr(std::is_same_v<T, errors::WrongArgsNumber>) { return s7_wrong_number_of_args_error(sc, data.caller.data(), data.args); }
    }

    /* variables and symbols */
    template <typename T>
    s7_pointer define(std::string_view name, const T &value, std::string_view doc = "")
    {
        auto object = this->from<T>(value);
        return s7_define_variable_with_documentation(sc, name.data(), object, doc.data());
    }

    template <typename T>
    s7_pointer define_const(std::string_view name, const T &value, std::string_view doc = "")
    {
        auto object = this->from<T>(value);
        return s7_define_constant_with_documentation(sc, name.data(), object, doc.data());
    }

    Variable operator[](std::string_view name);

    /* signatures */

private:
    template <typename T>
    s7_pointer sig_type()
    {
        auto f = [&](auto s) { return s7_make_symbol(sc, s); };
             if constexpr(std::is_same_v<T, s7_pointer>) { return s7_t(sc); }
        else if constexpr(std::is_same_v<T, bool>) { return f("boolean?"); }
        else if constexpr(std::is_same_v<T, s7_int>) { return f("integer?"); }
        else if constexpr(std::is_same_v<T, double>) { return f("real?"); }
        else if constexpr(std::is_same_v<T, const char *>) { return f("string?"); }
        else if constexpr(std::is_same_v<T, std::string_view>) { return f("string?"); }
        else if constexpr(std::is_same_v<T, char>) { return f("character?"); }
        else if constexpr(std::is_same_v<T, std::span<s7_pointer>>) { return f("vector?"); }
        else if constexpr(std::is_same_v<T, std::span<s7_int>>) { return f("int-vector?"); }
        else if constexpr(std::is_same_v<T, std::span<double>>) { return f("float-vector?"); }
        else if constexpr(std::is_same_v<T, std::span<uint8_t>>) { return f("byte-vector?"); }
        else if constexpr(std::is_pointer_v<T>) { return f("c-pointer?"); }
        else if constexpr(std::is_same_v<T, List>) { return f("list?"); }
        else { return f("c-object?"); }
    }

    template <typename T>
    s7_pointer sig_output_type()
    {
        auto f = [&](auto s) { return s7_make_symbol(sc, s); };
             if constexpr(std::is_same_v<T, s7_pointer>) { return s7_t(sc); }
        else if constexpr(std::is_same_v<T, bool>) { return f("boolean?"); }
        else if constexpr(std::is_same_v<T, s7_int> || std::is_same_v<T, int>
                       || std::is_same_v<T, short>  || std::is_same_v<T, long>) { return f("integer?"); }
        else if constexpr(std::is_same_v<T, double> || std::is_same_v<T, float>) { return f("real?"); }
        else if constexpr(std::is_same_v<std::decay_t<std::remove_cvref_t<T>>, char *>) { return f("string?"); }
        else if constexpr(std::is_same_v<std::remove_cvref_t<T>, std::string>) { return f("string?"); }
        else if constexpr(std::is_same_v<T, unsigned char>) { return f("character?"); }
        else if constexpr(std::is_same_v<T, std::span<s7_pointer>>
                       || std::is_same_v<T, std::vector<s7_pointer>>) { return f("vector?"); }
        else if constexpr(std::is_same_v<T, std::span<s7_int>> || std::is_same_v<T, std::vector<s7_int>>
                       || std::is_same_v<T, std::span<int>>    || std::is_same_v<T, std::vector<int>>
                       || std::is_same_v<T, std::span<short>>  || std::is_same_v<T, std::vector<short>>
                       || std::is_same_v<T, std::span<long>>   || std::is_same_v<T, std::vector<long>>) { return f("int-vector?"); }
        else if constexpr(std::is_same_v<T, std::span<double>> || std::is_same_v<T, std::vector<double>>
                       || std::is_same_v<T, std::span<float>>  || std::is_same_v<T, std::vector<float>>) { return f("float-vector?"); }
        else if constexpr(std::is_same_v<T, std::span<uint8_t>> || std::is_same_v<T, std::vector<uint8_t>>) { return f("byte-vector?"); }
        else if constexpr(std::is_pointer_v<T>) { return f("c-pointer?"); }
        else if constexpr(std::is_same_v<T, List>) { return f("list?"); }
        else { return f("c-object?"); }
    }

public:
    template <typename R, typename... Args>
    s7_pointer make_signature(R (*)(Args...))
    {
        return s7_make_signature(sc, sizeof...(Args) + 2, sig_output_type<R>(), sig_type<Args>()...);
    }

    template <typename C, typename R, typename... Args>
    s7_pointer make_signature(R (C::*)(Args...) const)
    {
        return s7_make_signature(sc, sizeof...(Args) + 1, sig_output_type<R>(), sig_type<Args>()...);
    }

    template <typename C, typename R, typename... Args>
    s7_pointer make_signature(R (C::*)(Args...))
    {
        return s7_make_signature(sc, sizeof...(Args) + 1, sig_output_type<R>(), sig_type<Args>()...);
    }

    template <typename F>
    s7_pointer make_signature(F &&)
    {
        return make_signature(&std::remove_cvref_t<F>::operator());
    }

    /* calling functions */
    template <typename... T>
    s7_pointer call(std::string_view name, T&&... args)
    {
        return s7_call(sc, s7_name_to_value(sc, name.data()), this->list(args...).ptr());
    }

    /* function creation */
private:
    template <typename L, typename R, typename... Args>
    s7_function _make_s7_function()
    {
        constexpr auto NumArgs = FunctionTraits<L>::arity;
        return [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
            auto &fn = detail::LambdaTable<L>::lambda;
            auto name = detail::LambdaTable<L>::name
                .find(reinterpret_cast<uintptr_t>(sc))->second;
            auto &scheme = *reinterpret_cast<Scheme *>(&sc);

            auto arglist = List(args);
            std::array<s7_pointer, NumArgs> arr;
            for (std::size_t i = 0; i < NumArgs; i++) {
                arr[i] = arglist.advance();
            }

            auto bools = [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                return std::array<bool, NumArgs> { scheme.is<Args>(arr[Is])... };
            }(std::make_index_sequence<NumArgs>());
            auto first_wrong_type = std::find(bools.begin(), bools.end(), false);

            if (first_wrong_type != bools.end()) {
                auto i = first_wrong_type - bools.begin();
                arglist = List(args);
                auto types = std::array<const char *, NumArgs> { scheme.type_to_string<Args>()... };
                return s7_wrong_type_arg_error(sc, name, i+1, arglist[i], types[i]);
            }

            if constexpr(std::is_same_v<R, void>) {
                [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                    fn(scheme.to<Args>(arr[Is])...);
                }(std::make_index_sequence<NumArgs>());
                return s7_undefined(sc);
            } else {
                auto res = [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                    return fn(scheme.to<Args>(arr[Is])...);
                }(std::make_index_sequence<NumArgs>());
                return scheme.from<R>(res);
            }
        };
    }

    template <typename L, typename R, typename... Args>
    s7_function _make_s7_function(R (L::*)(Args...) const)
    {
        return _make_s7_function<L, R, Args...>();
    }

    template <typename L, typename R, typename... Args>
    s7_function _make_s7_function(R (L::*)(Args...))
    {
        return _make_s7_function<L, R, Args...>();
    }

    template <typename L>
    s7_function make_s7_function(const char *name, L&& lambda)
    {
        using Lambda = std::remove_cvref_t<L>;
        detail::LambdaTable<Lambda>::lambda = lambda;
        detail::LambdaTable<Lambda>::name
            .insert_or_assign(reinterpret_cast<uintptr_t>(sc), name);
        return _make_s7_function(&Lambda::operator());
    }

    template <typename R, typename... Args>
    s7_function make_s7_function(const char *name, R (*fptr)(Args...))
    {
        return make_s7_function(name, [=](Args... args) -> R { return fptr(args...); });
    }

    template <typename C, typename R, typename... Args>
    s7_function make_s7_function(const char *name, R (C::*fptr)(Args...))
    {
        return make_s7_function(name, [=](C &c, Args... args) -> R { return ((&c)->*fptr)(args...); });
    }

    template <typename C, typename R, typename... Args>
    s7_function make_s7_function(const char *name, R (C::*fptr)(Args...) const)
    {
        return make_s7_function(name, [=](const C &c, Args... args) -> R { return ((&c)->*fptr)(args...); });
    }

public:
    // special case for functions that follow s7's standard signature
    s7_pointer define_function(std::string_view name, std::string_view doc, s7_function fn,
        FunctionOpts opts = { .unsafe_body = false, .unsafe_arglist = false })
    {
        auto _name = s7_string(save_string(name));
        auto define = opts.unsafe_arglist || opts.unsafe_body
            ? s7_define_function
            : s7_define_safe_function;
        return define(sc, _name, fn, 0, 0, true, doc.data());
    }

    template <typename F>
    s7_pointer define_function(std::string_view name, std::string_view doc, F &&func,
        FunctionOpts opts = { .unsafe_body = false, .unsafe_arglist = false })
    {
        constexpr auto NumArgs = FunctionTraits<F>::arity;
        auto _name = s7_string(save_string(name));
        auto f = make_s7_function(_name, func);
        auto define = opts.unsafe_body && opts.unsafe_arglist ? s7_define_unsafe_typed_function
                    : opts.unsafe_body                        ? s7_define_semisafe_typed_function
                    :                                           s7_define_typed_function;
        auto sig = make_signature(func);
        return define(sc, _name, f, NumArgs, 0, false, doc.data(), sig);
    }

    template <typename F>
    void define_star_function(std::string_view name, std::string_view arglist_desc, std::string_view doc, F&& func)
    {
        auto _name = s7_string(save_string(name));
        auto f = make_s7_function(_name, func);
        auto sig = make_signature(func);
        s7_define_typed_function_star(sc, _name, f, arglist_desc.data(), doc.data(), sig);
    }

    // template <typename R>
    // void define_vararg_function(std::string_view name, std::string_view doc, R (*fptr)(List args))
    // {
    //     auto _name = s7_string(save_string(name));
    //     detail::LambdaTable<Lambda>::lambda = fptr;
    //     detail::LambdaTable<Lambda>::name
    //         .insert_or_assign(reinterpret_cast<uintptr_t>(sc), _name);
    //     auto f = make_s7_function(
    // }

    /* usertypes */
    template <typename T>
    s7_int make_c_type(std::string_view name)
    {
        auto tag = s7_make_c_type(sc, name.data());
        detail::TypeTag<T>::tags
            .insert_or_assign(reinterpret_cast<uintptr_t>(sc), tag);
        detail::TypeTag<T>::names
            .insert_or_assign(reinterpret_cast<uintptr_t>(sc), s7_string(save_string(name)));

        if constexpr(requires { T(); }) {
            auto ctor_name = std::format("make-{}", name);
            auto doc = std::format("(make-{}) creates a new {}", name, name);
            define_function(s7_string(save_string(ctor_name)), doc.c_str(), [this, tag]() -> s7_pointer {
                return s7_make_c_object(this->sc, tag, reinterpret_cast<void *>(new T()));
            });
        } else if constexpr(requires { T(*this); }) {
            auto ctor_name = std::format("make-{}", name);
            auto doc = std::format("(make-{}) creates a new {}", name, name);
            define_function(s7_string(save_string(ctor_name)), doc.c_str(), [this, tag]() -> s7_pointer {
                return s7_make_c_object(sc, tag, reinterpret_cast<void *>(new T(*this)));
            });
        }

        s7_c_type_set_gc_free(sc, tag, [](s7_scheme *, s7_pointer obj) -> s7_pointer {
            T *o = reinterpret_cast<T *>(s7_c_object_value(obj));
            delete o;
            return nullptr;
        });

        if constexpr(requires(T a, T b) { a == b; }) {
            s7_c_type_set_is_equal(sc, tag, [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
                s7_pointer a = s7_car(args);
                s7_pointer b = s7_cadr(args);
                if (a == b) {
                    return s7_t(sc);
                }
                auto tag = detail::get_type_tag<T>(sc);
                if (!s7_is_c_object(b) || (s7_c_object_type(b) != tag)) {
                    return s7_f(sc);
                }
                T *pa = (T *) s7_c_object_value(a);
                T *pb = (T *) s7_c_object_value(b);
                return s7_make_boolean(sc, *pa == *pb);
            });
        }

        if constexpr(requires(T t) { t.size(); }) {
            s7_c_type_set_length(sc, tag, [](s7_scheme *sc, s7_pointer obj) -> s7_pointer {
                T *o = reinterpret_cast<T *>(obj);
                return o->size();
            });
        }

        if constexpr(requires() { &T::operator[]; }) {
            s7_c_type_set_ref(sc, tag, [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
                auto &scheme = *reinterpret_cast<Scheme *>(&sc);
                auto *obj = reinterpret_cast<T *>(s7_c_object_value(s7_car(args)));
                using ArgType = typename FunctionTraits<decltype(&T::operator[])>::Argument<1>::Type;
                auto arg = s7_cadr(args);
                if (!scheme.is<ArgType>(arg)) {
                    return s7_wrong_type_arg_error(sc, "T ref", 1, arg,
                        scheme.type_to_string<ArgType>());
                }
                return scheme.from((*obj)[scheme.to<ArgType>(arg)]);
            });

            s7_c_type_set_set(sc, tag, [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
                auto &scheme = *reinterpret_cast<Scheme *>(&sc);
                auto *obj = reinterpret_cast<T *>(s7_c_object_value(s7_car(args)));
                using IndexType = typename FunctionTraits<decltype(&T::operator[])>::Argument<1>::Type;
                using ValueType = std::remove_cvref_t<typename FunctionTraits<decltype(&T::operator[])>::ReturnType>;
                auto index = s7_cadr(args);
                if (!scheme.is<IndexType>(index)) {
                    return s7_wrong_type_arg_error(sc, "T ref", 1, index,
                        scheme.type_to_string<IndexType>());
                }
                auto value = s7_caddr(args);
                if (!scheme.is<ValueType>(value)) {
                    return s7_wrong_type_arg_error(sc, "T ref", 2, value,
                        scheme.type_to_string<ValueType>());
                }
                (*obj)[scheme.to<IndexType>(index)] = scheme.to<ValueType>(value);
                return s7_undefined(sc);
            });
        }

        auto is_name = std::format("{}?", name);
        auto is_doc  = std::format("({}? value) checks if value is a {}", name, name);
        auto is = [](s7_scheme *sc, s7_pointer args) {
            auto &scheme = *reinterpret_cast<Scheme *>(&sc);
            return scheme.from<bool>(scheme.is<T>(s7_car(args)));
        };
        s7_define_function(sc, s7_string(save_string(is_name)), is, 1, 0, false, is_doc.c_str());

        return tag;
    }

    template <typename T, typename F, typename... Args>
    void make_c_type(std::string_view name, Op op, F &&fn, Args&&... args)
    {
        auto tag = make_c_type<T, Args...>(name, args...);
        auto set_func = op == Op::Equal    ? s7_c_type_set_is_equal
                      : op == Op::Copy     ? s7_c_type_set_copy
                      : op == Op::Fill     ? s7_c_type_set_fill
                      : op == Op::Reverse  ? s7_c_type_set_reverse
                      : op == Op::GcMark   ? s7_c_type_set_gc_mark
                      : op == Op::GcFree   ? s7_c_type_set_gc_free
                      : op == Op::Length   ? s7_c_type_set_length
                      : op == Op::ToString ? s7_c_type_set_to_string
                      : op == Op::ToList   ? s7_c_type_set_to_list
                      : op == Op::Ref      ? s7_c_type_set_ref
                      :                            s7_c_type_set_set;
        auto func_name = std::format("{}-op", name);
        auto _name = s7_string(save_string(func_name));
        auto f = make_s7_function(_name, fn);
        set_func(sc, tag, f);
    }

    /* utilities */
    s7_pointer save_string(std::string_view s)
    {
        return s7_make_semipermanent_string(sc, s.data());
    }

    template <typename T>
    s7_int get_type_tag()
    {
        return detail::get_type_tag<T>(sc);
    }

    std::optional<size_t> type_tag_of(std::string_view name)
    {
        auto types = List(this->eval("(*s7* 'c-types)"));
        size_t i = 0;
        for (const auto &p : types) {
            if (name == to<std::string_view>(p)) {
                return i;
            }
            i++;
        }
        return std::nullopt;
    }

    template <typename T>
    const char *type_to_string()
    {
             if constexpr(std::is_same_v<T, s7_pointer>) { return "s7_pointer"; }
        else if constexpr(std::is_same_v<T, double>) { return "real"; }
        else if constexpr(std::is_same_v<T, bool>) { return "boolean"; }
        else if constexpr(std::is_same_v<T, s7_int>
                       || std::is_same_v<T, int>
                       || std::is_same_v<T, short>
                       || std::is_same_v<T, long>) { return "integer"; }
        else if constexpr(std::is_same_v<T, double>) { return "real"; }
        else if constexpr(std::is_same_v<T, const char *>
                       || std::is_same_v<T, std::string_view>) { return "string"; }
        else if constexpr(std::is_same_v<T, unsigned char>) { return "character"; }
        else if constexpr(std::is_same_v<T, std::span<s7_pointer>>) { return "vector"; }
        else if constexpr(std::is_same_v<T, std::span<s7_int>>) { return "int-vector"; }
        else if constexpr(std::is_same_v<T, std::span<double>>) { return "float-vector"; }
        else if constexpr(std::is_same_v<T, std::span<uint8_t>>) { return "byte-vector"; }
        else if constexpr(std::is_pointer_v<T>) { return "c-pointer"; }
        else if constexpr(std::is_same_v<T, List>) { return "list"; }
        else { return detail::get_type_name<T>(sc); }
    }
};

class Variable {
    Scheme *scheme;
    s7_pointer sym;

public:
    Variable(Scheme *scheme, s7_pointer sym) : scheme{scheme}, sym{sym} {}

    template <typename T>
    Variable & operator=(const T &v)
    {
        s7_symbol_set_value(scheme->ptr(), sym, scheme->from<T>(v)); return *this;
    }

    template <typename T> T as()        { return scheme->to<T>(s7_symbol_value(scheme->ptr(), sym)); }
    template <typename T> auto as_opt() { return scheme->to_opt<T>(s7_symbol_value(scheme->ptr(), sym)); }
};

Variable Scheme::operator[](std::string_view name)
{
    auto sym = s7_symbol_table_find_name(this->sc, name.data());
    if (!sym) {
        sym = s7_define_variable(this->sc, name.data(), s7_nil(sc));
    }
    return Variable(this, sym);
}

} // namespace s7
