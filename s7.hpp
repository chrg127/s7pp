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
    template<typename T> struct remove_class { };
    template<typename C, typename R, typename... A> struct remove_class<R(C::*)(A...)>                { using type = R(A...); };
    template<typename C, typename R, typename... A> struct remove_class<R(C::*)(A...) const>          { using type = R(A...); };
    template<typename C, typename R, typename... A> struct remove_class<R(C::*)(A...) volatile>       { using type = R(A...); };
    template<typename C, typename R, typename... A> struct remove_class<R(C::*)(A...) const volatile> { using type = R(A...); };

    template<typename T>
    struct get_signature_impl { using type = typename remove_class<
        decltype(&std::remove_reference<T>::type::operator())>::type; };

    template<typename R, typename... A> struct get_signature_impl<R(A...)>    { using type = R(A...); };
    template<typename R, typename... A> struct get_signature_impl<R(&)(A...)> { using type = R(A...); };
    template<typename R, typename... A> struct get_signature_impl<R(*)(A...)> { using type = R(A...); };
    template<typename T> using get_signature = typename get_signature_impl<T>::type;

    template<typename F> using make_function_type = std::function<get_signature<F>>;
    template<typename F> make_function_type<F> make_function(F &&f) {
        return make_function_type<F>(std::forward<F>(f)); }

    template <typename L>
    struct LambdaTable {
        static inline make_function_type<L> lambda;
    };
}

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

template <typename T>
struct TypeTag {
    static inline s7_int tag = -1;
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

// TODO: get rid of std::string?
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

template <typename T>
const char * type_to_string()
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
    else { return "?"; }
}

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

struct s7 {
    s7_scheme *sc;

    s7() : sc(s7_init()) {}

    ~s7()
    {
        s7_quit(sc);
        s7_free(sc);
    }

    s7(const s7 &) = delete;
    s7 & operator=(const s7 &) = delete;
    s7(s7 &&other) { operator=(std::move(other)); }
    s7 & operator=(s7 &&other) { this->sc = other.sc; other.sc = nullptr; return *this; }

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
        return s7_is_c_object(p) && s7_c_object_type(p) == TypeTag<std::remove_cvref_t<T>>::tag;
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
        else if constexpr(std::is_pointer_v<T>) { return s7_c_pointer(p); }
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
        else if constexpr(std::is_same_v<std::remove_cvref<T>, std::string>) { return s7_make_string_with_length(sc, x.c_str(), x.size()); }
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
            auto tag = TypeTag<std::remove_cvref_t<T>>::tag;
            if (tag != -1) {
                return s7_make_c_object(sc, tag, reinterpret_cast<void *>(new T(x)));
            }
            assert(false && "failed to create s7_pointer from T");
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

    friend struct Variable;

    Variable operator[](std::string_view name);

    /* signatures */
    template <typename T>
    s7_pointer sig_type()
    {
             if constexpr(std::is_same_v<T, s7_pointer>) { return s7_t(sc); }
        else if constexpr(std::is_same_v<T, bool>) { return s7_make_symbol(sc, "boolean?"); }
        else if constexpr(std::is_same_v<T, s7_int>) { return s7_make_symbol(sc, "integer?"); }
        else if constexpr(std::is_same_v<T, double>) { return s7_make_symbol(sc, "real?"); }
        else if constexpr(std::is_same_v<T, const char *>) { return s7_make_symbol(sc, "string?"); }
        else if constexpr(std::is_same_v<T, std::string_view>) { return s7_make_symbol(sc, "string?"); }
        else if constexpr(std::is_same_v<T, char>) { return s7_make_symbol(sc, "character?"); }
        else if constexpr(std::is_same_v<T, std::span<s7_pointer>>) { return s7_make_symbol(sc, "vector?"); }
        else if constexpr(std::is_same_v<T, std::span<s7_int>>) { return s7_make_symbol(sc, "int-vector?"); }
        else if constexpr(std::is_same_v<T, std::span<double>>) { return s7_make_symbol(sc, "float-vector?"); }
        else if constexpr(std::is_same_v<T, std::span<uint8_t>>) { return s7_make_symbol(sc, "byte-vector?"); }
        else if constexpr(std::is_pointer_v<T>) { return s7_make_symbol(sc, "c-pointer?"); }
        else if constexpr(std::is_same_v<T, List>) { return s7_make_symbol(sc, "list?"); }
        else { return s7_make_symbol(sc, "c-object?"); }
    }

    template <typename R, typename... Args>
    s7_pointer make_signature(R (*)(Args...))
    {
        return s7_make_signature(sc, sizeof...(Args) + 2, sig_type<R>(), sig_type<Args>()...);
    }

    template <typename C, typename R, typename... Args>
    s7_pointer make_signature(R (C::*)(Args...) const)
    {
        return s7_make_signature(sc, sizeof...(Args) + 1, sig_type<R>(), sig_type<Args>()...);
    }

    template <typename C, typename R, typename... Args>
    s7_pointer make_signature(R (C::*)(Args...))
    {
        return s7_make_signature(sc, sizeof...(Args) + 1, sig_type<R>(), sig_type<Args>()...);
    }

    /* calling functions */
    template <typename... T>
    s7_pointer call(std::string_view name, T&&... args)
    {
        return s7_call(sc, s7_name_to_value(sc, name.data()), this->list(args...).ptr());
    }

    /* function creation */
    s7_pointer define_function(std::string_view name, std::string_view doc, s7_pointer (*fptr)(s7_scheme *sc, s7_pointer args))
    {
        return s7_define_function(sc, name.data(), fptr, 0, 0, true, doc.data());
    }

    template <typename R, typename... Args>
    s7_pointer define_function(std::string_view name, std::string_view doc, R (*fptr)(Args...))
    {
        constexpr auto NumArgs = sizeof...(Args);

        auto f = [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
            auto this_fn = s7_car(args);
            auto &scheme = *reinterpret_cast<s7 *>(s7_c_pointer(s7_let_ref(sc, this_fn, s7_make_symbol(sc, "interpreter"))));
            auto *fn = reinterpret_cast<R(*)(Args...)>(s7_c_pointer(s7_let_ref(sc, this_fn, s7_make_symbol(sc, "fn-ptr"))));
            auto name = std::string_view(s7_string(s7_let_ref(sc, this_fn, s7_make_symbol(sc, "fn-name"))));
            args = s7_cdr(args);

            auto arglist = List(args);
            std::array<s7_pointer, NumArgs> arr;
            for (std::size_t i = 0; i < NumArgs; i++) {
                arr[i] = arglist.advance();
            }

            auto bools = [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                return std::array { scheme.is<Args>(arr[Is])... };
            }(std::make_index_sequence<NumArgs>());
            auto first_wrong_type = std::find(bools.begin(), bools.end(), false);

            if (first_wrong_type != bools.end()) {
                auto i = first_wrong_type - bools.begin();
                arglist = List(args);
                // auto types = std::array<const char *> { type_to_string<Args>()... };
                return s7_wrong_type_arg_error(sc, name.data(), i+1, arglist[i], "bye");
            }

            auto res = [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                return fn(scheme.to<Args>(arr[Is])...);
            }(std::make_index_sequence<NumArgs>());
            return scheme.from<R>(res);
        };

        auto private_name = name;
        auto let = s7_sublet(sc, s7_rootlet(sc), s7_nil(sc));
        s7_define(sc, let, s7_make_symbol(sc, "interpreter"), s7_make_c_pointer(sc, reinterpret_cast<void *>(this)));
        s7_define(sc, let, s7_make_symbol(sc, "fn-ptr"), s7_make_c_pointer(sc, reinterpret_cast<void *>(fptr)));
        s7_define(sc, let, s7_make_symbol(sc, "fn-name"), s7_make_string(sc, private_name.data()));
        auto p = s7_make_typed_function_with_environment(sc, private_name.data(), f, NumArgs+1, 0, false, doc.data(), make_signature<R, Args...>(), let);
        s7_define_variable(sc, private_name.data(), p);
        return p;
    }

    template <typename C, typename R, typename... Args>
    s7_pointer define_function(std::string_view name, std::string_view doc, R (C::*fptr)(Args...))
    {
        constexpr auto NumArgs = sizeof...(Args);

        auto f = [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
            auto this_fn = s7_car(args);
            auto &scheme = *reinterpret_cast<s7 *>(s7_c_pointer(s7_let_ref(sc, this_fn, s7_make_symbol(sc, "interpreter"))));
            void *fn_ptr = s7_c_pointer(s7_let_ref(sc, this_fn, s7_make_symbol(sc, "fn-ptr")));
            auto name = std::string_view(s7_string(s7_let_ref(sc, this_fn, s7_make_symbol(sc, "fn-name"))));

            using FnPtr = R(C::*)(Args...);
            FnPtr fn = nullptr;
            reinterpret_cast<void *&>(fn) = fn_ptr;

            auto this_obj = s7_cadr(args);
            if (!scheme.is<C>(this_obj)) {
                return s7_wrong_type_arg_error(sc, name.data(), 1, this_obj, "c-object");
            }
            C *obj = reinterpret_cast<C *>(s7_c_object_value(this_obj));

            args = s7_cddr(args);

            auto arglist = List(args);
            std::array<s7_pointer, NumArgs> arr;
            for (std::size_t i = 0; i < NumArgs; i++) {
                arr[i] = arglist.advance();
            }

            auto bools = [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                return std::array { scheme.is<Args>(arr[Is])... };
            }(std::make_index_sequence<NumArgs>());
            auto first_wrong_type = std::find(bools.begin(), bools.end(), false);

            if (first_wrong_type != bools.end()) {
                auto i = first_wrong_type - bools.begin();
                arglist = List(args);
                // auto types = std::array<const char *> { type_to_string<Args>()... };
                return s7_wrong_type_arg_error(sc, name.data(), i+1, arglist[i], "bye");
            }

            auto res = [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                return (obj->*fn)(scheme.to<Args>(arr[Is])...);
            }(std::make_index_sequence<NumArgs>());
            return scheme.from<R>(res);
        };

        auto private_name = name;
        auto let = s7_sublet(sc, s7_rootlet(sc), s7_nil(sc));
        s7_define(sc, let, s7_make_symbol(sc, "interpreter"), s7_make_c_pointer(sc, reinterpret_cast<void *>(this)));
        s7_define(sc, let, s7_make_symbol(sc, "fn-ptr"), s7_make_c_pointer(sc, reinterpret_cast<void *>(reinterpret_cast<void *&>(fptr))));
        s7_define(sc, let, s7_make_symbol(sc, "fn-name"), s7_make_string(sc, private_name.data()));
        auto p = s7_make_typed_function_with_environment(sc, private_name.data(), f, NumArgs+1+1, 0, false, doc.data(), make_signature<R, Args...>(), let);
        s7_define_variable(sc, private_name.data(), p);
        return p;
    }

    template <typename L, typename R, typename... Args>
    s7_function make_f(R (L::*)(Args...) const)
    {
        constexpr auto NumArgs = FunctionTraits<L>::arity;
        return [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
            auto name = std::string_view("caller");
            auto fn = detail::LambdaTable<L>::lambda;
            auto &scheme = *reinterpret_cast<s7 *>(sc);

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
                auto types = std::array<const char *, NumArgs> { type_to_string<Args>()... };
                return s7_wrong_type_arg_error(sc, name.data(), i+1, arglist[i], types[i]);
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
    s7_function make_f(R (L::*)(Args...))
    {
        constexpr auto NumArgs = FunctionTraits<L>::arity;
        return [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
            auto name = std::string_view("caller");
            auto fn = detail::LambdaTable<L>::lambda;
            auto &scheme = *reinterpret_cast<s7 *>(sc);

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
                auto types = std::array<const char *, NumArgs> { type_to_string<Args>()... };
                return s7_wrong_type_arg_error(sc, name.data(), i+1, arglist[i], types[i]);
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

    template <typename L>
    s7_pointer define_function(std::string_view name, std::string_view doc, L&& lambda)
    {
        constexpr auto NumArgs = FunctionTraits<L>::arity;
        detail::LambdaTable<std::remove_cvref_t<L>>::lambda = lambda;
        auto f = make_f(&std::remove_cvref_t<L>::operator());
        auto let = s7_sublet(sc, s7_rootlet(sc), s7_nil(sc));
        auto p = s7_make_typed_function_with_environment(sc, name.data(), f, NumArgs, 0, false, doc.data(), make_signature(&std::remove_cvref_t<L>::operator()), let);
        s7_define_variable(sc, name.data(), p);
        return p;
    }

    /* usertypes */
    template <typename T>
    void make_c_type(std::string_view name)
    {
        auto tag = s7_make_c_type(sc, name.data());
        TypeTag<T>::tag = tag;

        if constexpr(requires { T(sc); }) {
            auto ctor = [](s7_scheme *sc, s7_pointer) -> s7_pointer {
                // s7 *scheme = reinterpret_cast<s7 *>(&sc);
                return s7_make_c_object(sc, TypeTag<T>::tag, reinterpret_cast<void *>(new T(sc)));
            };
            auto ctor_name = std::string("make-") + std::string(name);
            auto doc = std::string("(") + ctor_name + ") creates a new " + std::string(name);
            s7_define_function(sc, ctor_name.c_str(), ctor, 0, 0, false, doc.c_str());
        }

        if constexpr(requires(T t) { t.gc_mark(*this); }) {
            s7_c_type_set_gc_mark(sc, tag, [](s7_scheme *sc, s7_pointer obj) -> s7_pointer {
                s7 *scheme = reinterpret_cast<s7 *>(&sc);
                T *o = reinterpret_cast<T *>(obj);
                o->gc_mark(*scheme);
                return nullptr;
            });
        }

        if constexpr(requires(T t) { t.gc_free(*this); }) {
            s7_c_type_set_gc_free(sc, tag, [](s7_scheme *sc, s7_pointer obj) -> s7_pointer {
                s7 *scheme = reinterpret_cast<s7 *>(&sc);
                T *o = reinterpret_cast<T *>(obj);
                o->gc_free(*scheme);
                delete o;
                return nullptr;
            });
        } else {
            s7_c_type_set_gc_free(sc, tag, [](s7_scheme *, s7_pointer obj) -> s7_pointer {
                T *o = reinterpret_cast<T *>(obj);
                delete o;
                return nullptr;
            });
        }

        if constexpr(requires(T a, T b) { a == b; }) {
            s7_c_type_set_is_equal(sc, tag, [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
                s7_pointer a = s7_car(args);
                s7_pointer b = s7_cadr(args);
                if (a == b) {
                    return s7_t(sc);
                }
                auto tag = TypeTag<T>::tag;
                if (!s7_is_c_object(b) || (s7_c_object_type(b) != tag)) {
                    return s7_f(sc);
                }
                T *pa = (T *) s7_c_object_value(a);
                T *pb = (T *) s7_c_object_value(b);
                return s7_make_boolean(sc, *pa == *pb);
            });
        }

        if constexpr(requires(T t) { t.to_string(*this); }) {
            s7_c_type_set_to_string(sc, tag, [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
                s7 *scheme = reinterpret_cast<s7 *>(&sc);
                return s7_make_string(sc, reinterpret_cast<T *>(s7_c_object_value(s7_car(args)))->to_string(*scheme).c_str());
            });
        }

        auto is_name = std::string(name) + "?";
        auto is_doc  = std::string("(") + is_name + " x) checks if x is a " + std::string(name);
        auto is = [](s7_scheme *sc, s7_pointer args) {
            auto &scheme = *reinterpret_cast<s7 *>(&sc);
            return scheme.from<bool>(scheme.is<T>(s7_car(args)));
        };
        s7_define_function(sc, is_name.c_str(), is, 0, 0, false, is_doc.c_str());
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
};

struct Variable {
    s7 *scheme;
    s7_pointer sym;

    template <typename T>
    Variable & operator=(const T &v)
    {
        s7_symbol_set_value(scheme->sc, sym, scheme->from<T>(v)); return *this;
    }

    template <OutputType T>
    T as()
    {
        return scheme->to<T>(s7_symbol_value(scheme->sc, sym));
    }

    template <OutputType T>
    auto as_opt()
    {
        return scheme->to_opt<T>(s7_symbol_value(scheme->sc, sym));
    }
};

Variable s7::operator[](std::string_view name)
{
    auto sym = s7_symbol_table_find_name(this->sc, name.data());
    if (!sym) {
        sym = s7_define_variable(this->sc, name.data(), s7_nil(sc));
    }
    return Variable(this, sym);
}

} // namespace s7
