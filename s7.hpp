#ifndef S7_HPP_INCLUDED
#define S7_HPP_INCLUDED

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
#include <unordered_set>
#include <optional>
#include <utility>
#include <array>
#include "function_traits.hpp"
#include "s7/s7.h"

#define FWD(x) std::forward<decltype(x)>(x)

namespace s7 {

class List {
    s7_pointer p;

public:
    explicit List(s7_pointer p) : p{p} {}

    s7_pointer operator[](std::size_t i) const
    {
        s7_pointer x = p;
        while (i-- > 0) {
            x = s7_cdr(x);
        }
        return s7_car(x);
    }

    s7_pointer car() const { return s7_car(p); }
    List cdr() const       { return List(s7_cdr(p)); }
    s7_pointer ptr() const { return p; }
    bool at_end()          { return !s7_is_pair(p); }
    s7_pointer advance()   { auto tmp = s7_car(p); p = s7_cdr(p); return tmp; }

    std::size_t size() const
    {
        size_t s = 0;
        for (auto x = p; s7_is_pair(x); x = s7_cdr(x), s++)
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

        bool operator==(const iterator &i) const
        {
            auto is_nil   =   p == nullptr || !s7_is_pair(p);
            auto i_is_nil = i.p == nullptr || !s7_is_pair(i.p);
            return (is_nil && i_is_nil) || (!is_nil && !i_is_nil && s7_is_eq(p, i.p));
        }
    };

    iterator begin() { return iterator(p); }
    iterator end() { return iterator(); }
};

struct Values {
    s7_pointer p;
};

enum class Type {
    Any, Undefined, Unspecified, Nil, Eof, Let, OpenLet,
    Boolean, Integer, Real, String, Character, Ratio, Complex,
    Vector, IntVector, FloatVector, ByteVector, ComplexVector,
    List, CPointer, CObject, RandomState, HashTable,
    InputPort, OutputPort, Syntax, Symbol, Keyword,
    Procedure, Macro, Dilambda, Values, Iterator,
    BigNum,
    Unknown,
};

std::string_view scheme_type_to_string(Type t)
{
    switch (t) {
    case Type::Any:           { return "s7_pointer";      }
    case Type::Undefined:     { return "undefined";       }
    case Type::Unspecified:   { return "unspecified";     }
    case Type::Nil:           { return "null";            }
    case Type::Eof:           { return "eof-object";      }
    case Type::Let:           { return "let";             }
    case Type::OpenLet:       { return "openlet";         }
    case Type::Boolean:       { return "boolean";         }
    case Type::Integer:       { return "integer";         }
    case Type::Real:          { return "real";            }
    case Type::String:        { return "string";          }
    case Type::Character:     { return "char";            }
    case Type::Ratio:         { return "rational";        }
    case Type::Complex:       { return "complex";         }
    case Type::Vector:        { return "vector";          }
    case Type::IntVector:     { return "int-vector";      }
    case Type::FloatVector:   { return "float-vector";    }
    case Type::ByteVector:    { return "byte-vector";     }
    case Type::ComplexVector: { return "complex-vector";  }
    case Type::List:          { return "list";            }
    case Type::CPointer:      { return "c-pointer";       }
    case Type::CObject:       { return "c-object";        }
    case Type::RandomState:   { return "random-state";    }
    case Type::HashTable:     { return "hash-table";      }
    case Type::InputPort:     { return "input-port";      }
    case Type::OutputPort:    { return "output-port";     }
    case Type::Syntax:        { return "syntax";          }
    case Type::Symbol:        { return "symbol";          }
    case Type::Keyword:       { return "keyword";         }
    case Type::Procedure:     { return "procedure";       }
    case Type::Macro:         { return "macro";           }
    case Type::Dilambda:      { return "dilambda";        }
    case Type::Values:        { return "values";          }
    case Type::Iterator:      { return "iterator";        }
    case Type::BigNum:        { return "bignum";          }
    default:
    case Type::Unknown:       { return "unknown";         }
    }
}

template <typename Tp, bool Output = false>
Type scheme_type()
{
    using T = std::decay_t<std::remove_cvref_t<Tp>>;
         if constexpr(std::is_same_v<T, s7_pointer>)            { return Type::Any;         }
    else if constexpr(std::is_same_v<T, void>)                  { return Type::Unspecified; }
    else if constexpr(std::is_same_v<T, bool>)                  { return Type::Boolean;     }
    else if constexpr(std::is_same_v<T, s7_int>)                { return Type::Integer;     }
    else if constexpr(std::is_same_v<T, double>)                { return Type::Real;        }
    else if constexpr(std::is_same_v<T, const char *>
                   || std::is_same_v<T, std::string_view>)      { return Type::String;      }
    else if constexpr(std::is_same_v<T, unsigned char>)         { return Type::Character;   }
    else if constexpr(std::is_same_v<T, std::span<s7_pointer>>) { return Type::Vector;      }
    else if constexpr(std::is_same_v<T, std::span<s7_int>>)     { return Type::IntVector;   }
    else if constexpr(std::is_same_v<T, std::span<double>>)     { return Type::FloatVector; }
    else if constexpr(std::is_same_v<T, std::span<uint8_t>>)    { return Type::ByteVector;  }
    else if constexpr(std::is_pointer_v<T>)                     { return Type::CPointer;    }
    else if constexpr(std::is_same_v<T, List>)                  { return Type::List;        }
    // types that should only be in function return
    else if constexpr(Output &&
                     (std::is_same_v<T, int> || std::is_same_v<T, short> || std::is_same_v<T, long>))   { return Type::Integer; }
    else if constexpr(Output && std::is_same_v<T, float>)                                               { return Type::Real; }
    else if constexpr(Output &&
                     (std::is_same_v<T, std::vector<s7_int>>
                   || std::is_same_v<T, std::span<int>>    || std::is_same_v<T, std::vector<int>>
                   || std::is_same_v<T, std::span<short>>  || std::is_same_v<T, std::vector<short>>
                   || std::is_same_v<T, std::span<long>>   || std::is_same_v<T, std::vector<long>>))    { return Type::IntVector;   }
    else if constexpr(Output &&
                     (std::is_same_v<T, std::vector<double>>
                   || std::is_same_v<T, std::span<float>>  || std::is_same_v<T, std::vector<float>>))   { return Type::FloatVector; }
    else if constexpr(Output && std::is_same_v<T, std::vector<uint8_t>>)                                { return Type::ByteVector;  }
    else if constexpr(Output && std::is_same_v<T, Values>)                                              { return Type::Values;      }
    // anything else
    else                                                                                                { return Type::CObject;     }
}

template <typename T>
Type scheme_output_type()
{
         if constexpr(std::is_same_v<T, s7_pointer>)                                                    { return Type::Any;         }
    else if constexpr(std::is_same_v<T, bool>)                                                          { return Type::Boolean;     }
    else if constexpr(std::is_same_v<T, s7_int> || std::is_same_v<T, int>
                   || std::is_same_v<T, short>  || std::is_same_v<T, long>)                             { return Type::Integer;     }
    else if constexpr(std::is_same_v<T, double> || std::is_same_v<T, float>)                            { return Type::Real;        }
    else if constexpr(std::is_same_v<std::decay_t<std::remove_cvref_t<T>>, char *>)                     { return Type::String;      }
    else if constexpr(std::is_same_v<std::remove_cvref_t<T>, std::string>)                              { return Type::String;      }
    else if constexpr(std::is_same_v<T, unsigned char>)                                                 { return Type::Character;   }
    else if constexpr(std::is_same_v<T, std::span<s7_pointer>>
                   || std::is_same_v<T, std::vector<s7_pointer>>)                                       { return Type::Vector;      }
    else if constexpr(std::is_same_v<T, std::span<s7_int>> || std::is_same_v<T, std::vector<s7_int>>
                   || std::is_same_v<T, std::span<int>>    || std::is_same_v<T, std::vector<int>>
                   || std::is_same_v<T, std::span<short>>  || std::is_same_v<T, std::vector<short>>
                   || std::is_same_v<T, std::span<long>>   || std::is_same_v<T, std::vector<long>>)     { return Type::IntVector;   }
    else if constexpr(std::is_same_v<T, std::span<double>> || std::is_same_v<T, std::vector<double>>
                   || std::is_same_v<T, std::span<float>>  || std::is_same_v<T, std::vector<float>>)    { return Type::FloatVector; }
    else if constexpr(std::is_same_v<T, std::span<uint8_t>> || std::is_same_v<T, std::vector<uint8_t>>) { return Type::ByteVector;  }
    else if constexpr(std::is_pointer_v<T>)                                                             { return Type::CPointer;    }
    else if constexpr(std::is_same_v<T, List>)                                                          { return Type::List;        }
    else                                                                                                { return Type::CObject;     }
}

namespace detail {
    template <typename L>
    struct LambdaTable {
        static inline std::function<typename FunctionTraits<L>::Signature> lambda;
        static inline std::unordered_map<uintptr_t, const char *> name;
    };

    template <typename T>
    struct TypeTag {
        static inline std::unordered_map<uintptr_t, s7_int> tag;
        static inline std::unordered_map<uintptr_t, s7_pointer> let;
    };

    template <typename T>
    s7_int get_type_tag(s7_scheme *sc)
    {
        auto &m = TypeTag<std::remove_cvref_t<T>>::tag;
        auto it = m.find(reinterpret_cast<uintptr_t>(sc));
        assert(it != m.end() && "missing tag for T");
        return it->second;
    }

    template <typename T>
    std::string_view get_type_name(s7_scheme *sc)
    {
        auto tag = get_type_tag<T>(sc);
        auto ctypes = s7_let_field_ref(sc, s7_make_symbol(sc, "c-types"));
        auto name = s7_list_ref(sc, ctypes, tag);
        return std::string_view(s7_string(name), s7_string_length(name));
    }

    template <typename R, typename... Args>
    auto as_lambda(R (*fptr)(Args...))
    {
        return [=](Args... args) -> R { return fptr(args...); };
    }

    template <typename C, typename R, typename... Args>
    auto as_lambda(R (C::*fptr)(Args...))
    {
        return [=](C &c, Args... args) -> R { return ((&c)->*fptr)(args...); };
    }

    template <typename C, typename R, typename... Args>
    auto as_lambda(R (C::*fptr)(Args...) const)
    {
        return [=](const C &c, Args... args) -> R { return ((&c)->*fptr)(args...); };
    }

    template <typename F>
    auto as_lambda(F &&f)
    {
        return f;
    }

    template <typename T>
    bool is(s7_scheme *sc, s7_pointer p)
    {
             if constexpr(std::is_same_v<T, s7_pointer>)            { return p;                     }
        else if constexpr(std::is_same_v<T, bool>)                  { return s7_is_boolean(p);      }
        else if constexpr(std::is_same_v<T, s7_int>)                { return s7_is_integer(p);      }
        else if constexpr(std::is_same_v<T, double>)                { return s7_is_real(p);         }
        else if constexpr(std::is_same_v<T, const char *>
                       || std::is_same_v<T, std::string_view>)      { return s7_is_string(p);       }
        else if constexpr(std::is_same_v<T, unsigned char>)         { return s7_is_character(p);    }
        else if constexpr(std::is_same_v<T, std::span<s7_pointer>>) { return s7_is_vector(p);       }
        else if constexpr(std::is_same_v<T, std::span<s7_int>>)     { return s7_is_int_vector(p);   }
        else if constexpr(std::is_same_v<T, std::span<double>>)     { return s7_is_float_vector(p); }
        else if constexpr(std::is_same_v<T, std::span<uint8_t>>)    { return s7_is_byte_vector(p);  }
        else if constexpr(std::is_pointer_v<T>)                     { return s7_is_c_pointer(p);    }
        else if constexpr(std::is_same_v<T, List>)                  { return s7_is_pair(p);         }
        return s7_is_c_object(p) && s7_c_object_type(p) == detail::get_type_tag<T>(sc);
    }

    template <typename T>
    T to(s7_scheme *sc, s7_pointer p)
    {
        assert(is<T>(sc, p) && "p isn't an object of type T");
             if constexpr(std::is_same_v<T, s7_pointer>)            { return p;                                                                 }
        else if constexpr(std::is_same_v<T, bool>)                  { return s7_boolean(sc, p);                                                 }
        else if constexpr(std::is_same_v<T, s7_int>)                { return s7_integer(p);                                                     }
        else if constexpr(std::is_same_v<T, double>)                { return s7_real(p);                                                        }
        else if constexpr(std::is_same_v<T, const char *>)          { return s7_string(p);                                                      }
        else if constexpr(std::is_same_v<T, std::string_view>)      { return std::string_view(s7_string(p));                                    }
        else if constexpr(std::is_same_v<T, char>)                  { return static_cast<char>(s7_character(p));                                }
        else if constexpr(std::is_same_v<T, std::span<s7_pointer>>) { return std::span(s7_vector_elements(p), s7_vector_length(p));             }
        else if constexpr(std::is_same_v<T, std::span<s7_int>>)     { return std::span(s7_int_vector_elements(p), s7_vector_length(p));         }
        else if constexpr(std::is_same_v<T, std::span<double>>)     { return std::span(s7_float_vector_elements(p), s7_vector_length(p));       }
        else if constexpr(std::is_same_v<T, std::span<uint8_t>>)    { return std::span(s7_byte_vector_elements(p), s7_vector_length(p));        }
        else if constexpr(std::is_pointer_v<T>)                     { return reinterpret_cast<T>(s7_c_pointer(p));                              }
        else if constexpr(std::is_same_v<T, List>)                  { return List(p);                                                           }
        else                                                        { return *reinterpret_cast<std::remove_cvref_t<T> *>(s7_c_object_value(p)); }
    }

    template <typename T, bool OutputType = false>
    std::string_view type_to_string(s7_scheme *sc)
    {
        Type t = scheme_type<T, OutputType>();
        return t != Type::CObject ? scheme_type_to_string(t) : detail::get_type_name<T>(sc);
    }
} // namespace detail

template <typename T>
struct VarArgs {
    s7_scheme *sc;
    s7_pointer p;
    const char *caller;
    s7_int arg_n;

public:
    VarArgs(s7_scheme *sc, s7_pointer p, const char *caller)
        : sc(sc), p(p), caller(caller), arg_n(1) {}

    VarArgs(s7_scheme *sc, s7_pointer p, const char *caller, s7_int arg_n)
        : sc(sc), p(p), caller(caller), arg_n(arg_n) {}

    s7_pointer operator[](std::size_t i) const
    {
        s7_pointer x = p;
        while (i-- > 0) {
            x = s7_cdr(x);
        }
        return s7_car(x);
    }

    T car() const
    {
        if constexpr(std::is_same_v<T, s7_pointer>) {
            return s7_car(p);
        } else {
            auto r = s7_car(p);
            if (!detail::is<T>(sc, r)) {
                // this is actually fine, since s7_wrong_type_arg_error is a
                // noreturn function (despite not marked as such)
                return detail::to<T>(sc, s7_wrong_type_arg_error(sc, caller, arg_n, r, detail::type_to_string<T>(sc).data()));
            }
            return detail::to<T>(sc, s7_car(p));
        }
    }

    VarArgs cdr() const    { return VarArgs(sc, s7_cdr(p), caller, arg_n+1); }
    s7_pointer ptr() const { return p; }
    bool at_end()          { return !s7_is_pair(p); }
    T advance()            { auto tmp = car(); p = s7_cdr(p); return tmp; }

    std::size_t size() const
    {
        size_t s = 0;
        for (auto x = p; s7_is_pair(x); x = s7_cdr(x), s++)
            ;
        return s;
    }

    struct iterator {
        using value_type = T;

        VarArgs va = VarArgs(nullptr, nullptr, nullptr);

        iterator() = default;
        explicit iterator(VarArgs va) : va(va) {}
        iterator & operator++() { va = va.cdr(); return *this; }
        iterator operator++(int) { auto i = *this; va = va.cdr(); return i; }
        T operator*() const { return va.car(); }

        bool operator==(const iterator &i) const
        {
            auto is_nil   =   va.p == nullptr || !s7_is_pair(va.p);
            auto i_is_nil = i.va.p == nullptr || !s7_is_pair(i.va.p);
            return (is_nil && i_is_nil) || (!is_nil && !i_is_nil && s7_is_eq(va.p, i.va.p));
        }
    };

    iterator begin() { return iterator(*this); }
    iterator end() { return iterator(); }
};

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
    bool unsafe_body = false;
    bool unsafe_arglist = false;
};

enum class Op {
    Equal, Equivalent, Copy, Fill, Reverse, GcMark, GcFree,
    Length, ToString, ToList, Ref, Set,
};

enum class MethodOp {
    Add, Sub, Mul, Div
};

template <MethodOp op>
constexpr std::string_view method_op_fn()
{
    if constexpr(op == MethodOp::Add) { return "+"; }
    if constexpr(op == MethodOp::Sub) { return "-"; }
    if constexpr(op == MethodOp::Mul) { return "*"; }
    if constexpr(op == MethodOp::Div) { return "/"; }
}

template <typename... Fns>
struct Overload {
    std::tuple<Fns...> fns;
    explicit Overload(Fns&&... fns) : fns(fns...) {}
};

template <typename... Fns>
struct Constructors {
    std::string_view name = "";
    Overload<Fns...> overload;

    explicit Constructors(Fns&&... fns) : name(""), overload(std::forward<Fns>(fns)...) {}
    explicit Constructors(std::string_view name, Fns&&... fns) : name(name), overload(std::forward<Fns>(fns)...) {}
};

template             <typename R, typename... Args> inline constexpr auto resolve(R (*f)(Args...))          { return f; }
template <typename C, typename R, typename... Args> inline constexpr auto resolve(R (C::*f)(Args...))       { return f; }
template <typename C, typename R, typename... Args> inline constexpr auto resolve(R (C::*f)(Args...) const) { return f; }

struct Variable;

class Scheme {
    s7_scheme *sc;
    // NOTE: any following field can't be accessed inside non-capturing lambdas
    std::unordered_set<MethodOp> substitured_ops;

    template <typename L, typename R, typename... Args>
    s7_pointer _call_fn_impl(s7_pointer args)
    {
        constexpr auto NumArgs = FunctionTraits<L>::arity;
        auto arglist = List(args);
        std::array<s7_pointer, NumArgs> arr;
        for (std::size_t i = 0; i < NumArgs; i++) {
            arr[i] = arglist.advance();
        }

        auto bools = [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            return std::array<bool, NumArgs> { is<Args>(arr[Is])...  };
        }(std::make_index_sequence<NumArgs>());
        auto first_wrong_type = std::find(bools.begin(), bools.end(), false);

        if (first_wrong_type != bools.end()) {
            auto i = first_wrong_type - bools.begin();
            arglist = List(args);
            auto types = std::array<const char *, NumArgs> {
                type_to_string<Args>().data()...
            };
            auto name = detail::LambdaTable<L>::name
                .find(reinterpret_cast<uintptr_t>(sc))->second;
            return s7_wrong_type_arg_error(sc, name, i+1, arglist[i], types[i]);
        }

        auto &fn = detail::LambdaTable<L>::lambda;
        if constexpr(std::is_same_v<R, void>) {
            [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                fn(to<Args>(arr[Is])...);
            }(std::make_index_sequence<NumArgs>());
            return s7_unspecified(sc);
        } else {
            return from<R>([&]<std::size_t... Is>(std::index_sequence<Is...>) {
                return fn(to<Args>(arr[Is])...);
            }(std::make_index_sequence<NumArgs>()));
        }
    }

    // called from call_fn and match_fn
    // varargs always matches... is it an error to allow it in ctors?
    template <typename L, typename R, typename T>
    s7_pointer _call_varargs_fn(s7_pointer args)
    {
        auto &fn = detail::LambdaTable<L>::lambda;
        auto name = detail::LambdaTable<L>::name
            .find(reinterpret_cast<uintptr_t>(sc))->second;
        if constexpr(std::is_same_v<R, void>) {
            fn(VarArgs(sc, args, name));
            return unspecified();
        } else {
            return from<R>(fn(VarArgs<T>(sc, args, name)));
        }
    }

    template <typename L, typename R, typename... Args>
    s7_pointer call_fn(R (L::*)(Args...) const, s7_pointer args)
    {
        return _call_fn_impl<L, R, Args...>(args);
    }

    template <typename L, typename R, typename... Args>
    s7_pointer call_fn(R (L::*)(Args...), s7_pointer args)
    {
        return _call_fn_impl<L, R, Args...>(args);
    }

    template <typename L, typename R, typename T>
    s7_pointer call_fn(R (L::*)(VarArgs<T>) const, s7_pointer args)
    {
        return _call_varargs_fn<L, R, T>(args);
    }

    template <typename L, typename R, typename T>
    s7_pointer call_fn(R (L::*)(VarArgs<T>), s7_pointer args)
    {
        return _call_varargs_fn<L, R, T>(args);
    }

    template <typename F>
    s7_function make_s7_function(const char *name, F &&fn)
    {
        auto lambda = detail::as_lambda(fn);
        using Lambda = std::remove_cvref_t<decltype(lambda)>;
        detail::LambdaTable<Lambda>::lambda = lambda;
        detail::LambdaTable<Lambda>::name
            .insert_or_assign(reinterpret_cast<uintptr_t>(sc), name);
        return [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
            auto &scheme = *reinterpret_cast<Scheme *>(&sc);
            return scheme.call_fn(&Lambda::operator(), args);
        };
    }

    template <typename F, typename R, typename... Args>
    s7_pointer match_fn(s7_int length, s7_pointer args)
    {
        constexpr auto NumArgs = FunctionTraits<F>::arity;
        if (length != NumArgs) {
            return nullptr;
        }

        auto arglist = s7::List(args);
        std::array<s7_pointer, NumArgs> arr;
        for (std::size_t i = 0; i < NumArgs; i++) {
            arr[i] = arglist.advance();
        }

        auto bools = [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            return std::array<bool, NumArgs> { is<Args>(arr[Is])...  };
        }(std::make_index_sequence<NumArgs>());
        auto matches = std::find(bools.begin(), bools.end(), false) == bools.end();
        if (!matches) {
            return nullptr;
        }

        auto &fn = detail::LambdaTable<std::remove_cvref_t<F>>::lambda;
        if constexpr(std::is_same_v<R, void>) {
            [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                fn(to<Args>(arr[Is])...);
            }(std::make_index_sequence<NumArgs>());
            return s7_unspecified(sc);
        } else {
            return from<R>([&]<std::size_t... Is>(std::index_sequence<Is...>) {
                return fn(to<Args>(arr[Is])...);
            }(std::make_index_sequence<NumArgs>()));
        }
    }

    template <typename F, typename R, typename... Args>
    s7_pointer match_fn(s7_int length, s7_pointer args, R (F::*)(Args...))
    {
        return match_fn<F, R, Args...>(length, args);
    }

    template <typename F, typename R, typename... Args>
    s7_pointer match_fn(s7_int length, s7_pointer args, R (F::*)(Args...) const)
    {
        return match_fn<F, R, Args...>(length, args);
    }

    template <typename F, typename R, typename T>
    s7_pointer match_fn(s7_int, s7_pointer args, R (F::*)(VarArgs<T>))
    {
        return _call_varargs_fn<F, R, T>(args);
    }

    template <typename F, typename R, typename T>
    s7_pointer match_fn(s7_int, s7_pointer args, R (F::*)(VarArgs<T>) const)
    {
        return _call_varargs_fn<F, R, T>(args);
    }

    template <typename... Fns>
    s7_function _make_overload(Fns&&... fns)
    {
        constexpr auto NumFns = sizeof...(Fns);
        auto set = []<typename F>(F &&fn) {
            detail::LambdaTable<std::remove_cvref_t<F>>::lambda = fn;
        };
        (set(fns), ...);

        return [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
            auto &scheme = *reinterpret_cast<Scheme *>(&sc);
            auto length = s7_list_length(sc, args);
            auto results = std::array<s7_pointer, NumFns> {
                scheme.match_fn<Fns>(length, args, &Fns::operator())...
            };

            auto make_message = [&](int n) -> s7_pointer {
                auto str = std::string("arglist ~a doesn't match any signature for this function\n"
                                       ";valid signatures:");
                for (auto i = 0; i < n; i++) {
                    str += "\n;~a";
                }
                return scheme.from(str);
            };

            auto it = std::find_if(results.begin(), results.end(),
                [](s7_pointer p) { return p != nullptr; });
            if (it != results.end()) {
                return *it;
            }

            std::vector<s7_pointer> types;
            for (auto arg : s7::List(args)) {
                types.push_back(s7_make_symbol(sc, scheme_type_to_string(scheme.type_of(arg)).data()));
            }
            return s7_error(sc, s7_make_symbol(sc, "no-overload-match"), scheme.list(
                make_message(NumFns),
                s7_array_to_list(sc, types.size(), types.data()),
                scheme.make_signature(&std::remove_cvref_t<Fns>::operator())...
            ).ptr());
        };
    }

    template <MethodOp op>
    auto make_method_op_function()
    {
        auto old = s7_name_to_value(sc, method_op_fn<op>().data());
        return [this, old](VarArgs<s7_pointer> args) -> s7_pointer {
            constexpr auto Name = method_op_fn<op>();
            if (args.size() == 0) {
                return call(old, nil());
            } else if (args.size() == 0) {
                auto p = args.car();
                if (s7_is_c_object(p)) {
                    auto method = s7_method(sc, s7_c_object_let(p), s7_make_symbol(sc, Name.data()));
                    if (method == s7_undefined(sc)) {
                        s7_wrong_type_arg_error(sc, Name.data(), 0, p, "a c-object that defines +");
                    }
                    return call(method, p);
                } else {
                    return call(old, p);
                }
            }
            auto res = args.advance();
            for (auto arg : args) {
                if (s7_is_c_object(res) || s7_is_c_object(arg)) {
                    auto p = s7_is_c_object(res) ? res : arg;
                    auto method = s7_method(sc, s7_c_object_let(p), s7_make_symbol(sc, Name.data()));
                    if (method == s7_undefined(sc)) {
                        s7_wrong_type_arg_error(sc, Name.data(), 0, res, "a c-object that defines +");
                    }
                    res = call(method, res, arg);
                } else {
                    res = call(old, res, arg);
                }
            }
            return res;
        };
    }

public:
    Scheme() : sc(s7_init()) {}

    ~Scheme()
    {
        s7_quit(sc);
        s7_free(sc);
    }

    Scheme(const Scheme &) = delete;
    Scheme & operator=(const Scheme &) = delete;
    Scheme(Scheme &&other) { operator=(std::move(other)); }
    Scheme & operator=(Scheme &&other) { sc = other.sc; other.sc = nullptr; return *this; }

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
    void protect(s7_pointer p) { s7_gc_protect(sc, p); }
    void mark(s7_pointer p)    { s7_mark(p); }

    /* constants */
    s7_pointer nil()         { return s7_nil(sc); }
    s7_pointer undefined()   { return s7_undefined(sc); }
    s7_pointer unspecified() { return s7_unspecified(sc); }
    s7_pointer eof()          { return s7_eof_object(sc); }

    /* functions for inspecting and converting from/to scheme objects */
    template <typename T> bool is(s7_pointer p) { return s7::detail::is<T>(sc, p); }
    template <typename T> T to(s7_pointer p)    { return s7::detail::to<T>(sc, p); }

    template <typename T>
    std::optional<T> to_opt(s7_pointer p)
    {
        if (!detail::is<T>(sc, p)) {
            return std::nullopt;
        }
        return detail::to<T>(sc, p);
    }

    template <typename T>
    s7_pointer from(const T &x)
    {
        using Type = std::remove_cvref_t<T>;
             if constexpr(std::is_same_v<Type, s7_pointer>)                                      { return x;                                                   }
        else if constexpr(std::is_same_v<Type, bool>)                                            { return s7_make_boolean(sc, x);                              }
        else if constexpr(std::is_same_v<Type, s7_int> || std::is_same_v<Type, int>
                       || std::is_same_v<Type, short> || std::is_same_v<Type, long>)                { return s7_make_integer(sc, x);                              }
        else if constexpr(std::is_same_v<Type, double> || std::is_same_v<Type, float>)              { return s7_make_real(sc, x);                                 }
        else if constexpr(std::is_same_v<std::remove_cvref_t<std::decay_t<Type>>, char *>)       { return s7_make_string(sc, x);                               }
        else if constexpr(std::is_same_v<std::remove_cvref_t<std::decay_t<Type>>, const char *>) { return s7_make_string(sc, x);                               }
        else if constexpr(std::is_same_v<Type, std::string>)                { return s7_make_string_with_length(sc, x.c_str(), x.size()); }
        else if constexpr(std::is_same_v<Type, std::string_view>)                                { return s7_make_string_with_length(sc, x.data(), x.size());  }
        else if constexpr(std::is_same_v<Type, unsigned char>)                                   { return s7_make_character(sc, x);                            }
        else if constexpr(std::is_pointer_v<Type>)                                               { return s7_make_c_pointer(sc, x);                            }
        else if constexpr(std::is_same_v<Type, List>)                                            { return x.ptr();                                             }
        else if constexpr(std::is_same_v<Type, Values>)                                          { return x.p;                                                 }
        else if constexpr(std::is_same_v<Type, std::span<s7_pointer>> || std::is_same_v<Type, std::vector<s7_pointer>>) {
            auto vec = s7_make_vector(sc, x.size());
            for (size_t i = 0; i < x.size(); i++) {
                s7_vector_set(sc, vec, i, x[i]);
            }
            return vec;
        } else if constexpr(std::is_same_v<Type, std::span<s7_int>> || std::is_same_v<Type, std::vector<s7_int>>
                         || std::is_same_v<Type, std::span<int>>    || std::is_same_v<Type, std::vector<int>>
                         || std::is_same_v<Type, std::span<short>>  || std::is_same_v<Type, std::vector<short>>
                         || std::is_same_v<Type, std::span<long>>   || std::is_same_v<Type, std::vector<long>>) {
            auto vec = s7_make_int_vector(sc, x.size(), 1, nullptr);
            for (size_t i = 0; i < x.size(); i++) {
                s7_int_vector_set(vec, i, x[i]);
            }
            return vec;
        } else if constexpr(std::is_same_v<Type, std::span<double>> || std::is_same_v<Type, std::vector<double>>
                         || std::is_same_v<Type, std::span<float>>  || std::is_same_v<Type, std::vector<float>>) {
            auto vec = s7_make_float_vector(sc, x.size(), 1, nullptr);
            for (size_t i = 0; i < x.size(); i++) {
                s7_float_vector_set(vec, i, x[i]);
            }
            return vec;
        } else if constexpr(std::is_same_v<Type, std::span<uint8_t>> || std::is_same_v<Type, std::vector<uint8_t>>) {
            auto vec = s7_make_byte_vector(sc, x.size(), 1, nullptr);
            for (size_t i = 0; i < x.size(); i++) {
                s7_byte_vector_set(vec, i, x[i]);
            }
            return vec;
        } else {
            using Type = std::remove_cvref_t<Type>;
            return make_c_object(new Type(x));
        }
    }

    template <typename T>
    s7_pointer from(T &&x)
    {
        using Type = std::remove_cvref_t<T>;
             if constexpr(std::is_same_v<Type, s7_pointer>)                                      { return x;                                                   }
        else if constexpr(std::is_same_v<Type, bool>)                                            { return s7_make_boolean(sc, x);                              }
        else if constexpr(std::is_same_v<Type, s7_int> || std::is_same_v<Type, int>
                       || std::is_same_v<Type, short> || std::is_same_v<Type, long>)                { return s7_make_integer(sc, x);                              }
        else if constexpr(std::is_same_v<Type, double> || std::is_same_v<Type, float>)              { return s7_make_real(sc, x);                                 }
        else if constexpr(std::is_same_v<std::remove_cvref_t<std::decay_t<Type>>, char *>)       { return s7_make_string(sc, x);                               }
        else if constexpr(std::is_same_v<std::remove_cvref_t<std::decay_t<Type>>, const char *>) { return s7_make_string(sc, x);                               }
        else if constexpr(std::is_same_v<Type, std::string>)                { return s7_make_string_with_length(sc, x.c_str(), x.size()); }
        else if constexpr(std::is_same_v<Type, std::string_view>)                                { return s7_make_string_with_length(sc, x.data(), x.size());  }
        else if constexpr(std::is_same_v<Type, unsigned char>)                                   { return s7_make_character(sc, x);                            }
        else if constexpr(std::is_pointer_v<Type>)                                               { return s7_make_c_pointer(sc, x);                            }
        else if constexpr(std::is_same_v<Type, List>)                                            { return x.ptr();                                             }
        else if constexpr(std::is_same_v<Type, Values>)                                          { return x.p;                                                 }
        else if constexpr(std::is_same_v<Type, std::span<s7_pointer>> || std::is_same_v<Type, std::vector<s7_pointer>>) {
            auto vec = s7_make_vector(sc, x.size());
            for (size_t i = 0; i < x.size(); i++) {
                s7_vector_set(sc, vec, i, x[i]);
            }
            return vec;
        } else if constexpr(std::is_same_v<Type, std::span<s7_int>> || std::is_same_v<Type, std::vector<s7_int>>
                         || std::is_same_v<Type, std::span<int>>    || std::is_same_v<Type, std::vector<int>>
                         || std::is_same_v<Type, std::span<short>>  || std::is_same_v<Type, std::vector<short>>
                         || std::is_same_v<Type, std::span<long>>   || std::is_same_v<Type, std::vector<long>>) {
            auto vec = s7_make_int_vector(sc, x.size(), 1, nullptr);
            for (size_t i = 0; i < x.size(); i++) {
                s7_int_vector_set(vec, i, x[i]);
            }
            return vec;
        } else if constexpr(std::is_same_v<Type, std::span<double>> || std::is_same_v<Type, std::vector<double>>
                         || std::is_same_v<Type, std::span<float>>  || std::is_same_v<Type, std::vector<float>>) {
            auto vec = s7_make_float_vector(sc, x.size(), 1, nullptr);
            for (size_t i = 0; i < x.size(); i++) {
                s7_float_vector_set(vec, i, x[i]);
            }
            return vec;
        } else if constexpr(std::is_same_v<Type, std::span<uint8_t>> || std::is_same_v<Type, std::vector<uint8_t>>) {
            auto vec = s7_make_byte_vector(sc, x.size(), 1, nullptr);
            for (size_t i = 0; i < x.size(); i++) {
                s7_byte_vector_set(vec, i, x[i]);
            }
            return vec;
        } else {
            using Type = std::remove_cvref_t<Type>;
            return make_c_object(new Type(std::move(x)));
        }
    }

    std::string_view to_string(s7_pointer p)
    {
        // avoid s7_object_to_c_string since return value must be freed
        return to<std::string_view>(s7_object_to_string(sc, p, true));
    }

    // (list ...)
    template <typename T>
    List list(const T &arg)
    {
        return List(s7_cons(sc, from(arg), s7_nil(sc)));
    }

    template <typename T>
    List list(const T &&arg)
    {
        return List(s7_cons(sc, from(std::move(arg)), s7_nil(sc)));
    }

    template <typename T, typename... Args>
    List list(const T &arg, Args &&...args)
    {
        return List(s7_cons(sc, from(arg), list(FWD(args)...).ptr()));
    }

    template <typename T, typename... Args>
    List list(T &&arg, Args&&... args)
    {
        return List(s7_cons(sc, from(std::move(arg)), list(FWD(args)...).ptr()));
    }

    List list() { return s7::List(s7_nil(sc)); }

    Values values(List l) { return Values { .p = s7_values(sc, l.ptr()) }; }

    template <typename T>
    Values values(VarArgs<T> l) { return Values { .p = s7_values(sc, l.ptr()) }; }

    template <typename... Args>
    Values values(Args &&...args)
    {
        return Values { .p = s7_values(sc, list(FWD(args)...).ptr()) };
    }

    template <typename T>
    s7_pointer make_c_object(s7_int tag, T *p)
    {
        auto obj = s7_make_c_object(sc, tag, reinterpret_cast<void *>(p));
        s7_c_object_set_let(sc, obj, s7_openlet(sc, get_type_let<T>()));
        return obj;
    }

    template <typename T>
    s7_pointer make_c_object(T *p)
    {
        return make_c_object(detail::get_type_tag<T>(sc), p);
    }

    /* errors */
    template <typename T>
    s7_pointer error(T &&data)
    {
        if constexpr(std::is_same_v<T, errors::Error>)           { return s7_error(sc, s7_make_symbol(sc, data.type.data()), data.info.ptr()); }
        if constexpr(std::is_same_v<T, errors::WrongType>)       { return s7_wrong_type_arg_error(sc, data.caller.data(), data.arg_n, data.arg, data.type.data()); }
        if constexpr(std::is_same_v<T, errors::OutOfRange>)      { return s7_out_of_range_error(sc, data.caller.data(), data.arg_n, data.arg, data.type.data()); }
        if constexpr(std::is_same_v<T, errors::WrongArgsNumber>) { return s7_wrong_number_of_args_error(sc, data.caller.data(), data.args); }
    }

    /* variables and symbols */
    template <typename T>
    s7_pointer define(std::string_view name, const T &value, std::string_view doc = "")
    {
        auto object = from(value);
        return s7_define_variable_with_documentation(sc, name.data(), object, doc.data());
    }

    template <typename T>
    s7_pointer define(std::string_view name, T &&value, std::string_view doc = "")
    {
        auto object = from(std::move(value));
        return s7_define_variable_with_documentation(sc, name.data(), object, doc.data());
    }

    template <typename T>
    s7_pointer define_const(std::string_view name, const T &value, std::string_view doc = "")
    {
        auto object = from(value);
        return s7_define_constant_with_documentation(sc, name.data(), object, doc.data());
    }

    template <typename T>
    s7_pointer define_const(std::string_view name, T &&value, std::string_view doc = "")
    {
        auto object = from(std::move(value));
        return s7_define_constant_with_documentation(sc, name.data(), object, doc.data());
    }

    Variable operator[](std::string_view name);

    s7_pointer sym(std::string_view name) { return s7_make_symbol(sc, name.data()); }

    /* signatures */
    template <typename R, typename... Args>
    s7_pointer make_signature(R (*)(Args...))
    {
        return s7_make_signature(sc, sizeof...(Args) + 2, type_is_fn<R, true>(), type_is_fn<Args>()...);
    }

    template <typename C, typename R, typename... Args>
    s7_pointer make_signature(R (C::*)(Args...))
    {
        return s7_make_signature(sc, sizeof...(Args) + 1, type_is_fn<R, true>(), type_is_fn<Args>()...);
    }

    template <typename C, typename R, typename... Args>
    s7_pointer make_signature(R (C::*)(Args...) const)
    {
        return s7_make_signature(sc, sizeof...(Args) + 1, type_is_fn<R, true>(), type_is_fn<Args>()...);
    }

    template <typename R, typename T>
    s7_pointer make_signature(R (*)(VarArgs<T>))
    {
        return s7_make_circular_signature(sc, 0, 2, type_is_fn<R, true>(), type_is_fn<T>());
    }

    template <typename C, typename R, typename T>
    s7_pointer make_signature(R (C::*)(VarArgs<T>))
    {
        return s7_make_circular_signature(sc, 1, 2, type_is_fn<R, true>(), type_is_fn<T>());
    }

    template <typename C, typename R, typename T>
    s7_pointer make_signature(R (C::*)(VarArgs<T>) const)
    {
        return s7_make_circular_signature(sc, 1, 2, type_is_fn<R, true>(), type_is_fn<T>());
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
        return s7_call(sc, s7_name_to_value(sc, name.data()), list(FWD(args)...).ptr());
    }

    template <typename... T>
    s7_pointer call(s7_pointer func, T&&... args)
    {
        return s7_call(sc, func, list(FWD(args)...).ptr());
    }

    s7_pointer apply(s7_pointer fn, s7_pointer list) { return s7_apply_function(sc, fn, list); }
    s7_pointer apply(s7_pointer fn, List list) { return s7_apply_function(sc, fn, list.ptr()); }

    template <typename T>
    s7_pointer apply(s7_pointer fn, VarArgs<T> args) { return s7_apply_function(sc, fn, args.ptr()); }

    /* function creation */

    // special case for functions that follow s7's standard signature
    s7_pointer define_function(std::string_view name, std::string_view doc, s7_function fn, FunctionOpts opts = {})
    {
        auto _name = s7_string(save_string(name));
        auto define = opts.unsafe_arglist || opts.unsafe_body
            ? s7_define_function
            : s7_define_safe_function;
        return define(sc, _name, fn, 0, 0, true, doc.data());
    }

    template <typename F>
    s7_pointer define_function(std::string_view name, std::string_view doc, F &&func, FunctionOpts opts = {})
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

    template <typename... Fns>
    s7_pointer define_function(std::string_view name, std::string_view doc, Overload<Fns...> &&overload, FunctionOpts opts = {})
    {
        constexpr auto MaxArgs = max_arity<Fns...>();
        constexpr auto MinArgs = min_arity<Fns...>();
        auto f = std::apply([&]<typename ...F>(F &&...fns) {
            return _make_overload(detail::as_lambda(fns)...);
        }, overload.fns);
        auto _name = s7_string(save_string(name));
        auto define = opts.unsafe_arglist || opts.unsafe_body
            ? s7_define_function
            : s7_define_safe_function;
        return define(sc, _name, f, MinArgs, MaxArgs - MinArgs, false, doc.data());
    }

    void define_star_function(std::string_view name, std::string_view arglist_desc, std::string_view doc, s7_function f)
    {
        auto _name = s7_string(save_string(name));
        s7_define_function_star(sc, _name, f, arglist_desc.data(), doc.data());
    }

    template <typename F>
    void define_star_function(std::string_view name, std::string_view arglist_desc, std::string_view doc, F&& func)
    {
        auto _name = s7_string(save_string(name));
        auto f = make_s7_function(_name, func);
        auto sig = make_signature(func);
        s7_define_typed_function_star(sc, _name, f, arglist_desc.data(), doc.data(), sig);
    }

    template <typename F>
    void define_varargs_function(std::string_view name, std::string_view doc, F &&func, FunctionOpts opts = {})
    {
        auto _name = s7_string(save_string(name));
        auto f = make_s7_function(_name, func);
        auto define = opts.unsafe_body && opts.unsafe_arglist ? s7_define_unsafe_typed_function
                    : opts.unsafe_body                        ? s7_define_semisafe_typed_function
                    :                                           s7_define_typed_function;
        auto sig = make_signature(func);
        define(sc, _name, f, 0, 0, true, doc.data(), sig);
    }

    void define_macro(std::string_view name, std::string_view doc, s7_function f)
    {
        auto _name = s7_string(save_string(name));
        s7_define_macro(sc, _name, f, 0, 0, true, doc.data());
    }

    template <typename F>
    void define_macro(std::string_view name, std::string_view doc, F &&func)
    {
        constexpr auto NumArgs = FunctionTraits<F>::arity;
        auto _name = s7_string(save_string(name));
        auto f = make_s7_function(_name, func);
        s7_define_macro(sc, _name, f, NumArgs, 0, false, doc.data());
    }

    s7_pointer make_function(std::string_view name, std::string_view doc, s7_function fn, FunctionOpts opts = {})
    {
        auto _name = s7_string(save_string(name));
        auto make = opts.unsafe_arglist || opts.unsafe_body
            ? s7_make_function
            : s7_make_safe_function;
        return make(sc, _name, fn, 0, 0, true, doc.data());
    }

    template <typename F>
    s7_pointer make_function(std::string_view name, std::string_view doc, F &&func)
    {
        constexpr auto NumArgs = FunctionTraits<F>::arity;
        auto _name = s7_string(save_string(name));
        auto f = make_s7_function(_name, func);
        auto sig = make_signature(func);
        return s7_make_typed_function(sc, _name, f, NumArgs, 0, false, doc.data(), sig);
    }

    template <typename... Fns>
    s7_pointer make_function(std::string_view name, std::string_view doc, Overload<Fns...> &&overload, FunctionOpts opts = {})
    {
        constexpr auto MaxArgs = max_arity<Fns...>();
        constexpr auto MinArgs = min_arity<Fns...>();
        auto f = std::apply([&]<typename ...F>(F &&...fns) {
            return _make_overload(detail::as_lambda(fns)...);
        }, overload.fns);
        auto _name = s7_string(save_string(name));
        auto make = opts.unsafe_arglist || opts.unsafe_body
            ? s7_make_function
            : s7_make_safe_function;
        return make(sc, _name, f, MinArgs, MaxArgs - MinArgs, false, doc.data());
    }

    s7_pointer make_star_function(std::string_view name, std::string_view arglist_desc, std::string_view doc, s7_function f)
    {
        auto _name = s7_string(save_string(name));
        return s7_make_function_star(sc, _name, f, arglist_desc.data(), doc.data());
    }

    template <typename F>
    s7_pointer make_star_function(std::string_view name, std::string_view arglist_desc, std::string_view doc, F&& func)
    {
        auto _name = s7_string(save_string(name));
        auto f = make_s7_function(_name, func);
        return s7_make_function_star(sc, _name, f, arglist_desc.data(), doc.data());
    }

    template <typename F>
    s7_pointer make_varargs_function(std::string_view name, std::string_view doc, F &&func)
    {
        auto _name = s7_string(save_string(name));
        auto f = make_s7_function(_name, func);
        auto sig = make_signature(func);
        return s7_make_typed_function(sc, _name, f, 0, 0, true, doc.data(), sig);
    }

    /* usertypes */

private:
    template <typename T, typename F>
    void usertype_add_op(std::string_view name, s7_int tag, Op op, F &&fn)
        requires (std::is_same_v<T,          std::remove_cvref_t<typename FunctionTraits<F>::Argument<0>::Type>>
               || std::is_same_v<s7_pointer, std::remove_cvref_t<typename FunctionTraits<F>::Argument<0>::Type>>)
    {
        auto set_func = op == Op::Equal    ? s7_c_type_set_is_equal
                      : op == Op::Equivalent ? s7_c_type_set_is_equivalent
                      : op == Op::Copy     ? s7_c_type_set_copy
                      : op == Op::Fill     ? s7_c_type_set_fill
                      : op == Op::Reverse  ? s7_c_type_set_reverse
                      : op == Op::GcMark   ? s7_c_type_set_gc_mark
                      : op == Op::GcFree   ? s7_c_type_set_gc_free
                      : op == Op::Length   ? s7_c_type_set_length
                      : op == Op::ToString ? s7_c_type_set_to_string
                      : op == Op::ToList   ? s7_c_type_set_to_list
                      : op == Op::Ref      ? s7_c_type_set_ref
                      : op == Op::Set      ? s7_c_type_set_set
                      : nullptr;
        auto func_name = std::format("{}-op", name);
        auto _name = s7_string(save_string(func_name));
        s7_function f;
        if constexpr(FunctionTraits<F>::arity == 1) {
            if (op == Op::GcMark) {
                auto fn2 = detail::as_lambda(fn);
                f = make_s7_function(_name, [fn2](s7_pointer obj) -> s7_pointer {
                    auto obj_let = s7_c_object_let(obj);
                    s7_mark(obj_let);
                    fn2(*reinterpret_cast<T *>(s7_c_object_value(obj)));
                    return nullptr;
                });
            } else {
                f = make_s7_function(_name, fn);
            }
        } else {
            f = make_s7_function(_name, fn);
        }
        set_func(sc, tag, f);
    }

    template <typename T, typename F>
    void usertype_add_method_op(std::string_view name, s7_pointer let, MethodOp op, F &&fn)
        //requires (FunctionTraits<F>::arity == 2)
    {
        auto opname = op == MethodOp::Add ? "+"
                    : op == MethodOp::Sub ? "-"
                    : op == MethodOp::Mul ? "*"
                    : op == MethodOp::Div ? "/"
                    : "";
        auto doc    = op == MethodOp::Add ? "(+ ...) adds its arguments"
                    : op == MethodOp::Sub ? "(- ...) subtracts its trailing arguments from the first, or negates the first if only one it is given"
                    : op == MethodOp::Mul ? "(* ...) multiplies its arguments"
                    : op == MethodOp::Div ? "(/ x1 ...) divides its first argument by the rest, or inverts the first if there is only one argument"
                    : "";
        // put fn as a method
        auto _name = std::format("{} ({} method)", opname, name);
        auto add_method = make_function(_name, "custom method for usertype", std::move(fn));
        s7_define(sc, let, s7_make_symbol(sc, opname), add_method);
        // substitute op
        if (!substitured_ops.contains(op)) {
                 if (op == MethodOp::Add) { define_varargs_function(opname, doc, make_method_op_function<MethodOp::Add>()); }
            else if (op == MethodOp::Sub) { define_varargs_function(opname, doc, make_method_op_function<MethodOp::Sub>()); }
            else if (op == MethodOp::Mul) { define_varargs_function(opname, doc, make_method_op_function<MethodOp::Mul>()); }
            else if (op == MethodOp::Div) { define_varargs_function(opname, doc, make_method_op_function<MethodOp::Div>()); }
            substitured_ops.insert(op);
        }
    }

public:
    template <typename T, typename F>
    void add_op(Op op, F &&fn)
    {
        usertype_add_op(detail::get_type_name<T>(sc), get_type_tag<T>(), op, std::move(fn));
    }

    template <typename T, typename F>
    void add_method_op(MethodOp op, F &&fn)
    {
        usertype_add_method_op(detail::get_type_name<T>(sc), get_type_let<T>(), op, std::move(fn));
    }

    template <typename T, typename... Fns>
    s7_int make_usertype(std::string_view name, Constructors<Fns...> constructors, s7_pointer let)
    {
        auto tag = s7_make_c_type(sc, name.data());
        detail::TypeTag<T>::tag.insert_or_assign(reinterpret_cast<uintptr_t>(sc), tag);
        detail::TypeTag<T>::let.insert_or_assign(reinterpret_cast<uintptr_t>(sc), let);

        if constexpr(sizeof...(Fns) != 0) {
            auto doc = std::format("(make-{}) creates a new {}", name, name);
            if (constructors.name.empty()) {
                auto _name = std::format("make-{}", name);
                define_function(_name, doc.c_str(), std::move(constructors.overload));
            } else {
                define_function(constructors.name, doc.c_str(), std::move(constructors.overload));
            }
        } else if constexpr(requires { T(); }) {
            auto ctor_name = std::format("make-{}", name);
            auto doc = std::format("(make-{}) creates a new {}", name, name);
            define_function(s7_string(save_string(ctor_name)), doc.c_str(), [this, tag]() -> s7_pointer {
                auto &scheme = *reinterpret_cast<Scheme *>(&sc);
                return scheme.make_c_object(tag, new T());
            });
        } else if constexpr(requires { T(*this); }) {
            auto ctor_name = std::format("make-{}", name);
            auto doc = std::format("(make-{}) creates a new {}", name, name);
            define_function(s7_string(save_string(ctor_name)), doc.c_str(), [this, tag]() -> s7_pointer {
                auto &scheme = *reinterpret_cast<Scheme *>(&sc);
                return scheme.make_c_object(tag, new T(*this));
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
                return s7_make_integer(sc, o->size());
            });
        }

        if constexpr(requires() { &T::operator[]; }) {
            s7_c_type_set_ref(sc, tag, [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
                auto &scheme = *reinterpret_cast<Scheme *>(&sc);
                auto *obj = reinterpret_cast<T *>(s7_c_object_value(s7_car(args)));
                using ArgType = typename FunctionTraits<decltype(&T::operator[])>::Argument<1>::Type;
                auto arg = s7_cadr(args);
                if (!scheme.is<ArgType>(arg)) {
                    return s7_wrong_type_arg_error(sc, "T ref", 1, arg, scheme.type_to_string<ArgType>().data());
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
                        scheme.type_to_string<IndexType>().data());
                }
                auto value = s7_caddr(args);
                if (!scheme.is<ValueType>(value)) {
                    return s7_wrong_type_arg_error(sc, "T ref", 2, value,
                        scheme.type_to_string<ValueType>().data());
                }
                (*obj)[scheme.to<IndexType>(index)] = scheme.to<ValueType>(value);
                return s7_undefined(sc);
            });
        }

        s7_c_type_set_gc_mark(sc, tag, [](s7_scheme *, s7_pointer arg) -> s7_pointer {
            auto obj_let = s7_c_object_let(arg);
            s7_mark(obj_let);
            return nullptr;
        });

        auto is_name = std::format("{}?", name);
        auto is_doc  = std::format("({}? value) checks if value is a {}", name, name);
        auto is_fn = [](s7_scheme *sc, s7_pointer args) {
            auto &scheme = *reinterpret_cast<Scheme *>(&sc);
            return scheme.from<bool>(scheme.is<T>(s7_car(args)));
        };
        s7_define_function(sc, s7_string(save_string(is_name)), is_fn, 1, 0, false, is_doc.c_str());

        return tag;
    }

    template <typename T, typename... Fns>
    s7_int make_usertype(std::string_view name, Constructors<Fns...> constructors)
    {
        return make_usertype<T>(name, constructors, s7_inlet(sc, s7_nil(sc)));
    }

    template <typename T, typename F, typename... Fns>
    s7_int make_usertype(std::string_view name, Constructors<Fns...> constructors, s7_pointer let, Op op, F &&fn, auto&&... args)
    {
        auto tag = make_usertype<T>(name, constructors, let, FWD(args)...);
        usertype_add_op<T>(name, tag, op, fn);
        return tag;
    }

    template <typename T, typename F, typename... Fns>
    s7_int make_usertype(std::string_view name, Constructors<Fns...> constructors, Op op, F &&fn, auto&&... args)
    {
        auto tag = make_usertype<T>(name, constructors, FWD(args)...);
        usertype_add_op<T>(name, tag, op, fn);
        return tag;
    }

    template <typename T, typename F, typename... Fns>
    s7_int make_usertype(std::string_view name, Constructors<Fns...> constructors, MethodOp op, F &&fn, auto&&... args)
    {
        auto let = s7_inlet(sc, s7_nil(sc));
        auto tag = make_usertype<T>(name, constructors, let, FWD(args)...);
        usertype_add_method_op<T>(name, let, op, fn);
        return tag;
    }

    template <typename T, typename F, typename... Fns>
    s7_int make_usertype(std::string_view name, Constructors<Fns...> constructors, s7_pointer let, MethodOp op, F &&fn, auto&&... args)
    {
        auto tag = make_usertype<T>(name, constructors, let, FWD(args)...);
        usertype_add_method_op<T>(name, let, op, fn);
        return tag;
    }

    // also known as dilambda, but that is such a bad name (although technically right)
    template <typename F, typename G>
    void define_property(std::string_view name, std::string_view doc, F &&getter, G &&setter)
    {
        constexpr auto NumArgsF = FunctionTraits<F>::arity;
        constexpr auto NumArgsG = FunctionTraits<G>::arity;
        auto g = make_s7_function(name.data(), getter);
        auto s = make_s7_function(name.data(), setter);
        auto gsig = make_signature(getter);
        auto ssig = make_signature(setter);
        s7_define_variable(sc, name.data(),
            s7_typed_dilambda(sc, name.data(), g, NumArgsF, 0,
                s, NumArgsG, 0, doc.data(), gsig, ssig)
        );
    }

    /* type related stuff */
    Type type_of(s7_pointer p)
    {
             if (s7_is_null(sc, p))        { return Type::Nil;           }
        else if (s7_is_unspecified(sc, p)) { return Type::Unspecified;   }
        else if (s7_is_let(p))             { return Type::Let;           }
        else if (s7_is_openlet(p))         { return Type::OpenLet;       }
        else if (s7_is_boolean(p))         { return Type::Boolean;       }
        else if (s7_is_integer(p))         { return Type::Integer;       }
        else if (s7_is_real(p))            { return Type::Real;          }
        else if (s7_is_string(p))          { return Type::String;        }
        else if (s7_is_character(p))       { return Type::Character;     }
        else if (s7_is_ratio(p))           { return Type::Ratio;         }
        else if (s7_is_complex(p))         { return Type::Complex;       }
        else if (s7_is_vector(p))          { return Type::Vector;        }
        else if (s7_is_int_vector(p))      { return Type::IntVector;     }
        else if (s7_is_float_vector(p))    { return Type::FloatVector;   }
        else if (s7_is_byte_vector(p))     { return Type::ByteVector;    }
        else if (s7_is_complex_vector(p))  { return Type::ComplexVector; }
        else if (s7_is_pair(p))            { return Type::List;          }
        else if (s7_is_c_pointer(p))       { return Type::CPointer;      }
        else if (s7_is_c_object(p))        { return Type::CObject;       }
        else if (s7_is_random_state(p))    { return Type::RandomState;   }
        else if (s7_is_hash_table(p))      { return Type::HashTable;     }
        else if (s7_is_input_port(sc, p))  { return Type::InputPort;     }
        else if (s7_is_output_port(sc, p)) { return Type::OutputPort;    }
        else if (s7_is_syntax(p))          { return Type::Syntax;        }
        else if (s7_is_symbol(p))          { return Type::Symbol;        }
        else if (s7_is_keyword(p))         { return Type::Keyword;       }
        else if (s7_is_procedure(p))       { return Type::Procedure;     }
        else if (s7_is_macro(sc, p))       { return Type::Macro;         }
        else if (s7_is_dilambda(p))        { return Type::Dilambda;      }
        else if (s7_is_multiple_value(p))  { return Type::Values;        }
        else if (s7_is_iterator(p))        { return Type::Iterator;      }
        else if (s7_is_bignum(p))          { return Type::BigNum;        }
        else                               { return Type::Unknown;       }
    }

    template <typename T>
    s7_int get_type_tag()
    {
        return detail::get_type_tag<T>(sc);
    }

    template <typename T>
    s7_pointer get_type_let()
    {
        auto &m = detail::TypeTag<std::remove_cvref_t<T>>::let;
        auto it = m.find(reinterpret_cast<uintptr_t>(sc));
        assert(it != m.end() && "missing tag for T");
        return it->second;
    }

    template <typename T, bool OutputType = false>
    std::string_view type_to_string()
    {
        return s7::detail::type_to_string<T, OutputType>(sc);
    }

    template <typename T, bool OutputType = false>
    s7_pointer type_is_fn()
    {
        if constexpr(std::is_same_v<T, s7_pointer>) { return s7_t(sc); }
        if constexpr(std::is_same_v<T, Values>) { return sym("values"); }
        auto s = std::string(type_to_string<T, OutputType>()) + "?";
        return sym(s.c_str());
    }

    /* utilities */
    s7_pointer save_string(std::string_view s)
    {
        return s7_make_semipermanent_string(sc, s.data());
    }

    s7_pointer make_continuation()
    {
        return s7_make_continuation(sc);
    }

    s7_pointer set_setter(s7_pointer p, s7_pointer setter)
    {
        return s7_set_setter(sc, p, setter);
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
        s7_symbol_set_value(scheme->ptr(), sym, scheme->from(v)); return *this;
    }

    template <typename T>
    Variable & operator=(T &&v)
    {
        s7_symbol_set_value(scheme->ptr(), sym, scheme->from(std::move(v))); return *this;
    }

    template <typename T> T as()        { return scheme->to<T>(s7_symbol_value(scheme->ptr(), sym)); }
    template <typename T> auto as_opt() { return scheme->to_opt<T>(s7_symbol_value(scheme->ptr(), sym)); }
};

Variable Scheme::operator[](std::string_view name)
{
    auto sym = s7_symbol_table_find_name(sc, name.data());
    if (!sym) {
        sym = s7_define_variable(sc, name.data(), s7_nil(sc));
    }
    return Variable(this, sym);
}

struct Equal {
    Scheme *sc;

    explicit Equal(Scheme &s) : sc(&s) {}

    bool operator()(const s7_pointer &a, const s7_pointer &b) const {
        return s7_is_equal(sc->ptr(), a, b);
    }
};

struct Hash {
    Scheme *sc;

    explicit Hash(Scheme &s) : sc(&s) {}

    size_t operator()(const s7_pointer& p) const
    {
        return s7_hash_code(sc->ptr(), p, s7_name_to_value(sc->ptr(), "equal?"));
    }
};

} // namespace s7

#undef FWD
#endif

