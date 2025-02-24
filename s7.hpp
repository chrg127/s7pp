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
#include "s7/s7-config.h"

#define FWD(x) std::forward<decltype(x)>(x)

#ifdef WITH_WARNINGS
    #define WARN_PRINT(...) do { fprintf(stderr, __VA_ARGS__); } while (0)
#else
    #define WARN_PRINT(...)
#endif

namespace s7 {

class List {
    s7_pointer p;

public:
    explicit List(s7_pointer p) : p(p) {}

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
public:
    Values(s7_pointer p) : p(p) {}
    s7_pointer ptr() const { return p; }
};

class Function {
    s7_pointer p;

public:
    Function(s7_pointer p) : p(p)
    {
#ifdef S7_DEBUGGING
        assert(s7_is_procedure(p));
#endif
    }

    s7_pointer ptr() const { return p; }
};

class Let;

class InputPort {
    s7_scheme *sc;
    s7_pointer p;
public:
    explicit InputPort(s7_scheme *sc, s7_pointer p) : sc(sc), p(p) {}
    s7_pointer ptr() const { return p; }
    std::string_view filename() const { return s7_port_filename(sc, p); }
    s7_int line() const { return s7_port_line_number(sc, p); }
    s7_pointer read_char() { return s7_read_char(sc, p); }
    s7_pointer peek_char() { return s7_peek_char(sc, p); }
    s7_pointer read() { return s7_read(sc, p); }
    void close() { s7_close_input_port(sc, p); }
};

enum class InputMode {
    Read,
};

class OutputPort {
    s7_scheme *sc;
    s7_pointer p;
public:
    explicit OutputPort(s7_scheme *sc, s7_pointer p) : sc(sc), p(p) {}
    s7_pointer ptr() const { return p; }
    std::string_view filename() const { return s7_port_filename(sc, p); }
    s7_int line() const { return s7_port_line_number(sc, p); }
    std::string_view get_string() const { return s7_get_output_string(sc, p); }
    void newline() { s7_newline(sc, p); }
    s7_pointer write_char(s7_pointer c) { return s7_write_char(sc, c, p); }
    s7_pointer write(s7_pointer obj) { return s7_write(sc, obj, p); }
    s7_pointer display(s7_pointer obj) { return s7_display(sc, obj, p); }
    bool flush() { return s7_flush_output_port(sc, p); }
    void close() { s7_close_output_port(sc, p); }
};

enum class OutputMode {
    Write, Alter
};

struct FunctionOpts {
    bool unsafe_body = false;
    bool unsafe_arglist = false;
};

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

template <>
struct Constructors<> {
    std::string_view name = "";
    Constructors() = default;
    explicit Constructors(std::string_view name) : name(name) {}
};

namespace detail {
    template <typename L>
    struct LambdaTable {
        static inline std::function<typename FunctionTraits<L>::Signature> lambda;
        static inline std::unordered_map<uintptr_t, std::string_view> name;
    };

    template <typename F>
    std::string_view get_lambda_name(s7_scheme *sc)
    {
        return LambdaTable<F>::name.find(reinterpret_cast<uintptr_t>(sc))->second;
    }

    template <typename F>
    void set_lambda(s7_scheme *sc, F &&f, std::string_view name)
    {
        LambdaTable<F>::lambda = std::move(f);
        LambdaTable<F>::name.insert_or_assign(reinterpret_cast<uintptr_t>(sc), name.data());
    }

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
#ifdef S7_DEBUGGING
        assert(it != m.end() && "missing tag for T");
#endif
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

    template <typename T>
    s7_pointer get_type_let(s7_scheme *sc)
    {
        auto &m = detail::TypeTag<std::remove_cvref_t<T>>::let;
        auto it = m.find(reinterpret_cast<uintptr_t>(sc));
#ifdef S7_DEBUGGING
        assert(it != m.end() && "missing tag for T");
#endif
        return it->second;
    }

    template <typename T>
    s7_pointer make_c_object(s7_scheme *sc, s7_int tag, T *p)
    {
        auto obj = s7_make_c_object(sc, tag, reinterpret_cast<void *>(p));
        s7_c_object_set_let(sc, obj, s7_openlet(sc, get_type_let<T>(sc)));
        return obj;
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

    // (type-of p) that also works for c types
    std::string_view type_of(s7_scheme *sc, s7_pointer p)
    {
             if (s7_is_null(sc, p))        { return "null";            }
        else if (p == s7_undefined(sc))    { return "undefined";       }
        else if (s7_is_unspecified(sc, p)) { return "unspecified";     }
        else if (s7_is_openlet(p))         { return "openlet";         }
        else if (s7_is_let(p))             { return "let";             }
        else if (s7_is_boolean(p))         { return "boolean";         }
        else if (s7_is_integer(p))         { return "integer";         }
        else if (s7_is_real(p))            { return "real";            }
        else if (s7_is_string(p))          { return "string";          }
        else if (s7_is_character(p))       { return "char";            }
        else if (s7_is_ratio(p))           { return "rational";        }
        else if (s7_is_complex(p))         { return "complex";         }
        else if (s7_is_vector(p))          { return "vector";          }
        else if (s7_is_int_vector(p))      { return "int-vector";      }
        else if (s7_is_float_vector(p))    { return "float-vector";    }
        else if (s7_is_byte_vector(p))     { return "byte-vector";     }
        else if (s7_is_complex_vector(p))  { return "complex-vector";  }
        else if (s7_is_pair(p))            { return "list";            }
        else if (s7_is_c_pointer(p))       { return "c-pointer";       }
        else if (s7_is_random_state(p))    { return "random-state";    }
        else if (s7_is_hash_table(p))      { return "hash-table";      }
        else if (s7_is_input_port(sc, p))  { return "input-port";      }
        else if (s7_is_output_port(sc, p)) { return "output-port";     }
        else if (s7_is_syntax(p))          { return "syntax";          }
        else if (s7_is_symbol(p))          { return "symbol";          }
        else if (s7_is_keyword(p))         { return "keyword";         }
        else if (s7_is_procedure(p))       { return "procedure";       }
        else if (s7_is_macro(sc, p))       { return "macro";           }
        else if (s7_is_dilambda(p))        { return "dilambda";        }
        else if (s7_is_multiple_value(p))  { return "values";          }
        else if (s7_is_iterator(p))        { return "iterator";        }
        else if (s7_is_bignum(p))          { return "bignum";          }
        else if (s7_is_c_object(p)) {
            auto ctypes = s7_let_field_ref(sc, s7_make_symbol(sc, "c-types"));
            return s7_string(s7_list_ref(sc, ctypes, s7_c_object_type(p)));
        } else {
            return "unknown (should never happen)";
        }
    }

    template <typename Tp, bool Output = false>
    std::string_view type_to_string(s7_scheme *sc)
    {
        using T = std::decay_t<std::remove_cvref_t<Tp>>;
             if constexpr(std::is_same_v<T, s7_pointer>)            { return "#t";         }
        else if constexpr(std::is_same_v<T, void>)                  { return "unspecified"; }
        else if constexpr(std::is_same_v<T, bool>)                  { return "boolean";     }
        else if constexpr(std::is_same_v<T, s7_int>)                { return "integer";     }
        else if constexpr(std::is_same_v<T, double>)                { return "real";        }
        else if constexpr(std::is_same_v<T, s7_complex>)            { return "complex";     }
        else if constexpr(std::is_same_v<T, const char *>
                       || std::is_same_v<T, std::string_view>)      { return "string";      }
        else if constexpr(std::is_same_v<T, unsigned char>)         { return "character";   }
        else if constexpr(std::is_same_v<T, std::span<s7_pointer>>) { return "vector";      }
        else if constexpr(std::is_same_v<T, std::span<s7_int>>)     { return "int-vector";   }
        else if constexpr(std::is_same_v<T, std::span<double>>)     { return "float-vector"; }
        else if constexpr(std::is_same_v<T, std::span<uint8_t>>)    { return "byte-vector";  }
        else if constexpr(std::is_same_v<T, std::span<s7_complex>>) { return "complex-vector"; }
        else if constexpr(std::is_pointer_v<T>)                     { return "c-pointer";    }
        else if constexpr(std::is_same_v<T, List>)                  { return "list";        }
        else if constexpr(std::is_same_v<T, Function>)              { return "procedure";   }
        else if constexpr(std::is_same_v<T, Let>)                   { return "let"; }
        else if constexpr(std::is_same_v<T, InputPort>)             { return "input-port"; }
        else if constexpr(std::is_same_v<T, OutputPort>)            { return "output-port"; }
        // allowed in to(), but with truncation
        else if constexpr(std::is_same_v<T, int> || std::is_same_v<T, short> || std::is_same_v<T, long>) { return "integer"; }
        else if constexpr(std::is_same_v<T, float>)                                                      { return "real"; }
        // types that should only be in function return
        else if constexpr(Output &&
                         (std::is_same_v<T, std::vector<s7_int>>
                       || std::is_same_v<T, std::span<int>>    || std::is_same_v<T, std::vector<int>>
                       || std::is_same_v<T, std::span<short>>  || std::is_same_v<T, std::vector<short>>
                       || std::is_same_v<T, std::span<long>>   || std::is_same_v<T, std::vector<long>>))    { return "int-vector";   }
        else if constexpr(Output && std::is_same_v<T, std::string>)                                         { return "string";      }
        else if constexpr(Output &&
                         (std::is_same_v<T, std::vector<double>>
                       || std::is_same_v<T, std::span<float>>  || std::is_same_v<T, std::vector<float>>))   { return "float-vector"; }
        else if constexpr(Output && std::is_same_v<T, std::vector<uint8_t>>)                                { return "byte-vector";  }
        else if constexpr(Output && std::is_same_v<T, std::vector<s7_complex>>)                             { return "complex-vector"; }
        else if constexpr(Output && std::is_same_v<T, Values>)                                              { return "values";      }
        // anything else
        else                                                                                                { return detail::get_type_name<T>(sc); }
    }

    template <typename T>
    bool is(s7_scheme *sc, s7_pointer p)
    {
             if constexpr(std::is_same_v<T, s7_pointer>)            { return p;                     }
        else if constexpr(std::is_same_v<T, bool>)                  { return s7_is_boolean(p);      }
        else if constexpr(std::is_same_v<T, s7_int>)                { return s7_is_integer(p);      }
        else if constexpr(std::is_same_v<T, double>)                { return s7_is_real(p);         }
        else if constexpr(std::is_same_v<T, s7_complex>)            { return s7_is_complex(p);      }
        else if constexpr(std::is_same_v<T, const char *>
                       || std::is_same_v<T, std::string_view>)      { return s7_is_string(p);       }
        else if constexpr(std::is_same_v<T, unsigned char>)         { return s7_is_character(p);    }
        else if constexpr(std::is_same_v<T, std::span<s7_pointer>>) { return s7_is_vector(p);       }
        else if constexpr(std::is_same_v<T, std::span<s7_int>>)     { return s7_is_int_vector(p);   }
        else if constexpr(std::is_same_v<T, std::span<double>>)     { return s7_is_float_vector(p); }
        else if constexpr(std::is_same_v<T, std::span<uint8_t>>)    { return s7_is_byte_vector(p);  }
        else if constexpr(std::is_pointer_v<T>)                     { return s7_is_c_pointer(p);    }
        else if constexpr(std::is_same_v<T, List>)                  { return s7_is_pair(p);         }
        else if constexpr(std::is_same_v<T, Function>)              { return s7_is_procedure(p);    }
        else if constexpr(std::is_same_v<T, Let>)                   { return s7_is_let(p);          }
        else if constexpr(std::is_same_v<T, InputPort>)             { return s7_is_input_port(sc, p);  }
        else if constexpr(std::is_same_v<T, OutputPort>)            { return s7_is_output_port(sc, p); }
        // allowed in to() with truncation
        else if constexpr(std::is_same_v<T, int> || std::is_same_v<T, short> || std::is_same_v<T, long>) { return s7_is_integer(p); }
        else if constexpr(std::is_same_v<T, float>) { return s7_is_real(p); }
        // anything else
        return s7_is_c_object(p) && s7_c_object_type(p) == detail::get_type_tag<T>(sc);
    }

    template <typename T>
    T to(s7_scheme *sc, s7_pointer p)
    {
#ifdef S7_DEBUGGING
        assert(is<T>(sc, p) && "p isn't an object of type T");
#endif
             if constexpr(std::is_same_v<T, s7_pointer>)            { return p;                                                                 }
        else if constexpr(std::is_same_v<T, bool>)                  { return s7_boolean(sc, p);                                                 }
        else if constexpr(std::is_same_v<T, s7_int>)                { return s7_integer(p);                                                     }
        else if constexpr(std::is_same_v<T, double>)                { return s7_real(p);                                                        }
        else if constexpr(std::is_same_v<T, s7_complex>)            { return s7_complex(s7_real_part(p), s7_imag_part(p));                      }
        else if constexpr(std::is_same_v<T, const char *>)          { return s7_string(p);                                                      }
        else if constexpr(std::is_same_v<T, std::string_view>)      { return std::string_view(s7_string(p));                                    }
        else if constexpr(std::is_same_v<T, char>)                  { return static_cast<char>(s7_character(p));                                }
        else if constexpr(std::is_same_v<T, std::span<s7_pointer>>) { return std::span(s7_vector_elements(p), s7_vector_length(p));             }
        else if constexpr(std::is_same_v<T, std::span<s7_int>>)     { return std::span(s7_int_vector_elements(p), s7_vector_length(p));         }
        else if constexpr(std::is_same_v<T, std::span<double>>)     { return std::span(s7_float_vector_elements(p), s7_vector_length(p));       }
        else if constexpr(std::is_same_v<T, std::span<uint8_t>>)    { return std::span(s7_byte_vector_elements(p), s7_vector_length(p));        }
        else if constexpr(std::is_pointer_v<T>)                     { return reinterpret_cast<T>(s7_c_pointer(p));                              }
        else if constexpr(std::is_same_v<T, List>)                  { return List(p);                                                           }
        else if constexpr(std::is_same_v<T, Function>)              { return Function(p);                                                       }
        else if constexpr(std::is_same_v<T, Let>)                   { return Let(sc, p);                                                        }
        else if constexpr(std::is_same_v<T, InputPort>)             { return InputPort(sc, p);   }
        else if constexpr(std::is_same_v<T, OutputPort>)            { return OutputPort(sc, p);   }
        else if constexpr(std::is_same_v<T, int> || std::is_same_v<T, short> || std::is_same_v<T, long>) {
            WARN_PRINT(";truncanting s7_int (%zu bytes) to %zu bytes\n", sizeof(s7_int), sizeof(T));
            return static_cast<T>(s7_integer(p));
        }
        else if constexpr(std::is_same_v<T, float>) {
            WARN_PRINT(";converting double to float\n");
            return static_cast<T>(s7_real(p));
        }
        else                                                        { return *reinterpret_cast<std::remove_cvref_t<T> *>(s7_c_object_value(p)); }
    }

    template <typename F> Function make_function(s7_scheme *sc, std::string_view name, std::string_view doc, F &&func, FunctionOpts opts = {});

    template <typename T>
    s7_pointer from(s7_scheme *sc, const T &x)
    {
        using Type = std::remove_cvref_t<T>;
             if constexpr(std::is_same_v<Type, s7_pointer>)                                      { return x;                                                   }
        else if constexpr(std::is_same_v<Type, bool>)                                            { return s7_make_boolean(sc, x);                              }
        else if constexpr(std::is_same_v<Type, s7_int> || std::is_same_v<Type, int>
                       || std::is_same_v<Type, short> || std::is_same_v<Type, long>)             { return s7_make_integer(sc, x);                              }
        else if constexpr(std::is_same_v<Type, double> || std::is_same_v<Type, float>)           { return s7_make_real(sc, x);                                 }
        else if constexpr(std::is_same_v<Type, s7_complex>)                                      { return s7_make_complex(sc, x.real(), x.imag());             }
        else if constexpr(std::is_same_v<std::remove_cvref_t<std::decay_t<Type>>, char *>)       { return s7_make_string(sc, x);                               }
        else if constexpr(std::is_same_v<std::remove_cvref_t<std::decay_t<Type>>, const char *>) { return s7_make_string(sc, x);                               }
        else if constexpr(std::is_same_v<Type, std::string>)                                     { return s7_make_string_with_length(sc, x.c_str(), x.size()); }
        else if constexpr(std::is_same_v<Type, std::string_view>)                                { return s7_make_string_with_length(sc, x.data(), x.size());  }
        else if constexpr(std::is_same_v<Type, unsigned char>)                                   { return s7_make_character(sc, x);                            }
        else if constexpr(std::is_same_v<Type, List>)                                            { return x.ptr();                                             }
        else if constexpr(std::is_same_v<Type, Function>)                                        { return x.ptr();                                             }
        else if constexpr(std::is_same_v<Type, Let>)                                             { return x.ptr();                                             }
        else if constexpr(std::is_same_v<Type, Values>)                                          { return x.ptr();                                             }
        else if constexpr(std::is_same_v<Type, InputPort>)                                       { return x.ptr();                                             }
        else if constexpr(std::is_same_v<Type, OutputPort>)                                      { return x.ptr();                                             }
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
        } else if constexpr(std::is_function_v<std::remove_cvref_t<T>>
                  || std::is_function_v<std::remove_pointer_t<T>>
                  || requires { T::operator(); }) {
            return make_function(sc, "anonymous", "generated by from()", x, {}).ptr();
        } else if constexpr(std::is_pointer_v<Type>) {
            return s7_make_c_pointer(sc, x);
        } else {
            using Type = std::remove_cvref_t<Type>;
            return detail::make_c_object(sc, detail::get_type_tag<Type>(sc), new Type(x));
        }
    }

    template <typename T>
    s7_pointer from(s7_scheme *sc, T &&x)
    {
        using Type = std::remove_cvref_t<T>;
             if constexpr(std::is_same_v<Type, s7_pointer>)                                      { return x;                                                   }
        else if constexpr(std::is_same_v<Type, bool>)                                            { return s7_make_boolean(sc, x);                              }
        else if constexpr(std::is_same_v<Type, s7_int> || std::is_same_v<Type, int>
                       || std::is_same_v<Type, short> || std::is_same_v<Type, long>)             { return s7_make_integer(sc, x);                              }
        else if constexpr(std::is_same_v<Type, double> || std::is_same_v<Type, float>)           { return s7_make_real(sc, x);                                 }
        else if constexpr(std::is_same_v<Type, s7_complex>)                                      { return s7_make_complex(sc, x.real(), x.imag());             }
        else if constexpr(std::is_same_v<std::remove_cvref_t<std::decay_t<Type>>, char *>)       { return s7_make_string(sc, x);                               }
        else if constexpr(std::is_same_v<std::remove_cvref_t<std::decay_t<Type>>, const char *>) { return s7_make_string(sc, x);                               }
        else if constexpr(std::is_same_v<Type, std::string>)                                     { return s7_make_string_with_length(sc, x.c_str(), x.size()); }
        else if constexpr(std::is_same_v<Type, std::string_view>)                                { return s7_make_string_with_length(sc, x.data(), x.size());  }
        else if constexpr(std::is_same_v<Type, unsigned char>)                                   { return s7_make_character(sc, x);                            }
        else if constexpr(std::is_same_v<Type, List>)                                            { return x.ptr();                                             }
        else if constexpr(std::is_same_v<Type, Function>)                                        { return x.ptr();                                             }
        else if constexpr(std::is_same_v<Type, Let>)                                             { return x.ptr();                                             }
        else if constexpr(std::is_same_v<Type, Values>)                                          { return x.ptr();                                             }
        else if constexpr(std::is_same_v<Type, InputPort>)                                       { return x.ptr();                                             }
        else if constexpr(std::is_same_v<Type, OutputPort>)                                      { return x.ptr();                                             }
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
        } else if constexpr(std::is_function_v<std::remove_cvref_t<T>>
                  || std::is_function_v<std::remove_pointer_t<T>>
                  || requires { T::operator(); }) {
            return make_function(sc, "anonymous", "generated by from()", std::move(x), {}).ptr();
        } else if constexpr(std::is_pointer_v<Type>) {
            return s7_make_c_pointer(sc, x);
        } else {
            using Type = std::remove_cvref_t<Type>;
            return detail::make_c_object(sc, detail::get_type_tag<Type>(sc), new Type(std::move(x)));
        }
    }

    constexpr auto vmin(auto a) { return a; }
    constexpr auto vmin(auto a, auto &&...args) { return std::min(a, vmin(args...)); }
    constexpr auto vmax(auto a) { return a; }
    constexpr auto vmax(auto a, auto &&...args) { return std::max(a, vmin(args...)); }
    template <typename... Fns> constexpr auto max_arity() { return vmax(FunctionTraits<Fns>::arity...); }
    template <typename... Fns> constexpr auto min_arity() { return vmin(FunctionTraits<Fns>::arity...); }

    const char *input_mode_to_string(InputMode m) { return m == InputMode::Read ? "r" : ""; }
    const char *output_mode_to_string(OutputMode m) { return m == OutputMode::Write ? "w" : "a"; }
} // namespace detail

template <typename T>
struct VarArgs {
    s7_scheme *sc;
    s7_pointer p;
    std::string_view caller;
    s7_int arg_n;

public:
    using Type = T;

    VarArgs(s7_scheme *sc, s7_pointer p, std::string_view caller)
        : sc(sc), p(p), caller(caller), arg_n(1) {}

    VarArgs(s7_scheme *sc, s7_pointer p, std::string_view caller, s7_int arg_n)
        : sc(sc), p(p), caller(caller), arg_n(arg_n) {}

    T convert(s7_pointer x) const
    {
        if constexpr(std::is_same_v<T, s7_pointer>) {
            return x;
        } else {
#ifdef S7_DEBUGGING
            if (!detail::is<T>(sc, x)) {
                // this is actually fine, since s7_wrong_type_arg_error is a
                // noreturn function (despite not marked as such)
                auto s = std::format("a {}", detail::type_to_string<T>(sc));
                return detail::to<T>(sc, s7_wrong_type_arg_error(sc, caller.data(), arg_n, x, s.c_str()));
            }
#endif
            return detail::to<T>(sc, x);
        }
    }

    T car() const                     { return convert(s7_car(p)); }
    T operator[](std::size_t i) const { return convert(s7_list_ref(sc, p, i)); }
    std::size_t size() const          { return s7_list_length(sc, p); }
    VarArgs cdr() const               { return VarArgs(sc, s7_cdr(p), caller, arg_n+1); }
    s7_pointer ptr() const            { return p; }
    bool at_end()                     { return !s7_is_pair(p); }
    T advance()                       { auto tmp = car(); p = s7_cdr(p); return tmp; }

    struct iterator {
        using value_type = T;

        VarArgs va = VarArgs(nullptr, nullptr, "");

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

template <typename T> struct is_varargs             { static constexpr inline bool value = false; };
template <typename T> struct is_varargs<VarArgs<T>> { static constexpr inline bool value = true; };
template <typename T> constexpr bool is_varargs_v = is_varargs<T>::value;

template <typename F>
constexpr bool function_has_varargs()
{
    if constexpr(FunctionTraits<F>::arity == 0) {
        return false;
    } else if constexpr(!is_varargs_v<typename FunctionTraits<F>::Argument<FunctionTraits<F>::arity - 1>::Type>) {
        return false;
    }
    return true;
}

template <typename F> constexpr bool function_has_varargs(F &&) { return function_has_varargs<F>(); }

namespace detail {
    template <typename T, bool OutputType = false>
    s7_pointer type_is_fn(s7_scheme *sc)
    {
        if constexpr(std::is_same_v<T, s7_pointer>) { return s7_t(sc); }
        if constexpr(std::is_same_v<T, Values>) { return s7_make_symbol(sc, "values"); }
        if constexpr(is_varargs_v<T>) { return type_is_fn<typename T::Type, OutputType>(sc); }
        auto s = std::format("{}?", type_to_string<T, OutputType>(sc));
        return s7_make_symbol(sc, s.c_str());
    }

    template <typename F>
    s7_pointer make_signature(s7_scheme *sc)
    {
        using R = typename FunctionTraits<F>::ReturnType;
        constexpr auto Arity = FunctionTraits<F>::arity;
        return FunctionTraits<F>::call_with_args([&]<typename... Args>() {
            if constexpr(function_has_varargs<F>()) {
                return s7_make_circular_signature(sc, Arity, Arity + 1, type_is_fn<R, true>(sc), type_is_fn<Args>(sc)...);
            } else {
                return s7_make_signature(sc, Arity + 1, type_is_fn<R, true>(sc), type_is_fn<Args>(sc)...);
            }
        });
    }

    template <typename F> s7_pointer make_signature(s7_scheme *sc, F &&) { return make_signature<F>(sc); }

    template <typename R, typename... Args>
    s7_pointer call_fn(s7_scheme *sc, s7_pointer args, auto &&fn, std::string_view name)
    {
        constexpr auto NumArgs = sizeof...(Args);
        auto arglist = List(args);
        std::array<s7_pointer, NumArgs> arr;
        for (std::size_t i = 0; i < NumArgs; i++) {
            arr[i] = arglist.advance();
        }

#ifdef S7_DEBUGGING
        auto bools = [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            return std::array<bool, NumArgs> { detail::is<Args>(sc, arr[Is])...  };
        }(std::make_index_sequence<NumArgs>());
        auto first_wrong_type = std::find(bools.begin(), bools.end(), false);

        if (first_wrong_type != bools.end()) {
            auto i = first_wrong_type - bools.begin();
            arglist = List(args);
            [[maybe_unused]] auto f = [&](auto s) { return std::format("a {}", s); };
            auto types = std::array<std::string_view, NumArgs> {
                f(detail::type_to_string<Args>(sc))...
            };
            return s7_wrong_type_arg_error(sc, name.data(), i+1, arglist[i], types[i].data());
        }
#endif

        if constexpr(std::is_same_v<R, void>) {
            [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                fn(detail::to<Args>(sc, arr[Is])...);
            }(std::make_index_sequence<NumArgs>());
            return s7_unspecified(sc);
        } else {
            return detail::from(sc, [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                return fn(detail::to<Args>(sc, arr[Is])...);
            }(std::make_index_sequence<NumArgs>()));
        }
    }

    template <typename R, typename T>
    s7_pointer call_varargs_fn(s7_scheme *sc, s7_pointer args, auto &&fn, std::string_view name)
    {
        if constexpr(std::is_same_v<R, void>) {
            fn(VarArgs<T>(sc, args, name));
            return s7_unspecified(sc);
        } else {
            return detail::from(sc, fn(VarArgs<T>(sc, args, name)));
        }
    }

    template <typename F>
    s7_function make_s7_function(s7_scheme *sc, std::string_view name, F &&fn)
    {
        auto lambda = detail::as_lambda(fn);
        using L = std::remove_cvref_t<decltype(lambda)>;
        set_lambda<L>(sc, std::move(lambda), name);
        return [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
            return FunctionTraits<F>::call_with_args([&]<typename...Args>() {
                auto &fn = LambdaTable<L>::lambda;
                auto name = get_lambda_name<L>(sc);
                using R = typename FunctionTraits<F>::ReturnType;
                if constexpr(function_has_varargs<F>()) {
                    using LastArg = typename FunctionTraits<F>::Argument<FunctionTraits<F>::arity - 1>::Type;
                    return call_varargs_fn<R, typename LastArg::Type>(sc, args, fn, name);
                } else {
                    return call_fn<R, Args...>(sc, args, fn, name);
                }
            });
        };
    }

    template <typename R, typename... Args>
    s7_pointer _match_fn(s7_scheme *sc, s7_pointer args, s7_int length, auto &&fn)
    {
        constexpr auto NumArgs = FunctionTraits<decltype(fn)>::arity;
        if (length != NumArgs) {
            return nullptr;
        }

        auto arglist = s7::List(args);
        std::array<s7_pointer, NumArgs> arr;
        for (std::size_t i = 0; i < NumArgs; i++) {
            arr[i] = arglist.advance();
        }

        auto bools = [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            return std::array<bool, NumArgs> { detail::is<Args>(sc, arr[Is])...  };
        }(std::make_index_sequence<NumArgs>());
        auto matches = std::find(bools.begin(), bools.end(), false) == bools.end();
        if (!matches) {
            return nullptr;
        }

        if constexpr(std::is_same_v<R, void>) {
            [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                fn(detail::to<Args>(sc, arr[Is])...);
            }(std::make_index_sequence<NumArgs>());
            return s7_unspecified(sc);
        } else {
            return detail::from<R>(sc, [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                return fn(detail::to<Args>(sc, arr[Is])...);
            }(std::make_index_sequence<NumArgs>()));
        }
    }

    template <typename F>
    s7_pointer match_fn(s7_scheme *sc, s7_pointer args, s7_int length, std::string_view name, F &&fn)
    {
        return FunctionTraits<F>::call_with_args([&]<typename...Args>() {
            using R = typename FunctionTraits<F>::ReturnType;
            if constexpr(function_has_varargs<F>()) {
                using LastArg = typename FunctionTraits<F>::Argument<FunctionTraits<F>::arity - 1>::Type;
                return call_varargs_fn<R, typename LastArg::Type>(sc, args, fn, name);
            } else {
                return _match_fn<R, Args...>(sc, args, length, fn);
            }
        });
    }

    template <typename F>
    s7_pointer match_fns(s7_scheme *sc, s7_pointer args, s7_int length, std::string_view name, F &&f)
    {
        return match_fn(sc, args, length, name, f);
    }

    template <typename F, typename... Fns>
    s7_pointer match_fns(s7_scheme *sc, s7_pointer args, s7_int length, std::string_view name, F &&f, Fns &&...fns)
    {
        auto p = match_fn(sc, args, length, name, f);
        return p ? p : match_fns(sc, args, length, name, fns...);
    }

    template <typename... Fns>
    s7_function make_overload(s7_scheme *sc, std::string_view name, Fns&&... fns)
    {
        constexpr auto NumFns = sizeof...(Fns);
        (set_lambda(sc, std::move(fns), name), ...);

        return [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
            auto length = s7_list_length(sc, args);
            using FirstFn = std::remove_cvref_t<typename std::tuple_element<0, std::tuple<Fns...>>::type>;
            auto name = get_lambda_name<FirstFn>(sc);
            auto res = match_fns(sc, args, length, name, detail::LambdaTable<std::remove_cvref_t<Fns>>::lambda...);
            if (res) {
                return res;
            }

            std::vector<s7_pointer> types;
            for (auto arg : s7::List(args)) {
                auto name = std::format("{}?", type_of(sc, arg));
                types.push_back(s7_make_symbol(sc, name.c_str()));
            }
            auto str = std::format("{}: arglist ~a doesn't match any signature\n"
                                   ";valid signatures:", name);
            for (auto i = 0u; i < NumFns; i++) {
                str += "\n;~a";
            }
            auto msg = detail::from(sc, str);
            return s7_error(sc, s7_make_symbol(sc, "overload-no-match"), s7_list_nl(
                sc, sizeof...(Fns) + 2,
                msg, s7_array_to_list(sc, types.size(), types.data()),
                detail::make_signature<Fns>(sc)...,
                nullptr
            ));
        };
    }

    Function make_function(s7_scheme *sc, std::string_view name, std::string_view doc, s7_function fn, FunctionOpts opts)
    {
        auto _name = s7_string(s7_make_semipermanent_string(sc, name.data()));
        auto make = opts.unsafe_arglist || opts.unsafe_body
            ? s7_make_function
            : s7_make_safe_function;
        return Function(make(sc, _name, fn, 0, 0, true, doc.data()));
    }

    template <typename F>
    Function make_function(s7_scheme *sc, std::string_view name, std::string_view doc, F &&func, FunctionOpts)
    {
        constexpr auto NumArgs = FunctionTraits<F>::arity;
        auto _name = s7_string(s7_make_semipermanent_string(sc, name.data()));
        auto f = detail::make_s7_function(sc, _name, func);
        auto sig = make_signature(sc, func);
        return Function(s7_make_typed_function(sc, _name, f, NumArgs, 0, false, doc.data(), sig));
    }

    template <typename... Fns>
    Function make_function(s7_scheme *sc, std::string_view name, std::string_view doc, Overload<Fns...> &&overload, FunctionOpts opts)
    {
        auto f = std::apply([&]<typename ...F>(F &&...fns) {
            return detail::make_overload(sc, name, detail::as_lambda(fns)...);
        }, overload.fns);
        auto _name = s7_string(s7_make_semipermanent_string(sc, name.data()));
        auto make = opts.unsafe_arglist || opts.unsafe_body
            ? s7_make_function
            : s7_make_safe_function;
        constexpr auto has_varargs = std::apply([&]<typename...F>(F &&...fs) {
            return (function_has_varargs(fs) || ...);
        }, overload.fns);
        if constexpr(has_varargs) {
            constexpr auto MinArgs = detail::min_arity<Fns...>();
            return Function(make(sc, _name, f, MinArgs, 0, true, doc.data()));
        } else {
            constexpr auto MaxArgs = detail::max_arity<Fns...>();
            constexpr auto MinArgs = detail::min_arity<Fns...>();
            return Function(make(sc, _name, f, MinArgs, MaxArgs - MinArgs, false, doc.data()));
        }
    }

    Function make_star_function(s7_scheme *sc, std::string_view name, std::string_view arglist_desc, std::string_view doc, s7_function f)
    {
        return Function(s7_make_function_star(sc, s7_string(s7_make_semipermanent_string(sc, name.data())), f, arglist_desc.data(), doc.data()));
    }

    template <typename F>
    Function make_star_function(s7_scheme *sc, std::string_view name, std::string_view arglist_desc, std::string_view doc, F&& func)
    {
        auto _name = s7_string(s7_make_semipermanent_string(sc, name.data()));
        auto f = detail::make_s7_function(sc, _name, func);
        return Function(s7_make_function_star(sc, _name, f, arglist_desc.data(), doc.data()));
    }
} // namespace detail

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

// implementation taken from https://github.com/ThePhD/sol2/blob/develop/include/sol/resolve.hpp
// and kept intentionally simple (you shouldn't need to use these except for cases like operators)
template <typename Sig>             inline constexpr Sig    *resolve(Sig    *f) { return f; }
template <typename Sig, typename C> inline constexpr Sig C::*resolve(Sig C::*f) { return f; }

struct Variable;

class Let {
    s7_scheme *sc;
    s7_pointer let;

public:
    explicit Let(s7_scheme *sc, s7_pointer let) : sc(sc), let(let) {}

    s7_pointer ptr() const { return let; }

    template <typename T> s7_pointer define(std::string_view name, const T &value, std::string_view doc = "")
    {
        auto object = detail::from(sc, value);
        auto sym = s7_make_symbol(sc, name.data());
        s7_define(sc, let, sym, object);
        s7_set_documentation(sc, sym, doc.data());
        return sym;
    }

    template <typename T> s7_pointer define(std::string_view name, T &&value, std::string_view doc = "")
    {
        auto object = detail::from(sc, std::move(value));
        auto sym = s7_make_symbol(sc, name.data());
        s7_define(sc, let, sym, object);
        s7_set_documentation(sc, sym, doc.data());
        return sym;
    }

    template <typename T>
    s7_pointer define_const(std::string_view name, const T &value, std::string_view doc = "")
    {
        auto sym = define(name, value, doc);
        s7_set_immutable(sc, sym);
        return sym;
    }

    template <typename T>
    s7_pointer define_const(std::string_view name, T &&value, std::string_view doc = "")
    {
        auto sym = define(name, std::move(value), doc);
        s7_set_immutable(sc, sym);
        return sym;
    }

    Variable operator[](std::string_view name);

    template <typename T> T get(std::string_view name)        { return to<T>(s7_let_ref(sc, let, s7_make_symbol(sc, name.data()))); }
    template <typename T> auto get_opt(std::string_view name) { return to_opt<T>(s7_let_ref(sc, let, s7_make_symbol(sc, name.data()))); }

    template <typename T> void set(std::string_view name, const T  &value) { s7_let_set(sc, s7_make_symbol(sc, name.data()), from(value)); }
    template <typename T> void set(std::string_view name,       T &&value) { s7_let_set(sc, s7_make_symbol(sc, name.data()), from(std::move(value))); }

    List to_list() const { return List(s7_let_to_list(sc, let)); }

    template <typename F>
    s7_pointer define_function(std::string_view name, std::string_view doc, F &&fn, FunctionOpts opts = {})
    {
        auto f = detail::make_function(sc, name, doc, std::move(fn), opts);
        define(name, f.ptr(), doc);
        return f.ptr();
    }

    template <typename F>
    void define_star_function(std::string_view name, std::string_view arglist_desc, std::string_view doc, F &&fn)
    {
        auto f = detail::make_star_function(sc, name, arglist_desc, doc, std::move(fn));
        define(name, f.ptr(), doc);
    }
};

class Scheme {
    s7_scheme *sc;
    // NOTE: any following field can't be accessed inside non-capturing lambdas
    std::unordered_set<MethodOp> substitured_ops;

    template <MethodOp op>
    auto make_method_op_function()
    {
        auto old = Function(s7_name_to_value(sc, method_op_fn<op>().data()));
        return [this, old](VarArgs<s7_pointer> args) -> s7_pointer {
            constexpr auto Name = method_op_fn<op>();
            if (args.size() == 0) {
                return call(old, nil());
            } else if (args.size() == 0) {
                auto p = args.car();
                if (s7_is_c_object(p)) {
                    auto method = find_method(s7_c_object_let(p), Name);
                    if (!method) {
                        auto msg = std::format("a c-object that defines {}", Name);
                        return s7_wrong_type_arg_error(sc, Name.data(), 0, p, msg.c_str());
                    }
                    return call(method.value(), p);
                } else {
                    return call(old, p);
                }
            }
            auto res = args.advance();
            for (auto arg : args) {
                if (s7_is_c_object(res) || s7_is_c_object(arg)) {
                    auto p = s7_is_c_object(res) ? res : arg;
                    auto method = find_method(s7_c_object_let(p), Name);
                    if (!method) {
                        return s7_wrong_type_arg_error(sc, Name.data(), 0, res, "a c-object that defines +");
                    }
                    res = call(method.value(), res, arg);
                } else {
                    res = call(old, res, arg);
                }
            }
            return res;
        };
    }

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
                f = detail::make_s7_function(sc, _name, [fn2](s7_pointer obj) -> s7_pointer {
                    auto obj_let = s7_c_object_let(obj);
                    s7_mark(obj_let);
                    fn2(*reinterpret_cast<T *>(s7_c_object_value(obj)));
                    return nullptr;
                });
            } else {
                f = detail::make_s7_function(sc, _name, fn);
            }
        } else {
            f = detail::make_s7_function(sc, _name, fn);
        }
        set_func(sc, tag, f);
    }

    template <typename T, typename F>
    void usertype_add_method_op(std::string_view name, s7_pointer let, MethodOp op, F &&fn)
    {
        auto opname = op == MethodOp::Add ? "+"
                    : op == MethodOp::Sub ? "-"
                    : op == MethodOp::Mul ? "*"
                    : op == MethodOp::Div ? "/"
                    : "";
        // put fn as a method
        auto _name = std::format("{} ({} method)", opname, name);
        Function add_method = make_function(_name, "custom method for usertype", std::move(fn));
        Let(sc, let).define(opname, add_method);
        // substitute op
        if (!substitured_ops.contains(op)) {
            auto doc = s7_documentation(sc, s7_name_to_value(sc, opname));
                 if (op == MethodOp::Add) { define_function(opname, doc, make_method_op_function<MethodOp::Add>()); }
            else if (op == MethodOp::Sub) { define_function(opname, doc, make_method_op_function<MethodOp::Sub>()); }
            else if (op == MethodOp::Mul) { define_function(opname, doc, make_method_op_function<MethodOp::Mul>()); }
            else if (op == MethodOp::Div) { define_function(opname, doc, make_method_op_function<MethodOp::Div>()); }
            substitured_ops.insert(op);
        }
    }

    using InputFn  = s7_pointer (*)(s7_scheme *sc, s7_read_t read_choice, s7_pointer port);
    using OutputFn = void (*)(s7_scheme *sc, uint8_t c, s7_pointer port);

    InputFn make_input_fn(auto &&fn)
    {
        auto lambda = detail::as_lambda(fn);
        using L = std::remove_cvref_t<decltype(lambda)>;
        detail::set_lambda<L>(sc, std::move(lambda), "an input function");
        return [](s7_scheme *sc, s7_read_t read_choice, s7_pointer port) -> s7_pointer {
            auto &fn = detail::LambdaTable<L>::lambda;
            return fn(*reinterpret_cast<Scheme *>(&sc), InputPort(sc, port), read_choice);
        };
    }

    OutputFn make_output_fn(auto &&fn)
    {
        auto lambda = detail::as_lambda(fn);
        using L = std::remove_cvref_t<decltype(lambda)>;
        detail::set_lambda<L>(sc, std::move(lambda), "an output function");
        return [](s7_scheme *sc, uint8_t c, s7_pointer port) -> void {
            auto &fn = detail::LambdaTable<L>::lambda;
            fn(*reinterpret_cast<Scheme *>(&sc), OutputPort(sc, port), c);
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

    /* gc */
    s7_pointer gc_on(bool on)                 { return s7_gc_on(sc, on); }
    s7_int protect(s7_pointer p)              { return s7_gc_protect(sc, p); }
    template <typename T> s7_int protect(T p) { return s7_gc_protect(sc, p.ptr()); }
    void unprotect_at(s7_int loc)             { s7_gc_unprotect_at(sc, loc); }
    void mark(s7_pointer p)                   { s7_mark(p); }
    template <typename T> void mark(T p)      { s7_mark(p.ptr()); }

    /* constants */
    s7_pointer nil()         { return s7_nil(sc); }
    s7_pointer undefined()   { return s7_undefined(sc); }
    s7_pointer unspecified() { return s7_unspecified(sc); }
    s7_pointer eof()         { return s7_eof_object(sc); }

    /* functions for inspecting and converting from/to scheme objects */
    template <typename T> bool is(s7_pointer p)         { return s7::detail::is<T>(sc, p); }
    template <typename T> T to(s7_pointer p)            { return s7::detail::to<T>(sc, p); }
    template <typename T> s7_pointer from(const T &obj) { return s7::detail::from(sc, obj); }
    template <typename T> s7_pointer from(T &&obj)      { return s7::detail::from(sc, std::move(obj)); }
    bool is_number(s7_pointer p) { return s7_is_number(p); }
    bool is_equal(s7_pointer a, s7_pointer b) { return s7_is_equal(sc, a, b); }
    bool is_equivalent(s7_pointer a, s7_pointer b) { return s7_is_equivalent(sc, a, b); }

    template <typename T>
    std::optional<T> to_opt(s7_pointer p)
    {
        if (!detail::is<T>(sc, p)) {
            return std::nullopt;
        }
        return detail::to<T>(sc, p);
    }

    std::string_view to_string(s7_pointer p)
    {
        // avoid s7_object_to_c_string since return value must be freed
        return to<std::string_view>(s7_object_to_string(sc, p, true));
    }

    // (list ...)
    List list() { return s7::List(s7_nil(sc)); }
    template <typename T> List list(const T  &arg) { return List(s7_cons(sc, from(arg),            s7_nil(sc))); }
    template <typename T> List list(      T &&arg) { return List(s7_cons(sc, from(std::move(arg)), s7_nil(sc))); }
    template <typename T, typename... Args> List list(const T  &arg, Args &&...args) { return List(s7_cons(sc, from(arg),            list(FWD(args)...).ptr())); }
    template <typename T, typename... Args> List list(      T &&arg, Args &&...args) { return List(s7_cons(sc, from(std::move(arg)), list(FWD(args)...).ptr())); }

    // (values...)
    Values values(List l)                                     { return Values(s7_values(sc, l.ptr())); }
    template <typename T> Values values(VarArgs<T> l)         { return Values(s7_values(sc, l.ptr())); }
    template <typename... Args> Values values(Args &&...args) { return Values(s7_values(sc, list(FWD(args)...).ptr())); }

    // make_c_object
    template <typename T> s7_pointer make_c_object(s7_int tag, T *p) { return detail::make_c_object(sc, tag, p); }
    template <typename T> s7_pointer make_c_object(T *p)             { return make_c_object(detail::get_type_tag<T>(sc), p); }

    /* errors */
    template <typename T>
    s7_pointer error(T &&data)
    {
        if constexpr(std::is_same_v<T, errors::Error>)           { return s7_error(sc, s7_make_symbol(sc, data.type.data()), data.info.ptr()); }
        if constexpr(std::is_same_v<T, errors::WrongType>)       { return s7_wrong_type_arg_error(sc, data.caller.data(), data.arg_n, data.arg, data.type.data()); }
        if constexpr(std::is_same_v<T, errors::OutOfRange>)      { return s7_out_of_range_error(sc, data.caller.data(), data.arg_n, data.arg, data.type.data()); }
        if constexpr(std::is_same_v<T, errors::WrongArgsNumber>) { return s7_wrong_number_of_args_error(sc, data.caller.data(), data.args); }
    }

    /* define/get/set variables + sym */
    template <typename T>
    s7_pointer define(std::string_view name, const T &value, std::string_view doc = "")
    {
        return s7_define_variable_with_documentation(sc, name.data(), from(value), doc.data());
    }

    template <typename T>
    s7_pointer define(std::string_view name, T &&value, std::string_view doc = "")
    {
        return s7_define_variable_with_documentation(sc, name.data(), from(std::move(value)), doc.data());
    }

    template <typename T>
    s7_pointer define_const(std::string_view name, const T &value, std::string_view doc = "")
    {
        return s7_define_constant_with_documentation(sc, name.data(), from(value), doc.data());
    }

    template <typename T>
    s7_pointer define_const(std::string_view name, T &&value, std::string_view doc = "")
    {
        return s7_define_constant_with_documentation(sc, name.data(), from(std::move(value)), doc.data());
    }

    Variable operator[](std::string_view name);

    template <typename T> T get(std::string_view name)        { return to<T>(s7_name_to_value(sc, name.data())); }
    template <typename T> auto get_opt(std::string_view name) { return to_opt<T>(s7_name_to_value(sc, name.data())); }

    template <typename T> void set(std::string_view name, const T  &value) { s7_symbol_set_value(sc, sym(name.data()), from(value)); }
    template <typename T> void set(std::string_view name,       T &&value) { s7_symbol_set_value(sc, sym(name.data()), from(std::move(value))); }

    s7_pointer sym(std::string_view name) { return s7_make_symbol(sc, name.data()); }

    /* calling functions */
    template <typename... T>
    s7_pointer call(std::string_view name, T&&... args)
    {
        return s7_call(sc, s7_name_to_value(sc, name.data()), list(FWD(args)...).ptr());
    }

    template <typename... T>
    s7_pointer call(Function func, T&&... args)
    {
        return s7_call(sc, func.ptr(), list(FWD(args)...).ptr());
    }

    s7_pointer apply(Function fn, List list)                             { return s7_apply_function(sc, fn.ptr(), list.ptr()); }
    template <typename T> s7_pointer apply(Function fn, VarArgs<T> args) { return s7_apply_function(sc, fn.ptr(), args.ptr()); }

    /* function creation */
    template <typename F>
    s7_pointer make_signature(F &&f)
    {
        return detail::make_signature(sc, f);
    }

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
        auto _name = s7_string(save_string(name));
        auto f = detail::make_s7_function(sc, _name, func);
        auto define = opts.unsafe_body && opts.unsafe_arglist ? s7_define_unsafe_typed_function
                    : opts.unsafe_body                        ? s7_define_semisafe_typed_function
                    :                                           s7_define_typed_function;
        auto sig = make_signature(func);
        if constexpr(function_has_varargs(func)) {
            return define(sc, _name, f, 0, 0, true, doc.data(), sig);
        } else {
            constexpr auto NumArgs = FunctionTraits<F>::arity;
            return define(sc, _name, f, NumArgs, 0, false, doc.data(), sig);
        }
    }

    template <typename... Fns>
    s7_pointer define_function(std::string_view name, std::string_view doc, Overload<Fns...> &&overload, FunctionOpts opts = {})
    {
        auto f = std::apply([&]<typename ...F>(F &&...fns) {
            return detail::make_overload(sc, name, detail::as_lambda(fns)...);
        }, overload.fns);
        auto _name = s7_string(save_string(name));
        auto define = opts.unsafe_arglist || opts.unsafe_body
            ? s7_define_function
            : s7_define_safe_function;
        constexpr auto has_varargs = std::apply([&]<typename...F>(F &&...fs) {
            return (function_has_varargs(fs) || ...);
        }, overload.fns);
        constexpr auto MinArgs = detail::min_arity<Fns...>();
        if constexpr(has_varargs) {
            return define(sc, _name, f, MinArgs, 0, true, doc.data());
        } else {
            constexpr auto MaxArgs = detail::max_arity<Fns...>();
            return define(sc, _name, f, MinArgs, MaxArgs - MinArgs, false, doc.data());
        }
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
        auto f = detail::make_s7_function(sc, _name, func);
        auto sig = make_signature(func);
        s7_define_typed_function_star(sc, _name, f, arglist_desc.data(), doc.data(), sig);
    }

    void define_macro(std::string_view name, std::string_view doc, s7_function f)
    {
        s7_define_macro(sc, s7_string(save_string(name)), f, 0, 0, true, doc.data());
    }

    template <typename F>
    void define_macro(std::string_view name, std::string_view doc, F &&func)
    {
        constexpr auto NumArgs = FunctionTraits<F>::arity;
        auto _name = s7_string(save_string(name));
        auto f = detail::make_s7_function(sc, _name, func);
        s7_define_macro(sc, _name, f, NumArgs, 0, false, doc.data());
    }

    template <typename F>
    Function make_function(std::string_view name, std::string_view doc, F &&func, FunctionOpts opts = {})
    {
        return detail::make_function(sc, name, doc, std::move(func), opts);
    }

    template <typename F>
    Function make_star_function(s7_scheme *sc, std::string_view name, std::string_view arglist_desc, std::string_view doc, F&& func)
    {
        return detail::make_star_function(sc, name, arglist_desc, doc, std::move(func));
    }

    /* usertypes */
    template <typename T, typename... Fns>
    s7_int make_usertype(std::string_view name, Constructors<Fns...> constructors, s7_pointer let)
    {
        auto tag = s7_make_c_type(sc, name.data());
        detail::TypeTag<T>::tag.insert_or_assign(reinterpret_cast<uintptr_t>(sc), tag);
        detail::TypeTag<T>::let.insert_or_assign(reinterpret_cast<uintptr_t>(sc), let);

        auto doc = std::format("(make-{} ...) creates a new {}", name, name);
        auto ctor_name = !constructors.name.empty() ? constructors.name.data() : std::format("make-{}", name).c_str();
             if constexpr(sizeof...(Fns) != 0)    { define_function(ctor_name, doc.c_str(), std::move(constructors.overload)); }
        else if constexpr(requires { T(); })      { define_function(ctor_name, doc.c_str(), [this, tag]() -> s7_pointer { return make_c_object(tag, new T()); }); }
        else if constexpr(requires { T(*this); }) { define_function(ctor_name, doc.c_str(), [this, tag]() -> s7_pointer { return make_c_object(tag, new T(*this)); }); }

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
#ifdef S7_DEBUGGING
                if (!scheme.is<ArgType>(arg)) {
                    auto s = std::format("a {}", scheme.type_to_string<ArgType>());
                    return s7_wrong_type_arg_error(sc, "T ref", 1, arg, s.c_str());
                }
#endif
                return scheme.from((*obj)[scheme.to<ArgType>(arg)]);
            });

            s7_c_type_set_set(sc, tag, [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
                auto &scheme = *reinterpret_cast<Scheme *>(&sc);
                auto *obj = reinterpret_cast<T *>(s7_c_object_value(s7_car(args)));
                using IndexType = typename FunctionTraits<decltype(&T::operator[])>::Argument<1>::Type;
                using ValueType = std::remove_cvref_t<typename FunctionTraits<decltype(&T::operator[])>::ReturnType>;
                auto index = s7_cadr(args);
#ifdef S7_DEBUGGING
                if (!scheme.is<IndexType>(index)) {
                    auto s = std::format("a {}", scheme.type_to_string<IndexType>());
                    return s7_wrong_type_arg_error(sc, "T ref", 1, index, s.c_str());
                }
#endif
                auto value = s7_caddr(args);
#ifdef S7_DEBUGGING
                if (!scheme.is<ValueType>(value)) {
                    auto s = std::format("a {}", scheme.type_to_string<ValueType>());
                    return s7_wrong_type_arg_error(sc, "T ref", 2, value, s.c_str());
                }
#endif
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
    s7_int make_usertype(std::string_view name, Constructors<Fns...> constructors = {})
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

    // also known as dilambda, but that is such a bad name (although technically right)
    template <typename F, typename G>
    void define_property(std::string_view name, std::string_view doc, F &&getter, G &&setter)
    {
        constexpr auto NumArgsF = FunctionTraits<F>::arity;
        constexpr auto NumArgsG = FunctionTraits<G>::arity;
        auto g = detail::make_s7_function(sc, name.data(), getter);
        auto s = detail::make_s7_function(sc, name.data(), setter);
        auto gsig = make_signature(getter);
        auto ssig = make_signature(setter);
        s7_define_variable(sc, name.data(),
            s7_typed_dilambda(sc, name.data(), g, NumArgsF, 0,
                                               s, NumArgsG, 0, doc.data(), gsig, ssig));
    }

    /* type related stuff */
    std::string_view type_of(s7_pointer p) { return detail::type_of(sc, p); }

    template <typename T> s7_int     get_type_tag() { return detail::get_type_tag<T>(sc); }
    template <typename T> s7_pointer get_type_let() { return detail::get_type_let<T>(sc); }

    template <typename T, bool OutputType = false>
    std::string_view type_to_string()
    {
        return detail::type_to_string<T, OutputType>(sc);
    }

    /* let creation */
    Let rootlet() { return Let(sc, s7_rootlet(sc)); }
    Let curlet()  { return Let(sc, s7_curlet(sc)); }
    Let new_let() { return Let(sc, s7_sublet(sc, s7_curlet(sc), s7_nil(sc))); }
    Let new_let(List bindings) { return Let(sc, s7_sublet(sc, s7_curlet(sc), bindings.ptr())); }
    Let new_let_from(Let sub) { return Let(sc, s7_sublet(sc, sub.ptr(), s7_nil(sc))); }
    Let new_let_from(Let sub, List bindings) { return Let(sc, s7_sublet(sc, sub.ptr(), bindings.ptr())); }
    Let new_empty_let() { return Let(sc, s7_inlet(sc, s7_nil(sc))); }
    Let new_empty_let(List bindings) { return Let(sc, s7_inlet(sc, bindings.ptr())); }

    /* ports */
    InputPort  open_file(std::string_view name,  InputMode mode) { return InputPort( sc, s7_open_input_file( sc, name.data(), detail::input_mode_to_string( mode))); }
    OutputPort open_file(std::string_view name, OutputMode mode) { return OutputPort(sc, s7_open_output_file(sc, name.data(), detail::output_mode_to_string(mode))); }
    InputPort  open_string(std::string_view string) { return InputPort( sc, s7_open_input_string( sc, string.data())); }
    OutputPort open_string()                        { return OutputPort(sc, s7_open_output_string(sc)); }

    InputPort  open_input_function( auto &&fn) { return  InputPort(sc, s7_open_input_function( sc, make_input_fn(std::move(fn)))); }
    OutputPort open_output_function(auto &&fn) { return OutputPort(sc, s7_open_output_function(sc, make_output_fn(std::move(fn)))); }

    InputPort current_input_port()                         { return InputPort( sc, s7_current_input_port(sc)); }
    InputPort set_current_input_port(InputPort p)          { return InputPort( sc, s7_set_current_input_port(sc, p.ptr())); }
    InputPort set_current_input_port(std::string_view str) { return set_current_input_port(open_string(str)); }
    InputPort set_current_input_port(auto &&fn)            { return set_current_input_port(open_input_function(std::move(fn))); }
    OutputPort current_output_port()                       { return OutputPort(sc, s7_current_output_port(sc)); }
    OutputPort set_current_output_port(OutputPort p)       { return OutputPort(sc, s7_set_current_output_port(sc, p.ptr())); }
    OutputPort set_current_output_port(auto &&fn)          { return set_current_output_port(open_output_function(std::move(fn))); }
    OutputPort current_error_port()                        { return OutputPort(sc, s7_current_error_port(sc)); }
    OutputPort set_current_error_port(OutputPort p)        { return OutputPort(sc, s7_set_current_error_port(sc, p.ptr())); }
    OutputPort set_current_error_port(auto &&fn)           { return set_current_error_port(open_output_function(std::move(fn))); }

    /* utilities */
    s7_pointer save_string(std::string_view s)
    {
        return s7_make_semipermanent_string(sc, s.data());
    }

    s7_pointer make_continuation()
    {
        return s7_make_continuation(sc);
    }

    s7_pointer set_setter(s7_pointer p, Function setter)
    {
        return s7_set_setter(sc, p, setter.ptr());
    }

    std::optional<Function> find_method(s7_pointer p, std::string_view name)
    {
        auto m = s7_method(sc, p, sym(name.data()));
        if (!s7_is_procedure(m)) {
            return std::nullopt;
        }
        return Function(m);
    }

    std::string_view stacktrace() { return to_string(s7_stacktrace(sc)); }

    // probably worth noting: s7's history is reeeaaaally awkward. it's a circular list, yes, but it goes like this:
    // (last first second third last ...)
    // the only way to make any sense of it is 1) take the history's size; 2) walk it backwards using s7_list_ref().
    // you also need to turn it on with a define too (#define WITH_HISTORY 1). set_history_enabled(true) doesn't work.
    // (maybe i shouldn't provide this stuff if WITH_HISTORY isn't defined...)
    // if you're making a repl (the only reason anyone would use s7's history...), you might as well make your own
    // implementation, it's not that hard (especially if you're using C++).
    bool history_enabled() { return s7_history_enabled(sc); }
    bool set_history_enabled(bool enabled) { return s7_set_history_enabled(sc, enabled); }
    List history() { return List(s7_history(sc)); }
    s7_pointer add_history(s7_pointer entry) { return s7_add_to_history(sc, entry); }
};

class Variable {
    Scheme *scheme;
    s7_pointer let;
    s7_pointer sym;

public:
    Variable(Scheme *scheme, s7_pointer let, s7_pointer sym) : scheme(scheme), let(let), sym(sym) {}

    template <typename T> Variable & operator=(const T &v) { s7_let_set(scheme->ptr(), let, sym, scheme->from(v));            return *this; }
    template <typename T> Variable & operator=(T &&v)      { s7_let_set(scheme->ptr(), let, sym, scheme->from(std::move(v))); return *this; }

    template <typename T> T to()        { return scheme->to<T>(s7_let_ref(scheme->ptr(), let, sym)); }
    template <typename T> auto to_opt() { return scheme->to_opt<T>(s7_let_ref(scheme->ptr(), let, sym)); }
};

Variable Scheme::operator[](std::string_view name)
{
    auto sym = s7_make_symbol(sc, name.data());
    auto let = s7_rootlet(sc);
    if (s7_let_ref(sc, let, sym) == s7_undefined(sc)) {
        s7_define(sc, let, sym, s7_nil(sc));
    }
    return Variable(this, let, sym);
}

Variable Let::operator[](std::string_view name)
{
    auto sym = s7_make_symbol(sc, name.data());
    if (s7_let_ref(sc, let, sym) == s7_undefined(sc)) {
        s7_define(sc, let, sym, s7_nil(sc));
    }
    return Variable(reinterpret_cast<Scheme *>(&sc), let, sym);
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

