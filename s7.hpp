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
    const char *get_type_name(s7_scheme *sc)
    {
        auto tag = get_type_tag<T>(sc);
        auto ctypes = s7_let_field_ref(sc, s7_make_symbol(sc, "c-types"));
        return s7_string(s7_list_ref(sc, ctypes, tag));
    }

    template <typename T>
    s7_pointer get_type_let(s7_scheme *sc)
    {
        auto &m = TypeTag<std::remove_cvref_t<T>>::let;
        auto it = m.find(reinterpret_cast<uintptr_t>(sc));
        assert(it != m.end() && "missing tag for T");
        return it->second;
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
}

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
    List cdr() const       { return List(s7_cdr(p)); }
    s7_pointer ptr() const { return p; }
    bool at_end()          { return !s7_is_pair(p); }
    s7_pointer advance()   { auto tmp = s7_car(p); p = s7_cdr(p); return tmp; }

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
    Any, Unspecified, Nil, Let, OpenLet,
    Boolean, Integer, Real, String, Character, Ratio, Complex,
    Vector, IntVector, FloatVector, ByteVector, ComplexVector,
    List, CPointer, CObject, RandomState, HashTable,
    InputPort, OutputPort, Syntax, Symbol, Keyword,
    Procedure, Macro, Dilambda, Values, Iterator,
    BigNum,
    Unknown,
};

const char *type_to_string(Type t)
{
    switch (t) {
    case Type::Any:           { return "s7_pointer";      }
    case Type::Unspecified:   { return "unspecified";     }
    case Type::Nil:           { return "nil";             }
    case Type::Let:           { return "let";             }
    case Type::OpenLet:       { return "openlet";         }
    case Type::Boolean:       { return "boolean";         }
    case Type::Integer:       { return "integer";         }
    case Type::Real:          { return "real";            }
    case Type::String:        { return "string";          }
    case Type::Character:     { return "character";       }
    case Type::Ratio:         { return "ratio";           }
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
    case Type::BigNum:        { return "bigNum";          }
    default:
    case Type::Unknown:       { return "unknown";         }
    }
}

Type type_of(s7_pointer p)
{
         if (s7_is_let(p))            { return Type::Let;           }
    else if (s7_is_openlet(p))        { return Type::OpenLet;       }
    else if (s7_is_boolean(p))        { return Type::Boolean;       }
    else if (s7_is_integer(p))        { return Type::Integer;       }
    else if (s7_is_real(p))           { return Type::Real;          }
    else if (s7_is_string(p))         { return Type::String;        }
    else if (s7_is_character(p))      { return Type::Character;     }
    else if (s7_is_ratio(p))          { return Type::Ratio;         }
    else if (s7_is_complex(p))        { return Type::Complex;       }
    else if (s7_is_vector(p))         { return Type::Vector;        }
    else if (s7_is_int_vector(p))     { return Type::IntVector;     }
    else if (s7_is_float_vector(p))   { return Type::FloatVector;   }
    else if (s7_is_byte_vector(p))    { return Type::ByteVector;    }
    else if (s7_is_complex_vector(p)) { return Type::ComplexVector; }
    else if (s7_is_pair(p))           { return Type::List;          }
    else if (s7_is_c_pointer(p))      { return Type::CPointer;      }
    else if (s7_is_c_object(p))       { return Type::CObject;       }
    else if (s7_is_random_state(p))   { return Type::RandomState;   }
    else if (s7_is_hash_table(p))     { return Type::HashTable;     }
    else if (s7_is_syntax(p))         { return Type::Syntax;        }
    else if (s7_is_symbol(p))         { return Type::Symbol;        }
    else if (s7_is_keyword(p))        { return Type::Keyword;       }
    else if (s7_is_procedure(p))      { return Type::Procedure;     }
    else if (s7_is_dilambda(p))       { return Type::Dilambda;      }
    else if (s7_is_multiple_value(p)) { return Type::Values;        }
    else if (s7_is_iterator(p))       { return Type::Iterator;      }
    else if (s7_is_bignum(p))         { return Type::BigNum;        }
    else                              { return Type::Unknown;       }
}

Type type_of(s7_scheme *sc, s7_pointer p)
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
Type to_s7_type()
{
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
    else                                                        { return Type::CObject;     }
}

template <typename T>
Type to_s7_output_type()
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

template <typename T>
std::optional<T> to_opt(s7_scheme *sc, s7_pointer p)
{
    if (!is<T>(sc, p)) {
        return std::nullopt;
    }
    return to<T>(sc, p);
}

template <typename T>
s7_pointer from(s7_scheme *sc, const T &x)
{
         if constexpr(std::is_same_v<T, s7_pointer>)                                      { return x;                                                   }
    else if constexpr(std::is_same_v<T, bool>)                                            { return s7_make_boolean(sc, x);                              }
    else if constexpr(std::is_same_v<T, s7_int> || std::is_same_v<T, int>
                   || std::is_same_v<T, short> || std::is_same_v<T, long>)                { return s7_make_integer(sc, x);                              }
    else if constexpr(std::is_same_v<T, double> || std::is_same_v<T, float>)              { return s7_make_real(sc, x);                                 }
    else if constexpr(std::is_same_v<std::remove_cvref_t<std::decay_t<T>>, char *>)       { return s7_make_string(sc, x);                               }
    else if constexpr(std::is_same_v<std::remove_cvref_t<std::decay_t<T>>, const char *>) { return s7_make_string(sc, x);                               }
    else if constexpr(std::is_same_v<std::remove_cvref_t<T>, std::string>)                { return s7_make_string_with_length(sc, x.c_str(), x.size()); }
    else if constexpr(std::is_same_v<T, std::string_view>)                                { return s7_make_string_with_length(sc, x.data(), x.size());  }
    else if constexpr(std::is_same_v<T, unsigned char>)                                   { return s7_make_character(sc, x);                            }
    else if constexpr(std::is_pointer_v<T>)                                               { return s7_make_c_pointer(sc, x);                            }
    else if constexpr(std::is_same_v<T, List>)                                            { return x.ptr();                                             }
    else if constexpr(std::is_same_v<T, Values>)                                          { return x.p;                                                 }
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
    } else {
        auto tag = detail::get_type_tag<T>(sc);
        return s7_make_c_object(sc, tag, reinterpret_cast<void *>(new T(x)));
    }
}

template <typename T>
s7_pointer from(s7_scheme *sc, T &&x)
{
         if constexpr(std::is_same_v<T, s7_pointer>)                                      { return x;                                                   }
    else if constexpr(std::is_same_v<T, bool>)                                            { return s7_make_boolean(sc, x);                              }
    else if constexpr(std::is_same_v<T, s7_int> || std::is_same_v<T, int>
                   || std::is_same_v<T, short> || std::is_same_v<T, long>)                { return s7_make_integer(sc, x);                              }
    else if constexpr(std::is_same_v<T, double> || std::is_same_v<T, float>)              { return s7_make_real(sc, x);                                 }
    else if constexpr(std::is_same_v<std::remove_cvref_t<std::decay_t<T>>, char *>)       { return s7_make_string(sc, x);                               }
    else if constexpr(std::is_same_v<std::remove_cvref_t<std::decay_t<T>>, const char *>) { return s7_make_string(sc, x);                               }
    else if constexpr(std::is_same_v<std::remove_cvref_t<T>, std::string>)                { return s7_make_string_with_length(sc, x.c_str(), x.size()); }
    else if constexpr(std::is_same_v<T, std::string_view>)                                { return s7_make_string_with_length(sc, x.data(), x.size());  }
    else if constexpr(std::is_same_v<T, unsigned char>)                                   { return s7_make_character(sc, x);                            }
    else if constexpr(std::is_pointer_v<T>)                                               { return s7_make_c_pointer(sc, x);                            }
    else if constexpr(std::is_same_v<T, List>)                                            { return x.ptr();                                             }
    else if constexpr(std::is_same_v<T, Values>)                                          { return x.p;                                                 }
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
    } else {
        auto tag = detail::get_type_tag<T>(sc);
        return s7_make_c_object(sc, tag, reinterpret_cast<void *>(new T(std::move(x))));
    }
}

template <typename T>
const char *c_type_to_string(s7_scheme *sc)
{
    auto t = to_s7_type<T>();
    return (t == Type::CObject) ? detail::get_type_name<T>(sc) : type_to_string(t);
}

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
        s7_pointer x = this->p;
        while (i-- > 0) {
            x = s7_cdr(x);
        }
        return s7_car(x);
    }

    T car() const
    {
        auto r = s7_car(p);
        if (!is<T>(sc, r)) {
            // this is actually fine, since s7_wrong_type_arg_error is a
            // noreturn function (despite not marked as such)
            return to<T>(sc, s7_wrong_type_arg_error(sc, caller, arg_n, r,
                        c_type_to_string<T>(sc)));
        }
        return to<T>(sc, s7_car(p));
    }

    VarArgs cdr() const    { return VarArgs(sc, s7_cdr(p), caller, arg_n+1); }
    s7_pointer ptr() const { return p; }
    bool at_end()          { return !s7_is_pair(p); }
    T advance()            { auto tmp = car(); p = s7_cdr(p); return tmp; }

    std::size_t size() const
    {
        size_t s = 0;
        for (auto p = this->p; s7_is_pair(p); p = s7_cdr(p), s++)
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

namespace detail {
    template <typename L, typename R, typename... Args>
    s7_function _make_s7_function()
    {
        constexpr auto NumArgs = FunctionTraits<L>::arity;
        return [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
            auto arglist = List(args);
            std::array<s7_pointer, NumArgs> arr;
            for (std::size_t i = 0; i < NumArgs; i++) {
                arr[i] = arglist.advance();
            }

            auto bools = [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                return std::array<bool, NumArgs> { s7::is<Args>(sc, arr[Is])...  };
            }(std::make_index_sequence<NumArgs>());
            auto first_wrong_type = std::find(bools.begin(), bools.end(), false);

            if (first_wrong_type != bools.end()) {
                auto i = first_wrong_type - bools.begin();
                arglist = List(args);
                auto types = std::array<const char *, NumArgs> {
                    c_type_to_string<Args>(sc)...
                };
                auto name = detail::LambdaTable<L>::name
                    .find(reinterpret_cast<uintptr_t>(sc))->second;
                return s7_wrong_type_arg_error(sc, name, i+1, arglist[i], types[i]);
            }

            auto &fn = detail::LambdaTable<L>::lambda;
            if constexpr(std::is_same_v<R, void>) {
                [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                    fn(to<Args>(sc, arr[Is])...);
                }(std::make_index_sequence<NumArgs>());
                return s7_unspecified(sc);
            } else {
                return from<R>(sc, [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                    return fn(to<Args>(sc, arr[Is])...);
                }(std::make_index_sequence<NumArgs>()));
            }
        };
    }

    template <typename L, typename R, typename T>
    s7_function _make_s7_varargs_function()
    {
        return [](s7_scheme *sc, s7_pointer args) -> s7_pointer {
            auto &fn = detail::LambdaTable<L>::lambda;
            auto name = detail::LambdaTable<L>::name
                .find(reinterpret_cast<uintptr_t>(sc))->second;
            if constexpr(std::is_same_v<R, void>) {
                fn(VarArgs(sc, args, name));
                return s7_unspecified(sc);
            } else {
                return from<R>(sc, fn(VarArgs<T>(sc, args, name)));
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

    template <typename L, typename R, typename T>
    s7_function _make_s7_function(R (L::*)(VarArgs<T>) const)
    {
        return _make_s7_varargs_function<L, R, T>();
    }

    template <typename L, typename R, typename T>
    s7_function _make_s7_function(R (L::*)(VarArgs<T>))
    {
        return _make_s7_varargs_function<L, R, T>();
    }

    template <typename F>
    s7_function make_s7_function(s7_scheme *sc, const char *name, F&& fn)
    {
        auto lambda = detail::as_lambda(fn);
        using Lambda = std::remove_cvref_t<decltype(lambda)>;
        detail::LambdaTable<Lambda>::lambda = lambda;
        detail::LambdaTable<Lambda>::name
            .insert_or_assign(reinterpret_cast<uintptr_t>(sc), name);
        return _make_s7_function(&Lambda::operator());
    }

    template <typename F, typename R, typename... Args>
    s7_pointer match_fn(s7_scheme *sc, s7_int length, s7_pointer args)
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
            return std::array<bool, NumArgs> { is<Args>(sc, arr[Is])...  };
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
            return from<R>(sc, [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                return fn(to<Args>(sc, arr[Is])...);
            }(std::make_index_sequence<NumArgs>()));
        }
    }

    // varargs always matches... is it an error to allow it in ctors?
    template <typename F, typename R, typename T>
    s7_pointer match_varargs_fn(s7_scheme *sc, s7_int length, s7_pointer args)
    {
        auto &fn = detail::LambdaTable<F>::lambda;
        auto name = detail::LambdaTable<F>::name
            .find(reinterpret_cast<uintptr_t>(sc))->second;
        if constexpr(std::is_same_v<R, void>) {
            fn(VarArgs(sc, args, name));
            return s7_unspecified(sc);
        } else {
            return from<R>(sc, fn(VarArgs<T>(sc, args, name)));
        }
    }

    template <typename F, typename R, typename... Args>
    s7_pointer match_fn(s7_scheme *sc, s7_int length, s7_pointer args, R (F::*)(Args...))
    {
        return match_fn<F, R, Args...>(sc, length, args);
    }

    template <typename F, typename R, typename... Args>
    s7_pointer match_fn(s7_scheme *sc, s7_int length, s7_pointer args, R (F::*)(Args...) const)
    {
        return match_fn<F, R, Args...>(sc, length, args);
    }

    template <typename F, typename R, typename T>
    s7_pointer match_fn(s7_scheme *sc, s7_int length, s7_pointer args, R (F::*)(VarArgs<T>))
    {
        return match_varargs_fn<F, R, T>(sc, length, args);
    }

    template <typename F, typename R, typename T>
    s7_pointer match_fn(s7_scheme *sc, s7_int length, s7_pointer args, R (F::*)(VarArgs<T>) const)
    {
        return match_varargs_fn<F, R, T>(sc, length, args);
    }

    template <typename T, bool output = false>
    s7_pointer sig_type(s7_scheme *sc)
    {
        Type t;
        if constexpr(output) { t = to_s7_type<T>(); }
        else                 { t = to_s7_output_type<T>(); }
        auto f = [&](auto s) { return s7_make_symbol(sc, s); };
        switch (t) {
        case Type::Any:         { return s7_t(sc);   }
        case Type::Unspecified: { return f("unspecified?"); }
        case Type::Boolean:     { return f("boolean?");      }
        case Type::Integer:     { return f("integer?");      }
        case Type::Real:        { return f("real?");         }
        case Type::String:      { return f("string?");       }
        case Type::Character:   { return f("character?");    }
        case Type::Vector:      { return f("vector?");       }
        case Type::IntVector:   { return f("int-vector?");   }
        case Type::FloatVector: { return f("float-vector?"); }
        case Type::ByteVector:  { return f("byte-vector?");  }
        case Type::CPointer:    { return f("c-pointer?");    }
        case Type::List:        { return f("list?");         }
        default: {
            auto s = std::string(get_type_name<T>(sc)) + "?";
            return f(s.c_str());
        }
        }
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

enum class MathOp {
    Add, Sub, Mul, Div
};

template <typename... Fns>
struct Constructors {
    std::string_view name = "";
    std::tuple<Fns...> fns;

    explicit Constructors(Fns&&... fns) : fns(fns...) {}
    explicit Constructors(std::string_view name, Fns&&... fns) : name(name), fns(fns...) {}
};

struct Variable;

class Scheme {
    s7_scheme *sc;

    template <typename T, typename... Fns>
    s7_function _make_constructor(Fns&&... fns)
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
                detail::match_fn<Fns>(sc, length, args, &Fns::operator())...
            };

            auto make_message = [&](int n) -> s7_pointer {
                auto str = std::string("no constructor found for argument types: ~a\n"
                                       ";valid constructors:");
                for (auto i = 0; i < n; i++) {
                    str += "\n;~a";
                }
                return scheme.from(str);
            };

            auto it = std::find_if(results.begin(), results.end(),
                [](s7_pointer p) { return p != nullptr; });
            if (it != results.end()) {
                auto obj = *it;
                s7_c_object_set_let(sc, obj, detail::get_type_let<T>(sc));
                return obj;
            }

            std::vector<s7_pointer> types;
            for (auto arg : s7::List(args)) {
                types.push_back(s7_make_symbol(sc, type_to_string(type_of(arg))));
            }
            return s7_error(sc, s7_make_symbol(sc, "ctor-mismatch"), scheme.list(
                make_message(NumFns),
                s7_array_to_list(sc, types.size(), types.data()),
                scheme.make_signature(&std::remove_cvref_t<Fns>::operator())...
            ).ptr());
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
    void protect(s7_pointer p) { s7_gc_protect(sc, p); }
    void mark(s7_pointer p)    { s7_mark(p); }

    /* constants */
    s7_pointer nil()         { return s7_nil(sc); }
    s7_pointer undefined()   { return s7_undefined(sc); }
    s7_pointer unspecified() { return s7_unspecified(sc); }
    s7_pointer eof()          { return s7_eof_object(sc); }

    /* functions for inspecting and converting from/to scheme objects */
    template <typename T> bool is(s7_pointer p)                 { return s7::is<T>(sc, p); }
    template <typename T> T to(s7_pointer p)                    { return s7::to<T>(sc, p); }
    template <typename T> s7_pointer from(const T &x)           { return s7::from<T>(sc, x); }
    template <typename T> std::optional<T> to_opt(s7_pointer p) { return s7::to_opt<T>(sc, p); }

    std::string_view to_string(s7_pointer p)
    {
        // avoid s7_object_to_c_string since return value must be freed
        return this->to<std::string_view>(s7_object_to_string(sc, p, true));
    }

    template <typename T>
    List list(const T &arg)
    {
        return List(s7_cons(sc, from<T>(arg), s7_nil(sc)));
    }

    template <typename T, typename... Args>
    List list(const T &arg, Args&&... args)
    {
        return List(s7_cons(sc, from<T>(arg), list(args...).ptr()));
    }

    List list() { return s7::List(s7_nil(sc)); }

    Values values(List l) { return Values { .p = s7_values(sc, l.ptr()) }; }

    template <typename... Args>
    Values values(Args&&... args)
    {
        return Values { .p = s7_values(sc, list(std::forward<decltype(args)>(args)...).ptr()) };
    }

    template <typename T>
    s7_pointer make_c_object(T *p)
    {
        return s7_make_c_object(this->sc, detail::get_type_tag<T>(sc), reinterpret_cast<void *>(p));
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
        auto object = from<T>(value);
        return s7_define_variable_with_documentation(sc, name.data(), object, doc.data());
    }

    template <typename T>
    s7_pointer define_const(std::string_view name, const T &value, std::string_view doc = "")
    {
        auto object = from<T>(value);
        return s7_define_constant_with_documentation(sc, name.data(), object, doc.data());
    }

    Variable operator[](std::string_view name);

    s7_pointer sym(std::string_view name) { return s7_make_symbol(sc, name.data()); }

    /* signatures */
    template <typename R, typename... Args>
    s7_pointer make_signature(R (*)(Args...))
    {
        return s7_make_signature(sc, sizeof...(Args) + 2, detail::sig_type<R, true>(sc), detail::sig_type<Args>(sc)...);
    }

    template <typename C, typename R, typename... Args>
    s7_pointer make_signature(R (C::*)(Args...) const)
    {
        return s7_make_signature(sc, sizeof...(Args) + 1, detail::sig_type<R, true>(sc), detail::sig_type<Args>(sc)...);
    }

    template <typename C, typename R, typename... Args>
    s7_pointer make_signature(R (C::*)(Args...))
    {
        return s7_make_signature(sc, sizeof...(Args) + 1, detail::sig_type<R, true>(sc), detail::sig_type<Args>(sc)...);
    }

    template <typename R, typename T>
    s7_pointer make_signature(R (*)(VarArgs<T>))
    {
        return s7_make_circular_signature(sc, 0, 2, detail::sig_type<R, true>(sc), detail::sig_type<T>(sc));
    }

    template <typename C, typename R, typename T>
    s7_pointer make_signature(R (C::*)(VarArgs<T>))
    {
        return s7_make_circular_signature(sc, 1, 2, detail::sig_type<R, true>(sc), detail::sig_type<T>(sc));
    }

    template <typename C, typename R, typename T>
    s7_pointer make_signature(R (C::*)(VarArgs<T>) const)
    {
        return s7_make_circular_signature(sc, 1, 2, detail::sig_type<R, true>(sc), detail::sig_type<T>(sc));
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
        return s7_call(sc, s7_name_to_value(sc, name.data()), list(args...).ptr());
    }

    s7_pointer apply(s7_pointer fn, s7_pointer list) { return s7_apply_function(sc, fn, list); }
    s7_pointer apply(s7_pointer fn, List list) { return s7_apply_function(sc, fn, list.ptr()); }

    template <typename T>
    s7_pointer apply(s7_pointer fn, VarArgs<T> list) { return s7_apply_function(sc, fn, list.ptr()); }

    /* function creation */

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
        auto f = detail::make_s7_function(sc, _name, func);
        auto define = opts.unsafe_body && opts.unsafe_arglist ? s7_define_unsafe_typed_function
                    : opts.unsafe_body                        ? s7_define_semisafe_typed_function
                    :                                           s7_define_typed_function;
        auto sig = make_signature(func);
        return define(sc, _name, f, NumArgs, 0, false, doc.data(), sig);
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

    template <typename F>
    void define_varargs_function(std::string_view name, std::string_view doc, F &&func,
            FunctionOpts opts = { .unsafe_body = false, .unsafe_arglist = false })
    {
        auto _name = s7_string(save_string(name));
        auto f = detail::make_s7_function(sc, _name, func);
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
        auto f = detail::make_s7_function(sc, _name, func);
        s7_define_macro(sc, _name, f, NumArgs, 0, false, doc.data());
    }

    s7_pointer make_function(std::string_view name, std::string_view doc, s7_function fn,
        FunctionOpts opts = { .unsafe_body = false, .unsafe_arglist = false })
    {
        auto _name = s7_string(save_string(name));
        auto make = opts.unsafe_arglist || opts.unsafe_body
            ? s7_make_function
            : s7_make_safe_function;
        return make(sc, _name, fn, 0, 0, true, doc.data());
    }

    template <typename F>
    s7_pointer make_function(std::string_view name, std::string_view doc, F &&func)
        // FunctionOpts opts = { .unsafe_body = false, .unsafe_arglist = false })
    {
        constexpr auto NumArgs = FunctionTraits<F>::arity;
        auto _name = s7_string(save_string(name));
        auto f = detail::make_s7_function(sc, _name, func);
        auto sig = make_signature(func);
        return s7_make_typed_function(sc, _name, f, NumArgs, 0, false, doc.data(), sig);
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
        auto f = detail::make_s7_function(sc, _name, func);
        return s7_make_function_star(sc, _name, f, arglist_desc.data(), doc.data());
    }

    template <typename F>
    s7_pointer make_varargs_function(std::string_view name, std::string_view doc, F &&func)
        // FunctionOpts opts = { .unsafe_body = false, .unsafe_arglist = false })
    {
        auto _name = s7_string(save_string(name));
        auto f = detail::make_s7_function(sc, _name, func);
        auto sig = make_signature(func);
        return s7_make_typed_function(sc, _name, f, 0, 0, true, doc.data(), sig);
    }

    /* usertypes */
    template <typename T, typename... Fns>
    s7_function make_constructor(Fns&&... fns)
    {
        return _make_constructor<T>(detail::as_lambda(fns)...);
    }

    template <typename T, typename... Fns>
    s7_int make_usertype(std::string_view name, Constructors<Fns...> constructors, s7_pointer let)
    {
        auto tag = s7_make_c_type(sc, name.data());
        detail::TypeTag<T>::tag.insert_or_assign(reinterpret_cast<uintptr_t>(sc), tag);
        detail::TypeTag<T>::let.insert_or_assign(reinterpret_cast<uintptr_t>(sc), let);

        if constexpr(sizeof...(Fns) != 0) {
            auto ctor = std::apply([&]<typename... F>(F &&...fns) { return make_constructor<T>(fns...); }, constructors.fns);
            auto doc = std::format("(make-{}) creates a new {}", name, name);
            if (constructors.name.empty()) {
                auto ctor_name = std::format("make-{}", name);
                define_function(s7_string(save_string(ctor_name)), doc.c_str(), ctor);
            } else {
                define_function(s7_string(save_string(constructors.name)), doc.c_str(), ctor);
            }
        } else if constexpr(requires { T(); }) {
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
                    return s7_wrong_type_arg_error(sc, "T ref", 1, arg,
                        scheme.c_type_to_string<ArgType>());
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
                        scheme.c_type_to_string<IndexType>());
                }
                auto value = s7_caddr(args);
                if (!scheme.is<ValueType>(value)) {
                    return s7_wrong_type_arg_error(sc, "T ref", 2, value,
                        scheme.c_type_to_string<ValueType>());
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
        auto is = [](s7_scheme *sc, s7_pointer args) {
            auto &scheme = *reinterpret_cast<Scheme *>(&sc);
            return scheme.from<bool>(scheme.is<T>(s7_car(args)));
        };
        s7_define_function(sc, s7_string(save_string(is_name)), is, 1, 0, false, is_doc.c_str());

        return tag;
    }

    template <typename T, typename F, typename... Fns>
    s7_int make_usertype(std::string_view name, Constructors<Fns...> constructors,
        s7_pointer let, Op op, F &&fn, auto&&... args)
        requires (FunctionTraits<F>::arity == 1)
    {
        auto tag = make_usertype<T>(name, constructors, let, args...);
        auto set_func = op == Op::Copy     ? s7_c_type_set_copy
                      : op == Op::Reverse  ? s7_c_type_set_reverse
                      : op == Op::GcMark   ? s7_c_type_set_gc_mark
                      : op == Op::GcFree   ? s7_c_type_set_gc_free
                      : op == Op::Length   ? s7_c_type_set_length
                      : op == Op::ToString ? s7_c_type_set_to_string
                      : op == Op::ToList   ? s7_c_type_set_to_list
                      :                      s7_c_type_set_ref;
        auto func_name = std::format("{}-op", name);
        auto _name = s7_string(save_string(func_name));
        if (op == Op::GcMark) {
            auto fn2 = detail::as_lambda(fn);
            auto f = detail::make_s7_function(sc, _name, [fn2](s7_pointer obj) -> s7_pointer {
                auto obj_let = s7_c_object_let(obj);
                s7_mark(obj_let);
                fn2(*reinterpret_cast<T *>(s7_c_object_value(obj)));
                return nullptr;
            });
            set_func(sc, tag, f);
        } else {
            auto f = detail::make_s7_function(sc, _name, fn);
            set_func(sc, tag, f);
        }
        return tag;
    }

    template <typename T, typename F, typename... Fns>
    s7_int make_usertype(std::string_view name, Constructors<Fns...> constructors, s7_pointer let, Op op, F &&fn, auto&&... args)
    {
        auto tag = make_usertype<T>(name, constructors, let, args...);
        auto func_name = std::format("{}-op", name);
        auto _name = s7_string(save_string(func_name));
        auto f = detail::make_s7_function(sc, _name, fn);
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
                      :                      s7_c_type_set_set;
        set_func(sc, tag, f);
        return tag;
    }

    // template <typename T, typename F, typename... Fns>
    // s7_int make_usertype(std::string_view name, Constructors<Fns...> constructors, s7_pointer let, MathOp op, F &&fn, auto&&... args)
    // {
        // signal that the object has an openlet
        // add functions to object's let in ctor
        // install a new definition of the math op that will check any c-object's let

//         auto old_add = (*this)["+"];
//         auto new_add = [old_add](s7_scheme *sc, s7_pointer _args) -> s7_pointer {
//             auto args = List(_args);
//             if (args.size() == 0) {
//                 return s7_make_integer(sc, 0);
//             }
//             if (args.size() == 1) {
//                 return args.car();
//             }
//             auto res = l.advance();
//             for (auto arg = l.advance(); !l.at_end(); arg = l.advance()) {
//                 if (s7_is_c_object(res)) {
//                     auto method = s7_method(sc, res, s7_make_symbol("+"));
//                     if (method == s7_undefined(sc)) {
//                         s7_wrong_type_arg_error(sc, "+", 0, res, "a real");
//                     }
//                     res = s7_call(sc, method, s7_list(sc, 2, res, arg));
//                 } else if (s7_is_c_object(arg)) {
//                     auto method = s7_method(sc, arg, s7_make_symbol("+"));
//                     if (method == s7_undefined(sc)) {
//                         s7_wrong_type_arg_error(sc, "+", 0, arg, "a real");
//                     }
//                     res = s7_call(sc, method, s7_list(sc, 2, res, arg));
//                 } else {
//                     res = s7_call(sc, old_add, s7_list(sc, 2, res, arg));
//                 }
//             }
//         }
//         s7_define_function(sc, "+", new_add, 0, 0, true, "(+ ...) adds its arguments");
    // }

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
                s, NumArgsG, 0, doc.data(), gsig, ssig)
        );
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

    template <typename T>
    const char *c_type_to_string()
    {
        return s7::c_type_to_string<T>(sc);
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
