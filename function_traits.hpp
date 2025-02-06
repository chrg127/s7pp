#pragma once

#include <utility>
#include <tuple>

namespace detail {

    template <typename T> struct remove_class { };
    template <typename C, typename R, typename... A> struct remove_class<R(C::*)(A...)>                { using type = R(A...); };
    template <typename C, typename R, typename... A> struct remove_class<R(C::*)(A...) const>          { using type = R(A...); };
    template <typename C, typename R, typename... A> struct remove_class<R(C::*)(A...) volatile>       { using type = R(A...); };
    template <typename C, typename R, typename... A> struct remove_class<R(C::*)(A...) const volatile> { using type = R(A...); };

    template <typename T>
    struct get_signature_impl {
        using type = typename remove_class<
            decltype(&std::remove_reference_t<T>::operator())
        >::type;
    };

    template <typename T>
    using get_signature = typename get_signature_impl<T>::type;
} // namespace detail

template <typename F>
struct FunctionTraits;

template <typename R, typename... Args>
struct FunctionTraits<R(*)(Args...)> : public FunctionTraits<R(Args...)>
{};

template <typename R, typename... Args>
struct FunctionTraits<R(Args...)> {
    using ReturnType = R;
    using Signature = R(Args...);

    static constexpr std::size_t arity = sizeof...(Args);

    template <std::size_t N>
    struct Argument {
        static_assert(N < arity, "error: invalid parameter index.");
        using Type = typename std::tuple_element<N, std::tuple<Args...>>::type;
    };
};

// member function pointer
template <typename C, typename R, typename... Args>
struct FunctionTraits<R(C::*)(Args...)> : public FunctionTraits<R(C&, Args...)> {};

// const member function pointer
template <typename C, typename R, typename... Args>
struct FunctionTraits<R(C::*)(Args...) const> : public FunctionTraits<R(C&, Args...)> {};

// member object pointer
template <typename C, typename R>
struct FunctionTraits<R(C::*)()> : public FunctionTraits<R(C&)> {};

// functor
template <typename F>
struct FunctionTraits {
private:
    using CallType = FunctionTraits<decltype(&F::operator())>;

public:
    using ReturnType = typename CallType::ReturnType;
    // remove class for lambdas
    using Signature = detail::get_signature<F>;

    static constexpr std::size_t arity = CallType::arity - 1;

    template <std::size_t N>
    struct Argument {
        static_assert(N < arity, "error: invalid parameter index.");
        using Type = typename CallType::template Argument<N+1>::Type;
    };
};

template <typename F>
struct FunctionTraits<F &> : public FunctionTraits<F> {};

template <typename F>
struct FunctionTraits<F &&> : public FunctionTraits<F> {};

template <typename... Fns>
constexpr auto max_arity()
{
    if constexpr(sizeof...(Fns) == 1) {
        return (FunctionTraits<Fns>::arity, ...);
    } else {
        return std::max(FunctionTraits<Fns>::arity...);
    }
}

template <typename... Fns>
constexpr auto min_arity()
{
    if constexpr(sizeof...(Fns) == 1) {
        return (FunctionTraits<Fns>::arity, ...);
    } else {
        return std::min(FunctionTraits<Fns>::arity...);
    }
}

