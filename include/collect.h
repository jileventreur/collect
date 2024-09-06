#include <vector>
#include <ranges>
#include <algorithm>
#include <vector>
#include <concepts>
#include <type_traits>
#include <tuple>

namespace detail {

    template <typename T>
    struct is_template_type : std::false_type {};

    template <template <class...> class T, class... Args>
    struct is_template_type<T<Args...>> : std::true_type {};

    template <typename T>
    static constexpr bool is_template_type_v = is_template_type<T>::value;

    template <typename T>
    struct get_template_args_size {
        static_assert(false, "T is not a templated type");
    };

    template <template <class...> class T, class... Args>
    struct get_template_args_size<T<Args...>> {
        static constexpr size_t value = sizeof...(Args);
    };

    template <typename T>
    static constexpr auto get_template_args_size_v = get_template_args_size<T>::value;

    template <typename T, size_t N>
    struct get_template_n_arg_type {
        static_assert(false, "T is not a templated type");
    };

    template <template <class...> class T, class... Args, size_t N>
    struct get_template_n_arg_type<T<Args...>, N> {
        using type = std::tuple_element_t<N, std::tuple<Args...>>;
    };

    template <typename T, size_t N>
    using get_template_n_arg_type_t = get_template_n_arg_type<T, N>::type;

    /// Need to check is_template + one template arg + template<T>::value_type = T
    template <typename T>
    concept optional_like =
        requires(T c) {
        typename T::value_type;
        { c.has_value() } -> std::same_as<bool>;
        { c.reset() };
        { c.value() };
    }
    && std::same_as<std::remove_cvref_t<decltype(std::declval<T>().value())>, typename T::value_type>
    && std::default_initializable<T>
    //doest not work if optional underlying type is not constexpr
    //&& T{}.has_value() == false // error case can be construct thanks to default constructor
    && is_template_type_v<T>
    && get_template_args_size_v<T> >= 1
    && std::same_as<get_template_n_arg_type_t<T, 0>, typename T::value_type>
    && std::constructible_from<T, typename T::value_type>
    ;

    template <typename T>
    concept expected_like = requires(T c) {
        typename T::value_type;
        typename T::error_type;
        typename T::unexpected_type;
        { c.has_value() } -> std::same_as<bool>;
        { c.value() };
        { c.error() };
    }
    && std::constructible_from<typename T::unexpected_type, typename T::error_type>
    && std::constructible_from<T, typename T::unexpected_type>
    && std::same_as<std::remove_cvref_t<decltype(std::declval<T>().value())>, typename T::value_type>
    && std::same_as<std::remove_cvref_t<decltype(std::declval<T>().error())>, typename T::error_type>
    && is_template_type_v<T>
    && get_template_args_size_v<T> >= 2
    && std::same_as<get_template_n_arg_type_t<T, 0>, typename T::value_type>
    && std::same_as<get_template_n_arg_type_t<T, 1>, typename T::error_type>
    ;

    template <typename T>
    concept potential_type = optional_like<T> || expected_like<T>;

    template <class C, class R>
    concept reservable = std::ranges::sized_range<R> && requires(C & c, R && rng) {
        { c.capacity() } -> std::same_as<std::ranges::range_size_t<C>>;
        { c.reserve(std::ranges::range_size_t<R>(0)) };
    };

    template <class C>
    concept insertable = requires(C c) {
        std::inserter(c, std::ranges::end(c));
    };

    template <class>
    constexpr inline bool always_false = false;

    /// value passed must contains an optional or expected like error
    template <class ReturnType, class T>
    constexpr auto construct_error(T&& value) {
        if constexpr (expected_like<ReturnType>) {
            return ReturnType(typename ReturnType::unexpected_type(value.error()));
        }
        else if constexpr (optional_like<ReturnType>) {
            auto error = ReturnType{};
            error.reset();
            return error;
        }
        else
            static_assert(potential_type<ReturnType> ,
                "T is neither an expected or optional like type");
    }



    template <typename Container,
        template <typename...> class Exp, class Value, class Error>
        requires(expected_like<Exp<Value, Error>>)
    [[maybe_unused]] constexpr auto get_return_type(Exp<Value, Error>) -> Exp<Container, Error>;

    template <typename Container,
        template <typename...> class Opt, class Value>
        requires(optional_like<Opt<Value>>)
    [[maybe_unused]] constexpr auto get_return_type(Opt<Value>) -> Opt<Container>;


}  // namespace detail


namespace ranges {

    template <typename Container, class Input>
    struct collect_return {
        using type = decltype(detail::get_return_type<Container>(std::declval<Input>()));
    };

    template <typename Container, class Input, typename... Args>
    using collect_return_t = collect_return<Container, Input>::type;
}


namespace detail {
    template <std::ranges::input_range Container,
        std::ranges::input_range R, typename... Args,
        class return_type = ranges::collect_return_t<Container, std::ranges::range_value_t<R>>>
#ifndef TESTS
        requires
    (!std::ranges::view<Container>) // ensure Container is container and not view
        && detail::potential_type<std::ranges::range_value_t<R>>
        && (std::same_as<
            std::ranges::range_value_t<Container>,
            typename std::ranges::range_value_t<R>::value_type>)
#endif
        [[nodiscard]] constexpr return_type collect_one_pass(R&& range, Args&&... args)
    {
        static_assert(std::constructible_from<Container, Args...>,
            "Container is not constructible from args...");
        Container success(std::forward<Args>(args)...);
        if constexpr (detail::reservable<Container, R>)
        {
            success.reserve(std::ranges::size(range));
        }
        auto inserter = std::inserter(success, std::end(success));
        for (auto&& potential_value : range) {
            if (potential_value.has_value() == false)
                return return_type(detail::construct_error<return_type>(std::move(potential_value)));
            *inserter = std::move(potential_value.value());
        }
        return return_type(std::move(success));
    }
}// namespace detail

namespace ranges {

    /// <summary>
    /// Take a range of std::expected[value, error] like types and returns an
    /// std::expected[Container[value], error]
    /// </summary>
    /// <returns>
    /// The return expected value contains either all values in the templated
    /// container (currently only works with std::vector) or in the error case, the
    /// first error found in the range
    /// </returns>
    template <std::ranges::input_range Container,
              std::ranges::input_range R, typename... Args>
    requires (!std::ranges::view<Container>) // ensure Container is container and not view
    [[nodiscard]] constexpr auto collect(R&& range, Args&&... args)
    {
        if constexpr (!detail::potential_type<std::ranges::range_value_t<R>>)
        {
            return std::ranges::to<Container>(std::forward<R>(range));
        }
        else {
            using return_type = collect_return_t<Container, std::ranges::range_value_t<R>>;
            // two pass construction case : first check then construct
            if constexpr (std::ranges::forward_range<R>)
            {
                // check if error on first pass
                for (auto&& potential_value : range) {
                    if (potential_value.has_value() == false)
                        return return_type(detail::construct_error<return_type>(std::move(potential_value)));
                }
                //construct from transform
                auto transform_view = range
                    | std::views::transform([](auto&& exp) { return exp.value(); });
                return return_type(transform_view | std::ranges::to<Container>(std::forward<Args>(args)...));
            }
            static_assert(detail::insertable<Container>, "Container is not insertable");
            // one pass construction case : check and fill on the fly
            return detail::collect_one_pass<Container>(range, std::forward<Args>(args)...);
        }
    }

    template <template <typename...> typename Container = std::vector,
        std::ranges::input_range R, typename... Args>
    [[nodiscard]] constexpr
        auto collect(R&& r, Args&&... args) {
        if constexpr (detail::potential_type<std::ranges::range_value_t<R>>) {
            using exp_value = typename std::ranges::range_value_t<R>::value_type;
            return collect<Container<exp_value>>(std::forward<R>(r), std::forward<Args>(args)...);
        } else {
            return collect<Container<std::ranges::range_value_t<R>>>(std::forward<R>(r), std::forward<Args>(args)...);
        }
    }
}  // namespace ranges

namespace detail {

    template<template <typename...> class Container = std::vector,
        class... Args>
    struct collect_fn_tplt : std::ranges::range_adaptor_closure<collect_fn_tplt<Container, Args...>> {
        std::tuple<Args...> args;

        template <std::ranges::input_range R>
        [[nodiscard]] constexpr
            auto operator()(R&& range) {
            auto apply_collect = [&range]
                <class... As>(As &&... inner_args) {
                return ranges::collect<Container>(
                    std::forward<R>(range),
                    std::forward<As>(inner_args)...);
            };
            return std::apply(apply_collect, args);
        }
    };

    template <std::ranges::input_range Container, class... Args>
    struct collect_fn_cont : std::ranges::range_adaptor_closure<collect_fn_cont<Container, Args...>> {
        std::tuple<Args...> args;

        template <std::ranges::input_range R>
        [[nodiscard]] constexpr
            // maybe use deducing this do decide either copy or move args
            auto operator()(R&& range) {
            auto apply_collect = [&range]
                <class... As>(As &&... inner_args) {
                return ranges::collect<Container>(
                    std::forward<R>(range),
                    std::forward<As>(inner_args)...);
            };
            return std::apply(apply_collect, args);
        }
    };
}  // namespace detail

namespace ranges {
    /// <summary>
    /// Take a range of std::expected[value, error] like types and returns an
    /// std::expected[Container[value], error]
    /// </summary>
    /// <returns>
    /// The return expected value contains either all values in the templated
    /// container (currently only works with std::vector) or in the error case, the
    /// first error found in the range
    /// </returns>
    template<template <typename...> class Container = std::vector, typename... Args>
    [[nodiscard]] constexpr inline
        auto collect(Args&&... args) -> detail::collect_fn_tplt<Container, Args...> {
        return { .args {std::forward<Args>(args)...} };
    }

    template <std::ranges::input_range Container, typename... Args>
    [[nodiscard]] constexpr inline
        auto collect(Args&&... args) -> detail::collect_fn_cont<Container, Args...> {
        return { .args {std::forward<Args>(args)...} };
    };

}  // namespace ranges