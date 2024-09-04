#include <ranges>
#include <algorithm>
#include <vector>
#include <concepts>
#include <type_traits>

namespace detail {

    template <typename T>
    struct is_template_type : std::false_type {};

    template <template <class...> class T, class... Args>
    struct is_template_type<T<Args...>> : std::true_type {};

    template <typename T>
    static constexpr bool is_template_type_v = is_template_type<T>::value;

    template <typename T>
    struct get_template_args_size {
        static_assert(always_false<T>, "T is not a templated type");
    };

    template <template <class...> class T, class... Args>
    struct get_template_args_size<T<Args...>> {
        static constexpr size_t value = sizeof...(Args);
    };

    template <typename T>
    static constexpr auto get_template_args_size_v = get_template_args_size<T>::value;

    template <typename T, size_t N>
    struct get_template_n_arg_type {
        static_assert(always_false<T>, "T is not a templated type");
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
        { c.value() };
    }
    && std::same_as<std::remove_cvref_t<decltype(std::declval<T>().value())>, typename T::value_type>
    && std::default_initializable<T>
    && T{}.has_value() == false // error case can be construct thanks to default constructor
    && is_template_type_v<T>
    && get_template_args_size_v<T> >= 1
    && std::same_as<get_template_n_arg_type_t<T, 0>, typename T::value_type>
    && std::constructible_from<T, typename T::value_type>;

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
    auto construct_error(T&& value) {
        if constexpr (expected_like<ReturnType>) {
            return ReturnType(typename ReturnType::unexpected_type(value.error()));
        }
        else if constexpr (optional_like<ReturnType>) {
            return ReturnType{};
        }
        else
            static_assert(always_false<T>,
                "T is neither an expected or optional like type");
    }

    /// nested range collect tools : https://godbolt.org/z/77Wzb8xsK
    /// TOOL 1 : contains_potential_type_underneath
    /// Needed for deciding to act like ranges::to if no potential underneah
    template <typename T>
    struct contains_potential_type_underneath : std::false_type {};

    template <template <class...> class T, class... Args>
        requires potential_type<std::ranges::range_value_t<T<Args...>>>
    struct contains_potential_type_underneath<T<Args...>> : std::true_type {};

    template <template <class...> class T, class... Args>
        requires std::ranges::range<std::ranges::range_value_t<T<Args...>>> &&
    (!potential_type<std::ranges::range_value_t<T<Args...>>>)
        struct contains_potential_type_underneath<T<Args...>>
        : contains_potential_type_underneath<
        std::ranges::range_value_t<T<Args...>>> {};

    template <class T>
    static constexpr bool contains_potential_type_underneath_v =
        contains_potential_type_underneath<T>::value;

    template <typename Container,
        template <typename...> class Exp, class Value, class Error>
        requires(expected_like<Exp<Value, Error>>)
    [[maybe_unused]] constexpr auto get_return_type(Exp<Value, Error>) -> Exp<Container, Error>;

    template <typename Container,
        template <typename...> class Opt, class Value>
        requires(optional_like<Opt<Value>>)
    [[maybe_unused]] constexpr auto get_return_type(Opt<Value>) -> Opt<Container>;


    ////NESTED WIP
    //template <template <typename...> typename Container, class RangeValue>
    //    requires(contains_potential_type_underneath_v<RangeValue> == false)
    //[[maybe_unused]] constexpr auto get_return_type(RangeValue) -> Container<RangeValue>;

}  // namespace detail

namespace ranges {

    template <typename Container, class Input>
    struct collect_return_type {
        using type = decltype(detail::get_return_type<Container>(std::declval<Input>()));
    };

    template <typename Container, class Input>
    using collect_return_type_t = collect_return_type<Container, Input>::type;

    /// <summary>
    /// Take a range of std::expected[value, error] like types and returns an
    /// std::expected[Container[value], error]
    /// </summary>
    /// <returns>
    /// The return expected value contains either all values in the templated
    /// container (currently only works with std::vector) or in the error case, the
    /// first error found in the range
    /// </returns>
    /// TODO for nested and acts like ranges::to :
    ///  - check that dimensionality of Container and input range R match
    ///  - edit collect_return_type to return std::ranges::to type if no potential underneath
    template <std::ranges::input_range Container,
              std::ranges::input_range R>
        requires 
            detail::potential_type<std::ranges::range_value_t<R>>
            && (std::same_as<
                std::ranges::range_value_t<Container>,
                typename std::ranges::range_value_t<R>::value_type>)
        [[nodiscard]] constexpr
        collect_return_type_t<Container, std::ranges::range_value_t<R>>
        collect(R&& range) {
        using value_type = typename std::ranges::range_value_t<R>::value_type;
        using container_type = Container;
        using return_type = collect_return_type_t<Container, std::ranges::range_value_t<R>>;

        ////static_assert(detail::contains_potential_type_underneath_v<std::remove_cvref_t<R>>);
        //if constexpr (detail::contains_potential_type_underneath_v<std::remove_cvref_t<R>> == false)
        //{ // if no potential in any underlying container value acts like std::ranges::to
        //    return std::ranges::to<Container>(range);
        //}

        if constexpr (std::constructible_from<container_type/*, Args...*/> == false) //TODO add args
        {
            static_assert(detail::always_false<container_type>, "Container is not constructible");
        }

        // two pass construction case : first check then construct
        if constexpr (std::ranges::forward_range<R> && detail::insertable<container_type>)
        {
            // check if error on first pass
            for (auto&& potential_value : range) {
                if (potential_value.has_value() == false)
                    return return_type(detail::construct_error<return_type>(std::move(potential_value)));
            }
            //construct from transform
            auto transform_view = range | std::views::transform([](auto&& exp) { return exp.value(); });
            using transform_range = decltype(transform_view);
            //if constexpr (std::constructible_from<container_type, transform_range>) { // maybe never true ? 
            //    return return_type(container_type(std::move(transform_view)));
            //}
            //else 
            if constexpr (std::constructible_from<container_type, std::from_range_t, transform_range>) {
                return return_type(container_type(std::from_range, std::move(transform_view)));
            }
            else if constexpr (std::indirectly_copyable<std::ranges::iterator_t<transform_range>, std::ranges::iterator_t<container_type>>
                && detail::insertable<container_type>) {
                container_type success;
                if constexpr (detail::reservable<container_type, R>) {
                    //std::cout << "2 pass reserve\n";
                    success.reserve(std::ranges::size(range));
                }
                std::ranges::copy(std::move(transform_view), std::inserter(success, std::end(success)));
                return return_type(std::move(success));
            }
            else {
                static_assert(detail::always_false<container_type>, "Container cannot be filled with iterator_t<R>.value()");
            }
        }
        // one pass construction case : check and fill on the fly
        else if (detail::insertable<container_type>) {
            container_type success;
            if constexpr (detail::reservable<container_type, R>)
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
        else {
            static_assert(detail::always_false<container_type>, "Container is not insertable");
        }
    }

    template <template <typename...> typename Container = std::vector,
              std::ranges::input_range R>
    requires detail::potential_type<std::ranges::range_value_t<R>>
    // requires dimensionality of R is 1
    constexpr auto collect(R&& r) {
        using range_value = std::ranges::range_value_t<R>;
        return collect<Container<typename range_value::value_type>>(std::forward<R>(r));
    }
}  // namespace ranges

namespace detail {
    template<template <typename...> class Container = std::vector>
    struct collect_fn_tplt : std::ranges::range_adaptor_closure<collect_fn_tplt<Container>> {
        template <std::ranges::input_range R>
        constexpr auto operator()(R&& range) {
            return ranges::collect<Container>(std::forward<R>(range));
        }
    };

    template <std::ranges::input_range Container>
    struct collect_fn_cont: std::ranges::range_adaptor_closure<collect_fn_cont<Container>> {
        template <std::ranges::input_range R>
        constexpr auto operator()(R&& range) {
            return ranges::collect<Container>(std::forward<R>(range));
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
    template<template <typename...> class Container = std::vector>
    constexpr inline auto collect() -> detail::collect_fn_tplt<Container> { return {}; }

    template <std::ranges::input_range Container>
    constexpr auto collect() -> detail::collect_fn_cont<Container>{ return {}; };
    
}  // namespace ranges