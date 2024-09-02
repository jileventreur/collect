#pragma once
#include <expected>
#include <ranges>
#include <algorithm>
#include <vector>

namespace detail {
    template <typename T>
    concept expected_like = requires(T c) {
        typename T::value_type;
        typename T::error_type;
        { c.has_value() } -> std::same_as<bool>;
    };

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
template <template <typename...> class Container = std::vector,
    std::ranges::input_range R>
    requires detail::expected_like<std::ranges::range_value_t<R>>
[[nodiscard]] constexpr 
std::expected<
    /*value*/ Container<typename std::ranges::range_value_t<R>::value_type>,
    /*error*/ typename std::ranges::range_value_t<R>::error_type>
    collect(R&& range) {
    using value_type = typename std::ranges::range_value_t<R>::value_type;
    using error_type = typename std::ranges::range_value_t<R>::error_type;
    using container_type = Container<value_type>;
    using return_type = std::expected<container_type, error_type>;

    if constexpr (std::ranges::forward_range<R> && detail::insertable<container_type>)
    {
        for (auto&& expected : range) {
            if (expected.has_value() == false)
                return std::unexpected(std::move(expected.error()));
        }

        container_type success;
        if constexpr (detail::reservable<container_type, R>)
        {
            success.reserve(std::ranges::size(range));
        }
        std::ranges::transform(range, std::inserter(success, std::end(success)),
                [](auto &&exp) { return exp.value(); });
        return return_type(success);
    }
    else if (detail::insertable<container_type>){
        container_type success;
        if constexpr (detail::reservable<container_type, R>)
        {
            success.reserve(std::ranges::size(range));
        }
        auto inserter = std::inserter(success, std::end(success));
        for (auto&& expected : range) {
            if (expected.has_value() == false)
                return std::unexpected(std::move(expected.error()));
            *inserter = std::move(expected.value());
        }
        return return_type(std::move(success));
    }
    else {
        static_assert(detail::always_false<container_type>, "Container is not insertable");
    }
}
}  // namespace ranges

namespace detail {
    template<template <typename...> class Container = std::vector>
    struct collect_fn : std::ranges::range_adaptor_closure<collect_fn<Container>> {
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
    constexpr inline auto collect() -> detail::collect_fn<Container> { return {}; }
}  // namespace views