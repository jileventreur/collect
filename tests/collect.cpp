#include <catch2/catch.hpp>
#include "collect.h"
#include <vector>
#include <expected>
#include <string>

TEST_CASE("basic") {
    using VecOfExp = std::vector<std::expected<int, std::string>>;
    using ExpOfVec = std::expected<std::vector<int>, std::string>;

    VecOfExp has_error = { 1, 2, std::unexpected("NOT INT") };
    VecOfExp no_error = { 1, 2, 3 };

    auto exp_error = has_error | ranges::collect();
    std::same_as<ExpOfVec> auto exp_value = ranges::collect(no_error);

    REQUIRE(exp_error == std::unexpected("NOT INT"));
    REQUIRE(exp_value == std::expected<std::vector<int>, std::string>(std::vector<int>{1, 2, 3}));
}
//
//template<class Value>
//struct fake_vector {
//    reserve_called = false;
//    size_t reserved = 0u;
//    void reserve(size_t i) {
//        reserve_called = true;
//        reserved = i;
//    };
//
//    void push_back(auto &&) { }
//};

TEST_CASE("specify vector") {
    using VecOfExp = std::vector<std::expected<int, std::string>>;
    VecOfExp has_error = { 1, 2, std::unexpected("NOT INT") };
    VecOfExp no_error = { 1, 2, 3 };

    auto exp_error = has_error | ranges::collect<std::vector>();
    auto exp_value = ranges::collect<std::vector>(no_error);

    REQUIRE(exp_error == std::unexpected("NOT INT"));
    REQUIRE(exp_value == std::expected<std::vector<int>, std::string>(std::vector<int>{1, 2, 3}));
}


// TODO 
// constexpr test
// impl for multiple containers
// for forward_range check first then fill result
// for reservable container + sized range reserve before filling
// impl for optional-like
// for non expected, optional act like range::to ? 
// enable nested containers ?
// work with associative types