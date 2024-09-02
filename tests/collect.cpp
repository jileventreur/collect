#include <catch2/catch.hpp>
#include "collect.h"
#include <vector>
#include <expected>
#include <string>

using VecOfExp = std::vector<std::expected<int, std::string>>;
using ExpOfVec = std::expected<std::vector<int>, std::string>;

TEST_CASE("basic") {

    VecOfExp has_error = { 1, 2, std::unexpected("NOT INT") };
    VecOfExp no_error = { 1, 2, 3 };

    std::same_as<ExpOfVec> auto exp_error = has_error | ranges::collect();
    std::same_as<ExpOfVec> auto exp_value = ranges::collect(no_error);

    REQUIRE(exp_error == std::unexpected("NOT INT"));
    REQUIRE(exp_value == ExpOfVec(std::vector<int>{1, 2, 3}));
}


TEST_CASE("specify vector") {
    VecOfExp has_error = { 1, 2, std::unexpected("NOT INT") };
    VecOfExp no_error = { 1, 2, 3 };

    std::same_as<ExpOfVec> auto exp_error = has_error | ranges::collect<std::vector>();
    std::same_as<ExpOfVec> auto exp_value = ranges::collect<std::vector>(no_error);

    REQUIRE(exp_error == std::unexpected("NOT INT"));
    REQUIRE(exp_value == ExpOfVec(std::vector<int>{1, 2, 3}));
}


template<class Value>
struct fake_vector {
    bool reserve_called = false;
    size_t reserved = 0u;
    void reserve(size_t i) {
        reserve_called = true;
        reserved = i;
    };

    std::vector<Value> vec{};
    auto begin() { return vec.begin(); }
    auto end() { return vec.end(); }
    size_t size() { return {}; }
    size_t capacity() { return {}; }
    void push_back(auto &&) {}
};

TEST_CASE("calls reserve") {
    VecOfExp no_error = { 1, 2, 3 };

    auto fake_vec = (no_error | ranges::collect<fake_vector>()).value();

    static_assert(detail::reservable<fake_vector<int>, VecOfExp>);
    REQUIRE(fake_vec.reserve_called);
    REQUIRE(fake_vec.reserved == no_error.size());
    // maybe test a not std::ranges::sized_range R type
}

// TODO 
// for reservable container + sized range reserve before filling DONE
// constexpr test
// impl for multiple containers
// for forward_range check first then fill result
// impl for optional-like
// for non expected, optional act like range::to ? 
// enable nested containers ?
// work with associative types