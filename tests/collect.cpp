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
    std::vector<Value> vec{};
    using value_type = std::vector<Value>::value_type;
    bool reserve_called = false;
    size_t reserved = 0u;
    void reserve(size_t i) {
        reserve_called = true;
        reserved = i;
    };

    auto begin() { return vec.begin(); }
    auto end() { return vec.end(); }
    size_t size() { return {}; }
    size_t capacity() { return {}; }
    void push_back(auto&&) {}
    auto insert(auto &&... args) { return vec.insert(args...); }
};
static_assert(detail::reservable<fake_vector<int>, VecOfExp>);

#include<iostream>
TEST_CASE("calls reserve") {
    VecOfExp no_error = { 1, 2, 3 };
    auto fake_vec = (no_error | ranges::collect<fake_vector>()).value();
    REQUIRE(fake_vec.reserve_called);
    REQUIRE(fake_vec.reserved == no_error.size());

    auto fake_vec2 = *(no_error 
        // convert to not sized tange
        | std::views::take_while([](auto&&) {return true; }) 
        | ranges::collect<fake_vector>());
    REQUIRE(fake_vec2.reserve_called == false);
}

#include <list>
TEST_CASE("to list") {
    using ExpOfLst = std::expected<std::list<int>, std::string>;
    VecOfExp has_error = { 1, 2, std::unexpected("NOT INT") };
    VecOfExp no_error = { 1, 2, 3 };

    std::same_as<ExpOfLst> auto exp_error = has_error | ranges::collect<std::list>();
    std::same_as<ExpOfLst> auto exp_value = ranges::collect<std::list>(no_error);
 

    REQUIRE(exp_error == std::unexpected("NOT INT"));
    REQUIRE(exp_value == ExpOfLst(std::list<int>{1, 2, 3}));
}

// TODO 
// for reservable container + sized range reserve before filling DONE
// constexpr test
// impl for multiple containers DONE
// for forward_range check first then fill result DONE
//  -> to check
// impl for optional-like
// for non expected, optional act like range::to ? 
// enable nested containers ?
// work with associative types