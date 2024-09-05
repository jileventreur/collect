#include <catch2/catch.hpp>
#include "collect.h"
#include <vector>
#include <expected>
#include <list>
#include <string>

using VecOfExp = std::vector<std::expected<int, std::string>>;
using ExpOfVec = std::expected<std::vector<int>, std::string>;

using VecOfOpt = std::vector<std::optional<int>>;
using OptOfVec = std::optional<std::vector<int>>;

TEST_CASE("basic expected") {
    VecOfExp has_error = { 1, 2, std::unexpected("NOT INT") };
    VecOfExp no_error = { 1, 2, 3 };

    std::same_as<ExpOfVec> auto exp_error = has_error | ranges::collect();
    std::same_as<ExpOfVec> auto exp_value = ranges::collect(no_error);
    
    static_assert(detail::contains_potential_type_underneath_v<ExpOfVec> == false);

    REQUIRE(exp_error == std::unexpected("NOT INT"));
    REQUIRE(exp_value == ExpOfVec(std::vector<int>{1, 2, 3}));
}

TEST_CASE("basic optional") {
    VecOfOpt  has_error = { 1, 2, std::nullopt };
    VecOfOpt no_error = { 1, 2, 3 };

    std::same_as<OptOfVec> auto opt_error = has_error | ranges::collect();
    std::same_as<OptOfVec> auto opt_value = ranges::collect(no_error);

    REQUIRE(opt_error == std::nullopt);
    REQUIRE(opt_value == OptOfVec(std::vector<int>{1, 2, 3}));
}

TEST_CASE("partially specify vector") {
    VecOfExp has_error = { 1, 2, std::unexpected("NOT INT") };
    VecOfExp no_error = { 1, 2, 3 };

    std::same_as<ExpOfVec> auto exp_error = has_error | ranges::collect<std::vector>();
    std::same_as<ExpOfVec> auto exp_value = ranges::collect<std::vector>(no_error);

    REQUIRE(exp_error == std::unexpected("NOT INT"));
    REQUIRE(exp_value == ExpOfVec(std::vector<int>{1, 2, 3}));
}

TEST_CASE("fully specified container") {
    VecOfOpt has_error = { 1, 2, std::nullopt };
    VecOfOpt no_error = { 1, 2, 3 };

    std::same_as<OptOfVec> auto opt_error = has_error | ranges::collect<std::vector<int>>();
    std::same_as<OptOfVec> auto opt_value = ranges::collect<std::vector<int>>(no_error);

    REQUIRE(opt_error == std::nullopt);
    REQUIRE(opt_value == OptOfVec(std::vector<int>{1, 2, 3}));
}

TEST_CASE("to list") {
    using ExpOfLst = std::expected<std::list<int>, std::string>;
    VecOfExp has_error = { 1, 2, std::unexpected("NOT INT") };
    VecOfExp no_error = { 1, 2, 3 };

    std::same_as<ExpOfLst> auto exp_error = has_error | ranges::collect<std::list>();
    std::same_as<ExpOfLst> auto exp_value = ranges::collect<std::list>(no_error);

    REQUIRE(exp_error == std::unexpected("NOT INT"));
    REQUIRE(exp_value == ExpOfLst(std::list<int>{1, 2, 3}));
}

TEST_CASE("to string") {
    std::vector<std::optional<char>> vec{ 'h', 'e', 'l', 'l', 'o' };;

    std::same_as <std::optional<std::string>> auto opt = vec | ranges::collect<std::string>();
    REQUIRE(opt.has_value());
    REQUIRE(*opt == std::string("hello"));
}

TEST_CASE("one pass to list") {
    using ExpOfLst = std::expected<std::list<int>, std::string>;
    VecOfExp has_error = { 1, 2, std::unexpected("NOT INT") };
    VecOfExp no_error = { 1, 2, 3 };

    std::same_as<ExpOfLst> auto exp_error = detail::collect_one_pass<std::list<int>>(has_error);
    std::same_as<ExpOfLst> auto exp_value = detail::collect_one_pass<std::list<int>>(no_error);

    REQUIRE(exp_error == std::unexpected("NOT INT"));
    REQUIRE(exp_value == ExpOfLst(std::list<int>{1, 2, 3}));
}

TEST_CASE("one pass to string") {
    std::vector<std::optional<char>> vec{ 'h', 'e', 'l', 'l', 'o' };;

    std::same_as <std::optional<std::string>> auto opt = detail::collect_one_pass<std::string>(vec);
    REQUIRE(opt.has_value());
    REQUIRE(*opt == std::string("hello"));
}

template<class Value>
struct my_optional {
    //interface
    using value_type = Value;
    constexpr my_optional() = default;
    constexpr my_optional(Value val) : value_(val), default_init(false) {};

    constexpr value_type value() { return value_; }
    constexpr bool has_value() { return ! default_init; }
    
    //details
    Value value_ = {};
    bool default_init = true;
};

TEST_CASE("custom optional") {
    {
        REQUIRE(detail::optional_like<my_optional<int>>);

        std::vector<my_optional<int>> has_error = {
            my_optional<int>(1),
            my_optional<int>(2),
            my_optional<int>() };
        using return_type = ranges::collect_return_t<std::vector<int>, std::ranges::range_value_t<decltype(has_error)>>;
        REQUIRE(detail::optional_like<return_type>);

        auto opt_error = has_error | ranges::collect();
        REQUIRE(std::same_as<decltype(opt_error), my_optional<std::vector<int>>>);
        REQUIRE(opt_error.has_value() == false);
    }
    {
        std::vector<my_optional<int>> no_error = {
        my_optional<int>(1),
        my_optional<int>(2),
        my_optional<int>(3)};
        auto opt_result = no_error | ranges::collect();
        REQUIRE(opt_result.has_value());
        REQUIRE(opt_result.value() == std::vector{1, 2, 3});
    }
}

template <class Value, class Error>
struct my_expected{
    //interface
    using value_type = Value;
    using error_type = Error;
    struct unexpected_type {
        constexpr unexpected_type(Error error) : error_(error) {};
        Error error_ = {};
    };

    constexpr my_expected(unexpected_type unexp) :
        error_constructed(true), error_(unexp.error_) {}
    constexpr my_expected(Value val) : value_(val) {}

    constexpr value_type value() { return value_; }
    constexpr error_type error() { return error_; }
    constexpr bool has_value() { return ! error_constructed; }

    //details
    Value value_ = {};
    Error error_ = {};
    bool error_constructed = false;
};

TEST_CASE("custom expected") {
    using Exp = my_expected<int, bool>;
    {
        REQUIRE(detail::expected_like<my_expected<int, bool>>);

        std::vector<Exp> has_error = {
            Exp(1),
            Exp(2),
            Exp(Exp::unexpected_type(true)) };
        using return_type = ranges::collect_return_t<std::vector<int>, std::ranges::range_value_t<decltype(has_error)>>;
        REQUIRE(detail::expected_like<return_type>);

        auto exp_error = has_error | ranges::collect();
        REQUIRE(std::same_as<decltype(exp_error), my_expected<std::vector<int>, bool>>);
        REQUIRE(exp_error.has_value() == false);
        REQUIRE(exp_error.error() == true);
    }
    {
        std::vector<Exp> no_error = {
            Exp(1),
            Exp(2),
            Exp(3) };
        auto exp_result = no_error | ranges::collect();
        REQUIRE(exp_result.has_value());
        REQUIRE(exp_result.value() == std::vector{ 1, 2, 3 });
    }
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

    inline static int contructed_count = 0;
    fake_vector() {
        ++contructed_count;
    }

    auto begin() { return vec.begin(); }
    auto end() { return vec.end(); }
    size_t size() { return {}; }
    size_t capacity() { return {}; }
    void push_back(auto&&) {}
    auto insert(auto &&... args) { return vec.insert(args...); }
};
static_assert(detail::reservable<fake_vector<int>, VecOfExp>);

TEST_CASE("calls reserve one pass") {
    VecOfExp no_error = { 1, 2, 3 };
    auto fake_vec = (detail::collect_one_pass<fake_vector<int>>(no_error)).value();
    REQUIRE(fake_vec.reserve_called);
    REQUIRE(fake_vec.reserved == no_error.size());

    auto not_sized_view = no_error
        | std::views::take_while([](auto&&) { return true; });
    auto fake_vec2 = *(detail::collect_one_pass<fake_vector<int>>(not_sized_view));
    REQUIRE(fake_vec2.reserve_called == false);
}

TEST_CASE("no construction on forward_range error") {
    VecOfExp has_error = { 1, 2, std::unexpected("NOT INT") };
    {
        fake_vector<int>::contructed_count = 0;
        auto exp_error = has_error | ranges::collect<fake_vector<int>>();
        REQUIRE(fake_vector<int>::contructed_count == 0);
    }
    {
        fake_vector<int>::contructed_count = 0;
        auto exp_error = detail::collect_one_pass<fake_vector<int>>(has_error);
        REQUIRE(fake_vector<int>::contructed_count == 1);
    }
    //{
        //fake_vector<int>::contructed_count = 0;
        //todo test on input_range
        //auto test = std::views::iota(0) 
        //    | std::views::transform([&](auto i) { return has_error[i];});
        // i need a weakly_incrementable only type in iota to get input_range
        //static_assert(std::ranges::random_access_range<decltype(test)>);
        //REQUIRE(fake_vector<int>::contructed_count == 1);
    //}
}



// -------------- CASES NOT WORKING FTM --------------
//TEST_CASE("conversion test") {
//    VecOfExp has_error = { 1, 2, std::unexpected("NOT INT") };
//
//    // should allow conversion? 
//    auto exp_error = has_error | ranges::collect<std::vector<float>>();
//
//}


#include <memory_resource>
#include <functional>
#include <array>
TEST_CASE("allocator") {
    VecOfExp no_error = { 1, 2, 3 };
    char buffer[256];
    std::pmr::monotonic_buffer_resource res(std::begin(buffer), std::size(buffer));

    using exp_generator = std::function<
        std::expected<std::pmr::vector<int>, std::string>(void)>;
    std::vector<exp_generator> construct_exps {
        [&] {return ranges::collect<std::pmr::vector<int>>(no_error, &res); },
        [&] {return no_error | ranges::collect<std::pmr::vector<int>>(&res); },
        // those are not permitted by std::ranges::to. Probably shouldnt be alloewd here aswell
        [&] {return no_error | ranges::collect<std::pmr::vector>(&res); },
        [&] {return ranges::collect<std::pmr::vector>(no_error, &res); },
    };

    for (auto&& exp_generator : construct_exps)
    {
        auto exp = exp_generator();
        REQUIRE(exp.has_value());
        REQUIRE(exp->get_allocator().resource()->is_equal(res));
    }
}

//NESTED WIP
//TEST_CASE("acts like ranges::to if no potential underneath") {

    //std::ranges::input_range<std::vector>;

    //std::vector<std::vector<int>> vec2d = { 
    //    {1, 2, 3},
    //    {4, 5, 6},
    //    {7, 8, 9},
    //};
    //auto res = vec2d | ranges::collect<std::list<std::list<int>>>();
    //REQUIRE(std::same_as<decltype(res), std::list<std::list<int>>>);
    //const std::list<std::list<int>> lst2d = {
    //{1, 2, 3},
    //{4, 5, 6},
    //{7, 8, 9},
    //};
    //REQUIRE(res == lst2d);
//}

//// TODO 
//// for reservable container + sized range reserve before filling OK
//// impl for multiple containers OK
//// for forward_range check first then fill result OK
////  -> check input_ranges pass
//// impl for optional/potential-like : OK
////  * be able to construct error return type for both case OK
////  * deduce return type OK
////  * test with optional custom OK
////  * test with expected custom OK
//// allow range value conversion if possible
//// work with associative types (Maybe OK but need tests)
//// custom allocator and following args OK
//// constexpr tests
//// ----------------------------------------------------------------------
//// Nested containers notes : 
////  * enable nested containers ?
////  * then for non expected, optional act like range::to
//// if not allowing nested can still alows ranges::to behavior on 1 dimension container
// 
// Nested implementation note
// input : list list exp list 
// ret Container type : vec, lst, lst 
// -> exp vec lst lst
// 
// 
// input : list exp list list 
// ret Container type : vec, lst, lst 
// -> exp vec lst lst
// 
// EXAMPLE WORKINGS LIKE RANGES::TO 
// 
// input : list list list 
// ret Container type : vec list queue
// -> vec list queue
// 
// IN Short : first potential in front and everything 
// 
// to verif : 
// input container dimensionality by by passing first potential == output container dimensionality
// TODO EVEN IF NOT working on nested container
// 
// to do: 
//  - meta function checking if potentential like type underneath
//  - if no potential like type underneath -> act like ranges::to
//  - get return type recursively
//  - add constexpr if case range_value_t is container && not potential (in case both is possible)
//     then 
//     return range | transform(collect<range_value_t<Container>>) | collect<Container>()
//
//// Impl question :
//// Is two pass even a good idea or should i always allocate as I check?
//// for non expected, optional acting like range::to ? 
//// should i work for std::expected + optional only and return accordingly or with expected and optional like and return those type?
////  -> not much addition for those custom types and more general :  good thing imo
//// enable nested? if multiple nested expected/optional layers, return only the expected of the first or do them all when possible? 
//// What about cv_ref range value types? 
//// Is std::vector default a good things ? 