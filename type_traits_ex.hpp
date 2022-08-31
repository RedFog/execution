#pragma once
#include <type_traits>

namespace ns_type_traits{
	namespace member_type_detail{
        template<typename cvref, typename T>
        struct member_type : public std::type_identity<T>{};
        template<typename O, typename T>
		struct member_type<O&, T> : public std::type_identity<T&>{};
		template<typename O, typename T>
		struct member_type<O&&, T> : public std::type_identity<T&&>{};
        template<typename O, typename T>
		struct member_type<O const&, T> : public std::type_identity<T const&>{};
		template<typename O, typename T>
		struct member_type<O const&&, T> : public std::type_identity<T const&&>{};
        template<typename O, typename T>
		struct member_type<O volatile&, T> : public std::type_identity<T volatile&>{};
		template<typename O, typename T>
		struct member_type<O volatile&&, T> : public std::type_identity<T volatile&&>{};
        template<typename O, typename T>
		struct member_type<O const volatile&, T> : public std::type_identity<T const volatile&>{};
		template<typename O, typename T>
		struct member_type<O const volatile&&, T> : public std::type_identity<T const volatile&&>{};
		template<typename cvref, typename T>
		using member_type_t = typename member_type<cvref, T>::type;
    }
    template<typename Self, typename Mem>
    using member_type = member_type_detail::member_type_t<Self, Mem>;

	template<typename T>
	concept movable_value = std::move_constructible<std::remove_cvref_t<T>> && std::constructible_from<std::remove_cvref_t<T>, T>;

    template<typename T, typename... Ts>
	concept any_of = (std::is_same_v<T, Ts> || ...);

    template<typename T, typename... Ts>
	concept all_of = (std::is_same_v<T, Ts> && ...);

	template<typename... F>
	struct overload_t : public F...{ using F::operator()...; };
	template<typename... F>
	overload_t(F...)->overload_t<F...>;

	template<typename FL, template<typename...> typename F>
	struct is_instantiation_of : public std::false_type {};
	template<template<typename...> typename F, typename... Ts>
	struct is_instantiation_of<F<Ts...>, F> : public std::true_type {};
	template<typename FL, template<typename...> typename F>
	concept is_instantiation_of_v = is_instantiation_of<FL, F>::value;
}