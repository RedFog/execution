#pragma once
#include <tuple>
#include <type_traits>
#include "type_traits_ex.hpp"

namespace ns_type_list{
    template<typename... Ts>
    struct type_list{};

    template<typename... Ts>
    struct type_count : public std::integral_constant<std::size_t, sizeof...(Ts)>{};
    template<typename... Ts>
    inline constexpr std::size_t type_count_v = type_count<Ts...>::value;

    template<typename TL>
    struct type_list_count;
    template<typename... Ts>
    struct type_list_count<type_list<Ts...>> : public type_count<Ts...>{};
    template<typename TL>
    inline constexpr std::size_t type_list_count_v = type_list_count<TL>::value;

    template<typename FL>
    struct to_type_list;
    template<template<typename...>typename F, typename... Ts>
    struct to_type_list<F<Ts...>> : public std::type_identity<type_list<Ts...>>{};
    template<typename FL>
    using to_type_list_t = typename to_type_list<FL>::type;

    template<typename... TLs>
    struct type_list_concat;
    template<>
    struct type_list_concat<> : public std::type_identity<type_list<>>{};
    template<typename... Ts>
    struct type_list_concat<type_list<Ts...>> : public std::type_identity<type_list<Ts...>>{};
    template<typename... Ts, typename... Ys, typename... TLs>
    struct type_list_concat<type_list<Ts...>, type_list<Ys...>, TLs...> : public type_list_concat<type_list<Ts..., Ys...>, TLs...>{};
    template<typename... TLs>
    using type_list_concat_t = typename type_list_concat<TLs...>::type;

    template<typename TL, template<typename...>typename F>
    struct type_list_apply;
    template<typename... Ts, template<typename...>typename F>
    struct type_list_apply<type_list<Ts...>, F> : public std::type_identity<F<Ts...>>{};
    template<typename TL, template<typename...>typename F>
    using type_list_apply_t = typename type_list_apply<TL, F>::type;

    template<typename TL, template<typename>typename UnF>
    struct type_list_map;
    template<typename... Ts, template<typename>typename UnF>
    struct type_list_map<type_list<Ts...>, UnF> : public std::type_identity<type_list<typename UnF<Ts>::type...>>{};
    template<typename TL, template<typename>typename UnF>
    using type_list_map_t = typename type_list_map<TL, UnF>::type;

    template<typename TL, typename S, template<typename, typename>typename Acc>
    struct type_list_fold;
    template<typename S, template<typename, typename>typename Acc>
    struct type_list_fold<type_list<>, S, Acc> : public std::type_identity<S>{};
    template<typename S, typename T, template<typename, typename>typename Acc>
    struct type_list_fold<type_list<T>, S, Acc> : public Acc<S, T>{};
    template<typename S, typename T, typename... Ts, template<typename, typename>typename Acc>
    struct type_list_fold<type_list<T, Ts...>, S, Acc> : public type_list_fold<type_list<Ts...>, typename Acc<S, T>::type, Acc>{};

    template<typename TL, typename T>
    struct include_or_add;
    template<typename... Ts, typename T>
    struct include_or_add<type_list<Ts...>, T> : public std::conditional<ns_type_traits::any_of<T, Ts...>, type_list<Ts...>, type_list<Ts..., T>>{};
    template<typename TL, typename T>
    using include_or_add_t = typename include_or_add<TL, T>::type;

    template<typename TL>
    struct type_list_unique;
    template<>
    struct type_list_unique<type_list<>> : public std::type_identity<type_list<>>{};
    template<typename T>
    struct type_list_unique<type_list<T>> : public std::type_identity<type_list<T>>{};
    template<typename T, typename Y>
    struct type_list_unique<type_list<T, Y>> : public std::conditional<std::is_same_v<T, Y>, type_list<T>, type_list<T, Y>>{};
    template<typename T, typename Y, typename... Ts>
    struct type_list_unique<type_list<T, Y, Ts...>> : public type_list_fold<type_list<Ts...>, typename type_list_unique<type_list<T, Y>>::type, include_or_add>{};
    template<typename TL>
    using type_list_unique_t = typename type_list_unique<TL>::type;

    template<typename TL, typename... TLs>
    struct include_or_merge;
	template<typename... Ts>
	struct include_or_merge<type_list<Ts...>> : public std::type_identity<type_list<Ts...>>{};
    template<typename... Ts, typename... Ys>
	struct include_or_merge<type_list<Ts...>, type_list<Ys...>> : public type_list_concat<type_list<Ts...>, type_list_apply_t<type_list_unique_t<type_list<std::conditional_t<ns_type_traits::any_of<Ys, Ts...>, type_list<>, type_list<Ys>>...>>, type_list_concat_t>> {};
    template<typename... Ts, typename... Ys, typename... TLs>
    struct include_or_merge<type_list<Ts...>, type_list<Ys...>, TLs...> : public include_or_merge<typename include_or_merge<type_list<Ts...>, type_list<Ys...>>::type, TLs...>{};
    template<typename... TLs>
    using include_or_merge_t = typename include_or_merge<TLs...>::type;

    template<typename... TLs>
    struct type_set_concat;
    template<>
    struct type_set_concat<> : public std::type_identity<type_list<>>{};
    template<typename TL>
    struct type_set_concat<TL> : public std::type_identity<type_list_unique_t<TL>>{};
    template<typename TL, typename... TLs>
    struct type_set_concat<TL, TLs...> : public include_or_merge<type_list_unique_t<TL>, TLs...>{};
    template<typename... TLs>
    using type_set_concat_t = typename type_set_concat<TLs...>::type;

    template<typename TL, template<typename>typename Pred>
    struct type_list_any_of;
    template<template<typename>typename Pred>
	struct type_list_any_of<type_list<>, Pred> : public std::false_type {};
    template<typename T, template<typename>typename Pred>
	struct type_list_any_of<type_list<T>, Pred> : public Pred<T> {};
    template<typename T, typename... Ts, template<typename>typename Pred>
	struct type_list_any_of<type_list<T, Ts...>, Pred> : public std::conditional_t<Pred<T>::value, std::true_type, type_list_any_of<type_list<Ts...>, Pred>> {};
    template<typename TL, template<typename>typename Pred>
	inline constexpr bool type_list_any_of_v = type_list_any_of<TL, Pred>::value;

    template<typename TL, template<typename>typename Pred>
	struct type_list_all_of;
    template<template<typename>typename Pred>
	struct type_list_all_of<type_list<>, Pred> : public std::true_type {};
    template<typename T, template<typename>typename Pred>
	struct type_list_all_of<type_list<T>, Pred> : public Pred<T> {};
    template<typename T, typename... Ts, template<typename>typename Pred>
	struct type_list_all_of<type_list<T, Ts...>, Pred> : public std::conditional_t<Pred<T>::value, type_list_all_of<type_list<Ts...>, Pred>, std::false_type> {};
    template<typename TL, template<typename>typename Pred>
	inline constexpr bool type_list_all_of_v = type_list_all_of<TL, Pred>::value;

    namespace select_detail{
        template<template<typename>typename Pred>
        struct select_helper{
			template<typename S, typename T>
            struct apply;
			template<typename T, typename... Ts>
            struct apply<type_list<Ts...>, T> : public std::conditional<Pred<T>::value, type_list<Ts..., T>, type_list<Ts...>>{};
        };
    }
    template<typename TL, template<typename>typename Pred>
    struct type_list_select : public type_list_fold<TL, type_list<>, select_detail::select_helper<Pred>::template apply>{};
    template<typename TL, template<typename>typename Pred>
    using type_list_select_t = typename type_list_select<TL, Pred>::type;

    namespace product_detail{
        template<template<typename, typename>typename BiF>
        struct product_helper{
            template<typename S, typename T>
			struct apply;
			template<typename... Ts>
            struct apply<type_list<Ts...>, type_list<>> : public std::type_identity<type_list<>>{};
            template<typename T, typename... Ts>
            struct apply<type_list<Ts...>, type_list<T>> : public std::type_identity<type_list<typename BiF<Ts, T>::type...>>{};
            template<typename... Ts, typename... Us>
            struct apply<type_list<Ts...>, type_list<Us...>> : public type_list_concat<typename apply<type_list<Ts...>, type_list<Us>>::type...>{};
        };
    }
    template<template<typename, typename>typename BiF, typename... TLs>
    struct type_list_product;
    template<template<typename, typename>typename BiF>
    struct type_list_product<BiF> : public std::type_identity<type_list<>>{};
    template<template<typename, typename>typename BiF, typename... TLs>
    struct type_list_product<BiF, type_list<>, TLs...> : public std::type_identity<type_list<>>{};
    template<template<typename, typename>typename BiF, typename TL, typename... TLs>
    struct type_list_product<BiF, TL, TLs...> : public type_list_fold<type_list<TLs...>, TL, product_detail::product_helper<BiF>::template apply>{};
    template<template<typename, typename>typename BiF, typename... TLs>
    using type_list_product_t = typename type_list_product<BiF, TLs...>::type;

    template<typename TL, template<typename...>typename... Applies>
    struct type_list_deep_apply;
    template<typename T>
    struct type_list_deep_apply<T> : public std::type_identity<T>{};
    template<typename... Ts, template<typename...>typename Apply>
    struct type_list_deep_apply<type_list<Ts...>, Apply> : public std::type_identity<Apply<Ts...>>{};
    template<typename... Ts, template<typename...>typename Apply, template<typename...>typename... Applies>
    struct type_list_deep_apply<type_list<Ts...>, Apply, Applies...> : public std::type_identity<Apply<typename type_list_deep_apply<Ts, Applies...>::type...>>{};
    template<typename TL, template<typename...>typename... Applies>
    using type_list_deep_apply_t = typename type_list_deep_apply<TL, Applies...>::type;
}