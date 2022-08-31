#pragma once
#include "then.hpp"


namespace execution{
	namespace upon_detail{
        using namespace ns_type_list;
        using ns_type_traits::member_type;
        using ns_type_traits::movable_value;
        using then_detail::receiver_impl;

        template<receiver R, typename F>
        using upon_error_receiver_t = receiver_impl<std::remove_cvref_t<R>, std::remove_cvref_t<F>, set_error_t>;
        template<receiver R, typename F>
        using upon_stopped_receiver_t = receiver_impl<std::remove_cvref_t<R>, std::remove_cvref_t<F>, set_stopped_t>;

        template<sender S, movable_value F>
        struct upon_error_sender_impl{
        private:
            template<typename T, typename = void>
            struct type_or_empty : public std::type_identity<type_list<T>>{};
            template<typename T>
            struct type_or_empty<T, std::enable_if_t<std::is_void_v<T>>> : public std::type_identity<type_list<>>{};
            template<typename... Ts>
            using result = type_list<typename type_or_empty<std::invoke_result_t<F, Ts>>::type...>;
            template<typename E>
            struct completion_signatures_of_E{
                using type_list_from_S = include_or_merge_t<value_types_of_t<S, E, type_list, type_list>, error_types_of_t<S, E, result>>;

                template<template<typename...>typename Tuple, template<typename...>typename Variant>
                using value_types = type_list_deep_apply_t<type_list_from_S, Variant, Tuple>;

                template<template<typename...>typename Variant>
                using error_types = Variant<std::exception_ptr>;

			    static constexpr bool sends_stopped = completion_signatures_of_t<S, E>::sends_stopped;

                using completion_signatures = completion_signatures_to_list_t<value_types, error_types, sends_stopped>;
            };
            template<typename E, typename = void>
            struct completion_signatures_SFINAE : public std::type_identity<dependent_completion_signatures<E>>{};
            template<typename E>
            struct completion_signatures_SFINAE<E, std::void_t<completion_signatures_of_E<E>>> : public std::type_identity<typename completion_signatures_of_E<E>::completion_signatures>{};
            template<typename E>
            using completion_signatures_SFINAE_t = typename completion_signatures_SFINAE<E>::type;
        public:

            [[no_unique_address]] S pre_sender;
            [[no_unique_address]] F func;

            template<typename Self, receiver R> requires std::is_same_v<std::remove_cvref_t<Self>, upon_error_sender_impl> && sender_to<member_type<Self&&, S>, upon_error_receiver_t<R&&, F>>
            [[nodiscard]] friend auto tag_invoke(connect_t, Self&& s, R&& r) noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&> && std::is_nothrow_constructible_v<F, member_type<Self&&, F>> && nothrow_connect_invocable<member_type<Self&&, S>, upon_error_receiver_t<R&&, F>>){
				return execution::connect(std::forward<Self>(s).pre_sender, upon_error_receiver_t<R, F>{ std::forward<R>(r), std::forward<Self>(s).func });
            }
            
            template<typename Self, typename E> requires std::is_same_v<std::remove_cvref_t<Self>, upon_error_sender_impl> && sender<S, E>
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<completion_signatures_SFINAE_t<E>>)->completion_signatures_SFINAE_t<E>{
				return completion_signatures_SFINAE_t<E>{};
            }

            template<typename Self, typename E> requires std::is_same_v<std::remove_cvref_t<Self>, upon_error_sender_impl> && (!sender<S, E>)
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<dependent_completion_signatures<E>>)->dependent_completion_signatures<E>{
				return dependent_completion_signatures<E>{};
            }

			template<typename Self, typename CPO> requires std::is_same_v<std::remove_cvref_t<Self>, upon_error_sender_impl> && std::invocable<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>
			[[nodiscard]] friend auto tag_invoke(get_completion_scheduler_t<CPO> tag, Self&& s) noexcept(std::is_nothrow_invocable_v<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>){
				return tag(std::forward<Self>(s).pre_sender);
			}

        };

        template<sender S, movable_value F>
        struct upon_stopped_sender_impl{
        private:
            template<typename T, typename = void>
            struct type_or_empty : public std::type_identity<type_list<T>>{};
            template<typename T>
            struct type_or_empty<T, std::enable_if_t<std::is_void_v<T>>> : public std::type_identity<type_list<>>{};
            template<typename... Ts>
            using result = typename type_or_empty<std::invoke_result_t<F, Ts...>>::type;
            template<typename E>
            struct completion_signatures_of_E{
                using type_list_from_S = include_or_add_t<value_types_of_t<S, E, type_list, type_list>, result<>>;

                template<template<typename...>typename Tuple, template<typename...>typename Variant>
                using value_types = type_list_deep_apply_t<type_list_from_S, Variant, Tuple>;

                template<template<typename...>typename Variant>
                using error_types = error_types_of_t<S, E, Variant>;

			    static constexpr bool sends_stopped = false;

                using completion_signatures = completion_signatures_to_list_t<value_types, error_types, sends_stopped>;
            };
            template<typename E, typename = void>
            struct completion_signatures_SFINAE : public std::type_identity<dependent_completion_signatures<E>>{};
            template<typename E>
            struct completion_signatures_SFINAE<E, std::void_t<completion_signatures_of_E<E>>> : public std::type_identity<typename completion_signatures_of_E<E>::completion_signatures>{};
            template<typename E>
            using completion_signatures_SFINAE_t = typename completion_signatures_SFINAE<E>::type;
        public:

            [[no_unique_address]] S pre_sender;
            [[no_unique_address]] F func;

            template<typename Self, receiver R> requires std::is_same_v<std::remove_cvref_t<Self>, upon_stopped_sender_impl> && sender_to<member_type<Self&&, S>, upon_stopped_receiver_t<R&&, F>>
            [[nodiscard]] friend auto tag_invoke(connect_t, Self&& s, R&& r) noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&> && std::is_nothrow_constructible_v<F, member_type<Self&&, F>> && nothrow_connect_invocable<member_type<Self&&, S>, upon_stopped_receiver_t<R&&, F>>){
				return execution::connect(std::forward<Self>(s).pre_sender, upon_stopped_receiver_t<R, F>{ std::forward<R>(r), std::forward<Self>(s).func });
            }
            
            template<typename Self, typename E> requires std::is_same_v<std::remove_cvref_t<Self>, upon_stopped_sender_impl> && sender<S, E>
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<completion_signatures_SFINAE_t<E>>)->completion_signatures_SFINAE_t<E>{
				return completion_signatures_SFINAE_t<E>{};
            }

            template<typename Self, typename E> requires std::is_same_v<std::remove_cvref_t<Self>, upon_stopped_sender_impl> && (!sender<S, E>)
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<dependent_completion_signatures<E>>)->dependent_completion_signatures<E>{
				return dependent_completion_signatures<E>{};
            }

			template<typename Self, typename CPO> requires std::is_same_v<std::remove_cvref_t<Self>, upon_stopped_sender_impl> && std::invocable<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>
			[[nodiscard]] friend auto tag_invoke(get_completion_scheduler_t<CPO> tag, Self&& s) noexcept(std::is_nothrow_invocable_v<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>){
				return tag(std::forward<Self>(s).pre_sender);
			}

        };

        template<sender S, ns_type_traits::movable_value F>
        using upon_error_sender = upon_error_sender_impl<std::remove_cvref_t<S>, std::remove_cvref_t<F>>;
        template<sender S, ns_type_traits::movable_value F>
        using upon_stopped_sender = upon_stopped_sender_impl<std::remove_cvref_t<S>, std::remove_cvref_t<F>>;
	}

	struct upon_error_t{
		template<ns_type_traits::movable_value F>
        [[nodiscard]] inline auto operator()(F &&f) const noexcept;

        template<sender S, ns_type_traits::movable_value F> requires tag_invocable_with_completion_scheduler<upon_error_t, set_error_t, S&&, F&&> && sender<tag_invoke_result_t<upon_error_t, completion_scheduler_for<S&&, set_error_t>, S&&, F&&>>
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(nothrow_tag_invocable<upon_error_t, completion_scheduler_for<S&&, set_error_t>, S&&, F&&>)->tag_invoke_result_t<upon_error_t, completion_scheduler_for<S&&, set_error_t>, S&&, F&&>{
			auto cs = get_completion_scheduler<set_error_t>(s); // guarantee the order of evaluation
            return tag_invoke(upon_error_t{}, std::move(cs), std::forward<S>(s), std::forward<F>(f));
        }

        template<sender S, ns_type_traits::movable_value F> requires tag_invocable<upon_error_t, S&&, F&&> && (!tag_invocable_with_completion_scheduler<upon_error_t, set_error_t, S&&, F&&>) && sender<tag_invoke_result_t<upon_error_t, S&&, F&&>>
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(nothrow_tag_invocable<upon_error_t, S&&, F&&>)->tag_invoke_result_t<upon_error_t, S&&, F&&>{
            return tag_invoke(upon_error_t{}, std::forward<S>(s), std::forward<F>(f));
        }

        template<sender S, ns_type_traits::movable_value F> requires (!tag_invocable<upon_error_t, S&&, F&&>) && (!tag_invocable_with_completion_scheduler<upon_error_t, set_error_t, S&&, F&&>)
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<S>, S&&> && std::is_nothrow_constructible_v<std::remove_cvref_t<F>, F&&>){
			return upon_detail::upon_error_sender<S, F>{ std::forward<S>(s), std::forward<F>(f) };
        }
    };
    inline constexpr upon_error_t upon_error{};
	template<sender S, ns_type_traits::movable_value F>
    using upon_error_result_t = decltype(upon_error(std::declval<S>(), std::declval<F>()));
    template<typename S, typename F>
    concept nothrow_upon_error_invocable = sender<S> && ns_type_traits::movable_value<F> && noexcept(upon_error(std::declval<S>(), std::declval<F>()));

    struct upon_stopped_t{
		template<ns_type_traits::movable_value F>
        [[nodiscard]] inline auto operator()(F &&f) const noexcept;

        template<sender S, ns_type_traits::movable_value F> requires tag_invocable_with_completion_scheduler<upon_stopped_t, set_stopped_t, S&&, F&&> && sender<tag_invoke_result_t<upon_stopped_t, completion_scheduler_for<S&&, set_stopped_t>, S&&, F&&>>
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(nothrow_tag_invocable<upon_stopped_t, completion_scheduler_for<S&&, set_stopped_t>, S&&, F&&>)->tag_invoke_result_t<upon_stopped_t, completion_scheduler_for<S&&, set_stopped_t>, S&&, F&&>{
			auto cs = get_completion_scheduler<set_stopped_t>(s); // guarantee the order of evaluation
			return tag_invoke(upon_stopped_t{}, std::move(cs), std::forward<S>(s), std::forward<F>(f));
        }

        template<sender S, ns_type_traits::movable_value F> requires tag_invocable<upon_stopped_t, S&&, F&&> && (!tag_invocable_with_completion_scheduler<upon_stopped_t, set_stopped_t, S&&, F&&>) && sender<tag_invoke_result_t<upon_stopped_t, S&&, F&&>>
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(nothrow_tag_invocable<upon_stopped_t, S&&, F&&>)->tag_invoke_result_t<upon_stopped_t, S&&, F&&>{
			return tag_invoke(upon_stopped_t{}, std::forward<S>(s), std::forward<F>(f));
        }

        template<sender S, ns_type_traits::movable_value F> requires (!tag_invocable<upon_stopped_t, S&&, F&&>) && (!tag_invocable_with_completion_scheduler<upon_stopped_t, set_stopped_t, S&&, F&&>)
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<S>, S&&> && std::is_nothrow_constructible_v<std::remove_cvref_t<F>, F&&>){
			return upon_detail::upon_stopped_sender<S, F>{ std::forward<S>(s), std::forward<F>(f) };
        }
    };
	inline constexpr upon_stopped_t upon_stopped{};
	template<sender S, ns_type_traits::movable_value F>
    using upon_stopped_result_t = decltype(upon_stopped(std::declval<S>(), std::declval<F>()));
    template<typename S, typename F>
    concept nothrow_upon_stopped_invocable = sender<S> && ns_type_traits::movable_value<F> && noexcept(upon_stopped(std::declval<S>(), std::declval<F>()));

    namespace upon_detail{
		template<movable_value F>
        struct upon_error_closure_t : public sender_adaptor_closure<upon_error_closure_t<F>>{
			[[no_unique_address]] std::tuple<F> data;

			[[nodiscard]] decltype(auto) get() noexcept{
				return data;
			}
			[[nodiscard]] auto get_tag() const noexcept{
				return upon_error_t{};
			}
        };
        template<movable_value F>
        struct upon_stopped_closure_t : public sender_adaptor_closure<upon_stopped_closure_t<F>>{
			[[no_unique_address]] std::tuple<F> data;

			[[nodiscard]] decltype(auto) get() noexcept{
				return data;
			}
			[[nodiscard]] auto get_tag() const noexcept{
				return upon_stopped_t{};
			}
        };
    }

    template<ns_type_traits::movable_value F>
    [[nodiscard]] inline auto upon_error_t::operator()(F&& f) const noexcept{
		return upon_detail::upon_error_closure_t<std::remove_cvref_t<F>>{ {}, { std::forward<F>(f) } };
    }
    template<ns_type_traits::movable_value F>
    [[nodiscard]] inline auto upon_stopped_t::operator()(F&& f) const noexcept{
		return upon_detail::upon_stopped_closure_t<std::remove_cvref_t<F>>{ {}, { std::forward<F>(f) } };
    }
}