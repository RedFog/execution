#pragma once
#include <functional>
#include "execution.hpp"

namespace execution{
    namespace then_detail{
        using namespace ns_type_list;
        using ns_type_traits::member_type;
        using ns_type_traits::movable_value;

        template<typename F, typename... Vs>
        concept f_return_void = std::invocable<F, Vs...> && std::is_void_v<std::invoke_result_t<F, Vs...>>;

        template<typename F, typename... Vs>
        concept f_return_value = std::invocable<F, Vs...> && !std::is_void_v<std::invoke_result_t<F, Vs...>>;

        template<typename R, typename F, typename Tag> requires ns_type_traits::any_of<Tag, set_value_t, set_error_t, set_stopped_t>
        struct receiver_impl{
            [[no_unique_address]] R out_receiver;
            [[no_unique_address]] F func;

            template<typename... Vs> requires f_return_void<F&&, Vs&&...>
            static void invoke_this(Tag, receiver_impl&& r, Vs&&... vs) noexcept{
                try {
                    std::invoke(std::move(r.func), std::forward<Vs>(vs)...);
                    execution::set_value(std::move(r.out_receiver));
                } catch (...){
                    execution::set_error(std::move(r.out_receiver), std::current_exception());
                }
            }
            template<typename... Vs> requires f_return_value<F&&, Vs&&...>
            static void invoke_this(Tag, receiver_impl&& r, Vs&&... vs) noexcept{
                try {
					execution::set_value(std::move(r.out_receiver), std::invoke(std::move(r.func), std::forward<Vs>(vs)...));
                } catch (...){
                    execution::set_error(std::move(r.out_receiver), std::current_exception());
                }
            }

            template<typename... Vs>
            friend void tag_invoke(Tag, receiver_impl&& r, Vs&&... vs) noexcept{
                invoke_this(Tag{}, std::move(r), std::forward<Vs>(vs)...);
            }

            // for satisfying concept `receiver`.
            friend void tag_invoke(set_error_t, receiver_impl&& r, std::exception_ptr e) noexcept requires std::same_as<Tag, set_error_t>{
				if constexpr (std::invocable<F, std::exception_ptr>)
                    invoke_this(set_error_t{}, std::move(r), e);
				else
                    execution::set_error(std::move(r.out_receiver), e);
            }

			template<typename Tag2, typename... Vs> requires (!std::same_as<Tag, Tag2>) && ns_type_traits::any_of<Tag2, set_value_t, set_error_t, set_stopped_t>
			friend void tag_invoke(Tag2 tag, receiver_impl&& r, Vs&&... vs) noexcept(std::is_nothrow_invocable_v<Tag2, R&&, Vs&&...>){
				tag(std::move(r.out_receiver), std::forward<Vs>(vs)...);
            }
        };

        template<receiver R, typename F>
        using receiver_t = receiver_impl<std::remove_cvref_t<R>, std::remove_cvref_t<F>, set_value_t>;

        template<sender S, movable_value F>
        struct sender_impl{
        private:
            template<typename T, typename = void>
            struct type_or_empty : public std::type_identity<type_list<T>>{};
            template<typename T>
            struct type_or_empty<T, std::enable_if_t<std::is_void_v<T>>> : public std::type_identity<type_list<>>{};
            template<typename... Ts>
            using result = type_list<typename type_or_empty<std::invoke_result_t<F, Ts...>>::type>;
            template<typename... TLs>
            using flatten_and_unique = type_list_unique_t<type_list_concat_t<TLs...>>;
            template<typename E>
            struct completion_signatures_of_E{
                using type_list_from_S = value_types_of_t<S, E, result, flatten_and_unique>;

                template<template<typename...>typename Tuple, template<typename...>typename Variant>
                using value_types = type_list_deep_apply_t<type_list_from_S, Variant, Tuple>;

                template<template<typename...>typename Variant>
                using error_types = type_list_apply_t<include_or_add_t<error_types_of_t<S, E, type_list>, std::exception_ptr>, Variant>;

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

            template<typename Self, receiver R> requires std::same_as<std::remove_cvref_t<Self>, sender_impl> && sender_to<member_type<Self&&, S>, receiver_t<R&&, F>>
            [[nodiscard]] friend auto tag_invoke(connect_t, Self&& s, R&& r) noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&> && std::is_nothrow_constructible_v<F, member_type<Self&&, F>> && nothrow_connect_invocable<member_type<Self&&, S>, receiver_t<R&&, F>>){
                return execution::connect(std::forward<Self>(s).pre_sender, receiver_t<R&&, F>{ std::forward<R>(r), std::forward<Self>(s).func });
            }

            template<typename Self, typename E> requires std::same_as<std::remove_cvref_t<Self>, sender_impl> && sender<S, E>
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<completion_signatures_SFINAE_t<E>>)->completion_signatures_SFINAE_t<E>{
				return completion_signatures_SFINAE_t<E>{};
            }

            template<typename Self, typename E> requires std::same_as<std::remove_cvref_t<Self>, sender_impl> && (!sender<S, E>)
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<dependent_completion_signatures<E>>)->dependent_completion_signatures<E>{
				return dependent_completion_signatures<E>{};
            }

			template<typename Self, typename CPO> requires std::same_as<std::remove_cvref_t<Self>, sender_impl> && std::invocable<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>
			[[nodiscard]] friend auto tag_invoke(get_completion_scheduler_t<CPO> tag, Self&& s) noexcept(std::is_nothrow_invocable_v<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>){
				return tag(std::forward<Self>(s).pre_sender);
			}

        };

        template<sender S, ns_type_traits::movable_value F>
        using then_sender = sender_impl<std::remove_cvref_t<S>, std::remove_cvref_t<F>>;
    }

    struct then_t{
		template<ns_type_traits::movable_value F>
        [[nodiscard]] inline auto operator()(F &&f) const noexcept;

        template<sender S, ns_type_traits::movable_value F> requires tag_invocable_with_completion_scheduler<then_t, set_value_t, S&&, F&&> && sender<tag_invoke_result_t<then_t, completion_scheduler_for<S&&, set_value_t>, S&&, F&&>>
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(nothrow_tag_invocable<then_t, completion_scheduler_for<S&&, set_value_t>, S&&, F&&>)->tag_invoke_result_t<then_t, completion_scheduler_for<S&&, set_value_t>, S&&, F&&>{
			auto cs = get_completion_scheduler<set_value_t>(s); // guarantee the order of evaluation
            return tag_invoke(then_t{}, std::move(cs), std::forward<S>(s), std::forward<F>(f));
        }

        template<sender S, ns_type_traits::movable_value F> requires tag_invocable<then_t, S&&, F&&> && (!tag_invocable_with_completion_scheduler<then_t, set_value_t, S&&, F&&>) && sender<tag_invoke_result_t<then_t, S&&, F&&>>
            [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(nothrow_tag_invocable<then_t, S&&, F&&>)->tag_invoke_result_t<then_t, S&&, F&&>{
            return tag_invoke(then_t{}, std::forward<S>(s), std::forward<F>(f));
        }

        template<sender S, ns_type_traits::movable_value F> requires (!tag_invocable<then_t, S&&, F&&>) && (!tag_invocable_with_completion_scheduler<then_t, set_value_t, S&&, F&&>)
            [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<S>, S&&> && std::is_nothrow_constructible_v<std::remove_cvref_t<F>, F&&>){
            return then_detail::then_sender<S, F>{ std::forward<S>(s), std::forward<F>(f) };
        }
    };
    inline constexpr then_t then{};
	template<sender S, ns_type_traits::movable_value F>
    using then_result_t = decltype(then(std::declval<S>(), std::declval<F>()));
    template<typename S, typename F>
    concept nothrow_then_invocable = sender<S> && ns_type_traits::movable_value<F> && noexcept(then(std::declval<S>(), std::declval<F>()));

    namespace then_detail{
		template<movable_value F>
        struct then_closure_t : public sender_adaptor_closure<then_closure_t<F>>{
			[[no_unique_address]] std::tuple<F> data;

			[[nodiscard]] decltype(auto) get() noexcept{
				return data;
			}
			[[nodiscard]] auto get_tag() const noexcept{
				return then_t{};
			}
        };
    }

    template<ns_type_traits::movable_value F>
    [[nodiscard]] inline auto then_t::operator()(F&& f) const noexcept{
        return then_detail::then_closure_t<std::remove_cvref_t<F>>{ {}, { std::forward<F>(f) } };
    }
}