#pragma once
#include <optional>
#include <utility>
#include <mutex>
#include "execution.hpp"

#define EXECUTION_WHEN_ALL_ALLOW_MULTI_RETURN_VALUE true

namespace execution{
	namespace when_all_detail{
		using namespace ns_type_list;
		using ns_type_traits::member_type;
		using ns_utility::monostate_variant;

		struct stop_callback_func{
			in_place_stop_source& src;
			void operator()() noexcept{ src.request_stop(); }
		};

		template<receiver R>
		struct when_all_env{
			template<typename Self> requires std::same_as<std::remove_cvref_t<Self>, when_all_env>
		    [[nodiscard]] friend auto tag_invoke(get_stop_token_t, Self&& env) noexcept{
				return std::forward<Self>(env).st;
		    }
			template<typename Tag, typename Self, typename... Args> requires std::same_as<std::remove_cvref_t<Self>, when_all_env> && (execution::forwarding_env_query(Tag{})) && std::invocable<Tag, env_of_t<R>, Args&&...>
			friend decltype(auto) tag_invoke(Tag tag, Self&& env, Args&&... args) noexcept(std::is_nothrow_invocable_v<Tag, env_of_t<R>, Args&&...>){
				return tag(std::forward<Self>(env).out_of_env, std::forward<Args>(args)...);
			}

			[[no_unique_address]] env_of_t<R> out_env;
		    in_place_stop_token st;
		};

		template<typename E>
		struct when_all_env_with_token{
			template<typename Tag, typename Self, typename... Args> requires std::same_as<std::remove_cvref_t<Self>, when_all_env_with_token> && std::invocable<Tag, E, Args&&...>
			friend auto tag_invoke(Tag tag, Self&& env, Args&&... args) noexcept(std::is_nothrow_invocable_v<Tag, E, Args&&...>)->std::invoke_result_t<Tag, E, Args&&...>;
			template<typename Self> requires std::same_as<std::remove_cvref_t<Self>, when_all_env_with_token>
			friend auto tag_invoke(get_stop_token_t, Self&& env) noexcept->in_place_stop_token;
		};

		template<sender... Ss>
		struct sender_impl{
			static_assert(sizeof...(Ss) > 0);

			[[no_unique_address]] std::tuple<Ss...> pre_senders;

			template<typename R, typename IS>
			struct operation;
			template<receiver R, std::size_t... Is>
			struct operation<R, std::index_sequence<Is...>>{
			private:
				template<typename S>
				struct variant_of_S{
					template<typename... Ts>
					using value_type_list = type_list<set_value_t, std::remove_cvref_t<Ts>...>;
					template<typename... Ts>
					using error_type_list = type_list<std::remove_cvref_t<Ts>...>;
					using value_types_list = value_types_of_t<S, env_of_t<R>, value_type_list, type_list>;
					using error_types_list = include_or_add_t<error_types_of_t<S, env_of_t<R>, error_type_list>, std::exception_ptr>;
					using data_type = type_list_deep_apply_t<type_set_concat_t<value_types_list, type_list<type_list<set_error_t>>, type_list<type_list<set_stopped_t>>>, monostate_variant, std::tuple>;
				};
				using data_type = std::tuple<typename variant_of_S<Ss>::data_type...>;
				using error_type = type_list_apply_t<include_or_merge_t<typename variant_of_S<Ss>::error_types_list...>, monostate_variant>;
				template<typename T>
				struct is_same_as{
					template<typename Y>
					using apply = std::is_same<T, Y>;
				};
				struct visit_functor{
					template<typename Tuple>
					struct is_what_tuple{
						static constexpr bool is_set_value = false;
						static constexpr bool is_set_error = false;
						static constexpr bool is_set_stopped = false;
					};
					template<typename... Args>
					struct is_what_tuple<std::tuple<set_value_t, Args...>>{
						static constexpr bool is_set_value = true;
						static constexpr bool is_set_error = false;
						static constexpr bool is_set_stopped = false;
					};
					template<typename... Args>
					struct is_what_tuple<std::tuple<set_error_t, Args...>>{
						static constexpr bool is_set_value = false;
						static constexpr bool is_set_error = true;
						static constexpr bool is_set_stopped = false;
					};
					template<typename... Args>
					struct is_what_tuple<std::tuple<set_stopped_t, Args...>>{
						static constexpr bool is_set_value = false;
						static constexpr bool is_set_error = false;
						static constexpr bool is_set_stopped = true;
					};
					template<typename Tuple>
					static constexpr bool is_set_value_v = is_what_tuple<std::remove_cvref_t<Tuple>>::is_set_value;
					template<typename Tuple>
					static constexpr bool is_set_error_v = is_what_tuple<std::remove_cvref_t<Tuple>>::is_set_error;
					template<typename Tuple>
					static constexpr bool is_set_stopped_v = is_what_tuple<std::remove_cvref_t<Tuple>>::is_set_stopped;
					template<typename Tuple>
					static constexpr bool is_invalid_v = !is_set_value_v<Tuple> && !is_set_error_v<Tuple> && !is_set_stopped_v<Tuple>;

					template<typename... Tuples> requires (is_set_error_v<Tuples> || ...)
					void operator()(Tuples&&... tuples) noexcept{
						std::visit(ns_type_traits::overload_t{
							[](std::monostate) noexcept{}, // never reach here
							[this]<typename E>(E&& e) noexcept{
								execution::set_error(std::move(op->out_receiver), std::forward<E>(e));
							},
						}, std::move(op->error));
					}
					template<typename... Tuples> requires (!(is_set_error_v<Tuples> || ...)) && (is_set_stopped_v<Tuples> || ...)
					void operator()(Tuples&&... tuples) noexcept{
						execution::set_stopped(std::move(op->out_receiver));
					}
					template<typename... Tuples> requires (is_set_value_v<Tuples> && ...)
					void operator()(Tuples&&... tuples) noexcept{
						this->invoke([this]<typename... Args>(Args&&... args) noexcept{
							try {
								execution::set_value(std::move(op->out_receiver), std::forward<Args>(args)...);
							} catch (...){
								execution::set_error(std::move(op->out_receiver), std::current_exception());
							}
						}, std::forward<Tuples>(tuples)...);
					}
					template<typename... Tuples>
					void operator()(Tuples&&... tuples) noexcept{} // never reach here

					template<typename F, typename Last>
					static void invoke(F&& func, Last&& last) noexcept{
						std::apply([&func]<typename... Args>(set_value_t, Args&&... args) noexcept{
							std::invoke(std::forward<F>(func), std::forward<Args>(args)...);
						}, std::forward<Last>(last));
					}
					template<typename F, typename First, typename... Tuples>
					static void invoke(F&& func, First&& first, Tuples&&... tuples) noexcept{
						invoke([&func, &first]<typename... OtherArgs>(OtherArgs&&... other_args) noexcept{
							std::apply([&func, &other_args...]<typename... Args>(set_value_t, Args&&... args){
								std::invoke(std::forward<F>(func), std::forward<Args>(args)..., std::forward<OtherArgs>(other_args)...);
							}, std::forward<First>(first));
						}, std::forward<Tuples>(tuples)...);
					}

					operation* op;
				};
			public:
				template<std::size_t I>
				struct receiver_impl{
					operation* op;
				};
				template<std::size_t I, typename... Args>
				friend void tag_invoke(set_value_t, receiver_impl<I>&& r, Args&&... args) noexcept{
					try {
						std::get<I>(r.op->data).template emplace<decayed_tuple<set_value_t, Args...>>(set_value_t{}, std::forward<Args>(args)...);
					} catch (...){
						execution::set_error(std::move(r), std::current_exception());
						return;
					}
					r.op->notify();
				}
				template<std::size_t I, typename E>
				friend void tag_invoke(set_error_t, receiver_impl<I>&& r, E&& e) noexcept{
					try {
						std::unique_lock<std::mutex> lock(r.op->error_mutex);
						if (r.op->error.index() == 0)
							r.op->error.template emplace<std::remove_cvref_t<E>>(std::forward<E>(e));
						std::get<I>(r.op->data).template emplace<decayed_tuple<set_error_t>>(set_error_t{});
					} catch (...){
						execution::set_error(std::move(r.op->out_receiver), std::current_exception());
						return;
					}
					r.op->notify();
				}
				template<std::size_t I>
				friend void tag_invoke(set_stopped_t, receiver_impl<I>&& r) noexcept{
					try {
						std::get<I>(r.op->data).template emplace<decayed_tuple<set_stopped_t>>(set_stopped_t{});
					} catch (...){
						execution::set_error(std::move(r.op->out_receiver), std::current_exception());
						return;
					}
					r.op->notify();
				}

				template<typename R2, typename... Ss2>
				operation(R2&& r, Ss2&&... ss) noexcept((std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&> && ... && nothrow_connect_invocable<Ss2&&, receiver_impl<Is>>))
					:out_receiver(std::forward<R2>(r)), ops{ std::forward_as_tuple(std::forward<Ss2>(ss)...), std::forward_as_tuple(receiver_impl<Is>{ this }...) }{}

				operation(operation const&) = delete;
				operation(operation&&) = delete;
				operation& operator=(operation const&) = delete;
				operation& operator=(operation&&) = delete;

				friend void tag_invoke(start_t, operation& op) noexcept{
					op.token.emplace(execution::get_stop_token(execution::get_env(op.out_receiver)), stop_callback_func{ op.src });
					if (op.src.stop_requested())
						execution::set_stopped(std::move(op.out_receiver));
					else
						apply([]<typename... Ops>(Ops&... ops) noexcept{
							(... , execution::start(ops));
						}, op.ops);
				}

				void notify() noexcept{
					if ((!std::holds_alternative<std::monostate>(std::get<Is>(data)) && ...))
						try {
							std::apply([this]<typename... Variants>(Variants&&... variants) noexcept{
								std::visit(visit_functor{ this }, std::forward<Variants>(variants)...);
							}, std::move(data));
						} catch (...){
							execution::set_error(std::move(out_receiver), std::current_exception());
						}
				}

			private:
				[[no_unique_address]] R out_receiver;
				[[no_unique_address]] tuple_connect_result_t<std::tuple<Ss...>, std::tuple<receiver_impl<Is>...>> ops;
				[[no_unique_address]] data_type data;
				[[no_unique_address]] error_type error;
				std::mutex error_mutex;
				in_place_stop_source src;
				std::optional<typename stop_token_of_t<env_of_t<R>>::template callback_type<stop_callback_func>> token;
			};

			template<std::size_t I, typename R, typename IS>
			[[nodiscard]] friend auto tag_invoke(get_env_t, typename operation<R, IS>::template receiver_impl<I> r) noexcept(nothrow_get_env_invocable<R> && std::is_nothrow_move_constructible_v<env_of_t<R>>){
				return when_all_env<R>{ execution::get_env(r.op->out_receiver), r.op->src.get_token() };
			}

			template<typename Self, receiver R> requires std::same_as<std::remove_cvref_t<Self>, sender_impl>
			[[nodiscard]] friend auto tag_invoke(connect_t, Self&& self, R&& out_r) noexcept((std::is_nothrow_constructible_v<Ss, member_type<Self&&, Ss>> && ... && std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&>)){
				return std::apply([&out_r]<typename... Ss2>(Ss2&&... ss) noexcept((std::is_nothrow_constructible_v<Ss, member_type<Self&&, Ss>> && ... && std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&>)){
					return operation<std::remove_cvref_t<R>, std::index_sequence_for<Ss...>>{ std::move(out_r), std::forward<Ss2>(ss)... };
				}, std::forward<Self>(self).pre_senders);
			}

		private:
			template<typename E>
            struct completion_signatures_of_E{
				template<typename A, typename B>
				using type_list_concat_delegate = type_list_concat<A, B>; // compatible with clang

				using type_list_from_Ss = type_list_product_t<type_list_concat_delegate, value_types_of_t<Ss, E, type_list, type_list>...>;

				using type_set_from_Ss = type_list_unique_t<type_list_from_Ss>;

                template<template<typename...>typename Tuple, template<typename...>typename Variant>
                using value_types = type_list_deep_apply_t<type_set_from_Ss, Variant, Tuple>;

                template<template<typename...>typename Variant>
                using error_types = type_list_apply_t<include_or_add_t<include_or_merge_t<error_types_of_t<Ss, E, type_list>...>, std::exception_ptr>, Variant>;

			    static constexpr bool sends_stopped = true;

#if EXECUTION_WHEN_ALL_ALLOW_MULTI_RETURN_VALUE
                using completion_signatures = completion_signatures_to_list_t<value_types, error_types, sends_stopped>;
#else
				// why I have to limit S to provide not more than one argument for `set_value`?
				// requirement from: https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2022/p2300r4.html#spec-execution.senders.adaptor.when_all
				template<typename... Ts> requires (sizeof...(Ts) <= 1)
				using zero_or_one = void;
				template<typename S>
				struct of_S{
					using check_if = value_types_of_t<S, E, decayed_tuple, zero_or_one>;
				};
				static constexpr bool valid = requires{ typename std::void_t<of_S<Ss>::check_if...>; };

				using completion_signatures = typename std::conditional_t<valid, completion_signatures_to_list<value_types, error_types, sends_stopped>, std::type_identity<dependent_completion_signatures<E>>>::type;
#endif
            };
            template<typename E, typename = void>
            struct completion_signatures_SFINAE : public std::type_identity<dependent_completion_signatures<E>>{};
            template<typename E>
            struct completion_signatures_SFINAE<E, std::void_t<completion_signatures_of_E<E>>> : public std::type_identity<typename completion_signatures_of_E<E>::completion_signatures>{};
            template<typename E>
            using completion_signatures_SFINAE_t = typename completion_signatures_SFINAE<E>::type;
		public:
            template<typename Self, typename E> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl> && ((sender<Ss, when_all_env_with_token<E>> && ...))
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<completion_signatures_SFINAE_t<E>>)->completion_signatures_SFINAE_t<E>{
				return completion_signatures_SFINAE_t<E>{};
            }

            template<typename Self, typename E> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl> && (!(sender<Ss, when_all_env_with_token<E>> && ...))
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<dependent_completion_signatures<E>>)->dependent_completion_signatures<E>{
				return dependent_completion_signatures<E>{};
            }

			template<typename Self, typename CPO> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl>
			friend auto tag_invoke(get_completion_scheduler_t<CPO> tag, Self&& s) = delete;
		};

		template<sender... Ss>
		using when_all_sender = sender_impl<std::remove_cvref_t<Ss>...>;
	}
	
	struct when_all_t{
		template<sender... Ss>
		[[nodiscard]] inline auto operator()(Ss&&...) const noexcept;

        template<sender S, sender... Ss> requires tag_invocable<when_all_t, S&&, Ss&&...> && sender<tag_invoke_result_t<when_all_t, S&&, Ss&&...>>
        [[nodiscard]] auto operator()(S&& s, Ss&&... ss) const noexcept(nothrow_tag_invocable<when_all_t, S&&, Ss&&...>)->tag_invoke_result_t<when_all_t, S&&, Ss&&...>{
            return tag_invoke(when_all_t{}, std::forward<S>(s), std::forward<Ss>(ss)...);
        }

        template<sender S, sender... Ss> requires (!tag_invocable<when_all_t, S&&, Ss&&...>)
        [[nodiscard]] auto operator()(S&& s, Ss&&... ss) const noexcept((std::is_nothrow_constructible_v<std::remove_cvref_t<S>, S&&> && ... && std::is_nothrow_constructible_v<std::remove_cvref_t<Ss>, Ss&&>)){
            return when_all_detail::when_all_sender<S, Ss...>{{ std::forward<S>(s), std::forward<Ss>(ss)... }};
        }
	};
	inline constexpr when_all_t when_all{};
	template<sender S, sender... Ss>
    using when_all_result_t = decltype(when_all(std::declval<S>(), std::declval<Ss>()...));
	template<typename S, typename... Ss>
	concept nothrow_when_all_invocable = (sender<S> && ... && sender<Ss>) && noexcept(when_all(std::declval<S>(), std::declval<Ss>()...));

	namespace when_all_detail{
		template<sender... Ss>
        struct when_all_closure_t : public sender_adaptor_closure<when_all_closure_t<Ss...>>{
			[[no_unique_address]] std::tuple<Ss...> data;

			[[nodiscard]] decltype(auto) get() noexcept{
				return data;
			}
			[[nodiscard]] auto get_tag() const noexcept{
				return when_all_t{};
			}
        };
	}
	template<sender... Ss>
	[[nodiscard]] inline auto when_all_t::operator()(Ss&&... ss) const noexcept{
		return when_all_detail::when_all_closure_t<std::remove_cvref_t<Ss>...>{ {}, { std::forward<Ss>(ss)... } };
	}
}