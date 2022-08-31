#pragma once
#include "execution.hpp"


#define EXECUTION_INTO_VARIANT_OPERATION_WRAPPER false

namespace execution{
	namespace into_variant_detail{
		using namespace ns_type_list;
		using ns_type_traits::member_type;

		template<typename S, typename E = no_env>
		using into_variant_type = value_types_of_t<S, E, decayed_tuple, variant_or_empty>;

		template<sender S>
		struct sender_impl{
			[[no_unique_address]] S pre_sender;
#if EXECUTION_INTO_VARIANT_OPERATION_WRAPPER
			// why I have to wrap it rather than just connect it like `then`?
			// requirement from: https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2022/p2300r4.html#spec-execution.senders.adapt.into_variant
            template<receiver R>
            struct operation{
                struct receiver_impl{
                    template<typename... Args>
					friend void tag_invoke(set_value_t, receiver_impl&& r, Args&&... args) noexcept{
						try {
							execution::set_value(std::move(r.op->out_receiver), into_variant_type<S, env_of_t<R>>(decayed_tuple<Args...>(std::forward<Args>(args)...)));
						} catch (...){
							execution::set_error(std::move(r.op->out_receiver), std::current_exception());
						}
					}
					template<ns_type_traits::any_of<set_error_t, set_stopped_t> Tag, typename... Args> requires tag_invocable<Tag, R&&, Args&&...>
					friend void tag_invoke(Tag tag, receiver_impl&& r, Args&&... args) noexcept(std::is_nothrow_invocable_v<Tag, R&&, Args&&...>){
						tag(std::move(r.op->out_receiver), std::forward<Args>(args)...);
					}

                    operation* op;
                };

                friend void tag_invoke(start_t, operation& op) noexcept{
                    return execution::start(op.op);
                }

                template<typename S2, typename R2>
                operation(S2&& s, R2&& r) noexcept(std::is_nothrow_constructible_v<R, R2&&> && nothrow_connect_invocable<S2&&, receiver_impl>) :out_receiver(std::forward<R2>(r)), op(execution::connect(std::forward<S2>(s), receiver_impl{ this })){}

				operation(operation const&) = delete;
				operation(operation&&) = delete;
				operation& operator=(operation const&) = delete;
				operation& operator=(operation&&) = delete;

				[[no_unique_address]] R out_receiver;
                [[no_unique_address]] connect_result_t<S, receiver_impl> op;
            };

			template<typename Self, receiver R> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl> && sender_to<member_type<Self&&, S>, typename operation<std::remove_cvref_t<R>>::receiver_impl&&>
            [[nodiscard]] friend auto tag_invoke(connect_t, Self&& self, R&& out_r) noexcept(std::is_nothrow_constructible_v<S, member_type<Self&&, S>> && std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&>){
				return operation<std::remove_cvref_t<R>>{ std::forward<Self>(self).pre_sender, std::move(out_r) };
			}
#else
			template<receiver R>
			struct receiver_impl{
				[[no_unique_address]] R out_receiver;

                template<typename... Args>
                friend void tag_invoke(set_value_t, receiver_impl&& r, Args&&... args) noexcept{
                    try {
                        execution::set_value(std::move(r.out_receiver), into_variant_type<S, env_of_t<R>>(decayed_tuple<Args...>(std::forward<Args>(args)...)));
                    } catch (...){
                        execution::set_error(std::move(r.out_receiver), std::current_exception());
                    }
                }
                template<ns_type_traits::any_of<set_error_t, set_stopped_t> Tag, typename... Args>
				friend void tag_invoke(Tag tag, receiver_impl&& r, Args&&... args) noexcept(std::is_nothrow_invocable_v<Tag, R&&, Args&&...>){
					tag(std::move(r.out_receiver), std::forward<Args>(args)...);
				}
			};

			template<typename Self, receiver R> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl> && sender_to<member_type<Self&&, S>, receiver_impl<std::remove_cvref_t<R>>&&>
			[[nodiscard]] friend auto tag_invoke(connect_t, Self&& self, R&& out_r) noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&> && nothrow_connect_invocable<member_type<Self&&, S>, receiver_impl<std::remove_cvref_t<R>>>){
				return execution::connect(std::forward<Self>(self).pre_sender, receiver_impl<std::remove_cvref_t<R>>{ std::move(out_r) });
			}
#endif
		private:
            template<typename E>
            struct completion_signatures_of_E{

                template<template<typename...>typename Tuple, template<typename...>typename Variant>
                using value_types = Variant<Tuple<into_variant_type<S, E>>>;

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
            template<typename Self, typename E> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl> && sender<S, E>
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<completion_signatures_SFINAE_t<E>>)->completion_signatures_SFINAE_t<E>{
				return completion_signatures_SFINAE_t<E>{};
            }

            template<typename Self, typename E> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl> && (!sender<S, E>)
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<dependent_completion_signatures<E>>)->dependent_completion_signatures<E>{
				return dependent_completion_signatures<E>{};
            }

			template<typename Self, typename CPO> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl> && std::invocable<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>
			[[nodiscard]] friend auto tag_invoke(get_completion_scheduler_t<CPO> tag, Self&& s) noexcept(std::is_nothrow_invocable_v<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>){
				return tag(std::forward<Self>(s).pre_sender);
			}
		};

		template<sender S>
		using into_variant_sender = sender_impl<std::remove_cvref_t<S>>;
	}
	using into_variant_detail::into_variant_type;

	struct into_variant_t{
		[[nodiscard]] inline auto operator()() const noexcept;

        template<sender S> requires tag_invocable<into_variant_t, S&&> && sender<tag_invoke_result_t<into_variant_t, S&&>>
        [[nodiscard]] auto operator()(S&& s) const noexcept(nothrow_tag_invocable<into_variant_t, S&&>)->tag_invoke_result_t<into_variant_t, S&&>{
            return tag_invoke(into_variant_t{}, std::forward<S>(s));
        }

        template<sender S> requires (!tag_invocable<into_variant_t, S&&>)
        [[nodiscard]] auto operator()(S&& s) const noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<S>, S&&>){
            return into_variant_detail::into_variant_sender<S>{ std::forward<S>(s) };
        }
	};
	inline constexpr into_variant_t into_variant{};
	template<sender S>
    using into_variant_result_t = decltype(into_variant(std::declval<S>()));
	template<typename S>
	concept nothrow_into_variant_invocable = sender<S> && noexcept(into_variant(std::declval<S>()));

	namespace into_variant_detail{
        struct into_variant_closure_t : public sender_adaptor_closure<into_variant_closure_t>{
			[[no_unique_address]] std::tuple<> data;

			[[nodiscard]] decltype(auto) get() noexcept{
				return data;
			}
			[[nodiscard]] auto get_tag() const noexcept{
				return into_variant_t{};
			}
        };
	}
	[[nodiscard]] inline auto into_variant_t::operator()() const noexcept{
		return into_variant_detail::into_variant_closure_t{};
	}
}