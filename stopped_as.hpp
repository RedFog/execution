#pragma once
#include "just.hpp"
#include "then.hpp"
#include "let.hpp"

#define EXECUTION_STOPPED_AS_OPTIONAL_EAGER_START false

namespace execution{
    namespace stopped_as_detail{
        struct get_env_sender{
            template<receiver R>
            struct operation{
                [[no_unique_address]] R receiver;

                friend void tag_invoke(start_t, operation& s) noexcept try {
					execution::set_value(std::move(s.receiver), execution::get_env(s.receiver));
				} catch (...){
					execution::set_error(std::move(s.receiver), std::current_exception());
				}
            };

            template<receiver R>
			[[nodiscard]] friend operation<std::remove_cvref_t<R>> tag_invoke(connect_t, get_env_sender, R&& r) noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&>){
#if EXECUTION_STOPPED_AS_OPTIONAL_EAGER_START
                // why `connect` have to `start` immediately? will it `start` twice?
                // requirement from: https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2022/p2300r4.html#spec-execution.senders.adapt.stopped_as_optional
				auto o = operation<std::remove_cvref_t<R>>{ std::forward<R>(r) };
                execution::start(o);
                return o; // maybe ill-formed if no NRVO
#else
				return operation<std::remove_cvref_t<R>>{ std::forward<R>(r) };
#endif
			}

            template<typename E>
            using completion_signatures = completion_signatures<set_value_t(E&&), set_error_t(std::exception_ptr)>;

			template<typename Env>
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, get_env_sender, Env) noexcept(std::is_nothrow_default_constructible_v<completion_signatures<Env>>)->completion_signatures<Env>{
                return {};
            };
        };
    }

	struct stopped_as_optional_t{
        [[nodiscard]] inline auto operator()() const noexcept;

        template<sender S> requires tag_invocable<stopped_as_optional_t, S&&> && sender<tag_invoke_result_t<stopped_as_optional_t, S&&>>
		[[nodiscard]] auto operator()(S&& s) const noexcept(nothrow_tag_invocable<stopped_as_optional_t, S&&>)->tag_invoke_result_t<stopped_as_optional_t, S&&>{
            return tag_invoke(stopped_as_optional_t{}, std::forward<S>(s));
        }
        template<sender S> requires (!tag_invocable<stopped_as_optional_t, S&&>)
		[[nodiscard]] auto operator()(S&& s) const noexcept{
            return stopped_as_detail::get_env_sender{}
                | execution::let_value([&s]<typename E>(E const& e) noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<S>, S&&>) requires single_sender<S, E>{
                    return std::forward<S>(s)
                        | execution::then([]<typename T>(T&& t) noexcept(std::is_nothrow_constructible_v<std::optional<std::remove_cvref_t<single_sender_value_type<S, E>>>, T&&>){
                            return std::optional<std::remove_cvref_t<single_sender_value_type<S, E>>>{ std::forward<T>(t) };
                        })
                        | execution::let_stopped([]() noexcept{
                            return execution::just(std::optional<std::remove_cvref_t<single_sender_value_type<S, E>>>{});
                        })
                    ;
                })
            ;
        }
    };
	inline constexpr stopped_as_optional_t stopped_as_optional;
	template<sender S>
    using stopped_as_optional_result_t = decltype(stopped_as_optional(std::declval<S>()));
    template<typename S>
    concept nothrow_stopped_as_optional_invocable = sender<S> && noexcept(stopped_as_optional(std::declval<S>()));

    struct stopped_as_error_t{
        template<ns_type_traits::movable_value E>
        [[nodiscard]] inline auto operator()(E&& e) const noexcept;

        template<sender S, ns_type_traits::movable_value E> requires tag_invocable<stopped_as_error_t, S&&, E&&> && sender<tag_invoke_result_t<stopped_as_error_t, S&&, E&&>>
		[[nodiscard]] auto operator()(S&& s, E&& e) const noexcept(nothrow_tag_invocable<stopped_as_error_t, S&&, E&&>)->tag_invoke_result_t<stopped_as_error_t, S&&, E&&>{
            return tag_invoke(stopped_as_error_t{}, std::forward<S>(s), std::forward<E>(e));
        }
        template<sender S, ns_type_traits::movable_value E> requires (!tag_invocable<stopped_as_error_t, S&&, E&&>)
		[[nodiscard]] auto operator()(S&& s, E&& e) const noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<S>, S&&>){
            using RefE = std::conditional_t<std::is_lvalue_reference_v<E>, std::reference_wrapper<std::remove_reference_t<E>>, E>;
            return std::forward<S>(s)
                | execution::let_stopped([e = RefE{ std::forward<E>(e) }]() mutable noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<E>, E&&>){
                    return execution::just_error(std::forward<E>(e));
                })
            ;
        }
    };
	inline constexpr stopped_as_error_t stopped_as_error;
	template<sender S>
    using stopped_as_error_result_t = decltype(stopped_as_error(std::declval<S>()));
    template<typename S>
    concept nothrow_stopped_as_error_invocable = sender<S> && noexcept(stopped_as_error(std::declval<S>()));

	namespace stopped_as_detail{
        struct stopped_as_optional_closure_t : public sender_adaptor_closure<stopped_as_optional_closure_t>{
			[[no_unique_address]] std::tuple<> data;

			[[nodiscard]] decltype(auto) get() noexcept{
				return data;
			}
			[[nodiscard]] auto get_tag() const noexcept{
				return stopped_as_optional_t{};
			}
        };
        template<ns_type_traits::movable_value E>
        struct stopped_as_error_closure_t : public sender_adaptor_closure<stopped_as_error_closure_t<E>>{
			[[no_unique_address]] std::tuple<E> data;

			[[nodiscard]] decltype(auto) get() noexcept{
				return data;
			}
			[[nodiscard]] auto get_tag() const noexcept{
				return stopped_as_error_t{};
			}
        };
	}
	[[nodiscard]] inline auto stopped_as_optional_t::operator()() const noexcept{
		return stopped_as_detail::stopped_as_optional_closure_t{};
	}
    template<ns_type_traits::movable_value E>
	[[nodiscard]] inline auto stopped_as_error_t::operator()(E&& e) const noexcept{
		return stopped_as_detail::stopped_as_error_closure_t<std::remove_cvref_t<E>>{ {}, std::forward<E>(e) };
	}
}