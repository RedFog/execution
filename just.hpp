#pragma once
#include "execution.hpp"

namespace execution{
    namespace just_detail{
        using ns_type_traits::member_type;
        using ns_type_traits::movable_value;
		template<movable_value... Values>
        struct just_sender : public completion_signatures<set_value_t(Values...), set_error_t(std::exception_ptr)>{
            [[no_unique_address]] std::tuple<Values...> values;

            template<receiver R>
            struct operation{
                [[no_unique_address]] std::tuple<Values...> values;
                [[no_unique_address]] R receiver;

                friend void tag_invoke(execution::start_t, operation& s) noexcept{
                    try {
                        std::apply([&s](Values&... values) noexcept(std::is_nothrow_invocable_v<set_value_t, R&&, Values&&...>){
                            execution::set_value(std::move(s.receiver), std::move(values)...);
                        }, s.values);
                    }
                    catch (...){
                        execution::set_error(std::move(s.receiver), std::current_exception());
                    }
                }
            };

            template<typename... Values2>
            just_sender(Values2&&... vs) noexcept(std::is_nothrow_constructible_v<std::tuple<Values...>, Values2&&...>) :values{ std::forward<Values2>(vs)... }{}

            template<typename Self, receiver R> requires std::is_same_v<std::remove_cvref_t<Self>, just_sender> && receiver_of<R&&, Values&&...> && std::constructible_from<std::tuple<Values...>, member_type<Self&&, std::tuple<Values...>>>
            [[nodiscard]] friend auto tag_invoke(execution::connect_t, Self&& s, R&& r) noexcept(std::is_nothrow_constructible_v<std::tuple<Values...>, member_type<Self&&, std::tuple<Values...>>>){
                return operation<std::remove_cvref_t<R>>{ std::forward<Self>(s).values, std::forward<R>(r) };
            }

            template<typename Self, typename CPO> requires std::is_same_v<std::remove_cvref_t<Self>, just_sender>
			friend auto tag_invoke(get_completion_scheduler_t<CPO> tag, Self&& s) = delete;

        };

        template<movable_value E>
        struct just_error_sender : public completion_signatures<set_error_t(E)>{
            [[no_unique_address]] E error;

            template<receiver R>
            struct operation{
                [[no_unique_address]] E error;
				[[no_unique_address]] R receiver;

                friend void tag_invoke(execution::start_t, operation& s) noexcept{
                    execution::set_error(std::move(s.receiver), std::move(s.error));
                }
            };

            template<typename E2>
            just_error_sender(E2&& e) noexcept(std::is_nothrow_constructible_v<E, E2&&>) :error(std::forward<E2>(e)){}

            template<typename Self, typename R> requires std::is_same_v<std::remove_cvref_t<Self>, just_error_sender> && std::constructible_from<E, member_type<Self&&, E>>
            [[nodiscard]] friend auto tag_invoke(execution::connect_t, Self&& s, R&& r) noexcept(std::is_nothrow_constructible_v<E, member_type<Self&&, E>>){
                return operation<std::remove_cvref_t<R>>{ std::forward<Self>(s).error, std::forward<R>(r) };
            }

            template<typename Self, typename CPO> requires std::is_same_v<std::remove_cvref_t<Self>, just_error_sender>
			friend auto tag_invoke(get_completion_scheduler_t<CPO> tag, Self&& s) = delete;
        };

        struct just_stopped_sender : public completion_signatures<set_stopped_t()>{
            template<receiver R>
            struct operation{
                [[no_unique_address]] R receiver;

                friend void tag_invoke(execution::start_t, operation& s) noexcept{
                    execution::set_stopped(std::move(s.receiver));
                }
            };

            template<typename Self, typename R> requires std::is_same_v<std::remove_cvref_t<Self>, just_stopped_sender>
            [[nodiscard]] friend auto tag_invoke(execution::connect_t, Self&& s, R&& r) noexcept{
                return operation<std::remove_cvref_t<R>>{ std::forward<R>(r) };
            }

            template<typename Self, typename CPO> requires std::is_same_v<std::remove_cvref_t<Self>, just_stopped_sender>
			friend auto tag_invoke(get_completion_scheduler_t<CPO> tag, Self&& s) = delete;
        };
    }

    template<ns_type_traits::movable_value... Values>
    using just_result_t = just_detail::just_sender<std::remove_cvref_t<Values>...>;
    struct just_t{
        template<ns_type_traits::movable_value... Values> requires tag_invocable<just_t, Values&&...> && sender<tag_invoke_result_t<just_t, Values&&...>>
        [[nodiscard]] auto operator()(Values&&... vs) const noexcept(nothrow_tag_invocable<just_t, Values&&...>)->tag_invoke_result_t<just_t, Values&&...>{
            return tag_invoke(just_t{}, std::forward<Values>(vs)...);
        }
        template<ns_type_traits::movable_value... Values> requires (!tag_invocable<just_t, Values&&...>)
        [[nodiscard]] auto operator()(Values&&... vs) const noexcept(std::is_nothrow_constructible_v<just_result_t<Values&&...>, Values&&...>)->just_result_t<Values&&...>{
            return just_result_t<Values&&...>{ std::forward<Values>(vs)... };
        }
    };
    inline constexpr just_t just;
    template<typename... Values>
	concept nothrow_just_invocable = (ns_type_traits::movable_value<Values> && ... && (noexcept(just(std::declval<Values>()...))));

    template<ns_type_traits::movable_value E>
    using just_error_result_t = just_detail::just_error_sender<std::remove_cvref_t<E>>;
    struct just_error_t{
        template<ns_type_traits::movable_value E> requires tag_invocable<just_error_t, E&&> && sender<tag_invoke_result_t<just_error_t, E&&>>
        [[nodiscard]] auto operator()(E&& e) const noexcept(nothrow_tag_invocable<just_error_t, E&&>)->tag_invoke_result_t<just_error_t, E&&>{
            return tag_invoke(just_error_t{}, std::forward<E>(e));
        }
        template<ns_type_traits::movable_value E> requires (!tag_invocable<just_error_t, E&&>)
        [[nodiscard]] auto operator()(E&& e) const noexcept(std::is_nothrow_constructible_v<just_error_result_t<E&&>, E&&>)->just_error_result_t<E&&>{
            return just_error_result_t<E&&>{ std::forward<E>(e) };
        }
    };
    inline constexpr just_error_t just_error;
    template<typename E>
    concept nothrow_just_error_invocable = ns_type_traits::movable_value<E> && noexcept(just_error(std::declval<E>()));

    using just_stopped_result_t = just_detail::just_stopped_sender;
    struct just_stopped_t{
        [[nodiscard]] auto operator()() const noexcept(std::is_nothrow_default_constructible_v<just_stopped_result_t>)->just_stopped_result_t{
            return just_stopped_result_t{};
        }
    };
    inline constexpr just_stopped_t just_stopped;
	inline constexpr bool nothrow_just_stopped_invocable = noexcept(just_stopped());
}