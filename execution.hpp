#pragma once
#include <type_traits>
#include <coroutine>
#include <variant>
#include <optional>
#include <tuple>
#include "type_list.hpp"
#include "stop_token.hpp"


namespace execution{

    // tag_invoke

    namespace tag_invoke_detail{
        void tag_invoke();
        struct tag_invoke_t{
            template<typename Tag, typename... Args>
            constexpr auto operator()(Tag tag, Args&&... args) const noexcept(noexcept(tag_invoke(std::forward<Tag>(tag), std::forward<Args>(args)...)))->decltype(tag_invoke(std::forward<Tag>(tag), std::forward<Args>(args)...)){
                return tag_invoke(std::forward<Tag>(tag), std::forward<Args>(args)...);
            }
        };
    }
    inline constexpr tag_invoke_detail::tag_invoke_t tag_invoke{};

    template<auto& Tag>
    using tag_t = std::decay_t<decltype(Tag)>;

    template<typename Tag, typename... Args>
    concept tag_invocable = std::invocable<decltype(tag_invoke), Tag, Args...>;

    template<typename Tag, typename... Args>
    concept nothrow_tag_invocable = std::is_nothrow_invocable_v<decltype(tag_invoke), Tag, Args...>;

    template<typename Tag, typename... Args>
	using tag_invoke_result_t = std::invoke_result_t<decltype(tag_invoke), Tag, Args...>;

    // no_env, get_env, forwarding_env_query

    namespace exec_envs{
		struct no_env{
			friend void tag_invoke(auto, std::same_as<no_env> auto, auto&&...) = delete;
		};
        struct empty_env{}; // ???
        struct get_env_t{
		    template<typename R> requires tag_invocable<get_env_t, R&&>
		    [[nodiscard]] auto operator()(R&& r) const noexcept(nothrow_tag_invocable<get_env_t, R&&>)->tag_invoke_result_t<get_env_t, R&&>{
			    return tag_invoke(get_env_t{}, std::forward<R>(r));
		    }
            template<typename R>
		    [[nodiscard]] auto operator()(R&& r) const noexcept{
			    return empty_env{};
		    }
	    };
		struct forwarding_env_query_t{
		    template<typename T> requires tag_invocable<forwarding_env_query_t, T&&>
		    [[nodiscard]] constexpr bool operator()(T&& t) const noexcept(nothrow_tag_invocable<forwarding_env_query_t, T&&>){
				return tag_invoke(forwarding_env_query_t{}, std::forward<T>(t));
		    }
            template<typename T>
		    [[nodiscard]] constexpr bool operator()(T&& t) const noexcept{
			    return true;
		    }
	    };
    }
    using exec_envs::no_env;
    using exec_envs::empty_env;
    using exec_envs::get_env_t;
    using exec_envs::forwarding_env_query_t;
	inline constexpr get_env_t get_env{};
	inline constexpr forwarding_env_query_t forwarding_env_query{};
    template<typename T>
    using env_of_t = decltype(get_env(std::declval<T>()));
    template<typename T>
    concept nothrow_get_env_invocable = noexcept(get_env(std::declval<T>()));

    // set_value, set_error, set_stopped

    struct set_stopped_t{
        template<typename R> requires tag_invocable<set_stopped_t, R&&>
        auto operator()(R&& r) const noexcept(nothrow_tag_invocable<set_stopped_t, R&&>)->tag_invoke_result_t<set_stopped_t, R&&>{
            return tag_invoke(set_stopped_t{}, std::forward<R>(r));
        }
    };
    inline constexpr set_stopped_t set_stopped{};
    template<typename R>
    using set_stopped_result_t = tag_invoke_result_t<set_stopped_t, R>;
    template<typename R>
    concept nothrow_set_stopped_invocable = nothrow_tag_invocable<set_stopped_t, R>;

    struct set_error_t{
        template<typename R, typename E> requires tag_invocable<set_error_t, R&&, E&&>
        auto operator()(R&& r, E&& e) const noexcept(nothrow_tag_invocable<set_error_t, R&&, E&&>)->tag_invoke_result_t<set_error_t, R&&, E&&>{
            return tag_invoke(set_error_t{}, std::forward<R>(r), std::forward<E>(e));
        }
    };
    inline constexpr set_error_t set_error{};
    template<typename R, typename E>
    using set_error_result_t = tag_invoke_result_t<set_error_t, R, E>;
    template<typename R, typename E>
    concept nothrow_set_error_invocable = nothrow_tag_invocable<set_error_t, R, E>;

    struct set_value_t{
        template<typename R, typename... Vs> requires tag_invocable<set_value_t, R&&, Vs&&...>
        auto operator()(R&& r, Vs&&... vs) const noexcept(nothrow_tag_invocable<set_value_t, R&&, Vs&&...>)->tag_invoke_result_t<set_value_t, R&&, Vs&&...>{
            return tag_invoke(set_value_t{}, std::forward<R>(r), std::forward<Vs>(vs)...);
        }
    };
    inline constexpr set_value_t set_value{};
    template<typename R, typename... Vs>
	using set_value_result_t = tag_invoke_result_t<set_value_t, R, Vs...>;
    template<typename R, typename... Vs>
    concept nothrow_set_value_invocable = nothrow_tag_invocable<set_value_t, R, Vs...>;

	// no_completion_signatures, completion_signatures

    struct no_completion_signatures{};

    namespace completion_signatures_of_detail{
        struct empty_promise{};
        template<typename S>
        concept is_exactly_awaitable_general = requires(S&& s){ 
            std::forward<S>(s).await_ready();
			std::forward<S>(s).await_suspend(std::coroutine_handle<>());
			std::forward<S>(s).await_resume();
        };
        template<typename S, typename P>
        concept is_exactly_awaitable_specific = requires(S&& s){ 
            std::forward<S>(s).await_ready();
			std::forward<S>(s).await_suspend(std::coroutine_handle<std::remove_cvref_t<P>>());
			std::forward<S>(s).await_resume();
        };
        template<typename S, typename P>
		concept is_exactly_awaitable = is_exactly_awaitable_general<S> || is_exactly_awaitable_specific<S, P>;

        template<typename S>
        concept maybe_convert_to_awaitable_member = requires(S&& s){ std::forward<S>(s).operator co_await(); };
        template<typename S>
        concept maybe_convert_to_awaitable_nonmember = requires(S&& s) { operator co_await(std::forward<S>(s)); };

        template<typename S, typename P, typename = void>
        struct try_await_transform_impl : public std::type_identity<S>{};
        template<typename S, typename P>
        struct try_await_transform_impl<S, P, std::enable_if_t<requires(S&& s, P& p){ p.await_transform(s); }>> : public std::type_identity<decltype(std::declval<P&>().await_transform(std::declval<S&&>()))>{};
        template<typename S, typename P>
        using try_await_transform = typename try_await_transform_impl<S, std::remove_cvref_t<P>>::type;

        template<typename S, typename P, typename = void>
        struct is_awaitable_impl : public std::false_type{};
        template<typename S, typename P>
        struct is_awaitable_impl<S, P, std::enable_if_t<maybe_convert_to_awaitable_member<S> && !maybe_convert_to_awaitable_nonmember<S>>> : public is_awaitable_impl<decltype(std::declval<S>().operator co_await()), P>{};
        template<typename S, typename P>
        struct is_awaitable_impl<S, P, std::enable_if_t<!maybe_convert_to_awaitable_member<S> && maybe_convert_to_awaitable_nonmember<S>>> : public is_awaitable_impl<decltype(operator co_await(std::declval<S>())), P>{};
        template<typename S, typename P>
        struct is_awaitable_impl<S, P, std::enable_if_t<maybe_convert_to_awaitable_member<S> && maybe_convert_to_awaitable_nonmember<S>>> : public std::false_type{};
        template<typename S, typename P>
        struct is_awaitable_impl<S, P, std::enable_if_t<!maybe_convert_to_awaitable_member<S> && !maybe_convert_to_awaitable_nonmember<S> && is_exactly_awaitable<S, P>>> : public std::true_type{};

        template<typename S, typename P = empty_promise>
		concept is_awaitable = is_awaitable_impl<try_await_transform<S, P>, std::remove_cvref_t<P>>::value;

        template<typename S, typename P, typename = void>
        struct await_result_type_impl;
        template<typename S, typename P>
        struct await_result_type_impl<S, P, std::enable_if_t<maybe_convert_to_awaitable_member<S> && !maybe_convert_to_awaitable_nonmember<S>>> : public await_result_type_impl<decltype(std::declval<S>().operator co_await()), P>{};
        template<typename S, typename P>
        struct await_result_type_impl<S, P, std::enable_if_t<!maybe_convert_to_awaitable_member<S> && maybe_convert_to_awaitable_nonmember<S>>> : public await_result_type_impl<decltype(operator co_await(std::declval<S>())), P>{};
        template<typename S, typename P>
        struct await_result_type_impl<S, P, std::enable_if_t<maybe_convert_to_awaitable_member<S> && maybe_convert_to_awaitable_nonmember<S>>>;
        template<typename S, typename P>
        struct await_result_type_impl<S, P, std::enable_if_t<!maybe_convert_to_awaitable_member<S> && !maybe_convert_to_awaitable_nonmember<S> && is_exactly_awaitable<S, P>>> : public std::type_identity<decltype(std::declval<S>().await_resume())>{};

        template<typename S, typename P = empty_promise>
        using await_result_type = typename await_result_type_impl<try_await_transform<S, P>, std::remove_cvref_t<P>>::type;

        template<typename S>
		concept has_completion_signatures = requires{
			typename std::remove_cvref_t<S>::completion_signatures;
        };

        using namespace ns_type_list;
        template<typename F>
		struct completion_signature_check : public std::false_type{};
        template<typename... Vs>
		struct completion_signature_check<set_value_t(Vs...)> : public std::true_type{};
        template<typename E>
		struct completion_signature_check<set_error_t(E)> : public std::true_type{};
        template<>
		struct completion_signature_check<set_stopped_t()> : public std::true_type{};
        template<typename F>
		concept completion_signature = completion_signature_check<F>::value;

        template<typename F>
        struct is_set_value : public std::false_type{};
        template<typename... Vs>
        struct is_set_value<set_value_t(Vs...)> : public std::true_type{};
        template<typename F>
        struct is_set_error : public std::false_type{};
        template<typename E>
        struct is_set_error<set_error_t(E)> : public std::true_type{};

        template<typename F>
        struct to_type_list;
        template<typename R, typename... Vs>
        struct to_type_list<R(Vs...)> : public std::type_identity<type_list<Vs...>>{};
        template<typename F>
        using to_type_list_t = typename to_type_list<F>::type;
        template<typename F>
        struct to_type;
        template<typename R, typename E>
        struct to_type<R(E)> : public std::type_identity<E>{};
        template<typename F>
        using to_type_t = typename to_type<F>::type;

        template<completion_signature... Fs>
		using value_types_from_Fs = type_list_map_t<type_list_select_t<type_list<Fs...>, is_set_value>, to_type_list>;
        template<completion_signature... Fs>
		using error_types_from_Fs = type_list_map_t<type_list_select_t<type_list<Fs...>, is_set_error>, to_type>;
    }

    template<completion_signatures_of_detail::completion_signature... Fs>
	struct completion_signatures{
	private:
		using value_types_from_Fs = completion_signatures_of_detail::value_types_from_Fs<Fs...>;
		using error_types_from_Fs = completion_signatures_of_detail::error_types_from_Fs<Fs...>;
	public:
        template<template<typename...>typename Tuple, template<typename...>typename Variant>
        using value_types = ns_type_list::type_list_deep_apply_t<value_types_from_Fs, Variant, Tuple>;
        
        template <template<typename...>typename Variant>
        using error_types = ns_type_list::type_list_apply_t<error_types_from_Fs, Variant>;

        static constexpr bool sends_stopped = ns_type_traits::any_of<set_stopped_t(), Fs...>;
    };

    // get_completion_signatures

    struct get_completion_signatures_t{
        template<typename S>
        [[nodiscard]] auto operator()(S&& s) const noexcept(noexcept((*this)(std::forward<S>(s), no_env{}))){
			return (*this)(std::forward<S>(s), no_env{});
        }
        template<typename S, typename E> requires tag_invocable<get_completion_signatures_t, S, E>
        [[nodiscard]] auto operator()(S&& s, E&& e) const noexcept(nothrow_tag_invocable<get_completion_signatures_t, S, E>)->tag_invoke_result_t<get_completion_signatures_t, S, E>{
			return tag_invoke(get_completion_signatures_t{}, std::forward<S>(s), std::forward<E>(e));
        }
        template<typename S, typename E> requires (!tag_invocable<get_completion_signatures_t, S, E>) && completion_signatures_of_detail::has_completion_signatures<S>
        [[nodiscard]] auto operator()(S&& s, E&& e) const noexcept(std::is_nothrow_default_constructible_v<typename std::remove_cvref_t<S>::completion_signatures>){
			return typename std::remove_cvref_t<S>::completion_signatures{};
        }
        template<typename S, typename E> requires (!tag_invocable<get_completion_signatures_t, S, E>) && (!completion_signatures_of_detail::has_completion_signatures<S>) && completion_signatures_of_detail::is_awaitable<S> && std::is_void_v<std::remove_cvref_t<completion_signatures_of_detail::await_result_type<S>>>
        [[nodiscard]] auto operator()(S&& s, E&& e) const noexcept(std::is_nothrow_default_constructible_v<completion_signatures<set_value_t(), set_error_t(std::exception_ptr), set_stopped_t()>>){
			return completion_signatures<set_value_t(), set_error_t(std::exception_ptr), set_stopped_t()>{};
        }
        template<typename S, typename E> requires (!tag_invocable<get_completion_signatures_t, S, E>) && (!completion_signatures_of_detail::has_completion_signatures<S>) && completion_signatures_of_detail::is_awaitable<S> && (!std::is_void_v<std::remove_cvref_t<completion_signatures_of_detail::await_result_type<S>>>)
        [[nodiscard]] auto operator()(S&& s, E&& e) const noexcept(std::is_nothrow_default_constructible_v<completion_signatures<set_value_t(completion_signatures_of_detail::await_result_type<S>), set_error_t(std::exception_ptr), set_stopped_t()>>){
			return completion_signatures<set_value_t(completion_signatures_of_detail::await_result_type<S>), set_error_t(std::exception_ptr), set_stopped_t()>{};
        }
        template<typename S, typename E> requires (!tag_invocable<get_completion_signatures_t, S, E>) && (!completion_signatures_of_detail::has_completion_signatures<S>) && (!completion_signatures_of_detail::is_awaitable<S>)
        [[nodiscard]] auto operator()(S&& s, E&& e) const noexcept{
			return no_completion_signatures{};
        }
    };
	inline constexpr get_completion_signatures_t get_completion_signatures{};

    // completion_signatures_of_t

    namespace completion_signatures_of_detail{
        template<typename S, typename E>
        using completion_signatures_of = decltype(execution::get_completion_signatures(std::declval<S>(), std::declval<E>()));
    }

    template<typename S, typename E> requires (!std::same_as<completion_signatures_of_detail::completion_signatures_of<S, E>, no_completion_signatures>)
    using completion_signatures_of_t = completion_signatures_of_detail::completion_signatures_of<S, E>;

	// sender

    namespace sender_detail{
		template<template<template<typename...>typename, template<typename...>typename>typename>
		struct check_value_types;
		template<template<template<typename...>typename>typename>
		struct check_error_types;

        template<typename S>
		concept has_sender_type = requires{
			typename check_value_types<S::template value_types>;
			typename check_error_types<S::template error_types>;
			typename std::bool_constant<S::sends_stopped>;
		};

        template<typename S, typename E>
        concept sender_base = requires{ typename completion_signatures_of_t<S, E>; } && has_sender_type<completion_signatures_of_t<S, E>>;
    }

	template<typename S, typename E = no_env>
	concept sender = sender_detail::sender_base<S, E> && sender_detail::sender_base<S, no_env> && std::move_constructible<std::remove_cvref_t<S>>;

    template<typename S, typename E = no_env, typename... Ts>
	concept sender_of = sender<S, E> && std::same_as<ns_type_list::type_list<Ts...>, typename completion_signatures_of_t<S, E>::template value_types<ns_type_list::type_list, std::type_identity_t>>;

    // value_types_of_t, error_types_of_t

    namespace completion_signatures_of_detail{
        template<typename... Ts>
        using decayed_tuple = std::tuple<std::remove_cvref_t<Ts>...>;

        struct empty_variant{
			empty_variant() = delete;
        };
        template<typename... Ts>
        using variant_or_empty = std::conditional_t<(sizeof...(Ts) > 0), std::variant<std::remove_cvref_t<Ts>...>, empty_variant>;
    }
    using completion_signatures_of_detail::empty_variant;
    using completion_signatures_of_detail::decayed_tuple;
    using completion_signatures_of_detail::variant_or_empty;

    template<sender S, typename E = no_env, template<typename...>typename Tuple = completion_signatures_of_detail::decayed_tuple, template<typename...>typename Variant = completion_signatures_of_detail::variant_or_empty> requires sender<S, E>
    using value_types_of_t = typename completion_signatures_of_t<S, E>::template value_types<Tuple, Variant>;

    template<sender S, typename E = no_env, template<typename...>typename Variant = completion_signatures_of_detail::variant_or_empty> requires sender<S, E>
    using error_types_of_t = typename completion_signatures_of_t<S, E>::template error_types<Variant>;

    // dependent_completion_signatures, make_completion_signatures

    namespace make_completion_signatures_detail{
        using namespace ns_type_list;
        using completion_signatures_of_detail::is_set_value;
        using completion_signatures_of_detail::is_set_error;

        template<typename... Ts>
        using default_set_value = auto(Ts...)->set_value_t;
        template<typename E>
        using default_set_error = auto(E)->set_error_t;

        template<typename TL, typename = void>
        struct is_non_void_type_list : public std::true_type{};
        template<typename... Ts>
        struct is_non_void_type_list<type_list<Ts...>, std::enable_if_t<ns_type_traits::any_of<void, Ts...>>> : public std::false_type{};
        template<typename TL>
		inline constexpr bool is_non_void_type_list_v = is_non_void_type_list<TL>::value;

        template<typename TL, typename = void>
        struct is_set_value_type_list : public std::false_type{};
        template<typename... Ts>
        struct is_set_value_type_list<type_list<Ts...>, std::enable_if_t<type_list_all_of_v<type_list<Ts...>, is_set_value>>> : public std::true_type{};
        template<typename TL>
		inline constexpr bool is_set_value_type_list_v = is_set_value_type_list<TL>::value;

        template<typename TL, typename = void>
        struct is_set_error_type_list : public std::false_type{};
        template<typename... Ts>
        struct is_set_error_type_list<type_list<Ts...>, std::enable_if_t<type_list_all_of_v<type_list<Ts...>, is_set_error>>> : public std::true_type{};
        template<typename TL>
		inline constexpr bool is_set_error_type_list_v = is_set_error_type_list<TL>::value;

        template<template<typename>typename SetError>
        struct error_list{
            template<typename... Ts>
            using apply = type_list<SetError<Ts>...>;
        };

        template<template<template<typename...>typename, template<typename...>typename>typename ValueTypes, template<template<typename...>typename>typename ErrorTypes, bool SendsStopped, template<typename...>typename SetValue = default_set_value, template<typename>typename SetError = default_set_error>
        struct completion_signatures_to_list{
            using Vs = ValueTypes<SetValue, type_list>;
            using Es = ErrorTypes<error_list<SetError>::template apply>;
            using Ss = std::conditional_t<SendsStopped, type_list<set_stopped_t()>, type_list<>>;

            using type = type_list_apply_t<type_list_concat_t<Vs, Es, Ss>, completion_signatures>;
        };

        template<sender S, typename Env, typename AddlSigs, template<typename...>typename SetValue, template<typename>typename SetError, bool SendsStopped>
		struct make_completion_signatures{
            using Vs = value_types_of_t<S, Env, SetValue, type_list>;
            using Es = error_types_of_t<S, Env, error_list<SetError>::template apply>;
            using Ss = std::conditional_t<SendsStopped, type_list<set_stopped_t()>, type_list<>>;
            using MoreSigs = to_type_list_t<AddlSigs>;

            using type = type_list_apply_t<include_or_merge_t<type_list_concat_t<Vs, Es, Ss>, MoreSigs>, completion_signatures>;
        };
    }

    using make_completion_signatures_detail::default_set_value;
    using make_completion_signatures_detail::default_set_error;
    using make_completion_signatures_detail::completion_signatures_to_list;
    template<template<template<typename...>typename, template<typename...>typename>typename ValueTypes, template<template<typename...>typename>typename ErrorTypes, bool SendsStopped, template<typename...>typename SetValue = default_set_value, template<typename>typename SetError = default_set_error>
    using completion_signatures_to_list_t = typename completion_signatures_to_list<ValueTypes, ErrorTypes, SendsStopped, SetValue, SetError>::type;

    template<typename E>
    struct dependent_completion_signatures{};
    template<>
	struct dependent_completion_signatures<no_env>{
        template<template<typename...>typename, template<typename...>typename> requires false
        using value_types = void;
        template<template<typename...>typename> requires false
        using error_types = void;

        static constexpr bool sends_stopped = false;
    };

    template<
        sender S, typename Env = no_env, typename AddlSigs = completion_signatures<>, 
        template<typename...>typename SetValue = make_completion_signatures_detail::default_set_value, template<typename>typename SetError = make_completion_signatures_detail::default_set_error, 
        bool SendsStopped = completion_signatures_of_t<S, Env>::sends_stopped
    > requires sender<S, Env>
    using make_completion_signatures = typename std::conditional_t<
        requires{
		    typename value_types_of_t<S, Env, SetValue, ns_type_list::type_list>;
            typename error_types_of_t<S, Env, make_completion_signatures_detail::error_list<SetError>::template apply>;
        } &&
		ns_type_traits::is_instantiation_of_v<AddlSigs, completion_signatures> &&
		make_completion_signatures_detail::is_set_value_type_list_v<value_types_of_t<S, Env, SetValue, ns_type_list::type_list>> &&
        make_completion_signatures_detail::is_set_error_type_list_v<error_types_of_t<S, Env, make_completion_signatures_detail::error_list<SetError>::template apply>> &&
		make_completion_signatures_detail::is_non_void_type_list_v<make_completion_signatures_detail::to_type_list_t<AddlSigs>>,
        make_completion_signatures_detail::make_completion_signatures<S, Env, AddlSigs, SetValue, SetError, SendsStopped>, 
        std::type_identity<dependent_completion_signatures<Env>>
    >::type;

    // scheduler

	template<ns_type_traits::any_of<set_value_t, set_error_t, set_stopped_t> CPO>
	struct get_completion_scheduler_t;
	struct schedule_t{
		template<typename Sch> requires nothrow_tag_invocable<schedule_t, Sch> && sender<tag_invoke_result_t<schedule_t, Sch>>
		[[nodiscard]] auto operator()(Sch&& sch) const noexcept->tag_invoke_result_t<schedule_t, Sch>{
			return tag_invoke(schedule_t{}, std::forward<Sch>(sch));
		}
	};
	inline constexpr schedule_t schedule{};
	template<typename Sch>
	using schedule_result_t = tag_invoke_result_t<schedule_t, Sch>;
    template<typename Sch>
    concept nothrow_schedule_invocable = tag_invocable<schedule_t, Sch>;

	template<typename Sch>
	concept scheduler = std::copy_constructible<std::remove_cvref_t<Sch>> && std::equality_comparable<std::remove_cvref_t<Sch>> && requires(Sch&& sch, get_completion_scheduler_t<set_value_t> const tag){
		{ execution::schedule(std::forward<Sch>(sch)) } noexcept->sender;
		{ tag_invoke(tag, execution::schedule(std::forward<Sch>(sch))) }->std::same_as<std::remove_cvref_t<Sch>>;
	};

    // get_completion_scheduler, completion_scheduler_for, tag_invocable_with_completion_scheduler

    template<ns_type_traits::any_of<set_value_t, set_error_t, set_stopped_t> CPO>
    struct get_completion_scheduler_t{
		template<sender S> requires tag_invocable<get_completion_scheduler_t, S> && scheduler<tag_invoke_result_t<get_completion_scheduler_t, S>>
		[[nodiscard]] auto operator()(S&& s) const noexcept(nothrow_tag_invocable<get_completion_scheduler_t, S>)->tag_invoke_result_t<get_completion_scheduler_t, S>{
			return tag_invoke(get_completion_scheduler_t{}, std::forward<S>(s));
        }
    };
	template<ns_type_traits::any_of<set_value_t, set_error_t, set_stopped_t> CPO>
	inline constexpr get_completion_scheduler_t<CPO> get_completion_scheduler;
	template<ns_type_traits::any_of<set_value_t, set_error_t, set_stopped_t> CPO, sender S>
    using get_completion_scheduler_result_t = tag_invoke_result_t<get_completion_scheduler_t<CPO>, S>;
    template<typename CPO, typename S>
	concept nothrow_get_completion_scheduler_invocable = ns_type_traits::any_of<CPO, set_value_t, set_error_t, set_stopped_t> && sender<S> && noexcept(get_completion_scheduler<CPO>(std::declval<S>()));

    template<sender S, ns_type_traits::any_of<set_value_t, set_error_t, set_stopped_t> Tag>
    using completion_scheduler_for = get_completion_scheduler_result_t<Tag, S>;

    template<typename Tag1, typename Tag2, typename S, typename... Args>
	concept tag_invocable_with_completion_scheduler = ns_type_traits::any_of<Tag2, set_value_t, set_error_t, set_stopped_t> && sender<S> && tag_invocable<get_completion_scheduler_t<Tag2>, S> && tag_invocable<Tag1, completion_scheduler_for<S, Tag2>, S, Args...>;
    template<typename Tag1, typename Tag2, typename S, typename... Args>
	concept nothrow_tag_invocable_with_completion_scheduler = tag_invocable_with_completion_scheduler<Tag1, Tag2, S, Args...> && nothrow_tag_invocable<Tag1, completion_scheduler_for<S, Tag2>, S, Args...>;
    template<typename Tag, ns_type_traits::any_of<set_value_t, set_error_t, set_stopped_t> CPO, sender S, typename... Args>
    using tag_invoke_with_completion_scheduler_result_t = tag_invoke_result_t<Tag, completion_scheduler_for<S, CPO>, S, Args...>;

    // forwarding_scheduler_query, forwarding_receiver_query, forwarding_sender_query_t

    struct forwarding_scheduler_query_t{
		template<typename T> requires tag_invocable<forwarding_scheduler_query_t, T&&>
		[[nodiscard]] constexpr bool operator()(T&& t) const noexcept(nothrow_tag_invocable<forwarding_scheduler_query_t, T&&>){
			return tag_invoke(forwarding_scheduler_query_t{}, std::forward<T>(t));
		}
		template<typename T>
        [[nodiscard]] constexpr bool operator()(T&& t) const noexcept{
			return false;
		}
	};
	inline constexpr forwarding_scheduler_query_t forwarding_scheduler_query{};

    struct forwarding_receiver_query_t{
		template<typename T> requires tag_invocable<forwarding_receiver_query_t, T&&>
		[[nodiscard]] constexpr bool operator()(T&& t) const noexcept(nothrow_tag_invocable<forwarding_receiver_query_t, T&&>){
			return tag_invoke(forwarding_receiver_query_t{}, std::forward<T>(t));
		}
		template<typename T>
        [[nodiscard]] constexpr bool operator()(T&& t) const noexcept{
			return !ns_type_traits::any_of<T, set_value_t, set_error_t, set_stopped_t>;
		}
	};
	inline constexpr forwarding_receiver_query_t forwarding_receiver_query{};

    struct forwarding_sender_query_t{
		template<typename T> requires tag_invocable<forwarding_sender_query_t, T&&>
		[[nodiscard]] constexpr bool operator()(T&& t) const noexcept(nothrow_tag_invocable<forwarding_sender_query_t, T&&>){
			return tag_invoke(forwarding_sender_query_t{}, std::forward<T>(t));
		}
		template<typename T>
        [[nodiscard]] constexpr bool operator()(T&& t) const noexcept{
			return false;
		}
	};
	inline constexpr forwarding_sender_query_t forwarding_sender_query{};

	// get_forward_progress_guarantee

    enum class forward_progress_guarantee{
		concurrent,
		parallel,
		weakly_parallel
	};

    struct get_forward_progress_guarantee_t{
		template<scheduler Sch> requires nothrow_tag_invocable<get_forward_progress_guarantee_t, Sch&&> && std::is_same_v<forward_progress_guarantee, tag_invoke_result_t<get_forward_progress_guarantee_t, Sch&&>>
		[[nodiscard]] forward_progress_guarantee operator()(Sch&& sch) const noexcept{
			return tag_invoke(get_forward_progress_guarantee_t{}, std::as_const(sch));
		}
		template<scheduler Sch>
        [[nodiscard]] forward_progress_guarantee operator()(Sch&& sch) const noexcept{
			return forward_progress_guarantee::weakly_parallel;
		}
	};
	inline constexpr get_forward_progress_guarantee_t get_forward_progress_guarantee{};

    // receiver

    template<typename T, typename E = std::exception_ptr>
	concept receiver = std::move_constructible<std::remove_cvref_t<T>> && std::constructible_from<std::remove_cvref_t<T>, T> && requires(std::remove_cvref_t<T>&& t, E&& e){
        { execution::set_stopped(std::move(t)) } noexcept;
        { execution::set_error(std::move(t), std::forward<E>(e)) } noexcept;
    };

    template<typename T, typename... An>
    concept receiver_of = receiver<T> && requires(std::remove_cvref_t<T>&& t, An&&... an){
        execution::set_value(std::move(t), std::forward<An>(an)...);
    };

    // start

    struct start_t{
        template<typename O> requires nothrow_tag_invocable<start_t, O&>
        auto operator()(O& op) const noexcept->tag_invoke_result_t<start_t, O&>{
            return tag_invoke(start_t{}, op);
        }
    };
    inline constexpr start_t start{};
    template<typename O>
    using start_result_t = tag_invoke_result_t<start_t, O>;
    template<typename O>
    concept nothrow_start_invocable = tag_invocable<start_t, O>;

    // operation_state

    template<typename O>
	concept operation_state = std::destructible<O> && std::is_object_v<O> && requires(O& o){
		{ execution::start(o) } noexcept;
	};

    // connect

    namespace connect_detail{
        using completion_signatures_of_detail::is_awaitable;
        using completion_signatures_of_detail::await_result_type;

        template<typename S, typename R>
        struct operation_state_task{
            struct promise_type{
                promise_type(S const&, R r) noexcept(std::is_nothrow_move_constructible_v<R>) :out_receiver(std::move(r)){}
                operation_state_task get_return_object() noexcept{
					return { std::coroutine_handle<promise_type>::from_promise(*this) };
                }
                std::suspend_always initial_suspend() noexcept{ return {}; }
                std::suspend_never final_suspend() noexcept{ return {}; }
                void return_void() noexcept{}
                [[noreturn]] void unhandled_exception() noexcept{ // never called
                    std::terminate();
                }
                auto unhandled_stopped(){
                    return (execution::set_stopped(std::forward<R>(out_receiver)), std::noop_coroutine());
                }

                template<typename Tag, typename... Args> requires (execution::forwarding_receiver_query(Tag{}))
                friend decltype(auto) tag_invoke(Tag tag, promise_type& p, Args&&... args) noexcept(std::is_nothrow_invocable_v<Tag, R const&, Args&&...>){
					return tag(std::as_const(p.out_receiver), std::forward<Args>(args)...);
                }
                template<typename E>
                [[nodiscard]] decltype(auto) await_transform(E&& e) noexcept;

                [[no_unique_address]] R out_receiver;
            };

            friend void tag_invoke(start_t, operation_state_task& o) noexcept{
                o.handle.resume();
            }

            std::coroutine_handle<promise_type> handle;
        };

        template<typename S, typename R>
        using connect_awaitable_promise = typename std::coroutine_traits<operation_state_task<std::remove_cvref_t<S>, std::remove_cvref_t<R>>, S, R>::promise_type;

        template<typename S, receiver R>// requires (std::is_void_v<await_result_type<S, connect_awaitable_promise<S&&, R&&>>> ? receiver_of<R> : receiver_of<R, await_result_type<S, connect_awaitable_promise<S&&, R&&>>>)
		operation_state_task<std::remove_cvref_t<S>, std::remove_cvref_t<R>> connect_awaitable(S&& s, R&& r) noexcept(nothrow_set_error_invocable<R&&, std::exception_ptr&&>){
            std::exception_ptr eptr;
            try {
				if constexpr (std::is_void_v<await_result_type<S, connect_awaitable_promise<S&&, R&&>>>){
                    co_await std::forward<S>(s);
                    execution::set_value(std::forward<R>(r));
                } else {
					auto&& res = co_await std::forward<S>(s);
					execution::set_value(std::forward<R>(r), std::forward<decltype(res)>(res));
				}
            } catch(...){
				eptr = std::current_exception();
			    execution::set_error(std::forward<R>(r), std::move(eptr));
			}
        }
    }

    struct connect_t{
        template<sender S, receiver R> requires tag_invocable<connect_t, S&&, R&&> && operation_state<tag_invoke_result_t<connect_t, S&&, R&&>>
        [[nodiscard]] auto operator()(S&& s, R&& r) const noexcept(nothrow_tag_invocable<connect_t, S&&, R&&>)->tag_invoke_result_t<connect_t, S&&, R&&>{
            return tag_invoke(connect_t{}, std::forward<S>(s), std::forward<R>(r));
        }
        template<typename S, receiver R> requires (!tag_invocable<connect_t, S&&, R&&>) && connect_detail::is_awaitable<S, connect_detail::connect_awaitable_promise<S, R>>
        [[nodiscard]] auto operator()(S&& s, R&& r) const noexcept{
            return connect_detail::connect_awaitable(std::forward<S>(s), std::forward<R>(r));
        }
    };
    inline constexpr connect_t connect;
	template<typename S, receiver R>
    using connect_result_t = decltype(connect(std::declval<S>(), std::declval<R>()));
    template<typename S, typename R>
    concept nothrow_connect_invocable = receiver<R> && noexcept(connect(std::declval<S>(), std::declval<R>()));

    // connect_into_handle, connect_into_variant, connect_into_optional, tuple_connect
    // `operation_state` may be uncopiable, unmovable and only constructible by `connect`, which means its construction depends on RVO.
    // it's troublesome if we need to store it in a dynamic storage, `std::optional`, `std::variant` or `std::tuple`, in this case, and then `connect_into_handler`, `connect_into_variant`, `connect_into_optional` and `tuple_connect` will be useful.
    // attention: the results of `connect_into_variant`, `connect_into_optional` and `tuple_connect` are also uncopiable and unmovable.
    // just use `std::variant`, `std::optional` and `std::tuple` instead if you need to copy or move it and all `operation_state`s are copiable and movable.
    // thinking: why `std::shared_ptr`, `std::variant`, `std::optional`, `std::tuple` and so on can not construct with something such as `std::in_place_invoke_t`, which is similar to the implementation of `std::optional::transform`?

    namespace connect_detail{
        template<typename T>
        concept dereferable = requires{ *std::declval<T>(); } && !std::is_void_v<decltype(*std::declval<T>())>;

        template<dereferable T>
        using deref_to = decltype(*std::declval<T>());

        template<typename T>
		concept operation_state_optional = dereferable<T> && operation_state<deref_to<T>>;

        template<typename T>
        concept operation_state_handler = operation_state_optional<T> && requires(T t){ { t.ownership() } noexcept->std::convertible_to<std::shared_ptr<void>>; };

        template<typename Op>
        struct buffer{
			alignas(Op) std::byte data[sizeof(Op)];
            Op* obj = nullptr;

            constexpr void reset() noexcept(std::is_nothrow_destructible_v<Op>){
                if (obj){
				    std::destroy_at(obj);
                    obj = nullptr;
                }
            }
            constexpr ~buffer() noexcept(std::is_nothrow_destructible_v<Op>){
                if (obj)
                    std::destroy_at(obj);
            }
        };

        template<typename Op>
		class operation_state_handler_impl{
            std::shared_ptr<buffer<Op>> storage;
		public:
            operation_state_handler_impl() :storage(std::make_shared<buffer<Op>>()){}
            template<typename S, typename R> requires std::same_as<connect_result_t<S&&, R&&>, Op>
            operation_state_handler_impl(S&& s, R&& r) :storage(std::make_shared<buffer<Op>>()){
                storage.get()->obj = new (storage.get()->data) Op(execution::connect(std::forward<S>(s), std::forward<R>(r)));
            }

            template<typename S, typename R> requires std::same_as<connect_result_t<S&&, R&&>, Op>
            decltype(auto) connect(S&& s, R&& r) noexcept(nothrow_connect_invocable<S&&, R&&>){
                if (!storage.get()->obj)
				    storage.get()->obj = new (storage.get()->data) Op(execution::connect(std::forward<S>(s), std::forward<R>(r)));
				return *storage.get()->obj;
            }

            std::shared_ptr<void> ownership() const noexcept{
                return storage;
            }

            Op& operator*(){
				return *storage.get()->obj;
			}
        };
        template<typename S, typename R>
        operation_state_handler_impl(S&&, R&&)->operation_state_handler_impl<connect_result_t<S&&, R&&>>;

        template<typename... Ops>
		class operation_state_variant_impl;

        template<typename T>
        struct variant_size;
        template<typename... Ops>
		struct variant_size<operation_state_variant_impl<Ops...>> : public std::integral_constant<std::size_t, sizeof...(Ops)>{};
        template<typename T>
        struct variant_size<T const> : public variant_size<T>{};
        template<typename T>
        struct variant_size<T volatile> : public variant_size<T>{};
        template<typename T>
        struct variant_size<T const volatile> : public variant_size<T>{};
        template<typename T>
        inline constexpr std::size_t variant_size_v = variant_size<T>::value;

        template<std::size_t I, typename T>
        struct variant_alternative;
        template<std::size_t I, typename... Ops>
        struct variant_alternative<I, operation_state_variant_impl<Ops...>> : public std::variant_alternative_t<I, std::variant<Ops...>>{};
        template<std::size_t I, typename... Ops>
        struct variant_alternative<I, operation_state_variant_impl<Ops...> const> : public std::variant_alternative_t<I, std::variant<Ops...> const>{};
        template<std::size_t I, typename... Ops>
        struct variant_alternative<I, operation_state_variant_impl<Ops...> volatile> : public std::variant_alternative_t<I, std::variant<Ops...> volatile>{};
        template<std::size_t I, typename... Ops>
        struct variant_alternative<I, operation_state_variant_impl<Ops...> const volatile> : public std::variant_alternative_t<I, std::variant<Ops...> const volatile>{};
        template<std::size_t I, typename T>
        using variant_alternative_t = typename variant_alternative<I, T>::type;

        template<typename... Ops>
		inline constexpr auto decay_to_operation_state_variant(operation_state_variant_impl<Ops...>&) noexcept->std::true_type;
        template<typename T>
        inline constexpr auto decay_to_operation_state_variant(T&) noexcept->std::false_type;

        template<typename T>
        concept is_true_type = requires{ T::value; } && T::value;

        template<typename T>
        concept is_base_of_operation_state_variant = requires(T& t){ { decay_to_operation_state_variant(t) }->is_true_type; };

        template<typename... Ops>
        class operation_state_variant_impl{
            using base = std::variant<std::monostate, buffer<Ops>...>;
            base data;
		public:
			constexpr operation_state_variant_impl() noexcept = default;
            template<typename S, typename R> requires ns_type_traits::any_of<connect_result_t<S&&, R&&>, Ops...>
			constexpr operation_state_variant_impl(S&& s, R&& r) noexcept(nothrow_connect_invocable<S&&, R&&>) :data(std::in_place_type<buffer<connect_result_t<S&&, R&&>>>){
				std::get<buffer<connect_result_t<S&&, R&&>>>(data).obj = new (std::get<buffer<connect_result_t<S&&, R&&>>>(data).data) connect_result_t<S&&, R&&>(execution::connect(std::forward<S>(s), std::forward<R>(r)));
			}

			operation_state_variant_impl(operation_state_variant_impl const&) noexcept = delete;
			operation_state_variant_impl(operation_state_variant_impl&&) noexcept = delete;
			operation_state_variant_impl& operator=(operation_state_variant_impl const&) noexcept = delete;
			operation_state_variant_impl& operator=(operation_state_variant_impl&&) noexcept = delete;

            constexpr std::size_t index() const noexcept{ return data.index(); }
            constexpr bool valueless_by_exception() const noexcept{ return data.valueless_by_exception(); }

            template<typename S, typename R> requires ns_type_traits::any_of<connect_result_t<S&&, R&&>, Ops...>
            constexpr auto connect(S&& s, R&& r) noexcept(nothrow_connect_invocable<S&&, R&&>)->connect_result_t<S&&, R&&>&{
				data.template emplace<buffer<connect_result_t<S&&, R&&>>>();
				std::get<buffer<connect_result_t<S&&, R&&>>>(data).obj = new (std::get<buffer<connect_result_t<S&&, R&&>>>(data).data) connect_result_t<S&&, R&&>(execution::connect(std::forward<S>(s), std::forward<R>(r)));
				return *std::get<buffer<connect_result_t<S&&, R&&>>>(data).obj;
            }

            template<typename Op> requires ns_type_traits::any_of<Op, Ops...>
			constexpr friend bool holds_alternative(operation_state_variant_impl const& variant) noexcept{
                return std::holds_alternative<buffer<Op>>(variant.data);
            }

            template<std::size_t I> requires (I < variant_size_v<operation_state_variant_impl>)
            constexpr friend variant_alternative_t<I, operation_state_variant_impl>& get(operation_state_variant_impl& variant){
				return *std::get<I>(variant.data).obj;
            }
            template<std::size_t I> requires (I < variant_size_v<operation_state_variant_impl>)
            constexpr friend variant_alternative_t<I, operation_state_variant_impl> const& get(operation_state_variant_impl const& variant){
				return std::as_const(*std::get<I>(variant.data).obj);
            }
            template<std::size_t I> requires (I < variant_size_v<operation_state_variant_impl>)
            constexpr friend variant_alternative_t<I, operation_state_variant_impl>&& get(operation_state_variant_impl&& variant){
				return std::move(*std::get<I>(variant.data).obj);
            }
            template<std::size_t I> requires (I < variant_size_v<operation_state_variant_impl>)
            constexpr friend variant_alternative_t<I, operation_state_variant_impl> const&& get(operation_state_variant_impl const&& variant){
				return std::move(std::as_const(*std::get<I>(variant.data).obj));
            }
            template<typename Op> requires ns_type_traits::any_of<Op, Ops...>
            constexpr friend Op& get(operation_state_variant_impl& variant){
                return *std::get<buffer<Op>>(variant.data).obj;
            }
            template<typename Op> requires ns_type_traits::any_of<Op, Ops...>
            constexpr friend Op const& get(operation_state_variant_impl const& variant){
				return std::as_const(*std::get<buffer<Op>>(variant.data).obj);
            }
            template<typename Op> requires ns_type_traits::any_of<Op, Ops...>
            constexpr friend Op&& get(operation_state_variant_impl&& variant){
                return std::move(*std::get<buffer<Op>>(variant.data).obj);
            }
            template<typename Op> requires ns_type_traits::any_of<Op, Ops...>
            constexpr friend Op const&& get(operation_state_variant_impl const&& variant){
				return std::move(std::as_const(*std::get<buffer<Op>>(variant.data).obj));
            }

            template<std::size_t I> requires (I < variant_size_v<operation_state_variant_impl>)
            constexpr friend std::add_pointer_t<variant_alternative_t<I, operation_state_variant_impl>> get_if(operation_state_variant_impl* variant) noexcept{
				auto* ptr = variant ? std::get_if<I>(variant->data) : nullptr;
				return ptr ? ptr->obj : nullptr;
            }
            template<std::size_t I> requires (I < variant_size_v<operation_state_variant_impl>)
            constexpr friend std::add_pointer_t<variant_alternative_t<I, operation_state_variant_impl> const> get_if(operation_state_variant_impl const* variant) noexcept{
				auto* ptr = variant ? std::get_if<I>(variant->data) : nullptr;
				return ptr ? ptr->obj : nullptr;
            }
            template<typename Op> requires ns_type_traits::any_of<Op, Ops...>
            constexpr friend std::add_pointer_t<Op> get_if(operation_state_variant_impl* variant) noexcept{
				auto* ptr = variant ? std::get_if<buffer<Op>>(variant->data) : nullptr;
				return ptr ? ptr->obj : nullptr;
            }
            template<typename Op> requires ns_type_traits::any_of<Op, Ops...>
            constexpr friend std::add_pointer_t<Op const> get_if(operation_state_variant_impl const* variant) noexcept{
				auto* ptr = variant ? std::get_if<buffer<Op>>(variant->data) : nullptr;
				return ptr ? ptr->obj : nullptr;
            }

            template<typename Visitor, typename... Variants> requires (is_base_of_operation_state_variant<Variants> && ...)
		    inline constexpr friend decltype(auto) visit(Visitor&& visitor, Variants&&... variants);
            template<typename R, typename Visitor, typename... Variants> requires (is_base_of_operation_state_variant<Variants> && ...)
		    inline constexpr friend R visit(Visitor&& visitor, Variants&&... variants);
        };

        template<typename... Ops>
        using operation_state_variant_t = ns_type_list::type_list_apply_t<ns_type_list::type_list_unique_t<ns_type_list::type_list<Ops...>>, operation_state_variant_impl>;

        template<typename Buffer>
        inline decltype(auto) getOp(Buffer&& buffer) noexcept{
            return std::forward<ns_type_traits::member_type<Buffer, std::remove_cvref_t<decltype(*buffer.obj)>>>(*buffer.obj);
        }
        inline auto getOp(std::monostate ms) noexcept{
            return ms;
        }

		template<typename Visitor, typename... Variants> requires (is_base_of_operation_state_variant<Variants> && ...)
		inline constexpr decltype(auto) visit(Visitor&& visitor, Variants&&... variants){
            return std::visit([&visitor]<typename... Buffers>(Buffers&&... buffers){
                return std::invoke(std::forward<Visitor>(visitor), getOp(std::forward<Buffers>(buffers))...);
            }, std::forward<Variants>(variants).data...);
        }
        template<typename R, typename Visitor, typename... Variants> requires (is_base_of_operation_state_variant<Variants> && ...)
		inline constexpr R visit(Visitor&& visitor, Variants&&... variants){
            return (R)std::visit([&visitor]<typename... Buffers>(Buffers&&... buffers){
                return std::invoke(std::forward<Visitor>(visitor), getOp(std::forward<Buffers>(buffers))...);
            }, std::forward<Variants>(variants).data...);
        }

        template<typename Op>
        class operation_state_optional_impl{
            buffer<Op> storage;
		public:
            constexpr operation_state_optional_impl() noexcept = default;
            template<typename S, typename R> requires std::same_as<connect_result_t<S&&, R&&>, Op>
            constexpr operation_state_optional_impl(S&& s, R&& r) noexcept{
                storage.obj = new (storage.data) Op(execution::connect(std::forward<S>(s), std::forward<R>(r)));
            }

			operation_state_optional_impl(operation_state_optional_impl const&) noexcept = delete;
			operation_state_optional_impl(operation_state_optional_impl&&) noexcept = delete;
			operation_state_optional_impl& operator=(operation_state_optional_impl const&) noexcept = delete;
			operation_state_optional_impl& operator=(operation_state_optional_impl&&) noexcept = delete;

            template<typename S, typename R> requires std::same_as<connect_result_t<S&&, R&&>, Op>
            constexpr decltype(auto) connect(S&& s, R&& r) noexcept(nothrow_connect_invocable<S&&, R&&>){
                if (!storage.obj)
				    storage.obj = new (storage.data) Op(execution::connect(std::forward<S>(s), std::forward<R>(r)));
                return *storage.obj;
            }

            constexpr Op* operator->() noexcept{
                return storage.obj;
            }
            constexpr Op const* operator->() const noexcept{
                return storage.obj;
            }
            constexpr Op& operator*() & noexcept{
                return *storage.obj;
            }
            constexpr Op const& operator*() const& noexcept{
                return *storage.obj;
            }
            constexpr Op&& operator*() && noexcept{
                return std::move(*storage.obj);
            }
            constexpr Op const&& operator*() const&& noexcept{
                return std::move(*storage.obj);
            }

            constexpr operator bool() const noexcept{
                return storage.obj;
            }
            constexpr bool has_value() const noexcept{
                return storage.obj;
            }

            constexpr Op& value() & noexcept{
                return *storage.obj;
            }
            constexpr Op const& value() const& noexcept{
                return *storage.obj;
            }
            constexpr Op&& value() && noexcept{
                return std::move(*storage.obj);
            }
            constexpr Op const&& value() const&& noexcept{
                return std::move(*storage.obj);
            }

            template<typename F>
            constexpr auto and_then(F&& func) & noexcept(std::is_nothrow_invocable_v<F&&, Op&> && std::is_nothrow_constructible_v<std::remove_cvref_t<std::invoke_result_t<F&&, Op&>>, std::invoke_result_t<F&&, Op&>> && std::is_nothrow_default_constructible_v<std::remove_cvref_t<std::invoke_result_t<F&&, Op&>>>){
                if (has_value())
                    return std::invoke(std::forward<F>(func), value());
				else
					return std::remove_cvref_t<std::invoke_result_t<F&&, Op&>>{};
            }
            template<typename F>
            constexpr auto and_then(F&& func) const& noexcept(std::is_nothrow_invocable_v<F&&, Op const&> && std::is_nothrow_constructible_v<std::remove_cvref_t<std::invoke_result_t<F&&, Op const&>>, std::invoke_result_t<F&&, Op const&>> && std::is_nothrow_default_constructible_v<std::remove_cvref_t<std::invoke_result_t<F&&, Op const&>>>){
                if (has_value())
                    return std::invoke(std::forward<F>(func), value());
				else
					return std::remove_cvref_t<std::invoke_result_t<F&&, Op const&>>{};
            }
            template<typename F>
            constexpr auto and_then(F&& func) && noexcept(std::is_nothrow_invocable_v<F&&, Op&&> && std::is_nothrow_constructible_v<std::remove_cvref_t<std::invoke_result_t<F&&, Op&&>>, std::invoke_result_t<F&&, Op&&>> && std::is_nothrow_default_constructible_v<std::remove_cvref_t<std::invoke_result_t<F&&, Op&&>>>){
                if (has_value())
					return std::invoke(std::forward<F>(func), std::move(value()));
				else
					return std::remove_cvref_t<std::invoke_result_t<F&&, Op&&>>{};
            }
            template<typename F>
            constexpr auto and_then(F&& func) const&& noexcept(std::is_nothrow_invocable_v<F&&, Op const&&> && std::is_nothrow_constructible_v<std::remove_cvref_t<std::invoke_result_t<F&&, Op const&&>>, std::invoke_result_t<F&&, Op const&&>> && std::is_nothrow_default_constructible_v<std::remove_cvref_t<std::invoke_result_t<F&&, Op const&&>>>){
                if (has_value())
					return std::invoke(std::forward<F>(func), std::move(value()));
				else
					return std::remove_cvref_t<std::invoke_result_t<F&&, Op const&&>>{};
            }

            // weak implementation: not in place construction. requires std::is_move_constructible<U>.
            template<typename F>
            constexpr auto transform(F&& func) & noexcept(std::is_nothrow_invocable_v<F&&, Op&> && std::is_nothrow_constructible_v<std::optional<std::invoke_result_t<F&&, Op&>>, std::invoke_result_t<F&&, Op&>>){
                using U = std::remove_cv_t<std::invoke_result_t<F&&, Op&>>;
                if (has_value())
                    return std::optional<U>(std::invoke(std::forward<F>(func), value()));
				else
					return std::optional<U>{};
            }
            template<typename F>
            constexpr auto transform(F&& func) const& noexcept(std::is_nothrow_invocable_v<F&&, Op const&> && std::is_nothrow_constructible_v<std::optional<std::invoke_result_t<F&&, Op const&>>, std::invoke_result_t<F&&, Op const&>>){
                using U = std::remove_cv_t<std::invoke_result_t<F&&, Op const&>>;
                if (has_value())
                    return std::optional<U>(std::invoke(std::forward<F>(func), value()));
				else
					return std::optional<U>{};
            }
            template<typename F>
            constexpr auto transform(F&& func) && noexcept(std::is_nothrow_invocable_v<F&&, Op&&> && std::is_nothrow_constructible_v<std::optional<std::invoke_result_t<F&&, Op&&>>, std::invoke_result_t<F&&, Op&&>>){
                using U = std::remove_cv_t<std::invoke_result_t<F&&, Op&&>>;
                if (has_value())
					return std::optional<U>(std::invoke(std::forward<F>(func), std::move(value())));
				else
					return std::optional<U>{};
            }
            template<typename F>
            constexpr auto transform(F&& func) const&& noexcept(std::is_nothrow_invocable_v<F&&, Op const&&> && std::is_nothrow_constructible_v<std::optional<std::invoke_result_t<F&&, Op const&&>>, std::invoke_result_t<F&&, Op const&&>>){
                using U = std::remove_cv_t<std::invoke_result_t<F&&, Op const&&>>;
                if (has_value())
					return std::optional<U>(std::invoke(std::forward<F>(func), std::move(value())));
				else
					return std::optional<U>{};
            }

            constexpr void reset() noexcept{
                storage.reset();
            }
        };
        template<typename S, typename R>
		operation_state_optional_impl(S&& s, R&& r) -> operation_state_optional_impl<connect_result_t<S&&, R&&>>;

		template<typename... Ops>
		class operation_state_tuple_impl;

        template<typename T>
        struct tuple_size;
        template<typename... Ops>
		struct tuple_size<operation_state_tuple_impl<Ops...>> : public std::integral_constant<std::size_t, sizeof...(Ops)>{};
        template<typename T>
        struct tuple_size<T const> : public tuple_size<T>{};
        template<typename T>
        struct tuple_size<T volatile> : public tuple_size<T>{};
        template<typename T>
        struct tuple_size<T const volatile> : public tuple_size<T>{};
        template<typename T>
        inline constexpr std::size_t tuple_size_v = tuple_size<T>::value;

        template<std::size_t I, typename T>
        struct tuple_element;
        template<std::size_t I, typename... Ops>
        struct tuple_element<I, operation_state_tuple_impl<Ops...>> : public std::tuple_element<I, std::tuple<Ops...>>{};
        template<std::size_t I, typename... Ops>
        struct tuple_element<I, operation_state_tuple_impl<Ops...> const> : public std::tuple_element<I, std::tuple<Ops...> const>{};
        template<std::size_t I, typename... Ops>
        struct tuple_element<I, operation_state_tuple_impl<Ops...> volatile> : public std::tuple_element<I, std::tuple<Ops...> volatile>{};
        template<std::size_t I, typename... Ops>
        struct tuple_element<I, operation_state_tuple_impl<Ops...> const volatile> : public std::tuple_element<I, std::tuple<Ops...> const volatile>{};
        template<std::size_t I, typename T>
        using tuple_element_t = typename tuple_element<I, T>::type;

        template<typename... Ops>
        class operation_state_tuple_impl{
            using base = std::tuple<buffer<Ops>...>;
            base data;
		public:
			constexpr operation_state_tuple_impl() noexcept = default;
            template<typename... Ss, typename... Rs> requires std::same_as<std::tuple<connect_result_t<Ss&, Rs&>...>, std::tuple<Ops...>>
			constexpr operation_state_tuple_impl(std::tuple<Ss...>& ss, std::tuple<Rs...>& rs) noexcept((nothrow_connect_invocable<Ss&, Rs&> && ...)){
				((std::get<buffer<connect_result_t<Ss&, Rs&>>>(data).obj = new (std::get<buffer<connect_result_t<Ss&, Rs&>>>(data).data) connect_result_t<Ss&, Rs&>(execution::connect(std::get<Ss>(ss), std::get<Rs>(rs)))) , ...);
			}
            template<typename... Ss, typename... Rs> requires std::same_as<std::tuple<connect_result_t<Ss const&, Rs const&>...>, std::tuple<Ops...>>
			constexpr operation_state_tuple_impl(std::tuple<Ss...> const& ss, std::tuple<Rs...> const& rs) noexcept((nothrow_connect_invocable<Ss const&, Rs const&> && ...)){
				((std::get<buffer<connect_result_t<Ss const&, Rs const&>>>(data).obj = new (std::get<buffer<connect_result_t<Ss const&, Rs const&>>>(data).data) connect_result_t<Ss const&, Rs const&>(execution::connect(std::get<Ss>(ss), std::get<Rs>(rs)))) , ...);
			}
            template<typename... Ss, typename... Rs> requires std::same_as<std::tuple<connect_result_t<Ss&&, Rs&&>...>, std::tuple<Ops...>>
			constexpr operation_state_tuple_impl(std::tuple<Ss...>&& ss, std::tuple<Rs...>&& rs) noexcept((nothrow_connect_invocable<Ss&&, Rs&&> && ...)){
				((std::get<buffer<connect_result_t<Ss&&, Rs&&>>>(data).obj = new (std::get<buffer<connect_result_t<Ss&&, Rs&&>>>(data).data) connect_result_t<Ss&&, Rs&&>(execution::connect(std::get<Ss>(std::move(ss)), std::get<Rs>(std::move(rs))))) , ...);
			}
            template<typename... Ss, typename... Rs> requires std::same_as<std::tuple<connect_result_t<Ss const&&, Rs const&&>...>, std::tuple<Ops...>>
			constexpr operation_state_tuple_impl(std::tuple<Ss...> const&& ss, std::tuple<Rs...> const&& rs) noexcept((nothrow_connect_invocable<Ss const&&, Rs const&&> && ...)){
				((std::get<buffer<connect_result_t<Ss const&&, Rs const&&>>>(data).obj = new (std::get<buffer<connect_result_t<Ss const&&, Rs const&&>>>(data).data) connect_result_t<Ss const&&, Rs const&&>(execution::connect(std::get<Ss>(std::move(ss)), std::get<Rs>(std::move(rs))))) , ...);
			}

            template<typename... Ss, typename... Rs> requires std::same_as<std::tuple<connect_result_t<Ss&, Rs&>...>, std::tuple<Ops...>>
			constexpr operation_state_tuple_impl& connect(std::tuple<Ss...>& ss, std::tuple<Rs...>& rs) noexcept((nothrow_connect_invocable<Ss&, Rs&> && ...)){
				((std::get<buffer<connect_result_t<Ss&, Rs&>>>(data).obj = new (std::get<buffer<connect_result_t<Ss&, Rs&>>>(data).data) connect_result_t<Ss&, Rs&>(execution::connect(std::get<Ss>(ss), std::get<Rs>(rs)))) , ...);
                return *this;
			}
            template<typename... Ss, typename... Rs> requires std::same_as<std::tuple<connect_result_t<Ss const&, Rs const&>...>, std::tuple<Ops...>>
			constexpr operation_state_tuple_impl& connect(std::tuple<Ss...> const& ss, std::tuple<Rs...> const& rs) noexcept((nothrow_connect_invocable<Ss const&, Rs const&> && ...)){
				((std::get<buffer<connect_result_t<Ss const&, Rs const&>>>(data).obj = new (std::get<buffer<connect_result_t<Ss const&, Rs const&>>>(data).data) connect_result_t<Ss const&, Rs const&>(execution::connect(std::get<Ss>(ss), std::get<Rs>(rs)))) , ...);
                return *this;
			}
            template<typename... Ss, typename... Rs> requires std::same_as<std::tuple<connect_result_t<Ss&&, Rs&&>...>, std::tuple<Ops...>>
			constexpr operation_state_tuple_impl& connect(std::tuple<Ss...>&& ss, std::tuple<Rs...>&& rs) noexcept((nothrow_connect_invocable<Ss&&, Rs&&> && ...)){
				((std::get<buffer<connect_result_t<Ss&&, Rs&&>>>(data).obj = new (std::get<buffer<connect_result_t<Ss&&, Rs&&>>>(data).data) connect_result_t<Ss&&, Rs&&>(execution::connect(std::get<Ss>(std::move(ss)), std::get<Rs>(std::move(rs))))) , ...);
                return *this;
			}
            template<typename... Ss, typename... Rs> requires std::same_as<std::tuple<connect_result_t<Ss const&&, Rs const&&>...>, std::tuple<Ops...>>
			constexpr operation_state_tuple_impl& connect(std::tuple<Ss...> const&& ss, std::tuple<Rs...> const&& rs) noexcept((nothrow_connect_invocable<Ss const&&, Rs const&&> && ...)){
				((std::get<buffer<connect_result_t<Ss const&&, Rs const&&>>>(data).obj = new (std::get<buffer<connect_result_t<Ss const&&, Rs const&&>>>(data).data) connect_result_t<Ss const&&, Rs const&&>(execution::connect(std::get<Ss>(std::move(ss)), std::get<Rs>(std::move(rs))))) , ...);
                return *this;
			}

			operation_state_tuple_impl(operation_state_tuple_impl const&) noexcept = delete;
			operation_state_tuple_impl(operation_state_tuple_impl&&) noexcept = delete;
			operation_state_tuple_impl& operator=(operation_state_tuple_impl const&) noexcept = delete;
			operation_state_tuple_impl& operator=(operation_state_tuple_impl&&) noexcept = delete;

            template<std::size_t I> requires (I < tuple_size_v<operation_state_tuple_impl>)
            constexpr friend tuple_element_t<I, operation_state_tuple_impl>& get(operation_state_tuple_impl& tuple){
				return *std::get<I>(tuple.data).obj;
            }
            template<std::size_t I> requires (I < tuple_size_v<operation_state_tuple_impl>)
            constexpr friend tuple_element_t<I, operation_state_tuple_impl> const& get(operation_state_tuple_impl const& tuple){
				return std::as_const(*std::get<I>(tuple.data).obj);
            }
            template<std::size_t I> requires (I < tuple_size_v<operation_state_tuple_impl>)
            constexpr friend tuple_element_t<I, operation_state_tuple_impl>&& get(operation_state_tuple_impl&& tuple){
				return std::move(*std::get<I>(tuple.data).obj);
            }
            template<std::size_t I> requires (I < tuple_size_v<operation_state_tuple_impl>)
            constexpr friend tuple_element_t<I, operation_state_tuple_impl> const&& get(operation_state_tuple_impl const&& tuple){
				return std::move(std::as_const(*std::get<I>(tuple.data).obj));
            }
            template<typename Op> requires ns_type_traits::any_of<Op, Ops...>
            constexpr friend Op& get(operation_state_tuple_impl& tuple){
                return *std::get<buffer<Op>>(tuple.data).obj;
            }
            template<typename Op> requires ns_type_traits::any_of<Op, Ops...>
            constexpr friend Op const& get(operation_state_tuple_impl const& tuple){
				return std::as_const(*std::get<buffer<Op>>(tuple.data).obj);
            }
            template<typename Op> requires ns_type_traits::any_of<Op, Ops...>
            constexpr friend Op&& get(operation_state_tuple_impl&& tuple){
                return std::move(*std::get<buffer<Op>>(tuple.data).obj);
            }
            template<typename Op> requires ns_type_traits::any_of<Op, Ops...>
            constexpr friend Op const&& get(operation_state_tuple_impl const&& tuple){
				return std::move(std::as_const(*std::get<buffer<Op>>(tuple.data).obj));
            }

            template<typename F, typename Tuple, std::size_t... Is>
            static constexpr decltype(auto) apply_impl(F&& func, Tuple&& tuple, std::index_sequence<Is...>* = nullptr){
				return std::invoke(std::forward<F>(func), get<Is>(std::forward<Tuple>(tuple))...);
            }
            template<typename F>
			constexpr friend decltype(auto) apply(F&& func, operation_state_tuple_impl& tuple){
                return apply_impl(std::forward<F>(func), tuple, (std::index_sequence_for<Ops...>*)nullptr);
            }
            template<typename F>
			constexpr friend decltype(auto) apply(F&& func, operation_state_tuple_impl const& tuple){
                return apply_impl(std::forward<F>(func), tuple, (std::index_sequence_for<Ops...>*)nullptr);
            }
            template<typename F>
			constexpr friend decltype(auto) apply(F&& func, operation_state_tuple_impl&& tuple){
				return apply_impl(std::forward<F>(func), std::move(tuple), (std::index_sequence_for<Ops...>*)nullptr);
            }
            template<typename F>
			constexpr friend decltype(auto) apply(F&& func, operation_state_tuple_impl const&& tuple){
				return apply_impl(std::forward<F>(func), std::move(tuple), (std::index_sequence_for<Ops...>*)nullptr);
            }
        };
        template<typename... Ss, typename... Rs>
        operation_state_tuple_impl(std::tuple<Ss...>, std::tuple<Rs...>)->operation_state_tuple_impl<connect_result_t<Ss, Rs>...>;

        template<typename tupleS, typename tupleR>
        struct tuple_connectable_impl : public std::false_type{};
		template<typename... Ss, typename... Rs>
		struct tuple_connectable_impl<std::tuple<Ss...>, std::tuple<Rs...>> : public std::bool_constant<(std::invocable<connect_t, Ss, Rs> && ...)>{};
        template<typename tupleS, typename tupleR>
        struct nothrow_tuple_connectable_impl : public std::false_type{};
		template<typename... Ss, typename... Rs>
		struct nothrow_tuple_connectable_impl<std::tuple<Ss...>, std::tuple<Rs...>> : public std::bool_constant<(nothrow_connect_invocable<Ss, Rs> && ...)>{};
        template<typename tupleS, typename tupleR>
        struct connect_tuple_result;
		template<typename... Ss, typename... Rs>
		struct connect_tuple_result<std::tuple<Ss...>, std::tuple<Rs...>> : public std::type_identity<std::tuple<connect_result_t<Ss, Rs>...>>{};
        template<typename tuple, typename remove_cvref_tuple>
        struct tuple_element_type : public std::type_identity<tuple>{};
        template<typename tuple, typename... Ts>
        struct tuple_element_type<tuple, std::tuple<Ts...>> : public std::type_identity<std::tuple<ns_type_traits::member_type<tuple, Ts>...>>{};
        template<typename tuple>
        using tuple_element_type_t = typename tuple_element_type<tuple, std::remove_cvref_t<tuple>>::type;
        template<typename tupleS, typename tupleR>
		concept tuple_connectable = tuple_connectable_impl<tuple_element_type_t<tupleS>, tuple_element_type_t<tupleR>>::value;
        template<typename tupleS, typename tupleR>
		concept nothrow_tuple_connectable = nothrow_tuple_connectable_impl<tuple_element_type_t<tupleS>, tuple_element_type_t<tupleR>>::value;
		template<typename tupleS, typename tupleR>
        using connect_tuple_result_t = typename connect_tuple_result<tuple_element_type_t<tupleS>, tuple_element_type_t<tupleR>>::type;
    }

    struct connect_into_handler_t{
        template<typename S, receiver R> requires tag_invocable<connect_into_handler_t, S&&, R&&> && connect_detail::operation_state_handler<tag_invoke_result_t<connect_into_handler_t, S&&, R&&>>
        [[nodiscard]] auto operator()(S&& s, R&& r) const noexcept(nothrow_tag_invocable<connect_into_handler_t, S&&, R&&>)->tag_invoke_result_t<connect_into_handler_t, S&&, R&&>{
            return tag_invoke(connect_into_handler_t{}, std::forward<S>(s), std::forward<R>(r));
        }
        template<typename S, receiver R> requires (!tag_invocable<connect_into_handler_t, S&&, R&&>) && std::invocable<connect_t, S&&, R&&>
        [[nodiscard]] auto operator()(S&& s, R&& r) const{
			return connect_detail::operation_state_handler_impl(std::forward<S>(s), std::forward<R>(r));
        }
        template<typename Op, typename S, receiver R> requires std::invocable<connect_t, S&&, R&&> && std::same_as<connect_result_t<S&&, R&&>, Op>
        auto operator()(connect_detail::operation_state_handler_impl<Op> handler, S&& s, R&& r) const noexcept(nothrow_connect_invocable<S&&, R&&>){
            handler.connect(std::forward<S>(s), std::forward<R>(r));
            return handler;
        }
        template<typename S, typename R>
        [[nodiscard]] auto empty() const{
            return connect_detail::operation_state_handler_impl<connect_result_t<S, R>>{};
        }
        template<typename Op>
        [[nodiscard]] auto empty() const{
            return connect_detail::operation_state_handler_impl<Op>{};
        }
    };
    inline constexpr connect_into_handler_t connect_into_handler;
	template<typename S, receiver R>
    using connect_into_handler_result_t = decltype(connect_into_handler(std::declval<S>(), std::declval<R>()));
    template<typename S, typename R>
    concept nothrow_connect_into_handler_invocable = receiver<R> && noexcept(connect_into_handler(std::declval<S>(), std::declval<R>()));

    struct connect_into_variant_t{
        template<typename... Ops, typename S, receiver R> requires std::invocable<connect_t, S&&, R&&> && ns_type_traits::any_of<connect_result_t<S&&, R&&>, Ops...>
        decltype(auto) operator()(connect_detail::operation_state_variant_impl<Ops...>& variant, S&& s, R&& r) const noexcept(nothrow_connect_invocable<S&&, R&&>){
            variant.connect(std::forward<S>(s), std::forward<R>(r));
            return variant;
        }
        template<typename... Ops>
        [[nodiscard]] auto empty() const noexcept{
            return connect_detail::operation_state_variant_t<Ops...>{};
        }
    };
    inline constexpr connect_into_variant_t connect_into_variant;

    struct connect_into_optional_t{
        template<typename S, receiver R> requires tag_invocable<connect_into_optional_t, S&&, R&&> && connect_detail::operation_state_optional<tag_invoke_result_t<connect_into_optional_t, S&&, R&&>>
        [[nodiscard]] auto operator()(S&& s, R&& r) const noexcept(nothrow_tag_invocable<connect_into_optional_t, S&&, R&&>)->tag_invoke_result_t<connect_into_optional_t, S&&, R&&>{
            return tag_invoke(connect_into_optional_t{}, std::forward<S>(s), std::forward<R>(r));
        }
        template<typename S, receiver R> requires (!tag_invocable<connect_into_optional_t, S&&, R&&>) && std::invocable<connect_t, S&&, R&&>
        [[nodiscard]] auto operator()(S&& s, R&& r) const{
			return connect_detail::operation_state_optional_impl(std::forward<S>(s), std::forward<R>(r));
        }
        template<typename Op, typename S, receiver R> requires std::invocable<connect_t, S&&, R&&> && std::same_as<connect_result_t<S&&, R&&>, Op>
        decltype(auto) operator()(connect_detail::operation_state_optional_impl<Op>& optional, S&& s, R&& r) const noexcept(nothrow_connect_invocable<S&&, R&&>){
            optional.connect(std::forward<S>(s), std::forward<R>(r));
            return optional;
        }
        template<typename Op, typename S, receiver R> requires std::invocable<connect_t, S&&, R&&> && std::same_as<connect_result_t<S&&, R&&>, Op>
        decltype(auto) operator()(connect_detail::operation_state_optional_impl<Op>&& optional, S&& s, R&& r) const noexcept(nothrow_connect_invocable<S&&, R&&>){
            optional.connect(std::forward<S>(s), std::forward<R>(r));
			return std::move(optional);
        }
        template<typename S, typename R>
        [[nodiscard]] auto empty() const{
            return connect_detail::operation_state_optional_impl<connect_result_t<S, R>>{};
        }
        template<typename Op>
        [[nodiscard]] auto empty() const{
            return connect_detail::operation_state_optional_impl<Op>{};
        }
    };
    inline constexpr connect_into_optional_t connect_into_optional;
	template<typename S, receiver R>
    using connect_into_optional_result_t = decltype(connect_into_optional(std::declval<S>(), std::declval<R>()));
    template<typename S, typename R>
    concept nothrow_connect_into_optional_invocable = receiver<R> && noexcept(connect_into_optional(std::declval<S>(), std::declval<R>()));

    struct tuple_connect_t{
        template<typename tupleS, typename tupleR> requires tag_invocable<tuple_connect_t, tupleS&&, tupleR&&>
        [[nodiscard]] auto operator()(tupleS&& ss, tupleR&& rs) const noexcept(nothrow_tag_invocable<tuple_connect_t, tupleS&&, tupleR&&>)->tag_invoke_result_t<tuple_connect_t, tupleS&&, tupleR&&>{
            return tag_invoke(tuple_connect_t{}, std::forward<tupleS>(ss), std::forward<tupleR>(rs));
        }
        template<typename tupleS, typename tupleR> requires (!tag_invocable<tuple_connect_t, tupleS&&, tupleR&&>) && connect_detail::tuple_connectable<tupleS&&, tupleR&&>
        [[nodiscard]] auto operator()(tupleS&& ss, tupleR&& rs) const noexcept(connect_detail::nothrow_tuple_connectable<tupleS&&, tupleR&&>){
			return connect_detail::operation_state_tuple_impl(std::forward<tupleS>(ss), std::forward<tupleR>(rs));
        }
        template<typename... Ops, typename tupleS, typename tupleR> requires connect_detail::tuple_connectable<tupleS&&, tupleR&&> && std::same_as<connect_detail::connect_tuple_result_t<tupleS&&, tupleR&&>, std::tuple<Ops...>>
		decltype(auto) operator()(connect_detail::operation_state_tuple_impl<Ops...>& tuple, tupleS&& ss, tupleR&& rs) const noexcept(connect_detail::nothrow_tuple_connectable<tupleS&&, tupleR&&>){
            tuple.connect(std::forward<tupleS>(ss), std::forward<tupleR>(rs));
            return tuple;
        }
        template<typename tupleS, typename tupleR>
        [[nodiscard]] auto empty() const noexcept{
			return ns_type_list::type_list_apply_t<ns_type_list::to_type_list_t<connect_detail::connect_tuple_result_t<tupleS, tupleR>>, connect_detail::operation_state_tuple_impl>{};
        }
        template<typename... Ops>
        [[nodiscard]] auto empty() const noexcept{
            return connect_detail::operation_state_tuple_impl<Ops...>{};
        }
    };
    inline constexpr tuple_connect_t tuple_connect;
	template<typename tupleS, typename tupleR>
    using tuple_connect_result_t = decltype(tuple_connect(std::declval<tupleS>(), std::declval<tupleR>()));
    template<typename tupleS, typename tupleR>
    concept nothrow_tuple_connect_invocable = noexcept(tuple_connect(std::declval<tupleS>(), std::declval<tupleR>()));

    // sender_to

    template<typename S, typename R>
    concept sender_to = sender<S> && receiver<R> && requires(S&& s, R&& r){
        execution::connect(std::forward<S>(s), std::move(r));
    };

    // coroutine utilities

    namespace coroutine_detail{
        using completion_signatures_of_detail::is_awaitable;

        template<typename...>
        struct place_holder1;
        template<typename...>
        struct place_holder2;

        template<typename T>
        struct check_value_types;
        template<>
        struct check_value_types<place_holder1<>> : public std::type_identity<void>{};
        template<>
        struct check_value_types<place_holder1<place_holder2<>>> : public std::type_identity<void>{};
        template<typename T>
        struct check_value_types<place_holder1<place_holder2<T>>> : public std::type_identity<T>{};

        template<typename S, typename E>
        using single_sender_value_type = typename check_value_types<value_types_of_t<S, E, place_holder2, place_holder1>>::type;

        template<typename S, typename E>
        concept single_sender = sender<S, E> && requires{ typename single_sender_value_type<S, E>; };

        template<typename S, typename P>
        class sender_awaitable{
            struct unit{};
            using value_t = single_sender_value_type<S, env_of_t<P>>;
            using result_t = std::conditional_t<std::is_void_v<value_t>, unit, value_t>;
		public:
            struct awaitable_receiver{
                std::variant<std::monostate, result_t, std::exception_ptr>* result_ptr;
                std::coroutine_handle<P> continuation;

                friend void tag_invoke(set_value_t, awaitable_receiver&& r) noexcept requires(std::is_void_v<value_t>){
                    (r.result_ptr->template emplace<1>(), r.continuation.resume());
                }
                template<typename R> requires std::same_as<std::remove_cvref_t<R>, result_t> && (!std::is_void_v<value_t>)
                friend void tag_invoke(set_value_t, awaitable_receiver&& r, R&& v) noexcept{
                    (r.result_ptr->template emplace<1>(std::forward<R>(v)), r.continuation.resume());
                }
                template<typename E>
				friend void tag_invoke(set_error_t, awaitable_receiver&& r, E&& e) noexcept{
                    if constexpr (std::same_as<std::remove_cvref_t<E>, std::exception_ptr>)
						(r.result_ptr->template emplace<2>(std::forward<E>(e)), r.continuation.resume());
					else if constexpr (std::same_as<std::remove_cvref_t<E>, std::error_code>)
						(r.result_ptr->template emplace<2>(std::make_exception_ptr(std::system_error(std::forward<E>(e)))), r.continuation.resume());
					else
						(r.result_ptr->template emplace<2>(std::make_exception_ptr(std::forward<E>(e))), r.continuation.resume());
                }
				friend void tag_invoke(set_stopped_t, awaitable_receiver&& r) noexcept{
					static_cast<std::coroutine_handle<>>(r.continuation.promise().unhandled_stopped()).resume();
                }
                template<typename Tag, typename... Args> requires (execution::forwarding_receiver_query(Tag{}))
                friend decltype(auto) tag_invoke(Tag tag, awaitable_receiver const& r, Args&&... args) noexcept(std::is_nothrow_invocable_v<Tag, P const&, Args&&...>){
                    return tag(std::as_const(r.continuation.promise()), std::forward<Args>(args)...);
                }
            };
		private:
            std::variant<std::monostate, result_t, std::exception_ptr> result;
			connect_result_t<S, awaitable_receiver> op;
		public:
            template<typename S2>
			sender_awaitable(S2&& s, P& p) noexcept(nothrow_connect_invocable<S2&&, awaitable_receiver>) :op(execution::connect(std::forward<S2>(s), awaitable_receiver{ &result, std::coroutine_handle<P>::from_promise(p) })){};
            [[nodiscard]] bool await_ready() const noexcept{ return false; }
            void await_suspend(std::coroutine_handle<P>) noexcept{ execution::start(op); }
            value_t await_resume(){
                if (result.index() == 2)
                    std::rethrow_exception(std::get<2>(result));
				if constexpr (!std::is_void_v<value_t>)
                    return static_cast<value_t&&>(std::get<1>(result));
            }
        };
        template<typename S, typename P>
        using awaitable_receiver = typename sender_awaitable<S, std::remove_cvref_t<P>>::awaitable_receiver;

        template<typename S, typename P>
        concept awaitable_sender = single_sender<S, env_of_t<P>> && sender_to<S, awaitable_receiver<S, P>> && requires(P& p){
            { p.unhandled_stopped() }->std::convertible_to<std::coroutine_handle<>>;
        };

        struct as_awaitable_t{
            template<typename E, typename P> requires tag_invocable<as_awaitable_t, E&&, P&&> && is_awaitable<tag_invoke_result_t<as_awaitable_t, E&&, P&&>>
            [[nodiscard]] auto operator()(E&& e, P&& p) const noexcept(nothrow_tag_invocable<as_awaitable_t, E&&, P&&>)->tag_invoke_result_t<as_awaitable_t, E&&, P&&>{
                return tag_invoke(as_awaitable_t{}, std::forward<E>(e), std::forward<P>(p));
            }
            template<typename E, typename P> requires (!tag_invocable<as_awaitable_t, E&&, P&&>) && is_awaitable<E>
            [[nodiscard]] decltype(auto) operator()(E&& e, P&& p) const noexcept{
                return std::forward<E>(e);
            }
            template<typename E, typename P> requires (!tag_invocable<as_awaitable_t, E&&, P&&>) && (!is_awaitable<E>) && awaitable_sender<E, P>
            [[nodiscard]] auto operator()(E&& e, P&& p) const noexcept(std::is_nothrow_constructible_v<sender_awaitable<E, std::remove_cvref_t<P>>, E&&, P&&>){
				return sender_awaitable<E, std::remove_cvref_t<P>>{ std::forward<E>(e), std::forward<P>(p) };
            }
            template<typename E, typename P> requires (!tag_invocable<as_awaitable_t, E&&, P&&>) && (!is_awaitable<E>) && (!awaitable_sender<E, P>)
            [[nodiscard]] decltype(auto) operator()(E&& e, P&& p) const noexcept{
				return std::forward<E>(e);
            }
        };

        template<typename Promise> requires std::same_as<std::remove_cvref_t<Promise>, Promise> && std::is_class_v<Promise>
        struct with_awaitable_senders{
            template<typename OtherPromise> requires (!std::same_as<OtherPromise, void>)
            void set_continuation(std::coroutine_handle<OtherPromise> h) noexcept{
                this_continuation = h;
                if constexpr (requires(OtherPromise& other){ other.unhandled_stopped(); })
                    stopped_handler = [](void* p) noexcept->std::coroutine_handle<>{
                        return std::coroutine_handle<OtherPromise>::from_address(p).promise().unhandled_stopped();
                    };
				else
					stopped_handler = &default_unhandled_stopped;
            }

            [[nodiscard]] std::coroutine_handle<> continuation() const noexcept{ return this_continuation; }

            [[nodiscard]] std::coroutine_handle<> unhandled_stopped() noexcept{
                return stopped_handler(this_continuation.address());
            }

            template<typename Value>
            [[nodiscard]] decltype(auto) await_transform(Value&& value) noexcept(std::is_nothrow_invocable_v<as_awaitable_t, Value&&, Promise&>){
				return as_awaitable_t{}(std::forward<Value>(value), static_cast<Promise&>(*this));
            }

		private:
            [[noreturn]] static std::coroutine_handle<> default_unhandled_stopped(void*) noexcept{
                std::terminate();
            }
            std::coroutine_handle<> this_continuation = {};
            std::coroutine_handle<> (*stopped_handler)(void*) noexcept = &default_unhandled_stopped;
        };
    }
    using coroutine_detail::single_sender_value_type;
    using coroutine_detail::single_sender;
    using coroutine_detail::awaitable_sender;
    using coroutine_detail::sender_awaitable;
    using coroutine_detail::as_awaitable_t;
    inline constexpr as_awaitable_t as_awaitable;
    using coroutine_detail::with_awaitable_senders;

    namespace connect_detail{
        template<typename S, typename R>
        template<typename E>
		[[nodiscard]] decltype(auto) operation_state_task<S, R>::promise_type::await_transform(E&& e) noexcept {
			if constexpr (tag_invocable<as_awaitable_t, E&&, operation_state_task<S, R>::promise_type&>)
                tag_invoke(execution::as_awaitable, std::forward<E>(e), *this);
			else
                return std::forward<E>(e);
        }
    }

    // state

    template<typename S, typename R> requires sender_to<S, R>
    using state_t = decltype(execution::connect(std::declval<S>(), std::declval<R>()));

    // read

    namespace read_detail{
        template<typename Tag>
        struct read_sender{
            template<receiver R>
            struct operation{
                [[no_unique_address]] R receiver;

                friend void tag_invoke(start_t, operation& s) noexcept try {
					auto value = Tag{}(execution::get_env(s.receiver));
					execution::set_value(std::move(s.receiver), std::move(value));
				} catch (...){
					execution::set_error(std::move(s.receiver), std::current_exception());
				}
            };

            template<receiver R> requires std::invocable<Tag, env_of_t<R>> && receiver_of<R, std::invoke_result_t<Tag, env_of_t<R>>>
			[[nodiscard]] friend operation<std::remove_cvref_t<R>> tag_invoke(connect_t, read_sender, R&& r) noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&>){
				return { std::forward<R>(r) };
			}

			[[nodiscard]] friend empty_env tag_invoke(get_completion_signatures_t, read_sender, auto) noexcept{
                return {};
            };
            // why `get_completion_signatures` returns `empty_env`?
            // requirement from: https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2022/p2300r4.html#spec-execution.senders.read

            template<typename E>
            using completion_signatures = completion_signatures<set_value_t(std::invoke_result_t<Tag, E>), set_error_t(std::exception_ptr)>;

			template<typename Env> requires(!std::same_as<Env, no_env>) && std::invocable<Tag, Env>
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, read_sender, Env) noexcept(std::is_nothrow_default_constructible_v<completion_signatures<Env>>)->completion_signatures<Env>{
                return {};
            };
        };

        struct read_t{
            template<typename Tag>
            read_sender<Tag> operator()(Tag) const noexcept{ return {}; }
        };
    }
    using read_detail::read_t;
	inline constexpr read_t read{};
    template<typename Tag>
    using read_result_t = decltype(read(std::declval<Tag>()));
    template<typename Tag>
    concept nothrow_read_invocable = noexcept(read(std::declval<Tag>()));

    // get_scheduler, get_delegatee_scheduler, get_allocator, get_stop_token

    struct get_scheduler_t{
		template<typename R> requires (!std::same_as<std::remove_cvref_t<R>, no_env>) && nothrow_tag_invocable<get_scheduler_t, R&&> && scheduler<tag_invoke_result_t<get_scheduler_t, R&&>>
		[[nodiscard]] auto operator()(R&& r) const noexcept->tag_invoke_result_t<get_scheduler_t, R&&>{
			return tag_invoke(get_scheduler_t{}, std::as_const(r));
        }
		[[nodiscard]] auto operator()() const noexcept(nothrow_read_invocable<get_scheduler_t>){
			return execution::read(get_scheduler_t{});
        }
    };
	inline constexpr get_scheduler_t get_scheduler{};
	template<typename R>
    using get_scheduler_result_t = tag_invoke_result_t<get_scheduler_t, R>;
    template<typename R>
    concept nothrow_get_scheduler_invocable = noexcept(get_scheduler(std::declval<R>()));

    struct get_delegatee_scheduler_t{
		template<typename R> requires (!std::same_as<std::remove_cvref_t<R>, no_env>) && nothrow_tag_invocable<get_delegatee_scheduler_t, R&&> && scheduler<tag_invoke_result_t<get_delegatee_scheduler_t, R&&>>
		[[nodiscard]] auto operator()(R&& r) const noexcept->tag_invoke_result_t<get_delegatee_scheduler_t, R&&>{
			return tag_invoke(get_delegatee_scheduler_t{}, std::as_const(r));
        }
		[[nodiscard]] auto operator()() const noexcept(nothrow_read_invocable<get_delegatee_scheduler_t>){
			return execution::read(get_delegatee_scheduler_t{});
        }
    };
	inline constexpr get_delegatee_scheduler_t get_delegatee_scheduler{};
	template<typename R>
    using get_delegatee_scheduler_result_t = tag_invoke_result_t<get_delegatee_scheduler_t, R>;
    template<typename R>
    concept nothrow_get_delegatee_scheduler_invocable = noexcept(get_delegatee_scheduler(std::declval<R>()));

    struct get_allocator_t{
		template<typename R> requires (!std::same_as<std::remove_cvref_t<R>, no_env>) && nothrow_tag_invocable<get_allocator_t, R&&> && requires{ sizeof(std::allocator_traits<std::remove_cvref_t<tag_invoke_result_t<get_allocator_t, R&&>>>); }
		[[nodiscard]] auto operator()(R&& r) const noexcept(nothrow_tag_invocable<get_allocator_t, R&&>)->tag_invoke_result_t<get_allocator_t, R&&>{
			return tag_invoke(get_allocator_t{}, std::as_const(r));
        }
		[[nodiscard]] auto operator()() const noexcept(nothrow_read_invocable<get_allocator_t>){
			return execution::read(get_allocator_t{});
        }
    };
	inline constexpr get_allocator_t get_allocator{};
	template<typename R>
    using get_allocator_result_t = tag_invoke_result_t<get_allocator_t, R>;
    template<typename R>
    concept nothrow_get_allocator_invocable = noexcept(get_allocator(std::declval<R>()));

    struct get_stop_token_t{
		template<typename R> requires (!std::same_as<std::remove_cvref_t<R>, no_env>) && nothrow_tag_invocable<get_stop_token_t, R&&> && stoppable_token<tag_invoke_result_t<get_stop_token_t, R&&>>
		[[nodiscard]] auto operator()(R&& r) const noexcept->tag_invoke_result_t<get_stop_token_t, R&&> {
			return tag_invoke(get_stop_token_t{}, std::as_const(r));
		}
		template<typename R>
		[[nodiscard]] auto operator()(R&& r) const noexcept{
			return never_stop_token{};
		}
		[[nodiscard]] auto operator()() const noexcept(nothrow_read_invocable<get_stop_token_t>){
			return execution::read(get_stop_token_t{});
        }
	};
	inline constexpr get_stop_token_t get_stop_token{};
	template<typename T>
	using stop_token_of_t = std::remove_cvref_t<decltype(get_stop_token(std::declval<T>()))>;
    template<typename T>
    concept nothrow_get_stop_token_invocable = noexcept(get_stop_token(std::declval<T>()));

    // sender_adaptor_closure

    namespace sender_adaptor_closure_detail{
        template<typename F, typename S, typename Tuple>
        struct apply_nothrow_invocable_impl;
        template<typename F, typename S, typename... Args>
        struct apply_nothrow_invocable_impl<F, S, std::tuple<Args...>> : public std::bool_constant<std::is_nothrow_invocable_v<F, S, Args...>>{};
        template<typename F, typename S, typename Tuple>
		concept apply_nothrow_invocable = apply_nothrow_invocable_impl<F, S, std::remove_cvref_t<Tuple>>::value;
    }

    template<typename Derive>
	class sender_adaptor_closure{
        [[nodiscard]] decltype(auto) get() noexcept{
            return static_cast<Derive*>(this)->get();
        }
        [[nodiscard]] auto get_tag() const noexcept{
            return static_cast<Derive const*>(this)->get_tag();
        }
	public:
        template<sender S>
		[[nodiscard]] friend auto operator|(S&& s, sender_adaptor_closure&& c) noexcept(sender_adaptor_closure_detail::apply_nothrow_invocable<decltype(c.get_tag()), S&&, decltype(c.get())>){
            return std::apply([&s, &c]<typename... Args>(Args&&... args) noexcept(std::is_nothrow_invocable_v<decltype(c.get_tag()), S&&, Args&&...>){
                return c.get_tag()(std::forward<S>(s), std::forward<Args>(args)...);
            }, std::move(c.get()));
		}
		template<sender S>
		[[nodiscard]] friend auto operator|(S&& s, sender_adaptor_closure& c) noexcept(sender_adaptor_closure_detail::apply_nothrow_invocable<decltype(c.get_tag()), S&&, decltype(c.get())>){
            return std::apply([&s, &c]<typename... Args>(Args&&... args) noexcept(std::is_nothrow_invocable_v<decltype(c.get_tag()), S&&, Args&&...>){
                return c.get_tag()(std::forward<S>(s), std::forward<Args>(args)...);
            }, std::move(c.get()));
		}

        template<sender S>
        [[nodiscard]] auto operator()(S&& s) noexcept(noexcept(std::forward<S>(s) | *this)){
			return std::forward<S>(s) | *this;
        }
    };

    // is_multi_shot_sender, is_single_shot_sender

    namespace multi_shot_sender_detail{
        struct wildcard_receiver{
            template<ns_type_traits::any_of<set_value_t, set_error_t, set_stopped_t> Tag, typename... Args>
			friend void tag_invoke(Tag, wildcard_receiver&&, Args&&...) noexcept;
        };
    }

	template<typename S, typename E = no_env>
	concept is_multi_shot_sender = sender<S, E> && std::copy_constructible<S> && requires(std::remove_cvref_t<S>& s, multi_shot_sender_detail::wildcard_receiver&& r){ execution::connect(s, std::move(r)); };
    template<typename S, typename E = no_env>
    concept is_single_shot_sender = sender<S, E> && !is_multi_shot_sender<S, E>;

    // receiver_adaptor

    namespace receiver_adaptor_detail{
        using ns_type_traits::member_type;

        template<typename T, typename U> requires std::same_as<std::remove_cvref_t<T>, T>
        [[nodiscard]] member_type<U&&, T> c_style_cast(U&& u) noexcept{
			return (member_type<U&&, T>)std::forward<U>(u);
        }

        template<typename T>
		concept class_type = std::same_as<std::remove_cvref_t<T>, T> && std::is_class_v<T>;

        template<typename R, typename... Args>
        concept has_set_value = requires(R&& r, Args&&... args){
            std::forward<R>(r).set_value(std::forward<Args>(args)...);
        };
        template<typename R, typename E>
        concept has_set_error = requires(R&& r, E&& e){
            std::forward<R>(r).set_error(std::forward<E>(e));
        };
        template<typename R>
        concept has_set_stopped = requires(R&& r){
            std::forward<R>(r).set_stopped();
        };

        struct empty_receiver{
            template<typename... Args>
            friend void tag_invoke(set_value_t, empty_receiver&&, Args&&...) noexcept{}
            template<typename E>
            friend void tag_invoke(set_error_t, empty_receiver&&, E&&) noexcept{}
            friend void tag_invoke(set_stopped_t, empty_receiver&&) noexcept{}
        };

        template<class_type Derived, receiver Base = empty_receiver>
		class receiver_adaptor{
			static constexpr bool has_base = !std::same_as<std::remove_cvref_t<Base>, empty_receiver>;
            template<typename Self> requires std::same_as<std::remove_cvref_t<Self>, receiver_adaptor>
            [[nodiscard]] static constexpr decltype(auto) get_base(Self&& self) noexcept{
                if constexpr (has_base)
                    return c_style_cast<receiver_adaptor>(std::forward<Self>(self)).base();
				else
                    return std::forward<Self>(self).base();
            }
            template<typename T>
            using base_type = decltype(get_base(std::declval<T>()));
            friend Derived;

		public:
            receiver_adaptor() = default;
            template<typename B> requires has_base && std::constructible_from<Base, B>
            explicit receiver_adaptor(B&& base) noexcept(std::is_nothrow_constructible_v<Base, B&&>) :base_receiver(std::forward<B>(base)){}

		private:
            using set_value = set_value_t;
            using set_error = set_error_t;
            using set_stopped = set_stopped_t;

            
            [[nodiscard]] Base& base() & noexcept requires has_base{
                return base_receiver;
            }
            [[nodiscard]] Base const& base() const& noexcept requires has_base{
                return base_receiver;
            }
            [[nodiscard]] Base&& base() && noexcept requires has_base{
				return std::move(base_receiver);
            }
            [[nodiscard]] Base const&& base() const&& noexcept requires has_base{
				return std::move(base_receiver);
            }

        public:
            template<typename D = Derived, typename... Args> requires has_set_value<D, Args&&...>
            friend void tag_invoke(set_value_t, Derived&& self, Args&&... args) noexcept(noexcept(std::move(self).set_value(std::forward<Args>(args)...))){
                return std::move(self).set_value(std::forward<Args>(args)...);
            }
            template<typename D = Derived, typename... Args> requires (!has_set_value<D, Args&&...>) && requires{ typename D::set_value; } && receiver_of<base_type<D>, Args&&...>
            friend void tag_invoke(set_value_t, Derived&& self, Args&&... args) noexcept(nothrow_set_value_invocable<base_type<Derived&&>, Args&&...>){
				execution::set_value(get_base(std::move(self)), std::forward<Args>(args)...);
            }
            template<typename E, typename D = Derived> requires has_set_error<D, E&&>
            friend void tag_invoke(set_error_t, Derived&& self, E&& e) noexcept{
                return std::move(self).set_error(std::forward<E>(e));
            }
            template<typename E, typename D = Derived> requires (!has_set_error<D, E&&>) && requires{ typename D::set_error; } && receiver<base_type<D>, E>
            friend void tag_invoke(set_error_t, Derived&& self, E&& e) noexcept{
                execution::set_error(get_base(std::move(self)), std::forward<E>(e));
            }
            template<typename D = Derived> requires has_set_stopped<D>
            friend void tag_invoke(set_stopped_t, Derived&& self) noexcept{
                std::move(self).set_stopped();
            }
            template<typename D = Derived> requires (!has_set_stopped<D>) && requires{ typename D::set_stopped; }
            friend void tag_invoke(set_stopped_t, Derived&& self) noexcept{
                execution::set_stopped(get_base(std::move(self)));
            }

            template<typename Tag, typename D = Derived, typename... Args> requires (execution::forwarding_receiver_query(Tag{})) && std::invocable<Tag, base_type<D const&>, Args&&...>
            friend decltype(auto) tag_invoke(Tag tag, Derived const& self, Args&&... args) noexcept(std::is_nothrow_invocable_v<Tag, base_type<D const&>, Args&&...>){
                return tag(get_base(self), std::forward<Args>(args)...);
            }

		private:
            [[no_unique_address]] Base base_receiver;
        };
    }
    using receiver_adaptor_detail::receiver_adaptor;

    // constructive_receiver

    namespace constructive_receiver_detail{
        template<typename SV, typename SE, typename SS>
        struct constructive_receiver{
            [[no_unique_address]] SV sv;
            [[no_unique_address]] SE se;
            [[no_unique_address]] SS ss;

            template<typename... Args> requires std::invocable<SV&&, Args&&...>
            friend void tag_invoke(set_value_t, constructive_receiver&& r, Args&&... args) noexcept(std::is_nothrow_invocable_v<SV&&, Args&&...>){
                std::move(r.sv)(std::forward<Args>(args)...);
            }
            template<typename E> requires std::invocable<SE&&, E&&>
            friend void tag_invoke(set_error_t, constructive_receiver&& r, E&& e) noexcept(std::is_nothrow_invocable_v<SE&&, E&&>){
                std::move(r.se)(std::forward<E>(e));
            }
            friend void tag_invoke(set_stopped_t, constructive_receiver&& r) noexcept(std::is_nothrow_invocable_v<SS&&>) requires(std::invocable<SS&&>){
                std::move(r.ss)();
            }
        };
        template<typename SV, typename SE, typename SS>
		constructive_receiver(SV&&, SE&&, SS&&)->constructive_receiver<std::remove_cvref_t<SV>, std::remove_cvref_t<SE>, std::remove_cvref_t<SS>>;
    }
    using constructive_receiver_detail::constructive_receiver;
}

namespace this_thread{

    // execute_may_block_caller

	struct execute_may_block_caller_t{
		template<execution::scheduler Sch> requires execution::nothrow_tag_invocable<execute_may_block_caller_t, Sch> && std::is_same_v<bool, execution::tag_invoke_result_t<execute_may_block_caller_t, Sch>>
	    [[nodiscard]] bool operator()(Sch&& sch) const noexcept{
			return execution::tag_invoke(execute_may_block_caller_t{}, std::forward<Sch>(sch));
		}
		template<execution::scheduler Sch>
		[[nodiscard]] bool operator()(Sch&& sch) const noexcept{
			return true;
		}
	};
	inline constexpr execute_may_block_caller_t execute_may_block_caller{};
}

namespace std{
    template<typename... Ops>
	struct tuple_size<execution::connect_detail::operation_state_tuple_impl<Ops...>> : public tuple_size<tuple<Ops...>>{};

    template<size_t I, typename... Ops>
	struct tuple_element<I, execution::connect_detail::operation_state_tuple_impl<Ops...>> : public tuple_element<I, tuple<Ops...>>{};
}