#pragma once
#include <optional>
#include "then.hpp"

#define EXECUTION_LET_OPT_SAVE_ARGS false
#define EXECUTION_LET_OPT_SAVE_OP_STATE true


namespace execution{
	namespace let_detail{
        using namespace ns_type_list;
        using ns_type_traits::member_type;
        using ns_type_traits::movable_value;
        using ns_utility::monostate_variant;
        
        template<typename F, typename... Vs>
        concept f_return_sender = std::invocable<F, Vs...> && sender<std::invoke_result_t<F, Vs...>>;

        template<typename F, typename R, typename... Vs>
        concept f_return_sender_to = f_return_sender<F, Vs...> && sender_to<std::invoke_result_t<F, Vs...>, R>;

        template<typename Op, typename R, typename F, typename Tag> requires ns_type_traits::any_of<Tag, set_value_t, set_error_t, set_stopped_t>
        struct receiver_impl{
            [[no_unique_address]] Op* op;

            [[nodiscard]] R& out_receiver() noexcept{ return op->out_receiver; }
			[[nodiscard]] F& func() noexcept{ return op->func; }
            
            template<typename... Vs> requires f_return_sender_to<F, R, Vs&&...>
            static void invoke_this(Tag, receiver_impl&& r, Vs&&... vs) noexcept{
                try {
#if EXECUTION_LET_OPT_SAVE_ARGS
					// why I have to save the decayed-copies?
					// requirement from: https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2022/p2300r4.html#spec-execution.senders.adapt.let_value
					emplace_args<decayed_tuple<Vs...>>(*r.op, std::forward<Vs>(vs)...);
					emplace_op(*r.op, std::apply(std::move(r.func()), std::get<decayed_tuple<Vs...>>(r.op->data)), std::move(r.out_receiver()));
					start_op(*r.op);
#else
					emplace_op(*r.op, std::invoke(std::move(r.func()), std::forward<Vs>(vs)...), std::move(r.out_receiver()));
					start_op(*r.op);
#endif
                } catch (...){
                    execution::set_error(std::move(r.out_receiver()), std::current_exception());
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
                    execution::set_error(std::move(r.out_receiver()), e);
            }

            template<typename Tag2, typename... Vs> requires (!std::same_as<Tag, Tag2>) && ns_type_traits::any_of<Tag2, set_value_t, set_error_t, set_stopped_t>
			friend void tag_invoke(Tag2 tag, receiver_impl&& r, Vs&&... vs) noexcept(std::is_nothrow_invocable_v<Tag2, R&&, Vs&&...>){
				tag(std::move(r.out_receiver()), std::forward<Vs>(vs)...);
            }
		};

        template<sender S, movable_value F>
        struct let_value_sender_impl{
            [[no_unique_address]] S pre_sender;
            [[no_unique_address]] F func;
        private:
#if EXECUTION_LET_OPT_SAVE_ARGS
            template<typename... Ts>
            using result = std::invoke_result_t<F, std::remove_cvref_t<Ts>&...>;
#else
            template<typename... Ts>
            using result = std::invoke_result_t<F, Ts...>;
#endif
            template<typename E>
			struct completion_signatures_of_E{
				using sender_lists = value_types_of_t<S, E, result, type_list>;
                template<typename S2>
				struct is_sender : public std::bool_constant<sender<S2, E>>{};

                template<typename SL>
                struct all_sender : public std::type_identity<dependent_completion_signatures<E>>{};
                template<>
                struct all_sender<type_list<>>{
                    template<template<typename...>typename Tuple, template<typename...>typename Variant>
                    using value_types = Variant<>;
                    template<template<typename...>typename Variant>
                    using error_types = type_list_apply_t<include_or_add_t<error_types_of_t<S, E, type_list>, std::exception_ptr>, Variant>;
					static constexpr bool sends_stopped = completion_signatures_of_t<S, E>::sends_stopped;

                    using type = completion_signatures_to_list_t<value_types, error_types, sends_stopped>;
                };
                template<typename... S3s>
                struct all_sender<type_list<S3s...>>{
                    using type_list_from_S3s = include_or_merge_t<value_types_of_t<S3s, E, type_list, type_list>...>;
                    template<template<typename...>typename Tuple, template<typename...>typename Variant>
                    using value_types = type_list_deep_apply_t<type_list_from_S3s, Variant, Tuple>;

                    template<template<typename...>typename Variant>
                    using error_types = type_list_apply_t<include_or_add_t<include_or_merge_t<error_types_of_t<S, E, type_list>, error_types_of_t<S3s, E, type_list>...>, std::exception_ptr>, Variant>;

			        static constexpr bool sends_stopped = (completion_signatures_of_t<S, E>::sends_stopped || ... || completion_signatures_of_t<S3s, E>::sends_stopped);

                    using type = completion_signatures_to_list_t<value_types, error_types, sends_stopped>;
                };

                static_assert(type_list_all_of_v<sender_lists, is_sender>);
                using completion_signatures = typename all_sender<sender_lists>::type;
            };
            template<typename E, typename = void>
            struct completion_signatures_SFINAE : public std::type_identity<dependent_completion_signatures<E>>{};
            template<typename E>
            struct completion_signatures_SFINAE<E, std::void_t<completion_signatures_of_E<E>>> : public std::type_identity<typename completion_signatures_of_E<E>::completion_signatures>{};
            template<typename E>
            using completion_signatures_SFINAE_t = typename completion_signatures_SFINAE<E>::type;
            template<typename R>
            struct connect_with{
                template<typename... Ts>
                using result_apply = connect_result_t<result<Ts...>, R>;
            };
            template<typename... Ops>
            using operation_state_variant_t = decltype(execution::connect_into_variant.empty<Ops...>());
            template<typename R>
            using connect_result_variant = type_list_apply_t<value_types_of_t<S, env_of_t<R>, connect_with<R>::template result_apply, type_list>, operation_state_variant_t>;
        public:

            template<receiver R>
            struct operation{
				[[no_unique_address]] R out_receiver;
				[[no_unique_address]] F func;
                
				friend void tag_invoke(start_t, operation& self) noexcept{
					execution::start(self.op1);
				}

#if EXECUTION_LET_OPT_SAVE_ARGS
                template<typename Elem, typename... Args>
                friend void emplace_args(operation& op, Args&&... args) noexcept(std::is_nothrow_constructible_v<Elem, Args&&...>){
                    op.data.template emplace<Elem>(std::forward<Args>(args)...);
				}
#endif
                template<typename S2, typename R2>
                friend void emplace_op(operation& op, S2&& s, R2&& r) noexcept{
                    execution::connect_into_variant(op.op2, std::forward<S2>(s), std::forward<R2>(r));
				}
                friend void start_op(operation& op) noexcept{
                    visit(ns_type_traits::overload_t{
                        [](std::monostate) noexcept{},
                        []<typename Op>(Op& op2) noexcept{ execution::start(op2); },
                    }, op.op2);
                }

                using receiver_t = receiver_impl<operation, R, F, set_value_t>;

				template<sender S2, typename F2, receiver R2>
				operation(S2&& s, F2&& f, R2&& r) noexcept(std::is_nothrow_constructible_v<F, F2> && std::is_nothrow_constructible_v<R, R2&&> && nothrow_connect_invocable<S2&&, receiver_t>)
                    :out_receiver(std::forward<R2>(r)), func(std::forward<F2>(f)), op1{ execution::connect(std::forward<S2>(s), receiver_t{ this }) }{} 

				operation(operation const&) = delete;
				operation(operation&&) = delete;
				operation& operator=(operation const&) = delete;
				operation& operator=(operation&&) = delete;

                [[no_unique_address]] connect_result_t<S, receiver_t> op1;
				[[no_unique_address]] connect_result_variant<R> op2;

#if EXECUTION_LET_OPT_SAVE_ARGS
				[[no_unique_address]] value_types_of_t<S, env_of_t<R>, std::tuple, monostate_variant> data;
#endif
            };

            template<typename Self, receiver R> requires std::is_same_v<std::remove_cvref_t<Self>, let_value_sender_impl> && sender_to<member_type<Self, S>, typename operation<std::remove_cvref_t<R>>::receiver_t>
			[[nodiscard]] friend auto tag_invoke(connect_t, Self&& self, R&& out_r) noexcept(std::is_nothrow_constructible_v<S, member_type<Self&&, S>> && std::is_nothrow_constructible_v<F, member_type<Self&&, F>> && std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&>){
				return operation<std::remove_cvref_t<R>>{ std::forward<Self>(self).pre_sender, std::forward<Self>(self).func, std::move(out_r) };
			}
            template<typename Self, typename E> requires std::is_same_v<std::remove_cvref_t<Self>, let_value_sender_impl> && sender<S, E>
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<completion_signatures_SFINAE_t<E>>)->completion_signatures_SFINAE_t<E>{
				return completion_signatures_SFINAE_t<E>{};
            }
            template<typename Self, typename E> requires std::is_same_v<std::remove_cvref_t<Self>, let_value_sender_impl> && (!sender<S, E>)
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<dependent_completion_signatures<E>>)->dependent_completion_signatures<E>{
				return dependent_completion_signatures<E>{};
            }

			/*template<typename Self, typename CPO> requires std::is_same_v<std::remove_cvref_t<Self>, let_value_sender_impl> && std::invocable<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>
			[[nodiscard]] friend auto tag_invoke(get_completion_scheduler_t<CPO> tag, Self&& s) noexcept(std::is_nothrow_invocable_v<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>){
				return tag(std::forward<Self>(s).pre_sender);
			}*/
        };
        template<sender S, movable_value F>
        struct let_error_sender_impl{
            [[no_unique_address]] S pre_sender;
            [[no_unique_address]] F func;
        private:
#if EXECUTION_LET_OPT_SAVE_ARGS
            template<typename E>
            using result = std::invoke_result_t<F, std::remove_cvref_t<E>&>;
            template<typename... Ts>
            using result_type_list = type_list<std::invoke_result_t<F, std::remove_cvref_t<Ts>&>...>;
#else
            template<typename E>
            using result = std::invoke_result_t<F, E>;
            template<typename... Ts>
            using result_type_list = type_list<std::invoke_result_t<F, Ts>...>;
#endif
            template<typename E>
			struct completion_signatures_of_E{
				using sender_lists = error_types_of_t<S, E, result_type_list>;
                template<typename S2>
				struct is_sender : public std::bool_constant<sender<S2, E>>{};

                template<typename SL>
                struct all_sender : public std::type_identity<dependent_completion_signatures<E>>{};
                template<>
                struct all_sender<type_list<>>{
                    template<template<typename...>typename Tuple, template<typename...>typename Variant>
                    using value_types = value_types_of_t<S, E, Tuple, Variant>;
                    template<template<typename...>typename Variant>
                    using error_types = Variant<std::exception_ptr>;
					static constexpr bool sends_stopped = completion_signatures_of_t<S, E>::sends_stopped;

                    using type = completion_signatures_to_list_t<value_types, error_types, sends_stopped>;
                };
                template<typename... S3s>
                struct all_sender<type_list<S3s...>>{
                    using type_list_from_S3s = include_or_merge_t<value_types_of_t<S, E, type_list, type_list>, value_types_of_t<S3s, E, type_list, type_list>...>;
                    template<template<typename...>typename Tuple, template<typename...>typename Variant>
                    using value_types = type_list_deep_apply_t<type_list_from_S3s, Variant, Tuple>;

                    template<template<typename...>typename Variant>
                    using error_types = type_list_apply_t<include_or_add_t<include_or_merge_t<error_types_of_t<S3s, E, type_list>...>, std::exception_ptr>, Variant>;

			        static constexpr bool sends_stopped = (completion_signatures_of_t<S, E>::sends_stopped || ... || completion_signatures_of_t<S3s, E>::sends_stopped);

                    using type = completion_signatures_to_list_t<value_types, error_types, sends_stopped>;
                };
                
                static_assert(type_list_all_of_v<sender_lists, is_sender>);
                using completion_signatures = typename all_sender<sender_lists>::type;
            };
            template<typename E, typename = void>
            struct completion_signatures_SFINAE : public std::type_identity<dependent_completion_signatures<E>>{};
            template<typename E>
            struct completion_signatures_SFINAE<E, std::void_t<completion_signatures_of_E<E>>> : public std::type_identity<typename completion_signatures_of_E<E>::completion_signatures>{};
            template<typename E>
            using completion_signatures_SFINAE_t = typename completion_signatures_SFINAE<E>::type;
            template<typename R>
            struct connect_with{
                template<typename... Ts>
                using result_apply = type_list<connect_result_t<result<Ts>, R>...>;
            };
            template<typename... Ops>
            using operation_state_variant_t = decltype(execution::connect_into_variant.empty<Ops...>());
            template<typename R>
            using connect_result_variant = type_list_apply_t<error_types_of_t<S, env_of_t<R>, connect_with<R>::template result_apply>, operation_state_variant_t>;
        public:

            template<receiver R>
            struct operation{
				[[no_unique_address]] R out_receiver;
				[[no_unique_address]] F func;
                
				friend void tag_invoke(start_t, operation& self) noexcept{
					execution::start(self.op1);
				}

#if EXECUTION_LET_OPT_SAVE_ARGS
                template<typename Elem, typename... Args>
                friend void emplace_args(operation& op, Args&&... args) noexcept(std::is_nothrow_constructible_v<Elem, Args&&...>){
                    op.data.template emplace<Elem>(std::forward<Args>(args)...);
				}
#endif
                template<typename S2, typename R2>
                friend void emplace_op(operation& op, S2&& s, R2&& r) noexcept{
                    execution::connect_into_variant(op.op2, std::forward<S2>(s), std::forward<R2>(r));
				}
                friend void start_op(operation& op) noexcept{
                    visit(ns_type_traits::overload_t{
                        [](std::monostate) noexcept{},
                        []<typename Op>(Op& op2) noexcept{ execution::start(op2); },
                    }, op.op2);
                }

                using receiver_t = receiver_impl<operation, R, F, set_error_t>;

				template<sender S2, typename F2, receiver R2>
				operation(S2&& s, F2&& f, R2&& r) noexcept(std::is_nothrow_constructible_v<F, F2> && std::is_nothrow_constructible_v<R, R2&&> && nothrow_connect_invocable<S2&&, receiver_t>)
                    :out_receiver(std::forward<R2>(r)), func(std::forward<F2>(f)), op1{ execution::connect(std::forward<S2>(s), receiver_t{ this }) }{} 

				operation(operation const&) = delete;
				operation(operation&&) = delete;
				operation& operator=(operation const&) = delete;
				operation& operator=(operation&&) = delete;

                [[no_unique_address]] connect_result_t<S, receiver_t> op1;
				[[no_unique_address]] connect_result_variant<R> op2;

#if EXECUTION_LET_OPT_SAVE_ARGS
				[[no_unique_address]] error_types_of_t<S, env_of_t<R>, monostate_variant> data;
#endif
            };

            template<typename Self, receiver R> requires std::is_same_v<std::remove_cvref_t<Self>, let_error_sender_impl> && sender_to<member_type<Self, S>, typename operation<std::remove_cvref_t<R>>::receiver_t>
			[[nodiscard]] friend auto tag_invoke(connect_t, Self&& self, R&& out_r) noexcept(std::is_nothrow_constructible_v<S, member_type<Self&&, S>> && std::is_nothrow_constructible_v<F, member_type<Self&&, F>> && std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&>){
				return operation<std::remove_cvref_t<R>>{ std::forward<Self>(self).pre_sender, std::forward<Self>(self).func, std::move(out_r) };
			}
            template<typename Self, typename E> requires std::is_same_v<std::remove_cvref_t<Self>, let_error_sender_impl> && sender<S, E>
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<completion_signatures_SFINAE_t<E>>)->completion_signatures_SFINAE_t<E>{
				return completion_signatures_SFINAE_t<E>{};
            }
            template<typename Self, typename E> requires std::is_same_v<std::remove_cvref_t<Self>, let_error_sender_impl> && (!sender<S, E>)
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<dependent_completion_signatures<E>>)->dependent_completion_signatures<E>{
				return dependent_completion_signatures<E>{};
            }

			/*template<typename Self, typename CPO> requires std::is_same_v<std::remove_cvref_t<Self>, let_error_sender_impl> && std::invocable<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>
			[[nodiscard]] friend auto tag_invoke(get_completion_scheduler_t<CPO> tag, Self&& s) noexcept(std::is_nothrow_invocable_v<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>){
				return tag(std::forward<Self>(s).pre_sender);
			}*/
        };
        template<sender S, movable_value F>
        struct let_stopped_sender_impl{
            [[no_unique_address]] S pre_sender;
            [[no_unique_address]] F func;
        private:
            using FS = std::invoke_result_t<F>;
            template<typename E>
			struct completion_signatures_of_E{
                template<typename S2>
				struct is_sender : public std::bool_constant<sender<S2, E>>{};

                template<typename S3>
                struct all_sender{
                    using type_list_from_S3 = include_or_merge_t<value_types_of_t<S, E, type_list, type_list>, value_types_of_t<S3, E, type_list, type_list>>;
                    template<template<typename...>typename Tuple, template<typename...>typename Variant>
                    using value_types = type_list_deep_apply_t<type_list_from_S3, Variant, Tuple>;
                    
                    template<template<typename...>typename Variant>
                    using error_types = type_list_apply_t<include_or_add_t<include_or_merge_t<error_types_of_t<S, E, type_list>, error_types_of_t<S3, E, type_list>>, std::exception_ptr>, Variant>;

			        static constexpr bool sends_stopped = completion_signatures_of_t<S3, E>::sends_stopped;

                    using type = completion_signatures_to_list_t<value_types, error_types, sends_stopped>;
                };

                static_assert(is_sender<FS>::value);
                using completion_signatures = typename all_sender<FS>::type;
            };
            template<typename E, typename = void>
            struct completion_signatures_SFINAE : public std::type_identity<dependent_completion_signatures<E>>{};
            template<typename E>
            struct completion_signatures_SFINAE<E, std::void_t<completion_signatures_of_E<E>>> : public std::type_identity<typename completion_signatures_of_E<E>::completion_signatures>{};
            template<typename E>
            using completion_signatures_SFINAE_t = typename completion_signatures_SFINAE<E>::type;
        public:

            template<receiver R>
            struct operation{
				[[no_unique_address]] R out_receiver;
				[[no_unique_address]] F func;
                
				friend void tag_invoke(start_t, operation& self) noexcept{
					execution::start(self.op1);
				}

#if EXECUTION_LET_OPT_SAVE_ARGS
                template<typename Elem, typename... Args>
                friend void emplace_args(operation& op, Args&&... args) noexcept(std::is_nothrow_constructible_v<Elem, Args&&...>){
                    op.data.template emplace<Elem>(std::forward<Args>(args)...);
				}
#endif
                template<typename S2, typename R2>
                friend void emplace_op(operation& op, S2&& s, R2&& r) noexcept{
                    execution::connect_into_optional(op.op2, std::forward<S2>(s), std::forward<R2>(r));
				}
                friend void start_op(operation& op) noexcept{
                    if (op.op2)
                        execution::start(*op.op2);
                }

                using receiver_t = receiver_impl<operation, R, F, set_stopped_t>;

				template<sender S2, typename F2, receiver R2>
				operation(S2&& s, F2&& f, R2&& r) noexcept(std::is_nothrow_constructible_v<F, F2> && std::is_nothrow_constructible_v<R, R2&&> && nothrow_connect_invocable<S2&&, receiver_t>)
                    :out_receiver(std::forward<R2>(r)), func(std::forward<F2>(f)), op1{ execution::connect(std::forward<S2>(s), receiver_t{ this }) }{} 

				operation(operation const&) = delete;
				operation(operation&&) = delete;
				operation& operator=(operation const&) = delete;
				operation& operator=(operation&&) = delete;

                [[no_unique_address]] connect_result_t<S, receiver_t> op1;
				[[no_unique_address]] connect_into_optional_result_t<std::invoke_result_t<F>, R> op2;

#if EXECUTION_LET_OPT_SAVE_ARGS
				[[no_unique_address]] monostate_variant<std::tuple<>> data;
#endif
            };

            template<typename Self, receiver R> requires std::is_same_v<std::remove_cvref_t<Self>, let_stopped_sender_impl> && sender_to<member_type<Self, S>, typename operation<std::remove_cvref_t<R>>::receiver_t>
			[[nodiscard]] friend auto tag_invoke(connect_t, Self&& self, R&& out_r) noexcept(std::is_nothrow_constructible_v<S, member_type<Self&&, S>> && std::is_nothrow_constructible_v<F, member_type<Self&&, F>> && std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&>){
				return operation<std::remove_cvref_t<R>>{ std::forward<Self>(self).pre_sender, std::forward<Self>(self).func, std::move(out_r) };
			}
            template<typename Self, typename E> requires std::is_same_v<std::remove_cvref_t<Self>, let_stopped_sender_impl> && sender<S, E>
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<completion_signatures_SFINAE_t<E>>)->completion_signatures_SFINAE_t<E>{
				return completion_signatures_SFINAE_t<E>{};
            }
            template<typename Self, typename E> requires std::is_same_v<std::remove_cvref_t<Self>, let_stopped_sender_impl> && (!sender<S, E>)
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<dependent_completion_signatures<E>>)->dependent_completion_signatures<E>{
				return dependent_completion_signatures<E>{};
            }

			/*template<typename Self, typename CPO> requires std::is_same_v<std::remove_cvref_t<Self>, let_stopped_sender_impl> && std::invocable<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>
			[[nodiscard]] friend auto tag_invoke(get_completion_scheduler_t<CPO> tag, Self&& s) noexcept(std::is_nothrow_invocable_v<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>){
				return tag(std::forward<Self>(s).pre_sender);
			}*/
        };

        template<sender S, movable_value F>
        using let_value_sender = let_value_sender_impl<std::remove_cvref_t<S>, std::remove_cvref_t<F>>;
        template<sender S, movable_value F>
        using let_error_sender = let_error_sender_impl<std::remove_cvref_t<S>, std::remove_cvref_t<F>>;
        template<sender S, movable_value F>
        using let_stopped_sender = let_stopped_sender_impl<std::remove_cvref_t<S>, std::remove_cvref_t<F>>;
	}

    struct let_value_t{
		template<ns_type_traits::movable_value F>
        [[nodiscard]] inline auto operator()(F &&f) const noexcept;

        template<sender S, ns_type_traits::movable_value F> requires tag_invocable_with_completion_scheduler<let_value_t, set_value_t, S&&, F&&> && sender<tag_invoke_result_t<let_value_t, completion_scheduler_for<S&&, set_value_t>, S&&, F&&>>
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(nothrow_tag_invocable<let_value_t, completion_scheduler_for<S&&, set_value_t>, S&&, F&&>)->tag_invoke_result_t<let_value_t, completion_scheduler_for<S&&, set_value_t>, S&&, F&&>{
			auto cs = get_completion_scheduler<set_value_t>(s); // guarantee the order of evaluation
            return tag_invoke(let_value_t{}, std::move(cs), std::forward<S>(s), std::forward<F>(f));
        }

        template<sender S, ns_type_traits::movable_value F> requires tag_invocable<let_value_t, S&&, F&&> && (!tag_invocable_with_completion_scheduler<let_value_t, set_value_t, S&&, F&&>) && sender<tag_invoke_result_t<let_value_t, S&&, F&&>>
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(nothrow_tag_invocable<let_value_t, S&&, F&&>)->tag_invoke_result_t<let_value_t, S&&, F&&>{
            return tag_invoke(let_value_t{}, std::forward<S>(s), std::forward<F>(f));
        }

        template<sender S, ns_type_traits::movable_value F> requires (!tag_invocable<let_value_t, S&&, F&&>) && (!tag_invocable_with_completion_scheduler<let_value_t, set_value_t, S&&, F&&>)
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<S>, S&&> && std::is_nothrow_constructible_v<std::remove_cvref_t<F>, F&&>){
            return let_detail::let_value_sender<S, F>{ std::forward<S>(s), std::forward<F>(f) };
        }
    };
    inline constexpr let_value_t let_value{};
	template<sender S, ns_type_traits::movable_value F>
    using let_value_result_t = decltype(let_value(std::declval<S>(), std::declval<F>()));
    template<typename S, typename F>
    concept nothrow_let_value_invocable = sender<S> && ns_type_traits::movable_value<F> && noexcept(let_value(std::declval<S>(), std::declval<F>()));

    struct let_error_t{
		template<ns_type_traits::movable_value F>
        [[nodiscard]] inline auto operator()(F &&f) const noexcept;

        template<sender S, ns_type_traits::movable_value F> requires tag_invocable_with_completion_scheduler<let_error_t, set_error_t, S&&, F&&> && sender<tag_invoke_result_t<let_error_t, completion_scheduler_for<S&&, set_error_t>, S&&, F&&>>
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(nothrow_tag_invocable<let_error_t, completion_scheduler_for<S&&, set_error_t>, S&&, F&&>)->tag_invoke_result_t<let_error_t, completion_scheduler_for<S&&, set_error_t>, S&&, F&&>{
			auto cs = get_completion_scheduler<set_error_t>(s); // guarantee the order of evaluation
            return tag_invoke(let_error_t{}, std::move(cs), std::forward<S>(s), std::forward<F>(f));
        }

        template<sender S, ns_type_traits::movable_value F> requires tag_invocable<let_error_t, S&&, F&&> && (!tag_invocable_with_completion_scheduler<let_error_t, set_error_t, S&&, F&&>) && sender<tag_invoke_result_t<let_error_t, S&&, F&&>>
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(nothrow_tag_invocable<let_error_t, S&&, F&&>)->tag_invoke_result_t<let_error_t, S&&, F&&>{
            return tag_invoke(let_error_t{}, std::forward<S>(s), std::forward<F>(f));
        }

        template<sender S, ns_type_traits::movable_value F> requires (!tag_invocable<let_error_t, S&&, F&&>) && (!tag_invocable_with_completion_scheduler<let_error_t, set_error_t, S&&, F&&>)
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<S>, S&&> && std::is_nothrow_constructible_v<std::remove_cvref_t<F>, F&&>){
            return let_detail::let_error_sender<S, F>{ std::forward<S>(s), std::forward<F>(f) };
        }
    };
    inline constexpr let_error_t let_error{};
	template<sender S, ns_type_traits::movable_value F>
    using let_error_result_t = decltype(let_error(std::declval<S>(), std::declval<F>()));
    template<typename S, typename F>
    concept nothrow_let_error_invocable = sender<S> && ns_type_traits::movable_value<F> && noexcept(let_error(std::declval<S>(), std::declval<F>()));

    struct let_stopped_t{
		template<ns_type_traits::movable_value F>
        [[nodiscard]] inline auto operator()(F &&f) const noexcept;

        template<sender S, ns_type_traits::movable_value F> requires tag_invocable_with_completion_scheduler<let_stopped_t, set_stopped_t, S&&, F&&> && sender<tag_invoke_result_t<let_stopped_t, completion_scheduler_for<S&&, set_stopped_t>, S&&, F&&>>
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(nothrow_tag_invocable<let_stopped_t, completion_scheduler_for<S&&, set_stopped_t>, S&&, F&&>)->tag_invoke_result_t<let_stopped_t, completion_scheduler_for<S&&, set_stopped_t>, S&&, F&&>{
			auto cs = get_completion_scheduler<set_stopped_t>(s); // guarantee the order of evaluation
            return tag_invoke(let_stopped_t{}, std::move(cs), std::forward<S>(s), std::forward<F>(f));
        }

        template<sender S, ns_type_traits::movable_value F> requires tag_invocable<let_stopped_t, S&&, F&&> && (!tag_invocable_with_completion_scheduler<let_stopped_t, set_stopped_t, S&&, F&&>) && sender<tag_invoke_result_t<let_stopped_t, S&&, F&&>>
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(nothrow_tag_invocable<let_stopped_t, S&&, F&&>)->tag_invoke_result_t<let_stopped_t, S&&, F&&>{
            return tag_invoke(let_stopped_t{}, std::forward<S>(s), std::forward<F>(f));
        }

        template<sender S, ns_type_traits::movable_value F> requires (!tag_invocable<let_stopped_t, S&&, F&&>) && (!tag_invocable_with_completion_scheduler<let_stopped_t, set_stopped_t, S&&, F&&>)
        [[nodiscard]] auto operator()(S&& s, F&& f) const noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<S>, S&&> && std::is_nothrow_constructible_v<std::remove_cvref_t<F>, F&&>){
            return let_detail::let_stopped_sender<S, F>{ std::forward<S>(s), std::forward<F>(f) };
        }
    };
    inline constexpr let_stopped_t let_stopped{};
	template<sender S, ns_type_traits::movable_value F>
    using let_stopped_result_t = decltype(let_stopped(std::declval<S>(), std::declval<F>()));
    template<typename S, typename F>
    concept nothrow_let_stopped_invocable = sender<S> && ns_type_traits::movable_value<F> && noexcept(let_stopped(std::declval<S>(), std::declval<F>()));

    namespace let_detail{
		template<movable_value F>
        struct let_value_closure_t : public sender_adaptor_closure<let_value_closure_t<F>>{
			[[no_unique_address]] std::tuple<F> data;

			[[nodiscard]] decltype(auto) get() noexcept{
				return data;
			}
			[[nodiscard]] auto get_tag() const noexcept{
				return let_value_t{};
			}
        };
        template<movable_value F>
        struct let_error_closure_t : public sender_adaptor_closure<let_error_closure_t<F>>{
			[[no_unique_address]] std::tuple<F> data;

			[[nodiscard]] decltype(auto) get() noexcept{
				return data;
			}
			[[nodiscard]] auto get_tag() const noexcept{
				return let_error_t{};
			}
        };
        template<movable_value F>
        struct let_stopped_closure_t : public sender_adaptor_closure<let_stopped_closure_t<F>>{
			[[no_unique_address]] std::tuple<F> data;

			[[nodiscard]] decltype(auto) get() noexcept{
				return data;
			}
			[[nodiscard]] auto get_tag() const noexcept{
				return let_stopped_t{};
			}
        };
    }

    template<ns_type_traits::movable_value F>
    [[nodiscard]] inline auto let_value_t::operator()(F&& f) const noexcept{
        return let_detail::let_value_closure_t<std::remove_cvref_t<F>>{ {}, { std::forward<F>(f) } };
    }
	template<ns_type_traits::movable_value F>
	[[nodiscard]] inline auto let_error_t::operator()(F&& f) const noexcept{
		return let_detail::let_error_closure_t<std::remove_cvref_t<F>>{ {}, { std::forward<F>(f) } };
	}
	template<ns_type_traits::movable_value F>
	[[nodiscard]] inline auto let_stopped_t::operator()(F&& f) const noexcept{
		return let_detail::let_stopped_closure_t<std::remove_cvref_t<F>>{ {}, { std::forward<F>(f) } };
	}
}