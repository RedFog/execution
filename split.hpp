#pragma once
#include <atomic>
#include <optional>
#include "execution.hpp"


#define EXECUTION_SPLIT_MULTI_SHOT_OPTIMIZATION false

namespace execution{
	namespace split_detail{
		using namespace ns_type_list;
        using ns_type_traits::member_type;
		using ns_type_traits::overload_t;
        using ns_type_traits::movable_value;
		using ns_utility::monostate_variant;

		struct stop_callback_func{
			stop_source& src;
			void operator()() noexcept{ src.request_stop(); }
		};

	    struct split_env{
			template<typename Self> requires std::same_as<std::remove_cvref_t<Self>, split_env>
		    [[nodiscard]] friend auto tag_invoke(get_stop_token_t, Self&& env) noexcept{
			    return std::forward<Self>(env).st;
		    }

		    stop_token st;
	    };

		template<typename ShWRef>
		struct receiver_impl{
			ShWRef sh;

			template<typename Tag, typename... Args>
			inline void tag_invoke_shared_state(Tag, Args&&... args);
			inline stop_token shared_state_get_stop_token() noexcept;

			template<typename... Args>
			friend void tag_invoke(set_value_t, receiver_impl&& r, Args&&... args) noexcept{
				try {
					r.tag_invoke_shared_state(set_value_t{}, std::forward<Args>(args)...);
				} catch (...){
					execution::set_error(std::move(r), std::current_exception());
				}
			}
			template<ns_type_traits::any_of<set_error_t, set_stopped_t> Tag, typename... Args>
			friend void tag_invoke(Tag, receiver_impl&& r, Args&&... args) noexcept{
				r.tag_invoke_shared_state(Tag{}, std::forward<Args>(args)...);
			}
			[[nodiscard]] friend split_env tag_invoke(get_env_t, receiver_impl r) noexcept{
				return split_env{ r.shared_state_get_stop_token() };
			}
		};

		template<typename Sh>
		using ref_t = std::shared_ptr<Sh>; // it's confirmed that an instance of split_sender can not be used by more than one thread.
		template<typename Sh>
		using wref_t = std::weak_ptr<Sh>;

		template<typename S, typename Data, typename OpBase>
		struct shared_state_impl{
			using receiver_t = receiver_impl<std::weak_ptr<shared_state_impl>>;
			using data_type = Data;
			using operation_base = OpBase;
			[[no_unique_address]] decltype(connect_into_optional.empty<S, receiver_t>()) op;
			stop_source src;
			data_type data;
			ns_utility::locked_pointer<operation_base> list;

			inline bool attach(operation_base*) noexcept;
			inline void detach(operation_base*) noexcept;
			inline void notify_all() noexcept;
		};

		template<sender<split_env> S>
		struct sender_impl{
		private:
			template<typename... Ts>
			using value_type_list = type_list<set_value_t, std::remove_cvref_t<Ts>...>;
			template<typename... Ts>
			using error_type_list = type_list<type_list<set_error_t, std::remove_cvref_t<Ts>>...>;
			using value_types_list = value_types_of_t<S, split_env, value_type_list, type_list>;
			using error_types_list = include_or_add_t<error_types_of_t<S, split_env, error_type_list>, type_list<set_error_t, std::exception_ptr>>;
			using data_type = type_list_deep_apply_t<type_list_concat_t<value_types_list, error_types_list, type_list<type_list<set_stopped_t>>>, monostate_variant, std::tuple>;
			template<typename T>
			struct is_same_as{
				template<typename Y>
				using apply = std::is_same<T, Y>;
			};
		public:
			struct operation_base;

			using shared_state = shared_state_impl<S, data_type, operation_base>;
			using shared_state_ref = ref_t<shared_state>;
			using shared_state_wref = wref_t<shared_state>;

			struct operation_base{
				operation_base(shared_state_ref sh) noexcept :sh(sh){}
				virtual ~operation_base() noexcept = default;

				virtual void exec() noexcept = 0;

				shared_state_ref sh;
				operation_base* prev;
				operation_base* next;
			};

			using receiver_t = typename shared_state::receiver_t;

			shared_state_ref sh;

			template<receiver R>
			struct operation final : public operation_base{
				using operation_base::sh;
				virtual void exec() noexcept{
					token.reset();
					std::visit(overload_t{
						[](std::monostate) noexcept{},
						[this]<typename Tuple>(Tuple&& tup) noexcept{
							std::apply([this]<typename Tag, typename... Args>(Tag tag, Args&&... args) noexcept{
								try {
									tag(std::move(out_receiver), std::forward<Args>(args)...);
								} catch (...){
									execution::set_error(std::move(out_receiver), std::current_exception());
								}
							}, std::forward<Tuple>(tup));
						}
					}, sh->data);
				}

				friend void tag_invoke(start_t, operation& op) noexcept{
					if (std::holds_alternative<std::monostate>(op.sh->data)){
						op.token.emplace(execution::get_stop_token(execution::get_env(op.out_receiver)), stop_callback_func{ op.sh->src });
						if (op.sh->src.stop_requested())
							execution::set_stopped(std::move(op.out_receiver));
						else if (op.sh->attach(&op))
							execution::start(*op.sh->op);
					} else
						op.exec();
				}

				template<typename R2>
				operation(R2&& r, shared_state_ref sh) noexcept(std::is_nothrow_constructible_v<R, R2&&>) :operation_base{ sh }, out_receiver(std::forward<R2>(r)){}
				virtual ~operation() = default;

				operation(operation const&) = delete;
				operation(operation&&) = delete;
				operation& operator=(operation const&) = delete;
				operation& operator=(operation&&) = delete;

				[[no_unique_address]] R out_receiver;
				std::optional<typename stop_token_of_t<env_of_t<R>>::template callback_type<stop_callback_func>> token;
			};

			template<typename Self, receiver R> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl> && sender_to<member_type<Self&&, S>, receiver_t>
			[[nodiscard]] friend auto tag_invoke(connect_t, Self&& self, R&& out_r) noexcept(std::is_nothrow_constructible_v<S, member_type<Self&&, S>> && std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&>){
				return operation<std::remove_cvref_t<R>>{ std::move(out_r), std::forward<Self>(self).sh };
			}

			template<typename S2> requires std::same_as<std::remove_cvref_t<S2>, S>
			sender_impl(S2&& s) :sh(std::make_shared<shared_state>()){
				execution::connect_into_optional(sh->op, std::forward<S2>(s), receiver_t{ sh });
			}

		private:
			template<typename... Ts>
			using lvalue_ref_type_list = type_list<std::remove_cvref_t<Ts>&...>;
            template<typename E>
            struct completion_signatures_of_E{
                using type_list_from_S = value_types_of_t<S, E, lvalue_ref_type_list, type_list>;

                template<template<typename...>typename Tuple, template<typename...>typename Variant>
                using value_types = type_list_deep_apply_t<type_list_from_S, Variant, Tuple>;

                template<template<typename...>typename Variant>
                using error_types = type_list_apply_t<include_or_add_t<error_types_of_t<S, E, lvalue_ref_type_list>, std::exception_ptr&>, Variant>;

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

			/*template<typename Self, typename CPO> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl> && std::invocable<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>
			[[nodiscard]] friend auto tag_invoke(get_completion_scheduler_t<CPO> tag, Self&& s) noexcept(std::is_nothrow_invocable_v<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>){
				return tag(std::forward<Self>(s).pre_sender);
			}*/
		};

		template<sender<split_env> S>
		using split_sender = sender_impl<std::remove_cvref_t<S>>;

		template<typename Sh>
		template<typename Tag, typename... Args>
		inline void receiver_impl<Sh>::tag_invoke_shared_state(Tag, Args&&... args){
			auto ssh = sh.lock();
			ssh->data.template emplace<decayed_tuple<Tag, Args...>>(Tag{}, std::forward<Args>(args)...);
			ssh->notify_all();
		}
		template<typename Sh>
		inline stop_token receiver_impl<Sh>::shared_state_get_stop_token() noexcept{
			auto ssh = sh.lock();
			return stop_token{ ssh->src.get_token() };
		}

		template<typename S, typename Data, typename OpBase>
		inline bool shared_state_impl<S, Data, OpBase>::attach(OpBase* ptr) noexcept{
			auto head = list.lock_and_load();
			bool ret = head;

			if (head)
				head->prev = ptr;
			ptr->next = head;

			list.store_and_unlock(ptr);
			return !ret; // returns if the original list is empty
		}
		template<typename S, typename Data, typename OpBase>
		inline void shared_state_impl<S, Data, OpBase>::detach(OpBase* ptr) noexcept{
			auto head = list.lock_and_load();

			if (head == ptr){
				auto next = ptr->next;
				ptr->next = nullptr;
				if (next)
					next->prev = nullptr;
				list.store_and_unlock(next);
			} else {
				auto next = ptr->next;
				ptr->next = nullptr;
				if (next)
					next->prev = ptr->prev;
				ptr->prev->next = next;
				list.store_and_unlock(head);
			}
		}
		template<typename S, typename Data, typename OpBase>
		inline void shared_state_impl<S, Data, OpBase>::notify_all() noexcept{
			while (true) {
				auto head = list.lock_and_load();
				if (!head) {
					list.store_and_unlock(nullptr);
					return;
				}

				auto next = head->next;
				head->next = nullptr;
				if (next)
					next->prev = nullptr;

				list.store_and_unlock(next);
				head->exec();
			}
		}
	}

	struct split_t{
		[[nodiscard]] inline auto operator()() const noexcept;

		template<sender<split_detail::split_env> S> requires tag_invocable_with_completion_scheduler<split_t, set_value_t, S&&> && sender<tag_invoke_result_t<split_t, completion_scheduler_for<S&&, set_value_t>, S&&>>
        [[nodiscard]] auto operator()(S&& s) const noexcept(nothrow_tag_invocable<split_t, completion_scheduler_for<S&&, set_value_t>, S&&>)->tag_invoke_result_t<split_t, completion_scheduler_for<S&&, set_value_t>, S&&>{
			auto cs = get_completion_scheduler<set_value_t>(s); // guarantee the order of evaluation
            return tag_invoke(split_t{}, std::move(cs), std::forward<S>(s));
        }

        template<sender<split_detail::split_env> S> requires tag_invocable<split_t, S&&> && (!tag_invocable_with_completion_scheduler<split_t, set_value_t, S&&>) && sender<tag_invoke_result_t<split_t, S&&>>
        [[nodiscard]] auto operator()(S&& s) const noexcept(nothrow_tag_invocable<split_t, S&&>)->tag_invoke_result_t<split_t, S&&>{
            return tag_invoke(split_t{}, std::forward<S>(s));
        }

        template<sender<split_detail::split_env> S> requires (!tag_invocable<split_t, S&&>) && (!tag_invocable_with_completion_scheduler<split_t, set_value_t, S&&>)
        [[nodiscard]] auto operator()(S&& s) const noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<S>, S&&>){
#if EXECUTION_SPLIT_MULTI_SHOT_OPTIMIZATION
			// should I check if S is multi-shot and then just return it?
			// requirement from: https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2022/p2300r4.html#design-sender-adaptor-split
			// but not mentioned here: https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2022/p2300r4.html#spec-execution.senders.adapt.split
			if constexpr (is_multi_shot_sender<S, split_detail::split_env>)
				return std::forward<S>(s);
			else
				return split_detail::split_sender<S>{ std::forward<S>(s) };
#else
            return split_detail::split_sender<S>{ std::forward<S>(s) };
#endif
        }
	};
	inline constexpr split_t split{};
	template<sender<split_detail::split_env> S>
    using split_result_t = decltype(split(std::declval<S>()));
	template<typename S>
	concept nothrow_split_invocable = sender<S, split_detail::split_env> && noexcept(split(std::declval<S>()));

	namespace split_detail{
        struct split_closure_t : public sender_adaptor_closure<split_closure_t>{
			[[no_unique_address]] std::tuple<> data;

			[[nodiscard]] decltype(auto) get() noexcept{
				return data;
			}
			[[nodiscard]] auto get_tag() const noexcept{
				return split_t{};
			}
        };
	}
	[[nodiscard]] inline auto split_t::operator()() const noexcept{
		return split_detail::split_closure_t{};
	}
}