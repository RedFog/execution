#pragma once
#include "execution.hpp"

namespace execution{
	namespace schedule_from_detail{
		using namespace ns_type_list;
		using ns_type_traits::member_type;
		using ns_type_traits::overload_t;
		using ns_utility::monostate_variant;

		template<scheduler Sch, sender S>
		struct sender_impl{
			[[no_unique_address]] Sch scheduler;
			[[no_unique_address]] S pre_sender;

			template<receiver R>
			struct operation{
			private:
				template<typename... Ts>
				using value_type_list = type_list<set_value_t, std::remove_cvref_t<Ts>...>;
				template<typename... Ts>
				using error_type_list = type_list<type_list<set_error_t, std::remove_cvref_t<Ts>>...>;
				using value_types_list = value_types_of_t<S, env_of_t<R>, value_type_list, type_list>;
				using error_types_list = include_or_add_t<error_types_of_t<S, env_of_t<R>, error_type_list>, type_list<set_error_t, std::exception_ptr>>;
				using data_type = type_list_deep_apply_t<type_set_concat_t<value_types_list, error_types_list, type_list<type_list<set_stopped_t>>>, monostate_variant, std::tuple>;

				struct receiver_before_schedule{
					operation* op;
				};
				struct receiver_after_schedule{
					operation* op;
				};

			public:
				template<ns_type_traits::any_of<set_value_t, set_error_t, set_stopped_t> Tag, typename... Args>
				friend void tag_invoke(Tag tag, receiver_before_schedule&& r, Args&&... args) noexcept{
					try {
						r.op->data.template emplace<decayed_tuple<Tag, Args&&...>>(tag, std::forward<Args>(args)...);
						execution::start(r.op->op2);
					} catch (...){
						execution::set_error(std::move(r.op->out_receiver), std::current_exception());
					}
				}
				friend void tag_invoke(set_value_t, receiver_after_schedule&& r) noexcept{
					try {
						std::visit(overload_t{
							[](std::monostate) noexcept{},
							[op = r.op]<typename Tuple>(Tuple&& tuple) noexcept{
								std::apply([op]<typename Tag, typename... Args>(Tag tag, Args&&... args) noexcept{
									tag(std::move(op->out_receiver), std::forward<Args>(args)...);
								}, std::forward<Tuple>(tuple));
							}
						}, std::move(r.op->data));
					} catch (...){
						execution::set_error(std::move(r.op->out_receiver), std::current_exception());
					}
				}
				template<ns_type_traits::any_of<set_error_t, set_stopped_t> Tag, typename... Args>
				friend void tag_invoke(Tag tag, receiver_after_schedule&& r, Args&&... args) noexcept(std::is_nothrow_invocable_v<Tag, R&&, Args&&...>){
					tag(std::move(r.op->out_receiver), std::forward<Args>(args)...);
				}

			private:
				[[no_unique_address]] R out_receiver;
				[[no_unique_address]] connect_result_t<S, receiver_before_schedule> op1;
				[[no_unique_address]] connect_result_t<schedule_result_t<Sch>, receiver_after_schedule> op2;
				[[no_unique_address]] data_type data;

			public:
				template<typename Sch2, typename S2, typename R2>
				operation(Sch2&& sch, S2&& s, R2&& r) noexcept(std::is_nothrow_constructible_v<R, R2&&> && nothrow_connect_invocable<S2&&, receiver_before_schedule> && nothrow_schedule_invocable<Sch2&&> && nothrow_connect_invocable<schedule_result_t<Sch&&>, receiver_after_schedule>)
					:out_receiver(std::forward<R2>(r)), op1(execution::connect(std::forward<S2>(s), receiver_before_schedule{ this })), op2(execution::connect(execution::schedule(std::forward<Sch2>(sch)), receiver_after_schedule{ this })){}

				operation(operation const&) = delete;
				operation(operation&&) = delete;
				operation& operator=(operation const&) = delete;
				operation& operator=(operation&&) = delete;

				friend void tag_invoke(start_t, operation& op) noexcept{
					execution::start(op.op1);
				}
			};

		private:
			template<typename E>
			using completion_signatures = make_completion_signatures<S, E, std::conditional_t<completion_signatures_of_t<schedule_result_t<Sch>, E>::sends_stopped, execution::completion_signatures<set_error_t(std::exception_ptr), set_stopped_t()>, execution::completion_signatures<set_error_t(std::exception_ptr)>>>;
		public:
			template<typename Self, typename E> requires std::same_as<std::remove_cvref_t<Self>, sender_impl>
			[[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<completion_signatures<E>>)->completion_signatures<E>{
				return completion_signatures<E>{};
			}

			template<typename Self, receiver R> requires std::same_as<std::remove_cvref_t<Self>, sender_impl>
			[[nodiscard]] friend auto tag_invoke(connect_t, Self&& s, R&& r) noexcept(std::is_nothrow_constructible_v<Sch, member_type<Self, Sch>> && std::is_nothrow_constructible_v<S, member_type<Self, S>> && std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&>){
				return operation<std::remove_cvref_t<R>>{ std::forward<Self>(s).scheduler, std::forward<Self>(s).pre_sender, std::forward<R>(r) };
			}

			template<typename Self, typename CPO> requires std::same_as<std::remove_cvref_t<Self>, sender_impl> && ns_type_traits::any_of<CPO, set_value_t, set_stopped_t>
			[[nodiscard]] friend auto tag_invoke(get_completion_scheduler_t<CPO>, Self&& s) noexcept(std::is_nothrow_constructible_v<Sch, member_type<Self, Sch>>){
				return std::forward<Self>(s).scheduler;
			}
		};

		template<scheduler Sch, sender S>
		using schedule_from_sender = sender_impl<std::remove_cvref_t<Sch>, std::remove_cvref_t<S>>;

	}

	struct schedule_from_t{
		template<scheduler Sch, sender S> requires tag_invocable<schedule_from_t, Sch&&, S&&> && sender<tag_invoke_result_t<schedule_from_t, Sch&&, S&&>>
		[[nodiscard]] auto operator()(Sch&& sch, S&& s) const noexcept(nothrow_tag_invocable<schedule_from_t, Sch&&, S&&>)->tag_invoke_result_t<schedule_from_t, Sch&&, S&&>{
			return tag_invoke(schedule_from_t{}, std::forward<Sch>(sch), std::forward<S>(s));
		}

		template<scheduler Sch, sender S> requires(!tag_invocable<schedule_from_t, Sch&&, S&&>)
		[[nodiscard]] auto operator()(Sch&& sch, S&& s) const noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<S>, S&&> && std::is_nothrow_constructible_v<std::remove_cvref_t<Sch>, Sch&&>){
			return schedule_from_detail::schedule_from_sender<Sch, S>{ std::forward<Sch>(sch), std::forward<S>(s) };
		}
	};
	inline constexpr schedule_from_t schedule_from{};
	template<scheduler Sch, sender S>
    using schedule_from_result_t = decltype(schedule_from(std::declval<Sch>(), std::declval<S>()));
	template<typename Sch, typename S>
	concept nothrow_schedule_from_invocable = scheduler<Sch> && sender<S> && noexcept(schedule_from(std::declval<Sch>(), std::declval<S>()));

	struct transfer_t{
		template<typename Sch>
		[[nodiscard]] inline auto operator()(Sch&& sch) const noexcept;

		template<sender S, scheduler Sch> requires tag_invocable_with_completion_scheduler<transfer_t, set_value_t, S&&, Sch&&> && sender<tag_invoke_result_t<transfer_t, completion_scheduler_for<S&&, set_value_t>, S&&, Sch&&>>
		[[nodiscard]] auto operator()(S&& s, Sch&& sch) const noexcept(nothrow_tag_invocable<transfer_t, completion_scheduler_for<S&&, set_value_t>, S&&, Sch&&>)->tag_invoke_result_t<transfer_t, completion_scheduler_for<S&&, set_value_t>, S&&, Sch&&>{
			auto cs = get_completion_scheduler<set_value_t>(s); // guarantee the order of evaluation
			return tag_invoke(transfer_t{}, std::move(cs), std::forward<S>(s), std::forward<Sch>(sch));
		}

		template<sender S, scheduler Sch> requires(!tag_invocable_with_completion_scheduler<transfer_t, set_value_t, S&&, Sch&&>) && tag_invocable<transfer_t, S&&, Sch&&> && sender<tag_invoke_result_t<transfer_t, S&&, Sch&&>>
		[[nodiscard]] auto operator()(S&& s, Sch&& sch) const noexcept(nothrow_tag_invocable<transfer_t, S&&, Sch&&>)->tag_invoke_result_t<transfer_t, S&&, Sch&&>{
			return tag_invoke(transfer_t{}, std::forward<S>(s), std::forward<Sch>(sch));
		}

		template<sender S, scheduler Sch> requires(!tag_invocable_with_completion_scheduler<transfer_t, set_value_t, S&&, Sch&&>) && (!tag_invocable<transfer_t, S&&, Sch&&>)
		[[nodiscard]] auto operator()(S&& s, Sch&& sch) const noexcept(nothrow_schedule_from_invocable<Sch, S>){
			return execution::schedule_from(std::forward<Sch>(sch), std::forward<S>(s));
		}
	};
	inline constexpr transfer_t transfer{};
	template<sender S, scheduler Sch>
    using transfer_result_t = decltype(transfer(std::declval<S>(), std::declval<Sch>()));
	template<typename S, typename Sch>
	concept nothrow_transfer_invocable = sender<S> && scheduler<Sch> && noexcept(transfer(std::declval<Sch>(), std::declval<S>()));

	namespace transfer_detail{
		template<typename Sch>
		struct transfer_closure_t : public sender_adaptor_closure<transfer_closure_t<Sch>>{
			[[no_unique_address]] std::tuple<Sch> data;

			[[nodiscard]] decltype(auto) get() noexcept{
				return data;
			}
			[[nodiscard]] auto get_tag() const noexcept{
				return transfer_t{};
			}
		};
	}

	template<typename Sch>
	[[nodiscard]] inline auto transfer_t::operator()(Sch&& sch) const noexcept{
		return transfer_detail::transfer_closure_t<std::remove_cvref_t<Sch>>{ {}, { std::forward<Sch>(sch) } };
	}
}
