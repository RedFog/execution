#pragma once
#include "execution.hpp"

namespace execution{
	namespace on_detail{
		using namespace ns_type_list;
        using ns_type_traits::member_type;
		template<scheduler Sch, sender S>
		struct sender_impl{
			[[no_unique_address]] Sch scheduler;
			[[no_unique_address]] S that_sender;

			struct env_for_scheduler{
				[[no_unique_address]] Sch scheduler;

				template<typename Self> requires std::same_as<std::remove_cvref_t<Self>, env_for_scheduler>
				[[nodiscard]] friend auto tag_invoke(get_scheduler_t, Self&& self) noexcept(std::is_nothrow_constructible_v<Sch, member_type<Self&&, Sch>>){
					return std::forward<Self>(self).scheduler;
				}
				template<typename Tag, typename Self, typename... Args> requires std::same_as<std::remove_cvref_t<Self>, env_for_scheduler> && (!std::same_as<std::remove_cvref_t<Tag>, get_scheduler_t>) && (execution::forwarding_env_query(Tag{}))
				friend decltype(auto) tag_invoke(Tag tag, Self&& self, Args&&... args) noexcept(std::is_nothrow_invocable_v<Tag, Self&&, Args&&...>){
					return tag(std::forward<Self>(self), std::forward<Args>(args)...);
				}
			};

			template<receiver R>
			struct operation{
				struct receiver1{
					operation* op;
				};
				struct receiver2{
					operation* op;
				};
				friend void tag_invoke(set_value_t, receiver1&& self) noexcept{
					try {
						execution::start(self.op->op2);
					} catch (...){
						execution::set_error(std::move(self.op->out_receiver), std::current_exception());
					}
				}
				template<ns_type_traits::any_of<set_error_t, set_stopped_t> Tag, class... Args> requires tag_invocable<Tag, R, Args&&...>
				friend void tag_invoke(Tag, receiver1&& self, Args&&... args) noexcept{
					try {
						Tag{}(std::move(self.op->out_receiver), std::forward<Args>(args)...);
					} catch (...){
						execution::set_error(std::move(self.op->out_receiver), std::current_exception());
					}
				}
				template<ns_type_traits::any_of<set_value_t, set_error_t, set_stopped_t> Tag, typename... Args> requires tag_invocable<Tag, R, Args&&...>
				friend void tag_invoke(Tag tag, receiver2&& self, Args&&... args) noexcept{
					try {
						tag_invoke(tag, std::move(self.op->out_receiver), std::forward<Args>(args)...);
					} catch (...){
						execution::set_error(std::move(self.op->out_receiver), std::current_exception());
					}
				}
				[[nodiscard]] friend auto tag_invoke(get_env_t, receiver2 self) noexcept(std::is_nothrow_copy_constructible_v<Sch>){
					return env_for_scheduler{ self.op->scheduler };
				}

				friend void tag_invoke(start_t, operation& self) noexcept{
					execution::start(self.op1);
				}

				template<execution::scheduler Sch2, sender S2, receiver R2>
				operation(Sch2&& sch, S2&& s, R2&& r)
					noexcept(std::is_nothrow_constructible_v<Sch, Sch2&&> && nothrow_tag_invocable<schedule_t, Sch> && nothrow_connect_invocable<schedule_result_t<Sch>, receiver1> && std::is_nothrow_constructible_v<R, R2&&> && nothrow_connect_invocable<S2&&, receiver2>)
					:scheduler(std::forward<Sch2>(sch)), out_receiver(std::forward<R2>(r)), op1{ execution::connect(schedule(scheduler), receiver1{ this }) }, op2{ execution::connect(std::forward<S2>(s), receiver2{ this }) }{}

				operation(operation const&) = delete;
				operation(operation&&) = delete;
				operation& operator=(operation const&) = delete;
				operation& operator=(operation&&) = delete;

				[[no_unique_address]] Sch scheduler;
				[[no_unique_address]] R out_receiver;
				[[no_unique_address]] connect_result_t<schedule_result_t<Sch>, receiver1> op1;
				[[no_unique_address]] connect_result_t<S, receiver2> op2;
			};

		private:
			template<typename E>
			using completion_signatures = make_completion_signatures<S, E, execution::completion_signatures<auto(std::exception_ptr)->set_error_t>>;
		public:

			template<typename Self, receiver R> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl> && sender_to<member_type<Self&&, S>, std::remove_cvref_t<R>&&>
			[[nodiscard]] friend auto tag_invoke(connect_t, Self&& self, R&& out_r){
				return operation<std::remove_cvref_t<R>>{ self.scheduler, std::move(self.that_sender), std::move(out_r) };
			}

            template<typename Self, typename E>
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<completion_signatures<E>>)->completion_signatures<E>{
                return completion_signatures<E>{};
            }

			/*template<typename Self, typename CPO> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl> && ns_type_traits::any_of<CPO, set_value_t, set_error_t, set_stopped_t>
			[[nodiscard]] friend auto tag_invoke(get_completion_scheduler_t<CPO>, Self&& s) noexcept(std::is_nothrow_constructible_v<Sch, member_type<Self&&, Sch>>){
				return std::forward<Self>(s).scheduler;
			}*/
		};
		template<scheduler Sch, sender S>
		using on_sender = sender_impl<std::remove_cvref_t<Sch>, std::remove_cvref_t<S>>;
	}

	struct on_t{
		template<scheduler Sch, sender S> requires tag_invocable<on_t, Sch&&, S&&> && sender<tag_invoke_result_t<on_t, Sch&&, S&&>>
		[[nodiscard]] auto operator()(Sch&& sch, S&& s) const noexcept(nothrow_tag_invocable<on_t, Sch&&, S&&>)->tag_invoke_result_t<on_t, Sch&&, S&&>{
			return tag_invoke(on_t{}, std::forward<Sch>(sch), std::forward<S>(s));
		}

		template<scheduler Sch, sender S> requires(!tag_invocable<on_t, Sch&&, S&&>)
		[[nodiscard]] auto operator()(Sch&& sch, S&& s) const noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<S>, S&&> && std::is_nothrow_constructible_v<std::remove_cvref_t<Sch>, Sch&&>){
			return on_detail::on_sender<Sch, S>{ std::forward<Sch>(sch), std::forward<S>(s) };
		}
	};
	inline constexpr on_t on{};
	template<scheduler Sch, sender S>
	using on_result_t = decltype(on(std::declval<Sch>(), std::declval<S>()));
	template<typename Sch, typename S>
	concept nothrow_on_invocable = scheduler<Sch> && sender<S> && noexcept(on(std::declval<Sch>(), std::declval<S>()));
}