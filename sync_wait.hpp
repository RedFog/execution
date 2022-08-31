#pragma once
#include "into_variant.hpp"
#include "run_loop.hpp"


namespace this_thread{
	namespace sync_wait_detail{
		using namespace execution;

		struct sync_wait_env{
			template<typename Self> requires std::same_as<std::remove_cvref_t<Self>, sync_wait_env>
			[[nodiscard]] friend auto tag_invoke(get_scheduler_t, Self&& s) noexcept{
				return s.this_scheduler;
			}
			template<typename Self> requires std::same_as<std::remove_cvref_t<Self>, sync_wait_env>
			[[nodiscard]] friend auto tag_invoke(get_delegatee_scheduler_t, Self&& s) noexcept{
				return s.this_scheduler;
			}
			run_loop_scheduler this_scheduler;
		};

		template<typename T, typename E>
		struct receiver_impl{
			[[nodiscard]] friend auto tag_invoke(get_env_t, receiver_impl&& r) noexcept{
				return sync_wait_env{ r.loop->get_scheduler() };
			}
			template<typename... Args> requires std::same_as<std::optional<decayed_tuple<Args&&...>>, T>
			friend void tag_invoke(set_value_t, receiver_impl&& r, Args&&... args) noexcept(std::is_nothrow_constructible_v<decayed_tuple<Args&&...>, Args&&...>){
				r.ret->emplace(std::forward<Args>(args)...);
				r.loop->finish();
			}
			template<typename E2>
			friend void tag_invoke(set_error_t, receiver_impl&& r, E2&& e) noexcept{
				r.error->template emplace<std::remove_cvref_t<E2>>(std::forward<E2>(e));
				r.loop->finish();
			}
			friend void tag_invoke(set_stopped_t, receiver_impl&& r) noexcept{
				r.ret->emplace();
				r.loop->finish();
			}
			T* ret;
			E* error;
			run_loop* loop;
		};

		template<sender<sync_wait_env> S>
		using sync_wait_type = std::optional<value_types_of_t<S, sync_wait_env, decayed_tuple, std::type_identity_t>>;
		template<sender<sync_wait_env> S>
		using sync_wait_with_variant_type = std::optional<into_variant_type<S, sync_wait_env>>;
	}

	struct sync_wait_t{
		template<execution::sender<sync_wait_detail::sync_wait_env> S> requires execution::tag_invocable_with_completion_scheduler<sync_wait_t, execution::set_value_t, S&&> && std::same_as<sync_wait_detail::sync_wait_type<S>, execution::tag_invoke_result_t<sync_wait_t, execution::completion_scheduler_for<S&&, execution::set_value_t>, S&&>>
        auto operator()(S&& s) const noexcept(execution::nothrow_tag_invocable<sync_wait_t, execution::completion_scheduler_for<S&&, execution::set_value_t>, S&&>)->execution::tag_invoke_result_t<sync_wait_t, execution::completion_scheduler_for<S&&, execution::set_value_t>, S&&>{
			auto cs = execution::get_completion_scheduler<execution::set_value_t>(s); // guarantee the order of evaluation
			return execution::tag_invoke(sync_wait_t{}, std::move(cs), std::forward<S>(s));
        }

        template<execution::sender<sync_wait_detail::sync_wait_env> S> requires execution::tag_invocable<sync_wait_t, S&&> && (!execution::tag_invocable_with_completion_scheduler<sync_wait_t, execution::set_value_t, S&&>) && std::same_as<sync_wait_detail::sync_wait_type<S>, execution::tag_invoke_result_t<sync_wait_t, S&&>>
        auto operator()(S&& s) const noexcept(execution::nothrow_tag_invocable<sync_wait_t, S&&>)->execution::tag_invoke_result_t<sync_wait_t, S&&>{
			return execution::tag_invoke(sync_wait_t{}, std::forward<S>(s));
        }

        template<execution::sender<sync_wait_detail::sync_wait_env> S> requires (!execution::tag_invocable<sync_wait_t, S&&>) && (!execution::tag_invocable_with_completion_scheduler<sync_wait_t, execution::set_value_t, S&&>)
        auto operator()(S&& s) const->sync_wait_detail::sync_wait_type<S>{
			execution::run_loop loop;
			sync_wait_detail::sync_wait_type<S> ret;
			using ETL = execution::error_types_of_t<S, sync_wait_detail::sync_wait_env, ns_type_list::type_list>;
			using error_type = ns_type_list::type_list_apply_t<ns_type_list::include_or_add_t<ETL, std::exception_ptr>, ns_utility::monostate_variant>;
			error_type error;
			auto op = execution::connect(std::forward<S>(s), sync_wait_detail::receiver_impl<sync_wait_detail::sync_wait_type<S>, error_type>{ &ret, &error, &loop });
			auto sch = loop.get_scheduler();
			auto functor = [&op](auto...) noexcept{ execution::start(op); };
			auto unlock_op = execution::connect(execution::schedule(sch), execution::constructive_receiver{ functor, functor, functor });
			execution::start(unlock_op);
			loop.run();
			std::visit(ns_type_traits::overload_t{
				[](std::monostate) noexcept{},
				[](std::exception_ptr e){ std::rethrow_exception(e); },
				[](std::error_code e){ throw std::system_error(e); },
				[]<typename E>(E&& e){ throw std::forward<E>(e); },
			}, std::move(error));
			return ret;
        }
	};
	inline constexpr sync_wait_t sync_wait{};
	template<execution::sender<sync_wait_detail::sync_wait_env> S>
    using sync_wait_result_t = decltype(sync_wait(std::declval<S>()));
	template<typename S>
	concept nothrow_sync_wait_invocable = execution::sender<S, sync_wait_detail::sync_wait_env> && noexcept(sync_wait(std::declval<S>()));

	struct sync_wait_with_variant_t{
		template<execution::sender<sync_wait_detail::sync_wait_env> S> requires execution::tag_invocable_with_completion_scheduler<sync_wait_with_variant_t, execution::set_value_t, S&&> && std::same_as<sync_wait_detail::sync_wait_with_variant_type<S>, execution::tag_invoke_result_t<sync_wait_with_variant_t, execution::completion_scheduler_for<S&&, execution::set_value_t>, S&&>>
        auto operator()(S&& s) const noexcept(execution::nothrow_tag_invocable<sync_wait_with_variant_t, execution::completion_scheduler_for<S&&, execution::set_value_t>, S&&>)->execution::tag_invoke_result_t<sync_wait_with_variant_t, execution::completion_scheduler_for<S&&, execution::set_value_t>, S&&>{
			auto cs = execution::get_completion_scheduler<execution::set_value_t>(s); // guarantee the order of evaluation
			return execution::tag_invoke(sync_wait_with_variant_t{}, std::move(cs), std::forward<S>(s));
        }

        template<execution::sender<sync_wait_detail::sync_wait_env> S> requires execution::tag_invocable<sync_wait_with_variant_t, S&&> && (!execution::tag_invocable_with_completion_scheduler<sync_wait_with_variant_t, execution::set_value_t, S&&>) && std::same_as<sync_wait_detail::sync_wait_with_variant_type<S>, execution::tag_invoke_result_t<sync_wait_with_variant_t, S&&>>
        auto operator()(S&& s) const noexcept(execution::nothrow_tag_invocable<sync_wait_with_variant_t, S&&>)->execution::tag_invoke_result_t<sync_wait_with_variant_t, S&&>{
			return execution::tag_invoke(sync_wait_with_variant_t{}, std::forward<S>(s));
        }

        template<execution::sender<sync_wait_detail::sync_wait_env> S> requires (!execution::tag_invocable<sync_wait_with_variant_t, S&&>) && (!execution::tag_invocable_with_completion_scheduler<sync_wait_with_variant_t, execution::set_value_t, S&&>)
        auto operator()(S&& s) const noexcept(execution::nothrow_into_variant_invocable<S&&> && this_thread::nothrow_sync_wait_invocable<execution::into_variant_result_t<S&&>>){
            return this_thread::sync_wait(execution::into_variant(std::forward<S>(s)));
        }
	};
	inline constexpr sync_wait_with_variant_t sync_wait_with_variant{};
	template<execution::sender<sync_wait_detail::sync_wait_env> S>
    using sync_wait_with_variant_result_t = decltype(sync_wait_with_variant(std::declval<S>()));
	template<typename S>
	concept nothrow_sync_wait_with_variant_invocable = execution::sender<S, sync_wait_detail::sync_wait_env> && noexcept(sync_wait_with_variant(std::declval<S>()));
}