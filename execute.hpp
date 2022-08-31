#pragma once
#include "then.hpp"
#include "start_detached.hpp"


namespace execution{
	struct execute_t{
        template<scheduler Sch, std::invocable F> requires tag_invocable<execute_t, Sch&&, F&&> && std::is_void_v<tag_invoke_result_t<execute_t, Sch&&, F&&>>
        void operator()(Sch&& sch, F&& f) const noexcept(nothrow_tag_invocable<execute_t, Sch&&, F&&>){
            tag_invoke(execute_t{}, std::forward<Sch>(sch), std::forward<F>(f));
        }

        template<scheduler Sch, std::invocable F> requires (!tag_invocable<execute_t, Sch&&, F&&>)
        void operator()(Sch&& sch, F&& f) const noexcept(nothrow_schedule_invocable<Sch&&> && nothrow_then_invocable<schedule_result_t<Sch&&>, F&&> && nothrow_start_detached_invocable<then_result_t<schedule_result_t<Sch&&>, F&&>>){
            execution::start_detached(
                execution::schedule(std::forward<Sch>(sch))
                    | execution::then(std::forward<F>(f))
            );
        }
	};
	inline constexpr execute_t execute{};
	template<typename Sch, typename F>
	concept nothrow_execute_invocable = scheduler<Sch> && std::invocable<F> && noexcept(execute(std::declval<Sch>(), std::declval<F>()));
}