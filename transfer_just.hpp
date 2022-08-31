#pragma once
#include "just.hpp"
#include "transfer.hpp"

namespace execution{
	struct transfer_just_t{
        template<scheduler Sch, ns_type_traits::movable_value... Args> requires tag_invocable<transfer_just_t, Sch&&, Args&&...> && sender_of<tag_invoke_result_t<transfer_just_t, Sch&&, Args&&...>, no_env, Args&&...>
		[[nodiscard]] auto operator()(Sch&& sch, Args&&... args) const noexcept(nothrow_tag_invocable<transfer_just_t, Sch&&, Args&&...>)->tag_invoke_result_t<transfer_just_t, Sch&&, Args&&...>{
            return tag_invoke(transfer_just_t{}, std::forward<Sch>(sch), std::forward<Args>(args)...);
        }
        template<scheduler Sch, ns_type_traits::movable_value... Args> requires (!tag_invocable<transfer_just_t, Sch&&, Args&&...>)
		[[nodiscard]] auto operator()(Sch&& sch, Args&&... args) const noexcept(nothrow_just_invocable<Args&&...> && nothrow_transfer_invocable<just_result_t<Args&&...>, Sch&&>){
            return execution::transfer(execution::just(std::forward<Args>(args)...), std::forward<Sch>(sch));
        }
    };
	inline constexpr transfer_just_t transfer_just;
	template<scheduler Sch, ns_type_traits::movable_value... Args>
    using transfer_just_result_t = decltype(transfer_just(std::declval<Sch>(), std::declval<Args>()...));
    template<typename Sch, typename... Args>
    concept nothrow_transfer_just_invocable = (scheduler<Sch> && ... && ns_type_traits::movable_value<Args>) && noexcept(transfer_just(std::declval<Sch>(), std::declval<Args>()...));

}