#pragma once
#include "transfer.hpp"
#include "when_all.hpp"


namespace execution{
	struct transfer_when_all_t{
        template<scheduler Sch, sender S, sender... Ss> requires tag_invocable<transfer_when_all_t, Sch&&, S&&, Ss&&...> && sender<tag_invoke_result_t<transfer_when_all_t, Sch&&, S&&, Ss&&...>, no_env>
		[[nodiscard]] auto operator()(Sch&& sch, S&& s, Ss&&... ss) const noexcept(nothrow_tag_invocable<transfer_when_all_t, Sch&&, S&&, Ss&&...>)->tag_invoke_result_t<transfer_when_all_t, Sch&&, S&&, Ss&&...>{
            return tag_invoke(transfer_when_all_t{}, std::forward<Sch>(sch), std::forward<S>(s), std::forward<Ss>(ss)...);
        }
        template<scheduler Sch, sender S, sender... Ss> requires (!tag_invocable<transfer_when_all_t, Sch&&, S&&, Ss&&...>)
		[[nodiscard]] auto operator()(Sch&& sch, S&& s, Ss&&... ss) const noexcept(nothrow_when_all_invocable<S&&, Ss&&...> && nothrow_transfer_invocable<when_all_result_t<S&&, Ss&&...>, Sch&&>){
            return execution::transfer(execution::when_all(std::forward<S>(s), std::forward<Ss>(ss)...), std::forward<Sch>(sch));
        }
    };
	inline constexpr transfer_when_all_t transfer_when_all;
	template<scheduler Sch, sender S, sender... Ss>
    using transfer_when_all_result_t = decltype(transfer_when_all(std::declval<Sch>(), std::declval<S>(), std::declval<Ss>()...));
    template<typename Sch, typename S, typename... Ss>
    concept nothrow_transfer_when_all_invocable = scheduler<Sch> && (sender<S> && ... && sender<Ss>) && noexcept(transfer_when_all(std::declval<Sch>(), std::declval<S>(), std::declval<Ss>()...));
}