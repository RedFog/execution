#pragma once
#include "transfer_when_all.hpp"
#include "into_variant.hpp"


namespace execution{
    struct when_all_with_variant_t{
		template<sender... Ss>
		[[nodiscard]] inline auto operator()(Ss&&... ss) const noexcept;

        template<sender S, sender... Ss> requires tag_invocable<when_all_with_variant_t, S&&, Ss&&...> && sender<tag_invoke_result_t<when_all_with_variant_t, S&&, Ss&&...>>
		[[nodiscard]] auto operator()(S&& s, Ss&&... ss) const noexcept(nothrow_tag_invocable<when_all_with_variant_t, S&&, Ss&&...>)->tag_invoke_result_t<when_all_with_variant_t, S&&, Ss&&...>{
            return tag_invoke(when_all_with_variant_t{}, std::forward<S>(s), std::forward<Ss>(ss)...);
        }
        template<sender S, sender... Ss> requires (!tag_invocable<when_all_with_variant_t, S&&, Ss&&...>)
		[[nodiscard]] auto operator()(S&& s, Ss&&... ss) const noexcept((nothrow_into_variant_invocable<S&&> && ... && nothrow_into_variant_invocable<Ss&&>) && nothrow_when_all_invocable<into_variant_result_t<S&&>, into_variant_result_t<Ss&&>...>){
            return execution::when_all(execution::into_variant(std::forward<S>(s)), execution::into_variant(std::forward<Ss>(ss))...);
        }
    };
	inline constexpr when_all_with_variant_t when_all_with_variant;
	template<sender S, sender... Ss>
    using when_all_with_variant_result_t = decltype(when_all_with_variant(std::declval<S>(), std::declval<Ss>()...));
    template<typename S, typename... Ss>
    concept nothrow_when_all_with_variant_invocable = (sender<S> && ... && sender<Ss>) && noexcept(when_all_with_variant(std::declval<Ss>()...));

	struct transfer_when_all_with_variant_t{
        template<scheduler Sch, sender S, sender... Ss> requires tag_invocable<transfer_when_all_with_variant_t, Sch&&, S&&, Ss&&...> && sender<tag_invoke_result_t<transfer_when_all_with_variant_t, Sch&&, S&&, Ss&&...>>
		[[nodiscard]] auto operator()(Sch&& sch, S&& s, Ss&&... ss) const noexcept(nothrow_tag_invocable<transfer_when_all_with_variant_t, Sch&&, S&&, Ss&&...>)->tag_invoke_result_t<transfer_when_all_with_variant_t, Sch&&, S&&, Ss&&...>{
            return tag_invoke(transfer_when_all_with_variant_t{}, std::forward<Sch>(sch), std::forward<S>(s), std::forward<Ss>(ss)...);
        }
        template<scheduler Sch, sender S, sender... Ss> requires (!tag_invocable<transfer_when_all_with_variant_t, Sch&&, S&&, Ss&&...>)
		[[nodiscard]] auto operator()(Sch&& sch, S&& s, Ss&&... ss) const noexcept((nothrow_into_variant_invocable<S&&> && ... && nothrow_into_variant_invocable<Ss&&>) && nothrow_transfer_when_all_invocable<Sch&&, into_variant_result_t<S&&>, into_variant_result_t<Ss&&>...>){
			return execution::transfer_when_all(std::forward<Sch>(sch), execution::into_variant(std::forward<S>(s)), execution::into_variant(std::forward<Ss>(ss))...);
        }
    };
	inline constexpr transfer_when_all_with_variant_t transfer_when_all_with_variant;
	template<scheduler Sch, sender S, sender... Ss>
    using transfer_when_all_with_variant_result_t = decltype(transfer_when_all_with_variant(std::declval<Sch>(), std::declval<S>(), std::declval<Ss>()...));
    template<typename Sch, typename S, typename... Ss>
    concept nothrow_transfer_when_all_with_variant_invocable = scheduler<Sch> && (sender<S> && ... && sender<Ss>) && noexcept(transfer_when_all_with_variant(std::declval<Sch>(), std::declval<Ss>()...));

	namespace when_all_with_variant_detail{
		template<sender... Ss>
        struct when_all_with_variant_closure_t : public sender_adaptor_closure<when_all_with_variant_closure_t<Ss...>>{
			[[no_unique_address]] std::tuple<Ss...> data;

			[[nodiscard]] decltype(auto) get() noexcept{
				return data;
			}
			[[nodiscard]] auto get_tag() const noexcept{
				return when_all_with_variant_t{};
			}
        };
	}
	template<sender... Ss>
	[[nodiscard]] inline auto when_all_with_variant_t::operator()(Ss&&... ss) const noexcept{
		return when_all_with_variant_detail::when_all_with_variant_closure_t<std::remove_cvref_t<Ss>...>{ {}, { std::forward<Ss>(ss)... } };
	}
}