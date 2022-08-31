#pragma once
#include "execution.hpp"


namespace execution{
	namespace start_detached_detail{
        struct receiver_impl{
            template<typename Tag, typename... Args> requires ns_type_traits::any_of<Tag, set_value_t, set_error_t, set_stopped_t>
            friend void tag_invoke(Tag, receiver_impl&& r, Args&&...) noexcept{
                [[maybe_unused]] auto s = std::move(r.op);
                if constexpr (std::same_as<Tag, set_error_t>)
                    std::terminate();
            }

            std::shared_ptr<void> op;
        };
	}
	
	struct start_detached_t{
		template<sender S> requires tag_invocable_with_completion_scheduler<start_detached_t, set_value_t, S&&> && std::is_void_v<tag_invoke_result_t<start_detached_t, completion_scheduler_for<S&&, set_value_t>, S&&>>
        void operator()(S&& s) const noexcept(nothrow_tag_invocable<start_detached_t, completion_scheduler_for<S&&, set_value_t>, S&&>){
			auto cs = get_completion_scheduler<set_value_t>(s); // guarantee the order of evaluation
            tag_invoke(start_detached_t{}, std::move(cs), std::forward<S>(s));
        }

        template<sender S> requires tag_invocable<start_detached_t, S&&> && (!tag_invocable_with_completion_scheduler<start_detached_t, set_value_t, S&&>) && std::is_void_v<tag_invoke_result_t<start_detached_t, S&&>>
        void operator()(S&& s) const noexcept(nothrow_tag_invocable<start_detached_t, S&&>){
            tag_invoke(start_detached_t{}, std::forward<S>(s));
        }

        template<sender S> requires (!tag_invocable<start_detached_t, S&&>) && (!tag_invocable_with_completion_scheduler<start_detached_t, set_value_t, S&&>)
        void operator()(S&& s) const noexcept(nothrow_connect_invocable<S&&, start_detached_detail::receiver_impl>){
            auto handler = execution::connect_into_handler.empty<S&&, start_detached_detail::receiver_impl>();
			auto new_handler = execution::connect_into_handler(handler, std::forward<S>(s), start_detached_detail::receiver_impl{ handler.ownership() });
			execution::start(*new_handler);
        }
	};
	inline constexpr start_detached_t start_detached{};
	template<typename S>
	concept nothrow_start_detached_invocable = sender<S> && noexcept(start_detached(std::declval<S>()));
}