#pragma once
#include "execution.hpp"

#define EXECUTION_BULK_OPERATION_WRAPPER false

namespace execution{
	namespace bulk_detail{
		using namespace ns_type_list;
        using ns_type_traits::member_type;
        using ns_type_traits::movable_value;

		template<sender S, std::integral Shape, movable_value F>
		struct sender_impl{
            [[no_unique_address]] S pre_sender;
            [[no_unique_address]] F func;
            Shape shape;

#if EXECUTION_BULK_OPERATION_WRAPPER
			// why I have to wrap it rather than just connect it like `then`?
			// requirement from: https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2022/p2300r4.html#spec-execution.senders.adapt.bulk
            template<receiver R>
            struct operation{
                struct receiver_impl{
                    template<typename... Args>
                    friend void tag_invoke(set_value_t, receiver_impl&& r, Args&&... args) noexcept{
                        try {
                            for (Shape i = 0; i < r.op->shape; ++i)
							    r.op->func(i, args...);
                            execution::set_value(std::move(r.op->out_receiver), std::forward<Args>(args)...);
                        } catch (...){
                            execution::set_error(std::move(r.op->out_receiver), std::current_exception());
                        }
                    }
                    template<ns_type_traits::any_of<set_error_t, set_stopped_t> Tag, typename... Args> requires tag_invocable<Tag, R, Args&&...>
				    friend void tag_invoke(Tag tag, receiver_impl&& r, Args&&... args) noexcept{
					    tag(std::move(r.op->out_receiver), std::forward<Args>(args)...);
				    }

                    operation* op;
                };

                friend void tag_invoke(start_t, operation& op) noexcept{
                    return execution::start(op.op);
                }

                template<typename S2, typename F2, typename R2>
                operation(S2&& s, F2&& f, R2&& r, Shape s) noexcept(std::is_nothrow_constructible_v<F, F2> && std::is_nothrow_constructible_v<R, R2&&> && nothrow_connect_invocable<S2&&, receiver_impl>)
                    :out_receiver(std::forward<R2>(r)), func(std::forward<F2>(f)), op(execution::connect(std::forward<S2>(s), receiver_impl{ this })), shape(shape){}

				operation(operation const&) = delete;
				operation(operation&&) = delete;
				operation& operator=(operation const&) = delete;
				operation& operator=(operation&&) = delete;

				[[no_unique_address]] R out_receiver;
				[[no_unique_address]] F func;
                [[no_unique_address]] connect_result_t<S, receiver_impl> op;
				Shape shape;
            };

			template<typename Self, receiver R> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl> && sender_to<member_type<Self&&, S>, std::remove_cvref_t<R>&&>
            [[nodiscard]] friend auto tag_invoke(connect_t, Self&& self, R&& out_r) noexcept(std::is_nothrow_constructible_v<S, member_type<Self&&, S>> && std::is_nothrow_constructible_v<F, member_type<Self&&, F>> && std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&>){
				return operation<std::remove_cvref_t<R>>{ std::forward<Self>(self).pre_sender, std::forward<Self>(self).func, std::move(out_r), shape };
			}
#else
            template<receiver R>
            struct receiver_impl{
                [[no_unique_address]] R out_receiver;
                [[no_unique_address]] F func;
                Shape shape;

                template<typename... Args>
                friend void tag_invoke(set_value_t, receiver_impl&& r, Args&&... args) noexcept{
                    try {
                        for (Shape i = 0; i < r.shape; ++i)
							r.func(i, args...);
                        execution::set_value(std::move(r.out_receiver), std::forward<Args>(args)...);
                    } catch (...){
                        execution::set_error(std::move(r.out_receiver), std::current_exception());
                    }
                }
                template<ns_type_traits::any_of<set_error_t, set_stopped_t> Tag, typename... Args> requires tag_invocable<Tag, R, Args&&...>
				friend void tag_invoke(Tag tag, receiver_impl&& r, Args&&... args) noexcept(std::is_nothrow_invocable_v<Tag, R&&, Args&&...>){
					tag(std::move(r.out_receiver), std::forward<Args>(args)...);
				}
            };

            template<typename Self, receiver R> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl> && sender_to<member_type<Self&&, S>, std::remove_cvref_t<R>&&>
			[[nodiscard]] friend auto tag_invoke(connect_t, Self&& self, R&& out_r) noexcept(std::is_nothrow_constructible_v<F, member_type<Self&&, F>> && std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&> && nothrow_connect_invocable<member_type<Self&&, S>, receiver_impl<std::remove_cvref_t<R>>>){
				return execution::connect(std::forward<Self>(self).pre_sender, receiver_impl<std::remove_cvref_t<R>>{ std::move(out_r), std::forward<Self>(self).func, std::forward<Self>(self).shape });
			}
#endif

        private:
			template<typename E>
			using completion_signatures = make_completion_signatures<S, E, execution::completion_signatures<auto(std::exception_ptr)->set_error_t>>;
		public:
            template<typename Self, typename E> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl>
            [[nodiscard]] friend auto tag_invoke(get_completion_signatures_t, Self&& s, E&& e) noexcept(std::is_nothrow_default_constructible_v<completion_signatures<E>>)->completion_signatures<E>{
                return completion_signatures<E>{};
            }

			template<typename Self, typename CPO> requires std::is_same_v<std::remove_cvref_t<Self>, sender_impl> && std::invocable<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>
			[[nodiscard]] friend auto tag_invoke(get_completion_scheduler_t<CPO> tag, Self&& s) noexcept(std::is_nothrow_invocable_v<get_completion_scheduler_t<CPO>, member_type<Self&&, S>>){
				return tag(std::forward<Self>(s).pre_sender);
			}
        };

		template<sender S, std::integral Shape, movable_value F>
		using bulk_sender = sender_impl<std::remove_cvref_t<S>, std::remove_cvref_t<Shape>, std::remove_cvref_t<F>>;
	}

	struct bulk_t{
		template<std::integral Shape, ns_type_traits::movable_value F>
		[[nodiscard]] inline auto operator()(Shape, F&&) const noexcept;

		template<sender S, std::integral Shape, ns_type_traits::movable_value F> requires tag_invocable_with_completion_scheduler<bulk_t, set_value_t, S&&, Shape, F&&> && sender<tag_invoke_result_t<bulk_t, completion_scheduler_for<S&&, set_value_t>, S&&, Shape, F&&>>
        [[nodiscard]] auto operator()(S&& s, Shape shape, F&& f) const noexcept(nothrow_tag_invocable<bulk_t, completion_scheduler_for<S&&, set_value_t>, S&&, Shape, F&&>)->tag_invoke_result_t<bulk_t, completion_scheduler_for<S&&, set_value_t>, S&&, Shape, F&&>{
			auto cs = get_completion_scheduler<set_value_t>(s); // guarantee the order of evaluation
            return tag_invoke(bulk_t{}, std::move(cs), std::forward<S>(s), shape, std::forward<F>(f));
        }

        template<sender S, std::integral Shape, ns_type_traits::movable_value F> requires tag_invocable<bulk_t, S&&, Shape, F&&> && (!tag_invocable_with_completion_scheduler<bulk_t, set_value_t, S&&, Shape, F&&>) && sender<tag_invoke_result_t<bulk_t, S&&, Shape, F&&>>
        [[nodiscard]] auto operator()(S&& s, Shape shape, F&& f) const noexcept(nothrow_tag_invocable<bulk_t, S&&, Shape, F&&>)->tag_invoke_result_t<bulk_t, S&&, Shape, F&&>{
            return tag_invoke(bulk_t{}, std::forward<S>(s), shape, std::forward<F>(f));
        }

        template<sender S, std::integral Shape, ns_type_traits::movable_value F> requires (!tag_invocable<bulk_t, S&&, Shape, F&&>) && (!tag_invocable_with_completion_scheduler<bulk_t, set_value_t, S&&, Shape, F&&>)
        [[nodiscard]] auto operator()(S&& s, Shape shape, F&& f) const noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<S>, S&&> && std::is_nothrow_constructible_v<std::remove_cvref_t<F>, F&&>){
            return bulk_detail::bulk_sender<S, Shape, F>{ std::forward<S>(s), std::forward<F>(f), shape };
        }
	};
	inline constexpr bulk_t bulk{};
	template<sender S, std::integral Shape, ns_type_traits::movable_value F>
    using bulk_result_t = decltype(bulk(std::declval<S>(), std::declval<Shape>(), std::declval<F>()));
    template<typename S, typename Shape, typename F>
    concept nothrow_bulk_invocable = sender<S> && std::integral<Shape> && ns_type_traits::movable_value<F> && noexcept(bulk(std::declval<S>(), std::declval<Shape>(), std::declval<F>()));

	namespace bulk_detail{
		template<std::integral Shape, movable_value F>
        struct bulk_closure_t : public sender_adaptor_closure<bulk_closure_t<Shape, F>>{
			[[no_unique_address]] std::tuple<Shape, F> data;

			[[nodiscard]] decltype(auto) get() noexcept{
				return data;
			}
			[[nodiscard]] auto get_tag() const noexcept{
				return bulk_t{};
			}
        };
	}
	template<std::integral Shape, ns_type_traits::movable_value F>
	[[nodiscard]] inline auto bulk_t::operator()(Shape shape, F&& f) const noexcept{
		return bulk_detail::bulk_closure_t<Shape, std::remove_cvref_t<F>>{ {}, { shape, std::forward<F>(f) } };
	}
}