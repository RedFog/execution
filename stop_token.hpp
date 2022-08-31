#pragma once
#include <type_traits>
#include <concepts>
#include <stop_token>
#include <utility>
#include "utility.hpp"

namespace execution{

	// never_stop_token

	class never_stop_token{
		class callback{
		public:
			template<typename C>
			explicit callback(never_stop_token, C&&) noexcept{}
		};

	public:
		template<std::invocable CB>
		using callback_type = callback;

		[[nodiscard]] static constexpr bool stop_requested() noexcept{ return false; }
		[[nodiscard]] static constexpr bool stop_possible() noexcept{ return false; }
	};

	// stop_source, stop_token, stop_callback

	using std::stop_source;
	using std::stop_callback;
	struct stop_token : public std::stop_token{
		template<typename T>
		using callback_type = stop_callback<T>;
	};

	// in_place_stop_token in_place_stop_source in_place_stop_callback

	class in_place_stop_source;
	class in_place_stop_token;
	template<typename Callback> requires std::invocable<Callback> && std::destructible<Callback>
	class in_place_stop_callback;

	namespace stop_token_detail{
		class in_place_stop_callback_base{
		public:
			using CB = void(*)(in_place_stop_callback_base*) noexcept;

			explicit in_place_stop_callback_base(CB callback) noexcept :callback(callback){};

			inline void attach(in_place_stop_token&& st) noexcept;
			inline void detach() noexcept;

			in_place_stop_callback_base(in_place_stop_callback_base const&) = delete;
			in_place_stop_callback_base(in_place_stop_callback_base&&) = delete;
			in_place_stop_callback_base& operator=(in_place_stop_callback_base const&) = delete;
			in_place_stop_callback_base& operator=(in_place_stop_callback_base&&) = delete;
		private:
			friend class execution::in_place_stop_source;
			CB callback;
			in_place_stop_callback_base* prev = nullptr;
			in_place_stop_callback_base* next = nullptr;
			in_place_stop_source* parent = nullptr;
		};

		using ns_utility::locked_pointer;
	}

	class in_place_stop_source{
	public:
		in_place_stop_source() noexcept = default;

		in_place_stop_source(in_place_stop_source const&) = delete;
		in_place_stop_source(in_place_stop_source&&) = delete;
		in_place_stop_source& operator=(in_place_stop_source const&) = delete;
		in_place_stop_source& operator=(in_place_stop_source&&) = delete;
		~in_place_stop_source() noexcept{
			exec_cb<false>();
		};

		[[nodiscard]] inline in_place_stop_token get_token() const noexcept;
		[[nodiscard]] bool stop_possible() const noexcept{
			return !stop_state.load(std::memory_order_acquire);
		};
		[[nodiscard]] bool stop_requested() const noexcept{
			return stop_state.load(std::memory_order_acquire);
		};
		bool request_stop() noexcept{
			bool expected = false;
			stop_state.compare_exchange_weak(expected, true, std::memory_order_release, std::memory_order_acquire);
			if (!expected)
				exec_cb<true>();
			return !expected;
		};

	private:
		template<bool exec = false>
		void exec_cb() noexcept{
			while (true){
				auto head = cb_list.lock_and_load();
				current.store(head, std::memory_order_relaxed);
				current.notify_all();
				if (!head){
					cb_list.store_and_unlock(nullptr);
					return;
				}

				auto next = std::exchange(head->next, nullptr);
				if (next)
					next->prev = nullptr;

				cb_list.store_and_unlock(next); // cb will not block attach/detach
				if constexpr(exec)
					head->callback(head);
			}
		}

		friend class stop_token_detail::in_place_stop_callback_base;

		std::atomic_bool stop_state = false;
		stop_token_detail::locked_pointer<stop_token_detail::in_place_stop_callback_base> cb_list;
		std::atomic<stop_token_detail::in_place_stop_callback_base*> current = nullptr;
	};

	class in_place_stop_token{
	public:
		template<typename CB>
		using callback_type = in_place_stop_callback<CB>;

		in_place_stop_token() noexcept :source(nullptr){};
		~in_place_stop_token() noexcept{
			source = nullptr;
		};
		void swap(in_place_stop_token& rhs) noexcept{
			std::swap(source, rhs.source);
		};

		[[nodiscard]] bool stop_requested() const noexcept{
			return source && source->stop_requested();
		};
		[[nodiscard]] bool stop_possible() const noexcept{
			return source && source->stop_possible();
		};

		[[nodiscard]] bool operator==(const in_place_stop_token&) const noexcept = default;
		friend void swap(in_place_stop_token& lhs, in_place_stop_token& rhs) noexcept{
			lhs.swap(rhs);
		};

	private:
		friend class stop_token_detail::in_place_stop_callback_base;
		friend class in_place_stop_source;

		in_place_stop_source const* source;
		explicit in_place_stop_token(in_place_stop_source const* source) noexcept :source(source){};
	};

	[[nodiscard]] inline in_place_stop_token in_place_stop_source::get_token() const noexcept{
		return in_place_stop_token{ this };
	}

	template<typename Callback> requires std::invocable<Callback> && std::destructible<Callback>
	class in_place_stop_callback : public stop_token_detail::in_place_stop_callback_base{
	public:
		using callback_type = Callback;

		template<typename C> requires std::constructible_from<Callback, C&&>
		explicit in_place_stop_callback(in_place_stop_token st, C&& cb) noexcept(std::is_nothrow_constructible_v<Callback, C>) :in_place_stop_callback_base(&invoke_this), callback(std::forward<C>(cb)){
			attach(std::move(st));
		};
		~in_place_stop_callback() noexcept{
			detach();
		};

		in_place_stop_callback(in_place_stop_callback const&) = delete;
		in_place_stop_callback(in_place_stop_callback&&) = delete;
		in_place_stop_callback& operator=(in_place_stop_callback const&) = delete;
		in_place_stop_callback& operator=(in_place_stop_callback&&) = delete;

	private:
		static void invoke_this(stop_token_detail::in_place_stop_callback_base* base) noexcept{
			std::forward<Callback>(static_cast<in_place_stop_callback*>(base)->callback)();
		}

		Callback callback; 
	};

	template<typename Callback>
	in_place_stop_callback(in_place_stop_token, Callback)->in_place_stop_callback<Callback>;

	namespace stop_token_detail{

		inline void in_place_stop_callback_base::attach(in_place_stop_token&& st) noexcept{
			auto source = const_cast<in_place_stop_source*>(st.source);
			if (!source)
				return;

			if (source->stop_requested()){
				callback(this);
				return;
			}

			if (!source->stop_possible())
				return;

			auto head = source->cb_list.lock_and_load();

			if (source->stop_requested()){
				callback(this);
				return;
			}

			if (source->stop_possible()){
				parent = source;
				next = head;
				if (head)
					head->prev = this;
				head = this;
			}

			source->cb_list.store_and_unlock(head);
		};
		inline void in_place_stop_callback_base::detach() noexcept{
			auto source = parent;
			if (!parent)
				return;

			auto head = source->cb_list.lock_and_load();
			if (this == head){
				if (next)
					next->prev = nullptr;
				source->cb_list.store_and_unlock(next);
				return;
			}

			if (prev){
				if (next)
					next->prev = prev;
				prev->next = next;
				source->cb_list.store_and_unlock(head);
				return;
			}

			if (source->current.load(std::memory_order_acquire) != this){
				source->cb_list.store_and_unlock(head);
				return;
			}

			source->cb_list.store_and_unlock(head);
			source->current.wait(this, std::memory_order_acquire);
		};


		template<typename B>
		concept boolean_testable_impl = std::convertible_to<B, bool>;

		template<typename B>
		concept boolean_testable = boolean_testable_impl<B> && requires(B&& b){
			{ !std::forward<B>(b) }->boolean_testable_impl;
		};

		template<template<typename>typename Tem>
		struct check_type_alias_exist;
	}

	// stoppable_token, stoppable_token_for, unstoppable_token

	template<typename T>
	concept stoppable_token = std::copy_constructible<T> && std::move_constructible<T> && std::is_nothrow_copy_constructible_v<T> && std::is_nothrow_move_constructible_v<T> && std::equality_comparable<T> && requires(T const& token){
		{ token.stop_requested() } noexcept->stop_token_detail::boolean_testable;
		{ token.stop_possible() } noexcept->stop_token_detail::boolean_testable;
		typename stop_token_detail::check_type_alias_exist<T::template callback_type>;
	};

	template<typename T, typename CB, typename Initializer = CB>
	concept stoppable_token_for = stoppable_token<T> && std::invocable<CB> && requires{ typename T::template callback_type<CB>; } &&
		std::constructible_from<CB, Initializer> &&
		std::constructible_from<typename T::template callback_type<CB>, T, Initializer> &&
		std::constructible_from<typename T::template callback_type<CB>, T&, Initializer> &&
		std::constructible_from<typename T::template callback_type<CB>, T const, Initializer> &&
		std::constructible_from<typename T::template callback_type<CB>, T const&, Initializer>;

	template<typename T>
	concept unstoppable_token = stoppable_token<T> && requires{ { T::stop_possible() }->stop_token_detail::boolean_testable; } && (!T::stop_possible());
}