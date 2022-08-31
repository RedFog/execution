#pragma once
#include <mutex>
#include <memory>
#include <variant>



namespace ns_utility{
	template<typename... Vs>
	using monostate_variant = std::variant<std::monostate, Vs...>;

	template<typename T>
	struct locked_pointer{
		T* pointer = nullptr;
		std::mutex lock;

		[[nodiscard]] T* lock_and_load() noexcept{
			lock.lock();
			return pointer;
		}
		void store_and_unlock(T* ptr) noexcept{
			pointer = ptr;
			lock.unlock();
		}
	};
	template<typename T>
	struct locked_shared_ptr{
		std::shared_ptr<T> pointer = nullptr;
		std::mutex lock;

		[[nodiscard]] std::shared_ptr<T> lock_and_load() noexcept{
			lock.lock();
			return pointer;
		}
		void store_and_unlock(std::shared_ptr<T> ptr) noexcept{
			pointer = std::move(ptr);
			lock.unlock();
		}
	};
}