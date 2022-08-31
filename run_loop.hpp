#pragma once
#include <atomic>
#include <mutex>
#include <condition_variable>
#include "execution.hpp"


namespace execution{
	namespace run_loop_detail{
		class run_loop{
			template<receiver R>
			class run_loop_operation;
			class run_loop_scheduler{
				template<receiver R>
				friend class run_loop_operation;
				run_loop* loop;

			public:
				run_loop_scheduler(run_loop* loop) noexcept :loop(loop){}
				[[nodiscard]] bool operator==(run_loop_scheduler const&) const noexcept = default;
			};
			struct run_loop_operate_base{
				virtual ~run_loop_operate_base() = default;
				virtual void execute() = 0;
				run_loop_operate_base* next;
			};
			struct run_loop_operate_finish : public run_loop_operate_base{
				run_loop* loop;
				std::atomic<bool> flag = false;
				virtual void execute() noexcept override{
					loop->finish();
					flag.store(true, std::memory_order_release);
				}
			};
			template<receiver R>
			class run_loop_operation : public run_loop_operate_base{
				[[no_unique_address]] R receiver;
				[[no_unique_address]] run_loop_scheduler this_scheduler;

				[[nodiscard]] run_loop* get_loop() noexcept{ return this_scheduler.loop; }
			public:
				template<typename R2>
				run_loop_operation(R2&& r, run_loop_scheduler sch) noexcept(std::is_nothrow_constructible_v<R, R2&&>) :receiver(std::forward<R2>(r)), this_scheduler(sch){}
				virtual ~run_loop_operation() = default;

				virtual void execute() noexcept override{
					try {
						auto token = execution::get_stop_token(receiver);
						if (token.stop_requested())
							execution::set_stopped(std::move(receiver));
						else
							execution::set_value(std::move(receiver));
					} catch (...){
						execution::set_error(std::move(receiver), std::current_exception());
					}
				}

				friend void tag_invoke(execution::start_t, run_loop_operation& op) noexcept{
					try {
						op.get_loop()->push_back(&op);
					} catch (...){
						execution::set_error(std::move(op.receiver), std::current_exception());
					}
				}
			};
			template<receiver R>
			using run_loop_operate = run_loop_operation<R>;
			class run_loop_sender : public completion_signatures<set_value_t(), set_error_t(std::exception_ptr), set_stopped_t()>{
				run_loop_scheduler this_scheduler;

			public:
				explicit run_loop_sender(run_loop_scheduler sch) noexcept :this_scheduler(sch){}

				template<typename Self, receiver R> requires std::is_same_v<std::remove_cvref_t<Self>, run_loop_sender> && receiver_of<R&&>
				[[nodiscard]] friend auto tag_invoke(connect_t, Self&& s, R&& r) noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<R>, R&&>){
					return run_loop_operate<std::remove_cvref_t<R>>{ std::forward<R>(r), std::forward<Self>(s).this_scheduler };
				}
				template<typename Tag, typename Self> requires std::is_same_v<std::remove_cvref_t<Self>, run_loop_sender> && ns_type_traits::any_of<Tag, set_value_t, set_stopped_t>
				[[nodiscard]] friend auto tag_invoke(get_completion_scheduler_t<Tag>, Self&& s) noexcept{
					return std::forward<Self>(s).this_scheduler;
				}
			};
			[[nodiscard]] friend run_loop_sender tag_invoke(schedule_t, run_loop_scheduler const& s) noexcept{
				return run_loop_sender{ s };
			}

			std::atomic<bool> stop = true;
			run_loop_operate_base* head = nullptr;
			run_loop_operate_base* tail = nullptr;
			run_loop_operate_finish op_finish;
			std::mutex mutex;
			std::condition_variable cv;
		public:
			run_loop() noexcept{
				op_finish.loop = this;
			};
			run_loop(run_loop const&) = delete;
			run_loop(run_loop&&) = delete;
			run_loop& operator=(run_loop const&) = delete;
			run_loop& operator=(run_loop&&) = delete;
			~run_loop() noexcept{
				if (!stop || head)
					std::terminate();
			};
			void run(){
				stop.store(false, std::memory_order_release);
				while (run_loop_operate_base* t = pop_front())
					t->execute();
			}

			[[nodiscard]] run_loop_operate_base* pop_front(){
				using namespace std::chrono_literals;
				std::unique_lock lock(mutex);
				while (!head){
					if (stop.load(std::memory_order_acquire))
						return nullptr;
					cv.wait_for(lock, 10ms);
				}
				run_loop_operate_base* result = head;
				head = head->next;
				if (!head)
					tail = nullptr;
				return result;
			}

			void push_back(run_loop_operate_base* t){
				std::unique_lock lock(mutex);
				if (!head) 
					head = t;
				else
					tail->next = t;
				tail = t;
				t->next = nullptr;
				cv.notify_one();
			}

			void finish(){
				std::unique_lock lock(mutex);
				stop.store(true, std::memory_order_release);
				cv.notify_all();
			}

			void delay_finish(){
				if (!op_finish.flag.load(std::memory_order_acquire))
					push_back(&op_finish);
			}

			[[nodiscard]] run_loop_scheduler get_scheduler() noexcept{
				return run_loop_scheduler{ this };
			}

			using scheduler = run_loop_scheduler;
			using sender = run_loop_sender;
		};
	}
	using run_loop_detail::run_loop;
	using run_loop_scheduler = run_loop::scheduler;
	using run_loop_sender = run_loop::sender;
}