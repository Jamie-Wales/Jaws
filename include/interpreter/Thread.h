#include <chrono> // For timeouts
#include <iostream>
#include <optional>
#include <sstream>
#include <thread>

// #define DEBUG_LOGGING

#ifdef DEBUG_LOGGING
#define DEBUG_LOG(x) std::cerr << "[DEBUG] " << x << std::endl
#else
#define DEBUG_LOG(x)
#endif

class SchemeValue;

class ThreadHandle {
public:
    std::thread thread;
    std::mutex resultMutex;
    std::shared_ptr<SchemeValue> result;
    bool completed = false;
    std::thread::id threadId;

    ThreadHandle()
    {
        DEBUG_LOG("Creating ThreadHandle @ " << this);
    }

    ~ThreadHandle()
    {
        DEBUG_LOG("Destroying ThreadHandle @ " << this);
        if (thread.joinable()) {
            try {
                DEBUG_LOG("ThreadHandle destructor: joining thread " << threadId);
                thread.join();
            } catch (const std::exception& e) {
                DEBUG_LOG("ThreadHandle destructor: exception during join: " << e.what());
                thread.detach();
                DEBUG_LOG("ThreadHandle destructor: thread detached as fallback");
            }
        }
    }
};

class MutexHandle {
public:
    std::mutex mutex;

    MutexHandle()
    {
        DEBUG_LOG("Creating MutexHandle @ " << this);
    }

    ~MutexHandle()
    {
        DEBUG_LOG("Destroying MutexHandle @ " << this);
    }
};

class ConditionVarHandle {
public:
    std::condition_variable cv;

    ConditionVarHandle()
    {
        DEBUG_LOG("Creating ConditionVarHandle @ " << this);
    }

    ~ConditionVarHandle()
    {
        DEBUG_LOG("Destroying ConditionVarHandle @ " << this);
    }
};
