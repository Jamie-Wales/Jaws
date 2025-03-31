#include "builtins/JawsThread.h"
#include "Error.h"
#include "Procedure.h"
#include "Value.h"
#include "interpret.h"
#include <condition_variable>
#include <iostream>
#include <mutex>
#include <optional>
#include <sstream>
#include <thread>

// #define DEBUG_LOGGING
#ifdef DEBUG_LOGGING
#define DEBUG_LOG(x) std::cerr << "[DEBUG] " << x << std::endl
#else
#define DEBUG_LOG(x)
#endif

namespace jaws_thread {
std::optional<SchemeValue> threadSpawn(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("thread-spawn: requires exactly 1 argument (a procedure)");
    }
    auto procVal = args[0].ensureValue();
    if (!procVal.isProc()) {
        throw InterpreterError("thread-spawn: argument must be a procedure");
    }
    auto proc = procVal.asProc();
    auto threadHandle = std::make_shared<ThreadHandle>();
    auto capturedEnv = state.env;
    DEBUG_LOG("threadSpawn: Capturing current environment @ " << capturedEnv.get() << " for new thread");
    auto rootEnv = state.rootEnv; // Keep reference to rootEnv
    DEBUG_LOG("threadSpawn: Root environment is @ " << rootEnv.get());
    DEBUG_LOG("threadSpawn: starting thread");
    threadHandle->thread = std::thread([proc, threadHandle, capturedEnv, rootEnv]() {
        std::optional<SchemeValue> finalResult = std::nullopt;

        try {
            threadHandle->threadId = std::this_thread::get_id();
            DEBUG_LOG("Thread " << threadHandle->threadId << " started");
            interpret::InterpreterState threadState;
            threadState.rootEnv = rootEnv;
            threadState.env = capturedEnv->extend();
            DEBUG_LOG("Thread " << threadHandle->threadId << " created extended environment @ "
                                << threadState.env.get() << " with parent @ " << capturedEnv.get());
            SchemeValue procSchemeValue(proc);
            finalResult = interpret::executeProcedure(threadState, procSchemeValue, {});
            DEBUG_LOG("Thread " << threadHandle->threadId << " executeProcedure call completed.");
            std::lock_guard<std::mutex> lock(threadHandle->resultMutex);
            if (finalResult) {
                DEBUG_LOG("Thread " << threadHandle->threadId << " storing final result: " << finalResult->toString());
                threadHandle->result = std::make_shared<SchemeValue>(*finalResult);
            } else {
                DEBUG_LOG("Thread " << threadHandle->threadId << " procedure returned no final result");
            }
            threadHandle->completed = true;
            DEBUG_LOG("Thread " << threadHandle->threadId << " marked as completed");

        } catch (const InterpreterError& e) {
            DEBUG_LOG("Thread " << std::this_thread::get_id() << " InterpreterError: " << e.what());
            std::lock_guard<std::mutex> lock(threadHandle->resultMutex);
            threadHandle->completed = true; // Mark as completed even on error
        } catch (const std::exception& e) {
            DEBUG_LOG("Thread " << std::this_thread::get_id() << " std::exception: " << e.what());
            std::lock_guard<std::mutex> lock(threadHandle->resultMutex);
            threadHandle->completed = true;
        } catch (...) {
            DEBUG_LOG("Thread " << std::this_thread::get_id() << " unknown exception");
            std::lock_guard<std::mutex> lock(threadHandle->resultMutex);
            threadHandle->completed = true;
        }
        DEBUG_LOG("Thread " << threadHandle->threadId << " function ending");
    });

    DEBUG_LOG("threadSpawn: thread created, returning handle");
    return SchemeValue(threadHandle);
}

std::optional<SchemeValue> threadJoin(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    DEBUG_LOG("threadJoin called");

    if (args.size() != 1) {
        DEBUG_LOG("threadJoin: wrong number of arguments");
        throw InterpreterError("thread-join: requires exactly 1 argument (a thread)");
    }

    DEBUG_LOG("threadJoin: checking if argument is a thread");
    auto threadVal = args[0].ensureValue();
    if (!threadVal.isThread()) {
        DEBUG_LOG("threadJoin: argument is not a thread");
        throw InterpreterError("thread-join: argument must be a thread");
    }

    DEBUG_LOG("threadJoin: got thread handle");
    auto threadHandle = threadVal.asThread();
    if (threadHandle->thread.joinable()) {
        DEBUG_LOG("threadJoin: thread is joinable, joining...");
        threadHandle->thread.join();
        DEBUG_LOG("threadJoin: thread joined");
    } else {
        DEBUG_LOG("threadJoin: thread is not joinable");
    }
    DEBUG_LOG("threadJoin: locking result mutex");
    std::lock_guard<std::mutex> lock(threadHandle->resultMutex);

    if (threadHandle->completed) {
        DEBUG_LOG("threadJoin: thread is completed");
        if (threadHandle->result) {
            DEBUG_LOG("threadJoin: returning thread result");
            return *threadHandle->result;
        } else {
            DEBUG_LOG("threadJoin: thread has no result");
        }
    } else {
        DEBUG_LOG("threadJoin: thread is not marked as completed");
    }

    DEBUG_LOG("threadJoin: returning default value (false)");
    return SchemeValue(false);
}

std::optional<SchemeValue> threadSleep(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    DEBUG_LOG("threadSleep called");

    if (args.size() != 1) {
        DEBUG_LOG("threadSleep: wrong number of arguments");
        throw InterpreterError("thread-sleep!: requires exactly 1 argument (milliseconds)");
    }

    auto msVal = args[0].ensureValue();
    if (!msVal.isNumber()) {
        DEBUG_LOG("threadSleep: argument is not a number");
        throw InterpreterError("thread-sleep!: argument must be a number (milliseconds)");
    }

    int ms = msVal.asNumber().toInt();
    if (ms < 0) {
        DEBUG_LOG("threadSleep: negative milliseconds");
        throw InterpreterError("thread-sleep!: milliseconds cannot be negative");
    }

    DEBUG_LOG("threadSleep: sleeping for " << ms << " ms");
    std::this_thread::sleep_for(std::chrono::milliseconds(ms));
    DEBUG_LOG("threadSleep: sleep complete");

    return std::nullopt;
}

std::optional<SchemeValue> threadCurrentId(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    DEBUG_LOG("threadCurrentId called");

    if (!args.empty()) {
        DEBUG_LOG("threadCurrentId: wrong number of arguments");
        throw InterpreterError("thread-current-id: takes no arguments");
    }
    auto id = std::this_thread::get_id();
    std::stringstream ss;
    ss << id;
    std::string idStr = ss.str();

    DEBUG_LOG("threadCurrentId: returning ID " << idStr);
    return SchemeValue(idStr);
}

std::optional<SchemeValue> mutexCreate(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    DEBUG_LOG("mutexCreate called");

    if (!args.empty()) {
        DEBUG_LOG("mutexCreate: wrong number of arguments");
        throw InterpreterError("mutex-create: takes no arguments");
    }

    DEBUG_LOG("mutexCreate: creating mutex");
    auto mutexHandle = std::make_shared<MutexHandle>();
    DEBUG_LOG("mutexCreate: returning mutex handle @ " << mutexHandle.get());

    return SchemeValue(mutexHandle);
}

std::optional<SchemeValue> mutexLock(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    DEBUG_LOG("mutexLock called");

    if (args.size() != 1) {
        DEBUG_LOG("mutexLock: wrong number of arguments");
        throw InterpreterError("mutex-lock!: requires exactly 1 argument (a mutex)");
    }

    auto mutexVal = args[0].ensureValue();
    if (!mutexVal.isMutex()) {
        DEBUG_LOG("mutexLock: argument is not a mutex");
        throw InterpreterError("mutex-lock!: argument must be a mutex");
    }

    auto mutexHandle = mutexVal.asMutex();
    DEBUG_LOG("mutexLock: got mutex handle @ " << mutexHandle.get() << ", locking");
    mutexHandle->mutex.lock();
    DEBUG_LOG("mutexLock: mutex locked");

    return std::nullopt;
}

std::optional<SchemeValue> mutexUnlock(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    DEBUG_LOG("mutexUnlock called");

    if (args.size() != 1) {
        DEBUG_LOG("mutexUnlock: wrong number of arguments");
        throw InterpreterError("mutex-unlock!: requires exactly 1 argument (a mutex)");
    }

    auto mutexVal = args[0].ensureValue();
    if (!mutexVal.isMutex()) {
        DEBUG_LOG("mutexUnlock: argument is not a mutex");
        throw InterpreterError("mutex-unlock!: argument must be a mutex");
    }

    auto mutexHandle = mutexVal.asMutex();
    DEBUG_LOG("mutexUnlock: got mutex handle @ " << mutexHandle.get() << ", unlocking");
    mutexHandle->mutex.unlock();
    DEBUG_LOG("mutexUnlock: mutex unlocked");

    return std::nullopt;
}

std::optional<SchemeValue> conditionCreate(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    DEBUG_LOG("conditionCreate called");

    if (!args.empty()) {
        DEBUG_LOG("conditionCreate: wrong number of arguments");
        throw InterpreterError("condition-variable-create: takes no arguments");
    }

    DEBUG_LOG("conditionCreate: creating condition variable");
    auto condHandle = std::make_shared<ConditionVarHandle>();
    DEBUG_LOG("conditionCreate: returning condition variable handle @ " << condHandle.get());

    return SchemeValue(condHandle);
}

std::optional<SchemeValue> conditionWait(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    DEBUG_LOG("conditionWait called from thread " << std::this_thread::get_id());

    if (args.size() != 2) {
        DEBUG_LOG("conditionWait: wrong number of arguments");
        throw InterpreterError(
            "condition-variable-wait: requires exactly 2 arguments (condition variable and mutex)");
    }

    auto condVal = args[0].ensureValue();
    auto mutexVal = args[1].ensureValue();

    if (!condVal.isConditionVar()) {
        DEBUG_LOG("conditionWait: first argument is not a condition variable");
        throw InterpreterError("condition-variable-wait: first argument must be a condition variable");
    }

    if (!mutexVal.isMutex()) {
        DEBUG_LOG("conditionWait: second argument is not a mutex");
        throw InterpreterError("condition-variable-wait: second argument must be a mutex");
    }

    auto condHandle = condVal.asConditionVar();
    auto mutexHandle = mutexVal.asMutex();

    DEBUG_LOG("conditionWait: got condition variable @ " << condHandle.get()
                                                         << " and mutex @ " << mutexHandle.get() << ", waiting");
    std::unique_lock<std::mutex> lock(mutexHandle->mutex, std::adopt_lock);
    DEBUG_LOG("conditionWait: about to wait on condition variable");
    auto status = condHandle->cv.wait_for(lock, std::chrono::seconds(30));

    if (status == std::cv_status::timeout) {
        DEBUG_LOG("conditionWait: TIMED OUT after 30 seconds");
    } else {
        DEBUG_LOG("conditionWait: condition variable wait completed normally");
    }
    lock.release();
    DEBUG_LOG("conditionWait: unique_lock released");

    return std::nullopt;
}

std::optional<SchemeValue> conditionSignal(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    DEBUG_LOG("conditionSignal called from thread " << std::this_thread::get_id());

    if (args.size() != 1) {
        DEBUG_LOG("conditionSignal: wrong number of arguments");
        throw InterpreterError("condition-variable-signal!: requires exactly 1 argument (a condition variable)");
    }

    auto condVal = args[0].ensureValue();
    if (!condVal.isConditionVar()) {
        DEBUG_LOG("conditionSignal: argument is not a condition variable");
        throw InterpreterError("condition-variable-signal!: argument must be a condition variable");
    }

    auto condHandle = condVal.asConditionVar();
    DEBUG_LOG("conditionSignal: signaling condition variable @ " << condHandle.get());
    condHandle->cv.notify_one();
    DEBUG_LOG("conditionSignal: condition variable signaled");

    return std::nullopt;
}

std::optional<SchemeValue> conditionBroadcast(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    DEBUG_LOG("conditionBroadcast called from thread " << std::this_thread::get_id());

    if (args.size() != 1) {
        DEBUG_LOG("conditionBroadcast: wrong number of arguments");
        throw InterpreterError("condition-variable-broadcast!: requires exactly 1 argument (a condition variable)");
    }

    auto condVal = args[0].ensureValue();
    if (!condVal.isConditionVar()) {
        DEBUG_LOG("conditionBroadcast: argument is not a condition variable");
        throw InterpreterError("condition-variable-broadcast!: argument must be a condition variable");
    }

    auto condHandle = condVal.asConditionVar();
    DEBUG_LOG("conditionBroadcast: broadcasting condition variable @ " << condHandle.get());
    condHandle->cv.notify_all();
    DEBUG_LOG("conditionBroadcast: condition variable broadcast complete");

    return std::nullopt;
}
} // namespace jaws_thread
