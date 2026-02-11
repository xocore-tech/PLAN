// Copyright (c) 2026 xoCore Technologies
// SPDX-License-Identifier: MIT
// See LICENSE for full terms.
// Test harness for low-level condition variable implementation
// Includes basic functionality, producer-consumer, and stress testing

#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <unistd.h>
#include <time.h>
#include <assert.h>
#include <string.h>
#include <stdint.h>

typedef uint64_t u64;
typedef uint64_t Val;
typedef uint32_t u32;
typedef uint8_t  u8;

extern void mutex_init(u32* mutex);
extern void mutex_lock(u32* mutex);
extern void mutex_unlock(u32* mutex);

// Equivalent structure layout 
typedef struct {
  u32* mutex_pointer; // qword
  u32 sequence;       // dword
  u32 padding;        // unused
} cond_t;

extern void condition_init(cond_t* cv, u32* mutex);
extern void condition_signal(cond_t* cv);
extern void condition_broadcast(cond_t* cv);
extern void condition_wait(cond_t* cv);

/* void my_cond_destroy(cond_t* cond) { */
/*     // Clean up your condition variable */
/*     // ... your implementation */
/* } */

// Test contexts and structures

// Basic producer-consumer test
typedef struct {
    u32 mutex;
    cond_t cond;
    int buffer;
    bool data_ready;
    bool consumer_finished;
    bool test_passed;
} producer_consumer_ctx_t;

// Multiple waiter test
typedef struct {
    u32 mutex;
    cond_t cond;
    int counter;
    int num_threads;
    int expected_final;
    bool use_broadcast;
    bool test_passed;
    atomic_int threads_ready;
} multiple_waiter_ctx_t;

// Stress test
typedef struct {
    u32 mutex;
    cond_t cond;
    int iterations;
    int num_threads;
    int value;
    int expected_final;
    bool test_passed;
    atomic_int threads_finished;
} stress_test_ctx_t;

// Test 1: Basic producer-consumer test
void* producer_thread(void* arg) {
    producer_consumer_ctx_t* ctx = (producer_consumer_ctx_t*)arg;
    
    // Sleep briefly to ensure consumer is ready and waiting
    usleep(100000);  // 100ms
    
    mutex_lock(&ctx->mutex);
    printf("Producer: Producing data\n");
    ctx->buffer = 42;
    ctx->data_ready = true;
    
    printf("Producer: Signaling consumer\n");
    condition_signal(&ctx->cond);
    mutex_unlock(&ctx->mutex);
    
    // Wait for consumer to finish
    while (!ctx->consumer_finished) {
        usleep(10000);
    }
    
    return NULL;
}

void* consumer_thread(void* arg) {
    producer_consumer_ctx_t* ctx = (producer_consumer_ctx_t*)arg;

    /* printf("Consumer: ctx->cond=%p\n", &ctx->cond); */
    /* printf("Consumer: ctx->cond->mutex=%p\n", &ctx->cond.mutex_pointer); */
    /* printf("Consumer: ctx->cond->sequence=%p\n", &ctx->cond.sequence); */
    /* printf("Consumer: mutex=%p\n", &ctx->mutex); */
    /* printf("Consumer: mutexval=%d\n", ctx->mutex); */

    mutex_lock(&ctx->mutex);
    printf("Consumer: Waiting for data\n");
    
    while (!ctx->data_ready) {
        condition_wait(&ctx->cond);
    }
    
    printf("Consumer: Received data: %d\n", ctx->buffer);
    // Verify received data
    ctx->test_passed = (ctx->buffer == 42);
    ctx->consumer_finished = true;
    mutex_unlock(&ctx->mutex);
    
    return NULL;
}

bool run_producer_consumer_test() {
    producer_consumer_ctx_t ctx;
    
    // Initialize
    mutex_init(&ctx.mutex);
    condition_init(&ctx.cond, &ctx.mutex);
    ctx.buffer = 0;
    ctx.data_ready = false;
    ctx.consumer_finished = false;
    ctx.test_passed = false;
    
    pthread_t producer, consumer;
    
    // Create threads
    pthread_create(&consumer, NULL, consumer_thread, &ctx);
    pthread_create(&producer, NULL, producer_thread, &ctx);
    
    // Wait for completion
    pthread_join(producer, NULL);
    pthread_join(consumer, NULL);
    
    // Clean up
    /* my_cond_destroy(&ctx.cond); */
    
    printf("Producer-Consumer test %s\n", ctx.test_passed ? "PASSED" : "FAILED");
    return ctx.test_passed;
}

// Test 2: Multiple waiter test
void* waiter_thread(void* arg) {
    multiple_waiter_ctx_t* ctx = (multiple_waiter_ctx_t*)arg;
    
    // Signal that this thread is ready
    atomic_fetch_add(&ctx->threads_ready, 1);
    
    mutex_lock(&ctx->mutex);
    // Wait to be signaled
    condition_wait(&ctx->cond);
    
    // Increment counter after being signaled
    ctx->counter++;
    printf("Waiter thread: Incrementing counter to %d\n", ctx->counter);
    
    mutex_unlock(&ctx->mutex);
    return NULL;
}

void* signal_thread(void* arg) {
    multiple_waiter_ctx_t* ctx = (multiple_waiter_ctx_t*)arg;

    /* printf("Signal: ctx->cond=%p\n", &ctx->cond); */
    /* printf("Signal: ctx->cond->mutex=%p\n", &ctx->cond.mutex_pointer); */
    /* printf("Signal: ctx->cond->sequence=%p\n", &ctx->cond.sequence); */
    /* printf("Signal: mutex=%p\n", &ctx->mutex); */
    /* printf("Signal: mutexval=%d\n", ctx->mutex); */

    
    // Wait for all threads to be ready and waiting
    while (atomic_load(&ctx->threads_ready) < ctx->num_threads) {
        usleep(10000);  // 10ms
    }
    
    // Give additional time for threads to actually enter wait state
    usleep(200000);  // 200ms
    
    printf("Signal thread: All waiter threads ready, now signaling\n");
    
    if (ctx->use_broadcast) {
        // Broadcast variant
        mutex_lock(&ctx->mutex);
        condition_broadcast(&ctx->cond);
        mutex_unlock(&ctx->mutex);
        printf("Signal thread: Broadcast sent\n");
    } else {
        // Individual signal variant
        for (int i = 0; i < ctx->num_threads; i++) {
            mutex_lock(&ctx->mutex);
            condition_signal(&ctx->cond);
            mutex_unlock(&ctx->mutex);
            
            printf("Signal thread: Signal %d sent\n", i + 1);
            usleep(50000);  // 50ms between signals
        }
    }
    
    // Give time for all threads to process
    usleep(500000);  // 500ms
    
    // Check the final counter value
    mutex_lock(&ctx->mutex);
    ctx->test_passed = (ctx->counter == ctx->expected_final);
    mutex_unlock(&ctx->mutex);
    
    return NULL;
}

bool run_multiple_waiter_test(int num_waiter_threads, bool use_broadcast) {
    multiple_waiter_ctx_t ctx;
    
    // Initialize
    mutex_init(&ctx.mutex);
    condition_init(&ctx.cond, &ctx.mutex);
    ctx.counter = 0;
    ctx.num_threads = num_waiter_threads;
    ctx.expected_final = num_waiter_threads;
    ctx.use_broadcast = use_broadcast;
    ctx.test_passed = false;
    atomic_init(&ctx.threads_ready, 0);
    
    pthread_t* waiters = malloc(sizeof(pthread_t) * num_waiter_threads);
    pthread_t signaler;
    
    // Create waiter threads
    for (int i = 0; i < num_waiter_threads; i++) {
        pthread_create(&waiters[i], NULL, waiter_thread, &ctx);
    }
    
    // Create signaler thread
    pthread_create(&signaler, NULL, signal_thread, &ctx);
    
    // Wait for completion
    for (int i = 0; i < num_waiter_threads; i++) {
        pthread_join(waiters[i], NULL);
    }
    pthread_join(signaler, NULL);
    
    free(waiters);
    /* my_cond_destroy(&ctx.cond); */
    
    printf("Multiple waiter test (%s): %s\n", 
           use_broadcast ? "broadcast" : "signal", 
           ctx.test_passed ? "PASSED" : "FAILED");
    
    return ctx.test_passed;
}

// Main test runner
int main() {
    srand(time(NULL));
    
    printf("==== Condition Variable Test Harness ====\n\n");
    
    // Track overall pass/fail status
    bool all_tests_passed = true;
    
    // Test 1: Basic producer-consumer test
    printf("\n=== Test 1: Producer-Consumer Test ===\n");
    all_tests_passed &= run_producer_consumer_test();
    
    // Test 2a: Multiple waiters with individual signals
    printf("\n=== Test 2a: Multiple Waiters (Signal) ===\n");
    all_tests_passed &= run_multiple_waiter_test(5, false);
    
    // Test 2b: Multiple waiters with broadcast
    printf("\n=== Test 2b: Multiple Waiters (Broadcast) ===\n");
    all_tests_passed &= run_multiple_waiter_test(5, true);
    
    // Print overall result
    printf("\n==== Overall Test Result: %s ====\n", 
           all_tests_passed ? "PASSED" : "FAILED");
    
    return all_tests_passed ? 0 : 1;
}
