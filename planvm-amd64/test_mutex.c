// Copyright (c) 2026 xoCore Technologies
// SPDX-License-Identifier: MIT
// See LICENSE for full terms.
// Test harness for low-level mutex implementation
// Includes stress testing and correctness verification

#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <unistd.h>
#include <stdint.h>
#include <time.h>

typedef uint64_t u64;
typedef uint64_t Val;
typedef uint32_t u32;
typedef uint8_t  u8;

extern void mutex_init(u32* mutex);
extern void mutex_lock(u32* mutex);
extern void mutex_unlock(u32* mutex);

// Test structure
typedef struct {
    u32* mutex;
    int* shared_counter;
    int iterations;
    int expected_final;
    atomic_int* simultaneous_holders;
    atomic_int* max_simultaneous;
} test_context_t;

// Verification functions
bool verify_mutual_exclusion(test_context_t* ctx) {
    return atomic_load(ctx->max_simultaneous) <= 1;
}

bool verify_counter_correctness(test_context_t* ctx) {
    return *ctx->shared_counter == ctx->expected_final;
}

// Worker thread for stress testing
void* worker_thread(void* arg) {
    test_context_t* ctx = (test_context_t*)arg;

    for (int i = 0; i < ctx->iterations; i++) {
        /* printf("locking...\n"); */
        mutex_lock(ctx->mutex);
        /* printf("locked\n"); */

        // Track simultaneous holders
        int current = atomic_fetch_add(ctx->simultaneous_holders, 1);
        if (current + 1 > atomic_load(ctx->max_simultaneous)) {
            atomic_store(ctx->max_simultaneous, current + 1);
        }

        // Simulate work and modify shared state
        int temp = *ctx->shared_counter;
        usleep(rand() % 100); // Random delay to increase contention
        *ctx->shared_counter = temp + 1;

        atomic_fetch_sub(ctx->simultaneous_holders, 1);

        /* printf("unlock\n"); */
        mutex_unlock(ctx->mutex);
    }

    return NULL;
}

// Main test runner
bool run_mutex_test(int num_threads, int iterations_per_thread) {
    u32 mutex;
    mutex_init(&mutex);

    /* my_mutex_t mutex; */
    /* my_mutex_init(&mutex); */

    int shared_counter = 0;
    atomic_int simultaneous_holders = 0;
    atomic_int max_simultaneous = 0;

    test_context_t ctx = {
        .mutex = &mutex,
        .shared_counter = &shared_counter,
        .iterations = iterations_per_thread,
        .expected_final = num_threads * iterations_per_thread,
        .simultaneous_holders = &simultaneous_holders,
        .max_simultaneous = &max_simultaneous
    };

    pthread_t* threads = malloc(sizeof(pthread_t) * num_threads);

    // Launch threads
    for (int i = 0; i < num_threads; i++) {
        pthread_create(&threads[i], NULL, worker_thread, &ctx);
    }

    // Wait for completion
    for (int i = 0; i < num_threads; i++) {
        pthread_join(threads[i], NULL);
    }

    free(threads);

    // Verify results
    bool passed = true;
    if (!verify_mutual_exclusion(&ctx)) {
        printf("FAILED: Multiple threads held mutex simultaneously\n");
        passed = false;
    }

    if (!verify_counter_correctness(&ctx)) {
        printf("FAILED: Counter value incorrect. Expected %d, got %d\n",
               ctx.expected_final, *ctx.shared_counter);
        passed = false;
    }

    return passed;
}

int main() {
    srand(time(NULL));

    printf("Running mutex tests...\n");

    // Run multiple test configurations
    struct {
        int threads;
        int iterations;
    } configs[] = {
        {2, 1000},
        {4, 1000},
        {8, 1000},
        {16, 500},
        {32, 250}
    };

    for (int i = 0; i < sizeof(configs)/sizeof(configs[0]); i++) {
        printf("\nTest configuration: %d threads, %d iterations each\n",
               configs[i].threads, configs[i].iterations);

        bool passed = run_mutex_test(configs[i].threads, configs[i].iterations);
        printf("Result: %s\n", passed ? "PASSED" : "FAILED");
    }

    return 0;
}
