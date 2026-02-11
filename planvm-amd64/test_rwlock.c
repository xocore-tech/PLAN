// Copyright (c) 2026 xoCore Technologies
// SPDX-License-Identifier: MIT
// See LICENSE for full terms.
// Test harness for low-level rwlock implementation
// Tests correctness, contention handling, and starvation prevention

#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <unistd.h>
#include <time.h>
#include <string.h>

// External declarations for the assembly functions
extern void rwlock_init(void* lock);
extern void rwlock_read_lock(void* lock);
extern void rwlock_read_unlock(void* lock);
extern void rwlock_write_lock(void* lock);
extern void rwlock_write_unlock(void* lock);

// Structure to match the 56-byte rwlock implementation
typedef struct {
    char data[56]; // Internal structure managed by assembly implementation
} my_rwlock_t;

// Test context for readers/writers
typedef struct {
    my_rwlock_t* rwlock;
    int* shared_data;           // Data protected by the lock
    int iterations;             // Number of operations per thread
    atomic_int* active_readers; // Count of currently active readers
    atomic_int* active_writers; // Count of currently active writers
    atomic_int* max_readers;    // Max simultaneous readers observed
    atomic_int* max_writers;    // Should always be 1 or 0
    atomic_int* read_ops;       // Total completed read operations
    atomic_int* write_ops;      // Total completed write operations
    int thread_id;              // Thread identifier
    int seed;                   // Random seed for this thread
} test_context_t;

// Reader thread function
void* reader_thread(void* arg) {
    test_context_t* ctx = (test_context_t*)arg;
    int local_sum = 0;
    
    // Seed the random generator uniquely for this thread
    srand(ctx->seed);
    
    for (int i = 0; i < ctx->iterations; i++) {
        // Acquire read lock
        rwlock_read_lock(ctx->rwlock);
        
        // Track active readers
        int current = atomic_fetch_add(ctx->active_readers, 1);
        
        // Update max readers if we've seen more
        int current_max = atomic_load(ctx->max_readers);
        while (current + 1 > current_max) {
            if (atomic_compare_exchange_weak(ctx->max_readers, &current_max, current + 1)) {
                break;
            }
        }
        
        // Check no writers are active (would be a violation)
        if (atomic_load(ctx->active_writers) > 0) {
            printf("ERROR: Reader detected active writer!\n");
        }
        
        // Read the shared data
        local_sum += *ctx->shared_data;
        
        // Simulate some work
        usleep(rand() % 200);
        
        // Decrement active readers
        atomic_fetch_sub(ctx->active_readers, 1);
        
        // Increment completed read operations
        atomic_fetch_add(ctx->read_ops, 1);
        
        // Release read lock
        rwlock_read_unlock(ctx->rwlock);
        
        // Do some work outside the lock
        usleep(rand() % 300);
    }
    
    return NULL;
}

// Writer thread function
void* writer_thread(void* arg) {
    test_context_t* ctx = (test_context_t*)arg;
    
    // Seed the random generator uniquely for this thread
    srand(ctx->seed);
    
    for (int i = 0; i < ctx->iterations; i++) {
        // Acquire write lock
        rwlock_write_lock(ctx->rwlock);
        
        // Track active writers
        int current = atomic_fetch_add(ctx->active_writers, 1);
        
        // Should never have more than one active writer
        if (current > 0) {
            printf("ERROR: Multiple writers active simultaneously!\n");
        }
        
        // Update max writers if needed (should always be 1)
        int current_max = atomic_load(ctx->max_writers);
        while (current + 1 > current_max) {
            if (atomic_compare_exchange_weak(ctx->max_writers, &current_max, current + 1)) {
                break;
            }
        }
        
        // Check no readers are active (would be a violation)
        if (atomic_load(ctx->active_readers) > 0) {
            printf("ERROR: Writer detected active readers!\n");
        }
        
        // Modify the shared data
        (*ctx->shared_data)++;
        
        // Simulate some work in the critical section
        usleep(rand() % 500);
        
        // Decrement active writers
        atomic_fetch_sub(ctx->active_writers, 1);
        
        // Increment completed write operations
        atomic_fetch_add(ctx->write_ops, 1);
        
        // Release write lock
        rwlock_write_unlock(ctx->rwlock);
        
        // Do some work outside the lock
        usleep(rand() % 1000);
    }
    
    return NULL;
}

// Run a test with the given number of readers and writers
bool run_rwlock_test(int num_readers, int num_writers, int iterations, bool writer_preference) {
    // Initialize the rwlock
    my_rwlock_t rwlock;
    memset(&rwlock, 0, sizeof(rwlock)); // Zero-initialize before passing to init
    rwlock_init(&rwlock);
    
    // Shared data and counters
    int shared_data = 0;
    atomic_int active_readers = 0;
    atomic_int active_writers = 0;
    atomic_int max_readers = 0;
    atomic_int max_writers = 0;
    atomic_int read_ops = 0;
    atomic_int write_ops = 0;
    
    // Prepare thread handles
    pthread_t* reader_threads = malloc(sizeof(pthread_t) * num_readers);
    pthread_t* writer_threads = malloc(sizeof(pthread_t) * num_writers);
    
    // Prepare thread contexts
    test_context_t* reader_ctx = malloc(sizeof(test_context_t) * num_readers);
    test_context_t* writer_ctx = malloc(sizeof(test_context_t) * num_writers);
    
    // Initialize and start reader threads
    for (int i = 0; i < num_readers; i++) {
        reader_ctx[i] = (test_context_t) {
            .rwlock = &rwlock,
            .shared_data = &shared_data,
            .iterations = iterations,
            .active_readers = &active_readers,
            .active_writers = &active_writers,
            .max_readers = &max_readers,
            .max_writers = &max_writers,
            .read_ops = &read_ops,
            .write_ops = &write_ops,
            .thread_id = i,
            .seed = time(NULL) + i
        };
        
        pthread_create(&reader_threads[i], NULL, reader_thread, &reader_ctx[i]);
    }
    
    // Start writers after readers if testing for writer starvation
    if (!writer_preference) {
        usleep(500000); // 500ms head start for readers
    }
    
    // Initialize and start writer threads
    for (int i = 0; i < num_writers; i++) {
        writer_ctx[i] = (test_context_t) {
            .rwlock = &rwlock,
            .shared_data = &shared_data,
            .iterations = iterations,
            .active_readers = &active_readers,
            .active_writers = &active_writers,
            .max_readers = &max_readers,
            .max_writers = &max_writers,
            .read_ops = &read_ops,
            .write_ops = &write_ops,
            .thread_id = i,
            .seed = time(NULL) + num_readers + i
        };
        
        pthread_create(&writer_threads[i], NULL, writer_thread, &writer_ctx[i]);
    }
    
    // Wait for all threads to complete
    for (int i = 0; i < num_readers; i++) {
        pthread_join(reader_threads[i], NULL);
    }
    
    for (int i = 0; i < num_writers; i++) {
        pthread_join(writer_threads[i], NULL);
    }
    
    // Free allocated resources
    free(reader_threads);
    free(writer_threads);
    free(reader_ctx);
    free(writer_ctx);
    
    // Verify results
    bool passed = true;
    
    // Check if shared_data matches expected writes
    int expected_value = num_writers * iterations;
    if (shared_data != expected_value) {
        printf("ERROR: Shared data doesn't match expected value.\n");
        printf("Expected: %d, Actual: %d\n", expected_value, shared_data);
        passed = false;
    }
    
    // Check that max_writers never exceeded 1
    if (atomic_load(&max_writers) > 1) {
        printf("ERROR: Multiple writers held lock simultaneously!\n");
        passed = false;
    }
    
    // Check that all ops completed
    int expected_read_ops = num_readers * iterations;
    int expected_write_ops = num_writers * iterations;
    
    if (atomic_load(&read_ops) != expected_read_ops) {
        printf("ERROR: Not all read operations completed.\n");
        printf("Expected: %d, Actual: %d\n", expected_read_ops, atomic_load(&read_ops));
        passed = false;
    }
    
    if (atomic_load(&write_ops) != expected_write_ops) {
        printf("ERROR: Not all write operations completed.\n");
        printf("Expected: %d, Actual: %d\n", expected_write_ops, atomic_load(&write_ops));
        passed = false;
    }
    
    return passed;
}

// Structure for continuous reader
typedef struct {
    test_context_t base_ctx;
    atomic_bool* test_complete;
} continuous_reader_ctx_t;

// Continuous reader thread function (moved outside to avoid nested function issues)
void* continuous_reader(void* arg) {
    continuous_reader_ctx_t* ctx = (continuous_reader_ctx_t*)arg;
    
    // Seed random generator for this thread
    srand(ctx->base_ctx.seed);
    
    while (!atomic_load(ctx->test_complete)) {
        rwlock_read_lock(ctx->base_ctx.rwlock);
        
        // Track active readers
        atomic_fetch_add(ctx->base_ctx.active_readers, 1);
        
        // Update max readers if needed
        int current = atomic_load(ctx->base_ctx.active_readers);
        int current_max = atomic_load(ctx->base_ctx.max_readers);
        while (current > current_max) {
            if (atomic_compare_exchange_weak(ctx->base_ctx.max_readers, &current_max, current)) {
                break;
            }
        }
        
        // Read and simulate work
        int value = *ctx->base_ctx.shared_data;
        (void)value; // Unused, but prevents optimization
        usleep(rand() % 100);
        
        atomic_fetch_sub(ctx->base_ctx.active_readers, 1);
        atomic_fetch_add(ctx->base_ctx.read_ops, 1);
        
        rwlock_read_unlock(ctx->base_ctx.rwlock);
        
        // Small pause before next read
        usleep(rand() % 50);
    }
    
    return NULL;
}

// Test for writer starvation - run many readers continuously and see if writers get a turn
bool test_writer_starvation(int num_continuous_readers, int num_writers, int writer_iterations) {
    // Initialize the rwlock
    my_rwlock_t rwlock;
    memset(&rwlock, 0, sizeof(rwlock));
    rwlock_init(&rwlock);
    
    // Shared data and counters
    int shared_data = 0;
    atomic_int active_readers = 0;
    atomic_int active_writers = 0;
    atomic_int max_readers = 0;
    atomic_int max_writers = 0;
    atomic_int read_ops = 0;
    atomic_int write_ops = 0;
    atomic_bool test_complete = false;
    
    // Create continuous readers
    pthread_t* reader_threads = malloc(sizeof(pthread_t) * num_continuous_readers);
    if (!reader_threads) {
        printf("ERROR: Failed to allocate memory for reader threads\n");
        return false;
    }
    
    continuous_reader_ctx_t* reader_ctx = malloc(sizeof(continuous_reader_ctx_t) * num_continuous_readers);
    if (!reader_ctx) {
        printf("ERROR: Failed to allocate memory for reader contexts\n");
        free(reader_threads);
        return false;
    }
    
    // Initialize all reader contexts before creating threads
    for (int i = 0; i < num_continuous_readers; i++) {
        memset(&reader_ctx[i].base_ctx, 0, sizeof(test_context_t));
        reader_ctx[i].base_ctx.rwlock = &rwlock;
        reader_ctx[i].base_ctx.shared_data = &shared_data;
        reader_ctx[i].base_ctx.active_readers = &active_readers;
        reader_ctx[i].base_ctx.active_writers = &active_writers;
        reader_ctx[i].base_ctx.max_readers = &max_readers;
        reader_ctx[i].base_ctx.max_writers = &max_writers;
        reader_ctx[i].base_ctx.read_ops = &read_ops;
        reader_ctx[i].base_ctx.write_ops = &write_ops;
        reader_ctx[i].base_ctx.thread_id = i;
        reader_ctx[i].base_ctx.seed = (unsigned int)(time(NULL) + i);
        reader_ctx[i].test_complete = &test_complete;
    }
    
    printf("  - Creating %d continuous reader threads...\n", num_continuous_readers);
    fflush(stdout);
    
    for (int i = 0; i < num_continuous_readers; i++) {
        if (pthread_create(&reader_threads[i], NULL, continuous_reader, &reader_ctx[i]) != 0) {
            printf("ERROR: Failed to create reader thread %d\n", i);
            
            // Signal existing threads to terminate
            atomic_store(&test_complete, true);
            
            // Wait for any threads that did start
            for (int j = 0; j < i; j++) {
                pthread_join(reader_threads[j], NULL);
            }
            
            free(reader_threads);
            free(reader_ctx);
            return false;
        }
    }
    
    printf("  - All continuous reader threads created successfully\n");
    fflush(stdout);
    
    // Give readers a head start
    usleep(500000);
    
    // Start writer threads
    pthread_t* writer_threads = malloc(sizeof(pthread_t) * num_writers);
    if (!writer_threads) {
        printf("ERROR: Failed to allocate memory for writer threads\n");
        atomic_store(&test_complete, true);
        for (int i = 0; i < num_continuous_readers; i++) {
            pthread_join(reader_threads[i], NULL);
        }
        free(reader_threads);
        free(reader_ctx);
        return false;
    }
    
    test_context_t* writer_ctx = malloc(sizeof(test_context_t) * num_writers);
    if (!writer_ctx) {
        printf("ERROR: Failed to allocate memory for writer contexts\n");
        atomic_store(&test_complete, true);
        for (int i = 0; i < num_continuous_readers; i++) {
            pthread_join(reader_threads[i], NULL);
        }
        free(reader_threads);
        free(reader_ctx);
        free(writer_threads);
        return false;
    }
    
    printf("  - Creating %d writer threads...\n", num_writers);
    fflush(stdout);
    
    // Initialize all writer contexts before creating threads
    for (int i = 0; i < num_writers; i++) {
        memset(&writer_ctx[i], 0, sizeof(test_context_t));
        writer_ctx[i].rwlock = &rwlock;
        writer_ctx[i].shared_data = &shared_data;
        writer_ctx[i].iterations = writer_iterations;
        writer_ctx[i].active_readers = &active_readers;
        writer_ctx[i].active_writers = &active_writers;
        writer_ctx[i].max_readers = &max_readers;
        writer_ctx[i].max_writers = &max_writers;
        writer_ctx[i].read_ops = &read_ops;
        writer_ctx[i].write_ops = &write_ops;
        writer_ctx[i].thread_id = i;
        writer_ctx[i].seed = (unsigned int)(time(NULL) + num_continuous_readers + i);
    }
    
    for (int i = 0; i < num_writers; i++) {
        if (pthread_create(&writer_threads[i], NULL, writer_thread, &writer_ctx[i]) != 0) {
            printf("ERROR: Failed to create writer thread %d\n", i);
            
            // Signal all threads to terminate
            atomic_store(&test_complete, true);
            
            // Wait for reader threads
            for (int j = 0; j < num_continuous_readers; j++) {
                pthread_join(reader_threads[j], NULL);
            }
            
            // Wait for any writer threads that did start
            for (int j = 0; j < i; j++) {
                pthread_join(writer_threads[j], NULL);
            }
            
            free(reader_threads);
            free(writer_threads);
            free(reader_ctx);
            free(writer_ctx);
            return false;
        }
    }
    
    printf("  - All writer threads created successfully\n");
    fflush(stdout);
    
    // Wait for writers to complete
    for (int i = 0; i < num_writers; i++) {
        pthread_join(writer_threads[i], NULL);
    }
    
    // Signal continuous readers to stop
    atomic_store(&test_complete, true);
    
    // Wait for readers to complete
    for (int i = 0; i < num_continuous_readers; i++) {
        pthread_join(reader_threads[i], NULL);
    }
    
    // Free resources
    free(reader_threads);
    free(writer_threads);
    free(reader_ctx);
    free(writer_ctx);
    
    // Check if all write operations completed
    int expected_write_ops = num_writers * writer_iterations;
    bool passed = (atomic_load(&write_ops) == expected_write_ops);
    
    if (!passed) {
        printf("ERROR: Writer starvation detected! Only %d of %d write operations completed.\n",
               atomic_load(&write_ops), expected_write_ops);
    }
    
    return passed;
}

int main() {
    printf("Running RWLock Test Suite\n");
    printf("=========================\n\n");
    
    // Test configurations
    struct {
        const char* name;
        int readers;
        int writers;
        int iterations;
        bool writer_preference;
    } configs[] = {
        {"Basic RWLock Test (2R, 1W)", 2, 1, 100, false},
        {"High Concurrency (10R, 3W)", 10, 3, 100, false},
        {"Writer Heavy (2R, 5W)", 2, 5, 50, false},
        {"Reader Heavy (8R, 1W)", 8, 1, 50, false},
        {"Balanced Load (5R, 5W)", 5, 5, 50, false}
    };
    
    int passed = 0;
    int total = sizeof(configs) / sizeof(configs[0]);
    
    for (int i = 0; i < total; i++) {
        printf("Test %d: %s\n", i + 1, configs[i].name);
        printf("  - Readers: %d, Writers: %d, Iterations: %d\n", 
               configs[i].readers, configs[i].writers, configs[i].iterations);
        
        bool result = run_rwlock_test(
            configs[i].readers,
            configs[i].writers,
            configs[i].iterations,
            configs[i].writer_preference
        );
        
        printf("  - Result: %s\n\n", result ? "PASSED" : "FAILED");
        if (result) passed++;
    }
    
    // Writer starvation test
    printf("Writer Starvation Test\n");
    printf("  - Continuous Readers: 8, Writers: 2, Writer Iterations: 10\n");
    
    // Catch any segfaults or issues in the starvation test
    printf("  - Starting starvation test...\n");
    fflush(stdout); // Ensure output is visible if crash occurs
    
    bool starvation_result = test_writer_starvation(8, 2, 10);
    printf("  - Result: %s\n\n", starvation_result ? "PASSED" : "FAILED");
    if (starvation_result) passed++;
    
    printf("Test Summary: %d of %d tests passed\n", passed, total + 1);
    
    return (passed == total + 1) ? 0 : 1;
}
