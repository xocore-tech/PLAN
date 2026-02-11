// Copyright (c) 2026 xoCore Technologies
// SPDX-License-Identifier: MIT
// See LICENSE for full terms.
/*
 * Barrier test program - Tests the assembly implementation of barriers
 *
 * Compiling:
 * gcc -o barrier_test barrier_test.c barrier_asm.o -lpthread
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <pthread.h>
#include <unistd.h>
#include <time.h>

/* Import the assembly functions */
extern void barrier_set(int32_t* barrier, int32_t count);
extern void barrier_done(int32_t* barrier);
extern void barrier_wait(int32_t* barrier);

/* Global variables */
int32_t global_barrier;
pthread_mutex_t print_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Thread work structure */
typedef struct {
    int thread_id;
    int work_time_ms;
} thread_args_t;

/* Helper function for getting current time in milliseconds */
uint64_t get_time_ms() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)(ts.tv_sec * 1000 + ts.tv_nsec / 1000000);
}

/* Thread function */
void* worker_thread(void* arg) {
    thread_args_t* args = (thread_args_t*)arg;
    int thread_id = args->thread_id;
    int work_time = args->work_time_ms;

    /* Print starting message */
    pthread_mutex_lock(&print_mutex);
    printf("Thread %d: Starting work (will take ~%d ms)\n", thread_id, work_time);
    pthread_mutex_unlock(&print_mutex);

    /* Simulate work by sleeping */
    uint64_t start_time = get_time_ms();
    usleep(work_time * 1000);  /* Convert ms to μs */
    
    /* Print completion message */
    pthread_mutex_lock(&print_mutex);
    printf("Thread %d: Work completed after %lu ms, signaling barrier\n", 
           thread_id, get_time_ms() - start_time);
    pthread_mutex_unlock(&print_mutex);

    /* Signal that this thread is done */
    barrier_done(&global_barrier);

    /* Wait for all threads to complete */
    pthread_mutex_lock(&print_mutex);
    printf("Thread %d: Waiting at barrier\n", thread_id);
    pthread_mutex_unlock(&print_mutex);
    
    uint64_t wait_start = get_time_ms();
    barrier_wait(&global_barrier);
    
    /* Print message after barrier */
    pthread_mutex_lock(&print_mutex);
    printf("Thread %d: Passed barrier after waiting %lu ms\n", 
           thread_id, get_time_ms() - wait_start);
    pthread_mutex_unlock(&print_mutex);

    free(args);
    return NULL;
}

int main(int argc, char** argv) {
    int num_threads = 7;  /* Default number of threads */
    int seed = time(NULL);
    
    /* Parse command line arguments */
    if (argc > 1) {
        num_threads = atoi(argv[1]);
        if (num_threads <= 0) {
            fprintf(stderr, "Invalid number of threads\n");
            return 1;
        }
    }
    
    if (argc > 2) {
        seed = atoi(argv[2]);
    }
    
    printf("Starting barrier test with %d threads (random seed: %d)\n", num_threads, seed);
    srand(seed);

    /* Initialize the barrier */
    barrier_set(&global_barrier, num_threads);
    printf("Barrier initialized with count = %d\n", num_threads);

    /* Create threads */
    pthread_t threads[num_threads];
    printf("Creating %d worker threads...\n", num_threads);
    
    for (int i = 0; i < num_threads; i++) {
        thread_args_t* args = malloc(sizeof(thread_args_t));
        if (!args) {
            perror("Failed to allocate thread arguments");
            return 1;
        }
        
        args->thread_id = i;
        /* Assign random work time between 100ms and 2000ms */
        args->work_time_ms = 100 + (rand() % 1901); 
        
        if (pthread_create(&threads[i], NULL, worker_thread, args) != 0) {
            perror("Failed to create thread");
            free(args);
            return 1;
        }
    }

    /* Wait for all threads to complete */
    for (int i = 0; i < num_threads; i++) {
        pthread_join(threads[i], NULL);
    }

    printf("All threads have completed. Test finished successfully!\n");
    return 0;
}
