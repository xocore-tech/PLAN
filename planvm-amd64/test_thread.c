// Copyright (c) 2026 xoCore Technologies
// SPDX-License-Identifier: MIT
// See LICENSE for full terms.

#include "common.h"
#include "printf.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdnoreturn.h>

typedef uint64_t u64;

void syscall_write  (int, char*, u64);
noreturn void syscall_exit_group(u64);

// Thread context structure
typedef struct {
    int32_t tid;                     // Thread ID (kernel-assigned)
    int32_t tid_padding;
    uint32_t exit_flag;          // Synchronization: 0 = running, 1 = exited
    uint32_t padding;
    void *(*fn)(void *);         // Function to execute
    void *arg;                   // Argument for the thread function
    void *result;                // Return value from the thread function
    void *stack;                 // Allocated stack memory
    size_t stack_size;           // Size of the stack
} minimal_thread_t;

int thread_create(minimal_thread_t* thread, void* (*thread_work)(void*), void* value);
void thread_join(minimal_thread_t* thread, void** result);

// TODO: using malloc breaks because we don't do the entire glibc dance of
// setting up TLS, which malloc uses. So instead return values through a hard
// coded address.
static int result_area;

// A sample thread work function
void *thread_work(void *arg) {
    int val = *(int*)arg;
    printf("Thread running with value: %d\n", val);
    /* sleep(1);  // Simulate work */
    int *result = &result_area; // malloc(sizeof(int));
    *result = val * 2;
    return result;
}

int _start() {
    minimal_thread_t thread;
    int value = 42;
    
    // Create the thread using clone3()
    /* if (minimal_thread_create(&thread, thread_work, &value) != 0) { */
    int v = thread_create(&thread, thread_work, &value);
    if (v != 0) {
        printf("Failed to create thread: %d", v);
        return 1;
    }
    
    printf("Created thread with ID: %d\n", thread.tid);
    
    // Wait for the thread to complete
    void *ret;
    thread_join(&thread, &ret);

    printf("Thread returned: %d\n", *(int*)ret);
    /* free(ret); */

    syscall_exit_group(0);
}
