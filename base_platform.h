/* See LICENSE for license details. */
#ifndef BASE_PLATFORM_H
#define BASE_PLATFORM_H

#ifndef BASE_EXPORT
#define BASE_EXPORT
#endif

#ifndef BASE_IMPORT
#define BASE_IMPORT
#endif

#ifndef BASE_PLATFORM_NO_MAIN
#define BASE_PLATFORM_NO_MAIN 0
#endif

#include "base_types.h"
#include "base_intrinsics.h"

#define OSInvalidHandleValue ((u64)-1)
typedef struct { u64 value[1]; } OSBarrier;
typedef struct { u64 value[1]; } OSHandle;
typedef struct { u64 value[1]; } OSLibrary;
typedef struct { u64 value[1]; } OSThread;
typedef struct { u64 value[1]; } OSWindow;
typedef struct { u64 value[1]; } OSW32Semaphore;

typedef u64 os_thread_entry_point_fn(void *user_context);

typedef struct {
	u64 timer_frequency;

	u32 logical_processor_count;
	u32 page_size;

	u8  path_separator_byte;
} OSSystemInfo;

BASE_EXPORT OSSystemInfo * os_system_info(void);

BASE_EXPORT void no_return os_exit(i32 code);

BASE_EXPORT void *         os_memory_reserve(u64 size);
BASE_EXPORT void           os_memory_release(void *base, u64 size);
BASE_EXPORT u32            os_memory_commit(void *base, u64 size);
BASE_EXPORT void           os_memory_uncommit(void *base, u64 size);
BASE_EXPORT void           os_memory_seal(void *base, u64 size);

BASE_EXPORT u64            os_timer_count(void);

BASE_EXPORT str8           os_read_entire_file(Arena *arena, const char *file);

/* NOTE(rnp): memory watch timed waiting functions. (-1) is an infinite timeout.
 * Used with the intention of yielding the thread back to the OS. */
BASE_EXPORT u32            os_wait_on_address(i32 *lock, i32 current, u32 timeout_ms);
BASE_EXPORT void           os_wake_all_waiters(i32 *lock);


/* NOTE(rnp): this functionality is only needed on win32 to provide cross process
 * synchronization. While posix has equivalent functionality there is no reason to
 * use it over a value located in shared memory. */
#if OS_WINDOWS
BASE_EXPORT OSW32Semaphore os_w32_create_semaphore(const char *name, i32 initial_count, i32 maximum_count);
BASE_EXPORT u32            os_w32_semaphore_wait(OSW32Semaphore, u32 timeout_ms);
BASE_EXPORT void           os_w32_semaphore_release(OSW32Semaphore, i32 count);
#endif

#endif /* BASE_PLATFORM_H */
