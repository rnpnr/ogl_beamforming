/* See LICENSE for license details. */

/* NOTE(rnp): provides the platform layer for everything in this repo. */

#define OS_SHARED_MEMORY_NAME  "/ogl_beamformer_shared_memory"

#define OS_PATH_SEPARATOR_CHAR '/'
#define OS_PATH_SEPARATOR      "/"

#include "base_platform.h"

#include "util.h"

#include <errno.h>
#include <fcntl.h>
#include <linux/futex.h>
#include <poll.h>
#include <pthread.h>
#include <sys/auxv.h>
#include <sys/inotify.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/sysinfo.h>
#include <unistd.h>

typedef struct OSLinuxEntity OSLinuxEntity;
typedef struct {
	void          *handle;
	OSLinuxEntity *prev, *next;
} OSLinuxWindow;

typedef enum {
	OSLinuxFileWatchKind_Platform,
	OSLinuxFileWatchKind_User,
} OSLinuxFileWatchKind;

typedef struct OSLinuxFileWatchDirectory OSLinuxFileWatchDirectory;
typedef struct OSLinuxFileWatch OSLinuxFileWatch;
struct OSLinuxFileWatch {
	OSLinuxFileWatchKind  kind;
	u64                   hash;
	u64                   update_time;
	void                 *user_context;

	OSLinuxFileWatchDirectory *parent;
	OSLinuxFileWatch *prev, *next;
};

struct OSLinuxFileWatchDirectory {
	u64  hash;
	i64  handle;
	str8 name;

	OSLinuxFileWatch *first_child;
	OSLinuxFileWatch *last_child;
	OSLinuxFileWatchDirectory *prev, *next;
};

typedef enum {
	OSLinuxEntityKind_Window,
	OSLinuxEntityKind_FileWatch,
	OSLinuxEntityKind_FileWatchDirectory,
} OSLinuxEntityKind;

struct OSLinuxEntity {
	OSLinuxEntityKind kind;
	union {
		OSLinuxFileWatch          file_watch;
		OSLinuxFileWatchDirectory file_watch_directory;
		OSLinuxWindow             window;
	} as;
	OSLinuxEntity *next;
};

typedef struct {
	OSSystemInfo  system_info;

	Arena        *arena;
	i32           arena_lock;

	i32           inotify_handle;

	struct {
		OSLinuxFileWatchDirectory *first;
		OSLinuxFileWatchDirectory *last;
	} file_watch_directories;

	struct {
		OSLinuxEntity *first;
		OSLinuxEntity *last;
	} windows;

	OSLinuxEntity *entity_freelist;
} OSLinux_Context;
global OSLinux_Context os_linux_context;

function b32
os_write_file(i32 file, void *data, i64 length)
{
	i64 offset = 0;
	while (offset < length) {
		i64 r = write(file, (u8 *)data + offset, (u64)(length - offset));
		if (r < 0 && errno != EINTR) break;
		if (r >= 0) offset += r;
	}
	return offset == length;
}

BASE_EXPORT no_return void
os_exit(i32 code)
{
	_exit(code);
	unreachable();
}

function u64
os_timer_frequency(void)
{
	return 1000000000ULL;
}

BASE_EXPORT u64
os_timer_count(void)
{
	struct timespec time = {0};
	clock_gettime(CLOCK_MONOTONIC, &time);
	u64 result = (u64)time.tv_sec * 1000000000ULL + (u64)time.tv_nsec;
	return result;
}

function u64
os_number_of_processors(void)
{
	u64 set[128 / sizeof(u64)] = {0};
	syscall(SYS_sched_getaffinity, 0, sizeof(set), set);

	u64 result = 0;
	for EachElement(set, it)
		result += popcount_u64(set[it]);
	return result > 0 ? result : 1;
}

function void
os_system_info_init(void)
{
	os_linux_context.system_info.timer_frequency         = os_timer_frequency();
	os_linux_context.system_info.logical_processor_count = os_number_of_processors();
	os_linux_context.system_info.page_size               = ARCH_X64? KB(4) : getauxval(AT_PAGESZ);
	os_linux_context.system_info.path_separator_byte     = '/';
}

BASE_EXPORT OSSystemInfo *
os_system_info(void)
{
	#if BASE_PLATFORM_NO_MAIN
	if unlikely(os_linux_context.system_info.path_separator_byte == 0)
		os_system_info_init();
	#endif
	return &os_linux_context.system_info;
}

BASE_EXPORT void *
os_memory_reserve(u64 size)
{
	void *result = mmap(0, size, PROT_NONE, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);
	if (result == MAP_FAILED)
		result = 0;
	return result;
}

BASE_EXPORT void
os_memory_release(void *base, u64 size)
{
	munmap(base, size);
}

BASE_EXPORT b32
os_memory_commit(void *base, u64 size)
{
	mprotect(base, size, PROT_READ|PROT_WRITE);
	return 1;
}

BASE_EXPORT void
os_memory_uncommit(void *base, u64 size)
{
	madvise(base, size, MADV_DONTNEED);
	mprotect(base, size, PROT_NONE);
}

BASE_EXPORT void
os_memory_seal(void *base, u64 size)
{
	mprotect(base, size, PROT_READ);
}

BASE_EXPORT str8
os_read_entire_file(Arena *arena, const char *file)
{
	str8 result = {0};
	struct stat sb;
	i32 fd = open(file, O_RDONLY);
	if (fd >= 0 && fstat(fd, &sb) >= 0) {
		result.data = push_array(arena, u8, sb.st_size);
		do {
			i64 rlen = read(fd, result.data + result.length, (u64)(sb.st_size - result.length));
			if (rlen > 0) result.length += rlen;
		} while (result.length != sb.st_size && errno != EINTR);
		if (result.length != sb.st_size) {
			arena_pop(arena, sb.st_size);
			zero_struct(&result);
		}
	}
	if (fd >= 0) close(fd);

	return result;
}

function b32
os_write_new_file(char *fname, str8 raw)
{
	b32 result = 0;
	i32 fd = open(fname, O_WRONLY|O_TRUNC|O_CREAT, 0600);
	if (fd != INVALID_FILE) {
		result = os_write_file(fd, raw.data, raw.length);
		close(fd);
	}
	return result;
}

function b32
os_file_exists(char *path)
{
	struct stat st;
	b32 result = stat(path, &st) == 0;
	return result;
}

/* NOTE: complete garbage because there is no standarized copyfile() in POSix */
function b32
os_copy_file(char *name, char *new)
{
	b32 result = 0;
	struct stat sb;
	if (stat(name, &sb) == 0) {
		i32 fd_old = open(name, O_RDONLY);
		i32 fd_new = open(new,  O_WRONLY|O_CREAT, sb.st_mode);
		if (fd_old >= 0 && fd_new >= 0) {
			u8 buf[4096];
			i64 copied = 0;
			while (copied != sb.st_size) {
				i64 r = read(fd_old, buf, countof(buf));
				if (r < 0) break;
				i64 w = write(fd_new, buf, (u64)r);
				if (w < 0) break;
				copied += w;
			}
			result = copied == sb.st_size;
		}
		if (fd_old != -1) close(fd_old);
		if (fd_new != -1) close(fd_new);
	}
	return result;
}

function OSLinuxEntity *
os_entity_allocate(OSLinuxEntityKind kind)
{
	OSLinuxEntity *result = 0;
	DeferLoop(take_lock(&os_linux_context.arena_lock, -1), release_lock(&os_linux_context.arena_lock))
	{
		result = SLLPopFreelist(os_linux_context.entity_freelist);
		if (!result) result = push_struct_no_zero(os_linux_context.arena, OSLinuxEntity);
	}

	zero_struct(result);
	result->kind = kind;
	return result;
}

BASE_EXPORT OSThread
os_create_thread(const char *name, void *user_context, os_thread_entry_point_fn *fn)
{
	pthread_t thread;
	pthread_create(&thread, 0, (void *)fn, (void *)user_context);

	if (name) {
		char buffer[16];
		str8 name_str = str8_from_c_str((char *)name);
		u64  length   = (u64)Clamp(name_str.length, 0, countof(buffer) - 1);
		memory_copy(buffer, (char *)name, length);
		buffer[length] = 0;
		pthread_setname_np(thread, buffer);
	}

	OSThread result = {(u64)thread};
	return result;
}

BASE_EXPORT OSBarrier
os_barrier_alloc(u32 count)
{
	OSBarrier result = {0};
	DeferLoop(take_lock(&os_linux_context.arena_lock, -1), release_lock(&os_linux_context.arena_lock))
	{
		pthread_barrier_t *barrier = push_struct(os_linux_context.arena, pthread_barrier_t);
		pthread_barrier_init(barrier, 0, count);
		result.value[0] = (u64)barrier;
	}
	return result;
}

BASE_EXPORT void
os_barrier_enter(OSBarrier barrier)
{
	pthread_barrier_t *b = (pthread_barrier_t *)barrier.value[0];
	if (b) pthread_barrier_wait(b);
}

BASE_EXPORT b32
os_wait_on_address(i32 *value, i32 current, u32 timeout_ms)
{
	struct timespec *timeout = 0, timeout_value;
	if (timeout_ms != (u32)-1) {
		timeout_value.tv_sec  = timeout_ms / 1000;
		timeout_value.tv_nsec = (timeout_ms % 1000) * 1000000;
		timeout = &timeout_value;
	}
	return syscall(SYS_futex, value, FUTEX_WAIT, current, timeout, 0, 0) == 0;
}

BASE_EXPORT void
os_wake_all_waiters(i32 *sync)
{
	if (sync) {
		atomic_store_u32(sync, 0);
		syscall(SYS_futex, sync, FUTEX_WAKE, I32_MAX, 0, 0, 0);
	}
}

#if !BASE_PLATFORM_NO_MAIN
BASE_IMPORT void entry_point(i32 argc, char *argv[]);

extern i32
main(i32 argc, char *argv[])
{
	os_system_info_init();
	os_linux_context.arena = arena_create(.name = "Platform Arena");

	entry_point(argc, argv);

	return 0;
}
#endif
