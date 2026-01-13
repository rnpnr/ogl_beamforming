/* See LICENSE for license details. */

/* NOTE(rnp): provides the platform layer for the beamformer. This code must
 * be provided by any platform the beamformer is ported to. */

#define OS_SHARED_MEMORY_NAME "/ogl_beamformer_shared_memory"

#define OS_PATH_SEPARATOR_CHAR '/'
#define OS_PATH_SEPARATOR      "/"

#include "util.h"

#include <fcntl.h>
#include <linux/futex.h>
#include <poll.h>
#include <pthread.h>
#include <sys/inotify.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/sysinfo.h>
#include <unistd.h>

typedef enum {
	OSLinux_FileWatchKindPlatform,
	OSLinux_FileWatchKindUser,
} OSLinux_FileWatchKind;

typedef struct {
	OSLinux_FileWatchKind kind;
	u64                   hash;
	u64                   update_time;
	void *                user_context;
} OSLinux_FileWatch;

typedef struct {
	u64  hash;
	iptr handle;
	s8   name;

	OSLinux_FileWatch * data;
	iz                  count;
	iz                  capacity;
} OSLinux_FileWatchDirectory;
DA_STRUCT(OSLinux_FileWatchDirectory, OSLinux_FileWatchDirectory);

typedef struct {
	Arena         arena;
	i32           arena_lock;
	i32           inotify_handle;
	OS_SystemInfo system_info;

	OSLinux_FileWatchDirectoryList file_watch_list;
} OSLinux_Context;
global OSLinux_Context os_linux_context;

function OS_WRITE_FILE_FN(os_write_file)
{
	while (raw.len > 0) {
		iz r = write((i32)file, raw.data, (uz)raw.len);
		if (r < 0) return 0;
		raw = s8_cut_head(raw, r);
	}
	return 1;
}

function void __attribute__((noreturn))
os_exit(i32 code)
{
	_exit(code);
	unreachable();
}

function void __attribute__((noreturn))
os_fatal(s8 msg)
{
	os_write_file(STDERR_FILENO, msg);
	os_exit(1);
	unreachable();
}

function iptr
os_error_handle(void)
{
	return STDERR_FILENO;
}

function s8
os_path_separator(void)
{
	return s8("/");
}

function u64
os_get_timer_frequency(void)
{
	return 1000000000ULL;
}

function u64
os_get_timer_counter(void)
{
	struct timespec time = {0};
	clock_gettime(CLOCK_MONOTONIC, &time);
	u64 result = (u64)time.tv_sec * 1000000000ULL + (u64)time.tv_nsec;
	return result;
}

function void
os_common_init(void)
{
	os_linux_context.system_info.logical_processor_count = (u32)get_nprocs();
	os_linux_context.system_info.page_size               = (u32)getpagesize();
}

function iz
os_round_up_to_page_size(iz value)
{
	iz result = round_up_to(value, os_linux_context.system_info.page_size);
	return result;
}

function OS_ALLOC_ARENA_FN(os_alloc_arena)
{
	Arena result = {0};
	capacity   = os_round_up_to_page_size(capacity);
	result.beg = mmap(0, (uz)capacity, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);
	if (result.beg == MAP_FAILED)
		os_fatal(s8("os_alloc_arena: couldn't allocate memory\n"));
	result.end = result.beg + capacity;
	asan_poison_region(result.beg, result.end - result.beg);
	return result;
}

function OS_READ_WHOLE_FILE_FN(os_read_whole_file)
{
	s8 result = s8("");

	struct stat sb;
	i32 fd = open(file, O_RDONLY);
	if (fd >= 0 && fstat(fd, &sb) >= 0) {
		result = s8_alloc(arena, sb.st_size);
		iz rlen = read(fd, result.data, (uz)result.len);
		if (rlen != result.len)
			result = s8("");
	}
	if (fd >= 0) close(fd);

	return result;
}

function OS_WRITE_NEW_FILE_FN(os_write_new_file)
{
	b32 result = 0;
	i32 fd = open(fname, O_WRONLY|O_TRUNC|O_CREAT, 0600);
	if (fd != INVALID_FILE) {
		result = os_write_file(fd, raw);
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

function SharedMemoryRegion
os_create_shared_memory_area(Arena *arena, char *name, u32 lock_count, iz requested_capacity)
{
	iz capacity = os_round_up_to_page_size(requested_capacity);
	SharedMemoryRegion result = {0};
	i32 fd = shm_open(name, O_CREAT|O_RDWR, S_IRUSR|S_IWUSR);
	if (fd > 0 && ftruncate(fd, capacity) != -1) {
		void *new = mmap(0, (uz)capacity, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
		if (new != MAP_FAILED) result.region = new;
	}
	if (fd > 0) close(fd);
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
			iz copied = 0;
			while (copied != sb.st_size) {
				iz r = read(fd_old, buf, countof(buf));
				if (r < 0) break;
				iz w = write(fd_new, buf, (uz)r);
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

function OSLinux_FileWatchDirectory *
os_lookup_file_watch_directory(OSLinux_FileWatchDirectoryList *ctx, u64 hash)
{
	OSLinux_FileWatchDirectory *result = 0;
	for (iz i = 0; !result && i < ctx->count; i++)
		if (ctx->data[i].hash == hash)
			result = ctx->data + i;
	return result;
}

function void
os_linux_add_file_watch(s8 path, void *user_context, OSLinux_FileWatchKind kind)
{
	s8 directory  = path;
	directory.len = s8_scan_backwards(path, '/');
	assert(directory.len > 0);

	OSLinux_FileWatchDirectoryList *fwctx = &os_linux_context.file_watch_list;

	u64 hash = u64_hash_from_s8(directory);
	OSLinux_FileWatchDirectory *dir = os_lookup_file_watch_directory(fwctx, hash);
	if (!dir) {
		assert(path.data[directory.len] == '/');
		dir = da_push(&os_linux_context.arena, fwctx);
		dir->hash   = hash;
		dir->name   = push_s8(&os_linux_context.arena, directory);
		u32 mask    = IN_MOVED_TO|IN_CLOSE_WRITE;
		dir->handle = inotify_add_watch(os_linux_context.inotify_handle, (c8 *)dir->name.data, mask);
	}

	OSLinux_FileWatch *fw = da_push(&os_linux_context.arena, dir);
	fw->user_context = user_context;
	fw->hash         = u64_hash_from_s8(s8_cut_head(path, dir->name.len + 1));
	fw->kind         = kind;
}

function OS_WAIT_ON_VALUE_FN(os_wait_on_value)
{
	struct timespec *timeout = 0, timeout_value;
	if (timeout_ms != (u32)-1) {
		timeout_value.tv_sec  = timeout_ms / 1000;
		timeout_value.tv_nsec = (timeout_ms % 1000) * 1000000;
		timeout = &timeout_value;
	}
	return syscall(SYS_futex, value, FUTEX_WAIT, current, timeout, 0, 0) == 0;
}

function OS_WAKE_WAITERS_FN(os_wake_waiters)
{
	if (sync) {
		atomic_store_u32(sync, 0);
		syscall(SYS_futex, sync, FUTEX_WAKE, I32_MAX, 0, 0, 0);
	}
}

function b32
os_take_lock(i32 *lock, i32 timeout_ms)
{
	b32 result = 0;
	for (;;) {
		i32 current = 0;
		if (atomic_cas_u32(lock, &current, 1))
			result = 1;
		if (result || !timeout_ms || (!os_wait_on_value(lock, current, (u32)timeout_ms) && timeout_ms != -1))
			break;
	}
	return result;
}

function void
os_release_lock(i32 *lock)
{
	assert(atomic_load_u32(lock));
	atomic_store_u32(lock, 0);
	os_wake_waiters(lock);
}

function OS_SHARED_MEMORY_LOCK_REGION_FN(os_shared_memory_region_lock)
{
	b32 result = os_take_lock(locks + lock_index, (i32)timeout_ms);
	return result;
}

function OS_SHARED_MEMORY_UNLOCK_REGION_FN(os_shared_memory_region_unlock)
{
	os_release_lock(locks + lock_index);
}

function OS_SystemInfo *
os_get_system_info(void)
{
	return &os_linux_context.system_info;
}

function Barrier
os_barrier_alloc(u32 count)
{
	Barrier result = {0};
	DeferLoop(os_take_lock(&os_linux_context.arena_lock, -1),
	          os_release_lock(&os_linux_context.arena_lock))
	{
		pthread_barrier_t *barrier = push_struct(&os_linux_context.arena, pthread_barrier_t);
		pthread_barrier_init(barrier, 0, count);
		result.value[0] = (u64)barrier;
	}
	return result;
}

function void
os_barrier_wait(Barrier barrier)
{
	pthread_barrier_t *b = (pthread_barrier_t *)barrier.value[0];
	if (b) pthread_barrier_wait(b);
}

function iptr
os_create_thread(iptr user_context, os_thread_entry_point_fn *fn)
{
	pthread_t result;
	pthread_create(&result, 0, (void *)fn, (void *)user_context);
	return (iptr)result;
}

function void
os_set_thread_name(iptr thread, s8 name)
{
	char buffer[16];
	u64  length = (u64)CLAMP(name.len, 0, countof(buffer) - 1);
	mem_copy(buffer, name.data, length);
	buffer[length] = 0;
	pthread_setname_np((pthread_t)thread, buffer);
}
