/* See LICENSE for license details. */

/* NOTE(rnp): provides the platform layer for the beamformer. This code must
 * be provided by any platform the beamformer is ported to. */

#define OS_SHARED_MEMORY_NAME "/ogl_beamformer_shared_memory"

#define OS_PATH_SEPARATOR_CHAR '/'
#define OS_PATH_SEPARATOR      "/"

#include "util.h"

#include <dlfcn.h>
#include <fcntl.h>
#include <linux/futex.h>
#include <poll.h>
#include <pthread.h>
#include <sys/inotify.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <unistd.h>

/* NOTE(rnp): hidden behind feature flags -> screw compiler/standards idiots */
#ifndef CLOCK_MONOTONIC
  #define CLOCK_MONOTONIC 1
#endif
i32 ftruncate(i32, i64);
i64 syscall(i64, ...);
i32 clock_gettime(i32, struct timespec *);

#ifdef _DEBUG
function void *
os_get_module(char *name, Stream *e)
{
	void *result = dlopen(name, RTLD_NOW|RTLD_LOCAL|RTLD_NOLOAD);
	if (!result && e) {
		stream_append_s8s(e, s8("os_get_module(\""), c_str_to_s8(name), s8("\"): "),
		                  c_str_to_s8(dlerror()), s8("\n"));
	}
	return result;
}
#endif

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

function iz
os_round_up_to_page_size(iz value)
{
	iz result = round_up_to(value, sysconf(_SC_PAGESIZE));
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
os_create_shared_memory_area(Arena *arena, char *name, i32 lock_count, iz requested_capacity)
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

function void *
os_load_library(char *name, char *temp_name, Stream *e)
{
	if (temp_name) {
		if (os_copy_file(name, temp_name))
			name = temp_name;
	}

	void *result = dlopen(name, RTLD_NOW|RTLD_LOCAL);
	if (!result && e) {
		stream_append_s8s(e, s8("os_load_library(\""), c_str_to_s8(name), s8("\"): "),
		                  c_str_to_s8(dlerror()), s8("\n"));
	}

	if (temp_name)
		unlink(temp_name);

	return result;
}

function void *
os_lookup_dynamic_symbol(void *h, char *name, Stream *e)
{
	void *result = 0;
	if (h) {
		result = dlsym(h, name);
		if (!result && e) {
			stream_append_s8s(e, s8("os_lookup_dynamic_symbol(\""), c_str_to_s8(name),
			                  s8("\"): "), c_str_to_s8(dlerror()), s8("\n"));
		}
	}
	return result;
}

function void
os_unload_library(void *h)
{
	/* NOTE: glibc is buggy gnuware so we need to check this */
	if (h)
		dlclose(h);
}

function OS_ADD_FILE_WATCH_FN(os_add_file_watch)
{
	s8 directory  = path;
	directory.len = s8_scan_backwards(path, '/');
	ASSERT(directory.len > 0);

	u64 hash = s8_hash(directory);
	FileWatchContext *fwctx = &os->file_watch_context;
	FileWatchDirectory *dir = lookup_file_watch_directory(fwctx, hash);
	if (!dir) {
		ASSERT(path.data[directory.len] == '/');
		dir = da_push(a, fwctx);
		dir->hash   = hash;
		dir->name   = push_s8_zero(a, directory);
		u32 mask    = IN_MOVED_TO|IN_CLOSE_WRITE;
		dir->handle = inotify_add_watch((i32)fwctx->handle, (c8 *)dir->name.data, mask);
	}

	FileWatch *fw = da_push(a, dir);
	fw->user_data = user_data;
	fw->callback  = callback;
	fw->hash      = s8_hash(s8_cut_head(path, dir->name.len + 1));
}

i32 pthread_setname_np(pthread_t, char *);
function iptr
os_create_thread(Arena arena, iptr user_context, s8 name, os_thread_entry_point_fn *fn)
{
	pthread_t result;
	pthread_create(&result, 0, (void *)fn, (void *)user_context);
	pthread_setname_np(result, (char *)name.data);
	return (iptr)result;
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

function OS_SHARED_MEMORY_LOCK_REGION_FN(os_shared_memory_region_lock)
{
	b32 result = 0;
	for (;;) {
		i32 current = atomic_load_u32(locks + lock_index);
		if (current == 0 && atomic_cas_u32(locks + lock_index, &current, 1)) {
			result = 1;
			break;
		}
		if (!timeout_ms || !os_wait_on_value(locks + lock_index, current, timeout_ms))
			break;
	}
	return result;
}

function OS_SHARED_MEMORY_UNLOCK_REGION_FN(os_shared_memory_region_unlock)
{
	i32 *lock = locks + lock_index;
	assert(atomic_load_u32(lock));
	atomic_store_u32(lock, 0);
	os_wake_waiters(lock);
}
