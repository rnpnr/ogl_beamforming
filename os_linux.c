/* See LICENSE for license details. */

/* NOTE(rnp): provides the platform layer for the beamformer. This code must
 * be provided by any platform the beamformer is ported to. */

#define OS_SHARED_MEMORY_NAME "/ogl_beamformer_shared_memory"
#define OS_EXPORT_PIPE_NAME   "/tmp/beamformer_output_pipe"

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
i32 ftruncate(i32, i64);
i64 syscall(i64, ...);

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
	while (raw.len) {
		iz r = write(file, raw.data, raw.len);
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

function OS_ALLOC_ARENA_FN(os_alloc_arena)
{
	Arena result;
	iz pagesize = sysconf(_SC_PAGESIZE);
	if (capacity % pagesize != 0)
		capacity += pagesize - capacity % pagesize;

	iz oldsize = old.end - old.beg;
	if (oldsize > capacity)
		return old;

	if (old.beg)
		munmap(old.beg, oldsize);

	result.beg = mmap(0, capacity, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);
	if (result.beg == MAP_FAILED)
		os_fatal(s8("os_alloc_arena: couldn't allocate memory\n"));
	result.end = result.beg + capacity;
	return result;
}

function OS_CLOSE_FN(os_close)
{
	close(file);
}

function OS_OPEN_FOR_WRITE_FN(os_open_for_write)
{
	iptr result = open(fname, O_WRONLY|O_TRUNC);
	if (result == -1)
		result = INVALID_FILE;
	return result;
}

function OS_READ_WHOLE_FILE_FN(os_read_whole_file)
{
	s8 result = s8("");

	struct stat sb;
	i32 fd = open(file, O_RDONLY);
	if (fd >= 0 && fstat(fd, &sb) >= 0) {
		result = s8_alloc(arena, sb.st_size);
		iz rlen = read(fd, result.data, result.len);
		if (rlen != result.len)
			result = s8("");
	}
	if (fd >= 0) close(fd);

	return result;
}

function OS_WRITE_NEW_FILE_FN(os_write_new_file)
{
	iptr fd = open(fname, O_WRONLY|O_TRUNC|O_CREAT, 0600);
	if (fd == INVALID_FILE)
		return 0;
	b32 ret = os_write_file(fd, raw);
	close(fd);
	return ret;
}

function b32
os_file_exists(char *path)
{
	struct stat st;
	b32 result = stat(path, &st) == 0;
	return result;
}

function OS_READ_FILE_FN(os_read_file)
{
	iz r = 0, total_read = 0;
	do {
		if (r != -1)
			total_read += r;
		r = read(file, buf + total_read, size - total_read);
	} while (r);
	return total_read;
}

function void *
os_create_shared_memory_area(char *name, iz cap)
{
	void *result = 0;
	i32 fd = shm_open(name, O_CREAT|O_RDWR, S_IRUSR|S_IWUSR);
	if (fd > 0 && ftruncate(fd, cap) != -1) {
		void *new = mmap(NULL, cap, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
		if (new != MAP_FAILED)
			result = new;
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
				iz w = write(fd_new, buf, r);
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
		i32 mask    = IN_MOVED_TO|IN_CLOSE_WRITE;
		dir->handle = inotify_add_watch(fwctx->handle, (c8 *)dir->name.data, mask);
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
	pthread_create(&result, 0, (void *(*)(void *))fn, (void *)user_context);
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
		atomic_store(sync, 0);
		syscall(SYS_futex, sync, FUTEX_WAKE, I32_MAX, 0, 0, 0);
	}
}
