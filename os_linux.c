/* See LICENSE for license details. */

/* NOTE(rnp): provides the platform layer for the beamformer. This code must
 * be provided by any platform the beamformer is ported to. */

#define OS_SHARED_MEMORY_NAME "/ogl_beamformer_shared_memory"

#define OS_PATH_SEPARATOR_CHAR '/'
#define OS_PATH_SEPARATOR      "/"

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

function b32
os_write_file(i32 file, void *data, i64 length)
{
	i64 offset = 0;
	while (offset < length) {
		iz r = write(file, (u8 *)data + offset, length - offset);
		if (r < 0 && errno != EINTR) break;
		if (r >= 0) offset += r;
	}
	return offset == length;
}

function no_return void
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

BEAMFORMER_IMPORT u64
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

function OS_ALLOC_ARENA_FN(os_alloc_arena)
{
	Arena result = {0};
	capacity     = round_up_to(capacity, ARCH_X64? KB(4) : getauxval(AT_PAGESZ));
	void *memory = mmap(0, (uz)capacity, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);
	if (memory != MAP_FAILED) {
		result.beg = memory;
		result.end = result.beg + capacity;
		asan_poison_region(result.beg, result.end - result.beg);
	}
	return result;
}

BEAMFORMER_IMPORT OS_READ_ENTIRE_FILE_FN(os_read_entire_file)
{
	i64 result = 0;
	struct stat sb;
	i32 fd = open(file, O_RDONLY);
	if (fd >= 0 && fstat(fd, &sb) >= 0) {
		if (buffer_capacity >= sb.st_size) {
			do {
				i64 rlen = read(fd, (u8 *)buffer + result, sb.st_size - result);
				if (rlen > 0) result += rlen;
			} while (result != sb.st_size && errno != EINTR);
			if (result != sb.st_size) result = 0;
		}
	}
	if (fd >= 0) close(fd);

	return result;
}

function OS_WRITE_NEW_FILE_FN(os_write_new_file)
{
	b32 result = 0;
	i32 fd = open(fname, O_WRONLY|O_TRUNC|O_CREAT, 0600);
	if (fd != INVALID_FILE) {
		result = os_write_file(fd, raw.data, raw.len);
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

BEAMFORMER_IMPORT OS_WAIT_ON_ADDRESS_FN(os_wait_on_address)
{
	struct timespec *timeout = 0, timeout_value;
	if (timeout_ms != (u32)-1) {
		timeout_value.tv_sec  = timeout_ms / 1000;
		timeout_value.tv_nsec = (timeout_ms % 1000) * 1000000;
		timeout = &timeout_value;
	}
	return syscall(SYS_futex, value, FUTEX_WAIT, current, timeout, 0, 0) == 0;
}

BEAMFORMER_IMPORT OS_WAKE_ALL_WAITERS_FN(os_wake_all_waiters)
{
	if (sync) {
		atomic_store_u32(sync, 0);
		syscall(SYS_futex, sync, FUTEX_WAKE, I32_MAX, 0, 0, 0);
	}
}
