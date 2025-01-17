/* See LICENSE for license details. */
#include "util.h"

#include <dlfcn.h>
#include <fcntl.h>
#include <poll.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

static PLATFORM_WRITE_FILE_FN(os_write_file)
{
	while (raw.len) {
		size r = write(file, raw.data, raw.len);
		if (r < 0) return 0;
		raw = s8_cut_head(raw, r);
	}
	return 1;
}

static void
os_write_err_msg(s8 msg)
{
	os_write_file(STDERR_FILENO, msg);
}

static void __attribute__((noreturn))
os_fatal(s8 msg)
{
	os_write_err_msg(msg);
	_exit(1);
	unreachable();
}

static PLATFORM_ALLOC_ARENA_FN(os_alloc_arena)
{
	Arena result;
	size pagesize = sysconf(_SC_PAGESIZE);
	if (capacity % pagesize != 0)
		capacity += pagesize - capacity % pagesize;

	size oldsize = old.end - old.beg;
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

static PLATFORM_CLOSE_FN(os_close)
{
	close(file);
}

static PLATFORM_OPEN_FOR_WRITE_FN(os_open_for_write)
{
	iptr result = open(fname, O_WRONLY|O_TRUNC);
	if (result == -1)
		result = INVALID_FILE;
	return result;
}

static s8
os_read_file(Arena *a, char *fname, size fsize)
{
	if (fsize < 0)
		return (s8){.len = -1};

	i32 fd = open(fname, O_RDONLY);
	if (fd < 0)
		return (s8){.len = -1};

	s8 ret    = s8alloc(a, fsize);
	size rlen = read(fd, ret.data, ret.len);
	close(fd);

	if (rlen != ret.len)
		return (s8){.len = -1};

	return ret;
}

static PLATFORM_WRITE_NEW_FILE_FN(os_write_new_file)
{
	iptr fd = open(fname, O_WRONLY|O_TRUNC|O_CREAT, 0600);
	if (fd == INVALID_FILE)
		return 0;
	b32 ret = os_write_file(fd, raw);
	close(fd);
	return ret;
}

static FileStats
os_get_file_stats(char *fname)
{
	struct stat st;

	if (stat(fname, &st) < 0) {
		return ERROR_FILE_STATS;
	}

	return (FileStats){
		.filesize  = st.st_size,
		.timestamp = (f64)st.st_mtim.tv_sec + (f64)st.st_mtim.tv_nsec * 1e-9,
	};
}

static Pipe
os_open_named_pipe(char *name)
{
	mkfifo(name, 0660);
	return (Pipe){.file = open(name, O_RDONLY|O_NONBLOCK), .name = name};
}

static void
os_close_named_pipe(Pipe p)
{
	close(p.file);
	unlink(p.name);
}

static PLATFORM_POLL_PIPE_FN(os_poll_pipe)
{
	struct pollfd pfd = {.fd = p->file, .events = POLLIN};
	poll(&pfd, 1, 0);
	return !!(pfd.revents & POLLIN);
}

static PLATFORM_READ_PIPE_FN(os_read_pipe)
{
	size r = 0, total_read = 0;
	do {
		if (r != -1)
			total_read += r;
		r = read(pipe, buf + total_read, len - total_read);
	} while (r);
	return total_read;
}

static void *
os_open_shared_memory_area(char *name, size cap)
{
	i32 fd = shm_open(name, O_CREAT|O_RDWR, S_IRUSR|S_IWUSR);
	if (fd == -1)
		return NULL;

	if (ftruncate(fd, cap) == -1) {
		close(fd);
		return NULL;
	}

	void *new = mmap(NULL, cap, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
	close(fd);

	if (new == MAP_FAILED)
		return NULL;

	return new;
}

static void
os_remove_shared_memory(char *name)
{
	shm_unlink(name);
}

/* NOTE: complete garbage because there is no standarized copyfile() in POSix */
static b32
os_copy_file(char *name, char *new)
{
	b32 result = 0;
	struct stat sb;
	if (stat(name, &sb) < 0)
		return 0;

	i32 fd_old = open(name, O_RDONLY);
	i32 fd_new = open(new, O_WRONLY|O_TRUNC, sb.st_mode);

	if (fd_old < 0 || fd_new < 0)
		goto ret;
	u8 buf[4096];
	size copied = 0;
	while (copied != sb.st_size) {
		size r = read(fd_old, buf, ARRAY_COUNT(buf));
		if (r < 0) goto ret;
		size w = write(fd_new, buf, r);
		if (w < 0) goto ret;
		copied += w;
	}
	result = 1;
ret:
	if (fd_old != -1) close(fd_old);
	if (fd_new != -1) close(fd_new);
	return result;
}

static void *
os_load_library(char *name, char *temp_name, Stream *e)
{
	if (temp_name) {
		if (os_copy_file(name, temp_name))
			name = temp_name;
	}
	void *res = dlopen(name, RTLD_NOW|RTLD_LOCAL);
	if (!res && e) {
		s8 errs[] = {s8("WARNING: os_load_library("), cstr_to_s8(name), s8("): "),
		             cstr_to_s8(dlerror()), s8("\n")};
		stream_append_s8_array(e, errs, ARRAY_COUNT(errs));
		os_write_err_msg(stream_to_s8(e));
		e->widx = 0;
	}

	if (temp_name)
		unlink(temp_name);

	return res;
}

static void *
os_lookup_dynamic_symbol(void *h, char *name, Stream *e)
{
	if (!h)
		return 0;
	void *res = dlsym(h, name);
	if (!res && e) {
		s8 errs[] = {s8("WARNING: os_lookup_dynamic_symbol("), cstr_to_s8(name), s8("): "),
		             cstr_to_s8(dlerror()), s8("\n")};
		stream_append_s8_array(e, errs, ARRAY_COUNT(errs));
		os_write_err_msg(stream_to_s8(e));
		e->widx = 0;
	}

	return res;
}

static void
os_unload_library(void *h)
{
	/* NOTE: glibc is buggy gnuware so we need to check this */
	if (h)
		dlclose(h);
}
