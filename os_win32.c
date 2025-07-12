/* See LICENSE for license details. */

#define OS_SHARED_MEMORY_NAME "Local\\ogl_beamformer_parameters"

#define OS_PATH_SEPARATOR_CHAR '\\'
#define OS_PATH_SEPARATOR      "\\"

#include "util.h"

#define STD_INPUT_HANDLE  -10
#define STD_OUTPUT_HANDLE -11
#define STD_ERROR_HANDLE  -12

#define PAGE_READWRITE 0x04
#define MEM_COMMIT     0x1000
#define MEM_RESERVE    0x2000

#define GENERIC_WRITE  0x40000000
#define GENERIC_READ   0x80000000

#define FILE_SHARE_READ            0x00000001
#define FILE_MAP_ALL_ACCESS        0x000F001F
#define FILE_FLAG_BACKUP_SEMANTICS 0x02000000
#define FILE_FLAG_OVERLAPPED       0x40000000

#define FILE_NOTIFY_CHANGE_LAST_WRITE 0x00000010

#define FILE_ACTION_MODIFIED 0x00000003

#define CREATE_ALWAYS  2
#define OPEN_EXISTING  3

#define THREAD_SET_LIMITED_INFORMATION 0x0400

/* NOTE: this is packed because the w32 api designers are dumb and ordered the members
 * incorrectly. They worked around it be making the ft* members a struct {u32, u32} which
 * is aligned on a 4-byte boundary. Then in their documentation they explicitly tell you not
 * to cast to u64 because "it can cause alignment faults on 64-bit Windows" - go figure */
typedef struct w32_file_info w32_file_info;
pack_struct(struct w32_file_info {
	u32 dwFileAttributes;
	u64 ftCreationTime;
	u64 ftLastAccessTime;
	u64 ftLastWriteTime;
	u32 dwVolumeSerialNumber;
	u32 nFileSizeHigh;
	u32 nFileSizeLow;
	u32 nNumberOfLinks;
	u32 nFileIndexHigh;
	u32 nFileIndexLow;
});

typedef struct {
	u32 next_entry_offset;
	u32 action;
	u32 filename_size;
	u16 filename[];
} w32_file_notify_info;

typedef struct {
	uptr internal, internal_high;
	union {
		struct {u32 off, off_high;};
		iptr pointer;
	};
	iptr event_handle;
} w32_overlapped;

typedef struct {
	iptr io_completion_handle;
	u64  timer_frequency;
} os_w32_context;

typedef enum {
	W32_IO_FILE_WATCH,
	W32_IO_PIPE,
} W32_IO_Event;

typedef struct {
	u64  tag;
	iptr context;
} w32_io_completion_event;

typedef struct {
	iptr *semaphores;
} w32_shared_memory_context;

#define W32(r) __declspec(dllimport) r __stdcall
W32(b32)    CloseHandle(iptr);
W32(b32)    CopyFileA(c8 *, c8 *, b32);
W32(iptr)   CreateFileA(c8 *, u32, u32, void *, u32, u32, void *);
W32(iptr)   CreateFileMappingA(iptr, void *, u32, u32, u32, c8 *);
W32(iptr)   CreateIoCompletionPort(iptr, iptr, uptr, u32);
W32(iptr)   CreateSemaphoreA(iptr, i32, i32, c8 *);
W32(iptr)   CreateThread(iptr, uz, iptr, iptr, u32, u32 *);
W32(b32)    DeleteFileA(c8 *);
W32(void)   ExitProcess(i32);
W32(b32)    FreeLibrary(void *);
W32(i32)    GetFileAttributesA(c8 *);
W32(b32)    GetFileInformationByHandle(iptr, void *);
W32(i32)    GetLastError(void);
W32(void *) GetModuleHandleA(c8 *);
W32(void *) GetProcAddress(void *, c8 *);
W32(b32)    GetQueuedCompletionStatus(iptr, u32 *, uptr *, w32_overlapped **, u32);
W32(iptr)   GetStdHandle(i32);
W32(void)   GetSystemInfo(void *);
W32(void *) LoadLibraryA(c8 *);
W32(void *) MapViewOfFile(iptr, u32, u32, u32, u64);
W32(b32)    QueryPerformanceCounter(u64 *);
W32(b32)    QueryPerformanceFrequency(u64 *);
W32(b32)    ReadDirectoryChangesW(iptr, u8 *, u32, b32, u32, u32 *, void *, void *);
W32(b32)    ReadFile(iptr, u8 *, i32, i32 *, void *);
W32(b32)    ReleaseSemaphore(iptr, i32, i32 *);
W32(i32)    SetThreadDescription(iptr, u16 *);
W32(u32)    WaitForSingleObject(iptr, u32);
W32(b32)    WaitOnAddress(void *, void *, uz, u32);
W32(i32)    WakeByAddressAll(void *);
W32(iptr)   wglGetProcAddress(c8 *);
W32(b32)    WriteFile(iptr, u8 *, i32, i32 *, void *);
W32(void *) VirtualAlloc(u8 *, iz, u32, u32);

#ifdef _DEBUG
function void *
os_get_module(char *name, Stream *e)
{
	void *result = GetModuleHandleA(name);
	if (!result && e) {
		stream_append_s8s(e, s8("os_get_module(\""), c_str_to_s8(name), s8("\"): "));
		stream_append_i64(e, GetLastError());
		stream_append_byte(e, '\n');
	}
	return result;
}
#endif

function OS_WRITE_FILE_FN(os_write_file)
{
	i32 wlen = 0;
	if (raw.len > 0 && raw.len <= U32_MAX) WriteFile(file, raw.data, (i32)raw.len, &wlen, 0);
	return raw.len == wlen;
}

function no_return void
os_exit(i32 code)
{
	ExitProcess(1);
	unreachable();
}

function no_return void
os_fatal(s8 msg)
{
	os_write_file(GetStdHandle(STD_ERROR_HANDLE), msg);
	os_exit(1);
	unreachable();
}

function u64
os_get_timer_frequency(void)
{
	u64 result;
	QueryPerformanceFrequency(&result);
	return result;
}

function u64
os_get_timer_counter(void)
{
	u64 result;
	QueryPerformanceCounter(&result);
	return result;
}

function iz
os_round_up_to_page_size(iz value)
{
	struct {
		u16  architecture;
		u16  _pad1;
		u32  page_size;
		iz   minimum_application_address;
		iz   maximum_application_address;
		u64  active_processor_mask;
		u32  number_of_processors;
		u32  processor_type;
		u32  allocation_granularity;
		u16  processor_level;
		u16  processor_revision;
	} info;
	GetSystemInfo(&info);
	iz result = round_up_to(value, info.page_size);
	return result;
}

function OS_ALLOC_ARENA_FN(os_alloc_arena)
{
	Arena result = {0};
	capacity   = os_round_up_to_page_size(capacity);
	result.beg = VirtualAlloc(0, capacity, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
	if (!result.beg)
		os_fatal(s8("os_alloc_arena: couldn't allocate memory\n"));
	result.end = result.beg + capacity;
	return result;
}

function OS_READ_WHOLE_FILE_FN(os_read_whole_file)
{
	s8 result = s8("");

	w32_file_info fileinfo;
	iptr h = CreateFileA(file, GENERIC_READ, 0, 0, OPEN_EXISTING, 0, 0);
	if (h >= 0 && GetFileInformationByHandle(h, &fileinfo)) {
		iz filesize  = (iz)fileinfo.nFileSizeHigh << 32;
		filesize    |= (iz)fileinfo.nFileSizeLow;
		if (filesize <= U32_MAX) {
			result = s8_alloc(arena, filesize);
			i32 rlen;
			if (!ReadFile(h, result.data, (i32)result.len, &rlen, 0) || rlen != result.len)
				result = s8("");
		}
	}
	if (h >= 0) CloseHandle(h);

	return result;
}

function OS_WRITE_NEW_FILE_FN(os_write_new_file)
{
	b32 result = 0;
	iptr h = CreateFileA(fname, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, 0, 0);
	if (h >= 0) {
		while (raw.len > 0) {
			s8 chunk  = raw;
			chunk.len = MIN(chunk.len, (iz)GB(2));
			result    = os_write_file(h, chunk);
			if (!result) break;
			raw = s8_cut_head(raw, chunk.len);
		}
		CloseHandle(h);
	}
	return result;
}

function b32
os_file_exists(char *path)
{
	b32 result = GetFileAttributesA(path) != -1;
	return result;
}

function SharedMemoryRegion
os_create_shared_memory_area(Arena *arena, char *name, i32 lock_count, iz requested_capacity)
{
	iz capacity = os_round_up_to_page_size(requested_capacity);
	assert(capacity <= (iz)U32_MAX);
	SharedMemoryRegion result = {0};
	iptr h = CreateFileMappingA(-1, 0, PAGE_READWRITE, 0, (u32)capacity, name);
	if (h != INVALID_FILE) {
		void *new = MapViewOfFile(h, FILE_MAP_ALL_ACCESS, 0, 0, (u32)capacity);
		if (new) {
			w32_shared_memory_context *ctx = push_struct(arena, typeof(*ctx));
			ctx->semaphores   = push_array(arena, typeof(*ctx->semaphores), lock_count);
			result.os_context = (iptr)ctx;
			result.region     = new;

			Stream sb = arena_stream(*arena);
			stream_append_s8s(&sb, c_str_to_s8(name), s8("_lock_"));
			for (i32 i = 0; i < lock_count; i++) {
				Stream lb = sb;
				stream_append_i64(&lb, i);
				stream_append_byte(&lb, 0);
				ctx->semaphores[i] = CreateSemaphoreA(0, 1, 1, (c8 *)lb.data);
				if (ctx->semaphores[i] == INVALID_FILE) {
					os_fatal(s8("os_create_shared_memory_area: "
					            "failed to create semaphore\n"));
				}
			}
		}
	}
	return result;
}

function b32
os_copy_file(char *name, char *new)
{
	return CopyFileA(name, new, 0);
}

function void *
os_load_library(char *name, char *temp_name, Stream *e)
{
	if (temp_name && os_copy_file(name, temp_name))
		name = temp_name;

	void *result = LoadLibraryA(name);
	if (!result && e) {
		stream_append_s8s(e, s8("os_load_library(\""), c_str_to_s8(name), s8("\"): "));
		stream_append_i64(e, GetLastError());
		stream_append_byte(e, '\n');
	}

	if (temp_name)
		DeleteFileA(temp_name);

	return result;
}

function void *
os_lookup_dynamic_symbol(void *h, char *name, Stream *e)
{
	void *result = 0;
	if (h) {
		result = GetProcAddress(h, name);
		if (!result && e) {
			stream_append_s8s(e, s8("os_lookup_dynamic_symbol(\""), c_str_to_s8(name),
			                  s8("\"): "));
			stream_append_i64(e, GetLastError());
			stream_append_byte(e, '\n');
		}
	}
	return result;
}

function void
os_unload_library(void *h)
{
	FreeLibrary(h);
}

function OS_ADD_FILE_WATCH_FN(os_add_file_watch)
{
	s8 directory  = path;
	directory.len = s8_scan_backwards(path, '\\');
	ASSERT(directory.len > 0);

	u64 hash = s8_hash(directory);
	FileWatchContext *fwctx = &os->file_watch_context;
	FileWatchDirectory *dir = lookup_file_watch_directory(fwctx, hash);
	if (!dir) {
		ASSERT(path.data[directory.len] == '\\');

		dir = da_push(a, fwctx);
		dir->hash   = hash;
		dir->name   = push_s8_zero(a, directory);
		dir->handle = CreateFileA((c8 *)dir->name.data, GENERIC_READ, FILE_SHARE_READ, 0,
		                          OPEN_EXISTING,
		                          FILE_FLAG_BACKUP_SEMANTICS|FILE_FLAG_OVERLAPPED, 0);

		os_w32_context *ctx = (os_w32_context *)os->context;
		w32_io_completion_event *event = push_struct(a, typeof(*event));
		event->tag     = W32_IO_FILE_WATCH;
		event->context = (iptr)dir;
		CreateIoCompletionPort(dir->handle, ctx->io_completion_handle, (uptr)event, 0);

		dir->buffer = sub_arena(a, 4096 + sizeof(w32_overlapped), 64);
		w32_overlapped *overlapped = (w32_overlapped *)(dir->buffer.beg + 4096);
		zero_struct(overlapped);

		ReadDirectoryChangesW(dir->handle, dir->buffer.beg, 4096, 0,
		                      FILE_NOTIFY_CHANGE_LAST_WRITE, 0, overlapped, 0);
	}

	FileWatch *fw = da_push(a, dir);
	fw->user_data = user_data;
	fw->callback  = callback;
	fw->hash      = s8_hash(s8_cut_head(path, dir->name.len + 1));
}

function iptr
os_create_thread(Arena arena, iptr user_context, s8 name, os_thread_entry_point_fn *fn)
{
	iptr result = CreateThread(0, 0, (iptr)fn, user_context, 0, 0);
	SetThreadDescription(result, s8_to_s16(&arena, name).data);
	return result;
}

function OS_WAIT_ON_VALUE_FN(os_wait_on_value)
{
	return WaitOnAddress(value, &current, sizeof(*value), timeout_ms);
}

function OS_WAKE_WAITERS_FN(os_wake_waiters)
{
	if (sync) {
		atomic_store_u32(sync, 0);
		WakeByAddressAll(sync);
	}
}

function OS_SHARED_MEMORY_LOCK_REGION_FN(os_shared_memory_region_lock)
{
	w32_shared_memory_context *ctx = (typeof(ctx))sm->os_context;
	b32 result = !WaitForSingleObject(ctx->semaphores[lock_index], timeout_ms);
	if (result) atomic_store_u32(locks + lock_index, 1);
	return result;
}

function OS_SHARED_MEMORY_UNLOCK_REGION_FN(os_shared_memory_region_unlock)
{
	w32_shared_memory_context *ctx = (typeof(ctx))sm->os_context;
	assert(atomic_load_u32(locks + lock_index));
	os_wake_waiters(locks + lock_index);
	ReleaseSemaphore(ctx->semaphores[lock_index], 1, 0);
}
