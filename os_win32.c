/* See LICENSE for license details. */
#include "util.h"

#define STD_OUTPUT_HANDLE -11
#define STD_ERROR_HANDLE  -12

#define PAGE_READWRITE 0x04
#define MEM_COMMIT     0x1000
#define MEM_RESERVE    0x2000
#define MEM_RELEASE    0x8000

#define GENERIC_WRITE  0x40000000
#define GENERIC_READ   0x80000000

#define PIPE_WAIT      0x00
#define PIPE_NOWAIT    0x01

#define PIPE_TYPE_BYTE      0x00
#define PIPE_ACCESS_INBOUND 0x01

#define FILE_SHARE_READ            0x00000001
#define FILE_MAP_ALL_ACCESS        0x000F001F
#define FILE_FLAG_BACKUP_SEMANTICS 0x02000000
#define FILE_FLAG_OVERLAPPED       0x40000000

#define FILE_NOTIFY_CHANGE_LAST_WRITE 0x00000010

#define FILE_ACTION_MODIFIED 0x00000003

#define CREATE_ALWAYS  2
#define OPEN_EXISTING  3

#define ERROR_NO_DATA            232L
#define ERROR_PIPE_NOT_CONNECTED 233L
#define ERROR_PIPE_LISTENING     536L

typedef struct {
	u16  wProcessorArchitecture;
	u16  _pad1;
	u32  dwPageSize;
	size lpMinimumApplicationAddress;
	size lpMaximumApplicationAddress;
	u64  dwActiveProcessorMask;
	u32  dwNumberOfProcessors;
	u32  dwProcessorType;
	u32  dwAllocationGranularity;
	u16  wProcessorLevel;
	u16  wProcessorRevision;
} w32_sys_info;

/* NOTE: this is packed because the w32 api designers are dumb and ordered the members
 * incorrectly. They worked around it be making the ft* members a struct {u32, u32} which
 * is aligned on a 4-byte boundary. Then in their documentation they explicitly tell you not
 * to cast to u64 because "it can cause alignment faults on 64-bit Windows" - go figure */
typedef struct __attribute__((packed)) {
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
} w32_file_info;

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

#define W32(r) __declspec(dllimport) r __stdcall
W32(b32)    CloseHandle(iptr);
W32(b32)    CopyFileA(c8 *, c8 *, b32);
W32(iptr)   CreateFileA(c8 *, u32, u32, void *, u32, u32, void *);
W32(iptr)   CreateFileMappingA(iptr, void *, u32, u32, u32, c8 *);
W32(iptr)   CreateIoCompletionPort(iptr, iptr, uptr, u32);
W32(iptr)   CreateNamedPipeA(c8 *, u32, u32, u32, u32, u32, u32, void *);
W32(b32)    DeleteFileA(c8 *);
W32(b32)    DisconnectNamedPipe(iptr);
W32(void)   ExitProcess(i32);
W32(b32)    FreeLibrary(void *);
W32(b32)    GetFileInformationByHandle(iptr, w32_file_info *);
W32(i32)    GetLastError(void);
W32(void *) GetProcAddress(void *, c8 *);
W32(b32)    GetQueuedCompletionStatus(iptr, u32 *, uptr *, w32_overlapped **, u32);
W32(iptr)   GetStdHandle(i32);
W32(void)   GetSystemInfo(void *);
W32(void *) LoadLibraryA(c8 *);
W32(void *) MapViewOfFile(iptr, u32, u32, u32, u64);
W32(b32)    ReadDirectoryChangesW(iptr, u8 *, u32, b32, u32, u32 *, void *, void *);
W32(b32)    ReadFile(iptr, u8 *, i32, i32 *, void *);
W32(b32)    WriteFile(iptr, u8 *, i32, i32 *, void *);
W32(void *) VirtualAlloc(u8 *, size, u32, u32);
W32(b32)    VirtualFree(u8 *, size, u32);

static iptr win32_stderr_handle;

static PLATFORM_WRITE_FILE_FN(os_write_file)
{
	i32 wlen;
	WriteFile(file, raw.data, raw.len, &wlen, 0);
	return raw.len == wlen;
}

static void
os_write_err_msg(s8 msg)
{
	if (!win32_stderr_handle)
		win32_stderr_handle = GetStdHandle(STD_ERROR_HANDLE);
	os_write_file(win32_stderr_handle, msg);
}

static void __attribute__((noreturn))
os_fatal(s8 msg)
{
	os_write_err_msg(msg);
	ExitProcess(1);
	unreachable();
}

static PLATFORM_ALLOC_ARENA_FN(os_alloc_arena)
{
	Arena result;
	w32_sys_info Info;
	GetSystemInfo(&Info);

	if (capacity % Info.dwPageSize != 0)
		capacity += (Info.dwPageSize - capacity % Info.dwPageSize);

	size oldsize = old.end - old.beg;
	if (oldsize > capacity)
		return old;

	if (old.beg)
		VirtualFree(old.beg, oldsize, MEM_RELEASE);

	result.beg = VirtualAlloc(0, capacity, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
	if (result.beg == NULL)
		os_fatal(s8("os_alloc_arena: couldn't allocate memory\n"));
	result.end = result.beg + capacity;
	return result;
}

static PLATFORM_CLOSE_FN(os_close)
{
	CloseHandle(file);
}

static PLATFORM_OPEN_FOR_WRITE_FN(os_open_for_write)
{
	iptr result = CreateFileA(fname, GENERIC_WRITE, 0, 0, OPEN_EXISTING, 0, 0);
	return result;
}

static s8
os_read_file(Arena *a, char *file, size filesize)
{
	s8 result = {0};

	if (filesize > 0 && filesize <= (size)U32_MAX) {
		result = s8alloc(a, filesize);
		iptr h = CreateFileA(file, GENERIC_READ, 0, 0, OPEN_EXISTING, 0, 0);
		if (h >= 0) {
			i32 rlen;
			if (!ReadFile(h, result.data, result.len, &rlen, 0) || rlen != result.len) {
				result = (s8){0};
			}
			CloseHandle(h);
		}
	}

	return result;
}

static PLATFORM_WRITE_NEW_FILE_FN(os_write_new_file)
{
	if (raw.len > (size)U32_MAX) {
		os_write_err_msg(s8("os_write_file: files >4GB are not yet handled on win32\n"));
		return 0;
	}

	iptr h = CreateFileA(fname, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, 0, 0);
	if (h == INVALID_FILE)
		return  0;

	b32 ret = os_write_file(h, raw);
	CloseHandle(h);

	return ret;
}

static FileStats
os_get_file_stats(char *fname)
{
	iptr h = CreateFileA(fname, 0, 0, 0, OPEN_EXISTING, 0, 0);
	if (h == INVALID_FILE)
		return ERROR_FILE_STATS;

	w32_file_info fileinfo;
	if (!GetFileInformationByHandle(h, &fileinfo)) {
		os_write_err_msg(s8("os_get_file_stats: couldn't get file info\n"));
		CloseHandle(h);
		return ERROR_FILE_STATS;
	}
	CloseHandle(h);

	size filesize = (size)fileinfo.nFileSizeHigh << 32;
	filesize     |= (size)fileinfo.nFileSizeLow;

	return (FileStats){.filesize  = filesize, .timestamp = fileinfo.ftLastWriteTime};
}

static Pipe
os_open_named_pipe(char *name)
{
	iptr h = CreateNamedPipeA(name, PIPE_ACCESS_INBOUND, PIPE_TYPE_BYTE|PIPE_NOWAIT, 1,
	                          0, 1 * MEGABYTE, 0, 0);
	return (Pipe){.file = h, .name = name};
}

static PLATFORM_READ_PIPE_FN(os_read_pipe)
{
	i32 total_read = 0;
	ReadFile(pipe, buf, len, &total_read, 0);
	return total_read;
}

static void *
os_open_shared_memory_area(char *name, size cap)
{
	iptr h = CreateFileMappingA(-1, 0, PAGE_READWRITE, 0, cap, name);
	if (h == INVALID_FILE)
		return NULL;

	return MapViewOfFile(h, FILE_MAP_ALL_ACCESS, 0, 0, cap);
}

static void *
os_load_library(char *name, char *temp_name, Stream *e)
{
	if (temp_name) {
		if (CopyFileA(name, temp_name, 0))
			name = temp_name;
	}

	void *res = LoadLibraryA(name);
	if (!res && e) {
		s8 errs[] = {s8("WARNING: os_load_library("), cstr_to_s8(name), s8("): ")};
		stream_append_s8_array(e, errs, ARRAY_COUNT(errs));
		stream_append_i64(e, GetLastError());
		stream_append_byte(e, '\n');
		os_write_err_msg(stream_to_s8(e));
		e->widx = 0;
	}

	if (temp_name)
		DeleteFileA(temp_name);

	return res;
}

static void *
os_lookup_dynamic_symbol(void *h, char *name, Stream *e)
{
	if (!h)
		return 0;
	void *res = GetProcAddress(h, name);
	if (!res && e) {
		s8 errs[] = {s8("WARNING: os_lookup_dynamic_symbol("), cstr_to_s8(name), s8("): ")};
		stream_append_s8_array(e, errs, ARRAY_COUNT(errs));
		stream_append_i64(e, GetLastError());
		stream_append_byte(e, '\n');
		os_write_err_msg(stream_to_s8(e));
		e->widx = 0;
	}
	return res;
}

static void
os_unload_library(void *h)
{
	FreeLibrary(h);
}

static PLATFORM_ADD_FILE_WATCH_FN(os_add_file_watch)
{
	s8 directory  = path;
	directory.len = s8_scan_backwards(path, '\\');
	ASSERT(directory.len > 0);

	u64 hash = s8_hash(directory);
	FileWatchContext *fwctx = &platform->file_watch_context;
	FileWatchDirectory *dir = lookup_file_watch_directory(fwctx, hash);
	if (!dir) {
		ASSERT(path.data[directory.len] == '\\');

		dir         = fwctx->directory_watches + fwctx->directory_watch_count++;
		dir->hash   = hash;

		dir->name   = push_s8(a, directory);
		arena_commit(a, 1);
		dir->name.data[dir->name.len] = 0;

		dir->handle = CreateFileA((c8 *)dir->name.data, GENERIC_READ, FILE_SHARE_READ, 0,
		                          OPEN_EXISTING,
		                          FILE_FLAG_BACKUP_SEMANTICS|FILE_FLAG_OVERLAPPED, 0);

		w32_context *ctx = (w32_context *)platform->os_context;
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

	insert_file_watch(dir, s8_cut_head(path, dir->name.len + 1), user_data, callback);
}
