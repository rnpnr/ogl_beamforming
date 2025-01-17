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

#define FILE_MAP_ALL_ACCESS 0x000F001F

#define CREATE_ALWAYS  2
#define OPEN_EXISTING  3

#define ERROR_NO_DATA				232L
#define ERROR_PIPE_NOT_CONNECTED	233L
#define ERROR_PIPE_LISTENING		536L

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

#define W32(r) __declspec(dllimport) r __stdcall
W32(b32)    CloseHandle(iptr);
W32(b32)    CopyFileA(c8 *, c8 *, b32);
W32(iptr)   CreateFileA(c8 *, u32, u32, void *, u32, u32, void *);
W32(iptr)   CreateFileMappingA(iptr, void *, u32, u32, u32, c8 *);
W32(iptr)   CreateNamedPipeA(c8 *, u32, u32, u32, u32, u32, u32, void *);
W32(b32)    DeleteFileA(c8 *);
W32(b32)	DisconnectNamedPipe(iptr);
W32(void)   ExitProcess(i32);
W32(b32)    FreeLibrary(void *);
W32(b32)    GetFileInformationByHandle(iptr, w32_file_info *);
W32(i32)    GetLastError(void);
W32(void *) GetProcAddress(void *, c8 *);
W32(iptr)   GetStdHandle(i32);
W32(void)   GetSystemInfo(void *);
W32(void *) LoadLibraryA(c8 *);
W32(void *) MapViewOfFile(iptr, u32, u32, u32, u64);
W32(b32)    PeekNamedPipe(iptr, u8 *, i32, i32 *, i32 *, i32 *);
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
os_read_file(Arena *a, char *fname, size fsize)
{
	if (fsize < 0)
		return (s8){.len = -1};

	if (fsize > (size)U32_MAX) {
		os_write_err_msg(s8("os_read_file: files >4GB are not yet handled on win32\n"));
		return (s8){.len = -1};
	}

	iptr h = CreateFileA(fname, GENERIC_READ, 0, 0, OPEN_EXISTING, 0, 0);
	if (h == INVALID_FILE)
		return (s8){.len = -1};

	s8 ret = s8alloc(a, fsize);

	i32 rlen  = 0;
	b32 error = !ReadFile(h, ret.data, ret.len, &rlen, 0) || rlen != ret.len;
	CloseHandle(h);
	if (error)
		return (s8){.len = -1};

	return ret;
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
	iptr h = CreateNamedPipeA(name, PIPE_ACCESS_INBOUND, PIPE_TYPE_BYTE | PIPE_NOWAIT, 1,
	                          0, 1 * MEGABYTE, 0, 0);
	return (Pipe){.file = h, .name = name};
}

/* NOTE: win32 doesn't pollute the filesystem so no need to waste the user's time */
static void
os_close_named_pipe(Pipe p)
{
	CloseHandle(p.file);
}

static PLATFORM_POLL_PIPE_FN(os_poll_pipe)
{
	// Try and read 0 bytes, this will give more pipe status information than PeakNamedPipe
	u8 data = 0;
	i32 total_read = 0;
	b32 result = ReadFile(p->file, &data, 0, &total_read, NULL);

	if (result == true)
	{
		return true;
	}

	i32 error = GetLastError();

	// These three errors just mean nothing's been sent yet, otherwise the pipe is in a bad state
	// and needs to be recreated.
	if (error != ERROR_NO_DATA && error != ERROR_PIPE_LISTENING && error != ERROR_PIPE_NOT_CONNECTED)
	{
		/*char msg[1024];
		snprintf(msg, 1024, "os_poll_pipe: Data pipe poll failed, Windows error '%i'.\n", error);
		os_write_err_msg(cstr_to_s8(msg));*/

		result = DisconnectNamedPipe(p->file);
		result = CloseHandle(p->file);
		*p = os_open_named_pipe(p->name);

		if (p->file == INVALID_FILE)
		{
			error = GetLastError();
			char msg[1024];
			snprintf(msg, 1024, "os_poll_pipe: Failed to reopen data pipe after error: "
				"Windows error '%i'.\n", error);

			os_write_err_msg(cstr_to_s8(msg));
		}
	}
	return false;
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

/* NOTE: closing the handle releases the memory and this happens when program terminates */
static void
os_remove_shared_memory(char *name)
{
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
