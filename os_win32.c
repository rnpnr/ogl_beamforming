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
} w32_system_info;

typedef struct {
	uptr internal, internal_high;
	union {
		struct {u32 off, off_high;};
		iptr pointer;
	};
	iptr event_handle;
} w32_overlapped;

typedef enum {
	W32IOEvent_FileWatch,
} W32IOEvent;

typedef struct {
	u64  tag;
	iptr context;
} w32_io_completion_event;

typedef struct {
	iptr *semaphores;
	u32   reserved_count;
} w32_shared_memory_context;

#define W32(r) __declspec(dllimport) r __stdcall
W32(b32)    CloseHandle(iptr);
W32(b32)    CopyFileA(c8 *, c8 *, b32);
W32(iptr)   CreateFileA(c8 *, u32, u32, void *, u32, u32, void *);
W32(iptr)   CreateFileMappingA(iptr, void *, u32, u32, u32, c8 *);
W32(iptr)   CreateIoCompletionPort(iptr, iptr, uptr, u32);
W32(iptr)   CreateSemaphoreA(iptr, i32, i32, c8 *);
W32(b32)    DeleteFileA(c8 *);
W32(void)   ExitProcess(i32);
W32(i32)    GetFileAttributesA(c8 *);
W32(b32)    GetFileInformationByHandle(iptr, void *);
W32(i32)    GetLastError(void);
W32(b32)    GetQueuedCompletionStatus(iptr, u32 *, uptr *, w32_overlapped **, u32);
W32(iptr)   GetStdHandle(i32);
W32(void)   GetSystemInfo(w32_system_info *);
W32(void *) MapViewOfFile(iptr, u32, u32, u32, u64);
W32(b32)    QueryPerformanceCounter(u64 *);
W32(b32)    QueryPerformanceFrequency(u64 *);
W32(b32)    ReadDirectoryChangesW(iptr, u8 *, u32, b32, u32, u32 *, void *, void *);
W32(b32)    ReadFile(iptr, u8 *, i32, i32 *, void *);
W32(b32)    ReleaseSemaphore(iptr, i32, i32 *);
W32(u32)    WaitForSingleObject(iptr, u32);
W32(b32)    WaitOnAddress(void *, void *, uz, u32);
W32(i32)    WakeByAddressAll(void *);
W32(b32)    WriteFile(iptr, u8 *, i32, i32 *, void *);
W32(void *) VirtualAlloc(u8 *, iz, u32, u32);

function b32
os_write_file(iptr file, void *data, i64 length)
{
	i32 wlen = 0;
	if (length > 0 && length <= (i64)U32_MAX) WriteFile(file, data, (i32)length, &wlen, 0);
	return length == wlen;
}

function no_return void
os_exit(i32 code)
{
	ExitProcess(1);
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

function u32
os_get_page_size(void)
{
	w32_system_info info = {0};
	GetSystemInfo(&info);
	u32 result = info.page_size;
	return result;
}

function OS_ALLOC_ARENA_FN(os_alloc_arena)
{
	Arena result = {0};
	capacity   = round_up_to(capacity, os_get_page_size());
	result.beg = VirtualAlloc(0, capacity, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
	if (result.beg) {
		result.end = result.beg + capacity;
		asan_poison_region(result.beg, result.end - result.beg);
	}
	return result;
}

BEAMFORMER_IMPORT OS_READ_ENTIRE_FILE_FN(os_read_entire_file)
{
	i64 result = 0;
	w32_file_info fileinfo;
	iptr h = CreateFileA((c8 *)file, GENERIC_READ, 0, 0, OPEN_EXISTING, 0, 0);
	if (h >= 0 && GetFileInformationByHandle(h, &fileinfo)) {
		iz filesize  = (iz)fileinfo.nFileSizeHigh << 32;
		filesize    |= (iz)fileinfo.nFileSizeLow;
		if (buffer_capacity >= filesize) {
			result = filesize;
			i32 rlen;
			if (!ReadFile(h, buffer, (i32)filesize, &rlen, 0) || rlen != filesize)
				result = 0;
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
			i64 length = MIN(raw.len, (iz)GB(2));
			result     = os_write_file(h, raw.data, length);
			if (!result) break;
			raw = s8_cut_head(raw, length);
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

function b32
os_copy_file(char *name, char *new)
{
	return CopyFileA(name, new, 0);
}

BEAMFORMER_IMPORT OS_WAIT_ON_ADDRESS_FN(os_wait_on_address)
{
	return WaitOnAddress(value, &current, sizeof(*value), timeout_ms);
}

BEAMFORMER_IMPORT OS_WAKE_ALL_WAITERS_FN(os_wake_all_waiters)
{
	if (sync) {
		atomic_store_u32(sync, 0);
		WakeByAddressAll(sync);
	}
}

BEAMFORMER_IMPORT OSW32Semaphore
os_w32_create_semaphore(const char *name, i32 initial_count, i32 maximum_count)
{
	OSW32Semaphore result = {(u64)CreateSemaphoreA(0, initial_count, maximum_count, (c8 *)name)};
	return result;
}

BEAMFORMER_IMPORT u32
os_w32_semaphore_wait(OSW32Semaphore handle, u32 timeout_ms)
{
	b32 result = !WaitForSingleObject(handle.value[0], timeout_ms);
	return result;
}

BEAMFORMER_IMPORT void
os_w32_semaphore_release(OSW32Semaphore handle, i32 count)
{
	ReleaseSemaphore(handle.value[0], count, 0);
}
