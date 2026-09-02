/* See LICENSE for license details. */

/* NOTE(rnp): provides the platform layer for everything in this repo. */

#define OS_SHARED_MEMORY_NAME "Local\\ogl_beamformer_parameters"

#define OS_PATH_SEPARATOR_CHAR '\\'
#define OS_PATH_SEPARATOR      "\\"

#include "base_platform.h"

#include "util.h"

#define STD_INPUT_HANDLE  -10
#define STD_OUTPUT_HANDLE -11
#define STD_ERROR_HANDLE  -12

#define PAGE_READONLY  0x02
#define PAGE_READWRITE 0x04
#define MEM_COMMIT     0x1000
#define MEM_RESERVE    0x2000
#define MEM_DECOMMIT   0x4000
#define MEM_RELEASE    0x8000

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
	i64  minimum_application_address;
	i64  maximum_application_address;
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

typedef struct {
	u32 reserved1;
	u32 reserved2;
	u64 Reserved3[2];
	u32 reserved4;
	u32 reserved5;
} w32_synchronization_barrier;

typedef enum {
	W32IOEvent_FileWatch,
} W32IOEvent;

typedef struct {
	u64  tag;
	iptr context;
} w32_io_completion_event;

#define W32(r) __declspec(dllimport) r __stdcall
W32(b32)    CloseHandle(iptr);
W32(b32)    CopyFileA(c8 *, c8 *, b32);
W32(iptr)   CreateFileA(c8 *, u32, u32, void *, u32, u32, void *);
W32(iptr)   CreateFileMappingA(iptr, void *, u32, u32, u32, c8 *);
W32(iptr)   CreateIoCompletionPort(iptr, iptr, uptr, u32);
W32(iptr)   CreateSemaphoreA(iptr, i32, i32, c8 *);
W32(u64)    CreateThread(iptr, u64, iptr, iptr, u32, u32 *);
W32(b32)    DeleteFileA(c8 *);
W32(b32)    EnterSynchronizationBarrier(w32_synchronization_barrier *, u32);
W32(void)   ExitProcess(i32);
W32(i32)    GetFileAttributesA(c8 *);
W32(b32)    GetFileInformationByHandle(iptr, void *);
W32(i32)    GetLastError(void);
W32(b32)    GetQueuedCompletionStatus(iptr, u32 *, uptr *, w32_overlapped **, u32);
W32(iptr)   GetStdHandle(i32);
W32(void)   GetSystemInfo(w32_system_info *);
W32(b32)    InitializeSynchronizationBarrier(w32_synchronization_barrier *, i32, i32);
W32(void *) MapViewOfFile(iptr, u32, u32, u32, u64);
W32(b32)    QueryPerformanceCounter(u64 *);
W32(b32)    QueryPerformanceFrequency(u64 *);
W32(b32)    ReadDirectoryChangesW(iptr, u8 *, u32, b32, u32, u32 *, void *, void *);
W32(b32)    ReadFile(iptr, u8 *, i32, i32 *, void *);
W32(b32)    ReleaseSemaphore(iptr, i32, i32 *);
W32(i32)    SetThreadDescription(u64, u16 *);
W32(u32)    WaitForSingleObject(iptr, u32);
W32(b32)    WaitOnAddress(void *, void *, u64, u32);
W32(i32)    WakeByAddressAll(void *);
W32(b32)    WriteFile(iptr, u8 *, i32, i32 *, void *);
W32(void *) VirtualAlloc(u8 *, i64, u32, u32);
W32(b32)    VirtualFree(void *, u64, u32);
W32(b32)    VirtualProtect(void *, u64, u32, u32 *);

enum {OSW32_FileWatchDirectoryBufferSize = KB(4)};

typedef struct OSW32Entity OSW32Entity;
typedef struct {
	void *handle;
	OSW32Entity *prev, *next;
} OSW32Window;

typedef enum {
	OSW32FileWatchKind_Platform,
	OSW32FileWatchKind_User,
} OSW32FileWatchKind;

typedef struct OSW32FileWatchDirectory OSW32FileWatchDirectory;
typedef struct OSW32FileWatch OSW32FileWatch;
struct OSW32FileWatch {
	OSW32FileWatchKind  kind;
	u64                 hash;
	u64                 update_time;
	void               *user_context;

	OSW32FileWatchDirectory *parent;
	OSW32FileWatch *prev, *next;
};

struct OSW32FileWatchDirectory {
	u64  hash;
	i64  handle;
	str8 name;

	OSW32FileWatch *first_child;
	OSW32FileWatch *last_child;
	OSW32FileWatchDirectory *prev, *next;

	w32_overlapped          overlapped;
  w32_io_completion_event event;

	void *buffer;
};

typedef enum {
	OSW32EntityKind_Window,
	OSW32EntityKind_FileWatch,
	OSW32EntityKind_FileWatchDirectory,
} OSW32EntityKind;

struct OSW32Entity {
	OSW32EntityKind kind;
	union {
		OSW32FileWatch          file_watch;
		OSW32FileWatchDirectory file_watch_directory;
		OSW32Window             window;
	} as;
	OSW32Entity *next;
};

typedef struct {
	OSSystemInfo  system_info;

	Arena        *arena;
	i32           arena_lock;
	i64           error_handle;
	i64           io_completion_handle;

	struct {
		OSW32FileWatchDirectory *first;
		OSW32FileWatchDirectory *last;
	} file_watch_directories;

	struct {
		OSW32Entity *first;
		OSW32Entity *last;
	} windows;

	OSW32Entity *entity_freelist;
} OSW32_Context;
global OSW32_Context os_w32_context;

function b32
os_write_file(iptr file, void *data, i64 length)
{
	i32 wlen = 0;
	if (length > 0 && length <= (i64)U32_MAX) WriteFile(file, data, (i32)length, &wlen, 0);
	return length == wlen;
}

BASE_EXPORT no_return void
os_exit(i32 code)
{
	ExitProcess(code);
	unreachable();
}

function u64
os_timer_frequency(void)
{
	u64 result;
	QueryPerformanceFrequency(&result);
	return result;
}

BASE_EXPORT u64
os_timer_count(void)
{
	u64 result;
	QueryPerformanceCounter(&result);
	return result;
}

function void
os_system_info_init(void)
{
	w32_system_info info = {0};
	GetSystemInfo(&info);

	os_w32_context.system_info.timer_frequency         = os_timer_frequency();
	os_w32_context.system_info.logical_processor_count = info.number_of_processors;
	os_w32_context.system_info.page_size               = info.page_size;
	os_w32_context.system_info.path_separator_byte     = '\\';
}

BASE_EXPORT OSSystemInfo *
os_system_info(void)
{
	#if BASE_PLATFORM_NO_MAIN
	if unlikely(os_w32_context.system_info.path_separator_byte == 0)
		os_system_info_init();
	#endif
	return &os_w32_context.system_info;
}

BASE_EXPORT void *
os_memory_reserve(u64 size)
{
	void *result = VirtualAlloc(0, size, MEM_RESERVE, PAGE_READWRITE);
	return result;
}

BASE_EXPORT void
os_memory_release(void *base, u64 size)
{
	// NOTE(rnp): size must be 0 on w32, no partial releasing
	VirtualFree(base, 0, MEM_RELEASE);
}

BASE_EXPORT b32
os_memory_commit(void *base, u64 size)
{
	b32 result = VirtualAlloc(base, size, MEM_COMMIT, PAGE_READWRITE) != 0;
	return result;
}

BASE_EXPORT void
os_memory_uncommit(void *base, u64 size)
{
	VirtualFree(base, size, MEM_DECOMMIT);
}

BASE_EXPORT void
os_memory_seal(void *base, u64 size)
{
	u32 w32_dummy;
	VirtualProtect(base, size, PAGE_READONLY, &w32_dummy);
}

BASE_EXPORT OSW32Semaphore
os_w32_create_semaphore(const char *name, i32 initial_count, i32 maximum_count)
{
	OSW32Semaphore result = {(u64)CreateSemaphoreA(0, initial_count, maximum_count, (c8 *)name)};
	return result;
}

BASE_EXPORT u32
os_w32_semaphore_wait(OSW32Semaphore handle, u32 timeout_ms)
{
	b32 result = !WaitForSingleObject(handle.value[0], timeout_ms);
	return result;
}

BASE_EXPORT void
os_w32_semaphore_release(OSW32Semaphore handle, i32 count)
{
	ReleaseSemaphore(handle.value[0], count, 0);
}

BASE_EXPORT str8
os_read_entire_file(Arena *arena, const char *file)
{
	str8 result = {0};
	w32_file_info fileinfo;
	iptr h = CreateFileA((c8 *)file, GENERIC_READ, 0, 0, OPEN_EXISTING, 0, 0);
	if (h >= 0 && GetFileInformationByHandle(h, &fileinfo)) {
		i64 filesize  = (i64)fileinfo.nFileSizeHigh << 32;
		filesize     |= (i64)fileinfo.nFileSizeLow;
		result.data   = push_array(arena, u8, filesize);
		result.length = filesize;
		i32 rlen;
		if (!ReadFile(h, result.data, (i32)filesize, &rlen, 0) || rlen != filesize) {
			arena_pop(arena, filesize);
			zero_struct(&result);
		}
	}
	if (h >= 0) CloseHandle(h);

	return result;
}

function b32
os_write_new_file(char *fname, str8 raw)
{
	b32 result = 0;
	iptr h = CreateFileA(fname, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, 0, 0);
	if (h >= 0) {
		while (raw.length > 0) {
			i64 length = Min(raw.length, (i64)GB(2));
			result     = os_write_file(h, raw.data, length);
			if (!result) break;
			raw = str8_cut_head(raw, length);
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

function OSW32Entity *
os_entity_allocate(OSW32EntityKind kind)
{
	OSW32Entity *result = 0;
	DeferLoop(take_lock(&os_w32_context.arena_lock, -1), release_lock(&os_w32_context.arena_lock))
	{
		result = SLLPopFreelist(os_w32_context.entity_freelist);
		if (!result) result = push_struct_no_zero(os_w32_context.arena, OSW32Entity);
	}

	zero_struct(result);
	result->kind = kind;
	return result;
}

BASE_EXPORT OSThread
os_create_thread(const char *name, void *user_context, os_thread_entry_point_fn *fn)
{
	OSThread result = {(u64)CreateThread(0, 0, (iptr)fn, (iptr)user_context, 0, 0)};
	if (result.value[0]) {
		Temp scratch;
		DeferLoop(take_lock(&os_w32_context.arena_lock, -1), release_lock(&os_w32_context.arena_lock))
		DeferLoop(scratch = temp_begin(os_w32_context.arena), temp_end(scratch))
		{
			SetThreadDescription(result.value[0], str16_from_str8(scratch.arena, str8_from_c_str((c8 *)name)).data);
		}
	} else {
		result.value[0] = OSInvalidHandleValue;
	}
	return result;
}

BASE_EXPORT OSBarrier
os_barrier_alloc(u32 count)
{
	OSBarrier result = {0};
	DeferLoop(take_lock(&os_w32_context.arena_lock, -1), release_lock(&os_w32_context.arena_lock))
	{
		w32_synchronization_barrier *barrier = push_struct(os_w32_context.arena, w32_synchronization_barrier);
		InitializeSynchronizationBarrier(barrier, (i32)count, -1);
		result.value[0] = (u64)barrier;
	}
	return result;
}

BASE_EXPORT void
os_barrier_enter(OSBarrier barrier)
{
	w32_synchronization_barrier *b = (w32_synchronization_barrier *)barrier.value[0];
	if (b) EnterSynchronizationBarrier(b, 0);
}

BASE_EXPORT b32
os_wait_on_address(i32 *value, i32 current, u32 timeout_ms)
{
	return WaitOnAddress(value, &current, sizeof(*value), timeout_ms);
}

BASE_EXPORT void
os_wake_all_waiters(i32 *sync)
{
	if (sync) {
		atomic_store_u32(sync, 0);
		WakeByAddressAll(sync);
	}
}

#if !BASE_PLATFORM_NO_MAIN
BASE_IMPORT void entry_point(i32 argc, char *argv[]);

extern i32
main(i32 argc, char *argv[])
{
	os_system_info_init();
	os_w32_context.arena        = arena_create(.name = "Platform Arena");
	os_w32_context.error_handle = GetStdHandle(STD_ERROR_HANDLE);

	entry_point(argc, argv);

	return 0;
}
#endif
