/* See LICENSE for license details. */
#include "compiler.h"

// NOTE(rnp): for test compilations on linux (we don't use headers from windows) */
#if OS_LINUX
  #undef OS_WINDOWS
  #undef OS_LINUX
  #undef __declspec
  #undef __stdcall
  #define OS_WINDOWS 1
  #define OS_LINUX   0
  #define __declspec(x)
  #define __stdcall
  #define _WIN32
#endif

#if !OS_WINDOWS
#error This file is only meant to be compiled for Win32
#endif

#ifdef BEAMFORMER_DEBUG
  #define BEAMFORMER_IMPORT __declspec(dllexport)
#else
  #define BEAMFORMER_IMPORT static
  #define BEAMFORMER_EXPORT static
#endif

#include "beamformer.c"
#include "os_win32.c"

typedef struct {
	u32 reserved1;
	u32 reserved2;
	u64 Reserved3[2];
	u32 reserved4;
	u32 reserved5;
} w32_synchronization_barrier;

W32(u64)    CreateThread(iptr, uz, iptr, iptr, u32, u32 *);
W32(b32)    EnterSynchronizationBarrier(w32_synchronization_barrier *, u32);
W32(b32)    FreeLibrary(u64);
W32(void *) GetModuleHandleA(const c8 *);
W32(void *) GetProcAddress(u64, const c8 *);
W32(b32)    InitializeSynchronizationBarrier(w32_synchronization_barrier *, i32, i32);
W32(void *) LoadLibraryA(const c8 *);
W32(i32)    SetThreadDescription(u64, u16 *);

#define OS_DEBUG_LIB_NAME      ".\\beamformer.dll"
#define OS_DEBUG_LIB_TEMP_NAME ".\\beamformer_temp.dll"

#define OS_CUDA_LIB_NAME       "external\\cuda_toolkit.dll"
#define OS_CUDA_LIB_TEMP_NAME  "external\\cuda_toolkit_temp.dll"

#define OS_RENDERDOC_SONAME    "renderdoc.dll"

#define OS_VULKAN_SONAME_LIST \
	X("vulkan-1.dll") \

enum {OSW32_FileWatchDirectoryBufferSize = KB(4)};
typedef enum {
	OSW32_FileWatchKindPlatform,
	OSW32_FileWatchKindUser,
} OSW32_FileWatchKind;

typedef struct {
	OSW32_FileWatchKind kind;
	u64                 hash;
	u64                 update_time;
	void *              user_context;
} OSW32_FileWatch;

typedef struct {
	u64  hash;
	iptr handle;
	s8   name;

	OSW32_FileWatch *data;
	iz               count;
	iz               capacity;

	w32_overlapped          overlapped;
  w32_io_completion_event event;

	void *buffer;
} OSW32_FileWatchDirectory;
DA_STRUCT(OSW32_FileWatchDirectory, OSW32_FileWatchDirectory);

typedef struct {
	Arena         arena;
	i32           arena_lock;
	iptr          error_handle;
	iptr          io_completion_handle;

	OSW32_FileWatchDirectoryList file_watch_list;

	OSSystemInfo system_info;
} OSW32_Context;
global OSW32_Context os_w32_context;

BEAMFORMER_IMPORT OSSystemInfo *
os_system_info(void)
{
	return &os_w32_context.system_info;
}

BEAMFORMER_IMPORT OSThread
os_create_thread(const char *name, void *user_context, os_thread_entry_point_fn *fn)
{
	OSThread result = {(u64)CreateThread(0, 0, (iptr)fn, (iptr)user_context, 0, 0)};
	if (result.value[0]) {
		DeferLoop(take_lock(&os_w32_context.arena_lock, -1), release_lock(&os_w32_context.arena_lock))
		{
			Arena arena = os_w32_context.arena;
			SetThreadDescription(result.value[0], s8_to_s16(&arena, c_str_to_s8((c8 *)name)).data);
		}
	} else {
		result.value[0] = OSInvalidHandleValue;
	}
	return result;
}

BEAMFORMER_IMPORT OSBarrier
os_barrier_alloc(u32 count)
{
	OSBarrier result = {0};
	DeferLoop(take_lock(&os_w32_context.arena_lock, -1), release_lock(&os_w32_context.arena_lock))
	{
		w32_synchronization_barrier *barrier = push_struct(&os_w32_context.arena, w32_synchronization_barrier);
		InitializeSynchronizationBarrier(barrier, (i32)count, -1);
		result.value[0] = (u64)barrier;
	}
	return result;
}

BEAMFORMER_IMPORT void
os_barrier_enter(OSBarrier barrier)
{
	w32_synchronization_barrier *b = (w32_synchronization_barrier *)barrier.value[0];
	if (b) EnterSynchronizationBarrier(b, 0);
}

BEAMFORMER_IMPORT void
os_console_log(u8 *data, i64 length)
{
	os_write_file(os_w32_context.error_handle, data, length);
}

BEAMFORMER_IMPORT void
os_fatal(u8 *data, i64 length)
{
	os_write_file(os_w32_context.error_handle, data, length);
	os_exit(1);
	unreachable();
}

BEAMFORMER_IMPORT void *
os_lookup_symbol(OSLibrary library, const char *symbol)
{
	void *result = 0;
	if ValidHandle(library) result = GetProcAddress(library.value[0], symbol);
	return result;
}

function void *
allocate_shared_memory(char *name, iz requested_capacity, u64 *capacity)
{
	u64 rounded_capacity = round_up_to(requested_capacity, os_w32_context.system_info.page_size);
	void *result = 0;
	iptr h = CreateFileMappingA(-1, 0, PAGE_READWRITE, (rounded_capacity >> 32u),
	                            (rounded_capacity & 0xFFFFFFFFul), name);
	if (h != INVALID_FILE) {
		result = MapViewOfFile(h, FILE_MAP_ALL_ACCESS, 0, 0, rounded_capacity);
		if (result) *capacity = rounded_capacity;
	}
	return result;
}

function OSW32_FileWatchDirectory *
os_lookup_file_watch_directory(OSW32_FileWatchDirectoryList *ctx, u64 hash)
{
	OSW32_FileWatchDirectory *result = 0;
	for (iz i = 0; !result && i < ctx->count; i++)
		if (ctx->data[i].hash == hash)
			result = ctx->data + i;
	return result;
}

function void
os_w32_add_file_watch(s8 path, void *user_context, OSW32_FileWatchKind kind)
{
	s8 directory  = path;
	directory.len = s8_scan_backwards(path, '\\');
	assert(directory.len > 0);

	OSW32_FileWatchDirectoryList *fwctx = &os_w32_context.file_watch_list;

	u64 hash = u64_hash_from_s8(directory);
	OSW32_FileWatchDirectory *dir = os_lookup_file_watch_directory(fwctx, hash);
	if (!dir) {
		assert(path.data[directory.len] == '\\');

		dir = da_push(&os_w32_context.arena, fwctx);
		dir->hash   = hash;
		dir->name   = push_s8(&os_w32_context.arena, directory);
		dir->handle = CreateFileA((c8 *)dir->name.data, GENERIC_READ, FILE_SHARE_READ, 0,
		                          OPEN_EXISTING,
		                          FILE_FLAG_BACKUP_SEMANTICS|FILE_FLAG_OVERLAPPED, 0);

		dir->event.tag     = W32IOEvent_FileWatch;
		dir->event.context = (iptr)dir;
		CreateIoCompletionPort(dir->handle, os_w32_context.io_completion_handle, (uptr)&dir->event, 0);

		dir->buffer = arena_alloc(&os_w32_context.arena, .size = OSW32_FileWatchDirectoryBufferSize);
		ReadDirectoryChangesW(dir->handle, dir->buffer, OSW32_FileWatchDirectoryBufferSize, 0,
		                      FILE_NOTIFY_CHANGE_LAST_WRITE, 0, &dir->overlapped, 0);
	}

	OSW32_FileWatch *fw = da_push(&os_w32_context.arena, dir);
	fw->user_context = user_context;
	fw->hash         = u64_hash_from_s8(s8_cut_head(path, dir->name.len + 1));
	fw->kind         = kind;
}

BEAMFORMER_IMPORT void
os_add_file_watch(const char *path, int64_t path_length, void *user_context)
{
	s8 path_str = {.data = (u8 *)path, .len = path_length};
	os_w32_add_file_watch(path_str, user_context, OSW32_FileWatchKindUser);
}

#if BEAMFORMER_RENDERDOC_HOOKS
function OSLibrary
get_module(char *name)
{
	OSLibrary result = {(u64)GetModuleHandleA(name)};
	if (result.value[0] == 0) result.value[0] = OSInvalidHandleValue;
	return result;
}
#endif

function OSLibrary
load_library(char *name, char *temp_name)
{
	if (temp_name && os_copy_file(name, temp_name))
		name = temp_name;

	OSLibrary result = {(u64)LoadLibraryA(name)};
	if (result.value[0] == 0) result.value[0] = OSInvalidHandleValue;

	if (temp_name) DeleteFileA(temp_name);

	return result;
}

#if BEAMFORMER_DEBUG
function void
debug_library_reload(BeamformerInput *input)
{
	local_persist OSLibrary beamformer_library_handle = {OSInvalidHandleValue};
	OSLibrary new_handle = load_library(OS_DEBUG_LIB_NAME, OS_DEBUG_LIB_TEMP_NAME);

	if (InvalidHandle(beamformer_library_handle) && InvalidHandle(new_handle))
		fatal(s8("[os] failed to load: " OS_DEBUG_LIB_NAME "\n"));

	if ValidHandle(new_handle) {
		beamformer_debug_hot_reload(new_handle, input);

		if ValidHandle(beamformer_library_handle)
			FreeLibrary(beamformer_library_handle.value[0]);
		beamformer_library_handle = new_handle;
	}
}
#endif /* BEAMFORMER_DEBUG */

function void
load_platform_libraries(BeamformerInput *input)
{
	#if BEAMFORMER_DEBUG
		debug_library_reload(input);
		os_w32_add_file_watch(s8(OS_DEBUG_LIB_NAME), (void *)BeamformerInputEventKind_ExecutableReload,
		                      OSW32_FileWatchKindPlatform);
	#endif

	input->vulkan_library_handle = (OSLibrary){OSInvalidHandleValue};
	#define X(name) \
		if InvalidHandle(input->vulkan_library_handle) \
			input->vulkan_library_handle = load_library(name, 0);
	OS_VULKAN_SONAME_LIST
	#undef X

	if InvalidHandle(input->vulkan_library_handle)
		fatal(s8("[os] fatal error: failed to find valid vulkan library\n"));

	input->cuda_library_handle = load_library(OS_CUDA_LIB_NAME, OS_CUDA_LIB_TEMP_NAME);

	#if BEAMFORMER_RENDERDOC_HOOKS
	local_persist OSLibrary renderdoc_handle = {OSInvalidHandleValue};
	renderdoc_handle = get_module(OS_RENDERDOC_SONAME);
	load_renderdoc_functions(input, renderdoc_handle);
	#endif
}

function void
dispatch_file_watch(BeamformerInput *input, Arena arena, u64 current_time, OSW32_FileWatchDirectory *fw_dir)
{
	TempArena save_point = {0};
	i64       offset     = 0;

	w32_file_notify_info *fni = (w32_file_notify_info *)fw_dir->buffer;
	do {
		end_temp_arena(save_point);
		save_point = begin_temp_arena(&arena);

		Stream e = {.data = arena_commit(&arena, KB(1)), .cap = KB(1)};

		if (fni->action != FILE_ACTION_MODIFIED) {
			stream_append_s8(&e, s8("[os] unknown file watch event: "));
			stream_append_u64(&e, fni->action);
			stream_append_byte(&e, '\n');
			os_write_file(os_w32_context.error_handle, e.data, e.widx);
			stream_reset(&e, 0);
		}

		s8 file_name = s16_to_s8(&arena, (s16){.data = fni->filename, .len  = fni->filename_size / 2});
		u64 hash = u64_hash_from_s8(file_name);
		for (u32 i = 0; i < fw_dir->count; i++) {
			OSW32_FileWatch *fw = fw_dir->data + i;
			if (fw->hash == hash) {
				// NOTE(rnp): avoid multiple updates in a single frame
				if (fw->update_time < current_time) {
					BeamformerInputEvent input_event = {0};
					if (fw->kind == OSW32_FileWatchKindPlatform) {
						assert((u64)fw->user_context == BeamformerInputEventKind_ExecutableReload);
						#if BEAMFORMER_DEBUG
							if ((u64)fw->user_context == BeamformerInputEventKind_ExecutableReload)
								debug_library_reload(input);
						#endif
						input_event.kind = (u64)fw->user_context;
					} else {
						input_event.kind = BeamformerInputEventKind_FileEvent;
						input_event.file_watch_user_context = fw->user_context;
					}
					input->event_queue[input->event_count++] = input_event;
				}
				fw->update_time = current_time;
				break;
			}
		}

		offset = fni->next_entry_offset;
		fni    = (w32_file_notify_info *)((u8 *)fni + offset);
	} while (offset);
}

function void
clear_io_queue(BeamformerInput *input, Arena arena)
{
	iptr handle = os_w32_context.io_completion_handle;
	w32_overlapped *overlapped;
	u32  bytes_read;
	uptr user_data;

	u64 current_time = os_timer_count();

	while (GetQueuedCompletionStatus(handle, &bytes_read, &user_data, &overlapped, 0)) {
		w32_io_completion_event *event = (w32_io_completion_event *)user_data;
		switch (event->tag) {
		case W32IOEvent_FileWatch:{
			OSW32_FileWatchDirectory *dir = (OSW32_FileWatchDirectory *)event->context;
			dispatch_file_watch(input, arena, current_time, dir);
			zero_struct(&dir->overlapped);
			ReadDirectoryChangesW(dir->handle, dir->buffer, OSW32_FileWatchDirectoryBufferSize, 0,
			                      FILE_NOTIFY_CHANGE_LAST_WRITE, 0, &dir->overlapped, 0);
		}break;
		}
	}
}

extern i32
main(void)
{
	os_w32_context.error_handle                    = GetStdHandle(STD_ERROR_HANDLE);
	os_w32_context.io_completion_handle            = CreateIoCompletionPort(INVALID_FILE, 0, 0, 0);
	os_w32_context.system_info.timer_frequency     = os_timer_frequency();
	os_w32_context.system_info.path_separator_byte = '\\';
	{
		w32_system_info info = {0};
		GetSystemInfo(&info);

		os_w32_context.system_info.page_size               = info.page_size;
		os_w32_context.system_info.logical_processor_count = info.number_of_processors;
	}

	Arena program_memory = os_alloc_arena(MB(16) + MB(2));
	os_w32_context.arena = sub_arena(&program_memory, MB(2), os_w32_context.system_info.page_size);

	BeamformerInput *input = push_struct(&program_memory, BeamformerInput);
	input->memory          = program_memory.beg;
	input->memory_size     = program_memory.end - program_memory.beg;
	input->shared_memory   = allocate_shared_memory(OS_SHARED_MEMORY_NAME, BEAMFORMER_SHARED_MEMORY_SIZE,
	                                                &input->shared_memory_size);
	if (input->shared_memory) {
		input->shared_memory_name        = s8(OS_SHARED_MEMORY_NAME).data;
		input->shared_memory_name_length = s8(OS_SHARED_MEMORY_NAME).len;
	}

	input->event_queue[input->event_count++] = (BeamformerInputEvent){
		.kind = BeamformerInputEventKind_ExecutableReload,
	};

	load_platform_libraries(input);

	beamformer_init(input);

	while (!WindowShouldClose() && !beamformer_should_close(input)) {
		DeferLoop(take_lock(&os_w32_context.arena_lock, -1), release_lock(&os_w32_context.arena_lock))
		{
			clear_io_queue(input, os_w32_context.arena);
		}

		Vector2 new_mouse = GetMousePosition();
		input->last_mouse_x = input->mouse_x;
		input->last_mouse_y = input->mouse_y;
		input->mouse_x      = new_mouse.x;
		input->mouse_y      = new_mouse.y;

		beamformer_frame_step(input);

		input->event_count  = 0;
	}

	beamformer_terminate(input);
}
