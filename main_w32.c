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

#define OS_SHARED_MEMORY_SIZE  GB(2)

#define OS_DEBUG_LIB_NAME      ".\\beamformer.dll"
#define OS_DEBUG_LIB_TEMP_NAME ".\\beamformer_temp.dll"

#define OS_CUDA_LIB_NAME       "external\\cuda_toolkit.dll"
#define OS_CUDA_LIB_TEMP_NAME  "external\\cuda_toolkit_temp.dll"

#define OS_RENDERDOC_SONAME    "renderdoc.dll"

#define OS_VULKAN_SONAME_LIST \
	X("vulkan-1.dll") \

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
	Arena         arena;
	i32           arena_lock;
	i64           error_handle;
	i64           io_completion_handle;

	BeamformerInput *input;

	struct {
		OSW32FileWatchDirectory *first;
		OSW32FileWatchDirectory *last;
	} file_watch_directories;

	struct {
		OSW32Entity *first;
		OSW32Entity *last;
	} windows;

	OSW32Entity *entity_freelist;

	OSSystemInfo system_info;
} OSW32_Context;
global OSW32_Context os_w32_context;

function OSW32Entity *
os_entity_allocate(OSW32EntityKind kind)
{
	OSW32Entity *result = 0;
	DeferLoop(take_lock(&os_w32_context.arena_lock, -1), release_lock(&os_w32_context.arena_lock))
	{
		result = SLLPopFreelist(os_w32_context.entity_freelist);
		if (!result) result = push_struct_no_zero(&os_w32_context.arena, OSW32Entity);
	}

	zero_struct(result);
	result->kind = kind;
	return result;
}

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
			SetThreadDescription(result.value[0], str16_from_str8(&arena, str8_from_c_str((c8 *)name)).data);
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

function OSW32FileWatchDirectory *
os_lookup_file_watch_directory(u64 hash)
{
	OSW32FileWatchDirectory *result = 0;
	for (OSW32FileWatchDirectory *fwd = os_w32_context.file_watch_directories.first; fwd; fwd = fwd->next) {
		if (fwd->hash == hash) {
			result = fwd;
			break;
		}
	}
	return result;
}

function void
os_w32_add_file_watch(str8 path, void *user_context, OSW32FileWatchKind kind)
{
	str8 directory   = path;
	directory.length = str8_scan_backwards(path, '\\');
	assert(directory.length > 0);

	u64 hash = u64_hash_from_str8(directory);
	OSW32FileWatchDirectory *dir = os_lookup_file_watch_directory(hash);
	if (!dir) {
		assert(path.data[directory.length] == '\\');

		OSW32Entity *fwd = os_entity_allocate(OSW32EntityKind_FileWatchDirectory);
		dir = &fwd->as.file_watch_directory;
		DLLInsert(0, os_w32_context.file_watch_directories.first,
		          os_w32_context.file_watch_directories.last, dir, next, prev);

		dir->hash   = hash;
		dir->name   = push_str8(&os_w32_context.arena, directory);
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

	OSW32Entity    *fwe = os_entity_allocate(OSW32EntityKind_FileWatch);
	OSW32FileWatch *fw  = &fwe->as.file_watch;
	DLLInsert(0, dir->first_child, dir->last_child, fw, next, prev);
	fw->user_context = user_context;
	fw->hash         = u64_hash_from_str8(str8_cut_head(path, dir->name.length + 1));
	fw->kind         = kind;
	fw->parent       = dir;
}

BEAMFORMER_IMPORT void
os_add_file_watch(const char *path, int64_t path_length, void *user_context)
{
	str8 path_str = {.data = (u8 *)path, .length = path_length};
	os_w32_add_file_watch(path_str, user_context, OSW32FileWatchKind_User);
}

function void
os_window_resize_callback(void *window, i32 width, i32 height)
{
	OSWindow event_window = {0};
	for (OSW32Entity *we = os_w32_context.windows.first; we; we = we->as.window.next) {
		if (we->as.window.handle == window) {
			event_window.value[0] = (u64)we;
			break;
		}
	}

	os_push_input_event(beamformer_input, (BeamformerInputEvent){
		.kind      = BeamformerInputEventKind_WindowResize,
		.window_resize = {
			.width = (u32)width, .height = (u32)height,
			.window = event_window,
		},
	});

	raylib_window_resize(window, width, height);
}

BEAMFORMER_IMPORT OSWindow
os_window_create(u8 *title, i64 title_length, i32 width, i32 height)
{
	OSW32Entity *we = os_entity_allocate(OSW32EntityKind_Window);
	OSWindow result = {(u64)we};
	DLLInsert(0, os_w32_context.windows.first, os_w32_context.windows.last, we, as.window.next, as.window.prev);

	SetConfigFlags(FLAG_VSYNC_HINT|FLAG_WINDOW_ALWAYS_RUN);

	str8 name = {.data = title, .length = title_length};
	DeferLoop(take_lock(&os_w32_context.arena_lock, -1), release_lock(&os_w32_context.arena_lock))
	{
		Arena scratch = os_w32_context.arena;
		name.length = Min(name.length, arena_capacity(&scratch, u8) - 1);
		str8 title_string = push_str8(&scratch, name);
		InitWindow(width, height, (char *)title_string.data);
	}

	we->as.window.handle = GetPlatformWindowHandle();
	os_window_equip_common(os_w32_context.input, we->as.window.handle);
	raylib_window_resize = glfwSetWindowSizeCallback(we->as.window.handle, os_window_resize_callback);

	/* NOTE: do this after initing so that the window starts out floating in tiling wm */
	SetWindowState(FLAG_WINDOW_RESIZABLE);
	SetWindowMinSize(320, 240);

	return result;
}

BEAMFORMER_IMPORT u8 *
os_get_clipboard_text(i64 *length)
{
	str8 result = str8_from_c_str((char *)GetClipboardText());
	if (length) *length = result.length;
	return result.data;
}

BEAMFORMER_IMPORT void
os_set_clipboard_text(u8 *data, i64 length)
{
	DeferLoop(take_lock(&os_w32_context.arena_lock, -1), release_lock(&os_w32_context.arena_lock))
	{
		Arena scratch     = os_w32_context.arena;
		str8  string      = {.data = data, .length = Min(length, arena_capacity(&scratch, u8) - 1)};
		str8  safe_string = push_str8(&scratch, string);
		SetClipboardText((char *)safe_string.data);
	}
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

	if ValidHandle(beamformer_library_handle) {
		beamformer_debug_hot_release(input);
		FreeLibrary(beamformer_library_handle.value[0]);
		beamformer_library_handle = (OSLibrary){OSInvalidHandleValue};
	}

	OSLibrary new_handle = load_library(OS_DEBUG_LIB_NAME, OS_DEBUG_LIB_TEMP_NAME);
	if (InvalidHandle(beamformer_library_handle) && InvalidHandle(new_handle))
		fatal(s8("[os] failed to load: " OS_DEBUG_LIB_NAME "\n"));

	if ValidHandle(new_handle) {
		beamformer_debug_hot_reload(new_handle, input);
		beamformer_library_handle = new_handle;
	}
}
#else
#define debug_library_reload(a) (void)(a)
#endif /* BEAMFORMER_DEBUG */

function void
load_platform_libraries(BeamformerInput *input)
{
	#if BEAMFORMER_DEBUG
		debug_library_reload(input);
		os_w32_add_file_watch(str8(OS_DEBUG_LIB_NAME), (void *)BeamformerInputEventKind_ExecutableReload,
		                      OSW32FileWatchKind_Platform);
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
dispatch_file_watch(BeamformerInput *input, Arena arena, u64 current_time, OSW32FileWatchDirectory *fw_dir)
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

		str8 file_name = str8_from_str16(&arena, (str16){.data = fni->filename, .length = fni->filename_size / 2});
		u64  hash      = u64_hash_from_str8(file_name);
		for (OSW32FileWatch *fw = fw_dir->first_child; fw; fw = fw->next) if (fw->hash == hash) {
			// NOTE(rnp): avoid multiple updates in a single frame
			if (fw->update_time < current_time) {
				BeamformerInputEvent input_event = {0};
				if (fw->kind == OSW32FileWatchKind_Platform) {
					assert((u64)fw->user_context == BeamformerInputEventKind_ExecutableReload);
					if ((u64)fw->user_context == BeamformerInputEventKind_ExecutableReload)
						debug_library_reload(input);
					input_event.kind = (u64)fw->user_context;
				} else {
					input_event.kind = BeamformerInputEventKind_FileEvent;
					input_event.file_watch_user_context = fw->user_context;
				}
				os_push_input_event(input, input_event);
			}
			fw->update_time = current_time;
			break;
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
			OSW32FileWatchDirectory *dir = (OSW32FileWatchDirectory *)event->context;
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
	os_w32_context.input   = input;
	input->memory          = program_memory.beg;
	input->memory_size     = program_memory.end - program_memory.beg;
	input->shared_memory   = allocate_shared_memory(OS_SHARED_MEMORY_NAME, OS_SHARED_MEMORY_SIZE,
	                                                &input->shared_memory_size);
	if (input->shared_memory) {
		input->shared_memory_name        = s8(OS_SHARED_MEMORY_NAME).data;
		input->shared_memory_name_length = s8(OS_SHARED_MEMORY_NAME).len;
	}

	os_push_input_event(input, (BeamformerInputEvent){
		.kind = BeamformerInputEventKind_ExecutableReload,
	});

	load_platform_libraries(input);

	beamformer_init(input);

	while (!WindowShouldClose() && !beamformer_should_close(input)) {
		os_build_frame_input(input);

		DeferLoop(take_lock(&os_w32_context.arena_lock, -1), release_lock(&os_w32_context.arena_lock))
		{
			clear_io_queue(input, os_w32_context.arena);
		}

		beamformer_frame_step(input);

		// NOTE(rnp): this must happen at the end of frame to allow the pre loop events through
		// TODO(rnp): hack: until raylib is removed this happens in ui since raylib will cause
		// glfw to call the input callbacks in during EndDrawing()
		//input->event_count = 0;
	}

	beamformer_terminate(input);
}
