/* See LICENSE for license details. */
#include "compiler.h"

#if !OS_LINUX
#error This file is only meant to be compiled for Linux
#endif

#ifndef BEAMFORMER_DEBUG
  #define BEAMFORMER_IMPORT static
  #define BEAMFORMER_EXPORT static
#endif

#include "beamformer.c"
#include "os_linux.c"

#define OS_SHARED_MEMORY_SIZE  GB(2)

#define OS_DEBUG_LIB_NAME      "./beamformer.so"
#define OS_DEBUG_LIB_TEMP_NAME "./beamformer_temp.so"

#define OS_CUDA_LIB_NAME       "./external/cuda_toolkit.so"
#define OS_CUDA_LIB_TEMP_NAME  "./external/cuda_toolkit_temp.so"

#define OS_RENDERDOC_SONAME    "librenderdoc.so"

#define OS_VULKAN_SONAME_LIST \
	X("libvulkan.so") \
	X("libvulkan.so.1") \

#include <dlfcn.h>

typedef struct OSLinuxEntity OSLinuxEntity;
typedef struct {
	void          *handle;
	OSLinuxEntity *prev, *next;
} OSLinuxWindow;

typedef enum {
	OSLinuxFileWatchKind_Platform,
	OSLinuxFileWatchKind_User,
} OSLinuxFileWatchKind;

typedef struct OSLinuxFileWatchDirectory OSLinuxFileWatchDirectory;
typedef struct OSLinuxFileWatch OSLinuxFileWatch;
struct OSLinuxFileWatch {
	OSLinuxFileWatchKind  kind;
	u64                   hash;
	u64                   update_time;
	void                 *user_context;

	OSLinuxFileWatchDirectory *parent;
	OSLinuxFileWatch *prev, *next;
};

struct OSLinuxFileWatchDirectory {
	u64  hash;
	i64  handle;
	str8 name;

	OSLinuxFileWatch *first_child;
	OSLinuxFileWatch *last_child;
	OSLinuxFileWatchDirectory *prev, *next;
};

typedef enum {
	OSLinuxEntityKind_Window,
	OSLinuxEntityKind_FileWatch,
	OSLinuxEntityKind_FileWatchDirectory,
} OSLinuxEntityKind;

struct OSLinuxEntity {
	OSLinuxEntityKind kind;
	union {
		OSLinuxFileWatch          file_watch;
		OSLinuxFileWatchDirectory file_watch_directory;
		OSLinuxWindow             window;
	} as;
	OSLinuxEntity *next;
};

typedef struct {
	Arena         arena;
	i32           arena_lock;

	i32           inotify_handle;

	BeamformerInput *input;

	OSSystemInfo system_info;

	struct {
		OSLinuxFileWatchDirectory *first;
		OSLinuxFileWatchDirectory *last;
	} file_watch_directories;

	struct {
		OSLinuxEntity *first;
		OSLinuxEntity *last;
	} windows;

	OSLinuxEntity *entity_freelist;
} OSLinux_Context;
global OSLinux_Context os_linux_context;

function OSLinuxEntity *
os_entity_allocate(OSLinuxEntityKind kind)
{
	OSLinuxEntity *result = 0;
	DeferLoop(take_lock(&os_linux_context.arena_lock, -1), release_lock(&os_linux_context.arena_lock))
	{
		result = SLLPopFreelist(os_linux_context.entity_freelist);
		if (!result) result = push_struct_no_zero(&os_linux_context.arena, OSLinuxEntity);
	}

	zero_struct(result);
	result->kind = kind;
	return result;
}

BEAMFORMER_IMPORT OSSystemInfo *
os_system_info(void)
{
	return &os_linux_context.system_info;
}

BEAMFORMER_IMPORT OSThread
os_create_thread(const char *name, void *user_context, os_thread_entry_point_fn *fn)
{
	pthread_t thread;
	pthread_create(&thread, 0, (void *)fn, (void *)user_context);

	if (name) {
		char buffer[16];
		s8 name_str = c_str_to_s8((char *)name);
		u64  length = (u64)CLAMP(name_str.len, 0, countof(buffer) - 1);
		mem_copy(buffer, (char *)name, length);
		buffer[length] = 0;
		pthread_setname_np(thread, buffer);
	}

	OSThread result = {(u64)thread};
	return result;
}

BEAMFORMER_IMPORT OSBarrier
os_barrier_alloc(u32 count)
{
	OSBarrier result = {0};
	DeferLoop(take_lock(&os_linux_context.arena_lock, -1), release_lock(&os_linux_context.arena_lock))
	{
		pthread_barrier_t *barrier = push_struct(&os_linux_context.arena, pthread_barrier_t);
		pthread_barrier_init(barrier, 0, count);
		result.value[0] = (u64)barrier;
	}
	return result;
}

BEAMFORMER_IMPORT void
os_barrier_enter(OSBarrier barrier)
{
	pthread_barrier_t *b = (pthread_barrier_t *)barrier.value[0];
	if (b) pthread_barrier_wait(b);
}

BEAMFORMER_IMPORT void
os_console_log(u8 *data, i64 length)
{
	os_write_file(STDERR_FILENO, data, length);
}

BEAMFORMER_IMPORT void
os_fatal(u8 *data, i64 length)
{
	os_write_file(STDERR_FILENO, data, length);
	os_exit(1);
	unreachable();
}

BEAMFORMER_IMPORT void *
os_lookup_symbol(OSLibrary library, const char *symbol)
{
	void *result = 0;
	if ValidHandle(library) result = dlsym((void *)library.value[0], symbol);
	return result;
}

function void *
allocate_shared_memory(char *name, iz requested_capacity, u64 *capacity)
{
	u64 rounded_capacity = round_up_to(requested_capacity, ARCH_X64? KB(4) : os_linux_context.system_info.page_size);
	void *result = 0;
	i32 fd = shm_open(name, O_CREAT|O_RDWR, S_IRUSR|S_IWUSR);
	if (fd > 0 && ftruncate(fd, rounded_capacity) != -1) {
		void *new = mmap(0, rounded_capacity, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
		if (new != MAP_FAILED) {
			*capacity = rounded_capacity;
			result    = new;
		}
	}
	if (fd > 0) close(fd);
	return result;
}

function OSLinuxFileWatchDirectory *
os_lookup_file_watch_directory(u64 hash)
{
	OSLinuxFileWatchDirectory *result = 0;
	for (OSLinuxFileWatchDirectory *fwd = os_linux_context.file_watch_directories.first; fwd; fwd = fwd->next) {
		if (fwd->hash == hash) {
			result = fwd;
			break;
		}
	}
	return result;
}

function void
os_linux_add_file_watch(str8 path, void *user_context, OSLinuxFileWatchKind kind)
{
	str8 directory   = path;
	directory.length = str8_scan_backwards(path, '/');
	assert(directory.length > 0);

	u64 hash = u64_hash_from_str8(directory);
	OSLinuxFileWatchDirectory *dir = os_lookup_file_watch_directory(hash);
	if (!dir) {
		assert(path.data[directory.length] == '/');
		OSLinuxEntity *fwd = os_entity_allocate(OSLinuxEntityKind_FileWatchDirectory);
		dir = &fwd->as.file_watch_directory;
		DLLInsert(0, os_linux_context.file_watch_directories.first,
		          os_linux_context.file_watch_directories.last, dir, next, prev);

		dir->hash   = hash;
		dir->name   = push_str8(&os_linux_context.arena, directory);
		u32 mask    = IN_MOVED_TO|IN_CLOSE_WRITE;
		dir->handle = inotify_add_watch(os_linux_context.inotify_handle, (c8 *)dir->name.data, mask);
	}

	OSLinuxEntity    *fwe = os_entity_allocate(OSLinuxEntityKind_FileWatch);
	OSLinuxFileWatch *fw  = &fwe->as.file_watch;
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
	os_linux_add_file_watch(path_str, user_context, OSLinuxFileWatchKind_User);
}

function void
os_window_resize_callback(void *window, i32 width, i32 height)
{
	OSWindow event_window = {0};
	for (OSLinuxEntity *we = os_linux_context.windows.first; we; we = we->as.window.next) {
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
	OSLinuxEntity *we = os_entity_allocate(OSLinuxEntityKind_Window);
	OSWindow result = {(u64)we};
	DLLInsert(0, os_linux_context.windows.first, os_linux_context.windows.last, we, as.window.next, as.window.prev);

	SetConfigFlags(FLAG_VSYNC_HINT|FLAG_WINDOW_ALWAYS_RUN);

	str8 name = {.data = title, .length = title_length};
	DeferLoop(take_lock(&os_linux_context.arena_lock, -1), release_lock(&os_linux_context.arena_lock))
	{
		Arena scratch = os_linux_context.arena;
		name.length = Min(name.length, arena_capacity(&scratch, u8) - 1);
		str8 title_string = push_str8(&scratch, name);
		InitWindow(width, height, (char *)title_string.data);
	}

	we->as.window.handle = GetPlatformWindowHandle();
	os_window_equip_common(os_linux_context.input, we->as.window.handle);
	raylib_window_resize = glfwSetWindowSizeCallback(we->as.window.handle, os_window_resize_callback);

	/* NOTE: do this after initing so that the window starts out floating in tiling wm */
	SetWindowState(FLAG_WINDOW_RESIZABLE);
	SetWindowMinSize(320, 240);

	return result;
}

function OSLibrary
load_library(char *name, char *temp_name, u32 flags)
{
	if (temp_name && os_copy_file(name, temp_name))
		name = temp_name;

	OSLibrary result = {(u64)dlopen(name, flags)};
	if (result.value[0] == 0) result.value[0] = OSInvalidHandleValue;

	if (temp_name) unlink(temp_name);

	return result;
}

#if BEAMFORMER_DEBUG
function void
debug_library_reload(BeamformerInput *input)
{
	local_persist OSLibrary beamformer_library_handle = {OSInvalidHandleValue};

	if ValidHandle(beamformer_library_handle) {
		beamformer_debug_hot_release(input);
		dlclose((void *)beamformer_library_handle.value[0]);
		beamformer_library_handle = (OSLibrary){OSInvalidHandleValue};
	}

	OSLibrary new_handle = load_library(OS_DEBUG_LIB_NAME, OS_DEBUG_LIB_TEMP_NAME, RTLD_NOW|RTLD_LOCAL);
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
		os_linux_add_file_watch(str8(OS_DEBUG_LIB_NAME), (void *)BeamformerInputEventKind_ExecutableReload,
		                        OSLinuxFileWatchKind_Platform);
	#endif

	input->vulkan_library_handle = (OSLibrary){OSInvalidHandleValue};
	#define X(name) \
		if InvalidHandle(input->vulkan_library_handle) \
			input->vulkan_library_handle = load_library(name, 0, RTLD_NOW|RTLD_LOCAL);
	OS_VULKAN_SONAME_LIST
	#undef X

	if InvalidHandle(input->vulkan_library_handle)
		fatal(s8("[os] fatal error: failed to find valid vulkan library\n"));

	input->cuda_library_handle = load_library(OS_CUDA_LIB_NAME, OS_CUDA_LIB_TEMP_NAME, RTLD_NOW|RTLD_LOCAL);

	#if BEAMFORMER_RENDERDOC_HOOKS
	local_persist OSLibrary renderdoc_handle = {OSInvalidHandleValue};
	renderdoc_handle = load_library(OS_RENDERDOC_SONAME, 0, RTLD_NOW|RTLD_LOCAL|RTLD_NOLOAD);
	load_renderdoc_functions(input, renderdoc_handle);
	#endif
}

function void
dispatch_file_watch_events(BeamformerInput *input)
{
	Arena arena = os_linux_context.arena;
	u8 *mem     = arena_alloc(&arena, .size = 4096, .align = 16);
	struct inotify_event *event;

	u64 current_time = os_timer_count();

	i64 rlen;
	while ((rlen = read(os_linux_context.inotify_handle, mem, 4096)) > 0) {
		for (u8 *data = mem; data < mem + rlen; data += sizeof(*event) + event->len) {
			event = (struct inotify_event *)data;
			for (OSLinuxFileWatchDirectory *dir = os_linux_context.file_watch_directories.first; dir; dir = dir->next) {
				if (event->wd != dir->handle)
					continue;

				str8 file = str8_from_c_str(event->name);
				u64  hash = u64_hash_from_str8(file);
				for (OSLinuxFileWatch *fw = dir->first_child; fw; fw = fw->next) if (fw->hash == hash) {
					// NOTE(rnp): avoid multiple updates in a single frame
					if (fw->update_time < current_time) {
						BeamformerInputEvent input_event = {0};
						if (fw->kind == OSLinuxFileWatchKind_Platform) {
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
			}
		}
	}
}

extern i32
main(void)
{
	os_linux_context.system_info.timer_frequency         = os_timer_frequency();
	os_linux_context.system_info.logical_processor_count = os_number_of_processors();
	os_linux_context.system_info.page_size               = ARCH_X64? KB(4) : getauxval(AT_PAGESZ);
	os_linux_context.system_info.path_separator_byte     = '/';

	Arena program_memory = os_alloc_arena(MB(16) + MB(1));

	os_linux_context.arena = sub_arena(&program_memory, MB(1), KB(4));
	os_linux_context.inotify_handle = inotify_init1(IN_NONBLOCK|IN_CLOEXEC);

	BeamformerInput *input = push_struct(&program_memory, BeamformerInput);
	os_linux_context.input = input;
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

	struct pollfd fds[1] = {{0}};
	fds[0].fd     = os_linux_context.inotify_handle;
	fds[0].events = POLLIN;

	while (!WindowShouldClose() && !beamformer_should_close(input)) {
		os_build_frame_input(input);

		poll(fds, countof(fds), 0);
		if (fds[0].revents & POLLIN)
			dispatch_file_watch_events(input);

		beamformer_frame_step(input);

		// NOTE(rnp): this must happen at the end of frame to allow the pre loop events through
		// TODO(rnp): hack: until raylib is removed this happens in ui since raylib will cause
		// glfw to call the input callbacks in during EndDrawing()
		//input->event_count = 0;
	}

	beamformer_terminate(input);

	/* NOTE: make sure this will get cleaned up after external
	 * programs release their references */
	shm_unlink(OS_SHARED_MEMORY_NAME);
}
