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

#define OS_DEBUG_LIB_NAME      "./beamformer.so"
#define OS_DEBUG_LIB_TEMP_NAME "./beamformer_temp.so"

#define OS_CUDA_LIB_NAME       "./external/cuda_toolkit.so"
#define OS_CUDA_LIB_TEMP_NAME  "./external/cuda_toolkit_temp.so"

#define OS_RENDERDOC_SONAME    "librenderdoc.so"

#define OS_VULKAN_SONAME_LIST \
	X("libvulkan.so") \
	X("libvulkan.so.1") \

#include <dlfcn.h>

typedef enum {
	OSLinux_FileWatchKindPlatform,
	OSLinux_FileWatchKindUser,
} OSLinux_FileWatchKind;

typedef struct {
	OSLinux_FileWatchKind kind;
	u64                   hash;
	u64                   update_time;
	void *                user_context;
} OSLinux_FileWatch;

typedef struct {
	u64  hash;
	iptr handle;
	s8   name;

	OSLinux_FileWatch * data;
	iz                  count;
	iz                  capacity;
} OSLinux_FileWatchDirectory;
DA_STRUCT(OSLinux_FileWatchDirectory, OSLinux_FileWatchDirectory);

typedef struct {
	Arena         arena;
	i32           arena_lock;

	i32           inotify_handle;

	OSLinux_FileWatchDirectoryList file_watch_list;

	OSSystemInfo system_info;
} OSLinux_Context;
global OSLinux_Context os_linux_context;

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

function OSLinux_FileWatchDirectory *
os_lookup_file_watch_directory(OSLinux_FileWatchDirectoryList *ctx, u64 hash)
{
	OSLinux_FileWatchDirectory *result = 0;
	for (iz i = 0; !result && i < ctx->count; i++)
		if (ctx->data[i].hash == hash)
			result = ctx->data + i;
	return result;
}

function void
os_linux_add_file_watch(s8 path, void *user_context, OSLinux_FileWatchKind kind)
{
	s8 directory  = path;
	directory.len = s8_scan_backwards(path, '/');
	assert(directory.len > 0);

	OSLinux_FileWatchDirectoryList *fwctx = &os_linux_context.file_watch_list;

	u64 hash = u64_hash_from_s8(directory);
	OSLinux_FileWatchDirectory *dir = os_lookup_file_watch_directory(fwctx, hash);
	if (!dir) {
		assert(path.data[directory.len] == '/');
		dir = da_push(&os_linux_context.arena, fwctx);
		dir->hash   = hash;
		dir->name   = push_s8(&os_linux_context.arena, directory);
		u32 mask    = IN_MOVED_TO|IN_CLOSE_WRITE;
		dir->handle = inotify_add_watch(os_linux_context.inotify_handle, (c8 *)dir->name.data, mask);
	}

	OSLinux_FileWatch *fw = da_push(&os_linux_context.arena, dir);
	fw->user_context = user_context;
	fw->hash         = u64_hash_from_s8(s8_cut_head(path, dir->name.len + 1));
	fw->kind         = kind;
}

BEAMFORMER_IMPORT void
os_add_file_watch(const char *path, int64_t path_length, void *user_context)
{
	s8 path_str = {.data = (u8 *)path, .len = path_length};
	os_linux_add_file_watch(path_str, user_context, OSLinux_FileWatchKindUser);
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
	OSLibrary new_handle = load_library(OS_DEBUG_LIB_NAME, OS_DEBUG_LIB_TEMP_NAME, RTLD_NOW|RTLD_LOCAL);

	if (InvalidHandle(beamformer_library_handle) && InvalidHandle(new_handle))
		fatal(s8("[os] failed to load: " OS_DEBUG_LIB_NAME "\n"));

	if ValidHandle(new_handle) {
		beamformer_debug_hot_reload(new_handle, input);

		if ValidHandle(beamformer_library_handle)
			dlclose((void *)beamformer_library_handle.value[0]);
		beamformer_library_handle = new_handle;
	}
}
#endif /* BEAMFORMER_DEBUG */

function void
load_platform_libraries(BeamformerInput *input)
{
	#if BEAMFORMER_DEBUG
		debug_library_reload(input);
		os_linux_add_file_watch(s8(OS_DEBUG_LIB_NAME), (void *)BeamformerInputEventKind_ExecutableReload,
		                        OSLinux_FileWatchKindPlatform);
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
	OSLinux_FileWatchDirectoryList *fwctx = &os_linux_context.file_watch_list;
	Arena arena = os_linux_context.arena;
	u8 *mem     = arena_alloc(&arena, .size = 4096, .align = 16);
	struct inotify_event *event;

	u64 current_time = os_timer_count();

	iz rlen;
	while ((rlen = read(os_linux_context.inotify_handle, mem, 4096)) > 0) {
		for (u8 *data = mem; data < mem + rlen; data += sizeof(*event) + event->len) {
			event = (struct inotify_event *)data;
			for (u32 i = 0; i < fwctx->count; i++) {
				OSLinux_FileWatchDirectory *dir = fwctx->data + i;
				if (event->wd != dir->handle)
					continue;

				s8  file = c_str_to_s8(event->name);
				u64 hash = u64_hash_from_s8(file);
				for (u32 j = 0; j < dir->count; j++) {
					OSLinux_FileWatch *fw = dir->data + j;
					if (fw->hash == hash) {
						// NOTE(rnp): avoid multiple updates in a single frame
						if (fw->update_time < current_time) {
							BeamformerInputEvent input_event = {0};
							if (fw->kind == OSLinux_FileWatchKindPlatform) {
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

	Arena program_memory = os_alloc_arena(MB(16) + KB(16));

	os_linux_context.arena = sub_arena(&program_memory, KB(16), KB(4));
	os_linux_context.inotify_handle = inotify_init1(IN_NONBLOCK|IN_CLOEXEC);

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

	struct pollfd fds[1] = {{0}};
	fds[0].fd     = os_linux_context.inotify_handle;
	fds[0].events = POLLIN;

	while (!WindowShouldClose() && !beamformer_should_close(input)) {
		poll(fds, countof(fds), 0);
		if (fds[0].revents & POLLIN)
			dispatch_file_watch_events(input);

		Vector2 new_mouse = GetMousePosition();
		input->last_mouse_x = input->mouse_x;
		input->last_mouse_y = input->mouse_y;
		input->mouse_x      = new_mouse.x;
		input->mouse_y      = new_mouse.y;

		beamformer_frame_step(input);

		input->event_count  = 0;
	}

	beamformer_terminate(input);

	/* NOTE: make sure this will get cleaned up after external
	 * programs release their references */
	shm_unlink(OS_SHARED_MEMORY_NAME);
}
