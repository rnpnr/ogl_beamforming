/* See LICENSE for license details. */
/* NOTE: inspired by nob: https://github.com/tsoding/nob.h */

/* TODO(rnp):
 * [ ]: bake shaders and font data into binary
 *      - for shaders there is a way of making a separate data section and referring
 *        to it with extern from the C source (bake both data and size)
 *      - use objcopy, maybe need linker script maybe command line flags for ld will work
 * [ ]: cross compile/override baked compiler
 * [ ]: msvc build doesn't detect out of date files correctly
 * [ ]: seperate dwarf debug info
 */
#include <stdarg.h>
#include <stdio.h>

#include "util.h"

#include "beamformer_parameters.h"

global char *g_argv0;

#define OUTDIR    "out"
#define OUTPUT(s) OUTDIR "/" s

#if COMPILER_MSVC
  #define COMMON_FLAGS    "-nologo", "-std:c11", "-Fo:" OUTDIR "\\", "-Z7", "-Zo"
  #define DEBUG_FLAGS     "-Od", "-D_DEBUG"
  #define OPTIMIZED_FLAGS "-O2"
  #define EXTRA_FLAGS     ""
#else
  #define COMMON_FLAGS    "-std=c11", "-pipe", "-Wall"
  #define DEBUG_FLAGS     "-O0", "-D_DEBUG", "-Wno-unused-function"
  #define OPTIMIZED_FLAGS "-O3"
  #define EXTRA_FLAGS     "-Werror", "-Wextra", "-Wshadow", "-Wconversion", "-Wno-unused-parameter", \
                          "-Wno-error=unused-function", "-funsafe-math-optimizations", "-fno-math-errno"
#endif

#define is_aarch64 ARCH_ARM64
#define is_amd64   ARCH_X64
#define is_unix    OS_LINUX
#define is_w32     OS_WINDOWS
#define is_clang   COMPILER_CLANG
#define is_gcc     COMPILER_GCC
#define is_msvc    COMPILER_MSVC

#if OS_LINUX

  #include <errno.h>
  #include <string.h>
  #include <sys/select.h>
  #include <sys/wait.h>

  #include "os_linux.c"

  #define W32_DECL(x)

  #define OS_SHARED_LINK_LIB(s) "lib" s ".so"
  #define OS_SHARED_LIB(s)      s ".so"
  #define OS_STATIC_LIB(s)      s ".a"
  #define OS_MAIN "main_linux.c"

#elif OS_WINDOWS

  #include "os_win32.c"

  #define W32_DECL(x) x

  #define OS_SHARED_LINK_LIB(s) s ".dll"
  #define OS_SHARED_LIB(s)      s ".dll"
  #define OS_STATIC_LIB(s)      s ".lib"
  #define OS_MAIN "main_w32.c"

#else
  #error Unsupported Platform
#endif

#if COMPILER_CLANG
  #define COMPILER "clang"
#elif COMPILER_MSVC
  #define COMPILER "cl"
#else
  #define COMPILER "cc"
#endif

#if COMPILER_MSVC
  #define LINK_LIB(name)             name ".lib"
  #define OBJECT(name)               name ".obj"
  #define OUTPUT_DLL(name)           "/LD", "/Fe:", name
  #define OUTPUT_LIB(name)           "/out:" OUTPUT(name)
  #define OUTPUT_EXE(name)           "/Fe:", name
  #define STATIC_LIBRARY_BEGIN(name) "lib", "/nologo", name
#else
  #define LINK_LIB(name)             "-l" name
  #define OBJECT(name)               name ".o"
  #define OUTPUT_DLL(name)           "-fPIC", "-shared", "-o", name
  #define OUTPUT_LIB(name)           OUTPUT(name)
  #define OUTPUT_EXE(name)           "-o", name
  #define STATIC_LIBRARY_BEGIN(name) "ar", "rc", name
#endif

#define shift(list, count) ((count)--, *(list)++)

#define da_append_count(a, s, items, item_count) do { \
	da_reserve((a), (s), (item_count));                                             \
	mem_copy((s)->data + (s)->count, (items), sizeof(*(items)) * (uz)(item_count)); \
	(s)->count += (item_count);                                                     \
} while (0)

#define cmd_append_count da_append_count
#define cmd_append(a, s, ...) da_append_count(a, s, ((char *[]){__VA_ARGS__}), \
                                              (iz)(sizeof((char *[]){__VA_ARGS__}) / sizeof(char *)))

typedef struct {
	char **data;
	iz     count;
	iz     capacity;
} CommandList;

typedef struct {
	b32   debug;
	b32   generic;
	b32   sanitize;
	b32   tests;
	b32   time;
} Options;

#define BUILD_LOG_KINDS \
	X(Error,   "\x1B[31m[ERROR]\x1B[0m   ") \
	X(Warning, "\x1B[33m[WARNING]\x1B[0m ") \
	X(Info,    "\x1B[32m[INFO]\x1B[0m    ") \
	X(Command, "\x1B[36m[COMMAND]\x1B[0m ")
#define X(t, ...) BuildLogKind_##t,
typedef enum {BUILD_LOG_KINDS BuildLogKind_Count} BuildLogKind;
#undef X

function void
build_log_base(BuildLogKind kind, char *format, va_list args)
{
	#define X(t, pre) pre,
	read_only local_persist char *prefixes[BuildLogKind_Count + 1] = {BUILD_LOG_KINDS "[INVALID] "};
	#undef X
	FILE *out = kind == BuildLogKind_Error? stderr : stdout;
	fputs(prefixes[MIN(kind, BuildLogKind_Count)], out);
	vfprintf(out, format, args);
	fputc('\n', out);
}

#define build_log_failure(format, ...) build_log(BuildLogKind_Error, \
                                                 "failed to build: " format, ##__VA_ARGS__)
#define build_log_info(...)    build_log(BuildLogKind_Info,    ##__VA_ARGS__)
#define build_log_command(...) build_log(BuildLogKind_Command, ##__VA_ARGS__)
#define build_log_warning(...) build_log(BuildLogKind_Warning, ##__VA_ARGS__)
function void
build_log(BuildLogKind kind, char *format, ...)
{
	va_list ap;
	va_start(ap, format);
	build_log_base(kind, format, ap);
	va_end(ap);
}

#define build_fatal(fmt, ...) build_fatal_("%s: " fmt, __FUNCTION__, ##__VA_ARGS__)
function no_return void
build_fatal_(char *format, ...)
{
	va_list ap;
	va_start(ap, format);
	build_log_base(BuildLogKind_Error, format, ap);
	va_end(ap);
	os_exit(1);
}

function b32
s8_contains(s8 s, u8 byte)
{
	b32 result = 0;
	for (iz i = 0 ; !result && i < s.len; i++)
		result |= s.data[i] == byte;
	return result;
}

function void
stream_push_command(Stream *s, CommandList *c)
{
	if (!s->errors) {
		for (iz i = 0; i < c->count; i++) {
			s8 item    = c_str_to_s8(c->data[i]);
			if (item.len) {
				b32 escape = s8_contains(item, ' ') || s8_contains(item, '"');
				if (escape) stream_append_byte(s, '\'');
				stream_append_s8(s, item);
				if (escape) stream_append_byte(s, '\'');
				if (i != c->count - 1) stream_append_byte(s, ' ');
			}
		}
	}
}

#if OS_LINUX

function b32
os_rename_file(char *name, char *new)
{
	b32 result = rename(name, new) != -1;
	return result;
}

function b32
os_remove_file(char *name)
{
	b32 result = remove(name) != -1;
	return result;
}

function void
os_make_directory(char *name)
{
	mkdir(name, 0770);
}

function u64
os_get_filetime(char *file)
{
	struct stat sb;
	u64 result = (u64)-1;
	if (stat(file, &sb) != -1)
		result = (u64)sb.st_mtim.tv_sec;
	return result;
}

function iptr
os_spawn_process(CommandList *cmd, Stream sb)
{
	pid_t result = fork();
	switch (result) {
	case -1: build_fatal("failed to fork command: %s: %s", cmd->data[0], strerror(errno)); break;
	case  0: {
		if (execvp(cmd->data[0], cmd->data) == -1)
			build_fatal("failed to exec command: %s: %s", cmd->data[0], strerror(errno));
		unreachable();
	} break;
	}
	return (iptr)result;
}

function b32
os_wait_close_process(iptr handle)
{
	b32 result = 0;
	for (;;) {
		i32   status;
		iptr wait_pid = (iptr)waitpid((i32)handle, &status, 0);
		if (wait_pid == -1)
			build_fatal("failed to wait on child process: %s", strerror(errno));
		if (wait_pid == handle) {
			if (WIFEXITED(status)) {
				status = WEXITSTATUS(status);
				/* TODO(rnp): logging */
				result = status == 0;
				break;
			}
			if (WIFSIGNALED(status)) {
				/* TODO(rnp): logging */
				result = 0;
				break;
			}
		} else {
			/* TODO(rnp): handle multiple children */
			INVALID_CODE_PATH;
		}
	}
	return result;
}

#elif OS_WINDOWS

enum {
	MOVEFILE_REPLACE_EXISTING = 0x01,
};

W32(b32) CreateDirectoryA(c8 *, void *);
W32(b32) CreateProcessA(u8 *, u8 *, iptr, iptr, b32, u32, iptr, u8 *, iptr, iptr);
W32(b32) GetExitCodeProcess(iptr handle, u32 *);
W32(b32) GetFileTime(iptr, iptr, iptr, iptr);
W32(b32) MoveFileExA(c8 *, c8 *, u32);

function void
os_make_directory(char *name)
{
	CreateDirectoryA(name, 0);
}

function b32
os_rename_file(char *name, char *new)
{
	b32 result = MoveFileExA(name, new, MOVEFILE_REPLACE_EXISTING) != 0;
	return result;
}

function b32
os_remove_file(char *name)
{
	b32 result = DeleteFileA(name);
	return result;
}

function u64
os_get_filetime(char *file)
{
	u64 result = (u64)-1;
	iptr h = CreateFileA(file, 0, 0, 0, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
	if (h != INVALID_FILE) {
		struct { u32 low, high; } w32_filetime;
		GetFileTime(h, 0, 0, (iptr)&w32_filetime);
		result = (u64)w32_filetime.high << 32ULL | w32_filetime.low;
		CloseHandle(h);
	}
	return result;
}

function iptr
os_spawn_process(CommandList *cmd, Stream sb)
{
	struct {
		u32 cb;
		u8 *reserved, *desktop, *title;
		u32 x, y, x_size, y_size, x_count_chars, y_count_chars;
		u32 fill_attr, flags;
		u16 show_window, reserved_2;
		u8 *reserved_3;
		iptr std_input, std_output, std_error;
	} w32_startup_info = {
		.cb = sizeof(w32_startup_info),
		.flags = 0x100,
		.std_input  = GetStdHandle(STD_INPUT_HANDLE),
		.std_output = GetStdHandle(STD_OUTPUT_HANDLE),
		.std_error  = GetStdHandle(STD_ERROR_HANDLE),
	};

	struct {
		iptr phandle, thandle;
		u32  pid, tid;
	} w32_process_info = {0};

	/* TODO(rnp): warn if we need to clamp last string */
	sb.widx = MIN(sb.widx, (i32)(KB(32) - 1));
	if (sb.widx < sb.cap) sb.data[sb.widx]     = 0;
	else                  sb.data[sb.widx - 1] = 0;

	iptr result = INVALID_FILE;
	if (CreateProcessA(0, sb.data, 0, 0, 1, 0, 0, 0, (iptr)&w32_startup_info,
	                   (iptr)&w32_process_info))
	{
		CloseHandle(w32_process_info.thandle);
		result = w32_process_info.phandle;
	}
	return result;
}

function b32
os_wait_close_process(iptr handle)
{
	b32 result = WaitForSingleObject(handle, (u32)-1) != 0xFFFFFFFFUL;
	if (result) {
		u32 status;
		GetExitCodeProcess(handle, &status);
		result = status == 0;
	}
	CloseHandle(handle);
	return result;
}

#endif

#define needs_rebuild(b, ...) needs_rebuild_(b, ((char *[]){__VA_ARGS__}), \
                                             (sizeof((char *[]){__VA_ARGS__}) / sizeof(char *)))
function b32
needs_rebuild_(char *binary, char *deps[], iz deps_count)
{
	u64 binary_filetime = os_get_filetime(binary);
	u64 argv0_filetime  = os_get_filetime(g_argv0);
	b32 result = (binary_filetime == (u64)-1) | (argv0_filetime > binary_filetime);
	for (iz i = 0; i < deps_count; i++) {
		u64 filetime = os_get_filetime(deps[i]);
		result |= (filetime == (u64)-1) | (filetime > binary_filetime);
	}
	return result;
}

function b32
run_synchronous(Arena a, CommandList *command)
{
	Stream sb = arena_stream(a);
	stream_push_command(&sb, command);
	build_log_command("%.*s", (i32)sb.widx, sb.data);
	return os_wait_close_process(os_spawn_process(command, sb));
}

function CommandList
cmd_base(Arena *a, Options *o)
{
	CommandList result = {0};
	cmd_append(a, &result, COMPILER);

	if (!is_msvc) {
		/* TODO(rnp): support cross compiling with clang */
		if (!o->generic)     cmd_append(a, &result, "-march=native");
		else if (is_amd64)   cmd_append(a, &result, "-march=x86-64-v3");
		else if (is_aarch64) cmd_append(a, &result, "-march=armv8");
	}

	cmd_append(a, &result, COMMON_FLAGS, "-Iexternal/include");
	if (o->debug) cmd_append(a, &result, DEBUG_FLAGS);
	else          cmd_append(a, &result, OPTIMIZED_FLAGS);

	/* NOTE: ancient gcc bug: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=80454 */
	if (is_gcc) cmd_append(a, &result, "-Wno-missing-braces");

	if (is_w32 && is_clang) cmd_append(a, &result, "-fms-extensions");

	if (o->debug && is_unix) cmd_append(a, &result, "-ggdb");

	/* NOTE(rnp): need to avoid w32-gcc for ci */
	b32 sanitize = !is_msvc && (o->debug || o->sanitize) && !(is_w32 && is_gcc);
	if (sanitize) {
		cmd_append(a, &result, "-fsanitize=address,undefined");
		/* NOTE(rnp): impossible to autodetect on GCC versions < 14 (ci has 13) */
		cmd_append(a, &result, "-DASAN_ACTIVE=1");
	} else {
		cmd_append(a, &result, "-DASAN_ACTIVE=0");
	}
	if (!sanitize && o->sanitize) build_log_warning("santizers not supported with this compiler");

	return result;
}

function void
check_rebuild_self(Arena arena, i32 argc, char *argv[])
{
	char *binary = shift(argv, argc);
	if (needs_rebuild(binary, __FILE__, "os_win32.c", "os_linux.c", "util.c", "util.h", "beamformer_parameters.h")) {
		Stream name_buffer = arena_stream(arena);
		stream_append_s8s(&name_buffer, c_str_to_s8(binary), s8(".old"));
		char *old_name = (char *)arena_stream_commit_zero(&arena, &name_buffer).data;

		if (!os_rename_file(binary, old_name))
			build_fatal("failed to move: %s -> %s", binary, old_name);

		Options options = {0};
		CommandList c = cmd_base(&arena, &options);
		cmd_append(&arena, &c, EXTRA_FLAGS);
		if (!is_msvc) cmd_append(&arena, &c, "-Wno-unused-function");
		cmd_append(&arena, &c, __FILE__, OUTPUT_EXE(binary));
		if (is_msvc) cmd_append(&arena, &c, "/link", "-incremental:no", "-opt:ref");
		cmd_append(&arena, &c, (void *)0);
		if (!run_synchronous(arena, &c)) {
			os_rename_file(old_name, binary);
			build_fatal("failed to rebuild self");
		}
		os_remove_file(old_name);

		c.count = 0;
		cmd_append(&arena, &c, binary);
		cmd_append_count(&arena, &c, argv, argc);
		cmd_append(&arena, &c, (void *)0);
		if (!run_synchronous(arena, &c))
			os_exit(1);

		os_exit(0);
	}
}

function b32
s8_equal(s8 a, s8 b)
{
	b32 result = a.len == b.len;
	for (iz i = 0; result && i < a.len; i++)
		result = a.data[i] == b.data[i];
	return result;
}

function void
usage(char *argv0)
{
	printf("%s [--debug] [--sanitize] [--time]\n"
	       "    --debug:       dynamically link and build with debug symbols\n"
	       "    --generic:     compile for a generic target (x86-64-v3 or armv8 with NEON)\n"
	       "    --sanitize:    build with ASAN and UBSAN\n"
	       "    --tests:       also build programs in tests/\n"
	       "    --time:        print build time\n"
	       , argv0);
	os_exit(0);
}

function Options
parse_options(i32 argc, char *argv[])
{
	Options result = {0};

	char *argv0 = shift(argv, argc);
	while (argc > 0) {
		char *arg = shift(argv, argc);
		s8 str    = c_str_to_s8(arg);
		if (s8_equal(str, s8("--debug"))) {
			result.debug = 1;
		} else if (s8_equal(str, s8("--generic"))) {
			result.generic = 1;
		} else if (s8_equal(str, s8("--sanitize"))) {
			result.sanitize = 1;
		} else if (s8_equal(str, s8("--tests"))) {
			result.tests = 1;
		} else if (s8_equal(str, s8("--time"))) {
			result.time = 1;
		} else {
			usage(argv0);
		}
	}

	return result;
}

/* NOTE(rnp): produce pdbs on w32 */
function void
cmd_pdb(Arena *a, CommandList *cmd, char *name)
{
	if (is_w32 && is_clang) {
		cmd_append(a, cmd, "-fuse-ld=lld", "-g", "-gcodeview", "-Wl,--pdb=");
	} else if (is_msvc) {
		Stream sb = arena_stream(*a);
		stream_append_s8s(&sb, s8("-PDB:"), c_str_to_s8(name), s8(".pdb"));
		char *pdb = (char *)arena_stream_commit_zero(a, &sb).data;
		cmd_append(a, cmd, "/link", "-incremental:no", "-opt:ref", "-DEBUG", pdb);
	}
}

function void
git_submodule_update(Arena a, char *name)
{
	Stream sb = arena_stream(a);
	stream_append_s8s(&sb, c_str_to_s8(name), s8(OS_PATH_SEPARATOR), s8(".git"));
	arena_stream_commit_zero(&a, &sb);

	CommandList git = {0};
	/* NOTE(rnp): cryptic bs needed to get a simple exit code if name is dirty */
	cmd_append(&a, &git, "git", "diff-index", "--quiet", "HEAD", "--", name, (void *)0);
	if (!os_file_exists((c8 *)sb.data) || !run_synchronous(a, &git)) {
		git.count = 1;
		cmd_append(&a, &git, "submodule", "update", "--init", "--depth=1", name, (void *)0);
		if (!run_synchronous(a, &git))
			build_fatal("failed to clone required module: %s", name);
	}
}

function b32
build_shared_library(Arena a, CommandList cc, char *name, char *output, char **libs, iz libs_count, char **srcs, iz srcs_count)
{
	cmd_append_count(&a, &cc, srcs, srcs_count);
	cmd_append(&a, &cc, OUTPUT_DLL(output));
	cmd_pdb(&a, &cc, name);
	cmd_append_count(&a, &cc, libs, libs_count);
	cmd_append(&a, &cc, (void *)0);
	b32 result = run_synchronous(a, &cc);
	if (!result) build_log_failure("%s", output);
	return result;
}

function b32
cc_single_file(Arena a, CommandList cc, b32 exe, char *src, char *dest, char **tail, iz tail_count)
{
	char *executable[] = {src, is_msvc? "/Fe:" : "-o", dest};
	char *object[]     = {is_msvc? "/c" : "-c", src, is_msvc? "/Fo:" : "-o", dest};
	cmd_append_count(&a, &cc, exe? executable : object,
	                 exe? countof(executable) : countof(object));
	cmd_append_count(&a, &cc, tail, tail_count);
	cmd_append(&a, &cc, (void *)0);
	b32 result = run_synchronous(a, &cc);
	if (!result) build_log_failure("%s", dest);
	return result;
}

function b32
build_static_library_from_objects(Arena a, char *name, char **flags, iz flags_count, char **objects, iz count)
{
	CommandList ar = {0};
	cmd_append(&a, &ar, STATIC_LIBRARY_BEGIN(name));
	cmd_append_count(&a, &ar, flags, flags_count);
	cmd_append_count(&a, &ar, objects, count);
	cmd_append(&a, &ar, (void *)0);
	b32 result = run_synchronous(a, &ar);
	if (!result) build_log_failure("%s", name);
	return result;
}

function b32
build_static_library(Arena a, CommandList cc, char *name, char **deps, char **outputs, iz count)
{
	/* TODO(rnp): refactor to not need outputs */
	b32 result = 1;
	for (iz i = 0; i < count; i++)
		result &= cc_single_file(a, cc, 0, deps[i], outputs[i], 0, 0);
	if (result) result = build_static_library_from_objects(a, name, 0, 0, outputs, count);
	return result;
}

function b32
check_build_raylib(Arena a, CommandList cc, b32 shared)
{
	b32 result = 1;
	char *libraylib = shared ? OS_SHARED_LINK_LIB("raylib") : OUTPUT_LIB(OS_STATIC_LIB("raylib"));
	if (needs_rebuild(libraylib, __FILE__, "external/include/rlgl.h", "external/raylib")) {
		git_submodule_update(a, "external/raylib");
		os_copy_file("external/raylib/src/rlgl.h", "external/include/rlgl.h");

		if (is_unix) cmd_append(&a, &cc, "-D_GLFW_X11");
		cmd_append(&a, &cc, "-DPLATFORM_DESKTOP_GLFW");
		if (!is_msvc) cmd_append(&a, &cc, "-Wno-unused-but-set-variable");
		cmd_append(&a, &cc, "-Iexternal/raylib/src", "-Iexternal/raylib/src/external/glfw/include");
		#define RAYLIB_SOURCES \
			X(rglfw)     \
			X(rshapes)   \
			X(rtext)     \
			X(rtextures) \
			X(utils)
		#define X(name) "external/raylib/src/" #name ".c",
		char *srcs[] = {"external/rcore_extended.c", RAYLIB_SOURCES};
		#undef X
		#define X(name) OUTPUT(OBJECT(#name)),
		char *outs[] = {OUTPUT(OBJECT("rcore_extended")), RAYLIB_SOURCES};
		#undef X

		if (shared) {
			char *libs[] = {LINK_LIB("user32"), LINK_LIB("shell32"), LINK_LIB("gdi32"), LINK_LIB("winmm")};
			iz libs_count = is_w32 ? countof(libs) : 0;
			cmd_append(&a, &cc, "-DBUILD_LIBTYPE_SHARED", "-D_GLFW_BUILD_DLL");
			result = build_shared_library(a, cc, "raylib", libraylib, libs, libs_count, srcs, countof(srcs));
		} else {
			result = build_static_library(a, cc, libraylib, srcs, outs, countof(srcs));
		}
	}
	return result;
}

function b32
build_helper_library(Arena arena, CommandList cc)
{
	/////////////
	// library
	char *library = OUTPUT(OS_SHARED_LIB("ogl_beamformer_lib"));
	char *libs[]  = {LINK_LIB("Synchronization")};
	iz libs_count = is_w32 ? countof(libs) : 0;

	if (!is_msvc) cmd_append(&arena, &cc, "-Wno-unused-function");
	b32 result = build_shared_library(arena, cc, "ogl_beamformer_lib", library,
	                                  libs, libs_count,
	                                  arg_list(char *, "helpers/ogl_beamformer_lib.c"));

	/////////////
	// header
	char *lib_header_out = OUTPUT("ogl_beamformer_lib.h");
	if (needs_rebuild(lib_header_out, "helpers/ogl_beamformer_lib_base.h")) {
		s8 parameters_header = os_read_whole_file(&arena, "beamformer_parameters.h");
		s8 base_header       = os_read_whole_file(&arena, "helpers/ogl_beamformer_lib_base.h");
		result = parameters_header.len != 0 && base_header.len != 0 &&
		         parameters_header.data + parameters_header.len == base_header.data;
		if (result) {
			s8 output_file   = parameters_header;
			output_file.len += base_header.len;
			result &= os_write_new_file(lib_header_out, output_file);
		}
		if (!result) build_log_failure("%s", lib_header_out);
	}

	return result;
}

function b32
build_beamformer_as_library(Arena arena, CommandList cc)
{
	char *library = OS_SHARED_LIB("beamformer");
	char *libs[]  = {!is_msvc? "-L." : "", LINK_LIB("raylib"), LINK_LIB("gdi32"),
	                 LINK_LIB("shell32"), LINK_LIB("user32"), LINK_LIB("opengl32"),
	                 LINK_LIB("winmm"), LINK_LIB("Synchronization"), OUTPUT("main.lib")};
	iz libs_count = is_w32 ? countof(libs) : 0;
	cmd_append(&arena, &cc, "-D_BEAMFORMER_DLL");
	b32 result = build_shared_library(arena, cc, "beamformer", library,
	                                  libs, libs_count, arg_list(char *, "beamformer.c"));
	return result;
}

function b32
build_tests(Arena arena, CommandList cc)
{
	#define TEST_PROGRAMS \
		X("decode", W32_DECL(LINK_LIB("Synchronization"))) \
		X("throughput", LINK_LIB("zstd"), W32_DECL(LINK_LIB("Synchronization")))

	os_make_directory(OUTPUT("tests"));
	if (!is_msvc) cmd_append(&arena, &cc, "-Wno-unused-function");
	cmd_append(&arena, &cc, "-I.", "-Ihelpers");

	b32 result = 1;
	iz cc_count = cc.count;
	#define X(prog, ...) \
		cmd_pdb(&arena, &cc, prog); \
		result &= cc_single_file(arena, cc, 1, "tests/" prog ".c", \
		                         OUTPUT("tests/" prog),            \
		                         arg_list(char *, ##__VA_ARGS__)); \
		cc.count = cc_count;
	TEST_PROGRAMS
	#undef X
	return result;
}

typedef struct {
	s8 *data;
	iz  count;
	iz  capacity;
} s8_list;

function s8
s8_chop(s8 *in, iz count)
{
	count = CLAMP(count, 0, in->len);
	s8 result = {.data = in->data, .len = count};
	in->data += count;
	in->len  -= count;
	return result;
}

function void
s8_split(s8 str, s8 *left, s8 *right, u8 byte)
{
	iz i;
	for (i = 0; i < str.len; i++) if (str.data[i] == byte) break;

	if (left) *left = (s8){.data = str.data, .len = i};
	if (right) {
		right->data = str.data + i + 1;
		right->len  = MAX(0, str.len - (i + 1));
	}
}

function s8
s8_trim(s8 in)
{
	s8 result = in;
	for (iz i = 0; i < in.len && *result.data == ' '; i++) result.data++;
	result.len -= result.data - in.data;
	for (; result.len > 0 && result.data[result.len - 1] == ' '; result.len--);
	return result;
}

function void
s8_list_from_s8(s8_list *list, Arena *arena, s8 str)
{
	s8 right = str, left;
	while (right.len > 0) {
		s8_split(right, &left, &right, ' ');
		left = s8_trim(left);
		if (left.len > 0) { *da_push(arena, list) = left; }
	}
}

typedef struct {
	Stream stream;
	i32    indentation_level;
} MetaprogramContext;

function b32
meta_write_and_reset(MetaprogramContext *m, char *file)
{
	b32 result = os_write_new_file(file, stream_to_s8(&m->stream));
	if (!result) build_log_failure("%s", file);
	m->stream.widx       = 0;
	m->indentation_level = 0;
	return result;
}

function void
meta_indent(MetaprogramContext *m)
{
	for (i32 count = m->indentation_level; count > 0; count--)
		stream_append_byte(&m->stream, '\t');
}

#define meta_push(m, ...) meta_push_(m, arg_list(s8, __VA_ARGS__))
function void
meta_push_(MetaprogramContext *m, s8 *items, iz count)
{
	stream_append_s8s_(&m->stream, items, count);
}

#define meta_begin_line(m, ...)  do { meta_indent(m); meta_push(m, __VA_ARGS__);           } while(0)
#define meta_end_line(m, ...)    do {                 meta_push(m, __VA_ARGS__, s8("\n")); } while(0)
#define meta_push_line(m, ...)   do { meta_indent(m); meta_push(m, __VA_ARGS__, s8("\n")); } while(0)
#define meta_begin_scope(m, ...) do { meta_push_line(m, __VA_ARGS__); (m)->indentation_level++; } while(0)
#define meta_end_scope(m, ...)   do { (m)->indentation_level--; meta_push_line(m, __VA_ARGS__); } while(0)

#define meta_begin_matlab_class_cracker(_1, _2, FN, ...) FN
#define meta_begin_matlab_class_1(m, name) meta_begin_scope(m, s8("classdef " name))
#define meta_begin_matlab_class_2(m, name, type) \
  meta_begin_scope(m, s8("classdef " name " < " type))

#define meta_begin_matlab_class(m, ...) \
  meta_begin_matlab_class_cracker(__VA_ARGS__, \
                                  meta_begin_matlab_class_2, \
                                  meta_begin_matlab_class_1)(m, __VA_ARGS__)

function void
meta_push_matlab_property(MetaprogramContext *m, s8 name, i64 length)
{
	meta_indent(m);
	stream_append_s8s(&m->stream, name, s8("(1,"));
	stream_append_i64(&m->stream, length);
	stream_append_s8(&m->stream, s8(")\n"));
}

function void
meta_push_matlab_enum_with_value(MetaprogramContext *m, s8 name, i32 value)
{
	meta_indent(m);
	stream_append_s8s(&m->stream, name, s8(" ("));
	stream_append_i64(&m->stream, value);
	stream_append_s8(&m->stream, s8(")\n"));
}

function b32
meta_end_and_write_matlab(MetaprogramContext *m, char *path)
{
	while (m->indentation_level > 0) meta_end_scope(m, s8("end"));
	b32 result = meta_write_and_reset(m, path);
	return result;
}

#define META_ENTRY_KIND_LIST \
	X(BeginScope)  \
	X(EndScope)    \
	X(Permute)     \
	X(PermuteBits) \
	X(Shader)      \
	X(ShaderGroup)

#define X(k, ...) MetaEntryKind_## k,
typedef enum {META_ENTRY_KIND_LIST} MetaEntryKind;
#undef X

typedef struct {
	s8  *permutations;
	u64  max_permutation;
} MetaEntryPermute;

typedef struct {
	MetaEntryKind kind;
	s8            name;
	u32 line, column;
	union {
		MetaEntryPermute permute;
	};
} MetaEntry;

typedef struct {
	MetaEntry *data;
	iz         count;
	iz         capacity;
	s8         raw;
} MetaEntryStack;

#define META_PARSE_TOKEN_LIST \
	X('@', Entry)      \
	X('(', BeginArgs)  \
	X(')', EndArgs)    \
	X('[', BeginArray) \
	X(']', EndArray)   \
	X('{', BeginScope) \
	X('}', EndScope)   \
	X(',', Comma)

typedef enum {
	MetaParseToken_EOF,
	MetaParseToken_String,
	#define X(__1, kind, ...) MetaParseToken_## kind,
	META_PARSE_TOKEN_LIST
	#undef X
	MetaParseToken_Count,
} MetaParseToken;

typedef union {
	MetaEntryKind kind;
	s8            string;
} MetaParseUnion;

typedef struct {
	s8  s;
	u32 line;
	u32 column;
} MetaParsePoint;

typedef struct {
	MetaParsePoint p;
	MetaParseUnion u;
	MetaParsePoint save_point;
} MetaParser;

#define meta_parser_save(v)    (v)->save_point = (v)->p
#define meta_parser_restore(v) (v)->p = (v)->save_point

function void
meta_entry_print(MetaEntry *e, i32 depth)
{
	#define X(k, ...) #k,
	read_only local_persist char *kinds[] = {META_ENTRY_KIND_LIST};
	#undef X

	char *kind = kinds[e->kind];
	if (e->kind == MetaEntryKind_BeginScope) kind = "{";
	if (e->kind == MetaEntryKind_EndScope)   kind = "}";

	fprintf(stderr, "%*s%s", depth * 2, "", kind);
	if (e->name.len) fprintf(stderr, "(%.*s)", (i32)e->name.len, e->name.data);
	switch (e->kind) {
	case MetaEntryKind_PermuteBits:
	case MetaEntryKind_Permute:
	{
		u64 count;
		if (e->kind == MetaEntryKind_PermuteBits) count = (u64)popcnt_u64(e->permute.max_permutation);
		else                                      count = e->permute.max_permutation;
		fprintf(stderr, " [");
		for (u64 i = 0; i < count; i++) {
			if (i != 0) fprintf(stderr, ", ");
			fprintf(stderr, "%.*s", (i32)e->permute.permutations[i].len, e->permute.permutations[i].data);
		}
		fprintf(stderr, "]");
	}break;
	default:{}break;
	}
	fprintf(stderr, "\n");
}

function MetaEntryKind
meta_entry_kind_from_string(s8 s)
{
	#define X(k, ...) s8(#k),
	read_only local_persist s8 kinds[] = {META_ENTRY_KIND_LIST};
	#undef X
	i32 result = -1;
	for EachElement(kinds, it) {
		if (s8_equal(kinds[it], s)) {
			result = (i32)it;
			break;
		}
	}
	return (MetaEntryKind)result;
}

function void
meta_parser_trim(MetaParser *p)
{
	u8 *s, *end = p->p.s.data + p->p.s.len;
	b32 done    = 0;
	b32 comment = 0;
	for (s = p->p.s.data; !done && s != end;) {
		switch (*s) {
		case '\t':
		case ' ':
		{
			p->p.column++;
		}break;
		case '\n':{ p->p.line++; p->p.column = 0; comment = 0; }break;
		case '/':{
			comment = ((s + 1) != end  && s[1] == '/');
		} /* FALLTHROUGH */
		default:{done = !comment;}break;
		}
		if (!done) s++;
	}
	p->p.s.data = s;
	p->p.s.len  = end - s;
}

#define meta_parser_error(v, format, ...) \
	meta_generator_error_("%s:%u:%u: error: "format, compiler_file, (v)->p.line + 1, (v)->p.column + 1, ##__VA_ARGS__)
#define meta_compiler_error(e, format, ...) \
	meta_generator_error_("%s:%u:%u: error: "format, compiler_file, (e)->line + 1, (e)->column + 1, ##__VA_ARGS__)

global char *compiler_file;
global void *compiler_jmp_buf[5];
function no_return void
meta_generator_error_(char *format, ...)
{
	va_list ap;
	va_start(ap, format);
	vfprintf(stderr, format, ap);
	va_end(ap);
	longjmp(compiler_jmp_buf, 1);
}

function s8
meta_parser_extract_string(MetaParser *p)
{
	s8 result = {.data = p->p.s.data};
	for (; result.len < p->p.s.len; result.len++) {
		b32 done = 0;
		switch (p->p.s.data[result.len]) {
		#define X(t, ...) case t:
		META_PARSE_TOKEN_LIST
		#undef X
		case ' ': case '\n': case '\t':
		{done = 1;}break;
		case '/':{
			done = (result.len + 1 < p->p.s.len) && (p->p.s.data[result.len + 1] == '/');
		}break;
		default:{}break;
		}
		if (done) break;
	}
	p->p.column += result.len;
	p->p.s.data += result.len;
	p->p.s.len  -= result.len;
	return result;
}

function s8
meta_parser_token_name(MetaParser *p, MetaParseToken t)
{
	s8 result = s8("\"invalid\"");
	read_only local_persist s8 names[MetaParseToken_Count] = {
		[MetaParseToken_EOF] = s8("\"EOF\""),
		#define X(k, v, ...) [MetaParseToken_## v] = s8(#k),
		META_PARSE_TOKEN_LIST
		#undef X
	};
	if (t >= 0 && t < countof(names)) result = names[t];
	if (t == MetaParseToken_String)   result = p->u.string;
	return result;
}

function MetaParseToken
meta_parser_token(MetaParser *p)
{
	MetaParseToken result = MetaParseToken_EOF;
	meta_parser_save(p);
	if (p->p.s.len > 0) {
		b32 chop = 1;
		switch (p->p.s.data[0]) {
		#define X(t, kind, ...) case t:{ result = MetaParseToken_## kind; }break;
		META_PARSE_TOKEN_LIST
		#undef X
		default:{ result = MetaParseToken_String; chop = 0; }break;
		}
		if (chop) { s8_chop(&p->p.s, 1); p->p.column++; }

		meta_parser_trim(p);
		switch (result) {
		case MetaParseToken_String:{ p->u.string = meta_parser_extract_string(p); }break;

		/* NOTE(rnp): '{' and '}' are shorthand for @BeginScope and @EndScope */
		case MetaParseToken_BeginScope:{ p->u.kind = MetaEntryKind_BeginScope; }break;
		case MetaParseToken_EndScope:{   p->u.kind = MetaEntryKind_EndScope;   }break;

		case MetaParseToken_Entry:{
			s8 kind = meta_parser_extract_string(p);
			p->u.kind = meta_entry_kind_from_string(kind);
			if (p->u.kind < 0) {
				meta_parser_error(p, "invalid meta kind: %.*s\n", (i32)kind.len, kind.data);
			}
		}break;
		default:{}break;
		}
		meta_parser_trim(p);
	}

	return result;
}

function MetaParseToken
meta_parser_peek_token(MetaParser *p)
{
	MetaParseToken result = meta_parser_token(p);
	meta_parser_restore(p);
	return result;
}

function MetaParseUnion
meta_parser_expect_token(MetaParser *p, MetaParseToken expect)
{
	MetaParseToken result = meta_parser_token(p);
	if (result != expect) {
		meta_parser_restore(p);
		s8 result_name = meta_parser_token_name(p, result);
		s8 expect_name = meta_parser_token_name(p, expect);
		meta_parser_error(p, "expected %.*s but got: %.*s\n",
		                  (i32)expect_name.len, expect_name.data,
		                  (i32)result_name.len, result_name.data);
	}
	return p->u;
}

function void
meta_parser_unexpected_token(MetaParser *p, MetaParseToken t)
{
	meta_parser_restore(p);
	s8 token_name = meta_parser_token_name(p, t);
	meta_parser_error(p, "unexpected token: %.*s\n", (i32)token_name.len, token_name.data);
}

function MetaEntryStack
meta_entry_stack_from_file(Arena *arena, Arena scratch, char *file)
{
	MetaParser     parser = {.p.s = os_read_whole_file(arena, file)};
	MetaEntryStack result = {.raw = parser.p.s};

	compiler_file = file;

	meta_parser_trim(&parser);

	for (MetaParseToken token = meta_parser_token(&parser);
	     token != MetaParseToken_EOF;
	     token = meta_parser_token(&parser))
	{
		MetaEntry *e = da_push(arena, &result);
		switch (token) {
		case MetaParseToken_BeginScope:
		case MetaParseToken_EndScope:
		case MetaParseToken_Entry:
		{
			e->kind   = parser.u.kind;
			e->line   = parser.save_point.line;
			e->column = parser.save_point.column;
			if (meta_parser_peek_token(&parser) == MetaParseToken_BeginArgs) {
				/* NOTE(rnp): for now entries may only have a single name as an arg */
				meta_parser_token(&parser);
				e->name = meta_parser_expect_token(&parser, MetaParseToken_String).string;
				meta_parser_expect_token(&parser, MetaParseToken_EndArgs);
			}
			switch (e->kind) {
			case MetaEntryKind_PermuteBits:
			case MetaEntryKind_Permute:
			{
				Arena a = scratch;
				s8_list permutations = {0};
				meta_parser_expect_token(&parser, MetaParseToken_BeginArray);
				b32 bits = e->kind == MetaEntryKind_PermuteBits;
				b32 done = 0;
				for (;!done;) {
					MetaParseToken t = meta_parser_token(&parser);
					switch (t) {
					case MetaParseToken_Comma:{}break;
					case MetaParseToken_EndArray:{ done = 1; }break;
					case MetaParseToken_String:{
						*da_push(&a, &permutations) = parser.u.string;
						if (bits) e->permute.max_permutation = (e->permute.max_permutation << 1) | 1;
						else      e->permute.max_permutation++;
					}break;
					default:{ meta_parser_unexpected_token(&parser, t); }break;
					}
				}
				e->permute.permutations = push_array(arena, s8, permutations.count);
				mem_copy(e->permute.permutations, permutations.data, sizeof(s8) * (uz)permutations.count);
			}break;
			default:{}break;
			}
		}break;

		default:{ meta_parser_unexpected_token(&parser, token); }break;
		}
	}

	return result;
}

typedef struct {
	s8  name;
	u64 id;
} MetaShader;

typedef struct {
	s8 name;

	MetaShader *data;
	iz count;
	iz capacity;
} MetaShaderGroup;

typedef struct {
	MetaEntry **data;
	iz count;
	iz capacity;
} MetaEntryReferenceStack;

function s8
arena_stream_commit_and_reset(Arena *arena, Stream *s)
{
	s8 result = arena_stream_commit(arena, s);
	*s = arena_stream(*arena);
	return result;
}

function u64
meta_emit_shaders(Arena *arena, MetaShaderGroup *sg, MetaEntry *se,
                  MetaEntryReferenceStack *permute, u64 base_id)
{
	assert(se->kind == MetaEntryKind_Shader);
	u64 result    = 0;
	//s8  base_name = se->name;
	u64 count = 1;
	for (iz i = 0; i < permute->count; i++) {
		MetaEntry *e = permute->data[i];
		switch (e->kind) {
		case MetaEntryKind_Permute:{     count *= e->permute.max_permutation; }break;
		case MetaEntryKind_PermuteBits:{ count *= popcnt_u64(e->permute.max_permutation); }break;
		InvalidDefaultCase;
		}
	}
	da_reserve(arena, sg, (iz)count);

	//for (iz
	return result;
}

function b32
metagen_beamformer_files(Arena arena)
{
	b32 result = 0;

	if (setjmp(compiler_jmp_buf)) {
		/* NOTE(rnp): compiler error */
		return 0;
	}

	char *out = OUTPUT("out/metagen.c");
	if (needs_rebuild(out, "shader.meta")) {
		Arena scratch = sub_arena(&arena, MB(1), 16);
		MetaEntryStack entries = meta_entry_stack_from_file(&arena, scratch, "shader.meta");

		i32 stack_items[32];
		struct { i32 *data; iz capacity; iz count; } stack = {stack_items, countof(stack_items), 0};
		struct { MetaShaderGroup *data; iz capacity; iz count; } shader_groups = {0};

		MetaEntryReferenceStack permute_stack = {0};

		i32 depth     = 0;
		b32 in_shader = 0;
		u64 shader_id = 0;
		//MetaEntry       *last_entry    = 0;
		//MetaEntry       *current_entry = 0;
		MetaShaderGroup *current_shader_group = 0;
		for (iz i = 0; i < entries.count; i++) {
			MetaEntry *e = entries.data + i;
			if (e->kind == MetaEntryKind_EndScope)   depth--;
			meta_entry_print(e, depth);
			if (e->kind == MetaEntryKind_BeginScope) depth++;
			continue;

			switch (e->kind) {
			case MetaEntryKind_BeginScope:{
				*da_push(&scratch, &stack) = (i32)i;
			}break;
			case MetaEntryKind_EndScope:{
				i32 index = stack.data[--stack.count];
				if (index == 0)
					meta_compiler_error(entries.data + index, "Invalid base level scope");
				MetaEntry *last = entries.data + (index - 1);
				switch (last->kind) {
				case MetaEntryKind_Shader:{
					shader_id += meta_emit_shaders(&arena, current_shader_group, last, &permute_stack, shader_id);
					permute_stack.count = 0;
					in_shader           = 0;
				}break;
				default:{}break;
				}
			}break;
			case MetaEntryKind_ShaderGroup:{
				MetaShaderGroup *sg = da_push(&arena, &shader_groups);
				sg->name = e->name;
				current_shader_group = sg;
			}break;
			case MetaEntryKind_Shader:{
				if (!current_shader_group)
					meta_compiler_error(e, "Shader() declaration outside of ShaderGroup()");
				in_shader = 1;
			}break;
			case MetaEntryKind_Permute:{
				if (!in_shader)
					meta_compiler_error(e, "Permute() declaration outside of Shader()");
				*da_push(&scratch, &permute_stack) = e;
			}break;
			default:{}break;
			}
		}

		//for (;;/* stack frame in table */)
		{
			// jump to procedure for table entry kind
			// shader:
			//  - generate list of variations for inlcuding in shader kind enum
			//   - for each entry
			//     - generate an array of cases to iterate in load_shader
			//     - generate a function that takes
		}

		#if 0
		#define X(name, flag, ...) meta_push_line(&m, s8(#name " (" str(flag) ")"));
		meta_begin_matlab_class(&m, "OGLBeamformerLiveFeedbackFlags", "int32");
		meta_begin_scope(&m, s8("enumeration"));
		BEAMFORMER_LIVE_IMAGING_DIRTY_FLAG_LIST
		result &= meta_end_and_write_matlab(&m, out);

		meta_begin_matlab_class(&m, "OGLBeamformerDataKind", "int32");
		meta_begin_scope(&m, s8("enumeration"));
		BEAMFORMER_DATA_KIND_LIST
		result &= meta_end_and_write_matlab(&m, OUTPUT("matlab/OGLBeamformerDataKind.m"));
		#undef X

		#define X(kind, ...) meta_push_matlab_enum_with_value(&m, s8(#kind), BeamformerFilterKind_## kind);
		meta_begin_matlab_class(&m, "OGLBeamformerFilterKind", "int32");
		meta_begin_scope(&m, s8("enumeration"));
		BEAMFORMER_FILTER_KIND_LIST(,)
		result &= meta_end_and_write_matlab(&m, OUTPUT("matlab/OGLBeamformerFilterKind.m"));
		#undef X
		#endif
	}

	return result;
}

function b32
build_matlab_bindings(Arena arena)
{
	b32 result = 1;
	os_make_directory(OUTPUT("matlab"));

	Arena scratch = sub_arena(&arena, MB(1), 16);

	char *out = OUTPUT("matlab/OGLBeamformerLiveFeedbackFlags.m");
	if (needs_rebuild(out)) {
		/* TODO(rnp): recreate/clear directory incase these file names change */
		MetaprogramContext m = {.stream = arena_stream(arena)};

		#define X(name, flag, ...) meta_push_line(&m, s8(#name " (" str(flag) ")"));
		meta_begin_matlab_class(&m, "OGLBeamformerLiveFeedbackFlags", "int32");
		meta_begin_scope(&m, s8("enumeration"));
		BEAMFORMER_LIVE_IMAGING_DIRTY_FLAG_LIST
		result &= meta_end_and_write_matlab(&m, out);

		meta_begin_matlab_class(&m, "OGLBeamformerDataKind", "int32");
		meta_begin_scope(&m, s8("enumeration"));
		BEAMFORMER_DATA_KIND_LIST
		result &= meta_end_and_write_matlab(&m, OUTPUT("matlab/OGLBeamformerDataKind.m"));
		#undef X

		#define X(kind, ...) meta_push_matlab_enum_with_value(&m, s8(#kind), BeamformerFilterKind_## kind);
		meta_begin_matlab_class(&m, "OGLBeamformerFilterKind", "int32");
		meta_begin_scope(&m, s8("enumeration"));
		BEAMFORMER_FILTER_KIND_LIST(,)
		result &= meta_end_and_write_matlab(&m, OUTPUT("matlab/OGLBeamformerFilterKind.m"));
		#undef X

		#define X(kind, ...) meta_push_matlab_enum_with_value(&m, s8(#kind), BeamformerShaderKind_## kind);
		meta_begin_matlab_class(&m, "OGLBeamformerShaderStage", "int32");
		meta_begin_scope(&m, s8("enumeration"));
		COMPUTE_SHADERS
		result &= meta_end_and_write_matlab(&m, OUTPUT("matlab/OGLBeamformerShaderStage.m"));
		#undef X

		#define X(kind, ...) meta_push_matlab_enum_with_value(&m, s8(#kind), BeamformerTransmitMode_## kind);
		meta_begin_matlab_class(&m, "OGLBeamformerTransmitModes", "int32");
		meta_begin_scope(&m, s8("enumeration"));
		TRANSMIT_MODES_LIST
		result &= meta_end_and_write_matlab(&m, OUTPUT("matlab/OGLBeamformerTransmitModes.m"));
		#undef X

		#define X(kind, ...) meta_push_matlab_enum_with_value(&m, s8(#kind), BeamformerReceiveMode_## kind);
		meta_begin_matlab_class(&m, "OGLBeamformerReceiveModes", "int32");
		meta_begin_scope(&m, s8("enumeration"));
		RECEIVE_MODES_LIST
		result &= meta_end_and_write_matlab(&m, OUTPUT("matlab/OGLBeamformerReceiveModes.m"));
		#undef X

		#define X(kind, v, ...) meta_push_line(&m, s8(#kind " (" #v ")"));
		meta_begin_matlab_class(&m, "OGLBeamformerSamplingModes", "int32");
		meta_begin_scope(&m, s8("enumeration"));
		SAMPLING_MODES_LIST
		result &= meta_end_and_write_matlab(&m, OUTPUT("matlab/OGLBeamformerSamplingModes.m"));
		#undef X

		os_make_directory(OUTPUT("matlab/+OGLBeamformerFilter"));
		#define X(kind, ...) {OUTPUT("matlab/+OGLBeamformerFilter/" #kind ".m"), s8(#kind),  s8(#__VA_ARGS__)},
		read_only local_persist struct {char *out; s8 class, args;} filter_table[] = {
			BEAMFORMER_FILTER_KIND_LIST(,)
		};
		#undef X

		s8_list members = {0};
		for EachElement(filter_table, filter) {
			typeof(*filter_table) *f = filter_table + filter;
			members.count = 0;
			s8_list_from_s8(&members, &scratch, f->args);
			meta_begin_scope(&m, s8("classdef "), f->class, s8(" < OGLBeamformerFilter.BaseFilter"));

			meta_begin_scope(&m, s8("properties"));
			for (iz it = 0; it < members.count; it++)
				meta_push_matlab_property(&m, members.data[it], 1);
			meta_end_scope(&m, s8("end"));

			meta_begin_scope(&m, s8("methods"));
			meta_begin_line(&m, s8("function obj = "), f->class, s8("("));
			for (iz it = 0; it < members.count; it++)
				meta_push(&m, it > 0 ? s8(", ") : s8(""), members.data[it]);
			meta_end_line(&m, s8(")"));

			m.indentation_level++;
			for (iz it = 0; it < members.count; it++)
				meta_push_line(&m, s8("obj."), members.data[it], s8(" = "), members.data[it], s8(";"));
			result &= meta_end_and_write_matlab(&m, f->out);
		}

		meta_begin_matlab_class(&m, "BaseFilter");
		meta_begin_scope(&m, s8("methods"));
		meta_begin_scope(&m, s8("function out = Flatten(obj)"));
		meta_push_line(&m, s8("fields = struct2cell(struct(obj));"));
		meta_push_line(&m, s8("out    = zeros(1, numel(fields));"));
		meta_begin_scope(&m, s8("for i = 1:numel(fields)"));
		meta_push_line(&m, s8("out(i) = fields{i};"));
		result &= meta_end_and_write_matlab(&m, OUTPUT("matlab/+OGLBeamformerFilter/BaseFilter.m"));

		#define X(name, __t, __s, elements, ...) meta_push_line(&m, s8(#name "(1," #elements ")"));
		meta_begin_matlab_class(&m, "OGLBeamformerParameters");
		meta_begin_scope(&m, s8("properties"));
		BEAMFORMER_PARAMS_HEAD
		BEAMFORMER_UI_PARAMS
		result &= meta_end_and_write_matlab(&m, OUTPUT("matlab/OGLBeamformerParameters.m"));

		meta_begin_matlab_class(&m, "OGLBeamformerParametersHead");
		meta_begin_scope(&m, s8("properties"));
		BEAMFORMER_PARAMS_HEAD
		result &= meta_end_and_write_matlab(&m, OUTPUT("matlab/OGLBeamformerParametersHead.m"));

		meta_begin_matlab_class(&m, "OGLBeamformerParametersUI");
		meta_begin_scope(&m, s8("properties"));
		BEAMFORMER_UI_PARAMS
		result &= meta_end_and_write_matlab(&m, OUTPUT("matlab/OGLBeamformerParametersUI.m"));
		#undef X

		#define X(name, __t, __s, elements, ...) meta_push_matlab_property(&m, s8(#name), elements);
		meta_begin_matlab_class(&m, "OGLBeamformerLiveImagingParameters");
		meta_begin_scope(&m, s8("properties"));
		BEAMFORMER_LIVE_IMAGING_PARAMETERS_LIST
		result &= meta_end_and_write_matlab(&m, OUTPUT("matlab/OGLBeamformerLiveImagingParameters.m"));
		#undef X
	}

	return result;
}

i32
main(i32 argc, char *argv[])
{
	u64 start_time = os_get_timer_counter();
	g_argv0 = argv[0];

	b32 result  = 1;
	Arena arena = os_alloc_arena(MB(8));
	check_rebuild_self(arena, argc, argv);

	//if (!metagen_beamformer_files(arena)) return 1;

	Options options = parse_options(argc, argv);

	os_make_directory(OUTDIR);

	CommandList c = cmd_base(&arena, &options);
	if (!check_build_raylib(arena, c, options.debug)) return 1;

	/////////////////////////////////////
	// extra flags (unusable for raylib)
	cmd_append(&arena, &c, EXTRA_FLAGS);

	/////////////////
	// helpers/tests
	result &= build_matlab_bindings(arena);
	result &= build_helper_library(arena, c);
	if (options.tests) result &= build_tests(arena, c);

	//////////////////
	// static portion
	iz c_count = c.count;
	cmd_append(&arena, &c, OS_MAIN, OUTPUT_EXE("ogl"));
	cmd_pdb(&arena, &c, "ogl");
	if (options.debug) {
		if (!is_w32)  cmd_append(&arena, &c, "-Wl,--export-dynamic", "-Wl,-rpath,.");
		if (!is_msvc) cmd_append(&arena, &c, "-L.");
		cmd_append(&arena, &c, LINK_LIB("raylib"));
	} else {
		cmd_append(&arena, &c, OUTPUT(OS_STATIC_LIB("raylib")));
	}
	if (!is_msvc) cmd_append(&arena, &c, "-lm");
	if (is_unix)  cmd_append(&arena, &c, "-lGL");
	if (is_w32) {
		cmd_append(&arena, &c, LINK_LIB("user32"), LINK_LIB("shell32"), LINK_LIB("gdi32"),
		           LINK_LIB("opengl32"), LINK_LIB("winmm"), LINK_LIB("Synchronization"));
		if (!is_msvc) cmd_append(&arena, &c, "-Wl,--out-implib," OUTPUT(OS_STATIC_LIB("main")));
	}
	cmd_append(&arena, &c, (void *)0);

	result &= run_synchronous(arena, &c);
	c.count = c_count;

	/////////////////////////
	// hot reloadable portion
	//
	// NOTE: this is built after main because on w32 we need to export
	// gl function pointers for the reloadable portion to import
	if (options.debug) {
		if (is_msvc) {
			build_static_library_from_objects(arena, OUTPUT_LIB(OS_STATIC_LIB("main")),
			                                  arg_list(char *, "/def", "/name:ogl.exe"),
			                                  arg_list(char *, OUTPUT(OBJECT("main_w32"))));
		}
		result &= build_beamformer_as_library(arena, c);
	}

	if (options.time) {
		f64 seconds = (f64)(os_get_timer_counter() - start_time) / (f64)os_get_timer_frequency();
		build_log_info("took %0.03f [s]", seconds);
	}

	return result != 1;
}
