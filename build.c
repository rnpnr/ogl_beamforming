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
	da_reserve((a), (s), (s)->count + (item_count));                                \
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
	if (needs_rebuild(lib_header_out, "beamformer_parameters.h", "helpers/ogl_beamformer_lib_base.h")) {
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

function void
meta_begin_scope(MetaprogramContext *m, s8 line)
{
	meta_indent(m);
	m->indentation_level++;
	stream_append_s8s(&m->stream, line, s8("\n"));
}

function void
meta_push_line(MetaprogramContext *m, s8 line)
{
	meta_indent(m);
	stream_append_s8s(&m->stream, line, s8("\n"));
}

function void
meta_end_scope(MetaprogramContext *m, s8 line)
{
	m->indentation_level--;
	meta_push_line(m, line);
}

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

function b32
meta_end_and_write_matlab(MetaprogramContext *m, char *path)
{
	while (m->indentation_level > 0) meta_end_scope((m), s8("end"));
	b32 result = meta_write_and_reset(m, path);
	return result;
}

function b32
build_matlab_bindings(Arena arena)
{
	b32 result = 1;
	os_make_directory(OUTPUT("matlab"));

	char *out = OUTPUT("matlab/OGLBeamformerLiveFeedbackFlags.m");
	/* NOTE(rnp): if one file is outdated all files are outdated */
	if (needs_rebuild(out, "beamformer_parameters.h")) {
		/* TODO(rnp): recreate/clear directory incase these file names change */
		MetaprogramContext m = {.stream = arena_stream(arena)};

		#define X(name, flag, ...) meta_push_line(&m, s8(#name " (" str(flag) ")"));
		meta_begin_matlab_class(&m, "OGLBeamformerLiveFeedbackFlags", "int32");
		meta_begin_scope(&m, s8("enumeration"));
		BEAMFORMER_LIVE_IMAGING_DIRTY_FLAG_LIST
		result &= meta_end_and_write_matlab(&m, out);

		meta_begin_matlab_class(&m, "OGLBeamformerShaderStage", "int32");
		meta_begin_scope(&m, s8("enumeration"));
		COMPUTE_SHADERS
		result &= meta_end_and_write_matlab(&m, OUTPUT("matlab/OGLBeamformerShaderStage.m"));

		meta_begin_matlab_class(&m, "OGLBeamformerDataKind", "int32");
		meta_begin_scope(&m, s8("enumeration"));
		BEAMFORMER_DATA_KIND_LIST
		result &= meta_end_and_write_matlab(&m, OUTPUT("matlab/OGLBeamformerDataKind.m"));
		#undef X

		#define X(name, __t, __s, elements, ...) meta_push_line(&m, s8(#name "(1," #elements ")"));
		meta_begin_matlab_class(&m, "OGLBeamformerParameters");
		meta_begin_scope(&m, s8("properties"));
		BEAMFORMER_PARAMS_HEAD
		BEAMFORMER_UI_PARAMS
		BEAMFORMER_PARAMS_TAIL
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
		/* NOTE(rnp): (gnu) ld doesn't properly export global symbols without this */
		if (is_gcc) cmd_append(&arena, &c, "-Wl,--export-dynamic");

		if (!is_w32)  cmd_append(&arena, &c, "-Wl,-rpath,.");
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
