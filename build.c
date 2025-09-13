/* See LICENSE for license details. */
/* NOTE: inspired by nob: https://github.com/tsoding/nob.h */

/* TODO(rnp):
 * [ ]: refactor: "base" shaders should only be reloadable shaders
 *      - internally when a shader with no file is encountered it should
 *        not get pushed as a "base" shader.
 * [ ]: bug: column indicator for compile error is off
 * [ ]: bake shaders and font data into binary
 *      - for shaders there is a way of making a separate data section and referring
 *        to it with extern from the C source (bake both data and size)
 *      - use objcopy, maybe need linker script maybe command line flags for ld will work
 * [ ]: cross compile/override baked compiler
 * [ ]: msvc build doesn't detect out of date files correctly
 * [ ]: seperate dwarf debug info
 */
#include <stdarg.h>
#include <setjmp.h>
#include <stdio.h>

#include "util.h"

#define BeamformerShaderKind_ComputeCount (1)
#include "beamformer_parameters.h"

global char *g_argv0;

#define OUTDIR    "out"
#define OUTPUT(s) OUTDIR "/" s

#if COMPILER_MSVC
  #define COMMON_FLAGS     "-nologo", "-std:c11", "-Fo:" OUTDIR "\\", "-Z7", "-Zo"
  #define DEBUG_FLAGS      "-Od", "-D_DEBUG"
  #define OPTIMIZED_FLAGS  "-O2"
  #define EXTRA_FLAGS      ""
#else
  #define COMMON_FLAGS     "-std=c11", "-pipe", "-Wall"
  #define DEBUG_FLAGS      "-O0", "-D_DEBUG", "-Wno-unused-function"
  #define OPTIMIZED_FLAGS  "-O3"
  #define EXTRA_FLAGS_BASE "-Werror", "-Wextra", "-Wshadow", "-Wconversion", "-Wno-unused-parameter", \
                           "-Wno-error=unused-function", "-funsafe-math-optimizations", "-fno-math-errno"
  #if COMPILER_GCC
    #define EXTRA_FLAGS EXTRA_FLAGS_BASE, "-Wno-unused-variable"
  #else
    #define EXTRA_FLAGS EXTRA_FLAGS_BASE
  #endif
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

#define cmd_append_count da_append_count
#define cmd_append(a, s, ...) da_append_count(a, s, ((char *[]){__VA_ARGS__}), \
                                              (iz)(sizeof((char *[]){__VA_ARGS__}) / sizeof(char *)))

DA_STRUCT(char *, Command);

typedef struct {
	b32   debug;
	b32   generic;
	b32   sanitize;
	b32   tests;
	b32   time;
} Options;

#define BUILD_LOG_KINDS \
	X(Error,    "\x1B[31m[ERROR]\x1B[0m    ") \
	X(Warning,  "\x1B[33m[WARNING]\x1B[0m  ") \
	X(Generate, "\x1B[32m[GENERATE]\x1B[0m ") \
	X(Info,     "\x1B[33m[INFO]\x1B[0m     ") \
	X(Command,  "\x1B[36m[COMMAND]\x1B[0m  ")
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
#define build_log_generate(...) build_log(BuildLogKind_Generate, ##__VA_ARGS__)
#define build_log_info(...)     build_log(BuildLogKind_Info,     ##__VA_ARGS__)
#define build_log_command(...)  build_log(BuildLogKind_Command,  ##__VA_ARGS__)
#define build_log_warning(...)  build_log(BuildLogKind_Warning,  ##__VA_ARGS__)
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
			InvalidCodePath;
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

	if (o->debug && is_unix) cmd_append(a, &result, "-gdwarf-4");

	/* NOTE(rnp): need to avoid w32-gcc for ci */
	b32 sanitize = o->sanitize && !is_msvc && !(is_w32 && is_gcc);
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
cc_single_file(Arena a, CommandList cc, char *exe, char *src, char *dest, char **tail, iz tail_count)
{
	char *executable[] = {src, is_msvc? "/Fe:" : "-o", dest};
	char *object[]     = {is_msvc? "/c" : "-c", src, is_msvc? "/Fo:" : "-o", dest};


	cmd_append_count(&a, &cc, exe? executable : object,
	                 exe? countof(executable) : countof(object));
	if (exe) cmd_pdb(&a, &cc, exe);
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
		X("throughput", LINK_LIB("zstd"), W32_DECL(LINK_LIB("Synchronization")))

	os_make_directory(OUTPUT("tests"));
	if (!is_msvc) cmd_append(&arena, &cc, "-Wno-unused-function");
	cmd_append(&arena, &cc, "-I.", "-Ihelpers");

	b32 result = 1;
	iz cc_count = cc.count;
	#define X(prog, ...) \
		result &= cc_single_file(arena, cc, prog, "tests/" prog ".c", \
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
	Arena  scratch;
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

#define meta_push(m, ...) meta_push_(m, arg_list(s8, __VA_ARGS__))
function void
meta_push_(MetaprogramContext *m, s8 *items, iz count)
{
	stream_append_s8s_(&m->stream, items, count);
}

#define meta_pad(m, b, n)             stream_pad(&(m)->stream, (b), (n))
#define meta_indent(m)                meta_pad((m), '\t', (m)->indentation_level)
#define meta_begin_line(m, ...)  do { meta_indent(m); meta_push(m, __VA_ARGS__);           } while(0)
#define meta_end_line(m, ...)                         meta_push(m, __VA_ARGS__, s8("\n"))
#define meta_push_line(m, ...)   do { meta_indent(m); meta_push(m, __VA_ARGS__, s8("\n")); } while(0)
#define meta_begin_scope(m, ...) do { meta_push_line(m, __VA_ARGS__); (m)->indentation_level++; } while(0)
#define meta_end_scope(m, ...)   do { (m)->indentation_level--; meta_push_line(m, __VA_ARGS__); } while(0)
#define meta_push_u64(m, n)           stream_append_u64(&(m)->stream, (n))
#define meta_push_u64_hex(m, n)       stream_append_hex_u64(&(m)->stream, (n))

#define meta_begin_matlab_class_cracker(_1, _2, FN, ...) FN
#define meta_begin_matlab_class_1(m, name) meta_begin_scope(m, s8("classdef " name))
#define meta_begin_matlab_class_2(m, name, type) \
  meta_begin_scope(m, s8("classdef " name " < " type))

#define meta_begin_matlab_class(m, ...) \
  meta_begin_matlab_class_cracker(__VA_ARGS__, \
                                  meta_begin_matlab_class_2, \
                                  meta_begin_matlab_class_1)(m, __VA_ARGS__)

function void
meta_push_matlab_property(MetaprogramContext *m, s8 name, u64 length, s8 kind)
{
	meta_begin_line(m, name, s8("(1,"));
	meta_push_u64(m, (u64)length);
	meta_end_line(m, s8(")"), kind.len > 0 ? s8(" ") : s8(""), kind);
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
	X(Invalid)      \
	X(BeginScope)   \
	X(EndScope)     \
	X(Enumeration)  \
	X(Flags)        \
	X(Permute)      \
	X(PermuteFlags) \
	X(Shader)       \
	X(ShaderGroup)  \
	X(SubShader)

typedef enum {
	#define X(k, ...) MetaEntryKind_## k,
	META_ENTRY_KIND_LIST
	#undef X
	MetaEntryKind_Count,
} MetaEntryKind;

#define X(k, ...) #k,
read_only global char *meta_entry_kind_strings[] = {META_ENTRY_KIND_LIST};
#undef X

typedef struct { u32 line, column; } MetaLocation;

#define META_ENTRY_ARGUMENT_KIND_LIST \
	X(None)   \
	X(String) \
	X(Array)

#define X(k, ...) MetaEntryArgumentKind_## k,
typedef enum {META_ENTRY_ARGUMENT_KIND_LIST} MetaEntryArgumentKind;
#undef X

typedef struct {
	MetaEntryArgumentKind kind;
	MetaLocation          location;
	union {
		s8 string;
		struct {
			s8  *strings;
			u64  count;
		};
	};
} MetaEntryArgument;

typedef struct {
	MetaEntryKind      kind;
	u32                argument_count;
	MetaEntryArgument *arguments;
	s8                 name;
	MetaLocation       location;
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
	X('}', EndScope)

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
	s8 s;
	MetaLocation location;
} MetaParsePoint;

typedef struct {
	MetaParsePoint p;
	MetaParseUnion u;
	MetaParsePoint save_point;
} MetaParser;

global char    *compiler_file;
global jmp_buf  compiler_jmp_buf;

#define meta_parser_save(v)    (v)->save_point = (v)->p
#define meta_parser_restore(v) swap((v)->p, (v)->save_point)
#define meta_parser_commit(v)  meta_parser_restore(v)

#define meta_compiler_error_message(loc, format, ...) \
	fprintf(stderr, "%s:%u:%u: error: "format, compiler_file, \
	        loc.line + 1, loc.column + 1, ##__VA_ARGS__)

#define meta_compiler_error(loc, format, ...) do { \
	meta_compiler_error_message(loc, format, ##__VA_ARGS__); \
	meta_error(); \
} while (0)

#define meta_entry_error(e, ...) meta_entry_error_column((e), (i32)(e)->location.column, __VA_ARGS__)
#define meta_entry_error_column(e, column, ...) do { \
	meta_compiler_error_message((e)->location, __VA_ARGS__); \
	meta_entry_print((e), 1, (column)); \
	meta_error(); \
} while(0)

#define meta_entry_error_location(e, loc, ...) do { \
		meta_compiler_error_message((loc), __VA_ARGS__); \
		meta_entry_print((e), 1, (i32)(loc).column); \
		meta_error(); \
} while (0)

function no_return void
meta_error(void)
{
	assert(0);
	longjmp(compiler_jmp_buf, 1);
}

function void
meta_entry_print(MetaEntry *e, i32 depth, i32 caret)
{
	char *kind = meta_entry_kind_strings[e->kind];
	if (e->kind == MetaEntryKind_BeginScope) kind = "{";
	if (e->kind == MetaEntryKind_EndScope)   kind = "}";

	fprintf(stderr, "%*s@%s", depth * 2, "", kind);

	if (e->argument_count) {
		fprintf(stderr, "(");
		for (u32 i = 0; i < e->argument_count; i++) {
			MetaEntryArgument *a = e->arguments + i;
			if (i != 0) fprintf(stderr, " ");
			if (a->kind == MetaEntryArgumentKind_Array) {
				fprintf(stderr, "[");
				for (u64 j = 0; j < a->count; j++) {
					if (j != 0) fprintf(stderr, " ");
					fprintf(stderr, "%.*s", (i32)a->strings[j].len, a->strings[j].data);
				}
				fprintf(stderr, "]");
			} else {
				fprintf(stderr, "%.*s", (i32)a->string.len, a->string.data);
			}
		}
		fprintf(stderr, ")");
	}
	if (e->name.len) fprintf(stderr, " %.*s", (i32)e->name.len, e->name.data);

	if (caret >= 0) fprintf(stderr, "\n%.*s^", depth * 2 + caret, "");

	fprintf(stderr, "\n");
}

function MetaEntryKind
meta_entry_kind_from_string(s8 s)
{
	#define X(k, ...) s8_comp(#k),
	read_only local_persist s8 kinds[] = {META_ENTRY_KIND_LIST};
	#undef X
	MetaEntryKind result = MetaEntryKind_Invalid;
	for EachNonZeroEnumValue(MetaEntryKind, kind) {
		if (s8_equal(kinds[kind], s)) {
			result = kind;
			break;
		}
	}
	return result;
}

function void
meta_parser_trim(MetaParser *p)
{
	u8 *s, *end = p->p.s.data + p->p.s.len;
	b32 done    = 0;
	b32 comment = 0;
	for (s = p->p.s.data; !done && s != end;) {
		switch (*s) {
		case '\r': case '\t': case ' ':
		{
			p->p.location.column++;
		}break;
		case '\n':{ p->p.location.line++; p->p.location.column = 0; comment = 0; }break;
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
		case ' ': case '\n': case '\r': case '\t':
		{done = 1;}break;
		case '/':{
			done = (result.len + 1 < p->p.s.len) && (p->p.s.data[result.len + 1] == '/');
		}break;
		default:{}break;
		}
		if (done) break;
	}
	p->p.location.column += (u32)result.len;
	p->p.s.data          += result.len;
	p->p.s.len           -= result.len;
	return result;
}

function s8
meta_parser_token_name(MetaParser *p, MetaParseToken t)
{
	s8 result = s8("\"invalid\"");
	read_only local_persist s8 names[MetaParseToken_Count] = {
		[MetaParseToken_EOF] = s8_comp("\"EOF\""),
		#define X(k, v, ...) [MetaParseToken_## v] = s8_comp(#k),
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
		if (chop) { s8_chop(&p->p.s, 1); p->p.location.column++; }

		meta_parser_trim(p);
		switch (result) {
		case MetaParseToken_String:{ p->u.string = meta_parser_extract_string(p); }break;

		/* NOTE(rnp): '{' and '}' are shorthand for @BeginScope and @EndScope */
		case MetaParseToken_BeginScope:{ p->u.kind = MetaEntryKind_BeginScope; }break;
		case MetaParseToken_EndScope:{   p->u.kind = MetaEntryKind_EndScope;   }break;

		case MetaParseToken_Entry:{
			s8 kind = meta_parser_extract_string(p);
			p->u.kind = meta_entry_kind_from_string(kind);
			if (p->u.kind == MetaEntryKind_Invalid) {
				meta_compiler_error(p->p.location, "invalid keyword: @%.*s\n", (i32)kind.len, kind.data);
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

function void
meta_parser_unexpected_token(MetaParser *p, MetaParseToken t)
{
	meta_parser_restore(p);
	s8 token_name = meta_parser_token_name(p, t);
	meta_compiler_error(p->p.location, "unexpected token: %.*s\n", (i32)token_name.len, token_name.data);
}

function void
meta_parser_arguments(MetaParser *p, MetaEntry *e, Arena *arena)
{
	if (meta_parser_peek_token(p) == MetaParseToken_BeginArgs) {
		meta_parser_commit(p);

		MetaEntryArgument *arg = e->arguments = push_struct(arena, MetaEntryArgument);
		b32 array = 0;
		for (MetaParseToken token = meta_parser_token(p);
		     token != MetaParseToken_EndArgs;
		     token = meta_parser_token(p))
		{
			if (!arg) arg = push_struct(arena, MetaEntryArgument);
			switch (token) {
			case MetaParseToken_String:{
				if (array) {
					assert((u8 *)(arg->strings + arg->count) == arena->beg);
					*push_struct(arena, s8) = p->u.string;
					arg->count++;
				} else {
					e->argument_count++;
					arg->kind     = MetaEntryArgumentKind_String;
					arg->string   = p->u.string;
					arg->location = p->p.location;
					arg           = 0;
				}
			}break;
			case MetaParseToken_BeginArray:{
				arg->kind     = MetaEntryArgumentKind_Array;
				arg->strings  = (s8 *)arena_aligned_start(*arena, alignof(s8));
				arg->location = p->p.location;
				array         = 1;
			}break;
			case MetaParseToken_EndArray:{
				e->argument_count++;
				array = 0;
				arg   = 0;
			}break;
			default:{ meta_parser_unexpected_token(p, token); }break;
			}
		}
	}
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
			e->kind     = parser.u.kind;
			e->location = parser.save_point.location;

			if (token == MetaParseToken_Entry)
				meta_parser_arguments(&parser, e, arena);

			if (meta_parser_peek_token(&parser) == MetaParseToken_String) {
				meta_parser_commit(&parser);
				e->name = parser.u.string;
			}
		}break;

		default:{ meta_parser_unexpected_token(&parser, token); }break;
		}
	}

	return result;
}

#define meta_entry_argument_expected(e, ...) \
	meta_entry_argument_expected_((e), arg_list(s8, __VA_ARGS__))
function void
meta_entry_argument_expected_(MetaEntry *e, s8 *args, uz count)
{
	if (e->argument_count != count) {
		meta_compiler_error_message(e->location, "incorrect argument count for entry %s() got: %u expected: %u\n",
		                            meta_entry_kind_strings[e->kind], e->argument_count, (u32)count);
		fprintf(stderr, "  format: @%s(", meta_entry_kind_strings[e->kind]);
		for (uz i = 0; i < count; i++) {
			if (i != 0) fprintf(stderr, ", ");
			fprintf(stderr, "%.*s", (i32)args[i].len, args[i].data);
		}
		fprintf(stderr, ")\n");
		meta_error();
	}
}

function MetaEntryArgument
meta_entry_argument_expect(MetaEntry *e, u32 index, MetaEntryArgumentKind kind)
{
	#define X(k, ...) #k,
	read_only local_persist char *kinds[] = {META_ENTRY_ARGUMENT_KIND_LIST};
	#undef X

	assert(e->argument_count > index);
	MetaEntryArgument result = e->arguments[index];

	if (result.kind != kind) {
		meta_entry_error_location(e, result.location, "unexpected argument kind: expected %s but got: %s\n",
		                          kinds[kind], kinds[result.kind]);
	}

	if (kind == MetaEntryArgumentKind_Array && result.count == 0)
		meta_entry_error_location(e, result.location, "array arguments must have at least 1 element\n");

	return result;
}

typedef struct {
	s8_list *data;
	iz       count;
	iz       capacity;
} s8_list_table;

typedef struct {
	iz kind;
	iz variation;
} MetaEnumeration;

typedef struct {
	u32 *data;
	iz   count;
	iz   capacity;
} MetaIDList;

typedef struct {
	u32 *global_flags;
	u16  local_flags;
	u16  global_flags_count;
} MetaShaderPermutation;
DA_STRUCT(MetaShaderPermutation, MetaShaderPermutation);

typedef struct {
	MetaShaderPermutationList permutations;
	MetaIDList                global_flag_ids;
	MetaIDList                global_enumeration_ids;
	u32                       base_name_id;
	u32                       flag_list_id;
} MetaShader;
DA_STRUCT(MetaShader, MetaShader);

typedef struct {
	MetaShader *shader;
	MetaIDList  sub_shaders;
	s8          file;
} MetaBaseShader;
DA_STRUCT(MetaBaseShader, MetaBaseShader);

typedef struct {
	i32 first_match_vector_index;
	i32 one_past_last_match_vector_index;
	i32 sub_field_count;
	b32 has_local_flags;
} MetaShaderDescriptor;

typedef struct {
	s8         name;
	MetaIDList shaders;
} MetaShaderGroup;
DA_STRUCT(MetaShaderGroup, MetaShaderGroup);

typedef struct {
	Arena *arena, scratch;

	s8_list               enumeration_kinds;
	s8_list_table         enumeration_members;

	s8_list_table         flags_for_shader;

	MetaShaderGroupList   shader_groups;
	MetaShaderList        shaders;
	MetaBaseShaderList    base_shaders;
	s8_list               shader_names;

	MetaShaderDescriptor *shader_descriptors;
} MetaContext;


function u32
metagen_pack_permutation(MetaContext *ctx, MetaEnumeration e)
{
	u32 result = ((u32)(e.kind & 0xFFFFu) << 16u) | (u32)(e.variation & 0xFFFFu);
	return result;
}

function MetaEnumeration
metagen_unpack_permutation(MetaContext *ctx, u32 packed)
{
	MetaEnumeration result;
	result.kind      = (iz)(packed >> 16u);
	result.variation = (iz)(packed & 0xFFFFu);
	assert(result.kind      < ctx->enumeration_kinds.count);
	assert(result.variation < ctx->enumeration_members.data[result.kind].count);
	return result;
}

function s8
metagen_permutation_kind(MetaContext *ctx, u32 packed)
{
	MetaEnumeration p = metagen_unpack_permutation(ctx, packed);
	s8 result = ctx->enumeration_kinds.data[p.kind];
	return result;
}

function s8
metagen_permutation_variation(MetaContext *ctx, u32 packed)
{
	MetaEnumeration p = metagen_unpack_permutation(ctx, packed);
	s8 result = ctx->enumeration_members.data[p.kind].data[p.variation];
	return result;
}

function iz
meta_lookup_string_slow(s8_list *sv, s8 s)
{
	// TODO(rnp): obviously this is slow
	iz result = -1;
	for (iz i = 0; i < sv->count; i++) {
		if (s8_equal(s, sv->data[i])) {
			result = i;
			break;
		}
	}
	return result;
}

function iz
meta_lookup_id_slow(MetaIDList *v, u32 id)
{
	// TODO(rnp): obviously this is slow
	iz result = -1;
	for (iz i = 0; i < v->count; i++) {
		if (id == v->data[i]) {
			result = i;
			break;
		}
	}
	return result;
}

function iz
meta_intern_string(MetaContext *ctx, s8_list *sv, s8 s)
{
	iz result = meta_lookup_string_slow(sv, s);
	if (result < 0) {
		*da_push(ctx->arena, sv) = s;
		result = sv->count - 1;
	}
	return result;
}

function iz
meta_intern_id(MetaContext *ctx, MetaIDList *v, u32 id)
{
	iz result = meta_lookup_id_slow(v, id);
	if (result < 0) {
		*da_push(ctx->arena, v) = id;
		result = v->count - 1;
	}
	return result;
}

function iz
meta_enumeration_id(MetaContext *ctx, s8 kind)
{
	iz result = meta_intern_string(ctx, &ctx->enumeration_kinds, kind);
	if (ctx->enumeration_kinds.count != ctx->enumeration_members.count) {
		da_push(ctx->arena, &ctx->enumeration_members);
		assert(result == (ctx->enumeration_members.count - 1));
	}
	return result;
}

function MetaEnumeration
meta_commit_enumeration(MetaContext *ctx, s8 kind, s8 variation)
{
	iz kidx = meta_enumeration_id(ctx, kind);
	iz vidx = meta_intern_string(ctx, ctx->enumeration_members.data + kidx, variation);
	MetaEnumeration result = {.kind = kidx, .variation = vidx};
	return result;
}

function u16
meta_pack_shader_name(MetaContext *ctx, s8 base_name, MetaLocation loc)
{
	iz result = meta_intern_string(ctx, &ctx->shader_names, base_name);
	if (result > (iz)U16_MAX)
		meta_compiler_error(loc, "maximum base shaders exceeded: limit: %lu\n", U16_MAX);
	return (u16)result;
}

function u8
meta_commit_shader_flag(MetaContext *ctx, u32 flag_list_id, s8 flag, MetaEntry *e)
{
	assert(flag_list_id < ctx->flags_for_shader.count);
	iz index = meta_intern_string(ctx, ctx->flags_for_shader.data + flag_list_id, flag);
	if (index > 7) meta_entry_error(e, "Shaders only support 8 local flags\n");
	u8 result = (u8)index;
	return result;
}

typedef struct {
	u16 entry_id;
	struct {u8 current; u8 target;} cursor;
	u32 permutation_id;
} MetaShaderPermutationStackFrame;

typedef struct {
	MetaEntry *base_entry;

	MetaShaderPermutationStackFrame *data;
	iz count;
	iz capacity;
} MetaShaderPermutationStack;

function void
meta_pack_shader_permutation(MetaContext *ctx, MetaShaderPermutation *sp, MetaShader *base_shader,
                             MetaShaderPermutationStack *stack, MetaEntry *last, u32 frame_cursor)
{
	////////////////////////////////////
	// NOTE: fill ids from up the stack
	u32 global_flag_index = 0;
	for (iz i = 0; i < stack->count; i++) {
		MetaShaderPermutationStackFrame *f = stack->data + i;
		MetaEntry         *e = stack->base_entry + f->entry_id;
		MetaEntryArgument *a = e->arguments;
		u32 cursor = f->cursor.current;
		switch (e->kind) {
		case MetaEntryKind_PermuteFlags:{
			if (f->permutation_id == U32_MAX) {
				u32 test = cursor, packed = 0;
				for EachBit(test, flag) {
					u32 flag_index = meta_commit_shader_flag(ctx, base_shader->flag_list_id, a->strings[flag], e);
					packed |= (1u << flag_index);
				}
				f->permutation_id = packed;
			}
			sp->local_flags |= (u8)f->permutation_id;
		}break;
		case MetaEntryKind_Permute:{
			if (f->permutation_id == U32_MAX) {
				MetaEnumeration p = meta_commit_enumeration(ctx, a[0].string, a[1].strings[cursor]);
				f->permutation_id = ((u32)(p.kind & 0xFFFFu) << 16) | (u32)(p.variation & 0xFFFFu);
				meta_intern_id(ctx, &base_shader->global_flag_ids, (u32)p.kind);
			}
			sp->global_flags[global_flag_index++] = f->permutation_id;
		}break;
		InvalidDefaultCase;
		}
	}

	///////////////////////////////////
	// NOTE: fill ids from stack frame
	MetaEntryArgument *a = last->arguments;
	switch (last->kind) {
	case MetaEntryKind_PermuteFlags:{
		u32 packed = 0, test = frame_cursor;
		for EachBit(test, flag) {
			u32 flag_index = meta_commit_shader_flag(ctx, base_shader->flag_list_id, a->strings[flag], last);
			packed |= (1u << flag_index);
		}
		sp->local_flags |= (u8)packed;
	}break;
	case MetaEntryKind_Permute:{
		MetaEnumeration p = meta_commit_enumeration(ctx, a[0].string, a[1].strings[frame_cursor]);
		sp->global_flags[global_flag_index++] = metagen_pack_permutation(ctx, p);
		meta_intern_id(ctx, &base_shader->global_flag_ids, (u32)p.kind);
	}break;
	InvalidDefaultCase;
	}
}

function void
meta_pop_and_pack_shader_permutations(MetaContext *ctx, MetaShader *base_shader, u32 local_flags,
                                      MetaShaderPermutationStack *stack)
{
	assert(stack->count > 0);

	u32 global_flag_count = 0;
	for (iz i = 0; i < stack->count; i++) {
		switch (stack->base_entry[stack->data[i].entry_id].kind) {
		case MetaEntryKind_PermuteFlags:{}break;
		case MetaEntryKind_Permute:{ global_flag_count++; }break;
		InvalidDefaultCase;
		}
	}

	MetaShaderPermutationStackFrame *f = stack->data + (--stack->count);
	MetaEntry *last = stack->base_entry + f->entry_id;
	assert(f->cursor.current == 0);
	for (; f->cursor.current < f->cursor.target; f->cursor.current++) {
		MetaShaderPermutation *sp = da_push(ctx->arena, &base_shader->permutations);
		sp->global_flags_count = (u8)global_flag_count;
		sp->global_flags       = push_array(ctx->arena, typeof(*sp->global_flags), global_flag_count);
		sp->local_flags        = (u16)local_flags;

		meta_pack_shader_permutation(ctx, sp, base_shader, stack, last, f->cursor.current);
	}
}

function void
meta_emit_shader_permutations(MetaContext *ctx, Arena scratch, MetaShader *s, u32 local_flags,
                              MetaEntry *entries, iz entry_count)
{
	assert(entry_count > 0);
	assert(entries[0].kind == MetaEntryKind_Permute ||
	       entries[0].kind == MetaEntryKind_PermuteFlags ||
	       entries[0].kind == MetaEntryKind_SubShader);

	MetaShaderPermutationStack stack = {.base_entry = entries};
	da_reserve(&scratch, &stack, 32);

	b32 done = 0;
	for (iz i = 0; i < entry_count && !done; i++) {
		MetaEntry *e = entries + i;
		switch (e->kind) {
		case MetaEntryKind_PermuteFlags:
		case MetaEntryKind_Permute:
		{
			if (stack.count && stack.data[stack.count - 1].entry_id == (u16)i) {
				MetaShaderPermutationStackFrame *f = stack.data + (stack.count - 1);
				f->permutation_id = U32_MAX;
				f->cursor.current++;
				if (f->cursor.current == f->cursor.target) {
					stack.count--;
					done = stack.count == 0;
				}
			} else {
				u8 target;
				if (e->kind == MetaEntryKind_Permute) {
					meta_entry_argument_expected(e, s8("kind"), s8("[id ...]"));
					target = (u8)meta_entry_argument_expect(e, 1, MetaEntryArgumentKind_Array).count;
				} else {
					meta_entry_argument_expected(e, s8("[id ...]"));
					u32 count = (u32)meta_entry_argument_expect(e, 0, MetaEntryArgumentKind_Array).count;
					target = (u8)(2u << (count - 1));
				}
				*da_push(&scratch, &stack) = (MetaShaderPermutationStackFrame){
					.entry_id       = (u16)i,
					.permutation_id = U32_MAX,
					.cursor.target  = target,
				};
			}
		}break;
		case MetaEntryKind_SubShader:{}break;
		case MetaEntryKind_BeginScope:{}break;
		case MetaEntryKind_EndScope:{
			meta_pop_and_pack_shader_permutations(ctx, s, local_flags, &stack);
			if (stack.count != 0)
				i = stack.data[stack.count - 1].entry_id - 1;
		}break;
		InvalidDefaultCase;
		}
	}
	if (stack.count) {
		assert(stack.count == 1);
		meta_pop_and_pack_shader_permutations(ctx, s, local_flags, &stack);
	}
}

function iz
meta_pack_shader(MetaContext *ctx, MetaShaderGroup *sg, Arena scratch, MetaEntry *entries, iz entry_count)
{
	assert(entries[0].kind == MetaEntryKind_Shader);

	MetaBaseShader *base_shader = da_push(ctx->arena, &ctx->base_shaders);
	MetaShader     *s           = da_push(ctx->arena, &ctx->shaders);
	*da_push(ctx->arena, &sg->shaders) = (u32)da_index(s, &ctx->shaders);
	{
		s8_list *flag_list = da_push(ctx->arena, &ctx->flags_for_shader);
		s->flag_list_id    = (u32)da_index(flag_list, &ctx->flags_for_shader);
	}

	base_shader->shader = s;
	if (entries->argument_count > 1) {
		meta_entry_argument_expected(entries, s8("[file_name]"));
	} else if (entries->argument_count == 1) {
		base_shader->file = meta_entry_argument_expect(entries, 0, MetaEntryArgumentKind_String).string;
	}
	s->base_name_id = meta_pack_shader_name(ctx, entries->name, entries->location);

	i32 stack_items[32];
	struct { i32 *data; iz capacity; iz count; } stack = {stack_items, countof(stack_items), 0};

	iz result;
	b32 in_sub_shader = 0;
	for (result = 0; result < entry_count; result++) {
		MetaEntry *e = entries + result;
		switch (e->kind) {
		case MetaEntryKind_BeginScope:{}break;
		case MetaEntryKind_SubShader:{
			if (in_sub_shader) goto error;
			in_sub_shader = 1;
		} /* FALLTHROUGH */
		case MetaEntryKind_PermuteFlags:
		case MetaEntryKind_Permute:
		case MetaEntryKind_Shader:
		{
			*da_push(&scratch, &stack) = (i32)result;
			if ((result + 1 < entry_count) && entries[result + 1].kind == MetaEntryKind_BeginScope)
				break;
		} /* FALLTHROUGH */
		case MetaEntryKind_EndScope:{
			i32 index = stack.data[--stack.count];
			MetaEntry *ended = entries + index;
			if (index == 0) {
				assert(stack.count == 0 && ended->kind == MetaEntryKind_Shader);
				// NOTE(rnp): emit an empty single permutation
				if (s->permutations.count == 0)
					da_push(ctx->arena, &s->permutations);
			} else {
				u32 local_flags = 0;
				if (stack.count > 0 && entries[stack.data[stack.count - 1]].kind == MetaEntryKind_Shader) {
					MetaShader *fill = s;
					if (ended->kind == MetaEntryKind_SubShader) {
						fill = da_push(ctx->arena, &ctx->shaders);
						u32 sid = (u32)da_index(fill, &ctx->shaders);
						*da_push(ctx->arena, &sg->shaders) = sid;
						*da_push(ctx->arena, &base_shader->sub_shaders) = sid;

						fill->flag_list_id = s->flag_list_id;
						fill->base_name_id = meta_pack_shader_name(ctx, ended->name, ended->location);
						local_flags = 1u << meta_commit_shader_flag(ctx, s->flag_list_id, ended->name, ended);
						in_sub_shader = 0;
					}
					meta_emit_shader_permutations(ctx, scratch, fill, local_flags, ended, result - index + 1);
				}
			}
		}break;
		case MetaEntryKind_Enumeration:{
			meta_entry_argument_expected(e, s8("kind"));
			s8 kind = meta_entry_argument_expect(e, 0, MetaEntryArgumentKind_String).string;
			iz kid  = meta_enumeration_id(ctx, kind);
			meta_intern_id(ctx, &s->global_enumeration_ids, (u32)kid);
		}break;
		case MetaEntryKind_Flags:{
			meta_entry_argument_expected(e, s8("[flag ...]"));
			MetaEntryArgument flags = meta_entry_argument_expect(e, 0, MetaEntryArgumentKind_Array);
			for (u32 index = 0; index < flags.count; index++)
				meta_commit_shader_flag(ctx, s->flag_list_id, flags.strings[index], e);
		}break;

		default:
		error:
		{
			meta_entry_error(e, "invalid nested @%s() in @%s()\n",
			                 meta_entry_kind_strings[e->kind],
			                 meta_entry_kind_strings[MetaEntryKind_Shader]);
		}break;
		}
		if (stack.count == 0)
			break;
	}

	return result;
}

function void
metagen_push_table(MetaprogramContext *m, Arena scratch, s8 row_start, s8 row_end,
                   s8 **column_strings, uz rows, uz columns)
{
	u32 *column_widths = 0;
	if (columns > 1) {
		column_widths = push_array(&scratch, u32, (iz)columns - 1);
		for (uz column = 0; column < columns - 1; column++) {
			s8 *strings = column_strings[column];
			for (uz row = 0; row < rows; row++)
				column_widths[column] = MAX(column_widths[column], (u32)strings[row].len);
		}
	}

	for (uz row = 0; row < rows; row++) {
		meta_begin_line(m, row_start);
		for (uz column = 0; column < columns; column++) {
			s8 text = column_strings[column][row];
			meta_push(m, text);
			i32 pad = columns > 1 ? 1 : 0;
			if (column_widths && column < columns - 1)
				pad += (i32)column_widths[column] - (i32)text.len;
			if (column < columns - 1) meta_pad(m, ' ', pad);
		}
		meta_end_line(m, row_end);
	}
}

function void
metagen_push_c_struct(MetaprogramContext *m, s8 kind, s8 *types, uz types_count, s8 *fields, uz fields_count)
{
	assert(fields_count == types_count);
	meta_begin_scope(m, s8("typedef struct {"));
	metagen_push_table(m, m->scratch, s8(""), s8(";"), (s8 *[]){types, fields}, fields_count, 2);
	meta_end_scope(m, s8("} "), kind, s8(";\n"));
}

function void
metagen_push_counted_enum_body(MetaprogramContext *m, s8 kind, s8 prefix, s8 mid, s8 suffix, s8 *ids, iz ids_count)
{
	iz max_id_length = 0;
	for (iz id = 0; id < ids_count; id++)
		max_id_length = MAX(max_id_length, ids[id].len);

	for (iz id = 0; id < ids_count; id++) {
		meta_begin_line(m, prefix, kind, ids[id]);
		meta_pad(m, ' ', 1 + (i32)(max_id_length - ids[id].len));
		meta_push(m, mid);
		meta_push_u64(m, (u64)id);
		meta_end_line(m, suffix);
	}
}

function void
metagen_push_c_enum(MetaprogramContext *m, Arena scratch, s8 kind, s8 *ids, iz ids_count)
{
	s8 kind_full = push_s8_from_parts(&scratch, s8(""), kind, s8("_"));
	meta_begin_scope(m, s8("typedef enum {"));
	metagen_push_counted_enum_body(m, kind_full, s8(""), s8("= "), s8(","), ids, ids_count);
	meta_push_line(m, kind_full, s8("Count,"));
	meta_end_scope(m, s8("} "), kind, s8(";\n"));
}

function void
metagen_push_c_flag_enum(MetaprogramContext *m, Arena scratch, s8 kind, s8 *ids, iz ids_count)
{
	s8 kind_full = push_s8_from_parts(&scratch, s8(""), kind, s8("_"));
	meta_begin_scope(m, s8("typedef enum {"));
	metagen_push_counted_enum_body(m, kind_full, s8(""), s8("= (1 << "), s8("),"), ids, ids_count);
	meta_end_scope(m, s8("} "), kind, s8(";\n"));
}

function void
metagen_push_shader_derivative_vectors(MetaContext *ctx, MetaprogramContext *m, MetaShader *s,
                                       i32 sub_field_count, b32 has_local_flags)
{
	meta_push_line(m, s8("// "), ctx->shader_names.data[s->base_name_id]);
	for (iz perm = 0; perm < s->permutations.count; perm++) {
		MetaShaderPermutation *p = s->permutations.data + perm;
		if (!has_local_flags && sub_field_count == 0) {
			meta_push_line(m, s8("0,"));
		} else {
			meta_begin_line(m, s8("(i32 []){"));
			for (u8 id = 0; id < p->global_flags_count; id++) {
				s8 kind      = metagen_permutation_kind(ctx, p->global_flags[id]);
				s8 variation = metagen_permutation_variation(ctx, p->global_flags[id]);
				if (id != 0) meta_push(m, s8(", "));
				meta_push(m, s8("Beamformer"), kind, s8("_"), variation);
			}

			for (i32 id = p->global_flags_count; id < sub_field_count; id++)
				meta_push(m, s8(", -1"));

			if (has_local_flags) {
				meta_push(m, s8(", 0x"));
				meta_push_u64_hex(m, p->local_flags);
			}
			meta_end_line(m, s8("},"));
		}
	}
}

function void
meta_push_shader_descriptors_table(MetaprogramContext *m, MetaContext *ctx)
{
	Arena scratch_start = m->scratch;
	s8 *columns[5];
	for EachElement(columns, it)
		columns[it] = push_array(&m->scratch, s8, ctx->shaders.count);

	Stream sb = arena_stream(m->scratch);
	for (iz shader = 0; shader < ctx->shaders.count; shader++) {
		MetaShaderDescriptor *sd = ctx->shader_descriptors + shader;
		MetaShader           *s  = ctx->shaders.data + shader;

		stream_append_u64(&sb, (u64)sd->first_match_vector_index);
		stream_append_byte(&sb, ',');
		columns[0][shader] = arena_stream_commit_and_reset(&m->scratch, &sb);

		stream_append_u64(&sb, (u64)sd->one_past_last_match_vector_index);
		stream_append_byte(&sb, ',');
		columns[1][shader] = arena_stream_commit_and_reset(&m->scratch, &sb);

		stream_append_u64(&sb, (u64)sd->sub_field_count);
		stream_append_byte(&sb, ',');
		columns[2][shader] = arena_stream_commit_and_reset(&m->scratch, &sb);

		stream_append_u64(&sb, (u64)sd->sub_field_count + (u64)s->global_enumeration_ids.count);
		stream_append_byte(&sb, ',');
		columns[3][shader] = arena_stream_commit_and_reset(&m->scratch, &sb);

		columns[4][shader] = sd->has_local_flags ? s8("1") : s8 ("0");
	}

	meta_begin_scope(m, s8("read_only global BeamformerShaderDescriptor beamformer_shader_descriptors[] = {"));
	metagen_push_table(m, m->scratch, s8("{"), s8("},"), columns, (u32)ctx->shaders.count, countof(columns));
	meta_end_scope(m, s8("};\n"));

	m->scratch = scratch_start;
}

function void
meta_push_shader_reload_info(MetaprogramContext *m, MetaContext *ctx)
{
	///////////////////////////////
	// NOTE(rnp): reloadable infos
	i32 max_shader_name_length = 0;
	for (iz shader = 0; shader < ctx->base_shaders.count; shader++) {
		if (ctx->base_shaders.data[shader].file.len == 0) continue;
		s8 name = ctx->shader_names.data[ctx->base_shaders.data[shader].shader->base_name_id];
		max_shader_name_length = MAX((i32)name.len, max_shader_name_length);
	}

	meta_begin_scope(m, s8("read_only global BeamformerReloadableShaderInfo beamformer_reloadable_shader_infos[] = {"));
	for (iz shader = 0; shader < ctx->base_shaders.count; shader++) {
		MetaBaseShader *bs = ctx->base_shaders.data + shader;
		MetaShader     *s  = bs->shader;

		if (bs->file.len == 0) continue;

		s8 name = ctx->shader_names.data[s->base_name_id];
		meta_begin_line(m, s8("{BeamformerShaderKind_"), name, s8(", "));
		meta_pad(m, ' ', max_shader_name_length - (i32)name.len);
		meta_push_u64(m, (u64)bs->sub_shaders.count);

		if (bs->sub_shaders.count) {
			meta_push(m, s8(", (i32 []){"));
			for (iz sub_shader = 0; sub_shader < bs->sub_shaders.count; sub_shader++) {
				if (sub_shader != 0) meta_push(m, s8(", "));
				meta_push_u64(m, bs->sub_shaders.data[sub_shader]);
			}
			meta_push(m, s8("}"));
		} else {
			meta_push(m, s8(", 0"));
		}
		meta_end_line(m, s8("},"));
	}
	meta_end_scope(m, s8("};\n"));

	meta_begin_scope(m, s8("read_only global s8 beamformer_reloadable_shader_files[] = {"));
	for (iz shader = 0; shader < ctx->base_shaders.count; shader++) {
		MetaBaseShader *bs = ctx->base_shaders.data + shader;
		if (bs->file.len == 0) continue;
		meta_push_line(m, s8("s8_comp(\""), bs->file, s8("\"),"));
	}
	meta_end_scope(m, s8("};\n"));

	{
		u32 info_index = 0;
		for (iz group = 0; group < ctx->shader_groups.count; group++) {
			MetaShaderGroup *sg = ctx->shader_groups.data + group;
			meta_begin_line(m, s8("read_only global i32 beamformer_reloadable_"));
			for (iz i = 0; i < sg->name.len; i++)
				stream_append_byte(&m->stream, TOLOWER(sg->name.data[i]));
			meta_begin_scope(m, s8("_shader_info_indices[] = {"));

			for (iz shader = 0; shader < sg->shaders.count; shader++) {
				MetaShader *s = ctx->shaders.data + sg->shaders.data[shader];
				/* TODO(rnp): store base shader list in a better format */
				for (iz base_shader = 0; base_shader < ctx->base_shaders.count; base_shader++) {
					MetaBaseShader *bs = ctx->base_shaders.data + base_shader;
					if (bs->file.len && bs->shader == s) {
						meta_indent(m);
						meta_push_u64(m, info_index++);
						meta_end_line(m, s8(","));
						break;
					}
				}
			}
			meta_end_scope(m, s8("};\n"));
		}
	}

	////////////////////////////////////
	// NOTE(rnp): shader header strings
	meta_begin_scope(m, s8("read_only global s8 beamformer_shader_global_header_strings[] = {"));
	for (iz kind = 0; kind < ctx->enumeration_kinds.count; kind++) {
		s8_list *sub_list  = ctx->enumeration_members.data + kind;
		s8 kind_name = push_s8_from_parts(&m->scratch, s8(""), ctx->enumeration_kinds.data[kind], s8("_"));
		meta_push_line(m, s8("s8_comp(\"\""));
		metagen_push_counted_enum_body(m, kind_name, s8("\"#define "), s8(""), s8("\\n\""),
		                               sub_list->data, sub_list->count);
		meta_push_line(m, s8("\"\\n\"),"));
		m->scratch = ctx->scratch;
	}
	meta_end_scope(m, s8("};\n"));

	meta_begin_scope(m, s8("read_only global s8 beamformer_shader_local_header_strings[] = {"));
	for (iz shader = 0; shader < ctx->base_shaders.count; shader++) {
		if (ctx->base_shaders.data[shader].file.len == 0) continue;

		MetaShader *s         = ctx->base_shaders.data[shader].shader;
		s8_list    *flag_list = ctx->flags_for_shader.data + s->flag_list_id;

		if (flag_list->count) {
			meta_push_line(m, s8("s8_comp(\"\""));
			metagen_push_counted_enum_body(m, s8("ShaderFlags_"), s8("\"#define "), s8("(1 << "), s8(")\\n\""),
			                               flag_list->data, flag_list->count);
			meta_push_line(m, s8("\"\\n\"),"));
		} else {
			meta_push_line(m, s8("{0},"));
		}
	}
	meta_end_scope(m, s8("};\n"));

	meta_begin_scope(m, s8("read_only global s8 beamformer_shader_descriptor_header_strings[] = {"));
	for (iz kind = 0; kind < ctx->enumeration_kinds.count; kind++)
		meta_push_line(m, s8("s8_comp(\""), ctx->enumeration_kinds.data[kind], s8("\"),"));
	meta_end_scope(m, s8("};\n"));
}

function void
meta_push_shader_match_helper(MetaprogramContext *m, MetaContext *ctx, MetaShader *s, MetaShaderDescriptor *sd)
{
	s8 name = ctx->shader_names.data[s->base_name_id];
	meta_push_line(m, s8("function iz"));
	meta_begin_line(m, s8("beamformer_shader_"));
	for (iz i = 0; i < name.len; i++)
		stream_append_byte(&m->stream, TOLOWER(name.data[i]));
	meta_push(m, s8("_match("));

	assert(s->global_flag_ids.count < 27);
	for (iz flag = 0; flag < s->global_flag_ids.count; flag++) {
		if (flag != 0) meta_push(m, s8(", "));
		u32 index = s->global_flag_ids.data[flag];
		meta_push(m, s8("Beamformer"), ctx->enumeration_kinds.data[index], s8(" "));
		stream_append_byte(&m->stream, (u8)((iz)'a' + flag));
	}
	if (sd->has_local_flags) {
		if (s->global_flag_ids.count) meta_push(m, s8(", "));
		meta_push(m, s8("i32 flags"));
	}
	meta_end_line(m, s8(")"));

	meta_begin_scope(m, s8("{"));
		meta_begin_line(m, s8("iz result = beamformer_shader_match((i32 []){(i32)"));
		for (iz flag = 0; flag < s->global_flag_ids.count; flag++) {
			if (flag != 0) meta_push(m, s8(", (i32)"));
			stream_append_byte(&m->stream, (u8)((iz)'a' + flag));
		}
		if (sd->has_local_flags) {
			if (s->global_flag_ids.count) meta_push(m, s8(", "));
			meta_push(m, s8("flags"));
		}
		meta_push(m, s8("}, "));
		meta_push_u64(m, (u64)sd->first_match_vector_index);
		meta_push(m, s8(", "));
		meta_push_u64(m, (u64)sd->one_past_last_match_vector_index);
		meta_push(m, s8(", "));
		meta_push_u64(m, (u64)sd->sub_field_count + sd->has_local_flags);
		meta_end_line(m, s8(");"));
		meta_push_line(m, s8("return result;"));
	meta_end_scope(m, s8("}\n"));
}

function b32
metagen_emit_c_code(MetaContext *ctx, Arena arena)
{
	b32 result = 1;

	os_make_directory("generated");
	char *out = "generated/beamformer.meta.c";
	if (!needs_rebuild(out, "beamformer.meta"))
		return result;

	build_log_generate("Core C Code");

	MetaprogramContext meta_program = {.stream = arena_stream(arena), .scratch = ctx->scratch};
	MetaprogramContext *m = &meta_program;

	meta_push_line(m, s8("/* See LICENSE for license details. */\n"));
	meta_push_line(m, s8("// GENERATED CODE\n"));

	/////////////////////////
	// NOTE(rnp): enumarents
	for (iz kind = 0; kind < ctx->enumeration_kinds.count; kind++) {
		s8 enum_name = push_s8_from_parts(&m->scratch, s8(""), s8("Beamformer"), ctx->enumeration_kinds.data[kind]);
		metagen_push_c_enum(m, m->scratch, enum_name, ctx->enumeration_members.data[kind].data,
		                    ctx->enumeration_members.data[kind].count);
		m->scratch = ctx->scratch;
	}

	for (iz shader = 0; shader < ctx->base_shaders.count; shader++) {
		MetaShader *s = ctx->base_shaders.data[shader].shader;
		s8_list flag_list = ctx->flags_for_shader.data[s->flag_list_id];
		if (flag_list.count) {
			s8 enum_name = push_s8_from_parts(&m->scratch, s8(""), s8("BeamformerShader"),
			                                  ctx->shader_names.data[s->base_name_id], s8("Flags"));
			metagen_push_c_flag_enum(m, m->scratch, enum_name, flag_list.data, flag_list.count);
			m->scratch = ctx->scratch;
		}
	}

	{
		s8 kind      = s8("BeamformerShaderKind");
		s8 kind_full = s8("BeamformerShaderKind_");
		meta_begin_scope(m, s8("typedef enum {"));
		metagen_push_counted_enum_body(m, kind_full, s8(""), s8("= "), s8(","),
		                               ctx->shader_names.data, ctx->shader_names.count);
		meta_push_line(m, kind_full, s8("Count,\n"));

		s8 *columns[2];
		columns[0] = push_array(&m->scratch, s8, ctx->shader_groups.count * 3);
		columns[1] = push_array(&m->scratch, s8, ctx->shader_groups.count * 3);

		for (iz group = 0; group < ctx->shader_groups.count; group++) {
			MetaShaderGroup *sg = ctx->shader_groups.data + group;

			s8 first_name = ctx->shader_names.data[ctx->shaders.data[sg->shaders.data[0]].base_name_id];
			s8 last_name  = ctx->shader_names.data[ctx->shaders.data[sg->shaders.data[sg->shaders.count - 1]].base_name_id];

			columns[0][3 * group + 0] = push_s8_from_parts(&m->scratch, s8(""), kind, s8("_"), sg->name, s8("First"));
			columns[1][3 * group + 0] = push_s8_from_parts(&m->scratch, s8(""), s8("= "), kind, s8("_"), first_name);

			columns[0][3 * group + 1] = push_s8_from_parts(&m->scratch, s8(""), kind, s8("_"), sg->name, s8("Last"));
			columns[1][3 * group + 1] = push_s8_from_parts(&m->scratch, s8(""),s8("= "), kind, s8("_"), last_name);

			columns[0][3 * group + 2] = push_s8_from_parts(&m->scratch, s8(""), kind, s8("_"), sg->name, s8("Count"));
			Stream sb = arena_stream(m->scratch);
			stream_append_s8(&sb, s8("= "));
			stream_append_u64(&sb, (u64)sg->shaders.count);
			columns[1][3 * group + 2] = arena_stream_commit(&m->scratch, &sb);
		}
		metagen_push_table(m, m->scratch, s8(""), s8(","), columns, (uz)ctx->shader_groups.count * 3, 2);

		meta_end_scope(m, s8("} "), kind, s8(";\n"));
		m->scratch = ctx->scratch;
	}

	//////////////////////
	// NOTE(rnp): structs
	{
		s8 name    = s8_comp("BeamformerShaderDescriptor");
		s8 types[] = {s8_comp("i32"), s8_comp("i32"), s8_comp("i16"), s8_comp("i16"), s8_comp("b32")};
		s8 names[] = {
			s8_comp("first_match_vector_index"),
			s8_comp("one_past_last_match_vector_index"),
			s8_comp("match_vector_length"),
			s8_comp("header_vector_length"),
			s8_comp("has_local_flags"),
		};
		metagen_push_c_struct(m, name, types, countof(types), names, countof(names));
	}

	{
		s8 name    = s8_comp("BeamformerReloadableShaderInfo");
		s8 types[] = {s8_comp("BeamformerShaderKind"), s8_comp("i32"), s8_comp("i32 *")};
		s8 names[] = {
			s8_comp("kind"),
			s8_comp("sub_shader_descriptor_index_count"),
			s8_comp("sub_shader_descriptor_indices"),
		};
		metagen_push_c_struct(m, name, types, countof(types), names, countof(names));
	}

	///////////////////////////////////////
	// NOTE(rnp): shader descriptor tables
	i32 match_vectors_count = 0;
	meta_begin_scope(m, s8("read_only global i32 *beamformer_shader_match_vectors[] = {"));
	for (iz shader = 0; shader < ctx->shaders.count; shader++) {
		MetaShader           *s  = ctx->shaders.data + shader;
		MetaShaderDescriptor *sd = ctx->shader_descriptors + shader;
		metagen_push_shader_derivative_vectors(ctx, m, s, sd->sub_field_count, sd->has_local_flags);
		match_vectors_count += (i32)s->permutations.count;
	}
	meta_end_scope(m, s8("};"));
	meta_begin_line(m, s8("#define beamformer_match_vectors_count ("));
	meta_push_u64(m, (u64)match_vectors_count);
	meta_end_line(m, s8(")\n"));

	meta_push_shader_descriptors_table(m, ctx);

	/////////////////////////////////
	// NOTE(rnp): shader info tables
	meta_begin_scope(m, s8("read_only global s8 beamformer_shader_names[] = {"));
	metagen_push_table(m, m->scratch, s8("s8_comp(\""), s8("\"),"), &ctx->shader_names.data,
	                   (uz)ctx->shader_names.count, 1);
	meta_end_scope(m, s8("};\n"));

	meta_push_shader_reload_info(m, ctx);

	meta_begin_scope(m, s8("read_only global i32 *beamformer_shader_header_vectors[] = {"));
	for (iz shader = 0; shader < ctx->shaders.count; shader++) {
		MetaShader *s = ctx->shaders.data + shader;

		if (s->global_flag_ids.count) {
			meta_begin_line(m, s8("(i32 []){"));
			for (iz id = 0; id < s->global_flag_ids.count; id++) {
				if (id != 0) meta_push(m, s8(", "));
				meta_push_u64(m, s->global_flag_ids.data[id]);
			}
			for (iz id = 0; id < s->global_enumeration_ids.count; id++) {
				if (id != 0 || s->global_flag_ids.count) meta_push(m, s8(", "));
				meta_push_u64(m, s->global_enumeration_ids.data[id]);
			}
			meta_end_line(m, s8("},"));
		} else {
			meta_push_line(m, s8("0,"));
		}
	}
	meta_end_scope(m, s8("};\n"));

	//////////////////////////////////////
	// NOTE(rnp): shader matching helpers
	meta_push_line(m, s8("function iz"));
	meta_push_line(m, s8("beamformer_shader_match(i32 *match_vector, i32 first_index, i32 one_past_last_index, i32 vector_length)"));
	meta_begin_scope(m, s8("{"));
		meta_push_line(m, s8("iz result = first_index;"));
		meta_push_line(m, s8("i32 best_score = 0;"));
		meta_push_line(m, s8("for (i32 index = first_index; index < one_past_last_index; index++)"));
		meta_begin_scope(m, s8("{"));
			meta_push_line(m, s8("i32 score = 0;"));
			meta_push_line(m, s8("i32 *v = beamformer_shader_match_vectors[index];"));
			meta_begin_scope(m, s8("for (i32 i = 0; i < vector_length; i++) {"));
				meta_begin_scope(m, s8("if (match_vector[i] == v[i]) {"));
					meta_push_line(m, s8("score++;"));
				meta_end_scope(m, s8("}"));
			meta_end_scope(m, s8("}"));
			meta_begin_scope(m, s8("if (best_score < score) {"));
				meta_push_line(m, s8("result     = index;"));
				meta_push_line(m, s8("best_score = score;"));
			meta_end_scope(m, s8("}"));
		meta_end_scope(m, s8("}"));
		meta_push_line(m, s8("return result;"));
	meta_end_scope(m, s8("}\n"));

	for (iz shader = 0; shader < ctx->shaders.count; shader++) {
		MetaShader           *s  = ctx->shaders.data + shader;
		MetaShaderDescriptor *sd = ctx->shader_descriptors + shader;
		if (sd->sub_field_count || sd->has_local_flags)
			meta_push_shader_match_helper(m, ctx, s, sd);
	}

	//fprintf(stderr, "%.*s\n", (i32)m.stream.widx, m.stream.data);

	result = meta_write_and_reset(m, out);

	return result;
}

function b32
metagen_emit_matlab_code(MetaContext *ctx, Arena arena)
{
	b32 result = 1;
	if (!needs_rebuild(OUTPUT("matlab/OGLBeamformerFilterKind.m"), "beamformer_parameters.h"))
		return result;

	build_log_generate("MATLAB Bindings");
	/* TODO(rnp): recreate/clear directory incase these file names change */
	os_make_directory(OUTPUT("matlab"));

	MetaprogramContext meta_program = {.stream = arena_stream(arena), .scratch = ctx->scratch};
	MetaprogramContext *m = &meta_program;

	#define X(name, flag, ...) meta_push_line(m, s8(#name " (" str(flag) ")"));
	meta_begin_matlab_class(m, "OGLBeamformerLiveFeedbackFlags", "int32");
	meta_begin_scope(m, s8("enumeration"));
	BEAMFORMER_LIVE_IMAGING_DIRTY_FLAG_LIST
	result &= meta_end_and_write_matlab(m, OUTPUT("matlab/OGLBeamformerLiveFeedbackFlags.m"));
	#undef X

	#define X(kind, ...) meta_push_matlab_enum_with_value(m, s8(#kind), BeamformerFilterKind_## kind);
	meta_begin_matlab_class(m, "OGLBeamformerFilterKind", "int32");
	meta_begin_scope(m, s8("enumeration"));
	BEAMFORMER_FILTER_KIND_LIST(,)
	result &= meta_end_and_write_matlab(m, OUTPUT("matlab/OGLBeamformerFilterKind.m"));
	#undef X

	os_make_directory(OUTPUT("matlab/+OGLBeamformerFilter"));
	#define X(kind, ...) {OUTPUT("matlab/+OGLBeamformerFilter/" #kind ".m"), s8_comp(#kind),  s8_comp(#__VA_ARGS__)},
	read_only local_persist struct {char *out; s8 class, args;} filter_table[] = {
		BEAMFORMER_FILTER_KIND_LIST(,)
	};
	#undef X

	s8_list members = {0};
	for EachNonZeroEnumValue(BeamformerFilterKind, filter) {
		typeof(*filter_table) *f = filter_table + filter;
		members.count = 0;
		s8_list_from_s8(&members, &m->scratch, f->args);
		meta_begin_scope(m, s8("classdef "), f->class, s8(" < OGLBeamformerFilter.BaseFilter"));

		meta_begin_scope(m, s8("properties"));
		for (iz it = 0; it < members.count; it++)
			meta_push_matlab_property(m, members.data[it], 1, s8("single"));
		meta_end_scope(m, s8("end"));

		meta_begin_scope(m, s8("methods"));
		meta_begin_line(m, s8("function obj = "), f->class, s8("("));
		for (iz it = 0; it < members.count; it++)
			meta_push(m, it > 0 ? s8(", ") : s8(""), members.data[it]);
		meta_end_line(m, s8(")"));

		m->indentation_level++;
		for (iz it = 0; it < members.count; it++)
			meta_push_line(m, s8("obj."), members.data[it], s8(" = "), members.data[it], s8(";"));
		result &= meta_end_and_write_matlab(m, f->out);
	}
	m->scratch = ctx->scratch;

	meta_begin_matlab_class(m, "BaseFilter");
	meta_begin_scope(m, s8("methods"));
	meta_begin_scope(m, s8("function out = Flatten(obj)"));
	meta_push_line(m, s8("fields = struct2cell(struct(obj));"));
	meta_push_line(m, s8("out    = zeros(1, numel(fields));"));
	meta_begin_scope(m, s8("for i = 1:numel(fields)"));
	meta_push_line(m, s8("out(i) = fields{i};"));
	result &= meta_end_and_write_matlab(m, OUTPUT("matlab/+OGLBeamformerFilter/BaseFilter.m"));

	#define X(name, __t, __s, kind, elements, ...) meta_push_matlab_property(m, s8(#name), (u64)elements, s8(#kind));
	meta_begin_matlab_class(m, "OGLBeamformerParameters");
	meta_begin_scope(m, s8("properties"));
	BEAMFORMER_PARAMS_HEAD
	BEAMFORMER_UI_PARAMS
	result &= meta_end_and_write_matlab(m, OUTPUT("matlab/OGLBeamformerParameters.m"));

	meta_begin_matlab_class(m, "OGLBeamformerParametersHead");
	meta_begin_scope(m, s8("properties"));
	BEAMFORMER_PARAMS_HEAD
	result &= meta_end_and_write_matlab(m, OUTPUT("matlab/OGLBeamformerParametersHead.m"));

	meta_begin_matlab_class(m, "OGLBeamformerParametersUI");
	meta_begin_scope(m, s8("properties"));
	BEAMFORMER_UI_PARAMS
	result &= meta_end_and_write_matlab(m, OUTPUT("matlab/OGLBeamformerParametersUI.m"));

	meta_begin_matlab_class(m, "OGLBeamformerSimpleParameters");
	meta_begin_scope(m, s8("properties"));
	BEAMFORMER_PARAMS_HEAD
	BEAMFORMER_UI_PARAMS
	BEAMFORMER_SIMPLE_PARAMS
	result &= meta_end_and_write_matlab(m, OUTPUT("matlab/OGLBeamformerSimpleParameters.m"));
	#undef X

	#define X(name, __t, __s, elements, ...) meta_push_matlab_property(m, s8(#name), elements, s8(""));
	meta_begin_matlab_class(m, "OGLBeamformerLiveImagingParameters");
	meta_begin_scope(m, s8("properties"));
	BEAMFORMER_LIVE_IMAGING_PARAMETERS_LIST
	result &= meta_end_and_write_matlab(m, OUTPUT("matlab/OGLBeamformerLiveImagingParameters.m"));
	#undef X

	meta_begin_matlab_class(m, "OGLBeamformerShaderStage", "int32");
	meta_begin_scope(m, s8("enumeration"));
	{
		iz index = -1;
		for (iz group = 0; group < ctx->shader_groups.count; group++) {
			if (s8_equal(ctx->shader_groups.data[group].name, s8("Compute"))) {
				index = group;
				break;
			}
		}
		if (index != -1) {
			MetaShaderGroup *sg = ctx->shader_groups.data + index;
			/* TODO(rnp): this assumes that the shaders are sequential */
			s8 *names = ctx->shader_names.data + ctx->shaders.data[0].base_name_id;
			metagen_push_counted_enum_body(m, s8(""), s8(""), s8("("), s8(")"), names, sg->shaders.count);
		} else {
			build_log_failure("failed to find Compute shader group in meta info\n");
		}
		result &= index != -1;
	}
	result &= meta_end_and_write_matlab(m, OUTPUT("matlab/OGLBeamformerShaderStage.m"));

	for (iz kind = 0; kind < ctx->enumeration_kinds.count; kind++) {
		Arena scratch = ctx->scratch;
		s8 name   = ctx->enumeration_kinds.data[kind];
		s8 output = push_s8_from_parts(&scratch, s8(""), s8(OUTPUT("matlab/OGLBeamformer")), name, s8(".m"));
		s8_list *kinds = ctx->enumeration_members.data + kind;
		meta_begin_scope(m, s8("classdef OGLBeamformer"), name, s8(" < int32"));
		meta_begin_scope(m, s8("enumeration"));
		s8 prefix = s8("");
		if (kinds->count > 0 && ISDIGIT(kinds->data[0].data[0])) prefix = s8("m");
		metagen_push_counted_enum_body(m, s8(""), prefix, s8("("), s8(")"), kinds->data, kinds->count);
		result &= meta_end_and_write_matlab(m, (c8 *)output.data);
	}

	return result;
}

function b32
metagen_emit_helper_library_header(MetaContext *ctx, Arena arena)
{
	b32 result = 1;
	char *out = OUTPUT("ogl_beamformer_lib.h");
	if (!needs_rebuild(out, "helpers/ogl_beamformer_lib_base.h", "beamformer.meta"))
		return result;

	build_log_generate("Helper Library Header");

	s8 parameters_header = os_read_whole_file(&arena, "beamformer_parameters.h");
	s8 base_header       = os_read_whole_file(&arena, "helpers/ogl_beamformer_lib_base.h");

	MetaprogramContext meta_program = {.stream = arena_stream(arena), .scratch = ctx->scratch};
	MetaprogramContext *m = &meta_program;

	meta_push_line(m, s8("/* See LICENSE for license details. */\n"));
	meta_push_line(m, s8("// GENERATED CODE\n"));

	{
		iz index = meta_lookup_string_slow(&ctx->enumeration_kinds, s8("DataKind"));
		if (index != -1) {
			s8 enum_name = push_s8_from_parts(&m->scratch, s8(""), s8("Beamformer"), ctx->enumeration_kinds.data[index]);
			metagen_push_c_enum(m, m->scratch, enum_name, ctx->enumeration_members.data[index].data,
			                    ctx->enumeration_members.data[index].count);
			m->scratch = ctx->scratch;
		} else {
			build_log_failure("failed to find DataKind in meta info\n");
		}
	}

	{
		iz index = -1;
		for (iz group = 0; group < ctx->shader_groups.count; group++) {
			if (s8_equal(ctx->shader_groups.data[group].name, s8("Compute"))) {
				index = group;
				break;
			}
		}
		if (index != -1) {
			MetaShaderGroup *sg = ctx->shader_groups.data + index;
			meta_begin_line(m, s8("#define BeamformerShaderKind_ComputeCount ("));
			meta_push_u64(m, (u64)sg->shaders.count);
			meta_end_line(m, s8(")\n"));
		} else {
			build_log_failure("failed to find Compute shader group in meta info\n");
		}
	}

	meta_push(m, parameters_header, base_header);
	result &= meta_write_and_reset(m, out);

	return result;
}

function MetaContext *
metagen_load_context(Arena *arena)
{
	if (setjmp(compiler_jmp_buf)) {
		/* NOTE(rnp): compiler error */
		return 0;
	}

	MetaContext *ctx = push_struct(arena, MetaContext);
	ctx->scratch     = sub_arena(arena, MB(1), 16);
	ctx->arena       = arena;

	MetaContext *result = ctx;

	Arena scratch = ctx->scratch;
	MetaEntryStack entries = meta_entry_stack_from_file(ctx->arena, scratch, "beamformer.meta");

	i32 stack_items[32];
	struct { i32 *data; iz capacity; iz count; } stack = {stack_items, countof(stack_items), 0};

	MetaShaderGroup *current_shader_group = 0;
	for (iz i = 0; i < entries.count; i++) {
		MetaEntry *e = entries.data + i;
		//if (e->kind == MetaEntryKind_EndScope)   depth--;
		//meta_entry_print(e, depth, -1);
		//if (e->kind == MetaEntryKind_BeginScope) depth++;
		//continue;

		switch (e->kind) {
		case MetaEntryKind_BeginScope:{ *da_push(&scratch, &stack) = (i32)(i - 1); }break;
		case MetaEntryKind_EndScope:{
			i32 index = stack.data[--stack.count];
			MetaEntry *ended = entries.data + index;
			switch (ended->kind) {
			case MetaEntryKind_ShaderGroup:{ current_shader_group = 0; }break;
			default:{}break;
			}
		}break;
		case MetaEntryKind_Enumeration:{
			meta_entry_argument_expected(e, s8("kind"), s8("[id ...]"));
			s8 kind = meta_entry_argument_expect(e, 0, MetaEntryArgumentKind_String).string;
			MetaEntryArgument ids = meta_entry_argument_expect(e, 1, MetaEntryArgumentKind_Array);
			for (u32 id = 0; id < ids.count; id++)
				meta_commit_enumeration(ctx, kind, ids.strings[id]);
		}break;
		case MetaEntryKind_ShaderGroup:{
			MetaShaderGroup *sg = da_push(ctx->arena, &ctx->shader_groups);
			sg->name = e->name;
			current_shader_group = sg;
		}break;
		case MetaEntryKind_Shader:{
			if (!current_shader_group) goto error;
			i += meta_pack_shader(ctx, current_shader_group, scratch, e, entries.count - i);
		}break;

		error:
		default:
		{
			meta_entry_error(e, "invalid @%s() in global scope\n", meta_entry_kind_strings[e->kind]);
		}break;
		}
	}

	ctx->shader_descriptors = push_array(ctx->arena, MetaShaderDescriptor, ctx->shaders.count);
	{
		i32 match_vectors_count = 0;
		for (iz shader = 0; shader < ctx->shaders.count; shader++) {
			MetaShader           *s  = ctx->shaders.data + shader;
			MetaShaderDescriptor *sd = ctx->shader_descriptors + shader;

			sd->has_local_flags = ctx->flags_for_shader.data[s->flag_list_id].count > 0;
			sd->sub_field_count = (i32)s->global_flag_ids.count;
			sd->first_match_vector_index = match_vectors_count;
			match_vectors_count += (i32)s->permutations.count;
			sd->one_past_last_match_vector_index = match_vectors_count;
		}
	}

	result->arena = 0;
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

	os_make_directory(OUTDIR);

	MetaContext *meta = metagen_load_context(&arena);
	if (!meta) return 1;

	result &= metagen_emit_c_code(meta, arena);
	result &= metagen_emit_helper_library_header(meta, arena);
	result &= metagen_emit_matlab_code(meta, arena);

	Options options = parse_options(argc, argv);

	CommandList c = cmd_base(&arena, &options);
	if (!check_build_raylib(arena, c, options.debug)) return 1;

	/////////////////////////////////////
	// extra flags (unusable for raylib)
	cmd_append(&arena, &c, EXTRA_FLAGS);

	/////////////////
	// helpers/tests
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
