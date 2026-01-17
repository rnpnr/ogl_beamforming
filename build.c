/* See LICENSE for license details. */
/* NOTE: inspired by nob: https://github.com/tsoding/nob.h */

/* TODO(rnp):
 * [ ]: refactor: merge pack_table and bake_parameters
 * [ ]: refactor: allow @Expand to come before the table definition
 * [ ]: cross compile/override baked compiler
 * [ ]: msvc build doesn't detect out of date files correctly
 * [ ]: seperate dwarf debug info
 */

#include "util.h"

#include <stdarg.h>
#include <setjmp.h>
#include <stdio.h>

#define BeamformerShaderKind_ComputeCount (1)
#include "beamformer_parameters.h"

global char *g_argv0;

#define OUTDIR    "out"
#define OUTPUT(s) OUTDIR OS_PATH_SEPARATOR s

#if COMPILER_MSVC
  #define COMMON_CFLAGS    "-std:c11"
  #define COMMON_FLAGS     "-nologo", "-Fo:" OUTDIR "\\", "-Z7", "-Zo"
  #define DEBUG_FLAGS      "-Od", "-D_DEBUG"
  #define OPTIMIZED_FLAGS  "-O2"
  #define EXTRA_FLAGS      ""
#else
  #define COMMON_CFLAGS    "-std=c11"
  #define COMMON_FLAGS     "-pipe", "-Wall"
  #define DEBUG_FLAGS      "-O0", "-D_DEBUG", "-Wno-unused-function"
  #define OPTIMIZED_FLAGS  "-O3"
  #define EXTRA_FLAGS_BASE "-Werror", "-Wextra", "-Wshadow", "-Wno-unused-parameter", \
                           "-Wno-error=unused-function", "-fno-builtin"
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

#define BEAMFORMER_IMPORT function

#if OS_LINUX

  #include <dirent.h>
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

  #include <string.h>

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
  #define COMPILER     "clang"
  #define CPP_COMPILER "clang++"
  #define PREPROCESSOR "clang", "-E", "-P"
#elif COMPILER_MSVC
  #define COMPILER     "cl"
  #define CPP_COMPILER "cl"
  #define PREPROCESSOR "cl", "/EP"
#else
  #define COMPILER     "cc"
  #define CPP_COMPILER "c++"
  #define PREPROCESSOR "cc", "-E", "-P"
#endif

#if COMPILER_MSVC
  #define LINK_LIB(name)             name ".lib"
  #define OBJECT(name)               name ".obj"
  #define OUTPUT_DLL(name)           "/LD", "/Fe:", name
  #define OUTPUT_LIB(name)           "/out:" OUTPUT(name)
  #define OUTPUT_EXE(name)           "/Fe:", name
  #define COMPILER_OUTPUT            "/Fo:"
  #define STATIC_LIBRARY_BEGIN(name) "lib", "/nologo", name
#else
  #define LINK_LIB(name)             "-l" name
  #define OBJECT(name)               name ".o"
  #define OUTPUT_DLL(name)           "-fPIC", "-shared", "-o", name
  #define OUTPUT_LIB(name)           OUTPUT(name)
  #define OUTPUT_EXE(name)           "-o", name
  #define COMPILER_OUTPUT            "-o"
  #define STATIC_LIBRARY_BEGIN(name) "ar", "rc", name
#endif

#define shift(list, count) ((count)--, *(list)++)

#define cmd_append_count da_append_count
#define cmd_append(a, s, ...) da_append_count(a, s, ((char *[]){__VA_ARGS__}), \
                                              (iz)(sizeof((char *[]){__VA_ARGS__}) / sizeof(char *)))

DA_STRUCT(char *, Command);

typedef struct {
	b32   bake_shaders;
	b32   debug;
	b32   generic;
	b32   sanitize;
	b32   tests;
	b32   time;
} Config;
global Config config;

read_only global s8 c_file_header = s8_comp(""
	"/* See LICENSE for license details. */\n\n"
	"// GENERATED CODE\n\n"
);

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
#define build_log_error(...)    build_log(BuildLogKind_Error,    ##__VA_ARGS__)
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

function s8
read_entire_file(const char *file, Arena *arena)
{
	s8 result  = {0};
	result.len = os_read_entire_file(file, arena->beg, arena_capacity(arena, u8));
	if (result.len) result.data = arena_commit(arena, result.len);
	return result;
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

function char *
temp_sprintf(char *format, ...)
{
	local_persist char buffer[4096];
	va_list ap;
	va_start(ap, format);
	vsnprintf(buffer, countof(buffer), format, ap);
	va_end(ap);
	return buffer;
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

#define os_remove_directory(f) os_remove_directory_(AT_FDCWD, (f))
function b32
os_remove_directory_(i32 base_fd, char *name)
{
	/* POSix sucks */
	#ifndef DT_DIR
	enum {DT_DIR = 4, DT_REG = 8, DT_LNK = 10};
	#endif

	i32 dir_fd = openat(base_fd, name, O_DIRECTORY);
	b32 result = dir_fd != -1 || errno == ENOTDIR || errno == ENOENT;
	DIR *dir;
	if (dir_fd != -1 && (dir = fdopendir(dir_fd))) {
		struct dirent *dp;
		while ((dp = readdir(dir))) {
			switch (dp->d_type) {
			case DT_LNK:
			case DT_REG:
			{
				unlinkat(dir_fd, dp->d_name, 0);
			}break;
			case DT_DIR:{
				s8 dir_name = c_str_to_s8(dp->d_name);
				if (!s8_equal(s8("."), dir_name) && !s8_equal(s8(".."), dir_name))
					os_remove_directory_(dir_fd, dp->d_name);
			}break;
			default:{
				build_log_warning("\"%s\": unknown directory entry kind: %d", dp->d_name, dp->d_type);
			}break;
			}
		}

		closedir(dir);
		result = unlinkat(base_fd, name, AT_REMOVEDIR) == 0;
	}
	return result;
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

	FILE_ATTRIBUTE_DIRECTORY  = 0x10,

	ERROR_FILE_NOT_FOUND = 0x02,
	ERROR_PATH_NOT_FOUND = 0x03,
};

#pragma pack(push, 1)
typedef struct {
  u32 file_attributes;
  u64 creation_time;
  u64 last_access_time;
  u64 last_write_time;
  u64 file_size;
  u64 reserved;
  c8  file_name[260];
  c8  alternate_file_name[14];
  u32 file_type;
  u32 creator_type;
  u16 finder_flag;
} w32_find_data;
#pragma pack(pop)

W32(b32)  CreateDirectoryA(c8 *, void *);
W32(b32)  CreateProcessA(u8 *, u8 *, iptr, iptr, b32, u32, iptr, u8 *, iptr, iptr);
W32(b32)  FindClose(iptr);
W32(iptr) FindFirstFileA(c8 *, w32_find_data *);
W32(b32)  FindNextFileA(iptr, w32_find_data *);
W32(b32)  GetExitCodeProcess(iptr, u32 *);
W32(b32)  GetFileTime(iptr, iptr, iptr, iptr);
W32(b32)  MoveFileExA(c8 *, c8 *, u32);
W32(b32)  RemoveDirectoryA(c8 *);

function void
os_make_directory(char *name)
{
	CreateDirectoryA(name, 0);
}

function b32
os_remove_directory(char *name)
{
	w32_find_data find_data[1];
	char *search = temp_sprintf(".\\%s\\*", name);
	iptr  handle = FindFirstFileA(search, find_data);
	b32   result = 1;
	if (handle != INVALID_FILE) {
		do {
			s8 file_name = c_str_to_s8(find_data->file_name);
			if (!s8_equal(s8("."), file_name) && !s8_equal(s8(".."), file_name)) {
				char *full_path = temp_sprintf("%s" OS_PATH_SEPARATOR "%s", name, find_data->file_name);
				if (find_data->file_attributes & FILE_ATTRIBUTE_DIRECTORY) {
					char *wow_w32_is_even_worse_than_POSix = strdup(full_path);
					os_remove_directory(wow_w32_is_even_worse_than_POSix);
					free(wow_w32_is_even_worse_than_POSix);
				} else {
					DeleteFileA(full_path);
				}
			}
		} while (FindNextFileA(handle, find_data));
		FindClose(handle);
	} else {
		i32 error = GetLastError();
		result = error == ERROR_FILE_NOT_FOUND || error == ERROR_PATH_NOT_FOUND;
	}
	RemoveDirectoryA(name);
	return result;
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
		union { struct { u32 low, high; }; u64 U64; } w32_filetime;
		GetFileTime(h, 0, 0, (iptr)&w32_filetime);
		result = w32_filetime.U64;
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

function b32
use_sanitization(void)
{
	return config.sanitize && !is_msvc && !(is_w32 && is_gcc);
}

function void
cmd_base(Arena *a, CommandList *c, b32 cpp)
{
	Config *o = &config;

	cmd_append(a, c, cpp ? CPP_COMPILER : COMPILER);

	if (!is_msvc) {
		/* TODO(rnp): support cross compiling with clang */
		if (!o->generic)     cmd_append(a, c, "-march=native");
		else if (is_amd64)   cmd_append(a, c, "-march=x86-64-v3", "-msse4.1");
		else if (is_aarch64) cmd_append(a, c, "-march=armv8");
	}

	if (!cpp) cmd_append(a, c, COMMON_CFLAGS);
	cmd_append(a, c, COMMON_FLAGS);
	if (o->debug) cmd_append(a, c, DEBUG_FLAGS);
	else          cmd_append(a, c, OPTIMIZED_FLAGS);

	/* NOTE: ancient gcc bug: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=80454 */
	if (is_gcc) cmd_append(a, c, "-Wno-missing-braces");

	if (!is_msvc) cmd_append(a, c, "-fms-extensions");

	if (o->debug && is_unix) cmd_append(a, c, "-gdwarf-4");

	/* NOTE(rnp): need to avoid w32-gcc for ci */
	b32 sanitize = use_sanitization();
	if (sanitize) cmd_append(a, c, "-fsanitize=address,undefined");
	if (!sanitize && o->sanitize) build_log_warning("santizers not supported with this compiler");
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

		CommandList c = {0};
		cmd_base(&arena, &c, 0);
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

function void
usage(char *argv0)
{
	printf("%s [--bake-shaders] [--debug] [--sanitize] [--time]\n"
	       "    --debug:       dynamically link and build with debug symbols\n"
	       "    --generic:     compile for a generic target (x86-64-v3 or armv8 with NEON)\n"
	       "    --sanitize:    build with ASAN and UBSAN\n"
	       "    --tests:       also build programs in tests/\n"
	       "    --time:        print build time\n"
	       , argv0);
	os_exit(0);
}

function void
parse_config(i32 argc, char *argv[])
{
	char *argv0 = shift(argv, argc);
	while (argc > 0) {
		char *arg = shift(argv, argc);
		s8 str    = c_str_to_s8(arg);
		if (s8_equal(str, s8("--bake-shaders"))) {
			config.bake_shaders = 1;
		} else if (s8_equal(str, s8("--debug"))) {
			config.debug = 1;
		} else if (s8_equal(str, s8("--generic"))) {
			config.generic = 1;
		} else if (s8_equal(str, s8("--sanitize"))) {
			config.sanitize = 1;
		} else if (s8_equal(str, s8("--tests"))) {
			config.tests = 1;
		} else if (s8_equal(str, s8("--time"))) {
			config.time = 1;
		} else {
			usage(argv0);
		}
	}
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
build_raylib(Arena a)
{
	b32 result = 1, shared = config.debug;
	char *libraylib = shared ? OS_SHARED_LINK_LIB("raylib") : OUTPUT_LIB(OS_STATIC_LIB("raylib"));
	if (needs_rebuild(libraylib, "external/include/rlgl.h", "external/raylib")) {
		git_submodule_update(a, "external/raylib");
		os_copy_file("external/raylib/src/rlgl.h", "external/include/rlgl.h");

		CommandList cc = {0};
		cmd_base(&a, &cc, 0);
		if (is_unix) cmd_append(&a, &cc, "-D_GLFW_X11");
		cmd_append(&a, &cc, "-DPLATFORM_DESKTOP_GLFW");
		if (!is_msvc) cmd_append(&a, &cc, "-Wno-unused-but-set-variable");
		cmd_append(&a, &cc, "-Iexternal/include", "-Iexternal/raylib/src", "-Iexternal/raylib/src/external/glfw/include");
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
build_glslang(Arena a)
{
	b32 result = 1;
	char *lib = OUTPUT_LIB(OS_STATIC_LIB("glslang"));
	if (needs_rebuild(lib, "external/glslang")) {
		git_submodule_update(a, "external/glslang");
		os_copy_file("external/glslang/glslang/Include/glslang_c_interface.h", "external/include/glslang_c_interface.h");

		CommandList cc = {0};
		cmd_base(&a, &cc, 1);
		cmd_append(&a, &cc, "-std=c++17", "-fno-rtti", "-fno-exceptions", "-Wno-unused-but-set-variable");
		cmd_append(&a, &cc, "-Iexternal/glslang_local", "-Iexternal/glslang");

		#if OS_WINDOWS
		  #define GLSLANG_SOURCES_OS X(ossource, "glslang/glslang/OSDependent/Windows/")
		#else
		  #define GLSLANG_SOURCES_OS
		#endif

		#define GLSLANG_SOURCES_COMMON \
			X(glslang,           "glslang_local/") \
			X(spirv_c_interface, "glslang/SPIRV/CInterface/") \

		#define GLSLANG_SOURCES \
			GLSLANG_SOURCES_COMMON \
			GLSLANG_SOURCES_OS \

		#define X(name, extra) "external/" extra #name ".cpp",
		char *srcs[] = {GLSLANG_SOURCES};
		#undef X
		#define X(name, ...) OUTPUT(OBJECT(#name)),
		char *outs[] = {GLSLANG_SOURCES};
		#undef X

		result = build_static_library(a, cc, lib, srcs, outs, countof(srcs));
	}
	return result;
}

function b32
build_helper_library(Arena arena)
{
	CommandList cc = {0};
	cmd_base(&arena, &cc, 0);
	cmd_append(&arena, &cc, EXTRA_FLAGS);

	/////////////
	// library
	char *library = OUTPUT(OS_SHARED_LIB("ogl_beamformer_lib"));
	char *libs[]  = {LINK_LIB("Synchronization")};
	iz libs_count = is_w32 ? countof(libs) : 0;

	if (!is_msvc) cmd_append(&arena, &cc, "-Wno-unused-function");
	b32 result = build_shared_library(arena, cc, "ogl_beamformer_lib", library,
	                                  libs, libs_count, (char *[]){"lib/ogl_beamformer_lib.c"}, 1);
	return result;
}

function void
cmd_beamformer_base(Arena *a, CommandList *c)
{
	cmd_base(a, c, 0);
	cmd_append(a, c, "-Iexternal/include");
	cmd_append(a, c, EXTRA_FLAGS);
	cmd_append(a, c, config.bake_shaders? "-DBakeShaders=1" : "-DBakeShaders=0");
	if (config.debug) cmd_append(a, c, "-DBEAMFORMER_DEBUG", "-DBEAMFORMER_RENDERDOC_HOOKS");

	/* NOTE(rnp): impossible to autodetect on GCC versions < 14 (ci has 13) */
	cmd_append(a, c, use_sanitization() ? "-DASAN_ACTIVE=1" : "-DASAN_ACTIVE=0");
}

function b32
build_beamformer_main(Arena arena)
{
	CommandList c = {0};
	cmd_beamformer_base(&arena, &c);

	cmd_append(&arena, &c, OS_MAIN, OUTPUT_EXE("ogl"));
	cmd_pdb(&arena, &c, "ogl");
	if (config.debug) {
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

	return run_synchronous(arena, &c);
}

function b32
build_beamformer_as_library(Arena arena)
{
	CommandList cc = {0};
	cmd_beamformer_base(&arena, &cc);

	if (is_msvc) {
		build_static_library_from_objects(arena, OUTPUT_LIB(OS_STATIC_LIB("main")),
		                                  arg_list(char *, "/def", "/name:ogl.exe"),
		                                  arg_list(char *, OUTPUT(OBJECT("main_w32"))));
	}

	char *library = OS_SHARED_LIB("beamformer");
	char *libs[]  = {!is_msvc? "-L." : "", LINK_LIB("raylib"), LINK_LIB("gdi32"),
	                 LINK_LIB("shell32"), LINK_LIB("user32"), LINK_LIB("opengl32"),
	                 LINK_LIB("winmm"), LINK_LIB("Synchronization"), OUTPUT("main.lib")};
	iz libs_count = is_w32 ? countof(libs) : 0;
	cmd_append(&arena, &cc, "-D_BEAMFORMER_DLL");
	b32 result = build_shared_library(arena, cc, "beamformer", library,
	                                  libs, libs_count, arg_list(char *, "beamformer_core.c"));
	return result;
}

function b32
build_tests(Arena arena)
{
	CommandList cc = {0};
	cmd_base(&arena, &cc, 0);
	cmd_append(&arena, &cc, EXTRA_FLAGS);

	#define TEST_PROGRAMS \
		X("decode", W32_DECL(LINK_LIB("Synchronization"))) \
		X("throughput", LINK_LIB("zstd"), W32_DECL(LINK_LIB("Synchronization")))

	os_make_directory(OUTPUT("tests"));
	if (!is_msvc) cmd_append(&arena, &cc, "-Wno-unused-function");
	cmd_append(&arena, &cc, "-I.", "-Ilib");

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

#define meta_pad(m, b, n)                stream_pad(&(m)->stream, (b), (n))
#define meta_indent(m)                   meta_pad((m), '\t', (m)->indentation_level)
#define meta_begin_line(m, ...)     do { meta_indent(m); meta_push(m, __VA_ARGS__);                } while(0)
#define meta_end_line(m, ...)                            meta_push(m, ##__VA_ARGS__, s8("\n"))
#define meta_push_line(m, ...)      do { meta_indent(m); meta_push(m, ##__VA_ARGS__, s8("\n"));    } while(0)
#define meta_begin_scope(m, ...)    do { meta_push_line(m, __VA_ARGS__); (m)->indentation_level++; } while(0)
#define meta_end_scope(m, ...)      do { (m)->indentation_level--; meta_push_line(m, __VA_ARGS__); } while(0)
#define meta_push_u64(m, n)              stream_append_u64(&(m)->stream, (n))
#define meta_push_i64(m, n)              stream_append_i64(&(m)->stream, (n))
#define meta_push_u64_hex(m, n)          stream_append_hex_u64(&(m)->stream, (n))
#define meta_push_u64_hex_width(m, n, w) stream_append_hex_u64_width(&(m)->stream, (n), (w))

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

function b32
meta_end_and_write_matlab(MetaprogramContext *m, char *path)
{
	while (m->indentation_level > 0) meta_end_scope(m, s8("end"));
	b32 result = meta_write_and_reset(m, path);
	return result;
}

#define META_ENTRY_KIND_LIST \
	X(Invalid)      \
	X(Array)        \
	X(Bake)         \
	X(BakeInt)      \
	X(BakeFloat)    \
	X(BeginScope)   \
	X(Emit)         \
	X(Embed)        \
	X(EndScope)     \
	X(Enumeration)  \
	X(Expand)       \
	X(Flags)        \
	X(MUnion)       \
	X(String)       \
	X(Shader)       \
	X(ShaderGroup)  \
	X(SubShader)    \
	X(Table)

typedef enum {
	#define X(k, ...) MetaEntryKind_## k,
	META_ENTRY_KIND_LIST
	#undef X
	MetaEntryKind_Count,
} MetaEntryKind;

#define X(k, ...) #k,
read_only global char *meta_entry_kind_strings[] = {META_ENTRY_KIND_LIST};
#undef X

#define META_EMIT_LANG_LIST \
	X(C)        \
	X(CLibrary) \
	X(MATLAB)

typedef enum {
	#define X(k, ...) MetaEmitLang_## k,
	META_EMIT_LANG_LIST
	#undef X
	MetaEmitLang_Count,
} MetaEmitLang;

#define META_KIND_LIST \
	X(M4,  m4,  float,    single, 64, 16) \
	X(SV4, iv4, int32_t,  int32,  16,  4) \
	X(UV4, uv4, uint32_t, uint32, 16,  4) \
	X(UV2, uv2, uint32_t, uint32,  8,  2) \
	X(V3,  v3,  float,    single, 12,  3) \
	X(V2,  v2,  float,    single,  8,  2) \
	X(F32, f32, float,    single,  4,  1) \
	X(S32, i32, int32_t,  int32,   4,  1) \
	X(S16, i16, int16_t,  int16,   2,  1) \
	X(S8,  i8,  int8_t,   int8,    1,  1) \
	X(U32, u32, uint32_t, uint32,  4,  1) \
	X(U16, u16, uint16_t, uint16,  2,  1) \
	X(U8,  u8,  uint8_t,  uint8,   1,  1)

typedef enum {
	#define X(k, ...) MetaKind_## k,
	META_KIND_LIST
	#undef X
	MetaKind_Count,
} MetaKind;

read_only global u8 meta_kind_byte_sizes[] = {
	#define X(_k, _c, _b, _m, bytes, ...) bytes,
	META_KIND_LIST
	#undef X
};

read_only global u8 meta_kind_elements[] = {
	#define X(_k, _c, _b, _m, _by, elements, ...) elements,
	META_KIND_LIST
	#undef X
};

read_only global s8 meta_kind_meta_types[] = {
	#define X(k, ...) s8_comp(#k),
	META_KIND_LIST
	#undef X
};

read_only global s8 meta_kind_matlab_types[] = {
	#define X(_k, _c, _b, m, ...) s8_comp(#m),
	META_KIND_LIST
	#undef X
};

read_only global s8 meta_kind_base_c_types[] = {
	#define X(_k, _c, base, ...) s8_comp(#base),
	META_KIND_LIST
	#undef X
};

read_only global s8 meta_kind_c_types[] = {
	#define X(_k, c, ...) s8_comp(#c),
	META_KIND_LIST
	#undef X
};

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
	X('`', RawString)  \
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
	meta_entry_print((e), 2 * (column), 0); \
	meta_error(); \
} while(0)

#define meta_entry_pair_error(e, prefix, base_kind) \
	meta_entry_error(e, prefix"@%s() in @%s()\n", \
	                 meta_entry_kind_strings[(e)->kind], \
	                 meta_entry_kind_strings[(base_kind)])

#define meta_entry_nesting_error(e, base_kind) meta_entry_pair_error(e, "invalid nesting: ", base_kind)

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
meta_entry_print(MetaEntry *e, i32 indent, i32 caret)
{
	char *kind = meta_entry_kind_strings[e->kind];
	if (e->kind == MetaEntryKind_BeginScope) kind = "{";
	if (e->kind == MetaEntryKind_EndScope)   kind = "}";

	fprintf(stderr, "%*s@%s", indent, "", kind);

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

	if (caret >= 0) fprintf(stderr, "\n%*s^", indent + caret, "");

	fprintf(stderr, "\n");
}

function iz
meta_lookup_string_slow(s8 *strings, iz string_count, s8 s)
{
	// TODO(rnp): obviously this is slow
	iz result = -1;
	for (iz i = 0; i < string_count; i++) {
		if (s8_equal(s, strings[i])) {
			result = i;
			break;
		}
	}
	return result;
}

function MetaEntryKind
meta_entry_kind_from_string(s8 s)
{
	#define X(k, ...) s8_comp(#k),
	read_only local_persist s8 kinds[] = {META_ENTRY_KIND_LIST};
	#undef X
	MetaEntryKind result = MetaEntryKind_Invalid;
	iz id = meta_lookup_string_slow(kinds + 1, countof(kinds) - 1, s);
	if (id > 0) result = (MetaEntryKind)(id + 1);
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
			comment = ((s + 1) != end && s[1] == '/');
			if (comment) s++;
		} /* FALLTHROUGH */
		default:{done = !comment;}break;
		}
		if (!done) s++;
	}
	p->p.s.data = s;
	p->p.s.len  = end - s;
}

function s8
meta_parser_extract_raw_string(MetaParser *p)
{
	s8 result = {.data = p->p.s.data};
	for (; result.len < p->p.s.len; result.len++) {
		u8 byte = p->p.s.data[result.len];
		p->p.location.column++;
		if (byte == '`') {
			break;
		} else if (byte == '\n') {
			p->p.location.column = 0;
			p->p.location.line++;
		}
	}
	p->p.s.data += (result.len + 1);
	p->p.s.len  -= (result.len + 1);
	return result;
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
	if (t >= 0 && t < countof(names))  result = names[t];
	if (t == MetaParseToken_String)    result = p->u.string;
	if (t == MetaParseToken_RawString) result = (s8){.data = p->u.string.data - 1, .len = p->u.string.len + 1};
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

		if (result != MetaParseToken_RawString) meta_parser_trim(p);
		switch (result) {
		case MetaParseToken_RawString:{ p->u.string = meta_parser_extract_raw_string(p); }break;
		case MetaParseToken_String:{    p->u.string = meta_parser_extract_string(p);     }break;

		/* NOTE(rnp): '{' and '}' are shorthand for @BeginScope and @EndScope */
		case MetaParseToken_BeginScope:{ p->u.kind = MetaEntryKind_BeginScope; }break;
		case MetaParseToken_EndScope:{   p->u.kind = MetaEntryKind_EndScope;   }break;

		/* NOTE(rnp): loose '[' implies implicit @Array() */
		case MetaParseToken_BeginArray:{ p->u.kind = MetaEntryKind_Array; }break;

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
meta_parser_fill_argument_array(MetaParser *p, MetaEntryArgument *array, Arena *arena)
{
	array->kind     = MetaEntryArgumentKind_Array;
	array->strings  = arena_aligned_start(*arena, alignof(s8));
	array->location = p->p.location;
	for (MetaParseToken token = meta_parser_token(p);
	     token != MetaParseToken_EndArray;
	     token = meta_parser_token(p))
	{
		switch (token) {
		case MetaParseToken_RawString:
		case MetaParseToken_String:
		{
			assert((u8 *)(array->strings + array->count) == arena->beg);
			*push_struct(arena, s8) = p->u.string;
			array->count++;
		}break;
		default:{ meta_parser_unexpected_token(p, token); }break;
		}
	}
}

function void
meta_parser_arguments(MetaParser *p, MetaEntry *e, Arena *arena)
{
	if (meta_parser_peek_token(p) == MetaParseToken_BeginArgs) {
		meta_parser_commit(p);

		e->arguments = arena_aligned_start(*arena, alignof(MetaEntryArgument));
		for (MetaParseToken token = meta_parser_token(p);
		     token != MetaParseToken_EndArgs;
		     token = meta_parser_token(p))
		{
			e->argument_count++;
			MetaEntryArgument *arg = push_struct(arena, MetaEntryArgument);
			switch (token) {
			case MetaParseToken_RawString:
			case MetaParseToken_String:
			{
				arg->kind     = MetaEntryArgumentKind_String;
				arg->string   = p->u.string;
				arg->location = p->p.location;
			}break;
			case MetaParseToken_BeginArray:{
				meta_parser_fill_argument_array(p, arg, arena);
			}break;
			default:{ meta_parser_unexpected_token(p, token); }break;
			}
		}
	}
}

typedef struct {
	MetaEntry *start;
	MetaEntry *one_past_last;
	iz consumed;
} MetaEntryScope;

function MetaEntryScope
meta_entry_extract_scope(MetaEntry *base, iz entry_count)
{
	assert(base->kind != MetaEntryKind_BeginScope && base->kind != MetaEntryKind_EndScope);
	assert(entry_count > 0);

	MetaEntryScope result = {.start = base + 1, .consumed = 1};
	iz sub_scope = 0;
	for (MetaEntry *e = result.start; result.consumed < entry_count; result.consumed++, e++) {
		switch (e->kind) {
		case MetaEntryKind_BeginScope:{ sub_scope++; }break;
		case MetaEntryKind_EndScope:{   sub_scope--; }break;
		default:{}break;
		}
		if (sub_scope == 0) break;
	}

	if (sub_scope != 0)
		meta_entry_error(base, "unclosed scope for entry\n");

	result.one_past_last = base + result.consumed;
	if (result.start->kind == MetaEntryKind_BeginScope) result.start++;
	if (result.one_past_last == result.start) result.one_past_last++;

	return result;
}

function MetaEntryStack
meta_entry_stack_from_file(Arena *arena, char *file)
{
	MetaParser     parser = {.p.s = read_entire_file(file, arena)};
	MetaEntryStack result = {.raw = parser.p.s};

	compiler_file = file;

	meta_parser_trim(&parser);

	for (MetaParseToken token = meta_parser_token(&parser);
	     token != MetaParseToken_EOF;
	     token = meta_parser_token(&parser))
	{
		MetaEntry *e = da_push(arena, &result);
		switch (token) {
		case MetaParseToken_RawString:{
			e->kind     = MetaEntryKind_String;
			e->location = parser.save_point.location;
			e->name     = parser.u.string;
		}break;
		case MetaParseToken_BeginArray:
		case MetaParseToken_BeginScope:
		case MetaParseToken_EndScope:
		case MetaParseToken_Entry:
		{
			e->kind     = parser.u.kind;
			e->location = parser.save_point.location;

			if (token == MetaParseToken_Entry)
				meta_parser_arguments(&parser, e, arena);

			if (token == MetaParseToken_BeginArray) {
				MetaEntryArgument *a = e->arguments = push_struct(arena, MetaEntryArgument);
				e->argument_count = 1;
				meta_parser_fill_argument_array(&parser, a, arena);
			}

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
	s8  *names_upper;
	s8  *names_lower;
	u32  floating_point;
	u32  entry_count;
	u32  shader_id;
} MetaShaderBakeParameters;
DA_STRUCT(MetaShaderBakeParameters, MetaShaderBakeParameters);

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
	MetaIDList                global_flag_ids;
	MetaIDList                shader_enumeration_ids;
	MetaShaderBakeParameters *bake_parameters;
	u32                       name_id;
	u32                       flag_list_id;
	i32                       base_shader_id;
} MetaShader;
DA_STRUCT(MetaShader, MetaShader);

typedef struct {
	MetaShader *shader;
	MetaIDList  sub_shaders;
	s8          file;
} MetaBaseShader;
DA_STRUCT(MetaBaseShader, MetaBaseShader);

typedef struct {
	s8         name;
	MetaIDList shaders;
} MetaShaderGroup;
DA_STRUCT(MetaShaderGroup, MetaShaderGroup);

typedef struct {
	s8  *fields;
	s8 **entries;
	u32 field_count;
	u32 entry_count;
	u32 table_name_id;
} MetaTable;
DA_STRUCT(MetaTable, MetaTable);

typedef struct {
	s8   enumeration_name;
	s8  *sub_table_names;
	u32  sub_table_count;
	u32  namespace_id;

	MetaLocation location;
} MetaMUnion;
DA_STRUCT(MetaMUnion, MetaMUnion);

typedef enum {
	MetaExpansionPartKind_Alignment,
	MetaExpansionPartKind_Conditional,
	MetaExpansionPartKind_EvalKind,
	MetaExpansionPartKind_EvalKindCount,
	MetaExpansionPartKind_Reference,
	MetaExpansionPartKind_String,
} MetaExpansionPartKind;

typedef enum {
	MetaExpansionConditionalArgumentKind_Invalid,
	MetaExpansionConditionalArgumentKind_Number,
	MetaExpansionConditionalArgumentKind_Evaluation,
	MetaExpansionConditionalArgumentKind_Reference,
} MetaExpansionConditionalArgumentKind;

typedef struct {
	MetaExpansionConditionalArgumentKind kind;
	union {
		s8 *strings;
		i64 number;
	};
} MetaExpansionConditionalArgument;

typedef enum {
	MetaExpansionOperation_Invalid,
	MetaExpansionOperation_LessThan,
	MetaExpansionOperation_GreaterThan,
} MetaExpansionOperation;

typedef struct {
	MetaExpansionConditionalArgument lhs;
	MetaExpansionConditionalArgument rhs;
	MetaExpansionOperation           op;
	u32 instruction_skip;
} MetaExpansionConditional;

typedef struct {
	MetaExpansionPartKind kind;
	union {
		s8  string;
		s8 *strings;
		MetaExpansionConditional conditional;
	};
} MetaExpansionPart;
DA_STRUCT(MetaExpansionPart, MetaExpansionPart);

typedef enum {
	MetaEmitOperationKind_Expand,
	MetaEmitOperationKind_FileBytes,
	MetaEmitOperationKind_String,
} MetaEmitOperationKind;

typedef struct {
	MetaExpansionPart *parts;
	u32 part_count;
	u32 table_id;
} MetaEmitOperationExpansion;

typedef struct {
	union {
		s8 string;
		MetaEmitOperationExpansion expansion_operation;
	};
	MetaEmitOperationKind kind;
	MetaLocation          location;
} MetaEmitOperation;

typedef struct {
	MetaEmitOperation *data;
	iz count;
	iz capacity;

	s8 filename;
} MetaEmitOperationList;

typedef struct {
	MetaEmitOperationList *data;
	iz count;
	iz capacity;
} MetaEmitOperationListSet;

typedef struct {
	Arena *arena, scratch;

	s8 filename;
	s8 directory;

	s8_list                      enumeration_kinds;
	s8_list_table                enumeration_members;

	s8_list_table                flags_for_shader;

	s8_list                      table_names;
	MetaTableList                tables;

	s8_list                      munion_namespaces;
	MetaMUnionList               munions;

	MetaEmitOperationListSet     emit_sets[MetaEmitLang_Count];

	MetaShaderBakeParametersList shader_bake_parameters;
	MetaIDList                   shader_enumerations;
	MetaShaderGroupList          shader_groups;
	MetaShaderList               shaders;
	MetaBaseShaderList           base_shaders;
	s8_list                      shader_names;
} MetaContext;

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
	iz result = meta_lookup_string_slow(sv->data, sv->count, s);
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
meta_pack_shader_bake_parameters(MetaContext *ctx, MetaEntry *e, iz entry_count, u32 shader_id, u32 *table_id)
{
	assert(e->kind == MetaEntryKind_Bake);

	MetaShaderBakeParameters *bp = da_push(ctx->arena, &ctx->shader_bake_parameters);
	bp->shader_id = shader_id;
	if (table_id) *table_id = (u32)da_index(bp, &ctx->shader_bake_parameters);

	if (e->argument_count) meta_entry_argument_expected_(e, 0, 0);

	MetaEntryScope scope = meta_entry_extract_scope(e, entry_count);
	if (scope.consumed > 1) {
		for (MetaEntry *row = scope.start; row != scope.one_past_last; row++) {
			if (row->kind != MetaEntryKind_BakeInt && row->kind != MetaEntryKind_BakeFloat)
				meta_entry_nesting_error(row, MetaEntryKind_Bake);
			meta_entry_argument_expected(row, s8("name"), s8("name_lower"));
			bp->entry_count++;
		}

		if (bp->entry_count > 32)
			meta_entry_error(e, "maximum bake parameter count exceeded: limit: 32\n");

		bp->names_upper = push_array(ctx->arena, s8, bp->entry_count);
		bp->names_lower = push_array(ctx->arena, s8, bp->entry_count);

		u32 row_index = 0;
		for (MetaEntry *row = scope.start; row != scope.one_past_last; row++, row_index++) {
			bp->names_upper[row_index]  = row->arguments[0].string;
			bp->names_lower[row_index]  = row->arguments[1].string;
			bp->floating_point         |= (u32)(row->kind == MetaEntryKind_BakeFloat) << row_index;
		}
	}

	return scope.consumed;
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

function void
meta_extend_enumeration(MetaContext *ctx, s8 kind, s8 *variations, uz count)
{
	iz kidx = meta_enumeration_id(ctx, kind);
	/* NOTE(rnp): may overcommit if duplicates exist in variations */
	da_reserve(ctx->arena, ctx->enumeration_members.data + kidx, (iz)count);
	for (uz i = 0; i < count; i++)
		meta_intern_string(ctx, ctx->enumeration_members.data + kidx, variations[i]);
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
	if (index > 31) meta_entry_error(e, "maximum shader local flags exceeded: limit: 32\n");
	u8 result = (u8)index;
	return result;
}

function iz
meta_pack_shader(MetaContext *ctx, MetaShaderGroup *sg, Arena scratch, MetaEntry *entries, iz entry_count)
{
	assert(entries[0].kind == MetaEntryKind_Shader);

	MetaShader *s = da_push(ctx->arena, &ctx->shaders);
	*da_push(ctx->arena, &sg->shaders) = (u32)da_index(s, &ctx->shaders);
	{
		s8_list *flag_list = da_push(ctx->arena, &ctx->flags_for_shader);
		s->flag_list_id    = (u32)da_index(flag_list, &ctx->flags_for_shader);
	}
	s->name_id        = meta_pack_shader_name(ctx, entries->name, entries->location);
	s->base_shader_id = -1;

	MetaBaseShader *base_shader = 0;
	if (entries->argument_count > 1) {
		meta_entry_argument_expected(entries, s8("[file_name]"));
	} else if (entries->argument_count == 1) {
		base_shader = da_push(ctx->arena, &ctx->base_shaders);
		base_shader->file = meta_entry_argument_expect(entries, 0, MetaEntryArgumentKind_String).string;
		base_shader->shader = s;
		s->base_shader_id = (i32)da_index(base_shader, &ctx->base_shaders);
	}

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
		case MetaEntryKind_Shader:
		{
			*da_push(&scratch, &stack) = (i32)result;
			if ((result + 1 < entry_count) && entries[result + 1].kind == MetaEntryKind_BeginScope)
				break;
		} /* FALLTHROUGH */
		case MetaEntryKind_EndScope:{
			i32 index = stack.data[--stack.count];
			MetaEntry *ended = entries + index;
			if (index != 0) {
				if (stack.count > 0 && entries[stack.data[stack.count - 1]].kind == MetaEntryKind_Shader) {
					if (ended->kind == MetaEntryKind_SubShader) {
						if (!base_shader) {
							meta_entry_error(ended, "invalid nesting: @%s in @%s\n"
							                 "@%s only allowed in base shaders (shaders with a backing file)\n",
							                 meta_entry_kind_strings[ended->kind],
							                 meta_entry_kind_strings[MetaEntryKind_Shader],
							                 meta_entry_kind_strings[ended->kind]);
						}
						MetaShader *ss = da_push(ctx->arena, &ctx->shaders);
						u32 sid = (u32)da_index(ss, &ctx->shaders);
						*da_push(ctx->arena, &sg->shaders) = sid;
						*da_push(ctx->arena, &base_shader->sub_shaders) = sid;

						ss->flag_list_id   = s->flag_list_id;
						ss->base_shader_id = s->base_shader_id;
						ss->name_id        = meta_pack_shader_name(ctx, ended->name, ended->location);
						meta_commit_shader_flag(ctx, s->flag_list_id, ended->name, ended);
						in_sub_shader = 0;
					}
				}
			}
		}break;
		case MetaEntryKind_Enumeration:{
			meta_entry_argument_expected(e, s8("kind"));
			s8 kind = meta_entry_argument_expect(e, 0, MetaEntryArgumentKind_String).string;
			iz kid  = meta_enumeration_id(ctx, kind);
			meta_intern_id(ctx, &s->shader_enumeration_ids,
			               (u32)meta_intern_id(ctx, &ctx->shader_enumerations, (u32)kid));
		}break;
		case MetaEntryKind_Flags:{
			meta_entry_argument_expected(e, s8("[flag ...]"));
			MetaEntryArgument flags = meta_entry_argument_expect(e, 0, MetaEntryArgumentKind_Array);
			for (u32 index = 0; index < flags.count; index++)
				meta_commit_shader_flag(ctx, s->flag_list_id, flags.strings[index], e);
		}break;
		case MetaEntryKind_Bake:{
			if (s->bake_parameters) {
				meta_entry_error(e, "invalid @%s in @%s: only one @%s allowed per @%s\n",
				                 meta_entry_kind_strings[e->kind], meta_entry_kind_strings[MetaEntryKind_Shader],
				                 meta_entry_kind_strings[e->kind], meta_entry_kind_strings[MetaEntryKind_Shader]);
			}
			u32 table_id;
			result += meta_pack_shader_bake_parameters(ctx, e, entry_count - result, (u32)da_index(s, &ctx->shaders), &table_id);
			s->bake_parameters = ctx->shader_bake_parameters.data + table_id;
		}break;

		default:
		error:
		{
			meta_entry_nesting_error(e, MetaEntryKind_Shader);
		}break;
		}
		if (stack.count == 0)
			break;
	}

	return result;
}

function void
meta_expansion_string_split(s8 string, s8 *left, s8 *inner, s8 *remainder, MetaLocation loc)
{
	b32 found = 0;
	for (u8 *s = string.data, *e = s + string.len; (s + 1) != e; s++) {
		u32 val  = (u32)'$'  << 8u | (u32)'(';
		u32 test = (u32)s[0] << 8u | s[1];
		if (test == val) {
			if (left) {
				left->data = string.data;
				left->len  = s - string.data;
			}

			u8 *start = s + 2;
			while (s != e && *s != ')') s++;
			if (s == e) {
				meta_compiler_error_message(loc, "unterminated expansion in raw string:\n  %.*s\n",
				                            (i32)string.len, string.data);
				fprintf(stderr, "  %.*s^\n", (i32)(start - string.data), "");
				meta_error();
			}

			if (inner) {
				inner->data = start;
				inner->len  = s - start;
			}

			if (remainder) {
				remainder->data = s + 1;
				remainder->len  = string.len - (remainder->data - string.data);
			}
			found = 1;
			break;
		}
	}
	if (!found) {
		if (left)      *left      = string;
		if (inner)     *inner     = (s8){0};
		if (remainder) *remainder = (s8){0};
	}
}

function MetaExpansionPart *
meta_push_expansion_part(MetaContext *ctx, Arena *arena, MetaExpansionPartList *parts,
                         MetaExpansionPartKind kind, s8 string, MetaTable *t, MetaLocation loc)
{
	MetaExpansionPart *result = da_push(arena, parts);
	result->kind = kind;
	switch (kind) {
	case MetaExpansionPartKind_Alignment:
	case MetaExpansionPartKind_Conditional:
	{}break;
	case MetaExpansionPartKind_EvalKind:
	case MetaExpansionPartKind_EvalKindCount:
	case MetaExpansionPartKind_Reference:
	{
		iz index = meta_lookup_string_slow(t->fields, t->field_count, string);
		result->strings = t->entries[index];
		if (index < 0) {
			/* TODO(rnp): fix this location to point directly at the field in the string */
			s8 table_name = ctx->table_names.data[t->table_name_id];
			meta_compiler_error(loc, "table \"%.*s\" does not contain member: %.*s\n",
			                    (i32)table_name.len, table_name.data, (i32)string.len, string.data);
		}
	}break;
	case MetaExpansionPartKind_String:{ result->string = string; }break;
	InvalidDefaultCase;
	}
	return result;
}

#define META_EXPANSION_TOKEN_LIST \
	X('|', Alignment) \
	X('%', TypeEval) \
	X('#', TypeEvalElements) \
	X('"', Quote) \
	X('-', Dash) \
	X('>', GreaterThan) \
	X('<', LessThan) \

typedef enum {
	MetaExpansionToken_EOF,
	MetaExpansionToken_Identifier,
	MetaExpansionToken_Number,
	MetaExpansionToken_String,
	#define X(__1, kind, ...) MetaExpansionToken_## kind,
	META_EXPANSION_TOKEN_LIST
	#undef X
	MetaExpansionToken_Count,
} MetaExpansionToken;

read_only global s8 meta_expansion_token_strings[] = {
	s8_comp("EOF"),
	s8_comp("Indentifier"),
	s8_comp("Number"),
	s8_comp("String"),
	#define X(s, kind, ...) s8_comp(#s),
	META_EXPANSION_TOKEN_LIST
	#undef X
};

typedef	struct {
	s8 s;
	union {
		i64 number;
		s8  string;
	};
	s8 save;
	MetaLocation loc;
} MetaExpansionParser;

#define meta_expansion_save(v)    (v)->save = (v)->s
#define meta_expansion_restore(v) swap((v)->s, (v)->save)
#define meta_expansion_commit(v)  meta_expansion_restore(v)

#define meta_expansion_expected(loc, e, g) \
	meta_compiler_error(loc, "invalid expansion string: expected %.*s after %.*s\n", \
	                    (i32)meta_expansion_token_strings[e].len, meta_expansion_token_strings[e].data, \
	                    (i32)meta_expansion_token_strings[g].len, meta_expansion_token_strings[g].data)

function s8
meta_expansion_extract_string(MetaExpansionParser *p)
{
	s8 result = {.data = p->s.data};
	for (; result.len < p->s.len; result.len++) {
		b32 done = 0;
		switch (p->s.data[result.len]) {
		#define X(t, ...) case t:
		META_EXPANSION_TOKEN_LIST
		#undef X
		case ' ':
		{done = 1;}break;
		default:{}break;
		}
		if (done) break;
	}
	p->s.data += result.len;
	p->s.len  -= result.len;
	return result;
}

function MetaExpansionToken
meta_expansion_token(MetaExpansionParser *p)
{
	MetaExpansionToken result = MetaExpansionToken_EOF;
	meta_expansion_save(p);
	if (p->s.len > 0) {
		b32 chop = 1;
		switch (p->s.data[0]) {
		#define X(t, kind, ...) case t:{ result = MetaExpansionToken_## kind; }break;
		META_EXPANSION_TOKEN_LIST
		#undef X
		default:{
			chop = 0;
			if (BETWEEN(p->s.data[0], '0', '9')) result = MetaExpansionToken_Number;
			else                                 result = MetaExpansionToken_Identifier;
		}break;
		}
		if (chop) {
			s8_chop(&p->s, 1);
			p->s = s8_trim(p->s);
		}

		switch (result) {
		case MetaExpansionToken_Number:{
			IntegerConversion integer = integer_from_s8(p->s);
			if (integer.result != IntegerConversionResult_Success) {
				/* TODO(rnp): point at start */
				meta_compiler_error(p->loc, "invalid integer in expansion string\n");
			}
			p->number = integer.S64;
			p->s      = integer.unparsed;
		}break;
		case MetaExpansionToken_Identifier:{ p->string = meta_expansion_extract_string(p); }break;
		default:{}break;
		}
		p->s = s8_trim(p->s);
	}
	return result;
}

function MetaExpansionPart *
meta_expansion_start_conditional(MetaContext *ctx, Arena *arena, MetaExpansionPartList *ops,
                                 MetaExpansionParser *p, MetaExpansionToken token, b32 negate)
{
	MetaExpansionPart *result = meta_push_expansion_part(ctx, arena, ops, MetaExpansionPartKind_Conditional,
	                                                     s8(""), 0, p->loc);
	switch (token) {
	case MetaExpansionToken_Number:{
		result->conditional.lhs.kind   = MetaExpansionConditionalArgumentKind_Number;
		result->conditional.lhs.number = negate ? -p->number : p->number;
	}break;
	default:{}break;
	}
	return result;
}

function void
meta_expansion_end_conditional(MetaExpansionPart *ep, MetaExpansionParser *p, MetaExpansionToken token, b32 negate)
{
	if (ep->conditional.rhs.kind != MetaExpansionConditionalArgumentKind_Invalid) {
		meta_compiler_error(p->loc, "invalid expansion conditional: duplicate right hand expression: '%.*s'\n",
		                    (i32)p->save.len, p->save.data);
	}
	switch (token) {
	case MetaExpansionToken_Number:{
		ep->conditional.rhs.kind   = MetaExpansionConditionalArgumentKind_Number;
		ep->conditional.rhs.number = negate ? -p->number : p->number;
	}break;
	default:{}break;
	}
}

function MetaExpansionPartList
meta_generate_expansion_set(MetaContext *ctx, Arena *arena, s8 expansion_string, MetaTable *t, MetaLocation loc)
{
	MetaExpansionPartList result = {0};
	s8 left = {0}, inner, remainder = expansion_string;
	do {
		meta_expansion_string_split(remainder, &left, &inner, &remainder, loc);
		if (left.len)  meta_push_expansion_part(ctx, arena, &result, MetaExpansionPartKind_String, left, t, loc);
		if (inner.len) {
			MetaExpansionParser p[1] = {{.s = inner, .loc = loc}};

			MetaExpansionPart *test_part = 0;
			b32 count_test_parts = 0;

			for (MetaExpansionToken token = meta_expansion_token(p);
			     token != MetaExpansionToken_EOF;
			     token = meta_expansion_token(p))
			{
				if (count_test_parts) test_part->conditional.instruction_skip++;
				switch (token) {
				case MetaExpansionToken_Alignment:{
					meta_push_expansion_part(ctx, arena, &result, MetaExpansionPartKind_Alignment, p->s, t, loc);
				}break;

				case MetaExpansionToken_Identifier:{
					meta_push_expansion_part(ctx, arena, &result, MetaExpansionPartKind_Reference, p->string, t, loc);
				}break;

				case MetaExpansionToken_TypeEval:
				case MetaExpansionToken_TypeEvalElements:
				{
					if (meta_expansion_token(p) != MetaExpansionToken_Identifier) {
						loc.column += (u32)(p->save.data - expansion_string.data);
						meta_expansion_expected(loc, MetaExpansionToken_Identifier, token);
					}
					MetaExpansionPartKind kind = token == MetaExpansionToken_TypeEval ?
					                                      MetaExpansionPartKind_EvalKind :
					                                      MetaExpansionPartKind_EvalKindCount;
					meta_push_expansion_part(ctx, arena, &result, kind, p->string, t, loc);
				}break;

				case MetaExpansionToken_Quote:{
					u8 *point = p->s.data;
					s8 string = meta_expansion_extract_string(p);
					token = meta_expansion_token(p);
					if (token != MetaExpansionToken_Quote) {
						loc.column += (u32)(point - expansion_string.data);
						/* TODO(rnp): point at start */
						meta_compiler_error(loc, "unterminated string in expansion\n");
					}
					meta_push_expansion_part(ctx, arena, &result, MetaExpansionPartKind_String, string, t, loc);
				}break;

				case MetaExpansionToken_Dash:{
					token = meta_expansion_token(p);
					switch (token) {
					case MetaExpansionToken_GreaterThan:{
						if (!test_part) goto error;
						if (test_part->conditional.lhs.kind == MetaExpansionConditionalArgumentKind_Invalid ||
						    test_part->conditional.rhs.kind == MetaExpansionConditionalArgumentKind_Invalid)
						{
							b32 lhs = test_part->conditional.lhs.kind == MetaExpansionConditionalArgumentKind_Invalid;
							b32 rhs = test_part->conditional.rhs.kind == MetaExpansionConditionalArgumentKind_Invalid;
							if (lhs && rhs)
								meta_compiler_error(loc, "expansion string test terminated without arguments\n");
							meta_compiler_error(loc, "expansion string test terminated without %s argument\n",
							                    lhs? "left" : "right");
						}
						count_test_parts = 1;
					}break;
					case MetaExpansionToken_Number:{
						if (test_part) meta_expansion_end_conditional(test_part, p, token, 1);
						else           test_part = meta_expansion_start_conditional(ctx, arena, &result, p, token, 1);
					}break;
					default:{ goto error; }break;
					}
				}break;

				case MetaExpansionToken_Number:{
					if (test_part) meta_expansion_end_conditional(test_part, p, token, 0);
					else           test_part = meta_expansion_start_conditional(ctx, arena, &result, p, token, 0);
				}break;

				case MetaExpansionToken_GreaterThan:
				case MetaExpansionToken_LessThan:
				{
					if (test_part && test_part->conditional.op != MetaExpansionOperation_Invalid) goto error;
					if (!test_part) {
						if (result.count == 0) {
							meta_compiler_error(p->loc, "invalid expansion conditional: missing left hand side\n");
						}

						s8 *strings = result.data[result.count - 1].strings;
						MetaExpansionPartKind last_kind = result.data[result.count - 1].kind;
						if (last_kind != MetaExpansionPartKind_EvalKindCount &&
						    last_kind != MetaExpansionPartKind_Reference)
						{
							meta_compiler_error(p->loc, "invalid expansion conditional: left hand side not numeric\n");
						}
						result.count--;
						test_part = meta_expansion_start_conditional(ctx, arena, &result, p, token, 0);
						if (last_kind == MetaExpansionPartKind_EvalKindCount) {
							test_part->conditional.lhs.kind = MetaExpansionConditionalArgumentKind_Evaluation;
						} else {
							test_part->conditional.lhs.kind = MetaExpansionConditionalArgumentKind_Reference;
						}
						test_part->conditional.lhs.strings = strings;
					}
					test_part->conditional.op = token == MetaExpansionToken_LessThan ?
					                                     MetaExpansionOperation_LessThan :
					                                     MetaExpansionOperation_GreaterThan;
				}break;

				error:
				default:
				{
					meta_compiler_error(loc, "invalid nested %.*s in expansion string\n",
					                    (i32)meta_expansion_token_strings[token].len,
					                    meta_expansion_token_strings[token].data);
				}break;
				}
			}
		}
	} while (remainder.len);
	return result;
}

function iz
meta_expand(MetaContext *ctx, Arena scratch, MetaEntry *e, iz entry_count, MetaEmitOperationList *ops)
{
	assert(e->kind == MetaEntryKind_Expand);

	/* TODO(rnp): for now this requires that the @Table came first */
	meta_entry_argument_expected(e, s8("table_name"));
	s8 table_name = meta_entry_argument_expect(e, 0, MetaEntryArgumentKind_String).string;

	MetaTable *t = ctx->tables.data + meta_lookup_string_slow(ctx->table_names.data,
	                                                          ctx->table_names.count, table_name);
	if (t < ctx->tables.data)
		meta_entry_error(e, "undefined table %.*s\n", (i32)table_name.len, table_name.data);

	MetaEntryScope scope = meta_entry_extract_scope(e, entry_count);
	for (MetaEntry *row = scope.start; row != scope.one_past_last; row++) {
		switch (row->kind) {
		case MetaEntryKind_String:{
			if (!ops) goto error;

			MetaExpansionPartList parts = meta_generate_expansion_set(ctx, ctx->arena, row->name, t, row->location);

			MetaEmitOperation *op = da_push(ctx->arena, ops);
			op->kind     = MetaEmitOperationKind_Expand;
			op->location = row->location;
			op->expansion_operation.parts      = parts.data;
			op->expansion_operation.part_count = (u32)parts.count;
			op->expansion_operation.table_id   = (u32)da_index(t,	&ctx->tables);
		}break;
		case MetaEntryKind_Enumeration:{
			if (ops) meta_entry_nesting_error(row, MetaEntryKind_Emit);

			meta_entry_argument_expected(row, s8("kind"), s8("`raw_string`"));
			s8 kind   = meta_entry_argument_expect(row, 0, MetaEntryArgumentKind_String).string;
			s8 expand = meta_entry_argument_expect(row, 1, MetaEntryArgumentKind_String).string;

			MetaExpansionPartList parts = meta_generate_expansion_set(ctx, &scratch, expand, t, row->location);
			s8 *variations = push_array(&scratch, s8, t->entry_count);
			for (u32 expansion = 0; expansion < t->entry_count; expansion++) {
				Stream sb = arena_stream(*ctx->arena);
				for (iz part = 0; part < parts.count; part++) {
					MetaExpansionPart *p = parts.data + part;
					u32 index = 0;
					if (p->kind == MetaExpansionPartKind_Reference) index = expansion;
					stream_append_s8(&sb, p->strings[index]);
				}
				variations[expansion] = arena_stream_commit(ctx->arena, &sb);
			}
			meta_extend_enumeration(ctx, kind, variations, t->entry_count);
		}break;
		error:
		default:
		{
			meta_entry_nesting_error(row, MetaEntryKind_Expand);
		}break;
		}
	}
	return scope.consumed;
}

function void
meta_embed(MetaContext *ctx, Arena scratch, MetaEntry *e, iz entry_count)
{
	assert(e->kind == MetaEntryKind_Embed);

	meta_entry_argument_expected(e, s8("filename"));
	s8 filename = meta_entry_argument_expect(e, 0, MetaEntryArgumentKind_String).string;

	MetaEmitOperationList *ops = da_push(ctx->arena, ctx->emit_sets + MetaEmitLang_C);
	if (e->name.len == 0) meta_entry_error(e, "name must be provided for output array");

	MetaEmitOperation *op;
	op = da_push(ctx->arena, ops);
	op->kind   = MetaEmitOperationKind_String;
	op->string = push_s8_from_parts(ctx->arena, s8(""), s8("read_only global u8 "), e->name, s8("[] = {"));

	op = da_push(ctx->arena, ops);
	op->kind   = MetaEmitOperationKind_FileBytes;
	op->string = filename;

	op = da_push(ctx->arena, ops);
	op->kind   = MetaEmitOperationKind_String;
	op->string = s8("};");
}

function MetaKind
meta_map_kind(s8 kind, s8 table_name, MetaLocation location)
{
	iz id = meta_lookup_string_slow(meta_kind_meta_types, MetaKind_Count, kind);
	if (id < 0) {
		meta_compiler_error(location, "Invalid Kind in '%.*s' table expansion: %.*s\n",
		                    (i32)table_name.len, table_name.data, (i32)kind.len, kind.data);
	}
	MetaKind result = (MetaKind)id;
	return result;
}

function MetaEmitLang
meta_map_emit_lang(s8 lang, MetaEntry *e)
{
	#define X(k, ...) s8_comp(#k),
	read_only local_persist s8 meta_lang_strings[] = {META_EMIT_LANG_LIST};
	#undef X

	iz id = meta_lookup_string_slow(meta_lang_strings, MetaEmitLang_Count, lang);
	if (id < 0) {
		#define X(k, ...) #k ", "
		meta_entry_error(e, "Unknown Emit Language: '%.*s'\nPossible Values: "
		                 META_EMIT_LANG_LIST "\n", (i32)lang.len, lang.data);
		#undef X
	}
	MetaEmitLang result = (MetaEmitLang)id;
	return result;
}

function iz
meta_pack_emit(MetaContext *ctx, Arena scratch, MetaEntry *e, iz entry_count)
{
	assert(e->kind == MetaEntryKind_Emit);

	MetaEmitLang lang = MetaEmitLang_C;
	if (e->argument_count) {
		meta_entry_argument_expected(e, s8("emit_language"));
		s8 name = meta_entry_argument_expect(e, 0, MetaEntryArgumentKind_String).string;
		lang = meta_map_emit_lang(name, e);
	}

	MetaEmitOperationList *ops = da_push(ctx->arena, ctx->emit_sets + lang);
	/* TODO(rnp): probably we should check this is unique */
	ops->filename = e->name;

	MetaEntryScope scope = meta_entry_extract_scope(e, entry_count);
	for (MetaEntry *row = scope.start; row != scope.one_past_last; row++) {
		switch (row->kind) {
		case MetaEntryKind_String:{
			MetaEmitOperation *op = da_push(ctx->arena, ops);
			op->kind     = MetaEmitOperationKind_String;
			op->string   = row->name;
			op->location = row->location;
		}break;
		case MetaEntryKind_Expand:{
			row += meta_expand(ctx, scratch, row, entry_count - (row - e), ops);
		}break;
		default:{ meta_entry_nesting_error(row, MetaEntryKind_Emit); }break;
		}
	}
	return scope.consumed;
}

function void
meta_pack_munion(MetaContext *ctx, MetaEntry *e, iz entry_count)
{
	assert(e->kind == MetaEntryKind_MUnion);

	MetaMUnion *mu = da_push(ctx->arena, &ctx->munions);
	mu->location = e->location;

	iz namespace_id = meta_lookup_string_slow(ctx->munion_namespaces.data,
	                                          ctx->munion_namespaces.count, e->name);
	if (namespace_id >= 0) meta_entry_error(e, "MUnion redefined\n");

	s8 *m_name = da_push(ctx->arena, &ctx->munion_namespaces);
	mu->namespace_id = (u32)da_index(m_name, &ctx->munion_namespaces);
	*m_name = e->name;

	meta_entry_argument_expected(e, s8("enumeration_name"), s8("[parameter_table_name ...]"));

	MetaEntryArgument sub_tables = meta_entry_argument_expect(e, 1, MetaEntryArgumentKind_Array);
	mu->enumeration_name = meta_entry_argument_expect(e, 0, MetaEntryArgumentKind_String).string;
	mu->sub_table_names  = sub_tables.strings;
	mu->sub_table_count  = (u32)sub_tables.count;
}

function iz
meta_pack_table(MetaContext *ctx, MetaEntry *e, iz entry_count)
{
	assert(e->kind == MetaEntryKind_Table);

	MetaTable *t = da_push(ctx->arena, &ctx->tables);
	iz table_name_id = meta_lookup_string_slow(ctx->table_names.data, ctx->table_names.count, e->name);
	if (table_name_id >= 0) meta_entry_error(e, "table redefined\n");

	s8 *t_name = da_push(ctx->arena, &ctx->table_names);
	t->table_name_id = (u32)da_index(t_name, &ctx->table_names);
	*t_name = e->name;

	meta_entry_argument_expected(e, s8("[field ...]"));
	MetaEntryArgument fields = meta_entry_argument_expect(e, 0, MetaEntryArgumentKind_Array);
	t->fields      = fields.strings;
	t->field_count = (u32)fields.count;

	MetaEntryScope scope = meta_entry_extract_scope(e, entry_count);
	if (scope.consumed > 1) {
		for (MetaEntry *row = scope.start; row != scope.one_past_last; row++) {
			if (row->kind != MetaEntryKind_Array)
				meta_entry_nesting_error(row, MetaEntryKind_Table);

			MetaEntryArgument entries = meta_entry_argument_expect(row, 0, MetaEntryArgumentKind_Array);
			if (entries.count != t->field_count) {
				meta_compiler_error_message(row->location, "incorrect field count for @%s entry got: %zu expected: %u\n",
				                            meta_entry_kind_strings[MetaEntryKind_Table],
				                            entries.count, t->field_count);
				fprintf(stderr, "  fields: [");
				for (uz i = 0; i < t->field_count; i++) {
					if (i != 0) fprintf(stderr, ", ");
					fprintf(stderr, "%.*s", (i32)t->fields[i].len, t->fields[i].data);
				}
				fprintf(stderr, "]\n");
				meta_error();
			}
			t->entry_count++;
		}

		t->entries = push_array(ctx->arena, s8 *, t->field_count);
		for (u32 field = 0; field < t->field_count; field++)
			t->entries[field] = push_array(ctx->arena, s8, t->entry_count);

		u32 row_index = 0;
		for (MetaEntry *row = scope.start; row != scope.one_past_last; row++, row_index++) {
			s8 *fs = row->arguments->strings;
			for (u32 field = 0; field < t->field_count; field++)
				t->entries[field][row_index] = fs[field];
		}
	}

	return scope.consumed;
}

function CommandList
meta_extract_emit_file_dependencies(MetaContext *ctx, Arena *arena)
{
	CommandList result = {0};
	for (iz set = 0; set < ctx->emit_sets[MetaEmitLang_C].count; set++) {
		MetaEmitOperationList *ops = ctx->emit_sets[MetaEmitLang_C].data + set;
		for (iz opcode = 0; opcode < ops->count; opcode++) {
			MetaEmitOperation *op = ops->data + opcode;
			switch (op->kind) {
			case MetaEmitOperationKind_FileBytes:{
				s8 filename = push_s8_from_parts(arena, s8(OS_PATH_SEPARATOR), ctx->directory, op->string);
				*da_push(arena, &result) = (c8 *)filename.data;
			}break;
			default:{}break;
			}
		}
	}
	return result;
}

function void
metagen_push_byte_array(MetaprogramContext *m, s8 bytes)
{
	for (iz i = 0; i < bytes.len; i++) {
		b32 end_line = (i != 0) && (i % 16) == 0;
		if (i != 0) meta_push(m, end_line ? s8(",") : s8(", "));
		if (end_line) meta_end_line(m);
		if ((i % 16) == 0) meta_indent(m);
		meta_push(m, s8("0x"));
		meta_push_u64_hex(m, bytes.data[i]);
	}
	meta_end_line(m);
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

function i64
meta_expansion_part_conditional_argument(MetaExpansionConditionalArgument a, u32 entry,
                                         s8 table_name, MetaLocation loc)
{
	i64 result = 0;
	switch (a.kind) {
	case MetaExpansionConditionalArgumentKind_Number:{
		result = a.number;
	}break;

	case MetaExpansionConditionalArgumentKind_Evaluation:
	{
		s8 string     = a.strings[entry];
		MetaKind kind = meta_map_kind(string, table_name, loc);
		result        = meta_kind_elements[kind];
	}break;

	case MetaExpansionConditionalArgumentKind_Reference:{
		s8 string = a.strings[entry];
		IntegerConversion integer = integer_from_s8(string);
		if (integer.result != IntegerConversionResult_Success) {
			meta_compiler_error(loc, "Invalid integer in '%.*s' table expansion: %.*s\n",
			                    (i32)table_name.len, table_name.data, (i32)string.len, string.data);
		}
		result = integer.S64;
	}break;

	InvalidDefaultCase;
	}

	return result;
}

function b32
meta_expansion_part_conditional(MetaExpansionPart *p, u32 entry, s8 table_name, MetaLocation loc)
{
	assert(p->kind == MetaExpansionPartKind_Conditional);
	b32 result = 0;
	i64 lhs = meta_expansion_part_conditional_argument(p->conditional.lhs, entry, table_name, loc);
	i64 rhs = meta_expansion_part_conditional_argument(p->conditional.rhs, entry, table_name, loc);
	switch (p->conditional.op) {
	case MetaExpansionOperation_LessThan:{    result = lhs < rhs; }break;
	case MetaExpansionOperation_GreaterThan:{ result = lhs > rhs; }break;
	InvalidDefaultCase;
	}
	return result;
}

function void
metagen_run_emit(MetaprogramContext *m, MetaContext *ctx, MetaEmitOperationList *ops,
                 s8 *evaluation_table)
{
	for (iz opcode = 0; opcode < ops->count; opcode++) {
		MetaEmitOperation *op = ops->data + opcode;
		switch (op->kind) {
		case MetaEmitOperationKind_String:{ meta_push_line(m, op->string); }break;
		case MetaEmitOperationKind_FileBytes:{
			Arena scratch = m->scratch;
			s8 filename = push_s8_from_parts(&scratch, s8(OS_PATH_SEPARATOR), ctx->directory, op->string);
			s8 file     = read_entire_file((c8 *)filename.data, &scratch);
			m->indentation_level++;
			metagen_push_byte_array(m, file);
			m->indentation_level--;
		}break;
		case MetaEmitOperationKind_Expand:{
			Arena scratch = m->scratch;

			MetaEmitOperationExpansion *eop = &op->expansion_operation;
			MetaTable *t  = ctx->tables.data + eop->table_id;
			s8 table_name = ctx->table_names.data[t->table_name_id];

			u32 alignment_count  = 1;
			u32 evaluation_count = 0;
			for (u32 part = 0; part < eop->part_count; part++) {
				if (eop->parts[part].kind == MetaExpansionPartKind_Alignment)
					alignment_count++;
				if (eop->parts[part].kind == MetaExpansionPartKind_EvalKind ||
				    eop->parts[part].kind == MetaExpansionPartKind_EvalKindCount)
					evaluation_count++;
			}

			MetaKind **evaluation_columns = push_array(&scratch, MetaKind *, evaluation_count);
			for (u32 column = 0; column < evaluation_count; column++)
				evaluation_columns[column] = push_array(&scratch, MetaKind, t->entry_count);

			for (u32 part = 0; part < eop->part_count; part++) {
				u32 eval_column = 0;
				MetaExpansionPart *p = eop->parts + part;
				if (p->kind == MetaExpansionPartKind_EvalKind) {
					for (u32 entry = 0; entry < t->entry_count; entry++) {
						evaluation_columns[eval_column][entry] = meta_map_kind(p->strings[entry],
						                                                       table_name, op->location);
					}
					eval_column++;
				}
			}

			s8 **columns = push_array(&scratch, s8 *, alignment_count);
			for (u32 column = 0; column < alignment_count; column++)
				columns[column] = push_array(&scratch, s8, t->entry_count);

			Stream sb = arena_stream(scratch);
			for (u32 entry = 0; entry < t->entry_count; entry++) {
				u32 column      = 0;
				u32 eval_column = 0;
				for (u32 part = 0; part < eop->part_count; part++) {
					MetaExpansionPart *p = eop->parts + part;
					switch (p->kind) {
					case MetaExpansionPartKind_Alignment:{
						columns[column][entry] = arena_stream_commit_and_reset(&scratch, &sb);
						column++;
					}break;

					case MetaExpansionPartKind_Conditional:{
						if (!meta_expansion_part_conditional(p, entry, table_name, op->location))
							part += p->conditional.instruction_skip;
					}break;

					case MetaExpansionPartKind_EvalKind:{
						s8 kind = evaluation_table[evaluation_columns[eval_column][entry]];
						stream_append_s8(&sb, kind);
					}break;

					case MetaExpansionPartKind_EvalKindCount:{
						stream_append_u64(&sb, meta_kind_elements[evaluation_columns[eval_column][entry]]);
					}break;

					case MetaExpansionPartKind_Reference:
					case MetaExpansionPartKind_String:
					{
						s8 string = p->kind == MetaExpansionPartKind_Reference ? p->strings[entry] : p->string;
						stream_append_s8(&sb, string);
					}break;
					}
				}

				columns[column][entry] = arena_stream_commit_and_reset(&scratch, &sb);
			}
			metagen_push_table(m, scratch, s8(""), s8(""), columns, t->entry_count, alignment_count);
		}break;
		InvalidDefaultCase;
		}
	}
	meta_end_line(m);
}

function void
metagen_run_emit_set(MetaprogramContext *m, MetaContext *ctx, MetaEmitOperationListSet *emit_set,
                     s8 *evaluation_table)
{
	for (iz set = 0; set < emit_set->count; set++) {
		MetaEmitOperationList *ops = emit_set->data + set;
		metagen_run_emit(m, ctx, ops, evaluation_table);
	}
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
meta_push_shader_reload_info(MetaprogramContext *m, MetaContext *ctx)
{
	///////////////////////////////
	// NOTE(rnp): reloadable infos
	meta_begin_scope(m, s8("read_only global BeamformerShaderKind beamformer_reloadable_shader_kinds[] = {"));
	for (iz shader = 0; shader < ctx->base_shaders.count; shader++) {
		MetaShader *s = ctx->base_shaders.data[shader].shader;
		meta_push_line(m, s8("BeamformerShaderKind_"), ctx->shader_names.data[s->name_id], s8(","));
	}
	meta_end_scope(m, s8("};\n"));

	meta_begin_scope(m, s8("read_only global s8 beamformer_reloadable_shader_files[] = {"));
	for (iz shader = 0; shader < ctx->base_shaders.count; shader++)
		meta_push_line(m, s8("s8_comp(\""), ctx->base_shaders.data[shader].file, s8("\"),"));
	meta_end_scope(m, s8("};\n"));

	{
		meta_begin_scope(m, s8("read_only global i32 beamformer_shader_reloadable_index_by_shader[] = {"));
		for (iz shader = 0; shader < ctx->shaders.count; shader++) {
			meta_indent(m);
			meta_push_i64(m, ctx->shaders.data[shader].base_shader_id);
			meta_end_line(m, s8(","));
		}
		meta_end_scope(m, s8("};\n"));
	}

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
					if (bs->shader == s) {
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
	for (iz ref = 0; ref < ctx->shader_enumerations.count; ref++) {
		u32 kind = ctx->shader_enumerations.data[ref];
		s8_list *sub_list  = ctx->enumeration_members.data + kind;
		s8 kind_name = push_s8_from_parts(&m->scratch, s8(""), ctx->enumeration_kinds.data[kind], s8("_"));
		meta_push_line(m, s8("s8_comp(\"\""));
		metagen_push_counted_enum_body(m, kind_name, s8("\"#define "), s8(""), s8("\\n\""),
		                               sub_list->data, sub_list->count);
		meta_push_line(m, s8("\"\\n\"),"));
		m->scratch = ctx->scratch;
	}
	meta_end_scope(m, s8("};\n"));

	meta_begin_scope(m, s8("read_only global s8 *beamformer_shader_flag_strings[] = {"));
	for (iz shader = 0; shader < ctx->base_shaders.count; shader++) {
		MetaShader *s         = ctx->base_shaders.data[shader].shader;
		s8_list    *flag_list = ctx->flags_for_shader.data + s->flag_list_id;

		if (flag_list->count) {
			meta_begin_scope(m, s8("(s8 []){"));
			for (iz flag = 0; flag < flag_list->count; flag++)
				meta_push_line(m, s8("s8_comp(\""), flag_list->data[flag], s8("\"),"));
			meta_end_scope(m, s8("},"));
		} else {
			meta_push_line(m, s8("0,"));
		}
	}
	meta_end_scope(m, s8("};\n"));

	meta_begin_scope(m, s8("read_only global u8 beamformer_shader_flag_strings_count[] = {"));
	for (iz shader = 0; shader < ctx->base_shaders.count; shader++) {
		MetaShader *s         = ctx->base_shaders.data[shader].shader;
		s8_list    *flag_list = ctx->flags_for_shader.data + s->flag_list_id;

		meta_indent(m);
		meta_push_u64(m, (u64)flag_list->count);
		meta_end_line(m, s8(","));
	}
	meta_end_scope(m, s8("};\n"));
}

function void
meta_push_shader_bake(MetaprogramContext *m, MetaContext *ctx)
{
	for (iz shader = 0; shader < ctx->base_shaders.count; shader++) {
		MetaShader *s = ctx->base_shaders.data[shader].shader;
		s8 shader_name = ctx->shader_names.data[s->name_id];
		meta_begin_line(m, s8("read_only global u8 beamformer_shader_"));
		for (iz i = 0; i < shader_name.len; i++)
			stream_append_byte(&m->stream, TOLOWER(shader_name.data[i]));
		meta_begin_scope(m, s8("_bytes[] = {")); {
			Arena scratch = m->scratch;
			s8 filename = push_s8_from_parts(&scratch, s8(OS_PATH_SEPARATOR), s8("shaders"),
			                                 ctx->base_shaders.data[shader].file);
			s8 file = read_entire_file((c8 *)filename.data, &scratch);
			metagen_push_byte_array(m, file);
		} meta_end_scope(m, s8("};\n"));
	}

	meta_begin_scope(m, s8("read_only global s8 beamformer_shader_data[] = {")); {
		Arena scratch = m->scratch;
		s8 *columns[2];
		columns[0] = push_array(&m->scratch, s8, ctx->base_shaders.count);
		columns[1] = push_array(&m->scratch, s8, ctx->base_shaders.count);
		for (iz shader = 0; shader < ctx->base_shaders.count; shader++) {
			MetaShader *s = ctx->base_shaders.data[shader].shader;
			s8 shader_name = ctx->shader_names.data[s->name_id];

			Stream sb = arena_stream(m->scratch);
			for (iz i = 0; i < shader_name.len; i++)
				stream_append_byte(&sb, TOLOWER(shader_name.data[i]));
			stream_append_s8(&sb, s8("_bytes,"));
			columns[0][shader] = arena_stream_commit_and_reset(&m->scratch, &sb);

			stream_append_s8(&sb, s8(".len = countof(beamformer_shader_"));
			for (iz i = 0; i < shader_name.len; i++)
				stream_append_byte(&sb, TOLOWER(shader_name.data[i]));
			columns[1][shader] = arena_stream_commit(&m->scratch, &sb);
		}
		metagen_push_table(m, m->scratch, s8("{.data = beamformer_shader_"), s8("_bytes)},"), columns,
		                   (uz)ctx->base_shaders.count, 2);
		m->scratch = scratch;
	} meta_end_scope(m, s8("};\n"));
}

function b32
metagen_emit_c_code(MetaContext *ctx, Arena arena)
{
	os_make_directory("generated");
	char *out_meta    = "generated" OS_PATH_SEPARATOR "beamformer.meta.c";
	char *out_shaders = "generated" OS_PATH_SEPARATOR "beamformer_shaders.c";

	MetaprogramContext m[1] = {{.stream = arena_stream(arena), .scratch = ctx->scratch}};

	if (setjmp(compiler_jmp_buf)) {
		build_fatal("Failed to generate C Code");
	}

	b32 result = 1;

	////////////////////////////
	// NOTE(rnp): shader baking
	{
		char **deps = push_array(&m->scratch, char *, ctx->base_shaders.count);
		for (iz i = 0; i < ctx->base_shaders.count; i++) {
			MetaBaseShader *b = ctx->base_shaders.data + i;
			deps[i] = (c8 *)push_s8_from_parts(&m->scratch, s8(OS_PATH_SEPARATOR), s8("shaders"), b->file).data;
		}
		if (needs_rebuild_(out_shaders, deps, ctx->base_shaders.count)) {
			build_log_generate("Bake Shaders");
			meta_push(m, c_file_header);
			meta_push_shader_bake(m, ctx);
			result &= meta_write_and_reset(m, out_shaders);
		}
		m->scratch = ctx->scratch;
	}

	if (!needs_rebuild(out_meta, "beamformer.meta"))
		return result;

	build_log_generate("Core C Code");

	meta_push(m, c_file_header);

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
			                                  ctx->shader_names.data[s->name_id], s8("Flags"));
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

			s8 first_name = ctx->shader_names.data[ctx->shaders.data[sg->shaders.data[0]].name_id];
			s8 last_name  = ctx->shader_names.data[ctx->shaders.data[sg->shaders.data[sg->shaders.count - 1]].name_id];

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
	for (u32 bake = 0; bake < ctx->shader_bake_parameters.count; bake++) {
		Arena tmp = m->scratch;
		MetaShaderBakeParameters *b = ctx->shader_bake_parameters.data + bake;
		MetaShader *s = ctx->shaders.data + b->shader_id;
		s8 name = push_s8_from_parts(&m->scratch, s8(""), s8("BeamformerShader"),
		                             ctx->shader_names.data[s->name_id], s8("BakeParameters"));
		meta_begin_scope(m, s8("typedef struct {"));
			for (u32 entry = 0; entry < b->entry_count; entry++) {
				s8 kind = (b->floating_point & (1 << entry))? s8("f32 ") : s8("u32 ");
				meta_push_line(m, kind, b->names_lower[entry], s8(";"));
			}
		meta_end_scope(m, s8("} "), name, s8(";\n"));
		m->scratch = tmp;
	}

	// shader bake parameter struct
	meta_begin_scope(m, s8("typedef struct {"));
	{
		meta_begin_scope(m, s8("union {"));
		{
			Arena tmp = m->scratch;
			s8 *columns[2];
			columns[0] = push_array(&m->scratch, s8, ctx->shader_bake_parameters.count);
			columns[1] = push_array(&m->scratch, s8, ctx->shader_bake_parameters.count);
			for (u32 bake = 0; bake < ctx->shader_bake_parameters.count; bake++) {
				MetaShaderBakeParameters *b = ctx->shader_bake_parameters.data + bake;
				MetaShader *s = ctx->shaders.data + b->shader_id;
				columns[0][bake] = push_s8_from_parts(&m->scratch, s8(""), s8("BeamformerShader"),
				                                      ctx->shader_names.data[s->name_id], s8("BakeParameters"));
				columns[1][bake] = ctx->shader_names.data[s->name_id];
			}
			metagen_push_table(m, m->scratch, s8(""), s8(";"), columns,
			                   (uz)ctx->shader_bake_parameters.count, 2);
			m->scratch = tmp;
		} meta_end_scope(m, s8("};"));
		s8 names[] = {s8("data_kind"), s8("flags")};
		s8 types[] = {s8("u32"),       s8("u32")};
		metagen_push_table(m, m->scratch, s8(""), s8(";"), (s8 *[]){types, names}, countof(names), 2);
	} meta_end_scope(m, s8("} BeamformerShaderBakeParameters;\n"));

	metagen_run_emit_set(m, ctx, ctx->emit_sets + MetaEmitLang_C, meta_kind_c_types);

	/////////////////////////////////
	// NOTE(rnp): shader info tables
	meta_begin_scope(m, s8("read_only global s8 beamformer_shader_names[] = {"));
	metagen_push_table(m, m->scratch, s8("s8_comp(\""), s8("\"),"), &ctx->shader_names.data,
	                   (uz)ctx->shader_names.count, 1);
	meta_end_scope(m, s8("};\n"));

	meta_push_shader_reload_info(m, ctx);

	meta_begin_scope(m, s8("read_only global i32 *beamformer_shader_header_vectors[] = {"));
	for (iz shader = 0; shader < ctx->base_shaders.count; shader++) {
		MetaShader *s = ctx->base_shaders.data[shader].shader;
		if (s->global_flag_ids.count || s->shader_enumeration_ids.count) {
			meta_begin_line(m, s8("(i32 []){"));
			for (iz id = 0; id < s->global_flag_ids.count; id++) {
				if (id != 0) meta_push(m, s8(", "));
				meta_push_u64(m, s->global_flag_ids.data[id]);
			}
			for (iz id = 0; id < s->shader_enumeration_ids.count; id++) {
				if (id != 0 || s->global_flag_ids.count) meta_push(m, s8(", "));
				meta_push_u64(m, s->shader_enumeration_ids.data[id]);
			}
			meta_end_line(m, s8("},"));
		} else {
			meta_push_line(m, s8("0,"));
		}
	}
	meta_end_scope(m, s8("};\n"));

	meta_begin_scope(m, s8("read_only global i32 beamformer_shader_header_vector_lengths[] = {"));
	for (iz shader = 0; shader < ctx->base_shaders.count; shader++) {
		MetaShader *s = ctx->base_shaders.data[shader].shader;
		meta_indent(m);
		meta_push_u64(m, (u64)(s->global_flag_ids.count + s->shader_enumeration_ids.count));
		meta_end_line(m, s8(","));
	}
	meta_end_scope(m, s8("};\n"));

	meta_begin_scope(m, s8("read_only global s8 *beamformer_shader_bake_parameter_names[] = {"));
	for (iz shader = 0; shader < ctx->base_shaders.count; shader++) {
		MetaShader *s = ctx->base_shaders.data[shader].shader;
		if (s->bake_parameters) {
			meta_begin_scope(m, s8("(s8 []){"));
			for (u32 index = 0; index < s->bake_parameters->entry_count; index++)
				meta_push_line(m, s8("s8_comp(\""), s->bake_parameters->names_upper[index], s8("\"),"));
			meta_end_scope(m, s8("},"));
		} else {
			meta_push_line(m, s8("0,"));
		}
	}
	meta_end_scope(m, s8("};\n"));

	meta_begin_scope(m, s8("read_only global u32 beamformer_shader_bake_parameter_float_bits[] = {"));
	for (iz shader = 0; shader < ctx->base_shaders.count; shader++) {
		MetaShader *s = ctx->base_shaders.data[shader].shader;
		meta_begin_line(m, s8("0x"));
		meta_push_u64_hex_width(m, s->bake_parameters? s->bake_parameters->floating_point : 0, 8);
		meta_end_line(m, s8("UL,"));
	}
	meta_end_scope(m, s8("};\n"));

	meta_begin_scope(m, s8("read_only global i32 beamformer_shader_bake_parameter_counts[] = {"));
	for (iz shader = 0; shader < ctx->base_shaders.count; shader++) {
		MetaShader *s = ctx->base_shaders.data[shader].shader;
		if (s->bake_parameters) {
			meta_indent(m);
			meta_push_u64(m, s->bake_parameters->entry_count);
			meta_end_line(m, s8(","));
		} else {
			meta_push_line(m, s8("0,"));
		}
	}
	meta_end_scope(m, s8("};\n"));

	//fprintf(stderr, "%.*s\n", (i32)m.stream.widx, m.stream.data);

	result = meta_write_and_reset(m, out_meta);

	return result;
}

function b32
metagen_matlab_union(MetaprogramContext *m, MetaContext *ctx, MetaMUnion *mu, s8 outdir)
{
	b32   result  = 1;
	Arena scratch = m->scratch;

	iz enumeration_id = meta_lookup_string_slow(ctx->enumeration_kinds.data, ctx->enumeration_kinds.count,
	                                            mu->enumeration_name);
	if (enumeration_id < 0) {
		meta_compiler_error(mu->location, "Kind Enumeration '%.*s' requested by @MUnion not defined\n",
		                    (i32)mu->enumeration_name.len, mu->enumeration_name.data);
	}

	s8_list *enumeration_members = ctx->enumeration_members.data + enumeration_id;
	if ((u32)enumeration_members->count != mu->sub_table_count) {
		meta_compiler_error(mu->location, "'%.*s' contains %u members but %u were requested by @MUnion\n",
		                    (i32)mu->enumeration_name.len, mu->enumeration_name.data,
		                    (u32)enumeration_members->count, mu->sub_table_count);
	}

	struct table_match {
		MetaTable *table;
		MetaKind  *kinds;
		u32        name_index;
	} *table_matches;

	table_matches = push_array(&scratch, struct table_match, mu->sub_table_count);
	for (u32 index = 0; index < mu->sub_table_count; index++) {
		s8 sub_table_name = mu->sub_table_names[index];
		MetaTable *t = ctx->tables.data + meta_lookup_string_slow(ctx->table_names.data,
		                                                          ctx->table_names.count, sub_table_name);
		if (t < ctx->tables.data) {
			meta_compiler_error(mu->location, "Parameter Table '%.*s' requested by @MUnion not defined\n",
			                    (i32)sub_table_name.len, sub_table_name.data);
		}

		iz type_index = meta_lookup_string_slow(t->fields, t->field_count, s8("type"));
		iz name_index = meta_lookup_string_slow(t->fields, t->field_count, s8("name"));
		if (type_index < 0) {
			meta_compiler_error_message(mu->location, "'%.*s' missing fields required by @MUnion: type\n",
			                            (i32)sub_table_name.len, sub_table_name.data);
		}
		if (name_index < 0) {
			meta_compiler_error_message(mu->location, "'%.*s' missing fields required by @MUnion: name\n",
			                            (i32)sub_table_name.len, sub_table_name.data);
		}
		if (type_index < 0 || name_index < 0) meta_error();

		table_matches[index].table      = t;
		table_matches[index].kinds      = push_array(&scratch, MetaKind, t->entry_count);
		table_matches[index].name_index = (u32)name_index;

		for (u32 entry = 0; entry < t->entry_count; entry++) {
			table_matches[index].kinds[entry] = meta_map_kind(t->entries[type_index][entry], sub_table_name,
			                                                  mu->location);
		}
	}

	u32 max_parameter_size = 0;
	for (iz member = 0; member < enumeration_members->count; member++) {
		u32 parameter_size = 0;
		for (u32 prop = 0; prop < table_matches[member].table->entry_count; prop++)
			parameter_size += meta_kind_byte_sizes[table_matches[member].kinds[prop]];
		max_parameter_size = MAX(parameter_size, max_parameter_size);
	}

	Arena scratch_temp = scratch;
	s8    namespace    = ctx->munion_namespaces.data[mu->namespace_id];

	s8 outfile = push_s8_from_parts(&scratch, s8(OS_PATH_SEPARATOR), outdir, s8("Base.m"));
	meta_begin_scope(m, s8("classdef Base"));
	{
		meta_begin_scope(m, s8("methods"));
		{
			meta_begin_scope(m, s8("function out = Pack(obj)"));
			{
				meta_begin_line(m, s8("out = zeros(1, "));
				meta_push_u64(m, max_parameter_size);
				meta_end_line(m, s8(", 'uint8');"));
				meta_push_line(m, s8("fields = struct2cell(struct(obj));"));
				meta_push_line(m, s8("offset = 1;"));
				meta_begin_scope(m, s8("for i = 1:numel(fields)"));
				{
					meta_push_line(m, s8("bytes = typecast(fields{i}, 'uint8');"));
					meta_push_line(m, s8("out(offset:(offset + numel(bytes) - 1)) = bytes;"));
					meta_push_line(m, s8("offset = offset + numel(bytes);"));
				} meta_end_scope(m, s8("end"));
			} meta_end_scope(m, s8("end"));
		} meta_end_scope(m, s8("end"));
	} meta_end_scope(m, s8("end"));
	result &= meta_end_and_write_matlab(m, (c8 *)outfile.data);
	scratch = scratch_temp;

	for (iz member = 0; member < enumeration_members->count; member++) {
		MetaTable *t     = table_matches[member].table;
		MetaKind  *kinds = table_matches[member].kinds;

		s8 sub_name = enumeration_members->data[member];
		outfile = push_s8_from_parts(&scratch, s8(""), outdir, s8(OS_PATH_SEPARATOR), sub_name, s8(".m"));
		meta_begin_scope(m, s8("classdef "), sub_name, s8(" < OGLBeamformer"), namespace, s8(".Base"));
		{
			meta_begin_scope(m, s8("properties"));
			{
				u32 name_index = table_matches[member].name_index;
				for (u32 prop = 0; prop < t->entry_count; prop++) {
					meta_begin_line(m, t->entries[name_index][prop], s8("(1,"));
					meta_push_u64(m, meta_kind_elements[kinds[prop]]);
					meta_end_line(m, s8(") "), meta_kind_matlab_types[kinds[prop]]);
				}
			} meta_end_scope(m, s8("end"));
		} meta_end_scope(m, s8("end"));
		result &= meta_end_and_write_matlab(m, (c8 *)outfile.data);
		scratch = scratch_temp;
	}

	return result;
}

function b32
metagen_emit_matlab_code(MetaContext *ctx, Arena arena)
{
	b32 result = 1;
	if (!needs_rebuild(OUTPUT("matlab/OGLBeamformerLiveImagingParameters.m"), "beamformer_parameters.h", "beamformer.meta"))
		return result;

	build_log_generate("MATLAB Bindings");
	char *base_directory = OUTPUT("matlab");
	if (!os_remove_directory(base_directory))
		build_fatal("failed to remove directory: %s", base_directory);

	if (setjmp(compiler_jmp_buf)) {
		os_remove_directory(base_directory);
		build_log_error("Failed to generate MATLAB Bindings");
		return 0;
	}

	os_make_directory(base_directory);

	MetaprogramContext m[1] = {{.stream = arena_stream(arena), .scratch = ctx->scratch}};

	#define X(name, flag, ...) meta_push_line(m, s8(#name " (" str(flag) ")"));
	meta_begin_matlab_class(m, "OGLBeamformerLiveFeedbackFlags", "int32");
	meta_begin_scope(m, s8("enumeration"));
	BEAMFORMER_LIVE_IMAGING_DIRTY_FLAG_LIST
	result &= meta_end_and_write_matlab(m, OUTPUT("matlab/OGLBeamformerLiveFeedbackFlags.m"));
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
			s8 *names = ctx->shader_names.data + ctx->shaders.data[0].name_id;
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

	////////////////////
	// NOTE: emit files
	{
		MetaEmitOperationListSet *emit_set = ctx->emit_sets + MetaEmitLang_MATLAB;
		for (u32 list = 0; list < emit_set->count; list++) {
			MetaEmitOperationList *ops = emit_set->data + list;
			Arena scratch = m->scratch;
			s8 output = push_s8_from_parts(&m->scratch, s8(""),
			                               s8(OUTPUT("matlab") OS_PATH_SEPARATOR "OGLBeamformer"),
			                               ops->filename, s8(".m"));
			meta_push_line(m, s8("% GENERATED CODE"));
			metagen_run_emit(m, ctx, ops, meta_kind_matlab_types);
			result &= meta_write_and_reset(m, (c8 *)output.data);
			m->scratch = scratch;
		}
	}

	//////////////////
	// NOTE: MUnions
	for (iz munion = 0; munion < ctx->munions.count; munion++) {
		Arena scratch = m->scratch;
		MetaMUnion *mu = ctx->munions.data + munion;
		s8 namespace   = ctx->munion_namespaces.data[mu->namespace_id];
		s8 outdir      = push_s8_from_parts(&m->scratch, s8(""), s8(OUTPUT("matlab")),
		                                    s8(OS_PATH_SEPARATOR "+OGLBeamformer"), namespace);
		os_make_directory((c8 *)outdir.data);
		result &= metagen_matlab_union(m, ctx, mu, outdir);
		m->scratch = scratch;
	}

	return result;
}

function b32
metagen_emit_helper_library_header(MetaContext *ctx, Arena arena)
{
	b32 result = 1;
	char *out = OUTPUT("ogl_beamformer_lib.h");
	if (!needs_rebuild(out, "lib/ogl_beamformer_lib_base.h", "beamformer.meta"))
		return result;

	build_log_generate("Library Header");

	s8 parameters_header = read_entire_file("beamformer_parameters.h", &arena);
	s8 base_header       = read_entire_file("lib/ogl_beamformer_lib_base.h", &arena);

	MetaprogramContext m[1] = {{.stream = arena_stream(arena), .scratch = ctx->scratch}};

	meta_push(m, c_file_header);
	meta_push_line(m, s8("#include <stdint.h>\n"));

	/////////////////////////
	// NOTE(rnp): enumarents
	for (iz kind = 0; kind < ctx->enumeration_kinds.count; kind++) {
		s8 enum_name = push_s8_from_parts(&m->scratch, s8(""), s8("Beamformer"), ctx->enumeration_kinds.data[kind]);
		metagen_push_c_enum(m, m->scratch, enum_name, ctx->enumeration_members.data[kind].data,
		                    ctx->enumeration_members.data[kind].count);
		m->scratch = ctx->scratch;
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
			u64 offset = 0;
			for (iz group = 0; group < index; group++)
				offset += (u64)ctx->shader_groups.data[group].shaders.count;

			MetaShaderGroup *sg = ctx->shader_groups.data + index;
			s8 *columns[2];
			columns[0] = push_array(&m->scratch, s8, sg->shaders.count);
			columns[1] = push_array(&m->scratch, s8, sg->shaders.count);

			Stream sb = arena_stream(m->scratch);
			for (iz id = 0; id < sg->shaders.count; id++) {
				stream_append_s8(&sb, s8("= "));
				stream_append_u64(&sb, offset + (u64)id);

				MetaShader *s = ctx->shaders.data + sg->shaders.data[id];
				columns[0][id] = ctx->shader_names.data[s->name_id];
				columns[1][id] = arena_stream_commit_and_reset(&m->scratch, &sb);
			}

			meta_begin_scope(m, s8("typedef enum {"));
			metagen_push_table(m, m->scratch, s8("BeamformerShaderKind_"), s8(","), columns,
			                   (uz)sg->shaders.count, 2);
			meta_end_scope(m, s8("} BeamformerShaderKind;\n"));

			m->scratch = ctx->scratch;

			meta_begin_line(m, s8("#define BeamformerShaderKind_ComputeCount ("));
			meta_push_u64(m, (u64)sg->shaders.count);
			meta_end_line(m, s8(")\n"));
		} else {
			build_log_failure("failed to find Compute shader group in meta info\n");
		}
	}

	metagen_run_emit_set(m, ctx, ctx->emit_sets + MetaEmitLang_CLibrary, meta_kind_base_c_types);

	meta_push_line(m, s8("// END GENERATED CODE\n"));

	meta_push(m, parameters_header, base_header);
	result &= meta_write_and_reset(m, out);

	{
		CommandList cpp = {0};
		cmd_append(&arena, &cpp, PREPROCESSOR, out, COMPILER_OUTPUT, OUTPUT("ogl_beamformer_lib_python_ffi.h"));
		result &= run_synchronous(arena, &cpp);
	}

	return result;
}

function MetaContext *
metagen_load_context(Arena *arena, char *filename)
{
	if (setjmp(compiler_jmp_buf)) {
		/* NOTE(rnp): compiler error */
		return 0;
	}

	MetaContext *ctx = push_struct(arena, MetaContext);
	ctx->scratch     = sub_arena(arena, MB(1), 16);
	ctx->arena       = arena;

	MetaContext *result = ctx;

	ctx->filename  = c_str_to_s8(filename);
	ctx->directory = s8_chop(&ctx->filename, s8_scan_backwards(ctx->filename, OS_PATH_SEPARATOR_CHAR));
	s8_chop(&ctx->filename, 1);
	if (ctx->directory.len <= 0) ctx->directory = s8(".");

	Arena scratch = ctx->scratch;
	MetaEntryStack entries = meta_entry_stack_from_file(ctx->arena, filename);

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
		case MetaEntryKind_Emit:{
			i += meta_pack_emit(ctx, scratch, e, entries.count - i);
		}break;
		case MetaEntryKind_Enumeration:{
			meta_entry_argument_expected(e, s8("kind"), s8("[id ...]"));
			s8 kind = meta_entry_argument_expect(e, 0, MetaEntryArgumentKind_String).string;
			MetaEntryArgument ids = meta_entry_argument_expect(e, 1, MetaEntryArgumentKind_Array);
			for (u32 id = 0; id < ids.count; id++)
				meta_commit_enumeration(ctx, kind, ids.strings[id]);
		}break;
		case MetaEntryKind_Embed:{
			meta_embed(ctx, scratch, e, entries.count - i);
		}break;
		case MetaEntryKind_Expand:{
			i += meta_expand(ctx, scratch, e, entries.count - i, 0);
		}break;
		case MetaEntryKind_MUnion:{
			meta_pack_munion(ctx, e, entries.count - i);
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
		case MetaEntryKind_Table:{
			i += meta_pack_table(ctx, e, entries.count - i);
		}break;

		error:
		default:
		{
			meta_entry_error(e, "invalid @%s() in global scope\n", meta_entry_kind_strings[e->kind]);
		}break;
		}
	}

	result->arena = 0;
	return result;
}

function b32
metagen_file_direct(Arena arena, char *filename)
{
	MetaContext *ctx = metagen_load_context(&arena, filename);
	if (!ctx) return 0;
	if (ctx->shaders.count || ctx->base_shaders.count || ctx->shader_groups.count) {
		build_log_error("shaders not supported in file: %s\n", filename);
		return 0;
	}

	b32 result = 1;
	char *out;
	{
		s8 basename;
		s8_split(ctx->filename, &basename, 0, '.');

		Stream sb = arena_stream(arena);
		stream_append_s8s(&sb, ctx->directory, s8(OS_PATH_SEPARATOR), s8("generated"));
		stream_append_byte(&sb, 0);
		os_make_directory((c8 *)sb.data);
		stream_reset(&sb, sb.widx - 1);

		stream_append_s8s(&sb, s8(OS_PATH_SEPARATOR), basename, s8(".c"));
		stream_append_byte(&sb, 0);

		out = (c8 *)arena_stream_commit(&arena, &sb).data;
	}

	CommandList deps = meta_extract_emit_file_dependencies(ctx, &arena);
	MetaprogramContext m[1] = {{.stream = arena_stream(arena), .scratch = ctx->scratch}};
	if (needs_rebuild_(out, deps.data, deps.count)) {
		build_log_generate(out);
		meta_push(m, c_file_header);
		metagen_run_emit_set(m, ctx, ctx->emit_sets + MetaEmitLang_C, meta_kind_c_types);
		result &= meta_write_and_reset(m, out);
	}

	return result;
}

i32
main(i32 argc, char *argv[])
{
	u64 start_time = os_timer_count();
	g_argv0 = argv[0];

	b32 result  = 1;
	Arena arena = os_alloc_arena(MB(8));
	check_rebuild_self(arena, argc, argv);

	os_make_directory(OUTDIR);

	result &= metagen_file_direct(arena, "assets" OS_PATH_SEPARATOR "assets.meta");

	MetaContext *meta = metagen_load_context(&arena, "beamformer.meta");
	if (!meta) return 1;

	result &= metagen_emit_c_code(meta, arena);
	result &= metagen_emit_helper_library_header(meta, arena);
	result &= metagen_emit_matlab_code(meta, arena);

	parse_config(argc, argv);

	if (!build_raylib(arena))  return 1;
	if (!build_glslang(arena)) return 1;

	/////////////////
	// lib/tests
	result &= build_helper_library(arena);
	if (config.tests) result &= build_tests(arena);

	//////////////////
	// static portion
	result &= build_beamformer_main(arena);

	/////////////////////////
	// hot reloadable portion
	//
	// NOTE: this is built after main because on w32 we need to export
	// gl function pointers for the reloadable portion to import
	if (config.debug) result &= build_beamformer_as_library(arena);

	if (config.time) {
		f64 seconds = (f64)(os_timer_count() - start_time) / (f64)os_timer_frequency();
		build_log_info("took %0.03f [s]", seconds);
	}

	return result != 1;
}
