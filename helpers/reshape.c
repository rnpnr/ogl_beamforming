#include "../util.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zstd.h>

#define MAX_FILE_TRY_COUNT 64

#define ZEMP_BP_MAGIC (uint64_t)0x5042504D455AFECAull
typedef struct {
	u64 magic;
	u32 version;
	u16 decode_mode;
	u16 beamform_mode;
	u32 raw_data_dim[4];
	u32 decoded_data_dim[4];
	f32 xdc_element_pitch[2];
	f32 xdc_transform[16]; /* NOTE: column major order */
	i16 channel_mapping[256];
	f32 transmit_angles[256];
	f32 focal_depths[256];
	i16 sparse_elements[256];
	i16 hadamard_rows[256];
	f32 speed_of_sound;
	f32 center_frequency;
	f32 sampling_frequency;
	f32 time_offset;
	u32 transmit_mode;
} zemp_bp_v1;

#define die(...) die_((char *)__func__, __VA_ARGS__)
function void __attribute__((noreturn))
die_(char *function_name, char *format, ...)
{
	if (function_name)
		fprintf(stderr, "%s: ", function_name);

	va_list ap;

	va_start(ap, format);
	vfprintf(stderr, format, ap);
	va_end(ap);

	exit(1);
}

#if defined(__linux__)

#include "../os_linux.c"

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

function void
os_make_directory(u8 *name)
{
	mkdir((char *)name, 0770);
}

function s8
os_read_file_simp(u8 *name)
{
	s8 result;
	i32 fd = open((char *)name, O_RDONLY);
	if (fd < 0)
		die("couldn't open file: %s\n", name);

	struct stat st;
	if (stat((char *)name, &st) < 0)
		die("couldn't stat file\n");

	result.len  = st.st_size;
	result.data = malloc(st.st_size);
	if (!result.data)
		die("couldn't alloc space for reading\n");

	iz rlen = read(fd, result.data, st.st_size);
	close(fd);

	if (rlen != st.st_size)
		die("couldn't read file: %s\n", name);

	return result;
}

#elif defined(_WIN32)

#include "../os_win32.c"

W32(b32) CreateDirectoryA(u8 *, void *);

function void
os_make_directory(u8 *name)
{
	CreateDirectoryA(name, 0);
}

function s8
os_read_file_simp(u8 *name)
{
	s8 result;
	iptr h = CreateFileA(name, GENERIC_READ, 0, 0, OPEN_EXISTING, 0, 0);
	if (h == INVALID_FILE)
		die("couldn't open file: %s\n", name);

	w32_file_info fileinfo;
	if (!GetFileInformationByHandle(h, &fileinfo))
		die("couldn't get file info\n", stderr);

	result.len  = fileinfo.nFileSizeLow;
	result.data = malloc(fileinfo.nFileSizeLow);
	if (!result.data)
		die("couldn't alloc space for reading\n");

	i32 rlen = 0;
	if (!ReadFile(h, result.data, fileinfo.nFileSizeLow, &rlen, 0) && rlen != fileinfo.nFileSizeLow)
		die("couldn't read file: %s\n", name);
	CloseHandle(h);

	return result;
}

#else
#error Unsupported Platform
#endif

function void
stream_ensure_termination(Stream *s, u8 byte)
{
	b32 found = 0;
	if (!s->errors && s->widx > 0)
		found = s->data[s->widx - 1] == byte;
	if (!found) {
		s->errors |= s->cap - 1 < s->widx;
		if (!s->errors)
			s->data[s->widx++] = byte;
	}
}

function void
stream_append_u64_width(Stream *s, u64 n, u64 min_width)
{
	u8 tmp[64];
	u8 *end = tmp + sizeof(tmp);
	u8 *beg = end;
	min_width = MIN(sizeof(tmp), min_width);

	do { *--beg = '0' + (n % 10); } while (n /= 10);
	while (end - beg > 0 &&  end - beg < min_width)
		*--beg = '0';

	stream_append(s, beg, end - beg);
}

function void
stream_push_file_end_at_index(Stream *s, u32 index)
{
	stream_append_byte(s, '_');
	stream_append_u64_width(s, index, 2);
	stream_append_s8(s, s8(".zst"));
	stream_ensure_termination(s, 0);
}

function b32
write_i16_data_compressed(u8 *output_name, i16 *data, u32 data_element_count)
{
	iz data_size   = data_element_count * sizeof(*data);
	iz buffer_size = ZSTD_COMPRESSBOUND(data_size);

	b32 result = 0;
	void *buffer = malloc(buffer_size);
	if (buffer) {
		iz written = ZSTD_compress(buffer, buffer_size, data, data_size, ZSTD_CLEVEL_DEFAULT);
		result     = !ZSTD_isError(written);
		if (result) {
			result = os_write_new_file((char *)output_name,
			                           (s8){.data = buffer, .len = written});
		}
	}
	free(buffer);

	return result;
}

function void *
decompress_zstd_data(s8 raw)
{
	iz requested_size = ZSTD_getFrameContentSize(raw.data, raw.len);
	void *out         = malloc(requested_size);
	if (out) {
		iz decompressed = ZSTD_decompress(out, requested_size, raw.data, raw.len);
		if (decompressed != requested_size) {
			free(out);
			out = 0;
		}
	}
	return out;
}

function zemp_bp_v1 *
read_zemp_bp_v1(u8 *path)
{
	s8 raw = os_read_file_simp(path);
	zemp_bp_v1 *result = 0;
	if (raw.len == sizeof(zemp_bp_v1) && *(u64 *)raw.data == ZEMP_BP_MAGIC) {
		if (((zemp_bp_v1 *)raw.data)->version == 1)
			result = (zemp_bp_v1 *)raw.data;
	}
	return result;
}

function iz
reshaped_element_count(zemp_bp_v1 *zbp)
{
	iz frames   = zbp->raw_data_dim[0] / (zbp->decoded_data_dim[0] * zbp->decoded_data_dim[2]);
	iz channels = zbp->decoded_data_dim[1];

	iz channel_frame_elements = zbp->decoded_data_dim[0] * zbp->decoded_data_dim[2];
	iz frame_elements         = channel_frame_elements * channels;

	iz result = frames * frame_elements;

	return result;
}

function i16 *
reshape(i16 *restrict in, zemp_bp_v1 *restrict zbp)
{
	iz frames   = zbp->raw_data_dim[0] / (zbp->decoded_data_dim[0] * zbp->decoded_data_dim[2]);
	iz channels = zbp->decoded_data_dim[1];

	iz channel_frame_elements = zbp->decoded_data_dim[0] * zbp->decoded_data_dim[2];
	iz frame_elements         = channel_frame_elements * channels;

	i16 *result = malloc(sizeof(*in) * frames * frame_elements);
	if (result) {
		i16 *out = result;
		for (u32 i = 0; i < channels; i++) {
			i16 *in_channel = in + zbp->channel_mapping[i] * zbp->raw_data_dim[0];
			for (iz j = 0; j < frames; j++) {
				mem_copy(out + j * frame_elements, in_channel,
				         sizeof(*in) * channel_frame_elements);
				in_channel += channel_frame_elements;
			}
			out += channel_frame_elements;
		}
	}

	return result;
}

#define shift_n(v, c, n) v += n, c -= n
#define shift(v, c) shift_n(v, c, 1)

function void
usage(char *argv0)
{
	die("%s base_path study\n", argv0);
}

int
main(i32 argc, char *argv[])
{
	if (argc != 3)
		usage(argv[0]);

	char *argv0 = *argv;
	shift(argv, argc);

	s8 path_base = c_str_to_s8(*argv);
	shift(argv, argc);
	s8 study     = c_str_to_s8(*argv);
	shift(argv, argc);

	Arena arena     = os_alloc_arena((Arena){0}, KB(8));
	Stream path     = stream_alloc(&arena, KB(4));
	Stream out_path = arena_stream(arena);
	stream_append_s8(&path, path_base);
	stream_ensure_termination(&path, OS_PATH_SEPARATOR_CHAR);
	stream_append_s8(&path, study);
	stream_ensure_termination(&path, OS_PATH_SEPARATOR_CHAR);
	stream_append_s8(&path, study);

	stream_append_s8(&out_path, path_base);
	stream_ensure_termination(&out_path, OS_PATH_SEPARATOR_CHAR);
	stream_append_s8(&out_path, study);
	stream_append_s8(&out_path, s8("_reshaped"));

	iz path_work_index = path.widx;

	stream_append_s8(&path, s8(".bp"));
	stream_ensure_termination(&path, 0);
	zemp_bp_v1 *zbp = read_zemp_bp_v1(path.data);

	iz out_path_work_index = out_path.widx;

	stream_ensure_termination(&out_path, 0);
	os_make_directory(out_path.data);

	stream_reset(&out_path, out_path_work_index);
	stream_ensure_termination(&out_path, OS_PATH_SEPARATOR_CHAR);
	stream_append_s8(&out_path, study);
	stream_append_s8(&out_path, s8("_reshaped"));
	out_path_work_index = out_path.widx;

	/* NOTE(rnp): HACK: the way these files were saved was as if they had 1024 transmits
	 * instead we assume the transmit count == recieve channel count and fixup here */
	zbp->decoded_data_dim[2] = zbp->decoded_data_dim[1];

	i32 valid_frame_indices[MAX_FILE_TRY_COUNT];
	i32 valid_frames = 0;
	for (i32 frame = 0; frame < countof(valid_frame_indices); frame++) {
		stream_reset(&path, path_work_index);
		stream_push_file_end_at_index(&path, frame);
		if (os_file_exists((char *)path.data))
			valid_frame_indices[valid_frames++] = frame;
	}

	for (i32 i = 0; i < valid_frames; i++) {
		stream_reset(&path, path_work_index);
		stream_push_file_end_at_index(&path, valid_frame_indices[i]);

		s8 compressed_data = os_read_file_simp(path.data);
		i16 *data = decompress_zstd_data(compressed_data);
		if (data) {
			i16 *out = reshape(data, zbp);
			if (out) {
				stream_reset(&out_path, out_path_work_index);
				stream_push_file_end_at_index(&out_path, valid_frame_indices[i]);
				if (!write_i16_data_compressed(out_path.data, out,
				                               reshaped_element_count(zbp)))
				{
					fprintf(stderr, "failed to write: %s\n", out_path.data);
				}
				free(out);
			}
			free(data);
		}
		free(compressed_data.data);
	}

	zbp->raw_data_dim[2] = zbp->raw_data_dim[0] / (zbp->decoded_data_dim[0] * zbp->decoded_data_dim[2]);
	zbp->raw_data_dim[0] = zbp->decoded_data_dim[0] * zbp->decoded_data_dim[2];
	zbp->raw_data_dim[1] = zbp->decoded_data_dim[1];
	for (i32 i = 0; i < countof(zbp->channel_mapping); i++)
		zbp->channel_mapping[i] = i;

	stream_reset(&out_path, out_path_work_index);
	stream_append_s8(&out_path, s8(".bp"));
	stream_ensure_termination(&out_path, 0);
	if (!os_write_new_file((char *)out_path.data, (s8){.data = (u8 *)zbp, .len = sizeof(*zbp)}))
		fprintf(stderr, "failed to write: %s\n", out_path.data);

	return 0;
}
