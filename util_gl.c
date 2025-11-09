/* See LICENSE for license details. */
function u32
compile_shader(Arena a, u32 type, s8 shader, s8 name)
{
	u32 sid = glCreateShader(type);
	glShaderSource(sid, 1, (const char **)&shader.data, (int *)&shader.len);
	glCompileShader(sid);

	i32 res = 0;
	glGetShaderiv(sid, GL_COMPILE_STATUS, &res);

	if (res == GL_FALSE) {
		Stream buf = arena_stream(a);
		stream_append_s8s(&buf, s8("\n"), name, s8(": failed to compile\n"));

		i32 len = 0, out_len = 0;
		glGetShaderiv(sid, GL_INFO_LOG_LENGTH, &len);
		glGetShaderInfoLog(sid, len, &out_len, (char *)(buf.data + buf.widx));
		stream_commit(&buf, out_len);
		glDeleteShader(sid);
		os_write_file(os_error_handle(), stream_to_s8(&buf));

		sid = 0;
	}

	return sid;
}

function u32
link_program(Arena a, u32 *shader_ids, i32 shader_id_count)
{
	i32 success = 0;
	u32 result  = glCreateProgram();
	for (i32 i = 0; i < shader_id_count; i++)
		glAttachShader(result, shader_ids[i]);
	glLinkProgram(result);
	glGetProgramiv(result, GL_LINK_STATUS, &success);
	if (success == GL_FALSE) {
		i32 len    = 0;
		Stream buf = arena_stream(a);
		stream_append_s8(&buf, s8("shader link error: "));
		glGetProgramInfoLog(result, buf.cap - buf.widx, &len, (c8 *)(buf.data + buf.widx));
		stream_reset(&buf, len);
		stream_append_byte(&buf, '\n');
		os_write_file(os_error_handle(), stream_to_s8(&buf));
		glDeleteProgram(result);
		result = 0;
	}
	return result;
}

function u32
load_shader(Arena arena, s8 *shader_texts, u32 *shader_types, i32 count, s8 name)
{
	u32 result = 0;
	u32 *ids   = push_array(&arena, u32, count);
	b32 valid  = 1;
	for (i32 i = 0; i < count; i++) {
		ids[i]  = compile_shader(arena, shader_types[i], shader_texts[i], name);
		valid  &= ids[i] != 0;
	}

	if (valid) result = link_program(arena, ids, count);
	for (i32 i = 0; i < count; i++) glDeleteShader(ids[i]);

	if (result) glObjectLabel(GL_PROGRAM, result, (i32)name.len, (c8 *)name.data);

	return result;
}
