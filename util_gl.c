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
		os_console_log(buf.data, buf.widx);

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
		os_console_log(buf.data, buf.widx);
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

function void
gl_gpu_buffer_release(GLGPUBuffer *gl)
{
	glDeleteBuffers(1, &gl->ssbo);
	glDeleteMemoryObjectsEXT(1, &gl->memory_object);
	os_release_handle(gl->os_handle);
}

#if 0
function void
gl_gpu_buffer_import(GPUBuffer *b, GLGPUBuffer *gl, s8 label)
{
	glCreateBuffers(1, &gl->ssbo);
	glCreateMemoryObjectsEXT(1, &gl->memory_object);
	if (OS_WINDOWS) {
		glImportMemoryWin32HandleEXT(gl->memory_object, b->size, GL_HANDLE_TYPE_OPAQUE_WIN32_EXT, (void *)gl->os_handle.value[0]);
		// NOTE(rnp): w32 does not transfer ownership from handle back to driver
	} else {
		glImportMemoryFdEXT(gl->memory_object, b->size, GL_HANDLE_TYPE_OPAQUE_FD_EXT, gl->os_handle.value[0]);
		gl->os_handle.value[0] = -1;
	}
	glNamedBufferStorageMemEXT(gl->ssbo, b->size, gl->memory_object, 0);

	glObjectLabel(GL_BUFFER, gl->ssbo, (i32)label.len, (c8 *)label.data);
}
#endif
