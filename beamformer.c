/* See LICENSE for license details. */
#include "beamformer.h"

/* TODO: remove this */
#include <string.h> /* memmove */

static void
alloc_output_image(BeamformerCtx *ctx)
{
	BeamformerParameters *bp = &ctx->params->raw;
	ctx->out_data_dim.x = ORONE(bp->output_points.x);
	ctx->out_data_dim.y = ORONE(bp->output_points.y);
	ctx->out_data_dim.z = ORONE(bp->output_points.z);

	/* NOTE: allocate storage for beamformed output data;
	 * this is shared between compute and fragment shaders */
	uv4 odim    = ctx->out_data_dim;
	u32 max_dim = MAX(odim.x, MAX(odim.y, odim.z));
	/* TODO: does this actually matter or is 0 fine? */
	ctx->out_texture_unit = 0;
	ctx->out_texture_mips = _tzcnt_u32(max_dim) + 1;
	glActiveTexture(GL_TEXTURE0 + ctx->out_texture_unit);
	glDeleteTextures(1, &ctx->out_texture);
	glGenTextures(1, &ctx->out_texture);
	glBindTexture(GL_TEXTURE_3D, ctx->out_texture);
	glTexStorage3D(GL_TEXTURE_3D, ctx->out_texture_mips, GL_RG32F, odim.x, odim.y, odim.z);

	UnloadRenderTexture(ctx->fsctx.output);
	ctx->fsctx.output = LoadRenderTexture(odim.x, odim.y);

	ctx->flags &= ~ALLOC_OUT_TEX;
}

static void
upload_filter_coefficients(BeamformerCtx *ctx, Arena a)
{
	ctx->flags &= ~UPLOAD_FILTER;
	return;
#if 0
	f32 lpf_coeff[] = {
		0.001504252781, 0.006636276841, 0.01834679954,  0.0386288017,
		0.06680636108,  0.09852545708,  0.1264867932,   0.1429549307,
		0.1429549307,   0.1264867932,   0.09852545708,  0.06680636108,
		0.0386288017,   0.01834679954,  0.006636276841, 0.001504252781,
	};
	u32 lpf_coeff_count  = ARRAY_COUNT(lpf_coeff);
	ctx->csctx.lpf_order = lpf_coeff_count - 1;
	rlUnloadShaderBuffer(ctx->csctx.lpf_ssbo);
	ctx->csctx.lpf_ssbo  = rlLoadShaderBuffer(lpf_coeff_count * sizeof(f32), lpf_coeff, GL_STATIC_DRAW);
#endif
}

static void
alloc_shader_storage(BeamformerCtx *ctx, Arena a)
{
	BeamformerParameters *bp = &ctx->params->raw;
	uv4 dec_data_dim         = bp->dec_data_dim;
	uv2 rf_raw_dim           = bp->rf_raw_dim;
	size rf_raw_size         = rf_raw_dim.x * rf_raw_dim.y * sizeof(i16);
	size rf_decoded_size     = dec_data_dim.x * dec_data_dim.y * dec_data_dim.z * sizeof(f32) * 2;
	ctx->csctx.rf_raw_dim    = rf_raw_dim;
	ctx->csctx.dec_data_dim  = dec_data_dim;

	glDeleteBuffers(ARRAY_COUNT(ctx->csctx.rf_data_ssbos), ctx->csctx.rf_data_ssbos);
	glDeleteBuffers(1, &ctx->csctx.raw_data_ssbo);
	glGenBuffers(1, &ctx->csctx.raw_data_ssbo);
	glGenBuffers(ARRAY_COUNT(ctx->csctx.rf_data_ssbos), ctx->csctx.rf_data_ssbos);
	glBindBuffer(GL_SHADER_STORAGE_BUFFER, ctx->csctx.raw_data_ssbo);
	glBufferData(GL_SHADER_STORAGE_BUFFER, rf_raw_size, 0, GL_STREAM_DRAW);

	for (u32 i = 0; i < ARRAY_COUNT(ctx->csctx.rf_data_ssbos); i++) {
		glBindBuffer(GL_SHADER_STORAGE_BUFFER, ctx->csctx.rf_data_ssbos[i]);
		glBufferStorage(GL_SHADER_STORAGE_BUFFER, rf_decoded_size, 0, 0);
	}

	/* NOTE: store hadamard in GPU once; it won't change for a particular imaging session */
	ctx->csctx.hadamard_dim = (uv2){.x = dec_data_dim.z, .y = dec_data_dim.z};
	size hadamard_elements  = dec_data_dim.z * dec_data_dim.z;
	i32  *hadamard          = alloc(&a, i32, hadamard_elements);
	fill_hadamard(hadamard, dec_data_dim.z);

	rlUnloadShaderBuffer(ctx->csctx.hadamard_ssbo);
	ctx->csctx.hadamard_ssbo = rlLoadShaderBuffer(hadamard_elements * sizeof(i32), hadamard,
	                                              GL_STATIC_DRAW);
	ctx->flags &= ~ALLOC_SSBOS;
}

static void
do_compute_shader(BeamformerCtx *ctx, enum compute_shaders shader)
{
	ComputeShaderCtx *csctx = &ctx->csctx;

	glBeginQuery(GL_TIME_ELAPSED, csctx->timer_ids[shader]);

	glUseProgram(csctx->programs[shader]);

	glBindBufferBase(GL_UNIFORM_BUFFER, 0, csctx->shared_ubo);

	u32 output_ssbo_idx = !csctx->last_output_ssbo_index;
	u32 input_ssbo_idx  = csctx->last_output_ssbo_index;
	switch (shader) {
	case CS_HADAMARD:
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, csctx->raw_data_ssbo);
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, csctx->rf_data_ssbos[output_ssbo_idx]);
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 3, csctx->hadamard_ssbo);
		glDispatchCompute(ORONE(csctx->dec_data_dim.x / 32),
		                  ORONE(csctx->dec_data_dim.y / 32),
		                  ORONE(csctx->dec_data_dim.z));
		csctx->last_output_ssbo_index = !csctx->last_output_ssbo_index;
		break;
	case CS_LPF:
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, csctx->rf_data_ssbos[input_ssbo_idx]);
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, csctx->rf_data_ssbos[output_ssbo_idx]);
		#if 0
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 3, csctx->lpf_ssbo);
		glUniform1i(csctx->lpf_order_id, csctx->lpf_order);
		#endif
		glDispatchCompute(ORONE(csctx->dec_data_dim.x / 32),
		                  ORONE(csctx->dec_data_dim.y / 32),
		                  ORONE(csctx->dec_data_dim.z));
		csctx->last_output_ssbo_index = !csctx->last_output_ssbo_index;
		break;
	case CS_MIN_MAX:
		glBindImageTexture(ctx->out_texture_unit, ctx->out_texture, 0, GL_FALSE, 0,
		                   GL_WRITE_ONLY, GL_RG32F);
		glUniform1i(csctx->out_data_tex_id, ctx->out_texture_unit);
		for (u32 i = 1; i < ctx->out_texture_mips; i++) {
			u32 otu = ctx->out_texture_unit;
			glBindImageTexture(otu + 1, ctx->out_texture, i - 1,
			                   GL_FALSE, 0, GL_READ_ONLY, GL_RG32F);
			glBindImageTexture(otu + 2, ctx->out_texture, i,
			                   GL_FALSE, 0, GL_WRITE_ONLY, GL_RG32F);
			glUniform1i(csctx->out_data_tex_id, otu + 1);
			glUniform1i(csctx->mip_view_tex_id, otu + 2);
			glUniform1i(csctx->mips_level_id, i);

			u32 width  = ctx->out_data_dim.x >> i;
			u32 height = ctx->out_data_dim.y >> i;
			u32 depth  = ctx->out_data_dim.z >> i;
			glDispatchCompute(ORONE(width / 32), ORONE(height / 32), ORONE(depth));
			glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
		}
		break;
	case CS_UFORCES:
		glActiveTexture(GL_TEXTURE0 + ctx->out_texture_unit);
		glBindTexture(GL_TEXTURE_3D, ctx->out_texture);
		glBindImageTexture(ctx->out_texture_unit, ctx->out_texture, 0, GL_FALSE, 0,
		                   GL_WRITE_ONLY, GL_RG32F);
		glUniform1i(csctx->out_data_tex_id, ctx->out_texture_unit);
		glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, csctx->rf_data_ssbos[input_ssbo_idx]);
		glDispatchCompute(ORONE(ctx->out_data_dim.x / 32),
		                  ORONE(ctx->out_data_dim.y / 32),
		                  ORONE(ctx->out_data_dim.z));
		break;
	default: ASSERT(0);
	}

	glEndQuery(GL_TIME_ELAPSED);
}

static Color
colour_from_normalized(v4 rgba)
{
	return (Color){.r = rgba.r * 255.0f, .g = rgba.g * 255.0f,
	               .b = rgba.b * 255.0f, .a = rgba.a * 255.0f};
}

static f32
move_towards_f32(f32 current, f32 target, f32 delta)
{
	if (target < current) {
		current -= delta;
		if (current < target)
			current = target;
	} else {
		current += delta;
		if (current > target)
			current = target;
	}
	return current;
}

static v4
lerp_v4(v4 a, v4 b, f32 t)
{
	return (v4){
		.x = a.x + t * (b.x - a.x),
		.y = a.y + t * (b.y - a.y),
		.z = a.z + t * (b.z - a.z),
		.w = a.w + t * (b.w - a.w),
	};
}

static void
do_text_input(BeamformerCtx *ctx, i32 max_chars, Rect r, Color colour)
{
	v2 ts  = {.rl = MeasureTextEx(ctx->font, ctx->is.buf, ctx->font_size, 0)};
	v2 pos = {.x = r.pos.x, .y = r.pos.y + (r.size.y - ts.y) / 2};

	i32 buf_delta = ctx->is.buf_len - max_chars;
	if (buf_delta < 0) buf_delta = 0;
	char *buf     = ctx->is.buf + buf_delta;
	DrawTextEx(ctx->font, buf, pos.rl, ctx->font_size, 0, colour);

	ctx->is.cursor_blink_t = move_towards_f32(ctx->is.cursor_blink_t,
	                                          ctx->is.cursor_blink_target, 1.5 * ctx->dt);
	if (ctx->is.cursor_blink_t == ctx->is.cursor_blink_target) {
		if (ctx->is.cursor_blink_target == 0) ctx->is.cursor_blink_target = 1;
		else                                  ctx->is.cursor_blink_target = 0;
	}

	v4 bg = FOCUSED_COLOUR;
	bg.a  = 0;
	Color cursor_colour = colour_from_normalized(lerp_v4(bg, FOCUSED_COLOUR,
	                                                     ctx->is.cursor_blink_t));

	/* NOTE: guess a cursor position */
	if (ctx->is.cursor == -1) {
		f32 x_off = TEXT_BOX_EXTRA_X, x_bounds = r.size.w * ctx->is.cursor_hover_p;
		u32 i;
		for (i = 0; i < ctx->is.buf_len && x_off < x_bounds; i++) {
			u32 idx = GetGlyphIndex(ctx->font, ctx->is.buf[i]);
			if (ctx->font.glyphs[idx].advanceX == 0)
				x_off += ctx->font.recs[idx].width;
			else
				x_off += ctx->font.glyphs[idx].advanceX;
		}
		ctx->is.cursor = i;
	}

	/* NOTE: Braindead NULL termination stupidity */
	char saved_c = buf[ctx->is.cursor - buf_delta];
	buf[ctx->is.cursor - buf_delta] = 0;

	v2 sts           = {.rl = MeasureTextEx(ctx->font, buf, ctx->font_size, 0)};
	f32 cursor_x     = r.pos.x + sts.x;
	f32 cursor_width = ctx->is.cursor == ctx->is.buf_len ? 20 : 6;

	buf[ctx->is.cursor - buf_delta] = saved_c;

	Rect cursor_r = {
		.pos  = {.x = cursor_x,     .y = pos.y},
		.size = {.w = cursor_width, .h = ts.h},
	};

	DrawRectangleRec(cursor_r.rl, cursor_colour);

	/* NOTE: handle multiple input keys on a single frame */
	i32 key = GetCharPressed();
	while (key > 0) {
		if (ctx->is.buf_len == (ARRAY_COUNT(ctx->is.buf) - 1)) {
			ctx->is.buf[ARRAY_COUNT(ctx->is.buf) - 1] = 0;
			break;
		}

		b32 allow_key = ((key >= '0' && key <= '9') || (key == '.') ||
		                 (key == '-' && ctx->is.cursor == 0));
		if (allow_key) {
			/* TODO: remove memmove */
			memmove(ctx->is.buf + ctx->is.cursor + 1,
			        ctx->is.buf + ctx->is.cursor,
			        ctx->is.buf_len - ctx->is.cursor + 1);

			ctx->is.buf[ctx->is.cursor++] = key;
			ctx->is.buf_len++;
		}
		key = GetCharPressed();
	}

	if ((IsKeyPressed(KEY_LEFT) || IsKeyPressedRepeat(KEY_LEFT)) && ctx->is.cursor > 0)
		ctx->is.cursor--;

	if ((IsKeyPressed(KEY_RIGHT) || IsKeyPressedRepeat(KEY_RIGHT)) &&
	    ctx->is.cursor < ctx->is.buf_len)
		ctx->is.cursor++;

	if ((IsKeyPressed(KEY_BACKSPACE) || IsKeyPressedRepeat(KEY_BACKSPACE)) &&
	    ctx->is.cursor > 0) {
		/* TODO: remove memmove */
		ctx->is.cursor--;
		memmove(ctx->is.buf + ctx->is.cursor,
		        ctx->is.buf + ctx->is.cursor + 1,
			ctx->is.buf_len - ctx->is.cursor - 1);
		ctx->is.buf[--ctx->is.buf_len] = 0;
	}
}

struct listing {
	char *prefix;
	char *suffix;
	f32  *data;
	v2   limits;
	f32  data_scale;
	b32  editable;
};

static void
parse_and_store_text_input(BeamformerCtx *ctx, struct listing *l)
{
	f32 new_val = strtof(ctx->is.buf, NULL);
	/* TODO: allow zero for certain listings only */
	if (new_val / l->data_scale != *l->data) {
		*l->data = new_val / l->data_scale;
		CLAMP(*l->data, l->limits.x, l->limits.y);
		ctx->flags |= DO_COMPUTE;
		ctx->params->upload = 1;
	}
}

static void
set_text_input_idx(BeamformerCtx *ctx, i32 idx, struct listing *l, struct listing *last_l, Rect r,
                   v2 mouse)
{
	if (ctx->is.idx != idx && ctx->is.idx != -1)
		parse_and_store_text_input(ctx, last_l);

	ctx->is.buf_len = snprintf(ctx->is.buf, ARRAY_COUNT(ctx->is.buf), "%0.02f",
	                           *l->data * l->data_scale);

	ctx->is.idx    = idx;
	ctx->is.cursor = -1;

	if (ctx->is.idx == -1)
		return;

	ASSERT(CheckCollisionPointRec(mouse.rl, r.rl));
	ctx->is.cursor_hover_p = (mouse.x - r.pos.x) / r.size.w;
	CLAMP01(ctx->is.cursor_hover_p);
}

static void
draw_settings_ui(BeamformerCtx *ctx, Arena arena, Rect r, v2 mouse)
{
	BeamformerParameters *bp = &ctx->params->raw;

	f32 minx = bp->output_min_xz.x + 1e-6, maxx = bp->output_max_xz.x - 1e-6;
	f32 minz = bp->output_min_xz.y + 1e-6, maxz = bp->output_max_xz.y - 1e-6;
	struct listing listings[] = {
		{ "Sampling Rate:",    "[MHz]", &bp->sampling_frequency, {{0,    0}},     1e-6, 0 },
		{ "Center Frequency:", "[MHz]", &bp->center_frequency,   {{0,    100e6}}, 1e-6, 1 },
		{ "Speed of Sound:",   "[m/s]", &bp->speed_of_sound,     {{0,    1e6}},   1,    1 },
		{ "Min X Point:",      "[mm]",  &bp->output_min_xz.x,    {{-1e3, maxx}},  1e3,  1 },
		{ "Max X Point:",      "[mm]",  &bp->output_max_xz.x,    {{minx, 1e3}},   1e3,  1 },
		{ "Min Z Point:",      "[mm]",  &bp->output_min_xz.y,    {{0,    maxz}},  1e3,  1 },
		{ "Max Z Point:",      "[mm]",  &bp->output_max_xz.y,    {{minz, 1e3}},   1e3,  1 },
		{ "Dynamic Range:",    "[dB]",  &ctx->fsctx.db,          {{-120, 0}},     1,    1 },
	};

	static f32 hover_t[ARRAY_COUNT(listings)];

	f32 line_pad  = 10;

	v2 pos  = r.pos;
	pos.y  += 50;
	pos.x  += 20;

	s8 txt = s8alloc(&arena, 64);
	f32 max_prefix_len = 0;
	for (i32 i = 0; i < ARRAY_COUNT(listings); i++) {
		struct listing *l = listings + i;
		DrawTextEx(ctx->font, l->prefix, pos.rl, ctx->font_size, ctx->font_spacing,
		           colour_from_normalized(FG_COLOUR));
		v2 prefix_s = {.rl = MeasureTextEx(ctx->font, l->prefix, ctx->font_size,
		                                   ctx->font_spacing)};
		if (prefix_s.w > max_prefix_len)
			max_prefix_len = prefix_s.w;
		pos.y += prefix_s.y + line_pad;
	}
	pos.y = 50 + r.pos.y;

	for (i32 i = 0; i < ARRAY_COUNT(listings); i++) {
		struct listing *l = listings + i;

		v2 txt_s;
		if (ctx->is.idx == i) {
			txt_s.rl = MeasureTextEx(ctx->font, ctx->is.buf, ctx->font_size,
			                         ctx->font_spacing);
		} else {
			snprintf((char *)txt.data, txt.len, "%0.02f", *l->data * l->data_scale);
			txt_s.rl = MeasureTextEx(ctx->font, (char *)txt.data, ctx->font_size,
			                         ctx->font_spacing);
		}

		Rect edit_rect = {
			.pos  = {.x = pos.x + max_prefix_len + 15, .y = pos.y},
			.size = {.x = txt_s.w + TEXT_BOX_EXTRA_X,  .y = txt_s.h}
		};

		b32 collides = CheckCollisionPointRec(mouse.rl, edit_rect.rl);
		if (collides && l->editable) {
			f32 mouse_scroll = GetMouseWheelMove();
			if (mouse_scroll) {
				*l->data += mouse_scroll / l->data_scale;
				CLAMP(*l->data, l->limits.x, l->limits.y);
				ctx->flags |= DO_COMPUTE;
				ctx->params->upload = 1;
			}
		}

		if (collides && ctx->is.idx != i && l->editable)
			hover_t[i] += TEXT_HOVER_SPEED * ctx->dt;
		else
			hover_t[i] -= TEXT_HOVER_SPEED * ctx->dt;
		CLAMP01(hover_t[i]);

		if (!collides && ctx->is.idx == i && IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
			set_text_input_idx(ctx, -1, l, l, (Rect){0}, mouse);
			snprintf((char *)txt.data, txt.len, "%0.02f", *l->data * l->data_scale);
		}

		if (collides && l->editable && IsMouseButtonPressed(MOUSE_BUTTON_LEFT))
			set_text_input_idx(ctx, i, l, listings + ctx->is.idx, edit_rect, mouse);

		Color colour = colour_from_normalized(lerp_v4(FG_COLOUR, HOVERED_COLOUR, hover_t[i]));

		if (ctx->is.idx != i) {
			DrawTextEx(ctx->font, (char *)txt.data, edit_rect.pos.rl, ctx->font_size,
			           ctx->font_spacing, colour);
		} else {
			do_text_input(ctx, 7, edit_rect, colour);
		}

		v2 suffix_s = {.rl = MeasureTextEx(ctx->font, l->suffix, ctx->font_size,
		                                   ctx->font_spacing)};
		v2 suffix_p = {.x = r.pos.x + r.size.w - suffix_s.w, .y = pos.y};
		DrawTextEx(ctx->font, l->suffix, suffix_p.rl, ctx->font_size, ctx->font_spacing,
		           colour_from_normalized(FG_COLOUR));
		pos.y += txt_s.y + line_pad;
	}

	if (IsKeyPressed(KEY_ENTER) && ctx->is.idx != -1) {
		struct listing *l = listings + ctx->is.idx;
		set_text_input_idx(ctx, -1, l, l, (Rect){0}, mouse);
	}
}

static void
draw_debug_overlay(BeamformerCtx *ctx, Arena arena, Rect r)
{
	DrawFPS(20, 20);

	uv2 ws = ctx->window_size;
	u32 fontsize  = ctx->font_size;
	u32 fontspace = ctx->font_spacing;

	static char *labels[CS_LAST] = {
		[CS_HADAMARD] = "Decoding:",
		[CS_LPF]      = "LPF:",
		[CS_MIN_MAX]  = "Min/Max:",
		[CS_UFORCES]  = "UFORCES:",
	};

	ComputeShaderCtx *cs = &ctx->csctx;

	s8 txt_buf = s8alloc(&arena, 64);
	v2 pos = {.x = 20, .y = ws.h - 10};
	for (u32 i = 0; i < CS_LAST; i++) {
		v2 txt_fs  = {.rl = MeasureTextEx(ctx->font, labels[i], fontsize, fontspace)};
		pos.y     -= txt_fs.y;

		DrawTextEx(ctx->font, labels[i], pos.rl, fontsize, fontspace,
		           colour_from_normalized(FG_COLOUR));

		snprintf((char *)txt_buf.data, txt_buf.len, "%0.02e [s]", cs->last_frame_time[i]);
		txt_fs.rl = MeasureTextEx(ctx->font, (char *)txt_buf.data, fontsize, fontspace);
		v2 rpos   = {.x = r.pos.x + r.size.w - txt_fs.w, .y = pos.y};
		DrawTextEx(ctx->font, (char *)txt_buf.data, rpos.rl, fontsize, fontspace,
		           colour_from_normalized(FG_COLOUR));
	}

	{
		static v2 pos       = {.x = 32,  .y = 128};
		static v2 scale     = {.x = 1.0, .y = 1.0};
		static u32 txt_idx  = 0;
		static char *txt[2] = { "-_-", "^_^" };
		static v2 ts[2];
		if (ts[0].x == 0) {
			ts[0] = (v2){ .rl = MeasureTextEx(ctx->font, txt[0], fontsize, fontspace) };
			ts[1] = (v2){ .rl = MeasureTextEx(ctx->font, txt[1], fontsize, fontspace) };
		}

		pos.x += 130 * ctx->dt * scale.x;
		pos.y += 120 * ctx->dt * scale.y;

		if (pos.x > (ws.w - ts[txt_idx].x) || pos.x < 0) {
			txt_idx = !txt_idx;
			CLAMP(pos.x, 0, ws.w - ts[txt_idx].x);
			scale.x *= -1.0;
		}

		if (pos.y > (ws.h - ts[txt_idx].y) || pos.y < 0) {
			txt_idx = !txt_idx;
			CLAMP(pos.y, 0, ws.h - ts[txt_idx].y);
			scale.y *= -1.0;
		}

		DrawTextEx(ctx->font, txt[txt_idx], pos.rl, fontsize, fontspace, RED);
	}
}


DEBUG_EXPORT void
do_beamformer(BeamformerCtx *ctx, Arena arena)
{
	ctx->dt = GetFrameTime();

	if (IsWindowResized()) {
		ctx->window_size.h = GetScreenHeight();
		ctx->window_size.w = GetScreenWidth();
	}

	/* NOTE: Store the compute time for the last frame. */
	{
		i32 timer_status, _unused;
		glGetSynciv(ctx->csctx.timer_fence, GL_SYNC_STATUS, 4, &_unused, &timer_status);
		if (timer_status == GL_SIGNALED) {
			for (u32 i = 0; i < ARRAY_COUNT(ctx->csctx.timer_ids); i++) {
				u64 ns = 0;
				glGetQueryObjectui64v(ctx->csctx.timer_ids[i], GL_QUERY_RESULT, &ns);
				ctx->csctx.last_frame_time[i] = (f32)ns / 1e9;
			}
		}
	}

	BeamformerParameters *bp = &ctx->params->raw;
	/* NOTE: Check for and Load RF Data into GPU */
	if (os_poll_pipe(ctx->data_pipe)) {
		if (!uv4_equal(ctx->csctx.dec_data_dim, bp->dec_data_dim) || ctx->flags & ALLOC_SSBOS)
			alloc_shader_storage(ctx, arena);
		if (!uv4_equal(ctx->out_data_dim, bp->output_points) || ctx->flags & ALLOC_OUT_TEX)
			alloc_output_image(ctx);

		uv2  rf_raw_dim   = ctx->csctx.rf_raw_dim;
		size rf_raw_size  = rf_raw_dim.x * rf_raw_dim.y * sizeof(i16);
		glBindBuffer(GL_SHADER_STORAGE_BUFFER, ctx->csctx.raw_data_ssbo);
		glBufferData(GL_SHADER_STORAGE_BUFFER, rf_raw_size, 0, GL_STREAM_DRAW);
		void *rf_data_buf = glMapBuffer(GL_SHADER_STORAGE_BUFFER, GL_WRITE_ONLY);
		ASSERT(rf_data_buf);
		size rlen = os_read_pipe_data(ctx->data_pipe, rf_data_buf, rf_raw_size);
		glUnmapBuffer(GL_SHADER_STORAGE_BUFFER);

		if (rlen == rf_raw_size) ctx->flags |= DO_COMPUTE;
		else                     ctx->partial_transfer_count++;
	}

	if (ctx->flags & UPLOAD_FILTER)
		upload_filter_coefficients(ctx, arena);

	if (ctx->flags & DO_COMPUTE) {
		if (ctx->params->upload) {
			glBindBuffer(GL_UNIFORM_BUFFER, ctx->csctx.shared_ubo);
			void *ubo = glMapBuffer(GL_UNIFORM_BUFFER, GL_WRITE_ONLY);
			mem_copy((s8){.data = (u8 *)bp,  .len = sizeof(*bp)},
			         (s8){.data = (u8 *)ubo, .len = sizeof(*bp)});
			glUnmapBuffer(GL_UNIFORM_BUFFER);
			ctx->params->upload = 0;
		}
		do_compute_shader(ctx, CS_HADAMARD);
		do_compute_shader(ctx, CS_LPF);
		do_compute_shader(ctx, CS_UFORCES);
		do_compute_shader(ctx, CS_MIN_MAX);
		ctx->flags &= ~DO_COMPUTE;
		glDeleteSync(ctx->csctx.timer_fence);
		ctx->csctx.timer_fence = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
	}

	/* NOTE: draw output image texture using render fragment shader */
	BeginTextureMode(ctx->fsctx.output);
		ClearBackground(PINK);
		BeginShaderMode(ctx->fsctx.shader);
			FragmentShaderCtx *fs = &ctx->fsctx;
			glUseProgram(fs->shader.id);
			glUniform1i(fs->out_data_tex_id, ctx->out_texture_unit);
			glUniform1f(fs->db_cutoff_id, fs->db);
			DrawTexture(fs->output.texture, 0, 0, WHITE);
		EndShaderMode();
	EndTextureMode();

	/* NOTE: Draw UI */
	BeginDrawing();
		ClearBackground(colour_from_normalized(BG_COLOUR));

		Texture *output   = &ctx->fsctx.output.texture;

		v2 output_dim = {
			.x = bp->output_max_xz.x - bp->output_min_xz.x,
			.y = bp->output_max_xz.y - bp->output_min_xz.y,
		};

		v2 line_step_mm = {.x = 3, .y = 5};
		uv2 line_count  = {
			.x = ABS(output_dim.x) * 1e3/line_step_mm.x + 1,
			.y = ABS(output_dim.y) * 1e3/line_step_mm.y + 1,
		};

		s8 txt = s8alloc(&arena, 64);
		snprintf((char *)txt.data, txt.len, "%+0.01f mm", -88.8f);
		v2 txt_s = {.rl = MeasureTextEx(ctx->font, (char *)txt.data,
		                                ctx->font_size, ctx->font_spacing)};

		Rect wr = {.size = {.w = (f32)ctx->window_size.w, .h = (f32)ctx->window_size.h}};
		Rect lr = wr, rr = wr;
		lr.size.w = 420;

		rr.size.w  = wr.size.w - lr.size.w;
		rr.pos.x   = lr.pos.x  + lr.size.w;

		rr.pos.x  += 0.07 * rr.size.w;
		rr.size.w *= 0.86;
		rr.pos.y  += 0.02 * rr.size.h;
		rr.size.h *= 0.96;

		f32 tick_len = 20;
		f32 x_pad    = 1.0  * txt_s.x + tick_len + 0.5 * txt_s.y;
		f32 y_pad    = 1.25 * txt_s.x + tick_len;

		Rect vr    = rr;
		vr.size.h -= y_pad;
		vr.size.w  = vr.size.h * output_dim.w / output_dim.h - x_pad;
		if (vr.size.w + x_pad > rr.size.w) {
			vr.size.h = (rr.size.w - x_pad) * output_dim.h / output_dim.w;
			vr.size.w = vr.size.h * output_dim.w / output_dim.h;
		}
		vr.pos.x += (rr.size.w - (vr.size.w + x_pad) + txt_s.h) / 2;
		vr.pos.y += (rr.size.h - (vr.size.h + y_pad) + txt_s.h) / 2;

		Rectangle tex_r   = { 0.0f, 0.0f, (f32)output->width, -(f32)output->height };
		NPatchInfo tex_np = { tex_r, 0, 0, 0, 0, NPATCH_NINE_PATCH };
		DrawTextureNPatch(*output, tex_np, vr.rl, (Vector2){0}, 0, WHITE);

		/* NOTE: check mouse wheel for adjusting dynamic range of image */
		v2 mouse = { .rl = GetMousePosition() };
		if (CheckCollisionPointRec(mouse.rl, vr.rl)) {
			ctx->fsctx.db += GetMouseWheelMove();
			CLAMP(ctx->fsctx.db, -120, 0);
		}

		static f32 txt_colour_t[2];
		for (u32 i = 0; i < 2; i++) {
			f32 inc          = vr.size.E[i] / (line_count.E[i] - 1);
			v2 start_pos     = vr.pos;
			start_pos.E[!i] += vr.size.E[!i];

			v2 end_pos       = start_pos;
			end_pos.E[!i]   += tick_len;

			/* NOTE: Center the Text with the Tick center */
			f32 txt_pos_scale[2] = {1, -1};
			v2 txt_pos  = end_pos;
			txt_pos.E[i]  += txt_pos_scale[i] * txt_s.y/2;
			txt_pos.E[!i] += 10;

			Rect tick_rect       = {.pos = start_pos, .size = vr.size};
			tick_rect.size.E[!i] = 10 + tick_len + txt_s.x;

			if (CheckCollisionPointRec(mouse.rl, tick_rect.rl)) {
				f32 scale[2]   = {0.5e-3, 1e-3};
				f32 size_delta = GetMouseWheelMove() * scale[i];
				/* TODO: smooth scroll this? */
				if (i == 0)
					bp->output_min_xz.E[i] -= size_delta;
				bp->output_max_xz.E[i] += size_delta;
				if (size_delta) {
					ctx->flags |= DO_COMPUTE;
					ctx->params->upload = 1;
				}

				txt_colour_t[i] += TEXT_HOVER_SPEED * ctx->dt;
			} else {
				txt_colour_t[i] -= TEXT_HOVER_SPEED * ctx->dt;
			}
			CLAMP01(txt_colour_t[i]);

			f32 mm     = bp->output_min_xz.E[i] * 1e3;
			f32 mm_inc = inc * output_dim.E[i] * 1e3 / vr.size.E[i];

			Color txt_colour = colour_from_normalized(lerp_v4(FG_COLOUR, HOVERED_COLOUR,
			                                                  txt_colour_t[i]));

			char *fmt[2] = {"%+0.01f mm", "%0.01f mm"};
			f32 rot[2] = {90, 0};
			for (u32 j = 0; j < line_count.E[i]; j++) {
				DrawLineEx(start_pos.rl, end_pos.rl, 3, colour_from_normalized(FG_COLOUR));
				snprintf((char *)txt.data, txt.len, fmt[i], mm);
				DrawTextPro(ctx->font, (char *)txt.data, txt_pos.rl, (Vector2){0},
				            rot[i], ctx->font_size, ctx->font_spacing, txt_colour);
				start_pos.E[i] += inc;
				end_pos.E[i]   += inc;
				txt_pos.E[i]   += inc;
				mm             += mm_inc;
			}
		}

		f32 desired_width = lr.size.w + vr.size.w;
		if (desired_width > ctx->window_size.w) {
			ctx->window_size.w = desired_width;
			SetWindowSize(ctx->window_size.w, ctx->window_size.h);
			SetWindowMinSize(desired_width, 720);
		}

		draw_settings_ui(ctx, arena, lr, mouse);
		draw_debug_overlay(ctx, arena, lr);
	EndDrawing();

	if (IsKeyPressed(KEY_R))
		ctx->flags |= RELOAD_SHADERS;
}
