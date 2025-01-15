/* See LICENSE for license details. */
static Color
colour_from_normalized(v4 rgba)
{
	return (Color){.r = rgba.r * 255.0f, .g = rgba.g * 255.0f,
	               .b = rgba.b * 255.0f, .a = rgba.a * 255.0f};
}

static Color
fade(Color a, f32 alpha)
{
	a.a = (u8)((f32)a.a * alpha);
	return a;
}

static f32
lerp(f32 a, f32 b, f32 t)
{
	return a + t * (b - a);
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

static v2
measure_text(Font font, s8 text)
{
	v2 result = {.y = font.baseSize};
	for (size i = 0; i < text.len; i++) {
		/* NOTE: assumes font glyphs are ordered ASCII */
		i32 idx   = (i32)text.data[i] - 0x20;
		result.x += font.glyphs[idx].advanceX;
		if (font.glyphs[idx].advanceX == 0)
			result.x += (font.recs[idx].width + font.glyphs[idx].offsetX);
	}
	return result;
}

static v2
draw_text(Font font, s8 text, v2 pos, f32 rotation, Color colour)
{
	rlPushMatrix();

	rlTranslatef(pos.x, pos.y, 0);
	rlRotatef(rotation, 0, 0, 1);

	v2 off = {0};
	for (size i = 0; i < text.len; i++) {
		/* NOTE: assumes font glyphs are ordered ASCII */
		i32 idx = text.data[i] - 0x20;
		Rectangle dst = {
			off.x + font.glyphs[idx].offsetX - font.glyphPadding,
			off.y + font.glyphs[idx].offsetY - font.glyphPadding,
			font.recs[idx].width  + 2.0f * font.glyphPadding,
			font.recs[idx].height + 2.0f * font.glyphPadding
		};
		Rectangle src = {
			font.recs[idx].x - font.glyphPadding,
			font.recs[idx].y - font.glyphPadding,
			font.recs[idx].width  + 2.0f * font.glyphPadding,
			font.recs[idx].height + 2.0f * font.glyphPadding
		};
		DrawTexturePro(font.texture, src, dst, (Vector2){0}, 0, colour);

		off.x += font.glyphs[idx].advanceX;
		if (font.glyphs[idx].advanceX == 0)
			off.x += font.recs[idx].width;
	}
	rlPopMatrix();
	v2 result = {.x = off.x, .y = font.baseSize};
	return result;
}

static Rect
scale_rect_centered(Rect r, v2 scale)
{
	Rect or   = r;
	r.size.w *= scale.x;
	r.size.h *= scale.y;
	r.pos.x  += (or.size.w - r.size.w) / 2;
	r.pos.y  += (or.size.h - r.size.h) / 2;
	return r;
}

static v2
center_align_text_in_rect(Rect r, s8 text, Font font)
{
	v2 ts    = measure_text(font, text);
	v2 delta = { .w = r.size.w - ts.w, .h = r.size.h - ts.h };
	return (v2) {
		.x = r.pos.x + 0.5 * delta.w,
	        .y = r.pos.y + 0.5 * delta.h,
	};
}

static b32
hover_text(v2 mouse, Rect text_rect, f32 *hover_t, b32 can_advance)
{
	b32 hovering = CheckCollisionPointRec(mouse.rl, text_rect.rl);
	if (hovering && can_advance) *hover_t += TEXT_HOVER_SPEED * dt_for_frame;
	else                         *hover_t -= TEXT_HOVER_SPEED * dt_for_frame;
	*hover_t = CLAMP01(*hover_t);
	return hovering;
}

/* TODO(rnp): once this has more callers decide if it would be better for this to take
 * an orientation rather than force CCW/right-handed */
static void
draw_ruler(BeamformerUI *ui, Stream *buf, v2 start_point, v2 end_point,
           f32 start_value, f32 end_value, f32 *markers, u32 marker_count,
           u32 segments, s8 suffix, Color ruler_colour, Color txt_colour)
{
	b32 draw_plus = SIGN(start_value) != SIGN(end_value);

	end_point    = sub_v2(end_point, start_point);
	f32 rotation = atan2_f32(end_point.y, end_point.x) * 180 / PI;

	rlPushMatrix();
	rlTranslatef(start_point.x, start_point.y, 0);
	rlRotatef(rotation, 0, 0, 1);

	f32 inc       = magnitude_v2(end_point) / segments;
	f32 value_inc = (end_value - start_value) / segments;
	f32 value     = start_value;

	v2 sp = {0}, ep = {.y = RULER_TICK_LENGTH};
	v2 tp = {.x = ui->small_font_height / 2, .y = ep.y + RULER_TEXT_PAD};
	for (u32 j = 0; j <= segments; j++) {
		DrawLineEx(sp.rl, ep.rl, 3, ruler_colour);

		buf->widx = 0;
		if (draw_plus && value > 0) stream_append_byte(buf, '+');
		stream_append_f64(buf, value, 10);
		stream_append_s8(buf, suffix);
		draw_text(ui->small_font, stream_to_s8(buf), tp, 90, txt_colour);

		value += value_inc;
		sp.x  += inc;
		ep.x  += inc;
		tp.x  += inc;
	}

	ep.y += RULER_TICK_LENGTH;
	for (u32 i = 0; i < marker_count; i++) {
		if (markers[i] < F32_INFINITY) {
			ep.x  = sp.x = markers[i];
			DrawLineEx(sp.rl, ep.rl, 3, colour_from_normalized(RULER_COLOUR));
			DrawCircleV(ep.rl, 3, colour_from_normalized(RULER_COLOUR));
		}
	}

	rlPopMatrix();
}

static void
do_scale_bar(BeamformerUI *ui, Stream *buf, Variable var, v2 mouse, i32 direction, Rect draw_rect,
             f32 start_value, f32 end_value, s8 suffix)
{
	InteractionState *is = &ui->interaction;
	ScaleBar *sb         = var.store;

	v2 txt_s = measure_text(ui->small_font, s8("-288.8 mm"));

	Rect tick_rect = draw_rect;
	v2   start_pos = tick_rect.pos;
	v2   end_pos   = tick_rect.pos;
	v2   relative_mouse = sub_v2(mouse, tick_rect.pos);

	f32  markers[2];
	u32  marker_count = 1;

	u32  tick_count;
	if (direction == SB_AXIAL) {
		tick_rect.size.x  = RULER_TEXT_PAD + RULER_TICK_LENGTH + txt_s.x;
		tick_count        = tick_rect.size.y / (1.5 * ui->small_font_height);
		start_pos.y      += tick_rect.size.y;
		markers[0]        = tick_rect.size.y - sb->zoom_starting_point.y;
		markers[1]        = tick_rect.size.y - relative_mouse.y;
		sb->screen_offset = (v2){.y = tick_rect.pos.y};
		sb->screen_space_to_value = (v2){.y = (*sb->max_value - *sb->min_value) / tick_rect.size.y};
	} else {
		tick_rect.size.y  = RULER_TEXT_PAD + RULER_TICK_LENGTH + txt_s.x;
		tick_count        = tick_rect.size.x / (1.5 * ui->small_font_height);
		end_pos.x        += tick_rect.size.x;
		markers[0]        = sb->zoom_starting_point.x;
		markers[1]        = relative_mouse.x;
		/* TODO(rnp): screen space to value space transformation helper */
		sb->screen_offset = (v2){.x = tick_rect.pos.x};
		sb->screen_space_to_value = (v2){.x = (*sb->max_value - *sb->min_value) / tick_rect.size.x};
	}

	if (hover_text(mouse, tick_rect, &sb->hover_t, 1)) {
		is->hot_state = IS_SCALE_BAR;
		is->hot       = var;
		marker_count  = 2;
	}

	draw_ruler(ui, buf, start_pos, end_pos, start_value, end_value, markers, marker_count,
	           tick_count, suffix, colour_from_normalized(FG_COLOUR),
	           colour_from_normalized(lerp_v4(FG_COLOUR, HOVERED_COLOUR, sb->hover_t)));
}

static void
draw_display_overlay(BeamformerCtx *ctx, Arena a, v2 mouse, Rect display_rect, BeamformFrame *frame)
{
	BeamformerUI *ui         = ctx->ui;
	BeamformerParameters *bp = &ctx->params->raw;
	InteractionState *is     = &ui->interaction;

	Stream buf      = arena_stream(&a);
	Texture *output = &ctx->fsctx.output.texture;

	v2 txt_s = measure_text(ui->small_font, s8("-288.8 mm"));

	display_rect.pos.x  += 0.02 * display_rect.size.w;
	display_rect.pos.y  += 0.02 * display_rect.size.h;
	display_rect.size.w *= 0.96;
	display_rect.size.h *= 0.96;

	f32 pad    = 1.2 * txt_s.x + RULER_TICK_LENGTH;
	Rect vr    = display_rect;
	vr.pos.x  += 0.5 * ui->small_font_height;
	vr.pos.y  += 0.5 * ui->small_font_height;
	vr.size.h  = display_rect.size.h - pad;
	vr.size.w  = display_rect.size.w - pad;

	/* TODO(rnp): make this depend on the requested draw orientation (x-z or y-z or x-y) */
	v2 output_dim = {
		.x = frame->max_coordinate.x - frame->min_coordinate.x,
		.y = frame->max_coordinate.z - frame->min_coordinate.z,
	};
	v2 requested_dim = {
		.x = bp->output_max_coordinate.x - bp->output_min_coordinate.x,
		.y = bp->output_max_coordinate.z - bp->output_min_coordinate.z,
	};

	f32 aspect = requested_dim.h / requested_dim.w;
	if (display_rect.size.h < (vr.size.w * aspect) + pad) {
		vr.size.w = vr.size.h / aspect;
	} else {
		vr.size.h = vr.size.w * aspect;
	}
	vr.pos.x += (display_rect.size.w - (vr.size.w + pad)) / 2;
	vr.pos.y += (display_rect.size.h - (vr.size.h + pad)) / 2;

	v2 pixels_per_meter = {
		.w = (f32)output->width  / output_dim.w,
		.h = (f32)output->height / output_dim.h,
	};

	v2 texture_points  = mul_v2(pixels_per_meter, requested_dim);
	/* TODO(rnp): this also depends on x-y, y-z, x-z */
	v2 texture_start   = {
		.x = pixels_per_meter.x * 0.5 * (output_dim.x - requested_dim.x),
		.y = pixels_per_meter.y * (frame->max_coordinate.z - bp->output_max_coordinate.z),
	};

	Rectangle  tex_r  = {texture_start.x, texture_start.y, texture_points.x, -texture_points.y};
	NPatchInfo tex_np = { tex_r, 0, 0, 0, 0, NPATCH_NINE_PATCH };
	DrawTextureNPatch(*output, tex_np, vr.rl, (Vector2){0}, 0, WHITE);

	Variable var     = {.display_scale = 1e3};
	var.store        = ui->scale_bars[0] + SB_LATERAL;
	var.f32_limits   = (v2){.x = -1, .y = 1};
	var.scroll_scale = 0.5e-3;

	v2 start_pos  = vr.pos;
	start_pos.y  += vr.size.y;

	do_scale_bar(ui, &buf, var, mouse, SB_LATERAL, (Rect){.pos = start_pos, .size = vr.size},
	             bp->output_min_coordinate.x * 1e3, bp->output_max_coordinate.x * 1e3, s8(" mm"));

	var.store        = ui->scale_bars[0] + SB_AXIAL;
	var.f32_limits   = (v2){.x = 0, .y = 1};
	var.scroll_scale = 1e-3;

	start_pos    = vr.pos;
	start_pos.x += vr.size.x;

	do_scale_bar(ui, &buf, var, mouse, SB_AXIAL, (Rect){.pos = start_pos, .size = vr.size},
	             bp->output_max_coordinate.z * 1e3, bp->output_min_coordinate.z * 1e3, s8(" mm"));

	v2 pixels_to_mm = output_dim;
	pixels_to_mm.x /= vr.size.x * 1e-3;
	pixels_to_mm.y /= vr.size.y * 1e-3;

	if (CheckCollisionPointRec(mouse.rl, vr.rl)) {
		is->hot_state         = IS_DISPLAY;
		is->hot.store         = &ctx->fsctx.threshold;
		is->hot.type          = VT_F32;
		is->hot.f32_limits    = (v2){.y = 240};
		is->hot.flags         = V_GEN_MIPMAPS;
		is->hot.display_scale = 1;
		is->hot.scroll_scale  = 1;

		v2 relative_mouse = sub_v2(mouse, vr.pos);
		v2 mm = mul_v2(relative_mouse, pixels_to_mm);
		mm.x += 1e3 * bp->output_min_coordinate.x;
		mm.y += 1e3 * bp->output_min_coordinate.z;

		buf.widx = 0;
		stream_append_v2(&buf, mm);
		v2 txt_s = measure_text(ui->small_font, stream_to_s8(&buf));
		v2 txt_p = {
			.x = vr.pos.x + vr.size.w - txt_s.w - 4,
			.y = vr.pos.y + vr.size.h - txt_s.h - 4,
		};
		draw_text(ui->small_font, stream_to_s8(&buf), txt_p, 0,
		          colour_from_normalized(RULER_COLOUR));
	}

	/* TODO(rnp): store converted ruler points instead of screen points */
	if (ui->ruler_state != RS_NONE && CheckCollisionPointRec(ui->ruler_start_p.rl, vr.rl)) {
		v2 end_p;
		if (ui->ruler_state == RS_START) end_p = mouse;
		else                             end_p = ui->ruler_stop_p;

		Color colour = colour_from_normalized(RULER_COLOUR);

		end_p          = clamp_v2_rect(end_p, vr);
		v2 pixel_delta = sub_v2(ui->ruler_start_p, end_p);
		v2 mm_delta    = mul_v2(pixels_to_mm, pixel_delta);

		DrawCircleV(ui->ruler_start_p.rl, 3, colour);
		DrawLineEx(end_p.rl, ui->ruler_start_p.rl, 2, colour);
		DrawCircleV(end_p.rl, 3, colour);

		buf.widx = 0;
		stream_append_f64(&buf, magnitude_v2(mm_delta), 100);
		stream_append_s8(&buf, s8(" mm"));

		v2 txt_p = ui->ruler_start_p;
		v2 txt_s = measure_text(ui->small_font, stream_to_s8(&buf));
		if (pixel_delta.y < 0) txt_p.y -= txt_s.y;
		if (pixel_delta.x < 0) txt_p.x -= txt_s.x;
		draw_text(ui->small_font, stream_to_s8(&buf), txt_p, 0, colour);
	}
}

/* TODO(rnp): this is known after the first frame, we could unbind
 * the texture for the first draw pass or just accept a slight glitch
 * at start up (make a good default guess) */
/* NOTE: This is kinda sucks no matter how you spin it. If we want the values to be
 * left aligned in the center column we need to know the longest prefix length but
 * without either hardcoding one of the prefixes as the longest one or measuring all
 * of them we can't know this ahead of time. For now we hardcode this and manually
 * adjust when needed */
#define LISTING_LEFT_COLUMN_WIDTH 270.0f
#define LISTING_LINE_PAD           6.0f

static Rect
do_value_listing(s8 prefix, s8 suffix, Arena a, f32 value, Font font, Rect r)
{
	v2 suffix_s = measure_text(font, suffix);
	v2 suffix_p = {.x = r.pos.x + r.size.w - suffix_s.w, .y = r.pos.y};

	Stream buf = arena_stream(&a);
	stream_append_f64(&buf, value, 100);
	v2 txt_p = {.x = r.pos.x + LISTING_LEFT_COLUMN_WIDTH, .y = r.pos.y};

	draw_text(font, prefix,             r.pos,    0, colour_from_normalized(FG_COLOUR));
	draw_text(font, stream_to_s8(&buf), txt_p,    0, colour_from_normalized(FG_COLOUR));
	draw_text(font, suffix,             suffix_p, 0, colour_from_normalized(FG_COLOUR));
	r.pos.y  += suffix_s.h + LISTING_LINE_PAD;
	r.size.y -= suffix_s.h + LISTING_LINE_PAD;

	return r;
}

static Rect
do_text_input_listing(s8 prefix, s8 suffix, Variable var, BeamformerUI *ui, Rect r,
                      v2 mouse, f32 *hover_t)
{
	InputState   *is = &ui->text_input_state;
	b32 text_input_active = (ui->interaction.state == IS_TEXT) &&
	                        (var.store == ui->interaction.active.store);

	Arena  arena = ui->arena_for_frame;
	Stream buf   = arena_stream(&arena);
	v2 txt_s;

	if (text_input_active) {
		txt_s = measure_text(ui->font, (s8){.len = is->buf_len, .data = is->buf});
	} else {
		stream_append_variable(&buf, &var);
		txt_s = measure_text(ui->font, stream_to_s8(&buf));
	}

	Rect edit_rect = {
		.pos  = {.x = r.pos.x + LISTING_LEFT_COLUMN_WIDTH, .y = r.pos.y},
		.size = {.x = txt_s.w + TEXT_BOX_EXTRA_X, .y = txt_s.h}
	};

	b32 hovering = hover_text(mouse, edit_rect, hover_t, !text_input_active);
	if (hovering)
		ui->interaction.hot = var;

	/* TODO: where should this go? */
	if (text_input_active && is->cursor == -1) {
		/* NOTE: extra offset to help with putting a cursor at idx 0 */
		#define TEXT_HALF_CHAR_WIDTH 10
		f32 hover_p = CLAMP01((mouse.x - edit_rect.pos.x) / edit_rect.size.w);
		f32 x_off = TEXT_HALF_CHAR_WIDTH, x_bounds = edit_rect.size.w * hover_p;
		i32 i;
		for (i = 0; i < is->buf_len && x_off < x_bounds; i++) {
			/* NOTE: assumes font glyphs are ordered ASCII */
			i32 idx  = is->buf[i] - 0x20;
			x_off   += ui->font.glyphs[idx].advanceX;
			if (ui->font.glyphs[idx].advanceX == 0)
				x_off += ui->font.recs[idx].width;
		}
		is->cursor = i;
	}

	Color colour = colour_from_normalized(lerp_v4(FG_COLOUR, HOVERED_COLOUR, *hover_t));

	if (text_input_active) {
		s8 buf = {.len = is->buf_len, .data = is->buf};
		v2 ts  = measure_text(ui->font, buf);
		v2 pos = {.x = edit_rect.pos.x, .y = edit_rect.pos.y + (edit_rect.size.y - ts.y) / 2};

		#define MAX_DISP_CHARS 7
		i32 buf_delta = is->buf_len - MAX_DISP_CHARS;
		if (buf_delta < 0) buf_delta = 0;
		buf.len  -= buf_delta;
		buf.data += buf_delta;
		{
			/* NOTE: drop a char if the subtext still doesn't fit */
			v2 nts = measure_text(ui->font, buf);
			if (nts.w > 0.96 * edit_rect.size.w) {
				buf.data++;
				buf.len--;
			}
		}
		draw_text(ui->font, buf, pos, 0, colour);

		v4 bg = FOCUSED_COLOUR;
		bg.a  = 0;
		Color cursor_colour = colour_from_normalized(lerp_v4(bg, FOCUSED_COLOUR,
		                                                     CLAMP01(is->cursor_blink_t)));
		buf.len = is->cursor - buf_delta;
		v2 sts = measure_text(ui->font, buf);
		f32 cursor_x = pos.x + sts.x;
		f32 cursor_width;
		if (is->cursor == is->buf_len) cursor_width = 20;
		else                           cursor_width = 4;
		Rect cursor_r = {
			.pos  = {.x = cursor_x,     .y = pos.y},
			.size = {.w = cursor_width, .h = ts.h},
		};

		DrawRectanglePro(cursor_r.rl, (Vector2){0}, 0, cursor_colour);
	} else {
		draw_text(ui->font, stream_to_s8(&buf), edit_rect.pos, 0, colour);
	}

	v2 suffix_s = measure_text(ui->font, suffix);
	v2 suffix_p = {.x = r.pos.x + r.size.w - suffix_s.w, .y = r.pos.y};
	draw_text(ui->font, prefix, r.pos,    0, colour_from_normalized(FG_COLOUR));
	draw_text(ui->font, suffix, suffix_p, 0, colour_from_normalized(FG_COLOUR));

	r.pos.y  += suffix_s.h + LISTING_LINE_PAD;
	r.size.y -= suffix_s.h + LISTING_LINE_PAD;

	return r;
}

static Rect
do_text_toggle_listing(s8 prefix, s8 text0, s8 text1, Variable var,
                       BeamformerUI *ui, Rect r, v2 mouse, f32 *hover_t)
{
	b32 toggle = *(b32 *)var.store;
	v2 txt_s;
	if (toggle) txt_s = measure_text(ui->font, text1);
	else        txt_s = measure_text(ui->font, text0);

	Rect edit_rect = {
		.pos  = {.x = r.pos.x + LISTING_LEFT_COLUMN_WIDTH, .y = r.pos.y},
		.size = {.x = txt_s.w + TEXT_BOX_EXTRA_X, .y = txt_s.h}
	};

	if (hover_text(mouse, edit_rect, hover_t, 1))
		ui->interaction.hot = var;

	Color colour = colour_from_normalized(lerp_v4(FG_COLOUR, HOVERED_COLOUR, *hover_t));
	draw_text(ui->font, prefix, r.pos, 0, colour_from_normalized(FG_COLOUR));
	draw_text(ui->font, toggle? text1: text0, edit_rect.pos, 0, colour);

	r.pos.y  += txt_s.h + LISTING_LINE_PAD;
	r.size.y -= txt_s.h + LISTING_LINE_PAD;

	return r;
}

static b32
do_text_button(BeamformerUI *ui, s8 text, Rect r, v2 mouse, f32 *hover_t)
{
	b32 hovering = hover_text(mouse, r, hover_t, 1);
	b32 pressed  = 0;
	pressed     |= (hovering && IsMouseButtonPressed(MOUSE_BUTTON_LEFT));
	pressed     |= (hovering && IsMouseButtonPressed(MOUSE_BUTTON_RIGHT));

	f32 param  = lerp(1, 1.04, *hover_t);
	v2  bscale = (v2){
		.x = param + RECT_BTN_BORDER_WIDTH / r.size.w,
		.y = param + RECT_BTN_BORDER_WIDTH / r.size.h,
	};
	Rect sr    = scale_rect_centered(r, (v2){.x = param, .y = param});
	Rect sb    = scale_rect_centered(r, bscale);
	DrawRectangleRounded(sb.rl, RECT_BTN_ROUNDNESS, 0, RECT_BTN_BORDER_COLOUR);
	DrawRectangleRounded(sr.rl, RECT_BTN_ROUNDNESS, 0, RECT_BTN_COLOUR);

	v2 tpos   = center_align_text_in_rect(r, text, ui->font);
	v2 spos   = {.x = tpos.x + 1.75, .y = tpos.y + 2};
	v4 colour = lerp_v4(FG_COLOUR, HOVERED_COLOUR, *hover_t);

	draw_text(ui->font, text, spos, 0, fade(BLACK, 0.8));
	draw_text(ui->font, text, tpos, 0, colour_from_normalized(colour));

	return pressed;
}

static void
draw_settings_ui(BeamformerCtx *ctx, Rect r, v2 mouse)
{
	BeamformerUI *ui         = ctx->ui;
	BeamformerParameters *bp = &ctx->params->raw;

	f32 minx = bp->output_min_coordinate.x + 1e-6, maxx = bp->output_max_coordinate.x - 1e-6;
	f32 minz = bp->output_min_coordinate.z + 1e-6, maxz = bp->output_max_coordinate.z - 1e-6;

	Rect draw_r    = r;
	draw_r.pos.y  += 20;
	draw_r.pos.x  += 20;
	draw_r.size.x -= 20;
	draw_r.size.y -= 20;

	draw_r = do_value_listing(s8("Sampling Frequency:"), s8("[MHz]"), ui->arena_for_frame,
	                          bp->sampling_frequency * 1e-6, ui->font, draw_r);

	static f32 hover_t[15];
	i32 idx = 0;

	Variable var;

	var.store         = &bp->center_frequency;
	var.type          = VT_F32;
	var.f32_limits    = (v2){.y = 100e6};
	var.flags         = V_CAUSES_COMPUTE;
	var.display_scale = 1e-6;
	var.scroll_scale  = 1e5;
	draw_r = do_text_input_listing(s8("Center Frequency:"), s8("[MHz]"), var, ui, draw_r,
	                               mouse, hover_t + idx++);

	var.store         = &bp->speed_of_sound;
	var.type          = VT_F32;
	var.f32_limits    = (v2){.y = 1e6};
	var.flags         = V_CAUSES_COMPUTE;
	var.display_scale = 1;
	var.scroll_scale  = 10;
	draw_r = do_text_input_listing(s8("Speed of Sound:"), s8("[m/s]"), var, ui, draw_r,
	                               mouse, hover_t + idx++);

	var.store         = &bp->output_min_coordinate.x;
	var.type          = VT_F32;
	var.f32_limits    = (v2){.x = -1e3, .y = maxx};
	var.flags         = V_CAUSES_COMPUTE;
	var.display_scale = 1e3;
	var.scroll_scale  = 0.5e-3;
	draw_r = do_text_input_listing(s8("Min Lateral Point:"), s8("[mm]"), var, ui, draw_r,
	                               mouse, hover_t + idx++);

	var.store         = &bp->output_max_coordinate.x;
	var.type          = VT_F32;
	var.f32_limits    = (v2){.x = minx, .y = 1e3};
	var.flags         = V_CAUSES_COMPUTE;
	var.display_scale = 1e3;
	var.scroll_scale  = 0.5e-3;
	draw_r = do_text_input_listing(s8("Max Lateral Point:"), s8("[mm]"), var, ui, draw_r,
	                               mouse, hover_t + idx++);

	var.store         = &bp->output_min_coordinate.z;
	var.type          = VT_F32;
	var.f32_limits    = (v2){.y = maxz};
	var.flags         = V_CAUSES_COMPUTE;
	var.display_scale = 1e3;
	var.scroll_scale  = 0.5e-3;
	draw_r = do_text_input_listing(s8("Min Axial Point:"), s8("[mm]"), var, ui, draw_r,
	                               mouse, hover_t + idx++);

	var.store         = &bp->output_max_coordinate.z;
	var.type          = VT_F32;
	var.f32_limits    = (v2){.x = minz, .y = 1e3};
	var.flags         = V_CAUSES_COMPUTE;
	var.display_scale = 1e3;
	var.scroll_scale  = 0.5e-3;
	draw_r = do_text_input_listing(s8("Max Axial Point:"), s8("[mm]"), var, ui, draw_r,
	                               mouse, hover_t + idx++);

	var.store         = &bp->off_axis_pos;
	var.type          = VT_F32;
	var.f32_limits    = (v2){.x = minx, .y = maxx};
	var.flags         = V_CAUSES_COMPUTE;
	var.display_scale = 1e3;
	var.scroll_scale  = 0.5e-3;
	draw_r = do_text_input_listing(s8("Off Axis Position:"), s8("[mm]"), var, ui, draw_r,
	                               mouse, hover_t + idx++);

	var       = (Variable){0};
	var.store = &bp->beamform_plane;
	var.type  = VT_B32;
	var.flags = V_CAUSES_COMPUTE;
	draw_r = do_text_toggle_listing(s8("Beamform Plane:"), s8("XZ"), s8("YZ"), var, ui,
	                                draw_r, mouse, hover_t + idx++);

	var.store         = &bp->f_number;
	var.type          = VT_F32;
	var.f32_limits    = (v2){.y = 1e3};
	var.flags         = V_CAUSES_COMPUTE;
	var.display_scale = 1;
	var.scroll_scale  = 0.1;
	draw_r = do_text_input_listing(s8("F#:"), s8(""), var, ui, draw_r, mouse, hover_t + idx++);

	var.store         = &ctx->fsctx.db;
	var.type          = VT_F32;
	var.f32_limits    = (v2){.x = -120};
	var.flags         = V_GEN_MIPMAPS;
	var.display_scale = 1;
	var.scroll_scale  = 1;
	draw_r = do_text_input_listing(s8("Dynamic Range:"), s8("[dB]"), var, ui, draw_r,
	                               mouse, hover_t + idx++);

	var.store         = &ctx->fsctx.threshold;
	var.type          = VT_F32;
	var.f32_limits    = (v2){.y = 240};
	var.flags         = V_GEN_MIPMAPS;
	var.display_scale = 1;
	var.scroll_scale  = 1;
	draw_r = do_text_input_listing(s8("Threshold:"), s8(""), var, ui, draw_r,
	                               mouse, hover_t + idx++);

	draw_r.pos.y  += 2 * LISTING_LINE_PAD;
	draw_r.size.y -= 2 * LISTING_LINE_PAD;

	#if 0
	/* TODO: work this into the work queue */
	bmv = (BPModifiableValue){&ctx->partial_compute_ctx.volume_dim.x, bmv_store_power_of_two,
	                          .ilimits = (iv2){.x = 1, .y = ctx->gl.max_3d_texture_dim},
	                          MV_INT, 1, 1};
	draw_r = do_text_input_listing(s8("Export Dimension X:"), s8(""), bmv, ctx, arena,
	                               draw_r, mouse, hover_t + idx++);

	bmv = (BPModifiableValue){&ctx->partial_compute_ctx.volume_dim.y, bmv_store_power_of_two,
	                          .ilimits = (iv2){.x = 1, .y = ctx->gl.max_3d_texture_dim},
	                          MV_INT, 1, 1};
	draw_r = do_text_input_listing(s8("Export Dimension Y:"), s8(""), bmv, ctx, arena,
	                               draw_r, mouse, hover_t + idx++);

	bmv = (BPModifiableValue){&ctx->partial_compute_ctx.volume_dim.z, bmv_store_power_of_two,
	                          .ilimits = (iv2){.x = 1, .y = ctx->gl.max_3d_texture_dim},
	                          MV_INT, 1, 1};
	draw_r = do_text_input_listing(s8("Export Dimension Z:"), s8(""), bmv, ctx, arena,
	                               draw_r, mouse, hover_t + idx++);

	Rect btn_r = draw_r;
	btn_r.size.h  = ctx->font.baseSize * 1.3;
	btn_r.size.w *= 0.6;
	if (do_text_button(ctx, s8("Dump Raw Volume"), btn_r, mouse, hover_t + idx++)) {
		if (!ctx->partial_compute_ctx.state) {
		}
	}
	#endif

	/* NOTE: if C compilers didn't suck this would be a static assert */
	ASSERT(idx <= ARRAY_COUNT(hover_t));
}

static void
draw_debug_overlay(BeamformerCtx *ctx, Arena arena, Rect r)
{
	static s8 labels[CS_LAST] = {
		#define X(e, n, s, h, pn) [CS_##e] = s8(pn ":"),
		COMPUTE_SHADERS
		#undef X
	};

	BeamformerUI *ui     = ctx->ui;
	ComputeShaderCtx *cs = &ctx->csctx;
	uv2 ws = ctx->window_size;

	Stream buf = stream_alloc(&arena, 64);
	v2 pos     = {.x = 20, .y = ws.h - 10};

	f32 compute_time_sum = 0;
	u32 stages = ctx->params->compute_stages_count;
	for (u32 i = 0; i < stages; i++) {
		u32 index  = ctx->params->compute_stages[i];
		pos.y     -= measure_text(ui->font, labels[index]).y;
		draw_text(ui->font, labels[index], pos, 0, colour_from_normalized(FG_COLOUR));

		buf.widx = 0;
		stream_append_f64_e(&buf, cs->last_frame_time[index]);
		stream_append_s8(&buf, s8(" [s]"));
		v2 txt_fs = measure_text(ui->font, stream_to_s8(&buf));
		v2 rpos   = {.x = r.pos.x + r.size.w - txt_fs.w, .y = pos.y};
		draw_text(ui->font, stream_to_s8(&buf), rpos, 0, colour_from_normalized(FG_COLOUR));

		compute_time_sum += cs->last_frame_time[index];
	}

	static s8 totals[2] = {s8("Compute Total:"), s8("Volume Total:")};
	f32 times[2]        = {compute_time_sum, ctx->partial_compute_ctx.runtime};
	for (u32 i = 0; i < ARRAY_COUNT(totals); i++) {
		pos.y    -= measure_text(ui->font, totals[i]).y;
		draw_text(ui->font, totals[i], pos, 0, colour_from_normalized(FG_COLOUR));

		buf.widx = 0;
		stream_append_f64_e(&buf, times[i]);
		stream_append_s8(&buf, s8(" [s]"));
		v2 txt_fs = measure_text(ui->font, stream_to_s8(&buf));
		v2 rpos   = {.x = r.pos.x + r.size.w - txt_fs.w, .y = pos.y};
		draw_text(ui->font, stream_to_s8(&buf), rpos, 0, colour_from_normalized(FG_COLOUR));
	}

	{
		static v2 pos       = {.x = 32,  .y = 128};
		static v2 scale     = {.x = 1.0, .y = 1.0};
		static u32 txt_idx  = 0;
		static s8 txt[2]    = { s8("-_-"), s8("^_^") };
		static v2 ts[2];
		if (ts[0].x == 0) {
			ts[0] = measure_text(ui->font, txt[0]);
			ts[1] = measure_text(ui->font, txt[1]);
		}

		pos.x += 130 * dt_for_frame * scale.x;
		pos.y += 120 * dt_for_frame * scale.y;

		if (pos.x > (ws.w - ts[txt_idx].x) || pos.x < 0) {
			txt_idx  = !txt_idx;
			pos.x    = CLAMP(pos.x, 0, ws.w - ts[txt_idx].x);
			scale.x *= -1.0;
		}

		if (pos.y > (ws.h - ts[txt_idx].y) || pos.y < 0) {
			txt_idx  = !txt_idx;
			pos.y    = CLAMP(pos.y, 0, ws.h - ts[txt_idx].y);
			scale.y *= -1.0;
		}

		draw_text(ui->font, txt[txt_idx], pos, 0, RED);
	}
}

static void
ui_store_variable(Variable *var, void *new_value)
{
	/* TODO: special cases (eg. power of 2) */
	switch (var->type) {
	case VT_F32: {
		f32  f32_val = *(f32 *)new_value;
		f32 *f32_var = var->store;
		*f32_var     = CLAMP(f32_val, var->f32_limits.x, var->f32_limits.y);
	} break;
	case VT_I32: {
		i32  i32_val = *(i32 *)new_value;
		i32 *i32_var = var->store;
		*i32_var     = CLAMP(i32_val, var->i32_limits.x, var->i32_limits.y);
	} break;
	default: INVALID_CODE_PATH;
	}
}

static void
begin_text_input(InputState *is, Variable *var)
{
	ASSERT(var->store != NULL);

	Stream s = {.cap = ARRAY_COUNT(is->buf), .data = is->buf};
	stream_append_variable(&s, var);
	ASSERT(!s.errors);
	is->buf_len = s.widx;
	is->cursor  = -1;
}

static void
end_text_input(InputState *is, Variable *var)
{
	f32 value = parse_f64((s8){.len = is->buf_len, .data = is->buf}) / var->display_scale;
	ui_store_variable(var, &value);
}

static void
update_text_input(InputState *is)
{
	if (is->cursor == -1)
		return;

	is->cursor_blink_t += is->cursor_blink_scale * dt_for_frame;
	if (is->cursor_blink_t >= 1) is->cursor_blink_scale = -1.5f;
	if (is->cursor_blink_t <= 0) is->cursor_blink_scale =  1.5f;

	/* NOTE: handle multiple input keys on a single frame */
	i32 key = GetCharPressed();
	while (key > 0) {
		if (is->buf_len == ARRAY_COUNT(is->buf))
			break;

		b32 allow_key = ((key >= '0' && key <= '9') || (key == '.') ||
		                 (key == '-' && is->cursor == 0));
		if (allow_key) {
			mem_move(is->buf + is->cursor,
			         is->buf + is->cursor + 1,
			         is->buf_len - is->cursor + 1);

			is->buf[is->cursor++] = key;
			is->buf_len++;
		}
		key = GetCharPressed();
	}

	if ((IsKeyPressed(KEY_LEFT) || IsKeyPressedRepeat(KEY_LEFT)) && is->cursor > 0)
		is->cursor--;

	if ((IsKeyPressed(KEY_RIGHT) || IsKeyPressedRepeat(KEY_RIGHT)) && is->cursor < is->buf_len)
		is->cursor++;

	if ((IsKeyPressed(KEY_BACKSPACE) || IsKeyPressedRepeat(KEY_BACKSPACE)) && is->cursor > 0) {
		is->cursor--;
		mem_move(is->buf + is->cursor + 1,
		         is->buf + is->cursor,
		         is->buf_len - is->cursor);
		is->buf_len--;
	}

	if ((IsKeyPressed(KEY_DELETE) || IsKeyPressedRepeat(KEY_DELETE)) && is->cursor < is->buf_len) {
		mem_move(is->buf + is->cursor + 1,
		         is->buf + is->cursor,
		         is->buf_len - is->cursor);
		is->buf_len--;
	}
}

static b32
ui_can_start_compute(BeamformerCtx *ctx)
{
	BeamformFrame *displayed = ctx->beamform_frames + ctx->displayed_frame_index;
	b32 result  = ctx->beamform_work_queue.compute_in_flight == 0;
	result     &= (displayed->dim.x != 0 || displayed->dim.y != 0);
	result     &= displayed->dim.z != 0;
	return result;
}

static void
ui_start_compute(BeamformerCtx *ctx)
{
	/* NOTE: we do not allow ui to start a work if no work was previously completed */
	Arena a = {0};
	if (ui_can_start_compute(ctx)) {
		beamform_work_queue_push(ctx, &a, BW_RECOMPUTE);
		BeamformFrameIterator bfi = beamform_frame_iterator(ctx);
		for (BeamformFrame *frame = frame_next(&bfi); frame; frame = frame_next(&bfi)) {
			if (frame->dim.w && frame->textures[frame->dim.w - 1])
				glClearTexImage(frame->textures[frame->dim.w - 1], 0,
				                GL_RED, GL_FLOAT, 0);
		}
	}
	ctx->params->upload = 1;
}

static void
ui_gen_mipmaps(BeamformerCtx *ctx)
{
	if (ctx->fsctx.output.texture.id)
		ctx->fsctx.gen_mipmaps = 1;
}

static void
display_interaction_end(BeamformerUI *ui)
{
	b32 is_hot    = ui->interaction.hot_state == IS_DISPLAY;
	b32 is_active = ui->interaction.state     == IS_DISPLAY;
	if ((is_active && is_hot) || ui->ruler_state == RS_HOLD)
		return;
	ui->ruler_state = RS_NONE;
}

static void
display_interaction(BeamformerUI *ui, v2 mouse)
{
	b32 mouse_left_pressed  = IsMouseButtonPressed(MOUSE_BUTTON_LEFT);
	b32 mouse_right_pressed = IsMouseButtonPressed(MOUSE_BUTTON_RIGHT);
	b32 is_hot              = ui->interaction.hot_state == IS_DISPLAY;
	b32 is_active           = ui->interaction.state     == IS_DISPLAY;

	if (mouse_left_pressed && is_active) {
		ui->ruler_state++;
		switch (ui->ruler_state) {
		case RS_START: ui->ruler_start_p = mouse; break;
		case RS_HOLD:  ui->ruler_stop_p  = mouse; break;
		default:
			ui->ruler_state = RS_NONE;
			break;
		}
	} else if ((mouse_left_pressed && !is_hot) || (mouse_right_pressed && is_hot)) {
		ui->ruler_state = RS_NONE;
	}
}

static void
scale_bar_interaction(BeamformerCtx *ctx, v2 mouse)
{
	BeamformerUI *ui        = ctx->ui;
	InteractionState *is    = &ui->interaction;
	ScaleBar *sb            = is->active.store;
	b32 mouse_left_pressed  = IsMouseButtonPressed(MOUSE_BUTTON_LEFT);
	b32 mouse_right_pressed = IsMouseButtonPressed(MOUSE_BUTTON_RIGHT);
	f32 mouse_wheel         = GetMouseWheelMoveV().y * is->active.scroll_scale;

	if (mouse_left_pressed) {
		if (sb->zoom_starting_point.x == F32_INFINITY) {
			sb->zoom_starting_point = sub_v2(mouse, sb->screen_offset);
		} else {
			v2 relative_mouse = sub_v2(mouse, sb->screen_offset);
			f32 min = magnitude_v2(mul_v2(sb->zoom_starting_point, sb->screen_space_to_value));
			f32 max = magnitude_v2(mul_v2(relative_mouse,          sb->screen_space_to_value));
			if (min > max) { f32 tmp = min; min = max; max = tmp; }

			min += *sb->min_value;
			max += *sb->min_value;

			/* TODO(rnp): SLL_* macros */
			v2_sll *savepoint = ui->scale_bar_savepoint_freelist;
			if (!savepoint) savepoint = push_struct(&ui->arena_for_frame, v2_sll);
			ui->scale_bar_savepoint_freelist = savepoint->next;

			savepoint->v.x      = *sb->min_value;
			savepoint->v.y      = *sb->max_value;
			savepoint->next     = sb->savepoint_stack;
			sb->savepoint_stack = savepoint;

			*sb->min_value = MAX(min, is->active.f32_limits.x);
			*sb->max_value = MIN(max, is->active.f32_limits.y);

			sb->zoom_starting_point = (v2){.x = F32_INFINITY, .y = F32_INFINITY};
			ui_start_compute(ctx);
		}
	}

	if (mouse_right_pressed) {
		v2_sll *savepoint = sb->savepoint_stack;
		if (savepoint) {
			*sb->min_value = savepoint->v.x;
			*sb->max_value = savepoint->v.y;
			ui_start_compute(ctx);

			sb->savepoint_stack = savepoint->next;
			savepoint->next     = ui->scale_bar_savepoint_freelist;
			ui->scale_bar_savepoint_freelist = savepoint;
		}
		sb->zoom_starting_point = (v2){.x = F32_INFINITY, .y = F32_INFINITY};
	}

	if (mouse_wheel) {
		v2 limits = is->active.f32_limits;
		*sb->min_value -= mouse_wheel * sb->scroll_both;
		*sb->max_value += mouse_wheel;
		*sb->min_value  = MAX(limits.x, *sb->min_value);
		*sb->max_value  = MIN(limits.y, *sb->max_value);
		ui_start_compute(ctx);
	}
}

static void
ui_begin_interact(BeamformerUI *ui, BeamformerInput *input, b32 scroll, b32 mouse_left_pressed)
{
	InteractionState *is = &ui->interaction;
	if (is->hot_state != IS_NONE) {
		is->state = is->hot_state;
	} else {
		switch (is->hot.type) {
		case VT_NULL:  is->state = IS_NOP; break;
		case VT_B32:   is->state = IS_SET; break;
		case VT_GROUP: is->state = IS_SET; break;
		case VT_F32: {
			if (scroll) {
				is->state = IS_SCROLL;
			} else if (mouse_left_pressed) {
				is->state = IS_TEXT;
				begin_text_input(&ui->text_input_state, &is->hot);
			}
		} break;
		}
	}
	if (is->state != IS_NONE) {
		is->active = is->hot;
	}
}

static void
ui_end_interact(BeamformerCtx *ctx, v2 mouse)
{
	BeamformerUI *ui = ctx->ui;
	InteractionState *is = &ui->interaction;
	switch (is->state) {
	case IS_NONE: break;
	case IS_NOP:  break;
	case IS_SET: {
		switch (is->active.type) {
		case VT_B32: {
			b32 *val = is->active.store;
			*val = !(*val);
		} break;
		}
	} break;
	case IS_DISPLAY: display_interaction_end(ui); /* FALLTHROUGH */
	case IS_SCROLL: {
		f32 delta = GetMouseWheelMoveV().y * is->active.scroll_scale;
		switch (is->active.type) {
		case VT_B32: {
			b32 *old_val = is->active.store;
			b32  new_val = !(*old_val);
			ui_store_variable(&is->active, &new_val);
		} break;
		case VT_F32: {
			f32 *old_val = is->active.store;
			f32  new_val = *old_val + delta;
			ui_store_variable(&is->active, &new_val);
		} break;
		case VT_I32: {
			i32 *old_val = is->active.store;
			i32  new_val = *old_val + delta;
			ui_store_variable(&is->active, &new_val);
		} break;
		}
	} break;
	case IS_SCALE_BAR: break;
	case IS_TEXT:      end_text_input(&ui->text_input_state, &is->active); break;
	}

	if (is->active.flags & V_CAUSES_COMPUTE)
		ui_start_compute(ctx);

	if (is->active.flags & V_GEN_MIPMAPS)
		ui_gen_mipmaps(ctx);

	is->state  = IS_NONE;
	is->active = NULL_VARIABLE;
}

static void
ui_interact(BeamformerCtx *ctx, BeamformerInput *input)
{
	BeamformerUI *ui        = ctx->ui;
	InteractionState *is    = &ui->interaction;
	b32 mouse_left_pressed  = IsMouseButtonPressed(MOUSE_BUTTON_LEFT);
	b32 mouse_right_pressed = IsMouseButtonPressed(MOUSE_BUTTON_RIGHT);
	b32 wheel_moved         = GetMouseWheelMoveV().y != 0;
	if (mouse_right_pressed || mouse_left_pressed || wheel_moved) {
		if (is->state != IS_NONE)
			ui_end_interact(ctx, input->mouse);
		ui_begin_interact(ui, input, wheel_moved, mouse_left_pressed);
	}

	if (IsKeyPressed(KEY_ENTER) && is->state == IS_TEXT)
		ui_end_interact(ctx, input->mouse);

	switch (is->state) {
	case IS_DISPLAY: display_interaction(ui, input->mouse);    break;
	case IS_SCROLL:  ui_end_interact(ctx, input->mouse);       break;
	case IS_SET:     ui_end_interact(ctx, input->mouse);       break;
	case IS_TEXT:    update_text_input(&ui->text_input_state); break;
	case IS_DRAG: {
		if (IsMouseButtonReleased(MOUSE_BUTTON_LEFT)) {
			ui_end_interact(ctx, input->mouse);
		} else {
			switch (is->active.type) {
			}
		}
	} break;
	case IS_SCALE_BAR: scale_bar_interaction(ctx, input->mouse); break;
	}

	is->hot_state = IS_NONE;
	is->hot       = NULL_VARIABLE;
}

static void
ui_init(BeamformerCtx *ctx, Arena store)
{
	/* NOTE(rnp): store the ui at the base of the passed in arena and use the rest for
	 * temporary allocations within the ui. If needed we can recall this function to
	 * completely clear the ui state. The is that if we store pointers to static data
	 * such as embedded font data we will need to reset them when the executable reloads.
	 * We could also build some sort of ui structure here and store it then iterate over
	 * it to actually draw the ui. If we reload we may have changed it so we should
	 * rebuild it */

	/* NOTE: unload old fonts from the GPU */
	if (ctx->ui) {
		UnloadFont(ctx->ui->font);
		UnloadFont(ctx->ui->small_font);
	}

	BeamformerUI *ui = ctx->ui = alloc(&store, typeof(*ctx->ui), 1);
	ui->arena_for_frame = store;
	ui->frame_temporary_arena = begin_temp_arena(&ui->arena_for_frame);

	/* TODO: build these into the binary */
	ui->font       = LoadFontEx("assets/IBMPlexSans-Bold.ttf", 28, 0, 0);
	ui->small_font = LoadFontEx("assets/IBMPlexSans-Bold.ttf", 22, 0, 0);

	ui->font_height       = measure_text(ui->font, s8("8\\W")).h;
	ui->small_font_height = measure_text(ui->small_font, s8("8\\W")).h;

	/* TODO: multiple views */
	ui->scale_bars[0][SB_LATERAL].min_value = &ctx->params->raw.output_min_coordinate.x;
	ui->scale_bars[0][SB_LATERAL].max_value = &ctx->params->raw.output_max_coordinate.x;
	ui->scale_bars[0][SB_AXIAL].min_value   = &ctx->params->raw.output_min_coordinate.z;
	ui->scale_bars[0][SB_AXIAL].max_value   = &ctx->params->raw.output_max_coordinate.z;

	ui->scale_bars[0][SB_LATERAL].scroll_both = 1;
	ui->scale_bars[0][SB_AXIAL].scroll_both   = 0;

	ui->scale_bars[0][SB_LATERAL].zoom_starting_point = (v2){.x = F32_INFINITY, .y = F32_INFINITY};
	ui->scale_bars[0][SB_AXIAL].zoom_starting_point   = (v2){.x = F32_INFINITY, .y = F32_INFINITY};

}

static void
draw_ui(BeamformerCtx *ctx, BeamformerInput *input, BeamformFrame *frame_to_draw)
{
	BeamformerUI *ui = ctx->ui;

	/* TODO(rnp): we need an ALLOC_END flag so that we can have permanent storage
	 * or we need a sub arena for the save point stack */
	//end_temp_arena(ui->frame_temporary_arena);
	//ui->frame_temporary_arena = begin_temp_arena(&ui->arena_for_frame);

	/* NOTE: process interactions first because the user interacted with
	 * the ui that was presented last frame */
	ui_interact(ctx, input);

	BeginDrawing();
		ClearBackground(colour_from_normalized(BG_COLOUR));

		v2 mouse = input->mouse;
		Rect wr = {.size = {.w = (f32)ctx->window_size.w, .h = (f32)ctx->window_size.h}};
		Rect lr = wr, rr = wr;
		lr.size.w = INFO_COLUMN_WIDTH;
		rr.size.w = wr.size.w - lr.size.w;
		rr.pos.x  = lr.pos.x  + lr.size.w;

		draw_settings_ui(ctx, lr, mouse);
		if (frame_to_draw->dim.w)
			draw_display_overlay(ctx, ui->arena_for_frame, mouse, rr, frame_to_draw);
		draw_debug_overlay(ctx, ui->arena_for_frame, lr);
	EndDrawing();
}
