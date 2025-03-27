/* See LICENSE for license details. */
/* TODO(rnp):
 * [ ]: scroll bar for views that don't have enough space
 * [ ]: compute times through same path as parameter list ?
 * [ ]: global menu
 * [ ]: allow views to collapse to just their title bar
 *      - title bar struct with expanded. Check when pushing onto draw stack; if expanded
 *        do normal behaviour else make size title bar size and ignore the splits fraction.
 * [ ]: enforce a minimum region size or allow regions themselves to scroll
 * [ ]: move remaining fragment shader stuff into ui
 * [ ]: refactor: draw_left_aligned_list()
 * [ ]: refactor: draw_variable_table()
 * [ ]: refactor: add_variable_no_link()
 * [ ]: refactor: draw_text_limited should clamp to rect and measure text itself
 * [ ]: refactor: draw_active_menu should just use draw_variable_list
 * [ ]: refactor: scale bars should just be variables
 * [ ]: refactor: remove scale bar limits (limits should only prevent invalid program state)
 * [ ]: ui leaks split beamform views on hot-reload
 * [ ]: add tag based selection to frame views
 * [ ]: draw the ui with a post-order traversal instead of pre-order traversal
 * [ ]: consider V_HOVER_GROUP and use that to implement submenus
 * [ ]: bug: ruler needs to know which view is associated with it
 */

#define BG_COLOUR              (v4){.r = 0.15, .g = 0.12, .b = 0.13, .a = 1.0}
#define FG_COLOUR              (v4){.r = 0.92, .g = 0.88, .b = 0.78, .a = 1.0}
#define FOCUSED_COLOUR         (v4){.r = 0.86, .g = 0.28, .b = 0.21, .a = 1.0}
#define HOVERED_COLOUR         (v4){.r = 0.11, .g = 0.50, .b = 0.59, .a = 1.0}
#define RULER_COLOUR           (v4){.r = 1.00, .g = 0.70, .b = 0.00, .a = 1.0}

#define MENU_PLUS_COLOUR       (v4){.r = 0.33, .g = 0.42, .b = 1.00, .a = 1.0}
#define MENU_CLOSE_COLOUR      FOCUSED_COLOUR

#define NORMALIZED_FG_COLOUR   colour_from_normalized(FG_COLOUR)

#define HOVER_SPEED            5.0f

#define RULER_TEXT_PAD         10.0f
#define RULER_TICK_LENGTH      20.0f

#define UI_SPLIT_HANDLE_THICK  8.0f
#define UI_REGION_PAD          32.0f

/* TODO(rnp) smooth scroll */
#define UI_SCROLL_SPEED 12.0f

#define LISTING_ITEM_PAD   12.0f
#define LISTING_INDENT     20.0f
#define LISTING_LINE_PAD    6.0f
#define TITLE_BAR_PAD       6.0f

typedef struct {
	u8   buf[64];
	i32  buf_len;
	i32  cursor;
	f32  cursor_blink_t;
	f32  cursor_blink_scale;
	Rect rect, hot_rect;
	Font *font, *hot_font;
} InputState;

typedef struct {
	v2    at;
	Font *font, *hot_font;
} MenuState;

typedef enum {
	IT_NONE,
	IT_NOP,
	IT_DISPLAY,
	IT_DRAG,
	IT_MENU,
	IT_SCALE_BAR,
	IT_SCROLL,
	IT_SET,
	IT_TEXT,
} InteractionType;

enum ruler_state {
	RS_NONE,
	RS_START,
	RS_HOLD,
};

typedef struct v2_sll {
	struct v2_sll *next;
	v2             v;
} v2_sll;

typedef struct BeamformerUI BeamformerUI;
typedef struct Variable Variable;

typedef enum {
	RSD_VERTICAL,
	RSD_HORIZONTAL,
} RegionSplitDirection;

typedef struct {
	Variable *left;
	Variable *right;
	f32       fraction;
	RegionSplitDirection direction;
} RegionSplit;

/* TODO(rnp): this should be refactored to not need a BeamformerCtx */
typedef struct {
	BeamformerCtx *ctx;
	void          *stats;
} ComputeStatsView;

typedef struct {
	b32 *processing;
	f32 *progress;
	f32 display_t;
	f32 display_t_velocity;
} ComputeProgressBar;

typedef enum {
	VT_NULL,
	VT_B32,
	VT_F32,
	VT_I32,
	VT_GROUP,
	VT_BEAMFORMER_VARIABLE,
	VT_BEAMFORMER_FRAME_VIEW,
	VT_COMPUTE_STATS_VIEW,
	VT_COMPUTE_LATEST_STATS_VIEW,
	VT_COMPUTE_PROGRESS_BAR,
	VT_SCALE_BAR,
	VT_UI_BUTTON,
	VT_UI_VIEW,
	VT_UI_REGION_SPLIT,
} VariableType;

typedef enum {
	VG_LIST,
	/* NOTE(rnp): special groups for vectors with components
	 * stored in separate memory locations */
	VG_V2,
	VG_V4,
} VariableGroupType;

typedef struct {
	Variable *first;
	Variable *last;
	b32       expanded;
	f32       max_name_width;
	VariableGroupType type;
} VariableGroup;

typedef enum {
	UI_VIEW_CUSTOM_TEXT = 1 << 0,
} UIViewFlags;

typedef struct {
	/* NOTE(rnp): superset of group, group must come first */
	VariableGroup  group;
	Variable      *close;
	Variable      *menu;
	f32            needed_height;
	f32            offset;
	UIViewFlags    flags;
} UIView;

/* X(id, text) */
#define FRAME_VIEW_BUTTONS \
	X(FV_COPY_HORIZONTAL, "Copy Horizontal") \
	X(FV_COPY_VERTICAL,   "Copy Vertical")   \
	X(FV_EXPORT,          "Export Bitmap")

#define X(id, text) UI_BID_ ##id,
typedef enum {
	UI_BID_CLOSE_VIEW,
	FRAME_VIEW_BUTTONS
} UIButtonID;
#undef X

typedef struct {
	s8       suffix;
	/* TODO(rnp): think of something better than this */
	union {
		struct {s8 *names; u32 count;} name_table;
		struct {
			f32 display_scale;
			f32 scroll_scale;
			v2  limits;
		} params;
	};
	void         *store;
	VariableType  store_type;
} BeamformerVariable;

typedef enum {
	V_INPUT          = 1 << 0,
	V_TEXT           = 1 << 1,
	V_RADIO_BUTTON   = 1 << 2,
	V_MENU           = 1 << 3,
	V_CAUSES_COMPUTE = 1 << 29,
	V_UPDATE_VIEW    = 1 << 30,
} VariableFlags;

struct Variable {
	s8 name;
	union {
		void               *generic;
		BeamformerVariable  beamformer_variable;
		ComputeProgressBar  compute_progress_bar;
		ComputeStatsView    compute_stats_view;
		RegionSplit         region_split;
		UIButtonID          button;
		UIView              view;
		VariableGroup       group;
		b32                 b32;
		i32                 i32;
		f32                 f32;
	} u;
	Variable *next;
	Variable *parent;
	VariableFlags flags;
	VariableType  type;

	f32 hover_t;
	f32 name_width;
};

typedef enum {
	SB_LATERAL,
	SB_AXIAL,
} ScaleBarDirection;

typedef struct {
	f32    *min_value, *max_value;
	v2_sll *savepoint_stack;
	v2      zoom_starting_point;
	v2      screen_offset;
	v2      screen_space_to_value;
	v2      limits;
	v2      scroll_scale;
	f32     hover_t;
	b32     causes_compute;
} ScaleBar;

typedef enum {
	FVT_INDEXED,
	FVT_LATEST,
	FVT_COPY,
} BeamformerFrameViewType;

typedef struct BeamformerFrameView {
	ScaleBar lateral_scale_bar;
	ScaleBar axial_scale_bar;
	Variable *lateral_scale_bar_active;
	Variable *axial_scale_bar_active;

	v4 min_coordinate;
	v4 max_coordinate;

	Variable threshold;
	Variable dynamic_range;
	Variable gamma;
	f32      gamma_val;

	FragmentShaderCtx *ctx;
	BeamformFrame     *frame;
	struct BeamformerFrameView *prev, *next;

	RenderTexture2D rendered_view;
	BeamformerFrameViewType type;
	b32 needs_update;
} BeamformerFrameView;

typedef struct {
	Variable *hot;
	Variable *active;
	InteractionType hot_type;
	InteractionType type;
} InteractionState;

struct BeamformerUI {
	Arena arena;

	Font font;
	Font small_font;

	Variable *regions;
	Variable *variable_freelist;
	Variable *scratch_variable;
	Variable  scratch_variables[2];

	BeamformerFrameView *views;
	BeamformerFrameView *view_freelist;
	BeamformFrame       *frame_freelist;

	InteractionState interaction;
	/* TODO(rnp): these can be combined */
	MenuState        menu_state;
	InputState       text_input_state;

	v2_sll *scale_bar_savepoint_freelist;

	v2  ruler_start_p;
	v2  ruler_stop_p;
	u32 ruler_state;

	b32                 latest_frame_changed;
	BeamformFrame      *latest_frame;
	ComputeShaderStats *latest_compute_stats;

	BeamformerUIParameters params;
	b32                    flush_params;

	iptr                   last_displayed_frame;

	OS *os;
};

typedef enum {
	TF_NONE     = 0,
	TF_ROTATED  = 1 << 0,
	TF_LIMITED  = 1 << 1,
	TF_OUTLINED = 1 << 2,
} TextFlags;

typedef struct {
	Font  *font;
	Rect  limits;
	Color colour;
	Color outline_colour;
	f32   outline_thick;
	f32   rotation;
	TextFlags flags;
} TextSpec;

static v2
measure_text(Font font, s8 text)
{
	v2 result = {.y = font.baseSize};
	for (iz i = 0; i < text.len; i++) {
		/* NOTE: assumes font glyphs are ordered ASCII */
		i32 idx   = (i32)text.data[i] - 0x20;
		result.x += font.glyphs[idx].advanceX;
		if (font.glyphs[idx].advanceX == 0)
			result.x += (font.recs[idx].width + font.glyphs[idx].offsetX);
	}
	return result;
}

static void
stream_append_variable_base(Stream *s, VariableType type, void *var, void *scale)
{
	switch (type) {
	case VT_B32: {
		s8 *text = var;
		stream_append_s8(s, text[*(b32 *)scale != 0]);
	} break;
	case VT_F32: {
		f32 val = *(f32 *)var * *(f32 *)scale;
		stream_append_f64(s, val, 100);
	} break;
	default: INVALID_CODE_PATH;
	}
}

static void
stream_append_variable(Stream *s, Variable *var)
{
	switch (var->type) {
	case VT_UI_BUTTON:
	case VT_GROUP: stream_append_s8(s, var->name); break;
	case VT_BEAMFORMER_VARIABLE: {
		BeamformerVariable *bv = &var->u.beamformer_variable;
		switch (bv->store_type) {
		case VT_B32: {
			stream_append_variable_base(s, VT_B32, bv->name_table.names, bv->store);
		} break;
		case VT_F32: {
			stream_append_variable_base(s, VT_F32, bv->store, &bv->params.display_scale);
		} break;
		default: INVALID_CODE_PATH;
		}
	} break;
	case VT_B32: {
		s8 texts[] = {s8("False"), s8("True")};
		stream_append_variable_base(s, VT_B32, texts, &var->u.b32);
	} break;
	case VT_F32: {
		f32 scale = 1;
		stream_append_variable_base(s, VT_F32, &var->u.f32, &scale);
	} break;
	default: INVALID_CODE_PATH;
	}
}

static s8
push_das_shader_id(Stream *s, DASShaderID shader, u32 transmit_count)
{
	#define X(type, id, pretty, fixed_tx) s8(pretty),
	static s8 pretty_names[] = { DAS_TYPES };
	#undef X
	#define X(type, id, pretty, fixed_tx) fixed_tx,
	static u8 fixed_transmits[] = { DAS_TYPES };
	#undef X

	if ((u32)shader < (u32)DAS_LAST) {
		stream_append_s8(s, pretty_names[shader]);
		if (!fixed_transmits[shader]) {
			stream_append_byte(s, '-');
			stream_append_u64(s, transmit_count);
		}
	}

	return stream_to_s8(s);
}

static s8
push_custom_view_title(Stream *s, Variable *var)
{
	switch (var->type) {
	case VT_COMPUTE_STATS_VIEW:
	case VT_COMPUTE_LATEST_STATS_VIEW: {
		stream_append_s8(s, s8("Compute Stats"));
		if (var->type == VT_COMPUTE_LATEST_STATS_VIEW)
			stream_append_s8(s, s8(": Live"));
	} break;
	case VT_COMPUTE_PROGRESS_BAR: {
		stream_append_s8(s, s8("Compute Progress: "));
		stream_append_f64(s, 100 * *var->u.compute_progress_bar.progress, 100);
		stream_append_byte(s, '%');
	} break;
	case VT_BEAMFORMER_FRAME_VIEW: {
		BeamformerFrameView *bv = var->u.generic;
		stream_append_s8(s, s8("Frame View"));
		switch (bv->type) {
		case FVT_LATEST:  stream_append_s8(s, s8(": Live [")); break;
		case FVT_COPY:    stream_append_s8(s, s8(": Copy [")); break;
		case FVT_INDEXED: stream_append_s8(s, s8(": ["));      break;
		}
		stream_append_hex_u64(s, bv->frame? bv->frame->id : 0);
		stream_append_byte(s, ']');
	} break;
	default: INVALID_CODE_PATH;
	}
	return stream_to_s8(s);
}

static void
resize_frame_view(BeamformerFrameView *view, uv2 dim)
{
	UnloadRenderTexture(view->rendered_view);
	/* TODO(rnp): sometimes when accepting data on w32 something happens
	* and the program will stall in vprintf in TraceLog(...) here.
	* for now do this to avoid the problem */
	//SetTraceLogLevel(LOG_NONE);
	view->rendered_view = LoadRenderTexture(dim.x, dim.y);
	//SetTraceLogLevel(LOG_INFO);

	/* TODO(rnp): add some ID for the specific view here */
	LABEL_GL_OBJECT(GL_FRAMEBUFFER, view->rendered_view.id,         s8("Frame View"));
	LABEL_GL_OBJECT(GL_TEXTURE,     view->rendered_view.texture.id, s8("Frame View Texture"));
	glGenerateTextureMipmap(view->rendered_view.texture.id);

	//SetTextureFilter(view->rendered_view.texture, TEXTURE_FILTER_ANISOTROPIC_8X);
	//SetTextureFilter(view->rendered_view.texture, TEXTURE_FILTER_TRILINEAR);
	//SetTextureFilter(view->rendered_view.texture, TEXTURE_FILTER_BILINEAR);

	/* NOTE(rnp): work around raylib's janky texture sampling */
	i32 id = view->rendered_view.texture.id;
	glTextureParameteri(id, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
	glTextureParameteri(id, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
	glTextureParameterfv(id, GL_TEXTURE_BORDER_COLOR, (f32 []){0, 0, 0, 1});
}

static void
ui_variable_free(BeamformerUI *ui, Variable *var)
{
	if (var) {
		var->parent = 0;
		while (var) {
			if (var->type == VT_GROUP || var->type == VT_UI_VIEW) {
				var = var->u.group.first;
			} else {
				if (var->type == VT_BEAMFORMER_FRAME_VIEW) {
					/* TODO(rnp): instead there should be a way of linking these up */
					BeamformerFrameView *bv = var->u.generic;
					glDeleteTextures(1, &bv->frame->texture);
					bv->frame->texture = 0;
					DLLRemove(bv);
					/* TODO(rnp): hack; use a sentinal */
					if (bv == ui->views)
						ui->views = bv->next;
					SLLPush(bv->frame, ui->frame_freelist);
					SLLPush(bv,        ui->view_freelist);
				}

				Variable *next = SLLPush(var, ui->variable_freelist);
				if (next) {
					var = next;
				} else {
					var = var->parent;
					/* NOTE(rnp): when we assign parent here we have already
					 * released the children. Assign type so we don't loop */
					if (var) var->type = VT_NULL;
				}
			}
		}
	}
}

static void
ui_view_free(BeamformerUI *ui, Variable *view)
{
	ASSERT(view->type == VT_UI_VIEW);
	ui_variable_free(ui, view->u.view.close);
	ui_variable_free(ui, view->u.view.menu);
	ui_variable_free(ui, view);
}

static Variable *
fill_variable(Variable *var, Variable *group, s8 name, u32 flags, VariableType type, Font font)
{
	var->flags      = flags;
	var->type       = type;
	var->name       = name;
	var->parent     = group;
	var->name_width = measure_text(font, name).x;

	if (group && (group->type == VT_GROUP || group->type == VT_UI_VIEW)) {
		if (group->u.group.last) group->u.group.last = group->u.group.last->next = var;
		else                     group->u.group.last = group->u.group.first      = var;

		group->u.group.max_name_width = MAX(group->u.group.max_name_width, var->name_width);
	}

	return var;
}

static Variable *
add_variable(BeamformerUI *ui, Variable *group, Arena *arena, s8 name, u32 flags,
             VariableType type, Font font)
{
	Variable *result = SLLPop(ui->variable_freelist);
	if (result) zero_struct(result);
	else        result = push_struct(arena, Variable);
	return fill_variable(result, group, name, flags, type, font);
}

static Variable *
add_variable_group(BeamformerUI *ui, Variable *group, Arena *arena, s8 name, VariableGroupType type, Font font)
{
	Variable *result     = add_variable(ui, group, arena, name, V_INPUT, VT_GROUP, font);
	result->u.group.type = type;
	return result;
}

static Variable *
end_variable_group(Variable *group)
{
	ASSERT(group->type == VT_GROUP);
	return group->parent;
}

static Variable *
add_button(BeamformerUI *ui, Variable *group, Arena *arena, s8 name, UIButtonID id,
           b32 radio, Font font)
{
	u32 flags = V_INPUT;
	if (radio) flags |= V_RADIO_BUTTON;
	Variable *result = add_variable(ui, group, arena, name, flags, VT_UI_BUTTON, font);
	result->u.button = id;
	return result;
}

static Variable *
add_ui_split(BeamformerUI *ui, Variable *parent, Arena *arena, s8 name, f32 fraction,
             RegionSplitDirection direction, Font font)
{
	Variable *result = add_variable(ui, parent, arena, name, 0, VT_UI_REGION_SPLIT, font);
	result->u.region_split.direction = direction;
	result->u.region_split.fraction  = fraction;
	return result;
}

static Variable *
add_ui_view(BeamformerUI *ui, Variable *parent, Arena *arena, s8 name, u32 view_flags, b32 closable)
{
	Variable *result = add_variable(ui, parent, arena, name, 0, VT_UI_VIEW, ui->small_font);
	UIView   *view   = &result->u.view;
	view->group.type = VG_LIST;
	view->flags      = view_flags;
	if (closable) {
		view->close = add_button(ui, 0, arena, s8(""), UI_BID_CLOSE_VIEW, 0, ui->small_font);
		/* NOTE(rnp): we do this explicitly so that close doesn't end up in the view group */
		view->close->parent = result;
	}
	return result;
}

static void
add_beamformer_variable_f32(BeamformerUI *ui, Variable *group, Arena *arena, s8 name, s8 suffix,
                            f32 *store, v2 limits, f32 display_scale, f32 scroll_scale, u32 flags,
                            Font font)
{
	Variable *var = add_variable(ui, group, arena, name, flags, VT_BEAMFORMER_VARIABLE, font);
	BeamformerVariable *bv   = &var->u.beamformer_variable;
	bv->suffix               = suffix;
	bv->store                = store;
	bv->store_type           = VT_F32;
	bv->params.display_scale = display_scale;
	bv->params.scroll_scale  = scroll_scale;
	bv->params.limits        = limits;
}

static void
add_beamformer_variable_b32(BeamformerUI *ui, Variable *group, Arena *arena, s8 name,
                            s8 false_text, s8 true_text, b32 *store, u32 flags, Font font)
{
	Variable *var = add_variable(ui, group, arena, name, flags, VT_BEAMFORMER_VARIABLE, font);
	BeamformerVariable *bv  = &var->u.beamformer_variable;
	bv->store               = store;
	bv->store_type          = VT_B32;
	bv->name_table.names    = alloc(arena, s8, 2);
	bv->name_table.count    = 2;
	bv->name_table.names[0] = false_text;
	bv->name_table.names[1] = true_text;
}

static Variable *
add_beamformer_parameters_view(Variable *parent, BeamformerCtx *ctx)
{
	BeamformerUI *ui           = ctx->ui;
	BeamformerUIParameters *bp = &ui->params;

	v2 v2_inf = {.x = -F32_INFINITY, .y = F32_INFINITY};

	/* TODO(rnp): this can be closable once we have a way of opening new views */
	Variable *result = add_ui_view(ui, parent, &ui->arena, s8("Parameters"), 0, 0);

	add_beamformer_variable_f32(ui, result, &ui->arena, s8("Sampling Frequency:"), s8("[MHz]"),
	                            &bp->sampling_frequency, (v2){0}, 1e-6, 0, 0, ui->font);

	add_beamformer_variable_f32(ui, result, &ui->arena, s8("Center Frequency:"), s8("[MHz]"),
	                            &bp->center_frequency, (v2){.y = 100e-6}, 1e-6, 1e5,
	                            V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

	add_beamformer_variable_f32(ui, result, &ui->arena, s8("Speed of Sound:"), s8("[m/s]"),
	                            &bp->speed_of_sound, (v2){.y = 1e6}, 1, 10,
	                            V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

	result = add_variable_group(ui, result, &ui->arena, s8("Lateral Extent:"), VG_V2, ui->font);
	{
		add_beamformer_variable_f32(ui, result, &ui->arena, s8("Min:"), s8("[mm]"),
		                            &bp->output_min_coordinate.x, v2_inf, 1e3, 0.5e-3,
		                            V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

		add_beamformer_variable_f32(ui, result, &ui->arena, s8("Max:"), s8("[mm]"),
		                            &bp->output_max_coordinate.x, v2_inf, 1e3, 0.5e-3,
		                            V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);
	}
	result = end_variable_group(result);

	result = add_variable_group(ui, result, &ui->arena, s8("Axial Extent:"), VG_V2, ui->font);
	{
		add_beamformer_variable_f32(ui, result, &ui->arena, s8("Min:"), s8("[mm]"),
		                            &bp->output_min_coordinate.z, v2_inf, 1e3, 0.5e-3,
		                            V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

		add_beamformer_variable_f32(ui, result, &ui->arena, s8("Max:"), s8("[mm]"),
		                            &bp->output_max_coordinate.z, v2_inf, 1e3, 0.5e-3,
		                            V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);
	}
	result = end_variable_group(result);

	add_beamformer_variable_f32(ui, result, &ui->arena, s8("Off Axis Position:"), s8("[mm]"),
	                            &bp->off_axis_pos, (v2){.x = -1e3, .y = 1e3}, 1e3,
	                            0.5e-3, V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

	add_beamformer_variable_b32(ui, result, &ui->arena, s8("Beamform Plane:"), s8("XZ"), s8("YZ"),
	                            (b32 *)&bp->beamform_plane, V_INPUT|V_CAUSES_COMPUTE, ui->font);

	add_beamformer_variable_f32(ui, result, &ui->arena, s8("F#:"), s8(""), &bp->f_number,
	                            (v2){.y = 1e3}, 1, 0.1, V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

	add_beamformer_variable_b32(ui, result, &ui->arena, s8("Interpolate:"), s8("False"), s8("True"),
	                            &bp->interpolate, V_INPUT|V_CAUSES_COMPUTE, ui->font);

	return result;
}

static Variable *
add_beamformer_frame_view(BeamformerUI *ui, Variable *parent, Arena *arena,
                          BeamformerFrameViewType type, b32 closable)
{
	/* TODO(rnp): this can be always closable once we have a way of opening new views */
	Variable *result = add_ui_view(ui, parent, arena, s8(""), UI_VIEW_CUSTOM_TEXT, closable);
	Variable *menu   = result->u.view.menu = add_variable_group(ui, 0, arena, s8(""),
	                                                            VG_LIST, ui->small_font);
	menu->flags  = V_MENU;
	menu->parent = result;
	#define X(id, text) add_button(ui, menu, arena, s8(text), UI_BID_ ##id, 0, ui->small_font);
	FRAME_VIEW_BUTTONS
	#undef X

	Variable *var = add_variable(ui, result, arena, s8(""), 0, VT_BEAMFORMER_FRAME_VIEW,
	                             ui->small_font);

	BeamformerFrameView *bv = SLLPop(ui->view_freelist);
	if (bv) zero_struct(bv);
	else    bv = push_struct(arena, typeof(*bv));
	DLLPushDown(bv, ui->views);

	var->u.generic = bv;

	fill_variable(&bv->dynamic_range, var, s8("Dynamic Range:"), V_INPUT|V_TEXT|V_UPDATE_VIEW,
	              VT_F32, ui->small_font);
	fill_variable(&bv->threshold, var, s8("Threshold:"), V_INPUT|V_TEXT|V_UPDATE_VIEW,
	              VT_F32, ui->small_font);
	fill_variable(&bv->gamma, var, s8("Gamma:"), V_INPUT|V_TEXT|V_UPDATE_VIEW,
	              VT_BEAMFORMER_VARIABLE, ui->small_font);

	bv->type                = type;
	bv->dynamic_range.u.f32 = -50.0f;
	bv->threshold.u.f32     =  40.0f;
	bv->gamma_val           =  1.0f;

	BeamformerVariable *gamma = &bv->gamma.u.beamformer_variable;
	gamma->store              = &bv->gamma_val;
	gamma->store_type         = VT_F32;
	gamma->params.display_scale = 1.0f;
	gamma->params.scroll_scale  = 0.01f;
	gamma->params.limits        = (v2){.y = 8};

	bv->axial_scale_bar_active   = add_variable(ui, menu, arena, s8("Axial Scale Bar"),
	                                            V_INPUT|V_RADIO_BUTTON, VT_B32, ui->small_font);
	bv->lateral_scale_bar_active = add_variable(ui, menu, arena, s8("Lateral Scale Bar"),
	                                            V_INPUT|V_RADIO_BUTTON, VT_B32, ui->small_font);

	bv->lateral_scale_bar.limits              = (v2){.x = -1, .y = 1};
	bv->axial_scale_bar.limits                = (v2){.x =  0, .y = 1};
	bv->lateral_scale_bar.scroll_scale        = (v2){.x = -0.5e-3, .y = 0.5e-3};
	bv->axial_scale_bar.scroll_scale          = (v2){.x =  0,      .y = 1e-3};
	bv->lateral_scale_bar.zoom_starting_point = (v2){.x = F32_INFINITY, .y = F32_INFINITY};
	bv->axial_scale_bar.zoom_starting_point   = (v2){.x = F32_INFINITY, .y = F32_INFINITY};

	return result;
}

static Variable *
add_compute_progress_bar(Variable *parent, BeamformerCtx *ctx)
{
	BeamformerUI *ui = ctx->ui;
	/* TODO(rnp): this can be closable once we have a way of opening new views */
	Variable *result = add_ui_view(ui, parent, &ui->arena, s8(""), UI_VIEW_CUSTOM_TEXT, 0);
	add_variable(ui, result, &ui->arena, s8(""), 0, VT_COMPUTE_PROGRESS_BAR, ui->small_font);
	ComputeProgressBar *bar = &result->u.group.first->u.compute_progress_bar;
	bar->progress   = &ctx->csctx.processing_progress;
	bar->processing = &ctx->csctx.processing_compute;

	return result;
}

static Variable *
add_compute_stats_view(BeamformerUI *ui, Variable *parent, Arena *arena, VariableType type)
{
	/* TODO(rnp): this can be closable once we have a way of opening new views */
	Variable *result = add_ui_view(ui, parent, arena, s8(""), UI_VIEW_CUSTOM_TEXT, 0);
	add_variable(ui, result, &ui->arena, s8(""), 0, type, ui->small_font);
	return result;
}

static void
ui_copy_frame(BeamformerUI *ui, Variable *button, RegionSplitDirection direction)
{
	Variable *menu   = button->parent;
	Variable *view   = menu->parent;
	Variable *region = view->parent;
	ASSERT(region->type == VT_UI_REGION_SPLIT);
	ASSERT(view->type   == VT_UI_VIEW);

	BeamformerFrameView *old = view->u.group.first->u.generic;
	/* TODO(rnp): hack; it would be better if this was unreachable with a 0 old->frame */
	if (!old->frame)
		return;

	Variable *new_region = add_ui_split(ui, region, &ui->arena, s8(""), 0.5, direction, ui->small_font);

	if (view == region->u.region_split.left) {
		region->u.region_split.left  = new_region;
	} else {
		region->u.region_split.right = new_region;
	}
	view->parent = new_region;
	new_region->u.region_split.left  = view;
	new_region->u.region_split.right = add_beamformer_frame_view(ui, new_region, &ui->arena, FVT_COPY, 1);

	BeamformerFrameView *bv = new_region->u.region_split.right->u.group.first->u.generic;
	bv->lateral_scale_bar.min_value = &bv->min_coordinate.x;
	bv->lateral_scale_bar.max_value = &bv->max_coordinate.x;
	bv->axial_scale_bar.min_value   = &bv->min_coordinate.z;
	bv->axial_scale_bar.max_value   = &bv->max_coordinate.z;

	bv->ctx                 = old->ctx;
	bv->needs_update        = 1;
	bv->threshold.u.f32     = old->threshold.u.f32;
	bv->dynamic_range.u.f32 = old->dynamic_range.u.f32;
	bv->min_coordinate      = old->frame->min_coordinate;
	bv->max_coordinate      = old->frame->max_coordinate;

	bv->frame = SLLPop(ui->frame_freelist);
	if (!bv->frame) bv->frame = push_struct(&ui->arena, typeof(*bv->frame));

	ASSERT(old->frame->in_flight == 0);
	mem_copy(bv->frame, old->frame, sizeof(*bv->frame));
	bv->frame->texture = 0;
	bv->frame->next    = 0;
	alloc_beamform_frame(0, bv->frame, 0, old->frame->dim, s8("Frame Copy: "), ui->arena);
	bv->frame->ready_to_present = 1;

	glCopyImageSubData(old->frame->texture, GL_TEXTURE_3D, 0, 0, 0, 0,
	                   bv->frame->texture,  GL_TEXTURE_3D, 0, 0, 0, 0,
	                   bv->frame->dim.x, bv->frame->dim.y, bv->frame->dim.z);
	glMemoryBarrier(GL_TEXTURE_UPDATE_BARRIER_BIT);
	/* TODO(rnp): x vs y here */
	resize_frame_view(bv, (uv2){.x = bv->frame->dim.x, .y = bv->frame->dim.z});
}

#include <stdlib.h>
static void
ui_export_view(BeamformerUI *ui, Variable *view)
{
	ASSERT(view->type == VT_UI_VIEW);

	u8 buffer[1024];

	BeamformerFrameView *bv = view->u.group.first->u.generic;
	Arena tmp = ui->arena;
	Stream buf = arena_stream(&tmp);
	Stream path = {.data = buffer, .cap = sizeof(buffer)};

	v4 min = bv->frame->min_coordinate, max = bv->frame->max_coordinate;

	stream_append_s8(&path, s8("/tmp/downloads/"));
	push_das_shader_id(&path, bv->frame->das_shader_id, bv->frame->compound_count);
	stream_append_byte(&path, '_');
	stream_append_hex_u64(&path, bv->frame->id);
	iz pidx = path.widx;
	stream_append_s8(&path, s8("_params.csv"));
	stream_append_byte(&path, 0);

	stream_append_s8(&buf, s8("min_coord,max_coord,size,dynamic_range\n"));
	stream_append_f64(&buf, min.x, 1000); stream_append_byte(&buf, ',');
	stream_append_f64(&buf, max.x, 1000); stream_append_byte(&buf, ',');
	stream_append_u64(&buf, bv->rendered_view.texture.width); stream_append_byte(&buf, ',');
	stream_append_f64(&buf, bv->dynamic_range.u.f32, 100);
	stream_append_byte(&buf, '\n');
	stream_append_f64(&buf, min.z, 1000); stream_append_byte(&buf, ',');
	stream_append_f64(&buf, max.z, 1000); stream_append_byte(&buf, ',');
	stream_append_u64(&buf, bv->rendered_view.texture.height); stream_append_byte(&buf, ',');
	stream_append_byte(&buf, '\n');
	if (!ui->os->write_new_file((c8 *)path.data, stream_to_s8(&buf)))
		ui->os->write_file(ui->os->stderr, s8("failed to export view parameters\n"));

	stream_reset(&path, pidx);
	stream_append_s8(&path, s8(".bin"));
	stream_append_byte(&path, 0);

	iz out_size = bv->rendered_view.texture.height * bv->rendered_view.texture.width * sizeof(u32);
	void *out_buf = malloc(out_size);
	if (out_buf) {
		//ctx->export_buffer = ctx->os.alloc_arena(ctx->export_buffer, out_size);
		glGetTextureImage(bv->rendered_view.texture.id, 0, GL_RGBA,
		                  GL_UNSIGNED_INT_8_8_8_8, out_size, out_buf);
		s8 raw = {.len = out_size, .data = out_buf};
		if (!ui->os->write_new_file((c8 *)path.data, raw))
			ui->os->write_file(ui->os->stderr, s8("failed to export view\n"));
		free(out_buf);
	}
}

static b32
view_update(BeamformerUI *ui, BeamformerFrameView *view)
{
	b32 needs_resize = 0;
	uv2 current = {.w = view->rendered_view.texture.width, .h = view->rendered_view.texture.height};
	uv2 target;

	switch (view->type) {
	case FVT_LATEST: {
		/* TODO(rnp): x-z or y-z */
		target = (uv2){.w = ui->params.output_points.x, .h = ui->params.output_points.z};
		needs_resize = !uv2_equal(current, target) && !uv2_equal(target, (uv2){0});
		view->needs_update |= view->frame != ui->latest_frame;
		view->frame         = ui->latest_frame;
	} break;
	default: {
		/* TODO(rnp): add method of setting a target size in frame view */
		target = (uv2){.w = ui->params.output_points.x, .h = ui->params.output_points.z};
		needs_resize = !uv2_equal(current, target) && !uv2_equal(target, (uv2){0});
	} break;
	}

	if (needs_resize && view->frame) {
		resize_frame_view(view, target);
		view->needs_update = 1;
	}

	return (view->ctx->updated || view->needs_update) && view->frame;
}

static void
update_frame_views(BeamformerUI *ui)
{
	for (BeamformerFrameView *view = ui->views; view; view = view->next) {
		if (view_update(ui, view)) {
			BeamformFrame *frame = view->frame;
			BeginTextureMode(view->rendered_view);
				ClearBackground(PINK);
				BeginShaderMode(view->ctx->shader);
					glUseProgram(view->ctx->shader.id);
					glBindTextureUnit(0, frame->texture);
					glUniform1f(view->ctx->db_cutoff_id, view->dynamic_range.u.f32);
					glUniform1f(view->ctx->threshold_id, view->threshold.u.f32);
					glUniform1f(view->ctx->gamma_id,     view->gamma_val);
					DrawTexture(view->rendered_view.texture, 0, 0, WHITE);
				EndShaderMode();
			EndTextureMode();
			glGenerateTextureMipmap(view->rendered_view.texture.id);
			view->needs_update = 0;
		}
	}
}

static b32
frame_view_ready_to_present(BeamformerFrameView *view)
{
	uv2 render_size = {.w = view->rendered_view.texture.width,
	                   .h = view->rendered_view.texture.height};
	return !uv2_equal((uv2){0}, render_size);
}

static Color
colour_from_normalized(v4 rgba)
{
	return (Color){.r = rgba.r * 255.0f, .g = rgba.g * 255.0f,
	               .b = rgba.b * 255.0f, .a = rgba.a * 255.0f};
}

static Color
fade(Color a, f32 visibility)
{
	a.a = (u8)((f32)a.a * visibility);
	return a;
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
draw_text_base(Font font, s8 text, v2 pos, Color colour)
{
	v2 off = pos;
	for (iz i = 0; i < text.len; i++) {
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
	v2 result = {.x = off.x - pos.x, .y = font.baseSize};
	return result;
}

/* NOTE(rnp): expensive but of the available options in raylib this gives the best results */
static v2
draw_outlined_text(s8 text, v2 pos, TextSpec *ts)
{
	f32 ow = ts->outline_thick;
	draw_text_base(*ts->font, text, sub_v2(pos, (v2){.x =  ow, .y =  ow}), ts->outline_colour);
	draw_text_base(*ts->font, text, sub_v2(pos, (v2){.x =  ow, .y = -ow}), ts->outline_colour);
	draw_text_base(*ts->font, text, sub_v2(pos, (v2){.x = -ow, .y =  ow}), ts->outline_colour);
	draw_text_base(*ts->font, text, sub_v2(pos, (v2){.x = -ow, .y = -ow}), ts->outline_colour);

	v2 result = draw_text_base(*ts->font, text, pos, ts->colour);

	return result;
}

static v2
draw_text(s8 text, v2 pos, TextSpec *ts)
{
	if (ts->flags & TF_ROTATED) {
		rlPushMatrix();
		rlTranslatef(pos.x, pos.y, 0);
		rlRotatef(ts->rotation, 0, 0, 1);
		pos = (v2){0};
	}

	v2 result   = measure_text(*ts->font, text);
	s8 ellipsis = s8("...");
	b32 clamped = ts->flags & TF_LIMITED && result.w > ts->limits.size.w;
	if (clamped) {
		f32 elipsis_width = measure_text(*ts->font, ellipsis).x;
		if (elipsis_width < ts->limits.size.w) {
			/* TODO(rnp): there must be a better way */
			while (text.len) {
				text.len--;
				f32 width = measure_text(*ts->font, text).x;
				if (width + elipsis_width < ts->limits.size.w)
					break;
			}
		} else {
			text.len     = 0;
			ellipsis.len = 0;
		}
	}

	if (ts->flags & TF_OUTLINED) result.x = draw_outlined_text(text, pos, ts).x;
	else                         result.x = draw_text_base(*ts->font, text, pos, ts->colour).x;

	if (clamped) {
		pos.x += result.x;
		if (ts->flags & TF_OUTLINED) result.x += draw_outlined_text(ellipsis, pos, ts).x;
		else                         result.x += draw_text_base(*ts->font, ellipsis, pos,
		                                                        ts->colour).x;
	}

	if (ts->flags & TF_ROTATED) rlPopMatrix();

	return result;
}

static Rect
extend_rect_centered(Rect r, v2 delta)
{
	r.size.w += delta.x;
	r.size.h += delta.y;
	r.pos.x  -= delta.x / 2;
	r.pos.y  -= delta.y / 2;
	return r;
}

static Rect
shrink_rect_centered(Rect r, v2 delta)
{
	delta.x   = MIN(delta.x, r.size.w);
	delta.y   = MIN(delta.y, r.size.h);
	r.size.w -= delta.x;
	r.size.h -= delta.y;
	r.pos.x  += delta.x / 2;
	r.pos.y  += delta.y / 2;
	return r;
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

static b32
hover_rect(v2 mouse, Rect rect, f32 *hover_t)
{
	b32 hovering = CheckCollisionPointRec(mouse.rl, rect.rl);
	if (hovering) *hover_t += HOVER_SPEED * dt_for_frame;
	else          *hover_t -= HOVER_SPEED * dt_for_frame;
	*hover_t = CLAMP01(*hover_t);
	return hovering;
}

static b32
hover_var(BeamformerUI *ui, v2 mouse, Rect rect, Variable *var)
{
	b32 result = hover_rect(mouse, rect, &var->hover_t);
	if (result) {
		ui->interaction.hot_type = IT_NONE;
		ui->interaction.hot      = var;
	}
	return result;
}

static Rect
draw_title_bar(BeamformerUI *ui, Arena arena, Variable *ui_view, Rect r, v2 mouse)
{
	ASSERT(ui_view->type == VT_UI_VIEW);
	UIView *view = &ui_view->u.view;

	s8 title = ui_view->name;
	if (view->flags & UI_VIEW_CUSTOM_TEXT) {
		Stream buf = arena_stream(&arena);
		title      = push_custom_view_title(&buf, ui_view->u.group.first);
	}

	Rect result, title_rect;
	cut_rect_vertical(r, ui->small_font.baseSize + TITLE_BAR_PAD, &title_rect, &result);
	cut_rect_vertical(result, LISTING_LINE_PAD, 0, &result);

	DrawRectangleRec(title_rect.rl, BLACK);

	title_rect = shrink_rect_centered(title_rect, (v2){.x = 1.5 * TITLE_BAR_PAD});
	DrawRectangleRounded(title_rect.rl, 0.5, 0, fade(colour_from_normalized(BG_COLOUR), 0.55));
	title_rect = shrink_rect_centered(title_rect, (v2){.x = 3 * TITLE_BAR_PAD});

	if (view->close) {
		Rect close;
		cut_rect_horizontal(title_rect, title_rect.size.w - title_rect.size.h, &title_rect, &close);
		hover_var(ui, mouse, close, view->close);

		Color colour = colour_from_normalized(lerp_v4(MENU_CLOSE_COLOUR, FG_COLOUR, view->close->hover_t));
		close = shrink_rect_centered(close, (v2){.x = 16, .y = 16});
		DrawLineEx(close.pos.rl, add_v2(close.pos, close.size).rl, 4, colour);
		DrawLineEx(add_v2(close.pos, (v2){.x = close.size.w}).rl,
		           add_v2(close.pos, (v2){.y = close.size.h}).rl,  4, colour);
	}

	if (view->menu) {
		Rect menu;
		cut_rect_horizontal(title_rect, title_rect.size.w - title_rect.size.h, &title_rect, &menu);
		if (hover_var(ui, mouse, menu, view->menu))
			ui->menu_state.hot_font = &ui->small_font;

		Color colour = colour_from_normalized(lerp_v4(MENU_PLUS_COLOUR, FG_COLOUR, view->menu->hover_t));
		menu = shrink_rect_centered(menu, (v2){.x = 14, .y = 14});
		DrawLineEx(add_v2(menu.pos, (v2){.x = menu.size.w / 2}).rl,
		           add_v2(menu.pos, (v2){.x = menu.size.w / 2, .y = menu.size.h}).rl, 4, colour);
		DrawLineEx(add_v2(menu.pos, (v2){.y = menu.size.h / 2}).rl,
		           add_v2(menu.pos, (v2){.x = menu.size.w, .y = menu.size.h / 2}).rl, 4, colour);
	}

	/* TODO(rnp): hover the title text rect and use it to access the global menu */
	v2 title_pos = title_rect.pos;
	title_pos.y += 0.5 * TITLE_BAR_PAD;
	TextSpec text_spec = {.font = &ui->small_font, .flags = TF_LIMITED,
	                      .colour = NORMALIZED_FG_COLOUR, .limits.size = title_rect.size};
	draw_text(title, title_pos, &text_spec);

	return result;
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
	v2 tp = {.x = ui->small_font.baseSize / 2, .y = ep.y + RULER_TEXT_PAD};
	TextSpec text_spec = {.font = &ui->small_font, .rotation = 90, .colour = txt_colour, .flags = TF_ROTATED};
	for (u32 j = 0; j <= segments; j++) {
		DrawLineEx(sp.rl, ep.rl, 3, ruler_colour);

		stream_reset(buf, 0);
		if (draw_plus && value > 0) stream_append_byte(buf, '+');
		stream_append_f64(buf, value, 10);
		stream_append_s8(buf, suffix);
		draw_text(stream_to_s8(buf), tp, &text_spec);

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
do_scale_bar(BeamformerUI *ui, Stream *buf, ScaleBar *sb, ScaleBarDirection direction,
             v2 mouse, Rect draw_rect, f32 start_value, f32 end_value, s8 suffix)
{
	InteractionState *is = &ui->interaction;

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
		tick_count        = tick_rect.size.y / (1.5 * ui->small_font.baseSize);
		start_pos.y      += tick_rect.size.y;
		markers[0]        = tick_rect.size.y - sb->zoom_starting_point.y;
		markers[1]        = tick_rect.size.y - relative_mouse.y;
		sb->screen_offset = (v2){.y = tick_rect.pos.y};
		sb->screen_space_to_value = (v2){.y = (*sb->max_value - *sb->min_value) / tick_rect.size.y};
	} else {
		tick_rect.size.y  = RULER_TEXT_PAD + RULER_TICK_LENGTH + txt_s.x;
		tick_count        = tick_rect.size.x / (1.5 * ui->small_font.baseSize);
		end_pos.x        += tick_rect.size.x;
		markers[0]        = sb->zoom_starting_point.x;
		markers[1]        = relative_mouse.x;
		/* TODO(rnp): screen space to value space transformation helper */
		sb->screen_offset = (v2){.x = tick_rect.pos.x};
		sb->screen_space_to_value = (v2){.x = (*sb->max_value - *sb->min_value) / tick_rect.size.x};
	}

	if (hover_rect(mouse, tick_rect, &sb->hover_t)) {
		Variable *var  = zero_struct(ui->scratch_variable);
		var->u.generic = sb;
		var->type      = VT_SCALE_BAR;
		is->hot_type   = IT_SCALE_BAR;
		is->hot        = var;

		marker_count  = 2;
	}

	draw_ruler(ui, buf, start_pos, end_pos, start_value, end_value, markers, marker_count,
	           tick_count, suffix, colour_from_normalized(FG_COLOUR),
	           colour_from_normalized(lerp_v4(FG_COLOUR, HOVERED_COLOUR, sb->hover_t)));
}

static v2
draw_radio_button(BeamformerUI *ui, Variable *var, v2 at, v2 mouse, v4 base_colour, f32 size)
{
	ASSERT(var->type == VT_B32 || var->type == VT_BEAMFORMER_VARIABLE);
	b32 value;
	if (var->type == VT_B32) {
		value = var->u.b32;
	} else {
		ASSERT(var->u.beamformer_variable.store_type == VT_B32);
		value = *(b32 *)var->u.beamformer_variable.store;
	}

	v2 result = (v2){.x = size, .y = size};
	Rect hover_rect   = {.pos = at, .size = result};
	hover_rect.pos.y += 1;
	hover_var(ui, mouse, hover_rect, var);

	hover_rect = shrink_rect_centered(hover_rect, (v2){.x = 8, .y = 8});
	Rect inner = shrink_rect_centered(hover_rect, (v2){.x = 4, .y = 4});
	v4 fill = lerp_v4(value? base_colour : (v4){0}, HOVERED_COLOUR, var->hover_t);
	DrawRectangleRoundedLinesEx(hover_rect.rl, 0.2, 0, 2, colour_from_normalized(base_colour));
	DrawRectangleRec(inner.rl, colour_from_normalized(fill));

	return result;
}

static v2
draw_variable(BeamformerUI *ui, Arena arena, Variable *var, v2 at, v2 mouse, v4 base_colour, TextSpec text_spec)
{
	v2 result;
	if (var->flags & V_RADIO_BUTTON) {
		result = draw_radio_button(ui, var, at, mouse, base_colour, text_spec.font->baseSize);
	} else {
		Stream buf = arena_stream(&arena);
		stream_append_variable(&buf, var);
		result = measure_text(*text_spec.font, stream_to_s8(&buf));

		if (var->flags & V_INPUT) {
			Rect text_rect = {.pos = at, .size = result};
			text_rect = extend_rect_centered(text_rect, (v2){.x = 8});
			if (hover_var(ui, mouse, text_rect, var) && (var->flags & V_TEXT)) {
				InputState *is = &ui->text_input_state;
				is->hot_rect   = text_rect;
				is->hot_font   = text_spec.font;
			}
			text_spec.colour = colour_from_normalized(lerp_v4(base_colour, HOVERED_COLOUR,
			                                                  var->hover_t));
		}

		draw_text(stream_to_s8(&buf), at, &text_spec);
	}
	return result;
}

static void
draw_beamformer_frame_view(BeamformerUI *ui, Arena a, Variable *var, Rect display_rect, v2 mouse)
{
	ASSERT(var->type == VT_BEAMFORMER_FRAME_VIEW);
	BeamformerUIParameters *bp = &ui->params;
	InteractionState *is       = &ui->interaction;
	BeamformerFrameView *view  = var->u.generic;
	BeamformFrame *frame       = view->frame;

	v2 txt_s = measure_text(ui->small_font, s8("-288.8 mm"));

	f32  scale_bar_size = 1.2 * txt_s.x + RULER_TICK_LENGTH;
	Rect vr             = display_rect;

	v2 pad = {0};
	if (view->axial_scale_bar_active->u.b32) {
		vr.pos.x  += 0.5 * ui->small_font.baseSize;
		vr.size.w  = MAX(0, display_rect.size.w - scale_bar_size);
		vr.size.h -= ui->small_font.baseSize;
		pad.x      = scale_bar_size;
	}

	if (view->lateral_scale_bar_active->u.b32) {
		vr.pos.y  += 0.5 * ui->small_font.baseSize;
		vr.size.h  = MAX(0, display_rect.size.h - scale_bar_size);
		vr.size.w -= ui->small_font.baseSize;
		pad.y      = scale_bar_size;
	}

	/* TODO(rnp): ideally we hook up both versions to view->min/max */
	v4 min = (view->type == FVT_LATEST)? bp->output_min_coordinate : view->min_coordinate;
	v4 max = (view->type == FVT_LATEST)? bp->output_max_coordinate : view->max_coordinate;

	/* TODO(rnp): make this depend on the requested draw orientation (x-z or y-z or x-y) */
	v2 output_dim = {
		.x = frame->max_coordinate.x - frame->min_coordinate.x,
		.y = frame->max_coordinate.z - frame->min_coordinate.z,
	};
	v2 requested_dim = {.x = max.x - min.x, .y = max.z - min.z};

	f32 aspect = requested_dim.h / requested_dim.w;
	if (display_rect.size.h < (vr.size.w * aspect) + pad.y) {
		vr.size.w = vr.size.h / aspect;
	} else {
		vr.size.h = vr.size.w * aspect;
	}
	vr.pos.x += (display_rect.size.w - (vr.size.w + pad.x)) / 2;
	vr.pos.y += (display_rect.size.h - (vr.size.h + pad.y)) / 2;

	Texture *output = &view->rendered_view.texture;
	v2 pixels_per_meter = {
		.w = (f32)output->width  / output_dim.w,
		.h = (f32)output->height / output_dim.h,
	};

	v2 texture_points  = mul_v2(pixels_per_meter, requested_dim);
	/* TODO(rnp): this also depends on x-y, y-z, x-z */
	v2 texture_start   = {
		.x = pixels_per_meter.x * 0.5 * (output_dim.x - requested_dim.x),
		.y = pixels_per_meter.y * (frame->max_coordinate.z - max.z),
	};

	Rectangle  tex_r  = {texture_start.x, texture_start.y, texture_points.x, -texture_points.y};
	NPatchInfo tex_np = { tex_r, 0, 0, 0, 0, NPATCH_NINE_PATCH };
	DrawTextureNPatch(*output, tex_np, vr.rl, (Vector2){0}, 0, WHITE);

	v2 start_pos  = vr.pos;
	start_pos.y  += vr.size.y;

	if (vr.size.w > 0 && view->lateral_scale_bar_active->u.b32) {
		Arena  tmp = a;
		Stream buf = arena_stream(&tmp);
		do_scale_bar(ui, &buf, &view->lateral_scale_bar, SB_LATERAL, mouse,
		             (Rect){.pos = start_pos, .size = vr.size},
		             *view->lateral_scale_bar.min_value * 1e3,
		             *view->lateral_scale_bar.max_value * 1e3, s8(" mm"));
	}

	start_pos    = vr.pos;
	start_pos.x += vr.size.x;

	if (vr.size.h > 0 && view->axial_scale_bar_active->u.b32) {
		Arena  tmp = a;
		Stream buf = arena_stream(&tmp);
		do_scale_bar(ui, &buf, &view->axial_scale_bar, SB_AXIAL, mouse,
		             (Rect){.pos = start_pos, .size = vr.size},
		             *view->axial_scale_bar.max_value * 1e3,
		             *view->axial_scale_bar.min_value * 1e3, s8(" mm"));
	}

	v2 pixels_to_mm;
	pixels_to_mm.x = output_dim.x / (vr.size.x * 1e-3);
	pixels_to_mm.y = output_dim.y / (vr.size.y * 1e-3);

	TextSpec text_spec = {.font = &ui->small_font, .flags = TF_LIMITED|TF_OUTLINED,
	                      .colour = colour_from_normalized(RULER_COLOUR),
	                      .outline_thick = 1, .outline_colour = BLACK,
	                      .limits.size.x = vr.size.w};

	b32 drew_coordinates = 0;
	f32 remaining_width  = vr.size.w;
	if (CheckCollisionPointRec(mouse.rl, vr.rl)) {
		is->hot_type  = IT_DISPLAY;
		is->hot       = var;

		v2 relative_mouse = sub_v2(mouse, vr.pos);
		v2 mm = mul_v2(relative_mouse, pixels_to_mm);
		mm.x += 1e3 * min.x;
		mm.y += 1e3 * min.z;

		Arena  tmp = a;
		Stream buf = arena_stream(&tmp);
		stream_append_v2(&buf, mm);

		text_spec.limits.size.w -= 4;
		v2 txt_s = measure_text(*text_spec.font, stream_to_s8(&buf));
		v2 txt_p = {
			.x = vr.pos.x + vr.size.w - txt_s.w - 4,
			.y = vr.pos.y + vr.size.h - txt_s.h - 4,
		};
		txt_p.x = MAX(vr.pos.x, txt_p.x);
		remaining_width -= draw_text(stream_to_s8(&buf), txt_p, &text_spec).w;
		text_spec.limits.size.w += 4;
		drew_coordinates = 1;
	}

	{
		Arena  tmp = a;
		Stream buf = arena_stream(&tmp);
		s8 shader  = push_das_shader_id(&buf, frame->das_shader_id, frame->compound_count);
		text_spec.font = &ui->font;
		text_spec.limits.size.w -= 16;
		v2 txt_s   = measure_text(*text_spec.font, shader);
		v2 txt_p  = {
			.x = vr.pos.x + vr.size.w - txt_s.w - 16,
			.y = vr.pos.y + 4,
		};
		txt_p.x = MAX(vr.pos.x, txt_p.x);
		draw_text(stream_to_s8(&buf), txt_p, &text_spec);
		text_spec.font = &ui->small_font;
		text_spec.limits.size.w += 16;
	}

	/* TODO(rnp): store converted ruler points instead of screen points */
	if (ui->ruler_state != RS_NONE && CheckCollisionPointRec(ui->ruler_start_p.rl, vr.rl)) {
		v2 end_p;
		if (ui->ruler_state == RS_START) end_p = mouse;
		else                             end_p = ui->ruler_stop_p;

		end_p          = clamp_v2_rect(end_p, vr);
		v2 pixel_delta = sub_v2(ui->ruler_start_p, end_p);
		v2 mm_delta    = mul_v2(pixels_to_mm, pixel_delta);

		DrawCircleV(ui->ruler_start_p.rl, 3, text_spec.colour);
		DrawLineEx(end_p.rl, ui->ruler_start_p.rl, 2, text_spec.colour);
		DrawCircleV(end_p.rl, 3, text_spec.colour);

		Arena  tmp = a;
		Stream buf = arena_stream(&tmp);
		stream_append_f64(&buf, magnitude_v2(mm_delta), 100);
		stream_append_s8(&buf, s8(" mm"));

		v2 txt_p = ui->ruler_start_p;
		v2 txt_s = measure_text(*text_spec.font, stream_to_s8(&buf));
		if (pixel_delta.y < 0) txt_p.y -= txt_s.y;
		if (pixel_delta.x < 0) txt_p.x -= txt_s.x;
		draw_text(stream_to_s8(&buf), txt_p, &text_spec);
	}

	if (remaining_width > view->dynamic_range.name_width || !drew_coordinates) {
		f32 max_prefix_width = MAX(view->threshold.name_width, view->dynamic_range.name_width);

		v2  end     = add_v2(vr.pos, vr.size);
		f32 start_y = MAX(end.y - 4 - 3 * text_spec.font->baseSize, vr.pos.y);
		end.y -= text_spec.font->baseSize;
		v2 at = {.x = vr.pos.x + 4, .y = start_y};

		at.y += draw_text(view->dynamic_range.name, at, &text_spec).y;
		if (at.y < end.y) at.y += draw_text(view->threshold.name, at, &text_spec).y;
		if (at.y < end.y) at.y += draw_text(view->gamma.name, at, &text_spec).y;

		at.y  = start_y;
		at.x += max_prefix_width + 8;
		text_spec.limits.size.x = end.x - at.x;

		v2 size = draw_variable(ui, a, &view->dynamic_range, at, mouse, RULER_COLOUR, text_spec);

		f32 max_center_width = size.w;
		at.y += size.h;

		if (at.y < end.y) {
			size = draw_variable(ui, a, &view->threshold, at, mouse, RULER_COLOUR, text_spec);
			max_center_width = MAX(size.w, max_center_width);
			at.y += size.h;
		}

		if (at.y < end.y) {
			size = draw_variable(ui, a, &view->gamma, at, mouse, RULER_COLOUR, text_spec);
			max_center_width = MAX(size.w, max_center_width);
			at.y += size.h;
		}

		at.y  = start_y;
		at.x += max_center_width + 8;
		text_spec.limits.size.x = end.x - at.x;
		draw_text(s8(" [dB]"), at, &text_spec);
	}
}

static v2
draw_compute_progress_bar(BeamformerUI *ui, Arena arena, ComputeProgressBar *state, Rect r)
{
	if (*state->processing) state->display_t_velocity += 65 * dt_for_frame;
	else                    state->display_t_velocity -= 45 * dt_for_frame;

	state->display_t_velocity = CLAMP(state->display_t_velocity, -10, 10);
	state->display_t += state->display_t_velocity * dt_for_frame;
	state->display_t  = CLAMP01(state->display_t);

	if (state->display_t > (1.0 / 255.0)) {
		Rect outline = {.pos = r.pos, .size = {.w = r.size.w, .h = ui->font.baseSize}};
		outline      = scale_rect_centered(outline, (v2){.x = 0.96, .y = 0.7});
		Rect filled  = outline;
		filled.size.w *= *state->progress;
		DrawRectangleRounded(filled.rl, 2, 0, fade(colour_from_normalized(HOVERED_COLOUR),
		                                           state->display_t));
		DrawRectangleRoundedLinesEx(outline.rl, 2, 0, 3, fade(BLACK, state->display_t));
	}

	v2 result = {.x = r.size.w, .y = ui->font.baseSize};
	return result;
}

static v2
draw_compute_stats_view(BeamformerCtx *ctx, Arena arena, ComputeShaderStats *stats, Rect r)
{
	static s8 labels[CS_LAST] = {
		#define X(e, n, s, h, pn) [CS_##e] = s8(pn ":"),
		COMPUTE_SHADERS
		#undef X
	};

	BeamformerUI *ui        = ctx->ui;
	s8  compute_total       = s8("Compute Total:");
	f32 compute_total_width = measure_text(ui->font, compute_total).w;
	f32 max_label_width     = compute_total_width;

	f32 *label_widths = alloc(&arena, f32, ARRAY_COUNT(labels));
	for (u32 i = 0; i < ARRAY_COUNT(labels); i++) {
		label_widths[i] = measure_text(ui->font, labels[i]).x;
		max_label_width = MAX(label_widths[i], max_label_width);
	}

	v2 at = r.pos;
	Stream buf = stream_alloc(&arena, 64);
	f32 compute_time_sum = 0;
	u32 stages = ctx->params->compute_stages_count;
	TextSpec text_spec = {.font = &ui->font, .colour = NORMALIZED_FG_COLOUR, .flags = TF_LIMITED};
	for (u32 i = 0; i < stages; i++) {
		u32 index  = ctx->params->compute_stages[i];
		text_spec.limits.size.x = r.size.w;
		draw_text(labels[index], at, &text_spec);
		text_spec.limits.size.x -= LISTING_ITEM_PAD + max_label_width;

		stream_reset(&buf, 0);
		stream_append_f64_e(&buf, stats->times[index]);
		stream_append_s8(&buf, s8(" [s]"));
		v2 rpos = {.x = r.pos.x + max_label_width + LISTING_ITEM_PAD, .y = at.y};
		at.y += draw_text(stream_to_s8(&buf), rpos, &text_spec).h;

		compute_time_sum += stats->times[index];
	}

	stream_reset(&buf, 0);
	stream_append_f64_e(&buf, compute_time_sum);
	stream_append_s8(&buf, s8(" [s]"));
	v2 rpos = {.x = at.x + max_label_width + LISTING_ITEM_PAD, .y = at.y};
	text_spec.limits.size.w = r.size.w;
	draw_text(compute_total, at, &text_spec);
	text_spec.limits.size.w -= LISTING_ITEM_PAD - max_label_width;
	at.y -= draw_text(stream_to_s8(&buf), rpos, &text_spec).h;

	v2 result = {.x = r.size.w, .y = at.y - r.pos.y};
	return result;
}

static void
draw_ui_view(BeamformerUI *ui, Variable *ui_view, Rect r, v2 mouse, TextSpec text_spec)
{
	ASSERT(ui_view->type == VT_UI_VIEW);
	UIView *view = &ui_view->u.view;

	/* TODO(rnp): this should get moved up to draw_variable */
	hover_var(ui, mouse, r, ui_view);

	if (view->needed_height - r.size.h < view->offset)
		view->offset = view->needed_height - r.size.h;

	if (view->needed_height - r.size.h < 0)
		view->offset = 0;

	r.pos.y -= view->offset;

	f32 start_height = r.size.h;
	Variable *var    = view->group.first;
	f32 x_off        = view->group.max_name_width;
	while (var) {
		s8 suffix   = {0};
		v2 at       = r.pos;
		f32 advance = 0;
		switch (var->type) {
		case VT_BEAMFORMER_VARIABLE: {
			BeamformerVariable *bv = &var->u.beamformer_variable;

			suffix   = bv->suffix;
	                text_spec.limits.size.w = r.size.w + r.pos.x - at.x;
			advance  = draw_text(var->name, at, &text_spec).y;
			at.x    += x_off + LISTING_ITEM_PAD;
	                text_spec.limits.size.w = r.size.w + r.pos.x - at.x;
			at.x    += draw_variable(ui, ui->arena, var, at, mouse, FG_COLOUR, text_spec).x;

			while (var) {
				if (var->next) {
					var = var->next;
					break;
				}
				var       = var->parent;
				r.pos.x  -= LISTING_INDENT;
				r.size.x += LISTING_INDENT;
				x_off    += LISTING_INDENT;
			}
		} break;
		case VT_GROUP: {
			VariableGroup *g = &var->u.group;

	                text_spec.limits.size.w = r.size.w + r.pos.x - at.x;
			advance  = draw_variable(ui, ui->arena, var, at, mouse, FG_COLOUR, text_spec).y;
			at.x    += x_off + LISTING_ITEM_PAD;
			if (g->expanded) {
				cut_rect_horizontal(r, LISTING_INDENT, 0, &r);
				x_off -= LISTING_INDENT;
				var   = g->first;
			} else {
				Variable *v = g->first;

				ASSERT(!v || v->type == VT_BEAMFORMER_VARIABLE);
				/* NOTE(rnp): assume the suffix is the same for all elements */
				if (v) suffix = v->u.beamformer_variable.suffix;

				switch (g->type) {
				case VG_LIST: break;
				case VG_V2:
				case VG_V4: {
					text_spec.limits.size.w = r.size.w + r.pos.x - at.x;
					at.x += draw_text(s8("{"), at, &text_spec).x;
					while (v) {
						text_spec.limits.size.w = r.size.w + r.pos.x - at.x;
						at.x += draw_variable(ui, ui->arena, v, at, mouse,
						                      FG_COLOUR, text_spec).x;

						v = v->next;
						if (v) {
							text_spec.limits.size.w = r.size.w + r.pos.x - at.x;
							at.x += draw_text(s8(", "), at, &text_spec).x;
						}
					}
					text_spec.limits.size.w = r.size.w + r.pos.x - at.x;
					at.x += draw_text(s8("}"), at, &text_spec).x;
				} break;
				}

				var = var->next;
			}
		} break;
		case VT_BEAMFORMER_FRAME_VIEW: {
			BeamformerFrameView *bv = var->u.generic;
			if (frame_view_ready_to_present(bv))
				draw_beamformer_frame_view(ui, ui->arena, var, r, mouse);
			var = var->next;
		} break;
		case VT_COMPUTE_PROGRESS_BAR: {
			ComputeProgressBar *bar = &var->u.compute_progress_bar;
			advance = draw_compute_progress_bar(ui, ui->arena, bar, r).y;
			var = var->next;
		} break;
		case VT_COMPUTE_LATEST_STATS_VIEW:
		case VT_COMPUTE_STATS_VIEW: {
			ComputeShaderStats *stats = var->u.compute_stats_view.stats;
			if (var->type == VT_COMPUTE_LATEST_STATS_VIEW)
				stats = *(ComputeShaderStats **)stats;
			advance = draw_compute_stats_view(var->u.compute_stats_view.ctx, ui->arena, stats, r).y;
			var = var->next;
		} break;
		default: INVALID_CODE_PATH;
		}

		if (suffix.len) {
			v2 suffix_s = measure_text(ui->font, suffix);
			if (r.size.w + r.pos.x - LISTING_ITEM_PAD - suffix_s.x > at.x) {
				v2 suffix_p = {.x = r.pos.x + r.size.w - suffix_s.w, .y = r.pos.y};
				draw_text(suffix, suffix_p, &text_spec);
			}
		}

		/* NOTE(rnp): we want to let this overflow to the desired size */
		r.pos.y  += advance + LISTING_LINE_PAD;
		r.size.y -= advance + LISTING_LINE_PAD;
	}
	view->needed_height = start_height - r.size.h;
}

static void
draw_active_text_box(BeamformerUI *ui, Variable *var)
{
	InputState *is = &ui->text_input_state;
	Rect box       = is->rect;

	s8 text          = {.len = is->buf_len, .data = is->buf};
	v2 text_size     = measure_text(*is->font, text);
	v2 text_position = {.x = box.pos.x, .y = box.pos.y + (box.size.h - text_size.h) / 2};

	f32 cursor_width   = (is->cursor == is->buf_len) ? 16 : 4;
	f32 cursor_offset  = measure_text(*is->font, (s8){.data = text.data, .len = is->cursor}).w;
	cursor_offset     += text_position.x;

	box.size.w = MAX(box.size.w, text_size.w + cursor_width);
	Rect background = extend_rect_centered(box, (v2){.x = 12, .y = 8});
	box = extend_rect_centered(box, (v2){.x = 8, .y = 4});

	Rect cursor = {
		.pos  = {.x = cursor_offset, .y = text_position.y},
		.size = {.w = cursor_width,  .h = text_size.h},
	};

	v4 cursor_colour = FOCUSED_COLOUR;
	cursor_colour.a  = CLAMP01(is->cursor_blink_t);

	Color c = colour_from_normalized(lerp_v4(FG_COLOUR, HOVERED_COLOUR, var->hover_t));
	TextSpec text_spec = {.font = is->font, .colour = c};

	DrawRectangleRounded(background.rl, 0.2, 0, fade(BLACK, 0.8));
	DrawRectangleRounded(box.rl, 0.2, 0, colour_from_normalized(BG_COLOUR));
	draw_text(text, text_position, &text_spec);
	DrawRectanglePro(cursor.rl, (Vector2){0}, 0, colour_from_normalized(cursor_colour));
}

static void
draw_active_menu(BeamformerUI *ui, Arena arena, Variable *menu, v2 mouse, Rect window)
{
	ASSERT(menu->type == VT_GROUP);
	MenuState *ms = &ui->menu_state;

	f32 font_height     = ms->font->baseSize;
	f32 max_label_width = 0;

	Variable *item = menu->u.group.first;
	i32 item_count = 0;
	b32 radio = 0;
	while (item) {
		max_label_width = MAX(max_label_width, item->name_width);
		radio |= (item->flags & V_RADIO_BUTTON) != 0;
		item_count++;
		item = item->next;
	}

	f32 radio_button_width = radio? ms->font->baseSize : 0;
	v2  at          = ms->at;
	f32 menu_width  = max_label_width + radio_button_width + 8;
	f32 menu_height = item_count * font_height + (item_count - 1) * 2;
	menu_height = MAX(menu_height, 0);

	if (at.x + menu_width > window.size.w)
		at.x = window.size.w - menu_width  - 16;
	if (at.y + menu_height > window.size.h)
		at.y = window.size.h - menu_height - 12;
	/* TODO(rnp): scroll menu if it doesn't fit on screen */

	Rect menu_rect = {.pos = at, .size = {.w = menu_width, .h = menu_height}};
	Rect bg_rect   = extend_rect_centered(menu_rect, (v2){.x = 12, .y = 8});
	menu_rect      = extend_rect_centered(menu_rect, (v2){.x = 6,  .y = 4});
	DrawRectangleRounded(bg_rect.rl,   0.1, 0, fade(BLACK, 0.8));
	DrawRectangleRounded(menu_rect.rl, 0.1, 0, colour_from_normalized(BG_COLOUR));
	v2 start = at;
	for (i32 i = 0; i < item_count - 1; i++) {
		at.y += 2 + font_height;
		DrawLineEx((v2){.x = at.x - 3, .y = at.y}.rl,
		           add_v2(at, (v2){.w = menu_width + 3}).rl, 2, fade(BLACK, 0.8));
	}

	item = menu->u.group.first;
	TextSpec text_spec = {.font = ms->font, .colour = NORMALIZED_FG_COLOUR,
	                      .limits.size.w = menu_width};
	at = start;
	while (item) {
		at.x = start.x;
		if (item->flags & V_RADIO_BUTTON) {
			draw_text(item->name, at, &text_spec);
			at.x += max_label_width + 8;
		}
		at.y += draw_variable(ui, arena, item, at, mouse, FG_COLOUR, text_spec).y + 2;
		item = item->next;
	}
}

static void
draw_layout_variable(BeamformerUI *ui, Variable *var, Rect draw_rect, v2 mouse)
{
	if (var->type != VT_UI_REGION_SPLIT) {
		v2 shrink = {.x = UI_REGION_PAD, .y = UI_REGION_PAD};
		draw_rect = shrink_rect_centered(draw_rect, shrink);
		BeginScissorMode(draw_rect.pos.x, draw_rect.pos.y, draw_rect.size.w, draw_rect.size.h);
		draw_rect = draw_title_bar(ui, ui->arena, var, draw_rect, mouse);
		EndScissorMode();
	}

	/* TODO(rnp): post order traversal of the ui tree will remove the need for this */
	if (!CheckCollisionPointRec(mouse.rl, draw_rect.rl))
		mouse = (v2){.x = F32_INFINITY, .y = F32_INFINITY};

	BeginScissorMode(draw_rect.pos.x, draw_rect.pos.y, draw_rect.size.w, draw_rect.size.h);
	switch (var->type) {
	case VT_UI_VIEW: {
		TextSpec text_spec = {.font = &ui->font, .colour = NORMALIZED_FG_COLOUR, .flags = TF_LIMITED};
		draw_ui_view(ui, var, draw_rect, mouse, text_spec);
	} break;
	case VT_UI_REGION_SPLIT: {
		RegionSplit *rs = &var->u.region_split;

		Rect split, hover;
		switch (rs->direction) {
		case RSD_VERTICAL: {
			split_rect_vertical(draw_rect, rs->fraction, 0, &split);
			split.pos.x  += UI_REGION_PAD;
			split.pos.y  -= UI_SPLIT_HANDLE_THICK / 2;
			split.size.h  = UI_SPLIT_HANDLE_THICK;
			split.size.w -= 2 * UI_REGION_PAD;
			hover = extend_rect_centered(split, (v2){.y = 0.75 * UI_REGION_PAD});
		} break;
		case RSD_HORIZONTAL: {
			split_rect_horizontal(draw_rect, rs->fraction, 0, &split);
			split.pos.x  -= UI_SPLIT_HANDLE_THICK / 2;
			split.pos.y  += UI_REGION_PAD;
			split.size.w  = UI_SPLIT_HANDLE_THICK;
			split.size.h -= 2 * UI_REGION_PAD;
			hover = extend_rect_centered(split, (v2){.x = 0.75 * UI_REGION_PAD});
		} break;
		}

		hover_var(ui, mouse, hover, var);

		v4 colour = HOVERED_COLOUR;
		colour.a  = var->hover_t;
		DrawRectangleRounded(split.rl, 0.6, 0, colour_from_normalized(colour));
	} break;
	default: INVALID_CODE_PATH; break;
	}
	EndScissorMode();
}

static void
draw_ui_regions(BeamformerUI *ui, Rect window, v2 mouse)
{
	struct region_frame {
		Variable *var;
		Rect      rect;
	} init[16];

	struct {
		struct region_frame *data;
		iz count;
		iz capacity;
	} stack = {init, 0, ARRAY_COUNT(init)};

	TempArena arena_savepoint = begin_temp_arena(&ui->arena);

	*da_push(&ui->arena, &stack) = (struct region_frame){ui->regions, window};
	while (stack.count) {
		struct region_frame *top = stack.data + --stack.count;
		Rect rect = top->rect;
		draw_layout_variable(ui, top->var, rect, mouse);

		if (top->var->type == VT_UI_REGION_SPLIT) {
			Rect first, second;
			RegionSplit *rs = &top->var->u.region_split;
			switch (rs->direction) {
			case RSD_VERTICAL: {
				split_rect_vertical(rect, rs->fraction, &first, &second);
			} break;
			case RSD_HORIZONTAL: {
				split_rect_horizontal(rect, rs->fraction, &first, &second);
			} break;
			}

			*da_push(&ui->arena, &stack) = (struct region_frame){rs->right, second};
			*da_push(&ui->arena, &stack) = (struct region_frame){rs->left,  first};
		}
	}

	end_temp_arena(arena_savepoint);
}

static void
ui_store_variable_base(VariableType type, void *store, void *new_value, void *limits)
{
	switch (type) {
	case VT_B32: {
		*(b32 *)store = *(b32 *)new_value;
	} break;
	case VT_F32: {
		v2 *lim = limits;
		f32 val = *(f32 *)new_value;
		*(f32 *)store = CLAMP(val, lim->x, lim->y);
	} break;
	default: INVALID_CODE_PATH;
	}
}

static void
ui_store_variable(Variable *var, void *new_value)
{
	switch (var->type) {
	case VT_F32: {
		v2 limits = {.x = -F32_INFINITY, .y = F32_INFINITY};
		ui_store_variable_base(VT_F32, &var->u.f32, new_value, &limits);
	} break;
	case VT_BEAMFORMER_VARIABLE: {
		BeamformerVariable *bv = &var->u.beamformer_variable;
		ui_store_variable_base(bv->store_type, bv->store, new_value, &bv->params.limits);
	} break;
	default: INVALID_CODE_PATH;
	}
}

static void
scroll_interaction_base(VariableType type, void *store, f32 delta)
{
	switch (type) {
	case VT_B32: { *(b32 *)store  = !*(b32 *)store; } break;
	case VT_F32: { *(f32 *)store += delta;          } break;
	case VT_I32: { *(i32 *)store += delta;          } break;
	default: INVALID_CODE_PATH;
	}
}

static void
scroll_interaction(Variable *var, f32 delta)
{
	switch (var->type) {
	case VT_BEAMFORMER_VARIABLE: {
		BeamformerVariable *bv = &var->u.beamformer_variable;
		scroll_interaction_base(bv->store_type, bv->store, delta * bv->params.scroll_scale);
		ui_store_variable(var, bv->store);
	} break;
	case VT_UI_VIEW: {
		scroll_interaction_base(VT_F32, &var->u.view.offset, UI_SCROLL_SPEED * delta);
		var->u.view.offset = MAX(0, var->u.view.offset);
	} break;
	default: scroll_interaction_base(var->type, &var->u, delta); break;
	}
}

static void
begin_menu_input(MenuState *ms, v2 mouse)
{
	ms->at   = mouse;
	ms->font = ms->hot_font;
}

static void
begin_text_input(InputState *is, Variable *var, v2 mouse)
{
	Stream s = {.cap = ARRAY_COUNT(is->buf), .data = is->buf};
	stream_append_variable(&s, var);
	is->buf_len = s.widx;
	is->rect    = is->hot_rect;
	is->font    = is->hot_font;

	/* NOTE: extra offset to help with putting a cursor at idx 0 */
	#define TEXT_HALF_CHAR_WIDTH 10
	f32 hover_p = CLAMP01((mouse.x - is->rect.pos.x) / is->rect.size.w);
	f32 x_off = TEXT_HALF_CHAR_WIDTH, x_bounds = is->rect.size.w * hover_p;
	i32 i;
	for (i = 0; i < is->buf_len && x_off < x_bounds; i++) {
		/* NOTE: assumes font glyphs are ordered ASCII */
		i32 idx  = is->buf[i] - 0x20;
		x_off   += is->font->glyphs[idx].advanceX;
		if (is->font->glyphs[idx].advanceX == 0)
			x_off += is->font->recs[idx].width;
	}
	is->cursor = i;
}

static void
end_text_input(InputState *is, Variable *var)
{
	f32 scale = 1;
	if (var->type == VT_BEAMFORMER_VARIABLE) {
		BeamformerVariable *bv = &var->u.beamformer_variable;
		ASSERT(bv->store_type == VT_F32);
		scale = bv->params.display_scale;
		var->hover_t = 0;
	}
	f32 value = parse_f64((s8){.len = is->buf_len, .data = is->buf}) / scale;
	ui_store_variable(var, &value);
}

static void
update_text_input(InputState *is, Variable *var)
{
	ASSERT(is->cursor != -1);

	is->cursor_blink_t += is->cursor_blink_scale * dt_for_frame;
	if (is->cursor_blink_t >= 1) is->cursor_blink_scale = -1.5f;
	if (is->cursor_blink_t <= 0) is->cursor_blink_scale =  1.5f;

	var->hover_t -= 2 * HOVER_SPEED * dt_for_frame;
	var->hover_t  = CLAMP01(var->hover_t);

	/* NOTE: handle multiple input keys on a single frame */
	i32 key = GetCharPressed();
	while (key > 0) {
		if ((is->buf_len == ARRAY_COUNT(is->buf)) || (is->cursor == ARRAY_COUNT(is->buf) - 1))
			break;

		b32 allow_key = ((key >= '0' && key <= '9') || (key == '.') ||
		                 (key == '-' && is->cursor == 0));
		if (allow_key) {
			mem_move(is->buf + is->cursor + 1,
			         is->buf + is->cursor,
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
		if (is->cursor < ARRAY_COUNT(is->buf) - 1) {
			mem_move(is->buf + is->cursor,
			         is->buf + is->cursor + 1,
			         is->buf_len - is->cursor);
		}
		is->buf_len--;
	}

	if ((IsKeyPressed(KEY_DELETE) || IsKeyPressedRepeat(KEY_DELETE)) && is->cursor < is->buf_len) {
		mem_move(is->buf + is->cursor,
			 is->buf + is->cursor + 1,
		         is->buf_len - is->cursor);
		is->buf_len--;
	}
}

static void
display_interaction_end(BeamformerUI *ui)
{
	b32 is_hot    = ui->interaction.hot_type == IT_DISPLAY;
	b32 is_active = ui->interaction.type     == IT_DISPLAY;
	if ((is_active && is_hot) || ui->ruler_state == RS_HOLD)
		return;
	ui->ruler_state = RS_NONE;
}

static void
display_interaction(BeamformerUI *ui, v2 mouse, f32 scroll)
{
	b32 mouse_left_pressed  = IsMouseButtonPressed(MOUSE_BUTTON_LEFT);
	b32 mouse_right_pressed = IsMouseButtonPressed(MOUSE_BUTTON_RIGHT);
	b32 is_hot              = ui->interaction.hot_type == IT_DISPLAY;
	b32 is_active           = ui->interaction.type     == IT_DISPLAY;

	if (scroll) {
		ASSERT(ui->interaction.active->type == VT_BEAMFORMER_FRAME_VIEW);
		BeamformerFrameView *bv = ui->interaction.active->u.generic;
		bv->threshold.u.f32 += scroll;
		bv->needs_update = 1;
	}

	if (mouse_left_pressed && is_active) {
		ui->ruler_state++;
		switch (ui->ruler_state) {
		case RS_START: ui->ruler_start_p = mouse; break;
		case RS_HOLD:  ui->ruler_stop_p  = mouse; break;
		default:
			ui->ruler_state = RS_NONE;
			break;
		}
	} else if (mouse_right_pressed && is_hot) {
		ui->ruler_state = RS_NONE;
	}
}

static void
scale_bar_interaction(BeamformerCtx *ctx, v2 mouse)
{
	BeamformerUI *ui        = ctx->ui;
	InteractionState *is    = &ui->interaction;
	ScaleBar *sb            = is->active->u.generic;
	b32 mouse_left_pressed  = IsMouseButtonPressed(MOUSE_BUTTON_LEFT);
	b32 mouse_right_pressed = IsMouseButtonPressed(MOUSE_BUTTON_RIGHT);
	f32 mouse_wheel         = GetMouseWheelMoveV().y;

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

			v2_sll *savepoint = SLLPop(ui->scale_bar_savepoint_freelist);
			if (!savepoint) savepoint = push_struct(&ui->arena, v2_sll);

			savepoint->v.x = *sb->min_value;
			savepoint->v.y = *sb->max_value;
			SLLPush(savepoint, sb->savepoint_stack);

			*sb->min_value = MAX(min, sb->limits.x);
			*sb->max_value = MIN(max, sb->limits.y);

			sb->zoom_starting_point = (v2){.x = F32_INFINITY, .y = F32_INFINITY};
			if (sb->causes_compute)
				ui->flush_params = 1;
		}
	}

	if (mouse_right_pressed) {
		v2_sll *savepoint = sb->savepoint_stack;
		if (savepoint) {
			if (sb->causes_compute)
				ui->flush_params = 1;
			*sb->min_value      = savepoint->v.x;
			*sb->max_value      = savepoint->v.y;
			sb->savepoint_stack = SLLPush(savepoint, ui->scale_bar_savepoint_freelist);
		}
		sb->zoom_starting_point = (v2){.x = F32_INFINITY, .y = F32_INFINITY};
	}

	if (mouse_wheel) {
		*sb->min_value += mouse_wheel * sb->scroll_scale.x;
		*sb->max_value += mouse_wheel * sb->scroll_scale.y;
		*sb->min_value  = MAX(sb->limits.x, *sb->min_value);
		*sb->max_value  = MIN(sb->limits.y, *sb->max_value);
		if (sb->causes_compute)
			ui->flush_params = 1;
	}
}

static void
ui_button_interaction(BeamformerUI *ui, Variable *button)
{
	ASSERT(button->type == VT_UI_BUTTON);
	switch (button->u.button) {
	case UI_BID_FV_COPY_HORIZONTAL: ui_copy_frame(ui, button, RSD_HORIZONTAL);  break;
	case UI_BID_FV_COPY_VERTICAL:   ui_copy_frame(ui, button, RSD_VERTICAL);    break;
	case UI_BID_FV_EXPORT:          ui_export_view(ui, button->parent->parent); break;
	case UI_BID_CLOSE_VIEW: {
		Variable *view   = button->parent;
		Variable *region = view->parent;
		ASSERT(view->type == VT_UI_VIEW && region->type == VT_UI_REGION_SPLIT);

		Variable *parent    = region->parent;
		Variable *remaining = region->u.region_split.left;
		if (remaining == view) remaining = region->u.region_split.right;

		ui_view_free(ui, view);

		ASSERT(parent->type == VT_UI_REGION_SPLIT);
		if (parent->u.region_split.left == region) {
			parent->u.region_split.left  = remaining;
		} else {
			parent->u.region_split.right = remaining;
		}
		remaining->parent = parent;

		SLLPush(region, ui->variable_freelist);
	} break;
	}
}

static void
ui_begin_interact(BeamformerUI *ui, BeamformerInput *input, b32 scroll, b32 mouse_left_pressed)
{
	InteractionState *is = &ui->interaction;
	if (is->hot_type != IT_NONE) {
		is->type = is->hot_type;
	} else if (is->hot) {
		switch (is->hot->type) {
		case VT_NULL: is->type = IT_NOP; break;
		case VT_B32:  is->type = IT_SET; break;
		case VT_UI_REGION_SPLIT: { is->type = IT_DRAG; }                 break;
		case VT_UI_VIEW:         { if (scroll) is->type = IT_SCROLL; }   break;
		case VT_UI_BUTTON:       { ui_button_interaction(ui, is->hot); } break;
		case VT_GROUP: {
			if (mouse_left_pressed && is->hot->flags & V_MENU) {
				is->type = IT_MENU;
				begin_menu_input(&ui->menu_state, input->mouse);
			} else {
				is->type = IT_SET;
			}
		} break;
		case VT_BEAMFORMER_VARIABLE: {
			if (is->hot->u.beamformer_variable.store_type == VT_B32) {
				is->type = IT_SET;
				break;
			}
		} /* FALLTHROUGH */
		case VT_F32: {
			if (scroll) {
				is->type = IT_SCROLL;
			} else if (mouse_left_pressed && is->hot->flags & V_TEXT) {
				is->type = IT_TEXT;
				begin_text_input(&ui->text_input_state, is->hot, input->mouse);
			}
		} break;
		default: INVALID_CODE_PATH;
		}
	}
	if (is->type != IT_NONE) {
		is->active = is->hot;
		if ((iptr)is->hot == (iptr)ui->scratch_variables)
			ui->scratch_variable = ui->scratch_variables + 1;
		else
			ui->scratch_variable = ui->scratch_variables + 0;
	}
}

static void
ui_end_interact(BeamformerCtx *ctx, v2 mouse)
{
	BeamformerUI *ui = ctx->ui;
	InteractionState *is = &ui->interaction;
	switch (is->type) {
	case IT_NOP:  break;
	case IT_SET: {
		switch (is->active->type) {
		case VT_GROUP: {
			is->active->u.group.expanded = !is->active->u.group.expanded;
		} break;
		case VT_B32: {
			is->active->u.b32 = !is->active->u.b32;
		} break;
		case VT_BEAMFORMER_VARIABLE: {
			ASSERT(is->active->u.beamformer_variable.store_type == VT_B32);
			b32 *val = is->active->u.beamformer_variable.store;
			*val     = !(*val);
		} break;
		default: INVALID_CODE_PATH;
		}
	} break;
	case IT_DISPLAY: display_interaction_end(ui); break;
	case IT_SCROLL:  scroll_interaction(is->active, GetMouseWheelMoveV().y); break;
	case IT_TEXT:    end_text_input(&ui->text_input_state, is->active);      break;
	case IT_MENU:      break;
	case IT_SCALE_BAR: break;
	case IT_DRAG:      break;
	default: INVALID_CODE_PATH;
	}

	/* TODO(rnp): better way of clearing the state when the parent is a menu */
	if (is->active->parent && is->active->parent->flags & V_MENU)
		is->active->hover_t = 0;

	if (is->active->flags & V_CAUSES_COMPUTE)
		ui->flush_params = 1;
	if (is->active->flags & V_UPDATE_VIEW) {
		Variable *frame_view = is->active->parent;
		ASSERT(frame_view && frame_view->type == VT_BEAMFORMER_FRAME_VIEW);
		((BeamformerFrameView *)frame_view->u.generic)->needs_update = 1;
	}

	is->type   = IT_NONE;
	is->active = 0;
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
		if (is->type != IT_NONE)
			ui_end_interact(ctx, input->mouse);
		ui_begin_interact(ui, input, wheel_moved, mouse_left_pressed);
	}

	if (IsKeyPressed(KEY_ENTER) && is->type == IT_TEXT)
		ui_end_interact(ctx, input->mouse);

	switch (is->type) {
	case IT_NONE: break;
	case IT_NOP:  break;
	case IT_MENU: break;
	case IT_DISPLAY: display_interaction(ui, input->mouse, GetMouseWheelMoveV().y); break;
	case IT_SCROLL:  ui_end_interact(ctx, input->mouse);                            break;
	case IT_SET:     ui_end_interact(ctx, input->mouse);                            break;
	case IT_TEXT:    update_text_input(&ui->text_input_state, is->active);          break;
	case IT_DRAG: {
		if (!IsMouseButtonDown(MOUSE_BUTTON_LEFT) && !IsMouseButtonDown(MOUSE_BUTTON_RIGHT)) {
			ui_end_interact(ctx, input->mouse);
		} else {
			v2 ws     = (v2){.w = ctx->window_size.w, .h = ctx->window_size.h};
			v2 dMouse = sub_v2(input->mouse, input->last_mouse);
			dMouse    = mul_v2(dMouse, (v2){.x = 1.0f / ws.w, .y = 1.0f / ws.h});

			switch (is->active->type) {
			case VT_UI_REGION_SPLIT: {
				f32 min_fraction;
				RegionSplit *rs = &is->active->u.region_split;
				switch (rs->direction) {
				case RSD_VERTICAL: {
					min_fraction  = (UI_SPLIT_HANDLE_THICK + 0.5 * UI_REGION_PAD) / ws.h;
					rs->fraction += dMouse.y;
				} break;
				case RSD_HORIZONTAL: {
					min_fraction  = (UI_SPLIT_HANDLE_THICK + 0.5 * UI_REGION_PAD) / ws.w;
					rs->fraction += dMouse.x;
				} break;
				}
				rs->fraction = CLAMP(rs->fraction, min_fraction, 1 - min_fraction);
			} break;
			default: break;
			}

			if (is->active != is->hot) {
				is->active->hover_t += HOVER_SPEED * dt_for_frame;
				is->active->hover_t  = CLAMP01(is->active->hover_t);
			}
		}
	} break;
	case IT_SCALE_BAR: scale_bar_interaction(ctx, input->mouse); break;
	}

	is->hot_type = IT_NONE;
	is->hot      = 0;
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

	BeamformerUI *ui = ctx->ui;

	/* NOTE(rnp): unload old data from GPU */
	if (ui) {
		UnloadFont(ui->font);
		UnloadFont(ui->small_font);

		for (BeamformerFrameView *view = ui->views; view; view = view->next)
			if (view->rendered_view.id)
				UnloadRenderTexture(view->rendered_view);
	}

	ui = ctx->ui = push_struct(&store, typeof(*ui));
	ui->os    = &ctx->os;
	ui->arena = store;

	/* TODO: build these into the binary */
	/* TODO(rnp): better font, this one is jank at small sizes */
	ui->font       = LoadFontEx("assets/IBMPlexSans-Bold.ttf", 28, 0, 0);
	ui->small_font = LoadFontEx("assets/IBMPlexSans-Bold.ttf", 20, 0, 0);

	ui->scratch_variable = ui->scratch_variables + 0;
	Variable *split = ui->regions   = add_ui_split(ui, 0, &ui->arena, s8("UI Root"), 0.4,
	                                               RSD_HORIZONTAL, ui->font);
	split->u.region_split.left      = add_ui_split(ui, split, &ui->arena, s8(""), 0.475,
	                                               RSD_VERTICAL, ui->font);
	split->u.region_split.right     = add_beamformer_frame_view(ui, split, &ui->arena, FVT_LATEST, 0);

	BeamformerFrameView *bv         = split->u.region_split.right->u.group.first->u.generic;
	bv->lateral_scale_bar.min_value = &ui->params.output_min_coordinate.x;
	bv->lateral_scale_bar.max_value = &ui->params.output_max_coordinate.x;
	bv->axial_scale_bar.min_value   = &ui->params.output_min_coordinate.z;
	bv->axial_scale_bar.max_value   = &ui->params.output_max_coordinate.z;
	bv->axial_scale_bar.causes_compute   = 1;
	bv->lateral_scale_bar.causes_compute = 1;
	bv->axial_scale_bar_active->u.b32    = 1;
	bv->lateral_scale_bar_active->u.b32  = 1;
	bv->ctx = &ctx->fsctx;

	split = split->u.region_split.left;
	split->u.region_split.left  = add_beamformer_parameters_view(split, ctx);
	split->u.region_split.right = add_ui_split(ui, split, &ui->arena, s8(""), 0.22,
	                                           RSD_VERTICAL, ui->font);
	split = split->u.region_split.right;

	split->u.region_split.left  = add_compute_progress_bar(split, ctx);
	split->u.region_split.right = add_compute_stats_view(ui, split, &ui->arena,
	                                                     VT_COMPUTE_LATEST_STATS_VIEW);

	ComputeStatsView *compute_stats = &split->u.region_split.right->u.group.first->u.compute_stats_view;
	compute_stats->ctx   = ctx;
	compute_stats->stats = &ui->latest_compute_stats;

	ctx->ui_read_params = 1;
}

static void
validate_ui_parameters(BeamformerUIParameters *p)
{
	if (p->output_min_coordinate.x > p->output_max_coordinate.x)
		SWAP(p->output_min_coordinate.x, p->output_max_coordinate.x)
	if (p->output_min_coordinate.z > p->output_max_coordinate.z)
		SWAP(p->output_min_coordinate.z, p->output_max_coordinate.z)
}

static void
draw_ui(BeamformerCtx *ctx, BeamformerInput *input, BeamformFrame *frame_to_draw,
        ComputeShaderStats *latest_compute_stats)
{
	BeamformerUI *ui = ctx->ui;

	if (frame_to_draw->ready_to_present) ui->latest_frame = frame_to_draw;
	ui->latest_compute_stats = latest_compute_stats;

	/* TODO(rnp): there should be a better way of detecting this */
	if (ctx->ui_read_params) {
		mem_copy(&ui->params, &ctx->params->raw.output_min_coordinate, sizeof(ui->params));
		ui->flush_params    = 0;
		ctx->ui_read_params = 0;
	}

	/* NOTE: process interactions first because the user interacted with
	 * the ui that was presented last frame */
	ui_interact(ctx, input);

	if (ui->flush_params) {
		validate_ui_parameters(&ui->params);
		if (!ctx->csctx.processing_compute) {
			mem_copy(&ctx->params->raw.output_min_coordinate, &ui->params, sizeof(ui->params));
			ui->flush_params    = 0;
			ctx->params->upload = 1;
			ctx->start_compute  = 1;
		}
	}

	/* NOTE(rnp): can't render to a different framebuffer in the middle of BeginDrawing()... */
	update_frame_views(ui);

	BeginDrawing();
		ClearBackground(colour_from_normalized(BG_COLOUR));

		v2 mouse         = input->mouse;
		Rect window_rect = {.size = {.w = ctx->window_size.w, .h = ctx->window_size.h}};

		draw_ui_regions(ui, window_rect, mouse);
		if (ui->interaction.type == IT_TEXT)
			draw_active_text_box(ui, ui->interaction.active);
		if (ui->interaction.type == IT_MENU)
			draw_active_menu(ui, ui->arena, ui->interaction.active, mouse, window_rect);
	EndDrawing();
}
