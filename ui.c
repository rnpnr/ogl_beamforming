/* See LICENSE for license details. */
/* TODO(rnp):
 * [ ]: refactor: ui kind of needs to be mostly thrown away
 *      - want all drawing to be immediate mode
 *      - only layout information should be retained
 *        - leaf nodes of layout have a kind associated which
 *          instructs the builder code on how to build the view
 *      - ui items (currently called Variables) are stored in a hash
 *        table and are looked up for state information at frame building time
 *        - removed/recycled when last building frame index is less than drawing frame index
 *      - building:
 *        - loop over tiled layout tree and floating layout tree as is currently done
 *          - this will build current frame ui draw tree
 *        - for each view use a stack structure with grouping, similar to how tables are made
 *          - more general though: sub groups contain a draw axis (x or y)
 *        - each ui item gets looked up in the hash table for previous frame drawing info
 *          - this can then be used to construct a "ui comm" which contains relevant info
 *            about how that ui item is being interacted with
 *      - drawing:
 *        - must be separated into layout constraint solving and rendering
 *        - layout constraint solving handles sizing, clipping, etc.
 *          - this will need multiple passes per subgroup to allow for autosizing
 *          - pay attention to the fixed size points in the hierarchy. fixed size
 *            items are complete once their children are complete.
 *        - rendering simply uses the rect/clipping regions produced by layout
 *          to send draw commands
 * [ ]: bug: resizing live view causes texture to jump around
 * [ ]: bug: group at end of parameter listing
 * [ ]: refactor: ui should be in its own thread and that thread should only be concerned with the ui
 * [ ]: refactor: remove all the excessive measure_texts (cell drawing, hover_interaction in params table)
 * [ ]: refactor: move remaining fragment shader stuff into ui
 * [ ]: refactor: scale table to rect
 * [ ]: scroll bar for views that don't have enough space
 * [ ]: allow views to collapse to just their title bar
 *      - title bar struct with expanded. Check when pushing onto draw stack; if expanded
 *        do normal behaviour else make size title bar size and ignore the splits fraction.
 * [ ]: enforce a minimum region size or allow regions themselves to scroll
 * [ ]: refactor: add_variable_no_link()
 * [ ]: refactor: draw_text_limited should clamp to rect and measure text itself
 * [ ]: draw the ui with a post-order traversal instead of pre-order traversal
 * [ ]: consider V_HOVER_GROUP and use that to implement submenus
 * [ ]: menu's need to support nested groups
 * [ ]: don't redraw on every refresh; instead redraw on mouse movement/event or when a new frame
 *      arrives. For animations the ui can have a list of "timers" which while active will
 *      do a redraw on every refresh until completed.
 * [ ]: show full non-truncated string on hover
 * [ ]: refactor: hovered element type and show hovered element in full even when truncated
 * [ ]: bug: cross-plane view with different dimensions for each plane
 * [ ]: refactor: make table_skip_rows useful
 * [ ]: refactor: better method of grouping variables for views such as FrameView/ComputeStatsView
 */

#define BG_COLOUR              (v4){{0.15f, 0.12f, 0.13f, 1.0f}}
#define FG_COLOUR              (v4){{0.92f, 0.88f, 0.78f, 1.0f}}
#define FOCUSED_COLOUR         (v4){{0.86f, 0.28f, 0.21f, 1.0f}}
#define HOVERED_COLOUR         (v4){{0.11f, 0.50f, 0.59f, 1.0f}}
#define RULER_COLOUR           (v4){{1.00f, 0.70f, 0.00f, 1.0f}}
#define BORDER_COLOUR          v4_lerp(FG_COLOUR, BG_COLOUR, 0.85f)

#define MENU_PLUS_COLOUR       (v4){{0.33f, 0.42f, 1.00f, 1.00f}}
#define MENU_CLOSE_COLOUR      FOCUSED_COLOUR

read_only global v4 g_colour_palette[] = {
	{{0.32f, 0.20f, 0.50f, 1.00f}},
	{{0.14f, 0.39f, 0.61f, 1.00f}},
	{{0.61f, 0.14f, 0.25f, 1.00f}},
	{{0.20f, 0.60f, 0.24f, 1.00f}},
	{{0.80f, 0.60f, 0.20f, 1.00f}},
	{{0.15f, 0.51f, 0.74f, 1.00f}},
};

#define HOVER_SPEED            5.0f
#define BLINK_SPEED            1.5f

#define TABLE_CELL_PAD_HEIGHT  2.0f
#define TABLE_CELL_PAD_WIDTH   8.0f

#define RULER_TEXT_PAD         10.0f
#define RULER_TICK_LENGTH      20.0f

#define UI_SPLIT_HANDLE_THICK  8.0f
#define UI_REGION_PAD          32.0f

/* TODO(rnp) smooth scroll */
#define UI_SCROLL_SPEED 12.0f

#define LISTING_LINE_PAD    6.0f
#define TITLE_BAR_PAD       6.0f

typedef struct v2_sll {
	struct v2_sll *next;
	v2             v;
} v2_sll;

typedef struct {
	f32 t;
	f32 scale;
} UIBlinker;

typedef struct BeamformerUI BeamformerUI;
typedef struct Variable     Variable;

typedef struct {
	u8   buf[128];
	i32  count;
	i32  cursor;
	b32  numeric;
	UIBlinker cursor_blink;
	Font *font, *hot_font;
	Variable *container;
} InputState;

typedef enum {
	RulerState_None,
	RulerState_Start,
	RulerState_Hold,
} RulerState;

typedef struct {
	v2 start;
	v2 end;
	RulerState state;
} Ruler;

typedef enum {
	SB_LATERAL,
	SB_AXIAL,
} ScaleBarDirection;

typedef struct {
	f32    *min_value, *max_value;
	v2_sll *savepoint_stack;
	v2      scroll_scale;
	f32     zoom_starting_coord;
	ScaleBarDirection direction;
} ScaleBar;

typedef struct { f32 val, scale; } scaled_f32;

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

#define COMPUTE_STATS_VIEW_LIST \
	X(Average, "Average") \
	X(Bar,     "Bar")

#define X(kind, ...) ComputeStatsViewKind_ ##kind,
typedef enum {COMPUTE_STATS_VIEW_LIST ComputeStatsViewKind_Count} ComputeStatsViewKind;
#undef X

typedef struct {
	ComputeShaderStats *compute_shader_stats;
	Variable           *cycler;
	ComputeStatsViewKind kind;
	UIBlinker blink;
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
	VT_U32,
	VT_GROUP,
	VT_CYCLER,
	VT_SCALED_F32,
	VT_BEAMFORMER_VARIABLE,
	VT_BEAMFORMER_FRAME_VIEW,
	VT_COMPUTE_STATS_VIEW,
	VT_COMPUTE_PROGRESS_BAR,
	VT_LIVE_CONTROLS_VIEW,
	VT_LIVE_CONTROLS_STRING,
	VT_SCALE_BAR,
	VT_UI_BUTTON,
	VT_UI_MENU,
	VT_UI_REGION_SPLIT,
	VT_UI_TEXT_BOX,
	VT_UI_VIEW,
	VT_X_PLANE_SHIFT,
} VariableType;

typedef enum {
	VariableGroupKind_List,
	/* NOTE(rnp): special group for vectors with components
	 * stored in separate memory locations */
	VariableGroupKind_Vector,
} VariableGroupKind;

typedef struct {
	VariableGroupKind kind;
	b32       expanded;
	Variable *first;
	Variable *last;
	Variable *container;
} VariableGroup;

typedef enum {
	UIViewFlag_CustomText = 1 << 0,
	UIViewFlag_Floating   = 1 << 1,
} UIViewFlags;

typedef struct {
	Variable *child;
	Variable *close;
	Variable *menu;
	Rect      rect;
	UIViewFlags flags;
} UIView;

/* X(id, text) */
#define FRAME_VIEW_BUTTONS \
	X(FV_COPY_HORIZONTAL, "Copy Horizontal") \
	X(FV_COPY_VERTICAL,   "Copy Vertical")

#define GLOBAL_MENU_BUTTONS \
	X(GM_OPEN_VIEW_RIGHT,   "Open View Right") \
	X(GM_OPEN_VIEW_BELOW,   "Open View Below")

#define X(id, text) UI_BID_ ##id,
typedef enum {
	UI_BID_VIEW_CLOSE,
	GLOBAL_MENU_BUTTONS
	FRAME_VIEW_BUTTONS
} UIButtonID;
#undef X

typedef struct {
	s8  *labels;
	u32 *state;
	u32  cycle_length;
} VariableCycler;

typedef struct {
	s8  suffix;
	f32 display_scale;
	f32 scroll_scale;
	v2  limits;
	f32 *store;
} BeamformerVariable;

typedef struct {
	v3 start_point;
	v3 end_point;
} XPlaneShift;

typedef enum {
	V_INPUT          = 1 << 0,
	V_TEXT           = 1 << 1,
	V_RADIO_BUTTON   = 1 << 2,
	V_EXTRA_ACTION   = 1 << 3,
	V_HIDES_CURSOR   = 1 << 4,
	V_LIVE_CONTROL   = 1 << 28,
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
		ScaleBar            scale_bar;
		UIButtonID          button;
		UIView              view;
		VariableCycler      cycler;
		VariableGroup       group;
		XPlaneShift         x_plane_shift;
		scaled_f32          scaled_real32;
		b32                 bool32;
		i32                 signed32;
		u32                 unsigned32;
		f32                 real32;
	};
	Variable *next;
	Variable *parent;
	VariableFlags flags;
	VariableType  type;

	f32 hover_t;
	f32 name_width;
};

#define BEAMFORMER_FRAME_VIEW_KIND_LIST \
	X(Latest,   "Latest")     \
	X(3DXPlane, "3D X-Plane") \
	X(Indexed,  "Indexed")    \
	X(Copy,     "Copy")

typedef enum {
	#define X(kind, ...) BeamformerFrameViewKind_##kind,
	BEAMFORMER_FRAME_VIEW_KIND_LIST
	#undef X
	BeamformerFrameViewKind_Count,
} BeamformerFrameViewKind;

typedef struct BeamformerFrameView BeamformerFrameView;
struct BeamformerFrameView {
	BeamformerFrameViewKind kind;
	b32 dirty;
	BeamformerFrame     *frame;
	BeamformerFrameView *prev, *next;

	iv2 texture_dim;
	u32 textures[2];
	i32 texture_mipmaps;

	/* NOTE(rnp): any pointers to variables are added to the menu and will
	 * be put onto the freelist if the view is closed. */

	Variable *kind_cycler;
	Variable *log_scale;
	Variable threshold;
	Variable dynamic_range;
	Variable gamma;

	union {
		/* BeamformerFrameViewKind_Latest/BeamformerFrameViewKind_Indexed */
		struct {
			Variable lateral_scale_bar;
			Variable axial_scale_bar;
			Variable *lateral_scale_bar_active;
			Variable *axial_scale_bar_active;
			/* NOTE(rnp): if kind is Latest  selects which plane to use
			 *            if kind is Indexed selects the index */
			Variable *cycler;
			u32 cycler_state;

			Ruler ruler;

			v3 min_coordinate;
			v3 max_coordinate;
		};

		/* BeamformerFrameViewKind_3DXPlane */
		struct {
			Variable x_plane_shifts[2];
			Variable *demo;
			f32 rotation;
			v3  hit_test_point;
		};
	};
};

typedef struct BeamformerLiveControlsView BeamformerLiveControlsView;
struct BeamformerLiveControlsView {
	Variable transmit_power;
	Variable tgc_control_points[countof(((BeamformerLiveImagingParameters *)0)->tgc_control_points)];
	Variable save_button;
	Variable stop_button;
	Variable save_text;
	UIBlinker save_button_blink;
	u32      hot_field_flag;
	u32      active_field_flag;
};

typedef enum {
	InteractionKind_None,
	InteractionKind_Nop,
	InteractionKind_Auto,
	InteractionKind_Button,
	InteractionKind_Drag,
	InteractionKind_Menu,
	InteractionKind_Ruler,
	InteractionKind_Scroll,
	InteractionKind_Set,
	InteractionKind_Text,
} InteractionKind;

typedef struct {
	InteractionKind kind;
	union {
		void     *generic;
		Variable *var;
	};
	Rect rect;
} Interaction;

#define auto_interaction(r, v) (Interaction){.kind = InteractionKind_Auto, .var = v, .rect = r}

struct BeamformerUI {
	Arena arena;

	Font font;
	Font small_font;

	Variable *regions;
	Variable *variable_freelist;

	Variable floating_widget_sentinal;

	BeamformerFrameView *views;
	BeamformerFrameView *view_freelist;
	BeamformerFrame     *frame_freelist;

	Interaction interaction;
	Interaction hot_interaction;
	Interaction next_interaction;

	InputState  text_input_state;

	/* TODO(rnp): ideally this isn't copied all over the place */
	BeamformerRenderModel unit_cube_model;

	v2_sll *scale_bar_savepoint_freelist;

	BeamformerFrame *latest_plane[BeamformerViewPlaneTag_Count + 1];

	BeamformerUIParameters params;
	b32                    flush_params;
	u32 selected_parameter_block;

	FrameViewRenderContext *frame_view_render_context;

	SharedMemoryRegion  shared_memory;
	BeamformerCtx      *beamformer_context;
};

typedef enum {
	TF_NONE     = 0,
	TF_ROTATED  = 1 << 0,
	TF_LIMITED  = 1 << 1,
	TF_OUTLINED = 1 << 2,
} TextFlags;

typedef enum {
	TextAlignment_Center,
	TextAlignment_Left,
	TextAlignment_Right,
} TextAlignment;

typedef struct {
	Font  *font;
	Rect  limits;
	v4    colour;
	v4    outline_colour;
	f32   outline_thick;
	f32   rotation;
	TextAlignment align;
	TextFlags     flags;
} TextSpec;

typedef enum {
	TRK_CELLS,
	TRK_TABLE,
} TableRowKind;

typedef enum {
	TableCellKind_None,
	TableCellKind_Variable,
	TableCellKind_VariableGroup,
} TableCellKind;

typedef struct {
	s8 text;
	union {
		i64       integer;
		Variable *var;
		void     *generic;
	};
	TableCellKind kind;
	f32 width;
} TableCell;

typedef struct {
	void         *data;
	TableRowKind  kind;
} TableRow;

typedef struct Table {
	TableRow *data;
	iz        count;
	iz        capacity;

	/* NOTE(rnp): counted by columns */
	TextAlignment *alignment;
	f32           *widths;

	v4  border_colour;
	f32 column_border_thick;
	f32 row_border_thick;
	v2  size;
	v2  cell_pad;

	/* NOTE(rnp): row count including nested tables */
	i32 rows;
	i32 columns;

	struct Table *parent;
} Table;

typedef struct {
	Table *table;
	i32    row_index;
} TableStackFrame;

typedef struct {
	TableStackFrame *data;
	iz count;
	iz capacity;
} TableStack;

typedef enum {
	TIK_ROWS,
	TIK_CELLS,
} TableIteratorKind;

typedef struct {
	TableStack      stack;
	TableStackFrame frame;

	TableRow *row;
	i16       column;
	i16       sub_table_depth;

	TableIteratorKind kind;

	f32           start_x;
	TextAlignment alignment;
	Rect          cell_rect;
} TableIterator;

function f32
ui_blinker_update(UIBlinker *b, f32 scale)
{
	b->t += b->scale * dt_for_frame;
	if (b->t >= 1.0f) b->scale = -scale;
	if (b->t <= 0.0f) b->scale =  scale;
	f32 result = b->t;
	return result;
}

function v2
measure_glyph(Font font, u32 glyph)
{
	assert(glyph >= 0x20);
	v2 result = {.y = (f32)font.baseSize};
	/* NOTE: assumes font glyphs are ordered ASCII */
	result.x = (f32)font.glyphs[glyph - 0x20].advanceX;
	if (result.x == 0)
		result.x = (font.recs[glyph - 0x20].width + (f32)font.glyphs[glyph - 0x20].offsetX);
	return result;
}

function v2
measure_text(Font font, s8 text)
{
	v2 result = {.y = (f32)font.baseSize};
	for (iz i = 0; i < text.len; i++)
		result.x += measure_glyph(font, text.data[i]).x;
	return result;
}

function s8
clamp_text_to_width(Font font, s8 text, f32 limit)
{
	s8  result = text;
	f32 width  = 0;
	for (iz i = 0; i < text.len; i++) {
		f32 next = measure_glyph(font, text.data[i]).w;
		if (width + next > limit) {
			result.len = i;
			break;
		}
		width += next;
	}
	return result;
}

function v2
align_text_in_rect(s8 text, Rect r, Font font)
{
	v2 size   = measure_text(font, text);
	v2 pos    = v2_add(r.pos, v2_scale(v2_sub(r.size, size), 0.5));
	v2 result = clamp_v2_rect(pos, r);
	return result;
}

function Texture
make_raylib_texture(BeamformerFrameView *v)
{
	Texture result;
	result.id      = v->textures[0];
	result.width   = v->texture_dim.w;
	result.height  = v->texture_dim.h;
	result.mipmaps = v->texture_mipmaps;
	result.format  = PIXELFORMAT_UNCOMPRESSED_R8G8B8A8;
	return result;
}

function void
stream_append_variable(Stream *s, Variable *var)
{
	switch (var->type) {
	case VT_UI_BUTTON:
	case VT_GROUP:{ stream_append_s8(s, var->name); }break;
	case VT_F32:{   stream_append_f64(s, var->real32, 100); }break;
	case VT_B32:{   stream_append_s8(s, var->bool32 ? s8("True") : s8("False")); }break;
	case VT_SCALED_F32:{ stream_append_f64(s, var->scaled_real32.val, 100); }break;
	case VT_BEAMFORMER_VARIABLE:{
		BeamformerVariable *bv = &var->beamformer_variable;
		stream_append_f64(s, *bv->store * bv->display_scale, 100);
	}break;
	case VT_CYCLER:{
		u32 index = *var->cycler.state;
		if (var->cycler.labels) stream_append_s8(s, var->cycler.labels[index]);
		else                    stream_append_u64(s, index);
	}break;
	case VT_LIVE_CONTROLS_STRING:{
		BeamformerLiveImagingParameters *lip = var->generic;
		stream_append_s8(s, (s8){.data = (u8 *)lip->save_name_tag, .len = lip->save_name_tag_length});
		if (lip->save_name_tag_length <= 0) stream_append_s8(s, s8("Tag..."));
	}break;
	InvalidDefaultCase;
	}
}

function void
stream_append_variable_group(Stream *s, Variable *var)
{
	switch (var->type) {
	case VT_GROUP:{
		switch (var->group.kind) {
		case VariableGroupKind_Vector:{
			Variable *v = var->group.first;
			stream_append_s8(s, s8("{"));
			while (v) {
				stream_append_variable(s, v);
				v = v->next;
				if (v) stream_append_s8(s, s8(", "));
			}
			stream_append_s8(s, s8("}"));
		}break;
		InvalidDefaultCase;
		}
	}break;
	InvalidDefaultCase;
	}
}

function s8
push_das_shader_kind(Stream *s, DASShaderKind shader, u32 transmit_count)
{
	#define X(__1, __2, pretty, ...) s8_comp(pretty),
	read_only local_persist s8 pretty_names[DASShaderKind_Count + 1] = {
		DAS_SHADER_KIND_LIST
		s8_comp("Invalid")
	};
	#undef X
	#define X(__1, __2, __3, fixed_tx) fixed_tx,
	read_only local_persist u8 fixed_transmits[DASShaderKind_Count + 1] = {DAS_SHADER_KIND_LIST};
	#undef X

	stream_append_s8(s, pretty_names[MIN(shader, DASShaderKind_Count)]);
	if (!fixed_transmits[MIN(shader, DASShaderKind_Count)]) {
		stream_append_byte(s, '-');
		stream_append_u64(s, transmit_count);
	}

	return stream_to_s8(s);
}

function s8
push_custom_view_title(Stream *s, Variable *var)
{
	switch (var->type) {
	case VT_COMPUTE_STATS_VIEW:{
		stream_append_s8(s, s8("Compute Stats: "));
		stream_append_variable(s, var->compute_stats_view.cycler);
	}break;
	case VT_COMPUTE_PROGRESS_BAR:{
		stream_append_s8(s, s8("Compute Progress: "));
		stream_append_f64(s, 100 * *var->compute_progress_bar.progress, 100);
		stream_append_byte(s, '%');
	} break;
	case VT_BEAMFORMER_FRAME_VIEW:{
		BeamformerFrameView *bv = var->generic;
		stream_append_s8(s, s8("Frame View"));
		switch (bv->kind) {
		case BeamformerFrameViewKind_Copy:{ stream_append_s8(s, s8(": Copy [")); }break;
		case BeamformerFrameViewKind_Latest:{
			#define X(plane, id, pretty) s8_comp(": " pretty " ["),
			read_only local_persist s8 labels[BeamformerViewPlaneTag_Count + 1] = {
				BEAMFORMER_VIEW_PLANE_TAG_LIST
				s8_comp(": Live [")
			};
			#undef X
			stream_append_s8(s, labels[*bv->cycler->cycler.state % (BeamformerViewPlaneTag_Count + 1)]);
		}break;
		case BeamformerFrameViewKind_Indexed:{
			stream_append_s8(s, s8(": Index {"));
			stream_append_u64(s, *bv->cycler->cycler.state % BeamformerMaxSavedFrames);
			stream_append_s8(s, s8("} ["));
		}break;
		case BeamformerFrameViewKind_3DXPlane:{ stream_append_s8(s, s8(": 3D X-Plane")); }break;
		InvalidDefaultCase;
		}
		if (bv->kind != BeamformerFrameViewKind_3DXPlane) {
			stream_append_hex_u64(s, bv->frame? bv->frame->id : 0);
			stream_append_byte(s, ']');
		}
	}break;
	InvalidDefaultCase;
	}
	return stream_to_s8(s);
}

#define table_new(a, init, ...) table_new_(a, init, arg_list(TextAlignment, ##__VA_ARGS__))
function Table *
table_new_(Arena *a, i32 initial_capacity, TextAlignment *alignment, i32 columns)
{
	Table *result = push_struct(a, Table);
	da_reserve(a, result, initial_capacity);
	result->columns   = columns;
	result->alignment = push_array(a, TextAlignment, columns);
	result->widths    = push_array(a, f32, columns);
	result->cell_pad  = (v2){{TABLE_CELL_PAD_WIDTH, TABLE_CELL_PAD_HEIGHT}};
	mem_copy(result->alignment, alignment, sizeof(*alignment) * (u32)columns);
	return result;
}

function i32
table_skip_rows(Table *t, f32 draw_height, f32 text_height)
{
	i32 max_rows = (i32)(draw_height / (text_height + t->cell_pad.h));
	i32 result   = t->rows - MIN(t->rows, max_rows);
	return result;
}

function TableIterator *
table_iterator_new(Table *table, TableIteratorKind kind, Arena *a, i32 starting_row, v2 at, Font *font)
{
	TableIterator *result    = push_struct(a, TableIterator);
	result->kind             = kind;
	result->frame.table      = table;
	result->frame.row_index  = starting_row;
	result->start_x          = at.x;
	result->cell_rect.size.h = (f32)font->baseSize;
	result->cell_rect.pos    = v2_add(at, v2_scale(table->cell_pad, 0.5f));
	result->cell_rect.pos.y += (f32)(starting_row - 1) * (result->cell_rect.size.h + table->cell_pad.h + table->row_border_thick);
	da_reserve(a, &result->stack, 4);
	return result;
}

function void *
table_iterator_next(TableIterator *it, Arena *a)
{
	void *result = 0;

	if (!it->row || it->kind == TIK_ROWS) {
		for (;;) {
			TableRow *row = it->frame.table->data + it->frame.row_index++;
			if (it->frame.row_index <= it->frame.table->count) {
				if (row->kind == TRK_TABLE) {
					*da_push(a, &it->stack) = it->frame;
					it->frame = (TableStackFrame){.table = row->data};
					it->sub_table_depth++;
				} else {
					result = row;
					break;
				}
			} else if (it->stack.count) {
				it->frame = it->stack.data[--it->stack.count];
				it->sub_table_depth--;
			} else {
				break;
			}
		}
		Table *t   = it->frame.table;
		it->row    = result;
		it->column = 0;
		it->cell_rect.pos.x  = it->start_x + t->cell_pad.w / 2 +
		                       it->cell_rect.size.h * it->sub_table_depth;
		it->cell_rect.pos.y += it->cell_rect.size.h + t->row_border_thick + t->cell_pad.h;
	}

	if (it->row && it->kind == TIK_CELLS) {
		Table *t   = it->frame.table;
		i32 column = it->column++;
		it->cell_rect.pos.x  += column > 0 ? it->cell_rect.size.w + t->cell_pad.w : 0;
		it->cell_rect.size.w  = t->widths[column];
		it->alignment         = t->alignment[column];
		result                = (TableCell *)it->row->data + column;

		if (it->column == t->columns)
			it->row = 0;
	}

	return result;
}

function f32
table_width(Table *t)
{
	f32 result = 0;
	i32 valid  = 0;
	for (i32 i = 0; i < t->columns; i++) {
		result += t->widths[i];
		if (t->widths[i] > 0) valid++;
	}
	result += t->cell_pad.w * (f32)valid;
	result += MAX(0, ((f32)valid - 1)) * t->column_border_thick;
	return result;
}

function v2
table_extent(Table *t, Arena arena, Font *font)
{
	TableIterator *it = table_iterator_new(t, TIK_ROWS, &arena, 0, (v2){0}, font);
	for (TableRow *row = table_iterator_next(it, &arena);
	     row;
	     row = table_iterator_next(it, &arena))
	{
		for (i32 i = 0; i < it->frame.table->columns; i++) {
			TableCell *cell = (TableCell *)row->data + i;
			if (!cell->text.len && cell->var && cell->var->flags & V_RADIO_BUTTON) {
				cell->width = (f32)font->baseSize;
			} else {
				cell->width = measure_text(*font, cell->text).w;
			}
			it->frame.table->widths[i] = MAX(cell->width, it->frame.table->widths[i]);
		}
	}

	t->size = (v2){.x = table_width(t), .y = it->cell_rect.pos.y - t->cell_pad.h / 2};
	v2 result = t->size;
	return result;
}

function v2
table_cell_align(TableCell *cell, TextAlignment align, Rect r)
{
	v2 result = r.pos;
	if (r.size.w >= cell->width) {
		switch (align) {
		case TextAlignment_Left:{}break;
		case TextAlignment_Right:{  result.x += (r.size.w - cell->width);     }break;
		case TextAlignment_Center:{ result.x += (r.size.w - cell->width) / 2; }break;
		}
	}
	return result;
}

function TableCell
table_variable_cell(Arena *a, Variable *var)
{
	TableCell result = {.var = var, .kind = TableCellKind_Variable};
	if ((var->flags & V_RADIO_BUTTON) == 0) {
		Stream text = arena_stream(*a);
		stream_append_variable(&text, var);
		result.text = arena_stream_commit(a, &text);
	}
	return result;
}

function TableRow *
table_push_row(Table *t, Arena *a, TableRowKind kind)
{
	TableRow *result = da_push(a, t);
	if (kind == TRK_CELLS) {
		result->data = push_array(a, TableCell, t->columns);
		/* NOTE(rnp): do not increase rows for an empty subtable */
		t->rows++;
	}
	result->kind = kind;
	return result;
}

function TableRow *
table_push_parameter_row(Table *t, Arena *a, s8 label, Variable *var, s8 suffix)
{
	ASSERT(t->columns >= 3);
	TableRow *result = table_push_row(t, a, TRK_CELLS);
	TableCell *cells = result->data;

	cells[0].text  = label;
	cells[1]       = table_variable_cell(a, var);
	cells[2].text  = suffix;

	return result;
}

#define table_begin_subtable(t, a, ...) table_begin_subtable_(t, a, arg_list(TextAlignment, ##__VA_ARGS__))
function Table *
table_begin_subtable_(Table *table, Arena *a, TextAlignment *alignment, i32 columns)
{
	TableRow *row = table_push_row(table, a, TRK_TABLE);
	Table *result = row->data = table_new_(a, 0, alignment, columns);
	result->parent = table;
	return result;
}

function Table *
table_end_subtable(Table *table)
{
	Table *result = table->parent ? table->parent : table;
	return result;
}

function void
resize_frame_view(BeamformerFrameView *view, iv2 dim, b32 depth)
{
	glDeleteTextures(countof(view->textures), view->textures);
	glCreateTextures(GL_TEXTURE_2D, depth ? countof(view->textures) : countof(view->textures) - 1, view->textures);

	view->texture_dim     = dim;
	view->texture_mipmaps = (i32)ctz_u32((u32)MAX(dim.x, dim.y)) + 1;
	glTextureStorage2D(view->textures[0], view->texture_mipmaps, GL_RGBA8, dim.x, dim.y);
	if (depth) glTextureStorage2D(view->textures[1], 1, GL_DEPTH_COMPONENT24, dim.x, dim.y);

	glGenerateTextureMipmap(view->textures[0]);

	/* NOTE(rnp): work around raylib's janky texture sampling */
	v4 border_colour = {0};
	if (view->kind == BeamformerFrameViewKind_Copy) border_colour = (v4){{0, 0, 0, 1}};
	glTextureParameteri(view->textures[0], GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
	glTextureParameteri(view->textures[0], GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
	glTextureParameterfv(view->textures[0], GL_TEXTURE_BORDER_COLOR, border_colour.E);
	/* TODO(rnp): better choice when depth component is included */
	glTextureParameteri(view->textures[0], GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTextureParameteri(view->textures[0], GL_TEXTURE_MIN_FILTER, GL_NEAREST);

	/* TODO(rnp): add some ID for the specific view here */
	LABEL_GL_OBJECT(GL_TEXTURE, view->textures[0], s8("Frame View Texture"));
}

function void
ui_beamformer_frame_view_release_subresources(BeamformerUI *ui, BeamformerFrameView *bv, BeamformerFrameViewKind kind)
{
	if (kind == BeamformerFrameViewKind_Copy && bv->frame) {
		glDeleteTextures(1, &bv->frame->texture);
		bv->frame->texture = 0;
		SLLPushFreelist(bv->frame, ui->frame_freelist);
	}

	if (kind != BeamformerFrameViewKind_3DXPlane) {
		if (bv->axial_scale_bar.scale_bar.savepoint_stack)
			SLLPushFreelist(bv->axial_scale_bar.scale_bar.savepoint_stack, ui->scale_bar_savepoint_freelist);
		if (bv->lateral_scale_bar.scale_bar.savepoint_stack)
			SLLPushFreelist(bv->lateral_scale_bar.scale_bar.savepoint_stack, ui->scale_bar_savepoint_freelist);
	}
}

function void
ui_variable_free(BeamformerUI *ui, Variable *var)
{
	if (var) {
		var->parent = 0;
		while (var) {
			if (var->type == VT_GROUP) {
				var = var->group.first;
			} else {
				if (var->type == VT_BEAMFORMER_FRAME_VIEW) {
					/* TODO(rnp): instead there should be a way of linking these up */
					BeamformerFrameView *bv = var->generic;
					ui_beamformer_frame_view_release_subresources(ui, bv, bv->kind);
					DLLRemove(bv);
					/* TODO(rnp): hack; use a sentinal */
					if (bv == ui->views)
						ui->views = bv->next;
					SLLPushFreelist(bv, ui->view_freelist);
				}

				Variable *dead = var;
				if (var->next) {
					var = var->next;
				} else {
					var = var->parent;
					/* NOTE(rnp): when we assign parent here we have already
					 * released the children. Assign type so we don't loop */
					if (var) var->type = VT_NULL;
				}
				SLLPushFreelist(dead, ui->variable_freelist);
			}
		}
	}
}

function void
ui_variable_free_group_items(BeamformerUI *ui, Variable *group)
{
	assert(group->type == VT_GROUP);
	/* NOTE(rnp): prevent traversal back to us */
	group->group.last->parent = 0;
	ui_variable_free(ui, group->group.first);
	group->group.first = group->group.last = 0;
}

function void
ui_view_free(BeamformerUI *ui, Variable *view)
{
	assert(view->type == VT_UI_VIEW);
	ui_variable_free(ui, view->view.child);
	ui_variable_free(ui, view->view.close);
	ui_variable_free(ui, view->view.menu);
	ui_variable_free(ui, view);
}

function Variable *
fill_variable(Variable *var, Variable *group, s8 name, u32 flags, VariableType type, Font font)
{
	var->flags      = flags;
	var->type       = type;
	var->name       = name;
	var->parent     = group;
	var->name_width = measure_text(font, name).x;

	if (group && group->type == VT_GROUP) {
		if (group->group.last) group->group.last = group->group.last->next = var;
		else                   group->group.last = group->group.first      = var;
	}

	return var;
}

function Variable *
add_variable(BeamformerUI *ui, Variable *group, Arena *arena, s8 name, u32 flags,
             VariableType type, Font font)
{
	Variable *result = SLLPopFreelist(ui->variable_freelist);
	if (result) zero_struct(result);
	else        result = push_struct(arena, Variable);
	return fill_variable(result, group, name, flags, type, font);
}

function Variable *
add_variable_group(BeamformerUI *ui, Variable *group, Arena *arena, s8 name, VariableGroupKind kind, Font font)
{
	Variable *result   = add_variable(ui, group, arena, name, V_INPUT, VT_GROUP, font);
	result->group.kind = kind;
	return result;
}

function Variable *
end_variable_group(Variable *group)
{
	ASSERT(group->type == VT_GROUP);
	return group->parent;
}

function void
fill_variable_cycler(Variable *cycler, u32 *store, s8 *labels, u32 cycle_count)
{
	cycler->cycler.cycle_length = cycle_count;
	cycler->cycler.state        = store;
	cycler->cycler.labels       = labels;
}

function Variable *
add_variable_cycler(BeamformerUI *ui, Variable *group, Arena *arena, u32 flags, Font font, s8 name,
                    u32 *store, s8 *labels, u32 cycle_count)
{
	Variable *result = add_variable(ui, group, arena, name, V_INPUT|flags, VT_CYCLER, font);
	fill_variable_cycler(result, store, labels, cycle_count);
	return result;
}

function Variable *
add_button(BeamformerUI *ui, Variable *group, Arena *arena, s8 name, UIButtonID id,
           u32 flags, Font font)
{
	Variable *result = add_variable(ui, group, arena, name, V_INPUT|flags, VT_UI_BUTTON, font);
	result->button   = id;
	return result;
}

function Variable *
add_ui_split(BeamformerUI *ui, Variable *parent, Arena *arena, s8 name, f32 fraction,
             RegionSplitDirection direction, Font font)
{
	Variable *result = add_variable(ui, parent, arena, name, V_HIDES_CURSOR, VT_UI_REGION_SPLIT, font);
	result->region_split.direction = direction;
	result->region_split.fraction  = fraction;
	return result;
}

function Variable *
add_global_menu_to_group(BeamformerUI *ui, Arena *arena, Variable *group)
{
	#define X(id, text) add_button(ui, group, arena, s8(text), UI_BID_ ##id, 0, ui->small_font);
	GLOBAL_MENU_BUTTONS
	#undef X
	return group;
}

function Variable *
add_global_menu(BeamformerUI *ui, Arena *arena, Variable *parent)
{
	Variable *result = add_variable_group(ui, 0, arena, s8(""), VariableGroupKind_List, ui->small_font);
	result->parent = parent;
	return add_global_menu_to_group(ui, arena, result);
}

function Variable *
add_ui_view(BeamformerUI *ui, Variable *parent, Arena *arena, s8 name, u32 view_flags, b32 menu, b32 closable)
{
	Variable *result = add_variable(ui, parent, arena, name, 0, VT_UI_VIEW, ui->small_font);
	UIView   *view   = &result->view;
	view->flags      = view_flags;
	if (menu) view->menu = add_global_menu(ui, arena, result);
	if (closable) {
		view->close = add_button(ui, 0, arena, s8(""), UI_BID_VIEW_CLOSE, 0, ui->small_font);
		/* NOTE(rnp): we do this explicitly so that close doesn't end up in the view group */
		view->close->parent = result;
	}
	return result;
}

function Variable *
add_floating_view(BeamformerUI *ui, Arena *arena, VariableType type, v2 at, Variable *child, b32 closable)
{
	Variable *result = add_ui_view(ui, 0, arena, s8(""), UIViewFlag_Floating, 0, closable);
	result->type          = type;
	result->view.rect.pos = at;
	result->view.child    = child;

	result->parent = &ui->floating_widget_sentinal;
	result->next   = ui->floating_widget_sentinal.next;
	result->next->parent = result;
	ui->floating_widget_sentinal.next = result;
	return result;
}

function void
fill_beamformer_variable(Variable *var, s8 suffix, f32 *store, v2 limits, f32 display_scale, f32 scroll_scale)
{
	BeamformerVariable *bv = &var->beamformer_variable;
	bv->suffix        = suffix;
	bv->store         = store;
	bv->display_scale = display_scale;
	bv->scroll_scale  = scroll_scale;
	bv->limits        = limits;
}

function void
add_beamformer_variable(BeamformerUI *ui, Variable *group, Arena *arena, s8 name, s8 suffix, f32 *store,
                        v2 limits, f32 display_scale, f32 scroll_scale, u32 flags, Font font)
{
	Variable *var = add_variable(ui, group, arena, name, flags, VT_BEAMFORMER_VARIABLE, font);
	fill_beamformer_variable(var, suffix, store, limits, display_scale, scroll_scale);
}

function Variable *
add_beamformer_parameters_view(Variable *parent, BeamformerCtx *ctx)
{
	BeamformerUI *ui           = ctx->ui;
	BeamformerUIParameters *bp = &ui->params;

	v2 v2_inf = {.x = -F32_INFINITY, .y = F32_INFINITY};

	/* TODO(rnp): this can be closable once we have a way of opening new views */
	Variable *result = add_ui_view(ui, parent, &ui->arena, s8("Parameters"), 0, 1, 0);
	Variable *group  = result->view.child = add_variable(ui, result, &ui->arena, s8(""), 0,
	                                                     VT_GROUP, ui->font);

	add_beamformer_variable(ui, group, &ui->arena, s8("Sampling Frequency:"), s8("[MHz]"),
	                        &bp->sampling_frequency, (v2){0}, 1e-6f, 0, 0, ui->font);

	add_beamformer_variable(ui, group, &ui->arena, s8("Demodulation Frequency:"), s8("[MHz]"),
	                        &bp->demodulation_frequency, (v2){.y = 100e6f}, 1e-6f, 1e5f,
	                        V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

	add_beamformer_variable(ui, group, &ui->arena, s8("Speed of Sound:"), s8("[m/s]"),
	                        &bp->speed_of_sound, (v2){.y = 1e6f}, 1.0f, 10.0f,
	                        V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

	group = add_variable_group(ui, group, &ui->arena, s8("Lateral Extent:"),
	                           VariableGroupKind_Vector, ui->font);
	{
		add_beamformer_variable(ui, group, &ui->arena, s8("Min:"), s8("[mm]"),
		                       bp->output_min_coordinate + 0, v2_inf, 1e3f, 0.5e-3f,
		                       V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

		add_beamformer_variable(ui, group, &ui->arena, s8("Max:"), s8("[mm]"),
		                        bp->output_max_coordinate + 0, v2_inf, 1e3f, 0.5e-3f,
		                        V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);
	}
	group = end_variable_group(group);

	group = add_variable_group(ui, group, &ui->arena, s8("Axial Extent:"),
	                           VariableGroupKind_Vector, ui->font);
	{
		add_beamformer_variable(ui, group, &ui->arena, s8("Min:"), s8("[mm]"),
		                        bp->output_min_coordinate + 2, v2_inf, 1e3f, 0.5e-3f,
		                        V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

		add_beamformer_variable(ui, group, &ui->arena, s8("Max:"), s8("[mm]"),
		                        bp->output_max_coordinate + 2, v2_inf, 1e3f, 0.5e-3f,
		                        V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);
	}
	group = end_variable_group(group);

	add_beamformer_variable(ui, group, &ui->arena, s8("Off Axis Position:"), s8("[mm]"),
	                        &bp->off_axis_pos, (v2){{-1e3f, 1e3f}}, 0.25e3f, 0.5e-3f,
	                        V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

	read_only local_persist s8 beamform_plane_labels[] = {s8_comp("XZ"), s8_comp("YZ")};
	add_variable_cycler(ui, group, &ui->arena, V_CAUSES_COMPUTE, ui->font, s8("Beamform Plane:"),
	                    (u32 *)&bp->beamform_plane, beamform_plane_labels, countof(beamform_plane_labels));

	add_beamformer_variable(ui, group, &ui->arena, s8("F#:"), s8(""), &bp->f_number, (v2){.y = 1e3f},
	                        1, 0.1f, V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

	read_only local_persist s8 true_false_labels[] = {s8_comp("False"), s8_comp("True")};
	add_variable_cycler(ui, group, &ui->arena, V_CAUSES_COMPUTE, ui->font, s8("Interpolate:"),
	                    &bp->interpolate, true_false_labels, countof(true_false_labels));

	add_variable_cycler(ui, group, &ui->arena, V_CAUSES_COMPUTE, ui->font, s8("Coherency Weighting:"),
	                    &bp->coherency_weighting, true_false_labels, countof(true_false_labels));

	return result;
}

function void
ui_beamformer_frame_view_convert(BeamformerUI *ui, Arena *arena, Variable *view, Variable *menu,
                                 BeamformerFrameViewKind kind, BeamformerFrameView *old, b32 log_scale)
{
	assert(menu->group.first == menu->group.last && menu->group.first == 0);
	assert(view->type == VT_BEAMFORMER_FRAME_VIEW);

	BeamformerFrameView *bv = view->generic;
	bv->kind  = kind;
	bv->dirty = 1;

	fill_variable(&bv->dynamic_range, view, s8("Dynamic Range:"), V_INPUT|V_TEXT|V_UPDATE_VIEW,
	              VT_F32, ui->small_font);
	fill_variable(&bv->threshold, view, s8("Threshold:"), V_INPUT|V_TEXT|V_UPDATE_VIEW,
	              VT_F32, ui->small_font);
	fill_variable(&bv->gamma, view, s8("Gamma:"), V_INPUT|V_TEXT|V_UPDATE_VIEW,
	              VT_SCALED_F32, ui->small_font);

	bv->dynamic_range.real32      = old? old->dynamic_range.real32      : 50.0f;
	bv->threshold.real32          = old? old->threshold.real32          : 55.0f;
	bv->gamma.scaled_real32.val   = old? old->gamma.scaled_real32.val   : 1.0f;
	bv->gamma.scaled_real32.scale = old? old->gamma.scaled_real32.scale : 0.05f;
	bv->min_coordinate = (old && old->frame)? old->frame->min_coordinate : (v3){0};
	bv->max_coordinate = (old && old->frame)? old->frame->max_coordinate : (v3){0};

	#define X(_t, pretty) s8_comp(pretty),
	read_only local_persist s8 kind_labels[] = {BEAMFORMER_FRAME_VIEW_KIND_LIST};
	#undef X
	bv->kind_cycler = add_variable_cycler(ui, menu, arena, V_EXTRA_ACTION, ui->small_font,
	                                      s8("Kind:"), (u32 *)&bv->kind, kind_labels, countof(kind_labels));

	switch (kind) {
	case BeamformerFrameViewKind_3DXPlane:{
		view->flags |= V_HIDES_CURSOR;
		resize_frame_view(bv, (iv2){{FRAME_VIEW_RENDER_TARGET_SIZE}}, 0);
		glTextureParameteri(bv->textures[0], GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTextureParameteri(bv->textures[0], GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
		fill_variable(bv->x_plane_shifts + 0, view, s8("XZ Shift"), V_INPUT|V_HIDES_CURSOR,
		              VT_X_PLANE_SHIFT, ui->small_font);
		fill_variable(bv->x_plane_shifts + 1, view, s8("YZ Shift"), V_INPUT|V_HIDES_CURSOR,
		              VT_X_PLANE_SHIFT, ui->small_font);
		bv->demo = add_variable(ui, menu, arena, s8("Demo Mode"), V_INPUT|V_RADIO_BUTTON, VT_B32, ui->small_font);
	}break;
	default:{
		view->flags &= ~(u32)V_HIDES_CURSOR;
		fill_variable(&bv->lateral_scale_bar, view, s8(""), V_INPUT, VT_SCALE_BAR, ui->small_font);
		fill_variable(&bv->axial_scale_bar,   view, s8(""), V_INPUT, VT_SCALE_BAR, ui->small_font);
		ScaleBar *lateral            = &bv->lateral_scale_bar.scale_bar;
		ScaleBar *axial              = &bv->axial_scale_bar.scale_bar;
		lateral->direction           = SB_LATERAL;
		axial->direction             = SB_AXIAL;
		lateral->scroll_scale        = (v2){{-0.5e-3f, 0.5e-3f}};
		axial->scroll_scale          = (v2){{ 0,       1.0e-3f}};
		lateral->zoom_starting_coord = F32_INFINITY;
		axial->zoom_starting_coord   = F32_INFINITY;

		b32 copy = kind == BeamformerFrameViewKind_Copy;
		lateral->min_value = copy ? &bv->min_coordinate.x : ui->params.output_min_coordinate + 0;
		lateral->max_value = copy ? &bv->max_coordinate.x : ui->params.output_max_coordinate + 0;
		axial->min_value   = copy ? &bv->min_coordinate.z : ui->params.output_min_coordinate + 2;
		axial->max_value   = copy ? &bv->max_coordinate.z : ui->params.output_max_coordinate + 2;

		#define X(id, text) add_button(ui, menu, arena, s8(text), UI_BID_ ##id, 0, ui->small_font);
		FRAME_VIEW_BUTTONS
		#undef X

		bv->axial_scale_bar_active   = add_variable(ui, menu, arena, s8("Axial Scale Bar"),
		                                            V_INPUT|V_RADIO_BUTTON, VT_B32, ui->small_font);
		bv->lateral_scale_bar_active = add_variable(ui, menu, arena, s8("Lateral Scale Bar"),
		                                            V_INPUT|V_RADIO_BUTTON, VT_B32, ui->small_font);

		if (kind == BeamformerFrameViewKind_Latest) {
			bv->axial_scale_bar_active->bool32   = 1;
			bv->lateral_scale_bar_active->bool32 = 1;
			bv->axial_scale_bar.flags   |= V_CAUSES_COMPUTE;
			bv->lateral_scale_bar.flags |= V_CAUSES_COMPUTE;
		}
	}break;
	}

	bv->log_scale = add_variable(ui, menu, arena, s8("Log Scale"),
	                             V_INPUT|V_UPDATE_VIEW|V_RADIO_BUTTON, VT_B32, ui->small_font);
	bv->log_scale->bool32 = log_scale;

	switch (kind) {
	case BeamformerFrameViewKind_Latest:{
		#define X(_type, _id, pretty) s8_comp(pretty),
		read_only local_persist s8 labels[] = {BEAMFORMER_VIEW_PLANE_TAG_LIST s8_comp("Any")};
		#undef X
		bv->cycler = add_variable_cycler(ui, menu, arena, 0, ui->small_font, s8("Live:"),
		                                 &bv->cycler_state, labels, countof(labels));
		bv->cycler_state = BeamformerViewPlaneTag_Count;
	}break;
	case BeamformerFrameViewKind_Indexed:{
		bv->cycler = add_variable_cycler(ui, menu, arena, 0, ui->small_font, s8("Index:"),
		                                 &bv->cycler_state, 0, BeamformerMaxSavedFrames);
	}break;
	default:{}break;
	}

	add_global_menu_to_group(ui, arena, menu);
}

function BeamformerFrameView *
ui_beamformer_frame_view_new(BeamformerUI *ui, Arena *arena)
{
	BeamformerFrameView *result = SLLPopFreelist(ui->view_freelist);
	if (result) zero_struct(result);
	else        result = push_struct(arena, typeof(*result));
	DLLPushDown(result, ui->views);
	return result;
}

function Variable *
add_beamformer_frame_view(BeamformerUI *ui, Variable *parent, Arena *arena,
                          BeamformerFrameViewKind kind, b32 closable, BeamformerFrameView *old)
{
	/* TODO(rnp): this can be always closable once we have a way of opening new views */
	Variable *result = add_ui_view(ui, parent, arena, s8(""), UIViewFlag_CustomText, 1, closable);
	Variable *var = result->view.child = add_variable(ui, result, arena, s8(""), 0,
	                                                  VT_BEAMFORMER_FRAME_VIEW, ui->small_font);
	Variable *menu = result->view.menu = add_variable_group(ui, 0, arena, s8(""),
	                                                        VariableGroupKind_List, ui->small_font);
	menu->parent = result;
	var->generic = ui_beamformer_frame_view_new(ui, arena);
	ui_beamformer_frame_view_convert(ui, arena, var, menu, kind, old, old? old->log_scale->bool32 : 0);
	return result;
}

function Variable *
add_compute_progress_bar(Variable *parent, BeamformerCtx *ctx)
{
	BeamformerUI *ui = ctx->ui;
	/* TODO(rnp): this can be closable once we have a way of opening new views */
	Variable *result = add_ui_view(ui, parent, &ui->arena, s8(""), UIViewFlag_CustomText, 1, 0);
	result->view.child = add_variable(ui, result, &ui->arena, s8(""), 0,
	                                  VT_COMPUTE_PROGRESS_BAR, ui->small_font);
	ComputeProgressBar *bar = &result->view.child->compute_progress_bar;
	bar->progress   = &ctx->compute_context.processing_progress;
	bar->processing = &ctx->compute_context.processing_compute;

	return result;
}

function Variable *
add_compute_stats_view(BeamformerUI *ui, Variable *parent, Arena *arena, BeamformerCtx *ctx)
{
	/* TODO(rnp): this can be closable once we have a way of opening new views */
	Variable *result   = add_ui_view(ui, parent, arena, s8(""), UIViewFlag_CustomText, 0, 0);
	result->view.child = add_variable(ui, result, &ui->arena, s8(""), 0,
	                                  VT_COMPUTE_STATS_VIEW, ui->small_font);

	Variable *menu = result->view.menu = add_variable_group(ui, 0, arena, s8(""),
	                                                        VariableGroupKind_List, ui->small_font);
	menu->parent = result;

	#define X(_k, label) s8_comp(label),
	read_only local_persist s8 labels[] = {COMPUTE_STATS_VIEW_LIST};
	#undef X

	ComputeStatsView *csv = &result->view.child->compute_stats_view;
	csv->compute_shader_stats = ctx->compute_shader_stats;
	csv->cycler = add_variable_cycler(ui, menu, arena, 0, ui->small_font, s8("Stats View:"),
	                                  (u32 *)&csv->kind, labels, countof(labels));
	add_global_menu_to_group(ui, arena, menu);
	return result;
}

function Variable *
add_live_controls_view(BeamformerUI *ui, Variable *parent, Arena *arena)
{
	BeamformerSharedMemory *sm = ui->shared_memory.region;
	BeamformerLiveImagingParameters *lip = &sm->live_imaging_parameters;
	/* TODO(rnp): this can be closable once we have a way of opening new views */
	Variable *result = add_ui_view(ui, parent, &ui->arena, s8("Live Controls"), 0, 1, 0);
	result->view.child = add_variable(ui, result, &ui->arena, s8(""), 0,
	                                  VT_LIVE_CONTROLS_VIEW, ui->small_font);
	Variable *view = result->view.child;
	BeamformerLiveControlsView *lv = view->generic = push_struct(arena, typeof(*lv));

	fill_variable(&lv->transmit_power, view, s8(""), V_INPUT|V_LIVE_CONTROL,
	              VT_BEAMFORMER_VARIABLE, ui->small_font);
	fill_beamformer_variable(&lv->transmit_power, s8(""), &lip->transmit_power, (v2){{0, 1.0f}}, 100.0f, 0.05f);

	for (u32 i = 0; i < countof(lv->tgc_control_points); i++) {
		Variable *v = lv->tgc_control_points + i;
		fill_variable(v, view, s8(""), V_INPUT|V_LIVE_CONTROL, VT_BEAMFORMER_VARIABLE, ui->small_font);
		fill_beamformer_variable(v, s8(""), lip->tgc_control_points + i, (v2){{0, 1.0f}}, 0, 0.05f);
	}

	fill_variable(&lv->stop_button, view, s8("Stop Imaging"), V_INPUT|V_LIVE_CONTROL,
	              VT_B32, ui->small_font);

	read_only local_persist s8 save_labels[] = {s8_comp("Save Data"), s8_comp("Saving...")};
	fill_variable(&lv->save_button, view, s8("Save Data"), V_INPUT|V_LIVE_CONTROL,
	              VT_CYCLER, ui->small_font);
	fill_variable_cycler(&lv->save_button, &lip->save_active, save_labels, countof(save_labels));

	fill_variable(&lv->save_text, view, s8(""), V_INPUT|V_TEXT|V_LIVE_CONTROL,
	              VT_LIVE_CONTROLS_STRING, ui->small_font);
	lv->save_text.generic = lip;

	return result;
}

function Variable *
ui_split_region(BeamformerUI *ui, Variable *region, Variable *split_side, RegionSplitDirection direction)
{
	Variable *result = add_ui_split(ui, region, &ui->arena, s8(""), 0.5, direction, ui->small_font);
	if (split_side == region->region_split.left) {
		region->region_split.left  = result;
	} else {
		region->region_split.right = result;
	}
	split_side->parent = result;
	result->region_split.left = split_side;
	return result;
}

function void
ui_add_live_frame_view(BeamformerUI *ui, Variable *view, RegionSplitDirection direction,
                       BeamformerFrameViewKind kind)
{
	Variable *region = view->parent;
	assert(region->type == VT_UI_REGION_SPLIT);
	assert(view->type   == VT_UI_VIEW);
	Variable *new_region = ui_split_region(ui, region, view, direction);
	new_region->region_split.right = add_beamformer_frame_view(ui, new_region, &ui->arena, kind, 1, 0);
}

function void
ui_beamformer_frame_view_copy_frame(BeamformerUI *ui, BeamformerFrameView *new, BeamformerFrameView *old)
{
	assert(old->frame);
	new->frame = SLLPopFreelist(ui->frame_freelist);
	if (!new->frame) new->frame = push_struct(&ui->arena, typeof(*new->frame));

	mem_copy(new->frame, old->frame, sizeof(*new->frame));
	new->frame->texture = 0;
	new->frame->next    = 0;
	alloc_beamform_frame(0, new->frame, old->frame->dim, old->frame->gl_kind, s8("Frame Copy: "), ui->arena);

	glCopyImageSubData(old->frame->texture, GL_TEXTURE_3D, 0, 0, 0, 0,
	                   new->frame->texture, GL_TEXTURE_3D, 0, 0, 0, 0,
	                   new->frame->dim.x, new->frame->dim.y, new->frame->dim.z);
	glMemoryBarrier(GL_TEXTURE_UPDATE_BARRIER_BIT);
	/* TODO(rnp): x vs y here */
	resize_frame_view(new, (iv2){{new->frame->dim.x, new->frame->dim.z}}, 1);
}

function void
ui_copy_frame(BeamformerUI *ui, Variable *view, RegionSplitDirection direction)
{
	Variable *region = view->parent;
	assert(region->type == VT_UI_REGION_SPLIT);
	assert(view->type   == VT_UI_VIEW);

	BeamformerFrameView *old = view->view.child->generic;
	/* TODO(rnp): hack; it would be better if this was unreachable with a 0 old->frame */
	if (!old->frame)
		return;

	Variable *new_region = ui_split_region(ui, region, view, direction);
	new_region->region_split.right = add_beamformer_frame_view(ui, new_region, &ui->arena,
	                                                           BeamformerFrameViewKind_Copy, 1, old);

	BeamformerFrameView *bv = new_region->region_split.right->view.child->generic;
	ui_beamformer_frame_view_copy_frame(ui, bv, old);
}

function v3
beamformer_frame_view_plane_size(BeamformerUI *ui, BeamformerFrameView *view)
{
	v3 result;
	if (view->kind == BeamformerFrameViewKind_3DXPlane) {
		v3 min = v3_from_f32_array(ui->params.output_min_coordinate);
		v3 max = v3_from_f32_array(ui->params.output_max_coordinate);
		result = v3_sub(max, min);
		swap(result.y, result.z);
		result.x = MAX(1e-3f, result.x);
		result.y = MAX(1e-3f, result.y);
		result.z = MAX(1e-3f, result.z);
	} else {
		v2 size = v2_sub(XZ(view->max_coordinate), XZ(view->min_coordinate));
		result  = (v3){.x = size.x, .y = size.y};
	}
	return result;
}

function f32
x_plane_rotation_for_view_plane(BeamformerFrameView *view, BeamformerViewPlaneTag tag)
{
	f32 result = view->rotation;
	if (tag == BeamformerViewPlaneTag_YZ)
		result += 0.25f;
	return result;
}

function v2
normalized_p_in_rect(Rect r, v2 p, b32 invert_y)
{
	v2 result = v2_div(v2_scale(v2_sub(p, r.pos), 2.0f), r.size);
	if (invert_y) result = (v2){{result.x - 1.0f, 1.0f - result.y}};
	else          result = v2_sub(result, (v2){{1.0f, 1.0f}});
	return result;
}

function v3
x_plane_position(BeamformerUI *ui)
{
	f32 y_min = ui->params.output_min_coordinate[2];
	f32 y_max = ui->params.output_max_coordinate[2];
	v3 result = {.y = y_min + (y_max - y_min) / 2};
	return result;
}

function v3
offset_x_plane_position(BeamformerUI *ui, BeamformerFrameView *view, BeamformerViewPlaneTag tag)
{
	BeamformerSharedMemory          *sm = ui->shared_memory.region;
	BeamformerLiveImagingParameters *li = &sm->live_imaging_parameters;
	m4 x_rotation = m4_rotation_about_y(x_plane_rotation_for_view_plane(view, tag));
	v3 Z = x_rotation.c[2].xyz;
	v3 offset = v3_scale(Z, li->image_plane_offsets[tag]);
	v3 result = v3_add(x_plane_position(ui), offset);
	return result;
}

function v3
camera_for_x_plane_view(BeamformerUI *ui, BeamformerFrameView *view)
{
	v3 size   = beamformer_frame_view_plane_size(ui, view);
	v3 target = x_plane_position(ui);
	f32 dist  = v2_magnitude(XY(size));
	v3 result = v3_add(target, (v3){{dist, -0.5f * size.y * tan_f32(50.0f * PI / 180.0f), dist}});
	return result;
}

function m4
view_matrix_for_x_plane_view(BeamformerUI *ui, BeamformerFrameView *view, v3 camera)
{
	assert(view->kind == BeamformerFrameViewKind_3DXPlane);
	m4 result = camera_look_at(camera, x_plane_position(ui));
	return result;
}

function m4
projection_matrix_for_x_plane_view(BeamformerFrameView *view)
{
	assert(view->kind == BeamformerFrameViewKind_3DXPlane);
	f32 aspect = (f32)view->texture_dim.w / (f32)view->texture_dim.h;
	m4 result = perspective_projection(10e-3f, 500e-3f, 45.0f * PI / 180.0f, aspect);
	return result;
}

function ray
ray_for_x_plane_view(BeamformerUI *ui, BeamformerFrameView *view, v2 uv)
{
	assert(view->kind == BeamformerFrameViewKind_3DXPlane);
	ray result  = {.origin = camera_for_x_plane_view(ui, view)};
	v4 ray_clip = {{uv.x, uv.y, -1.0f, 1.0f}};

	/* TODO(rnp): combine these so we only do one matrix inversion */
	m4 proj_m   = projection_matrix_for_x_plane_view(view);
	m4 view_m   = view_matrix_for_x_plane_view(ui, view, result.origin);
	m4 proj_inv = m4_inverse(proj_m);
	m4 view_inv = m4_inverse(view_m);

	v4 ray_eye  = {.z = -1};
	ray_eye.x   = v4_dot(m4_row(proj_inv, 0), ray_clip);
	ray_eye.y   = v4_dot(m4_row(proj_inv, 1), ray_clip);
	result.direction = v3_normalize(m4_mul_v4(view_inv, ray_eye).xyz);

	return result;
}

function BeamformerViewPlaneTag
view_plane_tag_from_x_plane_shift(BeamformerFrameView *view, Variable *x_plane_shift)
{
	assert(BETWEEN(x_plane_shift, view->x_plane_shifts + 0, view->x_plane_shifts + 1));
	BeamformerViewPlaneTag result = BeamformerViewPlaneTag_XZ;
	if (x_plane_shift == view->x_plane_shifts + 1)
		result = BeamformerViewPlaneTag_YZ;
	return result;
}

function void
render_single_xplane(BeamformerUI *ui, BeamformerFrameView *view, Variable *x_plane_shift,
                     u32 program, f32 rotation_turns, v3 translate, BeamformerViewPlaneTag tag)
{
	u32 texture = 0;
	if (ui->latest_plane[tag])
		texture = ui->latest_plane[tag]->texture;

	v3 scale = beamformer_frame_view_plane_size(ui, view);
	m4 model_transform = y_aligned_volume_transform(scale, translate, rotation_turns);

	v4 colour = v4_lerp(FG_COLOUR, HOVERED_COLOUR, x_plane_shift->hover_t);
	glProgramUniformMatrix4fv(program, FRAME_VIEW_MODEL_MATRIX_LOC, 1, 0, model_transform.E);
	glProgramUniform4fv(program, FRAME_VIEW_BB_COLOUR_LOC, 1, colour.E);
	glProgramUniform1ui(program, FRAME_VIEW_SOLID_BB_LOC, 0);
	glBindTextureUnit(0, texture);
	glDrawElements(GL_TRIANGLES, ui->unit_cube_model.elements, GL_UNSIGNED_SHORT,
	               (void *)ui->unit_cube_model.elements_offset);

	XPlaneShift *xp = &x_plane_shift->x_plane_shift;
	v3 xp_delta = v3_sub(xp->end_point, xp->start_point);
	if (!f32_cmp(v3_magnitude(xp_delta), 0)) {
		m4 x_rotation = m4_rotation_about_y(rotation_turns);
		v3 Z = x_rotation.c[2].xyz;
		v3 f = v3_scale(Z, v3_dot(Z, v3_sub(xp->end_point, xp->start_point)));

		/* TODO(rnp): there is no reason to compute the rotation matrix again */
		model_transform = y_aligned_volume_transform(scale, v3_add(f, translate), rotation_turns);

		glProgramUniformMatrix4fv(program, FRAME_VIEW_MODEL_MATRIX_LOC, 1, 0, model_transform.E);
		glProgramUniform1ui(program, FRAME_VIEW_SOLID_BB_LOC, 1);
		glProgramUniform4fv(program, FRAME_VIEW_BB_COLOUR_LOC, 1, HOVERED_COLOUR.E);
		glDrawElements(GL_TRIANGLES, ui->unit_cube_model.elements, GL_UNSIGNED_SHORT,
		               (void *)ui->unit_cube_model.elements_offset);
	}
}

function void
render_3D_xplane(BeamformerUI *ui, BeamformerFrameView *view, u32 program)
{
	if (view->demo->bool32) {
		view->rotation += dt_for_frame * 0.125f;
		if (view->rotation > 1.0f) view->rotation -= 1.0f;
	}

	v3 camera     = camera_for_x_plane_view(ui, view);
	m4 view_m     = view_matrix_for_x_plane_view(ui, view, camera);
	m4 projection = projection_matrix_for_x_plane_view(view);

	glProgramUniformMatrix4fv(program, FRAME_VIEW_VIEW_MATRIX_LOC,  1, 0, view_m.E);
	glProgramUniformMatrix4fv(program, FRAME_VIEW_PROJ_MATRIX_LOC,  1, 0, projection.E);
	glProgramUniform1f(program, FRAME_VIEW_BB_FRACTION_LOC, FRAME_VIEW_BB_FRACTION);

	v3 model_translate = offset_x_plane_position(ui, view, BeamformerViewPlaneTag_XZ);
	render_single_xplane(ui, view, view->x_plane_shifts + 0, program,
	                     x_plane_rotation_for_view_plane(view, BeamformerViewPlaneTag_XZ),
	                     model_translate, BeamformerViewPlaneTag_XZ);
	model_translate = offset_x_plane_position(ui, view, BeamformerViewPlaneTag_YZ);
	model_translate.y -= 0.0001f;
	render_single_xplane(ui, view, view->x_plane_shifts + 1, program,
	                     x_plane_rotation_for_view_plane(view, BeamformerViewPlaneTag_YZ),
	                     model_translate, BeamformerViewPlaneTag_YZ);
}

function void
render_2D_plane(BeamformerUI *ui, BeamformerFrameView *view, u32 program)
{
	m4 view_m     = m4_identity();
	v3 size       = beamformer_frame_view_plane_size(ui, view);
	m4 model      = m4_scale(size);
	m4 projection = orthographic_projection(0, 1, size.y / 2, size.x / 2);

	glProgramUniformMatrix4fv(program, FRAME_VIEW_MODEL_MATRIX_LOC, 1, 0, model.E);
	glProgramUniformMatrix4fv(program, FRAME_VIEW_VIEW_MATRIX_LOC,  1, 0, view_m.E);
	glProgramUniformMatrix4fv(program, FRAME_VIEW_PROJ_MATRIX_LOC,  1, 0, projection.E);

	glProgramUniform1f(program, FRAME_VIEW_BB_FRACTION_LOC, 0);
	glBindTextureUnit(0, view->frame->texture);
	glDrawElements(GL_TRIANGLES, ui->unit_cube_model.elements, GL_UNSIGNED_SHORT,
	               (void *)ui->unit_cube_model.elements_offset);
}

function b32
frame_view_ready_to_present(BeamformerUI *ui, BeamformerFrameView *view)
{
	b32 result  = !iv2_equal((iv2){0}, view->texture_dim) && view->frame;
	result     |= view->kind == BeamformerFrameViewKind_3DXPlane &&
	              ui->latest_plane[BeamformerViewPlaneTag_Count];
	return result;
}

function b32
view_update(BeamformerUI *ui, BeamformerFrameView *view)
{
	if (view->kind == BeamformerFrameViewKind_Latest) {
		u32 index = *view->cycler->cycler.state;
		view->dirty |= view->frame != ui->latest_plane[index];
		view->frame  = ui->latest_plane[index];
		if (view->dirty) {
			view->min_coordinate = v3_from_f32_array(ui->params.output_min_coordinate);
			view->max_coordinate = v3_from_f32_array(ui->params.output_max_coordinate);
		}
	}

	/* TODO(rnp): x-z or y-z */
	/* TODO(rnp): add method of setting a target size in frame view */
	iv2 current = view->texture_dim;
	iv2 target  = {.w = (i32)ui->params.output_points[0], .h = (i32)ui->params.output_points[2]};
	if (view->kind != BeamformerFrameViewKind_Copy &&
	    view->kind != BeamformerFrameViewKind_3DXPlane &&
	    !iv2_equal(current, target) && !iv2_equal(target, (iv2){0}))
	{
		resize_frame_view(view, target, 1);
		view->dirty = 1;
	}
	view->dirty |= ui->frame_view_render_context->updated;
	view->dirty |= view->kind == BeamformerFrameViewKind_3DXPlane;

	b32 result = frame_view_ready_to_present(ui, view) && view->dirty;
	return result;
}

function void
update_frame_views(BeamformerUI *ui, Rect window)
{
	FrameViewRenderContext *ctx = ui->frame_view_render_context;
	b32 fbo_bound = 0;
	for (BeamformerFrameView *view = ui->views; view; view = view->next) {
		if (view_update(ui, view)) {
			if (!fbo_bound) {
				fbo_bound = 1;
				glBindFramebuffer(GL_FRAMEBUFFER, ctx->framebuffers[0]);
				glUseProgram(ctx->shader);
				glBindVertexArray(ui->unit_cube_model.vao);
				glEnable(GL_DEPTH_TEST);
			}

			u32 fb      = ctx->framebuffers[0];
			u32 program = ctx->shader;
			glViewport(0, 0, view->texture_dim.w, view->texture_dim.h);
			glProgramUniform1f(program,  FRAME_VIEW_THRESHOLD_LOC,     view->threshold.real32);
			glProgramUniform1f(program,  FRAME_VIEW_DYNAMIC_RANGE_LOC, view->dynamic_range.real32);
			glProgramUniform1f(program,  FRAME_VIEW_GAMMA_LOC,         view->gamma.scaled_real32.val);
			glProgramUniform1ui(program, FRAME_VIEW_LOG_SCALE_LOC,     view->log_scale->bool32);

			if (view->kind == BeamformerFrameViewKind_3DXPlane) {
				glNamedFramebufferRenderbuffer(fb, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, ctx->renderbuffers[0]);
				glNamedFramebufferRenderbuffer(fb, GL_DEPTH_ATTACHMENT,  GL_RENDERBUFFER, ctx->renderbuffers[1]);
				glClearNamedFramebufferfv(fb, GL_COLOR, 0, (f32 []){0, 0, 0, 0});
				glClearNamedFramebufferfv(fb, GL_DEPTH, 0, (f32 []){1});
				render_3D_xplane(ui, view, program);
				/* NOTE(rnp): resolve multisampled scene */
				glNamedFramebufferTexture(ctx->framebuffers[1], GL_COLOR_ATTACHMENT0, view->textures[0], 0);
				glBlitNamedFramebuffer(fb, ctx->framebuffers[1], 0, 0, FRAME_VIEW_RENDER_TARGET_SIZE,
				                       0, 0, FRAME_VIEW_RENDER_TARGET_SIZE, GL_COLOR_BUFFER_BIT, GL_NEAREST);
			} else {
				glNamedFramebufferTexture(fb, GL_COLOR_ATTACHMENT0, view->textures[0], 0);
				glNamedFramebufferTexture(fb, GL_DEPTH_ATTACHMENT,  view->textures[1], 0);
				glClearNamedFramebufferfv(fb, GL_COLOR, 0, (f32 []){0, 0, 0, 0});
				glClearNamedFramebufferfv(fb, GL_DEPTH, 0, (f32 []){1});
				render_2D_plane(ui, view, program);
			}
			glGenerateTextureMipmap(view->textures[0]);
			view->dirty = 0;
		}
	}
	if (fbo_bound) {
		glBindFramebuffer(GL_FRAMEBUFFER, 0);
		glViewport((i32)window.pos.x, (i32)window.pos.y, (i32)window.size.w, (i32)window.size.h);
		/* NOTE(rnp): I don't trust raylib to not mess with us */
		glBindVertexArray(0);
		glDisable(GL_DEPTH_TEST);
	}
}

function Color
colour_from_normalized(v4 rgba)
{
	Color result = {.r = (u8)(rgba.r * 255.0f), .g = (u8)(rgba.g * 255.0f),
	                .b = (u8)(rgba.b * 255.0f), .a = (u8)(rgba.a * 255.0f)};
	return result;
}

function Color
fade(Color a, f32 visibility)
{
	a.a = (u8)((f32)a.a * visibility);
	return a;
}

function v2
draw_text_base(Font font, s8 text, v2 pos, Color colour)
{
	v2 off = v2_floor(pos);
	f32 glyph_pad = (f32)font.glyphPadding;
	for (iz i = 0; i < text.len; i++) {
		/* NOTE: assumes font glyphs are ordered ASCII */
		i32 idx = text.data[i] - 0x20;
		Rectangle dst = {
			off.x + (f32)font.glyphs[idx].offsetX - glyph_pad,
			off.y + (f32)font.glyphs[idx].offsetY - glyph_pad,
			font.recs[idx].width  + 2.0f * glyph_pad,
			font.recs[idx].height + 2.0f * glyph_pad
		};
		Rectangle src = {
			font.recs[idx].x - glyph_pad,
			font.recs[idx].y - glyph_pad,
			font.recs[idx].width  + 2.0f * glyph_pad,
			font.recs[idx].height + 2.0f * glyph_pad
		};
		DrawTexturePro(font.texture, src, dst, (Vector2){0}, 0, colour);

		off.x += (f32)font.glyphs[idx].advanceX;
		if (font.glyphs[idx].advanceX == 0)
			off.x += font.recs[idx].width;
	}
	v2 result = {{off.x - pos.x, (f32)font.baseSize}};
	return result;
}

/* NOTE(rnp): expensive but of the available options in raylib this gives the best results */
function v2
draw_outlined_text(s8 text, v2 pos, TextSpec *ts)
{
	f32 ow = ts->outline_thick;
	Color outline = colour_from_normalized(ts->outline_colour);
	Color colour  = colour_from_normalized(ts->colour);
	draw_text_base(*ts->font, text, v2_sub(pos, (v2){{ ow,  ow}}), outline);
	draw_text_base(*ts->font, text, v2_sub(pos, (v2){{ ow, -ow}}), outline);
	draw_text_base(*ts->font, text, v2_sub(pos, (v2){{-ow,  ow}}), outline);
	draw_text_base(*ts->font, text, v2_sub(pos, (v2){{-ow, -ow}}), outline);

	v2 result = draw_text_base(*ts->font, text, pos, colour);

	return result;
}

function v2
draw_text(s8 text, v2 pos, TextSpec *ts)
{
	if (ts->flags & TF_ROTATED) {
		rlPushMatrix();
		rlTranslatef(pos.x, pos.y, 0);
		rlRotatef(ts->rotation, 0, 0, 1);
		pos = (v2){0};
	}

	v2 result   = measure_text(*ts->font, text);
	/* TODO(rnp): the size of this should be stored for each font */
	s8 ellipsis = s8("...");
	b32 clamped = ts->flags & TF_LIMITED && result.w > ts->limits.size.w;
	if (clamped) {
		f32 ellipsis_width = measure_text(*ts->font, ellipsis).x;
		if (ellipsis_width < ts->limits.size.w) {
			text = clamp_text_to_width(*ts->font, text, ts->limits.size.w - ellipsis_width);
		} else {
			text.len     = 0;
			ellipsis.len = 0;
		}
	}

	Color colour = colour_from_normalized(ts->colour);
	if (ts->flags & TF_OUTLINED) result.x = draw_outlined_text(text, pos, ts).x;
	else                         result.x = draw_text_base(*ts->font, text, pos, colour).x;

	if (clamped) {
		pos.x += result.x;
		if (ts->flags & TF_OUTLINED) result.x += draw_outlined_text(ellipsis, pos, ts).x;
		else                         result.x += draw_text_base(*ts->font, ellipsis, pos,
		                                                        colour).x;
	}

	if (ts->flags & TF_ROTATED) rlPopMatrix();

	return result;
}

function Rect
extend_rect_centered(Rect r, v2 delta)
{
	r.size.w += delta.x;
	r.size.h += delta.y;
	r.pos.x  -= delta.x / 2;
	r.pos.y  -= delta.y / 2;
	return r;
}

function Rect
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

function Rect
scale_rect_centered(Rect r, v2 scale)
{
	Rect or   = r;
	r.size.w *= scale.x;
	r.size.h *= scale.y;
	r.pos.x  += (or.size.w - r.size.w) / 2;
	r.pos.y  += (or.size.h - r.size.h) / 2;
	return r;
}

function b32
interactions_equal(Interaction a, Interaction b)
{
	b32 result = (a.kind == b.kind) && (a.generic == b.generic);
	return result;
}

function b32
interaction_is_sticky(Interaction a)
{
	b32 result = a.kind == InteractionKind_Text || a.kind == InteractionKind_Ruler;
	return result;
}

function b32
interaction_is_hot(BeamformerUI *ui, Interaction a)
{
	b32 result = interactions_equal(ui->hot_interaction, a);
	return result;
}

function b32
point_in_rect(v2 p, Rect r)
{
	v2  end    = v2_add(r.pos, r.size);
	b32 result = BETWEEN(p.x, r.pos.x, end.x) & BETWEEN(p.y, r.pos.y, end.y);
	return result;
}

function v2
screen_point_to_world_2d(v2 p, v2 screen_min, v2 screen_max, v2 world_min, v2 world_max)
{
	v2 pixels_to_m = v2_div(v2_sub(world_max, world_min), v2_sub(screen_max, screen_min));
	v2 result      = v2_add(v2_mul(v2_sub(p, screen_min), pixels_to_m), world_min);
	return result;
}

function v2
world_point_to_screen_2d(v2 p, v2 world_min, v2 world_max, v2 screen_min, v2 screen_max)
{
	v2 m_to_pixels = v2_div(v2_sub(screen_max, screen_min), v2_sub(world_max, world_min));
	v2 result      = v2_add(v2_mul(v2_sub(p, world_min), m_to_pixels), screen_min);
	return result;
}

function b32
hover_interaction(BeamformerUI *ui, v2 mouse, Interaction interaction)
{
	Variable *var = interaction.var;
	b32 result = point_in_rect(mouse, interaction.rect);
	if (result) ui->next_interaction = interaction;
	if (interaction_is_hot(ui, interaction)) var->hover_t += HOVER_SPEED * dt_for_frame;
	else                                     var->hover_t -= HOVER_SPEED * dt_for_frame;
	var->hover_t = CLAMP01(var->hover_t);
	return result;
}

function void
draw_close_button(BeamformerUI *ui, Variable *close, v2 mouse, Rect r, v2 x_scale)
{
	assert(close->type == VT_UI_BUTTON);
	hover_interaction(ui, mouse, auto_interaction(r, close));

	Color colour = colour_from_normalized(v4_lerp(MENU_CLOSE_COLOUR, FG_COLOUR, close->hover_t));
	r = scale_rect_centered(r, x_scale);
	DrawLineEx(r.pos.rl, v2_add(r.pos, r.size).rl, 4, colour);
	DrawLineEx(v2_add(r.pos, (v2){.x = r.size.w}).rl,
	           v2_add(r.pos, (v2){.y = r.size.h}).rl, 4, colour);
}

function Rect
draw_title_bar(BeamformerUI *ui, Arena arena, Variable *ui_view, Rect r, v2 mouse)
{
	assert(ui_view->type == VT_UI_VIEW);
	UIView *view = &ui_view->view;

	s8 title = ui_view->name;
	if (view->flags & UIViewFlag_CustomText) {
		Stream buf = arena_stream(arena);
		push_custom_view_title(&buf, ui_view->view.child);
		title = arena_stream_commit(&arena, &buf);
	}

	Rect result, title_rect;
	cut_rect_vertical(r, (f32)ui->small_font.baseSize + TITLE_BAR_PAD, &title_rect, &result);
	cut_rect_vertical(result, LISTING_LINE_PAD, 0, &result);

	DrawRectangleRec(title_rect.rl, BLACK);

	title_rect = shrink_rect_centered(title_rect, (v2){.x = 1.5f * TITLE_BAR_PAD});
	DrawRectangleRounded(title_rect.rl, 0.5f, 0, fade(colour_from_normalized(BG_COLOUR), 0.55f));
	title_rect = shrink_rect_centered(title_rect, (v2){.x = 3.0f * TITLE_BAR_PAD});

	if (view->close) {
		Rect close;
		cut_rect_horizontal(title_rect, title_rect.size.w - title_rect.size.h, &title_rect, &close);
		draw_close_button(ui, view->close, mouse, close, (v2){{0.4f, 0.4f}});
	}

	if (view->menu) {
		Rect menu;
		cut_rect_horizontal(title_rect, title_rect.size.w - title_rect.size.h, &title_rect, &menu);
		Interaction interaction = {.kind = InteractionKind_Menu, .var = view->menu, .rect = menu};
		hover_interaction(ui, mouse, interaction);

		Color colour = colour_from_normalized(v4_lerp(MENU_PLUS_COLOUR, FG_COLOUR, view->menu->hover_t));
		menu = shrink_rect_centered(menu, (v2){{14.0f, 14.0f}});
		DrawLineEx(v2_add(menu.pos, (v2){.x = menu.size.w / 2}).rl,
		           v2_add(menu.pos, (v2){.x = menu.size.w / 2, .y = menu.size.h}).rl, 4, colour);
		DrawLineEx(v2_add(menu.pos, (v2){.y = menu.size.h / 2}).rl,
		           v2_add(menu.pos, (v2){.x = menu.size.w, .y = menu.size.h / 2}).rl, 4, colour);
	}

	v2 title_pos = title_rect.pos;
	title_pos.y += 0.5f * TITLE_BAR_PAD;
	TextSpec text_spec = {.font = &ui->small_font, .flags = TF_LIMITED, .colour = FG_COLOUR,
	                      .limits.size = title_rect.size};
	draw_text(title, title_pos, &text_spec);

	return result;
}

/* TODO(rnp): once this has more callers decide if it would be better for this to take
 * an orientation rather than force CCW/right-handed */
function void
draw_ruler(BeamformerUI *ui, Arena arena, v2 start_point, v2 end_point,
           f32 start_value, f32 end_value, f32 *markers, u32 marker_count,
           u32 segments, s8 suffix, v4 marker_colour, v4 txt_colour)
{
	b32 draw_plus = SIGN(start_value) != SIGN(end_value);

	end_point    = v2_sub(end_point, start_point);
	f32 rotation = atan2_f32(end_point.y, end_point.x) * 180 / PI;

	rlPushMatrix();
	rlTranslatef(start_point.x, start_point.y, 0);
	rlRotatef(rotation, 0, 0, 1);

	f32 inc       = v2_magnitude(end_point) / (f32)segments;
	f32 value_inc = (end_value - start_value) / (f32)segments;
	f32 value     = start_value;

	Stream buf = arena_stream(arena);
	v2 sp = {0}, ep = {.y = RULER_TICK_LENGTH};
	v2 tp = {{(f32)ui->small_font.baseSize / 2.0f, ep.y + RULER_TEXT_PAD}};
	TextSpec text_spec = {.font = &ui->small_font, .rotation = 90.0f, .colour = txt_colour, .flags = TF_ROTATED};
	Color rl_txt_colour = colour_from_normalized(txt_colour);
	for (u32 j = 0; j <= segments; j++) {
		DrawLineEx(sp.rl, ep.rl, 3, rl_txt_colour);

		stream_reset(&buf, 0);
		if (draw_plus && value > 0) stream_append_byte(&buf, '+');
		stream_append_f64(&buf, value, 10);
		stream_append_s8(&buf, suffix);
		draw_text(stream_to_s8(&buf), tp, &text_spec);

		value += value_inc;
		sp.x  += inc;
		ep.x  += inc;
		tp.x  += inc;
	}

	Color rl_marker_colour = colour_from_normalized(marker_colour);
	ep.y += RULER_TICK_LENGTH;
	for (u32 i = 0; i < marker_count; i++) {
		if (markers[i] < F32_INFINITY) {
			ep.x  = sp.x = markers[i];
			DrawLineEx(sp.rl, ep.rl, 3, rl_marker_colour);
			DrawCircleV(ep.rl, 3, rl_marker_colour);
		}
	}

	rlPopMatrix();
}

function void
do_scale_bar(BeamformerUI *ui, Arena arena, Variable *scale_bar, v2 mouse, Rect draw_rect,
             f32 start_value, f32 end_value, s8 suffix)
{
	assert(scale_bar->type == VT_SCALE_BAR);
	ScaleBar *sb = &scale_bar->scale_bar;

	v2 txt_s = measure_text(ui->small_font, s8("-288.8 mm"));

	Rect tick_rect = draw_rect;
	v2   start_pos = tick_rect.pos;
	v2   end_pos   = tick_rect.pos;
	v2   relative_mouse = v2_sub(mouse, tick_rect.pos);

	f32  markers[2];
	u32  marker_count = 1;

	v2 world_zoom_point  = {{sb->zoom_starting_coord, sb->zoom_starting_coord}};
	v2 screen_zoom_point = world_point_to_screen_2d(world_zoom_point,
	                                                (v2){{*sb->min_value, *sb->min_value}},
	                                                (v2){{*sb->max_value, *sb->max_value}},
	                                                (v2){0}, tick_rect.size);
	u32  tick_count;
	if (sb->direction == SB_AXIAL) {
		tick_rect.size.x  = RULER_TEXT_PAD + RULER_TICK_LENGTH + txt_s.x;
		tick_count        = (u32)(tick_rect.size.y / (1.5f * (f32)ui->small_font.baseSize));
		start_pos.y      += tick_rect.size.y;
		markers[0]        = tick_rect.size.y - screen_zoom_point.y;
		markers[1]        = tick_rect.size.y - relative_mouse.y;
	} else {
		tick_rect.size.y  = RULER_TEXT_PAD + RULER_TICK_LENGTH + txt_s.x;
		tick_count        = (u32)(tick_rect.size.x / (1.5f * (f32)ui->small_font.baseSize));
		end_pos.x        += tick_rect.size.x;
		markers[0]        = screen_zoom_point.x;
		markers[1]        = relative_mouse.x;
	}

	if (hover_interaction(ui, mouse, auto_interaction(tick_rect, scale_bar)))
		marker_count = 2;

	draw_ruler(ui, arena, start_pos, end_pos, start_value, end_value, markers, marker_count,
	           tick_count, suffix, RULER_COLOUR, v4_lerp(FG_COLOUR, HOVERED_COLOUR, scale_bar->hover_t));
}

function v2
draw_radio_button(BeamformerUI *ui, Variable *var, v2 at, v2 mouse, v4 base_colour, f32 size)
{
	assert(var->type == VT_B32);
	b32 value = var->bool32;

	v2 result = (v2){.x = size, .y = size};
	Rect hover_rect   = {.pos = at, .size = result};
	hover_rect.pos.y += 1;
	hover_interaction(ui, mouse, auto_interaction(hover_rect, var));

	hover_rect = shrink_rect_centered(hover_rect, (v2){{8.0f, 8.0f}});
	Rect inner = shrink_rect_centered(hover_rect, (v2){{4.0f, 4.0f}});
	v4 fill = v4_lerp(value? base_colour : (v4){0}, HOVERED_COLOUR, var->hover_t);
	DrawRectangleRoundedLinesEx(hover_rect.rl, 0.2f, 0, 2, colour_from_normalized(base_colour));
	DrawRectangleRec(inner.rl, colour_from_normalized(fill));

	return result;
}

function f32
draw_variable_slider(BeamformerUI *ui, Variable *var, Rect r, f32 fill, v4 fill_colour, v2 mouse)
{
	f32  border_thick    = 3.0f;
	f32  bar_height_frac = 0.8f;
	v2   bar_size        = {{6.0f, bar_height_frac * r.size.y}};

	Rect inner  = shrink_rect_centered(r, (v2){{2.0f * border_thick, // NOTE(rnp): raylib jank
	                                            MAX(0, 2.0f * (r.size.y - bar_size.y))}});
	Rect filled = inner;
	filled.size.w *= fill;

	Rect bar;
	bar.pos  = v2_add(r.pos, (v2){{fill * (r.size.w - bar_size.w), (1 - bar_height_frac) * 0.5f * r.size.y}});
	bar.size = bar_size;
	v4 bar_colour = v4_lerp(FG_COLOUR, FOCUSED_COLOUR, var->hover_t);

	hover_interaction(ui, mouse, auto_interaction(inner, var));

	DrawRectangleRec(filled.rl, colour_from_normalized(fill_colour));
	DrawRectangleRoundedLinesEx(inner.rl, 0.2f, 0, border_thick, BLACK);
	DrawRectangleRounded(bar.rl, 0.6f, 1, colour_from_normalized(bar_colour));

	return r.size.y;
}

function v2
draw_fancy_button(BeamformerUI *ui, Variable *var, s8 label, Rect r, v4 border_colour, v2 mouse, TextSpec ts)
{
	assert((f32)ts.font->baseSize <= r.size.h * 0.8f);
	f32 pad = 0.1f * r.size.h;

	v2   shadow_off   = {{2.5f, 3.0f}};
	f32  border_thick = 3.0f;
	v2   border_size  = v2_add((v2){{pad + 2.0f * border_thick, pad}}, shadow_off);

	Rect border = shrink_rect_centered(r,      border_size);
	Rect inner  = shrink_rect_centered(border, (v2){{pad, pad}});

	ts.limits.size = inner.size;
	hover_interaction(ui, mouse, auto_interaction(inner, var));

	border.pos = v2_add(border.pos, shadow_off);

	DrawRectangleRoundedLinesEx(border.rl, 0.6f, 0, border_thick, fade(BLACK, 0.8f));
	border.pos = v2_sub(border.pos, shadow_off);
	DrawRectangleRounded(border.rl, 0.6f, 1, colour_from_normalized(BG_COLOUR));
	DrawRectangleRoundedLinesEx(border.rl, 0.6f, 0, border_thick, colour_from_normalized(border_colour));

	/* TODO(rnp): teach draw_text() about alignment */
	v2 at = align_text_in_rect(label, inner, *ts.font);
	at = v2_add(at, (v2){{3.0f, 3.0f}});
	v4 base_colour = ts.colour;
	ts.colour = (v4){{0, 0, 0, 0.8f}};
	draw_text(label, at, &ts);

	at = v2_sub(at, (v2){{3.0f, 3.0f}});
	ts.colour = v4_lerp(base_colour, HOVERED_COLOUR, var->hover_t);
	draw_text(label, at, &ts);

	v2 result = v2_add(r.size, border_size);
	return result;
}

function v2
draw_variable(BeamformerUI *ui, Arena arena, Variable *var, v2 at, v2 mouse, v4 base_colour, TextSpec text_spec)
{
	v2 result;
	if (var->flags & V_RADIO_BUTTON) {
		result = draw_radio_button(ui, var, at, mouse, base_colour, (f32)text_spec.font->baseSize);
	} else {
		Stream buf = arena_stream(arena);
		stream_append_variable(&buf, var);
		s8 text = arena_stream_commit(&arena, &buf);
		result = measure_text(*text_spec.font, text);

		if (var->flags & V_INPUT) {
			Rect text_rect = {.pos = at, .size = result};
			text_rect = extend_rect_centered(text_rect, (v2){.x = 8});
			if (hover_interaction(ui, mouse, auto_interaction(text_rect, var)) && (var->flags & V_TEXT))
				ui->text_input_state.hot_font = text_spec.font;
			text_spec.colour = v4_lerp(base_colour, HOVERED_COLOUR, var->hover_t);
		}

		draw_text(text, at, &text_spec);
	}
	return result;
}

function void
draw_table_cell(BeamformerUI *ui, Arena arena, TableCell *cell, Rect cell_rect,
                TextAlignment alignment, TextSpec ts, v2 mouse)
{
	f32 x_off  = cell_rect.pos.x;
	v2 cell_at = table_cell_align(cell, alignment, cell_rect);
	ts.limits.size.w -= (cell_at.x - x_off);
	cell_rect.size.w  = MIN(ts.limits.size.w, cell_rect.size.w);

	/* TODO(rnp): push truncated text for hovering */
	switch (cell->kind) {
	case TableCellKind_None:{ draw_text(cell->text, cell_at, &ts); }break;
	case TableCellKind_Variable:{
		if (cell->var->flags & V_INPUT) {
			draw_variable(ui, arena, cell->var, cell_at, mouse, ts.colour, ts);
		} else if (cell->text.len) {
			draw_text(cell->text, cell_at, &ts);
		}
	}break;
	case TableCellKind_VariableGroup:{
		Variable *v = cell->var->group.first;
		f32 dw = draw_text(s8("{"), cell_at, &ts).x;
		while (v) {
			cell_at.x        += dw;
			ts.limits.size.w -= dw;
			dw = draw_variable(ui, arena, v, cell_at, mouse, ts.colour, ts).x;

			v = v->next;
			if (v) {
				cell_at.x        += dw;
				ts.limits.size.w -= dw;
				dw = draw_text(s8(", "), cell_at, &ts).x;
			}
		}
		cell_at.x        += dw;
		ts.limits.size.w -= dw;
		draw_text(s8("}"), cell_at, &ts);
	}break;
	}
}

function void
draw_table_borders(Table *t, Rect r, f32 line_height)
{
	if (t->column_border_thick > 0) {
		v2 start  = {.x = r.pos.x, .y = r.pos.y + t->cell_pad.h / 2};
		v2 end    = start;
		end.y    += t->size.y - t->cell_pad.y;
		for (i32 i = 0; i < t->columns - 1; i++) {
			f32 dx = t->widths[i] + t->cell_pad.w + t->column_border_thick;
			start.x += dx;
			end.x   += dx;
			if (t->widths[i + 1] > 0)
				DrawLineEx(start.rl, end.rl, t->column_border_thick, fade(BLACK, 0.8f));
		}
	}

	if (t->row_border_thick > 0) {
		v2 start  = {.x = r.pos.x + t->cell_pad.w / 2, .y = r.pos.y};
		v2 end    = start;
		end.x    += t->size.x - t->cell_pad.x;
		for (i32 i = 0; i < t->rows - 1; i++) {
			f32 dy   = line_height + t->cell_pad.y + t->row_border_thick;
			start.y += dy;
			end.y   += dy;
			DrawLineEx(start.rl, end.rl, t->row_border_thick, fade(BLACK, 0.8f));
		}
	}
}

function v2
draw_table(BeamformerUI *ui, Arena arena, Table *table, Rect draw_rect, TextSpec ts, v2 mouse, b32 skip_rows)
{
	ts.flags |= TF_LIMITED;

	v2 result         = {.x = table_width(table)};
	i32 row_index     = skip_rows? table_skip_rows(table, draw_rect.size.h, (f32)ts.font->baseSize) : 0;
	TableIterator *it = table_iterator_new(table, TIK_CELLS, &arena, row_index, draw_rect.pos, ts.font);
	for (TableCell *cell = table_iterator_next(it, &arena);
	     cell;
	     cell = table_iterator_next(it, &arena))
	{
		ts.limits.size.w = draw_rect.size.w - (it->cell_rect.pos.x - it->start_x);
		draw_table_cell(ui, arena, cell, it->cell_rect, it->alignment, ts, mouse);
	}
	draw_table_borders(table, draw_rect, (f32)ts.font->baseSize);
	result.y = it->cell_rect.pos.y - draw_rect.pos.y - table->cell_pad.h / 2.0f;
	return result;
}

function void
draw_view_ruler(BeamformerFrameView *view, Arena a, Rect view_rect, TextSpec ts)
{
	v2 vr_max_p = v2_add(view_rect.pos, view_rect.size);
	v2 start_p  = world_point_to_screen_2d(view->ruler.start, XZ(view->min_coordinate),
	                                       XZ(view->max_coordinate), view_rect.pos, vr_max_p);
	v2 end_p    = world_point_to_screen_2d(view->ruler.end, XZ(view->min_coordinate),
	                                       XZ(view->max_coordinate), view_rect.pos, vr_max_p);

	Color rl_colour = colour_from_normalized(ts.colour);
	DrawCircleV(start_p.rl, 3, rl_colour);
	DrawLineEx(end_p.rl, start_p.rl, 2, rl_colour);
	DrawCircleV(end_p.rl, 3, rl_colour);

	Stream buf = arena_stream(a);
	stream_append_f64(&buf, 1e3 * v2_magnitude(v2_sub(view->ruler.end, view->ruler.start)), 100);
	stream_append_s8(&buf, s8(" mm"));

	v2 txt_p = start_p;
	v2 txt_s = measure_text(*ts.font, stream_to_s8(&buf));
	v2 pixel_delta = v2_sub(start_p, end_p);
	if (pixel_delta.y < 0) txt_p.y -= txt_s.y;
	if (pixel_delta.x < 0) txt_p.x -= txt_s.x;
	if (txt_p.x < view_rect.pos.x) txt_p.x = view_rect.pos.x;
	if (txt_p.x + txt_s.x > vr_max_p.x) txt_p.x -= (txt_p.x + txt_s.x) - vr_max_p.x;

	draw_text(stream_to_s8(&buf), txt_p, &ts);
}

function v2
draw_frame_view_controls(BeamformerUI *ui, Arena arena, BeamformerFrameView *view, Rect vr, v2 mouse)
{
	TextSpec text_spec = {.font = &ui->small_font, .flags = TF_LIMITED|TF_OUTLINED,
	                      .colour = RULER_COLOUR, .outline_thick = 1, .outline_colour.a = 1,
	                      .limits.size.x = vr.size.w};

	Table *table = table_new(&arena, 3, TextAlignment_Left, TextAlignment_Left, TextAlignment_Left);
	table_push_parameter_row(table, &arena, view->gamma.name,     &view->gamma,     s8(""));
	table_push_parameter_row(table, &arena, view->threshold.name, &view->threshold, s8(""));
	if (view->log_scale->bool32)
		table_push_parameter_row(table, &arena, view->dynamic_range.name, &view->dynamic_range, s8("[dB]"));

	Rect table_rect = vr;
	f32 height      = table_extent(table, arena, text_spec.font).y;
	height          = MIN(height, vr.size.h);
	table_rect.pos.w  += 8;
	table_rect.pos.y  += vr.size.h - height - 8;
	table_rect.size.h  = height;
	table_rect.size.w  = vr.size.w - 16;

	return draw_table(ui, arena, table, table_rect, text_spec, mouse, 0);
}

function void
draw_3D_xplane_frame_view(BeamformerUI *ui, Arena arena, Variable *var, Rect display_rect, v2 mouse)
{
	assert(var->type == VT_BEAMFORMER_FRAME_VIEW);
	BeamformerFrameView *view  = var->generic;

	f32 aspect = (f32)view->texture_dim.w / (f32)view->texture_dim.h;
	Rect vr = display_rect;
	if (aspect > 1.0f) vr.size.w = vr.size.h;
	else               vr.size.h = vr.size.w;

	if (vr.size.w > display_rect.size.w) {
		vr.size.w -= (vr.size.w - display_rect.size.w);
		vr.size.h  = vr.size.w / aspect;
	} else if (vr.size.h > display_rect.size.h) {
		vr.size.h -= (vr.size.h - display_rect.size.h);
		vr.size.w  = vr.size.h * aspect;
	}
	vr.pos = v2_add(vr.pos, v2_scale(v2_sub(display_rect.size, vr.size), 0.5));

	i32 id = -1;
	if (hover_interaction(ui, mouse, auto_interaction(vr, var))) {
		ray mouse_ray  = ray_for_x_plane_view(ui, view, normalized_p_in_rect(vr, mouse, 0));
		v3  x_size     = v3_scale(beamformer_frame_view_plane_size(ui, view), 0.5f);

		f32 rotation   = x_plane_rotation_for_view_plane(view, BeamformerViewPlaneTag_XZ);
		m4  x_rotation = m4_rotation_about_y(rotation);
		v3  x_position = offset_x_plane_position(ui, view, BeamformerViewPlaneTag_XZ);

		f32 test[2] = {0};
		test[0] = obb_raycast(x_rotation, x_size, x_position, mouse_ray);

		x_position = offset_x_plane_position(ui, view, BeamformerViewPlaneTag_YZ);
		rotation   = x_plane_rotation_for_view_plane(view, BeamformerViewPlaneTag_YZ);
		x_rotation = m4_rotation_about_y(rotation);
		test[1] = obb_raycast(x_rotation, x_size, x_position, mouse_ray);

		if (test[0] >= 0 && test[1] >= 0) id = test[1] < test[0]? 1 : 0;
		else if (test[0] >= 0) id = 0;
		else if (test[1] >= 0) id = 1;

		if (id != -1) {
			view->hit_test_point = v3_add(mouse_ray.origin, v3_scale(mouse_ray.direction, test[id]));
		}
	}

	for (i32 i = 0; i < countof(view->x_plane_shifts); i++) {
		Variable *it = view->x_plane_shifts + i;
		Interaction interaction = auto_interaction(vr, it);
		if (id == i) ui->next_interaction = interaction;
		if (interaction_is_hot(ui, interaction)) it->hover_t += HOVER_SPEED * dt_for_frame;
		else                                     it->hover_t -= HOVER_SPEED * dt_for_frame;
		it->hover_t = CLAMP01(it->hover_t);
	}

	Rectangle  tex_r  = {0, 0, (f32)view->texture_dim.w, (f32)view->texture_dim.h};
	NPatchInfo tex_np = {tex_r, 0, 0, 0, 0, NPATCH_NINE_PATCH};
	DrawTextureNPatch(make_raylib_texture(view), tex_np, vr.rl, (Vector2){0}, 0, WHITE);

	draw_frame_view_controls(ui, arena, view, vr, mouse);
}

function void
draw_beamformer_frame_view(BeamformerUI *ui, Arena a, Variable *var, Rect display_rect, v2 mouse)
{
	assert(var->type == VT_BEAMFORMER_FRAME_VIEW);
	BeamformerFrameView *view  = var->generic;
	BeamformerFrame     *frame = view->frame;

	f32 txt_w = measure_text(ui->small_font, s8("-288.8 mm")).w;
	f32 scale_bar_size = 1.2f * txt_w + RULER_TICK_LENGTH;

	v3 min = view->min_coordinate;
	v3 max = view->max_coordinate;
	v2 requested_dim = v2_sub(XZ(max), XZ(min));
	f32 aspect = requested_dim.w / requested_dim.h;

	Rect vr = display_rect;
	v2 scale_bar_area = {0};
	if (view->axial_scale_bar_active->bool32) {
		vr.pos.y         += 0.5f * (f32)ui->small_font.baseSize;
		scale_bar_area.x += scale_bar_size;
		scale_bar_area.y += (f32)ui->small_font.baseSize;
	}

	if (view->lateral_scale_bar_active->bool32) {
		vr.pos.x         += 0.5f * (f32)ui->small_font.baseSize;
		scale_bar_area.x += (f32)ui->small_font.baseSize;
		scale_bar_area.y += scale_bar_size;
	}

	vr.size = v2_sub(vr.size, scale_bar_area);
	if (aspect > 1) vr.size.h = vr.size.w / aspect;
	else            vr.size.w = vr.size.h * aspect;

	v2 occupied = v2_add(vr.size, scale_bar_area);
	if (occupied.w > display_rect.size.w) {
		vr.size.w -= (occupied.w - display_rect.size.w);
		vr.size.h  = vr.size.w / aspect;
	} else if (occupied.h > display_rect.size.h) {
		vr.size.h -= (occupied.h - display_rect.size.h);
		vr.size.w  = vr.size.h * aspect;
	}
	occupied = v2_add(vr.size, scale_bar_area);
	vr.pos   = v2_add(vr.pos, v2_scale(v2_sub(display_rect.size, occupied), 0.5));

	/* TODO(rnp): make this depend on the requested draw orientation (x-z or y-z or x-y) */
	v2 output_dim = v2_sub(XZ(frame->max_coordinate), XZ(frame->min_coordinate));
	v2 pixels_per_meter = {
		.w = (f32)view->texture_dim.w / output_dim.w,
		.h = (f32)view->texture_dim.h / output_dim.h,
	};

	/* NOTE(rnp): math to resize the texture without stretching when the view changes
	 * but the texture hasn't been (or cannot be) rebeamformed */
	v2 texture_points  = v2_mul(pixels_per_meter, requested_dim);
	/* TODO(rnp): this also depends on x-y, y-z, x-z */
	v2 texture_start   = {
		.x = pixels_per_meter.x * 0.5f * (output_dim.x - requested_dim.x),
		.y = pixels_per_meter.y * (frame->max_coordinate.z - max.z),
	};

	Rectangle  tex_r  = {texture_start.x, texture_start.y, texture_points.x, texture_points.y};
	NPatchInfo tex_np = { tex_r, 0, 0, 0, 0, NPATCH_NINE_PATCH };
	DrawTextureNPatch(make_raylib_texture(view), tex_np, vr.rl, (Vector2){0}, 0, WHITE);

	v2 start_pos  = vr.pos;
	start_pos.y  += vr.size.y;

	if (vr.size.w > 0 && view->lateral_scale_bar_active->bool32) {
		do_scale_bar(ui, a, &view->lateral_scale_bar, mouse,
		             (Rect){.pos = start_pos, .size = vr.size},
		             *view->lateral_scale_bar.scale_bar.min_value * 1e3f,
		             *view->lateral_scale_bar.scale_bar.max_value * 1e3f, s8(" mm"));
	}

	start_pos    = vr.pos;
	start_pos.x += vr.size.x;

	if (vr.size.h > 0 && view->axial_scale_bar_active->bool32) {
		do_scale_bar(ui, a, &view->axial_scale_bar, mouse,
		             (Rect){.pos = start_pos, .size = vr.size},
		             *view->axial_scale_bar.scale_bar.max_value * 1e3f,
		             *view->axial_scale_bar.scale_bar.min_value * 1e3f, s8(" mm"));
	}

	TextSpec text_spec = {.font = &ui->small_font, .flags = TF_LIMITED|TF_OUTLINED,
	                      .colour = RULER_COLOUR, .outline_thick = 1, .outline_colour.a = 1,
	                      .limits.size.x = vr.size.w};

	f32 draw_table_width = vr.size.w;
	/* NOTE: avoid hover_t modification */
	Interaction viewer = auto_interaction(vr, var);
	if (point_in_rect(mouse, viewer.rect)) {
		ui->next_interaction = viewer;

		v2 world = screen_point_to_world_2d(mouse, vr.pos, v2_add(vr.pos, vr.size),
		                                    XZ(view->min_coordinate),
		                                    XZ(view->max_coordinate));
		Stream buf = arena_stream(a);
		stream_append_v2(&buf, v2_scale(world, 1e3f));

		text_spec.limits.size.w -= 4.0f;
		v2 txt_s = measure_text(*text_spec.font, stream_to_s8(&buf));
		v2 txt_p = {
			.x = vr.pos.x + vr.size.w - txt_s.w - 4.0f,
			.y = vr.pos.y + vr.size.h - txt_s.h - 4.0f,
		};
		txt_p.x = MAX(vr.pos.x, txt_p.x);
		draw_table_width -= draw_text(stream_to_s8(&buf), txt_p, &text_spec).w;
		text_spec.limits.size.w += 4.0f;
	}

	{
		Stream buf = arena_stream(a);
		s8 shader  = push_das_shader_kind(&buf, frame->das_shader_kind, frame->compound_count);
		text_spec.font = &ui->font;
		text_spec.limits.size.w -= 16;
		v2 txt_s  = measure_text(*text_spec.font, shader);
		v2 txt_p  = {
			.x = vr.pos.x + vr.size.w - txt_s.w - 16,
			.y = vr.pos.y + 4,
		};
		txt_p.x = MAX(vr.pos.x, txt_p.x);
		draw_text(stream_to_s8(&buf), txt_p, &text_spec);
		text_spec.font = &ui->small_font;
		text_spec.limits.size.w += 16;
	}

	if (view->ruler.state != RulerState_None) draw_view_ruler(view, a, vr, text_spec);

	vr.size.w = draw_table_width;
	draw_frame_view_controls(ui, a, view, vr, mouse);
}

function v2
draw_compute_progress_bar(BeamformerUI *ui, ComputeProgressBar *state, Rect r)
{
	if (*state->processing) state->display_t_velocity += 65.0f * dt_for_frame;
	else                    state->display_t_velocity -= 45.0f * dt_for_frame;

	state->display_t_velocity = CLAMP(state->display_t_velocity, -10.0f, 10.0f);
	state->display_t += state->display_t_velocity * dt_for_frame;
	state->display_t  = CLAMP01(state->display_t);

	if (state->display_t > (1.0f / 255.0f)) {
		Rect outline = {.pos = r.pos, .size = {{r.size.w, (f32)ui->font.baseSize}}};
		outline      = scale_rect_centered(outline, (v2){{0.96f, 0.7f}});
		Rect filled  = outline;
		filled.size.w *= *state->progress;
		DrawRectangleRounded(filled.rl, 2.0f, 0, fade(colour_from_normalized(HOVERED_COLOUR),
		                                           state->display_t));
		DrawRectangleRoundedLinesEx(outline.rl, 2.0f, 0, 3, fade(BLACK, state->display_t));
	}

	v2 result = {{r.size.w, (f32)ui->font.baseSize}};
	return result;
}

function s8
push_compute_time(Arena *arena, s8 prefix, f32 time)
{
	Stream sb = arena_stream(*arena);
	stream_append_s8(&sb, prefix);
	stream_append_f64_e(&sb, time);
	return arena_stream_commit(arena, &sb);
}

function v2
draw_compute_stats_bar_view(BeamformerUI *ui, Arena arena, ComputeShaderStats *stats,
                            BeamformerShaderKind *stages, u32 stages_count, f32 compute_time_sum,
                            TextSpec ts, Rect r, v2 mouse)
{
	read_only local_persist s8 frame_labels[] = {s8_comp("0:"), s8_comp("-1:"), s8_comp("-2:"), s8_comp("-3:")};
	f32 total_times[countof(frame_labels)] = {0};
	Table *table = table_new(&arena, countof(frame_labels), TextAlignment_Right, TextAlignment_Left);
	for (u32 i = 0; i < countof(frame_labels); i++) {
		TableCell *cells = table_push_row(table, &arena, TRK_CELLS)->data;
		cells[0].text = frame_labels[i];
		u32 frame_index = (stats->latest_frame_index - i) % countof(stats->table.times);
		u32 seen_shaders = 0;
		for (u32 j = 0; j < stages_count; j++) {
			if ((seen_shaders & (1u << stages[j])) == 0)
				total_times[i] += stats->table.times[frame_index][stages[j]];
			seen_shaders |= (1u << stages[j]);
		}
	}

	v2 result = table_extent(table, arena, ts.font);

	f32 remaining_width = r.size.w - result.w - table->cell_pad.w;
	f32 average_width   = 0.8f * remaining_width;

	s8 mouse_text = s8("");
	v2 text_pos;

	u32 row_index = 0;
	TableIterator *it = table_iterator_new(table, TIK_ROWS, &arena, 0, r.pos, ts.font);
	for (TableRow *row = table_iterator_next(it, &arena);
	     row;
	     row = table_iterator_next(it, &arena))
	{
		Rect cr   = it->cell_rect;
		cr.size.w = table->widths[0];
		ts.limits.size.w = cr.size.w;
		draw_table_cell(ui, arena, (TableCell *)row->data, cr, table->alignment[0], ts, mouse);

		u32 frame_index = (stats->latest_frame_index - row_index) % countof(stats->table.times);
		f32 total_width = average_width * total_times[row_index] / compute_time_sum;
		Rect rect;
		rect.pos  = v2_add(cr.pos, (v2){{cr.size.w + table->cell_pad.w , cr.size.h * 0.15f}});
		rect.size = (v2){.y = 0.7f * cr.size.h};
		for (u32 i = 0; i < stages_count; i++) {
			rect.size.w = total_width * stats->table.times[frame_index][stages[i]] / total_times[row_index];
			Color color = colour_from_normalized(g_colour_palette[i % countof(g_colour_palette)]);
			DrawRectangleRec(rect.rl, color);
			if (point_in_rect(mouse, rect)) {
				text_pos   = v2_add(rect.pos, (v2){.x = table->cell_pad.w});
				s8 name    = push_s8_from_parts(&arena, s8(""), beamformer_shader_names[stages[i]], s8(": "));
				mouse_text = push_compute_time(&arena, name, stats->table.times[frame_index][stages[i]]);
			}
			rect.pos.x += rect.size.w;
		}
		row_index++;
	}

	v2 start = v2_add(r.pos, (v2){.x = table->widths[0] + average_width + table->cell_pad.w});
	v2 end   = v2_add(start, (v2){.y = result.y});
	DrawLineEx(start.rl, end.rl, 4, colour_from_normalized(FG_COLOUR));

	if (mouse_text.len) {
		ts.font = &ui->small_font;
		ts.flags &= ~(u32)TF_LIMITED;
		ts.flags |=  (u32)TF_OUTLINED;
		ts.outline_colour = (v4){.a = 1};
		ts.outline_thick  = 1;
		draw_text(mouse_text, text_pos, &ts);
	}

	return result;
}

function void
push_table_time_row(Table *table, Arena *arena, s8 label, f32 time)
{
	assert(table->columns == 3);
	TableCell *cells = table_push_row(table, arena, TRK_CELLS)->data;
	cells[0].text = push_s8_from_parts(arena, s8(""), label, s8(":"));
	cells[1].text = push_compute_time(arena, s8(""), time);
	cells[2].text = s8("[s]");
}

function void
push_table_time_row_with_fps(Table *table, Arena *arena, s8 label, f32 time)
{
	assert(table->columns == 3);
	TableCell *cells = table_push_row(table, arena, TRK_CELLS)->data;

	Stream sb = arena_stream(*arena);
	stream_append_f64_e(&sb, time);
	stream_append_s8(&sb, s8(" ("));
	stream_append_f64(&sb, time > 0 ? 1.0f / time : 0, 100);
	stream_append_s8(&sb, s8(")"));

	cells[0].text = label;
	cells[1].text = arena_stream_commit(arena, &sb);
	cells[2].text = s8("[s] (FPS)");
}

function void
push_table_memory_size_row(Table *table, Arena *arena, s8 label, u64 memory_size)
{
		TableCell *cells = table_push_row(table, arena, TRK_CELLS)->data;
		Stream sb = arena_stream(*arena);
		stream_append_u64(&sb, memory_size);
		cells[0].text = label;
		cells[1].text = arena_stream_commit(arena, &sb);
		cells[2].text = s8("[B/F]");
}

function v2
draw_compute_stats_view(BeamformerUI *ui, Arena arena, Variable *view, Rect r, v2 mouse)
{
	assert(view->type == VT_COMPUTE_STATS_VIEW);

	read_only local_persist BeamformerComputePlan dummy_plan = {0};
	u32 selected_plan = ui->selected_parameter_block % BeamformerMaxParameterBlockSlots;
	BeamformerComputePlan *cp = ui->beamformer_context->compute_context.compute_plans[selected_plan];
	if (!cp) cp = &dummy_plan;

	ComputeStatsView   *csv   = &view->compute_stats_view;
	ComputeShaderStats *stats = csv->compute_shader_stats;
	f32 compute_time_sum = 0;
	u32 stages           = cp->pipeline.shader_count;
	TextSpec text_spec   = {.font = &ui->font, .colour = FG_COLOUR, .flags = TF_LIMITED};

	ui_blinker_update(&csv->blink, BLINK_SPEED);

	static_assert(BeamformerShaderKind_ComputeCount <= 32, "shader kind bitfield test");
	u32 seen_shaders = 0;
	for (u32 i = 0; i < stages; i++) {
		BeamformerShaderKind index = cp->pipeline.shaders[i];
		if ((seen_shaders & (1u << index)) == 0)
			compute_time_sum += stats->average_times[index];
		seen_shaders |= (1u << index);
	}

	v2 result = {0};

	Table *table = table_new(&arena, 2, TextAlignment_Left, TextAlignment_Left, TextAlignment_Left);
	switch (csv->kind) {
	case ComputeStatsViewKind_Average:{
		da_reserve(&arena, table, stages);
		for (u32 i = 0; i < stages; i++) {
			push_table_time_row(table, &arena, beamformer_shader_names[cp->pipeline.shaders[i]],
			                    stats->average_times[cp->pipeline.shaders[i]]);
		}
	}break;
	case ComputeStatsViewKind_Bar:{
		result = draw_compute_stats_bar_view(ui, arena, stats, cp->pipeline.shaders, stages,
		                                     compute_time_sum, text_spec, r, mouse);
		r.pos = v2_add(r.pos, (v2){.y = result.y});
	}break;
	InvalidDefaultCase;
	}

	u32 rf_size = ui->beamformer_context->compute_context.rf_buffer.active_rf_size;
	push_table_time_row_with_fps(table, &arena, s8("Compute Total:"),   compute_time_sum);
	push_table_time_row_with_fps(table, &arena, s8("RF Upload Delta:"), stats->rf_time_delta_average);
	push_table_memory_size_row(table, &arena, s8("Input RF Size:"), rf_size);
	if (rf_size != cp->rf_size)
		push_table_memory_size_row(table, &arena, s8("DAS RF Size:"), cp->rf_size);

	result = v2_add(result, table_extent(table, arena, text_spec.font));

	u32 row_index = 0;
	u32 *programs = ui->beamformer_context->compute_context.programs;
	TableIterator *it = table_iterator_new(table, TIK_ROWS, &arena, 0, r.pos, text_spec.font);
	for (TableRow *row = table_iterator_next(it, &arena);
	     row;
	     row = table_iterator_next(it, &arena), row_index++)
	{
		Table *t = it->frame.table;
		Rect cell_rect = it->cell_rect;
		for (i32 column = 0; column < t->columns; column++) {
			TableCell *cell = (TableCell *)it->row->data + column;
			cell_rect.size.w = t->widths[column];
			text_spec.limits.size.w = r.size.w - (cell_rect.pos.x - it->start_x);

			if (column == 0 && row_index < stages && programs[cp->pipeline.shaders[row_index]] == 0 &&
			    cp->pipeline.shaders[row_index] != BeamformerShaderKind_CudaHilbert &&
			    cp->pipeline.shaders[row_index] != BeamformerShaderKind_CudaDecode)
			{
				text_spec.colour = v4_lerp(FG_COLOUR, FOCUSED_COLOUR, ease_in_out_quartic(csv->blink.t));
			} else {
				text_spec.colour = FG_COLOUR;
			}

			draw_table_cell(ui, arena, cell, cell_rect, t->alignment[column], text_spec, mouse);

			cell_rect.pos.x += cell_rect.size.w + t->cell_pad.w;
		}
	}

	return result;
}

function v2
draw_live_controls_view(BeamformerUI *ui, Variable *var, Rect r, v2 mouse, Arena arena)
{
	BeamformerSharedMemory          *sm  = ui->shared_memory.region;
	BeamformerLiveImagingParameters *lip = &sm->live_imaging_parameters;
	BeamformerLiveControlsView      *lv  = var->generic;

	TextSpec text_spec = {.font = &ui->font, .colour = FG_COLOUR, .flags = TF_LIMITED};

	v2 slider_size = {{MIN(140.0f, r.size.w), (f32)ui->font.baseSize}};
	v2 button_size = {{MIN(r.size.w, slider_size.x + (f32)ui->font.baseSize), (f32)ui->font.baseSize * 1.5f}};

	f32 text_off   = r.pos.x + 0.5f * MAX(0, (r.size.w - slider_size.w - (f32)ui->font.baseSize));
	f32 slider_off = r.pos.x + 0.5f * (r.size.w - slider_size.w);
	f32 button_off = r.pos.x + 0.5f * (r.size.w - button_size.w);

	text_spec.limits.size.w = r.size.w - (text_off - r.pos.x);

	v2 at = {{text_off, r.pos.y}};

	v4 hsv_power_slider = {{0.35f * ease_in_out_cubic(1.0f - lip->transmit_power), 0.65f, 0.65f, 1}};
	at.y += draw_text(s8("Power:"), at, &text_spec).y;
	at.x  = slider_off;
	at.y += draw_variable_slider(ui, &lv->transmit_power, (Rect){.pos = at, .size = slider_size},
	                             lip->transmit_power, hsv_to_rgb(hsv_power_slider), mouse);

	at.x  = text_off;
	at.y += draw_text(s8("TGC:"), at, &text_spec).y;
	at.x  = slider_off;
	for (u32 i = 0; i < countof(lip->tgc_control_points); i++) {
		Variable *v = lv->tgc_control_points + i;
		at.y += draw_variable_slider(ui, v, (Rect){.pos = at, .size = slider_size},
		                             lip->tgc_control_points[i], g_colour_palette[1], mouse);

		if (interaction_is_hot(ui, auto_interaction(r, v)))
			lv->hot_field_flag = BeamformerLiveImagingDirtyFlags_TGCControlPoints;
	}

	at.x  = button_off;
	at.y += (f32)ui->font.baseSize * 0.5f;
	at.y += draw_fancy_button(ui, &lv->stop_button, lv->stop_button.name,
	                          (Rect){.pos = at, .size = button_size},
	                          BORDER_COLOUR, mouse, text_spec).y;

	if (lip->save_enabled) {
		b32 active = lip->save_active;
		s8  label  = lv->save_button.cycler.labels[active % lv->save_button.cycler.cycle_length];

		f32 save_t = ui_blinker_update(&lv->save_button_blink, BLINK_SPEED);
		v4 border_colour = BORDER_COLOUR;
		if (active) border_colour = v4_lerp(BORDER_COLOUR, FOCUSED_COLOUR, ease_in_out_cubic(save_t));

		at.x  = text_off;
		at.y += draw_text(s8("File Tag:"), at, &text_spec).y;
		at.x += (f32)text_spec.font->baseSize / 2;
		text_spec.limits.size.w -= (f32)text_spec.font->baseSize;

		v4 save_text_colour = FG_COLOUR;
		if (lip->save_name_tag_length <= 0)
			save_text_colour.a = 0.6f;
		at.y += draw_variable(ui, arena, &lv->save_text, at, mouse, save_text_colour, text_spec).y;
		text_spec.limits.size.w += (f32)text_spec.font->baseSize;

		at.x  = button_off;
		at.y += (f32)ui->font.baseSize * 0.25f;
		at.y += draw_fancy_button(ui, &lv->save_button, label, (Rect){.pos = at, .size = button_size},
		                          border_colour, mouse, text_spec).y;

		if (interaction_is_hot(ui, auto_interaction(r, &lv->save_text)))
			lv->hot_field_flag = BeamformerLiveImagingDirtyFlags_SaveNameTag;
		if (interaction_is_hot(ui, auto_interaction(r, &lv->save_button)))
			lv->hot_field_flag = BeamformerLiveImagingDirtyFlags_SaveData;
	}

	if (interaction_is_hot(ui, auto_interaction(r, &lv->transmit_power)))
		lv->hot_field_flag = BeamformerLiveImagingDirtyFlags_TransmitPower;
	if (interaction_is_hot(ui, auto_interaction(r, &lv->stop_button)))
		lv->hot_field_flag = BeamformerLiveImagingDirtyFlags_StopImaging;

	v2 result = {{r.size.w, at.y - r.pos.y}};
	return result;
}

struct variable_iterator { Variable *current; };
function i32
variable_iterator_next(struct variable_iterator *it)
{
	i32 result = 0;

	if (it->current->type == VT_GROUP && it->current->group.expanded) {
		it->current = it->current->group.first;
		result++;
	} else {
		while (it->current) {
			if (it->current->next) {
				it->current = it->current->next;
				break;
			}
			it->current = it->current->parent;
			result--;
		}
	}

	return result;
}

function v2
draw_ui_view_menu(BeamformerUI *ui, Variable *group, Arena arena, Rect r, v2 mouse, TextSpec text_spec)
{
	assert(group->type == VT_GROUP);
	Table *table = table_new(&arena, 0, TextAlignment_Left, TextAlignment_Right);
	table->row_border_thick = 2.0f;
	table->cell_pad         = (v2){{16.0f, 8.0f}};

	i32 nesting = 0;
	for (struct variable_iterator it = {group->group.first};
	     it.current;
	     nesting = variable_iterator_next(&it))
	{
		(void)nesting;
		assert(nesting == 0);
		Variable *var = it.current;
		TableCell *cells = table_push_row(table, &arena, TRK_CELLS)->data;
		switch (var->type) {
		case VT_B32:
		case VT_CYCLER:
		{
			cells[0] = (TableCell){.text = var->name};
			cells[1] = table_variable_cell(&arena, var);
		}break;
		case VT_UI_BUTTON:{
			cells[0] = (TableCell){.text = var->name, .kind = TableCellKind_Variable, .var = var};
		}break;
		InvalidDefaultCase;
		}
	}

	r.size = table_extent(table, arena, text_spec.font);
	return draw_table(ui, arena, table, r, text_spec, mouse, 0);
}

function v2
draw_ui_view_listing(BeamformerUI *ui, Variable *group, Arena arena, Rect r, v2 mouse, TextSpec text_spec)
{
	assert(group->type == VT_GROUP);
	Table *table = table_new(&arena, 0, TextAlignment_Left, TextAlignment_Left, TextAlignment_Right);

	i32 nesting = 0;
	for (struct variable_iterator it = {group->group.first};
	     it.current;
	     nesting = variable_iterator_next(&it))
	{
		while (nesting > 0) {
			table = table_begin_subtable(table, &arena, TextAlignment_Left,
			                             TextAlignment_Center, TextAlignment_Right);
			nesting--;
		}
		while (nesting < 0) { table = table_end_subtable(table); nesting++; }

		Variable *var = it.current;
		switch (var->type) {
		case VT_CYCLER:
		case VT_BEAMFORMER_VARIABLE:
		{
			s8 suffix = s8("");
			if (var->type == VT_BEAMFORMER_VARIABLE)
				suffix = var->beamformer_variable.suffix;
			table_push_parameter_row(table, &arena, var->name, var, suffix);
		}break;
		case VT_GROUP:{
			VariableGroup *g = &var->group;

			TableCell *cells = table_push_row(table, &arena, TRK_CELLS)->data;
			cells[0] = (TableCell){.text = var->name, .kind = TableCellKind_Variable, .var = var};

			if (!g->expanded) {
				Stream sb = arena_stream(arena);
				stream_append_variable_group(&sb, var);
				cells[1].kind = TableCellKind_VariableGroup;
				cells[1].text = arena_stream_commit(&arena, &sb);
				cells[1].var  = var;

				Variable *v = g->first;
				assert(!v || v->type == VT_BEAMFORMER_VARIABLE);
				/* NOTE(rnp): assume the suffix is the same for all elements */
				if (v) cells[2].text = v->beamformer_variable.suffix;
			}
		}break;
		InvalidDefaultCase;
		}
	}

	v2 result = table_extent(table, arena, text_spec.font);
	draw_table(ui, arena, table, r, text_spec, mouse, 0);
	return result;
}

function Rect
draw_ui_view_container(BeamformerUI *ui, Variable *var, v2 mouse, Rect bounds)
{
	UIView *fw = &var->view;
	Rect result = fw->rect;
	if (fw->rect.size.x > 0 && fw->rect.size.y > 0) {
		f32 line_height = (f32)ui->small_font.baseSize;

		f32 pad = MAX(line_height + 5.0f, UI_REGION_PAD);
		if (fw->rect.pos.y < pad)
			fw->rect.pos.y += pad - fw->rect.pos.y;
		result = fw->rect;

		f32 delta_x = (result.pos.x + result.size.x) - (bounds.size.x + bounds.pos.x);
		if (delta_x > 0) {
			result.pos.x -= delta_x;
			result.pos.x  = MAX(0, result.pos.x);
		}

		Rect container = result;
		if (fw->close) {
			container.pos.y  -= 5 + line_height;
			container.size.y += 2 + line_height;
			Rect handle = {{container.pos, (v2){.x = container.size.w, .y = 2 + line_height}}};
			Rect close;
			hover_interaction(ui, mouse, auto_interaction(container, var));
			cut_rect_horizontal(handle, handle.size.w - handle.size.h - 6, 0, &close);
			close.size.w = close.size.h;
			DrawRectangleRounded(handle.rl, 0.1f, 0, colour_from_normalized(BG_COLOUR));
			DrawRectangleRoundedLinesEx(handle.rl, 0.2f, 0, 2, BLACK);
			draw_close_button(ui, fw->close, mouse, close, (v2){{0.45f, 0.45f}});
		} else {
			hover_interaction(ui, mouse, auto_interaction(container, var));
		}
		f32 roundness = 12.0f / fw->rect.size.y;
		DrawRectangleRounded(result.rl, roundness / 2.0f, 0, colour_from_normalized(BG_COLOUR));
		DrawRectangleRoundedLinesEx(result.rl, roundness, 0, 2, BLACK);
	}
	return result;
}

function void
draw_ui_view(BeamformerUI *ui, Variable *ui_view, Rect r, v2 mouse, TextSpec text_spec)
{
	assert(ui_view->type == VT_UI_VIEW || ui_view->type == VT_UI_MENU || ui_view->type == VT_UI_TEXT_BOX);

	UIView *view = &ui_view->view;

	if (view->flags & UIViewFlag_Floating) {
		r = draw_ui_view_container(ui, ui_view, mouse, r);
	} else {
		if (view->rect.size.h - r.size.h < view->rect.pos.h)
			view->rect.pos.h = view->rect.size.h - r.size.h;

		if (view->rect.size.h - r.size.h < 0)
			view->rect.pos.h = 0;

		r.pos.y -= view->rect.pos.h;
	}

	v2 size = {0};

	Variable *var = view->child;
	switch (var->type) {
	case VT_GROUP:{
		if (ui_view->type == VT_UI_MENU)
			size = draw_ui_view_menu(ui, var, ui->arena, r, mouse, text_spec);
		else {
			size = draw_ui_view_listing(ui, var, ui->arena, r, mouse, text_spec);
		}
	}break;
	case VT_BEAMFORMER_FRAME_VIEW: {
		BeamformerFrameView *bv = var->generic;
		if (frame_view_ready_to_present(ui, bv)) {
			if (bv->kind == BeamformerFrameViewKind_3DXPlane)
				draw_3D_xplane_frame_view(ui, ui->arena, var, r, mouse);
			else
				draw_beamformer_frame_view(ui, ui->arena, var, r, mouse);
		}
	} break;
	case VT_COMPUTE_PROGRESS_BAR: {
		size = draw_compute_progress_bar(ui, &var->compute_progress_bar, r);
	} break;
	case VT_COMPUTE_STATS_VIEW:{ size = draw_compute_stats_view(ui, ui->arena, var, r, mouse); }break;
	case VT_LIVE_CONTROLS_VIEW:{
		if (view->rect.size.h - r.size.h < 0)
			r.pos.y += 0.5f * (r.size.h - view->rect.size.h);
		BeamformerSharedMemory *sm = ui->shared_memory.region;
		if (sm->live_imaging_parameters.active)
			size = draw_live_controls_view(ui, var, r, mouse, ui->arena);
	}break;
	InvalidDefaultCase;
	}

	view->rect.size = size;
}

function void
draw_layout_variable(BeamformerUI *ui, Variable *var, Rect draw_rect, v2 mouse)
{
	if (var->type != VT_UI_REGION_SPLIT) {
		v2 shrink = {.x = UI_REGION_PAD, .y = UI_REGION_PAD};
		draw_rect = shrink_rect_centered(draw_rect, shrink);
		draw_rect.size = v2_floor(draw_rect.size);
		BeginScissorMode((i32)draw_rect.pos.x, (i32)draw_rect.pos.y, (i32)draw_rect.size.w, (i32)draw_rect.size.h);
		draw_rect = draw_title_bar(ui, ui->arena, var, draw_rect, mouse);
		EndScissorMode();
	}

	/* TODO(rnp): post order traversal of the ui tree will remove the need for this */
	if (!CheckCollisionPointRec(mouse.rl, draw_rect.rl))
		mouse = (v2){.x = F32_INFINITY, .y = F32_INFINITY};

	draw_rect.size = v2_floor(draw_rect.size);
	BeginScissorMode((i32)draw_rect.pos.x, (i32)draw_rect.pos.y, (i32)draw_rect.size.w, (i32)draw_rect.size.h);
	switch (var->type) {
	case VT_UI_VIEW: {
		hover_interaction(ui, mouse, auto_interaction(draw_rect, var));
		TextSpec text_spec = {.font = &ui->font, .colour = FG_COLOUR, .flags = TF_LIMITED};
		draw_ui_view(ui, var, draw_rect, mouse, text_spec);
	} break;
	case VT_UI_REGION_SPLIT: {
		RegionSplit *rs = &var->region_split;

		Rect split, hover;
		switch (rs->direction) {
		case RSD_VERTICAL: {
			split_rect_vertical(draw_rect, rs->fraction, 0, &split);
			split.pos.x  += UI_REGION_PAD;
			split.pos.y  -= UI_SPLIT_HANDLE_THICK / 2;
			split.size.h  = UI_SPLIT_HANDLE_THICK;
			split.size.w -= 2 * UI_REGION_PAD;
			hover = extend_rect_centered(split, (v2){.y = 0.75f * UI_REGION_PAD});
		} break;
		case RSD_HORIZONTAL: {
			split_rect_horizontal(draw_rect, rs->fraction, 0, &split);
			split.pos.x  -= UI_SPLIT_HANDLE_THICK / 2;
			split.pos.y  += UI_REGION_PAD;
			split.size.w  = UI_SPLIT_HANDLE_THICK;
			split.size.h -= 2 * UI_REGION_PAD;
			hover = extend_rect_centered(split, (v2){.x = 0.75f * UI_REGION_PAD});
		} break;
		}

		Interaction drag = {.kind = InteractionKind_Drag, .rect = hover, .var = var};
		hover_interaction(ui, mouse, drag);

		v4 colour = HOVERED_COLOUR;
		colour.a  = var->hover_t;
		DrawRectangleRounded(split.rl, 0.6f, 0, colour_from_normalized(colour));
	} break;
	InvalidDefaultCase;
	}
	EndScissorMode();
}

function void
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
			RegionSplit *rs = &top->var->region_split;
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

function void
draw_floating_widgets(BeamformerUI *ui, Rect window_rect, v2 mouse)
{
	TextSpec text_spec = {.font = &ui->small_font, .colour = FG_COLOUR};
	window_rect = shrink_rect_centered(window_rect, (v2){{UI_REGION_PAD, UI_REGION_PAD}});
	for (Variable *var = ui->floating_widget_sentinal.parent;
	     var != &ui->floating_widget_sentinal;
	     var = var->parent)
	{
		if (var->type == VT_UI_TEXT_BOX) {
			UIView *fw = &var->view;
			InputState *is = &ui->text_input_state;

			draw_ui_view_container(ui, var, mouse, fw->rect);

			f32 cursor_width = (is->cursor == is->count) ? 0.55f * (f32)is->font->baseSize : 4.0f;
			s8 text      = {.len = is->count, .data = is->buf};
			v2 text_size = measure_text(*is->font, text);

			f32 text_pad = 4.0f;
			f32 desired_width = text_pad + text_size.w + cursor_width;
			fw->rect.size = (v2){{MAX(desired_width, fw->rect.size.w), text_size.h + text_pad}};

			v2 text_position   = {{fw->rect.pos.x + text_pad / 2, fw->rect.pos.y + text_pad / 2}};
			f32 cursor_offset  = measure_text(*is->font, (s8){is->cursor, text.data}).w;
			cursor_offset     += text_position.x;

			Rect cursor;
			cursor.pos  = (v2){{cursor_offset, text_position.y}};
			cursor.size = (v2){{cursor_width,  text_size.h}};

			v4 cursor_colour = FOCUSED_COLOUR;
			cursor_colour.a  = ease_in_out_cubic(is->cursor_blink.t);
			v4 text_colour   = v4_lerp(FG_COLOUR, HOVERED_COLOUR, fw->child->hover_t);

			TextSpec input_text_spec = {.font = is->font, .colour = text_colour};
			draw_text(text, text_position, &input_text_spec);
			DrawRectanglePro(cursor.rl, (Vector2){0}, 0, colour_from_normalized(cursor_colour));
		} else {
			draw_ui_view(ui, var, window_rect, mouse, text_spec);
		}
	}
}

function void
scroll_interaction(Variable *var, f32 delta)
{
	switch (var->type) {
	case VT_B32:{ var->bool32  = !var->bool32; }break;
	case VT_F32:{ var->real32 += delta;        }break;
	case VT_I32:{ var->signed32 += (i32)delta; }break;
	case VT_SCALED_F32:{ var->scaled_real32.val += delta * var->scaled_real32.scale; }break;
	case VT_BEAMFORMER_FRAME_VIEW:{
		BeamformerFrameView *bv = var->generic;
		bv->threshold.real32 += delta;
		bv->dirty = 1;
	} break;
	case VT_BEAMFORMER_VARIABLE:{
		BeamformerVariable *bv = &var->beamformer_variable;
		f32 value  = *bv->store + delta * bv->scroll_scale;
		*bv->store = CLAMP(value, bv->limits.x, bv->limits.y);
	}break;
	case VT_CYCLER:{
		if (delta > 0) *var->cycler.state += 1;
		else           *var->cycler.state -= 1;
		*var->cycler.state %= var->cycler.cycle_length;
	}break;
	case VT_UI_VIEW:{
		var->view.rect.pos.h += UI_SCROLL_SPEED * delta;
		var->view.rect.pos.h  = MAX(0, var->view.rect.pos.h);
	}break;
	InvalidDefaultCase;
	}
}

function void
begin_text_input(InputState *is, Rect r, Variable *container, v2 mouse)
{
	assert(container->type == VT_UI_TEXT_BOX);
	Font *font = is->font = is->hot_font;
	Stream s = {.cap = countof(is->buf), .data = is->buf};
	stream_append_variable(&s, container->view.child);
	is->count = s.widx;
	is->container = container;

	is->numeric = container->view.child->type != VT_LIVE_CONTROLS_STRING;
	if (container->view.child->type == VT_LIVE_CONTROLS_STRING) {
		BeamformerLiveImagingParameters *lip = container->view.child->generic;
		if (lip->save_name_tag_length <= 0)
			is->count = 0;
	}

	/* NOTE: extra offset to help with putting a cursor at idx 0 */
	f32 text_half_char_width = 10.0f;
	f32 hover_p = CLAMP01((mouse.x - r.pos.x) / r.size.w);
	i32 i;
	f32 x_off = text_half_char_width, x_bounds = r.size.w * hover_p;
	for (i = 0; i < is->count && x_off < x_bounds; i++) {
		/* NOTE: assumes font glyphs are ordered ASCII */
		i32 idx  = is->buf[i] - 0x20;
		x_off   += (f32)font->glyphs[idx].advanceX;
		if (font->glyphs[idx].advanceX == 0)
			x_off += font->recs[idx].width;
	}
	is->cursor = i;
}

function void
end_text_input(InputState *is, Variable *var)
{
	f32 value = 0;
	if (is->numeric) value = (f32)parse_f64((s8){.len = is->count, .data = is->buf});

	switch (var->type) {
	case VT_SCALED_F32:{ var->scaled_real32.val = value; }break;
	case VT_F32:{        var->real32            = value; }break;
	case VT_BEAMFORMER_VARIABLE:{
		BeamformerVariable *bv = &var->beamformer_variable;
		*bv->store = CLAMP(value / bv->display_scale, bv->limits.x, bv->limits.y);
		var->hover_t = 0;
	}break;
	case VT_LIVE_CONTROLS_STRING:{
		BeamformerLiveImagingParameters *lip = var->generic;
		mem_copy(lip->save_name_tag, is->buf, (uz)is->count % countof(lip->save_name_tag));
		lip->save_name_tag_length = is->count % countof(lip->save_name_tag);
	}break;
	InvalidDefaultCase;
	}
}

function b32
update_text_input(InputState *is, Variable *var)
{
	assert(is->cursor != -1);

	ui_blinker_update(&is->cursor_blink, BLINK_SPEED);

	var->hover_t -= 2 * HOVER_SPEED * dt_for_frame;
	var->hover_t  = CLAMP01(var->hover_t);

	/* NOTE: handle multiple input keys on a single frame */
	for (i32 key = GetCharPressed();
	     is->count < countof(is->buf) && key > 0;
	     key = GetCharPressed())
	{
		b32 allow_key = !is->numeric || (BETWEEN(key, '0', '9') || (key == '.') ||
		                 (key == '-' && is->cursor == 0));
		if (allow_key) {
			mem_move(is->buf + is->cursor + 1,
			         is->buf + is->cursor,
			         (uz)(is->count - is->cursor));
			is->buf[is->cursor++] = (u8)key;
			is->count++;
		}
	}

	is->cursor -= (IsKeyPressed(KEY_LEFT)  || IsKeyPressedRepeat(KEY_LEFT))  && is->cursor > 0;
	is->cursor += (IsKeyPressed(KEY_RIGHT) || IsKeyPressedRepeat(KEY_RIGHT)) && is->cursor < is->count;

	if ((IsKeyPressed(KEY_BACKSPACE) || IsKeyPressedRepeat(KEY_BACKSPACE)) && is->cursor > 0) {
		is->cursor--;
		if (is->cursor < countof(is->buf) - 1) {
			mem_move(is->buf + is->cursor,
			         is->buf + is->cursor + 1,
			         (uz)(is->count - is->cursor - 1));
		}
		is->count--;
	}

	if ((IsKeyPressed(KEY_DELETE) || IsKeyPressedRepeat(KEY_DELETE)) && is->cursor < is->count) {
		mem_move(is->buf + is->cursor,
		         is->buf + is->cursor + 1,
		         (uz)(is->count - is->cursor - 1));
		is->count--;
	}

	b32 result = IsKeyPressed(KEY_ENTER);
	return result;
}

function void
scale_bar_interaction(BeamformerUI *ui, ScaleBar *sb, v2 mouse)
{
	Interaction *it = &ui->interaction;
	b32 mouse_left_pressed  = IsMouseButtonPressed(MOUSE_BUTTON_LEFT);
	b32 mouse_right_pressed = IsMouseButtonPressed(MOUSE_BUTTON_RIGHT);
	f32 mouse_wheel         = GetMouseWheelMoveV().y;

	if (mouse_left_pressed) {
		v2 world_mouse = screen_point_to_world_2d(mouse, it->rect.pos,
		                                          v2_add(it->rect.pos, it->rect.size),
		                                          (v2){{*sb->min_value, *sb->min_value}},
		                                          (v2){{*sb->max_value, *sb->max_value}});
		f32 new_coord = F32_INFINITY;
		switch (sb->direction) {
		case SB_LATERAL: new_coord = world_mouse.x; break;
		case SB_AXIAL:   new_coord = world_mouse.y; break;
		}
		if (sb->zoom_starting_coord == F32_INFINITY) {
			sb->zoom_starting_coord = new_coord;
		} else {
			f32 min = sb->zoom_starting_coord;
			f32 max = new_coord;
			if (min > max) swap(min, max);

			v2_sll *savepoint = SLLPopFreelist(ui->scale_bar_savepoint_freelist);
			if (!savepoint) savepoint = push_struct(&ui->arena, v2_sll);

			savepoint->v.x = *sb->min_value;
			savepoint->v.y = *sb->max_value;
			SLLPush(savepoint, sb->savepoint_stack);

			*sb->min_value = min;
			*sb->max_value = max;

			sb->zoom_starting_coord = F32_INFINITY;
		}
	}

	if (mouse_right_pressed) {
		v2_sll *savepoint = sb->savepoint_stack;
		if (savepoint) {
			*sb->min_value      = savepoint->v.x;
			*sb->max_value      = savepoint->v.y;
			sb->savepoint_stack = savepoint->next;
			SLLPushFreelist(savepoint, ui->scale_bar_savepoint_freelist);
		}
		sb->zoom_starting_coord = F32_INFINITY;
	}

	if (mouse_wheel != 0) {
		*sb->min_value += mouse_wheel * sb->scroll_scale.x;
		*sb->max_value += mouse_wheel * sb->scroll_scale.y;
	}
}

function void
ui_widget_bring_to_front(Variable *sentinal, Variable *widget)
{
	/* TODO(rnp): clean up the linkage so this can be a macro */
	widget->parent->next = widget->next;
	widget->next->parent = widget->parent;

	widget->parent = sentinal;
	widget->next   = sentinal->next;
	widget->next->parent = widget;
	sentinal->next = widget;
}

function void
ui_view_close(BeamformerUI *ui, Variable *view)
{
	switch (view->type) {
	case VT_UI_MENU:
	case VT_UI_TEXT_BOX:
	{
		UIView *fw = &view->view;
		if (view->type == VT_UI_MENU) {
			assert(fw->child->type == VT_GROUP);
			fw->child->group.expanded  = 0;
			fw->child->group.container = 0;
		} else {
			end_text_input(&ui->text_input_state, fw->child);
		}
		view->parent->next = view->next;
		view->next->parent = view->parent;
		if (fw->close) SLLPushFreelist(fw->close, ui->variable_freelist);
		SLLPushFreelist(view, ui->variable_freelist);
	}break;
	case VT_UI_VIEW:{
		assert(view->parent->type == VT_UI_REGION_SPLIT);
		Variable *region = view->parent;

		Variable *parent    = region->parent;
		Variable *remaining = region->region_split.left;
		if (remaining == view) remaining = region->region_split.right;

		ui_view_free(ui, view);

		assert(parent->type == VT_UI_REGION_SPLIT);
		if (parent->region_split.left == region) {
			parent->region_split.left  = remaining;
		} else {
			parent->region_split.right = remaining;
		}
		remaining->parent = parent;

		SLLPushFreelist(region, ui->variable_freelist);
	}break;
	InvalidDefaultCase;
	}
}

function void
ui_button_interaction(BeamformerUI *ui, Variable *button)
{
	assert(button->type == VT_UI_BUTTON);
	switch (button->button) {
	case UI_BID_VIEW_CLOSE:{ ui_view_close(ui, button->parent); }break;
	case UI_BID_FV_COPY_HORIZONTAL:{
		ui_copy_frame(ui, button->parent->parent, RSD_HORIZONTAL);
	}break;
	case UI_BID_FV_COPY_VERTICAL:{
		ui_copy_frame(ui, button->parent->parent, RSD_VERTICAL);
	}break;
	case UI_BID_GM_OPEN_VIEW_RIGHT:{
		ui_add_live_frame_view(ui, button->parent->parent, RSD_HORIZONTAL, BeamformerFrameViewKind_Latest);
	}break;
	case UI_BID_GM_OPEN_VIEW_BELOW:{
		ui_add_live_frame_view(ui, button->parent->parent, RSD_VERTICAL, BeamformerFrameViewKind_Latest);
	}break;
	}
}

function void
ui_begin_interact(BeamformerUI *ui, BeamformerInput *input, b32 scroll)
{
	Interaction hot = ui->hot_interaction;
	if (hot.kind != InteractionKind_None) {
		if (hot.kind == InteractionKind_Auto) {
			switch (hot.var->type) {
			case VT_NULL:{ hot.kind = InteractionKind_Nop; }break;
			case VT_B32:{ hot.kind  = InteractionKind_Set; }break;
			case VT_SCALE_BAR:{ hot.kind = InteractionKind_Set; }break;
			case VT_UI_BUTTON:{ hot.kind = InteractionKind_Button; }break;
			case VT_GROUP:{ hot.kind = InteractionKind_Set; }break;
			case VT_UI_TEXT_BOX:
			case VT_UI_MENU:
			{
				if (hot.var->type == VT_UI_MENU) {
					hot.kind = InteractionKind_Drag;
				} else {
					hot.kind = InteractionKind_Text;
					begin_text_input(&ui->text_input_state, hot.rect, hot.var, input->mouse);
				}
				ui_widget_bring_to_front(&ui->floating_widget_sentinal, hot.var);
			}break;
			case VT_UI_VIEW:{
				if (scroll) hot.kind = InteractionKind_Scroll;
				else        hot.kind = InteractionKind_Nop;
			}break;
			case VT_X_PLANE_SHIFT:{
				assert(hot.var->parent && hot.var->parent->type == VT_BEAMFORMER_FRAME_VIEW);
				BeamformerFrameView *bv = hot.var->parent->generic;
				if (IsMouseButtonDown(MOUSE_BUTTON_LEFT)) {
					XPlaneShift *xp = &hot.var->x_plane_shift;
					xp->start_point = xp->end_point = bv->hit_test_point;
					hot.kind = InteractionKind_Drag;
				} else {
					if (scroll) {
						hot.kind = InteractionKind_Scroll;
						hot.var  = &bv->threshold;
					} else {
						hot.kind = InteractionKind_Nop;
					}
				}
			}break;
			case VT_BEAMFORMER_FRAME_VIEW:{
				if (scroll) {
					hot.kind = InteractionKind_Scroll;
				} else {
					BeamformerFrameView *bv = hot.var->generic;
					switch (bv->kind) {
					case BeamformerFrameViewKind_3DXPlane:{ hot.kind = InteractionKind_Drag; }break;
					default:{
						hot.kind = InteractionKind_Nop;
						switch (++bv->ruler.state) {
						case RulerState_Start:{
							hot.kind = InteractionKind_Ruler;
							v2 r_max = v2_add(hot.rect.pos, hot.rect.size);
							v2 p = screen_point_to_world_2d(input->mouse, hot.rect.pos, r_max,
							                                XZ(bv->min_coordinate),
							                                XZ(bv->max_coordinate));
							bv->ruler.start = p;
						}break;
						case RulerState_Hold:{}break;
						default:{ bv->ruler.state = RulerState_None; }break;
						}
					}break;
					}
				}
			}break;
			case VT_CYCLER:{
				if (scroll) hot.kind = InteractionKind_Scroll;
				else        hot.kind = InteractionKind_Set;
			}break;
			case VT_BEAMFORMER_VARIABLE:
			case VT_LIVE_CONTROLS_STRING:
			case VT_F32:
			case VT_SCALED_F32:
			{
				if (scroll) {
					hot.kind = InteractionKind_Scroll;
				} else if (hot.var->flags & V_TEXT) {
					hot.kind = InteractionKind_Text;
					Variable *w = add_floating_view(ui, &ui->arena, VT_UI_TEXT_BOX,
					                                hot.rect.pos, hot.var, 0);
					w->view.rect = hot.rect;
					begin_text_input(&ui->text_input_state, hot.rect, w, input->mouse);
				} else {
					hot.kind = InteractionKind_Drag;
				}
			}break;
			InvalidDefaultCase;
			}
		}

		ui->interaction = hot;

		if (ui->interaction.var->flags & V_LIVE_CONTROL) {
			assert(ui->interaction.var->parent->type == VT_LIVE_CONTROLS_VIEW);
			BeamformerLiveControlsView *lv = ui->interaction.var->parent->generic;
			lv->active_field_flag = lv->hot_field_flag;
		}

		if (ui->interaction.var->flags & V_HIDES_CURSOR) {
			HideCursor();
			DisableCursor();
			/* wtf raylib */
			SetMousePosition((i32)input->mouse.x, (i32)input->mouse.y);
		}
	} else {
		ui->interaction.kind = InteractionKind_Nop;
	}
}

function void
ui_extra_actions(BeamformerUI *ui, Variable *var)
{
	switch (var->type) {
	case VT_CYCLER:{
		assert(var->parent && var->parent->parent && var->parent->parent->type == VT_UI_VIEW);
		Variable *view_var = var->parent->parent;
		UIView   *view     = &view_var->view;
		switch (view->child->type) {
		case VT_BEAMFORMER_FRAME_VIEW:{
			BeamformerFrameView *old = view->child->generic;
			BeamformerFrameView *new = view->child->generic = ui_beamformer_frame_view_new(ui, &ui->arena);
			BeamformerFrameViewKind last_kind = (old->kind - 1) % BeamformerFrameViewKind_Count;

			/* NOTE(rnp): log_scale gets released below before its needed */
			b32 log_scale = old->log_scale->bool32;
			ui_variable_free_group_items(ui, view->menu);

			ui_beamformer_frame_view_release_subresources(ui, old, last_kind);
			ui_beamformer_frame_view_convert(ui, &ui->arena, view->child, view->menu, old->kind, old, log_scale);
			if (new->kind == BeamformerFrameViewKind_Copy && old->frame)
				ui_beamformer_frame_view_copy_frame(ui, new, old);

			DLLRemove(old);
			SLLPushFreelist(old, ui->view_freelist);
		}break;
		InvalidDefaultCase;
		}
	}break;
	InvalidDefaultCase;
	}
}

function void
ui_live_control_update(BeamformerUI *ui, Variable *controls)
{
	assert(controls->type == VT_LIVE_CONTROLS_VIEW);
	BeamformerSharedMemory *sm = ui->shared_memory.region;
	BeamformerLiveControlsView *lv = controls->generic;
	atomic_or_u32(&sm->live_imaging_dirty_flags, lv->active_field_flag);
}

function void
ui_end_interact(BeamformerUI *ui, v2 mouse)
{
	Interaction *it = &ui->interaction;
	Variable *parent = it->var->parent;
	u32 flags = it->var->flags;

	switch (it->kind) {
	case InteractionKind_Nop:{}break;
	case InteractionKind_Drag:{
		switch (it->var->type) {
		case VT_X_PLANE_SHIFT:{
			assert(parent && parent->type == VT_BEAMFORMER_FRAME_VIEW);
			XPlaneShift *xp = &it->var->x_plane_shift;
			BeamformerFrameView *view = parent->generic;
			BeamformerViewPlaneTag plane = view_plane_tag_from_x_plane_shift(view, it->var);
			f32 rotation  = x_plane_rotation_for_view_plane(view, plane);
			m4 x_rotation = m4_rotation_about_y(rotation);
			v3 Z = x_rotation.c[2].xyz;
			f32 delta = v3_dot(Z, v3_sub(xp->end_point, xp->start_point));
			xp->start_point = xp->end_point;

			BeamformerSharedMemory          *sm = ui->shared_memory.region;
			BeamformerLiveImagingParameters *li = &sm->live_imaging_parameters;
			li->image_plane_offsets[plane] += delta;
			atomic_or_u32(&sm->live_imaging_dirty_flags, BeamformerLiveImagingDirtyFlags_ImagePlaneOffsets);
		}break;
		default:{}break;
		}
	}break;
	case InteractionKind_Set:{
		switch (it->var->type) {
		case VT_B32:{ it->var->bool32 = !it->var->bool32; }break;
		case VT_GROUP:{ it->var->group.expanded = !it->var->group.expanded; }break;
		case VT_SCALE_BAR:{ scale_bar_interaction(ui, &it->var->scale_bar, mouse); }break;
		case VT_CYCLER:{
			b32 right = IsMouseButtonPressed(MOUSE_BUTTON_RIGHT);
			if (right) *it->var->cycler.state -= 1;
			else       *it->var->cycler.state += 1;
			*it->var->cycler.state %= it->var->cycler.cycle_length;
		}break;
		InvalidDefaultCase;
		}
	}break;
	case InteractionKind_Menu:{
		assert(it->var->type == VT_GROUP);
		VariableGroup *g = &it->var->group;
		if (g->container) {
			ui_widget_bring_to_front(&ui->floating_widget_sentinal, g->container);
		} else {
			g->container = add_floating_view(ui, &ui->arena, VT_UI_MENU, mouse, it->var, 1);
		}
	}break;
	case InteractionKind_Ruler:{
		assert(it->var->type == VT_BEAMFORMER_FRAME_VIEW);
		((BeamformerFrameView *)it->var->generic)->ruler.state = RulerState_None;
	}break;
	case InteractionKind_Button:{ ui_button_interaction(ui, it->var); }break;
	case InteractionKind_Scroll:{ scroll_interaction(it->var, GetMouseWheelMoveV().y); }break;
	case InteractionKind_Text:{ ui_view_close(ui, ui->text_input_state.container); }break;
	InvalidDefaultCase;
	}

	if (flags & V_CAUSES_COMPUTE)
		ui->flush_params = 1;

	if (flags & V_UPDATE_VIEW) {
		BeamformerFrameView *frame = parent->generic;
		/* TODO(rnp): more straight forward way of achieving this */
		if (parent->type != VT_BEAMFORMER_FRAME_VIEW) {
			assert(parent->parent->type == VT_UI_VIEW);
			assert(parent->parent->view.child->type == VT_BEAMFORMER_FRAME_VIEW);
			frame = parent->parent->view.child->generic;
		}
		frame->dirty = 1;
	}

	if (flags & V_LIVE_CONTROL)
		ui_live_control_update(ui, it->var->parent);

	if (flags & V_HIDES_CURSOR)
		EnableCursor();

	if (flags & V_EXTRA_ACTION)
		ui_extra_actions(ui, it->var);

	ui->interaction = (Interaction){.kind = InteractionKind_None};
}

function void
ui_sticky_interaction_check_end(BeamformerUI *ui, v2 mouse)
{
	Interaction *it = &ui->interaction;
	switch (it->kind) {
	case InteractionKind_Ruler:{
		if (IsMouseButtonPressed(MOUSE_BUTTON_RIGHT) || !point_in_rect(mouse, it->rect))
			ui_end_interact(ui, mouse);
	}break;
	case InteractionKind_Text:{
		Interaction text_box = auto_interaction({{0}}, ui->text_input_state.container);
		if (!interactions_equal(text_box, ui->hot_interaction))
			ui_end_interact(ui, mouse);
	}break;
	InvalidDefaultCase;
	}
}

function void
ui_interact(BeamformerUI *ui, BeamformerInput *input, Rect window_rect)
{
	Interaction *it = &ui->interaction;
	if (it->kind == InteractionKind_None || interaction_is_sticky(*it)) {
		ui->hot_interaction = ui->next_interaction;

		b32 mouse_left_pressed  = IsMouseButtonPressed(MOUSE_BUTTON_LEFT);
		b32 mouse_right_pressed = IsMouseButtonPressed(MOUSE_BUTTON_RIGHT);
		b32 wheel_moved         = GetMouseWheelMoveV().y != 0;
		if (mouse_right_pressed || mouse_left_pressed || wheel_moved) {
			if (it->kind != InteractionKind_None)
				ui_sticky_interaction_check_end(ui, input->mouse);
			ui_begin_interact(ui, input, wheel_moved);
		}
	}

	switch (it->kind) {
	case InteractionKind_Nop:{ it->kind = InteractionKind_None; }break;
	case InteractionKind_None:{}break;
	case InteractionKind_Text:{
		if (update_text_input(&ui->text_input_state, it->var))
			ui_end_interact(ui, input->mouse);
	}break;
	case InteractionKind_Ruler:{
		assert(it->var->type == VT_BEAMFORMER_FRAME_VIEW);
		BeamformerFrameView *bv = it->var->generic;
		v2 r_max = v2_add(it->rect.pos, it->rect.size);
		v2 mouse = clamp_v2_rect(input->mouse, it->rect);
		bv->ruler.end = screen_point_to_world_2d(mouse, it->rect.pos, r_max,
		                                         XZ(bv->min_coordinate),
		                                         XZ(bv->max_coordinate));
	}break;
	case InteractionKind_Drag:{
		if (!IsMouseButtonDown(MOUSE_BUTTON_LEFT) && !IsMouseButtonDown(MOUSE_BUTTON_RIGHT)) {
			ui_end_interact(ui, input->mouse);
		} else {
			v2 ws     = window_rect.size;
			v2 dMouse = v2_sub(input->mouse, input->last_mouse);

			switch (it->var->type) {
			case VT_BEAMFORMER_VARIABLE:{
				BeamformerVariable *bv = &it->var->beamformer_variable;
				/* TODO(rnp): vertical sliders? */
				f32 mouse_frac = CLAMP01((input->mouse.x - it->rect.pos.x) / it->rect.size.w);
				*bv->store     = bv->limits.x + mouse_frac * (bv->limits.y - bv->limits.x);
			}break;
			case VT_X_PLANE_SHIFT:{
				assert(it->var->parent && it->var->parent->type == VT_BEAMFORMER_FRAME_VIEW);
				v2 mouse = clamp_v2_rect(input->mouse, it->rect);
				XPlaneShift *xp = &it->var->x_plane_shift;
				ray mouse_ray = ray_for_x_plane_view(ui, it->var->parent->generic,
				                                     normalized_p_in_rect(it->rect, mouse, 0));
				/* NOTE(rnp): project start point onto ray */
				v3 s = v3_sub(xp->start_point, mouse_ray.origin);
				v3 r = v3_sub(mouse_ray.direction, mouse_ray.origin);
				f32 scale     = v3_dot(s, r) / v3_magnitude_squared(r);
				xp->end_point = v3_add(mouse_ray.origin, v3_scale(r, scale));
			}break;
			case VT_BEAMFORMER_FRAME_VIEW:{
				BeamformerFrameView *bv = it->var->generic;
				switch (bv->kind) {
				case BeamformerFrameViewKind_3DXPlane:{
					bv->rotation += dMouse.x / ws.w;
					if (bv->rotation > 1.0f) bv->rotation -= 1.0f;
					if (bv->rotation < 0.0f) bv->rotation += 1.0f;
				}break;
				InvalidDefaultCase;
				}
			}break;
			case VT_UI_MENU:{
				v2 *pos = &ui->interaction.var->view.rect.pos;
				*pos = clamp_v2_rect(v2_add(*pos, dMouse), window_rect);
			}break;
			case VT_UI_REGION_SPLIT:{
				f32 min_fraction = 0;
				dMouse = v2_mul(dMouse, (v2){{1.0f / ws.w, 1.0f / ws.h}});
				RegionSplit *rs = &ui->interaction.var->region_split;
				switch (rs->direction) {
				case RSD_VERTICAL: {
					min_fraction  = (UI_SPLIT_HANDLE_THICK + 0.5f * UI_REGION_PAD) / ws.h;
					rs->fraction += dMouse.y;
				} break;
				case RSD_HORIZONTAL: {
					min_fraction  = (UI_SPLIT_HANDLE_THICK + 0.5f * UI_REGION_PAD) / ws.w;
					rs->fraction += dMouse.x;
				} break;
				}
				rs->fraction = CLAMP(rs->fraction, min_fraction, 1 - min_fraction);
			}break;
			default:{}break;
			}
			if (it->var->flags & V_LIVE_CONTROL)
				ui_live_control_update(ui, it->var->parent);
		}
	} break;
	default:{ ui_end_interact(ui, input->mouse); }break;
	}

	ui->next_interaction = (Interaction){.kind = InteractionKind_None};
}

/* NOTE(rnp): this only exists to make asan less annoying. do not waste
 * people's time by freeing, closing, etc... */
DEBUG_EXPORT BEAMFORMER_DEBUG_UI_DEINIT_FN(beamformer_debug_ui_deinit)
{
#if ASAN_ACTIVE
	BeamformerUI *ui = ctx->ui;
	UnloadFont(ui->font);
	UnloadFont(ui->small_font);
	CloseWindow();
#endif
}

function void
ui_init(BeamformerCtx *ctx, Arena store)
{
	BeamformerUI *ui = ctx->ui;
	if (!ui) {
		ui = ctx->ui = push_struct(&store, typeof(*ui));
		ui->arena = store;
		ui->frame_view_render_context = &ctx->frame_view_render_context;
		ui->unit_cube_model = ctx->compute_context.unit_cube_model;
		ui->shared_memory   = ctx->shared_memory;
		ui->beamformer_context = ctx;

		/* TODO(rnp): build these into the binary */
		/* TODO(rnp): better font, this one is jank at small sizes */
		ui->font       = LoadFontEx("assets/IBMPlexSans-Bold.ttf", 28, 0, 0);
		ui->small_font = LoadFontEx("assets/IBMPlexSans-Bold.ttf", 20, 0, 0);

		ui->floating_widget_sentinal.parent = &ui->floating_widget_sentinal;
		ui->floating_widget_sentinal.next   = &ui->floating_widget_sentinal;

		Variable *split = ui->regions = add_ui_split(ui, 0, &ui->arena, s8("UI Root"), 0.36f,
		                                             RSD_HORIZONTAL, ui->font);
		split->region_split.left = add_ui_split(ui, split, &ui->arena, s8(""), 0.475f,
		                                        RSD_VERTICAL, ui->font);

		split = split->region_split.right = add_ui_split(ui, split, &ui->arena, s8(""), 0.70f,
		                                                 RSD_HORIZONTAL, ui->font);
		{
			split->region_split.left  = add_beamformer_frame_view(ui, split, &ui->arena,
			                                                      BeamformerFrameViewKind_Latest, 0, 0);
			split->region_split.right = add_live_controls_view(ui, split, &ui->arena);
		}
		split = split->parent;

		split = split->region_split.left;
		split->region_split.left  = add_beamformer_parameters_view(split, ctx);
		split->region_split.right = add_ui_split(ui, split, &ui->arena, s8(""), 0.22f,
		                                         RSD_VERTICAL, ui->font);
		split = split->region_split.right;

		split->region_split.left  = add_compute_progress_bar(split, ctx);
		split->region_split.right = add_compute_stats_view(ui, split, &ui->arena, ctx);

		/* NOTE(rnp): shrink variable size once this fires */
		assert((uz)(ui->arena.beg - (u8 *)ui) < KB(64));
	}
}

function void
validate_ui_parameters(BeamformerUIParameters *p)
{
	if (p->output_min_coordinate[0] > p->output_max_coordinate[0])
		swap(p->output_min_coordinate[0], p->output_max_coordinate[0]);
	if (p->output_min_coordinate[2] > p->output_max_coordinate[2])
		swap(p->output_min_coordinate[2], p->output_max_coordinate[2]);
}

function void
draw_ui(BeamformerCtx *ctx, BeamformerInput *input, BeamformerFrame *frame_to_draw, BeamformerViewPlaneTag frame_plane)
{
	BeamformerUI *ui = ctx->ui;
	BeamformerSharedMemory *sm = ctx->shared_memory.region;

	ui->latest_plane[BeamformerViewPlaneTag_Count] = frame_to_draw;
	ui->latest_plane[frame_plane]                  = frame_to_draw;

	asan_poison_region(ui->arena.beg, ui->arena.end - ui->arena.beg);

	u32 selected_block = ui->selected_parameter_block % BeamformerMaxParameterBlockSlots;
	u32 selected_mask  = 1 << selected_block;
	if (ctx->ui_dirty_parameter_blocks & selected_mask) {
		BeamformerParameterBlock *pb = beamformer_parameter_block_lock(&ctx->shared_memory, selected_block, 0);
		if (pb) {
			mem_copy(&ui->params, &pb->parameters_ui, sizeof(ui->params));
			ui->flush_params = 0;
			atomic_and_u32(&ctx->ui_dirty_parameter_blocks, ~selected_mask);
			beamformer_parameter_block_unlock(&ctx->shared_memory, selected_block);
		}
	}

	/* NOTE: process interactions first because the user interacted with
	 * the ui that was presented last frame */
	Rect window_rect = {.size = {{(f32)ctx->window_size.w, (f32)ctx->window_size.h}}};
	ui_interact(ui, input, window_rect);

	if (ui->flush_params) {
		validate_ui_parameters(&ui->params);
		if (ctx->latest_frame) {
			BeamformerParameterBlock *pb = beamformer_parameter_block_lock(&ctx->shared_memory, selected_block, 0);
			if (pb) {
				ui->flush_params = 0;
				mem_copy(&pb->parameters_ui, &ui->params, sizeof(ui->params));
				mark_parameter_block_region_dirty(ctx->shared_memory.region, selected_block,
				                                  BeamformerParameterBlockRegion_Parameters);
				beamformer_parameter_block_unlock(&ctx->shared_memory, selected_block);

				BeamformerSharedMemoryLockKind dispatch_lock = BeamformerSharedMemoryLockKind_DispatchCompute;
				if (!sm->live_imaging_parameters.active &&
				    os_shared_memory_region_lock(&ctx->shared_memory, sm->locks, (i32)dispatch_lock, 0))
				{
					BeamformWork *work = beamform_work_queue_push(ctx->beamform_work_queue);
					BeamformerViewPlaneTag tag = frame_to_draw ? frame_to_draw->view_plane_tag : 0;
					if (fill_frame_compute_work(ctx, work, tag, selected_block, 0))
						beamform_work_queue_push_commit(ctx->beamform_work_queue);
				}
				os_wake_waiters(&ctx->os.compute_worker.sync_variable);
			}
		}
	}

	/* NOTE(rnp): can't render to a different framebuffer in the middle of BeginDrawing()... */
	update_frame_views(ui, window_rect);

	BeginDrawing();
		glClearNamedFramebufferfv(0, GL_COLOR, 0, BG_COLOUR.E);
		glClearNamedFramebufferfv(0, GL_DEPTH, 0, (f32 []){1});

		draw_ui_regions(ui, window_rect, input->mouse);
		draw_floating_widgets(ui, window_rect, input->mouse);
	EndDrawing();
}
