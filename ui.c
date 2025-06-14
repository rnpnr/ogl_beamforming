/* See LICENSE for license details. */
/* TODO(rnp):
 * [ ]: refactor: ui should be in its own thread and that thread should only be concerned with the ui
 * [ ]: refactor: ui shouldn't fully destroy itself on hot reload
 * [ ]: refactor: remove all the excessive measure_texts (cell drawing, hover_interaction in params table)
 * [ ]: refactor: move remaining fragment shader stuff into ui
 * [ ]: refactor: scale table to rect
 * [ ]: scroll bar for views that don't have enough space
 * [ ]: compute times through same path as parameter list ?
 * [ ]: allow views to collapse to just their title bar
 *      - title bar struct with expanded. Check when pushing onto draw stack; if expanded
 *        do normal behaviour else make size title bar size and ignore the splits fraction.
 * [ ]: enforce a minimum region size or allow regions themselves to scroll
 * [ ]: refactor: add_variable_no_link()
 * [ ]: refactor: draw_text_limited should clamp to rect and measure text itself
 * [ ]: refactor: draw_active_menu should just use draw_variable_list
 * [ ]: ui leaks split beamform views on hot-reload
 * [ ]: add tag based selection to frame views
 * [ ]: draw the ui with a post-order traversal instead of pre-order traversal
 * [ ]: consider V_HOVER_GROUP and use that to implement submenus
 * [ ]: menu's need to support nested groups
 * [ ]: don't redraw on every refresh; instead redraw on mouse movement/event or when a new frame
 *      arrives. For animations the ui can have a list of "timers" which while active will
 *      do a redraw on every refresh until completed.
 * [ ]: show full non-truncated string on hover
 * [ ]: refactor: hovered element type and show hovered element in full even when truncated
 * [ ]: visual indicator for broken shader stage gh#27
 * [ ]: bug: cross-plane view with different dimensions for each plane
 */

#define BG_COLOUR              (v4){.r = 0.15, .g = 0.12, .b = 0.13, .a = 1.0}
#define FG_COLOUR              (v4){.r = 0.92, .g = 0.88, .b = 0.78, .a = 1.0}
#define FOCUSED_COLOUR         (v4){.r = 0.86, .g = 0.28, .b = 0.21, .a = 1.0}
#define HOVERED_COLOUR         (v4){.r = 0.11, .g = 0.50, .b = 0.59, .a = 1.0}
#define RULER_COLOUR           (v4){.r = 1.00, .g = 0.70, .b = 0.00, .a = 1.0}

#define MENU_PLUS_COLOUR       (v4){.r = 0.33, .g = 0.42, .b = 1.00, .a = 1.0}
#define MENU_CLOSE_COLOUR      FOCUSED_COLOUR

#define HOVER_SPEED            5.0f

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

typedef struct BeamformerUI BeamformerUI;
typedef struct Variable     Variable;

typedef struct {
	u8   buf[64];
	i32  count;
	i32  cursor;
	f32  cursor_blink_t;
	f32  cursor_blink_scale;
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
	VT_U32,
	VT_GROUP,
	VT_CYCLER,
	VT_SCALED_F32,
	VT_BEAMFORMER_VARIABLE,
	VT_BEAMFORMER_FRAME_VIEW,
	VT_COMPUTE_STATS_VIEW,
	VT_COMPUTE_LATEST_STATS_VIEW,
	VT_COMPUTE_PROGRESS_BAR,
	VT_SCALE_BAR,
	VT_TEXT_BOX,
	VT_UI_BUTTON,
	VT_UI_FLOATING_WIDGET,
	VT_UI_REGION_SPLIT,
	VT_UI_VIEW,
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
	VariableGroupType type;
	Variable *container;
} VariableGroup;

typedef enum {
	UI_VIEW_CUSTOM_TEXT = 1 << 0,
} UIViewFlags;

typedef struct {
	Variable    *child;
	Variable    *close;
	Variable    *menu;
	f32          needed_height;
	f32          offset;
	UIViewFlags  flags;
} UIView;

/* X(id, text) */
#define FRAME_VIEW_BUTTONS \
	X(FV_COPY_HORIZONTAL, "Copy Horizontal") \
	X(FV_COPY_VERTICAL,   "Copy Vertical")   \
	X(FV_EXPORT,          "Export Bitmap")

#define GLOBAL_MENU_BUTTONS \
	X(GM_OPEN_LIVE_VIEW_RIGHT, "Open Live View Right") \
	X(GM_OPEN_LIVE_VIEW_BELOW, "Open Live View Below")

#define X(id, text) UI_BID_ ##id,
typedef enum {
	UI_BID_CLOSE_VIEW,
	UI_BID_CLOSE_WIDGET,
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
	void         *store;
	VariableType  store_type;
} BeamformerVariable;

typedef enum {
	FloatingWidgetKind_Menu,
	FloatingWidgetKind_TextBox,
} FloatingWidgetKind;

/* TODO(rnp): collapse with UIView? */
typedef struct {
	Variable *reference;
	Variable *close;
	Rect rect;
	FloatingWidgetKind kind;
} FloatingWidget;

typedef enum {
	V_INPUT          = 1 << 0,
	V_TEXT           = 1 << 1,
	V_RADIO_BUTTON   = 1 << 2,
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
		FloatingWidget      floating_widget;
		RegionSplit         region_split;
		ScaleBar            scale_bar;
		UIButtonID          button;
		UIView              view;
		VariableCycler      cycler;
		VariableGroup       group;
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

typedef enum {
	FVT_LATEST,
	FVT_INDEXED,
	FVT_COPY,
} BeamformerFrameViewType;

typedef struct BeamformerFrameView {
	Variable lateral_scale_bar;
	Variable axial_scale_bar;

	/* NOTE(rnp): these are pointers because they are added to the menu and will
	 * be put onto the freelist if the view is closed */
	Variable *lateral_scale_bar_active;
	Variable *axial_scale_bar_active;
	Variable *log_scale;
	/* NOTE(rnp): if type is LATEST  selects which type of latest to use
	 *            if type is INDEXED selects the index */
	Variable *cycler;
	u32 cycler_state;

	v4 min_coordinate;
	v4 max_coordinate;

	Ruler ruler;

	Variable threshold;
	Variable dynamic_range;
	Variable gamma;

	FrameViewRenderContext *ctx;
	BeamformFrame          *frame;
	struct BeamformerFrameView *prev, *next;

	uv2 texture_dim;
	u32 texture_mipmaps;
	u32 texture;

	BeamformerFrameViewType type;
	b32 needs_update;
} BeamformerFrameView;

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
	BeamformFrame       *frame_freelist;

	Interaction interaction;
	Interaction hot_interaction;
	Interaction next_interaction;

	InputState  text_input_state;

	v2_sll *scale_bar_savepoint_freelist;

	BeamformFrame      *latest_plane[IPT_LAST + 1];
	ComputeShaderStats *latest_compute_stats;

	BeamformerUIParameters params;
	b32                    flush_params;

	FrameViewRenderContext *frame_view_render_context;
	OS *os;
};

typedef enum {
	TF_NONE     = 0,
	TF_ROTATED  = 1 << 0,
	TF_LIMITED  = 1 << 1,
	TF_OUTLINED = 1 << 2,
} TextFlags;

typedef enum {
	TA_CENTER,
	TA_LEFT,
	TA_RIGHT,
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
	TCK_NONE,
	TCK_GENERIC,
	TCK_INTEGER,
	TCK_VARIABLE,
	TCK_VARIABLE_GROUP,
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

function v2
measure_glyph(Font font, u32 glyph)
{
	ASSERT(glyph >= 0x20);
	v2 result = {.y = font.baseSize};
	/* NOTE: assumes font glyphs are ordered ASCII */
	result.x = font.glyphs[glyph - 0x20].advanceX;
	if (result.x == 0)
		result.x = (font.recs[glyph - 0x20].width + font.glyphs[glyph - 0x20].offsetX);
	return result;
}

function v2
measure_text(Font font, s8 text)
{
	v2 result = {.y = font.baseSize};
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

function Texture
make_raylib_texture(BeamformerFrameView *v)
{
	Texture result;
	result.id      = v->texture;
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
		switch (bv->store_type) {
		case VT_F32:{ stream_append_f64(s, *(f32 *)bv->store * bv->display_scale, 100); }break;
		InvalidDefaultCase;
		}
	}break;
	case VT_CYCLER:{
		u32 index = *var->cycler.state;
		if (var->cycler.labels) stream_append_s8(s, var->cycler.labels[index]);
		else                    stream_append_u64(s, index);
	}break;
	InvalidDefaultCase;
	}
}

function Table *
table_new(Arena *a, i32 initial_capacity, i32 columns, TextAlignment *alignment)
{
	Table *result = push_struct(a, Table);
	da_reserve(a, result, initial_capacity);
	result->columns   = columns;
	result->alignment = push_array(a, TextAlignment, columns);
	result->widths    = push_array(a, f32, columns);
	mem_copy(result->alignment, alignment, sizeof(*alignment) * columns);
	return result;
}

function i32
table_skip_rows(Table *t, f32 draw_height, f32 text_height)
{
	i32 max_rows = draw_height / (text_height + TABLE_CELL_PAD_HEIGHT);
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
	result->cell_rect.size.h = font->baseSize + TABLE_CELL_PAD_HEIGHT;
	result->cell_rect.pos    = add_v2(at, (v2){.y = (starting_row - 1) * result->cell_rect.size.h});
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
		it->row    = result;
		it->column = 0;
		it->cell_rect.pos.x  = it->start_x;
		it->cell_rect.pos.y += it->cell_rect.size.h + it->frame.table->row_border_thick;
	}

	if (it->row && it->kind == TIK_CELLS) {
		i32 column = it->column++;
		it->cell_rect.pos.x  += column > 0 ? it->cell_rect.size.w : 0;
		it->cell_rect.size.w  = it->frame.table->widths[column];
		it->alignment         = it->frame.table->alignment[column];
		result                = (TableCell *)it->row->data + column;

		if (it->column == it->frame.table->columns)
			it->row = 0;
	}

	return result;
}

function f32
table_width(Table *t)
{
	f32 result = 0;
	for (i32 i = 0; i < t->columns; i++)
		result += t->widths[i];
	return result;
}

function v2
table_extent(Table *t, Arena arena, Font *font)
{
	TableIterator *it = table_iterator_new(t, TIK_ROWS, &arena, 0, (v2){0}, font);
	f32 max_row_width = 0;
	for (TableRow *row = table_iterator_next(it, &arena);
	     row;
	     row = table_iterator_next(it, &arena))
	{
		i32 columns   = it->frame.table->columns;
		f32 row_width = 0;
		for (i32 i = 0; i < columns; i++) {
			TableCell *cell = (TableCell *)row->data + i;
			if (!cell->text.len && cell->var && cell->var->flags & V_RADIO_BUTTON) {
				cell->width = 3 * font->baseSize;
			} else {
				cell->width = measure_text(*font, cell->text).w;
			}
			cell->width += TABLE_CELL_PAD_WIDTH;
			row_width   += cell->width;
			it->frame.table->widths[i] = MAX(cell->width, it->frame.table->widths[i]);
		}
		row_width     += (columns - 1) * it->frame.table->column_border_thick;
		max_row_width  = MAX(row_width, max_row_width);
	}
	v2 result = {.x = max_row_width, .y = it->cell_rect.pos.y};
	return result;
}

function v2
table_cell_align(TableCell *cell, TextAlignment align, Rect r)
{
	v2 result = r.pos;
	if (r.size.w >= cell->width) {
		switch (align) {
		case TA_LEFT:  result.x += TABLE_CELL_PAD_WIDTH / 2; break;
		case TA_RIGHT: result.x += r.size.w  - cell->width;  break;
		case TA_CENTER: {
			result.x += (r.size.w - cell->width + TABLE_CELL_PAD_WIDTH) / 2;
		} break;
		}
	}
	result.y += TABLE_CELL_PAD_HEIGHT / 2;
	return result;
}

function TableCell
table_variable_cell(Arena *a, Variable *var)
{
	TableCell result = {.var = var, .kind = TCK_VARIABLE};
	Stream text = arena_stream(*a);
	stream_append_variable(&text, var);
	result.text = arena_stream_commit(a, &text);
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

function Table *
table_begin_subtable(Table *table, Arena *a, i32 columns, TextAlignment *alignment)
{
	TableRow *row = table_push_row(table, a, TRK_TABLE);
	Table *result = row->data = table_new(a, 0, columns, alignment);
	result->parent = table;
	return result;
}

function Table *
table_end_subtable(Table *table)
{
	Table *result = table->parent ? table->parent : table;
	return result;
}

function s8
push_das_shader_kind(Stream *s, DASShaderKind shader, u32 transmit_count)
{
	#define X(type, id, pretty, fixed_tx) s8_comp(pretty),
	read_only local_persist s8 pretty_names[DASShaderKind_Count + 1] = {DAS_TYPES s8_comp("Invalid")};
	#undef X
	#define X(type, id, pretty, fixed_tx) fixed_tx,
	read_only local_persist u8 fixed_transmits[DASShaderKind_Count + 1] = {DAS_TYPES 0};
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
	case VT_COMPUTE_STATS_VIEW:
	case VT_COMPUTE_LATEST_STATS_VIEW: {
		stream_append_s8(s, s8("Compute Stats"));
		if (var->type == VT_COMPUTE_LATEST_STATS_VIEW)
			stream_append_s8(s, s8(": Live"));
	} break;
	case VT_COMPUTE_PROGRESS_BAR: {
		stream_append_s8(s, s8("Compute Progress: "));
		stream_append_f64(s, 100 * *var->compute_progress_bar.progress, 100);
		stream_append_byte(s, '%');
	} break;
	case VT_BEAMFORMER_FRAME_VIEW: {
		BeamformerFrameView *bv = var->generic;
		stream_append_s8(s, s8("Frame View"));
		switch (bv->type) {
		case FVT_COPY: stream_append_s8(s, s8(": Copy [")); break;
		case FVT_LATEST: {
			#define X(plane, id, pretty) s8_comp(": " pretty " ["),
			read_only local_persist s8 labels[IPT_LAST + 1] = {IMAGE_PLANE_TAGS s8_comp(": Live [")};
			#undef X
			stream_append_s8(s, labels[*bv->cycler->cycler.state % (IPT_LAST + 1)]);
		} break;
		case FVT_INDEXED: {
			stream_append_s8(s, s8(": Index {"));
			stream_append_u64(s, *bv->cycler->cycler.state % MAX_BEAMFORMED_SAVED_FRAMES);
			stream_append_s8(s, s8("} ["));
		} break;
		}
		stream_append_hex_u64(s, bv->frame? bv->frame->id : 0);
		stream_append_byte(s, ']');
	} break;
	default: INVALID_CODE_PATH;
	}
	return stream_to_s8(s);
}

function void
resize_frame_view(BeamformerFrameView *view, uv2 dim)
{
	glDeleteTextures(1, &view->texture);
	glCreateTextures(GL_TEXTURE_2D, 1, &view->texture);

	view->texture_dim     = dim;
	view->texture_mipmaps = ctz_u32(MAX(dim.x, dim.y)) + 1;
	/* TODO(rnp): HDR? */
	glTextureStorage2D(view->texture, view->texture_mipmaps, GL_RGBA8, dim.x, dim.y);
	glGenerateTextureMipmap(view->texture);

	/* NOTE(rnp): work around raylib's janky texture sampling */
	glTextureParameteri(view->texture, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
	glTextureParameteri(view->texture, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
	glTextureParameterfv(view->texture, GL_TEXTURE_BORDER_COLOR, (f32 []){0, 0, 0, 1});
	glTextureParameteri(view->texture, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTextureParameteri(view->texture, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

	/* TODO(rnp): add some ID for the specific view here */
	LABEL_GL_OBJECT(GL_TEXTURE, view->texture, s8("Frame View Texture"));
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
					if (bv->type == FVT_COPY) {
						glDeleteTextures(1, &bv->frame->texture);
						bv->frame->texture = 0;
						SLLPush(bv->frame, ui->frame_freelist);
					}
					if (bv->axial_scale_bar.scale_bar.savepoint_stack)
						SLLPush(bv->axial_scale_bar.scale_bar.savepoint_stack,
						        ui->scale_bar_savepoint_freelist);
					if (bv->lateral_scale_bar.scale_bar.savepoint_stack)
						SLLPush(bv->lateral_scale_bar.scale_bar.savepoint_stack,
						        ui->scale_bar_savepoint_freelist);
					DLLRemove(bv);
					/* TODO(rnp): hack; use a sentinal */
					if (bv == ui->views)
						ui->views = bv->next;
					SLLPush(bv, ui->view_freelist);
				}

				Variable *next = var->next;
				SLLPush(var, ui->variable_freelist);
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
	Variable *result = SLLPop(ui->variable_freelist);
	if (result) zero_struct(result);
	else        result = push_struct(arena, Variable);
	return fill_variable(result, group, name, flags, type, font);
}

function Variable *
add_variable_group(BeamformerUI *ui, Variable *group, Arena *arena, s8 name, VariableGroupType type, Font font)
{
	Variable *result   = add_variable(ui, group, arena, name, V_INPUT, VT_GROUP, font);
	result->group.type = type;
	return result;
}

function Variable *
end_variable_group(Variable *group)
{
	ASSERT(group->type == VT_GROUP);
	return group->parent;
}

function Variable *
add_variable_cycler(BeamformerUI *ui, Variable *group, Arena *arena, u32 flags, Font font, s8 name,
                    u32 *store, s8 *labels, u32 cycle_count)
{
	Variable *result = add_variable(ui, group, arena, name, V_INPUT|flags, VT_CYCLER, font);
	result->cycler.cycle_length = cycle_count;
	result->cycler.state        = store;
	result->cycler.labels       = labels;
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
add_floating_widget(BeamformerUI *ui, Arena *arena, FloatingWidgetKind kind, v2 at,
                    Variable *reference, b32 closable)
{
	Variable *result = add_variable(ui, 0, arena, s8(""), 0, VT_UI_FLOATING_WIDGET, ui->small_font);
	result->floating_widget.rect.pos  = at;
	result->floating_widget.kind      = kind;
	result->floating_widget.reference = reference;
	if (closable) {
		result->floating_widget.close = add_button(ui, result, arena, s8(""),
		                                           UI_BID_CLOSE_WIDGET, 0, ui->small_font);
	}
	result->parent = &ui->floating_widget_sentinal;
	result->next   = ui->floating_widget_sentinal.next;
	result->next->parent = result;
	ui->floating_widget_sentinal.next = result;
	return result;
}

function Variable *
add_ui_split(BeamformerUI *ui, Variable *parent, Arena *arena, s8 name, f32 fraction,
             RegionSplitDirection direction, Font font)
{
	Variable *result = add_variable(ui, parent, arena, name, 0, VT_UI_REGION_SPLIT, font);
	result->region_split.direction = direction;
	result->region_split.fraction  = fraction;
	return result;
}

function Variable *
add_global_menu(BeamformerUI *ui, Arena *arena, Variable *parent)
{
	Variable *result = add_variable_group(ui, 0, &ui->arena, s8(""), VG_LIST, ui->small_font);
	result->parent = parent;
	#define X(id, text) add_button(ui, result, &ui->arena, s8(text), UI_BID_ ##id, 0, ui->small_font);
	GLOBAL_MENU_BUTTONS
	#undef X
	return result;
}

function Variable *
add_ui_view(BeamformerUI *ui, Variable *parent, Arena *arena, s8 name, u32 view_flags, b32 closable)
{
	Variable *result = add_variable(ui, parent, arena, name, 0, VT_UI_VIEW, ui->small_font);
	UIView   *view   = &result->view;
	view->flags      = view_flags;
	view->menu       = add_global_menu(ui, arena, result);
	if (closable) {
		view->close = add_button(ui, 0, arena, s8(""), UI_BID_CLOSE_VIEW, 0, ui->small_font);
		/* NOTE(rnp): we do this explicitly so that close doesn't end up in the view group */
		view->close->parent = result;
	}
	return result;
}

function void
add_beamformer_variable_f32(BeamformerUI *ui, Variable *group, Arena *arena, s8 name, s8 suffix,
                            f32 *store, v2 limits, f32 display_scale, f32 scroll_scale, u32 flags,
                            Font font)
{
	Variable *var = add_variable(ui, group, arena, name, flags, VT_BEAMFORMER_VARIABLE, font);
	BeamformerVariable *bv = &var->beamformer_variable;
	bv->suffix        = suffix;
	bv->store         = store;
	bv->store_type    = VT_F32;
	bv->display_scale = display_scale;
	bv->scroll_scale  = scroll_scale;
	bv->limits        = limits;
}

function Variable *
add_beamformer_parameters_view(Variable *parent, BeamformerCtx *ctx)
{
	BeamformerUI *ui           = ctx->ui;
	BeamformerUIParameters *bp = &ui->params;

	v2 v2_inf = {.x = -F32_INFINITY, .y = F32_INFINITY};

	/* TODO(rnp): this can be closable once we have a way of opening new views */
	Variable *result = add_ui_view(ui, parent, &ui->arena, s8("Parameters"), 0, 0);
	Variable *group  = result->view.child = add_variable(ui, result, &ui->arena, s8(""), 0,
	                                                     VT_GROUP, ui->font);

	add_beamformer_variable_f32(ui, group, &ui->arena, s8("Sampling Frequency:"), s8("[MHz]"),
	                            &bp->sampling_frequency, (v2){0}, 1e-6, 0, 0, ui->font);

	add_beamformer_variable_f32(ui, group, &ui->arena, s8("Center Frequency:"), s8("[MHz]"),
	                            &bp->center_frequency, (v2){.y = 100e-6}, 1e-6, 1e5,
	                            V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

	add_beamformer_variable_f32(ui, group, &ui->arena, s8("Speed of Sound:"), s8("[m/s]"),
	                            &bp->speed_of_sound, (v2){.y = 1e6}, 1, 10,
	                            V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

	group = add_variable_group(ui, group, &ui->arena, s8("Lateral Extent:"), VG_V2, ui->font);
	{
		add_beamformer_variable_f32(ui, group, &ui->arena, s8("Min:"), s8("[mm]"),
		                            bp->output_min_coordinate + 0, v2_inf, 1e3, 0.5e-3,
		                            V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

		add_beamformer_variable_f32(ui, group, &ui->arena, s8("Max:"), s8("[mm]"),
		                            bp->output_max_coordinate + 0, v2_inf, 1e3, 0.5e-3,
		                            V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);
	}
	group = end_variable_group(group);

	group = add_variable_group(ui, group, &ui->arena, s8("Axial Extent:"), VG_V2, ui->font);
	{
		add_beamformer_variable_f32(ui, group, &ui->arena, s8("Min:"), s8("[mm]"),
		                            bp->output_min_coordinate + 2, v2_inf, 1e3, 0.5e-3,
		                            V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

		add_beamformer_variable_f32(ui, group, &ui->arena, s8("Max:"), s8("[mm]"),
		                            bp->output_max_coordinate + 2, v2_inf, 1e3, 0.5e-3,
		                            V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);
	}
	group = end_variable_group(group);

	add_beamformer_variable_f32(ui, group, &ui->arena, s8("Off Axis Position:"), s8("[mm]"),
	                            &bp->off_axis_pos, (v2){.x = -1e3, .y = 1e3}, 0.25e3,
	                            0.5e-3, V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

	read_only local_persist s8 beamform_plane_labels[] = {s8_comp("XZ"), s8_comp("YZ")};
	add_variable_cycler(ui, group, &ui->arena, V_CAUSES_COMPUTE, ui->font, s8("Beamform Plane:"),
	                    (u32 *)&bp->beamform_plane, beamform_plane_labels, countof(beamform_plane_labels));

	add_beamformer_variable_f32(ui, group, &ui->arena, s8("F#:"), s8(""), &bp->f_number,
	                            (v2){.y = 1e3}, 1, 0.1, V_INPUT|V_TEXT|V_CAUSES_COMPUTE, ui->font);

	read_only local_persist s8 true_false_labels[] = {s8_comp("False"), s8_comp("True")};
	add_variable_cycler(ui, group, &ui->arena, V_CAUSES_COMPUTE, ui->font, s8("Interpolate:"),
	                    &bp->interpolate, true_false_labels, countof(true_false_labels));

	add_variable_cycler(ui, group, &ui->arena, V_CAUSES_COMPUTE, ui->font, s8("Coherency Weighting:"),
	                    &bp->coherency_weighting, true_false_labels, countof(true_false_labels));

	return result;
}

function Variable *
add_beamformer_frame_view(BeamformerUI *ui, Variable *parent, Arena *arena,
                          BeamformerFrameViewType type, b32 closable)
{
	/* TODO(rnp): this can be always closable once we have a way of opening new views */
	Variable *result = add_ui_view(ui, parent, arena, s8(""), UI_VIEW_CUSTOM_TEXT, closable);
	Variable *var = result->view.child = add_variable(ui, result, arena, s8(""), 0,
	                                                  VT_BEAMFORMER_FRAME_VIEW, ui->small_font);

	BeamformerFrameView *bv = SLLPop(ui->view_freelist);
	if (bv) zero_struct(bv);
	else    bv = push_struct(arena, typeof(*bv));
	DLLPushDown(bv, ui->views);

	var->generic = bv;
	bv->type     = type;

	fill_variable(&bv->dynamic_range, var, s8("Dynamic Range:"), V_INPUT|V_TEXT|V_UPDATE_VIEW,
	              VT_F32, ui->small_font);
	fill_variable(&bv->threshold, var, s8("Threshold:"), V_INPUT|V_TEXT|V_UPDATE_VIEW,
	              VT_F32, ui->small_font);
	fill_variable(&bv->gamma, var, s8("Gamma:"), V_INPUT|V_TEXT|V_UPDATE_VIEW,
	              VT_SCALED_F32, ui->small_font);

	bv->dynamic_range.real32      = 50.0f;
	bv->threshold.real32          = 55.0f;
	bv->gamma.scaled_real32.val   = 1.0f;
	bv->gamma.scaled_real32.scale = 0.05f;

	fill_variable(&bv->lateral_scale_bar, var, s8(""), V_INPUT, VT_SCALE_BAR, ui->small_font);
	fill_variable(&bv->axial_scale_bar,   var, s8(""), V_INPUT, VT_SCALE_BAR, ui->small_font);
	ScaleBar *lateral            = &bv->lateral_scale_bar.scale_bar;
	ScaleBar *axial              = &bv->axial_scale_bar.scale_bar;
	lateral->direction           = SB_LATERAL;
	axial->direction             = SB_AXIAL;
	lateral->scroll_scale        = (v2){.x = -0.5e-3, .y = 0.5e-3};
	axial->scroll_scale          = (v2){.x =  0,      .y = 1e-3};
	lateral->zoom_starting_coord = F32_INFINITY;
	axial->zoom_starting_coord   = F32_INFINITY;

	Variable *menu = result->view.menu;
	/* TODO(rnp): push to head of list? */
	Variable *old_menu_first = menu->group.first;
	Variable *old_menu_last  = menu->group.last;
	menu->group.first = menu->group.last = 0;

	#define X(id, text) add_button(ui, menu, arena, s8(text), UI_BID_ ##id, 0, ui->small_font);
	FRAME_VIEW_BUTTONS
	#undef X

	switch (type) {
	case FVT_LATEST: {
		#define X(_type, _id, pretty) s8_comp(pretty),
		read_only local_persist s8 labels[] = {IMAGE_PLANE_TAGS s8_comp("Any")};
		#undef X
		bv->cycler = add_variable_cycler(ui, menu, arena, 0, ui->small_font, s8("Live: "),
		                                 &bv->cycler_state, labels, countof(labels));
		bv->cycler_state = IPT_LAST;
	} break;
	case FVT_INDEXED: {
		bv->cycler = add_variable_cycler(ui, menu, arena, 0, ui->small_font, s8("Index: "),
		                                 &bv->cycler_state, 0, MAX_BEAMFORMED_SAVED_FRAMES);
	} break;
	default: break;
	}

	bv->log_scale                = add_variable(ui, menu, arena, s8("Log Scale"),
	                                            V_INPUT|V_UPDATE_VIEW|V_RADIO_BUTTON, VT_B32,
	                                            ui->small_font);
	bv->axial_scale_bar_active   = add_variable(ui, menu, arena, s8("Axial Scale Bar"),
	                                            V_INPUT|V_RADIO_BUTTON, VT_B32, ui->small_font);
	bv->lateral_scale_bar_active = add_variable(ui, menu, arena, s8("Lateral Scale Bar"),
	                                            V_INPUT|V_RADIO_BUTTON, VT_B32, ui->small_font);

	menu->group.last->next = old_menu_first;
	menu->group.last       = old_menu_last;

	return result;
}

function Variable *
add_compute_progress_bar(Variable *parent, BeamformerCtx *ctx)
{
	BeamformerUI *ui = ctx->ui;
	/* TODO(rnp): this can be closable once we have a way of opening new views */
	Variable *result = add_ui_view(ui, parent, &ui->arena, s8(""), UI_VIEW_CUSTOM_TEXT, 0);
	result->view.child = add_variable(ui, result, &ui->arena, s8(""), 0,
	                                  VT_COMPUTE_PROGRESS_BAR, ui->small_font);
	ComputeProgressBar *bar = &result->view.child->compute_progress_bar;
	bar->progress   = &ctx->csctx.processing_progress;
	bar->processing = &ctx->csctx.processing_compute;

	return result;
}

function Variable *
add_compute_stats_view(BeamformerUI *ui, Variable *parent, Arena *arena, VariableType type)
{
	/* TODO(rnp): this can be closable once we have a way of opening new views */
	Variable *result   = add_ui_view(ui, parent, arena, s8(""), UI_VIEW_CUSTOM_TEXT, 0);
	result->view.child = add_variable(ui, result, &ui->arena, s8(""), 0, type, ui->small_font);
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
ui_fill_live_frame_view(BeamformerUI *ui, BeamformerFrameView *bv)
{
	ScaleBar *lateral = &bv->lateral_scale_bar.scale_bar;
	ScaleBar *axial   = &bv->axial_scale_bar.scale_bar;
	lateral->min_value = ui->params.output_min_coordinate + 0;
	lateral->max_value = ui->params.output_max_coordinate + 0;
	axial->min_value   = ui->params.output_min_coordinate + 2;
	axial->max_value   = ui->params.output_max_coordinate + 2;
	bv->axial_scale_bar_active->bool32   = 1;
	bv->lateral_scale_bar_active->bool32 = 1;
	bv->ctx = ui->frame_view_render_context;
	bv->axial_scale_bar.flags   |= V_CAUSES_COMPUTE;
	bv->lateral_scale_bar.flags |= V_CAUSES_COMPUTE;
}

function void
ui_add_live_frame_view(BeamformerUI *ui, Variable *view, RegionSplitDirection direction)
{
	Variable *region = view->parent;
	ASSERT(region->type == VT_UI_REGION_SPLIT);
	ASSERT(view->type   == VT_UI_VIEW);

	Variable *new_region = ui_split_region(ui, region, view, direction);
	new_region->region_split.right = add_beamformer_frame_view(ui, new_region, &ui->arena, FVT_LATEST, 1);

	ui_fill_live_frame_view(ui, new_region->region_split.right->group.first->generic);
}

function void
ui_copy_frame(BeamformerUI *ui, Variable *view, RegionSplitDirection direction)
{
	Variable *region = view->parent;
	ASSERT(region->type == VT_UI_REGION_SPLIT);
	ASSERT(view->type   == VT_UI_VIEW);

	BeamformerFrameView *old = view->group.first->generic;
	/* TODO(rnp): hack; it would be better if this was unreachable with a 0 old->frame */
	if (!old->frame)
		return;

	Variable *new_region = ui_split_region(ui, region, view, direction);
	new_region->region_split.right = add_beamformer_frame_view(ui, new_region, &ui->arena, FVT_COPY, 1);

	BeamformerFrameView *bv = new_region->region_split.right->group.first->generic;
	ScaleBar *lateral  = &bv->lateral_scale_bar.scale_bar;
	ScaleBar *axial    = &bv->axial_scale_bar.scale_bar;
	lateral->min_value = &bv->min_coordinate.x;
	lateral->max_value = &bv->max_coordinate.x;
	axial->min_value   = &bv->min_coordinate.z;
	axial->max_value   = &bv->max_coordinate.z;

	bv->ctx                  = old->ctx;
	bv->needs_update         = 1;
	bv->threshold.real32     = old->threshold.real32;
	bv->dynamic_range.real32 = old->dynamic_range.real32;
	bv->gamma.real32         = old->gamma.real32;
	bv->log_scale->bool32    = old->log_scale->bool32;
	bv->min_coordinate       = old->frame->min_coordinate;
	bv->max_coordinate       = old->frame->max_coordinate;

	bv->frame = SLLPop(ui->frame_freelist);
	if (!bv->frame) bv->frame = push_struct(&ui->arena, typeof(*bv->frame));

	mem_copy(bv->frame, old->frame, sizeof(*bv->frame));
	bv->frame->texture = 0;
	bv->frame->next    = 0;
	alloc_beamform_frame(0, bv->frame, 0, old->frame->dim, s8("Frame Copy: "), ui->arena);

	glCopyImageSubData(old->frame->texture, GL_TEXTURE_3D, 0, 0, 0, 0,
	                   bv->frame->texture,  GL_TEXTURE_3D, 0, 0, 0, 0,
	                   bv->frame->dim.x, bv->frame->dim.y, bv->frame->dim.z);
	glMemoryBarrier(GL_TEXTURE_UPDATE_BARRIER_BIT);
	/* TODO(rnp): x vs y here */
	resize_frame_view(bv, (uv2){.x = bv->frame->dim.x, .y = bv->frame->dim.z});
}

#include <stdlib.h>
function void
ui_export_view(BeamformerUI *ui, Variable *view)
{
	ASSERT(view->type == VT_UI_VIEW);

	u8 buffer[1024];

	BeamformerFrameView *bv = view->group.first->generic;
	if (!bv->frame)
		return;

	Stream buf  = arena_stream(ui->arena);
	Stream path = {.data = buffer, .cap = sizeof(buffer)};

	v4 min = bv->frame->min_coordinate, max = bv->frame->max_coordinate;

	stream_append_s8(&path, s8("/tmp/downloads/"));
	push_das_shader_kind(&path, bv->frame->das_shader_kind, bv->frame->compound_count);
	stream_append_byte(&path, '_');
	stream_append_hex_u64(&path, bv->frame->id);
	iz pidx = path.widx;
	stream_append_s8(&path, s8("_params.csv"));
	stream_append_byte(&path, 0);

	stream_append_s8(&buf, s8("min_coord,max_coord,size,dynamic_range\n"));
	stream_append_f64(&buf, min.x, 1000); stream_append_byte(&buf, ',');
	stream_append_f64(&buf, max.x, 1000); stream_append_byte(&buf, ',');
	stream_append_u64(&buf, bv->texture_dim.w); stream_append_byte(&buf, ',');
	stream_append_f64(&buf, bv->dynamic_range.real32, 100);
	stream_append_byte(&buf, '\n');
	stream_append_f64(&buf, min.z, 1000); stream_append_byte(&buf, ',');
	stream_append_f64(&buf, max.z, 1000); stream_append_byte(&buf, ',');
	stream_append_u64(&buf, bv->texture_dim.h); stream_append_byte(&buf, ',');
	stream_append_byte(&buf, '\n');
	if (!ui->os->write_new_file((c8 *)path.data, stream_to_s8(&buf)))
		ui->os->write_file(ui->os->error_handle, s8("failed to export view parameters\n"));

	stream_reset(&path, pidx);
	stream_append_s8(&path, s8(".bin"));
	stream_append_byte(&path, 0);

	iz out_size = bv->texture_dim.h * bv->texture_dim.w * sizeof(u32);
	void *out_buf = malloc(out_size);
	if (out_buf) {
		//ctx->export_buffer = ctx->os.alloc_arena(ctx->export_buffer, out_size);
		glGetTextureImage(bv->texture, 0, GL_RGBA,
		                  GL_UNSIGNED_INT_8_8_8_8, out_size, out_buf);
		s8 raw = {.len = out_size, .data = out_buf};
		if (!ui->os->write_new_file((c8 *)path.data, raw))
			ui->os->write_file(ui->os->error_handle, s8("failed to export view\n"));
		free(out_buf);
	}
}

function b32
view_update(BeamformerUI *ui, BeamformerFrameView *view)
{
	if (view->type == FVT_LATEST) {
		u32 index = *view->cycler->cycler.state;
		view->needs_update |= view->frame != ui->latest_plane[index];
		view->frame         = ui->latest_plane[index];
		if (view->needs_update) {
			view->min_coordinate = v4_from_f32_array(ui->params.output_min_coordinate);
			view->max_coordinate = v4_from_f32_array(ui->params.output_max_coordinate);
		}
	}

	/* TODO(rnp): x-z or y-z */
	/* TODO(rnp): add method of setting a target size in frame view */
	uv2 current = view->texture_dim;
	uv2 target  = {.w = ui->params.output_points[0], .h = ui->params.output_points[2]};
	if (view->type != FVT_COPY && !uv2_equal(current, target) && !uv2_equal(target, (uv2){0})) {
		resize_frame_view(view, target);
		view->needs_update = 1;
	}

	return (view->ctx->updated || view->needs_update) && view->frame;
}

function void
update_frame_views(BeamformerUI *ui, Rect window)
{
	b32 fbo_bound = 0;
	for (BeamformerFrameView *view = ui->views; view; view = view->next) {
		if (view_update(ui, view)) {
			if (!fbo_bound) {
				fbo_bound = 1;
				glBindFramebuffer(GL_FRAMEBUFFER, view->ctx->framebuffer);
				glUseProgram(view->ctx->shader);
				glBindVertexArray(view->ctx->vao);
			}
			glViewport(0, 0, view->texture_dim.w, view->texture_dim.h);
			glNamedFramebufferTexture(view->ctx->framebuffer, GL_COLOR_ATTACHMENT0,
			                          view->texture, 0);
			glClearNamedFramebufferfv(view->ctx->framebuffer, GL_COLOR, 0,
			                          (f32 []){0.79, 0.46, 0.77, 1});
			glBindTextureUnit(0, view->frame->texture);
			glProgramUniform1f(view->ctx->shader,  FRAME_VIEW_RENDER_DYNAMIC_RANGE_LOC, view->dynamic_range.real32);
			glProgramUniform1f(view->ctx->shader,  FRAME_VIEW_RENDER_THRESHOLD_LOC,     view->threshold.real32);
			glProgramUniform1f(view->ctx->shader,  FRAME_VIEW_RENDER_GAMMA_LOC,         view->gamma.scaled_real32.val);
			glProgramUniform1ui(view->ctx->shader, FRAME_VIEW_RENDER_LOG_SCALE_LOC,     view->log_scale->bool32);

			glDrawArrays(GL_TRIANGLES, 0, 6);
			glGenerateTextureMipmap(view->texture);
			view->needs_update = 0;
		}
	}
	if (fbo_bound) {
		glBindFramebuffer(GL_FRAMEBUFFER, 0);
		glViewport(window.pos.x, window.pos.y, window.size.w, window.size.h);
		/* NOTE(rnp): I don't trust raylib to not mess with us */
		glBindVertexArray(0);
	}
}

function b32
frame_view_ready_to_present(BeamformerFrameView *view)
{
	return !uv2_equal((uv2){0}, view->texture_dim) && view->frame;
}

function Color
colour_from_normalized(v4 rgba)
{
	return (Color){.r = rgba.r * 255.0f, .g = rgba.g * 255.0f,
	               .b = rgba.b * 255.0f, .a = rgba.a * 255.0f};
}

function Color
fade(Color a, f32 visibility)
{
	a.a = (u8)((f32)a.a * visibility);
	return a;
}

function v4
lerp_v4(v4 a, v4 b, f32 t)
{
	return (v4){
		.x = a.x + t * (b.x - a.x),
		.y = a.y + t * (b.y - a.y),
		.z = a.z + t * (b.z - a.z),
		.w = a.w + t * (b.w - a.w),
	};
}

function v2
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
function v2
draw_outlined_text(s8 text, v2 pos, TextSpec *ts)
{
	f32 ow = ts->outline_thick;
	Color outline = colour_from_normalized(ts->outline_colour);
	Color colour  = colour_from_normalized(ts->colour);
	draw_text_base(*ts->font, text, sub_v2(pos, (v2){.x =  ow, .y =  ow}), outline);
	draw_text_base(*ts->font, text, sub_v2(pos, (v2){.x =  ow, .y = -ow}), outline);
	draw_text_base(*ts->font, text, sub_v2(pos, (v2){.x = -ow, .y =  ow}), outline);
	draw_text_base(*ts->font, text, sub_v2(pos, (v2){.x = -ow, .y = -ow}), outline);

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
	v2  end    = add_v2(r.pos, r.size);
	b32 result = BETWEEN(p.x, r.pos.x, end.x) & BETWEEN(p.y, r.pos.y, end.y);
	return result;
}

function v2
screen_point_to_world_2d(v2 p, v2 screen_min, v2 screen_max, v2 world_min, v2 world_max)
{
	v2 pixels_to_m = div_v2(sub_v2(world_max, world_min), sub_v2(screen_max, screen_min));
	v2 result      = add_v2(mul_v2(sub_v2(p, screen_min), pixels_to_m), world_min);
	return result;
}

function v2
world_point_to_screen_2d(v2 p, v2 world_min, v2 world_max, v2 screen_min, v2 screen_max)
{
	v2 m_to_pixels = div_v2(sub_v2(screen_max, screen_min), sub_v2(world_max, world_min));
	v2 result      = add_v2(mul_v2(sub_v2(p, world_min), m_to_pixels), screen_min);
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

	Color colour = colour_from_normalized(lerp_v4(MENU_CLOSE_COLOUR, FG_COLOUR, close->hover_t));
	r = scale_rect_centered(r, x_scale);
	DrawLineEx(r.pos.rl, add_v2(r.pos, r.size).rl, 4, colour);
	DrawLineEx(add_v2(r.pos, (v2){.x = r.size.w}).rl,
	           add_v2(r.pos, (v2){.y = r.size.h}).rl, 4, colour);
}

function Rect
draw_title_bar(BeamformerUI *ui, Arena arena, Variable *ui_view, Rect r, v2 mouse)
{
	assert(ui_view->type == VT_UI_VIEW);
	UIView *view = &ui_view->view;

	s8 title = ui_view->name;
	if (view->flags & UI_VIEW_CUSTOM_TEXT) {
		Stream buf = arena_stream(arena);
		push_custom_view_title(&buf, ui_view->group.first);
		title = arena_stream_commit(&arena, &buf);
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
		draw_close_button(ui, view->close, mouse, close, (v2){{.4, .4}});
	}

	if (view->menu) {
		Rect menu;
		cut_rect_horizontal(title_rect, title_rect.size.w - title_rect.size.h, &title_rect, &menu);
		Interaction interaction = {.kind = InteractionKind_Menu, .var = view->menu, .rect = menu};
		hover_interaction(ui, mouse, interaction);

		Color colour = colour_from_normalized(lerp_v4(MENU_PLUS_COLOUR, FG_COLOUR, view->menu->hover_t));
		menu = shrink_rect_centered(menu, (v2){.x = 14, .y = 14});
		DrawLineEx(add_v2(menu.pos, (v2){.x = menu.size.w / 2}).rl,
		           add_v2(menu.pos, (v2){.x = menu.size.w / 2, .y = menu.size.h}).rl, 4, colour);
		DrawLineEx(add_v2(menu.pos, (v2){.y = menu.size.h / 2}).rl,
		           add_v2(menu.pos, (v2){.x = menu.size.w, .y = menu.size.h / 2}).rl, 4, colour);
	}

	v2 title_pos = title_rect.pos;
	title_pos.y += 0.5 * TITLE_BAR_PAD;
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

	end_point    = sub_v2(end_point, start_point);
	f32 rotation = atan2_f32(end_point.y, end_point.x) * 180 / PI;

	rlPushMatrix();
	rlTranslatef(start_point.x, start_point.y, 0);
	rlRotatef(rotation, 0, 0, 1);

	f32 inc       = magnitude_v2(end_point) / segments;
	f32 value_inc = (end_value - start_value) / segments;
	f32 value     = start_value;

	Stream buf = arena_stream(arena);
	v2 sp = {0}, ep = {.y = RULER_TICK_LENGTH};
	v2 tp = {.x = ui->small_font.baseSize / 2, .y = ep.y + RULER_TEXT_PAD};
	TextSpec text_spec = {.font = &ui->small_font, .rotation = 90, .colour = txt_colour, .flags = TF_ROTATED};
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
	v2   relative_mouse = sub_v2(mouse, tick_rect.pos);

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
		tick_count        = tick_rect.size.y / (1.5 * ui->small_font.baseSize);
		start_pos.y      += tick_rect.size.y;
		markers[0]        = tick_rect.size.y - screen_zoom_point.y;
		markers[1]        = tick_rect.size.y - relative_mouse.y;
	} else {
		tick_rect.size.y  = RULER_TEXT_PAD + RULER_TICK_LENGTH + txt_s.x;
		tick_count        = tick_rect.size.x / (1.5 * ui->small_font.baseSize);
		end_pos.x        += tick_rect.size.x;
		markers[0]        = screen_zoom_point.x;
		markers[1]        = relative_mouse.x;
	}

	if (hover_interaction(ui, mouse, auto_interaction(tick_rect, scale_bar)))
		marker_count = 2;

	draw_ruler(ui, arena, start_pos, end_pos, start_value, end_value, markers, marker_count,
	           tick_count, suffix, RULER_COLOUR, lerp_v4(FG_COLOUR, HOVERED_COLOUR, scale_bar->hover_t));
}

function v2
draw_radio_button(BeamformerUI *ui, Variable *var, v2 at, v2 mouse, v4 base_colour, f32 size)
{
	assert(var->type == VT_B32 || var->type == VT_BEAMFORMER_VARIABLE);
	b32 value;
	if (var->type == VT_B32) {
		value = var->bool32;
	} else {
		assert(var->beamformer_variable.store_type == VT_B32);
		value = *(b32 *)var->beamformer_variable.store;
	}

	v2 result = (v2){.x = size, .y = size};
	Rect hover_rect   = {.pos = at, .size = result};
	hover_rect.pos.y += 1;
	hover_interaction(ui, mouse, auto_interaction(hover_rect, var));

	hover_rect = shrink_rect_centered(hover_rect, (v2){.x = 8, .y = 8});
	Rect inner = shrink_rect_centered(hover_rect, (v2){.x = 4, .y = 4});
	v4 fill = lerp_v4(value? base_colour : (v4){0}, HOVERED_COLOUR, var->hover_t);
	DrawRectangleRoundedLinesEx(hover_rect.rl, 0.2, 0, 2, colour_from_normalized(base_colour));
	DrawRectangleRec(inner.rl, colour_from_normalized(fill));

	return result;
}

function v2
draw_variable(BeamformerUI *ui, Arena arena, Variable *var, v2 at, v2 mouse, v4 base_colour, TextSpec text_spec)
{
	v2 result;
	if (var->flags & V_RADIO_BUTTON) {
		result = draw_radio_button(ui, var, at, mouse, base_colour, text_spec.font->baseSize);
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
			text_spec.colour = lerp_v4(base_colour, HOVERED_COLOUR, var->hover_t);
		}

		draw_text(text, at, &text_spec);
	}
	return result;
}

function v2
draw_table_cell(BeamformerUI *ui, TableCell *cell, Rect cell_rect, TextAlignment alignment,
                TextSpec ts, v2 mouse)
{
	/* NOTE(rnp): use desired width for alignment and clamped width for drawing */
	f32 start_x = cell_rect.pos.x;
	v2 cell_at  = table_cell_align(cell, alignment, cell_rect);
	ts.limits.size.w -= (cell_at.x - start_x);
	cell_rect.size.w  = MIN(ts.limits.size.w, cell_rect.size.w);

	v4 base_colour = ts.colour;
	if (cell->kind == TCK_VARIABLE && cell->var->flags & V_INPUT) {
		Rect hover = {.pos = cell_at, .size = {.w = cell->width, .h = cell_rect.size.h}};
		if (hover_interaction(ui, mouse, auto_interaction(hover, cell->var)) && (cell->var->flags & V_TEXT))
			ui->text_input_state.hot_font = ts.font;
		ts.colour = lerp_v4(ts.colour, HOVERED_COLOUR, cell->var->hover_t);
	}

	/* TODO(rnp): push truncated text for hovering */
	if (cell->kind == TCK_VARIABLE && cell->var->flags & V_RADIO_BUTTON)
		draw_radio_button(ui, cell->var, cell_at, mouse, base_colour, ts.font->baseSize);
	else if (cell->text.len)
		draw_text(cell->text, cell_at, &ts);
	/* TODO(rnp): draw column border */

	return cell_rect.size;
}

function v2
draw_table_row(BeamformerUI *ui, Arena arena, TableCell *cells, TextAlignment *cell_alignments,
               f32 *widths, i32 cell_count, Rect draw_rect, TextSpec ts, v2 mouse)
{
	Rect cell_rect = {.pos = draw_rect.pos, .size.h = draw_rect.size.h};
	for (i32 i = 0; i < cell_count; i++) {
		TableCell *cell  = cells + i;
		cell_rect.size.w = widths[i];

		f32 dw = draw_table_cell(ui, cell, cell_rect, cell_alignments[i], ts, mouse).w;
		cell_rect.pos.x  += dw;
		ts.limits.size.w -= dw;
	}
	return (v2){.x = draw_rect.pos.x - cell_rect.pos.x, .y = draw_rect.size.h};
}

function v2
draw_table(BeamformerUI *ui, Arena arena, Table *table, Rect draw_rect, TextSpec ts, v2 mouse)
{
	ts.flags |= TF_LIMITED;
	ts.limits.size.w = draw_rect.size.w;

	f32 start_height  = draw_rect.size.h;
	i32 row_index     = table_skip_rows(table, draw_rect.size.h, ts.font->baseSize);
	TableIterator *it = table_iterator_new(table, TIK_ROWS, &arena, row_index, (v2){0}, ts.font);
	for (TableRow *row = table_iterator_next(it, &arena);
	     row;
	     row = table_iterator_next(it, &arena))
	{
		Table *table    = it->frame.table;
		Rect row_rect   = draw_rect;
		row_rect.size.h = ts.font->baseSize + TABLE_CELL_PAD_HEIGHT;
		f32 h = draw_table_row(ui, arena, row->data, table->alignment, table->widths,
		                       table->columns, row_rect, ts, mouse).y;
		draw_rect.pos.y  += h;
		draw_rect.size.y -= h;
		/* TODO(rnp): draw row border */
	}
	v2 result = {.x = table_width(table), .y = start_height - draw_rect.size.h};
	return result;
}

function void
draw_view_ruler(BeamformerFrameView *view, Arena a, Rect view_rect, TextSpec ts)
{
	v2 vr_max_p = add_v2(view_rect.pos, view_rect.size);
	v2 start_p  = world_point_to_screen_2d(view->ruler.start, XZ(view->min_coordinate),
	                                       XZ(view->max_coordinate), view_rect.pos, vr_max_p);
	v2 end_p    = world_point_to_screen_2d(view->ruler.end, XZ(view->min_coordinate),
		                               XZ(view->max_coordinate), view_rect.pos, vr_max_p);

	Color rl_colour = colour_from_normalized(ts.colour);
	DrawCircleV(start_p.rl, 3, rl_colour);
	DrawLineEx(end_p.rl, start_p.rl, 2, rl_colour);
	DrawCircleV(end_p.rl, 3, rl_colour);

	Stream buf = arena_stream(a);
	stream_append_f64(&buf, 1e3 * magnitude_v2(sub_v2(view->ruler.end, view->ruler.start)), 100);
	stream_append_s8(&buf, s8(" mm"));

	v2 txt_p = start_p;
	v2 txt_s = measure_text(*ts.font, stream_to_s8(&buf));
	v2 pixel_delta = sub_v2(start_p, end_p);
	if (pixel_delta.y < 0) txt_p.y -= txt_s.y;
	if (pixel_delta.x < 0) txt_p.x -= txt_s.x;
	if (txt_p.x < view_rect.pos.x) txt_p.x = view_rect.pos.x;
	if (txt_p.x + txt_s.x > vr_max_p.x) txt_p.x -= (txt_p.x + txt_s.x) - vr_max_p.x;

	draw_text(stream_to_s8(&buf), txt_p, &ts);
}

function void
draw_beamformer_frame_view(BeamformerUI *ui, Arena a, Variable *var, Rect display_rect, v2 mouse)
{
	assert(var->type == VT_BEAMFORMER_FRAME_VIEW);
	BeamformerFrameView *view = var->generic;
	BeamformFrame *frame      = view->frame;

	v2 txt_s = measure_text(ui->small_font, s8("-288.8 mm"));
	f32 scale_bar_size = 1.2 * txt_s.x + RULER_TICK_LENGTH;

	v4 min = view->min_coordinate;
	v4 max = view->max_coordinate;
	v2 requested_dim = sub_v2(XZ(max), XZ(min));
	f32 aspect = requested_dim.w / requested_dim.h;

	Rect vr = display_rect;
	v2 scale_bar_area = {0};
	if (view->axial_scale_bar_active->bool32) {
		vr.pos.y         += 0.5 * ui->small_font.baseSize;
		scale_bar_area.x += scale_bar_size;
		scale_bar_area.y += ui->small_font.baseSize;
	}

	if (view->lateral_scale_bar_active->bool32) {
		vr.pos.x         += 0.5 * ui->small_font.baseSize;
		scale_bar_area.x += ui->small_font.baseSize;
		scale_bar_area.y += scale_bar_size;
	}

	vr.size = sub_v2(vr.size, scale_bar_area);
	if (aspect > 1) vr.size.h = vr.size.w / aspect;
	else            vr.size.w = vr.size.h * aspect;

	v2 occupied = add_v2(vr.size, scale_bar_area);
	if (occupied.w > display_rect.size.w) {
		vr.size.w -= (occupied.w - display_rect.size.w);
		vr.size.h  = vr.size.w / aspect;
	} else if (occupied.h > display_rect.size.h) {
		vr.size.h -= (occupied.h - display_rect.size.h);
		vr.size.w  = vr.size.h * aspect;
	}
	occupied = add_v2(vr.size, scale_bar_area);
	vr.pos   = add_v2(vr.pos, scale_v2(sub_v2(display_rect.size, occupied), 0.5));

	/* TODO(rnp): make this depend on the requested draw orientation (x-z or y-z or x-y) */
	v2 output_dim = {
		.x = frame->max_coordinate.x - frame->min_coordinate.x,
		.y = frame->max_coordinate.z - frame->min_coordinate.z,
	};

	v2 pixels_per_meter = {
		.w = (f32)view->texture_dim.w / output_dim.w,
		.h = (f32)view->texture_dim.h / output_dim.h,
	};

	v2 texture_points  = mul_v2(pixels_per_meter, requested_dim);
	/* TODO(rnp): this also depends on x-y, y-z, x-z */
	v2 texture_start   = {
		.x = pixels_per_meter.x * 0.5 * (output_dim.x - requested_dim.x),
		.y = pixels_per_meter.y * (frame->max_coordinate.z - max.z),
	};

	Rectangle  tex_r  = {texture_start.x, texture_start.y, texture_points.x, -texture_points.y};
	NPatchInfo tex_np = { tex_r, 0, 0, 0, 0, NPATCH_NINE_PATCH };
	DrawTextureNPatch(make_raylib_texture(view), tex_np, vr.rl, (Vector2){0}, 0, WHITE);

	v2 start_pos  = vr.pos;
	start_pos.y  += vr.size.y;

	if (vr.size.w > 0 && view->lateral_scale_bar_active->bool32) {
		do_scale_bar(ui, a, &view->lateral_scale_bar, mouse,
		             (Rect){.pos = start_pos, .size = vr.size},
		             *view->lateral_scale_bar.scale_bar.min_value * 1e3,
		             *view->lateral_scale_bar.scale_bar.max_value * 1e3, s8(" mm"));
	}

	start_pos    = vr.pos;
	start_pos.x += vr.size.x;

	if (vr.size.h > 0 && view->axial_scale_bar_active->bool32) {
		do_scale_bar(ui, a, &view->axial_scale_bar, mouse,
		             (Rect){.pos = start_pos, .size = vr.size},
		             *view->axial_scale_bar.scale_bar.max_value * 1e3,
		             *view->axial_scale_bar.scale_bar.min_value * 1e3, s8(" mm"));
	}

	TextSpec text_spec = {.font = &ui->small_font, .flags = TF_LIMITED|TF_OUTLINED,
	                      .colour = RULER_COLOUR, .outline_thick = 1, .outline_colour.a = 1,
	                      .limits.size.x = vr.size.w};

	f32 draw_table_width = vr.size.w;
	/* NOTE: avoid hover_t modification */
	Interaction viewer = auto_interaction(vr, var);
	if (point_in_rect(mouse, viewer.rect)) {
		ui->next_interaction = viewer;

		v2 world = screen_point_to_world_2d(mouse, vr.pos, add_v2(vr.pos, vr.size),
		                                    XZ(view->min_coordinate),
		                                    XZ(view->max_coordinate));
		Stream buf = arena_stream(a);
		stream_append_v2(&buf, scale_v2(world, 1e3));

		text_spec.limits.size.w -= 4;
		v2 txt_s = measure_text(*text_spec.font, stream_to_s8(&buf));
		v2 txt_p = {
			.x = vr.pos.x + vr.size.w - txt_s.w - 4,
			.y = vr.pos.y + vr.size.h - txt_s.h - 4,
		};
		txt_p.x = MAX(vr.pos.x, txt_p.x);
		draw_table_width -= draw_text(stream_to_s8(&buf), txt_p, &text_spec).w;
		text_spec.limits.size.w += 4;
	}

	{
		Stream buf = arena_stream(a);
		s8 shader  = push_das_shader_kind(&buf, frame->das_shader_kind, frame->compound_count);
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

	if (view->ruler.state != RulerState_None) draw_view_ruler(view, a, vr, text_spec);

	Table *table = table_new(&a, 3, 3, (TextAlignment []){TA_LEFT, TA_LEFT, TA_LEFT});
	table_push_parameter_row(table, &a, view->gamma.name,     &view->gamma,     s8(""));
	table_push_parameter_row(table, &a, view->threshold.name, &view->threshold, s8(""));
	if (view->log_scale->bool32)
		table_push_parameter_row(table, &a, view->dynamic_range.name, &view->dynamic_range, s8("[dB]"));

	Rect table_rect = vr;
	f32 height      = table_extent(table, a, text_spec.font).y;
	height          = MIN(height, vr.size.h);
	table_rect.pos.w  += 8;
	table_rect.pos.y  += vr.size.h - height - 8;
	table_rect.size.h  = height;
	table_rect.size.w  = draw_table_width - 16;

	draw_table(ui, a, table, table_rect, text_spec, mouse);
}

function v2
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

function v2
draw_compute_stats_view(BeamformerCtx *ctx, Arena arena, ComputeShaderStats *stats, Rect r)
{
	#define X(e, n, s, h, pn) [ComputeShaderKind_##e] = s8_comp(pn ":"),
	read_only local_persist s8 labels[ComputeShaderKind_Count] = {COMPUTE_SHADERS};
	#undef X

	BeamformerSharedMemory *sm = ctx->shared_memory.region;
	BeamformerUI *ui     = ctx->ui;
	f32 compute_time_sum = 0;
	u32 stages           = sm->compute_stages_count;
	TextSpec text_spec   = {.font = &ui->font, .colour = FG_COLOUR, .flags = TF_LIMITED};

	Table *table = table_new(&arena, stages + 1, 3, (TextAlignment []){TA_LEFT, TA_LEFT, TA_LEFT});
	for (u32 i = 0; i < stages; i++) {
		TableCell *cells = table_push_row(table, &arena, TRK_CELLS)->data;

		Stream sb = arena_stream(arena);
		ShaderKind index = (ShaderKind)sm->compute_stages[i];
		compute_time_sum += stats->times[index];
		stream_append_f64_e(&sb, stats->times[index]);

		cells[0].text = labels[index];
		cells[1].text = arena_stream_commit(&arena, &sb);
		cells[2].text = s8("[s]");
	}

	TableCell *cells = table_push_row(table, &arena, TRK_CELLS)->data;
	Stream sb = arena_stream(arena);
	stream_append_f64_e(&sb, compute_time_sum);
	cells[0].text = s8("Compute Total:");
	cells[1].text = arena_stream_commit(&arena, &sb);
	cells[2].text = s8("[s]");

	table_extent(table, arena, text_spec.font);
	return draw_table(ui, arena, table, r, text_spec, (v2){0});
}

function v2
draw_ui_view_listing(BeamformerUI *ui, Variable *group, Arena arena, Rect r, v2 mouse, TextSpec text_spec)
{
	ASSERT(group->type == VT_GROUP);
	Table *table  = table_new(&arena, 0, 3, (TextAlignment []){TA_LEFT, TA_LEFT, TA_RIGHT});
	/* NOTE(rnp): minimum width for middle column */
	table->widths[1] = 150;

	Variable *var = group->group.first;
	while (var) {
		switch (var->type) {
		case VT_CYCLER:
		case VT_BEAMFORMER_VARIABLE: {
			s8 suffix = s8("");
			if (var->type == VT_BEAMFORMER_VARIABLE)
				suffix = var->beamformer_variable.suffix;
			table_push_parameter_row(table, &arena, var->name, var, suffix);
			while (var) {
				if (var->next) {
					var = var->next;
					break;
				}
				var   = var->parent;
				table = table_end_subtable(table);
			}
		} break;
		case VT_GROUP: {
			VariableGroup *g = &var->group;

			TableCell *cells = table_push_row(table, &arena, TRK_CELLS)->data;
			cells[0] = (TableCell){.text = var->name, .kind = TCK_VARIABLE, .var = var};

			if (g->expanded) {
				var = g->first;
				table = table_begin_subtable(table, &arena, table->columns,
				                             (TextAlignment []){TA_LEFT, TA_CENTER, TA_RIGHT});
				table->widths[1] = 100;
			} else {
				Variable *v = g->first;

				assert(!v || v->type == VT_BEAMFORMER_VARIABLE);
				/* NOTE(rnp): assume the suffix is the same for all elements */
				if (v) cells[2].text = v->beamformer_variable.suffix;

				Stream sb = arena_stream(arena);
				switch (g->type) {
				case VG_LIST: break;
				case VG_V2:
				case VG_V4: {
					stream_append_s8(&sb, s8("{"));
					while (v) {
						stream_append_variable(&sb, v);
						v = v->next;
						if (v) stream_append_s8(&sb, s8(", "));
					}
					stream_append_s8(&sb, s8("}"));
				} break;
				}
				cells[1].kind = TCK_VARIABLE_GROUP;
				cells[1].text = arena_stream_commit(&arena, &sb);
				cells[1].var  = var;

				var = var->next;
			}
		} break;
		INVALID_DEFAULT_CASE;
		}
	}

	text_spec.flags |= TF_LIMITED;
	v2 result = table_extent(table, arena, text_spec.font);
	TableIterator *it = table_iterator_new(table, TIK_CELLS, &arena, 0, r.pos, text_spec.font);
	for (TableCell *cell = table_iterator_next(it, &arena);
	     cell;
	     cell = table_iterator_next(it, &arena))
	{
		text_spec.limits.size.w = r.size.w - (it->cell_rect.pos.x - it->start_x);
		/* TODO(rnp): ensure this doesn't exceed r.size */
		Rect rect;
		rect.pos  = add_v2(it->cell_rect.pos, scale_v2((v2){.x = text_spec.font->baseSize}, it->sub_table_depth));
		rect.size = it->cell_rect.size;
		if (cell->kind == TCK_VARIABLE_GROUP) {
			Variable *v = cell->var->group.first;
			v2 at = table_cell_align(cell, it->alignment, rect);
			text_spec.limits.size.w = r.size.w - (at.x - it->start_x);
			f32 dw = draw_text(s8("{"), at, &text_spec).x;
			while (v) {
				at.x += dw;
				text_spec.limits.size.w -= dw;
				dw = draw_variable(ui, arena, v, at, mouse, text_spec.colour, text_spec).x;

				v = v->next;
				if (v) {
					at.x += dw;
					text_spec.limits.size.w -= dw;
					dw = draw_text(s8(", "), at, &text_spec).x;
				}
			}
			at.x += dw;
			text_spec.limits.size.w -= dw;
			draw_text(s8("}"), at, &text_spec);
		} else {
			draw_table_cell(ui, cell, rect, it->alignment, text_spec, mouse);
		}
	}

	return result;
}

function void
draw_ui_view(BeamformerUI *ui, Variable *ui_view, Rect r, v2 mouse, TextSpec text_spec)
{
	ASSERT(ui_view->type == VT_UI_VIEW);
	UIView *view = &ui_view->view;

	if (view->needed_height - r.size.h < view->offset)
		view->offset = view->needed_height - r.size.h;

	if (view->needed_height - r.size.h < 0)
		view->offset = 0;

	r.pos.y -= view->offset;

	v2 size = {0};

	Variable *var = view->child;
	switch (var->type) {
	case VT_GROUP: size = draw_ui_view_listing(ui, var, ui->arena, r, mouse, text_spec); break;
	case VT_BEAMFORMER_FRAME_VIEW: {
		BeamformerFrameView *bv = var->generic;
		if (frame_view_ready_to_present(bv))
			draw_beamformer_frame_view(ui, ui->arena, var, r, mouse);
	} break;
	case VT_COMPUTE_PROGRESS_BAR: {
		size = draw_compute_progress_bar(ui, ui->arena, &var->compute_progress_bar, r);
	} break;
	case VT_COMPUTE_LATEST_STATS_VIEW:
	case VT_COMPUTE_STATS_VIEW: {
		ComputeShaderStats *stats = var->compute_stats_view.stats;
		if (var->type == VT_COMPUTE_LATEST_STATS_VIEW)
			stats = *(ComputeShaderStats **)stats;
		size = draw_compute_stats_view(var->compute_stats_view.ctx, ui->arena, stats, r);
	} break;
	InvalidDefaultCase;
	}

	view->needed_height = size.y;
}

function Rect
draw_menu(BeamformerUI *ui, Arena arena, Variable *menu, Font *font, v2 at, v2 mouse, Rect window)
{
	assert(menu->type == VT_GROUP);

	f32 font_height     = font->baseSize;
	f32 max_label_width = 0;

	Variable *item = menu->group.first;
	i32 item_count = 0;
	b32 radio = 0;
	while (item) {
		max_label_width = MAX(max_label_width, item->name_width);
		radio |= (item->flags & V_RADIO_BUTTON) != 0;
		item_count++;
		item = item->next;
	}

	f32 radio_button_width = radio? font_height : 0;
	f32 menu_width  = max_label_width + radio_button_width + 8;
	f32 menu_height = item_count * font_height + (item_count - 1) * 2;
	menu_height = MAX(menu_height, 0);

	if (at.x + menu_width > window.size.w)
		at.x = window.size.w - menu_width  - 16;
	if (at.y + menu_height > window.size.h)
		at.y = window.size.h - menu_height - 12;
	/* TODO(rnp): scroll menu if it doesn't fit on screen */

	Rect result = {.pos = at, .size = {.w = menu_width, .h = menu_height}};
	result.size = add_v2(result.size, (v2){.x = 12, .y = 8});

	v2 start = at = add_v2(result.pos, (v2){.x = 6,  .y = 4});
	for (i32 i = 0; i < item_count - 1; i++) {
		at.y += 2 + font_height;
		DrawLineEx((v2){.x = at.x - 3, .y = at.y}.rl,
		           add_v2(at, (v2){.w = menu_width + 3}).rl, 2, fade(BLACK, 0.8));
	}

	item = menu->group.first;
	TextSpec text_spec = {.font = font, .colour = FG_COLOUR, .limits.size.w = menu_width};
	at = start;
	while (item) {
		at.x = start.x;
		if (item->type == VT_CYCLER) {
			at.x += draw_text(item->name, at, &text_spec).x;
		} else if (item->flags & V_RADIO_BUTTON) {
			draw_text(item->name, at, &text_spec);
			at.x += max_label_width + 8;
		}
		at.y += draw_variable(ui, arena, item, at, mouse, FG_COLOUR, text_spec).y + 2;
		item = item->next;
	}
	return result;
}

function void
draw_layout_variable(BeamformerUI *ui, Variable *var, Rect draw_rect, v2 mouse)
{
	if (var->type != VT_UI_REGION_SPLIT) {
		v2 shrink = {.x = UI_REGION_PAD, .y = UI_REGION_PAD};
		draw_rect = shrink_rect_centered(draw_rect, shrink);
		draw_rect.size = floor_v2(draw_rect.size);
		BeginScissorMode(draw_rect.pos.x, draw_rect.pos.y, draw_rect.size.w, draw_rect.size.h);
		draw_rect = draw_title_bar(ui, ui->arena, var, draw_rect, mouse);
		EndScissorMode();
	}

	/* TODO(rnp): post order traversal of the ui tree will remove the need for this */
	if (!CheckCollisionPointRec(mouse.rl, draw_rect.rl))
		mouse = (v2){.x = F32_INFINITY, .y = F32_INFINITY};

	draw_rect.size = floor_v2(draw_rect.size);
	BeginScissorMode(draw_rect.pos.x, draw_rect.pos.y, draw_rect.size.w, draw_rect.size.h);
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

		Interaction drag = {.kind = InteractionKind_Drag, .rect = hover, .var = var};
		hover_interaction(ui, mouse, drag);

		v4 colour = HOVERED_COLOUR;
		colour.a  = var->hover_t;
		DrawRectangleRounded(split.rl, 0.6, 0, colour_from_normalized(colour));
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
draw_floating_widget_container(BeamformerUI *ui, Variable *var, v2 mouse, Rect bounds)
{
	FloatingWidget *fw = &var->floating_widget;
	if (fw->rect.size.x > 0 && fw->rect.size.y > 0) {
		f32 line_height = ui->small_font.baseSize;

		if (fw->rect.pos.y - line_height < 0) fw->rect.pos.y += line_height - fw->rect.pos.y;
		f32 delta_x = fw->rect.pos.x + fw->rect.size.x - bounds.size.x;
		if (delta_x > 0) {
			fw->rect.pos.x -= delta_x;
			fw->rect.pos.x  = MAX(0, fw->rect.pos.x);
		}

		Rect container = fw->rect;
		if (fw->close) {
			container.pos.y  -= 5 + line_height;
			container.size.y += 2 + line_height;
			Rect handle = {{container.pos, (v2){.x = container.size.w, .y = 2 + line_height}}};
			Rect close;
			hover_interaction(ui, mouse, auto_interaction(container, var));
			cut_rect_horizontal(handle, handle.size.w - handle.size.h - 6, 0, &close);
			close.size.w = close.size.h;
			DrawRectangleRounded(handle.rl, 0.1, 0, colour_from_normalized(BG_COLOUR));
			DrawRectangleRoundedLinesEx(handle.rl, 0.2, 0, 2, BLACK);
			draw_close_button(ui, fw->close, mouse, close, (v2){{0.45, 0.45}});
		} else {
			hover_interaction(ui, mouse, auto_interaction(container, var));
		}
		f32 roundness = 12.0f / fw->rect.size.y;
		DrawRectangleRounded(fw->rect.rl, roundness / 2, 0, colour_from_normalized(BG_COLOUR));
		DrawRectangleRoundedLinesEx(fw->rect.rl, roundness, 0, 2, BLACK);
	}
}

function void
draw_floating_widgets(BeamformerUI *ui, Rect window_rect, v2 mouse)
{
	for (Variable *var = ui->floating_widget_sentinal.parent;
	     var != &ui->floating_widget_sentinal;
	     var = var->parent)
	{
		assert(var->type == VT_UI_FLOATING_WIDGET);
		FloatingWidget *fw = &var->floating_widget;
		draw_floating_widget_container(ui, var, mouse, window_rect);

		switch (fw->kind) {
		case FloatingWidgetKind_Menu:{
			fw->rect = draw_menu(ui, ui->arena, fw->reference, &ui->small_font,
			                      fw->rect.pos, mouse, window_rect);
		}break;
		case FloatingWidgetKind_TextBox:{
			InputState *is = &ui->text_input_state;

			f32 cursor_width = (is->cursor == is->count) ? 0.55 * is->font->baseSize : 4;
			s8 text      = {.len = is->count, .data = is->buf};
			v2 text_size = measure_text(*is->font, text);

			f32 text_pad = 4.0f;
			f32 desired_width = text_pad + text_size.w + cursor_width;
			fw->rect.size = (v2){{MAX(desired_width, fw->rect.size.w), text_size.h}};

			v2 text_position   = {{fw->rect.pos.x + text_pad / 2, fw->rect.pos.y}};
			f32 cursor_offset  = measure_text(*is->font, (s8){is->cursor, text.data}).w;
			cursor_offset     += text_position.x;

			Rect cursor;
			cursor.pos  = (v2){{cursor_offset, text_position.y}};
			cursor.size = (v2){{cursor_width,  text_size.h}};

			v4 cursor_colour = FOCUSED_COLOUR;
			cursor_colour.a  = CLAMP01(is->cursor_blink_t);
			v4 text_colour   = lerp_v4(FG_COLOUR, HOVERED_COLOUR, fw->reference->hover_t);

			TextSpec text_spec = {.font = is->font, .colour = text_colour};
			draw_text(text, text_position, &text_spec);
			DrawRectanglePro(cursor.rl, (Vector2){0}, 0, colour_from_normalized(cursor_colour));
		}break;
		InvalidDefaultCase;
		}
	}
}

function void
scroll_interaction(Variable *var, f32 delta)
{
	switch (var->type) {
	case VT_B32:{ var->bool32  = !var->bool32; }break;
	case VT_F32:{ var->real32 += delta;        }break;
	case VT_I32:{ var->signed32 += delta;      }break;
	case VT_U32:{ var->unsigned32 += delta;    }break;
	case VT_SCALED_F32:{ var->scaled_real32.val += delta * var->scaled_real32.scale; }break;
	case VT_BEAMFORMER_FRAME_VIEW:{
		BeamformerFrameView *bv = var->generic;
		bv->needs_update      = 1;
		bv->threshold.real32 += delta;
	} break;
	case VT_BEAMFORMER_VARIABLE:{
		BeamformerVariable *bv = &var->beamformer_variable;
		switch (bv->store_type) {
		case VT_F32:{
			f32 val = *(f32 *)bv->store + delta * bv->scroll_scale;
			*(f32 *)bv->store = CLAMP(val, bv->limits.x, bv->limits.y);
		}break;
		InvalidDefaultCase;
		}
	}break;
	case VT_CYCLER:{
		*var->cycler.state += delta > 0? 1 : -1;
		*var->cycler.state %= var->cycler.cycle_length;
	}break;
	case VT_UI_VIEW:{
		var->view.offset += UI_SCROLL_SPEED * delta;
		var->view.offset  = MAX(0, var->view.offset);
	}break;
	InvalidDefaultCase;
	}
}

function void
begin_text_input(InputState *is, Rect r, Variable *container, v2 mouse)
{
	assert(container->type == VT_UI_FLOATING_WIDGET);
	Font *font = is->font = is->hot_font;
	Stream s = {.cap = countof(is->buf), .data = is->buf};
	stream_append_variable(&s, container->floating_widget.reference);
	is->count = s.widx;
	is->container = container;

	/* NOTE: extra offset to help with putting a cursor at idx 0 */
	#define TEXT_HALF_CHAR_WIDTH 10
	f32 hover_p = CLAMP01((mouse.x - r.pos.x) / r.size.w);
	f32 x_off = TEXT_HALF_CHAR_WIDTH, x_bounds = r.size.w * hover_p;
	i32 i;
	for (i = 0; i < is->count && x_off < x_bounds; i++) {
		/* NOTE: assumes font glyphs are ordered ASCII */
		i32 idx  = is->buf[i] - 0x20;
		x_off   += font->glyphs[idx].advanceX;
		if (font->glyphs[idx].advanceX == 0)
			x_off += font->recs[idx].width;
	}
	is->cursor = i;
}

function void
end_text_input(InputState *is, Variable *var)
{
	f64 value = parse_f64((s8){.len = is->count, .data = is->buf});

	switch (var->type) {
	case VT_SCALED_F32:{ var->scaled_real32.val = value; }break;
	case VT_F32:{        var->real32            = value; }break;
	case VT_BEAMFORMER_VARIABLE:{
		BeamformerVariable *bv = &var->beamformer_variable;
		switch (bv->store_type) {
		case VT_F32:{
			value = CLAMP(value / bv->display_scale, bv->limits.x, bv->limits.y);
			*(f32 *)bv->store = value;
		}break;
		InvalidDefaultCase;
		}
		var->hover_t = 0;
	}break;
	InvalidDefaultCase;
	}
}

function b32
update_text_input(InputState *is, Variable *var)
{
	assert(is->cursor != -1);

	is->cursor_blink_t += is->cursor_blink_scale * dt_for_frame;
	if (is->cursor_blink_t >= 1) is->cursor_blink_scale = -1.5f;
	if (is->cursor_blink_t <= 0) is->cursor_blink_scale =  1.5f;

	var->hover_t -= 2 * HOVER_SPEED * dt_for_frame;
	var->hover_t  = CLAMP01(var->hover_t);

	/* NOTE: handle multiple input keys on a single frame */
	for (i32 key = GetCharPressed();
	     is->count < countof(is->buf) && key > 0;
	     key = GetCharPressed())
	{
		b32 allow_key = (BETWEEN(key, '0', '9') || (key == '.') ||
		                 (key == '-' && is->cursor == 0));
		if (allow_key) {
			mem_move(is->buf + is->cursor + 1,
			         is->buf + is->cursor,
			         is->count - is->cursor);
			is->buf[is->cursor++] = key;
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
			         is->count - is->cursor - 1);
		}
		is->count--;
	}

	if ((IsKeyPressed(KEY_DELETE) || IsKeyPressedRepeat(KEY_DELETE)) && is->cursor < is->count) {
		mem_move(is->buf + is->cursor,
		         is->buf + is->cursor + 1,
		         is->count - is->cursor - 1);
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
		                                          add_v2(it->rect.pos, it->rect.size),
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

			v2_sll *savepoint = SLLPop(ui->scale_bar_savepoint_freelist);
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
			SLLPush(savepoint, ui->scale_bar_savepoint_freelist);
		}
		sb->zoom_starting_coord = F32_INFINITY;
	}

	if (mouse_wheel) {
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
ui_close_widget(BeamformerUI *ui, Variable *widget)
{
	assert(widget->type == VT_UI_FLOATING_WIDGET);
	FloatingWidget *fw = &widget->floating_widget;
	switch (fw->kind) {
	case FloatingWidgetKind_Menu:{
		assert(fw->reference->type == VT_GROUP);
		fw->reference->group.expanded  = 0;
		fw->reference->group.container = 0;
	}break;
	case FloatingWidgetKind_TextBox:{
		end_text_input(&ui->text_input_state, fw->reference);
	}break;
	InvalidDefaultCase;
	}
	widget->parent->next = widget->next;
	widget->next->parent = widget->parent;
	if (fw->close) SLLPush(fw->close, ui->variable_freelist);
	SLLPush(widget, ui->variable_freelist);
}

function void
ui_button_interaction(BeamformerUI *ui, Variable *button)
{
	assert(button->type == VT_UI_BUTTON);
	switch (button->button) {
	case UI_BID_FV_EXPORT:{ ui_export_view(ui, button->parent->parent); }break;
	case UI_BID_FV_COPY_HORIZONTAL:{
		ui_copy_frame(ui, button->parent->parent, RSD_HORIZONTAL);
	}break;
	case UI_BID_FV_COPY_VERTICAL:{
		ui_copy_frame(ui, button->parent->parent, RSD_VERTICAL);
	}break;
	case UI_BID_GM_OPEN_LIVE_VIEW_RIGHT:{
		ui_add_live_frame_view(ui, button->parent->parent, RSD_HORIZONTAL);
	}break;
	case UI_BID_GM_OPEN_LIVE_VIEW_BELOW:{
		ui_add_live_frame_view(ui, button->parent->parent, RSD_VERTICAL);
	}break;
	case UI_BID_CLOSE_WIDGET:{ ui_close_widget(ui, button->parent); }break;
	case UI_BID_CLOSE_VIEW:{
		Variable *view   = button->parent;
		Variable *region = view->parent;
		assert(view->type == VT_UI_VIEW && region->type == VT_UI_REGION_SPLIT);

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

		SLLPush(region, ui->variable_freelist);
	}break;
	}
}

function void
ui_begin_interact(BeamformerUI *ui, BeamformerInput *input, b32 scroll)
{
	Interaction *hot = &ui->hot_interaction;
	if (hot->kind != InteractionKind_None) {
		if (hot->kind == InteractionKind_Auto) {
			switch (hot->var->type) {
			case VT_NULL:{ hot->kind = InteractionKind_Nop; }break;
			case VT_B32:{ hot->kind = InteractionKind_Set; }break;
			case VT_SCALE_BAR:{ hot->kind = InteractionKind_Set; }break;
			case VT_UI_BUTTON:{ hot->kind = InteractionKind_Button; }break;
			case VT_GROUP:{ hot->kind = InteractionKind_Set; }break;
			case VT_UI_FLOATING_WIDGET:{
				FloatingWidget *fw = &hot->var->floating_widget;
				switch (fw->kind) {
				case FloatingWidgetKind_Menu:{
					hot->kind = InteractionKind_Drag;
				}break;
				case FloatingWidgetKind_TextBox:{
					hot->kind = InteractionKind_Text;
					begin_text_input(&ui->text_input_state, hot->rect, hot->var, input->mouse);
				}break;
				}
				ui_widget_bring_to_front(&ui->floating_widget_sentinal, hot->var);
			}break;
			case VT_UI_VIEW:{
				if (scroll) hot->kind = InteractionKind_Scroll;
				else        hot->kind = InteractionKind_Nop;
			}break;
			case VT_BEAMFORMER_FRAME_VIEW:{
				if (scroll) {
					hot->kind = InteractionKind_Scroll;
				} else {
					hot->kind = InteractionKind_Nop;
					BeamformerFrameView *bv = hot->var->generic;
					switch (++bv->ruler.state) {
					case RulerState_Start:{
						hot->kind = InteractionKind_Ruler;
						v2 r_max = add_v2(hot->rect.pos, hot->rect.size);
						v2 p = screen_point_to_world_2d(input->mouse, hot->rect.pos, r_max,
						                                XZ(bv->min_coordinate),
						                                XZ(bv->max_coordinate));
						bv->ruler.start = p;
					}break;
					case RulerState_Hold:{}break;
					default:{ bv->ruler.state = RulerState_None; }break;
					}
				}
			}break;
			case VT_CYCLER:{
				if (scroll) hot->kind = InteractionKind_Scroll;
				else        hot->kind = InteractionKind_Set;
			}break;
			case VT_BEAMFORMER_VARIABLE:{
				if (hot->var->beamformer_variable.store_type == VT_B32) {
					hot->kind = InteractionKind_Set;
					break;
				}
			} /* FALLTHROUGH */
			case VT_F32:
			case VT_SCALED_F32:
			{
				if (scroll) {
					hot->kind = InteractionKind_Scroll;
				} else if (hot->var->flags & V_TEXT) {
					hot->kind = InteractionKind_Text;
					Variable *w = add_floating_widget(ui, &ui->arena,
					                                  FloatingWidgetKind_TextBox,
					                                  hot->rect.pos, hot->var, 0);
					w->floating_widget.rect = hot->rect;
					begin_text_input(&ui->text_input_state, hot->rect, w, input->mouse);
				}
			}break;
			InvalidDefaultCase;
			}
		}

		ui->interaction = ui->hot_interaction;
	} else {
		ui->interaction.kind = InteractionKind_Nop;
	}
}

function void
ui_end_interact(BeamformerUI *ui, v2 mouse)
{
	Interaction *it = &ui->interaction;
	b32 start_compute = (it->var->flags & V_CAUSES_COMPUTE) != 0;
	switch (it->kind) {
	case InteractionKind_Nop:{}break;
	case InteractionKind_Drag:{}break;
	case InteractionKind_Set:{
		switch (it->var->type) {
		case VT_B32:{ it->var->bool32 = !it->var->bool32; }break;
		case VT_GROUP:{ it->var->group.expanded = !it->var->group.expanded; }break;
		case VT_SCALE_BAR:{ scale_bar_interaction(ui, &it->var->scale_bar, mouse); }break;
		case VT_CYCLER:{
			*it->var->cycler.state += 1;
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
			g->container = add_floating_widget(ui, &ui->arena, FloatingWidgetKind_Menu,
			                                   mouse, it->var, 1);
		}
	}break;
	case InteractionKind_Ruler:{
		assert(it->var->type == VT_BEAMFORMER_FRAME_VIEW);
		((BeamformerFrameView *)it->var->generic)->ruler.state = RulerState_None;
	}break;
	case InteractionKind_Button:{ ui_button_interaction(ui, it->var); }break;
	case InteractionKind_Scroll:{ scroll_interaction(it->var, GetMouseWheelMoveV().y); }break;
	case InteractionKind_Text:{ ui_close_widget(ui, ui->text_input_state.container); }break;
	InvalidDefaultCase;
	}

	if (start_compute) ui->flush_params = 1;
	if (it->var->flags & V_UPDATE_VIEW) {
		Variable *parent = it->var->parent;
		BeamformerFrameView *frame;
		/* TODO(rnp): more straight forward way of achieving this */
		if (parent->type == VT_BEAMFORMER_FRAME_VIEW) {
			frame = parent->generic;
		} else {
			assert(parent->parent->group.first->type == VT_BEAMFORMER_FRAME_VIEW);
			frame = parent->parent->group.first->generic;
		}
		frame->needs_update = 1;
	}

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
		v2 r_max = add_v2(it->rect.pos, it->rect.size);
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
			v2 dMouse = sub_v2(input->mouse, input->last_mouse);

			switch (ui->interaction.var->type) {
			case VT_UI_FLOATING_WIDGET:{
				v2 *pos = &ui->interaction.var->floating_widget.rect.pos;
				*pos = clamp_v2_rect(add_v2(*pos, dMouse), window_rect);
			}break;
			case VT_UI_REGION_SPLIT:{
				f32 min_fraction = 0;
				dMouse = mul_v2(dMouse, (v2){.x = 1.0f / ws.w, .y = 1.0f / ws.h});
				RegionSplit *rs = &ui->interaction.var->region_split;
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
			}break;
			default:{}break;
			}
		}
	} break;
	default:{ ui_end_interact(ui, input->mouse); }break;
	}

	ui->next_interaction = (Interaction){.kind = InteractionKind_None};
}

function void
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
			if (view->texture)
				glDeleteTextures(1, &view->texture);
	}

	ui = ctx->ui = push_struct(&store, typeof(*ui));
	ui->os    = &ctx->os;
	ui->arena = store;
	ui->frame_view_render_context = &ctx->frame_view_render_context;

	/* TODO: build these into the binary */
	/* TODO(rnp): better font, this one is jank at small sizes */
	ui->font       = LoadFontEx("assets/IBMPlexSans-Bold.ttf", 28, 0, 0);
	ui->small_font = LoadFontEx("assets/IBMPlexSans-Bold.ttf", 20, 0, 0);

	ui->floating_widget_sentinal.parent = &ui->floating_widget_sentinal;
	ui->floating_widget_sentinal.next   = &ui->floating_widget_sentinal;

	Variable *split = ui->regions = add_ui_split(ui, 0, &ui->arena, s8("UI Root"), 0.4,
	                                             RSD_HORIZONTAL, ui->font);
	split->region_split.left    = add_ui_split(ui, split, &ui->arena, s8(""), 0.475,
	                                           RSD_VERTICAL, ui->font);
	split->region_split.right   = add_beamformer_frame_view(ui, split, &ui->arena, FVT_LATEST, 0);

	ui_fill_live_frame_view(ui, split->region_split.right->view.child->generic);

	split = split->region_split.left;
	split->region_split.left  = add_beamformer_parameters_view(split, ctx);
	split->region_split.right = add_ui_split(ui, split, &ui->arena, s8(""), 0.22,
	                                         RSD_VERTICAL, ui->font);
	split = split->region_split.right;

	split->region_split.left  = add_compute_progress_bar(split, ctx);
	split->region_split.right = add_compute_stats_view(ui, split, &ui->arena,
	                                                   VT_COMPUTE_LATEST_STATS_VIEW);

	ComputeStatsView *compute_stats = &split->region_split.right->group.first->compute_stats_view;
	compute_stats->ctx   = ctx;
	compute_stats->stats = &ui->latest_compute_stats;

	ctx->ui_read_params = 1;

	/* NOTE(rnp): shrink variable size once this fires */
	assert(ui->arena.beg - (u8 *)ui < KB(64));
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
draw_ui(BeamformerCtx *ctx, BeamformerInput *input, BeamformFrame *frame_to_draw, ImagePlaneTag frame_plane,
        ComputeShaderStats *latest_compute_stats)
{
	BeamformerUI *ui = ctx->ui;
	BeamformerSharedMemory *sm = ctx->shared_memory.region;

	ui->latest_plane[IPT_LAST]    = frame_to_draw;
	ui->latest_plane[frame_plane] = frame_to_draw;
	ui->latest_compute_stats      = latest_compute_stats;

	/* TODO(rnp): there should be a better way of detecting this */
	if (ctx->ui_read_params) {
		mem_copy(&ui->params, &sm->parameters.output_min_coordinate, sizeof(ui->params));
		ui->flush_params    = 0;
		ctx->ui_read_params = 0;
	}

	/* NOTE: process interactions first because the user interacted with
	 * the ui that was presented last frame */
	Rect window_rect = {.size = {.w = ctx->window_size.w, .h = ctx->window_size.h}};
	ui_interact(ui, input, window_rect);

	if (ui->flush_params) {
		i32 lock = BeamformerSharedMemoryLockKind_Parameters;
		validate_ui_parameters(&ui->params);
		if (ctx->os.shared_memory_region_lock(&ctx->shared_memory, sm->locks, lock, 0)) {
			mem_copy(&sm->parameters_ui, &ui->params, sizeof(ui->params));
			ui->flush_params = 0;
			atomic_or_u32(&sm->dirty_regions, (1 << (lock - 1)));
			b32 dispatch = ctx->os.shared_memory_region_lock(&ctx->shared_memory, sm->locks,
			                                                 BeamformerSharedMemoryLockKind_DispatchCompute,
			                                                 0);
			sm->start_compute_from_main |= dispatch &
			                               ctx->beamform_frames[ctx->display_frame_index].ready_to_present;
			ctx->os.shared_memory_region_unlock(&ctx->shared_memory, sm->locks, lock);
		}
	}

	/* NOTE(rnp): can't render to a different framebuffer in the middle of BeginDrawing()... */
	update_frame_views(ui, window_rect);

	BeginDrawing();
		f32 one = 1;
		glClearNamedFramebufferfv(0, GL_COLOR, 0, BG_COLOUR.E);
		glClearNamedFramebufferfv(0, GL_DEPTH, 0, &one);

		draw_ui_regions(ui, window_rect, input->mouse);
		draw_floating_widgets(ui, window_rect, input->mouse);
	EndDrawing();
}
