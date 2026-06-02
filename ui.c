/* See LICENSE for license details. */
/* TODO(rnp):
 * [ ]: rearranging
 *      - dragging implemented, still needs overlay and drop target tab group indicator
 * [ ]: parameter listing settings
 * [ ]: add panel_kind has parameters b8
 * [ ]: bug: cursor when input length is 0
 * [ ]: bug: ui_panel_group_equip() needs extra tab bar clip region
 * [ ]: rest of input shortcuts (cut/copy/paste, word scan)
 * [ ]: scale bar interactions need hooking up
 * [ ]: compute bar view
 * [ ]: copy frame view
 * [ ]: x-plane view
 * [ ]: animation state
 * [ ]: tooltips
 * [ ]: auto launch for live parameters
 * [ ]: refactor: can the scroll container just use the ViewScroll flags like the tab bar?
 * [ ]: refactor: ui_build_frame_view, ui_custom_draw_frame_view have too much duplicated code
 *      - draw shouldn't need most of that info since the size should already be determined
 * [*]: bug: shrinking the region less than the title bar should be disallowed
 * [*]: overlay on frame view
 * [*]: bug: scissor size can be invalid
 * [x]: bug: sticky interaction state on node kill
 * [x]: tab node kill
 * [x]: tab nodes
 * [x]: title bars
 * [x]: bug: nan on scale bar when view is small
 * [x]: refactor: scale bars need to be custom draws
 * [x]: bug: lateral bounding box
 * [x]: lateral scale bar (rotated text)
 * [x]: bug: 2D beamform plane transform
 * [x]: prune unused ui nodes
 * [x]: bug: don't push scrollbar when there isn't enough space
 * [x]: bug: scrollbar drag offset calculation
 * [x]: refactor: push left/right children for panel node container in build_regions()
 * [x]: horizontal scroll bars
 * [x]: vertical scroll bars
 * [x]: refactor: ui_scrollable_container(scroll_delta_x, scroll_delta_y)
 * [x]: bug: alignment of whole `{...}` item in parameters listing
 * [x]: bug: filter signal from lower nodes
 *
 * [ ]: command window
 * [ ]: 3D data view
 *    - add extra view controls, change view without recompute
 *      - to start just have starting plane/normal, plane uvs, rotation, and offset
 *    - confirmation on recompute
 * [ ]: bug: cross-plane view with different dimensions for each plane
 *
 * [ ]: multi-os windows
 * [ ]: ui color configuration at runtime
 */

#include "assets/generated/assets.c"

#define NIL_COLOUR             (v4){{0.76f, 0.00f, 0.65f, 1.0f}}
#define BG_COLOUR              (v4){{0.15f, 0.12f, 0.13f, 1.0f}}
#define FG_COLOUR              (v4){{0.92f, 0.88f, 0.78f, 1.0f}}
#define FOCUSED_COLOUR         (v4){{0.86f, 0.28f, 0.21f, 1.0f}}
#define HOVERED_COLOUR         (v4){{0.11f, 0.50f, 0.59f, 1.0f}}
#define SELECTION_COLOUR       (v4){{0.07f, 0.37f, 0.90f, 0.5f}}
#define RULER_COLOUR           (v4){{1.00f, 0.70f, 0.00f, 1.0f}}
#define BORDER_COLOUR          v4_lerp(FG_COLOUR, BG_COLOUR, 0.85f)

#define FRAME_VIEW_BB_COLOUR          (v4){{0.92f, 0.88f, 0.78f, 1.0f}}
#define FRAME_VIEW_BB_FRACTION        0.007f
#define FRAME_VIEW_RENDER_TARGET_SIZE 1024, 1024

#define MENU_PLUS_COLOUR       (v4){{0.33f, 0.42f, 1.00f, 1.00f}}
#define MENU_CLOSE_COLOUR      FOCUSED_COLOUR

#define UI_NODE_PAD         8.f
#define UI_BORDER_THICK     4.f

#define UI_HASH_TABLE_COUNT 4096

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

#define RULER_TEXT_PAD          6.0f
#define RULER_TICK_LENGTH      20.0f

#define UI_SPLIT_HANDLE_THICK  8.0f
#define UI_REGION_PAD          32.0f

/* TODO(rnp) smooth scroll */
#define UI_SCROLL_SPEED 12.0f

#define LISTING_LINE_PAD    6.0f
#define TITLE_BAR_PAD       6.0f

typedef enum {
	UINodeFlag_MouseClickable            = 1ull << 0,
	UINodeFlag_KeyboardClickable         = 1ull << 1,
	UINodeFlag_DropSite                  = 1ull << 2,
	UINodeFlag_ClickToFocus              = 1ull << 3,
	UINodeFlag_Scroll                    = 1ull << 4,
	UINodeFlag_FocusHot                  = 1ull << 5,
	UINodeFlag_FocusActive               = 1ull << 6,
	UINodeFlag_FocusHotDisabled          = 1ull << 7,
	UINodeFlag_FocusActiveDisabled       = 1ull << 8,
	UINodeFlag_Disabled                  = 1ull << 9,

	UINodeFlag_FloatingX                 = 1ull << 10,
	UINodeFlag_FloatingY                 = 1ull << 11,
	UINodeFlag_FixedWidth                = 1ull << 12,
	UINodeFlag_FixedHeight               = 1ull << 13,
	UINodeFlag_AllowOverflowX            = 1ull << 14,
	UINodeFlag_AllowOverflowY            = 1ull << 15,

	// NOTE(rnp): for scrollable containers
	UINodeFlag_ViewScrollX               = 1ull << 16,
	UINodeFlag_ViewScrollY               = 1ull << 17,

	UINodeFlag_DrawDropShadow            = 1ull << 18,
	UINodeFlag_DrawBackgroundBlur        = 1ull << 19,
	UINodeFlag_DrawBackground            = 1ull << 20,
	UINodeFlag_DrawBorder                = 1ull << 21,
	UINodeFlag_DrawText                  = 1ull << 22,
	UINodeFlag_DrawHotEffects            = 1ull << 23,
	UINodeFlag_DrawActiveEffects         = 1ull << 24,
	UINodeFlag_DrawOverlay               = 1ull << 25,
	UINodeFlag_Clip                      = 1ull << 26,
	UINodeFlag_DisableTextTrunc          = 1ull << 27,
	UINodeFlag_DisableFocusBorder        = 1ull << 28,
	UINodeFlag_DisableFocusOverlay       = 1ull << 29,

	UINodeFlag_TextInput                 = 1ull << 30,
	UINodeFlag_TextInputNumeric          = 1ull << 31,

	UINodeFlag_CustomDraw                = 1ull << 32,

	// TODO(rnp): hack: when text is not drawn with raylib do something smarter
	UINodeFlag_IconText                  = 1ull << 33,

	UINodeFlag_Clickable           = UINodeFlag_MouseClickable|UINodeFlag_KeyboardClickable,
	UINodeFlag_Floating            = UINodeFlag_FloatingX|UINodeFlag_FloatingY,
	UINodeFlag_FixedSize           = UINodeFlag_FixedWidth|UINodeFlag_FixedHeight,
	UINodeFlag_AllowOverflow       = UINodeFlag_AllowOverflowX|UINodeFlag_AllowOverflowY,
	UINodeFlag_DisableFocusEffects = UINodeFlag_DisableFocusBorder|UINodeFlag_DisableFocusOverlay,
	UINodeFlag_ViewScroll          = UINodeFlag_ViewScrollX|UINodeFlag_ViewScrollY,
} UINodeFlags;

typedef struct UINodeFlagsNode UINodeFlagsNode;
struct UINodeFlagsNode {UINodeFlagsNode *next; UINodeFlags v;};

typedef enum {
	Axis2_X = 0,
	Axis2_Y = 1,
	Axis2_Count,
} Axis2;

typedef struct Axis2Node Axis2Node;
struct Axis2Node {Axis2Node *next; Axis2 v;};

typedef enum {
	UISizeKind_Nil,
	UISizeKind_Pixels,
	UISizeKind_TextContent,
	UISizeKind_PercentOfParent,
	UISizeKind_ChildrenSum,
} UISizeKind;

typedef struct {
	UISizeKind kind;
	f32        value;
	f32        strictness;
} UISize;

typedef struct UISizeNode UISizeNode;
struct UISizeNode {UISizeNode *next; UISize v;};

typedef enum {
	UIAlign_Left,
	UIAlign_Right,
	UIAlign_Center,
	UIAlign_Count,
} UIAlign;

typedef struct UIAlignNode UIAlignNode;
struct UIAlignNode {UIAlignNode *next; UIAlign v;};

typedef struct {u64 value;} UINodeKey;

typedef struct UINode UINode;

#define UI_CUSTOM_DRAW_FUNCTION(name) void name(UINode *node, Rect node_rect)
typedef UI_CUSTOM_DRAW_FUNCTION(UICustomDrawFunction);

struct UINode {
	UINode *parent;
	UINode *first_child;
	UINode *last_child;
	UINode *previous_sibling;
	UINode *next_sibling;

	u32     child_count;

	UINodeFlags flags;
	str8        string;
	// NOTE(rnp): desired sizing info from build step
	union {
		struct {
			UISize semantic_width;
			UISize semantic_height;
		};
		UISize semantic_size[Axis2_Count];
	};

	union {
		struct {
			UIAlign alignment_x;
			UIAlign alignment_y;
		};
		UIAlign alignment[Axis2_Count];
	};

	UIAlign    text_alignment;

	Axis2      child_layout_axis;
	f32        font_size;

	u64        last_frame_active_index;
	UINodeKey  key;
	UINode    *hash_prev;
	UINode    *hash_next;

	// NOTE(rnp): recomputed every frame before drawing. also
	// used on next frame for mouse collision detection.
	f32  computed_position[Axis2_Count];
	f32  computed_size[Axis2_Count];

	v2   text_size;

	// NOTE(rnp): persistent data
	f32 active_t;
	f32 hot_t;

	v2  view_scroll_offset;

	v4  bg_colour;

	v4  text_colour;
	v4  text_outline_colour;
	f32 text_outline_thickness;

	v4  border_colour;
	f32 border_thickness;

	UICustomDrawFunction *custom_draw_function;
	void                 *custom_draw_context;
};

typedef struct {UINode *first, *last;} UINodeHashBucket;

typedef struct UIParentNode UIParentNode;
struct UIParentNode {UIParentNode *next; UINode *v;};

typedef enum {
	UIMouseButtonKind_Left,
	UIMouseButtonKind_Middle,
	UIMouseButtonKind_Right,
	UIMouseButtonKind_Count,
} UIMouseButtonKind;

typedef enum {
	UISignalFlag_LeftPressed          = (1 << 0),
	UISignalFlag_MiddlePressed        = (1 << 1),
	UISignalFlag_RightPressed         = (1 << 2),

	UISignalFlag_LeftDragging         = (1 << 3),
	UISignalFlag_MiddleDragging       = (1 << 4),
	UISignalFlag_RightDragging        = (1 << 5),

	UISignalFlag_LeftDoubleDragging   = (1 << 6),
	UISignalFlag_MiddleDoubleDragging = (1 << 7),
	UISignalFlag_RightDoubleDragging  = (1 << 8),

	UISignalFlag_LeftTripleDragging   = (1 << 9),
	UISignalFlag_MiddleTripleDragging = (1 << 10),
	UISignalFlag_RightTripleDragging  = (1 << 11),

	UISignalFlag_LeftReleased         = (1 << 12),
	UISignalFlag_MiddleReleased       = (1 << 13),
	UISignalFlag_RightReleased        = (1 << 14),

	UISignalFlag_LeftClicked          = (1 << 15),
	UISignalFlag_MiddleClicked        = (1 << 16),
	UISignalFlag_RightClicked         = (1 << 17),

	UISignalFlag_LeftDoubleClicked    = (1 << 18),
	UISignalFlag_MiddleDoubleClicked  = (1 << 19),
	UISignalFlag_RightDoubleClicked   = (1 << 20),

	UISignalFlag_LeftTripleClicked    = (1 << 21),
	UISignalFlag_MiddleTripleClicked  = (1 << 22),
	UISignalFlag_RightTripleClicked   = (1 << 23),

	UISignalFlag_ScrolledX            = (1 << 24),
	UISignalFlag_ScrolledY            = (1 << 25),

	UISignalFlag_KeyboardPressed      = (1 << 26),

	UISignalFlag_Hovering             = (1 << 27),

	UISignalFlag_TextCommit           = (1 << 28),

	UISignalFlag_Scrolled             = UISignalFlag_ScrolledX|UISignalFlag_ScrolledY,
	UISignalFlag_Pressed              = UISignalFlag_LeftPressed|UISignalFlag_KeyboardPressed,
	UISignalFlag_Released             = UISignalFlag_LeftReleased,
	UISignalFlag_Clicked              = UISignalFlag_LeftClicked|UISignalFlag_KeyboardPressed,
	UISignalFlag_DoubleClicked        = UISignalFlag_LeftDoubleClicked,
	UISignalFlag_TripleClicked        = UISignalFlag_LeftTripleClicked,
	UISignalFlag_Dragging             = UISignalFlag_LeftDragging,
} UISignalFlags;

typedef struct {
	UINode        *node;
	v2             scroll;
	str8           string;
	UISignalFlags  flags;
} UISignal;

typedef enum {
	RulerState_None,
	RulerState_Start,
	RulerState_Hold,
	RulerState_Count,
} RulerState;

typedef struct {
	v3 start;
	v3 end;
	RulerState state;
} Ruler;

#define BEAMFORMER_FRAME_VIEW_KIND_LIST \
	X(Latest,   "Latest")     \
	X(3DXPlane, "3D X-Plane") \
	X(Copy,     "Copy")       \

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

	// NOTE(rnp): for FrameViewKindCopy
	GPUBuffer copy_buffer;

	GPUImage colour_image;
	// NOTE(rnp): temporary, on w32 we must hold onto this when importing vulkan data to OpenGL
	OSHandle export_handle;
	u32      memory_object;
	u32      texture;

	b32 log_scale;
	f32 threshold;
	f32 dynamic_range;
	f32 gamma;

	union {
		/* BeamformerFrameViewKind_Latest/BeamformerFrameViewKind_Copy */
		struct {
			b32   scale_bar_active[2];
			Ruler ruler;
			BeamformerViewPlaneTag view_plane;
			BeamformerFrame frame;
		};

		/* BeamformerFrameViewKind_3DXPlane */
		struct {
			f32 x_plane_shifts[2];
			f32 rotation;
			b32 demo;
			v3  hit_test_point;
		};
	};

	BeamformerFrameView *prev, *next;
};

typedef struct {
	f32 t;
	f32 scale;
} UIBlinker;

typedef struct BeamformerUI BeamformerUI;
typedef struct Variable     Variable;

typedef struct {
	UINodeKey node_key;
	UINodeKey next_node_key;
	UINodeKey last_node_key;

	i16       cursor;
	i16       mark;
	i16       count;
	i16       last_count;
	b32       numeric;
	b32       changed;
	// TODO(rnp): animation key
	UIBlinker blinker;
	u8        buffer[256];
	u8        last_buffer[256];
} UITextInputState;

typedef struct UITreeNode UITreeNode;
struct UITreeNode {
	UITreeNode *parent;
	UITreeNode *last_child;
	UITreeNode *first_child;
	UITreeNode *previous_sibling;
	UITreeNode *next_sibling;

	u64         child_count;

	BeamformerPanelKind kind;
	union {
		BeamformerFrameView *frame_view;

		UITreeNode *tab_focus;

		struct {
			b32       bar_graph;
			UIBlinker broken_shader_blinker;
		} compute_stats;

		struct {
			u32 parameter_block;
			b16 expand_coordinate[2];
		} parameter_listing;

		struct {
			Axis2 axis;
			f32   fraction;
		} split;
	} u;
};

typedef struct {
	ComputeShaderStats *compute_shader_stats;
	UIBlinker blink;
} ComputeStatsView;

typedef enum {
	VT_NULL,
	VT_B32,
	VT_F32,
	VT_I32,
	VT_U32,
	VT_GROUP,
	VT_CYCLER,
	VT_BEAMFORMER_VARIABLE,
	VT_BEAMFORMER_FRAME_VIEW,
	VT_LIVE_CONTROLS_VIEW,
	VT_LIVE_CONTROLS_STRING,
	VT_UI_BUTTON,
	VT_UI_MENU,
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
	Rect      rect;
	UIViewFlags flags;
} UIView;
#define X(id, text) UI_BID_ ##id,
typedef enum {
	UI_BID_ACQUISITION_KIND_FIRST,
	UI_BID_ACQUISITION_KIND_LAST = UI_BID_ACQUISITION_KIND_FIRST + BeamformerAcquisitionKind_Count - 1,
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
	V_EXTRA_ACTION   = 1 << 1,
	V_HIDES_CURSOR   = 1 << 2,
	V_LIVE_CONTROL   = 1 << 28,
	V_CAUSES_COMPUTE = 1 << 29,
	V_UPDATE_VIEW    = 1 << 30,
} VariableFlags;

struct Variable {
	s8 name;
	union {
		void               *generic;
		BeamformerVariable  beamformer_variable;
		UIButtonID          button;
		UIView              view;
		VariableCycler      cycler;
		VariableGroup       group;
		XPlaneShift         x_plane_shift;
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

typedef struct BeamformerLiveControlsView BeamformerLiveControlsView;
struct BeamformerLiveControlsView {
	Variable acquisition_menu;
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
	InteractionKind_AcquisitionMenu,
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

typedef struct F32Node F32Node;
struct F32Node {F32Node *next; f32 v;};

typedef struct V4Node V4Node;
struct V4Node {V4Node *next; v4 v;};

#define UI_STACK_LIST \
	X(Axis2Node,       child_layout_axis,      Axis2,       0) \
	X(F32Node,         font_size,              f32,         0) \
	X(F32Node,         border_thickness,       f32,         UI_BORDER_THICK) \
	X(F32Node,         text_outline_thickness, f32,         0) \
	X(UINodeFlagsNode, flags,                  UINodeFlags, 0) \
	X(UIParentNode,    parent,                 UINode *,    (&ui_node_nil)) \
	X(UISizeNode,      semantic_height,        UISize,      {0}) \
	X(UISizeNode,      semantic_width,         UISize,      {0}) \
	X(UIAlignNode,     alignment_y,            UIAlign,     0) \
	X(UIAlignNode,     alignment_x,            UIAlign,     0) \
	X(UIAlignNode,     text_alignment,         UIAlign,     UIAlign_Left) \
	X(V4Node,          text_colour,            v4,          FG_COLOUR) \
	X(V4Node,          text_outline_colour,    v4,          NIL_COLOUR) \
	X(V4Node,          border_colour,          v4,          NIL_COLOUR) \
	X(V4Node,          bg_colour,              v4,          NIL_COLOUR) \

struct BeamformerUI {
	u64   current_frame_index;
	Arena arena;

	v2    current_mouse;
	v2    last_mouse;
	u64   input_consumed[countof(((BeamformerInput *)0)->event_queue) / 64];
	static_assert(countof(((BeamformerInput *)0)->event_queue) % 64 == 0, "");

	Font font;
	Font small_font;

	Variable *variable_freelist;

	Variable floating_widget_sentinal;

	BeamformerFrameView *view_first;
	BeamformerFrameView *view_last;
	BeamformerFrameView *view_freelist;

	Interaction interaction;
	Interaction hot_interaction;
	Interaction next_interaction;

	VulkanHandle    pipelines[BeamformerShaderKind_RenderCount];

	OSHandle        render_semaphores_export[2];
	VulkanHandle    render_semaphores[2];
	u32             render_semaphores_gl[2];

	GPUImage        render_3d_image;
	GPUImage        render_3d_depth_image;
	RenderModel     unit_cube_model;

	BeamformerFrame latest_plane[BeamformerViewPlaneTag_Count + 1];
	b32             latest_plane_valid[BeamformerViewPlaneTag_Count + 1];

	BeamformerUIParameters parameters;
	b32                    flush_parameters;
	u32 selected_parameter_block;

	// TODO(rnp): this should be per parameter block
	f32 off_axis_position;
	f32 beamform_plane;

	BeamformerSharedMemory *shared_memory;
	BeamformerCtx          *beamformer_context;

	UITreeNode      *tree;
	UITreeNode      *tree_node_freelist;

	// NOTE(rnp): context menu
	UINode          *context_menu_root;
	UINodeKey        context_menu_anchor_key;
	UINodeKey        context_menu_next_anchor_key;
	UITreeNode      *context_menu_panel;
	UITreeNode      *context_menu_next_panel;
	f32              context_menu_open_t;
	b32              context_menu_state_changed;

	// NOTE(rnp): drag info
	UINodeKey        drop_target_key;
	UINode          *drag_root;
	UINode          *drag_overlay_root;
	UITreeNode      *drag_panel;
	f32              drag_open_t;

	// NOTE(rnp): User Interaction
	UINodeKey        hot_node_key;
	UINodeKey        active_node_key[UIMouseButtonKind_Count];
	// TODO(rnp): click timestamp history (double/triple press)

	// NOTE(rnp): Builder State
	UINode          *node_freelist;
	UINode          *root_node;
	Arena            build_arenas[2];
	TempArena        build_arena_savepoints[2];
	// NOTE(rnp): Builder Stacks
	#define X(type, name, ...) struct {type *top; type *free; u64 count;} name##_node_stack;
	UI_STACK_LIST
	#undef X

	UINodeHashBucket node_hash_table[UI_HASH_TABLE_COUNT];

	UITextInputState text_input_state;
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
	da_count  count;
	da_count  capacity;

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
	da_count         count;
	da_count         capacity;
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

global BeamformerUI    *ui_context;
global BeamformerInput *beamformer_input;

read_only global UINode ui_node_nil = {
	.parent           = &ui_node_nil,
	.first_child      = &ui_node_nil,
	.last_child       = &ui_node_nil,
	.previous_sibling = &ui_node_nil,
	.next_sibling     = &ui_node_nil,
};

#define X(type, name, _t, impl) read_only global type ui_##name##_node_nil = {.v = impl};
UI_STACK_LIST
#undef X

#define ui_node_is_nil(n) ((n) == 0 || (n) == &ui_node_nil)
#define ui_build_arena()  (ui_context->build_arenas + (ui_context->current_frame_index % countof(ui_context->build_arenas)))

#define UIStackPushBody(name_upper, name_lower, type, new_value) \
	name_upper *node = SLLPop(ui_context->name_lower##_node_stack.free, next); \
	if (!node) node = push_struct_no_zero(ui_build_arena(), name_upper); \
	node->v = new_value; \
	type result = ui_context->name_lower##_node_stack.top->v; \
	SLLStackPush(ui_context->name_lower##_node_stack.top, node, next); \
	ui_context->name_lower##_node_stack.count++; \
	return result

#define UIStackPopBody(name_upper, name_lower, type) \
	name_upper *node = ui_context->name_lower##_node_stack.top; \
	type result = node->v; \
	if (node != &ui_##name_lower##_node_nil) { \
		node = SLLPop(ui_context->name_lower##_node_stack.top, next); \
		SLLStackPush(ui_context->name_lower##_node_stack.free, node, next); \
	} \
	return result

#define UIAlign(v)                DeferLoop(ui_push_alignment(UIAlign_##v), ui_pop_alignment())
#define UIAxisAlign(axis, v)      DeferLoop(ui_push_axis_alignment(axis, UIAlign_##v), ui_pop_axis_alignment(axis))
#define UIAxisSize(axis, v)       DeferLoop(ui_push_axis_size(axis, v), ui_pop_axis_size(axis))
#define UIBorderColour(v)         DeferLoop(ui_push_border_colour(v), ui_pop_border_colour())
#define UIBorderThickness(v)      DeferLoop(ui_push_border_thickness(v), ui_pop_border_thickness())
#define UIBGColour(v)             DeferLoop(ui_push_bg_colour(v), ui_pop_bg_colour())
#define UIChildLayoutAxis(v)      DeferLoop(ui_push_child_layout_axis(v), ui_pop_child_layout_axis())
#define UIFlags(v)                DeferLoop(ui_push_flags(v), ui_pop_flags())
#define UIFontSize(v)             DeferLoop(ui_push_font_size(v), ui_pop_font_size())
#define UIParent(v)               DeferLoop(ui_push_parent(v), ui_pop_parent())
#define UIPrefHeight(v)           DeferLoop(ui_push_semantic_height(v), ui_pop_semantic_height())
#define UIPrefWidth(v)            DeferLoop(ui_push_semantic_width(v), ui_pop_semantic_width())
#define UISize(v)                 DeferLoop(ui_push_size(v), ui_pop_size())
#define UITextAlign(v)            DeferLoop(ui_push_text_alignment(UIAlign_##v), ui_pop_text_alignment())
#define UITextOutlineColour(v)    DeferLoop(ui_push_text_outline_colour(v), ui_pop_text_outline_colour())
#define UITextOutlineThickness(v) DeferLoop(ui_push_text_outline_thickness(v), ui_pop_text_outline_thickness())
#define UITextColour(v)           DeferLoop(ui_push_text_colour(v), ui_pop_text_colour())

#define UIScroll()                DeferLoop(ui_scroll_begin(), ui_scroll_end())

#define X(type, name, value_type, ...) \
	function value_type ui_push_##name(value_type v) {UIStackPushBody(type, name, value_type, v);} \
	function value_type ui_pop_##name(void)          {UIStackPopBody(type, name, value_type);} \
	function value_type ui_top_##name(void)          {return ui_context->name##_node_stack.top->v;}
UI_STACK_LIST
#undef X

#define ui_size(k, v, s) (UISize){.kind = UISizeKind_##k, .value = (v), .strictness = (s)}
#define ui_em(value, strictness)         ui_size(Pixels, (value) * ui_top_font_size(), (strictness))
#define ui_px(value, strictness)         ui_size(Pixels, (value), (strictness))
#define ui_pct(value, strictness)        ui_size(PercentOfParent, (value), (strictness))
#define ui_children_sum(strictness)      ui_size(ChildrenSum, 0.f, (strictness))
#define ui_text_dim(padding, strictness) ui_size(TextContent, (padding), (strictness))

#define ui_node_key_zero() (UINodeKey){0}

#define ui_spacer(flags) ui_build_node_from_key(flags, ui_node_key_zero())
#define ui_pad()   ui_build_node_from_key(0, ui_node_key_zero())
#define ui_padw(v) UIPrefWidth(ui_px(v, 1.f))  ui_pad()
#define ui_padh(v) UIPrefHeight(ui_px(v, 1.f)) ui_pad()
#define ui_pads(v) UISize(ui_px(v, 1.f))       ui_pad()

#define axis2_flip(v) (!(v))

#define ui_dragging(s) (!!(s.flags & UISignalFlag_Dragging))
#define ui_released(s) (!!(s.flags & UISignalFlag_Released))
#define ui_pressed(s)  (!!(s.flags & UISignalFlag_Pressed))

function UIAlign
ui_push_axis_alignment(Axis2 axis, UIAlign v)
{
	UIAlign result = 0;
	switch (axis) {
	case Axis2_X:{result = ui_push_alignment_x(v);}break;
	case Axis2_Y:{result = ui_push_alignment_y(v);}break;
	InvalidDefaultCase;
	}
	return result;
}

function UIAlign
ui_pop_axis_alignment(Axis2 axis)
{
	UIAlign result = 0;
	switch (axis) {
	case Axis2_X:{result = ui_pop_alignment_x();}break;
	case Axis2_Y:{result = ui_pop_alignment_y();}break;
	InvalidDefaultCase;
	}
	return result;
}

function UIAlign
ui_push_alignment(UIAlign v)
{
	UIAlign result = ui_push_axis_alignment(ui_top_child_layout_axis(), v);
	return result;
}

function UIAlign
ui_pop_alignment(void)
{
	UIAlign result = ui_pop_axis_alignment(ui_top_child_layout_axis());
	return result;
}

function UISize
ui_push_axis_size(Axis2 axis, UISize v)
{
	UISize result = {0};
	switch (axis) {
	case Axis2_X:{result = ui_push_semantic_width(v); }break;
	case Axis2_Y:{result = ui_push_semantic_height(v);}break;
	InvalidDefaultCase;
	}
	return result;
}

function UISize
ui_pop_axis_size(Axis2 axis)
{
	UISize result = {0};
	switch (axis) {
	case Axis2_X:{result = ui_pop_semantic_width(); }break;
	case Axis2_Y:{result = ui_pop_semantic_height();}break;
	InvalidDefaultCase;
	}
	return result;
}

function UISize
ui_push_size(UISize v)
{
	UISize result = ui_push_axis_size(ui_top_child_layout_axis(), v);
	return result;
}

function UISize
ui_pop_size(void)
{
	UISize result = ui_pop_axis_size(ui_top_child_layout_axis());
	return result;
}

#define ui_node_key_nil(a) ui_node_key_equal((a), ui_node_key_zero())
function b32
ui_node_key_equal(UINodeKey a, UINodeKey b)
{
	b32 result = a.value == b.value;
	return result;
}

function UINodeKey
ui_node_ancestor_key(void)
{
	UINode *node = ui_top_parent();
	while (!ui_node_is_nil(node) && ui_node_key_equal(node->key, ui_node_key_zero()))
		node = node->parent;
	UINodeKey result = node->key;
	return result;
}

function Rect
ui_node_rect(UINode *node)
{
	Rect result = {0};
	result.size = (v2){{node->computed_size[0], node->computed_size[1]}};
	result.pos  = (v2){{node->computed_position[0], node->computed_position[1]}};
	return result;
}

function f32
ui_alignment_correction(UIAlign alignment, f32 delta)
{
	f32 result = 0;
	switch (alignment) {
	InvalidDefaultCase;
	case UIAlign_Left:{  result = 0;           }break;
	case UIAlign_Center:{result = 0.5f * delta;}break;
	case UIAlign_Right:{ result = delta;       }break;
	}
	return result;
}

function v2
ui_node_text_position(UINode *node)
{
	Rect r = ui_node_rect(node);
	v2 result = r.pos;
	result.x += ui_alignment_correction(node->text_alignment, r.size.x - node->text_size.x);
	result.y += (r.size.y - node->text_size.y) / 2.f;
	return result;
}

function Vector2
rl_v2(v2 a)
{
	Vector2 result = {a.x, a.y};
	return result;
}

function Rectangle
rl_rect(Rect a)
{
	Rectangle result = {a.pos.x, a.pos.y, a.size.w, a.size.h};
	return result;
}

function BeamformerViewPlaneTag
ui_plane_layout_from_normal(v3 normal)
{
	BeamformerViewPlaneTag result = BeamformerViewPlaneTag_Arbitrary;
	b32 has_x = !f32_equal(normal.x, 0.0f);
	b32 has_y = !f32_equal(normal.y, 0.0f);
	b32 has_z = !f32_equal(normal.z, 0.0f);
	if ((has_x + has_y + has_z) == 1) {
		if (has_x) result = BeamformerViewPlaneTag_YZ;
		if (has_y) result = BeamformerViewPlaneTag_XZ;
		if (has_z) result = BeamformerViewPlaneTag_XY;
		assert(result != BeamformerViewPlaneTag_Arbitrary);
	}
	return result;
}

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
measure_text_tight(Font font, str8 text)
{
	v2 result = {0};
	for (i64 i = 0; i < text.length; i++) {
		assert(text.data[i] >= 0x20);
		u8 glyph = text.data[i] - 0x20;
		result.x += font.recs[glyph].width;
		result.y  = Max(font.recs[glyph].height, result.y);
	}
	return result;
}

function v2
measure_text(Font font, str8 text)
{
	v2 result = {.y = (f32)font.baseSize};
	for (i64 i = 0; i < text.length; i++)
		result.x += measure_glyph(font, text.data[i]).x;
	return result;
}

function str8
clamp_text_to_width(Font font, str8 text, f32 limit)
{
	str8 result = text;
	f32  width  = 0;
	for (i64 i = 0; i < text.length; i++) {
		f32 next = measure_glyph(font, text.data[i]).w;
		if (width + next > limit) {
			result.length = i;
			break;
		}
		width += next;
	}
	return result;
}

function v2
align_text_in_rect(str8 text, Rect r, Font font)
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
	result.id      = v->texture;
	result.width   = v->colour_image.width;
	result.height  = v->colour_image.height;
	result.mipmaps = v->colour_image.mip_map_levels;
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

function str8
push_acquisition_kind(Arena *arena, BeamformerAcquisitionKind kind, u32 transmit_count, BeamformerContrastMode contrast_mode)
{
	str8 name           = str8("Invalid");
	b32 fixed_transmits = 0;
	if Between(kind, 0, BeamformerAcquisitionKind_Count - 1) {
		name            = beamformer_acquisition_kind_strings[kind];
		fixed_transmits = beamformer_acquisition_kind_has_fixed_transmits[kind];
	}

	Stream sb = arena_stream(*arena);
	stream_append_s8(&sb, s8_from_str8(name));
	if (!fixed_transmits) {
		stream_append_byte(&sb, '-');
		stream_append_u64(&sb, transmit_count);
	}

	if (contrast_mode != BeamformerContrastMode_None)
		stream_append_s8s(&sb, s8(" ("), s8_from_str8(beamformer_contrast_mode_strings[contrast_mode]), s8(")"));

	s8 result = arena_stream_commit(arena, &sb);
	return str8_from_s8(result);
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
			cell->width = measure_text(*font, str8_from_s8(cell->text)).w;
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

function void
resize_frame_view(BeamformerFrameView *view, uv2 dim)
{
	if ValidHandle(view->export_handle) os_release_handle(view->export_handle);

	glDeleteMemoryObjectsEXT(1, &view->memory_object);
	glCreateMemoryObjectsEXT(1, &view->memory_object);

	glDeleteTextures(1, &view->texture);
	glCreateTextures(GL_TEXTURE_2D, 1, &view->texture);

	/* TODO(rnp): add some ID for the specific view here */
	s8 label = s8("Frame View Texture");
	vk_image_allocate(&view->colour_image, dim.w, dim.h, 1, 1, VulkanImageUsage_Colour,
	                  VulkanUsageFlag_ImageSampling, &view->export_handle, label);

	glMemoryObjectParameterivEXT(view->memory_object, GL_DEDICATED_MEMORY_OBJECT_EXT, (GLint []){1});

	if (OS_WINDOWS) {
		glImportMemoryWin32HandleEXT(view->memory_object, view->colour_image.memory_size,
		                             GL_HANDLE_TYPE_OPAQUE_WIN32_EXT, (void *)view->export_handle.value[0]);
		// NOTE(rnp): w32 does not transfer ownership from handle back to driver
	} else {
		glImportMemoryFdEXT(view->memory_object, view->colour_image.memory_size,
		                    GL_HANDLE_TYPE_OPAQUE_FD_EXT, view->export_handle.value[0]);
		view->export_handle.value[0] = OSInvalidHandleValue;
	}

	glTextureStorageMem2DEXT(view->texture, view->colour_image.mip_map_levels, GL_RGBA8,
	                         view->colour_image.width, view->colour_image.height,
	                         view->memory_object, 0);

	/* NOTE(rnp): work around raylib's janky texture sampling */
	v4 border_colour = {{0, 0, 0, 1}};
	if (view->kind != BeamformerFrameViewKind_Copy) border_colour = (v4){0};
	glTextureParameteri(view->texture, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
	glTextureParameteri(view->texture, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
	glTextureParameterfv(view->texture, GL_TEXTURE_BORDER_COLOR, border_colour.E);
	/* TODO(rnp): better choice when depth component is included */
	glTextureParameteri(view->texture, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTextureParameteri(view->texture, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

	glObjectLabel(GL_TEXTURE, view->texture, (i32)label.len, (char *)label.data);
}

function void
ui_beamformer_frame_view_release_subresources(BeamformerFrameView *bv, BeamformerFrameViewKind kind)
{
	if (kind == BeamformerFrameViewKind_Copy)
		vk_buffer_release(&bv->copy_buffer);

	if (kind != BeamformerFrameViewKind_3DXPlane) {
		//if (bv->axial_scale_bar.scale_bar.savepoint_stack)
		//	SLLPushFreelist(bv->axial_scale_bar.scale_bar.savepoint_stack, ui->scale_bar_savepoint_freelist);
		//if (bv->lateral_scale_bar.scale_bar.savepoint_stack)
		//	SLLPushFreelist(bv->lateral_scale_bar.scale_bar.savepoint_stack, ui->scale_bar_savepoint_freelist);
	}
}

function Variable *
fill_variable(Variable *var, Variable *group, s8 name, u32 flags, VariableType type, Font font)
{
	var->flags      = flags;
	var->type       = type;
	var->name       = name;
	var->parent     = group;
	var->name_width = measure_text(font, str8_from_s8(name)).x;

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
	if (!result) result = push_struct_no_zero(arena, Variable);
	zero_struct(result);
	return fill_variable(result, group, name, flags, type, font);
}

function void
fill_variable_cycler(Variable *cycler, u32 *store, s8 *labels, u32 cycle_count)
{
	cycler->cycler.cycle_length = cycle_count;
	cycler->cycler.state        = store;
	cycler->cycler.labels       = labels;
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
add_ui_view(BeamformerUI *ui, Variable *parent, Arena *arena, s8 name, u32 view_flags)
{
	Variable *result = add_variable(ui, parent, arena, name, 0, VT_UI_VIEW, ui->small_font);
	UIView   *view   = &result->view;
	view->flags      = view_flags;
	return result;
}

function Variable *
add_floating_view(BeamformerUI *ui, Arena *arena, VariableType type, v2 at, Variable *child)
{
	Variable *result = add_ui_view(ui, 0, arena, s8(""), UIViewFlag_Floating);
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
ui_beamformer_frame_view_convert(BeamformerUI *ui, Arena *arena, BeamformerFrameView *bv,
                                 BeamformerFrameViewKind kind, BeamformerFrameView *old, b32 log_scale)
{
	bv->kind  = kind;
	bv->dirty = 1;

	bv->log_scale     = log_scale;
	bv->dynamic_range = old? old->dynamic_range : 50.0f;
	bv->threshold     = old? old->threshold     : 55.0f;
	bv->gamma         = old? old->gamma         : 1.0f;

	/* TODO(rnp): this is quite dumb. what we actually want is to render directly
	 * into the view region with the appropriate size for that region (scissor) */
	resize_frame_view(bv, (uv2){{FRAME_VIEW_RENDER_TARGET_SIZE}});

	switch (kind) {
	case BeamformerFrameViewKind_3DXPlane:{
		glTextureParameteri(bv->texture, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTextureParameteri(bv->texture, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
	}break;
	default:{
		b32 copy = kind == BeamformerFrameViewKind_Copy;
		bv->scale_bar_active[0] = copy ? old->scale_bar_active[0] : 1;
		bv->scale_bar_active[1] = copy ? old->scale_bar_active[1] : 1;
	}break;
	}

	switch (kind) {
	case BeamformerFrameViewKind_Latest:{
		bv->view_plane = BeamformerViewPlaneTag_Count;
	}break;
	default:{}break;
	}
}

function BeamformerFrameView *
ui_beamformer_frame_view_new(BeamformerUI *ui, Arena *arena, BeamformerFrameViewKind kind)
{
	BeamformerFrameView *result = SLLPopFreelist(ui->view_freelist);
	if (!result) result = push_struct_no_zero(arena, typeof(*result));
	zero_struct(result);
	result->export_handle.value[0] = OSInvalidHandleValue;
	DLLInsertLast(0, ui->view_first, ui->view_last, result, next, prev);
	ui_beamformer_frame_view_convert(ui, arena, result, kind, 0, 0);
	return result;
}

function Variable *
add_live_controls_view(BeamformerUI *ui, Variable *parent, Arena *arena)
{
	BeamformerLiveImagingParameters *lip = &ui->shared_memory->live_imaging_parameters;
	Variable *result = add_ui_view(ui, parent, &ui->arena, s8("Live Controls"), 0);
	result->view.child = add_variable(ui, result, &ui->arena, s8(""), 0,
	                                  VT_LIVE_CONTROLS_VIEW, ui->small_font);
	Variable *view = result->view.child;
	BeamformerLiveControlsView *lv = view->generic = push_struct(arena, typeof(*lv));

	Variable *amenu = &lv->acquisition_menu;
	fill_variable(amenu, view, s8(""), V_INPUT, VT_GROUP, ui->small_font);
	amenu->group.kind = VariableGroupKind_List;

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

	fill_variable(&lv->save_text, view, s8(""), V_INPUT|V_LIVE_CONTROL,
	              VT_LIVE_CONTROLS_STRING, ui->small_font);
	lv->save_text.generic = lip;

	return result;
}

function void
ui_beamformer_frame_view_copy_frame(BeamformerUI *ui, BeamformerFrameView *new, BeamformerFrameView *old)
{
	mem_copy(&new->frame, &old->frame, sizeof(old->frame));

	iv3 points     = new->frame.points;
	i64 frame_size = points.x * points.y * points.z * beamformer_data_kind_byte_size[new->frame.data_kind];

	Stream sb = arena_stream(ui->arena);
	stream_append_s8(&sb, s8("Frame Copy ["));
	stream_append_hex_u64(&sb, new->frame.id);
	stream_append_s8(&sb, s8("]"));
	stream_append_byte(&sb, 0);

	GPUBufferAllocateInfo allocate_info = {
		.size  = frame_size,
		.flags = VulkanUsageFlag_TransferDestination,
		.label = stream_to_str8(&sb),
	};
	vk_buffer_allocate(&new->copy_buffer, &allocate_info);

	GPUBuffer *backlog = ui->beamformer_context->compute_context.backlog.buffer;
	VulkanHandle cmd = vk_command_begin(VulkanTimeline_Compute);
	vk_command_wait_timeline(cmd, VulkanTimeline_Compute, old->frame.timeline_valid_value);
	vk_command_copy_buffer(cmd, &new->copy_buffer, backlog, old->frame.buffer_offset, frame_size);
	new->frame.timeline_valid_value = vk_command_end(cmd, (VulkanHandle){0}, (VulkanHandle){0});
}

function v3
beamformer_frame_view_plane_size(BeamformerUI *ui, BeamformerFrameView *view)
{
	assert(view->kind == BeamformerFrameViewKind_3DXPlane);
	v3 result = {0};
	//result.xy = v2_sub(ui->max_coordinate, ui->min_coordinate);
	result.x  = Max(1e-3f, result.x);
	result.y  = Max(1e-3f, result.y);
	result.z  = Max(1e-3f, result.z);
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
	f32 y_min = 0; //ui->min_coordinate.y;
	f32 y_max = 0; //ui->max_coordinate.y;
	v3 result = {.y = y_min + (y_max - y_min) / 2};
	return result;
}

function v3
offset_x_plane_position(BeamformerUI *ui, BeamformerFrameView *view, BeamformerViewPlaneTag tag)
{
	BeamformerLiveImagingParameters *li = &ui->shared_memory->live_imaging_parameters;
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
	f32 aspect = (f32)view->colour_image.width / (f32)view->colour_image.height;
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

function void
render_single_xplane(BeamformerUI *ui, BeamformerFrameView *view, Variable *x_plane_shift,
                     f32 rotation_turns, v3 translate, BeamformerViewPlaneTag tag,
                     VulkanHandle command, BeamformerRenderBeamformedPushConstants *pc)
{
	GPUBuffer *beamformed_buffer = ui->beamformer_context->compute_context.backlog.buffer;
	pc->input_data = 0;
	if (ui->latest_plane_valid[tag]) {
		BeamformerFrame *f = ui->latest_plane + tag;
		pc->input_data   = beamformed_buffer->gpu_pointer + f->buffer_offset;
		pc->input_size_x = f->points.x;
		pc->input_size_y = f->points.y;
		pc->input_size_z = f->points.z;
		pc->data_kind    = f->data_kind;
		vk_command_wait_timeline(command, VulkanTimeline_Compute, f->timeline_valid_value);
	}

	v3 camera = camera_for_x_plane_view(ui, view);
	v3 scale  = beamformer_frame_view_plane_size(ui, view);

	m4 model_transform = y_aligned_volume_transform(scale, translate, rotation_turns);
	m4 view_m          = view_matrix_for_x_plane_view(ui, view, camera);
	m4 projection      = projection_matrix_for_x_plane_view(view);

	//pc->mvp_matrix          = m4_mul(m4_mul(model_transform, view_m), projection);
	pc->mvp_matrix            = m4_mul(projection, m4_mul(view_m, model_transform));
	pc->bounding_box_colour   = v4_lerp(FG_COLOUR, HOVERED_COLOUR, x_plane_shift->hover_t);
	pc->bounding_box_fraction = FRAME_VIEW_BB_FRACTION;

	vk_command_push_constants(command, 0, sizeof(*pc), pc);
	vk_command_draw(command, &ui->unit_cube_model.model);

	XPlaneShift *xp = &x_plane_shift->x_plane_shift;
	v3 xp_delta = v3_sub(xp->end_point, xp->start_point);
	if (!f32_equal(v3_magnitude(xp_delta), 0)) {
		m4 x_rotation = m4_rotation_about_y(rotation_turns);
		v3 Z = x_rotation.c[2].xyz;
		v3 f = v3_scale(Z, v3_dot(Z, v3_sub(xp->end_point, xp->start_point)));

		/* TODO(rnp): there is no reason to compute the rotation matrix again */
		model_transform = y_aligned_volume_transform(scale, v3_add(f, translate), rotation_turns);

		pc->mvp_matrix            = m4_mul(projection, m4_mul(view_m, model_transform));
		pc->bounding_box_colour   = HOVERED_COLOUR;
		pc->bounding_box_fraction = 1.0f;
		pc->input_data            = 0;

		vk_command_push_constants(command, 0, sizeof(*pc), pc);
		vk_command_draw(command, &ui->unit_cube_model.model);
	}
}

function void
render_3D_xplane(BeamformerUI *ui, BeamformerFrameView *view, VulkanHandle command, BeamformerRenderBeamformedPushConstants *pc)
{
	if (view->demo) {
		view->rotation += dt_for_frame * 0.125f;
		if (view->rotation > 1.0f) view->rotation -= 1.0f;
	}

	v3 model_translate = offset_x_plane_position(ui, view, BeamformerViewPlaneTag_XZ);
	render_single_xplane(ui, view, 0 /* view->x_plane_shifts + 0 */,
	                     x_plane_rotation_for_view_plane(view, BeamformerViewPlaneTag_XZ),
	                     model_translate, BeamformerViewPlaneTag_XZ, command, pc);
	model_translate = offset_x_plane_position(ui, view, BeamformerViewPlaneTag_YZ);
	model_translate.y -= 0.0001f;
	render_single_xplane(ui, view, 0 /* view->x_plane_shifts + 1 */,
	                     x_plane_rotation_for_view_plane(view, BeamformerViewPlaneTag_YZ),
	                     model_translate, BeamformerViewPlaneTag_YZ, command, pc);
}

function void
render_2D_plane(BeamformerUI *ui, BeamformerFrameView *view, VulkanHandle command, BeamformerRenderBeamformedPushConstants *pc)
{
	m4 view_m     = m4_identity();
	m4 model      = m4_scale((v3){{2.0f, 2.0f, 0.0f}});
	m4 projection = orthographic_projection(0, 1, 1, 1);

	GPUBuffer *beamformed_buffer = ui->beamformer_context->compute_context.backlog.buffer;
	pc->mvp_matrix   = m4_mul(m4_mul(model, view_m), projection);
	pc->input_data   = beamformed_buffer->gpu_pointer + view->frame.buffer_offset,
	pc->input_size_x = view->frame.points.x,
	pc->input_size_y = view->frame.points.y,
	pc->input_size_z = view->frame.points.z,
	pc->data_kind    = view->frame.data_kind,

	vk_command_wait_timeline(command, VulkanTimeline_Compute, view->frame.timeline_valid_value);
	vk_command_push_constants(command, 0, sizeof(*pc), pc);
	vk_command_draw(command, &ui->unit_cube_model.model);
}

function b32
view_update(BeamformerUI *ui, BeamformerFrameView *view)
{
	if (view->kind == BeamformerFrameViewKind_Latest) {
		u32 index = view->view_plane;
		view->dirty |= view->frame.timeline_valid_value != ui->latest_plane[index].timeline_valid_value;
		memory_copy(&view->frame, ui->latest_plane + index, sizeof(view->frame));
	}

	/* TODO(rnp): x-z or y-z */
	// TODO(rnp): how to track this now? use pipeline handle value?
	view->dirty |= ui->beamformer_context->render_shader_updated;
	view->dirty |= view->kind == BeamformerFrameViewKind_3DXPlane;

	b32 result = view->dirty;
	return result;
}

function void
update_frame_views(BeamformerUI *ui, Rect window)
{
	for (BeamformerFrameView *view = ui->view_first; view; view = view->next) {
		if (view_update(ui, view)) {
			BeamformerRenderBeamformedPushConstants pc = {
				.bounding_box_colour = FRAME_VIEW_BB_COLOUR,
				.db_cutoff           = view->log_scale ? view->dynamic_range : 0,
				.threshold           = view->threshold,
				.gamma               = view->gamma,
				.positions           = ui->unit_cube_model.model.gpu_pointer,
				.normals             = ui->unit_cube_model.model.gpu_pointer + ui->unit_cube_model.normals_offset,
			};

			//start_renderdoc_capture();

			glSignalSemaphoreEXT(ui->render_semaphores_gl[0], 0, 0, 1, &view->texture, (GLenum []){GL_NONE});

			VulkanHandle cmd = vk_command_begin(VulkanTimeline_Graphics);
			vk_command_bind_pipeline(cmd, ui->pipelines[BeamformerShaderKind_RenderBeamformed - BeamformerShaderKind_RenderFirst]);
			vk_command_begin_rendering(cmd, &ui->render_3d_image, &ui->render_3d_depth_image, &view->colour_image);
			vk_command_viewport(cmd, view->colour_image.width, view->colour_image.height, 0, 0, 0.0f, 1.0f);
			vk_command_scissor(cmd, view->colour_image.width, view->colour_image.height, 0, 0);
			if (view->kind == BeamformerFrameViewKind_3DXPlane) {
				render_3D_xplane(ui, view, cmd, &pc);
			} else {
				render_2D_plane(ui, view, cmd, &pc);
			}
			vk_command_end_rendering(cmd);
			vk_command_end(cmd, ui->render_semaphores[0], ui->render_semaphores[1]);

			glWaitSemaphoreEXT(ui->render_semaphores_gl[1], 0, 0, 1, &view->texture, (GLenum[]){GL_LAYOUT_COLOR_ATTACHMENT_EXT});

			//end_renderdoc_capture();
			view->dirty = 0;
		}
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

function void
draw_text_tight(Font font, str8 text, v2 pos, Color colour)
{
	v2 off = v2_floor(pos);
	for (i64 i = 0; i < text.length; i++) {
		/* NOTE: assumes font glyphs are ordered ASCII */
		i32 idx = text.data[i] - 0x20;
		Rectangle dst = {
			off.x, off.y,
			font.recs[idx].width,
			font.recs[idx].height,
		};
		Rectangle src = {
			font.recs[idx].x,
			font.recs[idx].y,
			font.recs[idx].width,
			font.recs[idx].height,
		};
		DrawTexturePro(font.texture, src, dst, (Vector2){0}, 0, colour);

		off.x += (f32)font.recs[idx].width;
	}
}

function v2
draw_text_base(Font font, str8 text, v2 pos, Color colour)
{
	v2 off = v2_floor(pos);
	f32 glyph_pad = (f32)font.glyphPadding;
	for (i64 i = 0; i < text.length; i++) {
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
draw_outlined_text(str8 text, v2 pos, TextSpec *ts)
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
draw_text(str8 text, v2 pos, TextSpec *ts)
{
	if (ts->flags & TF_ROTATED) {
		rlPushMatrix();
		rlTranslatef(pos.x, pos.y, 0);
		rlRotatef(ts->rotation, 0, 0, 1);
		pos = (v2){0};
	}

	v2 result   = measure_text(*ts->font, text);
	/* TODO(rnp): the size of this should be stored for each font */
	str8 ellipsis = str8("...");
	b32 clamped = ts->flags & TF_LIMITED && result.w > ts->limits.size.w;
	if (clamped) {
		f32 ellipsis_width = measure_text(*ts->font, ellipsis).x;
		if (ellipsis_width < ts->limits.size.w) {
			text = clamp_text_to_width(*ts->font, text, ts->limits.size.w - ellipsis_width);
		} else {
			text.length     = 0;
			ellipsis.length = 0;
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
	b32 result = Between(p.x, r.pos.x, end.x) & Between(p.y, r.pos.y, end.y);
	return result;
}

function v2
rect_uv(v2 p, Rect r)
{
	v2 result = v2_div(v2_sub(p, r.pos), r.size);
	return result;
}

function v3
world_point_from_plane_uv(m4 world, v2 uv)
{
	v3 U   = world.c[0].xyz;
	v3 V   = world.c[1].xyz;
	v3 min = world.c[3].xyz;
	v3 result =  v3_add(v3_add(v3_scale(U, uv.x), v3_scale(V, uv.y)), min);
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

	DrawRectangleRec(rl_rect(filled), colour_from_normalized(fill_colour));
	DrawRectangleRoundedLinesEx(rl_rect(inner), 0.2f, 0, border_thick, BLACK);
	DrawRectangleRounded(rl_rect(bar), 0.6f, 1, colour_from_normalized(bar_colour));

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

	DrawRectangleRoundedLinesEx(rl_rect(border), 0.6f, 0, border_thick, fade(BLACK, 0.8f));
	border.pos = v2_sub(border.pos, shadow_off);
	DrawRectangleRounded(rl_rect(border), 0.6f, 1, colour_from_normalized(BG_COLOUR));
	DrawRectangleRoundedLinesEx(rl_rect(border), 0.6f, 0, border_thick, colour_from_normalized(border_colour));

	/* TODO(rnp): teach draw_text() about alignment */
	v2 at = align_text_in_rect(str8_from_s8(label), inner, *ts.font);
	at = v2_add(at, (v2){{3.0f, 3.0f}});
	v4 base_colour = ts.colour;
	ts.colour = (v4){{0, 0, 0, 0.8f}};
	draw_text(str8_from_s8(label), at, &ts);

	at = v2_sub(at, (v2){{3.0f, 3.0f}});
	ts.colour = v4_lerp(base_colour, HOVERED_COLOUR, var->hover_t);
	draw_text(str8_from_s8(label), at, &ts);

	v2 result = v2_add(r.size, border_size);
	return result;
}

function v2
draw_variable(BeamformerUI *ui, Arena arena, Variable *var, v2 at, v2 mouse, v4 base_colour, TextSpec text_spec)
{
	Stream buf = arena_stream(arena);
	stream_append_variable(&buf, var);
	s8 text = arena_stream_commit(&arena, &buf);
	v2 result = measure_text(*text_spec.font, str8_from_s8(text));

	if (var->flags & V_INPUT) {
		Rect text_rect = {.pos = at, .size = result};
		text_rect = extend_rect_centered(text_rect, (v2){.x = 8});
		//if (hover_interaction(ui, mouse, auto_interaction(text_rect, var)) && (var->flags & V_TEXT))
		//	ui->text_input_state.hot_font = text_spec.font;
		text_spec.colour = v4_lerp(base_colour, HOVERED_COLOUR, var->hover_t);
	}

	draw_text(str8_from_s8(text), at, &text_spec);
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
	case TableCellKind_None:{ draw_text(str8_from_s8(cell->text), cell_at, &ts); }break;
	case TableCellKind_Variable:{
		if (cell->var->flags & V_INPUT) {
			draw_variable(ui, arena, cell->var, cell_at, mouse, ts.colour, ts);
		} else if (cell->text.len) {
			draw_text(str8_from_s8(cell->text), cell_at, &ts);
		}
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
				DrawLineEx(rl_v2(start), rl_v2(end), t->column_border_thick, fade(BLACK, 0.8f));
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
			DrawLineEx(rl_v2(start), rl_v2(end), t->row_border_thick, fade(BLACK, 0.8f));
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
	// TODO(rnp): merge this into draw function, tons of duplicate code
	v2 vr_max_p = v2_add(view_rect.pos, view_rect.size);

	v3 U   = view->frame.voxel_transform.c[0].xyz;
	v3 V   = view->frame.voxel_transform.c[1].xyz;
	v3 min = view->frame.voxel_transform.c[3].xyz;

	v3 end = view->ruler.end;
	if (view->ruler.state != RulerState_Hold)
		end = world_point_from_plane_uv(view->frame.voxel_transform, rect_uv(ui_context->current_mouse, view_rect));

	v2 start_uv = plane_uv(v3_sub(view->ruler.start, min), U, V);
	v2 end_uv   = plane_uv(v3_sub(end,               min), U, V);

	v2 start_p  = v2_add(view_rect.pos, v2_mul(start_uv, view_rect.size));
	v2 end_p    = v2_add(view_rect.pos, v2_mul(end_uv,   view_rect.size));

	b32 start_in_bounds = point_in_rect(start_p, view_rect);
	b32 end_in_bounds   = point_in_rect(end_p,   view_rect);

	// TODO(rnp): this should be a ray intersection not a clamp
	start_p = clamp_v2_rect(start_p, view_rect);
	end_p   = clamp_v2_rect(end_p, view_rect);

	Color rl_colour = colour_from_normalized(ts.colour);
	DrawLineEx(rl_v2(end_p), rl_v2(start_p), 2, rl_colour);
	if (start_in_bounds) DrawCircleV(rl_v2(start_p), 3, rl_colour);
	if (end_in_bounds)   DrawCircleV(rl_v2(end_p),   3, rl_colour);

	Stream buf = arena_stream(a);
	stream_append_f64(&buf, 1e3 * v3_magnitude(v3_sub(end, view->ruler.start)), 100);
	stream_append_s8(&buf, s8(" mm"));

	s8 s = stream_to_s8(&buf);
	v2 txt_p = start_p;
	v2 txt_s = measure_text(*ts.font, str8_from_s8(s));
	v2 pixel_delta = v2_sub(start_p, end_p);
	if (pixel_delta.y < 0) txt_p.y -= txt_s.y;
	if (pixel_delta.x < 0) txt_p.x -= txt_s.x;
	if (txt_p.x < view_rect.pos.x) txt_p.x = view_rect.pos.x;
	if (txt_p.x + txt_s.x > vr_max_p.x) txt_p.x -= (txt_p.x + txt_s.x) - vr_max_p.x;

	draw_text(str8_from_s8(s), txt_p, &ts);
}

function v2
draw_frame_view_controls(BeamformerUI *ui, Arena arena, BeamformerFrameView *view, Rect vr, v2 mouse)
{
	TextSpec text_spec = {.font = &ui->small_font, .flags = TF_LIMITED|TF_OUTLINED,
	                      .colour = RULER_COLOUR, .outline_thick = 1, .outline_colour.a = 1,
	                      .limits.size.x = vr.size.w};

	Table *table = table_new(&arena, 3, TextAlignment_Left, TextAlignment_Left, TextAlignment_Left);
	//table_push_parameter_row(table, &arena, view->gamma.name,     &view->gamma,     s8(""));
	//table_push_parameter_row(table, &arena, view->threshold.name, &view->threshold, s8(""));
	//if (view->log_scale)
	//	table_push_parameter_row(table, &arena, view->dynamic_range.name, &view->dynamic_range, s8("[dB]"));

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

	f32 aspect = (f32)view->colour_image.width / (f32)view->colour_image.height;
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

	//for (i32 i = 0; i < countof(view->x_plane_shifts); i++) {
	//	Variable *it = view->x_plane_shifts + i;
	//	Interaction interaction = auto_interaction(vr, it);
	//	if (id == i) ui->next_interaction = interaction;
	//	if (interaction_is_hot(ui, interaction)) it->hover_t += HOVER_SPEED * dt_for_frame;
	//	else                                     it->hover_t -= HOVER_SPEED * dt_for_frame;
	//	it->hover_t = CLAMP01(it->hover_t);
	//}

	Rectangle  tex_r  = {0, 0, (f32)view->colour_image.width, (f32)view->colour_image.height};
	NPatchInfo tex_np = {tex_r, 0, 0, 0, 0, NPATCH_NINE_PATCH};
	DrawTextureNPatch(make_raylib_texture(view), tex_np, rl_rect(vr), (Vector2){0}, 0, WHITE);

	draw_frame_view_controls(ui, arena, view, vr, mouse);
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
                            f32 compute_time_sum, TextSpec ts, Rect r, v2 mouse)
{
	read_only local_persist s8 frame_labels[] = {s8_comp("0:"), s8_comp("-1:"), s8_comp("-2:"), s8_comp("-3:")};
	f32 total_times[countof(frame_labels)] = {0};

	u32 stages = stats->table.shader_count;
	Table *table = table_new(&arena, countof(frame_labels), TextAlignment_Right, TextAlignment_Left);
	for (u32 i = 0; i < countof(frame_labels); i++) {
		TableCell *cells = table_push_row(table, &arena, TRK_CELLS)->data;
		cells[0].text = frame_labels[i];
		u32 frame_index = (stats->latest_frame_index - i - 1) % countof(stats->table.times);
		for (u32 j = 0; j < stages; j++)
			total_times[i] += stats->table.times[frame_index][j];
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

		u32 frame_index = (stats->latest_frame_index - row_index - 1) % countof(stats->table.times);
		f32 total_width = average_width * total_times[row_index] / compute_time_sum;
		Rect rect;
		rect.pos  = v2_add(cr.pos, (v2){{cr.size.w + table->cell_pad.w , cr.size.h * 0.15f}});
		rect.size = (v2){.y = 0.7f * cr.size.h};
		for (u32 i = 0; i < stages; i++) {
			rect.size.w = total_width * stats->table.times[frame_index][i] / total_times[row_index];
			Color color = colour_from_normalized(g_colour_palette[i % countof(g_colour_palette)]);
			DrawRectangleRec(rl_rect(rect), color);
			if (point_in_rect(mouse, rect)) {
				text_pos   = v2_add(rect.pos, (v2){.x = table->cell_pad.w});
				s8 name    = push_s8_from_parts(&arena, s8(""), beamformer_shader_names[stats->table.shader_ids[i]], s8(": "));
				mouse_text = push_compute_time(&arena, name, stats->table.times[frame_index][i]);
			}
			rect.pos.x += rect.size.w;
		}
		row_index++;
	}

	v2 start = v2_add(r.pos, (v2){.x = table->widths[0] + average_width + table->cell_pad.w});
	v2 end   = v2_add(start, (v2){.y = result.y});
	DrawLineEx(rl_v2(start), rl_v2(end), 4, colour_from_normalized(FG_COLOUR));

	if (mouse_text.len) {
		ts.font = &ui->small_font;
		ts.flags &= ~(u32)TF_LIMITED;
		ts.flags |=  (u32)TF_OUTLINED;
		ts.outline_colour = (v4){.a = 1};
		ts.outline_thick  = 1;
		draw_text(str8_from_s8(mouse_text), text_pos, &ts);
	}

	return result;
}

function v2
draw_live_controls_view(BeamformerUI *ui, Variable *var, Rect r, v2 mouse, Arena arena)
{
	BeamformerLiveImagingParameters *lip = &ui->shared_memory->live_imaging_parameters;
	BeamformerLiveControlsView      *lv  = var->generic;

	TextSpec text_spec = {.font = &ui->font, .colour = FG_COLOUR, .flags = TF_LIMITED};

	v2 slider_size = {{MIN(140.0f, r.size.w), (f32)ui->font.baseSize}};
	v2 button_size = {{MIN(r.size.w, slider_size.x + (f32)ui->font.baseSize), (f32)ui->font.baseSize * 1.5f}};

	f32 text_off   = r.pos.x + 0.5f * MAX(0, (r.size.w - slider_size.w - (f32)ui->font.baseSize));
	f32 slider_off = r.pos.x + 0.5f * (r.size.w - slider_size.w);
	f32 button_off = r.pos.x + 0.5f * (r.size.w - button_size.w);

	text_spec.limits.size.w = r.size.w - (text_off - r.pos.x);

	v2 at = {{text_off, r.pos.y}};

	if (popcount_u64(lip->acquisition_kind_enabled_flags) > 1) {
		u32 kind = lip->acquisition_kind;
		str8 kind_string = kind < BeamformerAcquisitionKind_Count ? beamformer_acquisition_kind_strings[kind]
		                                                          : str8("Invalid");
		v2 label_size = draw_text(str8("Acquisition:"), at, &text_spec);

		text_spec.colour = v4_lerp(FG_COLOUR, HOVERED_COLOUR, lv->acquisition_menu.hover_t);
		text_spec.limits.size.w -= label_size.x + (f32)ui->small_font.baseSize / 2;

		Rect menu   = {.pos = at};
		menu.pos.x += label_size.x + (f32)ui->small_font.baseSize / 2;
		menu.size.h = (f32)ui->small_font.baseSize + TITLE_BAR_PAD;
		menu.size.w = draw_text(kind_string, menu.pos, &text_spec).x;

		text_spec.colour = FG_COLOUR;
		text_spec.limits.size.w = r.size.w - (text_off - r.pos.x);

		Interaction interaction = {.kind = InteractionKind_AcquisitionMenu,
		                           .var  = &lv->acquisition_menu,
		                           .rect = menu};
		hover_interaction(ui, mouse, interaction);

		at.y += label_size.y;
	}

	v4 hsv_power_slider = {{0.35f * ease_in_out_cubic(1.0f - lip->transmit_power), 0.65f, 0.65f, 1}};
	at.y += draw_text(str8("Power:"), at, &text_spec).y;
	at.x  = slider_off;
	at.y += draw_variable_slider(ui, &lv->transmit_power, (Rect){.pos = at, .size = slider_size},
	                             lip->transmit_power, hsv_to_rgb(hsv_power_slider), mouse);

	at.x  = text_off;
	at.y += draw_text(str8("TGC:"), at, &text_spec).y;
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
		at.y += draw_text(str8("File Tag:"), at, &text_spec).y;
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

function void
ui_event_consume(BeamformerInput *input, BeamformerInputEvent *current)
{
	BeamformerUI *ui = ui_context;
	BeamformerInputEvent *last = input->event_queue + input->event_count - 1;
	if Between(current, input->event_queue, last) {
		u64 index = current - input->event_queue;
		u64 bin   = index / (sizeof(ui->input_consumed[0]) * 8);
		u64 bit   = index % (sizeof(ui->input_consumed[0]) * 8);
		ui->input_consumed[bin] |= (1 << bit);
	}
}

function BeamformerInputEvent *
ui_event_next(BeamformerInput *input, BeamformerInputEvent *current)
{
	BeamformerUI *ui = ui_context;
	BeamformerInputEvent *result = 0, *last = input->event_queue + input->event_count - 1;

	current++;
	current = Max(current, input->event_queue);

	for (; !result && Between(current, input->event_queue, last); current++) {
		u64 index = current - input->event_queue;
		u64 bin   = index / (sizeof(ui->input_consumed[0]) * 8);
		u64 bit   = index % (sizeof(ui->input_consumed[0]) * 8);

		if (!(ui->input_consumed[bin] & (1 << bit)) &&
		    (current->kind == BeamformerInputEventKind_ButtonPress   ||
		     current->kind == BeamformerInputEventKind_ButtonRelease ||
		     current->kind == BeamformerInputEventKind_MouseScroll))
		{
			result = current;
		}
	}
	return result;
}

function UINode *
ui_node_from_key(UINodeKey key)
{
	UINodeHashBucket *hb     = ui_context->node_hash_table + (key.value % UI_HASH_TABLE_COUNT);
	UINode           *result = &ui_node_nil;

	for (UINode *b = hb->first; !ui_node_is_nil(b); b = b->hash_next) {
		if (ui_node_key_equal(b->key, key)) {
			result = b;
			break;
		}
	}

	return result;
}

function str8
ui_draw_part_from_key_string(str8 string)
{
	str8 result = string;
	i64 index = str8_find_needle(string, str8("##"), 0);
	if (index < string.length)
		result.length = index;
	return result;
}

function str8
ui_hash_part_from_key_string(str8 string)
{
	str8 result = string;
	// NOTE(rnp): for xxx###yyy only use the ###yyy otherwise the whole string is hashed
	i64 index = str8_find_needle(string, str8("###"), 0);
	if (index < string.length)
		result = str8_skip(string, index);
	return result;
}

function UINodeKey
ui_key_from_string(str8 string, UINodeKey seed)
{
	UINodeKey result = {0};
	if (string.length > 0) {
		str8 hash_string = ui_hash_part_from_key_string(string);
		result.value     = u64_hash_from_str8_seed(hash_string, seed.value);
	}
	return result;
}

function Font
ui_font_for_node(UINode *node)
{
	Font result = node->font_size > 28.0f ? ui_context->font : ui_context->small_font;
	return result;
}

function b32
ui_number_conversion_f64(str8 s, f64 *out_value)
{
	b32 result = 0;
	NumberConversion number = number_from_str8(s);
	if (number.result == NumberConversionResult_Success) {
		result     = 1;
		if (number.kind == NumberConversionKind_Float)
			*out_value = number.F64;
		else
			*out_value = (f64)number.S64;
	}
	return result;
}

function iv2
ui_text_input_cursor_range(void)
{
	UITextInputState *tis = &ui_context->text_input_state;
	iv2 range;
	range.x = Min(tis->cursor, tis->mark);
	range.y = Max(tis->cursor, tis->mark);
	return range;
}

function str8
ui_text_input_string(void)
{
	UITextInputState *tis = &ui_context->text_input_state;
	str8 result = {.data = tis->buffer, .length = tis->count};
	return result;
}

function str8
ui_text_input_last_string(void)
{
	UITextInputState *tis = &ui_context->text_input_state;
	str8 result = {.data = tis->last_buffer, .length = tis->last_count};
	return result;
}

function Rect
ui_text_input_rect(void)
{
	Rect result = ui_node_rect(ui_node_from_key(ui_context->text_input_state.node_key));
	f32 text_box_slop = 4.0f;
	result.pos.x  -= text_box_slop;
	result.size.x += 2 * text_box_slop;
	return result;
}

function i32
ui_text_input_index_from_point(f32 point)
{
	i32 result = 0;

	// TODO(rnp): visible range, extended virtual rect which exactly fits the visible text
	UITextInputState *tis = &ui_context->text_input_state;
	Rect r = ui_text_input_rect();

	Font font = ui_font_for_node(ui_node_from_key(tis->node_key));

	/* NOTE: extra offset to help with putting a cursor at idx 0 */
	f32 pct   = Clamp01((point - r.pos.x) / r.size.w);
	f32 x_off = 10.0f, x_bounds = r.size.w * pct;
	for (; result < tis->count && x_off < x_bounds; result++) {
		/* NOTE: assumes font glyphs are ordered ASCII */
		i32 idx  = tis->buffer[result] - 0x20;
		x_off   += (f32)font.glyphs[idx].advanceX;
		if (font.glyphs[idx].advanceX == 0)
			x_off += font.recs[idx].width;
	}

	return result;
}

function void
ui_text_input_end(void)
{
	UITextInputState *tis = &ui_context->text_input_state;

	UINode *next_node     = ui_node_from_key(tis->next_node_key);
	str8 new_input_string = ui_draw_part_from_key_string(next_node->string);

	tis->cursor = tis->mark = 0;
	tis->last_count = tis->count;
	tis->count      = Min(new_input_string.length, countof(tis->buffer));
	tis->numeric    = (next_node->flags & UINodeFlag_TextInputNumeric) != 0;
	memory_copy(tis->last_buffer, tis->buffer, tis->last_count);
	memory_copy(tis->buffer, new_input_string.data, tis->count);

	tis->last_node_key = tis->node_key;
	tis->node_key      = ui_node_key_zero();
}

function void
ui_text_input_insert(str8 text)
{
	UITextInputState *tis = &ui_context->text_input_state;
	iv2 cursor_range       = ui_text_input_cursor_range();
	i64 bytes_after_cursor = tis->count - cursor_range.y;
	i64 remaining_length   = ((i32)countof(tis->buffer) - cursor_range.x) - bytes_after_cursor;
	i64 truncated_length   = Min(remaining_length, text.length);

	memory_move(tis->buffer + cursor_range.x + truncated_length,
	            tis->buffer + cursor_range.y, bytes_after_cursor);
	memory_copy(tis->buffer + cursor_range.x, text.data, truncated_length);

	tis->count -= cursor_range.y - cursor_range.x;
	tis->count += truncated_length;
	tis->cursor = tis->mark = cursor_range.x + truncated_length;
}

function b32
ui_text_input_update(BeamformerInput *input)
{
	UITextInputState *tis = &ui_context->text_input_state;

	Arena  scratch = *ui_build_arena();
	Stream sb = arena_stream(scratch);

	enum {
		DeltaPicksSide  = (1 << 0),
		WordScan        = (1 << 1),
		Delete          = (1 << 2),
		KeepMark        = (1 << 3),
		Copy            = (1 << 4),
		Paste           = (1 << 5),
	};

	i32 delta = 0;
	u32 flags = 0;

	b32 result = 0;

	// NOTE(rnp): first pass, non uniform inputs
	for (BeamformerInputEvent *event = ui_event_next(input, 0);
	     event;
	     event = ui_event_next(input, event))
	{
		b32 taken = 0;

		BeamformerInputModifiers mods = event->modifiers;
		if (event->kind == BeamformerInputEventKind_ButtonPress) {
			if (mods & BeamformerInputModifier_Control)
				flags |= WordScan;

			if (mods & BeamformerInputModifier_Shift)
				flags |= KeepMark;

			switch (event->button_id) {
			default:{}break;
			case BeamformerButtonID_Escape:
			case BeamformerButtonID_Enter:
			{
				taken  = 1;
				result = 1;
			}break;

			case BeamformerButtonID_A: if (mods & BeamformerInputModifier_Control) {
				tis->cursor = 0;
				tis->mark   = tis->count;
				taken       = 1;
			}break;

			case BeamformerButtonID_Backspace:{
				delta -= 1;
				flags |= Delete|KeepMark;
				taken  = 1;
			}break;

			case BeamformerButtonID_Delete:{
				delta += 1;
				flags |= Delete|KeepMark;
				taken  = 1;
			}break;

			case BeamformerButtonID_Left:{
				delta -= 1;
				flags |= DeltaPicksSide;
				taken  = 1;
			}break;

			case BeamformerButtonID_Right:{
				delta += 1;
				flags |= DeltaPicksSide;
				taken  = 1;
			}break;

			}

			if (!taken && event->codepoint) {
				u32 cp = event->codepoint;
				taken = !tis->numeric || (Between(cp, '0', '9') || (cp == '.') || (cp == '-' && tis->cursor == 0));
				if (taken) stream_append_codepoint(&sb, event->codepoint);
			}
		}

		if (taken) ui_event_consume(input, event);
	}

	if ((flags & Delete) && tis->mark != tis->cursor)
		delta = 0;

	// TODO(rnp): word selection
	tis->mark += delta;
	tis->mark  = Clamp(tis->mark, 0, tis->count);

	if (!(flags & KeepMark) && delta) {
		i32 new_cursor = tis->mark;
		if (flags & DeltaPicksSide) {
			if (delta < 0) new_cursor = Min(tis->mark, tis->cursor);
			if (delta > 0) new_cursor = Max(tis->mark, tis->cursor);
		}
		tis->mark = tis->cursor = new_cursor;
	}

	if ((flags & Delete) || sb.widx)
		ui_text_input_insert(stream_to_str8(&sb));

	if (flags || delta || sb.widx)
		tis->blinker.t = 1.0;

	return result;
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
	{
		UIView *fw = &view->view;
		assert(fw->child->type == VT_GROUP);
		fw->child->group.expanded  = 0;
		fw->child->group.container = 0;
		view->parent->next = view->next;
		view->next->parent = view->parent;
		SLLPushFreelist(view, ui->variable_freelist);
	}break;
	InvalidDefaultCase;
	}
}

function void
ui_button_interaction(BeamformerUI *ui, Variable *button)
{
	assert(button->type == VT_UI_BUTTON);
	if (Between(button->button, UI_BID_ACQUISITION_KIND_FIRST, UI_BID_ACQUISITION_KIND_LAST)) {
		BeamformerSharedMemory          *sm  = ui->shared_memory;
		BeamformerLiveImagingParameters *lip = &sm->live_imaging_parameters;

		lip->acquisition_kind = button->button - UI_BID_ACQUISITION_KIND_FIRST;
		atomic_or_u32(&sm->live_imaging_dirty_flags, BeamformerLiveImagingDirtyFlags_AcquisitionKind);
		ui_view_close(ui, button->parent->group.container);
	} else {
		InvalidCodePath;
	}
}

function void
ui_begin_interact(BeamformerUI *ui, v2 mouse, b32 scroll)
{
	Interaction hot = ui->hot_interaction;
	if (hot.kind != InteractionKind_None) {
		if (hot.kind == InteractionKind_Auto) {
			switch (hot.var->type) {
			case VT_NULL:{ hot.kind = InteractionKind_Nop; }break;
			case VT_B32:{ hot.kind  = InteractionKind_Set; }break;
			case VT_UI_BUTTON:{ hot.kind = InteractionKind_Button; }break;
			case VT_GROUP:{ hot.kind = InteractionKind_Set; }break;
			case VT_UI_MENU:
			{
				if (hot.var->type == VT_UI_MENU) {
					hot.kind = InteractionKind_Drag;
				} else {
					hot.kind = InteractionKind_Text;
				}
				ui_widget_bring_to_front(&ui->floating_widget_sentinal, hot.var);

				// TODO(rnp): hack. this won't be needed with a proper immediate mode UI
				if (ui->interaction.kind == InteractionKind_Text)
					hot.var = hot.var->view.child;
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
						//hot.var  = &bv->threshold;
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
							bv->ruler.start = world_point_from_plane_uv(bv->frame.voxel_transform,
							                                            rect_uv(mouse, hot.rect));
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
			{
				if (scroll) {
					hot.kind = InteractionKind_Scroll;
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
			SetMousePosition((i32)mouse.x, (i32)mouse.y);
		}
	} else {
		ui->interaction.kind = InteractionKind_Nop;
	}
}

function u32
ui_cycler_delta_for_frame(void)
{
	u32 result = (u32)GetMouseWheelMoveV().y;
	if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT))  result += 1;
	if (IsMouseButtonPressed(MOUSE_BUTTON_RIGHT)) result -= 1;
	return result;
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
			u32 delta = ui_cycler_delta_for_frame();
			BeamformerFrameView *old = view->child->generic;
			view->child->generic = ui_beamformer_frame_view_new(ui, &ui->arena, old->kind);
			BeamformerFrameViewKind last_kind = ((u32)old->kind - delta) % BeamformerFrameViewKind_Count;

			/* NOTE(rnp): log_scale gets released below before its needed */
			//b32 log_scale = old->log_scale;

			ui_beamformer_frame_view_release_subresources(old, last_kind);
			//ui_beamformer_frame_view_convert(ui, &ui->arena, view->child, view->menu, old->kind, old, log_scale);

			DLLRemove(0, old->next, old->prev, old, next, prev);
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
	BeamformerLiveControlsView *lv = controls->generic;
	atomic_or_u32(&ui->shared_memory->live_imaging_dirty_flags, lv->active_field_flag);
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
			BeamformerViewPlaneTag plane = 0; //view_plane_tag_from_x_plane_shift(view, it->var);
			f32 rotation  = x_plane_rotation_for_view_plane(view, plane);
			m4 x_rotation = m4_rotation_about_y(rotation);
			v3 Z = x_rotation.c[2].xyz;
			f32 delta = v3_dot(Z, v3_sub(xp->end_point, xp->start_point));
			xp->start_point = xp->end_point;

			BeamformerSharedMemory *          sm = ui->shared_memory;
			BeamformerLiveImagingParameters * li = &sm->live_imaging_parameters;
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
		case VT_CYCLER:{
			*it->var->cycler.state += ui_cycler_delta_for_frame();
			*it->var->cycler.state %= it->var->cycler.cycle_length;
		}break;
		InvalidDefaultCase;
		}
	}break;
	case InteractionKind_AcquisitionMenu:
	case InteractionKind_Menu:
	{
		assert(it->var->type == VT_GROUP);
		VariableGroup *g = &it->var->group;
		if (g->container) {
			ui_widget_bring_to_front(&ui->floating_widget_sentinal, g->container);
		} else {
			if (it->kind == InteractionKind_AcquisitionMenu) {
				for (Variable *var = g->first; var;) {
					Variable *next = var->next;
					var->next = var->parent = 0;
					SLLPushFreelist(var, ui->variable_freelist);
					var = next;
				}
				g->first = g->last = 0;

				BeamformerLiveImagingParameters *lip = &ui->shared_memory->live_imaging_parameters;
				u64 enabled_kinds = atomic_load_u64(&lip->acquisition_kind_enabled_flags);
				for EachBit(enabled_kinds, kind) {
					Variable *button = add_variable(ui, it->var, &ui->arena, s8_from_str8(beamformer_acquisition_kind_strings[kind]),
					                                V_INPUT, VT_UI_BUTTON, ui->small_font);
					button->button = UI_BID_ACQUISITION_KIND_FIRST + kind;
				}
			}
			g->container = add_floating_view(ui, &ui->arena, VT_UI_MENU, mouse, it->var);
		}
	}break;
	case InteractionKind_Ruler:{
		assert(it->var->type == VT_BEAMFORMER_FRAME_VIEW);
		((BeamformerFrameView *)it->var->generic)->ruler.state = RulerState_None;
	}break;
	case InteractionKind_Button:{ ui_button_interaction(ui, it->var); }break;
	case InteractionKind_Text:{ ui_view_close(ui, 0); }break;
	InvalidDefaultCase;
	}

	if (flags & V_CAUSES_COMPUTE)
		ui->flush_parameters = 1;

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
ui_interact(BeamformerUI *ui, BeamformerInput *input, Rect window_rect)
{
	v2 input_mouse = {{input->mouse_x, input->mouse_y}};
	Interaction *it = &ui->interaction;
	if (it->kind == InteractionKind_None || interaction_is_sticky(*it)) {
		ui->hot_interaction = ui->next_interaction;

		b32 mouse_left_pressed  = IsMouseButtonPressed(MOUSE_BUTTON_LEFT);
		b32 mouse_right_pressed = IsMouseButtonPressed(MOUSE_BUTTON_RIGHT);
		b32 wheel_moved         = GetMouseWheelMoveV().y != 0;
		if (mouse_right_pressed || mouse_left_pressed || wheel_moved) {
			ui_begin_interact(ui, input_mouse, wheel_moved);
		}
	}

	switch (it->kind) {
	case InteractionKind_Nop:{ it->kind = InteractionKind_None; }break;
	case InteractionKind_None:{}break;
	case InteractionKind_Text:{}break;
	case InteractionKind_Drag:{
		if (!IsMouseButtonDown(MOUSE_BUTTON_LEFT) && !IsMouseButtonDown(MOUSE_BUTTON_RIGHT)) {
			ui_end_interact(ui, input_mouse);
		} else {
			v2 ws     = window_rect.size;
			v2 dMouse = {0};

			switch (it->var->type) {
			case VT_BEAMFORMER_VARIABLE:{}break;
			case VT_X_PLANE_SHIFT:{
				assert(it->var->parent && it->var->parent->type == VT_BEAMFORMER_FRAME_VIEW);
				v2 mouse = clamp_v2_rect(input_mouse, it->rect);
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
					bv->rotation -= dMouse.x / ws.w;
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
			default:{}break;
			}
			if (it->var->flags & V_LIVE_CONTROL)
				ui_live_control_update(ui, it->var->parent);
		}
	} break;
	default:{ ui_end_interact(ui, input_mouse); }break;
	}

	ui->next_interaction = (Interaction){.kind = InteractionKind_None};
}

function void
ui_context_menu_close(void)
{
	ui_context->context_menu_next_anchor_key = ui_node_key_zero();
	ui_context->context_menu_state_changed   = 1;
}

function void
ui_context_menu_open(UINodeKey anchor_node_key, UITreeNode *panel)
{
	ui_context->context_menu_next_anchor_key = anchor_node_key;
	ui_context->context_menu_next_panel      = panel;
	ui_context->context_menu_state_changed   = 1;
	ui_context->context_menu_open_t          = 0;
}

function void
ui_drag_end(void)
{
	ui_context->drag_panel = 0;
	ui_context->drag_root  = 0;
}

function void
ui_drag_begin(UITreeNode *panel)
{
	if (!ui_context->drag_panel) {
		ui_context->drag_panel  = panel;
		ui_context->drag_open_t = 0;
	}
}

function v2
ui_node_final_position(UINode *node)
{
	v2 result = ui_node_rect(node).pos;
	for (UINode *p = node->parent; !ui_node_is_nil(p); p = p->parent)
		if (p->flags & UINodeFlag_ViewScroll)
			result = v2_sub(result, p->view_scroll_offset);
	return result;
}

function UISignal
ui_signal_from_node(UINode *node)
{
	BeamformerUI    *ui    = ui_context;
	BeamformerInput *input = beamformer_input;

	UISignal result = {.node = node};
	Rect nr = ui_node_rect(node);

	// NOTE(rnp): use the last mouse as this matches what the user saw when they positioned
	v2 mouse = ui->last_mouse;

	// NOTE(rnp): apply offset
	nr.pos = ui_node_final_position(node);

	// NOTE(rnp): apply clipping
	for (UINode *p = node->parent; !ui_node_is_nil(p); p = p->parent)
		if (p->flags & UINodeFlag_Clip)
			nr = rect_intersect(nr, ui_node_rect(p));

	// NOTE(rnp): filter when node is under context menu
	b32 context_menu_descendent = 0;
	for (UINode *p = node->parent; !ui_node_is_nil(p); p = p->parent)
		if (p == ui->context_menu_root)
			context_menu_descendent = 1;

	Rect filter_rect = {0};
	if (!context_menu_descendent && !ui_node_key_nil(ui->context_menu_anchor_key))
		filter_rect = ui_node_rect(ui->context_menu_root);

	b32 collides = point_in_rect(mouse, nr) && !point_in_rect(mouse, filter_rect);

	result.flags |= collides * UISignalFlag_Hovering;

	for (BeamformerInputEvent *event = ui_event_next(input, 0);
	     event;
	     event = ui_event_next(input, event))
	{
		b32 taken   = 0;
		b32 press   = event->kind == BeamformerInputEventKind_ButtonPress;
		b32 release = event->kind == BeamformerInputEventKind_ButtonRelease;
		b32 event_is_mouse = (press || release) && (
		                     event->button_id == BeamformerButtonID_MouseLeft   ||
		                     event->button_id == BeamformerButtonID_MouseRight  ||
		                     event->button_id == BeamformerButtonID_MouseMiddle ||
		                     (0));
		UIMouseButtonKind mouse_button = (event->button_id == BeamformerButtonID_MouseLeft   ? UIMouseButtonKind_Left :
		                                  event->button_id == BeamformerButtonID_MouseRight  ? UIMouseButtonKind_Right :
		                                  event->button_id == BeamformerButtonID_MouseMiddle ? UIMouseButtonKind_Middle :
		                                  UIMouseButtonKind_Left);

		if ((node->flags & UINodeFlag_MouseClickable) && event_is_mouse && press && collides) {
			ui->hot_node_key                  = node->key;
			ui->active_node_key[mouse_button] = node->key;

			// TODO(rnp): store timestamp
			// TODO(rnp): check with timestamp for double/triple click

			result.flags |= UISignalFlag_LeftPressed << mouse_button;

			taken = 1;
		}

		// NOTE(rnp): release, applies whenever this node is active regardless of in bounds or not.
		if ((node->flags & UINodeFlag_MouseClickable) && event_is_mouse && release &&
		     ui_node_key_equal(ui->active_node_key[mouse_button], node->key))
		{
			ui->hot_node_key                  = ui_node_key_zero();
			ui->active_node_key[mouse_button] = ui_node_key_zero();
			result.flags |= UISignalFlag_LeftReleased << mouse_button;

			taken = 1;
		}

		// NOTE(rnp): custom scroll handling
		if (node->flags & UINodeFlag_Scroll && event->kind == BeamformerInputEventKind_MouseScroll && collides) {
			v2 delta = {{event->scroll.x, event->scroll.y}};
			// TODO(rnp): glfw doesn't pass these through
			if (event->modifiers & BeamformerInputModifier_Shift)
				swap(delta.x, delta.y);
			result.scroll = v2_add(result.scroll, delta);

			taken = 1;
		}

		// NOTE(rnp): scrollable container handling
		if (node->flags & UINodeFlag_ViewScroll && collides) {
			v2 delta = {{event->scroll.x, event->scroll.y}};
			// TODO(rnp): glfw doesn't pass these through
			if (event->modifiers & BeamformerInputModifier_Shift)
				swap(delta.x, delta.y);

			// NOTE(rnp): if the view only has scroll in one direction we ignore the delta's direction

			if ((node->flags & UINodeFlag_ViewScrollX) == 0) {
				if f32_equal(delta.y, 0)
					delta.y = delta.x;
				delta.x = 0;
			}

			if ((node->flags & UINodeFlag_ViewScrollY) == 0) {
				if f32_equal(delta.x, 0)
					delta.x = delta.y;
				delta.y = 0;
			}

			node->view_scroll_offset = v2_add(node->view_scroll_offset, v2_scale(delta, -10.f));
			taken = 1;
		}

		// TODO(rnp): clipboard

		if (taken) ui_event_consume(input, event);
	}

	// NOTE(rnp): single click dragging
	if (node->flags & UINodeFlag_MouseClickable) {
		for EachEnumValue(UIMouseButtonKind, k) {
			if (ui_node_key_equal(ui->active_node_key[k], node->key) ||
	        result.flags & (UISignalFlag_LeftPressed << k))
			{
				result.flags |= (UISignalFlag_LeftDragging << k);
			}
		}
	}

	// NOTE(rnp): drop handling
	if (node->flags & UINodeFlag_DropSite && collides
	    && ui_node_key_equal(ui->drop_target_key, ui_node_key_zero()))
	{
		ui->drop_target_key = node->key;
	}

	if (node->flags & UINodeFlag_DropSite && !collides
	    && ui_node_key_equal(ui->drop_target_key, node->key))
	{
		ui->drop_target_key = ui_node_key_zero();
	}

	// TODO(rnp): double click dragging

	// TODO(rnp): triple click dragging

	result.flags |= (!f32_equal(0, result.scroll.x) * UISignalFlag_ScrolledX);
	result.flags |= (!f32_equal(0, result.scroll.y) * UISignalFlag_ScrolledY);

	if (node->flags & UINodeFlag_MouseClickable && collides &&
	    (ui_node_key_nil(ui->hot_node_key) || ui_node_key_equal(ui->hot_node_key, node->key)) &&
	    (ui_node_key_nil(ui->active_node_key[UIMouseButtonKind_Left])   || ui_node_key_equal(ui->active_node_key[UIMouseButtonKind_Left],   node->key)) &&
	    (ui_node_key_nil(ui->active_node_key[UIMouseButtonKind_Middle]) || ui_node_key_equal(ui->active_node_key[UIMouseButtonKind_Middle], node->key)) &&
	    (ui_node_key_nil(ui->active_node_key[UIMouseButtonKind_Right])  || ui_node_key_equal(ui->active_node_key[UIMouseButtonKind_Right],  node->key)))
	{
		ui->hot_node_key = node->key;
	}

	if (node->flags & UINodeFlag_ViewScroll) {
		v2  offset  = node->view_scroll_offset;
		f32 clamp_x = Max(0, node->computed_size[Axis2_X] - node->parent->computed_size[Axis2_X]);
		f32 clamp_y = Max(0, node->computed_size[Axis2_Y] - node->parent->computed_size[Axis2_Y]);
		node->view_scroll_offset.x = Max(0, Sign(offset.x) * Min(Abs(offset.x), clamp_x));
		node->view_scroll_offset.y = Max(0, Sign(offset.y) * Min(Abs(offset.y), clamp_y));
	}

	// NOTE(rnp): activate text input
	if (ui_pressed(result) && !ui_node_key_equal(ui->text_input_state.node_key, node->key)) {
		ui->text_input_state.changed       = 1;
		ui->text_input_state.next_node_key = node->flags & UINodeFlag_TextInput ? node->key : ui_node_key_zero();
	}

	// NOTE(rnp): signal ended text input
	if (node->flags & UINodeFlag_TextInput &&
	    ui_node_key_equal(ui->text_input_state.last_node_key, node->key))
	{
		result.flags  |= UISignalFlag_TextCommit;
		result.string  = (str8){.length = ui->text_input_state.last_count,
		                        .data   = ui->text_input_state.last_buffer};
	}

	if (ui_pressed(result) && !context_menu_descendent)
		ui_context_menu_close();

	// TODO(rnp): move this to after UI building is complete
	b32 hot = ui_node_key_equal(ui->hot_node_key, node->key);
	if (hot) node->hot_t += HOVER_SPEED * dt_for_frame;
	else     node->hot_t -= HOVER_SPEED * dt_for_frame;
	node->hot_t = Clamp01(node->hot_t);

	return result;
}

function UINode *
ui_build_node_from_key(UINodeFlags flags, UINodeKey key)
{
	UINode *result = ui_node_from_key(key);

	b32 first_frame = ui_node_is_nil(result);
	b32 transient   = ui_node_key_equal(key, ui_node_key_zero());

	assert(first_frame || result->last_frame_active_index != ui_context->current_frame_index);

	if (first_frame) {
		result = transient ? 0 : ui_context->node_freelist;
		if (!ui_node_is_nil(result)) {
			SLLStackPop(ui_context->node_freelist, next_sibling);
		} else {
			result = push_struct_no_zero(transient ? ui_build_arena() : &ui_context->arena, UINode);
		}
		zero_struct(result);
	}

	// NOTE(rnp): reassigned per frame
	{
		result->parent = result->first_child = result->last_child = &ui_node_nil;
		result->next_sibling = result->previous_sibling = &ui_node_nil;
		result->child_count = 0;
	}

	if (first_frame && !transient) {
		UINodeHashBucket *hb = ui_context->node_hash_table + (key.value % UI_HASH_TABLE_COUNT);
		DLLInsert(&ui_node_nil, hb->first, hb->last, result, hash_next, hash_prev);
	}

	#define X(type, name, value_type, ...) result->name = ui_top_##name();
	UI_STACK_LIST
	#undef X

	result->last_frame_active_index = ui_context->current_frame_index;
	result->key = key;
	result->flags |= flags;

	if (!ui_node_is_nil(result->parent)) {
		DLLInsertLast(&ui_node_nil, result->parent->first_child, result->parent->last_child,
		              result, next_sibling, previous_sibling);
		result->parent->child_count++;
	}

	return result;
}

function UINode *
ui_node_from_string(UINodeFlags flags, str8 string)
{
	UINode *result = ui_build_node_from_key(flags, ui_key_from_string(string, ui_node_ancestor_key()));
	if (flags & UINodeFlag_DrawText) {
		if (ui_node_key_equal(ui_context->text_input_state.node_key, result->key))
			result->string = ui_text_input_string();
		else if (ui_node_key_equal(ui_context->text_input_state.last_node_key, result->key))
			result->string = ui_text_input_last_string();
		else
			result->string = string;
	}
	return result;
}

function print_format(2, 3) UINode *
ui_node_from_stringf(UINodeFlags flags, const char *format, ...)
{
	va_list args;
	va_start(args, format);
	str8 string = push_str8_fv(ui_build_arena(), format, args);
	va_end(args);
	UINode *result = ui_node_from_string(flags, string);
	return result;
}

function UISignal
ui_button(str8 string)
{
	UINode *node = ui_node_from_string(UINodeFlag_Clickable|
	                                   UINodeFlag_DrawBackground|
	                                   UINodeFlag_DrawBorder|
	                                   UINodeFlag_DrawText|
	                                   UINodeFlag_DrawHotEffects|
	                                   UINodeFlag_DrawActiveEffects,
	                                   string);
	UISignal result = ui_signal_from_node(node);
	return result;
}

function print_format(1, 2) UISignal
ui_buttonf(const char *format, ...)
{
	va_list args;
	va_start(args, format);
	str8 string = push_str8_fv(ui_build_arena(), format, args);
	va_end(args);
	UISignal result = ui_button(string);
	return result;
}

function UISignal
ui_toggle_button(b32 state, str8 string)
{
	UINode *node, *outer;

	UIAxisAlign(Axis2_Y, Center)
	UIAxisAlign(Axis2_X, Center)
	UIParent(ui_spacer(0))
	{
		UIPrefHeight(ui_pct(0.75f, 1.f))
		UIPrefWidth(ui_pct(0.75f, 1.f))
		UIBorderThickness(2.f)
		UIBorderColour(FG_COLOUR)
		outer = ui_node_from_string(UINodeFlag_Clickable|UINodeFlag_DrawBorder,
		                            push_str8_from_parts(ui_build_arena(), str8(""), string, str8("_outer")));

		UIParent(outer)
		UIPrefHeight(ui_pct(0.46f, 1.f))
		UIPrefWidth(ui_pct(0.46f, 1.f))
		UIBGColour(state ? FG_COLOUR : (v4){0})
		{
			node = ui_node_from_string(UINodeFlag_DrawBackground|
			                           UINodeFlag_DrawHotEffects|
			                           UINodeFlag_DrawActiveEffects,
			                           string);
			node->hot_t = outer->hot_t;
		}
	}

	UISignal result = ui_signal_from_node(outer);
	return result;
}

function print_format(2, 3) UISignal
ui_toggle_buttonf(b32 state, const char *format, ...)
{
	va_list args;
	va_start(args, format);
	str8 string = push_str8_fv(ui_build_arena(), format, args);
	va_end(args);
	UISignal result = ui_toggle_button(state, string);
	return result;
}

function UISignal
ui_label(str8 string)
{
	UINode *node = ui_node_from_string(UINodeFlag_DrawText, string);
	UISignal result = ui_signal_from_node(node);
	return result;
}

function print_format(1, 2) UISignal
ui_labelf(const char *format, ...)
{
	va_list args;
	va_start(args, format);
	str8 string = push_str8_fv(ui_build_arena(), format, args);
	va_end(args);
	UISignal result = ui_label(string);
	return result;
}

function UISignal
ui_label_button(str8 string)
{
	UINode *node = ui_node_from_string(UINodeFlag_DrawText|
	                                   UINodeFlag_Clickable|
	                                   UINodeFlag_DrawHotEffects|
	                                   UINodeFlag_DrawActiveEffects,
	                                   string);
	UISignal result = ui_signal_from_node(node);
	return result;
}

function print_format(1, 2) UISignal
ui_label_buttonf(const char *format, ...)
{
	va_list args;
	va_start(args, format);
	str8 string = push_str8_fv(ui_build_arena(), format, args);
	va_end(args);
	UISignal result = ui_label_button(string);
	return result;
}

function UISignal
ui_text_box(str8 string)
{
	UINode *node = ui_node_from_string(UINodeFlag_TextInput|
	                                   UINodeFlag_DrawText|
	                                   UINodeFlag_Clickable|
	                                   UINodeFlag_DrawHotEffects|
	                                   UINodeFlag_DrawActiveEffects,
	                                   string);
	UISignal result = ui_signal_from_node(node);
	return result;
}

function print_format(1, 2) UISignal
ui_text_boxf(const char *format, ...)
{
	va_list args;
	va_start(args, format);
	str8 string = push_str8_fv(ui_build_arena(), format, args);
	va_end(args);
	UISignal result = ui_text_box(string);
	return result;
}

function b32
ui_tweak_f32_compute_variable(UISignal signal, f32 *value, f32 text_scale, f32 scroll_scale, v2 limits)
{
	b32 result = 0;
	if (signal.flags) {
		f64 new_value = *value;
		if (signal.flags & UISignalFlag_TextCommit && ui_number_conversion_f64(signal.string, &new_value))
			new_value *= text_scale;

		if (signal.flags & UISignalFlag_ScrolledY)
			new_value += scroll_scale * signal.scroll.y;

		new_value = Clamp(new_value, limits.x, limits.y);

		result = !f32_equal(*value, (f32)new_value);
		*value = (f32)new_value;
	}
	return result;
}

function UI_CUSTOM_DRAW_FUNCTION(ui_custom_draw_frame_view)
{
	// TODO(rnp): we should always just draw inline, requires no raylib
	BeamformerUI        *ui    = ui_context;
	BeamformerFrameView *view  = node->custom_draw_context;
	BeamformerFrame     *frame = &view->frame;
	b32 is_1d = iv3_dimension(frame->points) == 1;

	v3 U = frame->voxel_transform.c[0].xyz;
	v3 V = frame->voxel_transform.c[1].xyz;

	v3 min_coordinate = m4_mul_v3(frame->voxel_transform, (v3){{0.f, 0.f, 0.f}});
	v3 max_coordinate = m4_mul_v3(frame->voxel_transform, (v3){{1.f, 1.f, 1.f}});

	v2 min_uv = plane_uv(min_coordinate, U, V);
	v2 max_uv = plane_uv(max_coordinate, U, V);

	v2 output_dim;
	output_dim.x = v3_magnitude(U);
	output_dim.y = v3_magnitude(V);

	// NOTE(rnp): may be different from UV if recompute in progress or Copy View
	v2 requested_dim;
	requested_dim.x = v3_magnitude(v3_sub(v3_scale(U, max_uv.x), v3_scale(U, min_uv.x)));
	requested_dim.y = v3_magnitude(v3_sub(v3_scale(V, max_uv.y), v3_scale(V, min_uv.y)));

	Rectangle tex_r;
	if (is_1d) {
		tex_r  = (Rectangle){0, 0, view->colour_image.width, -view->colour_image.height};
	} else {
		v2 pixels_per_meter = {
			.w = (f32)view->colour_image.width  / output_dim.w,
			.h = (f32)view->colour_image.height / output_dim.h,
		};

		/* NOTE(rnp): math to resize the texture without stretching when the view changes
		 * but the texture hasn't been (or cannot be) rebeamformed */
		v2 texture_points  = v2_mul(pixels_per_meter, requested_dim);
		v2 texture_start   = {
			.x = pixels_per_meter.x * 0.5f * (output_dim.x - requested_dim.x),
			.y = pixels_per_meter.y * (output_dim.y - requested_dim.y),
		};

		tex_r = (Rectangle){texture_start.x, texture_start.y, texture_points.x, texture_points.y};
	}

	NPatchInfo tex_np = { tex_r, 0, 0, 0, 0, NPATCH_NINE_PATCH };
	DrawTextureNPatch(make_raylib_texture(view), tex_np, rl_rect(node_rect), (Vector2){0}, 0, WHITE);

	TextSpec text_spec = {.font = &ui->small_font, .flags = TF_LIMITED|TF_OUTLINED,
	                      .colour = RULER_COLOUR, .outline_thick = 1, .outline_colour.a = 1,
	                      .limits.size.x = node_rect.size.w};
	if (view->ruler.state != RulerState_None) draw_view_ruler(view, *ui_build_arena(), node_rect, text_spec);
}

function b32
ui_rebuild_das_transform(u32 parameter_block, i32 dimension, v3 min, v3 max)
{
	BeamformerUI *ui = ui_context;

	b32 result = 0;
	m4 new_transform = m4_identity();

	BeamformerParameterBlock *pb = beamformer_parameter_block(ui->shared_memory, parameter_block);

	m4 das_transform = pb->parameters.das_voxel_transform;

	switch (dimension) {
	case 1:{new_transform = das_transform_1d(min, max);}break;
	case 3:{new_transform = das_transform_3d(min, max);}break;
	case 2:{
		v3 U = v3_normalize(das_transform.c[0].xyz);
		v3 V = v3_normalize(das_transform.c[1].xyz);
		v3 N = cross(V, U);

		v2 min_2d = {{min.E[0], min.E[1]}};
		v2 max_2d = {{max.E[0], max.E[1]}};

		new_transform = das_transform_2d_with_normal(N, min_2d, max_2d, 0);

		v3 rotation_axis = cross(v3_normalize(new_transform.c[0].xyz), N);

		m4 R = m4_rotation_about_axis(rotation_axis, ui->beamform_plane);
		m4 T = m4_translation(v3_scale(m4_mul_v3(R, N), ui->off_axis_position));

		new_transform = m4_mul(T, m4_mul(R, new_transform));
	}break;
	}

	new_transform = m4_mul(new_transform, m4_inverse(das_transform));

	BeamformerComputePlan *cp = ui->beamformer_context->compute_context.compute_plans[parameter_block];
	if (cp) {
		result |= !m4_equal(new_transform, cp->ui_voxel_transform);
		memory_copy(cp->ui_voxel_transform.E, new_transform.E, sizeof(new_transform));
	}

	if (result) {
		mark_parameter_block_region_dirty(ui->shared_memory, parameter_block,
		                                  BeamformerParameterBlockRegion_Parameters);
	}

	return result;
}

function void
ui_scroll_begin(void)
{
	UINode *outer, *inner, *clip, *child;

	UIChildLayoutAxis(Axis2_Y)
	UIParent(ui_spacer(0))
	{
		UIChildLayoutAxis(Axis2_X)
		UIPrefWidth(ui_pct(1.f, 0.5f))
		UIPrefHeight(ui_pct(1.f, 0.5f))
		UIFontSize(30.f)
		outer = ui_node_from_string(UINodeFlag_Scroll, str8("###scroll_box"));

		ui_padh(UI_NODE_PAD);
	}

	UIParent(outer)
	{
		ui_padw(UI_NODE_PAD);
		UIPrefWidth(ui_pct(1.f, 0.5f))
		UIPrefHeight(ui_pct(1.f, 0.5f))
		UIChildLayoutAxis(Axis2_Y)
		inner = ui_node_from_string(0, str8("###scroll_inner"));
	}

	UIParent(inner)
	clip = ui_node_from_string(UINodeFlag_Clip|
	                           UINodeFlag_ViewScroll|
	                           UINodeFlag_AllowOverflow|
	                           0, str8("###scroll_clip"));

	UIParent(clip)
	{
		UIPrefWidth(ui_children_sum(1.f))
		UIPrefHeight(ui_children_sum(1.f))
		child = ui_node_from_string(0, str8("###scroll_child"));
	}

	ui_push_parent(child);
}

function void
ui_scroll_end(void)
{
	BeamformerUI *ui = ui_context;
	UINode *child = ui_pop_parent();
	UINode *clip  = child->parent;
	UINode *inner = clip->parent;
	UINode *outer = inner->parent;

	v2 scroll_offset  = clip->view_scroll_offset;

	str8 labels[2][2] = {
		[Axis2_X] = {str8_comp("<"), str8_comp(">")},
		[Axis2_Y] = {str8_comp("^"), str8_comp("v")},
	};

	f32 btn_size = (f32)ui_font_for_node(outer).baseSize;

	UINode *axis_parents[] = {[Axis2_X] = inner, [Axis2_Y] = outer};
	for EachElement(axis_parents, axis) UIParent(axis_parents[axis]) {
		b32 build_scrollbar = btn_size < axis_parents[axis]->computed_size[axis] &&
		                      child->computed_size[axis] > axis_parents[axis]->computed_size[axis];

		// NOTE(rnp): vertical scroll bar shares padding on bottom with horizontal
		// scroll bar so padding was already pushed, if we aren't drawing the horizontal
		// scroll bar we need to avoid a double pad
		if (axis == Axis2_Y || build_scrollbar) {
			UIChildLayoutAxis(axis2_flip(axis))
			ui_pads(UI_NODE_PAD);
		}

		if (build_scrollbar)
		UIAxisSize(axis2_flip(axis), ui_px(12.f, 1.f))
		UIChildLayoutAxis(axis)
		UIParent(axis_parents[axis])
		{
			UINode *parent = axis_parents[axis];
			f32 d_size     = child->computed_size[axis] - clip->computed_size[axis];
			f32 used_pct   = clip->computed_size[axis] / child->computed_size[axis];
			f32 rem_pct    = 1.f - used_pct;
			f32 before_pct = rem_pct - (d_size - scroll_offset.E[axis]) / child->computed_size[axis];
			f32 after_pct  = rem_pct - before_pct;

			UINode *scroll_container;
			UIAxisAlign(axis2_flip(axis), Center)
			UIAxisSize(axis, ui_px(parent->computed_size[axis], 1.f))
			scroll_container = ui_spacer(0);

			UIAxisSize(axis2_flip(axis), ui_pct(1.f, 0.5f))
			UIFontSize(outer->font_size)
			UIParent(scroll_container)
			{
				UISignal signal;
				// TODO(rnp): icons
				UIFlags(UINodeFlag_IconText)
				UIAxisSize(axis2_flip(axis), ui_text_dim(1.f, 1.f))
				UIAxisSize(axis, ui_text_dim(1.f, 1.f))
				signal = ui_label_button(labels[axis][0]);
				if (signal.flags & UISignalFlag_LeftPressed) {
					// TODO(rnp): handle repeat
					scroll_offset.E[axis] -= btn_size * 0.5f;
				}

				ui_pads(3.f);

				UISignalFlags bar_flags = 0;

				UIBorderColour((v4){0})
				UIFlags(UINodeFlag_Clickable|UINodeFlag_DrawBorder|UINodeFlag_DrawHotEffects)
				UIAxisSize(axis, ui_pct(before_pct, 0.5f))
				bar_flags |= ui_signal_from_node(ui_node_from_string(0, str8("###before"))).flags;

				UIBGColour(FG_COLOUR)
				UIAxisSize(axis, ui_pct(used_pct, 0.5f))
				UIFlags(UINodeFlag_Clickable|UINodeFlag_DrawBackground|UINodeFlag_DrawHotEffects)
				signal = ui_signal_from_node(ui_node_from_string(0, str8("###used")));
				bar_flags |= signal.flags;

				UIBorderColour((v4){0})
				UIFlags(UINodeFlag_Clickable|UINodeFlag_DrawBorder|UINodeFlag_DrawHotEffects)
				UIAxisSize(axis, ui_pct(after_pct , 0.5f))
				bar_flags |= ui_signal_from_node(ui_node_from_string(0, str8("###after"))).flags;

				if (bar_flags & (UISignalFlag_Dragging|UISignalFlag_Pressed)) {
					f32 off_pct = rect_uv(ui->last_mouse, ui_node_rect(clip)).E[axis] - 0.5f * used_pct;
					scroll_offset.E[axis] = Clamp01(off_pct) * child->computed_size[axis];
				}

				ui_pads(3.f);

				UIFlags(UINodeFlag_IconText)
				UIAxisSize(axis2_flip(axis), ui_text_dim(1.f, 1.f))
				UIAxisSize(axis, ui_text_dim(1.f, 1.f))
				signal = ui_label_button(labels[axis][1]);
				if (signal.flags & UISignalFlag_LeftPressed) {
					// TODO(rnp): handle repeat
					scroll_offset.E[axis] += btn_size * 0.5f;
				}

			}

			// NOTE(rnp): vertical scroll bar needs padding next to it but must share
			// padding on the bottom with the horizontal scrollbar
			if (axis == Axis2_Y) ui_padw(UI_NODE_PAD);
		}
	}

	// TODO(rnp): view scroll is being added to ViewScroll node in ui_signal_from_node maybe we are ignoring it?
	UISignal signal = ui_signal_from_node(outer);
	scroll_offset = v2_sub(scroll_offset, v2_scale(signal.scroll, btn_size * 0.5f));

	scroll_offset.x = Max(0, Min(scroll_offset.x, child->computed_size[Axis2_X] - clip->computed_size[Axis2_X]));
	scroll_offset.y = Max(0, Min(scroll_offset.y, child->computed_size[Axis2_Y] - clip->computed_size[Axis2_Y]));
	clip->view_scroll_offset = scroll_offset;
}

typedef struct {
	Axis2 axis;
	f32   start_value;
	f32   end_value;
	u32   segments;
} UIDrawScaleBarData;

function UI_CUSTOM_DRAW_FUNCTION(ui_custom_draw_scale_bar)
{
	UIDrawScaleBarData *info = node->custom_draw_context;

	b32 draw_plus = Sign(info->end_value) != Sign(info->start_value);

	Font font        = ui_font_for_node(node);
	v2   start_point = node_rect.pos;
	v2   end_point   = node_rect.pos;

	if (info->axis == Axis2_Y) start_point.y += node_rect.size.y;
	else                       end_point.x   += node_rect.size.x;

	end_point = v2_sub(end_point, start_point);

	rlPushMatrix();
	rlTranslatef(start_point.x, start_point.y, 0);
	rlRotatef(atan2_f32(end_point.y, end_point.x) * 180 / PI, 0, 0, 1);

	Stream buf = arena_stream(*ui_build_arena());
	f32 inc       = v2_magnitude(end_point) / (f32)info->segments;
	f32 value_inc = (info->end_value - info->start_value) / (f32)info->segments;
	f32 value     = info->start_value;

	v2 sp = {0}, ep = {.y = RULER_TICK_LENGTH};
	v2 tp = {{(f32)font.baseSize / 2.0f, ep.y + RULER_TEXT_PAD}};

	TextSpec text_spec = {.font = &font, .rotation = 90.0f, .colour = node->text_colour, .flags = TF_ROTATED};
	if (node->flags & UINodeFlag_DrawHotEffects)
		text_spec.colour = v4_lerp(text_spec.colour, HOVERED_COLOUR, node->hot_t);

	Color rl_txt_colour = colour_from_normalized(node->text_colour);
	for (u32 j = 0; j <= info->segments; j++) {
		DrawLineEx(rl_v2(sp), rl_v2(ep), 4.f, rl_txt_colour);

		stream_reset(&buf, 0);
		if (draw_plus && value > 0) stream_append_byte(&buf, '+');
		stream_append_f64(&buf, value, Abs(value_inc) < 1 ? 100 : 10);
		stream_append_s8(&buf, s8("mm"));
		draw_text(stream_to_str8(&buf), tp, &text_spec);

		value += value_inc;
		sp.x  += inc;
		ep.x  += inc;
		tp.x  += inc;
	}

	rlPopMatrix();
}

function UISignal
ui_build_scale_bar(Axis2 axis, v2 min, v2 max)
{
	Font font = ui_font_for_node(ui_top_parent());
	f32  label_size = measure_text(font, str8("-288.88mm")).w;

	UISignal result;
	UIAxisSize(axis2_flip(axis), ui_px(RULER_TICK_LENGTH + RULER_TEXT_PAD + label_size, 1.f))
	UIFlags(UINodeFlag_Clickable|UINodeFlag_Scroll|UINodeFlag_DrawHotEffects|UINodeFlag_CustomDraw)
	{
		UINode *node = ui_node_from_string(0, str8("###scale_bar"));
		result = ui_signal_from_node(node);

		UIDrawScaleBarData *info = push_struct(ui_build_arena(), UIDrawScaleBarData);
		node->custom_draw_function = ui_custom_draw_scale_bar;
		node->custom_draw_context  = info;

		Rect tick_rect = ui_node_rect(node);
		if (tick_rect.size.E[axis] > 0) {
			info->axis        = axis;
			info->segments    = (u32)(tick_rect.size.E[axis] / (1.5f * font.baseSize));
			info->start_value = min.E[axis] * 1e3;
			info->end_value   = max.E[axis] * 1e3;
			if (axis == Axis2_Y) swap(info->start_value, info->end_value);
		}
	}
	return result;
}

function void
ui_build_frame_view(UINode *container, BeamformerFrameView *view)
{
	BeamformerUI    *ui    = ui_context;
	BeamformerFrame *frame = &view->frame;
	b32 is_1d = iv3_dimension(frame->points) == 1;
	f32 txt_w = measure_text(ui->small_font, str8(" -288.8 mm")).w;
	f32 scale_bar_size = 1.2f * txt_w + RULER_TICK_LENGTH;

	v3 U = frame->voxel_transform.c[0].xyz;
	v3 V = frame->voxel_transform.c[1].xyz;

	v2 output_dim;
	output_dim.x = v3_magnitude(U);
	output_dim.y = v3_magnitude(V);

	U = v3_scale(U, 1.f / output_dim.x);
	V = v3_scale(V, 1.f / output_dim.y);

	v3 min_coordinate = m4_mul_v3(frame->voxel_transform, (v3){{0.f, 0.f, 0.f}});
	v3 max_coordinate = m4_mul_v3(frame->voxel_transform, (v3){{1.f, 1.f, 1.f}});

	v2 min_2d = {{v3_dot(U, min_coordinate), v3_dot(V, min_coordinate)}};
	v2 max_2d = {{v3_dot(U, max_coordinate), v3_dot(V, max_coordinate)}};

	f32 aspect = is_1d ? 1.0f : output_dim.w / output_dim.h;

	Rect display_rect = ui_node_rect(container);
	Rect vr = display_rect;
	v2 scale_bar_area = {0};
	if (view->scale_bar_active[Axis2_Y]) {
		vr.pos.y         += 0.5f * (f32)ui->small_font.baseSize;
		scale_bar_area.x += scale_bar_size;
		scale_bar_area.y += (f32)ui->small_font.baseSize;
	}

	if (view->scale_bar_active[Axis2_X]) {
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

	UIParent(container)
	{
		ui_padh(vr.pos.y - display_rect.pos.y);

		UINode *frame_top, *frame_view;
		UIChildLayoutAxis(Axis2_X)
		UIPrefHeight(ui_children_sum(1.f))
		UIPrefWidth(ui_children_sum(1.f))
		frame_top = ui_node_from_string(0, str8("###frame_view_top"));

		UIParent(frame_top)
		UIPrefHeight(ui_px(vr.size.h, 1.f))
		{
			ui_padw(vr.pos.x - display_rect.pos.x);

			UIChildLayoutAxis(Axis2_Y)
			UIPrefWidth(ui_px(vr.size.w, 1.f))
			frame_view = ui_node_from_string(UINodeFlag_Clickable|
			                                 UINodeFlag_CustomDraw|
			                                 UINodeFlag_Clip|
			                                 UINodeFlag_Scroll|
			                                 0, str8("###frame_view"));
			frame_view->custom_draw_function = ui_custom_draw_frame_view;
			frame_view->custom_draw_context  = view;

			if (view->kind != BeamformerFrameViewKind_3DXPlane)
			UIParent(frame_view)
			UIChildLayoutAxis(Axis2_X)
			UIPrefHeight(ui_children_sum(1.f))
			UIPrefWidth(ui_pct(1.f, 0.5f))
			UITextOutlineColour((v4){.a = 1.f})
			UITextOutlineThickness(1.f)
			UITextColour(RULER_COLOUR)
			{
				ui_padh(UI_NODE_PAD);

				UIFontSize(30.f)
				UIParent(ui_spacer(0))
				{
					ui_spacer(0);

					UIPrefHeight(ui_text_dim(1.f, 1.f))
					UIPrefWidth(ui_text_dim(1.f, 1.f))
					ui_label(push_acquisition_kind(ui_build_arena(), view->frame.acquisition_kind,
					                               view->frame.compound_count, view->frame.contrast_mode));

					ui_padw(2.f * UI_NODE_PAD);
				}

				UIPrefHeight(ui_pct(1.f, 0.5f)) ui_spacer(0);

				UIFontSize(24.f)
				UIAxisAlign(Axis2_Y, Right)
				UIParent(ui_spacer(0))
				{
					ui_padw(2.f * UI_NODE_PAD);

					UINode *label_column, *value_column, *unit_column;
					UIAxisAlign(Axis2_X, Left)
					UIAxisAlign(Axis2_Y, Left)
					UIPrefWidth(ui_children_sum(1.f))
					UIParent(ui_spacer(0))
					UIChildLayoutAxis(Axis2_Y)
					{
						label_column = ui_node_from_string(0, str8("###labels"));
						ui_padw(UI_NODE_PAD);
						value_column = ui_node_from_string(0, str8("###values"));
						ui_padw(UI_NODE_PAD);
						unit_column  = ui_node_from_string(0, str8("###units"));
					}

					UIPrefWidth(ui_text_dim(1.f, 1.f))
					UIPrefHeight(ui_text_dim(1.f, 1.f))
					{
						if (view->log_scale) {
							UIParent(label_column) ui_label(str8("Dynamic Range:"));
							UIParent(unit_column)  ui_label(str8("[dB]"));
							UIParent(value_column)
							UIFlags(UINodeFlag_Scroll|UINodeFlag_TextInputNumeric)
							{
								UISignal signal = ui_text_boxf("%0.2f###dynamic_range", view->dynamic_range);
								view->dirty |= ui_tweak_f32_compute_variable(signal, &view->dynamic_range, 1.f, 0.5f, V2_INFINITY);
							}
						}

						// TODO(rnp): ui_em after text height matches correctly
						f32 spacer_height;
						UIParent(label_column) spacer_height = ui_label(str8("Gamma:")).node->computed_size[Axis2_Y];
						UIParent(unit_column)  ui_padh(spacer_height);
						UIParent(value_column)
						UIFlags(UINodeFlag_Scroll|UINodeFlag_TextInputNumeric)
						{
							UISignal signal = ui_text_boxf("%0.2f###gamma", view->gamma);
							view->dirty |= ui_tweak_f32_compute_variable(signal, &view->gamma, 1.f, 0.025f, V2_INFINITY);
						}

						UIParent(label_column) spacer_height = ui_label(str8("Threshold:")).node->computed_size[Axis2_Y];
						UIParent(unit_column)  ui_padh(spacer_height);
						UIParent(value_column)
						UIFlags(UINodeFlag_Scroll|UINodeFlag_TextInputNumeric)
						{
							UISignal signal = ui_text_boxf("%0.2f###threshold", view->threshold);
							view->dirty |= ui_tweak_f32_compute_variable(signal, &view->threshold, 1.f, 1.f, V2_INFINITY);
						}
					}

					UIPrefWidth(ui_pct(1.f, 0.5f)) ui_spacer(0);

					Rect nr = ui_node_rect(frame_view);
					if (point_in_rect(ui->last_mouse, nr) && ui->drag_panel == 0) {
						b32 is_1d = iv3_dimension(view->frame.points) == 1;
						v2 world = screen_point_to_world_2d(ui->last_mouse, nr.pos, v2_add(nr.pos, nr.size),
						                                    min_2d, max_2d);
						world = v2_scale(world, 1e3f);
						if (is_1d) world.y = ((nr.pos.y + nr.size.y) - ui->last_mouse.y) / nr.size.y;

						UIPrefWidth(ui_text_dim(1.f, 1.f))
						UIPrefHeight(ui_text_dim(1.f, 1.f))
						ui_labelf("{%0.2f%s, %0.2f}", world.x, is_1d ? " mm" : "", world.y);
					}

					ui_padw(2.f * UI_NODE_PAD);
				}

				ui_padh(UI_NODE_PAD);
			}

			UISignal signal = ui_signal_from_node(frame_view);
			// TODO(rnp): is this correct for x-plane?
			if (ui_tweak_f32_compute_variable(signal, &view->threshold, 1.f, 1.f, V2_INFINITY))
				view->dirty = 1;

			if (ui_pressed(signal) && view->kind != BeamformerFrameViewKind_3DXPlane) {
				view->ruler.state = circular_add(view->ruler.state, 1, RulerState_Count);
				// TODO(rnp): cleanup: this
				v3 p = world_point_from_plane_uv(frame->voxel_transform, rect_uv(ui->last_mouse, ui_node_rect(frame_view)));
				switch (view->ruler.state) {
				InvalidDefaultCase;
				case RulerState_None:{}break;
				case RulerState_Start:{view->ruler.start = p;}break;
				case RulerState_Hold:{ view->ruler.end   = p;}break;
				}
			}

			if (view->scale_bar_active[Axis2_Y]) {
				signal = ui_build_scale_bar(Axis2_Y, min_2d, max_2d);
				if ui_pressed(signal) {
					str8 msg = str8("Y scale bar pressed\n");
					os_console_log(msg.data, msg.length);
				}

				if (signal.flags & UISignalFlag_ScrolledY) {
					str8 msg = str8("Y scale bar scrolled\n");
					os_console_log(msg.data, msg.length);
				}
			}
		}

		if (view->scale_bar_active[Axis2_X])
		UIChildLayoutAxis(Axis2_X)
		UIPrefHeight(ui_children_sum(1.0f))
		UIPrefWidth(ui_children_sum(1.0f))
		UIParent(ui_node_from_string(0, str8("###frame_view_bot")))
		UIPrefWidth(ui_px(vr.size.w, 1.f))
		{
			f32 top_position_offset = frame_view->computed_position[Axis2_X] - display_rect.pos.x;
			ui_padw(top_position_offset);

			UISignal signal;
			signal = ui_build_scale_bar(Axis2_X, min_2d, max_2d);
			if ui_pressed(signal) {
				str8 msg = str8("X scale bar pressed\n");
				os_console_log(msg.data, msg.length);
			}

			if (signal.flags & UISignalFlag_ScrolledY) {
				str8 msg = str8("X scale bar scrolled\n");
				os_console_log(msg.data, msg.length);
			}

			ui_padw(display_rect.size.x - top_position_offset);
		}
	}
}

function void
ui_build_compute_stats(BeamformerComputePlan *cp, f32 broken_shader_t)
{
	ComputeShaderStats *stats = ui_context->beamformer_context->compute_shader_stats;
	f32 compute_time_sum = 0;
	u32 stages           = stats->table.shader_count;

	for (u32 index = 0; index < stages; index++)
		compute_time_sum += stats->average_times[index];

	UIFontSize(30.f)
	UIScroll()
	{
		ui_top_parent()->child_layout_axis = Axis2_X;

		UINode *label_column, *value_column, *unit_column;
		UIAxisAlign(Axis2_X, Left)
		UIChildLayoutAxis(Axis2_Y)
		UIPrefWidth(ui_children_sum(1.0f))
		UIPrefHeight(ui_children_sum(1.0f))
		{
			label_column = ui_node_from_string(0, str8("###labels"));
			ui_padw(UI_NODE_PAD);
			value_column = ui_node_from_string(0, str8("###values"));
			ui_padw(UI_NODE_PAD);
			unit_column  = ui_node_from_string(0, str8("###units"));
		}

		UIPrefWidth(ui_text_dim(1.0f, 1.0f))
		UIPrefHeight(ui_text_dim(1.05f, 1.0f))
		{
			for EachIndex(stages, it) {
				v4 label_colour = FG_COLOUR;
				if (vk_pipeline_valid(cp->vulkan_pipelines[it]) == 0 &&
				    stats->table.shader_ids[it] != BeamformerShaderKind_Hilbert)
				{
					v4_lerp(FG_COLOUR, FOCUSED_COLOUR, ease_in_out_quartic(broken_shader_t));
				}

				str8 shader = str8_from_s8(beamformer_shader_names[stats->table.shader_ids[it]]);

				UITextColour(label_colour)
				UIParent(label_column) ui_labelf("%.*s:###csl%u", (i32)shader.length, shader.data, (u32)it);
				UIParent(value_column) ui_labelf("%0.2e###csv%u", stats->average_times[it], (u32)it);
				UIParent(unit_column)  ui_labelf("[s]###csu%u", (u32)it);
			}

			UIParent(label_column) ui_label(str8("Compute Total:"));
			UIParent(value_column) ui_labelf("%0.2e (%0.2f)###csv_total", compute_time_sum,
			                                 compute_time_sum > 0.f ? 1.0f / compute_time_sum : 0.f);
			UIParent(unit_column)  ui_label(str8("[s] (FPS)###csv_total"));

			UIParent(label_column) ui_label(str8("RF Upload Delta:"));
			UIParent(value_column) ui_labelf("%0.2e (%0.2f)###csv_upload", stats->rf_time_delta_average,
			                                 stats->rf_time_delta_average > 0.f ? 1.0f / stats->rf_time_delta_average
			                                                                    : 0.f);
			UIParent(unit_column)  ui_label(str8("[s] (FPS)###csv_upload"));

			u32 rf_size = ui_context->beamformer_context->compute_context.rf_buffer.active_rf_size;
			UIParent(label_column) ui_label(str8("Input RF Size:"));
			UIParent(value_column) ui_labelf("%u###csv_rf_size", rf_size);
			UIParent(unit_column)  ui_label(str8("[B/F]###csv_rf_size"));

			UIParent(label_column) ui_label(str8("DAS RF Size:"));
			UIParent(value_column) ui_labelf("%u###csv_das_size", cp->rf_size);
			UIParent(unit_column)  ui_label(str8("[B/F]###csv_das_size"));
		}
	}
}

function void
ui_build_parameters_listing(UITreeNode *panel)
{
	BeamformerUI *ui = ui_context;

	UIFontSize(30.f)
	UIScroll()
	{
		ui_top_parent()->child_layout_axis = Axis2_X;

		UINode *label_column, *value_column, *unit_column;
		UIChildLayoutAxis(Axis2_Y)
		UIPrefWidth(ui_children_sum(1.0f))
		UIPrefHeight(ui_children_sum(1.0f))
		{
			UIAxisAlign(Axis2_X, Left)   label_column = ui_node_from_string(0, str8("###labels"));
			ui_padw(UI_NODE_PAD);
			UIAxisAlign(Axis2_X, Center) value_column = ui_node_from_string(0, str8("###values"));
			ui_padw(UI_NODE_PAD);
			UIAxisAlign(Axis2_X, Right)  unit_column  = ui_node_from_string(0, str8("###units"));
		}

		f32 line_pad_pct = 1.05f;
		UIPrefWidth(ui_text_dim(1.f, 1.f))
		UIPrefHeight(ui_text_dim(line_pad_pct, 1.f))
		{
			BeamformerUIParameters *bp = &ui_context->parameters;
			UIParent(label_column) ui_label(str8("Sampling Frequency"));
			UIParent(value_column) ui_labelf("%0.2f##sampling", bp->sampling_frequency * 1e-6);
			UIParent(unit_column)  ui_label(str8("[MHz]##sampling"));

			UIParent(label_column) ui_label(str8("Demodulation Frequency"));
			UIParent(value_column) ui_labelf("%0.2f###demod", bp->demodulation_frequency * 1e-6);
			UIParent(unit_column)  ui_label(str8("[MHz]##demod"));

			UIParent(label_column) ui_label(str8("Speed of Sound"));
			UIParent(unit_column)  ui_label(str8("[m/s]"));
			UIParent(value_column)
			UIFlags(UINodeFlag_Scroll|UINodeFlag_TextInputNumeric)
			{
				UISignal signal = ui_text_boxf("%0.2f###sound", bp->speed_of_sound);
				if (ui_tweak_f32_compute_variable(signal, &bp->speed_of_sound, 1.f, 10.f, (v2){{0, inf32()}}))
					ui->flush_parameters = 1;
			}

			u32 parameter_block = panel->u.parameter_listing.parameter_block;
			b32 rebuild_transform = 0;

			BeamformerParameterBlock *pb = beamformer_parameter_block(ui->shared_memory, parameter_block);
			BeamformerComputePlan    *cp = ui->beamformer_context->compute_context.compute_plans[parameter_block];
			m4 das_transform = pb->parameters.das_voxel_transform;
			if (cp) das_transform = m4_mul(cp->ui_voxel_transform, das_transform);
			v3 coordinates[2] = {
				m4_mul_v3(das_transform, (v3){{0.0f, 0.0f, 0.0f}}),
				m4_mul_v3(das_transform, (v3){{1.0f, 1.0f, 1.0f}}),
			};

			i32 dimension = iv3_dimension(bp->output_points.xyz);
			if (dimension > 0) {
				read_only local_persist str8 dimension_strings[3][2] = {
					{str8_comp("Start Point"),    str8_comp("End Point")   },
					{str8_comp("Lateral Extent"), str8_comp("Axial Extent")},
					{str8_comp("Min Corner"),     str8_comp("Max Corner")  },
				};

				for (u32 index = 0; index < 2; index++) {
					UIParent(label_column)
					{
						UISignal signal = ui_button(dimension_strings[dimension - 1][index]);
						signal.node->flags &= ~(UINodeFlag_DrawBackground|UINodeFlag_DrawBorder);
						if ui_pressed(signal)
							panel->u.parameter_listing.expand_coordinate[index] ^= 1u;
					}

					f32 values[3] = {coordinates[index].x, coordinates[index].y, coordinates[index].z};
					u32 value_count = dimension == 2 ? 2 : 3;
					v3  normalized_axis = v3_normalize(das_transform.c[index].xyz);
					if (dimension == 2) {
						values[0] = v3_dot(normalized_axis, coordinates[0]);
						values[1] = v3_dot(normalized_axis, coordinates[1]);
					}

					if (panel->u.parameter_listing.expand_coordinate[index]) {
						UIPrefHeight(ui_px((f32)ui_font_for_node(value_column).baseSize * line_pad_pct, 1.f))
						{
							UIParent(value_column) ui_spacer(0);
							UIParent(unit_column)  ui_spacer(0);
						}

						read_only local_persist str8 axis_strings[2][3] = {
							{str8_comp("  X:"),   str8_comp("  Y:"),   str8_comp("  Z:")},
							{str8_comp("  Min:"), str8_comp("  Max:"),                  },
						};
						str8 *strs  = dimension == 2 ? axis_strings[1] : axis_strings[0];
						for EachIndex(value_count, it) {
							UIParent(label_column) ui_labelf("  %.*s##label%u_%u",
							                                 (i32)strs[it].length, strs[it].data,
							                                 index, (u32)it);
							UIParent(unit_column)  ui_labelf("[mm]##%u_%u", index, (u32)it);
							UIParent(value_column)
							UIFlags(UINodeFlag_Scroll|UINodeFlag_TextInputNumeric)
							{
								UISignal signal = ui_text_boxf("%0.2f###%u_%u", values[it] * 1e3f, index, (u32)it);
								rebuild_transform |= ui_tweak_f32_compute_variable(signal, values + it,
								                                                   1e-3f, 0.5e-3f, V2_INFINITY);
							}
						}
					} else {
						UIParent(unit_column)  ui_labelf("[mm]##dim%u", index);

						UINode *group;
						UIParent(value_column)
						UIChildLayoutAxis(Axis2_X)
						UIPrefWidth(ui_children_sum(1.f))
						UIPrefHeight(ui_children_sum(1.f))
							group = ui_spacer(0);

						UIParent(group)
						{
							ui_labelf("{##%u", index);
							for EachIndex(value_count, it) {
								if (it != 0) ui_labelf(", ##%u_%u", index, (u32)it);
								UIFlags(UINodeFlag_Scroll|UINodeFlag_TextInputNumeric)
								{
									UISignal signal = ui_text_boxf("%0.2f###%u_%u", values[it] * 1e3f, index, (u32)it);
									rebuild_transform |= ui_tweak_f32_compute_variable(signal, values + it,
									                                                   1e-3f, 0.5e-3f, V2_INFINITY);
								}
							}
							ui_labelf("}##%u", index);
						}
					}

					if (dimension == 2) {
						coordinates[0].E[index] = values[0];
						coordinates[1].E[index] = values[1];
					}
				}
			}

			if (dimension == 2) {
				UIParent(label_column) ui_label(str8("Off Axis Position"));
				UIParent(unit_column)  ui_label(str8("[mm]##off_axis"));
				UIParent(value_column)
				UIFlags(UINodeFlag_Scroll|UINodeFlag_TextInputNumeric)
				{
					UISignal signal = ui_text_boxf("%0.2f###off_axis", ui->off_axis_position * 1e3f);
					rebuild_transform |= ui_tweak_f32_compute_variable(signal, &ui->off_axis_position,
					                                                   1e-3f, 0.1e-3f, V2_INFINITY);
				}

				UIParent(label_column) ui_label(str8("Beamform Plane"));
				UIParent(unit_column)  UIPrefHeight(ui_em(1.f, 1.f)) ui_spacer(0);
				UIParent(value_column)
				UIFlags(UINodeFlag_Scroll|UINodeFlag_TextInputNumeric)
				{
					UISignal signal = ui_text_boxf("%0.2f###beamform_plane", ui->beamform_plane);
					rebuild_transform |= ui_tweak_f32_compute_variable(signal, &ui->beamform_plane,
					                                                   1.f, 0.025f, (v2){{-1.f, 1.f}});
				}
			}

			UIParent(label_column) ui_label(str8("F#"));
			UIParent(unit_column)  UIPrefHeight(ui_em(1.f, 1.f)) ui_spacer(0);
			UIParent(value_column)
			UIFlags(UINodeFlag_Scroll|UINodeFlag_TextInputNumeric)
			{
				UISignal signal = ui_text_boxf("%0.2f###f_number", bp->f_number);
				if (ui_tweak_f32_compute_variable(signal, &bp->f_number, 1.f, 0.05f, (v2){{0, inf32()}}))
					ui->flush_parameters = 1;
			}

			UIParent(label_column) ui_label(str8("Interpolation"));
			UIParent(unit_column)  ui_build_node_from_key(0, ui_node_key_zero());
			UIParent(value_column)
			UIFlags(UINodeFlag_Scroll)
			{
				str8 label = str8_from_s8(beamformer_interpolation_mode_strings[bp->interpolation_mode]);
				UISignal signal = ui_label_button(label);
				if ui_pressed(signal) {
					bp->interpolation_mode++;
					bp->interpolation_mode %= countof(beamformer_interpolation_mode_strings);
					ui->flush_parameters = 1;
				}

				if (signal.flags & UISignalFlag_ScrolledY) {
					bp->interpolation_mode = circular_add(bp->interpolation_mode, signal.scroll.y,
					                                      countof(beamformer_interpolation_mode_strings));
					ui->flush_parameters = 1;
				}
			}

			UIParent(label_column) ui_label(str8("Coherency Weighting"));
			UIParent(unit_column)  UIPrefHeight(ui_em(1.0f, 1.0f)) ui_spacer(0);
			UIParent(value_column)
			UIFlags(UINodeFlag_Scroll)
			{
				UISignal signal = ui_label_button(bp->coherency_weighting ?
				                                  str8("True###coherency_weighting") :
				                                  str8("False###coherency_weighting"));
				if (signal.flags & (UISignalFlag_Pressed|UISignalFlag_Scrolled)) {
					bp->coherency_weighting = !bp->coherency_weighting;
					ui->flush_parameters = 1;
				}
			}

			if (rebuild_transform) {
				if (ui_rebuild_das_transform(parameter_block, dimension, coordinates[0], coordinates[1]))
					ui->flush_parameters = 1;
			}
		}
	}
}

function UISignal
ui_panel_label(UITreeNode *panel)
{
	Stream sb = arena_stream(*ui_build_arena());
	switch (panel->kind) {
	InvalidDefaultCase;
	case BeamformerPanelKind_ComputeStats:{stream_append_s8(&sb, s8("Compute Stats"));}break;
	case BeamformerPanelKind_FrameViewLive:{stream_append_s8(&sb, s8("Frame View"));}break;
	case BeamformerPanelKind_LiveImagingControls:{stream_append_s8(&sb, s8("Live Controls"));}break;
	case BeamformerPanelKind_ParameterListing:{
		stream_append_s8(&sb, s8("Parameter Listing ["));
		stream_append_u64(&sb, panel->u.parameter_listing.parameter_block);
		stream_append_s8(&sb, s8("]#"));
	}break;
	}
	stream_append_s8(&sb, s8("##"));
	stream_append_hex_u64(&sb, (u64)panel);
	s8 title_s8 = arena_stream_commit(ui_build_arena(), &sb);

	UISignal result;
	UIPrefWidth(ui_text_dim(1.f, 1.f))
	UIPrefHeight(ui_text_dim(1.4f, 1.f))
	result = ui_label(str8_from_s8(title_s8));

	return result;
}

function UITreeNode *
ui_panel_group_equip(UINode *node, UITreeNode *group)
{
	UITreeNode *result = group;
	if (group->kind != BeamformerPanelKind_Split)
	UIPrefWidth(ui_children_sum(1.f))
	UIPrefHeight(ui_children_sum(1.f))
	{
		assert(group->kind == BeamformerPanelKind_TabGroup);
		UITreeNode *focus = result = group->u.tab_focus;

		node->flags |= UINodeFlag_DropSite;

		UINode *tab_bar_node, *tab_clip_node;
		UIParent(node)
		UIChildLayoutAxis(Axis2_X)
		UIPrefWidth(ui_pct(1.f, 1.f))
		tab_bar_node = ui_node_from_string(UINodeFlag_Clip, str8("###tab_scroll"));

		UIParent(tab_bar_node)
		UIChildLayoutAxis(Axis2_X)
		tab_clip_node = ui_node_from_string(UINodeFlag_ViewScrollX, str8("###tab_clip"));

		f32 tab_pad = 6.f;
		UIParent(tab_clip_node)
		UIFontSize(24.f)
		{
			for (UITreeNode *tab = group->first_child; tab; tab = tab->next_sibling) {
				ui_padw(tab_pad);

				// NOTE(rnp): push tab
				UINode *tab_node;
				UIChildLayoutAxis(Axis2_X)
				// TODO(rnp): per edge border colour
				UIBorderColour((v4){.a = 0.9f})
				UIBGColour(tab == focus ? BG_COLOUR : (v4){.a = 0.6f})
				UIFlags(UINodeFlag_Clickable|
				        UINodeFlag_DrawBackground|
				        UINodeFlag_DrawBorder|
				        UINodeFlag_DrawHotEffects|
				        UINodeFlag_DrawActiveEffects)
				tab_node = ui_node_from_stringf(tab == focus ? UINodeFlag_FocusActive : 0, "###tab%p", tab);

				UISignal signal = {0};
				UIParent(tab_node)
				{
					ui_padw(UI_BORDER_THICK + tab_pad);

					ui_panel_label(tab);

					if (tab == focus)
					UIPrefWidth(ui_text_dim(2.f, 1.f))
					UIPrefHeight(ui_pct(1.f, 1.f))
					UITextAlign(Right)
					UIFlags(UINodeFlag_IconText)
					signal = ui_label_button(str8("+"));

					if ui_pressed(signal) {
						if (ui_node_key_equal(ui_context->context_menu_anchor_key, signal.node->key))
							ui_context_menu_close();
						else
							ui_context_menu_open(signal.node->key, tab);
					}

					UIPrefWidth(ui_text_dim(3.f, 1.f))
					UIPrefHeight(ui_pct(1.f, 1.f))
					UITextAlign(Center)
					UIFlags(UINodeFlag_IconText)
					signal = ui_label_button(str8("x"));
					if (ui_pressed(signal) || signal.flags & UISignalFlag_MiddlePressed)
						beamformer_command(beamformer_command_infos[BeamformerCommandKind_CloseTab].string, .tree_node = (u64)tab);
				}

				signal = ui_signal_from_node(tab_node);
				if ui_pressed(signal)
					group->u.tab_focus = tab;
				if (signal.flags & UISignalFlag_MiddlePressed)
					beamformer_command(beamformer_command_infos[BeamformerCommandKind_CloseTab].string, .tree_node = (u64)tab);
				if (ui_dragging(signal) && !point_in_rect(ui_context->last_mouse, ui_node_rect(signal.node)))
					ui_drag_begin(tab);
				if ui_released(signal) {
					// TODO(rnp): queue up panel move command
					ui_drag_end();
				}
			}

			ui_padw(tab_pad);

			// NOTE(rnp): context menu opener
			UISignal signal;
			UIPrefWidth(ui_text_dim(3.f, 1.f))
			UIPrefHeight(ui_pct(1.f, 1.f))
			UITextAlign(Center)
			UIFlags(UINodeFlag_IconText)
			signal = ui_label_button(str8("+"));
			if ui_pressed(signal) {
				if (ui_node_key_equal(ui_context->context_menu_anchor_key, signal.node->key))
					ui_context_menu_close();
				else
					ui_context_menu_open(signal.node->key, group);
			}

			ui_padw(tab_pad);
		}

		ui_signal_from_node(tab_clip_node);
	}

	// NOTE(rnp): close tabgroup button
	if (!result && group != ui_context->tree)
	UIParent(node)
	{
		UIPrefWidth(ui_pct(1.f, 0.5f))
		UIPrefHeight(ui_pct(0.5f, 0.5f))
		ui_spacer(0);

		UIChildLayoutAxis(Axis2_X)
		UIPrefWidth(ui_pct(1.f, 0.5f))
		UIPrefHeight(ui_children_sum(1.f))
		UIParent(ui_spacer(0))
		{
			UIPrefWidth(ui_pct(0.5f, 0.5f)) ui_spacer(0);

			UISignal signal;
			UIPrefWidth(ui_text_dim(1.5f, 1.f))
			UIPrefHeight(ui_text_dim(2.f, 1.f))
			UIBGColour((v4){.a = 0.3f})
			UIBorderColour((v4){.a = 0.6f})
			UITextAlign(Center)
			signal = ui_button(str8("Close Panel"));
			if ui_pressed(signal)
				beamformer_command(beamformer_command_infos[BeamformerCommandKind_CloseTab].string, .tree_node = (u64)group);

			UIPrefWidth(ui_pct(0.5f, 0.5f)) ui_spacer(0);
		}

		UIPrefWidth(ui_pct(1.f, 0.5f))
		UIPrefHeight(ui_pct(0.5f, 0.5f))
		ui_spacer(0);
	}

	ui_signal_from_node(node);

	return result;
}

function void
ui_build_regions(UINode *root_node, UITreeNode *tree_root)
{
	BeamformerUI *ui = ui_context;

	ui_push_font_size(28.0f);
	ui_push_semantic_width(ui_pct(1.f, 0.5f));
	ui_push_semantic_height(ui_pct(1.f, 0.5f));

	struct tree_frame {
		UITreeNode *tree;
		UINode     *node;
	} init[64];

	struct {
		struct tree_frame *data;
		da_count           count;
		da_count           capacity;
	} stack = {init, 0, countof(init)};

	*da_push(&ui->arena, &stack) = (struct tree_frame){
		.node = root_node,
		.tree = tree_root,
	};
	while (stack.count) {
		struct tree_frame *top = stack.data + --stack.count;

		UITreeNode *panel    = top->tree;
		UINode     *top_node = top->node;

		UIParent(top_node)
		switch (panel->kind) {

		case BeamformerPanelKind_TabGroup:{
			UINode *node;
			UIChildLayoutAxis(Axis2_Y)
			node = ui_node_from_stringf(UINodeFlag_Clip, "###%p_group", panel);
			UITreeNode *next = ui_panel_group_equip(node, panel);
			if (next) *da_push(&ui->arena, &stack) = (struct tree_frame){
				.tree = next,
				.node = node,
			};
		}break;

		case BeamformerPanelKind_Split:{
			UITreeNode *tree_node = top->tree;
			assert(tree_node->child_count == 2);

			Axis2 axis = panel->u.split.axis;
			UIAxisSize(axis2_flip(axis), ui_pct(1.f, 0.5f))
			{
				f32 split_pct = panel->u.split.fraction;

				UINode *left;
				UIAxisSize(axis, ui_pct(split_pct, 0.5f))
				UIChildLayoutAxis(Axis2_Y)
				left = ui_node_from_stringf(UINodeFlag_Clip, "###%p_left", tree_node);

				UITreeNode *next = ui_panel_group_equip(left, tree_node->first_child);
				if (next) *da_push(&ui->arena, &stack) = (struct tree_frame){
					.tree = next,
					.node = left,
				};

				UIAxisSize(axis, ui_px(UI_SPLIT_HANDLE_THICK, 1.f))
				UIBGColour((v4){.a = 0.6f})
				UIFlags(UINodeFlag_Clickable|
				        UINodeFlag_DrawBackground|
				        UINodeFlag_DrawHotEffects|
				        UINodeFlag_DrawActiveEffects)
				{
					UISignal signal = ui_signal_from_node(ui_node_from_stringf(0, "###%p_split", tree_node));
					if ui_dragging(signal) {
						UINode *node = signal.node;
						Rect nr = ui_node_rect(node->parent);
						v2   uv = rect_uv(clamp_v2_rect(ui->last_mouse, nr), nr);
						panel->u.split.fraction = Clamp(uv.E[panel->u.split.axis], 0.03f, 0.97f);
					}
				}

				UINode *right;
				UIAxisSize(axis, ui_pct(1.f - split_pct, 0.5f))
				UIChildLayoutAxis(Axis2_Y)
				right = ui_node_from_stringf(UINodeFlag_Clip, "###%p_right", tree_node);

				next = ui_panel_group_equip(right, tree_node->last_child);
				if (next) *da_push(&ui->arena, &stack) = (struct tree_frame){
					.tree = next,
					.node = right,
				};

			}
		}break;

		case BeamformerPanelKind_ComputeStats:{
			read_only local_persist BeamformerComputePlan dummy_plan = {0};
			u32 selected_plan = ui->selected_parameter_block % BeamformerMaxParameterBlocks;
			BeamformerComputePlan *cp = ui->beamformer_context->compute_context.compute_plans[selected_plan];
			if (!cp) cp = &dummy_plan;
			f32 t = ui_blinker_update(&panel->u.compute_stats.broken_shader_blinker, BLINK_SPEED);
			ui_build_compute_stats(cp, t);
		}break;

		case BeamformerPanelKind_FrameViewLive:{
			BeamformerFrameView *view = panel->u.frame_view;
			if (iv3_dimension(view->frame.points) != 0) {
				UINode *container;
				UIChildLayoutAxis(Axis2_Y)
				UIPrefWidth(ui_pct(1.f, 0.5f))
				UIPrefHeight(ui_pct(1.f, 0.5f))
				UIAxisAlign(Axis2_X, Center)
					container = ui_node_from_string(0, str8("###frame_view_container"));
				ui_build_frame_view(container, view);
			}
		}break;

		case BeamformerPanelKind_ParameterListing:{ ui_build_parameters_listing(panel); }break;

		InvalidDefaultCase;
		}

		ui_signal_from_node(top_node);
	}

	ui_pop_font_size();
	ui_pop_semantic_width();
	ui_pop_semantic_height();
}

function void
ui_build_context_menu_for_panel(UITreeNode *panel)
{
	if (!panel)
	{
		// TODO(rnp): command pallete
	}

	if (panel) {
		UIAxisSize(Axis2_X, ui_pct(1.f, 0.5f)) ui_padh(0.8f * UI_NODE_PAD);
		switch(panel->kind) {
		InvalidDefaultCase;

		case BeamformerPanelKind_TabGroup:{
			for EachElement(beamformer_panel_infos, it) {
				BeamformerPanelInfo *info = beamformer_panel_infos + it;
				if (info->list) {
					UIChildLayoutAxis(Axis2_X)
					UIPrefHeight(ui_children_sum(1.f))
					UIPrefWidth(ui_children_sum(1.f))
					UIParent(ui_spacer(0))
					{
						UISignal signal;
						ui_padw(UI_NODE_PAD);
						UIPrefHeight(ui_text_dim(1.1f, 1.f))
						UIPrefWidth(ui_text_dim(1.f, 1.f))
							signal = ui_label_button(info->display);
						ui_padw(UI_NODE_PAD);

						if ui_pressed(signal) {
							ui_context_menu_close();
							beamformer_command(beamformer_command_infos[BeamformerCommandKind_OpenTab].string,
							                   .tree_node = (u64)panel,
							                   .string    = info->string);
						}
					}
				}
			}
		}break;

		case BeamformerPanelKind_FrameViewLive:{
			BeamformerFrameView *view = panel->u.frame_view;

			UIChildLayoutAxis(Axis2_X)
			UIPrefHeight(ui_children_sum(1.f))
			UIPrefWidth(ui_children_sum(1.f))
			if (view->kind == BeamformerFrameViewKind_3DXPlane) {
			} else {
				read_only local_persist str8 dimension_strings[2][2] = {
					{str8_comp("Extent Scale Bar"),  str8_comp("Magnitude Scale Bar")},
					{str8_comp("Lateral Scale Bar"), str8_comp("Axial Scale Bar")    },
				};

				UINode *label_column, *button_column;

				UIParent(ui_spacer(0))
				UIChildLayoutAxis(Axis2_Y)
				{
					ui_padw(UI_NODE_PAD);
					UIAxisAlign(Axis2_X, Left)   label_column  = ui_node_from_string(0, str8("###labels"));
					ui_padw(UI_NODE_PAD * 2.f);
					UIAxisAlign(Axis2_X, Center)
						button_column = ui_node_from_string(0, str8("###buttons"));
					ui_padw(UI_NODE_PAD);
				}

				i32 dimension = iv3_dimension(view->frame.points);
				dimension = Min(dimension, 2);
				if (dimension > 0) {
					for EachEnumValue(Axis2, axis) {
						f32 row_height;
						UIParent(label_column)
						UIPrefHeight(ui_text_dim(1.1f, 1.f))
						UIPrefWidth(ui_text_dim(1.f, 1.f))
							row_height = ui_label(dimension_strings[dimension - 1][axis]).node->computed_size[Axis2_Y];

						UIParent(button_column)
						// TODO(rnp): ui_em(1.f, 1.f) once font size matches directly
						UIPrefHeight(ui_px(row_height, 1.f))
						UIPrefWidth(ui_px(row_height, 1.f))
						{
							UISignal signal = ui_toggle_buttonf(view->scale_bar_active[axis], "###axis_%u", axis);
							if ui_pressed(signal)
								view->scale_bar_active[axis] = !view->scale_bar_active[axis];
						}
					}
				}

				{
					f32 row_height;
					UIParent(label_column)
					UIPrefHeight(ui_text_dim(1.1f, 1.f))
					UIPrefWidth(ui_text_dim(1.f, 1.f))
						row_height = ui_label(str8("Log Scale")).node->computed_size[Axis2_Y];

					UIParent(button_column)
					// TODO(rnp): ui_em(1.f, 1.f) once font size matches directly
					UIPrefHeight(ui_px(row_height, 1.f))
					UIPrefWidth(ui_px(row_height, 1.f))
					{
						UISignal signal = ui_toggle_button(view->log_scale, str8("###log_scale"));
						if ui_pressed(signal) {
							view->log_scale = !view->log_scale;
							view->dirty     = 1;
						}
					}
				}
			}
		}break;

		}
		UIAxisSize(Axis2_X, ui_pct(1.f, 0.5f)) ui_padh(0.8f * UI_NODE_PAD);
	}
}

function void
ui_layout_constrain(UINode *root)
{
	assert(!ui_node_is_nil(root->first_child));

	// NOTE(rnp): for violations in non-layout axis all we can do is clamp
	{
		Axis2 axis  = axis2_flip(root->child_layout_axis);
		if ((root->flags & (UINodeFlag_AllowOverflowX << axis)) == 0) {
			for (UINode *child = root->first_child; !ui_node_is_nil(child); child = child->next_sibling)
				child->computed_size[axis] = Min(child->computed_size[axis], root->computed_size[axis]);
		}
	}

	Axis2 axis = root->child_layout_axis;
	if ((root->flags & (UINodeFlag_AllowOverflowX << axis)) == 0) {
		f32 allowed_size        = root->computed_size[axis];
		f32 total_size          = 0;
		f32 total_weighted_size = 0;

		for (UINode *child = root->first_child; !ui_node_is_nil(child); child = child->next_sibling) {
			total_size          += child->computed_size[axis];
			total_weighted_size += child->computed_size[axis] * (1.0f - child->semantic_size[axis].strictness);
		}

		f32 remaining_size = root->computed_size[axis];
		f32 violation = total_size - allowed_size;
		if (violation > 0 && total_weighted_size > 0) {
			f32 fixup_fraction = Clamp01(violation / total_weighted_size);
			for (UINode *child = root->first_child; !ui_node_is_nil(child); child = child->next_sibling) {
				f32 fixup = Max(0, child->computed_size[axis] * (1.0f - child->semantic_size[axis].strictness));
				child->computed_size[axis] -= fixup * fixup_fraction;

				if (child->semantic_size[axis].kind != UISizeKind_PercentOfParent)
					remaining_size -= child->computed_size[axis];
			}
		}

		// NOTE(rnp): fixup sizes dependant on parent
		for (UINode *child = root->first_child; !ui_node_is_nil(child); child = child->next_sibling)
			if (child->semantic_size[axis].kind == UISizeKind_PercentOfParent)
				child->computed_size[axis] = remaining_size * child->semantic_size[axis].value;
	}
}

function void
ui_layout_nodes(UINode *root)
{
	BeamformerUI *ui = ui_context;

	struct node_frame {
		UINode *node;
		// NOTE(rnp): for post order traversal
		b32     visited;
	} init[64] = {0};

	struct {
		struct node_frame *data;
		da_count           count;
		da_count           capacity;
	} stack = {init, 0, countof(init)};

	///////////////////////
	// NOTE(rnp): First Pass: non dependant sizes
	da_push(ui_build_arena(), &stack)->node = root;
	while (stack.count) {
		struct node_frame *top = stack.data + --stack.count;
		UINode *node = top->node;

		if (node->flags & UINodeFlag_DrawText) {
			Font font   = ui_font_for_node(node);
			str8 string = ui_draw_part_from_key_string(node->string);
			if (node->flags & UINodeFlag_IconText)
				node->text_size = measure_text_tight(font, string);
			else
				node->text_size = measure_text(font, string);
		}

		for EachElement(node->semantic_size, it) {
			switch (node->semantic_size[it].kind) {
			case UISizeKind_Pixels:{node->computed_size[it] = node->semantic_size[it].value;}break;

			case UISizeKind_TextContent:{
				node->computed_size[it] = node->semantic_size[it].value * node->text_size.E[it];
			}break;

			default:{}break;
			}
		}

		// NOTE(rnp): push children
		for (UINode *child = node->first_child; !ui_node_is_nil(child); child = child->next_sibling)
			da_push(ui_build_arena(), &stack)->node = child;
	}

	///////////////////////
	// NOTE(rnp): Second Pass (Pre Order): parent dependant sizes
	da_push(ui_build_arena(), &stack)->node = root;
	while (stack.count) {
		struct node_frame *top = stack.data + --stack.count;
		UINode *node = top->node;

		for EachElement(node->semantic_size, it) {
			if (node->semantic_size[it].kind == UISizeKind_PercentOfParent) {
				f32 parent_size = node->parent->computed_size[it];
				node->computed_size[it] = node->semantic_size[it].value * parent_size;
			}
		}

		// NOTE(rnp): push children
		for (UINode *child = node->first_child; !ui_node_is_nil(child); child = child->next_sibling)
			da_push(ui_build_arena(), &stack)->node = child;
	}

	///////////////////////
	// NOTE(rnp): Third Pass (Post Order): child dependant sizes
	da_push(ui_build_arena(), &stack)->node = root;
	while (stack.count) {
		struct node_frame *top = stack.data + stack.count - 1;

		UINode *node = top->node;
		if (!top->visited && node->child_count) {
			top->visited = 1;

			// NOTE(rnp): push children
			for (UINode *child = node->first_child; !ui_node_is_nil(child); child = child->next_sibling)
				da_push(ui_build_arena(), &stack)->node = child;
		} else {
			// NOTE(rnp): pop
			stack.count--;

			for EachElement(node->semantic_size, it) {
				if (node->semantic_size[it].kind == UISizeKind_ChildrenSum) {
					f32 size_sum = 0;
					for (UINode *child = node->first_child;
					     !ui_node_is_nil(child);
					     child = child->next_sibling)
					{
						if (it == node->child_layout_axis) {
							size_sum += child->computed_size[it];
						} else {
							size_sum = Max(size_sum, child->computed_size[it]);
						}
					}
					node->computed_size[it] = size_sum;
				}
			}
		}
	}

	///////////////////////
	// NOTE(rnp): Fourth Pass (Pre Order): solve violations
	da_push(ui_build_arena(), &stack)->node = root;
	while (stack.count) {
		struct node_frame *top = stack.data + --stack.count;

		UINode *node = top->node;
		if (node->child_count)
			ui_layout_constrain(node);

		// NOTE(rnp): push children
		for (UINode *child = node->first_child; !ui_node_is_nil(child); child = child->next_sibling)
			da_push(ui_build_arena(), &stack)->node = child;
	}

	///////////////////////
	// NOTE(rnp): Final Pass (Pre Order): fill positions
	da_push(&ui->arena, &stack)->node = root;
	while (stack.count) {
		struct node_frame *top = stack.data + --stack.count;

		UINode *node = top->node;
		Axis2 layout_axis  = node->child_layout_axis;
		Axis2 flipped_axis = axis2_flip(layout_axis);
		f32   offset       = 0;
		for (UINode *child = node->first_child; !ui_node_is_nil(child); child = child->next_sibling) {
			child->computed_position[flipped_axis] = node->computed_position[flipped_axis];
			child->computed_position[layout_axis]  = offset + node->computed_position[layout_axis];
			offset += child->computed_size[layout_axis];
		}

		for (UINode *child = node->first_child; !ui_node_is_nil(child); child = child->next_sibling) {
			for EachElement(node->alignment, axis) {
				f32 size_delta = node->computed_size[axis] - child->computed_size[axis];
				child->computed_position[axis] += ui_alignment_correction(node->alignment[axis], size_delta);
			}
		}

		// NOTE(rnp): push children
		for (UINode *child = node->first_child; !ui_node_is_nil(child); child = child->next_sibling)
			if (child->child_count > 0)
				da_push(ui_build_arena(), &stack)->node = child;
	}
}

function void
ui_draw_nodes(UINode *root, Rect window_rect)
{
	BeamformerUI *ui = ui_context;

	struct node_frame {
		Rect    clip;
		v2      view_offset;
		b32     visited;
		UINode *node;
	} init[64];

	struct {
		struct node_frame *data;
		da_count           count;
		da_count           capacity;
	} stack = {init, 0, countof(init)};

	u32 colour_index = 0;

	Rect clip     = window_rect;
	v2   view_off = {0};

	da_push(ui_build_arena(), &stack)->node = root;
	while (stack.count) {
		struct node_frame *top = stack.data + stack.count - 1;

		UINode *node = top->node;
		if (!top->visited) {
			top->visited = 1;

			Rect r = ui_node_rect(node);
			if (node->flags & UINodeFlag_Clip) {
				top->clip = clip;
				clip      = r;
				BeginScissorMode(r.pos.x, r.pos.y, r.size.w, r.size.h);
			}

			if (node->flags & UINodeFlag_ViewScroll) {
				top->view_offset = view_off;
				view_off = node->view_scroll_offset;
				rlPushMatrix();
				rlTranslatef(-view_off.x, -view_off.y, 0);
			}

			// TODO(rnp): push colour and hover colour into node

			v4 colour = g_colour_palette[(colour_index++) % countof(g_colour_palette)];
			(void)colour;
			//DrawRectangleLinesEx(rl_rect(r), 4.0f, colour_from_normalized(colour));

			v4 bg_colour = node->bg_colour;
			if (node->flags & UINodeFlag_DrawHotEffects)
				bg_colour = v4_lerp(bg_colour, HOVERED_COLOUR, node->hot_t);

			if (node->flags & UINodeFlag_DrawBackground)
				DrawRectangleRec(rl_rect(r), colour_from_normalized(bg_colour));

			if (node->flags & UINodeFlag_DrawBorder) {
				v4  colour = node->border_colour;
				u64 masked = node->flags & (UINodeFlag_DrawBackground|UINodeFlag_DrawHotEffects);
				if (masked == UINodeFlag_DrawHotEffects)
					colour = v4_lerp(colour, HOVERED_COLOUR, node->hot_t);

				DrawRectangleLinesEx(rl_rect(r), node->border_thickness, colour_from_normalized(colour));
			}

			if (node->flags & UINodeFlag_CustomDraw) {
				node->custom_draw_function(node, r);
			} else {
				if (node->flags & UINodeFlag_DrawText) {
					Font font = ui_font_for_node(node);

					TextSpec text_spec = {
						.font           = &font,
						.flags          = TF_LIMITED,
						.colour         = node->text_colour,
						.outline_colour = node->text_outline_colour,
						.outline_thick  = node->text_outline_thickness,
						.limits.size    = r.size,
					};
					if (node->text_outline_thickness > 0)
						text_spec.flags |= TF_OUTLINED;

					v2 pos = ui_node_text_position(node);

					UITextInputState *tis = &ui_context->text_input_state;
					b32  input  = ui_node_key_equal(node->key, tis->node_key);
					// TODO(rnp): cleanup: visible part
					str8 string = ui_draw_part_from_key_string(node->string);
					if (!input && node->flags & UINodeFlag_DrawHotEffects && (node->flags & UINodeFlag_DrawBackground) == 0)
						text_spec.colour = v4_lerp(text_spec.colour, HOVERED_COLOUR, node->hot_t);

					if (node->flags & UINodeFlag_IconText)
						draw_text_tight(*text_spec.font, string, pos, colour_from_normalized(text_spec.colour));
					else
						draw_text(string, pos, &text_spec);

					if (input) {
						iv2 range = ui_text_input_cursor_range();
						str8 parts[2];
						parts[0] = (str8){.data = string.data,           .length = range.x};
						parts[1] = (str8){.data = string.data + range.x, .length = range.y - range.x};

						Rect cursor = {.pos = pos};
						cursor.pos.x += measure_text(font, parts[0]).x;

						v4 cursor_colour = FOCUSED_COLOUR;
						if (parts[1].length > 0) {
							cursor_colour = SELECTION_COLOUR;
							cursor.size   = measure_text(font, parts[1]);

							if (range.x == 0) {
								cursor.pos.x  -= 2.f;
								cursor.size.x += 2.f;
							}

							if (range.y == tis->count)
								cursor.size.x += 2.f;
						} else {
							cursor_colour.a = ease_in_out_cubic(ui->text_input_state.blinker.t);
							cursor.size.x   = string.length - range.y > 0 ? 4.0f : 0.55f * (f32)font.baseSize;
							cursor.size.y   = font.baseSize;
						}

						if (cursor.size.x > 0)
							DrawRectanglePro(rl_rect(cursor), (Vector2){0}, 0, colour_from_normalized(cursor_colour));
					}
				}
			}

			// NOTE(rnp): push children
			for (UINode *child = node->first_child; !ui_node_is_nil(child); child = child->next_sibling) {
				Rect cr         = ui_node_rect(child);
				if ((cr.size.x > 0 && cr.size.y > 0) || ui_node_key_equal(child->key, ui->text_input_state.node_key))
					da_push(ui_build_arena(), &stack)->node = child;
			}
		} else {
			// NOTE(rnp): pop
			stack.count--;

			if (node->flags & UINodeFlag_ViewScroll) {
				rlPopMatrix();
			}

			if (node->flags & UINodeFlag_Clip) {
				EndScissorMode();
			}
		}
	}

	// TODO(rnp): can we make the mouse latency not shit?
	//if (ui->current_mouse.x > 0) DrawCircle(ui->current_mouse.x, ui->current_mouse.y, 6, GREEN);
}

function void
ui_kill_tree_node(UITreeNode *node)
{
	BeamformerUI *ui     = ui_context;
	UITreeNode   *parent = node->parent;

	if (node->kind == BeamformerPanelKind_FrameViewLive ||
	    node->kind == BeamformerPanelKind_FrameViewCopy ||
	    node->kind == BeamformerPanelKind_FrameViewXPlane)
	{
		BeamformerFrameView *bv = node->u.frame_view;
		ui_beamformer_frame_view_release_subresources(bv, bv->kind);
		DLLRemove(0, ui->view_first, ui->view_last, bv, next, prev);
		SLLStackPush(ui->view_freelist, bv, next);
	}

	if (parent->u.tab_focus == node)
		parent->u.tab_focus = node->previous_sibling ? node->previous_sibling : node->next_sibling;

	DLLRemove(0, parent->first_child, parent->last_child, node, next_sibling, previous_sibling);
	SLLStackPush(ui->tree_node_freelist, node, next_sibling);
	parent->child_count--;

	if (node->kind == BeamformerPanelKind_TabGroup) {
		assert(parent->kind == BeamformerPanelKind_Split);

		UITreeNode *child = parent->first_child;
		parent->first_child = child->first_child;
		parent->last_child  = child->last_child;
		parent->child_count = child->child_count;
		parent->kind        = child->kind;
		memory_copy(&parent->u, &child->u, sizeof(child->u));
		SLLStackPush(ui->tree_node_freelist, child, next_sibling);
		for (child = parent->first_child; child; child = child->next_sibling)
			child->parent = parent;
	}
}

function UITreeNode *
ui_push_tree_node(UITreeNode *parent)
{
	BeamformerUI *ui = ui_context;
	UITreeNode *result = ui->tree_node_freelist;
	if (result) SLLStackPop(ui->tree_node_freelist, next_sibling);
	else result = push_struct_no_zero(&ui->arena, UITreeNode);
	zero_struct(result);

	result->parent = parent;
	if (parent) {
		DLLInsertLast(0, parent->first_child, parent->last_child, result, next_sibling, previous_sibling);
		parent->child_count++;
	}

	return result;
}

function UITreeNode *
ui_push_panel(UITreeNode *parent, BeamformerPanelKind kind)
{
	UITreeNode *result = ui_push_tree_node(parent);
	result->kind = kind;
	if (parent && parent->kind == BeamformerPanelKind_TabGroup)
		parent->u.tab_focus = result;

	if (kind == BeamformerPanelKind_FrameViewLive ||
	    kind == BeamformerPanelKind_FrameViewCopy ||
	    kind == BeamformerPanelKind_FrameViewXPlane)
	{
		BeamformerFrameViewKind view_kind = BeamformerFrameViewKind_Latest;
		if (kind == BeamformerPanelKind_FrameViewCopy)
			view_kind = BeamformerFrameViewKind_Copy;
		if (kind == BeamformerPanelKind_FrameViewXPlane)
			view_kind = BeamformerFrameViewKind_3DXPlane;
		result->u.frame_view = ui_beamformer_frame_view_new(ui_context, &ui_context->arena, view_kind);
	}

	return result;
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
	BeamformerUI *ui = ui_context = ctx->ui;
	if (!ui) {
		ui = ui_context = ctx->ui = push_struct(&store, typeof(*ui));
		ui->arena = store;
		ui->shared_memory      = ctx->shared_memory;
		ui->beamformer_context = ctx;

		for EachElement(ui->build_arenas, it) {
			ui->build_arenas[it] = sub_arena(&ui->arena, KB(128), KB(4));
			ui->build_arena_savepoints[it] = begin_temp_arena(ui->build_arenas + it);
		}
		ui->node_freelist = &ui_node_nil;

		/* TODO(rnp): better font, this one is jank at small sizes */
		ui->font       = LoadFontFromMemory(".ttf", beamformer_base_font, sizeof(beamformer_base_font), 28, 0, 0);
		ui->small_font = LoadFontFromMemory(".ttf", beamformer_base_font, sizeof(beamformer_base_font), 20, 0, 0);

		ui->floating_widget_sentinal.parent = &ui->floating_widget_sentinal;
		ui->floating_widget_sentinal.next   = &ui->floating_widget_sentinal;

		// NOTE(rnp): push default UI layout
		// TODO(rnp): load last layout from file and only load default if not present
		{
			UITreeNode *node = ui->tree = ui_push_panel(0, BeamformerPanelKind_Split);
			node->u.split.fraction = 0.35f;
			node->u.split.axis     = Axis2_X;

			DeferLoop(node = ui_push_panel(node, BeamformerPanelKind_Split), node = node->parent)
			{
				node->u.split.fraction = 0.65f;
				node->u.split.axis     = Axis2_Y;

				UITreeNode *left   = ui_push_panel(node,  BeamformerPanelKind_TabGroup);
				UITreeNode *right  = ui_push_panel(node,  BeamformerPanelKind_TabGroup);
				left->u.tab_focus  = ui_push_panel(left,  BeamformerPanelKind_ParameterListing);
				right->u.tab_focus = ui_push_panel(right, BeamformerPanelKind_ComputeStats);
				ui_push_panel(left, BeamformerPanelKind_ComputeStats);
			}

			DeferLoop(node = ui_push_panel(node, BeamformerPanelKind_TabGroup), node = node->parent)
			{
				node->u.tab_focus = ui_push_panel(node, BeamformerPanelKind_FrameViewLive);
			}
		}

		u32 samples = vk_gpu_info()->max_msaa_samples;
		vk_image_allocate(&ui->render_3d_image,       FRAME_VIEW_RENDER_TARGET_SIZE, 1, samples, VulkanImageUsage_Colour,       0, 0, s8("Render Target Colour"));
		vk_image_allocate(&ui->render_3d_depth_image, FRAME_VIEW_RENDER_TARGET_SIZE, 1, samples, VulkanImageUsage_DepthStencil, 0, 0, s8("Render Target Depth"));

		glGenSemaphoresEXT(countof(ui->render_semaphores_gl), ui->render_semaphores_gl);
		for EachElement(ui->render_semaphores, it)
			ui->render_semaphores[it] = vk_create_semaphore(ui->render_semaphores_export + it);

		if (OS_WINDOWS) {
			glImportSemaphoreWin32HandleEXT(ui->render_semaphores_gl[0], GL_HANDLE_TYPE_OPAQUE_WIN32_EXT, (void *)ui->render_semaphores_export[0].value[0]);
			glImportSemaphoreWin32HandleEXT(ui->render_semaphores_gl[1], GL_HANDLE_TYPE_OPAQUE_WIN32_EXT, (void *)ui->render_semaphores_export[1].value[0]);
		} else {
			glImportSemaphoreFdEXT(ui->render_semaphores_gl[0], GL_HANDLE_TYPE_OPAQUE_FD_EXT, ui->render_semaphores_export[0].value[0]);
			glImportSemaphoreFdEXT(ui->render_semaphores_gl[1], GL_HANDLE_TYPE_OPAQUE_FD_EXT, ui->render_semaphores_export[1].value[0]);
			ui->render_semaphores_export[0].value[0] = OSInvalidHandleValue;
			ui->render_semaphores_export[1].value[0] = OSInvalidHandleValue;
		}

		if (!BakeShaders)
		{
			for EachElement(beamformer_reloadable_render_shader_info_indices, it) {
				i32 index = beamformer_reloadable_render_shader_info_indices[it];
				for (u32 i = 0; i < 2; i++) {
					BeamformerFileReloadContext *frc = push_struct(&ui->arena, typeof(*frc));
					frc->kind                   = BeamformerFileReloadKind_RenderShader;
					frc->shader_reload.shader   = beamformer_reloadable_shader_kinds[index];
					frc->shader_reload.pipeline = ui->pipelines + it;

					Arena scratch = ui->arena;
					s8 file = push_s8_from_parts(&scratch, os_path_separator(), s8("shaders"),
					                             beamformer_reloadable_shader_files[index][i]);

					os_add_file_watch((char *)file.data, file.len, frc);
				}
			}
		}

		f32 unit_cube_vertices[] = {
			 0.5f,  0.5f, -0.5f, 0.0f,
			 0.5f,  0.5f, -0.5f, 0.0f,
			 0.5f,  0.5f, -0.5f, 0.0f,
			 0.5f, -0.5f, -0.5f, 0.0f,
			 0.5f, -0.5f, -0.5f, 0.0f,
			 0.5f, -0.5f, -0.5f, 0.0f,
			 0.5f,  0.5f,  0.5f, 0.0f,
			 0.5f,  0.5f,  0.5f, 0.0f,
			 0.5f,  0.5f,  0.5f, 0.0f,
			 0.5f, -0.5f,  0.5f, 0.0f,
			 0.5f, -0.5f,  0.5f, 0.0f,
			 0.5f, -0.5f,  0.5f, 0.0f,
			-0.5f,  0.5f, -0.5f, 0.0f,
			-0.5f,  0.5f, -0.5f, 0.0f,
			-0.5f,  0.5f, -0.5f, 0.0f,
			-0.5f, -0.5f, -0.5f, 0.0f,
			-0.5f, -0.5f, -0.5f, 0.0f,
			-0.5f, -0.5f, -0.5f, 0.0f,
			-0.5f,  0.5f,  0.5f, 0.0f,
			-0.5f,  0.5f,  0.5f, 0.0f,
			-0.5f,  0.5f,  0.5f, 0.0f,
			-0.5f, -0.5f,  0.5f, 0.0f,
			-0.5f, -0.5f,  0.5f, 0.0f,
			-0.5f, -0.5f,  0.5f, 0.0f,
		};
		f32 unit_cube_normals[] = {
			 0.0f,  0.0f, -1.0f, 0.0f,
			 0.0f,  1.0f,  0.0f, 0.0f,
			 1.0f,  0.0f,  0.0f, 0.0f,
			 0.0f,  0.0f, -1.0f, 0.0f,
			 0.0f, -1.0f,  0.0f, 0.0f,
			 1.0f,  0.0f,  0.0f, 0.0f,
			 0.0f,  0.0f,  1.0f, 0.0f,
			 0.0f,  1.0f,  0.0f, 0.0f,
			 1.0f,  0.0f,  0.0f, 0.0f,
			 0.0f,  0.0f,  1.0f, 0.0f,
			 0.0f, -1.0f,  0.0f, 0.0f,
			 1.0f,  0.0f,  0.0f, 0.0f,
			 0.0f,  0.0f, -1.0f, 0.0f,
			 0.0f,  1.0f,  0.0f, 0.0f,
			-1.0f,  0.0f,  0.0f, 0.0f,
			 0.0f,  0.0f, -1.0f, 0.0f,
			 0.0f, -1.0f,  0.0f, 0.0f,
			-1.0f,  0.0f,  0.0f, 0.0f,
			 0.0f,  0.0f,  1.0f, 0.0f,
			 0.0f,  1.0f,  0.0f, 0.0f,
			-1.0f,  0.0f,  0.0f, 0.0f,
			 0.0f,  0.0f,  1.0f, 0.0f,
			 0.0f, -1.0f,  0.0f, 0.0f,
			-1.0f,  0.0f,  0.0f, 0.0f,
		};
		u16 unit_cube_indices[] = {
			1,  13, 19,
			1,  19, 7,
			9,  6,  18,
			9,  18, 21,
			23, 20, 14,
			23, 14, 17,
			16, 4,  10,
			16, 10, 22,
			5,  2,  8,
			5,  8,  11,
			15, 12, 0,
			15, 0,  3
		};

		static_assert(countof(unit_cube_normals) == countof(unit_cube_vertices), "");

		RenderModel *rm = &ui->unit_cube_model;
		rm->vertex_count   = countof(unit_cube_vertices) / 4;
		rm->normals_offset = round_up_to(sizeof(unit_cube_vertices), 16);

		u64 model_size = 2 * round_up_to(sizeof(unit_cube_vertices), 16);
		vk_render_model_allocate(&rm->model, unit_cube_indices, countof(unit_cube_indices), model_size, s8("unit_cube_model"));
		vk_render_model_range_upload(&rm->model, unit_cube_vertices, 0,                  sizeof(unit_cube_vertices), 0);
		vk_render_model_range_upload(&rm->model, unit_cube_normals,  rm->normals_offset, sizeof(unit_cube_normals),  0);
	}

	for EachElement(beamformer_reloadable_render_shader_info_indices, it) {
		i32 index = beamformer_reloadable_render_shader_info_indices[it];
		BeamformerShaderKind shader = beamformer_reloadable_shader_kinds[index];
		beamformer_reload_render_pipeline(ui->pipelines + it, shader, ui->arena);
	}
}

function void
draw_ui(BeamformerCtx *ctx, BeamformerInput *input, BeamformerFrame *frame_to_draw, BeamformerViewPlaneTag frame_plane)
{
	BeamformerUI *ui = ui_context = ctx->ui;
	beamformer_input = input;

	if (frame_to_draw) {
		mem_copy(ui->latest_plane + BeamformerViewPlaneTag_Count, frame_to_draw, sizeof(*frame_to_draw));
		mem_copy(ui->latest_plane + frame_plane,                  frame_to_draw, sizeof(*frame_to_draw));
		ui->latest_plane_valid[BeamformerViewPlaneTag_Count] = 1;
		ui->latest_plane_valid[frame_plane]                  = 1;
	} else {
		ui->latest_plane_valid[BeamformerViewPlaneTag_Count] = 0;
		ui->latest_plane_valid[frame_plane]                  = 0;
	}

	for EachIndex(input->event_count, it) {
		if (input->event_queue[it].kind == BeamformerInputEventKind_WindowResize) {
			// TODO(rnp): match window against window list
			ctx->window_size.w = input->event_queue[it].window_resize.width;
			ctx->window_size.h = input->event_queue[it].window_resize.height;
		}
	}

	asan_poison_region(ui->arena.beg, ui->arena.end - ui->arena.beg);

	u32 selected_block = ui->selected_parameter_block % BeamformerMaxParameterBlocks;
	u32 selected_mask  = 1 << selected_block;
	if (ctx->ui_dirty_parameter_blocks & selected_mask) {
		BeamformerParameterBlock *pb = beamformer_parameter_block_lock(ui->shared_memory, selected_block, 0);
		if (pb) {
			ui->flush_parameters = 0;

			m4 das_transform;
			memory_copy(&ui->parameters, &pb->parameters_ui, sizeof(ui->parameters));
			memory_copy(das_transform.E, pb->parameters.das_voxel_transform.E, sizeof(das_transform));

			atomic_and_u32(&ctx->ui_dirty_parameter_blocks, ~selected_mask);
			beamformer_parameter_block_unlock(ui->shared_memory, selected_block);

			BeamformerComputePlan *cp = ui->beamformer_context->compute_context.compute_plans[selected_block];
			m4 identity = m4_identity();
			b32 recompute = !m4_equal(identity, cp->ui_voxel_transform);
			memory_copy(cp->ui_voxel_transform.E, identity.E, sizeof(identity));

			if (recompute) {
				mark_parameter_block_region_dirty(ui->shared_memory, selected_block,
				                                  BeamformerParameterBlockRegion_Parameters);
				beamformer_queue_compute(ctx, frame_to_draw, selected_block);
			}

			v3 U = v3_normalize(das_transform.c[0].xyz);
			v3 V = v3_normalize(das_transform.c[1].xyz);
			v3 N = cross(V, U);

			ui->off_axis_position = v3_dot(N, das_transform.c[3].xyz);
			ui->beamform_plane    = 0;
		}
	}

	/* NOTE: process interactions first because the user interacted with
	 * the ui that was presented last frame */
	Rect window_rect = {.size = {{(f32)ctx->window_size.w, (f32)ctx->window_size.h}}};
	//ui_interact(ui, input, window_rect);

	ui->last_mouse      = ui->current_mouse;
	ui->current_mouse.x = input->mouse_x;
	ui->current_mouse.y = input->mouse_y;
	for EachElement(ui->input_consumed, it)
		ui->input_consumed[it] = 0;

	if (ui->flush_parameters) {
		if (ctx->latest_frame) {
			BeamformerParameterBlock *pb = beamformer_parameter_block_lock(ui->shared_memory, selected_block, 0);
			if (pb) {
				ui->flush_parameters = 0;
				memory_copy(&pb->parameters_ui, &ui->parameters, sizeof(ui->parameters));
				mark_parameter_block_region_dirty(ui->shared_memory, selected_block,
				                                  BeamformerParameterBlockRegion_Parameters);
				beamformer_parameter_block_unlock(ui->shared_memory, selected_block);
				beamformer_queue_compute(ctx, frame_to_draw, selected_block);
			}
		}
	}

	/* NOTE(rnp): can't render to a different framebuffer in the middle of BeginDrawing()... */
	update_frame_views(ui, window_rect);

	////////////////////////////
	// NOTE(rnp): Text Input
	{
		UITextInputState *tis = &ui->text_input_state;
		// NOTE(rnp): transition to new node
		tis->last_node_key = ui_node_key_zero();
		tis->last_count    = 0;
		if (tis->changed) {
			tis->changed = 0;
			ui_text_input_end();
			if (!ui_node_key_nil(tis->next_node_key)) {
				tis->node_key      = tis->next_node_key;
				tis->next_node_key = ui_node_key_zero();
				if (point_in_rect(ui->current_mouse, ui_text_input_rect()))
					tis->cursor = tis->mark = ui_text_input_index_from_point(ui->last_mouse.x);
				tis->blinker.t = 1.0f;
			}
		}

		if (!ui_node_key_nil(tis->node_key)) {
			ui_blinker_update(&tis->blinker, BLINK_SPEED);

			UISignal signal = ui_signal_from_node(ui_node_from_key(tis->node_key));

			if (signal.flags & UISignalFlag_LeftPressed) {
				if (point_in_rect(ui->current_mouse, ui_text_input_rect()))
					tis->cursor = tis->mark = ui_text_input_index_from_point(ui->last_mouse.x);
				tis->blinker.t = 1.0f;
			}

			if (signal.flags & UISignalFlag_LeftDragging)
				tis->mark = ui_text_input_index_from_point(ui->last_mouse.x);

			if (signal.flags & UISignalFlag_DoubleClicked) {
				// TODO(rnp): select word
			}

			if (signal.flags & UISignalFlag_TripleClicked) {
				tis->cursor = 0;
				tis->mark   = tis->count;
			}

			if (ui_text_input_update(input))
				ui_text_input_end();
		}
	}

	if (ui->context_menu_state_changed) {
		ui->context_menu_state_changed = 0;
		ui->context_menu_anchor_key    = ui->context_menu_next_anchor_key;
		ui->context_menu_panel         = ui->context_menu_next_panel;
	}

	{
		////////////////////////////
		// NOTE(rnp): Build Pass
		end_temp_arena(ui->build_arena_savepoints[ui->current_frame_index % countof(ui->build_arenas)]);
		// NOTE(rnp): reset last frame's build stacks
		{
			#define X(type, name, ...) \
				ui_context->name##_node_stack.top   = &ui_##name##_node_nil; \
				ui_context->name##_node_stack.free  = 0; \
				ui_context->name##_node_stack.count = 0;
			UI_STACK_LIST
			#undef X

			ui_push_semantic_width(ui_px(window_rect.size.x, 1.0f));
			ui_push_semantic_height(ui_px(window_rect.size.y, 1.0f));
			ui->root_node = ui_node_from_string(0, str8("UI Root Node"));
		}

		// NOTE(rnp): check for active nodes
		{
			b32 active = 0;
			for EachEnumValue(UIMouseButtonKind, k)
				active |= !ui_node_key_equal(ui->active_node_key[k], ui_node_key_zero());
			// NOTE(rnp): clear hot node if there are no active nodes
			if (!active) ui->hot_node_key = ui_node_key_zero();
		}

		// NOTE(rnp): context menu
		if (!ui_node_key_nil(ui->context_menu_anchor_key)) {
			// TODO(rnp): context_menu_open_t
			UIPrefWidth(ui_children_sum(1.f))
			UIPrefHeight(ui_children_sum(1.f))
			UIChildLayoutAxis(Axis2_Y)
			UIBGColour((v4){.a = 0.8f})
			{
				// TODO(rnp): this should be tied to the window state
				ui->context_menu_root = ui_node_from_string(UINodeFlag_DrawBackground, str8("context_menu_root"));
			}

			UIParent(ui->context_menu_root)
			{
				ui_build_context_menu_for_panel(ui->context_menu_panel);
			}
		}

		// NOTE(rnp): drag panel
		if (ui->drag_panel) {
			UIPrefWidth(ui_px(640.f, 1.f))
			UIPrefHeight(ui_px(480.f, 1.f))
			UIChildLayoutAxis(Axis2_Y)
			UIBGColour((v4){.a = 0.8f})
			{
				ui->drag_root = ui_node_from_string(UINodeFlag_DrawBackground, str8("drag_panel_root"));
			}

			UIParent(ui->drag_root)
			{
				UIChildLayoutAxis(Axis2_X)
				UIPrefHeight(ui_children_sum(1.f))
				UIPrefWidth(ui_children_sum(1.f))
				UIParent(ui_spacer(0))
				{
					ui_padw(UI_NODE_PAD);
					ui_panel_label(ui->drag_panel);
				}

				ui_build_regions(ui->drag_root, ui->drag_panel);
			}
		}

		ui_build_regions(ui->root_node, ui->tree);

		////////////////////////////
		// NOTE(rnp): Prune Dead UI Nodes
		for EachElement(ui->node_hash_table, it) {
			UINodeHashBucket *hb = ui->node_hash_table + it;
			UINode *next = hb->first;
			for (UINode *b = next; !ui_node_is_nil(b); b = next) {
				next = b == b->hash_next ? 0 : b->hash_next;
				if (b->last_frame_active_index != ui->current_frame_index) {
					for EachEnumValue(UIMouseButtonKind, k)
						if (ui_node_key_equal(ui->active_node_key[k], b->key))
							ui->active_node_key[k] = ui_node_key_zero();

					DLLRemove(&ui_node_nil, hb->first, hb->last, b, hash_next, hash_prev);
					SLLStackPush(ui->node_freelist, b, next_sibling);
				}
			}
		}

		for (BeamformerInputEvent *event = ui_event_next(input, 0);
		     event;
		     event = ui_event_next(input, event))
		{
			if (event->kind == BeamformerInputEventKind_ButtonPress) {
				if (event->button_id == BeamformerButtonID_Escape)
					ctx->state = BeamformerState_ShouldClose;

				if (!Between(event->button_id, BeamformerButtonID_ModifierFirst, BeamformerButtonID_ModifierLast)) {
					ui_context_menu_close();
					ui->text_input_state.changed       = 1;
					ui->text_input_state.next_node_key = ui_node_key_zero();
				}
			}
		}

		////////////////////////////
		// NOTE(rnp): Layout Pass
		if (ui->drag_root) {
			ui->drag_root->computed_position[Axis2_X] = ui->last_mouse.x;
			ui->drag_root->computed_position[Axis2_Y] = ui->last_mouse.y;
			ui_layout_nodes(ui->drag_root);
		}

		ui_layout_nodes(ui->root_node);

		if (!ui_node_key_nil(ui->context_menu_anchor_key)) {
			UINode *anchor   = ui_node_from_key(ui->context_menu_anchor_key);
			v2      anchor_p = ui_node_final_position(anchor);
			ui->context_menu_root->computed_position[Axis2_X] = anchor_p.x;
			ui->context_menu_root->computed_position[Axis2_Y] = anchor_p.y + anchor->computed_size[Axis2_Y];

			ui_layout_nodes(ui->context_menu_root);
		}

		BeginDrawing();
			glClearNamedFramebufferfv(0, GL_COLOR, 0, BG_COLOUR.E);
			glClearNamedFramebufferfv(0, GL_DEPTH, 0, (f32 []){1});
			ui_draw_nodes(ui->root_node, window_rect);

			if (!ui_node_key_nil(ui->context_menu_anchor_key))
				ui_draw_nodes(ui->context_menu_root, window_rect);

			if (ui->drag_root)
				ui_draw_nodes(ui->drag_root, window_rect);

			// TODO(rnp): hack: until raylib is removed this happens in ui since raylib will cause
			// glfw to call the input callbacks during EndDrawing()
			input->event_count = 0;
		EndDrawing();

		ui->current_frame_index++;
	}
}
