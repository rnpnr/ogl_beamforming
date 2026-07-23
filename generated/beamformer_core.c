/* See LICENSE for license details. */

// GENERATED CODE

typedef enum {
	BeamformerPanelKind_Nil                 = 0,
	BeamformerPanelKind_Split               = 1,
	BeamformerPanelKind_TabGroup            = 2,
	BeamformerPanelKind_ComputeBarGraph     = 3,
	BeamformerPanelKind_ComputeStats        = 4,
	BeamformerPanelKind_FrameViewLive       = 5,
	BeamformerPanelKind_FrameViewCopy       = 6,
	BeamformerPanelKind_FrameViewXPlane     = 7,
	BeamformerPanelKind_LiveImagingControls = 8,
	BeamformerPanelKind_ParameterListing    = 9,
	BeamformerPanelKind_Count,
} BeamformerPanelKind;

typedef enum {
	BeamformerRegisterSlot_String         = 0,
	BeamformerRegisterSlot_TreeNode       = 1,
	BeamformerRegisterSlot_Window         = 2,
	BeamformerRegisterSlot_Frame          = 3,
	BeamformerRegisterSlot_FrameView      = 4,
	BeamformerRegisterSlot_SplitAxis      = 5,
	BeamformerRegisterSlot_SplitLeftTree  = 6,
	BeamformerRegisterSlot_SplitRightTree = 7,
	BeamformerRegisterSlot_DropTargetTree = 8,
	BeamformerRegisterSlot_DropChildIndex = 9,
	BeamformerRegisterSlot_Count,
} BeamformerRegisterSlot;

typedef enum {
	BeamformerCommandKind_Nil       = 0,
	BeamformerCommandKind_CloseTab  = 1,
	BeamformerCommandKind_FocusTab  = 2,
	BeamformerCommandKind_MoveTab   = 3,
	BeamformerCommandKind_OpenTab   = 4,
	BeamformerCommandKind_SplitTree = 5,
	BeamformerCommandKind_Count,
} BeamformerCommandKind;

typedef enum {
	BeamformerPanelFlags_List        = 1 << 0,
	BeamformerPanelFlags_NeedsFrame  = 1 << 1,
	BeamformerPanelFlags_HasSettings = 1 << 2,
} BeamformerPanelFlags;

typedef struct {
	str8                 display;
	str8                 string;
	str8                 description;
	BeamformerPanelFlags flags;
} BeamformerPanelInfo;

typedef struct {
	str8 string;
	u64  tree_node;
	u64  window;
	u64  frame;
	u64  frame_view;
	u64  split_axis;
	u64  split_left_tree;
	u64  split_right_tree;
	u64  drop_target_tree;
	u64  drop_child_index;
} BeamformerRegisters;

typedef struct {
	str8 string;
	str8 display;
	str8 description;
} BeamformerCommandInfo;

read_only global BeamformerPanelInfo beamformer_panel_infos[] = {
	{str8_comp(""), str8_comp("nil"), str8_comp(""), (0*BeamformerPanelFlags_List)|(0*BeamformerPanelFlags_NeedsFrame)|(0*BeamformerPanelFlags_HasSettings)},
	{str8_comp(""), str8_comp("split"), str8_comp(""), (0*BeamformerPanelFlags_List)|(0*BeamformerPanelFlags_NeedsFrame)|(0*BeamformerPanelFlags_HasSettings)},
	{str8_comp(""), str8_comp("group"), str8_comp(""), (0*BeamformerPanelFlags_List)|(0*BeamformerPanelFlags_NeedsFrame)|(0*BeamformerPanelFlags_HasSettings)},
	{str8_comp("Compute Bar Graph"), str8_comp("compute_bar_graph"), str8_comp("Bar graph showing portions of compute occupied by each stage."), (1*BeamformerPanelFlags_List)|(0*BeamformerPanelFlags_NeedsFrame)|(0*BeamformerPanelFlags_HasSettings)},
	{str8_comp("Compute Stats"), str8_comp("compute_stats"), str8_comp("Average stats about beamforming computations."), (1*BeamformerPanelFlags_List)|(0*BeamformerPanelFlags_NeedsFrame)|(0*BeamformerPanelFlags_HasSettings)},
	{str8_comp("Frame View"), str8_comp("frame_view_live"), str8_comp("Latest frame with selected tag."), (1*BeamformerPanelFlags_List)|(0*BeamformerPanelFlags_NeedsFrame)|(1*BeamformerPanelFlags_HasSettings)},
	{str8_comp("Frame View (Copy)"), str8_comp("frame_view_copy"), str8_comp("Copy of an old frame. Useful for comparisons."), (0*BeamformerPanelFlags_List)|(1*BeamformerPanelFlags_NeedsFrame)|(1*BeamformerPanelFlags_HasSettings)},
	{str8_comp("3D X-Plane"), str8_comp("frame_view_xplane"), str8_comp("3D Cross Plane View."), (1*BeamformerPanelFlags_List)|(0*BeamformerPanelFlags_NeedsFrame)|(1*BeamformerPanelFlags_HasSettings)},
	{str8_comp("Live Controls"), str8_comp("live_controls"), str8_comp("Imaging system controls."), (1*BeamformerPanelFlags_List)|(0*BeamformerPanelFlags_NeedsFrame)|(0*BeamformerPanelFlags_HasSettings)},
	{str8_comp("Parameter Listing"), str8_comp("parameters_listing"), str8_comp("Editable parameter set used for beamforming."), (1*BeamformerPanelFlags_List)|(0*BeamformerPanelFlags_NeedsFrame)|(1*BeamformerPanelFlags_HasSettings)},
};

#define beamformer_registers_init_literal \
	.string           = beamformer_registers()->string, \
	.tree_node        = beamformer_registers()->tree_node, \
	.window           = beamformer_registers()->window, \
	.frame            = beamformer_registers()->frame, \
	.frame_view       = beamformer_registers()->frame_view, \
	.split_axis       = beamformer_registers()->split_axis, \
	.split_left_tree  = beamformer_registers()->split_left_tree, \
	.split_right_tree = beamformer_registers()->split_right_tree, \
	.drop_target_tree = beamformer_registers()->drop_target_tree, \
	.drop_child_index = beamformer_registers()->drop_child_index, \

read_only global BeamformerCommandInfo beamformer_command_infos[] = {
	{0},
	{str8_comp("close_tab"), str8_comp("Close Tab"), str8_comp("Closes currently active tab.")},
	{str8_comp("focus_tab"), str8_comp("Focus Tab"), str8_comp("Focus the currently selected tab in its group.")},
	{str8_comp("move_tab"), str8_comp("Move Tab"), str8_comp("Moves an exisiting tab to a different group/split.")},
	{str8_comp("open_tab"), str8_comp("Open Tab"), str8_comp("Opens a new tab with a specified view.")},
	{str8_comp("split_tree"), str8_comp("Split Tree"), str8_comp("Splits the UI into two new splits.")},
};

