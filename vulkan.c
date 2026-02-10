/* See LICENSE for license details. */
// TODO(rnp)
// [ ]: what is needed for HDR? I think it makes sense to just default to it nowadays
// [ ]: once opengl is removed switch images to SRGB and/or 16 bit Float

#include "beamformer_internal.h"
#include "vulkan.h"
#include "external/glslang/glslang/Include/glslang_c_interface.h"

#define glslang_info(s) s8("[glslang] " s)
#define vulkan_info(s)  s8("[vulkan]  " s)

#define ValidVulkanHandle(h) ((h).value[0] != 0)

#define MaxCommandBuffersInFlight  BeamformerMaxRawDataFramesInFlight
#define MaxCommandBufferTimestamps (1024)

typedef enum {
	VulkanQueueKind_Graphics,
	VulkanQueueKind_Compute,
	VulkanQueueKind_Transfer,
	VulkanQueueKind_Count,
} VulkanQueueKind;

typedef enum {
	VulkanMemoryKind_Device,
	VulkanMemoryKind_BAR,
	VulkanMemoryKind_Host,
	VulkanMemoryKind_Count,
} VulkanMemoryKind;

typedef struct {
	VkDeviceMemory    memory;
	VkBuffer          buffer;

	void *            host_pointer;

	VulkanMemoryKind  memory_kind;

	// NOTE: only used when the buffer is backing a VulkanRenderModel.
	VkIndexType       index_type;
} VulkanBuffer;

typedef struct {
	VkDeviceMemory    memory;
	VkImage           image;
	VkImageView       view;
} VulkanImage;

typedef struct {
	VkPipeline         pipeline;
	VkPipelineLayout   layout;
	VkShaderStageFlags stage_flags;
} VulkanPipeline;

typedef struct {
	VkSemaphore semaphore;
	u64         value;
} VulkanSemaphore;

typedef struct {
	VulkanQueueKind kind;
	u32             command_buffer_index;

	// NOTE(rnp): since there may not be QueueKind_Count queues, when putting values into this
	// array you must be careful to map through the queue_indices array in the vulkan_context.
	u64 in_flight_wait_values[VulkanQueueKind_Count];
} VulkanCommandBuffer;

typedef enum {
	VulkanEntityKind_Buffer,
	VulkanEntityKind_CommandBuffer,
	VulkanEntityKind_Image,
	VulkanEntityKind_Pipeline,
	VulkanEntityKind_RenderModel,
	VulkanEntityKind_Semaphore,
} VulkanEntityKind;

typedef struct VulkanEntity VulkanEntity;
struct VulkanEntity {
	VulkanEntity *   next;
	VulkanEntityKind kind;
	union {
		VulkanBuffer        buffer;
		VulkanCommandBuffer command_buffer;
		VulkanImage         image;
		VulkanPipeline      pipeline;
		VulkanSemaphore     semaphore;
	} as;
};

typedef alignas(64) struct {
	i32 lock;

	u16     queue_family;
	u16     queue_index;
	VkQueue queue;

	VkQueryPool     query_pool;
	u32             query_pool_occupied[VulkanQueueKind_Count];

	u32             next_command_buffer_index;
	VkCommandPool   command_pool;
	VkCommandBuffer command_buffers[MaxCommandBuffersInFlight];
	u64             command_buffer_submission_values[MaxCommandBuffersInFlight];

	VulkanSemaphore timeline_semaphore;

	VkPipelineStageFlags2 pipeline_stage_flags;

	VulkanPipeline *bound_pipeline;
} VulkanQueue;
static_assert(alignof(VulkanQueue) == 64, "VulkanQueue must be placed on its own cacheline");

typedef struct {
	Arena             arena;
	i32               arena_lock;

	VkInstance        handle;
	VkDevice          device;
	VkPhysicalDevice  physical_device;

	// NOTE(rnp): fallback for when a shader fails to compile
	VulkanPipeline    default_compute_pipeline;
	VulkanPipeline    default_graphics_pipeline;

	GPUInfo           gpu_info;

	struct {
		u64             max_allocation_size;
		u64             non_coherent_atom_size;
		u8              gpu_heap_index;
		i8              memory_type_indices[VulkanMemoryKind_Count];
		b8              memory_host_coherent[VulkanMemoryKind_Count];
		static_assert(VK_MAX_MEMORY_HEAPS < I8_MAX, "");
		static_assert(VK_MAX_MEMORY_TYPES < U8_MAX, "");
	} memory_info;

	VulkanQueue *     queues[VulkanQueueKind_Count];
	// NOTE(rnp): there are a few places in the code where simply going through the queues map
	// is not sufficient. those places need to know of the unique queues which unique queue
	// is being referred to. that code uses this map instead.
	u16               queue_indices[VulkanQueueKind_Count];
	u16               unique_queues;

	VkFormat          swap_chain_image_format;
	VkFormat          depth_stencil_format;

	VulkanEntity *    entity_freelist;
	Arena             entity_arena;
	i32               entity_lock;
} VulkanContext;

read_only global const char *vk_required_instance_extensions[] = {
};

#if OS_WINDOWS
#define VK_OS_REQUIRED_DEVICE_EXTENSIONS_LIST \
	X("VK_KHR_external_memory_win32") \
	X("VK_KHR_external_semaphore_win32") \

#else
#define VK_OS_REQUIRED_DEVICE_EXTENSIONS_LIST \
	X("VK_KHR_external_memory_fd") \
	X("VK_KHR_external_semaphore_fd") \

#endif

#define VK_REQUIRED_DEVICE_EXTENSIONS_LIST \
	X("VK_KHR_16bit_storage") \
	X("VK_KHR_external_memory") \
	X("VK_KHR_external_semaphore") \
	X("VK_KHR_storage_buffer_storage_class") \
	X("VK_KHR_timeline_semaphore") \
	VK_OS_REQUIRED_DEVICE_EXTENSIONS_LIST

#define X(str) str,
read_only global const char *vk_required_device_extensions[] = {
VK_REQUIRED_DEVICE_EXTENSIONS_LIST
};
#undef X

#define X(str) sizeof(str) - 1,
read_only global u32 vk_required_device_extension_name_lengths[] = {
VK_REQUIRED_DEVICE_EXTENSIONS_LIST
};
#undef X

#define VK_REQUIRED_PHYSICAL_FEATURES \
	X(shaderInt16) \
	X(shaderInt64) \

#define VK_REQUIRED_PHYSICAL_11_FEATURES \
	X(storageBuffer16BitAccess) \

#define VK_REQUIRED_PHYSICAL_12_FEATURES \
	X(bufferDeviceAddress) \
	X(shaderFloat16) \
	X(timelineSemaphore) \

#define VK_REQUIRED_PHYSICAL_13_FEATURES \
	X(dynamicRendering) \
	X(synchronization2) \

#define VK_DEBUG_EXTENSIONS \
	X(VK_KHR, shader_non_semantic_info) \
	X(VK_KHR, shader_relaxed_extended_instruction) \

#define X(p, s, ...) #p "_" #s,
read_only global const char *vk_debug_extensions[] = {VK_DEBUG_EXTENSIONS};
#undef X
#define X(p, s, ...) sizeof(#p "_" #s) - 1,
read_only global u32 vk_debug_extension_name_lengths[] = {VK_DEBUG_EXTENSIONS};
#undef X

global union {
	struct {
		#define X(_, name, ...) b8 name;
		VK_DEBUG_EXTENSIONS
		#undef X
	};
	b8 E[countof(vk_debug_extensions)];
} vulkan_debug;

global VulkanContext vulkan_context[1];

/* NOTE(rnp): the idea here is to set reasonable development constraints.
 * They should probably not match one to one with the maximums of the dev
 * machine's hardware. Instead these are here to cause compile time failure
 * for features which are not expected to work everywhere. */
global glslang_resource_t glslc_resource_constraints[1] = {{
	.max_compute_work_group_count_x = 65535,
	.max_compute_work_group_count_y = 65535,
	.max_compute_work_group_count_z = 65535,
	.max_compute_work_group_size_x  = 1024,
	.max_compute_work_group_size_y  = 1024,
	.max_compute_work_group_size_z  = 1024,

	// NOTE: taken from glslang defaults
	.max_lights = 32,
	.max_clip_planes = 6,
	.max_texture_units = 32,
	.max_texture_coords = 32,
	.max_vertex_attribs = 64,
	.max_vertex_uniform_components = 4096,
	.max_varying_floats = 64,
	.max_vertex_texture_image_units = 32,
	.max_combined_texture_image_units = 80,
	.max_texture_image_units = 32,
	.max_fragment_uniform_components = 4096,
	.max_draw_buffers = 32,
	.max_vertex_uniform_vectors = 128,
	.max_varying_vectors = 8,
	.max_fragment_uniform_vectors = 16,
	.max_vertex_output_vectors = 16,
	.max_fragment_input_vectors = 15,
	.min_program_texel_offset = -8,
	.max_program_texel_offset = 7,
	.max_clip_distances = 8,
	.max_compute_uniform_components = 1024,
	.max_compute_texture_image_units = 16,
	.max_compute_image_uniforms = 8,
	.max_compute_atomic_counters = 8,
	.max_compute_atomic_counter_buffers = 1,
	.max_varying_components = 60,
	.max_vertex_output_components = 64,
	.max_fragment_input_components = 128,
	.max_image_units = 8,
	.max_combined_image_units_and_fragment_outputs = 8,
	.max_combined_shader_output_resources = 8,
	.max_image_samples = 0,
	.max_vertex_image_uniforms = 0,
	.max_fragment_image_uniforms = 8,
	.max_combined_image_uniforms = 8,
	.max_viewports = 16,
	.max_vertex_atomic_counters = 0,
	.max_fragment_atomic_counters = 8,
	.max_combined_atomic_counters = 8,
	.max_atomic_counter_bindings = 1,
	.max_vertex_atomic_counter_buffers = 0,
	.max_fragment_atomic_counter_buffers = 1,
	.max_combined_atomic_counter_buffers = 1,
	.max_atomic_counter_buffer_size = 16384,
	.max_transform_feedback_buffers = 4,
	.max_transform_feedback_interleaved_components = 64,
	.max_cull_distances = 8,
	.max_combined_clip_and_cull_distances = 8,
	.max_samples = 4,
	.max_mesh_output_vertices_ext = 256,
	.max_mesh_output_primitives_ext = 256,
	.max_mesh_work_group_size_x_ext = 128,
	.max_mesh_work_group_size_y_ext = 128,
	.max_mesh_work_group_size_z_ext = 128,
	.max_task_work_group_size_x_ext = 128,
	.max_task_work_group_size_y_ext = 128,
	.max_task_work_group_size_z_ext = 128,
	.max_mesh_view_count_ext = 4,
	.max_dual_source_draw_buffers_ext = 1,

	.limits = {
		.non_inductive_for_loops                  = 1,
		.while_loops                              = 1,
		.do_while_loops                           = 1,
		.general_uniform_indexing                 = 1,
		.general_attribute_matrix_vector_indexing = 1,
		.general_varying_indexing                 = 1,
		.general_sampler_indexing                 = 1,
		.general_variable_indexing                = 1,
		.general_constant_matrix_vector_indexing  = 1,
	},
}};


#if BEAMFORMER_RENDERDOC_HOOKS
DEBUG_IMPORT void *
vk_renderdoc_instance_handle(void)
{
	return *((void **)vulkan_context->handle);
}
#endif

function VulkanEntity *
vk_entity_allocate(VulkanEntityKind kind)
{
	VulkanEntity *result = 0;
	DeferLoop(take_lock(&vulkan_context->entity_lock, -1), release_lock(&vulkan_context->entity_lock))
	{
		result = SLLPopFreelist(vulkan_context->entity_freelist);
		if (!result) result = push_array_no_zero(&vulkan_context->entity_arena, VulkanEntity, 1);
	}

	zero_struct(result);
	result->kind = kind;
	return result;
}

function void
vk_entity_release(VulkanEntity *entity)
{
	DeferLoop(take_lock(&vulkan_context->entity_lock, -1), release_lock(&vulkan_context->entity_lock))
	{
		SLLStackPush(vulkan_context->entity_freelist, entity);
	}
}

function void *
vk_entity_data(VulkanHandle h, VulkanEntityKind kind)
{
	VulkanEntity *e = (VulkanEntity *)h.value[0];
	assert(ValidVulkanHandle(h) && e->kind == kind);
	return &e->as;
}

function VkCommandBuffer
vk_command_buffer(VulkanHandle h)
{
	VulkanCommandBuffer *vcb = vk_entity_data(h, VulkanEntityKind_CommandBuffer);
	VulkanQueue         *vq  = vulkan_context->queues[vcb->kind];

	VkCommandBuffer result = vq->command_buffers[vcb->command_buffer_index];
	return result;
}

#define glslang_log(a, ...) glslang_log_(a, arg_list(s8, __VA_ARGS__))
function void
glslang_log_(Arena arena, s8 *items, uz count)
{
	Stream sb = arena_stream(arena);
	stream_append_s8(&sb, glslang_info(""));
	stream_append_s8s_(&sb, items, count);
	if (sb.data[sb.widx - 1] != '\n') stream_append_byte(&sb, '\n');
	os_console_log(sb.data, sb.widx);
}

function s8
glsl_to_spirv(Arena *arena, u32 kind, s8 shader_text, s8 name)
{
	/* NOTE(rnp): glslang's garbage c interface doesn't expose internal usage of strings with length */
	assert(shader_text.data[shader_text.len] == 0);

	glslang_input_t input = {
		.language                          = GLSLANG_SOURCE_GLSL,
		.stage                             = kind,
		.client                            = GLSLANG_CLIENT_VULKAN,
		.client_version                    = GLSLANG_TARGET_VULKAN_1_4,
		.target_language                   = GLSLANG_TARGET_SPV,
		.target_language_version           = GLSLANG_TARGET_SPV_1_6,
		.code                              = (c8 *)shader_text.data,
		.default_version                   = 460,
		.default_profile                   = GLSLANG_NO_PROFILE,
		.force_default_version_and_profile = 0,
		.forward_compatible                = 0,
		.messages                          = GLSLANG_MSG_DEFAULT_BIT,
		.resource                          = glslc_resource_constraints,
	};
	glslang_shader_t *shader = glslang_shader_create(&input);

	s8 error = {0};
	if (glslang_shader_preprocess(shader, &input)) {
		if (!glslang_shader_parse(shader, &input))
			error = s8("parsing failed");
	} else {
		error = s8("preprocessing failed");
	}

	if (error.len) {
		glslang_log(*arena, name, s8(": "), error, s8("\n"),
		            c_str_to_s8((c8 *)glslang_shader_get_info_log(shader)),
		            c_str_to_s8((c8 *)glslang_shader_get_info_debug_log(shader)));
		glslang_shader_delete(shader);
		shader = 0;
	}

	s8 result = {0};
	if (shader) {
		glslang_program_t *program = glslang_program_create();
		glslang_program_add_shader(program, shader);
		i32 messages = GLSLANG_MSG_DEBUG_INFO_BIT|GLSLANG_MSG_SPV_RULES_BIT|GLSLANG_MSG_VULKAN_RULES_BIT;
		if (glslang_program_link(program, messages)) {
			glslang_spv_options_t options = {.validate = 1,};

			if (vulkan_debug.shader_non_semantic_info) {
				options.generate_debug_info                  = 1;
				options.emit_nonsemantic_shader_debug_info   = 1;
				options.emit_nonsemantic_shader_debug_source = 1;
			}

			glslang_program_add_source_text(program, kind, (c8 *)shader_text.data, shader_text.len);
			glslang_program_SPIRV_generate_with_options(program, kind, &options);

			u32 words   = glslang_program_SPIRV_get_size(program);
			result.data = (u8 *)push_array(arena, u32, words);
			result.len  = words * sizeof(u32);
			glslang_program_SPIRV_get(program, (u32 *)result.data);

			s8 spirv_msg = c_str_to_s8((c8 *)glslang_program_SPIRV_get_messages(program));
			if (spirv_msg.len) glslang_log(*arena, name, s8(": spirv info: "), spirv_msg);
		} else {
			glslang_log(*arena, name, s8(": shader linking failed\n"),
			            c_str_to_s8((c8 *)glslang_program_get_info_log(program)),
			            c_str_to_s8((c8 *)glslang_program_get_info_debug_log(program)));
		}
		glslang_shader_delete(shader);
		glslang_program_delete(program);
	}

	return result;
}

function u32
vk_shader_kind_to_glslang_shader_kind(u32 kind)
{
	u32 result = ctz_u64(kind);
	return result;
}

function VkShaderModule
vk_compile_shader_module(Arena arena, u32 kind, s8 text, s8 name)
{
	VkShaderModule result = {0};
	s8 spirv = glsl_to_spirv(&arena, vk_shader_kind_to_glslang_shader_kind(kind), text, name);
	VkShaderModuleCreateInfo create_info = {
		.sType    = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
		.codeSize = (uz)spirv.len,
		.pCode    = (u32 *)spirv.data,
	};
	if (spirv.len > 0) vkCreateShaderModule(vulkan_context->device, &create_info, 0, &result);

	return result;
}

function VkShaderStageFlags
vk_stage_flags_from_shader_kind(VulkanShaderKind kind)
{
	read_only local_persist VkShaderStageFlags map[VulkanShaderKind_Count + 1] = {
		[VulkanShaderKind_Vertex]   = VK_SHADER_STAGE_VERTEX_BIT,
		[VulkanShaderKind_Mesh]     = VK_SHADER_STAGE_MESH_BIT_EXT,
		[VulkanShaderKind_Fragment] = VK_SHADER_STAGE_FRAGMENT_BIT,
		[VulkanShaderKind_Compute]  = VK_SHADER_STAGE_COMPUTE_BIT,
		[VulkanShaderKind_Count]    = 0,
	};
	VkShaderStageFlags result = map[Clamp((u32)kind, 0, VulkanShaderKind_Count)];
	return result;
}

function VulkanPipeline
vk_compute_pipeline_from_shader_text(Arena arena, s8 text, s8 name, u32 push_constants_size)
{
	VulkanPipeline result = {.stage_flags = VK_SHADER_STAGE_COMPUTE_BIT};
	VkShaderModule module = vk_compile_shader_module(arena, VK_SHADER_STAGE_COMPUTE_BIT, text, name);
	if (module) {
		VkPushConstantRange push_constant_range = {
			.stageFlags = VK_SHADER_STAGE_COMPUTE_BIT,
			.offset     = 0,
			.size       = push_constants_size,
		};

		VkPipelineLayoutCreateInfo pipeline_layout_create_info = {
			.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
			.pushConstantRangeCount = push_constants_size ? 1 : 0,
			.pPushConstantRanges    = push_constants_size ? &push_constant_range : 0,
		};

		vkCreatePipelineLayout(vulkan_context->device, &pipeline_layout_create_info, 0, &result.layout);

		VkComputePipelineCreateInfo pipeline_create_info = {
			.sType  = VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
			.layout = result.layout,
			.stage  = {
				.sType  = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
				.stage  = VK_SHADER_STAGE_COMPUTE_BIT,
				.module = module,
				.pName  = "main",
			},
		};

		vkCreateComputePipelines(vulkan_context->device, 0, 1, &pipeline_create_info, 0, &result.pipeline);
		vkDestroyShaderModule(vulkan_context->device, module, 0);
	}
	if (result.pipeline == 0) result = vulkan_context->default_compute_pipeline;

	return result;
}

function VulkanPipeline
vk_graphics_pipeline_from_infos(Arena arena, VulkanPipelineCreateInfo *infos, u32 count, u32 push_constants_size)
{
	assume(count == 2);

	VulkanPipeline result = {0};
	VkShaderModule modules[2];

	modules[0] = vk_compile_shader_module(arena, vk_stage_flags_from_shader_kind(infos[0].kind),
	                                      infos[0].text, infos[0].name);
	modules[1] = vk_compile_shader_module(arena, vk_stage_flags_from_shader_kind(infos[1].kind),
	                                      infos[1].text, infos[1].name);
	if (modules[0] && modules[1]) {
		result.stage_flags = vk_stage_flags_from_shader_kind(infos[0].kind)
		                     | vk_stage_flags_from_shader_kind(infos[1].kind);

		VkPushConstantRange pcr = {
			.stageFlags = result.stage_flags,
			.offset     = 0,
			.size       = push_constants_size,
		};

		VkPipelineLayoutCreateInfo pipeline_layout_info = {
			.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
			.pushConstantRangeCount = push_constants_size ? 1    : 0,
			.pPushConstantRanges    = push_constants_size ? &pcr : 0,
		};

		vkCreatePipelineLayout(vulkan_context->device, &pipeline_layout_info, 0, &result.layout);

		VkPipelineShaderStageCreateInfo shader_stage_create_infos[2] = {
			{
				.sType  = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
				.stage  = vk_stage_flags_from_shader_kind(infos[0].kind),
				.module = modules[0],
				.pName  = "main",
			},
			{
				.sType  = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
				.stage  = vk_stage_flags_from_shader_kind(infos[1].kind),
				.module = modules[1],
				.pName  = "main",
			},
		};

		VkPipelineVertexInputStateCreateInfo vertex_input_info = {
			.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO,
		};

		VkPipelineInputAssemblyStateCreateInfo input_assembly_info = {
			.sType    = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,
			.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
		};

		VkPipelineViewportStateCreateInfo viewport_info = {
			.sType         = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO,
			.viewportCount = 1,
			.scissorCount  = 1,
		};

		VkPipelineRasterizationStateCreateInfo rasterization_info = {
			.sType       = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,
			.polygonMode = VK_POLYGON_MODE_FILL,
			.lineWidth   = 1.0f,
			.cullMode    = VK_CULL_MODE_BACK_BIT,
			.frontFace   = VK_FRONT_FACE_CLOCKWISE,
		};

		VkPipelineMultisampleStateCreateInfo multisampling_info = {
			.sType                = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,
			.rasterizationSamples = vulkan_context->gpu_info.max_msaa_samples,
		};

		VkPipelineDepthStencilStateCreateInfo depth_test_create_info = {
			.sType = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO,
			.depthTestEnable       = 1,
			.depthWriteEnable      = 1,
			.depthCompareOp        = VK_COMPARE_OP_LESS,
			.depthBoundsTestEnable = 1,
			.stencilTestEnable     = 0,
			.front                 = {0},
			.back                  = {0},
			.minDepthBounds        = 0.0f,
			.maxDepthBounds        = 1.0f,
		};

		u32 colour_mask = VK_COLOR_COMPONENT_R_BIT|VK_COLOR_COMPONENT_G_BIT|VK_COLOR_COMPONENT_B_BIT|VK_COLOR_COMPONENT_A_BIT;
		VkPipelineColorBlendAttachmentState blend_state = {
			.colorWriteMask      = colour_mask,
			.blendEnable         = 1,
			.srcColorBlendFactor = VK_BLEND_FACTOR_SRC_ALPHA,
			.dstColorBlendFactor = VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
			.colorBlendOp        = VK_BLEND_OP_ADD,
			.srcAlphaBlendFactor = VK_BLEND_FACTOR_ONE,
			.dstAlphaBlendFactor = VK_BLEND_FACTOR_ZERO,
			.alphaBlendOp        = VK_BLEND_OP_ADD,
		};

		VkPipelineColorBlendStateCreateInfo colour_blend_state_create = {
			.sType           = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO,
			.logicOpEnable   = 0,
			.logicOp         = VK_LOGIC_OP_COPY,
			.attachmentCount = 1,
			.pAttachments    = &blend_state,
		};

		VkDynamicState dynamic_states[] = {
			VK_DYNAMIC_STATE_VIEWPORT,
			VK_DYNAMIC_STATE_SCISSOR,
		};

		VkPipelineDynamicStateCreateInfo dynamic_state_info = {
			.sType             = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO,
			.dynamicStateCount = countof(dynamic_states),
			.pDynamicStates    = dynamic_states,
		};

		//VkFormat colour_attachment_format = VK_FORMAT_R8G8B8A8_SRGB;
		VkFormat colour_attachment_format = VK_FORMAT_R8G8B8A8_UNORM;
		VkPipelineRenderingCreateInfo rendering_create_info = {
			.sType                   = VK_STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO,
			.colorAttachmentCount    = 1,
			.pColorAttachmentFormats = &colour_attachment_format,
			.depthAttachmentFormat   = vulkan_context->depth_stencil_format,
			.stencilAttachmentFormat = vulkan_context->depth_stencil_format,
		};

		VkGraphicsPipelineCreateInfo pci = {
			.sType               = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO,
			.pNext               = &rendering_create_info,
			.stageCount          = countof(shader_stage_create_infos),
			.pStages             = shader_stage_create_infos,
			.pVertexInputState   = &vertex_input_info,
			.pInputAssemblyState = &input_assembly_info,
			.pViewportState      = &viewport_info,
			.pRasterizationState = &rasterization_info,
			.pMultisampleState   = &multisampling_info,
			.pDepthStencilState  = &depth_test_create_info,
			.pColorBlendState    = &colour_blend_state_create,
			.pDynamicState       = &dynamic_state_info,
			.layout              = result.layout,
		};

		vkCreateGraphicsPipelines(vulkan_context->device, 0, 1, &pci,0, &result.pipeline);
	}

	if (modules[0]) vkDestroyShaderModule(vulkan_context->device, modules[0], 0);
	if (modules[1]) vkDestroyShaderModule(vulkan_context->device, modules[1], 0);

	if (result.pipeline == 0) result = vulkan_context->default_graphics_pipeline;

	return result;
}

function VulkanSemaphore
vk_make_semaphore(OSHandle *export)
{
	VulkanContext *vk = vulkan_context;

	VkSemaphoreCreateInfo       sci  = {.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO};
	VkExportSemaphoreCreateInfo esci = {
		.sType       = VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO,
		.handleTypes = OS_WINDOWS ? VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT
		                          : VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT,
	};
	VkSemaphoreTypeCreateInfo stc = {
		.sType         = VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO,
		.semaphoreType = VK_SEMAPHORE_TYPE_TIMELINE,
	};

	if (export) sci.pNext = &esci;
	else        sci.pNext = &stc;

	VulkanSemaphore result = {0};

	vkCreateSemaphore(vk->device, &sci, 0, &result.semaphore);

	if (export) {
		if (OS_WINDOWS) {
			VkSemaphoreGetWin32HandleInfoKHR ghi = {
				.sType      = VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR,
				.handleType = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT,
				.semaphore  = result.semaphore,
			};
			void *handle;
			vkGetSemaphoreWin32HandleKHR(vk->device, &ghi, &handle);
			export->value[0] = (u64)handle;
		} else {
			VkSemaphoreGetFdInfoKHR ghi = {
				.sType      = VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR,
				.handleType = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT,
				.semaphore  = result.semaphore,
			};
			i32 handle;
			vkGetSemaphoreFdKHR(vk->device, &ghi, &handle);
			export->value[0] = (u64)handle;
		}
	}

	return result;
}

function void
vk_release_memory(VkDeviceMemory memory, u64 size)
{
	VulkanContext *vk = vulkan_context;
	vkFreeMemory(vk->device, memory, 0);
	atomic_add_u64(&vk->gpu_info.gpu_heap_used, -size);
}

function b32
vk_allocate_memory(VkDeviceMemory *memory, u64 size, VulkanMemoryKind kind, VkMemoryAllocateFlags flags,
                   VkMemoryDedicatedAllocateInfo *dedicated_allocate_info, OSHandle *export)
{
	VulkanContext *vk = vulkan_context;

	VkExportMemoryAllocateInfo export_info = {
		.sType       = VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO,
		.handleTypes = OS_WINDOWS ? VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT
		                          : VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT,
	};

	VkMemoryAllocateFlagsInfo memory_allocate_flags_info = {
		.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO,
		.flags = flags,
		.pNext = dedicated_allocate_info,
	};

	if (export) {
		export_info.pNext = dedicated_allocate_info;
		memory_allocate_flags_info.pNext = &export_info;
	}

	VkMemoryAllocateInfo memory_allocate_info = {
		.sType           = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
		.allocationSize  = size,
		.memoryTypeIndex = vk->memory_info.memory_type_indices[kind],
		.pNext           = &memory_allocate_flags_info,
	};

	b32 result = vkAllocateMemory(vk->device, &memory_allocate_info, 0, memory) == VK_SUCCESS;
	if (result) {
		atomic_add_u64(&vk->gpu_info.gpu_heap_used, memory_allocate_info.allocationSize);

		if (export) {
			if (OS_WINDOWS) {
				VkMemoryGetWin32HandleInfoKHR handle_info = {
					.sType      = VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR,
					.memory     = *memory,
					.handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT,
				};
				void *handle;
				vkGetMemoryWin32HandleKHR(vk->device, &handle_info, &handle);
				export->value[0] = (u64)handle;
			} else {
				VkMemoryGetFdInfoKHR fd_info = {
					.sType      = VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR,
					.memory     = *memory,
					.handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT,
				};
				i32 fd;
				vkGetMemoryFdKHR(vk->device, &fd_info, &fd);
				export->value[0] = (u64)fd;
			}
		}
	}
	return result;
}

function u32
vk_index_size(VkIndexType type)
{
	u32 result = 0;
	switch (type) {
	case VK_INDEX_TYPE_UINT16:{ result = 2; }break;
	case VK_INDEX_TYPE_UINT32:{ result = 4; }break;
	InvalidDefaultCase;
	}
	return result;
}

typedef struct {
	GPUBuffer        *gpu_buffer;
	u64               size;
	VulkanUsageFlags  flags;
	u32               queue_family_count;
	u32               queue_family_indices[VulkanTimeline_Count];
	VkIndexType       index_type;
	s8                label;
} VulkanBufferAllocateInfo;

function b32
vk_buffer_allocate_common(VulkanBuffer *vb, VulkanBufferAllocateInfo *ai)
{
	VulkanContext *vk = vulkan_context;

	// TODO(rnp): this probably should be handled, its usually 4GB. likely
	// need to chain multiple allocations and handle it in shader code
	u64 size = Min(ai->size, vk->memory_info.max_allocation_size & ~(vk->memory_info.non_coherent_atom_size - 1));

	VkBufferCreateInfo buffer_create_info = {
		.sType       = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
		.usage       = VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT,
		.size        = size,
		.sharingMode = ai->queue_family_count > 1 ? VK_SHARING_MODE_CONCURRENT : VK_SHARING_MODE_EXCLUSIVE,
		.queueFamilyIndexCount = ai->queue_family_count,
		.pQueueFamilyIndices   = ai->queue_family_indices,
	};

	if (ai->flags & VulkanUsageFlag_TransferSource)
		buffer_create_info.usage |= VK_BUFFER_USAGE_TRANSFER_SRC_BIT;

	if (ai->flags & VulkanUsageFlag_TransferDestination)
		buffer_create_info.usage |= VK_BUFFER_USAGE_TRANSFER_DST_BIT;

	if (ai->index_type != VK_INDEX_TYPE_NONE_KHR)
		buffer_create_info.usage |= VK_BUFFER_USAGE_INDEX_BUFFER_BIT;

	vkCreateBuffer(vk->device, &buffer_create_info, 0, &vb->buffer);

	VkMemoryRequirements memory_requirements;
	vkGetBufferMemoryRequirements(vk->device, vb->buffer, &memory_requirements);

	assert((u64)size <= memory_requirements.size);
	size = memory_requirements.size;

	VkMemoryDedicatedAllocateInfo dedicated_allocate_info = {
		.sType  = VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO,
		.buffer = vb->buffer,
	};

	/* NOTE(rnp): to create a CPU writable buffer:
	 * 1. try to allocate and map the entire buffer
	 *    - this may fail if the buffer is bigger than the BAR size
	 *      (unknowable from vulkan), or the memory space has become
	 *      too fragmented (unlikely)
	 * 2. if allocation or mapping fails we must chain a host buffer
	 *    for staging. If this happens in practice we should add
	 *    the ability to import an existing external allocation
	 */
	b32 host_read_write = (ai->flags & VulkanUsageFlag_HostReadWrite) != 0;
	vb->memory_kind = host_read_write ? VulkanMemoryKind_BAR : VulkanMemoryKind_Device;

	b32 result = 0;
	// TODO(rnp): this may fail if the allocation is too big for the BAR size
	// it needs to handled properly
	if (vk_allocate_memory(&vb->memory, size, vb->memory_kind, VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT, &dedicated_allocate_info, 0)) {
		result  = 1;
		ai->gpu_buffer->size = size;

		vb->index_type = ai->index_type;

		if (host_read_write)
			vkMapMemory(vk->device, vb->memory, 0, size, 0, &vb->host_pointer);

		vkBindBufferMemory(vk->device, vb->buffer, vb->memory, 0);
		VkBufferDeviceAddressInfo buffer_device_address_info = {
			.sType  = VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO,
			.buffer = vb->buffer,
		};
		ai->gpu_buffer->gpu_pointer = vkGetBufferDeviceAddress(vk->device, &buffer_device_address_info);
	}
	return result;
}

function void
vk_load_instance(void)
{
	#define X(name, ...) name = (name##_fn *)vkGetInstanceProcAddr(0, #name);
	VkBaseProcedureList
	#undef X

	VkApplicationInfo app_info = {
		.sType              = VK_STRUCTURE_TYPE_APPLICATION_INFO,
		.pApplicationName   = BEAMFORMER_NAME_STRING,
		.applicationVersion = VK_MAKE_API_VERSION(0, 1, 0, 0),
		.pEngineName        = "No Engine",
		.engineVersion      = VK_MAKE_API_VERSION(0, 4, 0, 0),
		.apiVersion         = VK_MAKE_API_VERSION(0, 4, 0, 0),
	};

	/* TODO(rnp): debug only, and check for these before enabling */
	const char *validation_layers[] = {
		#if BEAMFORMER_DEBUG
		"VK_LAYER_KHRONOS_validation",
		#endif
	};

	VkInstanceCreateInfo instance_create_info = {
		.sType                   = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
		.pApplicationInfo        = &app_info,
		.ppEnabledExtensionNames = vk_required_instance_extensions,
		.enabledExtensionCount   = countof(vk_required_instance_extensions),
		.ppEnabledLayerNames     = validation_layers,
		.enabledLayerCount       = countof(validation_layers),
	};

	#if 0 && BEAMFORMER_DEBUG
	VkValidationFeatureEnableEXT validation_feature_enables[] = {
		VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT,
		VK_VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT,
		VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT,
		VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT,
	};

	VkValidationFeaturesEXT validation_features = {
		.sType                         = VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT,
		.enabledValidationFeatureCount = countof(validation_feature_enables),
		.pEnabledValidationFeatures    = validation_feature_enables,
	};

	instance_create_info.pNext = &validation_features;
	#endif

	vkCreateInstance(&instance_create_info, 0, &vulkan_context->handle);

	#define X(name, ...) name = (name##_fn *)vkGetInstanceProcAddr(vulkan_context->handle, #name);
	VkInstanceProcedureList
	#undef X
}

function void
vk_load_physical_device(Arena arena, Stream *err)
{
	VulkanContext *vk = vulkan_context;

	u32 device_count;
	vkEnumeratePhysicalDevices(vk->handle, &device_count, 0);

	VkPhysicalDevice *devices = push_array(&arena, typeof(*devices), device_count);
	vkEnumeratePhysicalDevices(vk->handle, &device_count, devices);

	i32 best_index = -1, best_score = -1;
	for (u32 i = 0; i < device_count; i++) {
		Arena scratch = arena;
		VkPhysicalDeviceProperties2 *dp = push_struct(&scratch, typeof(*dp));
		dp->sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2;
		vkGetPhysicalDeviceProperties2(devices[i], dp);

		i32 score = 0;
		if (dp->properties.deviceType == VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU)
			score++;

		if (score > best_score) {
			best_score = score;
			best_index = (i32)i;
		}
	}

	vk->physical_device = best_index >= 0 ? devices[best_index] : 0;
	if (!vk->physical_device)
		fatal(vulkan_info("failed to find a suitable GPU\n"));

	VkPhysicalDeviceProperties2        dp   = {.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2};
	VkPhysicalDeviceVulkan11Properties v11p = {.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES};
	dp.pNext = &v11p;

	vkGetPhysicalDeviceProperties2(vk->physical_device, &dp);

	stream_append_s8s(err, vulkan_info("selecting device: "), c_str_to_s8(dp.properties.deviceName), s8("\n"));

	{
		Arena scratch = arena;
		u32 extension_count = 0;
		vkEnumerateDeviceExtensionProperties(vk->physical_device, 0, &extension_count, 0);
		VkExtensionProperties *extensions = push_array(&scratch, VkExtensionProperties, extension_count);
		vkEnumerateDeviceExtensionProperties(vk->physical_device, 0, &extension_count, extensions);

		s8 *ext_str8s = push_array(&scratch, s8, extension_count);
		for (u32 index = 0; index < extension_count; index++)
			ext_str8s[index] = c_str_to_s8(extensions[index].extensionName);

		b8 *supported = push_array(&scratch, b8, countof(vk_required_device_extensions));
		for (u32 index = 0; index < extension_count; index++) {
			for EachElement(vk_required_device_extensions, it) {
				s8 test = {
					.data = (u8 *)vk_required_device_extensions[it],
					.len  = vk_required_device_extension_name_lengths[it],
				};
				supported[it] |= s8_equal(test, ext_str8s[index]);
			}
		}

		u32 supported_count = 0;
		for EachElement(vk_required_device_extensions, it)
		 supported_count += supported[it];

		u32 missing_count = countof(vk_required_device_extensions) - supported_count;
		if (missing_count) {
			stream_append_s8s(err, vulkan_info("fatal error: missing required device extension"),
			                  missing_count > 1 ? s8("s") : s8(""), s8(":\n"));
			for EachElement(vk_required_device_extensions, it) {
				if (!supported[it]) {
					s8 name = {
						.data = (u8 *)vk_required_device_extensions[it],
						.len  = vk_required_device_extension_name_lengths[it],
					};
					stream_append_s8s(err, vulkan_info("    "), name, s8("\n"));
				}
			}
			fatal(stream_to_s8(err));
		}

		#if BEAMFORMER_DEBUG
		for (u32 index = 0; index < extension_count; index++) {
			for EachElement(vk_debug_extensions, it) {
				s8 test = {
					.data = (u8 *)vk_debug_extensions[it],
					.len  = vk_debug_extension_name_lengths[it],
				};
				vulkan_debug.E[it] |= s8_equal(test, ext_str8s[index]);
			}
		}
		#endif
	}

	{
		VkPhysicalDeviceFeatures2        df   = {.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2};
		VkPhysicalDeviceVulkan11Features v11f = {.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES};
		VkPhysicalDeviceVulkan12Features v12f = {.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES};
		VkPhysicalDeviceVulkan13Features v13f = {.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES};
		df.pNext   = &v11f;
		v11f.pNext = &v12f;
		v12f.pNext = &v13f;
		vkGetPhysicalDeviceFeatures2(vk->physical_device, &df);

		{
			b32 all_supported = 1;
			#define X(name, ...) all_supported &= df.features.name;
			VK_REQUIRED_PHYSICAL_FEATURES
			#undef X

			if (!all_supported) {
				stream_append_s8(err, vulkan_info("fatal error: missing physical device features:\n"));
				#define X(name, ...) if (!df.features.name) stream_append_s8(err, s8("    " #name "\n"));
				VK_REQUIRED_PHYSICAL_FEATURES
				#undef X
				fatal(stream_to_s8(err));
			}
		}

		{
			b32 all_supported = 1;
			#define X(name, ...) all_supported &= v11f.name;
			VK_REQUIRED_PHYSICAL_11_FEATURES
			#undef X

			if (!all_supported) {
				stream_append_s8(err, vulkan_info("fatal error: missing physical device features:\n"));
				#define X(name, ...) if (!v11f.name) stream_append_s8(err, s8("    " #name "\n"));
				VK_REQUIRED_PHYSICAL_11_FEATURES
				#undef X
				fatal(stream_to_s8(err));
			}
		}

		{
			b32 all_supported = 1;
			#define X(name, ...) all_supported &= v12f.name;
			VK_REQUIRED_PHYSICAL_12_FEATURES
			#undef X

			if (!all_supported) {
				stream_append_s8(err, vulkan_info("fatal error: missing physical device features:\n"));
				#define X(name, ...) if (!v12f.name) stream_append_s8(err, s8("    " #name "\n"));
				VK_REQUIRED_PHYSICAL_12_FEATURES
				#undef X
				fatal(stream_to_s8(err));
			}
		}

		{
			b32 all_supported = 1;
			#define X(name, ...) all_supported &= v13f.name;
			VK_REQUIRED_PHYSICAL_13_FEATURES
			#undef X

			if (!all_supported) {
				stream_append_s8(err, vulkan_info("fatal error: missing physical device features:\n"));
				#define X(name, ...) if (!v13f.name) stream_append_s8(err, s8("    " #name "\n"));
				VK_REQUIRED_PHYSICAL_13_FEATURES
				#undef X
				fatal(stream_to_s8(err));
			}
		}
	}

	VkPhysicalDeviceMemoryProperties2 mp = {.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2};
	vkGetPhysicalDeviceMemoryProperties2(vk->physical_device, &mp);

	VkPhysicalDeviceMemoryProperties *bmp = &mp.memoryProperties;

	// NOTE(rnp): vulkan spec says that highest performance memory types must
	// come first. just take the first one found.

	for (u32 i = 0; i < bmp->memoryHeapCount; i++) {
		if (bmp->memoryHeaps[i].flags & VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) {
			vk->memory_info.gpu_heap_index = i;
			break;
		}
	}

	for (u32 i = 0; i < bmp->memoryTypeCount; i++) {
		if (bmp->memoryTypes[i].propertyFlags & VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) {
			assert(bmp->memoryTypes[i].heapIndex == vk->memory_info.gpu_heap_index);
			vk->memory_info.memory_type_indices[VulkanMemoryKind_Device] = i;
			break;
		}
	}

	// TODO(rnp): it is possible that this isn't available. for devices like that we would need
	// to copy into a staging buffer then DMA. For now that is unsupported.
	u32 bar_flags = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT|VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT;
	i32 bar_index = -1;
	for (u32 i = 0; i < bmp->memoryTypeCount; i++) {
		if ((bmp->memoryTypes[i].propertyFlags & bar_flags) == bar_flags) {
			assert(bmp->memoryTypes[i].heapIndex == vk->memory_info.gpu_heap_index);
			bar_index = (i32)i;
			break;
		}
	}

	// TODO(rnp): this shouldn't be fatal
	if (bar_index == -1) {
		stream_append_s8(err, vulkan_info("fatal error: GPU does not support host bar memory\n"));
		fatal(stream_to_s8(err));
	}

	vk->memory_info.memory_type_indices[VulkanMemoryKind_BAR] = bar_index;

	for (u32 i = 0; i < bmp->memoryTypeCount; i++) {
		if ((bmp->memoryTypes[i].propertyFlags & VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) == 0) {
			assert(bmp->memoryTypes[i].propertyFlags & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT);
			vk->memory_info.memory_type_indices[VulkanMemoryKind_Host] = i;
			break;
		}
	}

	for EachElement(vk->memory_info.memory_type_indices, it) {
		u32 ti    = vk->memory_info.memory_type_indices[it];
		u32 flags = bmp->memoryTypes[ti].propertyFlags;
		vk->memory_info.memory_host_coherent[it] = (flags & VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) != 0;
	}

	vk->memory_info.max_allocation_size    = v11p.maxMemoryAllocationSize;
	vk->memory_info.non_coherent_atom_size = dp.properties.limits.nonCoherentAtomSize;
	vk->gpu_info.vendor                    = dp.properties.vendorID;
	vk->gpu_info.gpu_heap_size             = bmp->memoryHeaps[vk->memory_info.gpu_heap_index].size;
	vk->gpu_info.timestamp_period_ns       = dp.properties.limits.timestampPeriod;
	vk->gpu_info.max_image_dimension_2D    = dp.properties.limits.maxImageDimension2D;
	vk->gpu_info.max_image_dimension_3D    = dp.properties.limits.maxImageDimension3D;
	vk->gpu_info.max_msaa_samples          = round_down_power_of_two(dp.properties.limits.framebufferColorSampleCounts);
	vk->gpu_info.subgroup_size             = v11p.subgroupSize;
	vk->gpu_info.max_compute_shared_memory_size = dp.properties.limits.maxComputeSharedMemorySize;

	// IMPORTANT(rnp): memory must only be pushed at the end of the function
	vk->gpu_info.name = push_s8(&vk->arena, c_str_to_s8(dp.properties.deviceName));
}

function void
vk_load_queues(Arena *memory, Stream *err)
{
	///////////////////////////////////////////////////////
	// NOTE(rnp): try to allocate an appropriate queue for
	// each of the following tasks:
	//   * UI Rendering (Graphics)
	//   * Beamforming  (Compute)
	//   * Upload       (Transfer)
	// Then create a logical device ready for use

	VulkanContext *vk = vulkan_context;

	u32 queue_family_count;
	vkGetPhysicalDeviceQueueFamilyProperties(vk->physical_device, &queue_family_count, 0);

	TempArena arena_save = begin_temp_arena(memory);
	VkQueueFamilyProperties *queues = push_array(memory, typeof(*queues), queue_family_count);
	vkGetPhysicalDeviceQueueFamilyProperties(vk->physical_device, &queue_family_count, queues);

	i32 queue_indices[VulkanQueueKind_Count];
	for EachElement(queue_indices, it) queue_indices[it] = -1;

	///////////////////////////////////////////////////////////////
	// NOTE(rnp): start by assigning queue families for each queue

	/* NOTE(rnp): try for exclusive transfer queue */
	{
		u32 mask = VK_QUEUE_GRAPHICS_BIT|VK_QUEUE_COMPUTE_BIT|VK_QUEUE_TRANSFER_BIT;
		u32 max_timestamp_bits = 0;
		for (u32 index = 0; index < queue_family_count; index++) {
			if ((queues[index].queueFlags & mask) == VK_QUEUE_TRANSFER_BIT) {
				if (queues[index].timestampValidBits > max_timestamp_bits) {
					max_timestamp_bits = queues[index].timestampValidBits;
					queue_indices[VulkanQueueKind_Transfer] = (i32)index;
				}
			}
		}
	}

	/* NOTE(rnp): try for compute separate from graphics */
	for (u32 index = 0; index < queue_family_count; index++) {
		if ((queues[index].queueFlags & VK_QUEUE_COMPUTE_BIT)  != 0 &&
		    (queues[index].queueFlags & VK_QUEUE_GRAPHICS_BIT) == 0)
		{
			queue_indices[VulkanQueueKind_Compute] = (i32)index;
			break;
		}
	}

	/* NOTE(rnp): find graphics family and verify it is exclusive */
	b32 multi_graphics = 0;
	for (u32 index = 0; index < queue_family_count; index++) {
		if ((queues[index].queueFlags & VK_QUEUE_GRAPHICS_BIT) != 0) {
			// TODO(rnp): check for presentation support
			multi_graphics = queue_indices[VulkanQueueKind_Graphics] != -1;
			queue_indices[VulkanQueueKind_Graphics] = (i32)index;
		}
	}

	if (multi_graphics)
		stream_append_s8(err, vulkan_info("warning: multiple queue families reported graphics support\n"));

	if (queue_indices[VulkanQueueKind_Graphics] == -1) {
		stream_append_s8(err, vulkan_info("fatal error: GPU does not support graphics presentation\n"));
		fatal(stream_to_s8(err));
	}

	if (queue_indices[VulkanQueueKind_Compute] == -1)
		if ((queues[queue_indices[VulkanQueueKind_Graphics]].queueFlags & VK_QUEUE_COMPUTE_BIT) != 0)
			queue_indices[VulkanQueueKind_Compute] = queue_indices[VulkanQueueKind_Graphics];

	if (queue_indices[VulkanQueueKind_Compute] == -1) {
		stream_append_s8(err, vulkan_info("fatal error: GPU does not support compute\n"));
		fatal(stream_to_s8(err));
	}

	if (queue_indices[VulkanQueueKind_Transfer] == -1) {
		if ((queues[queue_indices[VulkanQueueKind_Compute]].queueFlags & VK_QUEUE_TRANSFER_BIT) != 0)
			queue_indices[VulkanQueueKind_Transfer] = queue_indices[VulkanQueueKind_Compute];
		else if ((queues[queue_indices[VulkanQueueKind_Graphics]].queueFlags & VK_QUEUE_TRANSFER_BIT) != 0)
			queue_indices[VulkanQueueKind_Transfer] = queue_indices[VulkanQueueKind_Graphics];
	}

	if (queue_indices[VulkanQueueKind_Transfer] == -1) {
		stream_append_s8(err, vulkan_info("fatal error: GPU does not support data transfer\n"));
		fatal(stream_to_s8(err));
	}

	/////////////////////////////////////////////////////////////////
	// NOTE(rnp): if queues share families try to allocate subqueues

	u32 assigned_subindices[VulkanQueueKind_Count] = {0};
	i32 queue_subindices[VulkanQueueKind_Count]    = {0};

	assigned_subindices[VulkanQueueKind_Graphics] += 1;

	if (queue_indices[VulkanQueueKind_Compute] == queue_indices[VulkanQueueKind_Graphics]) {
		if (assigned_subindices[VulkanQueueKind_Graphics] < queues[queue_indices[VulkanQueueKind_Graphics]].queueCount)
			queue_subindices[VulkanQueueKind_Compute] = assigned_subindices[VulkanQueueKind_Graphics]++;
	} else {
		assigned_subindices[VulkanQueueKind_Compute] += 1;
	}

	if (queue_indices[VulkanQueueKind_Transfer] == queue_indices[VulkanQueueKind_Graphics]) {
		if (assigned_subindices[VulkanQueueKind_Graphics] < queues[queue_indices[VulkanQueueKind_Graphics]].queueCount)
			queue_subindices[VulkanQueueKind_Transfer] = assigned_subindices[VulkanQueueKind_Graphics]++;
	} else if (queue_indices[VulkanQueueKind_Transfer] == queue_indices[VulkanQueueKind_Compute]) {
		if (assigned_subindices[VulkanQueueKind_Compute] < queues[queue_indices[VulkanQueueKind_Compute]].queueCount)
			queue_subindices[VulkanQueueKind_Transfer] = assigned_subindices[VulkanQueueKind_Compute]++;
	} else {
		assigned_subindices[VulkanQueueKind_Transfer] += 1;
	}

	for EachElement(assigned_subindices, it)
		vk->unique_queues += assigned_subindices[it];

	end_temp_arena(arena_save);

	/////////////////////////////////////////////
	// NOTE(rnp): fill in info and create device
	for EachElement(vk->queues, it) {
		u32 index = queue_subindices[it];
		for (i32 i = 0; i < queue_indices[it]; i++)
			index += assigned_subindices[i];
		vk->queue_indices[it] = index;
	}

	for EachElement(vk->queues, it) {
		if (vk->queues[vk->queue_indices[it]] == 0) {
			vk->queues[vk->queue_indices[it]] = push_struct(memory, VulkanQueue);
			vk->queues[vk->queue_indices[it]]->queue_family = queue_indices[it];
			vk->queues[vk->queue_indices[it]]->queue_index  = queue_subindices[it];
		}
		vk->queues[it] = vk->queues[vk->queue_indices[it]];
	}

	VkDeviceQueueCreateInfo queue_create_infos[VulkanQueueKind_Count];

	f32 queue_priorities[VulkanQueueKind_Count][VulkanQueueKind_Count];
	for (u32 i = 0; i < VulkanQueueKind_Count; i++)
		for (u32 j = 0; j < VulkanQueueKind_Count; j++)
			queue_priorities[i][j] = 1.0f;
	queue_priorities[queue_indices[VulkanQueueKind_Compute]][queue_subindices[VulkanQueueKind_Compute]] = 0.5f;

	u32 queue_create_index = 0;
	b32 queue_info_filled[VulkanQueueKind_Count] = {0};
	for (u32 q = 0; q < vk->unique_queues; q++) {
		u32 base_q = queue_indices[q];
		if (!queue_info_filled[base_q]) {
			queue_create_infos[queue_create_index++] = (VkDeviceQueueCreateInfo){
				.sType            = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
				.queueFamilyIndex = base_q,
				.queueCount       = assigned_subindices[q],
				.pQueuePriorities = queue_priorities[q],
			};
		}
		queue_info_filled[base_q] = 1;
	}

	VkPhysicalDeviceVulkan13Features v13f = {
		.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES,
		#define X(name, ...) .name = 1,
		VK_REQUIRED_PHYSICAL_13_FEATURES
		#undef X
	};

	VkPhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR pdsre = {
		.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_RELAXED_EXTENDED_INSTRUCTION_FEATURES_KHR,
		.shaderRelaxedExtendedInstruction = 1,
	};
	if (vulkan_debug.shader_relaxed_extended_instruction) v13f.pNext = &pdsre;

	VkPhysicalDeviceVulkan12Features v12f = {
		.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES,
		.pNext = &v13f,
		#define X(name, ...) .name = 1,
		VK_REQUIRED_PHYSICAL_12_FEATURES
		#undef X
	};

	VkPhysicalDeviceVulkan11Features v11f = {
		.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES,
		.pNext = &v12f,
		#define X(name, ...) .name = 1,
		VK_REQUIRED_PHYSICAL_11_FEATURES
		#undef X
	};
	VkPhysicalDeviceFeatures2 device_features = {
		.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2,
		.pNext = &v11f,
		.features = {
			#define X(name, ...) .name = 1,
			VK_REQUIRED_PHYSICAL_FEATURES
			#undef X
		},
	};

	Arena arena = *memory;
	u32   enabled_count = countof(vk_required_device_extensions) + countof(vk_debug_extensions);
	const char **enabled_extensions = push_array(&arena, const char *, enabled_count);

	enabled_count = 0;
	for EachElement(vk_required_device_extensions, it)
		enabled_extensions[enabled_count++] = vk_required_device_extensions[it];

	for EachElement(vk_debug_extensions, it)
		if (vulkan_debug.E[it])
			enabled_extensions[enabled_count++] = vk_debug_extensions[it];

	VkDeviceCreateInfo device_create_info = {
		.sType                   = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
		.pNext                   = &device_features,
		.pQueueCreateInfos       = queue_create_infos,
		.queueCreateInfoCount    = queue_create_index,
		.ppEnabledExtensionNames = enabled_extensions,
		.enabledExtensionCount   = enabled_count,
	};
	vkCreateDevice(vk->physical_device, &device_create_info, 0, &vk->device);

	#define X(name, ...) name = (name##_fn *)vkGetDeviceProcAddr(vk->device, #name);
	VkDeviceProcedureList
	#undef X

	for (u32 q = 0; q < vk->unique_queues; q++) {
		VulkanQueue *qp = vk->queues[q];
		vkGetDeviceQueue(vk->device, qp->queue_family, qp->queue_index, &qp->queue);

		VkCommandPoolCreateInfo command_pool_create_info = {
			.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
			.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
			.queueFamilyIndex = qp->queue_family,
		};
		vkCreateCommandPool(vk->device, &command_pool_create_info, 0, &qp->command_pool);

		VkCommandBufferAllocateInfo command_buffer_allocate_info = {
			.sType              = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
			.commandPool        = qp->command_pool,
			.level              = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
			.commandBufferCount = countof(qp->command_buffers),
		};
		vkAllocateCommandBuffers(vk->device, &command_buffer_allocate_info, qp->command_buffers);

		qp->timeline_semaphore = vk_make_semaphore(0);

		VkQueryPoolCreateInfo query_pool_create_info = {
			.sType      = VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO,
			.queryType  = VK_QUERY_TYPE_TIMESTAMP,
			.queryCount = countof(qp->command_buffers) * MaxCommandBufferTimestamps,
		};
		vkCreateQueryPool(vk->device, &query_pool_create_info, 0, &qp->query_pool);
	}

	vk->queues[VulkanQueueKind_Graphics]->pipeline_stage_flags |= VK_PIPELINE_STAGE_2_ALL_GRAPHICS_BIT;
	vk->queues[VulkanQueueKind_Compute]->pipeline_stage_flags  |= VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT;
}

function void
vk_load_graphics(void)
{
	VulkanContext *vk = vulkan_context;

	// NOTE: swap chain image format
	{
	}

	// NOTE: depth/stencil format
	{
		VkFormat depth_formats[] = {
			VK_FORMAT_D32_SFLOAT_S8_UINT,
			VK_FORMAT_D24_UNORM_S8_UINT,
			VK_FORMAT_D16_UNORM_S8_UINT,
		};

		vk->depth_stencil_format = VK_FORMAT_UNDEFINED;
		for EachElement(depth_formats, it) {
			VkFormatProperties3 format_properties3 = {.sType = VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_3};
			VkFormatProperties2 format_properties2 = {
				.sType = VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2,
				.pNext = &format_properties3,
			};
			vkGetPhysicalDeviceFormatProperties2(vk->physical_device, depth_formats[it], &format_properties2);
			if (format_properties3.optimalTilingFeatures & VK_FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT) {
				vk->depth_stencil_format = depth_formats[it];
				break;
			}
		}
	}
}

///////////////////////
// NOTE(rnp): User API

DEBUG_IMPORT void
vk_load(OSLibrary vulkan_library_handle, Arena *memory, Stream *err)
{
	#define X(name, ...) name = (name##_fn *)os_lookup_symbol(vulkan_library_handle, #name);
	VkLoaderProcedureList
	#undef X

	if (!vkGetInstanceProcAddr) {
		stream_append_s8(err, vulkan_info("fatal error: failed to find \"vkGetInstanceProcAddr\"\n"));
		fatal(stream_to_s8(err));
	}

	VulkanContext *vk = vulkan_context;
	vk->entity_arena = sub_arena_end(memory, KB(64), KB(4));
	vk->arena        = sub_arena_end(memory, KB(96), KB(4));

	vk_load_instance();
	vk_load_physical_device(vulkan_context->arena, err);
	vk_load_queues(&vulkan_context->arena, err);
	vk_load_graphics();

	read_only local_persist s8 default_compute_shader = s8(""
		"#version 430 core\n"
		"layout(push_constant) uniform pc { uint data[256 / 4]; };\n"
		"void main() {}\n"
		"\n");
	vk->default_compute_pipeline = vk_compute_pipeline_from_shader_text(vk->arena, default_compute_shader,
	                                                                    s8("error_compute_shader"), 256);

	read_only local_persist s8 default_vertex_shader = s8(""
		"#version 430 core\n"
		"layout(push_constant) uniform pc { uint data[256 / 4]; };\n"
		"void main() {gl_Position = vec4(0);}\n"
		"\n");
	read_only local_persist s8 default_fragment_shader = s8(""
		"#version 430 core\n"
		"layout(location = 0) out vec4 out_colour;"
		"layout(push_constant) uniform pc { uint data[256 / 4]; };\n"
		"void main() {out_colour = vec4(0.5f, 0.0f, 0.5f, 1.0f);}\n"
		"\n");

	VulkanPipelineCreateInfo pipeline_create_infos[2] = {
		{
			.kind = VulkanShaderKind_Vertex,
			.text = default_vertex_shader,
			.name = s8("error_vertex_shader"),
		},
		{
			.kind = VulkanShaderKind_Fragment,
			.text = default_fragment_shader,
			.name = s8("error_fragment_shader"),
		},
	};
	vk->default_graphics_pipeline = vk_graphics_pipeline_from_infos(vk->arena, pipeline_create_infos, 2, 256);

	// TODO: setup ui render pipeline

	if (err->widx > 0) {
		os_console_log(err->data, err->widx);
		stream_reset(err, 0);
	}
}

DEBUG_IMPORT GPUInfo *
vk_gpu_info(void)
{
	return &vulkan_context->gpu_info;
}

function void
vk_vulkan_buffer_release(VulkanBuffer *vb, u64 size)
{
	VulkanContext *vk = vulkan_context;
	VulkanEntity  *e  = (VulkanEntity *)((u8 *)vb - offsetof(VulkanEntity, as));
	// TODO(rnp): this happens implicitly, probably just delete this if block
	if (vb->host_pointer)
		vkUnmapMemory(vk->device, vb->memory);

	if (vb->buffer)
		vkDestroyBuffer(vk->device, vb->buffer, 0);

	vk_release_memory(vb->memory, vb->memory_kind != VulkanMemoryKind_Host ? size : 0);
	vk_entity_release(e);
}

DEBUG_IMPORT void
vk_buffer_release(GPUBuffer *b)
{
	if ValidVulkanHandle(b->buffer)
		vk_vulkan_buffer_release(vk_entity_data(b->buffer, VulkanEntityKind_Buffer), b->size);
	zero_struct(b);
}

DEBUG_IMPORT void
vk_buffer_allocate(GPUBuffer *b, GPUBufferAllocateInfo *info)
{
	VulkanContext *vk = vulkan_context;

	vk_buffer_release(b);

	assert(info->size > 0);

	VulkanEntity *e = vk_entity_allocate(VulkanEntityKind_Buffer);
	VulkanBufferAllocateInfo vulkan_buffer_allocate_info = {
		.gpu_buffer = b,
		.size       = (u64)info->size,
		.flags      = info->flags,
		.index_type = VK_INDEX_TYPE_NONE_KHR,
		.label      = info->label,
	};

	u32 queue_index_hit_count[VulkanQueueKind_Count] = {0};
	for (u32 it = 0; it < info->timeline_count; it++)
		queue_index_hit_count[vk->queue_indices[info->timelines_used[it]]]++;

	for EachElement(queue_index_hit_count, it) {
		if (queue_index_hit_count[it] > 0) {
			u32 index = vulkan_buffer_allocate_info.queue_family_count++;
			vulkan_buffer_allocate_info.queue_family_indices[index] = vk->queues[vk->queue_indices[it]]->queue_family;
		}
	}

	if (vk_buffer_allocate_common(&e->as.buffer, &vulkan_buffer_allocate_info)) {
		b->buffer.value[0] = (u64)e;
	} else {
		vk_entity_release(e);
	}
}

DEBUG_IMPORT b32
vk_buffer_needs_sync(GPUBuffer *b)
{
	b32 result = 0;
	if ValidVulkanHandle(b->buffer) {
		VulkanBuffer *vb = vk_entity_data(b->buffer, VulkanEntityKind_Buffer);

		// TODO(rnp): not correct check. need to check if we used transfer queue
		result = vb->memory_kind != VulkanMemoryKind_BAR;
	}

	return result;
}

DEBUG_IMPORT u64
vk_round_up_to_sync_size(u64 size, u64 min)
{
	iz  round  = (iz)Max(min, vulkan_context->memory_info.non_coherent_atom_size);
	u64 result = (u64)round_up_to((iz)size, round);
	return result;
}

function force_inline void
vk_buffer_buffer_copy(VulkanBuffer *destination, VulkanBuffer *source, u64 destination_offset, u64 source_offset, u64 size, b32 non_temporal)
{
	VulkanContext *vk = vulkan_context;

	switch (source->memory_kind) {
	case VulkanMemoryKind_BAR:
	{
		switch (destination->memory_kind) {
		case VulkanMemoryKind_Host:{
			if (destination->memory) {
				// TODO(rnp): there is likely a more efficient way of doing this in this case
				InvalidCodePath;
			} else {
				assert(source->host_pointer);
				b32 coherent = vk->memory_info.memory_host_coherent[source->memory_kind];
				if (!coherent) {
					u64 nca_size = vk->memory_info.non_coherent_atom_size;
					VkMappedMemoryRange mrs[1] = {{
						.sType  = VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE,
						.memory = source->memory,
						.offset = source_offset - (source_offset % nca_size),
						.size   = vk_round_up_to_sync_size(size, nca_size),
					}};
					vkInvalidateMappedMemoryRanges(vk->device, countof(mrs), mrs);
				}

				void *dest = (u8 *)destination->host_pointer + destination_offset;
				void *src  = (u8 *)source->host_pointer + source_offset;

				// NOTE(rnp): don't trash the CPU cache for large data stores
				if (non_temporal) memory_copy_non_temporal(dest, src, size);
				else              mem_copy(dest, src, size);
			}
		}break;
		InvalidDefaultCase;
		}
	}break;

	case VulkanMemoryKind_Host:{
		switch (destination->memory_kind) {
		case VulkanMemoryKind_BAR:{
			assert(destination->host_pointer);

			void *dest = (u8 *)destination->host_pointer + destination_offset;
			void *src  = (u8 *)source->host_pointer + source_offset;

			// NOTE(rnp): don't trash the CPU cache for large data stores
			if (non_temporal) memory_copy_non_temporal(dest, src, size);
			else              mem_copy(dest, src, size);

			b32 coherent = vk->memory_info.memory_host_coherent[destination->memory_kind];
			if (!coherent) {
				u64 nca_size = vk->memory_info.non_coherent_atom_size;
				VkMappedMemoryRange mrs[1] = {{
					.sType  = VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE,
					.memory = destination->memory,
					.offset = destination_offset - (destination_offset % nca_size),
					.size   = vk_round_up_to_sync_size(size, nca_size),
				}};
				vkFlushMappedMemoryRanges(vk->device, countof(mrs), mrs);
			}
		}break;
		InvalidDefaultCase;

		}
	}break;

	// TODO(rnp): use transfer queue when not mapped
	InvalidDefaultCase;
	}
}

DEBUG_IMPORT void
vk_buffer_range_upload(GPUBuffer *b, void *data, u64 offset, u64 size, b32 non_temporal)
{
	VulkanBuffer *db = vk_entity_data(b->buffer, VulkanEntityKind_Buffer);
	VulkanBuffer  sb = {
		.host_pointer = data,
		.memory_kind  = VulkanMemoryKind_Host,
	};
	vk_buffer_buffer_copy(db, &sb, offset, 0, size, non_temporal);
}

DEBUG_IMPORT void
vk_buffer_range_download(void *destination, GPUBuffer *source, u64 offset, u64 size, b32 non_temporal)
{
	VulkanBuffer *sb = vk_entity_data(source->buffer, VulkanEntityKind_Buffer);
	VulkanBuffer  db = {
		.host_pointer = destination,
		.memory_kind  = VulkanMemoryKind_Host,
	};
	vk_buffer_buffer_copy(&db, sb, 0, offset, size, non_temporal);
}

DEBUG_IMPORT void
vk_render_model_release(GPUBuffer *model)
{
	if ValidVulkanHandle(model->buffer)
		vk_vulkan_buffer_release(vk_entity_data(model->buffer, VulkanEntityKind_RenderModel), model->size);
	zero_struct(model);
}

DEBUG_IMPORT void
vk_render_model_allocate(GPUBuffer *model, void *indices, u64 index_count, u64 model_size, s8 label)
{
	vk_render_model_release(model);

	VulkanEntity *e = vk_entity_allocate(VulkanEntityKind_RenderModel);

	assert(index_count <= U32_MAX);
	VkIndexType index_type;
	if (index_count <= U16_MAX) index_type = VK_INDEX_TYPE_UINT16;
	else                        index_type = VK_INDEX_TYPE_UINT32;

	i64 indices_size = round_up_to(vk_index_size(index_type) * index_count, 64);

	i64 size = round_up_to(model_size + indices_size, 64);
	assert(size > 0);

	VulkanBufferAllocateInfo vulkan_buffer_allocate_info = {
		.gpu_buffer              = model,
		.size                    = (u64)size,
		.flags                   = VulkanUsageFlag_HostReadWrite,
		.index_type              = index_type,
		.label                   = label,
		.queue_family_count      = 1,
		.queue_family_indices[0] = vulkan_context->queues[VulkanQueueKind_Graphics]->queue_family,
	};
	if (vk_buffer_allocate_common(&e->as.buffer, &vulkan_buffer_allocate_info)) {
		model->buffer.value[0] = (u64)e;
		model->index_count  = index_count;
		model->gpu_pointer += indices_size;

		VulkanBuffer  sb = {
			.host_pointer = indices,
			.memory_kind  = VulkanMemoryKind_Host,
		};

		vk_buffer_buffer_copy(&e->as.buffer, &sb, 0, 0, vk_index_size(index_type) * index_count, 0);
	} else {
		vk_entity_release(e);
	}
}

DEBUG_IMPORT void
vk_render_model_range_upload(GPUBuffer *model, void *data, u64 offset, u64 size, b32 non_temporal)
{
	VulkanBuffer *db = vk_entity_data(model->buffer, VulkanEntityKind_RenderModel);
	VulkanBuffer  sb = {
		.host_pointer = data,
		.memory_kind  = VulkanMemoryKind_Host,
	};

	offset += round_up_to(vk_index_size(db->index_type) * model->index_count, 64);

	vk_buffer_buffer_copy(db, &sb, offset, 0, size, non_temporal);
}

DEBUG_IMPORT void
vk_image_release(GPUImage *image)
{
	if ValidVulkanHandle(image->image) {
		VulkanContext *vk = vulkan_context;
		VulkanImage   *vi = vk_entity_data(image->image, VulkanEntityKind_Image);

		vkDestroyImageView(vk->device, vi->view, 0);
		vkDestroyImage(vk->device, vi->image, 0);
		vk_release_memory(vi->memory, image->memory_size);

		vk_entity_release((VulkanEntity *)image->image.value[0]);
	}
	zero_struct(image);
}

DEBUG_IMPORT void
vk_image_allocate(GPUImage *image, u32 width, u32 height, u32 mips, u32 samples,
                  VulkanImageUsage usage, VulkanUsageFlags flags, OSHandle *export)
{
	assert(IsPowerOfTwo(samples));

	vk_image_release(image);

	VulkanContext *vk = vulkan_context;
	VulkanEntity  *e  = vk_entity_allocate(VulkanEntityKind_Image);
	VulkanImage   *vi = &e->as.image;

	image->image.value[0] = (u64)e;
	image->width          = Min(width,   vk->gpu_info.max_image_dimension_2D);
	image->height         = Min(height,  vk->gpu_info.max_image_dimension_2D);
	image->mip_map_levels = Max(mips,    1);
	image->samples        = Min(samples, vk->gpu_info.max_msaa_samples);

	VkFormat usage_format_map[VulkanImageUsage_Count + 1] = {
		[VulkanImageUsage_None]         = VK_FORMAT_UNDEFINED,
		//[VulkanImageUsage_Colour]       = VK_FORMAT_R8G8B8A8_SRGB,
		[VulkanImageUsage_Colour]       = VK_FORMAT_R8G8B8A8_UNORM,
		[VulkanImageUsage_DepthStencil] = vk->depth_stencil_format,
		[VulkanImageUsage_Count]        = VK_FORMAT_UNDEFINED,
	};

	read_only local_persist VkImageUsageFlagBits usage_extra_bit_map[VulkanImageUsage_Count + 1] = {
		[VulkanImageUsage_None]         = 0,
		[VulkanImageUsage_Colour]       = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
		[VulkanImageUsage_DepthStencil] = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
		[VulkanImageUsage_Count]        = 0,
	};

	read_only local_persist VkImageAspectFlags usage_image_aspect_map[VulkanImageUsage_Count + 1] = {
		[VulkanImageUsage_None]         = 0,
		[VulkanImageUsage_Colour]       = VK_IMAGE_ASPECT_COLOR_BIT,
		[VulkanImageUsage_DepthStencil] = VK_IMAGE_ASPECT_DEPTH_BIT|VK_IMAGE_ASPECT_STENCIL_BIT,
		[VulkanImageUsage_Count]        = 0,
	};

	usage = Clamp((u32)usage, 0, VulkanImageUsage_Count);
	VkImageUsageFlagBits usage_flags = usage_extra_bit_map[usage];

	if (flags & VulkanUsageFlag_ImageSampling)       usage_flags |= VK_IMAGE_USAGE_SAMPLED_BIT;
	if (flags & VulkanUsageFlag_TransferSource)      usage_flags |= VK_IMAGE_USAGE_TRANSFER_SRC_BIT;
	if (flags & VulkanUsageFlag_TransferDestination) usage_flags |= VK_IMAGE_USAGE_TRANSFER_DST_BIT;

	u32 queue_family = vk->queues[VulkanQueueKind_Graphics]->queue_family;
	VkImageCreateInfo image_create_info = {
		.sType                 = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,
		.flags                 = export ? VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT : 0,
		.imageType             = VK_IMAGE_TYPE_2D,
		.format                = usage_format_map[usage],
		.extent                = {image->width, image->height, 1},
		.mipLevels             = image->mip_map_levels,
		.arrayLayers           = 1,
		.samples               = image->samples,
		.tiling                = VK_IMAGE_TILING_OPTIMAL,
		.usage                 = usage_flags,
		// NOTE(rnp): needed if multiple queue families are accessed
		.sharingMode           = VK_SHARING_MODE_EXCLUSIVE,
		.queueFamilyIndexCount = 1,
		.pQueueFamilyIndices   = &queue_family,
		.initialLayout         = VK_IMAGE_LAYOUT_UNDEFINED,
	};

	VkExternalMemoryImageCreateInfo external_memory_image_create_info = {
		.sType       = VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO,
		.handleTypes = OS_WINDOWS ? VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT
		                          : VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT,
	};

	if (export) image_create_info.pNext = &external_memory_image_create_info;

	vkCreateImage(vk->device, &image_create_info, 0, &vi->image);

	VkMemoryRequirements memory_requirements;
	vkGetImageMemoryRequirements(vk->device, vi->image, &memory_requirements);

	VkMemoryDedicatedAllocateInfo dedicated_allocate_info = {
		.sType  = VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO,
		.image  = vi->image,
	};

	if (vk_allocate_memory(&vi->memory, memory_requirements.size, VulkanMemoryKind_Device, 0, &dedicated_allocate_info, export)) {
		image->memory_size = memory_requirements.size;
		vkBindImageMemory(vk->device, vi->image, vi->memory, 0);

		VkImageViewCreateInfo image_view_info = {
			.sType      = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
			.image      = vi->image,
			.viewType   = VK_IMAGE_VIEW_TYPE_2D,
			.format     = usage_format_map[usage],
			.subresourceRange = {
				.aspectMask     = usage_image_aspect_map[usage],
				.baseMipLevel   = 0,
				.levelCount     = 1,
				.baseArrayLayer = 0,
				.layerCount     = 1,
			},
		};
		vkCreateImageView(vk->device, &image_view_info, 0, &vi->view);
	} else {
		vkDestroyImage(vk->device, vi->image, 0);
		vk_entity_release(e);
		zero_struct(image);
	}
}

DEBUG_IMPORT VulkanHandle
vk_create_semaphore(OSHandle *export)
{
	VulkanEntity *e = vk_entity_allocate(VulkanEntityKind_Semaphore);
	e->as.semaphore = vk_make_semaphore(export);
	VulkanHandle result = {(u64)e};
	return result;
}

DEBUG_IMPORT b32
vk_host_wait_timeline(VulkanTimeline timeline, u64 value, u64 timeout_ns)
{
	b32 result = 0;
	if Between(timeline, 0, VulkanTimeline_Count - 1) {
		VulkanContext *vk = vulkan_context;
		VulkanQueue   *vq = vk->queues[timeline];
		VkSemaphoreWaitInfo semaphore_wait_info = {
			.sType          = VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO,
			.pSemaphores    = &vq->timeline_semaphore.semaphore,
			.semaphoreCount = 1,
			.pValues        = &value,
		};
		result = vkWaitSemaphores(vk->device, &semaphore_wait_info, timeout_ns) == VK_SUCCESS;
	}
	return result;
}

DEBUG_IMPORT u64
vk_host_signal_timeline(VulkanTimeline timeline)
{
	u64 result = -1;
	if Between(timeline, 0, VulkanTimeline_Count - 1) {
		VulkanContext   *vk = vulkan_context;
		VulkanQueue     *vq = vk->queues[timeline];
		VulkanSemaphore *vs = &vq->timeline_semaphore;
		result = ++vs->value;
		VkSemaphoreSignalInfo ssi = {
			.sType     = VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO,
			.semaphore = vs->semaphore,
			.value     = result,
		};
		vkSignalSemaphore(vk->device, &ssi);
	}
	return result;
}

DEBUG_IMPORT VulkanHandle
vk_pipeline(VulkanPipelineCreateInfo *infos, u32 count, u32 push_constants_size)
{
	assert(Between(count, 1, 2));
	assert(count == 2 || infos[0].kind == VulkanShaderKind_Compute);

	VulkanHandle result = {0};
	DeferLoop(take_lock(&vulkan_context->arena_lock, -1), release_lock(&vulkan_context->arena_lock))
	{
		Arena arena = vulkan_context->arena;

		VulkanEntity *e = vk_entity_allocate(VulkanEntityKind_Pipeline);
		result = (VulkanHandle){(u64)e};

		if (count == 2) e->as.pipeline = vk_graphics_pipeline_from_infos(arena, infos, count, push_constants_size);
		else            e->as.pipeline = vk_compute_pipeline_from_shader_text(arena, infos[0].text, infos[0].name, push_constants_size);
	}
	return result;
}

DEBUG_IMPORT b32
vk_pipeline_valid(VulkanHandle h)
{
	b32 result = 0;
	if ValidVulkanHandle(h) {
		VulkanPipeline *vp = vk_entity_data(h, VulkanEntityKind_Pipeline);
		if (vp->stage_flags == VK_SHADER_STAGE_COMPUTE_BIT)
			result = vp->pipeline != vulkan_context->default_compute_pipeline.pipeline;
		else
			result = vp->pipeline != vulkan_context->default_graphics_pipeline.pipeline;
	}
	return result;
}

DEBUG_IMPORT void
vk_pipeline_release(VulkanHandle h)
{
	if (vk_pipeline_valid(h)) {
		VulkanEntity *e = (VulkanEntity *)h.value[0];
		VulkanTimeline timeline;
		if (e->as.pipeline.stage_flags == VK_SHADER_STAGE_COMPUTE_BIT) timeline = VulkanTimeline_Compute;
		else                                                           timeline = VulkanTimeline_Graphics;

		VulkanQueue  *vq = vulkan_context->queues[timeline];
		DeferLoop(take_lock(&vq->lock, -1), release_lock(&vq->lock))
		{
			u32 index = (vq->next_command_buffer_index - 1) % countof(vq->command_buffers);
			vk_host_wait_timeline(timeline, vq->command_buffer_submission_values[index], -1ULL);

			if (&e->as.pipeline == vq->bound_pipeline)
				vq->bound_pipeline = 0;

			vkDestroyPipeline(vulkan_context->device, e->as.pipeline.pipeline, 0);
			vkDestroyPipelineLayout(vulkan_context->device, e->as.pipeline.layout, 0);
		}
		vk_entity_release(e);
	}
}

DEBUG_IMPORT VulkanHandle
vk_command_begin(VulkanTimeline timeline)
{
	VulkanHandle result = {0};
	if Between(timeline, 0, VulkanTimeline_Count - 1) {
		VulkanContext *vk = vulkan_context;
		VulkanQueue   *vq = vk->queues[timeline];

		take_lock(&vq->lock, -1);

		VulkanEntity        *e   = vk_entity_allocate(VulkanEntityKind_CommandBuffer);
		VulkanCommandBuffer *vcb = &e->as.command_buffer;
		u32 index = vq->next_command_buffer_index++ % countof(vq->command_buffers);
		vcb->kind                 = (VulkanQueueKind)timeline;
		vcb->command_buffer_index = index;

		// TODO(rnp): probably not the best to have this here but it will likely not be hit
		b32 wait_result = vk_host_wait_timeline(timeline, vq->command_buffer_submission_values[index], -1ULL);
		assert(wait_result);

		VkCommandBufferBeginInfo buffer_begin_info = {
			.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
			.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
		};

		vq->query_pool_occupied[index] = 0;

		vkBeginCommandBuffer(vq->command_buffers[index], &buffer_begin_info);
		vkCmdResetQueryPool(vq->command_buffers[index], vq->query_pool,
		                    index * MaxCommandBufferTimestamps, MaxCommandBufferTimestamps);

		result = (VulkanHandle){(u64)e};
	}
	return result;
}

DEBUG_IMPORT void
vk_command_bind_pipeline(VulkanHandle command, VulkanHandle pipeline)
{
	if ValidVulkanHandle(command) {
		VulkanContext       *vk  = vulkan_context;
		VulkanCommandBuffer *vcb = vk_entity_data(command, VulkanEntityKind_CommandBuffer);
		VulkanQueue         *vq  = vk->queues[vcb->kind];

		VulkanPipeline *vp = 0;
		if ValidVulkanHandle(pipeline) {
			vp = vk_entity_data(pipeline, VulkanEntityKind_Pipeline);
		} else if (vcb->kind == VulkanQueueKind_Compute) {
			vp = &vk->default_compute_pipeline;
		} else if (vcb->kind == VulkanQueueKind_Graphics) {
			vp = &vk->default_graphics_pipeline;
		} else {
			InvalidCodePath;
		}

		read_only local_persist VkPipelineBindPoint bind_point_lut[VulkanQueueKind_Count] = {
			[VulkanQueueKind_Graphics] = VK_PIPELINE_BIND_POINT_GRAPHICS,
			[VulkanQueueKind_Compute]  = VK_PIPELINE_BIND_POINT_COMPUTE,
			[VulkanQueueKind_Transfer] = -1,
		};

		VkPipelineBindPoint bind_point = bind_point_lut[vcb->kind];
		assert(bind_point != (VkPipelineBindPoint)-1);

		vkCmdBindPipeline(vq->command_buffers[vcb->command_buffer_index], bind_point, vp->pipeline);
		vq->bound_pipeline = vp;
	}
}

DEBUG_IMPORT void
vk_command_buffer_memory_barriers(VulkanHandle command, GPUMemoryBarrierInfo *barriers, u64 count)
{
	if ValidVulkanHandle(command) {
		VulkanContext       *vk  = vulkan_context;
		VulkanCommandBuffer *vcb = vk_entity_data(command, VulkanEntityKind_CommandBuffer);
		VulkanQueue         *vq  = vk->queues[vcb->kind];

		DeferLoop(take_lock(&vk->arena_lock, -1), release_lock(&vk->arena_lock))
		{
			Arena arena = vk->arena;
			u32 valid_count = 0;
			VkBufferMemoryBarrier2 *memory_barriers = push_array(&arena, VkBufferMemoryBarrier2, count);
			for (u64 it = 0; it < count; it++) {
				if ValidVulkanHandle(barriers[it].gpu_buffer->buffer) {
					u32           index = valid_count++;
					VulkanBuffer *vb    = vk_entity_data(barriers[it].gpu_buffer->buffer, VulkanEntityKind_Buffer);
					memory_barriers[index].sType               = VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2;
					memory_barriers[index].srcStageMask        = vq->pipeline_stage_flags;
					memory_barriers[index].srcAccessMask       = VK_ACCESS_2_MEMORY_WRITE_BIT;
					memory_barriers[index].dstStageMask        = vq->pipeline_stage_flags;
					memory_barriers[index].dstAccessMask       = VK_ACCESS_2_MEMORY_READ_BIT;
					memory_barriers[index].srcQueueFamilyIndex = vq->queue_family;
					memory_barriers[index].dstQueueFamilyIndex = vq->queue_family;
					memory_barriers[index].buffer              = vb->buffer;
					memory_barriers[index].offset              = barriers[it].offset;
					memory_barriers[index].size                = barriers[it].size;
				}
			}

			VkDependencyInfo dependancy_info = {
				.sType                    = VK_STRUCTURE_TYPE_DEPENDENCY_INFO,
				.bufferMemoryBarrierCount = valid_count,
				.pBufferMemoryBarriers    = memory_barriers,
			};

			vkCmdPipelineBarrier2(vq->command_buffers[vcb->command_buffer_index], &dependancy_info);
		}
	}
}

DEBUG_IMPORT void
vk_command_dispatch_compute(VulkanHandle command, uv3 dispatch)
{
	assert(dispatch.x <= U16_MAX);
	assert(dispatch.y <= U16_MAX);
	assert(dispatch.z <= U16_MAX);
	if ValidVulkanHandle(command) {
		VkCommandBuffer cmd = vk_command_buffer(command);
		vkCmdDispatch(cmd, dispatch.x, dispatch.y, dispatch.z);
	}
}

DEBUG_IMPORT void
vk_command_push_constants(VulkanHandle command, u32 offset, u32 size, void *values)
{
	if ValidVulkanHandle(command) {
		VulkanCommandBuffer *vcb = vk_entity_data(command, VulkanEntityKind_CommandBuffer);
		VulkanQueue         *vq  = vulkan_context->queues[vcb->kind];
		VulkanPipeline      *vp  = vq->bound_pipeline;

		assert(vp);

		vkCmdPushConstants(vq->command_buffers[vcb->command_buffer_index], vp->layout, vp->stage_flags,
		                   offset, size, values);
	}
}

DEBUG_IMPORT void
vk_command_timestamp(VulkanHandle command)
{
	if ValidVulkanHandle(command) {
		VulkanContext       *vk  = vulkan_context;
		VulkanCommandBuffer *vcb = vk_entity_data(command, VulkanEntityKind_CommandBuffer);
		VulkanQueue         *vq  = vk->queues[vcb->kind];

		read_only local_persist VkPipelineStageFlags2 stage_lut[VulkanQueueKind_Count] = {
			[VulkanQueueKind_Graphics] = VK_PIPELINE_STAGE_2_ALL_GRAPHICS_BIT,
			[VulkanQueueKind_Compute]  = VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT,
			[VulkanQueueKind_Transfer] = -1,
		};

		VkPipelineStageFlags2 stage = stage_lut[vcb->kind];
		assert(stage != (VkPipelineStageFlags2)-1);

		if (vq->query_pool_occupied[vcb->command_buffer_index] < MaxCommandBufferTimestamps) {
			u32 query_index = vq->query_pool_occupied[vcb->command_buffer_index]++;
			vkCmdWriteTimestamp2(vq->command_buffers[vcb->command_buffer_index], stage,
			                     vq->query_pool,
			                     vcb->command_buffer_index * MaxCommandBufferTimestamps + query_index);
		}
	}
}

DEBUG_IMPORT void
vk_command_wait_timeline(VulkanHandle command, VulkanTimeline timeline, u64 value)
{
	if (ValidVulkanHandle(command) && Between(timeline, 0, VulkanTimeline_Count - 1)) {
		VulkanContext       *vk  = vulkan_context;
		VulkanCommandBuffer *vcb = vk_entity_data(command, VulkanEntityKind_CommandBuffer);

		u32 wait_index = vk->queue_indices[timeline];
		vcb->in_flight_wait_values[wait_index] = Max(value, vcb->in_flight_wait_values[wait_index]);
	}
}

DEBUG_IMPORT u64
vk_command_end(VulkanHandle command, VulkanHandle wait_semaphore, VulkanHandle finished_semaphore)
{
	u64 result = -1;
	if ValidVulkanHandle(command) {
		VulkanContext       *vk  = vulkan_context;
		VulkanCommandBuffer *vcb = vk_entity_data(command, VulkanEntityKind_CommandBuffer);
		VulkanQueue         *vq  = vk->queues[vcb->kind];
		VulkanSemaphore     *vs  = &vq->timeline_semaphore;

		vkEndCommandBuffer(vq->command_buffers[vcb->command_buffer_index]);

		VkCommandBufferSubmitInfo command_buffer_submit_info = {
			.sType         = VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO,
			.commandBuffer = vq->command_buffers[vcb->command_buffer_index],
		};

		result = ++vs->value;

		u32 signal_submit_info_count = 1;
		VkSemaphoreSubmitInfo signal_submit_infos[2] = {{
			.sType     = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
			.semaphore = vs->semaphore,
			.value     = result,
			.stageMask = vq->pipeline_stage_flags,
		}};

		if ValidVulkanHandle(finished_semaphore) {
			VulkanSemaphore *fs = vk_entity_data(finished_semaphore, VulkanEntityKind_Semaphore);
			signal_submit_infos[signal_submit_info_count++] = (VkSemaphoreSubmitInfo){
				.sType     = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
				.semaphore = fs->semaphore,
				.stageMask = vq->pipeline_stage_flags,
			};
		}

		u32 wait_submit_info_count = 0;
		VkSemaphoreSubmitInfo wait_submit_infos[VulkanQueueKind_Count + 1];
		for (u32 i = 0; i < vk->unique_queues; i++) {
			u32 queue_index = vk->queue_indices[i];
			if (vcb->in_flight_wait_values[queue_index] > 0) {
				VulkanQueue *q = vk->queues[queue_index];
				VkSemaphoreSubmitInfo wait_ssi = {
					.sType     = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
					.semaphore = q->timeline_semaphore.semaphore,
					.value     = vcb->in_flight_wait_values[queue_index],
					.stageMask = q->pipeline_stage_flags,
				};
				wait_submit_infos[wait_submit_info_count++] = wait_ssi;
			}
		}

		if ValidVulkanHandle(wait_semaphore) {
			VulkanSemaphore *ws = vk_entity_data(wait_semaphore, VulkanEntityKind_Semaphore);
			wait_submit_infos[wait_submit_info_count++] = (VkSemaphoreSubmitInfo){
				.sType     = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
				.semaphore = ws->semaphore,
				.stageMask = vq->pipeline_stage_flags,
			};
		}

		VkSubmitInfo2 submit_info = {
			.sType                    = VK_STRUCTURE_TYPE_SUBMIT_INFO_2,
			.commandBufferInfoCount   = 1,
			.pCommandBufferInfos      = &command_buffer_submit_info,
			.waitSemaphoreInfoCount   = wait_submit_info_count,
			.pWaitSemaphoreInfos      = wait_submit_infos,
			.signalSemaphoreInfoCount = signal_submit_info_count,
			.pSignalSemaphoreInfos    = signal_submit_infos,
		};

		vkQueueSubmit2(vq->queue, 1, &submit_info, 0);

		vq->bound_pipeline = 0;

		atomic_store_u64(vq->command_buffer_submission_values + vcb->command_buffer_index, result);

		release_lock(&vq->lock);

		vk_entity_release((VulkanEntity *)command.value[0]);
	}
	return result;
}

DEBUG_IMPORT void
vk_command_begin_rendering(VulkanHandle command, GPUImage *colour, GPUImage *depth, GPUImage *resolve)
{
	if ValidVulkanHandle(command) {
		VkCommandBuffer cmd = vk_command_buffer(command);

		assert((colour->width == depth->width) && (colour->height == depth->height));

		VulkanImage *ci = vk_entity_data(colour->image, VulkanEntityKind_Image);
		VulkanImage *di = vk_entity_data(depth->image,  VulkanEntityKind_Image);
		VulkanImage *ri = 0;
		if (resolve) ri = vk_entity_data(resolve->image, VulkanEntityKind_Image);

		// NOTE: Layout Transitions
		{
			u32 image_memory_barrier_count = 2;
			VkImageMemoryBarrier2 image_memory_barriers[3] = {
				{
					.sType            = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2,
					.srcStageMask     = VK_PIPELINE_STAGE_2_TOP_OF_PIPE_BIT,
					.srcAccessMask    = 0,
					.dstStageMask     = VK_PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT,
					.dstAccessMask    = VK_ACCESS_2_COLOR_ATTACHMENT_READ_BIT|VK_ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT,
					.oldLayout        = VK_IMAGE_LAYOUT_UNDEFINED,
					.newLayout        = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
					.image            = ci->image,
					.subresourceRange = {
						.aspectMask     = VK_IMAGE_ASPECT_COLOR_BIT,
						.baseMipLevel   = 0,
						.levelCount     = 1,
						.baseArrayLayer = 0,
						.layerCount     = 1,
					},
				},
				{
					.sType            = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2,
					.srcStageMask     = VK_PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT|VK_PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT,
					.srcAccessMask    = 0,
					.dstStageMask     = VK_PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT|VK_PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT,
					.dstAccessMask    = VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT,
					.oldLayout        = VK_IMAGE_LAYOUT_UNDEFINED,
					.newLayout        = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
					.image            = di->image,
					.subresourceRange = {
						.aspectMask     = VK_IMAGE_ASPECT_DEPTH_BIT|VK_IMAGE_ASPECT_STENCIL_BIT,
						.baseMipLevel   = 0,
						.levelCount     = 1,
						.baseArrayLayer = 0,
						.layerCount     = 1,
					},
				},
			};

			if (resolve) image_memory_barriers[image_memory_barrier_count++] = (VkImageMemoryBarrier2){
				.sType            = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2,
				.srcStageMask     = VK_PIPELINE_STAGE_2_TOP_OF_PIPE_BIT,
				.srcAccessMask    = 0,
				.dstStageMask     = VK_PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT|VK_PIPELINE_STAGE_2_RESOLVE_BIT,
				.dstAccessMask    = VK_ACCESS_2_COLOR_ATTACHMENT_READ_BIT|VK_ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT,
				.oldLayout        = VK_IMAGE_LAYOUT_UNDEFINED,
				.newLayout        = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
				.image            = ri->image,
				.subresourceRange = {
					.aspectMask     = VK_IMAGE_ASPECT_COLOR_BIT,
					.baseMipLevel   = 0,
					.levelCount     = 1,
					.baseArrayLayer = 0,
					.layerCount     = 1,
				},
			};

			VkDependencyInfo dependency_info = {
				.sType                   = VK_STRUCTURE_TYPE_DEPENDENCY_INFO,
				.imageMemoryBarrierCount = image_memory_barrier_count,
				.pImageMemoryBarriers    = image_memory_barriers,
			};

			vkCmdPipelineBarrier2(cmd, &dependency_info);
		}

		VkRenderingAttachmentInfo colour_attachment = {
			.sType              = VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO,
			.imageView          = ci->view,
			.imageLayout        = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
			.resolveMode        = ri ? VK_RESOLVE_MODE_AVERAGE_BIT : 0,
			.resolveImageView   = ri ? ri->view : 0,
			.resolveImageLayout = ri ? VK_IMAGE_LAYOUT_GENERAL : 0,
			.loadOp             = VK_ATTACHMENT_LOAD_OP_CLEAR,
			.storeOp            = VK_ATTACHMENT_STORE_OP_STORE,
			.clearValue         = {.color = {{0.0f, 0.0f, 0.0f, 0.0f}}},
		};

		VkRenderingAttachmentInfo depth_stencil_attachment = {
			.sType       = VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO,
			.imageView   = di->view,
			.imageLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
			.loadOp      = VK_ATTACHMENT_LOAD_OP_CLEAR,
			.storeOp     = VK_ATTACHMENT_STORE_OP_STORE,
			.clearValue  = {.depthStencil = {1.0f, 0}},
		};

		VkRenderingInfo rendering_info = {
			.sType                = VK_STRUCTURE_TYPE_RENDERING_INFO,
			.renderArea           = {.offset = {0}, .extent = {colour->width, colour->height}},
			.layerCount           = 1,
			.colorAttachmentCount = 1,
			.pColorAttachments    = &colour_attachment,
			.pDepthAttachment     = &depth_stencil_attachment,
			.pStencilAttachment   = &depth_stencil_attachment,
		};

		vkCmdBeginRendering(cmd, &rendering_info);
	}
}

DEBUG_IMPORT void
vk_command_draw(VulkanHandle command, GPUBuffer *model)
{
	if (ValidVulkanHandle(command) && ValidVulkanHandle(model->buffer)) {
		VkCommandBuffer cmd = vk_command_buffer(command);
		VulkanBuffer   *vb  = vk_entity_data(model->buffer, VulkanEntityKind_RenderModel);
		vkCmdBindIndexBuffer2(cmd, vb->buffer, 0, vk_index_size(vb->index_type) * model->index_count, vb->index_type);
		vkCmdDrawIndexed(cmd, model->index_count, 1, 0, 0, 0);
	}
}

DEBUG_IMPORT void
vk_command_scissor(VulkanHandle command, u32 width, u32 height, u32 x_offset, u32 y_offset)
{
	if ValidVulkanHandle(command) {
		VkCommandBuffer cmd = vk_command_buffer(command);
		VkRect2D scissor = {.offset = {x_offset, y_offset}, .extent = {width, height}};
		vkCmdSetScissor(cmd, 0, 1, &scissor);
	}
}

DEBUG_IMPORT void
vk_command_viewport(VulkanHandle command, f32 width, f32 height, f32 x_offset, f32 y_offset, f32 min_depth, f32 max_depth)
{
	if ValidVulkanHandle(command) {
		VkCommandBuffer cmd = vk_command_buffer(command);
		VkViewport viewport = {x_offset, y_offset, width, height, min_depth, max_depth};
		vkCmdSetViewport(cmd, 0, 1, &viewport);
	}
}

DEBUG_IMPORT void
vk_command_end_rendering(VulkanHandle command)
{
	if ValidVulkanHandle(command) vkCmdEndRendering(vk_command_buffer(command));
}

DEBUG_IMPORT void
vk_command_copy_buffer(VulkanHandle command, GPUBuffer *restrict destination,
                       GPUBuffer *restrict source, u64 source_offset, i64 size)
{
	if (ValidVulkanHandle(command) && ValidVulkanHandle(destination->buffer) && ValidVulkanHandle(source->buffer)) {
		VkCommandBuffer cmd = vk_command_buffer(command);
		VulkanBuffer *db = vk_entity_data(destination->buffer, VulkanEntityKind_Buffer);
		VulkanBuffer *sb = vk_entity_data(source->buffer,      VulkanEntityKind_Buffer);

		VkBufferCopy2 buffer_copy = {
			.sType     = VK_STRUCTURE_TYPE_BUFFER_COPY_2,
			.srcOffset = source_offset,
			.dstOffset = 0,
			.size      = size,
		};

		VkCopyBufferInfo2 copy_buffer_info = {
			.sType       = VK_STRUCTURE_TYPE_COPY_BUFFER_INFO_2,
			.srcBuffer   = sb->buffer,
			.dstBuffer   = db->buffer,
			.regionCount = 1,
			.pRegions    = &buffer_copy,
		};

		vkCmdCopyBuffer2(cmd, &copy_buffer_info);
	}
}

DEBUG_IMPORT u64 *
vk_command_read_timestamps(VulkanTimeline timeline, Arena *arena)
{
	u64 *result = 0;
	if Between(timeline, 0, VulkanTimeline_Count - 1) {
		VulkanContext *vk = vulkan_context;
		VulkanQueue   *vq = vk->queues[timeline];
		DeferLoop(take_lock(&vq->lock, -1), release_lock(&vq->lock)) {
			u32 index = (vq->next_command_buffer_index - 1) % countof(vq->command_buffers);
			u32 count = vq->query_pool_occupied[index];
			if (count > 0) {
				result = push_array(arena, u64, count + 1);
				result[0] = count;

				vk_host_wait_timeline(timeline, vq->command_buffer_submission_values[index], -1ULL);

				vkGetQueryPoolResults(vk->device, vq->query_pool, index * MaxCommandBufferTimestamps, count,
				                      count * sizeof(u64), result + 1, 8, VK_QUERY_RESULT_WAIT_BIT);
			}
		}
	} else {
		result = push_array(arena, u64, 1);
	}
	return result;
}
