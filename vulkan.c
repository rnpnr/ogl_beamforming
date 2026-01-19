#include "beamformer_internal.h"
#include "vulkan.h"
#include "external/glslang/glslang/Include/glslang_c_interface.h"

#define glslang_info(s) s8("[glslang] " s)
#define vulkan_info(s)  s8("[vulkan]  " s)

#define ValidVulkanHandle(h) ((h).value[0] != 0)

#define MaxCommandBuffersInFlight  BeamformerMaxRawDataFramesInFlight
#define MaxCommandBufferTimestamps (64)

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
} VulkanBuffer;

typedef struct {
	VkPipeline         pipeline;
	VkPipelineLayout   layout;
	VkShaderStageFlags stage_flags;
} VulkanShader;

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
	VulkanEntityKind_Semaphore,
	VulkanEntityKind_Shader,
} VulkanEntityKind;

typedef struct VulkanEntity VulkanEntity;
struct VulkanEntity {
	VulkanEntity *   next;
	VulkanEntityKind kind;
	union {
		VulkanBuffer        buffer;
		VulkanCommandBuffer command_buffer;
		VulkanSemaphore     semaphore;
		VulkanShader        shader;
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
} VulkanQueue;
static_assert(alignof(VulkanQueue) == 64, "VulkanQueue must be placed on its own cacheline");

typedef struct {
	Arena             arena;
	i32               arena_lock;

	VkInstance        handle;
	VkDevice          device;
	VkPhysicalDevice  physical_device;

	// NOTE(rnp): fallback for when a compute shader fails to compile
	VulkanShader      default_compute_shader;

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

	VkSemaphore       timeline_semaphores[VulkanQueueKind_Count];

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

#define VK_REQUIRED_PHYSICAL_11_FEATURES \
	X(storageBuffer16BitAccess) \

#define VK_REQUIRED_PHYSICAL_12_FEATURES \
	X(bufferDeviceAddress) \
	X(storageBuffer8BitAccess) \
	X(shaderInt8) \
	X(shaderFloat16) \
	X(timelineSemaphore) \

#define VK_REQUIRED_PHYSICAL_13_FEATURES \
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
		#define X(_, name, ...) b32 name;
		VK_DEBUG_EXTENSIONS
		#undef X
	};
	b32 E[countof(vk_debug_extensions)];
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
	VkShaderModule result = 0;
	s8 spirv = glsl_to_spirv(&arena, vk_shader_kind_to_glslang_shader_kind(kind), text, name);
	VkShaderModuleCreateInfo create_info = {
		.sType    = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
		.codeSize = (uz)spirv.len,
		.pCode    = (u32 *)spirv.data,
	};
	if (spirv.len > 0) vkCreateShaderModule(vulkan_context->device, &create_info, 0, &result);

	return result;
}

function VulkanShader
vk_compute_pipeline_from_shader_text(Arena arena, s8 text, s8 name, u32 push_constants_size)
{
	VulkanShader result = {.stage_flags = VK_SHADER_STAGE_COMPUTE_BIT};
	VkShaderModule module = vk_compile_shader_module(arena, VK_SHADER_STAGE_COMPUTE_BIT, text, name);
	if (module) {
		VkPushConstantRange pcr = {
			.stageFlags = VK_SHADER_STAGE_COMPUTE_BIT,
			.offset     = 0,
			.size       = push_constants_size,
		};

		VkPipelineLayoutCreateInfo pli = {
			.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
			.pushConstantRangeCount = push_constants_size ? 1    : 0,
			.pPushConstantRanges    = push_constants_size ? &pcr : 0,
		};

		vkCreatePipelineLayout(vulkan_context->device, &pli, 0, &result.layout);

		VkComputePipelineCreateInfo pi = {
			.sType  = VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
			.layout = result.layout,
			.stage  = {
				.sType  = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
				.stage  = VK_SHADER_STAGE_COMPUTE_BIT,
				.module = module,
				.pName  = "main",
			},
		};

		vkCreateComputePipelines(vulkan_context->device, 0, 1, &pi, 0, &result.pipeline);
		vkDestroyShaderModule(vulkan_context->device, module, 0);
	}

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
		"VK_LAYER_KHRONOS_validation",
	};

	VkInstanceCreateInfo instance_create_info = {
		.sType                   = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
		.pApplicationInfo        = &app_info,
		.ppEnabledExtensionNames = vk_required_instance_extensions,
		.enabledExtensionCount   = countof(vk_required_instance_extensions),
		.ppEnabledLayerNames     = validation_layers,
		.enabledLayerCount       = countof(validation_layers),
	};

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

		vk->queue_indices[it]        = index;
		vk->queues[it]               = push_struct(memory, VulkanQueue);
		vk->queues[it]->queue_family = queue_indices[it];
		vk->queues[it]->queue_index  = queue_subindices[it];
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

		VkCommandPoolCreateInfo cpc = {
			.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
			.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
			.queueFamilyIndex = qp->queue_family,
		};
		vkCreateCommandPool(vk->device, &cpc, 0, &qp->command_pool);

		VkCommandBufferAllocateInfo cba = {
			.sType              = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
			.commandPool        = qp->command_pool,
			.level              = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
			.commandBufferCount = countof(qp->command_buffers),
		};
		vkAllocateCommandBuffers(vk->device, &cba, qp->command_buffers);

		qp->timeline_semaphore = vk_make_semaphore(0);

		VkQueryPoolCreateInfo qpc = {
			.sType      = VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO,
			.queryType  = VK_QUERY_TYPE_TIMESTAMP,
			.queryCount = countof(qp->command_buffers) * MaxCommandBufferTimestamps,
		};
		vkCreateQueryPool(vk->device, &qpc, 0, &qp->query_pool);
	}

	vk->queues[VulkanQueueKind_Graphics]->pipeline_stage_flags |= VK_PIPELINE_STAGE_2_ALL_GRAPHICS_BIT;
	vk->queues[VulkanQueueKind_Compute]->pipeline_stage_flags  |= VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT;
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

	// TODO: setup compute pipeline
	read_only local_persist s8 default_compute_shader = s8(""
		"#version 430 core\n"
		"void main() {}\n"
		"\n");

	vk->default_compute_shader = vk_compute_pipeline_from_shader_text(vk->arena, default_compute_shader,
	                                                                  s8("error_compute_shader"), 0);

	// TODO: setup render pipeline

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

DEBUG_IMPORT void
vk_buffer_release(GPUBuffer *b)
{
	VulkanContext *vk = vulkan_context;
	if ValidVulkanHandle(b->buffer) {
		VulkanBuffer *vb = vk_entity_data(b->buffer, VulkanEntityKind_Buffer);
		// TODO(rnp): this happens implicitly, probably just delete this if block
		if (vb->host_pointer)
			vkUnmapMemory(vk->device, vb->memory);

		if (vb->buffer)
			vkDestroyBuffer(vk->device, vb->buffer, 0);

		vkFreeMemory(vk->device, vb->memory, 0);
		if (vb->memory_kind != VulkanMemoryKind_Host)
			vk->gpu_info.gpu_heap_used -= b->size;

		vk_entity_release((VulkanEntity *)b->buffer.value[0]);
	}
	zero_struct(b);
}

DEBUG_IMPORT void
vk_buffer_allocate(GPUBuffer *b, iz size, GPUBufferCreateFlags flags, OSHandle *export, s8 label)
{
	vk_buffer_release(b);
	VulkanContext *vk = vulkan_context;
	VulkanEntity  *e  = vk_entity_allocate(VulkanEntityKind_Buffer);
	VulkanBuffer  *vb = &e->as.buffer;

	b->buffer.value[0] = (u64)e;

	assert(size > 0);

	// TODO(rnp): this probably should be handled, its usually 4GB. likely
	// need to chain multiple allocations and handle it in shader code
	size = (iz)Min((u64)size, vk->memory_info.max_allocation_size & ~vk->memory_info.non_coherent_atom_size);

	if ((flags & GPUBufferCreateFlag_MemoryOnly) == 0) {
		VkBufferCreateInfo bci = {
			.sType       = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
			.usage       = VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT,
			.size        = size,
			// TODO(rnp): this matters if we are using multiple queue families only and
			// only if we are actually using the transfer queue. If we are just using
			// BAR access it doesn't matter at all.
			.sharingMode = VK_SHARING_MODE_EXCLUSIVE,
		};

		vkCreateBuffer(vk->device, &bci, 0, &vb->buffer);

		VkMemoryRequirements mr;
		vkGetBufferMemoryRequirements(vk->device, vb->buffer, &mr);

		assert((u64)size <= mr.size);
		size = mr.size;
	}

	VkExportMemoryAllocateInfo ei = {
		.sType       = VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO,
		.handleTypes = OS_WINDOWS ? VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT
		                          : VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT,
	};

	VkMemoryAllocateFlagsInfo mafi = {
		.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO,
		.flags = VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT,
		.pNext = (export) ? & ei: 0,
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
	vb->memory_kind = flags & GPUBufferCreateFlag_HostWritable ? VulkanMemoryKind_BAR : VulkanMemoryKind_Device;
	VkMemoryAllocateInfo mai = {
		.sType           = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
		.allocationSize  = size,
		.memoryTypeIndex = vk->memory_info.memory_type_indices[vb->memory_kind],
		.pNext           = &mafi,
	};

	// TODO(rnp): this may fail if the allocation is too big for the BAR size
	// it needs to handled properly
	if (vkAllocateMemory(vk->device, &mai, 0, &vb->memory) == VK_SUCCESS) {
		vk->gpu_info.gpu_heap_used += mai.allocationSize;
		b->size = mai.allocationSize;

		if (flags & GPUBufferCreateFlag_HostWritable)
			vkMapMemory(vk->device, vb->memory, 0, b->size, 0, &vb->host_pointer);

		if ((flags & GPUBufferCreateFlag_MemoryOnly) == 0) {
			vkBindBufferMemory(vk->device, vb->buffer, vb->memory, 0);
			VkBufferDeviceAddressInfo bda = {
				.sType  = VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO,
				.buffer = vb->buffer,
			};
			b->gpu_pointer = vkGetBufferDeviceAddress(vk->device, &bda);
		}

		if (export) {
			if (OS_WINDOWS) {
				VkMemoryGetWin32HandleInfoKHR handle_info = {
					.sType      = VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR,
					.memory     = vb->memory,
					.handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT,
				};
				void *handle;
				vkGetMemoryWin32HandleKHR(vk->device, &handle_info, &handle);
				export->value[0] = (u64)handle;
			} else {
				VkMemoryGetFdInfoKHR fd_info = {
					.sType      = VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR,
					.memory     = vb->memory,
					.handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT,
				};
				i32 fd;
				vkGetMemoryFdKHR(vk->device, &fd_info, &fd);
				export->value[0] = (u64)fd;
			}
		}
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
vk_buffer_range_upload(GPUBuffer *b, void *data, u64 offset, u64 size, b32 non_temporal)
{
	VulkanBuffer *db = vk_entity_data(b->buffer, VulkanEntityKind_Buffer);
	VulkanBuffer  sb = {
		.host_pointer = data,
		.memory_kind  = VulkanMemoryKind_Host,
	};
	vk_buffer_buffer_copy(db, &sb, offset, 0, size, non_temporal);
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
		VkSemaphoreWaitInfo swi = {
			.sType          = VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO,
			.pSemaphores    = &vq->timeline_semaphore.semaphore,
			.semaphoreCount = 1,
			.pValues        = &value,
		};
		result = vkWaitSemaphores(vk->device, &swi, timeout_ns) == VK_SUCCESS;
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

DEBUG_IMPORT u64
vk_timeline_value(VulkanTimeline timeline)
{
	u64 result = -1;
	if Between(timeline, 0, VulkanTimeline_Count - 1)
		result = vulkan_context->queues[timeline]->timeline_semaphore.value;
	return result;
}

DEBUG_IMPORT VulkanHandle
vk_compute_shader(s8 text, s8 name, u32 push_constants_size)
{
	VulkanHandle result = {0};
	DeferLoop(take_lock(&vulkan_context->arena_lock, -1), release_lock(&vulkan_context->arena_lock))
	{
		Arena arena = vulkan_context->arena;

		VulkanEntity *e = vk_entity_allocate(VulkanEntityKind_Shader);
		result = (VulkanHandle){(u64)e};

		e->as.shader = vk_compute_pipeline_from_shader_text(arena, text, name, push_constants_size);
		if (e->as.shader.pipeline == 0) e->as.shader = vulkan_context->default_compute_shader;
	}
	return result;
}

DEBUG_IMPORT b32
vk_valid_compute_shader(VulkanHandle h)
{
	b32 result = 0;
	if ValidVulkanHandle(h) {
		VulkanShader *vs = vk_entity_data(h, VulkanEntityKind_Shader);
		result = vs->pipeline != vulkan_context->default_compute_shader.pipeline;
	}
	return result;
}

DEBUG_IMPORT void
vk_compute_shader_release(VulkanHandle h)
{
	if (vk_valid_compute_shader(h)) {
		VulkanQueue  *vq = vulkan_context->queues[VulkanTimeline_Compute];
		VulkanEntity *e  = (VulkanEntity *)h.value[0];
		DeferLoop(take_lock(&vq->lock, -1), release_lock(&vq->lock))
		{
			u32 index = (vq->next_command_buffer_index - 1) % countof(vq->command_buffers);
			vk_host_wait_timeline(VulkanTimeline_Compute, vq->command_buffer_submission_values[index], -1ULL);
			vkDestroyPipeline(vulkan_context->device, e->as.shader.pipeline, 0);
			vkDestroyPipelineLayout(vulkan_context->device, e->as.shader.layout, 0);
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
		vk_host_wait_timeline(timeline, vq->command_buffer_submission_values[index], -1);

		VkCommandBufferBeginInfo bbi = {
			.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
			.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
		};

		vq->query_pool_occupied[index] = 0;

		vkBeginCommandBuffer(vq->command_buffers[index], &bbi);
		vkCmdResetQueryPool(vq->command_buffers[index], vq->query_pool,
		                    index * MaxCommandBufferTimestamps, MaxCommandBufferTimestamps);

		result = (VulkanHandle){(u64)e};
	}
	return result;
}

DEBUG_IMPORT void
vk_command_bind_shader(VulkanHandle command, VulkanHandle shader)
{
	if ValidHandle(command) {
		VulkanContext       *vk  = vulkan_context;
		VulkanCommandBuffer *vcb = vk_entity_data(command, VulkanEntityKind_CommandBuffer);
		VulkanQueue         *vq  = vk->queues[vcb->kind];

		VulkanShader *vs = 0;
		if ValidHandle(shader) {
			vs = vk_entity_data(shader, VulkanEntityKind_Shader);
		} else if (vcb->kind == VulkanQueueKind_Compute) {
			vs = &vk->default_compute_shader;
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

		vkCmdBindPipeline(vq->command_buffers[vcb->command_buffer_index], bind_point, vs->pipeline);
	}
}

DEBUG_IMPORT void
vk_command_buffer_memory_barrier(VulkanHandle command, GPUBuffer *b, u64 offset, u64 size)
{
	if (ValidVulkanHandle(command) && ValidVulkanHandle(b->buffer)) {
		VulkanContext       *vk  = vulkan_context;
		VulkanBuffer        *vb  = vk_entity_data(b->buffer, VulkanEntityKind_Buffer);
		VulkanCommandBuffer *vcb = vk_entity_data(command, VulkanEntityKind_CommandBuffer);
		VulkanQueue         *vq  = vk->queues[vcb->kind];

		VkBufferMemoryBarrier2 mb = {
			.sType               = VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2,
			.srcStageMask        = vq->pipeline_stage_flags,
			.srcAccessMask       = VK_ACCESS_2_MEMORY_WRITE_BIT,
			.dstStageMask        = vq->pipeline_stage_flags,
			.dstAccessMask       = VK_ACCESS_2_MEMORY_READ_BIT,
			.srcQueueFamilyIndex = vq->queue_family,
			.dstQueueFamilyIndex = vq->queue_family,
			.buffer              = vb->buffer,
			.offset              = offset,
			.size                = size,
		};

		VkDependencyInfo di = {
			.sType                    = VK_STRUCTURE_TYPE_DEPENDENCY_INFO,
			.bufferMemoryBarrierCount = 1,
			.pBufferMemoryBarriers    = &mb,
		};

		vkCmdPipelineBarrier2(vq->command_buffers[vcb->command_buffer_index], &di);
	}
}

DEBUG_IMPORT void
vk_command_dispatch_compute(VulkanHandle command, uv3 dispatch)
{
	assert(dispatch.x <= U16_MAX);
	assert(dispatch.y <= U16_MAX);
	assert(dispatch.z <= U16_MAX);
	if ValidHandle(command) {
		VkCommandBuffer cmd = vk_command_buffer(command);
		vkCmdDispatch(cmd, dispatch.x, dispatch.y, dispatch.z);
	}
}

DEBUG_IMPORT void
vk_command_push_constants(VulkanHandle command, VulkanHandle shader, u32 offset, u32 size, void *values)
{
	if (ValidHandle(command) && ValidHandle(shader)) {
		VkCommandBuffer cmd = vk_command_buffer(command);
		VulkanShader    *vs = vk_entity_data(shader, VulkanEntityKind_Shader);
		vkCmdPushConstants(cmd, vs->layout, vs->stage_flags, offset, size, values);
	}
}

DEBUG_IMPORT void
vk_command_timestamp(VulkanHandle command)
{
	if ValidHandle(command) {
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
	if (ValidHandle(command) && Between(timeline, 0, VulkanTimeline_Count - 1)) {
		VulkanContext       *vk  = vulkan_context;
		VulkanCommandBuffer *vcb = vk_entity_data(command, VulkanEntityKind_CommandBuffer);

		u32 wait_index = vk->queue_indices[timeline];
		vcb->in_flight_wait_values[wait_index] = Max(value, vcb->in_flight_wait_values[wait_index]);
	}
}

DEBUG_IMPORT u64
vk_command_end(VulkanHandle command)
{
	u64 result = -1;
	if ValidVulkanHandle(command) {
		VulkanContext       *vk  = vulkan_context;
		VulkanCommandBuffer *vcb = vk_entity_data(command, VulkanEntityKind_CommandBuffer);
		VulkanQueue         *vq  = vk->queues[vcb->kind];
		VulkanSemaphore     *vs  = &vq->timeline_semaphore;

		vkEndCommandBuffer(vq->command_buffers[vcb->command_buffer_index]);

		VkCommandBufferSubmitInfo cbsi = {
			.sType         = VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO,
			.commandBuffer = vq->command_buffers[vcb->command_buffer_index],
		};

		result = ++vs->value;

		VkSemaphoreSubmitInfo ssi = {
			.sType     = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
			.semaphore = vs->semaphore,
			.value     = result,
			.stageMask = vq->pipeline_stage_flags,
		};
		(void)ssi;

		u32 wait_ssi_count = 0;
		VkSemaphoreSubmitInfo wait_ssis[VulkanQueueKind_Count];
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
				wait_ssis[wait_ssi_count++] = wait_ssi;
			}
		}

		VkSubmitInfo2 si = {
			.sType                    = VK_STRUCTURE_TYPE_SUBMIT_INFO_2,
			.commandBufferInfoCount   = 1,
			.pCommandBufferInfos      = &cbsi,
			.waitSemaphoreInfoCount   = wait_ssi_count,
			.pWaitSemaphoreInfos      = wait_ssis,
			.signalSemaphoreInfoCount = 1,
			.pSignalSemaphoreInfos    = &ssi,
		};

		vkQueueSubmit2(vq->queue, 1, &si, 0);

		atomic_store_u64(vq->command_buffer_submission_values + vcb->command_buffer_index, result);

		release_lock(&vq->lock);

		vk_entity_release((VulkanEntity *)command.value[0]);
	}
	return result;
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

				vkGetQueryPoolResults(vk->device, vq->query_pool, index * MaxCommandBufferTimestamps, count,
				                      count * sizeof(u64), result + 1, 8, VK_QUERY_RESULT_WAIT_BIT);
			}
		}
	} else {
		result = push_array(arena, u64, 1);
	}
	return result;
}
