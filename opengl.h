/* See LICENSE for license details. */
#ifndef _OPENGL_H_
#define _OPENGL_H_

#if OS_WINDOWS
/* NOTE: msys2 compatibility kludge */
#define WINGDIAPI
#define APIENTRY
#endif

#include <GL/gl.h>

/* NOTE: do not add extra 0s to these, even at the start -> garbage compilers will complain */
#define GL_SHADER_IMAGE_ACCESS_BARRIER_BIT 0x00000020
#define GL_TEXTURE_UPDATE_BARRIER_BIT      0x00000100

#define GL_NONE                            0

#define GL_CLAMP_TO_BORDER                 0x812D
#define GL_RG32F                           0x8230
#define GL_READ_ONLY                       0x88B8
#define GL_WRITE_ONLY                      0x88B9
#define GL_READ_WRITE                      0x88BA
#define GL_DEBUG_OUTPUT                    0x92E0

#define GL_DEDICATED_MEMORY_OBJECT_EXT     0x9581
#define GL_HANDLE_TYPE_OPAQUE_FD_EXT       0x9586
#define GL_HANDLE_TYPE_OPAQUE_WIN32_EXT    0x9587
#define GL_LAYOUT_COLOR_ATTACHMENT_EXT     0x958E
#define GL_LAYOUT_SHADER_READ_ONLY_EXT     0x9591

typedef char GLchar;
typedef i64  GLsizeiptr;
typedef i64  GLintptr;
typedef u64  GLuint64;

/* X(name, ret, params) */
#define OGLProcedureList \
	X(glBindImageTexture,                    void,   (GLuint unit, GLuint texture, GLint level, GLboolean layered, GLint layer, GLenum access, GLenum format)) \
	X(glClearNamedFramebufferfv,             void,   (GLuint framebuffer, GLenum buffer, GLint drawbuffer, const GLfloat *value)) \
	X(glClearTexImage,                       void,   (GLuint texture, GLint level, GLenum format, GLenum type, const void *data)) \
	X(glCreateTextures,                      void,   (GLenum target, GLsizei n, GLuint *textures)) \
	X(glDebugMessageCallback,                void,   (void (*)(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar *message, const void *user), void *user)) \
	X(glDispatchCompute,                     void,   (GLuint num_groups_x, GLuint num_groups_y, GLuint num_groups_z)) \
	X(glMemoryBarrier,                       void,   (GLbitfield barriers)) \
	X(glObjectLabel,                         void,   (GLenum identifier, GLuint name, GLsizei length, const char *label)) \
	X(glTextureParameteri,                   void,   (GLuint texture, GLenum pname, GLint param)) \
	X(glTextureParameterfv,                  void,   (GLuint texture, GLenum pname, const GLfloat *param)) \

#define OGLRequiredExtensionProcedureListBase \
	X(glCreateMemoryObjectsEXT,              void,   (GLsizei n, GLuint *memoryObjects)) \
	X(glDeleteMemoryObjectsEXT,              void,   (GLsizei n, GLuint *memoryObjects)) \
	X(glGenSemaphoresEXT,                    void,   (GLsizei n, GLuint *semaphores)) \
	X(glMemoryObjectParameterivEXT,          void,   (GLuint memoryObject, GLenum pname, const GLint *params)) \
	X(glSignalSemaphoreEXT,                  void,   (GLuint semaphore, GLuint numBufferBarriers, const GLuint *buffers, GLuint numTextureBarriers, const GLuint *textures, const GLenum *dstLayouts)) \
	X(glTextureStorageMem2DEXT,              void,   (GLuint texture, GLsizei levels, GLenum internalFormat, GLsizei width, GLsizei height, GLuint memory, GLuint64 offset)) \
	X(glWaitSemaphoreEXT,                    void,   (GLuint semaphore, GLuint numBufferBarriers, const GLuint *buffers, GLuint numTextureBarriers, const GLuint *textures, const GLenum *srcLayouts)) \

#define OGLRequiredExtensionProcedureListW32 \
	X(glImportMemoryWin32HandleEXT,          void,   (GLuint memory, GLuint64 size, GLenum handleType, void *handle)) \
	X(glImportSemaphoreWin32HandleEXT,       void,   (GLuint semaphore, GLenum handleType, void *handle)) \

#define OGLRequiredExtensionProcedureListLinux \
	X(glImportMemoryFdEXT,                   void,   (GLuint memory, GLuint64 size, GLenum handleType, int fd)) \
	X(glImportSemaphoreFdEXT,                void,   (GLuint semaphore, GLenum handleType, int fd)) \

#define OGLRequiredExtensionProcedureList \
	OGLRequiredExtensionProcedureListBase \
	OGLRequiredExtensionProcedureListW32 \
	OGLRequiredExtensionProcedureListLinux \

#define X(name, ret, params) typedef ret name##_fn params;
OGLProcedureList
OGLRequiredExtensionProcedureList
#undef X
#define X(name, ret, params) DEBUG_IMPORT name##_fn *name;
OGLProcedureList
OGLRequiredExtensionProcedureList
#undef X

#endif /* _OPENGL_H_*/
