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
#define GL_DYNAMIC_STORAGE_BIT             0x0100
#define GL_SHADER_IMAGE_ACCESS_BARRIER_BIT 0x00000020
#define GL_TEXTURE_UPDATE_BARRIER_BIT      0x00000100
#define GL_SHADER_STORAGE_BARRIER_BIT      0x00002000

#define GL_UNSIGNED_INT_8_8_8_8            0x8035
#define GL_TEXTURE_3D                      0x806F
#define GL_MAX_3D_TEXTURE_SIZE             0x8073
#define GL_MULTISAMPLE                     0x809D
#define GL_CLAMP_TO_BORDER                 0x812D
#define GL_CLAMP_TO_EDGE                   0x812F
#define GL_DEPTH_COMPONENT24               0x81A6
#define GL_MAJOR_VERSION                   0x821B
#define GL_MINOR_VERSION                   0x821C
#define GL_RG                              0x8227
#define GL_RG32F                           0x8230
#define GL_R8I                             0x8231
#define GL_R16I                            0x8233
#define GL_DEBUG_OUTPUT_SYNCHRONOUS        0x8242
#define GL_BUFFER                          0x82E0
#define GL_PROGRAM                         0x82E2
#define GL_MIRRORED_REPEAT                 0x8370
#define GL_QUERY_RESULT                    0x8866
#define GL_READ_ONLY                       0x88B8
#define GL_WRITE_ONLY                      0x88B9
#define GL_READ_WRITE                      0x88BA
#define GL_TIME_ELAPSED                    0x88BF
#define GL_STATIC_DRAW                     0x88E4
#define GL_UNIFORM_BUFFER                  0x8A11
#define GL_MAX_UNIFORM_BLOCK_SIZE          0x8A30
#define GL_FRAGMENT_SHADER                 0x8B30
#define GL_VERTEX_SHADER                   0x8B31
#define GL_COMPILE_STATUS                  0x8B81
#define GL_LINK_STATUS                     0x8B82
#define GL_INFO_LOG_LENGTH                 0x8B84
#define GL_MAX_TEXTURE_BUFFER_SIZE         0x8C2B
#define GL_COLOR_ATTACHMENT0               0x8CE0
#define GL_DEPTH_ATTACHMENT                0x8D00
#define GL_FRAMEBUFFER                     0x8D40
#define GL_RENDERBUFFER                    0x8D41
#define GL_RED_INTEGER                     0x8D94
#define GL_TIMESTAMP                       0x8E28
#define GL_SHADER_STORAGE_BUFFER           0x90D2
#define GL_MAX_SHADER_STORAGE_BLOCK_SIZE   0x90DE
#define GL_MAX_SERVER_WAIT_TIMEOUT         0x9111
#define GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT 0x919F
#define GL_COMPUTE_SHADER                  0x91B9

typedef char      GLchar;
typedef ptrdiff_t GLsizeiptr;
typedef ptrdiff_t GLintptr;
typedef uint64_t  GLuint64;

/* X(name, ret, params) */
#define OGLProcedureList \
	X(glAttachShader,                        void,   (GLuint program, GLuint shader)) \
	X(glBeginQuery,                          void,   (GLenum target, GLuint id)) \
	X(glBindBufferBase,                      void,   (GLenum target, GLuint index, GLuint buffer)) \
	X(glBindBufferRange,                     void,   (GLenum target, GLuint index, GLuint buffer, GLintptr offset, GLsizeiptr size)) \
	X(glBindFramebuffer,                     void,   (GLenum target, GLuint framebuffer)) \
	X(glBindImageTexture,                    void,   (GLuint unit, GLuint texture, GLint level, GLboolean layered, GLint layer, GLenum access, GLenum format)) \
	X(glBindTextureUnit,                     void,   (GLuint unit, GLuint texture)) \
	X(glBindVertexArray,                     void,   (GLuint array)) \
	X(glBlitNamedFramebuffer,                void,   (GLuint sfb, GLuint dfb, GLint sx0, GLint sy0, GLint sx1, GLint sy1, GLint dx0, GLint dy0, GLint dx1, GLint dy1, GLbitfield mask, GLenum filter)) \
	X(glClearNamedFramebufferfv,             void,   (GLuint framebuffer, GLenum buffer, GLint drawbuffer, const GLfloat *value)) \
	X(glClearTexImage,                       void,   (GLuint texture, GLint level, GLenum format, GLenum type, const void *data)) \
	X(glCompileShader,                       void,   (GLuint shader)) \
	X(glCopyImageSubData,                    void,   (GLuint srcName, GLenum srcTarget, GLint srcLevel, GLint srcX, GLint srcY, GLint srcZ, GLuint dstName, GLenum dstTarget, GLint dstLevel, GLint dstX, GLint dstY, GLint dstZ, GLsizei srcWidth, GLsizei srcHeight, GLsizei srcDepth)) \
	X(glCreateBuffers,                       void,   (GLsizei n, GLuint *buffers)) \
	X(glCreateFramebuffers,                  void,   (GLsizei n, GLuint *ids)) \
	X(glCreateProgram,                       GLuint, (void)) \
	X(glCreateQueries,                       void,   (GLenum target, GLsizei n, GLuint *ids)) \
	X(glCreateRenderbuffers,                 void,   (GLsizei n, GLuint *renderbuffers)) \
	X(glCreateShader,                        GLuint, (GLenum shaderType)) \
	X(glCreateTextures,                      void,   (GLenum target, GLsizei n, GLuint *textures)) \
	X(glCreateVertexArrays,                  void,   (GLsizei n, GLuint *arrays)) \
	X(glDebugMessageCallback,                void,   (void (*)(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar *message, const void *user), void *user)) \
	X(glDeleteBuffers,                       void,   (GLsizei n, const GLuint *buffers)) \
	X(glDeleteProgram,                       void,   (GLuint program)) \
	X(glDeleteShader,                        void,   (GLuint shader)) \
	X(glDispatchCompute,                     void,   (GLuint num_groups_x, GLuint num_groups_y, GLuint num_groups_z)) \
	X(glEndQuery,                            void,   (GLenum target)) \
	X(glEnableVertexArrayAttrib,             void,   (GLuint vao, GLuint index)) \
	X(glGenerateTextureMipmap,               void,   (GLuint texture)) \
	X(glGetProgramInfoLog,                   void,   (GLuint program, GLsizei maxLength, GLsizei *length, GLchar *infoLog)) \
	X(glGetProgramiv,                        void,   (GLuint program, GLenum pname, GLint *params)) \
	X(glGetQueryObjectui64v,                 void,   (GLuint id, GLenum pname, GLuint64 *params)) \
	X(glGetShaderInfoLog,                    void,   (GLuint shader, GLsizei maxLength, GLsizei *length, GLchar *infoLog)) \
	X(glGetShaderiv,                         void,   (GLuint shader, GLenum pname, GLint *params)) \
	X(glGetTextureImage,                     void,   (GLuint texture, GLint level, GLenum format, GLenum type, GLsizei bufSize, void *pixels)) \
	X(glLinkProgram,                         void,   (GLuint program)) \
	X(glMemoryBarrier,                       void,   (GLbitfield barriers)) \
	X(glNamedBufferData,                     void,   (GLuint buffer, GLsizeiptr size, const void *data, GLenum usage)) \
	X(glNamedBufferStorage,                  void,   (GLuint buffer, GLsizeiptr size, const void *data, GLbitfield flags)) \
	X(glNamedBufferSubData,                  void,   (GLuint buffer, GLintptr offset, GLsizei size, const void *data)) \
	X(glNamedFramebufferRenderbuffer,        void,   (GLuint fb, GLenum attachment, GLenum renderbuffertarget, GLuint rb)) \
	X(glNamedFramebufferTexture,             void,   (GLuint fb, GLenum attachment, GLuint texture, GLint level)) \
	X(glNamedRenderbufferStorageMultisample, void,   (GLuint rb, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height)) \
	X(glObjectLabel,                         void,   (GLenum identifier, GLuint name, GLsizei length, const char *label)) \
	X(glProgramUniform1f,                    void,   (GLuint program, GLint location, GLfloat v0)) \
	X(glProgramUniform1i,                    void,   (GLuint program, GLint location, GLint v0)) \
	X(glProgramUniform1ui,                   void,   (GLuint program, GLint location, GLuint v0)) \
	X(glProgramUniform3iv,                   void,   (GLuint program, GLint location, GLsizei count, const GLint *value)) \
	X(glProgramUniform4fv,                   void,   (GLuint program, GLint location, GLsizei count, const GLfloat *value)) \
	X(glProgramUniformMatrix4fv,             void,   (GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)) \
	X(glQueryCounter,                        void,   (GLuint id, GLenum target)) \
	X(glShaderSource,                        void,   (GLuint shader, GLsizei count, const GLchar **strings, const GLint *lengths)) \
	X(glTextureParameteri,                   void,   (GLuint texture, GLenum pname, GLint param)) \
	X(glTextureParameterfv,                  void,   (GLuint texture, GLenum pname, const GLfloat *param)) \
	X(glTextureStorage1D,                    void,   (GLuint texture, GLsizei levels, GLenum internalformat, GLsizei width)) \
	X(glTextureStorage2D,                    void,   (GLuint texture, GLsizei levels, GLenum internalformat, GLsizei width, GLsizei height)) \
	X(glTextureStorage3D,                    void,   (GLuint texture, GLsizei levels, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth)) \
	X(glTextureSubImage1D,                   void,   (GLuint texture, GLint level, GLint xoff, GLsizei width, GLenum format, GLenum type, const void *pix)) \
	X(glTextureSubImage2D,                   void,   (GLuint texture, GLint level, GLint xoff, GLint yoff, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *pix)) \
	X(glTextureSubImage3D,                   void,   (GLuint texture, GLint level, GLint xoff, GLint yoff, GLint zoff, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const void *pix)) \
	X(glUseProgram,                          void,   (GLuint program)) \
	X(glVertexArrayAttribBinding,            void,   (GLuint vao, GLuint attribindex, GLuint bindingindex)) \
	X(glVertexArrayAttribFormat,             void,   (GLuint vao, GLuint attribindex, GLint size, GLenum type, GLboolean normalized, GLuint relativeoffset)) \
	X(glVertexArrayElementBuffer,            void,   (GLuint vao, GLuint buffer)) \
	X(glVertexArrayVertexBuffer,             void,   (GLuint vao, GLuint bindingindex, GLuint buffer, GLintptr offset, GLsizei stride))

#define X(name, ret, params) typedef ret name##_fn params;
OGLProcedureList
#undef X
#define X(name, ret, params) DEBUG_IMPORT name##_fn *name;
OGLProcedureList
#undef X

#endif /* _OPENGL_H_*/
