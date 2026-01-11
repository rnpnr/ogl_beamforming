/* See LICENSE for license details. */
#ifndef COMPILER_H
#define COMPILER_H

#if defined(__linux__)
  #define OS_LINUX   1
#elif defined(_WIN32)
  #define OS_WINDOWS 1
#else
  #error Unsupported Operating System
#endif

#if   defined(__clang__)
  #define COMPILER_CLANG 1
#elif defined(_MSC_VER)
  #define COMPILER_MSVC  1
#elif defined(__GNUC__)
  #define COMPILER_GCC   1
#else
  #error Unsupported Compiler
#endif

#if COMPILER_MSVC
  #if defined(_M_AMD64)
    #define ARCH_X64   1
  #elif defined(_M_ARM64)
    #define ARCH_ARM64 1
  #else
    #error Unsupported Architecture
  #endif
#else
  #if defined(__x86_64__)
    #define ARCH_X64   1
  #elif defined(__aarch64__)
    #define ARCH_ARM64 1
  #else
    #error Unsupported Architecture
  #endif
#endif

#if !defined(OS_WINDOWS)
  #define OS_WINDOWS 0
#endif
#if !defined(OS_LINUX)
  #define OS_LINUX   0
#endif
#if !defined(COMPILER_CLANG)
  #define COMPILER_CLANG 0
#endif
#if !defined(COMPILER_MSVC)
  #define COMPILER_MSVC  0
#endif
#if !defined(COMPILER_GCC)
  #define COMPILER_GCC   0
#endif
#if !defined(ARCH_X64)
  #define ARCH_X64   0
#endif
#if !defined(ARCH_ARM64)
  #define ARCH_ARM64 0
#endif

/* NOTE: glibc devs are actually buffoons who never write any real code. even the most
 * basic headers like stdint.h will pull in every other header in libc and most
 * useful functions are hidden behind this dogshit macro gaurd */
#if OS_LINUX
  #ifndef _GNU_SOURCE
    #define _GNU_SOURCE
  #endif
#endif

#endif /* COMPILER_H */
