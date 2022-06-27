/*
 *  npt -- ANSI Common Lisp Programming Language.
 *    https://github.com/nptcl/npt
 *    https://github.com/nptcl/npt-amalgamation
 *
 *  File: lisp.h
 */
#ifndef __LISP_HEADER__
#define __LISP_HEADER__

#define LISP_AMALGAMATION
#ifdef __cplusplus
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif
#endif

#include <float.h>
#include <inttypes.h>
#include <limits.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>


/************************************************************
 *  define.h
 ************************************************************/
#ifndef __LISP_DEFINE_HEADER__
#define __LISP_DEFINE_HEADER__

/*
 *  autoconf
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#undef PACKAGE
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_URL
#undef PACKAGE_VERSION
#undef VERSION
#endif


/*
 *  define.h
 */
#ifdef __cplusplus
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif
#endif


/*
 *  Force disable THREAD
 */
#undef LISP_THREAD


/*
 *  Mode
 */
#if defined(LISP_MODE_CORE)
#undef LISP_MODE_STANDALONE
#undef LISP_MODE_DEGRADE
#define LISP_MODE_STRING "core"

#elif defined(LISP_MODE_STANDALONE)
#undef LISP_MODE_CORE
#undef LISP_MODE_DEGRADE
#define LISP_MODE_STRING "standalone"

#elif defined(LISP_MODE_DEGRADE)
#undef LISP_MODE_CORE
#undef LISP_MODE_STANDALONE
#define LISP_MODE_STRING "degrade"

#else
#undef LISP_MODE_CORE
#define LISP_MODE_STANDALONE
#undef LISP_MODE_DEGRADE
#define LISP_MODE_STRING "standalone"
#endif


/*
 *  Version
 */
#ifndef LISP_REVISION
#define LISP_REVISION "0"
#endif

#ifndef LISP_VERSION_A
#define LISP_VERSION_A 0
#endif
#ifndef LISP_VERSION_B
#define LISP_VERSION_B 0
#endif
#ifndef LISP_VERSION_C
#define LISP_VERSION_C 0
#endif


/*
 *  Pointer size
 *    LISP_ARCH_64BIT
 *    LISP_ARCH_32BIT
 */
#undef LISP_ARCH_64BIT
#undef LISP_ARCH_32BIT

#if (SIZEOF_VOID_P == 8)
#define LISP_ARCH_MODE "64bit"
#define LISP_ARCH_64BIT
#elif (SIZEOF_VOID_P == 4)
#define LISP_ARCH_MODE "32bit"
#define LISP_ARCH_32BIT
#elif (0xFFFFFFFFUL < SIZE_MAX)
#define LISP_ARCH_MODE "64bit"
#define LISP_ARCH_64BIT
#else
#define LISP_ARCH_MODE "32bit"
#define LISP_ARCH_32BIT
#endif


/*
 *  Select architecture
 */
#if defined(LISP_ANSIC)
/* ANSI C */
#define LISP_MODE "ANSI-C"
#undef LISP_FREEBSD
#undef LISP_LINUX
#undef LISP_UNIX
#undef LISP_WINDOWS
#ifdef LISP_THREAD
#error Arch error, LISP_ANSIC do not allow LISP_THREAD.
#endif
#define LISP_THREAD_REMOVE

#elif defined(LISP_FREEBSD)
/* FreeBSD */
#define LISP_MODE "FreeBSD"
#define LISP_UNIX
#undef LISP_ANSIC
#undef LISP_LINUX
#undef LISP_WINDOWS
#undef LISP_ANSIC_WINDOWS
#ifdef LISP_THREAD
#define LISP_THREAD_FREEBSD
#endif

#elif defined(LISP_LINUX)
/* Linux */
#define LISP_MODE "Linux"
#define LISP_UNIX
#undef LISP_ANSIC
#undef LISP_FREEBSD
#undef LISP_WINDOWS
#undef LISP_ANSIC_WINDOWS
#ifdef LISP_THREAD
#define LISP_THREAD_LINUX
#endif

#elif defined(LISP_WINDOWS)
/* Windows */
#define LISP_MODE "Windows"
#undef LISP_ANSIC
#undef LISP_FREEBSD
#undef LISP_LINUX
#undef LISP_UNIX
#undef LISP_ANSIC_WINDOWS
#ifdef LISP_THREAD
#define LISP_THREAD_WINDOWS
#endif

#else
/* ANSI C [default] */
#define LISP_MODE "ANSI-C"
#define LISP_ANSIC
#undef LISP_FREEBSD
#undef LISP_LINUX
#undef LISP_UNIX
#undef LISP_WINDOWS
#undef LISP_ANSIC_WINDOWS
#ifdef LISP_THREAD
#error Arch error
#endif
#define LISP_THREAD_REMOVE
#endif

#if defined(LISP_ANSIC) && defined(LISP_ANSIC_WINDOWS)
#define LISP_MODE "ANSI-C [Windows]"
#endif


/* Windows */
#if defined(LISP_WINDOWS) || defined(LISP_ANSIC_WINDOWS)
#define LISP_WINDOWS_OR_ANSI
#else
#undef LISP_WINDOWS_OR_ANSI
#endif

#ifdef LISP_WINDOWS_OR_ANSI
#if defined(LISP_CONSOLE)
#undef LISP_WINMAIN
#elif defined(LISP_WINMAIN)
#undef LISP_CONSOLE
#else
#define LISP_CONSOLE
#undef LISP_WINMAIN
#endif

#if defined(LISP_CYGWIN)
#undef LISP_NO_CYGWIN
#elif defined(LISP_NO_CYGWIN)
#undef LISP_CYGWIN
#elif defined(__CYGWIN__)
#define LISP_CYGWIN
#undef LISP_NO_CYGWIN
#else
#undef LISP_CYGWIN
#define LISP_NO_CYGWIN
#endif

#else
#undef LISP_CONSOLE
#undef LISP_WINMAIN
#undef LISP_CYGWIN
#undef LISP_NO_CYGWIN
#endif

#if defined(LISP_WINMAIN) && defined(LISP_CYGWIN)
#error Platform cygwin must use a main function (not WinMain).
#endif

#if defined(LISP_WINDOWS_OR_ANSI) || defined(LISP_NO_CYGWIN)
#define LISP_WINDOWS_WIDE
#else
#undef LISP_WINDOWS_WIDE
#endif


/*
 *  Lisp mode
 *    LISP_64BIT  [default]
 *    LISP_32BIT
 */
#if defined(LISP_64BIT)
#undef LISP_32BIT
#elif defined(LISP_32BIT)
#undef LISP_64BIT
#else
#if defined(LISP_ARCH_64BIT)
#define LISP_64BIT
#else
#define LISP_32BIT
#endif
#endif

#ifdef LISP_64BIT
#define LISP_FIXNUM_MODE "64bit"
#else
#define LISP_FIXNUM_MODE "32bit"
#endif

#if defined(LISP_64BIT) && defined(LISP_ARCH_32BIT)
#error arch error.
#endif


/*
 *  Thread mode
 *    LISP_THREAD_SINGLE      -> add LISP_THREAD_DISABLE  [default]
 *    LISP_THREAD_REMOVE      -> add LISP_THREAD_DISABLE
 *    LISP_THREAD_FREEBSD     -> add LISP_THREAD_UNIX
 *    LISP_THREAD_LINUX       -> add LISP_THREAD_UNIX
 *    LISP_THREAD_WINDOWS     -> use Vista module
 */
#undef LISP_THREAD_ENABLE
#undef LISP_THREAD_DISABLE

/* single thread */
#if defined(LISP_THREAD_SINGLE)
#define LISP_THREAD_MODE     "single"
#define LISP_THREAD_DISABLE
#undef LISP_THREAD_REMOVE
#undef LISP_THREAD_UNIX
#undef LISP_THREAD_FREEBSD
#undef LISP_THREAD_LINUX
#undef LISP_THREAD_WINDOWS

/* single thread [remove] */
#elif defined(LISP_THREAD_REMOVE)
#define LISP_THREAD_MODE     "remove"
#define LISP_THREAD_DISABLE
#undef LISP_THREAD_SINGLE
#undef LISP_THREAD_UNIX
#undef LISP_THREAD_FREEBSD
#undef LISP_THREAD_LINUX
#undef LISP_THREAD_WINDOWS

/* FreeBSD */
#elif defined(LISP_THREAD_FREEBSD)
#define LISP_THREAD_MODE     "FreeBSD"
#define LISP_THREAD_UNIX
#undef LISP_THREAD_SINGLE
#undef LISP_THREAD_REMOVE
#undef LISP_THREAD_LINUX
#undef LISP_THREAD_WINDOWS

/* FreeBSD */
#elif defined(LISP_THREAD_UNIX)
#define LISP_THREAD_MODE     "FreeBSD"
#define LISP_THREAD_FREEBSD
#undef LISP_THREAD_SINGLE
#undef LISP_THREAD_REMOVE
#undef LISP_THREAD_LINUX
#undef LISP_THREAD_WINDOWS

/* Linux */
#elif defined(LISP_THREAD_LINUX)
#define LISP_THREAD_MODE     "Linux"
#define LISP_THREAD_UNIX
#undef LISP_THREAD_SINGLE
#undef LISP_THREAD_REMOVE
#undef LISP_THREAD_FREEBSD
#undef LISP_THREAD_WINDOWS

/* Windows */
#elif defined(LISP_THREAD_WINDOWS)
#undef LISP_THREAD_SINGLE
#undef LISP_THREAD_REMOVE
#undef LISP_THREAD_UNIX
#undef LISP_THREAD_FREEBSD
#undef LISP_THREAD_LINUX
#define LISP_THREAD_MODE     "Windows"

/* single thread */
#else
#define LISP_THREAD_MODE     "remove"
#define LISP_THREAD_DISABLE
#define LISP_THREAD_REMOVE
#undef LISP_THREAD_SINGLE
#undef LISP_THREAD_UNIX
#undef LISP_THREAD_FREEBSD
#undef LISP_THREAD_LINUX
#undef LISP_THREAD_WINDOWS
#endif

/* thread enable */
#ifndef LISP_THREAD_DISABLE
#define LISP_THREAD_ENABLE
#endif

/* mode string */
#ifdef LISP_DEBUG
#define LISP_DEBUG_STRING "debug"
#else
#define LISP_DEBUG_STRING "release"
#endif

#ifdef LISP_DEGRADE
#define LISP_DEGRADE_STRING "degrade"
#else
#define LISP_DEGRADE_STRING "release"
#endif

/* memory size */
#ifndef LISP_MEMORY_HEAP
#ifdef LISP_DEBUG
#define LISP_MEMORY_HEAP	(64UL * 1024UL * 1024UL);
#else
#define LISP_MEMORY_HEAP	(1024UL * 1024UL * 1024UL);
#endif
#endif

#ifndef LISP_MEMORY_LOCAL
#ifdef LISP_DEBUG
#define LISP_MEMORY_LOCAL	(16UL * 1024UL * 1024UL);
#else
#define LISP_MEMORY_LOCAL	(256UL * 1024UL * 1024UL);
#endif
#endif

/* memory management */
#if defined(LISP_MEMORY_INIT)
#undef LISP_MEMORY_UNINIT
#elif defined(LISP_MEMORY_UNINIT)
#undef LISP_MEMORY_INIT
#elif defined(LISP_DEBUG)
#define LISP_MEMORY_INIT
#undef LISP_MEMORY_UNINIT
#else
#undef LISP_MEMORY_INIT
#define LISP_MEMORY_UNINIT
#endif

/* garbage collection */
#ifdef LISP_DEBUG
#ifndef LISP_DEBUG_MEMORY
#define LISP_DEBUG_MEMORY
#endif
#endif
/* #define LISP_DEBUG_FORCE_GC 1 */

/* stream */
#ifndef LISP_STREAM_EXTEND
#define LISP_STREAM_EXTEND	3
#endif

/* pointer_table */
#ifndef LISP_POINTER_EXTEND
#define LISP_POINTER_EXTEND	32
#endif

/* long float */
#if defined(LISP_FLOAT_LONG_64)
#define LISP_FLOAT_LONG			64
#undef LISP_FLOAT_LONG_80
#undef LISP_FLOAT_LONG_128
#elif defined(LISP_FLOAT_LONG_80)
#define LISP_FLOAT_LONG			80
#undef LISP_FLOAT_LONG_64
#undef LISP_FLOAT_LONG_128
#elif defined(LISP_FLOAT_LONG_128)
#define LISP_FLOAT_LONG			128
#undef LISP_FLOAT_LONG_64
#undef LISP_FLOAT_LONG_80
#else
#if (LDBL_MANT_DIG == DBL_MANT_DIG)
/* Visual Studio
 *   long double == double
 */
#define LISP_FLOAT_LONG			64
#define LISP_FLOAT_LONG_64
#undef LISP_FLOAT_LONG_80
#undef LISP_FLOAT_LONG_128
#elif (LDBL_MANT_DIG == 64)
/* Intel x86
 *   long double (Intel 80bit) fraction: 63+0 bit (64bit)
 */
#define LISP_FLOAT_LONG			80
#undef LISP_FLOAT_LONG_64
#define LISP_FLOAT_LONG_80
#undef LISP_FLOAT_LONG_128
#else
/* IEEE745
 * long double (IEEE-754 binary128) fraction: 112+1 bit
 */
#define LISP_FLOAT_LONG			128
#undef LISP_FLOAT_LONG_64
#undef LISP_FLOAT_LONG_80
#define LISP_FLOAT_LONG_128
#endif
#endif

/* float (32bit) fraction: 23+1 bit */
#define LISP_FLOAT_SINGLE_FRACTION		FLT_MANT_DIG
/* double (64bit) fraction: 52+1 bit */
#define LISP_FLOAT_DOUBLE_FRACTION		DBL_MANT_DIG
/* long double */
#define LISP_FLOAT_LONG_FRACTION		LDBL_MANT_DIG

/* readline editline */
#if defined(LISP_TERME)
#define LISP_PROMPT_TERME
#undef LISP_EDITLINE
#undef LISP_READLINE
#undef LISP_STDIN

#elif defined(LISP_EDITLINE)
#define LISP_PROMPT_EDITLINE
#undef LISP_TERME
#undef LISP_READLINE
#undef LISP_STDIN

#elif defined(LISP_READLINE)
#define LISP_PROMPT_READLINE
#undef LISP_TERME
#undef LISP_EDITLINE
#undef LISP_STDIN

#elif defined(LISP_STDIN)
#define LISP_PROMPT_DISABLE
#undef LISP_TERME
#undef LISP_EDITLINE
#undef LISP_READLINE

#elif defined(LISP_FREEBSD) || defined(LISP_LINUX)
#define LISP_TERME
#define LISP_PROMPT_TERME
#undef LISP_EDITLINE
#undef LISP_READLINE
#undef LISP_STDIN

#else
#define LISP_PROMPT_DISABLE
#define LISP_STDIN
#undef LISP_TERME
#undef LISP_EDITLINE
#undef LISP_READLINE
#endif

#if defined(LISP_PROMPT_TERME)
#define LISP_PROMPT_STRING "terme"
#undef LISP_PROMPT_DISABLE
#undef LISP_PROMPT_READLINE
#undef LISP_PROMPT_EDITLINE

#elif defined(LISP_PROMPT_DISABLE)
#define LISP_PROMPT_STRING "stdin"
#undef LISP_PROMPT_TERME
#undef LISP_PROMPT_READLINE
#undef LISP_PROMPT_EDITLINE

#elif defined(LISP_PROMPT_READLINE)
#define LISP_PROMPT_STRING "readline"
#undef LISP_PROMPT_DISABLE
#undef LISP_PROMPT_TERME
#undef LISP_PROMPT_EDITLINE

#elif defined(LISP_PROMPT_EDITLINE)
#define LISP_PROMPT_STRING "editline"
#undef LISP_PROMPT_DISABLE
#undef LISP_PROMPT_TERME
#undef LISP_PROMPT_READLINE

#else
#define LISP_PROMPT_DISABLE
#define LISP_PROMPT_STRING "stdin"
#undef LISP_PROMPT_TERME
#undef LISP_PROMPT_READLINE
#undef LISP_PROMPT_EDITLINE
#endif

/* terme */
#ifdef LISP_TERME

/* code */
#if defined(LISP_FREEBSD) || defined(LISP_LINUX)
#define LISP_TERME_UNIX
#else
#undef LISP_TERME_UNIX
#endif

#ifdef LISP_WINDOWS
#define LISP_TERME_WINDOWS
#else
#undef LISP_TERME_WINDOWS
#endif

/* bright / dark */
#if defined(LISP_TERME_BRIGHT)
#undef LISP_TERME_DARK
#define LISP_TERME_COLOR1	"bright"
#elif defined(LISP_TERME_DARK)
#undef LISP_TERME_BRIGHT
#define LISP_TERME_COLOR1	"dark"
#else
#define LISP_TERME_BRIGHT
#define LISP_TERME_COLOR1	"bright"
#endif

/* monochrome / color */
#if defined(LISP_TERME_COLOR)
#undef LISP_TERME_MONOCHROME
#define LISP_TERME_COLOR2	"on"
#elif defined(LISP_TERME_MONOCHROME)
#undef LISP_TERME_COLOR
#define LISP_TERME_COLOR2	"off"
#else
#define LISP_TERME_COLOR
#define LISP_TERME_COLOR2	"on"
#endif
#endif

/* show-window [Windows] */
#ifdef LISP_TERME_WINDOWS
#if defined(LISP_TERME_DEFAULT_WINDOW)
#undef LISP_TERME_HIDE_WINDOW
#elif defined(LISP_TERME_HIDE_WINDOW)
#undef LISP_TERME_DEFAULT_WINDOW
#else
#define LISP_TERME_DEFAULT_WINDOW
#endif
#endif

/* Complex math library */
#undef LISP_COMPLEX_INACCURACY
/* Visual C++ */
#if defined(LISP_COMPLEX_WINDOWS)
#undef LISP_COMPLEX_CPLUSPLUS
#undef LISP_COMPLEX_C99
#define LISP_COMPLEX_INACCURACY
/* C++ */
#elif defined(LISP_COMPLEX_CPLUSPLUS)
#undef LISP_COMPLEX_WINDOWS
#undef LISP_COMPLEX_C99
#ifdef __clang__
#define LISP_COMPLEX_INACCURACY
#endif
/* c99 */
#elif defined(LISP_COMPLEX_C99)
#undef LISP_COMPLEX_WINDOWS
#undef LISP_COMPLEX_CPLUSPLUS
/* C++ */
#elif defined(__cplusplus)
#undef LISP_COMPLEX_WINDOWS
#define LISP_COMPLEX_CPLUSPLUS
#undef LISP_COMPLEX_C99
#ifdef __clang__
#define LISP_COMPLEX_INACCURACY
#endif
/* Visual C++ */
#elif defined(_MSC_VER)
#define LISP_COMPLEX_WINDOWS
#undef LISP_COMPLEX_CPLUSPLUS
#undef LISP_COMPLEX_C99
#define LISP_COMPLEX_INACCURACY
/* c99 */
#else
#undef LISP_COMPLEX_WINDOWS
#undef LISP_COMPLEX_CPLUSPLUS
#define LISP_COMPLEX_C99
#endif

/* complex-long */
#if defined(LISP_COMPLEX_LONG)
#undef LISP_COMPLEX_DOUBLE
#elif defined(LISP_COMPLEX_DOUBLE)
#undef LISP_COMPLEX_LONG
#elif defined(LISP_FREEBSD)
#define LISP_COMPLEX_DOUBLE
#elif defined(__FreeBSD__)
#define LISP_COMPLEX_DOUBLE
#else
#define LISP_COMPLEX_LONG
#endif

/* windows */
#ifdef _MSC_VER
#pragma warning(disable:4996)
#endif

/* main */
#if defined(LISP_WINMAIN) || defined(LISP_WINDOWS_WIDE)
#define LISP_WINMAIN_WIDE
#else
#undef LISP_WINMAIN_WIDE
#endif

/* setjmp */
#ifdef __cplusplus
#ifdef LISP_ABORT_SETJMP
#undef LISP_ABORT_EXCEPTION
#else
#undef LISP_ABORT_SETJMP
#undef LISP_ABORT_EXCEPTION
#define LISP_ABORT_EXCEPTION
#endif
#else
#undef LISP_ABORT_SETJMP
#undef LISP_ABORT_EXCEPTION
#define LISP_ABORT_SETJMP
#endif

/* dynamic-link */
#ifdef LISP_WINDOWS
#ifndef LISP_DYNAMIC_LINK
#define LISP_DYNAMIC_LINK
#endif
#endif

/* end of header file */
#endif


/************************************************************
 *  typedef_basic.h
 ************************************************************/
#ifndef __LISP_TYPEDEF_BASIC_HEADER__
#define __LISP_TYPEDEF_BASIC_HEADER__


typedef unsigned char byte;
typedef uint16_t byte16;
typedef uint32_t byte32;
typedef uint64_t byte64;
typedef float short_float;
typedef float single_float;
typedef double double_float;
typedef long double long_float;
typedef uint32_t unicode;
typedef byte *pbyte;
typedef byte *addr;

#endif


/************************************************************
 *  typedef_integer.h
 ************************************************************/
#ifndef __LISP_TYPEDEF_INTEGER_HEADER__
#define __LISP_TYPEDEF_INTEGER_HEADER__


#ifdef LISP_64BIT
#define LISP_INFO           "64bit-code"
#define LISP_INTEGER_BIT	64
#define LISP_INTEGER_MASK	UINT64_MAX
#define FIXNUM_MAX			INT64_MAX
#define FIXNUM_MIN			INT64_MIN
typedef int64_t fixnum;
typedef uint64_t fixed;
#define PRIdF PRId64
#define PRIuF PRIu64
#define PRIxF PRIx64
#define PRIXF PRIX64
#else
#define LISP_INFO           "32bit-code"
#define LISP_INTEGER_BIT	32
#define LISP_INTEGER_MASK	UINT32_MAX
#define FIXNUM_MAX			INT32_MAX
#define FIXNUM_MIN			INT32_MIN
typedef int32_t fixnum;
typedef uint32_t fixed;
#define PRIdF PRId32
#define PRIuF PRIu32
#define PRIxF PRIx32
#define PRIXF PRIX32
#endif

#define FIXNUM_UMIN			((fixed)FIXNUM_MIN)

#endif


/************************************************************
 *  typedef_stream.h
 ************************************************************/
#ifndef __LISP_TYPEDEF_STREAM_HEADER__
#define __LISP_TYPEDEF_STREAM_HEADER__


typedef int (*lisp_streamtype_close)(addr, addr *);
typedef int (*lisp_streamtype_read_byte)(addr, addr *, int *);
typedef int (*lisp_streamtype_unread_byte)(addr, byte);
typedef int (*lisp_streamtype_write_byte)(addr, addr);
typedef int (*lisp_streamtype_read_char)(addr, unicode *, int *);
typedef int (*lisp_streamtype_read_hang)(addr, unicode *, int *, int *);
typedef int (*lisp_streamtype_unread_char)(addr, unicode);
typedef int (*lisp_streamtype_write_char)(addr, unicode);
typedef int (*lisp_streamtype_getleft)(addr, size_t *);
typedef int (*lisp_streamtype_setleft)(addr, size_t);
typedef int (*lisp_streamtype_inputp)(addr, int *);
typedef int (*lisp_streamtype_outputp)(addr, int *);
typedef int (*lisp_streamtype_interactivep)(addr, int *);
typedef int (*lisp_streamtype_characterp)(addr, int *);
typedef int (*lisp_streamtype_binaryp)(addr, int *);
typedef int (*lisp_streamtype_element_type)(addr, addr *);
typedef int (*lisp_streamtype_external_format)(addr, addr *);
typedef int (*lisp_streamtype_file_length)(addr, addr *);
typedef int (*lisp_streamtype_file_position)(addr, size_t *, int *);
typedef int (*lisp_streamtype_file_position_start)(addr, int *);
typedef int (*lisp_streamtype_file_position_end)(addr, int *);
typedef int (*lisp_streamtype_file_position_set)(addr, size_t, int *);
typedef int (*lisp_streamtype_file_charlen)(addr, unicode, size_t *, int *);
typedef int (*lisp_streamtype_file_strlen)(addr, addr, size_t *, int *);
typedef int (*lisp_streamtype_listen)(addr, int *);
typedef int (*lisp_streamtype_clear_input)(addr);
typedef int (*lisp_streamtype_finish_output)(addr);
typedef int (*lisp_streamtype_force_output)(addr);
typedef int (*lisp_streamtype_clear_output)(addr);
typedef int (*lisp_streamtype_exitpoint)(addr);
typedef int (*lisp_streamtype_termsize)(addr, size_t *, int *);

#endif


/************************************************************
 *  typedef.h
 ************************************************************/
#ifndef __LISP_TYPEDEF_HEADER__
#define __LISP_TYPEDEF_HEADER__


/* setjmp */
typedef void (*lisp_abort_calltype)(void);
typedef int (*lisp_equal_calltype)(addr, addr, int *);

/* code */
union CODEVALUE {
	fixnum value;
	unicode character;
	size_t index;
	addr pos;
};
typedef union CODEVALUE CodeValue;

#endif


/************************************************************
 *  define_setjmp.h
 ************************************************************/
#ifndef __LISP_DEFINE_SETJMP__
#define __LISP_DEFINE_SETJMP__


/* abort */
#ifdef LISP_ABORT_SETJMP
extern jmp_buf Lisp_abort_setjmp;
#define Lisp_abort_throw()  longjmp(Lisp_abort_setjmp, 1)
#define Lisp_abort_Begin    if (setjmp(Lisp_abort_setjmp) == 0)
#define Lisp_abort_End      memset(&Lisp_abort_setjmp, 0, sizeof(Lisp_abort_setjmp))
#else
class Lisp_abort_class {};
#define Lisp_abort_throw()  throw Lisp_abort_class()
#define Lisp_abort_Begin    try
#define Lisp_abort_End      catch (Lisp_abort_class) {}
#endif

/* degrade */
#ifdef LISP_ABORT_SETJMP
extern jmp_buf Lisp_degrade_setjmp;
#define Lisp_degrade_throw()	longjmp(Lisp_degrade_setjmp, 1)
#define Lisp_degrade_Begin		if (setjmp(Lisp_degrade_setjmp) == 0)
#define Lisp_degrade_End		memset(&Lisp_degrade_setjmp, 0, sizeof(Lisp_degrade_setjmp))
#else
class Lisp_degrade_class {};
#define Lisp_degrade_throw()	throw Lisp_degrade_class()
#define Lisp_degrade_Begin		try
#define Lisp_degrade_End		catch (Lisp_degrade_class) {}
#endif

#endif


/************************************************************
 *  main_typedef.h
 ************************************************************/
#ifndef __LISP_MAIN_TYPEDEF_HEADER__
#define __LISP_MAIN_TYPEDEF_HEADER__


struct lispstringu_struct {
	unicode *ptr;
	size_t size;
};
typedef struct lispstringu_struct *lispstringu;

struct lisparrayu_struct {
	lispstringu *ptr;
	size_t size;
};
typedef struct lisparrayu_struct *lisparrayu;

struct lispkeyvalueu {
	lispstringu key, value;
};

struct lisptableu_struct {
	struct lispkeyvalueu *table;
	size_t size;
};
typedef struct lisptableu_struct *lisptableu;

#endif


/************************************************************
 *  main_argv.h
 ************************************************************/
#ifndef __LISP_MAIN_ARGV_HEADER__
#define __LISP_MAIN_ARGV_HEADER__


enum lispargv_execute {
	lispargv_load,
	lispargv_eval,
	lispargv_script,
	lispargv_minusminus
};

struct lispargv_string {
	enum lispargv_execute type;
	lispstringu value;
};

struct lispargv_input {
	struct lispargv_string *data;
	size_t size;
};

struct lispargv {
	/* mode */
	unsigned mode_help : 1;
	unsigned mode_version : 1;
	unsigned mode_core : 1;
	unsigned mode_standalone : 1;
	unsigned mode_degrade : 1;
	/* args */
	unsigned version_script : 1;
	unsigned nocore : 1;
	unsigned noinit : 1;
	unsigned debugger : 1;
	unsigned debuggerp : 1;
	unsigned quit : 1;
	unsigned reload : 1;
	unsigned terme_bright : 1;
	unsigned terme_dark : 1;
	unsigned terme_color : 1;
	unsigned terme_monochrome : 1;
	size_t heap, local, index, start;
	lispstringu core;
	lispstringu init;
	lisparrayu argv;
	lisptableu env;
	lispstringu reload_core;
	struct lispargv_input *input;
	int (*call)(void *);
	void *call_ptr;
};

void lispargv_free(struct lispargv *ptr);
struct lispargv *lispargv_main(int argc, char *argv[], char *env[]);
struct lispargv *lispargv_main_force(int argc, char *argv[], char *env[]);
#ifdef LISP_WINMAIN_WIDE
struct lispargv *lispargv_windows(void);
#endif

#endif


/************************************************************
 *  main_init.h
 ************************************************************/
#ifndef __LISP_MAIN_INIT_HEADER__
#define __LISP_MAIN_INIT_HEADER__


extern int lisp_code;
extern int lisp_result;
extern FILE *lisp_stderr;

void lisp_init(void);
void lisp_free(void);
int lisp_alloc(size_t heap, size_t local);

int lisp_main_help(FILE *file);
int lisp_main_version_text(FILE *file);
int lisp_main_version_script(FILE *file);
int lisp_main_version(struct lispargv *ptr, FILE *file);
int lisp_main_degrade(struct lispargv *ptr);

int lisp_argv_init(struct lispargv *ptr);
int lisp_argv_run(struct lispargv *ptr);

#endif


/************************************************************
 *  extern_typedef.h
 ************************************************************/
#ifndef __LISP_EXTERN_TYPEDEF_HEADER__
#define __LISP_EXTERN_TYPEDEF_HEADER__


enum lisp_escape {
	lisp_escape_normal,
	lisp_escape_tagbody,
	lisp_escape_block,
	lisp_escape_catch,
	lisp_escape_handler_case,
	lisp_escape_restart_case
};

enum LispEastAsianType {
	LispEastAsianType_error,
	LispEastAsianType_N,
	LispEastAsianType_A,
	LispEastAsianType_H,
	LispEastAsianType_W,
	LispEastAsianType_F,
	LispEastAsianType_NA
};

typedef int (*lisp_calltype_macro)(addr form, addr env);
typedef int (*lisp_calltype_rest)(addr args);
typedef int (*lisp_calltype_dynamic)(addr args);
typedef int (*lisp_calltype_any)(void);
typedef int (*lisp_calltype_empty)(void);
typedef int (*lisp_calltype_var1)(addr);
typedef int (*lisp_calltype_var2)(addr, addr);
typedef int (*lisp_calltype_var3)(addr, addr, addr);
typedef int (*lisp_calltype_var4)(addr, addr, addr, addr);
typedef int (*lisp_calltype_var5)(addr, addr, addr, addr, addr);
typedef int (*lisp_calltype_var6)(addr, addr, addr, addr, addr, addr);
typedef int (*lisp_calltype_opt1)(addr);
typedef int (*lisp_calltype_opt2)(addr, addr);
typedef int (*lisp_calltype_opt3)(addr, addr, addr);
typedef int (*lisp_calltype_var1opt1)(addr, addr);
typedef int (*lisp_calltype_var1opt2)(addr, addr, addr);
typedef int (*lisp_calltype_var1opt3)(addr, addr, addr, addr);
typedef int (*lisp_calltype_var2opt1)(addr, addr, addr);
typedef int (*lisp_calltype_var2opt2)(addr, addr, addr, addr);
typedef int (*lisp_calltype_var2opt3)(addr, addr, addr, addr, addr);
typedef int (*lisp_calltype_var3opt1)(addr, addr, addr, addr);
typedef int (*lisp_calltype_var3opt2)(addr, addr, addr, addr, addr);
typedef int (*lisp_calltype_var3opt3)(addr, addr, addr, addr, addr, addr);
typedef int (*lisp_calltype_var1rest)(addr, addr);
typedef int (*lisp_calltype_var2rest)(addr, addr, addr);
typedef int (*lisp_calltype_var3rest)(addr, addr, addr, addr);
typedef int (*lisp_calltype_var1dynamic)(addr, addr);
typedef int (*lisp_calltype_var2dynamic)(addr, addr, addr);
typedef int (*lisp_calltype_var3dynamic)(addr, addr, addr, addr);

#endif


/************************************************************
 *  extern_control.h
 ************************************************************/
#ifndef __LISP_EXTERN_CONTROL_HEADER__
#define __LISP_EXTERN_CONTROL_HEADER__


/* control */
void lisp_push_control(addr *ret);
int lisp_pop_control_(addr control);

/* special */
int lisp_push_special_(addr symbol, addr value);
int lisp_push_special8_(const void *name, addr value);
int lisp_push_special16_(const void *name, addr value);
int lisp_push_special32_(const void *name, addr value);
int lisp0_get_special_(addr *ret, addr symbol);
int lisp0_get_special8_(addr *ret, const void *name);
int lisp0_get_special16_(addr *ret, const void *name);
int lisp0_get_special32_(addr *ret, const void *name);
int lisp_get_special_(addr x, addr symbol);
int lisp_get_special8_(addr x, const void *name);
int lisp_get_special16_(addr x, const void *name);
int lisp_get_special32_(addr x, const void *name);
int lisp_set_special_(addr symbol, addr value);
int lisp_set_special8_(const void *name, addr value);
int lisp_set_special16_(const void *name, addr value);
int lisp_set_special32_(const void *name, addr value);

/* defvar */
int lisp_defvar_(addr symbol);
int lisp_defvar8_(const void *str);
int lisp_defvar16_(const void *str);
int lisp_defvar32_(const void *str);

/* catch / throw */
void lisp_catch(addr symbol);
int lisp_throw_(addr symbol);

/* handler */
int lisp_handler_bind_(addr name, addr call);
int lisp_handler_case_(addr name, addr call);
void lisp_handler_reverse(void);

/* restart */
void lisp0_restart_make(addr *ret, addr name, addr call, int casep);
void lisp_restart_make(addr x, addr name, addr call, int casep);
void lisp_restart_interactive(addr restart, addr call);
void lisp_restart_report(addr restart, addr call);
void lisp_restart_test(addr restart, addr call);
void lisp_restart_push(addr restart);
void lisp_restart_reverse(void);

#endif


/************************************************************
 *  extern_error.h
 ************************************************************/
#ifndef __LISP_EXTERN_ERROR_HEADER__
#define __LISP_EXTERN_ERROR_HEADER__


/* abort */
void lisp_abort(void);
void lisp_abortf(const char *fmt, ...);
void lisp_abort8(const void *fmt, ...);
void lisp_abort16(const void *fmt, ...);
void lisp_abort32(const void *fmt, ...);
lisp_abort_calltype lisp_set_abort_handler(lisp_abort_calltype call);
lisp_abort_calltype lisp_set_abort_setjmp_handler(void);

/* signal */
int lisp_signal_(addr condition);
int lisp_error_(addr condition);

/* error */
int lisp_error8_(const void *str, ...);
int lisp_error16_(const void *str, ...);
int lisp_error32_(const void *str, ...);

/* warn */
int lisp_warn8_(const void *str, ...);
int lisp_warn16_(const void *str, ...);
int lisp_warn32_(const void *str, ...);

#endif


/************************************************************
 *  extern_execute.h
 ************************************************************/
#ifndef __LISP_EXTERN_EXECUTE_HEADER__
#define __LISP_EXTERN_EXECUTE_HEADER__


/* eval */
int lisp0_eval_(addr *ret, addr pos);
int lisp0_eval8_(addr *ret, const void *str);
int lisp0_eval16_(addr *ret, const void *str);
int lisp0_eval32_(addr *ret, const void *str);
int lisp_eval_(addr x, addr pos);
int lisp_eval8_(addr x, const void *str);
int lisp_eval16_(addr x, const void *str);
int lisp_eval32_(addr x, const void *str);

/* call */
int lisp0_call_(addr *ret, addr call, addr args);
int lisp_call_(addr x, addr call, addr args);

/* funcall */
int lisp0_funcall_(addr *ret, addr call, ...);
int lisp0_funcall8_(addr *ret, const void *str, ...);
int lisp0_funcall16_(addr *ret, const void *str, ...);
int lisp0_funcall32_(addr *ret, const void *str, ...);
int lisp_funcall_(addr x, addr call, ...);
int lisp_funcall8_(addr x, const void *str, ...);
int lisp_funcall16_(addr x, const void *str, ...);
int lisp_funcall32_(addr x, const void *str, ...);

/* apply */
int lisp0_apply_(addr *ret, addr call, ...);
int lisp0_apply8_(addr *ret, const void *str, ...);
int lisp0_apply16_(addr *ret, const void *str, ...);
int lisp0_apply32_(addr *ret, const void *str, ...);
int lisp_apply_(addr x, addr call, ...);
int lisp_apply8_(addr x, const void *str, ...);
int lisp_apply16_(addr x, const void *str, ...);
int lisp_apply32_(addr x, const void *str, ...);

/* lowlevel */
int lisp_eval_control_(addr eval);
int lisp_eval_string_control_(addr eval);
int lisp_call_control_(addr call, addr args);
int lisp_funcall_control_(addr call, ...);
int lisp_apply_control_(addr call, ...);

/* values */
void lisp0_result_control(addr *ret);
void lisp0_result2_control(addr *ret1, addr *ret2);
void lisp0_values_control(addr *ret);
void lisp0_nth_value_control(addr *ret, size_t index);
void lisp_result_control(addr x);
void lisp_result2_control(addr x, addr y);
void lisp_values_control(addr x);
void lisp_nth_value_control(addr x, size_t index);
void lisp_set_result_control(addr value);
void lisp_set_values_control(addr first, ...);
void lisp_set_values_nil_control(void);
void lisp_set_values_list_control(addr list);

/* escape */
int lisp_equal_control(addr control);
int lisp_break_control(void);
int lisp_escape_control(void);
void lisp_reset_control(void);
enum lisp_escape lisp_escape_type_control(void);
void lisp_save_control(addr *ret);
void lisp_rollback_control(addr value);

/* system */
int lisp_eval_loop_(void);

#endif


/************************************************************
 *  extern_function.h
 ************************************************************/
#ifndef __LISP_EXTERN_FUNCTION_HEADER__
#define __LISP_EXTERN_FUNCTION_HEADER__


/* function */
void lisp0_get_function(addr *ret, addr symbol);
void lisp0_get_setf(addr *ret, addr symbol);
void lisp_get_function(addr x, addr symbol);
void lisp_get_setf(addr x, addr symbol);

int lisp0_get_function_(addr *ret, addr value);
int lisp0_get_function8_(addr *ret, const void *str);
int lisp0_get_function16_(addr *ret, const void *str);
int lisp0_get_function32_(addr *ret, const void *str);
int lisp_get_function_(addr x, addr value);
int lisp_get_function8_(addr x, const void *str);
int lisp_get_function16_(addr x, const void *str);
int lisp_get_function32_(addr x, const void *str);

int lisp0_get_setf_(addr *ret, addr value);
int lisp0_get_setf8_(addr *ret, const void *str);
int lisp0_get_setf16_(addr *ret, const void *str);
int lisp0_get_setf32_(addr *ret, const void *str);
int lisp_get_setf_(addr x, addr value);
int lisp_get_setf8_(addr x, const void *str);
int lisp_get_setf16_(addr x, const void *str);
int lisp_get_setf32_(addr x, const void *str);

/* compiled */
void lisp_compiled_macro(int index, lisp_calltype_macro call);
void lisp_compiled_rest(int index, lisp_calltype_rest call);
void lisp_compiled_dynamic(int index, lisp_calltype_dynamic call);
void lisp_compiled_any(int index, lisp_calltype_any call);
void lisp_compiled_empty(int index, lisp_calltype_empty call);
void lisp_compiled_var1(int index, lisp_calltype_var1 call);
void lisp_compiled_var2(int index, lisp_calltype_var2 call);
void lisp_compiled_var3(int index, lisp_calltype_var3 call);
void lisp_compiled_var4(int index, lisp_calltype_var4 call);
void lisp_compiled_var5(int index, lisp_calltype_var5 call);
void lisp_compiled_var6(int index, lisp_calltype_var6 call);
void lisp_compiled_opt1(int index, lisp_calltype_opt1 call);
void lisp_compiled_opt2(int index, lisp_calltype_opt2 call);
void lisp_compiled_opt3(int index, lisp_calltype_opt3 call);
void lisp_compiled_var1opt1(int index, lisp_calltype_var1opt1 call);
void lisp_compiled_var1opt2(int index, lisp_calltype_var1opt2 call);
void lisp_compiled_var1opt3(int index, lisp_calltype_var1opt3 call);
void lisp_compiled_var2opt1(int index, lisp_calltype_var2opt1 call);
void lisp_compiled_var2opt2(int index, lisp_calltype_var2opt2 call);
void lisp_compiled_var2opt3(int index, lisp_calltype_var2opt3 call);
void lisp_compiled_var3opt1(int index, lisp_calltype_var3opt1 call);
void lisp_compiled_var3opt2(int index, lisp_calltype_var3opt2 call);
void lisp_compiled_var3opt3(int index, lisp_calltype_var3opt3 call);
void lisp_compiled_var1rest(int index, lisp_calltype_var1rest call);
void lisp_compiled_var2rest(int index, lisp_calltype_var2rest call);
void lisp_compiled_var3rest(int index, lisp_calltype_var3rest call);
void lisp_compiled_var1dynamic(int index, lisp_calltype_var1dynamic call);
void lisp_compiled_var2dynamic(int index, lisp_calltype_var2dynamic call);
void lisp_compiled_var3dynamic(int index, lisp_calltype_var3dynamic call);

int lisp0_compiled_function_(addr *ret, int index, addr symbol);
int lisp0_compiled_function8_(addr *ret, int index, const void *str);
int lisp0_compiled_function16_(addr *ret, int index, const void *str);
int lisp0_compiled_function32_(addr *ret, int index, const void *str);
int lisp_compiled_function_(addr x, int index, addr symbol);
int lisp_compiled_function8_(addr x, int index, const void *str);
int lisp_compiled_function16_(addr x, int index, const void *str);
int lisp_compiled_function32_(addr x, int index, const void *str);
int lisp_compiled_defun_(int index, addr symbol);
int lisp_compiled_defun8_(int index, const void *str);
int lisp_compiled_defun16_(int index, const void *str);
int lisp_compiled_defun32_(int index, const void *str);
int lisp_compiled_defun_setf_(int index, addr symbol);
int lisp_compiled_defun_setf8_(int index, const void *str);
int lisp_compiled_defun_setf16_(int index, const void *str);
int lisp_compiled_defun_setf32_(int index, const void *str);

void lisp_compiled_setvalue(addr pos, addr value);
void lisp_compiled_getvalue(addr *ret);

#endif


/************************************************************
 *  extern_instance.h
 ************************************************************/
#ifndef __LISP_EXTERN_INSTANCE_HEADER__
#define __LISP_EXTERN_INSTANCE_HEADER__


/* find-class */
void lisp0_find_class(addr *ret, addr symbol);
int lisp0_find_class_(addr *ret, addr symbol);
int lisp0_find_class8_(addr *ret, const void *str);
int lisp0_find_class16_(addr *ret, const void *str);
int lisp0_find_class32_(addr *ret, const void *str);
void lisp_find_class(addr x, addr symbol);
int lisp_find_class_(addr x, addr symbol);
int lisp_find_class8_(addr x, const void *str);
int lisp_find_class16_(addr x, const void *str);
int lisp_find_class32_(addr x, const void *str);

/* make-instance */
int lisp0_instance_(addr *ret, addr clos, ...);
int lisp0_instance8_(addr *ret, const void *clos, ...);
int lisp0_instance16_(addr *ret, const void *clos, ...);
int lisp0_instance32_(addr *ret, const void *clos, ...);
int lisp_instance_(addr x, addr clos, ...);
int lisp_instance8_(addr x, const void *clos, ...);
int lisp_instance16_(addr x, const void *clos, ...);
int lisp_instance32_(addr x, const void *clos, ...);

/* slot-exists-p */
int lisp_slot_exists_(addr instance, addr symbol, int *ret);
int lisp_slot_exists8_(addr instance, const void *str, int *ret);
int lisp_slot_exists16_(addr instance, const void *str, int *ret);
int lisp_slot_exists32_(addr instance, const void *str, int *ret);

/* slot-boundp */
int lisp_slot_boundp_(addr instance, addr symbol, int *ret);
int lisp_slot_boundp8_(addr instance, const void *str, int *ret);
int lisp_slot_boundp16_(addr instance, const void *str, int *ret);
int lisp_slot_boundp32_(addr instance, const void *str, int *ret);

/* slot-makunbound */
int lisp_slot_makunbound_(addr instance, addr symbol);
int lisp_slot_makunbound8_(addr instance, const void *str);
int lisp_slot_makunbound16_(addr instance, const void *str);
int lisp_slot_makunbound32_(addr instance, const void *str);

/* slot-value */
int lisp0_slot_value_(addr *ret, addr instance, addr symbol);
int lisp0_slot_value8_(addr *ret, addr instance, const void *str);
int lisp0_slot_value16_(addr *ret, addr instance, const void *str);
int lisp0_slot_value32_(addr *ret, addr instance, const void *str);
int lisp_slot_value_(addr x, addr instance, addr symbol);
int lisp_slot_value8_(addr x, addr instance, const void *str);
int lisp_slot_value16_(addr x, addr instance, const void *str);
int lisp_slot_value32_(addr x, addr instance, const void *str);

/* setf slot-value */
int lisp_slot_setf_(addr instance, addr symbol, addr value);
int lisp_slot_setf8_(addr instance, const void *str, addr value);
int lisp_slot_setf16_(addr instance, const void *str, addr value);
int lisp_slot_setf32_(addr instance, const void *str, addr value);

#endif


/************************************************************
 *  extern_object.h
 ************************************************************/
#ifndef __LISP_EXTERN_OBJECT_HEADER__
#define __LISP_EXTERN_OBJECT_HEADER__


/* object */
int lisp0_character_(addr *ret, unicode value);
void lisp0_fixnum(addr *ret, fixnum value);
int lisp0_float_(addr *ret, float value);
int lisp0_double_(addr *ret, double value);
int lisp0_long_double_(addr *ret, long double value);

int lisp_character_(addr x, unicode value);
void lisp_fixnum(addr x, fixnum value);
int lisp_float_(addr x, float value);
int lisp_double_(addr x, double value);
int lisp_long_double_(addr x, long double value);

int lisp_zero_p(addr value);
int lisp_plus_p(addr value);
int lisp_minus_p(addr value);
void lisp_get_character(addr pos, unicode *ret);
void lisp_get_fixnum(addr pos, fixnum *ret);
int lisp_get_float_(addr pos, float *ret);
int lisp_get_double_(addr pos, double *ret);
int lisp_get_long_double_(addr pos, long double *ret);

/* package */
int lisp0_package_(addr *ret, addr pos);
int lisp0_package8_(addr *ret, const void *str);
int lisp0_package16_(addr *ret, const void *str);
int lisp0_package32_(addr *ret, const void *str);
int lisp_package_(addr x, addr pos);
int lisp_package8_(addr x, const void *str);
int lisp_package16_(addr x, const void *str);
int lisp_package32_(addr x, const void *str);

int lisp_in_package_(addr pos);
int lisp_in_package8_(const void *str);
int lisp_in_package16_(const void *str);
int lisp_in_package32_(const void *str);
int lisp_push_and_in_package_(addr pos);
int lisp_push_and_in_package8_(const void *str);
int lisp_push_and_in_package16_(const void *str);
int lisp_push_and_in_package32_(const void *str);

/* intern */
int lisp0_intern_(addr *ret, addr package, addr name);
int lisp0_intern8_(addr *ret, const void *package, const void *name);
int lisp0_intern16_(addr *ret, const void *package, const void *name);
int lisp0_intern32_(addr *ret, const void *package, const void *name);
int lisp_intern_(addr x, addr package, addr name);
int lisp_intern8_(addr x, const void *package, const void *name);
int lisp_intern16_(addr x, const void *package, const void *name);
int lisp_intern32_(addr x, const void *package, const void *name);

/* reader */
int lisp0_reader_(addr *ret, addr str);
int lisp0_reader8_(addr *ret, const void *str);
int lisp0_reader16_(addr *ret, const void *str);
int lisp0_reader32_(addr *ret, const void *str);
int lisp_reader_(addr x, addr str);
int lisp_reader8_(addr x, const void *str);
int lisp_reader16_(addr x, const void *str);
int lisp_reader32_(addr x, const void *str);

/* pathname */
int lisp0_pathname_(addr *ret, addr name);
int lisp0_pathname8_(addr *ret, const void *str);
int lisp0_pathname16_(addr *ret, const void *str);
int lisp0_pathname32_(addr *ret, const void *str);
int lisp0_namestring_(addr *ret, addr path);
int lisp_pathname_(addr x, addr name);
int lisp_pathname8_(addr x, const void *str);
int lisp_pathname16_(addr x, const void *str);
int lisp_pathname32_(addr x, const void *str);
int lisp_namestring_(addr x, addr path);

/* paper */
int lisp0_paper_(addr *ret, size_t array, size_t body);
int lisp_paper_(addr x, size_t array, size_t body);
int lisp_paper_gettype_(addr x, byte *ret);
int lisp_paper_settype_(addr x, byte value);
int lisp_paper_lenarray_(addr x, size_t *ret);
int lisp_paper_lenbody_(addr x, size_t *ret);
int lisp0_paper_getarray_(addr *ret, addr pos, size_t index);
int lisp_paper_getarray_(addr x, addr pos, size_t index);
int lisp_paper_setarray_(addr x, size_t index, addr value);
int lisp_paper_getbody_(addr x, size_t index, byte *ret);
int lisp_paper_setbody_(addr x, size_t index, byte value);
int lisp_paper_getmemory_(addr x, size_t a, size_t b, void *output, size_t *ret);
int lisp_paper_setmemory_(addr x, size_t a, size_t b, const void *input, size_t *ret);
int lisp_paper_body_unsafe_(addr x, byte **ptr, size_t *ret);

#endif


/************************************************************
 *  extern_print.h
 ************************************************************/
#ifndef __LISP_EXTERN_PRINT_HEADER__
#define __LISP_EXTERN_PRINT_HEADER__


/* format */
int lisp_format8_(addr stream, const void *str, ...);
int lisp_format16_(addr stream, const void *str, ...);
int lisp_format32_(addr stream, const void *str, ...);

/* stdout */
int lisp_stdout8_(const void *str, ...);
int lisp_stdout16_(const void *str, ...);
int lisp_stdout32_(const void *str, ...);

/* stderr */
int lisp_stderr8_(const void *str, ...);
int lisp_stderr16_(const void *str, ...);
int lisp_stderr32_(const void *str, ...);

/* stringf */
int lisp0_stringf8_(addr *ret, const void *str, ...);
int lisp0_stringf16_(addr *ret, const void *str, ...);
int lisp0_stringf32_(addr *ret, const void *str, ...);
int lisp_stringf8_(addr x, const void *str, ...);
int lisp_stringf16_(addr x, const void *str, ...);
int lisp_stringf32_(addr x, const void *str, ...);

#endif


/************************************************************
 *  extern_sequence.h
 ************************************************************/
#ifndef __LISP_EXTERN_SEQUENCE_HEADER__
#define __LISP_EXTERN_SEQUENCE_HEADER__


/* sequence */
void lisp0_cons(addr *ret, addr car, addr cdr);
void lisp_cons(addr x, addr car, addr cdr);
void lisp0_vector(addr *ret, size_t size);
void lisp_vector(addr x, size_t size);

void lisp0_list_va(addr *ret, va_list args);
void lisp0_lista_va(addr *ret, va_list args);
void lisp0_list(addr *ret, ...);
void lisp_list(addr x, ...);
void lisp0_lista(addr *ret, ...);
void lisp_lista(addr x, ...);

int lisp0_getelt_(addr *ret, addr pos, size_t index);
int lisp_getelt_(addr x, addr pos, size_t index);
int lisp_setelt_(addr pos, size_t index, addr value);
int lisp_length_(addr pos, size_t *ret);

int lisp0_reverse_(addr *ret, addr pos);
int lisp0_nreverse_(addr *ret, addr pos);
int lisp_reverse_(addr x, addr pos);
int lisp_nreverse_(addr x, addr pos);

/* cons */
void lisp0_car(addr *ret, addr list);
void lisp0_cdr(addr *ret, addr list);
void lisp0_carcdr(addr *car, addr *cdr, addr list);
void lisp_car(addr x, addr list);
void lisp_cdr(addr x, addr list);
void lisp_carcdr(addr x, addr y, addr list);

void lisp_setf_car(addr cons, addr value);
void lisp_setf_cdr(addr cons, addr value);
void lisp_setf_carcdr(addr cons, addr car, addr cdr);

/* string */
int lisp0_string8_(addr *ret, const void *str);
int lisp0_string16_(addr *ret, const void *str);
int lisp0_string32_(addr *ret, const void *str);
int lisp_string8_(addr x, const void *str);
int lisp_string16_(addr x, const void *str);
int lisp_string32_(addr x, const void *str);
int lisp_string_getc_(addr pos, size_t i, unicode *c);

/* strvect */
int lisp_strvect_getc(addr pos, size_t i, unicode *c);
int lisp_strvect_length(addr pos, size_t *ret);

#endif


/************************************************************
 *  extern_stream.h
 ************************************************************/
#ifndef __LISP_EXTERN_STREAM_HEADER__
#define __LISP_EXTERN_STREAM_HEADER__


/* stream object */
void lisp0_stream_define(addr *ret, int index, size_t size);
void lisp_stream_define(addr x, int index, size_t size);
void lisp_stream_memory(addr stream, void **ret);
void lisp0_getinfo_stream(addr *ret, addr stream);
void lisp_getinfo_stream(addr x, addr stream);
void lisp_setinfo_stream(addr stream, addr value);

void lisp_stream_calltype_close(int, lisp_streamtype_close);
void lisp_stream_calltype_read_byte(int, lisp_streamtype_read_byte);
void lisp_stream_calltype_unread_byte(int, lisp_streamtype_unread_byte);
void lisp_stream_calltype_write_byte(int, lisp_streamtype_write_byte);
void lisp_stream_calltype_read_char(int, lisp_streamtype_read_char);
void lisp_stream_calltype_read_hang(int, lisp_streamtype_read_hang);
void lisp_stream_calltype_unread_char(int, lisp_streamtype_unread_char);
void lisp_stream_calltype_write_char(int, lisp_streamtype_write_char);
void lisp_stream_calltype_getleft(int, lisp_streamtype_getleft);
void lisp_stream_calltype_setleft(int, lisp_streamtype_setleft);
void lisp_stream_calltype_inputp(int, lisp_streamtype_inputp);
void lisp_stream_calltype_outputp(int, lisp_streamtype_outputp);
void lisp_stream_calltype_interactivep(int, lisp_streamtype_interactivep);
void lisp_stream_calltype_characterp(int, lisp_streamtype_characterp);
void lisp_stream_calltype_binaryp(int, lisp_streamtype_binaryp);
void lisp_stream_calltype_element_type(int, lisp_streamtype_element_type);
void lisp_stream_calltype_external_format(int, lisp_streamtype_external_format);
void lisp_stream_calltype_file_length(int, lisp_streamtype_file_length);
void lisp_stream_calltype_file_position(int, lisp_streamtype_file_position);
void lisp_stream_calltype_file_position_start(int, lisp_streamtype_file_position_start);
void lisp_stream_calltype_file_position_end(int, lisp_streamtype_file_position_end);
void lisp_stream_calltype_file_position_set(int, lisp_streamtype_file_position_set);
void lisp_stream_calltype_file_charlen(int, lisp_streamtype_file_charlen);
void lisp_stream_calltype_file_strlen(int, lisp_streamtype_file_strlen);
void lisp_stream_calltype_listen(int, lisp_streamtype_listen);
void lisp_stream_calltype_clear_input(int, lisp_streamtype_clear_input);
void lisp_stream_calltype_finish_output(int, lisp_streamtype_finish_output);
void lisp_stream_calltype_force_output(int, lisp_streamtype_force_output);
void lisp_stream_calltype_clear_output(int, lisp_streamtype_clear_output);
void lisp_stream_calltype_exitpoint(int, lisp_streamtype_exitpoint);
void lisp_stream_calltype_termsize(int, lisp_streamtype_termsize);

void lisp_stream_calltype_error_close(int);
void lisp_stream_calltype_error_read_byte(int);
void lisp_stream_calltype_error_unread_byte(int);
void lisp_stream_calltype_error_write_byte(int);
void lisp_stream_calltype_error_read_char(int);
void lisp_stream_calltype_error_read_hang(int);
void lisp_stream_calltype_error_unread_char(int);
void lisp_stream_calltype_error_write_char(int);
void lisp_stream_calltype_error_getleft(int);
void lisp_stream_calltype_error_setleft(int);
void lisp_stream_calltype_error_inputp(int);
void lisp_stream_calltype_error_outputp(int);
void lisp_stream_calltype_error_interactivep(int);
void lisp_stream_calltype_error_characterp(int);
void lisp_stream_calltype_error_binaryp(int);
void lisp_stream_calltype_error_element_type(int);
void lisp_stream_calltype_error_external_format(int);
void lisp_stream_calltype_error_file_length(int);
void lisp_stream_calltype_error_file_position(int);
void lisp_stream_calltype_error_file_position_start(int);
void lisp_stream_calltype_error_file_position_end(int);
void lisp_stream_calltype_error_file_position_set(int);
void lisp_stream_calltype_error_file_charlen(int);
void lisp_stream_calltype_error_file_strlen(int);
void lisp_stream_calltype_error_listen(int);
void lisp_stream_calltype_error_clear_input(int);
void lisp_stream_calltype_error_finish_output(int);
void lisp_stream_calltype_error_force_output(int);
void lisp_stream_calltype_error_clear_output(int);
void lisp_stream_calltype_error_exitpoint(int);
void lisp_stream_calltype_error_termsize(int);

#endif


/************************************************************
 *  extern_type.h
 ************************************************************/
#ifndef __LISP_EXTERN_TYPE_HEADER__
#define __LISP_EXTERN_TYPE_HEADER__


/* hold */
int lisp_hold_p(addr x);
void lisp_hold_value(addr x, addr *ret);
void lisp_hold_set(addr x, addr value);
addr Lisp_holdv(addr x);
void lisp_hold(addr *ret, addr value);
addr Lisp_hold(void);

/* nil, t */
void lisp0_nil(addr *ret);
void lisp0_t(addr *ret);
void lisp_nil(addr x);
void lisp_t(addr x);
addr Lisp_nil(void);
addr Lisp_t(void);

/* type */
int lisp_nil_p(addr x);
int lisp_t_p(addr x);
int lisp_null_p(addr x);
int lisp_character_p(addr x);
int lisp_cons_p(addr x);
int lisp_list_p(addr x);
int lisp_string_p(addr x);
int lisp_strvect_p(addr x);
int lisp_symbol_p(addr x);
int lisp_array_p(addr x);
int lisp_vector_p(addr x);

int lisp_fixnum_p(addr x);
int lisp_bignum_p(addr x);
int lisp_integer_p(addr x);
int lisp_ratio_p(addr x);
int lisp_rational_p(addr x);
int lisp_single_float_p(addr x);
int lisp_double_float_p(addr x);
int lisp_long_float_p(addr x);
int lisp_float_p(addr x);
int lisp_real_p(addr x);
int lisp_complex_p(addr x);
int lisp_number_p(addr x);

int lisp_clos_p(addr x);
int lisp_hashtable_p(addr x);
int lisp_readtable_p(addr x);
int lisp_control_p(addr x);
int lisp_callname_p(addr x);
int lisp_function_p(addr x);
int lisp_package_p(addr x);
int lisp_random_state_p(addr x);
int lisp_pathname_p(addr x);
int lisp_stream_p(addr x);
int lisp_restart_p(addr x);
int lisp_environment_p(addr x);
int lisp_bitvector_p(addr x);
int lisp_print_dispatch_p(addr x);
int lisp_paper_p(addr x);

#endif


/************************************************************
 *  extern_unicode.h
 ************************************************************/
#ifndef __LISP_EXTERN_UNICODE_HEADER__
#define __LISP_EXTERN_UNICODE_HEADER__


/* eastasian */
int lisp_eastasian_set(enum LispEastAsianType type, unsigned width);
int lisp_eastasian_get(enum LispEastAsianType type, unsigned *ret);
enum LispEastAsianType lisp_eastasian_type_unicode(unicode c);
enum LispEastAsianType lisp_eastasian_type_character(addr value);

unsigned lisp_eastasian_unicode(unicode c);
int lisp_eastasian_character_(addr value, unsigned *ret);
int lisp_eastasian_string_(addr value, size_t *ret);
int lisp_eastasian_width_(addr value, size_t *ret);

int lisp_unicode_count(void);

int lisp_utf8_encode(unicode c, void *ptr, size_t *ret);
int lisp_utf16_range(unicode c);
int lisp_utf16_high(unicode c);
int lisp_utf16_low(unicode c);
unicode lisp_utf16_merge(byte16 first, byte16 second);

#endif

#endif
