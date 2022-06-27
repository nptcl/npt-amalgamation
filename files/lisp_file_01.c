/*
 *  npt -- ANSI Common Lisp Programming Language.
 *    https://github.com/nptcl/npt
 *    https://github.com/nptcl/npt-amalgamation
 *
 *  File: lisp_file_01.c
 */
#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-function"
#endif
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#endif

#define LISP_AMALGAMATION
#ifdef __cplusplus
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif
#endif

#include <errno.h>
#include <locale.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lisp_file.h"


/************************************************************
 *  alloc.c
 ************************************************************/
int lispd_empty_file_for_release;


/************************************************************
 *  arch.c
 ************************************************************/

/*
 *  read force
 */
#ifdef LISP_UNIX
#include <unistd.h>

/* Unix read */
int read_unix(int file, void *pos, size_t size, size_t *ret)
{
	ssize_t check;

retry:
	check = read(file, pos, size);
	if (check < 0) {
		if (errno == EINTR)
			goto retry;
		*ret = 0;
		return check;
	}
	if (check == 0) {
		*ret = 0;
		return 1;
	}
	*ret = (size_t)check;

	return 0;
}

int readf_unix(int file, void *ptr, size_t size, size_t *ret)
{
	ssize_t check;
	size_t count, rsize, diff;
	unsigned char *pos;

	pos = (unsigned char *)ptr;
	for (count = 0; count < size; count += rsize) {
		diff = size - count;
		check = read_unix(file, (void *)pos, diff, &rsize);
		/* Error */
		if (check < 0) {
			*ret = 0;
			return check;
		}
		/* EOF */
		if (check) {
			if (count == 0) {
				*ret = 0;
				return check;
			}
			break;
		}
		/* Next */
		pos += rsize;
	}
	*ret = count;

	return 0;
}
#endif

#ifdef LISP_WINDOWS
/* Windows ReadFile */
int read_windows(HANDLE file, void *pos, size_t size, size_t *ret)
{
	BOOL check;
	DWORD dsize;

	Check(0xFFFFFFFFULL < size, "size error");
	check = ReadFile(file, (LPVOID)pos, (DWORD)size, &dsize, NULL);
	if (check == 0) {
		*ret = 0;
		return -1;
	}
	if (dsize == 0) {
		*ret = 0;
		return 1;
	}
	*ret = (size_t)dsize;

	return 0;
}

int readf_windows(HANDLE file, void *ptr, size_t size, size_t *ret)
{
	int check;
	size_t count, rsize, diff;
	unsigned char *pos;

	pos = (unsigned char *)ptr;
	for (count = 0; count < size; count += rsize) {
		diff = size - count;
		if (0xFFFFFFFFULL < diff)
			diff = 0xFFFFFFFFULL;
		check = read_windows(file, (void *)pos, diff, &rsize);
		/* Error */
		if (check < 0) {
			*ret = 0;
			return check;
		}
		/* EOF */
		if (check) {
			if (count == 0) {
				*ret = 0;
				return check;
			}
			break;
		}
		/* Next */
		pos += rsize;
	}
	*ret = count;

	return 0;
}
#endif


/*
 *  safe
 */
int multisafe_size(size_t left, size_t right, size_t *result)
{
	size_t temp;

	if (left == 0 || right == 0) {
		*result = 0;
		return 0;
	}
	temp = left * right;
	if (temp / right < left) {
		*result = 0;
		return 1;
	}
	*result = temp;

	return 0;
}

int plussafe_size(size_t a, size_t b, size_t *result)
{
	if (a > SIZE_MAX - b)
		return 1;
	*result = a + b;

	return 0;
}


/*
 *  arch
 */
#ifdef LISP_TERME_WINDOWS
#include "windows_arch.h"
void exit_arch(int code)
{
	exit_windows(code);
}

void stdout_arch(const char *msg)
{
	stdout_windows(msg);
}

void stderr_arch(const char *msg)
{
	stderr_windows(msg);
}

#else

void exit_arch(int code)
{
	exit(1);
}

void stdout_arch(const char *msg)
{
	(void)printf("%s", msg);
}

void stderr_arch(const char *msg)
{
	(void)fprintf(stderr, "%s", msg);
}
#endif

#if defined(LISP_TERME_WINDOWS)
#include "windows_arch.h"

int getwidth_arch(unsigned *rx, unsigned *ry)
{
	unsigned x, y;

	if (getwidth_windows(&x, &y))
		return 1;
	if (rx)
		*rx = x;
	if (ry)
		*ry = y;

	return 0;
}

#elif defined(LISP_UNIX)
#include <sys/ioctl.h>
#include <unistd.h>

int getwidth_arch(unsigned *rx, unsigned *ry)
{
	unsigned x, y;
	struct winsize ws;

	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1)
		return 1;
	x = (unsigned)ws.ws_col;
	y = (unsigned)ws.ws_row;
	if (x <= 2)
		x = 2;
	if (y <= 1)
		y = 1;
	if (rx)
		*rx = x;
	if (ry)
		*ry = y;

#if 0
	/* for debug */
	if (rx)
		*rx = 10;
	if (ry)
		*ry = 5;
#endif

	return 0;
}

#else
int getwidth_arch(unsigned *rx, unsigned *ry)
{
	if (rx)
		*rx = 0;
	if (ry)
		*ry = 0;

	return 1;
}
#endif


/************************************************************
 *  array.c
 ************************************************************/

/*
 *  accessor
 */
void arraygen_set_debug(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_ARRAY_GENERAL);
	Check(arraygen_lenr_Low(pos) <= index, "size error");
	arraygen_set_Low(pos, index, value);
}

void arraygen_get_debug(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ARRAY_GENERAL);
	Check(arraygen_lenr_Low(pos) <= index, "size error");
	arraygen_get_Low(pos, index, ret);
}

void arraygen_len_debug(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_ARRAY_GENERAL);
	arraygen_len_Low(pos, ret);
}

size_t arraygen_lenr_debug(addr pos)
{
	return arraygen_lenr_Low(pos);
}

void arrayspec_pos_debug(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ARRAY_SPECIALIZED);
	arrayspec_pos_Low(pos, ret);
}

addr arrayspec_ptr_debug(addr pos)
{
	CheckType(pos, LISPSYSTEM_ARRAY_SPECIALIZED);
	return arrayspec_ptr_Low(pos);
}

size_t *arraysize_ptr_debug(addr pos)
{
	CheckType(pos, LISPSYSTEM_ARRAY_DIMENSION);
	return arraysize_ptr_Low(pos);
}

struct array_struct *arrayinfo_struct_debug(addr pos)
{
	CheckType(pos, LISPTYPE_ARRAY);
	return arrayinfo_struct_Low(pos);
}

void getarrayinfo_debug(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPTYPE_ARRAY);
	GetArrayInfo_Low(pos, index, ret);
}

void setarrayinfo_debug(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPTYPE_ARRAY);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetArrayInfo_Low(pos, index, value);
}

void lenarrayinfo_debug(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_ARRAY);
	LenArrayInfo_Low(pos, ret);
}

size_t lenarrayinfor_debug(addr pos)
{
	CheckType(pos, LISPTYPE_ARRAY);
	return LenArrayInfor_Low(pos);
}


/*
 *  memory allocate
 */
void arraygen_alloc(LocalRoot local, addr *ret, size_t size)
{
	arraygen_alloc_Low(local, ret, LISPSYSTEM_ARRAY_GENERAL, size);
}
void arraygen_local(LocalRoot local, addr *ret, size_t size)
{
	CheckLocal(local);
	arraygen_alloc(local, ret, size);
}
void arraygen_heap(addr *ret, size_t size)
{
	arraygen_alloc(NULL, ret, size);
}

void arrayspec_alloc(LocalRoot local, addr *ret, size_t size)
{
	arrayspec_alloc_Low(local, ret, LISPSYSTEM_ARRAY_SPECIALIZED, size);
}
void arrayspec_local(LocalRoot local, addr *ret, size_t size)
{
	CheckLocal(local);
	arrayspec_alloc(local, ret, size);
}
void arrayspec_heap(addr *ret, size_t size)
{
	arrayspec_alloc(NULL, ret, size);
}

void arrayinfo_alloc(LocalRoot local, addr *ret)
{
	arrayinfo_alloc_Low(local, ret, LISPTYPE_ARRAY,
			ARRAY_INDEX_SIZE, sizeoft(struct array_struct));
}
void arrayinfo_local(LocalRoot local, addr *ret)
{
	CheckLocal(local);
	arrayinfo_alloc(local, ret);
}
void arrayinfo_heap(addr *ret)
{
	arrayinfo_alloc(NULL, ret);
}

static void arraysize1_alloc(LocalRoot local, addr *ret, size_t size)
{
	arraysize1_alloc_Low(local, ret, LISPSYSTEM_ARRAY_DIMENSION, size);
}
int arraysize_alloc_(LocalRoot local, addr *ret, size_t index)
{
	if (multisafe_size(IdxSize, index, &index)) {
		*ret = Nil;
		return fmte_("Index overflow.", NULL);
	}
	arraysize1_alloc(local, ret, index);
	return 0;
}
int arraysize_local_(LocalRoot local, addr *ret, size_t index)
{
	CheckLocal(local);
	return arraysize_alloc_(local, ret, index);
}
int arraysize_heap_(addr *ret, size_t index)
{
	return arraysize_alloc_(NULL, ret, index);
}

int arraysize_copy_alloc_(LocalRoot local, addr *ret, addr pos, size_t size)
{
	addr one;
	size_t *data1;
	const size_t *data2;

	CheckType(pos, LISPSYSTEM_ARRAY_DIMENSION);
	Return(arraysize_alloc_(local, &one, size));
	data1 = arraysize_ptr(one);
	data2 = arraysize_ptr(pos);
	memcpy(data1, data2, IdxSize * size);

	return Result(ret, one);
}
int arraysize_copy_local_(LocalRoot local, addr *ret, addr pos, size_t size)
{
	CheckLocal(local);
	return arraysize_copy_alloc_(local, ret, pos, size);
}
int arraysize_copy_heap_(addr *ret, addr pos, size_t size)
{
	return arraysize_copy_alloc_(NULL, ret, pos, size);
}

void array_empty_alloc(LocalRoot local, addr *ret)
{
	addr pos;
	struct array_struct *str;

	arrayinfo_alloc(local, &pos);
	str = ArrayInfoStruct(pos);
	clearpoint(str);
	str->type = ARRAY_TYPE_EMPTY;
	*ret = pos;
}
void array_empty_local(LocalRoot local, addr *ret)
{
	CheckLocal(local);
	array_empty_alloc(local, ret);
}
void array_empty_heap(addr *ret)
{
	array_empty_alloc(NULL, ret);
}

int array_alloc_(LocalRoot local, addr *ret, size_t index, size_t size)
{
	addr pos, temp;
	struct array_struct *str;

	/* object */
	array_empty_alloc(local, &pos);
	str = ArrayInfoStruct(pos);

	/* dimension */
	if (2 <= index) {
		Return(arraysize_alloc_(local, &temp, index));
		SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, temp);
	}
	str->dimension = index;
	str->size = str->front = size;

	/* type */
	str->type = ARRAY_TYPE_T;
	GetTypeTable(&temp, T);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, temp);

	/* result */
	return Result(ret, pos);
}
int array_local_(LocalRoot local, addr *ret, size_t index, size_t size)
{
	CheckLocal(local);
	return array_alloc_(local, ret, index, size);
}
int array_heap_(addr *ret, size_t index, size_t size)
{
	return array_alloc_(NULL, ret, index, size);
}

static int array_va_stdarg_(LocalRoot local, addr *ret, va_list args)
{
	addr pos, dimension;
	size_t size, i, index, allcount, *data;
	va_list dest;

	/* index */
	va_copy(dest, args);
	allcount = 1;
	for (index = 0; ; index++) {
		size = (size_t)va_arg(dest, unsigned);
		if (size == 0)
			break;
		if (multisafe_size(allcount, size, &allcount)) {
			*ret = Nil;
			return fmte_("size overflow.", NULL);
		}
	}

	/* make */
	Return(array_alloc_(local, &pos, index, allcount));
	if (2 <= index) {
		GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &dimension);
		data = arraysize_ptr(dimension);
		for (i = 0; i < index; i++)
			data[i] = (size_t)va_arg(args, unsigned);
	}

	/* result */
	return Result(ret, pos);
}
int array_va_alloc_(LocalRoot local, addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	Return(array_va_stdarg_(local, ret, args));
	va_end(args);

	return 0;
}
int array_va_local_(LocalRoot local, addr *ret, ...)
{
	va_list args;

	CheckLocal(local);
	va_start(args, ret);
	Return(array_va_stdarg_(local, ret, args));
	va_end(args);

	return 0;
}
int array_va_heap_(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	Return(array_va_stdarg_(NULL, ret, args));
	va_end(args);

	return 0;
}


/*
 *  type check
 */
int array_system_general_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_ARRAY_GENERAL;
}

int array_system_specialized_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_ARRAY_SPECIALIZED;
}

int array_system_p(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPSYSTEM_ARRAY_GENERAL
		|| type == LISPSYSTEM_ARRAY_SPECIALIZED;
}

int arrayp(addr pos)
{
	return GetType(pos) == LISPTYPE_ARRAY;
}

int array_simple_p(addr pos)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);
	return str->simple;
}

int array_vector_p(addr pos)
{
	return arrayp(pos) && ArrayInfoStruct(pos)->dimension == 1;
}

int array_displaced_p(addr pos)
{
	return arrayp(pos) && ArrayInfoStruct(pos)->displaced;
}

int array_size_vector_p(addr pos, size_t size)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);
	if (str->dimension != 1)
		return 0;

	return ArrayInfoStruct(pos)->size == size;
}

int array_general_p(addr pos)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);

	return str->type == ARRAY_TYPE_T;
}

int array_specialized_p(addr pos)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);

	return str->type != ARRAY_TYPE_T;
}

int array_simple_vector_p(addr pos)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);
	return str->simple && str->dimension == 1 && str->type == ARRAY_TYPE_T;
}

int array_adjustable_p(addr pos)
{
	Check(! arrayp(pos), "type error");
	return ArrayInfoStruct(pos)->adjustable;
}

int array_fillpointer_p(addr pos)
{
	Check(! arrayp(pos), "type error");
	return ArrayInfoStruct(pos)->fillpointer;
}

size_t array_dimension_size(addr pos)
{
	Check(! arrayp(pos), "type error");
	return ArrayInfoStruct(pos)->dimension;
}

size_t array_total_size(addr pos)
{
	Check(! arrayp(pos), "type error");
	return ArrayInfoStruct(pos)->size;
}

size_t array_fill_size(addr pos)
{
	Check(! arrayp(pos), "type error");
	return ArrayInfoStruct(pos)->front;
}

enum ARRAY_TYPE array_type(addr pos)
{
	Check(! arrayp(pos), "type error");
	return ArrayInfoStruct(pos)->type;
}

unsigned array_type_size(addr pos)
{
	Check(! arrayp(pos), "type error");
	return ArrayInfoStruct(pos)->bytesize;
}


/*
 *  memory access
 */
const size_t *array_ptrsize(addr pos)
{
	struct array_struct *str;

	Check(GetType(pos) != LISPTYPE_ARRAY, "type error");
	str = ArrayInfoStruct(pos);
	switch (str->dimension) {
		case 0:
			return NULL;

		case 1:
			return (const size_t *)&(str->size);

		default:
			GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &pos);
			return arraysize_ptr(pos);
	}
}

int array_ptrwrite_(addr pos, size_t index, void **ret)
{
	enum ARRAY_TYPE type;
	struct array_struct *str;
	size_t size;

	str = ArrayInfoStruct(pos);
	type = str->type;
	size = str->size;
	if (type == ARRAY_TYPE_EMPTY) {
		*ret = NULL;
		return fmte_("The array has no memory yet.", NULL);
	}
	if (type == ARRAY_TYPE_T || type == ARRAY_TYPE_BIT) {
		*ret = NULL;
		return fmte_("The object is not specialized array.", NULL);
	}
	if (size <= index) {
		*ret = NULL;
		return fmte_("Index is too large.", NULL);
	}
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);

	*ret = (void *)(arrayspec_ptr(pos) + (index * str->element));
	return 0;
}

int array_ptrread_(addr pos, size_t index, void *const *ret)
{
	return array_ptrwrite_(pos, index, (void **)ret);
}


/*
 *  fill-pointer
 */
int array_fill_pointer(addr array, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	if (! str->fillpointer)
		return 1;
	*ret = intsizeh(str->front);

	return 0;
}

int array_setf_fill_pointer_(addr array, addr value, int *ret)
{
	struct array_struct *str;
	size_t size;

	str = ArrayInfoStruct(array);
	if (! str->fillpointer)
		return Result(ret, 1);
	if (GetIndex_integer(value, &size)) {
		*ret = 0;
		return fmte_("Invalid fill-pointer value ~S.", value, NULL);
	}
	if (str->size < size) {
		*ret = 0;
		return fmte_("Fill-pointer value ~A "
				"must be less than equal to array size ~A.",
				value, intsizeh(str->size), NULL);
	}
	str->front = size;

	return Result(ret, 0);
}

int array_fill_pointer_start(addr array)
{
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	if (! str->fillpointer) {
		return 1;
	}
	else {
		str->front = 0;
		return 0;
	}
}

int array_fill_pointer_end(addr array)
{
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	if (! str->fillpointer)
		return 1;
	else
		return 0;
}

int array_fill_pointer_set(addr array, size_t size)
{
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	if (! str->fillpointer)
		return 1;
	if (str->size <= size)
		return 1;
	str->front = size;

	return 0;
}


/************************************************************
 *  array_access.c
 ************************************************************/

/*
 *  arraymemory_get
 */
static int arraymemory_get_memory_(addr pos, size_t index,
		addr *retp, size_t *rets, int *ret)
{
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, retp);
	*rets = index;
	*ret = array_system_p(*retp);
	return 0;
}

int arraymemory_get_(addr pos, size_t index, addr *retp, size_t *rets, int *ret)
{
	struct array_struct *str;

	Check(! arrayp(pos), "type error");
	str = ArrayInfoStruct(pos);
	if (str->size <= index) {
		*retp = Nil;
		*rets = 0;
		*ret = 0;
		return fmte_("Index ~S must be less than array size.", intsizeh(index), NULL);
	}
	if (! str->displaced)
		return arraymemory_get_memory_(pos, index, retp, rets, ret);

	/* displaced */
	GetArrayInfo(pos, ARRAY_INDEX_DISPLACED, &pos);
	index += str->offset;
	if (arrayp(pos))
		return arraymemory_get_(pos, index, retp, rets, ret);

	/* not array */
	*retp = pos;
	*rets = index;
	*ret = 0;
	return 0;
}


/*
 *  arraymemory_set
 */
/* character */
static void arraymemory_setvalue_character(addr pos, size_t index, unicode value)
{
	((unicode *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setcharacter_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_character_(&str, value));
	arraymemory_setvalue_character(pos, index, str.value.character);

	return 0;
}

/* singed */
static void arraymemory_setvalue_signed8(addr pos, size_t index, int8_t value)
{
	((int8_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setsigned8_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_signed8_(&str, value));
	arraymemory_setvalue_signed8(pos, index, str.value.signed8);

	return 0;
}

static void arraymemory_setvalue_signed16(addr pos, size_t index, int16_t value)
{
	((int16_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setsigned16_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_signed16_(&str, value));
	arraymemory_setvalue_signed16(pos, index, str.value.signed16);

	return 0;
}

static void arraymemory_setvalue_signed32(addr pos, size_t index, int32_t value)
{
	((int32_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setsigned32_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_signed32_(&str, value));
	arraymemory_setvalue_signed32(pos, index, str.value.signed32);

	return 0;
}

#ifdef LISP_64BIT
static void arraymemory_setvalue_signed64(addr pos, size_t index, int64_t value)
{
	((int64_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setsigned64_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_signed64_(&str, value));
	arraymemory_setvalue_signed64(pos, index, str.value.signed64);

	return 0;
}
#endif

static int arraymemory_setsigned_(addr pos, unsigned size, size_t index, addr value)
{
	switch (size) {
		case 8:
			return arraymemory_setsigned8_(pos, index, value);

		case 16:
			return arraymemory_setsigned16_(pos, index, value);

		case 32:
			return arraymemory_setsigned32_(pos, index, value);

#ifdef LISP_64BIT
		case 64:
			return arraymemory_setsigned64_(pos, index, value);
#endif
		default:
			return fmte_("Invalid array size.", NULL);
	}
}

/* unsigned */
static void arraymemory_setvalue_unsigned8(addr pos, size_t index, uint8_t value)
{
	((uint8_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setunsigned8_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_unsigned8_(&str, value));
	arraymemory_setvalue_unsigned8(pos, index, str.value.unsigned8);

	return 0;
}

static void arraymemory_setvalue_unsigned16(addr pos, size_t index, uint16_t value)
{
	((uint16_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setunsigned16_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_unsigned16_(&str, value));
	arraymemory_setvalue_unsigned16(pos, index, str.value.unsigned16);

	return 0;
}

static void arraymemory_setvalue_unsigned32(addr pos, size_t index, uint32_t value)
{
	((uint32_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setunsigned32_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_unsigned32_(&str, value));
	arraymemory_setvalue_unsigned32(pos, index, str.value.unsigned32);

	return 0;
}

#ifdef LISP_64BIT
static void arraymemory_setvalue_unsigned64(addr pos, size_t index, uint64_t value)
{
	((uint64_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setunsigned64_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_unsigned64_(&str, value));
	arraymemory_setvalue_unsigned64(pos, index, str.value.unsigned64);

	return 0;

}
#endif

static int arraymemory_setunsigned_(addr pos, unsigned size, size_t index, addr value)
{
	switch (size) {
		case 8:
			return arraymemory_setunsigned8_(pos, index, value);

		case 16:
			return arraymemory_setunsigned16_(pos, index, value);

		case 32:
			return arraymemory_setunsigned32_(pos, index, value);

#ifdef LISP_64BIT
		case 64:
			return arraymemory_setunsigned64_(pos, index, value);
#endif
		default:
			return fmte_("Invalid array size.", NULL);
	}
}

/* single-float */
static void arraymemory_setvalue_single(addr pos, size_t index, single_float value)
{
	((single_float *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setsingle_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_single_(&str, value));
	arraymemory_setvalue_single(pos, index, str.value.single_value);

	return 0;
}

/* double-float */
static void arraymemory_setvalue_double(addr pos, size_t index, double_float value)
{
	((double_float *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setdouble_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_double_(&str, value));
	arraymemory_setvalue_double(pos, index, str.value.double_value);

	return 0;
}

/* long-float */
static void arraymemory_setvalue_long(addr pos, size_t index, long_float value)
{
	((long_float *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setlong_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_long_(&str, value));
	arraymemory_setvalue_long(pos, index, str.value.long_value);

	return 0;
}

static int arraymemory_set_(addr pos, addr mem, size_t index, addr value)
{
	enum ARRAY_TYPE type;
	unsigned bytesize;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	type = str->type;
	bytesize = str->bytesize;
	Check(str->size <= index, "size error");
	switch (type) {
		case ARRAY_TYPE_T:
			arraygen_set(mem, index, value);
			return 0;

		case ARRAY_TYPE_BIT:
			return fmte_("Invalid array object, ~S.", pos, NULL);

		case ARRAY_TYPE_CHARACTER:
			return arraymemory_setcharacter_(mem, index, value);

		case ARRAY_TYPE_SIGNED:
			return arraymemory_setsigned_(mem, bytesize, index, value);

		case ARRAY_TYPE_UNSIGNED:
			return arraymemory_setunsigned_(mem, bytesize, index, value);

		case ARRAY_TYPE_SINGLE_FLOAT:
			return arraymemory_setsingle_(mem, index, value);

		case ARRAY_TYPE_DOUBLE_FLOAT:
			return arraymemory_setdouble_(mem, index, value);

		case ARRAY_TYPE_LONG_FLOAT:
			return arraymemory_setlong_(mem, index, value);

		default:
			return fmte_("(SETF AREF) Invalid array type.", NULL);
	}
}


/*
 *  array-get/set
 */
int array_get_t_(addr pos, size_t index, addr *ret)
{
	int check;

	if (array_type(pos) != ARRAY_TYPE_T) {
		*ret = Nil;
		return fmte_("Invalid array type, ~S.", pos, NULL);
	}
	Return(arraymemory_get_(pos, index, &pos, &index, &check));
	if (check)
		arraygen_get(pos, index, ret);
	else
		getarray(pos, index, ret);

	return 0;
}

int array_get_bit_(addr pos, size_t index, int *ret)
{
	int ignore;

	if (array_type(pos) != ARRAY_TYPE_BIT) {
		*ret = 0;
		return fmte_("Invalid array type, ~S.", pos, NULL);
	}
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	return bitmemory_getint_(pos, index, ret);
}

static void arraymemory_getunicode(addr pos, size_t index, unicode *ret)
{
	*ret = ((unicode *)arrayspec_ptr(pos))[index];
}

int array_get_unicode_(addr pos, size_t index, unicode *ret)
{
	int check;

	if (array_type(pos) != ARRAY_TYPE_CHARACTER) {
		*ret = 0;
		return fmte_("Invalid array type, ~S.", pos, NULL);
	}
	Return(arraymemory_get_(pos, index, &pos, &index, &check));
	if (check) {
		arraymemory_getunicode(pos, index, ret);
	}
	else {
		Return(string_getc_(pos, index, ret));
	}

	return 0;
}

int array_get_(LocalRoot local, addr pos, size_t index, addr *ret)
{
	struct array_value str;

	Return(arrayinplace_get_(pos, index, &str));
	Return(arrayvalue_alloc_(local, ret, &str));

	return 0;
}

int array_set_bit_(addr pos, size_t index, int value)
{
	int ignore;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	if (array_type(pos) != ARRAY_TYPE_BIT)
		return fmte_("Invalid array type, ~S.", pos, NULL);
	if (value != 0 && value != 1)
		return fmte_("Value ~A must be a bit type (0 or 1).", fixnumh(value), NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	return bitmemory_setint_(pos, index, value);
}

int array_set_character_(addr pos, size_t index, unicode value)
{
	int check;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	if (array_type(pos) != ARRAY_TYPE_CHARACTER)
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &check));
	if (check) {
		arraymemory_setvalue_character(pos, index, value);
	}
	else {
		Return(string_setc_(pos, index, value));
	}

	return 0;
}

int array_set_signed8_(addr pos, size_t index, int8_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_SIGNED, 8))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_signed8(pos, index, value);

	return 0;
}

int array_set_signed16_(addr pos, size_t index, int16_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_SIGNED, 16))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_signed16(pos, index, value);

	return 0;
}

int array_set_signed32_(addr pos, size_t index, int32_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_SIGNED, 32))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_signed32(pos, index, value);

	return 0;
}

int array_set_unsigned8_(addr pos, size_t index, uint8_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_UNSIGNED, 8))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_unsigned8(pos, index, value);

	return 0;
}

int array_set_unsigned16_(addr pos, size_t index, uint16_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_UNSIGNED, 16))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_unsigned16(pos, index, value);

	return 0;
}

int array_set_unsigned32_(addr pos, size_t index, uint32_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_UNSIGNED, 32))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_unsigned32(pos, index, value);

	return 0;
}

#ifdef LISP_64BIT
int array_set_signed64_(addr pos, size_t index, int64_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_SIGNED, 64))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_signed64(pos, index, value);

	return 0;
}

int array_set_unsigned64_(addr pos, size_t index, uint64_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_UNSIGNED, 64))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_unsigned64(pos, index, value);

	return 0;
}
#endif

int array_set_single_(addr pos, size_t index, single_float value)
{
	int ignore;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	if (array_type(pos) != ARRAY_TYPE_SINGLE_FLOAT)
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_single(pos, index, value);

	return 0;
}

int array_set_double_(addr pos, size_t index, double_float value)
{
	int ignore;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	if (array_type(pos) != ARRAY_TYPE_DOUBLE_FLOAT)
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_double(pos, index, value);

	return 0;
}

int array_set_long_(addr pos, size_t index, long_float value)
{
	int ignore;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	if (array_type(pos) != ARRAY_TYPE_LONG_FLOAT)
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_long(pos, index, value);

	return 0;
}

int array_set_(addr pos, size_t index, addr value)
{
	int check;
	addr mem;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	Return(arraymemory_get_(pos, index, &mem, &index, &check));
	if (check)
		return arraymemory_set_(pos, mem, index, value);
	else
		return setelt_sequence_(mem, index, value);
}


/*
 *  setget
 */
int array_setget_(addr p1, size_t s1, addr p2, size_t s2)
{
	struct array_value value;
	struct array_struct *str1, *str2;

	if (GetStatusReadOnly(p1))
		return fmte_("Object ~S is constant.", p1, NULL);
	str1 = ArrayInfoStruct(p1);
	str2 = ArrayInfoStruct(p2);
	if (! array_equal_type(str1, str2->type, str2->bytesize))
		return fmte_("Array ~S type must be equal to base array ~S.", p1, p2, NULL);
	Return(arrayinplace_get_(p2, s2, &value));
	Return(arrayinplace_set_(p1, s1, &value));

	return 0;
}


/*
 *  array
 */
static int array1arefindex_(addr pos, addr args, struct array_struct *str, size_t *ret)
{
	addr arg;
	size_t index;

	if (! consp(args))
		return fmte_("Subscripts ~S must be list form.", args, NULL);
	GetCons(args, &arg, &args);
	if (args != Nil)
		return fmte_("Subscripts ~S too many arguments.", args, NULL);
	if (GetIndex_integer(arg, &index))
		return fmte_("Invalid subscript argument ~S.", arg, NULL);
	if (str->size <= index)
		return fmte_("Subscript ~S is too large.", arg, NULL);

	return Result(ret, index);
}

int array_arefindex_(addr pos, addr args, size_t *ret)
{
	struct array_struct *str;
	const size_t *data;
	size_t index, value, depth, dimension, range;
	addr check, list;

	CheckType(pos, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(pos);
	dimension = str->dimension;
	if (dimension == 1)
		return array1arefindex_(pos, args, str, ret);
	data = array_ptrsize(pos);
	index = 0;
	list = args;
	for (depth = 0; list != Nil; depth++) {
		if (! consp(list))
			return fmte_("Subscripts ~S must be a list type.", list, NULL);
		if (dimension <= depth)
			return fmte_("Subscripts ~A is too large.", args, NULL);
		GetCons(list, &check, &list);
		if (GetIndex_integer(check, &value))
			return fmte_("Invalid index value ~S.", check, NULL);
		range = data[depth];
		if (range <= value)
			return fmte_("Out of range ~S subscripts in ~S array.", list, pos, NULL);
		index = depth? (index * range): 0;
		index += value;
	}
	if (depth != str->dimension)
		return fmte_("Subscript ~S is too few.", args, NULL);

	return Result(ret, index);
}

int array_aref_(LocalRoot local, addr pos, addr args, addr *ret)
{
	size_t index;
	Return(array_arefindex_(pos, args, &index));
	return array_get_(local, pos, index, ret);
}

int array_setf_aref_(addr pos, addr args, addr value)
{
	size_t index;
	Return(array_arefindex_(pos, args, &index));
	return array_set_(pos, index, value);
}

int array_aref_bit_(LocalRoot local, addr pos, addr args, addr *ret)
{
	int value;
	size_t index;

	Return(array_arefindex_(pos, args, &index));
	Return(array_get_bit_(pos, index, &value));
	fixnum_alloc(local, ret, (fixnum)value);

	return 0;
}

int array_setf_aref_bit_(addr pos, addr args, addr value)
{
	int check;
	size_t index;

	Return(array_arefindex_(pos, args, &index));
	Return(bit_getint_error_(value, &check));
	fixnum_heap(&value, (fixnum)check);
	return array_set_(pos, index, value);
}


/*
 *  check
 */
int array_equal_type(struct array_struct *a, enum ARRAY_TYPE type, unsigned size)
{
	if (a->type != type)
		return 0;
	if (a->type == ARRAY_TYPE_SIGNED || a->type == ARRAY_TYPE_UNSIGNED)
		return a->bytesize == size;
	else
		return 1;
}

int array_equal_dimension(addr a, addr b)
{
	struct array_struct *str1, *str2;
	size_t size, i;
	const size_t *data1, *data2;

	CheckType(a, LISPTYPE_ARRAY);
	CheckType(b, LISPTYPE_ARRAY);
	str1 = ArrayInfoStruct(a);
	str2 = ArrayInfoStruct(b);
	size = str1->dimension;
	if (size != str2->dimension)
		return 0;
	if (size == 0)
		return 1;
	if (size == 1)
		return str1->front == str2->front;
	data1 = array_ptrsize(a);
	data2 = array_ptrsize(b);
	for (i = 0; i < size; i++) {
		if (data1[i] != data2[i])
			return 0;
	}

	return 1;
}

int array_get_element_type_(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_ARRAY);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &pos);
	return type_object_(ret, pos);
}

int array_get_vector_length_(addr pos, int fill, size_t *ret)
{
	struct array_struct *str;

	if (! array_vector_p(pos)) {
		*ret = 0;
		return TypeError_(pos, VECTOR);
	}
	str = ArrayInfoStruct(pos);
	*ret = fill? str->front: str->size;
	return 0;
}

void array_get_rowlength(addr pos, size_t *ret)
{
	struct array_struct *str;

	CheckType(pos, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(pos);
	*ret = (str->fillpointer)? str->front: str->size;
}


/************************************************************
 *  array_adjust.c
 ************************************************************/

/*
 *  adjust-array
 */
static int array_adjust_move_default_(addr pos, size_t index, addr value)
{
	struct array_struct *str;

	/* :initial-element */
	if (value != Unbound)
		return array_set_(pos, index, value);

	/* default value */
	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_CHARACTER:
			return array_set_character_(pos, index, 0);

#ifdef LISP_DEBUG
		case ARRAY_TYPE_BIT:
			return array_set_bit_(pos, index, 1);

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			fixnum_heap(&value, 88);
			return array_set_(pos, index, value);

		case ARRAY_TYPE_SINGLE_FLOAT:
			return array_set_single_(pos, index, nanf(""));

		case ARRAY_TYPE_DOUBLE_FLOAT:
			return array_set_double_(pos, index, nan(""));

		case ARRAY_TYPE_LONG_FLOAT:
			return array_set_long_(pos, index, nanl(""));
#endif

		default:
			return 0;
	}
}

static int array_index_dimension_(const size_t *data,
		const size_t *dimension, size_t depth, size_t *ret)
{
	size_t i, value;

	/*  [1] n1
	 *  [2] s2*... + n2
	 *  [3] s3*... + n3
	 *  [4] s4*... + n4
	 */
	if (depth == 0)
		return Result(ret, 1);
	if (depth == 1)
		return Result(ret, data[0]);

	value = data[0];
	for (i = 1; i < depth; i++) {
		if (multisafe_size(value, dimension[i], &value)) {
			*ret = 0;
			return fmte_("Too large index value.", NULL);
		}
		if (plussafe_size(value, data[i], &value)) {
			*ret = 0;
			return fmte_("Too large index value.", NULL);
		}
	}

	return Result(ret, value);
}

static int array_adjust_move_p(addr array, const size_t *data)
{
	struct array_struct *str;
	const size_t *bound;
	size_t size, i;

	str = ArrayInfoStruct(array);
	bound = array_ptrsize(array);
	size = str->dimension;
	for (i = 0; i < size; i++) {
		if (bound[i] <= data[i])
			return 0;
	}

	return 1;
}

static int array_adjust_move_element_(
		addr pos, addr array, addr initial, const size_t *data)
{
	struct array_struct *str1;
	const size_t *data1, *data2;
	size_t index1, index2, depth;

	str1 = ArrayInfoStruct(pos);
	data1 = array_ptrsize(pos);
	depth = str1->dimension;
	Return(array_index_dimension_(data, data1, depth, &index1));
	if (! array_adjust_move_p(array, data))
		return array_adjust_move_default_(pos, index1, initial);
	data2 = array_ptrsize(array);
	Return(array_index_dimension_(data, data2, depth, &index2));
	return array_setget_(pos, index1, array, index2);
}

static int array_adjust_move_depth_(
		addr pos, addr array, addr initial, size_t *data, size_t depth)
{
	struct array_struct *str;
	const size_t *data1;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	if (str->dimension <= depth)
		return array_adjust_move_element_(pos, array, initial, data);

	data1 = array_ptrsize(pos);
	size = data1[depth];
	for (i = 0; i < size; i++) {
		data[depth] = i;
		Return(array_adjust_move_depth_(pos, array, initial, data, depth + 1));
	}

	return 0;
}

static int array_adjust_move_(addr pos, addr array, addr initial)
{
	struct array_struct *str;
	size_t size, *data;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	str = ArrayInfoStruct(pos);
	size = str->dimension;
	push_local(local, &stack);
	data = (size_t *)lowlevel_local(local, IdxSize * size);
	memset(data, 0, IdxSize * size);
	Return(array_adjust_move_depth_(pos, array, initial, data, 0));
	rollback_local(local, stack);

	return 0;
}

static int array_adjust_notnot_(addr pos, addr array, addr initial, addr contents)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Return(array_allocate_(NULL, pos, str));
	if (contents == Unbound)
		return array_adjust_move_(pos, array, initial);
	else
		return array_make_initial_(pos, initial, contents);
}

static int array_adjust_make_(addr pos, addr array, addr initial, addr contents)
{
	int check1, check2;
	struct array_struct *str1, *str2;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	check1 = str1->displaced;
	check2 = str2->displaced;
	if (check1 && check2) {
		/* displaced -> displaced */
		return array_make_initial_(pos, initial, contents);
	}
	else if (check1) {
		/* not -> displaced */
		return array_make_initial_(pos, initial, contents);
	}
	else if (check2) {
		/* displaced -> not */
		return array_adjust_notnot_(pos, array, initial, contents);
	}
	else {
		/* not-displaced -> not-displaced */
		return array_adjust_notnot_(pos, array, initial, contents);
	}
}

static int array_adjust_element_type_(addr pos, addr type, addr array)
{
	enum ARRAY_TYPE value;
	int size;
	struct array_struct *str1, *str2;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	if (type == Unbound) {
		str1->type = str2->type;
		str1->bytesize = str2->bytesize;
	}
	else {
		Return(upgraded_array_value_(type, &value, &size));
		if (! array_equal_type(str2, value, size))
			return fmte_(":element-type ~S must be equal to base array.", type, NULL);
		str1->type = value;
		str1->bytesize = size;
	}
	array_set_type(pos);

	return 0;
}

static void array_adjust_adjustable(addr pos, addr array)
{
	ArrayInfoStruct(pos)->adjustable = ArrayInfoStruct(array)->adjustable;
}

static int array_adjust_fillpointer_(addr pos, addr array, addr fill)
{
	int check;
	struct array_struct *str1;
	struct array_struct *str2;
	addr type;
	size_t size;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	/* nil */
	if (fill == Nil) {
		if (str2->fillpointer == 0)
			return 0;
		str1->fillpointer = str2->fillpointer;
		str1->front = str2->front;
		goto fill_check;
	}
	/* t */
	if (fill == T) {
		str1->fillpointer = 1;
		str1->front = str1->size;
		goto fill_check;
	}
	/* integer */
	if (integerp(fill)) {
		Return(minusp_integer_(fill, &check));
		if (check)
			return fmte_("fill-pointer ~A must be a non-negative integer.", fill, NULL);
		if (GetIndex_integer(fill, &size))
			return fmte_("fill-pointer ~A is too large.", fill, NULL);
		str1->fillpointer = 1;
		str1->front = size;
		goto fill_check;
	}
	/* type error */
	return fmte_("Invalid fill-pointer value ~S.", fill, NULL);

fill_check:
	if (str2->fillpointer == 0) {
		GetTypeTable(&type, Array);
		return call_type_error_va_(NULL, array, type,
				"The argument ~S must be a fill-pointer array.", array, NULL);
	}
	if (str1->dimension != 1)
		return fmte_("fill-pointer array must be a 1 dimensional.", NULL);
	if (str1->size < str1->front) {
		return fmte_("fill-pointer ~A must be less than array size ~A.",
				intsizeh(str1->front), intsizeh(str1->size), NULL);
	}

	return 0;
}

static int array_adjust_dimension_(addr pos, addr array)
{
	struct array_struct *str1;
	struct array_struct *str2;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	if (str1->dimension != str2->dimension) {
		return fmte_("Array rank ~S must be equal to base array ~S.",
				intsizeh(str1->dimension),
				intsizeh(str2->dimension), NULL);
	}

	return 0;
}

static void array_adjust_replace(addr pos, addr array)
{
	struct array_struct *str1, *str2;
	int i;
	addr temp;

	str1 = ArrayInfoStruct(array);
	str2 = ArrayInfoStruct(pos);
	*str1 = *str2;

	for (i = 0; i < ARRAY_INDEX_SIZE; i++) {
		GetArrayInfo(pos, i, &temp);
		SetArrayInfo(array, i, temp);
	}
}

static void array_adjust_result(addr pos, addr array, addr *ret)
{
	if (array_adjustable_p(array)) {
		array_adjust_replace(pos, array);
		*ret = array;
	}
	else {
		*ret = pos;
	}
}

static int array_adjust_arraytype_(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fill, addr displaced, addr offset)
{
	addr pos;

	CheckType(array, LISPTYPE_ARRAY);
	array_empty_heap(&pos);
	Return(array_adjust_element_type_(pos, type, array));
	array_set_element_size(pos);
	Return(array_set_dimension_(pos, dimension));
	array_adjust_adjustable(pos, array);
	Return(array_adjust_fillpointer_(pos, array, fill));
	Return(array_set_displaced_(pos, displaced, offset));
	array_set_simple(pos);
	Return(array_adjust_dimension_(pos, array));
	Return(array_adjust_make_(pos, array, initial, contents));
	array_adjust_result(pos, array, ret);

	return 0;
}


/*
 *  array_adjust_simple
 */
static int array_adjust_simple_contents_list_(addr pos, size_t size, addr list)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		Return_getcons(list, &value, &list);
		Return(setelt_sequence_(pos, i, value));
	}

	return 0;
}

static int array_adjust_simple_contents_sequence_(addr pos, size_t size, addr x)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		Return(getelt_sequence_(NULL, x, i, &value));
		Return(setelt_sequence_(pos, i, value));
	}

	return 0;
}

static int array_adjust_simple_contents_(addr pos, size_t size, addr contents)
{
	size_t len;

	Return(length_sequence_(contents, 1, &len));
	if (size != len)
		return fmte_("Mismatch :displaced-to ~S length.", pos, NULL);
	if (listp(contents))
		return array_adjust_simple_contents_list_(pos, size, contents);
	else
		return array_adjust_simple_contents_sequence_(pos, size, contents);
}

static int array_adjust_simple_default_(addr pos, size_t index, addr initial)
{
	/* :initial-element */
	if (initial != Unbound)
		return setelt_sequence_(pos, index, initial);

	/* default value */
	switch (GetType(pos)) {
		case LISPTYPE_STRING:
			return strvect_setc_(pos, index, 0);

#ifdef LISP_DEBUG
		case LISPTYPE_BITVECTOR:
			return bitmemory_setint_(pos, index, 1);

		case LISPTYPE_VECTOR:
			setarray(pos, index, T);
			break;
#endif

		default:
			break;
	}

	return 0;
}

static int array_adjust_simple_move_(addr pos, addr array, size_t size, addr initial)
{
	addr value;
	size_t check, i;

	Return(length_sequence_(array, 1, &check));
	for (i = 0; i < size; i++) {
		if (i < check) {
			Return(getelt_sequence_(NULL, array, i, &value));
			Return(setelt_sequence_(pos, i, value));
		}
		else {
			Return(array_adjust_simple_default_(pos, i, initial));
		}
	}

	return 0;
}

static int array_adjust_simple_(
		addr pos, addr array, size_t size, addr initial, addr contents)
{
	if (contents != Unbound)
		return array_adjust_simple_contents_(pos, size, contents);
	else
		return array_adjust_simple_move_(pos, array, size, initial);
}


/*
 *  array_adjust_sequence
 */
static int array_adjust_vector_type_(addr pos, addr type, enum ARRAY_TYPE check)
{
	int size;
	enum ARRAY_TYPE value;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (type == Unbound) {
		str->type = check;
		str->bytesize = 0;
	}
	else {
		Return(upgraded_array_value_(type, &value, &size));
		if (check != value)
			return fmte_(":element-type ~S must be equal to base array.", type, NULL);
		str->type = value;
		str->bytesize = 0;
	}
	array_set_type(pos);

	return 0;
}

static int array_adjust_vector_move_(addr pos, addr array, addr initial)
{
	struct array_struct *str;
	addr value;
	size_t size1, size2, i;

	str = ArrayInfoStruct(pos);
	size1 = str->size;
	Return(length_sequence_(array, 0, &size2));
	for (i = 0; i < size1; i++) {
		if (i < size2) {
			Return(getelt_sequence_(NULL, array, i, &value));
			Return(setelt_sequence_(pos, i, value));
		}
		else {
			Return(array_adjust_move_default_(pos, i, initial));
		}
	}

	return 0;
}

static int array_adjust_vector_not_(
		addr pos, addr array, addr initial, addr contents)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Return(array_allocate_(NULL, pos, str));
	if (contents == Unbound)
		return array_adjust_vector_move_(pos, array, initial);
	else
		return array_make_initial_(pos, initial, contents);
}

static int array_adjust_vector_make_(addr pos, addr array, addr initial, addr contents)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->displaced) {
		/* not -> displaced */
		return array_make_initial_(pos, initial, contents);
	}
	else {
		/* not-displaced -> not-displaced */
		return array_adjust_vector_not_(pos, array, initial, contents);
	}
}

static void array_adjust_vector_adjustable(addr pos)
{
	ArrayInfoStruct(pos)->adjustable = 0;
}

static int array_adjust_vector_fillpointer_(addr pos, addr array, addr fill)
{
	addr type;

	if (fill != Nil) {
		GetTypeTable(&type, Array);
		return call_type_error_va_(NULL, array, type,
				"The argument ~S must be a fill-pointer array.", array, NULL);
	}
	ArrayInfoStruct(pos)->fillpointer = 0;
	return 0;
}

static int array_adjust_sequence_(addr *ret, addr array, addr dimension,
		addr type, enum ARRAY_TYPE type_value,
		addr initial, addr contents, addr fill, addr displaced, addr offset)
{
	addr pos;

	array_empty_heap(&pos);
	Return(array_adjust_vector_type_(pos, type, type_value));
	array_set_element_size(pos);
	Return(array_set_dimension_(pos, dimension));
	array_adjust_vector_adjustable(pos);
	Return(array_adjust_vector_fillpointer_(pos, array, fill));
	Return(array_set_displaced_(pos, displaced, offset));
	array_set_simple(pos);
	Return(array_adjust_vector_make_(pos, array, initial, contents));

	return Result(ret, pos);
}


/*
 *  array_adjust_array
 */
static int array_adjust_vector_check_(addr pos, size_t *ret)
{
	if (pos == Nil) {
		*ret = 0;
		return fmte_("Array rank must be a 1, but 0.", NULL);
	}
	if (singlep(pos))
		GetCar(pos, &pos);
	if (integerp(pos)) {
		if (GetIndex_integer(pos, ret)) {
			*ret = 0;
			return fmte_("Dimension ~A is too large.", pos, NULL);
		}
		return 0;
	}

	/* error */
	*ret = 0;
	if (consp(pos))
		return fmte_("Array rank must be a 1.", NULL);
	else
		return fmte_("Invalid pos type ~S.", pos, NULL);
}

static int array_adjust_simple_check(addr pos, addr fill, addr displaced)
{
	if (fill != Nil || displaced != Nil)
		return 0;
	if (! arrayp(pos))
		return 1;
	return ArrayInfoStruct(pos)->adjustable == 0;
}

static int array_adjust_vector_(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fill, addr displaced, addr offset)
{
	addr pos;
	size_t size;

	Return(array_adjust_vector_check_(dimension, &size));
	if (array_adjust_simple_check(array, fill, displaced)) {
		vector_heap(&pos, size);
		Return(array_adjust_simple_(pos, array, size, initial, contents));
	}
	else {
		Return(array_adjust_sequence_(&pos, array, dimension, type,
					ARRAY_TYPE_T, initial, contents, fill, displaced, offset));
	}

	return Result(ret, pos);
}

static int array_adjust_string_(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fill, addr displaced, addr offset)
{
	addr pos;
	size_t size;

	Return(array_adjust_vector_check_(dimension, &size));
	if (initial != Unbound && characterp(initial) == 0) {
		*ret = Nil;
		return fmte_(":initial-element ~S must be a character type.", initial, NULL);
	}
	if (array_adjust_simple_check(array, fill, displaced)) {
		strvect_heap(&pos, size);
		Return(array_adjust_simple_(pos, array, size, initial, contents));
	}
	else {
		Return(array_adjust_sequence_(&pos, array, dimension, type,
					ARRAY_TYPE_CHARACTER, initial, contents, fill, displaced, offset));
	}

	return Result(ret, pos);
}

static int array_adjust_bitvector_(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fill, addr displaced, addr offset)
{
	addr pos;
	size_t size;

	Return(array_adjust_vector_check_(dimension, &size));
	if (initial != Unbound && bitp(initial) == 0) {
		*ret = Nil;
		return fmte_(":initial-element ~S must be a bit type (0 or 1).", initial, NULL);
	}
	if (array_adjust_simple_check(array, fill, displaced)) {
		bitmemory_heap(&pos, size);
		Return(array_adjust_simple_(pos, array, size, initial, contents));
	}
	else {
		Return(array_adjust_sequence_(&pos, array, dimension, type,
					ARRAY_TYPE_BIT, initial, contents, fill, displaced, offset));
	}

	return Result(ret, pos);
}

int array_adjust_array_(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fill, addr displaced, addr offset)
{
	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			return array_adjust_arraytype_(ret, array, dimension,
					type, initial, contents, fill, displaced, offset);

		case LISPTYPE_VECTOR:
			return array_adjust_vector_(ret, array, dimension,
					type, initial, contents, fill, displaced, offset);

		case LISPTYPE_STRING:
			return array_adjust_string_(ret, array, dimension,
					type, initial, contents, fill, displaced, offset);

		case LISPTYPE_BITVECTOR:
			return array_adjust_bitvector_(ret, array, dimension,
					type, initial, contents, fill, displaced, offset);

		default:
			*ret = Nil;
			return TypeError_(array, ARRAY);
	}
}


/************************************************************
 *  array_coerce.c
 ************************************************************/

/*
 *  make
 */
static void array_coerce_type_struct(addr pos, addr array,
		enum ARRAY_TYPE type, unsigned size)
{
	addr value;
	struct array_struct *str1, *str2;

	/* struct */
	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	str1->simple = 1;
	str1->adjustable = 0;
	str1->fillpointer = 0;
	str1->displaced = 0;
	str1->dimension = str2->dimension;
	str1->offset = 0;
	str1->size = str1->front = str2->front; /* fill-pointer */
	SetArrayInfo(pos, ARRAY_INDEX_DISPLACED, Nil);
	/* type */
	str1->type = type;
	str1->bytesize = size;
	upgraded_array_object(str1->type, str1->bytesize, &value);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, value);
	array_set_element_size(pos);
}

static int array_coerce_type_heap_(addr *ret, addr array,
		enum ARRAY_TYPE type, unsigned size)
{
	addr pos;
	struct array_struct *str;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	array_coerce_type_struct(pos, array, type, size);
	/* dimension */
	Return(array_size_copy_(NULL, pos, array));
	/* allocate */
	str = ArrayInfoStruct(pos);
	Return(array_allocate_(NULL, pos, str));
	/* result */
	return Result(ret, pos);
}

int array_coerce_t_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_T, 0);
}

int array_coerce_bit_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_BIT, 0);
}

int array_coerce_character_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_CHARACTER, 0);
}

int array_coerce_signed8_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_SIGNED, 8);
}

int array_coerce_signed16_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_SIGNED, 16);
}

int array_coerce_signed32_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_SIGNED, 32);
}

#ifdef LISP_64BIT
int array_coerce_signed64_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_SIGNED, 64);
}
#endif

int array_coerce_unsigned8_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_UNSIGNED, 8);
}

int array_coerce_unsigned16_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_UNSIGNED, 16);
}

int array_coerce_unsigned32_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_UNSIGNED, 32);
}

#ifdef LISP_64BIT
int array_coerce_unsigned64_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_UNSIGNED, 64);
}
#endif

int array_coerce_single_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_SINGLE_FLOAT, 0);
}

int array_coerce_double_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_DOUBLE_FLOAT, 0);
}

int array_coerce_long_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_LONG_FLOAT, 0);
}

static int vector_coerce_type_heap_(addr *ret,
		enum ARRAY_TYPE type1, unsigned type2, size_t size)
{
	addr pos;
	struct array_struct *str;

	Return(array_va_heap_(&pos, size, 0));
	str = ArrayInfoStruct(pos);
	str->type = type1;
	str->bytesize = type2;
	Return(array_build_(pos));

	return Result(ret, pos);
}

int vector_coerce_signed8_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_SIGNED, 8, size);
}

int vector_coerce_signed16_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_SIGNED, 16, size);
}

int vector_coerce_signed32_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_SIGNED, 32, size);
}

#ifdef LISP_64BIT
int vector_coerce_signed64_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_SIGNED, 64, size);
}
#endif

int vector_coerce_unsigned8_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_UNSIGNED, 8, size);
}

int vector_coerce_unsigned16_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_UNSIGNED, 16, size);
}

int vector_coerce_unsigned32_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_UNSIGNED, 32, size);
}

#ifdef LISP_64BIT
int vector_coerce_unsigned64_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_UNSIGNED, 64, size);
}
#endif

int vector_coerce_single_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_SINGLE_FLOAT, 0, size);
}

int vector_coerce_double_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_DOUBLE_FLOAT, 0, size);
}

int vector_coerce_long_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_LONG_FLOAT, 0, size);
}


/*
 *  array_coerce_bit
 */
int array_coerce_bit_t_(addr pos, int *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			if (v == 0) {
				*rv = 0;
				return Result(ret, 0);
			}
			if (v == 1) {
				*rv = 1;
				return Result(ret, 0);
			}
			break;

		case LISPTYPE_BIGNUM:
			if (zerop_bignum(pos)) {
				*rv = 0;
				return Result(ret, 0);
			}
			if (equal_value_bignum(pos, SignPlus, 1)) {
				*rv = 1;
				return Result(ret, 0);
			}
			break;

		case LISPTYPE_RATIO:
			if (zerop_ratio(pos)) {
				*rv = 0;
				return Result(ret, 0);
			}
			if (equal_value_ratio(pos, SignPlus, 1, 1)) {
				*rv = 1;
				return Result(ret, 0);
			}
			break;

		default:
			break;
	}

	return Result(ret, 1);
}

static int array_coerce_bit_signed_(struct array_value *ptr, int *rv, int *ret)
{
	int8_t s8;
	int16_t s16;
	int32_t s32;
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			s8 = ptr->value.signed8;
			if (s8 == 0 || s8 == 1) {
				*rv = (int)s8;
				return Result(ret, 0);
			}
			break;

		case 16:
			s16 = ptr->value.signed16;
			if (s16 == 0 || s16 == 1) {
				*rv = (int)s16;
				return Result(ret, 0);
			}
			break;

		case 32:
			s32 = ptr->value.signed32;
			if (s32 == 0 || s32 == 1) {
				*rv = (int)s32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (s64 == 0 || s64 == 1) {
				*rv = (int)s64;
				return Result(ret, 0);
			}
			break;
#endif
		default:
			break;
	}

	return Result(ret, 1);
}

static int array_coerce_bit_unsigned_(struct array_value *ptr, int *rv, int *ret)
{
	uint8_t s8;
	uint16_t s16;
	uint32_t s32;
#ifdef LISP_64BIT
	uint64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			s8 = ptr->value.unsigned8;
			if (s8 == 0 || s8 == 1) {
				*rv = (int)s8;
				return Result(ret, 0);
			}
			break;

		case 16:
			s16 = ptr->value.unsigned16;
			if (s16 == 0 || s16 == 1) {
				*rv = (int)s16;
				return Result(ret, 0);
			}
			break;

		case 32:
			s32 = ptr->value.unsigned32;
			if (s32 == 0 || s32 == 1) {
				*rv = (int)s32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.unsigned64;
			if (s64 == 0 || s64 == 1) {
				*rv = (int)s64;
				return Result(ret, 0);
			}
			break;
#endif
		default:
			break;
	}

	return Result(ret, 1);
}

int array_coerce_bit_(addr pos, size_t i, int *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_bit_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (int)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_bit_signed_(&value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_bit_unsigned_(&value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_character
 */
int array_coerce_character_t_(addr pos, unicode *rv, int *ret)
{
	size_t size;

	if (symbolp(pos)) {
		GetNameSymbol(pos, &pos);
	}
	if (stringp(pos)) {
		string_length(pos, &size);
		if (size != 1)
			goto novalue;
		Return(string_getc_(pos, 0, rv));
		return Result(ret, 0);
	}
	if (characterp(pos)) {
		GetCharacter(pos, rv);
		return Result(ret, 0);
	}

novalue:
	*rv = 0;
	return Result(ret, 1);
}

int array_coerce_character_(addr pos, size_t i, unicode *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_character_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_CHARACTER:
			*rv = value.value.character;
			return Result(ret, 0);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_signed8
 */
int array_coerce_signed8_t_(addr pos, int8_t *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				goto novalue;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v))
				goto novalue;
			break;

		default:
			goto novalue;
	}

	if (INT8_MIN <= v && v <= INT8_MAX) {
		*rv = (int8_t)v;
		return Result(ret, 0);
	}

novalue:
	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_signed8_signed_(addr pos,
		size_t i, struct array_value *ptr, int8_t *rv, int *ret)
{
	int16_t s16;
	int32_t s32;
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = ptr->value.signed8;
			return Result(ret, 0);

		case 16:
			s16 = ptr->value.signed16;
			if (INT8_MIN <= s16 && s16 <= INT8_MAX) {
				*rv = (int8_t)s16;
				return Result(ret, 0);
			}
			break;

		case 32:
			s32 = ptr->value.signed32;
			if (INT8_MIN <= s32 && s32 <= INT8_MAX) {
				*rv = (int8_t)s32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (INT8_MIN <= s64 && s64 <= INT8_MAX) {
				*rv = (int8_t)s64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_signed8_unsigned_(addr pos,
		size_t i, struct array_value *ptr, int8_t *rv, int *ret)
{
	uint8_t u8;
	uint16_t u16;
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			u8 = ptr->value.unsigned8;
			if (u8 <= INT8_MAX) {
				*rv = (int8_t)u8;
				return Result(ret, 0);
			}
			break;

		case 16:
			u16 = ptr->value.unsigned16;
			if (u16 <= INT8_MAX) {
				*rv = (int8_t)u16;
				return Result(ret, 0);
			}
			break;

		case 32:
			u32 = ptr->value.unsigned32;
			if (u32 <= INT8_MAX) {
				*rv = (int8_t)u32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= INT8_MAX) {
				*rv = (int8_t)u64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

int array_coerce_signed8_(addr pos, size_t i, int8_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_signed8_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (int8_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_signed8_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_signed8_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_signed16
 */
int array_coerce_signed16_t_(addr pos, int16_t *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				goto novalue;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v))
				goto novalue;
			break;

		default:
			goto novalue;
	}

	if (INT16_MIN <= v && v <= INT16_MAX) {
		*rv = (int16_t)v;
		return Result(ret, 0);
	}

novalue:
	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_signed16_signed_(addr pos,
		size_t i, struct array_value *ptr, int16_t *rv, int *ret)
{
	int32_t s32;
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = (int16_t)ptr->value.signed8;
			return Result(ret, 0);

		case 16:
			*rv = ptr->value.signed16;
			return Result(ret, 0);

		case 32:
			s32 = ptr->value.signed32;
			if (INT16_MIN <= s32 && s32 <= INT16_MAX) {
				*rv = (int16_t)s32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (INT16_MIN <= s64 && s64 <= INT16_MAX) {
				*rv = (int16_t)s64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_signed16_unsigned_(addr pos,
		size_t i, struct array_value *ptr, int16_t *rv, int *ret)
{
	uint16_t u16;
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = (int16_t)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			u16 = ptr->value.unsigned16;
			if (u16 <= INT16_MAX) {
				*rv = (int16_t)u16;
				return Result(ret, 0);
			}
			break;

		case 32:
			u32 = ptr->value.unsigned32;
			if (u32 <= INT16_MAX) {
				*rv = (int16_t)u32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= INT16_MAX) {
				*rv = (int16_t)u64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

int array_coerce_signed16_(addr pos, size_t i, int16_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_signed16_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (int16_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_signed16_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_signed16_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_signed32
 */
int array_coerce_signed32_t_(addr pos, int32_t *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				goto novalue;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v))
				goto novalue;
			break;

		default:
			goto novalue;
	}

	if (INT32_MIN <= v && v <= INT32_MAX) {
		*rv = (int32_t)v;
		return Result(ret, 0);
	}

novalue:
	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_signed32_signed_(addr pos,
		size_t i, struct array_value *ptr, int32_t *rv, int *ret)
{
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = (int32_t)ptr->value.signed8;
			return Result(ret, 0);

		case 16:
			*rv = (int32_t)ptr->value.signed16;
			return Result(ret, 0);

		case 32:
			*rv = ptr->value.signed32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (INT32_MIN <= s64 && s64 <= INT32_MAX) {
				*rv = (int32_t)s64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_signed32_unsigned_(addr pos,
		size_t i, struct array_value *ptr, int32_t *rv, int *ret)
{
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = (int32_t)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = (int32_t)ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			u32 = ptr->value.unsigned32;
			if (u32 <= INT32_MAX) {
				*rv = (int32_t)u32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= INT32_MAX) {
				*rv = (int32_t)u64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

int array_coerce_signed32_(addr pos, size_t i, int32_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_signed32_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (int32_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_signed32_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_signed32_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


#ifdef LISP_64BIT
/*
 *  array_coerce_signed64
 */
int array_coerce_signed64_t_(addr pos, int64_t *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				goto novalue;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v))
				goto novalue;
			break;

		default:
			goto novalue;
	}
	*rv = (int64_t)v;
	return Result(ret, 0);

novalue:
	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_signed64_signed_(addr pos,
		size_t i, struct array_value *ptr, int64_t *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (int64_t)ptr->value.signed8;
			return Result(ret, 0);

		case 16:
			*rv = (int64_t)ptr->value.signed16;
			return Result(ret, 0);

		case 32:
			*rv = (int64_t)ptr->value.signed32;
			return Result(ret, 0);

		case 64:
			*rv = ptr->value.signed64;
			return Result(ret, 0);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}

static int array_coerce_signed64_unsigned_(addr pos,
		size_t i, struct array_value *ptr, int64_t *rv, int *ret)
{
	uint64_t u64;

	switch (ptr->size) {
		case 8:
			*rv = (int64_t)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = (int64_t)ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			*rv = (int64_t)ptr->value.unsigned32;
			return Result(ret, 0);

		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= INT64_MAX) {
				*rv = (int64_t)u64;
				return Result(ret, 0);
			}
			break;

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

int array_coerce_signed64_(addr pos, size_t i, int64_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_signed64_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (int64_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_signed64_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_signed64_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}
#endif


/*
 *  array_coerce_unsigned8
 */
int array_coerce_unsigned8_t_(addr pos, uint8_t *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				goto novalue;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v))
				goto novalue;
			break;

		default:
			goto novalue;
	}

	if (0 <= v && v <= UINT8_MAX) {
		*rv = (uint8_t)v;
		return Result(ret, 0);
	}

novalue:
	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_unsigned8_signed_(addr pos,
		size_t i, struct array_value *ptr, uint8_t *rv, int *ret)
{
	int8_t s8;
	int16_t s16;
	int32_t s32;
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			s8 = ptr->value.signed8;
			if (0 <= s8) {
				*rv = (uint8_t)s8;
				return Result(ret, 0);
			}
			break;

		case 16:
			s16 = ptr->value.signed16;
			if (0 <= s16 && s16 <= UINT8_MAX) {
				*rv = (uint8_t)s16;
				return Result(ret, 0);
			}
			break;

		case 32:
			s32 = ptr->value.signed32;
			if (0 <= s32 && s32 <= UINT8_MAX) {
				*rv = (uint8_t)s32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (0 <= s64 && s64 <= UINT8_MAX) {
				*rv = (uint8_t)s64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_unsigned8_unsigned_(addr pos,
		size_t i, struct array_value *ptr, uint8_t *rv, int *ret)
{
	uint16_t u16;
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			u16 = ptr->value.unsigned16;
			if (u16 <= UINT8_MAX) {
				*rv = (uint8_t)u16;
				return Result(ret, 0);
			}
			break;

		case 32:
			u32 = ptr->value.unsigned32;
			if (u32 <= UINT8_MAX) {
				*rv = (uint8_t)u32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= UINT8_MAX) {
				*rv = (uint8_t)u64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

int array_coerce_unsigned8_(addr pos, size_t i, uint8_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_unsigned8_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (uint8_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_unsigned8_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_unsigned8_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_unsigned16
 */
int array_coerce_unsigned16_t_(addr pos, uint16_t *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				goto novalue;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v))
				goto novalue;
			break;

		default:
			goto novalue;
	}

	if (0 <= v && v <= UINT16_MAX) {
		*rv = (uint16_t)v;
		return Result(ret, 0);
	}

novalue:
	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_unsigned16_signed_(addr pos,
		size_t i, struct array_value *ptr, uint16_t *rv, int *ret)
{
	int8_t s8;
	int16_t s16;
	int32_t s32;
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			s8 = ptr->value.signed8;
			if (0 <= s8) {
				*rv = (uint16_t)s8;
				return Result(ret, 0);
			}
			break;

		case 16:
			s16 = ptr->value.signed16;
			if (0 <= s16) {
				*rv = (uint16_t)s16;
				return Result(ret, 0);
			}
			break;

		case 32:
			s32 = ptr->value.signed32;
			if (0 <= s32 && s32 <= UINT16_MAX) {
				*rv = (uint16_t)s32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (0 <= s64 && s64 <= UINT16_MAX) {
				*rv = (uint16_t)s64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_unsigned16_unsigned_(addr pos,
		size_t i, struct array_value *ptr, uint16_t *rv, int *ret)
{
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = (uint16_t)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			u32 = ptr->value.unsigned32;
			if (u32 <= UINT16_MAX) {
				*rv = (uint16_t)u32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= UINT16_MAX) {
				*rv = (uint16_t)u64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

int array_coerce_unsigned16_(addr pos, size_t i, uint16_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_unsigned16_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (uint16_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_unsigned16_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_unsigned16_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_unsigned32
 */
#ifdef LISP_64BIT
int array_coerce_unsigned32_t_(addr pos, uint32_t *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				goto novalue;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v))
				goto novalue;
			break;

		default:
			goto novalue;
	}

	if (0 <= v && v <= UINT32_MAX) {
		*rv = (uint32_t)v;
		return Result(ret, 0);
	}

novalue:
	*rv = 0;
	return Result(ret, 1);
}
#else
int array_coerce_unsigned32_t_(addr pos, uint32_t *rv, int *ret)
{
	int sign;
	fixnum v;
	fixed u;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			if (0 <= v) {
				*rv = (uint32_t)v;
				return Result(ret, 0);
			}
			break;

		case LISPTYPE_BIGNUM:
			if (getfixed1_bignum(pos, &sign, &u))
				break;
			if (IsMinus(sign) && (u != 0))
				break;
			*rv = (uint32_t)u;
			return Result(ret, 0);

		case LISPTYPE_RATIO:
			if (getfixed1_ratio(pos, &sign, &u))
				break;
			if (IsMinus(sign) && (u != 0))
				break;
			*rv = (uint32_t)u;
			return Result(ret, 0);

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}
#endif

static int array_coerce_unsigned32_signed_(addr pos,
		size_t i, struct array_value *ptr, uint32_t *rv, int *ret)
{
	int8_t s8;
	int16_t s16;
	int32_t s32;
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			s8 = ptr->value.signed8;
			if (0 <= s8) {
				*rv = (uint32_t)s8;
				return Result(ret, 0);
			}
			break;

		case 16:
			s16 = ptr->value.signed16;
			if (0 <= s16) {
				*rv = (uint32_t)s16;
				return Result(ret, 0);
			}
			break;

		case 32:
			s32 = ptr->value.signed32;
			if (0 <= s32) {
				*rv = (uint32_t)s32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (0 <= s64 && s64 <= UINT32_MAX) {
				*rv = (uint32_t)s64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_unsigned32_unsigned_(addr pos,
		size_t i, struct array_value *ptr, uint32_t *rv, int *ret)
{
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = (uint32_t)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = (uint32_t)ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			*rv = ptr->value.unsigned32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= UINT32_MAX) {
				*rv = (uint32_t)u64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

int array_coerce_unsigned32_(addr pos, size_t i, uint32_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_unsigned32_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (uint32_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_unsigned32_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_unsigned32_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_unsigned64
 */
#ifdef LISP_64BIT
int array_coerce_unsigned64_t_(addr pos, uint64_t *rv, int *ret)
{
	int sign;
	fixnum v;
	fixed u;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			if (0 <= v) {
				*rv = (uint64_t)v;
				return Result(ret, 0);
			}
			break;

		case LISPTYPE_BIGNUM:
			if (getfixed1_bignum(pos, &sign, &u))
				break;
			if (IsMinus(sign) && (u != 0))
				break;
			*rv = (uint64_t)u;
			return Result(ret, 0);

		case LISPTYPE_RATIO:
			if (getfixed1_ratio(pos, &sign, &u))
				break;
			if (IsMinus(sign) && (u != 0))
				break;
			*rv = (uint64_t)u;
			return Result(ret, 0);

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_unsigned64_signed_(addr pos,
		size_t i, struct array_value *ptr, uint64_t *rv, int *ret)
{
	int8_t s8;
	int16_t s16;
	int32_t s32;
	int64_t s64;

	switch (ptr->size) {
		case 8:
			s8 = ptr->value.signed8;
			if (0 <= s8) {
				*rv = (uint64_t)s8;
				return Result(ret, 0);
			}
			break;

		case 16:
			s16 = ptr->value.signed16;
			if (0 <= s16) {
				*rv = (uint64_t)s16;
				return Result(ret, 0);
			}
			break;

		case 32:
			s32 = ptr->value.signed32;
			if (0 <= s32) {
				*rv = (uint64_t)s32;
				return Result(ret, 0);
			}
			break;

		case 64:
			s64 = ptr->value.signed64;
			if (0 <= s64) {
				*rv = (uint64_t)s64;
				return Result(ret, 0);
			}
			break;

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_unsigned64_unsigned_(addr pos,
		size_t i, struct array_value *ptr, uint64_t *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (uint64_t)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = (uint64_t)ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			*rv = (uint64_t)ptr->value.unsigned32;
			return Result(ret, 0);

		case 64:
			*rv = ptr->value.unsigned64;
			return Result(ret, 0);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}

int array_coerce_unsigned64_(addr pos, size_t i, uint64_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_unsigned64_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (uint64_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_unsigned64_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_unsigned64_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}
#endif


/*
 *  array_coerce_single
 */
int array_coerce_single_t_(addr value, single_float *rv, int *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			*rv = single_float_fixnum(value);
			return Result(ret, 0);

		case LISPTYPE_BIGNUM:
			*ret = 0;
			return single_float_bignum_(value, rv);

		case LISPTYPE_RATIO:
			*ret = 0;
			return single_float_ratio_(value, rv);

		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(value, rv);
			return Result(ret, 0);

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = 0;
			return cast_ds_value_(value, rv);

		case LISPTYPE_LONG_FLOAT:
			*ret = 0;
			return cast_ls_value_(value, rv);

		default:
			*rv = 0.0f;
			return Result(ret, 1);
	}
}

static int array_coerce_single_signed_(const struct array_value *ptr,
		single_float *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (single_float)ptr->value.signed8;
			return Result(ret, 0);

		case 16:
			*rv = (single_float)ptr->value.signed16;
			return Result(ret, 0);

		case 32:
			*rv = (single_float)ptr->value.signed32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			*rv = (single_float)ptr->value.signed64;
			return Result(ret, 0);
#endif

		default:
			*rv = 0.0f;
			return Result(ret, 1);
	}
}

static int array_coerce_single_unsigned_(const struct array_value *ptr,
		single_float *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (single_float)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = (single_float)ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			*rv = (single_float)ptr->value.unsigned32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			*rv = (single_float)ptr->value.unsigned64;
			return Result(ret, 0);
#endif

		default:
			*rv = 0.0f;
			return Result(ret, 1);
	}
}

int array_coerce_single_(addr pos, size_t i, single_float *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_single_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (single_float)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_single_signed_(&value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_single_unsigned_(&value, rv, ret);

		case ARRAY_TYPE_SINGLE_FLOAT:
			*rv = value.value.single_value;
			return Result(ret, 0);

		case ARRAY_TYPE_DOUBLE_FLOAT:
			*ret = 0;
			return cast_ds_float_(value.value.double_value, rv);

		case ARRAY_TYPE_LONG_FLOAT:
			*ret = 0;
			return cast_ls_float_(value.value.long_value, rv);

		default:
			*rv = 0.0f;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_double
 */
int array_coerce_double_t_(addr value, double_float *rv, int *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			*rv = double_float_fixnum(value);
			return Result(ret, 0);

		case LISPTYPE_BIGNUM:
			*ret = 0;
			return double_float_bignum_(value, rv);

		case LISPTYPE_RATIO:
			*ret = 0;
			return double_float_ratio_(value, rv);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = 0;
			return cast_sd_value_(value, rv);

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(value, rv);
			return Result(ret, 0);

		case LISPTYPE_LONG_FLOAT:
			*ret = 0;
			return cast_ld_value_(value, rv);

		default:
			*rv = 0.0;
			return Result(ret, 1);
	}
}

static int array_coerce_double_signed_(const struct array_value *ptr,
		double_float *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (double_float)ptr->value.signed8;
			return Result(ret, 0);

		case 16:
			*rv = (double_float)ptr->value.signed16;
			return Result(ret, 0);

		case 32:
			*rv = (double_float)ptr->value.signed32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			*rv = (double_float)ptr->value.signed64;
			return Result(ret, 0);
#endif

		default:
			*rv = 0.0;
			return Result(ret, 1);
	}
}

static int array_coerce_double_unsigned_(const struct array_value *ptr,
		double_float *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (double_float)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = (double_float)ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			*rv = (double_float)ptr->value.unsigned32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			*rv = (double_float)ptr->value.unsigned64;
			return Result(ret, 0);
#endif

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}

int array_coerce_double_(addr pos, size_t i, double_float *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_double_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (double_float)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_double_signed_(&value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_double_unsigned_(&value, rv, ret);

		case ARRAY_TYPE_SINGLE_FLOAT:
			*ret = 0;
			return cast_sd_float_(value.value.single_value, rv);

		case ARRAY_TYPE_DOUBLE_FLOAT:
			*rv = value.value.double_value;
			return Result(ret, 0);

		case ARRAY_TYPE_LONG_FLOAT:
			*ret = 0;
			return cast_ld_float_(value.value.long_value, rv);

		default:
			*rv = 0.0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_long
 */
int array_coerce_long_t_(addr value, long_float *rv, int *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			*rv = long_float_fixnum(value);
			return Result(ret, 0);

		case LISPTYPE_BIGNUM:
			*ret = 0;
			return long_float_bignum_(value, rv);

		case LISPTYPE_RATIO:
			*ret = 0;
			return long_float_ratio_(value, rv);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = 0;
			return cast_sl_value_(value, rv);

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = 0;
			return cast_dl_value_(value, rv);

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(value, rv);
			return Result(ret, 0);

		default:
			*rv = 0.0L;
			return Result(ret, 1);
	}
}

static int array_coerce_long_signed_(const struct array_value *ptr,
		long_float *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (long_float)ptr->value.signed8;
			return Result(ret, 0);

		case 16:
			*rv = (long_float)ptr->value.signed16;
			return Result(ret, 0);

		case 32:
			*rv = (long_float)ptr->value.signed32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			*rv = (long_float)ptr->value.signed64;
			return Result(ret, 0);
#endif

		default:
			*rv = 0.0L;
			return Result(ret, 1);
	}
}

static int array_coerce_long_unsigned_(const struct array_value *ptr,
		long_float *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (long_float)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = (long_float)ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			*rv = (long_float)ptr->value.unsigned32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			*rv = (long_float)ptr->value.unsigned64;
			return Result(ret, 0);
#endif

		default:
			*rv = 0.0L;
			return Result(ret, 1);
	}
}

int array_coerce_long_(addr pos, size_t i, long_float *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_long_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (long_float)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_long_signed_(&value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_long_unsigned_(&value, rv, ret);

		case ARRAY_TYPE_SINGLE_FLOAT:
			*ret = 0;
			return cast_sl_float_(value.value.single_value, rv);

		case ARRAY_TYPE_DOUBLE_FLOAT:
			*ret = 0;
			return cast_dl_float_(value.value.double_value, rv);

		case ARRAY_TYPE_LONG_FLOAT:
			*rv = value.value.long_value;
			return Result(ret, 0);

		default:
			*rv = 0.0L;
			return Result(ret, 1);
	}
}


/*
 *  vector_coerce_bit
 */
int vector_coerce_bit_(addr pos, size_t i, int *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_bit_t_(pos, rv, ret);
}

int vector_coerce_character_(addr pos, size_t i, unicode *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_character_t_(pos, rv, ret);
}

int vector_coerce_signed8_(addr pos, size_t i, int8_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_signed8_t_(pos, rv, ret);
}

int vector_coerce_signed16_(addr pos, size_t i, int16_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_signed16_t_(pos, rv, ret);
}

int vector_coerce_signed32_(addr pos, size_t i, int32_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_signed32_t_(pos, rv, ret);
}

#ifdef LISP_64BIT
int vector_coerce_signed64_(addr pos, size_t i, int64_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_signed64_t_(pos, rv, ret);
}
#endif

int vector_coerce_unsigned8_(addr pos, size_t i, uint8_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_unsigned8_t_(pos, rv, ret);
}

int vector_coerce_unsigned16_(addr pos, size_t i, uint16_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_unsigned16_t_(pos, rv, ret);
}

int vector_coerce_unsigned32_(addr pos, size_t i, uint32_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_unsigned32_t_(pos, rv, ret);
}

#ifdef LISP_64BIT
int vector_coerce_unsigned64_(addr pos, size_t i, uint64_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_unsigned64_t_(pos, rv, ret);
}
#endif

int vector_coerce_single_(addr pos, size_t i, single_float *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_single_t_(pos, rv, ret);
}

int vector_coerce_double_(addr pos, size_t i, double_float *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_double_t_(pos, rv, ret);
}

int vector_coerce_long_(addr pos, size_t i, long_float *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_long_t_(pos, rv, ret);
}


/************************************************************
 *  array_copy.c
 ************************************************************/

/*
 *  arary_copy
 */
static void array_struct_copy(addr pos, addr array)
{
	struct array_struct *str1, *str2;
	addr temp;

	/* array */
	GetArrayInfo(array, ARRAY_INDEX_TYPE, &temp);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, temp);
	GetArrayInfo(array, ARRAY_INDEX_DISPLACED, &temp);
	SetArrayInfo(pos, ARRAY_INDEX_DISPLACED, temp);

	/* body */
	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	*str1 = *str2;
}

int array_size_copy_(LocalRoot local, addr pos, addr array)
{
	addr temp;
	size_t size;

	GetArrayInfo(array, ARRAY_INDEX_DIMENSION, &temp);
	if (temp != Nil) {
		size = ArrayInfoStruct(array)->dimension;
		Return(arraysize_copy_alloc_(local, &temp, temp, size));
	}
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, temp);

	return 0;
}

static void copy_array_general(LocalRoot local, addr *ret, addr pos, size_t size)
{
	addr one, temp;
	size_t i;

	arraygen_alloc(local, &one, size);
	for (i = 0; i < size; i++) {
		arraygen_get(pos, i, &temp);
		arraygen_set(one, i, temp);
	}
	*ret = one;
}

static void copy_array_specialized(LocalRoot local, addr *ret, addr pos, size_t size)
{
	addr one;
	void *data1;
	const void *data2;

	arrayspec_alloc(local, &one, size);
	data1 = (void *)arrayspec_ptr(one);
	data2 = (const void *)arrayspec_ptr(pos);
	memcpy(data1, data2, size);
}

static void array_memory_copy(LocalRoot local, addr pos, addr array)
{
	addr mem;
	struct array_struct *str;

	GetArrayInfo(array, ARRAY_INDEX_MEMORY, &mem);
	switch (GetType(mem)) {
		case LISPTYPE_BITVECTOR:
			bitmemory_copy_alloc(local, &mem, mem);
			break;

		case LISPSYSTEM_ARRAY_GENERAL:
			str = ArrayInfoStruct(array);
			copy_array_general(local, &mem, mem, str->size);
			break;

		case LISPSYSTEM_ARRAY_SPECIALIZED:
			str = ArrayInfoStruct(array);
			copy_array_specialized(local, &mem, mem, str->size * str->element);
			break;

		default:
			break;
	}
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, mem);
}

int array_copy_alloc_(LocalRoot local, addr *ret, addr array)
{
	addr pos;

	/* object */
	array_empty_alloc(local, &pos);
	/* element-type */
	array_struct_copy(pos, array);
	/* dimension */
	Return(array_size_copy_(local, pos, array));
	/* allocate */
	array_memory_copy(local, pos, array);
	/* result */
	return Result(ret, pos);
}

int array_copy_local_(LocalRoot local, addr *ret, addr array)
{
	Check(local == NULL, "local error");
	return array_copy_alloc_(local, ret, array);
}

int array_copy_heap_(addr *ret, addr array)
{
	return array_copy_alloc_(NULL, ret, array);
}


/************************************************************
 *  array_inplace.c
 ************************************************************/

/*
 *  arrayinplace_get
 */
static int arrayinplace_get_general_(addr mem, size_t index, struct array_value *value)
{
	arraygen_get(mem, index, &mem);
	value->value.object = mem;
	value->type = ARRAY_TYPE_T;
	value->size = 0;

	return 0;
}

static int arrayinplace_get_specialized_(
		addr pos, addr mem, size_t index, struct array_value *value)
{
	unsigned element;
	const byte *data;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->type == ARRAY_TYPE_BIT)
		return fmte_("Invaild array object, ~S.", pos, NULL);

	element = (unsigned)str->element;
	data = (const byte *)arrayspec_ptr(mem);
	data += element * index;
	value->type = str->type;
	value->size = str->bytesize;
	memcpy(&(value->value), data, element);

	return 0;
}

static int arrayinplace_get_memory_(
		addr pos, addr mem, size_t index, struct array_value *value)
{
	if (array_type(pos) == ARRAY_TYPE_T)
		return arrayinplace_get_general_(mem, index, value);
	else
		return arrayinplace_get_specialized_(pos, mem, index, value);
}

static int arrayinplace_get_t_(addr mem, size_t index, struct array_value *str)
{
	CheckType(mem, LISPTYPE_VECTOR);
	getarray(mem, index, &mem);
	str->value.object = mem;
	str->type = ARRAY_TYPE_T;
	str->size = 0;

	return 0;
}

static int arrayinplace_get_bit_(addr mem, size_t index, struct array_value *str)
{
	int value;

	CheckType(mem, LISPTYPE_BITVECTOR);
	Return(bitmemory_getint_(mem, index, &value));
	str->value.bit = value? 1: 0;
	str->type = ARRAY_TYPE_BIT;
	str->size = 0;

	return 0;
}

static int arrayinplace_get_character_(addr mem, size_t index, struct array_value *str)
{
	unicode c;

	Check(! stringp(mem), "type error");
	Return(string_getc_(mem, index, &c));
	str->value.character = c;
	str->type = ARRAY_TYPE_CHARACTER;
	str->size = 0;

	return 0;
}

static int arrayinplace_get_object_(
		addr pos, addr mem, size_t index, struct array_value *value)
{
	switch (array_type(pos)) {
		case ARRAY_TYPE_T:
			return arrayinplace_get_t_(mem, index, value);

		case ARRAY_TYPE_BIT:
			return arrayinplace_get_bit_(mem, index, value);

		case ARRAY_TYPE_CHARACTER:
			return arrayinplace_get_character_(mem, index, value);

		default:
			return fmte_("Invalid memory object ~S", pos, NULL);
	}
}

int arrayinplace_get_(addr pos, size_t index, struct array_value *str)
{
	int check;
	addr mem;

	Return(arraymemory_get_(pos, index, &mem, &index, &check));
	if (check)
		return arrayinplace_get_memory_(pos, mem, index, str);
	else
		return arrayinplace_get_object_(pos, mem, index, str);
}


/*
 *  arrayinplace_set
 */
static int arrayinplace_set_general_(
		addr mem, size_t index, const struct array_value *value)
{
	arraygen_set(mem, index, value->value.object);
	return 0;
}

static int arrayinplace_set_specialized_(
		addr pos, addr mem, size_t index, const struct array_value *value)
{
	unsigned element;
	byte *data;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->type == ARRAY_TYPE_BIT)
		return fmte_("Invaild array object, ~S.", pos, NULL);

	element = (unsigned)str->element;
	data = (byte *)arrayspec_ptr(mem);
	data += element * index;
	memcpy(data, &(value->value), element);

	return 0;
}

static int arrayinplace_set_memory_(
		addr pos, addr mem, size_t index, const struct array_value *value)
{
	if (array_type(pos) == ARRAY_TYPE_T)
		return arrayinplace_set_general_(mem, index, value);
	else
		return arrayinplace_set_specialized_(pos, mem, index, value);
}

static int arrayinplace_set_t_(addr mem, size_t index, const struct array_value *str)
{
	CheckType(mem, LISPTYPE_VECTOR);
	setarray(mem, index, str->value.object);
	return 0;
}

static int arrayinplace_set_bit_(addr mem, size_t index, const struct array_value *str)
{
	CheckType(mem, LISPTYPE_BITVECTOR);
	return bitmemory_setint_(mem, index, str->value.bit);
}

static int arrayinplace_set_character_(
		addr mem, size_t index, const struct array_value *str)
{
	Check(! stringp(mem), "type error");
	return string_setc_(mem, index, str->value.character);
}

static int arrayinplace_set_object_(
		addr pos, addr mem, size_t index, const struct array_value *str)
{
	switch (array_type(pos)) {
		case ARRAY_TYPE_T:
			return arrayinplace_set_t_(mem, index, str);

		case ARRAY_TYPE_BIT:
			return arrayinplace_set_bit_(mem, index, str);

		case ARRAY_TYPE_CHARACTER:
			return arrayinplace_set_character_(mem, index, str);

		default:
			return fmte_("Invalid memory object ~S", pos, NULL);
	}
}

int arrayinplace_set_(addr pos, size_t index, const struct array_value *str)
{
	int check;
	addr mem;

	Return(arraymemory_get_(pos, index, &mem, &index, &check));
	if (check)
		return arrayinplace_set_memory_(pos, mem, index, str);
	else
		return arrayinplace_set_object_(pos, mem, index, str);
}


/************************************************************
 *  array_make.c
 ************************************************************/

/*
 *  control
 */
static void array_set_type_value(addr pos, enum ARRAY_TYPE type, unsigned size)
{
	addr value;
	struct array_struct *str;

	upgraded_array_object(type, size, &value);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, value);
	str = ArrayInfoStruct(pos);
	str->type = type;
	str->bytesize = size;
}

void array_set_type(addr pos)
{
	struct array_struct *str;
	addr type;

	str = ArrayInfoStruct(pos);
	upgraded_array_object(str->type, str->bytesize, &type);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, type);
}

void array_set_element_size(addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_CHARACTER:
			str->element = sizeoft(unicode);
			break;

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			str->element = str->bytesize / 8;
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			str->element = sizeoft(single_float);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			str->element = sizeoft(double_float);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			str->element = sizeoft(long_float);
			break;

		default:
			str->element = 0;
			break;
	}
}


/*
 *  array-set-dimension
 */
static int array_getsize_(addr pos, size_t *ret)
{
	struct array_struct *str;
	size_t dimension, i, size;
	const size_t *data;

	str = ArrayInfoStruct(pos);
	dimension = str->dimension;
	data = array_ptrsize(pos);
	size = 1;
	for (i = 0; i < dimension; i++) {
		if (multisafe_size(size, data[i], &size))
			return fmte_("size overflow.", NULL);
	}

	return Result(ret, size);
}

static void array_set_dimension0(addr pos, size_t *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	str->dimension = 0;
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, Nil);
	*ret = 1;
}

static int array_set_dimension1_(addr pos, addr value, size_t *ret)
{
	int check;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Return(minusp_integer_(value, &check));
	if (check) {
		*ret = 0;
		return fmte_("Array index ~A must be a non-negative integer.", value, NULL);
	}
	if (GetIndex_integer(value, ret)) {
		*ret = 0;
		return fmte_("Array index ~A is too large.", value, NULL);
	}
	str->dimension = 1;
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, Nil);

	return 0;
}

static int array_set_dimension2_(addr pos, addr value, size_t *ret)
{
	int check;
	struct array_struct *str;
	addr temp, index;
	size_t size, *data, i;

	str = ArrayInfoStruct(pos);
	Return(length_list_safe_(value, &size));
	if (size == 1) {
		GetCar(value, &value);
		return array_set_dimension1_(pos, value, ret);
	}
	Return(arraysize_heap_(&temp, size));
	data = arraysize_ptr(temp);
	for (i = 0; value != Nil; i++) {
		GetCons(value, &index, &value);
		if (! integerp(index)) {
			*ret = 0;
			return fmte_("Array index ~A must be an integer.", index, NULL);
		}
		Return(minusp_integer_(index, &check));
		if (check) {
			*ret = 0;
			return fmte_("Array index ~A must be a non-negative integer.", index, NULL);
		}
		if (GetIndex_integer(index, &size)) {
			*ret = 0;
			return fmte_("Array index ~A is too large.", index, NULL);
		}
		data[i] = size;
	}
	str->dimension = i;
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, temp);
	return array_getsize_(pos, ret);
}

int array_set_dimension_(addr pos, addr value)
{
	struct array_struct *str;
	size_t size;

	if (value == Nil) {
		array_set_dimension0(pos, &size);
	}
	else if (integerp(value)) {
		Return(array_set_dimension1_(pos, value, &size));
	}
	else if (consp(value)) {
		Return(array_set_dimension2_(pos, value, &size));
	}
	else {
		return fmte_("Array index ~A must be an integer or list.", value, NULL);
	}
	str = ArrayInfoStruct(pos);
	str->size = str->front = size;

	return 0;
}


/*
 *  array-allocate
 */
static void array_allocate_t(LocalRoot local, addr pos, struct array_struct *str)
{
	addr array;
	arraygen_alloc(local, &array, str->size);
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, array);
}

void array_allocate_bit(LocalRoot local, addr pos, struct array_struct *str)
{
	addr array;
	bitmemory_unsafe(local, &array, str->size);
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, array);
}

int array_allocate_size_(LocalRoot local, addr pos, struct array_struct *str)
{
	addr array;
	size_t size;

	if (multisafe_size(str->size , str->element, &size))
		return fmte_("size overflow.", NULL);
	arrayspec_alloc(local, &array, size);
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, array);

	return 0;
}

int array_allocate_(LocalRoot local, addr pos, struct array_struct *str)
{
	switch (str->type) {
		case ARRAY_TYPE_EMPTY:
			return fmte_("The array has no element size.", NULL);

		case ARRAY_TYPE_T:
			array_allocate_t(local, pos, str);
			return 0;

		case ARRAY_TYPE_BIT:
			array_allocate_bit(local, pos, str);
			return 0;

		default:
			return array_allocate_size_(local, pos, str);
	}
}


/*
 *  array-memory
 */
static void array_set_adjustable(struct array_struct *str, addr adjustable)
{
	str->adjustable = (adjustable != Nil);
}

static int array_set_fillpointer_(struct array_struct *str, addr fill)
{
	int check;
	size_t size;

	str->fillpointer = (fill != Nil);
	if (fill == Nil)
		return 0;
	if (str->dimension != 1)
		return fmte_("fill-pointer array must be a 1 dimensional.", NULL);
	if (fill == T)
		return 0;
	if (! integerp(fill))
		return fmte_("fill-pointer ~A must be an integer.", fill, NULL);
	Return(minusp_integer_(fill, &check));
	if (check)
		return fmte_("fill-pointer ~A must be a non-negative integer.", fill, NULL);
	if (GetIndex_integer(fill, &size))
		return fmte_("fill-pointer ~A is too large.", fill, NULL);
	if (str->size < size)
		return fmte_("fill-pointer ~A must be less than array size.", fill, NULL);
	str->front = size;

	return 0;
}

static int array_set_displaced_array_(
		addr pos, addr displaced, addr offset, size_t *ret)
{
	struct array_struct *str1, *str2;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(displaced);
	if (str2->size < str1->size) {
		*ret = 0;
		return fmte_("Array size and offset must be less than equal to "
				"displaced array size.", NULL);
	}
	if (! array_equal_type(str1, str2->type, str2->bytesize)) {
		*ret = 0;
		return fmte_("Array type must be equal to displaced array.", NULL);
	}

	return Result(ret, str2->size);
}

static int array_set_displaced_vector_(
		addr pos, addr displaced, addr offset, size_t *ret)
{
	struct array_struct *str;
	size_t size;

	/* :displaced-to */
	str = ArrayInfoStruct(pos);
	lenarray(displaced, &size);
	if (size < str->size) {
		*ret = 0;
		return fmte_("Array size and offset must be less than equal to "
				"displaced array size.", NULL);
	}
	if (! array_equal_type(str, ARRAY_TYPE_T, 0)) {
		*ret = 0;
		return fmte_("Array type must be equal to displaced array.", NULL);
	}

	return Result(ret, size);
}

static int array_set_displaced_bitvector_(
		addr pos, addr displaced, addr offset, size_t *ret)
{
	struct array_struct *str;
	size_t size;

	/* :displaced-to */
	str = ArrayInfoStruct(pos);
	bitmemory_length(displaced, &size);
	if (size < str->size) {
		*ret = 0;
		return fmte_("Array size and offset must be less than equal to "
				"displaced array size.", NULL);
	}
	if (! array_equal_type(str, ARRAY_TYPE_BIT, 0)) {
		*ret = 0;
		return fmte_("Array type must be equal to displaced array.", NULL);
	}

	return Result(ret, size);
}

static int array_set_displaced_string_(
		addr pos, addr displaced, addr offset, size_t *ret)
{
	struct array_struct *str;
	size_t size;

	/* :displaced-to */
	str = ArrayInfoStruct(pos);
	string_length(displaced, &size);
	if (size < str->size) {
		*ret = 0;
		return fmte_("Array size and offset must be less than equal to "
				"displaced array size.", NULL);
	}
	if (! array_equal_type(str, ARRAY_TYPE_CHARACTER, 0)) {
		*ret = 0;
		return fmte_("Array type must be equal to displaced array.", NULL);
	}

	return Result(ret, size);
}

static int array_set_displaced_value_(
		addr pos, size_t size, addr displaced, addr offset)
{
	int check;
	struct array_struct *str;
	size_t value;

	str = ArrayInfoStruct(pos);
	SetArrayInfo(pos, ARRAY_INDEX_DISPLACED, displaced);

	/* displaced-index-offset */
	if (! integerp(offset))
		return fmte_("Array offset ~A must be an integer.", offset, NULL);
	Return(minusp_integer_(offset, &check));
	if (check)
		return fmte_("Array offset ~A must be a non-negative integer.", offset, NULL);
	if (GetIndex_integer(offset, &value))
		return fmte_("Array offset ~A is too large.", offset, NULL);
	if (size < value)
		return fmte_("Too large offset size ~A.", offset, NULL);
	if (size - value < str->size)
		return fmte_("Array size is not enough length.", NULL);
	str->offset = value;

	return 0;
}

int array_set_displaced_(addr pos, addr displaced, addr offset)
{
	struct array_struct *str;
	size_t size;

	/* set structure */
	str = ArrayInfoStruct(pos);
	str->displaced = (displaced != Nil);
	if (displaced == Nil) {
		str->offset = 0;
		return 0;
	}

	/* type check */
	switch (GetType(displaced)) {
		case LISPTYPE_ARRAY:
			Return(array_set_displaced_array_(pos, displaced, offset, &size));
			break;

		case LISPTYPE_VECTOR:
			Return(array_set_displaced_vector_(pos, displaced, offset, &size));
			break;

		case LISPTYPE_BITVECTOR:
			Return(array_set_displaced_bitvector_(pos, displaced, offset, &size));
			break;

		case LISPTYPE_STRING:
			Return(array_set_displaced_string_(pos, displaced, offset, &size));
			break;

		default:
			return fmte_(":displaced-to parameter ~S "
					"must be a array type.", displaced, NULL);
	}

	return array_set_displaced_value_(pos, size, displaced, offset);
}

void array_set_simple(addr pos)
{
	struct array_struct *str;
	str = ArrayInfoStruct(pos);
	str->simple = ! (str->adjustable || str->fillpointer || str->displaced);
}

int array_make_memory_(addr pos, addr adjust, addr fill, addr displaced, addr offset)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	array_set_adjustable(str, adjust);
	Return(array_set_fillpointer_(str, fill));
	Return(array_set_displaced_(pos, displaced, offset));
	array_set_simple(pos);
	if (str->simple || str->displaced == 0) {
		Return(array_allocate_(NULL, pos, str));
	}

	return 0;
}


/*
 *  array-initial
 */
static int array_initial_t_(addr pos, addr value, size_t size)
{
	size_t i;

	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	for (i = 0; i < size; i++)
		arraygen_set(pos, i, value);

	return 0;
}

static int array_initial_bit_(addr pos, addr value, size_t size)
{
	fixnum init;

	if (! fixnump(value))
		return fmte_(":initial-element ~A must be integer type.", value, NULL);
	GetFixnum(value, &init);
	if (init != 0 && init != 1)
		return fmte_(":initail-element ~A must be 0 or 1.", value, NULL);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	bitmemory_memset(pos, (int)init);

	return 0;
}

#ifdef LISP_DEBUG
static void array_initial_memset_buffer(addr pos, int value)
{
	struct array_struct *str;
	size_t size, elem;

	str = ArrayInfoStruct(pos);
	size = str->size;
	elem = str->element;
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	memset(arrayspec_ptr(pos), value, size * elem);
}
#endif

static void array_initial_memset(addr pos, const void *src)
{
	struct array_struct *str;
	size_t size, elem, i;
	byte *dst;

	str = ArrayInfoStruct(pos);
	size = str->size;
	elem = str->element;
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	dst = (byte *)arrayspec_ptr(pos);
	for (i = 0; i < size; i++) {
		memcpy(dst, src, elem);
		dst += elem;
	}
}

static void array_initial_unicode(addr pos, unicode u)
{
	array_initial_memset(pos, (const void *)&u);
}

static int array_initial_character_(addr pos, addr value)
{
	unicode u;

	if (! characterp(value))
		return fmte_(":initial-element ~A must be character type.", value, NULL);
	GetCharacter(value, &u);
	array_initial_unicode(pos, u);

	return 0;
}

static int array_initial_signed8_(addr pos, addr value)
{
	void *data;
	struct array_struct *str;
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (init < INT8_MIN || INT8_MAX < init)
		goto error;
	str = ArrayInfoStruct(pos);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	data = (void *)arrayspec_ptr(pos);
	memset(data, (byte)init, str->size);
	return 0;

error:
	return fmte_("Overflow :initial-element ~A in (signed-byte 8).", value, NULL);
}

static int array_initial_signed16_(addr pos, addr value)
{
	int16_t c;
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (init < INT16_MIN || INT16_MAX < init)
		goto error;
	c = (int16_t)init;
	array_initial_memset(pos, (const void *)&c);
	return 0;

error:
	return fmte_("Overflow :initial-element ~A in (signed-byte 16).", value, NULL);
}

#ifdef LISP_64BIT
static int array_initial_signed32_(addr pos, addr value)
{
	int32_t c;
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (init < INT32_MIN || INT32_MAX < init)
		goto error;
	c = (int32_t)init;
	array_initial_memset(pos, (const void *)&c);
	return 0;

error:
	return fmte_("Overflow :initial-element ~A in (signed-byte 32).", value, NULL);
}

static int array_initial_signed64_(addr pos, addr value)
{
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		return fmte_("Overflow :initial-element ~A in (signed-byte 64).", value, NULL);
	GetFixnum(value, &init);
	array_initial_memset(pos, (const void *)&init);

	return 0;
}
#else
static int array_initial_signed32_(addr pos, addr value)
{
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		return fmte_("Overflow :initial-element ~A in (signed-byte 32).", value, NULL);
	GetFixnum(value, &init);
	array_initial_memset(pos, (const void *)&init);

	return 0;
}
#endif

static int array_initial_signed_(addr pos, addr value)
{
	switch (ArrayInfoStruct(pos)->bytesize) {
		case 8:
			return array_initial_signed8_(pos, value);

		case 16:
			return array_initial_signed16_(pos, value);

		case 32:
			return array_initial_signed32_(pos, value);

#ifdef LISP_64BIT
		case 64:
			return array_initial_signed64_(pos, value);
#endif
		default:
			return fmte_("Invalid array size.", NULL);
	}
}

static int array_initial_unsigned8_(addr pos, addr value)
{
	byte *data;
	struct array_struct *str;
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (minusp_integerp(value)) {
		return fmte_(":initial-element ~A "
				"must be greater than equal to 0.", value, NULL);
	}
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (UINT8_MAX < init)
		goto error;
	str = ArrayInfoStruct(pos);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	data = (byte *)arrayspec_ptr(pos);
	memset(data, (byte)init, str->size);
	return 0;

error:
	return fmte_("Overflow :initial-element ~A in (unsigned-byte 8).", value, NULL);
}

static int array_initial_unsigned16_(addr pos, addr value)
{
	uint16_t c;
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (minusp_integerp(value)) {
		return fmte_(":initial-element ~A "
				"must be greater than equal to 0.", value, NULL);
	}
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (UINT16_MAX < init)
		goto error;
	c = (uint16_t)init;
	array_initial_memset(pos, (const void *)&c);
	return 0;

error:
	return fmte_("Overflow :initial-element ~A in (unsigned-byte 16).", value, NULL);
}

#ifdef LISP_64BIT
static int array_initial_unsigned32_(addr pos, addr value)
{
	uint32_t c;
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (minusp_integerp(value)) {
		return fmte_(":initial-element ~A "
				"must be greater than equal to 0.", value, NULL);
	}
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (UINT32_MAX < init)
		goto error;
	c = (uint32_t)init;
	array_initial_memset(pos, (const void *)&c);
	return 0;

error:
	return fmte_("Overflow :initial-element ~A in (unsigned-byte 32).", value, NULL);
}

static int array_initial_unsigned64_(addr pos, addr value)
{
	fixnum init;
	fixed bigv;
	size_t size;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (minusp_integerp(value)) {
		return fmte_(":initial-element ~A "
				"must be greater than equal to 0.", value, NULL);
	}
	if (fixnump(value)) {
		GetFixnum(value, &init);
		if (init < 0)
			goto error;
		array_initial_memset(pos, (const void *)&init);
		return 0;
	}
	if (bignump(value)) {
		if (minusp_bignum(value))
			goto error;
		GetSizeBignum(value, &size);
		if (size != 1)
			goto error;
		getfixed_bignum(value, 0, &bigv);
		array_initial_memset(pos, (const void *)&bigv);
		return 0;
	}
	return TypeError_(value, INTEGER);

error:
	return fmte_("Overflow :initial-element ~A in (unsigned-byte 64).", value, NULL);
}
#else
static int array_initial_unsigned32_(addr pos, addr value)
{
	fixnum init;
	fixed bigv;
	size_t size;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (minusp_integerp(value)) {
		return fmte_(":initial-element ~A "
				"must be greater than equal to 0.", value, NULL);
	}
	if (fixnump(value)) {
		GetFixnum(value, &init);
		if (init < 0)
			goto error;
		array_initial_memset(pos, (const void *)&init);
		return 0;
	}
	if (bignump(value)) {
		if (minusp_bignum(value))
			goto error;
		GetSizeBignum(value, &size);
		if (size != 1)
			goto error;
		getfixed_bignum(value, 0, &bigv);
		array_initial_memset(pos, (const void *)&bigv);
		return 0;
	}
	return TypeError_(value, INTEGER);

error:
	return fmte_("Overflow :initial-element ~A in (unsigned-byte 32).", value, NULL);
}
#endif

static int array_initial_unsigned_(addr pos, addr value)
{
	switch (ArrayInfoStruct(pos)->bytesize) {
		case 8:
			return array_initial_unsigned8_(pos, value);

		case 16:
			return array_initial_unsigned16_(pos, value);

		case 32:
			return array_initial_unsigned32_(pos, value);

#ifdef LISP_64BIT
		case 64:
			return array_initial_unsigned64_(pos, value);
#endif
		default:
			return fmte_("Invalid array size.", NULL);
	}
}

static int array_initial_single_(addr pos, addr value)
{
	single_float v;

	if (GetType(value) != LISPTYPE_SINGLE_FLOAT)
		return fmte_(":initial-element ~A must be single-float type.", value, NULL);
	GetSingleFloat(value, &v);
	array_initial_memset(pos, (const void *)&v);

	return 0;
}

static int array_initial_double_(addr pos, addr value)
{
	double_float v;

	if (GetType(value) != LISPTYPE_DOUBLE_FLOAT)
		return fmte_(":initial-element ~A must be double-float type.", value, NULL);
	GetDoubleFloat(value, &v);
	array_initial_memset(pos, (const void *)&v);

	return 0;
}

static int array_initial_long_(addr pos, addr value)
{
	long_float v;

	if (GetType(value) != LISPTYPE_LONG_FLOAT)
		return fmte_(":initial-element ~A must be long-float type.", value, NULL);
	GetLongFloat(value, &v);
	array_initial_memset(pos, (const void *)&v);

	return 0;
}

static int array_initial_value_(addr pos, addr value)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return array_initial_t_(pos, value, str->size);

		case ARRAY_TYPE_BIT:
			return array_initial_bit_(pos, value, str->size);

		case ARRAY_TYPE_CHARACTER:
			return array_initial_character_(pos, value);

		case ARRAY_TYPE_SIGNED:
			return array_initial_signed_(pos, value);

		case ARRAY_TYPE_UNSIGNED:
			return array_initial_unsigned_(pos, value);

		case ARRAY_TYPE_SINGLE_FLOAT:
			return array_initial_single_(pos, value);

		case ARRAY_TYPE_DOUBLE_FLOAT:
			return array_initial_double_(pos, value);

		case ARRAY_TYPE_LONG_FLOAT:
			return array_initial_long_(pos, value);

		default:
			return fmte_("Invalid array type.", NULL);
	}
}


/*
 *  array-initial-contents
 *
 *  (s1 s2 s3 s4) : (n1 n2 n3 n4)
 *   -> (s4*s3*s2*n1) + (s4*s3*n2) + (s4*n3) + n4
 *   -> s4*(...) + n4
 *   -> s4*(s3*(...) + n3) + n4
 *   -> s4*(s3*(s2*n1 + n2) + n3) + n4
 *   [1] n1
 *   [2] s2*... + n2
 *   [3] s3*... + n3
 *   [4] s4*... + n4
 */
static int array_contents_recursive_(addr, addr,
		const size_t *data, size_t limit, size_t depth, size_t size);

static int array_contents_list_(addr pos, addr list,
		const size_t *data, size_t limit, size_t depth, size_t size)
{
	addr next;
	size_t i, length;

	if (depth < limit) {
		length = data[depth];
		size = depth? (size * length): 0;
		for (i = 0; list != Nil; i++) {
			if (length <= i)
				return fmte_("Too many :initial-contents ~S list.", list, NULL);
			Return_getcons(list, &next, &list);
			Return(array_contents_recursive_(pos, next, data, limit, depth+1, size+i));
		}
		if (i < length)
			return fmte_("Too few :initial-contents list.", NULL);
	}
	else {
		Return(array_set_(pos, size, list));
	}

	return 0;
}

static int array_contents_sequence_(addr pos, addr object,
		const size_t *data, size_t limit, size_t depth, size_t size)
{
	addr next;
	size_t i, length, check;

	if (depth < limit) {
		length = data[depth];
		size = depth? (size * length): 0;
		Return(length_sequence_(object, 1, &check));
		if (length < check)
			return fmte_("Too many :initial-contents ~S.", object, NULL);
		if (check < length)
			return fmte_("Too few :initial-contents ~S.", object, NULL);
		for (i = 0; i < check; i++) {
			Return(getelt_sequence_(NULL, object, i, &next));
			Return(array_contents_recursive_(pos, next, data, limit, depth+1, size+i));
		}
	}
	else {
		Return(array_set_(pos, size, object));
	}

	return 0;
}

static int array_contents_object_(addr pos, addr list,
		size_t limit, size_t depth, size_t size)
{
	if (depth < limit)
		return fmte_("Too few :initial-contents.", NULL);
	else
		return array_set_(pos, size, list);
}

static int array_contents_recursive_(addr pos,
		addr object, const size_t *data, size_t limit, size_t depth, size_t size)
{
	switch (GetType(object)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return array_contents_list_(pos, object, data, limit, depth, size);

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
		case LISPTYPE_BITVECTOR:
			return array_contents_sequence_(pos, object, data, limit, depth, size);

		default:
			return array_contents_object_(pos, object, limit, depth, size);
	}
}

static int array_contents_setf_(addr pos, addr contents)
{
	struct array_struct *str;
	const size_t *data;
	size_t dimension;

	str = ArrayInfoStruct(pos);
	dimension = str->dimension;
	data = array_ptrsize(pos);
	return array_contents_recursive_(pos, contents, data, dimension, 0, 0);
}

static int array_initial_contents_(addr pos, addr contents)
{
	size_t dimension;

	dimension = ArrayInfoStruct(pos)->dimension;
	if (dimension == 0)
		return array_set_(pos, 0, contents);
	else
		return array_contents_setf_(pos, contents);
}


/*
 *  array-make-array
 */
static void array_make_initial_clear(addr pos)
{
	struct array_struct *str;
#ifdef LISP_DEBUG
	single_float sv;
	double_float dv;
	long_float lv;
#endif

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_CHARACTER:
			array_initial_unicode(pos, 0);
			break;

#ifdef LISP_DEBUG
		case ARRAY_TYPE_BIT:
			GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
			bitmemory_memset(pos, 1);
			break;

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			array_initial_memset_buffer(pos, 0xFF);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			sv = nanf("");
			array_initial_memset(pos, (const void *)&sv);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			dv = nan("");
			array_initial_memset(pos, (const void *)&dv);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			lv = nanl("");
			array_initial_memset(pos, (const void *)&lv);
			break;
#endif

		default:
			break;
	}
}

int array_make_initial_(addr pos, addr initial, addr contents)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->displaced && (initial != Unbound || contents != Unbound)) {
		return fmte_("Displaced array don't have "
				":initial-element or :initial-contents.", NULL);
	}
	if (initial != Unbound && contents != Unbound) {
		return fmte_("Array parameter cannot have both :initial-element and "
				":initial-contens parameter.", NULL);
	}
	if (initial != Unbound) {
		return array_initial_value_(pos, initial);
	}
	if (contents != Unbound) {
		return array_initial_contents_(pos, contents);
	}
	if (str->displaced == 0) {
		array_make_initial_clear(pos);
	}

	return 0;
}

int array_make_clear_(addr pos)
{
	struct array_struct *str;
	addr value;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return array_initial_t_(pos, Nil, str->size);

		case ARRAY_TYPE_BIT:
			fixnum_heap(&value, 0);
			return array_initial_bit_(pos, value, str->size);

		case ARRAY_TYPE_CHARACTER:
			character_heap(&value, 0);
			return array_initial_character_(pos, value);

		case ARRAY_TYPE_SIGNED:
			fixnum_heap(&value, 0);
			return array_initial_signed_(pos, value);

		case ARRAY_TYPE_UNSIGNED:
			fixnum_heap(&value, 0);
			return array_initial_unsigned_(pos, value);

		case ARRAY_TYPE_SINGLE_FLOAT:
			single_float_heap(&value, 0.0f);
			return array_initial_single_(pos, value);

		case ARRAY_TYPE_DOUBLE_FLOAT:
			double_float_heap(&value, 0.0);
			return array_initial_double_(pos, value);

		case ARRAY_TYPE_LONG_FLOAT:
			long_float_heap(&value, 0.0L);
			return array_initial_long_(pos, value);

		default:
			return fmte_("Invalid array type.", NULL);
	}
}

static int array_set_type_upgraded_(addr pos, addr type)
{
	enum ARRAY_TYPE value;
	int size;

	Return(upgraded_array_value_(type, &value, &size));
	array_set_type_value(pos, value, size);

	return 0;
}

int array_make_array_(addr *ret, addr dimension,
		addr type, addr initial, addr contents,
		addr adjustable, addr fillpointer, addr displaced, addr offset)
{
	addr pos;

	array_empty_heap(&pos);
	Return(array_set_type_upgraded_(pos, type));
	array_set_element_size(pos);
	Return(array_set_dimension_(pos, dimension));
	Return(array_make_memory_(pos, adjustable, fillpointer, displaced, offset));
	Return(array_make_initial_(pos, initial, contents));

	return Result(ret, pos);
}


/*
 *  array_contents_heap
 */
static int array_contents_size_(addr pos, addr rankarg, addr contents)
{
	struct array_struct *str;
	size_t i, rank, size, *data;
	addr temp;

	if (GetIndex_integer(rankarg, &rank))
		return fmte_("Array rank ~A is too large.", rankarg, NULL);
	str = ArrayInfoStruct(pos);
	if (rank == 0) {
		str->dimension = 0;
		SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, Nil);
		size = 1;
	}
	else if (rank == 1) {
		str->dimension = 1;
		Return(length_list_safe_(contents, &size));
		SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, Nil);
	}
	else {
		str->dimension = rank;
		Return(arraysize_heap_(&temp, rank));
		data = arraysize_ptr(temp);
		for (i = 0; i < rank; i++) {
			if (! consp(contents))
				return fmte_("Invalid initial-contents parameter ~S.", contents, NULL);
			Return(length_list_safe_(contents, &(data[i])));
			GetCar(contents, &contents);
		}
		SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, temp);
		Return(array_getsize_(pos, &size));
	}
	str = ArrayInfoStruct(pos);
	str->size = str->front = size;

	return 0;
}

int array_contents_heap_(addr *ret, addr rank, addr contents)
{
	addr pos;

	array_empty_heap(&pos);
	array_set_type_value(pos, ARRAY_TYPE_T, 0);
	array_set_element_size(pos);
	Return(array_contents_size_(pos, rank, contents));
	Return(array_make_memory_(pos, Nil, Nil, Nil, Nil));
	Return(array_make_initial_(pos, Unbound, contents));

	return Result(ret, pos);
}


/*
 *  array function
 */
static int array_check_fillpointer_(addr pos, struct array_struct *str)
{
	if (str->fillpointer) {
		if (str->size < str->front)
			return fmte_("fill-pointer size must be smaller than element-size.", NULL);
		if (! array_vector_p(pos))
			return fmte_("fill-pointer array must be an one dimension.", NULL);
	}
	else {
		str->front = str->size;
	}

	return 0;
}

int array_character_alloc_(LocalRoot local, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	array_set_type_value(pos, ARRAY_TYPE_CHARACTER, 0);
	array_set_element_size(pos);
	array_set_simple(pos);
	Return(array_check_fillpointer_(pos, str));
	return array_allocate_(local, pos, str);
}

int array_unsigned8_heap_(addr *ret, size_t size)
{
	addr pos;
	struct array_struct *str;

	Return(array_heap_(&pos, 1, size));
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 8;
	Return(array_build_(pos));

	return Result(ret, pos);
}

int array_build_(addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	array_set_type(pos);
	array_set_element_size(pos);
	array_set_simple(pos);
	Return(array_check_fillpointer_(pos, str));
	return array_allocate_(NULL, pos, str);
}


/************************************************************
 *  array_sequence.c
 ************************************************************/

/*
 *  bitcalc
 */
static void array_bitcalc_struct(addr pos, addr src)
{
	struct array_struct *str1;
	const struct array_struct *str2;
	addr type;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(src);
	str1->simple = 1;
	str1->adjustable = 0;
	str1->fillpointer = 0;
	str1->displaced = 0;
	str1->type = str2->type;
	str1->element = str2->element;
	str1->bytesize = str2->bytesize;
	str1->size = str2->front;
	str1->front = str2->front;
	str1->dimension = str2->dimension;
	str1->offset = 0;
	GetArrayInfo(src, ARRAY_INDEX_TYPE, &type);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, type);
}

static int array_bitcalc_size_(addr pos, addr src)
{
	struct array_struct *str;
	const size_t *data2;
	addr temp;
	size_t size, i, *data1;

	str = ArrayInfoStruct(src);
	data2 = array_ptrsize(src);
	size = str->dimension;
	if (2 <= size) {
		Return(arraysize_heap_(&temp, size));
		data1 = arraysize_ptr(temp);
		for (i = 0; i < size; i++)
			data1[i] = data2[i];
		SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, temp);
	}

	return 0;
}

static int array_bitcalc_make_(addr src, addr *ret)
{
	struct array_struct *str;
	addr pos;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	array_bitcalc_struct(pos, src);
	/* dimension */
	Return(array_bitcalc_size_(pos, src));
	/* allocate */
	str = ArrayInfoStruct(pos);
	array_allocate_bit(NULL, pos, str);
	/* result */
	return Result(ret, pos);
}

static int array_bitvector_type_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_BITVECTOR:
			return Result(ret, 0);

		case LISPTYPE_ARRAY:
			return Result(ret, 1);

		default:
			*ret = 0;
			return fmte_("Argument ~S must be a bit-array type.", pos, NULL);
	}
}

static int array_bitvector_size_equal_(addr pos1, addr pos2, int *ret)
{
	int check1, check2;

	Return(array_bitvector_type_(pos1, &check1));
	Return(array_bitvector_type_(pos2, &check2));
	if (check1 && check2) {
		return Result(ret, array_equal_dimension(pos1, pos2));
	}
	if (check1) {
		if (ArrayInfoStruct(pos1)->dimension != 1)
			return Result(ret, 0);
		GetArrayInfo(pos1, ARRAY_INDEX_MEMORY, &pos1);
		return Result(ret, bitmemory_equal_length(pos1, pos2));
	}
	if (check2) {
		if (ArrayInfoStruct(pos2)->dimension != 1)
			return Result(ret, 0);
		GetArrayInfo(pos2, ARRAY_INDEX_MEMORY, &pos2);
		return Result(ret, bitmemory_equal_length(pos1, pos2));
	}
	else {
		return Result(ret, bitmemory_equal_length(pos1, pos2));
	}
}

static int array_bitcalc_aa_(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	int check;
	struct array_struct *str;

	Return(array_bitvector_size_equal_(pos1, pos2, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Dimension don't match ~S and ~S.", pos1, pos2, NULL);
	}
	if (opt == Nil) {
		Return(array_bitcalc_make_(pos1, &opt));
		*ret = opt;
		GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	else if (opt == T) {
		*ret = opt = pos1;
		GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	else {
		Return(array_bitvector_size_equal_(pos1, opt, &check));
		if (! check) {
			*ret = Nil;
			return fmte_("Length don't match ~S and optional ~S", pos1, opt, NULL);
		}
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos1);
	if (str->type != ARRAY_TYPE_BIT) {
		*ret = Nil;
		return fmte_("Array ~S must be a bit type.", pos1, NULL);
	}
	str = ArrayInfoStruct(pos2);
	if (str->type != ARRAY_TYPE_BIT) {
		*ret = Nil;
		return fmte_("Array ~S must be a bit type.", pos2, NULL);
	}
	if (! bitvectorp(*ret)) {
		return fmte_("Array ~S must be a bit type.", *ret, NULL);
	}
	GetArrayInfo(pos1, ARRAY_INDEX_MEMORY, &pos1);
	GetArrayInfo(pos2, ARRAY_INDEX_MEMORY, &pos2);
	bitmemory_bitcalc(opt, pos1, pos2, call);

	return 0;
}

static int array_bitcalc_ab_(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	int check;
	struct array_struct *str;
	size_t size;

	Return(array_bitvector_size_equal_(pos1, pos2, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Length don't match ~S and ~S", pos1, pos2, NULL);
	}
	if (opt == Nil) {
		bitmemory_length(pos2, &size);
		bitmemory_heap(&opt, size);
		*ret = opt;
	}
	else if (opt == T) {
		*ret = pos1;
		GetArrayInfo(pos1, ARRAY_INDEX_MEMORY, &opt);
	}
	else {
		Return(array_bitvector_size_equal_(pos1, opt, &check));
		if (! check) {
			*ret = Nil;
			return fmte_("Length don't match ~S and optional ~S", pos1, opt, NULL);
		}
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos1);
	if (str->type != ARRAY_TYPE_BIT) {
		*ret = Nil;
		return fmte_("Array ~S must be a bit type.", pos1, NULL);
	}
	if (! bitvectorp(*ret)) {
		return fmte_("Array ~S must be a bit type.", *ret, NULL);
	}
	GetArrayInfo(pos1, ARRAY_INDEX_MEMORY, &pos1);
	bitmemory_bitcalc(opt, pos1, pos2, call);

	return 0;
}

static int array_bitcalc_ba_(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	int check;
	struct array_struct *str;
	size_t size;

	Return(array_bitvector_size_equal_(pos1, pos2, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Length don't match ~S and ~S", pos1, pos2, NULL);
	}
	if (opt == Nil) {
		bitmemory_length(pos1, &size);
		bitmemory_heap(&opt, size);
		*ret = opt;
	}
	else if (opt == T) {
		*ret = opt = pos1;
	}
	else {
		Return(array_bitvector_size_equal_(pos1, opt, &check));
		if (! check) {
			*ret = Nil;
			return fmte_("Length don't match ~S and optional ~S", pos1, opt, NULL);
		}
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos2);
	if (str->type != ARRAY_TYPE_BIT) {
		*ret = Nil;
		return fmte_("Array ~S must be a bit type.", pos2, NULL);
	}
	if (! bitvectorp(*ret)) {
		return fmte_("Array ~S must be a bit type.", *ret, NULL);
	}
	GetArrayInfo(pos2, ARRAY_INDEX_MEMORY, &pos2);
	bitmemory_bitcalc(opt, pos1, pos2, call);

	return 0;
}

static int array_bitcalc_bb_(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	int check;
	size_t size;

	if (! bitmemory_equal_length(pos1, pos2)) {
		*ret = Nil;
		return fmte_("Length don't match ~S and ~S", pos1, pos2, NULL);
	}
	if (opt == Nil) {
		bitmemory_length(pos1, &size);
		bitmemory_heap(&opt, size);
		*ret = opt;
	}
	else if (opt == T) {
		*ret = opt = pos1;
	}
	else {
		Return(array_bitvector_size_equal_(pos1, opt, &check));
		if (! check) {
			*ret = Nil;
			return fmte_("Length don't match ~S and optional ~S", pos1, opt, NULL);
		}
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	if (! bitvectorp(*ret)) {
		return fmte_("Array ~S must be a bit type.", *ret, NULL);
	}
	bitmemory_bitcalc(opt, pos1, pos2, call);

	return 0;
}

int array_bitcalc_(addr *ret, addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	int check1, check2;

	Return(array_bitvector_type_(pos1, &check1));
	Return(array_bitvector_type_(pos2, &check2));
	if (check1 && check2)
		return array_bitcalc_aa_(ret, pos1, pos2, opt, call);
	else if (check1)
		return array_bitcalc_ab_(ret, pos1, pos2, opt, call);
	else if (check2)
		return array_bitcalc_ba_(ret, pos1, pos2, opt, call);
	else
		return array_bitcalc_bb_(ret, pos1, pos2, opt, call);
}


/*
 *  array_bitnot
 */
static int array_bitnot_array_(addr *ret, addr pos, addr opt)
{
	int check;
	struct array_struct *str;

	if (opt == Nil) {
		Return(array_bitcalc_make_(pos, &opt));
		*ret = opt;
		GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	else if (opt == T) {
		*ret = opt = pos;
		GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	else {
		Return(array_bitvector_size_equal_(pos, opt, &check));
		if (! check) {
			*ret = Nil;
			return fmte_("Length don't match ~S and optional ~S", pos, opt, NULL);
		}
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos);
	if (str->type != ARRAY_TYPE_BIT) {
		*ret = Nil;
		return fmte_("Array ~S must be a bit type.", pos, NULL);
	}
	if (! bitvectorp(*ret)) {
		return fmte_("Array ~S must be a bit type.", *ret, NULL);
	}
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	bitmemory_bitnot(opt, pos);

	return 0;
}

static int array_bitnot_bitmemory_(addr *ret, addr pos, addr opt)
{
	int check;
	size_t size;

	if (opt == Nil) {
		bitmemory_length(pos, &size);
		bitmemory_heap(&opt, size);
		*ret = opt;
	}
	else if (opt == T) {
		*ret = opt = pos;
	}
	else {
		Return(array_bitvector_size_equal_(pos, opt, &check));
		if (! check) {
			*ret = Nil;
			return fmte_("Length don't match ~S and optional ~S", pos, opt, NULL);
		}
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	if (! bitvectorp(*ret)) {
		return fmte_("Array ~S must be a bit type.", *ret, NULL);
	}
	bitmemory_bitnot(opt, pos);

	return 0;
}

int array_bitnot_(addr *ret, addr pos, addr opt)
{
	int check;

	Return(array_bitvector_type_(pos, &check));
	if (check)
		return array_bitnot_array_(ret, pos, opt);
	else
		return array_bitnot_bitmemory_(ret, pos, opt);
}


/*
 *  array_fill
 */
int array_fill_(addr pos, addr item, addr start, addr end)
{
	size_t index1, index2;
	struct array_struct *str;

	/* argument */
	str = ArrayInfoStruct(pos);
	Return(size_start_end_sequence_(start, end, str->size, &index1, &index2, NULL));

	/* fill */
	for (; index1 < index2; index1++) {
		Return(array_set_(pos, index1, item));
	}

	return 0;
}


/*
 *  array_subseq
 */
static int array_subseq_general_(addr *ret, addr pos, size_t index1, size_t index2)
{
	addr root, temp;
	size_t i;

	Check(index2 < index1, "index error");
	vector_heap(&root, index2 - index1);
	for (i = 0; index1 < index2; index1++, i++) {
		Return(array_get_t_(pos, index1, &temp));
		setarray(root, i, temp);
	}

	return Result(ret, root);
}

static int array_subseq_specialized_make_(addr *ret, addr array, size_t size)
{
	struct array_struct *str;
	addr pos;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	array_bitcalc_struct(pos, array);
	str = ArrayInfoStruct(pos);
	str->size = str->front = size;
	/* allocate */
	Check(str->dimension != 1, "dimension error");
	Return(array_allocate_size_(NULL, pos, str));
	/* result */
	return Result(ret, pos);
}

static int array_subseq_specialized_(addr *ret,
		addr array, size_t index1, size_t index2)
{
	byte *data1;
	const byte *data2;
	addr pos, mem1, mem2;
	size_t element, diff;

	/* make array */
	Check(index2 < index1, "index error");
	diff = index2 - index1;
	Return(array_subseq_specialized_make_(&pos, array, diff));

	/* subseq */
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem1);
	GetArrayInfo(array, ARRAY_INDEX_MEMORY, &mem2);
	data1 = (byte *)arrayspec_ptr(mem1);
	data2 = (const byte *)arrayspec_ptr(mem2);
	element = ArrayInfoStruct(pos)->element;
	memcpy(data1, data2 + index1 * element, diff * element);
	return Result(ret, pos);
}

static int array_subseq_type_(addr *ret, addr pos, size_t index1, size_t index2)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_BIT:
			GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
			return bitmemory_subseq_index_(ret, pos, index1, index2);

		case ARRAY_TYPE_CHARACTER:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return array_subseq_specialized_(ret, pos, index1, index2);

		default:
			return array_subseq_general_(ret, pos, index1, index2);
	}
}

int array_subseq_(addr *ret, addr pos, addr start, addr end)
{
	size_t index1, index2;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Return(size_start_end_sequence_(start, end, str->size, &index1, &index2, NULL));
	return array_subseq_type_(ret, pos, index1, index2);
}


/*
 *  array_reverse
 */
static int array_reverse_t_(addr *ret, addr pos)
{
	addr one, temp;
	size_t size, x, y;

	Return(length_sequence_(pos, 1, &size));
	vector_heap(&one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		Return(array_get_t_(pos, x, &temp));
		setarray(one, y, temp);
	}

	return Result(ret, one);
}

static int array_reverse_bit_(addr *ret, addr pos)
{
	int temp;
	addr one;
	size_t size, x, y;

	Return(length_sequence_(pos, 1, &size));
	bitmemory_unsafe(NULL, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		Return(array_get_bit_(pos, x, &temp));
		Return(bitmemory_setint_(one, y, temp));
	}

	return Result(ret, one);
}

static int array_reverse_character_(addr *ret, addr pos)
{
	unicode temp;
	addr one;
	size_t size, x, y;

	Return(length_sequence_(pos, 1, &size));
	strvect_heap(&one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		Return(array_get_unicode_(pos, x, &temp));
		Return(strvect_setc_(one, y, temp));
	}

	return Result(ret, one);
}

static void array_type_simple_vector(addr pos, enum ARRAY_TYPE type, unsigned size)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	str->type = type;
	str->bytesize = size;
	array_set_type(pos);
}

static int array_make_simple_vector_(addr *ret,
		size_t size, enum ARRAY_TYPE type, unsigned bytesize)
{
	struct array_struct *str;
	addr pos;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	array_type_simple_vector(pos, type, bytesize);
	array_set_element_size(pos);
	/* dimension */
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->size = str->front = size;
	/* allocate */
	Return(array_make_memory_(pos, Nil, Nil, Nil, Nil));
	/* initial value */
	Return(array_make_initial_(pos, Unbound, Unbound));
	/* result */
	return Result(ret, pos);
}

static int array_reverse_size_(addr *ret, addr pos)
{
	struct array_struct *str;
	addr one;
	size_t size, x, y;
	struct array_value value;

	str = ArrayInfoStruct(pos);
	size = str->size;
	Return(array_make_simple_vector_(&one, size, str->type, str->bytesize));
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		Return(arrayinplace_get_(pos, x, &value));
		Return(arrayinplace_set_(one, y, &value));
	}

	return Result(ret, one);
}

int array_reverse_(addr *ret, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Check(str->dimension != 1, "dimension error");
	switch (str->type) {
		case ARRAY_TYPE_T:
			return array_reverse_t_(ret, pos);

		case ARRAY_TYPE_BIT:
			return array_reverse_bit_(ret, pos);

		case ARRAY_TYPE_CHARACTER:
			return array_reverse_character_(ret, pos);

		default:
			return array_reverse_size_(ret, pos);
	}
}


/*
 *  array_nreverse
 */
int array_nreverse_(addr *ret, addr pos)
{
	size_t size, x, y;
	struct array_value a, b;

	Check(ArrayInfoStruct(pos)->dimension != 1, "dimension error");
	Return(length_sequence_(pos, 1, &size));
	if (size <= 1)
		return 0;
	x = 0;
	y = size - 1;
	while (x < y) {
		Return(arrayinplace_get_(pos, x, &a));
		Return(arrayinplace_get_(pos, y, &b));
		Return(arrayinplace_set_(pos, x, &b));
		Return(arrayinplace_set_(pos, y, &a));
		x++;
		y--;
	}

	return Result(ret, pos);
}


/************************************************************
 *  array_value.c
 ************************************************************/

/*
 *  arrayvalue
 */
int arrayvalue_get_character_(struct array_value *str, addr x)
{
	if (! characterp(x))
		return fmte_("~S must be character type.", x, NULL);
	GetCharacter(x, &(str->value.character));
	str->type = ARRAY_TYPE_CHARACTER;

	return 0;
}

int arrayvalue_get_bit_(struct array_value *str, addr x)
{
	int check;

	if (! fixnump(x))
		return fmte_("~S must be an integer type.", x, NULL);
	if (bit_getint(x, &check))
		return fmte_("The integer ~S must be a bit value.", x, NULL);
	str->value.bit = (check != 0);
	str->type = ARRAY_TYPE_BIT;

	return 0;
}

int arrayvalue_get_signed8_(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (init < INT8_MIN || INT8_MAX < init)
		goto error;
	str->value.signed8 = (int8_t)init;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 8;
	return 0;

error:
	return fmte_("Overflow ~S in (signed-byte 8).", x, NULL);
}

int arrayvalue_get_signed16_(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (init < INT16_MIN || INT16_MAX < init)
		goto error;
	str->value.signed16 = (int16_t)init;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 16;
	return 0;

error:
	return fmte_("Overflow ~S in (signed-byte 16).", x, NULL);
}

#ifdef LISP_64BIT
int arrayvalue_get_signed32_(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (init < INT32_MIN || INT32_MAX < init)
		goto error;
	str->value.signed32 = (int32_t)init;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 32;
	return 0;

error:
	return fmte_("Overflow ~S in (signed-byte 32).", x, NULL);
}

int arrayvalue_get_signed64_(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	if (! fixnump(x))
		return fmte_("Overflow ~S in (signed-byte 64).", x, NULL);
	GetFixnum(x, &init);
	str->value.signed64 = (int64_t)init;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 64;

	return 0;
}
#else
int arrayvalue_get_signed32_(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	if (! fixnump(x))
		return fmte_("Overflow ~S in (signed-byte 32).", x, NULL);
	GetFixnum(x, &init);
	str->value.signed32 = (int32_t)init;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 32;

	return 0;
}
#endif

int arrayvalue_get_signed_(struct array_value *str, addr x, unsigned size)
{
	switch (size) {
		case 8:
			return arrayvalue_get_signed8_(str, x);

		case 16:
			return arrayvalue_get_signed16_(str, x);

		case 32:
			return arrayvalue_get_signed32_(str, x);

#ifdef LISP_64BIT
		case 64:
			return arrayvalue_get_signed64_(str, x);
#endif
		default:
			return fmte_("Invalid array size.", NULL);
	}
}

int arrayvalue_get_unsigned8_(struct array_value *str, addr x)
{
	int check;
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	Return(minusp_integer_(x, &check));
	if (check)
		return fmte_("~S must be a non-negative integer.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (UINT8_MAX < init)
		goto error;
	str->value.unsigned8 = (uint8_t)init;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->size = 8;
	return 0;

error:
	return fmte_("Overflow ~S in (unsigned-byte 8).", x, NULL);
}

int arrayvalue_get_unsigned16_(struct array_value *str, addr x)
{
	int check;
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	Return(minusp_integer_(x, &check));
	if (check)
		return fmte_("~S must be a non-negative integer.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (UINT16_MAX < init)
		goto error;
	str->value.unsigned16 = (uint16_t)init;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->size = 16;
	return 0;

error:
	return fmte_("Overflow ~S in (unsigned-byte 16).", x, NULL);
}

#ifdef LISP_64BIT
int arrayvalue_get_unsigned32_(struct array_value *str, addr x)
{
	int check;
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	Return(minusp_integer_(x, &check));
	if (check)
		return fmte_("~S must be a non-negative integer.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (UINT32_MAX < init)
		goto error;
	str->value.unsigned32 = (uint32_t)init;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->size = 32;
	return 0;

error:
	return fmte_("Overflow ~S in (unsigned-byte 32).", x, NULL);
}

int arrayvalue_get_unsigned64_(struct array_value *str, addr x)
{
	int check;
	fixnum init;
	fixed bigv;
	size_t size;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	Return(minusp_integer_(x, &check));
	if (check)
		return fmte_("~S must be a non-negative integer.", x, NULL);
	if (fixnump(x)) {
		GetFixnum(x, &init);
		str->value.unsigned64 = (uint64_t)init;
		str->type = ARRAY_TYPE_UNSIGNED;
		str->size = 64;
		return 0;
	}
	if (bignump(x)) {
		GetSizeBignum(x, &size);
		if (size != 1)
			return fmte_("Overflow ~S in (unsigned-byte 64).", x, NULL);
		getfixed_bignum(x, 0, &bigv);
		str->value.unsigned64 = (uint64_t)bigv;
		str->type = ARRAY_TYPE_UNSIGNED;
		str->size = 64;
		return 0;
	}
	return TypeError_(x, INTEGER);
}
#else
int arrayvalue_get_unsigned32_(struct array_value *str, addr x)
{
	int check;
	fixnum init;
	fixed bigv;
	size_t size;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	Return(minusp_integer_(x, &check));
	if (check)
		return fmte_("~S must be a non-negative integer.", x, NULL);
	if (fixnump(x)) {
		GetFixnum(x, &init);
		str->value.unsigned32 = (uint32_t)init;
		str->type = ARRAY_TYPE_UNSIGNED;
		str->size = 32;
		return 0;
	}
	if (bignump(x)) {
		GetSizeBignum(x, &size);
		if (size != 1)
			return fmte_("Overflow ~S in (unsigned-byte 32).", x, NULL);
		getfixed_bignum(x, 0, &bigv);
		str->value.unsigned32 = (uint32_t)bigv;
		str->type = ARRAY_TYPE_UNSIGNED;
		str->size = 32;
		return 0;
	}
	return TypeError_(x, INTEGER);
}
#endif

int arrayvalue_get_unsigned_(struct array_value *str, addr x, unsigned size)
{
	switch (size) {
		case 8:
			return arrayvalue_get_unsigned8_(str, x);

		case 16:
			return arrayvalue_get_unsigned16_(str, x);

		case 32:
			return arrayvalue_get_unsigned32_(str, x);

#ifdef LISP_64BIT
		case 64:
			return arrayvalue_get_unsigned64_(str, x);
#endif
		default:
			return fmte_("Invalid array size.", NULL);
	}
}

int arrayvalue_get_single_(struct array_value *str, addr x)
{
	if (! single_float_p(x))
		return fmte_("~S must be single-float type.", x, NULL);
	GetSingleFloat(x, &(str->value.single_value));
	str->type = ARRAY_TYPE_SINGLE_FLOAT;

	return 0;
}

int arrayvalue_get_double_(struct array_value *str, addr x)
{
	if (! double_float_p(x))
		return fmte_("~S must be double-float type.", x, NULL);
	GetDoubleFloat(x, &(str->value.double_value));
	str->type = ARRAY_TYPE_DOUBLE_FLOAT;

	return 0;
}

int arrayvalue_get_long_(struct array_value *str, addr x)
{
	if (! long_float_p(x))
		return fmte_("~S must be long-float type.", x, NULL);
	GetLongFloat(x, &(str->value.long_value));
	str->type = ARRAY_TYPE_LONG_FLOAT;

	return 0;
}


/*
 *  array_value
 */
static int arrayvalue_make_signed_(LocalRoot local,
		addr *ret, const struct array_value *str)
{
	switch (str->size) {
		case 8:
			int8_integer_alloc(local, ret, str->value.signed8);
			break;

		case 16:
			int16_integer_alloc(local, ret, str->value.signed16);
			break;

		case 32:
			int32_integer_alloc(local, ret, str->value.signed32);
			break;

#ifdef LISP_64BIT
		case 64:
			int64_integer_alloc(local, ret, str->value.signed64);
			break;
#endif

		default:
			*ret = Nil;
			return fmte_("size error", NULL);
	}

	return 0;
}

static int arrayvalue_make_unsigned_(LocalRoot local,
		addr *ret, const struct array_value *str)
{
	switch (str->size) {
		case 8:
			uint8_integer_alloc(local, ret, str->value.unsigned8);
			break;

		case 16:
			uint16_integer_alloc(local, ret, str->value.unsigned16);
			break;

		case 32:
			uint32_integer_alloc(local, ret, str->value.unsigned32);
			break;

#ifdef LISP_64BIT
		case 64:
			uint64_integer_alloc(local, ret, str->value.unsigned64);
			break;
#endif

		default:
			*ret = Nil;
			return fmte_("size error", NULL);
	}

	return 0;
}

int arrayvalue_alloc_(LocalRoot local, addr *ret, const struct array_value *str)
{
	switch (str->type) {
		case ARRAY_TYPE_T:
			copylocal_object(local, ret, str->value.object);
			break;

		case ARRAY_TYPE_BIT:
			fixnum_alloc(local, ret, (str->value.bit)? 1: 0);
			break;

		case ARRAY_TYPE_CHARACTER:
			character_alloc(local, ret, str->value.character);
			break;

		case ARRAY_TYPE_SIGNED:
			return arrayvalue_make_signed_(local, ret, str);

		case ARRAY_TYPE_UNSIGNED:
			return arrayvalue_make_unsigned_(local, ret, str);

		case ARRAY_TYPE_SINGLE_FLOAT:
			single_float_alloc(local, ret, str->value.single_value);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			double_float_alloc(local, ret, str->value.double_value);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			long_float_alloc(local, ret, str->value.long_value);
			break;

		default:
			return fmte_("Invalid array value.", NULL);
	}

	return 0;
}

int arrayvalue_local_(LocalRoot local, addr *ret, const struct array_value *str)
{
	Check(local == NULL, "local error");
	return arrayvalue_alloc_(local, ret, str);
}

int arrayvalue_heap_(addr *ret, const struct array_value *str)
{
	return arrayvalue_alloc_(NULL, ret, str);
}


/************************************************************
 *  array_vector.c
 ************************************************************/

int vector_type_p(addr pos)
{
	return (GetType(pos) == LISPTYPE_VECTOR)
		|| stringp(pos)
		|| array_vector_p(pos);
}


/*
 *  vector
 */
static int vector_pop_array_(Execute ptr, addr pos, addr *ret)
{
	struct array_struct *str;

	Check(! array_vector_p(pos), "type error");
	str = ArrayInfoStruct(pos);
	if (! str->fillpointer) {
		*ret = Nil;
		return call_type_error_fill_pointer_(ptr, pos);
	}
	if (str->front == 0) {
		*ret = Nil;
		return call_type_error_fill_pointer_zero_(ptr, pos);
	}
	str->front--;
	return array_get_(NULL, pos, str->front, ret);
}

int vector_pop_common_(Execute ptr, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return vector_pop_array_(ptr, pos, ret);

		case LISPTYPE_VECTOR:
			*ret = Nil;
			return call_type_error_fill_pointer_(ptr, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, VECTOR);
	}
}

static int vector_push_array_(Execute ptr, addr pos, addr value, addr *ret)
{
	struct array_struct *str;

	Check(! array_vector_p(pos), "type error");
	str = ArrayInfoStruct(pos);
	if (! str->fillpointer) {
		*ret = Nil;
		return call_type_error_fill_pointer_(ptr, pos);
	}
	if (str->size <= str->front) {
		return Result(ret, Nil);
	}

	Return(array_set_(pos, str->front, value));
	make_index_integer_heap(ret, str->front);
	str->front++;

	return 0;
}

int vector_push_common_(Execute ptr, addr value, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return vector_push_array_(ptr, pos, value, ret);

		case LISPTYPE_VECTOR:
			return call_type_error_fill_pointer_(ptr, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, VECTOR);
	}
}

static int vector_push_extend_resize_(addr pos, size_t fill, size_t size)
{
	return array_adjust_array_(&pos, pos, intsizeh(size),
			Unbound, Unbound, Unbound,
			intsizeh(fill), Nil, fixnumh(0));
}

static int vector_push_extension_(addr extension, size_t *rsize, int *ret)
{
	if (extension == Unbound) {
		*rsize = 0;
		return Result(ret, 1);
	}
	if (GetIndex_integer(extension, rsize)) {
		*rsize = 0;
		*ret = 0;
		return fmte_("Invalid extension value ~S.", extension, NULL);
	}

	return Result(ret, 0);
}

static int vector_push_extend_normal_(addr pos, addr extension)
{
	int check;
	struct array_struct *str;
	size_t size;

	/* argument */
	str = ArrayInfoStruct(pos);
	Return(vector_push_extension_(extension, &size, &check));
	if (check)
		size = 16;
	if (size < 16)
		size = 16;
	size += str->size;
	/* allocate */
	Return(vector_push_extend_resize_(pos, str->size, size));
	/* size */
	str->size = size;

	return 0;
}

static int vector_push_extend1_(Execute ptr,
		addr pos, addr value, addr extension, addr *ret)
{
	struct array_struct *str;

	Check(! array_vector_p(pos), "type error");
	str = ArrayInfoStruct(pos);
	if (! str->fillpointer) {
		*ret = Nil;
		return call_type_error_fill_pointer_(ptr, pos);
	}
	if (! str->adjustable) {
		*ret = Nil;
		return call_type_error_adjustable_(ptr, pos);
	}
	Return(vector_push_extend_normal_(pos, extension));
	Return(array_set_(pos, str->front, value));
	make_index_integer_heap(ret, str->front);
	str->front++;

	return 0;
}

static int vector_push_extend_array_(Execute ptr,
		addr pos, addr value, addr extension, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->front < str->size)
		return vector_push_array_(ptr, pos, value, ret);
	else
		return vector_push_extend1_(ptr, pos, value, extension, ret);
}

int vector_push_extend_common_(Execute ptr,
		addr value, addr pos, addr extension, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return vector_push_extend_array_(ptr, pos, value, extension, ret);

		case LISPTYPE_VECTOR:
			*ret = Nil;
			return call_type_error_fill_pointer_(ptr, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, VECTOR);
	}
}

int vector_get_(addr pos, size_t index, addr *ret)
{
	size_t size;

	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	if (size <= index) {
		*ret = Nil;
		return fmte_("Out of range ~S.", intsizeh(size), NULL);
	}
	getarray(pos, index, ret);

	return 0;
}

int vector_aref_(addr pos, addr args, addr *ret)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_VECTOR);
	if (! consp(args)) {
		*ret = Nil;
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	}
	GetCons(args, &arg, &args);
	if (args != Nil) {
		*ret = Nil;
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	}
	if (! integerp(arg)) {
		*ret = Nil;
		return fmte_("AREF argument ~S must be a non-negative integer.", arg, NULL);
	}
	if (GetIndex_integer(arg, &index)) {
		*ret = Nil;
		return fmte_("Invalid index arg ~S.", arg, NULL);
	}

	return vector_get_(pos, index, ret);
}

int vector_set_(addr pos, size_t index, addr value)
{
	size_t size;

	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	if (size <= index)
		return fmte_("Out of range ~S.", intsizeh(size), NULL);
	setarray(pos, index, value);

	return 0;
}

int vector_setf_aref_(addr pos, addr args, addr value)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_VECTOR);
	if (GetStatusReadOnly(pos))
		return fmte_("The object ~S is constant.", pos, NULL);
	if (! consp(args))
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	GetCons(args, &arg, &args);
	if (args != Nil)
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	if (! integerp(arg))
		return fmte_("AREF argument ~S must be a non-negative integer.", arg, NULL);
	if (GetIndex_integer(arg, &index))
		return fmte_("Invalid index arg ~S.", arg, NULL);

	return vector_set_(pos, index, value);
}

int vector_array_dimension_(addr pos, addr arg, size_t size, addr *ret)
{
	size_t check;

	if (! integerp(arg)) {
		*ret = Nil;
		return fmte_("ARRAY-DIMENSION argument ~S must be integer type.", arg, NULL);
	}
	if (GetIndex_integer(arg, &check)) {
		*ret = Nil;
		return fmte_("Invalid index arg ~S.", arg, NULL);
	}
	if (check != 0) {
		*ret = Nil;
		return fmte_("Array rank ~A must be less than equal to 1.", arg, NULL);
	}
	make_index_integer_heap(ret, size);

	return 0;
}

void vector_array_dimensions(size_t size, addr *ret)
{
	addr pos;
	make_index_integer_heap(&pos, size);
	conscar_heap(ret, pos);
}

int vector_array_in_bounds_p_(addr rest, size_t size, int *ret)
{
	addr pos;
	size_t check;

	if (! consp(rest)) {
		*ret = 0;
		return fmte_("The subscripts ~S is too few argumens.", rest, NULL);
	}
	GetCons(rest, &pos, &rest);
	if (rest != Nil) {
		*ret = 0;
		return fmte_("The subscripts ~S is too many argumens.", rest, NULL);
	}
	if (! integerp(pos)) {
		*ret = 0;
		return fmte_("The subscript ~S must be integer type.", pos, NULL);
	}
	if (GetIndex_integer(pos, &check))
		return Result(ret, 0);
	else
		return Result(ret, check < size);
}

int vector_array_row_major_index_(addr rest, size_t size, addr *ret)
{
	int check;

	Return(vector_array_in_bounds_p_(rest, size, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Out of range ~S.", intsizeh(size), NULL);
	}
	GetCar(rest, ret);

	return 0;
}

static void vector_settype(addr pos, enum ARRAY_TYPE type, int size)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	str->type = type;
	str->bytesize = size;
	array_set_type(pos);
	array_set_element_size(pos);
}

static void vector_dimension(addr pos, size_t size)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->size = str->front = size;
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, Nil);
}

static int vector_type_(addr *ret,
		size_t size, enum ARRAY_TYPE type, int bs, addr value)
{
	addr pos;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	vector_settype(pos, type, bs);
	/* dimension */
	vector_dimension(pos, size);
	/* allocate */
	Return(array_make_memory_(pos, Nil, Nil, Nil, Nil));
	/* initial value */
	Return(array_make_initial_(pos, value, Unbound));
	/* result */
	return Result(ret, pos);
}

static int vector_signed_check_(enum ARRAY_TYPE type, int bytesize)
{
	switch (type) {
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			break;

		default:
			return fmte_("Invalid vector type.", NULL);
	}

	switch (bytesize) {
		case 8:
		case 16:
		case 32:
#ifdef LISP_64BIT
		case 64:
#endif
			break;

		default:
			return fmte_("Invalide vector type (size).", NULL);
	}

	return 0;
}

int vector_signed_uninit_(addr *ret, size_t size, enum ARRAY_TYPE type, int bs)
{
	Return(vector_signed_check_(type, bs));
	return vector_type_(ret, size, type, bs, Unbound);
}

int vector_signed_(addr *ret, size_t size, enum ARRAY_TYPE type, int bs, addr value)
{
	Return(vector_signed_check_(type, bs));
	if (value == Unbound)
		fixnum_heap(&value, 0);
	return vector_type_(ret, size, type, bs, value);
}

int vector_float_uninit_(addr *ret, size_t size, enum ARRAY_TYPE type)
{
	switch (type) {
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			break;

		default:
			return fmte_("Invalid vector type.", NULL);
	}

	return vector_type_(ret, size, type, 0, Unbound);
}

int vector_float_(addr *ret, size_t size, enum ARRAY_TYPE type, addr value)
{
	switch (type) {
		case ARRAY_TYPE_SINGLE_FLOAT:
			if (value == Unbound)
				single_float_heap(&value, 0.0f);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			if (value == Unbound)
				double_float_heap(&value, 0.0);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			if (value == Unbound)
				long_float_heap(&value, 0.0L);
			break;

		default:
			*ret = Nil;
			return fmte_("Invalid vector type.", NULL);
	}

	return vector_type_(ret, size, type, 0, value);
}

int vector_setelt_(addr pos, size_t index, addr value)
{
	size_t size;

	lenarray(pos, &size);
	if (size <= index) {
		return fmte_("Index ~A must be less than vector size ~A.",
				intsizeh(index), intsizeh(size), NULL);
	}
	setarray(pos, index, value);

	return 0;
}

void vector_reverse(LocalRoot local, addr *ret, addr pos)
{
	addr one, temp;
	size_t size, x, y;

	lenarray(pos, &size);
	vector_alloc(local, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		getarray(pos, x, &temp);
		setarray(one, y, temp);
	}
	*ret = one;
}

void vector_nreverse(addr *ret, addr pos)
{
	addr a, b;
	size_t size, x, y;

	lenarray(pos, &size);
	if (size <= 1)
		return;
	x = 0;
	y = size - 1;
	while (x < y) {
		getarray(pos, x, &a);
		getarray(pos, y, &b);
		setarray(pos, x, b);
		setarray(pos, y, a);
		x++;
		y--;
	}
	*ret = pos;
}

int vector_copy_heap_(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return array_copy_heap_(ret, pos);

		case LISPTYPE_VECTOR:
			copy_vector_heap(ret, pos);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, VECTOR);
	}
}


/************************************************************
 *  bignum.c
 ************************************************************/

/*
 *  power
 */
void power2_bignum_alloc(LocalRoot local, addr *ret, int sign, size_t value)
{
	addr pos;
	power2_bigdata_alloc(local, &pos, value);
	SetSignBignum(pos, sign);
	bignum_result_alloc(local, pos, ret);
}

void power2_bignum_local(LocalRoot local, addr *ret, int sign, size_t value)
{
	Check(local == NULL, "local error");
	power2_bignum_alloc(local, ret, sign, value);
}

void power2_bignum_heap(addr *ret, int sign, size_t value)
{
	power2_bignum_alloc(NULL, ret, sign, value);
}

void shiftup_bignum_alloc(LocalRoot local, addr *ret, addr left, size_t value)
{
	shiftup_bigdata_alloc(local, &left, left, value);
	bignum_result_alloc(local, left, ret);
}

void shiftup_bignum_local(LocalRoot local, addr *ret, addr left, size_t value)
{
	Check(local == NULL, "local error");
	shiftup_bignum_alloc(local, ret, left, value);
}

void shiftup_bignum_heap(addr *ret, addr left, size_t value)
{
	shiftup_bignum_alloc(NULL, ret, left, value);
}


/*
 *  integer
 */
static int castfixnum(int sign, fixed value, fixnum *result)
{
	Check(sign != SignPlus && sign != SignMinus, "sign error");

	if (IsPlus(sign)) {
		if (value <= FIXNUM_MAX) {
			*result = (fixnum)value;
			return 1;
		}
	}
	else {
		if (value <= FIXNUM_MAX) {
			*result = -(fixnum)value;
			return 1;
		}
		if (value == FIXNUM_UMIN) {
			*result = FIXNUM_MIN;
			return 1;
		}
	}

	return 0;
}

#ifdef BIGNUM_FULLCODE
int fixnum_cons_alloc(LocalRoot local, addr *ret, int sign, addr cons)
{
	fixnum result;
	addr root;
	struct bigbuffer *str;
	size_t size;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	GetCountBigcons(cons, &size);
	if (size == 1) {
		GetRootBigcons(cons, &root);
		str = StructBigbuffer(root);
		if (castfixnum(sign, str->buffer[0], &result)) {
			fixnum_alloc(local, ret, result);
			return 0;
		}
	}

	return 1;
}

#else
int fixnum_cons_alloc(LocalRoot local, addr *ret, int sign, addr cons)
{
	fixed value;
	fixnum result;
	addr root;
	struct bigbuffer *str;
	size_t size, i, rem;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	GetCountBigcons(cons, &size);
	if (size <= (LISP_INTEGER_BIT / BIGNUM_FULLBIT)) {
		GetRootBigcons(cons, &root);
		str = StructBigbuffer(root);
		value = 0;
		for (i = 0; i < size; i++) {
			rem = i % BIGCONS_SIZE;
			if (i && rem == 0) {
				GetNextBigbuffer(root, &root);
				str = StructBigbuffer(root);
			}
			value |= str->buffer[rem] << (i * BIGNUM_FULLBIT);
		}
		if (castfixnum(sign, value, &result)) {
			fixnum_alloc(local, ret, result);
			return 0;
		}
	}

	return 1;
}
#endif

int fixnum_cons_local(LocalRoot local, addr *ret, int sign, addr cons)
{
	Check(local == NULL, "local error");
	return fixnum_cons_alloc(local, ret, sign, cons);
}

int fixnum_cons_heap(addr *ret, int sign, addr cons)
{
	return fixnum_cons_alloc(NULL, ret, sign, cons);
}

void integer_cons_alloc(LocalRoot local, addr *ret, int sign, addr cons)
{
	if (fixnum_cons_alloc(local, ret, sign, cons))
		bignum_cons_alloc(local, ret, sign, cons);
}

void integer_cons_local(LocalRoot local, addr *ret, int sign, addr cons)
{
	Check(local == NULL, "local error");
	integer_cons_alloc(local, ret, sign, cons);
}

void integer_cons_heap(addr *ret, int sign, addr cons)
{
	integer_cons_alloc(NULL, ret, sign, cons);
}

void integer_fixed_alloc(LocalRoot local, addr *ret, int sign, fixed value)
{
	fixnum result;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	if (castfixnum(sign, value, &result)) {
		fixnum_alloc(local, ret, result);
	}
	else {
		bignum_value_alloc(local, ret, sign, value);
	}
}

void integer_fixed_local(LocalRoot local, addr *ret, int sign, fixed value)
{
	Check(local == NULL, "local error");
	integer_fixed_alloc(local, ret, sign, value);
}

void integer_fixed_heap(addr *ret, int sign, fixed value)
{
	integer_fixed_alloc(NULL, ret, sign, value);
}

void integer_bignum_alloc(LocalRoot local, addr *ret, addr pos)
{
	int sign;
	size_t size;

	GetSizeBignum(pos, &size);
	if (size == 1) {
		GetSignBignum(pos, &sign);
		GetRootBignum(pos, &pos);
		integer_fixed_alloc(local, ret, sign, PtrDataBignum(pos)[0]);
	}
	else {
		bignum_throw_alloc(local, pos, ret);
	}
}

void integer_bignum_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	integer_bignum_alloc(local, ret, pos);
}

void integer_bignum_heap(addr *ret, addr pos)
{
	integer_bignum_alloc(NULL, ret, pos);
}


/*
 *  integer-copy
 */
static void fixnum_copysign_alloc(LocalRoot local, int sign, fixnum value, addr *ret)
{
	if (value == FIXNUM_MIN) {
		if (IsPlus(sign))
			bignum_value_alloc(local, ret, SignPlus, FIXNUM_UMIN);
		else
			fixnum_alloc(local, ret, value);
	}
	else if (IsPlus(sign)) {
		if (0 <= value)
			fixnum_alloc(local, ret, value);
		else
			fixnum_alloc(local, ret, -value);
	}
	else {
		if (0 <= value)
			fixnum_alloc(local, ret, -value);
		else
			fixnum_alloc(local, ret, value);
	}
}

static void bignum_copysign_alloc(LocalRoot local, int sign, addr pos, addr *ret)
{
	bignum_copy_nosign_alloc(local, &pos, pos);
	SetSignBignum(pos, sign);
	*ret = pos;
}

int integer_copysign_alloc_(LocalRoot local, int sign, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_copysign_alloc(local, sign, RefFixnum(pos), ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_copysign_alloc(local, sign, pos, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}

	return 0;
}

int integer_copysign_local_(LocalRoot local, int sign, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return integer_copysign_alloc_(local, sign, pos, ret);
}

int integer_copysign_heap_(int sign, addr pos, addr *ret)
{
	return integer_copysign_alloc_(NULL, sign, pos, ret);
}


/*****************************************************************************
  float
 *****************************************************************************/
#define HexToChar(x) (((x) < 10)? ('0' + (x)): ('A' - 10 + (x)))
#define HEXCHAR_BIGTYPE_SIZE (BIGNUM_FULLBIT >> 2)

static char *hexchar_fixed(char *dst, fixed v)
{
	int i, c;

	for (i = 0; i < HEXCHAR_BIGTYPE_SIZE; i++) {
		c = (int)(v & 0x0F);
		dst[HEXCHAR_BIGTYPE_SIZE - 1 - i] = HexToChar(c);
		v >>= 4;
	}

	return dst + HEXCHAR_BIGTYPE_SIZE;
}

static char *hexfraction_string(char *dst, addr pos, int frac, size_t *exponent)
{
	size_t i, size, tail;
	fixed *data;

	GetSizeBignum(pos, &size);
	GetRootDataBignum(pos, &pos, &data);
	tail = size - 1;

	for (i = 0; ; i++, tail--) {
		if (size <= i) {
			*exponent = 0;
			break;
		}
		if (frac <= 0) {
			dst = hexchar_fixed(dst, data[tail]);
			*exponent = tail * BIGNUM_FULLBIT;
			break;
		}
		dst = hexchar_fixed(dst, data[tail]);
		frac -= BIGNUM_FULLBIT;
	}

	return dst;
}

static char *expchar_make_float(char *dst, size_t size)
{
	size_t i, m;
	char buffer[32];

	if (size == 0) {
		dst[0] = '0';
		return dst + 1;
	}
	for (i = 0; size; i++) {
		buffer[i] = '0' + (size % 10);
		size /= 10;
	}
	for (m = 0; m < i; m++) {
		dst[i - m - 1] = buffer[m];
	}

	return dst + i;
}

static void make_float_string(char *dst, addr pos, int size)
{
	int sign;
	size_t exponent;

	GetSignBignum(pos, &sign);
	*(dst++) = IsPlus(sign)? '+': '-';
	*(dst++) = '0';
	*(dst++) = 'x';
	dst = hexfraction_string(dst, pos, size, &exponent);
	*(dst++) = 'p';
	dst = expchar_make_float(dst, exponent);
	*dst = '\0';
}

single_float single_float_fixnum(addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	return (single_float)RefFixnum(pos);
}

double_float double_float_fixnum(addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	return (double_float)RefFixnum(pos);
}

long_float long_float_fixnum(addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	return (long_float)RefFixnum(pos);
}

int single_float_bignum_(addr pos, single_float *ret)
{
	char buffer[64];
	int sign;
	size_t size;
	single_float value;

	GetSizeBignum(pos, &size);
	if (size == 1) {
		GetSignBignum(pos, &sign);
		GetRootBignum(pos, &pos);
		value = (single_float)PtrDataBignum(pos)[0];
		return Result(ret, IsMinus(sign)? -value: value);
	}
	make_float_string(buffer, pos, LISP_FLOAT_SINGLE_FRACTION);

	return check_strtof_(buffer, pos, ret);
}

int double_float_bignum_(addr pos, double_float *ret)
{
	char buffer[64];
	int sign;
	size_t size;
	double_float value;

	GetSizeBignum(pos, &size);
	if (size == 1) {
		GetSignBignum(pos, &sign);
		GetRootBignum(pos, &pos);
		value = (double_float)PtrDataBignum(pos)[0];
		return Result(ret, IsMinus(sign)? -value: value);
	}
	make_float_string(buffer, pos, LISP_FLOAT_DOUBLE_FRACTION);

	return check_strtod_(buffer, pos, ret);
}

int long_float_bignum_(addr pos, long_float *ret)
{
	char buffer[64];
	int sign;
	size_t size;
	long_float value;

	GetSizeBignum(pos, &size);
	if (size == 1) {
		GetSignBignum(pos, &sign);
		GetRootBignum(pos, &pos);
		value = (long_float)PtrDataBignum(pos)[0];
		return Result(ret, IsMinus(sign)? -value: value);
	}
	make_float_string(buffer, pos, LISP_FLOAT_LONG_FRACTION);

	return check_strtold_(buffer, pos, ret);
}

void single_float_fixnum_alloc(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	single_float_alloc(local, ret, single_float_fixnum(pos));
}
void single_float_fixnum_local(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	single_float_local(local, ret, single_float_fixnum(pos));
}
void single_float_fixnum_heap(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	single_float_heap(ret, single_float_fixnum(pos));
}

void double_float_fixnum_alloc(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	double_float_alloc(local, ret, double_float_fixnum(pos));
}
void double_float_fixnum_local(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	double_float_local(local, ret, double_float_fixnum(pos));
}
void double_float_fixnum_heap(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	double_float_heap(ret, double_float_fixnum(pos));
}

void long_float_fixnum_alloc(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	long_float_alloc(local, ret, long_float_fixnum(pos));
}
void long_float_fixnum_local(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	long_float_local(local, ret, long_float_fixnum(pos));
}
void long_float_fixnum_heap(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	long_float_heap(ret, long_float_fixnum(pos));
}

int single_float_bignum_alloc_(LocalRoot local, addr *ret, addr pos)
{
	single_float value;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(single_float_bignum_(pos, &value));
	single_float_alloc(local, ret, value);

	return 0;
}
int single_float_bignum_local_(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return single_float_bignum_alloc_(local, ret, pos);
}
int single_float_bignum_heap_(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return single_float_bignum_alloc_(NULL, ret, pos);
}

int double_float_bignum_alloc_(LocalRoot local, addr *ret, addr pos)
{
	double_float value;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(double_float_bignum_(pos, &value));
	double_float_alloc(local, ret, value);

	return 0;
}
int double_float_bignum_local_(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return double_float_bignum_alloc_(local, ret, pos);
}
int double_float_bignum_heap_(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return double_float_bignum_alloc_(NULL, ret, pos);
}

int long_float_bignum_alloc_(LocalRoot local, addr *ret, addr pos)
{
	long_float value;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(long_float_bignum_(pos, &value));
	long_float_alloc(local, ret, value);

	return 0;
}
int long_float_bignum_local_(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return long_float_bignum_alloc_(local, ret, pos);
}
int long_float_bignum_heap_(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return long_float_bignum_alloc_(NULL, ret, pos);
}

static size_t printf_integer_float_size(int exp)
{
	size_t size;

	size = (31UL*exp / 100UL); /* log10(2) = 0.301 <= 0.31 */
	size += 1UL/*sign*/ + 1UL/*null*/ + 1UL/*1digit*/;

	return size;
}

void bignum_single_float_unsafe(
		LocalRoot local, single_float v, int is_heap, addr *ret)
{
	int expn, sign;
	addr pos;
	char *ptr;
	size_t size;

	Check(! IsIntegerFloat(v), "float error");
	sign = (v < 0.0f)? SignMinus: SignPlus;
	frexpf(v, &expn);
	Check(expn < 0, "exponent error");
	size = printf_integer_float_size(expn);
	ptr = (char *)lowlevel_local(local, size);
	snprintf(ptr, size, "%.0f", fabsf(v));
	bigcons_char_unsafe(local, &pos, 10, ptr);
	bignum_cons_alloc(is_heap? NULL: local, ret, sign, pos);
}

void bignum_double_float_unsafe(
		LocalRoot local, double_float v, int is_heap, addr *ret)
{
	int expn, sign;
	addr pos;
	char *ptr;
	size_t size;

	Check(! IsIntegerDouble(v), "float error");
	sign = (v < 0.0)? SignMinus: SignPlus;
	frexp(v, &expn);
	Check(expn < 0, "exponent error");
	size = printf_integer_float_size(expn);
	ptr = (char *)lowlevel_local(local, size);
	snprintf(ptr, size, "%.0f", fabs(v));
	bigcons_char_unsafe(local, &pos, 10, ptr);
	bignum_cons_alloc(is_heap? NULL: local, ret, sign, pos);
}

void bignum_long_float_unsafe(
		LocalRoot local, long_float v, int is_heap, addr *ret)
{
	int expn, sign;
	addr pos;
	char *ptr;
	size_t size;

	Check(! IsIntegerLongFloat(v), "float error");
	sign = (v < 0.0L)? SignMinus: SignPlus;
	frexpl(v, &expn);
	Check(expn < 0, "exponent error");
	size = printf_integer_float_size(expn);
	ptr = (char *)lowlevel_local(local, size);
	snprintf(ptr, size, "%.0Lf", fabsl(v));
	bigcons_char_unsafe(local, &pos, 10, ptr);
	bignum_cons_alloc(is_heap? NULL: local, ret, sign, pos);
}

static int bignum_single_float_alloc_(
		LocalRoot local, single_float v, int is_heap, addr *rv, int *ret)
{
	LocalStack stack;

	/* check */
	if (! IsIntegerFloat(v)) {
		if (ret)
			return Result(ret, 1);
		else
			return fmte_("Invalid float value.", NULL);
	}

	/* cast */
	stack = NULL;
	if (is_heap) {
		push_local(local, &stack);
	}
	bignum_single_float_unsafe(local, v, is_heap, rv);
	if (is_heap) {
		rollback_local(local, stack);
	}

	/* result */
	if (ret)
		return Result(ret, 0);

	return 0;
}

int bignum_single_float_local_(LocalRoot local, single_float v, addr *rv, int *ret)
{
	CheckLocal(local);
	return bignum_single_float_alloc_(local, v, 0, rv, ret);
}

int bignum_single_float_heap_(LocalRoot local, single_float v, addr *rv, int *ret)
{
	CheckLocal(local);
	return bignum_single_float_alloc_(local, v, 1, rv, ret);
}

static int bignum_double_float_alloc_(
		LocalRoot local, double_float v, int is_heap, addr *rv, int *ret)
{
	LocalStack stack;

	/* check */
	if (! IsIntegerDouble(v)) {
		if (ret)
			return Result(ret, 1);
		else
			return fmte_("Invalid float value.", NULL);
	}

	/* cast */
	stack = NULL;
	if (is_heap) {
		push_local(local, &stack);
	}
	bignum_double_float_unsafe(local, v, is_heap, rv);
	if (is_heap) {
		rollback_local(local, stack);
	}

	/* result */
	if (ret)
		return Result(ret, 0);

	return 0;
}

int bignum_double_float_local_(LocalRoot local, double_float v, addr *rv, int *ret)
{
	CheckLocal(local);
	return bignum_double_float_alloc_(local, v, 0, rv, ret);
}

int bignum_double_float_heap_(LocalRoot local, double_float v, addr *rv, int *ret)
{
	CheckLocal(local);
	return bignum_double_float_alloc_(local, v, 1, rv, ret);
}

static int bignum_long_float_alloc_(
		LocalRoot local, long_float v, int is_heap, addr *rv, int *ret)
{
	LocalStack stack;

	/* check */
	if (! IsIntegerLongFloat(v)) {
		if (ret)
			return Result(ret, 1);
		else
			return fmte_("Invalid float value.", NULL);
	}

	/* cast */
	stack = NULL;
	if (is_heap) {
		push_local(local, &stack);
	}
	bignum_long_float_unsafe(local, v, is_heap, rv);
	if (is_heap) {
		rollback_local(local, stack);
	}

	/* result */
	if (ret)
		return Result(ret, 0);

	return 0;
}

int bignum_long_float_local_(LocalRoot local, long_float v, addr *rv, int *ret)
{
	CheckLocal(local);
	return bignum_long_float_alloc_(local, v, 0, rv, ret);
}

int bignum_long_float_heap_(LocalRoot local, long_float v, addr *rv, int *ret)
{
	CheckLocal(local);
	return bignum_long_float_alloc_(local, v, 1, rv, ret);
}


/*
 *  getvalue
 */
int GetFixnum_bignum(addr pos, fixnum *ret)
{
	int sign;
	fixed value;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	if (RefSizeBignum(pos) != 1)
		return 1;
	GetSignBignum(pos, &sign);
	getfixed_bignum(pos, 0, &value);
	if (IsPlus(sign)) {
		if (FIXNUM_MAX < value)
			return 1;
		*ret = (fixnum)value;
		return 0;
	}
	else {
		if (FIXNUM_UMIN < value)
			return 1;
		*ret = -(fixnum)value;
		return 0;
	}

	return 1;
}

int GetFixnum_signed(addr pos, fixnum *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			return GetFixnum_bignum(pos, ret);

		default:
			break;
	}

	return 1;
}

int getfixnum_signed_(addr pos, fixnum *ret)
{
	if (GetFixnum_signed(pos, ret))
		return TypeError_(pos, FIXNUM);

	return 0;
}

int GetFixnum_unsigned(addr pos, fixnum *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				return 1;
			break;

		default:
			return 1;
	}
	if (v < 0)
		return 1;
	*ret = v;
	return 0;
}

int getfixnum_unsigned_(addr pos, fixnum *ret)
{
	addr type;

	if (GetFixnum_unsigned(pos, ret)) {
		type4integer_heap(Nil, 0, Nil, FIXNUM_MAX, &type);
		return call_type_error_(NULL, pos, type);
	}

	return 0;
}

static int getfixed1_fixnum(addr pos, int *sign, fixed *ret)
{
	fixnum v;

	CheckType(pos, LISPTYPE_FIXNUM);
	GetFixnum(pos, &v);
	if (v == FIXNUM_MIN) {
		*sign = signminus_bignum;
		*ret = (fixed)FIXNUM_MIN;
		return 0;
	}
	if (v < 0) {
		*sign = signminus_bignum;
		*ret = (fixed)-v;
	}
	else {
		*sign = signplus_bignum;
		*ret = (fixed)v;
	}

	return 0;
}

int getfixed1_bignum(addr pos, int *sign, fixed *ret)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	if (RefSizeBignum(pos) != 1)
		return 1;
	GetSignBignum(pos, sign);
	getfixed_bignum(pos, 0, ret);

	return 0;
}

int getfixed1_integer(addr pos, int *sign, fixed *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return getfixed1_fixnum(pos, sign, ret);

		case LISPTYPE_BIGNUM:
			return getfixed1_bignum(pos, sign, ret);

		default:
			*sign = signplus_bignum;
			*ret = 0;
			return 1;
	}
}

static int GetInt_bignum(addr pos, int *ret)
{
	int sign;
	fixed value;

	CheckType(pos, LISPTYPE_BIGNUM);
	if (RefSizeBignum(pos) != 1)
		return 1;
	GetSignBignum(pos, &sign);
	getfixed_bignum(pos, 0, &value);
	if (IsPlus(sign)) {
		if (INT_MAX < value)
			return 1;
		*ret = (int)value;
		return 0;
	}
	else {
		if (((fixed)INT_MIN) < value)
			return 1;
		*ret = -(int)value;
		return 0;
	}

	return 1;
}

#if (INT_MAX == FIXNUM_MAX)
static int GetInt_fixnum(addr pos, int *ret)
{
	fixnum value;

	CheckType(pos, LISPTYPE_FIXNUM);
	GetFixnum(pos, &value);
	return Result(ret, (int)value);
}
#else
static int GetInt_fixnum(addr pos, int *ret)
{
	fixnum value;

	CheckType(pos, LISPTYPE_FIXNUM);
	GetFixnum(pos, &value);
	if (INT_MAX < value)
		return 1;
	if (value < INT_MIN)
		return 1;
	return Result(ret, (int)value);
}
#endif

int GetInt_signed(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return GetInt_fixnum(pos, ret);

		case LISPTYPE_BIGNUM:
			return GetInt_bignum(pos, ret);

		default:
			break;
	}

	return 1;
}

int getint_signed_(addr pos, int *ret)
{
	addr type;

	if (GetInt_signed(pos, ret)) {
		type4integer_heap(Nil, INT_MIN, Nil, INT_MAX, &type);
		return call_type_error_(NULL, pos, type);
	}

	return 0;
}

int GetInt_unsigned(addr pos, int *ret)
{
	int v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			if (GetInt_fixnum(pos, &v))
				return 1;
			break;

		case LISPTYPE_BIGNUM:
			if (GetInt_bignum(pos, &v))
				return 1;
			break;

		default:
			return 1;
	}
	if (v < 0)
		return 1;
	*ret = v;
	return 0;
}

int getint_unsigned_(addr pos, int *ret)
{
	addr type;

	if (GetInt_unsigned(pos, ret)) {
		type4integer_heap(Nil, 0, Nil, INT_MAX, &type);
		return call_type_error_(NULL, pos, type);
	}

	return 0;
}


/*
 *  math
 */
void abs_fixnum_integer_local(LocalRoot local, addr left, addr *ret)
{
	fixnum value;

	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_FIXNUM);
	GetFixnum(left, &value);
	if (0 <= value)
		fixnum_throw_local(local, left, ret);
	else if (value == FIXNUM_MIN)
		bignum_value_local(local, ret, SignPlus, FIXNUM_UMIN);
	else
		fixnum_local(local, ret, -value);
}

void abs_fixnum_integer_common(addr left, addr *ret)
{
	fixnum value;

	CheckType(left, LISPTYPE_FIXNUM);
	GetFixnum(left, &value);
	if (0 <= value)
		fixnum_throw_heap(left, ret);
	else if (value == FIXNUM_MIN)
		bignum_value_heap(ret, SignPlus, FIXNUM_UMIN);
	else
		fixnum_heap(ret, -value);
}

void abs_bignum_integer_local(LocalRoot local, addr left, addr *ret)
{
	int sign;

	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_BIGNUM);
	GetSignBignum(left, &sign);
	if (IsPlus(sign))
		bignum_throw_local(local, left, ret);
	else
		bignum_copy_nosign_local(local, ret, left);
}

void abs_bignum_integer_common(addr left, addr *ret)
{
	int sign;

	CheckType(left, LISPTYPE_BIGNUM);
	GetSignBignum(left, &sign);
	if (IsPlus(sign))
		bignum_throw_heap(left, ret);
	else
		bignum_copy_nosign_heap(ret, left);
}


/************************************************************
 *  bignum_cons.c
 ************************************************************/
/*
 *  bigcons
 */

static void bigbuffer_local(LocalRoot local, addr *ret, fixed value)
{
	addr pos;
	struct bigbuffer *ptr;

	Check(0xFFFF < sizeoft(struct bigbuffer), "size error");
	local_arraybody(local, &pos, LISPSYSTEM_BIGBUFFER, 1, sizeof(struct bigbuffer));
	ptr = StructBigbuffer(pos);
#ifdef LISP_DEBUG
	aamemory(ptr, sizeof(struct bigbuffer));
#endif
	ptr->count = 1;
	ptr->buffer[0] = value;
	*ret = pos;
}

void bigcons_local(LocalRoot local, addr *ret)
{
	addr cons, pos;

	bigbuffer_local(local, &cons, 0);
	local_smallsize(local, &pos, LISPSYSTEM_BIGCONS, 1, sizeoft(struct bigcons_struct));
	SetUser(pos, 0);
	SetCountBigcons(pos, 1);
	SetRootBigcons(pos, cons);
	*ret = pos;
}

void clear_bigcons(addr cons)
{
	addr child, next;
	struct bigbuffer *ptr;

	GetRootBigcons(cons, &child);
	GetNextBigbuffer(child, &next);
	if (next != Nil)
		StructBigbuffer(next)->count = 0;
	ptr = StructBigbuffer(child);
	ptr->buffer[0] = 0;
	ptr->count = 1;
	SetUser(cons, 0);
	SetCountBigcons(cons, 1);
}

static void carrynext(LocalRoot local,
		addr cons, addr root, size_t len, fixed carry)
{
	addr next;
	struct bigbuffer *ptr;

	if (carry && root != Nil) {
		GetNextBigbuffer(root, &next);
		if (len < BIGCONS_SIZE) {
			ptr = StructBigbuffer(root);
			ptr->buffer[len] = carry;
			ptr->count++;
		}
		else if (next != Nil) {
			ptr = StructBigbuffer(next);
			ptr->count = 1;
			ptr->buffer[0] = carry;
			GetNextBigbuffer(next, &next);
			if (next != Nil)
				StructBigbuffer(next)->count = 0;
		}
		else {
			bigbuffer_local(local, &next, carry);
			SetNextBigbuffer(root, next);
		}
		IncCountBigcons(cons, 1);
	}
}

static void plus_bigcons(LocalRoot local, addr cons, fixed carry)
{
	fixed *buffer;
	addr root, prev, next;
	size_t i, len;
	struct bigbuffer *ptr;

	if (carry == 0)
		return;
	len = 0;
	GetRootBigcons(cons, &root);
	for (prev = root; root != Nil; root = next) {
		ptr = StructBigbuffer(root);
		len = ptr->count;
		if (len == 0) {
			carrynext(local, cons, root, 0, carry);
			return;
		}
		buffer = ptr->buffer;
		for (i = 0; i < len; i++) {
			plusnumber_bigdata(&buffer[i], &carry);
			if (carry == 0)
				return;
		}
		prev = root;
		GetNextBigbuffer(root, &next);
	}
	carrynext(local, cons, prev, len, carry);
}

static void multi_bigcons(LocalRoot local, addr cons, fixed value)
{
	fixed carry, *buffer;
	addr root, prev, next;
	size_t i, len;
	struct bigbuffer *ptr;

	carry = 0;
	len = 0;
	GetRootBigcons(cons, &root);
	for (prev = root; root != Nil; root = next) {
		ptr = StructBigbuffer(root);
		len = ptr->count;
		if (len == 0) {
			carrynext(local, cons, root, 0, carry);
			return;
		}
		buffer = ptr->buffer;
		for (i = 0; i < len; i++)
			multicarry_bigdata(&buffer[i], value, &carry);
		prev = root;
		GetNextBigbuffer(root, &next);
	}
	carrynext(local, cons, prev, len, carry);
}

void push_bigcons(LocalRoot local, addr cons, unsigned base, unsigned digit)
{
	addr root;
	size_t count;
	fixed *buffer;

	Check(! isBaseChar(base), "base error");
	Check(base <= digit, "digit error");
	Check(BIGNUM_FULL <= (size_t)digit, "fullsize error");

	SetUser(cons, 1);
	GetCountBigcons(cons, &count);
	if (count == 1) {
		GetRootBigcons(cons, &root);
		buffer = StructBigbuffer(root)->buffer;
		if (buffer[0] == 0) {
			buffer[0] = (fixed)digit;
			return;
		}
	}
	multi_bigcons(local, cons, (fixed)base);
	plus_bigcons(local, cons, (fixed)digit);
}

static int getnumber(unsigned base, int c, unsigned *ret)
{
	if ('0' <= c && c <= '9') {
		c -= '0';
	}
	else if ('A' <= c && c <= 'Z') {
		c = (c - 'A') + 10;
	}
	else if ('a' <= c && c <= 'z') {
		c = (c - 'a') + 10;
	}
	else {
		return 1;
	}

	if (base <= (unsigned)c) {
		return 1;
	}
	*ret = (unsigned)c;

	return 0;
}

int setchar_bigcons_(LocalRoot local, addr pos, unsigned base, const char *value)
{
	int c;
	unsigned ret;
	addr x;
	size_t i;

	clear_bigcons(pos);
	for (i = 0; ; i++) {
		c = value[i];
		if (c == '\0')
			break;
		if (getnumber(base, c, &ret)) {
			character_heap(&x, (unicode)c);
			return fmte_("Invalid digit character ~S.", x, NULL);
		}
		push_bigcons(local, pos, base, ret);
	}

	return 0;
}

int bigcons_char_local_(LocalRoot local, addr *ret, unsigned base, const char *value)
{
	bigcons_local(local, ret);
	return setchar_bigcons_(local, *ret, base, value);
}

static void setchar_bigcons_unsafe(LocalRoot local,
		addr pos, unsigned base, const char *value)
{
	int c;
	unsigned ret;
	size_t i;

	clear_bigcons(pos);
	for (i = 0; ; i++) {
		c = value[i];
		if (c == '\0')
			break;
		if (getnumber(base, c, &ret)) {
			Abort("Invalid digit.");
			return;
		}
		push_bigcons(local, pos, base, ret);
	}
}

void bigcons_char_unsafe(LocalRoot local, addr *ret, unsigned base, const char *value)
{
	bigcons_local(local, ret);
	setchar_bigcons_unsafe(local, *ret, base, value);
}

int bigcons_empty_p(addr pos)
{
	return GetUser(pos) == 0;
}


/************************************************************
 *  bignum_data.c
 ************************************************************/

/*****************************************************************************
  operator
 *****************************************************************************/
static void multinumber3(fixed *result, fixed rvalue, fixed *carry)
{
	/*     2 1  [rvalue]
	 *     2 1  [cvalue]
	 *   -----
	 *   a b c
	 * d e f
	 * -------
	 * C C R R
	 */
	fixed cvalue, r1, r2, c1, c2, a, b, c, d, e, f;

	cvalue = *carry;
	if (rvalue == 0 || cvalue == 0) {
		*result = *carry = 0;
		return;
	}
	if (rvalue == 1) {
		*result = cvalue;
		*carry = 0;
		return;
	}
	if (cvalue == 1) {
		*result = rvalue;
		*carry = 0;
		return;
	}

	r2 = HIGHVALUE(rvalue);
	c2 = HIGHVALUE(cvalue);
	if (r2 == 0 && c2 == 0) {
		*result = rvalue * cvalue;
		*carry = 0;
		return;
	}
	r1 = LOWVALUE(rvalue);
	c1 = LOWVALUE(cvalue);
	if (c2 == 0) {
		c = r1 * c1;
		b = r2 * c1;
		b += HIGHVALUE(c);
		a = HIGHVALUE(b);
		b = LOWVALUE(b);
		c = LOWVALUE(c);
		*result = HIGHLOW(b, c);
		*carry = a;
		return;
	}
	if (r2 == 0) {
		c = r1 * c1;
		b = r1 * c2;
		b += HIGHVALUE(c);
		a = HIGHVALUE(b);
		b = LOWVALUE(b);
		c = LOWVALUE(c);
		*result = HIGHLOW(b, c);
		*carry = a;
		return;
	}

	/* first */
	c = r1 * c1;
	b = r2 * c1;
	b += HIGHVALUE(c);
	a = HIGHVALUE(b);
	b = LOWVALUE(b);
	c = LOWVALUE(c);

	/* second */
	f = r1 * c2;
	e = r2 * c2;
	e += HIGHVALUE(f);
	d = HIGHVALUE(e);
	e = LOWVALUE(e);
	f = LOWVALUE(f);

	/* result
	 *   a b c
	 * d e f
	 * -------
	 * C C R R
	 */
	b += f;
	*result = HIGHLOW(LOWVALUE(b), c);
	a += e + HIGHVALUE(b);
	*carry = HIGHLOW(d + HIGHVALUE(a), LOWVALUE(a));
}

static inline void multinumber(fixed *result, fixed *carry)
{
	multinumber3(result, *result, carry);
}

static inline void plusnumber(fixed *result, fixed *carry)
{
	/*
	 *  *ret = c1 + c2;
	 *  return (*ret < c1);
	 */
#ifdef BIGNUM_FULLCODE
	*result += *carry;
	*carry = *result < *carry;
#else
	*result += *carry;
	*carry = (*result >> BIGNUM_FULLBIT) != 0;
	*result &= BIGNUM_FULL;
#endif
}

void plusnumber_bigdata(fixed *result, fixed *carry)
{
	plusnumber(result, carry);
}

static inline void plusnumber3(fixed *result, fixed value, fixed *carry)
{
	/*
	 *  *ret = c1 + c2;
	 *  return (*ret < c1);
	 */
#ifdef BIGNUM_FULLCODE
	*result = value + *carry;
	*carry = *result < *carry;
#else
	*result = value + *carry;
	*carry = (*result >> BIGNUM_FULLBIT) != 0;
	*result &= BIGNUM_FULL;
#endif
}

static void pluscarry(fixed *result, fixed value, fixed *carry)
{
	plusnumber(result, &value);
	plusnumber(result, carry);
	*carry += value;
}

static inline void pluscarry4(fixed *result, fixed left, fixed right, fixed *carry)
{
	plusnumber3(result, left, &right);
	plusnumber(result, carry);
	*carry += right;
}

static inline void multicarry(fixed *result, fixed value, fixed *carry)
{
	multinumber(result, &value);
	plusnumber(result, carry);
	*carry += value;
}

void multicarry_bigdata(fixed *result, fixed value, fixed *carry)
{
	multicarry(result, value, carry);
}

static void inline multicarry4(fixed *result, fixed left, fixed right, fixed *carry)
{
	multinumber3(result, left, &right);
	plusnumber(result, carry);
	*carry += right;
}


/*****************************************************************************
  compare
 *****************************************************************************/
int equal_bigdata(addr left, addr right)
{
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	if (size1 != size2)
		return 0;
	GetRootBignum(left, &left);
	GetRootBignum(right, &right);
	return bigcmp(PtrDataBignum(left), PtrDataBignum(right), size1) == 0;
}

int compare_bigdata(addr left, addr right)
{
	const fixed *data1, *data2;
	size_t size1, size2, i, index;
	fixed value1, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	if (size1 != size2)
		return (size1 < size2)? -1: 1;
	GetRootDataBignum(left, &left, &data1);
	GetRootDataBignum(right, &right, &data2);
	for (i = 0; i < size1; i++) {
		index = size1 - i - 1;
		value1 = data1[index];
		value2 = data2[index];
		if (value1 < value2)
			return -1;
		if (value1 > value2)
			return 1;
	}

	return 0;
}


/*****************************************************************************
  plus / minus
 *****************************************************************************/
static inline void minusnumber(fixed *result, fixed *carry)
{
	/*
	 * *ret = c1 - c2;
	 * return (*ret > c1);
	 */
	fixed value = *result;
#ifdef BIGNUM_FULLCODE
	*result -= *carry;
#else
	*result = BIGNUM_FULL & (value - *carry);
#endif
	*carry = *result > value;
}

static inline void minusnumber3(fixed *result, fixed value, fixed *carry)
{
	/*
	 * *ret = c1 - c2;
	 * return (*ret > c1);
	 */
#ifdef BIGNUM_FULLCODE
	*result = value - *carry;
#else
	*result = BIGNUM_FULL & (value - *carry);
#endif
	*carry = *result > value;
}

static void minuscarry(fixed *result, fixed value, fixed *carry)
{
	minusnumber(result, &value);
	minusnumber(result, carry);
	*carry += value;
}

static inline void minuscarry4(fixed *result, fixed left, fixed right, fixed *carry)
{
	minusnumber3(result, left, &right);
	minusnumber(result, carry);
	*carry += right;
}

#define TailCopy(a,b,s,i) bigcpy((a)+(i), (b)+(i), ((s)-(i)))

void setplusvalue_bigdata(addr set, addr left, int sign, fixed right)
{
	addr root;
	fixed *data1;
	const fixed *data2;
	size_t size, i;

	GetSizeBignum(left, &size);
	GetRootDataBignum(set, &root, &data1);
	GetRootDataBignum(left, &root, &data2);
	for (i = 0; right && i < size; i++)
		plusnumber3(&data1[i], data2[i], &right);
	if (i < size)
		TailCopy(data1, data2, size, i);
	if (right) {
		data1[i] = right;
		size++;
	}
	Check(RefAllocBignum(set) < size, "bignum size error");
	SetSizeBignum(set, size);
	SetSignBignum(set, sign);
}

void plusvalue_bigdata_alloc(LocalRoot local,
		addr left, int sign, fixed right, addr *ret)
{
	addr pos;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");
	alloc_bignum(local, &pos, RefSizeBignum(left) + 1UL);
	setplusvalue_bigdata(pos, left, sign, right);
	*ret = pos;
}

void setminusvalue_bigdata(addr set, addr left, int sign, fixed right)
{
	addr root;
	fixed *data1, value;
	const fixed *data2;
	size_t size, i;

	GetSizeBignum(left, &size);
	SetSizeBignum(set, size);
	GetRootDataBignum(set, &root, &data1);
	GetRootDataBignum(left, &root, &data2);

	/* single value */
	Check(size == 0, "size error");
	if (size == 1) {
		value = data2[0];
		if (value == right) {
			setzero_bignum(set);
		}
		else if (value < right) {
			data1[0] = right - value;
			SetSignBignum(set, SignNot(sign));
		}
		else {
			data1[0] = value - right;
			SetSignBignum(set, sign);
		}
		return;
	}

	/* multi value */
	SetSignBignum(set, sign);
	for (i = 0; right; i++)
		minusnumber3(&data1[i], data2[i], &right);
	if (i < size)
		TailCopy(data1, data2, size, i);
	sizepress_bignum(set);
}

void minusvalue_bigdata_alloc(LocalRoot local,
		addr left, int sign, fixed right, addr *ret)
{
	addr pos;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");
	alloc_bignum(local, &pos, RefSizeBignum(left));
	setminusvalue_bigdata(pos, left, sign, right);
	*ret = pos;
}

static void plusloop(addr pos, size_t size1, size_t size2,
		const fixed *data1, const fixed *data2)
{
	size_t i;
	fixed carry, *data;
	struct bignuminfo *ptr;

	ptr = StructBignum(pos);
	Check(size2 < size1, "size error");
	Check(ptr->alloc <= size2, "alloc error");
	GetRootDataBignum(pos, &pos, &data);
	carry = 0;
	for (i = 0; i < size1; i++)
		pluscarry4(&data[i], data1[i], data2[i], &carry);
	for (; carry && i < size2; i++)
		plusnumber3(&data[i], data2[i], &carry);
	if (i < size2)
		TailCopy(data, data2, size2, i);
	if (carry) {
		data[i] = carry;
		size2++;
	}
	ptr->size = size2;
}

void plus_bigdata_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	addr pos;
	const fixed *data1, *data2;
	size_t size1, size2;

	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	GetRootDataBignum(left, &left, &data1);
	GetRootDataBignum(right, &right, &data2);
	if (size1 < size2) {
		alloc_bignum(local, &pos, size2 + 1);
		plusloop(pos, size1, size2, data1, data2);
	}
	else {
		alloc_bignum(local, &pos, size1 + 1);
		plusloop(pos, size2, size1, data2, data1);
	}
	*ret = pos;
}

void letplus_noexpand_bigdata(addr left, addr right)
{
	addr root;
	const fixed *data1, *data2;
	size_t size1, size2;

	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	Check(RefAllocBignum(left) < size1 + 1, "size1 error");
	Check(RefAllocBignum(left) < size2 + 1, "size2 error");
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	if (size1 < size2) {
		plusloop(left, size1, size2, data1, data2);
	}
	else {
		plusloop(left, size2, size1, data2, data1);
	}
}

static inline void setminus_bigdata_call(addr pos,
		const fixed *data1,
		size_t size1,
		const fixed *data2,
		size_t size2)
{
	addr root;
	fixed carry, *data;
	size_t i;

	GetRootDataBignum(pos, &root, &data);
	SetSizeBignum(pos, size1);
	carry = 0;
	for (i = 0; i < size2; i++)
		minuscarry4(&data[i], data1[i], data2[i], &carry);
	for (; carry; i++)
		minusnumber3(&data[i], data1[i], &carry);
	if (i < size1)
		TailCopy(data, data1, size1, i);
	sizepress_bignum(pos);
}

static void minus_bigdata_call(LocalRoot local,
		addr *ret,
		const fixed *data1,
		size_t size1,
		const fixed *data2,
		size_t size2)
{
	alloc_bignum(local, ret, size1);
	setminus_bigdata_call(*ret, data1, size1, data2, size2);
}

void minus_bigdata_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	size_t size1, size2;
	const fixed *data1, *data2;

	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	Check(size1 < size2, "size error");
	Check((size1 == 1) && (data1[0] < data2[0]), "value error");
	Check(size1 == size2 && compare_bigdata(left, right) < 0, "compare error");
	minus_bigdata_call(local, ret, data1, size1, data2, size2);
}

int minuscheck_bigdata_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	int check;
	addr root;
	size_t size1, size2;
	const fixed *data1, *data2;

	check = compare_bigdata(left, right);
	if (check == 0) {
		bignum_value_alloc(local, ret, SignPlus, 0);
		return 0;
	}
	if (0 < check) {
		GetSizeBignum(left, &size1);
		GetSizeBignum(right, &size2);
		GetRootDataBignum(left, &root, &data1);
		GetRootDataBignum(right, &root, &data2);
		check = 0;
	}
	else {
		GetSizeBignum(left, &size2);
		GetSizeBignum(right, &size1);
		GetRootDataBignum(left, &root, &data2);
		GetRootDataBignum(right, &root, &data1);
		check = 1;
	}
	minus_bigdata_call(local, ret, data1, size1, data2, size2);

	return check;
}

static void setminus_noexpand(addr pos, addr left, addr right)
{
	addr root;
	size_t size1, size2;
	const fixed *data1, *data2;

	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	Check(size1 < size2, "size error");
	Check((size1 == 1) && (data1[0] < data2[0]), "value error");
	Check(size1 == size2 && compare_bigdata(left, right) < 0, "compare error");
	Check(RefAllocBignum(pos) < size1, "alloc error");
	setminus_bigdata_call(pos, data1, size1, data2, size2);
}

int letminus_noexpand_bigdata(addr left, addr right)
{
	int check;

	check = compare_bigdata(left, right);
	if (check == 0) {
		setzero_bignum(left);
		return 0;
	}
	if (check < 0) {
		setminus_noexpand(left, right, left);
		return 1;
	}
	else {
		setminus_noexpand(left, left, right);
		return 0;
	}
}


/*****************************************************************************
  multiple
 *****************************************************************************/
void multicarry_fixnum(LocalRoot local, fixnum left, fixnum right, addr *ret)
{
	int sign1, sign2;
	fixed fixed1, fixed2;

	castfixed(left, &sign1, &fixed1);
	castfixed(right, &sign2, &fixed2);
	sign1 = SignMulti(sign1, sign2);
	multinumber(&fixed1, &fixed2);
	if (fixed2 != 0) {
		bignum_value2_alloc(local, ret, sign1, fixed2, fixed1);
	}
	else if (IsPlus(sign1) && (fixed1 <= FIXNUM_MAX)) {
		fixnum_alloc(local, ret, (fixnum)fixed1);
	}
	else if (IsMinus(sign1) && (fixed1 <= FIXNUM_UMIN)) {
		fixnum_alloc(local, ret, -(fixnum)fixed1);
	}
	else {
		bignum_value_alloc(local, ret, sign1, fixed1);
	}
}

void multicarry_bignum(LocalRoot local, fixnum left, fixnum right, addr *ret)
{
	int sign1, sign2;
	fixed fixed1, fixed2;

	castfixed(left, &sign1, &fixed1);
	castfixed(right, &sign2, &fixed2);
	sign1 = SignMulti(sign1, sign2);
	multinumber(&fixed1, &fixed2);
	if (fixed2)
		bignum_value2_alloc(local, ret, sign1, fixed2, fixed1);
	else
		bignum_value_alloc(local, ret, sign1, fixed1);
}

void setmultivalue_bigdata(addr pos, addr left, fixed right)
{
	addr root;
	fixed *data1, carry;
	const fixed *data2;
	size_t i, size;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type pos error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");

	GetSizeBignum(left, &size);
	Check(RefAllocBignum(pos) <= size, "size error");
	GetRootDataBignum(pos, &root, &data1);
	GetRootDataBignum(left, &root, &data2);

	carry = 0;
	for (i = 0; i < size; i++)
		multicarry4(&data1[i], data2[i], right, &carry);
	if (carry)
		data1[i++] = carry;
	SetSizeBignum(pos, i);
}

void setmulti_bigdata(addr pos, addr left, addr right)
{
	addr root;
	size_t size1, size2, a, b, c;
	fixed *data, value1, value2, carry;
	const fixed *data1, *data2;

	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	Check(RefAllocBignum(pos) < size1 + size2, "size error");

	GetRootDataBignum(pos, &root, &data);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);

	bigset(data, 0, size1 + size2);
	for (b = c = 0; b < size2; b++) {
		value2 = data2[b];
		carry = 0;
		for (a = 0; a < size1; a++) {
			value1 = data1[a];
			multicarry(&value1, value2, &carry);
			for (c = a + b; value1; c++)
				plusnumber(&data[c], &value1);
		}
		for (c = a + b; carry; c++)
			plusnumber(&data[c], &carry);
	}
	SetSizeBignum(pos, c);
}

void multi_bigdata_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	alloc_plus_bignum(local, ret, RefSizeBignum(left), RefSizeBignum(right));
	setmulti_bigdata(*ret, left, right);
}


/*****************************************************************************
  division
 *****************************************************************************/
static void divhalf(fixed *high, fixed *low, fixed denom, fixed *carry)
{
	fixed v1, v2, n3, n4, a2, a3, a4, a;

	v1 = CUTB(*high);
	v2 = CUTB(*low);
	n3 = HIGHVALUE(v2);
	n4 = LOWVALUE(v2);

	a2 = v1 / denom;
	a = v1 % denom;
	v1 = HIGHLOW(a, n3);
	a3 = v1 / denom;
	a = v1 % denom;
	v1 = HIGHLOW(a, n4);
	a4 = v1 / denom;

	*high = CUTB(a2);
	*low = HIGHLOW(a3, a4);
	*carry = CUTB(v1 % denom);
}

static void divloop(fixed m1, fixed denom,
		fixed *quot, fixed *rem,
		fixed n1, fixed n2, fixed n3)
{
	/*
	 *  a: [n1 n2 n3] / denom
	 */
	fixed a, nn, v1, v2, v3;

	/* nn = [n2 n3] */
	SETB(n1);
	SETB(n2);
	SETB(n3);
	nn = CUTB(HIGHLOW(n2, n3));
	if (n1 == 0 && nn < denom) {
		*quot = 0;
		*rem = nn;
		return;
	}
	/* a = [n1 n2] / m1; */
	a = CUTB(HIGHLOW(n1, n2) / m1);
	/* a * denom = [v1 v2 v2] */
	v1 = 0;
	multicarry4(&v2, a, denom, &v1);
	SETB(v2);
	/* [n1 n2 n3] - [v1 v2 v2] */
	v3 = 0;
	minuscarry(&nn, v2, &v3);
	SETB(nn);
	SETB(v3);
	minuscarry(&n1, v1, &v3);
	SETB(n1);
	SETB(v3);
	while (v3) {
		a--;
		SETB(a);
		v3 = 0;
		pluscarry(&nn, denom, &v3);
		SETB(nn);
		SETB(v3);
		v3 = (v3 == 0);
	}
	*quot = a;
	*rem = nn;
}

#define GETSHIFTVALUECHECK (1ULL << (BIGNUM_FULLBIT - 1))
static int getshiftvalue(fixed *value)
{
	int count;

#ifdef LISP_BIGNUM_DEBUG
	if (*value == 0) {
		Abort("getshiftvalue error");
	}
#endif
	for (count = 0; *value < GETSHIFTVALUECHECK; count++)
		*value <<= 1;

	return count;
}

static void divfull(fixed *high, fixed *low, fixed denom, fixed *carry)
{
	int shift, nshift;
	fixed m1, a2, a3, a4;
	fixed nn, s1, s2;

	/* shift denom */
	shift = getshiftvalue(&denom);
#ifdef LISP_BIGNUM_DEBUG
	if (BIGNUM_HALFBIT <= shift) {
		Abort("getshiftvalue error");
	}
#endif

	m1 = HIGHVALUE(denom);
	s1 = CUTB(*high);
	s2 = CUTB(*low);
	if (shift == 0) {
		/* a2: [n1 n2] / denom */
		if (s1 < denom) {
			a2 = 0;
			nn = s1;
		}
		else {
			a2 = s1 / denom;
			nn = s1 % denom;
		}
		shift = 0;
		goto second;
	}

	/* shift */
	nshift = BIGNUM_FULLBIT - shift;
	nn = CUTB(s1 >> nshift);
	s1 = CUTB((s1 << shift) | (s2 >> nshift));
	s2 = CUTB(s2 << shift);

	/* a2: [n0 n1 n2] / denom */
	divloop(m1, denom, &a2, &nn, CUTB(nn), HIGHVALUE(s1), LOWVALUE(s1));
second:
	/* a3: [n1 n2 n3] / denom */
	divloop(m1, denom, &a3, &nn, HIGHVALUE(nn), LOWVALUE(nn), HIGHVALUE(s2));
	/* a4: [n2 n3 n4] / denom */
	divloop(m1, denom, &a4, &nn, HIGHVALUE(nn), LOWVALUE(nn), LOWVALUE(s2));

	/* result */
	*high = CUTB(a2);
	*low = HIGHLOW(a3, a4);
	*carry = CUTB(nn >> shift);
}

static void divdouble(fixed *high, fixed *low, fixed denom, fixed *carry)
{
	/*
	 *  1. ----/00  -> error
	 *  2. 0000/--  -> 0
	 *  3. 00bb/cc  -> b/c
	 *  4. aabb/-c  -> divhalf(aabb/-c)
	 *  5. aabb/cc  -> divfull(aabb/cc)
	 */
	fixed value;

#ifdef LISP_BIGNUM_DEBUG
	/* denom is zero */
	if (denom == 0) {
		Abort("divdouble error, denom is zero.");
	}
#endif

	/* single division */
	if (*high == 0) {
		/* numerator is zero */
		value = *low;
		if (value == 0) {
			*carry = 0;
			return;
		}
		/* normal division */
		*low = value / denom;
		*carry = value % denom;
		return;
	}

	if (denom == 1) {
		*carry = 0;
		return;
	}

	if (HIGHVALUE(denom))
		divfull(high, low, denom, carry);
	else
		divhalf(high, low, denom, carry);
}

static void divcarry4_half(fixed *ptr, fixed left, fixed denom, fixed *carry)
{
	fixed n1;
#ifndef LISP_BIGNUM_DEBUG
	fixed n2;
#endif

#ifdef LISP_BIGNUM_DEBUG
	if (denom <= *carry) {
		Abort("divcarry4_half error. (carry)");
	}
	if (HIGHVALUE(denom)) {
		Abort("divcarry4_half error. (denom)");
	}
#endif

	if (*carry == 0) {
		*ptr = left / denom;
		*carry = left % denom;
		return;
	}

#ifdef LISP_BIGNUM_DEBUG
	n1 = *carry;
	divhalf(&n1, &left, denom, carry);
	if (n1) {
		Abort("divcarry4_half error");
	}
	*ptr = left;
#else
	n1 = HIGHLOW(*carry, HIGHVALUE(left));
	n2 = n1 / denom;
	n1 = n1 % denom;
	n1 = HIGHLOW(n1, LOWVALUE(left));
	*carry = n1 % denom;
	*ptr = HIGHLOW(n2, n1 / denom);
#endif
}

static void divcarry4_full(fixed *ptr, fixed left, fixed denom, fixed *carry)
{
	fixed high;

	if (*carry == 0) {
		*ptr = left / denom;
		*carry = left % denom;
		return;
	}

	high = *carry;
	divfull(&high, &left, denom, carry);
#ifdef LISP_BIGNUM_DEBUG
	if (high) {
		Abort("divcarry4_full error");
	}
#endif
	*ptr = left;
}

static void multiminusdata(
		fixed *r, const fixed *data2, size_t size, fixed a, fixed *carry)
{
	size_t i;
	fixed m, check;

	check = 0;
	for (i = 0; i < size; i++) {
		multicarry4(&m, data2[i], a, &check);
		minuscarry(&r[i], m, carry);
	}
	minuscarry(&r[i], check, carry);
}

static void pluscarrydata(fixed *r, const fixed *data2, size_t size, fixed *carry)
{
	size_t i;
	fixed check;

	check = 0;
	for (i = 0; i < size; i++)
		pluscarry(&r[i], data2[i], &check);
	if (check)
		*carry = 0;
}

static int comparedata(
		const fixed *data1, size_t size1,
		const fixed *data2, size_t size2)
{
	size_t i, index1, index2;
	fixed a, b;

	for (i = 0; i < size2; i++) {
		index1 = size1 - i - 1;
		index2 = size2 - i - 1;
		a = data1[index1];
		b = data2[index2];
		if (a < b)
			return -1;
		if (a > b)
			return 1;
	}

	return 0;
}

static int comparedata2(
		const fixed *data1, size_t size1,
		const fixed *data2, size_t size2)
{
	size_t i, index;
	fixed a, b;

	for (; size1 && data1[size1 - 1] == 0; size1--)
		continue;
	for (; size2 && data2[size2 - 1] == 0; size2--)
		continue;
	if (size1 < size2)
		return -1;
	if (size1 > size2)
		return 1;

	for (i = 0; i < size1; i++) {
		index = size1 - i - 1;
		a = data1[index];
		b = data2[index];
		if (a < b)
			return -1;
		if (a > b)
			return 1;
	}

	return 0;
}

static void quotdata(fixed *datar, const fixed *data2, size_t size2, fixed *q)
{
	fixed carry;

	carry = 0;
	multiminusdata(datar, data2, size2, *q, &carry);
	while (carry) {
		(*q)--;
		pluscarrydata(datar, data2, size2, &carry);
	}
	datar[size2] = 0;
}

static void divrem_calculate(addr quot, addr rem, addr left, addr right)
{
	int compare;
	size_t pos, size1, size2, sizer, sizeq, len;
	const fixed *data1, *data2;
	fixed *datar, *dataq, high, q, dn, carry;

	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	GetSizeBignum(quot, &sizeq);
	GetSizeBignum(rem, &sizer);
	GetRootDataBignum(left, &left, &data1);
	GetRootDataBignum(right, &right, &data2);
	GetRootDataBignum(quot, &quot, &dataq);
	GetRootDataBignum(rem, &rem, &datar);

	pos = 0;
	len = size1 - size2 + 1;
	dn = data2[size2 - 1];

	/* first check */
	compare = comparedata(data1, size1, data2, size2);
	if (compare == 0) { /* equal */
		dataq[sizeq - pos++ - 1] = 1;
		bigset(datar, 0, sizer);
	}
	else if (0 < compare) { /* N division */
		q = data1[size1 - 1] / dn;
		bigcpy(datar, data1 + size1 - size2, size2);
		datar[size2] = 0;
		quotdata(datar, data2, size2, &q);
		dataq[sizeq - pos++ - 1] = q;
	}
	else { /* N+1 division */
		bigcpy(datar, data1 + size1 - size2, size2);
		datar[size2] = 0;
		dataq[sizeq - pos++ - 1] = 0;
	}

	/* N+1 division */
	for (; pos < len; pos++) {
		/* shift */
		bigmove(datar + 1, datar, size2);
		datar[0] = data1[size1 - size2 - pos];
		compare = comparedata2(datar, sizer, data2, size2);
		if (compare == 0) {
			dataq[sizeq - pos - 1] = 1;
			bigset(datar, 0, sizer);
			continue;
		}
		else if (compare < 0) {
			dataq[sizeq - pos - 1] = 0;
			continue;
		}

		/* quot */
		high = datar[sizer - 1];
		q = datar[sizer - 2];
		divdouble(&high, &q, dn, &carry);
		if (high) q = BIGNUM_FULL;
		quotdata(datar, data2, size2, &q);
		dataq[sizeq - pos - 1] = q;
	}
}

static void shiftup(int shift, int nshift,
		fixed *dst, const fixed *src, size_t size, int final)
{
	size_t i;
	fixed carry, temp;

	carry = 0;
	for (i = 0; i < size; i++) {
		temp = src[i];
		dst[i] = CUTB((temp << shift) | (carry >> nshift));
		carry = temp;
	}
	if (final)
		dst[i] = carry >> nshift;
}

static void shiftdown(int shift, int nshift, fixed *dst, size_t size)
{
	size_t i, index;
	fixed carry, temp;

	carry = 0;
	for (i = 0; i < size; i++) {
		index = size - i - 1;
		temp = dst[index];
		dst[index] = CUTB((carry << nshift) | (temp >> shift));
		carry = temp;
	}
}

#define make_bignum_local(local, ret, size) { \
	addr __pos; \
	alloc_bignum(local, &__pos, (size)); \
	SetSizeBignum(__pos, (size)); \
	*(ret) = __pos; \
}

static void div_noexpand(LocalRoot local, addr *rq, addr *rr, addr left, addr right)
{
	int shift, nshift;
	addr root, quot, rem, a, b;
	fixed *data1, *data2, *dataa, *datab, carry;
	size_t size1, size2, sizea;

	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	GetRootDataBignum(right, &root, &data2);
	carry = data2[size2 - 1];
	shift = getshiftvalue(&carry);
	if (shift == 0) {
		make_bignum_local(local, &quot, size1 - size2 + 1);
		make_bignum_local(local, &rem, size2 + 1);
		divrem_calculate(quot, rem, left, right);
	}
	else {
		/* memory allocate */
		make_bignum_local(local, &a, size1 + 1);
		make_bignum_local(local, &b, size2);

		/* shiftup */
		nshift = BIGNUM_FULLBIT - shift;
		GetRootDataBignum(left, &root, &data1);
		GetRootDataBignum(right, &root, &data2);
		GetRootDataBignum(a, &root, &dataa);
		GetRootDataBignum(b, &root, &datab);
		shiftup(shift, nshift, dataa, data1, size1, 1);
		shiftup(shift, nshift, datab, data2, size2, 0);
		sizepress_bignum(a);

		/* run calculate */
		GetSizeBignum(a, &sizea);
		make_bignum_local(local, &quot, sizea - size2 + 1);
		make_bignum_local(local, &rem, size2 + 1);
		divrem_calculate(quot, rem, a, b);

		/* shift reverse rem */
		GetRootDataBignum(rem, &root, &data1);
		GetSizeBignum(rem, &size1);
		shiftdown(shift, nshift, data1, size1);
	}

	sizepress_bignum(quot);
	sizepress_bignum(rem);
	*rq = quot;
	*rr = rem;
}

static void letdiv_noexpand(LocalRoot local, addr left, addr right)
{
	addr quot, rem;
	LocalStack stack;

	push_local(local, &stack);
	div_noexpand(local, &quot, &rem, left, right);
	copy_noexpand_bignum(left, quot);
	rollback_local(local, stack);
}

static fixed letdivvalue_buffer(addr left, fixed right)
{
	addr root;
	fixed *data, carry;
	size_t size, i, index;

	GetSizeBignum(left, &size);
	GetRootDataBignum(left, &root, &data);

	/* single */
	if (size == 1) {
		carry = data[0] % right;
		data[0] /= right;
		return carry;
	}

	/* multiple */
	carry = 0;
	index = size - 1;
	if (HIGHVALUE(right)) {
		for (i = 0; i < size; i++) {
			divcarry4_full(&data[index], data[index], right, &carry);
			index--;
		}
	}
	else {
		for (i = 0; i < size; i++) {
			divcarry4_half(&data[index], data[index], right, &carry);
			index--;
		}
	}
	sizepress_bignum(left);

	return carry;
}

void letdiv_noexpand_bigdata(LocalRoot local, addr left, addr right)
{
	int check1, check2, compare;
	size_t size1, size2;
	fixed a, b;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type left error");
	if (zerop_bignum(right)) {
		Abort("division-by-zero error.");
		return;
	}
	if (zerop_bignum(left)) {
		setzero_bignum(left);
		return;
	}
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	check1 = (size1 == 1);
	check2 = (size2 == 1);
	if (check1 && check2) {
		GetRootBignum(right, &right);
		b = PtrDataBignum(right)[0];
		GetRootBignum(left, &right);
		a = PtrDataBignum(right)[0];
		setvalue_bignum(left, SignPlus, a / b);
		return;
	}
	if (check2) {
		GetRootBignum(right, &right);
		b = PtrDataBignum(right)[0];
		if (b == 1)
			return;
		(void)letdivvalue_buffer(left, b);
		return;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		setvalue_bignum(left, SignPlus, 1);
		return;
	}
	if (compare < 0) {
		setzero_bignum(left);
		return;
	}
	letdiv_noexpand(local, left, right);
}

static void setrem_noexpand(LocalRoot local, addr set, addr left, addr right)
{
	addr quot, rem;
	LocalStack stack;

	push_local(local, &stack);
	div_noexpand(local, &quot, &rem, left, right);
	copy_noexpand_bignum(set, rem);
	rollback_local(local, stack);
}

static fixed remvalue_buffer(addr left, fixed right)
{
	addr root;
	fixed *data, carry, dummy;
	size_t size, i, index;

	GetSizeBignum(left, &size);
	GetRootDataBignum(left, &root, &data);

	/* single */
	if (size == 1)
		return data[0] % right;

	/* multiple */
	carry = 0;
	index = size - 1;
	if (HIGHVALUE(right)) {
		for (i = 0; i < size; i++) {
			divcarry4_full(&dummy, data[index], right, &carry);
			index--;
		}
	}
	else {
		for (i = 0; i < size; i++) {
			divcarry4_half(&dummy, data[index], right, &carry);
			index--;
		}
	}

	return carry;
}

void setrem_noexpand_bigdata(LocalRoot local, addr set, addr left, addr right)
{
	int check1, check2, compare;
	size_t size1, size2;
	fixed a, b;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type left error");
	if (zerop_bignum(right)) {
		Abort("division-by-zero error.");
		return;
	}
	if (zerop_bignum(left)) {
		setzero_bignum(set);
		return;
	}
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	check1 = (size1 == 1);
	check2 = (size2 == 1);
	if (check1 && check2) {
		GetRootBignum(left, &left);
		GetRootBignum(right, &right);
		a = PtrDataBignum(left)[0];
		b = PtrDataBignum(right)[0];
		setvalue_bignum(set, SignPlus, a % b);
		return;
	}
	if (check2) {
		GetRootBignum(right, &right);
		b = PtrDataBignum(right)[0];
		if (b == 1) {
			setzero_bignum(set);
			return;
		}
		b = remvalue_buffer(left, b);
		setvalue_bignum(set, SignPlus, b);
		return;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		setzero_bignum(set);
		return;
	}
	if (compare < 0) {
		copy_noexpand_bignum(set, left);
		return;
	}
	setrem_noexpand(local, set, left, right);
}

void divrem_bigdata_local(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	int check1, check2, compare;
	size_t size1, size2;
	fixed a, b;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type left error");
	if (zerop_bignum(right)) {
		Abort("division-by-zero error.");
		return;
	}
	if (zerop_bignum(left)) {
		bignum_value_alloc(local, quot, SignPlus, 0);
		bignum_value_alloc(local, rem, SignPlus, 0);
		return;
	}
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	Check(size1 == 0, "size1 error");
	Check(size2 == 0, "size2 error");
	check1 = (size1 <= 1);
	check2 = (size2 <= 1);
	if (check1 && check2) {
		GetRootBignum(left, &left);
		GetRootBignum(right, &right);
		a = PtrDataBignum(left)[0];
		b = PtrDataBignum(right)[0];
		bignum_value_alloc(local, quot, SignPlus, a / b);
		bignum_value_alloc(local, rem, SignPlus, a % b);
		return;
	}
	if (check2) {
		GetRootBignum(right, &right);
		b = PtrDataBignum(right)[0];
		if (b == 1) {
			bignum_copy_nosign_alloc(local, quot, left);
			bignum_value_alloc(local, rem, SignPlus, 0);
		}
		else {
			bignum_copy_nosign_alloc(local, quot, left);
			b = letdivvalue_buffer(*quot, b);
			bignum_value_alloc(local, rem, SignPlus, b);
		}
		return;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		bignum_value_alloc(local, quot, SignPlus, 1);
		bignum_value_alloc(local, rem, SignPlus, 0);
		return;
	}
	if (compare < 0) {
		bignum_value_alloc(local, quot, SignPlus, 0);
		bignum_copy_nosign_alloc(local, rem, left);
		return;
	}
	div_noexpand(local, quot, rem, left, right);
}


/*
 *  shift
 */
void power2_bigdata_alloc(LocalRoot local, addr *ret, size_t value)
{
	size_t count;
	fixed *data;
	addr pos, root;

	count = value / BIGNUM_FULLBIT;
	value = value % BIGNUM_FULLBIT;
	alloc_bignum(local, &pos, count + 1);
	GetRootDataBignum(pos, &root, &data);
	if (count) {
		bigset(data, 0, count);
	}
	data[count] = (1ULL << (fixed)value);
	SetSizeBignum(pos, count + 1);
	*ret = pos;
}

void division2_bigdata_alloc(LocalRoot local, addr *ret, addr left)
{
	addr pos, root;
	fixed *data1, *data2, carry, value;
	size_t size, i, index;

	GetSizeBignum(left, &size);
	GetRootDataBignum(left, &root, &data1);
	if (size == 1) {
		bignum_value_alloc(local, ret, SignPlus, data1[0] >> 1);
		return;
	}
	if (data1[size - 1] == 1) {
		carry = 1;
		size--;
	}
	else {
		carry = 0;
	}
	alloc_bignum(local, &pos, size);
	SetSizeBignum(pos, size);
	GetRootDataBignum(pos, &root, &data2);
	for (i = 0; i < size; i++) {
		index = size - i - 1ULL;
		value = data1[index];
		data2[index] = (carry << (BIGNUM_FULLBIT - 1ULL)) | (value >> 1ULL);
		carry = (value & 1ULL);
	}
	*ret = pos;
}

void shiftup_bigdata_alloc(LocalRoot local, addr *ret, addr left, size_t value)
{
	addr pos, root;
	size_t size, count, dsize, i;
	fixed *data1, *data2, next, temp;

	if (value == 0) {
		bignum_throw_alloc(local, left, ret);
		return;
	}
	count = value / BIGNUM_FULLBIT;
	value = value % BIGNUM_FULLBIT;
	GetSizeBignum(left, &size);
	GetRootDataBignum(left, &root, &data1);
	if (size == 1 && data1[0] == 0) {
		bignum_zero_alloc(local, ret);
		return;
	}

	dsize = size + count;
	if (value) {
		alloc_bignum(local, &pos, dsize + 1);
		GetRootDataBignum(pos, &root, &data2);
		SetSizeBignum(pos, dsize + 1);
		bigset(data2, 0, count);
		next = 0;
		for (i = 0; i < size; i++) {
			temp = data1[i];
			data2[i + count] = CUTB(temp << value) | next;
			next = temp >> (BIGNUM_FULLBIT - value);
		}
		data2[i + count] = next;
		sizepress_bignum(pos);
	}
	else {
		alloc_bignum(local, &pos, dsize);
		GetRootDataBignum(pos, &root, &data2);
		SetSizeBignum(pos, dsize);
		bigcpy(data2 + count, data1, size);
		bigset(data2, 0, count);
	}

	SetSignBignum(pos, RefSignBignum(left));
	*ret = pos;
}

void shiftdown_bigdata_alloc(LocalRoot local, addr *ret, addr left, size_t value)
{
	addr pos, root;
	size_t size, count, dsize, i;
	fixed *data1, *data2, x, y;

	if (value == 0) {
		bignum_throw_alloc(local, left, ret);
		return;
	}
	count = value / BIGNUM_FULLBIT;
	value = value % BIGNUM_FULLBIT;
	GetSizeBignum(left, &size);
	GetRootDataBignum(left, &root, &data1);
	if ((size == 1 && data1[0] == 0) || (size <= count)) {
		bignum_zero_alloc(local, ret);
		return;
	}

	dsize = size - count;
	alloc_bignum(local, &pos, dsize);
	GetRootDataBignum(pos, &root, &data2);
	SetSizeBignum(pos, dsize);
	if (value) {
		dsize--;
		for (i = 0; i < dsize; i++) {
			x = (data1[i + count] >> value);
			y = (data1[i + count + 1] << (BIGNUM_FULLBIT - value));
			data2[i] = x | y;
		}
		data2[i] = (data1[i + count] >> value);
		sizepress_bignum(pos);
	}
	else {
		bigcpy(data2, data1 + count, dsize);
	}

	SetSignBignum(pos, RefSignBignum(left));
	*ret = pos;
}

/* shiftdown-minus */
static void shiftdown_minus_make(LocalRoot local, addr *ret, addr src)
{
	addr pos, root;
	size_t size;
	fixed *data1, *data2;

	GetSizeBignum(src, &size);
	alloc_bignum(local, &pos, size + 1UL);
	GetRootDataBignum(src, &root, &data1);
	GetRootDataBignum(pos, &root, &data2);
	bigcpy(data2, data1, size);
	data2[size] = 0UL;
	SetSizeBignum(pos, size + 1UL);
	*ret = pos;
}

static void shiftdown_minus_reverse(addr pos)
{
	size_t size, i;
	fixed *data;

	GetDataBignum(pos, &data);
	GetSizeBignum(pos, &size);
	for (i = 0; i < size; i++)
		data[i] = ~(data[i]);
}

static void shiftdown_minus_oneplus(addr pos)
{
	setplusvalue_bigdata(pos, pos, SignPlus, 1);
}

static void shiftdown_minus_shift(addr pos, size_t value)
{
	addr root;
	size_t size, count, dsize, i, dfullbit;
	fixed *data, x, y;

	count = value / BIGNUM_FULLBIT;
	value = value % BIGNUM_FULLBIT;
	GetSizeBignum(pos, &size);
	GetRootDataBignum(pos, &root, &data);
	dsize = size - count;
	dfullbit = BIGNUM_FULLBIT - value;
	if (value) {
		dsize--;
		for (i = 0; i < dsize; i++) {
			x = data[i + count] >> value;
			y = data[i + count + 1] << dfullbit;
			data[i] = x | y;
		}
		x = data[i + count] >> value;
		y = (BIGNUM_FULL >> dfullbit) << dfullbit;
		data[i] = x | y;
		i++;
	}
	else {
		bigmove(data, data + count, dsize);
		i = dsize;
	}

	/* fill-bit */
	for (; i < size; i++)
		data[i] = BIGNUM_FULL;
}

void shiftdown_minus_bigdata(LocalRoot local, addr *ret, addr left, size_t value)
{
	int sign;
	addr pos, root;
	size_t size, count;
	fixed *data;

	if (value == 0) {
		bignum_throw_alloc(local, left, ret);
		return;
	}
	count = value / BIGNUM_FULLBIT;
	GetSizeBignum(left, &size);
	GetRootDataBignum(left, &root, &data);
	if ((size == 1 && data[0] == 0) || (size <= count)) {
		bignum_fixnum_value_alloc(local, ret, -1);
		return;
	}

	shiftdown_minus_make(local, &pos, left);
	/* reverse */
	shiftdown_minus_reverse(pos);
	shiftdown_minus_oneplus(pos);
	/* shift */
	shiftdown_minus_shift(pos, value);
	/* reverse */
	shiftdown_minus_reverse(pos);
	shiftdown_minus_oneplus(pos);
	/* result */
	GetSignBignum(left, &sign);
	SetSignBignum(pos, sign);
	sizepress_bignum(pos);
	*ret = pos;
}


/*
 *  output
 */
fixed letdiv_half_bigdata(addr left, fixed right)
{
	addr root;
	fixed *data, carry;
	size_t size, i, index;

	Check(HIGHVALUE(right), "right size error");
	GetSizeBignum(left, &size);
	GetRootDataBignum(left, &root, &data);

	/* single */
	if (size == 1) {
		carry = data[0] % right;
		data[0] /= right;
		return carry;
	}

	/* multiple */
	carry = 0;
	index = size - 1;
	for (i = 0; i < size; i++) {
		divcarry4_half(&data[index], data[index], right, &carry);
		index--;
	}
	sizepress_bignum(left);

	return carry;
}


/************************************************************
 *  bignum_equal.c
 ************************************************************/

/*
 *  check
 */
int zerop_or_plusp_bignum(addr pos)
{
	return zerop_bignum(pos) || (IsPlus(RefSignBignum(pos)));
}

int plusp_bignum(addr pos)
{
	return (! zerop_bignum(pos)) && (IsPlus(RefSignBignum(pos)));
}

int minusp_bignum(addr pos)
{
	return (! zerop_bignum(pos)) && (IsMinus(RefSignBignum(pos)));
}

int zerop_bignum(addr pos)
{
	addr root;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	GetRootBignum(pos, &root);

	return RefSizeBignum(pos) == 1 && PtrDataBignum(root)[0] == 0;
}

int evenp_bignum(addr pos)
{
	fixed value;
	getfixed_bignum(pos, 0, &value);
	return (value & 1) == 0;
}

int oddp_bignum(addr pos)
{
	fixed value;
	getfixed_bignum(pos, 0, &value);
	return (value & 1) != 0;
}


/*
 *  equal
 */
int equal_fb_real(addr left, addr right)
{
	int sign1, sign2;
	fixed check1, check2;
	fixnum value;
	addr root;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (1 < RefSizeBignum(right))
		return 0;
	GetFixnum(left, &value);
	GetRootBignum(right, &root);
	check2 = PtrDataBignum(root)[0];
	if (value == 0 && check2 == 0)
		return 1;
	GetSignBignum(right, &sign2);
	castfixed(value, &sign1, &check1);

	return sign1 == sign2 && check1 == check2;
}

int equal_bb_real(addr left, addr right)
{
	addr root;
	size_t size1, size2;
	fixed check1, check2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	if (size1 == 1 && size2 == 1) {
		GetRootBignum(left, &root);
		check1 = PtrDataBignum(root)[0];
		GetRootBignum(right, &root);
		check2 = PtrDataBignum(root)[0];
		return (check1 == 0 && check2 == 0)
			|| ((RefSignBignum(left) == RefSignBignum(right)) && (check1 == check2));
	}

	if (RefSignBignum(left) != RefSignBignum(right))
		return 0;
	if (size1 != size2)
		return 0;
	GetRootBignum(left, &left);
	GetRootBignum(right, &right);
	return bigcmp(PtrDataBignum(left), PtrDataBignum(right), size1) == 0;
}

int equal_nosign_bignum(addr left, addr right)
{
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	if (size1 != size2)
		return 0;
	GetRootBignum(left, &left);
	GetRootBignum(right, &right);
	return bigcmp(PtrDataBignum(left), PtrDataBignum(right), size1) == 0;
}

int equal_value_nosign_bignum(addr left, fixed value)
{
	if (RefSizeBignum(left) != 1)
		return 0;
	GetRootBignum(left, &left);
	return PtrDataBignum(left)[0] == value;
}

int equal_value_bignum(addr left, int sign1, fixed value1)
{
	int sign2;
	addr root;
	fixed value2;

	if (RefSizeBignum(left) != 1)
		return 0;
	GetRootBignum(left, &root);
	value2 = PtrDataBignum(root)[0];
	if (value1 == 0 && value2 == 0)
		return 1;
	GetSignBignum(left, &sign2);
	if (sign1 != sign2)
		return 0;

	return value1 == value2;
}

int equal_value2_nosign_bignum(addr left, fixed high, fixed low)
{
	addr root;
	fixed *data;

	if (high == 0)
		return equal_value_nosign_bignum(left, low);
	if (RefSizeBignum(left) != 2)
		return 0;
	GetRootDataBignum(left, &root, &data);

	return data[0] == low && data[1] == high;
}

int equal_value2_bignum(addr left, int sign1, fixed high, fixed low)
{
	int sign2;
	addr root;
	fixed *data;

	if (high == 0)
		return equal_value_bignum(left, sign1, low);
	if (RefSizeBignum(left) != 2)
		return 0;
	GetRootDataBignum(left, &root, &data);
	if (data[0] != low || data[1] != high)
		return 0;
	GetSignBignum(left, &sign2);

	return sign1 == sign2;
}


/*
 *  compare
 */
static int compare_fixed(int sign1, fixed check1, int sign2, fixed check2)
{
	if (check1 == 0 && check2 == 0)
		return 0;
	if (sign1 != sign2)
		return IsPlus(sign1)? 1: -1;
	if (IsPlus(sign1)) {
		if (check1 < check2)
			return -1;
		if (check1 > check2)
			return 1;
	}
	else {
		if (check1 < check2)
			return 1;
		if (check1 > check2)
			return -1;
	}

	return 0;
}

int compare_value_bignum(fixnum left, addr right)
{
	int sign1, sign2;
	fixed check1, check2;

	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (1 < RefSizeBignum(right))
		return IsPlus(RefSignBignum(right))? -1: 1;

	castfixed(left, &sign1, &check1);
	GetSignBignum(right, &sign2);
	GetRootBignum(right, &right);
	check2 = PtrDataBignum(right)[0];

	return compare_fixed(sign1, check1, sign2, check2);
}

int compare_bignum_value(addr value, fixnum right)
{
	return -compare_value_bignum(right, value);
}

int compare_fb_real(addr left, addr right)
{
	int sign1, sign2;
	fixed check1, check2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (1 < RefSizeBignum(right))
		return IsPlus(RefSignBignum(right))? -1: 1;

	castfixed_fixnum(left, &sign1, &check1);
	GetSignBignum(right, &sign2);
	GetRootBignum(right, &right);
	check2 = PtrDataBignum(right)[0];

	return compare_fixed(sign1, check1, sign2, check2);
}

int compare_bf_real(addr left, addr right)
{
	return -(compare_fb_real(right, left));
}

int compare_bb_real(addr left, addr right)
{
	int sign1, sign2;

	if (zerop_bignum(left)) {
		if (zerop_bignum(right))
			return 0;
		GetSignBignum(right, &sign2);
		return IsPlus(sign2)? -1: 1;
	}
	if (zerop_bignum(right)) {
		GetSignBignum(left, &sign1);
		return IsPlus(sign1)? 1: -1;
	}

	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 != sign2)
		return IsPlus(sign1)? 1: -1;

	sign2 = compare_bigdata(left, right);
	return IsPlus(sign1)? sign2: -sign2;
}

int compare_bs_real_(addr left, addr right, int *ret)
{
	float value1, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	Return(single_float_bignum_(left, &value1));
	GetSingleFloat(right, &value2);
	if (value1 < value2)
		return Result(ret, -1);
	if (value1 > value2)
		return Result(ret, 1);

	return Result(ret, 0);
}

int compare_bd_real_(addr left, addr right, int *ret)
{
	double_float value1, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	Return(double_float_bignum_(left, &value1));
	GetDoubleFloat(right, &value2);
	if (value1 < value2)
		return Result(ret, -1);
	if (value1 > value2)
		return Result(ret, 1);

	return Result(ret, 0);
}

int compare_bl_real_(addr left, addr right, int *ret)
{
	long_float value1, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	Return(long_float_bignum_(left, &value1));
	GetLongFloat(right, &value2);
	if (value1 < value2)
		return Result(ret, -1);
	if (value1 > value2)
		return Result(ret, 1);

	return Result(ret, 0);
}

int compare_sb_real_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_bs_real_(right, left, &check));
	return Result(ret, -check);
}

int compare_db_real_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_bd_real_(right, left, &check));
	return Result(ret, -check);
}

int compare_lb_real_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_bl_real_(right, left, &check));
	return Result(ret, -check);
}

/* compare byte */
int fixnum_unsigned_byte_p(addr value, size_t size)
{
	fixnum check;

	Check(size == 0, "unsigned-byte error");
	GetFixnum(value, &check);
	if (check < 0)
		return 0;
	if (BIGNUM_FULLBIT <= size)
		return 1;

	return ((fixed)check) < (1ULL << ((fixed)size));
}

int bignum_unsigned_byte_p(addr value, size_t size)
{
	addr root;
	fixed *data, left, right;
	size_t m, n;

	Check(size == 0, "unsigned-byte error");
	if (zerop_bignum(value))
		return 1;
	if (IsMinus(RefSignBignum(value)))
		return 0;
	n = size / BIGNUM_FULLBIT;
	GetSizeBignum(value, &m);
	m--;
	if (m < n)
		return 1;
	if (n < m)
		return 0;
	GetRootDataBignum(value, &root, &data);
	left = data[n];
	m = size % BIGNUM_FULLBIT;
	right = (fixed)(1ULL << m);

	return left < right;
}

int fixnum_signed_byte_p(addr value, size_t size)
{
	fixnum check;

	Check(size == 0, "signed-byte error");
	if (BIGNUM_FULLBIT <= size)
		return 1;
	GetFixnum(value, &check);
	size = 1ULL << ((fixed)(size - 1ULL));
	if (0 <= check)
		return ((fixed)check) < size;
	else
		return ((fixed)-check) <= size;
}

int bignum_signed_byte_p(addr value, size_t size)
{
	addr root;
	fixed *data, left, right;
	size_t m, n;

	Check(size == 0, "signed-byte error");
	if (zerop_bignum(value))
		return 1;
	if (IsPlus(RefSignBignum(value))) {
		if (size <= 1)
			return 0;
		return bignum_unsigned_byte_p(value, size - 1);
	}
	size--;
	n = size / BIGNUM_FULLBIT;
	GetSizeBignum(value, &m);
	m--;
	if (m < n)
		return 1;
	if (n < m)
		return 0;
	GetRootDataBignum(value, &root, &data);
	left = data[n];
	m = size % BIGNUM_FULLBIT;
	right = (fixed)(1ULL << m);
	if (left < right)
		return 1;
	if (right < left)
		return 0;
	if (n == 0)
		return 1;
	for (m = 0; m < n; m++) {
		if (data[m])
			return 0;
	}

	return 1;
}


/************************************************************
 *  bignum_multi.c
 ************************************************************/

/*****************************************************************************
  multiple
 *****************************************************************************/
static inline int multisafe_fixnum(fixnum c1, fixnum c2, fixnum *ret)
{
	if (c1 > 0) {
		if (c2 > 0) {
			if (c1 > (FIXNUM_MAX / c2))
				return 1;
		} else {
			if (c2 < (FIXNUM_MIN / c1))
				return 1;
		}
	} else {
		if (c2 > 0) {
			if (c1 < (FIXNUM_MIN / c2))
				return 1;
		} else {
			if ((c1 != 0) && (c2 < (FIXNUM_MAX / c1)))
				return 1;
		}
	}
	*ret = c1 * c2;

	return 0;
}

void multi_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	fixnum value1, value2, value;
	fixed fixed1;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* left */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		bignum_zero_local(local, ret);
		return;
	}
	if (value1 == 1) {
		bignum_fixnum_local(local, ret, right);
		return;
	}
	if (value1 == -1) {
		castfixed_fixnum(right, &sign, &fixed1);
		bignum_value_local(local, ret, SignNot(sign), fixed1);
		return;
	}

	/* right */
	GetFixnum(right, &value2);
	if (value2 == 0) {
		bignum_zero_local(local, ret);
		return;
	}
	if (value2 == 1) {
		bignum_fixnum_value_local(local, ret, value1);
		return;
	}
	if (value2 == -1) {
		castfixed(value1, &sign, &fixed1);
		bignum_value_local(local, ret, SignNot(sign), fixed1);
		return;
	}

	/* multiple */
	if (! multisafe_fixnum(value1, value2, &value)) {
		bignum_fixnum_value_local(local, ret, value);
		return;
	}
	multicarry_bignum(local, value1, value2, ret);
}

static void multi_ff_real_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value1, value2, value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* left */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		fixnum_throw_alloc(local, left, ret);
		return;
	}
	if (value1 == 1) {
		fixnum_throw_alloc(local, right, ret);
		return;
	}
	if (value1 == -1) {
		sigrev_fixnum_integer_alloc(local, right, ret);
		return;
	}

	/* right */
	GetFixnum(right, &value2);
	if (value2 == 0) {
		fixnum_throw_alloc(local, right, ret);
		return;
	}
	if (value2 == 1) {
		fixnum_throw_alloc(local, left, ret);
		return;
	}
	if (value2 == -1) {
		sigrev_fixnum_integer_alloc(local, left, ret);
		return;
	}

	/* multiple */
	if (! multisafe_fixnum(value1, value2, &value)) {
		fixnum_alloc(local, ret, value);
		return;
	}
	multicarry_fixnum(local, value1, value2, ret);
}

void multi_ff_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_ff_real_alloc(local, left, right, ret);
}

void multi_ff_real_common(addr left, addr right, addr *ret)
{
	multi_ff_real_alloc(NULL, left, right, ret);
}

void multi_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	addr pos, root;
	fixnum check;
	fixed value;
	size_t size;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* right */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_zero_local(local, ret);
		return;
	}
	if (check == 1) {
		bignum_throw_local(local, left, ret);
		return;
	}
	if (check == -1) {
		sigrev_bignum_bignum_local(local, left, ret);
		return;
	}

	/* left */
	GetSignBignum(left, &sign1);
	GetSizeBignum(left, &size);
	if (size == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, left, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign1))
				bignum_fixnum_local(local, ret, right);
			else
				sigrev_fixnum_bignum_local(local, right, ret);
			return;
		}
	}

	/* multi */
	alloc_bignum(local, &pos, size + 1);
	castfixed(check, &sign2, &value);
	setmultivalue_bigdata(pos, left, value);
	SetSignBignum(pos, SignMulti(sign1, sign2));
	*ret = pos;
}

void multi_bf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	addr pos, root;
	fixnum check;
	fixed value;
	size_t size;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* right */
	GetFixnum(right, &check);
	if (check == 0) {
		fixnum_throw_local(local, right, ret);
		return;
	}
	if (check == 1) {
		bignum_throw_local(local, left, ret);
		return;
	}
	if (check == -1) {
		sigrev_bignum_integer_local(local, left, ret);
		return;
	}

	/* left */
	GetSignBignum(left, &sign1);
	GetSizeBignum(left, &size);
	if (size == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, left, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign1))
				fixnum_throw_local(local, right, ret);
			else
				sigrev_fixnum_integer_local(local, right, ret);
			return;
		}
	}

	/* multi */
	alloc_bignum(local, &pos, size + 1);
	castfixed(check, &sign2, &value);
	setmultivalue_bigdata(pos, left, value);
	SetSignBignum(pos, SignMulti(sign1, sign2));
	bignum_result_local(local, pos, ret);
}

void multi_bf_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	addr pos, root;
	fixnum check;
	fixed value;
	size_t size;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* right */
	GetFixnum(right, &check);
	if (check == 0) {
		fixnum_throw_heap(right, ret);
		return;
	}
	if (check == 1) {
		bignum_throw_heap(left, ret);
		return;
	}
	if (check == -1) {
		sigrev_bignum_integer_common(left, ret);
		return;
	}

	/* left */
	GetSignBignum(left, &sign1);
	GetSizeBignum(left, &size);
	if (size == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_heap(left, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign1))
				fixnum_throw_heap(right, ret);
			else
				sigrev_fixnum_integer_common(right, ret);
			return;
		}
	}

	/* multi */
	push_local(local, &stack);
	alloc_bignum(local, &pos, size + 1);
	castfixed(check, &sign2, &value);
	setmultivalue_bigdata(pos, left, value);
	SetSignBignum(pos, SignMulti(sign1, sign2));
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

void multi_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	int sign1, sign2;
	size_t size1, size2;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* left */
	GetSignBignum(left, &sign1);
	GetSizeBignum(left, &size1);
	if (size1 == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, left, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign1))
				bignum_throw_local(local, right, ret);
			else
				sigrev_bignum_bignum_local(local, right, ret);
			return;
		}
	}

	/* right */
	GetSizeBignum(right, &size2);
	GetSignBignum(right, &sign2);
	if (size2 == 1) {
		GetRootBignum(right, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, right, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign2))
				bignum_throw_local(local, left, ret);
			else
				sigrev_bignum_bignum_local(local, left, ret);
			return;
		}
	}

	/* multi */
	alloc_bignum(local, &root, size1 + size2 + 1);
	setmulti_bigdata(root, left, right);
	SetSignBignum(root, SignMulti(sign1, sign2));
	*ret = root;
}

void multi_bb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	int sign1, sign2;
	size_t size1, size2;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* left */
	GetSignBignum(left, &sign1);
	GetSizeBignum(left, &size1);
	if (size1 == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, left, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign1))
				bignum_throw_local(local, right, ret);
			else
				sigrev_bignum_integer_local(local, right, ret);
			return;
		}
	}

	/* right */
	GetSizeBignum(right, &size2);
	GetSignBignum(right, &sign2);
	if (size2 == 1) {
		GetRootBignum(right, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, right, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign2))
				bignum_throw_local(local, left, ret);
			else
				sigrev_bignum_integer_local(local, left, ret);
			return;
		}
	}

	/* multi */
	alloc_bignum(local, &root, size1 + size2 + 1);
	setmulti_bigdata(root, left, right);
	SetSignBignum(root, SignMulti(sign1, sign2));
	bignum_result_local(local, root, ret);
}

void multi_bb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	int sign1, sign2;
	size_t size1, size2;
	fixed value;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* left */
	GetSignBignum(left, &sign1);
	GetSizeBignum(left, &size1);
	if (size1 == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_heap(left, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign1))
				bignum_throw_heap(right, ret);
			else
				sigrev_bignum_integer_common(right, ret);
			return;
		}
	}

	/* right */
	GetSizeBignum(right, &size2);
	GetSignBignum(right, &sign2);
	if (size2 == 1) {
		GetRootBignum(right, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_heap(right, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign2))
				bignum_throw_heap(left, ret);
			else
				sigrev_bignum_integer_common(left, ret);
			return;
		}
	}

	/* multi */
	push_local(local, &stack);
	alloc_bignum(local, &root, size1 + size2 + 1);
	setmulti_bigdata(root, left, right);
	SetSignBignum(root, SignMulti(sign1, sign2));
	bignum_result_heap(root, ret);
	rollback_local(local, stack);
}

void multi_bb_nosign_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	size_t size1, size2;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* left */
	GetSizeBignum(left, &size1);
	if (size1 == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, left, ret);
			return;
		}
		if (value == 1) {
			bignum_throw_local(local, right, ret);
			return;
		}
	}

	/* right */
	GetSizeBignum(right, &size2);
	if (size2 == 1) {
		GetRootBignum(right, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, right, ret);
			return;
		}
		if (value == 1) {
			bignum_throw_local(local, left, ret);
			return;
		}
	}

	/* multi */
	alloc_bignum(local, &root, size1 + size2 + 1);
	setmulti_bigdata(root, left, right);
	*ret = root;
}

void multi_bb_nosign_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	size_t size1, size2;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* left */
	GetSizeBignum(left, &size1);
	if (size1 == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, left, ret);
			return;
		}
		if (value == 1) {
			bignum_throw_local(local, right, ret);
			return;
		}
	}

	/* right */
	GetSizeBignum(right, &size2);
	if (size2 == 1) {
		GetRootBignum(right, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, right, ret);
			return;
		}
		if (value == 1) {
			bignum_throw_local(local, left, ret);
			return;
		}
	}

	/* multi */
	alloc_bignum(local, &root, size1 + size2 + 1);
	setmulti_bigdata(root, left, right);
	bignum_result_local(local, root, ret);
}

void multi_bb_nosign_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	size_t size1, size2;
	fixed value;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* left */
	GetSizeBignum(left, &size1);
	if (size1 == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_heap(left, ret);
			return;
		}
		if (value == 1) {
			bignum_throw_heap(right, ret);
			return;
		}
	}

	/* right */
	GetSizeBignum(right, &size2);
	if (size2 == 1) {
		GetRootBignum(right, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_heap(right, ret);
			return;
		}
		if (value == 1) {
			bignum_throw_heap(left, ret);
			return;
		}
	}

	/* multi */
	push_local(local, &stack);
	alloc_bignum(local, &root, size1 + size2 + 1);
	setmulti_bigdata(root, left, right);
	bignum_result_heap(root, ret);
	rollback_local(local, stack);
}


/************************************************************
 *  bignum_object.c
 ************************************************************/

/*
 *  object
 */
int bignump(addr pos)
{
	return GetType(pos) == LISPTYPE_BIGNUM;
}

struct bignuminfo *struct_bignum(addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return StructBignum_Low(pos);
}

size_t refalloc_bignum(addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return RefAllocBignum_Low(pos);
}

void setsize_bignum(addr pos, size_t value)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	SetSizeBignum_Low(pos, value);
}

void getsize_bignum(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	GetSizeBignum_Low(pos, ret);
}

size_t refsize_bignum(addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return RefSizeBignum_Low(pos);
}

void setroot_bignum(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	SetRootBignum_Low(pos, value);
}

void getroot_bignum(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	GetRootBignum_Low(pos, ret);
}

void setsign_bignum(addr pos, int sign)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	SetSignBignum_Low(pos, sign);
}

void getsign_bignum(addr pos, int *ret)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	GetSignBignum_Low(pos, ret);
}

int refsign_bignum(addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return RefSignBignum_Low(pos);
}

void alloc_bignum(LocalRoot local, addr *ret, size_t alloc)
{
	addr ptr, data;
	struct bignuminfo *str;

	alloc_smallsize(local, &ptr, LISPTYPE_BIGNUM, 1, sizeoft(struct bignuminfo));
	alloc_bigdata(local, &data, alloc);
	SetSignBignum(ptr, SignPlus);
	str = StructBignum(ptr);
	str->size = 0;
	str->alloc = alloc;
	SetRootBignum(ptr, data);
	*ret = ptr;
}

void alloc_plus_bignum(LocalRoot local, addr *ret, size_t a, size_t b)
{
	if (plussafe_size(a, b, &a)) {
		Abort("size error");
		*ret = NULL;
		return;
	}
	alloc_bignum(local, ret, a);
}

void realloc_bignum(LocalRoot local, addr pos, size_t alloc, int force)
{
	addr src, dst;
	struct bignuminfo *info;
	size_t size, copy;

	info = StructBignum(pos);
	if (force || info->alloc < alloc) {
		size = info->size;
		GetRootBignum(pos, &src);
		alloc_bigdata(local, &dst, alloc);
		SetRootBignum(pos, dst);

		if (alloc < size) {
			copy = size;
			info->size = alloc;
		}
		else {
			copy = alloc;
#ifdef LISP_DEBUG
			bigset(PtrDataBignum(dst) + size, 0xAA, (alloc - size));
#endif
		}
		bigcpy(PtrDataBignum(dst), PtrDataBignum(src), copy);
		info->alloc = alloc;
	}
}

void bignum_alloc(LocalRoot local, addr *ret, int sign, size_t size)
{
	addr pos, root;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	alloc_bignum(local, &pos, size);
	SetSignBignum(pos, sign);
	SetSizeBignum(pos, 1);
	GetRootBignum(pos, &root);
	PtrDataBignum(root)[0] = 0;
	*ret = pos;
}

static void bigconscopy(addr root, addr cons)
{
	fixed *data;
	struct bigbuffer *str;
	size_t i, count;

	data = PtrDataBignum(root);
	GetRootBigcons(cons, &root);
	for (i = 0; root != Nil; i += count) {
		str = StructBigbuffer(root);
		count = str->count;
		if (count == 0)
			break;
		bigcpy(data + i, str->buffer, count);
		GetNextBigbuffer(root, &root);
	}
}

void bignum_cons_alloc(LocalRoot local, addr *ret, int sign, addr cons)
{
	addr pos, root;
	size_t count;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	GetCountBigcons(cons, &count);
	alloc_bignum(local, &pos, count);
	GetRootBignum(pos, &root);
	bigconscopy(root, cons);
	SetSignBignum(pos, sign);
	SetSizeBignum(pos, count);
	*ret = pos;
}

void bignum_copy_nosign_alloc(LocalRoot local, addr *ret, addr right)
{
	addr pos, root;
	size_t size;

	Check(GetType(right) != LISPTYPE_BIGNUM, "type error");
	GetSizeBignum(right, &size);
	alloc_bignum(local, &pos, size);
	SetSizeBignum(pos, size);
	GetRootBignum(right, &right);
	GetRootBignum(pos, &root);
	bigcpy(PtrDataBignum(root), PtrDataBignum(right), size);
	*ret = pos;
}

void bignum_copy_alloc(LocalRoot local, addr *ret, addr right)
{
	addr pos;

	bignum_copy_nosign_alloc(local, &pos, right);
	SetSignBignum(pos, RefSignBignum(right));
	*ret = pos;
}

void bignum_value_alloc(LocalRoot local, addr *ret, int sign, fixed value)
{
	addr pos, root;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	alloc_bignum(local, &pos, 1);
	SetSizeBignum(pos, 1);
	SetSignBignum(pos, sign);
	GetRootBignum(pos, &root);
	PtrDataBignum(root)[0] = value;
	*ret = pos;
}

void bignum_value2_alloc(LocalRoot local, addr *ret, int sign, fixed high, fixed low)
{
	addr pos, root;
	fixed *data;

	if (high == 0) {
		bignum_value_alloc(local, ret, sign, low);
		return;
	}

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	alloc_bignum(local, &pos, 2);
	SetSizeBignum(pos, 2);
	SetSignBignum(pos, sign);
	GetRootDataBignum(pos, &root, &data);
	data[1] = high;
	data[0] = low;
	*ret = pos;
}

void bignum_zero_alloc(LocalRoot local, addr *ret)
{
	bignum_value_alloc(local, ret, SignPlus, 0);
}

void bignum_fixnum_alloc(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	bignum_fixnum_value_alloc(local, ret, RefFixnum(pos));
}

void bignum_fixnum_value_alloc(LocalRoot local, addr *ret, fixnum value)
{
	int sign;
	fixed result;
	castfixed(value, &sign, &result);
	bignum_value_alloc(local, ret, sign, result);
}

static int bignum_counter_alloc_p_(addr index, int *ret)
{
	if (! integerp(index))
		return Result(ret, 1);

	Return(minusp_integer_(index, ret));
	if (*ret)
		return 0;

	return zerop_integer_(index, ret);
}

int bignum_counter_alloc_(LocalRoot local, addr *ret, addr index)
{
	int check;

	Return(bignum_counter_alloc_p_(index, &check));
	if (check) {
		*ret = Nil;
		return fmte_("The value ~S must be a positive integer.", index, NULL);
	}
	if (GetType(index) == LISPTYPE_FIXNUM)
		bignum_fixnum_value_alloc(local, ret, RefFixnum(index));
	else
		bignum_copy_alloc(local, ret, index);

	return 0;
}

void bignum_debug(LocalRoot local, addr *ret, int sign, size_t size)
{
	Check(local == NULL, "local error");
	bignum_alloc(local, ret, sign, size);
}

void bignum_cons_debug(LocalRoot local, addr *ret, int sign, addr cons)
{
	Check(local == NULL, "local error");
	bignum_cons_alloc(local, ret, sign, cons);
}

void bignum_copy_nosign_debug(LocalRoot local, addr *ret, addr right)
{
	Check(local == NULL, "local error");
	bignum_copy_nosign_alloc(local, ret, right);
}

void bignum_copy_debug(LocalRoot local, addr *ret, addr right)
{
	Check(local == NULL, "local error");
	bignum_copy_alloc(local, ret, right);
}

void bignum_value_debug(LocalRoot local, addr *ret, int sign, fixed value)
{
	Check(local == NULL, "local error");
	bignum_value_alloc(local, ret, sign, value);
}

void bignum_value2_debug(LocalRoot local, addr *ret, int sign, fixed high, fixed low)
{
	Check(local == NULL, "local error");
	bignum_value2_alloc(local, ret, sign, high, low);
}

void bignum_zero_debug(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	bignum_zero_alloc(local, ret);
}

void bignum_fixnum_debug(LocalRoot local, addr *ret, addr value)
{
	Check(local == NULL, "local error");
	bignum_fixnum_alloc(local, ret, value);
}

void bignum_fixnum_value_debug(LocalRoot local, addr *ret, fixnum value)
{
	Check(local == NULL, "local error");
	bignum_fixnum_value_alloc(local, ret, value);
}

int bignum_counter_debug_(LocalRoot local, addr *ret, addr index)
{
	Check(local == NULL, "local error");
	return bignum_counter_alloc_(local, ret, index);
}

void bignum_result_debug(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	bignum_result_alloc(local, pos, ret);
}

int bignum_integer_debug_(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	return bignum_integer_alloc_(local, ret, pos);
}


/*
 *  access
 */
void getfixed_bignum(addr pos, size_t index, fixed *ret)
{
	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	Check(RefSizeBignum(pos) <= index, "index error");
	GetRootBignum(pos, &pos);
	*ret = PtrDataBignum(pos)[index];
}

fixed reffixed_bignum(addr pos, size_t index)
{
	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	Check(RefSizeBignum(pos) <= index, "index error");
	GetRootBignum(pos, &pos);
	return (fixed)PtrDataBignum(pos)[index];
}

void setfixed_bignum(addr pos, size_t index, fixed value)
{
	struct bignuminfo *ptr;

	ptr = StructBignum(pos);
	Check(ptr->alloc <= index, "alloc size error");
	if (ptr->size <= index)
		ptr->size = index + 1;
	GetRootBignum(pos, &pos);
	PtrDataBignum(pos)[index] = value;
}

void diet_bignum(LocalRoot local, addr pos)
{
	struct bignuminfo *info;

	info = StructBignum(pos);
	if (info->size != info->alloc)
		realloc_bignum(local, pos, info->size, 1);
}

void sizepress_bignum(addr left)
{
	size_t size, i;
	fixed *data;
	struct bignuminfo *ptr;

	ptr = StructBignum(left);
	size = ptr->size;
	GetRootDataBignum(left, &left, &data);
	if (1 < size) {
		for (i = size - 1; ; i--) {
			if (i == 0) {
				ptr->size = 1;
				break;
			}
			if (data[i] != 0) {
				ptr->size = i + 1;
				break;
			}
		}
	}
}


/*
 *  operation
 */
static void resize_nocopy_bignum(LocalRoot local, addr pos, size_t alloc, int force)
{
	addr src, dst;
	struct bignuminfo *info;
	size_t size;

	info = StructBignum(pos);
	if (force || info->alloc < alloc) {
		size = info->size;
		GetRootBignum(pos, &src);
		alloc_bigdata(local, &dst, alloc);
		SetRootBignum(pos, dst);

		if (alloc < size) {
			info->size = alloc;
		}
#ifdef LISP_DEBUG
		if (size < alloc) {
			bigset(PtrDataBignum(dst), 0xAA, alloc);
		}
#endif
		info->alloc = alloc;
	}
}

void copy_bignum(LocalRoot local, addr left, addr right, int force)
{
	size_t size;

	GetSizeBignum(right, &size);
	resize_nocopy_bignum(local, left, size, force);
	SetSizeBignum(left, size);
	SetSignBignum(left, RefSignBignum(right));
	GetRootBignum(left, &left);
	GetRootBignum(right, &right);
	bigcpy(PtrDataBignum(left), PtrDataBignum(right), size);
}

void copy_noexpand_bignum(addr left, addr right)
{
	addr dst, src;
	size_t size;

	GetSizeBignum(right, &size);
	Check(StructBignum(left)->alloc < size, "alloc size error");
	GetRootBignum(left, &dst);
	GetRootBignum(right, &src);
	bigcpy(PtrDataBignum(dst), PtrDataBignum(src), size);
	SetSignBignum(left, RefSignBignum(right));
	SetSizeBignum(left, size);
}

void setvalue_bignum(addr left, int sign, fixed value)
{
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	SetSignBignum(left, sign);
	SetSizeBignum(left, 1);
	GetRootBignum(left, &left);
	PtrDataBignum(left)[0] = value;
}

void setzero_bignum(addr left)
{
	setvalue_bignum(left, SignPlus, 0);
}

int getbit_bignum(addr pos, size_t index)
{
	size_t count, front;
	fixed value;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	count = index / BIGNUM_FULLBIT;
	front = index % BIGNUM_FULLBIT;
	if (RefSizeBignum(pos) <= count)
		return 0;
	getfixed_bignum(pos, count, &value);
	return ((1ULL << front) & value)? 1: 0;
}

void incf_bignum(addr pos, fixed value)
{
	int sign;
	GetSignBignum(pos, &sign);
	setplusvalue_bigdata(pos, pos, SignPlus, value);
}

void decf_bignum(addr pos, fixed value)
{
	int sign;
	GetSignBignum(pos, &sign);
	setminusvalue_bigdata(pos, pos, SignPlus, value);
}


/*
 *  result, throw
 */
void bignum_result_alloc(LocalRoot local, addr pos, addr *ret)
{
	addr root;
	fixed *data, value;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	if (RefSizeBignum(pos) != 1) {
		bignum_throw_alloc(local, pos, ret);
		return;
	}
	GetRootDataBignum(pos, &root, &data);
	value = data[0];
	if (IsPlus(RefSignBignum(pos))) {
		if (value <= FIXNUM_MAX) {
			fixnum_alloc(local, ret, (fixnum)value);
			return;
		}
	}
	else {
		if (value <= FIXNUM_UMIN) {
			fixnum_alloc(local, ret, -(fixnum)value);
			return;
		}
	}
	bignum_throw_alloc(local, pos, ret);
}

int bignum_integer_alloc_(LocalRoot local, addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			bignum_fixnum_alloc(local, ret, pos);
			break;

		case LISPTYPE_BIGNUM:
			bignum_copy_alloc(local, ret, pos);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}

	return 0;
}

void bignum_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	if (GetStatusDynamic(pos))
		bignum_copy_alloc(NULL, ret, pos);
	else
		*ret = pos;
}

void fixnum_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_FIXNUM, "type error");
	if (GetStatusDynamic(pos))
		fixnum_heap(ret, RefFixnum(pos));
	else
		*ret = pos;
}

void bignum_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		bignum_copy_alloc(local, ret, pos);
}

void fixnum_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_FIXNUM, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		fixnum_local(local, ret, RefFixnum(pos));
}

void bignum_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (local)
		bignum_throw_local(local, pos, ret);
	else
		bignum_throw_heap(pos, ret);
}

void fixnum_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (local)
		fixnum_throw_local(local, pos, ret);
	else
		fixnum_throw_heap(pos, ret);
}


/*
 *  cast
 */
void castfixed(fixnum value, int *sign, fixed *result)
{
	if (0 <= value) {
		*sign = SignPlus;
		*result = (fixed)value;
	}
	else if (value == FIXNUM_MIN) {
		*sign = SignMinus;
		*result = FIXNUM_UMIN;
	}
	else {
		*sign = SignMinus;
		*result = (fixed)-value;
	}
}

void castfixed_fixnum(addr pos, int *sign, fixed *result)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	castfixed(RefFixnum(pos), sign, result);
}

int castfixed_integer(addr pos, int *sign, fixed *result)
{
	size_t check;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			castfixed_fixnum(pos, sign, result);
			return 0;

		case LISPTYPE_BIGNUM:
			GetSizeBignum(pos, &check);
			if (check != 1)
				return 1;
			GetSignBignum(pos, sign);
			getfixed_bignum(pos, 0, result);
			return 0;

		default:
			return 1;
	}
}


/************************************************************
 *  bignum_output.c
 ************************************************************/

/*
 *  output
 */
int decimal_charqueue_fixnum_local_(LocalRoot local, addr pos, addr queue)
{
	char buffer[256];

	Check(GetType(pos) != LISPTYPE_FIXNUM, "type error");
	snprintf(buffer, 256, "%" PRIdF, RefFixnum(pos));
	return pushchar_charqueue_local_(local, queue, buffer);
}

int decimal_charqueue_bignum_local_(LocalRoot local, addr pos, addr queue)
{
	addr stream, value;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	/* zero */
	if (zerop_bignum(pos))
		return push_charqueue_local_(local, queue, '0');

	/* sign */
	if (minusp_bignum(pos)) {
		Return(push_charqueue_local_(local, queue, '-'));
	}

	/* body */
	open_output_string_stream(&stream, 0);
	Return(output_nosign_bignum_(local, stream, pos, 10, 0));
	Return(string_stream_heap_(stream, &value));
	clear_output_string_stream(stream);
	return pushstring_charqueue_local_(local, queue, value);
}

int decimal_charqueue_integer_local_(LocalRoot local, addr pos, addr queue)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return decimal_charqueue_fixnum_local_(local, pos, queue);

		case LISPTYPE_BIGNUM:
			return decimal_charqueue_bignum_local_(local, pos, queue);

		default:
			return TypeError_(pos, INTEGER);
	}
}


/*
 *  print
 */
/* base=2, 64bit -> 1+64+1 -> 66+padding -> 72? */
#define FIXNUM_BUFFER_SIZE  128
int output_nosign_index_(addr stream, size_t n, unsigned base, int upperp)
{
	size_t m;
	char buffer[FIXNUM_BUFFER_SIZE], *ptr, chara;

	/* zero */
	if (n == 0)
		return write_char_stream_(stream, '0');

	/* loop */
	chara = upperp? 'A': 'a';
	ptr = buffer + FIXNUM_BUFFER_SIZE - 1;
	*(ptr--) = 0;
	while (n != 0) {
		m = n % base;
		n = n / base;
		*(ptr--) = (char)((m <= 9)? ('0' + m): (chara - 10 + m));
		Check(ptr <= buffer, "buffer error");
	}

	return print_ascii_stream_(stream, ptr + 1);
}

int output_nosign_fixnum_(addr stream, fixnum value, unsigned base, int upperp)
{
	int sign;
	fixed m, n;
	char buffer[FIXNUM_BUFFER_SIZE], *ptr, chara;

	/* zero */
	if (value == 0)
		return write_char_stream_(stream, '0');

	/* fixnum -> fixed */
	castfixed(value, &sign, &n);

	/* loop */
	chara = upperp? 'A': 'a';
	ptr = buffer + FIXNUM_BUFFER_SIZE - 1;
	*(ptr--) = 0;
	while (n != 0) {
		m = n % base;
		n = n / base;
		*(ptr--) = (char)((m <= 9)? ('0' + m): (chara - 10 + m));
		Check(ptr <= buffer, "buffer error");
	}

	return print_ascii_stream_(stream, ptr + 1);
}

static int charbit_nil_p(addr pos)
{
	size_t size;

	if (pos == Nil)
		return 1;
	GetCharBitSize(pos, &size);

	return size == 0;
}

static void charqueue_nreverse(addr pos, addr *ret)
{
	addr tail, next;

	if (charbit_nil_p(pos)) {
		*ret = pos;
		return;
	}

	for (tail = Nil; ; tail = pos, pos = next) {
		GetCharBitNext(pos, &next);
		SetCharBitNext(pos, tail);
		if (charbit_nil_p(next))
			break;
	}
	*ret = pos;
}

static int charqueue_nreverse_output_(addr pos, addr stream)
{
	unicode *ptr;
	size_t size;

	GetCharQueueRoot(pos, &pos);
	charqueue_nreverse(pos, &pos);
	while (pos != Nil) {
		GetCharBitSize(pos, &size);
		if (size == 0)
			break;
		ptr = PtrCharBitChar(pos);
		do {
			size--;
			Return(write_char_stream_(stream, ptr[size]));
		}
		while (size);
		GetCharBitNext(pos, &pos);
	}

	return 0;
}

int output_nosign_bignum_(LocalRoot local,
		addr stream, addr pos, unsigned base, int upperp)
{
	unicode u;
	addr queue, error_character;
	fixed rem;
	size_t size;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(! isBaseChar(base), "base error");
	/* zero */
	if (zerop_bignum(pos))
		return write_char_stream_(stream, '0');

	/* loop */
	push_local(local, &stack);
	GetSizeBignum(pos, &size);
	size = (0x010000 < size)? 0x010000: size * 20; /* size * (log10(2**64)+1) */
	charqueue_local(local, &queue, size);
	bignum_copy_local(local, &pos, pos);
	do {
		rem = letdiv_half_bigdata(pos, (fixed)base);
		if (getchar_digit((unsigned)rem, upperp, &u)) {
			character_heap(&error_character, u);
			return fmte_("Invalid digit character ~S.", error_character, NULL);
		}
		Return(push_charqueue_local_(local, queue, u));
	}
	while (! zerop_bignum(pos));

	/* output */
	Return(charqueue_nreverse_output_(queue, stream));
	rollback_local(local, stack);

	return 0;
}

#define FIXNUM_BUFFER_DOUBLE_SIZE  (FIXNUM_BUFFER_SIZE * 2)
int output_nosign_comma_fixnum_(LocalRoot local,
		addr stream, fixnum value, unsigned base, int upperp,
		size_t range, unicode comma)
{
	int sign;
	fixed m, n;
	unicode buffer[FIXNUM_BUFFER_DOUBLE_SIZE], *ptr, chara;
	size_t index;

	Check(range < 2, "ragen error");
	/* zero */
	if (value == 0)
		return write_char_stream_(stream, '0');

	/* fixnum -> fixed */
	castfixed(value, &sign, &n);

	/* loop */
	chara = upperp? 'A': 'a';
	ptr = buffer + FIXNUM_BUFFER_DOUBLE_SIZE - 1;
	*(ptr--) = 0;
	index = 0;
	while (n != 0) {
		m = n % base;
		n = n / base;
		if (index && (index % range) == 0)
			*(ptr--) = comma;
		*(ptr--) = (unicode)((m <= 9)? ('0' + m): (chara - 10 + m));
		Check(ptr <= buffer, "buffer error");
		index++;
	}

	return print_unicode_stream_(stream, ptr + 1);
}

int output_nosign_comma_bignum_(LocalRoot local,
		addr stream, addr pos, unsigned base, int upperp,
		size_t range, unicode comma)
{
	unicode u;
	addr queue, error_character;
	fixed rem;
	size_t size, index;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(! isBaseChar(base), "base error");
	Check(range < 2, "ragen error");
	/* zero */
	if (zerop_bignum(pos))
		return write_char_stream_(stream, '0');

	/* loop */
	push_local(local, &stack);
	GetSizeBignum(pos, &size);
	size = (0x010000 < size)? 0x010000: size * 20; /* size * (log10(2**64)+1) */
	charqueue_local(local, &queue, size);
	bignum_copy_local(local, &pos, pos);
	index = 0;
	do {
		rem = letdiv_half_bigdata(pos, (fixed)base);
		if (getchar_digit((unsigned)rem, upperp, &u)) {
			character_heap(&error_character, u);
			return fmte_("Invalid digit character ~S.", error_character, NULL);
		}
		if (index && (index % range) == 0) {
			Return(push_charqueue_local_(local, queue, comma));
		}
		Return(push_charqueue_local_(local, queue, u));
		index++;
	}
	while (! zerop_bignum(pos));

	/* output */
	Return(charqueue_nreverse_output_(queue, stream));
	rollback_local(local, stack);

	return 0;
}


/************************************************************
 *  bignum_plus.c
 ************************************************************/

static void carryvalue_alloc(LocalRoot local, addr *ret, int sign, fixed value)
{
	addr pos, root;
	fixed *data;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	alloc_bignum(local, &pos, 2);
	SetSignBignum(pos, sign);
	SetSizeBignum(pos, 2);
	GetRootDataBignum(pos, &root, &data);
	data[0] = (fixed)value;
	data[1] = 1;
	*ret = pos;
}

/*****************************************************************************
  plus
 *****************************************************************************/
static void plusvalue(LocalRoot local, addr left, fixed value, addr *ret)
{
	int sign;
	size_t size;

	GetSignBignum(left, &sign);
	GetSizeBignum(left, &size);
	if (IsPlus(sign)) {
		alloc_bignum(local, ret, size + 1);
		setplusvalue_bigdata(*ret, left, sign, value);
	}
	else {
		alloc_bignum(local, ret, size);
		setminusvalue_bigdata(*ret, left, sign, value);
	}
}

static void minusvalue(LocalRoot local, addr left, fixed value, addr *ret)
{
	int sign;
	size_t size;

	GetSignBignum(left, &sign);
	GetSizeBignum(left, &size);
	if (IsPlus(sign)) {
		alloc_bignum(local, ret, size);
		setminusvalue_bigdata(*ret, left, sign, value);
	}
	else {
		alloc_bignum(local, ret, size + 1);
		setplusvalue_bigdata(*ret, left, sign, value);
	}
}

#define plus_vv_overflow(v1, v2) \
	(((v2) > 0 && (v1) > (FIXNUM_MAX - (v2))) || \
	 ((v2) < 0 && (v1) < (FIXNUM_MIN - (v2))))

static inline void plus_vv_bignum_local(LocalRoot local,
		fixnum v1, fixnum v2, addr *ret)
{
	int sign;
	fixed fixed1, fixed2;

	if (plus_vv_overflow(v1, v2)) {
		castfixed(v1, &sign, &fixed1);
		castfixed(v2, &sign, &fixed2);
		plusnumber_bigdata(&fixed1, &fixed2);
		if (fixed2)
			carryvalue_alloc(local, ret, sign, fixed1);
		else
			bignum_value_local(local, ret, sign, fixed1);
	}
	else {
		castfixed(v1 + v2, &sign, &fixed1);
		bignum_value_local(local, ret, sign, fixed1);
	}
}

static inline void plus_vv_real_alloc(LocalRoot local, fixnum v1, fixnum v2, addr *ret)
{
	int sign;
	fixed fixed1, fixed2;

	if (plus_vv_overflow(v1, v2)) {
		castfixed(v1, &sign, &fixed1);
		castfixed(v2, &sign, &fixed2);
		plusnumber_bigdata(&fixed1, &fixed2);
		if (fixed2)
			carryvalue_alloc(local, ret, sign, fixed1);
		else
			bignum_value_alloc(local, ret, sign, fixed1);
	}
	else {
		fixnum_alloc(local, ret, v1 + v2);
	}
}

void plus_fv_bignum_local(LocalRoot local, addr left, fixnum value2, addr *ret)
{
	fixnum value1;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");

	/* left + 0 */
	GetFixnum(left, &value1);
	if (value2 == 0) {
		bignum_fixnum_value_local(local, ret, value1);
		return;
	}

	/* 0 + right */
	if (value1 == 0) {
		bignum_fixnum_value_local(local, ret, value2);
		return;
	}

	plus_vv_bignum_local(local, value1, value2, ret);
}

static void plus_fv_real_alloc(LocalRoot local, addr left, fixnum value2, addr *ret)
{
	fixnum value1;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");

	/* left + 0 */
	if (value2 == 0) {
		fixnum_throw_alloc(local, left, ret);
		return;
	}

	/* 0 + right */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		fixnum_alloc(local, ret, value2);
		return;
	}

	plus_vv_real_alloc(local, value1, value2, ret);
}

void plus_fv_real_local(LocalRoot local, addr left, fixnum value2, addr *ret)
{
	Check(local == NULL, "local error");
	plus_fv_real_alloc(local, left, value2, ret);
}

void plus_fv_real_common(addr left, fixnum value2, addr *ret)
{
	plus_fv_real_alloc(NULL, left, value2, ret);
}

void plus_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value1, value2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 + right */
	GetFixnum(left, &value1);
	GetFixnum(right, &value2);
	if (value1 == 0) {
		bignum_fixnum_value_local(local, ret, value2);
		return;
	}

	/* left + 0 */
	if (value2 == 0) {
		bignum_fixnum_value_local(local, ret, value1);
		return;
	}

	plus_vv_bignum_local(local, value1, value2, ret);
}

static void plus_ff_real_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 + right */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		fixnum_throw_alloc(local, right, ret);
		return;
	}

	/* left + 0 */
	GetFixnum(right, &value2);
	if (value2 == 0) {
		fixnum_throw_alloc(local, left, ret);
		return;
	}

	plus_vv_real_alloc(local, value1, value2, ret);
}

void plus_ff_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_ff_real_alloc(local, left, right, ret);
}

void plus_ff_real_common(addr left, addr right, addr *ret)
{
	plus_ff_real_alloc(NULL, left, right, ret);
}

static inline void plusfixnum_bignum_local(LocalRoot local,
		addr left, fixnum right, addr *ret)
{
	int sign;
	fixed value;

	Check(local == NULL, "local error");
	castfixed(right, &sign, &value);
	if (IsPlus(sign)) {
		plusvalue(local, left, value, ret);
	}
	else {
		minusvalue(local, left, value, ret);
	}
}

static inline void plusfixnum_real_local(LocalRoot local,
		addr left, fixnum right, addr *ret)
{
	plusfixnum_bignum_local(local, left, right, &left);
	bignum_result_local(local, left, ret);
}

static inline void plusfixnum_real_common(LocalRoot local,
		addr left, fixnum right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	plusfixnum_bignum_local(local, left, right, &left);
	bignum_result_heap(left, ret);
	rollback_local(local, stack);
}

void plus_bv_bignum_local(LocalRoot local, addr left, fixnum right, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");

	/* left + 0 */
	if (right == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	/* 0 + right */
	if (zerop_bignum(left)) {
		bignum_fixnum_value_local(local, ret, right);
		return;
	}

	plusfixnum_bignum_local(local, left, right, ret);
}

void plus_bv_real_local(LocalRoot local, addr left, fixnum right, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");

	/* left + 0 */
	if (right == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	/* 0 + right */
	if (zerop_bignum(left)) {
		fixnum_local(local, ret, right);
		return;
	}

	plusfixnum_real_local(local, left, right, ret);
}

void plus_bv_real_common(LocalRoot local, addr left, fixnum right, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");

	/* left + 0 */
	if (right == 0) {
		bignum_throw_heap(left, ret);
		return;
	}

	/* 0 + right */
	if (zerop_bignum(left)) {
		fixnum_heap(ret, right);
		return;
	}

	plusfixnum_real_common(local, left, right, ret);
}

void plus_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 + right */
	if (zerop_bignum(left)) {
		bignum_fixnum_local(local, ret, right);
		return;
	}

	/* left + 0 */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	plusfixnum_bignum_local(local, left, check, ret);
}

void plus_bf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 + right */
	if (zerop_bignum(left)) {
		fixnum_throw_local(local, right, ret);
		return;
	}

	/* left + 0 */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	plusfixnum_real_local(local, left, check, ret);
}

void plus_bf_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 + right */
	if (zerop_bignum(left)) {
		fixnum_throw_heap(right, ret);
		return;
	}

	/* left + 0 */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_throw_heap(left, ret);
		return;
	}

	plusfixnum_real_common(local, left, check, ret);
}

void plus_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		bignum_throw_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_local(local, left, ret);
		return;
	}
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 == sign2) {
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		*ret = left;
		return;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		bignum_value_alloc(local, ret, SignPlus, 0);
		return;
	}
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = sign2;
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);
	*ret = left;
}

void plus_bb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		bignum_throw_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_local(local, left, ret);
		return;
	}
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 == sign2) {
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		goto finish;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = sign2;
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);

finish:
	bignum_result_local(local, left, ret);
}

void plus_bb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		bignum_throw_heap(right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_heap(left, ret);
		return;
	}
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 == sign2) {
		push_local(local, &stack);
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		goto finish;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		fixnum_heap(ret, 0);
		return;
	}
	push_local(local, &stack);
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = sign2;
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);

finish:
	bignum_result_heap(left, ret);
	rollback_local(local, stack);
}


/*****************************************************************************
  minus
 *****************************************************************************/
void sigrev_bignum_inplace(addr pos)
{
	int sign;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	GetSignBignum(pos, &sign);
	sign = SignNot(sign);
	SetSignBignum(pos, sign);
}

void sigrev_fixnum_bignum_local(LocalRoot local, addr left, addr *ret)
{
	int sign;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");
	castfixed_fixnum(left, &sign, &value);
	bignum_value_local(local, ret, SignNot(sign), value);
}

void sigrev_fixnum_integer_alloc(LocalRoot local, addr left, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");
	GetFixnum(left, &value);
	if (value == FIXNUM_MIN) {
		bignum_value_alloc(local, ret, SignPlus, FIXNUM_UMIN);
	}
	else {
		fixnum_alloc(local, ret, -value);
	}
}

void sigrev_fixnum_integer_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	sigrev_fixnum_integer_alloc(local, left, ret);
}

void sigrev_fixnum_integer_common(addr left, addr *ret)
{
	sigrev_fixnum_integer_alloc(NULL, left, ret);
}

void sigrev_bignum_bignum_local(LocalRoot local, addr left, addr *ret)
{
	int sign;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");
	GetSignBignum(left, &sign);
	bignum_copy_local(local, &left, left);
	SetSignBignum(left, SignNot(sign));
	*ret = left;
}

static void inline sigrev_bignum_integer_alloc(LocalRoot local, addr left, addr *ret)
{
	int sign;
	addr root;
	fixed *data, value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");
	GetSignBignum(left, &sign);
	if (RefSizeBignum(left) != 1) {
		bignum_copy_nosign_alloc(local, &left, left);
		SetSignBignum(left, SignNot(sign));
		*ret = left;
		return;
	}

	GetRootDataBignum(left, &root, &data);
	value = data[0];
	if (IsPlus(sign)) {
		if (value <= FIXNUM_UMIN)
			fixnum_alloc(local, ret, -(fixnum)value);
		else
			bignum_value_alloc(local, ret, SignMinus, value);
	}
	else {
		if (value <= FIXNUM_MAX)
			fixnum_alloc(local, ret, (fixnum)value);
		else
			bignum_value_alloc(local, ret, SignPlus, value);
	}
}

void sigrev_bignum_integer_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	sigrev_bignum_integer_alloc(local, left, ret);
}

void sigrev_bignum_integer_common(addr left, addr *ret)
{
	sigrev_bignum_integer_alloc(NULL, left, ret);
}

#define minus_vv_overflow(v1, v2) \
	(((v2) > 0 && (v1) < (FIXNUM_MIN + (v2))) || \
	 ((v2) < 0 && (v1) > (FIXNUM_MAX + (v2))))

static inline void minus_vv_bignum_local(LocalRoot local,
		fixnum v1, fixnum v2, addr *ret)
{
	int sign;
	fixed fixed1, fixed2;

	if (minus_vv_overflow(v1, v2)) {
		castfixed(v2, &sign, &fixed2);
		castfixed(v1, &sign, &fixed1);
		bignum_value_local(local, ret, sign, fixed1 + fixed2);
	}
	else {
		castfixed(v1 - v2, &sign, &fixed1);
		bignum_value_local(local, ret, sign, fixed1);
	}
}

static inline void minus_vv_real_alloc(LocalRoot local,
		fixnum v1, fixnum v2, addr *ret)
{
	int sign;
	fixed fixed1, fixed2;

	if (minus_vv_overflow(v1, v2)) {
		castfixed(v2, &sign, &fixed2);
		castfixed(v1, &sign, &fixed1);
		bignum_value_alloc(local, ret, sign, fixed1 + fixed2);
	}
	else {
		fixnum_alloc(local, ret, v1 - v2);
	}
}

void minus_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value1, value2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 - right */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		sigrev_fixnum_bignum_local(local, right, ret);
		return;
	}

	/* left - 0 */
	GetFixnum(right, &value2);
	if (value2 == 0) {
		fixnum_throw_local(local, left, ret);
		return;
	}

	minus_vv_bignum_local(local, value1, value2, ret);
}

static void minus_ff_real_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 - right */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		sigrev_fixnum_integer_alloc(local, right, ret);
		return;
	}

	/* left - 0 */
	GetFixnum(right, &value2);
	if (value2 == 0) {
		fixnum_throw_alloc(local, left, ret);
		return;
	}

	minus_vv_real_alloc(local, value1, value2, ret);
}

void minus_ff_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_ff_real_alloc(local, left, right, ret);
}

void minus_ff_real_common(addr left, addr right, addr *ret)
{
	minus_ff_real_alloc(NULL, left, right, ret);
}

static inline void minusfixnum_local(LocalRoot local,
		addr left, fixnum right, addr *ret)
{
	int sign;
	fixed value;

	Check(local == NULL, "local error");
	castfixed(right, &sign, &value);
	if (IsPlus(sign)) {
		minusvalue(local, left, value, ret);
	}
	else {
		plusvalue(local, left, value, ret);
	}
}

void minus_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	fixnum check;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 - right */
	GetFixnum(right, &check);
	if (zerop_bignum(left)) {
		castfixed(check, &sign, &value);
		bignum_value_local(local, ret, SignNot(sign), value);
		return;
	}

	/* left - 0 */
	if (check == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	minusfixnum_local(local, left, check, ret);
}

void minus_bf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 - right */
	if (zerop_bignum(left)) {
		sigrev_fixnum_integer_local(local, right, ret);
		return;
	}

	/* left - 0 */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	minusfixnum_local(local, left, check, &left);
	bignum_result_local(local, left, ret);
}

void minus_bf_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 - right */
	if (zerop_bignum(left)) {
		sigrev_fixnum_integer_common(right, ret);
		return;
	}

	/* left - 0 */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_throw_heap(left, ret);
		return;
	}

	push_local(local, &stack);
	minusfixnum_local(local, left, check, &left);
	bignum_result_heap(left, ret);
	rollback_local(local, stack);
}

void minus_fb_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* 0 - right */
	GetFixnum(left, &check);
	if (check == 0) {
		sigrev_bignum_bignum_local(local, right, ret);
		return;
	}

	/* left - 0 */
	if (zerop_bignum(right)) {
		bignum_fixnum_value_local(local, ret, check);
		return;
	}

	/* fixnum - bignum = -(bignum - fixnum) */
	minusfixnum_local(local, right, check, &right);
	sigrev_bignum_inplace(right);
	*ret = right;
}

void minus_fb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* 0 - right */
	GetFixnum(left, &check);
	if (check == 0) {
		sigrev_bignum_integer_local(local, right, ret);
		return;
	}

	/* left - 0 */
	if (zerop_bignum(right)) {
		fixnum_throw_local(local, left, ret);
		return;
	}

	/* fixnum - bignum = -(bignum - fixnum) */
	minusfixnum_local(local, right, check, &right);
	sigrev_bignum_inplace(right);
	bignum_result_local(local, right, ret);
}

void minus_fb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* 0 - right */
	GetFixnum(left, &check);
	if (check == 0) {
		sigrev_bignum_integer_common(right, ret);
		return;
	}

	/* left - 0 */
	if (zerop_bignum(right)) {
		fixnum_throw_heap(left, ret);
		return;
	}

	/* fixnum - bignum = -(bignum - fixnum) */
	push_local(local, &stack);
	minusfixnum_local(local, right, check, &right);
	sigrev_bignum_inplace(right);
	bignum_result_heap(right, ret);
	rollback_local(local, stack);
}

void minus_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		sigrev_bignum_bignum_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_local(local, left, ret);
		return;
	}
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 != sign2) {
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		*ret = left;
		return;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		bignum_zero_local(local, ret);
		return;
	}
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = SignNot(sign2);
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);
	*ret = left;
}

void minus_bb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		sigrev_bignum_integer_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_local(local, left, ret);
		return;
	}
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 != sign2) {
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		goto finish;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = SignNot(sign2);
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);

finish:
	bignum_result_local(local, left, ret);
}

void minus_bb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		sigrev_bignum_integer_common(right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_heap(left, ret);
		return;
	}
	push_local(local, &stack);
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 != sign2) {
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		goto finish;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		fixnum_heap(ret, 0);
		rollback_local(local, stack);
		return;
	}
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = SignNot(sign2);
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);

finish:
	bignum_result_heap(left, ret);
	rollback_local(local, stack);
}


/************************************************************
 *  bit.c
 ************************************************************/

#define FixedBit		(sizeoft(fixed) * 8UL)

/*
 *  bit
 */
int bitp(addr pos)
{
	fixnum value;

	if (GetType(pos) != LISPTYPE_FIXNUM)
		return 0;
	GetFixnum(pos, &value);

	return value == 0 || value == 1;
}

int bit_getint(addr pos, int *ret)
{
	fixnum value;

	if (GetType(pos) != LISPTYPE_FIXNUM)
		return 1;
	GetFixnum(pos, &value);
	if (value != 0 && value != 1)
		return 1;
	*ret = (int)value;

	return 0;
}

int bit_getint_error_(addr pos, int *ret)
{
	if (bit_getint(pos, ret))
		return fmte_("Bit value ~S must be a 0 or 1.", pos, NULL);
	return 0;
}


/*
 *  bitcons
 */
struct bitbuffer_struct {
	size_t index, array;
#ifdef __cplusplus
	fixed data[1];
#else
	fixed data[];
#endif
};

#define BitBufferStruct(x) ((struct bitbuffer_struct *)PtrBodySSa(x, 1))
#define RefBitBuffer(x)		RefArraySS(x,0)
#define GetBitBuffer(x,y)	GetArraySS(x,0,y)
#define SetBitBuffer(x,y)	SetArraySS(x,0,y)

static void bitbuffer_local(LocalRoot local, addr *ret, size_t fixedsize)
{
	addr pos;
	struct bitbuffer_struct *str;
	size_t allsize, bodysize;

	bodysize = fixedsize * sizeoft(fixed);
	allsize = sizeoft(struct bitbuffer_struct) + bodysize;
	Check(0xFF < allsize, "size error");
	local_smallsize(local, &pos, LISPSYSTEM_BITBUFFER, 1, allsize);
	str = BitBufferStruct(pos);
	str->index = 0;
	str->array = 0;
	memset(str->data, 0, bodysize);
	*ret = pos;
}

struct bitcons_struct {
	size_t bitsize, fixedsize, index;
};

#define BitConsStruct(x) ((struct bitcons_struct *)PtrBodySSa(x, 2))
#define GetBitConsRoot(x,y) GetArraySS(x,0,y)
#define GetBitConsTail(x,y) GetArraySS(x,1,y)
#define SetBitConsRoot(x,y) SetArraySS(x,0,y)
#define SetBitConsTail(x,y) SetArraySS(x,1,y)

#ifdef LISP_DEBUG
#define BITMEMORY_SIZE	1UL
#else
#define BITMEMORY_SIZE	(16UL * 8UL * 8UL)
#endif

static size_t getfixedsize(size_t bitsize)
{
	size_t size, fixedsize;

	/* bit -> byte */
	size = bitsize / 8;
	if (bitsize % 8)
		size++;

	/* byte -> word */
	fixedsize = size / sizeoft(fixed);
	if (size % sizeoft(fixed))
		fixedsize++;

	return fixedsize;
}

void bitcons_local(LocalRoot local, addr *ret, size_t bitsize)
{
	addr pos, child;
	struct bitcons_struct *str;
	size_t fixedsize;

	local_smallsize(local, &pos, LISPSYSTEM_BITCONS,
			2, sizeoft(struct bitcons_struct));
	if (bitsize == 0)
		bitsize = BITMEMORY_SIZE;
	fixedsize = getfixedsize(bitsize);
	bitbuffer_local(local, &child, fixedsize);
	SetBitConsRoot(pos, child);
	SetBitConsTail(pos, child);
	str = BitConsStruct(pos);
	str->fixedsize = fixedsize;
	str->bitsize = 8UL * fixedsize * sizeoft(fixed);
	str->index = 0;
	*ret = pos;
}

static void pushnext_bitcons(LocalRoot local, addr pos, addr *ret)
{
	addr child, one;
	struct bitcons_struct *str1;
	struct bitbuffer_struct *str2;

	GetBitConsTail(pos, &child);
	str1 = BitConsStruct(pos);
	str2 = BitBufferStruct(child);
	if (str2->index < str1->bitsize) {
		*ret = child;
	}
	else {
		bitbuffer_local(local, &one, str1->fixedsize);
		SetBitBuffer(child, one);
		SetBitConsTail(pos, one);
		*ret = one;
	}
}

void push_bitcons(LocalRoot local, addr pos, int value)
{
	addr child;
	struct bitcons_struct *str1;
	struct bitbuffer_struct *str2;
	size_t q, r;

	CheckType(pos, LISPSYSTEM_BITCONS);
	pushnext_bitcons(local, pos, &child);
	str2 = BitBufferStruct(child);
	q = str2->index / FixedBit;
	r = str2->index % FixedBit;
	if (value)
		str2->data[q] |= ((fixed)1UL) << r;
	str2->array = q;
	str2->index++;
	str1 = BitConsStruct(pos);
	str1->index++;
}


/*
 *  bitmemory
 */
void bitmemory_unsafe(LocalRoot local, addr *ret, size_t bitsize)
{
	addr pos;
	size_t fixedsize, allsize;
	struct bitmemory_struct *str;

	fixedsize = getfixedsize(bitsize);
	allsize = fixedsize * sizeoft(fixed) + sizeoft(struct bitmemory_struct);
	alloc_body(local, &pos, LISPTYPE_BITVECTOR, allsize);
	str = BitMemoryStruct(pos);
	str->bitsize = bitsize;
	str->fixedsize = fixedsize;
#ifdef LISP_DEBUG
	bitmemory_memset_byte(pos, 0xAA);
#endif
	*ret = pos;
}

void bitmemory_alloc(LocalRoot local, addr *ret, size_t bitsize)
{
	bitmemory_unsafe(local, ret, bitsize);
	bitmemory_memset(*ret, 0);
}

void bitmemory_local(LocalRoot local, addr *ret, size_t bitsize)
{
	Check(local == NULL, "local error");
	bitmemory_alloc(local, ret, bitsize);
}

void bitmemory_heap(addr *ret, size_t bitsize)
{
	bitmemory_alloc(NULL, ret, bitsize);
}

void bitmemory_cons_alloc(LocalRoot local, addr *ret, addr cons)
{
	struct bitcons_struct *str1;
	struct bitbuffer_struct *str2;
	fixed *data;
	addr pos;
	size_t bitsize, array, i;
#ifdef LISP_DEBUG
	size_t fixedsize;
#endif

	str1 = BitConsStruct(cons);
	bitsize = str1->index;
	bitmemory_unsafe(local, &pos, bitsize);
	if (bitsize) {
#ifdef LISP_DEBUG
		fixedsize = getfixedsize(bitsize);
#endif
		data = BitMemoryStruct(pos)->data;
		GetBitConsRoot(cons, &cons);
		for (i = 0; cons != Nil; i += array) {
			Check(fixedsize <= i, "size error");
			str2 = BitBufferStruct(cons);
			array = str2->array + 1UL;
			memcpy(data + i, str2->data, array * sizeoft(fixed));
			GetBitBuffer(cons, &cons);
		}
	}
	*ret = pos;
}

void bitmemory_cons_local(LocalRoot local, addr *ret, addr cons)
{
	Check(local == NULL, "local error");
	bitmemory_cons_alloc(local, ret, cons);
}

void bitmemory_cons_heap(addr *ret, addr cons)
{
	bitmemory_cons_alloc(NULL, ret, cons);
}

void bitmemory_char_heap(addr *ret, const char *str)
{
	char c;
	addr cons;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bitcons_local(local, &cons, 0);
	for (;;) {
		c = *str;
		if (c == '\0')
			break;
		Check(c != '0' && c != '1', "string error");
		push_bitcons(local, cons, (int)(c - '0'));
		str++;
	}
	bitmemory_cons_heap(ret, cons);
	rollback_local(local, stack);
}

void bitmemory_copy_alloc(LocalRoot local, addr *ret, addr pos)
{
	addr vector;
	size_t size;

	bitmemory_length(pos, &size);
	bitmemory_alloc(local, &vector, size);
	bitmemory_copy_unsafe(vector, pos, size);
	*ret = vector;
}

void bitmemory_copy_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	bitmemory_copy_alloc(local, ret, pos);
}

void bitmemory_copy_heap(addr *ret, addr pos)
{
	bitmemory_copy_alloc(NULL, ret, pos);
}

int bitmemoryp(addr pos)
{
	return GetType(pos) == LISPTYPE_BITVECTOR;
}

void bitmemory_memset_byte(addr pos, byte value)
{
	struct bitmemory_struct *str;
	str = BitMemoryStruct(pos);
	memset(str->data, value, str->fixedsize * sizeoft(fixed));
}

void bitmemory_memset(addr pos, int value)
{
	Check(value != 0 && value != 1, "value error");
	bitmemory_memset_byte(pos, value? 0xFF: 0x00);
}

void bitmemory_copy_unsafe(addr pos, addr refer, size_t bitsize)
{
	struct bitmemory_struct *str1, *str2;
	size_t fixedsize;
	fixed *data1;
	const fixed *data2;

	CheckType(pos, LISPTYPE_BITVECTOR);
	str1 = BitMemoryStruct(pos);
	str2 = BitMemoryStruct(refer);
	Check(str1->bitsize < bitsize, "size1 error");
	Check(str2->bitsize < bitsize, "size2 error");
	fixedsize = getfixedsize(bitsize);
	data1 = str1->data;
	data2 = str2->data;
	memcpy(data1, data2, fixedsize * sizeof(fixed));
}

void bitmemory_length(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_BITVECTOR);
	*ret = BitMemoryStruct(pos)->bitsize;
}

int bitmemory_equal_length(addr pos1, addr pos2)
{
	size_t size1, size2;

	CheckType(pos1, LISPTYPE_BITVECTOR);
	CheckType(pos2, LISPTYPE_BITVECTOR);
	bitmemory_length(pos1, &size1);
	bitmemory_length(pos2, &size2);

	return size1 == size2;
}

int bitmemory_equal(addr pos1, addr pos2)
{
	struct bitmemory_struct *str1, *str2;
	int check1, check2;
	size_t size, i, quot;

	CheckType(pos1, LISPTYPE_BITVECTOR);
	CheckType(pos2, LISPTYPE_BITVECTOR);
	bitmemory_length(pos1, &size);
	bitmemory_length(pos2, &i);
	if (size != i)
		return 0;
	quot = size / FixedBit;
	if (quot) {
		str1 = BitMemoryStruct(pos1);
		str2 = BitMemoryStruct(pos2);
		if (memcmp(str1->data, str2->data, quot * sizeof(fixed)))
			return 0;
	}
	for (i = quot * FixedBit; i < size; i++) {
		bitmemory_getint_unsafe(pos1, i, &check1);
		bitmemory_getint_unsafe(pos2, i, &check2);
		if (check1 != check2)
			return 0;
	}

	return 1;
}

int bitmemory_refint_debug(addr pos, size_t index)
{
	int check;
	check = 0;
	Error(bitmemory_getint_(pos, index, &check));
	return check;
}

void bitmemory_getint_unsafe(addr pos, size_t index, int *ret)
{
	struct bitmemory_struct *str;
	size_t q, r;

	CheckType(pos, LISPTYPE_BITVECTOR);
	str = BitMemoryStruct(pos);
	q = index / FixedBit;
	r = index % FixedBit;
	*ret = (int)((str->data[q] >> r) & 0x01);
}

void bitmemory_setint_unsafe(addr pos, size_t index, int value)
{
	struct bitmemory_struct *str;
	size_t q, r;

	CheckType(pos, LISPTYPE_BITVECTOR);
	str = BitMemoryStruct(pos);
	q = index / FixedBit;
	r = index % FixedBit;
	if (value)
		str->data[q] |= ((fixed)1UL) << r;
	else
		str->data[q] &= ~(((fixed)1UL) << r);
}

int bitmemory_getint_(addr pos, size_t index, int *ret)
{
	size_t size;

	CheckType(pos, LISPTYPE_BITVECTOR);
	bitmemory_length(pos, &size);
	if (size <= index) {
		*ret = 0;
		return fmte_("Out of range ~S.", intsizeh(index), NULL);
	}
	bitmemory_getint_unsafe(pos, index, ret);

	return 0;
}

int bitmemory_setint_(addr pos, size_t index, int value)
{
	size_t size;

	bitmemory_length(pos, &size);
	if (size <= index)
		return fmte_("Out of range ~S.", intsizeh(index), NULL);
	bitmemory_setint_unsafe(pos, index, value);

	return 0;
}

int bitmemory_get_(LocalRoot local, addr pos, size_t index, addr *ret)
{
	int check;
	size_t size;

	CheckType(pos, LISPTYPE_BITVECTOR);
	bitmemory_length(pos, &size);
	if (size <= index) {
		*ret = Nil;
		return fmte_("Out of range ~S.", intsizeh(index), NULL);
	}
	bitmemory_getint_unsafe(pos, index, &check);
	fixnum_alloc(local, ret, check? 1: 0);

	return 0;
}

int bitmemory_aref_(LocalRoot local, addr pos, addr args, addr *ret)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_BITVECTOR);
	if (! consp(args)) {
		*ret = Nil;
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	}
	GetCons(args, &arg, &args);
	if (args != Nil) {
		*ret = Nil;
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	}
	if (GetIndex_integer(arg, &index)) {
		*ret = Nil;
		return fmte_("Invalid index arg ~S.", arg, NULL);
	}

	return bitmemory_get_(local, pos, index, ret);
}

int bitmemory_set_(addr pos, size_t index, addr value)
{
	int check;
	size_t size;

	if (bit_getint(value, &check))
		return fmte_("The argument ~S must be bit type.", value, NULL);
	bitmemory_length(pos, &size);
	if (size <= index)
		return fmte_("Out of range ~S.", intsizeh(index), NULL);
	bitmemory_setint_unsafe(pos, index, check);

	return 0;
}

int bitmemory_setf_aref_(addr pos, addr args, addr value)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_BITVECTOR);
	if (GetStatusReadOnly(pos))
		return fmte_("The object ~S is constant.", pos, NULL);
	if (! consp(args))
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	GetCons(args, &arg, &args);
	if (args != Nil)
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	if (GetIndex_integer(arg, &index))
		return fmte_("Invalid index arg ~S.", arg, NULL);

	return bitmemory_set_(pos, index, value);
}

void bitmemory_bitcalc(addr pos, addr pos1, addr pos2, bitcalc_call call)
{
	struct bitmemory_struct *str, *str1, *str2;
	size_t size, i;
	fixed *data, *data1, *data2;

	str = BitMemoryStruct(pos);
	str1 = BitMemoryStruct(pos1);
	str2 = BitMemoryStruct(pos2);
	Check(str->bitsize != str1->bitsize, "size1 error");
	Check(str->bitsize != str2->bitsize, "size2 error");
	size = str->fixedsize;
	data = str->data;
	data1 = str1->data;
	data2 = str2->data;
	for (i = 0; i < size; i++)
		data[i] = (*call)(data1[i], data2[i]);
}

void bitmemory_bitnot(addr pos, addr pos1)
{
	struct bitmemory_struct *str, *str1;
	size_t size, i;
	fixed *data, *data1;

	str = BitMemoryStruct(pos);
	str1 = BitMemoryStruct(pos1);
	Check(str->bitsize != str1->bitsize, "size1 error");
	size = str->fixedsize;
	data = str->data;
	data1 = str1->data;
	for (i = 0; i < size; i++)
		data[i] = ~(data1[i]);
}

static int bitmemory_fillbit_(addr pos, int value, size_t index1, size_t index2)
{
	for (; index1 < index2; index1++) {
		Return(bitmemory_setint_(pos, index1, value));
	}

	return 0;
}

static int bitmemory_fillset_(addr pos, int value, size_t index1, size_t index2)
{
	size_t byte1, byte2, check;
	byte *data;

	byte1 = index1 / 8UL;
	byte2 = index2 / 8UL;
	if (byte1 == byte2)
		return bitmemory_fillbit_(pos, value, index1, index2);

	/* front */
	check = byte1 * 8UL;
	if (check != index1) {
		Return(bitmemory_fillbit_(pos, value, index1, check + 8UL));
	}

	/* tail */
	check = byte2 * 8UL;
	if (check != index2) {
		Return(bitmemory_fillbit_(pos, value, check, index2));
	}

	/* byte */
	data = (byte *)BitMemoryStruct(pos)->data;
	memset(data + byte1, value? 0xFF: 0x00, byte2 - byte1 - 1);

	return 0;
}

int bitmemory_fill_(addr pos, addr item, addr start, addr end)
{
	int value;
	size_t index1, index2;

	if (bit_getint(item, &value))
		return fmte_("FILL item ~S must be a bit (0 or 1 integer).", item, NULL);

	bitmemory_length(pos, &index1);
	Return(size_start_end_sequence_(start, end, index1, &index1, &index2, NULL));
	return bitmemory_fillset_(pos, value, index1, index2);
}

int bitmemory_subseq_index_(addr *ret, addr pos, size_t index1, size_t index2)
{
	int value;
	addr root;
	size_t i;

	bitmemory_unsafe(NULL, &root, index2 - index1);
	/* too slow */
	for (i = 0; index1 < index2; index1++, i++) {
		Return(bitmemory_getint_(pos, index1, &value));
		Return(bitmemory_setint_(root, i, value));
	}

	return Result(ret, root);
}

int bitmemory_subseq_(addr *ret, addr pos, addr start, addr end)
{
	size_t index1, index2;

	bitmemory_length(pos, &index1);
	Return(size_start_end_sequence_(start, end, index1, &index1, &index2, NULL));
	return bitmemory_subseq_index_(ret, pos, index1, index2);
}

int bitmemory_setget_(addr pos1, size_t index1, addr pos2, size_t index2)
{
	int value;

	Return(bitmemory_getint_(pos2, index2, &value));
	Return(bitmemory_setint_(pos1, index1, value));

	return 0;
}

int bitmemory_reverse_(LocalRoot local, addr *ret, addr pos)
{
	int temp;
	addr one;
	size_t size, x, y;

	bitmemory_length(pos, &size);
	bitmemory_unsafe(local, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		Return(bitmemory_getint_(pos, x, &temp));
		Return(bitmemory_setint_(one, y, temp));
	}

	return Result(ret, one);
}

int bitmemory_nreverse_(addr *ret, addr pos)
{
	int a, b;
	size_t size, x, y;

	bitmemory_length(pos, &size);
	if (size <= 1)
		return 0;
	x = 0;
	y = size - 1;
	while (x < y) {
		Return(bitmemory_getint_(pos, x, &a));
		Return(bitmemory_getint_(pos, y, &b));
		Return(bitmemory_setint_(pos, x, b));
		Return(bitmemory_setint_(pos, y, a));
		x++;
		y--;
	}

	return Result(ret, pos);
}


/*
 *  bvarray
 */
int array_bvarrayp(addr pos)
{
	struct array_struct *str;
	Check(GetType(pos) != LISPTYPE_ARRAY, "type error");
	str = ArrayInfoStruct(pos);
	return str->dimension == 1 && str->type == ARRAY_TYPE_BIT;
}

int bvarrayp(addr pos)
{
	if (GetType(pos) != LISPTYPE_ARRAY)
		return 0;
	return array_bvarrayp(pos);
}

int bitvectorp(addr pos)
{
	return GetType(pos) == LISPTYPE_BITVECTOR || bvarrayp(pos);
}

int simple_array_bvarrayp(addr pos)
{
	struct array_struct *str;
	Check(GetType(pos) != LISPTYPE_ARRAY, "type error");
	str = ArrayInfoStruct(pos);
	return str->simple && str->dimension == 1 && str->type == ARRAY_TYPE_BIT;
}

int simple_bvarrayp(addr pos)
{
	if (GetType(pos) != LISPTYPE_ARRAY)
		return 0;
	return simple_array_bvarrayp(pos);
}

int simple_bitvectorp(addr pos)
{
	return GetType(pos) == LISPTYPE_BITVECTOR || simple_bvarrayp(pos);
}


void bvarray_length(addr pos, size_t *ret)
{
	Check(! bvarrayp(pos), "type error");
	*ret = ArrayInfoStruct(pos)->front;
}

int bvarray_getint_(addr pos, size_t index, int *ret)
{
	int check;

	if (ArrayInfoStruct(pos)->front <= index) {
		*ret = 0;
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	}
	Return(array_get_bit_(pos, index, &check));
	return Result(ret, check);
}

int bvarray_setint_(addr pos, size_t index, int value)
{
	if (ArrayInfoStruct(pos)->front <= index)
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	return array_set_bit_(pos, index, value);
}


/*
 *  bitvector
 */
int bitvector_length_(addr pos, size_t *ret)
{
	if (bitmemoryp(pos)) {
		bitmemory_length(pos, ret);
		return 0;
	}
	if (bvarrayp(pos)) {
		bvarray_length(pos, ret);
		return 0;
	}
	return fmte_("type error", NULL);
}

int bitvector_getint_(addr pos, size_t index, int *ret)
{
	if (bitmemoryp(pos))
		return bitmemory_getint_(pos, index, ret);

	if (bvarrayp(pos))
		return bvarray_getint_(pos, index, ret);

	*ret = 0;
	return fmte_("type error", NULL);
}

int bitvector_setint_(addr pos, size_t index, int value)
{
	if (bitmemoryp(pos))
		return bitmemory_setint_(pos, index, value);

	if (bvarrayp(pos))
		return bvarray_setint_(pos, index, value);

	return fmte_("type error", NULL);
}

static int bitmemory_array_equal(addr left, addr right, int *ret)
{
	int check1, check2;
	size_t size, i;

	Check(! bitmemoryp(left), "type left error");
	Check(! bvarrayp(right), "type right error");
	bitmemory_length(left, &size);
	bvarray_length(right, &i);
	if (size != i)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(left, i, &check1));
		Return(bvarray_getint_(right, i, &check2));
		if (check1 != check2)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int bvarray_array_equal(addr left, addr right, int *ret)
{
	int check1, check2;
	size_t size, i;

	Check(! bvarrayp(left), "type left error");
	Check(! bvarrayp(right), "type right error");
	bvarray_length(left, &size);
	bvarray_length(right, &i);
	if (size != i)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		Return(bvarray_getint_(left, i, &check1));
		Return(bvarray_getint_(right, i, &check2));
		if (check1 != check2)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int bitvector_equal_(addr left, addr right, int *ret)
{
	Check(! bitvectorp(left), "type left error");
	Check(! bitvectorp(right), "type right error");
	if (bitmemoryp(left)) {
		if (bitmemoryp(right))
			return Result(ret, bitmemory_equal(left, right));
		if (bvarrayp(right))
			return bitmemory_array_equal(left, right, ret);
	}
	if (bvarrayp(left)) {
		if (bitmemoryp(right))
			return bitmemory_array_equal(right, left, ret);
		if (bvarrayp(right))
			return bvarray_array_equal(left, right, ret);
	}

	*ret = 0;
	return fmte_("type error", NULL);
}


/************************************************************
 *  boole.c
 ************************************************************/

typedef int (*BooleCall)(LocalRoot, addr, addr, addr *);
static BooleCall BooleTable[Boole_Size];

union boole_fixnumfixed {
	fixnum sign;
	fixed unsign;
};

/*
 *  boole-struct
 */
struct boole_struct {
	unsigned sign : 1;
	unsigned carry : 1;
	fixed *data, v, temp;
	size_t size, index;
	addr pos;
};

static void boole_struct_fixnum(struct boole_struct *ptr, addr pos)
{
	int check;
	unsigned sign;

	CheckType(pos, LISPTYPE_FIXNUM);
	castfixed_fixnum(pos, &check, &(ptr->temp));
	sign = IsPlus(check)? 0: 1;
	ptr->sign = sign;
	ptr->carry = sign;
	ptr->data = &(ptr->temp);
	ptr->v = sign? ~((fixed)0): 0;
	ptr->size = 1;
	ptr->index = 0;
	ptr->pos = pos;
}

static void boole_struct_bignum(struct boole_struct *ptr, addr pos)
{
	int check;
	unsigned sign;
	fixed *data;
	size_t size;

	CheckType(pos, LISPTYPE_BIGNUM);
	GetSignBignum(pos, &check);
	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);
	sign = IsPlus(check)? 0: 1;
	ptr->sign = sign;
	ptr->carry = sign;
	ptr->data = data;
	ptr->temp = 0;
	ptr->v = sign? ~((fixed)0): 0;
	ptr->size = size;
	ptr->index = 0;
	ptr->pos = pos;
}

static int boole_struct_integer_(struct boole_struct *ptr, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			boole_struct_fixnum(ptr, pos);
			break;

		case LISPTYPE_BIGNUM:
			boole_struct_bignum(ptr, pos);
			break;

		default:
			return TypeError_(pos, INTEGER);
	}

	return 0;
}

static int boole_struct_sign(
		struct boole_struct *a,
		struct boole_struct *b,
		fixed (*call)(fixed, fixed))
{
	fixed v1 = a->sign? a->v: 0;
	fixed v2 = b->sign? b->v: 0;
	return (*call)(v1, v2)? SignMinus: SignPlus;
}

static void boole_struct_alloc(LocalRoot local,
		struct boole_struct *a,
		struct boole_struct *b,
		struct boole_struct *c,
		fixed (*call)(fixed, fixed))
{
	int sign;
	size_t size;
	addr pos;

	sign = boole_struct_sign(a, b, call);
	size = (a->size < b->size)? b->size: a->size;
	size++;
	bignum_local(local, &pos, sign, size);
	SetSizeBignum(pos, size);
	boole_struct_bignum(c, pos);
}

static fixed boole_struct_get(struct boole_struct *ptr)
{
	fixed v;

	/* out of range */
	if (ptr->size <= ptr->index)
		return ptr->v;
	/* plus */
	v = ptr->data[ptr->index++];
	if (ptr->sign == 0)
		return v;
	/* not carry */
	if (ptr->carry == 0)
		return ~v;
	/* carry over */
	if (v == 0)
		return 0;
	/* carry end */
	ptr->carry = 0;
	return (~v) + 1ULL;
}

static void boole_struct_set(struct boole_struct *ptr, fixed v)
{
	Check(ptr->size <= ptr->index, "size error");
	ptr->data[ptr->index++] = v;
}

static void boole_struct_result(struct boole_struct *ptr)
{
	addr pos;
	fixed v;
	size_t size, i;
	struct boole_struct boole1, boole2;

	pos = ptr->pos;
	if (ptr->sign) {
		boole_struct_bignum(&boole1, pos);
		boole_struct_bignum(&boole2, pos);
		size = ptr->size;
		for (i = 0; i < size; i++) {
			v = boole_struct_get(&boole1);
			boole_struct_set(&boole2, v);
		}
	}
	sizepress_bignum(pos);
}


/*
 *  bit operator
 */
static void boole_call2_common(
		struct boole_struct *boole1,
		struct boole_struct *boole2,
		struct boole_struct *boole3,
		fixed (*call)(fixed, fixed))
{
	fixed x, y;
	size_t i, size;

	size = boole3->size;
	for (i = 0; i < size; i++) {
		x = boole_struct_get(boole1);
		y = boole_struct_get(boole2);
		boole_struct_set(boole3, (*call)(x, y));
	}
}

static int boole_call_bignum_(LocalRoot local, addr a, addr b, addr *ret,
		fixed (*call)(fixed, fixed))
{
	struct boole_struct boole1, boole2, boole3;
	LocalStack stack;

	push_local(local, &stack);
	Return(boole_struct_integer_(&boole1, a));
	Return(boole_struct_integer_(&boole2, b));
	boole_struct_alloc(local, &boole1, &boole2, &boole3, call);
	boole_call2_common(&boole1, &boole2, &boole3, call);
	boole_struct_result(&boole3);
	bignum_result_heap(boole3.pos, ret);
	rollback_local(local, stack);

	return 0;
}

static void boole_call_fixnum(LocalRoot local, addr a, addr b, addr *ret,
		fixed (*call)(fixed, fixed))
{
	union boole_fixnumfixed x, y;

	GetFixnum(a, &(x.sign));
	GetFixnum(b, &(y.sign));
	x.unsign = (*call)(x.unsign, y.unsign);
	fixnum_heap(ret, x.sign);
}

static int boole_fixnum_p(addr a, addr b)
{
	fixnum x, y;

	if ((! fixnump(a)) || (! fixnump(b)))
		return 0;
	GetFixnum(a, &x);
	GetFixnum(b, &y);

	return (x != FIXNUM_MIN) && (y != FIXNUM_MIN);
}

static int boole_call_common_(LocalRoot local, addr a, addr b, addr *ret,
		fixed (*call)(fixed, fixed))
{
	if (boole_fixnum_p(a, b)) {
		boole_call_fixnum(local, a, b, ret, call);
		return 0;
	}
	else {
		return boole_call_bignum_(local, a, b, ret, call);
	}
}

static int logcall_common_(LocalRoot local, addr args, addr *ret,
		fixnum ident, int (*call)(LocalRoot, addr, addr, addr *))
{
	addr left, right;

	/* no args */
	if (args == Nil) {
		fixnum_heap(ret, ident);
		return 0;
	}

	/* only one */
	Return(getcons_(args, &left, &args));
	if (args == Nil)
		return Result(ret, left);

	/* list */
	while (args != Nil) {
		Return(getcons_(args, &right, &args));
		Return((*call)(local, left, right, &left));
	}

	return Result(ret, left);
}


/*
 *  logand
 */
static fixed boole_call_and(fixed a, fixed b)
{
	return a & b;
}

static int boole_and_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	return boole_call_common_(local, a, b, ret, boole_call_and);
}

int logand_common_(LocalRoot local, addr args, addr *ret)
{
	return logcall_common_(local, args, ret, -1, boole_and_common_);
}


/*
 *  logandc1
 */
static fixed boole_call_andc1(fixed a, fixed b)
{
	return (~a) & b;
}

int logandc1_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	return boole_call_common_(local, a, b, ret, boole_call_andc1);
}

#define boole_andc1_common_ logandc1_common_


/*
 *  logandc2
 */
static fixed boole_call_andc2(fixed a, fixed b)
{
	return a & (~b);
}

int logandc2_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	return boole_call_common_(local, a, b, ret, boole_call_andc2);
}

#define boole_andc2_common_ logandc2_common_


/*
 *  logeqv
 */
static fixed boole_call_eqv(fixed a, fixed b)
{
	return ~(a ^ b);
}

static int boole_eqv_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	return boole_call_common_(local, a, b, ret, boole_call_eqv);
}

int logeqv_common_(LocalRoot local, addr args, addr *ret)
{
	return logcall_common_(local, args, ret, -1, boole_eqv_common_);
}


/*
 *  logior
 */
static fixed boole_call_ior(fixed a, fixed b)
{
	return a | b;
}

static int boole_ior_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	return boole_call_common_(local, a, b, ret, boole_call_ior);
}

int logior_common_(LocalRoot local, addr args, addr *ret)
{
	return logcall_common_(local, args, ret, 0, boole_ior_common_);
}


/*
 *  lognand
 */
static fixed boole_call_nand(fixed a, fixed b)
{
	return ~(a & b);
}

int lognand_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	return boole_call_common_(local, a, b, ret, boole_call_nand);
}

#define boole_nand_common_ lognand_common_


/*
 *  lognor
 */
static fixed boole_call_nor(fixed a, fixed b)
{
	return ~(a | b);
}

int lognor_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	return boole_call_common_(local, a, b, ret, boole_call_nor);
}

#define boole_nor_common_ lognor_common_


/*
 *  lognot
 */
int lognot_common_(LocalRoot local, addr a, addr *ret)
{
	addr b;
	fixnum_heap(&b, 0);
	return logorc1_common_(local, a, b, ret);
}

static int boole_c1_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	return lognot_common_(local, a, ret);
}

static int boole_c2_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	return lognot_common_(local, b, ret);
}


/*
 *  logorc1
 */
static fixed boole_call_orc1(fixed a, fixed b)
{
	return (~a) | b;
}

int logorc1_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	return boole_call_common_(local, a, b, ret, boole_call_orc1);
}

#define boole_orc1_common_ logorc1_common_


/*
 *  logorc2
 */
static fixed boole_call_orc2(fixed a, fixed b)
{
	return a | (~b);
}

int logorc2_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	return boole_call_common_(local, a, b, ret, boole_call_orc2);
}

#define boole_orc2_common_ logorc2_common_


/*
 *  logxor
 */
static fixed boole_call_xor(fixed a, fixed b)
{
	return a ^ b;
}

static int boole_xor_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	return boole_call_common_(local, a, b, ret, boole_call_xor);
}

int logxor_common_(LocalRoot local, addr args, addr *ret)
{
	return logcall_common_(local, args, ret, 0, boole_xor_common_);
}


/*
 *  boole
 */
static int boole_1_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	return integer_throw_heap_(a, ret);
}

static int boole_2_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	return integer_throw_heap_(b, ret);
}

static int boole_clr_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	fixnum_heap(ret, 0);
	return 0;
}

static int boole_set_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	fixnum_heap(ret, -1);
	return 0;
}

int boole_common_(LocalRoot local, addr op, addr a, addr b, addr *ret)
{
	fixnum index;
	BooleCall call;

	CheckLocalType(local, op, LISPTYPE_FIXNUM);
	GetFixnum(op, &index);
	Check(index < 0 || ((fixnum)Boole_Size) <= index, "index error");
	call = BooleTable[(int)index];
	Check(call == NULL, "boole call error");
	return (*call)(local, a, b, ret);
}


/*
 *  logbitp
 */
static int logbitp_fixnum(addr index, addr pos)
{
	union boole_fixnumfixed u;
	size_t size;

	CheckType(pos, LISPTYPE_FIXNUM);
	GetFixnum(pos, &(u.sign));
	if (GetIndex_integer(index, &size) || LISP_INTEGER_BIT <= size) {
		/* out of range */
		return u.sign < 0;
	}
	else {
		/* fixnum */
		return (int)((u.unsign >> size) & 1);
	}
}

static int logbitp_plus(addr pos, size_t size)
{
	size_t m, n;
	fixed *data;

	n = size / BIGNUM_FULLBIT;
	m = size % BIGNUM_FULLBIT;
	GetDataBignum(pos, &data);
	return (int)((data[n] >> m) & 1);
}

static int logbitp_minus(addr pos, size_t size)
{
	size_t m, n, i;
	fixed *data, v, carry;

	n = size / BIGNUM_FULLBIT;
	m = size % BIGNUM_FULLBIT;
	GetDataBignum(pos, &data);

	carry = 1;
	for (i = 0; i < n; i++) {
		if (data[i] != 0) {
			carry = 0;
			break;
		}
	}
	v = ~(data[n]) + carry;

	return (int)((v >> m) & 1);
}

static int logbitp_bignum(addr index, addr pos)
{
	int sign;
	size_t size, check;

	CheckType(pos, LISPTYPE_BIGNUM);
	/* range check */
	GetSignBignum(pos, &sign);
	GetSizeBignum(pos, &size);
	check = size * BIGNUM_FULLBIT;
	if (GetIndex_integer(index, &size) || check <= size)
		return IsPlus(sign)? 0: 1;
	/* sign */
	if (IsPlus(sign))
		return logbitp_plus(pos, size);
	else
		return logbitp_minus(pos, size);
}

int logbitp_common_(addr index, addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, logbitp_fixnum(index, pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, logbitp_bignum(index, pos));

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}


/*
 *  logcount
 */
static size_t logcount_fixed(fixed v, unsigned sign)
{
	size_t count;

	if (sign)
		v = ~v;
	for (count = 0; v; v >>= 1) {
		if (v & 1)
			count++;
	}

	return count;
}

static size_t logcount_fixnum(addr pos)
{
	union boole_fixnumfixed u;

	CheckType(pos, LISPTYPE_FIXNUM);
	GetFixnum(pos, &(u.sign));
	return logcount_fixed(u.unsign, u.sign < 0);
}

static size_t logcount_bignum(addr pos)
{
	unsigned sign;
	fixed v;
	size_t i, size, count;
	struct boole_struct boole;

	boole_struct_bignum(&boole, pos);
	sign = boole.sign;
	size = boole.size;
	count = 0;
	for (i = 0; i < size; i++) {
		v = boole_struct_get(&boole);
		count += logcount_fixed(v, sign);
	}

	return count;
}

int logcount_common_(addr pos, size_t *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, logcount_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, logcount_bignum(pos));

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}


/*
 *  logtest
 */
int logtest_common_(LocalRoot local, addr a, addr b, int *ret)
{
	struct boole_struct boole1, boole2;
	fixed x, y;
	size_t size, i;

	Return(boole_struct_integer_(&boole1, a));
	Return(boole_struct_integer_(&boole2, b));
	size = (boole1.size < boole2.size)? boole2.size: boole1.size;
	size++; /* sign check */
	for (i = 0; i < size; i++) {
		x = boole_struct_get(&boole1);
		y = boole_struct_get(&boole2);
		if (x & y)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}


/*
 *  initialize
 */
void init_boole(void)
{
	BooleTable[Boole_1] = boole_1_common_;
	BooleTable[Boole_2] = boole_2_common_;
	BooleTable[Boole_And] = boole_and_common_;
	BooleTable[Boole_AndC1] = boole_andc1_common_;
	BooleTable[Boole_AndC2] = boole_andc2_common_;
	BooleTable[Boole_C1] = boole_c1_common_;
	BooleTable[Boole_C2] = boole_c2_common_;
	BooleTable[Boole_Clr] = boole_clr_common_;
	BooleTable[Boole_Eqv] = boole_eqv_common_;
	BooleTable[Boole_Ior] = boole_ior_common_;
	BooleTable[Boole_Nand] = boole_nand_common_;
	BooleTable[Boole_Nor] = boole_nor_common_;
	BooleTable[Boole_Orc1] = boole_orc1_common_;
	BooleTable[Boole_Orc2] = boole_orc2_common_;
	BooleTable[Boole_Set] = boole_set_common_;
	BooleTable[Boole_Xor] = boole_xor_common_;
}


/*
 *  deposit-field
 */
static size_t deposit_field_maxsize(
		struct boole_struct *ptr1,
		struct boole_struct *ptr2,
		struct bytespec_mask *mask)
{
	size_t size1, size2, size3;

	size1 = bytespec_mask_getsize(mask);
	size2 = ptr1->size;
	size3 = ptr2->size;
	size1 = (size1 < size2)? size2: size1;
	size1 = (size1 < size3)? size3: size1;

	return size1;
}

static void deposit_field_alloc(LocalRoot local,
		struct boole_struct *init,
		struct boole_struct *right,
		size_t size)
{
	int sign;
	addr pos;

	sign = right->sign? SignMinus: SignPlus;
	bignum_local(local, &pos, sign, size);
	SetSizeBignum(pos, size);
	boole_struct_bignum(init, pos);
}

static void deposit_field_copy(
		struct boole_struct *ptr1,
		struct boole_struct *ptr2,
		struct boole_struct *write,
		struct bytespec_mask *mask,
		size_t size)
{
	fixed v1, v2, m1, m2;
	size_t i;

	for (i = 0; i < size; i++) {
		v1 = boole_struct_get(ptr1);
		v2 = boole_struct_get(ptr2);
		m1 = bytespec_mask_get(mask);
		m2 = ~m1;
		boole_struct_set(write, (v1 & m1) | (v2 & m2));
	}
}

int deposit_field_common_(LocalRoot local, addr *ret, addr a, addr spec, addr b)
{
	struct boole_struct str1, str2, write;
	struct bytespec_mask mask;
	size_t size;
	LocalStack stack;

	CheckLocalType(local, spec, LISPTYPE_BYTESPEC);
	bytespec_mask_init(&mask, spec);
	if (mask.size == 0)
		return integer_throw_heap_(b, ret);
	Return(boole_struct_integer_(&str1, a));
	Return(boole_struct_integer_(&str2, b));
	size = deposit_field_maxsize(&str1, &str2, &mask);

	push_local(local, &stack);
	deposit_field_alloc(local, &write, &str2, size);
	deposit_field_copy(&str1, &str2, &write, &mask, size);
	boole_struct_result(&write);
	bignum_result_heap(write.pos, ret);
	rollback_local(local, stack);

	return 0;
}


/*
 *  dpb
 */
struct dpb_struct {
	struct boole_struct boole;
	size_t size, start, index, bit;
	fixed carry;
};

static int dbp_struct_integer_(struct dpb_struct *ptr, addr pos, size_t shift)
{
	size_t m, n, size;

	Return(boole_struct_integer_(&(ptr->boole), pos));
	n = shift / BIGNUM_FULLBIT;
	m = shift % BIGNUM_FULLBIT;
	size = m? n+1: n;
	size += ptr->boole.size;

	ptr->start = n;
	ptr->bit = m;
	ptr->size = size;
	ptr->carry = 0;
	ptr->index = 0;

	return 0;
}

static size_t dpb_maxsize(
		struct dpb_struct *ptr1,
		struct boole_struct *ptr2,
		struct bytespec_mask *mask)
{
	size_t size1, size2, size3;

	size1 = bytespec_mask_getsize(mask);
	size2 = ptr1->size;
	size3 = ptr2->size;
	size1 = (size1 < size2)? size2: size1;
	size1 = (size1 < size3)? size3: size1;

	return size1;
}

static fixed dpb_struct_get(struct dpb_struct *ptr)
{
	fixed v, carry;
	size_t index, bit;

	index = ptr->index++;
	if (index < ptr->start)
		return 0;
	bit = ptr->bit;
	v = boole_struct_get(&(ptr->boole));
	if (bit) {
		carry = ptr->carry;
		ptr->carry = v >> (BIGNUM_FULLBIT - bit);
		v = (v << bit) | carry;
	}

	return v;
}

static void dpb_alloc(LocalRoot local,
		struct boole_struct *init,
		struct boole_struct *right,
		size_t size)
{
	deposit_field_alloc(local, init, right, size);
}

static void dpb_copy(
		struct dpb_struct *ptr1,
		struct boole_struct *ptr2,
		struct boole_struct *write,
		struct bytespec_mask *mask,
		size_t size)
{
	fixed v1, v2, m1, m2;
	size_t i;

	for (i = 0; i < size; i++) {
		v1 = dpb_struct_get(ptr1);
		v2 = boole_struct_get(ptr2);
		m1 = bytespec_mask_get(mask);
		m2 = ~m1;
		boole_struct_set(write, (v1 & m1) | (v2 & m2));
	}
}

int dpb_common_(LocalRoot local, addr *ret, addr a, addr spec, addr b)
{
	struct dpb_struct str1;
	struct boole_struct str2, write;
	struct bytespec_mask mask;
	size_t size;
	LocalStack stack;

	CheckLocalType(local, spec, LISPTYPE_BYTESPEC);
	bytespec_mask_init(&mask, spec);
	if (mask.size == 0)
		return integer_throw_heap_(b, ret);
	Return(dbp_struct_integer_(&str1, a, mask.position));
	Return(boole_struct_integer_(&str2, b));
	size = dpb_maxsize(&str1, &str2, &mask);

	push_local(local, &stack);
	dpb_alloc(local, &write, &str2, size);
	dpb_copy(&str1, &str2, &write, &mask, size);
	boole_struct_result(&write);
	bignum_result_heap(write.pos, ret);
	rollback_local(local, stack);

	return 0;
}


/*
 *  ldb
 */
struct ldb_struct {
	struct boole_struct boole;
	size_t size, count, shift, start;
	fixed carry;
};

static void boole_struct_front(struct boole_struct *ptr, size_t front)
{
	fixed *data;

	/* out of range */
	if (ptr->size <= ptr->index) {
		return;
	}
	if (ptr->size <= ptr->index + front) {
		ptr->index += front;
		return;
	}
	/* plus */
	if (ptr->sign == 0) {
		ptr->index += front;
		return;
	}
	/* not carry */
	if (ptr->carry == 0) {
		ptr->index += front;
		return;
	}
	/* carry */
	data = ptr->data;
	while (front) {
		front--;
		if (data[ptr->index++]) {
			ptr->carry = 0;
			break;
		}
		if (ptr->size <= ptr->index) {
			return;
		}
	}
	/* carry end */
	ptr->index += front;
}

static int ldb_struct_integer_(struct ldb_struct *ptr, addr var, addr spec)
{
	size_t size, pos, shift, div;
	struct bytespec_struct *str;
	struct boole_struct *boole;

	/* bytespec */
	str = ByteSpecStruct(spec);
	size = str->size;
	pos = str->position;

	/* size == 0 */
	if (size == 0) {
		ptr->count = 0;
		return 0;
	}

	/* size */
	div = size / BIGNUM_FULLBIT;
	shift = size % BIGNUM_FULLBIT;
	ptr->count = shift? (div + 1): div;

	/* start */
	div = pos / BIGNUM_FULLBIT;
	shift = pos % BIGNUM_FULLBIT;

	/* boole */
	boole = &(ptr->boole);
	Return(boole_struct_integer_(boole, var));
	boole_struct_front(boole, div);

	/* struct */
	ptr->carry = shift? boole_struct_get(boole): 0;
	ptr->shift = shift;
	ptr->size = size;

	return 0;
}

static void ldb_struct_alloc(LocalRoot local, size_t count, addr *ret)
{
	addr pos;

	bignum_local(local, &pos, SignPlus, count);
	SetSizeBignum(pos, count);
	*ret = pos;
}

static fixed ldb_struct_get(struct ldb_struct *ptr)
{
	fixed v1, v2, v;
	size_t shift;

	v1 = ptr->carry;
	v2 = boole_struct_get(&(ptr->boole));
	shift = ptr->shift;
	if (shift == 0)
		return v2;
	v = (v2 << (BIGNUM_FULLBIT - shift)) | (v1 >> shift);
	ptr->carry = v2;

	return v;
}

int ldb_common_(LocalRoot local, addr *ret, addr spec, addr pos)
{
	LocalStack stack;
	struct ldb_struct str;
	size_t size, count, i;
	fixed *data, v, m;

	CheckLocalType(local, spec, LISPTYPE_BYTESPEC);
	Return(ldb_struct_integer_(&str, pos, spec));
	count = str.count;
	if (count == 0) {
		fixnum_heap(ret, 0);
		return 0;
	}

	push_local(local, &stack);
	ldb_struct_alloc(local, count, &pos);
	GetDataBignum(pos, &data);
	size = str.size;
	for (i = 0; i < count; i++) {
		if (size == 0) {
			data[i] = 0;
		}
		else if (BIGNUM_FULLBIT <= size) {
			data[i] = ldb_struct_get(&str);
			size -= BIGNUM_FULLBIT;
		}
		else {
			v = ldb_struct_get(&str);
			m = (1ULL << size) - 1ULL;
			data[i] = v & m;
			size = 0;
		}
	}
	sizepress_bignum(pos);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);

	return 0;
}


/*
 *  (define-setf-expander ldb (spec place &environment env)
 *    (multiple-value-bind (a b g w r) (get-setf-expansion place env)
 *      (let ((v (gensym))
 *            (g (car g)))
 *        (values `(,g ,@a)
 *                `(nil ,@b)
 *                `(,v)
 *                `(prog1 ,v
 *                        (setq ,g (dpb ,v ,spec ,r))
 *                        ,w)
 *                r))))
 */
int function_setf_ldb(Execute ptr, addr form, addr env)
{
	addr args, spec, place, ra, rb, rg, rw, rr, a, b, g, w, r, v;
	addr prog1, setq, dpb;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &spec, &args))
		goto error;
	if (! consp_getcons(args, &place, &args))
		goto error;
	if (args != Nil)
		goto error;

	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	GetConst(COMMON_PROG1, &prog1);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_DPB, &dpb);
	Return(make_gensym_(ptr, &v));
	Return_getcar(g, &g);
	cons_heap(&ra, g, a);
	cons_heap(&rb, Nil, b);
	conscar_heap(&rg, v);
	list_heap(&dpb, dpb, v, spec, r, NULL);
	list_heap(&setq, setq, g, dpb, NULL);
	list_heap(&rw, prog1, v, setq, w, NULL);
	rr = r;
	setvalues_control(ptr, ra, rb, rg, rw, rr, NULL);
	return 0;

error:
	return fmte_("SETF-LDB form ~S must be a (byte-space place) form.", form, NULL);
}


/*
 *  ldb-test
 */
int ldb_test_common_(addr spec, addr pos, int *ret)
{
	struct ldb_struct str;
	size_t size, count, i;
	fixed v, m;

	CheckType(spec, LISPTYPE_BYTESPEC);
	Return(ldb_struct_integer_(&str, pos, spec));
	count = str.count;
	if (count == 0)
		return Result(ret, 0);

	size = str.size;
	for (i = 0; i < count; i++) {
		if (size == 0) {
			break;
		}
		else if (BIGNUM_FULLBIT <= size) {
			if (ldb_struct_get(&str))
				return Result(ret, 1);
			size -= BIGNUM_FULLBIT;
		}
		else {
			v = ldb_struct_get(&str);
			m = (1ULL << size) - 1ULL;
			if (v & m)
				return Result(ret, 1);
			size = 0;
		}
	}

	return Result(ret, 0);
}


/*
 *  mask-field
 */
static size_t mask_field_maxsize(
		struct boole_struct *ptr,
		struct bytespec_mask *mask)
{
	size_t size1 = bytespec_mask_getsize(mask);
	size_t size2 = ptr->size;
	return (size1 < size2)? size2: size1;
}

static void mask_field_struct_alloc(LocalRoot local, size_t count, addr *ret)
{
	ldb_struct_alloc(local, count, ret);
}

static void mask_field_copy(
		struct boole_struct *ptr,
		struct bytespec_mask *mask,
		addr pos,
		size_t size)
{
	fixed v, m;
	fixed *data;
	size_t i;

	GetDataBignum(pos, &data);
	for (i = 0; i < size; i++) {
		v = boole_struct_get(ptr);
		m = bytespec_mask_get(mask);
		data[i] = v & m;
	}
}

int mask_field_common_(LocalRoot local, addr *ret, addr spec, addr pos)
{
	struct boole_struct str;
	struct bytespec_mask mask;
	size_t size;
	LocalStack stack;

	CheckLocalType(local, spec, LISPTYPE_BYTESPEC);
	bytespec_mask_init(&mask, spec);
	if (mask.size == 0) {
		fixnum_heap(ret, 0);
		return 0;
	}
	Return(boole_struct_integer_(&str, pos));
	size = mask_field_maxsize(&str, &mask);

	push_local(local, &stack);
	mask_field_struct_alloc(local, size, &pos);
	mask_field_copy(&str, &mask, pos, size);
	sizepress_bignum(pos);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);

	return 0;
}


/*
 *  (define-setf-expander mask-field (spec place &environment env)
 *    (multiple-value-bind (a b g w r) (get-setf-expansion place env)
 *      (let ((v (gensym))
 *            (g (car g)))
 *        (values `(,g ,@a)
 *                `(nil ,@b)
 *                `(,v)
 *                `(prog1 ,v
 *                        (setq ,g (deposit-field ,v ,spec ,r))
 *                        ,w)
 *                r))))
 */
int function_setf_mask_field(Execute ptr, addr form, addr env)
{
	addr args, spec, place, ra, rb, rg, rw, rr, a, b, g, w, r, v;
	addr prog1, setq, deposit;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &spec, &args))
		goto error;
	if (! consp_getcons(args, &place, &args))
		goto error;
	if (args != Nil)
		goto error;

	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	GetConst(COMMON_PROG1, &prog1);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_DEPOSIT_FIELD, &deposit);
	Return(make_gensym_(ptr, &v));
	Return_getcar(g, &g);
	cons_heap(&ra, g, a);
	cons_heap(&rb, Nil, b);
	conscar_heap(&rg, v);
	list_heap(&deposit, deposit, v, spec, r, NULL);
	list_heap(&setq, setq, g, deposit, NULL);
	list_heap(&rw, prog1, v, setq, w, NULL);
	rr = r;
	setvalues_control(ptr, ra, rb, rg, rw, rr, NULL);
	return 0;

error:
	return fmte_("SETF-DEPOSIT-FIELD form ~S "
			"must be a (byte-space place) form.", form, NULL);
}


/************************************************************
 *  buffering.c
 ************************************************************/

/*
 *  bufcell
 */
#define PtrBufCell(x) ((byte *)PtrBodyB2(x))
static void bufcell_heap(addr *ret, size_t cell)
{
	addr pos;

	Check(0xFFFF < cell, "cell error");
	heap_body2(&pos, LISPSYSTEM_BUFCELL, cell);
	memset(PtrBufCell(pos), '\0', cell);
	*ret = pos;
}

static void get_bufcell(addr pos, size_t m, byte *ret)
{
	byte *data;

	CheckType(pos, LISPSYSTEM_BUFCELL);
	data = PtrBufCell(pos);
	*ret = data[m];
}

static void set_bufcell(addr pos, size_t m, byte c)
{
	byte *data;

	CheckType(pos, LISPSYSTEM_BUFCELL);
	data = PtrBufCell(pos);
	data[m] = c;
}


/*
 *  buffering
 */
struct buffering_struct {
	size_t cell, width, size, index, init_width;
};

#ifdef LISP_DEBUG
#define BUFFERING_CELL		64
#define BUFFERING_WIDTH		4
#else
#define BUFFERING_CELL		4096
#define BUFFERING_WIDTH		8
#endif

#define StructBuffering_Low(x)  ((struct buffering_struct *)PtrBodySS(x))
#define GetRootBuffering_Low(x, y)  GetArraySS((x), 0, (y))
#define SetRootBuffering_Low(x, y)  SetArraySS((x), 0, (y))

static struct buffering_struct *struct_buffering(addr pos)
{
	CheckType(pos, LISPSYSTEM_BUFFERING);
	return StructBuffering_Low(pos);
}

static void getroot_buffering(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_BUFFERING);
	GetRootBuffering_Low(pos, ret);
}

static void setroot_buffering(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_BUFFERING);
	SetRootBuffering_Low(pos, value);
}

int bufferingp(addr pos)
{
	return GetType(pos) == LISPSYSTEM_BUFFERING;
}

static void buffering_clear_root(addr pos)
{
	addr root;
	struct buffering_struct *str;

	str = struct_buffering(pos);
	vector_heap(&root, str->width);
	setroot_buffering(pos, root);
}

void buffering_heap(addr *ret, size_t cell, size_t width)
{
	addr pos;
	struct buffering_struct *str;

	if (cell == 0)
		cell = BUFFERING_CELL;
	if (width == 0)
		width = BUFFERING_WIDTH;
	heap_smallsize(&pos, LISPSYSTEM_BUFFERING, 1, sizeoft(struct buffering_struct));
	str = struct_buffering(pos);
	str->cell = cell;
	str->width = width;
	str->init_width = width;
	str->index = 0;
	str->size = 0;
	buffering_clear_root(pos);
	*ret = pos;
}

void clear_buffering(addr pos)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	str->width = str->init_width;
	str->index = 0;
	str->size = 0;
	buffering_clear_root(pos);
}

int end_buffering(addr pos)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	return str->size <= str->index;
}

void getcell_buffering(addr pos, size_t *ret)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	*ret = str->cell;
}

void getwidth_buffering(addr pos, size_t *ret)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	*ret = str->width;
}

static int realloc_buffering(addr pos, size_t n)
{
	struct buffering_struct *str;
	addr dst, src, value;
	size_t width, x, i;

	str = struct_buffering(pos);
	width = str->width;
	if (n < width)
		return 0;

	/* realloc width */
	while (width <= n) {
		x = width;
		width <<= 1ULL;
		if (width <= x)
			return 1; /* overflow */
	}
	str->width = width;

	/* realloc */
	vector_heap(&dst, width);
	getroot_buffering(pos, &src);
	lenarray(src, &width);
	for (i = 0; i < width; i++) {
		getarray(src, i, &value);
		setarray(dst, i, value);
	}
	setroot_buffering(pos, dst);

	return 0;
}

static void new_buffering(addr root, size_t n, size_t cell, addr *ret)
{
	addr pos;
	bufcell_heap(&pos, cell);
	setarray(root, n, pos);
	*ret = pos;
}

int putc_buffering(addr pos, byte c)
{
	struct buffering_struct *str;
	addr root, page;
	size_t cell, m, n;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	cell = str->cell;
	n = str->index / cell;
	m = str->index % cell;

	/* page */
	if (realloc_buffering(pos, n))
		return 1;
	getroot_buffering(pos, &root);
	getarray(root, n, &page);
	if (page == Nil)
		new_buffering(root, n, cell, &page);

	/* write */
	set_bufcell(page, m, c);
	str->index++;
	if (str->size < str->index)
		str->size = str->index;

	return 0;
}

static int get_buffering(addr pos, size_t index, byte *ret)
{
	struct buffering_struct *str;
	size_t m, n;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	if (str->size <= index)
		return 1; /* EOF */

	n = index / str->cell;
	m = index % str->cell;
	getroot_buffering(pos, &pos);
	getarray(pos, n, &pos);
	if (pos == Nil)
		*ret = 0;
	else
		get_bufcell(pos, m, ret);

	return 0;
}

int getc_buffering(addr pos, byte *ret)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	if (get_buffering(pos, str->index, ret))
		return 1;

	str->index++;
	if (str->size < str->index)
		str->size = str->index;

	return 0;
}

void position_get_buffering(addr pos, size_t *ret)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	*ret = str->index;
}

void position_set_buffering(addr pos, size_t value)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	str->index = value;
}

void position_start_buffering(addr pos)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	str->index = 0;
}

void position_end_buffering(addr pos)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	str->index = str->size;
}

void length_buffering(addr pos, size_t *ret)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	*ret = str->size;
}


/*
 *  make vector
 */
int make_vector_buffering_heap_(addr pos, addr *ret)
{
	byte c;
	struct buffering_struct *str;
	addr array;
	size_t size, i;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	size = str->size;
	Return(array_unsigned8_heap_(&array, size));

	for (i = 0; i < size; i++) {
		if (get_buffering(pos, i, &c))
			return fmte_("end-of-file error.", NULL);
		Return(array_set_unsigned8_(array, i, c));
	}

	return Result(ret, array);
}

int read_buffering_(addr pos, addr vector)
{
	byte c;
	addr value;
	size_t size, i;

	Return(length_sequence_(vector, 1, &size));
	for (i = 0; i < size; i++) {
		Return(getelt_sequence_(NULL, vector, i, &value));
		if (GetByte_integer(value, &c)) {
			return fmte_("The value ~S "
					"must be a (unsigned-byte 8) type.", value, NULL);
		}
		if (putc_buffering(pos, c))
			return fmte_("Too large file size.", NULL);
	}

	return 0;
}


/************************************************************
 *  build.c
 ************************************************************/

#define DEFAULT_MEMORY		(320UL * 1024UL * 1024UL)
#define DEFAULT_STACK		(160UL * 1024UL * 1024UL)

/*
 *  Initialize
 */
void initlisp(void)
{
	clear_pointer();
	init_boole();
	init_call();
	init_clos();
	init_code();
	init_common();
	init_compile();
	init_condition();
	init_control();
	init_copy();
	init_documentation();
	init_eastasian();
	init_encode();
	init_environment();
	init_eval();
	init_extern();
	init_format();
	init_function();
	init_heap();
	init_localtime();
	init_make();
	init_package();
	init_print();
	init_reader();
	init_require();
	init_restart();
	init_rt();
	init_scope();
	init_subtypep();
	init_stream();
	init_structure();
	init_sxhash();
	init_syscall();
	init_terme();
	init_type();
}

static void clearlisp_force(void)
{
	int i;

	for (i = 0; i < LISPINDEX_SIZE; i++)
		lisp_root[i] = 0;
	lisp_nil_object = 0;
	lisp_t_object = 0;
	lisp_gcsync = GcMode_Off;
	Lisp_abort_handler = NULL;
}

int alloclisp(size_t heap, size_t stack)
{
	if (lisp_initialize) {
		Debug("lisp object already allocated.");
		return 1;
	}

	if (heap == 0)
		heap = DEFAULT_MEMORY;
	if (stack == 0)
		stack = DEFAULT_STACK;
	if (heap < 1024UL * 1024UL) {
		Debug("heap size must be greater than 1MByte.");
		return 1;
	}
	if (stack < 1024UL * 1024UL) {
		Debug("stack size must be greater than 1MByte.");
		return 1;
	}
	clearlisp_force();

	/* file */
	if (init_file()) {
		Debug("init_file error.");
		return 1;
	}

	/* heap */
	if (alloc_heap(heap)) {
		Debug("alloc_heap error.");
		goto error_file;
	}

	/* execute */
	if (init_execute(stack)) {
		Debug("init_execute error");
		goto error_heap;
	}

	/* symbol */
	if (init_symbol()) {
		Debug("init_symbol error");
		goto error_execute;
	}

	/* random_state */
	if (init_random_state()) {
		Debug("init_random_state error");
		goto error_object;
	}

	/* check */
	lisp_initialize = 1;
	return 0;

error_object:
	free_symbol();
error_execute:
	free_execute();
error_heap:
	free_heap();
error_file:
	free_file();
	return 1;
}

void freelisp(void)
{
	if (lisp_initialize) {
		free_random_state();
		free_symbol();
		free_execute();
		free_heap();
		free_file();
		clearlisp_force();
	}
	lisp_initialize = 0;
}

int reloadlisp(void)
{
	int i;

	if (reload_execute())
		return 1;
	reload_heap();

	/* variable */
	for (i = 0; i < LISPINDEX_SIZE; i++)
		lisp_root[i] = 0;
	lisp_nil_object = 0;
	lisp_t_object = 0;
	lisp_gcsync = GcMode_Off;

	return 0;
}


/*
 *  buildlisp
 */
void setlisproot(enum LISPINDEX index, addr value)
{
	SetChain(value, 0xFF);
	lisp_root[index] = value;
}

void build_lisproot(Execute ptr)
{
	size_t i;

	nil_heap();
	t_heap();
	for (i = 0; i < LISPINDEX_SIZE; i++)
		lisp_root[i] = Nil;
	lisp_root[LISPINDEX_NIL] = Nil;
	lisp_root[LISPINDEX_T] = T;
	ptr->control = Nil;
}

static void build_finalize_package(constindex index)
{
	addr package;

	GetConstant(index, &package);
	CheckType(package, LISPTYPE_PACKAGE);
	set_readonly_package(package, 1);
}

static void build_finalize(void)
{
	build_finalize_package(CONSTANT_PACKAGE_COMMON_LISP);
	build_finalize_package(CONSTANT_PACKAGE_SYSTEM);
	build_finalize_package(CONSTANT_PACKAGE_CODE);
	build_finalize_package(CONSTANT_PACKAGE_CLOS);
	build_finalize_package(CONSTANT_PACKAGE_RT);
}

static void push_features(const char *name)
{
	addr symbol, keyword, cons;

	GetConst(SPECIAL_FEATURES, &symbol);
	Error(internchar_keyword_(name, &keyword, NULL));
	GetValueSymbol(symbol, &cons);
	Check(find_list_eq_unsafe(keyword, cons), "push error");
	cons_heap(&cons, keyword, cons);
	SetValueSymbol(symbol, cons);
}

static void set_features(void)
{
	addr symbol;

	GetConst(SPECIAL_FEATURES, &symbol);
	SetValueSymbol(symbol, Nil);

	push_features("ANSI-CL");
	push_features("COMMON-LISP");
#ifdef LISP_ANSIC
	push_features("ANSI-C");
	push_features(LISPNAME "-ANSI-C");
#endif
#ifdef LISP_ANSIC_WINDOWS
	push_features("ANSI-C-WINDOWS");
	push_features(LISPNAME "-ANSI-C-WINDOWS");
#endif
#ifdef LISP_FREEBSD
	push_features("UNIX");
	push_features("FREEBSD");
#endif
#ifdef LISP_LINUX
	push_features("UNIX");
	push_features("LINUX");
#endif
#ifdef LISP_WINDOWS
	push_features("WINDOWS");
	push_features("WIN32");
#endif

#ifdef LISP_ARCH_64BIT
	push_features("CPU-64");
#endif
#ifdef LISP_ARCH_32BIT
	push_features("CPU-32");
#endif
#ifdef LISP_64BIT
	push_features("FIXNUM-64");
#endif
#ifdef LISP_32BIT
	push_features("FIXNUM-32");
#endif

	push_features(LISPNAME);
#ifdef LISP_DEBUG
	push_features(LISPNAME "-DEBUG");
#endif
#ifdef LISP_DEGRADE
	push_features(LISPNAME "-DEGRADE");
#endif
#ifdef LISP_COMPLEX_INACCURACY
	push_features("MATH-INACCURACY");
#endif
#ifdef __cplusplus
	push_features("CPLUSPLUS");
#else
	push_features("C99");
#endif

#ifdef LISP_FLOAT_LONG_64
	push_features("LONG-FLOAT-64");
#endif
#ifdef LISP_FLOAT_LONG_80
	push_features("LONG-FLOAT-80");
#endif
#ifdef LISP_FLOAT_LONG_128
	push_features("LONG-FLOAT-128");
#endif

#ifdef LISP_DEBUG
#ifdef LISP_DEBUG_FORCE_GC
	push_features("FORCE-GC");
#endif
#endif

#if 0
#ifdef LISP_THREAD_ENABLE
	push_features("THREAD");
#endif
#endif
}

static void set_pretty_printing(void)
{
	addr symbol;
	GetConst(SPECIAL_PRINT_PRETTY, &symbol);
	SetValueSymbol(symbol, T);
}

void buildlisp(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_function();
	build_character();
	build_real();
	build_package();
	build_stream();
	build_symbol();
	build_clos(ptr);
	build_condition(ptr);
	build_type();
	build_syscall();
	build_terme();
	build_common();
	build_print(ptr);
	build_environment(ptr);
	build_documentation(ptr);
	build_reader();
	build_pathname();
	build_declare();
	build_code();
	build_require();
	build_rt();
	build_finalize();
	set_features();
	set_pretty_printing();
	gcexec(GcMode_Full);
}


/*
 *  core
 */
int save_lisp(filestream fm)
{
	int i;

	/* heap */
	if (save_heap(fm)) {
		Debug("save_heap error.");
		return 1;
	}

	/* build.c */
	for (i = 0; i < LISPINDEX_SIZE; i++) {
		if (writeaddr_filememory(fm, lisp_root[i])) {
			Debug2("writeaddr error: lisp_root[%d].", i);
			return 1;
		}
	}

	return 0;
}

int load_lisp(filestream fm)
{
	int i;

	/* heap */
	if (load_heap(fm)) {
		Debug("load_heap error.");
		return 1;
	}

	/* build.c */
	for (i = 0; i < LISPINDEX_SIZE; i++) {
		if (readaddr_filememory(fm, &(lisp_root[i]))) {
			Debug2("readaddr error: lisp_root[%d].", i);
			return 1;
		}
	}
	lisp_nil_object = lisp_root[LISPINDEX_NIL];
	lisp_t_object = lisp_root[LISPINDEX_T];

	return 0;
}


/************************************************************
 *  bytespec.c
 ************************************************************/

void bytespec_alloc(LocalRoot local, addr *ret, size_t size, size_t posi)
{
	addr pos;
	struct bytespec_struct *ptr;

	alloc_body2(local, &pos, LISPTYPE_BYTESPEC, sizeoft(struct bytespec_struct));
	ptr = ByteSpecStruct(pos);
	ptr->size = size;
	ptr->position = posi;
	*ret = pos;
}

void bytespec_local(LocalRoot local, addr *ret, size_t size, size_t posi)
{
	CheckLocal(local);
	bytespec_alloc(local, ret, size, posi);
}

void bytespec_heap(addr *ret, size_t size, size_t posi)
{
	bytespec_alloc(NULL, ret, size, posi);
}


/*
 *  byte
 */
int byte_common_(addr size, addr posi, addr *ret)
{
	size_t ssize, spos;

	if (GetIndex_integer(size, &ssize))
		return fmte_("Byte spec SIZE ~S is too large.", size, NULL);
	if (GetIndex_integer(posi, &spos))
		return fmte_("Byte spec POSITION ~S is too large.", posi, NULL);
	bytespec_heap(ret, ssize, spos);

	return 0;
}

void byte_size_common(addr pos, addr *ret)
{
	struct bytespec_struct *ptr;

	CheckType(pos, LISPTYPE_BYTESPEC);
	ptr = ByteSpecStruct(pos);
	make_index_integer_heap(ret, ptr->size);
}

void byte_position_common(addr pos, addr *ret)
{
	struct bytespec_struct *ptr;

	CheckType(pos, LISPTYPE_BYTESPEC);
	ptr = ByteSpecStruct(pos);
	make_index_integer_heap(ret, ptr->position);
}


/*
 *  bytespec-mask
 */
void bytespec_mask_init(struct bytespec_mask *ptr, addr pos)
{
	struct bytespec_struct *spec;
	size_t size, posi;

	CheckType(pos, LISPTYPE_BYTESPEC);
	spec = ByteSpecStruct(pos);
	size = spec->size;
	posi = spec->position;
	ptr->size = size;
	ptr->position = posi;
	ptr->posend = size + posi;
	ptr->index = 0;
	ptr->start = posi / BIGNUM_FULLBIT;
	ptr->end = ptr->posend / BIGNUM_FULLBIT;
}

static fixed bytespec_mask_start(size_t pos)
{
	size_t index;

	index = pos % BIGNUM_FULLBIT;
	if (index == BIGNUM_FULLBIT - 1ULL)
		return ((fixed)1ULL) << (BIGNUM_FULLBIT - 1ULL);
	else
		return ~((1ULL << index) - 1ULL);
}

static fixed bytespec_mask_end(size_t pos)
{
	return ~(bytespec_mask_start(pos));
}

fixed bytespec_mask_get(struct bytespec_mask *ptr)
{
	fixed v1, v2;
	size_t index, start, end, size, pos;

	index = ptr->index++;
	start = ptr->start;
	end = ptr->end;
	size = ptr->size;
	pos = ptr->position;
	if (size == 0)
		return 0;
	/* start */
	if (index < start)
		v1 = 0;
	else if (start < index)
		v1 = BIGNUM_FULL;
	else
		v1 =  bytespec_mask_start(pos);
	/* end */
	if (index < end)
		v2 = BIGNUM_FULL;
	else if (end < index)
		v2 = 0;
	else
		v2 = bytespec_mask_end(pos + size);
	/* result */
	return v1 & v2;
}

size_t bytespec_mask_getsize(struct bytespec_mask *ptr)
{
	size_t bit = ptr->position + ptr->size;
	size_t size = bit / BIGNUM_FULLBIT;
	return (bit % BIGNUM_FULLBIT)? size+1ULL: size;
}


/************************************************************
 *  c99.c
 ************************************************************/

const char *setlocale_c(int category)
{
	const char *ptr;

	ptr = setlocale(category, NULL);
	if (ptr && (ptr[0] != 'C' || ptr[1] != 0)) {
		return setlocale(category, "C");
	}

	return NULL;
}

int vsnprintc(char *buffer, size_t size, const char *fmt, va_list args)
{
	int result;
	const char *check;

	/* setlocale C */
	check = setlocale_c(LC_NUMERIC);

	/* printf */
	result = vsnprintf(buffer, size, fmt, args);
	if (result <= 0)
		buffer[size - 1] = 0;

	/* setlocale */
	if (check)
		setlocale(LC_NUMERIC, check);

	return result;
}

int snprintc(char *buffer, size_t size, const char *fmt, ...)
{
	int result;
	va_list args;

	va_start(args, fmt);
	result = vsnprintc(buffer, size, fmt, args);
	va_end(args);

	return result;
}

int sscanc(const char *buffer, const char *fmt, ...)
{
	int result;
	const char *check;
	va_list args;

	/* setlocale C */
	check = setlocale_c(LC_NUMERIC);

	/* sscanf */
	va_start(args, fmt);
#ifdef _MSC_VER
	result = vsscanf_s(buffer, fmt, args);
#else
	result = vsscanf(buffer, fmt, args);
#endif
	va_end(args);

	/* setlocale */
	if (check)
		setlocale(LC_NUMERIC, check);

	return result;
}


/************************************************************
 *  call.c
 ************************************************************/

void init_call(void)
{
	init_call_printer();
}


/************************************************************
 *  call_arrays.c
 ************************************************************/

/*
 *  make-array
 */
int make_array_common_(Execute ptr, addr var, addr rest, addr *ret)
{
	addr type, ielem, icont, adj, fill, dto, off;

	if (GetKeyArgs(rest, KEYWORD_ELEMENT_TYPE, &type))
		type = T;
	if (GetKeyArgs(rest, KEYWORD_INITIAL_ELEMENT, &ielem))
		ielem = Unbound;
	if (GetKeyArgs(rest, KEYWORD_INITIAL_CONTENTS, &icont))
		icont = Unbound;
	if (GetKeyArgs(rest, KEYWORD_ADJUSTABLE, &adj))
		adj = Nil;
	if (GetKeyArgs(rest, KEYWORD_FILL_POINTER, &fill))
		fill = Nil;
	if (GetKeyArgs(rest, KEYWORD_DISPLACED_TO, &dto))
		dto = Nil;
	if (GetKeyArgs(rest, KEYWORD_DISPLACED_INDEX_OFFSET, &off))
		fixnum_heap(&off, 0);
	Return(parse_type_(ptr, &type, type, Nil));
	return array_make_array_(ret, var, type, ielem, icont, adj, fill, dto, off);
}


/*
 *  adjust-array
 */
int adjust_array_common_(Execute ptr, addr pos, addr dim, addr rest, addr *ret)
{
	addr type, ielem, icont, fill, dto, off;

	if (GetKeyArgs(rest, KEYWORD_ELEMENT_TYPE, &type))
		type = Unbound;
	if (GetKeyArgs(rest, KEYWORD_INITIAL_ELEMENT, &ielem))
		ielem = Unbound;
	if (GetKeyArgs(rest, KEYWORD_INITIAL_CONTENTS, &icont))
		icont = Unbound;
	if (GetKeyArgs(rest, KEYWORD_FILL_POINTER, &fill))
		fill = Nil;
	if (GetKeyArgs(rest, KEYWORD_DISPLACED_TO, &dto))
		dto = Nil;
	if (GetKeyArgs(rest, KEYWORD_DISPLACED_INDEX_OFFSET, &off))
		fixnum_heap(&off, 0);
	if (type != Unbound) {
		Return(parse_type_(ptr, &type, type, Nil));
	}

	return array_adjust_array_(ret, pos, dim, type, ielem, icont, fill, dto, off);
}


/*
 *  adjustable-array-p
 */
int adjustable_array_p_common_(addr var, int *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return Result(ret, array_adjustable_p(var));

		case LISPTYPE_STRING:
		case LISPTYPE_VECTOR:
		case LISPTYPE_BITVECTOR:
			return Result(ret, 0);

		default:
			*ret = 0;
			return TypeError_(var, ARRAY);
	}
}


/*
 *  aref
 */
int aref_common_(addr var, addr rest, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_aref_(NULL, var, rest, ret);

		case LISPTYPE_STRING:
			return strvect_aref_(NULL, var, rest, ret);

		case LISPTYPE_VECTOR:
			return vector_aref_(var, rest, ret);

		case LISPTYPE_BITVECTOR:
			return bitmemory_aref_(NULL, var, rest, ret);

		default:
			*ret = Nil;
			return TypeError_(var, ARRAY);
	}
}


/*
 *  setf-aref
 */
int setf_aref_common_(addr value, addr var, addr rest)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_setf_aref_(var, rest, value);

		case LISPTYPE_STRING:
			return strvect_setf_aref_(var, rest, value);

		case LISPTYPE_VECTOR:
			return vector_setf_aref_(var, rest, value);

		case LISPTYPE_BITVECTOR:
			return bitmemory_setf_aref_(var, rest, value);

		default:
			return TypeError_(var, ARRAY);
	}
}


/*
 *  array_dimension
 */
static int array_array_dimension_common_(addr array, addr axis, addr *ret)
{
	int check;
	struct array_struct *str;
	size_t index, dimension;

	CheckType(array, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(array);
	dimension = str->dimension;
	if (dimension == 0)
		return fmte_("The array have no dimension.", NULL);

	if (GetIndex_integer(axis, &index)) {
		Return(minusp_integer_(axis, &check));
		if (check)
			return fmte_("Index ~A must be a non-negative integer.", axis, NULL);
		else
			return fmte_("Index ~A is too large.", axis, NULL);
	}

	if (dimension <= index) {
		return fmte_("The dimension index ~A must be less than "
				"the array-dimension limit ~A.", axis, intsizeh(dimension), NULL);
	}
	index = (array_ptrsize(array))[index];
	make_index_integer_heap(ret, index);
	return 0;
}

int array_dimension_common_(addr var, addr axis, addr *ret)
{
	size_t size;

	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_array_dimension_common_(var, axis, ret);

		case LISPTYPE_VECTOR:
			lenarray(var, &size);
			return vector_array_dimension_(var, axis, size, ret);

		case LISPTYPE_STRING:
			strvect_length(var, &size);
			return vector_array_dimension_(var, axis, size, ret);

		case LISPTYPE_BITVECTOR:
			bitmemory_length(var, &size);
			return vector_array_dimension_(var, axis, size, ret);

		default:
			*ret = Nil;
			return TypeError_(var, ARRAY);
	}
}


/*
 *  array-dimensions
 */
static void array_array_dimensions_common(addr array, addr *ret)
{
	struct array_struct *str;
	const size_t *data;
	size_t size, i, n;
	addr root, pos;

	CheckType(array, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(array);
	size = str->dimension;
	data = array_ptrsize(array);
	root = Nil;
	for (i = 0; i < size; i++) {
		n = size - i - 1;
		make_index_integer_heap(&pos, data[n]);
		cons_heap(&root, pos, root);
	}
	*ret = root;
}

int array_dimensions_common_(addr var, addr *ret)
{
	size_t size;

	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			array_array_dimensions_common(var, ret);
			break;

		case LISPTYPE_VECTOR:
			lenarray(var, &size);
			vector_array_dimensions(size, ret);
			break;

		case LISPTYPE_STRING:
			strvect_length(var, &size);
			vector_array_dimensions(size, ret);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_length(var, &size);
			vector_array_dimensions(size, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(var, ARRAY);
	}

	return 0;
}


/*
 *  array-element-type
 */
int array_element_type_common_(addr var, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_get_element_type_(var, ret);

		case LISPTYPE_VECTOR:
			*ret = T;
			break;

		case LISPTYPE_STRING:
			GetConst(COMMON_CHARACTER, ret);
			break;

		case LISPTYPE_BITVECTOR:
			GetConst(COMMON_BIT, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(var, ARRAY);
	}

	return 0;
}


/*
 *  array-has-fill-pointer-p
 */
int array_has_fill_pointer_p_common_(addr var, int *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return Result(ret, array_fillpointer_p(var));

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_BITVECTOR:
			return Result(ret, 0);

		default:
			*ret = 0;
			return TypeError_(var, ARRAY);
	}
}


/*
 *  array-displacement
 */
static void array_array_displacement_common(addr array, addr *displaced, addr *offset)
{
	struct array_struct *str;

	CheckType(array, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(array);
	if (str->displaced) {
		GetArrayInfo(array, ARRAY_INDEX_DISPLACED, displaced);
		make_index_integer_heap(offset, str->offset);
	}
	else {
		*displaced = Nil;
		fixnum_heap(offset, 0);
	}
}

int array_displacement_common_(addr pos, addr *ret, addr *offset)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			array_array_displacement_common(pos, ret, offset);
			break;

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_BITVECTOR:
			*ret = Nil;
			fixnum_heap(offset, 0);
			break;

		default:
			*ret = *offset = Nil;
			return TypeError_(pos, ARRAY);
	}

	return 0;
}


/*
 *  array-in-bounds-p
 */
static int array_array_in_bounds_p_common_(addr array, addr rest, int *ret)
{
	int value;
	struct array_struct *str;
	addr list, pos;
	size_t size, i, check;
	const size_t *data;

	str = ArrayInfoStruct(array);
	size = str->dimension;
	data = array_ptrsize(array);
	value = 1;
	list = rest;
	for (i = 0; i < size; i++) {
		if (list == Nil)
			return fmte_("The subscripts ~S is too few arguments.", rest, NULL);
		if (! consp_getcons(list, &pos, &list))
			return fmte_("Invalid subscripts arguments ~S.", rest, NULL);
		if (! integerp(pos))
			return fmte_("The subscript ~S must be integer type.", pos, NULL);
		if (GetIndex_integer(pos, &check)) {
			/* minus or large value */
			value = 0;
			continue;
		}
		if (data[i] <= check) {
			/* out of range */
			value = 0;
			continue;
		}
	}
	if (list != Nil)
		return fmte_("The subscripts ~S is too many arguments.", rest, NULL);

	return Result(ret, value);
}

int array_in_bounds_p_common_(addr array, addr rest, int *ret)
{
	size_t size;

	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			*ret = 0;
			return array_array_in_bounds_p_common_(array, rest, ret);

		case LISPTYPE_VECTOR:
			lenarray(array, &size);
			return vector_array_in_bounds_p_(rest, size, ret);

		case LISPTYPE_STRING:
			strvect_length(array, &size);
			return vector_array_in_bounds_p_(rest, size, ret);

		case LISPTYPE_BITVECTOR:
			bitmemory_length(array, &size);
			return vector_array_in_bounds_p_(rest, size, ret);

		default:
			*ret = 0;
			return TypeError_(array, ARRAY);
	}
}


/*
 *  array-rank
 */
int array_rank_common_(addr pos, addr *ret)
{
	size_t size;

	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			size = array_dimension_size(pos);
			make_index_integer_heap(ret, size);
			break;

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_BITVECTOR:
			fixnum_heap(ret, 1);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, ARRAY);
	}

	return 0;
}


/*
 *  array-row-major-index
 */
static int array_array_row_major_index_common_(addr array, addr rest, addr *ret)
{
	size_t size;

	Return(array_arefindex_(array, rest, &size));
	make_index_integer_heap(ret, size);

	return 0;
}

int array_row_major_index_common_(addr array, addr rest, addr *ret)
{
	size_t size;

	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			return array_array_row_major_index_common_(array, rest, ret);

		case LISPTYPE_VECTOR:
			lenarray(array, &size);
			return vector_array_row_major_index_(rest, size, ret);

		case LISPTYPE_STRING:
			strvect_length(array, &size);
			return vector_array_row_major_index_(rest, size, ret);

		case LISPTYPE_BITVECTOR:
			bitmemory_length(array, &size);
			return vector_array_row_major_index_(rest, size, ret);

		default:
			*ret = Nil;
			return TypeError_(array, ARRAY);
	}
}


/*
 *  array-total-size
 */
int array_total_size_common_(addr array, addr *ret)
{
	size_t size;

	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			size = array_total_size(array);
			break;

		case LISPTYPE_VECTOR:
			lenarray(array, &size);
			break;

		case LISPTYPE_STRING:
			strvect_length(array, &size);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_length(array, &size);
			break;

		default:
			*ret = Nil;
			return TypeError_(array, ARRAY);
	}
	make_index_integer_heap(ret, size);
	return 0;
}


/*
 *  arrayp
 */
int arrayp_common(addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_BITVECTOR:
			return 1;

		default:
			return 0;
	}
}


/*
 *  fill-pointer
 */
int fill_pointer_common_(Execute ptr, addr array, addr *ret)
{
	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			if (array_fill_pointer(array, ret))
				return call_type_error_fill_pointer_(ptr, array);
			return 0;

		case LISPTYPE_STRING:
		case LISPTYPE_VECTOR:
		case LISPTYPE_BITVECTOR:
			*ret = Nil;
			return call_type_error_fill_pointer_(ptr, array);

		default:
			*ret = Nil;
			return TypeError_(array, VECTOR);
	}
}

int setf_fill_pointer_common_(Execute ptr, addr value, addr array)
{
	int check;

	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			Return(array_setf_fill_pointer_(array, value, &check));
			if (check)
				return call_type_error_fill_pointer_(ptr, array);
			return 0;

		case LISPTYPE_STRING:
		case LISPTYPE_VECTOR:
		case LISPTYPE_BITVECTOR:
			return call_type_error_fill_pointer_(ptr, array);

		default:
			return TypeError_(array, VECTOR);
	}
}


/*
 *  row-major-aref
 */
int row_major_aref_common_(addr array, addr index, addr *ret)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return fmte_("Index ~A is too large.", index, NULL);
	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			return array_get_(NULL, array, size, ret);

		case LISPTYPE_VECTOR:
			return vector_get_(array, size, ret);

		case LISPTYPE_STRING:
			return strvect_get_(NULL, array, size, ret);

		case LISPTYPE_BITVECTOR:
			return bitmemory_get_(NULL, array, size, ret);

		default:
			*ret = Nil;
			return TypeError_(array, ARRAY);
	}
}

int setf_row_major_aref_common_(addr value, addr array, addr index)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return fmte_("Index ~A is too large.", index, NULL);
	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			return array_set_(array, size, value);

		case LISPTYPE_VECTOR:
			return vector_set_(array, size, value);

		case LISPTYPE_STRING:
			return strvect_set_(array, size, value);

		case LISPTYPE_BITVECTOR:
			return bitmemory_set_(array, size, value);

		default:
			return TypeError_(array, ARRAY);
	}
}


/*
 *  simple-vector-p
 */
int simple_vector_p_common(addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_simple_vector_p(var);

		case LISPTYPE_VECTOR:
			return 1;

		default:
			return 0;
	}
}


/*
 *  svref
 */
int svref_common_(addr pos, addr index, addr *ret)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return fmte_("Index ~A is too large.", index, NULL);
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return array_get_(NULL, pos, size, ret);

		case LISPTYPE_VECTOR:
			return vector_get_(pos, size, ret);

		default:
			*ret = Nil;
			return TypeError_(pos, SIMPLE_VECTOR);
	}
}

int setf_svref_common_(addr value, addr pos, addr index)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return fmte_("Index ~A is too large.", index, NULL);
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return array_set_(pos, size, value);

		case LISPTYPE_VECTOR:
			return vector_set_(pos, size, value);

		default:
			return TypeError_(pos, SIMPLE_VECTOR);
	}
}


/*
 *  vectorp
 */
int vectorp_common(addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_vector_p(var);

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_BITVECTOR:
			return 1;

		default:
			return 0;
	}
}


/*
 *  bit
 */
int bit_common_(addr pos, addr rest, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_BITVECTOR:
			return bitmemory_aref_(NULL, pos, rest, ret);

		case LISPTYPE_ARRAY:
			return array_aref_bit_(NULL, pos, rest, ret);

		default:
			*ret = Nil;
			return TypeError_(pos, ARRAY);
	}
}

int setf_bit_common_(addr value, addr pos, addr rest)
{
	switch (GetType(pos)) {
		case LISPTYPE_BITVECTOR:
			return bitmemory_setf_aref_(pos, rest, value);

		case LISPTYPE_ARRAY:
			return array_setf_aref_bit_(pos, rest, value);

		default:
			return TypeError_(pos, ARRAY);
	}
}


/*
 *  bit-vector-p
 */
int bit_vector_p_common(addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_bvarrayp(var);

		case LISPTYPE_BITVECTOR:
			return 1;

		default:
			return 0;
	}
}


/*
 *  simple-bit-vector-p
 */
int simple_bit_vector_p_common(addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return simple_array_bvarrayp(var);

		case LISPTYPE_BITVECTOR:
			return 1;

		default:
			return 0;
	}
}


/*
 *  bit-and
 */
static fixed bitcalc_and_common(fixed a, fixed b)
{
	return a & b;
}

int bit_and_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_and_common);
}


/*
 *  bit-andc1
 */
static fixed bitcalc_andc1_common(fixed a, fixed b)
{
	return (~a) & b;
}

int bit_andc1_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_andc1_common);
}


/*
 *  bit-andc2
 */
static fixed bitcalc_andc2_common(fixed a, fixed b)
{
	return a & (~b);
}

int bit_andc2_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_andc2_common);
}


/*
 *  bit-eqv
 */
static fixed bitcalc_eqv_common(fixed a, fixed b)
{
	return ~(a ^ b);
}

int bit_eqv_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_eqv_common);
}


/*
 *  bit-ior
 */
static fixed bitcalc_ior_common(fixed a, fixed b)
{
	return a | b;
}

int bit_ior_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_ior_common);
}


/*
 *  bit-nand
 */
static fixed bitcalc_nand_common(fixed a, fixed b)
{
	return ~(a & b);
}

int bit_nand_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_nand_common);
}


/*
 *  bit-nor
 */
static fixed bitcalc_nor_common(fixed a, fixed b)
{
	return ~(a | b);
}

int bit_nor_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_nor_common);
}


/*
 *  bit-orc1
 */
static fixed bitcalc_orc1_common(fixed a, fixed b)
{
	return (~a) | b;
}

int bit_orc1_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_orc1_common);
}


/*
 *  bit-orc2
 */
static fixed bitcalc_orc2_common(fixed a, fixed b)
{
	return a | (~b);
}

int bit_orc2_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_orc2_common);
}


/*
 *  bit-xor
 */
static fixed bitcalc_xor_common(fixed a, fixed b)
{
	return a ^ b;
}

int bit_xor_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_xor_common);
}


/*
 *  bit-not
 */
int bit_not_common_(addr x, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitnot_(ret, x, opt);
}


/************************************************************
 *  call_characters.c
 ************************************************************/

/*
 *  char=
 */
static int char_eql_check_common_(
		constindex name, addr list, addr *ret, int (*call)(unicode, unicode))
{
	addr var, pos;
	unicode a, b;

	if (list == Nil) {
		*ret = Nil;
		GetConstant(name, &pos);
		return call_simple_program_error_va_(NULL,
				"Too few arguemnts ~A.", pos, NULL);
	}
	GetCons(list, &var, &list);

	GetCharacter(var, &a);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		GetCharacter(pos, &b);
		if (! (call(a, b)))
			return Result(ret, Nil);
		a = b;
	}
	return Result(ret, T);
}
#define CharEqlCheckCommon_(name, list, ret, call) \
	char_eql_check_common_(CONSTANT_COMMON_##name, list, ret, call)

static int call_char_eql(unicode a, unicode b)
{
	return a == b;
}

int char_eql_common_(addr list, addr *ret)
{
	return CharEqlCheckCommon_(CHAR_EQL, list, ret, call_char_eql);
}


/*
 *  char/=
 */
int char_not_eql_common_(addr list, addr *ret)
{
	addr left, right, loop;
	unicode a, b;

	if (list == Nil) {
		*ret = Nil;
		GetConst(COMMON_CHAR_NOT_EQL, &left);
		return call_simple_program_error_va_(NULL,
				"Too few arguemnts ~A.", left, NULL);
	}

	for (;;) {
		Return_getcons(list, &left, &list);
		if (list == Nil)
			break;
		GetCharacter(left, &a);
		for (loop = list; loop != Nil; ) {
			Return_getcons(loop, &right, &loop);
			GetCharacter(right, &b);
			if (a == b)
				return Result(ret, Nil);
		}
	}
	return Result(ret, T);
}


/*
 *  char<
 */
static int call_char_less(unicode a, unicode b)
{
	return a < b;
}

int char_less_common_(addr list, addr *ret)
{
	return CharEqlCheckCommon_(CHAR_LESS, list, ret, call_char_less);
}


/*
 *  char>
 */
static int call_char_greater(unicode a, unicode b)
{
	return a > b;
}

int char_greater_common_(addr list, addr *ret)
{
	return CharEqlCheckCommon_(CHAR_GREATER, list, ret, call_char_greater);
}


/*
 *  char<=
 */
static int call_char_less_equal(unicode a, unicode b)
{
	return a <= b;
}

int char_less_equal_common_(addr list, addr *ret)
{
	return CharEqlCheckCommon_(CHAR_LESS_EQUAL, list, ret, call_char_less_equal);
}


/*
 *  char>=
 */
static int call_char_greater_equal(unicode a, unicode b)
{
	return a >= b;
}

int char_greater_equal_common_(addr list, addr *ret)
{
	return CharEqlCheckCommon_(CHAR_GREATER_EQUAL, list, ret, call_char_greater_equal);
}


/*
 *  char-equal
 */
static int char_equal_check_common_(
		constindex name, addr list, addr *ret, int (*call)(unicode, unicode))
{
	addr var, pos;
	unicode a, b;

	if (list == Nil) {
		*ret = Nil;
		GetConstant(name, &pos);
		return call_simple_program_error_va_(NULL,
				"Too few arguemnts ~A.", pos, NULL);
	}
	GetCons(list, &var, &list);

	GetCharacter(var, &a);
	a = toUpperUnicode(a);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		GetCharacter(pos, &b);
		b = toUpperUnicode(b);
		if (! (call(a, b)))
			return Result(ret, Nil);
		a = b;
	}
	return Result(ret, T);
}
#define CharEqualCheckCommon_(name, list, ret, call) \
	char_equal_check_common_(CONSTANT_COMMON_##name, list, ret, call)

int char_equal_common_(addr list, addr *ret)
{
	return CharEqualCheckCommon_(CHAR_EQUAL, list, ret, call_char_eql);
}


/*
 *  char-not-equal
 */
int char_not_equal_common_(addr list, addr *ret)
{
	addr left, right, loop;
	unicode a, b;

	if (list == Nil) {
		*ret = Nil;
		GetConst(COMMON_CHAR_NOT_EQUAL, &left);
		return call_simple_program_error_va_(NULL,
				"Too few arguemnts ~A.", left, NULL);
	}
	for (;;) {
		Return_getcons(list, &left, &list);
		if (list == Nil)
			break;
		GetCharacter(left, &a);
		a = toUpperUnicode(a);
		for (loop = list; loop != Nil; ) {
			Return_getcons(loop, &right, &loop);
			GetCharacter(right, &b);
			b = toUpperUnicode(b);
			if (a == b)
				return Result(ret, Nil);
		}
	}
	return Result(ret, T);
}


/*
 *  char-lessp
 */
int char_lessp_common_(addr list, addr *ret)
{
	return CharEqualCheckCommon_(CHAR_LESSP, list, ret, call_char_less);
}


/*
 *  char-greaterp
 */
int char_greaterp_common_(addr list, addr *ret)
{
	return CharEqualCheckCommon_(CHAR_GREATERP, list, ret, call_char_greater);
}


/*
 *  char-not-lessp
 */
int char_not_lessp_common_(addr list, addr *ret)
{
	return CharEqualCheckCommon_(CHAR_NOT_LESSP, list, ret, call_char_greater_equal);
}


/*
 *  char-not-greaterp
 */
int char_not_greaterp_common_(addr list, addr *ret)
{
	return CharEqualCheckCommon_(CHAR_NOT_GREATERP, list, ret, call_char_less_equal);
}


/*
 *  character
 */
static int character_common_error_(addr var)
{
	addr pos, size;

	GetConst(COMMON_STRING, &pos);
	fixnum_heap(&size, 1);
	list_heap(&pos, pos, size, NULL);
	return call_type_error_va_(Execute_Thread, var, pos,
			"The length of symbol ~S name must be 1.", var, NULL);
}

int character_common_(addr var, addr *ret)
{
	unicode c;
	size_t size;

	if (symbolp(var)) {
		/* string check */
		GetNameSymbol(var, &var);
		string_length(var, &size);
		if (size != 1) {
			*ret = Nil;
			return character_common_error_(var);
		}
		Return(string_getc_(var, 0, &c));
		character_heap(&var, c);
	}
	else if (stringp(var)) {
		string_length(var, &size);
		if (size != 1) {
			*ret = Nil;
			return character_common_error_(var);
		}
		Return(string_getc_(var, 0, &c));
		character_heap(&var, c);
	}
	else if (GetType(var) != LISPTYPE_CHARACTER) {
		return TypeError_(var, CHARACTER);
	}

	return Result(ret, var);
}


/*
 *  alpha-char-p
 */
void alpha_char_p_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = isAlphabetic(c)? T: Nil;
}


/*
 *  alphanumericp
 */
void alphanumericp_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = isAlphanumeric(c)? T: Nil;
}


/*
 *  digit-char
 */
void digit_char_common(addr var, addr opt, addr *ret)
{
	fixnum w, r;

	/* radix */
	if (opt == Unbound)
		r = 10;
	else
		GetFixnum(opt, &r);

	/* digit */
	GetFixnum(var, &w);
	if (0 <= w && w < r) {
		character_heap(&var, (unicode)(w < 10? ('0' + w): (w - 10 + 'A')));
		*ret = var;
	}
	else {
		*ret = Nil;
	}
}


/*
 *  digit-char-p
 */
void digit_char_p_common(addr var, addr opt, addr *ret)
{
	fixnum r, w;
	unicode c;

	/* radix */
	if (opt == Unbound)
		r = 10;
	else
		GetFixnum(opt, &r);
	/* character */
	GetCharacter(var, &c);
	if (isDigitCase(c))
		w = (fixnum)(c - '0');
	else if (isLowerCase(c))
		w = (fixnum)(c - 'a' + 10);
	else if (isUpperCase(c))
		w = (fixnum)(c - 'A' + 10);
	else {
		*ret = Nil;
		return;
	}
	if (r <= w) {
		*ret = Nil;
		return;
	}
	else {
		fixnum_heap(ret, w);
		return;
	}
}


/*
 *  graphic-char-p
 */
void graphic_char_p_common(addr var, addr *ret)
{
	unicode c;

	GetCharacter(var, &c);
	if (c < 0x80) {
		*ret = (c == ' ' || isgraphunicode(c))? T: Nil;
	}
	else {
		*ret = T;
	}
}


/*
 *  standard-char-p
 */
void standard_char_p_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = isStandardType(c)? T: Nil;
}


/*
 *  char-upcase
 */
void char_upcase_common(addr var, addr *ret)
{
	unicode c;

	GetCharacter(var, &c);
	if (isLowerCase(c))
		character_heap(&var, (c - 'a' + 'A'));
	*ret = var;
}


/*
 *  char-downcase
 */
void char_downcase_common(addr var, addr *ret)
{
	unicode c;

	GetCharacter(var, &c);
	if (isUpperCase(c))
		character_heap(&var, (c - 'A' + 'a'));
	*ret = var;
}


/*
 *  upper-case-p
 */
void upper_case_p_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = isUpperCase(c)? T: Nil;
}


/*
 *  lower-case-p
 */
void lower_case_p_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = isLowerCase(c)? T: Nil;
}


/*
 *  both-case-p
 */
void both_case_p_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = (isUpperCase(c) || isLowerCase(c))? T: Nil;
}


/*
 *  char-code
 */
void char_code_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	fixnum_heap(ret, (fixnum)c);
}


/*
 *  code-char
 */
void code_char_common(addr var, addr *ret)
{
	fixnum v;
	unicode c;

	GetFixnum(var, &v);
	c = (unicode)v;
	if (isBaseType(c))
		character_heap(ret, c);
	else
		*ret = Nil;
}


/*
 *  char-name
 */
int char_name_common_(addr var, addr *ret)
{
	return findtable_char_name_(ret, var);
}


/*
 *  name-char
 */
int name_char_common_(LocalRoot local, addr var, addr *ret)
{
	LocalStack stack;
	unicode c;

	if (GetType(var) == LISPTYPE_CHARACTER) {
		/* character */
		push_local(local, &stack);
		GetCharacter(var, &c);
		strvect_local(local, &var, 1);
		Return(strvect_setc_(var, 0, c));
		Return(findtable_name_char_(ret, var));
		rollback_local(local, stack);
	}
	else {
		/* symbol, string */
		if (symbolp(var))
			GetNameSymbol(var, &var);
		Return(findtable_name_char_(ret, var));
	}

	return 0;
}


/************************************************************
 *  call_conditions.c
 ************************************************************/

/*
 *  assert
 */
static int assert_retry_common_(addr test, addr *ret)
{
	/* (tagbody
	 *   loop
	 *   (restart-bind
	 *     ((continue (lambda () (go loop))
	 *        :report-function
	 *        (lambda (s)
	 *          (princ "Retry assersion." s))))
	 *     (unless test
	 *       (error "Failed assersion ~A." 'test))))
	 */
	addr tagbody, restart, cont, lambda, go, report;
	addr princ, unless, quote, error;
	addr loop, s, str1, str2, a;

	/* variable */
	make_symbolchar(&loop, "LOOP");
	make_symbolchar(&s, "STREAM");
	strvect_char_heap(&str1, "Retry assersion.");
	strvect_char_heap(&str2, "Failed assersion ~A.");
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_RESTART_BIND, &restart);
	GetConst(COMMON_CONTINUE, &cont);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_GO, &go);
	GetConst(KEYWORD_REPORT_FUNCTION, &report);
	GetConst(COMMON_PRINC, &princ);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_ERROR, &error);
	GetConst(COMMON_QUOTE, &quote);
	/* expand */
	list_heap(&a, quote, test, NULL);
	list_heap(&error, error, str2, a, NULL);
	list_heap(&unless, unless, test, error, NULL);
	list_heap(&princ, princ, str1, s, NULL);
	list_heap(&s, s, NULL);
	list_heap(&princ, lambda, s, princ, NULL);
	list_heap(&go, go, loop, NULL);
	list_heap(&lambda, lambda, Nil, go, NULL);
	list_heap(&cont, cont, lambda, report, princ, NULL);
	list_heap(&cont, cont, NULL);
	list_heap(&restart, restart, cont, unless, NULL);
	list_heap(ret, tagbody, loop, restart, NULL);

	return 0;
}

static int assert_prompt_common_(Execute ptr, addr env, addr *ret, addr place)
{
	/* (multiple-value-bind (a b g w r) (get-setf-expansion PLACE)
	 *   (declare (ignore r))
	 *   `(when (y-or-n-p "Do you want to set a new value in ~A? " ',place)
	 *      (let* ((,a1 ,b1) (,a2 ,b2) ... ,g)
	 *        (declare (ignorable ,a1 ,a2 ...))
	 *        (setq ,g (eval (prompt-for t "Input ~A> " ',place)))
	 *        ,w)))
	 */
	addr a, b, g, w, r;
	addr when, yornp, leta, declare, ignorable, setq, eval, prompt, quote;
	addr str1, str2, root, x, y;

	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	Return_getcar(g, &g);
	strvect_char_heap(&str1, "Do you want to setq a new value in ~A?");
	strvect_char_heap(&str2, "Input ~A> ");
	GetConst(COMMON_WHEN, &when);
	GetConst(COMMON_Y_OR_N_P, &yornp);
	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_EVAL, &eval);
	GetConst(SYSTEM_PROMPT_FOR, &prompt);
	GetConst(COMMON_QUOTE, &quote);
	/* expand */
	list_heap(&place, quote, place, NULL);
	list_heap(&prompt, prompt, T, str2, place, NULL);
	list_heap(&prompt, eval, prompt, NULL);
	list_heap(&setq, setq, g, prompt, NULL);
	lista_heap(&ignorable, ignorable, a, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	for (root = Nil; a != Nil; ) {
		Return_getcons(a, &x, &a);
		Return_getcons(b, &y, &b);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}
	cons_heap(&root, g, root);
	nreverse(&root, root);
	list_heap(&leta, leta, root, declare, setq, w, NULL);
	/* when */
	list_heap(&yornp, yornp, str1, place, NULL);
	list_heap(ret, when, yornp, leta, NULL);

	return 0;
}

static int assert_list_common_(Execute ptr, addr env,
		addr test, addr places, addr output, addr *ret)
{
	/* (tagbody
	 *   loop
	 *   (restart-bind
	 *     ((continue (lambda ()
	 *                  (assert-prompt place1)
	 *                  (assert-prompt place2)
	 *                  ...
	 *                  (go loop))
	 *        :report-function
	 *        (lambda (s)
	 *          (format s "Retry assersion with new value ~{~A~^,~}." places))))
	 *     (unless test
	 *       (error  "Failed assersion ~A." 'test))))
	 */
	addr tagbody, restart, cont, lambda, go, report;
	addr format, unless, error, quote;
	addr loop, s, str, a, b, c;
	LocalHold hold;

	/* places */
	hold = LocalHold_array(ptr, 1);
	for (a = Nil; places != Nil; ) {
		Return_getcons(places, &b, &places);
		Return(assert_prompt_common_(ptr, env, &b, b));
		cons_heap(&a, b, a);
		localhold_set(hold, 0, a);
	}
	localhold_end(hold);

	/* variable */
	make_symbolchar(&loop, "LOOP");
	make_symbolchar(&s, "STREAM");
	strvect_char_heap(&str, "Retry assersion with new value ~{~A~^,~}.");
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_RESTART_BIND, &restart);
	GetConst(COMMON_CONTINUE, &cont);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_GO, &go);
	GetConst(KEYWORD_REPORT_FUNCTION, &report);
	GetConst(COMMON_FORMAT, &format);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_ERROR, &error);
	GetConst(COMMON_QUOTE, &quote);
	/* format */
	if (output == Nil) {
		strvect_char_heap(&output, "Failed assersion ~A");
		list_heap(&c, quote, test, NULL);
		list_heap(&output, output, c, NULL);
	}
	/* expand */
	cons_heap(&error, error, output);
	list_heap(&unless, unless, test, error, NULL);
	list_heap(&quote, quote, places, NULL);
	list_heap(&format, format, s, str, quote, NULL);
	list_heap(&s, s, NULL);
	list_heap(&format, lambda, s, format, NULL);
	/* prompt */
	list_heap(&go, go, loop, NULL);
	cons_heap(&a, go, a);
	nreverse(&a, a);
	lista_heap(&lambda, lambda, Nil, a, NULL);
	list_heap(&cont, cont, lambda, report, format, NULL);
	list_heap(&cont, cont, NULL);
	list_heap(&restart, restart, cont, unless, NULL);
	list_heap(ret, tagbody, loop, restart, NULL);

	return 0;
}

int assert_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, test, list;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &test, &args))
		goto error;
	if (args == Nil)
		return assert_retry_common_(test, ret);
	if (! consp_getcons(args, &list, &args))
		goto error;
	if (list == Nil && args == Nil)
		return assert_retry_common_(test, ret);
	else
		return assert_list_common_(ptr, env, test, list, args, ret);

error:
	return fmte_("ASSERT arguments ~S must be "
			"(test &optional places format args) form.", form, NULL);
}


/*
 *  error
 */
static int error_datum_common_(Execute ptr, addr datum, addr rest, addr *ret)
{
	int check;
	addr make;

	/* symbol -> (make-instance symbol ...) */
	if (symbolp(datum)) {
		Return(clos_find_class_(datum, &datum));
		Return(conditionp_(datum, &check));
		if (! check) {
			*ret = Nil;
			return fmte_("The class ~S is not a condition subclass.", datum, NULL);
		}
		GetConst(COMMON_MAKE_INSTANCE, &make);
		return applya1_control_(ptr, ret, make, datum, rest, NULL);
	}

	/* condition -> (error condition) */
	Return(condition_instance_p_(datum, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Invalid datum argument ~S.", datum, NULL);
	}
	if (rest != Nil) {
		*ret = Nil;
		return fmte_("The datum argument ~S must be a nil "
				"if first argument is condition type.", datum, NULL);
	}

	return Result(ret, datum);
}

int error_common_(Execute ptr, addr datum, addr rest)
{
	if (stringp(datum)) {
		/* string -> simple-error */
		return call_simple_error_(ptr, datum, rest);
	}
	else {
		Return(error_datum_common_(ptr, datum, rest, &datum));
		return error_function_(ptr, datum);
	}
}


/*
 *  cerror
 */
static int cerror_restart_common_(Execute ptr, addr *ret, addr format, addr args)
{
	addr inst, pos;

	Return(format_string_lisp_(ptr, format, args, &format));
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&inst, pos);
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(inst, pos);
	setinteractive_restart(inst, Nil);
	setreport_restart(inst, format);
	settest_restart(inst, Nil);
	setescape_restart(inst, 1);

	return Result(ret, inst);
}

int cerror_common_(Execute ptr, addr restart, addr datum, addr rest)
{
	addr control;

	push_control(ptr, &control);
	Return(cerror_restart_common_(ptr, &restart, restart, rest));
	pushrestart_control(ptr, restart);

	/* (error ...) */
	(void)error_common_(ptr, datum, rest);
	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* continue */
	if (ptr->throw_handler == restart) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}


/*
 *  check-type
 */
static int check_type_expand_common_(Execute ptr, addr env, addr *ret,
		addr place, addr type, addr string)
{
	/* (let* ((a1 b1) (a2 b2) ... (value r) g)
	 *   (declare (ignorable a1 a2 ...))
	 *   (tagbody
	 *     loop
	 *     (restart-bind
	 *       ((store-value
	 *          (lambda (v) (setq g v value v) w (go loop))
	 *          :report-function
	 *            (lambda (s)
	 *              (princ "Retry check-type with new value xx." s))
	 *          :interactive-function
	 *            (lambda ()
	 *              (list (eval (prompt-for t "Input xx> "))))))
	 *       (unless (typep value 'type)
	 *         (error
	 *           (make-condition 'simple-type-error
	 *             :datum value
	 *             :expected-type 'type
	 *             :format-control "The value of xx, ~A, is not ~(~A~)."
	 *             :format-arguments (list value string)))))))
	 */
	addr a, b, g, r, w, v, s, str1, str2, str3;
	addr leta, declare, ignorable, tagbody, loop, restart, store, lambda, setq;
	addr value, go, report, inter, princ, list, eval, prompt, unless;
	addr typep, quote, invoke, make, simple, datum, expect, control, arguments;
	addr x, y, root;
	LocalHold hold;

	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	hold = LocalHold_local(ptr);
	localhold_pushva(hold, a, b, g, w, r, NULL);
	Return_getcar(g, &g);

	Return(format_string_(ptr, &str1,
				"Retry check-type with new value ~A.", place, NULL));
	localhold_push(hold, str1);

	Return(format_string_(ptr, &str2,
				"Input ~A> ", place, NULL));
	localhold_push(hold, str2);

	Return(format_string_(ptr, &str3,
				"The value of ~A, ~~A, is not ~~A.", place, NULL));
	localhold_push(hold, str3);

	if (string == Nil) {
		Return(parse_type_(ptr, &string, type, env));
		localhold_push(hold, string);
		Return(type_object_(&string, string));
		localhold_push(hold, string);
		Return(princ_string_heap_(ptr, &string, string));
	}
	localhold_end(hold);

	make_symbolchar(&v, "V");
	make_symbolchar(&s, "STREAM");
	make_symbolchar(&loop, "LOOP");
	make_symbolchar(&value, "VALUE");
	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_RESTART_BIND, &restart);
	GetConst(COMMON_STORE_VALUE, &store);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_GO, &go);
	GetConst(KEYWORD_REPORT_FUNCTION, &report);
	GetConst(KEYWORD_INTERACTIVE_FUNCTION, &inter);
	GetConst(COMMON_PRINC, &princ);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_EVAL, &eval);
	GetConst(SYSTEM_PROMPT_FOR, &prompt);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_TYPEP, &typep);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_ERROR, &invoke);
	GetConst(COMMON_MAKE_CONDITION, &make);
	GetConst(COMMON_SIMPLE_TYPE_ERROR, &simple);
	GetConst(KEYWORD_DATUM, &datum);
	GetConst(KEYWORD_EXPECTED_TYPE, &expect);
	GetConst(KEYWORD_FORMAT_CONTROL, &control);
	GetConst(KEYWORD_FORMAT_ARGUMENTS, &arguments);
	/* expand */
	list_heap(&x, list, value, string, NULL);
	list_heap(&type, quote, type, NULL);
	list_heap(&simple, quote, simple, NULL);
	list_heap(&make, make, simple,
			datum, value, expect, type, control, str3, arguments, x, NULL);
	list_heap(&invoke, invoke, make, NULL);
	list_heap(&typep, typep, value, type, NULL);
	list_heap(&unless, unless, typep, invoke, NULL);
	list_heap(&prompt, prompt, T, str2, NULL);
	list_heap(&eval, eval, prompt, NULL);
	list_heap(&list, list, eval, NULL);
	list_heap(&x, lambda, Nil, list, NULL);
	list_heap(&princ, princ, str1, s, NULL);
	list_heap(&s, s, NULL);
	list_heap(&y, lambda, s, princ, NULL);
	list_heap(&go, go, loop, NULL);
	list_heap(&setq, setq, g, v, value, v, NULL);
	list_heap(&v, v, NULL);
	list_heap(&lambda, lambda, v, setq, w, go, NULL);
	list_heap(&store, store, lambda, report, y, inter, x, NULL);
	list_heap(&store, store, NULL);
	list_heap(&restart, restart, store, unless, NULL);
	list_heap(&tagbody, tagbody, loop, restart, NULL);
	/* let* */
	lista_heap(&ignorable, ignorable, a, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	for (root = Nil; a != Nil; ) {
		Return_getcons(a, &x, &a);
		Return_getcons(b, &y, &b);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}
	list_heap(&value, value, r, NULL);
	cons_heap(&root, value, root);
	cons_heap(&root, g, root);
	nreverse(&root, root);
	list_heap(ret, leta, root, declare, tagbody, NULL);

	return 0;
}

int check_type_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, place, type, string;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &place, &args))
		goto error;
	if (! consp_getcons(args, &type, &args))
		goto error;
	if (args == Nil)
		string = Nil;
	else {
		if (! consp_getcons(args, &string, &args))
			goto error;
		if (args != Nil)
			goto error;
	}
	return check_type_expand_common_(ptr, env, ret, place, type, string);

error:
	return fmte_("CHECK-TYPE arguments ~S must be "
			"(place type &optional string) form.", form, NULL);
}


/*
 *  invalid-method-error
 */
int invalid_method_error_common_(Execute ptr, addr method, addr format, addr args)
{
	addr control, arguments, pos;

	strvect_char_heap(&control, "Method error: ~S~%~?");
	list_heap(&arguments, method, format, args, NULL);
	Return(instance_simple_error_(&pos, control, arguments));
	return error_function_(ptr, pos);
}


/*
 *  method-combination-error
 */
int method_combination_error_common_(Execute ptr, addr format, addr args)
{
	addr control, arguments, pos;

	strvect_char_heap(&control, "Method-Combination error:~%~?");
	list_heap(&arguments, format, args, NULL);
	Return(instance_simple_error_(&pos, control, arguments));
	return error_function_(ptr, pos);
}


/*
 *  signal
 */
int signal_common_(Execute ptr, addr datum, addr rest)
{
	if (stringp(datum)) {
		/* string -> simple-condition */
		Return(instance_simple_condition_(&datum, datum, rest));
	}
	else {
		Return(error_datum_common_(ptr, datum, rest, &datum));
	}
	return signal_function_(ptr, datum);
}


/*
 *  warn
 */
int warn_common_(Execute ptr, addr datum, addr rest)
{
	if (stringp(datum)) {
		/* string -> simple-warning */
		Return(instance_simple_warning_(&datum, datum, rest));
	}
	else {
		Return(error_datum_common_(ptr, datum, rest, &datum));
	}

	return warning_restart_case_(ptr, datum);
}


/*
 *  break
 */
static int break_invoke_common_(Execute ptr, addr format, addr args)
{
	addr symbol, condition;
	LocalHold hold;

	GetConst(SPECIAL_DEBUGGER_HOOK, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	Return(instance_simple_condition_(&condition, format, args));

	hold = LocalHold_local_push(ptr, condition);
	Return(invoke_debugger_(ptr, condition));
	localhold_end(hold);

	return 0;
}

static int break_restart_common_(Execute ptr, addr restart, addr format, addr args)
{
	addr control;

	push_control(ptr, &control);
	(void)restart2_control_(ptr, restart, break_invoke_common_, format, args);
	return pop_control_(ptr, control);
}

static void break_make_common(addr *ret)
{
	static const char *message = "Return from BREAK.";
	addr inst, pos;

	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&inst, pos);
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(inst, pos);
	setinteractive_restart(inst, Nil);
	strvect_char_heap(&pos, message);
	setreport_restart(inst, pos);
	settest_restart(inst, Nil);
	setescape_restart(inst, 1);
	*ret = inst;
}

int break_common_(Execute ptr, addr format, addr args)
{
	/* (defun break (&optional (format-control "Break") &rest args)
	 *   (with-simple-restart (continue "Return from BREAK.")
	 *     (let ((*debugger-hook* nil))
	 *       (invoke-debugger
	 *         (make-condition 'simple-condition
	 *           :format-control format :format-arguments args))))
	 *   nil)
	 */
	addr restart;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	if (format == Unbound) {
		strvect_char_heap(&format, "Break");
		localhold_push(hold, format);
		args = Nil;
	}
	break_make_common(&restart);
	localhold_push(hold, restart);
	Return(break_restart_common_(ptr, restart, format, args));
	localhold_end(hold);

	return 0;
}


/*
 *  handler-bind
 */
static int handler_bind_clauses_common_(addr form, addr *ret)
{
	addr cons, symbol, quote, root, name, lambda, temp;

	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_HANDLER_BIND, &symbol);
	conscar_heap(&root, symbol);
	while (form != Nil) {
		Return_getcons(form, &cons, &form);
		/* (name lambda) */
		Return(lista_bind_(cons, &name, &lambda, &temp, NULL));
		if (temp != Nil) {
			return fmte_("handler-bind argument ~S"
					" must be a (name lambda) form.", cons, NULL);
		}
		/* (push 'name root) */
		list_heap(&name, quote, name, NULL);
		cons_heap(&root, name, root);
		/* (push (lambda ...) root) */
		cons_heap(&root, lambda, root);
	}
	/* result */
	nreverse(ret, root);

	return 0;
}

int handler_bind_common_(addr form, addr env, addr *ret)
{
	addr symbol, body;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &form, &body))
		return fmte_("Too few handler-bind argument.", NULL);
	if (form == Nil) {
		GetConst(COMMON_PROGN, &symbol);
		cons_heap(ret, symbol, body);
	}
	else {
		GetConst(SYSTEM_HANDLER, &symbol);
		Return(handler_bind_clauses_common_(form, &form));
		if (body == Nil)
			consnil_heap(&body);
		lista_heap(ret, symbol, form, body, NULL);
	}
	return 0;
}


/*
 *  handler-case
 */
static int handler_case_gensym_common_(Execute ptr, addr form, addr *ret)
{
	addr args, declare, pos, cons, gensym;

	/* (lambda (gensym)
	 *   (declare (ignore gensym))
	 *   form)
	 */
	/* (gensym) */
	Return(make_gensym_(ptr, &gensym));
	conscar_heap(&args, gensym);
	/* (declare (ignore gensym)) */
	GetConst(COMMON_IGNORE, &pos);
	list_heap(&cons, pos, gensym, NULL);
	GetConst(COMMON_DECLARE, &pos);
	list_heap(&declare, pos, cons, NULL);
	/* (lambda ...) */
	GetConst(COMMON_LAMBDA, &pos);
	lista_heap(ret, pos, args, declare, form, NULL);

	return 0;
}

static void handler_case_lambda_common(addr args, addr form, addr *ret)
{
	addr pos;
	GetConst(COMMON_LAMBDA, &pos);
	lista_heap(ret, pos, args, form, NULL);
}

static int handler_case_noerror_common_(addr *no_error, addr args, addr form)
{
	addr lambda;

	if (*no_error) {
		return fmtw_("There are multiple :no-error clauses ~S"
				" in handler-case.", args, NULL);
	}
	else {
		GetConst(COMMON_LAMBDA, &lambda);
		lista_heap(no_error, lambda, args, form, NULL);
	}

	return 0;
}

static int handler_case_clauses_common_(Execute ptr, addr right, addr *ret, addr *rete)
{
	addr no_error, root, cons, name, args, form;
	addr keyword, symbol, quote;

	/* (lisp-system::handler-case name1 lambda1 ...) */
	no_error = NULL;
	GetConst(KEYWORD_NO_ERROR, &keyword);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_HANDLER_CASE, &symbol);
	conscar_heap(&root, symbol);
	while (right != Nil) {
		Return_getcons(right, &cons, &right);
		Return(lista_bind_(cons, &name, &args, &form, NULL));

		/* (:no-error (...) form) */
		if (name == keyword) {
			Return(handler_case_noerror_common_(&no_error, args, form));
			continue;
		}

		if (args == Nil) {
			/* (name () form) */
			Return(handler_case_gensym_common_(ptr, form, &cons));
		}
		else if (singlep(args)) {
			/* (name (c) form) */
			handler_case_lambda_common(args, form, &cons);
		}
		else {
			*ret = *rete = NULL;
			return fmte_("The argument ~S in handler-case clause "
					"must be a nil or (var) form.", args, NULL);
		}
		/* (push 'name root) */
		list_heap(&name, quote, name, NULL);
		cons_heap(&root, name, root);
		/* (push (lambda ...) root) */
		cons_heap(&root, cons, root);
	}
	/* result */
	nreverse(ret, root);
	*rete = no_error;

	return 0;
}

static void handler_case_body(addr no_error, addr expr, addr *ret)
{
	addr mvcall;

	if (no_error == NULL) {
		*ret = expr;
		return;
	}

	/* (multiple-value-call
	 *   (lambda (a b c d) ...)
	 *   expr))
	 */
	GetConst(COMMON_MULTIPLE_VALUE_CALL, &mvcall);
	list_heap(ret, mvcall, no_error, expr, NULL);
}

int handler_case_common_(Execute ptr, addr list, addr env, addr *ret)
{
	addr symbol, expr, no_error;

	Return_getcdr(list, &list);
	if (! consp_getcons(list, &expr, &list))
		return fmte_("Too few handler-case argument.", NULL);
	if (list == Nil)
		return Result(ret, expr);

	GetConst(SYSTEM_HANDLER, &symbol);
	Return(handler_case_clauses_common_(ptr, list, &list, &no_error));
	handler_case_body(no_error, expr, &expr);
	list_heap(ret, symbol, list, expr, NULL);

	return 0;
}


/*
 *  ignore-errors
 */
int ignore_errors_common_(Execute ptr, addr form, addr env, addr *ret)
{
	/* `(handler-case (progn ,@form)
	 *    (error (g) (values nil g)))
	 */
	addr handler, progn, error, values, g;

	Return_getcdr(form, &form);
	GetConst(COMMON_HANDLER_CASE, &handler);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_ERROR, &error);
	GetConst(COMMON_VALUES, &values);
	Return(make_gensym_(ptr, &g));
	list_heap(&values, values, Nil, g, NULL);
	list_heap(&g, g, NULL);
	list_heap(&error, error, g, values, NULL);
	cons_heap(&progn, progn, form);
	list_heap(ret, handler, progn, error, NULL);

	return 0;
}


/*
 *  make-condition
 */
int make_condition_common_(Execute ptr, addr args, addr *ret)
{
	int check;
	addr condition, expected, call;

	/* type check */
	Return_getcar(args, &condition);
	if (symbolp(condition)) {
		Return(clos_find_class_(condition, &condition));
	}
	Return(conditionp_(condition, &check));
	if (! check) {
		GetConst(COMMON_CONDITION, &expected);
		return call_type_error_va_(ptr, condition, expected,
				"The argument ~S must be a condition.", condition, NULL);
	}

	/* (make-instance ...) */
	GetConst(COMMON_MAKE_INSTANCE, &call);
	Return(getfunction_global_(call, &call));
	return apply1_control_(ptr, ret, call, args);
}


/*
 *  compute-restarts
 */
int compute_restarts_common_(Execute ptr, addr pos, addr *ret)
{
	if (pos == Unbound)
		pos = Nil;
	return compute_restarts_control_(ptr, pos, ret);
}


/*
 *  find-restart
 */
int find_restart_common_(Execute ptr, addr var, addr opt, addr *ret)
{
	int check;

	if (opt == Unbound)
		opt = Nil;
	Return(find_restart_control_(ptr, var, opt, &var, &check));

	return Result(ret, check? var: Nil);
}


/*
 *  restart-bind
 */
static int restart_bind_interactive_common_(addr *args, addr *inter)
{
	if (*args == Nil) {
		return fmte_("The key :interactive-function must have a value.", NULL);
	}
	if (*inter != Nil) {
		Return_getcdr(*args, args);
	}
	else {
		Return_getcons(*args, inter, args);
		if (*inter == Nil)
			return fmte_(":interactive-function ~S must be a function.", *inter, NULL);
	}

	return 0;
}

static int restart_bind_report_common_(addr *args, addr *report)
{
	if (*args == Nil) {
		return fmte_("The key :report-function must have a value.", NULL);
	}
	if (*report != Nil) {
		Return_getcdr(*args, args);
	}
	else {
		Return_getcons(*args, report, args);
		if (*report == Nil)
			return fmte_(":report-function ~S must be a function.", *report, NULL);
	}

	return 0;
}

static int restart_bind_test_common_(addr *args, addr *test)
{
	if (*args == Nil) {
		return fmte_("The key :test-function must have a value.", NULL);
	}
	if (*test != Nil) {
		Return_getcdr(*args, args);
	}
	else {
		Return_getcons(*args, test, args);
		if (*test == Nil)
			return fmte_(":test-function ~S must be a function.", *test, NULL);
	}

	return 0;
}

static void restart_bind_symbol_common(addr pos, addr *ret)
{
	addr funct;

	if (pos != Nil && symbolp(pos)) {
		GetConst(COMMON_FUNCTION, &funct);
		list_heap(ret, funct, pos, NULL);
	}
	else {
		*ret = pos;
	}
}

static int restart_bind_binding_common_(addr args, addr *ret)
{
	addr pos, name, lambda, inter, report, test;
	addr keyinter, keyreport, keytest;
	addr quote, list;

	GetConst(KEYWORD_INTERACTIVE_FUNCTION, &keyinter);
	GetConst(KEYWORD_REPORT_FUNCTION, &keyreport);
	GetConst(KEYWORD_TEST_FUNCTION, &keytest);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_QUOTE, &quote);

	/* (name lambda
	 *   :interactive-function x
	 *   :report-function y
	 *   :test-function z)
	 */
	Return(lista_bind_(args, &name, &lambda, &args, NULL));
	inter = report = test = Nil;
	while (args != Nil) {
		Return_getcons(args, &pos, &args);
		if (pos == keyinter) {
			Return(restart_bind_interactive_common_(&args, &inter));
		}
		else if (pos == keyreport) {
			Return(restart_bind_report_common_(&args, &report));
		}
		else if (pos == keytest) {
			Return(restart_bind_test_common_(&args, &test));
		}
		else {
			return fmte_("Invalid key parameter ~S.", pos, NULL);
		}
	}
	restart_bind_symbol_common(inter, &inter);
	restart_bind_symbol_common(report, &report);
	restart_bind_symbol_common(test, &test);
	list_heap(&name, quote, name, NULL);
	list_heap(ret, list, name, lambda, inter, report, test, NULL);

	return 0;
}

static int restart_bind_clauses_common_(addr right, addr *ret)
{
	addr cons, symbol, quote, root;

	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_RESTART_BIND, &symbol);
	conscar_heap(&root, symbol);
	while (right != Nil) {
		Return_getcons(right, &cons, &right);
		Return(restart_bind_binding_common_(cons, &cons));
		cons_heap(&root, cons, root);
	}
	nreverse(ret, root);

	return 0;
}

int restart_bind_common_(addr right, addr env, addr *ret)
{
	addr symbol, body;

	Return_getcdr(right, &right);
	if (! consp_getcons(right, &right, &body))
		return fmte_("Too few restart-bind argument.", NULL);
	if (right == Nil) {
		GetConst(COMMON_PROGN, &symbol);
		cons_heap(ret, symbol, body);
	}
	else {
		GetConst(SYSTEM_RESTART, &symbol);
		Return(restart_bind_clauses_common_(right, &right));
		if (body == Nil)
			consnil_heap(&body);
		lista_heap(ret, symbol, right, body, NULL);
	}

	return 0;
}


/*
 *  restart-case
 */
static int restart_case_interactive_(addr *args, addr *inter)
{
	if (*args == Nil) {
		return fmte_("The key :interactive must have a value.", NULL);
	}
	if (*inter != Nil) {
		Return_getcdr(*args, args);
	}
	else {
		Return_getcons(*args, inter, args);
		if (*inter == Nil)
			return fmte_(":interactive ~S must be a function.", *inter, NULL);
	}

	return 0;
}

static int restart_case_report_(addr *args, addr *report)
{
	if (*args == Nil) {
		return fmte_("The key :report must have a value.", NULL);
	}
	if (*report != Nil) {
		Return_getcdr(*args, args);
	}
	else {
		Return_getcons(*args, report, args);
		if (*report == Nil)
			return fmte_(":report ~S must be a function.", *report, NULL);
	}

	return 0;
}

static int restart_case_test_(addr *args, addr *test)
{
	if (*args == Nil) {
		return fmte_("The key :test must have a value.", NULL);
	}
	if (*test != Nil) {
		Return_getcdr(*args, args);
	}
	else {
		Return_getcons(*args, test, args);
		if (*test == Nil)
			return fmte_(":test ~S must be a function.", *test, NULL);
	}

	return 0;
}

static int restart_case_parse_(addr cons,
		addr *rname, addr *rinter, addr *rreport, addr *rtest, addr *rbody)
{
	addr name, args, body, inter, report, test, pos, lambda;
	addr keyinter, keyreport, keytest;

	*rname = *rinter = *rreport = *rtest = *rbody = Nil;
	GetConst(KEYWORD_INTERACTIVE, &keyinter);
	GetConst(KEYWORD_REPORT, &keyreport);
	GetConst(KEYWORD_TEST, &keytest);

	/* (name (c) body) */
	Return(lista_bind_(cons, &name, &args, &body, NULL));
	if (! symbolp(name))
		return fmte_("Restart name ~S must be a symbol type.", name, NULL);

	/* body */
	inter = report = test = Nil;
	for (; body != Nil; body = cons) {
		Return_getcons(body, &pos, &cons);
		if (pos == keyinter) {
			Return(restart_case_interactive_(&cons, &inter));
		}
		else if (pos == keyreport) {
			Return(restart_case_report_(&cons, &report));
		}
		else if (pos == keytest) {
			Return(restart_case_test_(&cons, &test));
		}
		else {
			break;
		}
	}

	/* `(lambda ,args ,@body) */
	GetConst(COMMON_LAMBDA, &lambda);
	lista_heap(rbody, lambda, args, body, NULL);

	/* result */
	*rname = name;
	restart_bind_symbol_common(inter, rinter);
	restart_bind_symbol_common(report, rreport);
	restart_bind_symbol_common(test, rtest);

	return 0;
}


/*
 *  restart-case  [condition]
 *
 *  (let* ((#:r1 (lisp-system::make-restart 'aaa :escape t))
 *         (#:r2 (lisp-system::make-restart 'bbb :escape t))
 *         (#:r3 (lisp-system::make-restart 'ccc :escape t))
 *         (#:list (list #:r1 #:r2 #:r3))
 *         (#:instance (lisp-system:condition-restarts-make 'signal ...)))
 *    (lisp-system::restart-progn
 *      #:list
 *      (lambda ()
 *        (with-condition-restarts
 *          #:instance
 *          #:list
 *          (signal #:instance)))))
 */
static int restart_condition_clauses_(addr form, addr *ret)
{
	addr root, cons, name, body, inter, report, test;
	addr quote, make, pos;
	addr key1, key2, key3, key4;

	/* ((lisp-system::make-restart 'name1 call ...)
	 *  (lisp-system::make-restart 'name2 call ...)
	 *  ...)
	 */
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_MAKE_RESTART, &make);
	GetConst(KEYWORD_INTERACTIVE_FUNCTION, &key1);
	GetConst(KEYWORD_REPORT_FUNCTION, &key2);
	GetConst(KEYWORD_TEST_FUNCTION, &key3);
	GetConst(KEYWORD_ESCAPE, &key4);

	root = Nil;
	while (form != Nil) {
		Return_getcons(form, &cons, &form);
		Return(restart_case_parse_(cons, &name, &inter, &report, &test, &body));

		/* (lisp-system::make-restart 'name ...) */
		conscar_heap(&pos, make);
		/* name */
		list_heap(&name, quote, name, NULL);
		cons_heap(&pos, name, pos);
		/* body */
		cons_heap(&pos, body, pos);
		/* interactive */
		if (inter != Nil) {
			cons_heap(&pos, key1, pos);
			cons_heap(&pos, inter, pos);
		}
		/* report */
		if (report != Nil) {
			cons_heap(&pos, key2, pos);
			cons_heap(&pos, report, pos);
		}
		/* test */
		if (report != Nil) {
			cons_heap(&pos, key3, pos);
			cons_heap(&pos, test, pos);
		}
		/* escape */
		cons_heap(&pos, key4, pos);
		cons_heap(&pos, T, pos);
		/* result */
		nreverse(&pos, pos);
		cons_heap(&root, pos, root);
	}

	/* result */
	nreverse(ret, root);
	return 0;
}

static int restart_condition_progn_(addr expr, addr *ret)
{
	/* `(progn ,expr) */
	addr progn;

	GetConst(COMMON_PROGN, &progn);
	cons_heap(ret, progn, expr);

	return 0;
}

static int restart_condition_signal_(addr car, addr cdr, addr g, addr *ret, addr *rexpr)
{
	addr make, type;

	/* (condition-restarts-make 'signal ...) */
	GetConst(SYSTEM_CONDITION_RESTARTS_MAKE, &make);
	quotelist_heap(&type, car);
	lista_heap(ret, make, type, cdr, NULL);

	/* (signal #:g) */
	list_heap(rexpr, car, g, NULL);

	return 0;
}

static int restart_condition_cerror_(addr car, addr cdr, addr g, addr *ret, addr *rexpr)
{
	addr make, type, first;

	/* (condition-restarts-make 'cerror ...) */
	GetConst(SYSTEM_CONDITION_RESTARTS_MAKE, &make);
	Return_getcons(cdr, &first, &cdr);
	quotelist_heap(&type, car);
	lista_heap(ret, make, type, cdr, NULL);

	/* `(cerror ,first #:g) */
	list_heap(rexpr, car, first, g, NULL);

	return 0;
}

static int restart_condition_make_(addr expr, addr g, addr *ret, addr *rexpr)
{
	addr check, car, cdr;

	/* signal */
	Return_getcons(expr, &car, &cdr);
	GetConst(COMMON_SIGNAL, &check);
	if (car == check)
		goto signal;

	/* error */
	GetConst(COMMON_ERROR, &check);
	if (car == check)
		goto signal;

	/* warn */
	GetConst(COMMON_WARN, &check);
	if (car == check)
		goto signal;

	/* cerror */
	GetConst(COMMON_CERROR, &check);
	if (car == check)
		goto cerror;

	/* error */
	*ret = Nil;
	return fmte_("Invalid format, ~S.", expr, NULL);

signal:
	return restart_condition_signal_(car, cdr, g, ret, rexpr);

cerror:
	return restart_condition_cerror_(car, cdr, g, ret, rexpr);
}

static int restart_condition_bind_(Execute ptr, addr expr, addr args, addr *ret)
{
	addr root, glist, ginst, make, pos, g, vars;
	addr list, leta, rprogn, lambda, with;

	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_WITH_CONDITION_RESTARTS, &with);
	GetConst(SYSTEM_RESTART_PROGN, &rprogn);

	/* args */
	root = vars = Nil;
	while (args != Nil) {
		GetCons(args, &pos, &args);
		Return(make_gensym_(ptr, &g));
		cons_heap(&vars, g, vars);
		list_heap(&pos, g, pos, NULL);
		cons_heap(&root, pos, root);
	}
	nreverse(&vars, vars);

	/* (#:glist (list #:r1 #:r2 #:r3)) */
	Return(make_gensym_(ptr, &glist));
	cons_heap(&list, list, vars);
	list_heap(&list, glist, list, NULL);
	cons_heap(&root, list, root);

	/* (#:ginst ,make) */
	Return(make_gensym_(ptr, &ginst));
	Return(restart_condition_make_(expr, ginst, &make, &expr));
	list_heap(&pos, ginst, make, NULL);
	cons_heap(&root, pos, root);

	/* (let* (,list)
	 *   (lisp-system::restart-progn
	 *     #:glist
	 *     (lambda ()
	 *       (with-condition-restarts
	 *         #:ginst
	 *         #:glist
	 *         (signal #:ginst)))))
	 */
	list_heap(&with, with, ginst, glist, expr, NULL);
	list_heap(&lambda, lambda, Nil, with, NULL);
	list_heap(&rprogn, rprogn, glist, with, NULL);
	nreverse(&root, root);
	list_heap(ret, leta, root, rprogn, NULL);

	return 0;
}

static int restart_condition_(Execute ptr, addr form, addr expr, addr *ret)
{
	addr args;

	Return(restart_condition_clauses_(form, &args));
	if (args == Nil)
		return restart_condition_progn_(expr, ret);

	return restart_condition_bind_(ptr, expr, args, ret);
}


/*
 *  restart-case  [normal]
 */
static int restart_case_expr_(addr expr, addr *ret)
{
	addr x, y;

	if (! consp_getcar(expr, &x))
		return Result(ret, Nil);

	/* signal, error, warn */
	GetConst(COMMON_SIGNAL, &y);
	if (x == y)
		return Result(ret, x);
	GetConst(COMMON_ERROR, &y);
	if (x == y)
		return Result(ret, x);
	GetConst(COMMON_WARN, &y);
	if (x == y)
		return Result(ret, x);

	/* cerror */
	GetConst(COMMON_CERROR, &y);
	if (x == y)
		return Result(ret, x);

	/* otherwise */
	return Result(ret, Nil);
}

static int restart_case_clauses_(addr form, addr *ret)
{
	addr root, cons, name, body, inter, report, test;
	addr quote, symbol, list;

	/* (lisp-system::restart-case (list ...) (list ...) ...) */
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_RESTART_CASE, &symbol);
	conscar_heap(&root, symbol);
	while (form != Nil) {
		Return_getcons(form, &cons, &form);
		Return(restart_case_parse_(cons, &name, &inter, &report, &test, &body));

		/* (list 'name (lambda ...) ...) */
		list_heap(&name, quote, name, NULL);
		list_heap(&cons, list, name, body, inter, report, test, NULL);
		cons_heap(&root, cons, root);
	}
	/* result */
	nreverse(ret, root);

	return 0;
}

int restart_case_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr symbol, expr, car;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &expr, &form))
		return fmte_("Too few restart-case argument.", NULL);
	if (form == Nil)
		return Result(ret, expr);

	GetConst(SYSTEM_RESTART, &symbol);
	Return(restart_case_expr_(expr, &car));
	if (car != Nil)
		return restart_condition_(ptr, form, expr, ret);
	Return(restart_case_clauses_(form, &form));
	list_heap(ret, symbol, form, expr, NULL);

	return 0;
}


/*
 *  with-condition-restarts
 */
static void with_condition_restarts_expander(addr *ret,
		addr condition_form, addr restarts_form, addr body)
{
	/* `(let ((,condition ,condition-form)
	 *        (,restarts ,restarts-form))
	 *    (unwind-protect
	 *      (progn
	 *        (lisp-system::condition-restarts-push ,condition ,restarts)
	 *        ,@body)
	 *      (lisp-system::condition-restarts-pop ,condition ,restarts)))
	 */
	addr let, push, pop, unwind, progn;
	addr condition, restarts;

	GetConst(COMMON_LETA, &let);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_PROGN, &progn);
	GetConst(SYSTEM_CONDITION_RESTARTS_PUSH, &push);
	GetConst(SYSTEM_CONDITION_RESTARTS_POP, &pop);
	make_symbolchar(&condition, "CONDITION");
	make_symbolchar(&restarts, "RESTARTS");

	list_heap(&push, push, condition, restarts, NULL);
	lista_heap(&progn, progn, push, body, NULL);
	list_heap(&pop, pop, condition, restarts, NULL);
	list_heap(&unwind, unwind, progn, pop, NULL);
	list_heap(&condition, condition, condition_form, NULL);
	list_heap(&restarts, restarts, restarts_form, NULL);
	list_heap(&condition, condition, restarts, NULL);
	list_heap(ret, let, condition, unwind, NULL);
}

int with_condition_restarts_common_(addr form, addr *ret)
{
	addr condition, list;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &condition, &form))
		return fmte_("Too few with-condition-restarts argument.", NULL);
	if (! consp_getcons(form, &list, &form))
		return fmte_("Too few with-condition-restarts argument.", NULL);
	if (form == Nil)
		consnil_heap(&form);

	with_condition_restarts_expander(ret, condition, list, form);
	return 0;
}


/*
 *  with-simple-restart
 */
int with_simple_restart_common_(addr form, addr env, addr *ret)
{
	/* (defmacro with-simple-restart ((name &rest args) &body body)
	 *   `(restart-case
	 *      (progn ,@body)
	 *      (,name ()
	 *        :report (lambda (s) (format s ,@args))
	 *        (values nil t))))
	 */
	addr args, body, name;
	addr restart, progn, report, lambda, s, format, values;

	/* parse */
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &args, &body))
		goto error;
	if (! consp_getcons(args, &name, &args))
		goto error;
	if (! consp(args))
		goto error;

	/* macro */
	GetConst(COMMON_RESTART_CASE, &restart);
	GetConst(COMMON_PROGN, &progn);
	GetConst(KEYWORD_REPORT, &report);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_FORMAT, &format);
	make_symbolchar(&s, "STREAM");
	GetConst(COMMON_VALUES, &values);
	list_heap(&values, values, Nil, T, NULL);
	lista_heap(&format, format, s, args, NULL);
	list_heap(&s, s, NULL);
	list_heap(&lambda, lambda, s, format, NULL);
	list_heap(&name, name, Nil, report, lambda, values, NULL);
	cons_heap(&progn, progn, body);
	list_heap(ret, restart, progn, name, NULL);
	return 0;

error:
	return fmte_("WITH-SIMPLE-RESTART arguments ~S must be "
			"((name format ...) &body body) form.", form, NULL);
}


/*
 *  abort
 */
int abort_common_(Execute ptr, addr opt)
{
	addr pos;

	if (opt == Unbound)
		opt = Nil;
	GetConst(COMMON_ABORT, &pos);
	Return(find_restart_control_error_(ptr, pos, opt, &opt));

	return invoke_restart_control_(ptr, opt, Nil);
}


/*
 *  continue
 */
int continue_common_(Execute ptr, addr opt)
{
	int check;
	addr pos;

	if (opt == Unbound)
		opt = Nil;
	GetConst(COMMON_CONTINUE, &pos);
	Return(find_restart_control_(ptr, pos, opt, &opt, &check));
	if (check) {
		Return(invoke_restart_control_(ptr, opt, Nil));
	}

	return 0;
}


/*
 *  muffle-warning
 */
int muffle_warning_common_(Execute ptr, addr opt)
{
	int check;
	addr pos;

	if (opt == Unbound)
		opt = Nil;
	GetConst(COMMON_MUFFLE_WARNING, &pos);
	Return(find_restart_control_(ptr, pos, opt, &opt, &check));
	if (check)
		return invoke_restart_control_(ptr, opt, Nil);
	else
		return call_control_error_(ptr);
}


/*
 *  store-value
 */
int store_value_common_(Execute ptr, addr var, addr opt)
{
	int check;
	addr pos;

	if (opt == Unbound)
		opt = Nil;
	GetConst(COMMON_STORE_VALUE, &pos);
	Return(find_restart_control_(ptr, pos, opt, &opt, &check));
	if (check) {
		list_local(ptr->local, &var, var, NULL);
		Return(invoke_restart_control_(ptr, opt, var));
	}

	return 0;
}


/*
 *  use-value
 */
int use_value_common_(Execute ptr, addr var, addr opt)
{
	int check;
	addr pos;

	if (opt == Unbound)
		opt = Nil;
	GetConst(COMMON_USE_VALUE, &pos);
	Return(find_restart_control_(ptr, pos, opt, &opt, &check));
	if (check) {
		list_local(ptr->local, &var, var, NULL);
		Return(invoke_restart_control_(ptr, opt, var));
	}

	return 0;
}


/************************************************************
 *  call_conses.c
 ************************************************************/

static int function_call_cons_(Execute ptr, int *result,
		addr item, addr key, addr test, addr check, int notret)
{
	if (key != Nil)
		Return(funcall1_control_(ptr, &check, key, check, NULL));
	Return(funcall1_control_(ptr, &check, test, item, check, NULL));
	*result = (notret? (check == Nil): (check != Nil));
	return 0;
}

static int function_if_call_cons_(Execute ptr, int *result,
		addr key, addr call, addr check)
{
	if (key != Nil)
		Return(funcall1_control_(ptr, &check, key, check, NULL));
	Return(funcall1_control_(ptr, &check, call, check, NULL));
	*result = (check != Nil);
	return 0;
}


/*
 *  sublis
 */
struct sublis_struct {
	Execute ptr;
	addr alist, key, test1, test2;
	int test;
};

static int default_sublis_cons_(addr alist, addr left, int *result, addr *ret)
{
	addr right, value;

	while (alist != Nil) {
		Return_getcons(alist, &right, &alist);
		if (right == Nil)
			continue;
		Return_getcons(right, &right, &value);
		if (eql_function(left, right)) {
			*result = 1;
			return Result(ret, value);
		}
	}
	*result = 0;
	return Result(ret, left);
}

static int test_sublis_cons_(Execute ptr,
		addr alist, addr test, addr left, int *result, addr *ret)
{
	addr right, value, check;

	while (alist != Nil) {
		Return_getcons(alist, &right, &alist);
		if (right == Nil)
			continue;
		Return_getcons(right, &right, &value);
		Return(funcall1_control_(ptr, &check, test, left, right, NULL));
		if (check != Nil) {
			*result = 1;
			return Result(ret, value);
		}
	}
	*result = 0;
	return Result(ret, left);
}

static int test_not_sublis_cons_(Execute ptr,
		addr alist, addr test, addr left, int *result, addr *ret)
{
	addr right, value, check;

	while (alist != Nil) {
		Return_getcons(alist, &right, &alist);
		if (right == Nil)
			continue;
		Return_getcons(right, &right, &value);
		Return(funcall1_control_(ptr, &check, test, left, right, NULL));
		if (check == Nil) {
			*result = 1;
			return Result(ret, value);
		}
	}
	*result = 0;
	return Result(ret, left);
}

static int replace_sublis_cons_(struct sublis_struct *str,
		addr tree, int *result, addr *ret)
{
	addr check;

	/* key */
	if (str->key != Nil) {
		Return(funcall1_control_(str->ptr, &check, str->key, tree, NULL));
	}
	else {
		check = tree;
	}

	/* test */
	switch (str->test) {
		case 0: /* nil */
			Return(default_sublis_cons_(str->alist, check, result, ret));
			break;

		case 1: /* :test */
			Return(test_sublis_cons_(str->ptr,
						str->alist, str->test1, check, result, ret));
			break;

		case 2: /* :test-not */
			Return(test_not_sublis_cons_(str->ptr,
						str->alist, str->test2, check, result, ret));
			break;

		default:
			return fmte_("Invalid test mode.", NULL);
	}
	if (*result == 0)
		*ret = tree;

	return 0;
}

static int recursive_sublis_cons_(struct sublis_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;
	LocalHold hold;

	/* atom */
	Return(replace_sublis_cons_(str, tree, &check, &tree));
	if (! consp(tree))
		return Result(ret, tree);

	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);

	/* car */
	Return(replace_sublis_cons_(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return(recursive_sublis_cons_(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return(replace_sublis_cons_(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return(recursive_sublis_cons_(str, cdr, &cdr));
		localhold_push(hold, cdr);
	}

	/* result */
	localhold_end(hold);
	cons_heap(ret, car, cdr);

	return 0;
}

static int argument_sublis_cons(Execute ptr,
		struct sublis_struct *str, addr alist, addr rest)
{
	addr key, test1, test2;

	clearpoint(str);
	if (rest == Nil) {
		key = test1 = test2 = Nil;
	}
	else {
		GetConst(KEYWORD_KEY, &key);
		if (getplist_safe(rest, key, &key))
			key = Nil;
		GetConst(KEYWORD_TEST, &test1);
		if (getplist_safe(rest, test1, &test1))
			test1 = Nil;
		GetConst(KEYWORD_TEST_NOT, &test2);
		if (getplist_safe(rest, test2, &test2))
			test2 = Nil;
		if (test1 != Nil && test2 != Nil)
			return 1;
	}

	/* recursive call */
	str->ptr = ptr;
	str->alist = alist;
	str->key = key;
	str->test1 = test1;
	str->test2 = test2;
	if (test1 == Nil && test2 == Nil)
		str->test = 0;
	else if (test1 != Nil)
		str->test = 1;
	else
		str->test = 2;

	return 0;
}

int sublis_common_(Execute ptr, addr alist, addr tree, addr rest, addr *ret)
{
	struct sublis_struct str;

	if (argument_sublis_cons(ptr, &str, alist, rest))
		return fmte_("SUBLIS don't accept both :test and :test-not parameter.", NULL);
	return recursive_sublis_cons_(&str, tree, ret);
}


/*
 *  nsublis
 */
static int recursive_nsublis_cons_(struct sublis_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;
	LocalHold hold;

	/* atom */
	Return(replace_sublis_cons_(str, tree, &check, &tree));
	if (! consp(tree))
		return Result(ret, tree);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return(replace_sublis_cons_(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return(recursive_nsublis_cons_(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return(replace_sublis_cons_(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return(recursive_nsublis_cons_(str, cdr, &cdr));
		localhold_push(hold, cdr);
	}

	/* result */
	localhold_end(hold);
	SetCons(tree, car, cdr);
	return Result(ret, tree);
}

int nsublis_common_(Execute ptr, addr alist, addr tree, addr rest, addr *ret)
{
	struct sublis_struct str;

	if (argument_sublis_cons(ptr, &str, alist, rest))
		return fmte_("NSUBLIS don't accept both :test and :test-not parameter.", NULL);
	return recursive_nsublis_cons_(&str, tree, ret);
}


/*
 *  subst
 */
struct subst_struct {
	Execute ptr;
	addr make, old, key, test1, test2;
	int test;
};

static int argument_subst_cons(Execute ptr,
		struct subst_struct *str, addr one, addr old, addr rest)
{
	addr key, test1, test2;

	clearpoint(str);
	if (rest == Nil) {
		key = test1 = test2 = Nil;
	}
	else {
		GetConst(KEYWORD_KEY, &key);
		if (getplist_safe(rest, key, &key))
			key = Nil;
		GetConst(KEYWORD_TEST, &test1);
		if (getplist_safe(rest, test1, &test1))
			test1 = Nil;
		GetConst(KEYWORD_TEST_NOT, &test2);
		if (getplist_safe(rest, test2, &test2))
			test2 = Nil;
		if (test1 != Nil && test2 != Nil)
			return 1;
	}

	str->ptr = ptr;
	str->make = one;
	str->old = old;
	str->key = key;
	str->test1 = test1;
	str->test2 = test2;
	if (test1 == Nil && test2 == Nil)
		str->test = 0;
	else if (test1 != Nil)
		str->test = 1;
	else
		str->test = 2;

	return 0;
}

static int default_subst_cons(struct subst_struct *str, addr tree, addr *ret)
{
	if (eql_function(str->old, tree)) {
		*ret = str->make;
		return 1;
	}
	else {
		*ret = tree;
		return 0;
	}
}

static int test_subst_cons_(struct subst_struct *str, addr tree, int *result, addr *ret)
{
	addr check;

	Return(funcall1_control_(str->ptr, &check, str->test1, str->old, tree, NULL));
	if (check != Nil) {
		*result = 1;
		*ret = str->make;
	}
	else {
		*result = 0;
		*ret = tree;
	}

	return 0;
}

static int test_not_subst_cons_(struct subst_struct *str,
		addr tree, int *result, addr *ret)
{
	addr check;

	Return(funcall1_control_(str->ptr, &check, str->test2, str->old, tree, NULL));
	if (check == Nil) {
		*result = 1;
		*ret = str->make;
	}
	else {
		*result = 0;
		*ret = tree;
	}

	return 0;
}

static int replace_subst_cons_(struct subst_struct *str,
		addr tree, int *result, addr *ret)
{
	addr check;

	/* key */
	if (str->key != Nil) {
		Return(funcall1_control_(str->ptr, &check, str->key, tree, NULL));
	}
	else {
		check = tree;
	}

	/* test */
	switch (str->test) {
		case 0: /* nil */
			*result = default_subst_cons(str, check, ret);
			break;

		case 1: /* :test */
			Return(test_subst_cons_(str, check, result, ret));
			break;

		case 2: /* :test-not */
			Return(test_not_subst_cons_(str, check, result, ret));
			break;

		default:
			return fmte_("Invalid test mode.", NULL);
	}
	if (*result == 0)
		*ret = tree;

	return 0;
}

static int recursive_subst_cons_(struct subst_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;
	LocalHold hold;

	/* atom */
	Return(replace_subst_cons_(str, tree, &check, &tree));
	if (! consp(tree))
		return Result(ret, tree);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return(replace_subst_cons_(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return(recursive_subst_cons_(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return(replace_subst_cons_(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return(recursive_subst_cons_(str, cdr, &cdr));
		localhold_push(hold, cdr);
	}

	/* result */
	localhold_end(hold);
	cons_heap(ret, car, cdr);

	return 0;
}

int subst_common_(Execute ptr, addr one, addr old, addr tree, addr key, addr *ret)
{
	struct subst_struct str;

	if (argument_subst_cons(ptr, &str, one, old, key))
		return fmte_("SUBST don't accept both :test and :test-not parameter.", NULL);
	return recursive_subst_cons_(&str, tree, ret);
}


/*
 *  nsubst
 */
static int recursive_nsubst_cons_(struct subst_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;
	LocalHold hold;

	/* atom */
	Return(replace_subst_cons_(str, tree, &check, &tree));
	if (! consp(tree))
		return Result(ret, tree);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return(replace_subst_cons_(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return(recursive_nsubst_cons_(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return(replace_subst_cons_(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return(recursive_nsubst_cons_(str, cdr, &cdr));
		localhold_push(hold, cdr);
	}

	/* result */
	localhold_end(hold);
	SetCons(tree, car, cdr);
	return Result(ret, tree);
}

int nsubst_common_(Execute ptr, addr one, addr old, addr tree, addr key, addr *ret)
{
	struct subst_struct str;

	if (argument_subst_cons(ptr, &str, one, old, key))
		return fmte_("NSUBST don't accept both :test and :test-not parameter.", NULL);
	return recursive_nsubst_cons_(&str, tree, ret);
}


/*
 *  subst-if
 */
static int argument_subst_if_cons(Execute ptr,
		struct subst_struct *str, addr one, addr test1, addr test2, addr rest)
{
	addr key;

	GetConst(KEYWORD_KEY, &key);
	if (getplist_safe(rest, key, &key))
		key = Nil;

	clearpoint(str);
	str->ptr = ptr;
	str->make = one;
	str->key = key;
	str->test1 = test1;
	str->test2 = test2;
	if (test1 != Nil)
		str->test = 1;
	else
		str->test = 2;

	return 0;
}

static int call_subst_if_cons_(struct subst_struct *str,
		addr tree, int *result, addr *ret)
{
	addr check;

	Return(funcall1_control_(str->ptr, &check, str->test1, tree, NULL));
	if (check != Nil) {
		*result = 1;
		*ret = str->make;
	}
	else {
		*result = 0;
		*ret = tree;
	}

	return 0;
}

static int call_subst_if_not_cons_(struct subst_struct *str,
		addr tree, int *result, addr *ret)
{
	addr check;

	Return(funcall1_control_(str->ptr, &check, str->test2, tree, NULL));
	if (check == Nil) {
		*result = 1;
		*ret = str->make;
	}
	else {
		*result = 0;
		*ret = tree;
	}

	return 0;
}

static int replace_subst_if_(struct subst_struct *str,
		addr tree, int *result, addr *ret)
{
	addr check;

	/* key */
	if (str->key != Nil) {
		Return(funcall1_control_(str->ptr, &check, str->key, tree, NULL));
	}
	else {
		check = tree;
	}

	/* test */
	switch (str->test) {
		case 1: /* :test */
			Return(call_subst_if_cons_(str, check, result, ret));
			break;

		case 2: /* :test-not */
			Return(call_subst_if_not_cons_(str, check, result, ret));
			break;

		default:
			return fmte_("Invalid test mode.", NULL);
	}
	if (*result == 0)
		*ret = tree;

	return 0;
}

static int recursive_subst_if_cons_(struct subst_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;
	LocalHold hold;

	/* atom */
	Return(replace_subst_if_(str, tree, &check, &tree));
	if (! consp(tree))
		return Result(ret, tree);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return(replace_subst_if_(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return(recursive_subst_if_cons_(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return(replace_subst_if_(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return(recursive_subst_if_cons_(str, cdr, &cdr));
		localhold_push(hold, cdr);
	}

	/* result */
	localhold_end(hold);
	cons_heap(ret, car, cdr);

	return 0;
}

int subst_if_common_(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret)
{
	struct subst_struct str;
	argument_subst_if_cons(ptr, &str, one, predicate, Nil, key);
	return recursive_subst_if_cons_(&str, tree, ret);
}


/*
 *  nsubst-if
 */
static int recursive_nsubst_if_cons_(struct subst_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;
	LocalHold hold;

	/* atom */
	Return(replace_subst_if_(str, tree, &check, &tree));
	if (! consp(tree))
		return Result(ret, tree);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return(replace_subst_if_(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return(recursive_nsubst_if_cons_(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return(replace_subst_if_(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return(recursive_nsubst_if_cons_(str, cdr, &cdr));
		localhold_push(hold, cdr);
	}

	/* result */
	localhold_end(hold);
	SetCons(tree, car, cdr);
	return Result(ret, tree);
}

int nsubst_if_common_(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret)
{
	struct subst_struct str;
	argument_subst_if_cons(ptr, &str, one, predicate, Nil, key);
	return recursive_nsubst_if_cons_(&str, tree, ret);
}


/*
 *  subst-if-not
 */
int subst_if_not_common_(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret)
{
	struct subst_struct str;
	argument_subst_if_cons(ptr, &str, one, Nil, predicate, key);
	return recursive_subst_if_cons_(&str, tree, ret);
}


/*
 *  nsubst-if-not
 */
int nsubst_if_not_common_(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret)
{
	struct subst_struct str;
	argument_subst_if_cons(ptr, &str, one, Nil, predicate, key);
	return recursive_nsubst_if_cons_(&str, tree, ret);
}


/*
 *  tree-equal
 */
struct tree_equal_struct {
	Execute ptr;
	addr test1, test2;
	int test;
};

static int argument_tree_equal_cons(Execute ptr,
		struct tree_equal_struct *str, addr rest)
{
	addr test1, test2;

	clearpoint(str);
	if (rest == Nil) {
		test1 = test2 = Nil;
	}
	else {
		GetConst(KEYWORD_TEST, &test1);
		if (getplist_safe(rest, test1, &test1))
			test1 = Nil;
		GetConst(KEYWORD_TEST_NOT, &test2);
		if (getplist_safe(rest, test2, &test2))
			test2 = Nil;
		if (test1 != Nil && test2 != Nil)
			return 1;
	}

	str->ptr = ptr;
	str->test1 = test1;
	str->test2 = test2;
	if (test1 == Nil && test2 == Nil)
		str->test = 0;
	else if (test1 != Nil)
		str->test = 1;
	else
		str->test = 2;

	return 0;
}

static int test_tree_equal_cons_(Execute ptr,
		int *result, addr test, addr left, addr right)
{
	Return(funcall1_control_(ptr, &test, test, left, right, NULL));
	return Result(result, (test != Nil));
}

static int test_not_tree_equal_cons_(Execute ptr,
		int *result, addr test, addr left, addr right)
{
	Return(funcall1_control_(ptr, &test, test, left, right, NULL));
	return Result(result, (test == Nil));
}

static int replace_tree_equal_cons_(struct tree_equal_struct *str,
		int *result, addr tree1, addr tree2)
{
	switch (str->test) {
		case 0: /* nil */
			*result = eql_function(tree1, tree2);
			return 0;

		case 1: /* :test */
			return test_tree_equal_cons_(str->ptr, result, str->test1, tree1, tree2);

		case 2: /* :test-not */
			return test_not_tree_equal_cons_(str->ptr, result, str->test2, tree1, tree2);

		default:
			return fmte_("Invalid test mode.", NULL);
	}
}

static int recursive_tree_equal_cons_(struct tree_equal_struct *str,
		int *result, addr tree1, addr tree2)
{
	int check, atom1, atom2;
	addr car1, cdr1, car2, cdr2;

	atom1 = atom_function(tree1);
	atom2 = atom_function(tree2);
	if (atom1 && atom2)
		return replace_tree_equal_cons_(str, result, tree1, tree2);
	if (atom1 || atom2)
		return Result(result, 0);

	/* cons */
	GetCons(tree1, &car1, &cdr1);
	GetCons(tree2, &car2, &cdr2);
	Return(recursive_tree_equal_cons_(str, &check, car1, car2));
	if (! check)
		return Result(result, 0);
	return recursive_tree_equal_cons_(str, result, cdr1, cdr2);
}

int tree_equal_common_(Execute ptr, addr tree1, addr tree2, addr key, int *ret)
{
	struct tree_equal_struct str;

	if (argument_tree_equal_cons(ptr, &str, key)) {
		return fmte_("TREE-EQUAL don't accept "
				"both :test and :test-not parameter.", NULL);
	}
	return recursive_tree_equal_cons_(&str, ret, tree1, tree2);
}


/*
 *  list-length
 */
static int index_list_length_cons_(addr list, size_t *rsize, int *ret)
{
	addr fast, slow, one;
	size_t size;

	slow = fast = list;
	size = 0;
	for (;;) {
		if (fast == Nil) {
			break;
		}
		Return_getcdr(fast, &one);
		if (one == Nil) {
			size++;
			break;
		}

		/* circular check */
		if (fast == slow && 0 < size)
			return Result(ret, 1);

		/* increment */
		size += 2;
		Return_getcdr(one, &fast);
		Return_getcdr(slow, &slow);
	}
	*rsize = size;
	return Result(ret, 0);
}

int list_length_common_(addr list, addr *ret)
{
	int check;
	size_t size;

	Return(index_list_length_cons_(list, &size, &check));
	if (check)
		return Result(ret, Nil);
	make_index_integer_heap(ret, size);
	return 0;
}


/*
 *  make-list
 */
int make_list_common_(addr var, addr rest, addr *ret)
{
	addr element, list;
	size_t size;

	/* argument */
	if (GetIndex_integer(var, &size))
		return fmte_("Too large index value ~S.", var, NULL);
	if (GetPlistConst(rest, KEYWORD_INITIAL_ELEMENT, &element))
		element = Nil;
	/* make-list */
	for (list = Nil; size--; )
		cons_heap(&list, element, list);
	/* result */
	return Result(ret, list);
}


/*
 *  push
 */
static int single_push_cons_(addr *ret,
		addr item, addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        (g (cons value r)))
	 *   (declare (ignorable a1 a2))
	 *   w g)
	 */
	addr list1, list2, leta, cons, declare, ignorable, args, x, y;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_CONS, &cons);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	/* (an bn) */
	list1 = a;
	list2 = b;
	args = Nil;
	while (list1 != Nil) {
		Return_getcons(list1, &x, &list1);
		Return_getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g (cons value r)) */
	Return_getcar(g, &g);
	list_heap(&cons, cons, item, r, NULL);
	list_heap(&x, g, cons, NULL);
	cons_heap(&args, x, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	nreverse(&args, args);
	list_heap(ret, leta, args, declare, w, g, NULL);

	return 0;
}

static int multiple_push_cons_(Execute ptr, addr *ret,
		addr item, addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((v value)
	 *        (a1 b1)
	 *        (a2 b2)
	 *        g1 g2 ...)
	 *   (declare (ignorable a1 a2))
	 *   (multiple-value-setq (g1 g2 ...) r)
	 *   (setq g1 (cons v g1))
	 *   (setq g2 (cons v g2))
	 *   ....
	 *   w
	 *   (values g1 g2 ...))
	 */
	addr leta, cons, declare, ignorable, mvsetq, setq, values;
	addr list1, list2, args, v, x, y, pos;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_CONS, &cons);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MULTIPLE_VALUE_SETQ, &mvsetq);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_VALUES, &values);
	/* (v value) */
	Return(make_gensym_(ptr, &v));
	list_heap(&args, v, item, NULL);
	conscar_heap(&args, args);
	/* (an bn) */
	list1 = a;
	list2 = b;
	while (list1 != Nil) {
		Return_getcons(list1, &x, &list1);
		Return_getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g1 g2 ...) */
	nreconc(&args, args, g);
	conscar_heap(&pos, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	cons_heap(&pos, declare, pos);
	/* (multiple-value-setq (g1 g2 ...) r */
	list_heap(&mvsetq, mvsetq, g, r, NULL);
	/* (setq g1 (cons v g1)) */
	for (list1 = g; list1 != Nil; ) {
		GetCons(list1, &x, &list1);
		list_heap(&y, cons, v, x, NULL);
		list_heap(&x, setq, x, y, NULL);
		cons_heap(&pos, x, pos);
	}
	/* w */
	cons_heap(&pos, w, pos);
	/* (values g1 g2 ...) */
	cons_heap(&values, values, g);
	cons_heap(&pos, values, pos);
	/* let* */
	nreverse(ret, pos);

	return 0;
}

static int expansion_push_cons_(Execute ptr, addr *ret, addr item, addr place, addr env)
{
	addr a, b, g, w, r;

	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	if (singlep(g))
		return single_push_cons_(ret, item, a, b, g, w, r);
	else
		return multiple_push_cons_(ptr, ret, item, a, b, g, w, r);
}

int push_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, item, place;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &item, &args))
		goto error;
	if (! consp_getcons(args, &place, &args))
		goto error;
	if (args != Nil)
		goto error;
	return expansion_push_cons_(ptr, ret, item, place, env);

error:
	*ret = Nil;
	return fmte_("PUSH argument ~S must be a (push item place) form.", form, NULL);
}


/*
 *  pop
 */
static int single_pop_cons_(Execute ptr, addr *ret,
		addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        (c r)
	 *        (g (cdr c)))
	 *   (declare (ignorable a1 a2))
	 *   w
	 *   (car c))
	 */
	addr list1, list2, leta, car, cdr, declare, ignorable, args, x, y, c;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_CAR, &car);
	GetConst(COMMON_CDR, &cdr);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	/* (an bn) */
	list1 = a;
	list2 = b;
	args = Nil;
	while (list1 != Nil) {
		Return_getcons(list1, &x, &list1);
		Return_getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (c r) */
	Return(make_gensym_(ptr, &c));
	list_heap(&x, c, r, NULL);
	cons_heap(&args, x, args);
	/* (g (cdr c)) */
	Return_getcar(g, &g);
	list_heap(&x, cdr, c, NULL);
	list_heap(&x, g, x, NULL);
	cons_heap(&args, x, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	nreverse(&args, args);
	list_heap(&x, car, c, NULL);
	list_heap(ret, leta, args, declare, w, x, NULL);

	return 0;
}

static int multiple_pop_cons_(Execute ptr, addr *ret,
		addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        g1 g2 ...)
	 *   (declare (ignorable a1 a2 ...))
	 *   (multiple-value-bind (r1 r2 ...) r
	 *     (setq g1 (cdr r1))
	 *     (setq g2 (cdr r2))
	 *     ...)
	 *   w
	 *   (values (car r1) (car r2) ...))
	 */
	addr leta, declare, ignorable, mvbind, setq, car, cdr, values;
	addr list1, list2, args, x, y, pos, c;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(COMMON_CAR, &car);
	GetConst(COMMON_CDR, &cdr);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_VALUES, &values);
	/* (an bn) */
	list1 = a;
	list2 = b;
	args = Nil;
	while (list1 != Nil) {
		Return_getcons(list1, &x, &list1);
		Return_getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g1 g2 ...) */
	nreconc(&args, args, g);
	conscar_heap(&pos, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	cons_heap(&pos, declare, pos);
	/* (multiple-value-bind (r1 r2 ...) r
	 *   (setq g1 (cdr r1)) ...)  */
	c = Nil;
	for (list1 = g; list1 != Nil; ) {
		GetCons(list1, &x, &list1);
		Return(make_gensym_(ptr, &y));
		cons_heap(&c, y, c);
		list_heap(&y, cdr, y, NULL);
		list_heap(&x, setq, x, y, NULL);
		cons_heap(&args, x, args);
	}
	nreverse(&c, c);
	nreverse(&args, args);
	conscar_heap(&args, mvbind);
	cons_heap(&args, c, args);
	cons_heap(&args, r, args);
	cons_heap(&pos, args, pos);
	/* w */
	cons_heap(&pos, w, pos);
	/* (values (car r1) (car r2) ...) */
	args = Nil;
	for (list1 = c; list1 != Nil; ) {
		GetCons(list1, &x, &list1);
		list_heap(&x, car, x, NULL);
		cons_heap(&args, x, args);
	}
	nreverse(&args, args);
	cons_heap(&values, values, args);
	cons_heap(&pos, values, pos);
	/* let* */
	nreverse(ret, pos);

	return 0;
}

static int expansion_pop_cons_(Execute ptr, addr *ret, addr place, addr env)
{
	addr a, b, g, w, r;

	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	if (singlep(g))
		return single_pop_cons_(ptr, ret, a, b, g, w, r);
	else
		return multiple_pop_cons_(ptr, ret, a, b, g, w, r);
}

int pop_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, place;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &place, &args))
		goto error;
	if (args != Nil)
		goto error;
	return expansion_pop_cons_(ptr, ret, place, env);

error:
	*ret = Nil;
	return fmte_("POP argument ~S must be a (pop place) form.", form, NULL);
}


/*
 *  nth
 */
int nth_common_(addr index, addr list, addr *ret)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return getnth_large_(list, index, ret);
	else
		return getnth_(list, size, ret);
}


/*
 *  (setf nth)
 */
int setf_nth_common_(addr value, addr index, addr list)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return fmte_("Too large index value ~S.", index, NULL);
	return setnth_(list, size, value);
}


/*
 *  nthcdr
 */
int nthcdr_common_(addr index, addr list, addr *ret)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return getnthcdr_large_(list, index, ret);
	else
		return getnthcdr_(list, size, ret);
}


/*
 *  member
 */
static int test_member_cons_(Execute ptr, addr *ret,
		addr item, addr list, addr key, addr call, int notret)
{
	int check;
	addr value, next;

	while (list != Nil) {
		if (! consp_getcons(list, &value, &next))
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		Return(function_call_cons_(ptr, &check, item, key, call, value, notret));
		if (check)
			return Result(ret, list);
		list = next;
	}

	return Result(ret, Nil);
}

int member_common_(Execute ptr, addr item, addr list, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		return fmte_("MEMBER don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_member_cons_(ptr, ret, item, list, key, testnot, 1);
	else if (check1)
		return test_member_cons_(ptr, ret, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_member_cons_(ptr, ret, item, list, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  member-if
 */
int member_if_common_(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, next;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	while (list != Nil) {
		if (! consp_getcons(list, &value, &next))
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		Return(function_if_call_cons_(ptr, &check, key, call, value));
		if (check)
			return Result(ret, list);
		list = next;
	}

	return Result(ret, Nil);
}


/*
 *  member-if-not
 */
int member_if_not_common_(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, next;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	while (list != Nil) {
		if (! consp_getcons(list, &value, &next))
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		Return(function_if_call_cons_(ptr, &check, key, call, value));
		if (! check)
			return Result(ret, list);
		list = next;
	}

	return Result(ret, Nil);
}


/*
 *  mapc
 */
int mapc_common_(Execute ptr, addr call, addr rest, addr *ret)
{
	addr result, pos, car, cdr, args, next, temp1, temp2;
	LocalRoot local;
	LocalStack stack;

	GetCar(rest, &result);
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) {
		*ret = Nil;
		return fmte_("Too few MAPC arguments.", NULL);
	}
	args = next = Nil;
	while (rest != Nil) {
		Return_getcons(rest, &pos, &rest);
		if (pos == Nil)
			goto finish;
		Return_getcons(pos, &car, &cdr);
		cons_local(local, &args, car, args);
		cons_local(local, &next, cdr, next);
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	Return(apply1_control_(ptr, &pos, call, args));

	/* second */
	for (;;) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil)
				goto finish;
			Return_getcons(cdr, &car, &cdr);
			SetCar(temp1, car);
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		Return(apply1_control_(ptr, &pos, call, args));
	}

finish:
	rollback_local(local, stack);
	return Result(ret, result);
}


/*
 *  mapcar
 */
int mapcar_common_(Execute ptr, addr call, addr rest, addr *ret)
{
	addr result, pos, car, cdr, args, next, temp1, temp2, hold;
	LocalRoot local;
	LocalStack stack;

	result = Nil;
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) {
		*ret = Nil;
		return fmte_("Too few MAPCAR arguments.", NULL);
	}
	args = next = Nil;
	while (rest != Nil) {
		Return_getcons(rest, &pos, &rest);
		if (pos == Nil)
			goto finish;
		Return_getcons(pos, &car, &cdr);
		cons_local(local, &args, car, args);
		cons_local(local, &next, cdr, next);
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	Return(apply1_control_(ptr, &pos, call, args));
	cons_heap(&result, pos, result);
	gchold_local(local, &hold, 1);
	setgchold(hold, 0, result);

	/* second */
	for (;;) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil)
				goto finish;
			Return_getcons(cdr, &car, &cdr);
			SetCar(temp1, car);
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		Return(apply1_control_(ptr, &pos, call, args));
		cons_heap(&result, pos, result);
		setgchold(hold, 0, result);
	}

finish:
	rollback_local(local, stack);
	nreverse(ret, result);

	return 0;
}


/*
 *  mapcan
 */
int mapcan_common_(Execute ptr, addr call, addr rest, addr *ret)
{
	addr result, pos, car, cdr, args, next, temp1, temp2, head, hold;
	LocalRoot local;
	LocalStack stack;

	result = Nil;
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) {
		*ret = Nil;
		return fmte_("Too few MAPCAN arguments.", NULL);
	}
	args = next = Nil;
	while (rest != Nil) {
		Return_getcons(rest, &pos, &rest);
		if (pos == Nil)
			goto finish;
		Return_getcons(pos, &car, &cdr);
		cons_local(local, &args, car, args);
		cons_local(local, &next, cdr, next);
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	Return(apply1_control_(ptr, &head, call, args));
	result = head;
	gchold_local(local, &hold, 1);
	setgchold(hold, 0, result);

	/* second */
	for (;;) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil)
				goto finish;
			Return_getcons(cdr, &car, &cdr);
			SetCar(temp1, car);
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		Return(apply1_control_(ptr, &pos, call, args));
		/* nconc */
		if (pos != Nil) {
			if (result == Nil) {
				result = head = pos;
				gchold_local(local, &hold, 1);
				setgchold(hold, 0, result);
			}
			else {
				Return(setlastcdr_safe_(head, pos));
				head = pos;
			}
		}
	}

finish:
	rollback_local(local, stack);
	return Result(ret, result);
}


/*
 *  mapl
 */
int mapl_common_(Execute ptr, addr call, addr rest, addr *ret)
{
	int loop;
	addr result, pos, cdr, args, next, temp1, temp2;
	LocalRoot local;
	LocalStack stack;

	GetCar(rest, &result);
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) {
		*ret = Nil;
		return fmte_("Too few MAPL arguments.", NULL);
	}
	args = next = Nil;
	loop = 1;
	while (rest != Nil) {
		Return_getcons(rest, &pos, &rest);
		if (pos == Nil)
			goto finish;
		Return_getcdr(pos, &cdr);
		cons_local(local, &args, pos, args);
		cons_local(local, &next, cdr, next);
		if (cdr == Nil)
			loop = 0;
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	Return(apply1_control_(ptr, &pos, call, args));

	/* second */
	while (loop) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			SetCar(temp1, cdr);
			GetCdr(cdr, &cdr);
			if (cdr == Nil)
				loop = 0;
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		Return(apply1_control_(ptr, &pos, call, args));
	}

finish:
	rollback_local(local, stack);
	return Result(ret, result);
}


/*
 *  maplist
 */
int maplist_common_(Execute ptr, addr call, addr rest, addr *ret)
{
	int loop;
	addr result, pos, cdr, args, next, temp1, temp2, hold;
	LocalRoot local;
	LocalStack stack;

	result = Nil;
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) {
		*ret = Nil;
		return fmte_("Too few MAPLIST arguments.", NULL);
	}
	args = next = Nil;
	loop = 1;
	while (rest != Nil) {
		Return_getcons(rest, &pos, &rest);
		if (pos == Nil)
			goto finish;
		Return_getcdr(pos, &cdr);
		cons_local(local, &args, pos, args);
		cons_local(local, &next, cdr, next);
		if (cdr == Nil)
			loop = 0;
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	Return(apply1_control_(ptr, &pos, call, args));
	cons_heap(&result, pos, result);
	gchold_local(local, &hold, 1);
	setgchold(hold, 0, result);

	/* second */
	while (loop) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			SetCar(temp1, cdr);
			GetCdr(cdr, &cdr);
			if (cdr == Nil)
				loop = 0;
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		Return(apply1_control_(ptr, &pos, call, args));
		cons_heap(&result, pos, result);
		setgchold(hold, 0, result);
	}

finish:
	rollback_local(local, stack);
	nreverse(ret, result);

	return 0;
}


/*
 *  mapcon
 */
int mapcon_common_(Execute ptr, addr call, addr rest, addr *ret)
{
	int loop;
	addr result, pos, cdr, args, next, temp1, temp2, head, hold;
	LocalRoot local;
	LocalStack stack;

	result = Nil;
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) {
		*ret = Nil;
		return fmte_("Too few MAPCON arguments.", NULL);
	}
	args = next = Nil;
	loop = 1;
	while (rest != Nil) {
		Return_getcons(rest, &pos, &rest);
		if (pos == Nil)
			goto finish;
		Return_getcdr(pos, &cdr);
		cons_local(local, &args, pos, args);
		cons_local(local, &next, cdr, next);
		if (cdr == Nil)
			loop = 0;
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	Return(apply1_control_(ptr, &head, call, args));
	result = head;
	gchold_local(local, &hold, 1);
	setgchold(hold, 0, result);

	/* second */
	while (loop) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			SetCar(temp1, cdr);
			GetCdr(cdr, &cdr);
			if (cdr == Nil)
				loop = 0;
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		Return(apply1_control_(ptr, &pos, call, args));
		/* nconc */
		if (pos != Nil) {
			if (result == Nil) {
				result = head = pos;
				gchold_local(local, &hold, 1);
				setgchold(hold, 0, result);
			}
			else {
				Return(setlastcdr_safe_(head, pos));
				head = pos;
			}
		}
	}

finish:
	rollback_local(local, stack);
	return Result(ret, result);
}


/*
 *  nconc
 */
static int concat_nconc_cons_(addr list, addr args)
{
	addr last;

	last = list;
	for (;;) {
		/* update lastcdr */
		do {
			last = list;
			GetCdr(list, &list);
		}
		while (IsCons(list));
		/* ignore (cdr list) */

		for (;;) {
			Return_getcons(args, &list, &args);
			if (! IsCons(args)) {
				SetCdr(last, list);
				return 0;
			}
			if (list == Nil)
				continue;
			if (IsCons(list)) {
				SetCdr(last, list);
				break;
			}
			return fmte_("nconc argument ~S must be a list.", list, NULL);
		}
	}

	return 0;
}

int nconc_common_(addr args, addr *ret)
{
	addr pos;

	/* (nconc) */
	if (args == Nil)
		return Result(ret, Nil);

	/* (nconc object) */
	for (;;) {
		Return_getcons(args, &pos, &args);
		if (args == Nil)
			return Result(ret, pos);
		if (pos == Nil)
			continue;
		if (IsCons(pos))
			break;
		return fmte_("nconc argument ~S must be a list.", pos, NULL);
	}

	/* (nconc x x ...) */
	Return(concat_nconc_cons_(pos, args));
	return Result(ret, pos);
}


/*
 *  append
 */
static int push_append_cons_(addr root, addr last, addr *ret)
{
	addr pos;

	if (! IsCons(last))
		return fmte_("The argument ~S must be a list.", last, NULL);
	while (last != Nil) {
		Return_getcons(last, &pos, &last);
		cons_heap(&root, pos, root);
	}
	return Result(ret, root);
}

static int concat_append_cons_(addr last, addr args, addr *ret)
{
	addr pos, root;

	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &pos, &args);
		if (args == Nil) {
			if (pos != Nil) {
				Return(push_append_cons_(root, last, &root));
				last = pos;
			}
			break;
		}
		if (pos == Nil) {
			continue;
		}
		Return(push_append_cons_(root, last, &root));
		last = pos;
	}
	nreconc(ret, root, last);
	return 0;
}

int append_common_(addr args, addr *ret)
{
	addr pos;

	/* (append) */
	if (args == Nil)
		return Result(ret, Nil);

	/* (append object) */
	for (;;) {
		Return_getcons(args, &pos, &args);
		if (args == Nil)
			return Result(ret, pos);
		if (pos == Nil)
			continue;
		if (IsCons(pos))
			break;
		return fmte_("append argument ~S must be a list.", pos, NULL);
	}

	/* (append x x ...) */
	Return(concat_append_cons_(pos, args, &pos));
	return Result(ret, pos);
}


/*
 *  revappend
 */
int revappend_common_(addr list, addr tail, addr *ret)
{
	addr pos;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		cons_heap(&tail, pos, tail);
	}

	return Result(ret, tail);
}


/*
 *  nreconc
 */
int nreconc_common_(addr list, addr tail, addr *ret)
{
	addr next;

	/* nil */
	if (list == Nil)
		return Result(ret, tail);

	/* loop */
	for (;;) {
		Return_getcdr(list, &next);
		Return_setcdr(list, tail);
		if (next == Nil)
			break;
		tail = list;
		list = next;
	}

	return Result(ret, list);
}


/*
 *  butlast
 */
static int index_butlast_cons_(addr list, size_t index, addr *ret)
{
	addr root, pos;
	size_t size;

	length_list_p(list, &size);
	if (size <= index)
		return Result(ret, Nil);
	size -= index;
	for (root = Nil; size--; ) {
		GetCons(list, &pos, &list);
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);
	return 0;
}

static int large_butlast_cons_(addr list, addr index, addr *ret)
{
	int check;
	addr size;
	size_t value;

	length_list_p(list, &value);
	size = intsizeh(value);
	Return(less_equal_integer_(size, index, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Too large butlast index ~S.", index, NULL);
	}

	return Result(ret, Nil);
}

int butlast_common_(addr list, addr index, addr *ret)
{
	size_t size;

	if (index == Unbound)
		return index_butlast_cons_(list, 1, ret);
	if (GetIndex_integer(index, &size))
		return large_butlast_cons_(list, index, ret);
	else
		return index_butlast_cons_(list, size, ret);
}


/*
 *  nbutlast
 */
static int index_nbutlast_cons_(addr list, size_t index, addr *ret)
{
	size_t size;

	length_list_p(list, &size);
	if (size <= index)
		return Result(ret, Nil);
	size -= index + 1;
	while (size--)
		GetCdr(list, &list);
	SetCdr(list, Nil);

	return 0;
}

static int large_nbutlast_cons_(addr list, addr index, addr *ret)
{
	int check;
	addr size;
	size_t value;

	length_list_p(list, &value);
	size = intsizeh(value);
	Return(less_equal_integer_(size, index, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Too large nbutlast index ~S.", index, NULL);
	}

	return Result(ret, Nil);
}

int nbutlast_common_(addr list, addr index, addr *ret)
{
	size_t size;

	if (index == Unbound)
		return index_nbutlast_cons_(list, 1, ret);
	if (GetIndex_integer(index, &size))
		return large_nbutlast_cons_(list, index, ret);
	else
		return index_nbutlast_cons_(list, size, ret);
}


/*
 *  last
 */
static int index_last_cons_(addr list, size_t index, addr *ret)
{
	size_t size;

	length_list_p(list, &size);
	if (size < index)
		return Result(ret, list);
	size -= index;
	while (size--) {
		Return_getcdr(list, &list);
	}

	return Result(ret, list);
}

static int large_last_cons_(addr list, addr index, addr *ret)
{
	int check;
	addr size;
	size_t value;

	length_list_p(list, &value);
	size = intsizeh(value);
	Return(less_equal_integer_(size, index, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Too large nbutlast index ~S.", index, NULL);
	}

	return Result(ret, list);
}

int last_common_(addr list, addr index, addr *ret)
{
	size_t size;

	if (index == Unbound)
		return index_last_cons_(list, 1, ret);
	if (GetIndex_integer(index, &size))
		return large_last_cons_(list, index, ret);
	else
		return index_last_cons_(list, size, ret);
}


/*
 *  ldiff
 */
void ldiff_common(addr list, addr object, addr *ret)
{
	addr root, pos;

	root = Nil;
	for (;;) {
		if (list == object) {
			list = Nil;
			break;
		}
		if (GetType(list) != LISPTYPE_CONS) {
			break;
		}
		GetCons(list, &pos, &list);
		cons_heap(&root, pos, root);
	}
	nreconc(ret, root, list);
}


/*
 *  tailp
 */
void tailp_common(addr object, addr list, int *ret)
{
	int check;

	for (;;) {
		if (list == object) {
			check = 1;
			break;
		}
		if (GetType(list) != LISPTYPE_CONS) {
			check = 0;
			break;
		}
		GetCdr(list, &list);
	}
	*ret = check;
}


/*
 *  assoc
 */
static int test_assoc_cons_(Execute ptr, addr *ret,
		addr item, addr list, addr key, addr call, int notret)
{
	int check;
	addr cons, value;

	while (list != Nil) {
		if (! consp_getcons(list, &cons, &list))
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		if (cons == Nil)
			continue;
		Return_getcar(cons, &value);
		Return(function_call_cons_(ptr, &check, item, key, call, value, notret));
		if (check)
			return Result(ret, cons);
	}

	return Result(ret, Nil);
}

int assoc_common_(Execute ptr, addr item, addr list, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		return fmte_("ASSOC don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_assoc_cons_(ptr, ret, item, list, key, testnot, 1);
	else if (check1)
		return test_assoc_cons_(ptr, ret, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_assoc_cons_(ptr, ret, item, list, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  assoc-if
 */
int assoc_if_common_(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, cons;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	while (list != Nil) {
		if (! consp_getcons(list, &cons, &list))
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		if (cons == Nil)
			continue;
		Return_getcar(cons, &value);
		Return(function_if_call_cons_(ptr, &check, key, call, value));
		if (check)
			return Result(ret, cons);
	}

	return Result(ret, Nil);
}


/*
 *  assoc-if-not
 */
int assoc_if_not_common_(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, cons;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	while (list != Nil) {
		if (! consp_getcons(list, &cons, &list))
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		if (cons == Nil)
			continue;
		Return_getcar(cons, &value);
		Return(function_if_call_cons_(ptr, &check, key, call, value));
		if (! check)
			return Result(ret, cons);
	}

	return Result(ret, Nil);
}


/*
 *  copy-alist
 */
int copy_alist_common_(addr list, addr *ret)
{
	addr root, cons, car, cdr;

	for (root = Nil; list != Nil; ) {
		Return_getcons(list, &cons, &list);
		if (cons != Nil) {
			Return_getcons(cons, &car, &cdr);
			cons_heap(&cons, car, cdr);
		}
		cons_heap(&root, cons, root);
	}
	nreverse(ret, root);

	return 0;
}


/*
 *  pairlis
 */
int pairlis_common_(addr keys, addr data, addr list, addr *ret)
{
	int check1, check2;
	addr car, cdr;

	if (list == Unbound)
		list = Nil;
	for (;;) {
		check1 = (keys == Nil);
		check2 = (data == Nil);
		if (check1 && check2)
			break;
		if (check1 || check2)
			return fmte_("The length of keys isn't equal to the data.", NULL);
		Return_getcons(keys, &car, &keys);
		Return_getcons(data, &cdr, &data);
		cons_heap(&cdr, car, cdr);
		cons_heap(&list, cdr, list);
	}

	return Result(ret, list);
}


/*
 *  rassoc
 */
static int test_rassoc_cons_(Execute ptr, addr *ret,
		addr item, addr list, addr key, addr call, int notret)
{
	int check;
	addr cons, value;

	while (list != Nil) {
		if (! consp_getcons(list, &cons, &list))
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		if (cons == Nil)
			continue;
		Return_getcdr(cons, &value);
		Return(function_call_cons_(ptr, &check, item, key, call, value, notret));
		if (check)
			return Result(ret, cons);
	}

	return Result(ret, Nil);
}

int rassoc_common_(Execute ptr, addr item, addr list, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		return fmte_("RASSOC don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_rassoc_cons_(ptr, ret, item, list, key, testnot, 1);
	else if (check1)
		return test_rassoc_cons_(ptr, ret, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_rassoc_cons_(ptr, ret, item, list, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  rassoc-if
 */
int rassoc_if_common_(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, cons;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	while (list != Nil) {
		if (! consp_getcons(list, &cons, &list))
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		if (cons == Nil)
			continue;
		Return_getcdr(cons, &value);
		Return(function_if_call_cons_(ptr, &check, key, call, value));
		if (check)
			return Result(ret, cons);
	}

	return Result(ret, Nil);
}


/*
 *  rssoc-if-not
 */
int rassoc_if_not_common_(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, cons;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	while (list != Nil) {
		if (! consp_getcons(list, &cons, &list))
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		if (cons == Nil)
			continue;
		Return_getcdr(cons, &value);
		Return(function_if_call_cons_(ptr, &check, key, call, value));
		if (! check)
			return Result(ret, cons);
	}

	return Result(ret, Nil);
}


/*
 *  get-properties
 */
int get_properties_common_(addr plist, addr indicator,
		addr *rkey, addr *rvalue, addr *rlist)
{
	addr key, value, next, list, check;

	while (plist != Nil) {
		Return_getcons(plist, &key, &next);
		if (! consp_getcons(next, &value, &next)) {
			*rkey = *rvalue = *rlist = Nil;
			return fmte_("The proper list ~S must be a cons object.", next, NULL);
		}
		for (list = indicator; list != Nil; ) {
			Return_getcons(list, &check, &list);
			if (check == key)
				goto find;
		}
		plist = next;
	}
	*rkey = *rvalue = *rlist = Nil;
	return 0;

find:
	*rkey = key;
	*rvalue = value;
	*rlist = plist;
	return 0;
}


/*
 *  getf
 */
int getf_common_(addr list, addr key, addr value, addr *ret)
{
	addr x, y;

	while (list != Nil) {
		Return_getcons(list, &x, &list);
		if (! consp_getcons(list, &y, &list)) {
			*ret = Nil;
			return fmte_("The proper list ~S must be a cons object.", list, NULL);
		}
		if (x == key)
			return Result(ret, y);
	}

	return Result(ret, value == Unbound? Nil: value);
}


/*
 *  (setf remf)
 */
static int expansion_remf_cons_(Execute ptr, addr *ret,
		addr place, addr indicator, addr env)
{
	/* (let* ((a1 b1)
	 *        (a2 b2))
	 *   (declare (ignorable a1 a2))
	 *   (multiple-value-bind (g c) (remlist indicator r)
	 *     w c))
	 */
	addr list1, list2, args, x, y, c;
	addr leta, remplist, declare, ignorable, mvbind;
	addr a, b, g, w, r;

	/* get-setf-expansion */
	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	/* macro */
	GetConst(COMMON_LETA, &leta);
	GetConst(SYSTEM_REMPLIST, &remplist);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	/* (an bn) */
	list1 = a;
	list2 = b;
	args = Nil;
	while (list1 != Nil) {
		GetCons(list1, &x, &list1);
		GetCons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* (multiple-value-bind (g c) (remplist indicator r) w c) */
	Return_getcar(g, &g);
	Return(make_gensym_(ptr, &c));
	list_heap(&g, g, c, NULL);
	list_heap(&remplist, remplist, indicator, r, NULL);
	list_heap(&mvbind, mvbind, g, remplist, w, c, NULL);
	/* let* */
	nreverse(&args, args);
	list_heap(ret, leta, args, declare, mvbind, NULL);

	return 0;
}

int remf_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, place, indicator;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &place, &args))
		goto error;
	if (! consp_getcons(args, &indicator, &args))
		goto error;
	if (args != Nil)
		goto error;
	return expansion_remf_cons_(ptr, ret, place, indicator, env);

error:
	*ret = Nil;
	return fmte_("REMF argument ~S must be a (place indicator) form.", form, NULL);
}


/*
 *  intersection
 */
static int check_intersection_cons_(Execute ptr, int *ret,
		addr left, addr list, addr key, addr test, int notret)
{
	int check;
	addr right;

	if (key != Nil) {
		Return(funcall1_control_(ptr, &left, key, left, NULL));
	}
	while (list != Nil) {
		Return_getcons(list, &right, &list);
		Return(function_call_cons_(ptr, &check, left, key, test, right, notret));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int test_intersection_cons_(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, left;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (list = Nil; list1 != Nil; ) {
		Return_getcons(list1, &left, &list1);
		Return(check_intersection_cons_(ptr, &check, left, list2, key, test, notret));
		if (check) {
			cons_heap(&list, left, list);
			localhold_set(hold, 0, list);
		}
	}
	localhold_end(hold);
	return Result(ret, list);
}

int intersection_common_(Execute ptr, addr list1, addr list2, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("INTERSECTION don't accept "
				"both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_intersection_cons_(ptr, ret, list1, list2, key, testnot, 1);
	else if (check1)
		return test_intersection_cons_(ptr, ret, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_intersection_cons_(ptr, ret, list1, list2, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  rintersection
 */
static int test_nintersection_cons_(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, pos, x, y, z;

	if (list1 == Nil)
		return Result(ret, Nil);
	if (list2 == Nil)
		return Result(ret, Nil);

	/* first */
	list = list1;
	for (;;) {
		Return_getcons(list, &pos, &x);
		Return(check_intersection_cons_(ptr, &check, pos, list2, key, test, notret));
		if (check)
			break;
		if (x == Nil) {
			list = Nil;
			goto finish;
		}
		list = x;
	}
	list1 = x;

	/* tail */
	z = list;
	while (list1 != Nil) {
		Return_getcons(list1, &pos, &y);
		Return(check_intersection_cons_(ptr, &check, pos, list2, key, test, notret));
		if (! check) {
			list1 = y;
			continue;
		}
		if (list1 == x) {
			z = x;
			list1 = x = y;
			continue;
		}
		GetCar(list1, &pos);
		SetCar(x, pos);
		z = x;
		GetCdr(x, &x);
		list1 = y;
	}
	SetCdr(z, Nil);

finish:
	return Result(ret, list);
}

int nintersection_common_(Execute ptr, addr list1, addr list2, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("NINTERSECTION don't accept "
				"both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_nintersection_cons_(ptr, ret, list1, list2, key, testnot, 1);
	else if (check1)
		return test_nintersection_cons_(ptr, ret, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_nintersection_cons_(ptr, ret, list1, list2, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  adjoin
 */
static int test_adjoin_cons_(Execute ptr, addr *ret,
		addr left, addr list, addr key, addr test, int notret)
{
	int check;
	addr find, item, right;

	/* item */
	if (key != Nil) {
		Return(funcall1_control_(ptr, &item, key, left, NULL));
	}
	else {
		item = left;
	}

	/* adjoin */
	for (find = list; find != Nil; ) {
		Return_getcons(find, &right, &find);
		Return(function_call_cons_(ptr, &check, item, key, test, right, notret));
		if (check)
			return Result(ret, list);
	}
	cons_heap(ret, left, list);
	return 0;
}

int adjoin_common_(Execute ptr, addr item, addr list, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		return fmte_("ADJOIN don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_adjoin_cons_(ptr, ret, item, list, key, testnot, 1);
	else if (check1)
		return test_adjoin_cons_(ptr, ret, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_adjoin_cons_(ptr, ret, item, list, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  pushnew
 */
static int single_pushnew_cons_(addr *ret,
		addr item, addr rest, addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        (g (adjoin value r . rest)))
	 *   (declare (ignorable a1 a2))
	 *   w g)
	 */
	addr list1, list2, leta, adjoin, declare, ignorable, args, x, y;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_ADJOIN, &adjoin);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	/* (an bn) */
	list1 = a;
	list2 = b;
	args = Nil;
	while (list1 != Nil) {
		GetCons(list1, &x, &list1);
		GetCons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g (adjoin value r . rest)) */
	Return_getcar(g, &g);
	lista_heap(&adjoin, adjoin, item, r, rest, NULL);
	list_heap(&x, g, adjoin, NULL);
	cons_heap(&args, x, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	nreverse(&args, args);
	list_heap(ret, leta, args, declare, w, g, NULL);

	return 0;
}

static int multiple_pushnew_cons_(Execute ptr, addr *ret,
		addr item, addr rest, addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((v value)
	 *        (a1 b1)
	 *        (a2 b2)
	 *        g1 g2 ...)
	 *   (declare (ignorable a1 a2))
	 *   (multiple-value-setq (g1 g2 ...) r)
	 *   (setq g1 (adjoin v g1 . rest))
	 *   (setq g2 (adjoin v g2 . rest))
	 *   ....
	 *   w
	 *   (values g1 g2 ...))
	 */
	addr leta, adjoin, declare, ignorable, mvsetq, setq, values;
	addr list1, list2, args, v, x, y, pos;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_ADJOIN, &adjoin);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MULTIPLE_VALUE_SETQ, &mvsetq);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_VALUES, &values);
	/* (v value) */
	Return(make_gensym_(ptr, &v));
	list_heap(&args, v, item, NULL);
	conscar_heap(&args, args);
	/* (an bn) */
	list1 = a;
	list2 = b;
	while (list1 != Nil) {
		GetCons(list1, &x, &list1);
		GetCons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g1 g2 ...) */
	nreconc(&args, args, g);
	conscar_heap(&pos, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	cons_heap(&pos, declare, pos);
	/* (multiple-value-setq (g1 g2 ...) r */
	list_heap(&mvsetq, mvsetq, g, r, NULL);
	/* (setq g1 (adjoin v g1)) */
	for (list1 = g; list1 != Nil; ) {
		GetCons(list1, &x, &list1);
		lista_heap(&y, adjoin, v, x, rest, NULL);
		list_heap(&x, setq, x, y, NULL);
		cons_heap(&pos, x, pos);
	}
	/* w */
	cons_heap(&pos, w, pos);
	/* (values g1 g2 ...) */
	cons_heap(&values, values, g);
	cons_heap(&pos, values, pos);
	/* let* */
	nreverse(ret, pos);

	return 0;
}

static int expansion_pushnew_cons_(Execute ptr, addr *ret,
		addr item, addr place, addr rest, addr env)
{
	addr a, b, g, w, r;

	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	if (singlep(g))
		return single_pushnew_cons_(ret, item, rest, a, b, g, w, r);
	else
		return multiple_pushnew_cons_(ptr, ret, item, rest, a, b, g, w, r);
}

int pushnew_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, item, place;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &item, &args))
		goto error;
	if (! consp_getcons(args, &place, &args))
		goto error;
	return expansion_pushnew_cons_(ptr, ret, item, place, args, env);

error:
	*ret = Nil;
	return fmte_("PUSH argument ~S "
			"must be a (item place &rest args) form.", form, NULL);
}


/*
 *  set-difference
 */
static int test_set_difference_cons_(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, left;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (list = Nil; list1 != Nil; ) {
		Return_getcons(list1, &left, &list1);
		Return(check_intersection_cons_(ptr, &check, left, list2, key, test, notret));
		if (! check) {
			cons_heap(&list, left, list);
			localhold_set(hold, 0, list);
		}
	}
	localhold_end(hold);
	return Result(ret, list);
}

int set_difference_common_(Execute ptr, addr a, addr b, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("SET-DIFFERENCE don't accept "
				"both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_set_difference_cons_(ptr, ret, a, b, key, testnot, 1);
	else if (check1)
		return test_set_difference_cons_(ptr, ret, a, b, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_set_difference_cons_(ptr, ret, a, b, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  nset-difference
 */
static int test_nset_difference_cons_(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, pos, x, y, z;

	if (list1 == Nil)
		return Result(ret, Nil);
	if (list2 == Nil)
		return Result(ret, list1);

	/* first */
	list = list1;
	for (;;) {
		Return_getcons(list, &pos, &x);
		Return(check_intersection_cons_(ptr, &check, pos, list2, key, test, notret));
		if (! check)
			break;
		if (x == Nil) {
			list = Nil;
			goto finish;
		}
		list = x;
	}
	list1 = x;

	/* tail */
	z = list;
	while (list1 != Nil) {
		Return_getcons(list1, &pos, &y);
		Return(check_intersection_cons_(ptr, &check, pos, list2, key, test, notret));
		if (check) {
			list1 = y;
			continue;
		}
		if (list1 == x) {
			z = x;
			list1 = x = y;
			continue;
		}
		GetCar(list1, &pos);
		SetCar(x, pos);
		z = x;
		GetCdr(x, &x);
		list1 = y;
	}
	SetCdr(z, Nil);

finish:
	return Result(ret, list);
}

int nset_difference_common_(Execute ptr, addr a, addr b, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("NSET-DIFFERENCE don't accept "
				"both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_nset_difference_cons_(ptr, ret, a, b, key, testnot, 1);
	else if (check1)
		return test_nset_difference_cons_(ptr, ret, a, b, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_nset_difference_cons_(ptr, ret, a, b, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  set-exclusive-or
 */
static int test_set_exclusive_or_cons_(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr result, list, left;
	LocalHold hold;

	result = Nil;
	/* left -> right */
	hold = LocalHold_array(ptr, 1);
	for (list = list1; list != Nil; ) {
		Return_getcons(list, &left, &list);
		Return(check_intersection_cons_(ptr, &check, left, list2, key, test, notret));
		if (! check) {
			cons_heap(&result, left, result);
			localhold_set(hold, 0, result);
		}
	}

	/* right -> left */
	for (list = list2; list != Nil; ) {
		Return_getcons(list, &left, &list);
		Return(check_intersection_cons_(ptr, &check, left, list1, key, test, notret));
		if (! check) {
			cons_heap(&result, left, result);
			localhold_set(hold, 0, result);
		}
	}

	/* result */
	localhold_end(hold);
	return Result(ret, result);
}

int set_exclusive_or_common_(Execute ptr, addr a, addr b, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("SET-EXCLUSIVE-OR "
				"don't accept both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_set_exclusive_or_cons_(ptr, ret, a, b, key, testnot, 1);
	else if (check1)
		return test_set_exclusive_or_cons_(ptr, ret, a, b, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_set_exclusive_or_cons_(ptr, ret, a, b, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  nset-exclucive-or
 */
static int test_nset_exclusive_or_remove_(Execute ptr, int *ret, addr *rlist,
		addr x, addr list, addr key, addr test, int notret)
{
	int check, remove;
	addr y, z, next, tail;

	/* nil */
	remove = 0;
	if (list == Nil)
		goto finish;

	/* key */
	if (key != Nil) {
		Return(funcall1_control_(ptr, &x, key, x, NULL));
	}

	/* first */
	next = Nil;
	while (list != Nil) {
		Return_getcons(list, &y, &next);
		Return(function_call_cons_(ptr, &check, x, key, test, y, notret));
		if (! check)
			break;
		remove = 1;
		list = next;
	}
	if (list == Nil)
		goto finish;

	/* tail */
	tail = list;
	z = next;
	while (z != Nil) {
		Return_getcons(z, &y, &next);
		Return(function_call_cons_(ptr, &check, x, key, test, y, notret));
		if (! check) {
			tail = z;
			z = next;
			continue;
		}
		SetCdr(tail, next);
		z = next;
		remove = 1;
	}

finish:
	*ret = remove;
	*rlist = list;
	return 0;
}

static int test_nset_exclusive_or_cons_(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr x, z, next, tail;

	/* nil */
	if (list1 == Nil)
		return Result(ret, list2);
	if (list2 == Nil)
		return Result(ret, list1);

	/* first */
	next = Nil;
	while (list1 != Nil) {
		Return_getcons(list1, &x, &next);
		Return(test_nset_exclusive_or_remove_(ptr,
					&check, &list2, x, list2, key, test, notret));
		if (! check)
			break;
		if (list2 == Nil)
			return Result(ret, next);
		list1 = next;
	}
	if (list1 == Nil)
		return Result(ret, list2);

	/* tail */
	tail = list1;
	z = next;
	while (z != Nil) {
		Return_getcons(z, &x, &next);
		Return(test_nset_exclusive_or_remove_(ptr,
					&check, &list2, x, list2, key, test, notret));
		if (! check) {
			tail = z;
			z = next;
			continue;
		}
		SetCdr(tail, next);
		z = next;
	}

	/* result */
	SetCdr(tail, list2);
	return Result(ret, list1);
}

int nset_exclusive_or_common_(Execute ptr, addr a, addr b, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("NSET-EXCLUSIVE-OR "
				"don't accept both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_nset_exclusive_or_cons_(ptr, ret, a, b, key, testnot, 1);
	else if (check1)
		return test_nset_exclusive_or_cons_(ptr, ret, a, b, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_nset_exclusive_or_cons_(ptr, ret, a, b, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  subsetp
 */
static int test_subsetp_cons_(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr left, result;

	if (list1 == Nil)
		return Result(ret, T);
	if (list2 == Nil)
		return Result(ret, Nil);

	for (result = T; list1 != Nil; ) {
		Return_getcons(list1, &left, &list1);
		Return(check_intersection_cons_(ptr, &check, left, list2, key, test, notret));
		if (! check) {
			result = Nil;
			break;
		}
	}

	return Result(ret, result);
}

int subsetp_common_(Execute ptr, addr list1, addr list2, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		return fmte_("SUBSETP don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_subsetp_cons_(ptr, ret, list1, list2, key, testnot, 1);
	else if (check1)
		return test_subsetp_cons_(ptr, ret, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_subsetp_cons_(ptr, ret, list1, list2, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  nunion
 */
static int test_nunion_remove_(Execute ptr, addr *ret,
		addr x, addr list, addr key, addr test, int notret)
{
	int check;
	addr y, z, next, tail;

	/* nil */
	if (list == Nil)
		return Result(ret, Nil);

	/* key */
	if (key != Nil) {
		Return(funcall1_control_(ptr, &x, key, x, NULL));
	}

	/* first */
	Return_getcons(list, &y, &next);
	Return(function_call_cons_(ptr, &check, x, key, test, y, notret));
	if (check)
		return Result(ret, next);

	/* tail */
	tail = list;
	z = next;
	while (z != Nil) {
		Return_getcons(z, &y, &next);
		Return(function_call_cons_(ptr, &check, x, key, test, y, notret));
		if (check) {
			SetCdr(tail, next);
			break;
		}
		tail = z;
		z = next;
	}

	return Result(ret, list);
}

static int test_nunion_cons_(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	addr list, x, next;

	/* nil */
	if (list1 == Nil)
		return Result(ret, list2);
	if (list2 == Nil)
		return Result(ret, list1);

	/* nunion */
	list = list1;
	for (;;) {
		Return_getcons(list, &x, &next);
		Return(test_nunion_remove_(ptr, &list2, x, list2, key, test, notret));
		if (next == Nil) {
			SetCdr(list, list2);
			break;
		}
		list = next;
	}

	return Result(ret, list1);
}

int nunion_common_(Execute ptr, addr list1, addr list2, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("UNION/NUNION don't accept "
				"both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_nunion_cons_(ptr, ret, list1, list2, key, testnot, 1);
	else if (check1)
		return test_nunion_cons_(ptr, ret, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_nunion_cons_(ptr, ret, list1, list2, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  union
 */
static int test_union_cons_(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	LocalHold hold;

	if (list1 == Nil)
		return Result(ret, list2);
	if (list2 == Nil)
		return Result(ret, list1);

	copy_list_heap_safe(&list1, list1);
	copy_list_heap_safe(&list2, list2);

	hold = LocalHold_local(ptr);
	localhold_pushva(hold, list1, list2, NULL);
	Return(test_nunion_cons_(ptr, ret, list1, list2, key, test, notret));
	localhold_end(hold);

	return 0;
}

int union_common_(Execute ptr, addr list1, addr list2, addr rest, addr *ret)

{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("UNION/NUNION don't accept "
				"both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_union_cons_(ptr, ret, list1, list2, key, testnot, 1);
	else if (check1)
		return test_union_cons_(ptr, ret, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_union_cons_(ptr, ret, list1, list2, key, test, 0);
	}

	return Result(ret, Nil);
}


/************************************************************
 *  call_data.c
 ************************************************************/

/*
 *  apply
 */
static int check_data_function_(addr call, addr *ret)
{
	int check;

	if (GetType(call) == LISPTYPE_SYMBOL) {
		Return(getfunction_global_(call, &call));
		if (macro_function_p(call))
			return fmte_("Cannot call the macro-function ~S.", call, NULL);
	}
	Return(funcallp_(call, &check));
	if (! check)
		return fmte_("The argument ~S is not executable.", call, NULL);

	return Result(ret, call);
}

int apply_common_(Execute ptr, addr call, addr arg, addr args)
{
	Return(check_data_function_(call, &call));
	Return(lista_safe_local_(ptr->local, &args, arg, args));
	return apply_named_control_(ptr, call, args);
}


/*
 *  defun
 */
int defun_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr right, eval, name, args, decl, doc;
	LocalHold hold;

	/* (defun . right) */
	Return_getcdr(form, &right);
	if (right == Nil)
		return fmte_("defun form must have at least a name and body.", NULL);
	if (GetType(right) != LISPTYPE_CONS)
		return fmte_("Invalid defun form.", NULL);

	/* name */
	hold = LocalHold_local(ptr);
	Return_getcons(right, &name, &right);
	if (parse_callname_heap(&name, name))
		return fmte_("defun name ~S must be a symbol or (setf name) form.", name, NULL);
	localhold_push(hold, name);
	if (right == Nil)
		return fmte_("defun form must have at least a name and body.", NULL);
	if (GetType(right) != LISPTYPE_CONS)
		return fmte_("Invalid defun form.", NULL);

	/* args */
	Return_getcons(right, &args, &right);
	if (! IsList(args))
		return fmte_("defun argument ~S don't allow dotted list.", args, NULL);
	if (! IsList(right))
		return fmte_("Invalid defun form.", NULL);

	/* parse */
	Return(check_function_variable_(name));
	Return(lambda_ordinary_(ptr->local, &args, args));
	localhold_push(hold, args);
	Return(declare_body_documentation_(ptr, env, right, &doc, &decl, &right));

	/* (eval::defun name args decl doc body) */
	GetConst(SYSTEM_DEFUN, &eval);
	list_heap(ret, eval, name, args, decl, doc, right, form, NULL);
	localhold_end(hold);

	return 0;
}


/*
 *  fdefinition
 */
int fdefinition_common_(Execute ptr, addr name, addr *ret)
{
	Return(parse_callname_error_(&name, name));
	return fdefinition_restart_(ptr, name, ret);
}


/*
 *  (setf fdefinition)
 */
static int setf_setf_fdefinition_common_(addr value, addr symbol)
{
	addr type;

	if (macro_function_p(value)) {
		GetTypeTable(&type, Function);
		return call_type_error_va_(NULL, value, type,
				"The function ~S must be a funcallable function"
				" rather than a macro function.", value, NULL);
	}

	Return(alldelete_function_(symbol));
	return setsetf_symbol_(symbol, value);
}

static int funcall_setf_fdefinition_common_(addr value, addr symbol)
{
	Return(alldelete_function_(symbol));
	if (macro_function_p(value))
		return setmacro_symbol_(symbol, value);
	else
		return setfunction_symbol_(symbol, value);
}

int setf_fdefinition_common_(addr value, addr name)
{
	CallNameType type;

	Return(parse_callname_error_(&name, name));
	GetCallNameType(name, &type);
	GetCallName(name, &name);
	if (type == CALLNAME_SETF)
		return setf_setf_fdefinition_common_(value, name);
	else
		return funcall_setf_fdefinition_common_(value, name);
}


/*
 *  fboundp
 */
int fboundp_common_(addr name, int *ret)
{
	addr check;
	CallNameType type;

	/* callname */
	Return(parse_callname_error_(&name, name));

	/* function check */
	getglobal_callname(name, &check);
	if (check != Unbound)
		return Result(ret, 1);

	/* setf */
	GetCallNameType(name, &type);
	if (type != CALLNAME_SYMBOL)
		return Result(ret, 0);

	/* macro check */
	GetCallName(name, &name);
	getmacro_symbol(name, &check);
	if (check != Unbound)
		return Result(ret, 1);

	/* special operator */
	return Result(ret, get_special_operator(name));
}


/*
 *  fmakunbound
 */
int fmakunbound_common_(addr name)
{
	addr check;
	CallNameType type;

	/* callname */
	Return(parse_callname_error_(&name, name));

	/* remove function */
	Return(remtype_global_callname_(name));
	Return(setglobal_callname_(name, Unbound));

	/* setf */
	GetCallNameType(name, &type);
	if (type != CALLNAME_SYMBOL)
		return 0;

	/* remove macro */
	GetCallName(name, &check);
	remmacro_symbol(check);

	return 0;
}


/*
 *  funcall
 */
int funcall_common_(Execute ptr, addr call, addr args)
{
	Return(check_data_function_(call, &call));
	return apply_named_control_(ptr, call, args);
}


/*
 *  function-lambda-expression
 */
static int function_closure_p(addr var)
{
	GetDataFunction(var, &var);
	return var != Nil;
}

void function_lambda_expression_common(addr var, addr *ret1, addr *ret2, addr *ret3)
{
	addr pos1, pos2, pos3;

	/* lambda-expression */
	getlambda_expression_function(var, &pos1);
	/* closure-p */
	pos2 = function_closure_p(var)? T: Nil;
	/* name */
	GetNameFunction(var, &pos3);
	if (GetType(pos3) == LISPTYPE_CALLNAME)
		name_callname_heap(pos3, &pos3);
	/* result */
	*ret1 = pos1;
	*ret2 = pos2;
	*ret3 = pos3;
}


/*
 *  lambda-list-keywords
 */
void lambda_list_keywords_common(addr *ret)
{
	static const constindex lambda_list_keywords[] = {
		CONSTANT_AMPERSAND_WHOLE,
		CONSTANT_AMPERSAND_OPTIONAL,
		CONSTANT_AMPERSAND_REST,
		CONSTANT_AMPERSAND_BODY,
		CONSTANT_AMPERSAND_KEY,
		CONSTANT_AMPERSAND_ALLOW,
		CONSTANT_AMPERSAND_AUX,
		CONSTANT_AMPERSAND_ENVIRONMENT,
		CONSTANT_EMPTY
	};
	int i;
	addr list, pos;
	constindex index;

	list = Nil;
	for (i = 0; ; i++) {
		index = lambda_list_keywords[i];
		if (index == CONSTANT_EMPTY)
			break;
		GetConstant(index, &pos);
		cons_heap(&list, pos, list);
	}
	*ret = list;
}


/*
 *  defconstant
 */
int defconstant_common_(addr form, addr env, addr *ret)
{
	/* (lisp-system::defconstant symbol value doc) */
	addr args, symbol, value, doc, quote;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &symbol, &args))
		goto error;
	if (! symbolp(symbol))
		return fmte_("The defconstant argument ~S must be a symbol.", symbol, NULL);
	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args == Nil) {
		doc = Nil;
	}
	else {
		if (! consp_getcons(args, &doc, &args))
			goto error;
		if (! stringp(doc))
			return fmte_("The defconstant argument ~S must be a string.", doc, NULL);
		if (args != Nil)
			goto error;
	}
	GetConst(SYSTEM_DEFCONSTANT, &args);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&symbol, quote, symbol, NULL);
	list_heap(&doc, quote, doc, NULL);
	list_heap(ret, args, symbol, value, doc, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("The defconstant argument ~S must be a "
			"(symbol value &optional documentation) form.", form, NULL);
}


/*
 *  defparameter
 */
static void defparameter_expand_common(addr symbol, addr value, addr doc, addr *ret)
{
	/* `(progn (declaim (special ,symbol))
	 *         (setf (symbol-value ',symbol) ,value)
	 *         ,(when doc
	 *            `(lisp-system::setdoc-variable ',symbol ',doc))
	 *         ',symbol)
	 */
	addr progn, declaim, special, setf, symbolv, setdoc, quote;

	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_DECLAIM, &declaim);
	GetConst(COMMON_SPECIAL, &special);
	GetConst(COMMON_SETF, &setf);
	GetConst(COMMON_SYMBOL_VALUE, &symbolv);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&special, special, symbol, NULL);
	list_heap(&declaim, declaim, special, NULL);
	list_heap(&symbol, quote, symbol, NULL);
	list_heap(&symbolv, symbolv, symbol, NULL);
	list_heap(&setf, setf, symbolv, value, NULL);
	if (doc == Nil) {
		list_heap(&progn, progn, declaim, setf, symbol, NULL);
	}
	else {
		GetConst(SYSTEM_SETDOC_VARIABLE, &setdoc);
		list_heap(&doc, quote, doc, NULL);
		list_heap(&setdoc, setdoc, symbol, doc, NULL);
		list_heap(&progn, progn, declaim, setf, setdoc, symbol, NULL);
	}
	*ret = progn;
}

int defparameter_common_(addr form, addr env, addr *ret)
{
	/* (lisp-system::defparameter symbol value doc) */
	addr args, symbol, value, doc;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &symbol, &args))
		goto error;
	if (! symbolp(symbol))
		return fmte_("The defparameter argument ~S must be a symbol.", symbol, NULL);
	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args == Nil) {
		doc = Nil;
		goto expand;
	}
	if (! consp_getcons(args, &doc, &args))
		goto error;
	if (! stringp(doc))
		return fmte_("The defparameter argument ~S must be a string.", doc, NULL);
	if (args != Nil)
		goto error;
expand:
	defparameter_expand_common(symbol, value, doc, ret);
	return 0;

error:
	*ret = Nil;
	return fmte_("The defparameter argument ~S must be a "
			"(symbol value &optional documentation) form.", form, NULL);
}


/*
 *  defvar
 */
static void defvar_novalue_common(addr symbol, addr *ret)
{
	/* `(progn (declaim (special ,symbol))
	 *         ',symbol)
	 */
	addr progn, declaim, special, quote;

	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_DECLAIM, &declaim);
	GetConst(COMMON_SPECIAL, &special);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&special, special, symbol, NULL);
	list_heap(&declaim, declaim, special, NULL);
	list_heap(&symbol, quote, symbol, NULL);
	list_heap(&progn, progn, declaim, symbol, NULL);
	*ret = progn;
}

static void defvar_expand_common(addr symbol, addr value, addr doc, addr *ret)
{
	/* `(progn (declaim (special ,symbol))
	 *         `(unless (boundp ',symbol)
	 *           (setf (symbol-value ',symbol) ,value))
	 *         ,(when doc
	 *           `(lisp-system::setdoc-variable ',symbol ',doc))
	 *         ',symbol)
	 */
	addr progn, declaim, special, unless, boundp, setf, symbolv, setdoc, quote;

	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_DECLAIM, &declaim);
	GetConst(COMMON_SPECIAL, &special);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_BOUNDP, &boundp);
	GetConst(COMMON_SETF, &setf);
	GetConst(COMMON_SYMBOL_VALUE, &symbolv);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&special, special, symbol, NULL);
	list_heap(&declaim, declaim, special, NULL);
	list_heap(&symbol, quote, symbol, NULL);
	list_heap(&symbolv, symbolv, symbol, NULL);
	list_heap(&setf, setf, symbolv, value, NULL);
	list_heap(&boundp, boundp, symbol, NULL);
	list_heap(&unless, unless, boundp, setf, NULL);
	if (doc == Nil) {
		list_heap(&progn, progn, declaim, unless, symbol, NULL);
	}
	else {
		GetConst(SYSTEM_SETDOC_VARIABLE, &setdoc);
		list_heap(&doc, quote, doc, NULL);
		list_heap(&setdoc, setdoc, symbol, doc, NULL);
		list_heap(&progn, progn, declaim, unless, setdoc, symbol, NULL);
	}
	*ret = progn;
}

int defvar_common_(addr form, addr env, addr *ret)
{
	addr args, symbol, value, doc;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &symbol, &args))
		goto error;
	if (args == Nil) {
		value = Unbound;
		doc = Nil;
		goto expand;
	}
	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args == Nil) {
		doc = Nil;
		goto expand;
	}
	if (! consp_getcons(args, &doc, &args))
		goto error;
	if (! stringp(doc))
		return fmte_("The defvar argument ~S must be a string.", doc, NULL);
	if (args != Nil)
		goto error;
expand:
	if (value == Unbound)
		defvar_novalue_common(symbol, &form);
	else
		defvar_expand_common(symbol, value, doc, &form);
	return Result(ret, form);

error:
	*ret = Nil;
	return fmte_("The defvar argument ~S must be a "
			"(symbol &optional value documentation) form.", form, NULL);
}


/*
 *  destructuring-bind
 */
static int destructuring_bind_check_common_(addr pos)
{
	getenvironment_macro_lambda(pos, &pos);
	if (pos != Nil) {
		return fmte_("destructuring-bind don't accept "
				"&environment parameter ~S.", pos, NULL);
	}

	return 0;
}

int destructuring_bind_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, lambda, expr, decl, eval;
	LocalHold hold;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &lambda, &args))
		goto error;
	if (! consp_getcons(args, &expr, &args))
		goto error;
	/* parse */
	if (! listp(lambda)) {
		return fmte_("destructuring-bind argument ~S "
				"must be a list type.", lambda, NULL);
	}

	Return(lambda_macro_(ptr->local, &lambda, lambda, Nil));
	Return(destructuring_bind_check_common_(lambda));
	hold = LocalHold_local_push(ptr, lambda);
	Return(declare_body_(ptr, env, args, &decl, &args));
	localhold_end(hold);
	/* (eval::destructuring-bind lambda expr decl args) */
	GetConst(SYSTEM_DESTRUCTURING_BIND, &eval);
	list_heap(ret, eval, lambda, expr, decl, args, form, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("destructuring-bind argument ~S must be a "
			"(lambda-list expr &body body) form.", form, NULL);
}


/*
 *  psetq
 */
static int psetq_common_constant_(Execute ptr, addr form, addr env, addr *ret,
		constindex setq_constant,
		constindex psetq_constant)
{
	addr args, root, var, value, gensym, cons, setq, let;

	Return_getcdr(form, &form);
	GetConstant(setq_constant, &setq);
	args = root = Nil;
	while (form != Nil) {
		if (! consp_getcons(form, &var, &form)) {
			GetConstant(psetq_constant, &setq);
			return fmte_("~A argument ~S don't allow dotted list.", setq, form, NULL);
		}
		if (! consp_getcons(form, &value, &form))
			return fmte_("After variable ~S must be a cons, but ~S.", var, form, NULL);
		Return(make_gensym_(ptr, &gensym));
		/* let argument */
		list_heap(&cons, gensym, value, NULL);
		cons_heap(&args, cons, args);
		/* body */
		list_heap(&cons, setq, var, gensym, NULL);
		cons_heap(&root, cons, root);
	}
	/* nil */
	cons_heap(&root, Nil, root);
	/* let form */
	nreverse(&args, args);
	nreverse(&root, root);
	GetConst(COMMON_LET, &let);
	lista_heap(ret, let, args, root, NULL);

	return 0;
}

int psetq_common_(Execute ptr, addr form, addr env, addr *ret)
{
	return psetq_common_constant_(ptr, form, env, ret,
			CONSTANT_COMMON_SETQ,
			CONSTANT_COMMON_PSETQ);
}


/*
 *  psetf
 */
int psetf_common_(Execute ptr, addr form, addr env, addr *ret)
{
	return psetq_common_constant_(ptr, form, env, ret,
			CONSTANT_COMMON_SETF,
			CONSTANT_COMMON_PSETF);
}


/*
 *  return
 */
int return_common_(addr form, addr env, addr *ret)
{
	addr args, value, return_from;

	Return_getcdr(form, &args);
	if (args == Nil) {
		value = Nil;
		goto expand;
	}
	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args != Nil)
		goto error;
expand:
	GetConst(COMMON_RETURN_FROM, &return_from);
	list_heap(ret, return_from, Nil, value, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("RETURN argument ~S must be a (&optional value) form.", form, NULL);
}


/*
 *  complement
 */
void complement_common(addr var, addr *ret)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_dynamic(pos, p_defun_lambda_complement);
	SetDataFunction(pos, var);
	*ret = pos;
}


/*
 *  constantly
 */
void constantly_common(addr var, addr *ret)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_any(pos, p_defun_lambda_constantly);
	SetDataFunction(pos, var);
	*ret = pos;
}


/*
 *  every
 */
int every_common_(Execute ptr, addr call, addr rest, addr *ret)
{
	addr pos, car, cdr, args, next, temp1, temp2;
	LocalRoot local;
	LocalStack stack;
	size_t size, len;

	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil)
		return fmte_("Too few arguments.", NULL);

	/* second */
	args = next = Nil;
	size = 0;
	while (rest != Nil) {
		GetCons(rest, &pos, &rest);
		if (pos == Nil)
			goto result_true;
		if (consp(pos)) {
			Return_getcons(pos, &car, &cdr);
			cons_local(local, &args, car, args);
			cons_local(local, &next, cdr, next);
		}
		else {
			Return(length_sequence_(pos, 1, &len));
			if (len <= size)
				goto result_true;
			Return(getelt_sequence_(NULL, pos, size, &car));
			cons_local(local, &args, car, args);
			cons_local(local, &next, pos, next);
		}
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	Return(apply1_control_(ptr, &pos, call, args));
	if (pos == Nil)
		goto result_false;

	/* second */
	for (size = 1; ; size++) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil)
				goto result_true;
			if (consp(cdr)) {
				Return_getcons(cdr, &car, &cdr);
				SetCar(temp1, car);
				SetCar(temp2, cdr);
			}
			else {
				Return(length_sequence_(cdr, 1, &len));
				if (len <= size)
					goto result_true;
				Return(getelt_sequence_(NULL, cdr, size, &car));
				SetCar(temp1, car);
			}
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		Return(apply1_control_(ptr, &pos, call, args));
		if (pos == Nil)
			goto result_false;
	}

result_true:
	rollback_local(local, stack);
	return Result(ret, T);

result_false:
	rollback_local(local, stack);
	return Result(ret, Nil);
}


/*
 *  some
 */
int some_common_(Execute ptr, addr call, addr rest, addr *ret)
{
	addr pos, car, cdr, args, next, temp1, temp2;
	LocalRoot local;
	LocalStack stack;
	size_t size, len;

	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil)
		return fmte_("Too few arguments.", NULL);

	/* second */
	args = next = Nil;
	size = 0;
	while (rest != Nil) {
		GetCons(rest, &pos, &rest);
		if (pos == Nil)
			goto result_false;
		if (consp(pos)) {
			Return_getcons(pos, &car, &cdr);
			cons_local(local, &args, car, args);
			cons_local(local, &next, cdr, next);
		}
		else {
			Return(length_sequence_(pos, 1, &len));
			if (len <= size)
				goto result_false;
			Return(getelt_sequence_(NULL, pos, size, &car));
			cons_local(local, &args, car, args);
			cons_local(local, &next, pos, next);
		}
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	if (apply1_control_(ptr, &pos, call, args))
		return 1;
	if (pos != Nil)
		goto result;

	/* second */
	for (size = 1; ; size++) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil)
				goto result_false;
			if (consp(cdr)) {
				Return_getcons(cdr, &car, &cdr);
				SetCar(temp1, car);
				SetCar(temp2, cdr);
			}
			else {
				Return(length_sequence_(cdr, 1, &len));
				if (len <= size)
					goto result_false;
				Return(getelt_sequence_(NULL, cdr, size, &car));
				SetCar(temp1, car);
			}
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (apply1_control_(ptr, &pos, call, args))
			return 1;
		if (pos != Nil)
			goto result;
	}

result:
	rollback_local(local, stack);
	*ret = pos;
	return 0;

result_false:
	rollback_local(local, stack);
	*ret = Nil;
	return 0;
}


/*
 *  notevery
 */
int notevery_common_(Execute ptr, addr call, addr rest, addr *ret)
{
	/* (notevery predicate sequence*) ==  (not (every predicate sequence*)) */
	Return(every_common_(ptr, call, rest, &rest));
	return Result(ret, (rest == Nil)? T: Nil);
}


/*
 *  notany
 */
int notany_common_(Execute ptr, addr call, addr rest, addr *ret)
{
	/* (notany predicate sequence*) ==  (not (some predicate sequence*)) */
	Return(some_common_(ptr, call, rest, &rest));
	return Result(ret, (rest == Nil)? T: Nil);
}


/*
 *  and
 */
int and_common_(addr form, addr env, addr *ret)
{
	addr expr, when, andv;

	Return_getcdr(form, &form);

	/* (and) */
	if (form == Nil)
		return Result(ret, T);

	/* (and expr) */
	if (singlep(form)) {
		GetCar(form, ret);
		return 0;
	}

	/* (and expr . tail) -> (when expr (and . tail)) */
	GetCons(form, &expr, &form);
	GetConst(COMMON_WHEN, &when);
	GetConst(COMMON_AND, &andv);
	cons_heap(&andv, andv, form);
	list_heap(ret, when, expr, andv, NULL);

	return 0;
}


/*
 *  cond
 */
int cond_common_(addr form, addr env, addr *ret)
{
	addr expr, tail, ifsym, progn, cond;

	Return_getcdr(form, &form);

	/* (cond) */
	if (form == Nil)
		return Result(ret, Nil);

	/* (cond clause ...) */
	if (! consp_getcons(form, &expr, &form))
		return fmte_("The cond form ~S must be a cons.", form, NULL);
	if (! consp_getcons(expr, &expr, &tail))
		return fmte_("The cond clause ~S must be a cons.", expr, NULL);
	if (tail == Nil) {
		/* (cond (expr) . form)
		 *   `(or ,expr (cond ,$form)))
		 */
		GetConst(COMMON_OR, &ifsym);
		GetConst(COMMON_COND, &cond);
		cons_heap(&form, cond, form);
		list_heap(ret, ifsym, expr, form, NULL);
	}
	else {
		/* (cond (expr . tail) . form)
		 *   `(if ,expr (progn ,@tail) (cond ,@form))
		 */
		GetConst(COMMON_IF, &ifsym);
		GetConst(COMMON_PROGN, &progn);
		GetConst(COMMON_COND, &cond);
		cons_heap(&form, cond, form);
		cons_heap(&tail, progn, tail);
		list_heap(ret, ifsym, expr, tail, form, NULL);
	}

	return 0;
}


/*
 *  or
 */
int or_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr gensym, let, ifsym, orv, expr;

	Return_getcdr(form, &form);

	/* (or) */
	if (form == Nil)
		return Result(ret, Nil);

	/* (or expr) */
	if (singlep(form)) {
		GetCar(form, ret);
		return 0;
	}

	/* (or expr . form) ->
	 *   (let ((#:g expr))
	 *     (if #:g #:g (or . form))) */
	if (! consp_getcons(form, &expr, &form))
		return fmte_("The or form ~S must be a cons.", NULL);
	Return(make_gensym_(ptr, &gensym));
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_IF, &ifsym);
	GetConst(COMMON_OR, &orv);
	list_heap(&expr, gensym, expr, NULL);
	conscar_heap(&expr, expr);
	cons_heap(&form, orv, form);
	list_heap(&form, ifsym, gensym, gensym, form, NULL);
	list_heap(ret, let, expr, form, NULL);

	return 0;
}


/*
 *  when
 */
int when_common_(addr form, addr env, addr *ret)
{
	addr args, expr, ifsym, cons;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &expr, &args))
		return fmte_("The when ~S must be a (when test . body) form.", form, NULL);
	/* `(if ,expr (progn ,@body)) */
	GetConst(COMMON_PROGN, &cons);
	cons_heap(&cons, cons, args);
	GetConst(COMMON_IF, &ifsym);
	list_heap(ret, ifsym, expr, cons, NULL);

	return 0;
}


/*
 *  unless
 */
int unless_common_(addr form, addr env, addr *ret)
{
	addr args, notv, expr, ifsym, cons;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &expr, &args))
		return fmte_("The unless ~S must be a (unless test . body) form.", form, NULL);
	/* `(if (not ,expr) (progn ,@body)) */
	GetConst(COMMON_PROGN, &cons);
	cons_heap(&cons, cons, args);
	GetConst(COMMON_NOT, &notv);
	list_heap(&expr, notv, expr, NULL);
	GetConst(COMMON_IF, &ifsym);
	list_heap(ret, ifsym, expr, cons, NULL);

	return 0;
}


/*
 *  case
 */
int case_common_(Execute ptr, addr form, addr env, addr *ret)
{
	int lastp;
	addr key, args, list, test, body, g, root;
	addr let, cond, eql, member, quote, otherwise, declare, ignorable;

	/* (let ((g key))
	 *   (declare (ignorable g))
	 *   (cond ((eql g 'test1) . body1)
	 *         ((member g '(test2)) . body2)
	 *         (t . otherwise)))
	 */
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &key, &args))
		return fmte_("CASE argument must be (key &rest clauses) form.", form, NULL);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_COND, &cond);
	GetConst(COMMON_EQL, &eql);
	GetConst(COMMON_MEMBER, &member);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_OTHERWISE, &otherwise);
	Return(make_gensym_(ptr, &g));

	lastp = 0;
	for (root = Nil; args != Nil; ) {
		if (! consp(args))
			return fmte_("CASE clauses ~S must be list type.", args, NULL);
		if (lastp) {
			return fmte_("CASE clauses ~S "
					"don't appear after otherwise clause.", args, NULL);
		}
		GetCons(args, &test, &args);
		if (! consp_getcons(test, &test, &body))
			return fmte_("CASE clauses ~S must be list type.", test, NULL);
		if (body == Nil)
			consnil_heap(&body);
		if (test == T || test == otherwise) {
			cons_heap(&list, T, body);
			lastp = 1;
		}
		else {
			list_heap(&list, quote, test, NULL);
			list_heap(&list, listp(test)? member: eql, g, list, NULL);
			cons_heap(&list, list, body);
		}
		cons_heap(&root, list, root);
	}
	/* otherwise */
	if (lastp == 0) {
		list_heap(&list, T, Nil, NULL);
		cons_heap(&root, list, root);
	}
	/* cond form */
	nreverse(&root, root);
	cons_heap(&root, cond, root);
	list_heap(&list, g, key, NULL);
	conscar_heap(&list, list);
	list_heap(&ignorable, ignorable, g, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(ret, let, list, declare, root, NULL);

	return 0;
}


/*
 *  ecase
 */
int ecase_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr key, args, list, test, body, g, root;
	addr let, cond, eql, member, quote, error, type, a;

	/* (let ((g key))
	 *   (cond ((eql g 'test1) . body1)
	 *         ((member g '(test2)) . body2)
	 *         (t . (type-error g (member ...)))))
	 */
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &key, &args))
		return fmte_("ECASE argument must be (key &rest clauses) form.", form, NULL);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_COND, &cond);
	GetConst(COMMON_EQL, &eql);
	GetConst(COMMON_MEMBER, &member);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_ECASE_ERROR, &error);
	Return(make_gensym_(ptr, &g));

	type = Nil;
	for (root = Nil; args != Nil; ) {
		if (! consp_getcons(args, &test, &args))
			return fmte_("ECASE clauses ~S must be list type.", args, NULL);
		if (! consp_getcons(test, &test, &body))
			return fmte_("ECASE clauses ~S must be list type.", test, NULL);
		if (body == Nil)
			consnil_heap(&body);
		list_heap(&list, quote, test, NULL);
		if (listp(test)) {
			list_heap(&list, member, g, list, NULL);
			while (test != Nil) {
				Return_getcons(test, &a, &test);
				cons_heap(&type, a, type);
			}
		}
		else {
			list_heap(&list, eql, g, list, NULL);
			cons_heap(&type, test, type);
		}
		cons_heap(&list, list, body);
		cons_heap(&root, list, root);
	}

	/* error */
	nreverse(&type, type);
	list_heap(&type, quote, type, NULL);
	list_heap(&list, error, g, type, NULL);
	list_heap(&list, T, list, NULL);
	cons_heap(&root, list, root);
	/* cond form */
	nreverse(&root, root);
	cons_heap(&root, cond, root);
	list_heap(&list, g, key, NULL);
	conscar_heap(&list, list);
	list_heap(ret, let, list, root, NULL);

	return 0;
}


/*
 *  ccase
 */
static int ccase_comma_common_(Execute ptr, addr stream, addr x, int *first)
{
	if (*first) {
		*first = 0;
	}
	else {
		Return(print_ascii_stream_(stream, ", "));
	}
	return princ_print_(ptr, stream, x);
}

static int ccase_string_common_(Execute ptr,
		addr *ret, addr *rtype, addr place, addr args)
{
	int first;
	addr stream, pos, x, list;
	LocalHold hold;

	/* member */
	GetConst(COMMON_MEMBER, &list);
	conscar_heap(&list, list);
	/* stream */
	open_output_string_stream(&stream, 0);
	hold = LocalHold_array(ptr, 2);
	localhold_set(hold, 0, stream);
	localhold_set(hold, 1, list);
	Return(format_stream_(ptr, stream, "The value of ~A, ~~A, is not ", place, NULL));
	/* loop */
	for (first = 1; args != Nil; ) {
		Return_getcons(args, &pos, &args);
		Return_getcar(pos, &pos);
		if (listp(pos)) {
			while (pos != Nil) {
				Return_getcons(pos, &x, &pos);
				Return(ccase_comma_common_(ptr, stream, x, &first));
				cons_heap(&list, x, list);
				localhold_set(hold, 1, list);
			}
		}
		else {
			Return(ccase_comma_common_(ptr, stream, pos, &first));
			cons_heap(&list, pos, list);
			localhold_set(hold, 1, list);
		}
	}
	localhold_end(hold);

	Return(write_char_stream_(stream, '.'));
	Return(string_stream_heap_(stream, ret));
	Return(close_stream_(stream, NULL));
	nreverse(rtype, list);

	return 0;
}

static int ccase_cond_common_(addr g, addr str3, addr type, addr args, addr *ret)
{
	/*  (cond ((eql g 'test1) . body1)
	 *        ((member g 'test2) . body2)
	 *        ...
	 *        (t (error
	 *             (make-condition 'simple-type-error
	 *               :datum g
	 *               :expected-type '(member ...)
	 *               :format-control "The g of xx, ~A, is not xx"
	 *               :format-arguments (list g)))))
	 */
	addr invoke, make, simple, datum, expect, control, arguments, quote, list;
	addr root, test, body, member, eql, a, cond;

	GetConst(COMMON_ERROR, &invoke);
	GetConst(COMMON_MAKE_CONDITION, &make);
	GetConst(COMMON_SIMPLE_TYPE_ERROR, &simple);
	GetConst(KEYWORD_DATUM, &datum);
	GetConst(KEYWORD_EXPECTED_TYPE, &expect);
	GetConst(KEYWORD_FORMAT_CONTROL, &control);
	GetConst(KEYWORD_FORMAT_ARGUMENTS, &arguments);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_MEMBER, &member);
	GetConst(COMMON_EQL, &eql);
	GetConst(COMMON_COND, &cond);

	/* (error ...) */
	list_heap(&list, list, g, NULL);
	list_heap(&type, quote, type, NULL);
	list_heap(&simple, quote, simple, NULL);
	list_heap(&make, make, simple,
			datum, g, expect, type, control, str3, arguments, list, NULL);
	list_heap(&invoke, invoke, make, NULL);

	/* cond */
	type = Nil;
	for (root = Nil; args != Nil; ) {
		if (! consp_getcons(args, &test, &args))
			return fmte_("CCASE clauses ~S must be list type.", args, NULL);
		if (! consp_getcons(test, &test, &body))
			return fmte_("CCASE clauses ~S must be list type.", test, NULL);
		if (body == Nil)
			consnil_heap(&body);
		list_heap(&list, quote, test, NULL);
		if (listp(test)) {
			list_heap(&list, member, g, list, NULL);
			while (test != Nil) {
				Return_getcons(test, &a, &test);
				cons_heap(&type, a, type);
			}
		}
		else {
			list_heap(&list, eql, g, list, NULL);
			cons_heap(&type, test, type);
		}
		cons_heap(&list, list, body);
		cons_heap(&root, list, root);
	}

	/* error */
	list_heap(&invoke, T, invoke, NULL);
	cons_heap(&root, invoke, root);
	nreverse(&root, root);
	cons_heap(ret, cond, root);

	return 0;
}

static int ccase_expand_common_(Execute ptr,
		addr env, addr *ret, addr place, addr args)
{
	/* (let* ((a1 b1) (a2 b2) ... (value r) g)
	 *   (declare (ignorable a1 a2 ...))
	 *   (block result
	 *     (tagbody
	 *       loop
	 *       (restart-bind
	 *         ((store-value
	 *            (lambda (v) (setq g v value v) w (go loop))
	 *            :report-function
	 *              (lambda (s)
	 *                (princ "Retry ccase with new value xx." s))
	 *            :interactive-function
	 *              (lambda ()
	 *                (list (eval (prompt-for t "Input xx> "))))))
	 *         (return-from result ...)))))
	 */
	addr a, b, g, r, w, v, s, str1, str2, str3;
	addr leta, declare, ignorable, tagbody, loop, restart, store, lambda, setq;
	addr value, go, report, inter, princ, list, eval, prompt, cond, quote;
	addr x, y, root, type, block, retfrom, result;
	LocalHold hold;

	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	hold = LocalHold_local(ptr);
	localhold_pushva(hold, a, b, g, w, r, NULL);

	Return_getcar(g, &g);
	Return(format_string_(ptr, &str1,
				"Retry ccase with new value ~A.", place, NULL));
	localhold_push(hold, str1);
	Return(format_string_(ptr, &str2,
				"Input ~A> ", place, NULL));
	localhold_push(hold, str2);
	Return(ccase_string_common_(ptr, &str3, &type, place, args));
	localhold_end(hold);

	make_symbolchar(&v, "V");
	make_symbolchar(&s, "STREAM");
	make_symbolchar(&loop, "LOOP");
	make_symbolchar(&value, "VALUE");
	make_symbolchar(&result, "RESULT");
	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_RESTART_BIND, &restart);
	GetConst(COMMON_STORE_VALUE, &store);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_GO, &go);
	GetConst(KEYWORD_REPORT_FUNCTION, &report);
	GetConst(KEYWORD_INTERACTIVE_FUNCTION, &inter);
	GetConst(COMMON_PRINC, &princ);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_EVAL, &eval);
	GetConst(SYSTEM_PROMPT_FOR, &prompt);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_RETURN_FROM, &retfrom);
	/* expand */
	Return(ccase_cond_common_(value, str3, type, args, &cond));
	list_heap(&cond, retfrom, result, cond, NULL);
	list_heap(&prompt, prompt, T, str2, NULL);
	list_heap(&eval, eval, prompt, NULL);
	list_heap(&list, list, eval, NULL);
	list_heap(&x, lambda, Nil, list, NULL);
	list_heap(&princ, princ, str1, s, NULL);
	list_heap(&s, s, NULL);
	list_heap(&y, lambda, s, princ, NULL);
	list_heap(&go, go, loop, NULL);
	list_heap(&setq, setq, g, v, value, v, NULL);
	list_heap(&v, v, NULL);
	list_heap(&lambda, lambda, v, setq, w, go, NULL);
	list_heap(&store, store, lambda, report, y, inter, x, NULL);
	list_heap(&store, store, NULL);
	list_heap(&restart, restart, store, cond, NULL);
	list_heap(&tagbody, tagbody, loop, restart, NULL);
	list_heap(&tagbody, block, result, tagbody, NULL);
	/* let* */
	lista_heap(&ignorable, ignorable, a, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	for (root = Nil; a != Nil; ) {
		Return_getcons(a, &x, &a);
		Return_getcons(b, &y, &b);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}
	list_heap(&value, value, r, NULL);
	cons_heap(&root, value, root);
	cons_heap(&root, g, root);
	nreverse(&root, root);
	list_heap(ret, leta, root, declare, tagbody, NULL);

	return 0;
}

int ccase_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, x;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, form, env, NULL);
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &x, &args))
		goto error;
	Return(ccase_expand_common_(ptr, env, ret, x, args));
	localhold_end(hold);
	return 0;

error:
	*ret = Nil;
	return fmte_("CCASE arguments ~S must be (place &rest args) form.", form, NULL);
}


/*
 *  typecase
 */
int typecase_common_(Execute ptr, addr form, addr env, addr *ret)
{
	int lastp;
	addr key, args, list, test, body, g, root;
	addr let, cond, typep, quote, otherwise, declare, ignorable;

	/* (let ((g key))
	 *   (declare (ignorable g))
	 *   (cond ((typep g 'test1) . body1)
	 *         (t . otherwise)))
	 */
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &key, &args))
		return fmte_("TYPECASE argument must be (key &rest clauses) form.", form, NULL);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_COND, &cond);
	GetConst(COMMON_TYPEP, &typep);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_OTHERWISE, &otherwise);
	Return(make_gensym_(ptr, &g));

	lastp = 0;
	for (root = Nil; args != Nil; ) {
		if (! consp(args))
			return fmte_("TYPECASE clauses ~S must be list type.", args, NULL);
		if (lastp) {
			return fmte_("TYPECASE clauses ~S don't "
					"appear after otherwise clause.", args, NULL);
		}
		GetCons(args, &test, &args);
		if (! consp_getcons(test, &test, &body))
			return fmte_("TYPECASE clauses ~S must be list type.", test, NULL);
		if (body == Nil)
			consnil_heap(&body);
		if (test == T || test == otherwise) {
			cons_heap(&list, T, body);
			lastp = 1;
		}
		else {
			list_heap(&list, quote, test, NULL);
			list_heap(&list, typep, g, list, NULL);
			cons_heap(&list, list, body);
		}
		cons_heap(&root, list, root);
	}
	/* otherwise */
	if (lastp == 0) {
		list_heap(&list, T, Nil, NULL);
		cons_heap(&root, list, root);
	}
	/* cond form */
	nreverse(&root, root);
	cons_heap(&root, cond, root);
	list_heap(&list, g, key, NULL);
	conscar_heap(&list, list);
	list_heap(&ignorable, ignorable, g, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(ret, let, list, declare, root, NULL);

	return 0;
}


/*
 *  etypecase
 */
int etypecase_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr key, args, list, test, body, g, root;
	addr let, cond, typep, quote, error, type;

	/* (let ((g key))
	 *   (cond ((typep g 'test1) . body1)
	 *         (t . (type-error g (or ...)))))
	 */
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &key, &args))
		return fmte_("ETYPECASE argument must be (key &rest clauses) form.", form, NULL);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_COND, &cond);
	GetConst(COMMON_TYPEP, &typep);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_ETYPECASE_ERROR, &error);
	Return(make_gensym_(ptr, &g));

	type = Nil;
	for (root = Nil; args != Nil; ) {
		if (! consp_getcons(args, &test, &args))
			return fmte_("ETYPECASE clauses ~S must be list type.", args, NULL);
		if (! consp_getcons(test, &test, &body))
			return fmte_("ETYPECASE clauses ~S must be list type.", test, NULL);
		if (body == Nil)
			consnil_heap(&body);
		list_heap(&list, quote, test, NULL);
		list_heap(&list, typep, g, list, NULL);
		cons_heap(&type, test, type);
		cons_heap(&list, list, body);
		cons_heap(&root, list, root);
	}
	/* error */
	nreverse(&type, type);
	list_heap(&type, quote, type, NULL);
	list_heap(&list, error, g, type, NULL);
	list_heap(&list, T, list, NULL);
	cons_heap(&root, list, root);
	/* cond form */
	nreverse(&root, root);
	cons_heap(&root, cond, root);
	list_heap(&list, g, key, NULL);
	conscar_heap(&list, list);
	list_heap(ret, let, list, root, NULL);

	return 0;
}


/*
 *  ctypecase
 */
static int ctypecase_string_common_(Execute ptr, addr *ret, addr args)
{
	addr list, pos;

	GetConst(COMMON_OR, &list);
	conscar_heap(&list, list);
	while (args != Nil) {
		Return_getcons(args, &pos, &args);
		Return_getcar(pos, &pos);
		cons_heap(&list, pos, list);
	}
	nreverse(ret, list);

	return 0;
}

static int ctypecase_cond_common_(addr g, addr type, addr args, addr *ret)
{
	/*  (cond ((typep g 'type1) . body1)
	 *        ...
	 *        (t (error
	 *             (make-condition 'type-error
	 *               :datum g
	 *               :expected-type 'type))))
	 */
	addr invoke, make, type_error, datum, expect, quote, typep, cond;
	addr root, test, body, list;

	GetConst(COMMON_ERROR, &invoke);
	GetConst(COMMON_MAKE_CONDITION, &make);
	GetConst(COMMON_TYPE_ERROR, &type_error);
	GetConst(KEYWORD_DATUM, &datum);
	GetConst(KEYWORD_EXPECTED_TYPE, &expect);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_TYPEP, &typep);
	GetConst(COMMON_COND, &cond);

	/* (error ...) */
	list_heap(&type, quote, type, NULL);
	list_heap(&type_error, quote, type_error, NULL);
	list_heap(&make, make, type_error, datum, g, expect, type, NULL);
	list_heap(&invoke, invoke, make, NULL);

	/* cond */
	type = Nil;
	for (root = Nil; args != Nil; ) {
		if (! consp_getcons(args, &test, &args))
			return fmte_("CTYPECASE clauses ~S must be list type.", args, NULL);
		if (! consp_getcons(test, &test, &body))
			return fmte_("CTYPECASE clauses ~S must be list type.", test, NULL);
		if (body == Nil)
			consnil_heap(&body);
		list_heap(&list, quote, test, NULL);
		list_heap(&list, typep, g, list, NULL);
		cons_heap(&type, test, type);
		cons_heap(&list, list, body);
		cons_heap(&root, list, root);
	}

	/* error */
	list_heap(&invoke, T, invoke, NULL);
	cons_heap(&root, invoke, root);
	nreverse(&root, root);
	cons_heap(ret, cond, root);

	return 0;
}

static int ctypecase_expand_common_(Execute ptr,
		addr env, addr *ret, addr place, addr args)
{
	/* (let* ((a1 b1) (a2 b2) ... (value r) g)
	 *   (declare (ignorable a1 a2 ...))
	 *   (block result
	 *     (tagbody
	 *       loop
	 *       (restart-bind
	 *         ((store-value
	 *            (lambda (v) (setq g v value v) w (go loop))
	 *            :report-function
	 *              (lambda (s)
	 *                (princ "Retry ctypecase with new value xx." s))
	 *            :interactive-function
	 *              (lambda ()
	 *                (list (eval (prompt-for t "Input xx> "))))))
	 *         (return-from result ...)))))
	 */
	addr a, b, g, r, w, v, s, str1, str2;
	addr leta, declare, ignorable, tagbody, loop, restart, store, lambda, setq;
	addr value, go, report, inter, princ, list, eval, prompt, cond;
	addr x, y, root, type, block, retfrom, result;
	LocalHold hold;

	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	hold = LocalHold_local(ptr);
	localhold_pushva(hold, a, b, g, w, r, NULL);

	Return_getcar(g, &g);
	Return(format_string_(ptr, &str1,
				"Retry ctypecase with new value ~A.", place, NULL));
	localhold_push(hold, str1);
	Return(format_string_(ptr, &str2,
				"Input ~A> ", place, NULL));
	localhold_push(hold, str2);
	Return(ctypecase_string_common_(ptr, &type, args));
	localhold_end(hold);

	make_symbolchar(&v, "V");
	make_symbolchar(&s, "STREAM");
	make_symbolchar(&loop, "LOOP");
	make_symbolchar(&value, "VALUE");
	make_symbolchar(&result, "RESULT");
	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_RESTART_BIND, &restart);
	GetConst(COMMON_STORE_VALUE, &store);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_GO, &go);
	GetConst(KEYWORD_REPORT_FUNCTION, &report);
	GetConst(KEYWORD_INTERACTIVE_FUNCTION, &inter);
	GetConst(COMMON_PRINC, &princ);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_EVAL, &eval);
	GetConst(SYSTEM_PROMPT_FOR, &prompt);
	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_RETURN_FROM, &retfrom);

	/* expand */
	Return(ctypecase_cond_common_(value, type, args, &cond));
	list_heap(&cond, retfrom, result, cond, NULL);
	list_heap(&prompt, prompt, T, str2, NULL);
	list_heap(&eval, eval, prompt, NULL);
	list_heap(&list, list, eval, NULL);
	list_heap(&x, lambda, Nil, list, NULL);
	list_heap(&princ, princ, str1, s, NULL);
	list_heap(&s, s, NULL);
	list_heap(&y, lambda, s, princ, NULL);
	list_heap(&go, go, loop, NULL);
	list_heap(&setq, setq, g, v, value, v, NULL);
	list_heap(&v, v, NULL);
	list_heap(&lambda, lambda, v, setq, w, go, NULL);
	list_heap(&store, store, lambda, report, y, inter, x, NULL);
	list_heap(&store, store, NULL);
	list_heap(&restart, restart, store, cond, NULL);
	list_heap(&tagbody, tagbody, loop, restart, NULL);
	list_heap(&tagbody, block, result, tagbody, NULL);
	/* let* */
	lista_heap(&ignorable, ignorable, a, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	for (root = Nil; a != Nil; ) {
		Return_getcons(a, &x, &a);
		Return_getcons(b, &y, &b);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}
	list_heap(&value, value, r, NULL);
	cons_heap(&root, value, root);
	cons_heap(&root, g, root);
	nreverse(&root, root);
	list_heap(ret, leta, root, declare, tagbody, NULL);

	return 0;
}

int ctypecase_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, x;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, form, env, NULL);
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &x, &args))
		goto error;
	Return(ctypecase_expand_common_(ptr, env, ret, x, args));
	localhold_end(hold);
	return 0;

error:
	*ret = Nil;
	return fmte_("CTYPECASE arguments ~S must be (place &rest args) form.", form, NULL);
}


/*
 *  multiple-value-bind
 */
int multiple_value_bind_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr body, list, pos, vars, expr, doc, decl;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, form, env, NULL);
	/* argument */
	Return_getcdr(form, &body);
	if (! consp_getcons(body, &vars, &body))
		goto error;
	for (list = vars; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(check_variable_(pos));
	}
	if (! consp_getcons(body, &expr, &body))
		goto error;
	/* extract */
	Return(declare_body_documentation_(ptr, env, body, &doc, &decl, &body));
	localhold_end(hold);
	GetConst(SYSTEM_MULTIPLE_VALUE_BIND, &pos);
	list_heap(ret, pos, vars, expr, decl, doc, body, form, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("The multiple-value-bind argument must be a "
			"((vars*) expr &body body) form.", NULL);
}


/*
 *  multiple-value-list
 */
int multiple_value_list_common_(addr form, addr env, addr *ret)
{
	addr args, expr, symbol, func, list;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &expr, &args))
		goto error;
	if (args != Nil)
		goto error;

	/* `(multiple-value-call #'list ,expr) */
	GetConst(COMMON_MULTIPLE_VALUE_CALL, &symbol);
	GetConst(COMMON_FUNCTION, &func);
	GetConst(COMMON_LIST, &list);
	list_heap(&list, func, list, NULL);
	list_heap(ret, symbol, list, expr, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("The multiple-value-list argument ~S "
			"must be a single list.", form, NULL);
}


/*
 *  multiple-value-setq
 */
int multiple_value_setq_common_(addr form, addr env, addr *ret)
{
	addr args, vars, expr, values, setf;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &vars, &args))
		goto error;
	if (! consp_getcons(args, &expr, &args))
		goto error;
	if (args != Nil)
		goto error;

	/* `(values (setf (values ,@vars) ,expr)) */
	GetConst(COMMON_VALUES, &values);
	GetConst(COMMON_SETF, &setf);
	cons_heap(&vars, values, vars);
	list_heap(&vars, setf, vars, expr, NULL);
	list_heap(ret, values, vars, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("The multiple-value-setq arguments ~S "
			"must be a (vars form).", form, NULL);
}


/*
 *  nth-value
 */
int nth_value_common_(addr form, addr env, addr *ret)
{
	addr list, args, nth, expr, nth_value;

	Return_getcdr(form, &list);
	if (! consp_getcons(list, &nth, &args))
		goto error;
	if (! consp_getcons(args, &expr, &args))
		goto error;
	if (args != Nil)
		goto error;
	GetConst(SYSTEM_NTH_VALUE, &nth_value);
	list_heap(ret, nth_value, nth, expr, form, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("NTH-VALUE argument ~S must be (nth expr) form.", form, NULL);
}


/*
 *  prog
 */
static int function_prog_constant_(addr form, addr *ret,
		constindex prog_constant,
		constindex let_constant)
{
	/*  `(block nil
	 *    (let ,var
	 *      ,@decl
	 *      (tagbody ,@body)))
	 */
	addr var, decl, root, pos;
	addr let, block, tagbody;

	/* argument */
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &var, &form)) {
		GetConstant(prog_constant, &var);
		return fmte_("~A argument ~S must be ([var] &rest body) form.", var, form, NULL);
	}
	Return(declare_body_form_(form, &decl, &form));

	/* expand */
	GetConstant(let_constant, &let);
	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_TAGBODY, &tagbody);
	/* (tagbody ...) */
	cons_heap(&form, tagbody, form);
	/* (let ...) */
	conscar_heap(&root, let);
	cons_heap(&root, var, root);
	while (decl != Nil) {
		GetCons(decl, &pos, &decl);
		cons_heap(&root, pos, root);
	}
	cons_heap(&root, form, root);
	nreverse(&root, root);
	/* (block ...) */
	list_heap(ret, block, Nil, root, NULL);

	return 0;
}

int prog_common_(addr form, addr env, addr *ret)
{
	return function_prog_constant_(form, ret,
			CONSTANT_COMMON_PROG,
			CONSTANT_COMMON_LET);
}


/*
 *  prog*
 */
int proga_common_(addr form, addr env, addr *ret)
{
	return function_prog_constant_(form, ret,
			CONSTANT_COMMON_PROGA,
			CONSTANT_COMMON_LETA);
}


/*
 *  prog1
 */
int prog1_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr expr, g, let, root;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &expr, &form))
		return fmte_("PROG1 arguemnt ~S must be (form1 &body body) form.", form, NULL);
	if (form == Nil)
		return Result(ret, expr);
	/* `(let ((,g ,expr)) ,@form ,g) */
	Return(make_gensym_(ptr, &g));
	GetConst(COMMON_LET, &let);
	list_heap(&expr, g, expr, NULL);
	conscar_heap(&expr, expr);
	conscar_heap(&root, let);
	cons_heap(&root, expr, root);
	while (form != Nil) {
		if (! consp_getcons(form, &expr, &form))
			return fmte_("PROG1 argument ~S don't accept a dotted list.", form, NULL);
		cons_heap(&root, expr, root);
	}
	cons_heap(&root, g, root);
	nreverse(ret, root);

	return 0;
}


/*
 *  prog2
 */
int prog2_common_(addr form, addr env, addr *ret)
{
	addr expr, progn, prog1;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &expr, &form))
		goto error;
	if (! consp(form))
		goto error;
	/* `(progn ,expr (prog1 ,@form)) */
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_PROG1, &prog1);
	cons_heap(&prog1, prog1, form);
	list_heap(ret, progn, expr, prog1, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("PROG2 arguemnt ~S "
			"must be (form1 form2 &body body) form.", form, NULL);
}


/*
 *  define-modify-macro
 */
static int define_modify_macro_expand_(LocalRoot local, addr *ret,
		addr name, addr args, addr call, addr doc)
{
	addr place, env, key, declare, ignore, append;
	addr defmacro, mvbind, expansion, quote, let, mapcar, function;
	addr list, lista, vars, x, cons, values;
	addr a, b, g, w, r, qmvbind, list1, list2, list3, rest;

	/* define-modify-macro lambda list */
	Return(define_modify_macro_heap_(local, &list1, &rest, args));
	GetConst(AMPERSAND_ENVIRONMENT, &key);
	make_symbolchar(&env, "ENV");
	make_symbolchar(&place, "PLACE");
	lista_heap(&args, key, env, place, args, NULL);

	/* expand
	 *
	 * `(defmacro name (&environment env place ,args)
	 *    ,doc
	 *    (multiple-value-bind (a b g w r) (get-setf-expansion ',place env)
	 *      `(let ,(mapcar #'list a b)
	 *         (declare (ignorable ,@a ,@b))
	 *         (multiple-value-bind ,g (call ,r ,v1 ,v2 ,v3 ... ,@rest)
	 *           ,w
	 *           (values ,@g)))))
	 *
	 *  (defmacro name (&environment env place args...)
	 *    doc
	 *    (multiple-value-bind (a b g w r) (get-setf-expansion (quote place) env)
	 *      (list (quote let) (mapcar (function list) a b)
	 *        (list (quote multiple-value-bind) g
	 *          (list (quote declare) (append (list (quote ignorable)) a))
	 *          (list* (quote call) r v1 v2 v3 ... rest)
	 *          w
	 *          (cons (quote values) g)))))
	 */
	GetConst(COMMON_DEFMACRO, &defmacro);
	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(COMMON_GET_SETF_EXPANSION, &expansion);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_MAPCAR, &mapcar);
	GetConst(COMMON_FUNCTION, &function);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_LISTA, &lista);
	GetConst(COMMON_CONS, &cons);
	GetConst(COMMON_VALUES, &values);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignore);
	GetConst(COMMON_APPEND, &append);
	make_symbolchar(&a, "A");
	make_symbolchar(&b, "B");
	make_symbolchar(&g, "G");
	make_symbolchar(&w, "W");
	make_symbolchar(&r, "R");
	list_heap(&values, quote, values, NULL);
	list_heap(&cons, cons, values, g, NULL);
	list_heap(&call, quote, call, NULL);

	if (rest == Nil) {
		/* (list 'call r v1 v2 o1 o2) */
		conscar_heap(&vars, list);
		cons_heap(&vars, call, vars);
		cons_heap(&vars, r, vars);
		while (list1 != Nil) {
			GetCons(list1, &x, &list1);
			cons_heap(&vars, x, vars);
		}
		nreverse(&vars, vars);
	}
	else {
		/* (list* 'call r v1 v2 o1 o2 rest) */
		conscar_heap(&vars, lista);
		cons_heap(&vars, call, vars);
		cons_heap(&vars, r, vars);
		while (list1 != Nil) {
			GetCons(list1, &x, &list1);
			cons_heap(&vars, x, vars);
		}
		cons_heap(&vars, rest, vars);
		nreverse(&vars, vars);
	}

	list_heap(&qmvbind, quote, mvbind, NULL);
	list_heap(&list2, list, qmvbind, g, vars, w, cons, NULL);
	list_heap(&function, function, list, NULL);
	list_heap(&mapcar, mapcar, function, a, b, NULL);
	list_heap(&let, quote, let, NULL);
	list_heap(&list1, list, let, mapcar, list2, NULL);
	list_heap(&expansion, expansion, place, env, NULL);
	list_heap(&list3, a, b, g, w, r, NULL);

	list_heap(&declare, quote, declare, NULL);
	list_heap(&ignore, quote, ignore, NULL);
	list_heap(&ignore, list, ignore, NULL);
	list_heap(&append, append, ignore, a, NULL);
	list_heap(&declare, list, declare, append, NULL);

	list_heap(&mvbind, mvbind, list3, expansion, declare, list1, NULL);
	list_heap(ret, defmacro, name, args, doc, mvbind, NULL);

	return 0;
}

int define_modify_macro_common_(LocalRoot local, addr form, addr env, addr *ret)
{
	addr args, name, lambda, call, doc;

	CheckLocal(local);
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &name, &args))
		goto error;
	if (! consp_getcons(args, &lambda, &args))
		goto error;
	if (! consp_getcons(args, &call, &args))
		goto error;
	if (args == Nil) {
		doc = Nil;
		goto expand;
	}
	GetCons(args, &doc, &args);
	if (args != Nil)
		goto error;
	if (! stringp(doc)) {
		return fmte_("DEFINE-MODIFY-MACRO documentation ~S "
				"must be a string type.", doc, NULL);
	}
expand:
	return define_modify_macro_expand_(local, ret, name, lambda, call, doc);

error:
	*ret = Nil;
	return fmte_("DEFINE-MODIFY-MACRO argument ~S must be "
			"(name lambda-list functionn &optional documentation) "
			"form.", form, NULL);
}


/*
 *  defsetf-short
 */
static int defsetf_short_common_(Execute ptr, addr name, addr call, addr doc, addr *ret)
{
	/*  (defmacro defsetf-short (name call &optional doc)
	 *    (let ((args (gensym))
	 *          (a (make-symbol "A"))
	 *          (g (make-symbol "G")))
	 *      `(define-setf-expander ,name (&rest ,args)
	 *         ,doc
	 *         (let ((,a (mapcar (lambda (,a)
	 *                             (declare (ignore ,a))
	 *                             (gensym))
	 *                     ,args)))
	 *           (values
	 *             ,a
	 *             ,args
	 *             (list ',g)
	 *             (append (list ',call) ,a (list ',g))
	 *             (append (list ',name) ,a))))))
	 */
	addr args, a, g, w, r, x;
	addr define, rest, let, mapcar, lambda, declare, ignore;
	addr gensym, values, append, list, quote;

	Return(make_gensym_(ptr, &args));
	make_symbolchar(&a, "A");
	make_symbolchar(&g, "G");

	GetConst(COMMON_DEFINE_SETF_EXPANDER, &define);
	GetConst(AMPERSAND_REST, &rest);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_MAPCAR, &mapcar);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORE, &ignore);
	GetConst(COMMON_GENSYM, &gensym);
	GetConst(COMMON_VALUES, &values);
	GetConst(COMMON_APPEND, &append);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_QUOTE, &quote);

	list_heap(&g, quote, g, NULL);
	list_heap(&g, list, g, NULL);
	list_heap(&w, quote, call, NULL);
	list_heap(&w, list, w, NULL);
	list_heap(&w, append, w, a, g, NULL);
	list_heap(&r, quote, name, NULL);
	list_heap(&r, list, r, NULL);
	list_heap(&r, append, r, a, NULL);
	list_heap(&values, values, a, args, g, w, r, NULL);
	list_heap(&ignore, ignore, a, NULL);
	list_heap(&declare, declare, ignore, NULL);
	list_heap(&x, a, NULL);
	list_heap(&gensym, gensym, NULL);
	list_heap(&lambda, lambda, x, declare, gensym, NULL);
	list_heap(&mapcar, mapcar, lambda, args, NULL);
	list_heap(&a, a, mapcar, NULL);
	list_heap(&a, a, NULL);
	list_heap(&let, let, a, values, NULL);
	list_heap(&rest, rest, args, NULL);
	list_heap(ret, define, name, rest, doc, let, NULL);

	return 0;
}


/*
 *  defsetf-long
 */
static int defsetf_long_var_(Execute ptr, addr list, addr *args, addr *a, addr *b)
{
	addr x, g;

	while (list != Nil) {
		GetCons(list, &x, &list);
		Return(make_gensym_(ptr, &g));
		cons_heap(args, g, *args);
		cons_heap(a, x, *a);
		cons_heap(b, g, *b);
	}

	return 0;
}

static int defsetf_long_opt_(Execute ptr, addr list, addr *args, addr *a, addr *b)
{
	addr x, g, h, var, init, sup;

	if (list == Nil)
		return 0;
	GetConst(AMPERSAND_OPTIONAL, &x);
	cons_heap(args, x, *args);

	while (list != Nil) {
		GetCons(list, &x, &list);
		List_bind(x, &var, &init, &sup, NULL);

		/* (var init sup) */
		if (sup != Nil) {
			Return(make_gensym_(ptr, &g));
			Return(make_gensym_(ptr, &h));
			list_heap(&x, g, init, h, NULL);
			cons_heap(args, x, *args);
			cons_heap(a, var, *a);
			cons_heap(b, g, *b);
			cons_heap(a, sup, *a);
			cons_heap(b, h, *b);
			continue;
		}

		/* (var init) */
		if (init != Nil) {
			Return(make_gensym_(ptr, &g));
			list_heap(&x, g, init, NULL);
			cons_heap(args, x, *args);
			cons_heap(a, var, *a);
			cons_heap(b, g, *b);
			continue;
		}

		/* var */
		Return(make_gensym_(ptr, &g));
		cons_heap(args, g, *args);
		cons_heap(a, var, *a);
		cons_heap(b, g, *b);
	}

	return 0;
}

static int defsetf_long_rest_(Execute ptr, addr rest, addr *args, addr *a, addr *b)
{
	addr x, g;

	if (rest == Nil)
		return 0;
	GetConst(AMPERSAND_REST, &x);
	cons_heap(args, x, *args);

	Return(make_gensym_(ptr, &g));
	cons_heap(args, g, *args);
	cons_heap(a, rest, *a);
	cons_heap(b, g, *b);

	return 0;
}

static int defsetf_long_key_(Execute ptr, addr list, addr *args, addr *a, addr *b)
{
	addr x, g, h, var, name, init, sup;

	if (list == Nil)
		return 0;
	GetConst(AMPERSAND_KEY, &x);
	cons_heap(args, x, *args);

	while (list != Nil) {
		GetCons(list, &x, &list);
		List_bind(x, &var, &name, &init, &sup, NULL);

		/* (var init sup) */
		if (sup != Nil) {
			Return(make_gensym_(ptr, &g));
			Return(make_gensym_(ptr, &h));
			list_heap(&name, name, g, NULL);
			list_heap(&x, name, init, h, NULL);
			cons_heap(args, x, *args);
			cons_heap(a, var, *a);
			cons_heap(b, g, *b);
			cons_heap(a, sup, *a);
			cons_heap(b, h, *b);
			continue;
		}

		/* (var init) */
		if (init != Nil) {
			Return(make_gensym_(ptr, &g));
			list_heap(&name, name, g, NULL);
			list_heap(&x, name, init, NULL);
			cons_heap(args, x, *args);
			cons_heap(a, var, *a);
			cons_heap(b, g, *b);
			continue;
		}

		/* var */
		Return(make_gensym_(ptr, &g));
		list_heap(&name, name, g, NULL);
		cons_heap(args, name, *args);
		cons_heap(a, var, *a);
		cons_heap(b, g, *b);
	}

	return 0;
}

static int defsetf_long_lambda_(Execute ptr,
		addr lambda, addr *rargs, addr *ra, addr *rb, addr *renv)
{
	addr var, opt, rest, key, allow;
	addr args, a, b;

	Return(lambda_defsetf_(ptr->local, &lambda, lambda));
	List_bind(lambda, &var, &opt, &rest, &key, &allow, renv, NULL);
	args = a = b = Nil;

	Return(defsetf_long_var_(ptr, var, &args, &a, &b));
	Return(defsetf_long_opt_(ptr, opt, &args, &a, &b));
	Return(defsetf_long_rest_(ptr, rest, &args, &a, &b));
	Return(defsetf_long_key_(ptr, key, &args, &a, &b));

	nreverse(rargs, args);
	nreverse(ra, a);
	nreverse(rb, b);

	return 0;
}

static int defsetf_long_store_(Execute ptr, addr store, addr *ret)
{
	addr list, g;

	list = Nil;
	while (store != Nil) {
		GetCdr(store, &store);
		Return(make_gensym_(ptr, &g));
		cons_heap(&list, g, list);
	}
	nreverse(ret, list);

	return 0;
}

static void defsetf_long_values_g(addr v3, addr *ret)
{
	addr quote;

	GetConst(COMMON_QUOTE, &quote);
	list_heap(ret, quote, v3, NULL);
}

static void defsetf_long_values_r(addr name, addr a, addr v3, addr *ret)
{
	/* r: (list* ',name 'g3 'g4 a) */
	addr lista, quote, root, x;

	GetConst(COMMON_LISTA, &lista);
	GetConst(COMMON_QUOTE, &quote);

	conscar_heap(&root, lista);
	list_heap(&x, quote, name, NULL);
	cons_heap(&root, x, root);
	while (v3 != Nil) {
		GetCons(v3, &x, &v3);
		list_heap(&x, quote, x, NULL);
		cons_heap(&root, x, root);
	}
	cons_heap(&root, a, root);
	nreverse(ret, root);
}

static void defsetf_long_values_w(addr v1, addr v2, addr v3, addr store,
		addr args, addr decl, addr body, addr a, addr *ret)
{
	addr declare, lambda, let, quote, list, cons;
	addr root, x, y;

	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_CONS, &cons);

	/* lambda-list */
	root = Nil;
	while (v1 != Nil) {
		GetCons(v1, &x, &v1);
		GetCons(v2, &y, &v2);
		list_heap(&y, quote, y, NULL);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}

	/* store */
	while (store != Nil) {
		GetCons(store, &x, &store);
		GetCons(v3, &y, &v3);
		list_heap(&y, quote, y, NULL);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}
	nreverse(&root, root);

	/* lambda */
	list_heap(&lambda, quote, lambda, NULL);
	list_heap(&args, quote, args, NULL);
	lista_heap(&lambda, list, lambda, args, body, NULL);
	list_heap(&body, cons, lambda, a, NULL);

	/* let */
	cons_heap(&declare, declare, decl);
	list_heap(ret, let, root, declare, body, NULL);
}

static int defsetf_long_common_(Execute ptr, addr name,
		addr args, addr store, addr body, addr *ret)
{
	/* `(define-setf-expander ,name (&rest ,b &environment env)
	 *    ,doc
	 *    (let ((,a (mapcar (lambda (,a)
	 *                        (declare (ignore ,a))
	 *                        (gensym))
	 *                      ,b)))
	 *      (values ,a
	 *              ,b
	 *              '(g3 g4 ...)
	 *              (let ((v1 'g1)
	 *                    (v2 'g2)
	 *                    ...
	 *                    (store1 'g3)
	 *                    (store2 'g4)
	 *                    ...)
	 *                (declare ,@decl)
	 *                `((lambda ,[args]
	 *                    ,@body)
	 *                  ,@a))
	 *              (list* ',name 'g3 'g4 a))))
	 */
	addr define, let, mapcar, lambda, declare, ignore, gensym, values;
	addr arest, aenv, doc, decl, a, b, g, w, r, x;
	addr v1, v2, v3, env;

	GetConst(COMMON_DEFINE_SETF_EXPANDER, &define);
	GetConst(AMPERSAND_REST, &arest);
	GetConst(AMPERSAND_ENVIRONMENT, &aenv);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_MAPCAR, &mapcar);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORE, &ignore);
	GetConst(COMMON_GENSYM, &gensym);
	GetConst(COMMON_VALUES, &values);

	Return(make_gensym_(ptr, &a));
	Return(make_gensym_(ptr, &b));
	Return(split_decl_body_doc_(body, &doc, &decl, &body));

	/* values */
	Return(defsetf_long_lambda_(ptr, args, &args, &v1, &v2, &env));
	Return(defsetf_long_store_(ptr, store, &v3));
	defsetf_long_values_g(v3, &g);
	defsetf_long_values_w(v1, v2, v3, store, args, decl, body, a, &w);
	defsetf_long_values_r(name, a, v3, &r);
	list_heap(&values, values, a, b, g, w, r, NULL);
	/* let */
	list_heap(&gensym, gensym, NULL);
	list_heap(&ignore, ignore, a, NULL);
	list_heap(&declare, declare, ignore, NULL);
	list_heap(&x, a, NULL);
	list_heap(&x, lambda, x, declare, gensym, NULL);
	list_heap(&x, mapcar, x, b, NULL);
	list_heap(&x, a, x, NULL);
	list_heap(&x, x, NULL);
	list_heap(&let, let, x, values, NULL);
	if (env == Nil)
		list_heap(&x, arest, b, NULL);
	else
		list_heap(&x, arest, b, aenv, env, NULL);
	list_heap(ret, define, name, x, doc, let, NULL);

	return 0;
}

int defsetf_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, arg1, arg2, arg3;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &arg1, &args))
		goto error;
	if (! consp_getcons(args, &arg2, &args))
		goto error;
	if (listp(arg2)) {
		/* long form */
		if (! consp_getcons(args, &arg3, &args))
			return fmte_("Invalid defsetf long form ~S.", form, NULL);
		if (! listp(arg3))
			return fmte_("defsetf argument ~S must be a list type.", arg3, NULL);
		Return(defsetf_long_common_(ptr, arg1, arg2, arg3, args, &form));
	}
	else if (args == Nil) {
		/* short form */
		Return(defsetf_short_common_(ptr, arg1, arg2, Nil, &form));
	}
	else {
		/* short form, documentation */
		if (! consp_getcons(args, &arg3, &args))
			return fmte_("Invalid defsetf short form ~S.", form, NULL);
		if (args != Nil)
			return fmte_("Invalid defsetf short form ~S.", form, NULL);
		if (! stringp(arg3))
			return fmte_("defsetf documentation ~S must be a string type.", arg3, NULL);
		Return(defsetf_short_common_(ptr, arg1, arg2, arg3, &form));
	}
	return Result(ret, form);

error:
	*ret = Nil;
	return fmte_("Invalid defsetf form ~S.", form, NULL);
}


/*
 *  define-setf-expander
 */
int define_setf_expander_common_(addr form, addr env, addr *ret)
{
	/* `(system::define-setf-expander
	 *     ',access
	 *     (system::macro-lambda ,@args))
	 */
	addr access, args, define, lambda, quote;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &access, &args))
		goto error;
	if (! consp(args))
		goto error;
	/* expand */
	GetConst(SYSTEM_DEFINE_SETF_EXPANDER, &define);
	GetConst(SYSTEM_MACRO_LAMBDA, &lambda);
	GetConst(COMMON_QUOTE, &quote);
	cons_heap(&lambda, lambda, args);
	list_heap(&access, quote, access, NULL);
	list_heap(ret, define, access, lambda, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("DEFINE-SETF-EXPANDER argument ~S "
			"must be (access lambda-list &rest body) form.", form, NULL);
}


/*
 *  setf
 */
static int setf_single_common_(addr *ret, addr value,
		addr var1, addr var2, addr store, addr writer, addr reader)
{
	/* (let* ((g1 a1)
	 *        (g2 a2)
	 *        (g value))
	 *   (declare (ignorable g1 g2))
	 *     writer)
	 */
	addr list1, list2, args, a, b, leta, declare, ignorable;

	list1 = var1;
	list2 = var2;
	args = Nil;
	while (list1 != Nil) {
		Return_getcons(list1, &a, &list1);
		Return_getcons(list2, &b, &list2);
		list_heap(&a, a, b, NULL);
		cons_heap(&args, a, args);
	}
	GetCar(store, &store);
	list_heap(&a, store, value, NULL);
	cons_heap(&args, a, args);
	nreverse(&args, args);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, var1);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	list_heap(ret, leta, args, declare, writer, NULL);

	return 0;
}

static int setf_multiple_common_(addr *ret, addr value,
		addr var1, addr var2, addr store, addr writer, addr reader)
{
	/* (let* ((g1 a1)
	 *        (g2 a2))
	 *   (declare (ignorable g1 g2))
	 *   (multiple-value-bind (g ...) value
	 *     writer
	 *     (values g1 ...)))
	 */
	addr list1, list2, args, a, b, leta, declare, ignorable, bind;

	list1 = var1;
	list2 = var2;
	args = Nil;
	while (list1 != Nil) {
		Return_getcons(list1, &a, &list1);
		Return_getcons(list2, &b, &list2);
		list_heap(&a, a, b, NULL);
		cons_heap(&args, a, args);
	}
	nreverse(&args, args);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, var1);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* multiple-value-bind */
	GetConst(COMMON_MULTIPLE_VALUE_BIND, &bind);
	list_heap(&bind, bind, store, value, writer, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	list_heap(ret, leta, args, declare, bind, NULL);

	return 0;
}

static int setf_expr_common_(Execute ptr, addr *ret, addr key, addr value, addr env)
{
	addr a, b, g, w, r;

	Return(get_setf_expansion_(ptr, key, env, &a, &b, &g, &w, &r));
	if (singlep(g))
		return setf_single_common_(ret, value, a, b, g, w, r);
	else
		return setf_multiple_common_(ret, value, a, b, g, w, r);
}

int setf_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr key, value, root, progn;
	LocalHold hold;

	Return_getcdr(form, &form);
	if (form == Nil)
		return Result(ret, Nil);

	hold = LocalHold_array(ptr, 3);
	localhold_set(hold, 0, form);
	localhold_set(hold, 1, env);
	for (root = Nil; form != Nil; ) {
		if (! consp_getcons(form, &key, &form))
			goto error;
		if (! consp_getcons(form, &value, &form))
			goto error;
		Return(setf_expr_common_(ptr, &key, key, value, env));
		cons_heap(&root, key, root);
		localhold_set(hold, 2, root);
	}
	localhold_end(hold);
	nreverse(&root, root);

	/* (progn ...) */
	GetConst(COMMON_PROGN, &progn);
	cons_heap(ret, progn, root);
	return 0;

error:
	*ret = Nil;
	return fmte_("The setf form ~S must be a place value form.", form, NULL);
}


/*
 *  shiftf
 */
static int shiftf_list2_common_(addr *ret, addr a, addr b, addr root)
{
	addr x, y;

	while (a != Nil) {
		Return_getcons(a, &x, &a);
		Return_getcons(b, &y, &b);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}

	return Result(ret, root);
}

static void shiftf_ignorable_common(addr *ret, addr list)
{
	addr declare, ignorable;

	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, list);
	list_heap(ret, declare, ignorable, NULL);
}

static void shiftf_mvbind_common(addr *ret, addr g, addr r, addr body)
{
	addr mvbind, declare;

	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	shiftf_ignorable_common(&declare, g);
	lista_heap(ret, mvbind, g, r, declare, body, NULL);
}

int shiftf_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, root, pos, let, declare, prog1;
	addr a, b, g, w, r, r0, alist, glist, wlist, rlist;
	LocalHold hold;

	/* (shiftf x0 x1 x2 value)
	 * (let* (...)
	 *   (multiple-value-prog1 r0
	 *     (multiple-value-bind (g0) r1
	 *     (declare (ignorable g0))
	 *       (multiple-value-bind (g1) r2
	 *       (declare (ignorable g1))
	 *         (multiple-value-bind (g2) value
	 *         (declare (ignorable g2))
	 *           w0 w1 w2)))))
	 */
	hold = LocalHold_array(ptr, 4);
	localhold_pushva_force(hold, form, env, NULL);

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &pos, &args))
		goto error;
	if (! consp(args))
		goto error;

	/* push */
	Return(get_setf_expansion_(ptr, pos, env, &a, &b, &g, &w, &r0));
	alist = glist = wlist = rlist = Nil;
	Return(shiftf_list2_common_(&alist, a, b, alist));
	cons_heap(&glist, g, glist);
	cons_heap(&wlist, w, wlist);
	localhold_set(hold, 0, alist);
	localhold_set(hold, 1, glist);
	localhold_set(hold, 2, wlist);
	for (;;) {
		Return_getcons(args, &pos, &args);
		if (args == Nil)
			break;
		Return(get_setf_expansion_(ptr, pos, env, &a, &b, &g, &w, &r));
		Return(shiftf_list2_common_(&alist, a, b, alist));
		cons_heap(&glist, g, glist);
		cons_heap(&wlist, w, wlist);
		cons_heap(&rlist, r, rlist);
		localhold_set(hold, 0, alist);
		localhold_set(hold, 1, glist);
		localhold_set(hold, 2, wlist);
		localhold_set(hold, 3, rlist);
	}
	localhold_end(hold);

	/* last expand */
	nreverse(&wlist, wlist);
	GetCons(glist, &g, &glist);
	shiftf_mvbind_common(&root, g, pos, wlist);

	/* loop expand */
	while (glist != Nil) {
		GetCons(glist, &g, &glist);
		GetCons(rlist, &r, &rlist);
		list_heap(&root, root, NULL);
		shiftf_mvbind_common(&root, g, r, root);
	}

	/* multiple-value-prog1 */
	GetConst(COMMON_MULTIPLE_VALUE_PROG1, &prog1);
	list_heap(&root, prog1, r0, root, NULL);

	/* let expand */
	if (alist != Nil) {
		nreverse(&alist, alist);
		GetConst(COMMON_LETA, &let);
		shiftf_ignorable_common(&declare, a);
		list_heap(&root, let, alist, declare, root, NULL);
	}

	/* result */
	return Result(ret, root);

error:
	*ret = Nil;
	return fmte_("SHIFT argument ~S must be (place ... value) form.", form, NULL);
}


/*
 *  rotatef
 */
int rotatef_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, root, pos, let, declare;
	addr a, b, g, w, r, r0, alist, glist, wlist, rlist;
	LocalHold hold;

	hold = LocalHold_array(ptr, 4);
	localhold_pushva_force(hold, form, env, NULL);

	Return_getcdr(form, &form);
	if (form == Nil) {
		localhold_end(hold);
		return Result(ret, Nil);
	}
	if (! consp_getcons(form, &pos, &args))
		goto error;
	if (args == Nil) {
		/* (progn pos nil) */
		GetConst(COMMON_PROGN, &root);
		list_heap(ret, root, pos, Nil, NULL);
		localhold_end(hold);
		return 0;
	}
	if (! consp(args))
		goto error;

	/* push */
	Return(get_setf_expansion_(ptr, pos, env, &a, &b, &g, &w, &r0));
	alist = glist = wlist = rlist = Nil;
	Return(shiftf_list2_common_(&alist, a, b, alist));
	cons_heap(&glist, g, glist);
	cons_heap(&wlist, w, wlist);
	localhold_set(hold, 0, alist);
	localhold_set(hold, 1, glist);
	localhold_set(hold, 2, wlist);
	while (args != Nil) {
		Return_getcons(args, &pos, &args);
		Return(get_setf_expansion_(ptr, pos, env, &a, &b, &g, &w, &r));
		Return(shiftf_list2_common_(&alist, a, b, alist));
		cons_heap(&glist, g, glist);
		cons_heap(&wlist, w, wlist);
		cons_heap(&rlist, r, rlist);
		localhold_set(hold, 0, alist);
		localhold_set(hold, 1, glist);
		localhold_set(hold, 2, wlist);
		localhold_set(hold, 3, rlist);
	}
	localhold_end(hold);

	/* last expand */
	cons_heap(&wlist, Nil, wlist);
	nreverse(&wlist, wlist);
	GetCons(glist, &g, &glist);
	shiftf_mvbind_common(&root, g, r0, wlist);

	/* loop expand */
	while (glist != Nil) {
		GetCons(glist, &g, &glist);
		GetCons(rlist, &r, &rlist);
		list_heap(&root, root, NULL);
		shiftf_mvbind_common(&root, g, r, root);
	}

	/* let expand */
	if (alist != Nil) {
		nreverse(&alist, alist);
		GetConst(COMMON_LETA, &let);
		shiftf_ignorable_common(&declare, a);
		list_heap(&root, let, alist, declare, root, NULL);
	}

	/* result */
	return Result(ret, root);

error:
	*ret = Nil;
	return fmte_("ROTATEF argument ~S don't accept a dotted list.", form, NULL);
}


/************************************************************
 *  call_environment.c
 ************************************************************/


/*
 *  decode-universal-time
 */
int decode_universal_time_common_(LocalRoot local, addr pos, addr zone,
		addr *rsecond, addr *rminute, addr *rhour,
		addr *rdate, addr *rmonth, addr *ryear,
		addr *rweek, addr *rdaylight, addr *rzone)
{
	struct universal_time_struct u;

	if (zone == Unbound)
		zone = Nil;
	Return(decode_universal_time_call_(local, &u, pos, zone));
	*rsecond = u.second;
	*rminute = u.minute;
	*rhour = u.hour;
	*rdate = u.date;
	*rmonth = u.month;
	*ryear = u.year;
	*rweek = u.week;
	*rdaylight = u.daylight_p;
	*rzone = u.zone;

	return 0;
}


/*
 *  encode-universal-time
 */
int encode_universal_time_common_(LocalRoot local, addr rest, addr *ret)
{
	addr s, mi, h, d, m, y, z;

	if (! consp_getcons(rest, &s, &rest))
		goto error;
	if (! consp_getcons(rest, &mi, &rest))
		goto error;
	if (! consp_getcons(rest, &h, &rest))
		goto error;
	if (! consp_getcons(rest, &d, &rest))
		goto error;
	if (! consp_getcons(rest, &m, &rest))
		goto error;
	if (! consp_getcons(rest, &y, &rest))
		goto error;
	if (! consp_getcons(rest, &z, &rest))
		z = Unbound;
	if (consp(rest))
		goto error;
	return encode_universal_time_call_(local, ret, s, mi, h, d, m, y, z);

error:
	return fmte_("Invalid argument ENCODE-UNIVERSAL-TIME.", NULL);
}


/*
 *  get-decoded-time
 */
int get_decoded_time_common_(LocalRoot local,
		addr *rsecond, addr *rminute, addr *rhour,
		addr *rdate, addr *rmonth, addr *ryear,
		addr *rweek, addr *rdaylight, addr *rzone)
{
	struct universal_time_struct u;

	Return(get_decoded_time_call_(local, &u));
	*rsecond = u.second;
	*rminute = u.minute;
	*rhour = u.hour;
	*rdate = u.date;
	*rmonth = u.month;
	*ryear = u.year;
	*rweek = u.week;
	*rdaylight = u.daylight_p;
	*rzone = u.zone;

	return 0;
}



/*
 *  apropos-list
 */
static int list_all_packages_sort_(Execute ptr, addr *ret)
{
	addr list, key, call;
	LocalHold hold;

	Return(list_all_packages_(&list));
	hold = LocalHold_local_push(ptr, list);
	/* key */
	GetConst(COMMON_PACKAGE_NAME, &key);
	Return(getfunction_global_(key, &key));
	localhold_push(hold, key);
	/* call */
	GetConst(COMMON_STRING_LESS, &call);
	Return(getfunction_global_(call, &call));
	localhold_push(hold, call);
	/* sort */
	Return(quick_sort_sequence_(ptr, list, call, key));
	localhold_end(hold);

	return Result(ret, list);
}

static int apropos_symbol_p_(addr var, addr name, int *ret)
{
	size_t size, all, diff, x, y;
	unicode a, b;

	Check(! stringp(var), "type error");
	Check(! stringp(name), "type error");
	string_length(var, &size);
	string_length(name, &all);
	if (all < size)
		return Result(ret, 0);
	diff = (all - size) + 1;
	for (x = 0; x < diff; x++) {
		for (y = 0; y < size; y++) {
			Return(string_getc_(var, y, &a));
			Return(string_getc_(name, x + y, &b));
			if (toUpperUnicode(a) != toUpperUnicode(b))
				goto next;
		}
		return Result(ret, 1);
next:
		continue;
	}

	return Result(ret, 0);
}

static int apropos_symbol_common_(Execute ptr, addr var, addr package, addr *ret)
{
	int check;
	addr list, symbol, x, key, call;
	LocalHold hold;

	/* list */
	Return(all_symbols_package_(package, &package));
	for (list = Nil; package != Nil; ) {
		GetCons(package, &symbol, &package);
		GetNameSymbol(symbol, &x);
		Return(apropos_symbol_p_(var, x, &check));
		if (check)
			cons_heap(&list, symbol, list);
	}

	/* sort */
	hold = LocalHold_local_push(ptr, list);
	/* key */
	GetConst(COMMON_SYMBOL_NAME, &key);
	Return(getfunction_global_(key, &key));
	localhold_push(hold, key);
	/* call */
	GetConst(COMMON_STRING_LESS, &call);
	Return(getfunction_global_(call, &call));
	localhold_push(hold, call);
	/* sort */
	Return(quick_sort_sequence_(ptr, list, call, key));
	localhold_end(hold);

	return Result(ret, list);
}

int apropos_list_common_(Execute ptr, addr var, addr package, addr *ret)
{
	addr list, root, x, y;
	LocalHold hold;

	Return(string_designator_heap_(&var, var, NULL));
	if (package != Nil)
		return apropos_symbol_common_(ptr, var, package, ret);

	/* list-all-packages */
	hold = LocalHold_array(ptr, 1);
	Return(list_all_packages_sort_(ptr, &list));
	localhold_push(hold, list);
	root = Nil;
	while (list != Nil) {
		GetCons(list, &package, &list);
		Return(apropos_symbol_common_(ptr, var, package, &x));
		while (x != Nil) {
			GetCons(x, &y, &x);
			pushnew_heap(root, y, &root);
			localhold_set(hold, 0, root);
		}
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}


/*
 *  apropos
 */
int apropos_common_(Execute ptr, addr var, addr package)
{
	addr stream, list, name;

	Return(standard_output_stream_(ptr, &stream));
	Return(apropos_list_common_(ptr, var, package, &list));
	Return(fresh_line_stream_(stream, NULL));
	while (list != Nil) {
		GetCons(list, &var, &list);
		/* PACKAGE::NAME */
		GetPackageSymbol(var, &package);
		Return(getname_package_(package, &package));
		GetNameSymbol(var, &name);
		Return(print_string_stream_(stream, package));
		Return(print_ascii_stream_(stream, "::"));
		Return(print_string_stream_(stream, name));

		/* variable */
		getspecial_local(ptr, var, &name);
		if (name != Unbound) {
			Return(print_ascii_stream_(stream, ", Variable"));
		}
		/* function */
		GetFunctionSymbol(var, &name);
		if (name != Unbound) {
			Return(print_ascii_stream_(stream, ", Function"));
		}
		/* macro */
		getmacro_symbol(var, &name);
		if (name != Unbound) {
			Return(print_ascii_stream_(stream, ", Macro"));
		}
		/* terpri */
		Return(terpri_stream_(stream));
	}

	return 0;
}


/*
 *  time
 */
int time_common_(addr form, addr env, addr *ret)
{
	/* (multiple-value-bind (real1 run1 space1 count1) (lisp-system::timeinfo)
	 *   (let ((list (multiple-value-list expr)))
	 *     (multiple-value-bind (real2 run2 space2 count2) (lisp-system::timeinfo)
	 *       (fresh-line)
	 *       (format *trace-output* "Real-Time~15T~A~%" (- real2 real1))
	 *       (format *trace-output* "Run-Time~15T~A~%" (- run2 run1))
	 *       (format *trace-output* "Heap-Space~15T~A~%" (- space2 space1))
	 *       (format *trace-output* "Heap-Count~15T~A~%" (- count2 count1)))
	 *     (values-list list)))
	 */
	addr expr, mvbind, mvlist, timeinfo, let, fresh, format, trace, minus, vlist;
	addr space1, space2, count1, count2, real1, real2, run1, run2;
	addr str1, str2, str3, str4;
	addr args1, args2, list;

	Return_getcdr(form, &args1);
	if (! consp_getcons(args1, &expr, &args1))
		goto error;
	if (args1 != Nil)
		goto error;

	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(COMMON_MULTIPLE_VALUE_LIST, &mvlist);
	GetConst(SYSTEM_TIMEINFO, &timeinfo);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_FRESH_LINE, &fresh);
	GetConst(COMMON_FORMAT, &format);
	GetConst(SPECIAL_TRACE_OUTPUT, &trace);
	GetConst(COMMON_MINUS, &minus);
	GetConst(COMMON_VALUES_LIST, &vlist);
	strvect_char_heap(&str1, "Real-Time~15T~A~%");
	strvect_char_heap(&str2, "Run-Time~15T~A~%");
	strvect_char_heap(&str3, "Heap-Space~15T~A~%");
	strvect_char_heap(&str4, "Heap-Count~15T~A~%");
	make_symbolchar(&real1, "REAL1");
	make_symbolchar(&real2, "REAL2");
	make_symbolchar(&run1, "RUN1");
	make_symbolchar(&run2, "RUN2");
	make_symbolchar(&space1, "SPACE1");
	make_symbolchar(&space2, "SPACE2");
	make_symbolchar(&count1, "COUNT1");
	make_symbolchar(&count2, "COUNT2");
	make_symbolchar(&list, "LIST");
	list_heap(&args1, real1, run1, space1, count1, NULL);
	list_heap(&args2, real2, run2, space2, count2, NULL);
	list_heap(&real1, minus, real2, real1, NULL);
	list_heap(&run1, minus, run2, run1, NULL);
	list_heap(&space1, minus, space2, space1, NULL);
	list_heap(&count1, minus, count2, count1, NULL);
	list_heap(&real1, format, trace, str1, real1, NULL);
	list_heap(&run1, format, trace, str2, run1, NULL);
	list_heap(&space1, format, trace, str3, space1, NULL);
	list_heap(&count1, format, trace, str4, count1, NULL);
	list_heap(&timeinfo, timeinfo, NULL);
	list_heap(&fresh, fresh, NULL);
	list_heap(&args2, mvbind, args2, timeinfo,
			fresh, real1, run1, space1, count1, NULL);
	list_heap(&vlist, vlist, list, NULL);
	list_heap(&mvlist, mvlist, expr, NULL);
	list_heap(&list, list, mvlist, NULL);
	list_heap(&list, list, NULL);
	list_heap(&let, let, list, args2, vlist, NULL);
	list_heap(ret, mvbind, args1, timeinfo, let, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("Macro TIME ~S must be a (time form).", form, NULL);
}


/*
 *  room
 */
static int room_output_common_(Execute ptr, addr stream)
{
	size_t size, object, space, percent;
	addr pos;

	/* heap memory */
	size = get_heap_size();
	object = get_heap_object();
	pos = intsizeh(size);
	Return(format_stream_(ptr, stream, "Heap Size:~20T~A~40T[byte]~%", pos, NULL));
	pos = intsizeh(object);
	Return(format_stream_(ptr, stream, "Object memory:~20T~A~40T[byte]~%", pos, NULL));
	pos = intsizeh(get_heap_count());
	Return(format_stream_(ptr, stream, "Object count:~20T~A~40T[object]~%", pos, NULL));
	pos = intsizeh(get_heap_gc_count());
	Return(format_stream_(ptr, stream, "GC count:~20T~A~40T[times]~%", pos, NULL));

	/* free space */
	space = size - object;
	pos = intsizeh(space);
	Return(format_stream_(ptr, stream, "Free space:~20T~A~40T[byte]~%", pos, NULL));

	/* percent */
	percent = (space >> 16) * 100 / (size >> 16);
	pos = intsizeh(percent);
	Return(format_stream_(ptr, stream, "Free percent:~20T~A~40T[percent]~%", pos, NULL));

	return 0;
}

static int room_default_common_(Execute ptr, addr stream)
{
	Return(format_stream_(ptr, stream, "Room default output.~%", NULL));
	return room_output_common_(ptr, stream);
}

static int room_minimal_common_(Execute ptr, addr stream)
{
	Return(format_stream_(ptr, stream, "Room minimal output.~%", NULL));
	return room_output_common_(ptr, stream);
}

static int room_maximal_common_(Execute ptr, addr stream)
{
	Return(format_stream_(ptr, stream, "Room maximal output.~%", NULL));
	return room_output_common_(ptr, stream);
}

int room_common_(Execute ptr, addr var)
{
	addr stream, check;

	Return(standard_output_stream_(ptr, &stream));
	Return(fresh_line_stream_(stream, NULL));
	if (var == Unbound)
		return room_default_common_(ptr, stream);
	if (var == Nil)
		return room_minimal_common_(ptr, stream);
	if (var == T)
		return room_maximal_common_(ptr, stream);
	GetConst(KEYWORD_DEFAULT, &check);
	if (var == check)
		return room_default_common_(ptr, stream);

	/* error */
	return fmte_("Invalid ROOM argument ~S.", var, NULL);
}


/*
 *  ed
 */
static int ed_execute_common_(Execute ptr, addr file)
{
	addr call;

	GetConst(SYSTEM_ED_FUNCTION, &call);
	Return(getspecialcheck_local_(ptr, call, &call));
	return funcall_control_(ptr, call, file, NULL);
}

static int ed_file_common_(Execute ptr, addr var)
{
	int check;

	/* nil */
	if (var == Nil)
		return ed_execute_common_(ptr, Nil);

	/* argument */
	Return(pathname_designator_heap_(ptr, var, &var));
	Return(wild_pathname_boolean_(var, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, var,
				"~ED can't use wild card pathname ~S.", var, NULL);
	}
	Return(physical_pathname_local_(ptr, var, &var));
	Return(namestring_common_(ptr, &var, var));
	return ed_execute_common_(ptr, var);
}

static int ed_function_lambda_(addr symbol, addr *ret)
{
	addr pos;

	Return(parse_callname_error_(&pos, symbol));
	getglobal_parse_callname(pos, &pos);
	if (pos == Unbound)
		goto error;
	getdefunform_function(pos, &pos);
	if (pos == Unbound)
		goto error;
	return Result(ret, pos);

error:
	*ret = Nil;
	return fmte_("Cannot edit ~S function.", symbol, NULL);
}

static void ed_function_name(Execute ptr, addr *ret)
{
	addr file;

	GetConst(SYSTEM_ED_TEMPFILE, &file);
	getspecial_local(ptr, file, &file);
	if (file == Unbound)
		strvect_char_heap(&file, "lisp_tempfile.lisp");
	*ret = file;
}

static int ed_function_write_call_(Execute ptr, addr file, addr lambda)
{
	enum Stream_Open_External external;
	size_t width;

	Return(open_external_format_(ptr, Unbound, &external));
	if (external == Stream_Open_External_Error)
		return fmte_("Invalid external-format ~S.", NULL);
	Return(open_stream_(ptr, &file, file,
				Stream_Open_Direction_Output,
				Stream_Open_Element_Character,
				Stream_Open_IfExists_Supersede,
				Stream_Open_IfDoesNot_Create,
				external));
	Return(right_margin_print_(ptr, file, &width));
	Return(push_right_margin_print_(ptr, width));
	Return(prin1_print_(ptr, file, lambda));
	Return(close_stream_(file, NULL));

	return 0;
}

static int ed_function_write_(Execute ptr, addr file, addr lambda)
{
	addr control;

	push_control(ptr, &control);
	(void)ed_function_write_call_(ptr, file, lambda);
	return pop_control_(ptr, control);
}

static int ed_function_common_(Execute ptr, addr symbol)
{
	int result;
	addr file, lambda;

	Return(ed_function_lambda_(symbol, &lambda));
	ed_function_name(ptr, &file);
	Return(ed_function_write_(ptr, file, lambda));
	Return(ed_file_common_(ptr, file));
	Return(eval_load_(ptr, &result, file, Unbound, Unbound, 1, Unbound));

	return 0;
}

int ed_common_(Execute ptr, addr var)
{
	if (var == Unbound)
		var = Nil;
	if (var == Nil)
		return ed_file_common_(ptr, Nil);
	else if (function_name_p(var))
		return ed_function_common_(ptr, var);
	else
		return ed_file_common_(ptr, var);
}


/*
 *  dribble
 */
static int dribble_message_begin_(Execute ptr, addr file)
{
	const char *str;
	addr name, stream;

	Return(pathname_designator_heap_(ptr, file, &name));
	Return(standard_output_stream_(ptr, &stream));
	str = "~&;; DRIBBLE begin to write ~S.~%";
	return format_stream_(ptr, stream, str, name, NULL);
}

static int dribble_message_end_(Execute ptr, addr file)
{
	const char *str;
	addr name, stream;

	Return(pathname_designator_heap_(ptr, file, &name));
	Return(standard_output_stream_(ptr, &stream));
	str = "~&;; DRIBBLE end to write ~S.~%";
	return format_stream_(ptr, stream, str, name, NULL);
}

static int dribble_set_stream_(addr file)
{
	addr dfile, dinput, doutput, decho, dbroadcast, sinput, soutput;
	addr input, output, echo, broadcast;

	/*  *dribble*          -> file-stream
	 *  *standard-input*   -> echo-stream
	 *  *standard-output*  -> broadcast-stream
	 *  echo-stream
	 *    echo-input       -> STDIN
	 *    echo-output      -> file-stream
	 *  broadcast-stream
	 *    output1          -> STDOUT
	 *    output2          -> file-stream
	 */

	/* symbol */
	GetConst(SYSTEM_DRIBBLE_FILE, &dfile);
	GetConst(SYSTEM_DRIBBLE_INPUT, &dinput);
	GetConst(SYSTEM_DRIBBLE_OUTPUT, &doutput);
	GetConst(SYSTEM_DRIBBLE_ECHO, &decho);
	GetConst(SYSTEM_DRIBBLE_BROADCAST, &dbroadcast);
	GetConst(SPECIAL_STANDARD_INPUT, &sinput);
	GetConst(SPECIAL_STANDARD_OUTPUT, &soutput);

	/* stream */
	GetValueSymbol(sinput, &input);
	GetValueSymbol(soutput, &output);
	Return(exitpoint_stream_(output));
	Return(force_output_stream_(output));
	open_echo_stream(&echo, input, file);
	list_heap(&broadcast, output, file, NULL);
	Return(open_broadcast_stream_(&broadcast, broadcast));

	/* variable */
	Return(setvalue_symbol_(dfile, file));
	Return(setvalue_symbol_(dinput, input));
	Return(setvalue_symbol_(doutput, output));
	Return(setvalue_symbol_(decho, echo));
	Return(setvalue_symbol_(dbroadcast, broadcast));
	Return(setvalue_symbol_(sinput, echo));
	Return(setvalue_symbol_(soutput, broadcast));

	return 0;
}

static int dribble_open_file_(Execute ptr, addr file)
{
	enum Stream_Open_External external;
	int check;

	Return(physical_pathname_heap_(ptr, file, &file));
	Return(wild_pathname_boolean_(file, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, file,
				"~DRIBBLE can't use wild card pathname ~S.", file, NULL);
	}
	Return(open_external_format_(ptr, Unbound, &external));
	if (external == Stream_Open_External_Error)
		return fmte_("Invalid external-format.", NULL);
	Return(open_stream_(ptr, &file, file,
				Stream_Open_Direction_Output,
				Stream_Open_Element_Character,
				Stream_Open_IfExists_Supersede,
				Stream_Open_IfDoesNot_Create,
				external));
	Return(dribble_set_stream_(file));
	return dribble_message_begin_(ptr, file);
}

static int dribble_open_(Execute ptr, addr file)
{
	addr check;

	GetConst(SYSTEM_DRIBBLE_FILE, &check);
	GetValueSymbol(check, &check);
	if (check != Unbound) {
		Return(pathname_designator_heap_(ptr, check, &check));
		return fmtw_("DRIBBLE already open file ~S.", check, NULL);
	}
	else {
		return dribble_open_file_(ptr, file);
	}
}

static int dribble_close_stream_(Execute ptr)
{
	addr dfile, dinput, doutput, decho, dbroadcast, sinput, soutput;
	addr file, input, output, echo, broadcast;

	/* symbol */
	GetConst(SYSTEM_DRIBBLE_FILE, &dfile);
	GetConst(SYSTEM_DRIBBLE_INPUT, &dinput);
	GetConst(SYSTEM_DRIBBLE_OUTPUT, &doutput);
	GetConst(SYSTEM_DRIBBLE_ECHO, &decho);
	GetConst(SYSTEM_DRIBBLE_BROADCAST, &dbroadcast);
	GetConst(SPECIAL_STANDARD_INPUT, &sinput);
	GetConst(SPECIAL_STANDARD_OUTPUT, &soutput);

	/* variable */
	GetValueSymbol(dfile, &file);
	GetValueSymbol(dinput, &input);
	GetValueSymbol(doutput, &output);
	GetValueSymbol(decho, &echo);
	GetValueSymbol(dbroadcast, &broadcast);

	/* close */
	Return(dribble_message_end_(ptr, file));
	Return(setvalue_symbol_(dfile, Unbound));
	Return(setvalue_symbol_(dinput, Unbound));
	Return(setvalue_symbol_(doutput, Unbound));
	Return(setvalue_symbol_(decho, Unbound));
	Return(setvalue_symbol_(dbroadcast, Unbound));
	Return(setvalue_symbol_(sinput, input));
	Return(setvalue_symbol_(soutput, output));
	Return(close_stream_(echo, NULL));
	Return(close_stream_(broadcast, NULL));
	Return(close_stream_(file, NULL));

	return 0;
}

static int dribble_close_(Execute ptr)
{
	addr symbol, file;

	GetConst(SYSTEM_DRIBBLE_FILE, &symbol);
	GetValueSymbol(symbol, &file);
	if (file != Unbound) {
		Return(dribble_close_stream_(ptr));
	}

	return 0;
}

int dribble_common_(Execute ptr, addr file)
{
	if (file != Nil)
		return dribble_open_(ptr, file);
	else
		return dribble_close_(ptr);
}


/************************************************************
 *  call_eval.c
 ************************************************************/

/*
 *  lambda
 */
void lambda_common(addr form, addr *ret)
{
	addr symbol;
	GetConst(COMMON_FUNCTION, &symbol);
	list_heap(ret, symbol, form, NULL);
}


/*
 *  compile
 */
static int compile_warning_implementation_(void)
{
	return 0;
	/* return fmtw_("This implementation cannot compile a function.", NULL); */
}

static int compile_variable_(Execute ptr, addr var, addr opt, addr *ret)
{
	addr call, check;

	Return(parse_callname_error_(&call, var));
	getglobal_callname(call, &check);
	if (check == Unbound) {
		if (! symbolp_callname(call))
			goto unbound;
		getmacro_symbol(var, &check);
		if (check == Unbound)
			goto unbound;
	}
	Return(compile_warning_implementation_());
	return Result(ret, var);

unbound:
	*ret = Nil;
	return fmte_("The function ~S is unbound.", var, NULL);
}

static int compile_lambda_p(addr opt)
{
	addr check;

	if (! consp(opt))
		return 0;
	GetCar(opt, &opt);
	GetConst(COMMON_LAMBDA, &check);
	return check == opt;
}

static int compile_lambda_(Execute ptr, addr opt, addr *ret)
{
	if (functionp(opt)) {
		Return(compile_warning_implementation_());
		return Result(ret, opt);
	}
	if (compile_lambda_p(opt)) {
		Return(compile_warning_implementation_());
		Return(eval_result_compile_(ptr, opt, &opt));
		return Result(ret, opt);
	}

	*ret = Nil;
	return fmte_("The second argument ~S must be a lambda expression.", opt, NULL);
}

static int compile_symbol_(Execute ptr, addr var, addr opt, addr *ret)
{
	addr call;
	LocalHold hold;

	Return(parse_callname_error_(&call, var));
	if (functionp(opt)) {
		Return(compile_warning_implementation_());
		Return(setglobal_callname_(call, opt));
		return Result(ret, var);
	}
	if (compile_lambda_p(opt)) {
		Return(compile_warning_implementation_());
		hold = LocalHold_local(ptr);
		localhold_pushva_force(hold, call, opt, NULL);
		Return(eval_result_compile_(ptr, opt, &opt));
		localhold_end(hold);
		Return(setglobal_callname_(call, opt));
		return Result(ret, var);
	}

	*ret = Nil;
	return fmte_("The second argument ~S must be a lambda expression.", opt, NULL);
}

static int compile_common_execute_(Execute ptr, addr var, addr opt, addr *ret)
{
	if (opt == Unbound)
		return compile_variable_(ptr, var, opt, ret);
	if (var == Nil)
		return compile_lambda_(ptr, opt, ret);
	if (function_name_p(var))
		return compile_symbol_(ptr, var, opt, ret);

	/* error */
	*ret = Nil;
	return fmte_("The first argument ~S "
			"in COMPILE must be a function-name.", var, NULL);
}

static int compile_common_call_(Execute ptr, LocalHold hold,
		addr var, addr opt, addr *ret1, addr *ret2, addr *ret3)
{
	Return(handler_compile_(ptr));
	Return(compile_common_execute_(ptr, var, opt, ret1));
	localhold_set(hold, 0, *ret1);
	/* warning */
	GetConst(SYSTEM_COMPILE_WARNING, &var);
	Return(getspecialcheck_local_(ptr, var, ret2));
	/* style-warning */
	GetConst(SYSTEM_COMPILE_STYLE_WARNING, &var);
	Return(getspecialcheck_local_(ptr, var, ret3));

	return 0;
}

int compile_common_(Execute ptr, addr var, addr opt,
		addr *ret1, addr *ret2, addr *ret3)
{
	addr control, check;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)compile_common_call_(ptr, hold, var, opt, ret1, ret2, &check);
	*ret3 = ((*ret2 != Nil) && (check == Nil))? T: Nil;
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}


/*
 *  eval
 */
int eval_common_(Execute ptr, addr var)
{
	return eval_execute_partial_(ptr, var);
}


/*
 *  compiler-macro-function
 */
static int compiler_macro_function_symbol_(addr var, addr env, addr *ret)
{
	if (env != Unbound) {
		Return(find_environment_(var, env, &env));
		if (env == Unbound) {
			/* compiler-macro-function is shadowed */
			return Result(ret, Nil);
		}
	}
	GetCallName(var, &var);
	get_compiler_macro_symbol(var, ret);
	return 0;
}

static int compiler_macro_function_setf_(addr var, addr env, addr *ret)
{
	if (env != Unbound) {
		*ret = Nil;
		return fmte_("Don't use environment argument ~S "
				"in COMPILER-MACRO-FUNCTION setf-form.", env, NULL);
	}
	GetCallName(var, &var);
	get_setf_compiler_macro_symbol(var, ret);
	return 0;
}

int compiler_macro_function_common_(addr var, addr env, addr *ret)
{
	Return(parse_callname_error_(&var, var));
	if (symbolp_callname(var))
		return compiler_macro_function_symbol_(var, env, ret);
	else
		return compiler_macro_function_setf_(var, env, ret);
}


/*
 *  (setf compiler-macro-function)
 */
static int setf_compiler_macro_function_symbol_(addr var, addr env, addr value)
{
	if (env != Unbound) {
		Return(find_environment_(var, env, &env));
		if (env == Unbound) {
			/* compiler-macro-function is shadowed */
			return fmte_("COMPILER-MACRO-FUNCTION ~S "
					"is shadowed in the environment.", var, NULL);
		}
	}
	GetCallName(var, &var);
	if (value == Nil)
		return rem_compiler_macro_symbol_(var);
	else
		return set_compiler_macro_symbol_(var, value);
}

static int setf_compiler_macro_function_setf_(addr var, addr env, addr value)
{
	if (env != Unbound) {
		return fmte_("Don't use environment argument ~S "
				"in COMPILER-MACRO-FUNCTION setf-form.", env, NULL);
	}
	GetCallName(var, &var);
	if (value == Nil)
		return rem_setf_compiler_macro_symbol_(var);
	else
		return set_setf_compiler_macro_symbol_(var, value);
}

int setf_compiler_macro_function_common_(addr value, addr var, addr env)
{
	if (! callnamep(var)) {
		Return(parse_callname_error_(&var, var));
	}
	if (symbolp_callname(var))
		return setf_compiler_macro_function_symbol_(var, env, value);
	else
		return setf_compiler_macro_function_setf_(var, env, value);
}


/*
 *  define-compiler-macro
 */
int define_compiler_macro_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr right, eval, name, args, decl, doc;

	/* (define-compiler-macro . form) */
	Return_getcdr(form, &right);
	if (right == Nil) {
		return fmte_("define-compiler-macro form "
				"must have at least a name and body.", NULL);
	}

	/* name */
	if (! consp_getcons(right, &name, &right))
		return fmte_("Invalid define-compiler-macro form.", NULL);
	Return(parse_callname_error_(&name, name));
	if (right == Nil) {
		return fmte_("define-compiler-macro form "
				"must have at least a name and body.", NULL);
	}

	/* args */
	if (! consp_getcons(right, &args, &right))
		return fmte_("Invalid define-compiler-macro form.", NULL);
	if (! IsList(right))
		return fmte_("Invalid define-compiler-macro form.", NULL);

	/* parse */
	Return(lambda_macro_(ptr->local, &args, args, Nil));
	Return(declare_body_documentation_(ptr, env, right, &doc, &decl, &right));

	/* (eval::define-compiler-macro name args decl doc body) */
	GetConst(SYSTEM_DEFINE_COMPILER_MACRO, &eval);
	list_heap(ret, eval, name, args, decl, doc, right, NULL);

	return 0;
}

int set_define_compiler_macro_(addr callname, addr value)
{
	return setf_compiler_macro_function_common_(value, callname, Unbound);
}


/*
 *  defmacro
 */
static void defmacro_common_block(addr name, addr form, addr *ret)
{
	/* `(block ,name ,@form) */
	addr block;

	GetConst(COMMON_BLOCK, &block);
	lista_heap(&block, block, name, form, NULL);
	list_heap(ret, block, NULL);
}

int defmacro_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr eval, name, args, decl, doc;

	/* (defmacro . form) */
	Return_getcdr(form, &form);
	if (form == Nil)
		return fmte_("defmacro form must have at least a name and body.", NULL);

	/* name */
	if (! consp_getcons(form, &name, &form))
		return fmte_("Invalid defmacro form.", NULL);
	if (! symbolp(name))
		return fmte_("defmacro name ~S must be a symbol.", name, NULL);
	if (form == Nil)
		return fmte_("defmacro form must have at least a name and body.", NULL);

	/* args */
	if (! consp_getcons(form, &args, &form))
		return fmte_("Invalid defmacro form.", NULL);
	if (! IsList(form))
		return fmte_("Invalid defmacro form.", NULL);

	/* parse */
	Return(check_function_variable_(name));
	Return(lambda_macro_(ptr->local, &args, args, Nil));
	Return(declare_body_documentation_(ptr, env, form, &doc, &decl, &form));
	defmacro_common_block(name, form, &form);

	/* (eval::defmacro name args decl doc body) */
	GetConst(SYSTEM_DEFMACRO, &eval);
	list_heap(ret, eval, name, args, decl, doc, form, NULL);

	return 0;
}


/*
 *  macro-function
 */
int macro_function_common_(addr symbol, addr env, addr *ret)
{
	if (env == Unbound)
		env = Nil;
	Return(find_environment_(symbol, env, &env));
	return Result(ret, env != Unbound? env: Nil);
}


/*
 *  macroexpand
 */
int macroexpand_common_(Execute ptr, addr form, addr env, addr *ret, addr *sec)
{
	int check;

	if (env == Unbound)
		env = Nil;
	Return(macroexpand_(ptr, ret, form, env, &check));
	*sec = check? T: Nil;

	return 0;
}


/*
 *  macroexpand_1
 */
int macroexpand_1_common_(Execute ptr, addr form, addr env, addr *ret, addr *sec)
{
	int check;

	if (env == Unbound)
		env = Nil;
	Return(macroexpand1_(ptr, ret, form, env, &check));
	*sec = check? T: Nil;

	return 0;
}


/*
 *  define-symbol-macro
 */
static int define_symbol_macro_check_(addr symbol)
{
	addr value;

	/* symbol, constant */
	Return(check_variable_(symbol));

	/* special */
	if (specialp_symbol(symbol)) {
		return call_simple_program_error_va_(NULL,
				"define-symbol-macro cannot bind the special symbol ~S.",
				symbol, NULL);
	}

	/* symbol-value */
	GetValueSymbol(symbol, &value);
	if (value != Unbound) {
		return call_simple_program_error_va_(NULL,
				"define-symbol-macro cannot bind the bounded symbol ~S.",
				symbol, NULL);
	}

	return 0;
}

int define_symbol_macro_common_(addr form, addr env, addr *ret)
{
	addr cons, symbol, expansion, quote;

	Return_getcdr(form, &cons);
	if (! consp_getcons(cons, &symbol, &cons))
		goto error;
	if (! consp_getcons(cons, &expansion, &cons))
		goto error;
	if (cons != Nil)
		goto error;
	Return(define_symbol_macro_check_(symbol));

	/* (lisp-system::define-symbol-macro (quote symbol) (quote expansion)) */
	GetConst(SYSTEM_DEFINE_SYMBOL_MACRO, &form);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&symbol, quote, symbol, NULL);
	list_heap(&expansion, quote, expansion, NULL);
	list_heap(ret, form, symbol, expansion, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("define-symbol-macro argument ~S "
			"must be a (symbol expansion) form.", form, NULL);
}


/*
 *  declaim
 */
int declaim_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr symbol;

	GetConst(SYSTEM_DECLAIM, &symbol);
	Return_getcdr(form, &form); /* (declaim . form) */
	Return(parse_declaim_heap_(ptr, Nil, form, &form));
	conscar_heap(&form, form);
	cons_heap(ret, symbol, form);

	return 0;
}


/*
 *  constantp
 */
static int eval_constantp_stable(addr var)
{
	addr check;

	switch (GetType(var)) {
		case LISPTYPE_CONS:
			GetCar(var, &var);
			GetConst(COMMON_QUOTE, &check);
			return check == var;

		case LISPTYPE_SYMBOL:
			if (keywordp(var))
				return 1;
			return GetStatusReadOnly(var);

		default:
			return 1;
	}
}

static int eval_constantp_(Execute ptr, addr var, addr env, int *ret)
{
	int check;
	addr pos;

	Return(macroexpand_(ptr, &pos, var, env, &check));
	if (check)
		var = pos;
	*ret = eval_constantp_stable(var);

	return 0;
}

int constantp_common_(Execute ptr, addr var, addr opt, addr *ret)
{
	int check;

	if (opt == Unbound)
		opt = Nil;
	Return(eval_constantp_(ptr, var, opt, &check));
	*ret = check? T: Nil;

	return 0;
}


/************************************************************
 *  call_filenames.c
 ************************************************************/

/* pathname */
int pathname_common_(Execute ptr, addr var, addr *ret)
{
	return pathname_designator_heap_(ptr, var, ret);
}


/* make-pathname */
static int pathname_case_local_p_(addr rest, int *ret)
{
	addr check;

	if (GetKeyArgs(rest, KEYWORD_CASE, &rest))
		return Result(ret, 1); /* default :local */
	GetConst(KEYWORD_LOCAL, &check);
	if (check == rest)
		return Result(ret, 1);
	GetConst(KEYWORD_COMMON, &check);
	if (check == rest)
		return Result(ret, 0);

	*ret = 0;
	return fmte_("Invalid :case value ~S.", rest, NULL);
}

int make_pathname_common_(Execute ptr, addr rest, addr *ret)
{
	int localp;
	addr host, device, directory, name, type, version, defaults;

	if (GetKeyArgs(rest, KEYWORD_HOST, &host))
		host = Unbound;
	if (GetKeyArgs(rest, KEYWORD_DEVICE, &device))
		device = Unbound;
	if (GetKeyArgs(rest, KEYWORD_DIRECTORY, &directory))
		directory = Unbound;
	if (GetKeyArgs(rest, KEYWORD_NAME, &name))
		name = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TYPE, &type))
		type = Unbound;
	if (GetKeyArgs(rest, KEYWORD_VERSION, &version))
		version = Unbound;
	if (GetKeyArgs(rest, KEYWORD_DEFAULTS, &defaults))
		defaults = Unbound;
	Return(pathname_case_local_p_(rest, &localp));

	/* *default-pathname-defaults* */
	Return(defaults_pathname_heap_(ptr, &defaults, defaults));

	/* default arguments */
	if (host == Unbound)
		GetHostPathname(defaults, &host);
	if (device == Unbound)
		GetDevicePathname(defaults, &device);
	if (directory == Unbound)
		GetDirectoryPathname(defaults, &directory);
	if (name == Unbound)
		GetNamePathname(defaults, &name);
	if (type == Unbound)
		GetTypePathname(defaults, &type);
	if (version == Unbound)
		GetVersionPathname(defaults, &version);

	/* make-pathname */
	return make_pathname_heap_(ret,
			host, device, directory, name, type, version,
			localp);
}


/* pathnamep */
void pathnamep_common(addr var, addr *ret)
{
	*ret = pathnamep(var)? T: Nil;
}


/* pathname-host */
int pathname_host_common_(Execute ptr, addr pos, addr rest, addr *ret)
{
	int localp;

	Return(pathname_designator_heap_(ptr, pos, &pos));
	Return(pathname_case_local_p_(rest, &localp));
	Return(pathname_host_(pos, &pos, localp));

	return Result(ret, pos);
}


/* pathname-device */
int pathname_device_common_(Execute ptr, addr pos, addr rest, addr *ret)
{
	int localp;

	Return(pathname_designator_heap_(ptr, pos, &pos));
	Return(pathname_case_local_p_(rest, &localp));
	Return(pathname_device_(pos, &pos, localp));

	return Result(ret, pos);
}


/* pathname-directory */
int pathname_directory_common_(Execute ptr, addr pos, addr rest, addr *ret)
{
	int localp;

	Return(pathname_designator_heap_(ptr, pos, &pos));
	Return(pathname_case_local_p_(rest, &localp));
	Return(pathname_directory_(pos, &pos, localp));

	return Result(ret, pos);
}


/* pathname-name */
int pathname_name_common_(Execute ptr, addr pos, addr rest, addr *ret)
{
	int localp;

	Return(pathname_designator_heap_(ptr, pos, &pos));
	Return(pathname_case_local_p_(rest, &localp));
	Return(pathname_name_(pos, &pos, localp));

	return Result(ret, pos);
}


/* pathname-type */
int pathname_type_common_(Execute ptr, addr pos, addr rest, addr *ret)
{
	int localp;

	Return(pathname_designator_heap_(ptr, pos, &pos));
	Return(pathname_case_local_p_(rest, &localp));
	Return(pathname_type_(pos, &pos, localp));

	return Result(ret, pos);
}


/* pathname-version */
int pathname_version_common_(Execute ptr, addr pos, addr *ret)
{
	Return(pathname_designator_heap_(ptr, pos, &pos));
	pathname_version(pos, &pos);

	return Result(ret, pos);
}


/* load-logical-pathname-translations */
static int load_logical_pathname_read_(Execute ptr,
		LocalHold hold, addr stream, addr *ret)
{
	addr list, pos;
	int result;

	list = Nil;
	for (;;) {
		Return(read_stream_(ptr, stream, &result, &pos));
		if (result)
			break;
		cons_heap(&list, pos, list);
		localhold_set(hold, 1, list);
	}
	nreverse(ret, list);
	localhold_set(hold, 1, list);

	return 0;
}

static int load_logical_pathname_file_(Execute ptr, addr file, addr *ret)
{
	addr control, save;
	LocalHold hold;

	push_control(ptr, &control);
	hold = LocalHold_array(ptr, 2);
	/* open file */
	if (open_input_stream_error_(ptr, &file, file, Unbound))
		goto escape;
	localhold_set(hold, 0, file);

	/* load file */
	(void)load_logical_pathname_read_(ptr, hold, file, ret);

	/* close */
	save_execute_control(ptr, &save);
	normal_throw_control(ptr);
	if (close_stream_(file, NULL))
		goto escape;
	restore_execute_control(ptr, save);

escape:
	return pop_control_(ptr, control);
}

int load_logical_pathname_translations_common_(Execute ptr, addr host, addr *ret)
{
	addr defaults, file, pos;

	/* pathname */
	GetConst(SYSTEM_LOAD_LOGICAL_PATHNAME_TRANSLATIONS, &defaults);
	getspecial_local(ptr, defaults, &defaults);
	Return(defaults_pathname_heap_(ptr, &defaults, defaults));
	pathname_heap(&file, Nil, Nil, Nil, host, Nil);
	Return(merge_pathnames_clang_(ptr, host, defaults, Unbound, &file));
	/* load */
	Return(probe_file_files_(ptr, &pos, file));
	/* host file is not exist */
	if (pos == Nil)
		return Result(ret, Nil);
	/* read file */
	Return(load_logical_pathname_file_(ptr, file, &pos));
	Return(setf_logical_pathname_translations_common_(ptr, host, pos));
	return Result(ret, T);
}


/* logical-pathname-translations */
int logical_pathname_translations_common_(addr host, addr *ret)
{
	return gethost_logical_pathname_(host, ret);
}


/* (setf logical-pathname-translations) */
static int setf_logical_pathname_translations_list_(Execute ptr,
		addr *ret, addr host, addr list)
{
	int check;
	addr show, value, root, left, right, temp;

	/* parse-list */
	show = list;
	root = Nil;
	while (list != Nil) {
		Return_getcons(list, &right, &list);
		Return_getcons(right, &left, &right);
		Return_getcons(right, &right, &value);
		if (value != Nil) {
			return fmte_("Invalid logical-pathname-translations "
					"arguments ~S.", show, NULL);
		}

		/* left */
		if (stringp(left)) {
			Return(parse_pathname_setf_heap_(ptr, left, host, &value));
		}
		Return(pathname_designator_heap_(ptr, value, &value));
		if (! pathname_logical_p(value)) {
			return fmte_("The left argument ~S "
					"must be a logical-pathname.", left, NULL);
		}
		GetHostPathname(value, &temp);
		Return(string_equalp_(host, temp, &check));
		if (! check) {
			return fmte_("The logical-pathname :HOST ~S "
					"must be ~S.", temp, host, NULL);
		}
		left = value;

		/* right */
		Return(pathname_designator_heap_(ptr, right, &value));
		if (! pathnamep(value)) {
			return fmte_("The right argument ~S "
					"must be a no-logical-pathname.", right, NULL);
		}
		right = value;

		/* result */
		list_heap(&right, left, right, NULL);
		cons_heap(&root, right, root);
	}
	nreverse(ret, root);

	return 0;
}

static int setf_logical_pathname_translations_code_(Execute ptr,
		addr host, addr list, addr cons)
{
	Return(setf_logical_pathname_translations_list_(ptr, &list, host, list));
	SetCdr(cons, list);
	return 0;
}

static int setf_logical_pathname_translations_call_(Execute ptr,
		addr host, addr list, addr table)
{
	addr cons;

	Return(intern_hashheap_(table, host, &cons));
	return setf_logical_pathname_translations_code_(ptr, host, list, cons);
}

static int setf_logical_pathname_translations_delete_(Execute ptr, addr host)
{
	int check;
	addr table;

	table_logical_pathname(&table);
	return delete_hashtable_(table, host, &check);
}

static int setf_logical_pathname_translations_intern_(Execute ptr,
		addr host, addr list, addr table)
{
	int check;
	addr control, save;

	push_control(ptr, &control);
	check = setf_logical_pathname_translations_call_(ptr, host, list, table);
	if (check == 0)
		goto escape;

	/* delete hashtable */
	save_execute_control(ptr, &save);
	normal_throw_control(ptr);
	if (setf_logical_pathname_translations_delete_(ptr, host))
		goto escape;
	restore_execute_control(ptr, save);

escape:
	return pop_control_(ptr, control);
}

int setf_logical_pathname_translations_common_(Execute ptr, addr host, addr list)
{
	addr table, cons;

	table_logical_pathname(&table);
	Return(find_hashtable_(table, host, &cons));
	if (cons != Unbound)
		return setf_logical_pathname_translations_code_(ptr, host, list, cons);
	else
		return setf_logical_pathname_translations_intern_(ptr, host, list, table);
}


/* logical-pathname */
int logical_pathname_common_(Execute ptr, addr *ret, addr pos)
{
	addr value, type;

	Return(pathname_designator_heap_(ptr, pos, &value));
	if (! pathname_logical_p(value)) {
		GetConst(COMMON_LOGICAL_PATHNAME, &type);
		return call_type_error_va_(ptr, value, type,
				"The pathname ~S is not logical-pathname.", pos, NULL);
	}

	return Result(ret, value);
}


/* *default-pathname-defaults* */
#ifdef LISP_WINDOWS
#define DEFAULT_PATHNAME_MODE CONSTANT_SYSTEM_WINDOWS
#else
#define DEFAULT_PATHNAME_MODE CONSTANT_SYSTEM_UNIX
#endif

void default_pathname_defaults_common(void)
{
	static const constindex index = DEFAULT_PATHNAME_MODE;
	addr symbol, value, type;

	/* value */
	GetConstant(index, &value);
	pathname_heap(&value, value, Nil, Nil, Nil, Nil);

	/* symbol */
	GetConst(SPECIAL_DEFAULT_PATHNAME_DEFAULTS, &symbol);
	SetValueSymbol(symbol, value);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, Pathname);
	settype_value_symbol(symbol, type);
}


/* namestring */
int namestring_common_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	if (stringp(pos)) {
		local = ptr->local;
		push_local(local, &stack);
		Return(pathname_designator_local_(ptr, pos, &pos));
		Return(name_pathname_heap_(ptr, pos, ret));
		rollback_local(local, stack);
	}
	else {
		Return(pathname_designator_heap_(ptr, pos, &pos));
		Return(name_pathname_heap_(ptr, pos, ret));
	}

	return 0;
}


/* file-namestring */
int file_namestring_common_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	if (stringp(pos)) {
		push_local(local, &stack);
		Return(pathname_designator_local_(ptr, pos, &pos));
		Return(file_name_pathname_heap_(local, pos, ret));
		rollback_local(local, stack);
	}
	else {
		Return(pathname_designator_heap_(ptr, pos, &pos));
		Return(file_name_pathname_heap_(local, pos, ret));
	}

	return 0;
}


/* directory-namestring */
int directory_namestring_common_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	if (stringp(pos)) {
		push_local(local, &stack);
		Return(pathname_designator_local_(ptr, pos, &pos));
		Return(directory_name_pathname_heap_(local, pos, ret));
		rollback_local(local, stack);
	}
	else {
		Return(pathname_designator_heap_(ptr, pos, &pos));
		Return(directory_name_pathname_heap_(local, pos, ret));
	}

	return 0;
}


/* host-namestring */
int host_namestring_common_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	if (stringp(pos)) {
		local = ptr->local;
		push_local(local, &stack);
		Return(pathname_designator_local_(ptr, pos, &pos));
		GetHostPathname(pos, &pos);
		if (! stringp(pos))
			strvect_heap(ret, 0);
		else
			copylocal_object(NULL, ret, pos);
		rollback_local(local, stack);
	}
	else {
		Return(pathname_designator_heap_(ptr, pos, &pos));
		GetHostPathname(pos, &pos);
		if (! stringp(pos))
			strvect_heap(ret, 0);
		else
			*ret = pos;
	}

	return 0;
}


/* enough-namestring */
static void enough_namestring_copy(addr pos, addr cdr, int relp, addr *ret)
{
	addr one, value;

	make_pathname_alloc(NULL, &one, RefLogicalPathname(pos));
	copylocal_pathname_array(NULL, pos, PATHNAME_INDEX_HOST, one);
	copylocal_pathname_array(NULL, pos, PATHNAME_INDEX_DEVICE, one);
	copylocal_pathname_array(NULL, pos, PATHNAME_INDEX_NAME, one);
	copylocal_pathname_array(NULL, pos, PATHNAME_INDEX_TYPE, one);
	copylocal_pathname_array(NULL, pos, PATHNAME_INDEX_VERSION, one);
	/* directory */
	if (relp) {
		GetDirectoryPathname(pos, &pos);
		GetConst(KEYWORD_RELATIVE, &value);
		copylocal_object(NULL, &cdr, cdr);
		cons_heap(&value, value, cdr);
		SetDirectoryPathname(one, value);
	}
	*ret = one;
}

static int enough_namestring_directory_(LocalRoot local,
		addr pos, addr a, addr b,
		lisp_equal_calltype equal,
		addr *value, int *ret)
{
	int check;
	addr car1, car2, cdr1, cdr2, next;

	/* nil, nil */
	if (a == Nil && b == Nil)
		return Result(ret, 0);

	/* a == Nil */
	if (a == Nil) {
		strvect_heap(value, 0);
		return Result(ret, 1);
	}

	/* b == Nil */
	if (a == Nil) {
		enough_namestring_copy(pos, b, 0, value);
		return Result(ret, 1);
	}

	/* relative, absolute */
	Return_getcons(a, &car1, &cdr1);
	Return_getcons(b, &car2, &cdr2);
	if (car1 != car2)
		return Result(ret, 0);

	/* directory */
	while (cdr2 != Nil) {
		if (cdr1 == Nil)
			return Result(ret, 0);
		Return_getcons(cdr1, &car1, &next);
		Return_getcons(cdr2, &car2, &cdr2);
		Return((*equal)(car1, car2, &check));
		if (! check)
			return Result(ret, 0);
		cdr1 = next;
	}

	/* result */
	enough_namestring_copy(pos, cdr1, 1, value);
	return Result(ret, 1);
}

static int enough_namestring_merge_(LocalRoot local,
		addr a, addr b, addr *value, int *ret)
{
	int check, check1, check2;
	addr pos1, pos2;
	lisp_equal_calltype equal;

	check1 = RefLogicalPathname(a);
	check2 = RefLogicalPathname(b);
	if (check1 != check2)
		return Result(ret, 0);

	/* host */
	GetHostPathname(a, &pos1);
	GetHostPathname(b, &pos2);
	Return(equalp_function_(pos1, pos2, &check));
	if (! check)
		return Result(ret, 0);
	equal = pathname_equal_function(a);

	/* device */
	GetDevicePathname(a, &pos1);
	GetDevicePathname(b, &pos2);
	Return((*equal)(pos1, pos2, &check));
	if (! check)
		return Result(ret, 0);

	/* directory */
	GetDirectoryPathname(a, &pos1);
	GetDirectoryPathname(b, &pos2);
	return enough_namestring_directory_(local, a, pos1, pos2, equal, value, ret);
}

int enough_namestring_common_(Execute ptr, addr *ret, addr pos, addr defaults)
{
	int check;
	addr value;

	Return(defaults_pathname_heap_(ptr, &defaults, defaults));
	Return(pathname_designator_heap_(ptr, pos, &pos));
	Return(enough_namestring_merge_(ptr->local, pos, defaults, &value, &check));
	if (check)
		pos = value;
	return name_pathname_heap_(ptr, pos, ret);
}


/* parse-namestring */
static int parse_namestring_call_(Execute ptr, addr *ret, addr *position,
		addr thing, addr host, addr defaults, addr start, addr end, addr junk)
{
	int check;
	addr value, type;
	size_t index1, index2;

	/* defaults */
	Return(defaults_pathname_heap_(ptr, &defaults, defaults));

	/* stream */
	if (streamp(thing))
		GetPathnameStream(thing, &thing);

	/* pathname */
	if (pathnamep(thing)) {
		GetHostPathname(thing, &value);
		if (host != Nil) {
			Return(equalp_function_(host, value, &check));
			if (! check) {
				GetConst(COMMON_PATHNAME, &type);
				return call_type_error_va_(ptr, thing, type,
						":HOST ~S does not match a pathname host ~S.",
						host, value, NULL);
			}
		}
		*position = start;
		return Result(ret, thing);
	}

	/* string */
	if (stringp(thing)) {
		string_length(thing, &index2);
		Return(keyword_start_end_value_(index2, start, end, &index1, &index2));
		Return(parse_pathname_full_heap_(ptr, thing, host, defaults,
					index1, index2, junk != Nil, &thing, &index1));
		make_index_integer_heap(position, index1);
		/* junk-allowed */
		if (thing == Nil)
			return Result(ret, Nil);
		/* host check */
		if (host != Nil) {
			GetHostPathname(thing, &value);
			Return(equalp_function_(host, value, &check));
			if (! check)
				return fmte_(":HOST ~S is not argument host ~S.", value, host, NULL);
		}
		/* result */
		return Result(ret, thing);
	}

	GetConst(COMMON_PATHNAME, &type);
	return call_type_error_va_(ptr, thing, type,
			"~S is not pathname-designator.", thing, NULL);
}

int parse_namestring_common_(Execute ptr,
		addr thing, addr rest, addr *ret1, addr *ret2)
{
	addr host, defaults, start, end, junk;

	if (rest == Nil) {
		host = defaults = Nil;
		goto keyargs;
	}
	Return_getcons(rest, &host, &rest);
	if (rest == Nil) {
		defaults = Nil;
		goto keyargs;
	}
	Return_getcons(rest, &defaults, &rest);
keyargs:
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Nil;
	if (GetKeyArgs(rest, KEYWORD_JUNK_ALLOWED, &junk))
		junk = Nil;

	return parse_namestring_call_(ptr,
			ret1, ret2, thing, host, defaults, start, end, junk);
}


/* wild-pathname-p */
int wild_pathname_p_common_(Execute ptr, addr *ret, addr file, addr field)
{
	int check;

	Return(pathname_designator_heap_(ptr, file, &file));
	if (field == Unbound)
		field = Nil;
	Return(wild_pathname_boolean_(file, field, &check));
	return Result(ret, check? T: Nil);
}


/* pathname-match-p */
int pathname_match_p_common_(Execute ptr, addr *ret, addr a, addr b)
{
	int check;

	Return(pathname_designator_heap_(ptr, a, &a));
	Return(pathname_designator_heap_(ptr, b, &b));
	Return(wildcard_pathname_(a, b, 1, &check));
	return Result(ret, check? T: Nil);
}


/* translate-pathname */
int translate_pathname_common_(Execute ptr, addr *ret, addr pos, addr from, addr to)
{
	return translate_pathname_heap_(ptr, ret, pos, from, to);
}


/* translate-logical-pathname */
int translate_logical_pathname_common_(Execute ptr, addr *ret, addr pos)
{
	return physical_pathname_heap_(ptr, pos, ret);
}


/* merge-pathnames */
int merge_pathnames_common_(Execute ptr,
		addr *ret, addr pos, addr defaults, addr version)
{
	return merge_pathnames_clang_(ptr, pos, defaults, version, ret);
}


/************************************************************
 *  call_files.c
 ************************************************************/


/************************************************************
 *  call_hashtables.c
 ************************************************************/

/*
 *  make-hash-table
 */
static int make_hash_table_symbol_common(addr pos, enum HASHTABLE_TEST *ret)
{
	addr check;

	GetConst(COMMON_EQ, &check);
	if (pos == check) {
		*ret = HASHTABLE_TEST_EQ;
		return 0;
	}
	GetConst(COMMON_EQL, &check);
	if (pos == check) {
		*ret = HASHTABLE_TEST_EQL;
		return 0;
	}
	GetConst(COMMON_EQUAL, &check);
	if (pos == check) {
		*ret = HASHTABLE_TEST_EQUAL;
		return 0;
	}
	GetConst(COMMON_EQUALP, &check);
	if (pos == check) {
		*ret = HASHTABLE_TEST_EQUALP;
		return 0;
	}

	return 1;
}

static int make_hash_table_test_common_(addr rest, enum HASHTABLE_TEST *ret)
{
	addr pos, check;

	if (GetKeyArgs(rest, KEYWORD_TEST, &pos)) {
		*ret = HASHTABLE_TEST_EQL;
		return 0;
	}
	if (symbolp(pos)) {
		if (make_hash_table_symbol_common(pos, ret))
			goto error;
		return 0;
	}
	if (functionp(pos)) {
		GetNameFunction(pos, &check);
		if (check == Nil)
			goto error;
		GetCallName(check, &check);
		if (make_hash_table_symbol_common(check, ret))
			goto error;
		return 0;
	}
error:
	*ret = HASHTABLE_TEST_EQL;
	return fmte_("Invalid hash-hable-test ~S.", pos, NULL);
}

static int make_hash_table_size_common_(addr rest, size_t *ret)
{
	addr pos;

	if (GetKeyArgs(rest, KEYWORD_SIZE, &pos)) {
		*ret = HASHTABLE_SIZE_DEFAULT;
		return 0;
	}
	if (! integerp(pos)) {
		*ret = 0;
		return TypeError_(pos, INTEGER);
	}
	if (GetIndex_integer(pos, ret)) {
		*ret = 0;
		return fmte_("Invalid hash size ~S.", pos, NULL);
	}

	return 0;
}

static int make_hash_table_rehash_size_common_(addr rest,
		int *floatp, double_float *rehashf, size_t *rehashi)
{
	addr pos;
	double_float valuef;
	size_t valuei;

	if (GetKeyArgs(rest, KEYWORD_REHASH_SIZE, &pos)) {
		*floatp = 1;
		*rehashf = HASHTABLE_REHASH_SIZE_DEFAULT;
		return 0;
	}
	if (integerp(pos)) {
		if (GetIndex_integer(pos, &valuei)) {
			*floatp = 0;
			*rehashf = 0;
			*rehashi = 0;
			return fmte_("Invalid rehash-size ~S.", pos, NULL);
		}
		if (valuei < 1UL) {
			*floatp = 0;
			*rehashf = 0;
			*rehashi = 0;
			return fmte_("rehash-size ~S must be greater than 1.", pos, NULL);
		}
		*floatp = 0;
		*rehashi = valuei;
		return 0;
	}
	else {
		Return(cast_double_float_unsafe_(pos, &valuef));
		if (valuef <= 1.0) {
			*floatp = 0;
			*rehashf = 0;
			*rehashi = 0;
			return fmte_("rehash-size ~S "
					"must be greater than equal to 1.0.", pos, NULL);
		}
		*floatp = 1;
		*rehashf = valuef;
		return 0;
	}

	return 0;
}

static int make_hash_table_rehash_threshold_common_(addr rest, double_float *ret)
{
	addr pos;
	double_float value;

	if (GetKeyArgs(rest, KEYWORD_REHASH_THRESHOLD, &pos)) {
		value = HASHTABLE_REHASH_THRESHOLD_DEFAULT;
	}
	else {
		Return(cast_double_float_unsafe_(pos, &value));
		if (value < 0.0 || 1.0 < value) {
			*ret = 0.0;
			return fmte_("rehash-threshold ~S "
					"must be a number between 0.0 and 1.0.", pos, NULL);
		}
	}
	*ret = value;

	return 0;
}

int make_hash_table_common_(addr rest, addr *ret)
{
	enum HASHTABLE_TEST test;
	int floatp;
	size_t size, rehashi;
	double_float rehashf, threshold;

	Return(make_hash_table_test_common_(rest, &test));
	Return(make_hash_table_size_common_(rest, &size));
	Return(make_hash_table_rehash_size_common_(rest, &floatp, &rehashf, &rehashi));
	Return(make_hash_table_rehash_threshold_common_(rest, &threshold));

	if (floatp)
		hashtable_full_heap(ret, test, size, rehashf, threshold);
	else
		hashtable_integer_heap(ret, test, size, rehashi, threshold);

	return 0;
}


/*
 *  hash-table-count
 */
void hash_table_count_common(addr var, addr *ret)
{
	size_t size;
	getcount_hashtable(var, &size);
	make_index_integer_heap(ret, size);
}


/*
 *  hash-table-rehash-size
 */
int hash_table_rehash_size_common_(addr var, addr *ret)
{
	double_float valuef;
	size_t valuei;

	if (getrehash_float_hashtable(var, &valuef)) {
		double_float_heap(ret, valuef);
		return 0;
	}
	if (getrehash_integer_hashtable(var, &valuei)) {
		make_index_integer_heap(ret, valuei);
		return 0;
	}

	return fmte_("Invalid hash-table structure ~S.", var, NULL);
}


/*
 *  hash-table-rehash-threshold
 */
void hash_table_rehash_threshold_common(addr var, addr *ret)
{
	double_float value;
	getrehash_threshold_hashtable(var, &value);
	double_float_heap(ret, value);
}


/*
 *  hash-table-size
 */
void hash_table_size_common(addr var, addr *ret)
{
	size_t size;
	getsize_hashtable(var, &size);
	make_index_integer_heap(ret, size);
}


/*
 *  hash-table-test
 */
void hash_table_test_common(addr var, addr *ret)
{
	enum HASHTABLE_TEST test;

	gettest_hashtable(var, &test);
	switch (test) {
		case HASHTABLE_TEST_EQ:
			GetConst(COMMON_EQ, ret);
			break;

		case HASHTABLE_TEST_EQL:
			GetConst(COMMON_EQL, ret);
			break;

		case HASHTABLE_TEST_EQUAL:
			GetConst(COMMON_EQUAL, ret);
			break;

		case HASHTABLE_TEST_EQUALP:
			GetConst(COMMON_EQUALP, ret);
			break;

		default:
			*ret = Nil;
			break;
	}
}


/*
 *  gethash
 */
int gethash_common_(addr key, addr table, addr value, addr *ret, addr *check)
{
	addr pos;

	Return(find_hashtable_(table, key, &pos));
	if (pos == Unbound) {
		*check = Nil;
		return Result(ret, (value == Unbound)? Nil: value);
	}
	else {
		*check = T;
		return Result(ret, pos);
	}
}


/*
 *  (setf gethash)
 */
int setf_gethash_common_(LocalRoot local, addr value, addr key, addr table)
{
	Return(intern_hashheap_(table, key, &table));
	SetCdr(table, value);
	return 0;
}


/*
 *  remhash
 */
int remhash_common_(addr key, addr table, addr *ret)
{
	int check;
	Return(delete_hashtable_(table, key, &check));
	return Result(ret, check? Nil: T);
}


/*
 *  maphash
 */
static int maphash_execute_common_(Execute ptr, addr call, addr key, addr value)
{
	addr control;

	push_control(ptr, &control);
	(void)funcall_control_(ptr, call, key, value, NULL);
	return pop_control_(ptr, control);
}

int maphash_common_(Execute ptr, addr call, addr table)
{
	addr cons, key, value;
	size_t i, size;

	GetTableHash(table, &table);
	LenArrayHash(table, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &cons);
		while (cons != Nil) {
			GetCons(cons, &key, &cons);
			GetCons(key, &key, &value);
			Return(maphash_execute_common_(ptr, call, key, value));
		}
	}

	return 0;
}


/*
 *  with-hash-table-iterator
 */
static int with_hash_table_iterator_expand_common_(Execute ptr,
		addr name, addr table, addr body, addr *ret)
{
	/* (let ((inst (make-hash-iterator table)))
	 *   (declare (ignorable inst))
	 *   (macrolet ((name () (list (quote next-hash-iterator) (quote inst))))
	 *     (declare (ignorable name))
	 *     . body))
	 */
	addr let, declare, ignorable, macrolet, list, quote, make, next;
	addr inst, let1, let2, let3;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MACROLET, &macrolet);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_MAKE_HASH_ITERATOR, &make);
	GetConst(SYSTEM_NEXT_HASH_ITERATOR, &next);
	Return(make_gensym_(ptr, &inst));

	list_heap(&let1, make, table, NULL);
	list_heap(&let1, inst, let1, NULL);
	conscar_heap(&let1, let1);
	list_heap(&let2, ignorable, inst, NULL);
	list_heap(&let2, declare, let2, NULL);
	list_heap(&let3, quote, inst, NULL);
	list_heap(&next, quote, next, NULL);
	list_heap(&let3, list, next, let3, NULL);
	list_heap(&let3, name, Nil, let3, NULL);
	conscar_heap(&let3, let3);
	list_heap(&name, ignorable, name, NULL);
	list_heap(&name, declare, name, NULL);
	lista_heap(&let3, macrolet, let3, name, body, NULL);
	list_heap(ret, let, let1, let2, let3, NULL);

	return 0;
}

int with_hash_table_iterator_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, name, table, check;

	/* args */
	Return_getcdr(form, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &name, &args);
	if (! consp(name))
		goto error;
	GetCons(name, &name, &table);
	if (! consp(table))
		goto error;
	GetCons(table, &table, &check);
	if (check != Nil)
		goto error;
	/* ((name table) . args) */
	return with_hash_table_iterator_expand_common_(ptr, name, table, args, ret);

error:
	return fmte_("with-hash-table-iterator form ~S must be "
			"((name hash-table) &body body)" , form, NULL);
}


/*
 *  sxhash
 */
int sxhash_common_(addr var, addr *ret)
{
	fixnum value;

	Return(sxhash_equal_(var, &value));
	fixnum_heap(ret, value);

	return 0;
}


/************************************************************
 *  call_iteration.c
 ************************************************************/

/*
 *  do / do*
 */
static int do_expand_common_(addr *ret, constindex let_const, constindex setq_const,
		addr var, addr end, addr result, addr decl, addr body)
{
	/*  (let ((var1 init1) ;; do* -> let*
	 *        (var2 init2)
	 *        ...)
	 *    (declare ...)
	 *    (block nil
	 *      (tagbody
	 *        #:do-loop
	 *        (if end
	 *          (return-from nil (progn . result)))
	 *        body
	 *        (psetq var1 update1 var2 update2 ...) ;; do* -> setq
	 *        (go #:do-loop)))))
	 */
	addr let, setq, block, tagbody, ifsym, retsym, progn, go, gloop;
	addr root, car, cdr, a, b;

	GetConstant(let_const, &let);
	GetConstant(setq_const, &setq);
	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_IF, &ifsym);
	GetConst(COMMON_RETURN_FROM, &retsym);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_GO, &go);
	make_symbolchar(&gloop, "DO-LOOP");

	/* (go #:do-loop) */
	list_heap(&go, go, gloop, NULL);
	/* (psetq ...) */
	root = Nil;
	for (cdr = var; cdr != Nil; ) {
		GetCons(cdr, &car, &cdr);
		if (symbolp(car))
			continue;
		GetCons(car, &a, &car);
		if (car == Nil)
			continue;
		GetCdr(car, &car);
		if (car == Nil)
			continue;
		GetCar(car, &b);
		cons_heap(&root, a, root);
		cons_heap(&root, b, root);
	}
	if (root == Nil)
		setq = Nil;
	else {
		nreverse(&root, root);
		cons_heap(&setq, setq, root);
	}
	/* (if ...) */
	cons_heap(&progn, progn, result);
	list_heap(&retsym, retsym, Nil, progn, NULL);
	list_heap(&ifsym, ifsym, end, retsym, NULL);
	/* (tagbody ...) */
	conscar_heap(&root, tagbody);
	cons_heap(&root, gloop, root);
	cons_heap(&root, ifsym, root);
	while (body != Nil) {
		Return_getcons(body, &car, &body);
		cons_heap(&root, car, root);
	}
	if (setq != Nil)
		cons_heap(&root, setq, root);
	cons_heap(&root, go, root);
	nreverse(&root, root);
	/* (block nil ...) */
	list_heap(&block, block, Nil, root, NULL);
	/* (let args ...) */
	root = Nil;
	while (var != Nil) {
		GetCons(var, &car, &var);
		if (symbolp(car)) {
			cons_heap(&root, car, root);
			continue;
		}
		GetCons(car, &a, &cdr);
		if (cdr == Nil) {
			cons_heap(&root, car, root);
			continue;
		}
		GetCons(cdr, &b, &cdr);
		if (cdr == Nil) {
			cons_heap(&root, car, root);
			continue;
		}
		list_heap(&car, a, b, NULL);
		cons_heap(&root, car, root);
	}
	nreverse(&var, root);
	/* (let ...) */
	conscar_heap(&root, let);
	cons_heap(&root, var, root);
	while (decl != Nil) {
		GetCons(decl, &car, &decl);
		cons_heap(&root, car, root);
	}
	cons_heap(&root, block, root);
	nreverse(ret, root);

	return 0;
}

static int do_constant_common_(addr form, addr *ret,
		constindex do_constant,
		constindex let,
		constindex setq)
{
	addr args, name, var, end, result, decl, car, cdr;

	GetConstant(do_constant, &name);
	Return_getcdr(form, &args);
	if (! consp_getcons(args, &var, &args))
		goto error;
	for (cdr = var; cdr != Nil; ) { /* check only */
		if (! consp_getcons(cdr, &car, &cdr))
			return fmte_("~S variable argument ~S must be a list.", name, cdr, NULL);
		if (symbolp(car))
			continue;
		if (! consp_getcdr(car, &car)) /* var */
			goto error_var;
		if (car == Nil)
			continue;
		if (! consp_getcdr(car, &car)) /* init */
			goto error_var;
		if (car == Nil)
			continue;
		if (! consp_getcdr(car, &car))
			goto error_var;
		if (car != Nil)
			goto error_var;
	}
	if (! consp_getcons(args, &end, &args))
		goto error;
	if (! consp_getcons(end, &end, &result))
		goto error;
	Return(declare_body_form_(args, &decl, &args));
	return do_expand_common_(ret, let, setq, var, end, result, decl, args);

error:
	return fmte_("~S arguemnt ~S must be ((var ...) "
			"(test &body result) &body code).", name, form, NULL);
error_var:
	return fmte_("~S variable argument ~S must be "
			"(symbol &optional initial udpate).", name, cdr, NULL);
}

int do_common_(addr form, addr env, addr *ret)
{
	return do_constant_common_(form, ret,
			CONSTANT_COMMON_DO,
			CONSTANT_COMMON_LET,
			CONSTANT_COMMON_PSETQ);
}

int doa_common_(addr form, addr env, addr *ret)
{
	return do_constant_common_(form, ret,
			CONSTANT_COMMON_DOA,
			CONSTANT_COMMON_LETA,
			CONSTANT_COMMON_SETQ);
}


/*
 *  dotimes
 */
static void dotimes_expand_common(addr *ret,
		addr var, addr count, addr result, addr body)
{
	/*  `(do ((,var 0 (1+ ,var)))
	 *    ((<= ,count ,var) ,result)
	 *      ,@body))
	 */
	addr dosym, oneplus, lessequal, form;

	GetConst(COMMON_DO, &dosym);
	GetConst(COMMON_ONE_PLUS, &oneplus);
	GetConst(COMMON_NUMBER_LESS_EQUAL, &lessequal);

	list_heap(&form, lessequal, count, var, NULL);
	list_heap(&form, form, result, NULL);
	list_heap(&oneplus, oneplus, var, NULL);
	list_heap(&var, var, fixnumh(0), oneplus, NULL);
	conscar_heap(&var, var);
	lista_heap(ret, dosym, var, form, body, NULL);
}

int dotimes_common_(addr form, addr env, addr *ret)
{
	addr args, var, count, result, check;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &var, &args))
		goto error;
	if (! consp_getcons(var, &var, &count))
		goto error;
	if (! consp_getcons(count, &count, &result))
		goto error;
	if (result != Nil) {
		if (! consp_getcons(result, &result, &check))
			goto error;
		if (check != Nil)
			goto error;
	}
	dotimes_expand_common(ret, var, count, result, args);
	return 0;

error:
	return fmte_("DOTIMES arguemnt ~S must be "
			"((var count &optional result) &body body) form.", form, NULL);
}


/*
 *  dolist
 */
static int dolist_expand_common_(Execute ptr, addr *ret,
		addr var, addr list, addr result, addr decl, addr body)
{
	/* `(prog* ((,g ,list)
	 *          (,var (car ,g)))
	 *    ,@declare
	 *    (unless ,g
	 *      (go #:finish))
	 *    #:loop
	 *    ,@body
	 *    (setq ,g (cdr ,g))
	 *    (unless ,g
	 *      (go #:finish))
	 *    (setq ,var (car ,g))
	 *    (go #:loop)
	 *    #:finish
	 *    (return ,result))
	 */
	addr proga, car, cdr, unless, go, setq, retsym;
	addr g, loop, finish, x, setq1, setq2, root;

	GetConst(COMMON_PROGA, &proga);
	GetConst(COMMON_CAR, &car);
	GetConst(COMMON_CDR, &cdr);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_RETURN, &retsym);
	Return(make_gensym_(ptr, &g));
	make_symbolchar(&loop, "LOOP");
	make_symbolchar(&finish, "FINISH");

	/* result */
	list_heap(&retsym, retsym, result, NULL);
	/* (unless ...) */
	list_heap(&x, go, finish, NULL);
	list_heap(&unless, unless, g, x, NULL);
	/* (go loop) */
	list_heap(&go, go, loop, NULL);
	/* (,var ...) */
	list_heap(&car, car, g, NULL);
	list_heap(&var, var, car, NULL);
	/* (setq ,g ...) */
	list_heap(&cdr, cdr, g, NULL);
	list_heap(&setq1, setq, g, cdr, NULL);
	/* (setq ,var ...) */
	lista_heap(&setq2, setq, var, NULL);
	/* ((,g ...) ...) */
	list_heap(&g, g, list, NULL);
	list_heap(&g, g, var, NULL);
	/* (prog* ...) */
	conscar_heap(&root, proga);
	cons_heap(&root, g, root);
	while (decl != Nil) {
		Return_getcons(decl, &g, &decl);
		cons_heap(&root, g, root);
	}
	cons_heap(&root, unless, root);
	cons_heap(&root, loop, root);
	while (body != Nil) {
		Return_getcons(body, &g, &body);
		cons_heap(&root, g, root);
	}
	cons_heap(&root, setq1, root);
	cons_heap(&root, unless, root);
	cons_heap(&root, setq2, root);
	cons_heap(&root, go, root);
	cons_heap(&root, finish, root);
	cons_heap(&root, retsym, root);
	nreverse(ret, root);

	return 0;
}

int dolist_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, var, list, result, decl, check;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &var, &args))
		goto error;
	if (! consp_getcons(var, &var, &list))
		goto error;
	if (! consp_getcons(list, &list, &result))
		goto error;
	if (result != Nil) {
		if (! consp_getcons(result, &result, &check))
			goto error;
		if (check != Nil)
			goto error;
	}
	Return(declare_body_form_(args, &decl, &args));
	return dolist_expand_common_(ptr, ret, var, list, result, decl, args);

error:
	return fmte_("DOLIST arguemnt ~S must be "
			"((var list &optional result) &body body) form.", form, NULL);
}


/************************************************************
 *  call_numbers.c
 ************************************************************/

/*
 *  =
 */
int number_equal_common_(LocalRoot local, addr left, addr rest, int *ret)
{
	int check;
	addr right;

	for (; rest != Nil; left = right) {
		Return_getcons(rest, &right, &rest);
		Return(equal_number_(local, left, right, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}


/*
 *  /=
 */
int number_not_equal_common_(LocalRoot local, addr left, addr rest, int *ret)
{
	int check;
	addr right, list;

	while (rest != Nil) {
		for (list = rest; list != Nil; ) {
			Return_getcons(list, &right, &list);
			Return(equal_number_(local, left, right, &check));
			if (check)
				return Result(ret, 0);
		}
		Return_getcons(rest, &left, &rest);
	}

	return Result(ret, 1);
}


/*
 *  <
 */
int number_less_common_(LocalRoot local, addr left, addr rest, int *ret)
{
	int check;
	addr right;

	for (; rest != Nil; left = right) {
		Return_getcons(rest, &right, &rest);
		Return(less_number_(local, left, right, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}


/*
 *  >
 */
int number_greater_common_(LocalRoot local, addr left, addr rest, int *ret)
{
	int check;
	addr right;

	for (; rest != Nil; left = right) {
		Return_getcons(rest, &right, &rest);
		Return(greater_number_(local, left, right, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}


/*
 *  <=
 */
int number_less_equal_common_(LocalRoot local, addr left, addr rest, int *ret)
{
	int check;
	addr right;

	for (; rest != Nil; left = right) {
		Return_getcons(rest, &right, &rest);
		Return(less_equal_number_(local, left, right, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}


/*
 *  >=
 */
int number_greater_equal_common_(LocalRoot local, addr left, addr rest, int *ret)
{
	int check;
	addr right;

	for (; rest != Nil; left = right) {
		Return_getcons(rest, &right, &rest);
		Return(greater_equal_number_(local, left, right, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}


/*
 *  max
 */
int max_common_(LocalRoot local, addr left, addr rest, addr *ret)
{
	int check;
	addr right;

	while (rest != Nil) {
		Return_getcons(rest, &right, &rest);
		Return(less_number_(local, left, right, &check));
		if (check)
			left = right;
	}

	return Result(ret, left);
}


/*
 *  min
 */
int min_common_(LocalRoot local, addr left, addr rest, addr *ret)
{
	int check;
	addr right;

	while (rest != Nil) {
		Return_getcons(rest, &right, &rest);
		Return(greater_number_(local, left, right, &check));
		if (check)
			left = right;
	}

	return Result(ret, left);
}


/*
 *  +
 */
int plus_common_(LocalRoot local, addr rest, addr *ret)
{
	addr left, right;

	/* nil */
	if (rest == Nil) {
		fixnum_heap(ret, 0);
		return 0;
	}

	/* list */
	Return_getcons(rest, &left, &rest);
	while (rest != Nil) {
		Return_getcons(rest, &right, &rest);
		Return(plus_number_heap_(local, left, right, &left));
	}

	return Result(ret, left);
}


/*
 *  -
 */
int minus_common_(LocalRoot local, addr left, addr rest, addr *ret)
{
	addr right;

	/* nil */
	if (rest == Nil) {
		return sign_reverse_number_common_(left, ret);
	}

	/* list */
	while (rest != Nil) {
		Return_getcons(rest, &right, &rest);
		Return(minus_number_heap_(local, left, right, &left));
	}

	return Result(ret, left);
}


/*
 *  *
 */
int asterisk_common_(LocalRoot local, addr rest, addr *ret)
{
	addr left, right;

	/* nil */
	if (rest == Nil) {
		fixnum_heap(ret, 1);
		return 0;
	}

	/* list */
	Return_getcons(rest, &left, &rest);
	while (rest != Nil) {
		Return_getcons(rest, &right, &rest);
		Return(multi_number_heap_(local, left, right, &left));
	}

	return Result(ret, left);
}


/*
 *  /
 */
int slash_common_(LocalRoot local, addr left, addr rest, addr *ret)
{
	addr right;

	/* nil */
	if (rest == Nil) {
		return inverse_number_heap_(local, left, ret);
	}

	/* list */
	while (rest != Nil) {
		Return_getcons(rest, &right, &rest);
		Return(div_number_heap_(local, left, right, &left));
	}

	return Result(ret, left);
}


/*
 *  incf
 */
static int incf_expand_common_(Execute ptr, addr *ret, addr place, addr value, addr env)
{
	addr a, b, g, w, r;
	addr c, d, ig, args, leta, declare, ignorable, plus;

	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	if (! singlep(g))
		return fmte_("INCF place ~S don't allow a multiple store value.", place, NULL);

	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        (g (+ r value)))  ;; (g (1+ r))
	 *   (declare (ignorable a1 a2))
	 *   w g)
	 */
	args = Nil;
	for (ig = a; a != Nil; ) {
		Return_getcons(a, &c, &a);
		Return_getcons(b, &d, &b);
		list_heap(&c, c, d, NULL);
		cons_heap(&args, c, args);
	}
	/* g */
	if (value == Unbound) {
		GetConst(COMMON_ONE_PLUS, &plus);
		list_heap(&plus, plus, r, NULL);
	}
	else {
		GetConst(COMMON_PLUS, &plus);
		list_heap(&plus, plus, r, value, NULL);
	}
	Return_getcar(g, &g);
	list_heap(&plus, g, plus, NULL);
	cons_heap(&args, plus, args);
	nreverse(&args, args);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, ig);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	list_heap(ret, leta, args, declare, w, g, NULL);

	return 0;
}

int incf_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, place, value;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &place, &args))
		goto error;
	if (args == Nil) {
		value = Unbound;
	}
	else {
		Return_getcons(args, &value, &args);
		if (args != Nil)
			goto error;
	}
	return incf_expand_common_(ptr, ret, place, value, env);

error:
	*ret = Nil;
	return fmte_("INCF ~S must be (place &optional value) form.", form, NULL);
}


/*
 *  decf
 */
static int decf_expand_common_(Execute ptr, addr *ret, addr place, addr value, addr env)
{
	addr a, b, g, w, r;
	addr c, d, ig, args, leta, declare, ignorable, minus;

	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	if (! singlep(g))
		return fmte_("DECF place ~S don't allow a multiple store value.", place, NULL);

	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        (g (- r value)))  ;; (g (1- r))
	 *   (declare (ignorable a1 a2))
	 *   w g)
	 */
	args = Nil;
	for (ig = a; a != Nil; ) {
		Return_getcons(a, &c, &a);
		Return_getcons(b, &d, &b);
		list_heap(&c, c, d, NULL);
		cons_heap(&args, c, args);
	}
	/* g */
	if (value == Unbound) {
		GetConst(COMMON_ONE_MINUS, &minus);
		list_heap(&minus, minus, r, NULL);
	}
	else {
		GetConst(COMMON_MINUS, &minus);
		list_heap(&minus, minus, r, value, NULL);
	}
	Return_getcar(g, &g);
	list_heap(&minus, g, minus, NULL);
	cons_heap(&args, minus, args);
	nreverse(&args, args);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, ig);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	list_heap(ret, leta, args, declare, w, g, NULL);

	return 0;
}

int decf_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, place, value;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &place, &args))
		goto error;
	if (args == Nil) {
		value = Unbound;
	}
	else {
		Return_getcons(args, &value, &args);
		if (args != Nil)
			goto error;
	}
	return decf_expand_common_(ptr, ret, place, value, env);

error:
	*ret = Nil;
	return fmte_("DECF ~S must be (place &optional value) form.", form, NULL);
}


/*
 *  random
 */
int random_common_(Execute ptr, addr limit, addr state, addr *ret)
{
	if (state == Unbound) {
		/* symbol-value *random-state* */
		GetConst(SPECIAL_RANDOM_STATE, &state);
		Return(getspecialcheck_local_(ptr, state, &state));
	}
	return random_number_common_(ptr->local, limit, state, ret);
}


/*
 *  conjugate
 */
int conjugate_common_(addr var, addr *ret)
{
	addr real, imag;

	if (complexp(var)) {
		GetRealComplex(var, &real);
		GetImagComplex(var, &imag);
		Return(sign_reverse_real_common_(imag, &imag));
		Return(complex_heap_(ret, real, imag));
		return 0;
	}
	if (realp(var))
		return Result(ret, var);

	/* error */
	*ret = Nil;
	return TypeError_(var, NUMBER);
}


/*
 *  realpart
 */
int realpart_common_(addr var, addr *ret)
{
	if (complexp(var)) {
		GetRealComplex(var, &var);
	}

	return real_throw_heap_(var, ret);
}


/*
 *  imagpart
 */
static int imagpart_complex_common_(addr var, addr *ret)
{
	GetImagComplex(var, &var);
	return real_throw_heap_(var, ret);
}

static int imagpart_rational_common_(addr var, addr *ret)
{
	fixnum_heap(ret, 0);
	return 0;
}

static int imagpart_single_common_(addr var, addr *ret)
{
	single_float value;

	GetSingleFloat(var, &value);
	if (value == 0.0f)  /* plus or minus */
		return Result(ret, var);

	single_float_heap(ret, 0.0f);
	return 0;
}

static int imagpart_double_common_(addr var, addr *ret)
{
	double_float value;

	GetDoubleFloat(var, &value);
	if (value == 0.0)  /* plus or minus */
		return Result(ret, var);

	double_float_heap(ret, 0.0);
	return 0;
}

static int imagpart_long_common_(addr var, addr *ret)
{
	long_float value;

	GetLongFloat(var, &value);
	if (value == 0.0L)  /* plus or minus */
		return Result(ret, var);

	long_float_heap(ret, 0.0L);
	return 0;
}

int imagpart_common_(addr var, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_COMPLEX:
			return imagpart_complex_common_(var, ret);

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			return imagpart_rational_common_(var, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return imagpart_single_common_(var, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return imagpart_double_common_(var, ret);

		case LISPTYPE_LONG_FLOAT:
			return imagpart_long_common_(var, ret);

		default:
			*ret = Nil;
			return TypeError_(var, NUMBER);
	}
}


/*
 *  parse-integer
 */
int parse_integer_common_(LocalRoot local,
		addr var, addr rest, addr *ret1, addr *ret2)
{
	addr radix, junk;
	size_t size, start, end;

	string_length(var, &size);
	Return(keyword_start_end_(size, rest, &start, &end));
	if (GetKeyArgs(rest, KEYWORD_RADIX, &radix))
		fixnum_heap(&radix, 10);
	if (GetKeyArgs(rest, KEYWORD_JUNK_ALLOWED, &junk))
		junk = Nil;
	return parse_integer_clang_(local, var, start, end,
			(unsigned)RefFixnum(radix), junk != Nil, ret1, ret2);
}


/************************************************************
 *  call_objects.c
 ************************************************************/

/*
 *  defclass
 */
static int defclass_parse_superclasses_(addr args, int defclass, addr *ret)
{
	addr root, pos, quote, find, refer, list;

	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_FIND_CLASS, &find);
	GetConst(CLOSNAME_REFERENCED_CLASS, &refer);
	GetConst(COMMON_LIST, &list);

	/* () -> (list (find-class (quote standard-object))) */
	if (args == Nil) {
		if (defclass)
			GetConst(COMMON_STANDARD_OBJECT, &pos);
		else
			GetConst(COMMON_CONDITION, &pos);
		list_heap(&pos, quote, pos, NULL);
		list_heap(&pos, find, pos, NULL);
		list_heap(ret, list, pos, NULL);
		return 0;
	}

	/* (a b ...) ->
	 *   (list (referenced-class (quote a))
	 *         (referenced-class (quote b))
	 *         ...)
	 */
	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &pos, &args);
		list_heap(&pos, quote, pos, NULL);
		list_heap(&pos, refer, pos, NULL);
		cons_heap(&root, pos, root);
	}
	/* result */
	nreverse(&root, root);
	cons_heap(ret, list, root);

	return 0;
}

static int defclass_eqconst(addr left, constindex index)
{
	addr right;
	GetConstant(index, &right);
	return left == right;
}
#define DefClassEqConst(x,y) defclass_eqconst((x), CONSTANT_KEYWORD_##y)

static int non_nil_symbol_p(addr pos)
{
	return (pos != Nil) && symbolp(pos);
}

static int defclass_allocation_p(addr pos)
{
	return DefClassEqConst(pos, INSTANCE) || DefClassEqConst(pos, CLASS);
}

static int defclass_parse_slotlist_(Execute ptr, addr env, addr list, addr *ret)
{
	addr name, key, value, pos, root, quote;
	addr readers, writers, allocation, initargs, initform, initfunction;
	addr type, doc, others;
	LocalHold hold;

	/* name */
	if (! consp(list))
		return fmte_("SLOT-SPECIFIER ~S must be a (name . tail) form.", list, NULL);
	GetCons(list, &name, &list);
	if (! symbolp(name))
		return fmte_("SLOT-NAME ~S must be a symbol.", name, NULL);

	/* arguments */
	readers = Nil;
	writers = Nil;
	allocation = Nil;
	initargs = Nil;
	initform = Unbound;
	initfunction = Nil;
	type = Nil;
	doc = Nil;
	others = Nil;

	hold = LocalHold_array(ptr, 6);
	while (list != Nil) {
		/* key - value */
		if (! consp(list))
			return fmte_("Invalid slot-specifier value ~S.", list, NULL);
		GetCons(list, &key, &list);
		if (! consp(list))
			return fmte_("SLOT-SPECIFIER ~S must be a key-value form", list, NULL);
		GetCons(list, &value, &list);

		/* :reader */
		if (DefClassEqConst(key, READER)) {
			if (! non_nil_symbol_p(value))
				return fmte_(":READER ~S must be a non-nil symbol.", value, NULL);
			pushnew_heap(readers, value, &readers);
			localhold_set(hold, 0, readers);
			continue;
		}

		/* :writer */
		if (DefClassEqConst(key, WRITER)) {
			if (! function_name_p(value))
				return fmte_(":WRITER ~S must be function name.", value, NULL);
			Return(pushnew_equal_heap_(writers, value, &writers));
			localhold_set(hold, 1, writers);
			continue;
		}

		/* :accessor */
		if (DefClassEqConst(key, ACCESSOR)) {
			if (! non_nil_symbol_p(value))
				return fmte_(":ACCESSOR ~S must be a non-nil symbol.", value, NULL);
			pushnew_heap(readers, value, &readers);
			localhold_set(hold, 0, readers);

			GetConst(COMMON_SETF, &pos);
			list_heap(&value, pos, value, NULL);
			Return(pushnew_equal_heap_(writers, value, &writers));
			localhold_set(hold, 1, writers);
			continue;
		}

		/* :allocation */
		if (DefClassEqConst(key, ALLOCATION)) {
			if (! defclass_allocation_p(value)) {
				return fmte_(":ALLOCATION ~S "
						"must be a :instance or :class.", value, NULL);
			}
			if (allocation != Nil) {
				return call_simple_program_error_va_(NULL,
						":ALLOCATION is already exist.", NULL);
			}
			allocation = value;
			continue;
		}

		/* :initarg */
		if (DefClassEqConst(key, INITARG)) {
			if (! symbolp(value))
				return fmte_(":INITARG ~S must be a symbol.", value, NULL);
			pushnew_heap(initargs, value, &initargs);
			localhold_set(hold, 2, initargs);
			continue;
		}

		/* :initform */
		if (DefClassEqConst(key, INITFORM)) {
			if (initfunction != Nil) {
				return call_simple_program_error_va_(NULL,
						":INITFORM is already exist.", NULL);
			}
			initform = value;
			GetConst(COMMON_LAMBDA, &pos);
			list_heap(&initfunction, pos, Nil, value, NULL);
			localhold_set(hold, 3, initfunction);
			continue;
		}

		/* :type */
		if (DefClassEqConst(key, TYPE)) {
			if (type != Nil) {
				return call_simple_program_error_va_(NULL,
						":TYPE is already exist.", NULL);
			}
			type = value;
			localhold_set(hold, 4, type);
			continue;
		}

		/* :document */
		if (DefClassEqConst(key, DOCUMENTATION)) {
			if (doc != Nil) {
				return call_simple_program_error_va_(NULL,
						":DOCUMENTATION is already exist.", NULL);
			}
			if (! stringp(value))
				return fmte_(":DOCUMENTATION ~S must be a string.", value, NULL);
			doc = value;
			continue;
		}

		/* error */
		return call_simple_program_error_va_(NULL,
				"Invalid slot option ~S.", key, NULL);
	}
	localhold_end(hold);

	root = Nil;
	GetConst(COMMON_QUOTE, &quote);
	/* list :name (quote name) */
	GetConst(COMMON_LIST, &pos);
	cons_heap(&root, pos, root);
	GetConst(KEYWORD_NAME, &pos);
	cons_heap(&root, pos, root);
	list_heap(&pos, quote, name, NULL);
	cons_heap(&root, pos, root);
	/* :readers (quote (a b c ...)) */
	if (readers != Nil) {
		GetConst(CLOSKEY_READERS, &pos);
		cons_heap(&root, pos, root);
		nreverse(&readers, readers);
		list_heap(&pos, quote, readers, NULL);
		cons_heap(&root, pos, root);
	}
	/* :writers (quote (a b c ...)) */
	if (writers != Nil) {
		GetConst(CLOSKEY_WRITERS, &pos);
		cons_heap(&root, pos, root);
		nreverse(&writers, writers);
		list_heap(&pos, quote, writers, NULL);
		cons_heap(&root, pos, root);
	}
	/* :allocation v */
	if (allocation != Nil) {
		GetConst(KEYWORD_ALLOCATION, &pos);
		cons_heap(&root, pos, root);
		cons_heap(&root, allocation, root);
	}
	/* :initargs (quote (a b c ...)) */
	if (initargs != Nil) {
		GetConst(CLOSKEY_INITARGS, &pos);
		cons_heap(&root, pos, root);
		nreverse(&initargs, initargs);
		list_heap(&pos, quote, initargs, NULL);
		cons_heap(&root, pos, root);
	}
	/* :initform (quote a) :initfunction b */
	if (initfunction != Nil) {
		GetConst(KEYWORD_INITFORM, &pos);
		cons_heap(&root, pos, root);
		list_heap(&pos, quote, initform, NULL);
		cons_heap(&root, pos, root);
		GetConst(CLOSKEY_INITFUNCTION, &pos);
		cons_heap(&root, pos, root);
		cons_heap(&root, initfunction, root);
	}
	/* :type (quote a) */
	if (type != Nil) {
		GetConst(KEYWORD_TYPE, &pos);
		cons_heap(&root, pos, root);
		list_heap(&pos, quote, type, NULL);
		cons_heap(&root, pos, root);
	}
	/* :documentation a */
	if (doc != Nil) {
		GetConst(KEYWORD_DOCUMENTATION, &pos);
		cons_heap(&root, pos, root);
		list_heap(&pos, quote, doc, NULL);
		cons_heap(&root, pos, root);
	}
	/* others */
	if (others != Nil) {
		nreverse(&others, others);
		while (others != Nil) {
			GetCons(others, &pos, &others);
			cons_heap(&root, pos, root);
		}
	}
	/* result */
	nreverse(ret, root);

	return 0;
}

static int defclass_parse_slot_(Execute ptr, addr env, addr list, addr *ret)
{
	addr symbol, keyword, quote;

	/* symbol */
	if (symbolp(list)) {
		/* (list :name (quote value)) */
		GetConst(COMMON_QUOTE, &quote);
		GetConst(COMMON_LIST, &symbol);
		GetConst(CLOSKEY_NAME, &keyword);
		list_heap(&list, quote, list, NULL);
		list_heap(ret, symbol, keyword, list, NULL);
		return 0;
	}

	/* list */
	if (consp(list))
		return defclass_parse_slotlist_(ptr, env, list, ret);

	/* error */
	*ret = Nil;
	return fmte_("DEFCLASS slot-specifier ~S must be a list or a symbol,", list, NULL);
}

static int defclass_parse_slots_(Execute ptr, addr env, addr list, addr *ret)
{
	addr root, pos;
	LocalHold hold;

	/* check */
	if (list == Nil)
		return Result(ret, Nil);
	/* hold */
	hold = LocalHold_array(ptr, 1);
	localhold_pushva_force(hold, env, list, NULL);
	/* (list ,@mapcar) */
	GetConst(COMMON_LIST, &root);
	conscar_heap(&root, root);
	localhold_set(hold, 0, root);
	while (list != Nil) {
		if (! consp(list))
			return fmte_("DEFCLASS slot-specifier ~S must be a list.", list, NULL);
		GetCons(list, &pos, &list);
		Return(defclass_parse_slot_(ptr, env, pos, &pos));
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	/* result */
	nreverse(ret, root);

	return 0;
}

static int defclass_parse_initargs_(addr args, addr *ret)
{
	/* (:aaa 100 bbb (hello))
	 * -> (list (list (quote :aaa) (quote 100) (lambda () 100))
	 *          (list (quote bbb) (quote (hello)) (lambda () (hello))))
	 */
	addr root, key, value, list, quote, lambda, a, b, c;

	/* parse */
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_LAMBDA, &lambda);
	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &key, &args);
		Return_getcons(args, &value, &args);
		list_heap(&a, quote, key, NULL);
		list_heap(&b, quote, value, NULL);
		list_heap(&c, lambda, Nil, value, NULL);
		list_heap(&value, list, a, b, c, NULL);
		cons_heap(&root, value, root);
	}

	/* result */
	if (root == Nil)
		*ret = Nil;
	else {
		nreverse(&root, root);
		cons_heap(ret, list, root);
	}

	return 0;
}

static int defclass_parse_options_(addr list, int defclass, addr *ret, addr *report)
{
	addr root, key, value;

	*report = NULL;
	for (root = Nil; list != Nil; ) {
		if (! consp(list))
			return fmte_("DEFCLASS options ~S don't allow dotted list.", list, NULL);
		GetCons(list, &key, &list);
		if (! consp(key))
			return fmte_("DEFCLASS option ~S must be a cons.", key, NULL);
		GetCons(key, &key, &value);

		/* :metaclass */
		if (DefClassEqConst(key, METACLASS)) {
			if (! defclass)
				return fmte_(":METACLASS is not supported in DEFINE-CONBINATION.", NULL);
			if (! singlep(value))
				return fmte_("Invalid :METACLASS ~S.", value, NULL);
			GetCar(value, &value);
			if (! non_nil_symbol_p(value))
				return fmte_(":METACLASS ~S must be a non-nil symbol.", value, NULL);
			/* :metaclass (find-class (quote value)) */
			cons_heap(&root, key, root);
			GetConst(COMMON_QUOTE, &key);
			list_heap(&value, key, value, NULL);
			GetConst(COMMON_FIND_CLASS, &key);
			list_heap(&value, key, value, NULL);
			cons_heap(&root, value, root);
			continue;
		}

		/* :default-initargs */
		if (DefClassEqConst(key, DEFAULT_INITARGS)) {
			Return(defclass_parse_initargs_(value, &value));
			GetConst(CLOSKEY_DIRECT_DEFAULT_INITARGS, &key);
			cons_heap(&root, key, root);
			cons_heap(&root, value, root);
			continue;
		}

		/* :documentation */
		if (DefClassEqConst(key, DOCUMENTATION)) {
			if (! singlep(value))
				return fmte_("Invalid :DOCUMENTATION ~S.", value, NULL);
			GetCar(value, &value);
			if (! stringp(value))
				return fmte_(":DOCUMENTATION ~S must be a string.", value, NULL);
			/* :documentation value */
			cons_heap(&root, key, root);
			cons_heap(&root, value, root);
			continue;
		}

		if (DefClassEqConst(key, REPORT)) {
			if (defclass)
				return fmte_(":REPORT is not supported in DEFCLASS.", NULL);
			if (! singlep(value))
				return fmte_("Invalid :REPORT ~S.", value, NULL);
			GetCar(value, &value);
			/* :report -> defmethod */
			*report = value;
			continue;
		}

		/* error */
		return call_simple_program_error_va_(NULL,
				"Invalid class option ~S.", key, NULL);
	}

	/* result */
	nreverse(ret, root);
	return 0;
}

static void define_condition_report_string(addr *ret, addr name, addr report)
{
	/* (defmethod print-object ((inst name) stream)
	 *   (declare (ignore inst))
	 *   (write-string report stream))
	 */
	addr defmethod, print, inst, stream, x;
	addr declare, ignore, write;

	GetConst(COMMON_DEFMETHOD, &defmethod);
	GetConst(COMMON_PRINT_OBJECT, &print);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORE, &ignore);
	GetConst(COMMON_WRITE_STRING, &write);

	make_symbolchar(&inst, "INST");
	make_symbolchar(&stream, "STREAM");
	list_heap(&x, inst, name, NULL);
	list_heap(&x, x, stream, NULL);
	list_heap(&write, write, report, stream, NULL);
	list_heap(&ignore, ignore, inst, NULL);
	list_heap(&declare, declare, ignore, NULL);
	list_heap(ret, defmethod, print, x, declare, write, NULL);
}

static void define_condition_report_symbol(addr *ret, addr name, addr report)
{
	/* (defmethod print-object ((inst name) stream)
	 *   (if *print-escape*
	 *     (call-next-method)
	 *     (report inst stream)))
	 */
	addr defmethod, print, inst, stream, x;
	addr ifsymbol, escape, call;

	GetConst(COMMON_DEFMETHOD, &defmethod);
	GetConst(COMMON_PRINT_OBJECT, &print);
	GetConst(COMMON_IF, &ifsymbol);
	GetConst(SPECIAL_PRINT_ESCAPE, &escape);
	GetConst(COMMON_CALL_NEXT_METHOD, &call);

	make_symbolchar(&inst, "INST");
	make_symbolchar(&stream, "STREAM");
	list_heap(&x, inst, name, NULL);
	list_heap(&x, x, stream, NULL);
	list_heap(&report, report, inst, stream, NULL);
	list_heap(&call, call, NULL);
	list_heap(&ifsymbol, ifsymbol, escape, call, report, NULL);
	list_heap(ret, defmethod, print, x, ifsymbol, NULL);
}

static void define_condition_report_lambda(addr *ret, addr name, addr report)
{
	/* (defmethod print-object ((inst name) stream)
	 *   (funcall (function report) inst stream))
	 */
	addr defmethod, print, inst, stream, x;
	addr funcall, fsymbol;

	GetConst(COMMON_DEFMETHOD, &defmethod);
	GetConst(COMMON_PRINT_OBJECT, &print);
	GetConst(COMMON_FUNCALL, &funcall);
	GetConst(COMMON_FUNCTION, &fsymbol);

	make_symbolchar(&inst, "INST");
	make_symbolchar(&stream, "STREAM");
	list_heap(&x, inst, name, NULL);
	list_heap(&x, x, stream, NULL);
	list_heap(&fsymbol, fsymbol, report, NULL);
	list_heap(&funcall, funcall, fsymbol, inst, stream, NULL);
	list_heap(ret, defmethod, print, x, funcall, NULL);
}

static void define_condition_report(addr *ret, addr args, addr name, addr report)
{
	addr form, prog1, x;

	/* (class-name
	 *   (ensure-class ...))
	 */
	GetConst(COMMON_CLASS_NAME, &form);
	list_heap(&form, form, args, NULL);

	/* (prog1
	 *   [define-condition]
	 *   (defmethod print-object ((inst name) stream)
	 *     ...))
	 */
	if (report) {
		GetConst(COMMON_PROG1, &prog1);
		if (stringp(report))
			define_condition_report_string(&x, name, report);
		else if (symbolp(report))
			define_condition_report_symbol(&x, name, report);
		else
			define_condition_report_lambda(&x, name, report);
		list_heap(&form, prog1, form, x, NULL);
	}
	*ret = form;
}

static int defclass_define_condition_(Execute ptr,
		addr form, addr env, addr *ret, int defclass)
{
	/* `(system::ensure-class ',name
	 *     :direct-superclasses ,supers
	 *     :direct-slots ,slots
	 *     ,@class-options)
	 */
	addr first, args, name, nameq, supers, slots, options, ensure;
	addr key1, key2, report;
	LocalHold hold;

	/* destructuring-bind */
	Return_getcons(form, &first, &args);
	if (! consp_getcons(args, &name, &args))
		goto error;
	if (! consp_getcons(args, &supers, &args))
		goto error;
	if (! consp_getcons(args, &slots, &options))
		goto error;

	hold = LocalHold_local(ptr);
	/* name */
	GetConst(COMMON_QUOTE, &key1);
	list_heap(&nameq, key1, name, NULL);
	localhold_push(hold, nameq);

	/* parse */
	Return(defclass_parse_superclasses_(supers, defclass, &supers));
	localhold_push(hold, supers);
	Return(defclass_parse_slots_(ptr, env, slots, &slots));
	Return(defclass_parse_options_(options, defclass, &options, &report));

	/* make */
	GetConst(CLOSNAME_ENSURE_CLASS, &ensure);
	GetConst(CLOSKEY_DIRECT_SUPERCLASSES, &key1);
	GetConst(CLOSKEY_DIRECT_SLOTS, &key2);
	lista_heap(&args, ensure, nameq, key1, supers, key2, slots, options, NULL);
	if (! defclass)
		define_condition_report(&args, args, name, report);
	return Result(ret, args);

error:
	*ret = Nil;
	return fmte_("The ~S ~S must be a "
			"(~S name (superclasses) (slots) ...) form.", first, form, first, NULL);
}

int defclass_common_(Execute ptr, addr form, addr env, addr *ret)
{
	return defclass_define_condition_(ptr, form, env, ret, 1);
}

int define_condition_common_(Execute ptr, addr form, addr env, addr *ret)
{
	return defclass_define_condition_(ptr, form, env, ret, 0);
}


/*
 *  find-class
 */
int find_class_common_(addr pos, int errorp, addr env_ignore, addr *ret)
{
	Check(! symbolp(pos), "type error");
	if (errorp) {
		Return(clos_find_class_(pos, ret));
	}
	else {
		clos_find_class_nil(pos, ret);
	}

	return 0;
}


/*
 *  (setf find-class)
 */
void setf_find_class_common(addr pos, addr name, addr env_ignore)
{
	Check((! closp(pos)) && (pos != Nil), "type error");
	Check(! symbolp(name), "type error");
	clos_define_class(name, pos);
}


/*
 *  with-accessors
 */
static int with_accessors_arguments_(addr args, addr g, addr *ret)
{
	addr root, var, name, temp;

	for (root = Nil; args != Nil; ) {
		if (! consp_getcons(args, &var, &args))
			goto error;
		/* parse */
		if (! consp_getcons(var, &var, &temp))
			goto error;
		if (! consp_getcons(temp, &name, &temp))
			goto error;
		if (temp != Nil)
			goto error;
		if (! symbolp(var))
			return fmte_("WITH-ACCESSORS argument ~S must be a symbol.", var, NULL);
		if (! symbolp(name))
			return fmte_("WITH-ACCESSORS argument ~S must be a symbol.", name, NULL);
		/* expand */
		list_heap(&name, name, g, NULL);
		list_heap(&var, var, name, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);
	return 0;

error:
	*ret = Nil;
	return fmte_("WITH-ACCESSORS arguments ~S "
			"must be a (var name) form.", args, NULL);
}

int with_accessors_common_(Execute ptr, addr form, addr env, addr *ret)
{
	/* `(let ((,g ,expr))
	 *    (declare (ignorable g))
	 *    (symbol-macrolet ((,var1 (,name1 ,g))
	 *                      (,varN (,nameN ,g)))
	 *      ,@args))
	 */
	addr args, var, expr, g, let, declare, ignorable, symm;

	/* arguments */
	Return_getcdr(form, &args);
	if (! consp_getcons(args, &var, &args))
		goto error;
	if (! consp_getcons(args, &expr, &args))
		goto error;

	/* expand */
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_SYMBOL_MACROLET, &symm);
	Return(make_gensym_(ptr, &g));
	Return(with_accessors_arguments_(var, g, &var));
	lista_heap(&symm, symm, var, args, NULL);
	list_heap(&expr, g, expr, NULL);
	list_heap(&expr, expr, NULL);
	list_heap(&ignorable, ignorable, g, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(ret, let, expr, declare, symm, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("WITH-ACCESSORS argument ~S must be a "
			"((var ...) &body form) form.", form, NULL);
}


/*
 *  with-slots
 */
static int with_slots_arguments_(addr args, addr g, addr *ret)
{
	addr slot, quote, root, var, name, temp;

	GetConst(COMMON_SLOT_VALUE, &slot);
	GetConst(COMMON_QUOTE, &quote);
	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &var, &args);
		/* parse */
		if (symbolp(var)) {
			name = var;
		}
		else {
			if (! consp_getcons(var, &var, &temp))
				goto error;
			if (! consp_getcons(temp, &name, &temp))
				goto error;
			if (temp != Nil)
				goto error;
			if (! symbolp(var))
				return fmte_("WITH-SLOTS argument ~S must be a symbol.", var, NULL);
			if (! symbolp(name))
				return fmte_("WITH-SLOTS argument ~S must be a symbol.", name, NULL);
		}
		/* expand */
		list_heap(&name, quote, name, NULL);
		list_heap(&name, slot, g, name, NULL);
		list_heap(&var, var, name, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);
	return 0;

error:
	*ret = Nil;
	return fmte_("WITH-SLOTS arguments ~S must be "
			"a symbol or (var name) form.", args, NULL);
}

int with_slots_common_(Execute ptr, addr form, addr env, addr *ret)
{
	/* `(let ((,g ,expr))
	 *    (declare (ignorable g))
	 *    (symbol-macrolet ((,var1 (slot-value ,g ',name1))
	 *                      (,varN (slot-value ,g ',nameN)))
	 *      ,@args))
	 */
	addr args, var, expr, g, let, declare, ignorable, symm;

	/* arguments */
	Return_getcdr(form, &args);
	if (! consp_getcons(args, &var, &args))
		goto error;
	if (! consp_getcons(args, &expr, &args))
		goto error;

	/* expand */
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_SYMBOL_MACROLET, &symm);
	Return(make_gensym_(ptr, &g));
	Return(with_slots_arguments_(var, g, &var));
	lista_heap(&symm, symm, var, args, NULL);
	list_heap(&expr, g, expr, NULL);
	list_heap(&expr, expr, NULL);
	list_heap(&ignorable, ignorable, g, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(ret, let, expr, declare, symm, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("WITH-SLOTS argument ~S "
			"must be a ((var ...) &body form) form.", form, NULL);
}


/*
 *  (defmacro defgeneric (name lambda &rest args) ...) -> generic-function
 *  args ->
 *    (:argument-precedence-order parameter-name+)
 *    (declare gf-declaration+)
 *    (:documentation gf-documentation)
 *    (:method-combination method-combination method-combination-argument*)
 *    (:generic-function-class generic-function-class)
 *    (:method-class method-class)
 *    (:method qualifiers* lambda declare* document* form*)*
 */
static int defgeneric_parse_order_(addr list, addr *ret)
{
	addr check;

	*ret = list;
	while (list != Nil) {
		Return_getcons(list, &check, &list);
		if (! symbolp(check)) {
			return fmte_(":ARGUMENT-PRECEDENCE-ORDER ~S "
					"must be a symbol.", check, NULL);
		}
	}

	return 0;
}

static int defgeneric_parse_declare_(addr list, addr *ret)
{
	int check;
	addr decl;

	Return(parse_optimize_heap_(list, &decl, &check));
	if (check)
		return fmte_(":DECLARE accept only OPTIMIZE but ~S.", list, NULL);

	return Result(ret, decl);
}

static int defgeneric_parse_document_(addr list, addr *ret)
{
	addr pos, doc;

	if (! consp_getcons(list, &doc, &pos))
		goto error;
	if (pos != Nil)
		goto error;
	if (! stringp(doc))
		return fmte_(":DOCUMENTATION ~S must be a string.", doc, NULL);
	return Result(ret, doc);

error:
	*ret = Nil;
	return fmte_(":DOCUMENTATION ~S must be a (string) form.", list, NULL);
}

static int defgeneric_parse_method_combination_(addr list, addr *ret)
{
	addr pos, check;

	if (! consp_getcons(list, &check, &pos))
		goto error;
	if (! symbolp(check))
		goto error;
	return Result(ret, list);

error:
	*ret = Nil;
	return fmte_(":METHOD-COMBINATION ~S must be a "
			"(symbol arguments*) form.", list, NULL);
}

static int defgeneric_parse_generic_(addr list, addr *ret)
{
	addr name, tail, find, quote;

	/* (find-class (quote name)) */
	if (! consp_getcons(list, &name, &tail))
		return fmte_(":GENERIC-FUNCTION-CLASS ~S must be a (symbol) form.", list, NULL);
	if (name == Nil)
		return fmte_(":GENERIC-FUNCTION-CLASS must be a non-nil symbol.", NULL);
	if (! symbolp(name))
		return fmte_(":GENERIC-FUNCTION-CLASS ~S must be a symbol.", name, NULL);
	if (tail != Nil)
		return fmte_(":GENERIC-FUNCTION-CLASS ~S must be (symbol) form.", list, NULL);

	GetConst(COMMON_FIND_CLASS, &find);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&name, quote, name, NULL);
	list_heap(ret, find, name, NULL);

	return 0;
}

static int defgeneric_parse_method_(addr list, addr *ret)
{
	addr name, tail, find, quote;

	/* (find-class (quote name)) */
	if (! consp_getcons(list, &name, &tail))
		return fmte_(":METHOD-CLASS ~S must be a (symbol) form.", list, NULL);
	if (name == Nil)
		return fmte_(":METHOD-CLASS must be a non-nil symbol.", NULL);
	if (! symbolp(name))
		return fmte_(":METHOD-CLASS ~S must be a symbol.", name, NULL);
	if (tail != Nil)
		return fmte_(":METHOD-CLASS ~S must be a (symbol) form.", list, NULL);

	GetConst(COMMON_FIND_CLASS, &find);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&name, quote, name, NULL);
	list_heap(ret, find, name, NULL);

	return 0;
}

static void defgeneric_parse_form(addr root, addr name, addr list, addr *ret)
{
	addr pos;

	GetConst(COMMON_DEFMETHOD, &pos);
	lista_heap(&pos, pos, name, list, NULL);
	cons_heap(ret, pos, root);
}

static int defgeneric_parse_options_(addr name, addr args,
		addr *rorder, addr *rdecl, addr *rdoc, addr *rcomb,
		addr *rgen, addr *rmethod, addr *rcode)
{
	addr pos, tail, type, check;
	addr order, decl, doc, comb, gen, method, code;

	decl = doc = comb = gen = method = Unbound;
	order = code = Nil;
	while (args != Nil) {
		if (! consp_getcons(args, &pos, &args))
			return fmte_("Invalid defgeneric argument ~S.", args, NULL);
		if (! consp_getcons(pos, &type, &tail))
			return fmte_("Invalid defgeneric argument ~S.", pos, NULL);

		/* :argument-precedence-order */
		GetConst(KEYWORD_ARGUMENT_PRECEDENCE_ORDER, &check);
		if (type == check) {
			Return(defgeneric_parse_order_(tail, &order));
			continue;
		}
		/* :declare */
		GetConst(COMMON_DECLARE, &check);
		if (type == check) {
			Return(defgeneric_parse_declare_(tail, &decl));
			continue;
		}
		/* :documentation */
		GetConst(KEYWORD_DOCUMENTATION, &check);
		if (type == check) {
			Return(defgeneric_parse_document_(tail, &doc));
			continue;
		}
		/* :method-combination */
		GetConst(KEYWORD_METHOD_COMBINATION, &check);
		if (type == check) {
			Return(defgeneric_parse_method_combination_(tail, &comb));
			continue;
		}
		/* :generic-function-class */
		GetConst(KEYWORD_GENERIC_FUNCTION_CLASS, &check);
		if (type == check) {
			Return(defgeneric_parse_generic_(tail, &gen));
			continue;
		}
		/* :method-class */
		GetConst(KEYWORD_METHOD_CLASS, &check);
		if (type == check) {
			Return(defgeneric_parse_method_(tail, &method));
			continue;
		}
		/* :method */
		GetConst(KEYWORD_METHOD, &check);
		if (type == check) {
			defgeneric_parse_form(code, name, tail, &code);
			continue;
		}
		/* error */
		return call_simple_program_error_va_(NULL,
				"Invalid defgeneric option ~S.", pos, NULL);
	}
	*rorder = order;
	*rdecl = decl;
	*rdoc = doc;
	*rcomb = comb;
	*rgen = gen;
	*rmethod = method;
	*rcode = code;
	return 0;
}

static void defgeneric_push_value(addr *ret, addr key, addr value, addr root)
{
	cons_heap(&root, key, root);
	cons_heap(ret, value, root);
}

static void defgeneric_push_quote(addr *ret, addr key, addr value, addr root)
{
	addr quote;

	GetConst(COMMON_QUOTE, &quote);
	list_heap(&value, quote, value, NULL);
	defgeneric_push_value(ret, key, value, root);
}

static int defgeneric_check_function_(addr name)
{
	addr check;

	/* name error */
	if (! function_name_p(name))
		return fmte_("Invalid function name ~S.", name, NULL);

	/* symbol */
	if (symbolp(name)) {
		GetFunctionSymbol(name, &check);
		if (check != Unbound && (! closp(check))) {
			return call_simple_program_error_va_(NULL,
					"Cannot update the function because "
					"~S is not a generic-function.", name, NULL);
		}
	}

	/* setf */
	if (consp(name)) {
		Return(parse_callname_error_(&check, name));
		getglobal_callname(check, &check);
		if (check != Unbound && (! closp(check))) {
			return call_simple_program_error_va_(NULL,
					"Cannot update the function because "
					"~S is not a generic-function.", name, NULL);
		}
		return 0;
	}

	/* macro */
	getmacro_symbol(name, &check);
	if (check != Unbound) {
		return call_simple_program_error_va_(NULL,
				"Cannot make the generic-function because "
				"~S is a macro-function.", name, NULL);
	}

	/* special-operator */
	if (get_special_operator(name)) {
		return call_simple_program_error_va_(NULL,
				"Cannot make the generic-function because "
				"~S is a special operator.", name, NULL);
	}

	return 0;
}

static int defgeneric_method_common_(Execute ptr,
		addr name, addr ensure, addr code, addr *ret)
{
	/*  (let ((g (defgeneric-define ...)))
	 *    (system::defgeneric-method g (defmethod ...))
	 *    (system::defgeneric-method g (defmethod ...))
	 *    g)
	 */
	addr g, let, method, args, list, pos;

	GetConst(COMMON_LET, &let);
	GetConst(SYSTEM_DEFGENERIC_METHOD, &method);
	Return(make_gensym_(ptr, &g));

	/* let */
	list_heap(&args, g, ensure, NULL);
	list_heap(&args, args, NULL);

	/* list */
	list = Nil;
	cons_heap(&list, let, list);
	cons_heap(&list, args, list);
	while (code != Nil) {
		GetCons(code, &pos, &code);
		list_heap(&pos, method, g, pos, NULL);
		cons_heap(&list, pos, list);
	}
	cons_heap(&list, g, list);
	nreverse(ret, list);

	return 0;
}

int defgeneric_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, name, lambda, order, decl, doc, comb, gen, method, code, key;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &name, &args))
		goto error;
	if (! consp_getcons(args, &lambda, &args))
		goto error;
	/* options */
	order = decl = doc = comb = gen = method = code = Nil;
	Return(defgeneric_parse_options_(name, args,
				&order, &decl, &doc, &comb, &gen, &method, &code));
	/* check */
	Return(defgeneric_check_function_(name));
	/* expand */
	args = Nil;
	GetConst(SYSTEM_DEFGENERIC_DEFINE, &key);
	defgeneric_push_quote(&args, key, name, args);
	GetConst(KEYWORD_LAMBDA_LIST, &key);
	defgeneric_push_quote(&args, key, lambda, args);
	if (order != Nil) {
		GetConst(KEYWORD_ARGUMENT_PRECEDENCE_ORDER, &key);
		defgeneric_push_quote(&args, key, order, args);
	}
	if (decl != Unbound) {
		GetConst(KEYWORD_DECLARE, &key);
		defgeneric_push_quote(&args, key, decl, args);
	}
	if (doc != Unbound) {
		GetConst(KEYWORD_DOCUMENTATION, &key);
		defgeneric_push_value(&args, key, doc, args);
	}
	if (comb != Unbound) {
		GetConst(KEYWORD_METHOD_COMBINATION, &key);
		defgeneric_push_quote(&args, key, comb, args);
	}
	if (gen != Unbound) {
		GetConst(KEYWORD_GENERIC_FUNCTION_CLASS, &key);
		defgeneric_push_value(&args, key, gen, args);
	}
	if (method != Unbound) {
		GetConst(KEYWORD_METHOD_CLASS, &key);
		defgeneric_push_value(&args, key, method, args);
	}
	nreverse(&args, args);
	if (code != Nil) {
		Return(defgeneric_method_common_(ptr, name, args, code, &args));
	}
	return Result(ret, args);

error:
	*ret = Nil;
	return fmte_("DEFGENERIC argument ~S must be a "
			"(name lambda-list &rest args) form.", form, NULL);
}


/*
 *  defmethod
 */
static int defmethod_parse_qualifiers(addr list, addr *qua, addr *args, addr *body)
{
	addr root, lambda;

	root = Nil;
	for (;;) {
		if (! consp(list))
			return 1;
		GetCons(list, &lambda, &list);
		if (listp(lambda))
			break;
		cons_heap(&root, lambda, root);
	}
	nreverse(qua, root);
	*args = lambda;
	*body = list;

	return 0;
}

static int defmethod_parse_specializers_(addr pos, addr *ret)
{
	addr list, var, type, root, quote;

	GetArgument(pos, ArgumentIndex_var, &pos);
	GetConst(COMMON_QUOTE, &quote);
	for (root = Nil; pos != Nil; ) {
		GetCons(pos, &list, &pos);
		Return(list_bind_(list, &var, &type, NULL));
		if (consp(type)) {
			Return_getcdr(type, &type);
			Return_getcar(type, &type);
			/* (intern-eql-specializer (quote value)) */
			GetConst(SYSTEM_INTERN_EQL_SPECIALIZER, &var);
		}
		else {
			/* (find-class (quote value)) */
			GetConst(COMMON_FIND_CLASS, &var);
		}
		list_heap(&type, quote, type, NULL);
		list_heap(&type, var, type, NULL);
		cons_heap(&root, type, root);
	}
	nreverse(&root, root);
	/* result */
	if (root != Nil) {
		GetConst(COMMON_LIST, &list);
		cons_heap(&root, list, root);
	}

	return Result(ret, root);
}

static int defmethod_parse_function_lambda_(addr name, addr ord, addr form,
		addr *ret, addr *rdoc)
{
	addr block, lambda, declare, doc, decl;

	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_DECLARE, &declare);

	/* block */
	Return(parse_callname_error_(&name, name));
	GetCallName(name, &name);
	Return(split_decl_body_doc_(form, &doc, &decl, &form));
	lista_heap(&block, block, name, form, NULL);
	/* lambda */
	Return(argument_method_lambda_heap_(&ord, ord));
	cons_heap(&decl, declare, decl);
	list_heap(ret, lambda, ord, decl, block, NULL);
	return Result(rdoc, doc);
}

static int defmethod_parse_function_(Execute ptr,
		addr name, addr env, addr form, addr ord, addr *ret)
{
	/* `(lambda (,method ,next &rest ,args)
	 *    (flet ((next-method-p ()
	 *             (clos::flet-method-p ,next))
	 *           (call-next-method (&rest ,rest)
	 *             (clos::flet-next-method ,method ,next ,args ,rest)))
	 *      (declare (ignorable #'next-method-p #'call-next-method))
	 *      "Documentation"
	 *      (apply (lambda ,lambda-list
	 *                ,@declare
	 *                (block ,name ,@form))
	 *             ,args)))
	 */
	addr lambda, apply, next1, next2, call1, call2, a, b, c, doc;
	addr method, next, args, rest, ignorable, declare, arest, flet;

	/* gensym */
	Return(make_gensym_(ptr, &method));
	Return(make_gensym_(ptr, &next));
	Return(make_gensym_(ptr, &args));
	Return(make_gensym_(ptr, &rest));
	/* constant */
	GetConst(COMMON_NEXT_METHOD_P, &next1);
	GetConst(CLOSNAME_FLET_METHOD_P, &next2);
	GetConst(COMMON_CALL_NEXT_METHOD, &call1);
	GetConst(CLOSNAME_FLET_NEXT_METHOD, &call2);
	/* apply */
	GetConst(COMMON_APPLY, &apply);
	Return(defmethod_parse_function_lambda_(name, ord, form, &form, &doc));
	list_heap(&apply, apply, form, args, NULL);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_FUNCTION, &a);
	list_heap(&b, a, next1, NULL);
	list_heap(&c, a, call1, NULL);
	list_heap(&ignorable, ignorable, b, c, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	/* next-method-p */
	list_heap(&next2, next2, next, NULL);
	list_heap(&next1, next1, Nil, next2, NULL);
	/* call-next-method */
	list_heap(&call2, call2, method, next, args, rest, NULL);
	GetConst(AMPERSAND_REST, &arest);
	list_heap(&rest, arest, rest, NULL);
	list_heap(&call1, call1, rest, call2, NULL);
	/* flet */
	list_heap(&next1, next1, call1, NULL);
	GetConst(COMMON_FLET, &flet);
	list_heap(&flet, flet, next1, declare, apply, NULL);
	/* lambda */
	GetConst(COMMON_LAMBDA, &lambda);
	list_heap(&method, method, next, arest, args, NULL);
	list_heap(ret, lambda, method, doc, flet, NULL);

	return 0;
}

int defmethod_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, name, qua, spec, lambda, list, quote;
	addr key1, key2, key3, key4, key5;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &name, &args))
		goto error;
	if (! function_name_p(name))
		return fmte_("Invalid function name ~S.", name, NULL);
	if (defmethod_parse_qualifiers(args, &qua, &lambda, &args))
		goto error;
	Return(argument_method_heap_(ptr->local, &list, lambda));
	Return(defmethod_parse_specializers_(list, &spec));
	Return(defmethod_parse_function_(ptr, name, env, args, list, &args));

	/* name qua lambda doc decl args */
	GetConst(COMMON_QUOTE, &quote);
	GetConst(CLOSNAME_ENSURE_METHOD, &key1);
	GetConst(CLOSKEY_LAMBDA_LIST, &key2);
	GetConst(CLOSKEY_QUALIFIERS, &key3);
	GetConst(CLOSKEY_SPECIALIZERS, &key4);
	GetConst(CLOSKEY_FUNCTION, &key5);
	list_heap(&name, quote, name, NULL);
	list_heap(&lambda, quote, lambda, NULL);
	if (qua != Nil)
		list_heap(&qua, quote, qua, NULL);
	list_heap(ret, key1, name, key2, lambda, key3, qua, key4, spec, key5, args, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("DEFMETHOD argument ~S must be a "
			"(name qualifier* lambda-list &body form).", form, NULL);
}


/*
 *  define-method-combination
 */
static int defcomb_short_(addr *ret, addr list, addr name)
{
	addr doc, ident, oper, key, value;
	addr kdoc, kident, koper, quote;

	/* parse */
	GetConst(KEYWORD_DOCUMENTATION, &kdoc);
	GetConst(KEYWORD_IDENTITY_WITH_ONE_ARGUMENT, &kident);
	GetConst(KEYWORD_OPERATOR, &koper);
	doc = ident = oper = Unbound;
	while (list != Nil) {
		Return_getcons(list, &key, &list);
		Return_getcons(list, &value, &list);
		if (key == kdoc) {
			if (! stringp(value))
				return fmte_(":DOCUMENTATION ~S must be a symbol.", value, NULL);
			doc = value;
			continue;
		}
		if (key == kident) {
			ident = (value != Nil)? T: Nil;
			continue;
		}
		if (key == koper) {
			if (! symbolp(value))
				return fmte_(":OPERATOR ~S must be a symbol.", value, NULL);
			oper = value;
			continue;
		}
		return fmte_("Invalid argument ~S ~S.", key, value, NULL);
	}

	/* `(ensure-method-combination-short
	 *    (quote ,name)
	 *    :documentation ,doc
	 *    :identity-with-one-argument ,ident
	 *    :operator (quote ,oper))
	 */
	GetConst(CLOSNAME_ENSURE_METHOD_COMBINATION_SHORT, &key);
	conscar_heap(&list, key);
	GetConst(COMMON_QUOTE, &quote);
	quotelist_heap(&name, name);
	cons_heap(&list, name, list);
	/* documentation */
	if (doc != Unbound)
		pushva_heap(&list, kdoc, doc, NULL);
	/* identity-with-one-argument */
	if (ident != Unbound)
		pushva_heap(&list, kident, ident, NULL);
	/* operator */
	if (oper != Unbound) {
		quotelist_heap(&oper, oper);
		pushva_heap(&list, koper, oper, NULL);
	}
	/* result */
	nreverse(ret, list);
	return 0;
}

static int defcomb_split_body_(addr list, addr *rargs, addr *rgen, addr *rbody)
{
	addr next, args, gen, a, b, c, kargs, kgen;

	GetConst(KEYWORD_ARGUMENTS, &kargs);
	GetConst(KEYWORD_GENERIC_FUNCTION, &kgen);
	gen = Unbound;
	args = Nil;
	while (list != Nil) {
		Return_getcons(list, &a, &next);
		if (! consp_getcons(a, &a, &b))
			break;
		/* (:arguments . args) */
		if (a == kargs) {
			if (args == Nil)
				args = b;
			list = next;
			continue;
		}
		/* (:generic-function gen) */
		if (a == kgen) {
			if (! consp(b))
				return fmte_(":GENERIC-FUNCTION ~S must be a cons form.", b, NULL);
			GetCons(b, &a, &c);
			if (c != Nil)
				return fmte_("Invalid :GENERIC-FUNCTION form ~S.", b, NULL);
			if (! symbolp(a))
				return fmte_(":GENERIC-FUNCTION ~S must be a symbol.", a, NULL);
			if (gen == Unbound)
				gen = a;
			list = next;
			continue;
		}
		/* form */
		break;
	}
	/* result */
	*rargs = args;
	*rgen = gen;
	*rbody = list;
	return 0;
}

static int defcomb_long_specifiers_(addr *ret, addr list)
{
	addr root, name, spec, tail, key, value, check, order, req, desc;

	if (! listp(list)) {
		return fmte_("DEFINE-METHOD-COMBINATION specifiers ~S "
				"must be a list.", list, NULL);
	}
	for (root = Nil; list != Nil; ) {
		/* (name spec &key order required description) */
		Return_getcons(list, &spec, &list);
		Return_getcons(spec, &name, &spec);
		Return_getcons(spec, &spec, &tail);
		if ((! symbolp(spec)) && (! listp(spec))) {
			return fmte_("The qualifiers pattern ~S "
					"must be a symbol or list.", spec, NULL);
		}
		/* &key */
		order = req = desc = Unbound;
		while (tail != Nil) {
			Return_getcons(tail, &key, &tail);
			Return_getcons(tail, &value, &tail);
			/* order */
			GetConst(KEYWORD_ORDER, &check);
			if (key == check) {
				if (order == Unbound)
					order = value;
				continue;
			}
			/* required */
			GetConst(KEYWORD_REQUIRED, &check);
			if (key == check) {
				if (req == Unbound)
					req = value;
				continue;
			}
			/* desc */
			GetConst(KEYWORD_DESCRIPTION, &check);
			if (key == check) {
				if (desc == Unbound)
					desc = value;
				continue;
			}
			/* error */
			return fmte_("Invalid specifiers keyword ~S.", key, NULL);
		}
		if (order == Unbound)
			GetConst(KEYWORD_MOST_SPECIFIC_FIRST, &order);
		if (req == Unbound)
			req = Nil;
		if (desc == Unbound)
			desc = Nil;
		list_heap(&name, name, spec, order, req, desc, NULL);
		cons_heap(&root, name, root);
	}
	nreverse(ret, root);
	return 0;
}

static int defcomb_arguments_(LocalRoot local, addr *ret, addr args)
{
	addr pos;
	struct argument_struct *str;

	Return(argument_combination_heap_(local, &args, args));
	str = ArgumentStruct(args);
	if (str->rest == 0) {
		GetArgument(args, ArgumentIndex_rest, &pos);
		if (pos == Nil) {
			make_symbolchar(&pos, "IGNORE");
			SetArgument(args, ArgumentIndex_rest, pos);
		}
		str->rest = 1;
	}

	return Result(ret, args);
}

static int defcomb_long_(LocalRoot local, addr form, addr env, addr *ret,
		addr list, addr name)
{
	addr pos, lambda, spec, args, gen, doc, body, decl;

	/* long form */
	GetCons(list, &lambda, &list);
	if (! consp_getcons(list, &spec, &list))
		goto error;
	args = gen = Nil;
	Return(defcomb_split_body_(list, &args, &gen, &list));
	/* parser */
	Return(defcomb_long_specifiers_(&spec, spec));
	Return(split_decl_body_doc_(list, &doc, &decl, &body));

	/* `(ensure-method-combination-long
	 *    (quote ,name)
	 *    (quote ,lambda)
	 *    (quote ,spec)
	 *    :arguments (quote ,args)
	 *    :generic-function (quote ,gen)
	 *    :documentation ,doc
	 *    :form (lambda ...)
	 */
	list = Nil;
	PushConst(&list, CLOSNAME_ENSURE_METHOD_COMBINATION_LONG);
	/* name */
	quotelist_heap(&pos, name);
	pushva_heap(&list, pos, NULL);
	/* lambda-list */
	Return(argument_ordinary_heap_(local, &pos, lambda));
	quotelist_heap(&pos, pos);
	pushva_heap(&list, pos, NULL);
	/* specifiers */
	quotelist_heap(&pos, spec);
	pushva_heap(&list, pos, NULL);
	/* arguments */
	if (args != Nil) {
		Return(defcomb_arguments_(local, &args, args));
		PushConst(&list, KEYWORD_ARGUMENTS);
		quotelist_heap(&pos, args);
		pushva_heap(&list, pos, NULL);
	}
	/* generic-function */
	if (gen != Unbound) {
		PushConst(&list, KEYWORD_GENERIC_FUNCTION);
		quotelist_heap(&pos, gen);
		pushva_heap(&list, pos, NULL);
	}
	/* documentation */
	if (doc != Nil) {
		PushConst(&list, KEYWORD_DOCUMENTATION);
		pushva_heap(&list, doc, NULL);
	}
	/* body */
	PushConst(&list, CLOSKEY_FORM);
	Return(comb_longmacro_(&body, lambda, spec, args, gen, decl, body));
	pushva_heap(&list, body, NULL);
	/* result */
	nreverse(ret, list);
	return 0;

error:
	*ret = Nil;
	return fmte_("Invalid DEFINE-METHOD-COMBINATION form ~S.", form, NULL);
}

int define_method_combination_common_(LocalRoot local,
		addr form, addr env, addr *ret)
{
	addr list, name, check;

	/* arguments */
	Return_getcdr(form, &list);
	if (! consp_getcons(list, &name, &list))
		goto error;
	if (! symbolp(name))
		return fmte_("DEFINE-METHOD-COMBINATION name ~S must be a symbol.", name, NULL);
	if (list == Nil)
		return defcomb_short_(ret, list, name);
	if (! consp_getcar(list, &check))
		goto error;
	if (keywordp(check))
		return defcomb_short_(ret, list, name);
	return defcomb_long_(local, form, env, ret, list, name);

error:
	*ret = Nil;
	return fmte_("Invalid DEFINE-METHOD-COMBINATION form ~S.", form, NULL);
}


/*
 *  make-load-form-saving-slots
 */
static void make_load_form_saving_slots_list(addr var, addr *ret)
{
	addr vector, list, x;
	size_t size, i;

	GetSlotClos(var, &vector);
	LenSlotVector(vector, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		GetSlotVector(vector, i, &x);
		GetNameSlot(x, &x);
		cons_heap(&list, x, list);
	}
	nreverse(ret, list);
}

int make_load_form_saving_slots_common_(Execute ptr,
		addr var, addr list, addr env, addr *ret1, addr *ret2)
{
	/* (allocate-instance
	 *   (find-class
	 *     (quote class-name)))
	 * (lisp-system::set-slots var
	 *   (quote (slot1  slot2  ...))
	 *   (quote (value1 value2 ...)))
	 */
	addr alloc, find, call, name;
	addr set, root, values, x, y;

	/* type check */
	if (! closp(var)) {
		*ret1 = *ret2 = Nil;
		return fmte_("The object ~S must be a clos-object.", var, NULL);
	}

	/* first */
	GetConst(COMMON_ALLOCATE_INSTANCE, &alloc);
	GetConst(COMMON_FIND_CLASS, &find);
	GetConst(COMMON_CLASS_NAME, &call);
	Return(clos_class_of_(var, &name));
	Return(getfunction_global_(call, &call));
	Return(funcall1_control_(ptr, &name, call, name, NULL));
	quotelist_heap(&name, name);
	list_heap(&find, find, name, NULL);
	list_heap(&alloc, alloc, find, NULL);

	/* second */
	GetConst(SYSTEM_SET_SLOTS, &set);
	if (list == Unbound)
		make_load_form_saving_slots_list(var, &list);
	values = Nil;
	root = list;
	while (root != Nil) {
		Return_getcons(root, &x, &root);
		Return(clos_get_(var, x, &y));
		if (y == Unbound)
			GetConst(SYSTEM_UNBOUND_VALUE, &y);
		cons_heap(&values, y, values);
	}
	nreverse(&values, values);
	quotelist_heap(&list, list);
	quotelist_heap(&values, values);
	list_heap(&set, set, var, list, values, NULL);

	/* result */
	*ret1 = alloc;
	*ret2 = set;
	return 0;
}

int set_slots_syscall_(addr var, addr slots, addr values)
{
	addr x, y, unbound;

	GetConst(SYSTEM_UNBOUND_VALUE, &unbound);
	while (slots != Nil || values != Nil) {
		Return_getcons(slots, &x, &slots);
		Return_getcons(values, &y, &values);
		if (y == unbound)
			y = Unbound;
		Return(clos_set_(var, x, y));
	}

	return 0;
}

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#ifdef __clang__
#pragma clang diagnostic pop
#endif
