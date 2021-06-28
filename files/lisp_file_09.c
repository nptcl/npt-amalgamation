/*
 *  npt -- ANSI Common Lisp Programming Language.
 *    https://github.com/nptcl/npt
 *    https://github.com/nptcl/npt-amalgamation
 *
 *  File: lisp_file_09.c
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
#include <signal.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lisp_file.h"


/************************************************************
 *  subtypep_range.c
 ************************************************************/

/* (integetr * *) */
int range_asterisk_p(addr type)
{
	addr check;

	GetArrayType(type, 0, &check);
	if (! type_asterisk_p(check))
		return 0;
	GetArrayType(type, 2, &check);

	return type_asterisk_p(check);
}

/* (integer 10 *) */
int range_left_p(addr type)
{
	addr check;

	GetArrayType(type, 0, &check);
	if (type_asterisk_p(check))
		return 0;
	GetArrayType(type, 2, &check);

	return type_asterisk_p(check);
}

/* (integer 10 ?) */
int range_left_any_p(addr type)
{
	addr check;
	GetArrayType(type, 0, &check);
	return ! type_asterisk_p(check);
}

/* (integer * 10) */
int range_right_p(addr type)
{
	addr check;

	GetArrayType(type, 0, &check);
	if (! type_asterisk_p(check))
		return 0;
	GetArrayType(type, 2, &check);

	return ! type_asterisk_p(check);
}

/* (integer ? 10) */
int range_any_right_p(addr type)
{
	addr check;
	GetArrayType(type, 2, &check);
	return ! type_asterisk_p(check);
}

/* (integer 10 20) */
int range_between_p(addr type)
{
	addr check;

	GetArrayType(type, 0, &check);
	if (type_asterisk_p(check))
		return 0;
	GetArrayType(type, 2, &check);

	return ! type_asterisk_p(check);
}

void range_left_value(addr value, addr *left1, addr *left2)
{
	GetArrayType(value, 0, left1);
	GetArrayType(value, 1, left2);
}

void range_right_value(addr value, addr *right1, addr *right2)
{
	GetArrayType(value, 2, right1);
	GetArrayType(value, 3, right2);
}

/*
 * ( 10  *) ( 10  *) -> nil
 * ((10) *) ( 10  *) -> nil
 * ( 10  *) ((10) *) -> t
 * ((10) *) ((10) *) -> nil
 */
int range_left_left_less_(addr left, addr right, int *ret)
{
	addr left1, right1;

	range_left_value(left, &left1, &left);
	range_left_value(right, &right1, &right);
	if (left1 == Nil && right1 != Nil)
		return less_equal_real_(Local_Thread, left, right, ret);
	else
		return less_real_(Local_Thread, left, right, ret);
}

/*
 * ( 10  *) ( 10  *) -> t
 * ((10) *) ( 10  *) -> nil
 * ( 10  *) ((10) *) -> t
 * ((10) *) ((10) *) -> t
 */
int range_left_left_less_equal_(addr left, addr right, int *ret)
{
	addr left1, right1;

	range_left_value(left, &left1, &left);
	range_left_value(right, &right1, &right);
	if (left1 != Nil && right1 == Nil)
		return less_real_(Local_Thread, left, right, ret);
	else
		return less_equal_real_(Local_Thread, left, right, ret);
}

int range_left_left_greater_(addr left, addr right, int *ret)
{
	int check;
	Return(range_left_left_less_equal_(left, right, &check));
	return Result(ret, ! check);
}

int range_left_left_greater_equal_(addr left, addr right, int *ret)
{
	int check;
	Return(range_left_left_less_(left, right, &check));
	return Result(ret, ! check);
}

/*
 * ( 10  *) (*  10 ) -> nil
 * ((10) *) (*  10 ) -> nil
 * ( 10  *) (* (10)) -> nil
 * ((10) *) (* (10)) -> nil
 */
int range_left_right_less_(addr left, addr right, int *ret)
{
	/* range_left_value(left, &left1, &left); */
	GetArrayType(left, 1, &left);
	/* range_right_value(right, &right1, &right); */
	GetArrayType(right, 3, &right);
	return less_real_(Local_Thread, left, right, ret);
}

/*
 * ( 10  *) (*  10 ) -> t
 * ((10) *) (*  10 ) -> nil
 * ( 10  *) (* (10)) -> nil
 * ((10) *) (* (10)) -> nil
 */
int range_left_right_less_equal_(addr left, addr right, int *ret)
{
	addr left1, right1;

	range_left_value(left, &left1, &left);
	range_right_value(right, &right1, &right);
	if (left1 == Nil && right1 == Nil)
		return less_equal_real_(Local_Thread, left, right, ret);
	else
		return less_real_(Local_Thread, left, right, ret);
}

int range_left_right_greater_(addr left, addr right, int *ret)
{
	int check;
	Return(range_left_right_less_equal_(left, right, &check));
	return Result(ret, ! check);
}

int range_left_right_greater_equal_(addr left, addr right, int *ret)
{
	int check;
	Return(range_left_right_less_(left, right, &check));
	return Result(ret, ! check);
}

/*
 * (*  10 ) ( 10  *) -> nil
 * (* (10)) ( 10  *) -> t
 * (*  10 ) ((10) *) -> t
 * (* (10)) ((10) *) -> t
 */
int range_right_left_less_(addr left, addr right, int *ret)
{
	addr left1, right1;

	range_right_value(left, &left1, &left);
	range_left_value(right, &right1, &right);
	if (left1 == Nil && right1 == Nil)
		return less_real_(Local_Thread, left, right, ret);
	else
		return less_equal_real_(Local_Thread, left, right, ret);
}

/*
 * (*  10 ) ( 10  *) -> t
 * (* (10)) ( 10  *) -> t
 * (*  10 ) ((10) *) -> t
 * (* (10)) ((10) *) -> t
 */
int range_right_left_less_equal_(addr left, addr right, int *ret)
{
	/* range_right_value(left, &left1, &left); */
	GetArrayType(left, 3, &left);
	/* range_left_value(right, &right1, &right); */
	GetArrayType(right, 1, &right);
	return less_equal_real_(Local_Thread, left, right, ret);
}

int range_right_left_greater_(addr left, addr right, int *ret)
{
	int check;
	Return(range_right_left_less_equal_(left, right, &check));
	return Result(ret, ! check);
}

int range_right_left_greater_equal_(addr left, addr right, int *ret)
{
	int check;
	Return(range_right_left_less_(left, right, &check));
	return Result(ret, ! check);
}

/*
 * (*  10 ) (*  10 ) -> nil
 * (* (10)) (*  10 ) -> t
 * (*  10 ) (* (10)) -> nil
 * (* (10)) (* (10)) -> nil
 */
int range_right_right_less_(addr left, addr right, int *ret)
{
	addr left1, right1;

	range_right_value(left, &left1, &left);
	range_right_value(right, &right1, &right);
	if (left1 != Nil && right1 == Nil)
		return less_equal_real_(Local_Thread, left, right, ret);
	else
		return less_real_(Local_Thread, left, right, ret);
}

/*
 * (*  10 ) (*  10 ) -> t
 * (* (10)) (*  10 ) -> t
 * (*  10 ) (* (10)) -> nil
 * (* (10)) (* (10)) -> t
 */
int range_right_right_less_equal_(addr left, addr right, int *ret)
{
	addr left1, right1;

	range_right_value(left, &left1, &left);
	range_right_value(right, &right1, &right);
	if (left1 == Nil && right1 != Nil)
		return less_real_(Local_Thread, left, right, ret);
	else
		return less_equal_real_(Local_Thread, left, right, ret);
}

int range_right_right_greater_(addr left, addr right, int *ret)
{
	int check;
	Return(range_right_right_less_equal_(left, right, &check));
	return Result(ret, ! check);
}

int range_right_right_greater_equal_(addr left, addr right, int *ret)
{
	int check;
	Return(range_right_right_less_(left, right, &check));
	return Result(ret, ! check);
}

/* (10 30) (20 *) -> t */
	static int range_and2_call_(
			int (*call1)(addr, addr, int *), addr a, addr b,
			int (*call2)(addr, addr, int *), addr c, addr d,
			int *ret)
{
	int check;

	Return((*call1)(a, b, &check));
	if (! check)
		return Result(ret, 0);
	return (*call2)(c, d, ret);
}

int range_between_left_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_left_left_less_equal_, left, right,
			range_left_right_less_equal_, right, left, ret);
}

/* (20 *) (10 30) -> t */
int range_left_between_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_left_left_less_equal_, right, left,
			range_left_right_less_equal_, left, right, ret);
}

/* (10 30) (* 20) -> t */
int range_between_right_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_left_right_less_equal_, left, right,
			range_right_right_less_equal_, right, left, ret);
}

/* (* 20) (10 30) -> t */
int range_right_between_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_left_right_less_equal_, right, left,
			range_right_right_less_equal_, left, right, ret);
}

/* (10 30) (11 12) -> t */
int range_between_in_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_between_left_, left, right,
			range_between_right_, left, right, ret);
}

/* (11 12) (10 30) -> t */
int range_in_between_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_left_between_, left, right,
			range_right_between_, left, right, ret);
}

/*
 * (?  10 ) ( 10  ?) -> t
 * (? (10)) ( 10  ?) -> t
 * (?  10 ) ((10) ?) -> t
 * (? (10)) ((10) ?) -> nil
 */
int range_connect_right_left_(addr left, addr right, int *ret)
{
	addr left1, right1;

	range_right_value(left, &left1, &left);
	range_left_value(right, &right1, &right);
	if (left1 != Nil && right1 != Nil)
		return less_real_(Local_Thread, right, left, ret);
	else
		return less_equal_real_(Local_Thread, right, left, ret);
}

/* (10 20) (20 *) */
int range_connect_between_left_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_left_left_less_equal_, left, right,
			range_connect_right_left_, left, right, ret);
}

/* (10 20) (* 10) */
int range_connect_between_right_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_right_right_less_equal_, right, left,
			range_connect_right_left_, right, left, ret);
}


/************************************************************
 *  subtypep_table.c
 ************************************************************/

static call_type_subtypep TypeSubtypep[LISPDECL_SIZE];

/*
 *  call error
 */
static int subtypep_call_error_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	infoprint(x);
	infoprint(y);
	*ret = SUBTYPEP_INVALID;
	return fmte_("Invalid subtypep argument.", NULL);
}


/*
 *  call cons
 */
static int subtypep_call_cons_t_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	if (type_astert_p(y))
		return ReturnInclude(ret);
	if (type_astert_p(x))
		return ReturnFalse(ret);

	return subtypep_compound_(ptr, x, y, ret);
}

static int subtypep_call_cons_value_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult value1, value2;
	addr car1, car2, cdr1, cdr2;

	GetArrayType(x, 0, &car1);
	GetArrayType(y, 0, &car2);
	GetArrayType(x, 1, &cdr1);
	GetArrayType(y, 1, &cdr2);

	/* subtypep */
	Return(subtypep_call_cons_t_(ptr, car1, car2, &value1));
	ReturnSecondThrow(ret, value1);
	Return(subtypep_call_cons_t_(ptr, cdr1, cdr2, &value2));
	ReturnSecondThrow(ret, value2);

	/* include */
	if (value1 == SUBTYPEP_INCLUDE && value2 == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* reverse */
	if (value1 == SUBTYPEP_INCLUDE) {
		Return(subtypep_call_cons_t_(ptr, car2, car1, &value1));
		return ReturnSecondValue(ret, value1);
	}
	if (value2 == SUBTYPEP_INCLUDE) {
		Return(subtypep_call_cons_t_(ptr, cdr2, cdr1, &value2));
		return ReturnSecondValue(ret, value2);
	}

	return ReturnFalse(ret);
}

static int subtypep_call_cons_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_CONS:
			return subtypep_call_cons_value_(ptr, x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  call complex
 */
static int subtypep_complex_value_(addr x, addr y, SubtypepResult *ret)
{
	int check1, check2;

	GetArrayType(x, 0, &x);
	GetArrayType(y, 0, &y);
	if (type_astert_p(y))
		return ReturnInclude(ret);
	if (type_astert_p(x))
		return ReturnFalse(ret);
	check1 = (RefLispDecl(x) == RefLispDecl(y));
	check2 = (RefNotDecl(x) == RefNotDecl(y));

	return ReturnIncludeExclude(ret, check1 && check2);
}

static int subtypep_call_complex_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_COMPLEX:
			return subtypep_complex_value_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  call function
 */
static int subtypep_ordinary_subtypep_(Execute ptr,
		const ordargs *ptr1, const ordtype *type1,
		const ordargs *ptr2, const ordtype *type2,
		SubtypepResult *ret)
{
	SubtypepResult value;
	addr x, y;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	merge_ordargs(local, &x, ptr1, type1);
	merge_ordargs(local, &y, ptr2, type2);
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);
	Return(subtypep_compound_(ptr, x, y, &value));
	rollback_local(local, stack);

	return Result(ret, value);
}

static int subtypep_ordinary_size_(Execute ptr,
		const ordargs *ptr1, const ordargs *ptr2,
		size_t size, SubtypepResult *ret)
{
	SubtypepResult value;
	size_t i;
	ordtype type1, type2;

	for (i = 0; i < size; i++) {
		Return(gettype_ordargs_(ptr1, i, &type1));
		Return(gettype_ordargs_(ptr2, i, &type2));
		if (type1.nil)
			break;
		if (type2.nil)
			return ReturnExclude(ret);
		Return(subtypep_ordinary_subtypep_(ptr, ptr1, &type1, ptr2, &type2, &value));
		if (value != SUBTYPEP_INCLUDE)
			return Result(ret, value);
	}

	return ReturnInclude(ret);
}

static int subtypep_ordinary_simple_(Execute ptr,
		const ordargs *ptr1, const ordargs *ptr2, SubtypepResult *ret)
{
	if (ptr1->size > ptr2->size)
		return ReturnExclude(ret);
	/* short size */
	return subtypep_ordinary_size_(ptr, ptr1, ptr2, ptr1->size, ret);
}

static int subtypep_ordinary_simple_left_(Execute ptr,
		const ordargs *ptr1, const ordargs *ptr2, SubtypepResult *ret)
{
	/* short size */
	return subtypep_ordinary_size_(ptr, ptr1, ptr2, ptr1->size, ret);
}

static int subtypep_ordinary_check_(Execute ptr,
		const ordargs *ptr1, const ordargs *ptr2, SubtypepResult *ret)
{
	/* long size */
	return subtypep_ordinary_size_(ptr, ptr1, ptr2,
			(ptr1->size > ptr2->size? ptr1->size: ptr2->size),
			ret);
}

static int subtypep_function_ordinary_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check1, check2;
	ordargs ptr1, ptr2;

	/* asterisk */
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);

	/* list */
	make_ordargs(&ptr1, x);
	make_ordargs(&ptr2, y);
	if (ptr1.size_var < ptr2.size_var)
		return ReturnExclude(ret);

	check1 = simple_p_ordargs(&ptr1);
	check2 = simple_p_ordargs(&ptr2);
	if (check1 && check2)
		return subtypep_ordinary_simple_(ptr, &ptr1, &ptr2, ret);
	if (check1)
		return subtypep_ordinary_simple_left_(ptr, &ptr1, &ptr2, ret);
	if (check2)
		return ReturnExclude(ret);
	else
		return subtypep_ordinary_check_(ptr, &ptr1, &ptr2, ret);
}

static int subtypep_function_values_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	/* asterisk */
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);

	/* type check */
	return subtypep_compound_(ptr, x, y, ret);
}

static int subtypep_function_check_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult value1, value2;
	addr arg1, arg2, the1, the2;

	GetArrayType(x, 0, &arg1);
	GetArrayType(y, 0, &arg2);
	GetArrayType(x, 1, &the1);
	GetArrayType(y, 1, &the2);

	/* subtypep */
	Return(subtypep_function_ordinary_(ptr, arg1, arg2, &value1));
	ReturnSecondThrow(ret, value1);
	Return(subtypep_function_values_(ptr, the1, the2, &value2));
	ReturnSecondThrow(ret, value2);

	/* include */
	if (value1 == SUBTYPEP_INCLUDE && value2 == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* reverse */
	if (value1 == SUBTYPEP_INCLUDE) {
		Return(subtypep_function_ordinary_(ptr, arg2, arg1, &value1));
		return ReturnSecondValue(ret, value1);
	}
	if (value2 == SUBTYPEP_INCLUDE) {
		Return(subtypep_function_values_(ptr, the2, the1, &value2));
		return ReturnSecondValue(ret, value2);
	}

	return ReturnFalse(ret);
}

static int subtypep_call_function_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_FUNCTION:
		case LISPDECL_COMPILED_FUNCTION:
			return subtypep_function_check_(ptr, x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_call_compiled_function_function_(
		Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult check;
	Return(subtypep_function_check_(ptr, y, x, &check));
	return ReturnReverse(ret, check);
}

static int subtypep_call_compiled_function_(
		Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_FUNCTION:
			return subtypep_call_compiled_function_function_(ptr, x, y, ret);

		case LISPDECL_COMPILED_FUNCTION:
			return subtypep_function_check_(ptr, x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  table
 */
int subtypep_table_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	call_type_subtypep call;

	call = TypeSubtypep[(int)RefLispDecl(y)];
	if (call)
		return (*call)(ptr, x, y, ret);
	*ret = SUBTYPEP_INVALID;
	return 0;
}

void init_subtypep_table(void)
{
	int i;

	for (i = 0; i < LISPDECL_SIZE; i++)
		TypeSubtypep[i] = subtypep_call_error_;

	TypeSubtypep[LISPDECL_INVALID] = subtypep_call_invalid_;
	TypeSubtypep[LISPDECL_TYPE] = subtypep_call_error_;
	TypeSubtypep[LISPDECL_CLOS] = subtypep_call_clos_;
	TypeSubtypep[LISPDECL_ASTERISK] = subtypep_call_asterisk_;
	TypeSubtypep[LISPDECL_NIL] = subtypep_call_nil_;
	TypeSubtypep[LISPDECL_T] = subtypep_call_t_;
	TypeSubtypep[LISPDECL_NULL] = subtypep_call_null_;
	TypeSubtypep[LISPDECL_CONS] = subtypep_call_cons_;
	TypeSubtypep[LISPDECL_HASH_TABLE] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_SYMBOL] = subtypep_call_symbol_;
	TypeSubtypep[LISPDECL_KEYWORD] = subtypep_call_keyword_;
	TypeSubtypep[LISPDECL_PACKAGE] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_RANDOM_STATE] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_READTABLE] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_FUNCTION] = subtypep_call_function_;
	TypeSubtypep[LISPDECL_COMPILED_FUNCTION] = subtypep_call_compiled_function_;
	TypeSubtypep[LISPDECL_PATHNAME] = subtypep_call_pathname_;
	TypeSubtypep[LISPDECL_LOGICAL_PATHNAME] = subtypep_call_logical_pathname_;
	TypeSubtypep[LISPDECL_ARRAY] = subtypep_call_array_;
	TypeSubtypep[LISPDECL_SIMPLE_ARRAY] = subtypep_call_simple_array_;
	TypeSubtypep[LISPDECL_CHARACTER] = subtypep_call_character_;
	TypeSubtypep[LISPDECL_BASE_CHAR] = subtypep_call_base_char_;
	TypeSubtypep[LISPDECL_STANDARD_CHAR] = subtypep_call_standard_char_;
	TypeSubtypep[LISPDECL_INTEGER] = subtypep_call_integer_;
	TypeSubtypep[LISPDECL_RATIONAL] = subtypep_call_rational_;
	TypeSubtypep[LISPDECL_REAL] = subtypep_call_real_;
	TypeSubtypep[LISPDECL_NUMBER] = subtypep_call_number_;
	TypeSubtypep[LISPDECL_FLOAT] = subtypep_call_float_;
	TypeSubtypep[LISPDECL_SHORT_FLOAT] = subtypep_call_short_float_;
	TypeSubtypep[LISPDECL_SINGLE_FLOAT] = subtypep_call_single_float_;
	TypeSubtypep[LISPDECL_DOUBLE_FLOAT] = subtypep_call_double_float_;
	TypeSubtypep[LISPDECL_LONG_FLOAT] = subtypep_call_long_float_;
	TypeSubtypep[LISPDECL_RATIO] = subtypep_call_ratio_;
	TypeSubtypep[LISPDECL_COMPLEX] = subtypep_call_complex_;
	TypeSubtypep[LISPDECL_RESTART] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_ENVIRONMENT] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_STREAM] = subtypep_call_stream_;
	TypeSubtypep[LISPDECL_BROADCAST_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_CONCATENATED_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_ECHO_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_FILE_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_STRING_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_SYNONYM_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_TWO_WAY_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_PROMPT_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_PRETTY_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_MEMORY_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_BYTESPEC] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_PRINT_DISPATCH] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_EVAL] = subtypep_call_eqltype_;
}


/************************************************************
 *  sxhash.c
 ************************************************************/

/*
 *  Declarations
 */
#define FixedSize (sizeof(fixed))
#define FIXNUM_BIT1M (LISP_INTEGER_BIT - 1ULL)
#define FIXNUM_MASK_SIGN (1ULL << FIXNUM_BIT1M)
#define FIXNUM_MASK_BODY (FIXNUM_MASK_SIGN - 1ULL)
#define FIXNUM_BODY(x)  ((fixnum)((x) & FIXNUM_MASK_BODY))

typedef int (*calltype_sxhash)(addr pos, int depth, fixed *ret);
static calltype_sxhash SxhashTableEqual[LISPTYPE_SIZE];
static calltype_sxhash SxhashTableEqualp[LISPTYPE_SIZE];
static int sxfixed_equal_(addr pos, int depth, fixed *ret);
static int sxfixed_equalp_(addr pos, int depth, fixed *ret);


/*
 *  eq
 */
#define sxhash_diffshift(x,y,z) ((((uintptr_t)(x)) - ((uintptr_t)(y))) >> (z))
#define sxhash_diffheap(x,s) ((fixed)sxhash_diffshift(heap_alloc, (x), (s)))
static int sxfixed_eq_(addr pos, int depth, fixed *ret)
{
#ifdef LISP_ARCH_64BIT
	return Result(ret, sxhash_diffheap(pos, 3));
#else
	return Result(ret, sxhash_diffheap(pos, 2));
#endif
}


/*
 *  character
 */
static int sxfixed_character_(addr pos, int depth, fixed *ret)
{
	unicode c;
	GetCharacter(pos, &c);
	return Result(ret, (fixed)c);
}

static int sxfixed_character_p_(addr pos, int depth, fixed *ret)
{
	unicode c;
	GetCharacter(pos, &c);
	return Result(ret, (fixed)toUpperUnicode(c));
}


/*
 *  character2
 */
static int sxfixed_character2_(addr pos, int depth, fixed *ret)
{
	fixed a, b;

	a = (fixed)refcharacter2a(pos);
	b = (fixed)refcharacter2b(pos);

	return Result(ret, a + b);
}

static int sxfixed_character2_p_(addr pos, int depth, fixed *ret)
{
	unicode a, b;
	fixed c, d;

	getcharacter2a(pos, &a);
	getcharacter2b(pos, &b);
	c = (fixed)toUpperUnicode(a);
	d = (fixed)toUpperUnicode(b);

	return Result(ret, c + d);
}


/*
 *  binary
 */
static int sxfixed_binary_(const byte *u, size_t size, fixed *ret)
{
	int m;
	size_t i;
	fixed p[FixedSize], value;

	cleartype(p);
	for (i = 0; i < size; i++) {
		m = i % FixedSize;
		p[m] += u[i];
	}
	value = (fixed)size;
	for (i = 0; i < FixedSize; i++)
		value += p[i] << (i * 8);

	return Result(ret, value);
}

static int sxhash_binary_equal_(const void *pos, size_t size, fixnum *ret)
{
	fixed value;
	Return(sxfixed_binary_((const byte *)pos, size, &value));
	return Result(ret, FIXNUM_BODY(value));
}

int sxhash_char_equal_(const char *pos, fixnum *ret)
{
	return sxhash_binary_equal_((const void *)pos, strlen(pos), ret);
}

static int fixed_binary_p_(const byte *u, size_t size, fixed *ret)
{
	int m;
	size_t i;
	fixed p[FixedSize], value;

	cleartype(p);
	for (i = 0; i < size; i++) {
		m = i % FixedSize;
		p[m] += toupperunicode(u[i]);
	}
	value = (fixed)size;
	for (i = 0; i < FixedSize; i++)
		value += p[i] << (i * 8);

	return Result(ret, value);
}

static int sxhash_binary_equalp_(const void *pos, size_t size, fixnum *ret)
{
	fixed value;
	Return(fixed_binary_p_((const byte *)pos, size, &value));
	return Result(ret, FIXNUM_BODY(value));
}

int sxhash_char_equalp_(const char *pos, fixnum *ret)
{
	return sxhash_binary_equalp_((const void *)pos, strlen(pos), ret);
}


/*
 *  string
 */
static int sxfixed_string_(addr pos, int depth, fixed *ret)
{
	int m;
	size_t i, len;
	fixed p[FixedSize], value;
	unicode u;

	cleartype(p);
	string_length(pos, &len);
	for (i = 0; i < len; i++) {
		Return(string_getc_(pos, i, &u));
		m = i % FixedSize;
		p[m] += u;
	}
	value = (fixed)len;
	for (i = 0; i < FixedSize; i++)
		value += p[i] << (i * 8);

	return Result(ret, value);
}

static int sxfixed_string_p_(addr pos, int depth, fixed *ret)
{
	int m;
	size_t i, len;
	fixed p[FixedSize], value;
	unicode u;

	cleartype(p);
	string_length(pos, &len);
	for (i = 0; i < len; i++) {
		Return(string_getc_(pos, i, &u));
		m = i % FixedSize;
		p[m] += toupperunicode(u);
	}
	value = (fixed)len;
	for (i = 0; i < FixedSize; i++)
		value += p[i] << (i * 8);

	return Result(ret, value);
}


/*
 *  fixnum
 */
static int sxfixed_fixnum_(addr pos, int depth, fixed *ret)
{
	fixnum value;
	GetFixnum(pos, &value);
	return Result(ret, (fixed)value);
}


/*
 *  bignum
 */
static int sxfixed_bignum_(addr pos, int depth, fixed *ret)
{
	int sign;
	bigtype *data;
	size_t size, i;
	fixed value;

	GetSignBignum(pos, &sign);
	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);

	value = (fixed)sign;
	value += (fixed)size;
	for (i = 0; i < size; i++)
		value += (fixed)data[i];

	return Result(ret, value);
}


/*
 *  ratio
 */
static int sxfixed_ratio_(addr pos, int depth, fixed *ret)
{
	int sign;
	addr numer, denom;
	fixed value, v;

	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	value = (fixed)sign;
	Return(sxfixed_bignum_(numer, depth, &v));
	value += v;
	Return(sxfixed_bignum_(numer, depth, &v));
	value += v;

	return Result(ret, value);
}


/*
 *  float
 */
static int sxfixed_single_float_(addr pos, int depth, fixed *ret)
{
	single_float value;
	GetSingleFloat(pos, &value);
	return sxfixed_binary_((const byte *)&value, sizeoft(value), ret);
}

static int sxfixed_double_float_(addr pos, int depth, fixed *ret)
{
	double_float value;
	GetDoubleFloat(pos, &value);
	return sxfixed_binary_((const byte *)&value, sizeoft(value), ret);
}

static int sxfixed_long_float_(addr pos, int depth, fixed *ret)
{
	long_float value;
	GetLongFloat(pos, &value);
	return sxfixed_binary_((const byte *)&value, sizeoft(value), ret);
}

static int sxfixed_pathname_(addr pos, int depth, fixed *ret)
{
	int i;
	addr child;
	fixed result, v;

	result = 0;
	depth--;
	for (i = 0; i < PATHNAME_INDEX_SIZE; i++) {
		GetArrayA2(pos, i, &child);
		Return(sxfixed_equal_(child, depth, &v));
		result += v;
	}

	return Result(ret, result);
}

static int sxfixed_float_p_(addr pos, int depth, fixed *ret)
{
	Execute ptr;

	ptr = Execute_Thread;
	Return(rationalize_common_(ptr, pos, &pos));
	return sxfixed_equalp_(pos, depth, ret);
}


/*
 *  Global function
 */
static int sxfixed_equal_(addr pos, int depth, fixed *ret)
{
	if (pos == Nil)
		return Result(ret, 0);
	if (pos == T)
		return Result(ret, 1);

	return (SxhashTableEqual[(size_t)GetType(pos)])(pos, depth, ret);
}

static int sxfixed_equalp_(addr pos, int depth, fixed *ret)
{
	if (pos == Nil)
		return Result(ret, 0);
	if (pos == T)
		return Result(ret, 1);

	return (SxhashTableEqualp[(size_t)GetType(pos)])(pos, depth, ret);
}

static int sxhash_call_(addr pos, int depth, fixnum *ret, calltype_sxhash call)
{
	fixed value;
	Return((*call)(pos, depth, &value));
	return Result(ret, FIXNUM_BODY(value));
}

int sxhash_equal_depth_(addr pos, int depth, fixnum *ret)
{
	return sxhash_call_(pos, depth, ret, sxfixed_equal_);
}

int sxhash_equal_(addr pos, fixnum *ret)
{
	return sxhash_call_(pos, -1, ret, sxfixed_equal_);
}

int sxhash_equalp_depth_(addr pos, int depth, fixnum *ret)
{
	return sxhash_call_(pos, depth, ret, sxfixed_equalp_);
}

int sxhash_equalp_(addr pos, fixnum *ret)
{
	return sxhash_call_(pos, -1, ret, sxfixed_equalp_);
}

int sxhash_eq_(addr pos, fixnum *ret)
{
	return sxhash_call_(pos, -1, ret, sxfixed_eq_);
}

int sxhash_unicode_equalp_(unicode pos, fixnum *ret)
{
	fixed value;
	value = (fixed)toUpperUnicode(pos);
	return Result(ret, FIXNUM_BODY(value));
}

int sxhash_unicode_equal_(unicode pos, fixnum *ret)
{
	fixnum value;
	value = (fixed)pos;
	return Result(ret, FIXNUM_BODY(value));
}

int sxhash_character2_equal_(unicode a, unicode b, fixnum *ret)
{
	fixed c, d;

	c = (fixed)a;
	d = (fixed)b;

	return Result(ret, FIXNUM_BODY(c + d));
}

int sxhash_character2_equalp_(unicode a, unicode b, fixnum *ret)
{
	fixed c, d;

	c = (fixed)toUpperUnicode(a);
	d = (fixed)toUpperUnicode(b);

	return Result(ret, FIXNUM_BODY(c + d));
}


/*
 *  cons
 */
static int sxfixed_cons_(addr pos, int depth, fixed *ret)
{
	addr right;
	fixed v1, v2;

	if (depth == 0)
		return Result(ret, 0);
	GetCons(pos, &pos, &right);
	depth--;
	Return(sxfixed_equal_(pos, depth, &v1));
	Return(sxfixed_equal_(right, depth, &v2));

	return Result(ret, depth + v1 + (3 * v2));
}

static int sxfixed_cons_p_(addr pos, int depth, fixed *ret)
{
	addr right;
	fixed v1, v2;

	if (depth == 0)
		return Result(ret, 0);
	GetCons(pos, &pos, &right);
	depth--;
	Return(sxfixed_equalp_(pos, depth, &v1));
	Return(sxfixed_equalp_(right, depth, &v2));

	return Result(ret, depth + v1 + (3 * v2));
}


/*
 *  vector
 */
static int sxfixed_vector_(addr pos, int depth, fixed *ret)
{
	size_t len;
	lenarray(pos, &len);
	return Result(ret, (fixed)len);
}


/*
 *  array
 */
static int sxfixed_array_(addr pos, int depth, fixed *ret)
{
	if (strarrayp(pos))
		return sxfixed_string_(pos, depth, ret);
	else
		return sxfixed_eq_(pos, depth, ret);
}

static int sxfixed_array_p_(addr pos, int depth, fixed *ret)
{
	if (strarrayp(pos))
		return sxfixed_string_p_(pos, depth, ret);
	else
		return sxfixed_eq_(pos, depth, ret);
}


/*
 *  Initialize
 */
static void SetSxhashEqual(enum LISPTYPE type, calltype_sxhash call)
{
	SxhashTableEqual[(size_t)type] = call;
}

static void SetSxhashEqualp(enum LISPTYPE type, calltype_sxhash call)
{
	SxhashTableEqualp[(size_t)type] = call;
}

void init_sxhash(void)
{
	int i;

	/*
	 *  equal
	 */
	for (i = 0; i < LISPTYPE_SIZE; i++)
		SxhashTableEqual[i] = sxfixed_eq_;
	SetSxhashEqual(LISPTYPE_CONS, sxfixed_cons_);
	SetSxhashEqual(LISPTYPE_VECTOR, sxfixed_vector_);
	SetSxhashEqual(LISPTYPE_ARRAY, sxfixed_array_);
	/* object */
	SetSxhashEqual(LISPTYPE_CHARACTER, sxfixed_character_);
	SetSxhashEqual(LISPSYSTEM_CHARACTER2, sxfixed_character2_);
	SetSxhashEqual(LISPTYPE_STRING, sxfixed_string_);
	SetSxhashEqual(LISPTYPE_FIXNUM, sxfixed_fixnum_);
	SetSxhashEqual(LISPTYPE_BIGNUM, sxfixed_bignum_);
	SetSxhashEqual(LISPTYPE_RATIO, sxfixed_ratio_);
	SetSxhashEqual(LISPTYPE_SINGLE_FLOAT, sxfixed_single_float_);
	SetSxhashEqual(LISPTYPE_DOUBLE_FLOAT, sxfixed_double_float_);
	SetSxhashEqual(LISPTYPE_LONG_FLOAT, sxfixed_long_float_);
	SetSxhashEqual(LISPTYPE_PATHNAME, sxfixed_pathname_);


	/*
	 *  equlap
	 */
	for (i = 0; i < LISPTYPE_SIZE; i++)
		SxhashTableEqualp[i] = SxhashTableEqual[i];
	SetSxhashEqualp(LISPTYPE_CONS, sxfixed_cons_p_);
	SetSxhashEqualp(LISPTYPE_ARRAY, sxfixed_array_p_);
	/* object */
	SetSxhashEqualp(LISPTYPE_CHARACTER, sxfixed_character_p_);
	SetSxhashEqualp(LISPSYSTEM_CHARACTER2, sxfixed_character2_p_);
	SetSxhashEqualp(LISPTYPE_STRING, sxfixed_string_p_);
	SetSxhashEqual(LISPTYPE_SINGLE_FLOAT, sxfixed_float_p_);
	SetSxhashEqual(LISPTYPE_DOUBLE_FLOAT, sxfixed_float_p_);
	SetSxhashEqual(LISPTYPE_LONG_FLOAT, sxfixed_float_p_);
}


/************************************************************
 *  symbol.c
 ************************************************************/

#ifdef LISP_THREAD_ENABLE
#define SYMSTACK_SIZE       4
#else
#define SYMSTACK_SIZE       1
#endif

static int InitObject = 0;
static rwlocklite MutexSymbol;

int init_symbol(void)
{
	if (InitObject) {
		Debug("InitObject error.");
		return 1;
	}
	if (lispd_make_rwlocklite(&MutexSymbol)) {
		Debug("lispd_make_mutexlite error");
		return 1;
	}
	InitObject = 1;

	return 0;
}

void free_symbol(void)
{
	if (InitObject) {
		lispd_destroy_rwlocklite(&MutexSymbol);
		InitObject = 0;
	}
}

void build_symbol(void)
{
	addr symbol, value;

	GetConst(SPECIAL_GENSYM_COUNTER, &symbol);
	fixnum_heap(&value, 0);
	SetValueSymbol(symbol, value);
}


/*
 *  symbol
 */
void symbol_heap(addr *ret)
{
	int i;
	addr pos, make;

	/* symbol object */
	heap_symbol(&pos);

	/* Unbound */
	SetValueSymbol_Low(pos, Unbound);
	SetFunctionSymbol_Low(pos, Unbound);

	/* stack */
	heap_array4(&make, LISPSYSTEM_SYMSTACK, SYMSTACK_SIZE);
	for (i = 0; i < SYMSTACK_SIZE; i++) {
		SetArrayA4(make, i, NULL);
	}
	SetSpecialSymbol_Low(pos, make);
	*ret = pos;
}

int symbolp(addr pos)
{
	return GetType(pos) == LISPTYPE_SYMBOL || pos == Nil || pos == T;
}

int keywordp(addr pos)
{
	addr keyword;

	if (GetType(pos) != LISPTYPE_SYMBOL)
		return 0;
	GetConst(PACKAGE_KEYWORD, &keyword);
	Check(GetType(keyword) != LISPTYPE_PACKAGE, "package error");
	GetPackageSymbol(pos, &pos);

	return pos == keyword;
}

static int setcheck_symbol_(addr symbol, size_t index, addr value)
{
	CheckSymbol(symbol);
	if (GetStatusReadOnly(symbol))
		return fmte_("Cannot set the constant variable ~S.", symbol, NULL);
	SetArrayA2(symbol, index, value);
	return 0;
}

/* name */
void getname_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetNameSymbol_Low(symbol, ret);
}
void setname_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(GetStatusReadOnly(symbol), "readonly error");
	SetNameSymbol_Low(symbol, value);
}

/* value */
void getvalue_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetValueSymbol_Low(symbol, ret);
}
void setvalue_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(GetStatusReadOnly(symbol), "readonly error");
	SetValueSymbol_Low(symbol, value);
}
int setvalue_symbol_(addr symbol, addr value)
{
	return setcheck_symbol_(symbol, SYMBOL_INDEX_VALUE, value);
}

/* function */
void getfunction_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetFunctionSymbol_Low(symbol, ret);
}
void setfunction_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(GetStatusReadOnly(symbol), "readonly error");
	SetFunctionSymbol_Low(symbol, value);
}
int setfunction_symbol_(addr symbol, addr value)
{
	return setcheck_symbol_(symbol, SYMBOL_INDEX_FUNCTION, value);
}

/* package */
void getpackage_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetPackageSymbol_Low(symbol, ret);
}
void setpackage_symbol(addr symbol, addr value)
{
	Check(value != Nil && GetType(value) != LISPTYPE_PACKAGE, "type error");
	/* no-readonly-check */
	SetPackageSymbol_Low(symbol, value);
}

/* plist */
void getplist_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetPlistSymbol_Low(symbol, ret);
}
void setplist_symbol(addr symbol, addr value)
{
	Check(! listp(value), "type error");
	/* no-readonly-check */
	SetPlistSymbol_Low(symbol, value);
}

/* info */
static void getinfo_constant(addr symbol, constindex index, addr *ret)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	*ret = getplist_constant(symbol, index, &symbol)? Nil: symbol;
}

static void setinfo_constant(addr symbol, constindex index, addr value)
{
	addr plist;

	CheckSymbol(symbol);
	Check(GetStatusReadOnly(symbol), "readonly error");
	GetInfoSymbol_Low(symbol, &plist);
	if (setplist_constant_heap(plist, index, value, &plist))
		SetInfoSymbol_Low(symbol, plist);
}

static void setinfo_nocheck_constant(addr symbol, constindex index, addr value)
{
	addr plist;

	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &plist);
	if (setplist_constant_heap(plist, index, value, &plist))
		SetInfoSymbol_Low(symbol, plist);
}

static int setinfo_constant_(addr symbol, constindex index, addr value)
{
	addr plist;

	CheckSymbol(symbol);
	if (GetStatusReadOnly(symbol))
		return fmte_("Cannot set the constant variable ~S.", symbol, NULL);
	GetInfoSymbol_Low(symbol, &plist);
	if (setplist_constant_heap(plist, index, value, &plist))
		SetInfoSymbol_Low(symbol, plist);

	return 0;
}

static void reminfo_constant(addr symbol, constindex index)
{
	addr plist;

	CheckSymbol(symbol);
	Check(GetStatusReadOnly(symbol), "readonly error");
	GetInfoSymbol_Low(symbol, &plist);
	if (remplist_constant(plist, index, &plist))
		SetInfoSymbol_Low(symbol, plist);
}

static int reminfo_constant_(addr symbol, constindex index)
{
	addr plist;

	CheckSymbol(symbol);
	if (GetStatusReadOnly(symbol))
		return fmte_("Cannot set the constant variable ~S.", symbol, NULL);
	GetInfoSymbol_Low(symbol, &plist);
	if (remplist_constant(plist, index, &plist))
		SetInfoSymbol_Low(symbol, plist);

	return 0;
}

static void reminfo_nocheck_constant(addr symbol, constindex index)
{
	addr plist;

	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &plist);
	if (remplist_constant(plist, index, &plist))
		SetInfoSymbol_Low(symbol, plist);
}

/* type value */
void gettype_value_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_SYSTEM_VALUE, ret);
}
void settype_value_symbol(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	setinfo_constant(symbol, CONSTANT_SYSTEM_VALUE, value);
}
int settype_value_symbol_(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	return setinfo_constant_(symbol, CONSTANT_SYSTEM_VALUE, value);
}
void remtype_value_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_SYSTEM_VALUE);
}

/* type function */
void gettype_function_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_SYSTEM_FUNCTION, ret);
}
void settype_function_symbol(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	setinfo_constant(symbol, CONSTANT_SYSTEM_FUNCTION, value);
}
int settype_function_symbol_(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	return setinfo_constant_(symbol, CONSTANT_SYSTEM_FUNCTION, value);
}
int remtype_function_symbol_(addr symbol)
{
	return reminfo_constant_(symbol, CONSTANT_SYSTEM_FUNCTION);
}

/* type setf */
void gettype_setf_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_SYSTEM_SETF, ret);
}
void settype_setf_symbol(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	setinfo_constant(symbol, CONSTANT_SYSTEM_SETF, value);
}
int settype_setf_symbol_(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	return setinfo_constant_(symbol, CONSTANT_SYSTEM_SETF, value);
}
int remtype_setf_symbol_(addr symbol)
{
	return reminfo_constant_(symbol, CONSTANT_SYSTEM_SETF);
}

/* inline */
int inlinep_function_symbol(addr symbol)
{
	addr check;
	getinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION, &symbol);
	GetConst(COMMON_INLINE, &check);
	return symbol == check;
}
void setinline_function_symbol(addr symbol)
{
	addr value;
	GetConst(COMMON_INLINE, &value);
	setinfo_nocheck_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
}
int notinlinep_function_symbol(addr symbol)
{
	addr check;
	getinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION, &symbol);
	GetConst(COMMON_NOTINLINE, &check);
	return symbol == check;
}
void setnotinline_function_symbol(addr symbol)
{
	addr value;
	GetConst(COMMON_NOTINLINE, &value);
	setinfo_nocheck_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
}
void reminline_function_symbol(addr symbol)
{
	reminfo_nocheck_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION);
}

int inlinep_setf_symbol(addr symbol)
{
	addr check;
	getinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF, &symbol);
	GetConst(COMMON_INLINE, &check);
	return symbol == check;
}
void setinline_setf_symbol(addr symbol)
{
	addr value;
	GetConst(COMMON_INLINE, &value);
	setinfo_nocheck_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF, value);
}
int notinlinep_setf_symbol(addr symbol)
{
	addr check;
	getinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF, &symbol);
	GetConst(COMMON_NOTINLINE, &check);
	return symbol == check;
}
void setnotinline_setf_symbol(addr symbol)
{
	addr value;
	GetConst(COMMON_NOTINLINE, &value);
	setinfo_nocheck_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF, value);
}
void reminline_setf_symbol(addr symbol)
{
	reminfo_nocheck_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF);
}

/* setf */
void getsetf_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	*ret = getplist_constant(symbol, CONSTANT_COMMON_SETF, &symbol)? Unbound: symbol;
}
void setsetf_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_SETF, value);
}
int setsetf_symbol_(addr symbol, addr value)
{
	return setinfo_constant_(symbol, CONSTANT_COMMON_SETF, value);
}
void remsetf_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_COMMON_SETF);
}

/* setf-macro */
void getsetfmacro_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	*ret = getplist_constant(symbol, CONSTANT_COMMON_DEFINE_SETF_EXPANDER,
			&symbol)? Unbound: symbol;
}
void setsetfmacro_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_DEFINE_SETF_EXPANDER, value);
}
int setsetfmacro_symbol_(addr symbol, addr value)
{
	return setinfo_constant_(symbol, CONSTANT_COMMON_DEFINE_SETF_EXPANDER, value);
}
void remsetfmacro_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_COMMON_DEFINE_SETF_EXPANDER);
}

/* macro */
void getmacro_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	*ret = getplist_constant(symbol, CONSTANT_COMMON_DEFMACRO, &symbol)?
		Unbound: symbol;
}
void setmacro_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_DEFMACRO, value);
}
int setmacro_symbol_(addr symbol, addr value)
{
	return setinfo_constant_(symbol, CONSTANT_COMMON_DEFMACRO, value);
}
void remmacro_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_COMMON_DEFMACRO);
}

/* symbol-macro */
void getsymbol_macro_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	*ret = getplist_constant(symbol, CONSTANT_COMMON_DEFINE_SYMBOL_MACRO, &symbol)?
		Unbound: symbol;
}
int setsymbol_macro_symbol_(addr symbol, addr value)
{
	return setinfo_constant_(symbol, CONSTANT_COMMON_DEFINE_SYMBOL_MACRO, value);
}
void remsymbol_macro_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_COMMON_DEFINE_SYMBOL_MACRO);
}

/* compiler-macro-function */
void get_compiler_macro_symbol(addr symbol, addr *value)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_COMPILER_MACRO_FUNCTION, value);
}

int set_compiler_macro_symbol_(addr symbol, addr value)
{
	CheckSymbol(symbol);
	return setinfo_constant_(symbol, CONSTANT_SYSTEM_COMPILER_MACRO_FUNCTION, value);
}

int rem_compiler_macro_symbol_(addr symbol)
{
	if (GetStatusReadOnly(symbol))
		return fmte_("Cannot set the constant variable ~S.", symbol, NULL);
	reminfo_constant(symbol, CONSTANT_SYSTEM_COMPILER_MACRO_FUNCTION);

	return 0;
}

void get_setf_compiler_macro_symbol(addr symbol, addr *value)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_SETF_COMPILER_MACRO_FUNCTION, value);
}

int set_setf_compiler_macro_symbol_(addr symbol, addr value)
{
	CheckSymbol(symbol);
	return setinfo_constant_(symbol,
			CONSTANT_SYSTEM_SETF_COMPILER_MACRO_FUNCTION, value);
}

int rem_setf_compiler_macro_symbol_(addr symbol)
{
	if (GetStatusReadOnly(symbol))
		return fmte_("Cannot set the constant variable ~S.", symbol, NULL);
	reminfo_constant(symbol, CONSTANT_SYSTEM_SETF_COMPILER_MACRO_FUNCTION);

	return 0;
}


/* scope */
void getscope_symbol(addr symbol, addr *value)
{
	getinfo_constant(symbol, CONSTANT_COMMON_SPECIAL, value);
}
static void setscope_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_SPECIAL, value);
}
void setspecial_symbol(addr symbol)
{
	CheckSymbol(symbol);
	setscope_symbol(symbol, T);
}
int setspecial_symbol_(addr symbol)
{
	return setinfo_constant_(symbol, CONSTANT_COMMON_SPECIAL, T);
}
void setlexical_symbol(addr symbol)
{
	CheckSymbol(symbol);
	setscope_symbol(symbol, Nil);
}
int specialp_symbol(addr symbol)
{
	CheckSymbol(symbol);
	getscope_symbol(symbol, &symbol);
	return symbol != Nil;
}
int lexicalp_symbol(addr symbol)
{
	CheckSymbol(symbol);
	getscope_symbol(symbol, &symbol);
	return symbol == Nil;
}

/* special-operator */
void set_special_operator(addr symbol)
{
	CheckSymbol(symbol);
	setinfo_constant(symbol, CONSTANT_COMMON_SPECIAL_OPERATOR_P, T);
}
int get_special_operator(addr symbol)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_COMMON_SPECIAL_OPERATOR_P, &symbol);
	return symbol != Nil;
}

/* document */
void getdocument_variable_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_COMMON_VARIABLE, ret);
}
void setdocument_variable_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_VARIABLE, value);
}
void getdocument_type_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_SYSTEM_TYPE_DOCUMENTATION, ret);
}
void setdocument_type_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_SYSTEM_TYPE_DOCUMENTATION, value);
}

/* deftype */
void getdeftype_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_DEFTYPE, ret);
}
int setdeftype_symbol_(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(value != Nil && GetType(value) != LISPTYPE_FUNCTION, "type error");
	return setinfo_constant_(symbol, CONSTANT_SYSTEM_DEFTYPE, value);
}
void remdeftype_symbol(addr symbol)
{
	CheckSymbol(symbol);
	reminfo_constant(symbol, CONSTANT_SYSTEM_DEFTYPE);
}

/* type-symbol */
void getsymboltype_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_TYPE_SYMBOL, ret);
}

void setsymboltype_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	setinfo_nocheck_constant(symbol, CONSTANT_SYSTEM_TYPE_SYMBOL, value);
}

void getlisttype_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_TYPE_LIST, ret);
}

void setlisttype_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	setinfo_nocheck_constant(symbol, CONSTANT_SYSTEM_TYPE_LIST, value);
}

/* clos */
void getclass_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_COMMON_CLASS, ret);
}

void setclass_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(GetType(value) != LISPTYPE_CLOS, "type error");
	setinfo_nocheck_constant(symbol, CONSTANT_COMMON_CLASS, value);
}

void remclass_symbol(addr symbol)
{
	CheckSymbol(symbol);
	reminfo_nocheck_constant(symbol, CONSTANT_COMMON_CLASS);
}

void getcombination_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_COMMON_METHOD_COMBINATION, ret);
}

void setcombination_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(GetType(value) != LISPTYPE_CLOS, "type error");
	setinfo_nocheck_constant(symbol, CONSTANT_COMMON_METHOD_COMBINATION, value);
}


/*
 *  symstack
 */
static void realloc_symbol(addr *stack, size_t size1, size_t size2)
{
	addr make, old, temp;
	byte32 size32;
	size_t i;

	old = *stack;
	size32 = (byte32)size2;
	if ((size_t)size32 <= size1)
		Abort("size error");

	heap_array4(&make, LISPSYSTEM_SYMSTACK, size32);
	for (i = 0; i < size1; i++) {
		GetArrayA4(old, i, &temp);
		SetArrayA4(make, i, temp);
		SetArrayA4(old, i, Nil);
	}
	for (; i < size2; i++) {
		SetArrayA4(make, i, NULL);
	}
	*stack = make;
}

static void symstack(size_t index, addr symbol, addr *ret)
{
	addr stack, child;
	size_t i, size, size2;

	Check(GetStatusDynamic(symbol), "dynamic error.");
	lispd_rdlock_rwlocklite(&MutexSymbol);
	GetSpecialSymbol_Low(symbol, &stack);
	size = GetLenArrayA4(stack);
	if (index < size) {
		*ret = stack;
		lispd_unrdlock_rwlocklite(&MutexSymbol);
		return;
	}

	/* write lock */
	lispd_unrdlock_rwlocklite(&MutexSymbol);
	lispd_wrlock_rwlocklite(&MutexSymbol);
	/* reload */
	GetSpecialSymbol_Low(symbol, &stack);
	size = GetLenArrayA4(stack);
	if (index < size) {
		*ret = stack;
		lispd_unwrlock_rwlocklite(&MutexSymbol);
		return;
	}

	/* size extension */
	for (size2 = size; size2 <= index; )
		size2 <<= 1;
	realloc_symbol(&stack, size, size2);
	SetSpecialSymbol_Low(symbol, stack);
	for (i = size; i < size2; i++) {
		consnil_heap(&child);
		SetArrayA4(stack, i, child);
	}
	*ret = stack;
	lispd_unwrlock_rwlocklite(&MutexSymbol);
}

void getspecial_unsafe(Execute ptr, addr pos, addr *ret)
{
	CheckSymbol(pos);
	symstack(ptr->index, pos, &pos);
	GetArrayA4(pos, ptr->index, ret);
}

void setspecial_unsafe(Execute ptr, addr pos, addr value)
{
	CheckSymbol(pos);
	symstack(ptr->index, pos, &pos);
	SetArrayA4_force(pos, ptr->index, value);
}

void getspecial_local(Execute ptr, addr pos, addr *ret)
{
	CheckSymbol(pos);
	if (GetStatusReadOnly(pos)) {
		GetValueSymbol(pos, ret);
		return;
	}
	getspecial_unsafe(ptr, pos, ret);
	if (*ret == NULL) {
		GetValueSymbol(pos, ret);
	}
}

int getspecialcheck_local_(Execute ptr, addr pos, addr *ret)
{
	getspecial_local(ptr, pos, ret);
	if (*ret == Unbound)
		return call_unbound_variable_(ptr, pos);

	return 0;
}

void setspecial_local(Execute ptr, addr pos, addr value)
{
	addr check;

	CheckSymbol(pos);
	getspecial_unsafe(ptr, pos, &check);
	if (check == NULL) {
		SetValueSymbol(pos, value);
	}
	else {
		setspecial_unsafe(ptr, pos, value);
	}
}

int getfunction_global_(addr pos, addr *ret)
{
	GetFunctionSymbol(pos, ret);
	if (*ret == Unbound)
		return call_undefined_function_(NULL, pos);

	return 0;
}

int getsetf_global_(addr pos, addr *ret)
{
	addr setf;

	getsetf_symbol(pos, ret);
	if (*ret == Unbound) {
		GetConst(COMMON_SETF, &setf);
		list_heap(&pos, setf, pos, NULL);
		return call_undefined_function_(NULL, pos);
	}

	return 0;
}

int alldelete_function_(addr pos)
{
	Check(! symbolp(pos), "type error");
	if (GetStatusReadOnly(pos))
		return fmte_("Cannot set the constant variable ~S.", pos, NULL);
	SetFunctionSymbol(pos, Unbound);
	remsetf_symbol(pos);
	remsetfmacro_symbol(pos);
	remmacro_symbol(pos);
	Return(remtype_function_symbol_(pos));
	Return(remtype_setf_symbol_(pos));

	return 0;
}


/*
 *  gensym
 */
int gensymp(addr pos)
{
	if (GetType(pos) != LISPTYPE_SYMBOL)
		return 0;
	GetPackageSymbol(pos, &pos);
	return pos == Nil;
}

void make_symbolchar(addr *ret, const char *str)
{
	addr pos, name;

	symbol_heap(&pos);
	strvect_char_heap(&name, str);
	SetNameSymbol(pos, name);
	*ret = pos;
}

static int make_gensym_argument_(Execute ptr,
		const char *prefix1, addr prefix2, addr counter, addr *ret)
{
	addr symbol, value, queue, name, gensym;
	LocalRoot local;
	LocalStack stack;

	/* symbol-name */
	local = ptr->local;
	push_local(local, &stack);
	if (counter == NULL) {
		GetConst(SPECIAL_GENSYM_COUNTER, &symbol);
		Return(getspecialcheck_local_(ptr, symbol, &value));
	}
	else {
		symbol = NULL;
		value = counter;
	}
	Check(! integerp(value), "type error");
	charqueue_local(local, &queue, 1 + 16);
	if (prefix1) {
		Return(pushchar_charqueue_local_(local, queue, prefix1));
	}
	else {
		Return(pushstring_charqueue_local_(local, queue, prefix2));
	}
	Return(decimal_charqueue_integer_local_(local, value, queue));
	make_charqueue_heap(queue, &name);
	rollback_local(local, stack);

	/* gensym */
	symbol_heap(&gensym);
	SetNameSymbol(gensym, name);
	*ret = gensym;

	/* (1+ *gensym-counter*) */
	if (counter == NULL) {
		Return(oneplus_integer_common_(local, value, &value));
		setspecial_local(ptr, symbol, value);
	}

	return 0;
}
int make_gensym_(Execute ptr, addr *ret)
{
	return make_gensym_argument_(ptr, "G", NULL, NULL, ret);
}
int make_gensym_prefix_(Execute ptr, addr prefix, addr *ret)
{
	Check(! stringp(prefix), "type error");
	return make_gensym_argument_(ptr, NULL, prefix, NULL, ret);
}
int make_gensym_integer_(Execute ptr, addr value, addr *ret)
{
	Check(! integerp(value), "type error");
	return make_gensym_argument_(ptr, "G", NULL, value, ret);
}
int make_gensym_char_(Execute ptr, const char *str, addr value, addr *ret)
{
	Check(! integerp(value), "type error");
	return make_gensym_argument_(ptr, str, NULL, value, ret);
}

void setcounter_gensym(Execute ptr, fixnum value)
{
	addr pos, symbol;

	Check(value < 0, "value error");
	fixnum_heap(&pos, value);
	GetConst(SPECIAL_GENSYM_COUNTER, &symbol);
	setspecial_local(ptr, symbol, pos);
}


/************************************************************
 *  syscall.c
 ************************************************************/

void init_syscall(void)
{
	init_syscall_common();
	init_syscall_function();
}

void build_syscall(void)
{
	build_syscall_common();
	build_syscall_function();
}


/************************************************************
 *  syscall_common.c
 ************************************************************/

/* (defun redirect-restart (condition list) ...) -> null */
static int syscall_redirect_restart(Execute ptr, addr condition, addr list)
{
	Return(redirect_restart_syscode(ptr, condition, list));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_redirect_restart(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Condition);
	GetTypeTable(&values, List);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_redirect_restart(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_REDIRECT_RESTART, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_redirect_restart);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_redirect_restart(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun define-symbol-macro (symbol form) ...) -> symbol */
static int syscall_define_symbol_macro(Execute ptr, addr symbol, addr form)
{
	Return(setsymbol_macro_symbol_(symbol, form));
	setresult_control(ptr, symbol);
	return 0;
}

static void type_syscall_define_symbol_macro(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_define_symbol_macro(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFINE_SYMBOL_MACRO, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_define_symbol_macro);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_define_symbol_macro(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun symbol-macro-expander (&rest form) form) */
static int syscall_symbol_macro_expander(Execute ptr, addr form, addr env)
{
	setresult_control(ptr, form);
	return 0;
}

static void defun_symbol_macro_expander(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SYMBOL_MACRO_EXPANDER, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_syscall_symbol_macro_expander);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun defconstant (symbol value document) ...) -> symbol */
static int syscall_defconstant(Execute ptr, addr symbol, addr value, addr doc)
{
	Return(defconstant_syscode(symbol, value, doc));
	setresult_control(ptr, symbol);
	return 0;
}

static void type_syscall_defconstant(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	GetTypeTable(&type, StringNull);
	typeargs_var3(&args, args, values, type);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_defconstant(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFCONSTANT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_syscall_defconstant);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_defconstant(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun in-package (string-designer) ...) -> package */
static int syscall_in_package(Execute ptr, addr name)
{
	Return(in_package_syscode_(ptr, name, &name));
	setresult_control(ptr, name);
	return 0;
}

static void type_syscall_in_package(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringDesigner);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Package);
	type_compiled_heap(args, values, ret);
}

static void defun_in_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_IN_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_in_package);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_in_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun setplist (key value list) ...) -> list */
static int syscall_setplist(Execute ptr, addr key, addr value, addr list)
{
	setplist_syscode(key, value, list, &list);
	setresult_control(ptr, list);
	return 0;
}

static void defun_setplist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SETPLIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_syscall_setplist);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Acons);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun remplist (key list) ...) -> value, check
 *   key    t
 *   list   list
 *   value  list
 *   check  boolean
 */
static int syscall_remplist(Execute ptr, addr key, addr list)
{
	Return(remplist_syscode_(key, list, &key, &list));
	setvalues_control(ptr, key, list, NULL);
	return 0;
}

static void type_syscall_remplist(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var2(&args, args, values);
	GetTypeTable(&type, Boolean);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_remplist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_REMPLIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_remplist);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_remplist(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-hash-iterator (table) ...) -> hash-iterator */
static int syscall_make_hash_iterator(Execute ptr, addr pos)
{
	make_hash_iterator_syscode(pos, &pos);
	setresult_control(ptr, pos);
	return 0;
}

static void type_make_hash_iterator(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Hashtable);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_make_hash_iterator(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_HASH_ITERATOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_make_hash_iterator);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_hash_iterator(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun next-hash-iterator (iterator) ...) -> (values boolean &rest t) */
static int syscall_next_hash_iterator(Execute ptr, addr pos)
{
	addr key, value;

	next_hash_iterator_syscode(pos, &pos, &key, &value);
	if (pos == Nil)
		setresult_control(ptr, Nil);
	else
		setvalues_control(ptr, pos, key, value, NULL);

	return 0;
}

static void type_next_hash_iterator(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&type, T);
	typeargs_var1(&args, type);
	/* (values boolean &rest t) */
	GetTypeValues(&values, Boolean);
	conscar_heap(&values, values);
	type_values_heap(values, Nil, type, Nil, &values);
	/* result */
	type_compiled_heap(args, values, ret);
}

static void defun_next_hash_iterator(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_NEXT_HASH_ITERATOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_next_hash_iterator);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_next_hash_iterator(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-package-iterator (table internal external inherited) ...)
 *     -> package-iterator
 *   internal   t
 *   external   t
 *   inherited  t
 */
static int syscall_make_package_iterator(Execute ptr, addr pos, addr a, addr b, addr c)
{
	Return(make_package_iterator_syscode_(pos, a, b, c, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_make_package_iterator(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PackageDesigner);
	GetTypeTable(&values, List);
	type2or_heap(args, values, &args);
	GetTypeTable(&values, T);
	typeargs_var4(&args, args, values, values, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_make_package_iterator(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_PACKAGE_ITERATOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var4(pos, p_defun_syscall_make_package_iterator);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_package_iterator(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun next-package-iterator (iterator) ...) -> * */
static int syscall_next_package_iterator(Execute ptr, addr pos)
{
	addr symbol, status, package;

	Return(next_package_iterator_syscode_(ptr, pos, &pos, &symbol, &status, &package));
	if (pos == Nil)
		setresult_control(ptr, Nil);
	else
		setvalues_control(ptr, pos, symbol, status, package, NULL);

	return 0;
}

static void type_next_package_iterator(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeTable(&values, Asterisk);
	type_compiled_heap(args, values, ret);
}

static void defun_next_package_iterator(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_NEXT_PACKAGE_ITERATOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_next_package_iterator);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_next_package_iterator(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun defpackage (name &key size docuemntation nicknames use
 *     shadow shadowing-import-from import-from export intern)
 *     -> package
 *   name                    string-designer
 *   :size                   (or null (integer 0 *))
 *   :documentation          (or null string)
 *   :nicknames              list
 *   :use                    list
 *   :shadow                 list
 *   :shadowing-import-from  list
 *   :import-from            list
 *   :export                 list
 *   :intern                 list
 */
static int syscall_defpackage(Execute ptr, addr var, addr rest)
{
	Return(defpackage_syscode(ptr, var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_defpackage(addr *ret)
{
	addr args, values;
	addr key, key1, key2, key3, key4, key5, key6, key7, key8, key9, key10;

	GetTypeTable(&args, StringDesigner);
	KeyTypeTable(&key1, SIZE, IntplusNull);
	KeyTypeTable(&key2, DOCUMENTATION, StringNull);
	KeyTypeTable(&key3, NICKNAMES, List);
	KeyTypeTable(&key4, USE, List);
	KeyTypeTable(&key5, SHADOW, List);
	KeyTypeTable(&key6, SHADOWING_IMPORT_FROM, List);
	KeyTypeTable(&key7, SHADOWING_IMPORT_FROM, List);
	KeyTypeTable(&key8, IMPORT_FROM, List);
	KeyTypeTable(&key9, EXPORT, List);
	KeyTypeTable(&key10, INTERN, List);
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, key8, key9, key10, NULL);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, Package);
	type_compiled_heap(args, values, ret);
}

static void defun_defpackage(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFPACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_defpackage);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_defpackage(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun do-symbols (function package) ...) -> nil */
static int syscall_do_symbols(Execute ptr, addr call, addr package)
{
	Return(do_symbols_syscode(ptr, call, package));
	setvalues_nil_control(ptr);
	return 0;
}

static void defun_do_symbols(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DO_SYMBOLS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_do_symbols);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, DoSymbols);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun do-external-symbols (function package) ...) -> nil */
static int syscall_do_external_symbols(Execute ptr, addr call, addr package)
{
	Return(do_external_symbols_syscode(ptr, call, package));
	setvalues_nil_control(ptr);
	return 0;
}

static void defun_do_external_symbols(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DO_EXTERNAL_SYMBOLS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_do_external_symbols);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, DoSymbols);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun do-all-symbols (function) ...) -> nil */
static int syscall_do_all_symbols(Execute ptr, addr call)
{
	Return(do_all_symbols_syscode_(ptr, call));
	setvalues_nil_control(ptr);
	return 0;
}

static void type_do_all_symbols(addr *ret)
{
	/* (function (function) (values &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Function);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Nil);
	type_compiled_heap(args, values, ret);
}

static void defun_do_all_symbols(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DO_ALL_SYMBOLS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_do_all_symbols);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_do_all_symbols(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun getdoc-variable (symbol) ...) -> (or string null) */
static int syscall_getdoc_variable(Execute ptr, addr var)
{
	getdoc_variable_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_getdoc_variable(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, ret);
}

static void defun_getdoc_variable(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_GETDOC_VARIABLE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_getdoc_variable);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_getdoc_variable(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun setdoc-variable (symbol string) ...) -> string */
static int syscall_setdoc_variable(Execute ptr, addr var, addr value)
{
	setdoc_variable_syscode(var, value);
	setresult_control(ptr, value);
	return 0;
}

static void type_setdoc_variable(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, String);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, ret);
}

static void defun_setdoc_variable(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SETDOC_VARIABLE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_setdoc_variable);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_setdoc_variable(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ecase-error (value list) ...) -> nil */
static int syscall_ecase_error(Execute ptr, addr value, addr list)
{
	Return(ecase_error_syscode_(ptr, value, list));
	setvalues_nil_control(ptr);
	return 0;
}

static void defun_ecase_error(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ECASE_ERROR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_ecase_error);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EcaseError);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun etypecase-error (value list) ...) -> nil */
static int syscall_etypecase_error(Execute ptr, addr value, addr list)
{
	Return(etypecase_error_syscode_(ptr, value, list));
	setvalues_nil_control(ptr);
	return 0;
}

static void defun_etypecase_error(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ETYPECASE_ERROR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_etypecase_error);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EcaseError);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun define-setf-expander (name lambda) ...) -> name */
static int syscall_define_setf_expander(Execute ptr, addr symbol, addr call)
{
	Return(define_setf_expander_syscode_(symbol, call));
	setresult_control(ptr, symbol);
	return 0;
}

static void type_syscall_define_setf_expander(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, Function);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_define_setf_expander(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFINE_SETF_EXPANDER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_define_setf_expander);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_define_setf_expander(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun end-input-stream (string-stream) -> index */
static int syscall_end_input_stream(Execute ptr, addr var)
{
	end_input_stream_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_end_input_stream(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Index);
	type_compiled_heap(args, values, ret);
}

static void defun_end_input_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_END_INPUT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_end_input_stream);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_end_input_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-extend-output-stream (string &key element-type) ...)
 *     -> string-stream
 */
static int syscall_make_extend_output_stream(Execute ptr, addr var, addr rest)
{
	make_extend_output_stream_syscode(var, rest, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_make_extend_output_stream(addr *ret)
{
	addr args, values;

	/* key */
	KeyTypeTable(&args, ELEMENT_TYPE, Symbol);
	list_heap(&args, args, NULL);
	GetTypeTable(&values, String);
	/* type */
	typeargs_var1key(&args, values, args);
	GetTypeTable(&values, StringStream);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_extend_output_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_EXTEND_OUTPUT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_make_extend_output_stream);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_extend_output_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun prompt-for (type &rest args) ...) -> t */
static int syscall_prompt_for(Execute ptr, addr type, addr args)
{
	Return(prompt_for_syscode(ptr, type, args, &type));
	setresult_control(ptr, type);
	return 0;
}

static void type_prompt_for(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, TypeSpec);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_prompt_for(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PROMPT_FOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_prompt_for);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_prompt_for(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun print-unreadable-call (stream pos type identity body) ...) -> null */
static int syscall_print_unreadable_call(Execute ptr,
		addr stream, addr pos, addr type, addr identity, addr body)
{
	Return(print_unreadable_call_syscode(ptr, stream, pos, type, identity, body));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_print_unreadable_call(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, StreamDesigner);
	typeargs_var5(&args, values, args, args, args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_print_unreadable_call(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PRINT_UNREADABLE_CALL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var5(pos, p_defun_syscall_print_unreadable_call);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_print_unreadable_call(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-default (stream object) ...) -> t */
static int syscall_write_default(Execute ptr, addr stream, addr var)
{
	Return(write_default_syscode(ptr, stream, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_write_default(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StreamDesigner);
	GetTypeTable(&values, T);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_write_default(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_WRITE_DEFAULT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_write_default);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_write_default(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun symbol-deftype (symbol) ...) -> (or null function) */
static int syscall_symbol_deftype(Execute ptr, addr var)
{
	symbol_deftype_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_symbol_deftype(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeTable(&values, FunctionNull);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_symbol_deftype(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SYMBOL_DEFTYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_symbol_deftype);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_symbol_deftype(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun delete-deftype (symbol) ...) -> boolean */
static int syscall_delete_deftype(Execute ptr, addr var)
{
	delete_deftype_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_delete_deftype(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_delete_deftype(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DELETE_DEFTYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_delete_deftype);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_delete_deftype(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ensure-structure (symbol list &rest args &key &allow-other-keys) ...)
 *   -> symbol
 */
static int syscall_ensure_structure(Execute ptr, addr name, addr slots, addr rest)
{
	Return(ensure_structure_syscode_(ptr, name, slots, rest));
	setresult_control(ptr, name);
	return 0;
}

static void type_ensure_structure(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, List);
	GetTypeTable(&type, T);
	typeargs_var2rest(&args, args, values, type);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_structure(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ENSURE_STRUCTURE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_ensure_structure);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_ensure_structure(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun structure-constructor (symbol &rest t &key &other-allow-keys) ...)
 *   -> structure-object
 */
static int syscall_structure_constructor(Execute ptr, addr symbol, addr rest)
{
	Return(structure_constructor_syscode(ptr, symbol, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void type_structure_constructor(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	GetTypeTable(&values, StructureObject);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_constructor(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_STRUCTURE_CONSTRUCTOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_structure_constructor);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_structure_constructor(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun loop-bind (tree type value) ...) -> tree */
static int syscall_loop_bind(Execute ptr, addr a, addr b, addr c)
{
	Return(loop_bind_syscode(ptr, a, b, c, &a));
	setresult_control(ptr, a);
	return 0;
}

static void type_loop_bind(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var3(&args, args, args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_loop_bind(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_LOOP_BIND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_syscall_loop_bind);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_loop_bind(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-pprint-stream (stream object
 *     prefix per-line-prefix suffix) ...) -> result
 *   stream           stream
 *   object           t
 *   prefix           string
 *   per-line-prefix  string
 *   suffix           string
 *   result           stream-pretty
 */
static int syscall_make_pprint_stream(Execute ptr,
		addr stream, addr object, addr prefix, addr perline, addr suffix)
{
	Return(make_pprint_stream_syscode_(ptr,
				&stream, stream, object, prefix, perline, suffix));
	setresult_control(ptr, stream);
	return 0;
}

static void type_syscall_make_pprint_stream(addr *ret)
{
	addr args, values, type1, type2, type3;

	GetTypeTable(&type1, Stream);
	GetTypeTable(&type2, T);
	GetTypeTable(&type3, StringNull);
	typeargs_var5(&args, type1, type2, type3, type3, type3);
	GetTypeValues(&values, PrettyStream);
	type_compiled_heap(args, values, ret);
}

static void defun_make_pprint_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_PPRINT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var5(pos, p_defun_syscall_make_pprint_stream);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_make_pprint_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-gensym (stream-pretty) ...) -> symbol */
static int syscall_pprint_gensym(Execute ptr, addr stream)
{
	Return(pprint_gensym_syscode(stream, &stream));
	setresult_control(ptr, stream);
	return 0;
}

static void type_syscall_pprint_gensym(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrettyStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_gensym(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PPRINT_GENSYM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_pprint_gensym);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_gensym(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-exit (stream-pretty) ...) -> null */
static int syscall_pprint_exit(Execute ptr, addr stream)
{
	Return(pprint_exit_syscode(ptr, stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_pprint_exit(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrettyStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_exit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PPRINT_EXIT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_pprint_exit);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_exit(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-pop (stream-pretty) ...) -> t */
static int syscall_pprint_pop(Execute ptr, addr stream)
{
	Return(pprint_pop_syscode(ptr, stream, &stream));
	setresult_control(ptr, stream);
	return 0;
}

static void type_syscall_pprint_pop(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrettyStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_pop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PPRINT_POP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_pprint_pop);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_pop(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-check (stream-pretty) ...) -> nil */
static int syscall_pprint_check(Execute ptr, addr stream)
{
	Return(pprint_check_syscode(ptr, stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_pprint_check(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrettyStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_check(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PPRINT_CHECK, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_pprint_check);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_check(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-close (stream-pretty) ...) -> nil */
static int syscall_pprint_close(Execute ptr, addr stream)
{
	Return(pprint_close_syscode(ptr, stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_pprint_close(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrettyStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_close(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PPRINT_CLOSE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_pprint_close);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_close(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-pretty (stream-pretty) ...) -> nil */
static int syscall_pprint_pretty(Execute ptr, addr stream, addr call)
{
	Return(pprint_pretty_syscode(ptr, stream, call));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_pprint_pretty(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrettyStream);
	GetTypeTable(&values, Function);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_pretty(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PPRINT_PRETTY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_pprint_pretty);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_pretty(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun timeinfo () ...) -> (values intplus intplus intplus intplus) */
static int syscall_timeinfo(Execute ptr)
{
	addr real, run, size, count;

	Return(timeinfo_syscode_(ptr->local, &real, &run, &size, &count));
	setvalues_control(ptr, real, run, size, count, NULL);

	return 0;
}

static void type_syscall_timeinfo(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, Empty);
	GetTypeTable(&values, Intplus);
	typevalues_values_va(&values, values, values, values, values, NULL);
	type_compiled_heap(args, values, ret);
}

static void defun_timeinfo(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_TIMEINFO, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_syscall_timeinfo);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_timeinfo(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defvar *ed-function* (lambda (file) ... null))
 *    file    (or null string)
 */
static int syscall_ed_function(Execute ptr, addr file)
{
	Return(ed_function_syscode_(ptr, file));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_ed_function(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringNull);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defvar_ed_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ED_FUNCTION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_ed_function);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_ed_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
	/* (defvar *ed-function* [function]) */
	SetValueSymbol(symbol, pos);
}


/* (defun trace-add (list) ...) -> list */
static int syscall_trace_add(Execute ptr, addr var)
{
	Return(trace_add_syscode_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_trace_add(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_trace_add(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_TRACE_ADD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_trace_add);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_trace_add(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun trace-del (list-or-t) ...) -> list */
static int syscall_trace_del(Execute ptr, addr var)
{
	Return(trace_del_syscode_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_trace_del(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	GetTypeTable(&values, EqlT);
	type2or_heap(args, values, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_trace_del(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_TRACE_DEL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_trace_del);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_trace_del(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun with-compilation-unit (override args lambda) ...) -> any */
static void type_syscall_with_compilation_unit(addr *ret)
{
	addr args, values, type1, type2, type3;

	GetTypeTable(&type1, T);
	GetTypeTable(&type2, List);
	GetTypeTable(&type3, Function);
	typeargs_var3(&args, type1, type2, type3);
	GetTypeTable(&values, T);
	typevalues_rest(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_with_compilation_unit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_WITH_COMPILATION_UNIT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_syscall_with_compilation_unit);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_with_compilation_unit(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun set-slots (instance slots values) ...) -> t */
static int syscall_set_slots(Execute ptr, addr var, addr slots, addr values)
{
	Return(set_slots_syscode(var, slots, values));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_set_slots(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var3(&args, args, values, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_set_slots(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SET_SLOTS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_syscall_set_slots);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_set_slots(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun intern-eql-specializer (instance slots values) ...) -> t */
static int syscall_intern_eql_specializer(Execute ptr, addr var)
{
	Return(intern_eql_specializer_syscode(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_intern_eql_specializer(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_intern_eql_specializer(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_INTERN_EQL_SPECIALIZER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_intern_eql_specializer);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_intern_eql_specializer(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_syscall_common(void)
{
	SetPointerSysCall(defun, var2, redirect_restart);
	SetPointerSysCall(defun, var2, define_symbol_macro);
	SetPointerSysCall(defmacro, macro, symbol_macro_expander);
	SetPointerSysCall(defun, var3, defconstant);
	SetPointerSysCall(defun, var1, in_package);
	SetPointerSysCall(defun, var3, setplist);
	SetPointerSysCall(defun, var2, remplist);
	SetPointerSysCall(defun, var1, make_hash_iterator);
	SetPointerSysCall(defun, var1, next_hash_iterator);
	SetPointerSysCall(defun, var4, make_package_iterator);
	SetPointerSysCall(defun, var1, next_package_iterator);
	SetPointerSysCall(defun, var1dynamic, defpackage);
	SetPointerSysCall(defun, var2, do_symbols);
	SetPointerSysCall(defun, var2, do_external_symbols);
	SetPointerSysCall(defun, var1, do_all_symbols);
	SetPointerSysCall(defun, var1, getdoc_variable);
	SetPointerSysCall(defun, var2, setdoc_variable);
	SetPointerSysCall(defun, var2, ecase_error);
	SetPointerSysCall(defun, var2, etypecase_error);
	SetPointerSysCall(defun, var2, define_setf_expander);
	SetPointerSysCall(defun, var1, end_input_stream);
	SetPointerSysCall(defun, var1dynamic, make_extend_output_stream);
	SetPointerSysCall(defun, var1dynamic, prompt_for);
	SetPointerSysCall(defun, var5, print_unreadable_call);
	SetPointerSysCall(defun, var2, write_default);
	SetPointerSysCall(defun, var1, symbol_deftype);
	SetPointerSysCall(defun, var1, delete_deftype);
	SetPointerSysCall(defun, var2dynamic, ensure_structure);
	SetPointerSysCall(defun, var1dynamic, structure_constructor);
	SetPointerSysCall(defun, var3, loop_bind);
	SetPointerSysCall(defun, var5, make_pprint_stream);
	SetPointerSysCall(defun, var1, pprint_gensym);
	SetPointerSysCall(defun, var1, pprint_exit);
	SetPointerSysCall(defun, var1, pprint_pop);
	SetPointerSysCall(defun, var1, pprint_check);
	SetPointerSysCall(defun, var1, pprint_close);
	SetPointerSysCall(defun, var2, pprint_pretty);
	SetPointerSysCall(defun, empty, timeinfo);
	SetPointerSysCall(defun, var1, ed_function);
	SetPointerSysCall(defun, var1, trace_add);
	SetPointerSysCall(defun, var1, trace_del);
	SetPointerSysCall(defun, var3, with_compilation_unit);
	SetPointerSysCall(defun, var3, set_slots);
	SetPointerSysCall(defun, var1, intern_eql_specializer);
}

void build_syscall_common(void)
{
	defun_redirect_restart();
	defun_define_symbol_macro();
	defun_symbol_macro_expander();
	defun_defconstant();
	defun_in_package();
	defun_setplist();
	defun_remplist();
	defun_make_hash_iterator();
	defun_next_hash_iterator();
	defun_make_package_iterator();
	defun_next_package_iterator();
	defun_defpackage();
	defun_do_symbols();
	defun_do_external_symbols();
	defun_do_all_symbols();
	defun_getdoc_variable();
	defun_setdoc_variable();
	defun_ecase_error();
	defun_etypecase_error();
	defun_define_setf_expander();
	defun_end_input_stream();
	defun_make_extend_output_stream();
	defun_prompt_for();
	defun_print_unreadable_call();
	defun_write_default();
	defun_symbol_deftype();
	defun_delete_deftype();
	defun_ensure_structure();
	defun_structure_constructor();
	defun_loop_bind();
	defun_make_pprint_stream();
	defun_pprint_gensym();
	defun_pprint_exit();
	defun_pprint_pop();
	defun_pprint_check();
	defun_pprint_close();
	defun_pprint_pretty();
	defun_timeinfo();
	defvar_ed_function();
	defun_trace_add();
	defun_trace_del();
	defun_with_compilation_unit();
	defun_set_slots();
	defun_intern_eql_specializer();
}


/************************************************************
 *  syscall_function.c
 ************************************************************/

/* (defun hello () ...) -> null */
static int syscall_hello(Execute ptr)
{
	Return(hello_syscode(ptr));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_hello(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_HELLO, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_syscall_hello);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeTable(&type, CompiledFunction);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun infobit (&rest args) ...) -> object */
static int syscall_infobit(Execute ptr, addr rest)
{
	infobit_syscode(rest, &rest);
	setresult_control(ptr, rest);
	return 0;
}

static void defun_infobit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_INFOBIT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_infobit);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InfoBit);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun infoprint (&rest args) ...) -> object */
static int syscall_infoprint(Execute ptr, addr rest)
{
	infoprint_syscode(rest, &rest);
	setresult_control(ptr, rest);
	return 0;
}

static void defun_infoprint(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_INFOPRINT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_infoprint);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InfoBit);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun gc (&key full) ...) -> null */
static int syscall_gc(Execute ptr, addr rest)
{
	gc_syscode(rest);
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_gc(addr *ret)
{
	addr args, values;

	/* key */
	KeyTypeTable(&args, FULL, T);
	list_heap(&args, args, NULL);
	/* type */
	typeargs_key(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_gc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_GC, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_gc);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_gc(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun savecore (pathname-designer) ...) -> null */
static int syscall_savecore(Execute ptr, addr file)
{
	Return(savecore_syscode(ptr, file));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_savecore(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesigner);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_savecore(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SAVECORE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_savecore);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_savecore(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-export-list (package-designer) ...) -> list */
static int syscall_package_export_list(Execute ptr, addr var)
{
	Return(package_export_list_syscode_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_package_export_list(addr *ret)
{
	/* (function (package_designer) (values list &rest nil)) */
	addr args, values;

	GetTypeTable(&args, PackageDesigner);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_package_export_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PACKAGE_EXPORT_LIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_package_export_list);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_package_export_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun specialp (symbol) ...) -> boolean */
static int syscall_specialp(Execute ptr, addr var)
{
	specialp_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_specialp(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_specialp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SPECIALP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_specialp);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_specialp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-general-p (object) ...) -> boolean */
static int syscall_array_general_p(Execute ptr, addr var)
{
	array_general_p_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_array_general_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ARRAY_GENERAL_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_array_general_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-specialized-p (object) ...) -> boolean */
static int syscall_array_specialized_p(Execute ptr, addr var)
{
	array_specialized_p_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_array_specialized_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ARRAY_SPECIALIZED_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_array_specialized_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun simple-sort (sequence call &key key) ...) -> sequence */
static int syscall_simple_sort(Execute ptr, addr pos, addr call, addr rest)
{
	Return(simple_sort_syscode(ptr, pos, call, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_simple_sort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SIMPLE_SORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_simple_sort);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bubble-sort (sequence call &key key) ...) -> sequence */
static int syscall_bubble_sort(Execute ptr, addr pos, addr call, addr rest)
{
	Return(bubble_sort_syscode(ptr, pos, call, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_bubble_sort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BUBBLE_SORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_bubble_sort);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun quick-sort (sequence call &key key) ...) -> sequence */
static int syscall_quick_sort(Execute ptr, addr pos, addr call, addr rest)
{
	Return(quick_sort_syscode(ptr, pos, call, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_quick_sort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_QUICK_SORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_quick_sort);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun merge-sort (sequence call &key key) ...) -> sequence */
static int syscall_merge_sort(Execute ptr, addr pos, addr call, addr rest)
{
	Return(merge_sort_syscode(ptr, pos, call, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_merge_sort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MERGE_SORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_merge_sort);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun exit/quit (&optional code) ...) -> (values &rest nil) */
static int syscall_exit(Execute ptr, addr code)
{
	Return(exit_syscode_(ptr, code));
	setvalues_nil_control(ptr);
	return 0;
}

static void defun_exit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EXIT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_syscall_exit);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Exit);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static void defun_quit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_QUIT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_syscall_exit);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Exit);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun closp (object) ...) -> boolean */
static int syscall_closp(Execute ptr, addr var)
{
	closp_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_closp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_CLOSP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_closp);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun fixnump (object) ...) -> boolean */
static int syscall_fixnump(Execute ptr, addr var)
{
	fixnump_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_fixnump(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_FIXNUMP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_fixnump);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bignump (object) ...) -> boolean */
static int syscall_bignump(Execute ptr, addr var)
{
	bignump_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_bignump(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BIGNUMP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_bignump);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ratiop (object) ...) -> boolean */
static int syscall_ratiop(Execute ptr, addr var)
{
	ratiop_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_ratiop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_RATIOP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_ratiop);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun short-float-p (object) ...) -> boolean */
static int syscall_short_float_p(Execute ptr, addr var)
{
	short_float_p_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_short_float_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SHORT_FLOAT_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_short_float_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun single-float-p (object) ...) -> boolean */
static int syscall_single_float_p(Execute ptr, addr var)
{
	single_float_p_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_single_float_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SINGLE_FLOAT_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_single_float_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun double-float-p (object) ...) -> boolean */
static int syscall_double_float_p(Execute ptr, addr var)
{
	double_float_p_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_double_float_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DOUBLE_FLOAT_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_double_float_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun long-float-p (object) ...) -> boolean */
static int syscall_long_float_p(Execute ptr, addr var)
{
	long_float_p_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_long_float_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_LONG_FLOAT_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_long_float_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun callnamep (object) ...) -> boolean */
static int syscall_callnamep(Execute ptr, addr var)
{
	callnamep_syscall(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_callnamep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_CALLNAMEP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_callnamep);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun large-number (value &optional (cardinal t)) ...) -> string
 *   value  (integer 0 fixnum-max)
 */
static int syscall_large_number(Execute ptr, addr var, addr opt)
{
	Return(large_number_syscode_(ptr->local, var, opt, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_large_number(addr *ret)
{
	addr args, values;

	type4integer_heap(Nil, 0, Nil, FIXNUM_MAX, &args);
	GetTypeTable(&values, T);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, ret);
}

static void defun_large_number(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_LARGE_NUMBER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_syscall_large_number);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_large_number(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-character (x) ...) -> character
 *   x:  (or character integer)
 */
static int syscall_make_character(Execute ptr, addr var)
{
	Return(make_character_syscode(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_make_character(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Character);
	GetTypeTable(&values, Integer);
	type2or_heap(args, values, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Character);
	type_compiled_heap(args, values, ret);
}

static void defun_make_character(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_CHARACTER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_make_character);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-fixnum (integer) ...) -> fixnum */
static int syscall_make_fixnum(Execute ptr, addr var)
{
	Return(make_fixnum_syscode(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_make_fixnum(addr *ret)
{
	addr args, values;

	GetTypeTable(&values, Fixnum);
	typeargs_var1(&args, values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_fixnum(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_FIXNUM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_make_fixnum);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_fixnum(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-bignum (integer) ...) -> bignum */
static int syscall_make_bignum(Execute ptr, addr var)
{
	Return(make_bignum_syscode(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_make_bignum(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Integer);
	type_compiled_heap(args, values, ret);
}

static void defun_make_bignum(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_BIGNUM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_make_bignum);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_bignum(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-ratio (numer denom) ...) -> ratio */
static int syscall_make_ratio(Execute ptr, addr numer, addr denom)
{
	Return(make_ratio_syscode(numer, denom, &numer));
	setresult_control(ptr, numer);
	return 0;
}

static void type_make_ratio(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Rational);
	type_compiled_heap(args, values, ret);
}

static void defun_make_ratio(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_RATIO, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_make_ratio);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_ratio(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-complex (real imag) ...) -> complex */
static int syscall_make_complex(Execute ptr, addr real, addr imag)
{
	Return(make_complex_code_(real, imag, &real));
	setresult_control(ptr, real);
	return 0;
}

static void type_make_complex(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Real);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Complex);
	type_compiled_heap(args, values, ret);
}

static void defun_make_complex(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_COMPLEX, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_make_complex);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_complex(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun equal-random-state (a b) ...) -> boolean */
static int syscall_equal_random_state(Execute ptr, addr left, addr right)
{
	equal_random_state_syscode(left, right, &left);
	setresult_control(ptr, left);
	return 0;
}

static void type_equal_random_state(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, RandomState);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_equal_random_state(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EQUAL_RANDOM_STATE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_equal_random_state);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_equal_random_state(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subtypep! (x y &optional env check) ...) -> symbol */
static int syscall_subtypep_extend(Execute ptr, addr x, addr y, addr env, addr check)
{
	Return(subtypep_extend_syscode_(ptr, x, y, env, check, &x));
	setresult_control(ptr, x);
	return 0;
}

static void type_syscall_subtypep_extend(addr *ret)
{
	addr args, values, env;

	GetTypeTable(&args, TypeSpec);
	GetTypeTable(&env, EnvironmentNull);
	GetTypeTable(&values, T);
	typeargs_var2opt2(&args, args, args, env, values);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_subtypep_extend(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SUBTYPEP_EXTEND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt2(pos, p_defun_syscall_subtypep_extend);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_subtypep_extend(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subtypep-number (x) ...) -> type-specifier */
static int syscall_subtypep_number(Execute ptr, addr x)
{
	Return(subtypep_number_syscode_(ptr, x, &x));
	setresult_control(ptr, x);
	return 0;
}

static void type_syscall_subtypep_number(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, TypeSpec);
	typeargs_var1(&args, args);
	GetTypeTable(&values, TypeSpec);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_subtypep_number(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SUBTYPEP_NUMBER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_subtypep_number);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_subtypep_number(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eastasian-set (string-designer intplus &optional error) ...) -> boolean) */
static int syscall_eastasian_set(Execute ptr, addr var, addr value, addr errorp)
{
	Return(eastasian_set_syscode_(var, value, errorp, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_eastasian_set(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, StringDesigner);
	GetTypeTable(&values, Intplus);
	GetTypeTable(&type, T);
	typeargs_var3(&args, args, values, type);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_eastasian_set(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EASTASIAN_SET, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_syscall_eastasian_set);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_eastasian_set(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eastasian-get (string-designer) ...) -> (values IntplusNull symbol) */
static int syscall_eastasian_get(Execute ptr, addr var)
{
	addr symbol;

	Return(eastasian_get_syscode_(var, &var, &symbol));
	setvalues_control(ptr, var, symbol, NULL);

	return 0;
}

static void type_syscall_eastasian_get(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, StringDesigner);
	typeargs_var1(&args, args);
	GetTypeTable(&values, IntplusNull);
	GetTypeTable(&type, Symbol);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_eastasian_get(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EASTASIAN_GET, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_eastasian_get);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_eastasian_get(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eastasian-width (var) ...) -> (values IntplusNull boolean)
 *   var  (or integer character string)
 */
static int syscall_eastasian_width(Execute ptr, addr pos)
{
	addr value;

	Return(eastasian_width_syscode_(pos, &pos, &value));
	setvalues_control(ptr, pos, value, NULL);

	return 0;
}

static void type_syscall_eastasian_width(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Integer);
	GetTypeTable(&values, Character);
	GetTypeTable(&type, String);
	type3or_heap(args, values, type, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, ret);
}

static void defun_eastasian_width(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EASTASIAN_WIDTH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_eastasian_width);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_eastasian_width(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun run-program (program args &rest args) ...) -> t */
static int syscall_run_program(Execute ptr, addr var, addr args, addr rest)
{
	Return(run_program_syscode_(ptr, var, args, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_run_program(addr *ret)
{
	addr args, values, rest;

	GetTypeTable(&args, String);
	GetTypeTable(&values, List);
	GetTypeTable(&rest, T);
	typeargs_var2rest(&args, args, values, rest);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_run_program(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_RUN_PROGRAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_run_program);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_run_program(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-callname (var) ...) -> callname */
static int syscall_make_callname(Execute ptr, addr var)
{
	Return(make_callname_syscode_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_make_callname(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, FunctionName);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_make_callname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_CALLNAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_make_callname);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_make_callname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun remove-file (pathname &optional (error t)) ...) -> boolean */
static int syscall_remove_file(Execute ptr, addr var, addr opt)
{
	Return(remove_file_syscode(ptr, var, opt, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_remove_file(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_REMOVE_FILE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_syscall_remove_file);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, RemoveFile);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun remove-directory (pathname &optional (error t)) ...) -> boolean */
static int syscall_remove_directory(Execute ptr, addr var, addr opt)
{
	Return(remove_directory_syscode(ptr, var, opt, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_remove_directory(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_REMOVE_DIRECTORY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_syscall_remove_directory);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, RemoveFile);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro declare-parse (symbol) ...) -> integer */
static int syscall_declare_parse(Execute ptr, addr form, addr env)
{
	Return(declare_parse_syscode(form, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_declare_parse(void)
{
	addr symbol, pos, type;

	GetConst(SYSTEM_DECLARE_PARSE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_syscall_declare_parse);
	setmacro_symbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun parse-type (object) ...) -> type */
static int syscall_parse_type(Execute ptr, addr var)
{
	Return(parse_type_syscode(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_parse_type(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeTable(&values, TypeSpec);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_parse_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PARSE_TYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_parse_type);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_parse_type(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun upgraded-open-element-type (type) ...) -> type */
static int syscall_upgraded_open_element_type(Execute ptr, addr var)
{
	Return(upgraded_open_element_type_syscode_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_upgraded_open_element_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_UPGRADED_OPEN_ELEMENT_TYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_upgraded_open_element_type);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, UpgradedType);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-memory-input-stream (sequence &key size array cache) ...) -> stream
 *   sequence  sequence
 *   size      (or null (integer 1 *))
 *   array     (or null (integer 1 *))
 *   cache     t  ;; boolean
 *   stream    input-memory-stream
 */
static int syscall_make_memory_input_stream(Execute ptr, addr var, addr rest)
{
	Return(make_memory_input_stream_syscode_(var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_make_memory_input_stream(addr *ret)
{
	addr args, values, key1, key2, key3, key;

	/* key */
	KeyTypeTable(&key1, SIZE, Plus1Null);
	KeyTypeTable(&key2, ARRAY, Plus1Null);
	KeyTypeTable(&key3, CACHE, T);
	list_heap(&key, key1, key2, key3, NULL);
	/* type */
	GetTypeTable(&args, Sequence);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, MemoryStream);
	type_compiled_heap(args, values, ret);
}

static void defun_make_memory_input_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_MEMORY_INPUT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_make_memory_input_stream);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_make_memory_input_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-memroy-output-stream (&key input size array cache) ...) -> stream
 *   input     sequence
 *   size      (or null (integer 1 *))
 *   array     (or null (integer 1 *))
 *   cache     t  ;; boolean
 *   stream    output-memory-stream
 */
static int syscall_make_memory_output_stream(Execute ptr, addr rest)
{
	Return(make_memory_output_stream_syscode_(rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_make_memory_output_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_MEMORY_OUTPUT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_make_memory_output_stream);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MakeMemoryOutputStream);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-memroy-io-stream (&key input size array cache) ...) -> stream
 *   input     sequence
 *   size      (or null (integer 1 *))
 *   array     (or null (integer 1 *))
 *   cache     t  ;; boolean
 *   stream    io-memory-stream
 */
static int syscall_make_memory_io_stream(Execute ptr, addr rest)
{
	Return(make_memory_io_stream_syscode_(rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_make_memory_io_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_MEMORY_IO_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_make_memory_io_stream);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MakeMemoryOutputStream);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-input-from-memory
 *   ((stream vector &key size array) declaration* form*) ...)
 *   -> result
 */
static int syscall_with_input_from_memory(Execute ptr, addr form, addr env)
{
	Return(with_input_from_memory_syscode_(ptr, form, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_input_from_memory(void)
{
	addr symbol, pos, type;

	GetConst(SYSTEM_WITH_INPUT_FROM_MEMORY, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_syscall_with_input_from_memory);
	setmacro_symbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro with-output-to-memory
 *   ((var &key input size array) declaration* form*) ...)
 *   -> result
 */
static int syscall_with_output_to_memory(Execute ptr, addr form, addr env)
{
	Return(with_output_to_memory_syscode_(ptr, form, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_output_to_memory(void)
{
	addr symbol, pos, type;

	GetConst(SYSTEM_WITH_OUTPUT_TO_MEMORY, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_syscall_with_output_to_memory);
	setmacro_symbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun get-output-stream-memory (stream) ...) -> vector */
static int syscall_get_output_stream_memory(Execute ptr, addr var)
{
	Return(get_output_stream_memory_syscode_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_get_output_stream_memory(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, MemoryStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Vector);
	type_compiled_heap(args, values, ret);
}

static void defun_get_output_stream_memory(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_GET_OUTPUT_STREAM_MEMORY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_get_output_stream_memory);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_get_output_stream_memory(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun memory-stream-p (t) ...) -> (member :input :output :io nil) */
static int syscall_memory_stream_p(Execute ptr, addr var)
{
	memory_stream_p_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_memory_stream_p(addr *ret)
{
	addr args, values, type1, type2, type3;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetConst(KEYWORD_INPUT, &type1);
	GetConst(KEYWORD_OUTPUT, &type2);
	GetConst(KEYWORD_IO, &type3);
	type_member_heap(&values, type1, type2, type3, Nil, NULL);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_memory_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MEMORY_STREAM_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_memory_stream_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_memory_stream_p(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf memory-stream-p) (result stream) ...) -> result
 *   stream  memory-stream
 *   result  (member :input :output :io)
 */
static int syscall_setf_memory_stream_p(Execute ptr, addr value, addr var)
{
	Return(setf_memory_stream_p_syscode_(var, value));
	setresult_control(ptr, value);
	return 0;
}

static void type_syscall_setf_memory_stream_p(addr *ret)
{
	addr args, values, type1, type2, type3;

	GetTypeTable(&args, MemoryStream);
	GetConst(KEYWORD_INPUT, &type1);
	GetConst(KEYWORD_OUTPUT, &type2);
	GetConst(KEYWORD_IO, &type3);
	type_member_heap(&values, type1, type2, type3, NULL);
	typeargs_var2(&args, values, args);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_memory_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MEMORY_STREAM_P, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_setf_memory_stream_p);
	setsetf_symbol(symbol, pos);
	/* type */
	type_syscall_setf_memory_stream_p(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun byte-integer (&rest unsigned-byte-8) ...) -> (integer 0 *) */
static int syscall_byte_integer(Execute ptr, addr list)
{
	Return(byte_integer_syscode_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void type_syscall_byte_integer(addr *ret)
{
	addr args, values;

	type4integer_heap(Nil, 0, Nil, 0xFF, &args);
	typeargs_rest(&args, args);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, ret);
}

static void defun_byte_integer(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BYTE_INTEGER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_byte_integer);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_byte_integer(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun question (object &rest args) ...) -> (values &rest t) */
static int syscall_question(Execute ptr, addr var, addr args)
{
	return question_syscode_(ptr, var, args);
}

static void type_syscall_question(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&type, T);
	typeargs_var1rest(&args, type, type);
	typevalues_rest(&values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_question(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_QUESTION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_question);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_question(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun extension (t) ...) -> t */
static int syscall_extension(Execute ptr, addr var)
{
	return extension_syscode(ptr, var);
}

static void type_syscall_extension(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_extension(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EXTENSION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_extension);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_extension(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_syscall_function(void)
{
	SetPointerSysCall(defun, empty, hello);
	SetPointerSysCall(defun, dynamic, infobit);
	SetPointerSysCall(defun, dynamic, infoprint);
	SetPointerSysCall(defun, dynamic, gc);
	SetPointerSysCall(defun, var1, savecore);
	SetPointerSysCall(defun, var1, package_export_list);
	SetPointerSysCall(defun, var1, specialp);
	SetPointerSysCall(defun, var1, array_general_p);
	SetPointerSysCall(defun, var1, array_specialized_p);
	SetPointerSysCall(defun, var2dynamic, simple_sort);
	SetPointerSysCall(defun, var2dynamic, bubble_sort);
	SetPointerSysCall(defun, var2dynamic, quick_sort);
	SetPointerSysCall(defun, var2dynamic, merge_sort);
	SetPointerSysCall(defun, opt1, exit);
	SetPointerSysCall(defun, var1, closp);
	SetPointerSysCall(defun, var1, fixnump);
	SetPointerSysCall(defun, var1, bignump);
	SetPointerSysCall(defun, var1, ratiop);
	SetPointerSysCall(defun, var1, callnamep);
	SetPointerSysCall(defun, var1, short_float_p);
	SetPointerSysCall(defun, var1, single_float_p);
	SetPointerSysCall(defun, var1, double_float_p);
	SetPointerSysCall(defun, var1, long_float_p);
	SetPointerSysCall(defun, var1opt1, large_number);
	SetPointerSysCall(defun, var1, make_character);
	SetPointerSysCall(defun, var1, make_fixnum);
	SetPointerSysCall(defun, var1, make_bignum);
	SetPointerSysCall(defun, var2, make_ratio);
	SetPointerSysCall(defun, var2, make_complex);
	SetPointerSysCall(defun, var2, equal_random_state);
	SetPointerSysCall(defun, var2opt2, subtypep_extend);
	SetPointerSysCall(defun, var1, subtypep_number);
	SetPointerSysCall(defun, var3, eastasian_set);
	SetPointerSysCall(defun, var1, eastasian_get);
	SetPointerSysCall(defun, var1, eastasian_width);
	SetPointerSysCall(defun, var2dynamic, run_program);
	SetPointerSysCall(defun, var1, make_callname);
	SetPointerSysCall(defun, var1opt1, remove_file);
	SetPointerSysCall(defun, var1opt1, remove_directory);
	SetPointerSysCall(defmacro, macro, declare_parse);
	SetPointerSysCall(defun, var1, parse_type);
	SetPointerSysCall(defun, var1, upgraded_open_element_type);
	SetPointerSysCall(defun, var1dynamic, make_memory_input_stream);
	SetPointerSysCall(defun, dynamic, make_memory_output_stream);
	SetPointerSysCall(defun, dynamic, make_memory_io_stream);
	SetPointerSysCall(defmacro, macro, with_input_from_memory);
	SetPointerSysCall(defmacro, macro, with_output_to_memory);
	SetPointerSysCall(defun, var1, get_output_stream_memory);
	SetPointerSysCall(defun, var1, memory_stream_p);
	SetPointerSysCall(defun, var2, setf_memory_stream_p);
	SetPointerSysCall(defun, dynamic, byte_integer);
	SetPointerSysCall(defun, var1dynamic, question);
	SetPointerSysCall(defun, var1, extension);
}

void build_syscall_function(void)
{
	defun_hello();
	defun_infobit();
	defun_infoprint();
	defun_gc();
	defun_savecore();
	defun_package_export_list();
	defun_specialp();
	defun_array_general_p();
	defun_array_specialized_p();
	defun_simple_sort();
	defun_bubble_sort();
	defun_quick_sort();
	defun_merge_sort();
	defun_exit();
	defun_quit();
	defun_closp();
	defun_fixnump();
	defun_bignump();
	defun_ratiop();
	defun_short_float_p();
	defun_single_float_p();
	defun_double_float_p();
	defun_long_float_p();
	defun_callnamep();
	defun_large_number();
	defun_make_character();
	defun_make_fixnum();
	defun_make_bignum();
	defun_make_ratio();
	defun_make_complex();
	defun_equal_random_state();
	defun_subtypep_extend();
	defun_subtypep_number();
	defun_eastasian_set();
	defun_eastasian_get();
	defun_eastasian_width();
	defun_run_program();
	defun_make_callname();
	defun_remove_file();
	defun_remove_directory();
	defmacro_declare_parse();
	defun_parse_type();
	defun_upgraded_open_element_type();
	defun_make_memory_input_stream();
	defun_make_memory_output_stream();
	defun_make_memory_io_stream();
	defmacro_with_input_from_memory();
	defmacro_with_output_to_memory();
	defun_get_output_stream_memory();
	defun_memory_stream_p();
	defun_setf_memory_stream_p();
	defun_byte_integer();
	defun_question();
	defun_extension();
}


/************************************************************
 *  syscode_common.c
 ************************************************************/

/* redirect-restart */
int redirect_restart_syscode(Execute ptr, addr condition, addr list)
{
	addr pos;

	Check(! conditionp_debug(condition), "type error");
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (GetType(pos) != LISPTYPE_RESTART)
			return fmte_("The argument ~S must be a restart.", pos, NULL);
		pushbind_restart_control(ptr, pos, 0);
	}
	reverse_restart_control(ptr);

	return 0;
}


/* defconstant */
int defconstant_syscode(addr symbol, addr value, addr doc)
{
	addr check;

	Check(! symbolp(symbol), "type symbol error");
	Check(doc != Nil && (! stringp(doc)), "type documentation error");
	GetValueSymbol(symbol, &check);
	if (check != Unbound && (! eql_function(check, value)))
		return fmte_("The defconstant cannot setq ~S value.", symbol, NULL);
	ResetStatusReadOnly(symbol);
	SetValueSymbol(symbol, value);
	setdocument_variable_symbol(symbol, doc);
	setspecial_symbol(symbol);
	SetStatusReadOnly(symbol);

	return 0;
}


/* in-package */
int in_package_syscode_(Execute ptr, addr name, addr *ret)
{
	return in_package_(ptr, name, ret);
}


/* setplist */
void setplist_syscode(addr key, addr value, addr list, addr *ret)
{
	setplist_heap_safe(list, key, value, ret);
}


/* remplist */
int remplist_syscode_(addr key, addr list, addr *ret1, addr *ret2)
{
	enum RemPlist check;

	Return(remplist_safe_(list, key, &list, &check));
	*ret1 = list;
	*ret2 = (check != RemPlist_NotFound)? T: Nil;
	return 0;
}


/* make-hash-iterator */
void make_hash_iterator_syscode(addr pos, addr *ret)
{
	hash_iterator_heap(ret, pos);
}


/* next-hash-iterator */
void next_hash_iterator_syscode(addr pos, addr *ret1, addr *ret2, addr *ret3)
{
	int check;
	addr key, value;

	check = next_hash_iterator(pos, &key, &value);
	if (check) {
		*ret1 = T;
		*ret2 = key;
		*ret3 = value;
	}
	else {
		*ret1 = *ret2 = *ret3 = Nil;
	}
}


/* make-package-iterator */
int make_package_iterator_syscode_(addr pos, addr a, addr b, addr c, addr *ret)
{
	return package_iterator_heap_(ret, pos, (a != Nil), (b != Nil), (c != Nil));
}


/* next-package-iterator */
int next_package_iterator_syscode_(Execute ptr, addr pos,
		addr *ret1, addr *ret2, addr *ret3, addr *ret4)
{
	enum PACKAGE_TYPE check;
	addr symbol, status, package;

	Return(next_package_iterator_(pos, &symbol, &package, &check));
	if (check == PACKAGE_TYPE_NIL) {
		*ret1 = *ret2 = *ret3 = *ret4 = Nil;
	}
	else {
		keyword_packagetype(check, &status);
		*ret1 = T;
		*ret2 = symbol;
		*ret3 = status;
		*ret4 = package;
	}

	return 0;
}


/* defpackage */
int defpackage_syscode(Execute ptr, addr var, addr rest, addr *ret)
{
	return defpackage_execute(ptr, var, rest, ret);
}


/* do-symbols */
int do_symbols_syscode(Execute ptr, addr call, addr package)
{
	return do_symbols_package(ptr, call, package);
}


/* do-external-symbols */
int do_external_symbols_syscode(Execute ptr, addr call, addr package)
{
	return do_external_symbols_package(ptr, call, package);
}


/* do-all-symbols */
int do_all_symbols_syscode_(Execute ptr, addr call)
{
	return do_all_symbols_package_(ptr, call);
}


/* getdoc-variable */
void getdoc_variable_syscode(addr var, addr *ret)
{
	getdocument_variable_symbol(var, ret);
}


/* setdoc-variable */
void setdoc_variable_syscode(addr var, addr value)
{
	setdocument_variable_symbol(var, value);
}


/* ecase-error */
int ecase_error_syscode_(Execute ptr, addr value, addr list)
{
	Return(make_vector4_from_list_(&list, list));
	type1_heap(LISPDECL_MEMBER, list, &list);
	return call_type_error_(ptr, value, list);
}


/* etypecase-error */
int etypecase_error_syscode_(Execute ptr, addr value, addr list)
{
	Return(make_vector4_from_list_(&list, list));
	type1_heap(LISPDECL_OR, list, &list);
	return call_type_error_(ptr, value, list);
}


/* define-setf-expander */
int define_setf_expander_syscode_(addr symbol, addr call)
{
	return setsetfmacro_symbol_(symbol, call);
}


/* end-input-stream */
void end_input_stream_syscode(addr var, addr *ret)
{
	size_t size;
	getindex_input_stream(var, &size);
	make_index_integer_heap(ret, size);
}


/* make-extend-output-stream */
void make_extend_output_stream_syscode(addr var, addr rest, addr *ret)
{
	/* ignore rest */
	open_extend_output_stream(ret, var);
}


/* prompt-for */
int prompt_for_syscode(Execute ptr, addr type, addr args, addr *ret)
{
	addr format;
	LocalHold hold;

	if (args == Nil) {
		strvect_char_heap(&format, "Input> ");
	}
	else {
		Return_getcons(args, &format, &args);
		Return(format_string_lisp(ptr, format, args, &format));
	}

	hold = LocalHold_local_push(ptr, format);
	Return(prompt_for_stream(ptr, type, format, &format));
	localhold_end(hold);
	*ret = format;

	return 0;
}


/* print-unreadable-call */
int print_unreadable_call_syscode(Execute ptr,
		addr stream, addr pos, addr type, addr identity, addr body)
{
	int check1, check2;

	check1 = (type != Nil);
	check2 = (identity != Nil);
	return print_unreadable_common_(ptr, stream, pos, check1, check2, body);
}


/* write-default */
static int write_default_syscode_call_(Execute ptr, addr stream, addr var, addr *ret)
{
	LocalHold hold;

	Return(output_stream_designer_(ptr, stream, &stream));
	hold = LocalHold_local_push(ptr, stream);
	Return(write_default_print_(ptr, stream, var));
	localhold_end(hold);

	return Result(ret, var);
}

int write_default_syscode(Execute ptr, addr stream, addr var, addr *ret)
{
	addr control;

	Return(output_stream_designer_(ptr, stream, &stream));
	push_control(ptr, &control);
	(void)write_default_syscode_call_(ptr, stream, var, ret);
	return pop_control_(ptr, control);
}


/* symbol-deftype */
void symbol_deftype_syscode(addr var, addr *ret)
{
	getdeftype_symbol(var, ret);
}


/* delete-deftype */
void delete_deftype_syscode(addr var, addr *ret)
{
	addr check;

	getdeftype_symbol(var, &check);
	if (check == Nil) {
		*ret = Nil;
	}
	else {
		remdeftype_symbol(var);
		*ret = T;
	}
}


/* ensure-structure */
int ensure_structure_syscode_(Execute ptr, addr name, addr slots, addr rest)
{
	return ensure_structure_common_(ptr, name, slots, rest);
}


/* structure-constructor */
int structure_constructor_syscode(Execute ptr, addr symbol, addr rest, addr *ret)
{
	return structure_constructor_common(ptr, symbol, rest, ret);
}


/* loop-bind */
int loop_bind_syscode(Execute ptr, addr a, addr b, addr c, addr *ret)
{
	return loop_bind_common(ptr, a, b, c, ret);
}


/* make-pprint-stream */
int make_pprint_stream_syscode_(Execute ptr, addr *ret,
		addr stream, addr object, addr prefix, addr perline, addr suffix)
{
	return open_pretty_stream_(ptr, ret, stream, object, prefix, perline, suffix);
}


/* pprint-gensym */
int pprint_gensym_syscode(addr stream, addr *ret)
{
	Check(! pretty_stream_p(stream), "type error");
	return gensym_pretty_stream_(stream, ret);
}


/* pprint-exit */
int pprint_exit_syscode(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	return pprint_exit_common(ptr, stream);
}


/* pprint-pop */
int pprint_pop_syscode(Execute ptr, addr stream, addr *ret)
{
	Check(! pretty_stream_p(stream), "type error");
	return pprint_pop_common(ptr, stream, ret);
}


/* pprint-check */
int pprint_check_syscode(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	return check_pretty_stream(ptr, stream);
}


/* pprint-close */
int pprint_close_syscode(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	return close_pretty_stream_(ptr, stream);
}


/* pprint-pretty */
int pprint_pretty_syscode(Execute ptr, addr stream, addr call)
{
	Check(! pretty_stream_p(stream), "type error");
	return call_pretty_stream(ptr, stream, call);
}


/* timeinfo */
int timeinfo_syscode_(LocalRoot local,
		addr *rreal, addr *rrun, addr *rsize, addr *rcount)
{
	Return(get_internal_real_time_common_(local, rreal));
	get_internal_run_time_common(rrun);
	make_index_integer_heap(rsize, get_heap_object());
	make_index_integer_heap(rcount, get_heap_count());

	return 0;
}


/* ed-function */
int ed_function_syscode_(Execute ptr, addr file)
{
	return ed_process_(ptr, file);
}


/* trace-add */
int trace_add_syscode_(Execute ptr, addr var, addr *ret)
{
	return trace_add_common_(ptr, var, ret);
}


/* trace-del */
int trace_del_syscode_(Execute ptr, addr var, addr *ret)
{
	return trace_del_common_(ptr, var, ret);
}


/* set-slots */
int set_slots_syscode(addr var, addr slots, addr values)
{
	return set_slots_syscall(var, slots, values);
}


/* intern-eql-specializer */
int intern_eql_specializer_syscode(addr var, addr *ret)
{
	return clos_intern_specializer_(var, ret);
}


/************************************************************
 *  syscode_function.c
 ************************************************************/

/* hello */
int hello_syscode(Execute ptr)
{
	addr stream;

	Return(standard_output_stream_(ptr, &stream));
	Return(fresh_line_stream_(stream, NULL));
	Return(print_ascii_stream_(stream, "Hello"));
	Return(terpri_stream_(stream));

	return 0;
}


/* infobit */
void infobit_syscode(addr rest, addr *ret)
{
	addr x, y;

	for (y = Nil; rest != Nil; y = x) {
		GetCons(rest, &x, &rest);
		infobit(x);
	}
	*ret = y;
}


/* infoprint */
void infoprint_syscode(addr rest, addr *ret)
{
	addr x, y;

	for (y = Nil; rest != Nil; y = x) {
		GetCons(rest, &x, &rest);
		infoprint(x);
	}
	*ret = y;
}


/* gc */
void gc_syscode(addr rest)
{
	enum GcMode mode;

	if (GetKeyArgs(rest, KEYWORD_FULL, &rest))
		rest = Nil;
	mode = (rest == Nil)? GcMode_Default: GcMode_Full;
	gcstate_execute(mode);
}


/* savecore */
int savecore_syscode(Execute ptr, addr file)
{
	Return(pathname_designer_local_(ptr, file, &file));
	return savecore_execute_(ptr, file);
}


/* package-export-list */
int package_export_list_syscode_(addr var, addr *ret)
{
	Return(package_designer_(var, &var));
	getexport_package_unsafe(var, ret);
	return 0;
}


/* specialp */
void specialp_syscode(addr var, addr *ret)
{
	*ret = specialp_symbol(var)? T: Nil;
}


/* array-general-p */
void array_general_p_syscode(addr var, addr *ret)
{
	*ret = array_general_p(var)? T: Nil;
}


/* array-specialized-p */
void array_specialized_p_syscode(addr var, addr *ret)
{
	*ret = array_specialized_p(var)? T: Nil;
}


/* simple-sort */
int simple_sort_syscode(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key)) key = Nil;
	return simple_sort_sequence_(ptr, pos, call, key);
}


/* bubble-sort */
int bubble_sort_syscode(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key)) key = Nil;
	return bubble_sort_sequence_(ptr, pos, call, key);
}


/* quick-sort */
int quick_sort_syscode(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key)) key = Nil;
	return quick_sort_sequence_(ptr, pos, call, key);
}


/* merge-sort */
int merge_sort_syscode(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key)) key = Nil;
	return merge_sort_sequence_(ptr, pos, call, key);
}


/* exit */
int exit_syscode_(Execute ptr, addr code)
{
	fixnum value;

	/* default 0 */
	if (code == Unbound)
		fixnum_heap(&code, 0);

	/* value */
	if (GetFixnum_signed(code, &value))
		return fmte_("EXIT code ~S must be a integer type.", code, NULL);
	ptr->result = (int)value;

	/* invoke */
	return call_exit_condition_(ptr, code);
}


/* closp */
void closp_syscode(addr var, addr *ret)
{
	*ret = closp(var)? T: Nil;
}


/* fixnump */
void fixnump_syscode(addr var, addr *ret)
{
	*ret = fixnump(var)? T: Nil;
}


/* bignump */
void bignump_syscode(addr var, addr *ret)
{
	*ret = bignump(var)? T: Nil;
}


/* ratiop */
void ratiop_syscode(addr var, addr *ret)
{
	*ret = ratiop(var)? T: Nil;
}


/* short-float-p */
void short_float_p_syscode(addr var, addr *ret)
{
	*ret = (GetType(var) == LISPTYPE_SHORT_FLOAT)? T: Nil;
}


/* single-float-p */
void single_float_p_syscode(addr var, addr *ret)
{
	*ret = single_float_p(var)? T: Nil;
}


/* double-float-p */
void double_float_p_syscode(addr var, addr *ret)
{
	*ret = double_float_p(var)? T: Nil;
}


/* long-float-p */
void long_float_p_syscode(addr var, addr *ret)
{
	*ret = long_float_p(var)? T: Nil;
}


/* callnamep */
void callnamep_syscall(addr var, addr *ret)
{
	*ret = callnamep(var)? T: Nil;
}


/* large-number */
int large_number_syscode_(LocalRoot local, addr var, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = T;
	return english_unit_heap_(local, ret, var, opt != Nil);
}


/* make-character */
int make_character_syscode(addr var, addr *ret)
{
	unicode c;

	if (integerp(var)) {
		Return(getunicode_integer_(var, &c));
		if (isExtendedType(c)) {
			Return(make_extended_char_heap_(ret, c));
		}
		else {
			make_character_heap(ret, c);
		}
		return 0;
	}

	if (characterp(var)) {
		GetCharacter(var, &c);
		make_character_heap(ret, c);
		return 0;
	}

	*ret = Nil;
	return TypeError_(var, CHARACTER);
}


/* make-fixnum */
int make_fixnum_syscode(addr var, addr *ret)
{
	fixnum value;

	switch (GetType(var)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(var, &value);
			make_fixnum_heap(ret, value);
			break;

		default:
			*ret = Nil;
			return TypeError_(var, FIXNUM);
	}

	return 0;
}


/* make-bignum */
int make_bignum_syscode(addr var, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_FIXNUM:
			bignum_fixnum_heap(ret, var);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_heap(var, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(var, INTEGER);
	}

	return 0;
}


/* make-ratio */
static int make_ratio_force_(addr *ret, addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_FIXNUM:
			bignum_fixnum_heap(ret, var);
			break;

		case LISPTYPE_BIGNUM:
			bignum_copy_heap(ret, var);
			break;

		default:
			*ret = Nil;
			return TypeError_(var, INTEGER);
	}

	return 0;
}

int make_ratio_syscode(addr numer, addr denom, addr *ret)
{
	int sign1, sign2;

	Return(make_ratio_force_(&numer, numer));
	Return(make_ratio_force_(&denom, denom));
	GetSignBignum(numer, &sign1);
	GetSignBignum(denom, &sign2);
	SetSignBignum(numer, SignPlus);
	SetSignBignum(denom, SignPlus);
	sign1 = SignMulti(sign1, sign2);
	make_ratio_alloc_unsafe(NULL, &numer, sign1, numer, denom);
	*ret = numer;

	return 0;
}


/* make-complex */
int make_complex_code_(addr real, addr imag, addr *ret)
{
	return complex_force_heap_(ret, real, imag, ComplexType_error);
}


/* equal-random-state */
void equal_random_state_syscode(addr left, addr right, addr *ret)
{
	*ret = equal_random_state_addr(left, right)? T: Nil;
}


/* subtypep-extend */
int subtypep_extend_syscode_(Execute ptr,
		addr x, addr y, addr env, addr check, addr *ret)
{
	if (env == Unbound)
		env = Nil;
	if (check == Unbound)
		check = Nil;
	return subtypep_extend_(ptr, x, y, env, check, ret);
}


/* subtypep-number */
int subtypep_number_syscode_(Execute ptr, addr x, addr *ret)
{
	Return(parse_type(ptr, &x, x, Nil));
	Return(type_subtypep_throw_heap_(ptr->local, x, &x));
	get_type_subtypep(&x, x);
	Return(type_object_(&x, x));

	return Result(ret, x);
}


/* eastasian-set */
int eastasian_set_syscode_(addr var, addr value, addr errorp, addr *ret)
{
	return eastasian_set_syscall_(var, value, errorp, ret);
}


/* eastasian-get */
int eastasian_get_syscode_(addr var, addr *ret1, addr *ret2)
{
	return eastasian_get_syscall_(var, ret1, ret2);
}


/* eastasian-width */
int eastasian_width_syscode_(addr pos, addr *ret1, addr *ret2)
{
	return eastasian_width_syscall_(pos, ret1, ret2);
}


/* run-process */
int run_program_syscode_(Execute ptr, addr var, addr args, addr rest, addr *ret)
{
	return run_process_(ptr, var, args, rest, &var);
}


/* make-callname */
int make_callname_syscode_(addr var, addr *ret)
{
	return parse_callname_error_(ret, var);
}


/* remove-file */
int remove_file_syscode(Execute ptr, addr var, addr opt, addr *ret)
{
	int check;
	Return(remove_file_common_(ptr, var, (opt != Nil), &check));
	return Result(ret, check? T: Nil);
}


/* remove-directory */
int remove_directory_syscode(Execute ptr, addr var, addr opt, addr *ret)
{
	int check;
	Return(remove_directory_common_(ptr, var, (opt != Nil), &check));
	return Result(ret, check? T: Nil);
}


/* declare-parse */
static int declare_parse_value_(addr symbol, OptimizeType *ret)
{
	addr root, check;

	getroot_declare(&root);
	/* safety */
	GetConst(COMMON_SAFETY, &check);
	if (symbol == check)
		return Result(ret, get_optimize_safety_declare(root));
	/* speed */
	GetConst(COMMON_SPEED, &check);
	if (symbol == check)
		return Result(ret, get_optimize_speed_declare(root));
	/* space */
	GetConst(COMMON_SPACE, &check);
	if (symbol == check)
		return Result(ret, get_optimize_space_declare(root));
	/* debug */
	GetConst(COMMON_DEBUG, &check);
	if (symbol == check)
		return Result(ret, get_optimize_debug_declare(root));
	/* compilation */
	GetConst(COMMON_COMPILATION_SPEED, &check);
	if (symbol == check)
		return Result(ret, get_optimize_compilation_declare(root));

	/* error */
	*ret = 0;
	return fmte_("Invalid declare-parse argument ~S.", symbol, NULL);
}

int declare_parse_syscode(addr form, addr *ret)
{
	OptimizeType value;
	addr symbol, check;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &symbol, &check))
		goto error;
	if (check != Nil)
		goto error;
	Return(declare_parse_value_(symbol, &value));
	fixnum_heap(ret, (fixnum)value);
	return 0;

error:
	return fmte_("The declare-parse form ~S must be a (symbol).", form, NULL);
}


/* parse-type */
int parse_type_syscode(Execute ptr, addr var, addr *ret)
{
	Return(parse_type(ptr, &var, var, Nil));
	Return(type_object_(&var, var));

	return Result(ret, var);
}


/* upgraded-open-element-type */
int upgraded_open_element_type_syscode_(addr var, addr *ret)
{
	return upgrade_open_element_type_stream_(var, ret);
}


/* make-memory-input-stream */
static int getkeyindex_syscode_(addr list, constindex key, size_t *ret)
{
	addr pos;

	if (getplist_constant_safe(list, key, &pos)) {
		*ret = 0;
	}
	else if (pos == Nil) {
		*ret = 0;
	}
	else {
		Return(getindex_integer_(pos, ret));
	}

	return 0;
}

static int getkeycache_syscode(addr list)
{
	if (GetKeyArgs(list, KEYWORD_CACHE, &list)) {
#ifdef LISP_DEBUG
		return 1;  /* on */
#else
		return 0;  /* off */
#endif
	}

	return (list == Nil)? 0: 1;
}

int make_memory_input_stream_syscode_(addr var, addr rest, addr *ret)
{
	int cache;
	size_t size, array;

	/* &key */
	Return(getkeyindex_syscode_(rest, CONSTANT_KEYWORD_SIZE, &size));
	Return(getkeyindex_syscode_(rest, CONSTANT_KEYWORD_ARRAY, &array));
	cache = getkeycache_syscode(rest);

	/* call */
	Return(open_input_memory_stream_(&var, var, size, array, cache));
	return Result(ret, var);
}


/* make-memory-output-stream */
int make_memory_output_stream_syscode_(addr rest, addr *ret)
{
	int cache;
	addr input;
	size_t size, array;

	/* &key */
	if (GetKeyArgs(rest, KEYWORD_INPUT, &input))
		input = Nil;
	Return(getkeyindex_syscode_(rest, CONSTANT_KEYWORD_SIZE, &size));
	Return(getkeyindex_syscode_(rest, CONSTANT_KEYWORD_ARRAY, &array));
	cache = getkeycache_syscode(rest);

	/* call */
	Return(open_output_memory_stream_(&rest, input, size, array, cache));
	return Result(ret, rest);
}


/* make-memory-io-stream */
int make_memory_io_stream_syscode_(addr rest, addr *ret)
{
	int cache;
	addr input;
	size_t size, array;

	/* &key */
	if (GetKeyArgs(rest, KEYWORD_INPUT, &input))
		input = Nil;
	Return(getkeyindex_syscode_(rest, CONSTANT_KEYWORD_SIZE, &size));
	Return(getkeyindex_syscode_(rest, CONSTANT_KEYWORD_ARRAY, &array));
	cache = getkeycache_syscode(rest);

	/* call */
	Return(open_io_memory_stream_(&rest, input, size, array, cache));
	return Result(ret, rest);
}


/* with-input-from-memory */
int with_input_from_memory_syscode_(Execute ptr, addr form, addr *ret)
{
	addr args, body, var, vector;
	addr let, make, unwind, progn, close, decl, pos;

	/* argument */
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &args, &body))
		goto error;
	if (! consp_getcons(args, &var, &args))
		goto error;
	if (! consp_getcons(args, &vector, &args))
		goto error;

	/* `(let ((,var (system::make-memory-input-stream ,vector)))
	 *    ,@decl
	 *    (unwind-protect
	 *      (progn ,@form)
	 *      (close ,var)))
	 */
	GetConst(COMMON_LET, &let);
	GetConst(SYSTEM_MAKE_MEMORY_INPUT_STREAM, &make);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_CLOSE, &close);
	Return(declare_body_form_(body, &decl, &body));
	list_heap(&close, close, var, NULL);
	cons_heap(&progn, progn, body);
	list_heap(&unwind, unwind, progn, close, NULL);
	lista_heap(&make, make, vector, args, NULL);
	list_heap(&make, var, make, NULL);
	conscar_heap(&make, make);
	conscar_heap(&let, let);
	cons_heap(&let, make, let);
	while (decl != Nil) {
		GetCons(decl, &pos, &decl);
		cons_heap(&let, pos, let);
	}
	cons_heap(&let, unwind, let);
	nreverse(ret, let);
	return 0;

error:
	return fmte_("WITH-INPUT-FROM-MEMORY form ~S must be a "
			"((var vector ...) &body body).", form, NULL);
}


/* with-output-to-memory */
int with_output_to_memory_syscode_(Execute ptr, addr form, addr *ret)
{
	addr args, var, body;
	addr let, make, unwind, progn, get, close, decl, pos;

	/* argument */
	Return_getcdr(form, &args);
	if (! consp_getcons(args, &args, &body))
		goto error;
	if (! consp_getcons(args, &var, &args))
		goto error;

	/* `(let ((,var (make-string-output-stream ...)))
	 *    ,@decl
	 *    (unwind-protect
	 *      (progn ,@body
	 *             (get-output-stream-string ,var))
	 *      (close ,var)))
	 */
	GetConst(COMMON_LET, &let);
	GetConst(SYSTEM_MAKE_MEMORY_OUTPUT_STREAM, &make);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_PROGN, &progn);
	GetConst(SYSTEM_GET_OUTPUT_STREAM_MEMORY, &get);
	GetConst(COMMON_CLOSE, &close);
	Return(declare_body_form_(body, &decl, &body));
	list_heap(&close, close, var, NULL);
	list_heap(&get, get, var, NULL);
	conscar_heap(&progn, progn);
	while (body != Nil) {
		GetCons(body, &pos, &body);
		cons_heap(&progn, pos, progn);
	}
	cons_heap(&progn, get, progn);
	nreverse(&progn, progn);
	list_heap(&unwind, unwind, progn, close, NULL);
	cons_heap(&make, make, args);
	list_heap(&make, var, make, NULL);
	conscar_heap(&make, make);
	conscar_heap(&let, let);
	cons_heap(&let, make, let);
	while (decl != Nil) {
		GetCons(decl, &pos, &decl);
		cons_heap(&let, pos, let);
	}
	cons_heap(&let, unwind, let);
	nreverse(ret, let);
	return 0;

error:
	return fmte_("WITH-OUTPUT-TO-MEMORY form ~S must be a "
			"((var) &body body).", form, NULL);
}


/* get-output-stream-memory */
int get_output_stream_memory_syscode_(addr var, addr *ret)
{
	return memory_stream_heap_(var, ret);
}


/* memory-stream-p */
void memory_stream_p_syscode(addr var, addr *ret)
{
	gettype_memory_stream(var, ret);
}


/* (setf memory-stream-p) */
int setf_memory_stream_p_syscode_(addr var, addr value)
{
	return settype_memory_stream_(var, value);
}


/* byte-integer */
static int byte_integer_endian_(addr list, int littlep, addr *ret)
{
	addr x, y, i, v8;
	LocalRoot local;

	local = Local_Thread;
	fixnum_heap(&x, 0);
	fixnum_heap(&i, 0);
	fixnum_heap(&v8, 8);
	while (list != Nil) {
		Return_getcons(list, &y, &list);
		if (littlep) {
			Return(ash_integer_common_(local, y, i, &y));
		}
		else {
			Return(ash_integer_common_(local, x, i, &x));
		}
		Return(plus_ii_real_common_(local, x, y, &x));
		Return(plus_ii_real_common_(local, i, v8, &i));
	}

	return Result(ret, x);
}

int byte_integer_syscode_(addr list, addr *ret)
{
	union byte_integer_union {
		uint16_t u16;
		uint8_t u8[2];
	} u;

	u.u16 = 1;
	return byte_integer_endian_(list, u.u8[0] != 0, ret);
}


/* question */
int question_syscode_(Execute ptr, addr var, addr args)
{
	return question_values_(ptr, var, args);
}


/* extension */
#ifdef LISP_EXTENSION
#include "ext_eval.h"
#endif
int extension_syscode(Execute ptr, addr var)
{
#ifdef LISP_EXTENSION
	return lisps_eval_(ptr, var);
#else
	return fmte_("The implementation is not supported.", NULL);
#endif
}


/************************************************************
 *  terme.c
 ************************************************************/

#ifdef LISP_TERME
/************************************************************
  terme_call.h
 ************************************************************/
#ifndef __TERME_CALL_HEADER__
#define __TERME_CALL_HEADER__


#define terme_init _n(terme_init)
#define terme_begin _n(terme_begin)
#define terme_end _n(terme_end)
#define terme_switch_textmode _n(terme_switch_textmode)
#define terme_switch_rawmode _n(terme_switch_rawmode)
#define terme_screen_x _n(terme_screen_x)

void terme_init(void);
int terme_begin(void);
int terme_end(void);
int terme_switch_textmode(int *ret);
int terme_switch_rawmode(int *ret);
void terme_screen_x(int *ret);

#endif


/************************************************************
  terme_escape.h
 ************************************************************/
#ifndef __TERME_ESCAPE_HEADER__
#define __TERME_ESCAPE_HEADER__


#define terme_font _n(terme_font)
#define terme_text_color _n(terme_text_color)
#define terme_back_color _n(terme_back_color)
#define terme_cursor_left _n(terme_cursor_left)
#define terme_cursor_right _n(terme_cursor_right)
#define terme_cursor_move _n(terme_cursor_move)
#define terme_cursor_delete_line_left _n(terme_cursor_delete_line_left)
#define terme_cursor_delete_line_right _n(terme_cursor_delete_line_right)
#define terme_cursor_delete_line _n(terme_cursor_delete_line)
#define terme_cursor_delete_page _n(terme_cursor_delete_page)

int terme_font(PrintFont value);
int terme_text_color(PrintColor value);
int terme_back_color(PrintColor value);
int terme_cursor_left(int n);
int terme_cursor_right(int n);
int terme_cursor_move(int n);
int terme_cursor_delete_line_left(void);
int terme_cursor_delete_line_right(void);
int terme_cursor_delete_line(void);
int terme_cursor_delete_page(void);

#endif


/************************************************************
  terme_input.h
 ************************************************************/
#ifndef __TERME_INPUT_HEADER__
#define __TERME_INPUT_HEADER__


#define terme_input_init _n(terme_input_init)
#define terme_clear_input _n(terme_clear_input)
#define terme_unread_char _n(terme_unread_char)
#define terme_listen _n(terme_listen)
#define terme_hang_char _n(terme_hang_char)
#define terme_read_char _n(terme_read_char)

enum terme_escape {
	terme_escape_error,
	terme_escape_code,
	terme_escape_up,         /* ^P */
	terme_escape_down,       /* ^N */
	terme_escape_left,       /* ^F */
	terme_escape_right,      /* ^B */
	terme_escape_function,   /* Fx, PFx */
	terme_escape_return,     /* ^J, ^M, Enter */
	terme_escape_backspace,  /* ^H, BS */
	terme_escape_first,      /* ^A */
	terme_escape_last,       /* ^E */
	terme_escape_update,     /* ^L */
	terme_escape_delete,     /* ^D */
	terme_escape_rmleft,     /* ^U */
	terme_escape_rmright,    /* ^K */
	terme_escape_tab,        /* ^I */
	terme_escape_size
};
typedef enum terme_escape TermeEscape;

struct terme_keyboard {
	TermeEscape type;
	unicode c;
};
typedef struct terme_keyboard TermeKeyboard;

void terme_input_init(void);
int terme_clear_input(void);
int terme_unread_char(unicode c);
int terme_listen(int *ret);
int terme_hang_char(unicode *value, int *ret);
int terme_read_char(unicode *value, int *ret);
int terme_read_keyboard(TermeKeyboard *ret);

#endif


/************************************************************
  terme_output.h
 ************************************************************/
#ifndef __TERME_OUTPUT_HEADER__
#define __TERME_OUTPUT_HEADER__


#define terme_output_init _n(terme_output_init)
#define terme_finish_output _n(terme_finish_output)
#define terme_write_byte _n(terme_write_byte)
#define terme_write_char _n(terme_write_char)
#define terme_terpri _n(terme_terpri)
#define terme_fresh_line _n(terme_fresh_line)

void terme_output_init(void);
int terme_finish_output(void);
int terme_write_byte(byte c);
int terme_write_char(unicode c, int width);
int terme_terpri(void);
int terme_fresh_line(void);

#endif


/************************************************************
  terme_prompt.h
 ************************************************************/
#ifndef __TERME_PROMPT_HEADER__
#define __TERME_PROMPT_HEADER__


#define terme_prompt_ _n(terme_prompt_)
#define terme_readline_ _n(terme_readline_)

int terme_prompt_(Execute ptr, addr pos, enum prompt_mode mode);
int terme_readline_(Execute ptr, addr *ret);

#endif


/************************************************************
  terme_value.h
 ************************************************************/
#ifndef __TERME_VALUE_HEADER__
#define __TERME_VALUE_HEADER__


#define terme_build _n(terme_build)
#define terme_set_prompt_ _n(terme_set_prompt_)
#define terme_get_prompt_ _n(terme_get_prompt_)

#define terme_data_init_ _n(terme_data_init_)
#define terme_data_push_ _n(terme_data_push_)
#define terme_data_get_ _n(terme_data_get_)
#define terme_data_get_width_ _n(terme_data_get_width_)
#define terme_data_size_ _n(terme_data_size_)
#define terme_data_size_width_ _n(terme_data_size_width_)
#define terme_data_allwidth_ _n(terme_data_allwidth_)
#define terme_data_delete_ _n(terme_data_delete_)
#define terme_data_delete_left_ _n(terme_data_delete_left_)
#define terme_data_delete_right_ _n(terme_data_delete_right_)
#define terme_data_make_ _n(terme_data_make_)
#define terme_history_save_ _n(terme_history_save_)
#define terme_history_update_ _n(terme_history_update_)

void terme_build(void);
int terme_set_prompt_(Execute ptr, addr value, enum prompt_mode mode);
int terme_get_prompt_(Execute ptr, addr *value, enum prompt_mode *mode);

int terme_data_init_(Execute ptr);
int terme_data_push_(Execute ptr, int index, unicode c, int *ret);
int terme_data_get_(Execute ptr, int index, unicode *value, int *ret);
int terme_data_get_width_(Execute ptr, int index, int *ret);
int terme_data_size_(Execute ptr, int *ret);
int terme_data_size_width_(Execute ptr, int *size, int *width);
int terme_data_allwidth_(Execute ptr, int *ret);
int terme_data_delete_(Execute ptr, int index, int *ret);
int terme_data_delete_left_(Execute ptr, int index, int *ret);
int terme_data_delete_right_(Execute ptr, int index, int *ret);
int terme_data_make_(Execute ptr, addr *ret);
int terme_history_save_(Execute ptr);
int terme_history_update_(Execute ptr, int index, int *ret);

#endif


/************************************************************
  terme_interface.c
 ************************************************************/

void init_terme(void)
{
	terme_init();
}

void build_terme(void)
{
	terme_build();
}

int begin_terme(void)
{
	return terme_begin();
}

int end_terme(void)
{
	terme_end();
	return 0;
}

int prompt_terme_(Execute ptr, addr pos, enum prompt_mode mode)
{
	return terme_prompt_(ptr, pos, mode);
}

int readline_terme_(Execute ptr, addr *ret)
{
	return terme_readline_(ptr, ret);
}

int font_terme(PrintFont value)
{
	return terme_font(value);
}

int text_color_terme(PrintColor value)
{
	return terme_text_color(value);
}

int back_color_terme(PrintColor value)
{
	return terme_back_color(value);
}


/************************************************************
  terme_call.c
 ************************************************************/
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

static struct termios terme_textmode_termios;
static struct termios terme_switch_termios;
static int terme_switch_textmode_p;
static int terme_x;
static int terme_y;

/*
 *  terme-init
 */
static void terme_init_handler(int sig)
{
	struct winsize ws;

	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1) {
		terme_x = 0;
		terme_y = 0;
	}
	else {
		terme_x = (int)ws.ws_col;
		terme_y = (int)ws.ws_row;
	}
}

void terme_init(void)
{
	struct sigaction act;

	act.sa_handler = terme_init_handler;
	sigemptyset(&act.sa_mask);
	act.sa_flags= SA_RESTART;
	if (sigaction(SIGWINCH, &act, NULL)) {
		Abort("sigaction error.");
	}
}


/*
 *  terme-begin
 */
static int terme_begin_get(struct termios *ret)
{
	if (ret == NULL)
		return 0;
	return tcgetattr(STDIN_FILENO, ret);
}

static int terme_begin_set(struct termios *ret)
{
	return tcsetattr(STDIN_FILENO, TCSAFLUSH, ret);
}

static int terme_begin_termios(void)
{
	struct termios v;

	/* backup */
	if (terme_begin_get(&v)) {
		fprintf(stderr, "tcgetattr value error\n");
		return 1;
	}
	terme_textmode_termios = v;

	/* set terminal */
	v.c_iflag &= ~(PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
	v.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
	v.c_oflag &= ~OPOST;
	v.c_cflag &= ~(CSIZE | PARENB);
	v.c_cflag |= CS8;
	v.c_cc[VMIN] = 1;
	v.c_cc[VTIME] = 0;
	terme_switch_termios = v;

	return 0;
}

static int terme_getsize(void)
{
	struct winsize ws;

	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1)
		return 1;
	terme_x = (int)ws.ws_col;
	terme_y = (int)ws.ws_row;

	return 0;
}

int terme_begin(void)
{
	/* terminal size */
	if (terme_getsize())
		return 1;

	/* termios */
	if (terme_begin_termios())
		return 1;

	/* switch */
	terme_switch_textmode_p = 1;

	/* output */
	terme_input_init();
	terme_output_init();

	return 0;
}

int terme_end(void)
{
	return terme_begin_set(&terme_textmode_termios);
}


/*
 *  terme-switch
 */
int terme_switch_textmode(int *ret)
{
	if (terme_switch_textmode_p) {
		if (ret)
			*ret = 0;
		return 0;
	}
	if (terme_begin_get(&terme_switch_termios)) {
		if (ret)
			*ret = 0;
		return 1;
	}
	if (ret)
		*ret = 1;
	terme_switch_textmode_p = 1;
	return terme_begin_set(&terme_textmode_termios);
}

int terme_switch_rawmode(int *ret)
{
	if (! terme_switch_textmode_p) {
		if (ret)
			*ret = 0;
		return 0;
	}
	if (terme_begin_set(&terme_switch_termios)) {
		if (ret)
			*ret = 0;
		return 1;
	}
	if (ret)
		*ret = 1;
	terme_switch_textmode_p = 0;
	memset(&terme_switch_termios, '\0', sizeof(terme_switch_termios));
	return 0;
}

void terme_screen_x(int *ret)
{
	*ret = terme_x;
}


/************************************************************
  terme_escape.c
 ************************************************************/

static int terme_escape_operator(const char *str)
{
	byte c;
	int i;

	for (i = 0; ; i++) {
		c = (byte)str[i];
		if (c == 0)
			break;
		if (terme_write_byte(c))
			return 1;
	}
	return terme_finish_output();
}

int terme_font(PrintFont value)
{
	const char *str;

	switch (value) {
		case print_font_reset:      str = "\x1B[0m"; break;
		case print_font_bold:       str = "\x1B[1m"; break;
		case print_font_faint:      str = "\x1B[2m"; break;
		case print_font_italic:     str = "\x1B[3m"; break;
		case print_font_underline:  str = "\x1B[4m"; break;
		case print_font_blink1:     str = "\x1B[5m"; break;
		case print_font_blink2:     str = "\x1B[6m"; break;
		case print_font_reverse:    str = "\x1B[7m"; break;
		case print_font_hide_in:    str = "\x1B[8m"; break;
		case print_font_hide_out:   str = "\x1B[9m"; break;
		default: return 1;
	}

	return terme_escape_operator(str);
}

int terme_text_color(PrintColor value)
{
	const char *str;

	switch (value) {
		case print_color_reset:           str = "\x1B[0m"; break;
		case print_color_black:           str = "\x1B[30m"; break;
		case print_color_red:             str = "\x1B[31m"; break;
		case print_color_green:           str = "\x1B[32m"; break;
		case print_color_yellow:          str = "\x1B[33m"; break;
		case print_color_blue:            str = "\x1B[34m"; break;
		case print_color_magenta:         str = "\x1B[35m"; break;
		case print_color_cyan:            str = "\x1B[36m"; break;
		case print_color_white:           str = "\x1B[37m"; break;
		case print_color_bright_black:    str = "\x1B[90m"; break;
		case print_color_bright_red:      str = "\x1B[91m"; break;
		case print_color_bright_green:    str = "\x1B[92m"; break;
		case print_color_bright_yellow:   str = "\x1B[93m"; break;
		case print_color_bright_blue:     str = "\x1B[94m"; break;
		case print_color_bright_magenta:  str = "\x1B[95m"; break;
		case print_color_bright_cyan:     str = "\x1B[96m"; break;
		case print_color_bright_white:    str = "\x1B[97m"; break;
		default: return 1;
	}

	return terme_escape_operator(str);
}

int terme_back_color(PrintColor value)
{
	const char *str;

	switch (value) {
		case print_color_reset:           str = "\x1B[0m"; break;
		case print_color_black:           str = "\x1B[40m"; break;
		case print_color_red:             str = "\x1B[41m"; break;
		case print_color_green:           str = "\x1B[42m"; break;
		case print_color_yellow:          str = "\x1B[43m"; break;
		case print_color_blue:            str = "\x1B[44m"; break;
		case print_color_magenta:         str = "\x1B[45m"; break;
		case print_color_cyan:            str = "\x1B[46m"; break;
		case print_color_white:           str = "\x1B[47m"; break;
		case print_color_bright_black:    str = "\x1B[100m"; break;
		case print_color_bright_red:      str = "\x1B[101m"; break;
		case print_color_bright_green:    str = "\x1B[102m"; break;
		case print_color_bright_yellow:   str = "\x1B[103m"; break;
		case print_color_bright_blue:     str = "\x1B[104m"; break;
		case print_color_bright_magenta:  str = "\x1B[105m"; break;
		case print_color_bright_cyan:     str = "\x1B[106m"; break;
		case print_color_bright_white:    str = "\x1B[107m"; break;
		default: return 1;
	}

	return terme_escape_operator(str);
}

int terme_cursor_left(int n)
{
	char data[64];

	if (n == 0)
		return terme_escape_operator("\x1B[D");
	snprintf(data, 64, "\x1B[%dD", n);
	return terme_escape_operator(data);
}

int terme_cursor_right(int n)
{
	char data[64];

	if (n == 0)
		return terme_escape_operator("\x1B[C");
	snprintf(data, 64, "\x1B[%dC", n);
	return terme_escape_operator(data);
}

int terme_cursor_move(int n)
{
	char data[64];
	snprintf(data, 64, "\x1B[%dG", n + 1);
	return terme_escape_operator(data);
}

int terme_cursor_delete_line_left(void)
{
	return terme_escape_operator("\x1B[1K");
}

int terme_cursor_delete_line_right(void)
{
	return terme_escape_operator("\x1B[K");
}

int terme_cursor_delete_line(void)
{
	return terme_escape_operator("\x1B[2K");
}

int terme_cursor_delete_page(void)
{
	return terme_escape_operator("\x1B[2J")
		|| terme_escape_operator("\x1B[1;1H");
}


/************************************************************
  terme_input.c
 ************************************************************/
#include <unistd.h>
#include <sys/select.h>

#ifdef LISP_DEBUG
#define TERME_INPUT_SIZE		3
#else
#define TERME_INPUT_SIZE		4096
#endif

#define TERME_INPUT_UNBYTE		64
#define TERME_INPUT_UNREAD		8

/* buffer */
static byte terme_input_buffer[TERME_INPUT_SIZE];
static size_t terme_input_size;
static size_t terme_input_now;
/* unicode */
static byte terme_input_unbyte[TERME_INPUT_UNBYTE];
static int terme_input_unbyte_size;
static int terme_input_unbyte_base;
static int terme_input_unbyte_now;
/* unread */
static unicode terme_input_unread[TERME_INPUT_UNREAD];
static int terme_input_unread_size;
static int terme_input_unread_base;
static int terme_input_unread_now;

void terme_input_init(void)
{
	terme_input_size = 0;
	terme_input_now = 0;
	terme_input_unbyte_size = 0;
	terme_input_unbyte_base = 0;
	terme_input_unbyte_now = 0;
	terme_input_unread_size = 0;
	terme_input_unread_base = 0;
	terme_input_unread_now = 0;
}

static int terme_input_select(int *ret)
{
	int fd, reti;
	fd_set fdset;
	struct timeval tm;

	fd = STDIN_FILENO;
	FD_ZERO(&fdset);
	FD_SET(fd, &fdset);
	tm.tv_sec = 0;
	tm.tv_usec = 0;
	reti = select(fd + 1, &fdset, NULL, NULL, &tm);
	if (reti < 0) {
		*ret = 0;
		return 1; /* error */
	}
	if (reti == 0) {
		/* empty */
		*ret = 0;
		return 0;
	}
	else {
		/* can read */
		*ret = 1;
		return 0;
	}
}

#define TERME_CLEAR_INPUT_STDIN		4096
static int terme_clear_input_stdin(void)
{
	byte data[TERME_CLEAR_INPUT_STDIN];
	int check;
	ssize_t rets;

	for (;;) {
		if (terme_input_select(&check))
			return 1;
		if (! check)
			break;
		rets = read(STDIN_FILENO, data, TERME_CLEAR_INPUT_STDIN);
		if (rets < 0)
			return 1;
	}

	return 0;
}

int terme_clear_input(void)
{
	terme_input_size = 0;
	terme_input_now = 0;
	terme_input_unbyte_size = 0;
	terme_input_unbyte_base = 0;
	terme_input_unbyte_now = 0;
	terme_input_unread_size = 0;
	terme_input_unread_base = 0;
	terme_input_unread_now = 0;
	return terme_clear_input_stdin();
}


/*
 *  unbyte
 */
static int terme_unbyte_push(byte c)
{
	if (TERME_INPUT_UNBYTE <= terme_input_unbyte_size)
		return 1; /* error */

	terme_input_unbyte_size++;
	terme_input_unbyte[terme_input_unbyte_base] = c;
	terme_input_unbyte_base++;
	terme_input_unbyte_base %= TERME_INPUT_UNBYTE;

	return 0;
}

static void terme_unbyte_pop(byte *value, int *ret)
{
	if (terme_input_unbyte_size == 0) {
		*value = 0;
		*ret = 0;
		return;
	}

	*value = terme_input_unbyte[terme_input_unbyte_now];
	terme_input_unbyte_size--;
	if (terme_input_unbyte_size) {
		terme_input_unbyte_now++;
		terme_input_unbyte_now %= TERME_INPUT_UNBYTE;
	}
	else {
		terme_input_unbyte_base = 0;
		terme_input_unbyte_now = 0;
	}
	*ret = 1;
}


/*
 *  getc
 */
static int terme_input_wait(void)
{
	int fd, reti;
	fd_set fdset;

	fd = STDIN_FILENO;
	FD_ZERO(&fdset);
	FD_SET(fd, &fdset);
	reti = select(fd + 1, &fdset, NULL, NULL, NULL);
	return reti < 0;
}

static int terme_getc_buffering(void)
{
	ssize_t size;

	terme_input_size = 0; /* for error */
	terme_input_now = 0;
	size = read(STDIN_FILENO, terme_input_buffer, TERME_INPUT_SIZE);
	if (size < 0)
		return 1;
	terme_input_size = (size_t)size;

	return 0;
}

static int terme_getc_hang(byte *value, int *ret)
{
	byte c;
	int check;

	/* unbyte */
	terme_unbyte_pop(&c, &check);
	if (check) {
		*value = c;
		*ret = 1;
		return 0;
	}

	/* input buffer */
	if (terme_input_size <= terme_input_now) {
		if (terme_input_select(&check))
			goto error;
		if (! check) { /* empty */
			*value = 0;
			*ret = 0;
			return 0;
		}
		if (terme_getc_buffering())
			goto error;
	}
	*ret = 1;
	*value = terme_input_buffer[terme_input_now];
	terme_input_now++;
	return 0;

error:
	*value = 0;
	*ret = 0;
	return 1;
}

static int terme_getc(byte *ret)
{
	byte c;
	int check;

	for (;;) {
		if (terme_getc_hang(&c, &check))
			goto error;
		if (check)
			break;
		if (terme_input_wait())
			goto error;
		return 0;
	}
	*ret = c;
	return 0;

error:
	*ret = 0;
	return 1;
}


/*
 *  unread-char
 */
int terme_unread_char(unicode c)
{
	if (TERME_INPUT_UNREAD <= terme_input_unread_size)
		return 1; /* error */

	terme_input_unread_size++;
	terme_input_unread[terme_input_unread_base] = c;
	terme_input_unread_base++;
	terme_input_unread_base %= TERME_INPUT_UNREAD;

	return 0;
}

static void terme_unread_pop(unicode *value, int *ret)
{
	if (terme_input_unread_size == 0) {
		*value = 0;
		*ret = 0;
		return;
	}

	*value = terme_input_unread[terme_input_unread_now];
	terme_input_unread_size--;
	if (terme_input_unread_size) {
		terme_input_unread_now++;
		terme_input_unread_now %= TERME_INPUT_UNREAD;
	}
	else {
		terme_input_unread_base = 0;
		terme_input_unread_now = 0;
	}
	*ret = 1;
}


/*
 *  read-char
 */
static int terme_listen_rollback(byte *data, int size)
{
	while (size) {
		size--;
		if (terme_unbyte_push(data[size]))
			return 1;
	}

	return 0;
}

#define terme_listen_utf16(x) (0xD800 <= (x) && (x) < 0xE000)

#define terme_listen_getc() { \
	if (terme_getc_hang(&c, &check)) goto error; \
	if (! check) goto hang; \
	if (c == 0x1B) goto escape; \
	data[i++] = c; \
}
static int terme_listen_unicode(void)
{
	byte data[8], c;
	int i, check;
	unicode value;

	i = 0;
	terme_listen_getc();

	/* encode */
	if (0x00 <= c && c <= 0x7F)
		goto sequence1;
	if (0xC2 <= c && c <= 0xDF)
		goto sequence2;
	if (0xE0 <= c && c <= 0xEF)
		goto sequence3;
	if (0xF0 <= c && c <= 0xF7)
		goto sequence4;
	goto invalid;

sequence1:
	value = (unicode)c;
	goto normal;

sequence2:
	value = (0x1F & c) << 6;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= 0x3F & c;
	if (value < 0x80)
		goto invalid;
	goto normal;

sequence3:
	value = (0x0F & c) << 12;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= (0x3F & c) << 6;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= 0x3F & c;
	if (value < 0x0800)
		goto invalid;
	if (terme_listen_utf16(value))
		goto invalid;
	goto normal;

sequence4:
	value = (0x07 & c) << 18;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= (0x3F & c) << 12;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= (0x3F & c) << 6;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= 0x3F & c;
	if (value < 0x010000)
		goto invalid;
	if (UnicodeCount <= value)
		goto invalid;
	goto normal;

normal:
	if (terme_unread_char(value))
		goto error;
	return 0;

invalid:
	return 1;

error:
	(void)terme_listen_rollback(data, i);
	return 1;

hang:
	return terme_listen_rollback(data, i);

escape:
	if (terme_listen_rollback(data, i))
		return 1;
	if (terme_unread_char(0x1B))
		goto error;
	return 0;
}

int terme_listen(int *ret)
{
	/* unread */
	if (terme_input_unread_size) {
		*ret = 1;
		return 0;
	}

	/* listen */
	if (terme_listen_unicode()) {
		*ret = 0;
		return 1; /* error */
	}

	/* unread */
	*ret = (terme_input_unread_size != 0);
	return 0;
}

int terme_hang_char(unicode *value, int *ret)
{
	int check;
	unicode c;

	/* unread */
	terme_unread_pop(&c, &check);
	if (check) {
		*value = c;
		*ret = 1;
		return 0;
	}

	/* listen */
	if (terme_listen_unicode()) {
		*value = 0;
		*ret = 0;
		return 1; /* error */
	}

	/* unread */
	terme_unread_pop(value, ret);
	return 0;
}

int terme_read_char(unicode *value, int *ret)
{
	int check;
	unicode c;

	for (;;) {
		if (terme_hang_char(&c, &check))
			goto error;
		if (check)
			break;
		if (terme_input_wait())
			goto error;
	}
	*value = c;
	*ret = 0; /* normal */
	return 0;

error:
	*value = 0;
	*ret = 0;
	return 1;
}


/*
 *  read-keyboard
 *
 *  Up       ^[OA
 *  Down     ^[OB
 *  Right    ^[OC
 *  Left     ^[OD
 *  PF1      ^[OP
 *  PF2      ^[OQ
 *  PF3      ^[OR
 *  PF4      ^[OS
 *  F1       ^[[11~  ^[OP
 *  F2       ^[[12~  ^[OQ
 *  F3       ^[[13~  ^[OR
 *  F4       ^[[14~  ^[OS
 *  F5       ^[[15~  ^[Ot
 *  F6       ^[[17~  ^[Ou
 *  F7       ^[[18~  ^[Ov
 *  F8       ^[[19~  ^[Ol
 *  F9       ^[[20~  ^[Ow
 *  F10      ^[[21~  ^[Ox
 *  F11      ^[[23~
 *  F12      ^[[24~
 */
static void terme_read_escape(TermeKeyboard *ret)
{
	byte c;

	if (terme_getc(&c))
		goto error;

	if (c == 0x4F)
		goto third_4F;
	if (c == 0x5B)
		goto third_5B;
	goto error;

third_4F:
	if (terme_getc(&c))
		goto error;
	if (0x50 <= c && c <= 0x53) /* PF1 - PF4 */
		goto program;
	goto error;

third_5B:
	if (terme_getc(&c))
		goto error;
	if (c == 0x31)
		goto forth_31;
	if (c == 0x41)
		goto escape_up;
	if (c == 0x42)
		goto escape_down;
	if (c == 0x43)
		goto escape_right;
	if (c == 0x44)
		goto escape_left;
	goto error;

forth_31:
	if (terme_getc(&c))
		goto error;
	if (0x31 <= c && c <= 0x39) { /* F1 - F9 */
		if (terme_getc(&c))
			goto error;
		if (c == 0x7E) /* \E[[11~: F1 */
			goto function1;
	}
	goto error;

escape_up: /* 0x1B 0x5B 0x41 */
	ret->type = terme_escape_up;
	return;

escape_down: /* 0x1B 0x5B 0x42 */
	ret->type = terme_escape_down;
	return;

escape_right: /* 0x1B 0x5B 0x43 */
	ret->type = terme_escape_right;
	return;

escape_left: /* 0x1B 0x5B 0x44 */
	ret->type = terme_escape_left;
	return;

program:
	ret->type = terme_escape_function;
	ret->c = (c - 0x50) + 1; /* PF1 -> 1 */
	return;

function1:
	ret->type = terme_escape_function;
	ret->c = (c - 0x31) + 1; /* F1 -> 1 */
	return;

error:
	ret->type = terme_escape_error;
	return;
}

int terme_read_keyboard(TermeKeyboard *ret)
{
	int check;
	unicode c;

	if (terme_read_char(&c, &check))
		return 1;
	if (c != 0x1B) {
		ret->type = terme_escape_code;
		ret->c = c;
		return 0;
	}

	/* escape sequence */
	terme_read_escape(ret);
	return 0;
}


/************************************************************
  terme_output.c
 ************************************************************/
#include <unistd.h>

#ifdef LISP_DEBUG
#define TERME_OUTPUT_SIZE	3
#else
#define TERME_OUTPUT_SIZE	4096
#endif

static byte terme_output_buffer[TERME_OUTPUT_SIZE];
size_t terme_output_size;
size_t terme_output_x;

void terme_output_init(void)
{
	terme_output_size = 0;
	terme_output_x = 0;
}

int terme_finish_output(void)
{
	byte *data;
	size_t size, retu;
	ssize_t rets;

	size = terme_output_size;
	data = terme_output_buffer;
	while (size) {
		rets = write(STDOUT_FILENO, data, size);
		if (rets < 0)
			return 1; /* error */
		retu = (size_t)rets;
		if (size <= retu)
			break;
		size -= retu;
		data += retu;
	}
	terme_output_size = 0;

	return 0;
}

int terme_write_byte(byte c)
{
	if (TERME_OUTPUT_SIZE <= terme_output_size) {
		if (terme_finish_output())
			return 1;
	}

	terme_output_buffer[terme_output_size] = c;
	terme_output_size++;

	return 0;
}

static int terme_write_memory(const byte *data, size_t size)
{
	size_t i;

	for (i = 0; i < size; i++) {
		if (terme_write_byte(data[i]))
			return 1;
	}

	return 0;
}

static int terme_write_utf8(unicode u, byte *dst, size_t *ret)
{
	size_t w;

	w = 0;
	/* 1 byte */
	if (u < 0x80) {
		dst[w++] = u;
		goto normal;
	}
	/* 2 byte */
	if (u < 0x0800) {
		dst[w++] = 0xC2 | (u >> 6);
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
	/* 3 byte */
	if (u < 0xD800) {
		goto sequence3;
	}
	/* surrogate pair */
	if (u < 0xE000) {
		goto error;
	}
	/* 3 byte */
	if (u < 0x010000) {
sequence3:
		dst[w++] = 0xE0 | (u >> 12);
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
	/* 4 byte */
	if (u < UnicodeCount) {
		dst[w++] = 0xF0 | (u >> 18);
		dst[w++] = 0x80 | (0x3F & (u >> 12));
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
	/* error */

error:
	return 1;

normal:
	*ret = w;
	return 0;
}

int terme_write_char(unicode c, int width)
{
	byte data[8];
	size_t size;

	if (terme_write_utf8(c, data, &size)) {
		/* encode error */
		return 0;
	}
	if (c == 0x0D)
		terme_output_x = 0;
	else
		terme_output_x += width;

	/* output */
	return terme_write_memory(data, size);
}


/*
 *  call
 */
int terme_terpri(void)
{
	return terme_write_byte(0x0D) || terme_write_byte(0x0A);
}

int terme_fresh_line(void)
{
	return terme_output_x? terme_terpri(): 0;
}


/************************************************************
  terme_prompt.c
 ************************************************************/
#include <sys/types.h>
#include <unistd.h>


static int terme_prompt_size;
static int terme_prompt_width;
static int terme_value_now;
static int terme_value_width;
static int terme_history_now;

/*
 *  error
 */
static int terme_fmte_va_(addr format, addr args)
{
	int mode, check;

	if (terme_switch_textmode(&mode)) {
		Abort("terme_switch_textmode error.");
		return 0;
	}
	check = call_simple_error_(NULL, format, args);
	if (mode && terme_switch_rawmode(NULL)) {
		Abort("terme_switch_rawmode error.");
		return 0;
	}

	return check;
}

static int terme_fmte_(const char *str, ...)
{
	addr format, args;
	va_list va;

	if (terme_fresh_line()) {
		Abort("terme_fresh_line error.");
		return 0;
	}
	if (terme_finish_output()) {
		Abort("terme_finish_output error.");
		return 0;
	}
	strvect_char_heap(&format, str);
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return terme_fmte_va_(format, args);
}


/*
 *  prompt
 */
int terme_prompt_(Execute ptr, addr pos, enum prompt_mode mode)
{
	Check(! stringp(pos), "type error");
	return terme_set_prompt_(ptr, pos, mode);
}

static int terme_prompt_string_(addr pos)
{
	int width, check;
	unicode c;
	size_t size, i;

	string_length(pos, &size);
	width = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		check = (int)eastasian_width(c);
		if (terme_write_char(c, check))
			return terme_fmte_("terme_write_char error.", NULL);
		width += check;
	}
	terme_prompt_width = width;

	return 0;
}

static PrintColor terme_prompt_color(Execute ptr, enum prompt_mode mode)
{
	addr pos;
	GetConst(SYSTEM_PROMPT_BRIGHT, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Unbound)
		goto bright;
	if (pos == Nil)
		goto dark;

bright:
	/* bright */
	switch (mode) {
		case prompt_for:
			return print_color_bright_yellow;

		case prompt_debugger:
		case prompt_inspect:
		case prompt_step:
			return print_color_bright_blue;

		case prompt_eval:
		default:
			return print_color_bright_green;
	}

dark:
	/* dark */
	switch (mode) {
		case prompt_for:
			return print_color_yellow;

		case prompt_debugger:
		case prompt_inspect:
		case prompt_step:
			return print_color_blue;

		case prompt_eval:
		default:
			return print_color_green;
	}
}

static int terme_prompt_output_(Execute ptr)
{
	int check;
	enum prompt_mode mode;
	PrintColor color;
	addr pos, io;
	size_t size;

	/* special */
	Return(terme_get_prompt_(ptr, &pos, &mode));
	if (pos == Nil)
		return 0;

	/* prompt */
	string_length(pos, &size);
	terme_prompt_size = (int)size;

	/* fresh-line */
	Return(terminal_io_stream_(ptr, &io));
	Return(fresh_line_stream_(io, &check));
	if (terme_font(print_font_reset))
		goto error;
	color = terme_prompt_color(ptr, mode);
	if (terme_text_color(color))
		goto error;
	Return(terme_prompt_string_(pos));
	if (terme_font(print_font_reset))
		goto error;
	if (terme_finish_output())
		goto error;
	return 0;

error:
	return terme_fmte_("terme output error.", NULL);
}


/*
 *  readline
 */
static int terme_readline_loop_(Execute ptr, TermeKeyboard *, addr *, int *);

static int terme_readline_ctrl_z_(TermeKeyboard *str)
{
	int mode;
	pid_t pid;

	if (terme_switch_textmode(&mode))
		return terme_fmte_("terme_switch_textmode error.", NULL);

	pid = getpid();
	if (kill(pid, SIGTSTP))
		return terme_fmte_("kill error.", NULL);

	if (mode && terme_switch_rawmode(NULL))
		return terme_fmte_("terme_switch_rawmode error.", NULL);

	/* ignore */
	str->type = terme_escape_error;
	return 0;
}

static int terme_readline_control_(Execute ptr,
		TermeKeyboard *str, addr *value, int *ret)
{
	switch (str->c) {
		case 0x03:	/* C */
			return terme_fmte_("Ctrl + C", NULL);

		case 0x04:  /* D, delete */
			str->type = terme_escape_delete;
			break;

		case 0x0A:  /* J */
		case 0x0D:  /* Return, Enter, Ctrl+M */
			str->type = terme_escape_return;
			break;

		case 0x10:  /* P, up */
			str->type = terme_escape_up;
			break;

		case 0x0E:  /* N, down */
			str->type = terme_escape_down;
			break;

		case 0x06:  /* F, left */
			str->type = terme_escape_right;
			break;

		case 0x02:  /* B, right */
			str->type = terme_escape_left;
			break;

		case 0x01:  /* A */
			str->type = terme_escape_first;
			break;

		case 0x05:  /* E */
			str->type = terme_escape_last;
			break;

		case 0x0C:  /* L */
			str->type = terme_escape_update;
			break;

		case 0x08:  /* H, backspace */
			str->type = terme_escape_backspace;
			break;

		case 0x15:  /* U, rmleft */
			str->type = terme_escape_rmleft;
			break;

		case 0x0B:  /* K, rmright */
			str->type = terme_escape_rmright;
			break;

		case 0x09:  /* I, tab */
			str->type = terme_escape_tab;
			break;

		case 0x1A:  /* Z */
			Return(terme_readline_ctrl_z_(str));
			break;

		default:
			str->type = terme_escape_error;
			break;
	}

	return terme_readline_loop_(ptr, str, value, ret);
}

static int terme_readline_write_line_(Execute ptr, int first)
{
	int i, check;
	unicode c;

	for (i = first; ; i++) {
		Return(terme_data_get_(ptr, i, &c, &check));
		if (check < 0)
			break;
		if (terme_write_char(c, check))
			return terme_fmte_("terme_write_char error.", NULL);
	}

	return 0;
}

static int terme_readline_code_(Execute ptr, TermeKeyboard *str, addr *value, int *ret)
{
	int check;
	unicode c;

	c = str->c;
	if (c < 0x20)
		return terme_readline_control_(ptr, str, value, ret);

	/* value */
	Return(terme_data_push_(ptr, terme_value_now, c, &check));
	if (check < 0)  /* buffer overflow */
		return Result(ret, 0);
	terme_value_width += check;

	/* output */
	if (terme_readline_write_line_(ptr, terme_value_now))
		return terme_fmte_("terme_write_char error.", NULL);
	if (terme_cursor_move(terme_prompt_width + terme_value_width))
		return terme_fmte_("terme_cursor_move error.", NULL);
	terme_value_now++;

	return Result(ret, 0);
}

static int terme_readline_up_down_(Execute ptr, int diff)
{
	int index, check;

	if (terme_history_now == 0) {
		Return(terme_history_save_(ptr));
	}
	index = terme_history_now + diff;
	Return(terme_history_update_(ptr, index, &check));
	if (! check)
		return 0;
	terme_history_now = index;

	/* output */
	Return(terme_data_size_width_(ptr, &terme_value_now, &terme_value_width));
	if (terme_cursor_delete_line())
		return terme_fmte_("terme_cursor_delete_line error.", NULL);
	if (terme_cursor_move(0))
		return terme_fmte_("terme_cursor_move error.", NULL);
	Return(terme_prompt_output_(ptr));
	Return(terme_readline_write_line_(ptr, 0));
	if (terme_finish_output())
		return terme_fmte_("terme_finish_output error.", NULL);

	return 0;
}

static int terme_readline_up_(Execute ptr)
{
	return terme_readline_up_down_(ptr, 1);
}

static int terme_readline_down_(Execute ptr)
{
	return terme_readline_up_down_(ptr, -1);
}

static int terme_readline_left_(Execute ptr)
{
	int check;

	if (terme_value_now == 0)
		return 0;
	Return(terme_data_get_width_(ptr, terme_value_now - 1UL, &check));
	if (check < 0)
		return terme_fmte_("terme_data_get_width_ error.", NULL);
	check = (check? 2: 1);
	if (terme_cursor_left(check))
		return terme_fmte_("terme_cursor_left error.", NULL);
	terme_value_now--;
	terme_value_width -= check;

	return 0;
}

static int terme_readline_right_(Execute ptr)
{
	int check;

	Return(terme_data_size_(ptr, &check));
	if (check <= terme_value_now)
		return 0;
	Return(terme_data_get_width_(ptr, terme_value_now, &check));
	if (check < 0)
		return terme_fmte_("terme_data_get_width_ error.", NULL);
	check = (check? 2: 1);
	if (terme_cursor_right(check))
		return terme_fmte_("terme_cursor_right error.", NULL);
	terme_value_now++;
	terme_value_width += check;

	return 0;
}

static int terme_readline_return(Execute ptr, addr *value, int *ret)
{
	terme_history_now = 0;
	Return(terme_data_make_(ptr, value));
	return Result(ret, 1);
}

static int terme_readline_backspace_(Execute ptr)
{
	int width, check;

	if (terme_value_now <= 0)
		return 0;
	Return(terme_data_get_width_(ptr, terme_value_now - 1UL, &width));
	if (width < 0)
		return terme_fmte_("terme_data_get_width_ error.", NULL);
	width = (width? 2: 1);

	/* backspace */
	Return(terme_data_delete_(ptr, terme_value_now - 1UL, &check));
	if (! check)
		return 0;
	terme_value_now--;
	terme_value_width -= width;

	/* output */
	if (terme_cursor_left(width))
		return terme_fmte_("terme_cursor_left error.", NULL);
	if (terme_cursor_delete_line_right())
		return terme_fmte_("terme_cursor_delete_line_right error.", NULL);
	Return(terme_readline_write_line_(ptr, terme_value_now));
	if (terme_cursor_move(terme_prompt_width + terme_value_width))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_first_(Execute ptr)
{
	terme_value_now = 0;
	terme_value_width = 0;
	if (terme_cursor_move(terme_prompt_width))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_last_(Execute ptr)
{
	int now, width;

	Return(terme_data_size_(ptr, &now));
	Return(terme_data_allwidth_(ptr, &width));
	terme_value_now = now;
	terme_value_width = width;
	if (terme_cursor_move(terme_prompt_width + width))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_update_(Execute ptr)
{
	int base;

	/* cursor position */
	base = terme_prompt_width + terme_value_width;

	/* all clean */
	if (terme_cursor_delete_page())
		return terme_fmte_("terme_cursor_delete_page error.", NULL);

	/* output prompt */
	Return(terme_prompt_output_(ptr));

	/* output text */
	Return(terme_readline_write_line_(ptr, 0));
	if (terme_cursor_move(base))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_delete_(Execute ptr, addr *value, int *ret)
{
	int check;

	/* exit */
	Return(terme_data_size_(ptr, &check));
	if (check == 0) {
		*value = Nil;
		return Result(ret, 1);
	}

	/* delete */
	Return(terme_data_delete_(ptr, terme_value_now, &check));
	if (! check)
		return 0;
	if (terme_cursor_delete_line_right())
		return terme_fmte_("terme_cursor_delete_line_right error.", NULL);
	Return(terme_readline_write_line_(ptr, terme_value_now));
	if (terme_cursor_move(terme_prompt_width + terme_value_width))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_rmleft_(Execute ptr)
{
	int check;

	/* data */
	Return(terme_data_delete_left_(ptr, terme_value_now, &check));
	if (! check)
		return 0;
	terme_value_now = 0;
	terme_value_width = 0;

	/* cursor */
	if (terme_cursor_move(terme_prompt_width))
		return terme_fmte_("terme_cursor_move error.", NULL);
	if (terme_cursor_delete_line_right())
		return terme_fmte_("terme_cursor_delete_line_right error.", NULL);
	Return(terme_readline_write_line_(ptr, 0));
	if (terme_cursor_move(terme_prompt_width))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_rmright_(Execute ptr)
{
	int check;

	Return(terme_data_delete_right_(ptr, terme_value_now, &check));
	if (! check)
		return 0;
	if (terme_cursor_delete_line_right())
		return terme_fmte_("terme_cursor_delete_line_right error.", NULL);

	return 0;
}

static int terme_readline_loop_(Execute ptr, TermeKeyboard *str, addr *value, int *ret)
{
	*ret = 0;
	switch (str->type) {
		case terme_escape_error:
			break;

		case terme_escape_code:
			return terme_readline_code_(ptr, str, value, ret);

		case terme_escape_up:
			return terme_readline_up_(ptr);

		case terme_escape_down:
			return terme_readline_down_(ptr);

		case terme_escape_left:
			return terme_readline_left_(ptr);

		case terme_escape_right:
			return terme_readline_right_(ptr);

		case terme_escape_return:
			return terme_readline_return(ptr, value, ret);

		case terme_escape_backspace:
			return terme_readline_backspace_(ptr);

		case terme_escape_first:
			return terme_readline_first_(ptr);

		case terme_escape_last:
			return terme_readline_last_(ptr);

		case terme_escape_update:
			return terme_readline_update_(ptr);

		case terme_escape_delete:
			return terme_readline_delete_(ptr, value, ret);

		case terme_escape_rmleft:
			return terme_readline_rmleft_(ptr);

		case terme_escape_rmright:
			return terme_readline_rmright_(ptr);

		case terme_escape_function:
		case terme_escape_tab:
			break; /* ignore */

		default:
			return terme_fmte_("terme_readline error.", NULL);
	}

	return 0;
}

static int terme_readline_call_(Execute ptr, addr *ret)
{
	int check;
	addr pos;
	TermeKeyboard str;

	/* loop */
	Return(terme_prompt_output_(ptr));
	pos = Nil;
	for (;;) {
		if (terme_read_keyboard(&str))
			continue;
		Return(terme_readline_loop_(ptr, &str, &pos, &check));
		if (check)
			break;
	}

	/* finish */
	if (pos != Nil) {
		if (terme_fresh_line())
			goto error;
	}
	if (terme_finish_output())
		goto error;
	return Result(ret, pos);

error:
	*ret = Nil;
	return terme_fmte_("terme output error.", NULL);
}

int terme_readline_(Execute ptr, addr *ret)
{
	int mode, check;

	/* initialize */
	terme_prompt_size = 0;
	terme_prompt_width = 0;
	terme_value_now = 0;
	terme_value_width = 0;
	terme_history_now = 0;
	Return(terme_data_init_(ptr));

	/* readline */
	if (terme_switch_rawmode(&mode)) {
		*ret = Nil;
		return terme_fmte_("terme_switch_rawmode error.", NULL);
	}
	check = terme_readline_call_(ptr, ret);
	if (mode && terme_switch_textmode(NULL)) {
		*ret = Nil;
		return terme_fmte_("terme_switch_textmode error.", NULL);
	}

	return check;
}


/************************************************************
  terme_value.c
 ************************************************************/

#define TERME_VALUE_SIZE		4096
#define TERME_VALUE_HISTORY		5

static int terme_history_index = 0;

enum terme_index {
	terme_index_prompt,
	terme_index_data,
	terme_index_width,
	terme_index_history,
	terme_index_size
};

struct terme_struct {
	enum prompt_mode mode;
};
#define PtrTerme(x) ((struct terme_struct *)PtrBodySS(x))

static struct terme_struct *struct_terme(addr pos)
{
	CheckType(pos, LISPSYSTEM_TERME);
	return PtrTerme(pos);
}

static void get_terme(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_TERME);
	GetArraySS(pos, index, ret);
}

static void set_terme(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_TERME);
	SetArraySS(pos, index, value);
}

static void terme_heap(addr *ret)
{
	addr pos;
	struct terme_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_TERME,
			terme_index_size, sizeoft(struct terme_struct));
	str = struct_terme(pos);
	str->mode = prompt_eval;
	*ret = pos;
}


/*
 *  build
 */
static int terme_value_array_(addr *ret)
{
	addr pos;
	struct array_struct *str;

	Return(array_heap_(&pos, 1, TERME_VALUE_SIZE));
	str = ArrayInfoStruct(pos);
	str->fillpointer = 1;
	Return(array_character_alloc_(NULL, pos));
	str->front = 0;

	return Result(ret, pos);
}

static int terme_value_heap_(addr *ret)
{
	addr pos, x;

	/* object */
	terme_heap(&pos);

	/* array */
	Return(terme_value_array_(&x));
	set_terme(pos, terme_index_data, x);

	/* width */
	bitmemory_heap(&x, TERME_VALUE_SIZE);
	set_terme(pos, terme_index_width, x);

	/* queue */
	vector_heap(&x, TERME_VALUE_HISTORY);
	setarray(pos, terme_index_history, x);

	return Result(ret, pos);
}

void terme_build(void)
{
	addr symbol, pos;

	GetConst(SYSTEM_TERME, &symbol);
	pos = Nil;
	Error(terme_value_heap_(&pos));
	SetValueSymbol(symbol, pos);
}

static int terme_get_value_(Execute ptr, addr *ret)
{
	addr symbol;
	GetConst(SYSTEM_TERME, &symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}


/*
 *  prompt
 */
int terme_set_prompt_(Execute ptr, addr value, enum prompt_mode mode)
{
	addr pos;
	struct terme_struct *str;

	Return(terme_get_value_(ptr, &pos));
	set_terme(pos, terme_index_prompt, value);
	str = struct_terme(pos);
	str->mode = mode;

	return 0;
}

int terme_get_prompt_(Execute ptr, addr *value, enum prompt_mode *mode)
{
	addr pos;
	struct terme_struct *str;

	Return(terme_get_value_(ptr, &pos));
	if (value) {
		get_terme(pos, terme_index_prompt, value);
	}
	if (mode) {
		str = struct_terme(pos);
		*mode = str->mode;
	}

	return 0;
}


/*
 *  data
 */
static int terme_data_array_(Execute ptr, addr *array, addr *width)
{
	addr pos;

	Return(terme_get_value_(ptr, &pos));
	get_terme(pos, terme_index_data, array);
	if (width) {
		get_terme(pos, terme_index_width, width);
	}

	return 0;
}

int terme_data_init_(Execute ptr)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, NULL));
	str = ArrayInfoStruct(pos);
	str->front = 0;

	return 0;
}

static int terme_data_shift_right_(addr pos, addr width, int x, int y)
{
	int i, size, src, dst, check;
	unicode c;

	if (x == y)
		return 0;
	size = y - x;
	for (i = 0; i < size; i++) {
		dst = y - i;
		src = dst - 1;

		Return(array_get_unicode_(pos, src, &c));
		Return(array_set_character_(pos, dst, c));

		Return(bitmemory_getint_(width, src, &check));
		Return(bitmemory_setint_(width, dst, check));
	}

	return 0;
}

int terme_data_push_(Execute ptr, int index, unicode c, int *ret)
{
	int check;
	addr pos, width;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, &width));
	str = ArrayInfoStruct(pos);
	if (str->size <= str->front)
		return Result(ret, -1);
	if (index < 0 || str->front < index)
		return Result(ret, -1);

	/* shift */
	Return(terme_data_shift_right_(pos, width, index, (int)str->front));

	/* set */
	Return(array_set_character_(pos, index, c));
	str->front++;

	/* width */
	check = (int)eastasian_width(c);
	Return(bitmemory_setint_(width, index, check == 2));

	return Result(ret, check);
}

int terme_data_get_(Execute ptr, int index, unicode *value, int *ret)
{
	addr pos, width;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, &width));
	str = ArrayInfoStruct(pos);
	if (index < 0 || str->front <= index)
		return Result(ret, -1);

	Return(bitmemory_getint_(width, index, ret));
	return array_get_unicode_(pos, (size_t)index, value);
}

int terme_data_get_width_(Execute ptr, int index, int *ret)
{
	addr pos, width;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, &width));
	str = ArrayInfoStruct(pos);
	if (index < 0 || str->front <= index)
		return Result(ret, -1);

	return bitmemory_getint_(width, index, ret);
}

int terme_data_size_(Execute ptr, int *ret)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, NULL));
	str = ArrayInfoStruct(pos);

	return Result(ret, (int)str->front);
}

int terme_data_size_width_(Execute ptr, int *size, int *width)
{
	addr pos;
	struct array_struct *str;
	size_t value;

	Return(terme_data_array_(ptr, &pos, NULL));
	Return(eastasian_length_(pos, &value, NULL));
	str = ArrayInfoStruct(pos);
	*size = (int)str->front;
	*width = (int)value;

	return 0;
}

int terme_data_allwidth_(Execute ptr, int *ret)
{
	int size, i, all, check;
	addr pos, width;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, &width));
	str = ArrayInfoStruct(pos);
	size = (int)str->front;
	all = 0;
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(width, i, &check));
		all += (check? 2: 1);
	}

	return Result(ret, all);
}

static int terme_data_shift_left_(addr pos, addr width, int x, int y)
{
	int check;
	unicode c;

	for (; x < y; x++) {
		Return(array_get_unicode_(pos, x + 1, &c));
		Return(array_set_character_(pos, x, c));

		Return(bitmemory_getint_(width, x + 1, &check));
		Return(bitmemory_setint_(width, x, check));
	}

	return 0;
}

int terme_data_delete_(Execute ptr, int index, int *ret)
{
	addr pos, width;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, &width));
	str = ArrayInfoStruct(pos);
	if (index < 0 || str->front <= index)
		return Result(ret, 0);
	/* shift */
	Return(terme_data_shift_left_(pos, width, index, (int)str->front));
	str->front--;

	return Result(ret, 1);
}

static int terme_data_delete_left_shift_(addr pos, addr width, int index, int size)
{
	int i, check;
	unicode c;

	for (i = 0; i < size; i++) {
		Return(array_get_unicode_(pos, i + index, &c));
		Return(array_set_character_(pos, i, c));

		Return(bitmemory_getint_(width, i + index, &check));
		Return(bitmemory_setint_(width, i, check));
	}

	return 0;
}

int terme_data_delete_left_(Execute ptr, int index, int *ret)
{
	int size;
	addr pos, width;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, &width));
	str = ArrayInfoStruct(pos);
	/* do nothing */
	if (index == 0)
		return Result(ret, 0);

	/* all delete */
	if (index == str->front) {
		str->front = 0;
		return Result(ret, 1);
	}

	/* invalid */
	if (index < 0 || str->front < index)
		return Result(ret, 0);

	/* shift */
	size = str->front - index;
	Return(terme_data_delete_left_shift_(pos, width, index, size));
	str->front = size;

	return Result(ret, 1);
}

int terme_data_delete_right_(Execute ptr, int index, int *ret)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, NULL));
	str = ArrayInfoStruct(pos);
	if (index < 0 || str->front <= index)
		return Result(ret, 0);
	/* shift */
	str->front = index;

	return Result(ret, 1);
}

static int terme_data_heap_(Execute ptr, addr *ret, int eol)
{
	addr pos, make;
	unicode c;
	struct array_struct *str;
	size_t size, i;

	Return(terme_data_array_(ptr, &pos, NULL));
	str = ArrayInfoStruct(pos);
	size = str->front;
	strvect_heap(&make, size + (eol? 1: 0));

	for (i = 0; i < size; i++) {
		Return(array_get_unicode_(pos, i, &c));
		Return(strvect_setc_(make, i, c));
	}
	if (eol) {
		Return(strvect_setc_(make, i, 0x0A));
	}

	return Result(ret, make);
}

static int terme_history_set_(Execute ptr, addr value)
{
	addr pos;

	Return(terme_get_value_(ptr, &pos));
	get_terme(pos, terme_index_history, &pos);
	setarray(pos, terme_history_index, value);

	return 0;
}

static int terme_history_next_(Execute ptr)
{
	int sizei;
	addr pos;
	size_t size;

	Return(terme_get_value_(ptr, &pos));
	get_terme(pos, terme_index_history, &pos);
	lenarray(pos, &size);
	sizei = (int)size;
	terme_history_index = (terme_history_index + 1) % sizei;

	return 0;
}

int terme_data_make_(Execute ptr, addr *ret)
{
	addr pos;

	/* history */
	Return(terme_data_heap_(ptr, &pos, 0));
	Return(terme_history_set_(ptr, pos));
	Return(terme_history_next_(ptr));

	/* result */
	return terme_data_heap_(ptr, ret, 1);
}

int terme_history_save_(Execute ptr)
{
	addr pos;
	Return(terme_data_heap_(ptr, &pos, 0));
	return terme_history_set_(ptr, pos);
}

static int terme_history_get_(Execute ptr, int index, addr *value, int *ret)
{
	int sizei;
	addr pos;
	size_t size;

	Return(terme_get_value_(ptr, &pos));
	get_terme(pos, terme_index_history, &pos);
	lenarray(pos, &size);
	sizei = (int)size;
	if (index < 0 || sizei <= index) {
		*value = Nil;
		return Result(ret, 0);
	}
	index = (sizei + terme_history_index - index) % sizei;
	getarray(pos, index, value);

	return Result(ret, 1);
}

static int terme_history_copy_(Execute ptr, addr pos)
{
	int check;
	unicode c;
	addr array, width;
	struct array_struct *str;
	size_t size, i;

	Return(terme_data_array_(ptr, &array, &width));
	str = ArrayInfoStruct(array);
	strvect_length(pos, &size);
	if (str->size < size)
		size = str->size;

	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &c);
		Return(array_set_character_(array, i, c));
		check = (int)eastasian_width(c);
		Return(bitmemory_setint_(width, i, check == 2));
	}
	str->front = size;

	return 0;
}

int terme_history_update_(Execute ptr, int index, int *ret)
{
	int check;
	addr pos;

	Return(terme_history_get_(ptr, index, &pos, &check));
	if ((! check) || pos == Nil)
		return Result(ret, 0);
	Return(terme_history_copy_(ptr, pos));

	return Result(ret, 1);
}


#else
/************************************************************
  terme_disable.c
 ************************************************************/

void init_terme(void)
{
}

void build_terme(void)
{
	addr symbol;
	GetConst(SYSTEM_TERME, &symbol);
	SetValueSymbol(symbol, Nil);
}

int begin_terme(void)
{
	return 0;
}

int end_terme(void)
{
	return 0;
}

int prompt_terme_(Execute ptr, addr pos, enum prompt_mode mode)
{
	addr symbol;

	Check(! stringp(pos), "type error");
	GetConst(SYSTEM_TERME, &symbol);
	setspecial_local(ptr, symbol, pos);

	return 0;
}

static int readline_terme_append_newline_(addr pos, addr *ret)
{
	unicode c;
	addr value;
	size_t size, i;

	strvect_length(pos, &size);
	strvect_heap(&value, size + 1UL);
	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &c);
		Return(strvect_setc_(value, i, c));
	}
	Return(strvect_setc_(value, i, 0x0A));

	return Result(ret, value);
}

int readline_terme_(Execute ptr, addr *ret)
{
	int check;
	addr input, output, prompt, pos;

	GetConst(STREAM_STDIN, &input);
	GetConst(STREAM_STDOUT, &output);
	GetConst(SYSTEM_TERME, &prompt);
	Return(getspecialcheck_local_(ptr, prompt, &prompt));
	Return(fresh_line_stream_(output, &check));
	if (prompt != Nil) {
		Return(princ_print(ptr, output, prompt));
	}
	Return(finish_output_stream_(output));
	Return(clear_input_stream_(input));
	Return(read_line_stream_(ptr, &pos, &check, input, 0, Nil, 0));
	if (pos == Nil)
		return Result(ret, Nil);
	Return(readline_terme_append_newline_(pos, &pos));
	return Result(ret, pos);
}

int font_terme(PrintFont value)
{
	return 0;
}

int text_color_terme(PrintColor value)
{
	return 0;
}

int back_color_terme(PrintColor value)
{
	return 0;
}


#endif


/************************************************************
 *  thread.c
 ************************************************************/


/*
 *  single
 */
#ifdef LISP_THREAD_SINGLE

/*
 *  mutexlite
 */
int lispd_make_mutexlite(mutexlite *mutex)
{
	*mutex = 0;
	return 0;
}

void lispd_destroy_mutexlite(mutexlite *mutex)
{
	if (*mutex) {
		Debug("lispd_destroy_mutexlite error.");
		lispd_threaderror();
	}
}

void lispd_lock_mutexlite(mutexlite *mutex)
{
	if (*mutex) {
		Debug("lispd_lock_mutexlite error.");
		lispd_threaderror();
	}
	*mutex = 1;
}

int lispd_trylock_mutexlite(mutexlite *mutex)
{
	if (*mutex) {
		Debug("lispd_trylock_mutexlite error.");
		lispd_threaderror();
	}

	return 0;
}

void lispd_unlock_mutexlite(mutexlite *mutex)
{
	if (*mutex == 0) {
		Debug("lispd_unlock_mutexlite error.");
		lispd_threaderror();
	}
	*mutex = 0;
}


/*
 *  rwlocklite
 */
int lispd_make_rwlocklite(rwlocklite *lock)
{
	*lock = 0;
	return 0;
}

void lispd_destroy_rwlocklite(rwlocklite *lock)
{
	if (*lock) {
		Debug("lispd_destroy_rwlocklite error.");
		lispd_threaderror();
	}
}

void lispd_rdlock_rwlocklite(rwlocklite *lock)
{
	if (*lock == 2) {
		Debug("lispd_rdlock_rwlocklite error.");
		lispd_threaderror();
	}
	*lock = 1;
}

void lispd_wrlock_rwlocklite(rwlocklite *lock)
{
	if (*lock) {
		Debug("lispd_wrlock_rwlocklite error.");
		lispd_threaderror();
	}
	*lock = 2;
}

int lispd_tryrdlock_rwlocklite(rwlocklite *lock)
{
	if (*lock == 2) {
		Debug("lispd_tryrdlock_rwlocklite error.");
		lispd_threaderror();
	}

	return 0;
}

int lispd_trywrlock_rwlocklite(rwlocklite *lock)
{
	if (*lock) {
		Debug("lispd_trywrlock_rwlocklite error.");
		lispd_threaderror();
	}

	return 0;
}

void lispd_unrdlock_rwlocklite(rwlocklite *lock)
{
	if (*lock != 1) {
		Debug("lispd_unrdlock_rwlocklite error.");
		lispd_threaderror();
	}
}

void lispd_unwrlock_rwlocklite(rwlocklite *lock)
{
	if (*lock != 2) {
		Debug("lispd_unwrlock_rwlocklite error.");
		lispd_threaderror();
	}
}


/*
 *  threadlocal
 */
void lispd_make_threadlocal(threadlocal *key)
{
	struct threadlocal_single *ptr;
	ptr = malloctype(struct threadlocal_single);
	ptr->value = 0;
	*key = ptr;
}

void lispd_destroy_threadlocal(threadlocal key)
{
	if (key == NULL) {
		Debug("lispd_destroy_threadlocal error");
		lispd_threaderror();
	}
	free(key);
}

const void *lispd_get_threadlocal(threadlocal key)
{
	if (key == NULL) {
		Debug("lispd_get_threadlocal error");
		lispd_threaderror();
	}
	return key->value;
}

void lispd_set_threadlocal(threadlocal key, const void *value)
{
	if (key == NULL) {
		Debug("lispd_set_threadlocal error");
		lispd_threaderror();
	}
	key->value = value;
}


/*
 *  binary semaphore
 */
void lispd_make_binsem(binsem *x)
{
	*x = 0;
}

void lispd_destroy_binsem(binsem *x)
{
}

void lispd_lock_binsem(binsem *x)
{
	if (*x) {
		Debug("lispd_lock_binsem error.");
		lispd_threaderror();
	}
	*x = 1;
}

int lispd_trylock_binsem(binsem *x)
{
	if (*x) {
		Debug("lispd_trylock_binsem error.");
		lispd_threaderror();
	}

	return 0;
}

void lispd_unlock_binsem(binsem *x)
{
	if (*x == 0) {
		Debug("lispd_unlock_mutexlite error.");
		lispd_threaderror();
	}
	*x = 0;
}


/*
 *  condition variable
 */
void lispd_make_condlite(condlite *x)
{
	*x = 0;
}

void lispd_destroy_condlite(condlite *x)
{
}

void lispd_wait_condlite(condlite *x, mutexlite *m)
{
}

void lispd_signal_condlite(condlite *x)
{
}

void lispd_broadcast_condlite(condlite *x)
{
}

#endif


/*
 *  disable
 */
#ifdef LISP_THREAD_DISABLE

int create_thread(execfunction proc, Execute arg)
{
	fprintf(stderr, "create_thread error\n");
	return 1;
}

int join_thread(threadhandle *handle)
{
	return 0;
}

#endif


/*
 *  posix
 */
#ifdef LISP_THREAD_POSIX

/*
 *  posix semaphore
 */
int lispd_trylock_semposix(semposix *sem)
{
	int result;

	result = sem_trywait(sem);
	if (result) {
		if (errno == EAGAIN) return 1;
		Debug("sem_trywait error");
		lispd_threaderror();
	}
	return 0;
}

int lispd_get_semposix(semposix *sem)
{
	int result;

	if (sem_getvalue(sem, &result)) {
		Debug("sem_post error");
		lispd_threaderror();
	}

	return result;
}


/*
 *  thread
 */
static void *start_routine(void *pvoid)
{
	Execute arg;

	arg = (Execute)pvoid;
	set_execute_local(arg);
	(*arg->routine)(arg);
	setstate_execute(arg, ThreadState_Finish);

	return NULL;
}

int create_thread(execfunction proc, Execute arg)
{
	if (pthread_create(&arg->handle, NULL, start_routine, (void *)arg)) {
		fprintf(stderr, "pthread_create error\n");
		return 1;
	}

	return 0;
}

int join_thread(threadhandle *handle)
{
	return pthread_join(*handle, NULL);
}

#endif


/*
 *  Windows
 */
#ifdef LISP_THREAD_WINDOWS

#undef USE_CLIB

#ifdef USE_CLIB
#define beginth(sec,size,p1,p2,s,id) _beginthreadex(sec,size,p1,p2,s,id)
#define endth(x) _endthreadex(x)
#else
#define beginth(sec,size,p1,p2,s,id) CreateThread(sec,size,p1,p2,s,id)
#define endth(x) ExitThread(x)
#endif


/*
 *  mutexlite
 */
int lispd_make_mutexlite(mutexlite *ptr)
{
	InitializeCriticalSection(ptr);
	return 0;
}

int lispd_make_rwlocklite(rwlocklite *ptr)
{
	InitializeSRWLock(ptr);
	return 0;
}


/*
 *  semaphore windows
 */
int lispd_trylock_semwindows(semwindows *ptr)
{
	DWORD result = WaitForSingleObject(*ptr, 0);
	if (result == WAIT_TIMEOUT) return 1;
	if (result != WAIT_OBJECT_0) {
		Debug("WaitForSingleObject (trylock semaphore) error");
		lispd_threaderror();
	}
	return 0;
}


/*
 *  binary semaphore  [condition variable]
 */
void lispd_make_binsemlite(binsemlite *ptr)
{
	lispd_make_mutexlite(&ptr->mutex);
	lispd_make_condlite(&ptr->cond);
	ptr->value = 1;
}
void lispd_destroy_binsemlite(binsemlite *ptr)
{
	lispd_destroy_condlite(&ptr->cond);
	lispd_destroy_mutexlite(&ptr->mutex);
}
void lispd_lock_binsemlite(binsemlite *ptr)
{
	lispd_lock_mutexlite(&ptr->mutex);
	while (ptr->value <= 0)
		lispd_wait_condlite(&ptr->cond, &ptr->mutex);
	ptr->value--;
	lispd_unlock_mutexlite(&ptr->mutex);
}
void lispd_unlock_binsemlite(binsemlite *ptr)
{
	lispd_lock_mutexlite(&ptr->mutex);
	/* binary semaphore check */
	if (1 <= ptr->value) {
		lispd_unlock_mutexlite(&ptr->mutex);
		Debug("lispd_unlock_binsemlite error");
		lispd_threaderror();
	}
	/* semaphore */
	if (ptr->value <= 0)
		lispd_signal_condlite(&ptr->cond);
	ptr->value++;
	lispd_unlock_mutexlite(&ptr->mutex);
}
int lispd_trylock_binsemlite(binsemlite *ptr)
{
	int result;

	lispd_lock_mutexlite(&ptr->mutex);
	if (ptr->value <= 0) {
		result = 1;
	}
	else {
		ptr->value--;
		result = 0;
	}
	lispd_unlock_mutexlite(&ptr->mutex);

	return result;
}


/*
 *  thread
 */
static DWORD WINAPI start_routine(LPVOID pvoid)
{
	Execute arg;

	arg = (Execute)pvoid;
	set_execute_local(arg);
	(*arg->routine)(arg);
	setstate_execute(arg, ThreadState_Finish);
	endth(0);

	return 0;
}

int create_thread(execfunction proc, Execute arg)
{
	HANDLE result;

	result = beginth(NULL, 0, start_routine, (LPVOID)arg,
			CREATE_SUSPENDED, &arg->handleid);
	if (result == NULL) {
		fprintf(stderr, "CreateThread error\n");
		return 1;
	}
	arg->handle = result;
	if (ResumeThread(result) < 0) {
		fprintf(stderr, "ResumeThread error\n");
		CloseHandle(result);
		return 1;
	}

	return 0;
}

int join_thread(threadhandle *handle)
{
	if (WaitForSingleObject(*handle, INFINITE) == WAIT_FAILED) {
		fprintf(stderr, "WaitForSingleObject error\n");
		return 1;
	}
	CloseHandle(*handle);

	return 0;
}

#endif


/*
 *  tools
 */
void lispd_threaderror(void)
{
	Abort("thread error");
}

void lispd_wrlock2_rwlocklite(rwlocklite *lock1, rwlocklite *lock2)
{
	if (lock1 == lock2) {
		lispd_wrlock_rwlocklite(lock1);
		return;
	}
	if (lock2 < lock1)
		goto trylock2;

trylock1:
	lispd_wrlock_rwlocklite(lock1);
	if (lispd_trywrlock_rwlocklite(lock2) == 0) return;
	lispd_unwrlock_rwlocklite(lock1);
trylock2:
	lispd_wrlock_rwlocklite(lock2);
	if (lispd_trywrlock_rwlocklite(lock1) == 0) return;
	lispd_unwrlock_rwlocklite(lock2);
	goto trylock1;
}

void lispd_unwrlock2_rwlocklite(rwlocklite *lock1, rwlocklite *lock2)
{
	if (lock1 == lock2) {
		lispd_unwrlock_rwlocklite(lock1);
	}
	else {
		lispd_unwrlock_rwlocklite(lock1);
		lispd_unwrlock_rwlocklite(lock2);
	}
}

#define SwapVariable(a,b,temp) { temp = a; a = b; b = temp; }
void lispd_wrlock3_rwlocklite(rwlocklite *m1, rwlocklite *m2, rwlocklite *m3)
{
	int check1, check2;
	rwlocklite *temp;

	check1 = (m1 == m2);
	check2 = (m2 == m3);
	if (check1 && check2) {
		lispd_wrlock_rwlocklite(m1);
	}
	else if (check1) {
		lispd_wrlock2_rwlocklite(m2, m3);
	}
	else if (check2) {
		lispd_wrlock2_rwlocklite(m3, m1);
	}
	else if (m3 == m1) {
		lispd_wrlock2_rwlocklite(m1, m2);
	}
	else {
		if (m2 < m1) SwapVariable(m2, m1, temp);
		if (m3 < m1) SwapVariable(m3, m1, temp);
		if (m3 < m2) SwapVariable(m3, m2, temp);

trylock1: /* m1->m2->m3 */
		lispd_wrlock_rwlocklite(m1);
		if (lispd_trywrlock_rwlocklite(m2) == 0) {
			if (lispd_trywrlock_rwlocklite(m3) == 0) return;
			lispd_unwrlock_rwlocklite(m2);
			lispd_unwrlock_rwlocklite(m1);
			goto trylock3;
		}
		lispd_unwrlock_rwlocklite(m1);

trylock2: /* m2->m3->m1 */
		lispd_wrlock_rwlocklite(m2);
		if (lispd_trywrlock_rwlocklite(m3) == 0) {
			if (lispd_trywrlock_rwlocklite(m1) == 0) return;
			lispd_unwrlock_rwlocklite(m3);
			lispd_unwrlock_rwlocklite(m2);
			goto trylock1;
		}
		lispd_unwrlock_rwlocklite(m2);

trylock3: /* m3->m1->m2 */
		lispd_wrlock_rwlocklite(m3);
		if (lispd_trywrlock_rwlocklite(m1) == 0) {
			if (lispd_trywrlock_rwlocklite(m2) == 0) return;
			lispd_unwrlock_rwlocklite(m1);
			lispd_unwrlock_rwlocklite(m3);
			goto trylock2;
		}
		lispd_unwrlock_rwlocklite(m3);
		goto trylock1;
	}
}

void lispd_unwrlock3_rwlocklite(rwlocklite *m1, rwlocklite *m2, rwlocklite *m3)
{
	int check1, check2;

	check1 = (m1 == m2);
	check2 = (m2 == m3);
	if (check1 && check2) {
		lispd_unwrlock_rwlocklite(m1);
	}
	else if (check1) {
		lispd_unwrlock_rwlocklite(m2);
		lispd_unwrlock_rwlocklite(m3);
	}
	else if (check2) {
		lispd_unwrlock_rwlocklite(m3);
		lispd_unwrlock_rwlocklite(m1);
	}
	else if (m3 == m1) {
		lispd_unwrlock_rwlocklite(m1);
		lispd_unwrlock_rwlocklite(m2);
	}
	else {
		lispd_unwrlock_rwlocklite(m1);
		lispd_unwrlock_rwlocklite(m2);
		lispd_unwrlock_rwlocklite(m3);
	}
}


/************************************************************
 *  token.c
 ************************************************************/
/*
 *  token
 */

#define FLOATSIZE		64
#define FLOATBUFFER		(FLOATSIZE * 2)

/*
 *  integer (fixnum, bignum)
 */
int getchar_digit(unsigned v, int upperp, unicode *ret)
{
	if (IntegerBaseMax < v)
		return 1;
	if (v < 10)
		*ret = '0' + v;
	else
		*ret = (upperp? 'A': 'a') + v - 10;

	return 0;
}

int getvalue_digit(unsigned base, unicode c, unsigned *ret)
{
	unsigned value;

	if (('0' <= c) && (c <= '9')) {
		value = c - '0';
	}
	else if ('a' <= c && c <= 'z') {
		value = c - 'a' + 10;
	}
	else if ('A' <= c && c <= 'Z') {
		value = c - 'A' + 10;
	}
	else {
		return 1;
	}
	if (base <= value) {
		return 1;
	}
	*ret = value;

	return 0;
}

unicode checkchar_digit(unsigned v, int upperp)
{
	unicode value;

	if (getchar_digit(v, upperp, &value)) {
		Abort("character error");
		return 0;
	}

	return value;
}

unsigned checkvalue_digit(unsigned base, unicode c)
{
	unsigned value;

	if (getvalue_digit(base, c, &value)) {
		Abort("character error");
		return 0;
	}

	return value;
}

void maketoken_integer(LocalRoot local, addr queue, unsigned base, addr *ret)
{
	int sign;
	size_t i, m, size, max;
	addr pos, cons;
	unicode u;
	LocalStack stack;

	GetCharQueueSize(queue, &size);
	GetCharQueueMax(queue, &max);
	GetCharQueueRoot(queue, &pos);
	push_local(local, &stack);
	bigcons_local(local, &cons);

	sign = 0;
	for (i = 0; i < size; i++) {
		m = i % max;
		if (i && m == 0)
			GetCharBitNext(pos, &pos);
		GetCharBitChar(pos, m, &u);
		if (u == '+' || u == '.')
			continue;
		if (u == '-') {
			sign = 1;
			continue;
		}
		push_bigcons(local, cons, base, checkvalue_digit(base, u));
	}
	integer_cons_heap(ret, sign, cons);
	rollback_local(local, stack);
}


/*
 *  float
 */
static void makefloat_buffer(int sign, const char *fract, int exp, char *ret)
{
	const char *tail;

	if (fract[0] == 0) {
		snprintf(ret, FLOATBUFFER, "%c0.0e0",
				(sign? '-': '+'));
	}
	else {
		tail = fract + 1;
		snprintf(ret, FLOATBUFFER, "%c%c.%se%d",
				(sign? '-': '+'),
				fract[0],
				(*fract && *tail)? tail: "0",
				exp);
	}
}

static int makefloat_single_(const char *ptr, addr *ret)
{
	single_float value;

	Return(check_strtof_(ptr, NULL, &value));
	single_float_heap(ret, value);

	return 0;
}

static int makefloat_double_(const char *ptr, addr *ret)
{
	double_float value;

	Return(check_strtod_(ptr, NULL, &value));
	double_float_heap(ret, value);

	return 0;
}

static int makefloat_long_(const char *ptr, addr *ret)
{
	long_float value;

	Return(check_strtold_(ptr, NULL, &value));
	long_float_heap(ret, value);

	return 0;
}

static int read_default_float_format_(Execute ptr, int *ret)
{
	addr symbol, check;

	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &symbol));
	GetConst(COMMON_SINGLE_FLOAT, &check);
	if (symbol == check)
		return Result(ret, 'f');
	GetConst(COMMON_DOUBLE_FLOAT, &check);
	if (symbol == check)
		return Result(ret, 'd');
	GetConst(COMMON_LONG_FLOAT, &check);
	if (symbol == check)
		return Result(ret, 'l');
	GetConst(COMMON_SHORT_FLOAT, &check);
	if (symbol == check)
		return Result(ret, 's');

	return Result(ret, 0); /* error */
}

static int makefloat_(Execute ptr,
		int sign, const char *fract, int v, int type, addr *ret)
{
	char buffer[FLOATBUFFER];

	makefloat_buffer(sign, fract, v, buffer);
	if (type == 'e') {
		Return(read_default_float_format_(ptr, &type));
	}
	switch (type) {
		case 's': /* short */
		case 'f': /* single */
			return makefloat_single_(buffer, ret);

		case 'd': /* double */
			return makefloat_double_(buffer, ret);

		case 'l': /* long */
			return makefloat_long_(buffer, ret);

		default: /* error */
			return Result(ret, Unbound);
	}
}

static int atolcheck(const char *str, long *value)
{
	long v;
	char *endp;

	errno = 0;
	v = strtol(str, &endp, 10);
	if (errno == ERANGE || errno == EINVAL)
		return 1;
	if (str == endp)
		return 1;
	*value = v;

	return 0;
}

static int plus_safe(long a, long b, long *result)
{
	if (((b > 0) && (a > (LONG_MAX - b)))
			|| ((b < 0) && (a < (LONG_MIN - b)))) {
		return 1;
	}
	*result = a + b;

	return 0;
}

static int isexponent(unicode c)
{
	/* "defslDEFSL" */
	if (c == 'e' || c == 'E')
		return 'e'; /* default */
	if (c == 'f' || c == 'F')
		return 'f'; /* single */
	if (c == 'd' || c == 'D')
		return 'd'; /* double */
	if (c == 'l' || c == 'L')
		return 'l'; /* long */
	if (c == 's' || c == 'S')
		return 's'; /* short */

	return 0;
}

#define NextChar() (i < size? str[i++]: 0)

static int floattable_(Execute ptr, const unicode *str, size_t size, addr *ret)
{
	int w, e, ds, zero, sign, type;
	long v;
	char fract[FLOATSIZE];
	char exp[FLOATSIZE];
	unicode c;
	size_t i;

	i = w = e = ds = 0;
	zero = 0;
	sign = 0;
	type = 'e';

	/* start */
	c = NextChar();
	if (c == '+') {
		sign = 0;
		c = NextChar();
	}
	else if (c == '-') {
		sign = 1;
		c = NextChar();
	}

first:
	if (c == 0)
		goto final;
	if (zero == 0 && c == '0') {
		c = NextChar();
		goto first;
	}
	if (isDigitCase(c)) {
		if (zero == 0)
			zero = 1;
		else
			ds++;
		if (w < FLOATSIZE) fract[w] = (char)c;
		w++;
		c = NextChar();
		goto first;
	}
	if (c == '.') {
		goto fract;
	}
	type = isexponent(c);
	if (type)
		goto exponent;
	goto error;

fract:
	c = NextChar();
	if (c == 0)
		goto final;
	if (zero == 0 && c == '0') {
		ds--;
		goto fract;
	}
	if (isDigitCase(c)) {
		if (zero == 0) {
			ds--;
			zero = 1;
		}
		if (w < FLOATSIZE) fract[w] = (char)c;
		w++;
		goto fract;
	}
	type = isexponent(c);
	if (type)
		goto exponent;
	goto error;

exponent:
	c = NextChar();
	if (c == '+') {
		if (e < FLOATSIZE) exp[e++] = '+';
		c = NextChar();
	}
	else if (c == '-') {
		if (e < FLOATSIZE) exp[e++] = '-';
		c = NextChar();
	}
	if (c == 0) {
		if (e < FLOATSIZE) exp[e++] = '0';
		goto final;
	}
	if (isDigitCase(c)) {
		if (e < FLOATSIZE) exp[e++] = (char)c;
		goto expnext;
	}
	goto error;

expnext:
	c = NextChar();
	if (c == 0)
		goto final;
	if (isDigitCase(c)) {
		if (e < FLOATSIZE) exp[e++] = (char)c;
		goto expnext;
	}
	goto error;

error:
	return Result(ret, Unbound);

final:
	if (zero == 0) {
		fract[0] = 0;
		return makefloat_(ptr, sign, fract, 0, type, ret);
	}
	fract[w < FLOATSIZE-1? w: FLOATSIZE-1] = 0;
	exp[e] = 0;

	v = 0;
	if (e && atolcheck(exp, &v))  /* atol(exp) error */
		return Result(ret, Unbound);
	if (plus_safe(v, ds, &v))  /* v += ds; overflow */
		return Result(ret, Unbound);

	return makefloat_(ptr, sign, fract, (int)v, type, ret);
}

int maketoken_float_(Execute ptr, addr queue, addr *ret)
{
	const unicode *body;
	size_t size;

	make_charqueue_heap(queue, &queue);
	strvect_posbodylen(queue, &body, &size);
	Return(floattable_(ptr, body, size, ret));
	if (*ret == Unbound)
		return fmte_("parse-float error", NULL);

	return 0;
}


/*
 *  ratio: [-+]?\\d+/\\d+
 */
void maketoken_ratio(LocalRoot local, addr queue, unsigned base, addr *ret)
{
	int sign;
	size_t i, m, size, max;
	addr pos, numer, denom, cons;
	unicode u;
	LocalStack stack;

	GetCharQueueSize(queue, &size);
	GetCharQueueMax(queue, &max);
	GetCharQueueRoot(queue, &pos);
	push_local(local, &stack);
	bigcons_local(local, &cons);

	/* numer */
	sign = signplus_bignum;
	for (i = 0; ; i++) {
		Check(size <= i, "size error");
		m = i % max;
		if (i && m == 0)
			GetCharBitNext(pos, &pos);
		GetCharBitChar(pos, m, &u);
		if (u == '+')
			continue;
		if (u == '-') {
			sign = signminus_bignum;
			continue;
		}
		if (u == '/') break;
		push_bigcons(local, cons, base, checkvalue_digit(base, u));
	}
	bignum_cons_alloc(local, &numer, signplus_bignum, cons);

	/* denom */
	clear_bigcons(cons);
	for (i++; i < size; i++) {
		m = i % max;
		if (i && m == 0)
			GetCharBitNext(pos, &pos);
		GetCharBitChar(pos, m, &u);
		push_bigcons(local, cons, base, checkvalue_digit(base, u));
	}
	bignum_cons_alloc(local, &denom, signplus_bignum, cons);

	/* result */
	ratio_reduction_heap(local, ret, sign, numer, denom);
	rollback_local(local, stack);
}


/************************************************************
 *  type.c
 ************************************************************/

/*
 *  check
 */
static int decl_function_p(LispDecl type)
{
	return type == LISPDECL_FUNCTION
		|| type == LISPDECL_COMPILED_FUNCTION;
}
static int decl_astert_p(LispDecl type)
{
	return type == LISPDECL_ASTERISK
		|| type == LISPDECL_T;
}

int decl_character_p(LispDecl type)
{
	return type == LISPDECL_CHARACTER
		|| type == LISPDECL_BASE_CHAR
		|| type == LISPDECL_STANDARD_CHAR
		|| type == LISPDECL_EXTENDED_CHAR;
}

int decl_float_p(LispDecl type)
{
	return type == LISPDECL_FLOAT
		|| type == LISPDECL_SINGLE_FLOAT
		|| type == LISPDECL_DOUBLE_FLOAT
		|| type == LISPDECL_LONG_FLOAT
		|| type == LISPDECL_SHORT_FLOAT;
}

int decl_range_p(LispDecl type)
{
	return type == LISPDECL_INTEGER
		|| type == LISPDECL_RATIONAL
		|| type == LISPDECL_REAL
		|| decl_float_p(type);
}

int decl_subtypep_real(LispDecl left, LispDecl right)
{
	switch (right) {
		case LISPDECL_INTEGER:
			return left == LISPDECL_INTEGER;

		case LISPDECL_RATIONAL:
			return left == LISPDECL_INTEGER
				|| left == LISPDECL_RATIONAL;

		case LISPDECL_REAL:
			return left == LISPDECL_INTEGER
				|| left == LISPDECL_RATIONAL
				|| left == LISPDECL_REAL
				|| decl_float_p(left);

		case LISPDECL_FLOAT:
			return decl_float_p(left);

		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
		case LISPDECL_SHORT_FLOAT:
			return left == right;

		default:
			break;
	}

	return 0;
}

int type_error_p(addr pos)
{
	return GetType(pos) == LISPTYPE_TYPE && RefLispDecl(pos) == LISPDECL_ERROR;
}

int type_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_TYPE && decl_function_p(LowLispDecl(pos));
}

int type_astert_p(addr pos)
{
	return GetType(pos) == LISPTYPE_TYPE && decl_astert_p(LowLispDecl(pos));
}

int type_function_aster_p(addr pos)
{
	LispDecl type;
	addr args, values;

	if (GetType(pos) != LISPTYPE_TYPE)
		return 0;
	type = LowLispDecl(pos);
	if (type != LISPDECL_FUNCTION && type != LISPDECL_COMPILED_FUNCTION)
		return 0;
	GetArrayType(pos, 0, &args);
	GetArrayType(pos, 1, &values);
	return type_asterisk_p(args) && type_asterisk_p(values);
}

int type_asterisk_p(addr pos)
{
	return GetType(pos) == LISPTYPE_TYPE && LowLispDecl(pos) == LISPDECL_ASTERISK;
}

int type_range_p(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	return decl_range_p(LowLispDecl(pos));
}

int type_string_p(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	switch (LowLispDecl(pos)) {
		case LISPDECL_STRING:
		case LISPDECL_BASE_STRING:
		case LISPDECL_SIMPLE_STRING:
		case LISPDECL_SIMPLE_BASE_STRING:
			return 1;

		default:
			return 0;
	}
}


/*
 *  init
 */
void init_type(void)
{
	init_type_coerce();
	init_type_copy();
	init_type_error();
	init_type_name();
	init_type_object();
	init_type_parse();
	init_type_symbol();
	init_type_typep();
	init_type_value();
}

void build_type(void)
{
	build_type_table();
	build_type_constant();
	build_type_upgraded();
	build_type_symbol();
	build_type_parse();
}


/************************************************************
 *  type_coerce.c
 ************************************************************/

static int coerce_type_(Execute ptr, addr pos, addr type, addr *ret);

/*
 *  type
 */
static int coerce_error(Execute ptr, addr pos, addr type)
{
	copyheap(&pos, pos);
	copyheap(&type, type);
	Return(type_object_(&type, type));
	return call_type_error_va_(ptr, pos, type,
			"Cannot covert value ~A to a ~S type.", pos, type, NULL);
}

static int coerce_typep(Execute ptr, addr pos, addr value, addr type, addr *ret)
{
	int check;

	Return(typep_clang_(ptr, value, type, &check));
	if (! check)
		return coerce_error(ptr, pos, type);

	return Result(ret, value);
}


/*
 *  float
 */
static int coerce_fixnum_single(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_FIXNUM);
	single_float_fixnum_heap(&value, pos);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_bignum_single(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(single_float_bignum_heap_(&value, pos));
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_ratio_single(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_RATIO);
	Return(single_float_ratio_heap_(&value, pos));
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_float(Execute ptr, addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
			return coerce_typep(ptr, pos, pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_single(ptr, pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_single(ptr, pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_single(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}


/*
 *  single-float
 */
static int coerce_double_single(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	single_float v;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	Return(cast_ds_value_(pos, &v));
	single_float_heap(&value, v);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_long_single(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	single_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	Return(cast_ls_value_(pos, &v));
	single_float_heap(&value, v);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_single(Execute ptr, addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return coerce_typep(ptr, pos, pos, type, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return coerce_double_single(ptr, pos, type, ret);

		case LISPTYPE_LONG_FLOAT:
			return coerce_long_single(ptr, pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_single(ptr, pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_single(ptr, pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_single(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}


/*
 *  double-float
 */
static int coerce_single_double(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	double_float v;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	Return(cast_sd_value_(pos, &v));
	double_float_heap(&value, v);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_long_double(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	double_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	Return(cast_ld_value_(pos, &v));
	double_float_heap(&value, v);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_fixnum_double(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_FIXNUM);
	double_float_fixnum_heap(&value, pos);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_bignum_double(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(double_float_bignum_heap_(&value, pos));
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_ratio_double(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_RATIO);
	Return(double_float_ratio_heap_(&value, pos));
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_double(Execute ptr, addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return coerce_single_double(ptr, pos, type, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return coerce_typep(ptr, pos, pos, type, ret);

		case LISPTYPE_LONG_FLOAT:
			return coerce_long_double(ptr, pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_double(ptr, pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_double(ptr, pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_double(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}


/*
 *  long-float
 */
static int coerce_single_long(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	long_float v;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	Return(cast_sl_value_(pos, &v));
	long_float_heap(&value, v);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_double_long(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	long_float v;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	Return(cast_dl_value_(pos, &v));
	long_float_heap(&value, v);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_fixnum_long(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_FIXNUM);
	long_float_fixnum_heap(&value, pos);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_bignum_long(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(long_float_bignum_heap_(&value, pos));
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_ratio_long(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_RATIO);
	Return(long_float_ratio_heap_(&value, pos));
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_long(Execute ptr, addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return coerce_single_long(ptr, pos, type, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return coerce_double_long(ptr, pos, type, ret);

		case LISPTYPE_LONG_FLOAT:
			return coerce_typep(ptr, pos, pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_long(ptr, pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_long(ptr, pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_long(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}


/*
 *  complex
 */
static int coerce_complex_complex(Execute ptr, addr pos, addr type, addr *ret)
{
	addr real, imag;
	LocalHold hold;

	GetArrayType(type, 0, &type);
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);

	Return(coerce_type_(ptr, real, type, &real));
	hold = LocalHold_local_push(ptr, real);
	Return(coerce_type_(ptr, imag, type, &imag));
	localhold_end(hold);
	Return(complex_heap_(ret, real, imag));

	return 0;
}

static int coerce_complex_real(Execute ptr, addr pos, addr type, addr *ret)
{
	GetArrayType(type, 0, &type);
	if (! type_asterisk_p(type)) {
		Return(coerce_type_(ptr, pos, type, &pos));
	}

	return complex_heap_(ret, pos, fixnumh(0));
}

static int coerce_complex(Execute ptr, addr pos, addr type, addr *ret)
{
	if (complexp(pos))
		return coerce_complex_complex(ptr, pos, type, ret);
	if (realp(pos))
		return coerce_complex_real(ptr, pos, type, ret);
	else
		return coerce_typep(ptr, pos, pos, type, ret);
}


/*
 *  charcter
 */
static int coerce_unicode_character(Execute ptr,
		addr pos, unicode c, addr type, addr *ret)
{
	addr value;
	character_heap(&value, c);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_string_character(Execute ptr, addr pos, addr type, addr *ret)
{
	unicode c;
	size_t size;

	string_length(pos, &size);
	if (size != 1)
		return coerce_error(ptr, pos, type);
	Return(string_getc_(pos, 0, &c));
	return coerce_unicode_character(ptr, pos, c, type, ret);
}

static int coerce_symbol_character(Execute ptr, addr pos, addr type, addr *ret)
{
	addr name;
	unicode c;
	size_t size;

	GetNameSymbol(pos, &name);
	string_length(name, &size);
	if (size != 1)
		return coerce_error(ptr, pos, type);
	Return(string_getc_(name, 0, &c));
	return coerce_unicode_character(ptr, pos, c, type, ret);
}

static int coerce_character(Execute ptr, addr pos, addr type, addr *ret)
{
	/* (or symbol string character) */
	if (stringp(pos))
		return coerce_string_character(ptr, pos, type, ret);
	else if (symbolp(pos))
		return coerce_symbol_character(ptr, pos, type, ret);
	else
		return coerce_typep(ptr, pos, pos, type, ret);
}


/*
 *  function
 */
static int coerce_function(Execute ptr, addr pos, addr type, addr *ret)
{
	addr call;

	if (symbolp(pos)) {
		Return(getfunction_global_(pos, &call));
		return coerce_typep(ptr, pos, call, type, ret);
	}
	else {
		return coerce_typep(ptr, pos, pos, type, ret);
	}
}


/*
 *  list
 */
static int coerce_vector_list(Execute ptr, addr pos, addr type, addr *ret)
{
	addr list, x;
	size_t size, i;

	lenarray(pos, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		getarray(pos, i, &x);
		cons_heap(&list, x, list);
	}
	nreverse(&list, list);

	return coerce_typep(ptr, pos, list, type, ret);
}

static int coerce_string_list(Execute ptr, addr pos, addr type, addr *ret)
{
	unicode c;
	addr list, x;
	size_t size, i;

	strvect_length(pos, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &c);
		character_heap(&x, c);
		cons_heap(&list, x, list);
	}
	nreverse(&list, list);

	return coerce_typep(ptr, pos, list, type, ret);
}

static int coerce_array_list(Execute ptr, addr pos, addr type, addr *ret)
{
	addr list, x;
	size_t size, i;

	/* not vector */
	if (! array_vector_p(pos))
		return coerce_typep(ptr, pos, type, type, ret);

	/* cast list */
	list = Nil;
	Return(array_get_vector_length_(pos, 1, &size));
	for (i = 0; i < size; i++) {
		Return(array_get_(NULL, pos, i, &x));
		cons_heap(&list, x, list);
	}
	nreverse(&list, list);

	return coerce_typep(ptr, pos, list, type, ret);
}

static int coerce_bitvector_list(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr list, x;
	size_t size, i;

	Return(bitvector_length_(pos, &size));
	list = Nil;
	for (i = 0; i < size; i++) {
		Return(bitvector_getint_(pos, i, &v));
		fixnum_heap(&x, (fixnum)v);
		cons_heap(&list, x, list);
	}
	nreverse(&list, list);

	return coerce_typep(ptr, pos, list, type, ret);
}

static int coerce_list(Execute ptr, addr pos, addr type, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_VECTOR:
			return coerce_vector_list(ptr, pos, type, ret);

		case LISPTYPE_STRING:
			return coerce_string_list(ptr, pos, type, ret);

		case LISPTYPE_ARRAY:
			return coerce_array_list(ptr, pos, type, ret);

		case LISPTYPE_BITVECTOR:
			return coerce_bitvector_list(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}


/*
 *  array
 */
/* array.bit -> array.t */
static int coerce_aa_bit_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int bit, check;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_bit_(pos, i, &bit, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		fixnum_heap(&value, (fixnum)bit);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.character -> array.t */
static int coerce_aa_character_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	unicode c;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_character_(pos, i, &c, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		character_heap(&value, c);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.signed8 -> array.t */
static int coerce_aa_signed8_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int8_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed8_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.signed16 -> array.t */
static int coerce_aa_signed16_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int16_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed16_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.signed32 -> array.t */
static int coerce_aa_signed32_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int32_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed32_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.signed64 -> array.t */
static int coerce_aa_signed64_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int64_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed64_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* array.signed -> array.t */
static int coerce_aa_signed_t(Execute ptr,
		addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_signed8_t(ptr, pos, type, ret);

		case 16:
			return coerce_aa_signed16_t(ptr, pos, type, ret);

		case 32:
			return coerce_aa_signed32_t(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_signed64_t(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* array.unsigned8 -> array.t */
static int coerce_aa_unsigned8_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint8_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned8_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.unsigned16 -> array.t */
static int coerce_aa_unsigned16_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint16_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned16_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.unsigned32 -> array.t */
static int coerce_aa_unsigned32_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint32_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned32_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
#ifdef LISP_64BIT
		fixnum_heap(&value, (fixnum)v);
#else
		integer_fixed_heap(&value, SignPlus, (fixed)v);
#endif
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.unsigned64 -> array.t */
static int coerce_aa_unsigned64_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint64_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned64_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		integer_fixed_heap(&value, SignPlus, (fixed)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* array.unsigned -> array.t */
static int coerce_aa_unsigned_t(Execute ptr,
		addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_unsigned8_t(ptr, pos, type, ret);

		case 16:
			return coerce_aa_unsigned16_t(ptr, pos, type, ret);

		case 32:
			return coerce_aa_unsigned32_t(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_unsigned64_t(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* array.single-float -> array.t */
static int coerce_aa_single_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	single_float v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_single_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		single_float_heap(&value, v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.double-float -> array.t */
static int coerce_aa_double_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	double_float v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_double_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		double_float_heap(&value, v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.long-float -> array.t */
static int coerce_aa_long_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	long_float v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_long_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		long_float_heap(&value, v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array -> array.t */
static int coerce_aa_t(Execute ptr, addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return coerce_typep(ptr, pos, pos, type, ret);

		case ARRAY_TYPE_BIT:
			return coerce_aa_bit_t(ptr, pos, type, ret);

		case ARRAY_TYPE_CHARACTER:
			return coerce_aa_character_t(ptr, pos, type, ret);

		case ARRAY_TYPE_SIGNED:
			return coerce_aa_signed_t(ptr, pos, type, str->bytesize, ret);

		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_unsigned_t(ptr, pos, type, str->bytesize, ret);

		case ARRAY_TYPE_SINGLE_FLOAT:
			return coerce_aa_single_t(ptr, pos, type, ret);

		case ARRAY_TYPE_DOUBLE_FLOAT:
			return coerce_aa_double_t(ptr, pos, type, ret);

		case ARRAY_TYPE_LONG_FLOAT:
			return coerce_aa_long_t(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array.t -> bitvector */
static int coerce_aa_bitvector(Execute ptr, addr pos, addr type, addr *ret)
{
	int v, check;
	addr vector;
	size_t size, i;

	array_get_rowlength(pos, &size);
	bitmemory_unsafe(NULL, &vector, size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_bit_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(bitmemory_setint_(vector, i, v));
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* array.* -> array.bit */
static int coerce_aa_type_bit(Execute ptr, addr pos, addr type, addr *ret)
{
	int v, check;
	addr array;
	size_t size, i;

	/* bit-vector */
	if (array_vector_p(pos))
		return coerce_aa_bitvector(ptr, pos, type, ret);

	/* array.bit */
	array_get_rowlength(pos, &size);
	Return(array_coerce_bit_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_bit_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_bit_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array -> array.bit */
static int coerce_aa_bit(Execute ptr, addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_type_bit(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array.t -> string */
static int coerce_aa_string(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	unicode v;
	addr vector;
	size_t size, i;

	array_get_rowlength(pos, &size);
	strvect_heap(&vector, size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_character_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(strvect_setc_(vector, i, v));
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* array.t -> array.character */
static int coerce_aa_t_character(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	unicode v;
	addr array;
	size_t size, i;

	/* bit-vector */
	if (array_vector_p(pos))
		return coerce_aa_string(ptr, pos, type, ret);

	/* array.bit */
	array_get_rowlength(pos, &size);
	Return(array_coerce_character_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_character_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_character_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array -> array.character */
static int coerce_aa_character(Execute ptr, addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return coerce_aa_t_character(ptr, pos, type, ret);

		case ARRAY_TYPE_CHARACTER:
		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array.* -> array.signed8 */
static int coerce_aa_signed8(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int8_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_signed8_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed8_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed8_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.* -> array.signed16 */
static int coerce_aa_signed16(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int16_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_signed16_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed16_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed16_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.* -> array.signed32 */
static int coerce_aa_signed32(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int32_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_signed32_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed32_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed32_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.* -> array.signed64 */
static int coerce_aa_signed64(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int64_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_signed64_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed64_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed64_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* array.* -> array.signed */
static int coerce_aa_type_signed(Execute ptr,
		addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_signed8(ptr, pos, type, ret);

		case 16:
			return coerce_aa_signed16(ptr, pos, type, ret);

		case 32:
			return coerce_aa_signed32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_signed64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* array -> array.signed */
static int type_second_size(addr type, unsigned *ret)
{
	enum LISPDECL decl;
	addr check;
	size_t size;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY)
		return 1;

	/* upgraded type check */
	GetArrayType(type, 0, &check);
	decl = LowLispDecl(check);
	if (decl != LISPDECL_SIGNED_BYTE && decl != LISPDECL_UNSIGNED_BYTE)
		return 1;

	/* (array (signed-byte size) *) */
	GetArrayType(check, 0, &check);
	if (GetIndex_integer(check, &size))
		return 1;

	switch (size) {
		case 8:
		case 16:
		case 32:
#ifdef LISP_64BIT
		case 64:
#endif
			*ret = (unsigned)size;
			return 0;

		default:
			return 1;
	}
}

static int coerce_aa_signed(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_type_signed(ptr, pos, type, size, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array.* -> array.unsigned8 */
static int coerce_aa_unsigned8(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint8_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_unsigned8_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned8_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned8_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.* -> array.unsigned16 */
static int coerce_aa_unsigned16(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint16_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_unsigned16_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned16_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned16_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.* -> array.unsigned32 */
static int coerce_aa_unsigned32(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint32_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_unsigned32_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned32_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned32_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.* -> array.unsigned64 */
static int coerce_aa_unsigned64(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint64_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_unsigned64_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned64_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned64_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* array.* -> array.unsigned */
static int coerce_aa_type_unsigned(Execute ptr,
		addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_unsigned8(ptr, pos, type, ret);

		case 16:
			return coerce_aa_unsigned16(ptr, pos, type, ret);

		case 32:
			return coerce_aa_unsigned32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_unsigned64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* array -> array.unsigned */
static int coerce_aa_unsigned(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_type_unsigned(ptr, pos, type, size, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array.float -> array.single */
static int coerce_aa_type_single(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	single_float v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_single_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_single_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_single_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array -> array.single-float */
static int coerce_aa_single(Execute ptr, addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return coerce_aa_type_single(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array.float -> array.double */
static int coerce_aa_type_double(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	double_float v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_double_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_double_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_double_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array -> array.double-float */
static int coerce_aa_double(Execute ptr, addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return coerce_aa_type_double(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array.float -> array.long */
static int coerce_aa_type_long(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	long_float v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_long_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_long_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_long_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array -> array.long-float */
static int coerce_aa_long(Execute ptr, addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
			return coerce_aa_type_long(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array -> array */
static int coerce_aa(Execute ptr, addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep(ptr, pos, pos, type, ret);

	/* (array upg .) */
	switch (LowLispDecl(upg)) {
		case LISPDECL_T:
			return coerce_aa_t(ptr, pos, type, ret);

		case LISPDECL_BIT:
			return coerce_aa_bit(ptr, pos, type, ret);

		case LISPDECL_CHARACTER:
			return coerce_aa_character(ptr, pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_aa_signed(ptr, pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_aa_unsigned(ptr, pos, type, ret);

		case LISPDECL_SINGLE_FLOAT:
			return coerce_aa_single(ptr, pos, type, ret);

		case LISPDECL_DOUBLE_FLOAT:
			return coerce_aa_double(ptr, pos, type, ret);

		case LISPDECL_LONG_FLOAT:
			return coerce_aa_long(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* vector -> array.bit */
static int coerce_av_bit(Execute ptr, addr pos, addr type, addr *ret)
{
	int v, check;
	addr vector;
	size_t size, i;

	lenarray(pos, &size);
	bitmemory_unsafe(NULL, &vector, size);
	for (i = 0; i < size; i++) {
		Return(vector_coerce_bit_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(bitmemory_setint_(vector, i, v));
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* vector -> array.character */
static int coerce_av_character(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	unicode v;
	addr vector;
	size_t size, i;

	lenarray(pos, &size);
	strvect_heap(&vector, size);
	for (i = 0; i < size; i++) {
		Return(vector_coerce_character_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(strvect_setc_(vector, i, v));
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* vector -> array.signed8 */
static int coerce_av_signed8(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int8_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_signed8_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_signed8_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed8_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* vector -> array.signed16 */
static int coerce_av_signed16(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int16_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_signed16_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_signed16_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed16_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* vector -> array.signed32 */
static int coerce_av_signed32(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int32_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_signed32_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_signed32_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed32_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* vector -> array.signed64 */
static int coerce_av_signed64(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int64_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_signed64_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_signed64_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed64_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* vector -> array.signed */
static int coerce_av_signed(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_av_signed8(ptr, pos, type, ret);

		case 16:
			return coerce_av_signed16(ptr, pos, type, ret);

		case 32:
			return coerce_av_signed32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_av_signed64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* vector -> array.unsigned8 */
static int coerce_av_unsigned8(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint8_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_unsigned8_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_unsigned8_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned8_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* vector -> array.unsigned16 */
static int coerce_av_unsigned16(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint16_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_unsigned16_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_unsigned16_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned16_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* vector -> array.unsigned32 */
static int coerce_av_unsigned32(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint32_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_unsigned32_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_unsigned32_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned32_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* vector -> array.unsigned64 */
static int coerce_av_unsigned64(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint64_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_unsigned64_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_unsigned64_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned64_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* vector -> array.unsigned */
static int coerce_av_unsigned(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_av_unsigned8(ptr, pos, type, ret);

		case 16:
			return coerce_av_unsigned16(ptr, pos, type, ret);

		case 32:
			return coerce_av_unsigned32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_av_unsigned64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* vector -> array.single-float */
static int coerce_av_single(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	single_float v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_single_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_single_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_single_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* vector -> array.double-float */
static int coerce_av_double(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	double_float v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_double_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_double_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_double_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* vector -> array.long-float */
static int coerce_av_long(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	long_float v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_long_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_long_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_long_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* vector -> array */
static int coerce_av(Execute ptr, addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep(ptr, pos, pos, type, ret);

	/* (array upg .) */
	switch (LowLispDecl(upg)) {
		case LISPDECL_T:
			return coerce_typep(ptr, pos, pos, type, ret);

		case LISPDECL_BIT:
			return coerce_av_bit(ptr, pos, type, ret);

		case LISPDECL_CHARACTER:
			return coerce_av_character(ptr, pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_av_signed(ptr, pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_av_unsigned(ptr, pos, type, ret);

		case LISPDECL_SINGLE_FLOAT:
			return coerce_av_single(ptr, pos, type, ret);

		case LISPDECL_DOUBLE_FLOAT:
			return coerce_av_double(ptr, pos, type, ret);

		case LISPDECL_LONG_FLOAT:
			return coerce_av_long(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* string -> array.t */
static int coerce_as_t(Execute ptr, addr pos, addr type, addr *ret)
{
	unicode c;
	addr vector, value;
	size_t size, i;

	strvect_length(pos, &size);
	vector_heap(&vector, size);
	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &c);
		character_heap(&value, c);
		setarray(vector, i, value);
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* string -> array */
static int coerce_as(Execute ptr, addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep(ptr, pos, pos, type, ret);

	/* (array upg .) */
	switch (LowLispDecl(upg)) {
		case LISPDECL_T:
			return coerce_as_t(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* bit-vector -> array.t */
static int coerce_ab_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr vector, value;
	size_t size, i;

	bitmemory_length(pos, &size);
	vector_heap(&vector, size);
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		fixnum_heap(&value, (fixnum)v);
		setarray(vector, i, value);
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* bit-vector -> array.signed8 */
static int coerce_ab_signed8(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_signed8_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_signed8_(array, i, (int8_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* bit-vector -> array.signed16 */
static int coerce_ab_signed16(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_signed16_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_signed16_(array, i, (int16_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* bit-vector -> array.signed32 */
static int coerce_ab_signed32(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_signed32_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_signed32_(array, i, (int32_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* bit-vector -> array.signed64 */
static int coerce_ab_signed64(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_signed64_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_signed64_(array, i, (int64_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* bit-vector -> array.signed */
static int coerce_ab_signed(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_ab_signed8(ptr, pos, type, ret);

		case 16:
			return coerce_ab_signed16(ptr, pos, type, ret);

		case 32:
			return coerce_ab_signed32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_ab_signed64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* bit-vector -> array.unsigned8 */
static int coerce_ab_unsigned8(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_unsigned8_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_unsigned8_(array, i, (uint8_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* bit-vector -> array.unsigned16 */
static int coerce_ab_unsigned16(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_unsigned16_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_unsigned16_(array, i, (uint16_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* bit-vector -> array.unsigned32 */
static int coerce_ab_unsigned32(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_unsigned32_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_unsigned32_(array, i, (uint32_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* bit-vector -> array.unsigned64 */
static int coerce_ab_unsigned64(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_unsigned64_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_unsigned64_(array, i, (uint64_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* bit-vector -> array.unsigned */
static int coerce_ab_unsigned(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_ab_unsigned8(ptr, pos, type, ret);

		case 16:
			return coerce_ab_unsigned16(ptr, pos, type, ret);

		case 32:
			return coerce_ab_unsigned32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_ab_unsigned64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* bit-vector -> array */
static int coerce_ab(Execute ptr, addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep(ptr, pos, pos, type, ret);

	/* (array upg .) */
	switch (LowLispDecl(upg)) {
		case LISPDECL_T:
			return coerce_ab_t(ptr, pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_ab_signed(ptr, pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_ab_unsigned(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* list -> array.bit */
static int coerce_al_t(Execute ptr, addr pos, addr type, addr *ret)
{
	addr vector, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	vector_heap(&vector, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		setarray(vector, i, value);
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* list -> array.bit */
static int coerce_al_bit(Execute ptr, addr pos, addr type, addr *ret)
{
	int v, check;
	addr vector, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	bitmemory_unsafe(NULL, &vector, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_bit_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(bitmemory_setint_(vector, i, v));
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* list -> array.character */
static int coerce_al_character(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	unicode v;
	addr vector, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	strvect_heap(&vector, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_character_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(strvect_setc_(vector, i, v));
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* list -> array.signed8 */
static int coerce_al_signed8(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int8_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_signed8_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_signed8_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed8_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* list -> array.signed16 */
static int coerce_al_signed16(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int16_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_signed16_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_signed16_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed16_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* list -> array.signed32 */
static int coerce_al_signed32(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int32_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_signed32_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_signed32_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed32_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* list -> array.signed64 */
static int coerce_al_signed64(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int64_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_signed64_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_signed64_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed64_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* list -> array.signed */
static int coerce_al_signed(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_al_signed8(ptr, pos, type, ret);

		case 16:
			return coerce_al_signed16(ptr, pos, type, ret);

		case 32:
			return coerce_al_signed32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_al_signed64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* list -> array.unsigned8 */
static int coerce_al_unsigned8(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint8_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_unsigned8_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_unsigned8_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned8_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* list -> array.unsigned16 */
static int coerce_al_unsigned16(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint16_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_unsigned16_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_unsigned16_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned16_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* list -> array.unsigned32 */
static int coerce_al_unsigned32(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint32_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_unsigned32_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_unsigned32_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned32_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* list -> array.unsigned64 */
static int coerce_al_unsigned64(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint64_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_unsigned64_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_unsigned64_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned64_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* list -> array.unsigned */
static int coerce_al_unsigned(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_al_unsigned8(ptr, pos, type, ret);

		case 16:
			return coerce_al_unsigned16(ptr, pos, type, ret);

		case 32:
			return coerce_al_unsigned32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_al_unsigned64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* list -> array.single-float */
static int coerce_al_single(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	single_float v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_single_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_single_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_single_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* list -> array.double-float */
static int coerce_al_double(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	double_float v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_double_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_double_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_double_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* list -> array.long-float */
static int coerce_al_long(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	long_float v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_long_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_long_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_long_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* list -> array */
static int coerce_al(Execute ptr, addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_al_t(ptr, pos, type, ret);

	/* (array upg .) */
	switch (LowLispDecl(upg)) {
		case LISPDECL_T:
			return coerce_al_t(ptr, pos, type, ret);

		case LISPDECL_BIT:
			return coerce_al_bit(ptr, pos, type, ret);

		case LISPDECL_CHARACTER:
			return coerce_al_character(ptr, pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_al_signed(ptr, pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_al_unsigned(ptr, pos, type, ret);

		case LISPDECL_SINGLE_FLOAT:
			return coerce_al_single(ptr, pos, type, ret);

		case LISPDECL_DOUBLE_FLOAT:
			return coerce_al_double(ptr, pos, type, ret);

		case LISPDECL_LONG_FLOAT:
			return coerce_al_long(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* ? -> array */
static int coerce_array(Execute ptr, addr pos, addr type, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return coerce_aa(ptr, pos, type, ret);

		case LISPTYPE_VECTOR:
			return coerce_av(ptr, pos, type, ret);

		case LISPTYPE_STRING:
			return coerce_as(ptr, pos, type, ret);

		case LISPTYPE_BITVECTOR:
			return coerce_ab(ptr, pos, type, ret);

		case LISPTYPE_CONS:
		case LISPTYPE_NIL:
			return coerce_al(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}


/*
 *  table
 */
typedef int (*coerce_call)(Execute ptr, addr pos, addr type, addr *ret);
static coerce_call CoerceTable[LISPDECL_SIZE];

void init_type_coerce(void)
{
	cleartype(CoerceTable);
	/* number */
	CoerceTable[LISPDECL_FLOAT] = coerce_float;
	CoerceTable[LISPDECL_SHORT_FLOAT] = coerce_single; /* single */
	CoerceTable[LISPDECL_SINGLE_FLOAT] = coerce_single;
	CoerceTable[LISPDECL_DOUBLE_FLOAT] = coerce_double;
	CoerceTable[LISPDECL_LONG_FLOAT] = coerce_long;
	CoerceTable[LISPDECL_COMPLEX] = coerce_complex;
	CoerceTable[LISPDECL_CHARACTER] = coerce_character;
	CoerceTable[LISPDECL_BASE_CHAR] = coerce_character;
	CoerceTable[LISPDECL_STANDARD_CHAR] = coerce_character;
	CoerceTable[LISPDECL_EXTENDED_CHAR] = coerce_character;
	CoerceTable[LISPDECL_FUNCTION] = coerce_function;
	CoerceTable[LISPDECL_COMPILED_FUNCTION] = coerce_function;
	/* list */
	CoerceTable[LISPDECL_LIST] = coerce_list;
	CoerceTable[LISPDECL_CONS] = coerce_list;
	/* array */
	CoerceTable[LISPDECL_ARRAY] = coerce_array;
	CoerceTable[LISPDECL_SIMPLE_ARRAY] = coerce_array;
}

static int coerce_table(Execute ptr, addr pos, addr type, addr *ret)
{
	enum LISPDECL decl;
	coerce_call call;

	CheckType(type, LISPTYPE_TYPE);
	if (! RefNotDecl(type)) {
		decl = LowLispDecl(type);
		/* call table */
		call = CoerceTable[(int)decl];
		if (call)
			return (*call)(ptr, pos, type, ret);
	}
	/* others */
	return Result(ret, Unbound);
}

static int coerce_optimize(Execute ptr, addr pos, addr type, addr *ret)
{
	int ignore;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	Return(type_optimize_local_(local, type, &type, &ignore));
	get_type_optimized(&type, type);
	Return(coerce_table(ptr, pos, type, ret));
	rollback_local(local, stack);

	return 0;
}

static int coerce_type_call(Execute ptr, addr pos, addr type, addr *ret)
{
	addr check;

	CheckType(type, LISPTYPE_TYPE);
	if (! RefNotDecl(type)) {
		Return(coerce_table(ptr, pos, type, &check));
		if (check != Unbound)
			return Result(ret, check);
	}
	Return(coerce_optimize(ptr, pos, type, &check));
	if (check != Unbound)
		return Result(ret, check);

	return coerce_typep(ptr, pos, pos, type, ret);
}

static int coerce_type_(Execute ptr, addr pos, addr type, addr *ret)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva(hold, pos, type, NULL);
	Return(coerce_type_call(ptr, pos, type, ret));
	localhold_end(hold);

	return 0;
}

static int coerce_parse(Execute ptr, addr pos, addr type, addr *ret)
{
	Return(parse_type(ptr, &type, type, Nil));
	return coerce_type_(ptr, pos, type, ret);
}

static int coerce_execute_(Execute ptr, addr pos, addr type, addr *ret)
{
	switch (GetType(type)) {
		case LISPTYPE_NIL:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_CONS:
			return coerce_parse(ptr, pos, type, ret);

		case LISPTYPE_TYPE:
			return coerce_type_(ptr, pos, type, ret);

		case LISPTYPE_T:
			return Result(ret, pos);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

int coerce_common(Execute ptr, addr pos, addr type, addr *ret)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva(hold, pos, type, NULL);
	Return(coerce_execute_(ptr, pos, type, ret));
	localhold_end(hold);

	return 0;
}


/************************************************************
 *  type_constant.c
 ************************************************************/

/*
 *  Atomic-Type
 */
static void typetable_type0(enum LISPDECL type, enum TypeTable table)
{
	addr pos;
	type0_heap(type, &pos);
	settypetable(table, pos);
}

static void typetable_type1(enum LISPDECL type, enum TypeTable table)
{
	addr pos;
	type1aster_heap(type, &pos);
	settypetable(table, pos);
}

static void typetable_type2(enum LISPDECL type, enum TypeTable table)
{
	addr pos;
	type2aster_heap(type, &pos);
	settypetable(table, pos);
}

static void typetable_type3(enum LISPDECL type, enum TypeTable table)
{
	addr pos;
	type3aster_heap(type, &pos);
	settypetable(table, pos);
}

static void typetable_type4(enum LISPDECL type, enum TypeTable table)
{
	addr pos;
	type4aster_heap(type, &pos);
	settypetable(table, pos);
}

#define TypeTable0(a,b) typetable_type0(LISPDECL_##a, TypeTable_##b)
#define TypeTable1(a,b) typetable_type1(LISPDECL_##a, TypeTable_##b)
#define TypeTable2(a,b) typetable_type2(LISPDECL_##a, TypeTable_##b)
#define TypeTable3(a,b) typetable_type3(LISPDECL_##a, TypeTable_##b)
#define TypeTable4(a,b) typetable_type4(LISPDECL_##a, TypeTable_##b)

#define DefTypeTable(n,a,b) \
	static void typetable_##b(void) { TypeTable##n(a,b); }

DefTypeTable(0,  INVALID,              Invalid              );
DefTypeTable(0,  ASTERISK,             Asterisk             );
DefTypeTable(0,  ATOM,                 Atom                 );
DefTypeTable(0,  LIST,                 List                 );
DefTypeTable(0,  BOOLEAN,              Boolean              );
DefTypeTable(1,  CLOS,                 Clos                 );
DefTypeTable(2,  VECTOR,               Vector               );
DefTypeTable(1,  SIMPLE_VECTOR,        SimpleVector         );
DefTypeTable(1,  BIT_VECTOR,           BitVector            );
DefTypeTable(1,  SIMPLE_BIT_VECTOR,    SimpleBitVector      );
DefTypeTable(0,  EXTENDED_CHAR,        ExtendedChar         );
DefTypeTable(1,  STRING,               String               );
DefTypeTable(1,  BASE_STRING,          BaseString           );
DefTypeTable(1,  SIMPLE_STRING,        SimpleString         );
DefTypeTable(1,  SIMPLE_BASE_STRING,   SimpleBaseString     );
DefTypeTable(1,  SIGNED_BYTE,          SignedByte           );
DefTypeTable(1,  UNSIGNED_BYTE,        UnsignedByte         );
DefTypeTable(0,  BIT,                  Bit                  );
DefTypeTable(0,  FIXNUM,               Fixnum               );
DefTypeTable(0,  BIGNUM,               Bignum               );
DefTypeTable(2,  CONS,                 Cons                 );
DefTypeTable(0,  HASH_TABLE,           Hashtable            );
DefTypeTable(0,  SYMBOL,               Symbol               );
DefTypeTable(0,  KEYWORD,              Keyword              );
DefTypeTable(0,  PACKAGE,              Package              );
DefTypeTable(0,  RANDOM_STATE,         RandomState          );
DefTypeTable(0,  READTABLE,            Readtable            );
DefTypeTable(3,  FUNCTION,             Function             ); /* not 2 */
DefTypeTable(3,  COMPILED_FUNCTION,    CompiledFunction     ); /* not 2 */
DefTypeTable(0,  PATHNAME,             Pathname             );
DefTypeTable(0,  LOGICAL_PATHNAME,     LogicalPathname      );
DefTypeTable(0,  SEQUENCE,             Sequence             );
DefTypeTable(2,  ARRAY,                Array                );
DefTypeTable(2,  SIMPLE_ARRAY,         SimpleArray          );
DefTypeTable(0,  CHARACTER,            Character            );
DefTypeTable(0,  BASE_CHAR,            BaseChar             );
DefTypeTable(0,  STANDARD_CHAR,        StandardChar         );
DefTypeTable(0,  NUMBER,               Number               );
DefTypeTable(4,  REAL,                 Real                 );
DefTypeTable(4,  RATIONAL,             Rational             );
DefTypeTable(0,  RATIO,                Ratio                );
DefTypeTable(4,  INTEGER,              Integer              );
DefTypeTable(1,  COMPLEX,              Complex              );
DefTypeTable(4,  FLOAT,                Float                );
DefTypeTable(4,  SHORT_FLOAT,          ShortFloat           );
DefTypeTable(4,  SINGLE_FLOAT,         SingleFloat          );
DefTypeTable(4,  DOUBLE_FLOAT,         DoubleFloat          );
DefTypeTable(4,  LONG_FLOAT,           LongFloat            );
DefTypeTable(0,  RESTART,              Restart              );
DefTypeTable(0,  ENVIRONMENT,          Environment          );
DefTypeTable(0,  STREAM,               Stream               );
DefTypeTable(0,  BROADCAST_STREAM,     BroadcastStream      );
DefTypeTable(0,  CONCATENATED_STREAM,  ConcatenatedStream   );
DefTypeTable(0,  ECHO_STREAM,          EchoStream           );
DefTypeTable(0,  FILE_STREAM,          FileStream           );
DefTypeTable(0,  STRING_STREAM,        StringStream         );
DefTypeTable(0,  SYNONYM_STREAM,       SynonymStream        );
DefTypeTable(0,  TWO_WAY_STREAM,       TwoWayStream         );
DefTypeTable(0,  PROMPT_STREAM,        PromptStream         );
DefTypeTable(0,  PRETTY_STREAM,        PrettyStream         );
DefTypeTable(0,  MEMORY_STREAM,        MemoryStream         );
DefTypeTable(0,  QUOTE,                Quote                );
DefTypeTable(0,  BYTESPEC,             ByteSpec             );
DefTypeTable(0,  PRINT_DISPATCH,       PrintDispatch        );
DefTypeTable(0,  EVAL,                 Eval                 );

static void typetable_Nil(void)
{
	typetable_type0(LISPDECL_NIL, TypeTable_Nil);
}

static void typetable_T(void)
{
	typetable_type0(LISPDECL_T, TypeTable_T);
}

static void typetable_Null(void)
{
	typetable_type0(LISPDECL_NULL, TypeTable_Null);
}


/*
 *  Condition
 */
static void define_type_table_condition(constindex index, enum TypeTable type)
{
	addr pos;

	GetConstant(index, &pos);
	CheckType(pos, LISPTYPE_CLOS);
	type_clos_heap(pos, &pos);
	settypetable(type, pos);
}
#define DefTypeTableCondition(x,y) \
	static void typetable_##y(void) { \
		define_type_table_condition(CONSTANT_CONDITION_##x, TypeTable_##y); \
	}

DefTypeTableCondition(ARITHMETIC_ERROR,    ArithmeticError);
DefTypeTableCondition(CELL_ERROR,          CellError);
DefTypeTableCondition(FILE_ERROR,          FileError);
DefTypeTableCondition(PACKAGE_ERROR,       PackageError);
DefTypeTableCondition(PRINT_NOT_READABLE,  PrintNotReadable);
DefTypeTableCondition(SIMPLE_CONDITION,    SimpleCondition);
DefTypeTableCondition(STREAM_ERROR,        StreamError);
DefTypeTableCondition(TYPE_ERROR,          TypeError);


/*
 *  Type
 */
static void typetable_cons2(addr car, addr cdr, addr *ret)
{
	type2_heap(LISPDECL_CONS, car, cdr, ret);
}

static void typetable_cxr(void)
{
	/* (or null cons) */
	addr pos, type;
	GetTypeTable(&pos, Null);
	GetTypeTable(&type, Cons);
	type2or_heap(pos, type, &pos);
	SetTypeTable(Cxr, pos);
}

static void type_cxr_carcdr(addr car, addr cdr, addr *ret)
{
	/* (or null (cons car cdr)) */
	addr pos, type;
	GetTypeTable(&pos, Null);
	typetable_cons2(car, cdr, &type);
	type2or_heap(pos, type, ret);
}

static addr type_list_car(addr type)
{
	addr pos;
	GetTypeTable(&pos, Asterisk);
	type_cxr_carcdr(type, pos, &pos);
	return pos;
}

static addr type_list_cdr(addr type)
{
	addr pos;
	GetTypeTable(&pos, Asterisk);
	type_cxr_carcdr(pos, type, &pos);
	return pos;
}

static void typetable_car(void)
{
	addr cxr = reftypetable(TypeTable_Cxr);
	addr cxar = type_list_car(cxr);
	addr cxdr = type_list_cdr(cxr);
	addr cxaar = type_list_car(cxar);
	addr cxadr = type_list_cdr(cxar);
	addr cxdar = type_list_car(cxdr);
	addr cxddr = type_list_cdr(cxdr);
	addr cxaaar = type_list_car(cxaar);
	addr cxaadr = type_list_cdr(cxaar);
	addr cxadar = type_list_car(cxadr);
	addr cxaddr = type_list_cdr(cxadr);
	addr cxdaar = type_list_car(cxdar);
	addr cxdadr = type_list_cdr(cxdar);
	addr cxddar = type_list_car(cxddr);
	addr cxdddr = type_list_cdr(cxddr);
	addr fifth = type_list_cdr(cxdddr);
	addr sixth = type_list_cdr(fifth);
	addr seventh = type_list_cdr(sixth);
	addr eighth = type_list_cdr(seventh);
	addr ninth = type_list_cdr(eighth);
	addr tenth = type_list_cdr(ninth);
	SetTypeTable(Cxar, cxar);
	SetTypeTable(Cxdr, cxdr);
	SetTypeTable(Cxaar, cxaar);
	SetTypeTable(Cxadr, cxadr);
	SetTypeTable(Cxdar, cxdar);
	SetTypeTable(Cxddr, cxddr);
	SetTypeTable(Cxaaar, cxaaar);
	SetTypeTable(Cxaadr, cxaadr);
	SetTypeTable(Cxadar, cxadar);
	SetTypeTable(Cxaddr, cxaddr);
	SetTypeTable(Cxdaar, cxdaar);
	SetTypeTable(Cxdadr, cxdadr);
	SetTypeTable(Cxddar, cxddar);
	SetTypeTable(Cxdddr, cxdddr);
	SetTypeTable(Fifth, fifth);
	SetTypeTable(Sixth, sixth);
	SetTypeTable(Seventh, seventh);
	SetTypeTable(Eighth, eighth);
	SetTypeTable(Ninth, ninth);
	SetTypeTable(Tenth, tenth);
}

static void type_setf_cxr_carcdr(addr car, addr cdr, addr *ret)
{
	/* (cons car cdr) */
	typetable_cons2(car, cdr, ret);
}

static addr type_cons_car(addr type)
{
	addr pos;
	GetTypeTable(&pos, Asterisk);
	type_setf_cxr_carcdr(type, pos, &pos);
	return pos;
}

static addr type_cons_cdr(addr type)
{
	addr pos;
	GetTypeTable(&pos, Asterisk);
	type_setf_cxr_carcdr(pos, type, &pos);
	return pos;
}

static void typetable_setf_car(void)
{
	addr cxr = reftypetable(TypeTable_Cons);
	addr cxar = type_cons_car(cxr);
	addr cxdr = type_cons_cdr(cxr);
	addr cxaar = type_cons_car(cxar);
	addr cxadr = type_cons_cdr(cxar);
	addr cxdar = type_cons_car(cxdr);
	addr cxddr = type_cons_cdr(cxdr);
	addr cxaaar = type_cons_car(cxaar);
	addr cxaadr = type_cons_cdr(cxaar);
	addr cxadar = type_cons_car(cxadr);
	addr cxaddr = type_cons_cdr(cxadr);
	addr cxdaar = type_cons_car(cxdar);
	addr cxdadr = type_cons_cdr(cxdar);
	addr cxddar = type_cons_car(cxddr);
	addr cxdddr = type_cons_cdr(cxddr);
	addr fifth = type_cons_cdr(cxdddr);
	addr sixth = type_cons_cdr(fifth);
	addr seventh = type_cons_cdr(sixth);
	addr eighth = type_cons_cdr(seventh);
	addr ninth = type_cons_cdr(eighth);
	addr tenth = type_cons_cdr(ninth);
	SetTypeTable(SetfCxar, cxar);
	SetTypeTable(SetfCxdr, cxdr);
	SetTypeTable(SetfCxaar, cxaar);
	SetTypeTable(SetfCxadr, cxadr);
	SetTypeTable(SetfCxdar, cxdar);
	SetTypeTable(SetfCxddr, cxddr);
	SetTypeTable(SetfCxaaar, cxaaar);
	SetTypeTable(SetfCxaadr, cxaadr);
	SetTypeTable(SetfCxadar, cxadar);
	SetTypeTable(SetfCxaddr, cxaddr);
	SetTypeTable(SetfCxdaar, cxdaar);
	SetTypeTable(SetfCxdadr, cxdadr);
	SetTypeTable(SetfCxddar, cxddar);
	SetTypeTable(SetfCxdddr, cxdddr);
	SetTypeTable(SetfFifth, fifth);
	SetTypeTable(SetfSixth, sixth);
	SetTypeTable(SetfSeventh, seventh);
	SetTypeTable(SetfEighth, eighth);
	SetTypeTable(SetfNinth, ninth);
	SetTypeTable(SetfTenth, tenth);
}

static void typetable_ornull(enum TypeTable type, enum TypeTable typenull)
{
	addr pos, null;

	gettypetable(type, &pos);
	GetTypeTable(&null, Null);
	type2or_heap(pos, null, &pos);
	settypetable(typenull, pos);
}
#define SetTypeTableNull(a) typetable_ornull(TypeTable_##a, TypeTable_##a##Null)

static void typetable_characternull(void)
{
	SetTypeTableNull(Character);
}

static void typetable_stringnull(void)
{
	SetTypeTableNull(String);
}

static void typetable_streamnull(void)
{
	SetTypeTableNull(Stream);
}

static void typetable_condition(void)
{
	addr pos;

	GetConst(CLOS_CONDITION, &pos);
	CheckType(pos, LISPTYPE_CLOS);
	type_clos_heap(pos, &pos);
	SetTypeTable(Condition, pos);
}

static void typetable_conditionnull(void)
{
	SetTypeTableNull(Condition);
}

static void typetable_restartnull(void)
{
	SetTypeTableNull(Restart);
}

static void typetable_functionnull(void)
{
	SetTypeTableNull(Function);
}

static void typetable_environmentnull(void)
{
	SetTypeTableNull(Environment);
}

static void typetable_integernull(void)
{
	SetTypeTableNull(Integer);
}

static void typetable_pathnamenull(void)
{
	SetTypeTableNull(Pathname);
}

static void typetable_packagenull(void)
{
	SetTypeTableNull(Package);
}

static void typetable_printdispatchnull(void)
{
	SetTypeTableNull(PrintDispatch);
}

static void typetable_stringdesigner(void)
{
	/* (or string symbol character) */
	addr type1, type2, type3, pos;

	GetTypeTable(&type1, String);
	GetTypeTable(&type2, Symbol);
	GetTypeTable(&type3, Character);
	type3or_heap(type1, type2, type3, &pos);
	SetTypeTable(StringDesigner, pos);
}

static void typetable_packagedesigner(void)
{
	/* (or package string symbol character) */
	addr type1, type2, type3, type4, pos;

	GetTypeTable(&type1, Package);
	GetTypeTable(&type2, String);
	GetTypeTable(&type3, Symbol);
	GetTypeTable(&type4, Character);
	type4or_heap(type1, type2, type3, type4, &pos);
	SetTypeTable(PackageDesigner, pos);
}

static void typetable_packagedesignernull(void)
{
	SetTypeTableNull(PackageDesigner);
}

static void typetable_functiondesigner(void)
{
	/* (or function symbol) */
	addr type1, type2, pos;

	GetTypeTable(&type1, Function);
	GetTypeTable(&type2, Symbol);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(FunctionDesigner, pos);
}

static void typetable_restartdesigner(void)
{
	/* (or restart (and symbol (not null))) */
	addr type1, type2, type3, pos;

	GetTypeTable(&type1, Restart);
	GetTypeTable(&type2, Symbol);
	type0not_heap(LISPDECL_NULL, &type3);
	type2and_heap(type2, type3, &type2);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(RestartDesigner, pos);
}

static void typetable_pathnamedesigner(void)
{
	/* (or pathname string stream) */
	addr type1, type2, type3, pos;

	GetTypeTable(&type1, Pathname);
	GetTypeTable(&type2, String);
	GetTypeTable(&type3, Stream);
	type3or_heap(type1, type2, type3, &pos);
	SetTypeTable(PathnameDesigner, pos);
}

static void typetable_streamdesigner(void)
{
	/* (or stream boolean) */
	addr type1, type2, pos;

	GetTypeTable(&type1, Stream);
	GetTypeTable(&type2, Boolean);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(StreamDesigner, pos);
}

static void typetable_readtabledesigner(void)
{
	/* (or readtable null) */
	addr type1, type2, pos;

	GetTypeTable(&type1, Readtable);
	GetTypeTable(&type2, Null);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(ReadtableDesigner, pos);
}

static void typetable_conditiondesigner(void)
{
	/* (or string symbol condition) */
	addr type1, type2, type3, pos;

	GetTypeTable(&type1, String);
	GetTypeTable(&type2, Symbol);
	GetTypeTable(&type3, Condition);
	type3or_heap(type1, type2, type3, &pos);
	SetTypeTable(ConditionDesigner, pos);
}

static void typetable_index(void)
{
	/* (integer 0 SIZE_MAX) */
	addr left, right;

	fixnum_heap(&left, 0);
	GetConst(INDEX_MAX, &right);
	type4_heap(LISPDECL_INTEGER, Nil, left, Nil, right, &left);
	SetTypeTable(Index, left);
}

static void typetable_indexnull(void)
{
	SetTypeTableNull(Index);
}

static void typetable_plus1(void)
{
	addr pos;

	type2integer_ab_heap(Nil, 1, &pos);
	SetTypeTable(Plus1, pos);
}

static void typetable_plus1null(void)
{
	SetTypeTableNull(Plus1);
}

static void typetable_intplus(void)
{
	addr pos;

	type2integer_ab_heap(Nil, 0, &pos);
	SetTypeTable(Intplus, pos);
}

static void typetable_intplusnull(void)
{
	SetTypeTableNull(Intplus);
}

static void typetable_input_stream(void)
{
	addr pos;

	GetConst(COMMON_INPUT_STREAM_P, &pos);
	type_satisfies_heap(pos, &pos);
	SetTypeTable(InputStream, pos);
}

static void typetable_output_stream(void)
{
	addr pos;

	GetConst(COMMON_OUTPUT_STREAM_P, &pos);
	type_satisfies_heap(pos, &pos);
	SetTypeTable(OutputStream, pos);
}

static void typetable_typespec(void)
{
	/* (or symbol cons clos) */
	addr type1, type2, type3, pos;

	GetTypeTable(&type1, Symbol);
	GetTypeTable(&type2, Cons);
	GetTypeTable(&type3, Clos);
	type3or_heap(type1, type2, type3, &pos);
	SetTypeTable(TypeSpec, pos);
}

static void typetable_typesymbol(void)
{
	/* (or symbol cons) */
	addr type1, type2, pos;

	GetTypeTable(&type1, Symbol);
	GetTypeTable(&type2, Cons);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(TypeSymbol, pos);
}

static void typetable_bit_array(void)
{
	addr pos, aster;

	GetTypeTable(&pos, Bit);
	GetTypeTable(&aster, Asterisk);
	type2_heap(LISPDECL_ARRAY, pos, aster, &pos);
	SetTypeTable(BitArray, pos);
}

static void typetable_simple_bit_array(void)
{
	addr pos, aster;

	GetTypeTable(&pos, Bit);
	GetTypeTable(&aster, Asterisk);
	type2_heap(LISPDECL_SIMPLE_ARRAY, pos, aster, &pos);
	SetTypeTable(SimpleBitArray, pos);
}

static void typetable_keywordstart(void)
{
	addr pos;
	GetTypeTable(&pos, Index);
	SetTypeTable(KeywordStart, pos);
}

static void typetable_keywordend(void)
{
	addr pos;
	GetTypeTable(&pos, IndexNull);
	SetTypeTable(KeywordEnd, pos);
}

static void typetable_keystart1end1(void)
{
	/* &key (start keyword-start) (end keyword-end) */
	addr key, key1, key2, type;

	/* :start1 */
	GetConst(KEYWORD_START, &key1);
	GetTypeTable(&type, KeywordStart);
	cons_heap(&key1, key1, type);
	/* :end1 */
	GetConst(KEYWORD_END, &key2);
	GetTypeTable(&type, KeywordEnd);
	cons_heap(&key2, key2, type);
	/* &key ... */
	list_heap(&key, key1, key2, NULL);
	SetTypeTable(KeyStart1End1, key);
}

static void typetable_keystart2end2(void)
{
	/* &key (start1 keyword-start) (end1 keyword-end)
	 *      (start2 keyword-start) (end2 keyword-end))
	 */
	addr key, key1, key2, key3, key4, start, end;

	GetTypeTable(&start, KeywordStart);
	GetTypeTable(&end, KeywordEnd);
	/* :start1 */
	GetConst(KEYWORD_START1, &key1);
	cons_heap(&key1, key1, start);
	/* :end1 */
	GetConst(KEYWORD_END1, &key2);
	cons_heap(&key2, key2, end);
	/* :start2 */
	GetConst(KEYWORD_START2, &key3);
	cons_heap(&key3, key3, start);
	/* :end2 */
	GetConst(KEYWORD_END2, &key4);
	cons_heap(&key4, key4, end);
	/* &key ... */
	list_heap(&key, key1, key2, key3, key4, NULL);
	SetTypeTable(KeyStart2End2, key);
}

static void typetable_functionname(void)
{
	/* (or symbol (setf symbol)) */
	/* (or symbol (cons (eql setf) (cons symbol null))) */
	addr symbol, setf, pos, cons;

	/* (cons symbol null) */
	GetTypeTable(&symbol, Symbol);
	GetTypeTable(&pos, Null);
	typetable_cons2(symbol, pos, &cons);
	/* (cons (eql 'setf) [cons]) */
	GetConst(COMMON_SETF, &setf);
	type_eql_heap(setf, &setf);
	typetable_cons2(setf, cons, &pos);
	type2or_heap(symbol, pos, &pos);
	SetTypeTable(FunctionName, pos);
}

static void typetable_radixinteger(void)
{
	/* (integer 2 36) */
	addr pos;
	type4integer_heap(Nil, 2, Nil, 36, &pos);
	SetTypeTable(RadixInteger, pos);
}

static void typetable_floatsymbol(void)
{
	addr pos, v1, v2, v3, v4;

	/* (member short-float single-float double-float long-float) */
	GetConst(COMMON_SINGLE_FLOAT, &v1);
	GetConst(COMMON_DOUBLE_FLOAT, &v2);
	GetConst(COMMON_LONG_FLOAT, &v3);
	GetConst(COMMON_SHORT_FLOAT, &v4);
	type_member_heap(&pos, v1, v2, v3, v4, NULL);
	SetTypeTable(FloatSymbol, pos);
}

static void typetable_eqlt(void)
{
	addr pos;
	type_eql_heap(T, &pos);
	SetTypeTable(EqlT, pos);
}

static void typetable_case_sensitivity(void)
{
	addr pos, v1, v2, v3, v4;

	GetConst(KEYWORD_UPCASE, &v1);
	GetConst(KEYWORD_DOWNCASE, &v2);
	GetConst(KEYWORD_PRESERVE, &v3);
	GetConst(KEYWORD_INVERT, &v4);
	type_member_heap(&pos, v1, v2, v3, v4, NULL);
	SetTypeTable(CaseSensitivity, pos);
}

static void typetable_print_case(void)
{
	addr key1, key2, key3;

	GetConst(KEYWORD_UPCASE, &key1);
	GetConst(KEYWORD_DOWNCASE, &key2);
	GetConst(KEYWORD_CAPITALIZE, &key3);
	type_member_heap(&key1, key1, key2, key3, NULL);
	SetTypeTable(PrintCase, key1);
}


static void typetable_keytestlist(void)
{
	/* &key (:key      [function-designer])
	 *      (:test     [function-designer])
	 *      (:test-not [function-designer])
	 */
	addr key, key1, key2, key3, type;

	GetConst(KEYWORD_KEY, &key1);
	GetConst(KEYWORD_TEST, &key2);
	GetConst(KEYWORD_TEST_NOT, &key3);
	GetTypeTable(&type, FunctionDesigner);
	/* key */
	cons_heap(&key1, key1, type);
	cons_heap(&key2, key2, type);
	cons_heap(&key3, key3, type);
	list_heap(&key, key1, key2, key3, NULL);
	/* result */
	SetTypeTable(KeyTestList, key);
}

static void typetable_rehashsize(void)
{
	/* (or (integer 1 *) (float (1.0f0) *)) */
	addr type1, type2, pos;

	type2integer_ab_heap(Nil, 1, &type1);
	type2float_ab_heap(T, 1.0f, &type2);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(RehashSize, pos);
}

static void typetable_rehashthreshold(void)
{
	/* (real 0.0 1.0) */
	addr pos;

	type4realf_heap(Nil, 0.0f, Nil, 1.0f, &pos);
	SetTypeTable(RehashThreshold, pos);
}

static void typetable_countkey(void)
{
	addr key, key1, key2, key3, key4, key5, key6;

	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, START, KeywordStart);
	KeyTypeTable(&key3, END, KeywordEnd);
	KeyTypeTable(&key4, KEY, FunctionDesigner);
	KeyTypeTable(&key5, TEST, FunctionDesigner);
	KeyTypeTable(&key6, TEST_NOT, FunctionDesigner);
	list_heap(&key, key1, key2, key3, key4, key5, key6, NULL);
	SetTypeTable(CountKey, key);
}

static void typetable_countifkey(void)
{
	addr key, key1, key2, key3, key4;

	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, START, KeywordStart);
	KeyTypeTable(&key3, END, KeywordEnd);
	KeyTypeTable(&key4, KEY, FunctionDesigner);
	list_heap(&key, key1, key2, key3, key4, NULL);
	SetTypeTable(CountIfKey, key);
}

static void typetable_pathnamehost(void)
{
	/* host       (or string symbol) */
	addr type1, type2, pos;

	GetTypeTable(&type1, Symbol);
	GetTypeTable(&type2, String);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(PathnameHost, pos);
}

static void typetable_pathnamedevice(void)
{
	/* device     (or string symbol)  ;; (eql :unspecific) */
	addr type;

	GetTypeTable(&type, PathnameHost);
	SetTypeTable(PathnameDevice, type);
}

static void typetable_pathnamedirectory(void)
{
	/* directory  (or list string (member :wild :wild-inferiors :unspecific)) */
	addr type, list, string, wild, wild_inferiors, unspec;

	GetTypeTable(&list, List);
	GetTypeTable(&string, String);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wild_inferiors);
	GetConst(KEYWORD_UNSPECIFIC, &unspec);
	type_member_heap(&type, wild, wild_inferiors, unspec, NULL);
	type3or_heap(list, string, type, &type);
	SetTypeTable(PathnameDirectory, type);
}

static void typetable_pathnamename(void)
{
	/* name       (or string cons (member nil :wild)) */
	addr type, string, cons, wild;

	GetTypeTable(&string, String);
	GetTypeTable(&cons, Cons);
	GetConst(KEYWORD_WILD, &wild);
	type_member_heap(&type, Nil, wild, NULL);
	type3or_heap(string, cons, type, &type);
	SetTypeTable(PathnameName, type);
}

static void typetable_pathnametype(void)
{
	/* type       (or string (member nil :wild :unspecific))) */
	addr type, string, wild, unspec;

	GetTypeTable(&string, String);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_UNSPECIFIC, &unspec);
	type_member_heap(&type, Nil, wild, unspec, NULL);
	type2or_heap(string, type, &type);
	SetTypeTable(PathnameType, type);
}

static void typetable_pathnameversion(void)
{
	/* version    (or (integer 1 *) (member nil :wild :unspecific :newest)) */
	addr type1, type2, newest, wild, unspec, pos;

	type2integer_ab_heap(Nil, 0, &type1);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_UNSPECIFIC, &unspec);
	GetConst(KEYWORD_NEWEST, &newest);
	type_member_heap(&type2, Nil, wild, unspec, newest, NULL);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(PathnameVersion, pos);
}

static void typetable_signed8(void)
{
	addr pos;
	type_signed_heap(8, &pos);
	SetTypeTable(Signed8, pos);
}

static void typetable_signed16(void)
{
	addr pos;
	type_signed_heap(16, &pos);
	SetTypeTable(Signed16, pos);
}

static void typetable_signed32(void)
{
	addr pos;
	type_signed_heap(32, &pos);
	SetTypeTable(Signed32, pos);
}

static void typetable_unsigned8(void)
{
	addr pos;
	type_unsigned_heap(8, &pos);
	SetTypeTable(Unsigned8, pos);
}

static void typetable_unsigned16(void)
{
	addr pos;
	type_unsigned_heap(16, &pos);
	SetTypeTable(Unsigned16, pos);
}

static void typetable_unsigned32(void)
{
	addr pos;
	type_unsigned_heap(32, &pos);
	SetTypeTable(Unsigned32, pos);
}

#ifdef LISP_64BIT
static void typetable_signed64(void)
{
	addr pos;
	type_signed_heap(64, &pos);
	SetTypeTable(Signed64, pos);
}

static void typetable_unsigned64(void)
{
	addr pos;
	type_unsigned_heap(64, &pos);
	SetTypeTable(Unsigned64, pos);
}
#endif

static void typetable_opendirection(void)
{
	addr type, input, output, io, probe;

	GetConst(KEYWORD_INPUT, &input);
	GetConst(KEYWORD_OUTPUT, &output);
	GetConst(KEYWORD_IO, &io);
	GetConst(KEYWORD_PROBE, &probe);
	type_member_heap(&type, input, output, io, probe, NULL);
	SetTypeTable(OpenDirection, type);
}

static void typetable_openelementtype(void)
{
	addr type, keyword;

	GetTypeTable(&type, TypeSpec);
	GetConst(KEYWORD_DEFAULT, &keyword);
	type_eql_heap(keyword, &keyword);
	type2or_heap(type, keyword, &type);
	SetTypeTable(OpenElementType, type);
}

static void typetable_openifexists(void)
{
	addr type, error, new_version, rename, rename_and_delete;
	addr overwrite, append, supersede;

	GetConst(KEYWORD_ERROR, &error);
	GetConst(KEYWORD_NEW_VERSION, &new_version);
	GetConst(KEYWORD_RENAME, &rename);
	GetConst(KEYWORD_RENAME_AND_DELETE, &rename_and_delete);
	GetConst(KEYWORD_OVERWRITE, &overwrite);
	GetConst(KEYWORD_APPEND, &append);
	GetConst(KEYWORD_SUPERSEDE, &supersede);
	type_member_heap(&type, error, new_version, rename, rename_and_delete,
			overwrite, append, supersede, Nil, NULL);
	SetTypeTable(OpenIfExists, type);
}

static void typetable_openifdoesnotexist(void)
{
	addr type, error, create;

	GetConst(KEYWORD_ERROR, &error);
	GetConst(KEYWORD_CREATE, &create);
	type_member_heap(&type, error, create, Nil, NULL);
	SetTypeTable(OpenIfDoesNotExist, type);
}

static void typetable_externalformat(void)
{
	addr type1, type2;

	GetTypeTable(&type1, Symbol);
	GetTypeTable(&type2, String);
	type2or_heap(type1, type2, &type1);
	SetTypeTable(ExternalFormat, type1);
}

static void typetable_pprint_newline(void)
{
	addr type, key1, key2, key3, key4;
	GetConst(KEYWORD_LINEAR, &key1);
	GetConst(KEYWORD_FILL, &key2);
	GetConst(KEYWORD_MISER, &key3);
	GetConst(KEYWORD_MANDATORY, &key4);
	type_member_heap(&type, key1, key2, key3, key4, NULL);
	SetTypeTable(PprintNewline, type);
}

static void typetable_pprint_tabular(void)
{
	addr type, key1, key2, key3, key4;
	GetConst(KEYWORD_LINE, &key1);
	GetConst(KEYWORD_SECTION, &key2);
	GetConst(KEYWORD_LINE_RELATIVE, &key3);
	GetConst(KEYWORD_SECTION_RELATIVE, &key4);
	type_member_heap(&type, key1, key2, key3, key4, NULL);
	SetTypeTable(PprintTabular, type);
}

static void typetable_format(void)
{
	addr type, null, eqlt, stream, string;

	/* (or null (eql t) stream string) */
	GetTypeTable(&null, Null);
	GetTypeTable(&eqlt, EqlT);
	GetTypeTable(&stream, Stream);
	GetTypeTable(&string, String);
	type4or_heap(null, eqlt, stream, string, &type);
	SetTypeTable(Format, type);
}

static void typetable_time_second(void)
{
	addr type;
	type4integer_heap(Nil, 0, Nil, 59, &type);
	SetTypeTable(TimeSecond, type);
}

static void typetable_time_hour(void)
{
	addr type;
	type4integer_heap(Nil, 0, Nil, 23, &type);
	SetTypeTable(TimeHour, type);
}

static void typetable_time_day(void)
{
	addr type;
	type4integer_heap(Nil, 1, Nil, 31, &type);
	SetTypeTable(TimeDay, type);
}

static void typetable_time_month(void)
{
	addr type;
	type4integer_heap(Nil, 1, Nil, 12, &type);
	SetTypeTable(TimeMonth, type);
}

static void typetable_time_zone(void)
{
	addr v1, v2, type;

	fixnum_heap(&v1, -24);
	fixnum_heap(&v2, 24);
	type4_heap(LISPDECL_RATIONAL, Nil, v1, Nil, v2, &type);
	SetTypeTable(TimeZone, type);
}

static void typetable_orlist(enum TypeTable type, enum TypeTable typelist)
{
	addr pos, list;

	gettypetable(type, &pos);
	GetTypeTable(&list, List);
	type2or_heap(pos, list, &pos);
	settypetable(typelist, pos);
}

static void typetable_symbol_list(void)
{
	typetable_orlist(TypeTable_Symbol, TypeTable_SymbolList);
}

static void typetable_string_list(void)
{
	typetable_orlist(TypeTable_String, TypeTable_StringList);
}

static void typetable_string_designer_list(void)
{
	typetable_orlist(TypeTable_StringDesigner, TypeTable_StringDesignerList);
}

static void typetable_package_designer_list(void)
{
	typetable_orlist(TypeTable_PackageDesigner, TypeTable_PackageDesignerList);
}

static void typetable_method(void)
{
	addr pos;

	/* method1 */
	GetConst(CLOS_METHOD, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(Method, pos);
	SetTypeTable(Method1, pos);

	/* method2 */
	GetTypeTable(&pos, List);
	SetTypeTable(Method2, pos);
}

static void typetable_class(void)
{
	addr pos;

	GetConst(CLOS_CLASS, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(Class, pos);
}

static void typetable_classnull(void)
{
	SetTypeTableNull(Class);
}

static void typetable_standardclass(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(StandardClass, pos);
}

static void typetable_standardobject(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_OBJECT, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(StandardObject, pos);
}

static void typetable_structureclass(void)
{
	addr pos;

	GetConst(CLOS_STRUCTURE_CLASS, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(StructureClass, pos);
}

static void typetable_structureobject(void)
{
	addr pos;

	GetConst(CLOS_STRUCTURE_OBJECT, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(StructureObject, pos);
}

static void typetable_standard_method(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_METHOD, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(StandardMethod, pos);
}

static void typetable_methodcombination(void)
{
	addr pos;

	GetConst(CLOS_METHOD_COMBINATION, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(MethodCombination, pos);
}


/*
 *  Array
 */
static void typetable_array_t(void)
{
	addr pos;
	GetTypeTable(&pos, T);
	SetTypeTable(Array_T, pos);
}

static void typetable_array_bit(void)
{
	addr pos;
	GetTypeTable(&pos, Bit);
	SetTypeTable(Array_Bit, pos);
}

static void typetable_array_character(void)
{
	addr pos;
	GetTypeTable(&pos, Character);
	SetTypeTable(Array_Character, pos);
}

static void typetable_array_singlefloat(void)
{
	addr pos;
	GetTypeTable(&pos, SingleFloat);
	SetTypeTable(Array_SingleFloat, pos);
}

static void typetable_array_doublefloat(void)
{
	addr pos;
	GetTypeTable(&pos, DoubleFloat);
	SetTypeTable(Array_DoubleFloat, pos);
}

static void typetable_array_longfloat(void)
{
	addr pos;
	GetTypeTable(&pos, LongFloat);
	SetTypeTable(Array_LongFloat, pos);
}

static void typetable_array_signed8(void)
{
	addr pos;
	GetTypeTable(&pos, Signed8);
	SetTypeTable(Array_Signed8, pos);
}

static void typetable_array_signed16(void)
{
	addr pos;
	GetTypeTable(&pos, Signed16);
	SetTypeTable(Array_Signed16, pos);
}

static void typetable_array_signed32(void)
{
	addr pos;
	GetTypeTable(&pos, Signed32);
	SetTypeTable(Array_Signed32, pos);
}

static void typetable_array_unsigned8(void)
{
	addr pos;
	GetTypeTable(&pos, Unsigned8);
	SetTypeTable(Array_Unsigned8, pos);
}

static void typetable_array_unsigned16(void)
{
	addr pos;
	GetTypeTable(&pos, Unsigned16);
	SetTypeTable(Array_Unsigned16, pos);
}

static void typetable_array_unsigned32(void)
{
	addr pos;
	GetTypeTable(&pos, Unsigned32);
	SetTypeTable(Array_Unsigned32, pos);
}

#ifdef LISP_64BIT
static void typetable_array_signed64(void)
{
	addr pos;
	GetTypeTable(&pos, Signed64);
	SetTypeTable(Array_Signed64, pos);
}

static void typetable_array_unsigned64(void)
{
	addr pos;
	GetTypeTable(&pos, Unsigned64);
	SetTypeTable(Array_Unsigned64, pos);
}
#endif


/*
 *  Arguments
 */
static void typeargs_empty_constant(void)
{
	addr pos;
	typeargs_empty(&pos);
	SetTypeArgs(Empty, pos);
}

static void typeargs_optconditionnull(void)
{
	addr pos;

	GetTypeTable(&pos, ConditionNull);
	typeargs_opt1(&pos, pos);
	SetTypeArgs(OptConditionNull, pos);
}

static void typeargs_packagedesigner(void)
{
	addr pos;
	GetTypeTable(&pos, PackageDesigner);
	typeargs_var1(&pos, pos);
	SetTypeArgs(PackageDesigner, pos);
}

static void typeargs_pathnamecase(void)
{
	addr args, key, symbol, common, keylocal;

	/* key */
	GetConst(KEYWORD_CASE, &symbol);
	GetConst(KEYWORD_COMMON, &common);
	GetConst(KEYWORD_LOCAL, &keylocal);
	type_member_heap(&key, common, keylocal, NULL);
	cons_heap(&key, symbol, key);
	conscar_heap(&key, key);
	/* type */
	GetTypeTable(&args, PathnameDesigner);
	typeargs_var1key(&args, args, key);
	SetTypeArgs(PathnameCase, args);
}

static void typeargs_error(void)
{
	addr args, type;

	GetTypeTable(&args, ConditionDesigner);
	GetTypeTable(&type, T);
	typeargs_var1rest(&args, args, type);
	SetTypeArgs(Error, args);
}


/*
 *  Values
 */
static void deftypevalues(enum TypeTable a, enum TypeTable b)
{
	addr pos;

	gettypetable(a, &pos);
	typevalues_result(&pos, pos);
	settypetable(b, pos);
}
#define TypeValues(a) deftypevalues(TypeTable_##a, TypeValues_##a)
#define DefTypeValues(a) static void typevalues_##a(void) { TypeValues(a); }

static void typevalues_Nil(void)
{
	addr pos;
	GetTypeTable(&pos, Nil);
	typevalues_rest(&pos, pos);
	SetTypeValues(Nil, pos);
}

static void typevalues_T(void)
{
	addr pos;
	GetTypeTable(&pos, T);
	typevalues_result(&pos, pos);
	SetTypeValues(T, pos);
}

DefTypeValues(Null);
DefTypeValues(Cons);
DefTypeValues(List);
DefTypeValues(Boolean);
DefTypeValues(Character);
DefTypeValues(CharacterNull);
DefTypeValues(Symbol);
DefTypeValues(Vector);
DefTypeValues(SimpleVector);
DefTypeValues(String);
DefTypeValues(StringNull);
DefTypeValues(SimpleString);
DefTypeValues(Stream);
DefTypeValues(StreamNull);
DefTypeValues(PrettyStream);
DefTypeValues(MemoryStream);
DefTypeValues(Function);
DefTypeValues(FunctionNull);
DefTypeValues(EqlT);
DefTypeValues(Package);
DefTypeValues(PackageNull);
DefTypeValues(Sequence);
DefTypeValues(Array);
DefTypeValues(Integer);
DefTypeValues(Ratio);
DefTypeValues(Rational);
DefTypeValues(Index);
DefTypeValues(IndexNull);
DefTypeValues(Intplus);
DefTypeValues(IntplusNull);
DefTypeValues(Bit);
DefTypeValues(BitArray);
DefTypeValues(Pathname);
DefTypeValues(PathnameNull);
DefTypeValues(LogicalPathname);
DefTypeValues(Float);
DefTypeValues(Real);
DefTypeValues(Number);
DefTypeValues(Complex);
DefTypeValues(TypeSymbol);
DefTypeValues(Class);
DefTypeValues(ClassNull);


/*
 *  Values
 */
static void typevalues_decode_universal_time(void)
{
	addr sec, hour, day, month, year, week, daylight, zone;
	addr values;

	GetTypeTable(&sec, TimeSecond);
	GetTypeTable(&hour, TimeHour);
	GetTypeTable(&day, TimeDay);
	GetTypeTable(&month, TimeMonth);
	GetTypeTable(&year, Intplus);
	type4integer_heap(Nil, 0, Nil, 6, &week);
	GetTypeTable(&daylight, Boolean);
	GetTypeTable(&zone, TimeZone);
	typevalues_values_va(&values,
			sec, sec, hour, day, month, year, week, daylight, zone, NULL);
	SetTypeValues(DecodeUniversalTime, values);
}

static void typevalues_empty(void)
{
	addr values;
	GetTypeTable(&values, Nil);
	typevalues_rest(&values, values);
	SetTypeValues(Empty, values);
}


/*
 *  Compiled-Function
 */
static void typecompiled_object_boolean(void)
{
	addr args, values;

	GetTypeTable(&args, Asterisk);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Object_Boolean, args);
}

static void typecompiled_symbol_boolean(void)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Symbol_Boolean, args);
}

static void typecompiled_stringcase(void)
{
	/* (function (string-designer &key (start keyword-start)
	 *                                 (end keyword-end))
	 *           (values string &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, StringDesigner);
	GetTypeTable(&values, KeyStart1End1);
	typeargs_var1key(&args, args, values);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(StringCase, args);
}

static void typecompiled_nstringcase(void)
{
	/* (function (string &key (start keyword-start)
	 *                        (end keyword-end))
	 *           (values string &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, String);
	GetTypeTable(&values, KeyStart1End1);
	typeargs_var1key(&args, args, values);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(NStringCase, args);
}

static void typecompiled_stringtrim(void)
{
	/* (function (sequence string-designer) (values string &rest nil)) */
	addr args, values, type;

	GetTypeTable(&args, Sequence);
	GetTypeTable(&type, StringDesigner);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(StringTrim, args);
}

static void typecompiled_stringequal(void)
{
	/* (function (string-designer string-designer &key
	 *             (start1 keyword-start)
	 *             (end1   keyword-end)
	 *             (start2 keyword-start)
	 *             (end2   keyword-end))
	 *           (values boolean &rest null))
	 */
	addr args, values;

	GetTypeTable(&args, StringDesigner);
	GetTypeTable(&values, KeyStart2End2);
	typeargs_var2key(&args, args, args, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(StringEqual, args);
}

static void typecompiled_stringmismatch(void)
{
	/* (function (string-designer string-designer &key
	 *             (start1 keyword-start)
	 *             (end1   keyword-end)
	 *             (start2 keyword-start)
	 *             (end2   keyword-end))
	 *           (values keyword-end &rest null))
	 */
	addr args, values;

	GetTypeTable(&args, StringDesigner);
	GetTypeTable(&values, KeyStart2End2);
	typeargs_var2key(&args, args, args, values);
	GetTypeValues(&values, IndexNull);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(StringMismatch, args);
}

static void typecompiled_rplaca(void)
{
	/* (function cons (values cons &rest nil)) */
	addr args, values, type;

	GetTypeTable(&args, Cons);
	GetTypeTable(&type, Asterisk);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, Cons);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Rplaca, args);
}

static void typecompiled_list_list(void)
{
	/* (function (list) (values list &rest nil)) */
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(List_List, args);
}

static void typecompiled_nth(void)
{
	/* (function (integer-plus list) (values t &rest nil)) */
	addr args, values, type;

	GetTypeTable(&args, Intplus);
	GetTypeTable(&type, List);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Nth, args);
}

static void typecompiled_nconc(void)
{
	/* (function (&rest t) (values t &rest nil)) */
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Nconc, args);
}

static void typecompiled_renconc(void)
{
	/* (function (list t) (values t &rest nil)) */
	addr args, values, type;

	GetTypeTable(&args, List);
	GetTypeTable(&type, T);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Nreconc, args);
}

static void typecompiled_butlast(void)
{
	/* (function (list &optional intplus) (values list &rest nil)) */
	addr args, values, type;

	GetTypeTable(&args, List);
	GetTypeTable(&type, Intplus);
	typeargs_var1opt1(&args, args, type);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(ButLast, args);
}

static void typecompiled_macrofunction(void)
{
	/* (function (t (or null environment)) t) */
	addr args, values, env;

	GetTypeTable(&args, T);
	GetTypeTable(&env, EnvironmentNull);
	typeargs_var2(&args, args, env);
	GetTypeTable(&values, Asterisk);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(MacroFunction, args);
}

static void typecompiled_macroexpand(void)
{
	/* (function (t &optional (or null environment))
	 *           (values t boolean &rest nil))
	 */
	addr args, values, pos1, pos2;

	GetTypeTable(&pos1, T);
	GetTypeTable(&pos2, EnvironmentNull);
	typeargs_var1opt1(&args, pos1, pos2);
	GetTypeTable(&pos2, Boolean);
	typevalues_values2(&values, pos1, pos2);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(MacroExpand, args);
}

static void typecompiled_abort(void)
{
	addr args, values;

	/* (function (&optional (or condition null)) nil) */
	GetTypeArgs(&args, OptConditionNull);
	GetTypeTable(&values, Nil);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Abort, args);
}

static void typecompiled_continue(void)
{
	addr args, values;

	/* (function (&optional (or condition null)) null) */
	GetTypeArgs(&args, OptConditionNull);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Continue, args);
}

static void typecompiled_storevalue(void)
{
	addr args, values;

	/* (function (t &optional (or condition null)) null) */
	GetTypeTable(&args, T);
	GetTypeTable(&values, ConditionNull);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(StoreValue, args);
}

static void typecompiled_macroreader(void)
{
	/* (function (stream character) *) */
	addr args, values, type;

	GetTypeTable(&args, Stream);
	GetTypeTable(&type, Character);
	typeargs_var2(&args, args, type);
	GetTypeTable(&values, Asterisk);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(MacroReader, args);
}

static void typecompiled_macrodispatch(void)
{
	/* (function (stream character intplus-null) *) */
	addr args, values, type, intplus;

	GetTypeTable(&args, Stream);
	GetTypeTable(&type, Character);
	GetTypeTable(&intplus, IntplusNull);
	typeargs_var3(&args, args, type, intplus);
	GetTypeTable(&values, Asterisk);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(MacroDispatch, args);
}

static void typecompiled_read(void)
{
	addr args, values, type;

	/* (function (&optional input-stream-designer t t t) (values t &rest nil)) */
	GetTypeTable(&args, StreamDesigner);
	GetTypeTable(&type, T);
	list_heap(&args, args, type, type, type, NULL);
	typeargs_full(&args, Nil, args, Nil, Nil);
	typevalues_result(&values, type);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Read, args);
}

static void typecompiled_sublis(void)
{
	/* (function
	 *   (list list
	 *    &key (:key      (or (function (t &rest t) *)   symbol))
	 *         (:test     (or (function (t t &rest t) *) symbol))
	 *         (:test-not (or (function (t t &rest t) *) symbol)))
	 *   (values list &rest nil))
	 */
	addr args, values, list, tree;

	GetTypeTable(&list, List);
	GetTypeTable(&tree, T);
	GetTypeTable(&args, KeyTestList);
	typeargs_var2key(&args, list, tree, args);
	typevalues_result(&values, list);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Sublis, args);
}

static void typecompiled_subst(void)
{
	/* (function
	 *   (t t t
	 *    &key (:key      (or (function (t &rest t) *)   symbol))
	 *         (:test     (or (function (t t &rest t) *) symbol))
	 *         (:test-not (or (function (t t &rest t) *) symbol)))
	 *   (values t &rest nil))
	 */
	addr args, values, type;

	GetTypeTable(&type, T);
	GetTypeTable(&args, KeyTestList);
	typeargs_var3key(&args, type, type, type, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Subst, args);
}

static void typecompiled_subst_if(void)
{
	/* (function
	 *   (t (or (function (t &rest t) *) symbol) t
	 *    &key (:key (or (function (t &rest t) *) symbol)))
	 *   (values t &rest nil))
	 */
	addr args, values, type, call, key;

	GetTypeTable(&type, T);
	GetTypeTable(&call, FunctionDesigner);
	GetConst(KEYWORD_KEY, &key);
	cons_heap(&key, key, call);
	conscar_heap(&key, key);
	typeargs_var3key(&args, type, call, type, key);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(SubstIf, args);
}

static void typecompiled_eq(void)
{
	/* (function (t t) boolean) */
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Eq, args);
}


static void typecompiled_every(void)
{
	/* (function (function-designer sequence &rest sequence)
	 *   (values boolean &rest nil))
	 */
	addr args, values, call, sequence;

	GetTypeTable(&call, FunctionDesigner);
	GetTypeTable(&sequence, Sequence);
	typeargs_var2rest(&args, call, sequence, sequence);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Every, args);
}

static void typecompiled_number_equal(void)
{
	/* (function (number &rest number) (values boolean &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Number);
	typeargs_var1rest(&args, args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Number_Equal, args);
}

static void typecompiled_number_compare(void)
{
	/* (function (real &rest real) (values boolean &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Real);
	typeargs_var1rest(&args, args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Number_Compare, args);
}

static void typecompiled_max(void)
{
	/* (function (real &rest real) (values real &rest nil)) */
	addr args, values;

	GetTypeTable(&values, Real);
	typeargs_var1rest(&args, values, values);
	GetTypeValues(&values, Real);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Max, args);
}

static void typecompiled_minusp(void)
{
	/* (function (real) (values boolean &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Real);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Minusp, args);
}

static void typecompiled_zerop(void)
{
	/* (function (number) (values boolean &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Number);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Zerop, args);
}

static void typecompiled_plus(void)
{
	/* (function (&rest number) (values number &rest nil)) */
	addr args, values;

	GetTypeTable(&values, Number);
	typeargs_rest(&args, values);
	GetTypeValues(&values, Number);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Plus, args);
}

static void typecompiled_minus(void)
{
	/* (function (number &rest number) (values number &rest nil)) */
	addr args, values;

	GetTypeTable(&values, Number);
	typeargs_var1rest(&args, values, values);
	GetTypeValues(&values, Number);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Minus, args);
}

static void typecompiled_oneplus(void)
{
	/* (function (number) (values number &rest nil)) */
	addr args, values;

	GetTypeTable(&values, Number);
	typeargs_var1(&args, values);
	GetTypeValues(&values, Number);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(OnePlus, args);
}

static void typecompiled_hashtablecount(void)
{
	/* (function (hash-table) (values Index &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Hashtable);
	typeargs_var1(&args, args);
	GetTypeTable(&values, Index);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(HashTableCount, args);
}

static void typecompiled_evenp(void)
{
	/* (function (integer) (values boolean &res nil) */
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Evenp, args);
}

static void typecompiled_export(void)
{
	/* (function
	 *   ((or list symbol) &optional package-designer)
	 *   (values (eql t) &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, SymbolList);
	GetTypeTable(&values, PackageDesigner);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Export, args);
}

static void typecompiled_usepackage(void)
{
	/*  (function
	 *    ((or list package-designer) &optional package-designer)
	 *    (values (eql t) &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, List);
	GetTypeTable(&values, PackageDesigner);
	type2or_heap(args, values, &args);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(UsePackage, args);
}

static void typecompiled_intern(void)
{
	/*  (function
	 *    (string &optional package-designer)
	 *    (values symbol (member :internal :external :inherited nil) &rest nil))
	 */
	addr args, values, symbol, key1, key2, key3, status;

	/* args */
	GetTypeTable(&args, String);
	GetTypeTable(&values, PackageDesigner);
	typeargs_var1opt1(&args, args, values);
	/* values */
	GetTypeTable(&symbol, Symbol);
	GetConst(KEYWORD_INTERNAL, &key1);
	GetConst(KEYWORD_EXTERNAL, &key2);
	GetConst(KEYWORD_INHERITED, &key3);
	type_member_heap(&status, key1, key2, key3, Nil, NULL);
	typevalues_values2(&values, symbol, status);
	/* result */
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Intern, args);
}

static void typecompiled_packagenicknames(void)
{
	/*  (function
	 *    (package-designer)
	 *    (values list &rest nil))
	 */
	addr args, values;

	GetTypeArgs(&args, PackageDesigner);
	GetTypeTable(&values, List);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(PackageNicknames, args);
}

static void typecompiled_prin1(void)
{
	/*  (function
	 *    (t &optional stream-designer)
	 *    (values t &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, StreamDesigner);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Prin1, args);
}


static void typecompiled_prin1tostring(void)
{
	/* (function (t) (values string &rest nil)) */
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Prin1ToString, args);
}

static void typecompiled_reverse(void)
{
	/* (function (sequence) (values sequence &rest nil)) */
	addr args, values;

	GetTypeTable(&values, Sequence);
	typeargs_var1(&args, values);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Reverse, args);
}

static void typecompiled_member(void)
{
	/* (function (t list &key [KeyTestList]) (values list &rest nil)) */
	addr args, values, key;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	GetTypeTable(&key, KeyTestList);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Member, args);
}

static void typecompiled_memberif(void)
{
	/* (function (function-designer list &key (key function-designer))
	 *           (values list &rest nil))
	 */
	addr args, values, type, key;

	GetTypeTable(&type, FunctionDesigner);
	GetTypeTable(&args, List);
	GetConst(KEYWORD_KEY, &key);
	cons_heap(&key, key, type);
	conscar_heap(&key, key);
	typeargs_var2key(&args, type, args, key);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(MemberIf, args);
}

static void typecompiled_mapc(void)
{
	/* (function (function-designer list &rest list)
	 *           (values list &rest nil))
	 */
	addr args, values, type;

	GetTypeTable(&type, FunctionDesigner);
	GetTypeTable(&args, List);
	typeargs_var2rest(&args, type, args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Mapc, args);
}

static void typecompiled_acons(void)
{
	/* (function (t t list) (values list &rest nil)) */
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&type, List);
	typeargs_var3(&args, args, args, type);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Acons, args);
}

static void typecompiled_intersection(void)
{
	/* (function (list list &key key test test-not) (values list &rest nil)) */
	addr args, values;

	GetTypeTable(&args, List);
	GetTypeTable(&values, KeyTestList);
	typeargs_var2key(&args, args, args, values);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Intersection, args);
}


static void typecompiled_ecaseerror(void)
{
	/* (function (T list) (values &rest nil)) */
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Nil);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(EcaseError, args);
}

static void typecompiled_dosymbols(void)
{
	/* (function (function package-designer) (values &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Function);
	GetTypeTable(&values, PackageDesigner);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Nil);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(DoSymbols, args);
}

static void typecompiled_arrayboolean(void)
{
	/* (function (array) (values boolean &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Array);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(ArrayBoolean, args);
}

static void typecompiled_arrayindex(void)
{
	/* (function (array) (values index &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Array);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Index);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(ArrayIndex, args);
}

static void typecompiled_bitand(void)
{
	/* (function (bit-array bit-array
	 *     &optional (or boolean bit-array))
	 *   (values bit-array &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, BitArray);
	GetTypeTable(&values, Boolean);
	type2or_heap(args, values, &values);
	typeargs_var2opt1(&args, args, args, values);
	GetTypeValues(&values, BitArray);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(BitAnd, args);
}

static void typecompiled_countif(void)
{
	/* (function (function-designer sequence
	 *     &key (:from-end t)
	 *          (:start (integer 0 *))
	 *          (:end (or (integer 0 *) null))
	 *          (:key function-designer))
	 *     (values index &rest nil))
	 */
	addr args, values, key;

	GetTypeTable(&args, FunctionDesigner);
	GetTypeTable(&values, Sequence);
	GetTypeTable(&key, CountIfKey);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, Index);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(CountIf, args);
}

static void typecompiled_sort(void)
{
	/* (function (sequence call &key key) (values sequence &rest nil)) */
	addr args, values, type, key;

	/* key */
	GetConst(KEYWORD_KEY, &key);
	GetTypeTable(&type, FunctionDesigner);
	cons_heap(&key, key, type);
	conscar_heap(&key, key);
	/* type */
	GetTypeTable(&args, Sequence);
	typeargs_var2key(&args, args, type, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Sort, args);
}

static void typecompiled_findif(void)
{
	/* (function (function-designer sequence
	 *     &key (:from-end t)
	 *          (:start (integer 0 *))
	 *          (:end (or (integer 0 *) null))
	 *          (:key function-designer))
	 *     (values t &rest nil))
	 */
	addr args, values, key;

	GetTypeTable(&args, FunctionDesigner);
	GetTypeTable(&values, Sequence);
	GetTypeTable(&key, CountIfKey);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(FindIf, args);
}

static void typecompiled_positionif(void)
{
	/* (function (function-designer sequence
	 *     &key (:from-end t)
	 *          (:start (integer 0 *))
	 *          (:end (or (integer 0 *) null))
	 *          (:key function-designer))
	 *     (values index-null &rest nil))
	 */
	addr args, values, key;

	GetTypeTable(&args, FunctionDesigner);
	GetTypeTable(&values, Sequence);
	GetTypeTable(&key, CountIfKey);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, IndexNull);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(PositionIf, args);
}

static void typecompiled_search(void)
{
	/* (function (sequence1 sequence2
	 *     &key from-end test test-not key start1 start2 end1 end2)
	 *     (values index-null &rest nil))
	 */
	addr args, values, key;
	addr key1, key2, key3, key4, key5, key6, key7, key8;

	/* key */
	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, TEST, FunctionDesigner);
	KeyTypeTable(&key3, TEST_NOT, FunctionDesigner);
	KeyTypeTable(&key4, KEY, FunctionDesigner);
	KeyTypeTable(&key5, START1, KeywordStart);
	KeyTypeTable(&key6, START2, KeywordStart);
	KeyTypeTable(&key7, END1, KeywordEnd);
	KeyTypeTable(&key8, END2, KeywordEnd);
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, key8, NULL);

	/* type */
	GetTypeTable(&args, Sequence);
	typeargs_var2key(&args, args, args, key);
	GetTypeValues(&values, IndexNull);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Search, args);
}

static void typecompiled_substitute(void)
{
	/* (function (t t sequence
	 *     &key from-end test test-not key start end count)
	 *     (values sequence &rest nil))
	 */
	addr args, values, key;
	addr key1, key2, key3, key4, key5, key6, key7;

	/* key */
	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, TEST, FunctionDesigner);
	KeyTypeTable(&key3, TEST_NOT, FunctionDesigner);
	KeyTypeTable(&key4, KEY, FunctionDesigner);
	KeyTypeTable(&key5, START, KeywordStart);
	KeyTypeTable(&key6, END, KeywordEnd);
	KeyTypeTable(&key7, COUNT, IntegerNull);
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, NULL);

	/* type */
	GetTypeTable(&args, T);
	GetTypeTable(&values, Sequence);
	typeargs_var3key(&args, args, args, values, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Substitute, args);
}

static void typecompiled_substituteif(void)
{
	/* (function (t call sequence
	 *     &key from-end key start end count)
	 *     (values sequence &rest nil))
	 */
	addr args, values, type, key;
	addr key1, key2, key3, key4, key5;

	/* key */
	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, KEY, FunctionDesigner);
	KeyTypeTable(&key3, START, KeywordStart);
	KeyTypeTable(&key4, END, KeywordEnd);
	KeyTypeTable(&key5, COUNT, IntegerNull);
	list_heap(&key, key1, key2, key3, key4, key5, NULL);

	/* type */
	GetTypeTable(&args, T);
	GetTypeTable(&values, FunctionDesigner);
	GetTypeTable(&type, Sequence);
	typeargs_var3key(&args, args, values, type, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(SubstituteIf, args);
}

static void typecompiled_remove(void)
{
	/* (function (t sequence
	 *     &key from-end test test-not key start end count)
	 *     (values sequence &rest nil))
	 */
	addr args, values, key;
	addr key1, key2, key3, key4, key5, key6, key7;

	/* key */
	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, TEST, FunctionDesigner);
	KeyTypeTable(&key3, TEST_NOT, FunctionDesigner);
	KeyTypeTable(&key4, KEY, FunctionDesigner);
	KeyTypeTable(&key5, START, KeywordStart);
	KeyTypeTable(&key6, END, KeywordEnd);
	KeyTypeTable(&key7, COUNT, IntegerNull);
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, NULL);

	/* type */
	GetTypeTable(&args, T);
	GetTypeTable(&values, Sequence);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Remove, args);
}

static void typecompiled_removeif(void)
{
	/* (function (call sequence
	 *     &key from-end key start end count)
	 *     (values sequence &rest nil))
	 */
	addr args, values, key;
	addr key1, key2, key3, key4, key5;

	/* key */
	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, KEY, FunctionDesigner);
	KeyTypeTable(&key3, START, KeywordStart);
	KeyTypeTable(&key4, END, KeywordEnd);
	KeyTypeTable(&key5, COUNT, IntegerNull);
	list_heap(&key, key1, key2, key3, key4, key5, NULL);

	/* type */
	GetTypeTable(&args, FunctionDesigner);
	GetTypeTable(&values, Sequence);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(RemoveIf, args);
}

static void typecompiled_removeduplicates(void)
{
	/* (function (sequence
	 *     &key from-end test test-not start end key)
	 *     (values sequence &rest nil))
	 */
	addr args, values, key;
	addr key1, key2, key3, key4, key5, key6;

	/* key */
	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, TEST, FunctionDesigner);
	KeyTypeTable(&key3, TEST_NOT, FunctionDesigner);
	KeyTypeTable(&key4, KEY, FunctionDesigner);
	KeyTypeTable(&key5, START, KeywordStart);
	KeyTypeTable(&key6, END, KeywordEnd);
	list_heap(&key, key1, key2, key3, key4, key5, key6, NULL);

	/* type */
	GetTypeTable(&args, Sequence);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(RemoveDuplicates, args);
}

static void typecompiled_namestring(void)
{
	/* (function (pathname-designer) (values string &rest nil)) */
	addr args, values;

	GetTypeTable(&args, PathnameDesigner);
	typeargs_var1(&args, args);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Namestring, args);
}

static void typecompiled_pathname(void)
{
	/* (function (pathname-designer) (values pathname &rest nil)) */
	addr args, values;

	GetTypeTable(&args, PathnameDesigner);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Pathname);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Pathname, args);
}

static void typecompiled_inputstreamp(void)
{
	/* (function (stream) (values boolean &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Stream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(InputStreamP, args);
}

static void typecompiled_exit(void)
{
	/* (function (&optional Intplus) (values &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Intplus);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Nil);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Exit, args);
}

static void typecompiled_readchar(void)
{
	/* (function (&optional stream-designer t t t) (values t &rest nil)) */
	addr args, values;

	GetTypeTable(&args, StreamDesigner);
	GetTypeTable(&values, T);
	typeargs_opt4(&args, args, values, values, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(ReadChar, args);
}

static void typecompiled_writestring(void)
{
	/* (function (string &optional stream-designer &key start end)
	 *           (values string &rest nil))
	 */
	addr args, values, var, opt, key, key1, key2;

	/* var */
	GetTypeTable(&var, String);
	conscar_heap(&var, var);
	/* opt */
	GetTypeTable(&opt, StreamDesigner);
	conscar_heap(&opt, opt);
	/* key */
	KeyTypeTable(&key1, START, KeywordStart);
	KeyTypeTable(&key2, END, KeywordEnd);
	list_heap(&key, key1, key2, NULL);
	/* args */
	typeargs_full(&args, var, opt, Nil, key);
	/* values */
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(WriteString, args);
}

static void typecompiled_finishoutput(void)
{
	/* (function (&optional output-stream) (values null &rest nil)) */
	addr args, values;

	GetTypeTable(&args, OutputStream);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(FinishOutput, args);
}

static void typecompiled_yesornop(void)
{
	/* (function (&optional (or string null) &rest t)
	 *           (values boolean &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, StringNull);
	conscar_heap(&args, args);
	GetTypeTable(&values, T);
	typeargs_full(&args, Nil, args, values, Nil);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(YesOrNoP, args);
}

static void typecompiled_floor(void)
{
	/* (function (real &optional real) (values integer real &rest nil) */
	addr args, values, type;

	GetTypeTable(&type, Real);
	typeargs_var1opt1(&args, type, type);
	GetTypeTable(&values, Integer);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Floor, args);
}

static void typecompiled_ffloor(void)
{
	/* (function (real &optional real) (values real real &rest nil) */
	addr args, values, type;

	GetTypeTable(&type, Real);
	typeargs_var1opt1(&args, type, type);
	typevalues_values2(&values, type, type);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Ffloor, args);
}

static void typecompiled_envinfo(void)
{
	/* (function () (values (or string null) &rest nil)) */
	addr args, values;

	typeargs_empty(&args);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(EnvInfo, args);
}

static void typecompiled_sin(void)
{
	/* (function (number) (values number &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Number);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Number);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Sin, args);
}

static void typecompiled_realpart(void)
{
	/* (function (number) (values real &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Number);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Real);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(RealPart, args);
}

static void typecompiled_gcd(void)
{
	/* (function (&rest integer) (values (integer 0 *) &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_rest(&args, args);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Gcd, args);
}

static void typecompiled_mod(void)
{
	/* (function (real real) (values real &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Real);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Real);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Mod, args);
}

static void typecompiled_float_digits(void)
{
	/* (function (float) (values (integer 0 *) &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Float);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(FloatDigits, args);
}

static void typecompiled_rational(void)
{
	/* (function (real) (values rational &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Real);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Rational);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Rational, args);
}

static void typecompiled_logand(void)
{
	/* (function (&rest integer) (values integer &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_rest(&args, args);
	GetTypeValues(&values, Integer);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Logand, args);
}

static void typecompiled_logandc1(void)
{
	/* (function (integer integer) (values integer &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Integer);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Logandc1, args);
}

static void typecompiled_byte_size(void)
{
	/* (function (byte) (values (integer 0 *) &rest nil)) */
	addr args, values;

	GetTypeTable(&args, ByteSpec);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(ByteSize, args);
}

static void typecompiled_deposit_field(void)
{
	/* (function (integer byte integer) (values integer &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Integer);
	GetTypeTable(&values, ByteSpec);
	typeargs_var3(&args, args, values, args);
	GetTypeValues(&values, Integer);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(DepositField, args);
}

static void typecompiled_ldb(void)
{
	/* (function (bytespec integer) (values (integer 0 *) &rest nil)) */
	addr args, values;

	GetTypeTable(&args, ByteSpec);
	GetTypeTable(&values, Integer);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Ldb, args);
}

static void typecompiled_upgraded_type(void)
{
	/* (function (typespec &optional environment) (values type &rest nil)) */
	addr args, values;

	GetTypeTable(&args, TypeSpec);
	GetTypeTable(&values, EnvironmentNull);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, TypeSymbol);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(UpgradedType, args);
}

static void typecompiled_slot_boundp(void)
{
	/* (function (clos symbol) (values t &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Clos);
	GetTypeTable(&values, Symbol);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(SlotBoundp, args);
}

static void typecompiled_slot_boundp_method(void)
{
	/* (function (t t t t symbol) (values t &rest nil)) */
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Symbol);
	typeargs_var3(&args, args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(SlotBoundp_Method, args);
}

static void typecompiled_reader_method(void)
{
	/* (function (t t t) (values t &rest nil)) */
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Reader_Method, args);
}

static void typecompiled_writer_method(void)
{
	/* (function (t t t t) (values t &rest nil)) */
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var2(&args, args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Writer_Method, args);
}

static void typecompiled_signal(void)
{
	addr args, values;

	GetTypeArgs(&args, Error);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Signal, args);
}

static void typecompiled_print_object_method(void)
{
	/* (function (t t t stream) (values t &rest nil)) */
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Stream);
	typeargs_var2(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(PrintObject_Method, args);
}

static void typecompiled_pprint_fill(void)
{
	/* (function (output-stream-designer t &optional t t)
	 *           (values null &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, StreamDesigner);
	GetTypeTable(&values, T);
	typeargs_var2opt2(&args, args, values, values, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(PprintFill, args);
}

static void typecompiled_dispatch_function(void)
{
	/* (function (output-stream-designer t) (values T &rest nil)) */
	addr args, values;

	GetTypeTable(&args, StreamDesigner);
	GetTypeTable(&values, T);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(DispatchFunction, args);
}

static void typecompiled_formatter_function(void)
{
	addr args, values;

	GetTypeTable(&args, Stream);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(FormatterFunction, args);
}

static void typecompiled_get_internal_real_time(void)
{
	addr args, values;

	GetTypeArgs(&args, Empty);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(GetInternalRealTime, args);
}

static void typecompiled_remove_file(void)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesigner);
	GetTypeTable(&values, T);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(RemoveFile, args);
}

static void typecompiled_infobit(void)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(InfoBit, args);
}

static void typecompiled_chareql(void)
{
	addr args, values;

	GetTypeTable(&args, Character);
	typeargs_rest(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(CharEql, args);
}

static void typecompiled_make_memory_output_stream(void)
{
	addr args, values, key1, key2, key3, key4, key;

	/* key */
	KeyTypeTable(&key1, INPUT, Sequence);
	KeyTypeTable(&key2, SIZE, Plus1Null);
	KeyTypeTable(&key3, ARRAY, Plus1Null);
	KeyTypeTable(&key4, CACHE, T);
	list_heap(&key, key1, key2, key3, key4, NULL);
	/* type */
	typeargs_key(&args, key);
	GetTypeValues(&values, MemoryStream);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(MakeMemoryOutputStream, args);
}


/*
 *  Interface
 */
void build_type_constant(void)
{
	/* Atomic-Type */
	typetable_Invalid();
	typetable_Asterisk();
	typetable_Atom();
	typetable_List();
	typetable_Boolean();
	typetable_Clos();
	typetable_Vector();
	typetable_SimpleVector();
	typetable_BitVector();
	typetable_ExtendedChar();
	typetable_SimpleBitVector();
	typetable_String();
	typetable_BaseString();
	typetable_SimpleString();
	typetable_SimpleBaseString();
	typetable_SignedByte();
	typetable_UnsignedByte();
	typetable_Bit();
	typetable_Fixnum();
	typetable_Bignum();
	typetable_Nil();
	typetable_T();
	typetable_Null();
	typetable_Cons();
	typetable_Hashtable();
	typetable_Symbol();
	typetable_Keyword();
	typetable_Package();
	typetable_RandomState();
	typetable_Readtable();
	typetable_Function();
	typetable_CompiledFunction();
	typetable_Pathname();
	typetable_LogicalPathname();
	typetable_Sequence();
	typetable_Array();
	typetable_SimpleArray();
	typetable_Character();
	typetable_BaseChar();
	typetable_StandardChar();
	typetable_Number();
	typetable_Real();
	typetable_Rational();
	typetable_Ratio();
	typetable_Integer();
	typetable_Complex();
	typetable_Float();
	typetable_ShortFloat();
	typetable_SingleFloat();
	typetable_DoubleFloat();
	typetable_LongFloat();
	typetable_Restart();
	typetable_Environment();
	typetable_Stream();
	typetable_BroadcastStream();
	typetable_ConcatenatedStream();
	typetable_EchoStream();
	typetable_FileStream();
	typetable_StringStream();
	typetable_SynonymStream();
	typetable_TwoWayStream();
	typetable_PromptStream();
	typetable_PrettyStream();
	typetable_MemoryStream();
	typetable_Quote();
	typetable_ByteSpec();
	typetable_PrintDispatch();
	typetable_Eval();

	/* Condition */
	typetable_ArithmeticError();
	typetable_CellError();
	typetable_FileError();
	typetable_PackageError();
	typetable_PrintNotReadable();
	typetable_SimpleCondition();
	typetable_StreamError();
	typetable_TypeError();

	/* Type */
	typetable_cxr();
	typetable_car();
	typetable_setf_car();
	typetable_characternull();
	typetable_stringnull();
	typetable_streamnull();
	typetable_condition();
	typetable_conditionnull();
	typetable_restartnull();
	typetable_functionnull();
	typetable_environmentnull();
	typetable_integernull();
	typetable_pathnamenull();
	typetable_packagenull();
	typetable_printdispatchnull();
	typetable_stringdesigner();
	typetable_packagedesigner();
	typetable_packagedesignernull();
	typetable_functiondesigner();
	typetable_restartdesigner();
	typetable_pathnamedesigner();
	typetable_streamdesigner();
	typetable_readtabledesigner();
	typetable_conditiondesigner();
	typetable_index();
	typetable_indexnull();
	typetable_plus1();
	typetable_plus1null();
	typetable_intplus();
	typetable_intplusnull();
	typetable_input_stream();
	typetable_output_stream();
	typetable_typespec();
	typetable_typesymbol();
	typetable_bit_array();
	typetable_simple_bit_array();
	typetable_keywordstart();
	typetable_keywordend();
	typetable_keystart1end1();
	typetable_keystart2end2();
	typetable_functionname();
	typetable_radixinteger();
	typetable_floatsymbol();
	typetable_eqlt();
	typetable_case_sensitivity();
	typetable_print_case();
	typetable_keytestlist();
	typetable_rehashsize();
	typetable_rehashthreshold();
	typetable_countkey();
	typetable_countifkey();
	typetable_pathnamehost();
	typetable_pathnamedevice();
	typetable_pathnamedirectory();
	typetable_pathnamename();
	typetable_pathnametype();
	typetable_pathnameversion();
	typetable_signed8();
	typetable_signed16();
	typetable_signed32();
	typetable_unsigned8();
	typetable_unsigned16();
	typetable_unsigned32();
#ifdef LISP_64BIT
	typetable_signed64();
	typetable_unsigned64();
#endif
	typetable_opendirection();
	typetable_openelementtype();
	typetable_openifexists();
	typetable_openifdoesnotexist();
	typetable_externalformat();
	typetable_pprint_newline();
	typetable_pprint_tabular();
	typetable_format();
	typetable_time_second();
	typetable_time_hour();
	typetable_time_day();
	typetable_time_month();
	typetable_time_zone();
	typetable_symbol_list();
	typetable_string_list();
	typetable_string_designer_list();
	typetable_package_designer_list();

	typetable_method();
	typetable_class();
	typetable_classnull();
	typetable_standardclass();
	typetable_standardobject();
	typetable_structureclass();
	typetable_structureobject();
	typetable_standard_method();
	typetable_methodcombination();

	/* Array */
	typetable_array_t();
	typetable_array_bit();
	typetable_array_character();
	typetable_array_singlefloat();
	typetable_array_doublefloat();
	typetable_array_longfloat();
	typetable_array_signed8();
	typetable_array_signed16();
	typetable_array_signed32();
	typetable_array_unsigned8();
	typetable_array_unsigned16();
	typetable_array_unsigned32();
#ifdef LISP_64BIT
	typetable_array_signed64();
	typetable_array_unsigned64();
#endif

	/* Arguments */
	typeargs_empty_constant();
	typeargs_optconditionnull();
	typeargs_packagedesigner();
	typeargs_pathnamecase();
	typeargs_error();

	/* Values */
	typevalues_Nil();
	typevalues_T();
	typevalues_Null();
	typevalues_Cons();
	typevalues_List();
	typevalues_Boolean();
	typevalues_Character();
	typevalues_CharacterNull();
	typevalues_Symbol();
	typevalues_Vector();
	typevalues_SimpleVector();
	typevalues_String();
	typevalues_StringNull();
	typevalues_SimpleString();
	typevalues_Stream();
	typevalues_StreamNull();
	typevalues_PrettyStream();
	typevalues_MemoryStream();
	typevalues_Function();
	typevalues_FunctionNull();
	typevalues_EqlT();
	typevalues_Package();
	typevalues_PackageNull();
	typevalues_Sequence();
	typevalues_Array();
	typevalues_Integer();
	typevalues_Ratio();
	typevalues_Rational();
	typevalues_Index();
	typevalues_IndexNull();
	typevalues_Intplus();
	typevalues_IntplusNull();
	typevalues_Bit();
	typevalues_BitArray();
	typevalues_Pathname();
	typevalues_PathnameNull();
	typevalues_LogicalPathname();
	typevalues_Float();
	typevalues_Real();
	typevalues_Number();
	typevalues_Complex();
	typevalues_TypeSymbol();
	typevalues_Class();
	typevalues_ClassNull();

	typevalues_decode_universal_time();
	typevalues_empty();

	/* Compiled-Function */
	typecompiled_object_boolean();
	typecompiled_symbol_boolean();
	typecompiled_stringcase();
	typecompiled_nstringcase();
	typecompiled_stringtrim();
	typecompiled_stringequal();
	typecompiled_stringmismatch();
	typecompiled_rplaca();
	typecompiled_list_list();
	typecompiled_nth();
	typecompiled_nconc();
	typecompiled_renconc();
	typecompiled_butlast();
	typecompiled_macrofunction();
	typecompiled_macroexpand();
	typecompiled_abort();
	typecompiled_continue();
	typecompiled_storevalue();
	typecompiled_macroreader();
	typecompiled_macrodispatch();
	typecompiled_read();
	typecompiled_sublis();
	typecompiled_subst();
	typecompiled_subst_if();
	typecompiled_eq();
	typecompiled_every();
	typecompiled_number_equal();
	typecompiled_number_compare();
	typecompiled_max();
	typecompiled_minusp();
	typecompiled_zerop();
	typecompiled_plus();
	typecompiled_minus();
	typecompiled_oneplus();
	typecompiled_hashtablecount();
	typecompiled_evenp();
	typecompiled_export();
	typecompiled_usepackage();
	typecompiled_intern();
	typecompiled_packagenicknames();
	typecompiled_prin1();
	typecompiled_prin1tostring();
	typecompiled_reverse();
	typecompiled_member();
	typecompiled_memberif();
	typecompiled_mapc();
	typecompiled_acons();
	typecompiled_intersection();
	typecompiled_ecaseerror();
	typecompiled_dosymbols();
	typecompiled_arrayboolean();
	typecompiled_arrayindex();
	typecompiled_bitand();
	typecompiled_countif();
	typecompiled_sort();
	typecompiled_findif();
	typecompiled_positionif();
	typecompiled_search();
	typecompiled_substitute();
	typecompiled_substituteif();
	typecompiled_remove();
	typecompiled_removeif();
	typecompiled_removeduplicates();
	typecompiled_namestring();
	typecompiled_pathname();
	typecompiled_inputstreamp();
	typecompiled_exit();
	typecompiled_readchar();
	typecompiled_writestring();
	typecompiled_finishoutput();
	typecompiled_yesornop();
	typecompiled_floor();
	typecompiled_ffloor();
	typecompiled_envinfo();
	typecompiled_sin();
	typecompiled_realpart();
	typecompiled_gcd();
	typecompiled_mod();
	typecompiled_float_digits();
	typecompiled_rational();
	typecompiled_logand();
	typecompiled_logandc1();
	typecompiled_byte_size();
	typecompiled_deposit_field();
	typecompiled_ldb();
	typecompiled_upgraded_type();
	typecompiled_slot_boundp();
	typecompiled_slot_boundp_method();
	typecompiled_reader_method();
	typecompiled_writer_method();
	typecompiled_signal();
	typecompiled_print_object_method();
	typecompiled_pprint_fill();
	typecompiled_dispatch_function();
	typecompiled_formatter_function();
	typecompiled_get_internal_real_time();
	typecompiled_remove_file();
	typecompiled_infobit();
	typecompiled_chareql();
	typecompiled_make_memory_output_stream();
}


/************************************************************
 *  type_copy.c
 ************************************************************/

typedef void (*call_type_copy)(LocalRoot, addr *, addr);
static call_type_copy TypeCopyTable[LISPDECL_SIZE];

static void type_copy_error(LocalRoot local, addr *ret, addr type)
{
	infobit(type);
	Abort("Invalid type.");
}

static void getset_arraytype(LocalRoot local, addr dst, addr src, size_t index)
{
	Check(GetType(dst) != LISPTYPE_TYPE, "type left error");
	Check(GetType(src) != LISPTYPE_TYPE, "type right error");
	GetArrayType(src, index, &src);
	copylocal_object(local, &src, src);
	SetArrayType(dst, index, src);
}

static void getsettype_arraytype(LocalRoot local, addr dst, addr src, size_t index)
{
	Check(GetType(dst) != LISPTYPE_TYPE, "type left error");
	Check(GetType(src) != LISPTYPE_TYPE, "type right error");
	GetArrayType(src, index, &src);
	type_copy_alloc(local, &src, src);
	SetArrayType(dst, index, src);
}

static void getset_array4(LocalRoot local, addr dst, addr src, size_t index)
{
	Check(GetStatusSize(dst) != LISPSIZE_ARRAY4, "size dst error");
	Check(GetStatusSize(src) != LISPSIZE_ARRAY4, "size src error");
	GetArrayA4(src, index, &src);
	copylocal_object(local, &src, src);
	SetArrayA4(dst, index, src);
}

static void getsettype_array4(LocalRoot local, addr dst, addr src, size_t index)
{
	Check(GetStatusSize(dst) != LISPSIZE_ARRAY4, "size dst error");
	Check(GetStatusSize(src) != LISPSIZE_ARRAY4, "size src error");
	GetArrayA4(src, index, &src);
	type_copy_alloc(local, &src, src);
	SetArrayA4(dst, index, src);
}

static void typecopy_empty(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;

	GetLispDecl(type, &decl);
	Check(lenarrayr(type) != 0, "length error");
	type0_alloc(local, decl, ret);
}

static void typecopy_allobject(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr pos;
	size_t size, i;

	GetLispDecl(type, &decl);
	LenArrayType(type, &size);
	type_alloc(local, &pos, decl, (byte)size);
	for (i = 0; i < size; i++)
		getset_arraytype(local, pos, type, i);
	*ret = pos;
}

static void typecopy_alltype(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr pos;
	size_t size, i;

	GetLispDecl(type, &decl);
	LenArrayType(type, &size);
	type_alloc(local, &pos, decl, (byte)size);
	for (i = 0; i < size; i++)
		getsettype_arraytype(local, pos, type, i);
	*ret = pos;
}

static void typecopy_vector_alltype(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr src, dst, pos;
	size_t size, i;

	GetLispDecl(type, &decl);
	GetArrayType(type, 0, &src);
	LenArrayA4(src, &size);
	vector4_alloc(local, &dst, size);
	for (i = 0; i < size; i++)
		getsettype_array4(local, dst, src, i);
	type_alloc(local, &pos, decl, 1);
	SetArrayType(pos, 0, dst);
	*ret = pos;
}

static void typecopy_error_object(LocalRoot local, addr *ret, addr type)
{
	addr expr;

	GetArrayType(type, 0, &expr);
	GetArrayType(type, 1, &type);
	if (type != Nil)
		type_copy_alloc(local, &type, type);
	copylocal_object(local, &expr, expr);
	type2_alloc(local, LISPDECL_ERROR, expr, type, &type);
	*ret = type;
}

static void typecopy_clos(LocalRoot local, addr *ret, addr type)
{
	addr check;

	if (local == NULL && GetStatusDynamic(type)) {
		GetArrayType(type, 0, &check);
		if (GetStatusDynamic(check))
			Abort("dynamic scope error");
	}
	GetArrayType(type, 0, &type);
	type1_alloc(local, LISPDECL_CLOS, type, &type);
	*ret = type;
}

static void typecopy_eql(LocalRoot local, addr *ret, addr type)
{
	GetArrayType(type, 0, &type);
	copylocal_object(local, &type, type);
	type1_alloc(local, LISPDECL_EQL, type, &type);
	*ret = type;
}

static void typecopy_member(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr src, dst, pos;
	size_t size, i;

	GetLispDecl(type, &decl);
	GetArrayType(type, 0, &src);
	LenArrayA4(src, &size);
	vector4_alloc(local, &dst, size);
	for (i = 0; i < size; i++)
		getset_array4(local, dst, src, i);
	type_alloc(local, &pos, decl, 1);
	SetArrayType(pos, 0, dst);
	*ret = pos;
}


static void copylist_type(LocalRoot local, addr *ret, addr cons)
{
	addr root, child;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &child, &cons);
		type_copy_alloc(local, &child, child);
		cons_alloc(local, &root, child, root);
	}
	nreverse(ret, root);
}

static void copylist_arraytype(LocalRoot local, addr dst, addr src, size_t index)
{
	GetArrayType(src, index, &src);
	copylist_type(local, &src, src);
	SetArrayType(dst, index, src);
}

static void typecopy_values(LocalRoot local, addr *ret, addr type)
{
	addr pos, check;

	type_alloc(local, &pos, LISPDECL_VALUES, 4);
	/* var */
	copylist_arraytype(local, pos, type, 0);
	/* opt */
	copylist_arraytype(local, pos, type, 1);
	/* rest */
	GetArrayType(type, 2, &check);
	if (check != Nil) {
		type_copy_alloc(local, &check, check);
		SetArrayType(pos, 2, check);
	}
	/* allow */
	getset_arraytype(local, pos, type, 3);
	*ret = pos;
}

static void typecopy_vector(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr pos, child;

	GetLispDecl(type, &decl);
	type_alloc(local, &pos, decl, 2);
	/* type */
	getsettype_arraytype(local, pos, type, 0);
	/* dimension */
	GetArrayType(type, 1, &child);
	if (GetType(child) == LISPTYPE_FIXNUM)
		copylocal_object(local, &child, child);
	else
		type_copy_alloc(local, &child, child);
	SetArrayType(pos, 1, child);
	*ret = pos;
}

static void typecopy_size(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr pos, child;

	GetLispDecl(type, &decl);
	type_alloc(local, &pos, decl, 1);

	GetArrayType(type, 0, &child);
	if (integerp(child))
		copylocal_object(local, &child, child);
	else
		type_copy_alloc(local, &child, child);
	SetArrayType(pos, 0, child);
	*ret = pos;
}

static void typecopy_function_key(LocalRoot local, addr *ret, addr list)
{
	addr root, name, type;

	if (list == T) {
		*ret = T;
		return;
	}
	for (root = Nil; list != Nil; ) {
		GetCons(list, &name, &list);
		GetCons(name, &name, &type);
		type_copy_alloc(local, &type, type);
		cons_alloc(local, &name, name, type);
		cons_alloc(local, &root, name, root);
	}
	nreverse(ret, root);
}

static void typecopy_function_arguments(LocalRoot local, addr *ret, addr src)
{
	addr dst, pos;

	if (type_asterisk_p(src)) {
		type_copy_alloc(local, ret, src);
		return;
	}
	vector2_alloc(local, &dst, 4);
	/* var */
	GetArrayA2(src, 0, &pos);
	copylist_type(local, &pos, pos);
	SetArrayA2(dst, 0, pos);
	/* opt */
	GetArrayA2(src, 1, &pos);
	copylist_type(local, &pos, pos);
	SetArrayA2(dst, 1, pos);
	/* rest */
	GetArrayA2(src, 2, &pos);
	if (pos != Nil)
		type_copy_alloc(local, &pos, pos);
	SetArrayA2(dst, 2, pos);
	/* key */
	GetArrayA2(src, 3, &pos);
	typecopy_function_key(local, &pos, pos);
	SetArrayA2(dst, 3, pos);
	*ret = dst;
}

static void typecopy_function(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr pos, child;

	GetLispDecl(type, &decl);
	type_alloc(local, &pos, decl, 3);
	/* arguments */
	GetArrayType(type, 0, &child);
	typecopy_function_arguments(local, &child, child);
	SetArrayType(pos, 0, child);
	/* result */
	getsettype_arraytype(local, pos, type, 1);
	/* check */
	getset_arraytype(local, pos, type, 2);
	*ret = pos;
}

static void typecopy_array_dimension(LocalRoot local, addr *ret, addr src)
{
	addr dst, pos;
	size_t size, i;

	if (type_asterisk_p(src)) {
		type_copy_alloc(local, ret, src);
		return;
	}
	if (GetType(src) == LISPTYPE_FIXNUM) {
		copylocal_object(local, ret, src);
		return;
	}
	Check(GetType(src) != LISPTYPE_VECTOR, "type error");
	LenArrayA4(src, &size);
	vector4_alloc(local, &dst, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(src, i, &pos);
		if (GetType(pos) == LISPTYPE_TYPE)
			type_copy_alloc(local, &pos, pos);
		else
			copylocal_object(local, &pos, pos);
		SetArrayA4(dst, i, pos);
	}
	*ret = dst;
}

static void typecopy_array(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr pos, child;

	GetLispDecl(type, &decl);
	type_alloc(local, &pos, decl, 2);
	/* arguments */
	getsettype_arraytype(local, pos, type, 0);
	/* dimension */
	GetArrayType(type, 1, &child);
	typecopy_array_dimension(local, &child, child);
	SetArrayType(pos, 1, child);
	*ret = pos;
}

void init_type_copy(void)
{
	int i;

	for (i = 0; i < LISPDECL_SIZE; i++)
		TypeCopyTable[i] = type_copy_error;

	TypeCopyTable[LISPDECL_ERROR] = typecopy_error_object;
	TypeCopyTable[LISPDECL_INVALID] = typecopy_empty;
	TypeCopyTable[LISPDECL_TYPE] = typecopy_empty;
	TypeCopyTable[LISPDECL_CLOS] = typecopy_clos;
	TypeCopyTable[LISPDECL_ASTERISK] = typecopy_allobject;
	TypeCopyTable[LISPDECL_OPTIMIZED] = typecopy_alltype;
	TypeCopyTable[LISPDECL_SUBTYPEP] = typecopy_alltype;
	/* Compound-type */
	TypeCopyTable[LISPDECL_AND] = typecopy_vector_alltype;
	TypeCopyTable[LISPDECL_OR] = typecopy_vector_alltype;
	TypeCopyTable[LISPDECL_EQL] = typecopy_eql;
	TypeCopyTable[LISPDECL_MEMBER] = typecopy_member;
	TypeCopyTable[LISPDECL_MOD] = typecopy_allobject;
	TypeCopyTable[LISPDECL_NOT] = typecopy_alltype;
	TypeCopyTable[LISPDECL_SATISFIES] = typecopy_allobject;
	TypeCopyTable[LISPDECL_VALUES] = typecopy_values;
	/* Extract-type */
	TypeCopyTable[LISPDECL_ATOM] = typecopy_empty;
	TypeCopyTable[LISPDECL_LIST] = typecopy_empty;
	TypeCopyTable[LISPDECL_BOOLEAN] = typecopy_empty;
	TypeCopyTable[LISPDECL_VECTOR] = typecopy_vector;
	TypeCopyTable[LISPDECL_SIMPLE_VECTOR] = typecopy_size;
	TypeCopyTable[LISPDECL_BIT_VECTOR] = typecopy_size;
	TypeCopyTable[LISPDECL_SIMPLE_BIT_VECTOR] = typecopy_size;
	TypeCopyTable[LISPDECL_EXTENDED_CHAR] = typecopy_empty;
	TypeCopyTable[LISPDECL_STRING] = typecopy_size;
	TypeCopyTable[LISPDECL_BASE_STRING] = typecopy_size;
	TypeCopyTable[LISPDECL_SIMPLE_STRING] = typecopy_size;
	TypeCopyTable[LISPDECL_SIMPLE_BASE_STRING] = typecopy_size;
	TypeCopyTable[LISPDECL_SIGNED_BYTE] = typecopy_size;
	TypeCopyTable[LISPDECL_UNSIGNED_BYTE] = typecopy_size;
	TypeCopyTable[LISPDECL_BIT] = typecopy_empty;
	TypeCopyTable[LISPDECL_FIXNUM] = typecopy_empty;
	TypeCopyTable[LISPDECL_BIGNUM] = typecopy_empty;
	/* Atomic-type */
	TypeCopyTable[LISPDECL_NIL] = typecopy_empty;
	TypeCopyTable[LISPDECL_T] = typecopy_empty;
	TypeCopyTable[LISPDECL_NULL] = typecopy_empty;
	TypeCopyTable[LISPDECL_CONS] = typecopy_alltype;
	TypeCopyTable[LISPDECL_HASH_TABLE] = typecopy_empty;
	TypeCopyTable[LISPDECL_SYMBOL] = typecopy_empty;
	TypeCopyTable[LISPDECL_KEYWORD] = typecopy_empty;
	TypeCopyTable[LISPDECL_PACKAGE] = typecopy_empty;
	TypeCopyTable[LISPDECL_RANDOM_STATE] = typecopy_empty;
	TypeCopyTable[LISPDECL_READTABLE] = typecopy_empty;
	TypeCopyTable[LISPDECL_FUNCTION] = typecopy_function;
	TypeCopyTable[LISPDECL_COMPILED_FUNCTION] = typecopy_function;
	TypeCopyTable[LISPDECL_PATHNAME] = typecopy_empty;
	TypeCopyTable[LISPDECL_LOGICAL_PATHNAME] = typecopy_empty;
	TypeCopyTable[LISPDECL_SEQUENCE] = typecopy_empty;
	TypeCopyTable[LISPDECL_ARRAY] = typecopy_array;
	TypeCopyTable[LISPDECL_SIMPLE_ARRAY] = typecopy_array;
	TypeCopyTable[LISPDECL_CHARACTER] = typecopy_empty;
	TypeCopyTable[LISPDECL_BASE_CHAR] = typecopy_empty;
	TypeCopyTable[LISPDECL_STANDARD_CHAR] = typecopy_empty;
	TypeCopyTable[LISPDECL_NUMBER] = typecopy_empty;
	TypeCopyTable[LISPDECL_REAL] = typecopy_allobject;
	TypeCopyTable[LISPDECL_RATIO] = typecopy_allobject;
	TypeCopyTable[LISPDECL_INTEGER] = typecopy_allobject;
	TypeCopyTable[LISPDECL_RATIONAL] = typecopy_allobject;
	TypeCopyTable[LISPDECL_COMPLEX] = typecopy_alltype;
	TypeCopyTable[LISPDECL_FLOAT] = typecopy_allobject;
	TypeCopyTable[LISPDECL_SHORT_FLOAT] = typecopy_allobject;
	TypeCopyTable[LISPDECL_SINGLE_FLOAT] = typecopy_allobject;
	TypeCopyTable[LISPDECL_DOUBLE_FLOAT] = typecopy_allobject;
	TypeCopyTable[LISPDECL_LONG_FLOAT] = typecopy_allobject;
	TypeCopyTable[LISPDECL_RESTART] = typecopy_empty;
	TypeCopyTable[LISPDECL_ENVIRONMENT] = typecopy_empty;
	TypeCopyTable[LISPDECL_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_BROADCAST_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_CONCATENATED_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_ECHO_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_FILE_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_STRING_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_SYNONYM_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_TWO_WAY_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_PROMPT_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_PRETTY_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_MEMORY_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_QUOTE] = typecopy_empty;
	TypeCopyTable[LISPDECL_BYTESPEC] = typecopy_empty;
	TypeCopyTable[LISPDECL_PRINT_DISPATCH] = typecopy_empty;
	TypeCopyTable[LISPDECL_EVAL] = typecopy_empty;
}


/*
 *  type-copy
 */
void type_copy_alloc(LocalRoot local, addr *ret, addr type)
{
	call_type_copy call;
	addr pos;

	CheckType(type, LISPTYPE_TYPE);
	call = TypeCopyTable[(int)RefLispDecl(type)];
	Check(call == NULL, "build error");
	call(local, &pos, type);
	type_setnotobject(pos, type);
	*ret = pos;
}
void type_copy_local(LocalRoot local, addr *ret, addr type)
{
	Check(local == NULL, "local error");
	type_copy_alloc(local, ret, type);
}
void type_copy_heap(addr *ret, addr type)
{
	type_copy_alloc(NULL, ret, type);
}


/*
 *  type-throw
 */
void type_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (local)
		type_throw_local(local, pos, ret);
	else
		type_throw_heap(pos, ret);
}
void type_throw_local(LocalRoot local, addr pos, addr *ret)
{
	CheckLocal(local);
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		type_copy_local(local, ret, pos);
}
void type_throw_heap(addr pos, addr *ret)
{
	if (GetStatusDynamic(pos))
		type_copy_heap(ret, pos);
	else
		*ret = pos;
}


/************************************************************
 *  type_deftype.c
 ************************************************************/

void getdeftype(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getdeftype_symbol(symbol, ret);
}

int setdeftype_(addr symbol, addr pos)
{
	CheckSymbol(symbol);
	return setdeftype_symbol_(symbol, pos);
}

int symbol_deftypep(addr symbol)
{
	CheckSymbol(symbol);
	getdeftype(symbol, &symbol);
	return symbol != Nil;
}

int execute_list_deftype(Execute ptr, addr *ret, addr list, addr env)
{
	addr call, symbol;

	CheckType(list, LISPTYPE_CONS);
	GetCar(list, &symbol);
	CheckSymbol(symbol);
	getdeftype(symbol, &call);
	if (call == Nil)
		return Result(ret, NULL);

	return callclang_funcall(ptr, ret, call, list, env, NULL);
}

int execute_symbol_deftype(Execute ptr, addr *ret, addr symbol, addr env)
{
	addr call;

	CheckSymbol(symbol);
	getdeftype(symbol, &call);
	if (call == Nil)
		return Result(ret, NULL);

	/* name -> (call `(name ,@args) env) */
	cons_heap(&symbol, symbol, Nil);
	return callclang_funcall(ptr, ret, call, symbol, env, NULL);
}


/*
 *  deftype
 */
int deftype_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr right, eval, name, args, decl, doc;

	/* (deftype . form) */
	Return_getcdr(form, &right);
	if (right == Nil)
		return fmte_("deftype form must have at least a name and body.", NULL);
	if (! consp(right))
		return fmte_("Invalid deftype form.", NULL);

	/* name */
	Return_getcons(right, &name, &right);
	if (! symbolp(name))
		return fmte_("deftype name ~S must be a symbol.", name, NULL);
	if (right == Nil)
		return fmte_("deftype form must have at least a name and body.", NULL);
	if (! consp(right))
		return fmte_("Invalid deftype form.", NULL);

	/* args */
	Return_getcons(right, &args, &right);
	if (! IsList(right))
		return fmte_("Invalid deftype form.", NULL);

	/* parse */
	Return(lambda_deftype_(ptr->local, &args, args, Nil));
	Return(declare_body_documentation_(ptr, env, right, &doc, &decl, &right));

	/* (eval::deftype name args decl doc body) */
	GetConst(SYSTEM_DEFTYPE, &eval);
	list_heap(ret, eval, name, args, decl, doc, right, NULL);

	return 0;
}


/************************************************************
 *  type_error.c
 ************************************************************/

static int get_error_type_no_error(Execute ptr, addr pos, addr *ret)
{
	int notp;
	addr expr, type;

	CheckType(pos, LISPTYPE_TYPE);
	Check(RefLispDecl(pos) != LISPDECL_ERROR, "decl error");
	GetArrayType(pos, 1, &type);
	if (type != Nil)
		return Result(ret, type);

	/* parse-type */
	GetNotDecl(pos, &notp);
	GetArrayType(pos, 0, &expr);
	if (notp) {
		Return(parse_type_not(ptr, &type, expr, Nil));
	}
	else {
		Return(parse_type(ptr, &type, expr, Nil));
	}

	/* error check */
	if (type_error_p(type))
		return Result(ret, Nil);

	/* Result */
	if (notp) {
		SetNotDecl(pos, 0);
	}
	SetArrayType(pos, 1, type);

	return Result(ret, type);
}

int get_error_type_(Execute ptr, addr pos, addr *ret)
{
	addr check;

	Return(get_error_type_no_error(ptr, pos, &check));
	if (check != Nil)
		return Result(ret, check);

	/* error */
	*ret = Nil;
	GetArrayType(pos, 0, &check);
	return call_type_error_va_(ptr, check, Nil, "Invalid type-spec ~S.", check, NULL);
}


/*
 *  check-error-type
 */
typedef int (*type_error_calltype)(Execute, addr, int, int *);
static type_error_calltype TypeErrorCall[LISPDECL_SIZE];

static int check_error_call_(Execute ptr, addr pos, int errorp, int *ret)
{
	LispDecl decl;
	type_error_calltype call_;

	CheckType(pos, LISPTYPE_TYPE);
	GetLispDecl(pos, &decl);
	call_ = TypeErrorCall[decl];
	if (call_ == NULL)
		return Result(ret, 1);

	return (*call_)(ptr, pos, errorp ,ret);
}

static int check_error_type_error_(Execute ptr, addr pos, int errorp, int *ret)
{
	addr check;

	Return(get_error_type_no_error(ptr, pos, &check));
	if (check != Nil)
		return Result(ret, 1);
	if (! errorp)
		return Result(ret, 0);

	/* type-error */
	*ret = 0;
	GetArrayType(pos, 0, &check);
	return call_type_error_va_(ptr, check, Nil, "Invalid type-spec ~S.", check, NULL);
}

static int check_error_type_get1_(Execute ptr, addr pos, int errorp, int *ret)
{
	GetArrayType(pos, 0, &pos);
	return check_error_call_(ptr, pos, errorp, ret);
}

static int check_error_type_loop1_(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	addr vector;
	size_t size, i;

	GetArrayType(pos, 0, &vector);
	LenArrayA4(vector, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(vector, i, &pos);
		Return(check_error_call_(ptr, pos, errorp, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int check_error_type_cons_(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	addr x;

	/* car */
	GetArrayType(pos, 0, &x);
	Return(check_error_call_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* cdr */
	GetArrayType(pos, 1, &x);
	Return(check_error_call_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	return Result(ret, 1);
}

static int check_error_type_loop_(Execute ptr, addr list, int errorp, int *ret)
{
	int check;
	addr x;

	while (list != Nil) {
		GetCons(list, &x, &list);
		Return(check_error_call_(ptr, x, errorp, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int check_error_type_values_(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	addr x;

	/* var */
	GetArrayType(pos, 0, &x);
	Return(check_error_type_loop_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* opt */
	GetArrayType(pos, 1, &x);
	Return(check_error_type_loop_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* rest */
	GetArrayType(pos, 2, &x);
	Return(check_error_call_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	return Result(ret, 1);
}

static int check_error_type_function_key_(Execute ptr, addr list, int errorp, int *ret)
{
	int check;
	addr x;

	if (list == T)
		return Result(ret, 1);
	while (list != Nil) {
		GetCons(list, &x, &list);
		GetCdr(x, &x);
		Return(check_error_call_(ptr, x, errorp, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int check_error_type_function_arguments_(Execute ptr,
		addr pos, int errorp, int *ret)
{
	int check;
	addr x;

	if (type_astert_p(pos))
		return Result(ret, 1);

	/* var */
	GetArrayA2(pos, 0, &x);
	Return(check_error_type_loop_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* opt */
	GetArrayA2(pos, 1, &x);
	Return(check_error_type_loop_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* rest */
	GetArrayA2(pos, 2, &x);
	if (x != Nil) {
		Return(check_error_call_(ptr, x, errorp, &check));
		if (! check)
			return Result(ret, 0);
	}

	/* key */
	GetArrayA2(pos, 3, &x);
	Return(check_error_type_function_key_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	return 0;
}

static int check_error_type_function_(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	addr x;

	/* arguments */
	GetArrayType(pos, 0, &x);
	Return(check_error_type_function_arguments_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* values */
	GetArrayType(pos, 1, &x);
	Return(check_error_call_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	return Result(ret, 1);
}

int check_error_type_(Execute ptr, addr pos, int *ret)
{
	return check_error_call_(ptr, pos, 0, ret);
}

int execute_error_type_(Execute ptr, addr pos)
{
	int ignore;
	return check_error_call_(ptr, pos, 1, &ignore);
}

void init_type_error(void)
{
	cleartype(TypeErrorCall);
	TypeErrorCall[LISPDECL_ERROR] = check_error_type_error_;
	TypeErrorCall[LISPDECL_OPTIMIZED] = check_error_type_get1_;
	TypeErrorCall[LISPDECL_SUBTYPEP] = check_error_type_get1_;
	TypeErrorCall[LISPDECL_AND] = check_error_type_loop1_;
	TypeErrorCall[LISPDECL_OR] = check_error_type_loop1_;
	TypeErrorCall[LISPDECL_NOT] = check_error_type_get1_;
	TypeErrorCall[LISPDECL_CONS] = check_error_type_cons_;
	TypeErrorCall[LISPDECL_VECTOR] = check_error_type_get1_;
	TypeErrorCall[LISPDECL_ARRAY] = check_error_type_get1_;
	TypeErrorCall[LISPDECL_SIMPLE_ARRAY] = check_error_type_get1_;
	TypeErrorCall[LISPDECL_VALUES] = check_error_type_values_;
	TypeErrorCall[LISPDECL_FUNCTION] = check_error_type_function_;
	TypeErrorCall[LISPDECL_COMPILED_FUNCTION] = check_error_type_function_;
}


/************************************************************
 *  type_function.c
 ************************************************************/

void make_ordargs(ordargs *ptr, addr pos)
{
	int check1, check2;

	clearpoint(ptr);
	GetArrayA2(pos, 0, &(ptr->var));
	GetArrayA2(pos, 1, &(ptr->opt));
	GetArrayA2(pos, 2, &(ptr->rest));
	GetArrayA2(pos, 3, &(ptr->key));
	ptr->size_var = length_list_unsafe(ptr->var);
	ptr->size_opt = length_list_unsafe(ptr->opt);
	ptr->size_key = (ptr->key == T)? 0: length_list_unsafe(ptr->key);
	ptr->pos_rest = ptr->size = ptr->size_var + ptr->size_opt;
	check1 = (ptr->rest != Nil);
	check2 = (ptr->key != Nil);
	if (check2) /* (rest key) or (key) */
		ptr->size += 2;
	else if (check1) /* (rest) */
		ptr->size++;
}

int gettype_ordargs_(const ordargs *ptr, size_t index, ordtype *ret)
{
	int check1, check2;

	memset(ret, 0, sizeoft(ordtype));
	ret->type = Nil;

	/* var */
	if (index < ptr->size_var) {
		Return(getnth_(ptr->var, index, &(ret->type)));
		ret->var = 1;
		return 0;
	}
	index -= ptr->size_var;

	/* opt */
	if (index < ptr->size_opt) {
		Return(getnth_(ptr->opt, index, &(ret->type)));
		ret->var = 1;
		return 0;
	}
	index -= ptr->size_opt;

	/* rest */
	check1 = (ptr->rest == Nil);
	check2 = (ptr->key == Nil);
	if (check1 && check2) {
		ret->nil = 1;
		return 0;
	}
	if (! check1) {
		ret->type = ptr->rest;
		ret->var = 1;
		ret->rest = 1;
	}

	/* key */
	if (! check2) {
		if ((index % 2) == 0) {
			ret->key = 1;
			ret->value = 0;
		}
		else {
			ret->key = 0;
			ret->value = 1;
		}
	}

	return 0;
}

int simple_p_ordargs(const ordargs *ptr)
{
	return ptr->rest == Nil && ptr->key == Nil;
}

static void merge_key_ordargs(LocalRoot local, addr *ret, const ordargs *ptr)
{
	addr cons, array, pos;
	size_t size, i;

	/* &allow-other-keys */
	if (ptr->key == T) {
		GetTypeTable(ret, Symbol);
		return;
	}

	/* (eql key) */
	cons = ptr->key;
	if (singlep(cons)) {
		GetCar(cons, &pos);
		GetCar(pos, &pos);
		type_eql_local(local, pos, ret);
		return;
	}

	/* (or (eql key1) (eql key2) ...) */
	size = length_list_unsafe(cons);
	vector4_alloc(local, &array, size);
	for (i = 0; i < size; i++) {
		GetCons(cons, &pos, &cons);
		GetCar(pos, &pos);
		type_eql_local(local, pos, &pos);
		SetArrayA4(array, i, pos);
	}
	type1_local(local, LISPDECL_OR, array, ret);
}

static void merge_value_ordargs(LocalRoot local, addr *ret, const ordargs *ptr)
{
	addr cons, array, pos;
	size_t size, i;

	/* &allow-other-keys */
	if (ptr->key == T) {
		GetTypeTable(ret, T);
		return;
	}

	/* type */
	cons = ptr->key;
	if (singlep(cons)) {
		GetCar(cons, &pos);
		GetCdr(pos, ret);
		return;
	}

	/* (or type1 type2 ...) */
	size = length_list_unsafe(cons);
	vector4_alloc(local, &array, size);
	for (i = 0; i < size; i++) {
		GetCons(cons, &pos, &cons);
		GetCdr(pos, &pos);
		SetArrayA4(array, i, pos);
	}
	type1_local(local, LISPDECL_OR, array, ret);
}

void merge_ordargs(LocalRoot local, addr *ret, const ordargs *ptr, const ordtype *type)
{
	addr pos;

	/* var        -> type */
	/* rest       -> type */
	/* key        -> (and key1 key2 ...) */
	/* rest + key -> (or rest (and key1 key2 ...)) */
	/* &allow-other-keys -> (symbol t) */
	Check(type->nil, "nil error");

	/* var only */
	if (type->var && (! type->rest)) {
		*ret = type->type;
		return;
	}

	/* rest only */
	if (type->rest && (! type->key) && (! type->value)) {
		*ret = type->type;
		return;
	}

	/* key */
	if (type->key) {
		merge_key_ordargs(local, &pos, ptr);
		if (type->rest)
			type2and_local(local, type->type, pos, &pos);
		*ret = pos;
		return;
	}

	/* value */
	if (type->value) {
		merge_value_ordargs(local, &pos, ptr);
		if (type->rest)
			type2and_local(local, type->type, pos, &pos);
		*ret = pos;
		return;
	}

	/* error */
	*ret = 0;
	Abort("type error");
}


/*
 *  size_check
 */
static int size_check_recursive_ordcall_(addr pos, int value, size_t size, int *ret);

static int size_check_optimized_ordcall_(addr pos, int value, size_t size, int *ret)
{
	get_type_optimized(&pos, pos);
	return size_check_recursive_ordcall_(pos, value, size, ret);
}

static int size_check_subtypep_ordcall_(addr pos, int value, size_t size, int *ret)
{
	get_type_subtypep(&pos, pos);
	return size_check_recursive_ordcall_(pos, value, size, ret);
}

static int size_check_t_ordcall_(addr pos, int value, size_t size, int *ret)
{
	return Result(ret, value);
}

static int size_check_nil_ordcall_(addr pos, int value, size_t size, int *ret)
{
	return Result(ret, ! value);
}

static int size_check_not_ordcall_(addr pos, int value, size_t size, int *ret)
{
	GetArrayType(pos, 0, &pos);
	return size_check_recursive_ordcall_(pos, ! value, size, ret);
}

static int size_check_function_ordcall_(addr pos, int value, size_t size, int *ret)
{
	size_t check;
	addr x;

	Check(! type_function_p(pos), "type error");
	GetArrayType(pos, 0, &pos); /* args */

	/* asterisk */
	if (type_asterisk_p(pos))
		return Result(ret, value);

	/* var */
	GetArrayA2(pos, 0, &x);
	check = length_list_unsafe(x);
	if (size == check)
		return Result(ret, value);
	if (size < check)
		goto error;

	/* opt */
	GetArrayA2(pos, 1, &x);
	check += length_list_unsafe(x);
	if (size <= check)
		return Result(ret, value);

	/* key */
	GetArrayA2(pos, 3, &x);
	if (x != Nil) {
		check -= size;
		if (check % 2)
			return fmte_("There is no value in &key argument.", NULL);
		return Result(ret, value);
	}

	/* rest */
	GetArrayA2(pos, 2, &x);
	if (x != Nil)
		return Result(ret, value);

error:
	return Result(ret, ! value);
}

static int size_check_and_ordcall_(addr pos, int value, size_t size, int *ret)
{
	int loop_check, check;
	addr x;
	size_t loop, i;

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &loop);
	loop_check = 1;
	for (i = 0; i < loop; i++) {
		GetArrayA4(pos, i, &x);
		Return(size_check_recursive_ordcall_(x, 1, size, &check));
		if (! check)
			loop_check = 0;
	}
	if (! loop_check)
		value = ! value;

	return Result(ret, value);
}

static int size_check_or_ordcall_(addr pos, int value, size_t size, int *ret)
{
	int loop_check, check;
	addr x;
	size_t loop, i;

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &loop);
	loop_check = 0;
	for (i = 0; i < loop; i++) {
		GetArrayA4(pos, i, &x);
		Return(size_check_recursive_ordcall_(x, 1, size, &check));
		if (check)
			loop_check = 1;
	}
	if (! loop_check)
		value = ! value;

	return Result(ret, value);
}

static int size_check_recursive_ordcall_(addr pos, int value, size_t size, int *ret)
{
	int notp;
	enum LISPDECL decl;

	CheckType(pos, LISPTYPE_TYPE);
	GetLispDecl(pos, &decl);
	GetNotDecl(pos, &notp);
	if (notp)
		value = ! value;

	switch (decl) {
		case LISPDECL_OPTIMIZED:
			return size_check_optimized_ordcall_(pos, value, size, ret);

		case LISPDECL_SUBTYPEP:
			return size_check_subtypep_ordcall_(pos, value, size, ret);

		case LISPDECL_ASTERISK:
		case LISPDECL_T:
			return size_check_t_ordcall_(pos, value, size, ret);

		case LISPDECL_NIL:
			return size_check_nil_ordcall_(pos, value, size, ret);

		case LISPDECL_NOT:
			return size_check_not_ordcall_(pos, value, size, ret);

		case LISPDECL_FUNCTION:
		case LISPDECL_COMPILED_FUNCTION:
			return size_check_function_ordcall_(pos, value, size, ret);

		case LISPDECL_AND:
			return size_check_and_ordcall_(pos, value, size, ret);

		case LISPDECL_OR:
			return size_check_or_ordcall_(pos, value, size, ret);

		default:
			return fmte_("Invalid type-specifier ~S.", pos, NULL);
	}
}

int size_check_ordcall_(addr pos, size_t size, int *ret)
{
	CheckType(pos, LISPTYPE_TYPE);
	*ret = 0;
	return size_check_recursive_ordcall_(pos, 1, size, ret);
}


/*
 *  gettype_ordcall
 */
static void reverse_ordcall(addr pos, int notp)
{
	CheckType(pos, LISPTYPE_TYPE);
	if (notp == 0)
		return;
	GetNotDecl(pos, &notp);
	notp = ! notp;
	SetNotDecl(pos, notp);
}

static int gettype_optimized_ordcall_(addr pos, size_t i, addr *ret)
{
	int notp;

	GetNotDecl(pos, &notp);
	get_type_optimized(&pos, pos);
	Return(gettype_ordcall_(pos, i, &pos));
	reverse_ordcall(pos, notp);

	return Result(ret, pos);
}

static int gettype_subtypep_ordcall_(addr pos, size_t i, addr *ret)
{
	int notp;

	GetNotDecl(pos, &notp);
	get_type_subtypep(&pos, pos);
	Return(gettype_ordcall_(pos, i, &pos));
	reverse_ordcall(pos, notp);

	return Result(ret, pos);
}

static void gettype_bool_ordcall(int value, addr *ret)
{
	if (value)
		type0_heap(LISPDECL_T, ret);
	else
		type0_heap(LISPDECL_NIL, ret);
}

static int gettype_t_ordcall_(addr pos, size_t i, addr *ret)
{
	int notp;

	GetNotDecl(pos, &notp);
	gettype_bool_ordcall(! notp, ret);

	return 0;
}

static int gettype_nil_ordcall_(addr pos, size_t i, addr *ret)
{
	int notp;

	GetNotDecl(pos, &notp);
	gettype_bool_ordcall(notp, ret);

	return 0;
}

static int gettype_not_ordcall_(addr pos, size_t i, addr *ret)
{
	int notp;

	GetNotDecl(pos, &notp);
	notp = ! notp;
	GetArrayType(pos, 0, &pos);
	get_type_optimized(&pos, pos);
	Return(gettype_ordcall_(pos, i, &pos));
	reverse_ordcall(pos, notp);

	return Result(ret, pos);
}

static int gettype_function_ordcall_(addr pos, size_t i, addr *ret)
{
	int notp;
	ordargs str;
	ordtype type;
	LocalRoot local;
	LocalStack stack;

	Check(! type_function_p(pos), "type error");
	GetNotDecl(pos, &notp);
	GetArrayType(pos, 0, &pos); /* args */

	/* asterisk */
	if (type_asterisk_p(pos)) {
		gettype_bool_ordcall(! notp, ret);
		return 0;
	}

	/* ordargs */
	make_ordargs(&str, pos);
	Return(gettype_ordargs_(&str, i, &type));
	if (type.nil) {
		gettype_bool_ordcall(notp, ret);
		return 0;
	}

	local = Local_Thread;
	push_local(local, &stack);
	merge_ordargs(local, &pos, &str, &type);
	type_copy_heap(&pos, pos);
	reverse_ordcall(pos, notp);
	rollback_local(local, stack);

	return Result(ret, pos);
}

static int gettype_vector_ordcall_(enum LISPDECL decl, addr pos, size_t i, addr *ret)
{
	int notp;
	addr type, vector, x;
	size_t size, index;

	GetNotDecl(pos, &notp);
	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);

	vector4_heap(&vector, size);
	type1_heap(decl, vector, &type);
	SetNotDecl(type, notp);

	for (index = 0; index < size; index++) {
		GetArrayA4(pos, index, &x);
		Return(gettype_ordcall_(x, index, &x));
		SetArrayA4(vector, index, x);
	}

	return Result(ret, type);
}

static int gettype_and_ordcall_(addr pos, size_t i, addr *ret)
{
	return gettype_vector_ordcall_(LISPDECL_OR, pos, i, ret);
}

static int gettype_or_ordcall_(addr pos, size_t i, addr *ret)
{
	return gettype_vector_ordcall_(LISPDECL_AND, pos, i, ret);
}

static int gettype_error_ordcall_(addr pos, size_t i, addr *ret)
{
	int notp;

	GetNotDecl(pos, &notp);
	if (notp) {
		type0_heap(LISPDECL_T, ret);
		return 0;
	}

	*ret = Nil;
	return fmte_("Invalid type-specifier ~S in function type.", pos, NULL);
}

int gettype_ordcall_(addr pos, size_t i, addr *ret)
{
	CheckType(pos, LISPTYPE_TYPE);
	switch (RefLispDecl(pos)) {
		case LISPDECL_OPTIMIZED:
			return gettype_optimized_ordcall_(pos, i, ret);

		case LISPDECL_SUBTYPEP:
			return gettype_subtypep_ordcall_(pos, i, ret);

		case LISPDECL_ASTERISK:
		case LISPDECL_T:
			return gettype_t_ordcall_(pos, i, ret);

		case LISPDECL_NIL:
			return gettype_nil_ordcall_(pos, i, ret);

		case LISPDECL_NOT:
			return gettype_not_ordcall_(pos, i, ret);

		case LISPDECL_FUNCTION:
		case LISPDECL_COMPILED_FUNCTION:
			return gettype_function_ordcall_(pos, i, ret);

		case LISPDECL_AND:
			return gettype_and_ordcall_(pos, i, ret);

		case LISPDECL_OR:
			return gettype_or_ordcall_(pos, i, ret);

		default:
			return gettype_error_ordcall_(pos, i, ret);
	}
}


/*
 *  function values
 */
enum OrdValues_Type {
	OrdValues_Var,
	OrdValues_Opt,
	OrdValues_Rest
};
typedef enum OrdValues_Type ordvalues;

static void make_recursive_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype);

static void make_optimized_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	int notp;

	GetNotDecl(pos, &notp);
	get_type_optimized(&pos, pos);
	make_recursive_ordvalues(pos, i, &pos, rtype);
	reverse_ordcall(pos, notp);
	*ret = pos;
}

static void make_subtypep_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	int notp;

	GetNotDecl(pos, &notp);
	get_type_subtypep(&pos, pos);
	make_recursive_ordvalues(pos, i, &pos, rtype);
	reverse_ordcall(pos, notp);
	*ret = pos;
}

static void make_t_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	int notp;

	GetNotDecl(pos, &notp);
	gettype_bool_ordcall(! notp, ret);
	*rtype = (i == 0)? OrdValues_Var: OrdValues_Rest;
}

static void make_nil_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	int notp;

	GetNotDecl(pos, &notp);
	gettype_bool_ordcall(notp, ret);
	*rtype = (i == 0)? OrdValues_Var: OrdValues_Rest;
}

static void make_not_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	int notp;

	GetNotDecl(pos, &notp);
	notp = ! notp;
	GetArrayType(pos, 0, &pos);
	get_type_optimized(&pos, pos);
	make_recursive_ordvalues(pos, i, &pos, rtype);
	reverse_ordcall(pos, notp);
	*ret = pos;
}

static void make_function_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	int notp;
	addr x;
	size_t size;

	Check(! type_function_p(pos), "type error");
	GetNotDecl(pos, &notp);
	GetArrayType(pos, 1, &pos); /* values */

	/* asterisk */
	if (RefLispDecl(pos) != LISPDECL_VALUES) {
		make_recursive_ordvalues(pos, i, ret, rtype);
		return;
	}

	/* var */
	GetArrayA2(pos, 0, &x);
	size = length_list_unsafe(x);
	if (i < size) {
		getnth_unsafe(x, i, &x);
		*rtype = OrdValues_Var;
		goto result;
	}

	/* opt */
	GetArrayA2(pos, 1, &x);
	i -= size;
	size = length_list_unsafe(x);
	if (i < size) {
		getnth_unsafe(x, i, &x);
		*rtype = OrdValues_Opt;
		goto result;
	}

	/* rest */
	GetArrayA2(pos, 2, &x);
	*rtype = OrdValues_Rest;

result:
	type_copy_heap(&x, x);
	reverse_ordcall(x, notp);
	*ret = x;
}

static void make_vector_ordvalues(enum LISPDECL decl,
		addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	ordvalues v, check;
	int notp;
	addr type, vector, x;
	size_t size, index;

	GetNotDecl(pos, &notp);
	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);

	vector4_heap(&vector, size);
	type1_heap(decl, vector, &type);
	SetNotDecl(type, notp);

	v = OrdValues_Rest;
	for (index = 0; index < size; index++) {
		GetArrayA4(pos, index, &x);
		make_recursive_ordvalues(x, i, &x, &check);
		SetArrayA4(vector, index, x);

		/* valeus check */
		if (check == OrdValues_Var)
			v = OrdValues_Var;
		else if (check == OrdValues_Opt && v == OrdValues_Rest)
			v = OrdValues_Opt;
	}

	*rtype = v;
	*ret = type;
}

static void make_and_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	make_vector_ordvalues(LISPDECL_AND, pos, i, ret, rtype);
}

static void make_or_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	make_vector_ordvalues(LISPDECL_OR, pos, i, ret, rtype);
}

static void make_type_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	if (i == 0) {
		type_copy_heap(ret, pos);
		*rtype = OrdValues_Var;
	}
	else {
		make_t_ordvalues(pos, i, ret, rtype);
	}
}

static void make_recursive_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	CheckType(pos, LISPTYPE_TYPE);
	switch (RefLispDecl(pos)) {
		case LISPDECL_OPTIMIZED:
			make_optimized_ordvalues(pos, i, ret, rtype);
			break;

		case LISPDECL_SUBTYPEP:
			make_subtypep_ordvalues(pos, i, ret, rtype);
			break;

		case LISPDECL_ASTERISK:
		case LISPDECL_T:
			make_t_ordvalues(pos, i, ret, rtype);
			break;

		case LISPDECL_NIL:
			make_nil_ordvalues(pos, i, ret, rtype);
			break;

		case LISPDECL_NOT:
			make_not_ordvalues(pos, i, ret, rtype);
			break;

		case LISPDECL_FUNCTION:
		case LISPDECL_COMPILED_FUNCTION:
			make_function_ordvalues(pos, i, ret, rtype);
			break;

		case LISPDECL_AND:
			make_and_ordvalues(pos, i, ret, rtype);
			break;

		case LISPDECL_OR:
			make_or_ordvalues(pos, i, ret, rtype);
			break;

		default:
			make_type_ordvalues(pos, i, ret, rtype);
			break;
	}
}

void make_ordvalues_heap(addr pos, addr *ret)
{
	ordvalues type;
	addr x, var, opt, rest;
	size_t i;

	var = opt = rest = Nil;
	for (i = 0; ; i++) {
		make_recursive_ordvalues(pos, i, &x, &type);
		/* var */
		if (type == OrdValues_Var) {
			cons_heap(&var, x, var);
			continue;
		}
		/* opt */
		if (type == OrdValues_Opt) {
			cons_heap(&opt, x, opt);
			continue;
		}
		/* rest */
		rest = x;
		break;
	}
	nreverse(&var, var);
	nreverse(&opt, opt);

	/* type */
	type4_heap(LISPDECL_VALUES, var, opt, rest, Nil, ret);
}


/************************************************************
 *  type_memory.c
 ************************************************************/

/*
 *  allocate
 */
void type_alloc(LocalRoot local, addr *ret, LispDecl type, size_t size)
{
	addr pos;

	Check(LISPDECL_SIZE <= type, "type too large.");
	alloc_array2(local, &pos, LISPTYPE_TYPE, size);
	SetUser(pos, (byte)type);
	*ret = pos;
}

void type_local(LocalRoot local, addr *ret, LispDecl type, size_t size)
{
	CheckLocal(local);
	type_alloc(local, ret, type, size);
}

void type_heap(addr *ret, LispDecl type, size_t size)
{
	type_alloc(NULL, ret, type, size);
}


/*
 *  access
 */
LispDecl type_lowlispdecl(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	return LowLispDecl_Low(pos);
}

LispDecl type_reflispdecl(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	return RefLispDecl_Low(pos);
}

void type_getlispdecl(addr pos, LispDecl *ret)
{
	CheckType(pos, LISPTYPE_TYPE);
	GetLispDecl_Low(pos, ret);
}

void type_setlispdecl(addr pos, LispDecl value)
{
	CheckType(pos, LISPTYPE_TYPE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetLispDecl_Low(pos, value);
}

int type_refnotdecl(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	return RefNotDecl_Low(pos);
}

void type_getnotdecl(addr pos, int *ret)
{
	CheckType(pos, LISPTYPE_TYPE);
	GetNotDecl_Low(pos, ret);
}

void type_setnotdecl(addr pos, int value)
{
	byte user;

	CheckType(pos, LISPTYPE_TYPE);
	Check(GetStatusReadOnly(pos), "readonly error");
	user = GetUser(pos);
	if (value)
		SetUser(pos, 0x80 | user);
	else
		SetUser(pos, 0x7F & user);
}

void type_revnotdecl(addr pos)
{
	type_setnotdecl(pos, RefNotDecl(pos) == 0);
}

void type_setnotobject(addr pos, addr value)
{
	SetNotDecl(pos, RefNotDecl(value));
}

addr type_refarraytype(addr pos, size_t index)
{
	CheckType(pos, LISPTYPE_TYPE);
	return RefArrayType_Low(pos, index);
}

void type_getarraytype(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPTYPE_TYPE);
	GetArrayType_Low(pos, index, ret);
}

void type_setarraytype(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPTYPE_TYPE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetArrayType_Low(pos, index, value);
}

void type_lenarraytype(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_TYPE);
	LenArrayType_Low(pos, ret);
}

void type_getvalues1(addr type, addr *ret)
{
	addr check;

	CheckType(type, LISPTYPE_TYPE);
	/* normal type */
	if (RefLispDecl(type) != LISPDECL_VALUES) {
		*ret = type;
		return;
	}
	/* var */
	GetArrayType(type, 0, &check);
	if (check != Nil) {
		GetCar(check, ret);
		return;
	}
	/* opt */
	GetArrayType(type, 1, &check);
	if (check != Nil) {
		GetCar(check, ret);
		return;
	}
	/* rest */
	GetArrayType(type, 2, ret);
}


/*
 *  object
 */
void type0_alloc(LocalRoot local, LispDecl type, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 0);
	*ret = pos;
}

void type1_alloc(LocalRoot local, LispDecl type, addr a, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 1);
	SetArrayType(pos, 0, a);
	*ret = pos;
}

void type2_alloc(LocalRoot local, LispDecl type, addr a, addr b, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 2);
	SetArrayType(pos, 0, a);
	SetArrayType(pos, 1, b);
	*ret = pos;
}

void type3_alloc(LocalRoot local, LispDecl type, addr a, addr b, addr c, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 3);
	SetArrayType(pos, 0, a);
	SetArrayType(pos, 1, b);
	SetArrayType(pos, 2, c);
	*ret = pos;
}

void type4_alloc(LocalRoot local, LispDecl type,
		addr a, addr b, addr c, addr d, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 4);
	SetArrayType(pos, 0, a);
	SetArrayType(pos, 1, b);
	SetArrayType(pos, 2, c);
	SetArrayType(pos, 3, d);
	*ret = pos;
}

void type0_local(LocalRoot local, LispDecl type, addr *ret)
{
	CheckLocal(local);
	type0_alloc(local, type, ret);
}

void type1_local(LocalRoot local, LispDecl type, addr a, addr *ret)
{
	CheckLocal(local);
	type1_alloc(local, type, a, ret);
}

void type2_local(LocalRoot local, LispDecl type, addr a, addr b, addr *ret)
{
	CheckLocal(local);
	type2_alloc(local, type, a, b, ret);
}

void type3_local(LocalRoot local, LispDecl type, addr a, addr b, addr c, addr *ret)
{
	CheckLocal(local);
	type3_alloc(local, type, a, b, c, ret);
}

void type4_local(LocalRoot local, LispDecl type,
		addr a, addr b, addr c, addr d, addr *ret)
{
	CheckLocal(local);
	type4_alloc(local, type, a, b, c, d, ret);
}

void type0_heap(LispDecl type, addr *ret)
{
	type0_alloc(NULL, type, ret);
}

void type1_heap(LispDecl type, addr a, addr *ret)
{
	type1_alloc(NULL, type, a, ret);
}

void type2_heap(LispDecl type, addr a, addr b, addr *ret)
{
	type2_alloc(NULL, type, a, b, ret);
}

void type3_heap(LispDecl type, addr a, addr b, addr c, addr *ret)
{
	type3_alloc(NULL, type, a, b, c, ret);
}

void type4_heap(LispDecl type, addr a, addr b, addr c, addr d, addr *ret)
{
	type4_alloc(NULL, type, a, b, c, d, ret);
}

void type0not_alloc(LocalRoot local, LispDecl type, addr *ret)
{
	type0_alloc(local, type, ret);
	SetNotDecl(*ret, 1);
}

void type1not_alloc(LocalRoot local, LispDecl type, addr a, addr *ret)
{
	type1_alloc(local, type, a, ret);
	SetNotDecl(*ret, 1);
}

void type2not_alloc(LocalRoot local, LispDecl type, addr a, addr b, addr *ret)
{
	type2_alloc(local, type, a, b, ret);
	SetNotDecl(*ret, 1);
}

void type3not_alloc(LocalRoot local, LispDecl type,
		addr a, addr b, addr c, addr *ret)
{
	type3_alloc(local, type, a, b, c, ret);
	SetNotDecl(*ret, 1);
}

void type4not_alloc(LocalRoot local, LispDecl type,
		addr a, addr b, addr c, addr d, addr *ret)
{
	type4_alloc(local, type, a, b, c, d, ret);
	SetNotDecl(*ret, 1);
}

void type0not_local(LocalRoot local, LispDecl type, addr *ret)
{
	CheckLocal(local);
	type0not_alloc(local, type, ret);
}

void type1not_local(LocalRoot local, LispDecl type, addr a, addr *ret)
{
	CheckLocal(local);
	type1not_alloc(local, type, a, ret);
}

void type2not_local(LocalRoot local, LispDecl type, addr a, addr b, addr *ret)
{
	CheckLocal(local);
	type2not_alloc(local, type, a, b, ret);
}

void type3not_local(LocalRoot local, LispDecl type, addr a, addr b, addr c, addr *ret)
{
	CheckLocal(local);
	type3not_alloc(local, type, a, b, c, ret);
}

void type4not_local(LocalRoot local, LispDecl type,
		addr a, addr b, addr c, addr d, addr *ret)
{
	CheckLocal(local);
	type4not_alloc(local, type, a, b, c, d, ret);
}

void type0not_heap(LispDecl type, addr *ret)
{
	type0not_alloc(NULL, type, ret);
}

void type1not_heap(LispDecl type, addr a, addr *ret)
{
	type1not_alloc(NULL, type, a, ret);
}

void type2not_heap(LispDecl type, addr a, addr b, addr *ret)
{
	type2not_alloc(NULL, type, a, b, ret);
}

void type3not_heap(LispDecl type, addr a, addr b, addr c, addr *ret)
{
	type3not_alloc(NULL, type, a, b, c, ret);
}

void type4not_heap(LispDecl type, addr a, addr b, addr c, addr d, addr *ret)
{
	type4not_alloc(NULL, type, a, b, c, d, ret);
}

static void type_aster_local(LocalRoot local, addr *ret)
{
	CheckLocal(local);
	type0_local(local, LISPDECL_ASTERISK, ret);
}

void type1aster_localall(LocalRoot local, LispDecl type, addr *ret)
{
	addr a1;

	CheckLocal(local);
	type_aster_local(local, &a1);
	type1_local(local, type, a1, ret);
}

void type2aster_localall(LocalRoot local, LispDecl type, addr *ret)
{
	addr a1, a2;

	CheckLocal(local);
	type_aster_local(local, &a1);
	type_aster_local(local, &a2);
	type2_local(local, type, a1, a2, ret);
}

void type3aster_localall(LocalRoot local, LispDecl type, addr *ret)
{
	addr a1, a2, a3;

	CheckLocal(local);
	type_aster_local(local, &a1);
	type_aster_local(local, &a2);
	type_aster_local(local, &a3);
	type3_local(local, type, a1, a2, a3, ret);
}

void type4aster_localall(LocalRoot local, LispDecl type, addr *ret)
{
	addr a1, a2, a3, a4;

	CheckLocal(local);
	type_aster_local(local, &a1);
	type_aster_local(local, &a2);
	type_aster_local(local, &a3);
	type_aster_local(local, &a4);
	type4_local(local, type, a1, a2, a3, a4, ret);
}


/*
 *  copy
 */
static void type_copy_unsafe(LocalRoot local, addr *ret, addr left, byte value)
{
	addr right, one;
	size_t size, i;

	Check(GetType(left) != LISPTYPE_TYPE, "type error");
	LenArrayType(left, &size);
	type_alloc(local, &right, LISPDECL_EMPTY, (byte)size);
	SetUser(right, value);
	for (i = 0; i < size; i++) {
		GetArrayType(left, i, &one);
		SetArrayType(right, i, one);
	}
	*ret = right;
}

void type_copy_unsafe_alloc(LocalRoot local, addr *ret, addr left)
{
	type_copy_unsafe(local, ret, left, GetUser(left));
}

void type_copy_unsafe_local(LocalRoot local, addr *ret, addr left)
{
	CheckLocal(local);
	type_copy_unsafe_alloc(local, ret, left);
}

void type_copy_unsafe_heap(addr *ret, addr left)
{
	type_copy_unsafe_alloc(NULL, ret, left);
}

void type_copydecl_unsafe_alloc(LocalRoot local, addr *ret, addr left)
{
	type_copy_unsafe(local, ret, left, RefLispDecl(left));
}

void type_copydecl_unsafe_local(LocalRoot local, addr *ret, addr left)
{
	CheckLocal(local);
	type_copydecl_unsafe_alloc(local, ret, left);
}

void type_copydecl_unsafe_heap(addr *ret, addr left)
{
	type_copydecl_unsafe_alloc(NULL, ret, left);
}


/*
 *  make
 */
void type_eql_alloc(LocalRoot local, addr pos, addr *ret)
{
	type1_alloc(local, LISPDECL_EQL, pos, ret);
}

void type_eql_local(LocalRoot local, addr pos, addr *ret)
{
	type1_local(local, LISPDECL_EQL, pos, ret);
}

void type_eql_heap(addr pos, addr *ret)
{
	type1_heap(LISPDECL_EQL, pos, ret);
}

static void vector4_va_heap(addr *ret, va_list args)
{
	va_list temp;
	addr pos, array;
	size_t size, i;

	/* length args */
	va_copy(temp, args);
	for (size = 0; ; size++) {
		pos = va_arg(temp, addr);
		if (pos == NULL) break;
	}

	/* make vector4 */
	vector4_heap(&array, size);
	for (i = 0; i < size; i++) {
		pos = va_arg(args, addr);
		if (pos == NULL) break;
		SetArrayA4(array, i, pos);
	}
	*ret = array;
}

void type_member_heap(addr *ret, ...)
{
	addr array;
	va_list args;

	va_start(args, ret);
	vector4_va_heap(&array, args);
	va_end(args);
	type1_heap(LISPDECL_MEMBER, array, ret);
}

void type_satisfies_heap(addr call, addr *ret)
{
	Check((! symbolp(call)) && (! functionp(call)), "type error");
	type1_heap(LISPDECL_SATISFIES, call, ret);
}

void type_values_local(LocalRoot local, addr v1, addr v2, addr v3, addr v4, addr *ret)
{
	type4_local(local, LISPDECL_VALUES, v1, v2, v3, v4, ret);
}

void type_values_heap(addr v1, addr v2, addr v3, addr v4, addr *ret)
{
	type4_heap(LISPDECL_VALUES, v1, v2, v3, v4, ret);
}

void type_signed_alloc(LocalRoot local, fixnum value, addr *ret)
{
	addr pos;

	Check(value <= 0, "size error");
	fixnum_alloc(local, &pos, value);
	type1_alloc(local, LISPDECL_SIGNED_BYTE, pos, ret);
}

void type_signed_local(LocalRoot local, fixnum value, addr *ret)
{
	CheckLocal(local);
	type_signed_alloc(local, value, ret);
}

void type_signed_heap(fixnum value, addr *ret)
{
	type_signed_alloc(NULL, value, ret);
}

void type_unsigned_alloc(LocalRoot local, fixnum value, addr *ret)
{
	addr pos;

	Check(value <= 0, "size error");
	fixnum_alloc(local, &pos, value);
	type1_alloc(local, LISPDECL_UNSIGNED_BYTE, pos, ret);
}

void type_unsigned_local(LocalRoot local, fixnum value, addr *ret)
{
	CheckLocal(local);
	type_unsigned_alloc(local, value, ret);
}

void type_unsigned_heap(fixnum value, addr *ret)
{
	type_unsigned_alloc(NULL, value, ret);
}

/*
 *  function / compiled-function
 *    function         -> (function * * [*])
 *    (function)       -> (function * * [nil])
 *    (function * *)   -> (function * * [nil])
 *    (function * * *) -> ERROR
 *
 *  (typep value 'function) -> ok
 *  (typep value '(function)) -> ERROR
 *  (declare (type function a b)) -> ok
 *  (declare (type (function) a b)) -> ok
 */
void type_function_heap(addr args, addr values, addr *ret)
{
	type3_heap(LISPDECL_FUNCTION, args, values, Nil, ret);
}

void type_compiled_heap(addr args, addr values, addr *ret)
{
	type3_heap(LISPDECL_COMPILED_FUNCTION, args, values, Nil, ret);
}

void type_clos_heap(addr clos, addr *ret)
{
	CheckType(clos, LISPTYPE_CLOS);
	type1_heap(LISPDECL_CLOS, clos, ret);
}

void type_error_heap(addr pos, addr *ret)
{
	type2_heap(LISPDECL_ERROR, pos, Nil, ret);
}


/************************************************************
 *  type_name.c
 ************************************************************/

static constindex TypeNameTable[LISPTYPE_SIZE];
#define DefTypeName(x,y) (TypeNameTable[x] = CONSTANT_##y)

static int type_name_clos_(addr pos, addr *value, int *ret)
{
	Return(clos_class_of_(pos, &pos));
	Return(stdget_class_name_(pos, value));
	return Result(ret, 0);
}

static int type_name_symbol_(addr pos, addr *value, int *ret)
{
	if (keywordp(pos))
		GetConst(COMMON_KEYWORD, value);
	else
		GetConst(COMMON_SYMBOL, value);

	return Result(ret, 0);
}

static int type_name_function_(addr pos, addr *value, int *ret)
{
	if (interpreted_funcall_function_p(pos)) {
		GetConst(COMMON_FUNCTION, value);
		goto normal;
	}
	if (interpreted_macro_function_p(pos)) {
		GetConst(COMMON_MACRO_FUNCTION, value);
		goto normal;
	}
	if (compiled_funcall_function_p(pos)) {
		GetConst(COMMON_COMPILED_FUNCTION, value);
		goto normal;
	}
	if (compiled_macro_function_p(pos)) {
		GetConst(SYSTEM_COMPILED_MACRO_FUNCTION, value);
		goto normal;
	}
	else {
		GetConst(COMMON_FUNCTION, value);
		goto normal;
	}

normal:
	return Result(ret, 0);
}

static int type_name_stream_(addr pos, addr *value, int *ret)
{
	switch (getstreamtype(pos)) {
		case StreamType_StringInput:
		case StreamType_StringOutput:
			GetConst(COMMON_STRING_STREAM, value);
			goto normal;

		case StreamType_Synonym:
			GetConst(COMMON_SYNONYM_STREAM, value);
			goto normal;

		case StreamType_BroadCast:
			GetConst(COMMON_BROADCAST_STREAM, value);
			goto normal;

		case StreamType_Concatenated:
			GetConst(COMMON_CONCATENATED_STREAM, value);
			goto normal;

		case StreamType_Echo:
			GetConst(COMMON_ECHO_STREAM, value);
			goto normal;

		case StreamType_TwoWay:
			GetConst(COMMON_TWO_WAY_STREAM, value);
			goto normal;

		case StreamType_Prompt:
			GetConst(SYSTEM_PROMPT_STREAM, value);
			goto normal;

		case StreamType_Pretty:
			GetConst(SYSTEM_PRETTY_STREAM, value);
			goto normal;

		case StreamType_MemoryInput:
		case StreamType_MemoryOutput:
		case StreamType_MemoryIO:
			GetConst(SYSTEM_MEMORY_STREAM, value);
			goto normal;

		default:
			break;
	}

	GetPathnameStream(pos, &pos);
	if (pos != Nil) {
		GetConst(COMMON_FILE_STREAM, value);
		goto normal;
	}

	GetConst(COMMON_STREAM, value);
	goto normal;

normal:
	return Result(ret, 0);
}

int type_name_p_(addr pos, addr *value, int *ret)
{
	enum LISPTYPE type;
	constindex index;

	/* table */
	type = GetType(pos);
	index = TypeNameTable[(int)type];
	if (index != CONSTANT_EMPTY) {
		GetConstant(index, value);
		return Result(ret, 0);
	}

	/* others */
	switch (type) {
		case LISPTYPE_CLOS:
			return type_name_clos_(pos, value, ret);

		case LISPTYPE_SYMBOL:
			return type_name_symbol_(pos, value, ret);

		case LISPTYPE_FUNCTION:
			return type_name_function_(pos, value, ret);

		case LISPTYPE_STREAM:
			return type_name_stream_(pos, value, ret);

		default:
			*value = Nil;
			return Result(ret, 1);
	}
}

int type_name_(addr pos, addr *value)
{
	int check;

	Return(type_name_p_(pos, value, &check));
	if (check) {
		*value = Nil;
		return fmte_("The type ~S don't have a name.", pos, NULL);
	}

	return 0;
}

void init_type_name(void)
{
	int i;

	for (i = 0; i < LISPTYPE_SIZE; i++)
		TypeNameTable[i] = CONSTANT_EMPTY;

	DefTypeName(LISPTYPE_NIL, COMMON_NIL);
	DefTypeName(LISPTYPE_T, COMMON_T);
	DefTypeName(LISPTYPE_TYPE, COMMON_TYPE);
	/*DefTypeName(LISPTYPE_CLOS, COMMON_EMPTY);*/
	DefTypeName(LISPTYPE_CONS, COMMON_CONS);
	DefTypeName(LISPTYPE_ARRAY, COMMON_ARRAY);
	DefTypeName(LISPTYPE_VECTOR, COMMON_VECTOR);
	DefTypeName(LISPTYPE_CHARACTER, COMMON_CHARACTER);
	DefTypeName(LISPTYPE_STRING, COMMON_STRING);
	DefTypeName(LISPTYPE_HASHTABLE, COMMON_HASH_TABLE);
	DefTypeName(LISPTYPE_READTABLE, COMMON_READTABLE);
	/*DefTypeName(LISPTYPE_SYMBOL, COMMON_EMPTY);*/
	DefTypeName(LISPTYPE_FIXNUM, COMMON_INTEGER);
	DefTypeName(LISPTYPE_BIGNUM, COMMON_INTEGER);
	DefTypeName(LISPTYPE_RATIO, COMMON_RATIO);
	DefTypeName(LISPTYPE_SHORT_FLOAT, COMMON_FLOAT);
	DefTypeName(LISPTYPE_SINGLE_FLOAT, COMMON_FLOAT);
	DefTypeName(LISPTYPE_DOUBLE_FLOAT, COMMON_FLOAT);
	DefTypeName(LISPTYPE_LONG_FLOAT, COMMON_FLOAT);
	DefTypeName(LISPTYPE_COMPLEX, COMMON_COMPLEX);
	DefTypeName(LISPTYPE_CONTROL, SYSTEM_CONTROL);
	DefTypeName(LISPTYPE_CODE, SYSTEM_CODE);
	DefTypeName(LISPTYPE_CALLNAME, SYSTEM_CALLNAME);
	/*DefTypeName(LISPTYPE_FUNCTION, COMMON_EMPTY);*/
	DefTypeName(LISPTYPE_INDEX, SYSTEM_INDEX);
	DefTypeName(LISPTYPE_PACKAGE, COMMON_PACKAGE);
	DefTypeName(LISPTYPE_RANDOM_STATE, COMMON_RANDOM_STATE);
	DefTypeName(LISPTYPE_PATHNAME, COMMON_PATHNAME);
	/*DefTypeName(LISPTYPE_STREAM, COMMON_EMPTY);*/
	DefTypeName(LISPTYPE_QUOTE, SYSTEM_QUOTE);
	DefTypeName(LISPTYPE_RESTART, COMMON_RESTART);
	DefTypeName(LISPTYPE_EVAL, SYSTEM_EVAL);
	DefTypeName(LISPTYPE_ENVIRONMENT, SYSTEM_ENVIRONMENT);
	DefTypeName(LISPTYPE_BITVECTOR, COMMON_BIT_VECTOR);
	DefTypeName(LISPTYPE_BYTESPEC, SYSTEM_BYTESPEC);
	DefTypeName(LISPTYPE_PRINT_DISPATCH, SYSTEM_PRINT_DISPATCH);
	DefTypeName(LISPTYPE_EVAL, SYSTEM_EVAL);

	DefTypeName(LISPSYSTEM_CHARACTER2, SYSTEM_CHARACTER2);
	DefTypeName(LISPSYSTEM_CHARQUEUE, SYSTEM_CHARQUEUE);
	DefTypeName(LISPSYSTEM_CHARBIT, SYSTEM_CHARBIT);
	DefTypeName(LISPSYSTEM_SYMSTACK, SYSTEM_SYMSTACK);
	DefTypeName(LISPSYSTEM_BITTYPE, SYSTEM_BITTYPE);
	DefTypeName(LISPSYSTEM_READLABEL, SYSTEM_READLABEL);
	DefTypeName(LISPSYSTEM_READINFO, SYSTEM_READINFO_SYMBOL);
	DefTypeName(LISPSYSTEM_READTYPE, SYSTEM_READTYPE);
	DefTypeName(LISPSYSTEM_BITCONS, SYSTEM_BITCONS);
	DefTypeName(LISPSYSTEM_BITBUFFER, SYSTEM_BITBUFFER);
	DefTypeName(LISPSYSTEM_HASHITERATOR, SYSTEM_HASHITERATOR);
	DefTypeName(LISPSYSTEM_PACKAGEITERATOR, SYSTEM_PACKAGEITERATOR);
	DefTypeName(LISPSYSTEM_TAGINFO, SYSTEM_TAGINFO);
	DefTypeName(LISPSYSTEM_ARRAY_DIMENSION, SYSTEM_ARRAY_DIMENSION);
	DefTypeName(LISPSYSTEM_ARRAY_GENERAL, SYSTEM_ARRAY_GENERAL);
	DefTypeName(LISPSYSTEM_ARRAY_SPECIALIZED, SYSTEM_ARRAY_SPECIALIZED);

	DefTypeName(LISPSYSTEM_UNBOUND, SYSTEM_UNBOUND);
	DefTypeName(LISPSYSTEM_SPACE, SYSTEM_SPACE);
	DefTypeName(LISPSYSTEM_SPACE1, SYSTEM_SPACE1);
	DefTypeName(LISPSYSTEM_RESERVED, SYSTEM_RESERVED);
	DefTypeName(LISPSYSTEM_END, SYSTEM_END);
}


/************************************************************
 *  type_object.c
 ************************************************************/

typedef int (*type_object_call)(addr *, addr);
static type_object_call TypeObjectTable[LISPTYPE_SIZE];

static int type_object_error(addr *ret, addr pos)
{
	infobit(pos);
	return fmte_("Invalid type.", NULL);
}

static int type_object_error_type(addr *ret, addr pos)
{
	addr type;

	GetArrayType(pos, 1, &type);
	if (type != Nil)
		return type_object_(ret, type);

	GetArrayType(pos, 0, &type);
	return Result(ret, type);
}

static int type_object_name(addr *ret, addr pos)
{
	constindex index = getdeclname(RefLispDecl(pos));
	GetConstant(index, ret);
	return 0;
}

static int type_object_optimized(addr *ret, addr pos)
{
	get_type_optimized(&pos, pos);
	return type_object_(ret, pos);
}

static int type_object_subtypep(addr *ret, addr pos)
{
	get_type_subtypep(&pos, pos);
	return type_object_(ret, pos);
}

static int type_object_type(addr *ret, addr pos)
{
	GetConst(SYSTEM_TYPE, ret);
	return 0;
}

static int type_object_clos(addr *ret, addr pos)
{
	GetArrayType(pos, 0, &pos);
	if (type_asterisk_p(pos)) {
		GetConst(COMMON_STANDARD_OBJECT, ret);
	}
	else {
		Return(stdget_class_name_(pos, ret));
	}

	return 0;
}

static int type_object_vectortype_(addr *ret, addr name, addr pos)
{
	addr array, root, temp;
	size_t size;

	GetArrayType(pos, 0, &array);
	LenArrayA4(array, &size);
	for (root = Nil; size; ) {
		size--;
		GetArrayA4(array, size, &temp);
		Return(type_object_(&temp, temp));
		cons_heap(&root, temp, root);
	}
	cons_heap(ret, name, root);

	return 0;
}

static int type_object_and(addr *ret, addr pos)
{
	addr name;
	GetConst(COMMON_AND, &name);
	return type_object_vectortype_(ret, name, pos);
}

static int type_object_or(addr *ret, addr pos)
{
	addr name;
	GetConst(COMMON_OR, &name);
	return type_object_vectortype_(ret, name, pos);
}

static int type_object_operator1_(addr *ret, constindex index, addr pos)
{
	addr name;

	GetConstant(index, &name);
	GetArrayType(pos, 0, &pos);
	copyheap(&pos, pos);
	list_heap(ret, name, pos, NULL);

	return 0;
}

static int type_object_eql(addr *ret, addr pos)
{
	return type_object_operator1_(ret, CONSTANT_COMMON_EQL, pos);
}

static int type_object_member(addr *ret, addr pos)
{
	addr array, root, temp;
	size_t size;

	GetArrayType(pos, 0, &array);
	LenArrayA4(array, &size);
	for (root = Nil; size; ) {
		size--;
		GetArrayA4(array, size, &temp);
		copyheap(&temp, temp);
		cons_heap(&root, temp, root);
	}
	GetConst(COMMON_MEMBER, &temp);
	cons_heap(ret, temp, root);

	return 0;
}

static int type_object_mod(addr *ret, addr pos)
{
	return type_object_operator1_(ret, CONSTANT_COMMON_MOD, pos);
}

static int type_object_not(addr *ret, addr pos)
{
	addr name;

	GetConst(COMMON_NOT, &name);
	GetArrayType(pos, 0, &pos);
	Return(type_object_(&pos, pos));
	list_heap(ret, name, pos, NULL);

	return 0;
}

static int type_object_satisfies(addr *ret, addr pos)
{
	return type_object_operator1_(ret, CONSTANT_COMMON_SATISFIES, pos);
}

static int type_object_values(addr *ret, addr pos)
{
	addr root, list, value;

	/* first */
	GetConst(COMMON_VALUES, &value);
	conscar_heap(&root, value);

	/* variable */
	GetArrayType(pos, 0, &list);
	while (list != Nil) {
		GetCons(list, &value, &list);
		Return(type_object_(&value, value));
		cons_heap(&root, value, root);
	}

	/* &optional */
	GetArrayType(pos, 1, &list);
	if (list != Nil) {
		GetConst(AMPERSAND_OPTIONAL, &value);
		cons_heap(&root, value, root);
	}
	while (list != Nil) {
		GetCons(list, &value, &list);
		Return(type_object_(&value, value));
		cons_heap(&root, value, root);
	}

	/* &rest */
	GetArrayType(pos, 2, &list);
	if (list != Nil) {
		GetConst(AMPERSAND_REST, &value);
		cons_heap(&root, value, root);
		Return(type_object_(&list, list));
		cons_heap(&root, list, root);
	}

	/* &allow-other-keys (always nil) */
	GetArrayType(pos, 3, &list);
	if (list != Nil) {
		GetConst(AMPERSAND_ALLOW, &value);
		cons_heap(&root, value, root);
	}

	/* result */
	nreverse(ret, root);
	return 0;
}

static int type_object_vector(addr *ret, addr pos)
{
	addr name, type;

	GetConst(COMMON_VECTOR, &name);
	GetArrayType(pos, 0, &type);
	GetArrayType(pos, 1, &pos);
	if (type_asterisk_p(type) && type_asterisk_p(pos))
		return Result(ret, name);
	Return(type_object_(&type, type));
	if (type_asterisk_p(pos))
		GetConst(COMMON_ASTERISK, &pos);
	list_heap(ret, name, type, pos, NULL);

	return 0;
}

static int type_object_size_(addr *ret, constindex index, addr pos)
{
	addr name;

	GetConstant(index, &name);
	GetArrayType(pos, 0, &pos);
	if (type_asterisk_p(pos))
		return Result(ret, name);
	list_heap(ret, name, pos, NULL);

	return 0;
}

static int type_object_simple_vector(addr *ret, addr pos)
{
	return type_object_size_(ret, CONSTANT_COMMON_SIMPLE_VECTOR, pos);
}

static int type_object_bit_vector(addr *ret, addr pos)
{
	return type_object_size_(ret, CONSTANT_COMMON_BIT_VECTOR, pos);
}

static int type_object_simple_bit_vector(addr *ret, addr pos)
{
	return type_object_size_(ret, CONSTANT_COMMON_SIMPLE_BIT_VECTOR, pos);
}

static int type_object_string(addr *ret, addr pos)
{
	return type_object_size_(ret, CONSTANT_COMMON_STRING, pos);
}

static int type_object_base_string(addr *ret, addr pos)
{
	return type_object_size_(ret, CONSTANT_COMMON_BASE_STRING, pos);
}

static int type_object_simple_string(addr *ret, addr pos)
{
	return type_object_size_(ret, CONSTANT_COMMON_SIMPLE_STRING, pos);
}

static int type_object_simple_base_string(addr *ret, addr pos)
{
	return type_object_size_(ret, CONSTANT_COMMON_SIMPLE_BASE_STRING, pos);
}

static int type_object_signed_byte(addr *ret, addr pos)
{
	return type_object_size_(ret, CONSTANT_COMMON_SIGNED_BYTE, pos);
}

static int type_object_unsigned_byte(addr *ret, addr pos)
{
	return type_object_size_(ret, CONSTANT_COMMON_UNSIGNED_BYTE, pos);
}

static int type_object_cons(addr *ret, addr pos)
{
	addr name, car, cdr;

	GetConst(COMMON_CONS, &name);
	GetArrayType(pos, 0, &car);
	GetArrayType(pos, 1, &cdr);
	if (type_asterisk_p(car) && type_asterisk_p(cdr))
		return Result(ret, name);
	Return(type_object_(&car, car));
	Return(type_object_(&cdr, cdr));
	list_heap(ret, name, car, cdr, NULL);

	return 0;
}

static int type_object_function_args_(addr *ret, addr type)
{
	addr root, list, pos, value;

	CheckType(type, LISPTYPE_VECTOR);
	root = Nil;
	/* var */
	GetArrayA2(type, 0, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(type_object_(&pos, pos));
		cons_heap(&root, pos, root);
	}

	/* opt */
	GetArrayA2(type, 1, &list);
	if (list != Nil) {
		GetConst(AMPERSAND_OPTIONAL, &pos);
		cons_heap(&root, pos, root);
		while (list != Nil) {
			GetCons(list, &pos, &list);
			Return(type_object_(&pos, pos));
			cons_heap(&root, pos, root);
		}
	}

	/* rest */
	GetArrayA2(type, 2, &list);
	if (list != Nil) {
		GetConst(AMPERSAND_REST, &pos);
		cons_heap(&root, pos, root);
		Return(type_object_(&list, list));
		cons_heap(&root, list, root);
	}

	/* key */
	GetArrayA2(type, 3, &list);
	if (list != Nil) {
		GetConst(AMPERSAND_KEY, &pos);
		cons_heap(&root, pos, root);
		while (list != Nil) {
			GetCons(list, &pos, &list);
			GetCons(pos, &pos, &value);
			Return(type_object_(&value, value));
			list_heap(&pos, pos, value, NULL);
			cons_heap(&root, pos, root);
		}
	}

	/* result */
	nreverse(ret, root);
	return 0;
}

static int type_object_functiontype_(addr *ret, constindex index, addr pos)
{
	addr name, type;

	/* name */
	GetConstant(index, &name);
	/* argument */
	GetArrayType(pos, 0, &type);
	GetArrayType(pos, 1, &pos);
	if (type_asterisk_p(type) && type_asterisk_p(pos))
		return Result(ret, name);
	if (type_asterisk_p(type)) {
		GetConst(COMMON_ASTERISK, &type);
	}
	else {
		Return(type_object_function_args_(&type, type));
	}
	/* values */
	if (type_asterisk_p(pos)) {
		GetConst(COMMON_ASTERISK, &pos);
	}
	else {
		Return(type_object_(&pos, pos));
	}
	/* result */
	list_heap(ret, name, type, pos, NULL);

	return 0;
}

static int type_object_function(addr *ret, addr pos)
{
	return type_object_functiontype_(ret, CONSTANT_COMMON_FUNCTION, pos);
}

static int type_object_compiled_function(addr *ret, addr pos)
{
	return type_object_functiontype_(ret, CONSTANT_COMMON_COMPILED_FUNCTION, pos);
}

static void type_object_arraydimension(addr pos, addr *ret)
{
	addr root, temp;
	size_t size;

	if (GetType(pos) == LISPTYPE_FIXNUM) {
		copyheap(ret, pos);
		return;
	}
	Check(GetType(pos) != LISPTYPE_VECTOR, "type error");
	LenArrayA4(pos, &size);
	for (root = Nil; size; ) {
		size--;
		GetArrayA4(pos, size, &temp);
		copyheap(&temp, temp);
		cons_heap(&root, temp, root);
	}
	*ret = root;
}

static int type_object_arraytype_(addr *ret, constindex index, addr pos)
{
	addr name, type;

	/* name */
	GetConstant(index, &name);
	/* type */
	GetArrayType(pos, 0, &type);
	GetArrayType(pos, 1, &pos);
	if (type_asterisk_p(type) && type_asterisk_p(pos))
		return Result(ret, name);
	if (type_asterisk_p(type)) {
		GetConst(COMMON_ASTERISK, &type);
	}
	else {
		Return(type_object_(&type, type));
	}
	/* dimension */
	if (type_asterisk_p(pos)) {
		GetConst(COMMON_ASTERISK, &pos);
	}
	else {
		type_object_arraydimension(pos, &pos);
	}
	/* result */
	list_heap(ret, name, type, pos, NULL);

	return 0;
}

static int type_object_array(addr *ret, addr pos)
{
	return type_object_arraytype_(ret, CONSTANT_COMMON_ARRAY, pos);
}

static int type_object_simple_array(addr *ret, addr pos)
{
	return type_object_arraytype_(ret, CONSTANT_COMMON_SIMPLE_ARRAY, pos);
}

static void type_object_realtype(addr *ret, constindex index, addr pos)
{
	addr name, left1, left2, right1, right2;

	/* name */
	GetConstant(index, &name);
	/* argument */
	GetArrayType(pos, 0, &left1);
	GetArrayType(pos, 1, &left2);
	GetArrayType(pos, 2, &right1);
	GetArrayType(pos, 3, &right2);
	if (type_asterisk_p(left1) && type_asterisk_p(right1)) {
		*ret = name;
		return;
	}
	/* left */
	if (type_asterisk_p(left1))
		GetConst(COMMON_ASTERISK, &left2);
	else if (left1 == T)
		conscar_heap(&left2, left2);
	/* right */
	if (type_asterisk_p(right1))
		GetConst(COMMON_ASTERISK, &right2);
	else if (right1 == T)
		conscar_heap(&right2, right2);
	/* result */
	list_heap(ret, name, left2, right2, NULL);
}

static int type_object_real(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_REAL, pos);
	return 0;
}

static int type_object_rational(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_RATIONAL, pos);
	return 0;
}

static int type_object_integer(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_INTEGER, pos);
	return 0;
}

static int type_object_float(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_FLOAT, pos);
	return 0;
}

static int type_object_short_float(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_SHORT_FLOAT, pos);
	return 0;
}

static int type_object_single_float(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_SINGLE_FLOAT, pos);
	return 0;
}

static int type_object_double_float(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_DOUBLE_FLOAT, pos);
	return 0;
}

static int type_object_long_float(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_LONG_FLOAT, pos);
	return 0;
}

static int type_object_complex(addr *ret, addr pos)
{
	addr name;

	GetConst(COMMON_COMPLEX, &name);
	GetArrayType(pos, 0, &pos);
	if (type_asterisk_p(pos))
		return Result(ret, name);
	Return(type_object_(&pos, pos));
	list_heap(ret, name, pos, NULL);

	return 0;
}

int type_object_(addr *ret, addr pos)
{
	type_object_call call;
	addr result, notp;

	Check(GetType(pos) != LISPTYPE_TYPE, "type error");
	call = TypeObjectTable[(int)RefLispDecl(pos)];
	Return((*call)(&result, pos));
	if (RefNotDecl(pos)) {
		GetConst(COMMON_NOT, &notp);
		list_heap(ret, notp, result, NULL);
	}
	else {
		*ret = result;
	}
	Check(*ret == Unbound, "unbound error");

	return 0;
}

void init_type_object(void)
{
	int i;

	for (i = 0; i < LISPDECL_SIZE; i++)
		TypeObjectTable[i] = type_object_name;

	/* object */
	TypeObjectTable[LISPDECL_EMPTY] = type_object_error;
	TypeObjectTable[LISPDECL_ERROR] = type_object_error_type;
	TypeObjectTable[LISPDECL_INVALID] = type_object_name;
	TypeObjectTable[LISPDECL_OPTIMIZED] = type_object_optimized;
	TypeObjectTable[LISPDECL_SUBTYPEP] = type_object_subtypep;
	TypeObjectTable[LISPDECL_TYPE] = type_object_type;
	TypeObjectTable[LISPDECL_CLOS] = type_object_clos;
	TypeObjectTable[LISPDECL_ASTERISK] = type_object_name;
	/* Compound-type */
	TypeObjectTable[LISPDECL_AND] = type_object_and;
	TypeObjectTable[LISPDECL_OR] = type_object_or;
	TypeObjectTable[LISPDECL_EQL] = type_object_eql;
	TypeObjectTable[LISPDECL_MEMBER] = type_object_member;
	TypeObjectTable[LISPDECL_MOD] = type_object_mod;
	TypeObjectTable[LISPDECL_NOT] = type_object_not;
	TypeObjectTable[LISPDECL_SATISFIES] = type_object_satisfies;
	TypeObjectTable[LISPDECL_VALUES] = type_object_values;
	/* Extract-type */
	TypeObjectTable[LISPDECL_ATOM] = type_object_name;
	TypeObjectTable[LISPDECL_LIST] = type_object_name;
	TypeObjectTable[LISPDECL_BOOLEAN] = type_object_name;
	TypeObjectTable[LISPDECL_VECTOR] = type_object_vector;
	TypeObjectTable[LISPDECL_SIMPLE_VECTOR] = type_object_simple_vector;
	TypeObjectTable[LISPDECL_BIT_VECTOR] = type_object_bit_vector;
	TypeObjectTable[LISPDECL_SIMPLE_BIT_VECTOR] = type_object_simple_bit_vector;
	TypeObjectTable[LISPDECL_EXTENDED_CHAR] = type_object_name;
	TypeObjectTable[LISPDECL_STRING] = type_object_string;
	TypeObjectTable[LISPDECL_BASE_STRING] = type_object_base_string;
	TypeObjectTable[LISPDECL_SIMPLE_STRING] = type_object_simple_string;
	TypeObjectTable[LISPDECL_SIMPLE_BASE_STRING] = type_object_simple_base_string;
	TypeObjectTable[LISPDECL_SIGNED_BYTE] = type_object_signed_byte;
	TypeObjectTable[LISPDECL_UNSIGNED_BYTE] = type_object_unsigned_byte;
	TypeObjectTable[LISPDECL_BIT] = type_object_name;
	TypeObjectTable[LISPDECL_FIXNUM] = type_object_name;
	TypeObjectTable[LISPDECL_BIGNUM] = type_object_name;
	/* Atomic-type */
	TypeObjectTable[LISPDECL_NIL] = type_object_name;
	TypeObjectTable[LISPDECL_T] = type_object_name;
	TypeObjectTable[LISPDECL_NULL] = type_object_name;
	TypeObjectTable[LISPDECL_CONS] = type_object_cons;
	TypeObjectTable[LISPDECL_HASH_TABLE] = type_object_name;
	TypeObjectTable[LISPDECL_SYMBOL] = type_object_name;
	TypeObjectTable[LISPDECL_KEYWORD] = type_object_name;
	TypeObjectTable[LISPDECL_PACKAGE] = type_object_name;
	TypeObjectTable[LISPDECL_RANDOM_STATE] = type_object_name;
	TypeObjectTable[LISPDECL_READTABLE] = type_object_name;
	TypeObjectTable[LISPDECL_FUNCTION] = type_object_function;
	TypeObjectTable[LISPDECL_COMPILED_FUNCTION] = type_object_compiled_function;
	TypeObjectTable[LISPDECL_PATHNAME] = type_object_name;
	TypeObjectTable[LISPDECL_LOGICAL_PATHNAME] = type_object_name;
	TypeObjectTable[LISPDECL_SEQUENCE] = type_object_name;
	TypeObjectTable[LISPDECL_ARRAY] = type_object_array;
	TypeObjectTable[LISPDECL_SIMPLE_ARRAY] = type_object_simple_array;
	TypeObjectTable[LISPDECL_CHARACTER] = type_object_name;
	TypeObjectTable[LISPDECL_BASE_CHAR] = type_object_name;
	TypeObjectTable[LISPDECL_STANDARD_CHAR] = type_object_name;
	TypeObjectTable[LISPDECL_NUMBER] = type_object_name;
	TypeObjectTable[LISPDECL_REAL] = type_object_real;
	TypeObjectTable[LISPDECL_RATIONAL] = type_object_rational;
	TypeObjectTable[LISPDECL_RATIO] = type_object_name;
	TypeObjectTable[LISPDECL_INTEGER] = type_object_integer;
	TypeObjectTable[LISPDECL_COMPLEX] = type_object_complex;
	TypeObjectTable[LISPDECL_FLOAT] = type_object_float;
	TypeObjectTable[LISPDECL_SHORT_FLOAT] = type_object_short_float;
	TypeObjectTable[LISPDECL_SINGLE_FLOAT] = type_object_single_float;
	TypeObjectTable[LISPDECL_DOUBLE_FLOAT] = type_object_double_float;
	TypeObjectTable[LISPDECL_LONG_FLOAT] = type_object_long_float;
	TypeObjectTable[LISPDECL_RESTART] = type_object_name;
	TypeObjectTable[LISPDECL_ENVIRONMENT] = type_object_name;
	TypeObjectTable[LISPDECL_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_BROADCAST_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_CONCATENATED_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_ECHO_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_FILE_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_STRING_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_SYNONYM_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_TWO_WAY_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_PROMPT_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_PRETTY_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_MEMORY_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_QUOTE] = type_object_error;
	TypeObjectTable[LISPDECL_BYTESPEC] = type_object_name;
	TypeObjectTable[LISPDECL_PRINT_DISPATCH] = type_object_name;
	TypeObjectTable[LISPDECL_EVAL] = type_object_name;
}


/************************************************************
 *  type_parse.c
 ************************************************************/

static int localhold_parse_type(LocalHold hold,
		Execute ptr, addr *ret, addr pos, addr env)
{
	Return(parse_type(ptr, ret, pos, env));
	localhold_push(hold, *ret);
	return 0;
}


/*
 *  and/or
 */
static int typelist_array4(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr pos, aster, list, array;
	size_t size;
	LocalHold hold;

	GetConst(COMMON_ASTERISK, &aster);
	for (size = 0, list = right; list != Nil; size++) {
		if (! consp(list))
			return fmte_("Invalid ~A form ~S.", left, right, NULL);
		GetCons(list, &pos, &list);
		if (pos == aster)
			return fmte_("~A arguments don't use *.", left, NULL);
	}
	if (0xFFFFFFFFUL < size)
		return fmte_("~A arguments S~ too long.", left, right, NULL);

	vector4_heap(&array, size);
	hold = LocalHold_local_push(ptr, array);
	for (size = 0; right != Nil; size++) {
		GetCons(right, &pos, &right);
		Return(parse_type(ptr, &pos, pos, env));
		SetArrayA4(array, size, pos);
	}
	localhold_end(hold);
	type1_heap(type, array, ret);

	return 0;
}


/*
 *  eql
 */
static int typelist_eql(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr pos, list;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &pos, &list);
	if (list != Nil)
		return fmte_("~A type must be a one argument ~S.", left, right, NULL);
	copyheap(&pos, pos);
	type_eql_heap(pos, ret);

	return 0;
}


/*
 *  member
 */
static int typelist_member(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	size_t size;
	addr pos, array;

	for (size = 0, pos = right; pos != Nil; size++) {
		if (! consp(pos))
			return fmte_("Invalid ~A form ~S.", left, right, NULL);
		GetCdr(pos, &pos);
	}
	if (0xFFFFFFFFUL < size)
		return fmte_("~A arguments ~S too long.", left, right, NULL);
	vector4_heap(&array, size);
	for (size = 0; right != Nil; size++) {
		GetCons(right, &pos, &right);
		copyheap(&pos, pos);
		SetArrayA4(array, size, pos);
	}
	type1_heap(type, array, ret);

	return 0;
}


/*
 *  mod
 */
static int typelist_mod(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	int check;
	addr pos, list;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &pos, &list);
	if (list != Nil)
		return fmte_("~A arguments ~S must be one integer.", right, NULL);
	if (! integerp(pos))
		return fmte_("~A argument ~S must be an integer type.", left, pos, NULL);
	Return(plusp_integer_(pos, &check));
	if (! check)
		return fmte_("~A argument ~S must be a plus integer.", left, pos, NULL);
	copyheap(&pos, pos);
	type1_heap(type, pos, ret);

	return 0;
}


/*
 *  not
 */
static int typelist_not(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr pos, list;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &pos, &list);
	if (list != Nil)
		return fmte_("~A arguments ~S must be one argument.", left, right, NULL);
	GetConst(COMMON_ASTERISK, &list);
	if (pos == list)
		return fmte_("~A argument don't be *.", left, NULL);
	Return(parse_type(ptr, &pos, pos, env));
	type1_heap(type, pos, ret);

	return 0;
}


/*
 *  satisfies
 */
static int typelist_satisfies(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr pos, list;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &pos, &list);
	if (list != Nil)
		return fmte_("~A arguments ~S must be one symbol.", left, right, NULL);
	if (! symbolp(pos))
		return fmte_("~A argument ~S must be a symbol.", left, pos, NULL);
	copyheap(&pos, pos);
	type_satisfies_heap(pos, ret);

	return 0;
}


/*
 *  cons
 */
static int typelist_cons(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr list, car, cdr, aster;
	LocalHold hold;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &car, &list);
	GetConst(COMMON_ASTERISK, &aster);
	hold = LocalHold_local(ptr);
	if (list == Nil) {
		/* one argument */
		if (car == aster)
			goto asterisk;
		Return(localhold_parse_type(hold, ptr, &car, car, env));
		GetTypeTable(&cdr, Asterisk);
	}
	else {
		/* two arguments */
		if (! consp(list))
			return fmte_("Invalid ~A form ~S.", left, right, NULL);
		GetCons(list, &cdr, &list);
		if (list != Nil) {
			return fmte_("~A arguments ~S must have 1 or 2 arguments.",
					left, right, NULL);
		}
		if (car == aster && cdr == aster)
			goto asterisk;
		Return(localhold_parse_type(hold, ptr, &car, car, env));
		Return(localhold_parse_type(hold, ptr, &cdr, cdr, env));
	}
	localhold_end(hold);
	type2_heap(type, car, cdr, ret);
	return 0;

asterisk:
	return parse_type(ptr, ret, left, env);
}


/*
 *  function
 */
static int type_function_lambda(Execute ptr, addr *ret, addr list, addr env)
{
	addr const_opt, const_rest, const_key;
	addr var, opt, rest, key, one, name, type;
	LocalHold hold;

	GetConst(AMPERSAND_OPTIONAL, &const_opt);
	GetConst(AMPERSAND_REST, &const_rest);
	GetConst(AMPERSAND_KEY, &const_key);
	var = opt = rest = key = one = Nil;
	hold = LocalHold_array(ptr, 5);

var_label:
	if (list == Nil)
		goto final;
	Return_getcons(list, &one, &list);
	if (one == const_opt)
		goto opt_label;
	if (one == const_rest)
		goto rest_label;
	if (one == const_key)
		goto key_label;
	Return(parse_type(ptr, &one, one, env));
	cons_heap(&var, one, var);
	localhold_set(hold, 0, var);
	goto var_label;

opt_label:
	if (list == Nil)
		goto final;
	Return_getcons(list, &one, &list);
	if (one == const_opt)
		return fmte_("&optional parameter don't allow this place.", NULL);
	if (one == const_rest)
		goto rest_label;
	if (one == const_key)
		goto key_label;
	Return(parse_type(ptr, &one, one, env));
	cons_heap(&opt, one, opt);
	localhold_set(hold, 1, opt);
	goto opt_label;

rest_label:
	if (list == Nil)
		return fmte_("After &rest parameter must be have a typespec.", NULL);
	Return_getcons(list, &one, &list);
	if (one == const_opt || one == const_rest || one == const_key)
		return fmte_("After &rest parameter don't allow to be a &-symbol.", NULL);
	Return(parse_type(ptr, &rest, one, env));
	localhold_set(hold, 2, rest);
	if (list == Nil)
		goto final;
	Return_getcons(list, &one, &list);
	if (one != const_key)
		return fmte_("After &rest argument don't allow to be a type.", NULL);
	goto key_label;

key_label:
	if (list == Nil)
		goto final;
	Return_getcons(list, &one, &list);
	if (one == const_opt || one == const_rest || one == const_key)
		return fmte_("After &key parameter don't allow to be a &-symbol.", NULL);
	if (! consp(one))
		return fmte_("After &key parameter must be a list.", NULL);
	Return_getcons(one, &name, &one);
	copyheap(&name, name);
	localhold_set(hold, 3, name);
	Return_getcons(one, &type, &one);
	if (one != Nil)
		return fmte_("&key parameter must be a (key type) list.", NULL);
	Return(parse_type(ptr, &type, type, env));
	cons_heap(&one, name, type);
	cons_heap(&key, one, key);
	localhold_set(hold, 4, key);
	goto key_label;

final:
	localhold_end(hold);
	vector2_heap(&one, 4);
	nreverse(&var, var);
	nreverse(&opt, opt);
	nreverse(&key, key);
	SetArrayA2(one, 0, var);
	SetArrayA2(one, 1, opt);
	SetArrayA2(one, 2, rest);
	SetArrayA2(one, 3, key);
	return Result(ret, one);
}

static int type_function_list(Execute ptr, addr *ret, addr right, addr env)
{
	addr aster;

	GetConst(COMMON_ASTERISK, &aster);
	CheckType(aster, LISPTYPE_SYMBOL);
	if (right == aster) {
		GetTypeTable(ret, Asterisk);
		return 0;
	}

	return type_function_lambda(ptr, ret, right, env);
}

/*  typespec* [&optional typespec*] [&rest typespec]
 *  typespec* [&optional typespec*] [&rest typespec] [&allow-other-keys]
 */
#undef LISP_VALUES_ALLOW_ENABLE
static int type_values_typespec(Execute ptr, addr list, addr env,
		addr *retvar, addr *retopt, addr *retrest, addr *retallow)
{
	addr var, vars, opt, rest, allow;
	addr const_opt, const_rest;
#ifdef LISP_VALUES_ALLOW_ENABLE
	addr const_allow;
#endif
	LocalHold hold;

	GetConst(AMPERSAND_OPTIONAL, &const_opt);
	GetConst(AMPERSAND_REST, &const_rest);
#ifdef LISP_VALUES_ALLOW_ENABLE
	GetConst(AMPERSAND_ALLOW, &const_allow);
#endif
	vars = opt = rest = allow = Nil;
	hold = LocalHold_array(ptr, 3);

var_label:
	if (list == Nil)
		goto final;
	Return_getcons(list, &var, &list);
	if (var == const_opt)
		goto optional_label;
	if (var == const_rest)
		goto rest_label;
#ifdef LISP_VALUES_ALLOW_ENABLE
	if (var == const_allow)
		goto allow_label;
#endif
	Return(parse_type(ptr, &var, var, env));
	cons_heap(&vars, var, vars);
	localhold_set(hold, 0, vars);
	goto var_label;

optional_label:
	if (list == Nil)
		goto final;
	Return_getcons(list, &var, &list);
	if (var == const_rest)
		goto rest_label;
#ifdef LISP_VALUES_ALLOW_ENABLE
	if (var == const_allow)
		goto allow_label;
#endif
	Return(parse_type(ptr, &var, var, env));
	cons_heap(&opt, var, opt);
	localhold_set(hold, 1, opt);
	goto optional_label;

rest_label:
	if (list == Nil)
		return fmte_("After &rest argument must be a type.", NULL);
	Return_getcons(list, &var, &list);
	if (var == const_opt || var == const_rest)
		return fmte_("After &rest argument must be a type.", NULL);
#ifdef LISP_VALUES_ALLOW_ENABLE
	if (var == const_allow)
		return fmte_("After &rest argument must be a type.", NULL);
#endif
	Return(parse_type(ptr, &rest, var, env));
	localhold_set(hold, 2, rest);
	if (list == Nil)
		goto final;
	Return_getcons(list, &var, &list);
#ifdef LISP_VALUES_ALLOW_ENABLE
	if (var == const_allow)
		goto allow_label;
#endif
	return fmte_("Invalid values form.", NULL);

#ifdef LISP_VALUES_ALLOW_ENABLE
allow_label:
	allow = T;
	if (list != Nil)
		return fmte_("After &allow-other-keys must be nil.", NULL);
	goto final;
#endif

final:
	localhold_end(hold);
	nreverse(retvar, vars);
	nreverse(retopt, opt);
	*retrest = rest;
	*retallow = allow;

	return 0;
}

static int type_values(Execute ptr, addr *ret, addr right, addr env)
{
	addr var, opt, rest, allow;

	var = opt = rest = allow = Nil;
	Return(type_values_typespec(ptr, right, env, &var, &opt, &rest, &allow));
	if (rest == Nil)
		GetTypeTable(&rest, T);
	type_values_heap(var, opt, rest, allow, ret);

	return 0;
}

static int type_function_values(Execute ptr, addr *ret, addr type, addr env)
{
	addr pos, check, list;

	if (! consp(type))
		return parse_type(ptr, ret, type, env);
	Return_getcons(type, &pos, &list);
	GetConst(COMMON_VALUES, &check);
	if (check != pos)
		return parse_type(ptr, ret, type, env);
	else
		return type_values(ptr, ret, list, env);
}

static int typelist_function(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr list, aster, first, second;
	LocalHold hold;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &first, &list);
	GetConst(COMMON_ASTERISK, &aster);
	if (list == Nil) {
		/* one argument */
		if (first == aster)
			goto asterisk;
		Return(type_function_list(ptr, &first, first, env));
		GetTypeTable(&second, Asterisk);
	}
	else {
		/* two arguments */
		if (! consp(list))
			return fmte_("Invalid ~A form ~S.", left, right, NULL);
		GetCons(list, &second, &list);
		if (list != Nil) {
			return fmte_("~A arguments ~S must have 1 or 2 arguments.",
					left, right, NULL);
		}
		if (first == aster && second == aster)
			goto asterisk;
		Return(type_function_list(ptr, &first, first, env));
		hold = LocalHold_local_push(ptr, first);
		Return(type_function_values(ptr, &second, second, env));
		localhold_end(hold);
	}
	type3_heap(type, first, second, Nil, ret);
	return 0;

asterisk:
	GetTypeTable(&aster, Asterisk);
	type3_heap(type, aster, aster, Nil, ret);
	return 0;
}


/*
 *  array
 */
static int parse_array_length(addr right, size_t *rsize, int *ret)
{
	addr aster, left;
	size_t size;

	GetConst(COMMON_ASTERISK, &aster);
	for (size = 0; right != Nil; size++) {
		if (! consp(right)) {
			*ret = 0;
			*rsize = 0;
			return fmte_("The dimension parameter ~S must be a list.", right, NULL);
		}
		GetCons(right, &left, &right);
		if (left != aster)
			return Result(ret, 0);
	}
	*rsize = size;
	return Result(ret, 1);
}

static int parse_array_fixnum_check(addr *ret, addr pos)
{
	if ((! fixnump(pos)) || RefFixnum(pos) < 0) {
		return fmte_("The dimension value ~S "
				"must be a non-negative fixnum.", pos, NULL);
	}
	copyheap(ret, pos);
	return 0;
}

static int parse_array_dimension(addr *ret, addr right)
{
	addr aster, left, array;
	size_t size;

	/* length */
	for (size = 0, left = right; left != Nil; size++) {
		if (! consp(left))
			return fmte_("The dimension parameter ~S must be a list.", left, NULL);
		GetCdr(left, &left);
	}

	/* make vector */
	GetConst(COMMON_ASTERISK, &aster);
	vector4_heap(&array, size);
	for (size = 0; right != Nil; size++) {
		GetCons(right, &left, &right);
		if (left == aster) {
			GetTypeTable(&left, Asterisk);
		}
		else {
			Return(parse_array_fixnum_check(&left, left));
		}
		SetArrayA4(array, size, left);
	}

	return Result(ret, array);
}

static int parse_array_second(addr *ret, addr right)
{
	int check;
	addr aster;
	size_t size;

	GetConst(COMMON_ASTERISK, &aster);
	if (right == Nil) {
		/* dimension arguments, 0 */
		fixnum_heap(ret, 0);
	}
	else if (consp(right)) {
		/* dimension arguments */
		Return(parse_array_length(right, &size, &check));
		if (check) {
			if (FIXNUM_MAX < size)
				return fmte_("size overflow.", NULL);
			fixnum_heap(ret, (fixnum)size);
		}
		else {
			Return(parse_array_dimension(ret, right));
		}
	}
	else {
		/* finxum arguments */
		Return(parse_array_fixnum_check(ret, right));
	}

	return 0;
}

static int typelist_array(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr list, aster, first, second;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &first, &list);
	GetConst(COMMON_ASTERISK, &aster);
	if (list == Nil) {
		/* one argument */
		if (first == aster)
			goto asterisk;
		Return(parse_type(ptr, &first, first, env));
		GetTypeTable(&second, Asterisk);
	}
	else {
		/* two arguments */
		if (! consp(list))
			return fmte_("Invalid ~A form ~S.", left, right, NULL);
		GetCons(list, &second, &list);
		if (list != Nil)
			return fmte_("~A type arguments too long.", left, NULL);
		if (first == aster && second == aster)
			goto asterisk;
		if (first == aster) {
			GetTypeTable(&first, Asterisk);
		}
		else {
			Return(parse_type(ptr, &first, first, env));
		}
		if (second == aster) {
			GetTypeTable(&second, Asterisk);
		}
		else {
			Return(parse_array_second(&second, second));
		}
	}
	if (! type_asterisk_p(first)) {
		Return(upgraded_array_type_(first, &first));
	}
	type2_heap(type, first, second, ret);
	return 0;

asterisk:
	return parse_type(ptr, ret, left, env);
}


/*
 *  vector
 */
static int typelist_vector(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr list, aster, first, second;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &first, &list);
	GetConst(COMMON_ASTERISK, &aster);
	if (list == Nil) {
		/* one argument */
		if (first == aster)
			goto asterisk;
		Return(parse_type(ptr, &first, first, env));
		GetTypeTable(&second, Asterisk);
	}
	else {
		/* two arguments */
		if (! consp(list))
			return fmte_("Invalid ~A form ~S.", left, right, NULL);
		GetCons(list, &second, &list);
		if (list != Nil)
			return fmte_("~A arguments ~S too long.", left, right, NULL);
		if (first == aster && second == aster)
			goto asterisk;
		if (first == aster) {
			GetTypeTable(&first, Asterisk);
		}
		else {
			Return(parse_type(ptr, &first, first, env));
		}
		if (second == aster) {
			GetTypeTable(&second, Asterisk);
		}
		else {
			Return(parse_array_fixnum_check(&second, second));
		}
	}
	if (! type_asterisk_p(first)) {
		Return(upgraded_array_type_(first, &first));
	}
	type2_heap(type, first, second, ret);
	return 0;

asterisk:
	return parse_type(ptr, ret, left, env);
}


/*
 *  size
 */
static int typelist_size(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr list, aster, first;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	/* one argument */
	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &first, &list);
	if (list != Nil)
		return fmte_("~A arguments ~S too long.", left, right, NULL);
	GetConst(COMMON_ASTERISK, &aster);
	if (first == aster)
		goto asterisk;
	Return(parse_array_fixnum_check(&first, first));
	type1_heap(type, first, ret);
	return 0;

asterisk:
	return parse_type(ptr, ret, left, env);
}


/*
 *  range
 */
static int type_range_element(addr left, addr right, int (*call)(addr),
		addr *ret1, addr *ret2)
{
	addr pos, list;

	if (consp(right)) {
		GetCons(right, &pos, &list);
		if (list != Nil) {
			return fmte_("~A argument ~S "
					"must be a real or (real) form.", left, right, NULL);
		}
		Return((*call)(pos));
		*ret1 = T;
		copyheap(ret2, pos);
	}
	else {
		Return((*call)(right));
		*ret1 = Nil;
		copyheap(ret2, right);
	}

	return 0;
}

/* (integer 10 (20))  -> (integer nil 10 t 20) */
static int typelist_range(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env,
		int (*call)(addr))
{
	addr list, aster, first1, first2, second1, second2;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &first1, &list);
	GetConst(COMMON_ASTERISK, &aster);
	if (list == Nil) {
		/* one argument */
		if (first1 == aster)
			goto asterisk;
		Return(type_range_element(left, first1, call, &first1, &first2));
		GetTypeTable(&second1, Asterisk);
		second2 = second1;
	}
	else {
		/* two arguments */
		if (! consp(list))
			return fmte_("Invalid ~A form ~S.", left, right, NULL);
		GetCons(list, &second1, &list);
		if (list != Nil)
			return fmte_("~A arguments ~S too long.", left, right, NULL);
		if (first1 == aster && second1 == aster)
			goto asterisk;
		if (first1 == aster) {
			GetTypeTable(&first1, Asterisk);
			first2 = first1;
		}
		else {
			Return(type_range_element(left, first1, call, &first1, &first2));
		}
		if (second1 == aster) {
			GetTypeTable(&second1, Asterisk);
			second2 = second1;
		}
		else {
			Return(type_range_element(left, second1, call, &second1, &second2));
		}
	}
	type4_heap(type, first1, first2, second1, second2, ret);
	return 0;

asterisk:
	return parse_type(ptr, ret, left, env);
}


/*
 *  real
 */
static int typelist_real_p(addr pos)
{
	if (! realp(pos))
		return fmte_("REAL argument ~S must be a real.", pos, NULL);
	return 0;
}
static int typelist_real(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_real_p);
}


/*
 *  rational
 */
static int typelist_rational_p(addr pos)
{
	if (! rationalp(pos))
		return fmte_("RATIONAL argument ~S must be a rational.", pos, NULL);
	return 0;
}
static int typelist_rational(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_rational_p);
}


/*
 *  integer
 */
static int typelist_integer_p(addr pos)
{
	if (! integerp(pos))
		return fmte_("INTEGER argument ~S must be an integer.", pos, NULL);
	return 0;
}
static int typelist_integer(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_integer_p);
}


/*
 *  float
 */
static int typelist_float_p(addr pos)
{
	if (! floatp(pos))
		return fmte_("FLOAT argument ~S must be a float.", pos, NULL);
	return 0;
}
static int typelist_float(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_float_p);
}


/*
 *  short-float
 */
static int typelist_short_float_p(addr pos)
{
	if (GetType(pos) != LISPTYPE_SHORT_FLOAT)
		return fmte_("SHORT-FLOAT argument ~S must be a short-float.", pos, NULL);
	return 0;
}
static int typelist_short(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_short_float_p);
}


/*
 *  single-float
 */
static int typelist_single_float_p(addr pos)
{
	if (GetType(pos) != LISPTYPE_SINGLE_FLOAT)
		return fmte_("SINGLE-FLOAT argument ~S must be a single-float.", pos, NULL);
	return 0;
}
static int typelist_single(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_single_float_p);
}


/*
 *  double-float
 */
static int typelist_double_float_p(addr pos)
{
	if (GetType(pos) != LISPTYPE_DOUBLE_FLOAT)
		return fmte_("DOUBLE-FLOAT argument ~S must be a double-float.", pos, NULL);
	return 0;
}
static int typelist_double(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_double_float_p);
}


/*
 *  long-float
 */
static int typelist_long_float_p(addr pos)
{
	if (GetType(pos) != LISPTYPE_LONG_FLOAT)
		return fmte_("LONG-FLOAT argument ~S must be a long-float.", pos, NULL);
	return 0;
}
static int typelist_long(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_long_float_p);
}


/*
 *  byte
 */
static int type_byte_integer_check(addr *ret, addr pos)
{
	int check;

	if (GetType(pos) != LISPTYPE_FIXNUM)
		goto error;
	Return(plusp_integer_(pos, &check));
	if (! check)
		goto error;
	copyheap(ret, pos);
	return 0;

error:
	*ret = Nil;
	return fmte_("The value ~S must be a positive integer.", pos, NULL);
}

static int typelist_byte(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr list, aster, first;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	/* one argument */
	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &first, &list);
	if (list != Nil)
		return fmte_("~A arguments ~S too long.", left, right, NULL);
	GetConst(COMMON_ASTERISK, &aster);
	if (first == aster)
		goto asterisk;
	Return(type_byte_integer_check(&first, first));
	type1_heap(type, first, ret);
	return 0;

asterisk:
	return parse_type(ptr, ret, left, env);
}


/*
 *  complex
 */
static int typelist_complex(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr list, aster, first;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	/* one argument */
	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &first, &list);
	if (list != Nil)
		return fmte_("~A arguments ~S too long.", left, right, NULL);
	GetConst(COMMON_ASTERISK, &aster);
	if (first == aster)
		goto asterisk;
	Return(parse_type(ptr, &first, first, env));
	Return(upgraded_complex_type_(ptr, env, first, &first));
	type1_heap(type, first, ret);
	return 0;

asterisk:
	return parse_type(ptr, ret, left, env);
}


/*
 *  parse-type
 */
typedef int (*call_typelist)(Execute, addr *, enum LISPDECL, addr, addr, addr);
static call_typelist TypeParseList[LISPDECL_SIZE];
#define DefListInit(a, b) (TypeParseList[LISPDECL_##a] = typelist_##b)

void init_type_parse(void)
{
	/* Compound-type */
	DefListInit(AND,                 array4      );
	DefListInit(OR,                  array4      );
	DefListInit(EQL,                 eql         );
	DefListInit(MEMBER,              member      );
	DefListInit(MOD,                 mod         );
	DefListInit(NOT,                 not         );
	DefListInit(SATISFIES,           satisfies   );
	/* LispInit(VALUES,              values      ); */

	/* Atomic-type */
	DefListInit(CONS,                cons        );
	DefListInit(FUNCTION,            function    );
	DefListInit(COMPILED_FUNCTION,   function    );
	DefListInit(ARRAY,               array       );
	DefListInit(SIMPLE_ARRAY,        array       );
	DefListInit(VECTOR,              vector      );
	DefListInit(SIMPLE_VECTOR,       size        );
	DefListInit(BIT_VECTOR,          size        );
	DefListInit(SIMPLE_BIT_VECTOR,   size        );
	DefListInit(STRING,              size        );
	DefListInit(BASE_STRING,         size        );
	DefListInit(SIMPLE_STRING,       size        );
	DefListInit(SIMPLE_BASE_STRING,  size        );
	DefListInit(REAL,                real        );
	DefListInit(RATIONAL,            rational    );
	DefListInit(INTEGER,             integer     );
	DefListInit(SIGNED_BYTE,         byte        );
	DefListInit(UNSIGNED_BYTE,       byte        );
	DefListInit(COMPLEX,             complex     );
	DefListInit(FLOAT,               float       );
	DefListInit(SHORT_FLOAT,         short       );
	DefListInit(SINGLE_FLOAT,        single      );
	DefListInit(DOUBLE_FLOAT,        double      );
	DefListInit(LONG_FLOAT,          long        );
}

#define SetTypeParseObject(x,v) (*(enum LISPDECL *)PtrBodyB2(x) = (v))
#define GetTypeParseObject(x,r) (*(r) = *(enum LISPDECL *)PtrBodyB2(x))
static void make_type_parse_object(addr *ret, enum LISPDECL type)
{
	addr pos;

	heap_body2(&pos, LISPSYSTEM_TYPE_PARSE, sizeoft(enum LISPDECL));
	SetTypeParseObject(pos, type);
	*ret = pos;
}

static void define_type_parse_object(enum LISPDECL type, constindex name)
{
	addr symbol, pos;

	GetConstant(name, &symbol);
	CheckType(symbol, LISPTYPE_SYMBOL);
	make_type_parse_object(&pos, type);
	setlisttype_symbol(symbol, pos);
}
#define DefListType(a) define_type_parse_object(LISPDECL_##a, CONSTANT_COMMON_##a)

void build_type_parse(void)
{
	/* Compound-type */
	DefListType(AND                 );
	DefListType(OR                  );
	DefListType(EQL                 );
	DefListType(MEMBER              );
	DefListType(MOD                 );
	DefListType(NOT                 );
	DefListType(SATISFIES           );
	/* ListType(VALUES              );*/

	/* Atomic-type */
	DefListType(CONS                );
	DefListType(FUNCTION            );
	DefListType(COMPILED_FUNCTION   );
	DefListType(ARRAY               );
	DefListType(SIMPLE_ARRAY        );
	DefListType(VECTOR              );
	DefListType(SIMPLE_VECTOR       );
	DefListType(BIT_VECTOR          );
	DefListType(SIMPLE_BIT_VECTOR   );
	DefListType(STRING              );
	DefListType(BASE_STRING         );
	DefListType(SIMPLE_STRING       );
	DefListType(SIMPLE_BASE_STRING  );
	DefListType(REAL                );
	DefListType(RATIONAL            );
	DefListType(INTEGER             );
	DefListType(SIGNED_BYTE         );
	DefListType(UNSIGNED_BYTE       );
	DefListType(COMPLEX             );
	DefListType(FLOAT               );
	DefListType(SHORT_FLOAT         );
	DefListType(SINGLE_FLOAT        );
	DefListType(DOUBLE_FLOAT        );
	DefListType(LONG_FLOAT          );
}

static int parse_type_default(Execute ptr,
		addr *ret, addr symbol, addr args, addr env)
{
	enum LISPDECL type;
	call_typelist call;
	addr pos;

	CheckType(symbol, LISPTYPE_SYMBOL);
	getlisttype_symbol(symbol, &pos);
	if (pos == Nil)
		return Result(ret, NULL);
	CheckType(pos, LISPSYSTEM_TYPE_PARSE);
	GetTypeParseObject(pos, &type);
	call = TypeParseList[type];

	return (*call)(ptr, ret, type, symbol, args, env);
}

static int parse_type_list(Execute ptr, addr *ret, addr pos, addr env)
{
	addr symbol, args, check;

	GetCons(pos, &symbol, &args);
	if (! symbolp(symbol))
		return TypeError_(symbol, SYMBOL);

	/* lisp type */
	Return(parse_type_default(ptr, &check, symbol, args, env));
	if (check)
		return Result(ret, check);

	/* deftype */
	Return(execute_list_deftype(ptr, &check, pos, env));
	if (check)
		return parse_type(ptr, ret, check, env);

	/* error */
	type_error_heap(pos, ret);
	return 0;
}

static int parse_type_symbol(Execute ptr, addr *ret, addr pos, addr env)
{
	addr check;

	Return(find_symbol_type(ptr, &check, pos, env));
	if (check)
		return parse_type(ptr, ret, check, env);

	/* error */
	type_error_heap(pos, ret);
	return 0;
}

static int parse_type_type(Execute ptr, addr *ret, addr pos)
{
	addr x, y, z;

	/* type */
	type_throw_heap(pos, &pos);
	if (RefLispDecl(pos) != LISPDECL_CLOS)
		return Result(ret, pos);

	/* clos */
	GetArrayType(pos, 0, &x);
	if (type_asterisk_p(x))
		return Result(ret, pos);
	GetClassOfClos(x, &y);
	GetConst(CLOS_BUILT_IN_CLASS, &z);
	if (y != z)
		return Result(ret, pos);

	/* built-in-class */
	Return(stdget_class_name_(x, &x));
	return parse_type(ptr, ret, x, Nil);  /* don't use env */
}

static int parse_type_clos(Execute ptr, addr *ret, addr pos)
{
	addr x, y;

	GetClassOfClos(pos, &x);
	GetConst(CLOS_BUILT_IN_CLASS, &y);
	if (x != y) {
		type_clos_heap(pos, ret);
		return 0;
	}

	/* built-in-class */
	Return(stdget_class_name_(pos, &x));
	return parse_type(ptr, ret, x, Nil);  /* don't use env */
}

static int parse_type_null(Execute ptr, addr *ret, addr pos, addr env)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			return parse_type_symbol(ptr, ret, pos, env);

		case LISPTYPE_CONS:
			return parse_type_list(ptr, ret, pos, env);

		case LISPTYPE_TYPE:
			return parse_type_type(ptr, ret, pos);

		case LISPTYPE_CLOS:
			return parse_type_clos(ptr, ret, pos);

		default:
			return Result(ret, NULL);
	}
}

int parse_type(Execute ptr, addr *ret, addr pos, addr env)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, pos, env, NULL);
	Return(parse_type_null(ptr, ret, pos, env));
	localhold_end(hold);

	if (*ret == NULL)
		return fmte_("Invalid type-spec ~S.", pos, NULL);

	return 0;
}

int parse_type_not(Execute ptr, addr *ret, addr pos, addr env)
{
	Return(parse_type(ptr, &pos, pos, env));
	type_copy_unsafe_heap(&pos, pos);
	type_revnotdecl(pos);
	return Result(ret, pos);
}

int parse_type_noaster(Execute ptr, addr *ret, addr pos, addr env)
{
	addr aster;

	GetConst(COMMON_ASTERISK, &aster);
	if (pos == aster)
		return fmte_("Don't allow to use asterisk type.", NULL);

	return parse_type(ptr, ret, pos, env);
}

void parse_type_unsafe(addr *ret, addr pos)
{
	if (parse_type(Execute_Thread, ret, pos, Nil)) {
		Abort("parse-type error.");
	}
}


/* debug */
int parse_type_values(Execute ptr, addr *ret, addr type, addr env)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, type, env, NULL);
	Return(type_function_values(ptr, ret, type, env));
	localhold_end(hold);

	return 0;
}


/************************************************************
 *  type_symbol.c
 ************************************************************/

/*
 *  build-type-symbol
 */
#define DefSymbolType(a,b) define_symbol_type(CONSTANT_##a, TypeTable_##b)
static void define_symbol_type(constindex name, enum TypeTable type)
{
	addr symbol, value;

	GetConstant(name, &symbol);
	gettypetable(type, &value);
	setsymboltype_symbol(symbol, value);
}

void build_type_symbol(void)
{
	DefSymbolType(SYSTEM_INVALID,              Invalid             );
	DefSymbolType(COMMON_ASTERISK,             Asterisk            );

	/* Extract-type */
	DefSymbolType(COMMON_ATOM,                 Atom                );
	DefSymbolType(COMMON_LIST,                 List                );
	DefSymbolType(COMMON_BOOLEAN,              Boolean             );
	DefSymbolType(COMMON_VECTOR,               Vector              );
	DefSymbolType(COMMON_SIMPLE_VECTOR,        SimpleVector        );
	DefSymbolType(COMMON_BIT_VECTOR,           BitVector           );
	DefSymbolType(COMMON_SIMPLE_BIT_VECTOR,    SimpleBitVector     );
	DefSymbolType(COMMON_EXTENDED_CHAR,        ExtendedChar        );
	DefSymbolType(COMMON_STRING,               String              );
	DefSymbolType(COMMON_BASE_STRING,          BaseString          );
	DefSymbolType(COMMON_SIMPLE_STRING,        SimpleString        );
	DefSymbolType(COMMON_SIMPLE_BASE_STRING,   SimpleBaseString    );
	DefSymbolType(COMMON_SIGNED_BYTE,          SignedByte          );
	DefSymbolType(COMMON_UNSIGNED_BYTE,        UnsignedByte        );
	DefSymbolType(COMMON_BIT,                  Bit                 );
	DefSymbolType(COMMON_FIXNUM,               Fixnum              );
	DefSymbolType(COMMON_BIGNUM,               Bignum              );

	/* Atomic-type */
	DefSymbolType(COMMON_NIL,                  Nil                 );
	DefSymbolType(COMMON_T,                    T                   );
	DefSymbolType(COMMON_NULL,                 Null                );
	DefSymbolType(COMMON_CONS,                 Cons                );
	DefSymbolType(COMMON_HASH_TABLE,           Hashtable           );
	DefSymbolType(COMMON_SYMBOL,               Symbol              );
	DefSymbolType(COMMON_KEYWORD,              Keyword             );
	DefSymbolType(COMMON_PACKAGE,              Package             );
	DefSymbolType(COMMON_RANDOM_STATE,         RandomState         );
	DefSymbolType(COMMON_READTABLE,            Readtable           );
	DefSymbolType(COMMON_FUNCTION,             Function            );
	DefSymbolType(COMMON_COMPILED_FUNCTION,    CompiledFunction    );
	DefSymbolType(COMMON_PATHNAME,             Pathname            );
	DefSymbolType(COMMON_LOGICAL_PATHNAME,     LogicalPathname     );
	DefSymbolType(COMMON_SEQUENCE,             Sequence            );
	DefSymbolType(COMMON_ARRAY,                Array               );
	DefSymbolType(COMMON_SIMPLE_ARRAY,         SimpleArray         );
	DefSymbolType(COMMON_CHARACTER,            Character           );
	DefSymbolType(COMMON_BASE_CHAR,            BaseChar            );
	DefSymbolType(COMMON_STANDARD_CHAR,        StandardChar        );
	DefSymbolType(COMMON_NUMBER,               Number              );
	DefSymbolType(COMMON_REAL,                 Real                );
	DefSymbolType(COMMON_RATIONAL,             Rational            );
	DefSymbolType(COMMON_RATIO,                Ratio               );
	DefSymbolType(COMMON_INTEGER,              Integer             );
	DefSymbolType(COMMON_COMPLEX,              Complex             );
	DefSymbolType(COMMON_FLOAT,                Float               );
	DefSymbolType(COMMON_SHORT_FLOAT,          ShortFloat          );
	DefSymbolType(COMMON_SINGLE_FLOAT,         SingleFloat         );
	DefSymbolType(COMMON_DOUBLE_FLOAT,         DoubleFloat         );
	DefSymbolType(COMMON_LONG_FLOAT,           LongFloat           );
	DefSymbolType(COMMON_RESTART,              Restart             );
	DefSymbolType(SYSTEM_ENVIRONMENT,          Environment         );
	DefSymbolType(COMMON_STREAM,               Stream              );
	DefSymbolType(COMMON_BROADCAST_STREAM,     BroadcastStream     );
	DefSymbolType(COMMON_CONCATENATED_STREAM,  ConcatenatedStream  );
	DefSymbolType(COMMON_ECHO_STREAM,          EchoStream          );
	DefSymbolType(COMMON_FILE_STREAM,          FileStream          );
	DefSymbolType(COMMON_STRING_STREAM,        StringStream        );
	DefSymbolType(COMMON_SYNONYM_STREAM,       SynonymStream       );
	DefSymbolType(COMMON_TWO_WAY_STREAM,       TwoWayStream        );
	DefSymbolType(SYSTEM_PROMPT_STREAM,        PromptStream        );
	DefSymbolType(SYSTEM_PRETTY_STREAM,        PrettyStream        );
	DefSymbolType(SYSTEM_MEMORY_STREAM,        MemoryStream        );
	DefSymbolType(SYSTEM_QUOTE,                Quote               );
	DefSymbolType(SYSTEM_BYTESPEC,             ByteSpec            );
	DefSymbolType(SYSTEM_PRINT_DISPATCH,       PrintDispatch       );
	DefSymbolType(SYSTEM_EVAL,                 Eval                );
}


/*
 *  init-type-symbol
 */
static constindex TypeSymbolTable[LISPDECL_SIZE];
#define DefTypeSymbol(a,b) define_type_symbol(LISPDECL_##a, CONSTANT_##b)
static void define_type_symbol(enum LISPDECL type, constindex name)
{
	TypeSymbolTable[(int)type] = name;
}

void init_type_symbol(void)
{
	DefTypeSymbol(EMPTY,                EMPTY                       );
	DefTypeSymbol(INVALID,              SYSTEM_INVALID              );
	DefTypeSymbol(OPTIMIZED,            EMPTY                       );
	DefTypeSymbol(SUBTYPEP,             EMPTY                       );
	DefTypeSymbol(TYPE,                 SYSTEM_TYPE                 );
	DefTypeSymbol(CLOS,                 EMPTY                       );
	DefTypeSymbol(ASTERISK,             COMMON_ASTERISK             );

	/* Compound-type */
	DefTypeSymbol(AND,                  EMPTY                       );
	DefTypeSymbol(EQL,                  EMPTY                       );
	DefTypeSymbol(MEMBER,               EMPTY                       );
	DefTypeSymbol(MOD,                  EMPTY                       );
	DefTypeSymbol(NOT,                  EMPTY                       );
	DefTypeSymbol(OR,                   EMPTY                       );
	DefTypeSymbol(SATISFIES,            EMPTY                       );
	DefTypeSymbol(VALUES,               EMPTY                       );

	/* Extract-type */
	DefTypeSymbol(ATOM,                 COMMON_ATOM                 );
	DefTypeSymbol(LIST,                 COMMON_LIST                 );
	DefTypeSymbol(BOOLEAN,              COMMON_BOOLEAN              );
	DefTypeSymbol(VECTOR,               COMMON_VECTOR               );
	DefTypeSymbol(SIMPLE_VECTOR,        COMMON_SIMPLE_VECTOR        );
	DefTypeSymbol(BIT_VECTOR,           COMMON_BIT_VECTOR           );
	DefTypeSymbol(SIMPLE_BIT_VECTOR,    COMMON_SIMPLE_BIT_VECTOR    );
	DefTypeSymbol(EXTENDED_CHAR,        COMMON_EXTENDED_CHAR        );
	DefTypeSymbol(STRING,               COMMON_STRING               );
	DefTypeSymbol(BASE_STRING,          COMMON_BASE_STRING          );
	DefTypeSymbol(SIMPLE_STRING,        COMMON_SIMPLE_STRING        );
	DefTypeSymbol(SIMPLE_BASE_STRING,   COMMON_SIMPLE_BASE_STRING   );
	DefTypeSymbol(SIGNED_BYTE,          COMMON_SIGNED_BYTE          );
	DefTypeSymbol(UNSIGNED_BYTE,        COMMON_UNSIGNED_BYTE        );
	DefTypeSymbol(BIT,                  COMMON_BIT                  );
	DefTypeSymbol(FIXNUM,               COMMON_FIXNUM               );
	DefTypeSymbol(BIGNUM,               COMMON_BIGNUM               );

	/* Atomic-type */
	DefTypeSymbol(NIL,                  COMMON_NIL                  );
	DefTypeSymbol(T,                    COMMON_T                    );
	DefTypeSymbol(NULL,                 COMMON_NULL                 );
	DefTypeSymbol(CONS,                 COMMON_CONS                 );
	DefTypeSymbol(HASH_TABLE,           COMMON_HASH_TABLE           );
	DefTypeSymbol(SYMBOL,               COMMON_SYMBOL               );
	DefTypeSymbol(KEYWORD,              COMMON_KEYWORD              );
	DefTypeSymbol(PACKAGE,              COMMON_PACKAGE              );
	DefTypeSymbol(RANDOM_STATE,         COMMON_RANDOM_STATE         );
	DefTypeSymbol(READTABLE,            COMMON_READTABLE            );
	DefTypeSymbol(FUNCTION,             COMMON_FUNCTION             );
	DefTypeSymbol(COMPILED_FUNCTION,    COMMON_COMPILED_FUNCTION    );
	DefTypeSymbol(PATHNAME,             COMMON_PATHNAME             );
	DefTypeSymbol(LOGICAL_PATHNAME,     COMMON_LOGICAL_PATHNAME     );
	DefTypeSymbol(SEQUENCE,             COMMON_SEQUENCE             );
	DefTypeSymbol(ARRAY,                COMMON_ARRAY                );
	DefTypeSymbol(SIMPLE_ARRAY,         COMMON_SIMPLE_ARRAY         );
	DefTypeSymbol(CHARACTER,            COMMON_CHARACTER            );
	DefTypeSymbol(BASE_CHAR,            COMMON_BASE_CHAR            );
	DefTypeSymbol(STANDARD_CHAR,        COMMON_STANDARD_CHAR        );
	DefTypeSymbol(NUMBER,               COMMON_NUMBER               );
	DefTypeSymbol(REAL,                 COMMON_REAL                 );
	DefTypeSymbol(RATIONAL,             COMMON_RATIONAL             );
	DefTypeSymbol(RATIO,                COMMON_RATIO                );
	DefTypeSymbol(INTEGER,              COMMON_INTEGER              );
	DefTypeSymbol(COMPLEX,              COMMON_COMPLEX              );
	DefTypeSymbol(FLOAT,                COMMON_FLOAT                );
	DefTypeSymbol(SHORT_FLOAT,          COMMON_SHORT_FLOAT          );
	DefTypeSymbol(SINGLE_FLOAT,         COMMON_SINGLE_FLOAT         );
	DefTypeSymbol(DOUBLE_FLOAT,         COMMON_DOUBLE_FLOAT         );
	DefTypeSymbol(LONG_FLOAT,           COMMON_LONG_FLOAT           );
	DefTypeSymbol(RESTART,              COMMON_RESTART              );
	DefTypeSymbol(ENVIRONMENT,          SYSTEM_ENVIRONMENT          );
	DefTypeSymbol(STREAM,               COMMON_STREAM               );
	DefTypeSymbol(BROADCAST_STREAM,     COMMON_BROADCAST_STREAM     );
	DefTypeSymbol(CONCATENATED_STREAM,  COMMON_CONCATENATED_STREAM  );
	DefTypeSymbol(ECHO_STREAM,          COMMON_ECHO_STREAM          );
	DefTypeSymbol(FILE_STREAM,          COMMON_FILE_STREAM          );
	DefTypeSymbol(STRING_STREAM,        COMMON_STRING_STREAM        );
	DefTypeSymbol(SYNONYM_STREAM,       COMMON_SYNONYM_STREAM       );
	DefTypeSymbol(TWO_WAY_STREAM,       COMMON_TWO_WAY_STREAM       );
	DefTypeSymbol(PROMPT_STREAM,        SYSTEM_PROMPT_STREAM        );
	DefTypeSymbol(PRETTY_STREAM,        SYSTEM_PRETTY_STREAM        );
	DefTypeSymbol(MEMORY_STREAM,        SYSTEM_MEMORY_STREAM        );
	DefTypeSymbol(QUOTE,                SYSTEM_QUOTE                );
	DefTypeSymbol(BYTESPEC,             SYSTEM_BYTESPEC             );
	DefTypeSymbol(PRINT_DISPATCH,       SYSTEM_PRINT_DISPATCH       );
	DefTypeSymbol(EVAL,                 SYSTEM_EVAL                 );
}


/*
 *  symbol -> type
 *    1. find symbol.
 *    2. find clos.
 *    3. find deftype.
 *    4. return unbound.
 */
int find_symbol_type(Execute ptr, addr *ret, addr symbol, addr env)
{
	addr check;

	Check(! symbolp(symbol), "type error");
	/* find symbol */
	getsymboltype_symbol(symbol, &check);
	if (check != Nil)
		return Result(ret, check);

	/* find clos */
	clos_find_class_nil(symbol, &check);
	if (check != Nil) {
		type_clos_heap(check, ret);
		return 0;
	}

	/* find deftype */
	Return(execute_symbol_deftype(ptr, &check, symbol, env));
	if (check)
		return parse_type(ptr, ret, check, env);

	/* error */
	return Result(ret, NULL);
}


/*
 *  type -> symbol
 */
constindex getdeclname(enum LISPDECL type)
{
	Check(LISPDECL_SIZE <= type, "index error");
	return TypeSymbolTable[type];
}


/*
 *  function
 */
int type_symbol_p(addr symbol)
{
	addr check;

	/* symbol check */
	if (! symbolp(symbol))
		return 0;

	/* find symbol */
	getsymboltype_symbol(symbol, &check);
	if (check != Nil)
		return 1;

	/* find clos */
	clos_find_class_nil(symbol, &check);
	if (check != Nil)
		return 1;

	/* find deftype */
	return symbol_deftypep(symbol);
}


/************************************************************
 *  type_table.c
 ************************************************************/


/*
 *  interface
 */
static void getroot_typetable(addr *ret)
{
	*ret = LispRoot(TYPETABLE);
	CheckType(*ret, LISPTYPE_VECTOR);
}

addr reftypetable(enum TypeTable index)
{
	addr pos;
	gettypetable(index, &pos);
	return pos;
}

void gettypetable(enum TypeTable index, addr *ret)
{
	addr table;

	getroot_typetable(&table);
	GetArrayA4(table, (size_t)index, ret);
	Check(*ret == Nil, "type error");
}

void settypetable(enum TypeTable index, addr pos)
{
	addr table;

	getroot_typetable(&table);
	SetArrayA4(table, (size_t)index, pos);
	SetStatusReadOnly(pos);
}

void keytypetable(constindex name, enum TypeTable type, addr *ret)
{
	addr value1, value2;

	GetConstant(name, &value1);
	gettypetable(type, &value2);
	cons_heap(ret, value1, value2);
}

void build_type_table(void)
{
	addr pos;
	vector4_heap(&pos, TypeTable_Size);
	SetLispRoot(TYPETABLE, pos);
}


/*
 *  arguments
 */
void typeargs_empty(addr *ret)
{
	vector2_heap(ret, 4);
}

void typeargs_full(addr *ret, addr var, addr opt, addr rest, addr key)
{
	addr pos;

	vector2_heap(&pos, 4);
	SetArrayA2(pos, 0, var);
	SetArrayA2(pos, 1, opt);
	SetArrayA2(pos, 2, rest);
	SetArrayA2(pos, 3, key);
	*ret = pos;
}

void typeargs_var1(addr *ret, addr v1)
{
	conscar_heap(&v1, v1);
	typeargs_full(ret, v1, Nil, Nil, Nil);
}

void typeargs_var2(addr *ret, addr v1, addr v2)
{
	list_heap(&v1, v1, v2, NULL);
	typeargs_full(ret, v1, Nil, Nil, Nil);
}

void typeargs_var3(addr *ret, addr v1, addr v2, addr v3)
{
	list_heap(&v1, v1, v2, v3, NULL);
	typeargs_full(ret, v1, Nil, Nil, Nil);
}

void typeargs_var4(addr *ret, addr v1, addr v2, addr v3, addr v4)
{
	list_heap(&v1, v1, v2, v3, v4, NULL);
	typeargs_full(ret, v1, Nil, Nil, Nil);
}

void typeargs_var5(addr *ret, addr v1, addr v2, addr v3, addr v4, addr v5)
{
	list_heap(&v1, v1, v2, v3, v4, v5, NULL);
	typeargs_full(ret, v1, Nil, Nil, Nil);
}

void typeargs_var1key(addr *ret, addr v1, addr key)
{
	conscar_heap(&v1, v1);
	typeargs_full(ret, v1, Nil, Nil, key);
}

void typeargs_var2key(addr *ret, addr v1, addr v2, addr key)
{
	list_heap(&v1, v1, v2, NULL);
	typeargs_full(ret, v1, Nil, Nil, key);
}

void typeargs_var3key(addr *ret, addr v1, addr v2, addr var3, addr key)
{
	list_heap(&v1, v1, v2, var3, NULL);
	typeargs_full(ret, v1, Nil, Nil, key);
}

void typeargs_var4key(addr *ret, addr v1, addr v2, addr v3, addr v4, addr key)
{
	list_heap(&v1, v1, v2, v3, v4, NULL);
	typeargs_full(ret, v1, Nil, Nil, key);
}

void typeargs_opt1(addr *ret, addr v1)
{
	conscar_heap(&v1, v1);
	typeargs_full(ret, Nil, v1, Nil, Nil);
}

void typeargs_opt2(addr *ret, addr v1, addr v2)
{
	list_heap(&v1, v1, v2, NULL);
	typeargs_full(ret, Nil, v1, Nil, Nil);
}

void typeargs_opt3(addr *ret, addr v1, addr v2, addr v3)
{
	list_heap(&v1, v1, v2, v3, NULL);
	typeargs_full(ret, Nil, v1, Nil, Nil);
}
void typeargs_opt4(addr *ret, addr v1, addr v2, addr v3, addr v4)
{
	list_heap(&v1, v1, v2, v3, v4, NULL);
	typeargs_full(ret, Nil, v1, Nil, Nil);
}

void typeargs_opt5(addr *ret, addr v1, addr v2, addr v3, addr v4, addr v5)
{
	list_heap(&v1, v1, v2, v3, v4, v5, NULL);
	typeargs_full(ret, Nil, v1, Nil, Nil);
}

void typeargs_var1opt1(addr *ret, addr var1, addr opt1)
{
	conscar_heap(&var1, var1);
	conscar_heap(&opt1, opt1);
	typeargs_full(ret, var1, opt1, Nil, Nil);
}

void typeargs_var1opt2(addr *ret, addr var1, addr opt1, addr opt2)
{
	conscar_heap(&var1, var1);
	list_heap(&opt1, opt1, opt2, NULL);
	typeargs_full(ret, var1, opt1, Nil, Nil);
}

void typeargs_var1opt2key(addr *ret, addr var1, addr opt1, addr opt2, addr key)
{
	conscar_heap(&var1, var1);
	list_heap(&opt1, opt1, opt2, NULL);
	typeargs_full(ret, var1, opt1, Nil, key);
}

void typeargs_var2opt1(addr *ret, addr var1, addr var2, addr opt1)
{
	list_heap(&var1, var1, var2, NULL);
	conscar_heap(&opt1, opt1);
	typeargs_full(ret, var1, opt1, Nil, Nil);
}

void typeargs_var2opt2(addr *ret, addr var1, addr var2, addr opt1, addr opt2)
{
	list_heap(&var1, var1, var2, NULL);
	list_heap(&opt1, opt1, opt2, NULL);
	typeargs_full(ret, var1, opt1, Nil, Nil);
}

void typeargs_var2opt3(addr *ret, addr v1, addr v2, addr o1, addr o2, addr o3)
{
	list_heap(&v1, v1, v2, NULL);
	list_heap(&o1, o1, o2, o3, NULL);
	typeargs_full(ret, v1, o1, Nil, Nil);
}

void typeargs_var3opt1(addr *ret, addr var1, addr var2, addr var3, addr opt1)
{
	list_heap(&var1, var1, var2, var3, NULL);
	conscar_heap(&opt1, opt1);
	typeargs_full(ret, var1, opt1, Nil, Nil);
}

void typeargs_var4opt1(addr *ret, addr v1, addr v2, addr v3, addr v4, addr opt1)
{
	list_heap(&v1, v1, v2, v3, v4, NULL);
	conscar_heap(&opt1, opt1);
	typeargs_full(ret, v1, opt1, Nil, Nil);
}

void typeargs_var1rest(addr *ret, addr v1, addr rest)
{
	conscar_heap(&v1, v1);
	typeargs_full(ret, v1, Nil, rest, Nil);
}

void typeargs_var2rest(addr *ret, addr v1, addr v2, addr rest)
{
	list_heap(&v1, v1, v2, NULL);
	typeargs_full(ret, v1, Nil, rest, Nil);
}

void typeargs_var3rest(addr *ret, addr v1, addr v2, addr v3, addr rest)
{
	list_heap(&v1, v1, v2, v3, NULL);
	typeargs_full(ret, v1, Nil, rest, Nil);
}

void typeargs_var4rest(addr *ret, addr v1, addr v2, addr v3, addr v4, addr rest)
{
	list_heap(&v1, v1, v2, v3, v4, NULL);
	typeargs_full(ret, v1, Nil, rest, Nil);
}

void typeargs_opt1rest(addr *ret, addr opt1, addr rest)
{
	conscar_heap(&opt1, opt1);
	typeargs_full(ret, Nil, opt1, rest, Nil);
}

void typeargs_var1rest_allow(addr *ret, addr v1, addr rest)
{
	conscar_heap(&v1, v1);
	typeargs_full(ret, v1, Nil, rest, T);
}

void typeargs_var2rest_allow(addr *ret, addr v1, addr v2, addr rest)
{
	list_heap(&v1, v1, v2, NULL);
	typeargs_full(ret, v1, Nil, rest, T);
}

void typeargs_var3rest_allow(addr *ret, addr v1, addr v2, addr v3, addr rest)
{
	list_heap(&v1, v1, v2, v3, NULL);
	typeargs_full(ret, v1, Nil, rest, T);
}

void typeargs_var4rest_allow(addr *ret, addr v1, addr v2, addr v3, addr v4, addr rest)
{
	list_heap(&v1, v1, v2, v3, v4, NULL);
	typeargs_full(ret, v1, Nil, rest, T);
}

void typeargs_rest(addr *ret, addr rest)
{
	typeargs_full(ret, Nil, Nil, rest, Nil);
}

void typeargs_key(addr *ret, addr key)
{
	typeargs_full(ret, Nil, Nil, Nil, key);
}

void typeargs_method(addr pos)
{
	addr var, method1, method2;

	GetArrayA2(pos, 0, &var); /* var */
	GetTypeTable(&method1, Method1);
	GetTypeTable(&method2, Method2);
	lista_heap(&var, method1, method2, var, NULL);
	SetArrayA2(pos, 0, var); /* var */
}

void typeargs_methodkey(addr pos)
{
	addr rest;

	typeargs_method(pos);
	GetArrayA2(pos, 2, &rest); /* rest */
	if (rest == Nil) {
		GetTypeTable(&rest, T);
		SetArrayA2(pos, 2, rest); /* rest */
	}
}


/*
 *  values
 */
void typevalues_result(addr *ret, addr v1)
{
	/* (values v1 &rest nil) */
	addr pos;

	GetTypeTable(&pos, Nil);
	conscar_heap(&v1, v1);
	type_values_heap(v1, Nil, pos, Nil, ret);
	SetStatusReadOnly(*ret);
}

void typevalues_values2(addr *ret, addr v1, addr v2)
{
	/* (values v1 v2 &rest nil) */
	addr pos;

	GetTypeTable(&pos, Nil);
	list_heap(&v1, v1, v2, NULL);
	type_values_heap(v1, Nil, pos, Nil, ret);
	SetStatusReadOnly(*ret);
}

void typevalues_values3(addr *ret, addr v1, addr v2, addr v3)
{
	/* (values v1 v2 v3 &rest nil) */
	addr pos;

	GetTypeTable(&pos, Nil);
	list_heap(&v1, v1, v2, v3, NULL);
	type_values_heap(v1, Nil, pos, Nil, ret);
	SetStatusReadOnly(*ret);
}

void typevalues_values4(addr *ret, addr v1, addr v2, addr v3, addr v4)
{
	/* (values v1 v2 v3 v4 &rest nil) */
	addr pos;

	GetTypeTable(&pos, Nil);
	list_heap(&v1, v1, v2, v3, v4, NULL);
	type_values_heap(v1, Nil, pos, Nil, ret);
	SetStatusReadOnly(*ret);
}

void typevalues_values5(addr *ret, addr v1, addr v2, addr v3, addr v4, addr v5)
{
	/* (values v1 v2 v3 v4 v5 &rest nil) */
	addr pos;

	GetTypeTable(&pos, Nil);
	list_heap(&v1, v1, v2, v3, v4, v5, NULL);
	type_values_heap(v1, Nil, pos, Nil, ret);
	SetStatusReadOnly(*ret);
}

void typevalues_values_va(addr *ret, ...)
{
	/* (values ... &rest nil) */
	addr args, type;
	va_list va;

	/* args */
	va_start(va, ret);
	list_stdarg_alloc(NULL, &args, va);
	va_end(va);
	/* type */
	GetTypeTable(&type, Nil);
	type_values_heap(args, Nil, type, Nil, ret);
	SetStatusReadOnly(*ret);
}

void typevalues_rest(addr *ret, addr type)
{
	/* (values &rest type) */

	type_values_heap(Nil, Nil, type, Nil, ret);
	SetStatusReadOnly(*ret);
}

/* type asterisk */
void type1aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	CheckType(aster, LISPTYPE_TYPE);
	type1_alloc(local, type, aster, ret);
}

void type2aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	CheckType(aster, LISPTYPE_TYPE);
	type2_alloc(local, type, aster, aster, ret);
}

void type3aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	CheckType(aster, LISPTYPE_TYPE);
	type3_alloc(local, type, aster, aster, aster, ret);
}

void type4aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	CheckType(aster, LISPTYPE_TYPE);
	type4_alloc(local, type, aster, aster, aster, aster, ret);
}

void type1aster_local(LocalRoot local, enum LISPDECL type, addr *ret)
{
	CheckLocal(local);
	type1aster_alloc(local, type, ret);
}

void type2aster_local(LocalRoot local, enum LISPDECL type, addr *ret)
{
	CheckLocal(local);
	type2aster_alloc(local, type, ret);
}

void type3aster_local(LocalRoot local, enum LISPDECL type, addr *ret)
{
	CheckLocal(local);
	type3aster_alloc(local, type, ret);
}

void type4aster_local(LocalRoot local, enum LISPDECL type, addr *ret)
{
	CheckLocal(local);
	type4aster_alloc(local, type, ret);
}

void type1aster_heap(enum LISPDECL type, addr *ret)
{
	type1aster_alloc(NULL, type, ret);
}

void type2aster_heap(enum LISPDECL type, addr *ret)
{
	type2aster_alloc(NULL, type, ret);
}

void type3aster_heap(enum LISPDECL type, addr *ret)
{
	type3aster_alloc(NULL, type, ret);
}

void type4aster_heap(enum LISPDECL type, addr *ret)
{
	type4aster_alloc(NULL, type, ret);
}


/*
 *  and/or
 */
void type2and_alloc(LocalRoot local, addr a, addr b, addr *ret)
{
	enum LISPDECL decl;
	addr array;

	CheckType2(a, LISPTYPE_TYPE, "type left error");
	CheckType2(b, LISPTYPE_TYPE, "type right error");
	decl = LowLispDecl(a);
	if (decl == LISPDECL_ASTERISK || decl == LISPDECL_T) {
		*ret = b;
		return;
	}
	if (decl == LISPDECL_NIL) {
		GetTypeTable(ret, Nil);
		return;
	}
	decl = LowLispDecl(b);
	if (decl == LISPDECL_ASTERISK || decl == LISPDECL_T) {
		*ret = a;
		return;
	}
	if (decl == LISPDECL_NIL) {
		GetTypeTable(ret, Nil);
		return;
	}

	vector4_alloc(local, &array, 2);
	SetArrayA4(array, 0, a);
	SetArrayA4(array, 1, b);
	type1_alloc(local, LISPDECL_AND, array, ret);
}

void type2or_alloc(LocalRoot local, addr a, addr b, addr *ret)
{
	enum LISPDECL decl;
	addr array;

	CheckType2(a, LISPTYPE_TYPE, "type left error");
	CheckType2(b, LISPTYPE_TYPE, "type right error");
	decl = LowLispDecl(a);
	if (decl == LISPDECL_ASTERISK || decl == LISPDECL_T) {
		GetTypeTable(ret, T);
		return;
	}
	if (decl == LISPDECL_NIL) {
		*ret = b;
		return;
	}
	decl = LowLispDecl(b);
	if (decl == LISPDECL_ASTERISK || decl == LISPDECL_T) {
		GetTypeTable(ret, T);
		return;
	}
	if (decl == LISPDECL_NIL) {
		*ret = a;
		return;
	}

	vector4_alloc(local, &array, 2);
	SetArrayA4(array, 0, a);
	SetArrayA4(array, 1, b);
	type1_alloc(local, LISPDECL_OR, array, ret);
}

void type3and_alloc(LocalRoot local, addr a, addr b, addr c, addr *ret)
{
	addr array;

	vector4_alloc(local, &array, 3);
	SetArrayA4(array, 0, a);
	SetArrayA4(array, 1, b);
	SetArrayA4(array, 2, c);
	type1_alloc(local, LISPDECL_AND, array, ret);
}

void type3or_alloc(LocalRoot local, addr a, addr b, addr c, addr *ret)
{
	addr array;

	vector4_alloc(local, &array, 3);
	SetArrayA4(array, 0, a);
	SetArrayA4(array, 1, b);
	SetArrayA4(array, 2, c);
	type1_alloc(local, LISPDECL_OR, array, ret);
}

void type4or_alloc(LocalRoot local, addr a, addr b, addr c, addr d, addr *ret)
{
	addr array;

	vector4_alloc(local, &array, 4);
	SetArrayA4(array, 0, a);
	SetArrayA4(array, 1, b);
	SetArrayA4(array, 2, c);
	SetArrayA4(array, 3, d);
	type1_alloc(local, LISPDECL_OR, array, ret);
}

void type2and_local(LocalRoot local, addr a, addr b, addr *ret)
{
	CheckLocal(local);
	type2and_alloc(local, a, b, ret);
}

void type2or_local(LocalRoot local, addr a, addr b, addr *ret)
{
	CheckLocal(local);
	type2or_alloc(local, a, b, ret);
}

void type3and_local(LocalRoot local, addr a, addr b, addr c, addr *ret)
{
	CheckLocal(local);
	type3and_alloc(local, a, b, c, ret);
}

void type3or_local(LocalRoot local, addr a, addr b, addr c, addr *ret)
{
	CheckLocal(local);
	type3or_alloc(local, a, b, c, ret);
}

void type4or_local(LocalRoot local, addr a, addr b, addr c, addr d, addr *ret)
{
	CheckLocal(local);
	type4or_alloc(local, a, b, c, d, ret);
}

void type2and_heap(addr a, addr b, addr *ret)
{
	type2and_alloc(NULL, a, b, ret);
}

void type2or_heap(addr a, addr b, addr *ret)
{
	type2or_alloc(NULL, a, b, ret);
}

void type3and_heap(addr a, addr b, addr c, addr *ret)
{
	type3and_alloc(NULL, a, b, c, ret);
}

void type3or_heap(addr a, addr b, addr c, addr *ret)
{
	type3or_alloc(NULL, a, b, c, ret);
}

void type4or_heap(addr a, addr b, addr c, addr d, addr *ret)
{
	type4or_alloc(NULL, a, b, c, d, ret);
}


/*
 *  range
 */
void type1real_heap(enum LISPDECL type, addr value, addr *ret)
{
	type4_heap(type, Nil, value, Nil, value, ret);
}

void type4integer_heap(addr a, fixnum b, addr c, fixnum d, addr *ret)
{
	addr x, y;

	Check(a != Nil && a != T, "left1 error");
	Check(c != Nil && c != T, "right1 error");
	fixnum_heap(&x, b);
	fixnum_heap(&y, d);
	type4_heap(LISPDECL_INTEGER, a, x, c, y, ret);
}

void type2integer_ab_heap(addr a, fixnum b, addr *ret)
{
	addr x, aster;

	Check(a != Nil && a != T, "left1 error");
	GetTypeTable(&aster, Asterisk);
	fixnum_heap(&x, b);
	type4_heap(LISPDECL_INTEGER, a, x, aster, aster, ret);
}

void type2integer_cd_heap(addr c, fixnum d, addr *ret)
{
	addr y, aster;

	Check(c != Nil && c != T, "right1 error");
	GetTypeTable(&aster, Asterisk);
	fixnum_heap(&y, d);
	type4_heap(LISPDECL_INTEGER, aster, aster, c, y, ret);
}

static void type4declf_heap(enum LISPDECL type,
		addr a, float b, addr c, float d, addr *ret)
{
	addr x, y;

	Check(a != Nil && a != T, "left1 error");
	Check(c != Nil && c != T, "right1 error");
	single_float_heap(&x, b);
	single_float_heap(&y, d);
	type4_heap(type, a, x, c, y, ret);
}

static void type2declf_ab_heap(enum LISPDECL type, addr a, float b, addr *ret)
{
	addr x, aster;

	Check(a != Nil && a != T, "left1 error");
	GetTypeTable(&aster, Asterisk);
	single_float_heap(&x, b);
	type4_heap(type, a, x, aster, aster, ret);
}

static void type2declf_cd_heap(enum LISPDECL type, addr c, float d, addr *ret)
{
	addr y, aster;

	Check(c != Nil && c != T, "right1 error");
	GetTypeTable(&aster, Asterisk);
	single_float_heap(&y, d);
	type4_heap(type, aster, aster, c, y, ret);
}

void type4float_heap(addr a, float b, addr c, float d, addr *ret)
{
	type4declf_heap(LISPDECL_FLOAT, a, b, c, d, ret);
}

void type2float_ab_heap(addr a, float b, addr *ret)
{
	type2declf_ab_heap(LISPDECL_FLOAT, a, b, ret);
}

void type2float_cd_heap(addr c, float d, addr *ret)
{
	type2declf_cd_heap(LISPDECL_FLOAT, c, d, ret);
}

void type4realf_heap(addr a, float b, addr c, float d, addr *ret)
{
	type4declf_heap(LISPDECL_REAL, a, b, c, d, ret);
}

void type2realf_ab_heap(addr a, float b, addr *ret)
{
	type2declf_ab_heap(LISPDECL_REAL, a, b, ret);
}

void type2realf_cd_heap(addr c, float d, addr *ret)
{
	type2declf_cd_heap(LISPDECL_REAL, c, d, ret);
}


/*
 *  vector
 */
void type_vector1_heap(size_t size, addr *ret)
{
	addr first, second;

	GetTypeTable(&first, Asterisk);
	vector4_heap(&second, 1);
	SetArrayA4(second, 0, intsizeh(size));
	type2_heap(LISPDECL_ARRAY, first, second, ret);
}


/************************************************************
 *  type_typep.c
 ************************************************************/

static int typep_call_(Execute ptr, addr value, addr type, int asterisk, int *ret);
typedef int (*call_type_typep)(Execute ptr, addr value, addr type, int *ret);
static call_type_typep TypeTypep[LISPDECL_SIZE];

static int typep_invalid_(Execute ptr, addr value, addr type, int *ret)
{
	infobit(type);
	*ret = 0;
	return fmte_("Invalid type.", NULL);
}

static int typep_error_(Execute ptr, addr value, addr type, int *ret)
{
	Return(get_error_type_(ptr, type, &type));
	return typep_call_(ptr, value, type, 1, ret);
}

static int typep_type_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_TYPE);
	return 0;
}

static int typep_clos_(Execute ptr, addr value, addr type, int *ret)
{
	if (GetType(value) != LISPTYPE_CLOS)
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	if (type_asterisk_p(type))
		return Result(ret, 1);

	return clos_subtype_p_(value, type, ret);
}

static int typep_asterisk_(Execute ptr, addr value, addr type, int *ret)
{
	return Result(ret, 1);
}

static int typep_optimized_(Execute ptr, addr value, addr type, int *ret)
{
	GetArrayType(type, 0, &type);
	return typep_call_(ptr, value, type, 1, ret);
}


/*
 *  Compound-type
 */
static int typep_and_(Execute ptr, addr value, addr type, int *ret)
{
	int result;
	addr check;
	size_t i, size;

	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		Return(typep_call_(ptr, value, check, 1, &result));
		if (! result)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int typep_or_(Execute ptr, addr value, addr type, int *ret)
{
	int result;
	addr check;
	size_t i, size;

	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		Return(typep_call_(ptr, value, check, 1, &result));
		if (result)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int typep_eql_(Execute ptr, addr value, addr type, int *ret)
{
	GetArrayType(type, 0, &type);
	*ret = eql_function(value, type);
	return 0;
}

static int typep_member_(Execute ptr, addr value, addr type, int *ret)
{
	addr check;
	size_t i, size;

	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (eql_function(value, check))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int typep_mod_(Execute ptr, addr value, addr type, int *ret)
{
	int check;

	if (! integerp(value))
		return Result(ret, 0);
	Return(minusp_integer_(value, &check));
	if (check)
		return Result(ret, 0);
	GetArrayType(type, 0, &type);

	return less_integer_(value, type, ret);
}

static int typep_not_(Execute ptr, addr value, addr type, int *ret)
{
	int check;

	GetArrayType(type, 0, &type);
	Return(typep_call_(ptr, value, type, 1, &check));
	return Result(ret, ! check);
}

static int typep_satisfies_(Execute ptr, addr value, addr type, int *ret)
{
	GetArrayType(type, 0, &type);
	Return(callclang_funcall(ptr, &type, type, value, NULL));
	return Result(ret, (type != Nil));
}

static int typep_values_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = 0;
	return fmte_("The values type don't use in typep context.", NULL);
}


/*
 *  Extract-type
 */
static int typep_atom_(Execute ptr, addr value, addr type, int *ret)
{
	return Result(ret, (! IsCons(value)));
}

static int typep_list_(Execute ptr, addr value, addr type, int *ret)
{
	return Result(ret, IsList(value));
}

static int typep_boolean_(Execute ptr, addr value, addr type, int *ret)
{
	return Result(ret, (value == Nil || value == T));
}

static int typep_vector_vector_(addr value, addr type, int *ret)
{
	enum LISPDECL left;
	addr check;
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &check);
	GetLispDecl(check, &left);
	if ((left != LISPDECL_ASTERISK) && (left != LISPDECL_T))
		return Result(ret, 0);
	GetArrayType(type, 1, &check);
	if (type_asterisk_p(check))
		return Result(ret, 1);
	lenarray(value, &size1);
	GetFixnum(check, &size2);
	return Result(ret, (size1 == (size_t)size2));
}

static int typep_vector_string_(addr value, addr type, int *ret)
{
	enum LISPDECL decl;
	addr check;
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &check);
	GetLispDecl(check, &decl);
	if ((decl != LISPDECL_ASTERISK) && (! decl_character_p(decl)))
		return Result(ret, 0);
	GetArrayType(type, 1, &check);
	if (type_asterisk_p(check))
		return Result(ret, 1);
	string_length(value, &size1);
	GetFixnum(check, &size2);
	return Result(ret, (size1 == (size_t)size2));
}

static int typep_vector_bitvector_(addr value, addr type, int *ret)
{
	enum LISPDECL decl;
	addr check;
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &check);
	GetLispDecl(check, &decl);
	if ((decl != LISPDECL_ASTERISK) && (decl != LISPDECL_BIT))
		return Result(ret, 0);
	GetArrayType(type, 1, &check);
	if (type_asterisk_p(check))
		return Result(ret, 1);
	bitmemory_length(value, &size1);
	GetFixnum(check, &size2);
	return Result(ret, (size1 == (size_t)size2));
}

static int typep_vector_dimension_(addr value, addr type, int *ret)
{
	/* asterisk */
	if (type_asterisk_p(type)) {
		*ret = array_vector_p(value);
		return 0;
	}

	/* fixnum */
	if (GetType(type) == LISPTYPE_FIXNUM) {
		*ret = array_size_vector_p(value, (size_t)RefFixnum(type));
		return 0;
	}

	/* error */
	*ret = 0;
	return fmte_("type error", NULL);
}

static int typep_vector_array_(addr value, addr type, int *ret)
{
	addr left, right;

	GetArrayType(type, 0, &left);
	GetArrayInfo(value, ARRAY_INDEX_TYPE, &right);
	if ((! type_asterisk_p(left)) && (! upgraded_array0_equal(left, right)))
		return Result(ret, 0);
	GetArrayType(type, 1, &type);
	return typep_vector_dimension_(value, type, ret);
}

static int typep_vector_(Execute ptr, addr value, addr type, int *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_VECTOR:
			return typep_vector_vector_(value, type, ret);

		case LISPTYPE_STRING:
			return typep_vector_string_(value, type, ret);

		case LISPTYPE_ARRAY:
			if (strarrayp(value))
				return typep_vector_string_(value, type, ret);
			return typep_vector_array_(value, type, ret);

		case LISPTYPE_BITVECTOR:
			return typep_vector_bitvector_(value, type, ret);

		default:
			return Result(ret, 0);
	}
}

static int typep_simple_vector_vector_(addr value, addr type, int *ret)
{
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &type);
	if (type_asterisk_p(type))
		return Result(ret, 1);
	lenarray(value, &size1);
	GetFixnum(type, &size2);
	return Result(ret, (size1 == (size_t)size2));
}

static int typep_type_vector_array_(addr value, addr type, enum LISPDECL decl, int *ret)
{
	addr check;

	GetArrayInfo(value, ARRAY_INDEX_TYPE, &check);
	if ((! type_asterisk_p(check)) && (RefLispDecl(check) != decl))
		return Result(ret, 0);
	GetArrayType(type, 0, &check);
	return typep_vector_dimension_(value, check, ret);
}

static int typep_simple_vector_(Execute ptr, addr value, addr type, int *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_VECTOR:
			return typep_simple_vector_vector_(value, type, ret);

		case LISPTYPE_ARRAY:
			if (! array_simple_p(value))
				return Result(ret, 0);
			return typep_type_vector_array_(value, type, LISPDECL_T, ret);

		default:
			return Result(ret, 0);
	}
}

static int typep_bit_vector_bit_vector_(addr value, addr type, int *ret)
{
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &type);
	if (type_asterisk_p(type))
		return Result(ret, 1);
	bitmemory_length(value, &size1);
	GetFixnum(type, &size2);
	return Result(ret, (size1 == (size_t)size2));
}

static int typep_bit_vector_(Execute ptr, addr value, addr type, int *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_ARRAY:
			return typep_type_vector_array_(value, type, LISPDECL_BIT, ret);

		case LISPTYPE_BITVECTOR:
			return typep_bit_vector_bit_vector_(value, type, ret);

		default:
			return Result(ret, 0);
	}
}

static int typep_simple_bit_vector_(Execute ptr, addr value, addr type, int *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_ARRAY:
			if (! array_simple_p(value))
				return Result(ret, 0);
			return typep_type_vector_array_(value, type, LISPDECL_BIT, ret);

		case LISPTYPE_BITVECTOR:
			return typep_bit_vector_bit_vector_(value, type, ret);

		default:
			return Result(ret, 0);
	}
}

static int typep_extended_char_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = extended_char_p(value);
	return 0;
}

static int typep_string_size_(addr value, addr type, int *ret)
{
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &type);
	if (type_asterisk_p(type))
		return Result(ret, 1);
	string_length(value, &size1);
	GetFixnum(type, &size2);

	return Result(ret, (size1 == (size_t)size2));
}

static int typep_string_(Execute ptr, addr value, addr type, int *ret)
{
	if (! stringp(value))
		return Result(ret, 0);

	return typep_string_size_(value, type, ret);
}

static int typep_base_string_size_(addr value, addr type, int *ret)
{
	enum CHARACTER_TYPE check;

	Return(string_character_type_(value, &check));
	if (check == CHARACTER_TYPE_EMPTY ||
			check == CHARACTER_TYPE_STANDARD ||
			check == CHARACTER_TYPE_BASE) {
		return typep_string_size_(value, type, ret);
	}

	return Result(ret, 0);
}

static int typep_base_string_(Execute ptr, addr value, addr type, int *ret)
{
	if (! stringp(value))
		return Result(ret, 0);

	return typep_base_string_size_(value, type, ret);
}

static int typep_simple_string_(Execute ptr, addr value, addr type, int *ret)
{
	enum LISPTYPE check;

	check = GetType(value);
	if (check == LISPTYPE_STRING)
		return typep_string_size_(value, type, ret);
	if (strarrayp(value) && array_simple_p(value))
		return typep_string_size_(value, type, ret);

	return Result(ret, 0);
}

static int typep_simple_base_string_(Execute ptr, addr value, addr type, int *ret)
{
	enum LISPTYPE check;

	check = GetType(value);
	if (check == LISPTYPE_STRING)
		return typep_base_string_size_(value, type, ret);
	if (strarrayp(value) && array_simple_p(value))
		return typep_base_string_size_(value, type, ret);

	return Result(ret, 0);
}

static int typep_signed_byte_(Execute ptr, addr value, addr type, int *ret)
{
	addr check;
	enum LISPTYPE listtype;

	listtype = GetType(value);
	if (listtype == LISPTYPE_FIXNUM) {
		GetArrayType(type, 0, &check);
		if (type_asterisk_p(check))
			return Result(ret, 1);
		*ret = fixnum_signed_byte_p(value, RefFixnum(check));
		return 0;
	}

	if (listtype == LISPTYPE_BIGNUM) {
		GetArrayType(type, 0, &check);
		if (type_asterisk_p(check))
			return Result(ret, 1);
		*ret = bignum_signed_byte_p(value, RefFixnum(check));
		return 0;
	}

	return Result(ret, 0);
}

static int typep_unsigned_byte_(Execute ptr, addr value, addr type, int *ret)
{
	addr check;
	enum LISPTYPE listtype;

	listtype = GetType(value);
	if (listtype == LISPTYPE_FIXNUM) {
		GetArrayType(type, 0, &check);
		if (type_asterisk_p(check))
			return Result(ret, (0 <= RefFixnum(value)));
		*ret = fixnum_unsigned_byte_p(value, RefFixnum(check));
		return 0;
	}

	if (listtype == LISPTYPE_BIGNUM) {
		GetArrayType(type, 0, &check);
		if (type_asterisk_p(check))
			return Result(ret, zerop_or_plusp_bignum(value));
		*ret = bignum_unsigned_byte_p(value, RefFixnum(check));
		return 0;
	}

	return Result(ret, 0);
}

static int typep_bit_(Execute ptr, addr value, addr type, int *ret)
{
	enum LISPTYPE lisptype;
	fixnum check;

	lisptype = GetType(value);
	if (lisptype == LISPTYPE_FIXNUM) {
		GetFixnum(value, &check);
		return Result(ret, (check == 0 || check == 1));
	}

	if (lisptype == LISPTYPE_BIGNUM) {
		*ret = zerop_bignum(value) || equal_value_bignum(value, signplus_bignum, 1);
		return 0;
	}

	return Result(ret, 0);
}

static int fbf_bignum(fixnum left, addr value, fixnum right)
{
	return compare_value_bignum(left, value) <= 0
		&& compare_bignum_value(value, right) <= 0;
}

static int typep_fixnum_(Execute ptr, addr value, addr type, int *ret)
{
	enum LISPTYPE lisptype;

	lisptype = GetType(value);
	if (lisptype == LISPTYPE_FIXNUM)
		return Result(ret, 1);

	if (lisptype == LISPTYPE_BIGNUM) {
		*ret = fbf_bignum(FIXNUM_MIN, value, FIXNUM_MAX);
		return 0;
	}

	return Result(ret, 0);
}

static int typep_bignum_(Execute ptr, addr value, addr type, int *ret)
{
	enum LISPTYPE lisptype;

	lisptype = GetType(value);
	if (lisptype == LISPTYPE_FIXNUM)
		return Result(ret, 0);

	if (lisptype == LISPTYPE_BIGNUM) {
		*ret = (! fbf_bignum(FIXNUM_MIN, value, FIXNUM_MAX));
		return 0;
	}

	return Result(ret, 0);
}


/*
 *  Atomic-type
 */
static int typep_nil_(Execute ptr, addr value, addr type, int *ret)
{
	return Result(ret, 0);
}

static int typep_t_(Execute ptr, addr value, addr type, int *ret)
{
	return Result(ret, 1);
}

static int typep_null_(Execute ptr, addr value, addr type, int *ret)
{
	return Result(ret, (value == Nil));
}

static int typep_cons_(Execute ptr, addr value, addr type, int *ret)
{
	int result;
	addr left, check;

	if (! IsCons(value))
		return Result(ret, 0);
	GetCons(value, &left, &value);
	GetArrayType(type, 0, &check);
	Return(typep_call_(ptr, left, check, 1, &result));
	if (! result)
		return Result(ret, 0);
	GetArrayType(type, 1, &check);

	return typep_call_(ptr, value, check, 1, ret);
}

static int typep_hash_table_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_HASHTABLE);
	return 0;
}

static int typep_symbol_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = symbolp(value);
	return 0;
}

static int typep_keyword_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = keywordp(value);
	return 0;
}

static int typep_package_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_PACKAGE);
	return 0;
}

static int typep_random_state_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_RANDOM_STATE);
	return 0;
}

static int typep_readtable_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_READTABLE);
	return 0;
}

static int typep_function_check_(Execute ptr, addr value, addr right, int *ret)
{
	addr left;

	gettype_function(value, &left);
	if (left == Nil) {
		if (compiled_function_p(value))
			GetTypeTable(&left, CompiledFunction);
		else
			GetTypeTable(&left, Function);
	}
	return subtypep_check_(ptr, left, right, Nil, ret, NULL);
}

static int typep_function_(Execute ptr, addr value, addr type, int *ret)
{
	addr check;

	GetArrayType(type, 2, &check);
	if (check == Nil) {
		*ret = 0;
		return fmte_("The cons type (FUNCTION ? ?) don't accept.", NULL);
	}
	if (! functionp(value))
		return Result(ret, 0);

	return typep_function_check_(ptr, value, type, ret);
}

static int typep_compiled_function_(Execute ptr, addr value, addr type, int *ret)
{
	addr check;

	GetArrayType(type, 2, &check);
	if (check == Nil) {
		*ret = 0;
		return fmte_("The cons type (COMPILED-FUNCTION ? ?) don't accept.", NULL);
	}
	if (! compiled_function_p(value))
		return Result(ret, 0);

	return typep_function_check_(ptr, value, type, ret);
}

static int typep_pathname_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = pathnamep(value);
	return 0;
}

static int typep_logical_pathname_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = pathname_logical_p(value);
	return 0;
}

static int typep_sequence_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = sequencep(value);
	return 0;
}

static int equal_array_dimension(addr value, addr right)
{
	addr left, check;
	size_t i, rank, index, *psize;
	struct array_struct *str;

	/* rank check */
	GetArrayInfo(value, ARRAY_INDEX_DIMENSION, &left);
	str = ArrayInfoStruct(value);
	rank = ArrayInfoStruct(value)->dimension;
	LenArrayA4(right, &i);
	if (rank != i)
		return 0;

	/* no-dimension */
	if (rank == 0) {
		return 0;
	}

	/* sequence */
	if (rank == 1) {
		GetArrayA4(right, 0, &check);
		if (type_asterisk_p(check))
			return 1;
		if (GetIndex_integer(check, &index))
			return 0;
		return str->size == index;
	}

	/* multi-dimension */
	CheckType(left, LISPSYSTEM_ARRAY_DIMENSION);
	psize = arraysize_ptr(left);
	for (i = 0; i < rank; i++) {
		GetArrayA4(right, i, &check);
		if (type_asterisk_p(check))
			continue;
		if (GetIndex_integer(check, &index))
			return 0;
		if (psize[i] != index)
			return 0;
	}
	return 1;
}

static int typep_array_dimension(addr value, addr type)
{
	/* asterisk */
	if (type_asterisk_p(type)) {
		return 1;
	}

	/* fixnum */
	if (GetType(type) == LISPTYPE_FIXNUM) {
		return ArrayInfoStruct(value)->dimension == (size_t)RefFixnum(type);
	}

	/* arraydimension */
	if (GetType(type) == LISPTYPE_VECTOR) {
		return equal_array_dimension(value, type);
	}

	/* error */
	Abort("type error");
	return 0;
}

static int typep_array_array(addr value, addr type)
{
	addr left, right;

	GetArrayType(type, 0, &left);
	GetArrayInfo(value, ARRAY_INDEX_TYPE, &right);
	if ((! type_asterisk_p(left)) && (! upgraded_array0_equal(left, right)))
		return 0;
	GetArrayType(type, 1, &type);

	return typep_array_dimension(value, type);
}

static int equal_fixnum_index(addr left, size_t right)
{
	fixnum value;

	GetFixnum(left, &value);
	if (value < 0)
		return 0;
	return (size_t)value == right;
}

static int typep_array_vector(addr value, addr type)
{
	enum LISPDECL decl;
	addr left;
	size_t size;

	/* type */
	GetArrayType(type, 0, &left);
	GetLispDecl(left, &decl);
	if (decl != LISPDECL_ASTERISK && decl != LISPDECL_T)
		return 0;

	/* dimension */
	GetArrayType(type, 1, &left);
	if (type_asterisk_p(left))
		return 1;

	/* fixnum */
	if (GetType(left) == LISPTYPE_FIXNUM) {
		return RefFixnum(left) == 1;
	}

	/* vector */
	if (GetType(left) == LISPTYPE_VECTOR) {
		LenArrayA4(left, &size);
		if (size != 1)
			return 0;
		GetArrayA4(left, 0, &left);
		if (type_asterisk_p(left))
			return 1;
		return equal_fixnum_index(left, lenarrayr(value));
	}

	/* error */
	Abort("Invalid array type.");
	return 0;
}

static int typep_array_string(addr value, addr type)
{
	enum LISPDECL decl;
	addr left;
	size_t size;

	/* type */
	GetArrayType(type, 0, &left);
	GetLispDecl(left, &decl);
	if ((decl != LISPDECL_ASTERISK) && (! decl_character_p(decl)))
		return 0;

	/* dimension */
	GetArrayType(type, 1, &left);
	if (type_asterisk_p(left))
		return 1;

	/* fixnum */
	if (GetType(left) == LISPTYPE_FIXNUM) {
		return RefFixnum(left) == 1;
	}

	/* vector */
	if (GetType(left) == LISPTYPE_VECTOR) {
		LenArrayA4(left, &size);
		if (size != 1)
			return 0;
		GetArrayA4(left, 0, &left);
		if (type_asterisk_p(left))
			return 1;
		string_length(value, &size);
		return equal_fixnum_index(left, size);
	}

	/* error */
	Abort("Invalid array type.");
	return 0;
}

static int typep_array_bitvector(addr value, addr type)
{
	enum LISPDECL decl;
	addr left;
	size_t size;

	/* type */
	GetArrayType(type, 0, &left);
	GetLispDecl(left, &decl);
	if (decl != LISPDECL_ASTERISK && decl != LISPDECL_BIT)
		return 0;

	/* dimension */
	GetArrayType(type, 1, &left);
	if (type_asterisk_p(left))
		return 1;

	/* fixnum */
	if (GetType(left) == LISPTYPE_FIXNUM) {
		return RefFixnum(left) == 1;
	}

	/* vector */
	if (GetType(left) == LISPTYPE_VECTOR) {
		LenArrayA4(left, &size);
		if (size != 1)
			return 0;
		GetArrayA4(left, 0, &left);
		if (type_asterisk_p(left))
			return 1;
		bitmemory_length(value, &size);
		return equal_fixnum_index(left, size);
	}

	/* error */
	Abort("Invalid array type.");
	return 0;
}

static int typep_array_result(addr value, addr type)
{
	switch (GetType(value)) {
		case LISPTYPE_ARRAY:
			return typep_array_array(value, type);

		case LISPTYPE_VECTOR:
			return typep_array_vector(value, type);

		case LISPTYPE_STRING:
			return typep_array_string(value, type);

		case LISPTYPE_BITVECTOR:
			return typep_array_bitvector(value, type);

		default:
			break;
	}

	return 0;
}

static int typep_array_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = typep_array_result(value, type);
	return 0;
}

static int typep_simple_array_result(addr value, addr type)
{
	switch (GetType(value)) {
		case LISPTYPE_ARRAY:
			return array_simple_p(value) && typep_array_array(value, type);

		case LISPTYPE_VECTOR:
			return typep_array_vector(value, type);

		case LISPTYPE_STRING:
			return typep_array_string(value, type);

		case LISPTYPE_BITVECTOR:
			return typep_array_bitvector(value, type);

		default:
			break;
	}

	return 0;
}

static int typep_simple_array_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = typep_simple_array_result(value, type);
	return 0;
}

static int typep_character_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_CHARACTER);
	return 0;
}

static int typep_base_char_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = base_char_p(value);
	return 0;
}

static int typep_standard_char_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = standard_char_p(value);
	return 0;
}

static int typep_number_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = numberp(value);
	return 0;
}

static int typep_ratio_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_RATIO);
	return 0;
}

static int typep_complex_(Execute ptr, addr value, addr type, int *ret)
{
	int result;
	addr check;

	if (GetType(value) != LISPTYPE_COMPLEX)
		return Result(ret, 0);
	GetRealComplex(value, &check);
	GetArrayType(type, 0, &type);
	Return(typep_call_(ptr, check, type, 1, &result));
	if (! result)
		return Result(ret, 0);
	GetImagComplex(value, &check);
	return typep_call_(ptr, check, type, 1, ret);
}


/*
 *  range
 */
static int less_mode_nolocal_(addr mode, addr left, addr right, int *ret,
		int (*call_less_)(addr, addr, int *),
		int (*call_less_equal_)(addr, addr, int *))
{
	if (type_asterisk_p(mode))
		return Result(ret, 1);
	if (mode == Nil)
		return (*call_less_equal_)(left, right, ret);
	else
		return (*call_less_)(left, right, ret);
}

static int typep_range_nolocal_(addr value, addr type, int *ret,
		int (*typecheck)(addr),
		int (*call_less_)(addr, addr, int *),
		int (*call_less_equal_)(addr, addr, int *))
{
	int check;
	addr mode, pos;

	/* type */
	if (! (*typecheck)(value))
		return Result(ret, 0);

	/* left */
	GetArrayType(type, 0, &mode);
	GetArrayType(type, 1, &pos);
	Return(less_mode_nolocal_(mode, pos, value, &check,
				call_less_, call_less_equal_));
	if (! check)
		return Result(ret, 0);

	/* right */
	GetArrayType(type, 2, &mode);
	GetArrayType(type, 3, &pos);
	return less_mode_nolocal_(mode, value, pos, ret,
			call_less_, call_less_equal_);
}

static int less_mode_local_(LocalRoot local,
		addr mode, addr left, addr right, int *ret,
		int (*call_less_)(LocalRoot, addr, addr, int *),
		int (*call_less_equal_)(LocalRoot, addr, addr, int *))
{
	if (type_asterisk_p(mode))
		return Result(ret, 1);
	if (mode == Nil)
		return (*call_less_equal_)(local, left, right, ret);
	else
		return (*call_less_)(local, left, right, ret);
}

static int typep_range_local_(LocalRoot local, addr value, addr type, int *ret,
		int (*typecheck)(addr),
		int (*call_less_)(LocalRoot, addr, addr, int *),
		int (*call_less_equal_)(LocalRoot, addr, addr, int *))
{
	int check;
	addr mode, pos;

	/* type */
	if (! (*typecheck)(value))
		return Result(ret, 0);

	/* left */
	GetArrayType(type, 0, &mode);
	GetArrayType(type, 1, &pos);
	Return(less_mode_local_(local, mode, pos, value, &check,
				call_less_, call_less_equal_));
	if (! check)
		return Result(ret, 0);

	/* right */
	GetArrayType(type, 2, &mode);
	GetArrayType(type, 3, &pos);
	return less_mode_local_(local, mode, value, pos, ret,
			call_less_, call_less_equal_);
}

static int typep_integer_(Execute ptr, addr value, addr type, int *ret)
{
	return typep_range_nolocal_(value, type, ret,
			integerp,
			less_integer_,
			less_equal_integer_);
}

static int typep_rational_(Execute ptr, addr value, addr type, int *ret)
{
	return typep_range_local_(ptr->local, value, type, ret,
			rationalp,
			less_rational_,
			less_equal_rational_);
}

static int typep_real_(Execute ptr, addr value, addr type, int *ret)
{
	return typep_range_local_(ptr->local, value, type, ret,
			realp,
			less_real_,
			less_equal_real_);
}

static int typep_float_(Execute ptr, addr value, addr type, int *ret)
{
	return typep_range_nolocal_(value, type, ret,
			floatp,
			less_float_clang_,
			less_equal_float_clang_);
}

static int single_float_p_clang(addr value)
{
	return (GetType(value) == LISPTYPE_SINGLE_FLOAT);
}
static int typep_single_float_(Execute ptr, addr value, addr type, int *ret)
{
	return typep_range_nolocal_(value, type, ret,
			single_float_p_clang,
			less_float_clang_,
			less_equal_float_clang_);
}

static int double_float_p_clang(addr value)
{
	return (GetType(value) == LISPTYPE_DOUBLE_FLOAT);
}
static int typep_double_float_(Execute ptr, addr value, addr type, int *ret)
{
	return typep_range_nolocal_(value, type, ret,
			double_float_p_clang,
			less_float_clang_,
			less_equal_float_clang_);
}

static int long_float_p_clang(addr value)
{
	return (GetType(value) == LISPTYPE_LONG_FLOAT);
}
static int typep_long_float_(Execute ptr, addr value, addr type, int *ret)
{
	return typep_range_nolocal_(value, type, ret,
			long_float_p_clang,
			less_float_clang_,
			less_equal_float_clang_);
}

static int typep_restart_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_RESTART);
	return 0;
}

static int typep_environment_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_ENVIRONMENT);
	return 0;
}

static int typep_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = streamp(value);
	return 0;
}

static int typep_broadcast_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = broadcast_stream_p(value);
	return 0;
}

static int typep_concatenated_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = concatenated_stream_p(value);
	return 0;
}

static int typep_echo_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = echo_stream_p(value);
	return 0;
}

static int typep_file_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = file_stream_p(value);
	return 0;
}

static int typep_string_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = string_stream_p(value);
	return 0;
}

static int typep_synonym_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = synonym_stream_p(value);
	return 0;
}

static int typep_two_way_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = twoway_stream_p(value);
	return 0;
}

static int typep_prompt_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = prompt_stream_p(value);
	return 0;
}

static int typep_pretty_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = pretty_stream_p(value);
	return 0;
}

static int typep_memory_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = memory_stream_p(value);
	return 0;
}

static int typep_byte_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_BYTESPEC);
	return 0;
}

static int typep_print_dispatch_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_PRINT_DISPATCH);
	return 0;
}

static int typep_eval_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_EVAL);
	return 0;
}


/*
 *  typep-clang
 */
int typep_table_(Execute ptr, addr value, addr type, int *ret)
{
	call_type_typep call;

	CheckType(type, LISPTYPE_TYPE);
	call = TypeTypep[(int)RefLispDecl(type)];
	Check(call == NULL, "build error");
	return (*call)(ptr, value, type, ret);
}

void init_type_typep(void)
{
	int i;

	for (i = 0; i < LISPDECL_SIZE; i++)
		TypeTypep[i] = typep_invalid_;

	TypeTypep[LISPDECL_ERROR] = typep_error_;
	TypeTypep[LISPDECL_TYPE] = typep_type_;
	TypeTypep[LISPDECL_CLOS] = typep_clos_;
	TypeTypep[LISPDECL_ASTERISK] = typep_asterisk_;
	TypeTypep[LISPDECL_OPTIMIZED] = typep_optimized_;
	TypeTypep[LISPDECL_SUBTYPEP] = typep_optimized_;
	/* Compound-type */
	TypeTypep[LISPDECL_AND] = typep_and_;
	TypeTypep[LISPDECL_OR] = typep_or_;
	TypeTypep[LISPDECL_EQL] = typep_eql_;
	TypeTypep[LISPDECL_MEMBER] = typep_member_;
	TypeTypep[LISPDECL_MOD] = typep_mod_;
	TypeTypep[LISPDECL_NOT] = typep_not_;
	TypeTypep[LISPDECL_SATISFIES] = typep_satisfies_;
	TypeTypep[LISPDECL_VALUES] = typep_values_;
	/* Extract-type */
	TypeTypep[LISPDECL_ATOM] = typep_atom_;
	TypeTypep[LISPDECL_LIST] = typep_list_;
	TypeTypep[LISPDECL_BOOLEAN] = typep_boolean_;
	TypeTypep[LISPDECL_VECTOR] = typep_vector_;
	TypeTypep[LISPDECL_SIMPLE_VECTOR] = typep_simple_vector_;
	TypeTypep[LISPDECL_BIT_VECTOR] = typep_bit_vector_;
	TypeTypep[LISPDECL_SIMPLE_BIT_VECTOR] = typep_simple_bit_vector_;
	TypeTypep[LISPDECL_EXTENDED_CHAR] = typep_extended_char_;
	TypeTypep[LISPDECL_STRING] = typep_string_;
	TypeTypep[LISPDECL_BASE_STRING] = typep_base_string_;
	TypeTypep[LISPDECL_SIMPLE_STRING] = typep_simple_string_;
	TypeTypep[LISPDECL_SIMPLE_BASE_STRING] = typep_simple_base_string_;
	TypeTypep[LISPDECL_SIGNED_BYTE] = typep_signed_byte_;
	TypeTypep[LISPDECL_UNSIGNED_BYTE] = typep_unsigned_byte_;
	TypeTypep[LISPDECL_BIT] = typep_bit_;
	TypeTypep[LISPDECL_FIXNUM] = typep_fixnum_;
	TypeTypep[LISPDECL_BIGNUM] = typep_bignum_;
	/* Atomic-type */
	TypeTypep[LISPDECL_NIL] = typep_nil_;
	TypeTypep[LISPDECL_T] = typep_t_;
	TypeTypep[LISPDECL_NULL] = typep_null_;
	TypeTypep[LISPDECL_CONS] = typep_cons_;
	TypeTypep[LISPDECL_HASH_TABLE] = typep_hash_table_;
	TypeTypep[LISPDECL_SYMBOL] = typep_symbol_;
	TypeTypep[LISPDECL_KEYWORD] = typep_keyword_;
	TypeTypep[LISPDECL_PACKAGE] = typep_package_;
	TypeTypep[LISPDECL_RANDOM_STATE] = typep_random_state_;
	TypeTypep[LISPDECL_READTABLE] = typep_readtable_;
	TypeTypep[LISPDECL_FUNCTION] = typep_function_;
	TypeTypep[LISPDECL_COMPILED_FUNCTION] = typep_compiled_function_;
	TypeTypep[LISPDECL_PATHNAME] = typep_pathname_;
	TypeTypep[LISPDECL_LOGICAL_PATHNAME] = typep_logical_pathname_;
	TypeTypep[LISPDECL_SEQUENCE] = typep_sequence_;
	TypeTypep[LISPDECL_ARRAY] = typep_array_;
	TypeTypep[LISPDECL_SIMPLE_ARRAY] = typep_simple_array_;
	TypeTypep[LISPDECL_CHARACTER] = typep_character_;
	TypeTypep[LISPDECL_BASE_CHAR] = typep_base_char_;
	TypeTypep[LISPDECL_STANDARD_CHAR] = typep_standard_char_;
	TypeTypep[LISPDECL_NUMBER] = typep_number_;
	TypeTypep[LISPDECL_REAL] = typep_real_;
	TypeTypep[LISPDECL_RATIO] = typep_ratio_;
	TypeTypep[LISPDECL_INTEGER] = typep_integer_;
	TypeTypep[LISPDECL_RATIONAL] = typep_rational_;
	TypeTypep[LISPDECL_COMPLEX] = typep_complex_;
	TypeTypep[LISPDECL_FLOAT] = typep_float_;
	TypeTypep[LISPDECL_SHORT_FLOAT] = typep_single_float_;
	TypeTypep[LISPDECL_SINGLE_FLOAT] = typep_single_float_;
	TypeTypep[LISPDECL_DOUBLE_FLOAT] = typep_double_float_;
	TypeTypep[LISPDECL_LONG_FLOAT] = typep_long_float_;
	TypeTypep[LISPDECL_RESTART] = typep_restart_;
	TypeTypep[LISPDECL_ENVIRONMENT] = typep_environment_;
	TypeTypep[LISPDECL_STREAM] = typep_stream_;
	TypeTypep[LISPDECL_BROADCAST_STREAM] = typep_broadcast_stream_;
	TypeTypep[LISPDECL_CONCATENATED_STREAM] = typep_concatenated_stream_;
	TypeTypep[LISPDECL_ECHO_STREAM] = typep_echo_stream_;
	TypeTypep[LISPDECL_FILE_STREAM] = typep_file_stream_;
	TypeTypep[LISPDECL_STRING_STREAM] = typep_string_stream_;
	TypeTypep[LISPDECL_SYNONYM_STREAM] = typep_synonym_stream_;
	TypeTypep[LISPDECL_TWO_WAY_STREAM] = typep_two_way_stream_;
	TypeTypep[LISPDECL_PROMPT_STREAM] = typep_prompt_stream_;
	TypeTypep[LISPDECL_PRETTY_STREAM] = typep_pretty_stream_;
	TypeTypep[LISPDECL_MEMORY_STREAM] = typep_memory_stream_;
	TypeTypep[LISPDECL_BYTESPEC] = typep_byte_;
	TypeTypep[LISPDECL_PRINT_DISPATCH] = typep_print_dispatch_;
	TypeTypep[LISPDECL_EVAL] = typep_eval_;
}

static int typep_call_(Execute ptr, addr value, addr type, int asterisk, int *ret)
{
	int result;
	LocalHold hold;

	if ((! asterisk) && type_asterisk_p(type))
		return fmte_("typep don't allow to be asterisk *.", NULL);
	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, value, type, NULL);
	Return(typep_table_(ptr, value, type, &result));
	*ret = RefNotDecl(type)? (! result): result;
	localhold_end(hold);

	return 0;
}

int typep_clang_(Execute ptr, addr value, addr type, int *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	return typep_call_(ptr, value, type, 0, ret);
}

int typep_asterisk_clang_(Execute ptr, addr value, addr type, int *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	return typep_call_(ptr, value, type, 1, ret);
}


/************************************************************
 *  type_upgraded.c
 ************************************************************/

/*
 *  build
 */
#define SetConstCommon(a) { \
	addr __pos; \
	GetConst(COMMON_##a, &__pos); \
	SetConst(ARRAY_##a, __pos); \
}
#define SetConstCommonN(a,b,n) { \
	addr __pos, __value; \
	GetConst(COMMON_##a, &__pos); \
	fixnum_heap(&__value, n); \
	list_heap(&__pos, __pos, __value, NULL); \
	SetStatusReadOnly(__pos); \
	SetConst(ARRAY_##b##n, __pos); \
}
void build_type_upgraded(void)
{
	SetConstCommon(T);
	SetConstCommon(BIT);
	SetConstCommon(CHARACTER);
	SetConstCommon(SINGLE_FLOAT);
	SetConstCommon(DOUBLE_FLOAT);
	SetConstCommon(LONG_FLOAT);
	SetConstCommonN(SIGNED_BYTE, SIGNED, 8);
	SetConstCommonN(SIGNED_BYTE, SIGNED, 16);
	SetConstCommonN(SIGNED_BYTE, SIGNED, 32);
	SetConstCommonN(UNSIGNED_BYTE, UNSIGNED, 8);
	SetConstCommonN(UNSIGNED_BYTE, UNSIGNED, 16);
	SetConstCommonN(UNSIGNED_BYTE, UNSIGNED, 32);
#ifdef LISP_64BIT
	SetConstCommonN(SIGNED_BYTE, SIGNED, 64);
	SetConstCommonN(UNSIGNED_BYTE, UNSIGNED, 64);
#endif
}


/*
 *  upgraded-array-element-type
 */
int upgraded_array0_equal(addr left, addr right)
{
	enum LISPDECL decl;

	decl = LowLispDecl(left);
	if (decl != LowLispDecl(right)) {
		return 0;
	}
	if (decl == LISPDECL_SIGNED_BYTE || decl == LISPDECL_UNSIGNED_BYTE) {
		GetArrayType(left, 0, &left);
		GetArrayType(right, 0, &right);
		CheckType(left, LISPTYPE_FIXNUM);
		CheckType(right, LISPTYPE_FIXNUM);
		return RefFixnum(left) == RefFixnum(right);
	}

	return 1;
}

static int upgraded_array_unsigned(bigtype value)
{
	if (value <= UINT8_MAX) return 8;
	if (value <= UINT16_MAX) return 16;
#ifdef LISP_64BIT
	if (value <= UINT32_MAX) return 32;
	return 64;
#else
	return 32;
#endif
}

static int upgraded_array_signed(int sign, bigtype value)
{
	if (IsPlus(sign)) {
		if (value <= INT8_MAX) return 8;
		if (value <= INT16_MAX) return 16;
		if (value <= INT32_MAX) return 32;
#ifdef LISP_64BIT
		if (value <= INT64_MAX) return 64;
#endif
	}
	else {
		if (value <= ((bigtype)INT8_MAX) + 1UL) return 8;
		if (value <= ((bigtype)INT16_MAX) + 1UL) return 16;
		if (value <= ((bigtype)INT32_MAX) + 1UL) return 32;
#ifdef LISP_64BIT
		if (value <= ((bigtype)INT64_MAX) + 1ULL) return 64;
#endif
	}
	return 0;
}

static enum ARRAY_TYPE upgraded_array_integer(addr type, int *size)
{
	int sign1, sign2, size1, size2;
	addr left1, left2, right1, right2;
	bigtype value1, value2;

	/* asterisk check */
	GetArrayType(type, 0, &left1);
	if (type_asterisk_p(left1))
		return ARRAY_TYPE_T;
	GetArrayType(type, 2, &right1);
	if (type_asterisk_p(right1))
		return ARRAY_TYPE_T;

	/* left */
	GetArrayType(type, 1, &left2);
	if (castfixed_integer(left2, &sign1, &value1))
		return ARRAY_TYPE_T;
	if (left1 == T)
		value1++;

	/* right */
	GetArrayType(type, 3, &right2);
	if (castfixed_integer(right2, &sign2, &value2))
		return ARRAY_TYPE_T;
	if (right1 == T)
		value2--;

	/* value */
	if (IsPlus(sign1)) {
		if (value1 == 0 && value2 == 1)
			return ARRAY_TYPE_BIT;
		size1 = upgraded_array_unsigned(value1);
		size2 = upgraded_array_unsigned(value2);
		*size = (size1 < size2)? size2: size1;
		return ARRAY_TYPE_UNSIGNED;
	}
	else {
		size1 = upgraded_array_signed(sign1, value1);
		size2 = upgraded_array_signed(sign2, value2);
		if (size1 == 0 || size2 == 0)
			return ARRAY_TYPE_T;
		*size = (size1 < size2)? size2: size1;
		return ARRAY_TYPE_SIGNED;
	}
}

static enum ARRAY_TYPE upgraded_array_decl(addr type, int *size)
{
	/* not */
	if (RefNotDecl(type))
		return ARRAY_TYPE_T;
	/* upgraded */
	switch (LowLispDecl(type)) {
		case LISPDECL_CHARACTER:
		case LISPDECL_BASE_CHAR:
		case LISPDECL_STANDARD_CHAR:
			return ARRAY_TYPE_CHARACTER;

		case LISPDECL_INTEGER:
			return upgraded_array_integer(type, size);

		case LISPDECL_SINGLE_FLOAT:
			return ARRAY_TYPE_SINGLE_FLOAT;

		case LISPDECL_DOUBLE_FLOAT:
			return ARRAY_TYPE_DOUBLE_FLOAT;

		case LISPDECL_LONG_FLOAT:
			return ARRAY_TYPE_LONG_FLOAT;

		default:
			break;
	}

	return ARRAY_TYPE_T;
}

static int upgraded_array_optimize_(LocalRoot local,
		addr type, enum ARRAY_TYPE *ret, int *size)
{
	int ignore;
	LocalStack stack;

	CheckType(type, LISPTYPE_TYPE);
	/* local */
	push_local(local, &stack);
	/* upgraded-array */
	Return(type_optimize_local_(local, type, &type, &ignore));
	Check(! type_optimized_p(type), "optimize error");
	get_type_optimized(&type, type);
	*size = 0;
	*ret = upgraded_array_decl(type, size);
	/* free */
	rollback_local(local, stack);

	return 0;
}

int upgraded_array_value_(addr type, enum ARRAY_TYPE *ret, int *size)
{
	return upgraded_array_optimize_(Local_Thread, type, ret, size);
}

static void upgraded_array_type_signed(int size, addr *ret)
{
	switch (size) {
		case 8:
			GetTypeTable(ret, Array_Signed8);
			break;

		case 16:
			GetTypeTable(ret, Array_Signed16);
			break;

		case 32:
			GetTypeTable(ret, Array_Signed32);
			break;

#ifdef LISP_64BIT
		case 64:
			GetTypeTable(ret, Array_Signed64);
			break;
#endif
		default:
			GetTypeTable(ret, Array_T);
			break;
	}
}

static void upgraded_array_type_unsigned(int size, addr *ret)
{
	switch (size) {
		case 8:
			GetTypeTable(ret, Array_Unsigned8);
			break;

		case 16:
			GetTypeTable(ret, Array_Unsigned16);
			break;

		case 32:
			GetTypeTable(ret, Array_Unsigned32);
			break;

#ifdef LISP_64BIT
		case 64:
			GetTypeTable(ret, Array_Unsigned64);
			break;
#endif
		default:
			GetTypeTable(ret, Array_T);
			break;
	}
}

void upgraded_array_object(enum ARRAY_TYPE type, int size, addr *ret)
{
	switch (type) {
		case ARRAY_TYPE_BIT:
			GetTypeTable(ret, Array_Bit);
			break;

		case ARRAY_TYPE_CHARACTER:
			GetTypeTable(ret, Array_Character);
			break;

		case ARRAY_TYPE_SIGNED:
			upgraded_array_type_signed(size, ret);
			break;

		case ARRAY_TYPE_UNSIGNED:
			upgraded_array_type_unsigned(size, ret);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			GetTypeTable(ret, Array_SingleFloat);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			GetTypeTable(ret, Array_DoubleFloat);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			GetTypeTable(ret, Array_LongFloat);
			break;

		default:
			GetTypeTable(ret, Array_T);
			break;
	}
}

static int type_upgraded_type_local_(LocalRoot local, addr type, addr *ret)
{
	enum ARRAY_TYPE value;
	int size;

	CheckType(type, LISPTYPE_TYPE);
	size = 0;
	Return(upgraded_array_optimize_(local, type, &value, &size));
	upgraded_array_object(value, size, ret);

	return 0;
}

int upgraded_array_type_(addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	return type_upgraded_type_local_(Local_Thread, type, ret);
}

static void upgraded_array_const_signed(int size, addr *ret)
{
	switch (size) {
		case 8:
			GetConst(ARRAY_SIGNED8, ret);
			break;

		case 16:
			GetConst(ARRAY_SIGNED16, ret);
			break;

		case 32:
			GetConst(ARRAY_SIGNED32, ret);
			break;

#ifdef LISP_64BIT
		case 64:
			GetConst(ARRAY_SIGNED64, ret);
			break;
#endif
		default:
			GetConst(ARRAY_T, ret);
			break;
	}
}

static void upgraded_array_const_unsigned(int size, addr *ret)
{
	switch (size) {
		case 8:
			GetConst(ARRAY_UNSIGNED8, ret);
			break;

		case 16:
			GetConst(ARRAY_UNSIGNED16, ret);
			break;

		case 32:
			GetConst(ARRAY_UNSIGNED32, ret);
			break;

#ifdef LISP_64BIT
		case 64:
			GetConst(ARRAY_UNSIGNED64, ret);
			break;
#endif
		default:
			GetConst(ARRAY_T, ret);
			break;
	}
}

void upgraded_array_const(enum ARRAY_TYPE type, int size, addr *ret)
{
	switch (type) {
		case ARRAY_TYPE_BIT:
			GetConst(ARRAY_BIT, ret);
			break;

		case ARRAY_TYPE_CHARACTER:
			GetConst(ARRAY_CHARACTER, ret);
			break;

		case ARRAY_TYPE_SIGNED:
			upgraded_array_const_signed(size, ret);
			break;

		case ARRAY_TYPE_UNSIGNED:
			upgraded_array_const_unsigned(size, ret);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			GetConst(ARRAY_SINGLE_FLOAT, ret);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			GetConst(ARRAY_DOUBLE_FLOAT, ret);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			GetConst(ARRAY_LONG_FLOAT, ret);
			break;

		default:
			GetConst(ARRAY_T, ret);
			break;
	}
}

int upgraded_array_common(Execute ptr, addr env, addr pos, addr *ret)
{
	int size;
	enum ARRAY_TYPE type;

	if (env == Unbound)
		env = Nil;
	Return(parse_type(ptr, &pos, pos, env));
	Return(upgraded_array_optimize_(ptr->local, pos, &type, &size));
	upgraded_array_const(type, size, ret);

	return 0;
}

/* make local */
void upgraded_array_t_local(LocalRoot local, addr *ret)
{
	type0_local(local, LISPDECL_T, ret);
}

void upgraded_array_bit_local(LocalRoot local, addr *ret)
{
	type0_local(local, LISPDECL_BIT, ret);
}

void upgraded_array_character_local(LocalRoot local, addr *ret)
{
	type0_local(local, LISPDECL_CHARACTER, ret);
}


/*
 *  upgraded-complex-part-type
 */
int upgraded_complex_type_(Execute ptr, addr env, addr type, addr *ret)
{
	int value;
	addr right;

	CheckType(type, LISPTYPE_TYPE);
	/* integer */
	GetTypeTable(&right, Integer);
	Return(subtypep_check_(ptr, type, right, env, &value, NULL));
	if (value)
		return Result(ret, right);

	/* rational */
	GetTypeTable(&right, Rational);
	Return(subtypep_check_(ptr, type, right, env, &value, NULL));
	if (value)
		return Result(ret, right);

	/* single-float */
	GetTypeTable(&right, SingleFloat);
	Return(subtypep_check_(ptr, type, right, env, &value, NULL));
	if (value)
		return Result(ret, right);

	/* double-float */
	GetTypeTable(&right, DoubleFloat);
	Return(subtypep_check_(ptr, type, right, env, &value, NULL));
	if (value)
		return Result(ret, right);

	/* long-float */
	GetTypeTable(&right, LongFloat);
	Return(subtypep_check_(ptr, type, right, env, &value, NULL));
	if (value)
		return Result(ret, right);

	/* Real */
	GetTypeTable(&right, Real);
	Return(subtypep_check_(ptr, type, right, env, &value, NULL));
	if (value) {
		GetTypeTable(ret, SingleFloat); /* single-float */
		return 0;
	}

	/* error */
	*ret = 0;
	Return(type_object_(&type, type));
	return fmte_("COMPLEX type ~S must be a subtype of a real.", type, NULL);
}

static int upgraded_complex_const_(Execute ptr, addr env, addr pos, addr *ret)
{
	int value;
	addr right;

	CheckType(pos, LISPTYPE_TYPE);
	/* integer */
	GetTypeTable(&right, Integer);
	Return(subtypep_check_(ptr, pos, right, env, &value, NULL));
	if (value) {
		GetConst(COMMON_INTEGER, ret);
		return 0;
	}

	/* rational */
	GetTypeTable(&right, Rational);
	Return(subtypep_check_(ptr, pos, right, env, &value, NULL));
	if (value) {
		GetConst(COMMON_RATIONAL, ret);
		return 0;
	}

	/* single-float */
	GetTypeTable(&right, SingleFloat);
	Return(subtypep_check_(ptr, pos, right, env, &value, NULL));
	if (value) {
		GetConst(COMMON_SINGLE_FLOAT, ret);
		return 0;
	}

	/* double-float */
	GetTypeTable(&right, DoubleFloat);
	Return(subtypep_check_(ptr, pos, right, env, &value, NULL));
	if (value) {
		GetConst(COMMON_DOUBLE_FLOAT, ret);
		return 0;
	}

	/* long-float */
	GetTypeTable(&right, LongFloat);
	Return(subtypep_check_(ptr, pos, right, env, &value, NULL));
	if (value) {
		GetConst(COMMON_LONG_FLOAT, ret);
		return 0;
	}

	/* short-float */
	GetTypeTable(&right, Real);
	Return(subtypep_check_(ptr, pos, right, env, &value, NULL));
	if (value) {
		GetConst(COMMON_SINGLE_FLOAT, ret); /* single-float */
		return 0;
	}

	/* error */
	*ret = 0;
	Return(type_object_(&pos, pos));
	return fmte_("COMPLEX type ~S must be a subtype of a real.", pos, NULL);
}

int upgraded_complex_common(Execute ptr, addr env, addr pos, addr *ret)
{
	Return(parse_type(ptr, &pos, pos, env));
	Return(upgraded_complex_const_(ptr, env, pos, ret));
	CheckType(*ret, LISPTYPE_SYMBOL);
	return 0;
}


/************************************************************
 *  type_value.c
 ************************************************************/

typedef int (*type_value_call)(addr *, addr);
static type_value_call TypeValueTable[LISPTYPE_SIZE];

/* nil */
void type_value_nil(addr *ret)
{
	GetTypeTable(ret, Null);
}

static int type_value_nil_(addr *ret, addr value)
{
	GetTypeTable(ret, Null);
	return 0;
}

/* t */
void type_value_t(addr *ret)
{
	GetTypeTable(ret, Boolean);
}

static int type_value_t_(addr *ret, addr value)
{
	GetTypeTable(ret, Boolean);
	return 0;
}

/* clos */
int type_value_clos_(addr *ret, addr value)
{
	Return(clos_class_of_(value, &value));
	type_clos_heap(value, ret);
	return 0;
}

/* cons */
static int type_value_cons_(addr *ret, addr value)
{
	GetTypeTable(ret, Cons);
	return 0;
}

/* array */
static int type_value_strarray_(addr *ret, addr value)
{
	enum LISPDECL decl;
	enum CHARACTER_TYPE type;
	int simple;
	addr arg, pos;
	size_t size;

	Check(GetType(value) != LISPTYPE_ARRAY, "type error");
	Check(! strarrayp(value), "type array error");
	simple = ArrayInfoStruct(value)->simple;
	strarray_length(value, &size);
	make_index_integer_heap(&arg, size);

	Return(strarray_character_type_(value, &type));
	switch (type) {
		case CHARACTER_TYPE_STANDARD:
		case CHARACTER_TYPE_BASE:
			decl = simple? LISPDECL_SIMPLE_BASE_STRING: LISPDECL_BASE_STRING;
			type1_heap(decl, arg, &pos);
			break;

		default:
			decl = simple? LISPDECL_SIMPLE_STRING: LISPDECL_STRING;
			type1_heap(decl, arg, &pos);
			break;
	}

	return Result(ret, pos);
}

static void type_value_array_nil(addr *ret, addr value)
{
	enum LISPDECL decl;
	struct array_struct *str;
	addr type, pos;

	GetArrayInfo(value, ARRAY_INDEX_TYPE, &type);
	str = ArrayInfoStruct(value);
	decl = str->simple? LISPDECL_SIMPLE_ARRAY: LISPDECL_ARRAY;
	fixnum_heap(&pos, 0);
	type2_heap(decl, type, pos, ret);
}

static void type_value_array_single(addr *ret, addr value)
{
	enum LISPDECL decl;
	struct array_struct *str;
	addr type, pos, array;

	GetArrayInfo(value, ARRAY_INDEX_TYPE, &type);
	GetArrayInfo(value, ARRAY_INDEX_DIMENSION, &pos);
	str = ArrayInfoStruct(value);
	decl = str->simple? LISPDECL_SIMPLE_ARRAY: LISPDECL_ARRAY;
	make_index_integer_heap(&pos, str->size);
	vector4_heap(&array, 1);
	SetArrayA4(array, 0, pos);
	type2_heap(decl, type, array, ret);
}

static void type_value_array_multiple(addr *ret, addr value)
{
	enum LISPDECL decl;
	struct array_struct *str;
	addr type, pos, array;
	size_t *psize, size, i;

	GetArrayInfo(value, ARRAY_INDEX_TYPE, &type);
	GetArrayInfo(value, ARRAY_INDEX_DIMENSION, &pos);
	str = ArrayInfoStruct(value);
	decl = str->simple? LISPDECL_SIMPLE_ARRAY: LISPDECL_ARRAY;
	size = str->dimension;

	Check(GetType(pos) != LISPSYSTEM_ARRAY_DIMENSION, "type dimension error");
	psize = arraysize_ptr(pos);
	vector4_heap(&array, size);
	for (i = 0; i < size; i++) {
		make_index_integer_heap(&pos, psize[i]);
		SetArrayA4(array, i, pos);
	}
	type2_heap(decl, type, array, ret);
}

int type_value_array_(addr *ret, addr value)
{
	size_t size;

	Check(GetType(value) != LISPTYPE_ARRAY, "type error");
	size = ArrayInfoStruct(value)->dimension;
	if (array_stringp(value)) {
		Return(type_value_strarray_(ret, value));
	}
	else if (size == 0) {
		type_value_array_nil(ret, value);
	}
	else if (size == 1) {
		type_value_array_single(ret, value);
	}
	else {
		type_value_array_multiple(ret, value);
	}

	return 0;
}

/* vector */
void type_value_vector(addr *ret, addr value)
{
	addr arg;
	size_t size;

	Check(GetType(value) != LISPTYPE_VECTOR, "type error");
	lenarray(value, &size);
	make_index_integer_heap(&arg, size);
	type1_heap(LISPDECL_SIMPLE_VECTOR, arg, ret);
}

static int type_value_vector_(addr *ret, addr value)
{
	type_value_vector(ret, value);
	return 0;
}

/* character */
void type_value_character(addr *ret, addr value)
{
	enum CHARACTER_TYPE type;

	Check(GetType(value) != LISPTYPE_CHARACTER, "type error");
	get_character_type(value, &type);
	switch (type) {
		case CHARACTER_TYPE_STANDARD:
			GetTypeTable(ret, StandardChar);
			break;

		case CHARACTER_TYPE_BASE:
			GetTypeTable(ret, BaseChar);
			break;

		case CHARACTER_TYPE_EXTENDED:
			GetTypeTable(ret, ExtendedChar);
			break;

		default:
			GetTypeTable(ret, Character);
			break;
	}
}

static int type_value_character_(addr *ret, addr value)
{
	type_value_character(ret, value);
	return 0;
}

/* string */
static int type_value_string_(addr *ret, addr value)
{
	enum CHARACTER_TYPE type;
	addr arg, pos;
	size_t size;

	Check(GetType(value) != LISPTYPE_STRING, "type error");
	strvect_length(value, &size);
	make_index_integer_heap(&arg, size);

	Return(strvect_character_type_(value, &type));
	switch (type) {
		case CHARACTER_TYPE_STANDARD:
		case CHARACTER_TYPE_BASE:
			type1_heap(LISPDECL_SIMPLE_BASE_STRING, arg, &pos);
			break;

		default:
			type1_heap(LISPDECL_SIMPLE_STRING, arg, &pos);
			break;
	}

	return Result(ret, pos);
}

/* hashtable */
static int type_value_hashtable_(addr *ret, addr value)
{
	GetTypeTable(ret, Hashtable);
	return 0;
}

/* readtable */
static int type_value_readtable_(addr *ret, addr value)
{
	GetTypeTable(ret, Readtable);
	return 0;
}

/* symbol */
static int type_value_symbol_(addr *ret, addr value)
{
	Check(GetType(value) != LISPTYPE_SYMBOL, "type error");
	if (keywordp(value))
		GetTypeTable(ret, Keyword);
	else
		GetTypeTable(ret, Symbol);

	return 0;
}

/* integer */
static void type_realvalue(enum LISPDECL type, addr value, addr *ret)
{
	type4_heap(type, Nil, value, Nil, value, ret);
}

void type_value_integer(addr *ret, addr value)
{
	Check(! integerp(value), "type error");
	type_realvalue(LISPDECL_INTEGER, value, ret);
}

static int type_value_integer_(addr *ret, addr value)
{
	type_value_integer(ret, value);
	return 0;
}

/* rational */
void type_value_rational(addr *ret, addr value)
{
	Check(! rationalp(value), "type error");
	type_realvalue(LISPDECL_RATIONAL, value, ret);
}

static int type_value_rational_(addr *ret, addr value)
{
	type_value_rational(ret, value);
	return 0;
}

/* single */
static int type_value_single_(addr *ret, addr value)
{
	CheckType(value, LISPTYPE_SINGLE_FLOAT);
	type_realvalue(LISPDECL_SINGLE_FLOAT, value, ret);
	return 0;
}

/* double */
static int type_value_double_(addr *ret, addr value)
{
	CheckType(value, LISPTYPE_DOUBLE_FLOAT);
	type_realvalue(LISPDECL_DOUBLE_FLOAT, value, ret);
	return 0;
}

/* long */
static int type_value_long_(addr *ret, addr value)
{
	CheckType(value, LISPTYPE_LONG_FLOAT);
	type_realvalue(LISPDECL_LONG_FLOAT, value, ret);
	return 0;
}

void type_value_float(addr *ret, addr value)
{
	switch (GetType(value)) {
		case LISPTYPE_SINGLE_FLOAT:
			(void)type_value_single_(ret, value);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			(void)type_value_double_(ret, value);
			break;

		case LISPTYPE_LONG_FLOAT:
			(void)type_value_long_(ret, value);
			break;

		default:
			Abort("type error");
			break;
	}
}

/* complex */
int type_value_complex_(addr *ret, addr value)
{
	addr real, imag, type;

	GetRealComplex(value, &real);
	GetImagComplex(value, &imag);
	Return(type_value_(&real, real));
	Return(type_value_(&imag, imag));
	type2or_heap(real, imag, &type);
	type1_heap(LISPDECL_COMPLEX, type, ret);

	return 0;
}

/* function */
static int type_value_function_(addr *ret, addr value)
{
	Check(! functionp(value), "type error");
	gettype_function(value, ret);
	if (*ret == Nil) {
		if (compiled_function_p(value))
			GetTypeTable(ret, CompiledFunction);
		else
			GetTypeTable(ret, Function);
	}

	return 0;
}

/* package */
void type_value_package(addr *ret, addr value)
{
	GetTypeTable(ret, Package);
}

static int type_value_package_(addr *ret, addr value)
{
	type_value_package(ret, value);
	return 0;
}

/* random-state */
void type_value_random_state(addr *ret, addr value)
{
	GetTypeTable(ret, RandomState);
}

static int type_value_random_state_(addr *ret, addr value)
{
	type_value_random_state(ret, value);
	return 0;
}

/* pathname */
void type_value_pathname(addr *ret, addr value)
{
	Check(! pathnamep(value), "type error");
	if (pathname_pathname_p(value))
		GetTypeTable(ret, Pathname);
	else
		GetTypeTable(ret, LogicalPathname);
}

static int type_value_pathname_(addr *ret, addr value)
{
	type_value_pathname(ret, value);
	return 0;
}

/* environment */
void type_value_environment(addr *ret, addr value)
{
	GetTypeTable(ret, Environment);
}

static int type_value_environment_(addr *ret, addr value)
{
	type_value_environment(ret, value);
	return 0;
}

/* stream */
static int type_value_stream_(addr *ret, addr value)
{
	CheckType(value, LISPTYPE_STREAM);
	switch (getstreamtype(value)) {
		case StreamType_BroadCast:
			GetTypeTable(ret, BroadcastStream);
			return 0;

		case StreamType_Concatenated:
			GetTypeTable(ret, ConcatenatedStream);
			return 0;

		case StreamType_Echo:
			GetTypeTable(ret, EchoStream);
			return 0;

		case StreamType_StringInput:
		case StreamType_StringOutput:
			GetTypeTable(ret, StringStream);
			return 0;

		case StreamType_Synonym:
			GetTypeTable(ret, SynonymStream);
			return 0;

		case StreamType_TwoWay:
			GetTypeTable(ret, TwoWayStream);
			return 0;

		case StreamType_Prompt:
			GetTypeTable(ret, PromptStream);
			return 0;

		case StreamType_Pretty:
			GetTypeTable(ret, PrettyStream);
			return 0;

		default:
			break;
	}

	GetPathnameStream(value, &value);
	if (value != Nil)
		GetTypeTable(ret, FileStream);
	else
		GetTypeTable(ret, Stream);

	return 0;
}

/* restart */
static int type_value_restart_(addr *ret, addr value)
{
	GetTypeTable(ret, Restart);
	return 0;
}

/* eval */
static int type_value_eval_(addr *ret, addr value)
{
	GetTypeTable(ret, Eval);
	return 0;
}

/* bit-vector */
void type_value_bitvector(addr *ret, addr value)
{
	addr arg;
	size_t size;

	Check(GetType(value) != LISPTYPE_BITVECTOR, "type error");
	bitmemory_length(value, &size);
	make_index_integer_heap(&arg, size);
	type1_heap(LISPDECL_SIMPLE_BIT_VECTOR, arg, ret);
}

static int type_value_bitvector_(addr *ret, addr value)
{
	type_value_bitvector(ret, value);
	return 0;
}

/* quote */
static int type_value_quote_(addr *ret, addr value)
{
	GetTypeTable(ret, Quote);
	return 0;
}

static int type_value_bytespec_(addr *ret, addr value)
{
	GetTypeTable(ret, ByteSpec);
	return 0;
}

static int type_value_print_dispatch_(addr *ret, addr value)
{
	GetTypeTable(ret, PrintDispatch);
	return 0;
}

static int type_value_argument_(addr *ret, addr value)
{
	GetTypeTable(ret, T);
	return 0;
}

static int type_value_error_(addr *ret, addr value)
{
	infobit(value);
	return fmte_("Invalid type-value.", NULL);
}

int type_value_(addr *ret, addr value)
{
	type_value_call call;

	call = TypeValueTable[GetType(value)];
	if (call == NULL)
		return type_value_error_(ret, value);
	else
		return (*call)(ret, value);
}

void init_type_value(void)
{
	TypeValueTable[LISPTYPE_NIL] = type_value_nil_;
	TypeValueTable[LISPTYPE_T] = type_value_t_;
	TypeValueTable[LISPTYPE_CLOS] = type_value_clos_;
	TypeValueTable[LISPTYPE_CONS] = type_value_cons_;
	TypeValueTable[LISPTYPE_ARRAY] = type_value_array_;
	TypeValueTable[LISPTYPE_VECTOR] = type_value_vector_;
	TypeValueTable[LISPTYPE_CHARACTER] = type_value_character_;
	TypeValueTable[LISPTYPE_STRING] = type_value_string_;
	TypeValueTable[LISPTYPE_HASHTABLE] = type_value_hashtable_;
	TypeValueTable[LISPTYPE_READTABLE] = type_value_readtable_;
	TypeValueTable[LISPTYPE_SYMBOL] = type_value_symbol_;
	TypeValueTable[LISPTYPE_FIXNUM] = type_value_integer_;
	TypeValueTable[LISPTYPE_BIGNUM] = type_value_integer_;
	TypeValueTable[LISPTYPE_RATIO] = type_value_rational_;
	TypeValueTable[LISPTYPE_SHORT_FLOAT] = type_value_error_;
	TypeValueTable[LISPTYPE_SINGLE_FLOAT] = type_value_single_;
	TypeValueTable[LISPTYPE_DOUBLE_FLOAT] = type_value_double_;
	TypeValueTable[LISPTYPE_LONG_FLOAT] = type_value_long_;
	TypeValueTable[LISPTYPE_COMPLEX] = type_value_complex_;
	TypeValueTable[LISPTYPE_CONTROL] = type_value_error_;
	TypeValueTable[LISPTYPE_CODE] = type_value_error_;
	TypeValueTable[LISPTYPE_CALLNAME] = type_value_error_;
	TypeValueTable[LISPTYPE_FUNCTION] = type_value_function_;
	TypeValueTable[LISPTYPE_INDEX] = type_value_error_;
	TypeValueTable[LISPTYPE_PACKAGE] = type_value_package_;
	TypeValueTable[LISPTYPE_RANDOM_STATE] = type_value_random_state_;
	TypeValueTable[LISPTYPE_PATHNAME] = type_value_pathname_;
	TypeValueTable[LISPTYPE_STREAM] = type_value_stream_;
	TypeValueTable[LISPTYPE_RESTART] = type_value_restart_;
	TypeValueTable[LISPTYPE_EVAL] = type_value_eval_;
	TypeValueTable[LISPTYPE_ENVIRONMENT] = type_value_environment_;
	TypeValueTable[LISPTYPE_BITVECTOR] = type_value_bitvector_;
	TypeValueTable[LISPTYPE_QUOTE] = type_value_quote_;
	TypeValueTable[LISPTYPE_BYTESPEC] = type_value_bytespec_;
	TypeValueTable[LISPTYPE_PRINT_DISPATCH] = type_value_print_dispatch_;
	TypeValueTable[LISPSYSTEM_ARGUMENT] = type_value_argument_;
}


/************************************************************
 *  unicode.c
 ************************************************************/

/*
 *  UTF-8
 */
int string8_size_alloc_(LocalRoot local, addr *ret, const char *name, size_t size)
{
	addr pos;
	unicode *destroy;
	size_t allsize;

	if (UTF8_size_strlen((const byte *)name, size, &allsize))
		return fmte_("UTF8 encoding error (length).", NULL);
	strvect_alloc(local, &pos, allsize);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF8_size_makeunicode(destroy, (const byte *)name, size))
		return fmte_("UTF8 encoding error (make).", NULL);
	return Result(ret,  pos);
}
int string8_size_local_(LocalRoot local, addr *ret, const char *name, size_t size)
{
	CheckLocal(local);
	return string8_size_alloc_(local, ret, name, size);
}
int string8_size_heap_(addr *ret, const char *name, size_t size)
{
	return string8_size_alloc_(NULL, ret, name, size);
}

int string8_null_alloc_(LocalRoot local, addr *ret, const char *name)
{
	addr pos;
	unicode *destroy;
	size_t size;

	if (UTF8_null_strlen((const byte *)name, &size))
		return fmte_("UTF8 encoding error (length).", NULL);
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF8_null_makeunicode(destroy, (const byte *)name))
		return fmte_("UTF8 encoding error (make).", NULL);
	return Result(ret, pos);
}
int string8_null_local_(LocalRoot local, addr *ret, const char *name)
{
	CheckLocal(local);
	return string8_null_alloc_(local, ret, name);
}
int string8_null_heap_(addr *ret, const char *name)
{
	return string8_null_alloc_(NULL, ret, name);
}

int string8_null_char1_heap_(addr *ret, const char *name, unicode c)
{
	addr pos;
	unicode *destroy;
	size_t size;

	if (UTF8_null_strlen((const byte *)name, &size))
		return fmte_("UTF8 encoding error (length).", NULL);
	strvect_alloc(NULL, &pos, size + 1UL);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF8_null_makeunicode(destroy, (const byte *)name))
		return fmte_("UTF8 encoding error (make).", NULL);
	destroy[size] = c;
	return Result(ret, pos);
}


/*
 *  UTF-16
 */
int string16_size_alloc_(LocalRoot local, addr *ret, const byte16 *name, size_t size)
{
	addr pos;
	unicode *destroy;
	size_t allsize;

	if (UTF16_size_strlen(name, size, &allsize))
		return fmte_("UTF16 encoding error (length).", NULL);
	strvect_alloc(local, &pos, allsize);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF16_size_makeunicode(destroy, name, size))
		return fmte_("UTF16 encoding error (make).", NULL);
	return Result(ret, pos);
}
int string16_size_local_(LocalRoot local, addr *ret, const byte16 *name, size_t size)
{
	CheckLocal(local);
	return string16_size_alloc_(local, ret, name, size);
}
int string16_size_heap_(addr *ret, const byte16 *name, size_t size)
{
	return string16_size_alloc_(NULL, ret, name, size);
}

int string16_null_alloc_(LocalRoot local, addr *ret, const byte16 *name)
{
	addr pos;
	unicode *destroy;
	size_t size;

	if (UTF16_null_strlen(name, &size))
		return fmte_("UTF16 encoding error (length).", NULL);
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF16_null_makeunicode(destroy, name))
		return fmte_("UTF16 encoding error (make).", NULL);
	return Result(ret, pos);
}
int string16_null_local_(LocalRoot local, addr *ret, const byte16 *name)
{
	CheckLocal(local);
	return string16_null_alloc_(local, ret, name);
}
int string16_null_heap_(addr *ret, const byte16 *name)
{
	return string16_null_alloc_(NULL, ret, name);
}


/*
 *  UTF-32
 */
int string32_size_alloc_(LocalRoot local, addr *ret, const unicode *name, size_t size)
{
	addr pos;
	unicode *destroy;
	size_t allsize;

	if (UTF32_size_strlen(name, size, &allsize))
		return fmte_("UTF32 encoding error (length).", NULL);
	strvect_alloc(local, &pos, allsize);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF32_size_makeunicode(destroy, name, size))
		return fmte_("UTF32 encoding error (make).", NULL);
	return Result(ret, pos);
}

int string32_size_local_(LocalRoot local, addr *ret, const unicode *name, size_t size)
{
	CheckLocal(local);
	return string32_size_alloc_(local, ret, name, size);
}

int string32_size_heap_(addr *ret, const unicode *name, size_t size)
{
	return string32_size_alloc_(NULL, ret, name, size);
}

int string32_null_alloc_(LocalRoot local, addr *ret, const unicode *name)
{
	addr pos;
	unicode *destroy;
	size_t size;

	if (UTF32_null_strlen(name, &size))
		return fmte_("UTF32 encoding error (length).", NULL);
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF32_null_makeunicode(destroy, name))
		return fmte_("UTF32 encoding error (make).", NULL);
	return Result(ret, pos);
}

int string32_null_local_(LocalRoot local, addr *ret, const unicode *name)
{
	CheckLocal(local);
	return string32_null_alloc_(local, ret, name);
}

int string32_null_heap_(addr *ret, const unicode *name)
{
	return string32_null_alloc_(NULL, ret, name);
}


/************************************************************
 *  variable.c
 ************************************************************/

/*
 *  build
 */
int      lisp_initialize    = 0;
addr     lisp_root[LISPINDEX_SIZE];
addr     lisp_nil_object    = 0;
addr     lisp_t_object      = 0;
int      lisp_info_enable   = 1;
enum GcMode lisp_gcsync     = GcMode_Off;


/*
 *  heap
 */
void   *heap_alloc = 0;
addr    heap_root = 0;
addr    heap_front = 0;
addr    heap_pos = 0;
addr    heap_tail = 0;
addr    heap_range = 0;
size_t  heap_object = 0;
size_t  heap_count = 0;
size_t  heap_gc_count = 0;
size_t  heap_gc_partial = 0;
size_t  heap_gc_full = 0;
size_t  heap_cons_count = 0;
size_t  heap_symbol_count = 0;


/*
 *  control
 */
#ifdef LISP_DEBUG_FORCE_GC
size_t GcCounterForce = 0;
#endif
size_t ControlCounter = 0;


/*
 *  clos
 */
addr Clos_standard_class = 0;
addr Clos_standard_generic = 0;
addr Clos_standard_method = 0;
addr Clos_standard_combination = 0;
addr Clos_standard_specializer = 0;


/*
 *  execute
 */
threadlocal ThreadLocal_Execute;
threadlocal ThreadLocal_Index;
threadlocal ThreadLocal_Local;
lisp_abort_calltype Lisp_abort_handler = NULL;


/*
 *  pointer
 */
struct callbind_struct pointer_table[SizePointer];

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#ifdef __clang__
#pragma clang diagnostic pop
#endif
