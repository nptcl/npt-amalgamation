/*
 *  npt -- ANSI Common Lisp Programming Language.
 *    https://github.com/nptcl/npt
 *    https://github.com/nptcl/npt-amalgamation
 *
 *  File: lisp_file_05.c
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

#include <ctype.h>
#include <locale.h>
#include <math.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "lisp_file.h"


/************************************************************
 *  float_object.c
 ************************************************************/

int single_float_check_alloc_(LocalRoot local, addr *ret, single_float value)
{
	Return_float_errorcheck0(CONSTANT_COMMON_FLOAT, value);
	single_float_alloc(local, ret, value);
	return 0;
}

int single_float_check_local_(LocalRoot local, addr *ret, single_float value)
{
	Check(local == NULL, "local error");
	return single_float_check_alloc_(local, ret, value);
}

int single_float_check_heap_(addr *ret, single_float value)
{
	return single_float_check_alloc_(NULL, ret, value);
}

int double_float_check_alloc_(LocalRoot local, addr *ret, double_float value)
{
	Return_float_errorcheck0(CONSTANT_COMMON_FLOAT, value);
	double_float_alloc(local, ret, value);
	return 0;
}

int double_float_check_local_(LocalRoot local, addr *ret, double_float value)
{
	Check(local == NULL, "local error");
	return double_float_check_alloc_(local, ret, value);
}

int double_float_check_heap_(addr *ret, double_float value)
{
	return double_float_check_alloc_(NULL, ret, value);
}

int long_float_check_alloc_(LocalRoot local, addr *ret, long_float value)
{
	Return_float_errorcheck0(CONSTANT_COMMON_FLOAT, value);
	long_float_alloc(local, ret, value);
	return 0;
}

int long_float_check_local_(LocalRoot local, addr *ret, long_float value)
{
	Check(local == NULL, "local error");
	return long_float_check_alloc_(local, ret, value);
}

int long_float_check_heap_(addr *ret, long_float value)
{
	return long_float_check_alloc_(NULL, ret, value);
}

void single_float_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_SINGLE_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		single_float_heap(ret, RefSingleFloat(pos));
	else
		*ret = pos;
}

void double_float_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_DOUBLE_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		double_float_heap(ret, RefDoubleFloat(pos));
	else
		*ret = pos;
}

void long_float_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_LONG_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		long_float_heap(ret, RefLongFloat(pos));
	else
		*ret = pos;
}

void single_float_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_SINGLE_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		single_float_local(local, ret, RefSingleFloat(pos));
}

void double_float_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_DOUBLE_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		double_float_local(local, ret, RefDoubleFloat(pos));
}

void long_float_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_LONG_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		long_float_local(local, ret, RefLongFloat(pos));
}

void single_float_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (local)
		single_float_throw_local(local, pos, ret);
	else
		single_float_throw_heap(pos, ret);
}

void double_float_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (local)
		double_float_throw_local(local, pos, ret);
	else
		double_float_throw_heap(pos, ret);
}

void long_float_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (local)
		long_float_throw_local(local, pos, ret);
	else
		long_float_throw_heap(pos, ret);
}

int float_throw_heap_(addr pos, addr *ret)
{
	return float_throw_alloc_(NULL, pos, ret);
}

int float_throw_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return float_throw_alloc_(local, pos, ret);
}

int float_throw_alloc_(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			single_float_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_SHORT_FLOAT:
			Abort("short float is not implemented.");
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, FLOAT);
	}

	return 0;
}

static void single_float_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	single_float_alloc(local, ret, RefSingleFloat(pos));
}

static void double_float_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	double_float_alloc(local, ret, RefDoubleFloat(pos));
}

static void long_float_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_LONG_FLOAT);
	long_float_alloc(local, ret, RefLongFloat(pos));
}

void float_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			single_float_copy_alloc(local, pos, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_copy_alloc(local, pos, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_copy_alloc(local, pos, ret);
			break;

		case LISPTYPE_SHORT_FLOAT:
			Abort("short float is not implemented.");
			break;

		default:
			*ret = Nil;
			Abort("type error");
			break;
	}
}

void float_copy_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	float_copy_alloc(local, pos, ret);
}

void float_copy_heap(addr pos, addr *ret)
{
	float_copy_alloc(NULL, pos, ret);
}


/*
 *  strtof
 */
fltclasstype fltclassify(int check, int sign)
{
	if (check == FP_INFINITE) {
		if (sign)
			return fltclass_underflow;
		else
			return fltclass_overflow;
	}
	if (check == FP_NAN) {
		return fltclass_nan;
	}

	return fltclass_normal;
}

int float_fltclass_(constindex index, fltclasstype type, ...)
{
	va_list args;
	addr list;

	va_start(args, type);
	list_stdarg_alloc(NULL, &list, args);
	va_end(args);

	switch (type) {
		case fltclass_overflow:
			return call_float_overflow_const_(NULL, index, list);

		case fltclass_underflow:
			return call_float_underflow_const_(NULL, index, list);

		case fltclass_nan:
		default:
			return call_float_invalid_const_(NULL, index, list);
	}
}

static single_float strtof_c(const char *str)
{
	single_float value;
	const char *check;
#ifdef BIGNUM_DEBUG
	char *endptr;
#endif

	/* setlocale C */
	check = setlocale_c(LC_NUMERIC);

	/* convert */
#ifdef BIGNUM_DEBUG
	value = strtof(str, &endptr);
	Check(*endptr, "strtof error");
#else
	value = strtof(str, NULL);
#endif

	/* setlocale */
	if (check)
		setlocale(LC_NUMERIC, check);

	/* result */
	return value;
}

static double_float strtod_c(const char *str)
{
	double_float value;
	const char *check;
#ifdef BIGNUM_DEBUG
	char *endptr;
#endif

	/* setlocale C */
	check = setlocale_c(LC_NUMERIC);

	/* convert */
#ifdef BIGNUM_DEBUG
	value = strtod(str, &endptr);
	Check(*endptr, "strtod error");
#else
	value = strtod(str, NULL);
#endif

	/* setlocale */
	if (check)
		setlocale(LC_NUMERIC, check);

	/* result */
	return value;
}

static long_float strtold_c(const char *str)
{
	long_float value;
	const char *check;
#ifdef BIGNUM_DEBUG
	char *endptr;
#endif

	/* setlocale C */
	check = setlocale_c(LC_NUMERIC);

	/* convert */
#ifdef BIGNUM_DEBUG
	value = strtold(str, &endptr);
	Check(*endptr, "strtold error");
#else
	value = strtold(str, NULL);
#endif

	/* setlocale */
	if (check)
		setlocale(LC_NUMERIC, check);

	/* result */
	return value;
}

int check_strtof_(const char *str, addr pos, single_float *ret)
{
	fltclasstype type;
	single_float value;

	value = strtof_c(str);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		*ret = 0.0f;
		return float_fltclass_(CONSTANT_SYSTEM_CAST_SINGLE_FLOAT, type, pos, NULL);
	}

	return Result(ret, value);
}

int check_strtod_(const char *str, addr pos, double_float *ret)
{
	fltclasstype type;
	double_float value;

	value = strtod_c(str);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		*ret = 0.0;
		return float_fltclass_(CONSTANT_SYSTEM_CAST_DOUBLE_FLOAT, type, pos, NULL);
	}

	return Result(ret, value);
}

int check_strtold_(const char *str, addr pos, long_float *ret)
{
	fltclasstype type;
	long_float value;

	value = strtold_c(str);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		*ret = 0.0L;
		return float_fltclass_(CONSTANT_SYSTEM_CAST_LONG_FLOAT, type, pos, NULL);
	}

	return Result(ret, value);
}

int check_strtof_reverse_(const char *str, addr pos, single_float *ret)
{
	fltclasstype type;
	single_float value;

	value = strtof_c(str);
	type = getfltclassify_reverse(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		*ret = 0.0f;
		return float_fltclass_(CONSTANT_SYSTEM_CAST_SINGLE_FLOAT, type, pos, NULL);
	}

	return Result(ret, value);
}

int check_strtod_reverse_(const char *str, addr pos, double_float *ret)
{
	fltclasstype type;
	double_float value;

	value = strtod_c(str);
	type = getfltclassify_reverse(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		*ret = 0.0;
		return float_fltclass_(CONSTANT_SYSTEM_CAST_DOUBLE_FLOAT, type, pos, NULL);
	}

	return Result(ret, value);
}

int check_strtold_reverse_(const char *str, addr pos, long_float *ret)
{
	fltclasstype type;
	long_float value;

	value = strtold_c(str);
	type = getfltclassify_reverse(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		*ret = 0.0L;
		return float_fltclass_(CONSTANT_SYSTEM_CAST_LONG_FLOAT, type, pos, NULL);
	}

	return Result(ret, value);
}


/*
 *  abs
 */
void abs_floats_alloc(LocalRoot local, addr left, addr *ret)
{
	single_float value;

	GetSingleFloat(left, &value);
	if (0.0f <= value)
		single_float_throw_alloc(local, left, ret);
	else
		single_float_alloc(local, ret, fabsf(value));
}

void abs_floatd_alloc(LocalRoot local, addr left, addr *ret)
{
	double_float value;

	GetDoubleFloat(left, &value);
	if (0.0 <= value)
		double_float_throw_alloc(local, left, ret);
	else
		double_float_alloc(local, ret, fabs(value));
}

void abs_floatl_alloc(LocalRoot local, addr left, addr *ret)
{
	long_float value;

	GetLongFloat(left, &value);
	if (0.0L <= value)
		long_float_throw_alloc(local, left, ret);
	else
		long_float_alloc(local, ret, fabsl(value));
}

void abs_floats_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	abs_floats_alloc(local, left, ret);
}

void abs_floatd_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	abs_floatd_alloc(local, left, ret);
}

void abs_floatl_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	abs_floatl_alloc(local, left, ret);
}

void abs_floats_heap(addr left, addr *ret)
{
	abs_floats_alloc(NULL, left, ret);
}

void abs_floatd_heap(addr left, addr *ret)
{
	abs_floatd_alloc(NULL, left, ret);
}

void abs_floatl_heap(addr left, addr *ret)
{
	abs_floatl_alloc(NULL, left, ret);
}


/*
 *  cast
 */
int cast_sd_float_(single_float v, double_float *ret)
{
	return Result(ret, (double_float)v);
}

int cast_sl_float_(single_float v, long_float *ret)
{
	return Result(ret, (long_float)v);
}

int cast_ds_float_(double_float v, single_float *ret)
{
	fltclasstype type;
	single_float value;
	addr pos;

	value = (single_float)v;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		double_float_heap(&pos, v);
		*ret = 0.0f;
		return float_fltclass_(CONSTANT_COMMON_COERCE, type, pos, NULL);
	}

	return Result(ret, value);
}

int cast_dl_float_(double_float v, long_float *ret)
{
	return Result(ret, (long_float)v);
}

int cast_ls_float_(long_float v, single_float *ret)
{
	fltclasstype type;
	single_float value;
	addr pos;

	value = (single_float)v;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		long_float_heap(&pos, v);
		*ret = 0.0f;
		return float_fltclass_(CONSTANT_COMMON_COERCE, type, pos, NULL);
	}

	return Result(ret, value);
}

int cast_ld_float_(long_float v, double_float *ret)
{
	fltclasstype type;
	double_float value;
	addr pos;

	value = (double_float)v;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		long_float_heap(&pos, v);
		*ret = 0.0;
		return float_fltclass_(CONSTANT_COMMON_COERCE, type, pos, NULL);
	}

	return Result(ret, value);
}

int cast_sd_value_(addr pos, double_float *ret)
{
	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	return Result(ret, (double_float)RefSingleFloat(pos));
}

int cast_sl_value_(addr pos, long_float *ret)
{
	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	return Result(ret, (long_float)RefSingleFloat(pos));
}

int cast_ds_value_(addr pos, single_float *ret)
{
	single_float v;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	v = (single_float)RefDoubleFloat(pos);
	Return_float_errorcheck1(CONSTANT_COMMON_COERCE, v, pos);

	return Result(ret, v);
}

int cast_dl_value_(addr pos, long_float *ret)
{
	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	return Result(ret, (long_float)RefDoubleFloat(pos));
}

int cast_ls_value_(addr pos, single_float *ret)
{
	single_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	v = (single_float)RefLongFloat(pos);
	Return_float_errorcheck1(CONSTANT_COMMON_COERCE, v, pos);

	return Result(ret, v);
}

int cast_ld_value_(addr pos, double_float *ret)
{
	double_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	v = (double_float)RefLongFloat(pos);
	Return_float_errorcheck1(CONSTANT_COMMON_COERCE, v, pos);

	return Result(ret, v);
}

int cast_ss_value_(addr pos, single_float *ret)
{
	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	GetSingleFloat(pos, ret);
	return 0;
}

int cast_dd_value_(addr pos, double_float *ret)
{
	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	GetDoubleFloat(pos, ret);
	return 0;
}

int cast_ll_value_(addr pos, long_float *ret)
{
	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, ret);
	return 0;
}


/*
 *  sqrt
 */
static int sqrt_single_float_alloc_(LocalRoot local, addr left, addr *ret)
{
	addr zero;
	single_float value;

	GetSingleFloat(left, &value);
	if (value < 0) {
		single_float_alloc(local, &zero, 0.0f);
		single_float_alloc(local, &left, sqrtf(-value));
		return complex_alloc_(local, ret, zero, left);
	}
	else {
		single_float_alloc(local, ret, sqrtf(value));
		return 0;
	}
}

static int sqrt_double_float_alloc_(LocalRoot local, addr left, addr *ret)
{
	addr zero;
	double_float value;

	GetDoubleFloat(left, &value);
	if (value < 0) {
		double_float_alloc(local, &zero, 0.0);
		double_float_alloc(local, &left, sqrt(-value));
		return complex_alloc_(local, ret, zero, left);
	}
	else {
		double_float_alloc(local, ret, sqrt(value));
		return 0;
	}
}

static int sqrt_long_float_alloc_(LocalRoot local, addr left, addr *ret)
{
	addr zero;
	long_float value;

	GetLongFloat(left, &value);
	if (value < 0) {
		long_float_alloc(local, &zero, 0.0L);
		long_float_alloc(local, &left, sqrtl(-value));
		return complex_alloc_(local, ret, zero, left);
	}
	else {
		long_float_alloc(local, ret, sqrtl(value));
		return 0;
	}
}

int sqrt_float_alloc_(LocalRoot local, addr left, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_SINGLE_FLOAT:
			return sqrt_single_float_alloc_(local, left, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return sqrt_double_float_alloc_(local, left, ret);

		case LISPTYPE_LONG_FLOAT:
			return sqrt_long_float_alloc_(local, left, ret);

		default:
			*ret = Nil;
			return TypeError_(left, FLOAT);
	}
}

int sqrt_float_local_(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	return sqrt_float_alloc_(local, left, ret);
}

int sqrt_float_heap_(LocalRoot local, addr left, addr *ret)
{
	return sqrt_float_alloc_(NULL, left, ret);
}


/* fpclassify */
static void fpclassify_type(int type, addr *ret)
{
	switch (type) {
		case FP_INFINITE:
			GetConst(SYSTEM_FP_INFINITE, ret);
			break;

		case FP_NAN:
			GetConst(SYSTEM_FP_NAN, ret);
			break;

		case FP_NORMAL:
			GetConst(SYSTEM_FP_NORMAL, ret);
			break;

		case FP_SUBNORMAL:
			GetConst(SYSTEM_FP_SUBNORMAL, ret);
			break;

		case FP_ZERO:
			GetConst(SYSTEM_FP_ZERO, ret);
			break;

		default:
			*ret = Nil;
			break;
	}
}

static void fpclassify_sign(int sign, addr *ret)
{
	fixnum_heap(ret, sign? -1: 1);
}

static void fpclassify_single_float(addr var, addr *rtype, addr *rsign)
{
	single_float value;

	GetSingleFloat(var, &value);
	fpclassify_type(fpclassify(value), rtype);
	fpclassify_sign(signbit(value), rsign);
}

static void fpclassify_double_float(addr var, addr *rtype, addr *rsign)
{
	double_float value;

	GetDoubleFloat(var, &value);
	fpclassify_type(fpclassify(value), rtype);
	fpclassify_sign(signbit(value), rsign);
}

static void fpclassify_long_float(addr var, addr *rtype, addr *rsign)
{
	long_float value;

	GetLongFloat(var, &value);
	fpclassify_type(fpclassify(value), rtype);
	fpclassify_sign(signbit(value), rsign);
}

static void fpclassify_short_float(addr var, addr *rtype, addr *rsign)
{
	fpclassify_single_float(var, rtype, rsign);
}

void fpclassify_float(addr var, addr *rtype, addr *rsign)
{
	switch (GetType(var)) {
		case LISPTYPE_SINGLE_FLOAT:
			fpclassify_single_float(var, rtype, rsign);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fpclassify_double_float(var, rtype, rsign);
			break;

		case LISPTYPE_LONG_FLOAT:
			fpclassify_long_float(var, rtype, rsign);
			break;

		case LISPTYPE_SHORT_FLOAT:
			fpclassify_short_float(var, rtype, rsign);
			break;

		default:
			*rtype = *rsign = Nil;
			break;
	}
}


/************************************************************
 *  float_plus.c
 ************************************************************/

/*
 *  plus/minus value
 */
int plus_float_sv_alloc_(LocalRoot local, addr left, single_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type error");
	value = RefSingleFloat(left) + right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		single_float_heap(&temp, right);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_PLUS, type, left, temp, NULL);
	}
	single_float_alloc(local, ret, value);

	return 0;
}

int plus_float_dv_alloc_(LocalRoot local, addr left, double_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type error");
	value = RefDoubleFloat(left) + right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		double_float_heap(&temp, right);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_PLUS, type, left, temp, NULL);
	}
	double_float_alloc(local, ret, value);

	return 0;
}

int plus_float_lv_alloc_(LocalRoot local, addr left, long_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type error");
	value = RefLongFloat(left) + right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		long_float_heap(&temp, right);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_PLUS, type, left, temp, NULL);
	}
	long_float_alloc(local, ret, value);

	return 0;
}

int plus_float_sv_local_(LocalRoot local, addr left, single_float right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_sv_alloc_(local, left, right, ret);
}

int plus_float_dv_local_(LocalRoot local, addr left, double_float right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_dv_alloc_(local, left, right, ret);
}

int plus_float_lv_local_(LocalRoot local, addr left, long_float right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_lv_alloc_(local, left, right, ret);
}

int plus_float_sv_heap_(addr left, single_float right, addr *ret)
{
	return plus_float_sv_alloc_(NULL, left, right, ret);
}

int plus_float_dv_heap_(addr left, double_float right, addr *ret)
{
	return plus_float_dv_alloc_(NULL, left, right, ret);
}

int plus_float_lv_heap_(addr left, long_float right, addr *ret)
{
	return plus_float_lv_alloc_(NULL, left, right, ret);
}

int minus_float_sv_alloc_(LocalRoot local, addr left, single_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type error");
	value = RefSingleFloat(left) - right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		single_float_heap(&temp, right);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_MINUS, type, left, temp, NULL);
	}
	single_float_alloc(local, ret, value);

	return 0;
}

int minus_float_dv_alloc_(LocalRoot local, addr left, double_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type error");
	value = RefDoubleFloat(left) - right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		double_float_heap(&temp, right);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_MINUS, type, left, temp, NULL);
	}
	double_float_alloc(local, ret, value);

	return 0;
}

int minus_float_lv_alloc_(LocalRoot local, addr left, long_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type error");
	value = RefLongFloat(left) - right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		long_float_heap(&temp, right);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_MINUS, type, left, temp, NULL);
	}
	long_float_alloc(local, ret, value);

	return 0;
}

int minus_float_sv_local_(LocalRoot local, addr left, single_float right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_sv_alloc_(local, left, right, ret);
}

int minus_float_dv_local_(LocalRoot local, addr left, double_float right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_dv_alloc_(local, left, right, ret);
}

int minus_float_lv_local_(LocalRoot local, addr left, long_float right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_lv_alloc_(local, left, right, ret);
}

int minus_float_sv_heap_(addr left, single_float right, addr *ret)
{
	return minus_float_sv_alloc_(NULL, left, right, ret);
}

int minus_float_dv_heap_(addr left, double_float right, addr *ret)
{
	return minus_float_dv_alloc_(NULL, left, right, ret);
}

int minus_float_lv_heap_(addr left, long_float right, addr *ret)
{
	return minus_float_lv_alloc_(NULL, left, right, ret);
}

int minus_float_vs_alloc_(LocalRoot local, single_float left, addr right, addr *ret)
{
	fltclasstype type;
	addr temp;
	single_float value;

	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type error");
	value = left - RefSingleFloat(right);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		single_float_heap(&temp, left);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_MINUS, type, temp, right, NULL);
	}
	single_float_alloc(local, ret, value);

	return 0;
}

int minus_float_vd_alloc_(LocalRoot local, double_float left, addr right, addr *ret)
{
	fltclasstype type;
	addr temp;
	double_float value;

	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type error");
	value = left - RefDoubleFloat(right);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		double_float_heap(&temp, left);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_MINUS, type, temp, right, NULL);
	}
	double_float_alloc(local, ret, value);

	return 0;
}

int minus_float_vl_alloc_(LocalRoot local, long_float left, addr right, addr *ret)
{
	fltclasstype type;
	addr temp;
	long_float value;

	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type error");
	value = left - RefLongFloat(right);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		long_float_heap(&temp, left);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_MINUS, type, temp, right, NULL);
	}
	long_float_alloc(local, ret, value);

	return 0;
}

int minus_float_vs_local_(LocalRoot local, single_float left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_vs_alloc_(local, left, right, ret);
}

int minus_float_vd_local_(LocalRoot local, double_float left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_vd_alloc_(local, left, right, ret);
}

int minus_float_vl_local_(LocalRoot local, long_float left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_vl_alloc_(local, left, right, ret);
}

int minus_float_vs_heap_(single_float left, addr right, addr *ret)
{
	return minus_float_vs_alloc_(NULL, left, right, ret);
}

int minus_float_vd_heap_(double_float left, addr right, addr *ret)
{
	return minus_float_vd_alloc_(NULL, left, right, ret);
}

int minus_float_vl_heap_(long_float left, addr right, addr *ret)
{
	return minus_float_vl_alloc_(NULL, left, right, ret);
}

void sign_reverse_floats_alloc(LocalRoot local, addr value, addr *ret)
{
	single_float_alloc(local, ret, -RefSingleFloat(value));
}

void sign_reverse_floatd_alloc(LocalRoot local, addr value, addr *ret)
{
	double_float_alloc(local, ret, -RefDoubleFloat(value));
}

void sign_reverse_floatl_alloc(LocalRoot local, addr value, addr *ret)
{
	long_float_alloc(local, ret, -RefLongFloat(value));
}

void sign_reverse_floats_local(LocalRoot local, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	sign_reverse_floats_alloc(local, value, ret);
}

void sign_reverse_floatd_local(LocalRoot local, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	sign_reverse_floatd_alloc(local, value, ret);
}

void sign_reverse_floatl_local(LocalRoot local, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	sign_reverse_floatl_alloc(local, value, ret);
}

void sign_reverse_floats_heap(addr value, addr *ret)
{
	sign_reverse_floats_alloc(NULL, value, ret);
}

void sign_reverse_floatd_heap(addr value, addr *ret)
{
	sign_reverse_floatd_alloc(NULL, value, ret);
}

void sign_reverse_floatl_heap(addr value, addr *ret)
{
	sign_reverse_floatl_alloc(NULL, value, ret);
}


/*
 *  plus fixnum
 */
int plus_float_fs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0) {
		single_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		return plus_float_sv_alloc_(local, right, (single_float)value, ret);
	}
}

int plus_float_fd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0) {
		double_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		return plus_float_dv_alloc_(local, right, (double_float)value, ret);
	}
}

int plus_float_fl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0) {
		long_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		return plus_float_lv_alloc_(local, right, (long_float)value, ret);
	}
}

int plus_float_fs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_fs_alloc_(local, left, right, ret);
}

int plus_float_fd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_fd_alloc_(local, left, right, ret);
}

int plus_float_fl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_fl_alloc_(local, left, right, ret);
}

int plus_float_fs_heap_(addr left, addr right, addr *ret)
{
	return plus_float_fs_alloc_(NULL, left, right, ret);
}

int plus_float_fd_heap_(addr left, addr right, addr *ret)
{
	return plus_float_fd_alloc_(NULL, left, right, ret);
}

int plus_float_fl_heap_(addr left, addr right, addr *ret)
{
	return plus_float_fl_alloc_(NULL, left, right, ret);
}

int plus_float_bs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	if (zerop_bignum(left)) {
		single_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		Return(single_float_bignum_(left, &value));
		return plus_float_sv_alloc_(local, right, value, ret);
	}
}

int plus_float_bd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_bignum(left)) {
		double_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		Return(double_float_bignum_(left, &value));
		return plus_float_dv_alloc_(local, right, value, ret);
	}
}

int plus_float_bl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_bignum(left)) {
		long_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		Return(long_float_bignum_(left, &value));
		return plus_float_lv_alloc_(local, right, value, ret);
	}
}

int plus_float_bs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_bs_alloc_(local, left, right, ret);
}

int plus_float_bd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_bd_alloc_(local, left, right, ret);
}

int plus_float_bl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_bl_alloc_(local, left, right, ret);
}

int plus_float_bs_heap_(addr left, addr right, addr *ret)
{
	return plus_float_bs_alloc_(NULL, left, right, ret);
}

int plus_float_bd_heap_(addr left, addr right, addr *ret)
{
	return plus_float_bd_alloc_(NULL, left, right, ret);
}

int plus_float_bl_heap_(addr left, addr right, addr *ret)
{
	return plus_float_bl_alloc_(NULL, left, right, ret);
}

int plus_float_rs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	if (zerop_ratio(left)) {
		single_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		Return(single_float_ratio_(left, &value));
		return plus_float_sv_alloc_(local, right, value, ret);
	}
}

int plus_float_rd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_ratio(left)) {
		double_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		Return(double_float_ratio_(left, &value));
		return plus_float_dv_alloc_(local, right, value, ret);
	}
}

int plus_float_rl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_ratio(left)) {
		long_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		Return(long_float_ratio_(left, &value));
		return plus_float_lv_alloc_(local, right, value, ret);
	}
}

int plus_float_rs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_rs_alloc_(local, left, right, ret);
}

int plus_float_rd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_rd_alloc_(local, left, right, ret);
}

int plus_float_rl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_rl_alloc_(local, left, right, ret);
}

int plus_float_rs_heap_(addr left, addr right, addr *ret)
{
	return plus_float_rs_alloc_(NULL, left, right, ret);
}

int plus_float_rd_heap_(addr left, addr right, addr *ret)
{
	return plus_float_rd_alloc_(NULL, left, right, ret);
}

int plus_float_rl_heap_(addr left, addr right, addr *ret)
{
	return plus_float_rl_alloc_(NULL, left, right, ret);
}

int plus_float_ss_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefSingleFloat(left) + RefSingleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

int plus_float_sd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = ((double_float)RefSingleFloat(left)) + RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int plus_float_sl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefSingleFloat(left)) + RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int plus_float_ds_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) + ((double_float)RefSingleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int plus_float_dd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) + RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int plus_float_dl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefDoubleFloat(left)) + RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int plus_float_ls_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefLongFloat(left) + ((long_float)RefSingleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int plus_float_ld_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefLongFloat(left) + ((long_float)RefDoubleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int plus_float_ll_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = RefLongFloat(left) + RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int plus_float_ss_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_ss_alloc_(local, left, right, ret);
}

int plus_float_sd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_sd_alloc_(local, left, right, ret);
}

int plus_float_sl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_sl_alloc_(local, left, right, ret);
}

int plus_float_ds_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_ds_alloc_(local, left, right, ret);
}

int plus_float_dd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_dd_alloc_(local, left, right, ret);
}

int plus_float_dl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_dl_alloc_(local, left, right, ret);
}

int plus_float_ls_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_ls_alloc_(local, left, right, ret);
}

int plus_float_ld_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_ld_alloc_(local, left, right, ret);
}

int plus_float_ll_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_ll_alloc_(local, left, right, ret);
}

int plus_float_ss_heap_(addr left, addr right, addr *ret)
{
	return plus_float_ss_alloc_(NULL, left, right, ret);
}

int plus_float_sd_heap_(addr left, addr right, addr *ret)
{
	return plus_float_sd_alloc_(NULL, left, right, ret);
}

int plus_float_sl_heap_(addr left, addr right, addr *ret)
{
	return plus_float_sl_alloc_(NULL, left, right, ret);
}

int plus_float_ds_heap_(addr left, addr right, addr *ret)
{
	return plus_float_ds_alloc_(NULL, left, right, ret);
}

int plus_float_dd_heap_(addr left, addr right, addr *ret)
{
	return plus_float_dd_alloc_(NULL, left, right, ret);
}

int plus_float_dl_heap_(addr left, addr right, addr *ret)
{
	return plus_float_dl_alloc_(NULL, left, right, ret);
}

int plus_float_ls_heap_(addr left, addr right, addr *ret)
{
	return plus_float_ls_alloc_(NULL, left, right, ret);
}

int plus_float_ld_heap_(addr left, addr right, addr *ret)
{
	return plus_float_ld_alloc_(NULL, left, right, ret);
}

int plus_float_ll_heap_(addr left, addr right, addr *ret)
{
	return plus_float_ll_alloc_(NULL, left, right, ret);
}


/*
 *  minus
 */
int minus_float_fs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0) {
		sign_reverse_floats_heap(right, ret);
		return 0;
	}
	else {
		return minus_float_vs_alloc_(local, (single_float)value, right, ret);
	}
}

int minus_float_fd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0) {
		sign_reverse_floatd_heap(right, ret);
		return 0;
	}
	else {
		return minus_float_vd_alloc_(local, (double_float)value, right, ret);
	}
}

int minus_float_fl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0) {
		sign_reverse_floatl_heap(right, ret);
		return 0;
	}
	else {
		return minus_float_vl_alloc_(local, (long_float)value, right, ret);
	}
}

int minus_float_fs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_fs_alloc_(local, left, right, ret);
}

int minus_float_fd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_fd_alloc_(local, left, right, ret);
}

int minus_float_fl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_fl_alloc_(local, left, right, ret);
}

int minus_float_fs_heap_(addr left, addr right, addr *ret)
{
	return minus_float_fs_alloc_(NULL, left, right, ret);
}

int minus_float_fd_heap_(addr left, addr right, addr *ret)
{
	return minus_float_fd_alloc_(NULL, left, right, ret);
}

int minus_float_fl_heap_(addr left, addr right, addr *ret)
{
	return minus_float_fl_alloc_(NULL, left, right, ret);
}

int minus_float_sf_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &value);
	if (value == 0) {
		single_float_throw_alloc(local, left, ret);
		return 0;
	}
	else {
		return minus_float_sv_alloc_(local, left, (single_float)value, ret);
	}
}

int minus_float_df_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &value);
	if (value == 0) {
		double_float_throw_alloc(local, left, ret);
		return 0;
	}
	else {
		return minus_float_dv_alloc_(local, left, (double_float)value, ret);
	}
}

int minus_float_lf_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &value);
	if (value == 0) {
		long_float_throw_alloc(local, left, ret);
		return 0;
	}
	else {
		return minus_float_lv_alloc_(local, left, (long_float)value, ret);
	}
}

int minus_float_sf_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_sf_alloc_(local, left, right, ret);
}

int minus_float_df_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_df_alloc_(local, left, right, ret);
}

int minus_float_lf_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_lf_alloc_(local, left, right, ret);
}

int minus_float_sf_heap_(addr left, addr right, addr *ret)
{
	return minus_float_sf_alloc_(NULL, left, right, ret);
}

int minus_float_df_heap_(addr left, addr right, addr *ret)
{
	return minus_float_df_alloc_(NULL, left, right, ret);
}

int minus_float_lf_heap_(addr left, addr right, addr *ret)
{
	return minus_float_lf_alloc_(NULL, left, right, ret);
}

int minus_float_bs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type right error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type left error");
	if (zerop_bignum(left)) {
		sign_reverse_floats_heap(right, ret);
		return 0;
	}
	else {
		Return(single_float_bignum_(left, &value));
		return minus_float_vs_alloc_(local, value, right, ret);
	}
}

int minus_float_bd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_bignum(left)) {
		sign_reverse_floatd_heap(right, ret);
		return 0;
	}
	else {
		Return(double_float_bignum_(left, &value));
		return minus_float_vd_alloc_(local, value, right, ret);
	}
}

int minus_float_bl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_bignum(left)) {
		sign_reverse_floatl_heap(right, ret);
		return 0;
	}
	else {
		Return(long_float_bignum_(left, &value));
		return minus_float_vl_alloc_(local, value, right, ret);
	}
}

int minus_float_bs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_bs_alloc_(local, left, right, ret);
}

int minus_float_bd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_bd_alloc_(local, left, right, ret);
}

int minus_float_bl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_bl_alloc_(local, left, right, ret);
}

int minus_float_bs_heap_(addr left, addr right, addr *ret)
{
	return minus_float_bs_alloc_(NULL, left, right, ret);
}

int minus_float_bd_heap_(addr left, addr right, addr *ret)
{
	return minus_float_bd_alloc_(NULL, left, right, ret);
}

int minus_float_bl_heap_(addr left, addr right, addr *ret)
{
	return minus_float_bl_alloc_(NULL, left, right, ret);
}

int minus_float_sb_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	Return(single_float_bignum_(right, &value));
	return minus_float_sv_alloc_(local, left, value, ret);
}

int minus_float_db_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	Return(double_float_bignum_(right, &value));
	return minus_float_dv_alloc_(local, left, value, ret);
}

int minus_float_lb_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	Return(long_float_bignum_(right, &value));
	return minus_float_lv_alloc_(local, left, value, ret);
}

int minus_float_sb_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_sb_alloc_(local, left, right, ret);
}

int minus_float_db_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_db_alloc_(local, left, right, ret);
}

int minus_float_lb_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_lb_alloc_(local, left, right, ret);
}

int minus_float_sb_heap_(addr left, addr right, addr *ret)
{
	return minus_float_sb_alloc_(NULL, left, right, ret);
}

int minus_float_db_heap_(addr left, addr right, addr *ret)
{
	return minus_float_db_alloc_(NULL, left, right, ret);
}

int minus_float_lb_heap_(addr left, addr right, addr *ret)
{
	return minus_float_lb_alloc_(NULL, left, right, ret);
}

int minus_float_rs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	if (zerop_ratio(left)) {
		sign_reverse_floats_heap(right, ret);
		return 0;
	}
	else {
		Return(single_float_ratio_(left, &value));
		return minus_float_vs_alloc_(local, value, right, ret);
	}
}

int minus_float_rd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_ratio(left)) {
		sign_reverse_floatd_heap(right, ret);
		return 0;
	}
	else {
		Return(double_float_ratio_(left, &value));
		return minus_float_vd_alloc_(local, value, right, ret);
	}
}

int minus_float_rl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_ratio(left)) {
		sign_reverse_floatl_heap(right, ret);
		return 0;
	}
	else {
		Return(long_float_ratio_(left, &value));
		return minus_float_vl_alloc_(local, value, right, ret);
	}
}

int minus_float_rs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_rs_alloc_(local, left, right, ret);
}

int minus_float_rd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_rd_alloc_(local, left, right, ret);
}

int minus_float_rl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_rl_alloc_(local, left, right, ret);
}

int minus_float_rs_heap_(addr left, addr right, addr *ret)
{
	return minus_float_rs_alloc_(NULL, left, right, ret);
}

int minus_float_rd_heap_(addr left, addr right, addr *ret)
{
	return minus_float_rd_alloc_(NULL, left, right, ret);
}

int minus_float_rl_heap_(addr left, addr right, addr *ret)
{
	return minus_float_rl_alloc_(NULL, left, right, ret);
}

int minus_float_sr_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	Return(single_float_ratio_(right, &value));
	return minus_float_sv_alloc_(local, left, value, ret);
}

int minus_float_dr_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	Return(double_float_ratio_(right, &value));
	return minus_float_dv_alloc_(local, left, value, ret);
}

int minus_float_lr_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	Return(long_float_ratio_(right, &value));
	return minus_float_lv_alloc_(local, left, value, ret);
}

int minus_float_sr_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_sr_alloc_(local, left, right, ret);
}

int minus_float_dr_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_dr_alloc_(local, left, right, ret);
}

int minus_float_lr_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_lr_alloc_(local, left, right, ret);
}

int minus_float_sr_heap_(addr left, addr right, addr *ret)
{
	return minus_float_sr_alloc_(NULL, left, right, ret);
}

int minus_float_dr_heap_(addr left, addr right, addr *ret)
{
	return minus_float_dr_alloc_(NULL, left, right, ret);
}

int minus_float_lr_heap_(addr left, addr right, addr *ret)
{
	return minus_float_lr_alloc_(NULL, left, right, ret);
}

int minus_float_ss_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefSingleFloat(left) - RefSingleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

int minus_float_sd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = ((double_float)RefSingleFloat(left)) - RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int minus_float_sl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefSingleFloat(left)) - RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int minus_float_ds_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) - ((double_float)RefSingleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int minus_float_dd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) - RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int minus_float_dl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefDoubleFloat(left)) - RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int minus_float_ls_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefLongFloat(left) - ((long_float)RefSingleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int minus_float_ld_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefLongFloat(left) - ((long_float)RefDoubleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int minus_float_ll_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = RefLongFloat(left) - RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int minus_float_ss_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_ss_alloc_(local, left, right, ret);
}

int minus_float_sd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_sd_alloc_(local, left, right, ret);
}

int minus_float_sl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_sl_alloc_(local, left, right, ret);
}

int minus_float_ds_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_ds_alloc_(local, left, right, ret);
}

int minus_float_dd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_dd_alloc_(local, left, right, ret);
}

int minus_float_dl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_dl_alloc_(local, left, right, ret);
}

int minus_float_ls_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_ls_alloc_(local, left, right, ret);
}

int minus_float_ld_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_ld_alloc_(local, left, right, ret);
}

int minus_float_ll_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_ll_alloc_(local, left, right, ret);
}

int minus_float_ss_heap_(addr left, addr right, addr *ret)
{
	return minus_float_ss_alloc_(NULL, left, right, ret);
}

int minus_float_sd_heap_(addr left, addr right, addr *ret)
{
	return minus_float_sd_alloc_(NULL, left, right, ret);
}

int minus_float_sl_heap_(addr left, addr right, addr *ret)
{
	return minus_float_sl_alloc_(NULL, left, right, ret);
}

int minus_float_ds_heap_(addr left, addr right, addr *ret)
{
	return minus_float_ds_alloc_(NULL, left, right, ret);
}

int minus_float_dd_heap_(addr left, addr right, addr *ret)
{
	return minus_float_dd_alloc_(NULL, left, right, ret);
}

int minus_float_dl_heap_(addr left, addr right, addr *ret)
{
	return minus_float_dl_alloc_(NULL, left, right, ret);
}

int minus_float_ls_heap_(addr left, addr right, addr *ret)
{
	return minus_float_ls_alloc_(NULL, left, right, ret);
}

int minus_float_ld_heap_(addr left, addr right, addr *ret)
{
	return minus_float_ld_alloc_(NULL, left, right, ret);
}

int minus_float_ll_heap_(addr left, addr right, addr *ret)
{
	return minus_float_ll_alloc_(NULL, left, right, ret);
}


/************************************************************
 *  format.c
 ************************************************************/

/*****************************************************************************
 *  format
 *****************************************************************************/
/*
 * ~args opt operator
 *
 * arg -> [+-]?[0-9]+ | '. | [vV] | #
 * args -> (arg (, arg)*)?
 * opt -> : | @ | :@ | @:
 * operator -> aAsSdDbBoOxXrRpPcCfFeEgG$%&|~\ntT*?_wWiI()[];{}<>^
 *
 * vV		argument
 * #		count
 *
 * aA		Aesthetic
 * sS		Standard
 * dD		Decimal
 * bB		Binary
 * oO		Octal
 * xX		Hexadecimal
 * rR		Radix
 * pP		Plural
 * cC		Character
 * fF		Fixed-format floating-point
 * eE		Exponential floating-point
 * gG		General floating-point
 * $		Monetary floating-point
 * %		Newline
 * &		Fresh-Line
 * |		Page
 * ~		Tilde
 * \n		Ignored Newline
 * tT		Tabulate
 * *		Go-To
 * ?		Recursive Processing
 * _		Conditional Newline
 * wW		Write
 * iI		Indent
 * ()		Case Conversion
 * []		Conditional Expression
 * ;		Clause Separator
 * {}		Iteration
 * <>		Justification, Logical Block
 * ^		Escape Upward
 */
int format_stream_lisp_(Execute ptr, addr stream, addr format, addr args)
{
	return format_execute_(ptr, stream, format, args, &args);
}

int format_string_lisp_(Execute ptr, addr format, addr args, addr *ret)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	if (format_stream_lisp_(ptr, stream, format, args)) {
		close_output_string_stream(stream);
		return 1;
	}
	Return(string_stream_heap_(stream, ret));
	close_output_string_stream(stream);

	return 0;
}

static int format_array_lisp(Execute ptr, addr array, addr format, addr args, addr *ret)
{
	int result;
	addr stream;

	open_extend_output_stream(&stream, array);
	result = format_stream_lisp_(ptr, stream, format, args);
	close_output_string_stream(stream);
	*ret = array;

	return result;
}

int format_lisp_(Execute ptr, addr stream, addr format, addr args, addr *ret)
{
	if (stream == Nil)
		return format_string_lisp_(ptr, format, args, ret);

	if (stream == T) {
		Return(standard_output_stream_(ptr, &stream));
		return format_stream_lisp_(ptr, stream, format, args);
	}

	if (stringp(stream))
		return format_array_lisp(ptr, stream, format, args, ret);

	return format_stream_lisp_(ptr, stream, format, args);
}


/*
 *  format clang
 */
static int format_stdarg_(Execute ptr,
		addr stream, const char *str, va_list args, addr *ret)
{
	addr format, list;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	strvect_char_local(local, &format, str);
	list_stdarg_alloc(local, &list, args);
	if (format_lisp_(ptr, stream, format, list, ret))
		return 1;
	rollback_local(local, stack);

	return 0;
}

int format_stream_(Execute ptr, addr stream, const char *str, ...)
{
	int check;
	va_list args;

	va_start(args, str);
	check = format_stdarg_(ptr, stream, str, args, NULL);
	va_end(args);

	return check;
}

int format_string_(Execute ptr, addr *ret, const char *str, ...)
{
	int check;
	va_list args;

	va_start(args, str);
	check = format_stdarg_(ptr, Nil, str, args, ret);
	va_end(args);

	return check;
}

int format_stdout_(Execute ptr, const char *str, ...)
{
	int check;
	va_list args;

	va_start(args, str);
	check = format_stdarg_(ptr, T, str, args, NULL);
	va_end(args);

	return check;
}

void formatf(const char *str, ...)
{
	int check;
	va_list args;

	va_start(args, str);
	check = format_stdarg_(Execute_Thread, T, str, args, NULL);
	va_end(args);
	if (check) {
		Abort("format error.");
	}
}


/*
 *  initialize
 */
void init_format(void)
{
	init_format_parse();
	init_format_function();
}


/************************************************************
 *  format_float.c
 ************************************************************/
/*
 *  Common-Lisp Floating-point formatter
 */

#define FMTDECIMAL_FLOAT_PARSE	"%+20.10e"
#define FMTDECIMAL_DOUBLE_PARSE	"%+30.20e"
#ifdef LISP_FLOAT_LONG_64
#define FMTDECIMAL_LONG_PARSE	FMTDECIMAL_DOUBLE_PARSE
#else
#define FMTDECIMAL_LONG_PARSE	"%+40.30Le"
#endif

#ifdef LISP_DEGRADE
#define WriteChar_		fmtfloat_write_char_
#define PrintAscii_		fmtfloat_print_ascii_
int fmtfloat_write_char_(addr stream, unicode c);
int fmtfloat_print_ascii_(addr stream, const char *);
#else
#define WriteChar_		write_char_stream_
#define PrintAscii_		print_ascii_stream_
#endif


/*****************************************************************************
 *  fmtdecimal
 *****************************************************************************/
static int fmtdecimal_parse(fmtdecimal str, const byte *input)
{
	int sign, exponent;
	byte buffer[FMTDECIMAL_EXPONENT];
	byte *ptr;
	unsigned i, x, y, z;

	ptr = str->fraction;
	sign = exponent = 0;
	i = x = y = z = 0;
	while (*input && *input == ' ') {
		input++;
	}
	if (*input == 0) {
		return 1;
	}
	if (*input == '+') {
		input++;
		goto first;
	}
	if (*input == '-') {
		sign = 1;
		input++;
		goto first;
	}
first:
	if (*input == 0) {
		return 1;
	}
	if (*input == '0') {
		ptr[i++] = *input - '0';
		if (FMTDECIMAL_FRACTION <= i)
			return 1;
		x++;
		input++;
	}
ignore:
	if (*input && *input == '0') {
		input++;
		goto ignore;
	}
loop:
	if (*input == 0) {
		goto return_value;
	}
	if (*input == '.') {
		input++;
		goto loop;
	}
	if (*input == 'e' || *input == 'E') {
		input++;
		goto exponent;
	}
	if (! isdigit(*input)) {
		return 1;
	}
	ptr[i++] = *input - '0';
	if (FMTDECIMAL_FRACTION <= i)
		return 1;
	x++;
	input++;
	goto loop;

exponent:
	if (*input == 0) {
		return 1;
	}
	if (*input == '-') {
		buffer[z++] = *input;
		if (FMTDECIMAL_EXPONENT <= z)
			return 1;
		input++;
		goto exploop;
	}
	if (*input == '+') {
		input++;
		goto exploop;
	}

exploop:
	if (*input == 0) {
		goto expfinish;
	}
	if (! isdigit(*input)) {
		goto reject;
	}
	buffer[z++] = *input;
	if (FMTDECIMAL_EXPONENT <= z)
		return 1;
	input++;
	goto exploop;

reject:
	if (*input && *input == ' ') {
		input++;
		goto reject;
	}
	if (*input) {
		return 1;
	}
	goto expfinish;

expfinish:
	buffer[z] = 0;
	exponent = atoi((const char *)buffer);
	goto return_value;

return_value:
	str->sign = sign;
	str->exponent = exponent;
	str->size = x;

	return 0;
}

static void fmtdecimal_press(fmtdecimal str)
{
	byte *ptr;
	unsigned size;

	ptr = str->fraction;
	size = str->size;
	while (2 <= size) {
		if (ptr[size - 1] != 0)
			break;
		size--;
		ptr[size] = 0;
	}
	str->size = size;
}

static void fmtdecimal_zero(fmtdecimal str)
{
	str->size = 1;
	str->exponent = 0;
	str->fraction[0] = 0;
}

static void fmtdecimal_left(fmtdecimal str)
{
	byte *ptr;
	unsigned i, size, mount;

	/* zero */
	size = str->size;
	if (size <= 1)
		return;

	/* check */
	ptr = str->fraction;
	for (mount = 0; mount < size; mount++) {
		if (ptr[mount] != 0)
			break;
	}
	if (mount == 0)
		return;

	/* shift */
	if (size == mount) {
		fmtdecimal_zero(str);
	}
	else {
		size -= mount;
		for (i = 0; i < size; i++) {
			ptr[i] = ptr[mount + i];
		}
		str->size = size;
		str->exponent -= mount;
	}
}

static void fmtdecimal_right(fmtdecimal str, byte c)
{
	byte *ptr, x;
	unsigned i, size;

	if (c == 0)
		return;
	ptr = str->fraction;
	size = str->size;
	for (i = 0; i < FMTDECIMAL_FRACTION && i < size; i++) {
		x = ptr[i];
		ptr[i] = c;
		c = x;
	}
	if (i < FMTDECIMAL_FRACTION) {
		ptr[i++] = c;
	}
	str->size = i;
	str->exponent++;
}

static int fmtdecimal_input(fmtdecimal str, const byte *input)
{
	if (fmtdecimal_parse(str, input)) {
		/* parse error */
		return 1;
	}
	else {
		fmtdecimal_press(str);
		fmtdecimal_left(str);
		return 0;
	}
}

static void fmtdecimal_carry(fmtdecimal str, unsigned i)
{
	byte *ptr, carry;

	ptr = str->fraction;
	for (carry = 1; carry; i--) {
		carry += ptr[i];
		if (carry <= 9) {
			ptr[i] = carry;
			break;
		}
		else {
			ptr[i] = carry % 10;
			carry = 1;
			if (i == 0) {
				fmtdecimal_right(str, carry);
				break;
			}
		}
	}
}

int fmtdecimal_zerop(fmtdecimal str)
{
	return str->size == 1 && str->fraction[0] == 0;
}

int fmtdecimal_round(fmtdecimal str, unsigned i)
{
	int check, zerop;
	byte *ptr;

	if (str->size <= i) {
		return 0;
	}
	ptr = str->fraction;
	check = 0;
	zerop = fmtdecimal_zerop(str);
	if (str->size != i) {
		str->size = i;
		check = 1;
	}
	if (ptr[i] <= 4) {
		/* reject 4 */
		if (i == 0) {
			check = ! zerop;
			fmtdecimal_zero(str);
		}
	}
	else {
		/* carryup 5 */
		if (i == 0) {
			ptr[0] = 0;
			ptr[1] = 1;
			str->size = 2;
		}
		else {
			fmtdecimal_carry(str, i - 1);
		}
		check = 1;
	}
	fmtdecimal_press(str);
	fmtdecimal_left(str);

	return check;
}

static int fmtdecimal_single_parse(fmtdecimal str, const char *fmt, single_float value)
{
	byte buffer[FMTDECIMAL_FRACTION];
	snprintc((char *)buffer, FMTDECIMAL_FRACTION, fmt, value);
	return fmtdecimal_input(str, buffer);
}

static int fmtdecimal_double_parse(fmtdecimal str, const char *fmt, double_float value)
{
	byte buffer[FMTDECIMAL_FRACTION];
	snprintc((char *)buffer, FMTDECIMAL_FRACTION, fmt, value);
	return fmtdecimal_input(str, buffer);
}

static int fmtdecimal_long_parse(fmtdecimal str, const char *fmt, long_float value)
{
	byte buffer[FMTDECIMAL_FRACTION];
	snprintc((char *)buffer, FMTDECIMAL_FRACTION, fmt, value);
	return fmtdecimal_input(str, buffer);
}

int fmtdecimal_single_float(fmtdecimal str, single_float value, int round)
{
	if (fmtdecimal_single_parse(str, FMTDECIMAL_FLOAT_PARSE, value))
		return 1;
	if (0 <= round)
		fmtdecimal_round(str, (unsigned)round);
	return 0;
}

int fmtdecimal_double_float(fmtdecimal str, double_float value, int round)
{
	if (fmtdecimal_double_parse(str, FMTDECIMAL_DOUBLE_PARSE, value))
		return 1;
	if (0 <= round)
		fmtdecimal_round(str, (unsigned)round);
	return 0;
}

int fmtdecimal_long_float(fmtdecimal str, long_float value, int round)
{
	if (fmtdecimal_long_parse(str, FMTDECIMAL_LONG_PARSE, value))
		return 1;
	if (0 <= round)
		fmtdecimal_round(str, (unsigned)round);
	return 0;
}

static int fmtdecimal_char(byte c)
{
	if (c <= 9)
		return '0' + c;
	else
		return '.';
}

void fmtdecimal_dump(FILE *file, fmtdecimal str)
{
	byte *ptr;
	unsigned size, i, c;

	ptr = str->fraction;
	size = str->size;
	if (size == 0) {
		fprintf(file, " 0: %c*.", str->sign? '-': '+');
	}
	else {
		c = fmtdecimal_char(ptr[0]);
		fprintf(file, "%2u: %c%c.", size, str->sign? '-': '+', c);
		for (i = 1; i < FMTDECIMAL_FRACTION; i++) {
			c = fmtdecimal_char(ptr[i]);
			fprintf(file, "%c", (i < size)? c: '*');
		}
	}
	fprintf(file, " e%+03" PRIdF "\n", str->exponent);
	fflush(file);
}


/*****************************************************************************
 *  fmtfloat
 *****************************************************************************/
static int fmtfixed_round_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t i, int *ret)
{
	if (fmtdecimal_round(dec, (unsigned)i)) {
		Return(fmtfloat_fixed_(stream, fmt, dec));
		return Result(ret, 1);
	}
	return Result(ret, 0);
}

static int fmtexponent_round_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t i, int *ret)
{
	if (fmtdecimal_round(dec, (unsigned)i)) {
		Return(fmtfloat_exponent_(stream, fmt, dec));
		return Result(ret, 1);
	}
	return Result(ret, 0);
}

static int decimal_stream_(addr stream, byte c)
{
	Check(! (0 <= c && c <= 9), "decimal_stream_ error");
	return WriteChar_(stream, '0' + c);
}

static int decimalcheck_stream_(addr stream, fmtdecimal dec, size_t index)
{
	Check(dec->size <= index, "fmtdecimal index error.");
	return decimal_stream_(stream, dec->fraction[index]);
}

static int decimalzero_stream_(addr stream, fmtdecimal dec, size_t index)
{
	return decimal_stream_(stream, (index < dec->size)? dec->fraction[index]: 0);
}

static int fixed_large_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum count, i;

	count = dec->exponent + fmt->k + 1;
	if (count <= 0)
		return WriteChar_(stream, '0');
	for (i = 0; i < count; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}

	return 0;
}

static int times_stream_(addr stream, size_t size, unicode u)
{
	size_t i;

	for (i = 0; i < size; i++) {
		Return(WriteChar_(stream, u));
	}

	return 0;
}

static int for_decimalcheck_stream_(addr stream,
		fmtdecimal dec, size_t index, size_t size)
{
	size_t i;

	for (i = index; i < size; i++) {
		Return(decimalcheck_stream_(stream, dec, i));
	}

	return 0;
}

static int for_decimalzero_stream_(addr stream,
		fmtdecimal dec, size_t index, size_t size)
{
	size_t i;

	for (i = index; i < size; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}

	return 0;
}

static int fixed_small_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum k;
	size_t size, ka;

	size = dec->size;
	k = dec->exponent + fmt->k + 1;
	ka = (size_t)(k < 0? -k: k);

	if (k < 0) {
		Return(times_stream_(stream, ka, '0'));
		Return(for_decimalcheck_stream_(stream, dec, 0, size));
	}
	else if (ka < size) {
		Return(for_decimalcheck_stream_(stream, dec, ka, size));
	}
	else {
		Return(WriteChar_(stream, '0'));
	}

	return 0;
}

static fixnum getexponent(fmtfloat fmt, fmtdecimal dec)
{
	return dec->exponent - (fmt->k - fmt->k_bias);
}

static int expsign_stream_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum value;

	value = getexponent(fmt, dec);
	if (value < 0) {
		Return(WriteChar_(stream, '-'));
	}
	else if (fmt->sign_exponent) {
		Return(WriteChar_(stream, '+'));
	}

	return 0;
}

static int exponent_stream_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum value;
	char buffer[32];
	size_t size, i;

	value = getexponent(fmt, dec);
	if (value < 0)
		value = -value; /* no carry */
	snprintf(buffer, 32, "%" PRIdF, value);

	if (fmt->ep) {
		size = strlen(buffer);
		if (size < fmt->e) {
			size = fmt->e - size;
			Return(WriteChar_(stream, fmt->marker));
			Return(expsign_stream_(stream, fmt, dec));
			for (i = 0; i < size; i++) {
				Return(WriteChar_(stream, '0'));
			}
			return PrintAscii_(stream, buffer);
		}
	}

	Return(WriteChar_(stream, fmt->marker));
	Return(expsign_stream_(stream, fmt, dec));
	return PrintAscii_(stream, buffer);
}

static int sign_stream_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	if (fmt->signbit) {
		Return(WriteChar_(stream, '-'));
	}
	else if (fmt->sign) {
		Return(WriteChar_(stream, '+'));
	}

	return 0;
}

static int fixed_free_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	Return(sign_stream_(stream, fmt, dec));
	Return(fixed_large_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	Return(fixed_small_(stream, fmt, dec));
	/* exponent */
	if (fmt->markerp) {
		Return(WriteChar_(stream, fmt->marker));
		Return(WriteChar_(stream, '0'));
	}

	return 0;
}

static size_t getsignsize(fmtfloat fmt, fmtdecimal dec)
{
	if (dec->sign)
		return 1;
	else if (fmt->sign)
		return 1;
	else
		return 0;
}

static int overflow_stream_(addr stream, fmtfloat fmt)
{
	return times_stream_(stream, fmt->w, fmt->overflow);
}

static int margin_stream_(addr stream, fmtfloat fmt, size_t size)
{
	return times_stream_(stream, size, fmt->pad);
}

static int fixed_width_underflow_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t kb)
{
	Return(sign_stream_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	if (size == kb) {
		fmtdecimal_round(dec, 0);
		Return(times_stream_(stream, size - 1, '0'));
		Return(decimalcheck_stream_(stream, dec, 0));
	}
	else {
		Return(times_stream_(stream, size, '0'));
	}

	return 0;
}

static int fixed_width_small2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t kb)
{
	int check;
	size_t info;

	info = size - kb;
	Return(fmtfixed_round_(stream, fmt, dec, info, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	Return(times_stream_(stream, kb, '0'));
	return for_decimalcheck_stream_(stream, dec, 0, info);
}

static int fixed_width_small1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t kb)
{
	size_t margin, dsize;

	/* no round */
	dsize = dec->size;
	margin = size - dsize - kb - 1;
	Return(margin_stream_(stream, fmt, margin));
	Return(sign_stream_(stream, fmt, dec));
	Return(PrintAscii_(stream, "0."));
	Return(times_stream_(stream, kb, '0'));
	return for_decimalcheck_stream_(stream, dec, 0, dsize);
}

static int fixed_width_large1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t margin, size_t psize, size_t kb)
{
	int check;

	Return(fmtfixed_round_(stream, fmt, dec, psize, &check));
	if (check)
		return 0;
	Return(margin_stream_(stream, fmt, margin));
	Return(sign_stream_(stream, fmt, dec));
	Return(fixed_large_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	return for_decimalcheck_stream_(stream, dec, kb, psize);
}

static int fixed_width_large2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t kb)
{
	int check;
	size_t margin, tail;

	Return(fmtfixed_round_(stream, fmt, dec, kb + 1, &check));
	if (check)
		return 0;
	margin = size - kb - 1;
	tail = kb - size;

	Return(margin_stream_(stream, fmt, margin));
	Return(sign_stream_(stream, fmt, dec));
	Return(fixed_large_(stream, fmt, dec));
	Return(for_decimalzero_stream_(stream, dec, tail, dec->size));
	Return(WriteChar_(stream, '.'));
	return decimalzero_stream_(stream, dec, kb + 1);
}

static int fixed_width_large3_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t kb)
{
	int check;

	Return(fmtfixed_round_(stream, fmt, dec, kb, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(fixed_large_(stream, fmt, dec));
	return WriteChar_(stream, '.');
}

static int fixed_width_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	int check;
	fixnum k, ke;
	size_t w, kb, size, dsize, psize;

	/* ~w,,,o,cF */
	w = fmt->w;
	k = fmt->k;
	ke = k + dec->exponent + 1;
	kb = (size_t)(ke < 0? -ke: ke);

	/* overflow */
	dsize = dec->size;
	size = getsignsize(fmt, dec) + 1;
	check = fmt->overflowp &&
		((ke < 0 && w < (size + 1)) || (0 <= ke && w < (size + kb)));
	if (check)
		return overflow_stream_(stream, fmt);
	size = (w <= size)? 0: (w - size);

	/* underflow */
	if (ke < 0 && size <= kb)
		return fixed_width_underflow_(stream, fmt, dec, size, kb);

	/* small2 */
	if (ke <= 0 && (size <= dsize + kb))
		return fixed_width_small2_(stream, fmt, dec, size, kb);

	/* small1 */
	if (ke <= 0)
		return fixed_width_small1_(stream, fmt, dec, size, kb);

	/* large1 */
	psize = size < dsize? size: dsize;
	if (kb < psize)
		return fixed_width_large1_(stream, fmt, dec, size - psize, psize, kb);

	/* large2 */
	if (kb < size)
		return fixed_width_large2_(stream, fmt, dec, size, kb);

	/* large3 (overflow) */
	return fixed_width_large3_(stream, fmt, dec, kb);
}

static int fixed_column_small2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t ka)
{
	Return(sign_stream_(stream, fmt, dec));
	Return(PrintAscii_(stream, "0."));
	return times_stream_(stream, ka, '0');
}

static int fixed_column_small1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t ka)
{
	int check;

	Return(fmtfixed_round_(stream, fmt, dec, d - ka, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(PrintAscii_(stream, "0."));
	Return(times_stream_(stream, ka, '0'));
	return for_decimalzero_stream_(stream, dec, 0, d - ka);
}

static int fixed_column_large1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t ka)
{
	int check;

	Return(fmtfixed_round_(stream, fmt, dec, ka + d, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(fixed_large_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	return for_decimalzero_stream_(stream, dec, ka, ka + d);
}

static int fixed_column_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum k;
	size_t d, ka;

	/* ~,d,,o,cF */
	d = fmt->d;
	k = dec->exponent + fmt->k + 1;
	ka = (size_t)(k < 0? -k: k);

	/* small2 */
	if (k < 0 && d <= ka)
		return fixed_column_small2_(stream, fmt, dec, ka);

	/* small1 */
	if (k <= 0)
		return fixed_column_small1_(stream, fmt, dec, d, ka);

	/* large1 */
	return fixed_column_large1_(stream, fmt, dec, d, ka);
}

static int fixed_limit_underflow_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t size, size_t kb)
{
	int zero;
	size_t margin;

	zero = (d < size);
	if ((d + zero) < size) {
		margin = size - (d + zero);
		Return(margin_stream_(stream, fmt, margin));
	}
	Return(sign_stream_(stream, fmt, dec));
	if (zero) {
		Return(WriteChar_(stream, '0'));
	}
	Return(WriteChar_(stream, '.'));
	if (d == kb) {
		fmtdecimal_round(dec, 0);
		Return(times_stream_(stream, d - 1, '0'));
		Return(decimalcheck_stream_(stream, dec, 0));
	}
	else {
		Return(times_stream_(stream, d, '0'));
	}

	return 0;
}

static int fixed_limit_small_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t size, size_t kb)
{
	int check, zero;
	size_t margin, info;

	info = d - kb;
	Return(fmtfixed_round_(stream, fmt, dec, info, &check));
	if (check)
		return 0;
	zero = (d < size);
	if ((d + zero) < size) {
		margin = size - (d + zero);
		Return(margin_stream_(stream, fmt, margin));
	}
	Return(sign_stream_(stream, fmt, dec));
	if (zero) {
		Return(WriteChar_(stream, '0'));
	}
	Return(WriteChar_(stream, '.'));
	Return(times_stream_(stream, kb, '0'));
	return for_decimalzero_stream_(stream, dec, 0, info);
}

static int fixed_limit_large1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t size, size_t kb)
{
	int check, zero;
	size_t margin, info;

	info = kb + d;
	Return(fmtfixed_round_(stream, fmt, dec, info, &check));
	if (check)
		return 0;
	zero = (d < size);
	if ((d + kb) < size) {
		margin = size - (d + kb);
		Return(margin_stream_(stream, fmt, margin));
	}
	Return(sign_stream_(stream, fmt, dec));
	if (zero) {
		Return(fixed_large_(stream, fmt, dec));
	}
	Return(WriteChar_(stream, '.'));
	return for_decimalzero_stream_(stream, dec, kb, d + kb);
}

static int fixed_limit_large2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t size, size_t kb)
{
	int check, zero;
	size_t info;

	info = kb + d;
	Return(fmtfixed_round_(stream, fmt, dec, info, &check));
	if (check)
		return 0;
	zero = (d < size) || dec->fraction[0] != 0;
	Return(sign_stream_(stream, fmt, dec));
	if (zero) {
		Return(fixed_large_(stream, fmt, dec));
	}
	Return(WriteChar_(stream, '.'));
	return for_decimalzero_stream_(stream, dec, kb, kb + d);
}

static int fixed_limit_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	int illegal;
	fixnum ke;
	size_t w, d, kb, size, psize;

	/* ~w,,,o,cF */
	w = fmt->w;
	d = fmt->d;
	ke = fmt->k + dec->exponent + 1;
	kb = (size_t)(ke < 0? -ke: ke);

	/* overflow */
	size = getsignsize(fmt, dec) + 1;
	illegal = (w < size + d);
	if (illegal) {
		w = d + size - 1;
	}
	psize = size + d;
	if (0 <= ke)
		psize += kb;
	if (fmt->overflowp && (illegal || (w < psize)))
		return overflow_stream_(stream, fmt);
	size = (w <= size)? 0: (w - size);

	/* underflow */
	if (ke < 0 && (d <= kb))
		return fixed_limit_underflow_(stream, fmt, dec, d, size, kb);

	/* small */
	if (ke <= 0)
		return fixed_limit_small_(stream, fmt, dec, d, size, kb);

	/* large1 */
	if (0 < ke && (! illegal) && (kb <= (size - d)))
		return fixed_limit_large1_(stream, fmt, dec, d, size, kb);

	/* large2 */
	return fixed_limit_large2_(stream, fmt, dec, d, size, kb);
}

int fmtfloat_fixed_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	int wp, dp, wpnot, dpnot;

	wp = fmt->wp;
	dp = fmt->dp;
	wpnot = ! wp;
	dpnot = ! dp;

	if (wpnot && dpnot) {
		return fixed_free_(stream, fmt, dec);
	}
	else if (dpnot) {
		return fixed_width_(stream, fmt, dec);
	}
	else if (wpnot) {
		return fixed_column_(stream, fmt, dec);
	}
	else {
		return fixed_limit_(stream, fmt, dec);
	}
}

static int exponent_fraction_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum index, i, first, size;

	index = fmt->k;
	size = (int)dec->size;

	first = 0;
	for (i = index; i <= 0; i++) {
		Return(WriteChar_(stream, '0'));
		if (first == 0) {
			Return(WriteChar_(stream, '.'));
			first = 1;
		}
	}

	for (i = 0; i < index; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}

	for (; i < size; i++) {
		if (first == 0) {
			Return(WriteChar_(stream, '.'));
			first = 1;
		}
		Return(decimalcheck_stream_(stream, dec, i));
	}

	if (first == 0) {
		Return(PrintAscii_(stream, ".0"));
	}

	return 0;
}

static int exponent_free_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	Return(sign_stream_(stream, fmt, dec));
	Return(exponent_fraction_(stream, fmt, dec));
	return exponent_stream_(stream, fmt, dec);
}

static size_t exponent_size(fmtfloat fmt, fmtdecimal dec)
{
	fixnum value;
	char buffer[32];
	size_t size;

	value = getexponent(fmt, dec);
	if (value < 0)
		value = -value; /* no carry */
	snprintf(buffer, 32, "%d", (int)value);
	size = strlen(buffer);
	if (fmt->ep && size < fmt->e)
		size = fmt->e;

	return + 1UL/*marker*/ + 1UL/*sign*/ + size;
}

static int exponent_width_overflow1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t ka)
{
	int check;

	Return(fmtexponent_round_(stream, fmt, dec, 1, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	Return(times_stream_(stream, ka, '0'));
	Return(decimalcheck_stream_(stream, dec, 0));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_width_small2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t ka)
{
	int check;
	size_t i, margin;

	Return(fmtexponent_round_(stream, fmt, dec, size - ka, &check));
	if (check)
		return 0;
	margin = size - (dec->size + ka  + 1);
	Return(margin_stream_(stream, fmt, margin));
	Return(sign_stream_(stream, fmt, dec));
	Return(PrintAscii_(stream, "0."));
	Return(times_stream_(stream, ka, '0'));
	size = dec->size;
	for (i = 0; i < size; i++) {
		Return(decimalcheck_stream_(stream, dec, i));
	}
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_width_small1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t ka)
{
	int check;
	size_t i;

	Return(fmtexponent_round_(stream, fmt, dec, size - ka, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	Return(times_stream_(stream, ka, '0'));
	size = dec->size;
	for (i = 0; i < size; i++) {
		Return(decimalcheck_stream_(stream, dec, i));
	}
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_width_large1_nomargin_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k)
{
	size_t i;

	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < size; i++) {
		if (i == k) {
			Return(WriteChar_(stream, '.'));
		}
		Return(decimalcheck_stream_(stream, dec, i));
	}
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_width_large1_small_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k)
{
	size_t i, margin;

	margin = size - dec->size;
	size = dec->size;
	Return(margin_stream_(stream, fmt, margin));
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < size; i++) {
		if (i == k) {
			Return(WriteChar_(stream, '.'));
		}
		Return(decimalcheck_stream_(stream, dec, i));
	}
	if (i == k) {
		Return(WriteChar_(stream, '.'));
	}
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_width_large1_large_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k)
{
	size_t i, margin;

	margin = size - k;
	Return(margin_stream_(stream, fmt, margin));
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < dec->size; i++) {
		Return(decimalcheck_stream_(stream, dec, i));
	}
	size = k - dec->size;
	Return(times_stream_(stream, size, '0'));
	Return(WriteChar_(stream, '.'));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_width_large1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k)
{
	int check;

	Return(fmtexponent_round_(stream, fmt, dec, size, &check));
	if (check)
		return 0;
	if (size <= dec->size) {
		return exponent_width_large1_nomargin_(stream, fmt, dec, size, k);
	}
	else if (k <= dec->size) {
		return exponent_width_large1_small_(stream, fmt, dec, size, k);
	}
	else {
		return exponent_width_large1_large_(stream, fmt, dec, size, k);
	}
}

static int exponent_width_large2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, fixnum k)
{
	int check;
	fixnum i;

	Return(fmtexponent_round_(stream, fmt, dec, k, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < k; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}
	Return(WriteChar_(stream, '.'));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_width_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	int check;
	fixnum k;
	size_t w, ka, size;

	w = fmt->w;
	k = fmt->k;
	ka = (size_t)(k < 0? -k: k);

	/* overflow */
	size = getsignsize(fmt, dec) + 1 + exponent_size(fmt, dec);
	check = fmt->overflowp &&
		((k < 0 && w <= (size + ka)) || (0 <= k && w < (size + ka)));
	if (check)
		return overflow_stream_(stream, fmt);
	size = (w <= size)? 0: (w - size);

	/* overflow1 */
	if (k < 0 && size <= ka)
		return exponent_width_overflow1_(stream, fmt, dec, ka);

	/* small2 */
	if (k <= 0 && (dec->size + ka < size))
		return exponent_width_small2_(stream, fmt, dec, size, ka);

	/* small1 */
	if (k <= 0)
		return exponent_width_small1_(stream, fmt, dec, size, ka);

	/* large1 */
	if (0 < k && ka < size)
		return exponent_width_large1_(stream, fmt, dec, size, ka);

	/* large2 */
	if (0 < k && size <= ka)
		return exponent_width_large2_(stream, fmt, dec, k);

	/* throw */
	return 0;
}

static int exponent_column_small2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t ka)
{
	int check;

	Return(fmtexponent_round_(stream, fmt, dec, 1, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(PrintAscii_(stream, "0."));
	Return(times_stream_(stream, ka, '0'));
	Return(decimalcheck_stream_(stream, dec, 0));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_column_small1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t ka)
{
	int check;
	size_t size, i;

	size = d - ka;
	Return(fmtexponent_round_(stream, fmt, dec, size, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(PrintAscii_(stream, "0."));
	Return(times_stream_(stream, ka, '0'));
	for (i = 0; i < dec->size; i++) {
		Return(decimalcheck_stream_(stream, dec, i));
	}
	Return(times_stream_(stream, size - dec->size, '0'));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_column_large1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t k)
{
	int check;
	size_t i, size;

	size = d + 1;
	Return(fmtexponent_round_(stream, fmt, dec, size, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < size; i++) {
		if (i == k) {
			Return(WriteChar_(stream, '.'));
		}
		Return(decimalzero_stream_(stream, dec, i));
	}
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_column_large2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t k)
{
	int check;
	size_t i;

	Return(fmtexponent_round_(stream, fmt, dec, k, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < k; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}
	Return(WriteChar_(stream, '.'));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_column_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum k;
	size_t d, ka;

	d = fmt->d;
	k = fmt->k;
	ka = (size_t)(k < 0? -k: k);

	/* small2 */
	if (k < 0 && d <= ka)
		return exponent_column_small2_(stream, fmt, dec, ka);

	/* small1 */
	if (k <= 0 && ka < d)
		return exponent_column_small1_(stream, fmt, dec, d, ka);

	/* large1 */
	if (0 < k && ka <= d)
		return exponent_column_large1_(stream, fmt, dec, d, ka);

	/* large2 */
	if (0 < k && d < ka)
		return exponent_column_large2_(stream, fmt, dec, ka);

	/* throw */
	return 0;
}

static int exponent_limit_overflow1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t ka)
{
	int check;

	Return(fmtexponent_round_(stream, fmt, dec, 1, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	Return(times_stream_(stream, ka, '0'));
	Return(decimalzero_stream_(stream, dec, 0));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_limit_small2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t ka)
{
	int check;
	size_t margin;

	Return(fmtexponent_round_(stream, fmt, dec, 1, &check));
	if (check)
		return 0;
	if (ka + 2 < size) {
		margin = size - (ka + 2);
		Return(margin_stream_(stream, fmt, margin));
	}
	Return(sign_stream_(stream, fmt, dec));
	if (size != ka + 1) {
		Return(WriteChar_(stream, '0'));
	}
	Return(WriteChar_(stream, '.'));
	Return(times_stream_(stream, ka, '0'));
	Return(decimalcheck_stream_(stream, dec, 0));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_limit_small1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t ka, size_t d)
{
	int check, zero;
	size_t margin, info;

	info = d - ka;
	Return(fmtexponent_round_(stream, fmt, dec, info, &check));
	if (check)
		return 0;
	zero = (d < size);
	if ((d + zero) < size) {
		margin = size - (d + zero);
		Return(margin_stream_(stream, fmt, margin));
	}
	Return(sign_stream_(stream, fmt, dec));
	if (zero) {
		Return(WriteChar_(stream, '0'));
	}
	Return(WriteChar_(stream, '.'));
	Return(times_stream_(stream, ka, '0'));
	Return(for_decimalzero_stream_(stream, dec, 0, info));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_limit_large1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k, size_t d)
{
	int check;
	size_t margin, info, i;

	info = d + 1;
	Return(fmtexponent_round_(stream, fmt, dec, info, &check));
	if (check)
		return 0;
	if (d + 1 < size) {
		margin = size - (d + 1);
		Return(margin_stream_(stream, fmt, margin));
	}
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < k; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}
	Return(WriteChar_(stream, '.'));
	for (; i < info; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_limit_large2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k)
{
	int check;
	size_t margin, i;

	Return(fmtexponent_round_(stream, fmt, dec, k, &check));
	if (check)
		return 0;
	if (k < size) {
		margin = size - k;
		Return(margin_stream_(stream, fmt, margin));
	}
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < k; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}
	Return(WriteChar_(stream, '.'));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_limit_overflow2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t k)
{
	int check;
	size_t i;

	Return(fmtexponent_round_(stream, fmt, dec, k, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < k; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}
	Return(WriteChar_(stream, '.'));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_limit_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	int check, illegal;
	fixnum k;
	size_t w, d, ka, size;

	w = fmt->w;
	d = fmt->d;
	k = fmt->k;
	ka = (size_t)(k < 0? -k: k);

	/* overflow */
	size = getsignsize(fmt, dec) + 1 + exponent_size(fmt, dec);
	illegal = (w < size + d);
	if (illegal) {
		w = d + size - 1;
	}
	check = ((k < 0 && w <= (size + ka)) || (0 <= k && w < (size + ka)));
	if (fmt->overflowp && (illegal || check))
		return overflow_stream_(stream, fmt);
	size = (w <= size)? 0: (w - size);

	/* overflow1 */
	if (k < 0 && size <= ka)
		return exponent_limit_overflow1_(stream, fmt, dec, ka);

	/* small2 */
	if (k <= 0 && d <= ka)
		return exponent_limit_small2_(stream, fmt, dec, size, ka);

	/* small1 */
	if (k <= 0)
		return exponent_limit_small1_(stream, fmt, dec, size, ka, d);

	/* large1 */
	if (0 < k && ka <= d)
		return exponent_limit_large1_(stream, fmt, dec, size, ka, d);

	/* large2 */
	if (0 < k && ka <= size)
		return exponent_limit_large2_(stream, fmt, dec, size, ka);

	/* overflow2 */
	if (0 < k && size < ka)
		return exponent_limit_overflow2_(stream, fmt, dec, ka);

	/* throw */
	return 0;
}

int fmtfloat_exponent_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	int wp, dp, wpnot, dpnot;

	wp = fmt->wp;
	dp = fmt->dp;
	wpnot = ! wp;
	dpnot = ! dp;

	if (wpnot && dpnot) {
		return exponent_free_(stream, fmt, dec);
	}
	else if (dpnot) {
		return exponent_width_(stream, fmt, dec);
	}
	else if (wpnot) {
		return exponent_column_(stream, fmt, dec);
	}
	else {
		return exponent_limit_(stream, fmt, dec);
	}
}

static int fmtfloat_general_fixedp(fmtdecimal dec, fmtfloat fmt)
{
	fixnum e;

	e = dec->exponent;
	if (e < -1) /* dec < 0.1 */
		return 0; /* ~E */
	if (! fmt->dp)
		return 1; /* ~F */
	if (e < 0 || ((size_t)e) <= fmt->d)
		return 1; /* ~F */
	else
		return 0; /* ~E */
}

int fmtfloat_general_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum n, ww, dd, d, ee, q;

	/* n = arg? (fixnum)(floorf(log10f(arg)) + 1.0f): 0; */
	n = dec->exponent;
	d = fmt->d;
	/* ee */
	ee = fmt->ep? fmt->e + 2: 4;
	/* ww */
	if (fmt->wp) {
		ww = fmt->w - ee;
		if (ww < 0) ww = 0;
	}
	else {
		ww = -1;
	}

	/* format */
	if (fmtfloat_general_fixedp(dec, fmt)) {
		q = (fixnum)dec->size;
		d = (n < 7)? n: 7;
		d = (q > d)? q: d;
		dd = d - n;
		/* call fmtfloat_fixed_ */
		if (ww < 0) {
			fmt->wp = 0;
			fmt->w = 0;
		}
		else {
			fmt->wp = 1;
			fmt->w = ww;
		}
		fmt->d = dd;
		fmt->k = 0;  /* default */
		fmt->k_bias = 0; /* 0 if fixed */
		fmt->markerp = 0;
		Return(fmtfloat_fixed_(stream, fmt, dec));
		return times_stream_(stream, ee, ' ');
	}
	else {
		/* call fmtfloat_exponent_ */
		fmt->k_bias = 1; /* 1 if exponential. */
		fmt->markerp = 1;
		fmt->sign_exponent = 1; /* for prin1 */
		return fmtfloat_exponent_(stream, fmt, dec);
	}
}

int fmtfloat_monetary_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum e, round, index;
	size_t d, n, w, sign, n1, n2, size, p, i;

	d = fmt->dp? fmt->d: 2;
	n = fmt->np? fmt->n: 1;
	w = fmt->wp? fmt->w: 0;

	/* round */
	round = dec->exponent + d + 1;
	if (0 <= round)
		fmtdecimal_round(dec, (unsigned)round);
	e = dec->exponent;

	/* size */
	sign = getsignsize(fmt, dec);
	n2 = (e < 0)? 0: (1UL + (size_t)e);
	n1 = (n2 < n)? (n - n2): 0;
	size = sign + n2 + n1 + 1 + d;
	p = (size < w)? (w - size): 0;

	/* output */
	if (fmt->markerp) {
		/* sign -> padding */
		Return(sign_stream_(stream, fmt, dec));
		Return(margin_stream_(stream, fmt, p));
	}
	else {
		/* padding -> sign*/
		Return(margin_stream_(stream, fmt, p));
		Return(sign_stream_(stream, fmt, dec));
	}
	/* large */
	Return(times_stream_(stream, n1, '0'));
	Return(for_decimalzero_stream_(stream, dec, 0, n2));
	/* dot */
	Return(WriteChar_(stream, '.'));
	/* small */
	for (i = 0; i < d; i++) {
		index = ((int)i) + e + 1;
		if (index < 0) {
			Return(WriteChar_(stream, '0'));
		}
		else {
			Return(decimalzero_stream_(stream, dec, index));
		}
	}

	return 0;
}


/*
 *  ~F  Fixed floating-point
 */
int fmtfloat_fixed_float_(addr stream, single_float value,
		int sign,
		fixnum w,
		fixnum d,
		fixnum k,
		unicode overflow,
		unicode pad)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_single = value;
	fmt.signbit = signbit(value)? 1: 0;
	fmt.sign = sign;
	fmt.w = (size_t)w;
	fmt.wp = (0 <= w);
	fmt.d = (size_t)d;
	fmt.dp = (0 <= d);
	fmt.k = k; /* default 0 */
	fmt.k_bias = 0; /* 0 if fixed */
	fmt.overflow = overflow;
	fmt.overflowp = overflow != 0;
	fmt.pad = pad? pad: ' ';
	fmt.markerp = 0;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE)) {
		Abort("fmtdecimal_single_float error");
	}

	return fmtfloat_fixed_(stream, &fmt, &dec);
}

int fmtfloat_fixed_double_(addr stream, double_float value,
		int sign,
		fixnum w,
		fixnum d,
		fixnum k,
		unicode overflow,
		unicode pad)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_double = value;
	fmt.signbit = signbit(value)? 1: 0;
	fmt.sign = sign;
	fmt.w = (size_t)w;
	fmt.wp = (0 <= w);
	fmt.d = (size_t)d;
	fmt.dp = (0 <= d);
	fmt.k = k; /* default 0 */
	fmt.k_bias = 0; /* 0 if fixed */
	fmt.overflow = overflow;
	fmt.overflowp = overflow != 0;
	fmt.pad = pad? pad: ' ';
	fmt.markerp = 0;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE)) {
		Abort("fmtdecimal_double_float error");
	}

	return fmtfloat_fixed_(stream, &fmt, &dec);
}


/*
 *  ~E  Exponential floating-point
 */
int fmtfloat_exponent_float_(addr stream, single_float value,
		int sign,
		fixnum w,
		fixnum d,
		fixnum e,
		fixnum k,
		unicode overflow,
		unicode pad,
		unicode exponent)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_single = value;
	fmt.signbit = signbit(value)? 1: 0;

	fmt.sign = sign;
	fmt.w = (size_t)w;
	fmt.wp = (0 <= w);
	fmt.d = (size_t)d;
	fmt.dp = (0 <= d);
	fmt.ep = 1;
	fmt.e = e; /* default 2 -> "+1" */
	fmt.k = k; /* default 1 */
	fmt.k_bias = 1; /* 1 if exponential. */
	fmt.overflow = overflow;
	fmt.overflowp = overflow != 0;
	fmt.pad = pad? pad: ' ';

	fmt.markerp = 1;
	fmt.marker = exponent? exponent: 'E';
	fmt.sign_exponent = 1; /* for prin1 */

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE)) {
		Abort("fmtdecimal_single_float error");
	}

	return fmtfloat_exponent_(stream, &fmt, &dec);
}

int fmtfloat_exponent_double_(addr stream, double_float value,
		int sign,
		fixnum w,
		fixnum d,
		fixnum e,
		fixnum k,
		unicode overflow,
		unicode pad,
		unicode exponent)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_double = value;
	fmt.signbit = signbit(value)? 1: 0;

	fmt.sign = sign;
	fmt.w = (size_t)w;
	fmt.wp = (0 <= w);
	fmt.d = (size_t)d;
	fmt.dp = (0 <= d);
	fmt.ep = 1;
	fmt.e = e; /* default 2 -> "+1" */
	fmt.k = k; /* default 1 */
	fmt.k_bias = 1; /* 1 if exponential. */
	fmt.overflow = overflow;
	fmt.overflowp = overflow != 0;
	fmt.pad = pad? pad: ' ';

	fmt.markerp = 1;
	fmt.marker = exponent? exponent: 'E';
	fmt.sign_exponent = 1; /* for prin1 */

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE)) {
		Abort("fmtdecimal_double_float error");
	}

	return fmtfloat_exponent_(stream, &fmt, &dec);
}


/*****************************************************************************
 *  princ / prin1
 *****************************************************************************/
static int fmtfloat_princ_fixedp(fmtdecimal dec)
{
	/* return 1e-3 <= fabs(dec) < 1e7 */
	return -3 <= dec->exponent && dec->exponent < 7;
}

static int fmtfloat_princ_(addr stream, fmtfloat fmt, fmtdecimal dec,
		int markerp, unicode marker)
{
	if (fmtfloat_princ_fixedp(dec)) {
		fmt->k = 0;  /* default */
		fmt->k_bias = 0; /* 0 if fixed */
		fmt->markerp = markerp;
		if (markerp) {
			fmt->ep = 1;
			fmt->e = 1;
			fmt->markerp = 1;
			fmt->marker = marker;
			fmt->sign_exponent = 0; /* 0 if princ / prin1 */
		}
		return fmtfloat_fixed_(stream, fmt, dec);
	}
	else {
		fmt->k = 1; /* default 1 */
		fmt->k_bias = 1; /* 1 if exponential. */
		fmt->markerp = markerp;
		fmt->ep = 1;
		fmt->e = 1;
		fmt->markerp = 1;
		fmt->marker = marker;
		fmt->sign_exponent = 0; /* 0 if princ / prin1 */
		return fmtfloat_exponent_(stream, fmt, dec);
	}
}

int fmtfloat_princ_single_float_(addr stream,
		single_float value, int markerp, unicode marker, int *ret)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE))
		return Result(ret, 1);
	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_single = value;
	fmt.signbit = signbit(value)? 1: 0;
	Return(fmtfloat_princ_(stream, &fmt, &dec, markerp, marker));

	return Result(ret, 0);
}

int fmtfloat_princ_double_float_(addr stream,
		double_float value, int markerp, unicode marker, int *ret)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE))
		return Result(ret, 1);
	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_double = value;
	fmt.signbit = signbit(value)? 1: 0;
	Return(fmtfloat_princ_(stream, &fmt, &dec, markerp, marker));

	return Result(ret, 0);
}

int fmtfloat_princ_long_float_(addr stream,
		long_float value, int markerp, unicode marker, int *ret)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	if (fmtdecimal_long_float(&dec, value, FMTFLOAT_ROUND_LONG))
		return Result(ret, 1);
	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_long = value;
	fmt.signbit = signbit(value)? 1: 0;
	Return(fmtfloat_princ_(stream, &fmt, &dec, markerp, marker));

	return Result(ret, 0);
}


/************************************************************
 *  format_function.c
 ************************************************************/

typedef int (*fmtcalltype)(fmtprint, struct format_operator *str);
static fmtcalltype FormatCallTable[FormatType_size];

static struct format_argument *format_getargs(
		struct format_operator *str, size_t index)
{
	Check(str->size <= index, "index error");
	return (struct format_argument *)
		(((byte *)str)
		 + sizeoft(struct format_operator)
		 + sizeoft(struct format_argument) * index);
}


/*
 *  reader
 */
static int fmtint_count_(fmtprint print, fixnum *ret)
{
	size_t value;

	Return(length_list_safe_(print->rest->front, &value));
	Check(value < 0, "cast error");

	return Result(ret, (fixnum)value);
}

static int fmtint_argument_(fmtprint print, struct format_operator *str,
		fixnum *ret, int *check)
{
	addr pos;

	Return(fmtprint_pop_(print, str, &pos));
	if (pos == Nil) {
		*ret = 0;
		*check = 1;
		return 0;
	}
	if (bignump(pos)) {
		*ret = 0;
		*check = 1;
		return fmtprop_abort_(print, str,
				"Too large the format argument ~S.", pos, NULL);
	}
	if (! fixnump(pos)) {
		*ret = 0;
		*check = 1;
		return fmtprop_abort_(print, str,
				"The format argument ~S must be an integer.", pos, NULL);
	}
	GetFixnum(pos, ret);
	*check = 0;
	return 0;
}

static int fmtint_nilp_(fmtprint print, struct format_operator *str,
		unsigned index, fixnum *ret, int *check)
{
	struct format_argument *arg;

	Check(str->args_size <= index, "index error");
	arg = format_getargs(str, index);
	switch (arg->type) {
		case fmtargs_nil:
			*ret = 0;
			*check = 1;
			return 0;

		case fmtargs_integer:
			*ret = arg->u.value;
			*check = 0;
			return 0;

		case fmtargs_count:
			Return(fmtint_count_(print, ret));
			*check = 0;
			return 0;

		case fmtargs_argument:
			return fmtint_argument_(print, str, ret, check);

		default:
			*ret = 0;
			*check = 1;
			return fmtprop_abort_(print, str,
					"The format parameter must be an integer.", NULL);
	}
}

static int fmtchar_argument_(fmtprint print,
		struct format_operator *str, unicode *value, int *ret)
{
	addr pos;

	Return(fmtprint_pop_(print, str, &pos));
	if (pos == Nil)
		return Result(ret, 1);
	if (! characterp(pos)) {
		*value = 0;
		return fmtprop_abort_(print, str,
				"The format argument ~S must be a character.", pos, NULL);
	}
	GetCharacter(pos, value);
	return Result(ret, 0);
}

static int fmtchar_nilp_(fmtprint print,
		struct format_operator *str, unsigned index, unicode *value, int *ret)
{
	struct format_argument *arg;

	Check(str->args_size <= index, "index error");
	arg = format_getargs(str, index);
	switch (arg->type) {
		case fmtargs_nil:
			return Result(ret, 1);

		case fmtargs_character:
			*value = arg->u.character;
			return Result(ret, 0);

		case fmtargs_argument:
			return fmtchar_argument_(print, str, value, ret);

		case fmtargs_count:
		default:
			*value = 0;
			*ret = 0;
			return fmtprop_abort_(print, str,
					"The format argument must be a character.", NULL);
	}
}

static int fmtint_default_(fmtprint print, struct format_operator *str,
		unsigned index, fixnum *ret, fixnum defvar)
{
	int check;

	Return(fmtint_nilp_(print, str, index, ret, &check));
	if (check)
		*ret = defvar;

	return 0;
}

static int fmtchar_default_(fmtprint print, struct format_operator *str,
		unsigned index, unicode *ret, unicode defvar)
{
	int check;

	Return(fmtchar_nilp_(print, str, index, ret, &check));
	if (check)
		*ret = defvar;

	return 0;
}

static int fmtargs_abort_(fmtprint print,
		struct format_operator *fmt, unsigned index, const char *str, ...)
{
	struct format_argument *arg;
	va_list args;

	arg = format_getargs(fmt, index);
	va_start(args, str);
	Return(format_abort_(print->format, arg->position, str, args));
	va_end(args);

	return 0;
}


/*
 *  Error
 */
static int format_call_Error_(fmtprint print, struct format_operator *str)
{
	return fmtprop_abort_(print, str,
			"Cannot execute format operator [Error].", NULL);
}


/*
 *  End
 */
static int format_call_End_(fmtprint print, struct format_operator *str)
{
	return fmtprop_abort_(print, str,
			"Cannot execute format operator [End].", NULL);
}


/*
 *  Format
 */
static int format_call_Format_(fmtprint print, struct format_operator *str)
{
	return fmtprop_abort_(print, str,
			"Cannot execute format operator [Format].", NULL);
}


/*
 *  Output
 */
static int format_call_Output_(fmtprint print, struct format_operator *str)
{
	int delete_space;
	struct format_argument *arg;
	unicode u, *body;
	size_t size, i;

	/* argument */
	Check(3 < str->args_size, "size error");
	arg = format_getargs(str, 0);
	Check(arg->type != fmtargs_index, "type error");
	size = arg->u.index;

	/* body */
	delete_space = print->delete_space;
	body = (unicode *)format_getargs(str, 3);
	for (i = 0; i < size; i++) {
		u = body[i];
		if ((! delete_space) || (! isSpaceUnicode(u))) {
			Return(fmtprint_putc_(print, u));
			delete_space = 0;
		}
	}
	print->delete_space = delete_space;

	return 0;
}


/*
 *  Aesthetic
 */
static size_t format_colinc_division(size_t size, size_t colinc)
{
	if (size % colinc)
		size = (size / colinc) + 1;
	else
		size = size / colinc;

	return size * colinc;
}

static int format_write_margin_(fmtprint print, addr string,
		int atsign, fixnum mc, fixnum cl, fixnum mp, unicode padchar)
{
	size_t mincol, colinc, minpad, size, space, s;

	Check(mc < 0, "mincol error");
	Check(cl < 1, "colinc error");
	Check(mp < 0, "minpad error");
	mincol = (size_t)mc;
	colinc = (size_t)cl;
	minpad = (size_t)mp;
	Return(eastasian_length_(string, &size, NULL));

	/* output margin */
	space = minpad;
	s = size + space;
	if (s < mincol)
		space += format_colinc_division(mincol - s, colinc);

	if (atsign) {
		/* insert space in left */
		Return(fmtprint_putc_times_(print, padchar, space));
		Return(fmtprint_string_(print, string));
	}
	else {
		/* insert space in right */
		Return(fmtprint_string_(print, string));
		Return(fmtprint_putc_times_(print, padchar, space));
	}

	return 0;
}

static int format_call_print_(fmtprint print, addr pos, int colon, int atsign,
		fixnum mincol, fixnum colinc, fixnum minpad, unicode padchar,
		int prin1p)
{
	int check;
	addr stream;

	if (pos == Nil && colon) {
		strvect_char_local(print->local, &pos, "()");
	}
	else {
		Return(fmtprint_stream_(print, &stream));
		if (prin1p)
			check = prin1_print_(print->ptr, stream, pos);
		else
			check = princ_print_(print->ptr, stream, pos);
		if (check)
			return 1;
		Return(string_stream_local_(print->local, stream, &pos));
		clear_output_string_stream(stream);
	}

	return format_write_margin_(print, pos, atsign, mincol, colinc, minpad, padchar);
}

static int format_call_Aesthetic_(fmtprint print, struct format_operator *str)
{
	addr pos;
	fixnum mincol, colinc, minpad;
	unicode padchar;

	Check(4 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &mincol, 0));
	Return(fmtint_default_(print, str, 1, &colinc, 1));
	Return(fmtint_default_(print, str, 2, &minpad, 0));
	Return(fmtchar_default_(print, str, 3, &padchar, ' '));
	if (mincol < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}
	if (colinc < 1) {
		return fmtargs_abort_(print, str, 1,
				"The parameter must be greater than 1.", NULL);
	}
	if (minpad < 0) {
		return fmtargs_abort_(print, str, 2,
				"The parameter must be a positive integer.", NULL);
	}

	/* output */
	Return(fmtprint_pop_(print, str, &pos));
	return format_call_print_(print, pos, str->colon, str->atsign,
			mincol, colinc, minpad, padchar, 0);
}

static int format_call_Standard_(fmtprint print, struct format_operator *str)
{
	addr pos;
	fixnum mincol, colinc, minpad;
	unicode padchar;

	Check(4 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &mincol, 0));
	Return(fmtint_default_(print, str, 1, &colinc, 1));
	Return(fmtint_default_(print, str, 2, &minpad, 0));
	Return(fmtchar_default_(print, str, 3, &padchar, ' '));
	if (mincol < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}
	if (colinc < 1) {
		return fmtargs_abort_(print, str, 1,
				"The parameter must be greater than 1.", NULL);
	}
	if (minpad < 0) {
		return fmtargs_abort_(print, str, 2,
				"The parameter must be a positive integer.", NULL);
	}

	/* output */
	Return(fmtprint_pop_(print, str, &pos));
	return format_call_print_(print, pos, str->colon, str->atsign,
			mincol, colinc, minpad, padchar, 1);
}


/*
 *  Binary, Octal, Decimal, Hexadecimal
 */
static int format_radix_parameter_(fmtprint print, struct format_operator *str,
		unsigned radix, fixnum mincol, unicode padchar, fixnum range, unicode comma)
{
	int minusp;
	addr pos, stream;
	LocalRoot local;
	size_t size;

	local = print->local;
	size = (size_t)range;
	Check(size < 0, "cast error");

	Return(fmtprint_pop_(print, str, &pos));
	if (! integerp(pos))
		return format_call_print_(print, pos, 0, 1, mincol, 1, 0, padchar, 0);

	/* integer */
	Return(fmtprint_stream_(print, &stream));
	/* sign */
	Return(minusp_integer_(pos, &minusp));
	if (str->atsign || minusp) {
		Return(write_char_stream_(stream, minusp? '-': '+'));
	}
	/* body */
	if (str->colon) {
		Return(output_nosign_comma_integer_(local, stream, pos, radix, 1, size, comma));
	}
	else {
		Return(output_nosign_integer_(local, stream, pos, radix, 1));
	}
	/* output */
	Return(string_stream_local_(print->local, stream, &pos));
	clear_output_string_stream(stream);

	return format_write_margin_(print, pos, 1, mincol, 1, 0, padchar);
}

static int format_radix_integer_call_(fmtprint print,
		struct format_operator *str, unsigned radix)
{
	Execute ptr;
	fixnum mincol, range;
	unicode padchar, comma;

	ptr = print->ptr;
	/*
	 *  (let ((*print-escape* nil)
	 *        (*print-radix* nil)
	 *        (*print-base* [radix]))
	 *    ...)
	 */
	push_escape_print(ptr, 0);
	push_radix_print(ptr, 0);
	push_base_print(ptr, radix);
	Return(fmtint_default_(print, str, 0, &mincol, 0));
	Return(fmtchar_default_(print, str, 1, &padchar, ' '));
	Return(fmtchar_default_(print, str, 2, &comma, ','));
	Return(fmtint_default_(print, str, 3, &range, 3));
	if (mincol < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}
	if (range < 1) {
		return fmtargs_abort_(print, str, 3,
				"The parameter must be greate than 1.", NULL);
	}

	return format_radix_parameter_(print, str, radix, mincol, padchar, range, comma);
}

static int format_radix_integer_(fmtprint print,
		struct format_operator *str, unsigned radix)
{
	Execute ptr;
	addr control;

	Check(4 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_radix_integer_call_(print, str, radix);
	return pop_control_(ptr, control);
}

static int format_call_Binary_(fmtprint print, struct format_operator *str)
{
	return format_radix_integer_(print, str, 2);
}
static int format_call_Octal_(fmtprint print, struct format_operator *str)
{
	return format_radix_integer_(print, str, 8);
}
static int format_call_Decimal_(fmtprint print, struct format_operator *str)
{
	return format_radix_integer_(print, str, 10);
}
static int format_call_Hexadecimal_(fmtprint print, struct format_operator *str)
{
	return format_radix_integer_(print, str, 16);
}


/*
 *  Radix
 */
static int format_call_Radix_call_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	fixnum mincol, range;
	unicode padchar, comma;
	unsigned radix;

	ptr = print->ptr;
	/*
	 *  (let ((*print-escape* nil)
	 *        (*print-radix* nil)
	 *        (*print-base* [radix]))
	 *    ...)
	 */
	push_escape_print(ptr, 0);
	push_radix_print(ptr, 0);

	Return(fmtint_default_(print, str, 0, &range, 10));
	if (! isBaseChar(range)) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be an integer between 2 and 36.", NULL);
	}
	radix = (unsigned)range;
	push_base_print(ptr, radix);

	Return(fmtint_default_(print, str, 1, &mincol, 0));
	Return(fmtchar_default_(print, str, 2, &padchar, ' '));
	Return(fmtchar_default_(print, str, 3, &comma, ','));
	Return(fmtint_default_(print, str, 4, &range, 3));
	if (mincol < 0) {
		return fmtargs_abort_(print, str, 1,
				"The paramter must be a positive integer.", NULL);
	}
	if (range < 2) {
		return fmtargs_abort_(print, str, 4,
				"The parameter must be greater than 1.", NULL);
	}

	return format_radix_parameter_(print, str, radix, mincol, padchar, range, comma);
}

static int format_call_Radix_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr control;

	Check(5 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_Radix_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  RadixText
 */
static int format_call_RadixText_roma_(fmtprint print,
		struct format_operator *str, addr pos)
{
	addr stream;
	fixnum value;

	if (! fixnump(pos)) {
		return fmtprop_abort_(print, str,
				"~~@R argument ~S must be an integer between 1 and 3999.", pos, NULL);
	}
	GetFixnum(pos, &value);
	if (! (1 <= value && value <= 3999)) {
		return fmtprop_abort_(print, str,
				"~~@R argument ~S must be an integer between 1 and 3999.", pos, NULL);
	}

	Return(fmtprint_stream_(print, &stream));
	Return(roma_integer_(stream, value, str->colon));
	return fmtprint_stream_output_(print);
}

static int format_call_RadixText_english_(fmtprint print,
		struct format_operator *str, addr pos)
{
	addr stream;

	if (! integerp(pos)) {
		return fmtprop_abort_(print, str,
				"~~R argument ~S must be an integer.", pos, NULL);
	}

	Return(fmtprint_stream_(print, &stream));
	Return(english_integer_(print->local, stream, pos, str->colon == 0));
	return fmtprint_stream_output_(print);
}

static int format_call_RadixText_call_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr pos;

	ptr = print->ptr;
	/*
	 *  (let ((*print-escape* nil)
	 *        (*print-radix* nil)
	 *        (*print-base* 10))
	 *    ...)
	 */
	push_escape_print(ptr, 0);
	push_radix_print(ptr, 0);
	push_base_print(ptr, 10);
	/* output */
	Return(fmtprint_pop_(print, str, &pos));
	if (str->atsign)
		return format_call_RadixText_roma_(print, str, pos);
	else
		return format_call_RadixText_english_(print, str, pos);
}

static int format_call_RadixText_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr control;

	Check(0 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_RadixText_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  Plural
 */
static int format_call_Plural_(fmtprint print, struct format_operator *str)
{
	addr pos;

	Check(0 < str->args_size, "size error");
	/* ~:* */
	if (str->colon) {
		Return(fmtprint_rollback_(print, str, 1));
	}

	/* plural */
	Return(fmtprint_pop_(print, str, &pos));
	if (! str->atsign) {
		if (! eql_function(pos, fixnumh(1))) {
			Return(fmtprint_putc_(print, 's'));
		}
	}
	else {
		if (eql_function(pos, fixnumh(1))) {
			Return(fmtprint_putc_(print, 'y'));
		}
		else {
			Return(fmtprint_putc_(print, 'i'));
			Return(fmtprint_putc_(print, 'e'));
			Return(fmtprint_putc_(print, 's'));
		}
	}

	return 0;
}


/*
 *  Character
 */
static int format_call_Character_atsign_(fmtprint print, addr pos)
{
	addr stream;

	CheckType(pos, LISPTYPE_CHARACTER);
	Return(fmtprint_stream_(print, &stream));
	Return(prin1_print_(print->ptr, stream, pos));
	Return(string_stream_local_(print->local, stream, &pos));
	clear_output_string_stream(stream);
	return fmtprint_string_(print, pos);
}

static int format_call_Character_(fmtprint print, struct format_operator *str)
{
	addr pos, name;

	Check(0 < str->args_size, "size error");
	Return(fmtprint_pop_(print, str, &pos));
	if (! characterp(pos)) {
		return fmtprop_abort_(print, str,
				"The argument ~S must be a character.", pos, NULL);
	}
	if (! str->colon && str->atsign) {
		/* ~@C */
		return format_call_Character_atsign_(print, pos);
	}
	if (str->colon || str->atsign) {
		/* ~:C or ~:@C */
		Return(findtable_char_name_(&name, pos));
		if (name != Nil) {
			Return(fmtprint_string_(print, name));
		}
		else {
			Return(fmtprint_putc_(print, RefCharacter(pos)));
		}
	}
	else {
		/* ~C */
		Return(fmtprint_putc_(print, RefCharacter(pos)));
	}

	return 0;
}


/*
 *  Fixed
 */
static int fmtfloat_w(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index)
{
	int check;
	fixnum value;

	Return(fmtint_nilp_(print, str, index, &value, &check));
	if (! check && value < 0) {
		return fmtargs_abort_(print, str, index,
				"The parameter must be a positive integer.", NULL);
	}
	ff->wp = ! check;
	ff->w = check? 0: (size_t)value;

	return 0;
}

static int fmtfloat_d(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index)
{
	int check;
	fixnum value;

	Return(fmtint_nilp_(print, str, index, &value, &check));
	if (! check && value < 0) {
		return fmtargs_abort_(print, str, index,
				"The parameter must be a positive integer.", NULL);
	}
	ff->dp = ! check;
	ff->d = check? 0: (size_t)value;

	return 0;
}

static int fmtfloat_e(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index)
{
	fixnum value;

	Return(fmtint_default_(print, str, index, &value, 1));
	if (value < 0) {
		return fmtargs_abort_(print, str, index,
				"The parameter must be a positive integer.", NULL);
	}
	if (value == 0)
		value = 1;
	ff->ep = 1;
	ff->e = (size_t)value;

	return 0;
}

static int fmtfloat_k(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index, fixnum value)
{
	return fmtint_default_(print, str, index, &(ff->k), value);
}

static int fmtfloat_overflowchar_(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index)
{
	int check;
	unicode u;

	Return(fmtchar_nilp_(print, str, index, &u, &check));
	ff->overflowp = ! check;
	ff->overflow = check? 0: u;

	return 0;
}

static int fmtfloat_padchar_(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index)
{
	return fmtchar_default_(print, str, index, &(ff->pad), ' ');
}

static int format_fixed_single_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	single_float value;
	struct fmtdecimal_struct dec;

	GetSingleFloat(pos, &value);
	ff->u.value_single = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE))
		return fmte_("Invalid single-float.", NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_fixed_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_fixed_double_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	double_float value;
	struct fmtdecimal_struct dec;

	GetDoubleFloat(pos, &value);
	ff->u.value_double = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE))
		return fmte_("Invalid double-float.", NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_fixed_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_fixed_long_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	long_float value;
	struct fmtdecimal_struct dec;

	GetLongFloat(pos, &value);
	ff->u.value_long = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_long_float(&dec, value, FMTFLOAT_ROUND_LONG))
		return fmte_("Invalid long-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_fixed_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_fixed_argument_(fmtprint print,
		struct format_operator *str, fmtfloat ff)
{
	clearpoint(ff);
	Return(fmtfloat_w(print, str, ff, 0));
	Return(fmtfloat_d(print, str, ff, 1));
	Return(fmtfloat_k(print, str, ff, 2, 0));
	Return(fmtfloat_overflowchar_(print, str, ff, 3));
	Return(fmtfloat_padchar_(print, str, ff, 4));
	ff->k_bias = 0; /* 0 if fixed */
	ff->sign = str->atsign;

	return 0;
}

static int format_fixed_fixnum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_fixnum_local(print->local, &pos, pos);
	return format_fixed_single_(print, ff, pos);
}

static int format_fixed_bignum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_bignum_local_(print->local, &pos, pos));
	return format_fixed_single_(print, ff, pos);
}

static int format_fixed_ratio_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_ratio_local_(print->local, &pos, pos));
	return format_fixed_single_(print, ff, pos);
}

static int format_fixed_float_(fmtprint print,
		struct format_operator *str, fmtfloat ff)
{
	addr pos;

	Return(fmtprint_pop_(print, str, &pos));
	switch (GetType(pos)) {
		case LISPTYPE_DOUBLE_FLOAT:
			return format_fixed_double_(print, ff, pos);

		case LISPTYPE_SINGLE_FLOAT:
			return format_fixed_single_(print, ff, pos);

		case LISPTYPE_LONG_FLOAT:
			return format_fixed_long_(print, ff, pos);

		case LISPTYPE_FIXNUM:
			return format_fixed_fixnum_(print, ff, pos);

		case LISPTYPE_BIGNUM:
			return format_fixed_bignum_(print, ff, pos);

		case LISPTYPE_RATIO:
			return format_fixed_ratio_(print, ff, pos);

		default:
			return fmtprop_abort_(print, str,
					"~~F argument ~S must be a real type.", pos, NULL);
	}
}

static int format_call_Fixed_call_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	struct fmtfloat_struct ff;

	ptr = print->ptr;
	/* (let ((*print-escape* nil)) ...) */
	push_escape_print(ptr, 0);
	Return(format_fixed_argument_(print, str, &ff));
	Return(format_fixed_float_(print, str, &ff));

	return 0;
}

static int format_call_Fixed_(fmtprint print, struct format_operator *str)
{
	addr control;
	Execute ptr;

	Check(5 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_Fixed_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  Exponential
 */
static int format_exponent_single_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	single_float value;
	struct fmtdecimal_struct dec;

	GetSingleFloat(pos, &value);
	ff->u.value_single = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE))
		return fmte_("Invalid single-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_exponent_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_exponent_double_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	double_float value;
	struct fmtdecimal_struct dec;

	GetDoubleFloat(pos, &value);
	ff->u.value_double = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE))
		return fmte_("Invalid double-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_exponent_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_exponent_long_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	long_float value;
	struct fmtdecimal_struct dec;

	GetLongFloat(pos, &value);
	ff->u.value_long = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_long_float(&dec, value, FMTFLOAT_ROUND_LONG))
		return fmte_("Invalid long-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_exponent_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_exponent_argument_(fmtprint print,
		struct format_operator *str, fmtfloat ff)
{
	memset(ff, 0, sizeoft(struct fmtfloat_struct));
	Return(fmtfloat_w(print, str, ff, 0));
	Return(fmtfloat_d(print, str, ff, 1));
	Return(fmtfloat_e(print, str, ff, 2));
	Return(fmtfloat_k(print, str, ff, 3, 1));
	Return(fmtfloat_overflowchar_(print, str, ff, 4));
	Return(fmtfloat_padchar_(print, str, ff, 5));
	ff->k_bias = 1; /* 1 if exponent */
	ff->markerp = 1;
	ff->sign_exponent = 1;
	ff->sign = str->atsign;

	return 0;
}

static int fmtfloat_default_marker_(Execute ptr, unicode *ret)
{
	addr pos, check;

	/* default */
	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	/* single-flaot */
	GetConst(COMMON_SINGLE_FLOAT, &check);
	if (check == pos)
		return Result(ret, 'F');
	/* double-float */
	GetConst(COMMON_DOUBLE_FLOAT, &check);
	if (check == pos)
		return Result(ret, 'D');
	/* long-float */
	GetConst(COMMON_LONG_FLOAT, &check);
	if (check == pos)
		return Result(ret, 'L');
	/* short-float */
	GetConst(COMMON_SHORT_FLOAT, &check);
	if (check == pos)
		return Result(ret, 'S');
	/* error */
	*ret = 0;
	return fmte_("Invalid *read-default-float-format* value ~S.", pos, NULL);
}

static unicode fmtfloat_type_marker(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT: return 'F';
		case LISPTYPE_DOUBLE_FLOAT: return 'D';
		case LISPTYPE_LONG_FLOAT: return 'L';
		default: return 'E';
	}
}

static int fmtfloat_marker_(fmtprint print,
		struct format_operator *str, fmtfloat ff, addr pos)
{
	int check;
	unicode u, marker1, marker2;

	Return(fmtchar_nilp_(print, str, 6, &u, &check));
	if (! check) {
		ff->marker = u;
	}
	else {
		Return(fmtfloat_default_marker_(print->ptr, &marker1));
		marker2 = fmtfloat_type_marker(pos);
		ff->marker = (marker1 == marker2)? 'E': marker2;
	}

	return 0;
}

static int format_exponent_fixnum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_fixnum_local(print->local, &pos, pos);
	return format_exponent_single_(print, ff, pos);
}

static int format_exponent_bignum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_bignum_local_(print->local, &pos, pos));
	return format_exponent_single_(print, ff, pos);
}

static int format_exponent_ratio_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_ratio_local_(print->local, &pos, pos));
	return format_exponent_single_(print, ff, pos);
}

static int format_exponent_float_(fmtprint print,
		struct format_operator *str, fmtfloat ff, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_DOUBLE_FLOAT:
			return format_exponent_double_(print, ff, pos);

		case LISPTYPE_SINGLE_FLOAT:
			return format_exponent_single_(print, ff, pos);

		case LISPTYPE_LONG_FLOAT:
			return format_exponent_long_(print, ff, pos);

		case LISPTYPE_FIXNUM:
			return format_exponent_fixnum_(print, ff, pos);

		case LISPTYPE_BIGNUM:
			return format_exponent_bignum_(print, ff, pos);

		case LISPTYPE_RATIO:
			return format_exponent_ratio_(print, ff, pos);

		default:
			return fmtprop_abort_(print, str,
					"~~E argument ~S must be a real type.", pos, NULL);
	}
}

static int format_call_Exponential_call_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr pos;
	struct fmtfloat_struct ff;

	ptr = print->ptr;
	/* (let ((*print-escape* nil)) ...) */
	push_escape_print(ptr, 0);
	Return(format_exponent_argument_(print, str, &ff));
	Return(fmtprint_pop_(print, str, &pos));
	Return(fmtfloat_marker_(print, str, &ff, pos));
	Return(format_exponent_float_(print, str, &ff, pos));

	return 0;
}

static int format_call_Exponential_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr control;

	Check(7 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_Exponential_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  General
 */
static int fmtfloat_e_general_(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index)
{
	int check;
	fixnum value;

	Return(fmtint_nilp_(print, str, index, &value, &check));
	if (check) {
		ff->ep = 0;
		ff->e = 1;
	}
	else {
		if (value < 0) {
			return fmtargs_abort_(print, str, index,
					"The parameter must be a positive integer.", NULL);
		}
		ff->ep = 1;
		ff->e = (size_t)(value? value: 1);
	}

	return 0;
}

static int format_general_argument_(fmtprint print,
		struct format_operator *str, fmtfloat ff)
{
	memset(ff, 0, sizeoft(struct fmtfloat_struct));
	Return(fmtfloat_w(print, str, ff, 0));
	Return(fmtfloat_d(print, str, ff, 1));
	Return(fmtfloat_e_general_(print, str, ff, 2));
	Return(fmtfloat_k(print, str, ff, 3, 1));
	Return(fmtfloat_overflowchar_(print, str, ff, 4));
	Return(fmtfloat_padchar_(print, str, ff, 5));
	ff->sign = str->atsign;

	return 0;
}

static int format_general_single_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	single_float value;
	struct fmtdecimal_struct dec;

	GetSingleFloat(pos, &value);
	ff->u.value_single = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE))
		return fmte_("Invalid single-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_general_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_general_double_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	double_float value;
	struct fmtdecimal_struct dec;

	GetDoubleFloat(pos, &value);
	ff->u.value_double = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE))
		return fmte_("Invalid double-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_general_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_general_long_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	long_float value;
	struct fmtdecimal_struct dec;

	GetLongFloat(pos, &value);
	ff->u.value_long = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_long_float(&dec, value, FMTFLOAT_ROUND_LONG))
		return fmte_("Invalid long-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_general_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_general_fixnum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_fixnum_local(print->local, &pos, pos);
	return format_general_single_(print, ff, pos);
}

static int format_general_bignum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_bignum_local_(print->local, &pos, pos));
	return format_general_single_(print, ff, pos);
}

static int format_general_ratio_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_ratio_local_(print->local, &pos, pos));
	return format_general_single_(print, ff, pos);
}

static int format_general_float_(fmtprint print,
		struct format_operator *str, fmtfloat ff, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_DOUBLE_FLOAT:
			return format_general_double_(print, ff, pos);

		case LISPTYPE_SINGLE_FLOAT:
			return format_general_single_(print, ff, pos);

		case LISPTYPE_LONG_FLOAT:
			return format_general_long_(print, ff, pos);

		case LISPTYPE_FIXNUM:
			return format_general_fixnum_(print, ff, pos);

		case LISPTYPE_BIGNUM:
			return format_general_bignum_(print, ff, pos);

		case LISPTYPE_RATIO:
			return format_general_ratio_(print, ff, pos);

		default:
			return fmtprop_abort_(print, str,
					"~~G argument ~S must be a real type.", pos, NULL);
	}
}

static int format_call_General_call_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr pos;
	struct fmtfloat_struct ff;

	ptr = print->ptr;
	/* (let ((*print-escape* nil)) ...) */
	push_escape_print(ptr, 0);
	Return(format_general_argument_(print, str, &ff));
	Return(fmtprint_pop_(print, str, &pos));
	Return(fmtfloat_marker_(print, str, &ff, pos));
	Return(format_general_float_(print, str, &ff, pos));

	return 0;
}

static int format_call_General_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr control;

	Check(7 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_General_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  Monetary
 */
static int format_monetary_single_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	single_float value;
	struct fmtdecimal_struct dec;

	GetSingleFloat(pos, &value);
	ff->u.value_single = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE))
		return fmte_("Invalid single-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_monetary_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_monetary_double_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	double_float value;
	struct fmtdecimal_struct dec;

	GetDoubleFloat(pos, &value);
	ff->u.value_double = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE))
		return fmte_("Invalid double-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_monetary_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_monetary_long_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	long_float value;
	struct fmtdecimal_struct dec;

	GetLongFloat(pos, &value);
	ff->u.value_long = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_long_float(&dec, value, FMTFLOAT_ROUND_LONG))
		return fmte_("Invalid long-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_monetary_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_monetary_fixnum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_fixnum_local(print->local, &pos, pos);
	return format_monetary_single_(print, ff, pos);
}

static int format_monetary_bignum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_bignum_local_(print->local, &pos, pos));
	return format_monetary_single_(print, ff, pos);
}

static int format_monetary_ratio_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_ratio_local_(print->local, &pos, pos));
	return format_monetary_single_(print, ff, pos);
}

static int format_monetary_default_(fmtprint print, fmtfloat ff, addr pos)
{
	return format_call_print_(print, pos, 0, 1, ff->w, 1, 0, ' ', 0);
}

static int format_monetary_float_(fmtprint print,
		struct format_operator *str, fmtfloat ff)
{
	addr pos;

	Return(fmtprint_pop_(print, str, &pos));
	switch (GetType(pos)) {
		case LISPTYPE_DOUBLE_FLOAT:
			return format_monetary_double_(print, ff, pos);

		case LISPTYPE_SINGLE_FLOAT:
			return format_monetary_single_(print, ff, pos);

		case LISPTYPE_LONG_FLOAT:
			return format_monetary_long_(print, ff, pos);

		case LISPTYPE_FIXNUM:
			return format_monetary_fixnum_(print, ff, pos);

		case LISPTYPE_BIGNUM:
			return format_monetary_bignum_(print, ff, pos);

		case LISPTYPE_RATIO:
			return format_monetary_ratio_(print, ff, pos);

		default:
			return format_monetary_default_(print, ff, pos);
	}
}

static int fmtfloat_n_(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index)
{
	int check;
	fixnum value;

	Return(fmtint_nilp_(print, str, index, &value, &check));
	if (! check && value < 0) {
		return fmtargs_abort_(print, str, index,
				"The parameter must be a positive integer.", NULL);
	}
	ff->np = ! check;
	ff->n = check? 0: (size_t)value;

	return 0;
}

static int format_monetary_argument_(fmtprint print,
		struct format_operator *str, fmtfloat ff)
{
	memset(ff, 0, sizeoft(struct fmtfloat_struct));
	Return(fmtfloat_d(print, str, ff, 0));
	Return(fmtfloat_n_(print, str, ff, 1));
	Return(fmtfloat_w(print, str, ff, 2));
	Return(fmtfloat_padchar_(print, str, ff, 3));
	ff->sign = str->atsign;
	ff->markerp = str->colon;

	return 0;
}

static int format_call_Monetary_call_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	struct fmtfloat_struct ff;

	ptr = print->ptr;
	/* (let ((*print-escape* nil)) ...) */
	push_escape_print(ptr, 0);
	Return(format_monetary_argument_(print, str, &ff));
	Return(format_monetary_float_(print, str, &ff));
	return 0;
}

static int format_call_Monetary_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr control;

	Check(4 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_Monetary_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  Newline
 */
static int format_call_Newline_(fmtprint print, struct format_operator *str)
{
	addr stream;
	fixnum i, size;

	Check(1 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &size, 1));
	if (size < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}

	Return(fmtprint_stream_(print, &stream));
	for (i = 0; i < size; i++) {
		Return(terpri_stream_(stream));
	}

	return fmtprint_stream_output_(print);
}


/*
 *  FreshLine
 */
static int format_call_FreshLine_(fmtprint print, struct format_operator *str)
{
	addr stream;
	fixnum i, size;

	Check(1 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &size, 1));
	if (size < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}
	if (size) {
		Return(fmtprint_stream_(print, &stream));
		Return(fresh_line_stream_(stream, NULL));
		for (i = 1; i < size; i++) {
			Return(terpri_stream_(stream));
		}
		Return(fmtprint_stream_output_(print));
	}

	return 0;
}


/*
 *  Page
 */
static int format_call_Page_(fmtprint print, struct format_operator *str)
{
	addr stream;
	fixnum i, size;

	Check(1 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &size, 1));
	if (size < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}

	Return(fmtprint_stream_(print, &stream));
	for (i = 0; i < size; i++) {
		Return(pageout_stream_(stream));
	}

	return fmtprint_stream_output_(print);
}


/*
 *  Tilde
 */
static int format_call_Tilde_(fmtprint print, struct format_operator *str)
{
	fixnum size;

	Check(1 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &size, 1));
	if (size < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}

	return fmtprint_putc_times_(print, '~', size);
}


/*
 *  IgnoredNewline
 */
static int format_call_IgnoredNewline_(fmtprint print, struct format_operator *str)
{
	/*
	 * ~\n   -> delete space
	 * ~:\n  -> do-nothing
	 * ~@\n  -> output newline, delete space
	 * ~:@\n -> output newline
	 */
	addr stream;

	Check(0 < str->args_size, "size error");
	if (str->atsign) {
		/* output newline */
		Return(fmtprint_stream_(print, &stream));
		Return(terpri_stream_(stream));
		Return(fmtprint_stream_output_(print));
	}
	if (! str->colon) {
		/* delete space */
		print->delete_space = 1;
	}

	return 0;
}


/*
 *  Tabulate
 */
static int format_call_Tabulate_(fmtprint print, struct format_operator *str)
{
	addr stream;
	fixnum column, colinc, now;
	size_t size;

	Check(2 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &column, 1));
	Return(fmtint_default_(print, str, 1, &colinc, 1));
	if (column < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be greater than equal to 0.", NULL);
	}
	if (colinc < 0) {
		return fmtargs_abort_(print, str, 1,
				"The parameter must be greater than equal to 0.", NULL);
	}

	if (str->colon) {
		Return(fmtprint_stream_(print, &stream));
		if (str->atsign) {
			Return(pprint_tab_section_relative_(print->ptr, stream, column, colinc));
		}
		else {
			Return(pprint_tab_section_(print->ptr, stream, column, colinc));
		}
		Return(fmtprint_stream_output_(print));
	}
	else {
		Return(getleft_stream_(print->stream, &size));
		now = (fixnum)size;
		Check(now < 0, "cast error");
		if (str->atsign) {
			Return(pprint_tab_relative_force_(print->stream, column, colinc, now));
		}
		else {
			Return(pprint_tab_absolute_force_(print->stream, column, colinc, now));
		}
	}

	return 0;
}


/*
 *  GoTo
 */
static int format_call_GoTo_(fmtprint print, struct format_operator *str)
{
	fixnum count;

	Check(1 < str->args_size, "size error");
	if (! str->atsign) {
		Return(fmtint_default_(print, str, 0, &count, 1));
		if (count < 0) {
			return fmtargs_abort_(print, str, 0,
					"The parameter must be greater than equal to 0.", NULL);
		}
		if (! str->colon) {
			Return(fmtprint_forward_(print, str, (size_t)count));
		}
		else {
			Return(fmtprint_rollback_(print, str, (size_t)count));
		}
	}
	else {
		Return(fmtint_default_(print, str, 0, &count, 0));
		if (count < 0) {
			return fmtargs_abort_(print, str, 0,
					"The parameter must be greater than equal to 0.", NULL);
		}
		if (! str->colon) {
			Return(fmtprint_absolute_(print, str, (size_t)count));
		}
		else {
			return fmtprop_abort_(print, str,
					"The parameter don't accept both : and @ parameter (~~:@*).", NULL);
		}
	}

	return 0;
}


/*
 *  Recursive
 */
static void fmtprint_format_forward(fmtprint print)
{
	struct format_operator *str;

	str = fmtprint_operator(print);
	Check(str->type != FormatType_Format, "format error");
	print->now += str->size;
}

static int format_call_Recursive_call_(fmtprint print,
		addr format, addr args, addr *ret)
{
	Execute ptr;
	addr stream, pos;

	ptr = print->ptr;
	Return(fmtprint_stream_(print, &stream));
	Return(format_execute_(ptr, stream, format, args, ret));
	Return(string_stream_local_(ptr->local, stream, &pos));
	clear_output_string_stream(stream);
	return fmtprint_string_(print, pos);
}

static int format_call_Recursive_function_(fmtprint print,
		struct format_operator *str, addr format)
{
	addr args;
	size_t size1, size2;

	args = print->rest->front;
	Return(length_list_safe_(args, &size1));
	Return(format_call_Recursive_call_(print, format, args, &args));
	/* result */
	Return(length_list_safe_(args, &size2));
	if (size1 < size2) {
		gchold_push_local(print->local, args);
		return fmtprint_abort_(print, str->colon_pos,
				"The FORMATTER ~S return illegal arguments ~S.", format, args, NULL);
	}
	for (; size2 < size1; size2++) {
		Return(fmtprint_pop_(print, str, &args));
	}

	return 0;
}

static int fmtcall_(fmtprint print, int *loop);
static int format_call_Recursive_format_(fmtprint print, addr format)
{
	addr backup_format;
	size_t backup_now;

	/* backup */
	backup_format = print->format;
	backup_now = print->now;
	/* execute */
	print->format = format;
	print->now = 0;
	fmtprint_format_forward(print);
	Return(fmtcall_(print, NULL));
	print->loop = 1;
	/* rollback */
	print->format = backup_format;
	print->now = backup_now;

	return 0;
}

static int format_call_Recursive_atsign_(fmtprint print,
		struct format_operator *str, addr format)
{
	if (functionp(format))
		return format_call_Recursive_function_(print, str, format);
	if (formatp(format))
		return format_call_Recursive_format_(print, format);
	if (stringp(format)) {
		Return(format_parse_local_(print->local, &format, format));
		return format_call_Recursive_format_(print, format);
	}
	return fmtprop_abort_(print, str, "Invalid control-string ~S.", format, NULL);
}

static int format_call_Recursive_(fmtprint print, struct format_operator *str)
{
	addr format, args;

	Check(0 < str->args_size, "size error");
	Check(str->colon, "Invalid argument [colon].");
	if (str->atsign) {
		Return(fmtprint_pop_(print, str, &format));
		return format_call_Recursive_atsign_(print, str, format);
	}
	else {
		Return(fmtprint_pop_(print, str, &format));
		Return(fmtprint_pop_(print, str, &args));
		return format_call_Recursive_call_(print, format, args, &args);
	}
}


/*
 *  ConditionalNewline
 */
static int format_call_ConditionalNewline_(fmtprint print, struct format_operator *str)
{
	addr stream;
	Execute ptr;

	Check(0 < str->args_size, "size error");
	ptr = print->ptr;
	stream = print->stream;
	if (str->colon && str->atsign)
		return pprint_newline_print_(ptr, pprint_newline_mandatory, stream);
	else if (str->colon)
		return pprint_newline_print_(ptr, pprint_newline_fill, stream);
	else if (str->atsign)
		return pprint_newline_print_(ptr, pprint_newline_miser, stream);
	else
		return pprint_newline_print_(ptr, pprint_newline_linear, stream);
}


/*
 *  Write
 */
static int format_call_Write_call_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr pos, stream;

	ptr = print->ptr;
	if (str->colon) {
		push_pretty_print(ptr, 1);
	}
	if (str->atsign) {
		push_level_nil_print(ptr);
		push_length_nil_print(ptr);
	}
	Return(fmtprint_pop_(print, str, &pos));
	Return(fmtprint_stream_(print, &stream));
	Return(write_print_(ptr, stream, pos));
	Return(fmtprint_stream_output_(print));

	return 0;
}

static int format_call_Write_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr control;

	Check(0 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_Write_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  Indent
 */
static int format_call_Indent_(fmtprint print, struct format_operator *str)
{
	fixnum n;

	Check(1 < str->args_size, "size error");
	Check(str->atsign, "Invalid argument [atsign].");
	Return(fmtint_default_(print, str, 0, &n, 0));

	return pprint_indent_print_(print->ptr, ! str->colon, n, print->stream);
}


/*
 *  Case
 */
static int format_call_Case_(fmtprint print, struct format_operator *str)
{
	enum fmtcase case1, case2;
	size_t now;

	Check(0 < str->args_size, "size error");
	/*
	 *  ~(		downcase
	 *  ~:(		all capitalize
	 *  ~@(		first capitalize
	 *  ~:@(	upcase
	 */
	case1 = print->conversion;
	now = print->now;

	if (str->colon && str->atsign)
		case2 = fmtcase_upcase;
	else if (str->colon) {
		print->word = 0;
		case2 = fmtcase_capitalize_all;
	}
	else if (str->atsign) {
		print->word = 0;
		print->first = 1;
		case2 = fmtcase_capitalize_first;
	}
	else
		case2 = fmtcase_downcase;

	/* update */
	print->conversion = case2;
	print->now += format_bytesize(0);
	/* call */
	Return(fmtcall_(print, NULL));
	/* rollback */
	print->conversion = case1;
	print->now = now;

	return 0;
}


/*
 *  Condition
 */
static size_t *format_condition_array(struct format_operator *str)
{
	return (size_t *)format_getargs(str, (str->colon || str->atsign)? 0: 1);
}

static int format_condition_execute_(
		fmtprint print, struct format_operator *str, size_t clause)
{
	size_t now;

	/* update */
	now = print->now;
	print->now += clause;
	/* call */
	Return(fmtcall_(print, NULL));
	print->loop = 1;
	/* rollback */
	print->now = now;

	return 0;
}

static int format_condition_index_(fmtprint print,
		struct format_operator *str, fixnum index, int ignore)
{
	size_t *array, pos, size;

	array = format_condition_array(str);
	size = *(array++);
	if (ignore || index < 0)
		goto clause_else;
	pos = (size_t)index;
	if (size <= pos)
		goto clause_else;
	return format_condition_execute_(print, str, array[pos]);

clause_else:
	if (str->option_check == 0)
		return 0;
	return format_condition_execute_(print, str, array[size - 1]);
}

static int format_condition_select_(fmtprint print, struct format_operator *str)
{
	int check, ignore;
	addr pos;
	fixnum index;

	Check(1 < str->args_size, "size error");
	/* index */
	ignore = 0;
	Return(fmtint_nilp_(print, str, 0, &index, &check));
	if (check) {
		Return(fmtprint_pop_(print, str, &pos));
		if (! integerp(pos)) {
			return fmtprop_abort_(print, str,
					"The argument ~S must be an integer.", pos, NULL);
		}
		if (fixnump(pos)) {
			GetFixnum(pos, &index);
		}
		else {
			index = 0;
			ignore = 1;
		}
	}

	/* print */
	return format_condition_index_(print, str, index, ignore);
}

static int format_condition_boolean_(fmtprint print, struct format_operator *str)
{
	addr pos;

	Check(0 < str->args_size, "size error");
	Return(fmtprint_pop_(print, str, &pos));
	return format_condition_index_(print, str, (pos != Nil), 0);
}

static int format_condition_true_(fmtprint print, struct format_operator *str)
{
	addr pos;

	Check(0 < str->args_size, "size error");
	Return(fmtprint_peek_(print, str, &pos));
	if (pos != Nil)
		return format_condition_index_(print, str, 0, 0);
	Return(fmtprint_pop_(print, str, &pos));
	return 0; /* do nothing */
}

static int format_call_Condition_(fmtprint print, struct format_operator *str)
{
	/*
	 *  ~[		select
	 *  ~:[		boolean
	 *  ~@[		true
	 *  ~:@[	error
	 */
	Check(str->colon && str->atsign, "Invalid arguemnt [colon && atsing].");
	if (str->colon)
		return format_condition_boolean_(print, str);
	else if (str->atsign)
		return format_condition_true_(print, str);
	else
		return format_condition_select_(print, str);
}


/*
 *  Iteration
 */
static int format_call_Iteration_exit(
		int intp, fixnum i, fixnum index, addr list, int *forcep)
{
	if (*forcep) {
		*forcep = 0;
		return 0;
	}
	if ((intp == 0) && (index <= i))
		return 1;
	if (list == Nil)
		return 1;

	return 0;
}

static int format_call_Iteration_list_(fmtprint print,
		struct format_operator *str, int forcep)
{
	int intp, check, escape;
	addr pos, stream, root;
	fixnum index, i;
	struct fmtstack args;
	size_t now;

	/* argument */
	Return(fmtprint_pop_(print, str, &pos));
	Return(fmtint_nilp_(print, str, 0, &index, &intp));

	/* save */
	args.root = pos;
	args.front = pos;
	args.index = 0;
	print->rest = &args;
	now = print->now;

	stream = print->stream;
	root = Nil;
	check = pretty_stream_p(stream);
	if (check) {
		Return(root_pretty_stream_(stream, &root));
		gchold_push_local(print->local, root);
		Return(setroot_pretty_stream_(stream, pos));
	}

	/* loop */
	escape = 0;
	for (i = 0; print->loop; i++) {
		if (format_call_Iteration_exit(intp, i, index, args.front, &forcep))
			break;
		print->now = now;
		if (fmtcall_(print, NULL)) {
			escape = 1;
			goto finish;
		}
	}

	/* rollback */
finish:
	if (check) {
		Return(setroot_pretty_stream_(stream, root));
	}

	return escape;
}

static int format_call_Iteration_rest_(fmtprint print,
		struct format_operator *str, int forcep)
{
	int intp;
	fixnum index, i;
	struct fmtstack *rest;
	size_t now;

	Return(fmtint_nilp_(print, str, 0, &index, &intp));
	now = print->now;
	rest = print->rest;
	for (i = 0; print->loop; i++) {
		if (format_call_Iteration_exit(intp, i, index, rest->front, &forcep))
			break;
		print->now = now;
		Return(fmtcall_(print, NULL));
	}

	return 0;
}

static int format_call_Iteration_listargs_(fmtprint print,
		struct format_operator *str, int forcep)
{
	int intp, check, escape, loop_check;
	fixnum index, i;
	addr stream, root, car, cdr;
	struct fmtstack args;
	size_t now;

	/* argument */
	Return(fmtprint_pop_(print, str, &cdr));
	Return(fmtint_nilp_(print, str, 0, &index, &intp));

	/* save */
	stream = print->stream;
	root = Nil;
	check = pretty_stream_p(stream);
	if (check) {
		Return(root_pretty_stream_(stream, &root));
		gchold_push_local(print->local, root);
	}

	/* loop */
	now = print->now;
	escape = 0;
	for (i = 0; ; i++) {
		if (format_call_Iteration_exit(intp, i, index, cdr, &forcep))
			break;
		Return_getcons(cdr, &car, &cdr);
		args.root = car;
		args.front = car;
		args.index = 0;
		print->now = now;
		print->rest = &args;
		print->last = (cdr == Nil);
		if (check) {
			Return(setroot_pretty_stream_(stream, car));
		}
		if (fmtcall_(print, &loop_check)) {
			escape = 1;
			goto finish;
		}
		if (loop_check)
			break;
		print->loop = 1;
	}

	/* rollback */
finish:
	if (check) {
		Return(setroot_pretty_stream_(stream, root));
	}

	return escape;
}

static int format_call_Iteration_restargs_(fmtprint print,
		struct format_operator *str, int forcep)
{
	int intp, check, escape, loop_check;
	addr stream, pos, root;
	fixnum index, i;
	struct fmtstack *rest, args;
	size_t now;

	Return(fmtint_nilp_(print, str, 0, &index, &intp));
	now = print->now;
	rest = print->rest;

	/* save */
	stream = print->stream;
	check = pretty_stream_p(stream);

	root = Nil;
	for (i = 0; ; i++) {
		if (format_call_Iteration_exit(intp, i, index, rest->front, &forcep))
			break;
		print->rest = rest;
		Return(fmtprint_pop_(print, str, &pos));
		if (check) {
			Return(root_pretty_stream_(stream, &root));
		}

		args.root = pos;
		args.front = pos;
		args.index = 0;
		print->now = now;
		print->rest = &args;
		print->last = (rest->front == Nil);
		if (check) {
			Return(setroot_pretty_stream_(stream, pos));
		}
		escape = fmtcall_(print, &loop_check);
		if (check) {
			Return(setroot_pretty_stream_(stream, root));
		}
		if (escape)
			return 1;
		if (loop_check)
			break;
		print->loop = 1;
	}

	return 0;
}

static int format_call_Iteration_function_(fmtprint print, struct format_operator *str)
{
	if (str->colon && str->atsign)
		return format_call_Iteration_restargs_(print, str, str->close_colon);
	else if (str->colon)
		return format_call_Iteration_listargs_(print, str, str->close_colon);
	else if (str->atsign)
		return format_call_Iteration_rest_(print, str, str->close_colon);
	else
		return format_call_Iteration_list_(print, str, str->close_colon);
}

static int format_call_Iteration_call_(fmtprint print, struct format_operator *str)
{
	int escape;
	struct fmtstack *rest;
	size_t now;

	/* backup */
	now = print->now;
	rest = print->rest;
	/* execute */
	print->now += format_bytesize(1);
	escape = format_call_Iteration_function_(print, str);
	/* rollback */
	print->now = now;
	print->rest = rest;
	print->loop = 1;

	return escape;
}

/* empty */
static int format_call_Iteration2_list_(fmtprint print,
		struct format_operator *str, addr format)
{
	int intp, forcep, escape, check;
	addr pos, stream, root;
	fixnum index, i;
	struct fmtstack args;

	/* argument */
	Return(fmtprint_pop_(print, str, &pos));
	Return(fmtint_nilp_(print, str, 0, &index, &intp));
	forcep = str->close_colon;

	/* save */
	args.root = pos;
	args.front = pos;
	args.index = 0;
	print->rest = &args;

	root = Nil;
	stream = print->stream;
	check = pretty_stream_p(stream);
	if (check) {
		Return(root_pretty_stream_(stream, &root));
		gchold_push_local(print->local, root);
		Return(setroot_pretty_stream_(stream, pos));
	}

	/* loop */
	escape = 0;
	for (i = 0; ; i++) {
		if (format_call_Iteration_exit(intp, i, index, args.front, &forcep))
			break;
		if (format_call_Recursive_function_(print, str, format)) {
			escape = 1;
			goto finish;
		}
	}

	/* rollback */
finish:
	if (check) {
		Return(setroot_pretty_stream_(stream, root));
	}

	return escape;
}

static int format_call_Iteration2_rest_(fmtprint print,
		struct format_operator *str, addr format)
{
	int intp, forcep;
	fixnum index, i;
	struct fmtstack *rest;

	Return(fmtint_nilp_(print, str, 0, &index, &intp));
	forcep = str->close_colon;
	rest = print->rest;
	for (i = 0; ; i++) {
		if (format_call_Iteration_exit(intp, i, index, rest->front, &forcep))
			break;
		Return(format_call_Recursive_function_(print, str, format));
	}

	return 0;
}

static int format_call_Iteration2_listargs_(fmtprint print,
		struct format_operator *str, addr format)
{
	int intp, forcep, escape, check;
	fixnum index, i;
	addr stream, root, car, cdr;
	struct fmtstack args;

	/* argument */
	Return(fmtprint_pop_(print, str, &cdr));
	Return(fmtint_nilp_(print, str, 0, &index, &intp));

	/* save */
	stream = print->stream;
	root = Nil;
	check = pretty_stream_p(stream);
	if (check) {
		Return(root_pretty_stream_(stream, &root));
		gchold_push_local(print->local, root);
	}

	/* loop */
	forcep = str->close_colon;
	escape = 0;
	for (i = 0; ; i++) {
		if (format_call_Iteration_exit(intp, i, index, cdr, &forcep))
			break;
		Return_getcons(cdr, &car, &cdr);
		args.root = car;
		args.front = car;
		args.index = 0;
		print->rest = &args;
		print->last = (cdr == Nil);
		if (check) {
			Return(setroot_pretty_stream_(stream, car));
		}
		if (format_call_Recursive_call_(print, format, car, &car)) {
			escape = 1;
			goto finish;
		}
	}

	/* rollback */
finish:
	if (check) {
		Return(setroot_pretty_stream_(stream, root));
	}

	return escape;
}

static int format_call_Iteration2_restargs_(fmtprint print,
		struct format_operator *str, addr format)
{
	int intp, forcep, check, escape;
	addr stream, pos, root;
	fixnum index, i;
	struct fmtstack *rest, args;

	Return(fmtint_nilp_(print, str, 0, &index, &intp));
	forcep = str->close_colon;
	rest = print->rest;

	/* save */
	stream = print->stream;
	check = pretty_stream_p(stream);

	for (i = 0; ; i++) {
		if (format_call_Iteration_exit(intp, i, index, rest->front, &forcep))
			break;
		print->rest = rest;
		Return(fmtprint_pop_(print, str, &pos));
		root = Nil;
		if (check) {
			Return(root_pretty_stream_(stream, &root));
		}

		args.root = pos;
		args.front = pos;
		args.index = 0;
		print->rest = &args;
		print->last = (rest->front == Nil);
		if (check) {
			Return(setroot_pretty_stream_(stream, pos));
		}
		escape = format_call_Recursive_call_(print, format, pos, &pos);
		if (check) {
			Return(setroot_pretty_stream_(stream, root));
		}
		if (escape)
			return 1;
	}

	return 0;
}

static int format_call_Iteration2_call_(
		fmtprint print, struct format_operator *str, addr format)
{
	if (str->colon && str->atsign)
		return format_call_Iteration2_restargs_(print, str, format);
	else if (str->colon)
		return format_call_Iteration2_listargs_(print, str, format);
	else if (str->atsign)
		return format_call_Iteration2_rest_(print, str, format);
	else
		return format_call_Iteration2_list_(print, str, format);
}

static int format_call_Iteration2_function_(
		fmtprint print, struct format_operator *str, addr format)
{
	struct fmtstack *rest;

	rest = print->rest;
	Return(format_call_Iteration2_call_(print, str, format));
	print->rest = rest;

	return 0;
}

static int format_call_Iteration2_format_(
		fmtprint print, struct format_operator *str, addr format)
{
	int escape;
	addr backup_format;
	struct fmtstack *backup_rest;
	size_t backup_now;

	/* structure */
	backup_format = print->format;
	backup_now = print->now;
	backup_rest = print->rest;
	print->now = 0;
	print->format = format;
	fmtprint_format_forward(print);
	/* call */
	escape = format_call_Iteration_function_(print, str);
	/* rollback */
	print->format = backup_format;
	print->now = backup_now;
	print->rest = backup_rest;
	print->loop = 1;

	return escape;
}

static int format_call_Iteration2_(fmtprint print, struct format_operator *str)
{
	addr format;

	/* first */
	Return(fmtprint_pop_(print, str, &format));
	if (functionp(format))
		return format_call_Iteration2_function_(print, str, format);
	if (formatp(format))
		return format_call_Iteration2_format_(print, str, format);
	if (stringp(format)) {
		Return(format_parse_local_(print->ptr->local, &format, format));
		return format_call_Iteration2_format_(print, str, format);
	}
	return fmtprop_abort_(print, str, "Invalid control-string ~S.", format, NULL);
}

static int format_call_Iteration_(fmtprint print, struct format_operator *str)
{
	/*
	 *  "~{"		"~{ ~S ~S~}" '(a 1 b 2 c 3)
	 *  "~:{"		"~{ ~S ~S~}" '((a 1) (b 2) (c 3))
	 *  "~@{"		"~{ ~S ~S~}" 'a '1 'b '2 'c '3
	 *  "~:@{"		"~{ ~S ~S~}" '(a 1) '(b 2) '(c 3)
	 */
	int escape;
	unsigned print_escape;

	Check(1 < str->args_size, "size error");
	print_escape = print->escape;
	print->escape = 1;
	if (str->option_check)
		escape = format_call_Iteration2_(print, str);
	else
		escape = format_call_Iteration_call_(print, str);
	print->escape = print_escape;

	return escape;
}


/*
 *  Justification
 */
struct format_justification {
	int second;
	Execute ptr;
	LocalRoot local;
	fmtprint print;
	struct format_operator *str;
	size_t *array, size, allsize, div, space, count, width;
	size_t mincol, colinc, minpad;
	unicode padchar;
	addr vector;
};

static size_t *format_justification_array(struct format_operator *str)
{
	return (size_t *)format_getargs(str, 4);
}

static int format_call_Justification_vector1_(struct format_justification *just)
{
	LocalRoot local;
	addr vector, pos;

	local = just->local;
	vector_local(local, &vector, 1);
	strvect_local(local, &pos, 0);
	Return(vector_set_(vector, 0, pos));
	just->vector = vector;
	just->size = 1;
	just->allsize = 0;

	return 0;
}

static int format_call_Justificaion_separator_(
		fmtprint print, struct format_operator *str,
		size_t *rcount, size_t *rwidth)
{
	int check;
	fixnum count, width;
	size_t value;

	/* NULL */
	if (rcount == NULL)
		return 0;

	/* first */
	Return(fmtint_default_(print, str, 0, &count, 0));
	if (count < 0) {
		return fmtargs_abort_(print, str, 0,
				"The paramter must be a non-negative integer.", NULL);
	}
	*rcount = (size_t)count;

	/* second */
	Return(fmtint_nilp_(print, str, 1, &width, &check));
	if (check) {
		Return(termsize_stream_(print->stream, &value, &check));
		if (check)
			value = PRINT_DEFAULT_WIDTH;
		*rwidth = value;
	}
	else {
		if (width < 0) {
			return fmtargs_abort_(print, str, 1,
					"The parameter must be non-negative integer.", NULL);
		}
		*rwidth = (size_t)width;
	}

	return 0;
}

static int format_call_Justificaion_fmtcall_(fmtprint print,
		size_t *count, size_t *width)
{
	enum FormatType type;
	addr stream, backup_string;
	struct format_operator *str;
	fmtcalltype call;

	/* string-stream */
	Return(fmtprint_make_string_(print, &stream, &backup_string));
	/* loop */
	str = fmtprint_operator(print);
	type = str->type;
	while (print->loop) {
		/* End */
		if (type == FormatType_End)
			break;
		/* ~:; */
		if (type == FormatType_ClauseSeparator) {
			Return(format_call_Justificaion_separator_(print, str, count, width));
			break;
		}
		/* delete-space */
		if (type != FormatType_Output)
			print->delete_space = 0;
		/* call */
		call = FormatCallTable[type];
		Check(call == NULL, "Invalid format type.");
		Return((*call)(print, str));
		/* next */
		print->now += str->size;
		str = fmtprint_operator(print);
		type = str->type;
	}
	/* close */
	close_output_string_stream(stream);
	print->string = backup_string;

	return 0;
}

static int format_call_Justificaion_call_(struct format_justification *just,
		size_t now, addr *ret, size_t *count, size_t *width)
{
	addr stream;
	fmtprint print;
	LocalRoot local;
	struct fmtprint_struct loop;

	print = just->print;
	local = print->local;

	fmtprint_copy(&loop, print);
	loop.pretty = 0;
	loop.escape = 0;
	loop.fill = print->fill;
	loop.fill_white = print->fill_white;
	loop.fill_ignore = print->fill_ignore;
	loop.conversion = print->conversion;
	loop.now = now;
	loop.rest = print->rest;

	Return(fmtprint_stream_(print, &stream));
	loop.stream = stream;
	Return(format_call_Justificaion_fmtcall_(&loop, count, width));
	if (loop.loop) {
		print->loop = 1;
		Return(string_stream_local_(local, stream, ret));
	}
	else {
		print->loop = 0;
		strvect_local(local, ret, 0);
	}
	clear_output_string_stream(stream);

	return 0;
}

static int format_call_Justificaion_prefix_(
		struct format_justification *just, size_t now, addr *ret)
{
	size_t count, width;

	Return(format_call_Justificaion_call_(just, now, ret, &count, &width));
	just->count = count;
	just->width = width;

	return 0;
}

static int format_call_Justificaion_clause_(
		struct format_justification *just, size_t now, addr *ret)
{
	return format_call_Justificaion_call_(just, now, ret, NULL, NULL);
}

static int format_call_Justification_vector2_(struct format_justification *just)
{
	LocalRoot local;
	fmtprint print;
	addr vector, pos;
	size_t *array, size, allsize, value, i, now;

	print = just->print;
	local = just->local;
	size = just->size;
	array = just->array;

	vector_local(local, &vector, size);
	allsize = 0;
	now = print->now;
	for (i = 0; i < size; i++) {
		Return(format_call_Justificaion_clause_(just, now + array[i], &pos));
		if (print->loop == 0) {
			just->size = i;
			break;
		}
		Return(vector_set_(vector, i, pos));
		/* string */
		Return(eastasian_length_(pos, &value, NULL));
		allsize += value;
	}
	print->loop = 1;
	just->vector = vector;
	just->allsize = allsize;

	return 0;
}

static int format_call_Justification_vector_(struct format_justification *just)
{
	size_t *array, size;

	array = format_justification_array(just->str);
	size = *(array++);
	if (just->second) {
		Check(size == 0, "size error");
		size--;
		array++;
	}
	just->array = array;
	just->size = size;
	if (size)
		Return(format_call_Justification_vector2_(just));
	if (just->size == 0)
		return format_call_Justification_vector1_(just);

	return 0;
}

static void format_call_Justification_count(struct format_justification *just)
{
	struct format_operator *str;
	size_t width;

	str = just->str;
	Check(just->size == 0, "size zero error");
	if (just->size == 1) {
		just->div = 1;
		if (str->colon && str->atsign)
			just->div++;
	}
	else {
		just->div = just->size - 1;
		if (str->colon)
			just->div++;
		if (str->atsign)
			just->div++;
	}
	width = just->div * just->minpad;
	if (just->mincol < just->allsize + width) {
		width = just->allsize + width - just->mincol;
		just->mincol += format_colinc_division(width, just->colinc);
	}
	Check(just->mincol < just->allsize, "mincol error");
	just->space = just->mincol - just->allsize;
}

static int format_call_Justification_space_(
		struct format_justification *just, size_t index)
{
	size_t space, div;

	space = just->space;
	div = just->div;
	space = ((index + 1) * space / div) - (index * space / div);
	return fmtprint_putc_times_(just->print, just->padchar, space);
}

static int format_call_Justification_output_(struct format_justification *just)
{
	addr pos;
	size_t index, i;
	struct format_operator *str;

	just->print->loop = 1;
	str = just->str;
	index = 0;
	/* prefix */
	if (str->colon || (just->size == 1 && (str->colon || str->atsign == 0))) {
		Return(format_call_Justification_space_(just, index++));
	}
	/* body */
	for (i = 0; i < just->size; i++) {
		if (i) {
			Return(format_call_Justification_space_(just, index++));
		}
		Return(vector_get_(just->vector, i, &pos));
		Return(fmtprint_string_(just->print, pos));
	}
	/* suffix */
	if (str->atsign) {
		Return(format_call_Justification_space_(just, index++));
	}

	return 0;
}

static int format_call_Justification1_(struct format_justification *just)
{
	Return(format_call_Justification_vector_(just));
	format_call_Justification_count(just);
	return format_call_Justification_output_(just);
}

static int format_call_Justification2_(struct format_justification *just)
{
	fmtprint print;
	addr prefix, stream;
	size_t *array, size, now;

	print = just->print;
	array = format_justification_array(just->str);
	size = *(array++);
	Check(size < 1, "size error");
	now = *(array++);
	size--;
	just->array = array;
	just->size = size;
	/* execute first */
	Return(format_call_Justificaion_prefix_(just, now + print->now, &prefix));
	if (print->loop) {
		Return(format_call_Justification_vector_(just));
	}
	else {
		just->size = 0;
		Return(format_call_Justification_vector1_(just));
	}
	format_call_Justification_count(just);
	/* terminal */
	stream = print->stream;
	Return(getleft_stream_(stream, &now));
	if (just->width < now + just->mincol + just->count) {
		Return(fmtprint_string_(print, prefix));
	}
	/* output */
	return format_call_Justification_output_(just);
}

static int format_call_Justification_(fmtprint print, struct format_operator *str)
{
	struct format_justification just;
	fixnum mincol, colinc, minpad;
	unicode padchar;

	Check(4 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &mincol, 0));
	Return(fmtint_default_(print, str, 1, &colinc, 1));
	Return(fmtint_default_(print, str, 2, &minpad, 0));
	Return(fmtchar_default_(print, str, 3, &padchar, ' '));
	if (mincol < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}
	if (colinc < 1) {
		return fmtargs_abort_(print, str, 1,
				"The parameter must be greater than 1.", NULL);
	}
	if (minpad < 0) {
		return fmtargs_abort_(print, str, 2,
				"The parameter must be a positive integer.", NULL);
	}

	cleartype(just);
	just.ptr = print->ptr;
	just.local = print->local;
	just.print = print;
	just.str = str;
	just.mincol = (size_t)mincol;
	just.colinc = (size_t)colinc;
	just.minpad = (size_t)minpad;
	just.padchar = padchar;
	just.second = str->option_check;

	if (str->option_check == 0)
		return format_call_Justification1_(&just);
	else
		return format_call_Justification2_(&just);
}


/*
 *  LogicalBlock
 */
struct format_pretty_struct {
	struct fmtprint_struct print;
	size_t index;
};

enum FormatPrettyIndex {
	FormatPrettyIndex_Stream,
	FormatPrettyIndex_Root,
	FormatPrettyIndex_Front,
	FormatPrettyIndex_Size
};

static void set_format_pretty(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	SetArraySS(pos, index, value);
}
static void get_format_pretty(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	GetArraySS(pos, index, ret);
}
static void *pointer_format_pretty(addr pos)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	return (void *)PtrBodySS(pos);
}

static void getstream_format_pretty(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	get_format_pretty(pos, FormatPrettyIndex_Stream, ret);
}
static void setstream_format_pretty(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	set_format_pretty(pos, FormatPrettyIndex_Stream, value);
}

static void getroot_format_pretty(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	get_format_pretty(pos, FormatPrettyIndex_Root, ret);
}
static void setroot_format_pretty(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	set_format_pretty(pos, FormatPrettyIndex_Root, value);
}

static void getfront_format_pretty(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	get_format_pretty(pos, FormatPrettyIndex_Front, ret);
}
static void setfront_format_pretty(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	set_format_pretty(pos, FormatPrettyIndex_Front, value);
}

static struct format_pretty_struct *struct_format_pretty(addr pos)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	return (struct format_pretty_struct *)pointer_format_pretty(pos);
}
static fmtprint fmtprint_format_pretty(addr pos)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	return (fmtprint)&(struct_format_pretty(pos)->print);
}

static void read_format_pretty(addr pos, fmtprint print, struct fmtstack *rest)
{
	addr root, front;
	struct format_pretty_struct *str;

	/* structure */
	str = struct_format_pretty(pos);
	*print = str->print;
	print->rest = rest;

	/* list */
	getroot_format_pretty(pos, &root);
	getfront_format_pretty(pos, &front);
	rest->root = root;
	rest->front = front;
	rest->index = str->index;
}

static void format_pretty_heap(addr *ret, addr stream, struct fmtstack *rest)
{
	addr pos;
	struct format_pretty_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_FORMAT_PRETTY,
			FormatPrettyIndex_Size,
			sizeoft(struct format_pretty_struct));
	str = struct_format_pretty(pos);
#ifdef LISP_DEBUG
	aamemory(str, sizeoft(struct format_pretty_struct));
#endif
	setstream_format_pretty(pos, stream);
	setroot_format_pretty(pos, rest->root);
	setfront_format_pretty(pos, rest->front);
	str->index = rest->index;
	*ret = pos;
}

static int format_logicalblock2_(Execute ptr)
{
	addr pos, stream;
	struct fmtprint_struct print;
	struct fmtstack rest;

	getdata_control(ptr, &pos);
	getstream_format_pretty(pos, &stream);
	Return(check_pretty_stream_(ptr, stream));
	read_format_pretty(pos, &print, &rest);
	return fmtcall_(&print, NULL);
}

static int format_logicalblock1_call_(Execute ptr, addr pretty, addr stream)
{
	addr gensym;

	Return(gensym_pretty_stream_(stream, &gensym));
	(void)catch_clang_(ptr, p_format_logicalblock2, gensym, pretty);
	return close_pretty_stream_unwind_protect_(ptr, stream);
}

static int format_logicalblock1_(Execute ptr)
{
	addr pretty, stream, control;

	/* stream */
	getdata_control(ptr, &pretty);
	getstream_format_pretty(pretty, &stream);
	Check(! pretty_stream_p(stream), "type error");
	/* unwind-protect */
	push_control(ptr, &control);
	(void)format_logicalblock1_call_(ptr, pretty, stream);
	return pop_control_(ptr, control);
}

static int format_make_strvect_alloc_(LocalRoot local,
		addr *value, byte *ptr, size_t *ret)
{
	unicode *data;
	size_t size;

	size = *(size_t *)ptr;
	data = (unicode *)(ptr + IdxSize);
	Return(strvect_sizeu_alloc_(local, value, data, size));

	return Result(ret, IdxSize + size * sizeoft(unicode));
}
static int format_make_strvect_heap_(addr *value, byte *ptr, size_t *ret)
{
	return format_make_strvect_alloc_(NULL, value, ptr, ret);
}

static int format_call_LogicalBlock_prefix_(struct format_operator *str,
		addr *rprefix, addr *rperline, addr *rsuffix, size_t *ret)
{
	byte *body;
	addr prefix, perline, suffix;
	size_t now, plus;

	/* Initial value */
	now = sizeoft(struct format_operator);
	if (str->colon) {
		strvect_char_heap(&prefix, "(");
		strvect_char_heap(&suffix, ")");
		perline = Nil;
	}
	else {
		prefix = perline = suffix = Nil;
	}

	/* :prefix, :per-line-prefix */
	body = (byte *)str;
	if (str->prefix) {
		Return(format_make_strvect_heap_(&prefix, body + now, &plus));
		now += plus;
		/* perline */
		if (str->option_check) {
			perline = prefix;
			prefix = Nil;
		}
	}

	/* :suffix */
	if (str->suffix) {
		Return(format_make_strvect_heap_(&suffix, body + now, &plus));
		now += plus;
	}

	/* result */
	*rprefix = prefix;
	*rperline = perline;
	*rsuffix = suffix;
	*ret = now;
	return 0;
}

static int format_call_LogicalBlock_lambda_(
		fmtprint print, struct format_operator *str, addr pos,
		addr *rstream, addr *rlambda)
{
	addr data, prefix, perline, suffix, stream, lambda;
	LocalRoot local;
	Execute ptr;
	size_t now;
	fmtprint save;

	/* make-pprint-stream */
	ptr = print->ptr;
	local = ptr->local;
	Return(format_call_LogicalBlock_prefix_(str, &prefix, &perline, &suffix, &now));
	gchold_pushva_local(local, prefix, perline, suffix, NULL);

	/* object */
	Return(open_pretty_stream_(ptr,
				&stream, print->stream, pos, prefix, perline, suffix));
	format_pretty_heap(&data, stream, print->rest);
	save = fmtprint_format_pretty(data);
	*save = *print;
	save->stream = stream;
	save->now += now;
	save->pretty = 1;
	save->fill = str->close_atsign;
	save->rest = NULL;

	/* function */
	compiled_heap(&lambda, Nil);
	setcompiled_empty(lambda, p_format_logicalblock1);
	SetDataFunction(lambda, data);
	gchold_push_local(local, lambda);

	/* result */
	*rstream = stream;
	*rlambda = lambda;
	return 0;
}

static int format_call_LogicalBlock_call1_(fmtprint print, struct format_operator *str)
{
	addr pos, stream;
	struct fmtstack *rest, args;

	/* argument */
	Return(fmtprint_pop_(print, str, &pos));
	copyheap(&pos, pos);
	rest = print->rest;
	print->rest = &args;
	args.root = pos;
	args.front = pos;
	args.index = 0;
	Return(format_call_LogicalBlock_lambda_(print, str, pos, &stream, &pos));

	/* call */
	Return(call_pretty_stream(print->ptr, stream, pos));

	/* rest */
	print->rest = rest;

	return 0;
}

static int format_call_LogicalBlock_call2_(fmtprint print, struct format_operator *str)
{
	addr pos, stream;
	struct fmtstack *rest;
	size_t index;

	/* argument */
	rest = print->rest;
	index = rest->index;
	copyheap(&(rest->root), rest->root);
	rest->front = rest->root;
	rest->index = 0;
	Return(fmtprint_absolute_(print, str, index));
	Return(format_call_LogicalBlock_lambda_(print, str, rest->front, &stream, &pos));

	/* call */
	Return(call_pretty_stream(print->ptr, stream, pos));

	/* atsign */
	return fmtprint_clear_(print);
}

static int format_call_LogicalBlock_call_(fmtprint print, struct format_operator *str)
{
	if (str->atsign == 0)
		return format_call_LogicalBlock_call1_(print, str);
	else
		return format_call_LogicalBlock_call2_(print, str);
}

static int format_call_LogicalBlock_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr control;

	Check(0 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_LogicalBlock_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  EscapeUpward
 */
static int format_call_EscapeUpward_(fmtprint print, struct format_operator *str)
{
	int ex1, ex2, ex3;
	fixnum v1, v2, v3;

	Check(3 < str->args_size, "size error");
	Check(str->atsign, "atsign error");
	v1 = v2 = v3 = 0;
	Return(fmtint_nilp_(print, str, 0, &v1, &ex1));
	Return(fmtint_nilp_(print, str, 1, &v2, &ex2));
	Return(fmtint_nilp_(print, str, 2, &v3, &ex3));
	ex1 = ! ex1;
	ex2 = ! ex2;
	ex3 = ! ex3;

	if (ex1 && ex2 && ex3) {
		if (v1 <= v2 && v2 <= v3)
			goto break_outside;
		return 0;
	}
	if (ex1 && ex2) {
		if (v1 == v2)
			goto break_outside;
		return 0;
	}
	if (ex1) {
		if (v1 == 0)
			goto break_outside;
		return 0;
	}
	else if (str->colon) {
		if (print->last)
			goto break_outside;
		return 0;
	}
	else {
		if (print->rest->front == Nil)
			goto break_outside;
		return 0;
	}

break_outside:
	if (print->escape == 0 && print->pretty)
		return pprint_throw_(print->ptr, print->stream);
	print->loop = 0;
	print->loop_colon = str->colon;

	return 0;
}


/*
 *  ClauseSeparator
 */
static int format_call_ClauseSeparator_(fmtprint print, struct format_operator *str)
{
	return fmtprop_abort_(print, str, "Invalid ~~; operator.", NULL);
}


/*
 *  CallFunction
 */
static int format_call_CallFunction_object_(fmtprint print,
		struct format_operator *str, size_t index, addr *ret)
{
	fixnum value;
	struct format_argument *arg;
	LocalRoot local;

	local = print->local;
	arg = format_getargs(str, index);
	switch (arg->type) {
		case fmtargs_nil:
			*ret = Nil;
			break;

		case fmtargs_integer:
			fixnum_local(local, ret, arg->u.value);
			break;

		case fmtargs_character:
			character_local(local, ret, arg->u.character);
			break;

		case fmtargs_argument:
			return fmtprint_pop_(print, str, ret);

		case fmtargs_count:
			Return(fmtint_count_(print, &value));
			fixnum_local(local, ret, value);
			break;

		default:
			return fmtprop_abort_(print, str, "Invalid format parameter.", NULL);
	}

	return 0;
}

static int format_call_CallFunction_call_(fmtprint print,
		struct format_operator *str, addr *ret)
{
	byte *ptr;
	addr package, name;
	size_t size;

	ptr = (byte *)format_getargs(str, str->args_size);
	Return(format_make_strvect_heap_(&package, ptr, &size));
	ptr += size;
	Return(format_make_strvect_heap_(&name, ptr, &size));
	return intern_package_(package, name, ret, NULL);
}

static int format_call_CallFunction_(fmtprint print, struct format_operator *str)
{
	addr pos, root;
	LocalRoot local;
	LocalStack stack;
	size_t size, i;

	/* (symbol stream pos colon atsign ...) */
	local = print->local;
	Return(fmtprint_pop_(print, str, &pos));
	/* list */
	push_local(local, &stack);
	root = Nil;
	cons_local(local, &root, print->stream, root);
	cons_local(local, &root, pos, root);
	cons_local(local, &root, str->colon? T: Nil, root);
	cons_local(local, &root, str->atsign? T: Nil, root);
	/* arguments */
	size = str->args_size;
	for (i = 0; i < size; i++) {
		Return(format_call_CallFunction_object_(print, str, i, &pos));
		copyheap(&pos, pos);
		cons_local(local, &root, pos, root);
	}
	nreverse(&root, root);
	/* call */
	Return(format_call_CallFunction_call_(print, str, &pos));
	Return(apply1_control_(print->ptr, &pos, pos, root));
	rollback_local(local, stack);

	return 0;
}


/*
 *  parse-format
 */
static int fmtcall_end(fmtprint print, enum FormatType type)
{
	return print->loop == 0
		|| type == FormatType_End
		|| type == FormatType_ClauseSeparator;
}

static int fmtcall_(fmtprint print, int *loop)
{
	enum FormatType type;
	addr stream, backup_string;
	struct format_operator *str;
	fmtcalltype call;

	/* string-stream */
	Return(fmtprint_make_string_(print, &stream, &backup_string));
	/* loop */
	str = fmtprint_operator(print);
	type = str->type;
	while (! fmtcall_end(print, type)) {
		/* delete-space */
		if (type != FormatType_Output)
			print->delete_space = 0;
		/* call */
		call = FormatCallTable[type];
		Check(call == NULL, "Invalid format type.");
		Return((*call)(print, str));
		/* next */
		print->now += str->size;
		str = fmtprint_operator(print);
		type = str->type;
	}
	if (loop)
		*loop = print->loop_colon;
	/* close */
	close_output_string_stream(stream);
	print->string = backup_string;

	return 0;
}

static int format_execute_call_(Execute ptr,
		addr stream, addr format, struct fmtstack *args)
{
	int check;
	struct fmtprint_struct print;

	fmtprint_make(&print, ptr, stream, format);
	print.rest = args;
	fmtprint_format_forward(&print);

	return fmtcall_(&print, &check);
}

static int format_execute_format_(Execute ptr,
		addr stream, addr format, addr args, addr *ret)
{
	struct fmtstack stack;

	CheckType(stream, LISPTYPE_STREAM);
	CheckType(format, LISPTYPE_FORMAT);
	Check(! listp(args), "type error");
	stack.root = args;
	stack.front = args;
	stack.index = 0;
	Return(format_execute_call_(ptr, stream, format, &stack));
	*ret = stack.front;

	return 0;
}

static int format_execute_function_(Execute ptr,
		addr stream, addr format, addr args, addr *ret)
{
	cons_local(ptr->local, &args, stream, args);
	return apply1_control_(ptr, ret, format, args);
}

static int format_execute_type_(Execute ptr,
		addr stream, addr format, addr args, addr *ret)
{
	if (functionp(format))
		return format_execute_function_(ptr, stream, format, args, ret);
	if (formatp(format))
		return format_execute_format_(ptr, stream, format, args, ret);
	if (stringp(format)) {
		Return(format_parse_local_(ptr->local, &format, format));
		return format_execute_format_(ptr, stream, format, args, ret);
	}
	return fmte_("Invalid control-string ~S.", format, NULL);
}

int format_execute_(Execute ptr, addr stream, addr format, addr args, addr *ret)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	gchold_pushva_force_local(local, stream, format, args, NULL);
	Return(format_execute_type_(ptr, stream, format, args, &args));
	rollback_local(local, stack);
	Return(exitpoint_stream_(stream));
	copyheap(ret, args);

	return 0;
}


/*
 *  initialize
 */
#define SetFormatCallTable(x) (FormatCallTable[FormatType_##x] = format_call_##x##_)
void init_format_function(void)
{
	cleartype(FormatCallTable);
	SetFormatCallTable(Error);
	SetFormatCallTable(End);
	SetFormatCallTable(Format);
	SetFormatCallTable(Output);
	SetFormatCallTable(Aesthetic);
	SetFormatCallTable(Standard);
	SetFormatCallTable(Binary);
	SetFormatCallTable(Octal);
	SetFormatCallTable(Decimal);
	SetFormatCallTable(Hexadecimal);
	SetFormatCallTable(Radix);
	SetFormatCallTable(RadixText);
	SetFormatCallTable(Plural);
	SetFormatCallTable(Character);
	SetFormatCallTable(Fixed);
	SetFormatCallTable(Exponential);
	SetFormatCallTable(General);
	SetFormatCallTable(Monetary);
	SetFormatCallTable(Newline);
	SetFormatCallTable(FreshLine);
	SetFormatCallTable(Page);
	SetFormatCallTable(Tilde);
	SetFormatCallTable(IgnoredNewline);
	SetFormatCallTable(Tabulate);
	SetFormatCallTable(GoTo);
	SetFormatCallTable(Recursive);
	SetFormatCallTable(ConditionalNewline);
	SetFormatCallTable(Write);
	SetFormatCallTable(Indent);
	SetFormatCallTable(Case);
	SetFormatCallTable(Condition);
	SetFormatCallTable(Iteration);
	SetFormatCallTable(Justification);
	SetFormatCallTable(LogicalBlock);
	SetFormatCallTable(EscapeUpward);
	SetFormatCallTable(ClauseSeparator);
	SetFormatCallTable(CallFunction);

	/* LogicalBlock */
	SetPointerType_(empty, format_logicalblock1);
	SetPointerType_(empty, format_logicalblock2);
}


/************************************************************
 *  format_parse.c
 ************************************************************/

/*
 *  fmtinput
 */
int format_abort_(addr format, size_t position, const char *str, va_list args)
{
	size_t i, size;
	addr pos, list, stream;

	copylocal_list_stdarg(NULL, &list, args);
	if (formatp(format)) {
		Return(format_string_heap_(&format, format));
	}
	else {
		copyheap(&format, format);
	}
	conscar_heap(&pos, format);
	nconc2_unsafe(list, pos, &list);

	strvect_char_heap(&pos, str);
	open_output_string_stream(&stream, 0);
	Return(print_string_stream_(stream, pos));
	Return(fresh_line_stream_(stream, NULL));

	/* Format error: ~1,2,3,4,5,6,7,8F */
	Return(print_ascii_stream_(stream, "Format error: ~A"));
	string_length(format, &size);
	Return(fresh_line_stream_(stream, NULL));

	/*                      |          */
	Return(print_ascii_stream_(stream, "             "));
	string_length(format, &size);
	for (i = 0; i < position; i++) {
		Return(write_char_stream_(stream, ' '));
	}
	Return(write_char_stream_(stream, '^'));

	/* error */
	Return(string_stream_heap_(stream, &pos));
	return call_simple_error_(NULL, pos, list);
}

static int format_abort_va_(addr format, size_t index, const char *str, ...)
{
	va_list args;

	va_start(args, str);
	Return(format_abort_(format, index, str, args));
	va_end(args);

	return 0;
}


/*
 *  fmtinput
 */
struct fmtinput_struct {
	LocalRoot local;
	addr format;
	size_t size, index;
};
typedef struct fmtinput_struct *fmtinput;

struct fmtargs {
	struct fmtargs *next;
	enum fmtargs_type type;
	size_t position;
	union format_union u;
};

struct fmtchar {
	unsigned colon : 1;
	unsigned atsign : 1;
	unsigned close_colon : 1;
	unsigned close_atsign : 1;
	unsigned option_check : 1;
	unsigned prefix : 1;
	unsigned suffix : 1;
	unsigned size;
	unicode character;
	enum FormatType type;
	struct fmtchar *next, *option;
	size_t position, colon_pos, atsign_pos, option_count;
	struct fmtargs *root, *tail;
	addr intern, format;
};

static int fmtinput_abort_(fmtinput input, const char *str, ...)
{
	va_list args;

	va_start(args, str);
	Return(format_abort_(input->format, input->index, str, args));
	va_end(args);

	return 0;
}

static int fmtinput_init_(fmtinput input, LocalRoot local, addr format)
{
#ifdef LISP_DEBUG
	aamemory(input, sizeoft(struct fmtinput_struct));
#endif
	input->local = local;
	input->format = format;
	input->index = 0;
	string_length(format, &(input->size));

	return 0;
}

static int fmtinput_peek_(fmtinput input, unicode *c, int *ret)
{
	if (input->size <= input->index)
		return Result(ret, 1);
	Return(string_getc_(input->format, input->index, c));
	return Result(ret, 0);
}

static int fmtinput_getc_(fmtinput input, unicode *c, int *ret)
{
	int check;

	Return(fmtinput_peek_(input, c, &check));
	if (! check)
		input->index++;

	return Result(ret, check);
}

static int fmtinput_getcheck_(fmtinput input, unicode *c)
{
	int check;

	Return(fmtinput_getc_(input, c, &check));
	if (check)
		return fmtinput_abort_(input, "Invalid format string.", NULL);

	return 0;
}


/*
 *  fmtchar-parse
 */
static void fmtchar_init(fmtinput input, struct fmtchar *str)
{
	clearpoint(str);
	str->type = FormatType_Error;
	str->position = input->index;
	str->format = input->format;
}

static struct fmtargs *fmtargs_make(fmtinput input, struct fmtchar *str)
{
	struct fmtargs *ptr;

	ptr = (struct fmtargs *)lowlevel_local(input->local, sizeoft(struct fmtargs));
	clearpoint(ptr);
	if (str->root)
		str->tail = str->tail->next = ptr;
	else
		str->root = str->tail = ptr;
	ptr->type = fmtargs_nil;
	ptr->position = input->index;
	str->size++;

	return ptr;
}

static void fmtchar_push(fmtinput input, struct fmtchar *str)
{
	struct fmtargs *argtype;

	argtype = fmtargs_make(input, str);
	argtype->type = fmtargs_argument;
}

static void fmtchar_sharp(fmtinput input, struct fmtchar *str)
{
	struct fmtargs *argtype;

	argtype = fmtargs_make(input, str);
	argtype->type = fmtargs_count;
}

static int fmtchar_character_(fmtinput input, struct fmtchar *str)
{
	unicode u;
	struct fmtargs *argtype;

	argtype = fmtargs_make(input, str);
	argtype->type = fmtargs_character;
	Return(fmtinput_getcheck_(input, &u));
	argtype->u.character = u;

	return 0;
}

static int fmtchar_sign_(fmtinput input, int *sign, unicode u)
{
	if (*sign)
		return fmtinput_abort_(input, "Invalid sign character.", NULL);
	*sign = (int)u;

	return 0;
}

static void fmtchar_nil(fmtinput input, struct fmtchar *str)
{
	struct fmtargs *argtype;

	argtype = fmtargs_make(input, str);
	argtype->type = fmtargs_nil;
}

static int fmtchar_value_parse_(fmtinput input, int sign, addr queue, fixnum *ret)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	fixnum value;

	local = input->local;
	push_local(local, &stack);
	if (sign == '+')
		sign = signplus_bignum;
	if (sign == '-')
		sign = signminus_bignum;
	if (fixnum_cons_alloc(local, &pos, sign, queue)) {
		*ret = 0;
		return fmtinput_abort_(input, "Too large integer value.", NULL);
	}
	GetFixnum(pos, &value);
	rollback_local(local, stack);
	clear_bigcons(queue);

	return Result(ret, value);
}

static int fmtchar_value_(fmtinput input, struct fmtchar *str, int *sign, addr queue)
{
	struct fmtargs *argtype;
	fixnum value;

	argtype = fmtargs_make(input, str);
	argtype->type = fmtargs_integer;
	Return(fmtchar_value_parse_(input, *sign, queue, &value));
	argtype->u.value = value;
	*sign = 0;

	return 0;
}

static int fmtchar_nilcheck_(fmtinput input, int sign, addr queue, int *ret)
{
	if (! bigcons_empty_p(queue))
		return Result(ret, 0);
	if (sign == 0)
		return Result(ret, 1);
	/* error */
	*ret = 0;
	return fmtinput_abort_(input, "Invalid integer value.", NULL);
}

static int fmtchar_colon_(fmtinput input, struct fmtchar *str)
{
	if (str->colon)
		return fmtinput_abort_(input, "Format parameter ':' already supplies.", NULL);
	str->colon_pos = input->index;
	str->colon = 1;

	return 0;
}

static int fmtchar_atsign_(fmtinput input, struct fmtchar *str)
{
	if (str->atsign)
		return fmtinput_abort_(input, "Format parameter '@' already supplies.", NULL);
	str->atsign_pos = input->index;
	str->atsign = 1;

	return 0;
}

static enum FormatType FormatCharacter[0x80];
static int fmtchar_type_(fmtinput input, struct fmtchar *str)
{
	enum FormatType type;
	addr pos;
	unicode c;

	c = str->character;
	if (0x80 <= c)
		goto error;
	type = FormatCharacter[c];
	if (type == FormatType_Radix) {
		if (str->size == 0)
			type = FormatType_RadixText;
	}
	if (type != FormatType_Error) {
		str->type = type;
		return 0;
	}
	if (c == '>' || c == ')' || c == ']' || c == '}') {
		str->type = FormatType_Error;
		return 0;
	}

error:
	character_heap(&pos, c);
	str->type = FormatType_Error;
	return fmtinput_abort_(input, "Invalid operator character, ~S.", pos, NULL);
}

static int fmtchar_parse_function_(fmtinput input, struct fmtchar *str)
{
	unicode u;
	addr queue, package, pos;
	LocalRoot local;
	size_t size;

	local = input->local;
	charqueue_local(local, &queue, 0);

first:
	Return(fmtinput_getcheck_(input, &u));
	if (u == '/')
		goto first1;
	if (u == ':')
		goto colon1;
	u = toUpperUnicode(u);
	Return(push_charqueue_local_(local, queue, u));
	goto first;

first1:
	GetCharQueueSize(queue, &size);
	if (size == 0)
		goto error;
	strvect_char_local(local, &package, LISP_COMMON_USER);
	goto finish;

colon1:
	GetCharQueueSize(queue, &size);
	if (size == 0) {
		return fmtinput_abort_(input,
				"The package name is empty. (Don't use keyword)", NULL);
	}
	make_charqueue_local(local, queue, &package);
	clear_charqueue(queue);

	Return(fmtinput_getcheck_(input, &u));
	if (u == '/')
		goto error;
	if (u == ':')
		goto colon2;
	u = toUpperUnicode(u);
	Return(push_charqueue_local_(local, queue, u));
	goto colon3;

colon2:
	Return(fmtinput_getcheck_(input, &u));
	if (u == '/')
		goto error;
	if (u == ':')
		return fmtinput_abort_(input, "Invalid colon ::: separator.", NULL);
	u = toUpperUnicode(u);
	Return(push_charqueue_local_(local, queue, u));
	goto colon3;

colon3:
	Return(fmtinput_getcheck_(input, &u));
	if (u == '/')
		goto finish;
	u = toUpperUnicode(u);
	Return(push_charqueue_local_(local, queue, u));
	goto colon3;

error:
	return fmtinput_abort_(input, "The function name is empty.", NULL);

finish:
	make_charqueue_local(local, queue, &pos);
	cons_local(local, &pos, package, pos);
	str->intern = pos;
	return 0;
}

static int fmtchar_parse_(fmtinput input, struct fmtchar *str, addr queue)
{
	int check, sign, comma;
	unicode u;
	LocalRoot local;

	local = input->local;
	clear_bigcons(queue);
	fmtchar_init(input, str);
	sign = 0;
	comma = 0;
arguments:
	Return(fmtinput_getcheck_(input, &u));
	if (u == ':' || u == '@') {
		goto colon_atsign;
	}
	if (u == 'v' || u == 'V') {
		fmtchar_push(input, str);
		comma = 1;
		goto arguments;
	}
	if (u == '#') {
		fmtchar_sharp(input, str);
		comma = 1;
		goto arguments;
	}
	if (u == '\'') {
		Return(fmtchar_character_(input, str));
		comma = 1;
		goto arguments;
	}
	if (u == '-' || u == '+') {
		Return(fmtchar_sign_(input, &sign, u));
		comma = 1;
		goto arguments;
	}
	if (isDigitCase(u)) {
		push_bigcons(local, queue, 10, (unsigned)(u - '0'));
		comma = 1;
		goto arguments;
	}
	if (u == ',') {
		goto push_value;
	}
	goto finish;

push_value:
	Return(fmtchar_nilcheck_(input, sign, queue, &check));
	if (check) {
		if (comma == 0)
			fmtchar_nil(input, str);
	}
	else {
		Return(fmtchar_value_(input, str, &sign, queue));
	}
	comma = 0;
	goto arguments;

colon_atsign:
	Return(fmtchar_nilcheck_(input, sign, queue, &check));
	if (! check) {
		Return(fmtchar_value_(input, str, &sign, queue));
	}

colon_atsign_loop:
	if (u == ':') {
		Return(fmtchar_colon_(input, str));
	}
	else if (u == '@') {
		Return(fmtchar_atsign_(input, str));
	}
	else {
		goto finish;
	}
	Return(fmtinput_getcheck_(input, &u));
	goto colon_atsign_loop;

finish:
	Return(fmtchar_nilcheck_(input, sign, queue, &check));
	if (! check) {
		Return(fmtchar_value_(input, str, &sign, queue));
	}
	if (u == 0) {
		return fmtinput_abort_(input, "Invalid operator character, \\0.", NULL);
	}
	if (u == '/') {
		Return(fmtchar_parse_function_(input, str));
	}

	/* operator */
	str->character = toUpperUnicode(u);
	return fmtchar_type_(input, str);
}


/*
 *  fmtchar-group
 */
struct fmtroot {
	struct fmtchar *root, *tail;
	size_t start;
};

static struct fmtchar *fmtchar_local(fmtinput input)
{
	struct fmtchar *ptr;

	ptr = (struct fmtchar *)lowlevel_local(input->local, sizeoft(struct fmtchar));
	clearpoint(ptr);
	ptr->format = input->format;

	return ptr;
}

static void fmtroot_push(LocalRoot local, struct fmtroot *root, struct fmtchar *comm)
{
	if (root->root)
		root->tail = root->tail->next = comm;
	else
		root->tail = root->root = comm;
}

static void fmtroot_output(fmtinput input, struct fmtroot *root)
{
	struct fmtchar *comm;
	struct fmtargs *args;
	size_t p1, p2;
	LocalRoot local;

	local = input->local;
	p1 = root->start;
	p2 = input->index;
	if (p1 != p2) {
		comm = fmtchar_local(input);
		comm->character = 0;
		comm->type = FormatType_Output;
		comm->position = p2;
		/* args[0]: count */
		args = fmtargs_make(input, comm);
		args->type = fmtargs_index;
		args->u.index = (p2 - p1);
		/* args[1]: start */
		args = fmtargs_make(input, comm);
		args->type = fmtargs_index;
		args->u.index = p1;
		/* args[2]: end */
		args = fmtargs_make(input, comm);
		args->type = fmtargs_index;
		args->u.index = p2;
		/* result */
		fmtroot_push(local, root, comm);
		root->start = p2;
	}
}

static int fmtchar_loop_(fmtinput input, struct fmtchar **ret)
{
	int check;
	LocalRoot local;
	addr queue;
	unicode u;
	struct fmtchar *comm;
	struct fmtroot root;

	local = input->local;
	cleartype(root);
	bigcons_local(local, &queue);
	for (;;) {
		Return(fmtinput_peek_(input, &u, &check));
		if (check)
			break;
		if (u != '~') {
			input->index++;
			continue;
		}
		/* text output */
		fmtroot_output(input, &root);
		input->index++;
		/* operator */
		comm = fmtchar_local(input);
		Return(fmtchar_parse_(input, comm, queue));
		fmtroot_push(local, &root, comm);
		root.start = input->index;
	}
	/* text output */
	fmtroot_output(input, &root);

	return Result(ret, root.root);
}


/* fmtgroup */
struct fmtgroup {
	unsigned semicolon : 1;
	unsigned result_colon : 1;
	unsigned result_atsign : 1;
	struct fmtchar *root, *list;
	LocalRoot local;
	addr format;
	unicode a;
};

static int fmtchar_group_eof_(struct fmtgroup *group)
{
	addr pos;
	size_t size;

	character_local(group->local, &pos, group->a);
	string_length(group->format, &size);
	return format_abort_va_(group->format,
			size, "There is no parensis ~S.", pos, NULL);
}

static int fmtchar_group_justification_(addr format, struct fmtchar *list)
{
	if (list->close_colon) {
		list->type = FormatType_LogicalBlock;
		return 0;
	}

	/* Disable operator in Justification block.
	 *   ~W          write
	 *   ~_          pprint-newline
	 *   ~I          pprint-indent
	 *   ~:T         pprint-tab
	 *   ~<...~:>    pprint-logical-block
	 */
	for (list = list->option; list; list = list->next) {
		switch (list->type) {
			case FormatType_Write:
			case FormatType_ConditionalNewline:
			case FormatType_Indent:
			case FormatType_LogicalBlock:
				goto error;

			case FormatType_Tabulate:
				if (list->colon)
					goto error;
				break;

			default:
				break;
		}
	}
	return 0;

	/* error */
error:
	return format_abort_va_(format, list->position,
			"Don't use the operator in Justification block.", NULL);
}

static int fmtchar_group_type_(addr format, struct fmtchar *list)
{
	switch (list->type) {
		case FormatType_Justification:
			return fmtchar_group_justification_(format, list);

		case FormatType_Error:
			return fmte_("format type error", NULL);

		default:
			return 0;
	}
}

static int fmtchar_group_(struct fmtgroup *group);
static int fmtchar_group_set_(struct fmtgroup *group, unicode a, int semicolon)
{
	struct fmtchar *list, *next;
	struct fmtgroup backup;

	/* backup */
	backup = *group;
	list = group->list;
	next = list->next;
	/* parameter */
	group->semicolon = (semicolon != 0);
	group->result_colon = 0;
	group->result_atsign = 0;
	group->a = a;
	group->list = next;
	/* group */
	Return(fmtchar_group_(group));
	/* option */
	list->option = next;
	list->close_colon = group->result_colon;
	list->close_atsign = group->result_atsign;
	group->result_colon = 0;
	group->result_atsign = 0;
	Return(fmtchar_group_type_(group->format, list));
	/* next list */
	list = list->next = group->list;
	*group = backup;
	group->list = list;

	return 0;
}

static int fmtchar_group_close_(struct fmtgroup *group)
{
	addr pos;
	struct fmtchar *list;

	list = group->list;
	if (group->a != list->character) {
		character_heap(&pos, group->a);
		return format_abort_va_(group->format, list->position,
				"The close parensis ~S mismatch.", pos, NULL);
	}
	if (list->size) {
		character_heap(&pos, group->a);
		return format_abort_va_(group->format, list->position,
				"The close parensis ~S cannot use parameters.", pos, NULL);
	}
	group->list = list->next;
	list->next = NULL;
	group->result_colon = list->colon;
	group->result_atsign = list->atsign;

	return 0;
}

static int fmtchar_group_semicolon_(struct fmtgroup *group)
{
	if (! group->semicolon) {
		return format_abort_va_(group->format, group->list->position,
				"Invalid ~~; parameter.", NULL);
	}
	group->list = group->list->next;

	return 0;
}

static int fmtchar_group_(struct fmtgroup *group)
{
	while (group->list) {
		switch (group->list->character) {
			case '(':
				Return(fmtchar_group_set_(group, ')', 0));
				break;

			case '[': /* semicolon */
				Return(fmtchar_group_set_(group, ']', 1));
				break;

			case '{':
				Return(fmtchar_group_set_(group, '}', 0));
				break;

			case '<': /* semicolon */
				Return(fmtchar_group_set_(group, '>', 1));
				break;

			case ')':
			case ']':
			case '}':
			case '>':
				return fmtchar_group_close_(group);

			case ';':
				Return(fmtchar_group_semicolon_(group));
				break;

			default:
				group->list = group->list->next;
				break;
		}
	}
	if (group->a != 0) {
		Return(fmtchar_group_eof_(group));
	}

	return 0;
}

static struct fmtchar *fmtchar_empty(fmtinput input)
{
	struct fmtchar *comm;
	struct fmtargs *args;

	comm = fmtchar_local(input);
	comm->character = 0;
	comm->type = FormatType_Output;
	comm->position = 0;
	/* args[0]: count */
	args = fmtargs_make(input, comm);
	args->type = fmtargs_index;
	args->u.index = 0;
	/* args[1]: start */
	args = fmtargs_make(input, comm);
	args->type = fmtargs_index;
	args->u.index = 0;
	/* args[2]: end */
	args = fmtargs_make(input, comm);
	args->type = fmtargs_index;
	args->u.index = 0;

	return comm;
}

static int fmtchar_make_(LocalRoot local, addr format, struct fmtchar **ret)
{
	struct fmtinput_struct input;
	struct fmtchar *list;
	struct fmtgroup group;

	/* loop */
	Return(fmtinput_init_(&input, local, format));
	Return(fmtchar_loop_(&input, &list));
	if (list == NULL)
		list = fmtchar_empty(&input);

	/* group */
	cleartype(group);
	group.local = local;
	group.format = format;
	group.root = group.list = list;
	Return(fmtchar_group_(&group));

	return Result(ret, list);
}


/*
 *  format write
 */
static int format_size_(struct fmtchar *list, size_t *ret);
static int format_write_(struct fmtchar *list, byte *ptr, size_t *ret);

#define FormatOperatorSize (sizeoft(struct format_operator))
#define FormatArgumentSize (sizeoft(struct format_argument))
#define FormatByteSize(x) (FormatOperatorSize + FormatArgumentSize * (x))

static struct format_operator *format_write_operator(
		struct fmtchar *list, byte *ptr, size_t args)
{
	struct format_operator *str;

	str = (struct format_operator *)ptr;
	str->type = list->type;
	str->size = FormatByteSize(args);
	str->colon = list->colon;
	str->atsign = list->atsign;
	str->close_colon = list->close_colon;
	str->close_atsign = list->close_atsign;
	str->option_check = list->option_check;
	str->prefix = list->prefix;
	str->suffix = list->suffix;
	str->args_size = args;
	str->position = list->position;
	str->colon_pos = list->colon_pos;
	str->atsign_pos = list->atsign_pos;

	return str;
}

static byte *format_write_body(byte *ptr, size_t index)
{
	return ptr + FormatByteSize(index);
}

static struct format_argument *format_write_argument(
		struct fmtargs **list, byte *ptr, size_t index)
{
	struct format_argument *str;
	struct fmtargs *arg;

	Check(list == NULL, "list error");
	arg = *list;
	str = (struct format_argument *)format_write_body(ptr, index);
	str->type = arg? arg->type: fmtargs_nil;
	str->position = arg? arg->position: 0;
	cleartype(str->u);
	switch (str->type) {
		case fmtargs_integer:
			str->u.value = arg->u.value;
			break;

		case fmtargs_character:
			str->u.character = arg->u.character;
			break;

		case fmtargs_index:
			str->u.index = arg->u.index;
			break;

		default:
			break;
	}
	if (arg)
		*list = arg->next;

	return str;
}

/* Output */
static int format_size_Output(struct fmtchar *list, size_t *ret)
{
	struct fmtargs *root;
	size_t count;
#ifdef LISP_DEBUG
	size_t a, b;
#endif

	Check(list->size != 3, "size error");
	/* count */
	root = list->root;
	Check(root->type != fmtargs_index, "argument count error");
	count = root->u.index;
#ifdef LISP_DEBUG
	/* start */
	root = root->next;
	Check(root->type != fmtargs_index, "argument start error");
	a = root->u.index;
	/* end */
	root = root->next;
	Check(root->type != fmtargs_index, "argument end error");
	b = root->u.index;
	/* size */
	Check(b < a, "size1 error");
	Check(count != (b - a), "size2 error");
#endif
	*ret = FormatByteSize(3) + count * sizeoft(unicode);

	return 0;
}

static int format_write_Output(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct fmtargs *root;
	size_t count, a, b, i;
	unicode c, *body;

	/* structure */
	str = format_write_operator(list, ptr, 3);
	/* count */
	root = list->root;
	count = root->u.index;
	format_write_argument(&root, ptr, 0);
	/* start */
	a = root->u.index;
	format_write_argument(&root, ptr, 1);
	/* end */
	b = root->u.index;
	format_write_argument(&root, ptr, 2);
	/* body */
	format = list->format;
	str->size += count * sizeoft(unicode);
	body = (unicode *)format_write_body(ptr, 3);
	for (i = 0; a < b; a++) {
		Return(string_getc_(format, a, &c));
		body[i++] = c;
	}
	/* result */
	return Result(ret, str->size);
}


/* Format */
static int format_size_Format(struct fmtchar *list, size_t *ret)
{
	size_t size;

	string_length(list->format, &size);
	*ret = FormatByteSize(1) + size * sizeoft(unicode);

	return 0;
}

static struct format_operator *format_write_empty_structure(byte *ptr,
		enum FormatType type, size_t args)
{
	struct format_operator *str;

	str = (struct format_operator *)ptr;
	str->type = type;
	str->size = FormatByteSize(args);
	str->colon = 0;
	str->atsign = 0;
	str->close_colon = 0;
	str->close_atsign = 0;
	str->option_check = 0;
	str->prefix = 0;
	str->suffix = 0;
	str->args_size = args;
	str->position = 0;
	str->colon_pos = 0;
	str->atsign_pos = 0;

	return str;
}

static int format_write_Format(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct fmtargs args, *root;
	size_t size, i;
	unicode c, *body;

	/* structure */
	str = format_write_empty_structure(ptr, FormatType_Format, 1);
	format = list->format;
	string_length(format, &size);
	/* start */
	cleartype(args);
	args.type = fmtargs_index;
	args.u.index = size;
	root = &args;
	format_write_argument(&root, ptr, 0);
	/* body */
	str->size += size * sizeoft(unicode);
	body = (unicode *)format_write_body(ptr, 1);
	for (i = 0; i < size; i++) {
		Return(string_getc_(format, i, &c));
		body[i] = c;
	}
	/* result */
	return Result(ret, str->size);
}


/* End */
static int format_size_End(size_t *ret)
{
	return Result(ret, FormatOperatorSize);
}

static int format_write_End(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	str = format_write_empty_structure(ptr, FormatType_End, 0);
	return Result(ret, str->size);
}


/* default parameter */
static int format_size_error_(struct fmtchar *list, size_t index)
{
	if (index < list->size) {
		return format_abort_va_(list->format,
				list->position, "Too many parameters.", NULL);
	}

	return 0;
}

static int format_size_index_(struct fmtchar *list, size_t *ret, size_t index)
{
	Return(format_size_error_(list, index));
	return Result(ret, FormatByteSize(index));
}
static int format_size_size0(struct fmtchar *list, size_t *ret)
{
	return format_size_index_(list, ret, 0);
}
static int format_size_size1(struct fmtchar *list, size_t *ret)
{
	return format_size_index_(list, ret, 1);
}
static int format_size_size2(struct fmtchar *list, size_t *ret)
{
	return format_size_index_(list, ret, 2);
}
static int format_size_size3(struct fmtchar *list, size_t *ret)
{
	return format_size_index_(list, ret, 3);
}
static int format_size_size4(struct fmtchar *list, size_t *ret)
{
	return format_size_index_(list, ret, 4);
}
static int format_size_size5(struct fmtchar *list, size_t *ret)
{
	return format_size_index_(list, ret, 5);
}
static int format_size_size7(struct fmtchar *list, size_t *ret)
{
	return format_size_index_(list, ret, 7);
}


/* ~A */
static int format_argument_integer_check_(addr format,
		struct format_argument *ptr, fixnum defvalue,
		fixnum *value, int *ret)
{
	switch (ptr->type) {
		case fmtargs_nil:
			ptr->type = fmtargs_integer;
			ptr->u.value = defvalue;
			*value = defvalue;
			return Result(ret, 1);

		case fmtargs_integer:
			*value = ptr->u.value;
			return Result(ret, 1);

		case fmtargs_argument:
		case fmtargs_count:
			return Result(ret, 0);

		default:
			*value = 0;
			*ret = 0;
			return format_abort_va_(format, ptr->position,
					"The format parameter must be an integer.", NULL);
	}
}

static int format_argument_integer_(addr format,
		struct format_argument *ptr, fixnum value)
{
	int check;
	return format_argument_integer_check_(format, ptr, value, &value, &check);
}

static int format_argument_less_(addr format,
		struct format_argument *ptr, fixnum defvalue, fixnum value)
{
	int check;
	fixnum less;

	Return(format_argument_integer_check_(format, ptr, defvalue, &less, &check));
	if (check) {
		if (less < value) {
			return format_abort_va_(format, ptr->position,
					"The parameter must be greater than ~A.",
					fixnumh(value), NULL);
		}
	}

	return 0;
}

static int format_argument_integer_nil_(addr format, struct format_argument *ptr)
{
	switch (ptr->type) {
		case fmtargs_nil:
		case fmtargs_integer:
		case fmtargs_argument:
		case fmtargs_count:
			return 0;

		default:
			return format_abort_va_(format, ptr->position,
					"The format parameter must be an integer.", NULL);
	}
}

static int format_argument_less_nil_(addr format,
		struct format_argument *ptr, fixnum check)
{
	switch (ptr->type) {
		case fmtargs_integer:
			if (ptr->u.value < check) {
				return format_abort_va_(format, ptr->position,
						"The parameter must be greater than ~A.",
						fixnumh(check), NULL);
			}
			break;

		case fmtargs_nil:
		case fmtargs_argument:
		case fmtargs_count:
			break;

		default:
			return format_abort_va_(format, ptr->position,
					"The format parameter must be an integer.", NULL);
	}

	return 0;
}

static int format_argument_radix_(addr format, struct format_argument *ptr)
{
	switch (ptr->type) {
		case fmtargs_integer:
			if (! isBaseChar(ptr->u.value)) {
				return format_abort_va_(format, ptr->position,
						"The parameter must be an integer between 2 and 36.", NULL);
			}
			break;

		case fmtargs_argument:
		case fmtargs_count:
			break;

		case fmtargs_nil:
			return format_abort_va_(format, ptr->position,
					"There is no radix parameter.", NULL);

		default:
			return format_abort_va_(format, ptr->position,
					"The format parameter must be an integer.", NULL);
	}

	return 0;
}

static int format_argument_character_(addr format,
		struct format_argument *ptr, unicode value)
{
	switch (ptr->type) {
		case fmtargs_nil:
			ptr->type = fmtargs_character;
			ptr->u.value = value;
			break;

		case fmtargs_character:
		case fmtargs_argument:
		case fmtargs_count:
			break;

		default:
			return format_abort_va_(format, ptr->position,
					"The format parameter must be a character.", NULL);
	}

	return 0;
}

static int format_argument_character_nil_(addr format, struct format_argument *ptr)
{
	switch (ptr->type) {
		case fmtargs_nil:
		case fmtargs_character:
		case fmtargs_argument:
		case fmtargs_count:
			return 0;

		default:
			return format_abort_va_(format, ptr->position,
					"The format parameter must be a character.", NULL);
	}
}


/* ~A, ~S */
static int format_write_Aesthetic(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	str = format_write_operator(list, ptr, 4);
	format = list->format;
	/* mincol */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_less_(format, arg, 0, 0));
	/* colinc */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_less_(format, arg, 1, 1));
	/* minpad */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_less_(format, arg, 0, 0));
	/* padchar */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_character_(format, arg, ' '));
	/* result */
	return Result(ret, str->size);
}


/* ~B, ~O, ~D, ~X */
static int format_write_Binary(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	str = format_write_operator(list, ptr, 4);
	format = list->format;
	/* mincol */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_less_(format, arg, 0, 0));
	/* padchar */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_character_(format, arg, ' '));
	/* commachar */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_character_(format, arg, ','));
	/* commainterval */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_less_(format, arg, 3, 1));
	/* result */
	return Result(ret, str->size);
}


/* ~nR */
static int format_write_Radix(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	str = format_write_operator(list, ptr, 5);
	format = list->format;
	/* radix */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_radix_(format, arg));
	/* mincol */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_less_(format, arg, 0, 0));
	/* padchar */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_character_(format, arg, ' '));
	/* commachar */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_character_(format, arg, ','));
	/* commainterval */
	arg = format_write_argument(&root, ptr, 4);
	Return(format_argument_less_(format, arg, 3, 1));
	/* result */
	return Result(ret, str->size);
}

/* ~R, ~P, ~C, ~\n */
static int format_write_Empty(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	str = format_write_operator(list, ptr, 0);
	return Result(ret, str->size);
}


/* ~F */
static int format_argument_wdn_(addr format, struct format_argument *ptr)
{
	switch (ptr->type) {
		case fmtargs_integer:
			if (ptr->u.value < 0) {
				return format_abort_va_(format, ptr->position,
						"The parameter must be a positive integer.", NULL);
			}
			break;

		case fmtargs_nil:
		case fmtargs_argument:
		case fmtargs_count:
			break;

		default:
			return format_abort_va_(format, ptr->position,
					"The format parameter must be an integer.", NULL);
	}

	return 0;
}

static int format_write_Fixed(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	/* colon */
	if (list->colon) {
		return format_abort_va_(list->format, list->colon_pos,
				"The operator cannot use : parameter.", NULL);
	}
	/* parameter */
	str = format_write_operator(list, ptr, 5);
	format = list->format;
	/* w */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_wdn_(format, arg));
	/* d */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_wdn_(format, arg));
	/* k */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_integer_(format, arg, 0)); /* fixed = 0 */
	/* overflowchar */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_character_nil_(format, arg));
	/* padchar */
	arg = format_write_argument(&root, ptr, 4);
	Return(format_argument_character_(format, arg, ' '));
	/* result */
	return Result(ret, str->size);
}


/* ~E */
static int format_write_Exponent(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	/* colon */
	if (list->colon) {
		return format_abort_va_(list->format, list->colon_pos,
				"The operator cannot use : parameter.", NULL);
	}
	/* parameter */
	str = format_write_operator(list, ptr, 7);
	format = list->format;
	/* w */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_wdn_(format, arg));
	/* d */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_wdn_(format, arg));
	/* e */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_less_(format, arg, 1, 0));
	if (arg->u.value == 0)
		arg->u.value = 1;
	/* k */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_integer_(format, arg, 1)); /* exponent = 1 */
	/* overflowchar */
	arg = format_write_argument(&root, ptr, 4);
	Return(format_argument_character_nil_(format, arg));
	/* padchar */
	arg = format_write_argument(&root, ptr, 5);
	Return(format_argument_character_(format, arg, ' '));
	/* exponentchar */
	arg = format_write_argument(&root, ptr, 6);
	Return(format_argument_character_nil_(format, arg));
	/* result */
	return Result(ret, str->size);
}


/* ~G */
static int format_argument_e_general_(addr format, struct format_argument *ptr)
{
	switch (ptr->type) {
		case fmtargs_integer:
			if (ptr->u.value < 0) {
				return format_abort_va_(format, ptr->position,
						"The parameter must be a positive integer.", NULL);
			}
			if (ptr->u.value == 0)
				ptr->u.value = 1;
			break;

		case fmtargs_nil:
		case fmtargs_argument:
		case fmtargs_count:
			break;

		default:
			return format_abort_va_(format, ptr->position,
					"The format parameter must be an integer.", NULL);
	}

	return 0;
}

static int format_write_General(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	/* colon */
	if (list->colon) {
		return format_abort_va_(list->format, list->colon_pos,
				"The operator cannot use : parameter.", NULL);
	}
	/* parameter */
	str = format_write_operator(list, ptr, 7);
	format = list->format;
	/* w */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_wdn_(format, arg));
	/* d */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_wdn_(format, arg));
	/* e */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_e_general_(format, arg));
	/* k */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_integer_(format, arg, 1)); /* general = 1 */
	/* overflowchar */
	arg = format_write_argument(&root, ptr, 4);
	Return(format_argument_character_nil_(format, arg));
	/* padchar */
	arg = format_write_argument(&root, ptr, 5);
	Return(format_argument_character_(format, arg, ' '));
	/* exponentchar */
	arg = format_write_argument(&root, ptr, 6);
	Return(format_argument_character_nil_(format, arg));
	/* result */
	return Result(ret, str->size);
}


/* ~$ */
static int format_write_Monetary(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	str = format_write_operator(list, ptr, 4);
	format = list->format;
	/* d */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_wdn_(format, arg));
	/* n */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_wdn_(format, arg));
	/* w */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_e_general_(format, arg));
	/* padchar */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_character_(format, arg, ' '));
	/* result */
	return Result(ret, str->size);
}


/* ~%, ~&, ~|, ~~ */
static int format_write_Newline(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	str = format_write_operator(list, ptr, 1);
	/* times */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_less_(list->format, arg, 1, 0));
	/* colon */
	if (list->colon) {
		return format_abort_va_(list->format, list->colon_pos,
				"The operator cannot use : parameter.", NULL);
	}
	/* atsign */
	if (list->atsign) {
		return format_abort_va_(list->format, list->atsign_pos,
				"The operator cannot use @ parameter.", NULL);
	}
	/* result */
	return Result(ret, str->size);
}


/* ~T */
static int format_write_Tabulate(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	str = format_write_operator(list, ptr, 2);
	format = list->format;
	/* column */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_less_(format, arg, 1, 0));
	/* colinc */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_less_(format, arg, 1, 0));
	/* result */
	return Result(ret, str->size);
}


/* ~T */
static int format_write_GoTo(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	str = format_write_operator(list, ptr, 1);
	/* count */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_less_(list->format, arg, list->atsign? 0: 1, 0));
	/* result */
	return Result(ret, str->size);
}


/* ~? */
static int format_write_Recursive(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;

	str = format_write_operator(list, ptr, 0);
	/* colon */
	if (list->colon) {
		return format_abort_va_(list->format, list->colon_pos,
				"The operator cannot use : parameter.", NULL);
	}
	/* result */
	return Result(ret, str->size);
}


/* ~? */
static int format_write_Indent(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	/* atsign */
	if (list->atsign) {
		return format_abort_va_(list->format, list->atsign_pos,
				"The operator cannot use @ parameter.", NULL);
	}
	/* parameter */
	str = format_write_operator(list, ptr, 1);
	/* indent */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_integer_(list->format, arg, 0));
	/* result */
	return Result(ret, str->size);
}


/* ~( */
static int format_size_Case(struct fmtchar *list, size_t *ret)
{
	struct fmtchar *x;
	size_t size, value;

	/* close parensis */
	if (list->close_colon) {
		return format_abort_va_(list->format, list->position,
				"Cannot use : parameter at the close parensis.", NULL);
	}
	if (list->close_atsign) {
		return format_abort_va_(list->format, list->position,
				"Cannot use @ parameter at the close parensis.", NULL);
	}

	/* argument */
	Return(format_size_index_(list, &size, 0));
	/* body */
	for (x = list->option; x; x = x->next) {
		if (x == NULL) {
			return format_abort_va_(x->format, x->position,
					"There is no close parensis ~~).", NULL);
		}
		if (x->type == FormatType_ClauseSeparator) {
			return format_abort_va_(x->format, x->position,
					"Cannot use ~~; operator in the ~~(...~~).", NULL);
		}
		if (x->character == ')')
			break;
		Return(format_size_(x, &value));
		size += value;
	}
	/* end */
	Return(format_size_End(&value));
	size += value;
	/* result */
	return Result(ret, size);
}

static int format_write_Case(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	struct fmtchar *x;
	size_t size, value;

	str = format_write_operator(list, ptr, 0);
	size = str->size;
	/* body */
	for (x = list->option; x->character != ')'; x = x->next) {
		Return(format_write_(x, ptr + size, &value));
		size += value;
	}
	/* end */
	Return(format_write_End(list, ptr + size, &value));
	size += value;
	/* result */
	*ret = str->size = size;
	return 0;
}


/* ~[, ~] */
static int format_size_Condition_body_(struct fmtchar *list,
		int pcheck, int *rfinal, size_t *rcount, size_t *rsize)
{
	int check, final;
	struct fmtchar *x;
	size_t size, value, count;

	size = 0;
	count = 0;
	final = 0;
	check = 1;
	for (x = list->option; ; x = x->next) {
		if (x == NULL)
			goto error1;
		if (x->character == ']')
			break;
		if (check) {
			count++;
			check = 0;
		}
		if (x->type == FormatType_ClauseSeparator) {
			if (x->atsign)
				goto error2;
			if (x->size)
				goto error3;
			if (final)
				goto error4;
			if (x->colon) {
				if (pcheck)
					goto error5;
				final = 1;
			}
			check = 1;
		}
		Return(format_size_(x, &value));
		size += value;
	}
	if (check)
		count++;
	/* result */
	*rfinal = final;
	*rcount = count;
	*rsize = size;
	return 0;

error1:
	*rfinal = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->position,
			"There is no close parensis ~~].", NULL);

error2:
	*rfinal = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->atsign_pos,
			"Cannot use @ parameter at ~~; operator.", NULL);

error3:
	*rfinal = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->position,
			"Cannot use prefix parameters at ~~; operator.", NULL);

error4:
	*rfinal = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->position,
			"After ~~:; clause must be a last position.", NULL);

error5:
	*rfinal = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->colon_pos,
			"Cannot use ~~:; operator at the ~~:[ or ~~@[ operator.", NULL);
}

static int format_size_Condition(struct fmtchar *list, size_t *ret)
{
	int colon, atsign, any, final;
	size_t size, value, count;

	/* close parensis */
	if (list->close_colon)
		goto error1;
	if (list->close_atsign)
		goto error2;
	/* arguments */
	colon = list->colon;
	atsign = list->atsign;
	any = colon || atsign;
	Return(format_size_index_(list, &size, any? 0: 1));
	if (colon && atsign)
		goto error3;
	/* body */
	Return(format_size_Condition_body_(list, any, &final, &count, &value));
	if (colon && count != 2)
		goto error4;
	if (atsign && count != 1)
		goto error5;
	size += value;
	/* end */
	Return(format_size_End(&value));
	size += value;
	/* count */
	list->option_check = (final != 0);
	list->option_count = count;
	size += sizeoft(size_t) * (count + 1);
	/* result */
	return Result(ret, size);

error1:
	*ret = 0;
	return format_abort_va_(list->format, list->position,
			"Cannot use : parameter at the close parensis.", NULL);

error2:
	*ret = 0;
	return format_abort_va_(list->format, list->position,
			"Cannot use @ parameter at the close parensis.", NULL);

error3:
	*ret = 0;
	return format_abort_va_(list->format, list->position,
			"The parameter don't accept both : and @ parameter (~~:@[).", NULL);

error4:
	*ret = 0;
	return format_abort_va_(list->format, list->position,
			"Count of clauses must be a 2 in ~~:[...~~].", NULL);

error5:
	*ret = 0;
	return format_abort_va_(list->format, list->position,
			"Count of clauses must be a 1 in ~~@[...~~].", NULL);
}

static int format_write_Condition(struct fmtchar *list, byte *ptr, size_t *ret)
{
	int colon, atsign, any, check;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;
	struct fmtchar *x;
	size_t size, value, *array;

	/* error */
	colon = list->colon;
	atsign = list->atsign;
	any = colon || atsign;
	str = format_write_operator(list, ptr, any? 0: 1);
	Check(colon && atsign, ":@[ error");
	/* argument */
	if (any == 0) {
		root = list->root;
		arg = format_write_argument(&root, ptr, 0);
		Return(format_argument_integer_nil_(list->format, arg));
	}
	/* condition array */
	size = str->size;
	array = (size_t *)(ptr + size);
	value = list->option_count;
	*(array++) = value;
	size += sizeoft(size_t) * (value + 1);
	/* body */
	check = 1;
	for (x = list->option; x->character != ']'; x = x->next) {
		if (check) {
			*(array++) = size;
			check = 0;
		}
		if (x->type == FormatType_ClauseSeparator)
			check = 1;
		Return(format_write_(x, ptr + size, &value));
		size += value;
	}
	if (check)
		*(array++) = size;
	/* end */
	Return(format_write_End(list, ptr + size, &value));
	size += value;
	/* result */
	*ret = str->size = size;
	return 0;
}


/* ~{, ~} */
static int format_size_Iteration(struct fmtchar *list, size_t *ret)
{
	int check;
	struct fmtchar *x;
	size_t size, value;

	/* close parensis */
	if (list->close_atsign) {
		return format_abort_va_(list->format, list->position,
				"Cannot use @ parameter at the close parensis.", NULL);
	}
	/* argument */
	Return(format_size_index_(list, &size, 1));
	/* body */
	check = 1;
	for (x = list->option; ; x = x->next) {
		if (x == NULL) {
			return format_abort_va_(x->format, x->position,
					"There is no close parensis ~~}.", NULL);
		}
		if (x->character == '}')
			break;
		if (x->type == FormatType_ClauseSeparator) {
			return format_abort_va_(x->format, x->position,
					"Cannot use ~~; operator in the ~~{...~~}.", NULL);
		}
		Return(format_size_(x, &value));
		size += value;
		check = 0;
	}
	/* end */
	Return(format_size_End(&value));
	size += value;
	/* result */
	list->option_check = check;
	return Result(ret, size);
}

static int format_write_Iteration(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtchar *x;
	struct fmtargs *root;
	size_t size, value;

	str = format_write_operator(list, ptr, 1);
	size = str->size;
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_less_nil_(list->format, arg, 0));
	/* body */
	for (x = list->option; x->character != '}'; x = x->next) {
		Return(format_write_(x, ptr + size, &value));
		size += value;
	}
	/* end */
	Return(format_write_End(list, ptr + size, &value));
	size += value;
	/* result */
	*ret = str->size = size;
	return 0;
}


/* Justification: ~<, ~> */
static int format_size_Justification_body_(struct fmtchar *list,
		int *rfirst, size_t *rcount, size_t *rsize)
{
	int check, first, first_check;
	struct fmtchar *x;
	size_t size, value, count;

	size = 0;
	count = 0;
	first = 0;
	first_check = 1;
	check = 1;
	for (x = list->option; ; x = x->next) {
		if (x == NULL)
			goto error1;
		if (x->character == '>')
			break;
		if (check) {
			count++;
			check = 0;
		}
		if (x->type == FormatType_ClauseSeparator) {
			if (x->atsign)
				goto error2;
			if (x->colon) {
				if (first_check)
					goto error3;
				if (2 < x->size)
					goto error4;
				first = 1;
			}
			else {
				if (x->size)
					goto error5;
			}
			check = 1;
		}
		Return(format_size_(x, &value));
		size += value;
		first_check = 0;
	}
	if (check)
		count++;
	*rfirst = first;
	*rcount = count;
	*rsize = size;
	return 0;

error1:
	*rfirst = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->position,
			"There is no close parensis ~~>.", NULL);

error2:
	*rfirst = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->atsign_pos,
			"The operator cannot use @ parameter.", NULL);

error3:
	*rfirst = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->position,
			"The separator ~~:; must be a first clause.", NULL);

error4:
	*rfirst = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->position,
			"Count of prefix parameters ~~...:; must be less than equal to 2.", NULL);

error5:
	*rfirst = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->position,
			"Cannot use prefix parameters ~~...; in the ~~<...~~>.", NULL);
}

static int format_size_Justification(struct fmtchar *list, size_t *ret)
{
	int first;
	size_t size, value, count;

	/* close parensis */
	if (list->close_colon) {
		return format_abort_va_(list->format, list->position,
				"Cannot use : parameter at the close parensis.", NULL);
	}
	if (list->close_atsign) {
		return format_abort_va_(list->format, list->position,
				"Cannot use @ parameter at the close parensis.", NULL);
	}
	/* arguments */
	Return(format_size_index_(list, &size, 4));
	/* body */
	Return(format_size_Justification_body_(list, &first, &count, &value));
	size += value;
	/* end */
	Return(format_size_End(&value));
	size += value;
	/* count */
	list->option_check = (first != 0);
	list->option_count = count;
	size += sizeoft(size_t) * (count + 1);
	/* result */
	return Result(ret, size);
}

static int format_write_Justification(struct fmtchar *list, byte *ptr, size_t *ret)
{
	int check;
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;
	struct fmtchar *x;
	size_t size, value, *array;

	/* arguments */
	str = format_write_operator(list, ptr, 4);
	format = list->format;
	/* mincol */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_less_(format, arg, 0, 0));
	/* colinc */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_less_(format, arg, 1, 1));
	/* minpad */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_less_(format, arg, 0, 0));
	/* padchar */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_character_(format, arg, ' '));
	/* array */
	size = str->size;
	array = (size_t *)(ptr + size);
	value = list->option_count;
	*(array++) = value;
	size += sizeoft(size_t) * (value + 1);
	/* body */
	check = 1;
	for (x = list->option; x->character != '>'; x = x->next) {
		if (check) {
			*(array++) = size;
			check = 0;
		}
		if (x->type == FormatType_ClauseSeparator)
			check = 1;
		Return(format_write_(x, ptr + size, &value));
		size += value;
	}
	if (check)
		*(array++) = size;
	/* end */
	Return(format_write_End(list, ptr + size, &value));
	size += value;
	/* result */
	*ret = str->size = size;
	return 0;
}


/* Logical Block: ~<, ~:> */
static int format_size_LogicalBlock_count_(
		struct fmtchar *list, size_t *ret, int *atsign)
{
	int check, first, first_check;
	struct fmtchar *x;
	size_t count;

	check = 1;
	first = 0;
	first_check = 1;
	count = 0;
	for (x = list->option; ; x = x->next) {
		if (x == NULL)
			goto error1;
		if (x->character == '>')
			break;
		if (check) {
			if (3 <= count)
				goto error2;
			count++;
			check = 0;
		}
		if (x->type == FormatType_ClauseSeparator) {
			if (x->colon)
				goto error3;
			if (x->size)
				goto error4;
			if (x->atsign) {
				if (first_check == 0)
					goto error5;
				first = 1;
			}
			check = 1;
			first_check = 0;
		}
	}
	if (check)
		count++;
	*ret = count;
	*atsign = first;
	return 0;

error1:
	*ret = 0;
	*atsign = 0;
	return format_abort_va_(x->format, x->position,
			"There is no close parensis ~~>.", NULL);

error2:
	*ret = 0;
	*atsign = 0;
	return format_abort_va_(x->format, x->position,
			"Count of clauses must be less than equal to 3.", NULL);

error3:
	*ret = 0;
	*atsign = 0;
	return format_abort_va_(x->format, x->atsign_pos,
			"The operator cannot use : parameter.", NULL);

error4:
	*ret = 0;
	*atsign = 0;
	return format_abort_va_(x->format, x->position,
			"Cannot use prefix parameters ~~...;.", NULL);

error5:
	*ret = 0;
	*atsign = 0;
	return format_abort_va_(x->format, x->position,
			"The separator ~~:; must be a first clause.", NULL);
}

static int format_size_LogicalBlock_body_(struct fmtchar *list, size_t *ret)
{
	struct fmtchar *x;
	size_t size, value;

	size = 0;
	for (x = list->option; x->character != '>'; x = x->next) {
		Check(x->type == FormatType_ClauseSeparator, "Clause separator error");
		Return(format_size_(x, &value));
		size += value;
	}
	return Result(ret, size);
}

static int format_size_LogicalBlock_output_(struct fmtchar *list, size_t *ret)
{
	struct fmtargs *root;
	size_t count;
#ifdef LISP_DEBUG
	size_t a, b;
#endif

	Check(list->size != 3, "size error");
	/* count */
	root = list->root;
	Check(root->type != fmtargs_index, "argument count error");
	count = root->u.index;
#ifdef LISP_DEBUG
	/* start */
	root = root->next;
	Check(root->type != fmtargs_index, "argument start error");
	a = root->u.index;
	/* end */
	root = root->next;
	Check(root->type != fmtargs_index, "argument end error");
	b = root->u.index;
	/* size */
	Check(b < a, "size1 error");
	Check(count != (b - a), "size2 error");
#endif
	return Result(ret, IdxSize + count * sizeoft(unicode));
}

static int format_end_separator(struct fmtchar *x)
{
	return x->type == FormatType_ClauseSeparator || x->character == '>';
}

static int format_size_LogicalBlock_text_(
		struct fmtchar *x, size_t *ret, struct fmtchar **next)
{
	Check(x == NULL, "Invalid prefix in logical block.");
	if (format_end_separator(x)) {
		*next = x->next;
		*ret = IdxSize;
		return 0;
	}
	if (x->type != FormatType_Output)
		goto error;
	Return(format_size_LogicalBlock_output_(x, ret));
	x = x->next;
	Check(x == NULL, "Invalid prefix in logical block.");
	if (format_end_separator(x)) {
		*next = x->next;
		return 0;
	}
	goto error;

error:
	*next = NULL;
	*ret = 0;
	return format_abort_va_(x->format, x->position,
			"Cannot use format directive in prefix/suffix form.", NULL);
}

static int format_size_LogicalBlock_prefix_(struct fmtchar *list, size_t *ret)
{
	struct fmtchar *x;
	size_t size, value;

	/* prefix */
	size = 0;
	x = list->option;
	Return(format_size_LogicalBlock_text_(x, &value, &x));
	size += value;
	/* body */
	for (;;) {
		if (x == NULL) {
			return format_abort_va_(x->format, x->position,
					"There is no clause separator ~~;.", NULL);
		}
		if (x->type == FormatType_ClauseSeparator) {
			x = x->next;
			break;
		}
		if (x->character == '>') {
			x = NULL;
			break;
		}
		Return(format_size_(x, &value));
		size += value;
		x = x->next;
	}
	/* suffix */
	if (x) {
		Return(format_size_LogicalBlock_text_(x, &value, &x));
		Check(x, "Invalid suffix in logical block.");
		size += value;
	}
	/* result */
	return Result(ret, size);
}

static int format_size_LogicalBlock(struct fmtchar *list, size_t *ret)
{
	int atsign;
	size_t size, value, count;

	/* close parensis */
	if (! list->close_colon) {
		return format_abort_va_(list->format, list->position,
				"Invalid close parensis ~~> in the logical-block.", NULL);
	}
	/* arguments */
	Return(format_size_index_(list, &size, 0));
	/* body */
	Return(format_size_LogicalBlock_count_(list, &count, &atsign));
	if (count <= 1) {
		Return(format_size_LogicalBlock_body_(list, &value));
	}
	else {
		Return(format_size_LogicalBlock_prefix_(list, &value));
	}
	size += value;
	/* end */
	Return(format_size_End(&value));
	size += value;
	/* count */
	list->option_check = (atsign != 0);
	list->option_count = count;
	list->prefix = (2 <= count);
	list->suffix = (3 <= count);
	/* result */
	return Result(ret, size);
}

static int format_write_LogicalBlock_output_(struct fmtchar *x, byte *ptr, size_t *ret)
{
	addr pos;
	struct fmtargs *root;
	unicode *data, u;
	size_t count, a, b, i;

	Check(x->size != 3, "size error");
	/* count */
	root = x->root;
	Check(root->type != fmtargs_index, "argument count error");
	count = root->u.index;
	/* start */
	root = root->next;
	Check(root->type != fmtargs_index, "argument start error");
	a = root->u.index;
	/* end */
	root = root->next;
	Check(root->type != fmtargs_index, "argument end error");
	b = root->u.index;
	/* write */
	Check(b < a, "size1 error");
	Check(count != (b - a), "size2 error");
	*(size_t *)ptr = count;
	data = (unicode *)(ptr + IdxSize);
	pos = x->format;
	for (i = 0; a < b; a++) {
		Return(string_getc_(pos, a, &u));
		data[i++] = u;
	}
	/* size */
	return Result(ret, IdxSize + count * sizeoft(unicode));
}

static int format_write_LogicalBlock_prefix_(struct fmtchar *x, byte *ptr, size_t *ret)
{
	if (format_end_separator(x)) {
		*(size_t *)ptr = 0;
		*ret = IdxSize;
	}
	else {
		Check(x->type != FormatType_Output, "type error");
		Return(format_write_LogicalBlock_output_(x, ptr, ret));
	}

	return 0;
}

static void format_write_LogicalBlock_next(
		int count, struct fmtchar *x, struct fmtchar **ret)
{
	int i;

	for (i = 0; i < count; i++) {
		for (;;) {
			Check(x == NULL, "fmtchar error");
			if (x->type == FormatType_ClauseSeparator) {
				x = x->next;
				break;
			}
			x = x->next;
		}
	}
	*ret = x;
}

static int format_write_LogicalBlock_suffix_(struct fmtchar *x, byte *ptr, size_t *ret)
{
	format_write_LogicalBlock_next(2, x, &x);
	return format_write_LogicalBlock_prefix_(x, ptr, ret);
}

static int format_write_LogicalBlock(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	struct fmtchar *x;
	size_t size, value;

	/* arguments */
	str = format_write_operator(list, ptr, 0);
	/* prefix */
	x = list->option;
	size = str->size;
	if (str->prefix) {
		Return(format_write_LogicalBlock_prefix_(x, ptr + size, &value));
		size += value;
	}
	/* suffix */
	if (str->suffix) {
		Return(format_write_LogicalBlock_suffix_(x, ptr + size, &value));
		size += value;
	}
	/* body */
	if (str->prefix)
		format_write_LogicalBlock_next(1, x, &x);
	for (; ! format_end_separator(x); x = x->next) {
		Return(format_write_(x, ptr + size, &value));
		size += value;
	}
	/* end */
	Return(format_write_End(list, ptr + size, &value));
	size += value;
	/* result */
	*ret = str->size = size;
	return 0;
}


/* ~^ */
static int format_write_EscapeUpward(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	/* atsign */
	if (list->atsign) {
		return format_abort_va_(list->format, list->atsign_pos,
				"The operator cannot use @ parameter.", NULL);
	}
	/* parameter */
	str = format_write_operator(list, ptr, 3);
	format = list->format;
	/* v1 */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_integer_nil_(format, arg));
	/* v2 */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_integer_nil_(format, arg));
	/* v3 */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_integer_nil_(format, arg));
	/* result */
	return Result(ret, str->size);
}


/* ~; */
static int format_write_ClauseSeparator(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	/* parameters */
	str = format_write_operator(list, ptr, 2);
	format = list->format;
	/* size */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_integer_nil_(format, arg));
	/* width */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_integer_nil_(format, arg));
	/* result */
	return Result(ret, str->size);
}


/* ~/ */
static int format_size_CallFunction(struct fmtchar *list, size_t *ret)
{
	addr package, name;
	size_t size, value;

	/* argument */
	Return(format_size_index_(list, &size, list->size));
	Check(list->intern == NULL, "intern error");
	GetCons(list->intern, &package, &name);
	/* package */
	size += IdxSize;
	strvect_length(package, &value);
	size += value * sizeoft(unicode);
	/* name */
	size += IdxSize;
	strvect_length(name, &value);
	size += value * sizeoft(unicode);
	/* result */
	return Result(ret, size);
}

static int format_write_CallFunction_string_(
		struct format_operator *str, byte *ptr, addr pos)
{
	size_t size, i;
	unicode *data, u;

	/* size_t */
	strvect_length(pos, &size);
	*((size_t *)(ptr + str->size)) = size;
	str->size += IdxSize;
	/* unicode */
	data = (unicode *)(ptr + str->size);
	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &u);
		data[i] = u;
	}
	str->size += size * sizeoft(unicode);

	return 0;
}

static int format_write_CallFunction(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr package, name;
	struct format_operator *str;
	struct fmtargs *x, *root;
	size_t i;

	/* parameters */
	str = format_write_operator(list, ptr, list->size);
	i = 0;
	for (x = list->root; x; x = x->next) {
		root = x;
		format_write_argument(&root, ptr, i++);
	}
	/* body */
	GetCons(list->intern, &package, &name);
	Return(format_write_CallFunction_string_(str, ptr, package));
	Return(format_write_CallFunction_string_(str, ptr, name));
	/* result */
	return Result(ret, str->size);
}


/*
 *  format write call
 */
typedef int (*format_size_call)(struct fmtchar *, size_t *);
typedef int (*format_write_call)(struct fmtchar *, byte *, size_t *);
static format_size_call FormatSize[FormatType_size];
static format_write_call FormatWrite[FormatType_size];

static int format_size_(struct fmtchar *list, size_t *ret)
{
	format_size_call call;

	call = FormatSize[list->type];
	Check(call == NULL, "size call error");
	return (*call)(list, ret);
}

static int format_write_(struct fmtchar *list, byte *ptr, size_t *ret)
{
	format_write_call call;

	call = FormatWrite[list->type];
	Check(call == NULL, "write call error");
	return (*call)(list, ptr, ret);
}

static int format_size_list_(struct fmtchar *list, size_t *ret)
{
	size_t size, value;

	/* Format */
	Return(format_size_Format(list, &size));
	/* body */
	for (; list; list = list->next) {
		if (list->type == FormatType_ClauseSeparator) {
			*ret = 0;
			return format_abort_va_(list->format, list->position,
					"Cannot use ~~; operator.", NULL);
		}
		Return(format_size_(list, &value));
		size += value;
	}
	/* End */
	Return(format_size_End(&value));
	size += value;
	/* result */
	return Result(ret, size);
}

static int format_write_list_(struct fmtchar *list, byte *ptr, size_t *ret)
{
	size_t size, value;

	/* Format */
	Return(format_write_Format(list, ptr, &size));
	/* body */
	for (; list; list = list->next) {
		Return(format_write_(list, ptr + size, &value));
		size += value;
	}
	/* End */
	Return(format_write_End(list, ptr + size, &value));
	size += value;
	/* result */
	return Result(ret, size);
}


/*
 *  format object
 */
int formatp(addr pos)
{
	return GetType(pos) == LISPTYPE_FORMAT;
}

static void format_alloc(LocalRoot local, addr *ret, size_t size)
{
	alloc_body4(local, ret, LISPTYPE_FORMAT, size);
}

void *format_pointer(addr pos)
{
	CheckType(pos, LISPTYPE_FORMAT);
	return (void *)PtrBodyB4(pos);
}

size_t format_bytesize(size_t count)
{
	return FormatByteSize(count);
}

static int format_parse_(LocalRoot local, addr *ret, addr format, int localp)
{
	addr pos;
	byte *ptr;
	struct fmtchar *list;
	size_t size, check;

	/* parse */
	Return(fmtchar_make_(local, format, &list));
	Return(format_size_list_(list, &size));
	/* allocation */
	format_alloc(localp? local: NULL, &pos, size);
	ptr = (byte *)format_pointer(pos);
#ifdef LISP_DEBUG
	aamemory(ptr, size);
#endif
	Return(format_write_list_(list, ptr, &check));
	Check(size != check, "size error");
	return Result(ret, pos);
}

int format_parse_local_(LocalRoot local, addr *ret, addr format)
{
	CheckLocal(local);
	CheckType(format, LISPTYPE_STRING);
	return format_parse_(local, ret, format, 1);
}

int format_parse_heap_(LocalRoot local, addr *ret, addr format)
{
	LocalStack stack;

	CheckLocal(local);
	CheckType(format, LISPTYPE_STRING);
	push_local(local, &stack);
	Return(format_parse_(local, ret, format, 0));
	rollback_local(local, stack);

	return 0;
}

int format_string_alloc_(LocalRoot local, addr *ret, addr format)
{
	byte *body;
	struct format_argument *arg;
	unicode *u;
	addr pos;
	size_t size, i;
#ifdef LISP_DEBUG
	struct format_operator *str;
#endif

	CheckType(format, LISPTYPE_FORMAT);
	/* header */
	body = (byte *)format_pointer(format);
#ifdef LISP_DEBUG
	str = (struct format_operator *)body;
	Check(str->type != FormatType_Format, "Invalid format object.");
#endif
	arg = (struct format_argument *)format_write_body(body, 0);
	Check(arg->type != fmtargs_index, "Invalid format arguemnt.");
	size = arg->u.index;
	/* body */
	strvect_alloc(local, &pos, size);
	u = (unicode *)format_write_body(body, 1);
	for (i = 0; i < size; i++) {
		Return(strvect_setc_(pos, i, u[i]));
	}

	return Result(ret, pos);
}

int format_string_local_(LocalRoot local, addr *ret, addr format)
{
	CheckLocal(local);
	return format_string_alloc_(local, ret, format);
}

int format_string_heap_(addr *ret, addr format)
{
	return format_string_alloc_(NULL, ret, format);
}


/*
 *  table
 */
#define SetFormatSize(x,y) (FormatSize[FormatType_##x] = format_size_##y)
#define SetFormatWrite(x,y) (FormatWrite[FormatType_##x] = format_write_##y)
#define SetFormatCharacter(x,y) (FormatCharacter[(int)(x)] = (y))

void init_format_parse(void)
{
	cleartype(FormatCharacter);
	SetFormatCharacter(0, FormatType_Output);
	SetFormatCharacter('A', FormatType_Aesthetic);
	SetFormatCharacter('S', FormatType_Standard);
	SetFormatCharacter('B', FormatType_Binary);
	SetFormatCharacter('O', FormatType_Octal);
	SetFormatCharacter('D', FormatType_Decimal);
	SetFormatCharacter('X', FormatType_Hexadecimal);
	SetFormatCharacter('R', FormatType_Radix);
	SetFormatCharacter('P', FormatType_Plural);
	SetFormatCharacter('C', FormatType_Character);
	SetFormatCharacter('F', FormatType_Fixed);
	SetFormatCharacter('E', FormatType_Exponential);
	SetFormatCharacter('G', FormatType_General);
	SetFormatCharacter('$', FormatType_Monetary);
	SetFormatCharacter('%', FormatType_Newline);
	SetFormatCharacter('&', FormatType_FreshLine);
	SetFormatCharacter('|', FormatType_Page);
	SetFormatCharacter('~', FormatType_Tilde);
	SetFormatCharacter('\n', FormatType_IgnoredNewline);
	SetFormatCharacter('T', FormatType_Tabulate);
	SetFormatCharacter('*', FormatType_GoTo);
	SetFormatCharacter('?', FormatType_Recursive);
	SetFormatCharacter('_', FormatType_ConditionalNewline);
	SetFormatCharacter('W', FormatType_Write);
	SetFormatCharacter('I', FormatType_Indent);
	SetFormatCharacter('(', FormatType_Case);
	SetFormatCharacter('[', FormatType_Condition);
	SetFormatCharacter('{', FormatType_Iteration);
	SetFormatCharacter('<', FormatType_Justification);
	SetFormatCharacter('^', FormatType_EscapeUpward);
	SetFormatCharacter(';', FormatType_ClauseSeparator);
	SetFormatCharacter('/', FormatType_CallFunction);

	cleartype(FormatSize);
	SetFormatSize(Output,              Output);           /* text */
	SetFormatSize(Aesthetic,           size4);            /* A */
	SetFormatSize(Standard,            size4);            /* S */
	SetFormatSize(Binary,              size4);            /* B */
	SetFormatSize(Octal,               size4);            /* O */
	SetFormatSize(Decimal,             size4);            /* D */
	SetFormatSize(Hexadecimal,         size4);            /* X */
	SetFormatSize(Radix,               size5);            /* R */
	SetFormatSize(RadixText,           size0);            /* R */
	SetFormatSize(Plural,              size0);            /* P */
	SetFormatSize(Character,           size0);            /* C */
	SetFormatSize(Fixed,               size5);            /* F */
	SetFormatSize(Exponential,         size7);            /* E */
	SetFormatSize(General,             size7);            /* G */
	SetFormatSize(Monetary,            size4);            /* $ */
	SetFormatSize(Newline,             size1);            /* % */
	SetFormatSize(FreshLine,           size1);            /* & */
	SetFormatSize(Page,                size1);            /* | */
	SetFormatSize(Tilde,               size1);            /* ~ */
	SetFormatSize(IgnoredNewline,      size0);            /* \n */
	SetFormatSize(Tabulate,            size2);            /* T */
	SetFormatSize(GoTo,                size1);            /* * */
	SetFormatSize(Recursive,           size0);            /* ? */
	SetFormatSize(ConditionalNewline,  size0);            /* _ */
	SetFormatSize(Write,               size0);            /* W */
	SetFormatSize(Indent,              size1);            /* I */
	SetFormatSize(Case,                Case);             /* () */
	SetFormatSize(Condition,           Condition);        /* [] */
	SetFormatSize(Iteration,           Iteration);        /* {} */
	SetFormatSize(Justification,       Justification);    /* <> */
	SetFormatSize(LogicalBlock,        LogicalBlock);     /* <> */
	SetFormatSize(EscapeUpward,        size3);            /* ^ */
	SetFormatSize(ClauseSeparator,     size2);            /* ; */
	SetFormatSize(CallFunction,        CallFunction);     /* / */

	cleartype(FormatWrite);
	SetFormatWrite(Output,              Output);           /* text */
	SetFormatWrite(Aesthetic,           Aesthetic);        /* A */
	SetFormatWrite(Standard,            Aesthetic);        /* S */
	SetFormatWrite(Binary,              Binary);           /* B */
	SetFormatWrite(Octal,               Binary);           /* O */
	SetFormatWrite(Decimal,             Binary);           /* D */
	SetFormatWrite(Hexadecimal,         Binary);           /* X */
	SetFormatWrite(Radix,               Radix);            /* R */
	SetFormatWrite(RadixText,           Empty);            /* R */
	SetFormatWrite(Plural,              Empty);            /* P */
	SetFormatWrite(Character,           Empty);            /* C */
	SetFormatWrite(Fixed,               Fixed);            /* F */
	SetFormatWrite(Exponential,         Exponent);         /* E */
	SetFormatWrite(General,             General);          /* G */
	SetFormatWrite(Monetary,            Monetary);         /* $ */
	SetFormatWrite(Newline,             Newline);          /* % */
	SetFormatWrite(FreshLine,           Newline);          /* & */
	SetFormatWrite(Page,                Newline);          /* | */
	SetFormatWrite(Tilde,               Newline);          /* ~ */
	SetFormatWrite(IgnoredNewline,      Empty);            /* \n */
	SetFormatWrite(Tabulate,            Tabulate);         /* T */
	SetFormatWrite(GoTo,                GoTo);             /* * */
	SetFormatWrite(Recursive,           Recursive);        /* ? */
	SetFormatWrite(ConditionalNewline,  Empty);            /* _ */
	SetFormatWrite(Write,               Empty);            /* W */
	SetFormatWrite(Indent,              Indent);           /* I */
	SetFormatWrite(Case,                Case);             /* () */
	SetFormatWrite(Condition,           Condition);        /* [] */
	SetFormatWrite(Iteration,           Iteration);        /* {} */
	SetFormatWrite(Justification,       Justification);    /* <> */
	SetFormatWrite(LogicalBlock,        LogicalBlock);     /* <> */
	SetFormatWrite(EscapeUpward,        EscapeUpward);     /* ^ */
	SetFormatWrite(ClauseSeparator,     ClauseSeparator);  /* ; */
	SetFormatWrite(CallFunction,        CallFunction);     /* / */
}


/************************************************************
 *  format_print.c
 ************************************************************/

/*
 *  format execute
 */
int fmtprint_abort_(fmtprint print, size_t index, const char *str, ...)
{
	va_list args;

	va_start(args, str);
	Return(format_abort_(print->format, index, str, args));
	va_end(args);

	return 0;
}

int fmtprop_abort_(fmtprint print,
		struct format_operator *fmt, const char *str, ...)
{
	va_list args;

	va_start(args, str);
	Return(format_abort_(print->format, fmt->position, str, args));
	va_end(args);

	return 0;
}

void fmtprint_make(fmtprint print, Execute ptr, addr stream, addr format)
{
	clearpoint(print);
	print->loop = 1;
	print->first = 1;
	print->conversion = fmtcase_normal;
	print->ptr = ptr;
	print->local = ptr->local;
	print->stream = stream;
	print->format = format;
}

void fmtprint_copy(fmtprint print, fmtprint src)
{
	fmtprint_make(print, src->ptr, src->stream, src->format);
	print->string = src->string;
}

int fmtprint_make_string_(fmtprint print, addr *ret, addr *backup)
{
	addr backup_stream, src, stream;

	backup_stream = print->string;
	src = print->stream;
	open_output_string_stream(&stream, 0);
	gchold_push_local(print->local, stream);
	Return(copyleft_stream_(stream, src));
	Return(copy_termsize_string_stream_(stream, src));
	if (pretty_stream_p(src))
		set_pretty_output_string_stream(stream);
	print->string = stream;
	*ret = stream;
	*backup = backup_stream;

	return 0;
}

int fmtprint_stream_(fmtprint print, addr *ret)
{
	addr stream;

	stream = print->string;
	Check(! output_string_stream_p(stream), "string-stream error");
	clear_output_string_stream(stream);
	Return(copyleft_stream_(stream, print->stream));
	return Result(ret, stream);
}

int fmtprint_stream_output_(fmtprint print)
{
	addr pos, stream;

	stream = print->string;
	Check(! output_string_stream_p(stream), "string-stream error");
	Return(string_stream_local_(print->local, stream, &pos));
	clear_output_string_stream(stream);
	return fmtprint_string_(print, pos);
}

struct format_operator *fmtprint_operator(fmtprint print)
{
	return (struct format_operator *)
		(print->now + (byte *)format_pointer(print->format));
}


/*
 *  putc
 */
static int fmtprint_char_(fmtprint print, unicode u)
{
	addr stream;

	stream = print->stream;
	if (print->pretty == 0)
		goto output;
	if (print->fill == 0)
		goto output;

	if (u == ' ') {
		print->fill_white = 1;
		goto output;
	}
	if (print->fill_white && print->fill_ignore == 0) {
		Return(pprint_newline_print_(print->ptr, pprint_newline_fill, stream));
	}
	print->fill_white = 0;
	print->fill_ignore = 0;

output:
	return write_char_stream_(stream, u);
}

int fmtprint_putc_(fmtprint print, unicode u)
{
	switch (print->conversion) {
		case fmtcase_upcase:
			u = toUpperUnicode(u);
			break;

		case fmtcase_downcase:
			u = toLowerUnicode(u);
			break;

		case fmtcase_capitalize_all:
			if (print->word == 0 && isAlphanumeric(u))
				u = toUpperUnicode(u);
			else
				u = toLowerUnicode(u);
			break;

		case fmtcase_capitalize_first:
			if (print->word == 0 && print->first && isAlphanumeric(u)) {
				u = toUpperUnicode(u);
				print->first = 0;
			}
			else
				u = toLowerUnicode(u);
			break;

		case fmtcase_normal:
		default:
			break;
	}

	print->word = isAlphanumeric(u);
	return fmtprint_char_(print, u);
}

int fmtprint_putc_times_(fmtprint print, unicode c, size_t size)
{
	for (; size; size--) {
		Return(fmtprint_putc_(print, c));
	}

	return 0;
}

int fmtprint_string_(fmtprint print, addr string)
{
	unicode u;
	size_t size, i;

	string_length(string, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(string, i, &u));
		Return(fmtprint_putc_(print, u));
	}

	return 0;
}


/*
 *  pop
 */
static int fmtprint_pop_error_(fmtprint print, struct format_operator *str,
		addr list, addr *car, addr *cdr, size_t n)
{
	addr pos;
	size_t i;

	pos = Nil;
	for (i = 0; i < n; i++) {
		if (! consp(list))
			return fmtprop_abort_(print, str, "Too few format arguments.", NULL);
		Return_getcons(list, &pos, &list);
	}
	if (car)
		*car = pos;
	*cdr = list;

	return 0;
}

static int fmtprint_pop1_(fmtprint print, struct format_operator *str, addr *ret)
{
	struct fmtstack *ptr;

	ptr = print->rest;
	Return(fmtprint_pop_error_(print, str, ptr->front, ret, &(ptr->front), 1));
	ptr->index++;

	return 0;
}

static int fmtprint_pop2_(fmtprint print, struct format_operator *str, addr *ret)
{
	addr stream;

	stream = print->stream;
	Check(! pretty_stream_p(stream), "pretty stream error");
	return pprint_pop_common_(print->ptr, stream, ret);
}

int fmtprint_pop_(fmtprint print, struct format_operator *str, addr *ret)
{
	addr pos;

	if (print->pretty == 0) {
		return fmtprint_pop1_(print, str, ret);
	}
	else {
		Return(fmtprint_pop2_(print, str, ret));
		return fmtprint_pop1_(print, str, &pos);
	}
}


/*
 *  peek
 */
int fmtprint_peek_(fmtprint print, struct format_operator *str, addr *ret)
{
	struct fmtstack *ptr;

	ptr = print->rest;
	if (ptr->front == Nil)
		return fmtprop_abort_(print, str, "Too few format arguments.", NULL);
	Return_getcar(ptr->front, ret);

	return 0;
}


/*
 *  forward
 */
static int fmtprint_forward1_(fmtprint print, struct format_operator *str, size_t n)
{
	struct fmtstack *ptr;

	ptr = print->rest;
	Return(fmtprint_pop_error_(print, str, ptr->front, NULL, &(ptr->front), n));
	ptr->index += n;

	return 0;
}

static int fmtprint_forward2_(fmtprint print, struct format_operator *str, size_t n)
{
	addr stream, list;

	stream = print->stream;
	Check(! pretty_stream_p(stream), "pretty stream error");

	Return(root_pretty_stream_(stream, &list));
	Return(fmtprint_pop_error_(print, str, list, NULL, &list, n));
	Return(setroot_pretty_stream_(stream, list));

	return 0;
}

int fmtprint_forward_(fmtprint print, struct format_operator *str, size_t n)
{
	if (n == 0)
		return 0;
	Return(fmtprint_forward1_(print, str, n));
	if (print->pretty) {
		Return(fmtprint_forward2_(print, str, n));
	}

	return 0;
}


/*
 *  rollback
 */
static int fmtprint_rollback1_(fmtprint print, struct format_operator *str, size_t n)
{
	struct fmtstack *rest;
	size_t size;

	rest = print->rest;
	size = rest->index;
	if (size < n)
		return fmtprop_abort_(print, str, "Cannot rollback ~~:*.", NULL);
	rest->front = rest->root;
	rest->index = 0;
	size -= n;
	Return(fmtprint_pop_error_(print, str, rest->front, NULL, &(rest->front), size));
	rest->index = size;

	return 0;
}

static int fmtprint_rollback2_(fmtprint print, struct format_operator *str, size_t n)
{
	addr stream;

	stream = print->stream;
	Check(! pretty_stream_p(stream), "pretty stream error");
	return setroot_pretty_stream_(stream, print->rest->front);
}

int fmtprint_rollback_(fmtprint print, struct format_operator *str, size_t n)
{
	if (n == 0)
		return 0;
	Return(fmtprint_rollback1_(print, str, n));
	if (print->pretty) {
		Return(fmtprint_rollback2_(print, str, n));
	}

	return 0;
}


/*
 *  absolute
 */
int fmtprint_absolute_(fmtprint print, struct format_operator *str, size_t n)
{
	size_t now;

	now = print->rest->index;
	if (now < n)
		return fmtprint_forward_(print, str, n - now);
	else
		return fmtprint_rollback_(print, str, now - n);
}


/*
 *  clear
 */
static void fmtprint_clear1(fmtprint print)
{
	struct fmtstack *rest;

	rest = print->rest;
	rest->root = Nil;
	rest->front = Nil;
	rest->index = 0;
}

static int fmtprint_clear2_(fmtprint print)
{
	addr stream;

	stream = print->stream;
	Check(! pretty_stream_p(stream), "pretty stream error");
	return setroot_pretty_stream_(stream, Nil);
}

int fmtprint_clear_(fmtprint print)
{
	fmtprint_clear1(print);
	if (print->pretty) {
		Return(fmtprint_clear2_(print));
	}

	return 0;
}


/************************************************************
 *  format_radix.c
 ************************************************************/
/*
 *  format radix ~R
 */

#define ENGLISH_RADIX_MODE1	","
#define ENGLISH_RADIX_MODE2	"and"


/*
 *  The Conway-Wechsler System.
 *    http://www.mrob.com/pub/math/largenum.html#conway-wechsler
 *    Large Numbers, Robert P. Munafo.
 */
struct english_struct {
	LocalRoot local;
	addr root, pos, string;
	int cardinal;
	fixnum index;
};
typedef struct english_struct *english;

static const char *name_standard_char(fixnum index, int llion)
{
	static const char *table[][2] = {
		{ "ni",     "thousand"    },
		{ "mi",     "million"     },
		{ "bi",     "billion"     },
		{ "tri",    "trillion"    },
		{ "quadri", "quadrillion" },
		{ "quinti", "quintillion" },
		{ "sexti",  "sextillion"  },
		{ "septi",  "septillion"  },
		{ "octi",   "octillion"   },
		{ "noni",   "nonillion"   }
	};
	Check(! (0 <= index && index <= 9), "index error");
	return table[index][llion != 0];
}

static void english_string(english input, const char *value)
{
	addr pos;
	LocalRoot local;

	local = input->local;
	strvect_char_local(local, &pos, value);
	cons_local(local, &(input->string), pos, input->string);
}

static void english_char(english input, char c)
{
	char value[2] = {c, 0};
	english_string(input, value);
}

static void name_table_1(const char **a, const char **b, fixed v)
{
	static const char *table[][2] = {
		{ NULL,       ""   },  /* 0 */
		{ "un",       ""   },  /* 1 */
		{ "duo",      ""   },  /* 2 */
		{ "tre",      "*"  },  /* 3 */
		{ "quattuor", ""   },  /* 4 */
		{ "quin",     ""   },  /* 5 */
		{ "se",       "sx" },  /* 6 */
		{ "septe",    "mn" },  /* 7 */
		{ "octo",     ""   },  /* 8 */
		{ "nove",     "mn" }   /* 9 */
	};
	Check(10 <= v, "index error");
	*a = table[v][0];
	*b = table[v][1];
}

static void name_table_10(const char **a, const char **b, fixed v)
{
	static const char *table[][2] = {
		{ NULL,           ""   },  /* 0 */
		{ "deci",         "n"  },  /* 1 */
		{ "viginti",      "ms" },  /* 2 */
		{ "triginta",     "ns" },  /* 3 */
		{ "quadraginta",  "ns" },  /* 4 */
		{ "quinquaginta", "ns" },  /* 5 */
		{ "sexaginta",    "n"  },  /* 6 */
		{ "septuaginta",  "n"  },  /* 7 */
		{ "octoginta",    "mx" },  /* 8 */
		{ "nonaginta",    ""   }   /* 9 */
	};
	Check(10 <= v, "index error");
	*a = table[v][0];
	*b = table[v][1];
}

static void name_table_100(const char **a, const char **b, fixed v)
{
	static const char *table[][2] = {
		{ NULL,           ""   },  /* 0 */
		{ "centi",        "nx" },  /* 1 */
		{ "ducenti",      "n"  },  /* 2 */
		{ "trecenti",     "ns" },  /* 3 */
		{ "quadringenti", "ns" },  /* 4 */
		{ "quingenti",    "ns" },  /* 5 */
		{ "sescenti",     "n"  },  /* 6 */
		{ "septingenti",  "n"  },  /* 7 */
		{ "octingenti",   "mx" },  /* 8 */
		{ "nongenti",     ""   }   /* 9 */
	};
	Check(10 <= v, "index error");
	*a = table[v][0];
	*b = table[v][1];
}

static void name_concat(english input,
		const char *a1, const char *b1,
		const char *a2, const char *b2)
{
	char c;

	english_string(input, a1);
	if (b1[0] == '*') {
		if (strchr(b2, 's') || strchr(b2, 'x'))
			english_char(input, 's');
	}
	else {
		while ((c = *(b1++)) != 0) {
			if (strchr(b2, c))
				english_char(input, c);
		}
	}
	english_string(input, a2);
}

static void number_name_front(english input, fixnum quot)
{
	int n1, n2, n3;
	fixnum s1, s10, s100;
	const char *a1, *b1;
	const char *a2, *b2;
	const char *a3, *b3;

	s100 = quot;
	s1 = s100 % 10; s100 /= 10;
	s10 = s100 % 10; s100 /= 10;
	s100 %= 10;
	name_table_1(&a1, &b1, s1);
	name_table_10(&a2, &b2, s10);
	name_table_100(&a3, &b3, s100);
	n1 = (a1 == NULL);
	n2 = (a2 == NULL);
	n3 = (a3 == NULL);
	if (n1 && n2) {
		english_string(input, a3);
		return;
	}
	if (n1 && n3) {
		english_string(input, a2);
		return;
	}
	if (n1) {
		english_string(input, a2);
		english_string(input, a3);
		return;
	}
	if (n2) {
		name_concat(input, a1, b1, a3, b3);
		return;
	}
	if (n3) {
		name_concat(input, a1, b1, a2, b2);
		return;
	}
	else {
		name_concat(input, a1, b1, a2, b2);
		english_string(input, a3);
		return;
	}
}

static void number_name_standard(english input, fixnum quot, int llion)
{
	const char *ptr = name_standard_char(quot, llion);
	english_string(input, ptr);
}

static void number_name_1000(english input, fixnum quot)
{
	if (quot < 10) {
		number_name_standard(input, quot, 0);
	}
	else {
		number_name_front(input, quot);
	}
}

static void number_name_vowel(english input)
{
	addr pos;
	size_t size;
	unicode u;

	GetCar(input->string, &pos);
	strvect_length(pos, &size);
	Check(size <= 1, "size error");
	strvect_getc(pos, size - 1, &u);
	if (u == 'a' || u == 'e' || u == 'i' || u == 'o' || u == 'u') {
		SetStringSize(pos, size - 1);
	}
}

static void number_name_extend(english input, fixnum quot)
{
	number_name_1000(input, quot);
	number_name_vowel(input);
	english_string(input, "illi");
}

static void number_name_recursive(english input, fixnum quot)
{
	fixnum high, low;

	high = quot / 1000;
	low = quot % 1000;
	/* high */
	if (high != 0) {
		number_name_recursive(input, high);
	}
	/* low */
	number_name_extend(input, low);
}

static void number_name_index(english input, fixnum quot)
{
	Check(quot < 0, "index error");
	if (quot < 10) {
		number_name_standard(input, quot, 1);
	}
	else {
		number_name_recursive(input, quot);
		english_string(input, "on");
	}
}

static const char *radix_table_20(fixed value, int cardinal)
{
	static const char *table[][2] = {
		{ "zero",      "zeroth"      },
		{ "one",       "first"       },
		{ "two",       "second"      },
		{ "three",     "third"       },
		{ "four",      "fourth"      },
		{ "five",      "fifth"       },
		{ "six",       "sixth"       },
		{ "seven",     "seventh"     },
		{ "eight",     "eighth"      },
		{ "nine",      "ninth"       },
		{ "ten",       "tenth"       },
		{ "eleven",    "eleventh"    },
		{ "twelve",    "twelfth"     },
		{ "thirteen",  "thirteenth"  },
		{ "fourteen",  "fourteenth"  },
		{ "fifteen",   "fifteenth"   },
		{ "sixteen",   "sixteenth"   },
		{ "seventeen", "seventeenth" },
		{ "eighteen",  "eighteenth"  },
		{ "nineteen",  "nineteenth"  }
	};
	Check(! (0 <= value && value <= 19), "value error");
	return table[value][cardinal == 0];
}

static const char *radix_table_100(fixed value, int cardinal)
{
	static const char *table[][2] = {
		{ NULL,      NULL         },
		{ NULL,      NULL         },
		{ "twenty",  "twentieth"  },
		{ "thirty",  "thirtieth"  },
		{ "forty",   "fortieth"   },
		{ "fifty",   "fiftieth"   },
		{ "sixty",   "sixtieth"   },
		{ "seventy", "seventieth" },
		{ "eighty",  "eightieth"  },
		{ "ninety",  "ninetieth"  }
	};
	Check(! (2 <= value && value <= 9), "value error");
	return table[value][cardinal == 0];
}

static void push_radix(english input, addr pos)
{
	cons_local(input->local, &(input->root), pos, input->root);
	input->cardinal = 1;
}

static void push_radix_char(english input, const char *ptr)
{
	addr pos;
	strvect_char_local(input->local, &pos, ptr);
	push_radix(input, pos);
}

static void push_radix_list(english input, addr pos)
{
	nreverse(&pos, pos);
	push_radix(input, pos);
}

static void push_radix_20(english input, fixed value)
{
	const char *ptr;

	ptr = radix_table_20(value, input->cardinal);
	push_radix_char(input, ptr);
}

static void push_radix_100(english input, fixed c)
{
	const char *ptr;
	fixed a, b;

	Check(100 <= c, "value error");
	if (c < 20) {
		push_radix_20(input, c);
		return;
	}

	a = c / 10;
	b = c % 10;
	if (a && b) {
		input->string = Nil;
		ptr = radix_table_100(a, 1); /* force cardinal */
		english_string(input, ptr);
		english_string(input, "-");
		ptr = radix_table_20(b, input->cardinal);
		english_string(input, ptr);
		push_radix_list(input, input->string);
		return;
	}
	if (a) {
		ptr = radix_table_100(a, input->cardinal);
		push_radix_char(input, ptr);
		return;
	}
	else {
		push_radix_20(input, b);
		return;
	}
}

static void push_radix_hundred(english input)
{
	push_radix_char(input, input->cardinal? "hundred": "hundredth");
}

static void number_name_cardinal(english input)
{
	input->string = Nil;
	number_name_index(input, input->index);
	if (! input->cardinal) {
		english_string(input, "th");
	}
#ifdef ENGLISH_RADIX_MODE1
	if (input->root != Nil) {
		english_string(input, ENGLISH_RADIX_MODE1);
	}
#endif
	push_radix_list(input, input->string);
}

static void english_execute_loop(english input)
{
	addr pos;
	fixed b, c;

	pos = input->pos;
	c = letdiv_half_bigdata(pos, 100);
	b = letdiv_half_bigdata(pos, 10);
	if (b || c) {
		if (0 <= input->index)
			number_name_cardinal(input);
		if (b && c) {
			push_radix_100(input, c);
#ifdef ENGLISH_RADIX_MODE2
			push_radix_char(input, ENGLISH_RADIX_MODE2);
#endif
			push_radix_hundred(input);
			push_radix_20(input, b);
		}
		else if (b) {
			push_radix_hundred(input);
			push_radix_20(input, b);
		}
		else if (c) {
			push_radix_100(input, c);
		}
	}
	if (! zerop_bignum(pos)) {
		input->index++;
		english_execute_loop(input);
	}
}

static void english_execute(english input)
{
	english_execute_loop(input);
	if (input->root == Nil)
		push_radix_20(input, 0);
}

static int english_output_(addr stream, english input, int minus)
{
	int first;
	addr left, right, pos;

	/* sign */
	if (minus) {
		Return(print_ascii_stream_(stream, "minus"));
		first = 0;
	}
	else {
		first = 1;
	}

	/* body */
	for (right = input->root; right != Nil; ) {
		GetCons(right, &left, &right);
		/* space */
		if (first) {
			first = 0;
		}
		else {
			Return(write_char_stream_(stream, ' '));
		}
		/* output */
		if (consp(left)) {
			while (left != Nil) {
				GetCons(left, &pos, &left);
				Return(print_string_stream_(stream, pos));
			}
		}
		else {
			Return(print_string_stream_(stream, left));
		}
	}

	return 0;
}

static int english_bignum_(LocalRoot local, addr stream, addr pos, int cardinal)
{
	int minus;
	struct english_struct str;

	/* input */
	cleartype(str);
	str.local = local;
	str.root = Nil;
	str.pos = pos;
	str.string = Nil;
	str.index = -1;

	/* sign */
	minus = minusp_bignum(pos);
	cardinal = (cardinal != 0);
	if (minus && (! cardinal))
		cardinal = 0;
	str.cardinal = cardinal;

	/* execute */
	english_execute(&str);
	return english_output_(stream, &str, minus);
}

int english_integer_(LocalRoot local, addr stream, addr pos, int cardinal)
{
	int sign;
	addr copy;
	fixed value;
	LocalStack stack;

	Check(local == NULL, "local error");
	push_local(local, &stack);

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			castfixed_fixnum(pos, &sign, &value);
			bignum_value_local(local, &copy, sign, value);
			Return(english_bignum_(local, stream, copy, cardinal));
			break;

		case LISPTYPE_BIGNUM:
			bignum_copy_local(local, &copy, pos);
			Return(english_bignum_(local, stream, copy, cardinal));
			break;

		default:
			return TypeError_(pos, INTEGER);
	}
	rollback_local(local, stack);

	return 0;
}

static int english_unit_string_(LocalRoot local,
		addr *ret, fixnum value, int cardinal, int localp)
{
	addr list, pos, queue;
	struct english_struct str;

	cleartype(str);
	str.local = local;
	str.root = Nil;
	str.index = value;
	str.cardinal = (cardinal != 0);
	str.string = Nil;
	number_name_cardinal(&str);

	/* output */
	charqueue_local(local, &queue, 0);
	GetCar(str.root, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(pushstring_charqueue_local_(local, queue, pos));
	}
	if (localp)
		make_charqueue_local(local, queue, ret);
	else
		make_charqueue_heap(queue, ret);

	return 0;
}

int english_unit_local_(LocalRoot local, addr *ret, addr pos, int cardinal)
{
	fixnum value;

	Check(local == NULL, "local error");
	if (GetFixnum_signed(pos, &value))
		return fmte_("The argument ~S must be a positive fixnum.", pos, NULL);

	return english_unit_string_(local, ret, value, cardinal, 1);
}

int english_unit_heap_(LocalRoot local, addr *ret, addr pos, int cardinal)
{
	fixnum value;
	LocalStack stack;

	Check(local == NULL, "local error");
	if (GetFixnum_signed(pos, &value))
		return fmte_("The argument ~S must be a positive fixnum.", pos, NULL);

	push_local(local, &stack);
	Return(english_unit_string_(local, ret, value, cardinal, 0));
	rollback_local(local, stack);

	return 0;
}


/*
 *  Roma number
 */
static int roma_10_(addr stream, unsigned value, int subp)
{
	const char *ptr;

	switch (value) {
		case 1: ptr = "I"; break;
		case 2: ptr = "II"; break;
		case 3: ptr = "III"; break;
		case 4: ptr = subp? "IIII": "IV"; break;
		case 5: ptr = "V"; break;
		case 6: ptr = "VI"; break;
		case 7: ptr = "VII"; break;
		case 8: ptr = "VIII"; break;
		case 9: ptr = subp? "VIIII": "IX"; break;
		default: return fmte_("Invalid radix value.", NULL);
	}
	return print_ascii_stream_(stream, ptr);
}

static int roma_100_(addr stream, unsigned value, int subp)
{
	const char *ptr;

	switch (value) {
		case 1: ptr = "X"; break;
		case 2: ptr = "XX"; break;
		case 3: ptr = "XXX"; break;
		case 4: ptr = subp? "XXXX": "XL"; break;
		case 5: ptr = "L"; break;
		case 6: ptr = "LX"; break;
		case 7: ptr = "LXX"; break;
		case 8: ptr = "LXXX"; break;
		case 9: ptr = subp? "LXXXX": "XC"; break;
		default: return fmte_("Invalid radix value.", NULL);
	}
	return print_ascii_stream_(stream, ptr);
}

static int roma_1000_(addr stream, unsigned value, int subp)
{
	const char *ptr;

	switch (value) {
		case 1: ptr = "C"; break;
		case 2: ptr = "CC"; break;
		case 3: ptr = "CCC"; break;
		case 4: ptr = subp? "CCCC": "CD"; break;
		case 5: ptr = "D"; break;
		case 6: ptr = "DC"; break;
		case 7: ptr = "DCC"; break;
		case 8: ptr = "DCCC"; break;
		case 9: ptr = subp? "DCCCC": "CM"; break;
		default: return fmte_("Invalid radix value.", NULL);
	}
	return print_ascii_stream_(stream, ptr);
}

static int roma_4000_(addr stream, unsigned value, int subp)
{
	const char *ptr;

	switch (value) {
		case 1: ptr = "M"; break;
		case 2: ptr = "MM"; break;
		case 3: ptr = "MMM"; break;
		default: return fmte_("Invalid radix value.", NULL);
	}
	return print_ascii_stream_(stream, ptr);
}

static int roma_call_(addr stream, int value, int subp)
{
	int a, b;

	if (value <= 0) {
		return 0;
	}

	if (value < 10) {
		return roma_10_(stream, value, subp);
	}
	else if (value < 100) {
		a = value / 10;
		b = value % 10;
		Return(roma_100_(stream, a, subp));
		return roma_call_(stream, b, subp);
	}
	else if (value < 1000) {
		a = value / 100;
		b = value % 100;
		Return(roma_1000_(stream, a, subp));
		return roma_call_(stream, b, subp);
	}
	else {
		a = value / 1000;
		b = value % 1000;
		Return(roma_4000_(stream, a, subp));
		return roma_call_(stream, b, subp);
	}
}

int roma_integer_(addr stream, fixnum value, int subp)
{
	Check(! (1 <= value && value <= 3999), "value error");
	return roma_call_(stream, (int)value, subp);
}


/************************************************************
 *  function.c
 ************************************************************/

static void alloc_function(LocalRoot local, addr *ret,
		addr name, addr code, int macro, int compiled)
{
	addr pos;
	struct function_struct *str;

	Check(name != Nil && ! callnamep(name), "name error");
	alloc_smallsize(local, &pos,
			LISPTYPE_FUNCTION,
			FUNCTION_INDEX_SIZE,
			sizeoft(struct function_struct));
	SetCodeFunction_Low(pos, code);
	SetNameFunction_Low(pos, name);
	str = StructFunction_Low(pos);
	str->macro = macro;
	str->compiled = compiled;
	str->trace = 0;
	str->index = p_empty;
	*ret = pos;
}

void function_alloc(LocalRoot local, addr *ret, addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	alloc_function(local, ret, name, code, 0, 0);
}
void function_local(LocalRoot local, addr *ret, addr name, addr code)
{
	Check(local == NULL, "local error");
	CheckType(code, LISPTYPE_CODE);
	alloc_function(local, ret, name, code, 0, 0);
}
void function_heap(addr *ret, addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	alloc_function(NULL, ret, name, code, 0, 0);
}

void function_empty_heap(addr *ret, addr name)
{
	alloc_function(NULL, ret, name, Nil, 0, 0);
}

void macro_alloc(LocalRoot local, addr *ret, addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	alloc_function(local, ret, name, code, 1, 0);
}
void macro_local(LocalRoot local, addr *ret, addr name, addr code)
{
	Check(local == NULL, "local error");
	CheckType(code, LISPTYPE_CODE);
	alloc_function(local, ret, name, code, 1, 0);
}
void macro_heap(addr *ret, addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	alloc_function(NULL, ret, name, code, 1, 0);
}

void compiled_alloc(LocalRoot local, addr *ret, addr name)
{
	alloc_function(local, ret, name, Nil, 0, 1);
}
void compiled_local(LocalRoot local, addr *ret, addr name)
{
	Check(local == NULL, "local error");
	alloc_function(local, ret, name, Nil, 0, 1);
}
void compiled_heap(addr *ret, addr name)
{
	alloc_function(NULL, ret, name, Nil, 0, 1);
}

void compiled_macro_alloc(LocalRoot local, addr *ret, addr name)
{
	alloc_function(local, ret, name, Nil, 1, 1);
}
void compiled_macro_local(LocalRoot local, addr *ret, addr name)
{
	Check(local == NULL, "local error");
	alloc_function(local, ret, name, Nil, 1, 1);
}
void compiled_macro_heap(addr *ret, addr name)
{
	alloc_function(NULL, ret, name, Nil, 1, 1);
}


/*
 *  system
 */
static int compiled_callname_heap(addr *ret, addr name)
{
	if (GetType(name) == LISPTYPE_CALLNAME) {
		*ret = name;
		return 0;
	}
	return parse_callname_heap(ret, name);
}

void compiled_system(addr *ret, addr name)
{
	if (compiled_callname_heap(&name, name))
		Abort("callname error.");
	alloc_function(NULL, ret, name, Nil, 0, 1);
}

void compiled_setf_system(addr *ret, addr symbol)
{
	if (GetType(symbol) != LISPTYPE_CALLNAME) {
		Check(! symbolp(symbol), "type error.");
		setf_callname_heap(&symbol, symbol);
	}
	alloc_function(NULL, ret, symbol, Nil, 0, 1);
}

void compiled_macro_system(addr *ret, addr name)
{
	if (compiled_callname_heap(&name, name))
		Abort("callname error.");
	alloc_function(NULL, ret, name, Nil, 1, 1);
}


/*
 *  setcompiled
 */
void setcompiled_code(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_code, "type error");
	StructFunction(pos)->index = p;
}

void getcompiled_code(addr pos, pointer *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	*ret = StructFunction(pos)->index;
	Check(pointer_table[*ret].type != CallBind_code, "type error");
}

void setcompiled_macro(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_macro, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_none(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_none, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_any(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_any, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_empty(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_empty, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_rest, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_dynamic, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var1, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var3(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var3, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var4(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var4, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var5(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var5, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var6(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var6, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt1, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_opt2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt2, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_opt3(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt3, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_opt4(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt4, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_opt5(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt5, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var1opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var1opt1, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var2opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2opt1, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var3opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var3opt1, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var4opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var4opt1, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var5opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var5opt1, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var1opt2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var1opt2, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var2opt2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2opt2, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var2opt3(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2opt3, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var1rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var1rest, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var2rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2rest, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var3rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var3rest, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var4rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var4rest, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_opt1rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt1rest, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var1dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var1dynamic, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var2dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2dynamic, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var3dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var3dynamic, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_var4dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var4dynamic, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_opt1dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt1dynamic, "type error");
	StructFunction(pos)->index = p;
}


void setcompiled_extend_macro(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_macro, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_rest, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_dynamic, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_any(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_any, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_empty(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_empty, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var1, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var2, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var3(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var3, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var4(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var4, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var5(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var5, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var6(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var6, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_opt1, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_opt2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_opt2, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_opt3(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_opt3, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var1opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var1opt1, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var1opt2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var1opt2, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var1opt3(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var1opt3, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var2opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var2opt1, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var2opt2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var2opt2, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var2opt3(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var2opt3, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var3opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var3opt1, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var3opt2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var3opt2, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var3opt3(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var3opt3, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var1rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var1rest, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var2rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var2rest, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var3rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var3rest, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var1dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var1dynamic, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var2dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var2dynamic, "type error");
	StructFunction(pos)->index = p;
}

void setcompiled_extend_var3dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_var3dynamic, "type error");
	StructFunction(pos)->index = p;
}


struct function_struct *structfunction(addr pos)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	return StructFunction_Low(pos);
}

void getcodefunction(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	GetCodeFunction_Low(pos, ret);
}

void setcodefunction(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetCodeFunction_Low(pos, value);
}

void getnamefunction(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	GetNameFunction_Low(pos, ret);
}

void setnamefunction(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetNameFunction_Low(pos, value);
}

void getdatafunction(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	GetDataFunction_Low(pos, ret);
}

void setdatafunction(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDataFunction_Low(pos, value);
}

static void getplist_function(addr pos, constindex index, addr *ret)
{
	addr type;

	CheckType(pos, LISPTYPE_FUNCTION);
	GetTableFunction_Low(pos, &pos);
	GetConstant(index, &type);
	getplist(pos, type, ret);
}
void gettype_function(addr pos, addr *ret)
{
	getplist_function(pos, CONSTANT_SYSTEM_TYPE_FUNCTION, ret);
}

static void setplist_function(addr pos, constindex index, addr value)
{
	addr table, type;

	CheckType(pos, LISPTYPE_FUNCTION);
	CheckReadOnly(pos);
	GetTableFunction_Low(pos, &table);
	GetConstant(index, &type);
	if (setplist_heap(table, type, value, &table))
		SetTableFunction_Low(pos, table);
}
void settype_function(addr pos, addr value)
{
	setplist_function(pos, CONSTANT_SYSTEM_TYPE_FUNCTION, value);
}

void getdocumentation_function(addr pos, addr *ret)
{
	getplist_function(pos, CONSTANT_COMMON_DOCUMENTATION, ret);
}

void setdocumentation_function(addr pos, addr value)
{
	setplist_function(pos, CONSTANT_COMMON_DOCUMENTATION, value);
}

void getlambda_expression_function(addr pos, addr *ret)
{
	getplist_function(pos, CONSTANT_COMMON_LAMBDA, ret);
}

void setlambda_expression_function(addr pos, addr value)
{
	setplist_function(pos, CONSTANT_COMMON_LAMBDA, value);
}

void getdefunform_function(addr pos, addr *ret)
{
	getplist_function(pos, CONSTANT_COMMON_DEFUN, ret);
}

void setdefunform_function(addr pos, addr value)
{
	setplist_function(pos, CONSTANT_COMMON_DEFUN, value);
}

int functionp(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION;
}

int funcall_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->macro == 0;
}

int macro_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->macro;
}

int interpreted_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->compiled == 0;
}

int interpreted_funcall_function_p(addr pos)
{
	struct function_struct *str;

	if (GetType(pos) != LISPTYPE_FUNCTION)
		return 0;
	str = StructFunction_Low(pos);

	return str->compiled == 0 && str->macro == 0;
}

int interpreted_macro_function_p(addr pos)
{
	struct function_struct *str;

	if (GetType(pos) != LISPTYPE_FUNCTION)
		return 0;
	str = StructFunction_Low(pos);

	return str->compiled == 0 && str->macro;
}

int compiled_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->compiled;
}

int compiled_funcall_function_p(addr pos)
{
	struct function_struct *str;

	if (GetType(pos) != LISPTYPE_FUNCTION)
		return 0;
	str = StructFunction_Low(pos);

	return str->compiled && str->macro == 0;
}

int compiled_macro_function_p(addr pos)
{
	struct function_struct *str;

	if (GetType(pos) != LISPTYPE_FUNCTION)
		return 0;
	str = StructFunction_Low(pos);

	return str->compiled && str->macro;
}

void settrace_function(addr pos)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(GetStatusReadOnly(pos), "readonly error");
	StructFunction_Low(pos)->trace = 1;
}

int tracep_function(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->trace;
}


/************************************************************
 *  function_constant.c
 ************************************************************/

/*
 *  constantly
 */
static int function_constantly_nil(Execute ptr, addr ignore)
{
	setresult_control(ptr, Nil);
	return 0;
}

static int function_constantly_t(Execute ptr, addr ignore)
{
	setresult_control(ptr, T);
	return 0;
}

static int function_constantly_values(Execute ptr, addr list)
{
	setvalues_list_control(ptr, list);
	return 0;
}

void build_function(void)
{
	addr pos;

	/* nil */
	compiled_heap(&pos, Nil);
	setcompiled_dynamic(pos, p_defun_constantly_nil);
	SetConst(FUNCTION_NIL, pos);
	/* t */
	compiled_heap(&pos, Nil);
	setcompiled_dynamic(pos, p_defun_constantly_t);
	SetConst(FUNCTION_T, pos);
	/* values */
	compiled_heap(&pos, Nil);
	setcompiled_dynamic(pos, p_defun_constantly_values);
	SetConst(FUNCTION_VALUES, pos);
}

void init_function(void)
{
	SetPointerCall(defun, dynamic, constantly_nil);
	SetPointerCall(defun, dynamic, constantly_t);
	SetPointerCall(defun, dynamic, constantly_values);
}


/************************************************************
 *  gc.c
 ************************************************************/

#define GCEXEC_DEFAULT			32
static void gcexec_default(void)
{
	if ((heap_gc_count % GCEXEC_DEFAULT) == 0)
		gcexec_full();
	else
		gcexec_partial();
}

void gcexec(enum GcMode mode)
{
	switch (mode) {
		case GcMode_Default:
			gcexec_default();
			break;

		case GcMode_Partial:
			gcexec_partial();
			break;

		case GcMode_Full:
			gcexec_full();
			break;

		case GcMode_Off:
		default:
			return;
	}
	heap_gc_count++;
}

void gcsync(Execute ptr, enum GcMode mode)
{
	clear_values_execute(ptr);
	gcstart_execute(ptr);
	if (ptr->index == 0) {
		gcexec(mode);
		gcend_execute();
	}
	else {
		gcwait_execute(ptr);
	}
	lisp_gcsync = GcMode_Off;
}


/************************************************************
 *  gc_check.c
 ************************************************************/

/* reference */
static int heap_check_object(addr pos)
{
	if (! valid_header(pos)) {
		info("HEAP-CHECK: ERROR");
		info("HEAP-CHECK: Invalid type %02X.", GetType(pos));
		return 1;
	}

	return 0;
}

static int heap_check_reference_heap(void)
{
	struct heap_addr *str;

	str = (struct heap_addr *)heap_tail;
	for (; LessPointer(str, heap_range); str++) {
		if (heap_check_object(str->pos)) {
			info("HEAP-CHECK: heap object error");
			return 1;
		}
	}

	return 0;
}

static int heap_check_reference_local(Execute ptr)
{
	addr *array;
	struct localcell *cell;
	size_t i, count;

	for (cell = ptr->local->cell; cell; cell = cell->next) {
		array = cell->point;
		count = cell->count;
		for (i = 0; i < count; i++) {
			if (heap_check_object(array[i])) {
				info("HEAP-CHECK: local error");
				return 1;
			}
		}
	}

	return 0;
}

static int heap_check_reference(void)
{
	if (heap_check_reference_heap())
		return 1;
	if (foreach_check_execute(heap_check_reference_local))
		return 1;

	return 0;
}

/* space */
static size_t heap_check_size(addr pos)
{
	size_t size;

	switch (GetType(pos)) {
		case LISPSYSTEM_SPACE1:
			GetSizeSpace1(pos, &size);
			break;

		case LISPSYSTEM_SPACE:
			GetSizeSpace(pos, &size);
			break;

		case LISPSYSTEM_RESERVED:
			GetSizeReserved(pos, &size);
			break;

		default:
			size = getobjectlength(pos);
			break;
	}

	return size;
}


static int heap_check_space(void)
{
	addr pos, next;

	for (pos = heap_root; pos ==  heap_front; pos = next) {
		if (heap_front < pos) {
			info("HEAP-CHECK: heap_front error");
			return 1;
		}
		if (heap_check_object(pos)) {
			return 1;
		}
		next = pos + heap_check_size(pos);
	}

	return 0;
}

void heap_check(void)
{
	info("HEAP-CHECK: start.");
	info("HEAP-CHECK: check-reference.");
	if (heap_check_reference()) {
		info("HEAP-CHECK: REFERENCE ERROR.");
		Abort("heap-check error");
		return;
	}
	info("HEAP-CHECK: check-space.");
	if (heap_check_space()) {
		info("HEAP-CHECK: SPACE ERROR.");
		Abort("heap-check error");
		return;
	}
	info("HEAP-CHECK: end.");
}


/************************************************************
 *  gc_execute.c
 ************************************************************/

#define IsObjectValue(x)    ((x) < LISPSYSTEM_SPACE)
#define IsObject(x)         (IsObjectValue(GetType(x)))

/***********************************************************************
 *  Full Garbage Collection
 ***********************************************************************/
/*
 *  checkallobject
 */
#ifdef LISP_DEBUG
static void checkallobject_heap(void)
{
	addr pos;
	struct heap_addr *str;

	str = (struct heap_addr *)heap_tail;
	for (; LessPointer(str, heap_range); str++) {
		pos = str->pos;
		Check((unsigned)LISPTYPE_SIZE <= (unsigned)pos[0], "type size error");
		Check(! IsObject(pos), "type error");
		if (GetStatusGc(pos))
			infoprint(pos);
	}
}

static void checkallarray_loop(addr *array, size_t count)
{
	addr pos;
	size_t i;

	for (i = 0; i < count; i++) {
		pos = array[i];
		Check((unsigned)LISPTYPE_SIZE <= (unsigned)pos[0], "type size error");
		Check(! IsObject(pos), "type error");
		if (GetStatusGc(pos))
			infoprint(pos);
	}
}

static void checkallobject_local(Execute ptr)
{
	struct localcell *cell;

	for (cell = ptr->local->cell; cell; cell = cell->next)
		checkallarray_loop(cell->point, cell->count);
}

static void checkallobject_debug(void)
{
	checkallobject_heap();
	foreach_execute(checkallobject_local);
}
#endif


/*
 *  walkthrough
 */
#define resetrecursive_loop(type) { \
	LenArray##type(pos, &size); \
	for (i = 0; i < size; i++) { \
		GetArray##type(pos, i, &value); \
		resetrecursive(value); \
	} \
}

static void resetrecursive(addr pos)
{
	size_t i, size;
	addr value;

	if (pos == Unbound || pos == NULL)
		return;
	Check(pos == NULL, "null error");
	Check(pos[0] == 0xAA, "memory 0xAA error");
	Check(! IsObject(pos), "type error");
	if (GetStatusGc(pos))
		return;

	SetStatusValue(pos, LISPSTATUS_GC, 1);
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			resetrecursive_loop(A2);
			break;

		case LISPSIZE_ARRAY4:
			resetrecursive_loop(A4);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			resetrecursive_loop(A8);
			break;
#endif

		case LISPSIZE_SMALLSIZE:
			resetrecursive_loop(SS);
			break;

		case LISPSIZE_ARRAYBODY:
			resetrecursive_loop(AB);
			break;

		default:
			break;
	}
}

static void walkthrough_heap(void)
{
	size_t i;

	for (i = 0; i < LISPINDEX_SIZE; i++)
		resetrecursive(lisp_root[i]);
}

static void resetrecursive_local(addr *array, size_t count)
{
	size_t i;

	for (i = 0; i < count; i++)
		resetrecursive(array[i]);
}

static void walkthrough_local(Execute ptr)
{
	struct localcell *cell;

	for (cell = ptr->local->cell; cell; cell = cell->next)
		resetrecursive_local(cell->point, cell->count);
}

static void walkthrough(void)
{
	walkthrough_heap();
	foreach_execute(walkthrough_local);
}


/*
 *  freegcobject
 */
static void freegcobject_type(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_STREAM:
			if (file_stream_p(pos) && open_stream_p(pos))
				force_close_stream_file(pos);
			break;

		default:
			break;
	}
}


/*
 *  replacespace
 */
static void replacespace(void)
{
	addr pos;
	struct heap_addr *root, *str;
	size_t size;

	root = (struct heap_addr *)heap_tail;
	for (str = root; LessPointer(str, heap_range); str++) {
		pos = str->pos;
		if (GetStatusGc(pos)) {
			SetStatusValue(pos, LISPSTATUS_GC, 0);
		}
		else {
			freegcobject_type(pos);
			size = getobjectlength(pos);
			makespace_heap(pos, size);
			if (root != str)
				memcpy(str, root, sizeoft(struct heap_addr));
			root++;
			/* statistics */
			Check(heap_object < size, "heap_object error");
			Check(heap_count == 0, "heap_count error");
			heap_object -= size;
			heap_count--;
		}
	}
	heap_tail = (addr)root;
}


/*
 *  setallobject
 */
static void setallarray(addr *array, size_t count)
{
	addr pos;
	size_t i;

	for (i = 0; i < count; i++) {
		pos = array[i];
		Check((unsigned)LISPTYPE_SIZE <= (unsigned)pos[0], "type size error");
		Check(! IsObject(pos), "type error");
		SetStatusValue(pos, LISPSTATUS_GC, 0);
	}
}

static void setallobject_local(Execute ptr)
{
	struct localcell *cell;

	for (cell = ptr->local->cell; cell; cell = cell->next)
		setallarray(cell->point, cell->count);
}

static void setallobject(void)
{
	foreach_execute(setallobject_local);
}


/*
 *  moveheappos
 */
static int getmemorylength_break(addr pos, size_t *ret)
{
	switch (GetType(pos)) {
		case LISPSYSTEM_SPACE1:
			GetSizeSpace1(pos, ret);
			break;

		case LISPSYSTEM_SPACE:
			GetSizeSpace(pos, ret);
			break;

		case LISPSYSTEM_RESERVED:
			GetSizeReserved(pos, ret);
			return 1;

		default:
			*ret = getobjectlength(pos);
			return 1;
	}

	return 0;
}

static void moveheappos(void)
{
	size_t size;

	heap_pos = heap_root;
	while (heap_pos < heap_front) {
		if (! getmemorylength_break(heap_pos, &size))
			break;
		heap_pos += size;
	}
}


/*
 *  full
 */
void gcexec_full(void)
{
#ifdef LISP_DEBUG
	checkallobject_debug();
#endif
	walkthrough();
	replacespace();
	setallobject();
	moveheappos();
	heap_gc_full++;
}


/***********************************************************************
 *  Partial Garbage Collection
 ***********************************************************************/
/*
 *  partial
 */
void gcexec_partial(void)
{
	gcexec_full();
	return;

	heap_gc_partial++;
}


/************************************************************
 *  hashtable.c
 ************************************************************/

/*
 *  frontend
 */
static void setlimit(addr pos)
{
	struct StructHashtable *ptr;

	ptr = PtrStructHashtable(pos);
	if (! ptr->expand_p) {
		ptr->limit = 0;
		return;
	}
	if (ptr->resize_float_p) {
		ptr->limit = (size_t)(ptr->size * ptr->threshold);
	}
	else {
		ptr->limit = ptr->size + ptr->resize_integer;
		if (ptr->limit < ptr->size)
			ptr->limit = SIZE_MAX;
	}
}

void hashtable_full_alloc(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		double_float resize, double_float threshold)
{
	addr pos, array;
	struct StructHashtable *ptr;

	/* argument check */
	Check(test != HASHTABLE_TEST_EQ &&
			test != HASHTABLE_TEST_EQL &&
			test != HASHTABLE_TEST_EQUAL &&
			test != HASHTABLE_TEST_EQUALP &&
			test != HASHTABLE_TEST_CACHE, "test error");
	if (size < HASHTABLE_SIZE_DEFAULT)
		size = HASHTABLE_SIZE_DEFAULT;
	Check(resize < 1.0, "rehash_size error");
	Check(threshold < 0.0 || 1.0 < threshold, "rehash_threshold error");

	/* allocate */
	alloc_smallsize(local, &pos, LISPTYPE_HASHTABLE, HASHTABLE_INDEX_SIZE,
			sizeoft(struct StructHashtable));
	alloc_hashtable(local, &array, LISPTYPE_VECTOR, size);
	ptr = PtrStructHashtable(pos);
	clearpoint(ptr);
	SetTableHash(pos, array);
	ptr->test = test;
	ptr->count = 0;
	ptr->size = size;
	ptr->threshold = threshold;
	setrehash_float_hashtable(pos, resize);
	*ret = pos;
}

void hashtable_full_local(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		double_float resize, double_float threshold)
{
	Check(local == NULL, "hashtable_local error");
	hashtable_full_alloc(local, ret, test, size, resize, threshold);
}

void hashtable_full_heap(addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		double_float resize, double_float threshold)
{
	hashtable_full_alloc(NULL, ret, test, size, resize, threshold);
}

void hashtable_integer_alloc(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		size_t resize, double_float threshold)
{
	addr pos, array;
	struct StructHashtable *ptr;

	/* argument check */
	Check(test != HASHTABLE_TEST_EQ &&
			test != HASHTABLE_TEST_EQL &&
			test != HASHTABLE_TEST_EQUAL &&
			test != HASHTABLE_TEST_EQUALP &&
			test != HASHTABLE_TEST_CACHE, "test error");
	if (size < HASHTABLE_SIZE_DEFAULT)
		size = HASHTABLE_SIZE_DEFAULT;
	Check(resize < 1, "rehash_size error");
	Check(threshold < 0.0 || 1.0 < threshold, "rehash_threshold error");

	/* allocate */
	alloc_smallsize(local, &pos, LISPTYPE_HASHTABLE, HASHTABLE_INDEX_SIZE,
			sizeoft(struct StructHashtable));
	alloc_hashtable(local, &array, LISPTYPE_VECTOR, size);
	ptr = PtrStructHashtable(pos);
	clearpoint(ptr);
	SetTableHash(pos, array);
	ptr->test = test;
	ptr->count = 0;
	ptr->size = size;
	ptr->threshold = threshold;
	setrehash_integer_hashtable(pos, resize);
	*ret = pos;
}

void hashtable_integer_local(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		size_t resize, double_float threshold)
{
	Check(local == NULL, "local error");
	hashtable_integer_alloc(local, ret, test, size, resize, threshold);
}

void hashtable_integer_heap(addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		size_t resize, double_float threshold)
{
	hashtable_integer_alloc(NULL, ret, test, size, resize, threshold);
}

/* default */
void hashtable_heap(addr *ret)
{
	hashtable_full_heap(ret,
			HASHTABLE_TEST_DEFAULT,
			HASHTABLE_SIZE_DEFAULT,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

void hashtable_local(LocalRoot local, addr *ret)
{
	hashtable_full_local(local, ret,
			HASHTABLE_TEST_DEFAULT,
			HASHTABLE_SIZE_DEFAULT,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

void hashtable_alloc(LocalRoot local, addr *ret)
{
	if (local)
		hashtable_local(local, ret);
	else
		hashtable_heap(ret);
}

void hashtable_size_heap(addr *ret, size_t size)
{
	hashtable_full_heap(ret,
			HASHTABLE_TEST_DEFAULT,
			size,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

void hashtable_size_local(LocalRoot local, addr *ret, size_t size)
{
	hashtable_full_local(local, ret,
			HASHTABLE_TEST_DEFAULT,
			size,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

void hashtable_size_alloc(LocalRoot local, addr *ret, size_t size)
{
	if (local)
		hashtable_size_local(local, ret, size);
	else
		hashtable_size_heap(ret, size);
}

void clear_hashtable_local(addr pos)
{
	addr temp;
	size_t i, size;

	GetTableHash(pos, &temp);
	LenArrayHash(temp, &size);
	for (i = 0; i < size; i++)
		SetArrayHash(temp, i, Nil);
	PtrStructHashtable(pos)->count = 0;
}

void clear_hashtable_heap(addr pos)
{
	addr temp;
	size_t size;
	struct StructHashtable *ptr;

	size = HASHTABLE_SIZE_DEFAULT;
	ptr = PtrStructHashtable(pos);
	/* array */
	heap_hashtable(&temp, LISPTYPE_VECTOR, (size_t)size);
	SetTableHash(pos, temp);
	/* struct */
	ptr->count = 0;
	ptr->size = size;
	setlimit(pos);
}

void clear_hashtable(addr pos)
{
	if (GetStatusDynamic(pos))
		clear_hashtable_heap(pos);
	else
		clear_hashtable_local(pos);
}

int hashtablep(addr pos)
{
	return GetType(pos) == LISPTYPE_HASHTABLE;
}

void gettest_hashtable(addr pos, enum HASHTABLE_TEST *ret)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	*ret = PtrStructHashtable(pos)->test;
}

void settest_hashtable(addr pos, enum HASHTABLE_TEST value)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	Check(GetStatusReadOnly(pos), "readonly error");
	PtrStructHashtable(pos)->test = value;
}

void gettest_symbol_hashtable(addr pos, addr *ret)
{
	enum HASHTABLE_TEST test;

	gettest_hashtable(pos, &test);
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

		case HASHTABLE_TEST_CACHE:
			GetConst(SYSTEM_CACHE, ret);
			break;

		default:
			GetConst(COMMON_ERROR, ret);
			break;
	}
}

void getcount_hashtable(addr pos, size_t *ret)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	*ret = PtrStructHashtable(pos)->count;
}

void inccount_hashtable(addr pos, size_t value)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	PtrStructHashtable(pos)->count += value;
}

void getsize_hashtable(addr pos, size_t *ret)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	*ret = PtrStructHashtable(pos)->size;
}

void setrehash_float_hashtable(addr pos, double_float value)
{
	struct StructHashtable *ptr;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	Check(value < 1.0, "rehash_size error");
	ptr = PtrStructHashtable(pos);
	ptr->resize_float_p = 1;
	ptr->resize_float = value;
	ptr->resize_integer = 0;
	ptr->expand_p = (value != 1.0);
	setlimit(pos);
}

int getrehash_float_hashtable(addr pos, double_float *ret)
{
	struct StructHashtable *ptr;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	ptr = PtrStructHashtable(pos);
	if (! ptr->resize_float_p)
		return 0;
	*ret = PtrStructHashtable(pos)->resize_float;

	return 1;
}

void setrehash_integer_hashtable(addr pos, size_t value)
{
	struct StructHashtable *ptr;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	ptr = PtrStructHashtable(pos);
	ptr->resize_float_p = 0;
	ptr->resize_float = 0;
	ptr->resize_integer = value;
	ptr->expand_p = (value != 0);
	setlimit(pos);
}

int getrehash_integer_hashtable(addr pos, size_t *ret)
{
	struct StructHashtable *ptr;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	ptr = PtrStructHashtable(pos);
	if (ptr->resize_float_p)
		return 0;
	*ret = PtrStructHashtable(pos)->resize_integer;

	return 1;
}

void getrehash_threshold_hashtable(addr pos, double_float *ret)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	*ret = PtrStructHashtable(pos)->threshold;
}


/*
 *  hashindex
 */
static int hashindex_eq_(addr key, size_t size, size_t *ret)
{
	fixnum value;
	Return(sxhash_eq_(key, &value));
	return Result(ret, (size_t)(value % size));
}

static int hashindex_eql_(addr key, size_t size, size_t *ret)
{
	fixnum value;

	switch (GetType(key)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_CHARACTER:
		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
			Return(sxhash_equal_(key, &value));
			return Result(ret, (size_t)(value % size));

		default:
			return hashindex_eq_(key, size, ret);
	}
}

static int hashindex_equal_(addr key, size_t size, size_t *ret)
{
	fixnum value;

	switch (GetType(key)) {
		case LISPTYPE_CONS:
		case LISPTYPE_ARRAY:
		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPSYSTEM_CHARACTER2:
			Return(sxhash_equal_(key, &value));
			return Result(ret, (size_t)(value % size));

		default:
			return hashindex_eql_(key, size, ret);
	}
}

static int hashindex_equalp_(addr key, size_t size, size_t *ret)
{
	fixnum value;

	switch (GetType(key)) {
		case LISPTYPE_CHARACTER:
		case LISPTYPE_ARRAY:
		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPSYSTEM_CHARACTER2:
			Return(sxhash_equalp_(key, &value));
			return Result(ret, (size_t)(value % size));

		default:
			return hashindex_equal_(key, size, ret);
	}
}

typedef int (*hashindextype)(addr, size_t, size_t *);
static const hashindextype hashindex_switch[] = {
	hashindex_eq_,
	hashindex_eql_,
	hashindex_equal_,
	hashindex_equalp_,
	hashindex_cache_,
};

static void gethashindex(addr pos, hashindextype *ret)
{
	*ret = hashindex_switch[GetTestHashtable(pos)];
}

static int call_hashindex_(addr pos, addr key, size_t *ret)
{
	hashindextype call_;
	gethashindex(pos, &call_);
	return (*call_)(key, PtrStructHashtable(pos)->size, ret);
}


/*
 *  gethashequal
 */
typedef int (*hashequaltype)(addr, addr, int *);
static const hashequaltype hashequal_switch[] = {
	eq_function_,
	eql_function_,
	equal_function_,
	equalp_function_,
	cache_equal_function_
};

static void gethashequal(addr pos, hashequaltype *ret)
{
	*ret = hashequal_switch[GetTestHashtable(pos)];
}


/*
 *  rehash
 */
static void insert_rehash(LocalRoot local,
		addr array, size_t index, addr key, addr value)
{
	addr cons, one, root;

	/* one */
	cons_alloc(local, &one, key, value);
	/* cons */
	GetArrayHash(array, index, &root);
	cons_alloc(local, &cons, one, root);
	/* setarray */
	SetArrayHash(array, index, cons);
}

static int resize_rehash_(LocalRoot local, addr pos, size_t resize)
{
	addr prev, next, left, right, key, value;
	size_t index, len, make;
	hashindextype call_;

	GetTableHash(pos, &prev);
	alloc_hashtable(local, &next, LISPTYPE_VECTOR, resize);

	gethashindex(pos, &call_);
	LenArrayHash(prev, &len);
	for (index = 0; index < len; index++) {
		GetArrayHash(prev, index, &right);
		while (right != Nil) {
			GetCons(right, &left, &right);
			GetCons(left, &key, &value);
			Return((*call_)(key, resize, &make));
			insert_rehash(local, next, make, key, value);
		}
	}
	SetTableHash(pos, next);

	return 0;
}

int force_resize_hashtable_(addr pos, size_t size)
{
	LocalRoot local;

	if (size == 0)
		size = 1;
	local = GetStatusDynamic(pos)? Local_Thread: NULL;
	Return(resize_rehash_(local, pos, size));
	PtrStructHashtable(pos)->size = size;
	setlimit(pos);

	return 0;
}

static int rehash_execute_(LocalRoot local, addr pos)
{
	size_t size, newsize;
	struct StructHashtable *ptr;

	/* get parameter */
	ptr = PtrStructHashtable(pos);
	if (! ptr->expand_p)
		return 0;
	if (ptr->count < ptr->limit)
		return 0;

	size = ptr->size;
	if (ptr->resize_float_p) {
		newsize = (size_t)(size * ptr->resize_float);
		if (size == newsize)
			newsize++;
	}
	else {
		newsize = size + ptr->resize_integer;
	}
	if (newsize < size)
		newsize = SIZE_MAX;

	/* resize array */
	Return(resize_rehash_(local, pos, newsize));
	ptr->size = newsize;
	setlimit(pos);

	return 0;
}


/*
 *  intern
 */
static int findroot_hashtable_(hashequaltype equal_,
		addr right, addr key, addr *value, int *ret)
{
	int check;
	addr left, leftkey;

	/* found=1, notfound=0 */
	while (right != Nil) {
		GetCons(right, &left, &right);
		GetCar(left, &leftkey);
		Return((*equal_)(leftkey, key, &check));
		if (check) {
			*value = left;
			return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static void appendroot_hashtable(LocalRoot local,
		addr array, size_t index, addr root, addr key, addr *ret)
{
	addr left, next;

	/* array[index] -> ((key . nil) . next)
	 * ret -> (key . nil) */
	conscar_alloc(local, &left, key);
	cons_alloc(local, &next, left, root);
	SetArrayHash(array, index, next);
	*ret = left;
}

static int intern_hashtable_alloc_(LocalRoot local,
		addr pos, addr key, addr *ret, int *existp)
{
	int check;
	size_t index;
	addr array, root;
	hashequaltype equal_;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Check(GetStatusReadOnly(pos), "readonly error");

	Return(rehash_execute_(local, pos));
	Return(call_hashindex_(pos, key, &index));
	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);

	gethashequal(pos, &equal_);
	Return(findroot_hashtable_(equal_, root, key, ret, &check));
	if (! check) {
		appendroot_hashtable(local, array, index, root, key, ret);
		PtrStructHashtable(pos)->count++;
		return Result(existp, 0); /* notfound, create */
	}

	return Result(existp, 1); /* find */
}

int internp_hashheap_(addr pos, addr key, addr *ret, int *existp)
{
	CheckType(pos, LISPTYPE_HASHTABLE);
	Check(GetStatusDynamic(pos), "dynamic error");
	return intern_hashtable_alloc_(NULL, pos, key, ret, existp);
}

int intern_hashheap_(addr pos, addr key, addr *ret)
{
	int check;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Check(GetStatusDynamic(pos), "dynamic error");
	return intern_hashtable_alloc_(NULL, pos, key, ret, &check);
}


/*
 *  find
 */
int findcons_hashtable_(addr pos, addr key, addr *ret)
{
	int check;
	size_t index;
	addr array, root, left, right, value;
	hashequaltype equal_;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(call_hashindex_(pos, key, &index));
	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);

	gethashequal(pos, &equal_);
	for (; root != Nil; root = right) {
		GetCons(root, &left, &right);
		GetCar(left, &value);
		Return((*equal_)(value, key, &check));
		if (check)
			return Result(ret, left);
	}

	return Result(ret, Nil);
}

int find_hashtable_(addr pos, addr key, addr *ret)
{
	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(findcons_hashtable_(pos, key, &pos));
	if (pos == Nil)
		return Result(ret, Unbound);
	GetCdr(pos, ret);
	return 0;
}

int findnil_hashtable_(addr pos, addr key, addr *ret)
{
	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(findcons_hashtable_(pos, key, &pos));
	if (pos == Nil)
		return Result(ret, Nil);
	GetCdr(pos, ret);
	return 0;
}

/* for debug */
int findnil_hashtable_debug(addr pos, addr key, addr *ret)
{
	CheckType(pos, LISPTYPE_HASHTABLE);
	Error(findcons_hashtable_(pos, key, &pos));
	if (pos == Nil)
		return 0;
	GetCdr(pos, ret);
	return 1;
}


/*
 *  delete
 */
int delete_hashtable_(addr pos, addr key, int *ret)
{
	int check;
	size_t index;
	addr array, root, left, right, leftkey, prev;
	hashequaltype equal_;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Check(GetStatusReadOnly(pos), "readonly error");

	Return(call_hashindex_(pos, key, &index));
	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);

	gethashequal(pos, &equal_);
	for (prev = Nil; root != Nil; root = right) {
		GetCons(root, &left, &right);
		GetCar(left, &leftkey);
		Return((*equal_)(leftkey, key, &check));
		if (check) {
			if (prev == Nil) {
				SetArrayHash(array, index, right);
			}
			else {
				SetCdr(prev, right);
			}
			PtrStructHashtable(pos)->count--;
			return Result(ret, 0);
		}
		prev = root;
	}

	return Result(ret, 1);
}


/*
 *  map-function
 */
void allkeys_hashtable_alloc(LocalRoot local, addr pos, addr *ret)
{
	addr cons, cell, result;
	size_t size, index;

	CheckType(pos, LISPTYPE_HASHTABLE);
	size = PtrStructHashtable(pos)->size;
	GetTableHash(pos, &pos);

	result = Nil;
	for (index = 0; index < size; index++) {
		GetArrayHash(pos, index, &cons);
		while (cons != Nil) {
			GetCons(cons, &cell, &cons);
			GetCar(cell, &cell);
			cons_alloc(local, &result, cell, result);
		}
	}

	*ret = result;
}

void allkeys_hashtable_heap(addr pos, addr *ret)
{
	allkeys_hashtable_alloc(NULL, pos, ret);
}

void allkeys_hashtable_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	allkeys_hashtable_alloc(local, pos, ret);
}

/* equalp */
static int equalp_allelement_(addr left, addr right, int *ret,
		int (*call_)(addr, addr, int *))
{
	int check;
	addr list, key, value1, value2;
	size_t size, index;

	size = PtrStructHashtable(left)->size;
	GetTableHash(left, &left);
	for (index = 0; index < size; index++) {
		GetArrayHash(left, index, &list);
		while (list != Nil) {
			GetCons(list, &key, &list);
			GetCons(key, &key, &value1);
			Return(find_hashtable_(right, key, &value2));
			if (value2 == Unbound)
				return Result(ret, 0);
			Return((*call_)(value1, value2, &check));
			if (! check)
				return Result(ret, 0);
		}
	}

	return Result(ret, 1);
}

static int equalcall_hashtable_(addr left, addr right, int *ret,
		int (*call_)(addr, addr, int *))
{
	struct StructHashtable *str1, *str2;

	str1 = PtrStructHashtable(left);
	str2 = PtrStructHashtable(right);
	if (str1->count != str2->count)
		return Result(ret, 0);
	if (str1->test != str2->test)
		return Result(ret, 0);

	return equalp_allelement_(left, right, ret, call_);
}

int equalp_hashtable_(addr left, addr right, int *ret)
{
	return equalcall_hashtable_(left, right, ret, equalp_function_);
}

int equalrt_hashtable_(addr left, addr right, int *ret)
{
	return equalcall_hashtable_(left, right, ret, equalrt_function_);
}


/* clang */
int findcons_char_hashtable_(addr pos, const char *key, addr *ret)
{
	int check;
	fixnum value;
	addr array, root, left, right, str;
	int (*equal_)(addr, const char *, int *);
	size_t index;

	CheckType(pos, LISPTYPE_HASHTABLE);
	switch (GetTestHashtable(pos)) {
		case HASHTABLE_TEST_EQUAL:
			Return(sxhash_char_equal_(key, &value));
			equal_ = string_equal_char_;
			break;

		case HASHTABLE_TEST_EQUALP:
			Return(sxhash_char_equalp_(key, &value));
			equal_ = string_equalp_char_;
			break;

		default:
			return Result(ret, Nil);
	}
	index = value % PtrStructHashtable(pos)->size;

	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);
	for (; root != Nil; root = right) {
		GetCons(root, &left, &right);
		GetCar(left, &str);
		if (! stringp(str))
			continue;
		Return((*equal_)(str, key, &check));
		if (! check)
			continue;

		return Result(ret, left);
	}

	return Result(ret, Nil);
}

int find_char_hashtable_(addr pos, const char *key, addr *ret)
{
	addr cons;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(findcons_char_hashtable_(pos, key, &cons));
	if (cons == Nil)
		return Result(ret, Unbound);
	GetCdr(cons, ret);
	return 0;
}

int findnil_char_hashtable_(addr pos, const char *key, addr *ret)
{
	addr cons;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(findcons_char_hashtable_(pos, key, &cons));
	if (cons == Nil)
		return Result(ret, Nil);
	GetCdr(cons, ret);
	return 0;
}

int findcons_unicode_hashtable_(addr pos, unicode key, addr *ret)
{
	fixnum value;
	addr array, root, left, right, check;
	int (*equal)(addr, unicode);
	size_t index;

	CheckType(pos, LISPTYPE_HASHTABLE);
	switch (GetTestHashtable(pos)) {
		case HASHTABLE_TEST_EQL:
		case HASHTABLE_TEST_EQUAL:
			Return(sxhash_unicode_equal_(key, &value));
			equal = character_equal_unicode;
			break;

		case HASHTABLE_TEST_EQUALP:
			Return(sxhash_unicode_equalp_(key, &value));
			equal = character_equalp_unicode;
			break;

		default:
			return Result(ret, Nil);
	}
	index = value % PtrStructHashtable(pos)->size;

	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);
	for (; root != Nil; root = right) {
		GetCons(root, &left, &right);
		GetCar(left, &check);
		if (! characterp(check))
			continue;
		if (! (*equal)(check, key))
			continue;

		return Result(ret, left);
	}

	return Result(ret, Nil);
}

int find_unicode_hashtable_(addr pos, unicode key, addr *ret)
{
	addr cons;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(findcons_unicode_hashtable_(pos, key, &cons));
	if (cons == Nil)
		return Result(ret, Unbound);
	GetCdr(cons, ret);
	return 0;
}

int findnil_unicode_hashtable_(addr pos, unicode key, addr *ret)
{
	addr cons;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(findcons_unicode_hashtable_(pos, key, &cons));
	if (cons == Nil)
		return Result(ret, Nil);
	GetCdr(cons, ret);
	return 0;
}

int findcons_character2_hashtable_(addr pos, unicode a, unicode b, addr *ret)
{
	fixnum value;
	addr array, root, left, right, check;
	int (*equal)(addr, unicode, unicode);
	size_t index;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	switch (GetTestHashtable(pos)) {
		case HASHTABLE_TEST_EQUAL:
			Return(sxhash_character2_equal_(a, b, &value));
			equal = character2_equal_unicode;
			break;

		case HASHTABLE_TEST_EQUALP:
			Return(sxhash_character2_equalp_(a, b, &value));
			equal = character2_equalp_unicode;
			break;

		default:
			return Result(ret, Nil);
	}
	index = value % PtrStructHashtable(pos)->size;

	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);
	for (; root != Nil; root = right) {
		GetCons(root, &left, &right);
		GetCar(left, &check);
		if (GetType(check) != LISPSYSTEM_CHARACTER2)
			continue;
		if (! (*equal)(check, a, b))
			continue;

		return Result(ret, left);
	}

	return Result(ret, Nil);
}

int find_character2_hashtable_(addr pos, unicode a, unicode b, addr *ret)
{
	addr cons;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	Return(findcons_character2_hashtable_(pos, a, b, &cons));
	if (cons == Nil)
		return Result(ret, Unbound);
	GetCdr(cons, ret);
	return 0;
}

int findnil_character2_hashtable_(addr pos, unicode a, unicode b, addr *ret)
{
	addr cons;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	Return(findcons_character2_hashtable_(pos, a, b, &cons));
	if (cons == Nil)
		return Result(ret, Nil);
	GetCdr(cons, ret);
	return 0;
}


/*
 *  iterator
 */
struct StructHashIterator {
	int finish;
	size_t index;
};

enum HashIterator {
	HashIterator_Hash,
	HashIterator_Array,
	HashIterator_List,
	HashIterator_Size
};

#define PtrHashIterator(x) PtrBodySSa(x, HashIterator_Size)
#define PtrStructHashIterator(x) ((struct StructHashIterator *)PtrHashIterator(x))
#define GetIndexHashIterator(x) (PtrStructHashtable(x)->index)
#define RetHashIterator RetArraySS
#define GetHashIterator GetArraySS
#define SetHashIterator SetArraySS

void hash_iterator_alloc(LocalRoot local, addr *ret, addr table)
{
	addr pos, array;
	struct StructHashIterator *ptr;

	CheckType(table, LISPTYPE_HASHTABLE);
	alloc_smallsize(local, &pos,
			LISPSYSTEM_HASHITERATOR,
			HashIterator_Size,
			sizeoft(struct StructHashIterator));
	ptr = PtrStructHashIterator(pos);
	clearpoint(ptr);
	ptr->finish = 0;
	ptr->index = 0;
	GetTableHash(table, &array);
	SetHashIterator(pos, HashIterator_Hash, table);
	SetHashIterator(pos, HashIterator_Array, array);
	*ret = pos;
}

void hash_iterator_local(LocalRoot local, addr *ret, addr table)
{
	Check(local == NULL, "local error");
	hash_iterator_alloc(local, ret, table);
}

void hash_iterator_heap(addr *ret, addr table)
{
	hash_iterator_alloc(NULL, ret, table);
}

void set_hash_iterator(addr pos, addr table)
{
	addr array;
	struct StructHashIterator *ptr;

	CheckType(pos, LISPSYSTEM_HASHITERATOR);
	CheckType(table, LISPTYPE_HASHTABLE);
	ptr = PtrStructHashIterator(pos);
	ptr->finish = 0;
	ptr->index = 0;
	GetTableHash(table, &array);
	SetHashIterator(pos, HashIterator_Hash, table);
	SetHashIterator(pos, HashIterator_Array, array);
}

/* 0:finish, 1:find */
int next_hash_iterator(addr pos, addr *key, addr *value)
{
	addr list, array, cons;
	struct StructHashIterator *ptr;
	size_t size, i;

	CheckType(pos, LISPSYSTEM_HASHITERATOR);
	/* Iterator is already closed */
	ptr = PtrStructHashIterator(pos);
	if (ptr->finish)
		return 0;
	GetHashIterator(pos, HashIterator_List, &list);
	GetHashIterator(pos, HashIterator_Array, &array);
	/* First search */
	if (list == Nil) {
		goto search;
	}
	/* After first */
	GetCdr(list, &list);
	if (list == Nil) {
		ptr->index++;
		goto search;
	}
	goto find;

search:
	LenArrayHash(array, &size);
	for (i = ptr->index; i < size; i++) {
		GetArrayHash(array, i, &list);
		if (list != Nil) {
			ptr->index = i;
			goto find;
		}
	}
	ptr->finish = 1;
	return 0;

find:
	GetCar(list, &cons);
	GetCons(cons, key, value);
	SetHashIterator(pos, HashIterator_List, list);
	return 1;
}


/************************************************************
 *  heap.c
 ************************************************************/

/*
 *  allocate
 */
void heap_cons(addr *ret)
{
	heap_array2_memory(ret, LISPTYPE_CONS, 2);
	heap_cons_count++;
}

void heap_symbol(addr *ret)
{
	heap_array2_memory(ret, LISPTYPE_SYMBOL, SYMBOL_INDEX_SIZE);
	heap_symbol_count++;
}

void heap_array2_memory(addr *ret, enum LISPTYPE type, byte16 array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA2(array);
	Check(0xFFFFUL < size, "size error");
	allocheap(size, type, &pos, 1);
	SetStatus(pos, LISPSIZE_ARRAY2);
	*PtrLenArrayA2(pos) = array;
	nilarray2(pos, array);
	*ret = pos;
}

void heap_array4_memory(addr *ret, enum LISPTYPE type, byte32 array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA4(array);
	Check(0xFFFFFFFFUL < size, "size error");
	allocheap(size, type, &pos, 0);
	SetStatus(pos, LISPSIZE_ARRAY4);
	*PtrLenArrayA4(pos) = array;
	nilarray4(pos, array);
	*ret = pos;
}

#ifdef LISP_ARCH_64BIT
void heap_array8(addr *ret, enum LISPTYPE type, size_t array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA8(array);
	allocheap(size, type, &pos, 0);
	SetStatus(pos, LISPSIZE_ARRAY8);
	*PtrLenArrayA8(pos) = array;
	nilarray8(pos, array);
	*ret = pos;
}
#endif

void heap_body2_memory(addr *ret, enum LISPTYPE type, byte16 body)
{
	addr pos;
	size_t size;

	size = MemoryLengthB2(body);
	Check(0xFFFFUL < size, "size error");
	allocheap(size, type, &pos, 1);
	SetStatus(pos, LISPSIZE_BODY2);
	*PtrLenBodyB2(pos) = body;
	*ret = pos;
}

void heap_body4_memory(addr *ret, enum LISPTYPE type, byte32 body)
{
	addr pos;
	size_t size;

	size = MemoryLengthB4(body);
	Check(0xFFFFFFFFUL < size, "size error");
	allocheap(size, type, &pos, 0);
	SetStatus(pos, LISPSIZE_BODY4);
	*PtrLenBodyB4(pos) = body;
	*ret = pos;
}

#ifdef LISP_ARCH_64BIT
void heap_body8(addr *ret, enum LISPTYPE type, size_t body)
{
	addr pos;
	size_t size;

	size = MemoryLengthB8(body);
	allocheap(size, type, &pos, 0);
	SetStatus(pos, LISPSIZE_BODY8);
	*PtrLenBodyB8(pos) = body;
	*ret = pos;
}
#endif

void heap_smallsize_memory(addr *ret, enum LISPTYPE type, byte array, byte body)
{
	addr pos;
	size_t size;

	size = MemoryLengthSS(array, body);
	Check(0xFFFFUL < size, "size error");
	allocheap(size, type, &pos, 1);
	SetStatus(pos, LISPSIZE_SMALLSIZE);
	nilarray2(pos, array);
	*PtrLenArraySS(pos) = array;
	*PtrLenBodySS(pos) = body;
	*ret = pos;
}

void heap_arraybody_memory(addr *ret, enum LISPTYPE type, byte16 array, byte16 body)
{
	addr pos;
	size_t size;

	size = MemoryLengthAB(array, body);
	Check(0xFFFFFFFFUL < size, "size error");
	allocheap(size, type, &pos, 0);
	SetStatus(pos, LISPSIZE_ARRAYBODY);
	nilarray4(pos, array);
	*PtrLenArrayAB(pos) = array;
	*PtrLenBodyAB(pos) = body;
	*ret = pos;
}

void heap_array(addr *ret, enum LISPTYPE type, size_t array)
{
#ifdef LISP_ARCH_64BIT
	if (MemoryLengthA2(array) <= 0xFFFFUL)
		heap_array2_memory(ret, type, (byte16)array);
	else if (MemoryLengthA4(array) <= 0xFFFFFFFFUL)
		heap_array4_memory(ret, type, (byte32)array);
	else
		heap_array8(ret, type, array);
#else
	if (MemoryLengthA2(array) <= 0xFFFFUL)
		heap_array2_memory(ret, type, (byte16)array);
	else
		heap_array4_memory(ret, type, array);
#endif
}

void heap_body(addr *ret, enum LISPTYPE type, size_t body)
{
#ifdef LISP_ARCH_64BIT
	if (MemoryLengthB2(body) <= 0xFFFFUL)
		heap_body2_memory(ret, type, (byte16)body);
	else if (MemoryLengthB4(body) <= 0xFFFFFFFFUL)
		heap_body4_memory(ret, type, (byte32)body);
	else
		heap_body8(ret, type, body);
#else
	if (MemoryLengthB2(body) <= 0xFFFFUL)
		heap_body2_memory(ret, type, (byte16)body);
	else
		heap_body4_memory(ret, type, body);
#endif
}

#ifdef LISP_DEBUG
void heap_array2_debug(addr *root, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFUL < array, "size error");
	heap_array2_memory(root, type, (byte16)array);
}
void heap_array4_debug(addr *root, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	heap_array4_memory(root, type, (byte32)array);
}
void heap_body2_debug(addr *root, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFUL < body, "size error");
	heap_body2_memory(root, type, (byte16)body);
}
void heap_body4_debug(addr *root, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFFFFFUL < body, "size error");
	heap_body4_memory(root, type, (byte32)body);
}
void heap_smallsize_debug(addr *root, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFUL < array, "array size error");
	Check(0xFFUL < body, "body size error");
	heap_smallsize_memory(root, type, (byte)array, (byte)body);
}
void heap_arraybody_debug(addr *root, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFFFUL < array, "array size error");
	Check(0xFFFFUL < body, "body size error");
	heap_arraybody_memory(root, type, (byte16)array, (byte16)body);
}
#endif


/*
 *  initialize
 */
void init_heap(void)
{
	init_heap_core();
}


/************************************************************
 *  heap_core.c
 ************************************************************/

/*
 *  core
 */
#define IfWriteCheck(fm,p,s,m) IfDebug(writecheck_filememory((fm),(p),(s)),(m))
#define IfWriteAddr(fm,p,m) IfDebug(writeaddr_filememory((fm),(p)),(m))
#define IfWritePtr(fm,p,m) IfDebug(writeptr_filememory((fm),(p)),(m))
#define IfWriteSize(fm,p,m) IfDebug(writesize_filememory((fm),(p)),(m))
#define IfReadCheck(fm,p,s,m) IfDebug(readcheck_filememory((fm),(p),(s)),(m))
#define IfReadAddr(fm,p,m) IfDebug(readaddr_filememory((fm),(p)),(m))
#define IfReadPtr(fm,p,m) IfDebug(readptr_filememory((fm),(p)),(m))
#define IfReadSize(fm,p,m) IfDebug(readsize_filememory((fm),(p)),(m))

/* save/load array2 */
static int writearray(filestream fm, const addr *array, size_t size)
{
	size_t i;

	for (i = 0; i < size; i++) {
		IfWriteAddr(fm, array[i], "writeaddr error.");
	}

	return 0;
}
static int readarray(filestream fm, addr *array, size_t size)
{
	size_t i;

	for (i = 0; i < size; i++) {
		IfReadAddr(fm, array + i, "readaddr error.");
	}

	return 0;
}

static int save_object_array2(filestream fm, addr pos)
{
	addr *array;
	size_t size;

	array = PtrArrayA2(pos);
	size = (size_t)GetLenArrayA2(pos);
	IfDebug(writearray(fm, array, size), "writearray error.");

	return 0;
}
static int load_object_array2(filestream fm, addr pos)
{
	addr *array;
	size_t size;

	array = PtrArrayA2(pos);
	size = (size_t)GetLenArrayA2(pos);
	IfDebug(readarray(fm, array, size), "readarray error.");

	return 0;
}


/* save/load array4 */
static int save_object_array4(filestream fm, addr pos)
{
	addr *array;
	size_t size;

	IfWriteCheck(fm, pos + 8UL, 8UL, "writecheck error.");
	array = PtrArrayA4(pos);
	size = (size_t)GetLenArrayA4(pos);
	IfDebug(writearray(fm, array, size), "writearray error.");

	return 0;
}
static int load_object_array4(filestream fm, addr pos)
{
	addr *array;
	size_t size;

	IfReadCheck(fm, pos + 8UL, 8UL, "readcheck error");
	array = PtrArrayA4(pos);
	size = (size_t)GetLenArrayA4(pos);
	IfDebug(readarray(fm, array, size), "readarray error.");

	return 0;
}


/* save/load array8 */
#ifdef LISP_ARCH_64BIT
static int save_object_array8(filestream fm, addr pos)
{
	addr *array;
	size_t size;

	IfWriteCheck(fm, pos + 8UL, 16UL, "writecheck error");
	array = PtrArrayA8(pos);
	size = (size_t)GetLenArrayA8(pos);
	IfDebug(writearray(fm, array, size), "writearray error.");

	return 0;
}
static int load_object_array8(filestream fm, addr pos)
{
	addr *array;
	size_t size;

	IfReadCheck(fm, pos + 8UL, 16UL, "readcheck error");
	array = PtrArrayA8(pos);
	size = (size_t)GetLenArrayA8(pos);
	IfDebug(readarray(fm, array, size), "readarray error.");

	return 0;
}
#else
static int save_object_array8(filestream fm, addr pos)
{
	Abort("Invalid object size: size8 [32bit]");
	return 1;
}
static int load_object_array8(filestream fm, addr pos)
{
	Abort("Invalid object size: size8 [32bit]");
	return 1;
}
#endif


/* save/load smallsize */
static int save_object_smallsize(filestream fm, addr pos)
{
	addr *array;
	byte *body;
	size_t size;

	/* array */
	array = PtrArraySS(pos);
	size = (size_t)GetLenArraySS(pos);
	IfDebug(writearray(fm, array, size), "writearray error.");
	/* body */
	body = PtrBodySSa(pos, size);
	size = (size_t)GetLenBodySS(pos);
	IfWriteCheck(fm, body, size, "writecheck error");

	return 0;
}
static int load_object_smallsize(filestream fm, addr pos)
{
	addr *array;
	byte *body;
	size_t size;

	/* array */
	array = PtrArraySS(pos);
	size = (size_t)GetLenArraySS(pos);
	IfDebug(readarray(fm, array, size), "readarray error.");
	/* body */
	body = PtrBodySSa(pos, size);
	size = (size_t)GetLenBodySS(pos);
	IfReadCheck(fm, body, size, "readcheck error");

	return 0;
}


/* save/load arraybody */
static int save_object_arraybody(filestream fm, addr pos)
{
	addr *array;
	byte *body;
	size_t size;

	IfWriteCheck(fm, pos + 8UL, 8UL, "writecheck error");
	/* array */
	array = PtrArrayAB(pos);
	size = (size_t)GetLenArrayAB(pos);
	IfDebug(writearray(fm, array, size), "writearray error.");
	/* body */
	body = PtrBodyABa(pos, size);
	size = (size_t)GetLenBodyAB(pos);
	IfWriteCheck(fm, body, size, "writecheck error");

	return 0;
}
static int load_object_arraybody(filestream fm, addr pos)
{
	addr *array;
	byte *body;
	size_t size;

	IfReadCheck(fm, pos + 8UL, 8UL, "readcheck error");
	/* array */
	array = PtrArrayAB(pos);
	size = (size_t)GetLenArrayAB(pos);
	IfDebug(readarray(fm, array, size), "readarray error.");
	/* body */
	body = PtrBodyABa(pos, size);
	size = (size_t)GetLenBodyAB(pos);
	IfReadCheck(fm, body, size, "readcheck error");

	return 0;
}


/* save/load body2 */
static int save_object_body2(filestream fm, addr pos)
{
	size_t size;

	size = (size_t)GetLenBodyB2(pos);
	IfWriteCheck(fm, pos + 8UL, size, "writecheck error");

	return 0;
}
static int load_object_body2(filestream fm, addr pos)
{
	size_t size;

	size = (size_t)GetLenBodyB2(pos);
	IfReadCheck(fm, pos + 8UL, size, "readcheck error");

	return 0;
}


/* save/load body4 */
static int save_object_body4(filestream fm, addr pos)
{
	size_t size;

	size = (size_t)GetLenBodyB4(pos);
	IfWriteCheck(fm, pos + 8UL, 8UL + size, "writecheck error.");

	return 0;
}
static int load_object_body4(filestream fm, addr pos)
{
	size_t size;

	size = (size_t)GetLenBodyB4(pos);
	IfReadCheck(fm, pos + 8UL, 8UL + size, "readcheck error");

	return 0;
}


/* save/load body8 */
#ifdef LISP_ARCH_64BIT
static int save_object_body8(filestream fm, addr pos)
{
	size_t size;

	size = (size_t)GetLenBodyB8(pos);
	IfWriteCheck(fm, pos + 8UL, 16UL + size, "writecheck error");

	return 0;
}
static int load_object_body8(filestream fm, addr pos)
{
	size_t size;

	IfReadCheck(fm, pos + 8UL, 16UL, "readcheck error");
	size = (size_t)GetLenBodyB8(pos);
	IfReadCheck(fm, pos + 24UL, size, "readcheck error");

	return 0;
}
#else
static int save_object_body8(filestream fm, addr pos)
{
	Abort("Invalid object size: size8 [32bit]");
	return 1;
}
static int load_object_body8(filestream fm, addr pos)
{
	Abort("Invalid object size: size8 [32bit]");
	return 1;
}
#endif


/* save/load object */
typedef int (*save_object_call)(filestream , addr);
typedef int (*load_object_call)(filestream , addr);
static save_object_call Heap_SaveObject[LISPSIZE_SIZE];
static load_object_call Heap_LoadObject[LISPSIZE_SIZE];

static int save_object(filestream fm, addr pos, size_t *ret)
{
	unsigned index;

	*ret = getobjectlength(pos);
	IfWriteCheck(fm, pos, 8UL, "writecheck error: save_object");
	index = (unsigned)GetStatusSize(pos);
	Check(LISPSIZE_SIZE <= index, "size error");
	return (Heap_SaveObject[index])(fm, pos);
}
static int load_object(filestream fm, addr pos, size_t *ret)
{
	unsigned index;

	IfReadCheck(fm, pos + 1UL, 7UL, "readcheck error: load_object");
	index = (unsigned)GetStatusSize(pos);
	Check(LISPSIZE_SIZE <= index, "size error");
	if ((Heap_LoadObject[index])(fm, pos)) {
		Debug("Heap_LoadObject error");
		return 1;
	}
	*ret = getobjectlength(pos);

	return 0;
}


/* save/load space1 */
static int save_space1(filestream fm, addr pos, size_t *ret)
{
	GetSizeSpace1(pos, ret);
	IfWriteCheck(fm, pos, 2UL, "writecheck error.");
	return 0;
}
static int load_space1(filestream fm, addr pos, size_t *ret)
{
	IfDebug(getc_filememory(fm, pos + 1), "readcheck error.");
	GetSizeSpace1(pos, ret);
	return 0;
}


/* save/load space */
static int save_space(filestream fm, addr pos, size_t *ret)
{
	size_t size;

	GetSizeSpace(pos, ret);
	IfDebug(putc_filememory(fm, pos[0]), "putc error.");
	GetValueSpace(pos, &size);
	IfWriteCheck(fm, &size, IdxSize, "writecheck error.");
	return 0;
}
static int load_space(filestream fm, addr pos, size_t *ret)
{
	IfReadCheck(fm, pos + 8UL, IdxSize, "readcheck error.");
	GetSizeSpace(pos, ret);
	return 0;
}


/* save/load reserved */
static int save_reserved(filestream fm, addr pos, size_t *ret)
{
	size_t size;

	GetSizeReserved(pos, ret);
	IfDebug(putc_filememory(fm, pos[0]), "putc error.");
	GetValueReserved(pos, &size);
	IfWriteCheck(fm, &size, IdxSize, "writecheck error.");
	return 0;
}
static int load_reserved(filestream fm, addr pos, size_t *ret)
{
	IfReadCheck(fm, pos + 8UL, IdxSize, "readcheck error.");
	GetSizeReserved(pos, ret);
	return 0;
}


/* save-dump-stream */
static int save_object_stream(filestream fm, addr pos, size_t *ret)
{
	if (save_stream(pos)) {
		Debug("save_stream error");
		return 1;
	}
	if (save_object(fm, pos, ret)) {
		Debug("save_object error");
		return 1;
	}

	return 0;
}


/* save/load symstack */
static int save_symstack(filestream fm, addr pos, size_t *ret)
{
	*ret = getobjectlength(pos);
	IfWriteCheck(fm, pos, 16UL, "writecheck error: save_symstack");
	return 0;
}

static int load_symstack(filestream fm, addr pos, size_t *ret)
{
	addr *array;
	size_t size, i;

	IfReadCheck(fm, pos + 1UL, 15L, "readcheck error: load_symstack");
	array = PtrArrayA4(pos);
	size = (size_t)GetLenArrayA4(pos);
	for (i = 0; i < size; i++)
		array[i] = NULL;
	*ret = getobjectlength(pos);

	return 0;
}


/* save/load dump */
static int save_dump(filestream fm)
{
	addr pos;
	size_t size;

	for (pos = heap_root; pos < heap_front; pos += size) {
		switch (GetType(pos)) {
			case LISPSYSTEM_SPACE1:
				IfDebug(save_space1(fm, pos, &size), "save_space1 error.");
				break;

			case LISPSYSTEM_SPACE:
				IfDebug(save_space(fm, pos, &size), "save_space error.");
				break;

			case LISPSYSTEM_RESERVED:
				IfDebug(save_reserved(fm, pos, &size), "save_reserved error.");
				break;

			case LISPTYPE_STREAM:
				IfDebug(save_object_stream(fm, pos, &size), "save_object_stream error.");
				break;

			case LISPSYSTEM_SYMSTACK:
				IfDebug(save_symstack(fm, pos, &size), "save_symstack error.");
				break;

			default:
				IfDebug(save_object(fm, pos, &size), "save_object error.");
				break;
		}
	}

	/* END check */
	IfDebug(putc_filememory(fm, (byte)LISPSYSTEM_END), "putc error.");

	return 0;
}
static int load_dump(filestream fm)
{
	byte c;
	addr pos;
	size_t size;

	for (pos = heap_root; pos < heap_front; pos += size) {
		IfDebug(getc_filememory(fm, pos), "getc error.");
		switch (pos[0]) {
			case LISPSYSTEM_SPACE1:
				IfDebug(load_space1(fm, pos, &size), "load_space1 error.");
				break;

			case LISPSYSTEM_SPACE:
				IfDebug(load_space(fm, pos, &size), "load_space error.");
				break;

			case LISPSYSTEM_RESERVED:
				IfDebug(load_reserved(fm, pos, &size), "load_reserved error.");
				break;

			case LISPTYPE_CODE:
				IfDebug(load_object(fm, pos, &size), "load_object error.");
				IfDebug(load_store_push(pos), "load_store_push error.");
				break;

			case LISPSYSTEM_SYMSTACK:
				IfDebug(load_symstack(fm, pos, &size), "load_symstack error.");
				break;

			default:
				IfDebug(load_object(fm, pos, &size), "load_object error.");
				break;
		}
	}

	/* END check */
	IfDebug(getc_filememory(fm, &c), "getc error.");
	IfDebug(c != LISPSYSTEM_END, "end error.");

	return 0;
}


/* save/load info */
static int save_info(filestream fm)
{
	addr pos;
	struct heap_addr *str;

	/* tail */
	str = (struct heap_addr *)heap_range;
	str--;
	while (LessEqualPointer(heap_tail, str)) {
		IfWriteAddr(fm, str->pos, "writeaddr error: heap_addr.");
		str--;
	}
	pos = NULL;
	IfWriteCheck(fm, &pos, sizeoft(void *), "writecheck: null.");

	return 0;
}

static int load_info(filestream fm)
{
	addr pos;
	struct heap_addr *str;
	size_t i;

	for (i = 0; i < heap_count; i++) {
		IfReadAddr(fm, &pos, "readaddr error: heap_addr.");
		str = alloctail();
		str->pos = pos;
	}
	IfReadCheck(fm, &pos, PtrSize, "readcheck error: null.");
	if (pos != NULL) {
		Abort("load_info null error.");
	}

	return 0;
}


/* save/load data */
static int save_data(filestream fm)
{
	IfWriteSize(fm, heap_object, "writeptr error: heap_object");
	IfWriteSize(fm, heap_count, "writeptr error: heap_count");
	IfWriteSize(fm, heap_gc_count, "writeptr error: heap_gc_count");
	IfWriteSize(fm, heap_gc_partial, "writeptr error: heap_gc_partial");
	IfWriteSize(fm, heap_gc_full, "writeptr error: heap_gc_full");
	IfWritePtr(fm, heap_front, "writeptr error: heap_front");
	IfWritePtr(fm, heap_pos, "writeptr error: heap_pos");
	if (save_dump(fm)) {
		Debug("save_dump error");
		return 1;
	}

	return 0;
}
static int load_data(filestream fm)
{
	IfReadSize(fm, &heap_object, "readptr error: heap_object");
	IfReadSize(fm, &heap_count, "readptr error: heap_count");
	IfReadSize(fm, &heap_gc_count, "readptr error: heap_gc_count");
	IfReadSize(fm, &heap_gc_partial, "readptr error: heap_gc_partial");
	IfReadSize(fm, &heap_gc_full, "readptr error: heap_gc_full");
	IfReadPtr(fm, (void **)&heap_front, "readptr error: heap_front");
	IfReadPtr(fm, (void **)&heap_pos, "readptr error: heap_pos");
	if (load_dump(fm)) {
		Debug("load_dump error");
		return 1;
	}

	return 0;
}


/* save/load info */
int save_heap(filestream fm)
{
	if (save_data(fm)) {
		Debug("save_data error.");
		return 1;
	}
	if (save_info(fm)) {
		Debug("save_info error.");
		return 1;
	}

	return 0;
}
int load_heap(filestream fm)
{
	if (load_store_init()) {
		Debug("load_store_init error.");
		return 1;
	}
	if (load_data(fm)) {
		Debug("load_data error.");
		load_store_error();
		return 1;
	}
	if (load_info(fm)) {
		Debug("load_info error.");
		load_store_error();
		return 1;
	}
	load_store_exec();

	return 0;
}


/*
 *  initialize
 */
void init_heap_core(void)
{
	/* save-object */
	Heap_SaveObject[LISPSIZE_ARRAY2] = save_object_array2;
	Heap_SaveObject[LISPSIZE_ARRAY4] = save_object_array4;
	Heap_SaveObject[LISPSIZE_ARRAY8] = save_object_array8;
	Heap_SaveObject[LISPSIZE_SMALLSIZE] = save_object_smallsize;
	Heap_SaveObject[LISPSIZE_ARRAYBODY] = save_object_arraybody;
	Heap_SaveObject[LISPSIZE_BODY2] = save_object_body2;
	Heap_SaveObject[LISPSIZE_BODY4] = save_object_body4;
	Heap_SaveObject[LISPSIZE_BODY8] = save_object_body8;
	/* load-object */
	Heap_LoadObject[LISPSIZE_ARRAY2] = load_object_array2;
	Heap_LoadObject[LISPSIZE_ARRAY4] = load_object_array4;
	Heap_LoadObject[LISPSIZE_ARRAY8] = load_object_array8;
	Heap_LoadObject[LISPSIZE_SMALLSIZE] = load_object_smallsize;
	Heap_LoadObject[LISPSIZE_ARRAYBODY] = load_object_arraybody;
	Heap_LoadObject[LISPSIZE_BODY2] = load_object_body2;
	Heap_LoadObject[LISPSIZE_BODY4] = load_object_body4;
	Heap_LoadObject[LISPSIZE_BODY8] = load_object_body8;
}


/************************************************************
 *  heap_memory.c
 ************************************************************/

/*
 *  varibales
 */
static size_t heap_size_init = 0;
static size_t heap_size = 0;
static addr   heap_front_max = 0;
static size_t heap_GcCounter = 0;
static addr   heap_GcCheck1 = 0;
static addr   heap_GcCheck2 = 0;
static addr   heap_GcCheck3 = 0;
static addr   heap_GcCheck4 = 0;


/*
 *  memory overflow
 */
static void memoryerror_heap(void)
{
	Debug("heap memory overflow.");
	abort_execute();
}


/*
 *  allocate front
 */
static int allocfront_size(addr pos, size_t *ret)
{
	enum LISPTYPE type;
	addr now;
	size_t size, value;

	size = 0;
	now = pos;
	for (;;) {
		if (heap_front <= now) {
			*ret = 0;
			return 1;
		}
		type = (enum LISPTYPE)GetType(now);
		if (type == LISPSYSTEM_SPACE1) {
			GetSizeSpace1(now, &value);
		}
		else if (type == LISPSYSTEM_SPACE) {
			GetSizeSpace(now, &value);
		}
		else {
			break;
		}
		size += value;
		now += value;
	}

	if (size == 0) {
		*ret = getobjectlength(pos);
		return 0;
	}
	else {
		*ret = size;
		return 1;
	}
}

static addr allocfront_search(addr pos, size_t *ret)
{
	size_t size;

	for (;;) {
		/* object */
		if (allocfront_size(pos, &size) == 0) {
			pos += size;
			continue;
		}
		/* expand */
		if (size == 0) {
			heap_front = pos;
			return NULL;
		}
		/* space */
		break;
	}
	*ret = size;
	return pos;
}

void makespace_heap(addr pos, size_t size)
{
	Check(size < 2, "size error");
	if (size < (8UL + IdxSize)) {
#ifdef LISP_DEBUG_MEMORY
		memset(pos, 0xAA, size);
#endif
		SetType(pos, LISPSYSTEM_SPACE1);
		SetSizeSpace1(pos, size);
	}
	else {
#ifdef LISP_DEBUG_MEMORY
		memset(pos, 0xAA, size);
#endif
		SetType(pos, LISPSYSTEM_SPACE);
		SetSizeSpace(pos, size);
	}
}

static void writereserved_heap(addr pos, size_t size, size_t check)
{
	/*
	 * |---------------| check
	 * |--------|        size
	 * [xxxxxxxx][space]
	 */
	Check(check < size, "writereserver error");
	heap_pos = pos + size;
	check -= size;
	if (check)
		makespace_heap(heap_pos, check);
}

static addr allocfront_expand(size_t size)
{
	addr check, result;

	check = heap_front + size;
	if (heap_tail < check) {
		Debug("allocfront_expand error");
		return NULL;
	}
	result = heap_front;
	heap_pos = heap_front = check;
	if (heap_front_max < heap_front)
		heap_front_max = heap_front;

	return result;
}

static addr allocfront_object(size_t size)
{
	addr pos;
	size_t check;

	CheckAlignSize8(size, "alignsize8 error");
	pos = heap_pos;
	for (;;) {
		pos = allocfront_search(pos, &check);
		if (pos == NULL) {
			pos = allocfront_expand(size);
			if (pos == NULL)
				return NULL;
			break;
		}
		if (size <= check) {
			writereserved_heap(pos, size, check);
			break;
		}
		pos += check;
	}
#ifdef LISP_DEBUG
	memset(pos, 0xAA, size);
#endif

	return pos;
}

/*
 *  gccheck
 *   0xF0	93%
 *  *0xE0	87%		5bit	32tims
 *   0xD0	81%
 *  *0xC0	75%		6bit	64tims
 *   0xB0	68%
 *  *0xA0	62%		7bit	128tims
 *   0x90	56%
 *  *0x80	50%		8bit	256tims
 */
static void gccheck_execute_heap(void)
{
	heap_GcCounter = 0;
	gcstate_execute(GcMode_Default);
}

static void gccheck_heap(void)
{
	if (lisp_gcsync != GcMode_Off)
		return;
	heap_GcCounter++;

	/* heap_GcCheck1 */
	if (heap_pos < heap_GcCheck1)
		return;
	if (heap_GcCounter & (1UL << 8UL)) {
		gccheck_execute_heap();
		if (lisp_gcsync != GcMode_Off)
			return;
	}

	/* heap_GcCheck2 */
	if (heap_pos < heap_GcCheck2)
		return;
	if (heap_GcCounter & (1UL << 7UL)) {
		gccheck_execute_heap();
		if (lisp_gcsync != GcMode_Off)
			return;
	}

	/* heap_GcCheck3 */
	if (heap_pos < heap_GcCheck3)
		return;
	if (heap_GcCounter & (1UL << 6UL)) {
		gccheck_execute_heap();
		if (lisp_gcsync != GcMode_Off)
			return;
	}

	/* heap_GcCheck4 */
	if (heap_pos < heap_GcCheck4)
		return;
	if (heap_GcCounter & (1UL << 5UL)) {
		gccheck_execute_heap();
		if (lisp_gcsync != GcMode_Off)
			return;
	}
}

static addr allocfront(size_t size)
{
	addr ret;

	ret = allocfront_object(size);
	gccheck_heap();
	if (ret == NULL) {
		memoryerror_heap();
		return NULL;
	}

	return ret;
}


/*
 *  allocate tail
 */
#define alloctail_size		(sizeoft(struct heap_addr))
struct heap_addr *alloctail(void)
{
	addr check;

	check = heap_tail - alloctail_size;
	if (check < heap_front) {
		memoryerror_heap();
		return NULL;
	}
	heap_tail = check;
#ifdef LISP_DEBUG
	memset(heap_tail, 0xAA, alloctail_size);
#endif

	return (struct heap_addr *)heap_tail;
}


/*
 *  allocate
 */
static void allocheap_object(size_t size, addr *ret)
{
	addr pos;
	struct heap_addr *info;

	/* front */
	pos = allocfront(size);
	/* tail */
	info = alloctail();
	info->pos = pos;
	/* result */
	*ret = pos;
}

void allocheap(size_t size, enum LISPTYPE type, addr *root, int size2)
{
	addr pos;

	/* alloc */
	AlignSize8Front(size, &size);
	allocheap_object(size, &pos);
	heap_object += size;
	heap_count++;

	/* initialize */
	SetType(pos, (byte)type);
	SetChain(pos, 0);
	if (size2)
		*PtrValue2L(pos) = (byte16)size;
	else
		*PtrValueL(pos) = size;
	*root = pos;
}


/*
 *  init heap
 */
static void tailheap(void)
{
	heap_tail = (addr)Align8Cut(heap_size + (uintptr_t)heap_root);
	heap_range = heap_tail;
}

static void frontheap(void *ptr, size_t size)
{
	int align;
	size_t q;

	/* memory */
	align = Align8Space(ptr);
	heap_size = size - align;
	heap_pos = heap_front = heap_front_max = heap_root = align + (addr)ptr;
	heap_object = 0;
	heap_count = 0;
	heap_gc_count = 0;
	CheckAlign8(heap_pos, "align8 error");

	/* gccheck */
	q = heap_size / 0x10;
	heap_GcCheck1 = heap_root + (q * 0x08);
	heap_GcCheck2 = heap_root + (q * 0x0A);
	heap_GcCheck3 = heap_root + (q * 0x0C);
	heap_GcCheck4 = heap_root + (q * 0x0E);
	heap_GcCounter = 0;
}

int alloc_heap(size_t size)
{
	void *ptr;

	if (heap_alloc) {
		Debug("heap memory already allocated.");
		return 1;
	}
	if (size < 1000UL * 1000UL) {
		Debug("heap size must be greater than 1MByte.");
		return 1;
	}
	heap_size_init = size;

	ptr = malloc(size);
	if (ptr == NULL) {
		Debug("malloc error");
		return 1;
	}
#ifdef LISP_MEMORY_INIT
	memset(ptr, 0xAA, size);
#endif

	/* make front */
	frontheap(ptr, size);

	/* make tail */
	tailheap();

	/* result */
	heap_alloc = ptr;
	return 0;
}

static void free_value_heap(void)
{
	heap_root = 0;
	heap_front = 0;
	heap_pos = 0;
	heap_tail = 0;
	heap_range = 0;
	heap_object = 0;
	heap_count = 0;
	heap_gc_count = 0;
	heap_gc_partial = 0;
	heap_gc_full = 0;
	heap_cons_count = 0;
	heap_symbol_count = 0;
	heap_size = 0;
	heap_size_init = 0;
	heap_front_max = 0;
	heap_GcCounter = 0;
	heap_GcCheck1 = 0;
	heap_GcCheck2 = 0;
	heap_GcCheck3 = 0;
	heap_GcCheck4 = 0;
	heap_alloc = 0;
}

void free_heap(void)
{
	if (heap_alloc) {
		free(heap_alloc);
		free_value_heap();
	}
}

void reload_heap(void)
{
	void *ptr;
	size_t size;

	if (heap_alloc == NULL)
		return;

	/* clear */
	size = heap_size_init;
	ptr = heap_alloc;
	free_value_heap();
	heap_size_init = size;
#ifdef LISP_MEMORY_INIT
	memset(ptr, 0xAA, size);
#endif

	/* make */
	frontheap(ptr, size);
	tailheap();
	heap_alloc = ptr;
}


/*
 *  gc
 */
int valid_heap(const void *pos)
{
	return (heap_root <= (addr)pos) && ((addr)pos <= heap_front);
}

size_t get_heap_object(void)
{
	return heap_object;
}

size_t get_heap_count(void)
{
	return heap_count;
}

size_t get_heap_gc_count(void)
{
	return heap_gc_count;
}

size_t get_heap_size(void)
{
	return heap_size;
}


/************************************************************
 *  hold.c
 ************************************************************/

/*
 *  gchold
 */
static int setgchold_p(addr pos)
{
#ifdef LISP_DEBUG
	Check((pos != Unbound && pos[0] == 0xAA), "break local memory.");
#endif
	return (pos != Unbound) && (! GetStatusDynamic(pos));
}

void setgchold(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_GCHOLD);
	Check(! setgchold_p(value), "gchold error");
	SetArrayA2(pos, index, value);
}

void gchold_local(LocalRoot local, addr *ret, size_t size)
{
	Check(0xFFFFUL <= size, "size error");
	local_array2(local, ret, LISPSYSTEM_GCHOLD, (byte16)size);
}

static void gchold_heap(addr *ret, size_t size)
{
	Check(0xFFFFUL <= size, "size error");
	heap_array2(ret, LISPSYSTEM_GCHOLD, (byte16)size);
}

void gchold_push_local(LocalRoot local, addr pos)
{
	addr array;
	gchold_local(local, &array, 1);
	setgchold(array, 0, pos);
}

void gchold_push_force_local(LocalRoot local, addr pos)
{
	if (setgchold_p(pos))
		gchold_push_local(local, pos);
}

static void gchold_pushva_stdarg(LocalRoot local, va_list args)
{
	addr pos, array;
	size_t size, i;
	va_list dest;

	/* index */
	va_copy(dest, args);
	for (size = 0; ; size++) {
		pos = va_arg(dest, addr);
		if (pos == NULL)
			break;
	}

	/* make */
	gchold_local(local, &array, size);
	for (i = 0; ; i++) {
		pos = va_arg(args, addr);
		if (pos == NULL)
			break;
		Check(size <= i, "size error");
		setgchold(array, i, pos);
	}
	Check(size != i, "size error");
}

void gchold_pushva_local(LocalRoot local, ...)
{
	va_list args;

	va_start(args, local);
	gchold_pushva_stdarg(local, args);
	va_end(args);
}

static void gchold_pushva_force_stdarg(LocalRoot local, va_list args)
{
	addr pos, array;
	size_t size, i;
	va_list dest;

	/* index */
	va_copy(dest, args);
	size = 0;
	for (;;) {
		pos = va_arg(dest, addr);
		if (pos == NULL)
			break;
		if (setgchold_p(pos))
			size++;
	}

	/* make */
	gchold_local(local, &array, size);
	i = 0;
	for (;;) {
		pos = va_arg(args, addr);
		if (pos == NULL)
			break;
		if (setgchold_p(pos)) {
			Check(size <= i, "size error");
			setgchold(array, i, pos);
			i++;
		}
	}
	Check(size != i, "size error");
}

void gchold_pushva_force_local(LocalRoot local, ...)
{
	va_list args;

	va_start(args, local);
	gchold_pushva_force_stdarg(local, args);
	va_end(args);
}

static void gchold_special(Execute ptr, addr value)
{
	addr symbol, pos;

	GetConst(SYSTEM_GCHOLD, &symbol);
	if (existspecial_control(ptr, symbol)) {
		getspecial_local(ptr, symbol, &pos);
		cons_heap(&value, value, (pos == Unbound)? Nil: pos);
		setspecial_local(ptr, symbol, value);
	}
	else {
		conscar_heap(&value, value);
		pushspecial_control(ptr, symbol, value);
	}
}

void gchold_push_special(Execute ptr, addr pos)
{
	addr array;
	gchold_heap(&array, 1);
	setgchold(array, 0, pos);
	gchold_special(ptr, array);
}

void gchold_pushva_special(Execute ptr, ...)
{
	addr pos, array;
	size_t size, i;
	va_list args, dest;

	/* index */
	va_start(args, ptr);
	va_copy(dest, args);
	for (size = 0; ; size++) {
		pos = va_arg(dest, addr);
		if (pos == NULL)
			break;
	}

	/* make */
	gchold_heap(&array, size);
	for (i = 0; ; i++) {
		pos = va_arg(args, addr);
		if (pos == NULL)
			break;
		Check(size <= i, "size error");
		setgchold(array, i, pos);
	}
	Check(size != (i + 1UL), "size error");
	va_end(args);

	/* push */
	gchold_special(ptr, array);
}


/*
 *  gchold
 */
LocalHold localhold_local(LocalRoot local)
{
	LocalStack stack;
	LocalHold ptr;

	push_local(local, &stack);
	ptr = (LocalHold)lowlevel_local(local, sizeoft(struct localhold));
	ptr->local = local;
	ptr->stack = stack;
	ptr->array = Nil;

	return ptr;
}

LocalHold localhold_local_push(LocalRoot local, addr pos)
{
	LocalHold hold;

	hold = localhold_local(local);
	localhold_push(hold, pos);

	return hold;
}

void localhold_push(LocalHold hold, addr pos)
{
	if (pos != Nil && pos != Unbound && pos != NULL)
		gchold_push_local(hold->local, pos);
}

void localhold_pushva(LocalHold hold, ...)
{
	va_list args;

	va_start(args, hold);
	gchold_pushva_stdarg(hold->local, args);
	va_end(args);
}

void localhold_pushva_force(LocalHold hold, ...)
{
	va_list args;

	va_start(args, hold);
	gchold_pushva_force_stdarg(hold->local, args);
	va_end(args);
}

LocalHold localhold_array(LocalRoot local, size_t size)
{
	LocalHold hold;

	hold = localhold_local(local);
	gchold_local(local, &(hold->array), size);

	return hold;
}

void localhold_end(LocalHold hold)
{
	rollback_local(hold->local, hold->stack);
}

void localhold_set(LocalHold hold, size_t index, addr value)
{
	CheckType(hold->array, LISPSYSTEM_GCHOLD);
	setgchold(hold->array, index, value);
}

void localhold_set_force(LocalHold hold, size_t index, addr value)
{
	CheckType(hold->array, LISPSYSTEM_GCHOLD);
	if (setgchold_p(value))
		setgchold(hold->array, index, value);
}


/*
 *  hold object
 */
void Hold_local(addr *ret, addr value)
{
	hold_value(value, &value);
	hold_local(Local_Thread, ret, value);
}

void hold_local(LocalRoot local, addr *ret, addr value)
{
	hold_value(value, &value);
	local_array2(local, ret, LISPSYSTEM_HOLD, 1);
	SetArrayA2(*ret, 0, value);
}

int holdp(addr pos)
{
	return pos && (pos != Unbound) && GetType(pos) == LISPSYSTEM_HOLD;
}

void hold_set(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_HOLD);
	hold_value(value, &value);
	SetArrayA2(pos, 0, value);
}

void hold_set_null(addr pos, addr value)
{
	if (pos) {
		CheckType(pos, LISPSYSTEM_HOLD);
		hold_value(value, &value);
		SetArrayA2(pos, 0, value);
	}
}

void hold_get(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_HOLD);
	GetArrayA2(pos, 0, ret);
}

void hold_value(addr pos, addr *ret)
{
	if (holdp(pos))
		GetArrayA2(pos, 0, ret);
	else
		*ret = pos;
}

addr holdv(addr pos)
{
	if (holdp(pos)) {
		GetArrayA2(pos, 0, &pos);
		return pos;
	}
	else {
		return pos;
	}
}


/************************************************************
 *  info.c
 ************************************************************/

#define INFO_STREAM		stdout
#define INFO_DEPTH		20

static void infobit_body(addr pos);
static void infoprint_stream(addr pos, int depth);
static FILE *InfoStream = NULL;
static int InfoDepth = INFO_DEPTH;


/*
 *  object string
 */
static const char *infochar_lisp(enum LISPTYPE type)
{
	switch (type) {
		case LISPTYPE_NIL:					return "nil";
		case LISPTYPE_T:					return "t";
		case LISPTYPE_TYPE:					return "type";
		case LISPTYPE_CLOS:					return "clos";
		case LISPTYPE_CONS:					return "cons";
		case LISPTYPE_ARRAY:				return "array";
		case LISPTYPE_VECTOR:				return "vector";
		case LISPTYPE_CHARACTER:			return "character";
		case LISPTYPE_STRING:				return "string";
		case LISPTYPE_HASHTABLE:			return "hashtable";
		case LISPTYPE_READTABLE:			return "readtable";
		case LISPTYPE_SYMBOL:				return "symbol";
		case LISPTYPE_FIXNUM:				return "fixnum";
		case LISPTYPE_BIGNUM:				return "bignum";
		case LISPTYPE_RATIO:				return "ratio";
		case LISPTYPE_SHORT_FLOAT:			return "short-float";
		case LISPTYPE_SINGLE_FLOAT:			return "single-float";
		case LISPTYPE_DOUBLE_FLOAT:			return "double-float";
		case LISPTYPE_LONG_FLOAT:			return "long-float";
		case LISPTYPE_COMPLEX:				return "complex";
		case LISPTYPE_CONTROL:				return "control";
		case LISPTYPE_CODE:					return "code";
		case LISPTYPE_CALLNAME:				return "callname";
		case LISPTYPE_FUNCTION:				return "function";
		case LISPTYPE_INDEX:				return "index";
		case LISPTYPE_PACKAGE:				return "package";
		case LISPTYPE_RANDOM_STATE:			return "random-state";
		case LISPTYPE_PATHNAME:				return "pathname";
		case LISPTYPE_STREAM:				return "stream";
		case LISPTYPE_QUOTE:				return "quote";
		case LISPTYPE_RESTART:				return "restart";
		case LISPTYPE_EVAL:					return "eval";
		case LISPTYPE_ENVIRONMENT:			return "environment";
		case LISPTYPE_BITVECTOR:			return "bitvector";
		case LISPTYPE_PRINT_DISPATCH:		return "print-dispatch";
		case LISPTYPE_BYTESPEC:				return "bytespec";
		case LISPTYPE_FORMAT:				return "format";
		case LISPTYPE_LOAD_TIME_VALUE:		return "load-time-value";
		case LISPTYPE_PAPER:				return "paper";
		case LISPTYPE_COMPILE:				return "??compile";

		case LISPSYSTEM_CONSTANT:			return "?constant";
		case LISPSYSTEM_FIXNUM_CACHE:		return "?fixnum-cache";
		case LISPSYSTEM_CHARACTER_CACHE:	return "?character-cache";
		case LISPSYSTEM_BIGBUFFER:			return "?bigbuffer";
		case LISPSYSTEM_BIGCONS:			return "?bigcons";
		case LISPSYSTEM_BIGDATA:			return "?bigdata";
		case LISPSYSTEM_CHARACTER2:			return "?character2";
		case LISPSYSTEM_CHARQUEUE:			return "?charqueue";
		case LISPSYSTEM_CHARBIT:			return "?charbit";
		case LISPSYSTEM_SYMSTACK:			return "?symstack";
		case LISPSYSTEM_BITTYPE:			return "?bittype";
		case LISPSYSTEM_READLABEL:			return "?readlabel";
		case LISPSYSTEM_READINFO:			return "?readinfo";
		case LISPSYSTEM_READTYPE:			return "?readtype";
		case LISPSYSTEM_BITCONS:			return "?bitcons";
		case LISPSYSTEM_BITBUFFER:			return "?bitbuffer";
		case LISPSYSTEM_HASHITERATOR:		return "?hash-iterator";
		case LISPSYSTEM_PACKAGEITERATOR:	return "?package-iterator";
		case LISPSYSTEM_TAGINFO:			return "?taginfo";
		case LISPSYSTEM_ARRAY_DIMENSION:	return "?array-dimension";
		case LISPSYSTEM_ARRAY_GENERAL:		return "?array-general";
		case LISPSYSTEM_ARRAY_SPECIALIZED:	return "?array-specialized";
		case LISPSYSTEM_CODE:				return "?code";
		case LISPSYSTEM_PROMPT:				return "?prompt";
		case LISPSYSTEM_ENVROOT:			return "?envroot";
		case LISPSYSTEM_ENVSTACK:			return "?envstack";
		case LISPSYSTEM_SLOT:				return "?slot";
		case LISPSYSTEM_SLOT_VECTOR:		return "?slot-vector";
		case LISPSYSTEM_CLOS_VALUE:			return "?clos-value";
		case LISPSYSTEM_GENERIC:			return "?generic";
		case LISPSYSTEM_ARGUMENT:			return "?argument";
		case LISPSYSTEM_UNICODE:			return "?unicode";
		case LISPSYSTEM_TYPE_PARSE:			return "?type-parse";
		case LISPSYSTEM_STRUCTURE:		    return "?structure";
		case LISPSYSTEM_STRUCTURE_TYPE:		return "?structure-type";
		case LISPSYSTEM_PRINT_TABLE:		return "?print-table";
		case LISPSYSTEM_PRINT_WRITE:		return "?print-write";
		case LISPSYSTEM_PRINT_CHECK:		return "?print-check";
		case LISPSYSTEM_PRINT_PRETTY:		return "?print-pretty";
		case LISPSYSTEM_EVALSTACK:			return "?eval-stack";
		case LISPSYSTEM_GCHOLD:				return "?gchold";
		case LISPSYSTEM_FORMAT_PRETTY:		return "?format-pretty";
		case LISPSYSTEM_SLEEP:				return "?sleep";
		case LISPSYSTEM_REDEFINE:			return "?redefine";
		case LISPSYSTEM_HANDLER:			return "?handler";
		case LISPSYSTEM_SPECIAL:			return "?special";
		case LISPSYSTEM_VALUES:				return "?values";
		case LISPSYSTEM_EXECUTE:			return "?execute";
		case LISPSYSTEM_LEXICAL:			return "?lexical";
		case LISPSYSTEM_CLOSURE:			return "?closure";
		case LISPSYSTEM_REFERENCE:			return "?reference";
		case LISPSYSTEM_HOLD:				return "?hold";
		case LISPSYSTEM_BUFFERING:			return "?buffering";
		case LISPSYSTEM_BUFCELL:			return "?bufcell";
		case LISPSYSTEM_CHECK:				return "?check";

		case LISPSYSTEM_UNBOUND:			return "unboundtype";
		case LISPSYSTEM_SPACE:				return "space";
		case LISPSYSTEM_SPACE1:				return "space1";
		case LISPSYSTEM_RESERVED:			return "reserved";
		case LISPSYSTEM_END:				return "end";
		default:							return "error";
	}
}

static const char *infochar_size(enum LISPSIZE size)
{
	switch (size) {
		case LISPSIZE_ARRAY2:		return "array2";
		case LISPSIZE_ARRAY4:		return "array4";
		case LISPSIZE_ARRAY8:		return "array8";
		case LISPSIZE_SMALLSIZE:	return "smallsize";
		case LISPSIZE_ARRAYBODY:	return "arraybody";
		case LISPSIZE_BODY2:		return "body2";
		case LISPSIZE_BODY4:		return "body4";
		case LISPSIZE_BODY8:		return "body8";
		default:					return "error";
	}
}

static const char *infochar_decl(enum LISPDECL decl)
{
	switch (decl) {
		case LISPDECL_EMPTY:				return "empty";
		case LISPDECL_DELAY:				return "delay";
		case LISPDECL_OPTIMIZED:			return "optimized";
		case LISPDECL_SUBTYPEP:				return "subtypep";
		case LISPDECL_TYPE:					return "type";
		case LISPDECL_CLOS:					return "clos";
		case LISPDECL_ASTERISK:				return "*";

		case LISPDECL_AND:					return "and";
		case LISPDECL_OR:					return "or";
		case LISPDECL_EQL:					return "eql";
		case LISPDECL_MEMBER:				return "member";
		case LISPDECL_MOD:					return "mod";
		case LISPDECL_NOT:					return "not";
		case LISPDECL_SATISFIES:			return "satisfies";
		case LISPDECL_VALUES:				return "values";

		case LISPDECL_ATOM:					return "atom";
		case LISPDECL_LIST:					return "list";
		case LISPDECL_BOOLEAN:				return "boolean";
		case LISPDECL_VECTOR:				return "vector";
		case LISPDECL_SIMPLE_VECTOR:		return "simple-vector";
		case LISPDECL_BIT_VECTOR:			return "bit-vector";
		case LISPDECL_SIMPLE_BIT_VECTOR:	return "simple-bit-vector";
		case LISPDECL_EXTENDED_CHAR:		return "extended-char";
		case LISPDECL_STRING:				return "string";
		case LISPDECL_BASE_STRING:			return "base-string";
		case LISPDECL_SIMPLE_STRING:		return "simple-string";
		case LISPDECL_SIMPLE_BASE_STRING:	return "simple-base-string";
		case LISPDECL_SIGNED_BYTE:			return "signed-byte";
		case LISPDECL_UNSIGNED_BYTE:		return "unsigned-byte";
		case LISPDECL_BIT:					return "bit";
		case LISPDECL_FIXNUM:				return "fixnum";
		case LISPDECL_BIGNUM:				return "bignum";

		case LISPDECL_NIL:					return "nil";
		case LISPDECL_T:					return "t";
		case LISPDECL_NULL:					return "null";
		case LISPDECL_CONS:					return "cons";
		case LISPDECL_HASH_TABLE:			return "hash-table";
		case LISPDECL_SYMBOL:				return "symbol";
		case LISPDECL_KEYWORD:				return "keyword";
		case LISPDECL_PACKAGE:				return "package";
		case LISPDECL_RANDOM_STATE:			return "random-state";
		case LISPDECL_READTABLE:			return "readtable";
		case LISPDECL_FUNCTION:				return "function";
		case LISPDECL_COMPILED_FUNCTION:	return "compiled-function";
		case LISPDECL_PATHNAME:				return "pathname";
		case LISPDECL_LOGICAL_PATHNAME:		return "logical-pathname";
		case LISPDECL_SEQUENCE:				return "sequence";
		case LISPDECL_ARRAY:				return "array";
		case LISPDECL_SIMPLE_ARRAY:			return "simple-array";
		case LISPDECL_CHARACTER:			return "character";
		case LISPDECL_BASE_CHAR:			return "base-char";
		case LISPDECL_STANDARD_CHAR:		return "standard-char";
		case LISPDECL_NUMBER:				return "number";
		case LISPDECL_REAL:					return "real";
		case LISPDECL_RATIONAL:				return "rational";
		case LISPDECL_RATIO:				return "ratio";
		case LISPDECL_INTEGER:				return "integer";
		case LISPDECL_COMPLEX:				return "complex";
		case LISPDECL_FLOAT:				return "float";
		case LISPDECL_SHORT_FLOAT:			return "short-float";
		case LISPDECL_SINGLE_FLOAT:			return "single-float";
		case LISPDECL_DOUBLE_FLOAT:			return "double-float";
		case LISPDECL_LONG_FLOAT:			return "long-float";
		case LISPDECL_RESTART:				return "restart";
		case LISPDECL_ENVIRONMENT:			return "environment";
		case LISPDECL_STREAM:				return "stream";
		case LISPDECL_BROADCAST_STREAM:		return "broadcast-stream";
		case LISPDECL_CONCATENATED_STREAM:	return "concatenated-stream";
		case LISPDECL_ECHO_STREAM:			return "echo-stream";
		case LISPDECL_FILE_STREAM:			return "file-stream";
		case LISPDECL_STRING_STREAM:		return "string-stream";
		case LISPDECL_SYNONYM_STREAM:		return "synonym-stream";
		case LISPDECL_TWO_WAY_STREAM:		return "two-way-stream";
		case LISPDECL_PROMPT_STREAM:		return "prompt-stream";
		case LISPDECL_PRETTY_STREAM:		return "pretty-stream";
		case LISPDECL_MEMORY_STREAM:		return "memory-stream";
		case LISPDECL_PIPE_STREAM:		    return "pipe-stream";
		case LISPDECL_QUOTE:				return "quote";
		case LISPDECL_BYTESPEC:				return "bytespec";
		case LISPDECL_PRINT_DISPATCH:		return "print-dispatch";
		case LISPDECL_PAPER:				return "paper";
		case LISPDECL_EVAL:					return "eval";
		default:							return "invalid";
	}
}

static const char *infochar_eval_parse_type(enum EVAL_PARSE type)
{
	switch (type) {
		case EVAL_PARSE_EMPTY:					return "empty";
												/* constant */
		case EVAL_PARSE_NIL:					return "nil";
		case EVAL_PARSE_T:						return "t";
		case EVAL_PARSE_CLOS:					return "clos";
		case EVAL_PARSE_INTEGER:				return "integer";
		case EVAL_PARSE_RATIONAL:				return "rational";
		case EVAL_PARSE_COMPLEX:				return "complex";
		case EVAL_PARSE_CHARACTER:				return "character";
		case EVAL_PARSE_ARRAY:					return "array";
		case EVAL_PARSE_VECTOR:					return "vector";
		case EVAL_PARSE_BITVECTOR:				return "bit-vector";
		case EVAL_PARSE_STRING:					return "string";
		case EVAL_PARSE_SYMBOL:					return "symbol";
		case EVAL_PARSE_FLOAT:					return "float";
		case EVAL_PARSE_DECLAIM:				return "declaim";
		case EVAL_PARSE_PATHNAME:				return "pathname";
		case EVAL_PARSE_PACKAGE:				return "package";
		case EVAL_PARSE_RANDOM_STATE:			return "random-state";
		case EVAL_PARSE_ENVIRONMENT:			return "environment";
		case EVAL_PARSE_LEXICAL:				return "lexical";
												/* cons */
		case EVAL_PARSE_PROGN:					return "progn";
		case EVAL_PARSE_LET:					return "let";
		case EVAL_PARSE_LETA:					return "let*";
		case EVAL_PARSE_SETQ:					return "setq";
		case EVAL_PARSE_DEFUN:					return "defun";
		case EVAL_PARSE_DEFMACRO:				return "defmacro";
		case EVAL_PARSE_MACRO_LAMBDA:			return "macro-lambda";
		case EVAL_PARSE_DEFTYPE:				return "deftype";
		case EVAL_PARSE_DEFINE_COMPILER_MACRO:	return "define-compiler-macro";
		case EVAL_PARSE_DESTRUCTURING_BIND:		return "destructuring-bind";
		case EVAL_PARSE_QUOTE:					return "quote";
		case EVAL_PARSE_FUNCTION:				return "function";
		case EVAL_PARSE_LAMBDA:					return "lambda";
		case EVAL_PARSE_IF:						return "if";
		case EVAL_PARSE_UNWIND_PROTECT:			return "unwind-protect";
		case EVAL_PARSE_TAGBODY:				return "tagbody";
		case EVAL_PARSE_TAG:					return "tag";
		case EVAL_PARSE_GO:						return "go";
		case EVAL_PARSE_BLOCK:					return "block";
		case EVAL_PARSE_RETURN_FROM:			return "return-from";
		case EVAL_PARSE_CATCH:					return "catch";
		case EVAL_PARSE_THROW:					return "throw";
		case EVAL_PARSE_FLET:					return "flet";
		case EVAL_PARSE_LABELS:					return "labels";
		case EVAL_PARSE_THE:					return "the";
		case EVAL_PARSE_EVAL_WHEN:				return "eval-when";
		case EVAL_PARSE_VALUES:					return "values";
		case EVAL_PARSE_LOCALLY:				return "locally";
		case EVAL_PARSE_CALL:					return "call";
		case EVAL_PARSE_MULTIPLE_VALUE_BIND:	return "multiple-value-bind";
		case EVAL_PARSE_MULTIPLE_VALUE_CALL:	return "multiple-value-call";
		case EVAL_PARSE_MULTIPLE_VALUE_PROG1:	return "multiple-value-prog1";
		case EVAL_PARSE_NTH_VALUE:				return "nth-value";
		case EVAL_PARSE_PROGV:					return "progv";
		case EVAL_PARSE_LOAD_TIME_VALUE:		return "load-time-value";
		case EVAL_PARSE_STEP:					return "step";
		default:								return "invalid";
	}
}

static const char *infochar_eval_parse(addr pos)
{
	return infochar_eval_parse_type(RefEvalParseType(pos));
}

static const char *infochar_eval_scope(addr pos)
{
	return infochar_eval_parse_type(RefEvalScopeType(pos));
}

static const char *infochar_eval_stack(addr pos)
{
	switch (RefEvalStackType(pos)) {
		case EVAL_STACK_MODE_NIL:		return "nil";
		case EVAL_STACK_MODE_LAMBDA:	return "lambda";
		default:						return "invalid";
	}
}

static const char *infochar_eval(addr pos)
{
	switch (RefEvalType(pos)) {
		case EVAL_TYPE_DECLARE:			return "declare";
		case EVAL_TYPE_PARSE:			return "parse";
		case EVAL_TYPE_STACK:			return "stack";
		case EVAL_TYPE_SCOPE:			return "scope";
		case EVAL_TYPE_TABLE:			return "table";
		case EVAL_TYPE_TABLEVALUE:		return "value";
		case EVAL_TYPE_TABLEFUNCTION:	return "function";
		case EVAL_TYPE_TABLETAGBODY:	return "tagblody";
		case EVAL_TYPE_TABLEBLOCK:		return "block";
		case EVAL_TYPE_TABLECALL:		return "call";
		case EVAL_TYPE_CODE:			return "code";
		default:						return "invalid";
	}
}

static const char *infochar_array(enum ARRAY_TYPE type)
{
	switch (type) {
		case ARRAY_TYPE_EMPTY:			return "empty";
		case ARRAY_TYPE_T:				return "t";
		case ARRAY_TYPE_BIT:			return "bit";
		case ARRAY_TYPE_CHARACTER:		return "character";
		case ARRAY_TYPE_SIGNED:			return "signed";
		case ARRAY_TYPE_UNSIGNED:		return "unsigned";
		case ARRAY_TYPE_SINGLE_FLOAT:	return "single-float";
		case ARRAY_TYPE_DOUBLE_FLOAT:	return "double-float";
		case ARRAY_TYPE_LONG_FLOAT:		return "long-float";
		default:						return "error";
	}
}


/*
 *  info stream
 */
static enum LISPTYPE info_gettype(addr pos)
{
	return (pos == Unbound)? LISPSYSTEM_UNBOUND: GetType(pos);
}

static void info_flush(void)
{
	if (InfoStream == NULL)
		InfoStream = INFO_STREAM;
	fflush(InfoStream);
}

static void info_valist(const char *fmt, va_list args)
{
	if (InfoStream == NULL)
		InfoStream = INFO_STREAM;
	vfprintf(InfoStream, fmt, args);
}

static void info_stdarg(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	info_valist(fmt, args);
	va_end(args);
}

static void info_eol(void)
{
	info_stdarg("\n");
	info_flush();
}

static void info_start_valist(const char *fmt, va_list args)
{
	info_stdarg("[INFO] ");
	info_valist(fmt, args);
}

static void info_start(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	info_start_valist(fmt, args);
	va_end(args);
}

void info(const char *fmt, ...)
{
	va_list args;

	if (lisp_info_enable) {
		va_start(args, fmt);
		info_start_valist(fmt, args);
		va_end(args);
		info_eol();
	}
}

void info_noeol(const char *fmt, ...)
{
	va_list args;

	if (lisp_info_enable) {
		va_start(args, fmt);
		info_start_valist(fmt, args);
		va_end(args);
		info_flush();
	}
}

void infoerror(const char *first, int line, const char *func, const char *fmt, ...)
{
	va_list args;

	if (lisp_info_enable) {
		va_start(args, fmt);
		info_start("%s(%d) %s: ", first, line, func);
		info_valist(fmt, args);
		info_eol();
		va_end(args);
	}
}

static void infotime(void)
{
	char buffer[32];

	if (nowtime_string(buffer, 32)) {
		Abort("nowtime_string error");
		return;
	}
	info("  %s", buffer);
}

void infosystem(void)
{
	info("*** SYSTEM-INFORMATION BEGIN ***");
	infotime();
	info("*** SYSTEM-INFORMATION END ***");
}

static void infobit_dump(addr pos)
{
	int c;
	size_t m, i, len;
	pbyte body;
	char buffer[256];

	if (IsBody(pos)) {
		posbodylen(pos, &body, &len);
		for (i = 0; i < len; i++) {
			c = RdByte(body + i);
			m = i % 16;
			if (i && m == 0) {
				buffer[16] = 0;
				info("%s", buffer);
			}
			buffer[m] = isstandardtype(c)? c: '.';
		}
		info("%s", buffer);
	}
	else {
		info("(nobody)");
	}
}

static int infobit_system(addr pos)
{
	switch (info_gettype(pos)) {
		case LISPSYSTEM_UNBOUND:
			info("  %-10s = %s", "type", "unbound");
			return 0;

		case LISPSYSTEM_SPACE:
			info("  %-10s = %s", "type", "space");
			return 0;

		case LISPSYSTEM_SPACE1:
			info("  %-10s = %s", "type", "space1");
			return 0;

		case LISPSYSTEM_RESERVED:
			info("  %-10s = %s", "type", "reserveed");
			return 0;

		default:
			return 1;
	}
}

#define ONOFF(x) ((x)? '1': '-')
static void infobit_info(addr pos)
{
	enum LISPTYPE type;
	int status, user, array, body;
	size_t length;

	type = info_gettype(pos);
	status = GetStatus(pos);
	user = GetUser(pos);
	array = IsArray(pos);
	body = IsBody(pos);

	info("  %-10s = %d: %s", "type", type, infochar_lisp(type));
	info("  %-10s = %02X: %s [DRSFG:%c%c%c%c%c]", "status",
			status,
			infochar_size(GetStatusSize(pos)),
			ONOFF(GetStatusDynamic(pos)),
			ONOFF(GetStatusReadOnly(pos)),
			ONOFF(GetStatusSystem(pos)),
			ONOFF(GetStatusFixed(pos)),
			ONOFF(GetStatusGc(pos)));
	info("  %-10s = %02X", "user", user);
	if (array) {
		lenarray(pos, &length);
		info("  %-10s = %" PRIu64, "array", (uint64_t)length);
	}
	else {
		info("  %-10s = 0", "array");
	}
	if (body) {
		lenbody(pos, &length);
		info("  %-10s = %" PRIu64, "body", (uint64_t)length);
	}
	else {
		info("  %-10s = 0", "body");
	}
}

void infobit(addr pos)
{
	info("*** LISPBIT BEGIN ***");
	if (infobit_system(pos)) {
		infobit_info(pos);
		info("---body---");
		infobit_body(pos);
	}
	info("*** LISPBIT END ***");
}

void infoprint(addr pos)
{
	InfoDepth = INFO_DEPTH;
	infoprint_stream(pos, 0);
	info_eol();
}

void infoprint_depth(addr pos, int depth)
{
	InfoDepth = depth;
	infoprint_stream(pos, 0);
	info_eol();
}

void infoprint_noeol(addr pos)
{
	InfoDepth = INFO_DEPTH;
	infoprint_stream(pos, 0);
}

void infoprint_once(addr pos, const char *name)
{
	FILE *backup, *file;

	file = fopen(name, "w");
	if (file == NULL) {
		fprintf(stderr, "fopen error\n");
		exit(1);
	}
	backup = InfoStream;
	InfoStream = file;
	infoprint(pos);
	/* close */
	InfoStream = backup;
	fclose(file);
}

void infotype(addr pos)
{
	if (type_object_(&pos, pos))
		Abort("escape error.");
	infoprint(pos);
}


/*
 *  function type
 */
/* nil */
static void infobit_nil(addr pos)
{
	info("nil object: %s", (Nil == pos)? "valid": "INVALID");
}

static void infoprint_nil(void)
{
	info_stdarg("NIL");
}


/* t */
static void infobit_t(addr pos)
{
	info("T object: %s", (T == pos)? "valid": "INVALID");
}

static void infoprint_t(void)
{
	info_stdarg("T");
}


/* type */
static void infobit_type(addr pos)
{
	info("Not: %d", RefNotDecl(pos));
	info("Decl: %s", infochar_decl(RefLispDecl(pos)));
}

static void infoprint_type(addr pos)
{
	info_stdarg("#<TYPE %s%s>",
			(RefNotDecl(pos)? "not.": ""),
			infochar_decl(RefLispDecl(pos)));
}


/* cons */
static void infobit_cons(addr pos)
{
	addr car, cdr;

	GetCons(pos, &car, &cdr);
	info("Car: %s", infochar_lisp(info_gettype(car)));
	info("Cdr: %s", infochar_lisp(info_gettype(cdr)));
}

static void infoprint_cons(addr pos, int depth)
{
	int next;
	addr left;

	if (InfoDepth <= depth) {
		info_stdarg("...");
		return;
	}
	info_stdarg("(");
	for (next = 0; ; next++) {
		GetCons(pos, &left, &pos);
		if (next)
			info_stdarg(" ");
		infoprint_stream(left, depth);
		if (pos == Nil) {
			info_stdarg(")");
			return;
		}
		if (InfoDepth <= next) {
			info_stdarg(" ...)");
			return;
		}
		if (pos == Unbound || GetType(pos) != LISPTYPE_CONS) {
			info_stdarg(" . ");
			infoprint_stream(pos, depth);
			info_stdarg(")");
			return;
		}
	}
}


/* array */
static void infobit_array(addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	info("simple      : %s", str->simple? "t": "nil");
	info("adjustable  : %s", str->adjustable? "t": "nil");
	info("fillpointer : %s", str->fillpointer? "t": "nil");
	info("displaced   : %s", str->displaced? "t": "nil");
	info("type        : %s", infochar_array(str->type));
	info("element     : %u", str->element);
	info("bytesize    : %u", str->bytesize);
	info("size        : %zu", str->size);
	info("front       : %zu", str->front);
	info("dimension   : %zu", str->dimension);
	info("offset      : %zu", str->offset);
}

static void infoprint_array(addr pos, int depth)
{
	info_stdarg("#<ARRAY>");
}


/* eval */
static void infobit_eval(addr pos)
{
	info("EvalType: %s", infochar_eval(pos));
	switch (RefEvalType(pos)) {
		case EVAL_TYPE_PARSE:
			info("ParseType: %s", infochar_eval_parse(pos));
			break;

		case EVAL_TYPE_STACK:
			info("StackType: %s", infochar_eval_stack(pos));
			break;

		case EVAL_TYPE_SCOPE:
			info_noeol("ScopeThe: ");
			infoprint(RefEvalScopeThe(pos));
			info_noeol("ScopeValue: ");
			infoprint(RefEvalScopeValue(pos));
			break;

		case EVAL_TYPE_DECLARE:
		default:
			break;
	}
}

static void infobit_symbol(addr pos)
{
	int c;
	addr package, name, pack;
	pbyte body;
	size_t i, len, size;
	char buffer[64];

	GetNameSymbol(pos, &name);
	GetPackageSymbol(pos, &package);
	if (package == Nil) {
		strcpy(buffer, "(GENSYM)::");
		len = strlen(buffer);
	}
	else {
		getname_package_unsafe(package, &pack);
		posbodylen(pack, &body, &size);
		len = 0;
		for (i = 0; i < size; i++) {
			c = RdByte(body + i);
			if (c < 0)
				return;
			buffer[i + len] = c;
			if (20 <= i) {
				buffer[i + len + 1] = '.';
				buffer[i + len + 2] = '.';
				buffer[i + len + 3] = '.';
				i += 4;
				break;
			}
		}
		len += i;
		buffer[len++] = ':';
		buffer[len++] = ':';
	}

	posbodylen(name, &body, &size);
	for (i = 0; i < size; i++) {
		c = RdByte(body + i);
		if (c < 0)
			return;
		buffer[i + len] = c;
		if (20 <= i) {
			buffer[i + len + 1] = '.';
			buffer[i + len + 2] = '.';
			buffer[i + len + 3] = '.';
			i += 4;
			break;
		}
	}
	len += i;
	buffer[len] = 0;
	info("Symbol-Name: %s", buffer);
}

static void infobit_fixnum(addr pos)
{
	fixnum value;
	GetFixnum(pos, &value);
	info("Fixnum: [#x%08" PRIXF "] %10" PRIdF, value, value);
}

static void infobit_single(addr pos)
{
	single_float value;
	GetSingleFloat(pos, &value);
	info("Fixnum: %e - single", value);
}

static void infobit_double(addr pos)
{
	double_float value;
	GetDoubleFloat(pos, &value);
	info("Fixnum: %e - double", value);
}

static void infoprint_vector(addr array, int depth)
{
	addr pos;
	size_t i, len;

	if (InfoDepth <= depth) {
		info_stdarg("...");
		return;
	}
	info_stdarg("#(");
	lenarray(array, &len);
	for (i = 0; i < len; i++) {
		getarray(array, i, &pos);
		if (i) info_stdarg(" ");
		infoprint_stream(pos, depth);
	}
	info_stdarg(")");
}

static void infoprint_pathname(addr pos, int depth)
{
	info_stdarg("#p:");
	infoprint_vector(pos, depth);
}

static void infostringbody(addr pos)
{
	size_t i, len;
	unicode c;

	string_length(pos, &len);
	for (i = 0; i < len; i++) {
		Error(string_getc_(pos, i, &c));
		info_stdarg("%c", isstandardtype(c)? c: '.');
	}
}

static void infoprint_clos(addr pos)
{
	addr check, key, name;

	info_stdarg("#<");
	/* class-of */
	GetClassOfClos(pos, &check);
	GetConst(CLOSNAME_NAME, &key);
	if (key == Unbound) {
		info_stdarg("CLOS>");
		return;
	}
	else if (check == Unbound) {
		info_stdarg("UNBOUND");
	}
	else if (! closp(check)) {
		info_stdarg("INVALID");
	}
	else if (! clos_getp(check, key, &name)) {
		info_stdarg("UNBOUND");
	}
	else if (name == Unbound) {
		info_stdarg("UNBOUND");
	}
	else if (! symbolp(name)) {
		info_stdarg("INVALID");
	}
	else {
		GetNameSymbol(name, &name);
		infostringbody(name);
	}
	info_stdarg(" ");

	/* name */
	if (! clos_getp(pos, key, &name)) {
		info_stdarg("%p", (void *)pos);
	}
	else if (name == Unbound) {
		info_stdarg("%p", (void *)pos);
	}
	else if (! symbolp(name)) {
		info_stdarg("%p", (void *)pos);
	}
	else {
		GetNameSymbol(name, &name);
		infostringbody(name);
	}
	info_stdarg(">");
}

static void infoprint_fixnum(addr pos)
{
	fixnum value;
	GetFixnum(pos, &value);
	info_stdarg("%" PRIdF, value);
}

static void infoprint_index(addr pos)
{
	info_stdarg("#%zu", RefIndex(pos));
}

static void infoprint_single(addr pos)
{
	single_float value;
	GetSingleFloat(pos, &value);
	info_stdarg("%g", value);
}

static void infoprint_double(addr pos)
{
	double_float value;
	GetDoubleFloat(pos, &value);
	info_stdarg("%g", value);
}

static void infoprint_character(addr pos)
{
	unicode value;
	GetCharacter(pos, &value);
	if (value < 0x80) {
		if (isstandardtype(value))
			info_stdarg("#\\%c", value);
		else
			info_stdarg("#\\(cannot-printable)");
	}
	else {
		info_stdarg("#\\(unicode)");
	}
}

static void infoprint_string(addr pos)
{
	info_stdarg("\"");
	infostringbody(pos);
	info_stdarg("\"");
}

static void infoprint_symbol(addr pos)
{
	addr package;

	GetPackageSymbol(pos, &package);
	if (package == Nil)
		info_stdarg("#:");
	else {
		getname_package_unsafe(package, &package);
		if (string_equal_char_debug(package, LISP_KEYWORD)) {
			info_stdarg(":");
		}
		else if (string_equal_char_debug(package, LISP_COMMON)) {
			/* no output */
		}
		else if (string_equal_char_debug(package, LISP_CODE)) {
			info_stdarg("@:");
		}
		else {
			infostringbody(package);
			info_stdarg("::");
		}
	}
	GetNameSymbol(pos, &pos);
	if (stringp(pos)) {
		infostringbody(pos);
	}
	else {
		infoprint_stream(pos, 0);
	}
}

static void infoprint_callname(addr pos)
{
	info_stdarg("#<callname:");
	GetCallName(pos, &pos);
	infoprint_symbol(pos);
	info_stdarg(">");
}

static void infoprint_function(addr pos)
{
	info_stdarg("#<function>");
}

static void infoprint_eval(addr pos)
{
	info_stdarg("#<eval.%s", infochar_eval(pos));
	switch (RefEvalType(pos)) {
		case EVAL_TYPE_DECLARE:
			break;

		case EVAL_TYPE_PARSE:
			info_stdarg(".%s", infochar_eval_parse(pos));
			break;

		case EVAL_TYPE_STACK:
			info_stdarg(".%s", infochar_eval_stack(pos));
			break;

		case EVAL_TYPE_SCOPE:
			info_stdarg(".%s", infochar_eval_scope(pos));
			break;

		default:
			info_stdarg(".invalid");
			break;
	}
	info_stdarg(">");
}

static void infoprint_code(addr pos)
{
	info_stdarg("#<code>");
}

static void infoquote_front(addr pos, const char *str)
{
	info_stdarg(str);
	getvalue_quote(pos, &pos);
	infoprint_noeol(pos);
}

static void infoquote_list(addr pos, const char *str)
{
	info_stdarg("#<");
	info_stdarg(str);
	info_stdarg(":");
	getvalue_quote(pos, &pos);
	infoprint_noeol(pos);
	info_stdarg(">");
}

static void infoprint_streamtype(addr pos)
{
	info_stdarg("#<STREAM:");
	switch (getstreamtype(pos)) {
		case StreamType_BinaryInput: info_stdarg("BINARY-INPUT"); break;
		case StreamType_BinaryOutput: info_stdarg("BINARY-OUTPUT"); break;
		case StreamType_BinaryIO: info_stdarg("BINARY-IO"); break;
		case StreamType_CharacterInput: info_stdarg("CHARACTER-INPUT"); break;
		case StreamType_CharacterOutput: info_stdarg("CHARACTER-OUTPUT"); break;
		case StreamType_CharacterIO: info_stdarg("CHARACTER-IO"); break;
		case StreamType_BincharInput: info_stdarg("BINCHAR-INPUT"); break;
		case StreamType_BincharOutput: info_stdarg("BINCHAR-OUTPUT"); break;
		case StreamType_BincharIO: info_stdarg("BINCHAR-IO"); break;
		case StreamType_Probe: info_stdarg("PROBE"); break;
		case StreamType_StringInput: info_stdarg("STRING-INPUT"); break;
		case StreamType_StringOutput: info_stdarg("STRING-OUTPUT"); break;
		case StreamType_Synonym: info_stdarg("SYNONYM"); break;
		case StreamType_BroadCast: info_stdarg("BROADCAST"); break;
		case StreamType_Concatenated: info_stdarg("CONCATENATED"); break;
		case StreamType_TwoWay: info_stdarg("TWOWAY"); break;
		case StreamType_Echo: info_stdarg("ECHO"); break;
		case StreamType_Prompt: info_stdarg("PROMPT"); break;
		case StreamType_Pretty: info_stdarg("PRETTY"); break;
		case StreamType_MemoryInput: info_stdarg("MEMORY-INPUT"); break;
		case StreamType_MemoryOutput: info_stdarg("MEMORY-OUTPUT"); break;
		case StreamType_MemoryIO: info_stdarg("MEMORY-IO"); break;
		case StreamType_Pipe: info_stdarg("PIPE"); break;
		default: info_stdarg("ERROR"); break;
	}
	info_stdarg(">");
}

static void infoprint_quote(addr pos)
{
	if (quote_back_p(pos))
		infoquote_front(pos, "`");
	else if (quote_comma_p(pos))
		infoquote_front(pos, ",");
	else if (quote_atsign_p(pos))
		infoquote_front(pos, ",@");
	else if (quote_dot_p(pos))
		infoquote_front(pos, ",.");
	else if (quote_quote_p(pos))
		infoquote_list(pos, "quote.quote");
	else if (quote_append_p(pos))
		infoquote_list(pos, "quote.append");
	else if (quote_nconc_p(pos))
		infoquote_list(pos, "quote.nconc");
	else if (quote_list_p(pos))
		infoquote_list(pos, "quote.list");
	else if (quote_lista_p(pos))
		infoquote_list(pos, "quote.list*");
	else if (quote_clobberable_p(pos))
		infoquote_list(pos, "quote.clobberable");
	else
		infoquote_list(pos, "quote.error");
}

static void infoprint_restart(addr pos)
{
	info_stdarg("#<RESTART ");
	getname_restart(pos, &pos);
	infoprint_noeol(pos);
	info_stdarg(">");
}

static void infoprint_bytespec(addr pos)
{
	struct bytespec_struct *ptr = ByteSpecStruct(pos);
	info_stdarg("#<BYTE size:%zu position:%zu>", ptr->size, ptr->position);
}

static void infoprint_unbound(void)
{
	info_stdarg("#<UNBOUND>");
}

static void infoprint_space(addr pos)
{
	size_t size;
	GetSizeSpace(pos, &size);
	info_stdarg("#<SPACE %lu>", (unsigned long)size);
}

static void infoprint_space1(addr pos)
{
	info_stdarg("#<SPACE %u>", (unsigned)pos[1]);
}

static void infoprint_reserved(addr pos)
{
	size_t size;
	GetSizeReserved(pos, &size);
	info_stdarg("#<RESERVED %lu>", (unsigned long)size);
}

static void infoprint_default(addr pos)
{
	enum LISPTYPE type;
	const char *ptr;

	type = info_gettype(pos);
	ptr = infochar_lisp(type);
	info_stdarg("#<OBJECT %s %d[0x%X]>", ptr, type, type);
}

static void infobit_body(addr pos)
{
	switch (info_gettype(pos)) {
		case LISPTYPE_NIL: infobit_nil(pos); break;
		case LISPTYPE_T: infobit_t(pos); break;
		case LISPTYPE_TYPE: infobit_type(pos); break;
		case LISPTYPE_CONS: infobit_cons(pos); break;
		case LISPTYPE_ARRAY: infobit_array(pos); break;
		case LISPTYPE_EVAL: infobit_eval(pos); break;
		case LISPTYPE_SYMBOL: infobit_symbol(pos); break;
		case LISPTYPE_FIXNUM: infobit_fixnum(pos); break;
		case LISPTYPE_SINGLE_FLOAT: infobit_single(pos); break;
		case LISPTYPE_DOUBLE_FLOAT: infobit_double(pos); break;
		default: infobit_dump(pos); break;
	}
}

static void infoprint_stream(addr pos, int depth)
{
	if (pos == NULL) {
		info_stdarg("#<C:NULL>");
		return;
	}
	depth++;
	switch (info_gettype(pos)) {
		case LISPTYPE_NIL: infoprint_nil(); break;
		case LISPTYPE_T: infoprint_t(); break;
		case LISPTYPE_TYPE: infoprint_type(pos); break;
		case LISPTYPE_CONS: infoprint_cons(pos, depth); break;
		case LISPTYPE_ARRAY: infoprint_array(pos, depth); break;
		case LISPTYPE_VECTOR: infoprint_vector(pos, depth); break;
		case LISPTYPE_PATHNAME: infoprint_pathname(pos, depth); break;
		case LISPTYPE_CLOS: infoprint_clos(pos); break;
		case LISPTYPE_CHARACTER: infoprint_character(pos); break;
		case LISPTYPE_STRING: infoprint_string(pos); break;
		case LISPTYPE_FIXNUM: infoprint_fixnum(pos); break;
		case LISPTYPE_INDEX: infoprint_index(pos); break;
		case LISPTYPE_SINGLE_FLOAT: infoprint_single(pos); break;
		case LISPTYPE_DOUBLE_FLOAT: infoprint_double(pos); break;
		case LISPTYPE_SYMBOL: infoprint_symbol(pos); break;
		case LISPTYPE_CALLNAME: infoprint_callname(pos); break;
		case LISPTYPE_FUNCTION: infoprint_function(pos); break;
		case LISPTYPE_EVAL: infoprint_eval(pos); break;
		case LISPTYPE_CODE: infoprint_code(pos); break;
		case LISPTYPE_STREAM: infoprint_streamtype(pos); break;
		case LISPTYPE_QUOTE: infoprint_quote(pos); break;
		case LISPTYPE_RESTART: infoprint_restart(pos); break;
		case LISPTYPE_BYTESPEC: infoprint_bytespec(pos); break;

		case LISPSYSTEM_UNBOUND: infoprint_unbound(); break;
		case LISPSYSTEM_SPACE: infoprint_space(pos); break;
		case LISPSYSTEM_SPACE1: infoprint_space1(pos); break;
		case LISPSYSTEM_RESERVED: infoprint_reserved(pos); break;
		default: infoprint_default(pos); break;
	}
}


/************************************************************
 *  integer.c
 ************************************************************/

/*
 *  type
 */
int integerp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_FIXNUM || type == LISPTYPE_BIGNUM;
}

int minusp_integerp(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return minusp_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return minusp_bignum(pos);

		default:
			return 0;
	}
}


/*
 *  throw
 */
int integer_throw_alloc_(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_alloc(local, pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_throw_alloc(local, pos, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}
}

int integer_throw_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return integer_throw_alloc_(local, pos, ret);
}

int integer_throw_heap_(addr pos, addr *ret)
{
	return integer_throw_alloc_(NULL, pos, ret);
}

int integer_result_alloc_(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_result_alloc(local, pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_result_alloc(local, pos, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}
}

int integer_result_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_result_local(local, pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_result_local(local, pos, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}
}

int integer_result_heap_(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_result_heap(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_result_heap(pos, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}
}

void fixnum_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	fixnum_alloc(local, ret, RefFixnum(pos));
}

void fixnum_copy_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	fixnum_copy_alloc(local, pos, ret);
}

void fixnum_copy_heap(LocalRoot local, addr pos, addr *ret)
{
	fixnum_copy_alloc(NULL, pos, ret);
}

int integer_copy_alloc_(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_copy_alloc(local, pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_copy_alloc(local, ret, pos);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}
}

int integer_copy_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return integer_copy_alloc_(local, pos, ret);
}

int integer_copy_heap_(addr pos, addr *ret)
{
	return integer_copy_alloc_(NULL, pos, ret);
}


/*
 *  compare
 */
int getsign_integer_(addr pos, int *ret)
{
	fixed v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			castfixed_fixnum(pos, ret, &v);
			break;

		case LISPTYPE_BIGNUM:
			GetSignBignum(pos, ret);
			break;

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}

	return 0;
}

int zerop_or_plusp_integer_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, zerop_or_plusp_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, zerop_or_plusp_bignum(pos));

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

int plusp_integer_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, plusp_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, plusp_bignum(pos));

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

int minusp_integer_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, minusp_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, minusp_bignum(pos));

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

int zerop_integer_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, zerop_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, zerop_bignum(pos));

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

static inline int equal_fixnum_integer_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, equal_ff_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, equal_fb_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, INTEGER);
	}
}

static inline int equal_bignum_integer_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, equal_bf_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, equal_bb_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, INTEGER);
	}
}

int equal_integer_(addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return equal_fixnum_integer_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return equal_bignum_integer_(left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, INTEGER);
	}
}

int not_equal_integer_(addr left, addr right, int *ret)
{
	int check;
	Return(equal_integer_(left, right, &check));
	return Result(ret, ! check);
}

static inline int compare_fixnum_integer_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, compare_ff_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, compare_fb_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, INTEGER);
	}
}

static inline int compare_bignum_integer_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, compare_bf_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, compare_bb_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, INTEGER);
	}
}

int compare_integer_(addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return compare_fixnum_integer_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return compare_bignum_integer_(left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, INTEGER);
	}
}

int less_integer_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_integer_(left, right, &check));
	return Result(ret, check < 0);
}

int less_equal_integer_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_integer_(left, right, &check));
	return Result(ret, check <= 0);
}

int greater_integer_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_integer_(left, right, &check));
	return Result(ret, check > 0);
}

int greater_equal_integer_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_integer_(left, right, &check));
	return Result(ret, check >= 0);
}

int zerop_integer_debug(addr pos)
{
	int check;

	Check(! integerp(pos), "left error");
	check = 0;
	Error(zerop_integer_(pos, &check));

	return check;
}

int plusp_integer_debug(addr pos)
{
	int check;

	Check(! integerp(pos), "left error");
	check = 0;
	Error(plusp_integer_(pos, &check));

	return check;
}

int minusp_integer_debug(addr pos)
{
	int check;

	Check(! integerp(pos), "left error");
	check = 0;
	Error(minusp_integer_(pos, &check));

	return check;
}

int less_integer_debug(addr left, addr right)
{
	int check;

	Check(! integerp(left), "left error");
	Check(! integerp(right), "right error");
	check = 0;
	Error(less_integer_(left, right, &check));

	return check;
}

int less_equal_integer_debug(addr left, addr right)
{
	int check;

	Check(! integerp(left), "left error");
	Check(! integerp(right), "right error");
	check = 0;
	Error(less_equal_integer_(left, right, &check));

	return check;
}

int sign_reverse_integer_common_(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			sigrev_fixnum_integer_common(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			sigrev_bignum_integer_common(pos, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}
}

static int evenp_fixnum(addr left)
{
	fixnum value;
	GetFixnum(left, &value);
	return (value & 1) == 0;
}

int evenp_integer_(addr left, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, evenp_fixnum(left));

		case LISPTYPE_BIGNUM:
			return Result(ret, evenp_bignum(left));

		default:
			*ret = 0;
			return TypeError_(left, INTEGER);
	}
}


/*
 *  size
 */
void make_index_integer_alloc(LocalRoot local, addr *ret, size_t value)
{
	size_t i, count;
	fixed *data;

	if (value <= FIXNUM_MAX) {
		fixnum_alloc(local, ret, (fixnum)value);
	}
	else if (value <= BIGNUM_FULL) {
		bignum_value_alloc(local, ret, signplus_bignum, (fixed)value);
	}
	else {
		count = value / BIGNUM_FULL;
		bignum_alloc(local, ret, signplus_bignum, count);
		GetDataBignum(*ret, &data);
		for (i = 0; i < count; i++) {
			data[i] = value % BIGNUM_FULL;
			value /= BIGNUM_FULL;
		}
	}
}

void make_index_integer_local(LocalRoot local, addr *ret, size_t value)
{
	CheckLocal(local);
	make_index_integer_alloc(local, ret, value);
}

void make_index_integer_heap(addr *ret, size_t value)
{
	make_index_integer_alloc(NULL, ret, value);
}

void make_indexmax_alloc(LocalRoot local, addr *ret)
{
#if (SIZE_MAX <= FIXNUM_MAX)
	fixnum_alloc(local, ret, (fixnum)SIZE_MAX);
#elif (SIZE_MAX <= BIGNUM_FULL)
	bignum_value_alloc(local, ret, signplus_bignum, (fixed)SIZE_MAX);
#else
	size_t size, i, count;
	fixed *data;

	size = SIZE_MAX;
	count = SIZE_MAX / BIGNUM_FULL;
	bignum_alloc(local, ret, signplus_bignum, count);
	GetDataBignum(*ret, &data);
	for (i = 0; i < count; i++) {
		data[i] = size % BIGNUM_FULL;
		size /= BIGNUM_FULL;
	}
#endif
}

#if (FIXNUM_MAX <= SIZE_MAX)
static int getindex_integer_fixnum(addr pos, size_t *ret)
{
	fixnum value;

	GetFixnum(pos, &value);
	if (value < 0)
		return 1;
	*ret = (size_t)value;

	return 0;
}
#else
static int getindex_integer_fixnum(addr pos, size_t *ret)
{
	fixnum value;

	GetFixnum(pos, &value);
	if (value < 0)
		return 1;
	if (SIZE_MAX < value)
		return 1;
	*ret = (size_t)value;

	return 0;
}
#endif

#if (BIGNUM_FULL < SIZE_MAX)
static int getindex_integer_bignum(addr pos, size_t *ret)
{
	int sign;
	size_t size, i, value;
	fixed *data;

	GetSignBignum(pos, &sign);
	if (IsMinus(sign))
		return 1;
	GetSizeBignum(pos, &size);
	if (SIZE_MAX / BIGNUM_FULL < size)
		return 1;

	value = 0;
	GetDataBignum(pos, &data);
	for (i = 0; i < size; i++) {
		value <<= BIGNUM_FULLBIT;
		value |= data[i];
	}
	*ret = value;

	return 0;
}

#else
static int getindex_integer_bignum(addr pos, size_t *ret)
{
	int sign;
	fixed value;
	size_t size;

	GetSignBignum(pos, &sign);
	if (IsMinus(sign))
		return 1;
	GetSizeBignum(pos, &size);
	if (size != 1)
		return 1;
	getfixed_bignum(pos, 0, &value);
	if (SIZE_MAX < value)
		return 1;
	*ret = (size_t)value;

	return 0;
}
#endif

int GetIndex_integer(addr pos, size_t *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return getindex_integer_fixnum(pos, ret);

		case LISPTYPE_BIGNUM:
			return getindex_integer_bignum(pos, ret);

		default:
			return 1;
	}
}

int getindex_integer_(addr pos, size_t *ret)
{
	addr type;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			if (getindex_integer_fixnum(pos, ret))
				break;
			return 0;

		case LISPTYPE_BIGNUM:
			if (getindex_integer_bignum(pos, ret))
				break;
			return 0;

		default:
			break;
	}

	/* error */
	GetTypeTable(&type, Index);
	return call_type_error_(NULL, pos, type);
}

addr reference_index_integer_alloc(LocalRoot local, size_t value)
{
	addr pos;
	make_index_integer_alloc(local, &pos, value);
	return pos;
}

addr reference_index_integer_local(LocalRoot local, size_t value)
{
	Check(local == NULL, "local error");
	return reference_index_integer_alloc(local, value);
}

addr reference_index_integer_heap(size_t value)
{
	return reference_index_integer_alloc(NULL, value);
}

static int cast_fixed_index(fixed value, size_t *ret)
{
#if (BIGNUM_FULL < SIZE_MAX)
	*ret = (size_t)value;
	return 0;
#else
	if (SIZE_MAX < value) {
		return 1;
	}
	else {
		*ret = (size_t)value;
		return 0;
	}
#endif
}

static int cast_fixnum_index_(addr pos, int *sign, size_t *value, int *ret)
{
	fixed body;

	CheckType(pos, LISPTYPE_FIXNUM);
	castfixed_fixnum(pos, sign, &body);
	*ret = cast_fixed_index(body, value);
	return 0;
}

static int cast_bignum_index_(addr pos, int *sign, size_t *value, int *ret)
{
	fixed *data;
	size_t size;

	CheckType(pos, LISPTYPE_BIGNUM);
	GetSizeBignum(pos, &size);
	GetSignBignum(pos, sign);
	if (size != 1) {
		*ret = 1;
		return 0;
	}
	GetDataBignum(pos, &data);
	*ret = cast_fixed_index(data[0], value);
	return 0;
}

int getindex_sign_integer_(addr pos, int *sign, size_t *value, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return cast_fixnum_index_(pos, sign, value, ret);

		case LISPTYPE_BIGNUM:
			return cast_bignum_index_(pos, sign, value, ret);

		default:
			*sign = 0;
			*value = 0;
			*ret = 1;
			return TypeError_(pos, INTEGER);
	}
}

int GetIndex_fixnum(addr pos, size_t *ret)
{
	fixnum value;

	if (! fixnump(pos))
		return 1;
	GetFixnum(pos, &value);
	if (value < 0)
		return 1;
	*ret = (size_t)value;

	return 0;
}

int getindex_fixnum_(addr pos, size_t *ret)
{
	if (GetIndex_fixnum(pos, ret)) {
		*ret = 0;
		return TypeError_(pos, FIXNUM);
	}

	return 0;
}

int fixnum_index_heap_(addr *ret, size_t value)
{
	if ((size_t)FIXNUM_MAX <= value)
		return fmte_("Too large value ~S.", intsizeh(value), NULL);
	fixnum_heap(ret, (fixnum)value);

	return 0;
}

int GetByte_integer(addr pos, byte *ret)
{
	fixnum v;

	if (GetFixnum_signed(pos, &v))
		goto error;
	if (! IsByteSign(v))
		goto error;
	*ret = (byte)v;
	return 0;

error:
	*ret = 0;
	return 1;
}

int getunicode_integer_(addr pos, unicode *ret)
{
	size_t value;

	if (GetIndex_integer(pos, &value)) {
		*ret = 0;
		goto error;
	}

#ifdef LISP_64BIT
	if (0xFFFFFFFFULL < value)
		goto error;
#endif
	return Result(ret, (unicode)value);

error:
	return fmte_("Invalid character code ~A.", pos, NULL);
}


/*
 *  standard type
 */
void int8_integer_alloc(LocalRoot local, addr *ret, int8_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

void int16_integer_alloc(LocalRoot local, addr *ret, int16_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

void int32_integer_alloc(LocalRoot local, addr *ret, int32_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

void uint8_integer_alloc(LocalRoot local, addr *ret, uint8_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

void uint16_integer_alloc(LocalRoot local, addr *ret, uint16_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

#ifdef LISP_64BIT
void uint32_integer_alloc(LocalRoot local, addr *ret, uint32_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}
#else
void uint32_integer_alloc(LocalRoot local, addr *ret, uint32_t value)
{
	bignum_value_alloc(local, ret, signplus_bignum, (fixed)value);
}
#endif

#ifdef LISP_64BIT
void int64_integer_alloc(LocalRoot local, addr *ret, int64_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

void uint64_integer_alloc(LocalRoot local, addr *ret, uint64_t value)
{
	bignum_value_alloc(local, ret, signplus_bignum, (fixed)value);
}
#endif


/************************************************************
 *  integer_calc.c
 ************************************************************/

/*
 *  oneplus
 */
int oneplus_integer_common_(LocalRoot local, addr value, addr *ret)
{
	CheckLocal(local);
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, 1, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, 1, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(value, INTEGER);
	}
}

int oneminus_integer_common_(LocalRoot local, addr value, addr *ret)
{
	CheckLocal(local);
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, -1, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, -1, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(value, INTEGER);
	}
}


/*
 *  plus
 */
int plus_fi_bignum_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_bignum_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_fb_bignum_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

int plus_fi_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_fb_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

int plus_fi_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_fb_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

int plus_bi_bignum_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_bignum_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bb_bignum_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

int plus_bi_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bb_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

int plus_bi_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bb_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

int plus_ii_bignum_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fi_bignum_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bi_bignum_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, INTEGER);
	}
}

int plus_ii_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fi_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bi_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, INTEGER);
	}
}

int plus_ii_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fi_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bi_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, INTEGER);
	}
}


/*
 *  minus
 */
static int minus_fi_integer_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_fb_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

static int minus_bi_integer_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_bb_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

int minus_ii_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_fi_integer_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_bi_integer_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, INTEGER);
	}
}


/*
 *  multi
 */
static int multi_fi_integer_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_fb_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

static int multi_bi_integer_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_bb_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

int multi_ii_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return multi_fi_integer_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_bi_integer_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, INTEGER);
	}
}


/************************************************************
 *  integer_common.c
 ************************************************************/

/*
 *  output
 */
int output_nosign_integer_(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return output_nosign_fixnum_(stream, RefFixnum(pos), base, upperp);

		case LISPTYPE_BIGNUM:
			return output_nosign_bignum_(local, stream, pos, base, upperp);

		default:
			return TypeError_(pos, INTEGER);
	}
}

int output_nosign_comma_integer_(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp, size_t range, unicode comma)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return output_nosign_comma_fixnum_(local, stream, RefFixnum(pos),
					base, upperp, range, comma);

		case LISPTYPE_BIGNUM:
			return output_nosign_comma_bignum_(local, stream, pos,
					base, upperp, range, comma);

		default:
			return TypeError_(pos, INTEGER);
	}
}

#define INTEGER_STREAM_SIZE		64
int string_nosign_comma_integer_(LocalRoot local, addr *ret, addr pos,
		unsigned base, int upperp, size_t range, unicode comma)
{
	addr stream;

	open_output_string_stream(&stream, INTEGER_STREAM_SIZE);
	Return(output_nosign_comma_integer_(local,
				stream, pos, base, upperp, range, comma));
	Return(string_stream_alloc_(local, stream, ret));
	close_output_string_stream(stream);

	return 0;
}


/*
 *  integer-length
 */
static size_t integer_length_fixed(fixed value)
{
	size_t size;

	for (size = 0; value; size++)
		value >>= 1;

	return size;
}

static size_t inverse_length_fixed(fixed value)
{
	if (value <= 1)
		return 0;
	else
		return integer_length_fixed(value - 1);
}

static void integer_length_fixnum(addr pos, size_t *ret)
{
	int sign;
	fixed value;

	castfixed_fixnum(pos, &sign, &value);
	if (IsPlus(sign))
		*ret = integer_length_fixed(value);
	else
		*ret = inverse_length_fixed(value);
}

static size_t integer_length_bigdata(addr pos)
{
	size_t size, size1, size2;
	fixed *data;

	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);
	if (size == 1 && data[0] == 0)
		return 0;
	size--;
	size1 = size * BIGNUM_FULLBIT;
	size2 = integer_length_fixed(data[size]);

	return size1 + size2;
}

static int check_length_bignum(addr pos)
{
	int sign;
	fixed *data;
	size_t size, i;

	GetSignBignum(pos, &sign);
	if (IsPlus(sign))
		return 0;
	GetSizeBignum(pos, &size);
	size--;
	GetDataBignum(pos, &data);
	for (i = 0; i < size; i++) {
		if (data[i] != 0)
			return 0;
	}

	return data[i] == 1;
}

static void integer_length_bignum(addr pos, size_t *ret)
{
	size_t size;

	size = integer_length_bigdata(pos);
	if (check_length_bignum(pos))
		size--;
	*ret = size;
}

int integer_length_value_(addr pos, size_t *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			integer_length_fixnum(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			integer_length_bignum(pos, ret);
			return 0;

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

int integer_length_common_(addr pos, addr *ret)
{
	size_t size;

	Return(integer_length_value_(pos, &size));
	make_index_integer_heap(ret, size);

	return 0;
}


/*
 *  ash
 */
static int ash_mm_common_(LocalRoot local, addr pos, size_t shift, addr *ret)
{
	size_t size;

	CheckType(pos, LISPTYPE_BIGNUM);
	Check(shift == 0, "shift error");

	/* size over */
	size = integer_length_bigdata(pos);
	if (size <= shift) {
		fixnum_heap(ret, -1);
		return 0;
	}

	/* shiftdown */
	shiftdown_minus_bigdata(local, &pos, pos, shift);
	SetSignBignum(pos, SignMinus);
	bignum_result_heap(pos, ret);

	return 0;
}

static int ash_up_common_(LocalRoot local, addr pos, size_t shift, addr *ret)
{
	int sign;

	GetSignBignum(pos, &sign);
	shiftup_bigdata_alloc(local, &pos, pos, shift);
	SetSignBignum(pos, sign);
	bignum_result_heap(pos, ret);

	return 0;
}

static int ash_down_common_(LocalRoot local, addr pos, size_t shift, addr *ret)
{
	int sign;

	GetSignBignum(pos, &sign);
	shiftdown_bigdata_alloc(local, &pos, pos, shift);
	SetSignBignum(pos, sign);
	bignum_result_heap(pos, ret);

	return 0;
}

static int ash_left_common_(LocalRoot local, addr pos, addr *value, int *ret)
{
	int check;

	*value = Nil;
	*ret = 0;
	Return(zerop_integer_(pos, &check));
	if (check) {
		*value = pos;
		return Result(ret, 1);
	}
	if (fixnump(pos))
		bignum_fixnum_local(local, &pos, pos);
	if (! bignump(pos))
		return TypeError_(pos, INTEGER);

	*value = pos;
	return Result(ret, 0);
}

static int ash_right_common_(LocalRoot local,
		addr count, size_t *value, int *sign, int *ret)
{
	int check;
	size_t shift;

	*value = 0;
	*sign = 0;
	*ret = 0;
	Return(getindex_sign_integer_(count, sign, &shift, &check));
	if (check)
		return fmte_("Too large shift value ~S.", count, NULL);
	if (shift == 0)
		return Result(ret, 1);

	*value = shift;
	return Result(ret, 0);
}

int ash_integer_common_(LocalRoot local, addr pos, addr count, addr *ret)
{
	int sign1, sign2, check;
	addr value;
	size_t shift;
	LocalStack stack;

	push_local(local, &stack);
	/* left */
	Return(ash_left_common_(local, pos, &value, &check));
	if (check) {
		*ret = pos;
		goto finish;
	}
	/* right */
	Return(ash_right_common_(local, count, &shift, &sign2, &check));
	if (check) {
		*ret = pos;
		goto finish;
	}

	/* sign */
	GetSignBignum(value, &sign1);
	if (IsPlus(sign1) && IsPlus(sign2)) {
		Return(ash_up_common_(local, value, shift, ret));
	}
	else if (IsPlus(sign1)) {
		Return(ash_down_common_(local, value, shift, ret));
	}
	else if (IsPlus(sign2)) {
		Return(ash_up_common_(local, value, shift, ret));
	}
	else {
		Return(ash_mm_common_(local, value, shift, ret));
	}
finish:
	rollback_local(local, stack);

	return 0;
}


/*
 *  parse-integer
 */
struct parse_integer_struct {
	unsigned sign, radix;
	addr input, cons;
	size_t end, index;
};

static int parse_integer_error_(LocalRoot local,
		struct parse_integer_struct *str, addr *ret, addr *rpos)
{
	unicode c;
	unsigned v;

	*ret = *rpos = Nil;
first:
	if (str->end <= str->index)
		goto error;
	Return(string_getc_(str->input, str->index, &c));
	if (isSpaceUnicode(c)) {
		str->index++;
		goto first;
	}
	/* sign */
	if (c == '+') {
		str->index++;
		str->sign = SignPlus;
		goto sign;
	}
	if (c == '-') {
		str->index++;
		str->sign = SignMinus;
		goto sign;
	}
	/* digit */
	if (getvalue_digit(str->radix, c, &v))
		goto error;
	str->index++;
	push_bigcons(local, str->cons, str->radix, v);
	goto loop;

sign:
	if (str->end <= str->index)
		goto error;
	Return(string_getc_(str->input, str->index, &c));
	if (getvalue_digit(str->radix, c, &v))
		goto error;
	str->index++;
	push_bigcons(local, str->cons, str->radix, v);

loop:
	if (str->end <= str->index)
		goto finish;
	Return(string_getc_(str->input, str->index, &c));
	if (isSpaceUnicode(c)) {
		str->index++;
		goto last;
	}
	if (getvalue_digit(str->radix, c, &v))
		goto error;
	str->index++;
	push_bigcons(local, str->cons, str->radix, v);
	goto loop;

last:
	if (str->end <= str->index)
		goto finish;
	Return(string_getc_(str->input, str->index, &c));
	if (isSpaceUnicode(c)) {
		str->index++;
		goto last;
	}
	goto error;

finish:
	integer_cons_heap(ret, str->sign, str->cons);
	make_index_integer_heap(rpos, str->index);
	return 0;

error:
	*ret = *rpos = Nil;
	return call_simple_parse_error_va_(NULL,
			"The string ~S is not integer.", str->input, NULL);
}

static int parse_integer_junk_allowed_(LocalRoot local,
		struct parse_integer_struct *str, addr *ret, addr *rpos)
{
	unicode c;
	unsigned v;

	*ret = *rpos = Nil;
first:
	if (str->end <= str->index)
		goto error;
	Return(string_getc_(str->input, str->index, &c));
	if (isSpaceUnicode(c)) {
		str->index++;
		goto first;
	}
	/* sign */
	if (c == '+') {
		str->index++;
		str->sign = SignPlus;
		goto sign;
	}
	if (c == '-') {
		str->index++;
		str->sign = SignMinus;
		goto sign;
	}
	/* digit */
	if (getvalue_digit(str->radix, c, &v))
		goto error;
	str->index++;
	push_bigcons(local, str->cons, str->radix, v);
	goto loop;

sign:
	if (str->end <= str->index)
		goto error;
	Return(string_getc_(str->input, str->index, &c));
	if (getvalue_digit(str->radix, c, &v))
		goto error;
	str->index++;
	push_bigcons(local, str->cons, str->radix, v);

loop:
	if (str->end <= str->index)
		goto finish;
	Return(string_getc_(str->input, str->index, &c));
	if (isSpaceUnicode(c))
		goto finish;
	if (getvalue_digit(str->radix, c, &v))
		goto finish;
	str->index++;
	push_bigcons(local, str->cons, str->radix, v);
	goto loop;

finish:
	integer_cons_heap(ret, str->sign, str->cons);
	make_index_integer_heap(rpos, str->index);
	return 0;

error:
	*ret = Nil;
	make_index_integer_heap(rpos, str->index);
	return 0;
}

int parse_integer_clang_(LocalRoot local,
		addr input, size_t start, size_t end, unsigned radix, int junk,
		addr *ret, addr *rpos)
{
	addr cons;
	LocalStack stack;
	struct parse_integer_struct str;

	push_local(local, &stack);
	bigcons_local(local, &cons);
	str.sign = SignPlus;
	str.input = input;
	str.cons = cons;
	str.index = start;
	str.end = end;
	str.radix = radix;
	if (! junk) {
		Return(parse_integer_error_(local, &str, ret, rpos));
	}
	else {
		Return(parse_integer_junk_allowed_(local, &str, ret, rpos));
	}
	rollback_local(local, stack);

	return 0;
}


/************************************************************
 *  intern.c
 ************************************************************/

enum symbol_package {
	COMMON,
	KEYWORD,
	SYSTEM,
	CODE,
	CLOS,
	RT,
	DEFAULT
};

struct symbol_header {
	constindex index;
	enum symbol_package package;
	const char *symbol;
	size_t length;
	fixnum sxhash;
	int specialp;
	int exportp;
	int findp;
};

static struct symbol_header SymbolHeader[] = {
#ifdef LISP_64BIT
{ CONSTANT_AMPERSAND_WHOLE, COMMON, "&WHOLE", 6, 0x0000454C4F48572CULL, 0, 0, 0 },
{ CONSTANT_AMPERSAND_OPTIONAL, COMMON, "&OPTIONAL", 9, 0x414E4F4954504F7BULL, 0, 0, 0 },
{ CONSTANT_AMPERSAND_REST, COMMON, "&REST", 5, 0x000000545345522BULL, 0, 0, 0 },
{ CONSTANT_AMPERSAND_BODY, COMMON, "&BODY", 5, 0x00000059444F422BULL, 0, 0, 0 },
{ CONSTANT_AMPERSAND_KEY, COMMON, "&KEY", 4, 0x0000000059454B2AULL, 0, 0, 0 },
{ CONSTANT_AMPERSAND_ALLOW, COMMON, "&ALLOW-OTHER-KEYS", 17, 0x2872A27C9E9189DEULL, 0, 0, 0 },
{ CONSTANT_AMPERSAND_AUX, COMMON, "&AUX", 4, 0x000000005855412AULL, 0, 0, 0 },
{ CONSTANT_AMPERSAND_ENVIRONMENT, COMMON, "&ENVIRONMENT", 12, 0x4E4F5249AA9C8A7FULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ABORT, KEYWORD, "ABORT", 5, 0x00000054524F4246ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ABSOLUTE, KEYWORD, "ABSOLUTE", 8, 0x4554554C4F534249ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ACCESSOR, KEYWORD, "ACCESSOR", 8, 0x524F535345434349ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ADJUSTABLE, KEYWORD, "ADJUSTABLE", 10, 0x42415453554A8997ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_AFTER, KEYWORD, "AFTER", 5, 0x0000005245544646ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ALLOCATION, KEYWORD, "ALLOCATION", 10, 0x495441434F4C9A9AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ALLOW_OTHER_KEYS, KEYWORD, "ALLOW-OTHER-KEYS", 16, 0x27A872A27C9E9199ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_AND, KEYWORD, "AND", 3, 0x0000000000444E44ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_APPEND, KEYWORD, "APPEND", 6, 0x0000444E45505047ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ARRAY, KEYWORD, "ARRAY", 5, 0x0000005941525246ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ARGUMENT_PRECEDENCE_ORDER, KEYWORD, "ARGUMENT-PRECEDENCE-ORDER", 25, 0x5ED6DCDFC7DEE627ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ARGUMENTS, KEYWORD, "ARGUMENTS", 9, 0x544E454D5547529DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_AROUND, KEYWORD, "AROUND", 6, 0x0000444E554F5247ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_BASE, KEYWORD, "BASE", 4, 0x0000000045534146ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_BEFORE, KEYWORD, "BEFORE", 6, 0x000045524F464548ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_BLOCK, KEYWORD, "BLOCK", 5, 0x0000004B434F4C47ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CAPITALIZE, KEYWORD, "CAPITALIZE", 10, 0x494C4154495086A7ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CASE, KEYWORD, "CASE", 4, 0x0000000045534147ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CLASS, KEYWORD, "CLASS", 5, 0x0000005353414C48ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CIRCLE, KEYWORD, "CIRCLE", 6, 0x0000454C43524949ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_COMMON, KEYWORD, "COMMON", 6, 0x00004E4F4D4D4F49ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_COMMON_LISP_USER, KEYWORD, "COMMON-LISP-USER", 16, 0x1E72A1A47A9DA29CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_COMPILE_TOPLEVEL, KEYWORD, "COMPILE-TOPLEVEL", 16, 0x798AA28E9C9D9EA7ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CONC_NAME, KEYWORD, "CONC-NAME", 9, 0x4D414E2D434E4F91ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CONSTRUCTOR, KEYWORD, "CONSTRUCTOR", 11, 0x4355525453A09EA2ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_COPIER, KEYWORD, "COPIER", 6, 0x0000524549504F49ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_COUNT, KEYWORD, "COUNT", 5, 0x000000544E554F48ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CREATE, KEYWORD, "CREATE", 6, 0x0000455441455249ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CURRENT, KEYWORD, "CURRENT", 7, 0x00544E455252554AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DATUM, KEYWORD, "DATUM", 5, 0x0000004D55544149ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DECLARE, KEYWORD, "DECLARE", 7, 0x004552414C43454BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DEFAULT, KEYWORD, "DEFAULT", 7, 0x00544C554146454BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DEFAULT_INITARGS, KEYWORD, "DEFAULT-INITARGS", 16, 0x009B9E96958F939DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DEFAULTS, KEYWORD, "DEFAULTS", 8, 0x53544C554146454CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DESCRIPTION, KEYWORD, "DESCRIPTION", 11, 0x5450495243A19498ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DEVICE, KEYWORD, "DEVICE", 6, 0x000045434956454AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DIRECTION, KEYWORD, "DIRECTION", 9, 0x4F4954434552499BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DIRECTORY, KEYWORD, "DIRECTORY", 9, 0x524F5443455249A6ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DISPLACED_TO, KEYWORD, "DISPLACED-TO", 12, 0x4543414C9FA77694ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DISPLACED_INDEX_OFFSET, KEYWORD, "DISPLACED-INDEX-OFFSET", 22, 0x729BDAD5F1E2BCEDULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DOCUMENTATION, KEYWORD, "DOCUMENTATION", 13, 0x544E459BA48CA392ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DOWNCASE, KEYWORD, "DOWNCASE", 8, 0x455341434E574F4CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ELEMENT_TYPE, KEYWORD, "ELEMENT-TYPE", 12, 0x2D544E459295A5A5ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_END, KEYWORD, "END", 3, 0x0000000000444E48ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_END1, KEYWORD, "END1", 4, 0x0000000031444E49ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_END2, KEYWORD, "END2", 4, 0x0000000032444E49ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ENVIRONMENT, KEYWORD, "ENVIRONMENT", 11, 0x4D4E4F5249AA9C95ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ERROR, KEYWORD, "ERROR", 5, 0x000000524F52524AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ESCAPE, KEYWORD, "ESCAPE", 6, 0x000045504143534BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_EXECUTE, KEYWORD, "EXECUTE", 7, 0x004554554345584CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_EXPECTED_TYPE, KEYWORD, "EXPECTED-TYPE", 13, 0x4445548895A9AC7FULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_EXPORT, KEYWORD, "EXPORT", 6, 0x000054524F50584BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_EXTERNAL, KEYWORD, "EXTERNAL", 8, 0x4C414E524554584DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_EXTERNAL_FORMAT, KEYWORD, "EXTERNAL-FORMAT", 15, 0x4C958F9F97A39E81ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_FILL, KEYWORD, "FILL", 4, 0x000000004C4C494AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_FILL_POINTER, KEYWORD, "FILL-POINTER", 12, 0x494F502D9E919DA0ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_FROM_END, KEYWORD, "FROM-END", 8, 0x444E452D4D4F524EULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_FORMAT_ARGUMENTS, KEYWORD, "FORMAT-ARGUMENTS", 16, 0x1481A2869AA796A8ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_FORMAT_CONTROL, KEYWORD, "FORMAT-CONTROL", 14, 0x432DA0909FA69DA3ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_FULL, KEYWORD, "FULL", 4, 0x000000004C4C554AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_GENERIC_FUNCTION, KEYWORD, "GENERIC-FUNCTION", 16, 0x7B9292A6889C9A9DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_GENERIC_FUNCTION_CLASS, KEYWORD, "GENERIC-FUNCTION-CLASS", 22, 0x7B92E5F9C9E8DDD0ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_GENSYM, KEYWORD, "GENSYM", 6, 0x00004D59534E454DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_HOST, KEYWORD, "HOST", 4, 0x0000000054534F4CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IDENTITY, KEYWORD, "IDENTITY", 8, 0x595449544E454451ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IDENTITY_WITH_ONE_ARGUMENT, KEYWORD, "IDENTITY-WITH-ONE-ARGUMENT", 26, 0x6CF0CBE3F4D01D23ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IF_DOES_NOT_EXIST, KEYWORD, "IF-DOES-NOT-EXIST", 17, 0x009C9D94718195FCULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IF_EXISTS, KEYWORD, "IF-EXISTS", 9, 0x54534958452D46A5ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IMPORT_FROM, KEYWORD, "IMPORT-FROM", 11, 0x462D54524F9D9CA6ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INCLUDE, KEYWORD, "INCLUDE", 7, 0x004544554C434E50ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INDEX, KEYWORD, "INDEX", 5, 0x0000005845444E4EULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INITARG, KEYWORD, "INITARG", 7, 0x0047524154494E50ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INITFORM, KEYWORD, "INITFORM", 8, 0x4D524F4654494E51ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INITIAL_ELEMENT, KEYWORD, "INITIAL-ELEMENT", 15, 0x2DA08F8EA18E9A9DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INITIAL_CONTENTS, KEYWORD, "INITIAL-CONTENTS", 16, 0x00A08F8EA8979D9CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INITIAL_OFFSET, KEYWORD, "INITIAL-OFFSET", 14, 0x2D4C958EA78F94A6ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INITIAL_VALUE, KEYWORD, "INITIAL-VALUE", 13, 0x2D4C418EA9958FACULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INHERITED, KEYWORD, "INHERITED", 9, 0x4554495245484E96ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INPUT, KEYWORD, "INPUT", 5, 0x0000005455504E4EULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INSTANCE, KEYWORD, "INSTANCE", 8, 0x45434E4154534E51ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INTERACTIVE, KEYWORD, "INTERACTIVE", 11, 0x544341524599A49DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INTERACTIVE_FUNCTION, KEYWORD, "INTERACTIVE-FUNCTION", 20, 0x17919698C0E8EDFAULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INTERN, KEYWORD, "INTERN", 6, 0x00004E5245544E4FULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INTERNAL, KEYWORD, "INTERNAL", 8, 0x4C414E5245544E51ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INVERT, KEYWORD, "INVERT", 6, 0x0000545245564E4FULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IO, KEYWORD, "IO", 2, 0x0000000000004F4BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_JUNK_ALLOWED, KEYWORD, "JUNK-ALLOWED", 12, 0x4C4C412D8F93ACA5ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_KEY, KEYWORD, "KEY", 3, 0x000000000059454EULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LAMBDA_LIST, KEYWORD, "LAMBDA-LIST", 11, 0x4C2D414442A194A0ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LENGTH, KEYWORD, "LENGTH", 6, 0x00004854474E4552ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LEVEL, KEYWORD, "LEVEL", 5, 0x0000004C45564551ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LINEAR, KEYWORD, "LINEAR", 6, 0x00005241454E4952ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LINE, KEYWORD, "LINE", 4, 0x00000000454E4950ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LINE_RELATIVE, KEYWORD, "LINE-RELATIVE", 13, 0x4C4552729B979D9AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LINES, KEYWORD, "LINES", 5, 0x00000053454E4951ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LOAD_TOPLEVEL, KEYWORD, "LOAD-TOPLEVEL", 13, 0x504F5479899794A5ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LOCAL, KEYWORD, "LOCAL", 5, 0x0000004C41434F51ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_MANDATORY, KEYWORD, "MANDATORY", 9, 0x524F5441444E41AFULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_METACLASS, KEYWORD, "METACLASS", 9, 0x53414C43415445A9ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_METHOD, KEYWORD, "METHOD", 6, 0x0000444F48544553ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_METHOD_CLASS, KEYWORD, "METHOD-CLASS", 12, 0x432D444F9BA786A5ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_METHOD_COMBINATION, KEYWORD, "METHOD-COMBINATION", 18, 0x0C81859D9196E0FDULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_MISER, KEYWORD, "MISER", 5, 0x0000005245534952ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_MISER_WIDTH, KEYWORD, "MISER-WIDTH", 11, 0x49572D52459B9D9CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_MOST_SPECIFIC_FIRST, KEYWORD, "MOST-SPECIFIC-FIRST", 19, 0x0E9680709DEDEBF5ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_MOST_SPECIFIC_LAST, KEYWORD, "MOST-SPECIFIC-LAST", 18, 0x069C80709D99ECF5ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_NAME, KEYWORD, "NAME", 4, 0x00000000454D4152ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_NAMED, KEYWORD, "NAMED", 5, 0x00000044454D4153ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_NEW_VERSION, KEYWORD, "NEW-VERSION", 11, 0x535245562DA594A2ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_NEWEST, KEYWORD, "NEWEST", 6, 0x0000545345574554ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_NICKNAMES, KEYWORD, "NICKNAMES", 9, 0x454D414E4B4349AAULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_NO_ERROR, KEYWORD, "NO-ERROR", 8, 0x524F5252452D4F56ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_NOT, KEYWORD, "NOT", 3, 0x0000000000544F51ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OBJECT, KEYWORD, "OBJECT", 6, 0x00005443454A4255ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OPERANDS, KEYWORD, "OPERANDS", 8, 0x53444E4152455057ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OPERATION, KEYWORD, "OPERATION", 9, 0x4F495441524550A6ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OPERATOR, KEYWORD, "OPERATOR", 8, 0x524F544152455057ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OR, KEYWORD, "OR", 2, 0x0000000000005251ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ORDER, KEYWORD, "ORDER", 5, 0x0000005245445254ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OUTPUT, KEYWORD, "OUTPUT", 6, 0x0000545550545555ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OUTPUT_FILE, KEYWORD, "OUTPUT-FILE", 11, 0x462D54555099A1A3ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OVERRIDE, KEYWORD, "OVERRIDE", 8, 0x4544495252455657ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OVERWRITE, KEYWORD, "OVERWRITE", 9, 0x544952575245569DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PACKAGE, KEYWORD, "PACKAGE", 7, 0x004547414B434157ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PATHNAME, KEYWORD, "PATHNAME", 8, 0x454D414E48544158ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PER_LINE_PREFIX, KEYWORD, "PER-LINE-PREFIX", 15, 0x45A6929272A4958CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PPRINT_DISPATCH, KEYWORD, "PPRINT-DISPATCH", 15, 0x447597A28AA2A3A8ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PREDICATE, KEYWORD, "PREDICATE", 9, 0x544143494445529EULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PREFIX, KEYWORD, "PREFIX", 6, 0x0000584946455256ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PRESERVE, KEYWORD, "PRESERVE", 8, 0x4556524553455258ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PRESERVE_WHITESPACE, KEYWORD, "PRESERVE-WHITESPACE", 19, 0x15A997999CD2ECD1ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PRETTY, KEYWORD, "PRETTY", 6, 0x0000595454455256ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PRINT, KEYWORD, "PRINT", 5, 0x000000544E495255ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PRINT_FUNCTION, KEYWORD, "PRINT-FUNCTION", 14, 0x55467BA3979D95ACULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PRINT_OBJECT, KEYWORD, "PRINT-OBJECT", 12, 0x424F2D54A28C97A6ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PROBE, KEYWORD, "PROBE", 5, 0x00000045424F5255ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_RADIX, KEYWORD, "RADIX", 5, 0x0000005849444157ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_READ_ONLY, KEYWORD, "READ-ONLY", 9, 0x4C4E4F2D444145B4ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_READABLY, KEYWORD, "READABLY", 8, 0x594C42414441455AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_READER, KEYWORD, "READER", 6, 0x0000524544414558ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_REHASH_SIZE, KEYWORD, "REHASH-SIZE", 11, 0x532D4853418D9FA6ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_REHASH_THRESHOLD, KEYWORD, "REHASH-THRESHOLD", 16, 0x1879979B948D97AAULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_RELATIVE, KEYWORD, "RELATIVE", 8, 0x45564954414C455AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_RENAME, KEYWORD, "RENAME", 6, 0x0000454D414E4558ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_RENAME_AND_DELETE, KEYWORD, "RENAME-AND-DELETE", 17, 0x15729192857B89F6ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_REPORT, KEYWORD, "REPORT", 6, 0x000054524F504558ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_REPORT_FUNCTION, KEYWORD, "REPORT-FUNCTION", 15, 0x467BA39BA39393B6ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_REQUIRED, KEYWORD, "REQUIRED", 8, 0x444552495551455AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_RIGHT_MARGIN, KEYWORD, "RIGHT-MARGIN", 12, 0x414D2D54969090B0ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SECTION, KEYWORD, "SECTION", 7, 0x004E4F495443455AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SECTION_RELATIVE, KEYWORD, "SECTION-RELATIVE", 16, 0x72A4989D958F8AB5ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SHADOW, KEYWORD, "SHADOW", 6, 0x0000574F44414859ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SHADOWING_IMPORT_FROM, KEYWORD, "SHADOWING-IMPORT-FROM", 21, 0x229BA6ECE0DCBBDCULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SIZE, KEYWORD, "SIZE", 4, 0x00000000455A4957ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SLOT_NAMES, KEYWORD, "SLOT-NAMES", 10, 0x4D414E2D544F9FA2ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_START, KEYWORD, "START", 5, 0x0000005452415458ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_START1, KEYWORD, "START1", 6, 0x0000315452415459ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_START2, KEYWORD, "START2", 6, 0x0000325452415459ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_STREAM, KEYWORD, "STREAM", 6, 0x00004D4145525459ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SUFFIX, KEYWORD, "SUFFIX", 6, 0x0000584946465559ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SUPERSEDE, KEYWORD, "SUPERSEDE", 9, 0x44455352455055A1ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_TEST, KEYWORD, "TEST", 4, 0x0000000054534558ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_TEST_FUNCTION, KEYWORD, "TEST-FUNCTION", 13, 0x4E55467BA39C99A4ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_TEST_NOT, KEYWORD, "TEST-NOT", 8, 0x544F4E2D5453455CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_TYPE, KEYWORD, "TYPE", 4, 0x0000000045505958ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_UNSPECIFIC, KEYWORD, "UNSPECIFIC", 10, 0x46494345505391A8ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_UP, KEYWORD, "UP", 2, 0x0000000000005057ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_UPCASE, KEYWORD, "UPCASE", 6, 0x000045534143505BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_USE, KEYWORD, "USE", 3, 0x0000000000455358ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_VERBOSE, KEYWORD, "VERBOSE", 7, 0x0045534F4252455DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_VERSION, KEYWORD, "VERSION", 7, 0x004E4F495352455DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_WILD, KEYWORD, "WILD", 4, 0x00000000444C495BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_WILD_INFERIORS, KEYWORD, "WILD-INFERIORS", 14, 0x464E9C7F93959BAAULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_WRITER, KEYWORD, "WRITER", 6, 0x000052455449525DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ARGS, KEYWORD, "ARGS", 4, 0x0000000053475245ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CACHE, KEYWORD, "CACHE", 5, 0x0000004548434148ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_EXIT, KEYWORD, "EXIT", 4, 0x0000000054495849ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IF_ERROR_EXISTS, KEYWORD, "IF-ERROR-EXISTS", 15, 0x52A2A6A58E858B85ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IF_INPUT_DOES_NOT_EXIST, KEYWORD, "IF-INPUT-DOES-NOT-EXIST", 23, 0x23F7D0EAE6C1B7E1ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IF_OUTPUT_EXISTS, KEYWORD, "IF-OUTPUT-EXISTS", 16, 0x28A4A79EA77273ADULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PIPE, KEYWORD, "PIPE", 4, 0x0000000045504954ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PROGRAM, KEYWORD, "PROGRAM", 7, 0x004D4152474F5257ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SEARCH, KEYWORD, "SEARCH", 6, 0x0000484352414559ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_VALUE, KEYWORD, "VALUE", 5, 0x00000045554C415BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_WAIT, KEYWORD, "WAIT", 4, 0x000000005449415BULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASTERISK, COMMON, "*", 1, 0x000000000000002BULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASTERISK2, COMMON, "**", 2, 0x0000000000002A2CULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASTERISK3, COMMON, "***", 3, 0x00000000002A2A2DULL, 0, 0, 0 },
{ CONSTANT_COMMON_PLUS, COMMON, "+", 1, 0x000000000000002CULL, 0, 0, 0 },
{ CONSTANT_COMMON_PLUS2, COMMON, "++", 2, 0x0000000000002B2DULL, 0, 0, 0 },
{ CONSTANT_COMMON_PLUS3, COMMON, "+++", 3, 0x00000000002B2B2EULL, 0, 0, 0 },
{ CONSTANT_COMMON_MINUS, COMMON, "-", 1, 0x000000000000002EULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLASH, COMMON, "/", 1, 0x0000000000000030ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLASH2, COMMON, "//", 2, 0x0000000000002F31ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLASH3, COMMON, "///", 3, 0x00000000002F2F32ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ONE_PLUS, COMMON, "1+", 2, 0x0000000000002B33ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ONE_MINUS, COMMON, "1-", 2, 0x0000000000002D33ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBER_EQUAL, COMMON, "=", 1, 0x000000000000003EULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBER_NOT_EQUAL, COMMON, "/=", 2, 0x0000000000003D31ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBER_LESS, COMMON, "<", 1, 0x000000000000003DULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBER_GREATER, COMMON, ">", 1, 0x000000000000003FULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBER_LESS_EQUAL, COMMON, "<=", 2, 0x0000000000003D3EULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBER_GREATER_EQUAL, COMMON, ">=", 2, 0x0000000000003D40ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ABORT, COMMON, "ABORT", 5, 0x00000054524F4246ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ABS, COMMON, "ABS", 3, 0x0000000000534244ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ACONS, COMMON, "ACONS", 5, 0x000000534E4F4346ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ACOS, COMMON, "ACOS", 4, 0x00000000534F4345ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ACOSH, COMMON, "ACOSH", 5, 0x00000048534F4346ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ADD_METHOD, COMMON, "ADD-METHOD", 10, 0x4854454D2D44889AULL, 0, 0, 0 },
{ CONSTANT_COMMON_ADJOIN, COMMON, "ADJOIN", 6, 0x00004E494F4A4447ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ADJUST_ARRAY, COMMON, "ADJUST-ARRAY", 12, 0x412D5453AE8B969FULL, 0, 0, 0 },
{ CONSTANT_COMMON_ADJUSTABLE_ARRAY_P, COMMON, "ADJUSTABLE-ARRAY-P", 18, 0x1B82A6A59677D9CCULL, 0, 0, 0 },
{ CONSTANT_COMMON_ALPHA_CHAR_P, COMMON, "ALPHA-CHAR-P", 12, 0x48432D41987D9E8EULL, 0, 0, 0 },
{ CONSTANT_COMMON_ALPHANUMERICP, COMMON, "ALPHANUMERICP", 13, 0x4D554E918B999E93ULL, 0, 0, 0 },
{ CONSTANT_COMMON_AND, COMMON, "AND", 3, 0x0000000000444E44ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ATOM, COMMON, "ATOM", 4, 0x000000004D4F5445ULL, 0, 0, 0 },
{ CONSTANT_COMMON_APPEND, COMMON, "APPEND", 6, 0x0000444E45505047ULL, 0, 0, 0 },
{ CONSTANT_COMMON_APPLY, COMMON, "APPLY", 5, 0x000000594C505046ULL, 0, 0, 0 },
{ CONSTANT_COMMON_APROPOS, COMMON, "APROPOS", 7, 0x00534F504F525048ULL, 0, 0, 0 },
{ CONSTANT_COMMON_APROPOS_LIST, COMMON, "APROPOS-LIST", 12, 0x2D534F50A3A59999ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARITHMETIC_ERROR, COMMON, "ARITHMETIC-ERROR", 16, 0x26949F9A9976959AULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARITHMETIC_ERROR_OPERANDS, COMMON, "ARITHMETIC-ERROR-OPERANDS", 25, 0x6AE2E0ECDEC6E523ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARITHMETIC_ERROR_OPERATION, COMMON, "ARITHMETIC-ERROR-OPERATION", 26, 0x6FE8E0ECDEC73320ULL, 0, 0, 0 },
{ CONSTANT_COMMON_AREF, COMMON, "AREF", 4, 0x0000000046455245ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY, COMMON, "ARRAY", 5, 0x0000005941525246ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_DIMENSION, COMMON, "ARRAY-DIMENSION", 15, 0x49927CA294A0979DULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_DIMENSION_LIMIT, COMMON, "ARRAY-DIMENSION-LIMIT", 21, 0x76927CF6DDEDE0EFULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_DIMENSIONS, COMMON, "ARRAY-DIMENSIONS", 16, 0x1C927CA294A0979EULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_ELEMENT_TYPE, COMMON, "ARRAY-ELEMENT-TYPE", 18, 0x25995AAD8F97E4E8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_HAS_FILL_POINTER_P, COMMON, "ARRAY-HAS-FILL-POINTER-P", 24, 0x61A2CBEADEE6C8FBULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_DISPLACEMENT, COMMON, "ARRAY-DISPLACEMENT", 18, 0x0E91729C829EF6F4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_IN_BOUNDS_P, COMMON, "ARRAY-IN-BOUNDS-P", 17, 0x7B9C71A796A194CFULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_RANK, COMMON, "ARRAY-RANK", 10, 0x41522D5941529D99ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_RANK_LIMIT, COMMON, "ARRAY-RANK-LIMIT", 16, 0x159B7AA28D7F9D9FULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_ROW_MAJOR_INDEX, COMMON, "ARRAY-ROW-MAJOR-INDEX", 21, 0x7CA47CFBC7E3CDF6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_TOTAL_SIZE, COMMON, "ARRAY-TOTAL-SIZE", 16, 0x14AE76AC6E9E93A5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_TOTAL_SIZE_LIMIT, COMMON, "ARRAY-TOTAL-SIZE-LIMIT", 22, 0x14AECAF5BBE7DFD8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAYP, COMMON, "ARRAYP", 6, 0x0000505941525247ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASH, COMMON, "ASH", 3, 0x0000000000485344ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASIN, COMMON, "ASIN", 4, 0x000000004E495345ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASINH, COMMON, "ASINH", 5, 0x000000484E495346ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASSERT, COMMON, "ASSERT", 6, 0x0000545245535347ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASSOC, COMMON, "ASSOC", 5, 0x000000434F535346ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASSOC_IF, COMMON, "ASSOC-IF", 8, 0x46492D434F535349ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASSOC_IF_NOT, COMMON, "ASSOC-IF-NOT", 12, 0x46492D43A3A2A17AULL, 0, 0, 0 },
{ CONSTANT_COMMON_ATAN, COMMON, "ATAN", 4, 0x000000004E415445ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ATANH, COMMON, "ATANH", 5, 0x000000484E415446ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BASE_CHAR, COMMON, "BASE-CHAR", 9, 0x4148432D4553419DULL, 0, 0, 0 },
{ CONSTANT_COMMON_BASE_STRING, COMMON, "BASE-STRING", 11, 0x5254532D459A8F96ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIGNUM, COMMON, "BIGNUM", 6, 0x00004D554E474948ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT, COMMON, "BIT", 3, 0x0000000000544945ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_AND, COMMON, "BIT-AND", 7, 0x00444E412D544949ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_ANDC1, COMMON, "BIT-ANDC1", 9, 0x43444E412D54497CULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_ANDC2, COMMON, "BIT-ANDC2", 9, 0x43444E412D54497DULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_EQV, COMMON, "BIT-EQV", 7, 0x005651452D544949ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_IOR, COMMON, "BIT-IOR", 7, 0x00524F492D544949ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_NAND, COMMON, "BIT-NAND", 8, 0x444E414E2D54494AULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_NOR, COMMON, "BIT-NOR", 7, 0x00524F4E2D544949ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_NOT, COMMON, "BIT-NOT", 7, 0x00544F4E2D544949ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_ORC1, COMMON, "BIT-ORC1", 8, 0x3143524F2D54494AULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_ORC2, COMMON, "BIT-ORC2", 8, 0x3243524F2D54494AULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_VECTOR, COMMON, "BIT-VECTOR", 10, 0x544345562D549B9BULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_VECTOR_P, COMMON, "BIT-VECTOR-P", 12, 0x544345567D819B9DULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_XOR, COMMON, "BIT-XOR", 7, 0x00524F582D544949ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BLOCK, COMMON, "BLOCK", 5, 0x0000004B434F4C47ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE, COMMON, "BOOLE", 5, 0x000000454C4F4F47ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_1, COMMON, "BOOLE-1", 7, 0x00312D454C4F4F49ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_2, COMMON, "BOOLE-2", 7, 0x00322D454C4F4F49ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_AND, COMMON, "BOOLE-AND", 9, 0x4E412D454C4F4F8FULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_ANDC1, COMMON, "BOOLE-ANDC1", 11, 0x4E412D454C809291ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_ANDC2, COMMON, "BOOLE-ANDC2", 11, 0x4E412D454C819291ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_C1, COMMON, "BOOLE-C1", 8, 0x31432D454C4F4F4AULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_C2, COMMON, "BOOLE-C2", 8, 0x32432D454C4F4F4AULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_CLR, COMMON, "BOOLE-CLR", 9, 0x4C432D454C4F4F9DULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_EQV, COMMON, "BOOLE-EQV", 9, 0x51452D454C4F4FA1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_IOR, COMMON, "BOOLE-IOR", 9, 0x4F492D454C4F4F9DULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_NAND, COMMON, "BOOLE-NAND", 10, 0x414E2D454C4F939AULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_NOR, COMMON, "BOOLE-NOR", 9, 0x4F4E2D454C4F4F9DULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_ORC1, COMMON, "BOOLE-ORC1", 10, 0x524F2D454C4F808FULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_ORC2, COMMON, "BOOLE-ORC2", 10, 0x524F2D454C4F818FULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_SET, COMMON, "BOOLE-SET", 9, 0x45532D454C4F4F9FULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_XOR, COMMON, "BOOLE-XOR", 9, 0x4F582D454C4F4F9DULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLEAN, COMMON, "BOOLEAN", 7, 0x004E41454C4F4F49ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOTH_CASE_P, COMMON, "BOTH-CASE-P", 11, 0x5341432D48A47C92ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOUNDP, COMMON, "BOUNDP", 6, 0x000050444E554F48ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BREAK, COMMON, "BREAK", 5, 0x0000004B41455247ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BROADCAST_STREAM, COMMON, "BROADCAST-STREAM", 16, 0x2082889695A27FA6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BROADCAST_STREAM_STREAMS, COMMON, "BROADCAST-STREAM-STREAMS", 24, 0x73CFC9DBE7F6D2DBULL, 0, 0, 0 },
{ CONSTANT_COMMON_BUILT_IN_CLASS, COMMON, "BUILT-IN-CLASS", 14, 0x4E4980A78D95987DULL, 0, 0, 0 },
{ CONSTANT_COMMON_BUTLAST, COMMON, "BUTLAST", 7, 0x005453414C545549ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BYTE, COMMON, "BYTE", 4, 0x0000000045545946ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BYTE_SIZE, COMMON, "BYTE-SIZE", 9, 0x5A49532D45545990ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BYTE_POSITION, COMMON, "BYTE-POSITION", 13, 0x534F507B949DAD98ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CALL_ARGUMENTS_LIMIT, COMMON, "CALL-ARGUMENTS-LIMIT", 20, 0x137F9481EEDADBF5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CALL_METHOD, COMMON, "CALL-METHOD", 11, 0x54454D2D4C909096ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CALL_NEXT_METHOD, COMMON, "CALL-NEXT-METHOD", 16, 0x1C94968191996EA7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAR, COMMON, "CAR", 3, 0x0000000000524146ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDR, COMMON, "CDR", 3, 0x0000000000524446ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAAR, COMMON, "CAAR", 4, 0x0000000052414147ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CADR, COMMON, "CADR", 4, 0x0000000052444147ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDAR, COMMON, "CDAR", 4, 0x0000000052414447ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDDR, COMMON, "CDDR", 4, 0x0000000052444447ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAAAR, COMMON, "CAAAR", 5, 0x0000005241414148ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAADR, COMMON, "CAADR", 5, 0x0000005244414148ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CADAR, COMMON, "CADAR", 5, 0x0000005241444148ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CADDR, COMMON, "CADDR", 5, 0x0000005244444148ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDAAR, COMMON, "CDAAR", 5, 0x0000005241414448ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDADR, COMMON, "CDADR", 5, 0x0000005244414448ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDDAR, COMMON, "CDDAR", 5, 0x0000005241444448ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDDDR, COMMON, "CDDDR", 5, 0x0000005244444448ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAAAAR, COMMON, "CAAAAR", 6, 0x0000524141414149ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAAADR, COMMON, "CAAADR", 6, 0x0000524441414149ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAADAR, COMMON, "CAADAR", 6, 0x0000524144414149ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAADDR, COMMON, "CAADDR", 6, 0x0000524444414149ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CADAAR, COMMON, "CADAAR", 6, 0x0000524141444149ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CADADR, COMMON, "CADADR", 6, 0x0000524441444149ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CADDAR, COMMON, "CADDAR", 6, 0x0000524144444149ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CADDDR, COMMON, "CADDDR", 6, 0x0000524444444149ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDAAAR, COMMON, "CDAAAR", 6, 0x0000524141414449ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDAADR, COMMON, "CDAADR", 6, 0x0000524441414449ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDADAR, COMMON, "CDADAR", 6, 0x0000524144414449ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDADDR, COMMON, "CDADDR", 6, 0x0000524444414449ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDDAAR, COMMON, "CDDAAR", 6, 0x0000524141444449ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDDADR, COMMON, "CDDADR", 6, 0x0000524441444449ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDDDAR, COMMON, "CDDDAR", 6, 0x0000524144444449ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDDDDR, COMMON, "CDDDDR", 6, 0x0000524444444449ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CASE, COMMON, "CASE", 4, 0x0000000045534147ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CATCH, COMMON, "CATCH", 5, 0x0000004843544148ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CCASE, COMMON, "CCASE", 5, 0x0000004553414348ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CEILING, COMMON, "CEILING", 7, 0x00474E494C49454AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CELL_ERROR, COMMON, "CELL-ERROR", 10, 0x5252452D4C4C979CULL, 0, 0, 0 },
{ CONSTANT_COMMON_CELL_ERROR_NAME, COMMON, "CELL-ERROR-NAME", 15, 0x5297926E9A7997A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CERROR, COMMON, "CERROR", 6, 0x0000524F52524549ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR, COMMON, "CHAR", 4, 0x0000000052414847ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHARACTER, COMMON, "CHARACTER", 9, 0x455443415241489EULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHARACTERP, COMMON, "CHARACTERP", 10, 0x455443415241989FULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_EQL, COMMON, "CHAR=", 5, 0x0000003D52414848ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_NOT_EQL, COMMON, "CHAR/=", 6, 0x00003D2F52414849ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_LESS, COMMON, "CHAR<", 5, 0x0000003C52414848ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_GREATER, COMMON, "CHAR>", 5, 0x0000003E52414848ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_LESS_EQUAL, COMMON, "CHAR<=", 6, 0x00003D3C52414849ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_GREATER_EQUAL, COMMON, "CHAR>=", 6, 0x00003D3E52414849ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_CODE, COMMON, "CHAR-CODE", 9, 0x444F432D52414891ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_CODE_LIMIT, COMMON, "CHAR-CODE-LIMIT", 15, 0x44A38C7A9B8D7597ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_DOWNCASE, COMMON, "CHAR-DOWNCASE", 13, 0x574F4472A5828B9EULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_EQUAL, COMMON, "CHAR-EQUAL", 10, 0x5551452D5241948EULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_GREATERP, COMMON, "CHAR-GREATERP", 13, 0x4552477DA4869C91ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_INT, COMMON, "CHAR-INT", 8, 0x544E492D5241484BULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_LESSP, COMMON, "CHAR-LESSP", 10, 0x53454C2D524198A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_NAME, COMMON, "CHAR-NAME", 9, 0x4D414E2D52414891ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_NOT_EQUAL, COMMON, "CHAR-NOT-EQUAL", 14, 0x544F9A6EA7928D7EULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_NOT_GREATERP, COMMON, "CHAR-NOT-GREATERP", 17, 0x2694A26E97938FD1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_NOT_LESSP, COMMON, "CHAR-NOT-LESSP", 14, 0x544F9E80A586947EULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_UPCASE, COMMON, "CHAR-UPCASE", 11, 0x4350552D52869B8FULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHECK_TYPE, COMMON, "CHECK-TYPE", 10, 0x59542D4B43458D9DULL, 0, 0, 0 },
{ CONSTANT_COMMON_CIS, COMMON, "CIS", 3, 0x0000000000534946ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CLASS, COMMON, "CLASS", 5, 0x0000005353414C48ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CLASS_OF, COMMON, "CLASS-OF", 8, 0x464F2D5353414C4BULL, 0, 0, 0 },
{ CONSTANT_COMMON_CLEAR_INPUT, COMMON, "CLEAR-INPUT", 11, 0x4E492D524199A19EULL, 0, 0, 0 },
{ CONSTANT_COMMON_CLEAR_OUTPUT, COMMON, "CLEAR-OUTPUT", 12, 0x554F2D52959A9CA3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CLOSE, COMMON, "CLOSE", 5, 0x00000045534F4C48ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CLRHASH, COMMON, "CLRHASH", 7, 0x0048534148524C4AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CODE_CHAR, COMMON, "CODE-CHAR", 9, 0x4148432D45444F9EULL, 0, 0, 0 },
{ CONSTANT_COMMON_COERCE, COMMON, "COERCE", 6, 0x0000454352454F49ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILATION_SPEED, COMMON, "COMPILATION-SPEED", 17, 0x19869C9C7D9B9EE1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILE, COMMON, "COMPILE", 7, 0x00454C49504D4F4AULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILED_FUNCTION, COMMON, "COMPILED-FUNCTION", 17, 0x138EA08C9EA295CFULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILED_FUNCTION_P, COMMON, "COMPILED-FUNCTION-P", 19, 0x138EA08C9EF2C2D1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILER_MACRO, COMMON, "COMPILER-MACRO", 14, 0x52459B9B938E9C7EULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILER_MACRO_FUNCTION, COMMON, "COMPILER-MACRO-FUNCTION", 23, 0x18C0EAE4E7D1EADCULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPLEMENT, COMMON, "COMPLEMENT", 10, 0x454D454C504DA39BULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPLEX, COMMON, "COMPLEX", 7, 0x0058454C504D4F4AULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPLEXP, COMMON, "COMPLEXP", 8, 0x5058454C504D4F4BULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPUTE_APPLICABLE_METHODS, COMMON, "COMPUTE-APPLICABLE-METHODS", 26, 0x3ECEEBE3E9CB382EULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPUTE_RESTARTS, COMMON, "COMPUTE-RESTARTS", 16, 0x0099A696A4A094A5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONCATENATE, COMMON, "CONCATENATE", 11, 0x4E4554414393A38FULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONCATENATED_STREAM, COMMON, "CONCATENATED-STREAM", 19, 0x2099A76E87E0E4DCULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONCATENATED_STREAM_STREAMS, COMMON, "CONCATENATED-STREAM-STREAMS", 27, 0x65EBFBC1B5343225ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COND, COMMON, "COND", 4, 0x00000000444E4F47ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONDITION, COMMON, "CONDITION", 9, 0x4F495449444E4F9AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONJUGATE, COMMON, "CONJUGATE", 9, 0x544147554A4E4F91ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILE_FILE, COMMON, "COMPILE-FILE", 12, 0x2D454C4995999895ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILE_FILE_PATHNAME, COMMON, "COMPILE-FILE-PATHNAME", 21, 0x01869CBBE2DAE6E6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONS, COMMON, "CONS", 4, 0x00000000534E4F47ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONSP, COMMON, "CONSP", 5, 0x00000050534E4F48ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONSTANTLY, COMMON, "CONSTANTLY", 10, 0x544E4154534EA899ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONSTANTP, COMMON, "CONSTANTP", 9, 0x544E4154534E4F9CULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONTINUE, COMMON, "CONTINUE", 8, 0x45554E49544E4F4BULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONTROL_ERROR, COMMON, "CONTROL-ERROR", 13, 0x2D4C4FA4A3A0A195ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_ALIST, COMMON, "COPY-ALIST", 10, 0x494C412D5950A3A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_LIST, COMMON, "COPY-LIST", 9, 0x53494C2D59504FA0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_PPRINT_DISPATCH, COMMON, "COPY-PPRINT-DISPATCH", 20, 0x22A39971CEE7F1E1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_READTABLE, COMMON, "COPY-READTABLE", 14, 0x414597799B91A395ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_SEQ, COMMON, "COPY-SEQ", 8, 0x5145532D59504F4BULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_STRUCTURE, COMMON, "COPY-STRUCTURE", 14, 0x5254987FAEA492A6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_SYMBOL, COMMON, "COPY-SYMBOL", 11, 0x4D59532D599C9E90ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_TREE, COMMON, "COPY-TREE", 9, 0x4552542D59504F91ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COS, COMMON, "COS", 3, 0x0000000000534F46ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COSH, COMMON, "COSH", 4, 0x0000000048534F47ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COUNT, COMMON, "COUNT", 5, 0x000000544E554F48ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COUNT_IF, COMMON, "COUNT-IF", 8, 0x46492D544E554F4BULL, 0, 0, 0 },
{ CONSTANT_COMMON_COUNT_IF_NOT, COMMON, "COUNT-IF-NOT", 12, 0x46492D54A2A49D7CULL, 0, 0, 0 },
{ CONSTANT_COMMON_CTYPECASE, COMMON, "CTYPECASE", 9, 0x5341434550595491ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEBUG, COMMON, "DEBUG", 5, 0x0000004755424549ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DECF, COMMON, "DECF", 4, 0x0000000046434548ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DECLARATION, COMMON, "DECLARATION", 11, 0x544152414C919498ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DECLARE, COMMON, "DECLARE", 7, 0x004552414C43454BULL, 0, 0, 0 },
{ CONSTANT_COMMON_DECLAIM, COMMON, "DECLAIM", 7, 0x004D49414C43454BULL, 0, 0, 0 },
{ CONSTANT_COMMON_DECODE_FLOAT, COMMON, "DECODE-FLOAT", 12, 0x462D4544A384949CULL, 0, 0, 0 },
{ CONSTANT_COMMON_DECODE_UNIVERSAL_TIME, COMMON, "DECODE-UNIVERSAL-TIME", 21, 0x216E98DBE1E2E2D4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFCLASS, COMMON, "DEFCLASS", 8, 0x5353414C4346454CULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFCONSTANT, COMMON, "DEFCONSTANT", 11, 0x54534E4F439A9390ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFGENERIC, COMMON, "DEFGENERIC", 10, 0x52454E4547468897ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFINE_COMPILER_MACRO, COMMON, "DEFINE-COMPILER-MACRO", 21, 0x707F8AE9E4D9D3F5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFINE_CONDITION, COMMON, "DEFINE-CONDITION", 16, 0x117C8EA2928A93A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFINE_METHOD_COMBINATION, COMMON, "DEFINE-METHOD-COMBINATION", 25, 0x6BB9C6D3E6D7DC3DULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFINE_MODIFY_MACRO, COMMON, "DEFINE-MODIFY-MACRO", 19, 0x0E7A72A78FDEDBE9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFINE_SETF_EXPANDER, COMMON, "DEFINE-SETF-EXPANDER", 20, 0x147D9D93C8D1DDEBULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFINE_SYMBOL_MACRO, COMMON, "DEFINE-SYMBOL-MACRO", 19, 0x147A729A98D7E4F3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFMACRO, COMMON, "DEFMACRO", 8, 0x4F5243414D46454CULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFMETHOD, COMMON, "DEFMETHOD", 9, 0x4F4854454D464591ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFPACKAGE, COMMON, "DEFPACKAGE", 10, 0x414B434150468A95ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFPARAMETER, COMMON, "DEFPARAMETER", 12, 0x4D415241A28B9995ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFSETF, COMMON, "DEFSETF", 7, 0x004654455346454BULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFSTRUCT, COMMON, "DEFSTRUCT", 9, 0x43555254534645A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFTYPE, COMMON, "DEFTYPE", 7, 0x004550595446454BULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFUN, COMMON, "DEFUN", 5, 0x0000004E55464549ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFVAR, COMMON, "DEFVAR", 6, 0x000052415646454AULL, 0, 0, 0 },
{ CONSTANT_COMMON_DELETE, COMMON, "DELETE", 6, 0x00004554454C454AULL, 0, 0, 0 },
{ CONSTANT_COMMON_DELETE_DUPLICATES, COMMON, "DELETE-DUPLICATES", 17, 0x098186978E9895FDULL, 0, 0, 0 },
{ CONSTANT_COMMON_DELETE_FILE, COMMON, "DELETE-FILE", 11, 0x462D455445919198ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DELETE_IF, COMMON, "DELETE-IF", 9, 0x492D4554454C4593ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DELETE_IF_NOT, COMMON, "DELETE-IF-NOT", 13, 0x492D45A8949A7297ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DELETE_PACKAGE, COMMON, "DELETE-PACKAGE", 14, 0x502D8A9B86978893ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DENOMINATOR, COMMON, "DENOMINATOR", 11, 0x414E494D4FA094A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEPOSIT_FIELD, COMMON, "DEPOSIT-FIELD", 13, 0x2D5449979B958E97ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DESCRIBE, COMMON, "DESCRIBE", 8, 0x454249524353454CULL, 0, 0, 0 },
{ CONSTANT_COMMON_DESCRIBE_OBJECT, COMMON, "DESCRIBE-OBJECT", 15, 0x45968C978D959480ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DESTRUCTURING_BIND, COMMON, "DESTRUCTURING-BIND", 18, 0x1D858299A29CDBF9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DIGIT_CHAR, COMMON, "DIGIT-CHAR", 10, 0x48432D5449479B8FULL, 0, 0, 0 },
{ CONSTANT_COMMON_DIGIT_CHAR_P, COMMON, "DIGIT-CHAR-P", 12, 0x48432D5499749B91ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DIRECTORY, COMMON, "DIRECTORY", 9, 0x524F5443455249A6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DIRECTORY_NAMESTRING, COMMON, "DIRECTORY-NAMESTRING", 20, 0x26A29990CDEEC003ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DISASSEMBLE, COMMON, "DISASSEMBLE", 11, 0x4D45535341989591ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DIVISION_BY_ZERO, COMMON, "DIVISION-BY-ZERO", 16, 0x1DA18EAD76AF8B81ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DO, COMMON, "DO", 2, 0x0000000000004F46ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DOA, COMMON, "DO*", 3, 0x00000000002A4F47ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DOCUMENTATION, COMMON, "DOCUMENTATION", 13, 0x544E459BA48CA392ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DOLIST, COMMON, "DOLIST", 6, 0x00005453494C4F4AULL, 0, 0, 0 },
{ CONSTANT_COMMON_DOTIMES, COMMON, "DOTIMES", 7, 0x0053454D49544F4BULL, 0, 0, 0 },
{ CONSTANT_COMMON_DOUBLE_FLOAT, COMMON, "DOUBLE-FLOAT", 12, 0x462D454C96969E9CULL, 0, 0, 0 },
{ CONSTANT_COMMON_DOUBLE_FLOAT_EPSILON, COMMON, "DOUBLE-FLOAT-EPSILON", 20, 0x197D8A79E4E5EAEDULL, 0, 0, 0 },
{ CONSTANT_COMMON_DOUBLE_FLOAT_NEGATIVE_EPSILON, COMMON, "DOUBLE-FLOAT-NEGATIVE-EPSILON", 29, 0x5DB7C10D3C2C3C41ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DO_ALL_SYMBOLS, COMMON, "DO-ALL-SYMBOLS", 14, 0x532D9F98906F9CABULL, 0, 0, 0 },
{ CONSTANT_COMMON_DO_EXTERNAL_SYMBOLS, COMMON, "DO-EXTERNAL-SYMBOLS", 19, 0x1492ADAB72CCDCF4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DO_SYMBOLS, COMMON, "DO-SYMBOLS", 10, 0x4F424D59532DA29AULL, 0, 0, 0 },
{ CONSTANT_COMMON_DPB, COMMON, "DPB", 3, 0x0000000000425047ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DRIBBLE, COMMON, "DRIBBLE", 7, 0x00454C424249524BULL, 0, 0, 0 },
{ CONSTANT_COMMON_DYNAMIC_EXTENT, COMMON, "DYNAMIC-EXTENT", 14, 0x2D439D9B86A2B197ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ECASE, COMMON, "ECASE", 5, 0x000000455341434AULL, 0, 0, 0 },
{ CONSTANT_COMMON_ECHO_STREAM, COMMON, "ECHO-STREAM", 11, 0x5254532D4F958495ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ECHO_STREAM_INPUT_STREAM, COMMON, "ECHO-STREAM-INPUT-STREAM", 24, 0x74E5E6C8D0E8B1F6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ECHO_STREAM_OUTPUT_STREAM, COMMON, "ECHO-STREAM-OUTPUT-STREAM", 25, 0x63EDFAD0CFC2D945ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ED, COMMON, "ED", 2, 0x0000000000004447ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EIGHTH, COMMON, "EIGHTH", 6, 0x000048544847494BULL, 0, 0, 0 },
{ CONSTANT_COMMON_ELT, COMMON, "ELT", 3, 0x0000000000544C48ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ENCODE_UNIVERSAL_TIME, COMMON, "ENCODE-UNIVERSAL-TIME", 21, 0x216E98DBE1E2EBD5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_END_OF_FILE, COMMON, "END-OF-FILE", 11, 0x462D464F2D899A99ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ENDP, COMMON, "ENDP", 4, 0x0000000050444E49ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ENOUGH_NAMESTRING, COMMON, "ENOUGH-NAMESTRING", 17, 0x1C769A9BA8949BDEULL, 0, 0, 0 },
{ CONSTANT_COMMON_ENSURE_DIRECTORIES_EXIST, COMMON, "ENSURE-DIRECTORIES-EXIST", 24, 0x61D2DDFEDDC5F3EBULL, 0, 0, 0 },
{ CONSTANT_COMMON_EQ, COMMON, "EQ", 2, 0x0000000000005147ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EQL, COMMON, "EQL", 3, 0x00000000004C5148ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EQUAL, COMMON, "EQUAL", 5, 0x0000004C4155514AULL, 0, 0, 0 },
{ CONSTANT_COMMON_EQUALP, COMMON, "EQUALP", 6, 0x0000504C4155514BULL, 0, 0, 0 },
{ CONSTANT_COMMON_ERROR, COMMON, "ERROR", 5, 0x000000524F52524AULL, 0, 0, 0 },
{ CONSTANT_COMMON_ETYPECASE, COMMON, "ETYPECASE", 9, 0x5341434550595493ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EVAL, COMMON, "EVAL", 4, 0x000000004C415649ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EVAL_WHEN, COMMON, "EVAL-WHEN", 9, 0x4548572D4C41569CULL, 0, 0, 0 },
{ CONSTANT_COMMON_EVENP, COMMON, "EVENP", 5, 0x000000504E45564AULL, 0, 0, 0 },
{ CONSTANT_COMMON_EVERY, COMMON, "EVERY", 5, 0x000000595245564AULL, 0, 0, 0 },
{ CONSTANT_COMMON_EXP, COMMON, "EXP", 3, 0x0000000000505848ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EXPORT, COMMON, "EXPORT", 6, 0x000054524F50584BULL, 0, 0, 0 },
{ CONSTANT_COMMON_EXPT, COMMON, "EXPT", 4, 0x0000000054505849ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EXTENDED_CHAR, COMMON, "EXTENDED-CHAR", 13, 0x444544A0869C9B7FULL, 0, 0, 0 },
{ CONSTANT_COMMON_FBOUNDP, COMMON, "FBOUNDP", 7, 0x0050444E554F424DULL, 0, 0, 0 },
{ CONSTANT_COMMON_FCEILING, COMMON, "FCEILING", 8, 0x474E494C4945434EULL, 0, 0, 0 },
{ CONSTANT_COMMON_FDEFINITION, COMMON, "FDEFINITION", 11, 0x54494E494693939AULL, 0, 0, 0 },
{ CONSTANT_COMMON_FFLOOR, COMMON, "FFLOOR", 6, 0x0000524F4F4C464CULL, 0, 0, 0 },
{ CONSTANT_COMMON_FMAKUNBOUND, COMMON, "FMAKUNBOUND", 11, 0x4F424E554B859BA6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIFTH, COMMON, "FIFTH", 5, 0x000000485446494BULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_AUTHOR, COMMON, "FILE-AUTHOR", 11, 0x5455412D459E9899ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_ERROR, COMMON, "FILE-ERROR", 10, 0x5252452D454C9B9FULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_ERROR_PATHNAME, COMMON, "FILE-ERROR-PATHNAME", 19, 0x209A996E95BEE8E9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_LENGTH, COMMON, "FILE-LENGTH", 11, 0x4E454C2D45949D98ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_POSITION, COMMON, "FILE-POSITION", 13, 0x534F507B94959D9CULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_NAMESTRING, COMMON, "FILE-NAMESTRING", 15, 0x4D889C7697A09C9AULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_STREAM, COMMON, "FILE-STREAM", 11, 0x5254532D45998A96ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_STRING_LENGTH, COMMON, "FILE-STRING-LENGTH", 18, 0x19A298797293DFF5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_WRITE_DATE, COMMON, "FILE-WRITE-DATE", 15, 0x4997AB6E89798EA9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILL, COMMON, "FILL", 4, 0x000000004C4C494AULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILL_POINTER, COMMON, "FILL-POINTER", 12, 0x494F502D9E919DA0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND, COMMON, "FIND", 4, 0x00000000444E494AULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_ALL_SYMBOLS, COMMON, "FIND-ALL-SYMBOLS", 16, 0x1F98906F91A79C83ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_CLASS, COMMON, "FIND-CLASS", 10, 0x414C432D444E9CA3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_IF, COMMON, "FIND-IF", 7, 0x0046492D444E494DULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_IF_NOT, COMMON, "FIND-IF-NOT", 11, 0x2D46492D44A2989FULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_METHOD, COMMON, "FIND-METHOD", 11, 0x54454D2D44929899ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_PACKAGE, COMMON, "FIND-PACKAGE", 12, 0x4341502D89958A9DULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_RESTART, COMMON, "FIND-RESTART", 12, 0x5345522D98A08AA6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_SYMBOL, COMMON, "FIND-SYMBOL", 11, 0x4D59532D449A9893ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FINISH_OUTPUT, COMMON, "FINISH-OUTPUT", 13, 0x4F2D48A79E9E9DA8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIRST, COMMON, "FIRST", 5, 0x000000545352494BULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIXNUM, COMMON, "FIXNUM", 6, 0x00004D554E58494CULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLET, COMMON, "FLET", 4, 0x0000000054454C4AULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOAT, COMMON, "FLOAT", 5, 0x00000054414F4C4BULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOATP, COMMON, "FLOATP", 6, 0x00005054414F4C4CULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOATING_POINT_INEXACT, COMMON, "FLOATING-POINT-INEXACT", 22, 0x107BF1E5CBF6E1D7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOATING_POINT_INVALID_OPERATION, COMMON, "FLOATING-POINT-INVALID-OPERATION", 32, 0x2DF82B4018323831ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOATING_POINT_OVERFLOW, COMMON, "FLOATING-POINT-OVERFLOW", 23, 0x16D2ECEED0F0E1E0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOATING_POINT_UNDERFLOW, COMMON, "FLOATING-POINT-UNDERFLOW", 24, 0x73CAE9E8DCE3E0D9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOAT_RADIX, COMMON, "FLOAT-RADIX", 11, 0x41522D5441A79595ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOAT_SIGN, COMMON, "FLOAT-SIGN", 10, 0x49532D54414F9A97ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOAT_DIGITS, COMMON, "FLOAT-DIGITS", 12, 0x49442D5494A39599ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOAT_PRECISION, COMMON, "FLOAT-PRECISION", 15, 0x529E7C9D94988F9AULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOOR, COMMON, "FLOOR", 5, 0x000000524F4F4C4BULL, 0, 0, 0 },
{ CONSTANT_COMMON_FORCE_OUTPUT, COMMON, "FORCE-OUTPUT", 12, 0x554F2D4597A79FA6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FORMAT, COMMON, "FORMAT", 6, 0x000054414D524F4CULL, 0, 0, 0 },
{ CONSTANT_COMMON_FORMATTER, COMMON, "FORMATTER", 9, 0x455454414D524FA1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FOURTH, COMMON, "FOURTH", 6, 0x0000485452554F4CULL, 0, 0, 0 },
{ CONSTANT_COMMON_FRESH_LINE, COMMON, "FRESH-LINE", 10, 0x494C2D485345979EULL, 0, 0, 0 },
{ CONSTANT_COMMON_FROUND, COMMON, "FROUND", 6, 0x0000444E554F524CULL, 0, 0, 0 },
{ CONSTANT_COMMON_FUNCALL, COMMON, "FUNCALL", 7, 0x004C4C41434E554DULL, 0, 0, 0 },
{ CONSTANT_COMMON_FUNCTION, COMMON, "FUNCTION", 8, 0x4E4F4954434E554EULL, 0, 0, 0 },
{ CONSTANT_COMMON_FUNCTION_LAMBDA_EXPRESSION, COMMON, "FUNCTION-LAMBDA-EXPRESSION", 26, 0x44E3E0DBE2E04821ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FUNCTIONP, COMMON, "FUNCTIONP", 9, 0x4E4F4954434E559FULL, 0, 0, 0 },
{ CONSTANT_COMMON_FTYPE, COMMON, "FTYPE", 5, 0x000000455059544BULL, 0, 0, 0 },
{ CONSTANT_COMMON_FTRUNCATE, COMMON, "FTRUNCATE", 9, 0x5441434E55525494ULL, 0, 0, 0 },
{ CONSTANT_COMMON_GCD, COMMON, "GCD", 3, 0x000000000044434AULL, 0, 0, 0 },
{ CONSTANT_COMMON_GENERIC_FUNCTION, COMMON, "GENERIC-FUNCTION", 16, 0x7B9292A6889C9A9DULL, 0, 0, 0 },
{ CONSTANT_COMMON_GENSYM, COMMON, "GENSYM", 6, 0x00004D59534E454DULL, 0, 0, 0 },
{ CONSTANT_COMMON_GENTEMP, COMMON, "GENTEMP", 7, 0x00504D45544E454EULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET, COMMON, "GET", 3, 0x000000000054454AULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_DECODED_TIME, COMMON, "GET-DECODED-TIME", 16, 0x14908E985A988A9BULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_DISPATCH_MACRO_CHARACTER, COMMON, "GET-DISPATCH-MACRO-CHARACTER", 28, 0x54E6D7BA0B0A3D39ULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_INTERNAL_REAL_TIME, COMMON, "GET-INTERNAL-REAL-TIME", 22, 0x0699E5C3C2E9C0FBULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_INTERNAL_RUN_TIME, COMMON, "GET-INTERNAL-RUN-TIME", 21, 0x13A9A0BBC6DEE7DBULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_MACRO_CHARACTER, COMMON, "GET-MACRO-CHARACTER", 19, 0x1584938E75E9B7FDULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_OUTPUT_STREAM_STRING, COMMON, "GET-OUTPUT-STREAM-STRING", 24, 0x58E7F0F5D4D4C701ULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_SETF_EXPANSION, COMMON, "GET-SETF-EXPANSION", 18, 0x0FA793947DACD8D5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_UNIVERSAL_TIME, COMMON, "GET-UNIVERSAL-TIME", 18, 0x1F9D7BA16EA7DCEBULL, 0, 0, 0 },
{ CONSTANT_COMMON_GETF, COMMON, "GETF", 4, 0x000000004654454BULL, 0, 0, 0 },
{ CONSTANT_COMMON_GETHASH, COMMON, "GETHASH", 7, 0x004853414854454EULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_PROPERTIES, COMMON, "GET-PROPERTIES", 14, 0x504FA59576A8979AULL, 0, 0, 0 },
{ CONSTANT_COMMON_GO, COMMON, "GO", 2, 0x0000000000004F49ULL, 0, 0, 0 },
{ CONSTANT_COMMON_GRAPHIC_CHAR_P, COMMON, "GRAPHIC-CHAR-P", 14, 0x2D439975A2829A98ULL, 0, 0, 0 },
{ CONSTANT_COMMON_HANDLER_BIND, COMMON, "HANDLER-BIND", 12, 0x2D52454C889C8A96ULL, 0, 0, 0 },
{ CONSTANT_COMMON_HANDLER_CASE, COMMON, "HANDLER-CASE", 12, 0x2D52454C89A18297ULL, 0, 0, 0 },
{ CONSTANT_COMMON_HASH_TABLE, COMMON, "HASH-TABLE", 10, 0x4241542D4853869EULL, 0, 0, 0 },
{ CONSTANT_COMMON_HASH_TABLE_COUNT, COMMON, "HASH-TABLE-COUNT", 16, 0x168FA97C8B8086A4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_HASH_TABLE_P, COMMON, "HASH-TABLE-P", 12, 0x4241542D988086A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_HASH_TABLE_REHASH_SIZE, COMMON, "HASH-TABLE-REHASH-SIZE", 22, 0x1582E1CCE3D3B3F2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_HASH_TABLE_REHASH_THRESHOLD, COMMON, "HASH-TABLE-REHASH-THRESHOLD", 27, 0x5DD5E1C4E3190046ULL, 0, 0, 0 },
{ CONSTANT_COMMON_HASH_TABLE_SIZE, COMMON, "HASH-TABLE-SIZE", 15, 0x4286AE769B8086A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_HASH_TABLE_TEST, COMMON, "HASH-TABLE-TEST", 15, 0x4295A7729C8086A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_HOST_NAMESTRING, COMMON, "HOST-NAMESTRING", 15, 0x4D889C76A6A7A29CULL, 0, 0, 0 },
{ CONSTANT_COMMON_IDENTITY, COMMON, "IDENTITY", 8, 0x595449544E454451ULL, 0, 0, 0 },
{ CONSTANT_COMMON_IF, COMMON, "IF", 2, 0x000000000000464BULL, 0, 0, 0 },
{ CONSTANT_COMMON_IGNORABLE, COMMON, "IGNORABLE", 9, 0x4C4241524F4E4797ULL, 0, 0, 0 },
{ CONSTANT_COMMON_IGNORE, COMMON, "IGNORE", 6, 0x000045524F4E474FULL, 0, 0, 0 },
{ CONSTANT_COMMON_IGNORE_ERRORS, COMMON, "IGNORE-ERRORS", 13, 0x452D45A5A19D99A8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_IMAGPART, COMMON, "IMAGPART", 8, 0x5452415047414D51ULL, 0, 0, 0 },
{ CONSTANT_COMMON_IMPORT, COMMON, "IMPORT", 6, 0x000054524F504D4FULL, 0, 0, 0 },
{ CONSTANT_COMMON_INCF, COMMON, "INCF", 4, 0x0000000046434E4DULL, 0, 0, 0 },
{ CONSTANT_COMMON_INLINE, COMMON, "INLINE", 6, 0x0000454E494C4E4FULL, 0, 0, 0 },
{ CONSTANT_COMMON_INPUT_STREAM_P, COMMON, "INPUT-STREAM-P", 14, 0x54537D81A29193A9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INSPECT, COMMON, "INSPECT", 7, 0x0054434550534E50ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTEGER, COMMON, "INTEGER", 7, 0x0052454745544E50ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTEGERP, COMMON, "INTEGERP", 8, 0x5052454745544E51ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTEGER_DECODE_FLOAT, COMMON, "INTEGER-DECODE-FLOAT", 20, 0x737F8A8BE8D8E2EDULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTEGER_LENGTH, COMMON, "INTEGER-LENGTH", 14, 0x2D528D9B8CA293A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTERACTIVE_STREAM_P, COMMON, "INTERACTIVE-STREAM-P", 20, 0x199595A5C2C6F1E7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTERN, COMMON, "INTERN", 6, 0x00004E5245544E4FULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTERNAL_TIME_UNITS_PER_SECOND, COMMON, "INTERNAL-TIME-UNITS-PER-SECOND", 30, 0x47E905360F343C30ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTERSECTION, COMMON, "INTERSECTION", 12, 0x4345535293A397A9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INVALID_METHOD_ERROR, COMMON, "INVALID-METHOD-ERROR", 20, 0x72718D9BDBF9E5FCULL, 0, 0, 0 },
{ CONSTANT_COMMON_INVOKE_DEBUGGER, COMMON, "INVOKE-DEBUGGER", 15, 0x447F8A9296AB909DULL, 0, 0, 0 },
{ CONSTANT_COMMON_INVOKE_RESTART, COMMON, "INVOKE-RESTART", 14, 0x522D999D90AAA19CULL, 0, 0, 0 },
{ CONSTANT_COMMON_INVOKE_RESTART_INTERACTIVELY, COMMON, "INVOKE-RESTART-INTERACTIVELY", 28, 0x64AEDCDF3C3C3B4EULL, 0, 0, 0 },
{ CONSTANT_COMMON_IN_PACKAGE, COMMON, "IN-PACKAGE", 10, 0x414B4341502D939AULL, 0, 0, 0 },
{ CONSTANT_COMMON_ISQRT, COMMON, "ISQRT", 5, 0x000000545251534EULL, 0, 0, 0 },
{ CONSTANT_COMMON_KEYWORD, COMMON, "KEYWORD", 7, 0x0044524F57594552ULL, 0, 0, 0 },
{ CONSTANT_COMMON_KEYWORDP, COMMON, "KEYWORDP", 8, 0x5044524F57594553ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LABELS, COMMON, "LABELS", 6, 0x0000534C45424152ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LAMBDA, COMMON, "LAMBDA", 6, 0x00004144424D4152ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LAMBDA_LIST_KEYWORDS, COMMON, "LAMBDA-LIST-KEYWORDS", 20, 0x2386868FC2E5E6F8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LAMBDA_PARAMETERS_LIMIT, COMMON, "LAMBDA-PARAMETERS-LIMIT", 23, 0x22C6DED6D8DAC0F7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LAST, COMMON, "LAST", 4, 0x0000000054534150ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LCM, COMMON, "LCM", 3, 0x00000000004D434FULL, 0, 0, 0 },
{ CONSTANT_COMMON_LDB, COMMON, "LDB", 3, 0x000000000042444FULL, 0, 0, 0 },
{ CONSTANT_COMMON_LDB_TEST, COMMON, "LDB-TEST", 8, 0x545345542D424454ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LDIFF, COMMON, "LDIFF", 5, 0x0000004646494451ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_DOUBLE_FLOAT, COMMON, "LEAST-NEGATIVE-DOUBLE-FLOAT", 27, 0x55C19FEFE92C1D4CULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_LONG_FLOAT, COMMON, "LEAST-NEGATIVE-LONG-FLOAT", 25, 0x52CABEF0C9DCD54FULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT, COMMON, "LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT", 38, 0x1E22528D7C734C79ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT, COMMON, "LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT", 36, 0x1F03034581705596ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT, COMMON, "LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT", 37, 0x062A0E9A67855291ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT, COMMON, "LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT", 38, 0x1E22578676824C79ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_SHORT_FLOAT, COMMON, "LEAST-NEGATIVE-SHORT-FLOAT", 26, 0x67C7B8D7F0E82A36ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_SINGLE_FLOAT, COMMON, "LEAST-NEGATIVE-SINGLE-FLOAT", 27, 0x64C19FEFE9311646ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_DOUBLE_FLOAT, COMMON, "LEAST-POSITIVE-DOUBLE-FLOAT", 27, 0x5FC39FEFE92C2558ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_LONG_FLOAT, COMMON, "LEAST-POSITIVE-LONG-FLOAT", 25, 0x5CCCBEF0C9DCDD5BULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT, COMMON, "LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT", 38, 0x2824528D7C735485ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_NORMALIZED_LONG_FLOAT, COMMON, "LEAST-POSITIVE-NORMALIZED-LONG-FLOAT", 36, 0x2905034581705DA2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT, COMMON, "LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT", 37, 0x102C0E9A67855A9DULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT, COMMON, "LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT", 38, 0x2824578676825485ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_SHORT_FLOAT, COMMON, "LEAST-POSITIVE-SHORT-FLOAT", 26, 0x71C9B8D7F0E83242ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_SINGLE_FLOAT, COMMON, "LEAST-POSITIVE-SINGLE-FLOAT", 27, 0x6EC39FEFE9311E52ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LENGTH, COMMON, "LENGTH", 6, 0x00004854474E4552ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LET, COMMON, "LET", 3, 0x000000000054454FULL, 0, 0, 0 },
{ CONSTANT_COMMON_LETA, COMMON, "LET*", 4, 0x000000002A544550ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LISP_IMPLEMENTATION_TYPE, COMMON, "LISP-IMPLEMENTATION-TYPE", 24, 0x69DEF6CFC2EEDDF9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LISP_IMPLEMENTATION_VERSION, COMMON, "LISP-IMPLEMENTATION-VERSION", 27, 0x77E0E2D1C33D2D45ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LIST, COMMON, "LIST", 4, 0x0000000054534950ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LISTEN, COMMON, "LISTEN", 6, 0x00004E4554534952ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LISTP, COMMON, "LISTP", 5, 0x0000005054534951ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LISTA, COMMON, "LIST*", 5, 0x0000002A54534951ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LIST_ALL_PACKAGES, COMMON, "LIST-ALL-PACKAGES", 17, 0x11938278979499DDULL, 0, 0, 0 },
{ CONSTANT_COMMON_LIST_LENGTH, COMMON, "LIST-LENGTH", 11, 0x4E454C2D549B9D9EULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOAD, COMMON, "LOAD", 4, 0x0000000044414F50ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOAD_LOGICAL_PATHNAME_TRANSLATIONS, COMMON, "LOAD-LOGICAL-PATHNAME-TRANSLATIONS", 34, 0x3D2E1DE12A17828EULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOAD_TIME_VALUE, COMMON, "LOAD-TIME-VALUE", 15, 0x4D8EA97985977CA0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOCALLY, COMMON, "LOCALLY", 7, 0x00594C4C41434F53ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOG, COMMON, "LOG", 3, 0x0000000000474F4FULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGAND, COMMON, "LOGAND", 6, 0x0000444E41474F52ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGANDC1, COMMON, "LOGANDC1", 8, 0x3143444E41474F54ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGANDC2, COMMON, "LOGANDC2", 8, 0x3243444E41474F54ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGBITP, COMMON, "LOGBITP", 7, 0x0050544942474F53ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGCOUNT, COMMON, "LOGCOUNT", 8, 0x544E554F43474F54ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGEQV, COMMON, "LOGEQV", 6, 0x0000565145474F52ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGICAL_PATHNAME, COMMON, "LOGICAL-PATHNAME", 16, 0x72998291919B90ACULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGICAL_PATHNAME_TRANSLATIONS, COMMON, "LOGICAL-PATHNAME-TRANSLATIONS", 29, 0x33E5D633213D2E3AULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGIOR, COMMON, "LOGIOR", 6, 0x0000524F49474F52ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGNAND, COMMON, "LOGNAND", 7, 0x00444E414E474F53ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGNOR, COMMON, "LOGNOR", 6, 0x0000524F4E474F52ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGNOT, COMMON, "LOGNOT", 6, 0x0000544F4E474F52ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGORC1, COMMON, "LOGORC1", 7, 0x003143524F474F53ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGORC2, COMMON, "LOGORC2", 7, 0x003243524F474F53ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGTEST, COMMON, "LOGTEST", 7, 0x0054534554474F53ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGXOR, COMMON, "LOGXOR", 6, 0x0000524F58474F52ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LONG_FLOAT, COMMON, "LONG-FLOAT", 10, 0x4F4C462D474EA397ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LONG_FLOAT_EPSILON, COMMON, "LONG-FLOAT-EPSILON", 18, 0x1B95997D8C7BF1EEULL, 0, 0, 0 },
{ CONSTANT_COMMON_LONG_FLOAT_NEGATIVE_EPSILON, COMMON, "LONG-FLOAT-NEGATIVE-EPSILON", 27, 0x6CE0DDB7C30F493DULL, 0, 0, 0 },
{ CONSTANT_COMMON_LONG_SITE_NAME, COMMON, "LONG-SITE-NAME", 14, 0x5449987A889C7C9FULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOOP, COMMON, "LOOP", 4, 0x00000000504F4F50ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOOP_FINISH, COMMON, "LOOP-FINISH", 11, 0x4E49462D5097A2A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOWER_CASE_P, COMMON, "LOWER-CASE-P", 12, 0x41432D52958494ABULL, 0, 0, 0 },
{ CONSTANT_COMMON_MACHINE_INSTANCE, COMMON, "MACHINE-INSTANCE", 16, 0x72889C8A9C968FA6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MACHINE_TYPE, COMMON, "MACHINE-TYPE", 12, 0x2D454E498D939AADULL, 0, 0, 0 },
{ CONSTANT_COMMON_MACHINE_VERSION, COMMON, "MACHINE-VERSION", 15, 0x2D939D929B9586B2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MACRO_FUNCTION, COMMON, "MACRO-FUNCTION", 14, 0x55467B9E9B9784A9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MACROEXPAND, COMMON, "MACROEXPAND", 11, 0x5058454F52878F99ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MACROEXPAND_1, COMMON, "MACROEXPAND-1", 13, 0x505845807F878F9BULL, 0, 0, 0 },
{ CONSTANT_COMMON_MACROLET, COMMON, "MACROLET", 8, 0x54454C4F52434155ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_ARRAY, COMMON, "MAKE-ARRAY", 10, 0x5252412D454B9A98ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_BROADCAST_STREAM, COMMON, "MAKE-BROADCAST-STREAM", 21, 0x227F96CDC7D3D7F7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_CONDITION, COMMON, "MAKE-CONDITION", 14, 0x4E4F917C8E9F8A9FULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_CONCATENATED_STREAM, COMMON, "MAKE-CONCATENATED-STREAM", 24, 0x60E4C9CDDEF2AFECULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_DISPATCH_MACRO_CHARACTER, COMMON, "MAKE-DISPATCH-MACRO-CHARACTER", 29, 0x66D7BA0AFB43183EULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_ECHO_STREAM, COMMON, "MAKE-ECHO-STREAM", 16, 0x15848A7F999E6EACULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_HASH_TABLE, COMMON, "MAKE-HASH-TABLE", 15, 0x5386946F869F6EA4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_INSTANCES_OBSOLETE, COMMON, "MAKE-INSTANCES-OBSOLETE", 23, 0x22C0F0B7D4E8D5FAULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_LIST, COMMON, "MAKE-LIST", 9, 0x53494C2D454B41AAULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_LOAD_FORM, COMMON, "MAKE-LOAD-FORM", 14, 0x414F997F94916E9FULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_LOAD_FORM_SAVING_SLOTS, COMMON, "MAKE-LOAD-FORM-SAVING-SLOTS", 27, 0x60CFC6C6E32E193CULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_METHOD, COMMON, "MAKE-METHOD", 11, 0x54454D2D458F90A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_PACKAGE, COMMON, "MAKE-PACKAGE", 12, 0x4341502D8A9282A4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_PATHNAME, COMMON, "MAKE-PATHNAME", 13, 0x54415072928C8FA2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_RANDOM_STATE, COMMON, "MAKE-RANDOM-STATE", 17, 0x2282A680729890E7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_SEQUENCE, COMMON, "MAKE-SEQUENCE", 13, 0x51455372889986AFULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_STRING, COMMON, "MAKE-STRING", 11, 0x5254532D45928FA1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_STRING_INPUT_STREAM, COMMON, "MAKE-STRING-INPUT-STREAM", 24, 0x74E5E6C8C6E5BD02ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_STRING_OUTPUT_STREAM, COMMON, "MAKE-STRING-OUTPUT-STREAM", 25, 0x63EDFAD0C5BFE451ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_SYMBOL, COMMON, "MAKE-SYMBOL", 11, 0x4D59532D4597909AULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_SYNONYM_STREAM, COMMON, "MAKE-SYNONYM-STREAM", 19, 0x20ADA65A92F1D0F4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_TWO_WAY_STREAM, COMMON, "MAKE-TWO-WAY-STREAM", 19, 0x21ABA75A9ED9D9D2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKUNBOUND, COMMON, "MAKUNBOUND", 10, 0x554F424E554B85A5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAP, COMMON, "MAP", 3, 0x0000000000504150ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAPC, COMMON, "MAPC", 4, 0x0000000043504151ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAPCAR, COMMON, "MAPCAR", 6, 0x0000524143504153ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAPCAN, COMMON, "MAPCAN", 6, 0x00004E4143504153ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAPHASH, COMMON, "MAPHASH", 7, 0x0048534148504154ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAPL, COMMON, "MAPL", 4, 0x000000004C504151ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAPLIST, COMMON, "MAPLIST", 7, 0x005453494C504154ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAPCON, COMMON, "MAPCON", 6, 0x00004E4F43504153ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAP_INTO, COMMON, "MAP-INTO", 8, 0x4F544E492D504155ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MASK_FIELD, COMMON, "MASK-FIELD", 10, 0x4549462D4B5385A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAX, COMMON, "MAX", 3, 0x0000000000584150ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MERGE, COMMON, "MERGE", 5, 0x0000004547524552ULL, 0, 0, 0 },
{ CONSTANT_COMMON_METHOD, COMMON, "METHOD", 6, 0x0000444F48544553ULL, 0, 0, 0 },
{ CONSTANT_COMMON_METHOD_COMBINATION_ERROR, COMMON, "METHOD-COMBINATION-ERROR", 24, 0x5ED0D7EFD6C3E103ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MEMBER, COMMON, "MEMBER", 6, 0x00005245424D4553ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MEMBER_IF, COMMON, "MEMBER-IF", 9, 0x492D5245424D459CULL, 0, 0, 0 },
{ CONSTANT_COMMON_MEMBER_IF_NOT, COMMON, "MEMBER-IF-NOT", 13, 0x492D5299919B72A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MERGE_PATHNAMES, COMMON, "MERGE-PATHNAMES", 15, 0x41A3729288A08DB0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_METHOD_COMBINATION, COMMON, "METHOD-COMBINATION", 18, 0x0C81859D9196E0FDULL, 0, 0, 0 },
{ CONSTANT_COMMON_MIN, COMMON, "MIN", 3, 0x00000000004E4950ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MINUSP, COMMON, "MINUSP", 6, 0x00005053554E4953ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MISMATCH, COMMON, "MISMATCH", 8, 0x484354414D534955ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOD, COMMON, "MOD", 3, 0x0000000000444F50ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_NEGATIVE_DOUBLE_FLOAT, COMMON, "MOST-NEGATIVE-DOUBLE-FLOAT", 26, 0x65D5C19FEFE93A3EULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_NEGATIVE_FIXNUM, COMMON, "MOST-NEGATIVE-FIXNUM", 20, 0x108B7B72F7F1F1FAULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_NEGATIVE_LONG_FLOAT, COMMON, "MOST-NEGATIVE-LONG-FLOAT", 24, 0x6AD2CABEF0C9EAF4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_NEGATIVE_SHORT_FLOAT, COMMON, "MOST-NEGATIVE-SHORT-FLOAT", 25, 0x50E7C7B8D7F0F64AULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_NEGATIVE_SINGLE_FLOAT, COMMON, "MOST-NEGATIVE-SINGLE-FLOAT", 26, 0x5FE4C19FEFE93F37ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_POSITIVE_DOUBLE_FLOAT, COMMON, "MOST-POSITIVE-DOUBLE-FLOAT", 26, 0x71DFC39FEFE93A46ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_POSITIVE_FIXNUM, COMMON, "MOST-POSITIVE-FIXNUM", 20, 0x1C957D72F7F1F202ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_POSITIVE_LONG_FLOAT, COMMON, "MOST-POSITIVE-LONG-FLOAT", 24, 0x76DCCCBEF0C9EAFCULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_POSITIVE_SHORT_FLOAT, COMMON, "MOST-POSITIVE-SHORT-FLOAT", 25, 0x5CF1C9B8D7F0F652ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_POSITIVE_SINGLE_FLOAT, COMMON, "MOST-POSITIVE-SINGLE-FLOAT", 26, 0x6BEEC39FEFE93F3FULL, 0, 0, 0 },
{ CONSTANT_COMMON_MUFFLE_WARNING, COMMON, "MUFFLE-WARNING", 14, 0x572D8C9A8F94A79CULL, 0, 0, 0 },
{ CONSTANT_COMMON_MULTIPLE_VALUE_BIND, COMMON, "MULTIPLE-VALUE-BIND", 19, 0x0779959EA0D1F9D6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MULTIPLE_VALUE_CALL, COMMON, "MULTIPLE-VALUE-CALL", 19, 0x0879959EA0D9F7CEULL, 0, 0, 0 },
{ CONSTANT_COMMON_MULTIPLE_VALUE_LIST, COMMON, "MULTIPLE-VALUE-LIST", 19, 0x1179959EA0E1FED6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MULTIPLE_VALUE_PROG1, COMMON, "MULTIPLE-VALUE-PROG1", 20, 0x1579959ED1D4FAE0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MULTIPLE_VALUE_SETQ, COMMON, "MULTIPLE-VALUE-SETQ", 19, 0x1879959EA0DEFFD2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MULTIPLE_VALUES_LIMIT, COMMON, "MULTIPLE-VALUES-LIMIT", 21, 0x729F95F2E9DAF4DBULL, 0, 0, 0 },
{ CONSTANT_COMMON_NAME_CHAR, COMMON, "NAME-CHAR", 9, 0x4148432D454D41A9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NAMESTRING, COMMON, "NAMESTRING", 10, 0x49525453454D88A6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NBUTLAST, COMMON, "NBUTLAST", 8, 0x5453414C54554256ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NCONC, COMMON, "NCONC", 5, 0x000000434E4F4353ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NEXT_METHOD_P, COMMON, "NEXT-METHOD-P", 13, 0x54454D7D819C94A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NINTH, COMMON, "NINTH", 5, 0x00000048544E4953ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NINTERSECTION, COMMON, "NINTERSECTION", 13, 0x45535293A3979D9EULL, 0, 0, 0 },
{ CONSTANT_COMMON_NO_APPLICABLE_METHOD, COMMON, "NO-APPLICABLE-METHOD", 20, 0x0E997D95D1BED8F9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NO_NEXT_METHOD, COMMON, "NO-NEXT-METHOD", 14, 0x2D549C94968194A9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NOT, COMMON, "NOT", 3, 0x0000000000544F51ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NOTANY, COMMON, "NOTANY", 6, 0x0000594E41544F54ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NOTEVERY, COMMON, "NOTEVERY", 8, 0x5952455645544F56ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NOTINLINE, COMMON, "NOTINLINE", 9, 0x4E494C4E49544F9CULL, 0, 0, 0 },
{ CONSTANT_COMMON_NRECONC, COMMON, "NRECONC", 7, 0x00434E4F43455255ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NREVERSE, COMMON, "NREVERSE", 8, 0x4553524556455256ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSET_DIFFERENCE, COMMON, "NSET-DIFFERENCE", 15, 0x468E877B999798A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSET_EXCLUSIVE_OR, COMMON, "NSET-EXCLUSIVE-OR", 17, 0x12858A839D98A8FDULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSTRING_CAPITALIZE, COMMON, "NSTRING-CAPITALIZE", 18, 0x76938F9D9BA4D9FDULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSTRING_DOWNCASE, COMMON, "NSTRING-DOWNCASE", 16, 0x729A8F8CA0ABA2A2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSTRING_UPCASE, COMMON, "NSTRING-UPCASE", 14, 0x2D47939C9397A3B1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSUBLIS, COMMON, "NSUBLIS", 7, 0x0053494C42555355ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSUBST, COMMON, "NSUBST", 6, 0x0000545342555354ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSUBSTITUTE, COMMON, "NSUBSTITUTE", 11, 0x54495453429AA7AEULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSUBSTITUTE_IF, COMMON, "NSUBSTITUTE-IF", 14, 0x54499A9C6F9AA7B1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSUBSTITUTE_IF_NOT, COMMON, "NSUBSTITUTE-IF-NOT", 18, 0x22769A9C6F9AFC04ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSUBST_IF, COMMON, "NSUBST-IF", 9, 0x492D54534255539DULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSUBST_IF_NOT, COMMON, "NSUBST-IF-NOT", 13, 0x492D54A791A380A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NTH, COMMON, "NTH", 3, 0x0000000000485451ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NTH_VALUE, COMMON, "NTH-VALUE", 9, 0x554C41562D48549CULL, 0, 0, 0 },
{ CONSTANT_COMMON_NTHCDR, COMMON, "NTHCDR", 6, 0x0000524443485454ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NULL, COMMON, "NULL", 4, 0x000000004C4C5552ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBER, COMMON, "NUMBER", 6, 0x00005245424D5554ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBERP, COMMON, "NUMBERP", 7, 0x00505245424D5555ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMERATOR, COMMON, "NUMERATOR", 9, 0x4F544152454D55A9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUNION, COMMON, "NUNION", 6, 0x00004E4F494E5554ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ODDP, COMMON, "ODDP", 4, 0x0000000050444453ULL, 0, 0, 0 },
{ CONSTANT_COMMON_OTHERWISE, COMMON, "OTHERWISE", 9, 0x534957524548549DULL, 0, 0, 0 },
{ CONSTANT_COMMON_OPEN, COMMON, "OPEN", 4, 0x000000004E455053ULL, 0, 0, 0 },
{ CONSTANT_COMMON_OPEN_STREAM_P, COMMON, "OPEN-STREAM-P", 13, 0x5254537D7B9291A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_OPTIMIZE, COMMON, "OPTIMIZE", 8, 0x455A494D49545057ULL, 0, 0, 0 },
{ CONSTANT_COMMON_OR, COMMON, "OR", 2, 0x0000000000005251ULL, 0, 0, 0 },
{ CONSTANT_COMMON_OUTPUT_STREAM_P, COMMON, "OUTPUT-STREAM-P", 15, 0x537D81A29199A7B2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE, COMMON, "PACKAGE", 7, 0x004547414B434157ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGEP, COMMON, "PACKAGEP", 8, 0x504547414B434158ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE_ERROR, COMMON, "PACKAGE-ERROR", 13, 0x2D4547939A9593A2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE_ERROR_PACKAGE, COMMON, "PACKAGE-ERROR-PACKAGE", 21, 0x6E9574D8E1D6DEEDULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE_NAME, COMMON, "PACKAGE-NAME", 12, 0x2D454741909082AAULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE_NICKNAMES, COMMON, "PACKAGE-NICKNAMES", 17, 0x7292888F96868B02ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE_SHADOWING_SYMBOLS, COMMON, "PACKAGE-SHADOWING-SYMBOLS", 25, 0x47DDE0DDE8D7B756ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE_USE_LIST, COMMON, "PACKAGE-USE-LIST", 16, 0x0198908D788894B5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE_USED_BY_LIST, COMMON, "PACKAGE-USED-BY-LIST", 20, 0x5A9E896EE3DBDE05ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PAIRLIS, COMMON, "PAIRLIS", 7, 0x0053494C52494157ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PARSE_ERROR, COMMON, "PARSE-ERROR", 11, 0x52452D4553A490ADULL, 0, 0, 0 },
{ CONSTANT_COMMON_PARSE_INTEGER, COMMON, "PARSE-INTEGER", 13, 0x4E492D97989986B1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PARSE_NAMESTRING, COMMON, "PARSE-NAMESTRING", 16, 0x089C7697A7A586ADULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME, COMMON, "PATHNAME", 8, 0x454D414E48544158ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME_HOST, COMMON, "PATHNAME-HOST", 13, 0x454D41A29BA3898AULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME_DEVICE, COMMON, "PATHNAME-DEVICE", 15, 0x459284979E99858CULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME_DIRECTORY, COMMON, "PATHNAME-DIRECTORY", 18, 0x14A184939A9DDEE1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME_NAME, COMMON, "PATHNAME-NAME", 13, 0x454D419395958F8AULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME_MATCH_P, COMMON, "PATHNAME-MATCH-P", 16, 0x157A89919C958E8DULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME_TYPE, COMMON, "PATHNAME-TYPE", 13, 0x454D419398AD958AULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME_VERSION, COMMON, "PATHNAME-VERSION", 16, 0x139C8AA19A99978DULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAMEP, COMMON, "PATHNAMEP", 9, 0x454D414E485441A9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PEEK_CHAR, COMMON, "PEEK-CHAR", 9, 0x4148432D4B4545ABULL, 0, 0, 0 },
{ CONSTANT_COMMON_PHASE, COMMON, "PHASE", 5, 0x0000004553414855ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PI, COMMON, "PI", 2, 0x0000000000004952ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PLUSP, COMMON, "PLUSP", 5, 0x0000005053554C55ULL, 0, 0, 0 },
{ CONSTANT_COMMON_POP, COMMON, "POP", 3, 0x0000000000504F53ULL, 0, 0, 0 },
{ CONSTANT_COMMON_POSITION, COMMON, "POSITION", 8, 0x4E4F495449534F58ULL, 0, 0, 0 },
{ CONSTANT_COMMON_POSITION_IF, COMMON, "POSITION-IF", 11, 0x4E4F495449999888ULL, 0, 0, 0 },
{ CONSTANT_COMMON_POSITION_IF_NOT, COMMON, "POSITION-IF-NOT", 15, 0x4EA398A27699988CULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT, COMMON, "PPRINT", 6, 0x0000544E49525056ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_DISPATCH, COMMON, "PPRINT-DISPATCH", 15, 0x447597A28AA2A3A8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_EXIT_IF_LIST_EXHAUSTED, COMMON, "PPRINT-EXIT-IF-LIST-EXHAUSTED", 29, 0x52A2F320E94F4063ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_FILL, COMMON, "PPRINT-FILL", 11, 0x462D544E499E9CA4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_INDENT, COMMON, "PPRINT-INDENT", 13, 0x492D54A2979794ABULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_LINEAR, COMMON, "PPRINT-LINEAR", 13, 0x4C2D54A08A979EA6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_LOGICAL_BLOCK, COMMON, "PPRINT-LOGICAL-BLOCK", 20, 0x0E5AA08FD7DEE6FFULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_NEWLINE, COMMON, "PPRINT-NEWLINE", 14, 0x4E2D999C929EA7A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_POP, COMMON, "PPRINT-POP", 10, 0x502D544E4952A0A9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_TAB, COMMON, "PPRINT-TAB", 10, 0x542D544E4952929BULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_TABULAR, COMMON, "PPRINT-TABULAR", 14, 0x542DA68F95A7929FULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRIN1, COMMON, "PRIN1", 5, 0x000000314E495255ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRIN1_TO_STRING, COMMON, "PRIN1-TO-STRING", 15, 0x4F9B7B7AA09DA58CULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRINC, COMMON, "PRINC", 5, 0x000000434E495255ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRINC_TO_STRING, COMMON, "PRINC-TO-STRING", 15, 0x4F9B7B8CA09DA58CULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRINT, COMMON, "PRINT", 5, 0x000000544E495255ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRINT_NOT_READABLE, COMMON, "PRINT-NOT-READABLE", 18, 0x118F7195939BC502ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRINT_NOT_READABLE_OBJECT, COMMON, "PRINT-NOT-READABLE-OBJECT", 25, 0x54D4BBD7E2C8C55DULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRINT_OBJECT, COMMON, "PRINT-OBJECT", 12, 0x424F2D54A28C97A6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRINT_UNREADABLE_OBJECT, COMMON, "PRINT-UNREADABLE-OBJECT", 23, 0x13F5B2DADCCCE6E6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROBE_FILE, COMMON, "PROBE-FILE", 10, 0x49462D45424F97A6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROCLAIM, COMMON, "PROCLAIM", 8, 0x4D49414C434F5258ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROG, COMMON, "PROG", 4, 0x00000000474F5254ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROG1, COMMON, "PROG1", 5, 0x00000031474F5255ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROG2, COMMON, "PROG2", 5, 0x00000032474F5255ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROGA, COMMON, "PROG*", 5, 0x0000002A474F5255ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROGN, COMMON, "PROGN", 5, 0x0000004E474F5255ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROGV, COMMON, "PROGV", 5, 0x00000056474F5255ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROGRAM_ERROR, COMMON, "PROGRAM-ERROR", 13, 0x2D4D41A496A1A4A2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROVIDE, COMMON, "PROVIDE", 7, 0x00454449564F5257ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PSETF, COMMON, "PSETF", 5, 0x0000004654455355ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PSETQ, COMMON, "PSETQ", 5, 0x0000005154455355ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PUSH, COMMON, "PUSH", 4, 0x0000000048535554ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PUSHNEW, COMMON, "PUSHNEW", 7, 0x0057454E48535557ULL, 0, 0, 0 },
{ CONSTANT_COMMON_QUOTE, COMMON, "QUOTE", 5, 0x00000045544F5556ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RANDOM, COMMON, "RANDOM", 6, 0x00004D4F444E4158ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RANDOM_STATE, COMMON, "RANDOM-STATE", 12, 0x532D4D4F89A282B2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RANDOM_STATE_P, COMMON, "RANDOM-STATE-P", 14, 0x532D9D7C89A282B4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RASSOC, COMMON, "RASSOC", 6, 0x0000434F53534158ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RASSOC_IF, COMMON, "RASSOC-IF", 9, 0x492D434F535341A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RASSOC_IF_NOT, COMMON, "RASSOC-IF-NOT", 13, 0x492D43A3A2A16EA5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RATIO, COMMON, "RATIO", 5, 0x0000004F49544157ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RATIONAL, COMMON, "RATIONAL", 8, 0x4C414E4F4954415AULL, 0, 0, 0 },
{ CONSTANT_COMMON_RATIONALIZE, COMMON, "RATIONALIZE", 11, 0x4C414E4F49999BA6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RATIONALP, COMMON, "RATIONALP", 9, 0x4C414E4F495441ABULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ, COMMON, "READ", 4, 0x0000000044414556ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_BYTE, COMMON, "READ-BYTE", 9, 0x5459422D444145A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_CHAR, COMMON, "READ-CHAR", 9, 0x4148432D444145ADULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_CHAR_NO_HANG, COMMON, "READ-CHAR-NO-HANG", 17, 0x0F898B5A938F72FCULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_DELIMITED_LIST, COMMON, "READ-DELIMITED-LIST", 19, 0x1872887298DEE5F7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_FROM_STRING, COMMON, "READ-FROM-STRING", 16, 0x16A08F7F989472AFULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_LINE, COMMON, "READ-LINE", 9, 0x4E494C2D444145A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_PRESERVING_WHITESPACE, COMMON, "READ-PRESERVING-WHITESPACE", 26, 0x33E9F1BBEEDD1859ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_SEQUENCE, COMMON, "READ-SEQUENCE", 13, 0x51455372878F8AB4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READER_ERROR, COMMON, "READER-ERROR", 12, 0x452D5245969097B0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READTABLE, COMMON, "READTABLE", 9, 0x4C424154444145A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READTABLE_CASE, COMMON, "READTABLE-CASE", 14, 0x4C4286A7858472A5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READTABLEP, COMMON, "READTABLEP", 10, 0x4C424154444195A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REAL, COMMON, "REAL", 4, 0x000000004C414556ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REALP, COMMON, "REALP", 5, 0x000000504C414557ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REALPART, COMMON, "REALPART", 8, 0x545241504C41455AULL, 0, 0, 0 },
{ CONSTANT_COMMON_REDUCE, COMMON, "REDUCE", 6, 0x0000454355444558ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REM, COMMON, "REM", 3, 0x00000000004D4555ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMF, COMMON, "REMF", 4, 0x00000000464D4556ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMHASH, COMMON, "REMHASH", 7, 0x00485341484D4559ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMOVE, COMMON, "REMOVE", 6, 0x000045564F4D4558ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMOVE_DUPLICATES, COMMON, "REMOVE-DUPLICATES", 17, 0x098186999899960BULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMOVE_IF, COMMON, "REMOVE-IF", 9, 0x492D45564F4D45A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMOVE_IF_NOT, COMMON, "REMOVE-IF-NOT", 13, 0x492D45AA9E9B72A5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMOVE_METHOD, COMMON, "REMOVE-METHOD", 13, 0x4D2D459A9E9599A4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMPROP, COMMON, "REMPROP", 7, 0x00504F52504D4559ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RENAME_FILE, COMMON, "RENAME-FILE", 11, 0x462D454D419391A6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RENAME_PACKAGE, COMMON, "RENAME-PACKAGE", 14, 0x502D8A94829988A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REPLACE, COMMON, "REPLACE", 7, 0x004543414C504559ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REQUIRE, COMMON, "REQUIRE", 7, 0x0045524955514559ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REST, COMMON, "REST", 4, 0x0000000054534556ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RESTART, COMMON, "RESTART", 7, 0x0054524154534559ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RESTART_BIND, COMMON, "RESTART-BIND", 12, 0x2D54524198A18EA0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RESTART_CASE, COMMON, "RESTART-CASE", 12, 0x2D54524199A686A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RESTART_NAME, COMMON, "RESTART-NAME", 12, 0x2D54524199A086ACULL, 0, 0, 0 },
{ CONSTANT_COMMON_RETURN, COMMON, "RETURN", 6, 0x00004E5255544558ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RETURN_FROM, COMMON, "RETURN-FROM", 11, 0x462D4E5255A194AFULL, 0, 0, 0 },
{ CONSTANT_COMMON_REVAPPEND, COMMON, "REVAPPEND", 9, 0x4E4550504156459FULL, 0, 0, 0 },
{ CONSTANT_COMMON_REVERSE, COMMON, "REVERSE", 7, 0x0045535245564559ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ROOM, COMMON, "ROOM", 4, 0x000000004D4F4F56ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ROTATEF, COMMON, "ROTATEF", 7, 0x0046455441544F59ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ROUND, COMMON, "ROUND", 5, 0x000000444E554F57ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ROW_MAJOR_AREF, COMMON, "ROW-MAJOR-AREF", 14, 0x4F4A87927F987CB2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RPLACA, COMMON, "RPLACA", 6, 0x00004143414C5058ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RPLACD, COMMON, "RPLACD", 6, 0x00004443414C5058ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SAFETY, COMMON, "SAFETY", 6, 0x0000595445464159ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SATISFIES, COMMON, "SATISFIES", 9, 0x45494653495441AFULL, 0, 0, 0 },
{ CONSTANT_COMMON_SBIT, COMMON, "SBIT", 4, 0x0000000054494257ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SCALE_FLOAT, COMMON, "SCALE-FLOAT", 11, 0x4C462D454C9584ADULL, 0, 0, 0 },
{ CONSTANT_COMMON_SCHAR, COMMON, "SCHAR", 5, 0x0000005241484358ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SEARCH, COMMON, "SEARCH", 6, 0x0000484352414559ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SECOND, COMMON, "SECOND", 6, 0x0000444E4F434559ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SEQUENCE, COMMON, "SEQUENCE", 8, 0x45434E455551455BULL, 0, 0, 0 },
{ CONSTANT_COMMON_SERIOUS_CONDITION, COMMON, "SERIOUS-CONDITION", 17, 0x7C9CA9988DA094F5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SET, COMMON, "SET", 3, 0x0000000000544556ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SET_DISPATCH_MACRO_CHARACTER, COMMON, "SET-DISPATCH-MACRO-CHARACTER", 28, 0x54E6D7BA0B0A3D45ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SET_MACRO_CHARACTER, COMMON, "SET-MACRO-CHARACTER", 19, 0x1584938E75E9B809ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SET_PPRINT_DISPATCH, COMMON, "SET-PPRINT-DISPATCH", 19, 0x0AA2A39971C9DD08ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SET_SYNTAX_FROM_CHAR, COMMON, "SET-SYNTAX-FROM-CHAR", 20, 0x019BA8A5C5C2E5EBULL, 0, 0, 0 },
{ CONSTANT_COMMON_SETF, COMMON, "SETF", 4, 0x0000000046544557ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SETQ, COMMON, "SETQ", 4, 0x0000000051544557ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SET_DIFFERENCE, COMMON, "SET-DIFFERENCE", 14, 0x46468E877B9997A6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SET_EXCLUSIVE_OR, COMMON, "SET-EXCLUSIVE-OR", 16, 0x1E92858A839D98B8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SEVENTH, COMMON, "SEVENTH", 7, 0x0048544E4556455AULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHADOW, COMMON, "SHADOW", 6, 0x0000574F44414859ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHADOWING_IMPORT, COMMON, "SHADOWING-IMPORT", 16, 0x229BA69F918A75AAULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHIFTF, COMMON, "SHIFTF", 6, 0x0000465446494859ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHORT_FLOAT, COMMON, "SHORT-FLOAT", 11, 0x4C462D5452A389ADULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHORT_FLOAT_EPSILON, COMMON, "SHORT-FLOAT-EPSILON", 19, 0x15997D997FF1D901ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHORT_FLOAT_NEGATIVE_EPSILON, COMMON, "SHORT-FLOAT-NEGATIVE-EPSILON", 28, 0x60DDB7D013491F5BULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHORT_SITE_NAME, COMMON, "SHORT-SITE-NAME", 15, 0x49987A95A07C8DB6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIGNAL, COMMON, "SIGNAL", 6, 0x00004C414E474959ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIGNED_BYTE, COMMON, "SIGNED-BYTE", 11, 0x422D44454E8C9DB7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIGNUM, COMMON, "SIGNUM", 6, 0x00004D554E474959ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_ARRAY, COMMON, "SIMPLE-ARRAY", 12, 0x412D454CA98E9BB1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_BASE_STRING, COMMON, "SIMPLE-BASE-STRING", 18, 0x0B7F999F7D92E3F4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_BIT_VECTOR, COMMON, "SIMPLE-BIT-VECTOR", 17, 0x11818891A67A9DFFULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_BIT_VECTOR_P, COMMON, "SIMPLE-BIT-VECTOR-P", 19, 0x11818891A6CACB01ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_CONDITION, COMMON, "SIMPLE-CONDITION", 16, 0x117C8EA0999197B2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_CONDITION_FORMAT_CONTROL, COMMON, "SIMPLE-CONDITION-FORMAT-CONTROL", 31, 0x3F1D1F40402F2D31ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_CONDITION_FORMAT_ARGUMENTS, COMMON, "SIMPLE-CONDITION-FORMAT-ARGUMENTS", 33, 0x131F153B41283084ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_ERROR, COMMON, "SIMPLE-ERROR", 12, 0x452D454CA29C9BB1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_STRING, COMMON, "SIMPLE-STRING", 13, 0x532D45939E969BB4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_STRING_P, COMMON, "SIMPLE-STRING-P", 15, 0x537D72939E969BB6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_TYPE_ERROR, COMMON, "SIMPLE-TYPE-ERROR", 17, 0x237F97917D929A0FULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_VECTOR, COMMON, "SIMPLE-VECTOR", 13, 0x562D459E9FA18CA5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_VECTOR_P, COMMON, "SIMPLE-VECTOR-P", 15, 0x567D729E9FA18CA7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_WARNING, COMMON, "SIMPLE-WARNING", 14, 0x572D8C9A999B9BA2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIN, COMMON, "SIN", 3, 0x00000000004E4956ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SINH, COMMON, "SINH", 4, 0x00000000484E4957ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SINGLE_FLOAT, COMMON, "SINGLE-FLOAT", 12, 0x462D454C9B8F98ABULL, 0, 0, 0 },
{ CONSTANT_COMMON_SINGLE_FLOAT_EPSILON, COMMON, "SINGLE-FLOAT-EPSILON", 20, 0x197D8A79E9DEE4FCULL, 0, 0, 0 },
{ CONSTANT_COMMON_SINGLE_FLOAT_NEGATIVE_EPSILON, COMMON, "SINGLE-FLOAT-NEGATIVE-EPSILON", 29, 0x5DB7C10D41253650ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIXTH, COMMON, "SIXTH", 5, 0x0000004854584958ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLEEP, COMMON, "SLEEP", 5, 0x0000005045454C58ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLOT_BOUNDP, COMMON, "SLOT-BOUNDP", 11, 0x554F422D549F90ACULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLOT_EXISTS_P, COMMON, "SLOT-EXISTS-P", 13, 0x4958457D81A2A0B3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLOT_MAKUNBOUND, COMMON, "SLOT-MAKUNBOUND", 15, 0x4B859B82A3919AB7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLOT_VALUE, COMMON, "SLOT-VALUE", 10, 0x4C41562D544F91B2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SOFTWARE_TYPE, COMMON, "SOFTWARE-TYPE", 13, 0x4552419CA49FA38DULL, 0, 0, 0 },
{ CONSTANT_COMMON_SOFTWARE_VERSION, COMMON, "SOFTWARE-VERSION", 16, 0x13A18AAAA68BA590ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SOME, COMMON, "SOME", 4, 0x00000000454D4F57ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SORT, COMMON, "SORT", 4, 0x0000000054524F57ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SPACE, COMMON, "SPACE", 5, 0x0000004543415058ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SPECIAL, COMMON, "SPECIAL", 7, 0x004C41494345505AULL, 0, 0, 0 },
{ CONSTANT_COMMON_SPECIAL_OPERATOR_P, COMMON, "SPECIAL-OPERATOR-P", 18, 0x7F9B958A958AF0E1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SPEED, COMMON, "SPEED", 5, 0x0000004445455058ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SQRT, COMMON, "SQRT", 4, 0x0000000054525157ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STABLE_SORT, COMMON, "STABLE-SORT", 11, 0x532D454C4295A6ADULL, 0, 0, 0 },
{ CONSTANT_COMMON_STANDARD, COMMON, "STANDARD", 8, 0x445241444E41545BULL, 0, 0, 0 },
{ CONSTANT_COMMON_STANDARD_CHAR, COMMON, "STANDARD-CHAR", 13, 0x445241968F89978DULL, 0, 0, 0 },
{ CONSTANT_COMMON_STANDARD_CHAR_P, COMMON, "STANDARD-CHAR-P", 15, 0x44A26E968F89978FULL, 0, 0, 0 },
{ CONSTANT_COMMON_STANDARD_CLASS, COMMON, "STANDARD-CLASS", 14, 0x445294978F8D978EULL, 0, 0, 0 },
{ CONSTANT_COMMON_STANDARD_GENERIC_FUNCTION, COMMON, "STANDARD-GENERIC-FUNCTION", 25, 0x56E4E7CCEADBE214ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STANDARD_METHOD, COMMON, "STANDARD-METHOD", 15, 0x4496908CA286A18FULL, 0, 0, 0 },
{ CONSTANT_COMMON_STANDARD_OBJECT, COMMON, "STANDARD-OBJECT", 15, 0x44A684899883A38FULL, 0, 0, 0 },
{ CONSTANT_COMMON_STEP, COMMON, "STEP", 4, 0x0000000050455457ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STORAGE_CONDITION, COMMON, "STORAGE-CONDITION", 17, 0x7C8E9B8A969DA3F5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STORE_VALUE, COMMON, "STORE-VALUE", 11, 0x41562D455294A9AAULL, 0, 0, 0 },
{ CONSTANT_COMMON_STREAM, COMMON, "STREAM", 6, 0x00004D4145525459ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STREAMP, COMMON, "STREAMP", 7, 0x00504D414552545AULL, 0, 0, 0 },
{ CONSTANT_COMMON_STREAM_ELEMENT_TYPE, COMMON, "STREAM-ELEMENT-TYPE", 19, 0x195AA18F8AE4EA0BULL, 0, 0, 0 },
{ CONSTANT_COMMON_STREAM_ERROR, COMMON, "STREAM-ERROR", 12, 0x452D4D4197A1A6B1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STREAM_ERROR_STREAM, COMMON, "STREAM-ERROR-STREAM", 19, 0x1781A06E97EEE7FDULL, 0, 0, 0 },
{ CONSTANT_COMMON_STREAM_EXTERNAL_FORMAT, COMMON, "STREAM-EXTERNAL-FORMAT", 22, 0x7279E2D0E4E9F807ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING, COMMON, "STRING", 6, 0x0000474E49525459ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_EQL, COMMON, "STRING=", 7, 0x003D474E4952545AULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_NOT_EQL, COMMON, "STRING/=", 8, 0x3D2F474E4952545BULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_LESS, COMMON, "STRING<", 7, 0x003C474E4952545AULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_GREATER, COMMON, "STRING>", 7, 0x003E474E4952545AULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_LESS_EQUAL, COMMON, "STRING<=", 8, 0x3D3C474E4952545BULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_GREATER_EQUAL, COMMON, "STRING>=", 8, 0x3D3E474E4952545BULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_CAPITALIZE, COMMON, "STRING-CAPITALIZE", 17, 0x1D76938F9D9BA4EAULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_DOWNCASE, COMMON, "STRING-DOWNCASE", 15, 0x44729A8F8CA0ABB1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_EQUAL, COMMON, "STRING-EQUAL", 12, 0x452D474E9593A9B0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_GREATERP, COMMON, "STRING-GREATERP", 15, 0x477D99939D9399B4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_LEFT_TRIM, COMMON, "STRING-LEFT-TRIM", 16, 0x197699A276A69AA8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_LESSP, COMMON, "STRING-LESSP", 12, 0x4C2D474E99A5A7A4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_NOT_EQUAL, COMMON, "STRING-NOT-EQUAL", 16, 0x1A6E9C9F8E7FA8B2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_NOT_GREATERP, COMMON, "STRING-NOT-GREATERP", 19, 0x226E8CA090CFFAFAULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_NOT_LESSP, COMMON, "STRING-NOT-LESSP", 16, 0x1E809A93957FA8B2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_RIGHT_TRIM, COMMON, "STRING-RIGHT-TRIM", 17, 0x1B7F9B7B9D9A9BFAULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_STREAM, COMMON, "STRING-STREAM", 13, 0x532D479B8A97A6B4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_TRIM, COMMON, "STRING-TRIM", 11, 0x542D474E499F9DB0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_UPCASE, COMMON, "STRING-UPCASE", 13, 0x552D47939C9397B0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRINGP, COMMON, "STRINGP", 7, 0x0050474E4952545AULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRUCTURE, COMMON, "STRUCTURE", 9, 0x52555443555254A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRUCTURE_CLASS, COMMON, "STRUCTURE-CLASS", 15, 0x52A8A784A19581A7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRUCTURE_OBJECT, COMMON, "STRUCTURE-OBJECT", 16, 0x2698998D97A181A8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STYLE_WARNING, COMMON, "STYLE-WARNING", 13, 0x41572D8C9AA2A2B2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBSETP, COMMON, "SUBSETP", 7, 0x005054455342555AULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBSEQ, COMMON, "SUBSEQ", 6, 0x0000514553425559ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBLIS, COMMON, "SUBLIS", 6, 0x000053494C425559ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBST, COMMON, "SUBST", 5, 0x0000005453425558ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBSTITUTE, COMMON, "SUBSTITUTE", 10, 0x5554495453429AB1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBSTITUTE_IF, COMMON, "SUBSTITUTE-IF", 13, 0x5554499A9C6F9AB4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBSTITUTE_IF_NOT, COMMON, "SUBSTITUTE-IF-NOT", 17, 0x24A2769A9C6F9B0CULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBST_IF, COMMON, "SUBST-IF", 8, 0x46492D545342555BULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBST_IF_NOT, COMMON, "SUBST-IF-NOT", 12, 0x46492D54A791A38CULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBTYPEP, COMMON, "SUBTYPEP", 8, 0x504550595442555BULL, 0, 0, 0 },
{ CONSTANT_COMMON_SVREF, COMMON, "SVREF", 5, 0x0000004645525658ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SXHASH, COMMON, "SXHASH", 6, 0x0000485341485859ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOL, COMMON, "SYMBOL", 6, 0x00004C4F424D5959ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOL_FUNCTION, COMMON, "SYMBOL-FUNCTION", 15, 0x467B9B989690A7B7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOL_MACROLET, COMMON, "SYMBOL-MACROLET", 15, 0x4D81919B919F9CA3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOL_NAME, COMMON, "SYMBOL-NAME", 11, 0x4E2D4C4F4292A69FULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOL_PACKAGE, COMMON, "SYMBOL-PACKAGE", 14, 0x502D919683989CA2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOL_PLIST, COMMON, "SYMBOL-PLIST", 12, 0x502D4C4F96A0A2ABULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOL_VALUE, COMMON, "SYMBOL-VALUE", 12, 0x562D4C4F87A2A5A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOLP, COMMON, "SYMBOLP", 7, 0x00504C4F424D595AULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYNONYM_STREAM, COMMON, "SYNONYM-STREAM", 14, 0x2D4DA68F94A0ADB4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYNONYM_STREAM_SYMBOL, COMMON, "SYNONYM-STREAM-SYMBOL", 21, 0x007AA6DBE3E2FB14ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TAGBODY, COMMON, "TAGBODY", 7, 0x0059444F4247415BULL, 0, 0, 0 },
{ CONSTANT_COMMON_TAILP, COMMON, "TAILP", 5, 0x000000504C494159ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TAN, COMMON, "TAN", 3, 0x00000000004E4157ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TANH, COMMON, "TANH", 4, 0x00000000484E4158ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TENTH, COMMON, "TENTH", 5, 0x00000048544E4559ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TERPRI, COMMON, "TERPRI", 6, 0x000049525052455AULL, 0, 0, 0 },
{ CONSTANT_COMMON_THE, COMMON, "THE", 3, 0x0000000000454857ULL, 0, 0, 0 },
{ CONSTANT_COMMON_THIRD, COMMON, "THIRD", 5, 0x0000004452494859ULL, 0, 0, 0 },
{ CONSTANT_COMMON_THROW, COMMON, "THROW", 5, 0x000000574F524859ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TIME, COMMON, "TIME", 4, 0x00000000454D4958ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TRACE, COMMON, "TRACE", 5, 0x0000004543415259ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TRANSLATE_LOGICAL_PATHNAME, COMMON, "TRANSLATE-LOGICAL-PATHNAME", 26, 0x56D2DDEEDEDDF24CULL, 0, 0, 0 },
{ CONSTANT_COMMON_TRANSLATE_PATHNAME, COMMON, "TRANSLATE-PATHNAME", 18, 0x158F94A78F91C4F8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TREE_EQUAL, COMMON, "TREE-EQUAL", 10, 0x5551452D45459E9FULL, 0, 0, 0 },
{ CONSTANT_COMMON_TRUENAME, COMMON, "TRUENAME", 8, 0x454D414E4555525CULL, 0, 0, 0 },
{ CONSTANT_COMMON_TRUNCATE, COMMON, "TRUNCATE", 8, 0x455441434E55525CULL, 0, 0, 0 },
{ CONSTANT_COMMON_TWO_WAY_STREAM, COMMON, "TWO-WAY-STREAM", 14, 0x2D598E9872A1ABB5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TWO_WAY_STREAM_INPUT_STREAM, COMMON, "TWO-WAY-STREAM-INPUT-STREAM", 27, 0x48DAE1C5C7443D55ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TWO_WAY_STREAM_OUTPUT_STREAM, COMMON, "TWO-WAY-STREAM-OUTPUT-STREAM", 28, 0x50D9BBED1533456AULL, 0, 0, 0 },
{ CONSTANT_COMMON_TYPE, COMMON, "TYPE", 4, 0x0000000045505958ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TYPEP, COMMON, "TYPEP", 5, 0x0000005045505959ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TYPE_ERROR, COMMON, "TYPE-ERROR", 10, 0x5252452D4550ABADULL, 0, 0, 0 },
{ CONSTANT_COMMON_TYPE_ERROR_DATUM, COMMON, "TYPE-ERROR-DATUM", 16, 0x1FA7996E897DABB3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TYPE_ERROR_EXPECTED_TYPE, COMMON, "TYPE-ERROR-EXPECTED-TYPE", 24, 0x5AE7EED9B7C1F10FULL, 0, 0, 0 },
{ CONSTANT_COMMON_TYPE_OF, COMMON, "TYPE-OF", 7, 0x00464F2D4550595BULL, 0, 0, 0 },
{ CONSTANT_COMMON_TYPECASE, COMMON, "TYPECASE", 8, 0x455341434550595CULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNBOUND_SLOT, COMMON, "UNBOUND-SLOT", 12, 0x2D444E55A3919AB4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNBOUND_SLOT_INSTANCE, COMMON, "UNBOUND-SLOT-INSTANCE", 21, 0x009297C7E6DFDC11ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNBOUND_VARIABLE, COMMON, "UNBOUND-VARIABLE", 16, 0x7290909698948FBBULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNDEFINED_FUNCTION, COMMON, "UNDEFINED-FUNCTION", 18, 0x0EA28C949A8AC9FAULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNEXPORT, COMMON, "UNEXPORT", 8, 0x54524F5058454E5DULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNINTERN, COMMON, "UNINTERN", 8, 0x4E5245544E494E5DULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNION, COMMON, "UNION", 5, 0x0000004E4F494E5AULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNLESS, COMMON, "UNLESS", 6, 0x00005353454C4E5BULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNREAD_CHAR, COMMON, "UNREAD-CHAR", 11, 0x432D444145A48FA8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNSIGNED_BYTE, COMMON, "UNSIGNED-BYTE", 13, 0x44454E8C9DAC908FULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNTRACE, COMMON, "UNTRACE", 7, 0x0045434152544E5CULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNUSE_PACKAGE, COMMON, "UNUSE-PACKAGE", 13, 0x41502D8A9A9699A5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNWIND_PROTECT, COMMON, "UNWIND-PROTECT", 14, 0x502D98918EAB9DB5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UPDATE_INSTANCE_FOR_DIFFERENT_CLASS, COMMON, "UPDATE-INSTANCE-FOR-DIFFERENT-CLASS", 35, 0x08FBFF3AFE839892ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UPDATE_INSTANCE_FOR_REDEFINED_CLASS, COMMON, "UPDATE-INSTANCE-FOR-REDEFINED-CLASS", 35, 0x07F9FB38F58C8F93ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UPGRADED_ARRAY_ELEMENT_TYPE, COMMON, "UPGRADED-ARRAY-ELEMENT-TYPE", 27, 0x5D9FF1D0EA2C2742ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UPGRADED_COMPLEX_PART_TYPE, COMMON, "UPGRADED-COMPLEX-PART-TYPE", 26, 0x75DEBDE5F1D82919ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UPPER_CASE_P, COMMON, "UPPER-CASE-P", 12, 0x41432D52957D95B4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_USE_PACKAGE, COMMON, "USE-PACKAGE", 11, 0x4B4341502D8A9AA1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_USE_VALUE, COMMON, "USE-VALUE", 9, 0x554C41562D4553A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_USER_HOMEDIR_PATHNAME, COMMON, "USER-HOMEDIR-PATHNAME", 21, 0x2190989FF1CFE5F7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_VALUES, COMMON, "VALUES", 6, 0x00005345554C415CULL, 0, 0, 0 },
{ CONSTANT_COMMON_VALUES_LIST, COMMON, "VALUES-LIST", 11, 0x4C2D534555A094AAULL, 0, 0, 0 },
{ CONSTANT_COMMON_VARIABLE, COMMON, "VARIABLE", 8, 0x454C42414952415EULL, 0, 0, 0 },
{ CONSTANT_COMMON_VECTOR, COMMON, "VECTOR", 6, 0x0000524F5443455CULL, 0, 0, 0 },
{ CONSTANT_COMMON_VECTOR_POP, COMMON, "VECTOR-POP", 10, 0x502D524F544395AFULL, 0, 0, 0 },
{ CONSTANT_COMMON_VECTOR_PUSH, COMMON, "VECTOR-PUSH", 11, 0x502D524F548B98B6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_VECTOR_PUSH_EXTEND, COMMON, "VECTOR-PUSH-EXTEND", 18, 0x1581AA94818BDD0BULL, 0, 0, 0 },
{ CONSTANT_COMMON_VECTORP, COMMON, "VECTORP", 7, 0x0050524F5443455DULL, 0, 0, 0 },
{ CONSTANT_COMMON_WARN, COMMON, "WARN", 4, 0x000000004E52415BULL, 0, 0, 0 },
{ CONSTANT_COMMON_WARNING, COMMON, "WARNING", 7, 0x00474E494E52415EULL, 0, 0, 0 },
{ CONSTANT_COMMON_WILD_PATHNAME_P, COMMON, "WILD-PATHNAME-P", 15, 0x54917D72918D97AEULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_ACCESSORS, COMMON, "WITH-ACCESSORS", 14, 0x4343947F97A79CAAULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_COMPILATION_UNIT, COMMON, "WITH-COMPILATION-UNIT", 21, 0x1B9E8CD5D2EEE7E9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_CONDITION_RESTARTS, COMMON, "WITH-CONDITION-RESTARTS", 23, 0x20CFE5CED2FCE5F7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_HASH_TABLE_ITERATOR, COMMON, "WITH-HASH-TABLE-ITERATOR", 24, 0x52D5E8B0DBEDCB00ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_INPUT_FROM_STRING, COMMON, "WITH-INPUT-FROM-STRING", 22, 0x7D9BDFCDD7D3F215ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_OPEN_FILE, COMMON, "WITH-OPEN-FILE", 14, 0x45509479919A76B3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_OPEN_STREAM, COMMON, "WITH-OPEN-STREAM", 16, 0x1291947F9CA776B5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_OUTPUT_TO_STRING, COMMON, "WITH-OUTPUT-TO-STRING", 21, 0x27829EC8C3F1F110ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_PACKAGE_ITERATOR, COMMON, "WITH-PACKAGE-ITERATOR", 21, 0x089599ACDCEFCC09ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_SIMPLE_RESTART, COMMON, "WITH-SIMPLE-RESTART", 19, 0x219C987F75EDE7FBULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_SLOTS, COMMON, "WITH-SLOTS", 10, 0x4F4C532D48549CB5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_STANDARD_IO_SYNTAX, COMMON, "WITH-STANDARD-IO-SYNTAX", 23, 0x10F5C1C5E8EEE0E9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WHEN, COMMON, "WHEN", 4, 0x000000004E45485BULL, 0, 0, 0 },
{ CONSTANT_COMMON_WRITE, COMMON, "WRITE", 5, 0x000000455449525CULL, 0, 0, 0 },
{ CONSTANT_COMMON_WRITE_BYTE, COMMON, "WRITE-BYTE", 10, 0x59422D45544997B5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WRITE_CHAR, COMMON, "WRITE-CHAR", 10, 0x48432D455449A4A2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WRITE_LINE, COMMON, "WRITE-LINE", 10, 0x494C2D45544997AFULL, 0, 0, 0 },
{ CONSTANT_COMMON_WRITE_SEQUENCE, COMMON, "WRITE-SEQUENCE", 14, 0x45537288A28EA7B6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WRITE_STRING, COMMON, "WRITE-STRING", 12, 0x54532D459B979BB5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WRITE_TO_STRING, COMMON, "WRITE-TO-STRING", 15, 0x4F9B7B8EA69DA593ULL, 0, 0, 0 },
{ CONSTANT_COMMON_YES_OR_NO_P, COMMON, "YES-OR-NO-P", 11, 0x4E2D524F2DA372B3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_Y_OR_N_P, COMMON, "Y-OR-N-P", 8, 0x502D4E2D524F2D61ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ZEROP, COMMON, "ZEROP", 5, 0x000000504F52455FULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DATUM, CLOS, "DATUM", 5, 0x0000004D55544149ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_EXPECTED_TYPE, CLOS, "EXPECTED-TYPE", 13, 0x4445548895A9AC7FULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, CLOS, "FORMAT-ARGUMENTS", 16, 0x1481A2869AA796A8ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FORMAT_CONTROL, CLOS, "FORMAT-CONTROL", 14, 0x432DA0909FA69DA3ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_OPERANDS, CLOS, "OPERANDS", 8, 0x53444E4152455057ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_OPERATION, CLOS, "OPERATION", 9, 0x4F495441524550A6ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_PACKAGE, CLOS, "PACKAGE", 7, 0x004547414B434157ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_PATHNAME, CLOS, "PATHNAME", 8, 0x454D414E48544158ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_STREAM, CLOS, "STREAM", 6, 0x00004D4145525459ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_NAME, CLOS, "NAME", 4, 0x00000000454D4152ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DIRECT_SLOTS, CLOS, "DIRECT-SLOTS", 12, 0x532D544398A6989CULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DIRECT_SUBCLASSES, CLOS, "DIRECT-SUBCLASSES", 17, 0x1880A78491958BFDULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DIRECT_SUPERCLASSES, CLOS, "DIRECT-SUPERCLASSES", 19, 0x266EA08697EADEFFULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_PRECEDENCE_LIST, CLOS, "CLASS-PRECEDENCE-LIST", 21, 0x17937BECEACFDBCAULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_EFFECTIVE_SLOTS, CLOS, "EFFECTIVE-SLOTS", 15, 0x569CA89291997399ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FINALIZED_P, CLOS, "FINALIZED-P", 11, 0x455A494C419E7695ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_PROTOTYPE, CLOS, "PROTOTYPE", 9, 0x5059544F544F529EULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DEFAULT_INITARGS, CLOS, "DEFAULT-INITARGS", 16, 0x009B9E96958F939DULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DIRECT_DEFAULT_INITARGS, CLOS, "DIRECT-DEFAULT-INITARGS", 23, 0x0DADEFE1DBE7D8EEULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_VERSION, CLOS, "VERSION", 7, 0x004E4F495352455DULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_REDEFINED_CLASS, CLOS, "REDEFINED-CLASS", 15, 0x45A19C87918772A5ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_NAME, KEYWORD, "NAME", 4, 0x00000000454D4152ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_DIRECT_SLOTS, KEYWORD, "DIRECT-SLOTS", 12, 0x532D544398A6989CULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_DIRECT_SUBCLASSES, KEYWORD, "DIRECT-SUBCLASSES", 17, 0x1880A78491958BFDULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_DIRECT_SUPERCLASSES, KEYWORD, "DIRECT-SUPERCLASSES", 19, 0x266EA08697EADEFFULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_CLASS_PRECEDENCE_LIST, KEYWORD, "CLASS-PRECEDENCE-LIST", 21, 0x17937BECEACFDBCAULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_EFFECTIVE_SLOTS, KEYWORD, "EFFECTIVE-SLOTS", 15, 0x569CA89291997399ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_FINALIZED_P, KEYWORD, "FINALIZED-P", 11, 0x455A494C419E7695ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_PROTOTYPE, KEYWORD, "PROTOTYPE", 9, 0x5059544F544F529EULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_DEFAULT_INITARGS, KEYWORD, "DEFAULT-INITARGS", 16, 0x009B9E96958F939DULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_DIRECT_DEFAULT_INITARGS, KEYWORD, "DIRECT-DEFAULT-INITARGS", 23, 0x0DADEFE1DBE7D8EEULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_VERSION, KEYWORD, "VERSION", 7, 0x004E4F495352455DULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_REDEFINED_CLASS, KEYWORD, "REDEFINED-CLASS", 15, 0x45A19C87918772A5ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHODS, CLOS, "METHODS", 7, 0x0053444F48544554ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_LAMBDA_LIST, CLOS, "LAMBDA-LIST", 11, 0x4C2D414442A194A0ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ARGUMENT_PRECEDENCE_ORDER, CLOS, "ARGUMENT-PRECEDENCE-ORDER", 25, 0x5ED6DCDFC7DEE627ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DECLARATIONS, CLOS, "DECLARATIONS", 12, 0x544152419F919499ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHOD_CLASS, CLOS, "METHOD-CLASS", 12, 0x432D444F9BA786A5ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHOD_COMBINATION, CLOS, "METHOD-COMBINATION", 18, 0x0C81859D9196E0FDULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_VECTOR, CLOS, "VECTOR", 6, 0x0000524F5443455CULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_REMOVE, CLOS, "REMOVE", 6, 0x000045564F4D4558ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ARGUMENT, CLOS, "ARGUMENT", 8, 0x544E454D55475249ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_EQLCHECK, CLOS, "EQLCHECK", 8, 0x4B434548434C514DULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CACHE, CLOS, "CACHE", 5, 0x0000004548434148ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CALL, CLOS, "CALL", 4, 0x000000004C4C4147ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FUNCTION, CLOS, "FUNCTION", 8, 0x4E4F4954434E554EULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_PRECEDENCE_INDEX, CLOS, "PRECEDENCE-INDEX", 16, 0x268A88938C7297A3ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_METHODS, KEYWORD, "METHODS", 7, 0x0053444F48544554ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_LAMBDA_LIST, KEYWORD, "LAMBDA-LIST", 11, 0x4C2D414442A194A0ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_ARGUMENT_PRECEDENCE_ORDER, KEYWORD, "ARGUMENT-PRECEDENCE-ORDER", 25, 0x5ED6DCDFC7DEE627ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_DECLARATIONS, KEYWORD, "DECLARATIONS", 12, 0x544152419F919499ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_METHOD_CLASS, KEYWORD, "METHOD-CLASS", 12, 0x432D444F9BA786A5ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_METHOD_COMBINATION, KEYWORD, "METHOD-COMBINATION", 18, 0x0C81859D9196E0FDULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_VECTOR, KEYWORD, "VECTOR", 6, 0x0000524F5443455CULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_REMOVE, KEYWORD, "REMOVE", 6, 0x000045564F4D4558ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_ARGUMENT, KEYWORD, "ARGUMENT", 8, 0x544E454D55475249ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_EQLCHECK, KEYWORD, "EQLCHECK", 8, 0x4B434548434C514DULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_CACHE, KEYWORD, "CACHE", 5, 0x0000004548434148ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_CALL, KEYWORD, "CALL", 4, 0x000000004C4C4147ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_FUNCTION, KEYWORD, "FUNCTION", 8, 0x4E4F4954434E554EULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_PRECEDENCE_INDEX, KEYWORD, "PRECEDENCE-INDEX", 16, 0x268A88938C7297A3ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION, CLOS, "GENERIC-FUNCTION", 16, 0x7B9292A6889C9A9DULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_QUALIFIERS, CLOS, "QUALIFIERS", 10, 0x454946494C41A8ADULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SPECIALIZERS, CLOS, "SPECIALIZERS", 12, 0x494C4149969795B9ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_LONG_P, CLOS, "LONG-P", 6, 0x0000502D474E4F52ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DOCUMENTATION, CLOS, "DOCUMENTATION", 13, 0x544E459BA48CA392ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_IDENTITY, CLOS, "IDENTITY", 8, 0x595449544E454451ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_OPERATOR, CLOS, "OPERATOR", 8, 0x524F544152455057ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ARGUMENTS, CLOS, "ARGUMENTS", 9, 0x544E454D5547529DULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC, CLOS, "GENERIC", 7, 0x00434952454E454EULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FORM, CLOS, "FORM", 4, 0x000000004D524F4AULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DECLARE, CLOS, "DECLARE", 7, 0x004552414C43454BULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_GENERIC_FUNCTION, KEYWORD, "GENERIC-FUNCTION", 16, 0x7B9292A6889C9A9DULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_QUALIFIERS, KEYWORD, "QUALIFIERS", 10, 0x454946494C41A8ADULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_SPECIALIZERS, KEYWORD, "SPECIALIZERS", 12, 0x494C4149969795B9ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_LONG_P, KEYWORD, "LONG-P", 6, 0x0000502D474E4F52ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_DOCUMENTATION, KEYWORD, "DOCUMENTATION", 13, 0x544E459BA48CA392ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_IDENTITY, KEYWORD, "IDENTITY", 8, 0x595449544E454451ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_OPERATOR, KEYWORD, "OPERATOR", 8, 0x524F544152455057ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_ARGUMENTS, KEYWORD, "ARGUMENTS", 9, 0x544E454D5547529DULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_GENERIC, KEYWORD, "GENERIC", 7, 0x00434952454E454EULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_FORM, KEYWORD, "FORM", 4, 0x000000004D524F4AULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_DECLARE, KEYWORD, "DECLARE", 7, 0x004552414C43454BULL, 0, 0, 1 },
{ CONSTANT_CLOSNAME_OBJECT, CLOS, "OBJECT", 6, 0x00005443454A4255ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_TYPE, CLOS, "TYPE", 4, 0x0000000045505958ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_OBJECT, KEYWORD, "OBJECT", 6, 0x00005443454A4255ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_TYPE, KEYWORD, "TYPE", 4, 0x0000000045505958ULL, 0, 0, 1 },
{ CONSTANT_CLOSNAME_READERS, CLOS, "READERS", 7, 0x0053524544414559ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_WRITERS, CLOS, "WRITERS", 7, 0x005352455449525EULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ACCESSORS, CLOS, "ACCESSORS", 9, 0x524F53534543439DULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_INITARGS, CLOS, "INITARGS", 8, 0x5347524154494E51ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_INITFORM, CLOS, "INITFORM", 8, 0x4D524F4654494E51ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_INITFUNCTION, CLOS, "INITFUNCTION", 12, 0x434E5546A29897A9ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ALLOCATION, CLOS, "ALLOCATION", 10, 0x495441434F4C9A9AULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_INSTANCE, CLOS, "INSTANCE", 8, 0x45434E4154534E51ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS, CLOS, "CLASS", 5, 0x0000005353414C48ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METACLASS, CLOS, "METACLASS", 9, 0x53414C43415445A9ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_BINDING, CLOS, "BINDING", 7, 0x00474E49444E4949ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ORDER, CLOS, "ORDER", 5, 0x0000005245445254ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_READERS, KEYWORD, "READERS", 7, 0x0053524544414559ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_WRITERS, KEYWORD, "WRITERS", 7, 0x005352455449525EULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_ACCESSORS, KEYWORD, "ACCESSORS", 9, 0x524F53534543439DULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_INITARGS, KEYWORD, "INITARGS", 8, 0x5347524154494E51ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_INITFORM, KEYWORD, "INITFORM", 8, 0x4D524F4654494E51ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_INITFUNCTION, KEYWORD, "INITFUNCTION", 12, 0x434E5546A29897A9ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_ALLOCATION, KEYWORD, "ALLOCATION", 10, 0x495441434F4C9A9AULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_INSTANCE, KEYWORD, "INSTANCE", 8, 0x45434E4154534E51ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_CLASS, KEYWORD, "CLASS", 5, 0x0000005353414C48ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_METACLASS, KEYWORD, "METACLASS", 9, 0x53414C43415445A9ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_BINDING, KEYWORD, "BINDING", 7, 0x00474E49444E4949ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_ORDER, KEYWORD, "ORDER", 5, 0x0000005245445254ULL, 0, 0, 1 },
{ CONSTANT_CLOSNAME_SLOTS, CLOS, "SLOTS", 5, 0x00000053544F4C58ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_INCLUDE, CLOS, "INCLUDE", 7, 0x004544554C434E50ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_VALUE, CLOS, "VALUE", 5, 0x00000045554C415BULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_PREDICATE, CLOS, "PREDICATE", 9, 0x544143494445529EULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ACCESS, CLOS, "ACCESS", 6, 0x0000535345434347ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_COPIER, CLOS, "COPIER", 6, 0x0000524549504F49ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CONSTRUCTOR, CLOS, "CONSTRUCTOR", 11, 0x4355525453A09EA2ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_SLOTS, KEYWORD, "SLOTS", 5, 0x00000053544F4C58ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_INCLUDE, KEYWORD, "INCLUDE", 7, 0x004544554C434E50ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_VALUE, KEYWORD, "VALUE", 5, 0x00000045554C415BULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_PREDICATE, KEYWORD, "PREDICATE", 9, 0x544143494445529EULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_ACCESS, KEYWORD, "ACCESS", 6, 0x0000535345434347ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_COPIER, KEYWORD, "COPIER", 6, 0x0000524549504F49ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_CONSTRUCTOR, KEYWORD, "CONSTRUCTOR", 11, 0x4355525453A09EA2ULL, 0, 0, 1 },
{ CONSTANT_SYSTEM_VALUE, SYSTEM, "VALUE", 5, 0x00000045554C415BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FUNCTION, SYSTEM, "FUNCTION", 8, 0x4E4F4954434E554EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SETF, SYSTEM, "SETF", 4, 0x0000000046544557ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INLINE_FUNCTION, SYSTEM, "INLINE-FUNCTION", 15, 0x467B94979D8F9CADULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INLINE_SETF, SYSTEM, "INLINE-SETF", 11, 0x532D454E4992A299ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TAGBODY, SYSTEM, "TAGBODY", 7, 0x0059444F4247415BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BLOCK, SYSTEM, "BLOCK", 5, 0x0000004B434F4C47ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DECLAIM, SYSTEM, "DECLAIM", 7, 0x004D49414C43454BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFUN, SYSTEM, "DEFUN", 5, 0x0000004E55464549ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFMACRO, SYSTEM, "DEFMACRO", 8, 0x4F5243414D46454CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFTYPE, SYSTEM, "DEFTYPE", 7, 0x004550595446454BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFINE_COMPILER_MACRO, SYSTEM, "DEFINE-COMPILER-MACRO", 21, 0x707F8AE9E4D9D3F5ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MACRO_LAMBDA, SYSTEM, "MACRO-LAMBDA", 12, 0x414C2D4F938783A6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DESTRUCTURING_BIND, SYSTEM, "DESTRUCTURING-BIND", 18, 0x1D858299A29CDBF9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SPECIAL, SYSTEM, "SPECIAL", 7, 0x004C41494345505AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LEXICAL, SYSTEM, "LEXICAL", 7, 0x004C414349584553ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOCAL, SYSTEM, "LOCAL", 5, 0x0000004C41434F51ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_GLOBAL, SYSTEM, "GLOBAL", 6, 0x00004C41424F4C4DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LAMBDA, SYSTEM, "LAMBDA", 6, 0x00004144424D4152ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SCOPE, SYSTEM, "SCOPE", 5, 0x00000045504F4358ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MULTIPLE_VALUE_BIND, SYSTEM, "MULTIPLE-VALUE-BIND", 19, 0x0779959EA0D1F9D6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DECLARATION, SYSTEM, "DECLARATION", 11, 0x544152414C919498ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INLINE, SYSTEM, "INLINE", 6, 0x0000454E494C4E4FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DYNAMIC_VALUE, SYSTEM, "DYNAMIC-VALUE", 13, 0x2D434992969A9AA7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DYNAMIC_FUNCTION, SYSTEM, "DYNAMIC-FUNCTION", 16, 0x7B9292A1849CAE9AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_IGNORE_VALUE, SYSTEM, "IGNORE-VALUE", 12, 0x562D455294A39396ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_IGNORE_FUNCTION, SYSTEM, "IGNORE-FUNCTION", 15, 0x467B949BA39195ADULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE, SYSTEM, "TYPE", 4, 0x0000000045505958ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_SPECIAL, SYSTEM, "TYPE-SPECIAL", 12, 0x4550532D9191A2A3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_LEXICAL, SYSTEM, "TYPE-LEXICAL", 12, 0x58454C2D91919CA9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_VALUE, SYSTEM, "TYPE-VALUE", 10, 0x4C41562D45509EB3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_FUNCTION, SYSTEM, "TYPE-FUNCTION", 13, 0x4E55467B9499ADA4ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_SETF, SYSTEM, "TYPE-SETF", 9, 0x5445532D455059A3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FUNCTION_ARGTYPE, SYSTEM, "FUNCTION-ARGTYPE", 16, 0x139FA2A88AA09683ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FUNCTION_RETTYPE, SYSTEM, "FUNCTION-RETTYPE", 16, 0x139FA2A89793A783ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_HANDLER, SYSTEM, "HANDLER", 7, 0x0052454C444E414FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_HANDLER_BIND, SYSTEM, "HANDLER-BIND", 12, 0x2D52454C889C8A96ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_HANDLER_CASE, SYSTEM, "HANDLER-CASE", 12, 0x2D52454C89A18297ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_RESTART, SYSTEM, "RESTART", 7, 0x0054524154534559ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_RESTART_BIND, SYSTEM, "RESTART-BIND", 12, 0x2D54524198A18EA0ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_RESTART_CASE, SYSTEM, "RESTART-CASE", 12, 0x2D54524199A686A1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_STANDARD, SYSTEM, "STANDARD", 8, 0x445241444E41545BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_COMPILED_MACRO_FUNCTION, SYSTEM, "COMPILED-MACRO-FUNCTION", 23, 0x0AC0EAE4E7D1EADCULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CONTROL, SYSTEM, "CONTROL", 7, 0x004C4F52544E4F4AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CODE, SYSTEM, "CODE", 4, 0x0000000045444F47ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CALLNAME, SYSTEM, "CALLNAME", 8, 0x454D414E4C4C414BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_EVAL, SYSTEM, "EVAL", 4, 0x000000004C415649ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INDEX, SYSTEM, "INDEX", 5, 0x0000005845444E4EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SYSTEM, SYSTEM, "SYSTEM", 6, 0x00004D4554535959ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_QUOTE, SYSTEM, "QUOTE", 5, 0x00000045544F5556ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ENVIRONMENT, SYSTEM, "ENVIRONMENT", 11, 0x4D4E4F5249AA9C95ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CHARACTER2, SYSTEM, "CHARACTER2", 10, 0x4554434152417A9FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CHARQUEUE, SYSTEM, "CHARQUEUE", 9, 0x5545555152414891ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CHARBIT, SYSTEM, "CHARBIT", 7, 0x005449425241484AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SYMSTACK, SYSTEM, "SYMSTACK", 8, 0x4B434154534D595BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BITTYPE, SYSTEM, "BITTYPE", 7, 0x0045505954544949ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_READLABEL, SYSTEM, "READLABEL", 9, 0x4542414C444145A7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_READINFO_SYMBOL, SYSTEM, "READINFO", 8, 0x4F464E494441455AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_READTYPE, SYSTEM, "READTYPE", 8, 0x455059544441455AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BITCONS, SYSTEM, "BITCONS", 7, 0x00534E4F43544949ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BITBUFFER, SYSTEM, "BITBUFFER", 9, 0x454646554254499DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_HASHITERATOR, SYSTEM, "HASHITERATOR", 12, 0x524554499AA29595ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PACKAGEITERATOR, SYSTEM, "PACKAGEITERATOR", 15, 0x499796958C9586B3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TAGINFO, SYSTEM, "TAGINFO", 7, 0x004F464E4947415BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ARRAY_DIMENSION, SYSTEM, "ARRAY-DIMENSION", 15, 0x49927CA294A0979DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ARRAY_GENERAL, SYSTEM, "ARRAY-GENERAL", 13, 0x45472DA582A4979CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ARRAY_SPECIALIZED, SYSTEM, "ARRAY-SPECIALIZED", 17, 0x15AD76A5829B95DBULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UNBOUND, SYSTEM, "UNBOUND", 7, 0x00444E554F424E5CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SPACE, SYSTEM, "SPACE", 5, 0x0000004543415058ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SPACE1, SYSTEM, "SPACE1", 6, 0x0000314543415059ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_RESERVED, SYSTEM, "RESERVED", 8, 0x444556524553455AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_END, SYSTEM, "END", 3, 0x0000000000444E48ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PROMPT_STREAM, SYSTEM, "PROMPT-STREAM", 13, 0x532D549D8E94A4B1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PRETTY_STREAM, SYSTEM, "PRETTY-STREAM", 13, 0x532D59A1958AA4B1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MEMORY_STREAM, SYSTEM, "MEMORY-STREAM", 13, 0x532D599F909297AEULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PIPE_STREAM, SYSTEM, "PIPE-STREAM", 11, 0x5254532D459D8AA0ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SIMPLE_CONTROL_ERROR, SYSTEM, "SIMPLE-CONTROL-ERROR", 20, 0x085A919BF4F0EA08ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SIMPLE_FILE_ERROR, SYSTEM, "SIMPLE-FILE-ERROR", 17, 0x157F97917D9295FFULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SIMPLE_PACKAGE_ERROR, SYSTEM, "SIMPLE-PACKAGE-ERROR", 20, 0x155A8A93E3E7DEFAULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SIMPLE_PARSE_ERROR, SYSTEM, "SIMPLE-PARSE-ERROR", 18, 0x227F8A7995A0EDF5ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SIMPLE_PROGRAM_ERROR, SYSTEM, "SIMPLE-PROGRAM-ERROR", 20, 0x155A928DF4E3EB0BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SIMPLE_READER_ERROR, SYSTEM, "SIMPLE-READER-ERROR", 19, 0x2472729E95E3D9FDULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SIMPLE_STYLE_WARNING, SYSTEM, "SIMPLE-STYLE-WARNING", 20, 0x256E9C79DCE7EC09ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_INITIALLY, SYSTEM, "LOOP-INITIALLY", 14, 0x494EA2799C9098AEULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FINALLY, SYSTEM, "LOOP-FINALLY", 12, 0x4E49462DA99B9B99ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_WITH, SYSTEM, "LOOP-WITH", 9, 0x5449572D504F4F9DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS, SYSTEM, "LOOP-FOR-AS", 11, 0x524F462D50A29084ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_IN_LIST, SYSTEM, "LOOP-FOR-AS-IN-LIST", 19, 0x1E7C94767DF6E3D5ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_ON_LIST, SYSTEM, "LOOP-FOR-AS-ON-LIST", 19, 0x1E7C947C7DF6E3D5ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_EQUALS_THEN, SYSTEM, "LOOP-FOR-AS-EQUALS-THEN", 23, 0x13F2DCBAD1CFE3DCULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_ACROSS, SYSTEM, "LOOP-FOR-AS-ACROSS", 18, 0x21A1896E7DA2E3DEULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_HASH, SYSTEM, "LOOP-FOR-AS-HASH", 16, 0x1AA287757DA29089ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_PACKAGE_SYMBOL, SYSTEM, "LOOP-FOR-AS-PACKAGE-SYMBOL", 26, 0x5FDFE0D0AAE82423ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_PACKAGE_PRESENT, SYSTEM, "LOOP-FOR-AS-PACKAGE-PRESENT", 27, 0x70D7D9CDAB3C261AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_PACKAGE_EXTERNAL, SYSTEM, "LOOP-FOR-AS-PACKAGE-EXTERNAL", 28, 0x62E6DFC2F7292628ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_ARITHMETIC_UP, SYSTEM, "LOOP-FOR-AS-ARITHMETIC-UP", 25, 0x7BC5DBB7D1E7DE2AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNTO, SYSTEM, "LOOP-FOR-AS-ARITHMETIC-DOWNTO", 29, 0x6AC5DC072636352DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNFROM, SYSTEM, "LOOP-FOR-AS-ARITHMETIC-DOWNFROM", 31, 0x6B132B0A1836352FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_DO, SYSTEM, "LOOP-DO", 7, 0x004F442D504F4F53ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_RETURN, SYSTEM, "LOOP-RETURN", 11, 0x5445522D509DA1ACULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_IF, SYSTEM, "LOOP-IF", 7, 0x0046492D504F4F53ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_UNLESS, SYSTEM, "LOOP-UNLESS", 11, 0x4C4E552D50A2A29CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_COLLECT, SYSTEM, "LOOP-COLLECT", 12, 0x4C4F432DA49294A4ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_APPEND, SYSTEM, "LOOP-APPEND", 11, 0x5050412D50939D9CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_NCONC, SYSTEM, "LOOP-NCONC", 10, 0x4F434E2D504F92A4ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_COUNT, SYSTEM, "LOOP-COUNT", 10, 0x554F432D504FA3A4ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_SUM, SYSTEM, "LOOP-SUM", 8, 0x4D55532D504F4F54ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_MAXIMIZE, SYSTEM, "LOOP-MAXIMIZE", 13, 0x58414D72AA989CA2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_MINIMIZE, SYSTEM, "LOOP-MINIMIZE", 13, 0x4E494D72AA989CA2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_WHILE, SYSTEM, "LOOP-WHILE", 10, 0x4948572D504F94A2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_UNTIL, SYSTEM, "LOOP-UNTIL", 10, 0x544E552D504F9B9FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_REPEAT, SYSTEM, "LOOP-REPEAT", 11, 0x5045522D50A3909CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_ALWAYS, SYSTEM, "LOOP-ALWAYS", 11, 0x574C412D50A2A898ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_NEVER, SYSTEM, "LOOP-NEVER", 10, 0x56454E2D504FA19BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_THEREIS, SYSTEM, "LOOP-THEREIS", 12, 0x4548542DA39894AAULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_NEXT_LOOP, SYSTEM, "NEXT-LOOP", 9, 0x4F4F4C2D545845A7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_END_LOOP, SYSTEM, "END-LOOP", 8, 0x504F4F4C2D444E4DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_VALUE_LOOP, SYSTEM, "VALUE-LOOP", 10, 0x4F4C2D45554C91AFULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FUNCTION_LOOP, SYSTEM, "FUNCTION-LOOP", 13, 0x4E4F49A4929DA180ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_IT_LOOP, SYSTEM, "IT-LOOP", 7, 0x00504F4F4C2D5450ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_STRUCTURE_GENSYM, SYSTEM, "STRUCTURE-GENSYM", 16, 0x1FAEA7919A9981A8ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_STRUCTURE_NAMED, SYSTEM, "STRUCTURE-NAMED", 15, 0x5299999096A081A7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CACHE, SYSTEM, "CACHE", 5, 0x0000004548434148ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_DOCUMENTATION, SYSTEM, "TYPE-DOCUMENTATION", 18, 0x0CA385819395F50AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_COMPILER_MACRO_FUNCTION, SYSTEM, "COMPILER-MACRO-FUNCTION", 23, 0x18C0EAE4E7D1EADCULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SETF_COMPILER_MACRO_FUNCTION, SYSTEM, "SETF-COMPILER-MACRO-FUNCTION", 28, 0x51EAC5C6073F2A56ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_COMPILE_WARNING, SYSTEM, "COMPILE-WARNING", 15, 0x2D8C9A929E9F90A9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_COMPILE_STYLE_WARNING, SYSTEM, "COMPILE-STYLE-WARNING", 21, 0x6E9C79D5EAEFF1FDULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_NTH_VALUE, SYSTEM, "NTH-VALUE", 9, 0x554C41562D48549CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_OPTIMIZE_CHECK, SYSTEM, "OPTIMIZE-CHECK", 14, 0x455A94908E9C938AULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_CAST_SINGLE_FLOAT, SYSTEM, "CAST-SINGLE-FLOAT", 17, 0x0F989F7381988DEFULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CAST_DOUBLE_FLOAT, SYSTEM, "CAST-DOUBLE-FLOAT", 17, 0x169E907381988DEAULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CAST_LONG_FLOAT, SYSTEM, "CAST-LONG-FLOAT", 15, 0x4EA38D7CA0996E99ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BYTESPEC, SYSTEM, "BYTESPEC", 8, 0x434550534554594AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_SYMBOL, SYSTEM, "TYPE-SYMBOL", 11, 0x4D59532D459CA8A1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_LIST, SYSTEM, "TYPE-LIST", 9, 0x53494C2D455059B1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UNIX, SYSTEM, "UNIX", 4, 0x0000000058494E59ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_WINDOWS, SYSTEM, "WINDOWS", 7, 0x0053574F444E495EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UNIVERSAL, SYSTEM, "UNIVERSAL", 9, 0x4153524556494EAAULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEVICE, SYSTEM, "DEVICE", 6, 0x000045434956454AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOGICAL_PATHNAME, SYSTEM, "LOGICAL-PATHNAME", 16, 0x72998291919B90ACULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TIME1970, SYSTEM, "TIME1970", 8, 0x30373931454D495CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ASCII, SYSTEM, "ASCII", 5, 0x0000004949435346ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_8, SYSTEM, "UTF-8", 5, 0x000000382D46545AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_8_BOM, SYSTEM, "UTF-8-BOM", 9, 0x4F422D382D4654ABULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_16, SYSTEM, "UTF-16", 6, 0x000036312D46545BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_16LE, SYSTEM, "UTF-16LE", 8, 0x454C36312D46545DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_16BE, SYSTEM, "UTF-16BE", 8, 0x454236312D46545DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_16LE_BOM, SYSTEM, "UTF-16LE-BOM", 12, 0x454C36317A95968EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_16BE_BOM, SYSTEM, "UTF-16BE-BOM", 12, 0x454236317A95968EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_32, SYSTEM, "UTF-32", 6, 0x000032332D46545BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_32LE, SYSTEM, "UTF-32LE", 8, 0x454C32332D46545DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_32BE, SYSTEM, "UTF-32BE", 8, 0x454232332D46545DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_32LE_BOM, SYSTEM, "UTF-32LE-BOM", 12, 0x454C32337A95968EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_32BE_BOM, SYSTEM, "UTF-32BE-BOM", 12, 0x454232337A95968EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CR, SYSTEM, "CR", 2, 0x0000000000005245ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LF, SYSTEM, "LF", 2, 0x000000000000464EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CRLF, SYSTEM, "CRLF", 4, 0x00000000464C5247ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_AUTO, SYSTEM, "AUTO", 4, 0x000000004F545545ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CLOSE_ABORT, SYSTEM, "CLOSE-ABORT", 11, 0x42412D4553A39E9DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PRINT_DISPATCH, SYSTEM, "PRINT-DISPATCH", 14, 0x49447597A28AA2B1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_N, SYSTEM, "N", 1, 0x000000000000004FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_A, SYSTEM, "A", 1, 0x0000000000000042ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_H, SYSTEM, "H", 1, 0x0000000000000049ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_W, SYSTEM, "W", 1, 0x0000000000000058ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_F, SYSTEM, "F", 1, 0x0000000000000047ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_NA, SYSTEM, "NA", 2, 0x0000000000004150ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DISPATCH_VECTOR, SYSTEM, "DISPATCH-VECTOR", 15, 0x4895A39593989F80ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DISPATCH_QUOTE, SYSTEM, "DISPATCH-QUOTE", 14, 0x484399959FA89A7FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DISPATCH_CALL, SYSTEM, "DISPATCH-CALL", 13, 0x4843548D9C948C7EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DISPATCH_DEFUN, SYSTEM, "DISPATCH-DEFUN", 14, 0x4843A29696988D7FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DISPATCH_LET, SYSTEM, "DISPATCH-LET", 12, 0x48435441A498957DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DELAY_WARNING, SYSTEM, "DELAY-WARNING", 13, 0x41572DA08F9593A3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DOC_TYPE, SYSTEM, "DOC-TYPE", 8, 0x455059542D434F4CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_OBJECT, SYSTEM, "OBJECT", 6, 0x00005443454A4255ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_STEP, SYSTEM, "STEP", 4, 0x0000000050455457ULL, 0, 0, 0 },
{ CONSTANT_CODE_NOP, CODE, "NOP", 3, 0x0000000000504F51ULL, 0, 0, 0 },
{ CONSTANT_CODE_BEGIN, CODE, "BEGIN", 5, 0x0000004E49474547ULL, 0, 0, 0 },
{ CONSTANT_CODE_BEGIN_CALL, CODE, "BEGIN-CALL", 10, 0x41432D4E49479198ULL, 0, 0, 0 },
{ CONSTANT_CODE_END, CODE, "END", 3, 0x0000000000444E48ULL, 0, 0, 0 },
{ CONSTANT_CODE_ESCAPE, CODE, "ESCAPE", 6, 0x000045504143534BULL, 0, 0, 0 },
{ CONSTANT_CODE_ESCAPE_NOT, CODE, "ESCAPE-NOT", 10, 0x4E2D45504143A79EULL, 0, 0, 0 },
{ CONSTANT_CODE_SAVE, CODE, "SAVE", 4, 0x0000000045564157ULL, 0, 0, 0 },
{ CONSTANT_CODE_RESTORE, CODE, "RESTORE", 7, 0x0045524F54534559ULL, 0, 0, 0 },
{ CONSTANT_CODE_NORMAL, CODE, "NORMAL", 6, 0x00004C414D524F54ULL, 0, 0, 0 },
{ CONSTANT_CODE_REVERT, CODE, "REVERT", 6, 0x0000545245564558ULL, 0, 0, 0 },
{ CONSTANT_CODE_REVERT_GOTO, CODE, "REVERT-GOTO", 11, 0x472D545245A599ACULL, 0, 0, 0 },
{ CONSTANT_CODE_SET, CODE, "SET", 3, 0x0000000000544556ULL, 0, 0, 0 },
{ CONSTANT_CODE_PUSH, CODE, "PUSH", 4, 0x0000000048535554ULL, 0, 0, 0 },
{ CONSTANT_CODE_PUSH_RESULT, CODE, "PUSH-RESULT", 11, 0x5345522D48A7A1B0ULL, 0, 0, 0 },
{ CONSTANT_CODE_PUSH_VALUES, CODE, "PUSH-VALUES", 11, 0x4C41562D48A69AB0ULL, 0, 0, 0 },
{ CONSTANT_CODE_NIL_SET, CODE, "NIL-SET", 7, 0x005445532D4C4955ULL, 0, 0, 0 },
{ CONSTANT_CODE_NIL_PUSH, CODE, "NIL-PUSH", 8, 0x485355502D4C4956ULL, 0, 0, 0 },
{ CONSTANT_CODE_T_SET, CODE, "T-SET", 5, 0x0000005445532D59ULL, 0, 0, 0 },
{ CONSTANT_CODE_T_PUSH, CODE, "T-PUSH", 6, 0x0000485355502D5AULL, 0, 0, 0 },
{ CONSTANT_CODE_LEXICAL, CODE, "LEXICAL", 7, 0x004C414349584553ULL, 0, 0, 0 },
{ CONSTANT_CODE_LEXICAL_SET, CODE, "LEXICAL-SET", 11, 0x2D4C414349AC8AAAULL, 0, 0, 0 },
{ CONSTANT_CODE_LEXICAL_PUSH, CODE, "LEXICAL-PUSH", 12, 0x2D4C414391AB9AA8ULL, 0, 0, 0 },
{ CONSTANT_CODE_LEXICAL_REM, CODE, "LEXICAL-REM", 11, 0x2D4C414349A58AA9ULL, 0, 0, 0 },
{ CONSTANT_CODE_LEXICAL_FREE, CODE, "LEXICAL-FREE", 12, 0x2D4C41438E9D979EULL, 0, 0, 0 },
{ CONSTANT_CODE_SPECIAL, CODE, "SPECIAL", 7, 0x004C41494345505AULL, 0, 0, 0 },
{ CONSTANT_CODE_SPECIAL_SET, CODE, "SPECIAL-SET", 11, 0x2D4C4149439995B1ULL, 0, 0, 0 },
{ CONSTANT_CODE_SPECIAL_PUSH, CODE, "SPECIAL-PUSH", 12, 0x2D4C41498B98A5AFULL, 0, 0, 0 },
{ CONSTANT_CODE_SPECIAL_REM, CODE, "SPECIAL-REM", 11, 0x2D4C4149439295B0ULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_SPECIAL, CODE, "DECLAIM-SPECIAL", 15, 0x2D998A8A8F8895A6ULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_TYPE_VALUE, CODE, "DECLAIM-TYPE-VALUE", 18, 0x798E9F6E9193E3FFULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_TYPE_FUNCTION, CODE, "DECLAIM-TYPE-FUNCTION", 21, 0x7BA28FBCE0DCF2F0ULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_INLINE, CODE, "DECLAIM-INLINE", 14, 0x2D4D8E8F958F939BULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_NOTINLINE, CODE, "DECLAIM-NOTINLINE", 17, 0x7B96958F959794E8ULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_COMPILATION, CODE, "DECLAIM-COMPILATION", 19, 0x018E958A9CDEE3E3ULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_DEBUG, CODE, "DECLAIM-DEBUG", 13, 0x2D4D4988A1858A95ULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_SAFETY, CODE, "DECLAIM-SAFETY", 14, 0x2D4DA295918986A5ULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_SPACE, CODE, "DECLAIM-SPACE", 13, 0x2D4D49868F8495A4ULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_SPEED, CODE, "DECLAIM-SPEED", 13, 0x2D4D4985918895A4ULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_DECLARATION, CODE, "DECLAIM-DECLARATION", 19, 0x018E9B8298D4D9E4ULL, 0, 0, 0 },
{ CONSTANT_CODE_TYPE_RESULT, CODE, "TYPE-RESULT", 11, 0x5345522D45A4A5B4ULL, 0, 0, 0 },
{ CONSTANT_CODE_TYPE_LEXICAL, CODE, "TYPE-LEXICAL", 12, 0x58454C2D91919CA9ULL, 0, 0, 0 },
{ CONSTANT_CODE_TYPE_SPECIAL, CODE, "TYPE-SPECIAL", 12, 0x4550532D9191A2A3ULL, 0, 0, 0 },
{ CONSTANT_CODE_TYPE_GLOBAL, CODE, "TYPE-GLOBAL", 11, 0x4F4C472D459C9AA1ULL, 0, 0, 0 },
{ CONSTANT_CODE_TYPE_FUNCTION, CODE, "TYPE-FUNCTION", 13, 0x4E55467B9499ADA4ULL, 0, 0, 0 },
{ CONSTANT_CODE_TYPE_SETF, CODE, "TYPE-SETF", 9, 0x5445532D455059A3ULL, 0, 0, 0 },
{ CONSTANT_CODE_LET_LEXICAL, CODE, "LET-LEXICAL", 11, 0x4958454C2DA0869AULL, 0, 0, 0 },
{ CONSTANT_CODE_LET_SPECIAL, CODE, "LET-SPECIAL", 11, 0x434550532DA086A0ULL, 0, 0, 0 },
{ CONSTANT_CODE_LETA_SPECIAL, CODE, "LET*-SPECIAL", 12, 0x4550532D76958E9BULL, 0, 0, 0 },
{ CONSTANT_CODE_SETQ_LEXICAL, CODE, "SETQ-LEXICAL", 12, 0x58454C2D9D9588A8ULL, 0, 0, 0 },
{ CONSTANT_CODE_SETQ_SPECIAL, CODE, "SETQ-SPECIAL", 12, 0x4550532D9D958EA2ULL, 0, 0, 0 },
{ CONSTANT_CODE_SETQ_GLOBAL, CODE, "SETQ-GLOBAL", 11, 0x4F4C472D51A086A0ULL, 0, 0, 0 },
{ CONSTANT_CODE_FUNCTION_SET, CODE, "FUNCTION-SET", 12, 0x4E4F49549793A87FULL, 0, 0, 0 },
{ CONSTANT_CODE_FUNCTION_PUSH, CODE, "FUNCTION-PUSH", 13, 0x4E4F499C96A3A580ULL, 0, 0, 0 },
{ CONSTANT_CODE_SETF_SET, CODE, "SETF-SET", 8, 0x5445532D4654455BULL, 0, 0, 0 },
{ CONSTANT_CODE_SETF_PUSH, CODE, "SETF-PUSH", 9, 0x5355502D465445A4ULL, 0, 0, 0 },
{ CONSTANT_CODE_DEFMACRO, CODE, "DEFMACRO", 8, 0x4F5243414D46454CULL, 0, 0, 0 },
{ CONSTANT_CODE_DEFTYPE, CODE, "DEFTYPE", 7, 0x004550595446454BULL, 0, 0, 0 },
{ CONSTANT_CODE_DEFINE_COMPILER_MACRO, CODE, "DEFINE-COMPILER-MACRO", 21, 0x707F8AE9E4D9D3F5ULL, 0, 0, 0 },
{ CONSTANT_CODE_DEFUN, CODE, "DEFUN", 5, 0x0000004E55464549ULL, 0, 0, 0 },
{ CONSTANT_CODE_CALL_NAME, CODE, "CALL-NAME", 9, 0x4D414E2D4C4C4191ULL, 0, 0, 0 },
{ CONSTANT_CODE_CALL_RESULT, CODE, "CALL-RESULT", 11, 0x5345522D4CA08DA3ULL, 0, 0, 0 },
{ CONSTANT_CODE_CALL_TYPE, CODE, "CALL-TYPE", 9, 0x5059542D4C4C4191ULL, 0, 0, 0 },
{ CONSTANT_CODE_CALL_KEY, CODE, "CALL-KEY", 8, 0x59454B2D4C4C414BULL, 0, 0, 0 },
{ CONSTANT_CODE_CALL_FUNCTION, CODE, "CALL-FUNCTION", 13, 0x4E55467B9B959593ULL, 0, 0, 0 },
{ CONSTANT_CODE_CALL_SETF, CODE, "CALL-SETF", 9, 0x5445532D4C4C4192ULL, 0, 0, 0 },
{ CONSTANT_CODE_CALL_LEXICAL, CODE, "CALL-LEXICAL", 12, 0x58454C2D988D8498ULL, 0, 0, 0 },
{ CONSTANT_CODE_VALUES_NIL, CODE, "VALUES-NIL", 10, 0x4E2D5345554C8DA9ULL, 0, 0, 0 },
{ CONSTANT_CODE_VALUES_SET, CODE, "VALUES-SET", 10, 0x532D5345554C95A5ULL, 0, 0, 0 },
{ CONSTANT_CODE_THE_SET, CODE, "THE-SET", 7, 0x005445532D45485BULL, 0, 0, 0 },
{ CONSTANT_CODE_THE_PUSH, CODE, "THE-PUSH", 8, 0x485355502D45485CULL, 0, 0, 0 },
{ CONSTANT_CODE_IF_UNBOUND, CODE, "IF-UNBOUND", 10, 0x554F424E552D8AA1ULL, 0, 0, 0 },
{ CONSTANT_CODE_IF_NIL, CODE, "IF-NIL", 6, 0x00004C494E2D464FULL, 0, 0, 0 },
{ CONSTANT_CODE_IF_T, CODE, "IF-T", 4, 0x00000000542D464DULL, 0, 0, 0 },
{ CONSTANT_CODE_GOTO, CODE, "GOTO", 4, 0x000000004F544F4BULL, 0, 0, 0 },
{ CONSTANT_CODE_GO, CODE, "GO", 2, 0x0000000000004F49ULL, 0, 0, 0 },
{ CONSTANT_CODE_RETURN_FROM, CODE, "RETURN-FROM", 11, 0x462D4E5255A194AFULL, 0, 0, 0 },
{ CONSTANT_CODE_CATCH, CODE, "CATCH", 5, 0x0000004843544148ULL, 0, 0, 0 },
{ CONSTANT_CODE_THROW, CODE, "THROW", 5, 0x000000574F524859ULL, 0, 0, 0 },
{ CONSTANT_CODE_TAG, CODE, "TAG", 3, 0x0000000000474157ULL, 0, 0, 0 },
{ CONSTANT_CODE_TAGINFO, CODE, "TAGINFO", 7, 0x004F464E4947415BULL, 0, 0, 0 },
{ CONSTANT_CODE_BLOCKINFO, CODE, "BLOCKINFO", 9, 0x464E494B434F4C9AULL, 0, 0, 0 },
{ CONSTANT_CODE_HANDLER_BIND, CODE, "HANDLER-BIND", 12, 0x2D52454C889C8A96ULL, 0, 0, 0 },
{ CONSTANT_CODE_HANDLER_CASE, CODE, "HANDLER-CASE", 12, 0x2D52454C89A18297ULL, 0, 0, 0 },
{ CONSTANT_CODE_RESTART_BIND, CODE, "RESTART-BIND", 12, 0x2D54524198A18EA0ULL, 0, 0, 0 },
{ CONSTANT_CODE_RESTART_CASE, CODE, "RESTART-CASE", 12, 0x2D54524199A686A1ULL, 0, 0, 0 },
{ CONSTANT_CODE_RESTART_PROGN, CODE, "RESTART-PROGN", 13, 0x2D54528F9BA297AFULL, 0, 0, 0 },
{ CONSTANT_CODE_FUNCALL, CODE, "FUNCALL", 7, 0x004C4C41434E554DULL, 0, 0, 0 },
{ CONSTANT_CODE_NTH_VALUE, CODE, "NTH-VALUE", 9, 0x554C41562D48549CULL, 0, 0, 0 },
{ CONSTANT_CODE_PROGV, CODE, "PROGV", 5, 0x00000056474F5255ULL, 0, 0, 0 },
{ CONSTANT_CODE_POP, CODE, "POP", 3, 0x0000000000504F53ULL, 0, 0, 0 },
{ CONSTANT_CODE_POP_UNBOUND, CODE, "POP-UNBOUND", 11, 0x4F424E552D949DB0ULL, 0, 0, 0 },
{ CONSTANT_CODE_GETF, CODE, "GETF", 4, 0x000000004654454BULL, 0, 0, 0 },
{ CONSTANT_CODE_REST_COPY, CODE, "REST_COPY", 9, 0x504F435F545345B4ULL, 0, 0, 0 },
{ CONSTANT_CODE_REST_BIND, CODE, "REST_BIND", 9, 0x4E49425F5453459FULL, 0, 0, 0 },
{ CONSTANT_CODE_ALLOW_OTHER_KEYS, CODE, "ALLOW-OTHER-KEYS", 16, 0x27A872A27C9E9199ULL, 0, 0, 0 },
{ CONSTANT_CODE_REST_NULL, CODE, "REST-NULL", 9, 0x4C554E2D545345A7ULL, 0, 0, 0 },
{ CONSTANT_CODE_WHOLE, CODE, "WHOLE", 5, 0x000000454C4F485CULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA, CODE, "LAMBDA", 6, 0x00004144424D4152ULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_NAME, CODE, "LAMBDA-NAME", 11, 0x4E2D414442928E98ULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_TYPE, CODE, "LAMBDA-TYPE", 11, 0x542D4144429291B0ULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_DOC, CODE, "LAMBDA-DOC", 10, 0x442D4144424D84A5ULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_FORM, CODE, "LAMBDA-FORM", 11, 0x462D4144429A93A6ULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_DEFUN, CODE, "LAMBDA-DEFUN", 12, 0x442D414490A2879DULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_CLOSURE, CODE, "LAMBDA-CLOSURE", 14, 0x432D869697A090A6ULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_LEXICAL, CODE, "LAMBDA-LEXICAL", 14, 0x4C2D8D858596999FULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_CACHE, CODE, "LAMBDA-CACHE", 12, 0x432D414487958499ULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_CACHE_SET, CODE, "LAMBDA-CACHE-SET", 16, 0x177294718795849DULL, 0, 0, 0 },
{ CONSTANT_CODE_MACRO, CODE, "MACRO", 5, 0x0000004F52434152ULL, 0, 0, 0 },
{ CONSTANT_CODE_MACRO_SPECIAL, CODE, "MACRO-SPECIAL", 13, 0x50532D9B938C849FULL, 0, 0, 0 },
{ CONSTANT_CODE_MACRO_ENV, CODE, "MACRO-ENV", 9, 0x4E452D4F524341ACULL, 0, 0, 0 },
{ CONSTANT_CODE_MACRO_WHOLE, CODE, "MACRO-WHOLE", 11, 0x48572D4F52888DA7ULL, 0, 0, 0 },
{ CONSTANT_CODE_LABELS_MAKE, CODE, "LABELS-MAKE", 11, 0x4D2D534C45878C98ULL, 0, 0, 0 },
{ CONSTANT_CODE_LABELS_LAMBDA, CODE, "LABELS-LAMBDA", 13, 0x4C2D538D89848E9AULL, 0, 0, 0 },
{ CONSTANT_CODE_BIND1_TYPE, CODE, "BIND1-TYPE", 10, 0x59542D31444E8E9CULL, 0, 0, 0 },
{ CONSTANT_CODE_BIND1_SPECIAL, CODE, "BIND1-SPECIAL", 13, 0x50532D7D85978C94ULL, 0, 0, 0 },
{ CONSTANT_CODE_BIND1_LEXICAL, CODE, "BIND1-LEXICAL", 13, 0x454C2D7D859192A7ULL, 0, 0, 0 },
{ CONSTANT_CODE_BIND2_TYPE, CODE, "BIND2-TYPE", 10, 0x59542D32444E8E9CULL, 0, 0, 0 },
{ CONSTANT_CODE_BIND2_SPECIAL, CODE, "BIND2-SPECIAL", 13, 0x50532D7E85978C94ULL, 0, 0, 0 },
{ CONSTANT_CODE_BIND2_LEXICAL, CODE, "BIND2-LEXICAL", 13, 0x454C2D7E859192A7ULL, 0, 0, 0 },
{ CONSTANT_CODE_LOAD_ALLOC, CODE, "LOAD-ALLOC", 10, 0x4C4C412D444192A5ULL, 0, 0, 0 },
{ CONSTANT_CODE_LOAD_GENSYM, CODE, "LOAD-GENSYM", 11, 0x4E45472D448EA8AAULL, 0, 0, 0 },
{ CONSTANT_CODE_LOAD_SET, CODE, "LOAD-SET", 8, 0x5445532D44414F54ULL, 0, 0, 0 },
{ CONSTANT_CODE_REFERENCE_SET, CODE, "REFERENCE-SET", 13, 0x434E45A68A9972A4ULL, 0, 0, 0 },
{ CONSTANT_CODE_REFERENCE_PUSH, CODE, "REFERENCE-PUSH", 14, 0x434E8DA59A9672A5ULL, 0, 0, 0 },
{ CONSTANT_CODE_STEP, CODE, "STEP", 4, 0x0000000050455457ULL, 0, 0, 0 },
{ CONSTANT_CODE_STEP_OFF, CODE, "STEP-OFF", 8, 0x46464F2D5045545BULL, 0, 0, 0 },
{ CONSTANT_CODE_STEP_BEGIN, CODE, "STEP-BEGIN", 10, 0x4745422D5045A2A6ULL, 0, 0, 0 },
{ CONSTANT_CODE_STEP_END, CODE, "STEP-END", 8, 0x444E452D5045545BULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_RESULT_TYPE, CODE, "OPTCODE-RESULT-TYPE", 19, 0x0172989B98ECE60DULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CAR0_SET, CODE, "OPTCODE-CAR0-SET", 16, 0x018A977C73A691A2ULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CAR0_PUSH, CODE, "OPTCODE-CAR0-PUSH", 17, 0x009A947C73A691EBULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CAR1_SET, CODE, "OPTCODE-CAR1-SET", 16, 0x018A977C74A691A2ULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CAR1_PUSH, CODE, "OPTCODE-CAR1-PUSH", 17, 0x009A947C74A691EBULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CDR0_SET, CODE, "OPTCODE-CDR0-SET", 16, 0x018A977C73A694A2ULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CDR0_PUSH, CODE, "OPTCODE-CDR0-PUSH", 17, 0x009A947C73A694EBULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CDR1_SET, CODE, "OPTCODE-CDR1-SET", 16, 0x018A977C74A694A2ULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CDR1_PUSH, CODE, "OPTCODE-CDR1-PUSH", 17, 0x009A947C74A694EBULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CONS, CODE, "OPTCODE-CONS", 12, 0x2D45444F96A29F9EULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METAOBJECT, CLOS, "METAOBJECT", 10, 0x454A424F4154999AULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SPECIALIZER, CLOS, "SPECIALIZER", 11, 0x494C4149439795B8ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_EQL_SPECIALIZER, CLOS, "EQL-SPECIALIZER", 15, 0x439795AD7698929DULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FORWARD_REFERENCED_CLASS, CLOS, "FORWARD-REFERENCED-CLASS", 24, 0x43E5D8DFDFC5D8F5ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FUNCALLABLE_STANDARD_OBJECT, CLOS, "FUNCALLABLE-STANDARD-OBJECT", 27, 0x59CFEFC1B53A262CULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FUNCALLABLE_STANDARD_CLASS, CLOS, "FUNCALLABLE-STANDARD-CLASS", 26, 0x50D9E3C1B4E63639ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_STANDARD_READER_METHOD, CLOS, "STANDARD-READER-METHOD", 22, 0x71A4CAD7D7DAEBE3ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_STANDARD_WRITER_METHOD, CLOS, "STANDARD-WRITER-METHOD", 22, 0x71A4CAE7DFE7F0E3ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_STANDARD_ACCESSOR_METHOD, CLOS, "STANDARD-ACCESSOR-METHOD", 24, 0x57F4DCDDD6D1C2EAULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SLOT_DEFINITION, CLOS, "SLOT-DEFINITION", 15, 0x46939376A8989AABULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DIRECT_SLOT_DEFINITION, CLOS, "DIRECT-SLOT-DEFINITION", 22, 0x1C73E7D6BBFAE1F4ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_EFFECTIVE_SLOT_DEFINITION, CLOS, "EFFECTIVE-SLOT-DEFINITION", 25, 0x69BFFCDBDFE2BA36ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_STANDARD_SLOT_DEFINITION, CLOS, "STANDARD-SLOT-DEFINITION", 24, 0x57E5B7ECE6DBF0DEULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_STANDARD_DIRECT_SLOT_DEFINITION, CLOS, "STANDARD-DIRECT-SLOT-DEFINITION", 31, 0x383A18004923333BULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_STANDARD_EFFECTIVE_SLOT_DEFINITION, CLOS, "STANDARD-EFFECTIVE-SLOT-DEFINITION", 34, 0x044F1D2430FB728BULL, 0, 0, 0 },
{ CONSTANT_COMMON_ALLOCATE_INSTANCE, CLOS, "ALLOCATE-INSTANCE", 17, 0x08A28297A29A95C4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INITIALIZE_INSTANCE, CLOS, "INITIALIZE-INSTANCE", 19, 0x0AA094979DBBD704ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REINITIALIZE_INSTANCE, CLOS, "REINITIALIZE-INSTANCE", 21, 0x14979DBBD6F1D007ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHARED_INITIALIZE, CLOS, "SHARED-INITIALIZE", 17, 0x237690869B9591F7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ENSURE_GENERIC_FUNCTION, CLOS, "ENSURE-GENERIC-FUNCTION", 23, 0x0DA8D7E4FBDBEAF6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_INSTANCE, CLOS, "MAKE-INSTANCE", 13, 0x534E4972889982AEULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLOT_MISSING, CLOS, "SLOT-MISSING", 12, 0x53494D2D9B9D95B2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLOT_UNBOUND, CLOS, "SLOT-UNBOUND", 12, 0x424E552D989DA1AEULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHANGE_CLASS, CLOS, "CHANGE-CLASS", 12, 0x432D4547A194899BULL, 0, 0, 0 },
{ CONSTANT_COMMON_FUNCTION_KEYWORDS, CLOS, "FUNCTION-KEYWORDS", 17, 0x12A198AB9C93A0D7ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_REDEFINED, CLOS, "REDEFINED", 9, 0x454E49464544459FULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_REFERENCED_CLASS, CLOS, "REFERENCED-CLASS", 16, 0x16A1869E887389A7ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ENSURE_CLASS, CLOS, "ENSURE-CLASS", 12, 0x432D4552A8A68F9DULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_ENSURE_CLASS_USING_CLASS, CLOS, "ENSURE-CLASS-USING-CLASS", 24, 0x5FD3DBCBEBD3D6F7ULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_ENSURE_GENERIC_FUNCTION_USING_CLASS, CLOS, "ENSURE-GENERIC-FUNCTION-USING-CLASS", 35, 0x06EC052C4A789198ULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_ENSURE_METHOD, CLOS, "ENSURE-METHOD", 13, 0x4D2D4596A49BA297ULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_SLOT_BOUNDP_USING_CLASS, CLOS, "SLOT-BOUNDP-USING-CLASS", 23, 0x23EBE8C3CDE2BDFFULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_SLOT_EXISTS_P_USING_CLASS, CLOS, "SLOT-EXISTS-P-USING-CLASS", 25, 0x6FEEBEC0AEE9EF5BULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_SLOT_MAKUNBOUND_USING_CLASS, CLOS, "SLOT-MAKUNBOUND-USING-CLASS", 27, 0x44C8C8C9F22E4159ULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_SLOT_VALUE_USING_CLASS, CLOS, "SLOT-VALUE-USING-CLASS", 22, 0x138FF2D3EAC8D4EBULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_FLET_METHOD_P, CLOS, "FLET-METHOD-P", 13, 0x54454D7D81899B9BULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FLET_NEXT_METHOD, CLOS, "FLET-NEXT-METHOD", 16, 0x1C949681999279AAULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DEFINE_METHOD_COMBINATION, CLOS, "DEFINE-METHOD-COMBINATION", 25, 0x6BB9C6D3E6D7DC3DULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DEFINE_LONG_METHOD_COMBINATION, CLOS, "DEFINE-LONG-METHOD-COMBINATION", 30, 0x5DC4263A030F194EULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DEFINE_SHORT_METHOD_COMBINATION, CLOS, "DEFINE-SHORT-METHOD-COMBINATION", 31, 0x6A0E31081F1E323CULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_LONG_METHOD_COMBINATION, CLOS, "LONG-METHOD-COMBINATION", 23, 0x16E0EBB9C8D3ECF4ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SHORT_METHOD_COMBINATION, CLOS, "SHORT-METHOD-COMBINATION", 24, 0x60EBB9D5D7ECDA01ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHOD_COMBINATION_INSTANCE, CLOS, "METHOD-COMBINATION-INSTANCE", 27, 0x4DD5D8EBDB092454ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ENSURE_METHOD_COMBINATION_SHORT, CLOS, "ENSURE-METHOD-COMBINATION-SHORT", 31, 0x6C0E19273B381244ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ENSURE_METHOD_COMBINATION_LONG, CLOS, "ENSURE-METHOD-COMBINATION-LONG", 30, 0x6BBA0E2642311243ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_QUALIFIERS_ELT, CLOS, "QUALIFIERS-ELT", 14, 0x45499A95916EA8B1ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_COMBINATION_BINDING, CLOS, "COMBINATION-BINDING", 19, 0x188F978B6FE2ECE8ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_MACRO_MAKE_METHOD, CLOS, "MACRO-MAKE-METHOD", 17, 0x109581949F7086EDULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_MACRO_CALL_METHOD, CLOS, "MACRO-CALL-METHOD", 17, 0x108B81949F708DEEULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_MACRO_METHOD_LAMBDA, CLOS, "MACRO-METHOD-LAMBDA", 19, 0x128E797C96D3CDF6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CLASS_NAME, CLOS, "CLASS-NAME", 10, 0x414E2D535341919AULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_SLOTS, CLOS, "CLASS-SLOTS", 11, 0x4C532D535394A09DULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_DIRECT_SLOTS, CLOS, "CLASS-DIRECT-SLOTS", 18, 0x18908080A784E4FBULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_DEFAULT_INITARGS, CLOS, "CLASS-DEFAULT-INITARGS", 22, 0x138DADEEF1D7E1E8ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_DIRECT_DEFAULT_INITARGS, CLOS, "CLASS-DIRECT-DEFAULT-INITARGS", 29, 0x58D7BB0143232847ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_DIRECT_SUPERCLASSES, CLOS, "CLASS-DIRECT-SUPERCLASSES", 25, 0x5EECD3C1F3C7E446ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_DIRECT_SUBCLASSES, CLOS, "CLASS-DIRECT-SUBCLASSES", 23, 0x0BECC5D3FAC5DDEFULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_FINALIZED_P, CLOS, "CLASS-FINALIZED-P", 17, 0x768A72AD9C8D8DF2ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_PROTOTYPE, CLOS, "CLASS-PROTOTYPE", 15, 0x52957DACA790A0A1ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SLOT_DEFINITION_NAME, CLOS, "SLOT-DEFINITION-NAME", 20, 0x73939376EDE5DBFEULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SLOT_DEFINITION_TYPE, CLOS, "SLOT-DEFINITION-TYPE", 20, 0x73939376EDE8F404ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SLOT_DEFINITION_ALLOCATION, CLOS, "SLOT-DEFINITION-ALLOCATION", 26, 0x3CE7D4B9F7E53546ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SLOT_DEFINITION_INITARGS, CLOS, "SLOT-DEFINITION-INITARGS", 24, 0x46DAE5B7FCE1E8FDULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SLOT_DEFINITION_INITFORM, CLOS, "SLOT-DEFINITION-INITFORM", 24, 0x40E5E2BCFCE1E8FDULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SLOT_DEFINITION_INITFUNCTION, CLOS, "SLOT-DEFINITION-INITFUNCTION", 28, 0x36E1E8BD4B313255ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION_NAME, CLOS, "GENERIC-FUNCTION-NAME", 21, 0x7B9292EBD5DDE8CFULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION_METHODS, CLOS, "GENERIC-FUNCTION-METHODS", 24, 0x4ED6E1EEDCE1E7D2ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION_LAMBDA_LIST, CLOS, "GENERIC-FUNCTION-LAMBDA-LIST", 28, 0x28D3D6E92A313022ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION_ARGUMENT_PRECEDENCE_ORDER, CLOS, "GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER", 42, 0x526F726E6782A9C2ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION_DECLARATIONS, CLOS, "GENERIC-FUNCTION-DECLARATIONS", 29, 0x3CE4D4461A31282BULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION_METHOD_CLASS, CLOS, "GENERIC-FUNCTION-METHOD-CLASS", 29, 0x28D6E2423023341AULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION_METHOD_COMBINATION, CLOS, "GENERIC-FUNCTION-METHOD-COMBINATION", 35, 0x7D1830381F7D8669ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHOD_FUNCTION, CLOS, "METHOD-FUNCTION", 15, 0x467B93989C9793B1ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHOD_GENERIC_FUNCTION, CLOS, "METHOD-GENERIC-FUNCTION", 23, 0x0DA8D6E1EEDCE1FEULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHOD_LAMBDA_LIST, CLOS, "METHOD-LAMBDA-LIST", 18, 0x157971908C96E6F3ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHOD_SPECIALIZERS, CLOS, "METHOD-SPECIALIZERS", 19, 0x2D76909091EADCF5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_METHOD_QUALIFIERS, CLOS, "METHOD-QUALIFIERS", 17, 0x23728D9591A08706ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ACCESSOR_METHOD_SLOT_DEFINITION, CLOS, "ACCESSOR-METHOD-SLOT-DEFINITION", 31, 0x4627361242212B29ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_MAKE_METHOD_LAMBDA, CLOS, "MAKE-METHOD-LAMBDA", 18, 0x16928E79728FD1EBULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FIND_METHOD_COMBINATION, CLOS, "FIND-METHOD-COMBINATION", 23, 0x16E0EBB9C5D3E6EEULL, 0, 1, 0 },
{ CONSTANT_RT_ERROR, RT, "ERROR", 5, 0x000000524F52524AULL, 0, 0, 0 },
{ CONSTANT_RT_PUSH_ENTRIES, RT, "PUSH-ENTRIES", 12, 0x544E452D9B989EAEULL, 0, 0, 0 },
{ CONSTANT_RT_DEFTEST, RT, "DEFTEST", 7, 0x005453455446454BULL, 0, 0, 0 },
{ CONSTANT_RT_DEFTEST_ERROR, RT, "DEFTEST-ERROR", 13, 0x2D545397A3989796ULL, 0, 0, 0 },
{ CONSTANT_RT_DEFTEST_ERROR_, RT, "DEFTEST-ERROR!", 14, 0x2D547497A3989797ULL, 0, 0, 0 },
{ CONSTANT_RT_DO_TESTS, RT, "DO-TESTS", 8, 0x53545345542D4F4CULL, 0, 0, 0 },
{ CONSTANT_RT_REM_ALL_TESTS, RT, "REM-ALL-TESTS", 13, 0x2D4C4C9481A08AB3ULL, 0, 0, 0 },
{ CONSTANT_RT_EQUALRT, RT, "EQUALRT", 7, 0x0054524C4155514CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INPUT, SYSTEM, "INPUT", 5, 0x0000005455504E4EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PROCESS, SYSTEM, "PROCESS", 7, 0x00535345434F5257ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_READTABLE_DOT, SYSTEM, "READTABLE-DOT", 13, 0x4C4241A8938572A4ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DOUBLE_QUOTE_READER, SYSTEM, "DOUBLE-QUOTE-READER", 19, 0x1272977987FBE3F0ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SINGLE_QUOTE_READER, SYSTEM, "SINGLE-QUOTE-READER", 19, 0x127297798CF4DDFFULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PARENSIS_OPEN_READER, SYSTEM, "PARENSIS-OPEN-READER", 20, 0x189B809CDCE7D4D2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PARENSIS_CLOSE_READER, SYSTEM, "PARENSIS-CLOSE-READER", 21, 0x257698F3D9E2C5D7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SEMICOLON_READER, SYSTEM, "SEMICOLON-READER", 16, 0x219193848E9F72B1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BACKQUOTE_READER, SYSTEM, "BACKQUOTE-READER", 16, 0x2694999290956E97ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_COMMA_READER, SYSTEM, "COMMA-READER", 12, 0x45522D419F929390ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SHARP_READER, SYSTEM, "SHARP-READER", 12, 0x45522D50A4868CA0ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DISPATCH_FUNCTION, SYSTEM, "DISPATCH-FUNCTION", 17, 0x178CA8849EA88FD0ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ERROR_DISPATCH, SYSTEM, "ERROR-DISPATCH", 14, 0x49447595A393A2A6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_EQUAL_DISPATCH, SYSTEM, "EQUAL-DISPATCH", 14, 0x4944758F9596A1A6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SHARP_DISPATCH, SYSTEM, "SHARP-DISPATCH", 14, 0x49447593A68298B4ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SINGLE_QUOTE_DISPATCH, SYSTEM, "SINGLE-QUOTE-DISPATCH", 21, 0x247689C1CFF6DA0DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PARENSIS_OPEN_DISPATCH, SYSTEM, "PARENSIS-OPEN-DISPATCH", 22, 0x1C8DC8DFDEE3E0E6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PARENSIS_CLOSE_DISPATCH, SYSTEM, "PARENSIS-CLOSE-DISPATCH", 23, 0x17BEDBF5D5EED7DDULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ASTERISK_DISPATCH, SYSTEM, "ASTERISK-DISPATCH", 17, 0x0EA78AA2989D97C7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_COLON_DISPATCH, SYSTEM, "COLON-DISPATCH", 14, 0x49447591A38D9FA4ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LESS_DISPATCH, SYSTEM, "LESS-DISPATCH", 13, 0x5349447596A786A9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BACKSLASH_DISPATCH, SYSTEM, "BACKSLASH-DISPATCH", 18, 0x27829CA69487B6DFULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_OR_DISPATCH, SYSTEM, "OR-DISPATCH", 11, 0x41505349447595AEULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PLUS_DISPATCH, SYSTEM, "PLUS-DISPATCH", 13, 0x5349447596A98DADULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MINUS_DISPATCH, SYSTEM, "MINUS-DISPATCH", 14, 0x49447596A98F99AEULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DOT_DISPATCH, SYSTEM, "DOT-DISPATCH", 12, 0x505349447597A391ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ARRAY_DISPATCH, SYSTEM, "ARRAY-DISPATCH", 14, 0x4944759C9593A2A2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BINARY_DISPATCH, SYSTEM, "BINARY-DISPATCH", 15, 0x44759CA6829E9C9AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_COMPLEX_DISPATCH, SYSTEM, "COMPLEX-DISPATCH", 16, 0x759B998DA0A09897ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_OCTAL_DISPATCH, SYSTEM, "OCTAL-DISPATCH", 14, 0x4944758F959593B0ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PATHNAME_DISPATCH, SYSTEM, "PATHNAME-DISPATCH", 17, 0x08A1829E9B9D85D6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_RADIX_DISPATCH, SYSTEM, "RADIX-DISPATCH", 14, 0x4944759B9D8591B3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_STRUCTURE_DISPATCH, SYSTEM, "STRUCTURE-DISPATCH", 18, 0x2696A4969E96C9EDULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_HEXADECIMAL_DISPATCH, SYSTEM, "HEXADECIMAL-DISPATCH", 20, 0x19968E88B6E7DAEAULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BACKQUOTE, SYSTEM, "BACKQUOTE", 9, 0x544F55514B434190ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UNBOUND_VALUE, SYSTEM, "UNBOUND-VALUE", 13, 0x2D444E9AA48E8FB8ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INCLUDE, SYSTEM, "INCLUDE", 7, 0x004544554C434E50ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_EXCLUDE, SYSTEM, "EXCLUDE", 7, 0x004544554C43584CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INVALID, SYSTEM, "INVALID", 7, 0x0044494C41564E50ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FALSE, SYSTEM, "FALSE", 5, 0x00000045534C414BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_ATOMIC, SYSTEM, "SUBTYPEP-ATOMIC", 15, 0x508899A6A396968FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_ATOMIC_NOT, SYSTEM, "SUBTYPEP-ATOMIC-NOT", 19, 0x7D8899A6A3EAE5E1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_COMPOUND, SYSTEM, "SUBTYPEP-COMPOUND", 17, 0x1E9A9FA9A19198D5ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_FORCE_NUMBER, SYSTEM, "SUBTYPEP-FORCE-NUMBER", 21, 0x1E7295EEEBD3E8EAULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_NORMAL, SYSTEM, "SUBTYPEP-NORMAL", 15, 0x509191A6A691A38FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TERME_INPUT, SYSTEM, "TERME-INPUT", 11, 0x4E492D454DA69AAFULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_OUTPUT, SYSTEM, "TERME-OUTPUT", 12, 0x554F2D45A1A795B4ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_MOVE, SYSTEM, "TERME-MOVE", 10, 0x4F4D2D454D528AB4ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_CLEAR, SYSTEM, "TERME-CLEAR", 11, 0x4C432D454DA486A4ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_DELETE, SYSTEM, "TERME-DELETE", 12, 0x45442D4592A68AACULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_FONT, SYSTEM, "TERME-FONT", 10, 0x4F462D454D5299ACULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_SIZE, SYSTEM, "TERME-SIZE", 10, 0x49532D454D528AB8ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_SCROLL, SYSTEM, "TERME-SCROLL", 12, 0x43532D45999E94B2ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_BEGIN, SYSTEM, "TERME-BEGIN", 11, 0x45422D454DA08EA6ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_END, SYSTEM, "TERME-END", 9, 0x4E452D454D5245A1ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_ENABLE, SYSTEM, "TERME-ENABLE", 12, 0x4E452D45929E87A1ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_SIGNAL, SYSTEM, "TERME-SIGNAL", 12, 0x49532D45999393A7ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_HANG, SYSTEM, "TERME-HANG", 10, 0x41482D454D528CACULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_CODE, SYSTEM, "TERME-CODE", 10, 0x4F432D454D528AA2ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_UP, SYSTEM, "TERME-UP", 8, 0x50552D454D52455CULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_DOWN, SYSTEM, "TERME-DOWN", 10, 0x4F442D454D5293B5ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_LEFT, SYSTEM, "TERME-LEFT", 10, 0x454C2D454D5299A4ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_RIGHT, SYSTEM, "TERME-RIGHT", 11, 0x49522D454DA68DA6ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_PAGE_UP, SYSTEM, "TERME-PAGE-UP", 13, 0x41502D95A27F8AA8ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_PAGE_DOWN, SYSTEM, "TERME-PAGE-DOWN", 15, 0x419E8494917F8AAAULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_HOME, SYSTEM, "TERME-HOME", 10, 0x4F482D454D528AABULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_INSERT, SYSTEM, "TERME-INSERT", 12, 0x4E492D45A1A48AB3ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_FUNCTION, SYSTEM, "TERME-FUNCTION", 14, 0x55467B9496A688B0ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_ESCAPE, SYSTEM, "TERME-ESCAPE", 12, 0x53452D4592A286A3ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_FP_NAN, SYSTEM, "FP-NAN", 6, 0x00004E414E2D504CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FP_INFINITE, SYSTEM, "FP-INFINITE", 11, 0x4E49464E4972A49AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FP_NORMAL, SYSTEM, "FP-NORMAL", 9, 0x414D524F4E2D509BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FP_SUBNORMAL, SYSTEM, "FP-SUBNORMAL", 12, 0x4F4E42559F6E9DA4ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FP_ZERO, SYSTEM, "FP-ZERO", 7, 0x004F52455A2D504DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PAPER, SYSTEM, "PAPER", 5, 0x0000005245504155ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFINE_SYMBOL_MACRO, SYSTEM, "DEFINE-SYMBOL-MACRO", 19, 0x147A729A98D7E4F3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SYMBOL_MACRO_EXPANDER, SYSTEM, "SYMBOL-MACRO-EXPANDER", 21, 0x1D8591CED6E3EAEAULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFCONSTANT, SYSTEM, "DEFCONSTANT", 11, 0x54534E4F439A9390ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_IN_PACKAGE, SYSTEM, "IN-PACKAGE", 10, 0x414B4341502D939AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SETPLIST, SYSTEM, "SETPLIST", 8, 0x5453494C5054455BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_REMPLIST, SYSTEM, "REMPLIST", 8, 0x5453494C504D455AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MAKE_HASH_ITERATOR, SYSTEM, "MAKE-HASH-ITERATOR", 18, 0x27829A729994C0F6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_NEXT_HASH_ITERATOR, SYSTEM, "NEXT-HASH-ITERATOR", 18, 0x27829A72A8A1C4F7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MAKE_PACKAGE_ITERATOR, SYSTEM, "MAKE-PACKAGE-ITERATOR", 21, 0x089599ACD9E6C3FFULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_NEXT_PACKAGE_ITERATOR, SYSTEM, "NEXT-PACKAGE-ITERATOR", 21, 0x089599ACE8F3C800ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFPACKAGE, SYSTEM, "DEFPACKAGE", 10, 0x414B434150468A95ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DO_SYMBOLS, SYSTEM, "DO-SYMBOLS", 10, 0x4F424D59532DA29AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DO_EXTERNAL_SYMBOLS, SYSTEM, "DO-EXTERNAL-SYMBOLS", 19, 0x1492ADAB72CCDCF4ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DO_ALL_SYMBOLS, SYSTEM, "DO-ALL-SYMBOLS", 14, 0x532D9F98906F9CABULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_GETDOC_VARIABLE, SYSTEM, "GETDOC-VARIABLE", 15, 0x56728F91859D9797ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SETDOC_VARIABLE, SYSTEM, "SETDOC-VARIABLE", 15, 0x56728F91859D97A3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ECASE_ERROR, SYSTEM, "ECASE-ERROR", 11, 0x52452D45539392A2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ETYPECASE_ERROR, SYSTEM, "ETYPECASE-ERROR", 15, 0x53939297A29E8199ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFINE_SETF_EXPANDER, SYSTEM, "DEFINE-SETF-EXPANDER", 20, 0x147D9D93C8D1DDEBULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_END_INPUT_STREAM, SYSTEM, "END-INPUT-STREAM", 16, 0x2291939B81977BA9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MAKE_EXTEND_OUTPUT_STREAM, SYSTEM, "MAKE-EXTEND-OUTPUT-STREAM", 25, 0x65F1ECD0C5BCE44DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PROMPT_FOR, SYSTEM, "PROMPT-FOR", 10, 0x462D54504D4FA4A9ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_PRINT_UNREADABLE_CALL, SYSTEM, "PRINT-UNREADABLE-CALL", 21, 0x13A16FE1DECBDAE4ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_WRITE_DEFAULT, SYSTEM, "WRITE-DEFAULT", 13, 0x45442D99A09E93AAULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SYMBOL_DEFTYPE, SYSTEM, "SYMBOL-DEFTYPE", 14, 0x442D919F9BA19FA6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DELETE_DEFTYPE, SYSTEM, "DELETE-DEFTYPE", 14, 0x442D8AA49EA08B97ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ENSURE_STRUCTURE, SYSTEM, "ENSURE-STRUCTURE", 16, 0x187F9AA698A8A0A9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_STRUCTURE_CONSTRUCTOR, SYSTEM, "STRUCTURE-CONSTRUCTOR", 21, 0x24A9A7E3F3E9C502ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_BIND, SYSTEM, "LOOP-BIND", 9, 0x4E49422D504F4F99ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MAKE_PPRINT_STREAM, SYSTEM, "MAKE-PPRINT-STREAM", 18, 0x17A2A480729FDCE9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PPRINT_GENSYM, SYSTEM, "PPRINT-GENSYM", 13, 0x472D549BA2A59EA2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PPRINT_EXIT, SYSTEM, "PPRINT-EXIT", 11, 0x452D544E49A699B3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PPRINT_POP, SYSTEM, "PPRINT-POP", 10, 0x502D544E4952A0A9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PPRINT_CHECK, SYSTEM, "PPRINT-CHECK", 12, 0x432D544E949595A4ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PPRINT_CLOSE, SYSTEM, "PPRINT-CLOSE", 12, 0x432D544E8EA59FA8ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PPRINT_PRETTY, SYSTEM, "PPRINT-PRETTY", 13, 0x502D54A79DA695AFULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TIMEINFO, SYSTEM, "TIMEINFO", 8, 0x4F464E49454D495CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TRACE_ADD, SYSTEM, "TRACE-ADD", 9, 0x44412D45434152A1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TRACE_DEL, SYSTEM, "TRACE-DEL", 9, 0x45442D45434152A9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_WITH_COMPILATION_UNIT, SYSTEM, "WITH-COMPILATION-UNIT", 21, 0x1B9E8CD5D2EEE7E9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SET_SLOTS, SYSTEM, "SET-SLOTS", 9, 0x544F4C532D5445AFULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INTERN_EQL_SPECIALIZER, SYSTEM, "INTERN-EQL-SPECIALIZER", 22, 0x0E70E5E7F2CAE6F1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFGENERIC_DEFINE, SYSTEM, "DEFGENERIC-DEFINE", 17, 0x208E948A8B7388E3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFGENERIC_METHOD, SYSTEM, "DEFGENERIC-METHOD", 17, 0x218DA28A947388E2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CONDITION_RESTARTS_PUSH, SYSTEM, "CONDITION-RESTARTS-PUSH", 23, 0x21D2FBF1D9CDCFFCULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CONDITION_RESTARTS_POP, SYSTEM, "CONDITION-RESTARTS-POP", 22, 0x218AF8EBD9CDCFFBULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CONDITION_RESTARTS_MAKE, SYSTEM, "CONDITION-RESTARTS-MAKE", 23, 0x21CFF3DDD6CDCFFCULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MAKE_RESTART, SYSTEM, "MAKE-RESTART", 12, 0x5345522D999D82ADULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_RESTART_PROGN, SYSTEM, "RESTART-PROGN", 13, 0x2D54528F9BA297AFULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ABORT_LISP, SYSTEM, "ABORT-LISP", 10, 0x494C2D54524F929EULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_HELLO, SYSTEM, "HELLO", 5, 0x0000004F4C4C454DULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_INFOBIT, SYSTEM, "INFOBIT", 7, 0x005449424F464E50ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_INFOPRINT, SYSTEM, "INFOPRINT", 9, 0x4E4952504F464EA6ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_GC, SYSTEM, "GC", 2, 0x0000000000004349ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SAVECORE, SYSTEM, "SAVECORE", 8, 0x45524F434556415BULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_LOADCORE, SYSTEM, "LOADCORE", 8, 0x45524F4344414F54ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_PACKAGE_EXPORT_LIST, SYSTEM, "PACKAGE-EXPORT-LIST", 19, 0x79729B939AE7ECF1ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SPECIALP, SYSTEM, "SPECIALP", 8, 0x504C41494345505BULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_ARRAY_GENERAL_P, SYSTEM, "ARRAY-GENERAL-P", 15, 0x45975AA582A4979EULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_ARRAY_SPECIALIZED_P, SYSTEM, "ARRAY-SPECIALIZED-P", 19, 0x15AD76A582EBC2DDULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SIMPLE_SORT, SYSTEM, "SIMPLE-SORT", 11, 0x532D454C50A19BADULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_BUBBLE_SORT, SYSTEM, "BUBBLE-SORT", 11, 0x532D454C4296A79CULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_QUICK_SORT, SYSTEM, "QUICK-SORT", 10, 0x4F532D4B4349A9ADULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MERGE_SORT, SYSTEM, "MERGE-SORT", 10, 0x4F532D45475299A9ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_EXIT, SYSTEM, "EXIT", 4, 0x0000000054495849ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_QUIT, SYSTEM, "QUIT", 4, 0x0000000054495555ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_CLOSP, SYSTEM, "CLOSP", 5, 0x00000050534F4C48ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_FIXNUMP, SYSTEM, "FIXNUMP", 7, 0x00504D554E58494DULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_BIGNUMP, SYSTEM, "BIGNUMP", 7, 0x00504D554E474949ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_RATIOP, SYSTEM, "RATIOP", 6, 0x0000504F49544158ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SHORT_FLOAT_P, SYSTEM, "SHORT-FLOAT-P", 13, 0x4C462DA47FA389AFULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SINGLE_FLOAT_P, SYSTEM, "SINGLE-FLOAT-P", 14, 0x462D95799B8F98ADULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_DOUBLE_FLOAT_P, SYSTEM, "DOUBLE-FLOAT-P", 14, 0x462D957996969E9EULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_LONG_FLOAT_P, SYSTEM, "LONG-FLOAT-P", 12, 0x4F4C462D977BA399ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_CALLNAMEP, SYSTEM, "CALLNAMEP", 9, 0x454D414E4C4C419CULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_LARGE_NUMBER, SYSTEM, "LARGE-NUMBER", 12, 0x554E2D45999783A5ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_CHARACTER, SYSTEM, "MAKE-CHARACTER", 14, 0x41489572998E82ADULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_FIXNUM, SYSTEM, "MAKE-FIXNUM", 11, 0x5849462D459896A6ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_BIGNUM, SYSTEM, "MAKE-BIGNUM", 11, 0x4749422D459896A6ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_RATIO, SYSTEM, "MAKE-RATIO", 10, 0x5441522D454B90A0ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_COMPLEX, SYSTEM, "MAKE-COMPLEX", 12, 0x4D4F432D9D908DA9ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_EQUAL_RANDOM_STATE, SYSTEM, "EQUAL-RANDOM-STATE", 18, 0x02A680798EA4DAF9ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_EXTEND, SYSTEM, "SUBTYPEP!", 9, 0x504550595442557DULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_NUMBER, SYSTEM, "SUBTYPEP-NUMBER", 15, 0x5097959BA197A38FULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_EASTASIAN_SET, SYSTEM, "EASTASIAN-SET", 13, 0x4149539599A66EA0ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_EASTASIAN_GET, SYSTEM, "EASTASIAN-GET", 13, 0x41495395999A6EA0ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_EASTASIAN_WIDTH, SYSTEM, "EASTASIAN-WIDTH", 15, 0x4191A7859DAA6EA2ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_RUN_PROGRAM, SYSTEM, "RUN-PROGRAM", 11, 0x474F52502D9B96AFULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_CALLNAME, SYSTEM, "MAKE-CALLNAME", 13, 0x4C414372928C8FA6ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_REMOVE_FILE, SYSTEM, "REMOVE-FILE", 11, 0x462D45564F9291A6ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_REMOVE_DIRECTORY, SYSTEM, "REMOVE-DIRECTORY", 16, 0x1D7F94AA929297ABULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_DECLARE_PARSE, SYSTEM, "DECLARE-PARSE", 13, 0x2D4552869F9586A1ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_PARSE_TYPE, SYSTEM, "PARSE-TYPE", 10, 0x59542D45535286AAULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_UPGRADED_OPEN_ELEMENT_TYPE, SYSTEM, "UPGRADED-OPEN-ELEMENT-TYPE", 26, 0x69DE9EE3E5DD3231ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_MEMORY_INPUT_STREAM, SYSTEM, "MAKE-MEMORY-INPUT-STREAM", 24, 0x6FD6E0C8C6F7C108ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_MEMORY_OUTPUT_STREAM, SYSTEM, "MAKE-MEMORY-OUTPUT-STREAM", 25, 0x5EDEF4D0C5D1E857ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_MEMORY_IO_STREAM, SYSTEM, "MAKE-MEMORY-IO-STREAM", 21, 0x20729CC3B3E9E605ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_WITH_INPUT_FROM_MEMORY, SYSTEM, "WITH-INPUT-FROM-MEMORY", 22, 0x7D9BF1D1DDCEE30FULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_WITH_OUTPUT_TO_MEMORY, SYSTEM, "WITH-OUTPUT-TO-MEMORY", 21, 0x21829EDAC7F7EC01ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_GET_OUTPUT_STREAM_MEMORY, SYSTEM, "GET-OUTPUT-STREAM-MEMORY", 24, 0x6AEBF6F0C5CEC701ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MEMORY_STREAM_P, SYSTEM, "MEMORY-STREAM-P", 15, 0x537D869F909297B0ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_BYTE_INTEGER, SYSTEM, "BYTE-INTEGER", 12, 0x544E492D9799A093ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SYSCTL, SYSTEM, "SYSCTL", 6, 0x00004C5443535959ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME, SYSTEM, "TERME", 5, 0x000000454D524559ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_FPCLASSIFY, SYSTEM, "FPCLASSIFY", 10, 0x495353414C43A996ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_PAPER, SYSTEM, "MAKE-PAPER", 10, 0x5041502D454B939CULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_INFO_PAPER, SYSTEM, "INFO-PAPER", 10, 0x5041502D4F46A098ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_ARRAY_PAPER, SYSTEM, "ARRAY-PAPER", 11, 0x41502D5941A4979CULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_BODY_PAPER, SYSTEM, "BODY-PAPER", 10, 0x5041502D5944A191ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_DLFILE, SYSTEM, "DLFILE", 6, 0x0000454C49464C4AULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_DLCALL, SYSTEM, "DLCALL", 6, 0x00004C4C41434C4AULL, 0, 1, 0 },
{ CONSTANT_SPECIAL_BREAK_ON_SIGNALS, COMMON, "*BREAK-ON-SIGNALS*", 18, 0x1B6E99888EA599DDULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_COMPILE_FILE_PATHNAME, COMMON, "*COMPILE-FILE-PATHNAME*", 23, 0x06C6BBE2DAE6D1C2ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_COMPILE_FILE_TRUENAME, COMMON, "*COMPILE-FILE-TRUENAME*", 23, 0x17CABBE2DAE6CEC3ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_COMPILE_PRINT, COMMON, "*COMPILE-PRINT*", 15, 0x45769D9E96A19366ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_COMPILE_VERBOSE, COMMON, "*COMPILE-VERBOSE*", 17, 0x0A9F98929F949992ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_DEBUG_IO, COMMON, "*DEBUG-IO*", 10, 0x492D475542456E83ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_DEBUGGER_HOOK, COMMON, "*DEBUGGER-HOOK*", 15, 0x457192A4918D718BULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_DEFAULT_PATHNAME_DEFAULTS, COMMON, "*DEFAULT-PATHNAME-DEFAULTS*", 27, 0x6DE2E4CFDFF5150BULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_ERROR_OUTPUT, COMMON, "*ERROR-OUTPUT*", 14, 0x4F2D7CA3A7A2998DULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_FEATURES, COMMON, "*FEATURES*", 10, 0x4552555441457087ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_GENSYM_COUNTER, COMMON, "*GENSYM-COUNTER*", 16, 0x579F9EA79C9A967DULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_LOAD_PATHNAME, COMMON, "*LOAD-PATHNAME*", 15, 0x417A7291829D948DULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_LOAD_PRINT, COMMON, "*LOAD-PRINT*", 12, 0x52502D446BA39A7FULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_LOAD_TRUENAME, COMMON, "*LOAD-TRUENAME*", 15, 0x527E7291829D918EULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_LOAD_VERBOSE, COMMON, "*LOAD-VERBOSE*", 14, 0x45565789949E8E8AULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_MACROEXPAND_HOOK, COMMON, "*MACROEXPAND-HOOK*", 18, 0x2794977F878FB8D7ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_MODULES, COMMON, "*MODULES*", 9, 0x53454C55444F4D5DULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PACKAGE, COMMON, "*PACKAGE*", 9, 0x4547414B4341505DULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_ARRAY, COMMON, "*PRINT-ARRAY*", 13, 0x412D5478A293A289ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_BASE, COMMON, "*PRINT-BASE*", 12, 0x422D544E7397A377ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_CASE, COMMON, "*PRINT-CASE*", 12, 0x432D544E7397A377ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_CIRCLE, COMMON, "*PRINT-CIRCLE*", 14, 0x432D7E939595A281ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_ESCAPE, COMMON, "*PRINT-ESCAPE*", 14, 0x452D7E939993938BULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_GENSYM, COMMON, "*PRINT-GENSYM*", 14, 0x472D7E9BA2A59E7DULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_LENGTH, COMMON, "*PRINT-LENGTH*", 14, 0x4C2D7E969D999E7DULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_LEVEL, COMMON, "*PRINT-LEVEL*", 13, 0x4C2D54789597A67CULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_LINES, COMMON, "*PRINT-LINES*", 13, 0x4C2D54789C979E80ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_MISER_WIDTH, COMMON, "*PRINT-MISER-WIDTH*", 19, 0x1176AB7B9BC1EBDAULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_PPRINT_DISPATCH, COMMON, "*PRINT-PPRINT-DISPATCH*", 23, 0x199BC9E5EBDCF2E4ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_PRETTY, COMMON, "*PRINT-PRETTY*", 14, 0x502D7EA79DA6958AULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_RADIX, COMMON, "*PRINT-RADIX*", 13, 0x522D5478A19B9478ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_READABLY, COMMON, "*PRINT-READABLY*", 16, 0x7C86A0908A96917FULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_RIGHT_MARGIN, COMMON, "*PRINT-RIGHT-MARGIN*", 20, 0x246EA17BC7E8E0CEULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_QUERY_IO, COMMON, "*QUERY-IO*", 10, 0x492D595245557B83ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_RANDOM_STATE, COMMON, "*RANDOM-STATE*", 14, 0x2D4D7989A282A68BULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_READTABLE, COMMON, "*READTABLE*", 11, 0x42415444416F9781ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_READ_BASE, COMMON, "*READ-BASE*", 11, 0x41422D44416F9788ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_READ_DEFAULT_FLOAT_FORMAT, COMMON, "*READ-DEFAULT-FLOAT-FORMAT*", 27, 0x5EDCA9DEBB19291BULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_READ_EVAL, COMMON, "*READ-EVAL*", 11, 0x56452D44416F9E76ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_READ_SUPPRESS, COMMON, "*READ-SUPPRESS*", 15, 0x557D80978697A289ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_STANDARD_INPUT, COMMON, "*STANDARD-INPUT*", 16, 0x7C95999E8F9D807EULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_STANDARD_OUTPUT, COMMON, "*STANDARD-OUTPUT*", 17, 0x269694A296A380A9ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_TERMINAL_IO, COMMON, "*TERMINAL-IO*", 13, 0x414E4977A18E8183ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_TRACE_OUTPUT, COMMON, "*TRACE-OUTPUT*", 14, 0x4F2D6F9796A2A88DULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_ENABLE_DEBUGGER, SYSTEM, "*ENABLE-DEBUGGER*", 17, 0x7F8A938996908AA9ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_INDEX_DEBUGGER, SYSTEM, "*INDEX-DEBUGGER*", 16, 0x6E7F9D8C8BA38B7FULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_PARSE_ENVIRONMENT, SYSTEM, "*PARSE-ENVIRONMENT*", 19, 0x0A7A93A2A4B4FAD9ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_PARSE_DECLARE, SYSTEM, "*PARSE-DECLARE*", 15, 0x44578AA5938D937EULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_SCOPE, SYSTEM, "*SCOPE*", 7, 0x002A45504F435331ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_SCOPE_GLOBAL, SYSTEM, "*SCOPE-GLOBAL*", 14, 0x472D6F9C9085A284ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_TOPLEVEL, SYSTEM, "*EVAL-TOPLEVEL*", 15, 0x4F7E7991979B9189ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_COMPILE_TIME, SYSTEM, "*EVAL-COMPILE-TIME*", 19, 0x18975A918DC9DAD7ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_COMPILE_TOPLEVEL, SYSTEM, "*EVAL-COMPILE-TOPLEVEL*", 23, 0x1EC1A6D6E3E4E1DEULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_LOAD_TOPLEVEL, SYSTEM, "*EVAL-LOAD-TOPLEVEL*", 20, 0x14987D9BBFCFCED5ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_EXECUTE, SYSTEM, "*EVAL-EXECUTE*", 14, 0x5845579195AB887DULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_SPECIAL_ENVIRONMENT, SYSTEM, "*ENVIRONMENT*", 13, 0x4E4F5273AA9C8A84ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_SPECIAL_ARGUMENTS, SYSTEM, "*ARGUMENTS*", 11, 0x4E454D55477C9489ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOAD_SIZE, SYSTEM, "*LOAD-SIZE*", 11, 0x49532D444179918FULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOAD_ARRAY, SYSTEM, "*LOAD-ARRAY*", 12, 0x52412D446BA88D88ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOAD_TABLE, SYSTEM, "*LOAD-TABLE*", 12, 0x41542D446B949878ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOAD_GENSYM, SYSTEM, "*LOAD-GENSYM*", 13, 0x45472D6E8EA89F85ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOAD_DEPEND, SYSTEM, "*LOAD-DEPEND*", 13, 0x45442D6E859D9187ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOAD_PUSH, SYSTEM, "*LOAD-PUSH*", 11, 0x55502D4441799488ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DEPEND_LOOP, SYSTEM, "*DEPEND-LOOP*", 13, 0x2D444E6FA0949383ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DEPEND_PASS, SYSTEM, "*DEPEND-PASS*", 13, 0x2D444E6FA3988587ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DEPEND_ERROR, SYSTEM, "*DEPEND-ERROR*", 14, 0x2D4478979F97967DULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DEPEND_ROOT, SYSTEM, "*DEPEND-ROOT*", 13, 0x2D444E6FA4949389ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_GCHOLD, SYSTEM, "*GCHOLD*", 8, 0x2A444C4F48434732ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_NAMED, SYSTEM, "*LOOP-NAMED*", 12, 0x414E2D5079939183ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_VARS, SYSTEM, "*LOOP-VARS*", 11, 0x41562D504F799F87ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_INIT, SYSTEM, "*LOOP-INIT*", 11, 0x4E492D504F79A07EULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FINAL, SYSTEM, "*LOOP-FINAL*", 12, 0x49462D50799B8D84ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FORM, SYSTEM, "*LOOP-FORM*", 11, 0x4F462D504F799987ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_LET, SYSTEM, "*LOOP-LET*", 10, 0x454C2D504F4F7688ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_ENABLE_COMPILER_MACRO, SYSTEM, "*ENABLE-COMPILER-MACRO*", 23, 0x7FB4E7DDD4DCE1B1ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOAD_LOGICAL_PATHNAME_TRANSLATIONS, SYSTEM, "*LOAD-LOGICAL-PATHNAME-TRANSLATIONS*", 36, 0x2E1DE12A41826D8AULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EXTERNAL_FORMAT, SYSTEM, "*EXTERNAL-FORMAT*", 17, 0x158F9F97A39E72B1ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_END_OF_LINE, SYSTEM, "*END-OF-LINE*", 13, 0x2D464F57899C8E83ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_PRINT_WRITE, SYSTEM, "*PRINT-WRITE*", 13, 0x572D54788EA69989ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DEFAULT_PRINT_DISPATCH, SYSTEM, "*DEFAULT-PRINT-DISPATCH*", 24, 0x42C1ECE3D0E7E7B8ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EMPTY_PRINT_DISPATCH, SYSTEM, "*EMPTY-PRINT-DISPATCH*", 22, 0x2376C7C9E7EFCFE2ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DELAY_WARNING_LIST, SYSTEM, "*DELAY-WARNING-LIST*", 20, 0x235AA08FBFE7E9C8ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DELAY_WARNING_SWITCH, SYSTEM, "*DELAY-WARNING-SWITCH*", 22, 0x2A5ACAD7D8E7DFD8ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_MODULE_PROVIDER_FUNCTIONS, SYSTEM, "*MODULE-PROVIDER-FUNCTIONS*", 27, 0x4ED3E4E1E91E3910ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_COMPILE_OUTPUT, SYSTEM, "*COMPILE-OUTPUT*", 16, 0x6FA09EA0A1A49267ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_COMPILE_CODE, SYSTEM, "*COMPILE-CODE*", 14, 0x454C7395919E8665ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_ENCODE_UNIVERSAL_1970, SYSTEM, "*ENCODE-UNIVERSAL-1970*", 23, 0x6EC2C6CBD2C8C0E2ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_ED_FUNCTION, SYSTEM, "*ED-FUNCTION*", 13, 0x434E55707B938E8BULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_ED_TEMPFILE, SYSTEM, "*ED-TEMPFILE*", 13, 0x504D457E72908E7DULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_ED_PROGRAM, SYSTEM, "*ED-PROGRAM*", 12, 0x474F525057918688ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_TRACE_LIST, SYSTEM, "*TRACE-LIST*", 12, 0x4C2D45436BA6A77FULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_TRACE_DEPTH, SYSTEM, "*TRACE-DEPTH*", 13, 0x442D456D89A6A47CULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DRIBBLE_FILE, SYSTEM, "*DRIBBLE-FILE*", 14, 0x454C6C87959B8A65ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DRIBBLE_INPUT, SYSTEM, "*DRIBBLE-INPUT*", 15, 0x4576969799A08D66ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DRIBBLE_OUTPUT, SYSTEM, "*DRIBBLE-OUTPUT*", 16, 0x6FA097929DA79367ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DRIBBLE_ECHO, SYSTEM, "*DRIBBLE-ECHO*", 14, 0x454C6C9191958965ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DRIBBLE_BROADCAST, SYSTEM, "*DRIBBLE-BROADCAST*", 19, 0x068F868398CEDABDULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_INSPECTED, SYSTEM, "*INSPECTED*", 11, 0x5443455053788D7AULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_STEP_BEGIN, SYSTEM, "*STEP-BEGIN*", 12, 0x45422D506FA29C7DULL, 1, 0, 0 },
{ CONSTANT_RT_RESULT, RT, "*RESULT*", 8, 0x2A544C5553455232ULL, 1, 0, 0 },
{ CONSTANT_RT_INDEX, RT, "*INDEX*", 7, 0x002A5845444E4931ULL, 1, 0, 0 },
{ CONSTANT_RT_ENTRIES, RT, "*ENTRIES*", 9, 0x53454952544E455DULL, 1, 0, 0 },
{ CONSTANT_RT_ENTRIES_TABLE, RT, "*ENTRIES-TABLE*", 15, 0x536F8E9E968F9966ULL, 1, 0, 0 },
{ CONSTANT_RT_ENTRIES_WARNING, RT, "*ENTRIES-WARNING*", 17, 0x1A9392A0A68F9C92ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_READINFO_SPECIAL, SYSTEM, "*READINFO*", 10, 0x464E494441457C83ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_LOOP_EXIT, SYSTEM, "*EVAL-LOOP-EXIT*", 16, 0x79A076A486839589ULL, 1, 1, 0 },
{ CONSTANT_SYSTEM_PROMPT, SYSTEM, "*PROMPT*", 8, 0x2A54504D4F525032ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_PROMPT_READING, SYSTEM, "*PROMPT-READING*", 16, 0x579B9E969393958CULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_PROMPT_VALUE, SYSTEM, "*PROMPT-VALUE*", 14, 0x2D547A92A49E918EULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_PROMPT_BRIGHT, SYSTEM, "*PROMPT-BRIGHT*", 15, 0x2D7EA495969BA27BULL, 1, 1, 0 },
{ CONSTANT_SYSTEM_PROMPT_COLOR, SYSTEM, "*PROMPT-COLOR*", 14, 0x2D547A9F9E9E9F7BULL, 1, 1, 0 },
{ CONSTANT_SYSTEM_SPECIAL_TERME, SYSTEM, "*TERME*", 7, 0x002A454D52455431ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_SPECIAL_DLFILE, SYSTEM, "*DLFILE*", 8, 0x2A454C49464C4432ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_CORE_INPUT, SYSTEM, "*CORE-INPUT*", 12, 0x4E492D457CA39886ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_CORE_OUTPUT, SYSTEM, "*CORE-OUTPUT*", 13, 0x554F2D6FA6A4938BULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_VALUE, SYSTEM, "*SUBTYPEP!*", 11, 0x45505954427F7485ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_STANDARD_INPUT, SYSTEM, "*STANDARD-INPUT*", 16, 0x7C95999E8F9D807EULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_STANDARD_OUTPUT, SYSTEM, "*STANDARD-OUTPUT*", 17, 0x269694A296A380A9ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_STANDARD_ERROR, SYSTEM, "*STANDARD-ERROR*", 16, 0x7C9393A09399807EULL, 1, 0, 0 },
#else
{ CONSTANT_AMPERSAND_WHOLE, COMMON, "&WHOLE", 6, 0x4F489C78ULL, 0, 0, 0 },
{ CONSTANT_AMPERSAND_OPTIONAL, COMMON, "&OPTIONAL", 9, 0x159E9EC4ULL, 0, 0, 0 },
{ CONSTANT_AMPERSAND_REST, COMMON, "&REST", 5, 0x5345527FULL, 0, 0, 0 },
{ CONSTANT_AMPERSAND_BODY, COMMON, "&BODY", 5, 0x444F4284ULL, 0, 0, 0 },
{ CONSTANT_AMPERSAND_KEY, COMMON, "&KEY", 4, 0x59454B2AULL, 0, 0, 0 },
{ CONSTANT_AMPERSAND_ALLOW, COMMON, "&ALLOW-OTHER-KEYS", 17, 0x47042C5AULL, 0, 0, 0 },
{ CONSTANT_AMPERSAND_AUX, COMMON, "&AUX", 4, 0x5855412AULL, 0, 0, 0 },
{ CONSTANT_AMPERSAND_ENVIRONMENT, COMMON, "&ENVIRONMENT", 12, 0x78EBDCC8ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ABORT, KEYWORD, "ABORT", 5, 0x524F429AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ABSOLUTE, KEYWORD, "ABSOLUTE", 8, 0x14A79795ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ACCESSOR, KEYWORD, "ACCESSOR", 8, 0x1792969CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ADJUSTABLE, KEYWORD, "ADJUSTABLE", 10, 0x178BDDEAULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_AFTER, KEYWORD, "AFTER", 5, 0x45544698ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ALLOCATION, KEYWORD, "ALLOCATION", 10, 0x18A0DBDDULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ALLOW_OTHER_KEYS, KEYWORD, "ALLOW-OTHER-KEYS", 16, 0x2447043BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_AND, KEYWORD, "AND", 3, 0x00444E44ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_APPEND, KEYWORD, "APPEND", 6, 0x45509495ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ARRAY, KEYWORD, "ARRAY", 5, 0x4152529FULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ARGUMENT_PRECEDENCE_ORDER, KEYWORD, "ARGUMENT-PRECEDENCE-ORDER", 25, 0x26B5C306ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ARGUMENTS, KEYWORD, "ARGUMENTS", 9, 0x299597EAULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_AROUND, KEYWORD, "AROUND", 6, 0x554F9695ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_BASE, KEYWORD, "BASE", 4, 0x45534146ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_BEFORE, KEYWORD, "BEFORE", 6, 0x4F468A9AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_BLOCK, KEYWORD, "BLOCK", 5, 0x434F4C92ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CAPITALIZE, KEYWORD, "CAPITALIZE", 10, 0x129CC7FBULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CASE, KEYWORD, "CASE", 4, 0x45534147ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CLASS, KEYWORD, "CLASS", 5, 0x53414C9BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CIRCLE, KEYWORD, "CIRCLE", 6, 0x43528E95ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_COMMON, KEYWORD, "COMMON", 6, 0x4D4D9D98ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_COMMON_LISP_USER, KEYWORD, "COMMON-LISP-USER", 16, 0x19104440ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_COMPILE_TOPLEVEL, KEYWORD, "COMPILE-TOPLEVEL", 16, 0x16284135ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CONC_NAME, KEYWORD, "CONC-NAME", 9, 0x108F9DBEULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CONSTRUCTOR, KEYWORD, "CONSTRUCTOR", 11, 0x16F5F0F6ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_COPIER, KEYWORD, "COPIER", 6, 0x4950A18EULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_COUNT, KEYWORD, "COUNT", 5, 0x4E554F9CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CREATE, KEYWORD, "CREATE", 6, 0x4145979DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CURRENT, KEYWORD, "CURRENT", 7, 0x52A6A38FULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DATUM, KEYWORD, "DATUM", 5, 0x55544196ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DECLARE, KEYWORD, "DECLARE", 7, 0x4C88978CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DEFAULT, KEYWORD, "DEFAULT", 7, 0x419A91A0ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DEFAULT_INITARGS, KEYWORD, "DEFAULT-INITARGS", 16, 0x162B3233ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DEFAULTS, KEYWORD, "DEFAULTS", 8, 0x149A91A1ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DESCRIPTION, KEYWORD, "DESCRIPTION", 11, 0x17F1DDEAULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DEVICE, KEYWORD, "DEVICE", 6, 0x49568A8DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DIRECTION, KEYWORD, "DIRECTION", 9, 0x149B9DDEULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DIRECTORY, KEYWORD, "DIRECTORY", 9, 0x17A19DE9ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DISPLACED_TO, KEYWORD, "DISPLACED-TO", 12, 0x64EAB7E0ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DISPLACED_INDEX_OFFSET, KEYWORD, "DISPLACED-INDEX-OFFSET", 22, 0x647E97C2ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DOCUMENTATION, KEYWORD, "DOCUMENTATION", 13, 0x78DAE92DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_DOWNCASE, KEYWORD, "DOWNCASE", 8, 0x13AA908FULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ELEMENT_TYPE, KEYWORD, "ELEMENT-TYPE", 12, 0x3FE9F3EAULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_END, KEYWORD, "END", 3, 0x00444E48ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_END1, KEYWORD, "END1", 4, 0x31444E49ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_END2, KEYWORD, "END2", 4, 0x32444E49ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ENVIRONMENT, KEYWORD, "ENVIRONMENT", 11, 0x16F8EBE7ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ERROR, KEYWORD, "ERROR", 5, 0x4F52529CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ESCAPE, KEYWORD, "ESCAPE", 6, 0x4143989BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_EXECUTE, KEYWORD, "EXECUTE", 7, 0x438AACA1ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_EXPECTED_TYPE, KEYWORD, "EXPECTED-TYPE", 13, 0x59EF0107ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_EXPORT, KEYWORD, "EXPORT", 6, 0x4F50AC9DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_EXTERNAL, KEYWORD, "EXTERNAL", 8, 0x1195A69FULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_EXTERNAL_FORMAT, KEYWORD, "EXTERNAL-FORMAT", 15, 0x64392E20ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_FILL, KEYWORD, "FILL", 4, 0x4C4C494AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_FILL_POINTER, KEYWORD, "FILL-POINTER", 12, 0x67E0EDCDULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_FROM_END, KEYWORD, "FROM-END", 8, 0x119D977BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_FORMAT_ARGUMENTS, KEYWORD, "FORMAT-ARGUMENTS", 16, 0x2F29392EULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_FORMAT_CONTROL, KEYWORD, "FORMAT-CONTROL", 14, 0x62D43E33ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_FULL, KEYWORD, "FULL", 4, 0x4C4C554AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_GENERIC_FUNCTION, KEYWORD, "GENERIC-FUNCTION", 16, 0x042F2D43ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_GENERIC_FUNCTION_CLASS, KEYWORD, "GENERIC-FUNCTION-CLASS", 22, 0x457BC3C9ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_GENSYM, KEYWORD, "GENSYM", 6, 0x534E92A6ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_HOST, KEYWORD, "HOST", 4, 0x54534F4CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IDENTITY, KEYWORD, "IDENTITY", 8, 0x27998DA5ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IDENTITY_WITH_ONE_ARGUMENT, KEYWORD, "IDENTITY-WITH-ONE-ARGUMENT", 26, 0x61C0E906ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IF_DOES_NOT_EXIST, KEYWORD, "IF-DOES-NOT-EXIST", 17, 0x721E3390ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IF_EXISTS, KEYWORD, "IF-EXISTS", 9, 0x19808FFDULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IMPORT_FROM, KEYWORD, "IMPORT-FROM", 11, 0x15CAF0F8ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INCLUDE, KEYWORD, "INCLUDE", 7, 0x4C8892A5ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INDEX, KEYWORD, "INDEX", 5, 0x45444EA6ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INITARG, KEYWORD, "INITARG", 7, 0x5490A091ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INITFORM, KEYWORD, "INITFORM", 8, 0x219B9D97ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INITIAL_ELEMENT, KEYWORD, "INITIAL-ELEMENT", 15, 0x4F2F2A2BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INITIAL_CONTENTS, KEYWORD, "INITIAL-CONTENTS", 16, 0x29382D2AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INITIAL_OFFSET, KEYWORD, "INITIAL-OFFSET", 14, 0x54DC2A34ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INITIAL_VALUE, KEYWORD, "INITIAL-VALUE", 13, 0x56E1D13AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INHERITED, KEYWORD, "INHERITED", 9, 0x0A9C97E8ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INPUT, KEYWORD, "INPUT", 5, 0x55504EA2ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INSTANCE, KEYWORD, "INSTANCE", 8, 0x19969C92ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INTERACTIVE, KEYWORD, "INTERACTIVE", 11, 0x19DCE5EFULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INTERACTIVE_FUNCTION, KEYWORD, "INTERACTIVE-FUNCTION", 20, 0x587A8492ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INTERN, KEYWORD, "INTERN", 6, 0x45549CA1ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INTERNAL, KEYWORD, "INTERNAL", 8, 0x11959CA3ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_INVERT, KEYWORD, "INVERT", 6, 0x4556A2A1ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IO, KEYWORD, "IO", 2, 0x00004F4BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_JUNK_ALLOWED, KEYWORD, "JUNK-ALLOWED", 12, 0x5BDFEDD2ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_KEY, KEYWORD, "KEY", 3, 0x0059454EULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LAMBDA_LIST, KEYWORD, "LAMBDA-LIST", 11, 0x0ECED5E4ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LENGTH, KEYWORD, "LENGTH", 6, 0x474E8DA6ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LEVEL, KEYWORD, "LEVEL", 5, 0x4556459DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LINEAR, KEYWORD, "LINEAR", 6, 0x454E9B93ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LINE, KEYWORD, "LINE", 4, 0x454E4950ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LINE_RELATIVE, KEYWORD, "LINE-RELATIVE", 13, 0x67DCF00CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LINES, KEYWORD, "LINES", 5, 0x454E49A4ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LOAD_TOPLEVEL, KEYWORD, "LOAD-TOPLEVEL", 13, 0x59E6E91EULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_LOCAL, KEYWORD, "LOCAL", 5, 0x41434F9DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_MANDATORY, KEYWORD, "MANDATORY", 9, 0x169D95F0ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_METACLASS, KEYWORD, "METACLASS", 9, 0x149591ECULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_METHOD, KEYWORD, "METHOD", 6, 0x485489A2ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_METHOD_CLASS, KEYWORD, "METHOD-CLASS", 12, 0x5ED4CAF4ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_METHOD_COMBINATION, KEYWORD, "METHOD-COMBINATION", 18, 0x1E18669AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_MISER, KEYWORD, "MISER", 5, 0x455349A4ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_MISER_WIDTH, KEYWORD, "MISER-WIDTH", 11, 0x0EF2CAEEULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_MOST_SPECIFIC_FIRST, KEYWORD, "MOST-SPECIFIC-FIRST", 19, 0x2C846C65ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_MOST_SPECIFIC_LAST, KEYWORD, "MOST-SPECIFIC-LAST", 18, 0x24366D65ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_NAME, KEYWORD, "NAME", 4, 0x454D4152ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_NAMED, KEYWORD, "NAMED", 5, 0x454D4197ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_NEW_VERSION, KEYWORD, "NEW-VERSION", 11, 0x00F7D9F8ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_NEWEST, KEYWORD, "NEWEST", 6, 0x455799A7ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_NICKNAMES, KEYWORD, "NICKNAMES", 9, 0x10908AF8ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_NO_ERROR, KEYWORD, "NO-ERROR", 8, 0x177CA1A8ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_NOT, KEYWORD, "NOT", 3, 0x00544F51ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OBJECT, KEYWORD, "OBJECT", 6, 0x454A9698ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OPERANDS, KEYWORD, "OPERANDS", 8, 0x25899E98ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OPERATION, KEYWORD, "OPERATION", 9, 0x218EA4E7ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OPERATOR, KEYWORD, "OPERATOR", 8, 0x2494A498ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OR, KEYWORD, "OR", 2, 0x00005251ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ORDER, KEYWORD, "ORDER", 5, 0x454452A6ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OUTPUT, KEYWORD, "OUTPUT", 6, 0x5054A9AAULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OUTPUT_FILE, KEYWORD, "OUTPUT-FILE", 11, 0x16C6F5F8ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OVERRIDE, KEYWORD, "OVERRIDE", 8, 0x17899FA9ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_OVERWRITE, KEYWORD, "OVERWRITE", 9, 0x268EA8F4ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PACKAGE, KEYWORD, "PACKAGE", 7, 0x4B888898ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PATHNAME, KEYWORD, "PATHNAME", 8, 0x0DA182A6ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PER_LINE_PREFIX, KEYWORD, "PER-LINE-PREFIX", 15, 0x384B281EULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PPRINT_DISPATCH, KEYWORD, "PPRINT-DISPATCH", 15, 0x4F183B4AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PREDICATE, KEYWORD, "PREDICATE", 9, 0x188695E7ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PREFIX, KEYWORD, "PREFIX", 6, 0x4645AA9FULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PRESERVE, KEYWORD, "PRESERVE", 8, 0x189BA49DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PRESERVE_WHITESPACE, KEYWORD, "PRESERVE-WHITESPACE", 19, 0x327C846AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PRETTY, KEYWORD, "PRETTY", 6, 0x5445ABAAULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PRINT, KEYWORD, "PRINT", 5, 0x4E4952A9ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PRINT_FUNCTION, KEYWORD, "PRINT-FUNCTION", 14, 0x6CE4114FULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PRINT_OBJECT, KEYWORD, "PRINT-OBJECT", 12, 0x64DBC4FAULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PROBE, KEYWORD, "PROBE", 5, 0x424F529AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_RADIX, KEYWORD, "RADIX", 5, 0x494441AFULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_READ_ONLY, KEYWORD, "READ-ONLY", 9, 0x108F94E1ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_READABLY, KEYWORD, "READABLY", 8, 0x1D8D879BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_READER, KEYWORD, "READER", 6, 0x4441979DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_REHASH_SIZE, KEYWORD, "REHASH-SIZE", 11, 0x14BAE7F9ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_REHASH_THRESHOLD, KEYWORD, "REHASH-THRESHOLD", 16, 0x2D072F45ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_RELATIVE, KEYWORD, "RELATIVE", 8, 0x06A28EAEULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_RENAME, KEYWORD, "RENAME", 6, 0x414E8AA5ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_RENAME_AND_DELETE, KEYWORD, "RENAME-AND-DELETE", 17, 0x1AEE1B88ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_REPORT, KEYWORD, "REPORT", 6, 0x4F5099AAULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_REPORT_FUNCTION, KEYWORD, "REPORT-FUNCTION", 15, 0x6A0F3751ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_REQUIRED, KEYWORD, "REQUIRED", 8, 0x199697A3ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_RIGHT_MARGIN, KEYWORD, "RIGHT-MARGIN", 12, 0x57DDBE04ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SECTION, KEYWORD, "SECTION", 7, 0x549194A3ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SECTION_RELATIVE, KEYWORD, "SECTION-RELATIVE", 16, 0x08342352ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SHADOW, KEYWORD, "SHADOW", 6, 0x44419FA8ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SHADOWING_IMPORT_FROM, KEYWORD, "SHADOWING-IMPORT-FROM", 21, 0x037862C8ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SIZE, KEYWORD, "SIZE", 4, 0x455A4957ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SLOT_NAMES, KEYWORD, "SLOT-NAMES", 10, 0x2190EDCFULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_START, KEYWORD, "START", 5, 0x524154ACULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_START1, KEYWORD, "START1", 6, 0x524185ADULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_START2, KEYWORD, "START2", 6, 0x524186ADULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_STREAM, KEYWORD, "STREAM", 6, 0x4552A19AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SUFFIX, KEYWORD, "SUFFIX", 6, 0x4646ADA2ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SUPERSEDE, KEYWORD, "SUPERSEDE", 9, 0x0995A8F3ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_TEST, KEYWORD, "TEST", 4, 0x54534558ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_TEST_FUNCTION, KEYWORD, "TEST-FUNCTION", 13, 0x71F1E01FULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_TEST_NOT, KEYWORD, "TEST-NOT", 8, 0x28A29389ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_TYPE, KEYWORD, "TYPE", 4, 0x45505958ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_UNSPECIFIC, KEYWORD, "UNSPECIFIC", 10, 0x169CD4EDULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_UP, KEYWORD, "UP", 2, 0x00005057ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_UPCASE, KEYWORD, "UPCASE", 6, 0x414395AEULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_USE, KEYWORD, "USE", 3, 0x00455358ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_VERBOSE, KEYWORD, "VERBOSE", 7, 0x429798ACULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_VERSION, KEYWORD, "VERSION", 7, 0x53A094A6ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_WILD, KEYWORD, "WILD", 4, 0x444C495BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_WILD_INFERIORS, KEYWORD, "WILD-INFERIORS", 14, 0x59E43829ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_WRITER, KEYWORD, "WRITER", 6, 0x5449A4A2ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_ARGS, KEYWORD, "ARGS", 4, 0x53475245ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_CACHE, KEYWORD, "CACHE", 5, 0x4843418DULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_EXIT, KEYWORD, "EXIT", 4, 0x54495849ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IF_ERROR_EXISTS, KEYWORD, "IF-ERROR-EXISTS", 15, 0x6128322AULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IF_INPUT_DOES_NOT_EXIST, KEYWORD, "IF-INPUT-DOES-NOT-EXIST", 23, 0x0AB988CBULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_IF_OUTPUT_EXISTS, KEYWORD, "IF-OUTPUT-EXISTS", 16, 0x50171B4BULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PIPE, KEYWORD, "PIPE", 4, 0x45504954ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_PROGRAM, KEYWORD, "PROGRAM", 7, 0x479C93A9ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_SEARCH, KEYWORD, "SEARCH", 6, 0x52418D9CULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_VALUE, KEYWORD, "VALUE", 5, 0x554C41A0ULL, 0, 0, 0 },
{ CONSTANT_KEYWORD_WAIT, KEYWORD, "WAIT", 4, 0x5449415BULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASTERISK, COMMON, "*", 1, 0x0000002BULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASTERISK2, COMMON, "**", 2, 0x00002A2CULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASTERISK3, COMMON, "***", 3, 0x002A2A2DULL, 0, 0, 0 },
{ CONSTANT_COMMON_PLUS, COMMON, "+", 1, 0x0000002CULL, 0, 0, 0 },
{ CONSTANT_COMMON_PLUS2, COMMON, "++", 2, 0x00002B2DULL, 0, 0, 0 },
{ CONSTANT_COMMON_PLUS3, COMMON, "+++", 3, 0x002B2B2EULL, 0, 0, 0 },
{ CONSTANT_COMMON_MINUS, COMMON, "-", 1, 0x0000002EULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLASH, COMMON, "/", 1, 0x00000030ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLASH2, COMMON, "//", 2, 0x00002F31ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLASH3, COMMON, "///", 3, 0x002F2F32ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ONE_PLUS, COMMON, "1+", 2, 0x00002B33ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ONE_MINUS, COMMON, "1-", 2, 0x00002D33ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBER_EQUAL, COMMON, "=", 1, 0x0000003EULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBER_NOT_EQUAL, COMMON, "/=", 2, 0x00003D31ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBER_LESS, COMMON, "<", 1, 0x0000003DULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBER_GREATER, COMMON, ">", 1, 0x0000003FULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBER_LESS_EQUAL, COMMON, "<=", 2, 0x00003D3EULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBER_GREATER_EQUAL, COMMON, ">=", 2, 0x00003D40ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ABORT, COMMON, "ABORT", 5, 0x524F429AULL, 0, 0, 0 },
{ CONSTANT_COMMON_ABS, COMMON, "ABS", 3, 0x00534244ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ACONS, COMMON, "ACONS", 5, 0x4E4F4399ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ACOS, COMMON, "ACOS", 4, 0x534F4345ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ACOSH, COMMON, "ACOSH", 5, 0x534F438EULL, 0, 0, 0 },
{ CONSTANT_COMMON_ADD_METHOD, COMMON, "ADD-METHOD", 10, 0x7598CDE7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ADJOIN, COMMON, "ADJOIN", 6, 0x4F4A9290ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ADJUST_ARRAY, COMMON, "ADJUST-ARRAY", 12, 0x6FB8EAF2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ADJUSTABLE_ARRAY_P, COMMON, "ADJUSTABLE-ARRAY-P", 18, 0x31FA8071ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ALPHA_CHAR_P, COMMON, "ALPHA-CHAR-P", 12, 0x60C0CBCFULL, 0, 0, 0 },
{ CONSTANT_COMMON_ALPHANUMERICP, COMMON, "ALPHANUMERICP", 13, 0x58EEED24ULL, 0, 0, 0 },
{ CONSTANT_COMMON_AND, COMMON, "AND", 3, 0x00444E44ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ATOM, COMMON, "ATOM", 4, 0x4D4F5445ULL, 0, 0, 0 },
{ CONSTANT_COMMON_APPEND, COMMON, "APPEND", 6, 0x45509495ULL, 0, 0, 0 },
{ CONSTANT_COMMON_APPLY, COMMON, "APPLY", 5, 0x4C50509FULL, 0, 0, 0 },
{ CONSTANT_COMMON_APROPOS, COMMON, "APROPOS", 7, 0x4FA59F98ULL, 0, 0, 0 },
{ CONSTANT_COMMON_APROPOS_LIST, COMMON, "APROPOS-LIST", 12, 0x50F8E8E9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARITHMETIC_ERROR, COMMON, "ARITHMETIC-ERROR", 16, 0x400B3534ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARITHMETIC_ERROR_OPERANDS, COMMON, "ARITHMETIC-ERROR-OPERANDS", 25, 0x49A9C60FULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARITHMETIC_ERROR_OPERATION, COMMON, "ARITHMETIC-ERROR-OPERATION", 26, 0x4EB0140CULL, 0, 0, 0 },
{ CONSTANT_COMMON_AREF, COMMON, "AREF", 4, 0x46455245ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY, COMMON, "ARRAY", 5, 0x4152529FULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_DIMENSION, COMMON, "ARRAY-DIMENSION", 15, 0x5E33143FULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_DIMENSION_LIMIT, COMMON, "ARRAY-DIMENSION-LIMIT", 21, 0x54805DE5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_DIMENSIONS, COMMON, "ARRAY-DIMENSIONS", 16, 0x31331440ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_ELEMENT_TYPE, COMMON, "ARRAY-ELEMENT-TYPE", 18, 0x35313F95ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_HAS_FILL_POINTER_P, COMMON, "ARRAY-HAS-FILL-POINTER-P", 24, 0x408994E5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_DISPLACEMENT, COMMON, "ARRAY-DISPLACEMENT", 18, 0x11306990ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_IN_BOUNDS_P, COMMON, "ARRAY-IN-BOUNDS-P", 17, 0x123E0676ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_RANK, COMMON, "ARRAY-RANK", 10, 0x02A4CAF2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_RANK_LIMIT, COMMON, "ARRAY-RANK-LIMIT", 16, 0x231B1841ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_ROW_MAJOR_INDEX, COMMON, "ARRAY-ROW-MAJOR-INDEX", 21, 0x44884AF1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_TOTAL_SIZE, COMMON, "ARRAY-TOTAL-SIZE", 16, 0x034D0A51ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAY_TOTAL_SIZE_LIMIT, COMMON, "ARRAY-TOTAL-SIZE-LIMIT", 22, 0x5096AACDULL, 0, 0, 0 },
{ CONSTANT_COMMON_ARRAYP, COMMON, "ARRAYP", 6, 0x4152A2A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASH, COMMON, "ASH", 3, 0x00485344ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASIN, COMMON, "ASIN", 4, 0x4E495345ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASINH, COMMON, "ASINH", 5, 0x4E49538EULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASSERT, COMMON, "ASSERT", 6, 0x4553A799ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASSOC, COMMON, "ASSOC", 5, 0x4F535389ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASSOC_IF, COMMON, "ASSOC-IF", 8, 0x159C808CULL, 0, 0, 0 },
{ CONSTANT_COMMON_ASSOC_IF_NOT, COMMON, "ASSOC-IF-NOT", 12, 0x69EBCEBDULL, 0, 0, 0 },
{ CONSTANT_COMMON_ATAN, COMMON, "ATAN", 4, 0x4E415445ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ATANH, COMMON, "ATANH", 5, 0x4E41548EULL, 0, 0, 0 },
{ CONSTANT_COMMON_BASE_CHAR, COMMON, "BASE-CHAR", 9, 0x069B84CAULL, 0, 0, 0 },
{ CONSTANT_COMMON_BASE_STRING, COMMON, "BASE-STRING", 11, 0x17EEE2C3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIGNUM, COMMON, "BIGNUM", 6, 0x4E47969DULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT, COMMON, "BIT", 3, 0x00544945ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_AND, COMMON, "BIT-AND", 7, 0x2D98978AULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_ANDC1, COMMON, "BIT-ANDC1", 9, 0x709897BDULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_ANDC2, COMMON, "BIT-ANDC2", 9, 0x709897BEULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_EQV, COMMON, "BIT-EQV", 7, 0x2DAA9A8EULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_IOR, COMMON, "BIT-IOR", 7, 0x2DA69892ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_NAND, COMMON, "BIT-NAND", 8, 0x71A28A98ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_NOR, COMMON, "BIT-NOR", 7, 0x2DA69897ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_NOT, COMMON, "BIT-NOT", 7, 0x2DA89897ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_ORC1, COMMON, "BIT-ORC1", 8, 0x5E979B99ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_ORC2, COMMON, "BIT-ORC2", 8, 0x5F979B99ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_VECTOR, COMMON, "BIT-VECTOR", 10, 0x0197E0F1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_VECTOR_P, COMMON, "BIT-VECTOR-P", 12, 0x51C4E0F3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BIT_XOR, COMMON, "BIT-XOR", 7, 0x2DA698A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BLOCK, COMMON, "BLOCK", 5, 0x434F4C92ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE, COMMON, "BOOLE", 5, 0x4C4F4F8CULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_1, COMMON, "BOOLE-1", 7, 0x4C807C8EULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_2, COMMON, "BOOLE-2", 7, 0x4C817C8EULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_AND, COMMON, "BOOLE-AND", 9, 0x1A907CD4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_ANDC1, COMMON, "BOOLE-ANDC1", 11, 0x1AC1BFD6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_ANDC2, COMMON, "BOOLE-ANDC2", 11, 0x1AC2BFD6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_C1, COMMON, "BOOLE-C1", 8, 0x7D927C8FULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_C2, COMMON, "BOOLE-C2", 8, 0x7E927C8FULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_CLR, COMMON, "BOOLE-CLR", 9, 0x18927CE2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_EQV, COMMON, "BOOLE-EQV", 9, 0x1D947CE6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_IOR, COMMON, "BOOLE-IOR", 9, 0x1B987CE2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_NAND, COMMON, "BOOLE-NAND", 10, 0x0D9DC0DFULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_NOR, COMMON, "BOOLE-NOR", 9, 0x1B9D7CE2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_ORC1, COMMON, "BOOLE-ORC1", 10, 0x1E9EADD4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_ORC2, COMMON, "BOOLE-ORC2", 10, 0x1E9EAED4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_SET, COMMON, "BOOLE-SET", 9, 0x11A27CE4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLE_XOR, COMMON, "BOOLE-XOR", 9, 0x1BA77CE2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOOLEAN, COMMON, "BOOLEAN", 7, 0x4C9D908EULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOTH_CASE_P, COMMON, "BOTH-CASE-P", 11, 0x1BE5BFBFULL, 0, 0, 0 },
{ CONSTANT_COMMON_BOUNDP, COMMON, "BOUNDP", 6, 0x4E559F8CULL, 0, 0, 0 },
{ CONSTANT_COMMON_BREAK, COMMON, "BREAK", 5, 0x41455292ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BROADCAST_STREAM, COMMON, "BROADCAST-STREAM", 16, 0x3625083CULL, 0, 0, 0 },
{ CONSTANT_COMMON_BROADCAST_STREAM_STREAMS, COMMON, "BROADCAST-STREAM-STREAMS", 24, 0x5BC69CB6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BUILT_IN_CLASS, COMMON, "BUILT-IN-CLASS", 14, 0x5BDF1924ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BUTLAST, COMMON, "BUTLAST", 7, 0x4CA8A88AULL, 0, 0, 0 },
{ CONSTANT_COMMON_BYTE, COMMON, "BYTE", 4, 0x45545946ULL, 0, 0, 0 },
{ CONSTANT_COMMON_BYTE_SIZE, COMMON, "BYTE-SIZE", 9, 0x1F9DACBDULL, 0, 0, 0 },
{ CONSTANT_COMMON_BYTE_POSITION, COMMON, "BYTE-POSITION", 13, 0x67ECFE13ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CALL_ARGUMENTS_LIMIT, COMMON, "CALL-ARGUMENTS-LIMIT", 20, 0x025A7076ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CALL_METHOD, COMMON, "CALL-METHOD", 11, 0x20D5DDC3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CALL_NEXT_METHOD, COMMON, "CALL-NEXT-METHOD", 16, 0x2E2E0528ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAR, COMMON, "CAR", 3, 0x00524146ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDR, COMMON, "CDR", 3, 0x00524446ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAAR, COMMON, "CAAR", 4, 0x52414147ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CADR, COMMON, "CADR", 4, 0x52444147ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDAR, COMMON, "CDAR", 4, 0x52414447ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDDR, COMMON, "CDDR", 4, 0x52444447ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAAAR, COMMON, "CAAAR", 5, 0x4141419AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAADR, COMMON, "CAADR", 5, 0x4441419AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CADAR, COMMON, "CADAR", 5, 0x4144419AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CADDR, COMMON, "CADDR", 5, 0x4444419AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDAAR, COMMON, "CDAAR", 5, 0x4141449AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDADR, COMMON, "CDADR", 5, 0x4441449AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDDAR, COMMON, "CDDAR", 5, 0x4144449AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDDDR, COMMON, "CDDDR", 5, 0x4444449AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAAAAR, COMMON, "CAAAAR", 6, 0x4141938AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAAADR, COMMON, "CAAADR", 6, 0x4141938DULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAADAR, COMMON, "CAADAR", 6, 0x4441938AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CAADDR, COMMON, "CAADDR", 6, 0x4441938DULL, 0, 0, 0 },
{ CONSTANT_COMMON_CADAAR, COMMON, "CADAAR", 6, 0x4144938AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CADADR, COMMON, "CADADR", 6, 0x4144938DULL, 0, 0, 0 },
{ CONSTANT_COMMON_CADDAR, COMMON, "CADDAR", 6, 0x4444938AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CADDDR, COMMON, "CADDDR", 6, 0x4444938DULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDAAAR, COMMON, "CDAAAR", 6, 0x4141968AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDAADR, COMMON, "CDAADR", 6, 0x4141968DULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDADAR, COMMON, "CDADAR", 6, 0x4441968AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDADDR, COMMON, "CDADDR", 6, 0x4441968DULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDDAAR, COMMON, "CDDAAR", 6, 0x4144968AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDDADR, COMMON, "CDDADR", 6, 0x4144968DULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDDDAR, COMMON, "CDDDAR", 6, 0x4444968AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CDDDDR, COMMON, "CDDDDR", 6, 0x4444968DULL, 0, 0, 0 },
{ CONSTANT_COMMON_CASE, COMMON, "CASE", 4, 0x45534147ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CATCH, COMMON, "CATCH", 5, 0x43544190ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CCASE, COMMON, "CCASE", 5, 0x5341438DULL, 0, 0, 0 },
{ CONSTANT_COMMON_CEILING, COMMON, "CEILING", 7, 0x4C909393ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CELL_ERROR, COMMON, "CELL-ERROR", 10, 0x1E9EDCC9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CELL_ERROR_NAME, COMMON, "CELL-ERROR-NAME", 15, 0x6D112A0FULL, 0, 0, 0 },
{ CONSTANT_COMMON_CERROR, COMMON, "CERROR", 6, 0x52529798ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR, COMMON, "CHAR", 4, 0x52414847ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHARACTER, COMMON, "CHARACTER", 9, 0x17958BDFULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHARACTERP, COMMON, "CHARACTERP", 10, 0x1795DBE0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_EQL, COMMON, "CHAR=", 5, 0x52414885ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_NOT_EQL, COMMON, "CHAR/=", 6, 0x52418578ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_LESS, COMMON, "CHAR<", 5, 0x52414884ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_GREATER, COMMON, "CHAR>", 5, 0x52414886ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_LESS_EQUAL, COMMON, "CHAR<=", 6, 0x52418585ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_GREATER_EQUAL, COMMON, "CHAR>=", 6, 0x52418587ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_CODE, COMMON, "CHAR-CODE", 9, 0x16908BBEULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_CODE_LIMIT, COMMON, "CHAR-CODE-LIMIT", 15, 0x60310211ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_DOWNCASE, COMMON, "CHAR-DOWNCASE", 13, 0x7CD1D010ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_EQUAL, COMMON, "CHAR-EQUAL", 10, 0x2792D9BBULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_GREATERP, COMMON, "CHAR-GREATERP", 13, 0x69D8E40EULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_INT, COMMON, "CHAR-INT", 8, 0x268F9178ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_LESSP, COMMON, "CHAR-LESSP", 10, 0x2586E4CDULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_NAME, COMMON, "CHAR-NAME", 9, 0x1F8296BEULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_NOT_EQUAL, COMMON, "CHAR-NOT-EQUAL", 14, 0x7BE227ECULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_NOT_GREATERP, COMMON, "CHAR-NOT-GREATERP", 17, 0x3E28323FULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_NOT_LESSP, COMMON, "CHAR-NOT-LESSP", 14, 0x79D632FEULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHAR_UPCASE, COMMON, "CHAR-UPCASE", 11, 0x15D6F0BCULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHECK_TYPE, COMMON, "CHECK-TYPE", 10, 0x1C99BAE8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CIS, COMMON, "CIS", 3, 0x00534946ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CLASS, COMMON, "CLASS", 5, 0x53414C9BULL, 0, 0, 0 },
{ CONSTANT_COMMON_CLASS_OF, COMMON, "CLASS-OF", 8, 0x1990799EULL, 0, 0, 0 },
{ CONSTANT_COMMON_CLEAR_INPUT, COMMON, "CLEAR-INPUT", 11, 0x0FE2CEF0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CLEAR_OUTPUT, COMMON, "CLEAR-OUTPUT", 12, 0x6AE9C9F5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CLOSE, COMMON, "CLOSE", 5, 0x534F4C8DULL, 0, 0, 0 },
{ CONSTANT_COMMON_CLRHASH, COMMON, "CLRHASH", 7, 0x489A9F8BULL, 0, 0, 0 },
{ CONSTANT_COMMON_CODE_CHAR, COMMON, "CODE-CHAR", 9, 0x068C92CBULL, 0, 0, 0 },
{ CONSTANT_COMMON_COERCE, COMMON, "COERCE", 6, 0x5245948CULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILATION_SPEED, COMMON, "COMPILATION-SPEED", 17, 0x17223B7DULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILE, COMMON, "COMPILE", 7, 0x50929B93ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILED_FUNCTION, COMMON, "COMPILED-FUNCTION", 17, 0x3231365BULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILED_FUNCTION_P, COMMON, "COMPILED-FUNCTION-P", 19, 0x3281635DULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILER_MACRO, COMMON, "COMPILER-MACRO", 14, 0x65D43819ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILER_MACRO_FUNCTION, COMMON, "COMPILER-MACRO-FUNCTION", 23, 0x0092D5C0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPLEMENT, COMMON, "COMPLEMENT", 10, 0x159AE8E7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPLEX, COMMON, "COMPLEX", 7, 0x50A59496ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPLEXP, COMMON, "COMPLEXP", 8, 0x20A59497ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPUTE_APPLICABLE_METHODS, COMMON, "COMPUTE-APPLICABLE-METHODS", 26, 0x289A2411ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPUTE_RESTARTS, COMMON, "COMPUTE-RESTARTS", 16, 0x253A3B3BULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONCATENATE, COMMON, "CONCATENATE", 11, 0x11D8F7D0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONCATENATED_STREAM, COMMON, "CONCATENATED-STREAM", 19, 0x287A8C4AULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONCATENATED_STREAM_STREAMS, COMMON, "CONCATENATED-STREAM-STREAMS", 27, 0x1B202DE6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COND, COMMON, "COND", 4, 0x444E4F47ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONDITION, COMMON, "CONDITION", 9, 0x1397A3E3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONJUGATE, COMMON, "CONJUGATE", 9, 0x1E8F96E6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILE_FILE, COMMON, "COMPILE-FILE", 12, 0x42DEE4DEULL, 0, 0, 0 },
{ CONSTANT_COMMON_COMPILE_FILE_PATHNAME, COMMON, "COMPILE-FILE-PATHNAME", 21, 0x646183A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONS, COMMON, "CONS", 4, 0x534E4F47ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONSP, COMMON, "CONSP", 5, 0x534E4F98ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONSTANTLY, COMMON, "CONSTANTLY", 10, 0x279CE9EDULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONSTANTP, COMMON, "CONSTANTP", 9, 0x279C90F0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONTINUE, COMMON, "CONTINUE", 8, 0x19A39D94ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CONTROL_ERROR, COMMON, "CONTROL-ERROR", 13, 0x50ECF139ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_ALIST, COMMON, "COPY-ALIST", 10, 0x229CE4CDULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_LIST, COMMON, "COPY-LIST", 9, 0x2C999BCDULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_PPRINT_DISPATCH, COMMON, "COPY-PPRINT-DISPATCH", 20, 0x718B8B52ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_READTABLE, COMMON, "COPY-READTABLE", 14, 0x5CD73B0EULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_SEQ, COMMON, "COPY-SEQ", 8, 0x2A95A278ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_STRUCTURE, COMMON, "COPY-STRUCTURE", 14, 0x00F92B25ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_SYMBOL, COMMON, "COPY-SYMBOL", 11, 0x26F5F1BDULL, 0, 0, 0 },
{ CONSTANT_COMMON_COPY_TREE, COMMON, "COPY-TREE", 9, 0x1EA2A3BEULL, 0, 0, 0 },
{ CONSTANT_COMMON_COS, COMMON, "COS", 3, 0x00534F46ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COSH, COMMON, "COSH", 4, 0x48534F47ULL, 0, 0, 0 },
{ CONSTANT_COMMON_COUNT, COMMON, "COUNT", 5, 0x4E554F9CULL, 0, 0, 0 },
{ CONSTANT_COMMON_COUNT_IF, COMMON, "COUNT-IF", 8, 0x149E7C9FULL, 0, 0, 0 },
{ CONSTANT_COMMON_COUNT_IF_NOT, COMMON, "COUNT-IF-NOT", 12, 0x68EDCAD0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CTYPECASE, COMMON, "CTYPECASE", 9, 0x239A97D6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEBUG, COMMON, "DEBUG", 5, 0x55424590ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DECF, COMMON, "DECF", 4, 0x46434548ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DECLARATION, COMMON, "DECLARATION", 11, 0x20D2E6D9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DECLARE, COMMON, "DECLARE", 7, 0x4C88978CULL, 0, 0, 0 },
{ CONSTANT_COMMON_DECLAIM, COMMON, "DECLAIM", 7, 0x4C908E8CULL, 0, 0, 0 },
{ CONSTANT_COMMON_DECODE_FLOAT, COMMON, "DECODE-FLOAT", 12, 0x69B1D9E0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DECODE_UNIVERSAL_TIME, COMMON, "DECODE-UNIVERSAL-TIME", 21, 0x03517BAFULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFCLASS, COMMON, "DEFCLASS", 8, 0x16998698ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFCONSTANT, COMMON, "DEFCONSTANT", 11, 0x17EDE1DFULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFGENERIC, COMMON, "DEFGENERIC", 10, 0x198BD6DCULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFINE_COMPILER_MACRO, COMMON, "DEFINE-COMPILER-MACRO", 21, 0x55595EDEULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFINE_CONDITION, COMMON, "DEFINE-CONDITION", 16, 0x24072245ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFINE_METHOD_COMBINATION, COMMON, "DEFINE-METHOD-COMBINATION", 25, 0x5291A310ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFINE_MODIFY_MACRO, COMMON, "DEFINE-MODIFY-MACRO", 19, 0x1E594E90ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFINE_SETF_EXPANDER, COMMON, "DEFINE-SETF-EXPANDER", 20, 0x5D4F7B7EULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFINE_SYMBOL_MACRO, COMMON, "DEFINE-SYMBOL-MACRO", 19, 0x2D52578DULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFMACRO, COMMON, "DEFMACRO", 8, 0x1C98888DULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFMETHOD, COMMON, "DEFMETHOD", 9, 0x1C8E99D6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFPACKAGE, COMMON, "DEFPACKAGE", 10, 0x1191CDD6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFPARAMETER, COMMON, "DEFPARAMETER", 12, 0x6FCCEBD6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFSETF, COMMON, "DEFSETF", 7, 0x538C9990ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFSTRUCT, COMMON, "DEFSTRUCT", 9, 0x169B97F5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFTYPE, COMMON, "DEFTYPE", 7, 0x548B95A4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFUN, COMMON, "DEFUN", 5, 0x55464597ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEFVAR, COMMON, "DEFVAR", 6, 0x5646978BULL, 0, 0, 0 },
{ CONSTANT_COMMON_DELETE, COMMON, "DELETE", 6, 0x454C8A9EULL, 0, 0, 0 },
{ CONSTANT_COMMON_DELETE_DUPLICATES, COMMON, "DELETE-DUPLICATES", 17, 0x181A1C94ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DELETE_FILE, COMMON, "DELETE-FILE", 11, 0x0BBED6ECULL, 0, 0, 0 },
{ CONSTANT_COMMON_DELETE_IF, COMMON, "DELETE-IF", 9, 0x0E798AE7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DELETE_IF_NOT, COMMON, "DELETE-IF-NOT", 13, 0x5DC7B83FULL, 0, 0, 0 },
{ CONSTANT_COMMON_DELETE_PACKAGE, COMMON, "DELETE-PACKAGE", 14, 0x56C5132EULL, 0, 0, 0 },
{ CONSTANT_COMMON_DENOMINATOR, COMMON, "DENOMINATOR", 11, 0x10EEDDF0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DEPOSIT_FIELD, COMMON, "DEPOSIT-FIELD", 13, 0x48E9D82EULL, 0, 0, 0 },
{ CONSTANT_COMMON_DESCRIBE, COMMON, "DESCRIBE", 8, 0x08958E9EULL, 0, 0, 0 },
{ CONSTANT_COMMON_DESCRIBE_OBJECT, COMMON, "DESCRIBE-OBJECT", 15, 0x532C2117ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DESTRUCTURING_BIND, COMMON, "DESTRUCTURING-BIND", 18, 0x40225E92ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DIGIT_CHAR, COMMON, "DIGIT-CHAR", 10, 0x118AC8E3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DIGIT_CHAR_P, COMMON, "DIGIT-CHAR-P", 12, 0x61B7C8E5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DIRECTORY, COMMON, "DIRECTORY", 9, 0x17A19DE9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DIRECTORY_NAMESTRING, COMMON, "DIRECTORY-NAMESTRING", 20, 0x74915993ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DISASSEMBLE, COMMON, "DISASSEMBLE", 11, 0x0EDDE8E4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DIVISION_BY_ZERO, COMMON, "DIVISION-BY-ZERO", 16, 0x14511A2EULL, 0, 0, 0 },
{ CONSTANT_COMMON_DO, COMMON, "DO", 2, 0x00004F46ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DOA, COMMON, "DO*", 3, 0x002A4F47ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DOCUMENTATION, COMMON, "DOCUMENTATION", 13, 0x78DAE92DULL, 0, 0, 0 },
{ CONSTANT_COMMON_DOLIST, COMMON, "DOLIST", 6, 0x494CA39DULL, 0, 0, 0 },
{ CONSTANT_COMMON_DOTIMES, COMMON, "DOTIMES", 7, 0x49A79498ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DOUBLE_FLOAT, COMMON, "DOUBLE-FLOAT", 12, 0x5CC3E3E8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DOUBLE_FLOAT_EPSILON, COMMON, "DOUBLE-FLOAT-EPSILON", 20, 0x7E637566ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DOUBLE_FLOAT_NEGATIVE_EPSILON, COMMON, "DOUBLE-FLOAT-NEGATIVE-EPSILON", 29, 0x19E3FD4DULL, 0, 0, 0 },
{ CONSTANT_COMMON_DO_ALL_SYMBOLS, COMMON, "DO-ALL-SYMBOLS", 14, 0x639D3C43ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DO_EXTERNAL_SYMBOLS, COMMON, "DO-EXTERNAL-SYMBOLS", 19, 0x075F8A9FULL, 0, 0, 0 },
{ CONSTANT_COMMON_DO_SYMBOLS, COMMON, "DO-SYMBOLS", 10, 0x226FEFF3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DPB, COMMON, "DPB", 3, 0x00425047ULL, 0, 0, 0 },
{ CONSTANT_COMMON_DRIBBLE, COMMON, "DRIBBLE", 7, 0x428E9E8DULL, 0, 0, 0 },
{ CONSTANT_COMMON_DYNAMIC_EXTENT, COMMON, "DYNAMIC-EXTENT", 14, 0x33E64F32ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ECASE, COMMON, "ECASE", 5, 0x5341438FULL, 0, 0, 0 },
{ CONSTANT_COMMON_ECHO_STREAM, COMMON, "ECHO-STREAM", 11, 0x21E9D7C2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ECHO_STREAM_INPUT_STREAM, COMMON, "ECHO-STREAM-INPUT-STREAM", 24, 0x45CE98BEULL, 0, 0, 0 },
{ CONSTANT_COMMON_ECHO_STREAM_OUTPUT_STREAM, COMMON, "ECHO-STREAM-OUTPUT-STREAM", 25, 0x33B0D415ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ED, COMMON, "ED", 2, 0x00004447ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EIGHTH, COMMON, "EIGHTH", 6, 0x4847919FULL, 0, 0, 0 },
{ CONSTANT_COMMON_ELT, COMMON, "ELT", 3, 0x00544C48ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ENCODE_UNIVERSAL_TIME, COMMON, "ENCODE-UNIVERSAL-TIME", 21, 0x035184B0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_END_OF_FILE, COMMON, "END-OF-FILE", 11, 0x73B6E0E8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ENDP, COMMON, "ENDP", 4, 0x50444E49ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ENOUGH_NAMESTRING, COMMON, "ENOUGH-NAMESTRING", 17, 0x450B3679ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ENSURE_DIRECTORIES_EXIST, COMMON, "ENSURE-DIRECTORIES-EXIST", 24, 0x3F98D1E9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EQ, COMMON, "EQ", 2, 0x00005147ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EQL, COMMON, "EQL", 3, 0x004C5148ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EQUAL, COMMON, "EQUAL", 5, 0x41555196ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EQUALP, COMMON, "EQUALP", 6, 0x4155A197ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ERROR, COMMON, "ERROR", 5, 0x4F52529CULL, 0, 0, 0 },
{ CONSTANT_COMMON_ETYPECASE, COMMON, "ETYPECASE", 9, 0x239A97D8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EVAL, COMMON, "EVAL", 4, 0x4C415649ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EVAL_WHEN, COMMON, "EVAL-WHEN", 9, 0x1189ADC9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EVENP, COMMON, "EVENP", 5, 0x4E45569AULL, 0, 0, 0 },
{ CONSTANT_COMMON_EVERY, COMMON, "EVERY", 5, 0x524556A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EXP, COMMON, "EXP", 3, 0x00505848ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EXPORT, COMMON, "EXPORT", 6, 0x4F50AC9DULL, 0, 0, 0 },
{ CONSTANT_COMMON_EXPT, COMMON, "EXPT", 4, 0x54505849ULL, 0, 0, 0 },
{ CONSTANT_COMMON_EXTENDED_CHAR, COMMON, "EXTENDED-CHAR", 13, 0x4AE1E01FULL, 0, 0, 0 },
{ CONSTANT_COMMON_FBOUNDP, COMMON, "FBOUNDP", 7, 0x559F869BULL, 0, 0, 0 },
{ CONSTANT_COMMON_FCEILING, COMMON, "FCEILING", 8, 0x10938C9AULL, 0, 0, 0 },
{ CONSTANT_COMMON_FDEFINITION, COMMON, "FDEFINITION", 11, 0x1ADCE1E3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FFLOOR, COMMON, "FFLOOR", 6, 0x4F4C989BULL, 0, 0, 0 },
{ CONSTANT_COMMON_FMAKUNBOUND, COMMON, "FMAKUNBOUND", 11, 0x1AC7E9FBULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIFTH, COMMON, "FIFTH", 5, 0x54464993ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_AUTHOR, COMMON, "FILE-AUTHOR", 11, 0x19F3D9C6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_ERROR, COMMON, "FILE-ERROR", 10, 0x179EE0CCULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_ERROR_PATHNAME, COMMON, "FILE-ERROR-PATHNAME", 19, 0x36598257ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_LENGTH, COMMON, "FILE-LENGTH", 11, 0x13D9E9C5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_POSITION, COMMON, "FILE-POSITION", 13, 0x67E4EE17ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_NAMESTRING, COMMON, "FILE-NAMESTRING", 15, 0x65293910ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_STREAM, COMMON, "FILE-STREAM", 11, 0x17EDDDC3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_STRING_LENGTH, COMMON, "FILE-STRING-LENGTH", 18, 0x0C36786EULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILE_WRITE_DATE, COMMON, "FILE-WRITE-DATE", 15, 0x53113A17ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILL, COMMON, "FILL", 4, 0x4C4C494AULL, 0, 0, 0 },
{ CONSTANT_COMMON_FILL_POINTER, COMMON, "FILL-POINTER", 12, 0x67E0EDCDULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND, COMMON, "FIND", 4, 0x444E494AULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_ALL_SYMBOLS, COMMON, "FIND-ALL-SYMBOLS", 16, 0x31402CF2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_CLASS, COMMON, "FIND-CLASS", 10, 0x059ADFD0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_IF, COMMON, "FIND-IF", 7, 0x4494927AULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_IF_NOT, COMMON, "FIND-IF-NOT", 11, 0x71E8E1CCULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_METHOD, COMMON, "FIND-METHOD", 11, 0x18D7E5C6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_PACKAGE, COMMON, "FIND-PACKAGE", 12, 0x4CD6DACAULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_RESTART, COMMON, "FIND-RESTART", 12, 0x6BE5DCD3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIND_SYMBOL, COMMON, "FIND-SYMBOL", 11, 0x11F3EBC0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FINISH_OUTPUT, COMMON, "FINISH-OUTPUT", 13, 0x6DCBE64FULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIRST, COMMON, "FIRST", 5, 0x5352499FULL, 0, 0, 0 },
{ CONSTANT_COMMON_FIXNUM, COMMON, "FIXNUM", 6, 0x4E5896A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLET, COMMON, "FLET", 4, 0x54454C4AULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOAT, COMMON, "FLOAT", 5, 0x414F4C9FULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOATP, COMMON, "FLOATP", 6, 0x414F9CA0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOATING_POINT_INEXACT, COMMON, "FLOATING-POINT-INEXACT", 22, 0x5C72D3BCULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOATING_POINT_INVALID_OPERATION, COMMON, "FLOATING-POINT-INVALID-OPERATION", 32, 0x462A6370ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOATING_POINT_OVERFLOW, COMMON, "FLOATING-POINT-OVERFLOW", 23, 0x67C3CECEULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOATING_POINT_UNDERFLOW, COMMON, "FLOATING-POINT-UNDERFLOW", 24, 0x50AECAC1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOAT_RADIX, COMMON, "FLOAT-RADIX", 11, 0x02F9C2E9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOAT_SIGN, COMMON, "FLOAT-SIGN", 10, 0x0AA2C7EBULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOAT_DIGITS, COMMON, "FLOAT-DIGITS", 12, 0x5DE7C2EDULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOAT_PRECISION, COMMON, "FLOAT-PRECISION", 15, 0x67370C37ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FLOOR, COMMON, "FLOOR", 5, 0x4F4F4C9DULL, 0, 0, 0 },
{ CONSTANT_COMMON_FORCE_OUTPUT, COMMON, "FORCE-OUTPUT", 12, 0x6CF6CCEBULL, 0, 0, 0 },
{ CONSTANT_COMMON_FORMAT, COMMON, "FORMAT", 6, 0x4D52A38DULL, 0, 0, 0 },
{ CONSTANT_COMMON_FORMATTER, COMMON, "FORMATTER", 9, 0x12A6A3E2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FOURTH, COMMON, "FOURTH", 6, 0x525597A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FRESH_LINE, COMMON, "FRESH-LINE", 10, 0x1C91C4E6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FROUND, COMMON, "FROUND", 6, 0x554F969AULL, 0, 0, 0 },
{ CONSTANT_COMMON_FUNCALL, COMMON, "FUNCALL", 7, 0x439AA18EULL, 0, 0, 0 },
{ CONSTANT_COMMON_FUNCTION, COMMON, "FUNCTION", 8, 0x119D9EA2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FUNCTION_LAMBDA_EXPRESSION, COMMON, "FUNCTION-LAMBDA-EXPRESSION", 26, 0x27C428FCULL, 0, 0, 0 },
{ CONSTANT_COMMON_FUNCTIONP, COMMON, "FUNCTIONP", 9, 0x119D9EF3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FTYPE, COMMON, "FTYPE", 5, 0x50595490ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FTRUNCATE, COMMON, "FTRUNCATE", 9, 0x299397E2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_GCD, COMMON, "GCD", 3, 0x0044434AULL, 0, 0, 0 },
{ CONSTANT_COMMON_GENERIC_FUNCTION, COMMON, "GENERIC-FUNCTION", 16, 0x042F2D43ULL, 0, 0, 0 },
{ CONSTANT_COMMON_GENSYM, COMMON, "GENSYM", 6, 0x534E92A6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_GENTEMP, COMMON, "GENTEMP", 7, 0x549E9293ULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET, COMMON, "GET", 3, 0x0054454AULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_DECODED_TIME, COMMON, "GET-DECODED-TIME", 16, 0x6F291933ULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_DISPATCH_MACRO_CHARACTER, COMMON, "GET-DISPATCH-MACRO-CHARACTER", 28, 0x5FF114F2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_INTERNAL_REAL_TIME, COMMON, "GET-INTERNAL-REAL-TIME", 22, 0x4983A6BEULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_INTERNAL_RUN_TIME, COMMON, "GET-INTERNAL-RUN-TIME", 21, 0x5A888896ULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_MACRO_CHARACTER, COMMON, "GET-MACRO-CHARACTER", 19, 0x0B6E4B8BULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_OUTPUT_STREAM_STRING, COMMON, "GET-OUTPUT-STREAM-STRING", 24, 0x2DBCB7F6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_SETF_EXPANSION, COMMON, "GET-SETF-EXPANSION", 18, 0x0D546C69ULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_UNIVERSAL_TIME, COMMON, "GET-UNIVERSAL-TIME", 18, 0x0E45588CULL, 0, 0, 0 },
{ CONSTANT_COMMON_GETF, COMMON, "GETF", 4, 0x4654454BULL, 0, 0, 0 },
{ CONSTANT_COMMON_GETHASH, COMMON, "GETHASH", 7, 0x489C988FULL, 0, 0, 0 },
{ CONSTANT_COMMON_GET_PROPERTIES, COMMON, "GET-PROPERTIES", 14, 0x46F83D2FULL, 0, 0, 0 },
{ CONSTANT_COMMON_GO, COMMON, "GO", 2, 0x00004F49ULL, 0, 0, 0 },
{ CONSTANT_COMMON_GRAPHIC_CHAR_P, COMMON, "GRAPHIC-CHAR-P", 14, 0x4FC6340DULL, 0, 0, 0 },
{ CONSTANT_COMMON_HANDLER_BIND, COMMON, "HANDLER-BIND", 12, 0x35EECFE2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_HANDLER_CASE, COMMON, "HANDLER-CASE", 12, 0x36F3C7E3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_HASH_TABLE, COMMON, "HASH-TABLE", 10, 0x0A94DACBULL, 0, 0, 0 },
{ CONSTANT_COMMON_HASH_TABLE_COUNT, COMMON, "HASH-TABLE-COUNT", 16, 0x22103020ULL, 0, 0, 0 },
{ CONSTANT_COMMON_HASH_TABLE_P, COMMON, "HASH-TABLE-P", 12, 0x5AC1DACDULL, 0, 0, 0 },
{ CONSTANT_COMMON_HASH_TABLE_REHASH_SIZE, COMMON, "HASH-TABLE-REHASH-SIZE", 22, 0x795695BEULL, 0, 0, 0 },
{ CONSTANT_COMMON_HASH_TABLE_REHASH_THRESHOLD, COMMON, "HASH-TABLE-REHASH-THRESHOLD", 27, 0x40EEE20AULL, 0, 0, 0 },
{ CONSTANT_COMMON_HASH_TABLE_SIZE, COMMON, "HASH-TABLE-SIZE", 15, 0x5E073519ULL, 0, 0, 0 },
{ CONSTANT_COMMON_HASH_TABLE_TEST, COMMON, "HASH-TABLE-TEST", 15, 0x5F162E15ULL, 0, 0, 0 },
{ CONSTANT_COMMON_HOST_NAMESTRING, COMMON, "HOST-NAMESTRING", 15, 0x74303F12ULL, 0, 0, 0 },
{ CONSTANT_COMMON_IDENTITY, COMMON, "IDENTITY", 8, 0x27998DA5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_IF, COMMON, "IF", 2, 0x0000464BULL, 0, 0, 0 },
{ CONSTANT_COMMON_IGNORABLE, COMMON, "IGNORABLE", 9, 0x1B9088E9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_IGNORE, COMMON, "IGNORE", 6, 0x4F4E8CA1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_IGNORE_ERRORS, COMMON, "IGNORE-ERRORS", 13, 0x66CADF4DULL, 0, 0, 0 },
{ CONSTANT_COMMON_IMAGPART, COMMON, "IMAGPART", 8, 0x1B938EA1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_IMPORT, COMMON, "IMPORT", 6, 0x4F50A1A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INCF, COMMON, "INCF", 4, 0x46434E4DULL, 0, 0, 0 },
{ CONSTANT_COMMON_INLINE, COMMON, "INLINE", 6, 0x494C939DULL, 0, 0, 0 },
{ CONSTANT_COMMON_INPUT_STREAM_P, COMMON, "INPUT-STREAM-P", 14, 0x76E5112AULL, 0, 0, 0 },
{ CONSTANT_COMMON_INSPECT, COMMON, "INSPECT", 7, 0x50A79195ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTEGER, COMMON, "INTEGER", 7, 0x45A69397ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTEGERP, COMMON, "INTEGERP", 8, 0x15A69398ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTEGER_DECODE_FLOAT, COMMON, "INTEGER-DECODE-FLOAT", 20, 0x5C586D78ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTEGER_LENGTH, COMMON, "INTEGER-LENGTH", 14, 0x39F5213EULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTERACTIVE_STREAM_P, COMMON, "INTERACTIVE-STREAM-P", 20, 0x5C5C878CULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTERN, COMMON, "INTERN", 6, 0x45549CA1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTERNAL_TIME_UNITS_PER_SECOND, COMMON, "INTERNAL-TIME-UNITS-PER-SECOND", 30, 0x571D4165ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INTERSECTION, COMMON, "INTERSECTION", 12, 0x56E8EAFBULL, 0, 0, 0 },
{ CONSTANT_COMMON_INVALID_METHOD_ERROR, COMMON, "INVALID-METHOD-ERROR", 20, 0x4E6B7397ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INVOKE_DEBUGGER, COMMON, "INVOKE-DEBUGGER", 15, 0x5B2B1B2FULL, 0, 0, 0 },
{ CONSTANT_COMMON_INVOKE_RESTART, COMMON, "INVOKE-RESTART", 14, 0x62D83B39ULL, 0, 0, 0 },
{ CONSTANT_COMMON_INVOKE_RESTART_INTERACTIVELY, COMMON, "INVOKE-RESTART-INTERACTIVELY", 28, 0x20EB182CULL, 0, 0, 0 },
{ CONSTANT_COMMON_IN_PACKAGE, COMMON, "IN-PACKAGE", 10, 0x1178D6DBULL, 0, 0, 0 },
{ CONSTANT_COMMON_ISQRT, COMMON, "ISQRT", 5, 0x525153A2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_KEYWORD, COMMON, "KEYWORD", 7, 0x579D97A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_KEYWORDP, COMMON, "KEYWORDP", 8, 0x279D97A2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LABELS, COMMON, "LABELS", 6, 0x4542949EULL, 0, 0, 0 },
{ CONSTANT_COMMON_LAMBDA, COMMON, "LAMBDA", 6, 0x424D8296ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LAMBDA_LIST_KEYWORDS, COMMON, "LAMBDA-LIST-KEYWORDS", 20, 0x666C6D87ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LAMBDA_PARAMETERS_LIMIT, COMMON, "LAMBDA-PARAMETERS-LIMIT", 23, 0x7BA19FCDULL, 0, 0, 0 },
{ CONSTANT_COMMON_LAST, COMMON, "LAST", 4, 0x54534150ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LCM, COMMON, "LCM", 3, 0x004D434FULL, 0, 0, 0 },
{ CONSTANT_COMMON_LDB, COMMON, "LDB", 3, 0x0042444FULL, 0, 0, 0 },
{ CONSTANT_COMMON_LDB_TEST, COMMON, "LDB-TEST", 8, 0x019589A8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LDIFF, COMMON, "LDIFF", 5, 0x46494497ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_DOUBLE_FLOAT, COMMON, "LEAST-NEGATIVE-DOUBLE-FLOAT", 27, 0x3EEDBD3BULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_LONG_FLOAT, COMMON, "LEAST-NEGATIVE-LONG-FLOAT", 25, 0x1CA7943FULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT, COMMON, "LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT", 38, 0x1A959F05ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT, COMMON, "LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT", 36, 0x207358DAULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT, COMMON, "LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT", 37, 0x6DAF612AULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT, COMMON, "LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT", 38, 0x14A4A3FEULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_SHORT_FLOAT, COMMON, "LEAST-NEGATIVE-SHORT-FLOAT", 26, 0x58AFE30DULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_NEGATIVE_SINGLE_FLOAT, COMMON, "LEAST-NEGATIVE-SINGLE-FLOAT", 27, 0x4DF2B635ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_DOUBLE_FLOAT, COMMON, "LEAST-POSITIVE-DOUBLE-FLOAT", 27, 0x48EFC547ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_LONG_FLOAT, COMMON, "LEAST-POSITIVE-LONG-FLOAT", 25, 0x26A99C4BULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT, COMMON, "LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT", 38, 0x2497A711ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_NORMALIZED_LONG_FLOAT, COMMON, "LEAST-POSITIVE-NORMALIZED-LONG-FLOAT", 36, 0x2A7560E6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT, COMMON, "LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT", 37, 0x77B16936ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT, COMMON, "LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT", 38, 0x1EA6AC0AULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_SHORT_FLOAT, COMMON, "LEAST-POSITIVE-SHORT-FLOAT", 26, 0x62B1EB19ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LEAST_POSITIVE_SINGLE_FLOAT, COMMON, "LEAST-POSITIVE-SINGLE-FLOAT", 27, 0x57F4BE41ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LENGTH, COMMON, "LENGTH", 6, 0x474E8DA6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LET, COMMON, "LET", 3, 0x0054454FULL, 0, 0, 0 },
{ CONSTANT_COMMON_LETA, COMMON, "LET*", 4, 0x2A544550ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LISP_IMPLEMENTATION_TYPE, COMMON, "LISP-IMPLEMENTATION-TYPE", 24, 0x2CCDD4C8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LISP_IMPLEMENTATION_VERSION, COMMON, "LISP-IMPLEMENTATION-VERSION", 27, 0x3B1E1016ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LIST, COMMON, "LIST", 4, 0x54534950ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LISTEN, COMMON, "LISTEN", 6, 0x54539797ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LISTP, COMMON, "LISTP", 5, 0x545349A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LISTA, COMMON, "LIST*", 5, 0x5453497BULL, 0, 0, 0 },
{ CONSTANT_COMMON_LIST_ALL_PACKAGES, COMMON, "LIST-ALL-PACKAGES", 17, 0x29281C55ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LIST_LENGTH, COMMON, "LIST-LENGTH", 11, 0x22E0E9CBULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOAD, COMMON, "LOAD", 4, 0x44414F50ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOAD_LOGICAL_PATHNAME_TRANSLATIONS, COMMON, "LOAD-LOGICAL-PATHNAME-TRANSLATIONS", 34, 0x6745A06EULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOAD_TIME_VALUE, COMMON, "LOAD-TIME-VALUE", 15, 0x53262619ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOCALLY, COMMON, "LOCALLY", 7, 0x419C9B9FULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOG, COMMON, "LOG", 3, 0x00474F4FULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGAND, COMMON, "LOGAND", 6, 0x414793A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGANDC1, COMMON, "LOGANDC1", 8, 0x728A93A2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGANDC2, COMMON, "LOGANDC2", 8, 0x738A93A2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGBITP, COMMON, "LOGBITP", 7, 0x4297A39CULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGCOUNT, COMMON, "LOGCOUNT", 8, 0x1795A4A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGEQV, COMMON, "LOGEQV", 6, 0x4547A5A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGICAL_PATHNAME, COMMON, "LOGICAL-PATHNAME", 16, 0x0435133DULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGICAL_PATHNAME_TRANSLATIONS, COMMON, "LOGICAL-PATHNAME-TRANSLATIONS", 29, 0x5523046CULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGIOR, COMMON, "LOGIOR", 6, 0x4947A1A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGNAND, COMMON, "LOGNAND", 7, 0x4E8B9D94ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGNOR, COMMON, "LOGNOR", 6, 0x4E47A1A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGNOT, COMMON, "LOGNOT", 6, 0x4E47A3A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGORC1, COMMON, "LOGORC1", 7, 0x4F7892A5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGORC2, COMMON, "LOGORC2", 7, 0x4F7992A5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGTEST, COMMON, "LOGTEST", 7, 0x549BA298ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOGXOR, COMMON, "LOGXOR", 6, 0x5847A1A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LONG_FLOAT, COMMON, "LONG-FLOAT", 10, 0x169AE9C4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LONG_FLOAT_EPSILON, COMMON, "LONG-FLOAT-EPSILON", 18, 0x28118B6BULL, 0, 0, 0 },
{ CONSTANT_COMMON_LONG_FLOAT_NEGATIVE_EPSILON, COMMON, "LONG-FLOAT-NEGATIVE-EPSILON", 27, 0x2FF026F4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LONG_SITE_NAME, COMMON, "LONG-SITE-NAME", 14, 0x5CE61519ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOOP, COMMON, "LOOP", 4, 0x504F4F50ULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOOP_FINISH, COMMON, "LOOP-FINISH", 11, 0x1EE0E8CDULL, 0, 0, 0 },
{ CONSTANT_COMMON_LOWER_CASE_P, COMMON, "LOWER-CASE-P", 12, 0x56C7C1FDULL, 0, 0, 0 },
{ CONSTANT_COMMON_MACHINE_INSTANCE, COMMON, "MACHINE-INSTANCE", 16, 0x0F1F2C30ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MACHINE_TYPE, COMMON, "MACHINE-TYPE", 12, 0x3AD8E8F6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MACHINE_VERSION, COMMON, "MACHINE-VERSION", 15, 0x49292444ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MACRO_FUNCTION, COMMON, "MACRO-FUNCTION", 14, 0x70DE0047ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MACROEXPAND, COMMON, "MACROEXPAND", 11, 0x22DFD4E8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MACROEXPAND_1, COMMON, "MACROEXPAND-1", 13, 0x4FDFD51BULL, 0, 0, 0 },
{ CONSTANT_COMMON_MACROLET, COMMON, "MACROLET", 8, 0x26888DA4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_ARRAY, COMMON, "MAKE-ARRAY", 10, 0x179DDBC5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_BROADCAST_STREAM, COMMON, "MAKE-BROADCAST-STREAM", 21, 0x6A536EC4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_CONDITION, COMMON, "MAKE-CONDITION", 14, 0x5CEF1C1BULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_CONCATENATED_STREAM, COMMON, "MAKE-CONCATENATED-STREAM", 24, 0x3FD779B9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_DISPATCH_MACRO_CHARACTER, COMMON, "MAKE-DISPATCH-MACRO-CHARACTER", 29, 0x621AD248ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_ECHO_STREAM, COMMON, "MAKE-ECHO-STREAM", 16, 0x2F22F92BULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_HASH_TABLE, COMMON, "MAKE-HASH-TABLE", 15, 0x5A260313ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_INSTANCES_OBSOLETE, COMMON, "MAKE-INSTANCES-OBSOLETE", 23, 0x77A9C6B1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_LIST, COMMON, "MAKE-LIST", 9, 0x18948DD7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_LOAD_FORM, COMMON, "MAKE-LOAD-FORM", 14, 0x55E1081EULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_LOAD_FORM_SAVING_SLOTS, COMMON, "MAKE-LOAD-FORM-SAVING-SLOTS", 27, 0x43FDE002ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_METHOD, COMMON, "MAKE-METHOD", 11, 0x19D4DDCDULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_PACKAGE, COMMON, "MAKE-PACKAGE", 12, 0x4DD3D2D1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_PATHNAME, COMMON, "MAKE-PATHNAME", 13, 0x66CDE014ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_RANDOM_STATE, COMMON, "MAKE-RANDOM-STATE", 17, 0x151B3767ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_SEQUENCE, COMMON, "MAKE-SEQUENCE", 13, 0x59DEDA21ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_STRING, COMMON, "MAKE-STRING", 11, 0x17E6E2CEULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_STRING_INPUT_STREAM, COMMON, "MAKE-STRING-INPUT-STREAM", 24, 0x3BCBA3CAULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_STRING_OUTPUT_STREAM, COMMON, "MAKE-STRING-OUTPUT-STREAM", 25, 0x29ADDF21ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_SYMBOL, COMMON, "MAKE-SYMBOL", 11, 0x12F0E3C7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_SYNONYM_STREAM, COMMON, "MAKE-SYNONYM-STREAM", 19, 0x339F774EULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_TWO_WAY_STREAM, COMMON, "MAKE-TWO-WAY-STREAM", 19, 0x4085812CULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKUNBOUND, COMMON, "MAKUNBOUND", 10, 0x2A9AC7F3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAP, COMMON, "MAP", 3, 0x00504150ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAPC, COMMON, "MAPC", 4, 0x43504151ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAPCAR, COMMON, "MAPCAR", 6, 0x43509394ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAPCAN, COMMON, "MAPCAN", 6, 0x43508F94ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAPHASH, COMMON, "MAPHASH", 7, 0x48989495ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAPL, COMMON, "MAPL", 4, 0x4C504151ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAPLIST, COMMON, "MAPLIST", 7, 0x4CA4949DULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAPCON, COMMON, "MAPCON", 6, 0x43508FA2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAP_INTO, COMMON, "MAP-INTO", 8, 0x7CA48F9EULL, 0, 0, 0 },
{ CONSTANT_COMMON_MASK_FIELD, COMMON, "MASK-FIELD", 10, 0x109CCBD0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAX, COMMON, "MAX", 3, 0x00584150ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MERGE, COMMON, "MERGE", 5, 0x47524597ULL, 0, 0, 0 },
{ CONSTANT_COMMON_METHOD, COMMON, "METHOD", 6, 0x485489A2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_METHOD_COMBINATION_ERROR, COMMON, "METHOD-COMBINATION-ERROR", 24, 0x3594B8F2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MEMBER, COMMON, "MEMBER", 6, 0x424D9798ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MEMBER_IF, COMMON, "MEMBER-IF", 9, 0x0B7A97E1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MEMBER_IF_NOT, COMMON, "MEMBER-IF-NOT", 13, 0x5AC8C539ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MERGE_PATHNAMES, COMMON, "MERGE-PATHNAMES", 15, 0x4A440042ULL, 0, 0, 0 },
{ CONSTANT_COMMON_METHOD_COMBINATION, COMMON, "METHOD-COMBINATION", 18, 0x1E18669AULL, 0, 0, 0 },
{ CONSTANT_COMMON_MIN, COMMON, "MIN", 3, 0x004E4950ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MINUSP, COMMON, "MINUSP", 6, 0x554E99A6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MISMATCH, COMMON, "MISMATCH", 8, 0x15969D96ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOD, COMMON, "MOD", 3, 0x00444F50ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_NEGATIVE_DOUBLE_FLOAT, COMMON, "MOST-NEGATIVE-DOUBLE-FLOAT", 26, 0x55BEFBDDULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_NEGATIVE_FIXNUM, COMMON, "MOST-NEGATIVE-FIXNUM", 20, 0x087D6D6CULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_NEGATIVE_LONG_FLOAT, COMMON, "MOST-NEGATIVE-LONG-FLOAT", 24, 0x5B9CB5B2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_NEGATIVE_SHORT_FLOAT, COMMON, "MOST-NEGATIVE-SHORT-FLOAT", 25, 0x28D8BE02ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_NEGATIVE_SINGLE_FLOAT, COMMON, "MOST-NEGATIVE-SINGLE-FLOAT", 26, 0x4FCE00D6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_POSITIVE_DOUBLE_FLOAT, COMMON, "MOST-POSITIVE-DOUBLE-FLOAT", 26, 0x61C8FDE5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_POSITIVE_FIXNUM, COMMON, "MOST-POSITIVE-FIXNUM", 20, 0x14876F74ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_POSITIVE_LONG_FLOAT, COMMON, "MOST-POSITIVE-LONG-FLOAT", 24, 0x67A6B7BAULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_POSITIVE_SHORT_FLOAT, COMMON, "MOST-POSITIVE-SHORT-FLOAT", 25, 0x34E2C00AULL, 0, 0, 0 },
{ CONSTANT_COMMON_MOST_POSITIVE_SINGLE_FLOAT, COMMON, "MOST-POSITIVE-SINGLE-FLOAT", 26, 0x5BD802DEULL, 0, 0, 0 },
{ CONSTANT_COMMON_MUFFLE_WARNING, COMMON, "MUFFLE-WARNING", 14, 0x66C23436ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MULTIPLE_VALUE_BIND, COMMON, "MULTIPLE-VALUE-BIND", 19, 0x284B8F74ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MULTIPLE_VALUE_CALL, COMMON, "MULTIPLE-VALUE-CALL", 19, 0x29538D6CULL, 0, 0, 0 },
{ CONSTANT_COMMON_MULTIPLE_VALUE_LIST, COMMON, "MULTIPLE-VALUE-LIST", 19, 0x325B9474ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MULTIPLE_VALUE_PROG1, COMMON, "MULTIPLE-VALUE-PROG1", 20, 0x674E907EULL, 0, 0, 0 },
{ CONSTANT_COMMON_MULTIPLE_VALUE_SETQ, COMMON, "MULTIPLE-VALUE-SETQ", 19, 0x39589570ULL, 0, 0, 0 },
{ CONSTANT_COMMON_MULTIPLE_VALUES_LIMIT, COMMON, "MULTIPLE-VALUES-LIMIT", 21, 0x5C7A8ACDULL, 0, 0, 0 },
{ CONSTANT_COMMON_NAME_CHAR, COMMON, "NAME-CHAR", 9, 0x069584D6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NAMESTRING, COMMON, "NAMESTRING", 10, 0x0E9FDCF9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NBUTLAST, COMMON, "NBUTLAST", 8, 0x28A883A2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NCONC, COMMON, "NCONC", 5, 0x4E4F4396ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NEXT_METHOD_P, COMMON, "NEXT-METHOD-P", 13, 0x55E1E220ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NINTH, COMMON, "NINTH", 5, 0x544E499BULL, 0, 0, 0 },
{ CONSTANT_COMMON_NINTERSECTION, COMMON, "NINTERSECTION", 13, 0x68EAF031ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NO_APPLICABLE_METHOD, COMMON, "NO-APPLICABLE-METHOD", 20, 0x6058568EULL, 0, 0, 0 },
{ CONSTANT_COMMON_NO_NEXT_METHOD, COMMON, "NO-NEXT-METHOD", 14, 0x43D6313DULL, 0, 0, 0 },
{ CONSTANT_COMMON_NOT, COMMON, "NOT", 3, 0x00544F51ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NOTANY, COMMON, "NOTANY", 6, 0x4154A8A2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NOTEVERY, COMMON, "NOTEVERY", 8, 0x1EA694ACULL, 0, 0, 0 },
{ CONSTANT_COMMON_NOTINLINE, COMMON, "NOTINLINE", 9, 0x179D9BEAULL, 0, 0, 0 },
{ CONSTANT_COMMON_NRECONC, COMMON, "NRECONC", 7, 0x4388A0A4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NREVERSE, COMMON, "NREVERSE", 8, 0x1B98A49BULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSET_DIFFERENCE, COMMON, "NSET-DIFFERENCE", 15, 0x6026201EULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSET_EXCLUSIVE_OR, COMMON, "NSET-EXCLUSIVE-OR", 17, 0x301E3380ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSTRING_CAPITALIZE, COMMON, "NSTRING-CAPITALIZE", 18, 0x1238699AULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSTRING_DOWNCASE, COMMON, "NSTRING-DOWNCASE", 16, 0x1346322EULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSTRING_UPCASE, COMMON, "NSTRING-UPCASE", 14, 0x40DF374DULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSUBLIS, COMMON, "NSUBLIS", 7, 0x42A89CA1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSUBST, COMMON, "NSUBST", 6, 0x4255A7A7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSUBSTITUTE, COMMON, "NSUBSTITUTE", 11, 0x16E3FC01ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSUBSTITUTE_IF, COMMON, "NSUBSTITUTE-IF", 14, 0x43E4424DULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSUBSTITUTE_IF_NOT, COMMON, "NSUBSTITUTE-IF-NOT", 18, 0x121196A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSUBST_IF, COMMON, "NSUBST-IF", 9, 0x0B82A7F0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NSUBST_IF_NOT, COMMON, "NSUBST-IF-NOT", 13, 0x5AD0D548ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NTH, COMMON, "NTH", 3, 0x00485451ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NTH_VALUE, COMMON, "NTH-VALUE", 9, 0x029495F2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NTHCDR, COMMON, "NTHCDR", 6, 0x4348A698ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NULL, COMMON, "NULL", 4, 0x4C4C5552ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBER, COMMON, "NUMBER", 6, 0x424DA799ULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMBERP, COMMON, "NUMBERP", 7, 0x429DA79AULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUMERATOR, COMMON, "NUMERATOR", 9, 0x14A196FBULL, 0, 0, 0 },
{ CONSTANT_COMMON_NUNION, COMMON, "NUNION", 6, 0x494EA3A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ODDP, COMMON, "ODDP", 4, 0x50444453ULL, 0, 0, 0 },
{ CONSTANT_COMMON_OTHERWISE, COMMON, "OTHERWISE", 9, 0x1891ABEFULL, 0, 0, 0 },
{ CONSTANT_COMMON_OPEN, COMMON, "OPEN", 4, 0x4E455053ULL, 0, 0, 0 },
{ CONSTANT_COMMON_OPEN_STREAM_P, COMMON, "OPEN-STREAM-P", 13, 0x4DE6E51EULL, 0, 0, 0 },
{ CONSTANT_COMMON_OPTIMIZE, COMMON, "OPTIMIZE", 8, 0x0EAE99A4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_OR, COMMON, "OR", 2, 0x00005251ULL, 0, 0, 0 },
{ CONSTANT_COMMON_OUTPUT_STREAM_P, COMMON, "OUTPUT-STREAM-P", 15, 0x65172954ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE, COMMON, "PACKAGE", 7, 0x4B888898ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGEP, COMMON, "PACKAGEP", 8, 0x1B888899ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE_ERROR, COMMON, "PACKAGE-ERROR", 13, 0x47DADB35ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE_ERROR_PACKAGE, COMMON, "PACKAGE-ERROR-PACKAGE", 21, 0x506C53C5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE_NAME, COMMON, "PACKAGE-NAME", 12, 0x3DD5C9EBULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE_NICKNAMES, COMMON, "PACKAGE-NICKNAMES", 17, 0x09191391ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE_SHADOWING_SYMBOLS, COMMON, "PACKAGE-SHADOWING-SYMBOLS", 25, 0x30B59833ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE_USE_LIST, COMMON, "PACKAGE-USE-LIST", 16, 0x7A212542ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PACKAGE_USED_BY_LIST, COMMON, "PACKAGE-USED-BY-LIST", 20, 0x3E7A6773ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PAIRLIS, COMMON, "PAIRLIS", 7, 0x529C8AA3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PARSE_ERROR, COMMON, "PARSE-ERROR", 11, 0x25E9BDF2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PARSE_INTEGER, COMMON, "PARSE-INTEGER", 13, 0x66E2B448ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PARSE_NAMESTRING, COMMON, "PARSE-NAMESTRING", 16, 0x3041FD44ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME, COMMON, "PATHNAME", 8, 0x0DA182A6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME_HOST, COMMON, "PATHNAME-HOST", 13, 0x60F0CB2CULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME_DEVICE, COMMON, "PATHNAME-DEVICE", 15, 0x642C0A23ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME_DIRECTORY, COMMON, "PATHNAME-DIRECTORY", 18, 0x2F3F6374ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME_NAME, COMMON, "PATHNAME-NAME", 13, 0x5AE2D11DULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME_MATCH_P, COMMON, "PATHNAME-MATCH-P", 16, 0x3210181EULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME_TYPE, COMMON, "PATHNAME-TYPE", 13, 0x5DFAD71DULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAME_VERSION, COMMON, "PATHNAME-VERSION", 16, 0x2E36222EULL, 0, 0, 0 },
{ CONSTANT_COMMON_PATHNAMEP, COMMON, "PATHNAMEP", 9, 0x0DA182F7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PEEK_CHAR, COMMON, "PEEK-CHAR", 9, 0x0C8D88D8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PHASE, COMMON, "PHASE", 5, 0x5341489AULL, 0, 0, 0 },
{ CONSTANT_COMMON_PI, COMMON, "PI", 2, 0x00004952ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PLUSP, COMMON, "PLUSP", 5, 0x53554CA5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_POP, COMMON, "POP", 3, 0x00504F53ULL, 0, 0, 0 },
{ CONSTANT_COMMON_POSITION, COMMON, "POSITION", 8, 0x17A298ACULL, 0, 0, 0 },
{ CONSTANT_COMMON_POSITION_IF, COMMON, "POSITION-IF", 11, 0x17E8E1DCULL, 0, 0, 0 },
{ CONSTANT_COMMON_POSITION_IF_NOT, COMMON, "POSITION-IF-NOT", 15, 0x453D312EULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT, COMMON, "PPRINT", 6, 0x4952A4A4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_DISPATCH, COMMON, "PPRINT-DISPATCH", 15, 0x4F183B4AULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_EXIT_IF_LIST_EXHAUSTED, COMMON, "PPRINT-EXIT-IF-LIST-EXHAUSTED", 29, 0x3BF23383ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_FILL, COMMON, "PPRINT-FILL", 11, 0x0FCBF0F2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_INDENT, COMMON, "PPRINT-INDENT", 13, 0x60C4E94DULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_LINEAR, COMMON, "PPRINT-LINEAR", 13, 0x56C4F346ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_LOGICAL_BLOCK, COMMON, "PPRINT-LOGICAL-BLOCK", 20, 0x6639878EULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_NEWLINE, COMMON, "PPRINT-NEWLINE", 14, 0x60CC413FULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_POP, COMMON, "PPRINT-POP", 10, 0x197FF4F7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_TAB, COMMON, "PPRINT-TAB", 10, 0x1D7FE6E9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PPRINT_TABULAR, COMMON, "PPRINT-TABULAR", 14, 0x69D5392EULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRIN1, COMMON, "PRIN1", 5, 0x4E495286ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRIN1_TO_STRING, COMMON, "PRIN1-TO-STRING", 15, 0x70392106ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRINC, COMMON, "PRINC", 5, 0x4E495298ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRINC_TO_STRING, COMMON, "PRINC-TO-STRING", 15, 0x70392118ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRINT, COMMON, "PRINT", 5, 0x4E4952A9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRINT_NOT_READABLE, COMMON, "PRINT-NOT-READABLE", 18, 0x252B3697ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRINT_NOT_READABLE_OBJECT, COMMON, "PRINT-NOT-READABLE-OBJECT", 25, 0x379D8134ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRINT_OBJECT, COMMON, "PRINT-OBJECT", 12, 0x64DBC4FAULL, 0, 0, 0 },
{ CONSTANT_COMMON_PRINT_UNREADABLE_OBJECT, COMMON, "PRINT-UNREADABLE-OBJECT", 23, 0x70C299C0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROBE_FILE, COMMON, "PROBE-FILE", 10, 0x0B95C4EBULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROCLAIM, COMMON, "PROCLAIM", 8, 0x109893A4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROG, COMMON, "PROG", 4, 0x474F5254ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROG1, COMMON, "PROG1", 5, 0x474F5286ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROG2, COMMON, "PROG2", 5, 0x474F5287ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROGA, COMMON, "PROG*", 5, 0x474F527FULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROGN, COMMON, "PROGN", 5, 0x474F52A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROGV, COMMON, "PROGV", 5, 0x474F52ABULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROGRAM_ERROR, COMMON, "PROGRAM-ERROR", 13, 0x43EEE646ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PROVIDE, COMMON, "PROVIDE", 7, 0x569496A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PSETF, COMMON, "PSETF", 5, 0x5445539BULL, 0, 0, 0 },
{ CONSTANT_COMMON_PSETQ, COMMON, "PSETQ", 5, 0x544553A6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PUSH, COMMON, "PUSH", 4, 0x48535554ULL, 0, 0, 0 },
{ CONSTANT_COMMON_PUSHNEW, COMMON, "PUSHNEW", 7, 0x48AA9AA5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_QUOTE, COMMON, "QUOTE", 5, 0x544F559BULL, 0, 0, 0 },
{ CONSTANT_COMMON_RANDOM, COMMON, "RANDOM", 6, 0x444E8EA7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RANDOM_STATE, COMMON, "RANDOM-STATE", 12, 0x5CCFD001ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RANDOM_STATE_P, COMMON, "RANDOM-STATE-P", 14, 0x5CD02030ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RASSOC, COMMON, "RASSOC", 6, 0x535384A7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RASSOC_IF, COMMON, "RASSOC-IF", 9, 0x1C8084F0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RASSOC_IF_NOT, COMMON, "RASSOC-IF-NOT", 13, 0x6BCEB248ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RATIO, COMMON, "RATIO", 5, 0x495441A6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RATIONAL, COMMON, "RATIONAL", 8, 0x15958FA9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RATIONALIZE, COMMON, "RATIONALIZE", 11, 0x15DAE9F5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RATIONALP, COMMON, "RATIONALP", 9, 0x15958FFAULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ, COMMON, "READ", 4, 0x44414556ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_BYTE, COMMON, "READ-BYTE", 9, 0x189A87CDULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_CHAR, COMMON, "READ-CHAR", 9, 0x058988DAULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_CHAR_NO_HANG, COMMON, "READ-CHAR-NO-HANG", 17, 0x2318FE56ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_DELIMITED_LIST, COMMON, "READ-DELIMITED-LIST", 19, 0x31516E69ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_FROM_STRING, COMMON, "READ-FROM-STRING", 16, 0x2F35022EULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_LINE, COMMON, "READ-LINE", 9, 0x128A91CDULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_PRESERVING_WHITESPACE, COMMON, "READ-PRESERVING-WHITESPACE", 26, 0x22C70A14ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READ_SEQUENCE, COMMON, "READ-SEQUENCE", 13, 0x58D4DE26ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READER_ERROR, COMMON, "READER-ERROR", 12, 0x5BBDE9F5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READTABLE, COMMON, "READTABLE", 9, 0x108386F4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_READTABLE_CASE, COMMON, "READTABLE-CASE", 14, 0x51C6F94CULL, 0, 0, 0 },
{ CONSTANT_COMMON_READTABLEP, COMMON, "READTABLEP", 10, 0x1083D6F5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REAL, COMMON, "REAL", 4, 0x4C414556ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REALP, COMMON, "REALP", 5, 0x4C4145A7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REALPART, COMMON, "REALPART", 8, 0x209386AAULL, 0, 0, 0 },
{ CONSTANT_COMMON_REDUCE, COMMON, "REDUCE", 6, 0x55448A9BULL, 0, 0, 0 },
{ CONSTANT_COMMON_REM, COMMON, "REM", 3, 0x004D4555ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMF, COMMON, "REMF", 4, 0x464D4556ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMHASH, COMMON, "REMHASH", 7, 0x4895989AULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMOVE, COMMON, "REMOVE", 6, 0x4F4D8AAEULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMOVE_DUPLICATES, COMMON, "REMOVE-DUPLICATES", 17, 0x221B1CA4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMOVE_IF, COMMON, "REMOVE-IF", 9, 0x187A8AF7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMOVE_IF_NOT, COMMON, "REMOVE-IF-NOT", 13, 0x67C8B84FULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMOVE_METHOD, COMMON, "REMOVE-METHOD", 13, 0x6BC2DF3EULL, 0, 0, 0 },
{ CONSTANT_COMMON_REMPROP, COMMON, "REMPROP", 7, 0x509D94ABULL, 0, 0, 0 },
{ CONSTANT_COMMON_RENAME_FILE, COMMON, "RENAME-FILE", 11, 0x07C0D6F3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RENAME_PACKAGE, COMMON, "RENAME-PACKAGE", 14, 0x52C71335ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REPLACE, COMMON, "REPLACE", 7, 0x4C95889AULL, 0, 0, 0 },
{ CONSTANT_COMMON_REQUIRE, COMMON, "REQUIRE", 7, 0x559697A2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REST, COMMON, "REST", 4, 0x54534556ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RESTART, COMMON, "RESTART", 7, 0x54A7979AULL, 0, 0, 0 },
{ CONSTANT_COMMON_RESTART_BIND, COMMON, "RESTART-BIND", 12, 0x45F5E0E1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RESTART_CASE, COMMON, "RESTART-CASE", 12, 0x46FAD8E2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RESTART_NAME, COMMON, "RESTART-NAME", 12, 0x46F4D8EDULL, 0, 0, 0 },
{ CONSTANT_COMMON_RETURN, COMMON, "RETURN", 6, 0x555493AAULL, 0, 0, 0 },
{ CONSTANT_COMMON_RETURN_FROM, COMMON, "RETURN-FROM", 11, 0x1BCEE301ULL, 0, 0, 0 },
{ CONSTANT_COMMON_REVAPPEND, COMMON, "REVAPPEND", 9, 0x0F9B95EFULL, 0, 0, 0 },
{ CONSTANT_COMMON_REVERSE, COMMON, "REVERSE", 7, 0x459B98ABULL, 0, 0, 0 },
{ CONSTANT_COMMON_ROOM, COMMON, "ROOM", 4, 0x4D4F4F56ULL, 0, 0, 0 },
{ CONSTANT_COMMON_ROTATEF, COMMON, "ROTATEF", 7, 0x419A94ADULL, 0, 0, 0 },
{ CONSTANT_COMMON_ROUND, COMMON, "ROUND", 5, 0x4E554F9BULL, 0, 0, 0 },
{ CONSTANT_COMMON_ROW_MAJOR_AREF, COMMON, "ROW-MAJOR-AREF", 14, 0x4EE30444ULL, 0, 0, 0 },
{ CONSTANT_COMMON_RPLACA, COMMON, "RPLACA", 6, 0x414C919BULL, 0, 0, 0 },
{ CONSTANT_COMMON_RPLACD, COMMON, "RPLACD", 6, 0x414C949BULL, 0, 0, 0 },
{ CONSTANT_COMMON_SAFETY, COMMON, "SAFETY", 6, 0x45469AADULL, 0, 0, 0 },
{ CONSTANT_COMMON_SATISFIES, COMMON, "SATISFIES", 9, 0x0E9D8802ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SBIT, COMMON, "SBIT", 4, 0x54494257ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SCALE_FLOAT, COMMON, "SCALE-FLOAT", 11, 0x18DBB1F2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SCHAR, COMMON, "SCHAR", 5, 0x414843AAULL, 0, 0, 0 },
{ CONSTANT_COMMON_SEARCH, COMMON, "SEARCH", 6, 0x52418D9CULL, 0, 0, 0 },
{ CONSTANT_COMMON_SECOND, COMMON, "SECOND", 6, 0x4F4389A7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SEQUENCE, COMMON, "SEQUENCE", 8, 0x1A9493A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SERIOUS_CONDITION, COMMON, "SERIOUS-CONDITION", 17, 0x0A3D3E8DULL, 0, 0, 0 },
{ CONSTANT_COMMON_SET, COMMON, "SET", 3, 0x00544556ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SET_DISPATCH_MACRO_CHARACTER, COMMON, "SET-DISPATCH-MACRO-CHARACTER", 28, 0x5FF114FEULL, 0, 0, 0 },
{ CONSTANT_COMMON_SET_MACRO_CHARACTER, COMMON, "SET-MACRO-CHARACTER", 19, 0x0B6E4B97ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SET_PPRINT_DISPATCH, COMMON, "SET-PPRINT-DISPATCH", 19, 0x7C6C80A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SET_SYNTAX_FROM_CHAR, COMMON, "SET-SYNTAX-FROM-CHAR", 20, 0x475E8E90ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SETF, COMMON, "SETF", 4, 0x46544557ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SETQ, COMMON, "SETQ", 4, 0x51544557ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SET_DIFFERENCE, COMMON, "SET-DIFFERENCE", 14, 0x41E0262DULL, 0, 0, 0 },
{ CONSTANT_COMMON_SET_EXCLUSIVE_OR, COMMON, "SET-EXCLUSIVE-OR", 16, 0x22301E42ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SEVENTH, COMMON, "SEVENTH", 7, 0x459E99A8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHADOW, COMMON, "SHADOW", 6, 0x44419FA8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHADOWING_IMPORT, COMMON, "SHADOWING-IMPORT", 16, 0x34261C49ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHIFTF, COMMON, "SHIFTF", 6, 0x46498EADULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHORT_FLOAT, COMMON, "SHORT-FLOAT", 11, 0x1EE9B701ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHORT_FLOAT_EPSILON, COMMON, "SHORT-FLOAT-EPSILON", 19, 0x158B569AULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHORT_FLOAT_NEGATIVE_EPSILON, COMMON, "SHORT-FLOAT-NEGATIVE-EPSILON", 28, 0x7426D72AULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHORT_SITE_NAME, COMMON, "SHORT-SITE-NAME", 15, 0x6A15084BULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIGNAL, COMMON, "SIGNAL", 6, 0x4E47959AULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIGNED_BYTE, COMMON, "SIGNED-BYTE", 11, 0x10B9E1FCULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIGNUM, COMMON, "SIGNUM", 6, 0x4E4796AEULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_ARRAY, COMMON, "SIMPLE-ARRAY", 12, 0x6ABBE0FDULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_BASE_STRING, COMMON, "SIMPLE-BASE-STRING", 18, 0x09127D93ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_BIT_VECTOR, COMMON, "SIMPLE-BIT-VECTOR", 17, 0x37FC2690ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_BIT_VECTOR_P, COMMON, "SIMPLE-BIT-VECTOR-P", 19, 0x384C5392ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_CONDITION, COMMON, "SIMPLE-CONDITION", 16, 0x2B0E2652ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_CONDITION_FORMAT_CONTROL, COMMON, "SIMPLE-CONDITION-FORMAT-CONTROL", 31, 0x7F4C4C70ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_CONDITION_FORMAT_ARGUMENTS, COMMON, "SIMPLE-CONDITION-FORMAT-ARGUMENTS", 33, 0x544745BEULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_ERROR, COMMON, "SIMPLE-ERROR", 12, 0x67C9E0FDULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_STRING, COMMON, "SIMPLE-STRING", 13, 0x71C3E147ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_STRING_P, COMMON, "SIMPLE-STRING-P", 15, 0x72140E49ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_TYPE_ERROR, COMMON, "SIMPLE-TYPE-ERROR", 17, 0x211231A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_VECTOR, COMMON, "SIMPLE-VECTOR", 13, 0x75CED243ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_VECTOR_P, COMMON, "SIMPLE-VECTOR-P", 15, 0x761EFF45ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIMPLE_WARNING, COMMON, "SIMPLE-WARNING", 14, 0x70C9283CULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIN, COMMON, "SIN", 3, 0x004E4956ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SINH, COMMON, "SINH", 4, 0x484E4957ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SINGLE_FLOAT, COMMON, "SINGLE-FLOAT", 12, 0x61BCDDF7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SINGLE_FLOAT_EPSILON, COMMON, "SINGLE-FLOAT-EPSILON", 20, 0x035C6F75ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SINGLE_FLOAT_NEGATIVE_EPSILON, COMMON, "SINGLE-FLOAT-NEGATIVE-EPSILON", 29, 0x1EDCF75CULL, 0, 0, 0 },
{ CONSTANT_COMMON_SIXTH, COMMON, "SIXTH", 5, 0x545849A0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLEEP, COMMON, "SLEEP", 5, 0x45454CA8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLOT_BOUNDP, COMMON, "SLOT-BOUNDP", 11, 0x29EED2D9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLOT_EXISTS_P, COMMON, "SLOT-EXISTS-P", 13, 0x4AFAE630ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLOT_MAKUNBOUND, COMMON, "SLOT-MAKUNBOUND", 15, 0x6F173639ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLOT_VALUE, COMMON, "SLOT-VALUE", 10, 0x2090E7DFULL, 0, 0, 0 },
{ CONSTANT_COMMON_SOFTWARE_TYPE, COMMON, "SOFTWARE-TYPE", 13, 0x69F1E529ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SOFTWARE_VERSION, COMMON, "SOFTWARE-VERSION", 16, 0x3A2D303AULL, 0, 0, 0 },
{ CONSTANT_COMMON_SOME, COMMON, "SOME", 4, 0x454D4F57ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SORT, COMMON, "SORT", 4, 0x54524F57ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SPACE, COMMON, "SPACE", 5, 0x4341509DULL, 0, 0, 0 },
{ CONSTANT_COMMON_SPECIAL, COMMON, "SPECIAL", 7, 0x439191A3ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SPECIAL_OPERATOR_P, COMMON, "SPECIAL-OPERATOR-P", 18, 0x1526866BULL, 0, 0, 0 },
{ CONSTANT_COMMON_SPEED, COMMON, "SPEED", 5, 0x4545509CULL, 0, 0, 0 },
{ CONSTANT_COMMON_SQRT, COMMON, "SQRT", 4, 0x54525157ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STABLE_SORT, COMMON, "STABLE-SORT", 11, 0x15C2EBF9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STANDARD, COMMON, "STANDARD", 8, 0x1293959FULL, 0, 0, 0 },
{ CONSTANT_COMMON_STANDARD_CHAR, COMMON, "STANDARD-CHAR", 13, 0x53DBD923ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STANDARD_CHAR_P, COMMON, "STANDARD-CHAR-P", 15, 0x542C0625ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STANDARD_CLASS, COMMON, "STANDARD-CLASS", 14, 0x53E02C25ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STANDARD_GENERIC_FUNCTION, COMMON, "STANDARD-GENERIC-FUNCTION", 25, 0x41C0C9E0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STANDARD_METHOD, COMMON, "STANDARD-METHOD", 15, 0x671D321BULL, 0, 0, 0 },
{ CONSTANT_COMMON_STANDARD_OBJECT, COMMON, "STANDARD-OBJECT", 15, 0x5D2A2818ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STEP, COMMON, "STEP", 4, 0x50455457ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STORAGE_CONDITION, COMMON, "STORAGE-CONDITION", 17, 0x132C3F7FULL, 0, 0, 0 },
{ CONSTANT_COMMON_STORE_VALUE, COMMON, "STORE-VALUE", 11, 0x13EAD6EFULL, 0, 0, 0 },
{ CONSTANT_COMMON_STREAM, COMMON, "STREAM", 6, 0x4552A19AULL, 0, 0, 0 },
{ CONSTANT_COMMON_STREAMP, COMMON, "STREAMP", 7, 0x45A2A19BULL, 0, 0, 0 },
{ CONSTANT_COMMON_STREAM_ELEMENT_TYPE, COMMON, "STREAM-ELEMENT-TYPE", 19, 0x243F8B9AULL, 0, 0, 0 },
{ CONSTANT_COMMON_STREAM_ERROR, COMMON, "STREAM-ERROR", 12, 0x5CCEF3F2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STREAM_ERROR_STREAM, COMMON, "STREAM-ERROR-STREAM", 19, 0x2F70886BULL, 0, 0, 0 },
{ CONSTANT_COMMON_STREAM_EXTERNAL_FORMAT, COMMON, "STREAM-EXTERNAL-FORMAT", 22, 0x5763DAD7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING, COMMON, "STRING", 6, 0x49529BA7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_EQL, COMMON, "STRING=", 7, 0x498F9BA8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_NOT_EQL, COMMON, "STRING/=", 8, 0x06819BA9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_LESS, COMMON, "STRING<", 7, 0x498E9BA8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_GREATER, COMMON, "STRING>", 7, 0x49909BA8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_LESS_EQUAL, COMMON, "STRING<=", 8, 0x068E9BA9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_GREATER_EQUAL, COMMON, "STRING>=", 8, 0x06909BA9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_CAPITALIZE, COMMON, "STRING-CAPITALIZE", 17, 0x3B123879ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_DOWNCASE, COMMON, "STRING-DOWNCASE", 15, 0x51134640ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_EQUAL, COMMON, "STRING-EQUAL", 12, 0x5AC0F0FEULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_GREATERP, COMMON, "STRING-GREATERP", 15, 0x65113347ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_LEFT_TRIM, COMMON, "STRING-LEFT-TRIM", 16, 0x101D344AULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_LESSP, COMMON, "STRING-LESSP", 12, 0x65D2EEF2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_NOT_EQUAL, COMMON, "STRING-NOT-EQUAL", 16, 0x28EE4551ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_NOT_GREATERP, COMMON, "STRING-NOT-GREATERP", 19, 0x333E879AULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_NOT_LESSP, COMMON, "STRING-NOT-LESSP", 16, 0x34004345ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_RIGHT_TRIM, COMMON, "STRING-RIGHT-TRIM", 17, 0x391A3775ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_STREAM, COMMON, "STRING-STREAM", 13, 0x5DC4EE4FULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_TRIM, COMMON, "STRING-TRIM", 11, 0x1DCCE4FEULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRING_UPCASE, COMMON, "STRING-UPCASE", 13, 0x71C0DF43ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRINGP, COMMON, "STRINGP", 7, 0x49A29BA8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRUCTURE, COMMON, "STRUCTURE", 9, 0x27A7A8E4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRUCTURE_CLASS, COMMON, "STRUCTURE-CLASS", 15, 0x743E292BULL, 0, 0, 0 },
{ CONSTANT_COMMON_STRUCTURE_OBJECT, COMMON, "STRUCTURE-OBJECT", 16, 0x3E3A1B35ULL, 0, 0, 0 },
{ CONSTANT_COMMON_STYLE_WARNING, COMMON, "STYLE-WARNING", 13, 0x5BF9D03EULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBSETP, COMMON, "SUBSETP", 7, 0x5392A99FULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBSEQ, COMMON, "SUBSEQ", 6, 0x5342A69EULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBLIS, COMMON, "SUBLIS", 6, 0x4C42A8A2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBST, COMMON, "SUBST", 5, 0x534255ACULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBSTITUTE, COMMON, "SUBSTITUTE", 10, 0x2896E405ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBSTITUTE_IF, COMMON, "SUBSTITUTE-IF", 13, 0x71C3E44EULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBSTITUTE_IF_NOT, COMMON, "SUBSTITUTE-IF-NOT", 17, 0x411211A6ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBST_IF, COMMON, "SUBST-IF", 8, 0x198B82AFULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBST_IF_NOT, COMMON, "SUBST-IF-NOT", 12, 0x6DDAD0E0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SUBTYPEP, COMMON, "SUBTYPEP", 8, 0x2487A5B4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SVREF, COMMON, "SVREF", 5, 0x4552569EULL, 0, 0, 0 },
{ CONSTANT_COMMON_SXHASH, COMMON, "SXHASH", 6, 0x4148A0ACULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOL, COMMON, "SYMBOL", 6, 0x424DA5A8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOL_FUNCTION, COMMON, "SYMBOL-FUNCTION", 15, 0x5D0C434FULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOL_MACROLET, COMMON, "SYMBOL-MACROLET", 15, 0x5F212E3EULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOL_NAME, COMMON, "SYMBOL-NAME", 11, 0x10BFF2EEULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOL_PACKAGE, COMMON, "SYMBOL-PACKAGE", 14, 0x53C62E38ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOL_PLIST, COMMON, "SYMBOL-PLIST", 12, 0x66CDEEFAULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOL_VALUE, COMMON, "SYMBOL-VALUE", 12, 0x5DCFF1EFULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYMBOLP, COMMON, "SYMBOLP", 7, 0x429DA5A9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYNONYM_STREAM, COMMON, "SYNONYM-STREAM", 14, 0x41EE5443ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SYNONYM_STREAM_SYMBOL, COMMON, "SYNONYM-STREAM-SYMBOL", 21, 0x645DA1EFULL, 0, 0, 0 },
{ CONSTANT_COMMON_TAGBODY, COMMON, "TAGBODY", 7, 0x42A085AAULL, 0, 0, 0 },
{ CONSTANT_COMMON_TAILP, COMMON, "TAILP", 5, 0x4C4941A9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TAN, COMMON, "TAN", 3, 0x004E4157ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TANH, COMMON, "TANH", 4, 0x484E4158ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TENTH, COMMON, "TENTH", 5, 0x544E45A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TERPRI, COMMON, "TERPRI", 6, 0x50528EACULL, 0, 0, 0 },
{ CONSTANT_COMMON_THE, COMMON, "THE", 3, 0x00454857ULL, 0, 0, 0 },
{ CONSTANT_COMMON_THIRD, COMMON, "THIRD", 5, 0x5249489DULL, 0, 0, 0 },
{ CONSTANT_COMMON_THROW, COMMON, "THROW", 5, 0x4F5248B0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TIME, COMMON, "TIME", 4, 0x454D4958ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TRACE, COMMON, "TRACE", 5, 0x4341529EULL, 0, 0, 0 },
{ CONSTANT_COMMON_TRANSLATE_LOGICAL_PATHNAME, COMMON, "TRANSLATE-LOGICAL-PATHNAME", 26, 0x35B0D03AULL, 0, 0, 0 },
{ CONSTANT_COMMON_TRANSLATE_PATHNAME, COMMON, "TRANSLATE-PATHNAME", 18, 0x2521599FULL, 0, 0, 0 },
{ CONSTANT_COMMON_TREE_EQUAL, COMMON, "TREE-EQUAL", 10, 0x1A96E3CCULL, 0, 0, 0 },
{ CONSTANT_COMMON_TRUENAME, COMMON, "TRUENAME", 8, 0x0AA293AAULL, 0, 0, 0 },
{ CONSTANT_COMMON_TRUNCATE, COMMON, "TRUNCATE", 8, 0x13A9939FULL, 0, 0, 0 },
{ CONSTANT_COMMON_TWO_WAY_STREAM, COMMON, "TWO-WAY-STREAM", 14, 0x1FFB3A4DULL, 0, 0, 0 },
{ CONSTANT_COMMON_TWO_WAY_STREAM_INPUT_STREAM, COMMON, "TWO-WAY-STREAM-INPUT-STREAM", 27, 0x101F1F1AULL, 0, 0, 0 },
{ CONSTANT_COMMON_TWO_WAY_STREAM_OUTPUT_STREAM, COMMON, "TWO-WAY-STREAM-OUTPUT-STREAM", 28, 0x660D0156ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TYPE, COMMON, "TYPE", 4, 0x45505958ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TYPEP, COMMON, "TYPEP", 5, 0x455059A9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TYPE_ERROR, COMMON, "TYPE-ERROR", 10, 0x17A2F0DAULL, 0, 0, 0 },
{ CONSTANT_COMMON_TYPE_ERROR_DATUM, COMMON, "TYPE-ERROR-DATUM", 16, 0x29254521ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TYPE_ERROR_EXPECTED_TYPE, COMMON, "TYPE-ERROR-EXPECTED-TYPE", 24, 0x12A9DFE8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TYPE_OF, COMMON, "TYPE-OF", 7, 0x4596A888ULL, 0, 0, 0 },
{ CONSTANT_COMMON_TYPECASE, COMMON, "TYPECASE", 8, 0x0AA39A9FULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNBOUND_SLOT, COMMON, "UNBOUND-SLOT", 12, 0x50D5E909ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNBOUND_SLOT_INSTANCE, COMMON, "UNBOUND-SLOT-INSTANCE", 21, 0x677273D8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNBOUND_VARIABLE, COMMON, "UNBOUND-VARIABLE", 16, 0x0B252051ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNDEFINED_FUNCTION, COMMON, "UNDEFINED-FUNCTION", 18, 0x292D568EULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNEXPORT, COMMON, "UNEXPORT", 8, 0x2C979DADULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNINTERN, COMMON, "UNINTERN", 8, 0x1C9B93B1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNION, COMMON, "UNION", 5, 0x4F494EA8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNLESS, COMMON, "UNLESS", 6, 0x454CA1AEULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNREAD_CHAR, COMMON, "UNREAD-CHAR", 11, 0x08D1D3E9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNSIGNED_BYTE, COMMON, "UNSIGNED-BYTE", 13, 0x61F1DF1BULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNTRACE, COMMON, "UNTRACE", 7, 0x5299919DULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNUSE_PACKAGE, COMMON, "UNUSE-PACKAGE", 13, 0x5BE6C72FULL, 0, 0, 0 },
{ CONSTANT_COMMON_UNWIND_PROTECT, COMMON, "UNWIND-PROTECT", 14, 0x5ED93646ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UPDATE_INSTANCE_FOR_DIFFERENT_CLASS, COMMON, "UPDATE-INSTANCE-FOR-DIFFERENT-CLASS", 35, 0x077F97CCULL, 0, 0, 0 },
{ CONSTANT_COMMON_UPDATE_INSTANCE_FOR_REDEFINED_CLASS, COMMON, "UPDATE-INSTANCE-FOR-REDEFINED-CLASS", 35, 0x7D868ACBULL, 0, 0, 0 },
{ CONSTANT_COMMON_UPGRADED_ARRAY_ELEMENT_TYPE, COMMON, "UPGRADED-ARRAY-ELEMENT-TYPE", 27, 0x47CC1912ULL, 0, 0, 0 },
{ CONSTANT_COMMON_UPGRADED_COMPLEX_PART_TYPE, COMMON, "UPGRADED-COMPLEX-PART-TYPE", 26, 0x67B6E6FEULL, 0, 0, 0 },
{ CONSTANT_COMMON_UPPER_CASE_P, COMMON, "UPPER-CASE-P", 12, 0x56C0C306ULL, 0, 0, 0 },
{ CONSTANT_COMMON_USE_PACKAGE, COMMON, "USE-PACKAGE", 11, 0x78CDDBF1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_USE_VALUE, COMMON, "USE-VALUE", 9, 0x029194F9ULL, 0, 0, 0 },
{ CONSTANT_COMMON_USER_HOMEDIR_PATHNAME, COMMON, "USER-HOMEDIR-PATHNAME", 21, 0x13607E96ULL, 0, 0, 0 },
{ CONSTANT_COMMON_VALUES, COMMON, "VALUES", 6, 0x554C94A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_VALUES_LIST, COMMON, "VALUES-LIST", 11, 0x21CDE7EFULL, 0, 0, 0 },
{ CONSTANT_COMMON_VARIABLE, COMMON, "VARIABLE", 8, 0x0E9E839FULL, 0, 0, 0 },
{ CONSTANT_COMMON_VECTOR, COMMON, "VECTOR", 6, 0x544397ABULL, 0, 0, 0 },
{ CONSTANT_COMMON_VECTOR_POP, COMMON, "VECTOR-POP", 10, 0x2470E7FEULL, 0, 0, 0 },
{ CONSTANT_COMMON_VECTOR_PUSH, COMMON, "VECTOR-PUSH", 11, 0x24B8EB05ULL, 0, 0, 0 },
{ CONSTANT_COMMON_VECTOR_PUSH_EXTEND, COMMON, "VECTOR-PUSH-EXTEND", 18, 0x170D879FULL, 0, 0, 0 },
{ CONSTANT_COMMON_VECTORP, COMMON, "VECTORP", 7, 0x549397ACULL, 0, 0, 0 },
{ CONSTANT_COMMON_WARN, COMMON, "WARN", 4, 0x4E52415BULL, 0, 0, 0 },
{ CONSTANT_COMMON_WARNING, COMMON, "WARNING", 7, 0x4E998FA7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WILD_PATHNAME_P, COMMON, "WILD-PATHNAME-P", 15, 0x661F1520ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_ACCESSORS, COMMON, "WITH-ACCESSORS", 14, 0x5AEB3129ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_COMPILATION_UNIT, COMMON, "WITH-COMPILATION-UNIT", 21, 0x6E8D74BEULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_CONDITION_RESTARTS, COMMON, "WITH-CONDITION-RESTARTS", 23, 0x73CCCBC5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_HASH_TABLE_ITERATOR, COMMON, "WITH-HASH-TABLE-ITERATOR", 24, 0x2EC3B3B0ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_INPUT_FROM_STRING, COMMON, "WITH-INPUT-FROM-STRING", 22, 0x556FD1E2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_OPEN_FILE, COMMON, "WITH-OPEN-FILE", 14, 0x56EB0B2CULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_OPEN_STREAM, COMMON, "WITH-OPEN-STREAM", 16, 0x2F390B34ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_OUTPUT_TO_STRING, COMMON, "WITH-OUTPUT-TO-STRING", 21, 0x6B748FD8ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_PACKAGE_ITERATOR, COMMON, "WITH-PACKAGE-ITERATOR", 21, 0x658565B5ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_SIMPLE_RESTART, COMMON, "WITH-SIMPLE-RESTART", 19, 0x178A807AULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_SLOTS, COMMON, "WITH-SLOTS", 10, 0x17A0EFE2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WITH_STANDARD_IO_SYNTAX, COMMON, "WITH-STANDARD-IO-SYNTAX", 23, 0x79E4A2AEULL, 0, 0, 0 },
{ CONSTANT_COMMON_WHEN, COMMON, "WHEN", 4, 0x4E45485BULL, 0, 0, 0 },
{ CONSTANT_COMMON_WRITE, COMMON, "WRITE", 5, 0x544952A1ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WRITE_BYTE, COMMON, "WRITE-BYTE", 10, 0x2D8BC4FAULL, 0, 0, 0 },
{ CONSTANT_COMMON_WRITE_CHAR, COMMON, "WRITE-CHAR", 10, 0x1C8CD1E7ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WRITE_LINE, COMMON, "WRITE-LINE", 10, 0x1D95C4F4ULL, 0, 0, 0 },
{ CONSTANT_COMMON_WRITE_SEQUENCE, COMMON, "WRITE-SEQUENCE", 14, 0x67E21A3EULL, 0, 0, 0 },
{ CONSTANT_COMMON_WRITE_STRING, COMMON, "WRITE-STRING", 12, 0x6FEAC8FAULL, 0, 0, 0 },
{ CONSTANT_COMMON_WRITE_TO_STRING, COMMON, "WRITE-TO-STRING", 15, 0x76392121ULL, 0, 0, 0 },
{ CONSTANT_COMMON_YES_OR_NO_P, COMMON, "YES-OR-NO-P", 11, 0x7BD0C502ULL, 0, 0, 0 },
{ CONSTANT_COMMON_Y_OR_N_P, COMMON, "Y-OR-N-P", 8, 0x227C7B8EULL, 0, 0, 0 },
{ CONSTANT_COMMON_ZEROP, COMMON, "ZEROP", 5, 0x4F5245AFULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DATUM, CLOS, "DATUM", 5, 0x55544196ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_EXPECTED_TYPE, CLOS, "EXPECTED-TYPE", 13, 0x59EF0107ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, CLOS, "FORMAT-ARGUMENTS", 16, 0x2F29392EULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FORMAT_CONTROL, CLOS, "FORMAT-CONTROL", 14, 0x62D43E33ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_OPERANDS, CLOS, "OPERANDS", 8, 0x25899E98ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_OPERATION, CLOS, "OPERATION", 9, 0x218EA4E7ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_PACKAGE, CLOS, "PACKAGE", 7, 0x4B888898ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_PATHNAME, CLOS, "PATHNAME", 8, 0x0DA182A6ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_STREAM, CLOS, "STREAM", 6, 0x4552A19AULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_NAME, CLOS, "NAME", 4, 0x454D4152ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DIRECT_SLOTS, CLOS, "DIRECT-SLOTS", 12, 0x6BD3ECDFULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DIRECT_SUBCLASSES, CLOS, "DIRECT-SUBCLASSES", 17, 0x2A163381ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DIRECT_SUPERCLASSES, CLOS, "DIRECT-SUPERCLASSES", 19, 0x3E597F85ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_PRECEDENCE_LIST, CLOS, "CLASS-PRECEDENCE-LIST", 21, 0x026357B6ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_EFFECTIVE_SLOTS, CLOS, "EFFECTIVE-SLOTS", 15, 0x68361C2BULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FINALIZED_P, CLOS, "FINALIZED-P", 11, 0x06F8BFE1ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_PROTOTYPE, CLOS, "PROTOTYPE", 9, 0x24A8A6EDULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DEFAULT_INITARGS, CLOS, "DEFAULT-INITARGS", 16, 0x162B3233ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DIRECT_DEFAULT_INITARGS, CLOS, "DIRECT-DEFAULT-INITARGS", 23, 0x6995C8CFULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_VERSION, CLOS, "VERSION", 7, 0x53A094A6ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_REDEFINED_CLASS, CLOS, "REDEFINED-CLASS", 15, 0x57290F2CULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_NAME, KEYWORD, "NAME", 4, 0x454D4152ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_DIRECT_SLOTS, KEYWORD, "DIRECT-SLOTS", 12, 0x6BD3ECDFULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_DIRECT_SUBCLASSES, KEYWORD, "DIRECT-SUBCLASSES", 17, 0x2A163381ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_DIRECT_SUPERCLASSES, KEYWORD, "DIRECT-SUPERCLASSES", 19, 0x3E597F85ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_CLASS_PRECEDENCE_LIST, KEYWORD, "CLASS-PRECEDENCE-LIST", 21, 0x026357B6ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_EFFECTIVE_SLOTS, KEYWORD, "EFFECTIVE-SLOTS", 15, 0x68361C2BULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_FINALIZED_P, KEYWORD, "FINALIZED-P", 11, 0x06F8BFE1ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_PROTOTYPE, KEYWORD, "PROTOTYPE", 9, 0x24A8A6EDULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_DEFAULT_INITARGS, KEYWORD, "DEFAULT-INITARGS", 16, 0x162B3233ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_DIRECT_DEFAULT_INITARGS, KEYWORD, "DIRECT-DEFAULT-INITARGS", 23, 0x6995C8CFULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_VERSION, KEYWORD, "VERSION", 7, 0x53A094A6ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_REDEFINED_CLASS, KEYWORD, "REDEFINED-CLASS", 15, 0x57290F2CULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHODS, CLOS, "METHODS", 7, 0x48A789A3ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_LAMBDA_LIST, CLOS, "LAMBDA-LIST", 11, 0x0ECED5E4ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ARGUMENT_PRECEDENCE_ORDER, CLOS, "ARGUMENT-PRECEDENCE-ORDER", 25, 0x26B5C306ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DECLARATIONS, CLOS, "DECLARATIONS", 12, 0x73D2E6DAULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHOD_CLASS, CLOS, "METHOD-CLASS", 12, 0x5ED4CAF4ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHOD_COMBINATION, CLOS, "METHOD-COMBINATION", 18, 0x1E18669AULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_VECTOR, CLOS, "VECTOR", 6, 0x544397ABULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_REMOVE, CLOS, "REMOVE", 6, 0x4F4D8AAEULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ARGUMENT, CLOS, "ARGUMENT", 8, 0x29959796ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_EQLCHECK, CLOS, "EQLCHECK", 8, 0x0E8F9695ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CACHE, CLOS, "CACHE", 5, 0x4843418DULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CALL, CLOS, "CALL", 4, 0x4C4C4147ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FUNCTION, CLOS, "FUNCTION", 8, 0x119D9EA2ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_PRECEDENCE_INDEX, CLOS, "PRECEDENCE-INDEX", 16, 0x32FD2036ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_METHODS, KEYWORD, "METHODS", 7, 0x48A789A3ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_LAMBDA_LIST, KEYWORD, "LAMBDA-LIST", 11, 0x0ECED5E4ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_ARGUMENT_PRECEDENCE_ORDER, KEYWORD, "ARGUMENT-PRECEDENCE-ORDER", 25, 0x26B5C306ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_DECLARATIONS, KEYWORD, "DECLARATIONS", 12, 0x73D2E6DAULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_METHOD_CLASS, KEYWORD, "METHOD-CLASS", 12, 0x5ED4CAF4ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_METHOD_COMBINATION, KEYWORD, "METHOD-COMBINATION", 18, 0x1E18669AULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_VECTOR, KEYWORD, "VECTOR", 6, 0x544397ABULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_REMOVE, KEYWORD, "REMOVE", 6, 0x4F4D8AAEULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_ARGUMENT, KEYWORD, "ARGUMENT", 8, 0x29959796ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_EQLCHECK, KEYWORD, "EQLCHECK", 8, 0x0E8F9695ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_CACHE, KEYWORD, "CACHE", 5, 0x4843418DULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_CALL, KEYWORD, "CALL", 4, 0x4C4C4147ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_FUNCTION, KEYWORD, "FUNCTION", 8, 0x119D9EA2ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_PRECEDENCE_INDEX, KEYWORD, "PRECEDENCE-INDEX", 16, 0x32FD2036ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION, CLOS, "GENERIC-FUNCTION", 16, 0x042F2D43ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_QUALIFIERS, CLOS, "QUALIFIERS", 10, 0x118AEEF6ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SPECIALIZERS, CLOS, "SPECIALIZERS", 12, 0x5FE3D702ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_LONG_P, CLOS, "LONG-P", 6, 0x474E9F7FULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DOCUMENTATION, CLOS, "DOCUMENTATION", 13, 0x78DAE92DULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_IDENTITY, CLOS, "IDENTITY", 8, 0x27998DA5ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_OPERATOR, CLOS, "OPERATOR", 8, 0x2494A498ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ARGUMENTS, CLOS, "ARGUMENTS", 9, 0x299597EAULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC, CLOS, "GENERIC", 7, 0x45918EA0ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FORM, CLOS, "FORM", 4, 0x4D524F4AULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DECLARE, CLOS, "DECLARE", 7, 0x4C88978CULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_GENERIC_FUNCTION, KEYWORD, "GENERIC-FUNCTION", 16, 0x042F2D43ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_QUALIFIERS, KEYWORD, "QUALIFIERS", 10, 0x118AEEF6ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_SPECIALIZERS, KEYWORD, "SPECIALIZERS", 12, 0x5FE3D702ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_LONG_P, KEYWORD, "LONG-P", 6, 0x474E9F7FULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_DOCUMENTATION, KEYWORD, "DOCUMENTATION", 13, 0x78DAE92DULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_IDENTITY, KEYWORD, "IDENTITY", 8, 0x27998DA5ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_OPERATOR, KEYWORD, "OPERATOR", 8, 0x2494A498ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_ARGUMENTS, KEYWORD, "ARGUMENTS", 9, 0x299597EAULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_GENERIC, KEYWORD, "GENERIC", 7, 0x45918EA0ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_FORM, KEYWORD, "FORM", 4, 0x4D524F4AULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_DECLARE, KEYWORD, "DECLARE", 7, 0x4C88978CULL, 0, 0, 1 },
{ CONSTANT_CLOSNAME_OBJECT, CLOS, "OBJECT", 6, 0x454A9698ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_TYPE, CLOS, "TYPE", 4, 0x45505958ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_OBJECT, KEYWORD, "OBJECT", 6, 0x454A9698ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_TYPE, KEYWORD, "TYPE", 4, 0x45505958ULL, 0, 0, 1 },
{ CONSTANT_CLOSNAME_READERS, CLOS, "READERS", 7, 0x4494979EULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_WRITERS, CLOS, "WRITERS", 7, 0x549CA4A3ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ACCESSORS, CLOS, "ACCESSORS", 9, 0x179296F0ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_INITARGS, CLOS, "INITARGS", 8, 0x2790A092ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_INITFORM, CLOS, "INITFORM", 8, 0x219B9D97ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_INITFUNCTION, CLOS, "INITFUNCTION", 12, 0x65E6ECEFULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ALLOCATION, CLOS, "ALLOCATION", 10, 0x18A0DBDDULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_INSTANCE, CLOS, "INSTANCE", 8, 0x19969C92ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS, CLOS, "CLASS", 5, 0x53414C9BULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METACLASS, CLOS, "METACLASS", 9, 0x149591ECULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_BINDING, CLOS, "BINDING", 7, 0x44959792ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ORDER, CLOS, "ORDER", 5, 0x454452A6ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_READERS, KEYWORD, "READERS", 7, 0x4494979EULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_WRITERS, KEYWORD, "WRITERS", 7, 0x549CA4A3ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_ACCESSORS, KEYWORD, "ACCESSORS", 9, 0x179296F0ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_INITARGS, KEYWORD, "INITARGS", 8, 0x2790A092ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_INITFORM, KEYWORD, "INITFORM", 8, 0x219B9D97ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_INITFUNCTION, KEYWORD, "INITFUNCTION", 12, 0x65E6ECEFULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_ALLOCATION, KEYWORD, "ALLOCATION", 10, 0x18A0DBDDULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_INSTANCE, KEYWORD, "INSTANCE", 8, 0x19969C92ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_CLASS, KEYWORD, "CLASS", 5, 0x53414C9BULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_METACLASS, KEYWORD, "METACLASS", 9, 0x149591ECULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_BINDING, KEYWORD, "BINDING", 7, 0x44959792ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_ORDER, KEYWORD, "ORDER", 5, 0x454452A6ULL, 0, 0, 1 },
{ CONSTANT_CLOSNAME_SLOTS, CLOS, "SLOTS", 5, 0x544F4CABULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_INCLUDE, CLOS, "INCLUDE", 7, 0x4C8892A5ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_VALUE, CLOS, "VALUE", 5, 0x554C41A0ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_PREDICATE, CLOS, "PREDICATE", 9, 0x188695E7ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ACCESS, CLOS, "ACCESS", 6, 0x4543969AULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_COPIER, CLOS, "COPIER", 6, 0x4950A18EULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CONSTRUCTOR, CLOS, "CONSTRUCTOR", 11, 0x16F5F0F6ULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_SLOTS, KEYWORD, "SLOTS", 5, 0x544F4CABULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_INCLUDE, KEYWORD, "INCLUDE", 7, 0x4C8892A5ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_VALUE, KEYWORD, "VALUE", 5, 0x554C41A0ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_PREDICATE, KEYWORD, "PREDICATE", 9, 0x188695E7ULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_ACCESS, KEYWORD, "ACCESS", 6, 0x4543969AULL, 0, 0, 0 },
{ CONSTANT_CLOSKEY_COPIER, KEYWORD, "COPIER", 6, 0x4950A18EULL, 0, 0, 1 },
{ CONSTANT_CLOSKEY_CONSTRUCTOR, KEYWORD, "CONSTRUCTOR", 11, 0x16F5F0F6ULL, 0, 0, 1 },
{ CONSTANT_SYSTEM_VALUE, SYSTEM, "VALUE", 5, 0x554C41A0ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FUNCTION, SYSTEM, "FUNCTION", 8, 0x119D9EA2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SETF, SYSTEM, "SETF", 4, 0x46544557ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INLINE_FUNCTION, SYSTEM, "INLINE-FUNCTION", 15, 0x640B3144ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INLINE_SETF, SYSTEM, "INLINE-SETF", 11, 0x1CBFE7E7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TAGBODY, SYSTEM, "TAGBODY", 7, 0x42A085AAULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BLOCK, SYSTEM, "BLOCK", 5, 0x434F4C92ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DECLAIM, SYSTEM, "DECLAIM", 7, 0x4C908E8CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFUN, SYSTEM, "DEFUN", 5, 0x55464597ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFMACRO, SYSTEM, "DEFMACRO", 8, 0x1C98888DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFTYPE, SYSTEM, "DEFTYPE", 7, 0x548B95A4ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFINE_COMPILER_MACRO, SYSTEM, "DEFINE-COMPILER-MACRO", 21, 0x55595EDEULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MACRO_LAMBDA, SYSTEM, "MACRO-LAMBDA", 12, 0x54D3B0F5ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DESTRUCTURING_BIND, SYSTEM, "DESTRUCTURING-BIND", 18, 0x40225E92ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SPECIAL, SYSTEM, "SPECIAL", 7, 0x439191A3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LEXICAL, SYSTEM, "LEXICAL", 7, 0x49A48696ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOCAL, SYSTEM, "LOCAL", 5, 0x41434F9DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_GLOBAL, SYSTEM, "GLOBAL", 6, 0x424F988EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LAMBDA, SYSTEM, "LAMBDA", 6, 0x424D8296ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SCOPE, SYSTEM, "SCOPE", 5, 0x504F439DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MULTIPLE_VALUE_BIND, SYSTEM, "MULTIPLE-VALUE-BIND", 19, 0x284B8F74ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DECLARATION, SYSTEM, "DECLARATION", 11, 0x20D2E6D9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INLINE, SYSTEM, "INLINE", 6, 0x494C939DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DYNAMIC_VALUE, SYSTEM, "DYNAMIC-VALUE", 13, 0x43DDE439ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DYNAMIC_FUNCTION, SYSTEM, "DYNAMIC-FUNCTION", 16, 0x002F413BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_IGNORE_VALUE, SYSTEM, "IGNORE-VALUE", 12, 0x6AD0D8E8ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_IGNORE_FUNCTION, SYSTEM, "IGNORE-FUNCTION", 15, 0x6A0D2A48ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE, SYSTEM, "TYPE", 4, 0x45505958ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_SPECIAL, SYSTEM, "TYPE-SPECIAL", 12, 0x56E1F5D0ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_LEXICAL, SYSTEM, "TYPE-LEXICAL", 12, 0x69D6E8D6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_VALUE, SYSTEM, "TYPE-VALUE", 10, 0x1191F4E0ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_FUNCTION, SYSTEM, "TYPE-FUNCTION", 13, 0x62EEF41FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_SETF, SYSTEM, "TYPE-SETF", 9, 0x1995ACD0ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FUNCTION_ARGTYPE, SYSTEM, "FUNCTION-ARGTYPE", 16, 0x1E40392BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FUNCTION_RETTYPE, SYSTEM, "FUNCTION-RETTYPE", 16, 0x2B334A2BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_HANDLER, SYSTEM, "HANDLER", 7, 0x44A0869BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_HANDLER_BIND, SYSTEM, "HANDLER-BIND", 12, 0x35EECFE2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_HANDLER_CASE, SYSTEM, "HANDLER-CASE", 12, 0x36F3C7E3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_RESTART, SYSTEM, "RESTART", 7, 0x54A7979AULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_RESTART_BIND, SYSTEM, "RESTART-BIND", 12, 0x45F5E0E1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_RESTART_CASE, SYSTEM, "RESTART-CASE", 12, 0x46FAD8E2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_STANDARD, SYSTEM, "STANDARD", 8, 0x1293959FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_COMPILED_MACRO_FUNCTION, SYSTEM, "COMPILED-MACRO-FUNCTION", 23, 0x7292D5C0ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CONTROL, SYSTEM, "CONTROL", 7, 0x549A9E9CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CODE, SYSTEM, "CODE", 4, 0x45444F47ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CALLNAME, SYSTEM, "CALLNAME", 8, 0x11998299ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_EVAL, SYSTEM, "EVAL", 4, 0x4C415649ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INDEX, SYSTEM, "INDEX", 5, 0x45444EA6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SYSTEM, SYSTEM, "SYSTEM", 6, 0x5453A69EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_QUOTE, SYSTEM, "QUOTE", 5, 0x544F559BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ENVIRONMENT, SYSTEM, "ENVIRONMENT", 11, 0x16F8EBE7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CHARACTER2, SYSTEM, "CHARACTER2", 10, 0x1795BDE0ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CHARQUEUE, SYSTEM, "CHARQUEUE", 9, 0x27869DE2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CHARBIT, SYSTEM, "CHARBIT", 7, 0x5295918CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SYMSTACK, SYSTEM, "SYMSTACK", 8, 0x1E909AAFULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BITTYPE, SYSTEM, "BITTYPE", 7, 0x549999A2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_READLABEL, SYSTEM, "READLABEL", 9, 0x098386F3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_READINFO_SYMBOL, SYSTEM, "READINFO", 8, 0x138793A3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_READTYPE, SYSTEM, "READTYPE", 8, 0x09919EAEULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BITCONS, SYSTEM, "BITCONS", 7, 0x43A79798ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BITBUFFER, SYSTEM, "BITBUFFER", 9, 0x079A8FF2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_HASHITERATOR, SYSTEM, "HASHITERATOR", 12, 0x6CE7E9DEULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PACKAGEITERATOR, SYSTEM, "PACKAGEITERATOR", 15, 0x562D1D48ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TAGINFO, SYSTEM, "TAGINFO", 7, 0x499687A9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ARRAY_DIMENSION, SYSTEM, "ARRAY-DIMENSION", 15, 0x5E33143FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ARRAY_GENERAL, SYSTEM, "ARRAY-GENERAL", 13, 0x47EBC541ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ARRAY_SPECIALIZED, SYSTEM, "ARRAY-SPECIALIZED", 17, 0x18490C80ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UNBOUND, SYSTEM, "UNBOUND", 7, 0x4F869CB1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SPACE, SYSTEM, "SPACE", 5, 0x4341509DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SPACE1, SYSTEM, "SPACE1", 6, 0x4341819EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_RESERVED, SYSTEM, "RESERVED", 8, 0x09989BACULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_END, SYSTEM, "END", 3, 0x00444E48ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PROMPT_STREAM, SYSTEM, "PROMPT-STREAM", 13, 0x61C1F94EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PRETTY_STREAM, SYSTEM, "PRETTY-STREAM", 13, 0x68B7FE52ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MEMORY_STREAM, SYSTEM, "MEMORY-STREAM", 13, 0x63BFF14DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PIPE_STREAM, SYSTEM, "PIPE-STREAM", 11, 0x17F1DDCDULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SIMPLE_CONTROL_ERROR, SYSTEM, "SIMPLE-CONTROL-ERROR", 20, 0x7D4B7BA3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SIMPLE_FILE_ERROR, SYSTEM, "SIMPLE-FILE-ERROR", 17, 0x13122D90ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SIMPLE_PACKAGE_ERROR, SYSTEM, "SIMPLE-PACKAGE-ERROR", 20, 0x7942698DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SIMPLE_PARSE_ERROR, SYSTEM, "SIMPLE-PARSE-ERROR", 18, 0x3820786EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SIMPLE_PROGRAM_ERROR, SYSTEM, "SIMPLE-PROGRAM-ERROR", 20, 0x0A3E7D98ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SIMPLE_READER_ERROR, SYSTEM, "SIMPLE-READER-ERROR", 19, 0x3A564C9BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SIMPLE_STYLE_WARNING, SYSTEM, "SIMPLE-STYLE-WARNING", 20, 0x02568882ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_INITIALLY, SYSTEM, "LOOP-INITIALLY", 14, 0x65DF3B27ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FINALLY, SYSTEM, "LOOP-FINALLY", 12, 0x77E4E1C6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_WITH, SYSTEM, "LOOP-WITH", 9, 0x2498A6CAULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS, SYSTEM, "LOOP-FOR-AS", 11, 0x22F1D6B1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_IN_LIST, SYSTEM, "LOOP-FOR-AS-IN-LIST", 19, 0x1C73784BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_ON_LIST, SYSTEM, "LOOP-FOR-AS-ON-LIST", 19, 0x1C737851ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_EQUALS_THEN, SYSTEM, "LOOP-FOR-AS-EQUALS-THEN", 23, 0x65C2C096ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_ACROSS, SYSTEM, "LOOP-FOR-AS-ACROSS", 18, 0x1F446D4CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_HASH, SYSTEM, "LOOP-FOR-AS-HASH", 16, 0x184517FEULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_PACKAGE_SYMBOL, SYSTEM, "LOOP-FOR-AS-PACKAGE-SYMBOL", 26, 0x0AC804F3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_PACKAGE_PRESENT, SYSTEM, "LOOP-FOR-AS-PACKAGE-PRESENT", 27, 0x1C13FFE7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_PACKAGE_EXTERNAL, SYSTEM, "LOOP-FOR-AS-PACKAGE-EXTERNAL", 28, 0x5A1005EAULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_ARITHMETIC_UP, SYSTEM, "LOOP-FOR-AS-ARITHMETIC-UP", 25, 0x4DADB9E1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNTO, SYSTEM, "LOOP-FOR-AS-ARITHMETIC-DOWNTO", 29, 0x10FC1133ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNFROM, SYSTEM, "LOOP-FOR-AS-ARITHMETIC-DOWNFROM", 31, 0x03496038ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_DO, SYSTEM, "LOOP-DO", 7, 0x509E9380ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_RETURN, SYSTEM, "LOOP-RETURN", 11, 0x24E2F3D9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_IF, SYSTEM, "LOOP-IF", 7, 0x50959880ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_UNLESS, SYSTEM, "LOOP-UNLESS", 11, 0x1CF0F7C9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_COLLECT, SYSTEM, "LOOP-COLLECT", 12, 0x70E1D7D1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_APPEND, SYSTEM, "LOOP-APPEND", 11, 0x20E3DEC9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_NCONC, SYSTEM, "LOOP-NCONC", 10, 0x1F92E0D1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_COUNT, SYSTEM, "LOOP-COUNT", 10, 0x259EE6D1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_SUM, SYSTEM, "LOOP-SUM", 8, 0x1DA4A281ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_MAXIMIZE, SYSTEM, "LOOP-MAXIMIZE", 13, 0x02D9EA14ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_MINIMIZE, SYSTEM, "LOOP-MINIMIZE", 13, 0x78E1EA14ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_WHILE, SYSTEM, "LOOP-WHILE", 10, 0x1997EBCFULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_UNTIL, SYSTEM, "LOOP-UNTIL", 10, 0x249DF0CCULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_REPEAT, SYSTEM, "LOOP-REPEAT", 11, 0x20E8E2C9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_ALWAYS, SYSTEM, "LOOP-ALWAYS", 11, 0x27EEE9C5ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_NEVER, SYSTEM, "LOOP-NEVER", 10, 0x2694EFC8ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_THEREIS, SYSTEM, "LOOP-THEREIS", 12, 0x68E0E8D7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_NEXT_LOOP, SYSTEM, "NEXT-LOOP", 9, 0x23A791D4ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_END_LOOP, SYSTEM, "END-LOOP", 8, 0x7D939D99ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_VALUE_LOOP, SYSTEM, "VALUE-LOOP", 10, 0x2498BEF4ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FUNCTION_LOOP, SYSTEM, "FUNCTION-LOOP", 13, 0x60ECEB24ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_IT_LOOP, SYSTEM, "IT-LOOP", 7, 0x4C7DA39FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_STRUCTURE_GENSYM, SYSTEM, "STRUCTURE-GENSYM", 16, 0x3A482939ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_STRUCTURE_NAMED, SYSTEM, "STRUCTURE-NAMED", 15, 0x693A1B37ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CACHE, SYSTEM, "CACHE", 5, 0x4843418DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_DOCUMENTATION, SYSTEM, "TYPE-DOCUMENTATION", 18, 0x20397A8BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_COMPILER_MACRO_FUNCTION, SYSTEM, "COMPILER-MACRO-FUNCTION", 23, 0x0092D5C0ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SETF_COMPILER_MACRO_FUNCTION, SYSTEM, "SETF-COMPILER-MACRO-FUNCTION", 28, 0x5929F01BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_COMPILE_WARNING, SYSTEM, "COMPILE-WARNING", 15, 0x4C2C2B3BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_COMPILE_STYLE_WARNING, SYSTEM, "COMPILE-STYLE-WARNING", 21, 0x598C6BD2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_NTH_VALUE, SYSTEM, "NTH-VALUE", 9, 0x029495F2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_OPTIMIZE_CHECK, SYSTEM, "OPTIMIZE-CHECK", 14, 0x53F7281AULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_CAST_SINGLE_FLOAT, SYSTEM, "CAST-SINGLE-FLOAT", 17, 0x11312D62ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CAST_DOUBLE_FLOAT, SYSTEM, "CAST-DOUBLE-FLOAT", 17, 0x18371E5DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CAST_LONG_FLOAT, SYSTEM, "CAST-LONG-FLOAT", 15, 0x6F3CFC15ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BYTESPEC, SYSTEM, "BYTESPEC", 8, 0x0899A99DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_SYMBOL, SYSTEM, "TYPE-SYMBOL", 11, 0x12F5FBCEULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TYPE_LIST, SYSTEM, "TYPE-LIST", 9, 0x1899A5DEULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UNIX, SYSTEM, "UNIX", 4, 0x58494E59ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_WINDOWS, SYSTEM, "WINDOWS", 7, 0x44A1A0ADULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UNIVERSAL, SYSTEM, "UNIVERSAL", 9, 0x179CA0EFULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEVICE, SYSTEM, "DEVICE", 6, 0x49568A8DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOGICAL_PATHNAME, SYSTEM, "LOGICAL-PATHNAME", 16, 0x0435133DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TIME1970, SYSTEM, "TIME1970", 8, 0x7584828DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ASCII, SYSTEM, "ASCII", 5, 0x4943538FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_8, SYSTEM, "UTF-8", 5, 0x2D465492ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_8_BOM, SYSTEM, "UTF-8-BOM", 9, 0x7C8881E3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_16, SYSTEM, "UTF-16", 6, 0x2D468A8CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_16LE, SYSTEM, "UTF-16LE", 8, 0x72928A8EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_16BE, SYSTEM, "UTF-16BE", 8, 0x72888A8EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_16LE_BOM, SYSTEM, "UTF-16LE-BOM", 12, 0x3FE1CCBFULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_16BE_BOM, SYSTEM, "UTF-16BE-BOM", 12, 0x3FD7CCBFULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_32, SYSTEM, "UTF-32", 6, 0x2D46868EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_32LE, SYSTEM, "UTF-32LE", 8, 0x72928690ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_32BE, SYSTEM, "UTF-32BE", 8, 0x72888690ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_32LE_BOM, SYSTEM, "UTF-32LE-BOM", 12, 0x3FE1C8C1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UTF_32BE_BOM, SYSTEM, "UTF-32BE-BOM", 12, 0x3FD7C8C1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CR, SYSTEM, "CR", 2, 0x00005245ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LF, SYSTEM, "LF", 2, 0x0000464EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CRLF, SYSTEM, "CRLF", 4, 0x464C5247ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_AUTO, SYSTEM, "AUTO", 4, 0x4F545545ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CLOSE_ABORT, SYSTEM, "CLOSE-ABORT", 11, 0x15E4CBE2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PRINT_DISPATCH, SYSTEM, "PRINT-DISPATCH", 14, 0x6BCF1848ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_N, SYSTEM, "N", 1, 0x0000004FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_A, SYSTEM, "A", 1, 0x00000042ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_H, SYSTEM, "H", 1, 0x00000049ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_W, SYSTEM, "W", 1, 0x00000058ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_F, SYSTEM, "F", 1, 0x00000047ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_NA, SYSTEM, "NA", 2, 0x00004150ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DISPATCH_VECTOR, SYSTEM, "DISPATCH-VECTOR", 15, 0x5C2E4315ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DISPATCH_QUOTE, SYSTEM, "DISPATCH-QUOTE", 14, 0x67EC3414ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DISPATCH_CALL, SYSTEM, "DISPATCH-CALL", 13, 0x64D7E10BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DISPATCH_DEFUN, SYSTEM, "DISPATCH-DEFUN", 14, 0x5EDC3015ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DISPATCH_LET, SYSTEM, "DISPATCH-LET", 12, 0x6CDBE9BEULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DELAY_WARNING, SYSTEM, "DELAY-WARNING", 13, 0x50ECC143ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DOC_TYPE, SYSTEM, "DOC-TYPE", 8, 0x7293A8A0ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_OBJECT, SYSTEM, "OBJECT", 6, 0x454A9698ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_STEP, SYSTEM, "STEP", 4, 0x50455457ULL, 0, 0, 0 },
{ CONSTANT_CODE_NOP, CODE, "NOP", 3, 0x00504F51ULL, 0, 0, 0 },
{ CONSTANT_CODE_BEGIN, CODE, "BEGIN", 5, 0x49474595ULL, 0, 0, 0 },
{ CONSTANT_CODE_BEGIN_CALL, CODE, "BEGIN-CALL", 10, 0x0A8ABEE6ULL, 0, 0, 0 },
{ CONSTANT_CODE_END, CODE, "END", 3, 0x00444E48ULL, 0, 0, 0 },
{ CONSTANT_CODE_ESCAPE, CODE, "ESCAPE", 6, 0x4143989BULL, 0, 0, 0 },
{ CONSTANT_CODE_ESCAPE_NOT, CODE, "ESCAPE-NOT", 10, 0x0F70ECEEULL, 0, 0, 0 },
{ CONSTANT_CODE_SAVE, CODE, "SAVE", 4, 0x45564157ULL, 0, 0, 0 },
{ CONSTANT_CODE_RESTORE, CODE, "RESTORE", 7, 0x549897A8ULL, 0, 0, 0 },
{ CONSTANT_CODE_NORMAL, CODE, "NORMAL", 6, 0x4D529B95ULL, 0, 0, 0 },
{ CONSTANT_CODE_REVERT, CODE, "REVERT", 6, 0x455699AAULL, 0, 0, 0 },
{ CONSTANT_CODE_REVERT_GOTO, CODE, "REVERT-GOTO", 11, 0x0CD2EDFEULL, 0, 0, 0 },
{ CONSTANT_CODE_SET, CODE, "SET", 3, 0x00544556ULL, 0, 0, 0 },
{ CONSTANT_CODE_PUSH, CODE, "PUSH", 4, 0x48535554ULL, 0, 0, 0 },
{ CONSTANT_CODE_PUSH_RESULT, CODE, "PUSH-RESULT", 11, 0x1BECF3DDULL, 0, 0, 0 },
{ CONSTANT_CODE_PUSH_VALUES, CODE, "PUSH-VALUES", 11, 0x14E7F0DDULL, 0, 0, 0 },
{ CONSTANT_CODE_NIL_SET, CODE, "NIL-SET", 7, 0x2DA08EA8ULL, 0, 0, 0 },
{ CONSTANT_CODE_NIL_PUSH, CODE, "NIL-PUSH", 8, 0x759F9EA6ULL, 0, 0, 0 },
{ CONSTANT_CODE_T_SET, CODE, "T-SET", 5, 0x45532DADULL, 0, 0, 0 },
{ CONSTANT_CODE_T_PUSH, CODE, "T-PUSH", 6, 0x555075ADULL, 0, 0, 0 },
{ CONSTANT_CODE_LEXICAL, CODE, "LEXICAL", 7, 0x49A48696ULL, 0, 0, 0 },
{ CONSTANT_CODE_LEXICAL_SET, CODE, "LEXICAL-SET", 11, 0x76F8CBEDULL, 0, 0, 0 },
{ CONSTANT_CODE_LEXICAL_PUSH, CODE, "LEXICAL-PUSH", 12, 0x3EF7DBEBULL, 0, 0, 0 },
{ CONSTANT_CODE_LEXICAL_REM, CODE, "LEXICAL-REM", 11, 0x76F1CBECULL, 0, 0, 0 },
{ CONSTANT_CODE_LEXICAL_FREE, CODE, "LEXICAL-FREE", 12, 0x3BE9D8E1ULL, 0, 0, 0 },
{ CONSTANT_CODE_SPECIAL, CODE, "SPECIAL", 7, 0x439191A3ULL, 0, 0, 0 },
{ CONSTANT_CODE_SPECIAL_SET, CODE, "SPECIAL-SET", 11, 0x70E5D6FAULL, 0, 0, 0 },
{ CONSTANT_CODE_SPECIAL_PUSH, CODE, "SPECIAL-PUSH", 12, 0x38E4E6F8ULL, 0, 0, 0 },
{ CONSTANT_CODE_SPECIAL_REM, CODE, "SPECIAL-REM", 11, 0x70DED6F9ULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_SPECIAL, CODE, "DECLAIM-SPECIAL", 15, 0x3D222030ULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_TYPE_VALUE, CODE, "DECLAIM-TYPE-VALUE", 18, 0x0B22836DULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_TYPE_FUNCTION, CODE, "DECLAIM-TYPE-FUNCTION", 21, 0x5C7F82ACULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_INLINE, CODE, "DECLAIM-INLINE", 14, 0x42DD222AULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_NOTINLINE, CODE, "DECLAIM-NOTINLINE", 17, 0x112E2A77ULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_COMPILATION, CODE, "DECLAIM-COMPILATION", 19, 0x1E6D796DULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_DEBUG, CODE, "DECLAIM-DEBUG", 13, 0x4ED2D41DULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_SAFETY, CODE, "DECLAIM-SAFETY", 14, 0x3ED7293AULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_SPACE, CODE, "DECLAIM-SPACE", 13, 0x3CD1DF2AULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_SPEED, CODE, "DECLAIM-SPEED", 13, 0x3ED5DF29ULL, 0, 0, 0 },
{ CONSTANT_CODE_DECLAIM_DECLARATION, CODE, "DECLAIM-DECLARATION", 19, 0x1A637566ULL, 0, 0, 0 },
{ CONSTANT_CODE_TYPE_RESULT, CODE, "TYPE-RESULT", 11, 0x18E9F7E1ULL, 0, 0, 0 },
{ CONSTANT_CODE_TYPE_LEXICAL, CODE, "TYPE-LEXICAL", 12, 0x69D6E8D6ULL, 0, 0, 0 },
{ CONSTANT_CODE_TYPE_SPECIAL, CODE, "TYPE-SPECIAL", 12, 0x56E1F5D0ULL, 0, 0, 0 },
{ CONSTANT_CODE_TYPE_GLOBAL, CODE, "TYPE-GLOBAL", 11, 0x14E8E1CEULL, 0, 0, 0 },
{ CONSTANT_CODE_TYPE_FUNCTION, CODE, "TYPE-FUNCTION", 13, 0x62EEF41FULL, 0, 0, 0 },
{ CONSTANT_CODE_TYPE_SETF, CODE, "TYPE-SETF", 9, 0x1995ACD0ULL, 0, 0, 0 },
{ CONSTANT_CODE_LET_LEXICAL, CODE, "LET-LEXICAL", 11, 0x76F8CBE6ULL, 0, 0, 0 },
{ CONSTANT_CODE_LET_SPECIAL, CODE, "LET-SPECIAL", 11, 0x70E5D6F3ULL, 0, 0, 0 },
{ CONSTANT_CODE_LETA_SPECIAL, CODE, "LET*-SPECIAL", 12, 0x3BE5E1C8ULL, 0, 0, 0 },
{ CONSTANT_CODE_SETQ_LEXICAL, CODE, "SETQ-LEXICAL", 12, 0x75DAD4D5ULL, 0, 0, 0 },
{ CONSTANT_CODE_SETQ_SPECIAL, CODE, "SETQ-SPECIAL", 12, 0x62E5E1CFULL, 0, 0, 0 },
{ CONSTANT_CODE_SETQ_GLOBAL, CODE, "SETQ-GLOBAL", 11, 0x20ECCDCDULL, 0, 0, 0 },
{ CONSTANT_CODE_FUNCTION_SET, CODE, "FUNCTION-SET", 12, 0x65E2F1D3ULL, 0, 0, 0 },
{ CONSTANT_CODE_FUNCTION_PUSH, CODE, "FUNCTION-PUSH", 13, 0x64F2EF1CULL, 0, 0, 0 },
{ CONSTANT_CODE_SETF_SET, CODE, "SETF-SET", 8, 0x1A999888ULL, 0, 0, 0 },
{ CONSTANT_CODE_SETF_PUSH, CODE, "SETF-PUSH", 9, 0x19A995D1ULL, 0, 0, 0 },
{ CONSTANT_CODE_DEFMACRO, CODE, "DEFMACRO", 8, 0x1C98888DULL, 0, 0, 0 },
{ CONSTANT_CODE_DEFTYPE, CODE, "DEFTYPE", 7, 0x548B95A4ULL, 0, 0, 0 },
{ CONSTANT_CODE_DEFINE_COMPILER_MACRO, CODE, "DEFINE-COMPILER-MACRO", 21, 0x55595EDEULL, 0, 0, 0 },
{ CONSTANT_CODE_DEFUN, CODE, "DEFUN", 5, 0x55464597ULL, 0, 0, 0 },
{ CONSTANT_CODE_CALL_NAME, CODE, "CALL-NAME", 9, 0x198D8FBEULL, 0, 0, 0 },
{ CONSTANT_CODE_CALL_RESULT, CODE, "CALL-RESULT", 11, 0x1FE5DFD0ULL, 0, 0, 0 },
{ CONSTANT_CODE_CALL_TYPE, CODE, "CALL-TYPE", 9, 0x1CA595BEULL, 0, 0, 0 },
{ CONSTANT_CODE_CALL_KEY, CODE, "CALL-KEY", 8, 0x25918C78ULL, 0, 0, 0 },
{ CONSTANT_CODE_CALL_FUNCTION, CODE, "CALL-FUNCTION", 13, 0x69EADC0EULL, 0, 0, 0 },
{ CONSTANT_CODE_CALL_SETF, CODE, "CALL-SETF", 9, 0x209194BFULL, 0, 0, 0 },
{ CONSTANT_CODE_CALL_LEXICAL, CODE, "CALL-LEXICAL", 12, 0x70D2D0C5ULL, 0, 0, 0 },
{ CONSTANT_CODE_VALUES_NIL, CODE, "VALUES-NIL", 10, 0x2379E0EEULL, 0, 0, 0 },
{ CONSTANT_CODE_VALUES_SET, CODE, "VALUES-SET", 10, 0x2879E8EAULL, 0, 0, 0 },
{ CONSTANT_CODE_THE_SET, CODE, "THE-SET", 7, 0x2D998DAEULL, 0, 0, 0 },
{ CONSTANT_CODE_THE_PUSH, CODE, "THE-PUSH", 8, 0x75989DACULL, 0, 0, 0 },
{ CONSTANT_CODE_IF_UNBOUND, CODE, "IF-UNBOUND", 10, 0x2A7CCCEFULL, 0, 0, 0 },
{ CONSTANT_CODE_IF_NIL, CODE, "IF-NIL", 6, 0x4E2D9298ULL, 0, 0, 0 },
{ CONSTANT_CODE_IF_T, CODE, "IF-T", 4, 0x542D464DULL, 0, 0, 0 },
{ CONSTANT_CODE_GOTO, CODE, "GOTO", 4, 0x4F544F4BULL, 0, 0, 0 },
{ CONSTANT_CODE_GO, CODE, "GO", 2, 0x00004F49ULL, 0, 0, 0 },
{ CONSTANT_CODE_RETURN_FROM, CODE, "RETURN-FROM", 11, 0x1BCEE301ULL, 0, 0, 0 },
{ CONSTANT_CODE_CATCH, CODE, "CATCH", 5, 0x43544190ULL, 0, 0, 0 },
{ CONSTANT_CODE_THROW, CODE, "THROW", 5, 0x4F5248B0ULL, 0, 0, 0 },
{ CONSTANT_CODE_TAG, CODE, "TAG", 3, 0x00474157ULL, 0, 0, 0 },
{ CONSTANT_CODE_TAGINFO, CODE, "TAGINFO", 7, 0x499687A9ULL, 0, 0, 0 },
{ CONSTANT_CODE_BLOCKINFO, CODE, "BLOCKINFO", 9, 0x099D95E5ULL, 0, 0, 0 },
{ CONSTANT_CODE_HANDLER_BIND, CODE, "HANDLER-BIND", 12, 0x35EECFE2ULL, 0, 0, 0 },
{ CONSTANT_CODE_HANDLER_CASE, CODE, "HANDLER-CASE", 12, 0x36F3C7E3ULL, 0, 0, 0 },
{ CONSTANT_CODE_RESTART_BIND, CODE, "RESTART-BIND", 12, 0x45F5E0E1ULL, 0, 0, 0 },
{ CONSTANT_CODE_RESTART_CASE, CODE, "RESTART-CASE", 12, 0x46FAD8E2ULL, 0, 0, 0 },
{ CONSTANT_CODE_RESTART_PROGN, CODE, "RESTART-PROGN", 13, 0x48F6EA3EULL, 0, 0, 0 },
{ CONSTANT_CODE_FUNCALL, CODE, "FUNCALL", 7, 0x439AA18EULL, 0, 0, 0 },
{ CONSTANT_CODE_NTH_VALUE, CODE, "NTH-VALUE", 9, 0x029495F2ULL, 0, 0, 0 },
{ CONSTANT_CODE_PROGV, CODE, "PROGV", 5, 0x474F52ABULL, 0, 0, 0 },
{ CONSTANT_CODE_POP, CODE, "POP", 3, 0x00504F53ULL, 0, 0, 0 },
{ CONSTANT_CODE_POP_UNBOUND, CODE, "POP-UNBOUND", 11, 0x7CD6EC05ULL, 0, 0, 0 },
{ CONSTANT_CODE_GETF, CODE, "GETF", 4, 0x4654454BULL, 0, 0, 0 },
{ CONSTANT_CODE_REST_COPY, CODE, "REST_COPY", 9, 0x24A28913ULL, 0, 0, 0 },
{ CONSTANT_CODE_REST_BIND, CODE, "REST_BIND", 9, 0x229C87FEULL, 0, 0, 0 },
{ CONSTANT_CODE_ALLOW_OTHER_KEYS, CODE, "ALLOW-OTHER-KEYS", 16, 0x2447043BULL, 0, 0, 0 },
{ CONSTANT_CODE_REST_NULL, CODE, "REST-NULL", 9, 0x20A893D4ULL, 0, 0, 0 },
{ CONSTANT_CODE_WHOLE, CODE, "WHOLE", 5, 0x4C4F48A1ULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA, CODE, "LAMBDA", 6, 0x424D8296ULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_NAME, CODE, "LAMBDA-NAME", 11, 0x10BFCFDCULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_TYPE, CODE, "LAMBDA-TYPE", 11, 0x16BFD2F4ULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_DOC, CODE, "LAMBDA-DOC", 10, 0x067AC5E9ULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_FORM, CODE, "LAMBDA-FORM", 11, 0x08C7D4EAULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_DEFUN, CODE, "LAMBDA-DEFUN", 12, 0x54CFC8E1ULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_CLOSURE, CODE, "LAMBDA-CLOSURE", 14, 0x5ACE173CULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_LEXICAL, CODE, "LAMBDA-LEXICAL", 14, 0x51C42724ULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_CACHE, CODE, "LAMBDA-CACHE", 12, 0x4AC2C5DDULL, 0, 0, 0 },
{ CONSTANT_CODE_LAMBDA_CACHE_SET, CODE, "LAMBDA-CACHE-SET", 16, 0x1F08190EULL, 0, 0, 0 },
{ CONSTANT_CODE_MACRO, CODE, "MACRO", 5, 0x524341A1ULL, 0, 0, 0 },
{ CONSTANT_CODE_MACRO_SPECIAL, CODE, "MACRO-SPECIAL", 13, 0x63DFB23AULL, 0, 0, 0 },
{ CONSTANT_CODE_MACRO_ENV, CODE, "MACRO-ENV", 9, 0x20886EFBULL, 0, 0, 0 },
{ CONSTANT_CODE_MACRO_WHOLE, CODE, "MACRO-WHOLE", 11, 0x1ADFBAF6ULL, 0, 0, 0 },
{ CONSTANT_CODE_LABELS_MAKE, CODE, "LABELS-MAKE", 11, 0x12B4DFE4ULL, 0, 0, 0 },
{ CONSTANT_CODE_LABELS_LAMBDA, CODE, "LABELS-LAMBDA", 13, 0x55B1E227ULL, 0, 0, 0 },
{ CONSTANT_CODE_BIND1_TYPE, CODE, "BIND1-TYPE", 10, 0x1DA2BBCDULL, 0, 0, 0 },
{ CONSTANT_CODE_BIND1_SPECIAL, CODE, "BIND1-SPECIAL", 13, 0x55EABA11ULL, 0, 0, 0 },
{ CONSTANT_CODE_BIND1_LEXICAL, CODE, "BIND1-LEXICAL", 13, 0x4ADDC024ULL, 0, 0, 0 },
{ CONSTANT_CODE_BIND2_TYPE, CODE, "BIND2-TYPE", 10, 0x1DA2BBCEULL, 0, 0, 0 },
{ CONSTANT_CODE_BIND2_SPECIAL, CODE, "BIND2-SPECIAL", 13, 0x55EABA12ULL, 0, 0, 0 },
{ CONSTANT_CODE_BIND2_LEXICAL, CODE, "BIND2-LEXICAL", 13, 0x4ADDC025ULL, 0, 0, 0 },
{ CONSTANT_CODE_LOAD_ALLOC, CODE, "LOAD-ALLOC", 10, 0x108DD3D2ULL, 0, 0, 0 },
{ CONSTANT_CODE_LOAD_GENSYM, CODE, "LOAD-GENSYM", 11, 0x12D3EFD7ULL, 0, 0, 0 },
{ CONSTANT_CODE_LOAD_SET, CODE, "LOAD-SET", 8, 0x1886A281ULL, 0, 0, 0 },
{ CONSTANT_CODE_REFERENCE_SET, CODE, "REFERENCE-SET", 13, 0x4DE7B84AULL, 0, 0, 0 },
{ CONSTANT_CODE_REFERENCE_PUSH, CODE, "REFERENCE-PUSH", 14, 0x5DE5004AULL, 0, 0, 0 },
{ CONSTANT_CODE_STEP, CODE, "STEP", 4, 0x50455457ULL, 0, 0, 0 },
{ CONSTANT_CODE_STEP_OFF, CODE, "STEP-OFF", 8, 0x168BA388ULL, 0, 0, 0 },
{ CONSTANT_CODE_STEP_BEGIN, CODE, "STEP-BEGIN", 10, 0x178AE4D3ULL, 0, 0, 0 },
{ CONSTANT_CODE_STEP_END, CODE, "STEP-END", 8, 0x14939988ULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_RESULT_TYPE, CODE, "OPTCODE-RESULT-TYPE", 19, 0x1A5F7EA8ULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CAR0_SET, CODE, "OPTCODE-CAR0-SET", 16, 0x7531291EULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CAR0_PUSH, CODE, "OPTCODE-CAR0-PUSH", 17, 0x74412667ULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CAR1_SET, CODE, "OPTCODE-CAR1-SET", 16, 0x7631291EULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CAR1_PUSH, CODE, "OPTCODE-CAR1-PUSH", 17, 0x75412667ULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CDR0_SET, CODE, "OPTCODE-CDR0-SET", 16, 0x75312C1EULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CDR0_PUSH, CODE, "OPTCODE-CDR0-PUSH", 17, 0x74412967ULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CDR1_SET, CODE, "OPTCODE-CDR1-SET", 16, 0x76312C1EULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CDR1_PUSH, CODE, "OPTCODE-CDR1-PUSH", 17, 0x75412967ULL, 0, 0, 0 },
{ CONSTANT_CODE_OPTCODE_CONS, CODE, "OPTCODE-CONS", 12, 0x43E7E3EDULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METAOBJECT, CLOS, "METAOBJECT", 10, 0x069EDBE9ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SPECIALIZER, CLOS, "SPECIALIZER", 11, 0x0CE3D701ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_EQL_SPECIALIZER, CLOS, "EQL-SPECIALIZER", 15, 0x3A30284AULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FORWARD_REFERENCED_CLASS, CLOS, "FORWARD-REFERENCED-CLASS", 24, 0x23ABB1D4ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FUNCALLABLE_STANDARD_OBJECT, CLOS, "FUNCALLABLE-STANDARD-OBJECT", 27, 0x0F0A15EDULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FUNCALLABLE_STANDARD_CLASS, CLOS, "FUNCALLABLE-STANDARD-CLASS", 26, 0x05C019FAULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_STANDARD_READER_METHOD, CLOS, "STANDARD-READER-METHOD", 22, 0x497FB6BAULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_STANDARD_WRITER_METHOD, CLOS, "STANDARD-WRITER-METHOD", 22, 0x518CBBCAULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_STANDARD_ACCESSOR_METHOD, CLOS, "STANDARD-ACCESSOR-METHOD", 24, 0x2EC69FC7ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SLOT_DEFINITION, CLOS, "SLOT-DEFINITION", 15, 0x6F2C2E21ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DIRECT_SLOT_DEFINITION, CLOS, "DIRECT-SLOT-DEFINITION", 22, 0x586EC9CAULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_EFFECTIVE_SLOT_DEFINITION, CLOS, "EFFECTIVE-SLOT-DEFINITION", 25, 0x49A2B711ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_STANDARD_SLOT_DEFINITION, CLOS, "STANDARD-SLOT-DEFINITION", 24, 0x3EC1A8CAULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_STANDARD_DIRECT_SLOT_DEFINITION, CLOS, "STANDARD-DIRECT-SLOT-DEFINITION", 31, 0x015D4B3AULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_STANDARD_EFFECTIVE_SLOT_DEFINITION, CLOS, "STANDARD-EFFECTIVE-SLOT-DEFINITION", 34, 0x354A8FAEULL, 0, 0, 0 },
{ CONSTANT_COMMON_ALLOCATE_INSTANCE, CLOS, "ALLOCATE-INSTANCE", 17, 0x2B3D185BULL, 0, 0, 0 },
{ CONSTANT_COMMON_INITIALIZE_INSTANCE, CLOS, "INITIALIZE-INSTANCE", 19, 0x285C6B9BULL, 0, 0, 0 },
{ CONSTANT_COMMON_REINITIALIZE_INSTANCE, CLOS, "REINITIALIZE-INSTANCE", 21, 0x6B896DC2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SHARED_INITIALIZE, CLOS, "SHARED-INITIALIZE", 17, 0x3F0C227DULL, 0, 0, 0 },
{ CONSTANT_COMMON_ENSURE_GENERIC_FUNCTION, CLOS, "ENSURE-GENERIC-FUNCTION", 23, 0x0984C2DAULL, 0, 0, 0 },
{ CONSTANT_COMMON_MAKE_INSTANCE, CLOS, "MAKE-INSTANCE", 13, 0x5BE7CC20ULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLOT_MISSING, CLOS, "SLOT-MISSING", 12, 0x6EE6E2DFULL, 0, 0, 0 },
{ CONSTANT_COMMON_SLOT_UNBOUND, CLOS, "SLOT-UNBOUND", 12, 0x5AEBF6DBULL, 0, 0, 0 },
{ CONSTANT_COMMON_CHANGE_CLASS, CLOS, "CHANGE-CLASS", 12, 0x64C1CEE2ULL, 0, 0, 0 },
{ CONSTANT_COMMON_FUNCTION_KEYWORDS, CLOS, "FUNCTION-KEYWORDS", 17, 0x2F353982ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_REDEFINED, CLOS, "REDEFINED", 9, 0x0A928EE5ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_REFERENCED_CLASS, CLOS, "REFERENCED-CLASS", 16, 0x1F151045ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ENSURE_CLASS, CLOS, "ENSURE-CLASS", 12, 0x6BD3D4EFULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_ENSURE_CLASS_USING_CLASS, CLOS, "ENSURE-CLASS-USING-CLASS", 24, 0x4BA7B2C2ULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_ENSURE_GENERIC_FUNCTION_USING_CLASS, CLOS, "ENSURE-GENERIC-FUNCTION-USING-CLASS", 35, 0x516496C3ULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_ENSURE_METHOD, CLOS, "ENSURE-METHOD", 13, 0x71C8E82DULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_SLOT_BOUNDP_USING_CLASS, CLOS, "SLOT-BOUNDP-USING-CLASS", 23, 0x71CEA6C2ULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_SLOT_EXISTS_P_USING_CLASS, CLOS, "SLOT-EXISTS-P-USING-CLASS", 25, 0x1ED8AE1BULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_SLOT_MAKUNBOUND_USING_CLASS, CLOS, "SLOT-MAKUNBOUND-USING-CLASS", 27, 0x36F70A22ULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_SLOT_VALUE_USING_CLASS, CLOS, "SLOT-VALUE-USING-CLASS", 22, 0x7E58C7BEULL, 0, 1, 0 },
{ CONSTANT_CLOSNAME_FLET_METHOD_P, CLOS, "FLET-METHOD-P", 13, 0x55CEE918ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FLET_NEXT_METHOD, CLOS, "FLET-NEXT-METHOD", 16, 0x3627102BULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DEFINE_METHOD_COMBINATION, CLOS, "DEFINE-METHOD-COMBINATION", 25, 0x5291A310ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DEFINE_LONG_METHOD_COMBINATION, CLOS, "DEFINE-LONG-METHOD-COMBINATION", 30, 0x60D33F87ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_DEFINE_SHORT_METHOD_COMBINATION, CLOS, "DEFINE-SHORT-METHOD-COMBINATION", 31, 0x092C6343ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_LONG_METHOD_COMBINATION, CLOS, "LONG-METHOD-COMBINATION", 23, 0x5FB4D8ADULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SHORT_METHOD_COMBINATION, CLOS, "SHORT-METHOD-COMBINATION", 24, 0x38D893D6ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHOD_COMBINATION_INSTANCE, CLOS, "METHOD-COMBINATION-INSTANCE", 27, 0x28DEFD3FULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ENSURE_METHOD_COMBINATION_SHORT, CLOS, "ENSURE-METHOD-COMBINATION-SHORT", 31, 0x27462B6AULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ENSURE_METHOD_COMBINATION_LONG, CLOS, "ENSURE-METHOD-COMBINATION-LONG", 30, 0x2DEB2068ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_QUALIFIERS_ELT, CLOS, "QUALIFIERS-ELT", 14, 0x56B84346ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_COMBINATION_BINDING, CLOS, "COMBINATION-BINDING", 19, 0x08728473ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_MACRO_MAKE_METHOD, CLOS, "MACRO-MAKE-METHOD", 17, 0x30060881ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_MACRO_CALL_METHOD, CLOS, "MACRO-CALL-METHOD", 17, 0x2FFC0F82ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_MACRO_METHOD_LAMBDA, CLOS, "MACRO-METHOD-LAMBDA", 19, 0x29624772ULL, 0, 0, 0 },
{ CONSTANT_COMMON_CLASS_NAME, CLOS, "CLASS-NAME", 10, 0x148FBEEDULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_SLOTS, CLOS, "CLASS-SLOTS", 11, 0x1FE7CDF0ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_DIRECT_SLOTS, CLOS, "CLASS-DIRECT-SLOTS", 18, 0x4015657BULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_DEFAULT_INITARGS, CLOS, "CLASS-DEFAULT-INITARGS", 22, 0x05658FD6ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_DIRECT_DEFAULT_INITARGS, CLOS, "CLASS-DIRECT-DEFAULT-INITARGS", 29, 0x1BFAE347ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_DIRECT_SUPERCLASSES, CLOS, "CLASS-DIRECT-SUPERCLASSES", 25, 0x52B4B807ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_DIRECT_SUBCLASSES, CLOS, "CLASS-DIRECT-SUBCLASSES", 23, 0x06B2A3C2ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_FINALIZED_P, CLOS, "CLASS-FINALIZED-P", 17, 0x1318009FULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_CLASS_PROTOTYPE, CLOS, "CLASS-PROTOTYPE", 15, 0x7A261E4DULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SLOT_DEFINITION_NAME, CLOS, "SLOT-DEFINITION-NAME", 20, 0x61796F74ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SLOT_DEFINITION_TYPE, CLOS, "SLOT-DEFINITION-TYPE", 20, 0x617C877AULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SLOT_DEFINITION_ALLOCATION, CLOS, "SLOT-DEFINITION-ALLOCATION", 26, 0x34CD09FFULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SLOT_DEFINITION_INITARGS, CLOS, "SLOT-DEFINITION-INITARGS", 24, 0x43BCCEB4ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SLOT_DEFINITION_INITFORM, CLOS, "SLOT-DEFINITION-INITFORM", 24, 0x3DC7CBB9ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_SLOT_DEFINITION_INITFUNCTION, CLOS, "SLOT-DEFINITION-INITFUNCTION", 28, 0x02131B11ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION_NAME, CLOS, "GENERIC-FUNCTION-NAME", 21, 0x51707BBAULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION_METHODS, CLOS, "GENERIC-FUNCTION-METHODS", 24, 0x2BB8C9C0ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION_LAMBDA_LIST, CLOS, "GENERIC-FUNCTION-LAMBDA-LIST", 28, 0x5305070AULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION_ARGUMENT_PRECEDENCE_ORDER, CLOS, "GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER", 42, 0x39F21C2FULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION_DECLARATIONS, CLOS, "GENERIC-FUNCTION-DECLARATIONS", 29, 0x5715FC70ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION_METHOD_CLASS, CLOS, "GENERIC-FUNCTION-METHOD-CLASS", 29, 0x58FA165BULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_GENERIC_FUNCTION_METHOD_COMBINATION, CLOS, "GENERIC-FUNCTION-METHOD-COMBINATION", 35, 0x1C95B6A0ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHOD_FUNCTION, CLOS, "METHOD-FUNCTION", 15, 0x63132749ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHOD_GENERIC_FUNCTION, CLOS, "METHOD-GENERIC-FUNCTION", 23, 0x7C85B8DFULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHOD_LAMBDA_LIST, CLOS, "METHOD-LAMBDA-LIST", 18, 0x22105883ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_METHOD_SPECIALIZERS, CLOS, "METHOD-SPECIALIZERS", 19, 0x3F616D85ULL, 0, 0, 0 },
{ CONSTANT_COMMON_METHOD_QUALIFIERS, CLOS, "METHOD-QUALIFIERS", 17, 0x3513149BULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_ACCESSOR_METHOD_SLOT_DEFINITION, CLOS, "ACCESSOR-METHOD-SLOT-DEFINITION", 31, 0x0848613AULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_MAKE_METHOD_LAMBDA, CLOS, "MAKE-METHOD-LAMBDA", 18, 0x09226064ULL, 0, 0, 0 },
{ CONSTANT_CLOSNAME_FIND_METHOD_COMBINATION, CLOS, "FIND-METHOD-COMBINATION", 23, 0x5CB4D2A7ULL, 0, 1, 0 },
{ CONSTANT_RT_ERROR, RT, "ERROR", 5, 0x4F52529CULL, 0, 0, 0 },
{ CONSTANT_RT_PUSH_ENTRIES, RT, "PUSH-ENTRIES", 12, 0x6FE6E3DBULL, 0, 0, 0 },
{ CONSTANT_RT_DEFTEST, RT, "DEFTEST", 7, 0x549A9890ULL, 0, 0, 0 },
{ CONSTANT_RT_DEFTEST_ERROR, RT, "DEFTEST-ERROR", 13, 0x50ECEB2DULL, 0, 0, 0 },
{ CONSTANT_RT_DEFTEST_ERROR_, RT, "DEFTEST-ERROR!", 14, 0x50ED0C2EULL, 0, 0, 0 },
{ CONSTANT_RT_DO_TESTS, RT, "DO-TESTS", 8, 0x2781A291ULL, 0, 0, 0 },
{ CONSTANT_RT_REM_ALL_TESTS, RT, "REM-ALL-TESTS", 13, 0x2EECD747ULL, 0, 0, 0 },
{ CONSTANT_RT_EQUALRT, RT, "EQUALRT", 7, 0x41A9A398ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INPUT, SYSTEM, "INPUT", 5, 0x55504EA2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PROCESS, SYSTEM, "PROCESS", 7, 0x43A2A59CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_READTABLE_DOT, SYSTEM, "READTABLE-DOT", 13, 0x5FC7B44CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DOUBLE_QUOTE_READER, SYSTEM, "DOUBLE-QUOTE-READER", 19, 0x1A6E7B69ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SINGLE_QUOTE_READER, SYSTEM, "SINGLE-QUOTE-READER", 19, 0x1F677578ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PARENSIS_OPEN_READER, SYSTEM, "PARENSIS-OPEN-READER", 20, 0x7583556EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PARENSIS_CLOSE_READER, SYSTEM, "PARENSIS-CLOSE-READER", 21, 0x7F595ECAULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SEMICOLON_READER, SYSTEM, "SEMICOLON-READER", 16, 0x30310635ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BACKQUOTE_READER, SYSTEM, "BACKQUOTE-READER", 16, 0x372A0829ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_COMMA_READER, SYSTEM, "COMMA-READER", 12, 0x64E4C0D1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SHARP_READER, SYSTEM, "SHARP-READER", 12, 0x69D8B9F0ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DISPATCH_FUNCTION, SYSTEM, "DISPATCH-FUNCTION", 17, 0x36353854ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ERROR_DISPATCH, SYSTEM, "ERROR-DISPATCH", 14, 0x6CD8183BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_EQUAL_DISPATCH, SYSTEM, "EQUAL-DISPATCH", 14, 0x5EDB1735ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SHARP_DISPATCH, SYSTEM, "SHARP-DISPATCH", 14, 0x6FC70E47ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SINGLE_QUOTE_DISPATCH, SYSTEM, "SINGLE-QUOTE-DISPATCH", 21, 0x746D63CEULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PARENSIS_OPEN_DISPATCH, SYSTEM, "PARENSIS-OPEN-DISPATCH", 22, 0x7B71A9C5ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PARENSIS_CLOSE_DISPATCH, SYSTEM, "PARENSIS-CLOSE-DISPATCH", 23, 0x6DADB3D2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ASTERISK_DISPATCH, SYSTEM, "ASTERISK-DISPATCH", 17, 0x27452269ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_COLON_DISPATCH, SYSTEM, "COLON-DISPATCH", 14, 0x6CD21535ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LESS_DISPATCH, SYSTEM, "LESS-DISPATCH", 13, 0x69F0CB1EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BACKSLASH_DISPATCH, SYSTEM, "BACKSLASH-DISPATCH", 18, 0x3C0A5385ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_OR_DISPATCH, SYSTEM, "OR-DISPATCH", 11, 0x05C5E8F7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PLUS_DISPATCH, SYSTEM, "PLUS-DISPATCH", 13, 0x69F2D222ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MINUS_DISPATCH, SYSTEM, "MINUS-DISPATCH", 14, 0x72D40F44ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DOT_DISPATCH, SYSTEM, "DOT-DISPATCH", 12, 0x45EAECD5ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ARRAY_DISPATCH, SYSTEM, "ARRAY-DISPATCH", 14, 0x5ED8183EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BINARY_DISPATCH, SYSTEM, "BINARY-DISPATCH", 15, 0x47143940ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_COMPLEX_DISPATCH, SYSTEM, "COMPLEX-DISPATCH", 16, 0x163C3224ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_OCTAL_DISPATCH, SYSTEM, "OCTAL-DISPATCH", 14, 0x5EDA093FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PATHNAME_DISPATCH, SYSTEM, "PATHNAME-DISPATCH", 17, 0x243F0874ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_RADIX_DISPATCH, SYSTEM, "RADIX-DISPATCH", 14, 0x66CA074EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_STRUCTURE_DISPATCH, SYSTEM, "STRUCTURE-DISPATCH", 18, 0x452D6E83ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_HEXADECIMAL_DISPATCH, SYSTEM, "HEXADECIMAL-DISPATCH", 20, 0x507E6972ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_BACKQUOTE, SYSTEM, "BACKQUOTE", 9, 0x1F9296E1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_UNBOUND_VALUE, SYSTEM, "UNBOUND-VALUE", 13, 0x51D2DE52ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INCLUDE, SYSTEM, "INCLUDE", 7, 0x4C8892A5ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_EXCLUDE, SYSTEM, "EXCLUDE", 7, 0x4C889CA1ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INVALID, SYSTEM, "INVALID", 7, 0x419A979CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FALSE, SYSTEM, "FALSE", 5, 0x534C4190ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_ATOMIC, SYSTEM, "SUBTYPEP-ATOMIC", 15, 0x741F3035ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_ATOMIC_NOT, SYSTEM, "SUBTYPEP-ATOMIC-NOT", 19, 0x21737F87ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_COMPOUND, SYSTEM, "SUBTYPEP-COMPOUND", 17, 0x402C387EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_FORCE_NUMBER, SYSTEM, "SUBTYPEP-FORCE-NUMBER", 21, 0x0A467ED8ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_NORMAL, SYSTEM, "SUBTYPEP-NORMAL", 15, 0x77233535ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TERME_INPUT, SYSTEM, "TERME-INPUT", 11, 0x1BEFC7F4ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_OUTPUT, SYSTEM, "TERME-OUTPUT", 12, 0x76F6C2F9ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_MOVE, SYSTEM, "TERME-MOVE", 10, 0x1C9FB7F9ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_CLEAR, SYSTEM, "TERME-CLEAR", 11, 0x19E7B3E9ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_DELETE, SYSTEM, "TERME-DELETE", 12, 0x57EAB7F1ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_FONT, SYSTEM, "TERME-FONT", 10, 0x1C98C6F1ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_SIZE, SYSTEM, "TERME-SIZE", 10, 0x16A5B7FDULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_SCROLL, SYSTEM, "TERME-SCROLL", 12, 0x5CF1C1F7ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_BEGIN, SYSTEM, "TERME-BEGIN", 11, 0x12E2BBEBULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_END, SYSTEM, "TERME-END", 9, 0x1B9772E6ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_ENABLE, SYSTEM, "TERME-ENABLE", 12, 0x60E3B4E6ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_SIGNAL, SYSTEM, "TERME-SIGNAL", 12, 0x62E6C0ECULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_HANG, SYSTEM, "TERME-HANG", 10, 0x0E9AB9F1ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_CODE, SYSTEM, "TERME-CODE", 10, 0x1C95B7E7ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_UP, SYSTEM, "TERME-UP", 8, 0x1DA772A1ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_DOWN, SYSTEM, "TERME-DOWN", 10, 0x1C96C0FAULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_LEFT, SYSTEM, "TERME-LEFT", 10, 0x129EC6E9ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_RIGHT, SYSTEM, "TERME-RIGHT", 11, 0x16F8BAEBULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_PAGE_UP, SYSTEM, "TERME-PAGE-UP", 13, 0x63CFB83DULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_PAGE_DOWN, SYSTEM, "TERME-PAGE-DOWN", 15, 0x531E0F3EULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_HOME, SYSTEM, "TERME-HOME", 10, 0x1C9AB7F0ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_INSERT, SYSTEM, "TERME-INSERT", 12, 0x6FEDB7F8ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_FUNCTION, SYSTEM, "TERME-FUNCTION", 14, 0x6BED0444ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME_ESCAPE, SYSTEM, "TERME-ESCAPE", 12, 0x65E7B3E8ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_FP_NAN, SYSTEM, "FP-NAN", 6, 0x4E2D9E8DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FP_INFINITE, SYSTEM, "FP-INFINITE", 11, 0x17BBEAE8ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FP_NORMAL, SYSTEM, "FP-NORMAL", 9, 0x0F7AA2EAULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FP_SUBNORMAL, SYSTEM, "FP-SUBNORMAL", 12, 0x6EBCDFF9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_FP_ZERO, SYSTEM, "FP-ZERO", 7, 0x5A7CA292ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PAPER, SYSTEM, "PAPER", 5, 0x455041A7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFINE_SYMBOL_MACRO, SYSTEM, "DEFINE-SYMBOL-MACRO", 19, 0x2D52578DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SYMBOL_MACRO_EXPANDER, SYSTEM, "SYMBOL-MACRO-EXPANDER", 21, 0x74697CB8ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFCONSTANT, SYSTEM, "DEFCONSTANT", 11, 0x17EDE1DFULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_IN_PACKAGE, SYSTEM, "IN-PACKAGE", 10, 0x1178D6DBULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SETPLIST, SYSTEM, "SETPLIST", 8, 0x24A78EA7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_REMPLIST, SYSTEM, "REMPLIST", 8, 0x24A08EA6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MAKE_HASH_ITERATOR, SYSTEM, "MAKE-HASH-ITERATOR", 18, 0x41175B68ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_NEXT_HASH_ITERATOR, SYSTEM, "NEXT-HASH-ITERATOR", 18, 0x50245F69ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MAKE_PACKAGE_ITERATOR, SYSTEM, "MAKE-PACKAGE-ITERATOR", 21, 0x627C5DABULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_NEXT_PACKAGE_ITERATOR, SYSTEM, "NEXT-PACKAGE-ITERATOR", 21, 0x718961ACULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFPACKAGE, SYSTEM, "DEFPACKAGE", 10, 0x1191CDD6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DO_SYMBOLS, SYSTEM, "DO-SYMBOLS", 10, 0x226FEFF3ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DO_EXTERNAL_SYMBOLS, SYSTEM, "DO-EXTERNAL-SYMBOLS", 19, 0x075F8A9FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DO_ALL_SYMBOLS, SYSTEM, "DO-ALL-SYMBOLS", 14, 0x639D3C43ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_GETDOC_VARIABLE, SYSTEM, "GETDOC-VARIABLE", 15, 0x5C102728ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SETDOC_VARIABLE, SYSTEM, "SETDOC-VARIABLE", 15, 0x5C102734ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ECASE_ERROR, SYSTEM, "ECASE-ERROR", 11, 0x25D8BFE7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ETYPECASE_ERROR, SYSTEM, "ETYPECASE-ERROR", 15, 0x76321430ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFINE_SETF_EXPANDER, SYSTEM, "DEFINE-SETF-EXPANDER", 20, 0x5D4F7B7EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_END_INPUT_STREAM, SYSTEM, "END-INPUT-STREAM", 16, 0x24290F44ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MAKE_EXTEND_OUTPUT_STREAM, SYSTEM, "MAKE-EXTEND-OUTPUT-STREAM", 25, 0x2BAED11DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PROMPT_FOR, SYSTEM, "PROMPT-FOR", 10, 0x137CF8F9ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_PRINT_UNREADABLE_CALL, SYSTEM, "PRINT-UNREADABLE-CALL", 21, 0x726D4AC5ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_WRITE_DEFAULT, SYSTEM, "WRITE-DEFAULT", 13, 0x65E2C143ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SYMBOL_DEFTYPE, SYSTEM, "SYMBOL-DEFTYPE", 14, 0x5FCF3145ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DELETE_DEFTYPE, SYSTEM, "DELETE-DEFTYPE", 14, 0x62CE163BULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ENSURE_STRUCTURE, SYSTEM, "ENSURE-STRUCTURE", 16, 0x31283B4FULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_STRUCTURE_CONSTRUCTOR, SYSTEM, "STRUCTURE-CONSTRUCTOR", 21, 0x18936CE5ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_BIND, SYSTEM, "LOOP-BIND", 9, 0x1E9891C6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MAKE_PPRINT_STREAM, SYSTEM, "MAKE-PPRINT-STREAM", 18, 0x0A428169ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PPRINT_GENSYM, SYSTEM, "PPRINT-GENSYM", 13, 0x69D2F33DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PPRINT_EXIT, SYSTEM, "PPRINT-EXIT", 11, 0x0ED3EE01ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PPRINT_POP, SYSTEM, "PPRINT-POP", 10, 0x197FF4F7ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PPRINT_CHECK, SYSTEM, "PPRINT-CHECK", 12, 0x57C2E9F2ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PPRINT_CLOSE, SYSTEM, "PPRINT-CLOSE", 12, 0x51D2F3F6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_PPRINT_PRETTY, SYSTEM, "PPRINT-PRETTY", 13, 0x6DD3EA56ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TIMEINFO, SYSTEM, "TIMEINFO", 8, 0x149397A5ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TRACE_ADD, SYSTEM, "TRACE-ADD", 9, 0x07827FE6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_TRACE_DEL, SYSTEM, "TRACE-DEL", 9, 0x08857FEEULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_WITH_COMPILATION_UNIT, SYSTEM, "WITH-COMPILATION-UNIT", 21, 0x6E8D74BEULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_SET_SLOTS, SYSTEM, "SET-SLOTS", 9, 0x01A39202ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_INTERN_EQL_SPECIALIZER, SYSTEM, "INTERN-EQL-SPECIALIZER", 22, 0x013BCCD8ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFGENERIC_DEFINE, SYSTEM, "DEFGENERIC-DEFINE", 17, 0x2C021D6DULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_DEFGENERIC_METHOD, SYSTEM, "DEFGENERIC-METHOD", 17, 0x36012B6CULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CONDITION_RESTARTS_PUSH, SYSTEM, "CONDITION-RESTARTS-PUSH", 23, 0x7BA0CBEDULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CONDITION_RESTARTS_POP, SYSTEM, "CONDITION-RESTARTS-POP", 22, 0x7B58C8E6ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_CONDITION_RESTARTS_MAKE, SYSTEM, "CONDITION-RESTARTS-MAKE", 23, 0x789DC3D9ULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_MAKE_RESTART, SYSTEM, "MAKE-RESTART", 12, 0x6CE2D4DAULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_RESTART_PROGN, SYSTEM, "RESTART-PROGN", 13, 0x48F6EA3EULL, 0, 0, 0 },
{ CONSTANT_SYSTEM_ABORT_LISP, SYSTEM, "ABORT-LISP", 10, 0x1B9BBFF2ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_HELLO, SYSTEM, "HELLO", 5, 0x4C4C459CULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_INFOBIT, SYSTEM, "INFOBIT", 7, 0x4F9A9792ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_INFOPRINT, SYSTEM, "INFOPRINT", 9, 0x1D8FA0F6ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_GC, SYSTEM, "GC", 2, 0x00004349ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SAVECORE, SYSTEM, "SAVECORE", 8, 0x0AA8909EULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_LOADCORE, SYSTEM, "LOADCORE", 8, 0x09939E97ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_PACKAGE_EXPORT_LIST, SYSTEM, "PACKAGE-EXPORT-LIST", 19, 0x145A8884ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SPECIALP, SYSTEM, "SPECIALP", 8, 0x139191A4ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_ARRAY_GENERAL_P, SYSTEM, "ARRAY-GENERAL-P", 15, 0x483BF243ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_ARRAY_SPECIALIZED_P, SYSTEM, "ARRAY-SPECIALIZED-P", 19, 0x18993982ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SIMPLE_SORT, SYSTEM, "SIMPLE-SORT", 11, 0x23CEE0F9ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_BUBBLE_SORT, SYSTEM, "BUBBLE-SORT", 11, 0x15C3ECE8ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_QUICK_SORT, SYSTEM, "QUICK-SORT", 10, 0x129CD6F8ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MERGE_SORT, SYSTEM, "MERGE-SORT", 10, 0x16A5C6EEULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_EXIT, SYSTEM, "EXIT", 4, 0x54495849ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_QUIT, SYSTEM, "QUIT", 4, 0x54495555ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_CLOSP, SYSTEM, "CLOSP", 5, 0x534F4C98ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_FIXNUMP, SYSTEM, "FIXNUMP", 7, 0x4EA896A2ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_BIGNUMP, SYSTEM, "BIGNUMP", 7, 0x4E97969EULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_RATIOP, SYSTEM, "RATIOP", 6, 0x495491A7ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SHORT_FLOAT_P, SYSTEM, "SHORT-FLOAT-P", 13, 0x4BE9B753ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SINGLE_FLOAT_P, SYSTEM, "SINGLE-FLOAT-P", 14, 0x61BD2E26ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_DOUBLE_FLOAT_P, SYSTEM, "DOUBLE-FLOAT-P", 14, 0x5CC43417ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_LONG_FLOAT_P, SYSTEM, "LONG-FLOAT-P", 12, 0x66C7E9C6ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_CALLNAMEP, SYSTEM, "CALLNAMEP", 9, 0x119982EAULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_LARGE_NUMBER, SYSTEM, "LARGE-NUMBER", 12, 0x6EE5B0EAULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_CHARACTER, SYSTEM, "MAKE-CHARACTER", 14, 0x5AD7181FULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_FIXNUM, SYSTEM, "MAKE-FIXNUM", 11, 0x1DE1DCD3ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_BIGNUM, SYSTEM, "MAKE-BIGNUM", 11, 0x0CE1D8D3ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_RATIO, SYSTEM, "MAKE-RATIO", 10, 0x198CE2CDULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_COMPLEX, SYSTEM, "MAKE-COMPLEX", 12, 0x6ADFD0D6ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_EQUAL_RANDOM_STATE, SYSTEM, "EQUAL-RANDOM-STATE", 18, 0x114B5B72ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_EXTEND, SYSTEM, "SUBTYPEP!", 9, 0x2487A5D6ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_NUMBER, SYSTEM, "SUBTYPEP-NUMBER", 15, 0x722F392AULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_EASTASIAN_SET, SYSTEM, "EASTASIAN-SET", 13, 0x5AEFC235ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_EASTASIAN_GET, SYSTEM, "EASTASIAN-GET", 13, 0x5AE3C235ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_EASTASIAN_WIDTH, SYSTEM, "EASTASIAN-WIDTH", 15, 0x5F3C1627ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_RUN_PROGRAM, SYSTEM, "RUN-PROGRAM", 11, 0x74EAE8FFULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_CALLNAME, SYSTEM, "MAKE-CALLNAME", 13, 0x5ECDD318ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_REMOVE_FILE, SYSTEM, "REMOVE-FILE", 11, 0x15BFD6FCULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_REMOVE_DIRECTORY, SYSTEM, "REMOVE-DIRECTORY", 16, 0x30122C55ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_DECLARE_PARSE, SYSTEM, "DECLARE-PARSE", 13, 0x4CDAD927ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_PARSE_TYPE, SYSTEM, "PARSE-TYPE", 10, 0x2CA6B3EFULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_UPGRADED_OPEN_ELEMENT_TYPE, SYSTEM, "UPGRADED-OPEN-ELEMENT-TYPE", 26, 0x4FBBD114ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_MEMORY_INPUT_STREAM, SYSTEM, "MAKE-MEMORY-INPUT-STREAM", 24, 0x36CEA1D0ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_MEMORY_OUTPUT_STREAM, SYSTEM, "MAKE-MEMORY-OUTPUT-STREAM", 25, 0x24B0DD27ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_MEMORY_IO_STREAM, SYSTEM, "MAKE-MEMORY-IO-STREAM", 21, 0x545C82C8ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_WITH_INPUT_FROM_MEMORY, SYSTEM, "WITH-INPUT-FROM-MEMORY", 22, 0x5B6AD4E0ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_WITH_OUTPUT_TO_MEMORY, SYSTEM, "WITH-OUTPUT-TO-MEMORY", 21, 0x697A8ADBULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_GET_OUTPUT_STREAM_MEMORY, SYSTEM, "GET-OUTPUT-STREAM-MEMORY", 24, 0x30BABDF1ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MEMORY_STREAM_P, SYSTEM, "MEMORY-STREAM-P", 15, 0x64101E4FULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_BYTE_INTEGER, SYSTEM, "BYTE-INTEGER", 12, 0x6BE7E9C0ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_SYSCTL, SYSTEM, "SYSCTL", 6, 0x4353A5ADULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_TERME, SYSTEM, "TERME", 5, 0x4D52459EULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_FPCLASSIFY, SYSTEM, "FPCLASSIFY", 10, 0x1596FCD7ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_MAKE_PAPER, SYSTEM, "MAKE-PAPER", 10, 0x158CE3C9ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_INFO_PAPER, SYSTEM, "INFO-PAPER", 10, 0x1F87F0C5ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_ARRAY_PAPER, SYSTEM, "ARRAY-PAPER", 11, 0x02F4C4F5ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_BODY_PAPER, SYSTEM, "BODY-PAPER", 10, 0x2985F1BEULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_DLFILE, SYSTEM, "DLFILE", 6, 0x49469196ULL, 0, 1, 0 },
{ CONSTANT_SYSTEM_DLCALL, SYSTEM, "DLCALL", 6, 0x41439896ULL, 0, 1, 0 },
{ CONSTANT_SPECIAL_BREAK_ON_SIGNALS, COMMON, "*BREAK-ON-SIGNALS*", 18, 0x2A143365ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_COMPILE_FILE_PATHNAME, COMMON, "*COMPILE-FILE-PATHNAME*", 23, 0x61AD8DA4ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_COMPILE_FILE_TRUENAME, COMMON, "*COMPILE-FILE-TRUENAME*", 23, 0x72B18AA5ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_COMPILE_PRINT, COMMON, "*COMPILE-PRINT*", 15, 0x5C183104ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_COMPILE_VERBOSE, COMMON, "*COMPILE-VERBOSE*", 17, 0x2A343224ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_DEBUG_IO, COMMON, "*DEBUG-IO*", 10, 0x0B72B5D8ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_DEBUGGER_HOOK, COMMON, "*DEBUGGER-HOOK*", 15, 0x56FF042FULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_DEFAULT_PATHNAME_DEFAULTS, COMMON, "*DEFAULT-PATHNAME-DEFAULTS*", 27, 0x4DD7F9DAULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_ERROR_OUTPUT, COMMON, "*ERROR-OUTPUT*", 14, 0x76D01630ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_FEATURES, COMMON, "*FEATURES*", 10, 0x0697C5DBULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_GENSYM_COUNTER, COMMON, "*GENSYM-COUNTER*", 16, 0x743A3524ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_LOAD_PATHNAME, COMMON, "*LOAD-PATHNAME*", 15, 0x4418071EULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_LOAD_PRINT, COMMON, "*LOAD-PRINT*", 12, 0x3DF3C7C3ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_LOAD_TRUENAME, COMMON, "*LOAD-TRUENAME*", 15, 0x551C041FULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_LOAD_VERBOSE, COMMON, "*LOAD-VERBOSE*", 14, 0x59F4E613ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_MACROEXPAND_HOOK, COMMON, "*MACROEXPAND-HOOK*", 18, 0x2F245056ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_MODULES, COMMON, "*MODULES*", 9, 0x179499B2ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PACKAGE, COMMON, "*PACKAGE*", 9, 0x088891A8ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_ARRAY, COMMON, "*PRINT-ARRAY*", 13, 0x63C0F701ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_BASE, COMMON, "*PRINT-BASE*", 12, 0x35C4F7C5ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_CASE, COMMON, "*PRINT-CASE*", 12, 0x36C4F7C5ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_CIRCLE, COMMON, "*PRINT-CIRCLE*", 14, 0x58C32114ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_ESCAPE, COMMON, "*PRINT-ESCAPE*", 14, 0x5EC1121EULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_GENSYM, COMMON, "*PRINT-GENSYM*", 14, 0x69D31D18ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_LENGTH, COMMON, "*PRINT-LENGTH*", 14, 0x69C71D13ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_LEVEL, COMMON, "*PRINT-LEVEL*", 13, 0x61C4FAF4ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_LINES, COMMON, "*PRINT-LINES*", 13, 0x68C4F2F8ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_MISER_WIDTH, COMMON, "*PRINT-MISER-WIDTH*", 19, 0x2D389755ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_PPRINT_DISPATCH, COMMON, "*PRINT-PPRINT-DISPATCH*", 23, 0x0578BCC9ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_PRETTY, COMMON, "*PRINT-PRETTY*", 14, 0x6DD41431ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_RADIX, COMMON, "*PRINT-RADIX*", 13, 0x73C8E8F0ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_READABLY, COMMON, "*PRINT-READABLY*", 16, 0x071D320FULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_PRINT_RIGHT_MARGIN, COMMON, "*PRINT-RIGHT-MARGIN*", 20, 0x6C578249ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_QUERY_IO, COMMON, "*QUERY-IO*", 10, 0x0E82D4D5ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_RANDOM_STATE, COMMON, "*RANDOM-STATE*", 14, 0x4FD02014ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_READTABLE, COMMON, "*READTABLE*", 11, 0x03B0EBC5ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_READ_BASE, COMMON, "*READ-BASE*", 11, 0x02B1C4CCULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_READ_DEFAULT_FLOAT_FORMAT, COMMON, "*READ-DEFAULT-FLOAT-FORMAT*", 27, 0x19F5D2F9ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_READ_EVAL, COMMON, "*READ-EVAL*", 11, 0x17B4CBBAULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_READ_SUPPRESS, COMMON, "*READ-SUPPRESS*", 15, 0x5C152320ULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_STANDARD_INPUT, COMMON, "*STANDARD-INPUT*", 16, 0x0C331A1CULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_STANDARD_OUTPUT, COMMON, "*STANDARD-OUTPUT*", 17, 0x3D3A154BULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_TERMINAL_IO, COMMON, "*TERMINAL-IO*", 13, 0x62DCCAFAULL, 1, 0, 0 },
{ CONSTANT_SPECIAL_TRACE_OUTPUT, COMMON, "*TRACE-OUTPUT*", 14, 0x65D01824ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_ENABLE_DEBUGGER, SYSTEM, "*ENABLE-DEBUGGER*", 17, 0x161B1E32ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_INDEX_DEBUGGER, SYSTEM, "*INDEX-DEBUGGER*", 16, 0x7A23290BULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_PARSE_ENVIRONMENT, SYSTEM, "*PARSE-ENVIRONMENT*", 19, 0x2F2F8E7BULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_PARSE_DECLARE, SYSTEM, "*PARSE-DECLARE*", 15, 0x57E51E23ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_SCOPE, SYSTEM, "*SCOPE*", 7, 0x4F6D9881ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_SCOPE_GLOBAL, SYSTEM, "*SCOPE-GLOBAL*", 14, 0x57B31220ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_TOPLEVEL, SYSTEM, "*EVAL-TOPLEVEL*", 15, 0x671A0B1AULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_COMPILE_TIME, SYSTEM, "*EVAL-COMPILE-TIME*", 19, 0x26613568ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_COMPILE_TOPLEVEL, SYSTEM, "*EVAL-COMPILE-TOPLEVEL*", 23, 0x02A688B4ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_LOAD_TOPLEVEL, SYSTEM, "*EVAL-LOAD-TOPLEVEL*", 20, 0x54684C70ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_EXECUTE, SYSTEM, "*EVAL-EXECUTE*", 14, 0x6DF0E00EULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_SPECIAL_ENVIRONMENT, SYSTEM, "*ENVIRONMENT*", 13, 0x78EBDCF7ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_SPECIAL_ARGUMENTS, SYSTEM, "*ARGUMENTS*", 11, 0x15C1E1DEULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOAD_SIZE, SYSTEM, "*LOAD-SIZE*", 11, 0x0ACCBED3ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOAD_ARRAY, SYSTEM, "*LOAD-ARRAY*", 12, 0x3DE9BACCULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOAD_TABLE, SYSTEM, "*LOAD-TABLE*", 12, 0x2CE8C5BCULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOAD_GENSYM, SYSTEM, "*LOAD-GENSYM*", 13, 0x53EFCCF3ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOAD_DEPEND, SYSTEM, "*LOAD-DEPEND*", 13, 0x4AE1BEF5ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOAD_PUSH, SYSTEM, "*LOAD-PUSH*", 11, 0x16C9C1CCULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DEPEND_LOOP, SYSTEM, "*DEPEND-LOOP*", 13, 0x4DD8E1F2ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DEPEND_PASS, SYSTEM, "*DEPEND-PASS*", 13, 0x50DCD3F6ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DEPEND_ERROR, SYSTEM, "*DEPEND-ERROR*", 14, 0x4CDC0F14ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DEPEND_ROOT, SYSTEM, "*DEPEND-ROOT*", 13, 0x51D8E1F8ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_GCHOLD, SYSTEM, "*GCHOLD*", 8, 0x72879381ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_NAMED, SYSTEM, "*LOOP-NAMED*", 12, 0x3AE1BED3ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_VARS, SYSTEM, "*LOOP-VARS*", 11, 0x10CFCCD7ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_INIT, SYSTEM, "*LOOP-INIT*", 11, 0x1DC2CDCEULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FINAL, SYSTEM, "*LOOP-FINAL*", 12, 0x42E1BAD4ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_FORM, SYSTEM, "*LOOP-FORM*", 11, 0x1EBFC6D7ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOOP_LET, SYSTEM, "*LOOP-LET*", 10, 0x149BA3D8ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_ENABLE_COMPILER_MACRO, SYSTEM, "*ENABLE-COMPILER-MACRO*", 23, 0x5491C98EULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_LOAD_LOGICAL_PATHNAME_TRANSLATIONS, SYSTEM, "*LOAD-LOGICAL-PATHNAME-TRANSLATIONS*", 36, 0x6FA04EB3ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EXTERNAL_FORMAT, SYSTEM, "*EXTERNAL-FORMAT*", 17, 0x392E1248ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_END_OF_LINE, SYSTEM, "*END-OF-LINE*", 13, 0x36E2DDDAULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_PRINT_WRITE, SYSTEM, "*PRINT-WRITE*", 13, 0x65D3EE01ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DEFAULT_PRINT_DISPATCH, SYSTEM, "*DEFAULT-PRINT-DISPATCH*", 24, 0x13A9D49BULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EMPTY_PRINT_DISPATCH, SYSTEM, "*EMPTY-PRINT-DISPATCH*", 22, 0x0B6697ABULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DELAY_WARNING_LIST, SYSTEM, "*DELAY-WARNING-LIST*", 20, 0x63428A57ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DELAY_WARNING_SWITCH, SYSTEM, "*DELAY-WARNING-SWITCH*", 22, 0x0342AAAFULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_MODULE_PROVIDER_FUNCTIONS, SYSTEM, "*MODULE-PROVIDER-FUNCTIONS*", 27, 0x37F21DF1ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_COMPILE_OUTPUT, SYSTEM, "*COMPILE-OUTPUT*", 16, 0x11453107ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_COMPILE_CODE, SYSTEM, "*COMPILE-CODE*", 14, 0x56EAF9FAULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_ENCODE_UNIVERSAL_1970, SYSTEM, "*ENCODE-UNIVERSAL-1970*", 23, 0x418B87ADULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_ED_FUNCTION, SYSTEM, "*ED-FUNCTION*", 13, 0x3EE1E3FBULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_ED_TEMPFILE, SYSTEM, "*ED-TEMPFILE*", 13, 0x42DDD3FBULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_ED_PROGRAM, SYSTEM, "*ED-PROGRAM*", 12, 0x1EE0D8D8ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_TRACE_LIST, SYSTEM, "*TRACE-LIST*", 12, 0x37D3ECC2ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_TRACE_DEPTH, SYSTEM, "*TRACE-DEPTH*", 13, 0x4DD3E9E9ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DRIBBLE_FILE, SYSTEM, "*DRIBBLE-FILE*", 14, 0x5AE7F6ECULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DRIBBLE_INPUT, SYSTEM, "*DRIBBLE-INPUT*", 15, 0x5F1723FDULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DRIBBLE_OUTPUT, SYSTEM, "*DRIBBLE-OUTPUT*", 16, 0x0D482AF9ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DRIBBLE_ECHO, SYSTEM, "*DRIBBLE-ECHO*", 14, 0x56E1F5F6ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_DRIBBLE_BROADCAST, SYSTEM, "*DRIBBLE-BROADCAST*", 19, 0x1F5E6140ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_INSPECTED, SYSTEM, "*INSPECTED*", 11, 0x27BBD2CAULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_STEP_BEGIN, SYSTEM, "*STEP-BEGIN*", 12, 0x34E4C9CDULL, 1, 0, 0 },
{ CONSTANT_RT_RESULT, RT, "*RESULT*", 8, 0x7D999E87ULL, 1, 0, 0 },
{ CONSTANT_RT_INDEX, RT, "*INDEX*", 7, 0x4478A176ULL, 1, 0, 0 },
{ CONSTANT_RT_ENTRIES, RT, "*ENTRIES*", 9, 0x27938EAFULL, 1, 0, 0 },
{ CONSTANT_RT_ENTRIES_TABLE, RT, "*ENTRIES-TABLE*", 15, 0x69FF2804ULL, 1, 0, 0 },
{ CONSTANT_RT_ENTRIES_WARNING, RT, "*ENTRIES-WARNING*", 17, 0x41232F32ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_READINFO_SPECIAL, SYSTEM, "*READINFO*", 10, 0x0793C5C7ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_EVAL_LOOP_EXIT, SYSTEM, "*EVAL-LOOP-EXIT*", 16, 0x00240C2DULL, 1, 1, 0 },
{ CONSTANT_SYSTEM_PROMPT, SYSTEM, "*PROMPT*", 8, 0x79A6A07FULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_PROMPT_READING, SYSTEM, "*PROMPT-READING*", 16, 0x6B2F3422ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_PROMPT_VALUE, SYSTEM, "*PROMPT-VALUE*", 14, 0x51F30C20ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_PROMPT_BRIGHT, SYSTEM, "*PROMPT-BRIGHT*", 15, 0x441A4710ULL, 1, 1, 0 },
{ CONSTANT_SYSTEM_PROMPT_COLOR, SYSTEM, "*PROMPT-COLOR*", 14, 0x4BF31A1AULL, 1, 1, 0 },
{ CONSTANT_SYSTEM_SPECIAL_TERME, SYSTEM, "*TERME*", 7, 0x526F997EULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_SPECIAL_DLFILE, SYSTEM, "*DLFILE*", 8, 0x7091907BULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_CORE_INPUT, SYSTEM, "*CORE-INPUT*", 12, 0x4AECC5CBULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_CORE_OUTPUT, SYSTEM, "*CORE-OUTPUT*", 13, 0x7BF3C0FAULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_SUBTYPEP_VALUE, SYSTEM, "*SUBTYPEP!*", 11, 0x07CFCDD9ULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_STANDARD_INPUT, SYSTEM, "*STANDARD-INPUT*", 16, 0x0C331A1CULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_STANDARD_OUTPUT, SYSTEM, "*STANDARD-OUTPUT*", 17, 0x3D3A154BULL, 1, 0, 0 },
{ CONSTANT_SYSTEM_STANDARD_ERROR, SYSTEM, "*STANDARD-ERROR*", 16, 0x102D141EULL, 1, 0, 0 },
#endif
	{ CONSTANT_EMPTY, DEFAULT, NULL, 0, 0, 0, 0, 0 }
};

static int intern_symbol_package_(addr package, struct symbol_header *str, addr *ret)
{
	addr name, value, car, cdr;
	size_t size, index;

	/* bitpackage */
	Check(! packagep(package), "type error");
	Return(strvect_size1_heap_(&name, str->symbol, str->length));
	if (str->findp)
		return intern_package_(package, name, ret, NULL);

#ifdef LISP_DEBUG
	{
		enum PACKAGE_TYPE debug;
		addr ignore;
		Error(find_symbol_package_(package, name, &ignore, &debug));
		Check(debug != PACKAGE_TYPE_NIL, "find error");
	}
#endif
	make_bitpackage_symbol(&value, ret, name, package);
#ifdef LISP_DEBUG
	{
		fixnum debug;
		Error(sxhash_char_equal_(str->symbol, &debug));
		Check(debug != str->sxhash, "sxhash error");
	}
#endif

	/* intern */
	GetPackage(package, PACKAGE_INDEX_TABLE, &package);
	getsize_hashtable(package, &size);
	inccount_hashtable(package, 1);
	GetTableHash(package, &package);
	/* array[index] -> ((key . nil) . next)
	 * ret -> (key . nil) */
	index = str->sxhash % size;
	GetArrayHash(package, index, &cdr);
	cons_heap(&car, name, value);
	cons_heap(&cdr, car, cdr);
	SetArrayHash(package, index, cdr);

	return 0;
}

int intern_symbol_header_(void)
{
	addr symbol, p_common, p_keyword, p_system, p_code, p_clos, p_rt, package;
	struct symbol_header *table;
	size_t i;

	GetConst(PACKAGE_COMMON_LISP, &p_common);
	GetConst(PACKAGE_KEYWORD, &p_keyword);
	GetConst(PACKAGE_SYSTEM, &p_system);
	GetConst(PACKAGE_CODE, &p_code);
	GetConst(PACKAGE_CLOS, &p_clos);
	GetConst(PACKAGE_RT, &p_rt);

	for (i = 0; ; i++) {
		table = &SymbolHeader[i];
		if (table->index == CONSTANT_EMPTY)
			break;
		switch (table->package) {
			case COMMON:
				package = p_common;
				Return(intern_symbol_package_(package, table, &symbol));
				Return(symbol_export_package_(package, symbol));
				break;

			case KEYWORD:
				package = p_keyword;
				Return(intern_symbol_package_(package, table, &symbol));
				Return(setkeyword_package_(symbol));
				break;

			case SYSTEM:
				package = p_system;
				Return(intern_symbol_package_(package, table, &symbol));
				break;

			case CODE:
				package = p_code;
				Return(intern_symbol_package_(package, table, &symbol));
				break;

			case CLOS:
				package = p_clos;
				Return(intern_symbol_package_(package, table, &symbol));
				break;

			case RT:
				package = p_rt;
				Return(intern_symbol_package_(package, table, &symbol));
				break;

			default:
				return fmte_("package error.", NULL);
		}
		if (table->specialp)
			setspecial_symbol(symbol);
		if (table->exportp) {
			Return(export_package_(package, symbol));
		}
		SetConstant(table->index, symbol);
	}

	return 0;
}


/************************************************************
 *  lambda.c
 ************************************************************/

/*
 *  error
 */
static int lambda_list_fmte_(const char *fmt, ...)
{
	va_list va;

	va_start(va, fmt);
	Return(call_simple_program_error_stdarg_(NULL, fmt, va));
	va_end(va);

	return 0;
}


/*
 *  parse
 */
enum AMPERSAND_ARGUMENT {
	AMPERSAND_GENERIC,
	AMPERSAND_ORDINARY,
	AMPERSAND_METHOD_COMBINATION,
	AMPERSAND_MACRO,
	AMPERSAND_SIZE
};

static int lambda_constant_eq(constindex index, addr pos)
{
	addr check;
	GetConstant(index, &check);
	return check == pos;
}

static int lambda_member_ampersand(addr pos, enum AMPERSAND_ARGUMENT ampersand)
{
	/* generic */
	if (lambda_constant_eq(CONSTANT_AMPERSAND_OPTIONAL, pos))
		return 1;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, pos))
		return 1;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, pos))
		return 1;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ALLOW, pos))
		return 1;
	if (ampersand <= AMPERSAND_GENERIC)
		return 0;
	/* ordinary */
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, pos))
		return 1;
	if (ampersand <= AMPERSAND_ORDINARY)
		return 0;
	/* method-combination */
	if (lambda_constant_eq(CONSTANT_AMPERSAND_WHOLE, pos))
		return 1;
	if (ampersand <= AMPERSAND_METHOD_COMBINATION)
		return 0;
	/* macro */
	if (lambda_constant_eq(CONSTANT_AMPERSAND_BODY, pos))
		return 1;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, pos))
		return 1;

	return 0;
}

static int lambda_variable_check_(addr var, enum AMPERSAND_ARGUMENT ampersand)
{
	if (var == Nil || var == T) {
		return lambda_list_fmte_("The symbol ~S "
				"must not be a constant symbol.", var, NULL);
	}
	if (! symbolp(var)) {
		return lambda_list_fmte_("The variable ~S "
				"is not a symbol.", var, NULL);
	}
	if (GetStatusReadOnly(var)) {
		return lambda_list_fmte_("The symbol ~S "
				"must not be a constant symbol.", var, NULL);
	}
	if (lambda_member_ampersand(var, ampersand)) {
		return lambda_list_fmte_("The symbol ~S "
				"don't use in the lambda-list.", var, NULL);
	}

	return 0;
}

/* push-varcons */
static void lambda_varcons_local(LocalRoot local, addr *ret)
{
	queue_local(local, ret);
}

static void lambda_varcons_data(addr instance, addr *ret)
{
	rootqueue(instance, ret);
}

static int lambda_push_varcons_(LocalRoot local,
		addr instance, addr var, enum AMPERSAND_ARGUMENT ampersand)
{
	addr data;

	Return(lambda_variable_check_(var, ampersand));
	lambda_varcons_data(instance, &data);
	if (find_list_eq_unsafe(var, data))
		return lambda_list_fmte_("The variable ~S is already used.", var, NULL);
	pushqueue_local(local, instance, var);

	return 0;
}

static int lambda_push_auxcons_(LocalRoot local,
		addr instance, addr var, enum AMPERSAND_ARGUMENT ampersand)
{
	Return(lambda_variable_check_(var, ampersand));
	pushqueue_local(local, instance, var);

	return 0;
}

static int lambda_push_namecons_(LocalRoot local, addr instance, addr name)
{
	addr data;

	lambda_varcons_data(instance, &data);
	if (find_list_eq_unsafe(name, data))
		return lambda_list_fmte_("The key name ~S is already used.", name, NULL);
	pushqueue_local(local, instance, name);

	return 0;
}

static int make_keyword_from_symbol_(addr symbol, addr *ret)
{
	addr keyword, package;

	if (! symbolp(symbol))
		return lambda_list_fmte_("The variable ~S must be a symbol.", symbol, NULL);
	GetConst(PACKAGE_KEYWORD, &keyword);
	GetPackageSymbol(symbol, &package);
	if (package == keyword)
		return Result(ret, symbol);

	GetNameSymbol(symbol, &symbol);
	return intern_package_(keyword, symbol, ret, NULL);
}

static int lambda_list2_check(addr cons, addr *ret1, addr *ret2)
{
	addr pos1, pos2;

	if (GetType(cons) != LISPTYPE_CONS)
		return 1;
	GetCons(cons, &pos1, &cons);
	if (GetType(cons) != LISPTYPE_CONS)
		return 1;
	GetCons(cons, &pos2, &cons);
	if (cons != Nil)
		return 1;
	*ret1 = pos1;
	*ret2 = pos2;

	return 0;
}

static int lambda_key_name_values_(addr pos, addr *symbol, addr *name)
{
	if (GetType(pos) == LISPTYPE_CONS) {
		/* swap (name, symbol) -> (symbol, name). */
		if (lambda_list2_check(pos, name, symbol)) {
			*symbol = *name = NULL;
			return lambda_list_fmte_("The variable ~S "
					"must be a symbol or (name symbol) list.", pos, NULL);
		}
		return 0;
	}
	else {
		*symbol = pos;
		return make_keyword_from_symbol_(pos, name);
	}
}


/*
 *  lambda-macro
 */
static int push_varcons_macro_(LocalRoot local, addr instance, addr key)
{
	return lambda_push_varcons_(local, instance, key, AMPERSAND_MACRO);
}

static int push_auxcons_macro_(LocalRoot local, addr instance, addr key)
{
	return lambda_push_auxcons_(local, instance, key, AMPERSAND_MACRO);
}

#define nextcons_finish_rest(one, cons) { \
	GetCdr(cons, &cons); \
	if (cons == Nil) goto finish; \
	if (! consp(cons)) goto finish_rest; \
	GetCar(cons, &one); \
}

#define nextcons_finish_error(one, cons, name) { \
	GetCdr(cons, &cons); \
	if (cons == Nil) goto finish; \
	if (! consp(cons)) { \
		addr __symbol; \
		GetConst(AMPERSAND_##name, &__symbol); \
		return lambda_list_fmte_("After ~A " \
				"parameter don't allow a dot list.", __symbol, NULL); \
	} \
	GetCar(cons, &one); \
}

#define environment_expander(local, instance, envcheck, env, one, cons) { \
	if (! envcheck) { \
		return lambda_list_fmte_("&environment don't accept " \
				"at no top-level argument.", NULL); \
	} \
	if (env != Nil) { \
		return lambda_list_fmte_("&environment must be " \
				"at once in arguments.", NULL); \
	} \
	GetCdr(cons, &cons); \
	if (cons == Nil) { \
		return lambda_list_fmte_("&environment parameter " \
				"must be a one argument.", NULL); \
	} \
	if (! consp(cons)) { \
		return lambda_list_fmte_("After &environment " \
				"don't allow a dot list.", NULL); \
	} \
	GetCar(cons, &one); \
	Return(push_varcons_macro_(local, instance, one)); \
	env = one; \
}

static int ordinary_opt_default_(addr cons, addr init,
		addr *rvar, addr *rinit, addr *rsup)
{
	addr base, var, sup;

	base = cons;
	var = sup = Nil;
	/* var */
	if (GetType(cons) != LISPTYPE_CONS) {
		var = cons;
		goto final;
	}
	/* (var) */
	GetCons(cons, &var, &cons);
	if (cons == Nil)
		goto final;
	if (GetType(cons) != LISPTYPE_CONS) {
		return lambda_list_fmte_("Dot list don't allow "
				"in this lambda-list: ~S.", base, NULL);
	}
	/* (var init) */
	GetCons(cons, &init, &cons);
	if (cons == Nil)
		goto final;
	if (GetType(cons) != LISPTYPE_CONS) {
		return lambda_list_fmte_("Dot list don't allow "
				"in this lambda-list: ~S.", base, NULL);
	}
	/* (var init sup) */
	GetCons(cons, &sup, &cons);
	if (sup == Nil) {
		return lambda_list_fmte_("The supplied variable ~S "
				"must not be a constant symbol.", sup, NULL);
	}
	if (cons == Nil)
		goto final;
	/* (var init sup . error) */
	return lambda_list_fmte_("Too many argument ~S.", base, NULL);

final:
	*rvar = var;
	*rinit = init;
	*rsup = sup;
	return 0;
}

static int ordinary_opt_(addr cons, addr *rvar, addr *rinit, addr *rsup)
{
	return ordinary_opt_default_(cons, Nil, rvar, rinit, rsup);
}

static int ordinary_key_default_(LocalRoot local, addr name, addr cons, addr value,
		addr *rvar, addr *rname, addr *rinit, addr *rsup)
{
	Return(ordinary_opt_default_(cons, value, &cons, rinit, rsup));
	Return(lambda_key_name_values_(cons, rvar, rname));
	return lambda_push_namecons_(local, name, *rname);
}

static int ordinary_key_(LocalRoot local, addr name, addr cons,
		addr *rvar, addr *rname, addr *rinit, addr *rsup)
{
	return ordinary_key_default_(local, name, cons, Nil, rvar, rname, rinit, rsup);
}

static int ordinary_aux_(addr cons, addr *rvar, addr *rinit)
{
	addr base, var, init;

	base = cons;
	var = init = Nil;
	/* var */
	if (GetType(cons) != LISPTYPE_CONS) {
		var = cons;
		goto final;
	}
	/* (var) */
	GetCons(cons, &var, &cons);
	if (cons == Nil)
		goto final;
	if (GetType(cons) != LISPTYPE_CONS) {
		return lambda_list_fmte_("Dot list don't allow "
				"in this lambda-list: ~S.", base, NULL);
	}
	/* (var init) */
	GetCons(cons, &init, &cons);
	if (cons == Nil)
		goto final;
	/* (var init . error) */
	return lambda_list_fmte_("Too many argument ~S.", base, NULL);

final:
	*rvar = var;
	*rinit = init;
	return 0;
}

static int lambda_macro_call_(LocalRoot, addr *, addr, addr, addr);

static int lambda_macro_variable_(LocalRoot local,
		addr *ret, addr pos, addr instance, addr defvalue)
{
	if (listp(pos)) {
		return lambda_macro_call_(local, ret, pos, instance, defvalue);
	}
	else {
		Return(push_varcons_macro_(local, instance, pos));
		return Result(ret, pos);
	}
}

static int lambda_macro_opt_(LocalRoot local,
		addr pos, addr instance, addr defvalue,
		addr *rvar, addr *rinit, addr *rsup)
{
	Return(ordinary_opt_default_(pos, defvalue, &pos, rinit, rsup));
	Return(lambda_macro_variable_(local, &pos, pos, instance, defvalue));
	return Result(rvar, pos);
}

static int lambda_macro_rest_(LocalRoot local,
		addr instance, addr defvalue, addr *ret, addr pos)
{
	GetCar(pos, &pos);
	Return(lambda_macro_variable_(local, &pos, pos, instance, defvalue));
	return Result(ret, pos);
}

static int lambda_macro_key_(LocalRoot local, addr instance, addr defvalue, addr name,
		addr pos, addr *rvar, addr *rsym, addr *rinit, addr *rsup)
{
	Return(ordinary_key_default_(local, name, pos, defvalue, &pos, rsym, rinit, rsup));
	Return(lambda_macro_variable_(local, &pos, pos, instance, defvalue));
	return Result(rvar, pos);
}

static int lambda_macro_call_(LocalRoot local,
		addr *ret, addr cons, addr instance, addr defvalue)
{
	int envcheck;
	addr name, var, opt, rest, key, key_p, allow, aux, one;
	addr whole, env, symbol, init, sup, base;

	base = cons;
	var = opt = rest = key = key_p = allow = aux = whole = env = Nil;
	envcheck = (instance == Nil);
	if (envcheck)
		lambda_varcons_local(local, &instance);
	lambda_varcons_local(local, &name);
	if (cons == Nil)
		goto finish;
	if (! consp(cons))
		goto finish_rest;
	GetCar(cons, &one);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_WHOLE, one))
		goto whole_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto var_environment;
	goto var_argument;

whole_argument:
	GetCdr(cons, &cons);
	if (cons == Nil) {
		return lambda_list_fmte_("&whole parameter must be a one argument.", NULL);
	}
	if (! consp(cons)) {
		return lambda_list_fmte_("After &whole parameter "
				"don't allow a dot list.", NULL);
	}
	GetCar(cons, &one);
	Return(push_varcons_macro_(local, instance, one));
	whole = one;
	nextcons_finish_rest(one, cons);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto var_environment;
	goto var_argument;

var_environment:
	environment_expander(local, instance, envcheck, env, one, cons);
	nextcons_finish_rest(one, cons);

var_argument:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_OPTIONAL, one))
		goto optional_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_BODY, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto optional_environment;
	Return(lambda_macro_variable_(local, &one, one, instance, defvalue));
	cons_heap(&var, one, var);
	nextcons_finish_rest(one, cons);
	goto var_argument;

optional_environment:
	environment_expander(local, instance, envcheck, env, one, cons);
	nextcons_finish_rest(one, cons);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_OPTIONAL, one))
		goto optional_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_BODY, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

optional_argument:
	nextcons_finish_rest(one, cons);
optional_loop:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_BODY, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto rest_environment;
	Return(lambda_macro_opt_(local, one, instance, defvalue, &one, &init, &sup));
	if (sup != Nil) {
		Return(push_varcons_macro_(local, instance, sup));
	}
	list_heap(&one, one, init, sup, NULL);
	cons_heap(&opt, one, opt);
	nextcons_finish_rest(one, cons);
	goto optional_loop;

rest_environment:
	environment_expander(local, instance, envcheck, env, one, cons);
	nextcons_finish_rest(one, cons);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_BODY, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

rest_argument:
	GetCdr(cons, &cons);
	if (cons == Nil)
		return lambda_list_fmte_("&rest parameter must be a one argument.", NULL);
	if (! consp(cons)) {
		return lambda_list_fmte_("Dot list don't allow "
				"in this lambda-list: ~S.", base, NULL);
	}
	Return(lambda_macro_rest_(local, instance, defvalue, &rest, cons));
	cons_heap(&rest, rest, one);
	nextcons_finish_error(one, cons, REST);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto key_environment;
	return lambda_list_fmte_("After &rest/&body ~S "
			"must be a null or &key arguments.", one, NULL);

key_environment:
	environment_expander(local, instance, envcheck, env, one, cons);
	nextcons_finish_error(one, cons, REST);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

key_argument:
	key_p = T;
	nextcons_finish_error(one, cons, KEY);
key_loop:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ALLOW, one))
		goto allow_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto aux_environment;
	Return(lambda_macro_key_(local, instance, defvalue, name,
				one, &one, &symbol, &init, &sup));
	if (sup != Nil) {
		Return(push_varcons_macro_(local, instance, sup));
	}
	list_heap(&one, one, symbol, init, sup, NULL);
	cons_heap(&key, one, key);
	nextcons_finish_error(one, cons, KEY);
	goto key_loop;

allow_argument:
	allow = T;
	nextcons_finish_error(one, cons, ALLOW);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto aux_environment;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

aux_environment:
	environment_expander(local, instance, envcheck, env, one, cons);
	nextcons_finish_error(one, cons, KEY);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

aux_argument:
	nextcons_finish_error(one, cons, AUX);
aux_loop:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto finish_environment;
	Return(ordinary_aux_(one, &symbol, &init));
	Return(push_auxcons_macro_(local, instance, symbol));
	list_heap(&one, symbol, init, NULL);
	cons_heap(&aux, one, aux);
	nextcons_finish_error(one, cons, AUX);
	goto aux_loop;

finish_environment:
	environment_expander(local, instance, envcheck, env, one, cons);
	nextcons_finish_error(one, cons, AUX);
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

finish_rest:
	Return(push_varcons_macro_(local, instance, cons));
	conscar_heap(&rest, cons);
	goto finish;

finish:
	nreverse(&var, var);
	nreverse(&opt, opt);
	nreverse(&key, key);
	nreverse(&aux, aux);
	list_heap(ret, var, opt,
			rest,  /* (var . &rest) (var . &body) (var . nil) */
			(key != Nil? key: key_p), allow, aux, whole, env, NULL);
	return 0;
}

int lambda_macro_(LocalRoot local, addr *ret, addr cons, addr instance)
{
	return lambda_macro_call_(local, ret, cons, instance, Nil);
}


/*
 *  lambda-ordinary
 */
int lambda_deftype_(LocalRoot local, addr *ret, addr cons, addr instance)
{
	addr aster;
	GetConst(COMMON_ASTERISK, &aster);
	quotelist_heap(&aster, aster);
	return lambda_macro_call_(local, ret, cons, instance, aster);
}


/*
 *  lambda-ordinary
 */
static int push_varcons_ordinary_(LocalRoot local, addr instance, addr key)
{
	return lambda_push_varcons_(local, instance, key, AMPERSAND_ORDINARY);
}

static int push_auxcons_ordinary_(LocalRoot local, addr instance, addr key)
{
	return lambda_push_auxcons_(local, instance, key, AMPERSAND_ORDINARY);
}


/*
 *  lambda-generic-function
 */
static int generic_function_key_cons_(addr cons, addr *symbol, addr *name)
{
	addr var;

	/* var */
	if (GetType(cons) != LISPTYPE_CONS) {
		var = cons;
		goto final;
	}
	/* (var) */
	GetCons(cons, &var, &cons);
	if (cons == Nil) {
		goto final;
	}
	/* (var . error) */
	return lambda_list_fmte_("&key parameter ~S "
			"must be at many one arguments.", cons, NULL);

final:
	return lambda_key_name_values_(var, symbol, name);
}

static int generic_function_key_(LocalRoot local,
		addr instance, addr cons, addr *var, addr *name)
{
	Return(generic_function_key_cons_(cons, var, name));
	return lambda_push_namecons_(local, instance, *name);
}

static int push_varcons_generic_function_(LocalRoot local,
		addr instance, addr key)
{
	return lambda_push_varcons_(local, instance, key, AMPERSAND_GENERIC);
}

#define nextcons_finish(one, cons, base) { \
	GetCdr(cons, &cons); \
	if (cons == Nil) goto finish; \
	if (GetType(cons) != LISPTYPE_CONS) { \
		return lambda_list_fmte_("Dot list don't allow " \
				"in this lambda-list: ~S.", base, NULL); \
	} \
	GetCar(cons, &one); \
}

int lambda_generic_function_(LocalRoot local, addr *ret, addr cons)
{
	addr instance, name, var, opt, rest, key, key_p, allow, one, symbol;
	addr base;

	base = cons;
	var = opt = rest = key = key_p = allow = one = symbol = Nil;
	lambda_varcons_local(local, &instance);
	lambda_varcons_local(local, &name);
	if (cons == Nil)
		goto finish;
	if (GetType(cons) != LISPTYPE_CONS) {
		return lambda_list_fmte_("Dot list don't allow "
				"in this lambda-list: ~S.", base, NULL);
	}
	GetCar(cons, &one);

var_argument:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_OPTIONAL, one))
		goto optional_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	Return(push_varcons_generic_function_(local, instance, one));
	cons_heap(&var, one, var);
	nextcons_finish(one, cons, base);
	goto var_argument;

optional_argument:
	nextcons_finish(one, cons, base);
optional_loop:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (singlep(one))
		GetCar(one, &one);
	Return(push_varcons_generic_function_(local, instance, one));
	cons_heap(&opt, one, opt);
	nextcons_finish(one, cons, base);
	goto optional_loop;

rest_argument:
	GetCdr(cons, &cons);
	if (cons == Nil)
		return lambda_list_fmte_("&rest parameter must be a one argument.", NULL);
	if (GetType(cons) != LISPTYPE_CONS) {
		return lambda_list_fmte_("Dot list don't allow "
				"in this lambda-list: ~S.", base, NULL);
	}
	GetCar(cons, &rest);
	Return(push_varcons_generic_function_(local, instance, rest));
	nextcons_finish(one, cons, base);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	return lambda_list_fmte_("After &rest ~S "
			"must be a null or &key arguments.", cons, NULL);

key_argument:
	key_p = T;
	nextcons_finish(one, cons, base);
key_loop:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ALLOW, one))
		goto allow_argument;
	Return(generic_function_key_(local, name, one, &symbol, &one));
	Return(push_varcons_generic_function_(local, instance, symbol));
	list_heap(&one, symbol, one, NULL);
	cons_heap(&key, one, key);
	nextcons_finish(one, cons, base);
	goto key_loop;

allow_argument:
	allow = T;
	nextcons_finish(one, cons, base);
	return lambda_list_fmte_("After &allow-other-keys ~S must be a null.", cons, NULL);

finish:
	nreverse(&var, var);
	nreverse(&opt, opt);
	nreverse(&key, key);
	list_heap(ret, var, opt, rest, (key != Nil? key: key_p), allow, NULL);
	return 0;
}

void atleast_argument_count(addr cons, size_t *ret)
{
	GetCar(cons, &cons);
	*ret = length_list_unsafe(cons);
}


/*
 *  lambda-specialized
 */
static int specialized_var_cons_(addr cons, addr *rvar, addr *rspec)
{
	addr var, spec;

	spec = T;
	/* var */
	if (GetType(cons) != LISPTYPE_CONS) {
		var = cons;
		goto final;
	}
	/* (var) */
	GetCons(cons, &var, &cons);
	if (cons == Nil)
		goto final;
	if (GetType(cons) != LISPTYPE_CONS) {
		return lambda_list_fmte_("The dot list don't allow "
				"in the argument ~S.", cons, NULL);
	}
	/* (var specializer) */
	GetCons(cons, &spec, &cons);
	if (cons == Nil)
		goto final;
	/* (var specializer . error) */
	return lambda_list_fmte_("The variable parameter "
			"must be at many two arguments.", NULL);

final:
	*rvar = var;
	*rspec = spec;
	return 0;
}

static int check_specializer_form(addr spec)
{
	enum LISPTYPE type;
	addr left, check;

	/* symbol */
	if (spec == Nil || spec == T)
		return 1;
	type = GetType(spec);
	if (type == LISPTYPE_SYMBOL)
		return 1;
	/* (eql spec) */
	if (type != LISPTYPE_CONS)
		return 0;
	GetCons(spec, &left, &spec);
	GetConst(COMMON_EQL, &check);
	if (left != check)
		return 0;
	if (GetType(spec) != LISPTYPE_CONS)
		return 0;
	GetCdr(spec, &spec);

	return spec == Nil;
}

static int specialized_var_(addr cons, addr *var, addr *spec)
{
	Return(specialized_var_cons_(cons, var, spec));
	if (! check_specializer_form(*spec)) {
		return lambda_list_fmte_("The parameter ~S "
				"don't allow a specializer form.", *spec, NULL);
	}

	return 0;
}

int lambda_specialized_(LocalRoot local, addr *ret, addr cons)
{
	addr instance, name, var, opt, rest, key, key_p, allow, aux, one;
	addr symbol, init, sup, base;

	base = cons;
	var = opt = rest = key = key_p = allow = aux = Nil;
	lambda_varcons_local(local, &instance);
	lambda_varcons_local(local, &name);
	if (cons == Nil)
		goto finish;
	if (GetType(cons) != LISPTYPE_CONS) {
		return lambda_list_fmte_("Dot list don't allow "
				"in this lambda-list: ~S.", base, NULL);
	}
	GetCar(cons, &one);

var_argument:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_OPTIONAL, one))
		goto optional_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	Return(specialized_var_(one, &symbol, &one));
	Return(push_varcons_ordinary_(local, instance, symbol));
	list_heap(&one, symbol, one, NULL);
	cons_heap(&var, one, var);
	nextcons_finish(one, cons, base);
	goto var_argument;

optional_argument:
	nextcons_finish(one, cons, base);
optional_loop:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	Return(ordinary_opt_(one, &symbol, &init, &sup));
	Return(push_varcons_ordinary_(local, instance, symbol));
	if (sup != Nil) {
		Return(push_varcons_ordinary_(local, instance, sup));
	}
	list_heap(&one, symbol, init, sup, NULL);
	cons_heap(&opt, one, opt);
	nextcons_finish(one, cons, base);
	goto optional_loop;

rest_argument:
	GetCdr(cons, &cons);
	if (cons == Nil)
		return lambda_list_fmte_("&rest parameter must be a one argument.", NULL);
	if (GetType(cons) != LISPTYPE_CONS) {
		return lambda_list_fmte_("Dot list don't allow "
				"in this lambda-list: ~S.", base, NULL);
	}
	GetCar(cons, &rest);
	Return(push_varcons_ordinary_(local, instance, rest));
	nextcons_finish(one, cons, base);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("After &rest ~S "
			"must be a null or &key arguments.", cons, NULL);

key_argument:
	key_p = T;
	nextcons_finish(one, cons, base);
key_loop:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ALLOW, one))
		goto allow_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	Return(ordinary_key_(local, name, one, &symbol, &one, &init, &sup));
	Return(push_varcons_ordinary_(local, instance, symbol));
	if (sup != Nil) {
		Return(push_varcons_ordinary_(local, instance, sup));
	}
	list_heap(&one, symbol, one, init, sup, NULL);
	cons_heap(&key, one, key);
	nextcons_finish(one, cons, base);
	goto key_loop;

allow_argument:
	allow = T;
	nextcons_finish(one, cons, base);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("After &allow-other-keys ~S must be a null.", cons, NULL);

aux_argument:
	nextcons_finish(one, cons, base);
aux_loop:
	Return(ordinary_aux_(one, &symbol, &init));
	Return(push_auxcons_ordinary_(local, instance, symbol));
	list_heap(&one, symbol, init, NULL);
	cons_heap(&aux, one, aux);
	nextcons_finish(one, cons, base);
	goto aux_loop;

finish:
	nreverse(&var, var);
	nreverse(&opt, opt);
	nreverse(&key, key);
	nreverse(&aux, aux);
	list_heap(ret, var, opt, rest, (key != Nil? key: key_p), allow, aux, NULL);
	return 0;
}


/*
 *  lambda_ordinary
 */
static int lambda_ordinary_g_(LocalRoot local, addr *ret, addr cons, addr g)
{
	addr instance, name, var, opt, rest, key, key_p, allow, aux, one;
	addr symbol, init, sup, base;

	base = cons;
	var = opt = rest = key = key_p = allow = aux = Nil;
	lambda_varcons_local(local, &instance);
	lambda_varcons_local(local, &name);
	if (cons == Nil)
		goto finish;
	if (GetType(cons) != LISPTYPE_CONS) {
		return lambda_list_fmte_("Dot list don't allow "
				"in this lambda-list: ~S.", base, NULL);
	}
	GetCar(cons, &one);

var_argument:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_OPTIONAL, one))
		goto optional_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	Return(push_varcons_ordinary_(local, instance, one));
	cons_heap(&var, one, var);
	nextcons_finish(one, cons, base);
	goto var_argument;

optional_argument:
	nextcons_finish(one, cons, base);
optional_loop:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	Return(ordinary_opt_default_(one, g, &symbol, &init, &sup));
	Return(push_varcons_ordinary_(local, instance, symbol));
	if (sup != Nil) {
		Return(push_varcons_ordinary_(local, instance, sup));
	}
	list_heap(&one, symbol, init, sup, NULL);
	cons_heap(&opt, one, opt);
	nextcons_finish(one, cons, base);
	goto optional_loop;

rest_argument:
	GetCdr(cons, &cons);
	if (cons == Nil)
		return lambda_list_fmte_("&rest parameter must be a one argument.", NULL);
	if (GetType(cons) != LISPTYPE_CONS) {
		return lambda_list_fmte_("Dot list don't allow "
				"in this lambda-list: ~S.", base, NULL);
	}
	GetCar(cons, &rest);
	Return(push_varcons_ordinary_(local, instance, rest));
	nextcons_finish(one, cons, base);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("After &rest ~S "
			"must be a null or &key arguments.", cons, NULL);

key_argument:
	key_p = T;
	nextcons_finish(one, cons, base);
key_loop:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ALLOW, one))
		goto allow_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	Return(ordinary_key_default_(local, name, one, g, &symbol, &one, &init, &sup));
	Return(push_varcons_ordinary_(local, instance, symbol));
	if (sup != Nil) {
		Return(push_varcons_ordinary_(local, instance, sup));
	}
	list_heap(&one, symbol, one, init, sup, NULL);
	cons_heap(&key, one, key);
	nextcons_finish(one, cons, base);
	goto key_loop;

allow_argument:
	allow = T;
	nextcons_finish(one, cons, base);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("After &allow-other-keys ~S must be a null.", cons, NULL);

aux_argument:
	nextcons_finish(one, cons, base);
aux_loop:
	Return(ordinary_aux_(one, &symbol, &init));
	Return(push_auxcons_ordinary_(local, instance, symbol));
	list_heap(&one, symbol, init, NULL);
	cons_heap(&aux, one, aux);
	nextcons_finish(one, cons, base);
	goto aux_loop;

finish:
	nreverse(&var, var);
	nreverse(&opt, opt);
	nreverse(&key, key);
	nreverse(&aux, aux);
	list_heap(ret, var, opt, rest, (key != Nil? key: key_p), allow, aux, NULL);
	return 0;
}

int lambda_ordinary_(LocalRoot local, addr *ret, addr cons)
{
	return lambda_ordinary_g_(local, ret, cons, Nil);
}


/*
 *  lambda_defsetf
 */
static int variable_check_defsetf_(addr var)
{
	if (var == Nil || var == T) {
		return lambda_list_fmte_("The symbol ~S "
				"must not be a constant symbol.", var, NULL);
	}
	if (! symbolp(var)) {
		return lambda_list_fmte_("The variable ~S is not a symbol.", var, NULL);
	}
	if (GetStatusReadOnly(var)) {
		return lambda_list_fmte_("The symbol ~S "
				"must not be a constant symbol.", var, NULL);
	}
	if (lambda_constant_eq(CONSTANT_AMPERSAND_OPTIONAL, var))
		return 0;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, var))
		return 0;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, var))
		return 0;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ALLOW, var))
		return 0;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, var))
		return 0;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_AUX, var))
		goto error;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_WHOLE, var))
		goto error;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_BODY, var))
		goto error;
	return 0;

error:
	return lambda_list_fmte_("The symbol ~S don't use in defsetf.", var, NULL);
}

static int push_varcons_defsetf_(LocalRoot local, addr instance, addr var)
{
	addr data;

	Return(variable_check_defsetf_(var));
	lambda_varcons_data(instance, &data);
	if (find_list_eq_unsafe(var, data))
		return lambda_list_fmte_("The variable ~S is already used.", var, NULL);
	pushqueue_local(local, instance, var);

	return 0;
}

#define environment_expander_defsetf(local, instance, env, one, cons) { \
	if (env != Nil) { \
		return lambda_list_fmte_("&environment must be at once in arguments.", NULL); \
	} \
	GetCdr(cons, &cons); \
	if (cons == Nil) { \
		return lambda_list_fmte_("&environment parameter must be a one argument.", NULL); \
	} \
	if (! consp(cons)) { \
		return lambda_list_fmte_("After &environment don't allow a dot list.", NULL); \
	} \
	GetCar(cons, &one); \
	Return(push_varcons_defsetf_(local, instance, one)); \
	env = one; \
}

int lambda_defsetf_(LocalRoot local, addr *ret, addr cons)
{
	addr instance, name, var, opt, rest, key, key_p, allow, one;
	addr env, symbol, init, sup, base;

	base = cons;
	var = opt = rest = key = key_p = allow = env = Nil;
	lambda_varcons_local(local, &instance);
	lambda_varcons_local(local, &name);
	if (cons == Nil)
		goto finish;
	if (! consp(cons)) {
		return lambda_list_fmte_("Dot list don't allow "
				"in this lambda-list: ~S.", base, NULL);
	}
	GetCar(cons, &one);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto var_environment;
	goto var_argument;

var_environment:
	environment_expander_defsetf(local, instance, env, one, cons);
	nextcons_finish(one, cons, base);

var_argument:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_OPTIONAL, one))
		goto optional_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto optional_environment;
	Return(push_varcons_defsetf_(local, instance, one));
	cons_heap(&var, one, var);
	nextcons_finish(one, cons, base);
	goto var_argument;

optional_environment:
	environment_expander_defsetf(local, instance, env, one, cons);
	nextcons_finish(one, cons, base);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_OPTIONAL, one))
		goto optional_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

optional_argument:
	nextcons_finish(one, cons, base);
optional_loop:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto rest_environment;
	Return(ordinary_opt_(one, &symbol, &init, &sup));
	Return(push_varcons_defsetf_(local, instance, symbol));
	if (sup != Nil) {
		Return(push_varcons_defsetf_(local, instance, sup));
	}
	list_heap(&one, symbol, init, sup, NULL);
	cons_heap(&opt, one, opt);
	nextcons_finish(one, cons, base);
	goto optional_loop;

rest_environment:
	environment_expander_defsetf(local, instance, env, one, cons);
	nextcons_finish(one, cons, base);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

rest_argument:
	GetCdr(cons, &cons);
	if (cons == Nil)
		return lambda_list_fmte_("&rest parameter must be a one argument.", NULL);
	if (! consp(cons)) {
		return lambda_list_fmte_("Dot list don't allow "
				"in this lambda-list: ~S.", base, NULL);
	}
	GetCar(cons, &rest);
	Return(push_varcons_defsetf_(local, instance, rest));
	nextcons_finish(one, cons, base);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto key_environment;
	return lambda_list_fmte_("After &rest/&body ~S "
			"must be a null or &key arguments.", one, NULL);

key_environment:
	environment_expander_defsetf(local, instance, env, one, cons);
	nextcons_finish(one, cons, base);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

key_argument:
	key_p = T;
	nextcons_finish(one, cons, base);
key_loop:
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ALLOW, one))
		goto allow_argument;
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto finish_environment;
	Return(ordinary_key_(local, name, one, &symbol, &one, &init, &sup));
	Return(push_varcons_defsetf_(local, instance, symbol));
	if (sup != Nil) {
		Return(push_varcons_defsetf_(local, instance, sup));
	}
	list_heap(&one, symbol, one, init, sup, NULL);
	cons_heap(&key, one, key);
	nextcons_finish(one, cons, base);
	goto key_loop;

allow_argument:
	allow = T;
	nextcons_finish(one, cons, base);
	if (lambda_constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto finish_environment;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

finish_environment:
	environment_expander_defsetf(local, instance, env, one, cons);
	nextcons_finish(one, cons, base);
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

finish:
	nreverse(&var, var);
	nreverse(&opt, opt);
	nreverse(&key, key);
	list_heap(ret, var, opt, rest, (key != Nil? key: key_p), allow, env, NULL);
	return 0;
}

void getenvironment_macro_lambda(addr pos, addr *ret)
{
	addr var, opt, rest, key, allow, aux, whole;
	List_bind(pos, &var, &opt, &rest, &key, &allow, &aux, &whole, ret, NULL);
}

int define_modify_macro_heap_(LocalRoot local, addr *ret, addr *rest, addr list)
{
	addr pos, root, x;
	struct argument_struct *str;

	Return(argument_ordinary_heap_(local, &pos, list));
	str = ArgumentStruct(pos);
	if (str->keyp) {
		return lambda_list_fmte_("Don't use &key arguments "
				"in define-modify-macro lambda list ~S.", list, NULL);
	}
	if (str->aux) {
		return lambda_list_fmte_("Don't use &aux arguments "
				"in define-modify-macro lambda list ~S.", list, NULL);
	}

	/* var */
	root = Nil;
	GetArgument(pos, ArgumentIndex_var, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		cons_heap(&root, x, root);
	}

	/* opt */
	GetArgument(pos, ArgumentIndex_opt, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		GetCar(x, &x);
		cons_heap(&root, x, root);
	}
	nreverse(ret, root);

	/* rest */
	GetArgument(pos, ArgumentIndex_rest, rest);

	return 0;
}


/*
 *  argument
 */
int argumentp(addr pos)
{
	return GetType(pos) == LISPSYSTEM_ARGUMENT;
}

void getargument(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ARGUMENT);
	GetArgument_Low(pos, index, ret);
}

void setargument(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_ARGUMENT);
	SetArgument_Low(pos, index, value);
}

struct argument_struct *argumentstruct(addr pos)
{
	CheckType(pos, LISPSYSTEM_ARGUMENT);
	return ArgumentStruct_Low(pos);
}

void argument_alloc(LocalRoot local, addr *ret)
{
	addr pos;
	struct argument_struct *str;

	alloc_smallsize(local, &pos, LISPSYSTEM_ARGUMENT,
			ArgumentIndex_size, sizeoft(struct argument_struct));
	str = ArgumentStruct(pos);
	clearpoint(str);
	*ret = pos;
}

void argument_local(LocalRoot local, addr *ret)
{
	CheckLocal(local);
	argument_alloc(local, ret);
}

void argument_heap(addr *ret)
{
	argument_alloc(NULL, ret);
}

static int argument_ordinary_heap_g_(LocalRoot local, addr *ret, addr list, addr g)
{
	addr pos, var, opt, rest, key, allow, aux;
	struct argument_struct *str;

	CheckLocal(local);
	/* parse */
	Return(lambda_ordinary_g_(local, &list, list, g));
	List_bind(list, &var, &opt, &rest, &key, &allow, &aux, NULL);
	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_ordinary;
	/* var */
	str->var = length_list_unsafe(var);
	SetArgument(pos, ArgumentIndex_var, var);
	/* opt */
	str->opt = length_list_unsafe(opt);
	SetArgument(pos, ArgumentIndex_opt, opt);
	/* rest */
	if (rest != Nil) {
		str->rest = 1;
		str->restbody = 1;
		SetArgument(pos, ArgumentIndex_rest, rest);
		SetArgument(pos, ArgumentIndex_restbody, rest);
	}
	/* key */
	if (key != Nil) {
		str->keyp = 1;
		str->key = length_list_unsafe(key);
		SetArgument(pos, ArgumentIndex_key, key);
	}
	/* allow-other-keys */
	if (allow != Nil) {
		str->allow = 1;
	}
	/* aux */
	str->aux = length_list_unsafe(aux);
	SetArgument(pos, ArgumentIndex_aux, aux);
	/* result */
	return Result(ret, pos);
}

int argument_ordinary_heap_(LocalRoot local, addr *ret, addr list)
{
	return argument_ordinary_heap_g_(local, ret, list, Nil);
}

int argument_generic_heap_(LocalRoot local, addr *ret, addr list)
{
	addr pos, var, opt, rest, key, allow;
	struct argument_struct *str;

	CheckLocal(local);
	/* parse */
	Return(lambda_generic_function_(local, &list, list));
	List_bind(list, &var, &opt, &rest, &key, &allow, NULL);
	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	/* var */
	str->var = length_list_unsafe(var);
	SetArgument(pos, ArgumentIndex_var, var);
	/* opt */
	str->opt = length_list_unsafe(opt);
	SetArgument(pos, ArgumentIndex_opt, opt);
	/* rest */
	if (rest != Nil) {
		str->rest = 1;
		str->restbody = 1;
		SetArgument(pos, ArgumentIndex_rest, rest);
		SetArgument(pos, ArgumentIndex_restbody, rest);
	}
	/* key */
	if (key == T) {
		str->keyp = 1;
	}
	else if (key != Nil) {
		str->keyp = 1;
		str->key = length_list_unsafe(key);
		SetArgument(pos, ArgumentIndex_key, key);
	}
	/* allow-other-keys */
	if (allow != Nil) {
		str->allow = 1;
	}
	/* result */
	return Result(ret, pos);
}

int argument_method_heap_(LocalRoot local, addr *ret, addr list)
{
	addr pos, var, opt, rest, key, allow, aux;
	struct argument_struct *str;

	CheckLocal(local);
	/* parse */
	Return(lambda_specialized_(local, &list, list));
	List_bind(list, &var, &opt, &rest, &key, &allow, &aux, NULL);
	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = length_list_unsafe(var);
	SetArgument(pos, ArgumentIndex_var, var);
	/* opt */
	str->opt = length_list_unsafe(opt);
	SetArgument(pos, ArgumentIndex_opt, opt);
	/* rest */
	if (rest != Nil) {
		str->rest = 1;
		str->restbody = 1;
		SetArgument(pos, ArgumentIndex_rest, rest);
		SetArgument(pos, ArgumentIndex_restbody, rest);
	}
	/* key */
	if (key == T) {
		str->keyp = 1;
	}
	else if (key != Nil) {
		str->keyp = 1;
		str->key = length_list_unsafe(key);
		SetArgument(pos, ArgumentIndex_key, key);
	}
	/* allow-other-keys */
	if (allow != Nil) {
		str->allow = 1;
	}
	/* aux */
	str->aux = length_list_unsafe(aux);
	SetArgument(pos, ArgumentIndex_aux, aux);
	/* result */
	return Result(ret, pos);
}

int argument_combination_heap_(LocalRoot local, addr *ret, addr list)
{
	addr whole, a, b, check;
	struct argument_struct *str;

	whole = Nil;
	if (! consp(list))
		goto ordinary;
	GetCons(list, &a, &b);
	GetConst(AMPERSAND_WHOLE, &check);
	if (a != check)
		goto ordinary;
	if (! consp(b))
		return lambda_list_fmte_("There is no variable after &whole.", NULL);
	GetCons(b, &whole, &list);
	Return(lambda_variable_check_(whole, AMPERSAND_ORDINARY));

ordinary:
	Return(argument_ordinary_heap_(local, &list, list));
	str = ArgumentStruct(list);
	str->type = ArgumentType_combination;
	if (whole != Nil) {
		str->whole = 1;
		SetArgument(list, ArgumentIndex_whole, whole);
	}
	return Result(ret, list);
}

int argument_boa_heap_(LocalRoot local, addr *ret, addr list, addr g)
{
	struct argument_struct *str;

	Return(argument_ordinary_heap_g_(local, &list, list, g));
	str = ArgumentStruct(list);
	str->type = ArgumentType_boa;
	return Result(ret, list);
}


/*
 *  expand
 */
int argument_generic_lambda_heap_(addr *ret, addr pos)
{
	addr root, list, var, a, b;
	struct argument_struct *str;

	root = Nil;
	str = ArgumentStruct(pos);
	Check(str->type != ArgumentType_generic, "type error");
	/* var */
	if (str->var) {
		GetArgument(pos, ArgumentIndex_var, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			cons_heap(&root, var, root);
		}
	}

	/* opt */
	if (str->opt) {
		GetConst(AMPERSAND_OPTIONAL, &list);
		cons_heap(&root, list, root);
		GetArgument(pos, ArgumentIndex_opt, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			cons_heap(&root, var, root);
		}
	}

	/* rest */
	if (str->rest) {
		GetConst(AMPERSAND_REST, &list);
		cons_heap(&root, list, root);
		GetArgument(pos, ArgumentIndex_rest, &list);
		cons_heap(&root, list, root);
	}

	/* key */
	if (str->keyp || str->key) {
		GetConst(AMPERSAND_KEY, &list);
		cons_heap(&root, list, root);
	}
	if (str->key) {
		GetArgument(pos, ArgumentIndex_key, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			Return(list_bind_(var, &a, &b, NULL));
			list_heap(&var, b, a, NULL);
			cons_heap(&root, var, root);
		}
	}

	/* allow-other-keys */
	if (str->allow) {
		GetConst(AMPERSAND_ALLOW, &list);
		cons_heap(&root, list, root);
	}

	/* result */
	nreverse(ret, root);
	return 0;
}

static int argument_expand_heap_(addr *ret, addr pos)
{
	addr root, list, var, a, b, c, d;
	struct argument_struct *str;

	root = Nil;
	str = ArgumentStruct(pos);
	/* var */
	if (str->var) {
		GetArgument(pos, ArgumentIndex_var, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			if (str->type == ArgumentType_method) {
				GetCar(var, &var);
			}
			cons_heap(&root, var, root);
		}
	}

	/* opt */
	if (str->opt) {
		GetConst(AMPERSAND_OPTIONAL, &list);
		cons_heap(&root, list, root);
		GetArgument(pos, ArgumentIndex_opt, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			Return(list_bind_(var, &a, &b, &c, NULL));
			if (c == Nil)
				list_heap(&var, a, b, NULL);
			else
				list_heap(&var, a, b, c, NULL);
			cons_heap(&root, var, root);
		}
	}

	/* rest */
	if (str->rest) {
		GetConst(AMPERSAND_REST, &list);
		cons_heap(&root, list, root);
		GetArgument(pos, ArgumentIndex_rest, &list);
		cons_heap(&root, list, root);
	}

	/* key */
	if (str->keyp || str->key) {
		GetConst(AMPERSAND_KEY, &list);
		cons_heap(&root, list, root);
	}
	if (str->key) {
		GetArgument(pos, ArgumentIndex_key, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			Return(list_bind_(var, &a, &b, &c, &d, NULL));
			list_heap(&a, b, a, NULL);
			if (d == Nil)
				list_heap(&var, a, c, NULL);
			else
				list_heap(&var, a, c, d, NULL);
			cons_heap(&root, var, root);
		}
	}

	/* allow-other-keys */
	if (str->allow) {
		GetConst(AMPERSAND_ALLOW, &list);
		cons_heap(&root, list, root);
	}

	/* aux */
	if (str->aux) {
		GetConst(AMPERSAND_AUX, &list);
		cons_heap(&root, list, root);
		GetArgument(pos, ArgumentIndex_aux, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			cons_heap(&root, var, root);
		}
	}

	/* result */
	nreverse(ret, root);
	return 0;
}

int argument_ordinary_lambda_heap_(addr *ret, addr pos)
{
	Check(ArgumentStruct(pos)->type != ArgumentType_ordinary, "type error");
	return argument_expand_heap_(ret, pos);
}

int argument_method_lambda_heap_(addr *ret, addr pos)
{
	Check(ArgumentStruct(pos)->type != ArgumentType_method, "type error");
	return argument_expand_heap_(ret, pos);
}

int argument_method_keywords_heap_(addr pos, addr *ret, int *allow)
{
	addr root, list, key;
	struct argument_struct *str;

	str = ArgumentStruct(pos);
	Check(str->type != ArgumentType_method, "type error");
	/* key */
	GetArgument(pos, ArgumentIndex_key, &list);
	for (root = Nil; list != Nil; ) {
		/* (var name init sup) */
		Return_getcons(list, &key, &list);
		Return_getcdr(key, &key);
		Return_getcar(key, &key);
		cons_heap(&root, key, root);
	}
	/* result */
	nreverse(ret, root);
	*allow = (int)str->allow;
	return 0;
}

void argument_method_to_generic(addr method, addr *ret)
{
	addr pos, list, root, var;
	struct argument_struct *str1, *str2;

	/* method */
	str1 = ArgumentStruct(method);
	Check(str1->type != ArgumentType_method, "type error");

	/* generic */
	argument_heap(&pos);
	str2 = ArgumentStruct(pos);
	str2->type = ArgumentType_generic;
	/* var */
	str2->var = str1->var;
	GetArgument(method, ArgumentIndex_var, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		cons_heap(&root, var, root);
	}
	SetArgument(pos, ArgumentIndex_var, root);
	/* opt */
	str2->opt = str1->opt;
	GetArgument(method, ArgumentIndex_opt, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		cons_heap(&root, var, root);
	}
	SetArgument(pos, ArgumentIndex_opt, root);
	/* rest */
	str2->rest = str1->rest;
	str2->restbody = str1->restbody;
	GetArgument(method, ArgumentIndex_rest, &list);
	SetArgument(pos, ArgumentIndex_rest, list);
	/* key */
	str2->key = str1->key;
	str2->keyp = str1->keyp;
	GetArgument(method, ArgumentIndex_key, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		cons_heap(&root, var, root);
	}
	SetArgument(pos, ArgumentIndex_key, root);
	/* aux */
	str2->aux = str1->aux;
	GetArgument(method, ArgumentIndex_aux, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		cons_heap(&root, var, root);
	}
	SetArgument(pos, ArgumentIndex_aux, root);
	/* result */
	*ret = pos;
}

int argument_boa_lambda_heap_(addr *ret, addr pos)
{
	Check(ArgumentStruct(pos)->type != ArgumentType_boa, "type error");
	return argument_expand_heap_(ret, pos);
}

int argument_boa_variables_heap_(addr *ret, addr pos)
{
	addr root, list, var, a, b, c, d;
	struct argument_struct *str;

	root = Nil;
	str = ArgumentStruct(pos);
	/* var */
	if (str->var) {
		GetArgument(pos, ArgumentIndex_var, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			if (str->type == ArgumentType_method) {
				GetCar(var, &var);
			}
			cons_heap(&root, var, root);
		}
	}

	/* opt */
	if (str->opt) {
		GetArgument(pos, ArgumentIndex_opt, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			Return(list_bind_(var, &a, &b, &c, NULL));
			cons_heap(&root, a, root);
		}
	}

	/* rest */
	if (str->rest) {
		GetArgument(pos, ArgumentIndex_rest, &a);
		cons_heap(&root, a, root);
	}

	/* key */
	if (str->key) {
		GetArgument(pos, ArgumentIndex_key, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			Return(list_bind_(var, &a, &b, &c, &d, NULL));
			cons_heap(&root, a, root);
		}
	}

	/* aux */
	if (str->aux) {
		GetArgument(pos, ArgumentIndex_aux, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			GetCar(var, &a);
			cons_heap(&root, a, root);
		}
	}

	/* result */
	nreverse(ret, root);
	return 0;
}


/*
 *  :allow-other-keys
 */
int find_keyword_allow_other_keys(addr list)
{
	addr pos;
	GetConst(KEYWORD_ALLOW_OTHER_KEYS, &pos);
	return getplist_safe(list, pos, &pos) == 0 && pos != Nil;
}


/************************************************************
 *  load_code.c
 ************************************************************/

static void load_array_symbol(addr *ret)
{
	GetConst(SYSTEM_LOAD_ARRAY, ret);
}


/*
 *  execute
 */
void execute_load_alloc(Execute ptr, size_t size)
{
	addr symbol, array;

	load_array_symbol(&symbol);
	vector_heap(&array, size);
	setspecial_local(ptr, symbol, array);
}

int execute_load_gensym_(Execute ptr, addr pos, size_t index)
{
	addr symbol, array;

	/* gensym */
	make_symbol_common(pos, &pos);
	/* set array &*/
	load_array_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &array));
	CheckType(array, LISPTYPE_VECTOR);
	setarray(array, index, pos);

	return 0;
}

int execute_load_set_(Execute ptr, size_t index)
{
	addr symbol, array, value;

	load_array_symbol(&symbol);
	getresult_control(ptr, &value);
	Return(getspecialcheck_local_(ptr, symbol, &array));
	CheckType(array, LISPTYPE_VECTOR);
	setarray(array, index, value);

	return 0;
}

int execute_load_get_(Execute ptr, size_t index, addr *ret)
{
	addr symbol, array;

	load_array_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &array));
	CheckType(array, LISPTYPE_VECTOR);
	getarray(array, index, ret);

	return 0;
}


/*
 *  load-time-value
 */
void code_make_load_alloc(Execute ptr, addr *ret, addr index)
{
	addr code;
	LocalRoot local;
	LocalStack stack;
	struct code_make_struct str;

	local = ptr->local;
	push_local(local, &stack);
	code_queue_local(local, &code);
	set_code_make_struct(&str, ptr, code);

	CodeQueue_cons(&str, LOAD_ALLOC, index);
	code_queue_pop(&str, ret);
	rollback_local(local, stack);
}

void code_make_load_gensym(Execute ptr, addr *ret, addr list)
{
	addr code, x, y;
	LocalRoot local;
	LocalStack stack;
	struct code_make_struct str;

	/* code-begin */
	local = ptr->local;
	push_local(local, &stack);
	code_queue_local(local, &code);
	set_code_make_struct(&str, ptr, code);

	/* code gensym */
	while (list != Nil) {
		GetCons(list, &x, &list);
		GetCons(x, &x, &y);
		GetNameSymbol(x, &x);
		CodeQueue_double(&str, LOAD_GENSYM, x, y);
	}

	/* code-end */
	code_queue_pop(&str, ret);
	rollback_local(local, stack);
}

void code_make_load_set(Execute ptr, addr *ret, addr index)
{
	addr code;
	LocalRoot local;
	LocalStack stack;
	struct code_make_struct str;

	local = ptr->local;
	push_local(local, &stack);
	code_queue_local(local, &code);
	set_code_make_struct(&str, ptr, code);

	CodeQueue_cons(&str, LOAD_SET, index);
	code_queue_pop(&str, ret);
	rollback_local(local, stack);
}


/*
 *  initialize
 */
void fasl_load_time_value(Execute ptr)
{
	addr symbol;

	/* *load-array* */
	load_array_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);
}


/************************************************************
 *  load_depend.c
 ************************************************************/

/*
 *  symbol
 */
static void depend_loop_symbol(addr *ret)
{
	GetConst(SYSTEM_DEPEND_LOOP, ret);
}
static int get_depend_loop_(Execute ptr, addr *ret)
{
	addr symbol;
	depend_loop_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}
static void set_depend_loop(Execute ptr, addr value)
{
	addr symbol;
	depend_loop_symbol(&symbol);
	setspecial_local(ptr, symbol, value);
}

static void depend_pass_symbol(addr *ret)
{
	GetConst(SYSTEM_DEPEND_PASS, ret);
}
static void push_depend_pass(Execute ptr, addr value)
{
	addr symbol;
	depend_pass_symbol(&symbol);
	pushspecial_control(ptr, symbol, value);
}
static int get_depend_pass_(Execute ptr, addr *ret)
{
	addr symbol;
	depend_pass_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}
static void set_depend_pass(Execute ptr, addr value)
{
	addr symbol;
	depend_pass_symbol(&symbol);
	setspecial_local(ptr, symbol, value);
}

static void depend_error_symbol(addr *ret)
{
	GetConst(SYSTEM_DEPEND_ERROR, ret);
}
static void push_depend_error(Execute ptr, addr value)
{
	addr symbol;
	depend_error_symbol(&symbol);
	pushspecial_control(ptr, symbol, value);
}
static int get_depend_error_(Execute ptr, addr *ret)
{
	addr symbol;
	depend_error_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}
static void set_depend_error(Execute ptr, addr value)
{
	addr symbol;
	depend_error_symbol(&symbol);
	setspecial_local(ptr, symbol, value);
}

static void depend_root_symbol(addr *ret)
{
	GetConst(SYSTEM_DEPEND_ROOT, ret);
}

int get_depend_root_(Execute ptr, addr *ret)
{
	addr symbol;
	depend_root_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

static void set_depend_root(Execute ptr, addr value)
{
	addr symbol;
	depend_root_symbol(&symbol);
	setspecial_local(ptr, symbol, value);
}


/*
 *  object
 */
void load_depend_heap(addr *ret, addr stream, addr value, addr index)
{
	addr pos;

	vector2_heap(&pos, 3);
	SetArrayA2(pos, 0, stream);
	SetArrayA2(pos, 1, value);
	SetArrayA2(pos, 2, index);
	*ret = pos;
}

static void empty_load_depend(addr *ret)
{
	load_depend_heap(ret, Nil, Nil, Nil);
}

void get_stream_load_depend(addr pos, addr *ret)
{
	GetArrayA2(pos, 0, ret);
}

void get_index_load_depend(addr pos, addr *ret)
{
	GetArrayA2(pos, 2, ret);
}


/*
 *  *load-depend*
 */
static void load_depend_symbol(addr *ret)
{
	GetConst(SYSTEM_LOAD_DEPEND, ret);
}

static int get_load_depend_(Execute ptr, addr *ret)
{
	addr symbol;
	load_depend_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

static int load_depend_find(addr x, addr y, addr list)
{
	addr x1, y1;

	while (list != Nil) {
		GetCons(list, &x1, &list);
		GetCons(x1, &x1, &y1);
		if (x1 == x && y1 == y)
			return 1;
	}

	return 0;
}

static int load_depend_pushnew_(Execute ptr, addr x, addr y)
{
	addr symbol, list;

	load_depend_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	if (load_depend_find(x, y, list))
		return 0;
	cons_heap(&x, x, y);
	cons_heap(&list, x, list);
	setspecial_local(ptr, symbol, list);

	return 0;
}


/*
 *  *load-push*
 */
static void load_push_symbol(addr *ret)
{
	GetConst(SYSTEM_LOAD_PUSH, ret);
}

void begin_load_push(Execute ptr)
{
	addr symbol;

	load_push_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);
}

int end_load_push_(Execute ptr, addr code)
{
	addr symbol, list, pos;

	load_push_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(load_depend_pushnew_(ptr, code, pos));
	}

	return 0;
}

int push_load_push_(Execute ptr, addr code)
{
	addr symbol, list;

	load_push_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	cons_heap(&list, code, list);
	setspecial_local(ptr, symbol, list);

	return 0;
}


/*
 *  loop
 */
static int load_depend_set_loop_(Execute ptr, addr x, addr y)
{
	addr list, cons;

	Return(get_depend_loop_(ptr, &list));
	if (find_assoc_eq_unsafe(x, list, &cons)) {
		SetCdr(cons, y);
		return 0;
	}

	/* push */
	cons_heap(&cons, x, y);
	cons_heap(&list, cons, list);
	set_depend_loop(ptr, list);

	return 0;
}


/*
 *  depend
 */
int load_depend_code_(Execute ptr, addr stream, addr value)
{
	addr code, pos, symbol, list;

	/* code */
	Return(get_depend_root_(ptr, &code));
	if (code == Nil)
		empty_load_depend(&code);
	load_depend_heap(&pos, stream, value, Nil);
	Return(load_depend_pushnew_(ptr, pos, code));

	/* push */
	load_push_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	while (list != Nil) {
		GetCons(list, &code, &list);
		Return(load_depend_pushnew_(ptr, pos, code));
	}
	set_depend_root(ptr, pos);

	return 0;
}

int load_depend_partial_(Execute ptr, addr stream, addr value, addr *ret)
{
	addr depend, index;

	Return(incf_load_size_(ptr, &index));
	load_depend_heap(&depend, stream, value, index);
	Return(end_load_push_(ptr, depend));

	return Result(ret, depend);
}

int load_depend_instance_(Execute ptr, addr instance, addr make, addr init)
{
	/*  (makefile :instance :make :init)
	 *  (makefile :init :make)
	 *  (makefile-loop :init :instance)
	 */
	Return(load_depend_pushnew_(ptr, instance, make));
	if (init == Nil)
		return 0;
	Return(load_depend_pushnew_(ptr, instance, init));
	Return(load_depend_pushnew_(ptr, init, make));
	Return(load_depend_set_loop_(ptr, init, instance));

	return 0;
}


/*
 *  make
 */
static int compile_depend_error_(Execute ptr, addr code)
{
	addr list;

	Return(get_depend_error_(ptr, &list));
	if (find_list_eq_unsafe(code, list))
		return fmte_("The loop occured by the ~S.", code, NULL);

	return 0;
}

static int compile_depend_pass_(Execute ptr, addr code, int *ret)
{
	int check;
	addr list;

	Return(get_depend_pass_(ptr, &list));
	check = find_list_eq_unsafe(code, list);

	return Result(ret, check);
}

static int compile_depend_loop_(Execute ptr, addr code)
{
	addr list, pos;

	Return(get_depend_loop_(ptr, &list));
	find_assoc_eq_unsafe(code, list, &pos);
	if (pos == Nil)
		return 0;
	GetCdr(pos, &pos);
	if (delete_list_eq_unsafe(code, list, &list))
		set_depend_loop(ptr, list);

	return 0;
}

static int compile_depend_push_error_(Execute ptr, addr code)
{
	addr list;

	Return(get_depend_error_(ptr, &list));
	cons_heap(&list, code, list);
	set_depend_error(ptr, list);

	return 0;
}

static int compile_depend_push_pass_(Execute ptr, addr code)
{
	addr list;

	Return(get_depend_pass_(ptr, &list));
	cons_heap(&list, code, list);
	set_depend_pass(ptr, list);

	return 0;
}

static int compile_depend_gather_(Execute ptr, addr code, addr *ret)
{
	addr root, list, x, y;

	Return(get_load_depend_(ptr, &root));
	list = Nil;
	while (root != Nil) {
		GetCons(root, &x, &root);
		GetCons(x, &x, &y);
		if (code == x)
			cons_heap(&list, y, list);
	}

	return Result(ret, list);
}

static int compile_depend_set_(Execute ptr, addr stream, addr index)
{
	addr pos;
	code_make_load_set(ptr, &pos, index);
	return faslwrite_value_(ptr, stream, pos);
}

static int compile_depend_redirect_(Execute ptr, addr stream, addr code)
{
	int ignore;
	addr memory, index;

	/* ignore */
	get_stream_load_depend(code, &memory);
	if (memory == Nil)
		return 0;

	/* memory */
	Return(file_position_start_stream_(memory, &ignore));
	Return(redirect_unsigned8_stream_(ptr, memory, stream));
	Return(faslwrite_break_(stream));

	/* index */
	get_index_load_depend(code, &index);
	if (index != Nil) {
		Return(compile_depend_set_(ptr, stream, index));
		Return(faslwrite_break_(stream));
	}

	return 0;
}

static int compile_depend_execute_(Execute ptr, addr stream, addr code);
static int compile_depend_execute_let_(Execute ptr, addr stream, addr code)
{
	addr list, pos;

	Return(compile_depend_loop_(ptr, code));
	Return(compile_depend_push_error_(ptr, code));
	Return(compile_depend_push_pass_(ptr, code));
	Return(compile_depend_gather_(ptr, code, &list));
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(compile_depend_execute_(ptr, stream, pos));
	}

	return compile_depend_redirect_(ptr, stream, code);
}

static int compile_depend_execute_(Execute ptr, addr stream, addr code)
{
	int check;
	addr control, list;

	Return(compile_depend_error_(ptr, code));
	Return(compile_depend_pass_(ptr, code, &check));
	if (check)
		return 0;

	Return(get_depend_error_(ptr, &list));
	push_control(ptr, &control);
	push_depend_error(ptr, list);
	(void)compile_depend_execute_let_(ptr, stream, code);
	return pop_control_(ptr, control);
}

int compile_depend_make_(Execute ptr, addr stream, addr code)
{
	addr control;

	push_control(ptr, &control);
	push_depend_pass(ptr, Nil);
	push_depend_error(ptr, Nil);
	/* make */
	(void)compile_depend_execute_(ptr, stream, code);
	return pop_control_(ptr, control);
}


/*
 *  initialize
 */
void init_load_depend(Execute ptr)
{
	addr symbol;

	/* *load-depend* */
	load_depend_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);

	/* *load-push* */
	load_push_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);

	/* *depend-loop* */
	depend_loop_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);

	/* *depend-root* */
	depend_root_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);
}

void disable_load_depend(Execute ptr)
{
	addr symbol;

	/* *load-depend* */
	load_depend_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);

	/* *load-push* */
	load_push_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);

	/* *depend-loop* */
	depend_loop_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);

	/* *depend-root* */
	depend_root_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);
}


/************************************************************
 *  load_gensym.c
 ************************************************************/

static void load_gensym_symbol(addr *ret)
{
	GetConst(SYSTEM_LOAD_GENSYM, ret);
}

void init_load_gensym(Execute ptr)
{
	addr symbol;
	load_gensym_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);
}

void disable_load_gensym(Execute ptr)
{
	addr symbol;
	load_gensym_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);
}

int list_load_gensym_(Execute ptr, addr *ret)
{
	addr symbol;
	load_gensym_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

static int intern_load_gensym_(Execute ptr, addr pos)
{
	addr index, symbol, cons;

	Return(get_load_table_(ptr, pos, &index));
	if (index != Nil)
		return 0;

	/* intern */
	Return(incf_load_size_(ptr, &index));
	Return(intern_load_table_(ptr, pos, index));

	/* push */
	load_gensym_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &cons));
	cons_heap(&pos, pos, index);
	cons_heap(&cons, pos, cons);
	setspecial_local(ptr, symbol, cons);

	return 0;
}


/*
 *  load_value
 */
static int load_value_call_(Execute ptr, addr pos);
static int load_value_cons_(Execute ptr, addr pos)
{
	addr car, cdr;

	CheckType(pos, LISPTYPE_CONS);
	GetCons(pos, &car, &cdr);
	Return(load_value_call_(ptr, car));
	Return(load_value_call_(ptr, cdr));

	return 0;
}

static int load_value_array_(Execute ptr, addr pos)
{
	addr x;
	size_t size, i;
	struct array_struct *str;

	CheckType(pos, LISPTYPE_ARRAY);
	if (! array_general_p(pos))
		return 0;

	str = ArrayInfoStruct(pos);
	size = str->size;
	for (i = 0; i < size; i++) {
		Return(array_get_t_(pos, i, &x));
		Return(load_value_call_(ptr, x));
	}

	return 0;
}

static int load_value_vector_(Execute ptr, addr pos)
{
	addr x;
	size_t size, i;

	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	for (i = 0; i < size; i++) {
		getarray(pos, i, &x);
		Return(load_value_call_(ptr, x));
	}

	return 0;
}

static int load_value_hashtable_(Execute ptr, addr pos)
{
	addr key, value;
	LocalRoot local;
	LocalStack stack;

	CheckType(pos, LISPTYPE_HASHTABLE);
	local = ptr->local;
	push_local(local, &stack);
	hash_iterator_local(local, &pos, pos);
	while (next_hash_iterator(pos, &key, &value)) {
		Return(load_value_call_(ptr, key));
		Return(load_value_call_(ptr, value));
	}
	rollback_local(local, stack);

	return 0;
}

static int load_value_symbol_(Execute ptr, addr pos)
{
	CheckType(pos, LISPTYPE_SYMBOL);
	if (gensymp(pos))
		return intern_load_gensym_(ptr, pos);

	return 0;
}

static int load_value_callname_(Execute ptr, addr pos)
{
	CheckType(pos, LISPTYPE_CALLNAME);
	GetCallName(pos, &pos);
	return load_value_call_(ptr, pos);
}

static int load_value_pathname_(Execute ptr, addr pos)
{
	addr value;

	CheckType(pos, LISPTYPE_PATHNAME);
	/* name */
	GetHostPathname(pos, &value);
	Return(load_value_call_(ptr, value));
	/* device */
	GetDevicePathname(pos, &value);
	Return(load_value_call_(ptr, value));
	/* directory */
	GetDirectoryPathname(pos, &value);
	Return(load_value_call_(ptr, value));
	/* name */
	GetNamePathname(pos, &value);
	Return(load_value_call_(ptr, value));
	/* type */
	GetTypePathname(pos, &value);
	Return(load_value_call_(ptr, value));
	/* version */
	GetVersionPathname(pos, &value);
	Return(load_value_call_(ptr, value));

	return 0;
}

static int load_value_quote_(Execute ptr, addr pos)
{
	addr x, y;

	CheckType(pos, LISPTYPE_QUOTE);
	GetQuote(pos, 0, &x);
	GetQuote(pos, 1, &y);
	Return(load_value_call_(ptr, x));
	Return(load_value_call_(ptr, y));

	return 0;
}

static int load_value_type_(Execute ptr, addr pos)
{
	addr value;
	size_t size, i;

	CheckType(pos, LISPTYPE_TYPE);
	LenArrayType(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayType(pos, i, &value);
		Return(load_value_call_(ptr, value));
	}

	return 0;
}

static int load_value_call_(Execute ptr, addr pos)
{
	Check(! eval_compile_p(ptr), "eval-compile-p error.");

	switch (GetType(pos)) {
		case LISPTYPE_CLOS:
			return intern_load_instance_(ptr, pos);

		case LISPTYPE_CONS:
			return load_value_cons_(ptr, pos);

		case LISPTYPE_ARRAY:
			return load_value_array_(ptr, pos);

		case LISPTYPE_VECTOR:
			return load_value_vector_(ptr, pos);

		case LISPTYPE_HASHTABLE:
			return load_value_hashtable_(ptr, pos);

		case LISPTYPE_SYMBOL:
			return load_value_symbol_(ptr, pos);

		case LISPTYPE_CODE:
			return load_value_code_(ptr, pos);

		case LISPTYPE_CALLNAME:
			return load_value_callname_(ptr, pos);

		case LISPTYPE_PATHNAME:
			return load_value_pathname_(ptr, pos);

		case LISPTYPE_QUOTE:
			return load_value_quote_(ptr, pos);

		case LISPTYPE_TYPE:
			return load_value_type_(ptr, pos);

		default:
			return 0;
	}
}

int load_value_(Execute ptr, addr pos)
{
	if (! eval_compile_p(ptr))
		return 0;
	return load_value_call_(ptr, pos);
}


/*
 *  code
 */
int load_value_code_(Execute ptr, addr code)
{
	addr array, pos;
	struct code_struct *str;
	size_t size, i;

	CheckType(code, LISPTYPE_CODE);
	Check(! eval_compile_p(ptr), "mode error");
	GetArrayCode(code, Code_Array, &array);
	str = StructCode(code);
	size = str->size;

	for (i = 0; i < size; i++) {
		getarray(array, i, &pos);
		Return(load_value_call_(ptr, pos));
	}

	return 0;
}


/************************************************************
 *  load_instance.c
 ************************************************************/

/*
 *  make-load-form initialize
 */
static int make_load_form_lambda_(addr pos, addr g, addr value, addr *ret);
static int make_load_form_lambda_cons_(addr pos, addr g, addr value, addr *ret)
{
	addr car, cdr;

	GetCons(value, &car, &cdr);
	Return(make_load_form_lambda_(pos, g, car, &car));
	Return(make_load_form_lambda_(pos, g, cdr, &cdr));
	cons_heap(ret, car, cdr);

	return 0;
}

static int make_load_form_lambda_vector_(addr pos, addr g, addr value, addr *ret)
{
	addr vector, x;
	size_t size, i;

	lenarray(value, &size);
	vector_type_heap(&vector, value, size);
	for (i = 0; i < size; i++) {
		getarray(value, i, &x);
		Return(make_load_form_lambda_(pos, g, x, &x));
		setarray(vector, i, x);
	}

	return Result(ret, vector);
}

static int make_load_form_lambda_array_heap_(addr *ret, addr value)
{
	addr array, x;
	struct array_struct *str;

	array_empty_heap(&array);
	str = ArrayInfoStruct(array);
	*str = *ArrayInfoStruct(value);
	str->displaced = 0;
	str->simple = str->adjustable == 0 && str->fillpointer == 0;
	str->offset = 0;

	GetArrayInfo(value, ARRAY_INDEX_TYPE, &x);
	SetArrayInfo(array, ARRAY_INDEX_TYPE, x);
	GetArrayInfo(value, ARRAY_INDEX_DIMENSION, &x);
	SetArrayInfo(array, ARRAY_INDEX_DIMENSION, x);
	Return(array_allocate_(NULL, array, str));

	return Result(ret, array);
}

static int make_load_form_lambda_array_copy_(addr pos, addr g, addr array, addr value)
{
	addr x;
	size_t size, i;
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	size = str->front;
	for (i = 0; i < size; i++) {
		Return(array_get_t_(value, i, &x));
		Return(make_load_form_lambda_(pos, g, x, &x));
		Return(array_set_(array, i, x));
	}

	return 0;
}

static int make_load_form_lambda_array_(addr pos, addr g, addr value, addr *ret)
{
	addr array;

	if (array_system_specialized_p(value))
		return Result(ret, value);

	Return(make_load_form_lambda_array_heap_(&array, value));
	Return(make_load_form_lambda_array_copy_(pos, g, array, value));

	return Result(ret, array);
}

static int make_load_form_lambda_(addr pos, addr g, addr value, addr *ret)
{
	if (pos == value)
		return Result(ret, g);

	switch (GetType(value)) {
		case LISPTYPE_CONS:
			return make_load_form_lambda_cons_(pos, g, value, ret);

		case LISPTYPE_VECTOR:
			return make_load_form_lambda_vector_(pos, g, value, ret);

		case LISPTYPE_ARRAY:
			return make_load_form_lambda_array_(pos, g, value, ret);

		default:
			return Result(ret, value);
	}
}

static int make_load_form_replace_(Execute ptr, addr pos, addr init, addr *ret)
{
	addr g, lambda, declare, ignorable;

	if (init == Nil)
		return Result(ret, Nil);

	/* (lambda (g)
	 *   (declare (ignorable g))
	 *   [replace pos -> g])
	 */
	Return(make_gensym_(ptr, &g));
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);

	Return(make_load_form_lambda_(pos, g, init, &init));
	list_heap(&ignorable, ignorable, g, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(&g, g, NULL);
	list_heap(ret, lambda, g, declare, init, NULL);

	return 0;
}


/*
 *  make-load-form generic function
 */
static int make_load_form_generic_call_(
		Execute ptr, LocalHold hold, addr pos, addr *rexpr, addr *rinit)
{
	addr call, expr, init;

	GetConst(COMMON_MAKE_LOAD_FORM, &call);
	GetFunctionSymbol(call, &call);
	Return(funcall_control_(ptr, call, pos, NULL));
	/* result */
	getresult_control(ptr, &expr);
	localhold_set(hold, 0, expr);
	getvalues_control(ptr, 1, &init);
	if (init == Unbound)
		init = Nil;
	localhold_set(hold, 1, init);

	*rexpr = expr;
	*rinit = init;
	return 0;
}

static int make_load_form_generic_(Execute ptr, addr pos, addr *ret1, addr *ret2)
{
	addr control, expr, init;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	/* (make-load-form clos) */
	push_control(ptr, &control);
	expr = init = Nil;
	(void)make_load_form_generic_call_(ptr, hold, pos, &expr, &init);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	*ret1 = expr;
	return make_load_form_replace_(ptr, pos, init, ret2);
}


/*
 *  intern
 */
int intern_load_instance_(Execute ptr, addr pos)
{
	addr index, expr, init;

	Check(! eval_compile_p(ptr), "mode error");

	/* already interned */
	Return(get_load_table_(ptr, pos, &index));
	if (index != Nil)
		return 0;

	/* make-load-form */
	Return(make_load_form_generic_(ptr, pos, &expr, &init));
	return compile_instance_(ptr, pos, expr, init);
}


/************************************************************
 *  load_object.c
 ************************************************************/

/*
 *  load-time-value
 */
void load_time_value_heap(addr *ret, addr value, addr index)
{
	addr pos;

	heap_array2(&pos, LISPTYPE_LOAD_TIME_VALUE, 2);
	SetArrayA2(pos, 0, value);
	SetArrayA2(pos, 1, index);
	*ret = pos;
}

void get_index_load_time_value(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_LOAD_TIME_VALUE);
	GetArrayA2(pos, 1, &pos);
	GetIndex(pos, ret);
}

int result_load_time_value_(Execute ptr, addr pos, addr *ret)
{
	if (GetType(pos) != LISPTYPE_LOAD_TIME_VALUE)
		return Result(ret, pos);

	GetArrayA2(pos, 0, &pos);
	Return(eval_result_partial_(ptr, pos, &pos));
	return Result(ret, pos);
}


/************************************************************
 *  load_time_value.c
 ************************************************************/

/*
 *  *load-table*
 */
static void load_table_symbol(addr *ret)
{
	GetConst(SYSTEM_LOAD_TABLE, ret);
}

int intern_load_table_(Execute ptr, addr pos, addr value)
{
	addr symbol, table;

	load_table_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &table));
	Return(intern_hashheap_(table, pos, &pos));
	SetCdr(pos, value);

	return 0;
}

int get_load_table_(Execute ptr, addr pos, addr *ret)
{
	addr symbol, table;

	load_table_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &table));
	return findnil_hashtable_(table, pos, ret);
}

int get_index_load_table_(Execute ptr, addr pos, size_t *ret)
{
	Return(get_load_table_(ptr, pos, &pos));
	Check(pos == Nil, "nil error");
	GetIndex(pos, ret);

	return 0;
}


/*
 *  *load-size*
 */
static void load_size_symbol(addr *ret)
{
	GetConst(SYSTEM_LOAD_SIZE, ret);
}

int incf_load_size_(Execute ptr, addr *ret)
{
	addr symbol, pos, value;
	size_t size;

	load_size_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &pos));
	Return(oneplus_integer_common_(ptr->local, pos, &value));
	setspecial_local(ptr, symbol, value);
	Return(getindex_integer_(pos, &size));
	index_heap(&pos, size);

	return Result(ret, pos);
}

int get_load_size_(Execute ptr, addr *ret)
{
	addr pos;
	size_t size;

	load_size_symbol(&pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	Return(getindex_integer_(pos, &size));
	index_heap(&pos, size);

	return Result(ret, pos);
}


/*
 *  parse
 */
static int parse_load_time_value_make_(Execute ptr, addr *ret,
		addr form, addr expr, addr readonly, addr index, addr type)
{
	addr eval;

	eval_parse_heap(&eval, EVAL_PARSE_LOAD_TIME_VALUE, 5);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, (readonly != Nil)? T: Nil);
	SetEvalParse(eval, 3, index);
	SetEvalParse(eval, 4, type);

	return Result(ret, eval);
}

static int parse_load_time_value_compile_(Execute ptr, addr *ret,
		addr form, addr expr, addr readonly)
{
	addr index, type;
	Return(compile_partial_(ptr, expr, &index, &type));
	return parse_load_time_value_make_(ptr, ret, form, expr, readonly, index, type);
}

static int parse_load_time_value_eval_(Execute ptr, addr *ret,
		addr form, addr expr, addr readonly)
{
	Return(eval_result_partial_(ptr, expr, &expr));
	return parse_execute_(ptr, ret, expr);
}

int parse_load_time_value_(Execute ptr, addr *ret, addr form)
{
	addr args, expr, readonly;

	/* parse */
	GetCdr(form, &args);
	if (! consp_getcons(args, &expr, &args))
		goto error;
	if (args == Nil)
		readonly = Nil;
	else if (! consp_getcons(args, &readonly, &args))
		goto error;
	if (args != Nil)
		goto error;

	/* mode */
	if (eval_compile_p(ptr))
		return parse_load_time_value_compile_(ptr, ret, form, expr, readonly);
	else
		return parse_load_time_value_eval_(ptr, ret, form, expr, readonly);

error:
	*ret = Nil;
	return fmte_("The form ~S must be "
			"(load-time-value expr &optional read-only-p)).", form, NULL);
}


/*
 *  copy
 */
void copy_eval_load_time_value(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, expr, readonly, index, the;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LOAD_TIME_VALUE, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &readonly);
	GetEvalParse(eval, 3, &index);
	GetEvalParse(eval, 4, &the);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, readonly);
	SetEvalParse(eval, 3, index);
	SetEvalParse(eval, 4, the);
	*ret = eval;
}


/*
 *  scope
 */
int scope_load_time_value_(Execute ptr, addr *ret, addr eval)
{
	addr form, expr, readonly, index, type;

	if (! eval_compile_p(ptr)) {
		*ret = Nil;
		return fmte_("Invalid scope object: load-time-value.", NULL);
	}

	/* parse */
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &readonly);
	GetEvalParse(eval, 3, &index);
	GetEvalParse(eval, 4, &type);

	/* eval */
	Return(eval_scope_size_(ptr, &eval, 3, EVAL_PARSE_LOAD_TIME_VALUE, type, form));
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, index);
	SetEvalScopeIndex(eval, 2, readonly);

	/* result */
	return Result(ret, eval);
}


/*
 *  initialize
 */
static void init_load_symbol(Execute ptr)
{
	addr symbol, pos;

	/* *load-size* */
	load_size_symbol(&symbol);
	fixnum_heap(&pos, 0);
	pushspecial_control(ptr, symbol, pos);

	/* *load-table* */
	load_table_symbol(&symbol);
	hashtable_heap(&pos);
	settest_hashtable(pos, HASHTABLE_TEST_EQ);
	pushspecial_control(ptr, symbol, pos);
}

static void disable_load_symbol(Execute ptr)
{
	addr symbol;

	/* *load-size* */
	load_size_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);

	/* *load-table* */
	load_table_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);
}

void init_load_time_value(Execute ptr)
{
	init_load_symbol(ptr);
	init_load_gensym(ptr);
	init_load_depend(ptr);
}

void disable_load_time_value(Execute ptr)
{
	disable_load_symbol(ptr);
	disable_load_gensym(ptr);
	disable_load_depend(ptr);
}


/************************************************************
 *  local.c
 ************************************************************/

#define LocalLimit		(40UL * 1024UL)

/*
 *  memory error
 */
static void memoryerror_local(void)
{
	Debug("local memory overflow.");
	abort_execute();
}


/*
 *  chain
 */
static void decrement_unbound_local(addr *point, size_t i, size_t count)
{
	byte *p;
	addr pos, value;
	size_t size, k;

	for (; i < count; i++) {
		pos = point[i];
		if (IsArray(pos)) {
			lenarray(pos, &size);
			for (k = 0; k < size; k++) {
				getarray(pos, k, &value);
				if (value != Unbound) {
					p = PtrChain(value);
					if (*p != 0xFF)
						(*p)--;
				}
			}
		}
	}
}

void decrement_local(struct localcell *cell, struct localstack *stack)
{
	if (cell == stack->cell) {
		decrement_unbound_local(cell->point, stack->cellcount, cell->count);
	}
	else {
		decrement_unbound_local(cell->point, 0, cell->count);
		decrement_local(cell->next, stack);
	}
}


#ifdef LISP_MEMORY_MALLOC
/***********************************************************************
 *  Use malloc
 ***********************************************************************/
static void *lowlevel_unsafe(struct localroot *local, size_t size)
{
	struct localmemory *mem;
	void *ptr;

	/* size check */
	if (local->size < local->now + size) {
		Debug("stack overflow.");
		return NULL;
	}

	/* localmemory */
	mem = local->mem;
	if (LocalCount <= mem->count) {
		mem = malloctype(struct localmemory);
		if (mem == NULL)
			return NULL;
#ifdef LISP_DEBUG
		aamemory(mem, sizeoft(struct localmemory));
#endif
		mem->count = 0;
		mem->next = local->mem;
		local->mem = mem;
	}

	/* memory */
	ptr = malloc(size);
	if (ptr == NULL)
		return NULL;
#ifdef LISP_MEMORY_INIT
	aamemory(ptr, size);
#endif
	mem->point[mem->count] = ptr;
	mem->size[mem->count] = size;
	mem->count++;
	local->now += size;

	return ptr;
}

void *lowlevel_local(struct localroot *local, size_t size)
{
	void *ptr;

	ptr = lowlevel_unsafe(local, size);
	if (ptr == NULL)
		memoryerror_local();

	return ptr;
}

static struct localcell *pushcell_local(struct localroot *local)
{
	struct localcell *cell;

	cell = (struct localcell *)lowlevel_local(local, sizeoft(struct localcell));
#ifdef LISP_DEBUG
	aamemory(cell, sizeoft(struct localcell));
#endif
	cell->count = 0;
	cell->next = local->cell;
	local->cell = cell;

	return cell;
}

addr alloc_local(struct localroot *local, size_t size)
{
	struct localcell *cell;
	addr pos;

	cell = local->cell;
	if (LocalCount <= cell->count)
		cell = pushcell_local(local);
	pos = (addr)lowlevel_local(local, size);
	cell->point[cell->count++] = pos;

	return pos;
}

static struct localmemory *make_local_memory(size_t size)
{
	struct localmemory *mem;

	mem = malloctype(struct localmemory);
	if (mem == NULL) {
		Debug("malloctype error");
		return NULL;
	}
#ifdef LISP_MEMORY_INIT
	aamemory(mem, sizeoft(struct localmemory));
#endif
	mem->count = 0;
	mem->next = NULL;

	return mem;
}

static struct localroot *make_local_localroot(struct localmemory *mem, size_t size)
{
	struct localroot *local;

	local = malloctype(struct localroot);
	if (local == NULL) {
		Debug("malloctype error");
		return NULL;
	}
#ifdef LISP_DEBUG
	aamemory(local, sizeoft(struct localroot));
#endif
	local->size = size;
	local->now = 0;
	local->mem = mem;
	local->cell = NULL;
	local->stack = NULL;

	return local;
}

static struct localcell *make_local_localcell(struct localroot *local)
{
	struct localcell *cell;

	cell = (struct localcell *)lowlevel_unsafe(local, sizeoft(struct localcell));
	if (cell == NULL) {
		Debug("lowlevel_unsafe error");
		return NULL;
	}
#ifdef LISP_DEBUG
	aamemory(cell, sizeoft(struct localcell));
#endif
	cell->count = 0;
	cell->next = local->cell;
	local->cell = cell;

	return cell;
}

struct localroot *make_local(size_t size)
{
	struct localmemory *mem;
	struct localcell *cell;
	struct localroot *local;

	mem = NULL;
	cell = NULL;
	local = NULL;
	if (size < LocalLimit)
		size = LocalLimit;

	/* memory */
	mem = make_local_memory(size);
	if (mem == NULL)
		goto error;

	/* localroot */
	local = make_local_localroot(mem, size);
	if (local == NULL)
		goto error;

	/* localcell */
	cell = make_local_localcell(local);
	if (cell == NULL) {
		free_local(local);
		return NULL;
	}

	/* result */
	return local;

error:
	free(mem);
	free(local);
	return NULL;
}

void free_local(struct localroot *local)
{
	void **mem_point;
	struct localmemory *x, *next;
	size_t i, count;

	if (local == NULL)
		return;

	/* localmemory */
	for (x = local->mem; x; x = next) {
		next = x->next;
		count = x->count;
		mem_point = x->point;
		for (i = 0; i < count; i++) {
			free(mem_point[i]);
			mem_point[i] = NULL;
		}
		free(x);
	}

	/* localroot */
	free(local);
}

void push_local(struct localroot *local, struct localstack **ret)
{
	struct localstack *stack;

	stack = (struct localstack *)lowlevel_local(local, sizeof(struct localstack));
	stack->stack = local->stack;
	stack->mem = local->mem;
	stack->cell = local->cell;
	stack->memcount = local->mem->count;
	stack->cellcount = local->cell->count;
	local->stack = stack;
	*ret = stack;
}

static void rollback_memory_local(struct localroot *local, struct localstack *stack)
{
	void **mem_point;
	struct localmemory *x, *y, *next;
	size_t i, count, *mem_size;

	y = stack->mem;
	for (x = local->mem; x != y; x = next) {
		next = x->next;
		count = x->count;
		mem_point = x->point;
		mem_size = x->size;
		for (i = 0; i < count; i++) {
			free(mem_point[i]);
			mem_point[i] = NULL;
			local->now -= mem_size[i];
		}
		free(x);
	}
}

void rollback_local(struct localroot *local, struct localstack *stack)
{
	void **mem_point;
	struct localmemory *local_mem;
	struct localstack save;
	size_t i, count, *mem_size;
#ifdef LISP_DEBUG
	struct localstack *root;

	Check(stack == NULL, "stack error.");
	for (root = local->stack; root == stack; root = root->stack) {
		Check(root == NULL, "rollback_local check error");
	}
#endif
	/*decrement_local(local->cell, stack);*/

	save = *stack;
	rollback_memory_local(local, stack);
	local->stack = save.stack;
	local->mem = save.mem;
	local->cell = save.cell;

	local_mem = local->mem;
	count = local_mem->count;
	mem_point = local_mem->point;
	mem_size = local_mem->size;
	for (i = save.memcount; i < count; i++) {
		free(mem_point[i]);
		local->now -= mem_size[i];
	}
	local_mem->count = save.memcount;
	local->cell->count = save.cellcount;
#ifdef LISP_DEBUG
	for (i = local->cell->count; i < LocalCount; i++)
		local->cell->point[i] = Unbound;
	for (i = local->mem->count; i < LocalCount; i++)
		local->mem->point[i] = Unbound;
#endif
}


#else
/***********************************************************************
 *  Memory pool
 ***********************************************************************/
static void *lowlevel_unsafe(struct localroot *local, size_t size)
{
	addr front, check;

	AlignSize8Front(size, &size);
	front = local->front;
	check = front + size;
	if (local->tail < check) {
		Debug("stack overflow.");
		return NULL;
	}
	local->front = check;
	CheckAlign8(front, "align8 error1");
	CheckAlign8(check, "align8 error2");

	return front;
}

void *lowlevel_local(struct localroot *local, size_t size)
{
	void *ptr;

	ptr = lowlevel_unsafe(local, size);
	if (ptr == NULL)
		memoryerror_local();

	return ptr;
}

static struct localcell *pushcell_local(struct localroot *local)
{
	struct localcell *cell;

	cell = (struct localcell *)lowlevel_local(local, sizeoft(struct localcell));
#ifdef LISP_DEBUG
	aamemory(cell, sizeoft(struct localcell));
#endif
	cell->count = 0;
	cell->next = local->cell;
	local->cell = cell;

	return cell;
}

addr alloc_local(struct localroot *local, size_t size)
{
	struct localcell *cell;
	addr pos;

	cell = local->cell;
	if (LocalCount <= cell->count)
		cell = pushcell_local(local);
	pos = (addr)lowlevel_local(local, size);
	cell->point[cell->count++] = pos;

	return pos;
}

static void *make_local_memory(size_t size)
{
	void *ptr;

	ptr = malloc(size + 8UL);
	if (ptr == NULL) {
		Debug("malloc error");
		return NULL;
	}
#ifdef LISP_MEMORY_INIT
	aamemory(ptr, size);
#endif

	return ptr;
}

static struct localroot *make_local_localroot(void *ptr, size_t size)
{
	struct localroot *local;

	local = malloctype(struct localroot);
	if (local == NULL) {
		Debug("malloctype error");
		return NULL;
	}
#ifdef LISP_DEBUG
	aamemory(local, sizeoft(struct localroot));
#endif
	local->alloc = ptr;
	Align8Front(ptr, &(local->front));
	local->tail = size + ((addr)ptr);
	local->size = size;
	local->stack = NULL;
	local->cell = NULL;
	CheckAlign8(local->front, "align8 error1");

	return local;
}

static struct localcell *make_local_localcell(struct localroot *local)
{
	struct localcell *cell;

	cell = (struct localcell *)lowlevel_unsafe(local, sizeoft(struct localcell));
	if (cell == NULL) {
		Debug("lowlevel_unsafe error");
		return NULL;
	}
#ifdef LISP_DEBUG
	aamemory(cell, sizeoft(struct localcell));
#endif
	cell->count = 0;
	cell->next = local->cell;
	local->cell = cell;
	CheckAlign8(local->front, "align8 error2");

	return cell;
}

struct localroot *make_local(size_t size)
{
	struct localroot *local;
	struct localcell *cell;
	void *ptr;

	if (size < LocalLimit)
		size = LocalLimit;

	/* memory */
	local = NULL;
	cell = NULL;
	ptr = make_local_memory(size);
	if (ptr == NULL)
		goto error;

	/* localroot */
	local = make_local_localroot(ptr, size);
	if (local == NULL)
		goto error;

	/* localcell */
	cell = make_local_localcell(local);
	if (cell == NULL)
		goto error;

	/* result */
	return local;

error:
	free(ptr);
	free(local);
	return NULL;
}

void free_local(struct localroot *local)
{
	if (local == NULL)
		return;
	free(local->alloc);
	local->alloc = NULL;
	free(local);
}

void push_local(struct localroot *local, struct localstack **ret)
{
	struct localstack *stack;

	stack = (struct localstack *)lowlevel_local(local, sizeof(struct localstack));
	stack->stack = local->stack;
	stack->cell = local->cell;
	stack->cellcount = local->cell->count;
	local->stack = stack;
	*ret = stack;
}

void rollback_local(struct localroot *local, struct localstack *stack)
{
#ifdef LISP_DEBUG
	struct localstack *root;
	addr front;
	size_t i;

	front = local->front;
	Check(stack == NULL, "stack error.");
	for (root = local->stack; root == stack; root = root->stack) {
		Check(root == NULL, "rollback_local check error");
	}
#endif
	/*decrement_local(local->cell, stack);*/

	local->front = (addr)stack;
	local->stack = stack->stack;
	local->cell = stack->cell;
	local->cell->count = stack->cellcount;
#ifdef LISP_DEBUG
	memset(local->front, 0xAA, front - local->front);
	for (i = local->cell->count; i < LocalCount; i++)
		local->cell->point[i] = Unbound;
#endif
}
#endif


/***********************************************************************
 *  Allocate
 ***********************************************************************/
static void allocobject(struct localroot *local,
		size_t size, enum LISPTYPE type, addr *ret, int size2)
{
	addr pos;

	/* memory */
	AlignSize8Front(size, &size);
	pos = alloc_local(local, size);

	/* body */
	SetType(pos, (byte)type);
	SetChain(pos, 0);
	if (size2)
		*PtrValue2L(pos) = (byte16)size;
	else
		*PtrValueL(pos) = size;
	*ret = pos;
}

void local_cons(struct localroot *local, addr *ret)
{
	local_array2_memory(local, ret, LISPTYPE_CONS, 2);
}

void local_symbol(struct localroot *local, addr *ret)
{
	local_array2_memory(local, ret, LISPTYPE_SYMBOL, SYMBOL_INDEX_SIZE);
}

void local_array2_memory(struct localroot *local,
		addr *ret, enum LISPTYPE type, byte16 array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA2(array);
	Check(0xFFFFUL < size, "size error");
	allocobject(local, size, type, &pos, 1);
	SetStatusSize(pos, LISPSIZE_ARRAY2, LISPSTATUS_DYNAMIC);
	*PtrLenArrayA2(pos) = array;
	nilarray2(pos, array);
	*ret = pos;
}

void local_array4_memory(struct localroot *local,
		addr *ret, enum LISPTYPE type, byte32 array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA4(array);
	Check(0xFFFFFFFFUL < size, "size error");
	allocobject(local, size, type, &pos, 0);
	SetStatusSize(pos, LISPSIZE_ARRAY4, LISPSTATUS_DYNAMIC);
	*PtrLenArrayA4(pos) = array;
	nilarray4(pos, array);
	*ret = pos;
}

#ifdef LISP_ARCH_64BIT
void local_array8(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA8(array);
	allocobject(local, size, type, &pos, 0);
	SetStatusSize(pos, LISPSIZE_ARRAY8, LISPSTATUS_DYNAMIC);
	*PtrLenArrayA8(pos) = array;
	nilarray8(pos, array);
	*ret = pos;
}
#endif

void local_body2_memory(struct localroot *local,
		addr *ret, enum LISPTYPE type, byte16 body)
{
	addr pos;
	size_t size;

	size = MemoryLengthB2(body);
	Check(0xFFFFUL < size, "size error");
	allocobject(local, size, type, &pos, 1);
	SetStatusSize(pos, LISPSIZE_BODY2, LISPSTATUS_DYNAMIC);
	*PtrLenBodyB2(pos) = body;
	*ret = pos;
}

void local_body4_memory(struct localroot *local,
		addr *ret, enum LISPTYPE type, byte32 body)
{
	addr pos;
	size_t size;

	size = MemoryLengthB4(body);
	Check(0xFFFFFFFFUL < size, "size error");
	allocobject(local, size, type, &pos, 0);
	SetStatusSize(pos, LISPSIZE_BODY4, LISPSTATUS_DYNAMIC);
	*PtrLenBodyB4(pos) = body;
	*ret = pos;
}

#ifdef LISP_ARCH_64BIT
void local_body8(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t body)
{
	addr pos;
	size_t size;

	size = MemoryLengthB8(body);
	allocobject(local, size, type, &pos, 0);
	SetStatusSize(pos, LISPSIZE_BODY8, LISPSTATUS_DYNAMIC);
	*PtrLenBodyB8(pos) = body;
	*ret = pos;
}
#endif

void local_smallsize_memory(struct localroot *local,
		addr *ret, enum LISPTYPE type, byte array, byte body)
{
	addr pos;
	size_t size;

	size = MemoryLengthSS(array, body);
	Check(0xFFFFUL < size, "size error");
	allocobject(local, size, type, &pos, 1);
	SetStatusSize(pos, LISPSIZE_SMALLSIZE, LISPSTATUS_DYNAMIC);
	nilarray2(pos, array);
	*PtrLenArraySS(pos) = array;
	*PtrLenBodySS(pos) = body;
	*ret = pos;
}

void local_arraybody_memory(struct localroot *local,
		addr *ret, enum LISPTYPE type, byte16 array, byte16 body)
{
	addr pos;
	size_t size;

	size = MemoryLengthAB(array, body);
	Check(0xFFFFFFFFUL < size, "size error");
	allocobject(local, size, type, &pos, 0);
	SetStatusSize(pos, LISPSIZE_ARRAYBODY, LISPSTATUS_DYNAMIC);
	nilarray4(pos, array);
	*PtrLenArrayAB(pos) = array;
	*PtrLenBodyAB(pos) = body;
	*ret = pos;
}

void local_array4_unbound_memory(struct localroot *local,
		addr *ret, enum LISPTYPE type, byte32 array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA4(array);
	Check(0xFFFFFFFFUL < size, "size error");
	allocobject(local, size, type, &pos, 0);
	SetStatusSize(pos, LISPSIZE_ARRAY4, LISPSTATUS_DYNAMIC);
	*PtrLenArrayA4(pos) = array;
	unboundarray4(pos, array);
	*ret = pos;
}

void local_array(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array)
{
#ifdef LISP_ARCH_64BIT
	if (MemoryLengthA2(array) <= 0xFFFFUL)
		local_array2_memory(local, ret, type, (byte16)array);
	else if (MemoryLengthA4(array) <= 0xFFFFFFFFUL)
		local_array4_memory(local, ret, type, (byte32)array);
	else
		local_array8(local, ret, type, array);
#else
	if (MemoryLengthA2(array) <= 0xFFFFUL)
		local_array2_memory(local, ret, type, (byte16)array);
	else
		local_array4_memory(local, ret, type, array);
#endif
}

void local_body(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t body)
{
#ifdef LISP_ARCH_64BIT
	if (MemoryLengthB2(body) <= 0xFFFFUL)
		local_body2_memory(local, ret, type, (byte16)body);
	else if (MemoryLengthB4(body) <= 0xFFFFFFFFUL)
		local_body4_memory(local, ret, type, (byte32)body);
	else
		local_body8(local, ret, type, body);
#else
	if (MemoryLengthB2(body) <= 0xFFFFUL)
		local_body2_memory(local, ret, type, (byte16)body);
	else
		local_body4_memory(local, ret, type, body);
#endif
}

#ifdef LISP_DEBUG
void local_array2_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFUL < array, "size error");
	local_array2_memory(local, ret, type, (byte16)array);
}
void local_array4_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	local_array4_memory(local, ret, type, (byte32)array);
}
void local_body2_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFUL < body, "size error");
	local_body2_memory(local, ret, type, (byte16)body);
}
void local_body4_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFFFFFUL < body, "size error");
	local_body4_memory(local, ret, type, (byte32)body);
}
void local_smallsize_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFUL < array, "array size error");
	Check(0xFFUL < body, "body size error");
	local_smallsize_memory(local, ret, type, (byte)array, (byte)body);
}
void local_arraybody_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFFFUL < array, "array size error");
	Check(0xFFFFUL < body, "body size error");
	local_arraybody_memory(local, ret, type, (byte16)array, (byte16)body);
}
void local_array4_unbound_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	local_array4_unbound_memory(local, ret, type, (byte32)array);
}
#endif


/************************************************************
 *  localtime.c
 ************************************************************/

#if defined(LISP_WINDOWS)
/*****************************************************************************
 *  Windows
 *****************************************************************************/
#include <windows.h>

void init_localtime(void)
{
	_tzset();
}
int gmtime_arch(struct tm *ret, const time_t *time)
{
	return gmtime_s(ret, time) != 0;
}
int localtime_arch(struct tm *ret, const time_t *time)
{
	return localtime_s(ret, time) != 0;
}

#elif defined(LISP_UNIX)
/*****************************************************************************
 *  Unix
 *****************************************************************************/
#define _BSD_SOURCE 1
void init_localtime(void)
{
	tzset();
}
int gmtime_arch(struct tm *ret, const time_t *time)
{
	return gmtime_r(time, ret) == NULL;
}
int localtime_arch(struct tm *ret, const time_t *time)
{
	return localtime_r(time, ret) == NULL;
}

#elif defined(__STDC_LIB_EXT1__)
/*****************************************************************************
 *  C11
 *****************************************************************************/
#define __STDC_WANT_LIB_EXT1__ 1
void init_localtime(void)
{
	tzset();
}
int gmtime_arch(struct tm *ret, const time_t *time)
{
	return gmtime_s(time, ret) == NULL;
}
int localtime_arch(struct tm *ret, const time_t *time)
{
	return localtime_s(time, ret) == NULL;
}

#else
/*****************************************************************************
 *  C99
 *****************************************************************************/
void init_localtime(void)
{
	/* Don't execute tzset() */
}
int gmtime_arch(struct tm *ret, const time_t *time)
{
	struct tm *check;

	check = gmtime(time);
	if (check == NULL)
		return 1;
	*ret = *check;

	return 0;
}
int localtime_arch(struct tm *ret, const time_t *time)
{
	struct tm *check;

	check = localtime(time);
	if (check == NULL)
		return 1;
	*ret = *check;

	return 0;
}
#endif


/*
 *  localtime
 */
#ifdef LISP_WINDOWS
int nowtime_string(char *ptr, size_t size)
{
	SYSTEMTIME st;

	GetLocalTime(&st);
	snprintf(ptr, size, "%04d/%02d/%02d-%2d:%02d:%02d",
			st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute, st.wSecond);

	return 0;
}
#else
int nowtime_string(char *ptr, size_t size)
{
	time_t now;
	struct tm str;

	now = time(NULL);
	if (now == (time_t)-1)
		return 1;
	if (localtime_arch(&str, &now))
		return 1;
	snprintf(ptr, size, "%04d/%02d/%02d-%2d:%02d:%02d",
			str.tm_year + 1900, str.tm_mon + 1, str.tm_mday,
			str.tm_hour, str.tm_min, str.tm_sec);

	return 0;
}
#endif


/************************************************************
 *  loop.c
 ************************************************************/

/*
 *  extended
 */
static void loop_expand_macrolet(addr *ret, addr form)
{
	/* `(macrolet ((loop-finish () (quote (go end-loop))))
	 *    ,form)
	 */
	addr macrolet, loop_finish, quote, go, end_loop;

	GetConst(COMMON_MACROLET, &macrolet);
	GetConst(COMMON_LOOP_FINISH, &loop_finish);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	list_heap(&go, go, end_loop, NULL);
	list_heap(&quote, quote, go, NULL);
	list_heap(&loop_finish, loop_finish, Nil, quote, NULL);
	list_heap(&loop_finish, loop_finish, NULL);
	list_heap(ret, macrolet, loop_finish, form, NULL);
}

static int loop_expand_symbol_(Execute ptr, addr *ret, addr form)
{
	/* `(let (value-loop it-loop function-loop ,@vars)
	 *    (declare (ignorable it-loop ,@vars))
	 *    ,form)
	 */
	addr value_loop, it_loop, function_loop;
	addr let, declare, ignorable, vars, x;

	GetConst(SYSTEM_VALUE_LOOP, &value_loop);
	GetConst(SYSTEM_IT_LOOP, &it_loop);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	Return(getvars_expand_loop_(ptr, &vars));

	lista_heap(&x, value_loop, it_loop, function_loop, vars, NULL);
	lista_heap(&ignorable, ignorable, it_loop, vars, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(ret, let, x, declare, form, NULL);

	return 0;
}

static int loop_expand_let_(Execute ptr, addr *ret, addr form)
{
	addr list, pos, root, x;

	Return(getlet_loop_(ptr, &list));
	while (list != Nil) {
		GetCons(list, &pos, &list);
		root = Nil;
		while (pos != Nil) {
			GetCons(pos, &x, &pos);
			cons_heap(&root, x, root);
		}
		cons_heap(&root, form, root);
		nreverse(&form, root);
	}

	return Result(ret, form);
}

static void loop_expand_return_from(addr *ret, addr named)
{
	/*  (return-from ,named
	 *    (if function-loop
	 *      (funcall function-loop value-loop)
	 *      value-loop))
	 */
	addr return_from, if_symbol, funcall, function_loop, value_loop;

	GetConst(COMMON_RETURN_FROM, &return_from);
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_FUNCALL, &funcall);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);

	list_heap(&funcall, funcall, function_loop, value_loop, NULL);
	list_heap(&if_symbol, if_symbol, function_loop, funcall, value_loop, NULL);
	list_heap(ret, return_from, named, if_symbol, NULL);
}

static int loop_expand_form_(Execute ptr, addr *ret)
{
	/* `(block ,named
	 *    (tagbody
	 *      (progn ,@initially)
	 *      next-loop
	 *      ,@form
	 *      (go next-loop)
	 *      end-loop
	 *      (progn ,@finally)
	 *      ,return-from))
	 */
	addr let, block, tagbody, progn, go;
	addr next_loop, end_loop, x, list;
	addr named, init, final, form;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_NEXT_LOOP, &next_loop);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	Return(getnamed_loop_(ptr, &named));
	Return(getinit_expand_loop_(ptr, &init));
	Return(getfinal_expand_loop_(ptr, &final));
	Return(getform_expand_loop_(ptr, &form));

	/* tagbody */
	conscar_heap(&list, tagbody);
	/* initially */
	if (init != Nil) {
		cons_heap(&init, progn, init);
		cons_heap(&list, init, list);
	}
	/* next-loop */
	cons_heap(&list, next_loop, list);
	/* form */
	while (form != Nil) {
		GetCons(form, &x, &form);
		cons_heap(&list, x, list);
	}
	/* go */
	list_heap(&go, go, next_loop, NULL);
	cons_heap(&list, go, list);
	/* end-loop */
	cons_heap(&list, end_loop, list);
	/* finally */
	if (final != Nil) {
		cons_heap(&final, progn, final);
		cons_heap(&list, final, list);
	}
	/* return-from */
	loop_expand_return_from(&x, named);
	cons_heap(&list, x, list);

	/* form */
	nreverse(&tagbody, list);
	list_heap(&form, block, named, tagbody, NULL);
	Return(loop_expand_let_(ptr, &form, form));
	Return(loop_expand_symbol_(ptr, &form, form));
	loop_expand_macrolet(ret, form);

	return 0;
}


/*
 *  compose
 */
static int loop_extended_common_(Execute ptr, addr *ret, addr vars, addr main)
{
	Return(loop_variables_(ptr, vars));
	Return(loop_main_(ptr, main));
	Return(loop_expand_form_(ptr, ret));

	return 0;
}


/*
 *  main
 */
static void loop_simple_common(addr *ret, addr form)
{
	/* `(do () (nil) ,@form) */
	addr x, y;

	GetConst(COMMON_DO, &x);
	list_heap(&y, Nil, NULL);
	lista_heap(ret, x, Nil, y, form, NULL);
}

static int loop_execute_common_(Execute ptr, addr *ret, addr list)
{
	addr named, vars, main;
	LocalHold hold;

	/* (loop) */
	Return_getcdr(list, &list);
	if (list == Nil) {
		loop_simple_common(ret, Nil);
		return 0;
	}

	/* clause */
	vars = main = Nil;
	Return(loop_parse_common_(ptr, &named, &vars, &main, &list));

	/* simple-loop */
	if (named == Nil && vars == Nil && main == Nil) {
		loop_simple_common(ret, list);
		return 0;
	}

	/* error */
	if (list != Nil)
		return fmte_("Invalid loop form ~S.", list, NULL);

	/* extended-loop */
	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, vars, main, NULL);
	Return(loop_extended_common_(ptr, ret, vars, main));
	localhold_end(hold);

	return 0;
}

int loop_common_(Execute ptr, addr *ret, addr list)
{
	addr control;

	push_control(ptr, &control);
	loop_push_special(ptr);
	(void)loop_execute_common_(ptr, ret, list);
	return pop_control_(ptr, control);
}


/************************************************************
 *  loop_bind.c
 ************************************************************/

/*
 *  loop-bind
 */
static int loop_subtypep_(Execute ptr, addr a, addr b, int *ret)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva(hold, a, b, NULL);
	if (GetType(a) != LISPTYPE_TYPE) {
		Return(parse_type_(ptr, &a, a, Nil));
		localhold_push(hold, a);
	}
	if (GetType(b) != LISPTYPE_TYPE) {
		Return(parse_type_(ptr, &b, b, Nil));
		localhold_push(hold, b);
	}
	localhold_end(hold);
	Return(subtypep_check_(ptr, a, b, Nil, ret, NULL));

	return 0;
}

static int loop_bind_initial_(Execute ptr, addr type, addr *ret)
{
	int check;
	addr right;

	if (type == Unbound)
		return Result(ret, Nil);
	if (GetType(type) != LISPTYPE_TYPE) {
		Return(parse_type_(ptr, &type, type, Nil));
	}
	/* float */
	GetConst(COMMON_FLOAT, &right);
	Return(loop_subtypep_(ptr, type, right, &check));
	if (check) {
		single_float_heap(ret, 0.0f);
		return 0;
	}
	/* integer */
	GetConst(COMMON_NUMBER, &right);
	Return(loop_subtypep_(ptr, type, right, &check));
	if (check) {
		fixnum_heap(ret, 0);
		return 0;
	}
	else {
		/* others */
		return Result(ret, Nil);
	}
}

static int loop_bind_initial_recursive_(Execute ptr, addr var, addr type, addr *ret)
{
	addr var1, var2, type1, type2, value1, value2;
	LocalHold hold;

	/* variable */
	if (var == Nil)
		return Result(ret, Nil);
	if (! consp(var))
		return loop_bind_initial_(ptr, type, ret);
	GetCons(var, &var1, &var2);
	/* type */
	if (consp(type)) {
		GetCons(type, &type1, &type2);
		if (type1 == Nil)
			type1 = type;
		if (type2 == Nil)
			type2 = type;
	}
	else {
		type1 = type2 = type;
	}
	/* initial value */
	Return(loop_bind_initial_recursive_(ptr, var1, type1, &value1));
	hold = LocalHold_local_push(ptr, value1);
	Return(loop_bind_initial_recursive_(ptr, var2, type2, &value2));
	localhold_end(hold);
	cons_heap(ret, value1, value2);

	return 0;
}

int loop_bind_initial_list_(Execute ptr, addr var, addr type, addr *ret)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, var, type, NULL);
	Return(loop_bind_initial_recursive_(ptr, var, type, &var));
	localhold_end(hold);

	if (consp(var))
		quotelist_heap(&var, var);

	return Result(ret, var);
}

static int loop_typep_(Execute ptr, addr pos, addr value, addr type)
{
	int check;

	if (GetType(type) != LISPTYPE_TYPE) {
		Return(parse_type_(ptr, &type, type, Nil));
	}
	Return(typep_clang_(ptr, value, type, &check));
	if (! check) {
		Return(type_object_(&type, type));
		return fmte_("LOOP let ~A form ~S must be a ~A type.", pos, value, type, NULL);
	}

	return 0;
}

static int loop_bind_recursive_(Execute ptr, addr pos, addr type, addr value, addr *ret)
{
	addr pos1, pos2, type1, type2, value1, value2;
	LocalHold hold;

	Check(pos == Nil, "type error");
	/* symbol */
	if (! consp(pos)) {
		Check(! symbolp(pos), "type error");
		Return(loop_typep_(ptr, pos, value, type));
		return Result(ret, value);
	}
	/* cons */
	GetCons(pos, &pos1, &pos2);
	if (consp(type)) {
		GetCons(type, &type1, &type2);
		if (type1 == Nil)
			type1 = type;
		if (type2 == Nil)
			type2 = type;
	}
	else {
		type1 = type2 = type;
	}

	/* default */
	hold = LocalHold_local(ptr);
	if (value == Nil) {
		if (pos1 == Nil) {
			value1 = Nil;
		}
		else {
			Return(loop_bind_initial_(ptr, type1, &value1));
		}
		if (pos2 == Nil) {
			value2 = Nil;
		}
		else {
			Return(loop_bind_initial_(ptr, type2, &value2));
		}
	}
	/* cons */
	else if (consp(value)) {
		GetCons(value, &value1, &value2);
		if (pos1 == Nil) {
			value1 = Nil;
		}
		else {
			Return(loop_bind_recursive_(ptr, pos1, type1, value1, &value1));
		}
		localhold_push(hold, value1);
		if (pos2 == Nil) {
			value2 = Nil;
		}
		else {
			Return(loop_bind_recursive_(ptr, pos2, type2, value2, &value2));
		}
		localhold_push(hold, value2);
	}
	/* error */
	else {
		return fmte_("LOOP let ~A form ~S must be a list type.", pos, value, NULL);
	}
	/* type check */
	if (! listp(pos1)) {
		Return(loop_typep_(ptr, pos1, value1, type1));
	}
	if (! listp(pos2)) {
		Return(loop_typep_(ptr, pos2, value2, type2));
	}
	localhold_end(hold);
	cons_heap(ret, value1, value2);

	return 0;
}

int loop_bind_common_(Execute ptr, addr pos, addr type, addr value, addr *ret)
{
	LocalHold hold;

	if (pos == Nil)
		return Result(ret, Nil);
	if (! listp(pos))
		return fmte_("LIST-BIND argument ~S must be a list type.", pos, NULL);
	if (type == Unbound)
		pos = T;

	hold = LocalHold_local(ptr);
	localhold_pushva(hold, pos, type, value, NULL);
	Return(loop_bind_recursive_(ptr, pos, type, value, ret));
	localhold_end(hold);

	return 0;
}


/************************************************************
 *  loop_main.c
 ************************************************************/

/*
 *  initially
 */
static int loop_main_initially_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(push_init_loop_(ptr, pos));
	}

	return 0;
}


/*
 *  finally
 */
static int loop_main_finally_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(push_final_loop_(ptr, pos));
	}

	return 0;
}


/*
 *  do
 */
static int loop_main_do_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(push_form_loop_(ptr, pos));
	}

	return 0;
}


/*
 *  return
 */
static int loop_main_return_(Execute ptr, addr list)
{
	addr return_from, named, expr, x;

	GetConst(COMMON_RETURN_FROM, &return_from);
	Return(getnamed_loop_(ptr, &named));
	Return(list_bind_(list, &expr, NULL));

	/* `(return-from ,named ,expr) */
	list_heap(&x, return_from, named, expr, NULL);
	return push_form_loop_(ptr, x);
}


/*
 *  if, unless
 */
static int loop_main_if_unless_then_(Execute ptr, addr car, addr form, addr expr1)
{
	/* `(setq it-loop ,form)
	 *  (if/unless it-loop
	 *    (go #:end-if))
	 *  ,@expr1
	 *  #:end-if
	 */
	addr setq, go, it_loop, end_if, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_IT_LOOP, &it_loop);
	make_symbolchar(&end_if, "END-IF");

	list_heap(&x, setq, it_loop, form, NULL);
	Return(push_form_loop_(ptr, x));
	list_heap(&x, go, end_if, NULL);
	list_heap(&x, car, it_loop, x, NULL);
	Return(push_form_loop_(ptr, x));
	Return(loop_main_(ptr, expr1));
	return push_form_loop_(ptr, end_if);
}

static int loop_main_if_unless_else_(Execute ptr,
		addr car, addr form, addr expr1, addr expr2)
{
	/* `(setq it-loop ,form)
	 *  (car it-loop (go #:else-if))
	 *    ,@expr1
	 *  (go #:end-if)
	 *  #:else-if
	 *  ,@expr2
	 *  #:end-if
	 */
	addr setq, go, it_loop, else_if, end_if, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_IT_LOOP, &it_loop);
	make_symbolchar(&else_if, "ELSE-IF");
	make_symbolchar(&end_if, "END-IF");

	list_heap(&x, setq, it_loop, form, NULL);
	Return(push_form_loop_(ptr, x));
	list_heap(&x, go, else_if, NULL);
	list_heap(&x, car, it_loop, x, NULL);
	Return(push_form_loop_(ptr, x));
	Return(loop_main_(ptr, expr1));
	list_heap(&x, go, end_if, NULL);
	Return(push_form_loop_(ptr, x));
	Return(push_form_loop_(ptr, else_if));
	Return(loop_main_(ptr, expr2));
	return push_form_loop_(ptr, end_if);
}

static int loop_main_if_unless_(Execute ptr, addr list, constindex index)
{
	addr car, form, expr1, expr2;

	GetConstant(index, &car);
	Return(list_bind_(list, &form, &expr1, &expr2, NULL));
	if (expr2 == Unbound)
		return loop_main_if_unless_then_(ptr, car, form, expr1);
	else
		return loop_main_if_unless_else_(ptr, car, form, expr1, expr2);
}

static int loop_main_if_(Execute ptr, addr list)
{
	return loop_main_if_unless_(ptr, list, CONSTANT_COMMON_UNLESS);
}

static int loop_main_unless_(Execute ptr, addr list)
{
	return loop_main_if_unless_(ptr, list, CONSTANT_COMMON_IF);
}


/*
 *  collect
 */
static int loop_main_collect_nointo_(Execute ptr, addr form)
{
	addr setq, function_loop, function_symbol, nreverse;
	addr push, value_loop, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);
	GetConst(COMMON_FUNCTION, &function_symbol);
	GetConst(COMMON_NREVERSE, &nreverse);
	GetConst(COMMON_PUSH, &push);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);

	/* `(setq function-loop (function nreverse)) */
	list_heap(&x, function_symbol, nreverse, NULL);
	list_heap(&x, setq, function_loop, x, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(push ,form value-loop) */
	list_heap(&x, push, form, value_loop, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_collect_into_(Execute ptr, addr form, addr into)
{
	/* `(setq ,into (append ,into (list ,form))) */
	addr setq, append, list, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_APPEND, &append);
	GetConst(COMMON_LIST, &list);
	Return(push_vars_loop_(ptr, into));
	list_heap(&list, list, form, NULL);
	list_heap(&append, append, into, list, NULL);
	list_heap(&x, setq, into, append, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_collect_(Execute ptr, addr list)
{
	addr form, into;

	Return(list_bind_(list, &form, &into, NULL));
	if (into == Unbound)
		return loop_main_collect_nointo_(ptr, form);
	else
		return loop_main_collect_into_(ptr, form, into);
}


/*
 *  append
 */
static int loop_main_append_nointo_(Execute ptr, addr form)
{
	addr setq, function_loop, function_symbol, nreverse;
	addr dolist, push, value_loop, g, x, y;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);
	GetConst(COMMON_FUNCTION, &function_symbol);
	GetConst(COMMON_NREVERSE, &nreverse);
	GetConst(COMMON_DOLIST, &dolist);
	GetConst(COMMON_PUSH, &push);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);

	/* `(setq function-loop (function nreverse)) */
	list_heap(&x, function_symbol, nreverse, NULL);
	list_heap(&x, setq, function_loop, x, NULL);
	Return(push_init_loop_(ptr, x));

	/* `(dolist (x ,form)
	 *    (push x value-loop))
	 */
	make_symbolchar(&g, "X");
	list_heap(&x, g, form, NULL);
	list_heap(&y, push, g, value_loop, NULL);
	list_heap(&x, dolist, x, y, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_append_into_(Execute ptr, addr form, addr into)
{
	addr setq, append, x;

	/* `(setq ,into (append ,into ,form)) */
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_APPEND, &append);

	Return(push_vars_loop_(ptr, into));
	list_heap(&x, append, into, form, NULL);
	list_heap(&x, setq, into, x, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_append_(Execute ptr, addr list)
{
	addr form, into;

	Return(list_bind_(list, &form, &into, NULL));
	if (into == Unbound)
		return loop_main_append_nointo_(ptr, form);
	else
		return loop_main_append_into_(ptr, form, into);
}


/*
 *  nconc
 */
static int loop_main_nconc_nointo_(Execute ptr, addr form)
{
	addr setq, function_loop, function_symbol, nreverse, nreconc, value_loop, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);
	GetConst(COMMON_FUNCTION, &function_symbol);
	GetConst(COMMON_NREVERSE, &nreverse);
	GetConst(COMMON_NRECONC, &nreconc);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);

	/* `(setq function-loop (function nreverse)) */
	list_heap(&x, function_symbol, nreverse, NULL);
	list_heap(&x, setq, function_loop, x, NULL);
	Return(push_init_loop_(ptr, x));

	/* `(setq value-loop (nreconc ,form value-loop)) */
	list_heap(&x, nreconc, form, value_loop, NULL);
	list_heap(&x, setq, value_loop, x, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_nconc_into_(Execute ptr, addr form, addr into)
{
	addr setq, nconc, x;

	/* `(setq ,into (nconc ,into ,form)) */
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_NCONC, &nconc);

	Return(push_vars_loop_(ptr, into));
	list_heap(&x, nconc, into, form, NULL);
	list_heap(&x, setq, into, x, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_nconc_(Execute ptr, addr list)
{
	addr form, into;

	Return(list_bind_(list, &form, &into, NULL));
	if (into == Unbound)
		return loop_main_nconc_nointo_(ptr, form);
	else
		return loop_main_nconc_into_(ptr, form, into);
}


/*
 *  count
 */
static int loop_main_count_nointo_(Execute ptr, addr form)
{
	addr setq, value_loop, function_loop, if_symbol, incf, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_INCF, &incf);

	/* `(setq value_loop 0 function-loop nil) */
	fixnum_heap(&x, 0);
	list_heap(&x, setq, value_loop, x, function_loop, Nil, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(if ,form (incf value_loop)) */
	list_heap(&incf, incf, value_loop, NULL);
	list_heap(&x, if_symbol, form, incf, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_count_into_(Execute ptr, addr form, addr into)
{
	addr setq, if_symbol, incf, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_INCF, &incf);

	Return(push_vars_loop_(ptr, into));
	/* `(setq ,into 0) */
	fixnum_heap(&x, 0);
	list_heap(&x, setq, into, x, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(if ,form (incf ,into)) */
	list_heap(&incf, incf, into, NULL);
	list_heap(&x, if_symbol, form, incf, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_count_(Execute ptr, addr list)
{
	addr form, into, type;

	Return(list_bind_(list, &form, &into, &type, NULL));
	if (into == Unbound)
		return loop_main_count_nointo_(ptr, form);
	else
		return loop_main_count_into_(ptr, form, into);
}


/*
 *  sum
 */
static int loop_main_sum_nointo_(Execute ptr, addr form)
{
	addr setq, value_loop, function_loop, incf, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);
	GetConst(COMMON_INCF, &incf);

	/* `(setq value-loop 0 function-loop nil) */
	fixnum_heap(&x, 0);
	list_heap(&x, setq, value_loop, x, function_loop, Nil, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(incf value_loop ,form) */
	list_heap(&x, incf, value_loop, form, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_sum_into_(Execute ptr, addr form, addr into)
{
	addr setq, incf, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_INCF, &incf);

	Return(push_vars_loop_(ptr, into));
	/* `(setq ,into 0) */
	fixnum_heap(&x, 0);
	list_heap(&x, setq, into, x, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(incf ,into ,form) */
	list_heap(&x, incf, into, form, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_sum_(Execute ptr, addr list)
{
	addr form, into, type;

	Return(list_bind_(list, &form, &into, &type, NULL));
	if (into == Unbound)
		return loop_main_sum_nointo_(ptr, form);
	else
		return loop_main_sum_into_(ptr, form, into);
}


/*
 *  maximize, minimize
 */
static int loop_main_maxmin_form_(Execute ptr, addr form, addr into, addr maxmin)
{
	/* `(let ((g ,form))
	 *    (if value-loop
	 *      (setq value-loop (maxmin value-loop ,g))
	 *      (setq value-loop ,g)))
	 */
	addr let, if_symbol, setq;
	addr g, x, y, z;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_SETQ, &setq);

	make_symbolchar(&g, "G");
	list_heap(&form, g, form, NULL);
	list_heap(&form, form, NULL);
	list_heap(&maxmin, maxmin, into, g, NULL);
	list_heap(&y, setq, into, maxmin, NULL);
	list_heap(&z, setq, into, g, NULL);
	list_heap(&if_symbol, if_symbol, into, y, z, NULL);
	list_heap(&x, let, form, if_symbol, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_maxmin_nointo_(Execute ptr, addr form, addr maxmin)
{
	addr setq, value_loop, function_loop, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);

	/* `(setq value-loop nil function-loop nil) */
	list_heap(&x, setq, value_loop, Nil, function_loop, Nil, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(let ...) */
	return loop_main_maxmin_form_(ptr, form, value_loop, maxmin);
}

static int loop_main_maxmin_into_(Execute ptr, addr form, addr into, addr maxmin)
{
	addr setq, function_loop, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);

	Return(push_vars_loop_(ptr, into));
	/* `(setq ,into nil) */
	list_heap(&x, setq, into, Nil, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(let ...) */
	return loop_main_maxmin_form_(ptr, form, into, maxmin);
}

static int loop_main_maxmin_(Execute ptr, addr list, constindex index)
{
	addr form, into, type, maxmin;

	Return(list_bind_(list, &form, &into, &type, NULL));
	GetConstant(index, &maxmin);
	if (into == Unbound)
		return loop_main_maxmin_nointo_(ptr, form, maxmin);
	else
		return loop_main_maxmin_into_(ptr, form, into, maxmin);
}

static int loop_main_maximize_(Execute ptr, addr list)
{
	return loop_main_maxmin_(ptr, list, CONSTANT_COMMON_MAX);
}

static int loop_main_minimize_(Execute ptr, addr list)
{
	return loop_main_maxmin_(ptr, list, CONSTANT_COMMON_MIN);
}


/*
 *  while, until
 */
static int loop_main_while_until_(Execute ptr, addr list, constindex index)
{
	addr expr, x, go, end_loop;

	Return(list_bind_(list, &expr, NULL));
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);
	GetConstant(index, &x);

	/* `(unless ,expr (go end-loop)) */
	list_heap(&go, go, end_loop, NULL);
	list_heap(&x, x, expr, go, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_while_(Execute ptr, addr list)
{
	return loop_main_while_until_(ptr, list, CONSTANT_COMMON_UNLESS);
}

static int loop_main_until_(Execute ptr, addr list)
{
	return loop_main_while_until_(ptr, list, CONSTANT_COMMON_WHEN);
}


/*
 *  always, never
 */
static int loop_main_terminate_(Execute ptr, addr list, constindex index)
{
	addr expr, setq, value_loop, check, return_from, named, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);
	GetConst(COMMON_RETURN_FROM, &return_from);
	GetConstant(index, &check);

	Return(list_bind_(list, &expr, NULL));
	/* `(setq value-loop t) */
	list_heap(&x, setq, value_loop, T, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(unless ,expr (return-from named nil)) */
	Return(getnamed_loop_(ptr, &named));
	list_heap(&x, return_from, named, Nil, NULL);
	list_heap(&x, check, expr, x, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_always_(Execute ptr, addr list)
{
	return loop_main_terminate_(ptr, list, CONSTANT_COMMON_UNLESS);
}

static int loop_main_never_(Execute ptr, addr list)
{
	return loop_main_terminate_(ptr, list, CONSTANT_COMMON_WHEN);
}


/*
 *  thereis
 */
static int loop_main_thereis_(Execute ptr, addr list)
{
	addr expr, setq, value_loop, let, if_symbol, return_from, named, x, g;

	Return(list_bind_(list, &expr, NULL));
	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);

	/* `(setq value-loop nil) */
	list_heap(&x, setq, value_loop, Nil, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(let ((,g ,expr))
	 *    (if ,g (return-from named ,g)))
	 */
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_RETURN_FROM, &return_from);
	make_symbolchar(&g, "G");
	Return(getnamed_loop_(ptr, &named));
	list_heap(&x, return_from, named, g, NULL);
	list_heap(&if_symbol, if_symbol, g, x, NULL);
	list_heap(&g, g, expr, NULL);
	list_heap(&g, g, NULL);
	list_heap(&x, let, g, if_symbol, NULL);
	return push_form_loop_(ptr, x);
}


/*
 *  repeat
 */
static int loop_main_repeat_form_(Execute ptr, addr expr, addr a, addr b)
{
	addr setq, if_symbol, less, incf, go, end_loop, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_NUMBER_LESS, &less);
	GetConst(COMMON_INCF, &incf);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	/* `(setq ,a 0 ,b ,expr) */
	fixnum_heap(&x, 0);
	list_heap(&x, setq, a, x, b, expr, NULL);
	Return(push_init_loop_(ptr, x));

	/* `(if (< ,a ,b)
	 *    (incf ,a)
	 *    (go end-loop))
	 */
	list_heap(&go, go, end_loop, NULL);
	list_heap(&incf, incf, a, NULL);
	list_heap(&less, less, a, b, NULL);
	list_heap(&x, if_symbol, less, incf, go, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_repeat_(Execute ptr, addr list)
{
	addr expr, a, b;

	Return(list_bind_(list, &expr, &a, &b, NULL));
	Return(push_vars_loop_(ptr, a));
	Return(push_vars_loop_(ptr, b));
	return loop_main_repeat_form_(ptr, expr, a, b);
}


/*
 *  loop_main_
 */
static int loop_main_type_(Execute ptr, addr list)
{
	addr pos;

	GetCons(list, &pos, &list);

	/* initially, finally */
	if (loop_symbol_initially_p(pos))
		return loop_main_initially_(ptr, list);
	if (loop_symbol_finally_p(pos))
		return loop_main_finally_(ptr, list);

	/* unconditional */
	if (loop_symbol_do_p(pos))
		return loop_main_do_(ptr, list);
	if (loop_symbol_return_p(pos))
		return loop_main_return_(ptr, list);

	/* conditional */
	if (loop_symbol_if_p(pos))
		return loop_main_if_(ptr, list);
	if (loop_symbol_unless_p(pos))
		return loop_main_unless_(ptr, list);

	/* list-accumulation */
	if (loop_symbol_collect_p(pos))
		return loop_main_collect_(ptr, list);
	if (loop_symbol_append_p(pos))
		return loop_main_append_(ptr, list);
	if (loop_symbol_nconc_p(pos))
		return loop_main_nconc_(ptr, list);

	/* numeric-accumulation */
	if (loop_symbol_count_p(pos))
		return loop_main_count_(ptr, list);
	if (loop_symbol_sum_p(pos))
		return loop_main_sum_(ptr, list);
	if (loop_symbol_maximize_p(pos))
		return loop_main_maximize_(ptr, list);
	if (loop_symbol_minimize_p(pos))
		return loop_main_minimize_(ptr, list);

	/* termination */
	if (loop_symbol_while_p(pos))
		return loop_main_while_(ptr, list);
	if (loop_symbol_until_p(pos))
		return loop_main_until_(ptr, list);
	if (loop_symbol_always_p(pos))
		return loop_main_always_(ptr, list);
	if (loop_symbol_never_p(pos))
		return loop_main_never_(ptr, list);
	if (loop_symbol_thereis_p(pos))
		return loop_main_thereis_(ptr, list);
	if (loop_symbol_repeat_p(pos))
		return loop_main_repeat_(ptr, list);

	/* error */
	return fmte_("Invalid loop operator ~S.", pos, NULL);
}

int loop_main_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(loop_main_type_(ptr, pos));
	}

	return 0;
}


/************************************************************
 *  loop_parse.c
 ************************************************************/

/*
 *  clause
 */
static int loop_parse_named_clause_(Execute ptr, addr *ret, addr *list)
{
	int check;
	addr pos, args;

	/* check */
	*ret = Nil;
	if (! consp_getcons(*list, &pos, &args))
		return 0;
	Return(loop_symbol_named_p_(pos, &check));
	if (! check)
		return 0;

	/* parse */
	if (! consp_getcons(args, &pos, &args))
		return fmte_("NAMED clause must be a name argument in loop.", NULL);
	if (! symbolp(pos))
		return fmte_("NAMED argument ~S must be a symbol type.", pos, NULL);

	/* result */
	setnamed_loop(ptr, pos);
	*ret = pos;
	*list = args;
	return 0;
}

static int loop_parse_with_variable_(addr pos)
{
	addr a, b;

	if (symbolp(pos))
		return 0;
	if (! consp_getcons(pos, &a, &b))
		return fmte_("The value ~S must be a symbol type.", pos, NULL);
	Return(loop_parse_with_variable_(a));
	Return(loop_parse_with_variable_(b));

	return 0;
}

static int loop_parse_with_clause1_(addr *list, addr *ret)
{
	int check;
	addr var, type, form, args, pos;

	args = *list;
	type = form = Unbound;
	/* var */
	GetCons(args, &var, &args);
	Return(loop_parse_with_variable_(var));
	if (args == Nil)
		goto loop_result;
	/* next */
	if (! consp_getcar(args, &pos))
		goto error;
	Return(loop_symbol_equal_p_(pos, &check));
	if (check)
		goto loop_form;
	Return(loop_symbol_form_p_(pos, &check));
	if (check)
		goto loop_result;
	Return(loop_symbol_of_type_p_(pos, &check));
	if (check) {
		GetCdr(args, &args);
		if (! consp_getcar(args, &pos))
			goto error;
	}
	goto loop_type;

	/* [type-spec] */
loop_type:
	type = pos;
	GetCdr(args, &args);
	if (args == Nil)
		goto loop_result;
	if (! consp_getcar(args, &pos))
		goto error;
	Return(loop_symbol_equal_p_(pos, &check));
	if (check)
		goto loop_form;
	Return(loop_symbol_form_p_(pos, &check));
	if (check)
		goto loop_result;
	else
		goto error;

	/* [= form] */
loop_form:
	GetCdr(args, &args);
	if (! consp_getcons(args, &form, &args))
		goto error;
	if (args == Nil)
		goto loop_result;
	if (! consp_getcar(args, &pos))
		goto error;
	Return(loop_symbol_form_p_(pos, &check));
	if (check)
		goto loop_result;
	else
		goto error;

loop_result:
	list_heap(ret, var, type, form, NULL);
	return Result(list, args);

error:
	*list = *ret = Nil;
	return fmte_("Invalid WITH form ~S in loop.", *list, NULL);
}

static int loop_parse_with_clause_(addr *root, addr *list)
{
	int check;
	addr args, vars, pos;

	/* loop */
	Return_getcdr(*list, &args);
	vars = Nil;
	if (! consp(args))
		goto error;
	for (;;) {
		/* push */
		Return(loop_parse_with_clause1_(&args, &pos));
		cons_heap(&vars, pos, vars);
		/* and */
		if (args == Nil)
			break;
		if (! consp_getcar(args, &pos))
			goto error;
		Return(loop_symbol_and_p_(pos, &check));
		if (! check)
			break;
		GetCdr(args, &args);
	}

	/* result */
	if (vars == Nil)
		goto error;
	GetConst(SYSTEM_LOOP_WITH, &pos);
	nreverse(&vars, vars);
	cons_heap(&pos, pos, vars);
	cons_heap(root, pos, *root);
	return Result(list, args);

error:
	return fmte_("Invalid WITH form ~S in loop.", *list, NULL);
}

static int loop_parse_form_variables_(addr *list, addr *ret)
{
	int check;
	addr root, pos, next;

	for (root = Nil; *list != Nil; ) {
		if (! consp_getcons(*list, &pos, &next))
			return fmte_("Invalid loop form ~S.", *list, NULL);
		Return(loop_symbol_form_p_(pos, &check));
		if (check)
			break;
		cons_heap(&root, pos, root);
		*list = next;
	}
	nreverse(ret, root);

	return 0;
}

static int loop_parse_initial_final_clause_(addr *root, addr *list)
{
	int check;
	addr key, pos;

	/* symbol */
	Return_getcons(*list, &pos, list);
	Return(loop_symbol_initially_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_INITIALLY, &key);
		goto variables;
	}
	Return(loop_symbol_finally_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_FINALLY, &key);
		goto variables;
	}
	return fmte_("Invalid value ~S.", pos, NULL);

variables:
	/* compound-form-variables */
	Return(loop_parse_form_variables_(list, &pos));
	cons_heap(&pos, key, pos);
	cons_heap(root, pos, *root);

	return 0;
}

struct for_as_arithmetic {
	addr from1, from2, to1, to2, by;
};

static void clear_for_as_arithmetic(struct for_as_arithmetic *str)
{
	str->from1 = str->from2 = str->to1 = str->to2 = str->by = Unbound;
}

static int loop_parse_for_as_arithmetic_struct_(
		struct for_as_arithmetic *str, addr *list, addr pos, int *ret)
{
	int check;
	*ret = 0;
loop:
	Return(loop_symbol_arithmetic1_p_(pos, &check));
	if (check) {
		if (str->from1 != Unbound)
			return fmte_("FOR-AS FROM expr already exists.", NULL);
		str->from1 = pos;
		if (! consp_getcons(*list, &(str->from2), list))
			return Result(ret, 1);
		goto next;
	}
	Return(loop_symbol_arithmetic2_p_(pos, &check));
	if (check) {
		if (str->to1 != Unbound)
			return fmte_("FOR-AS TO expr already exists.", NULL);
		str->to1 = pos;
		if (! consp_getcons(*list, &(str->to2), list))
			return Result(ret, 1);
		goto next;
	}
	Return(loop_symbol_by_p_(pos, &check));
	if (check) {
		if (str->by != Unbound)
			return fmte_("FOR-AS BY expr already exists.", NULL);
		if (! consp_getcons(*list, &(str->by), list))
			return Result(ret, 1);
		goto next;
	}
	return Result(ret, 1);

next:
	if (*list == Nil)
		return Result(ret, 0);
	if (! consp_getcar(*list, &pos))
		return Result(ret, 1);
	Return(loop_symbol_arithmetic_p_(pos, &check));
	if (! check)
		return Result(ret, 0);
	GetCdr(*list, list);
	goto loop;
}

static int loop_parse_for_as_arithmetic1_p_(struct for_as_arithmetic *str, int *ret)
{
	int check, check1, check2, check3, a, b, c;
	addr x, y, z;

	x = str->from1;
	y = str->to1;
	z = str->by;
	check1 = (x == Unbound);
	check2 = (y == Unbound);
	check3 = (z == Unbound);
	if (check1 && check2 && check3)
		return Result(ret, 0);
	/* from */
	if (! check1) {
		Return(loop_symbol_from_p_(x, &check));
		a = ! check;
		Return(loop_symbol_upfrom_p_(x, &check));
		b = ! check;
		if (a && b)
			return Result(ret, 0);
	}
	/* to */
	if (! check2) {
		Return(loop_symbol_to_p_(y, &check));
		a = ! check;
		Return(loop_symbol_upto_p_(y, &check));
		b = ! check;
		Return(loop_symbol_below_p_(y, &check));
		c = ! check;
		if (a && b && c)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int loop_parse_for_as_arithmetic2_p_(struct for_as_arithmetic *str, int *ret)
{
	int check, check1, check2, a, b;
	addr x, y;

	x = str->from1;
	y = str->to1;
	check1 = (x == Unbound);
	check2 = (y == Unbound);
	/* from */
	if (check1)
		return Result(ret, 0);
	Return(loop_symbol_from_p_(x, &check));
	if (! check)
		return Result(ret, 0);
	/* to */
	if (check2)
		return Result(ret, 0);
	Return(loop_symbol_downto_p_(y, &check));
	a = ! check;
	Return(loop_symbol_above_p_(y, &check));
	b = ! check;
	if (a && b)
		return Result(ret, 0);

	return Result(ret, 1);
}

static int loop_parse_for_as_arithmetic3_p_(struct for_as_arithmetic *str, int *ret)
{
	int check, check1, check2, a, b, c;
	addr x, y;

	x = str->from1;
	y = str->to1;
	check1 = (x == Unbound);
	check2 = (y == Unbound);
	/* from */
	if (check1)
		return Result(ret, 0);
	Return(loop_symbol_downfrom_p_(x, &check));
	if (! check)
		return Result(ret, 0);
	/* to */
	if (! check2) {
		Return(loop_symbol_to_p_(y, &check));
		a = ! check;
		Return(loop_symbol_downto_p_(y, &check));
		b = ! check;
		Return(loop_symbol_above_p_(y, &check));
		c = ! check;
		if (a && b && c)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int loop_parse_for_as_arithmetic_(Execute ptr,
		addr *list, addr *value, int *ret)
{
	int check;
	addr var, args, pos, g1, g2;
	struct for_as_arithmetic str;

	clear_for_as_arithmetic(&str);
	if (! consp_getcons(*list, &var, &args))
		return Result(ret, 0);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_arithmetic_p_(pos, &check));
	if (check)
		goto arithmetic;
	Return(loop_symbol_of_type_p_(pos, &check));
	if (check) {
		if (! consp_getcons(args, &pos, &args))
			return Result(ret, 0);
	}
	/* ignore type-spec */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_arithmetic_p_(pos, &check));
	if (! check)
		return Result(ret, 0);

arithmetic:
	Return(loop_parse_for_as_arithmetic_struct_(&str, &args, pos, &check));
	if (check)
		return Result(ret, 0);
	Return(loop_parse_for_as_arithmetic1_p_(&str, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_UP, &pos);
		goto next;
	}
	Return(loop_parse_for_as_arithmetic2_p_(&str, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNTO, &pos);
		goto next;
	}
	Return(loop_parse_for_as_arithmetic3_p_(&str, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNFROM, &pos);
		goto next;
	}
	return Result(ret, 0);
next:
	/* gensym */
	Return(make_gensym_(ptr, &g1));
	Return(make_gensym_(ptr, &g2));
	/* result */
	list_heap(value, pos, var,
			str.from1, str.from2, str.to1, str.to2, str.by, g1, g2, NULL);
	*list = args;
	return Result(ret, 1);
}

static int loop_parse_for_as_call_list_(Execute ptr,
		addr *list, addr *value, int *ret,
		int (*check1_)(addr, int *),
		int (*check2_)(addr, int *),
		constindex index)
{
	/* var [type-spec] in form [by step] */
	int check;
	addr args, var, type, form, step, pos, g;

	type = step = Unbound;
	if (! consp_getcons(*list, &var, &args))
		return Result(ret, 0);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return((*check1_)(pos, &check));
	if (check)
		goto next;

	/* of-type */
	Return(loop_symbol_of_type_p_(pos, &check));
	if (check) {
		if (! consp_getcons(args, &pos, &args))
			return Result(ret, 0);
	}

	/* type-spec */
	type = pos;
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return((*check1_)(pos, &check));
	if (! check)
		return Result(ret, 0);

next:
	if (! consp_getcons(args, &form, &args))
		return Result(ret, 0);
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		return Result(ret, 0);
	Return((*check2_)(pos, &check));
	if (! check)
		goto result;
	GetCdr(args, &args);
	if (! consp_getcons(args, &step, &args))
		return Result(ret, 0);
	goto result;

result:
	Return(make_gensym_(ptr, &g));
	GetConstant(index, &pos);
	list_heap(value, pos, var, type, form, step, g, NULL);
	*list = args;
	return Result(ret, 1);
}

static int loop_parse_for_as_in_list_(Execute ptr,
		addr *list, addr *value, int *ret)
{
	return loop_parse_for_as_call_list_(ptr, list, value, ret,
			loop_symbol_in_p_, loop_symbol_by_p_,
			CONSTANT_SYSTEM_LOOP_FOR_AS_IN_LIST);
}

static int loop_parse_for_as_on_list_(Execute ptr,
		addr *list, addr *value, int *ret)
{
	return loop_parse_for_as_call_list_(ptr, list, value, ret,
			loop_symbol_on_p_, loop_symbol_by_p_,
			CONSTANT_SYSTEM_LOOP_FOR_AS_ON_LIST);
}

static int loop_parse_for_as_equals_then_(Execute ptr,
		addr *list, addr *value, int *ret)
{
	return loop_parse_for_as_call_list_(ptr, list, value, ret,
			loop_symbol_equal_p_, loop_symbol_then_p_,
			CONSTANT_SYSTEM_LOOP_FOR_AS_EQUALS_THEN);
}

static int loop_parse_for_as_across_(Execute ptr, addr *list, addr *value, int *ret)
{
	/* var [type-spec] across vector */
	int check;
	addr args, var, type, vector, pos, g1, g2, g3;

	type = Unbound;
	if (! consp_getcons(*list, &var, &args))
		return Result(ret, 0);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_across_p_(pos, &check));
	if (check)
		goto next;

	/* of-type */
	Return(loop_symbol_of_type_p_(pos, &check));
	if (check) {
		if (! consp_getcons(args, &pos, &args))
			return Result(ret, 0);
	}

	/* type-spec */
	type = pos;
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_across_p_(pos, &check));
	if (! check)
		return Result(ret, 0);

next:
	if (! consp_getcons(args, &vector, &args))
		return Result(ret, 0);
	/* result */
	Return(make_gensym_(ptr, &g1));
	Return(make_gensym_(ptr, &g2));
	Return(make_gensym_(ptr, &g3));
	GetConst(SYSTEM_LOOP_FOR_AS_ACROSS, &pos);
	list_heap(value, pos, var, type, vector, g1, g2, g3, NULL);
	*list = args;
	return Result(ret, 1);
}

static int loop_parse_for_as_hash_(Execute ptr, addr *list, addr *value, int *ret)
{
	/* var [type-spec] being {each|the}
	 *     { hash-key | hash-keys | hash-value | hash-values }
	 *     {in|of} table [using ({hash-key|hash-value} var2)]
	 */
	int check;
	addr var, type, keyp, table, use, args, pos, g;

	type = use = Unbound;
	/* var */
	if (! consp_getcons(*list, &var, &args))
		return Result(ret, 0);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	/* begin */
	Return(loop_symbol_being_p_(pos, &check));
	if (check)
		goto next;
	/* of-type */
	Return(loop_symbol_of_type_p_(pos, &check));
	if (check) {
		if (! consp_getcons(args, &pos, &args))
			return Result(ret, 0);
	}
	/* type-spec, being */
	type = pos;
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_being_p_(pos, &check));
	if (! check)
		return Result(ret, 0);
next:
	/* each, the */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_each_the_p_(pos, &check));
	if (! check)
		return Result(ret, 0);
	/* hash-key, hash-keys, hash-value, hash-values */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_hash_key2_p_(pos, &check));
	if (check) {
		keyp = T;
		goto next1;
	}
	Return(loop_symbol_hash_value2_p_(pos, &check));
	if (check) {
		keyp = Nil;
		goto next1;
	}
	return Result(ret, 0);
next1:
	/* in, of */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_in_of_p_(pos, &check));
	if (! check)
		return Result(ret, 0);
	/* table */
	if (! consp_getcons(args, &table, &args))
		return Result(ret, 0);
	/* using */
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		return Result(ret, 0);
	Return(loop_symbol_using_p_(pos, &check));
	if (! check)
		goto result;
	/* hash-key, hash-value */
	GetCdr(args, &args);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (! consp_getcons(pos, &pos, &use))
		return Result(ret, 0);
	Return(loop_symbol_hash_key_p_(pos, &check));
	if (check) {
		if (keyp == T)
			return Result(ret, 0);
		goto next2;
	}
	Return(loop_symbol_hash_value_p_(pos, &check));
	if (check) {
		if (keyp == Nil)
			return Result(ret, 0);
		goto next2;
	}
	return Result(ret, 0);
next2:
	/* var2 */
	if (! consp_getcons(use, &use, &pos))
		return Result(ret, 0);
	if (pos != Nil)
		return Result(ret, 0);
	goto result;

result:
	Return(make_gensym_(ptr, &g));
	GetConst(SYSTEM_LOOP_FOR_AS_HASH, &pos);
	list_heap(value, pos, var, type, keyp, table, use, g, NULL);
	*list = args;
	return Result(ret, 1);
}

static int loop_parse_for_as_package_(Execute ptr, addr *list, addr *value, int *ret)
{
	/* var [type-spec] being {each|the}
	 *     { symbol | symbols |
	 *       present-symbol | present-symbols |
	 *       external-symbol | external-symbols }
	 *     [{in|of} package]
	 */
	int check, symbolp, presentp, externalp;
	addr var, type, package, args, pos, g;

	symbolp = presentp = externalp = 0;
	type = package = Unbound;
	/* var */
	if (! consp_getcons(*list, &var, &args))
		return Result(ret, 0);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);

	/* of-type */
	Return(loop_symbol_of_type_p_(pos, &check));
	if (check) {
		if (! consp_getcons(args, &pos, &args))
			return Result(ret, 0);
	}

	/* type-spec, being */
	Return(loop_symbol_being_p_(pos, &check));
	if (check)
		goto type_next;
	type = pos;
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_being_p_(pos, &check));
	if (! check)
		return Result(ret, 0);

type_next:
	/* each, the */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_each_the_p_(pos, &check));
	if (! check)
		return Result(ret, 0);
	/* { symbol | symbols |
	 *   present-symbol | present-symbols |
	 *   external-symbol | external-symbols }
	 */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_symbol2_p_(pos, &check));
	if (check) {
		symbolp = 1;
		goto next;
	}
	Return(loop_symbol_present_symbol2_p_(pos, &check));
	if (check) {
		presentp = 1;
		goto next;
	}
	Return(loop_symbol_external_symbol2_p_(pos, &check));
	if (check) {
		externalp = 1;
		goto next;
	}
	return Result(ret, 0);
next:
	/* in, of */
	if (args == Nil)
		goto result;
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_in_of_p_(pos, &check));
	if (! check)
		return Result(ret, 0);
	/* package */
	if (! consp_getcons(args, &package, &args))
		return Result(ret, 0);
	goto result;

result:
	if (symbolp)
		GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_SYMBOL, &pos);
	else if (presentp)
		GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_PRESENT, &pos);
	else if (externalp)
		GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_EXTERNAL, &pos);
	else
		return Result(ret, 0);
	/* result */
	Return(make_gensym_(ptr, &g));
	list_heap(value, pos, var, type, package, g, NULL);
	*list = args;
	return Result(ret, 1);
}

static int loop_parse_for_as_clause1_(Execute ptr, addr *list, addr *value, int *ret)
{
	int check;

	Return(loop_parse_for_as_arithmetic_(ptr, list, value, &check));
	if (check)
		return Result(ret, 1);

	Return(loop_parse_for_as_in_list_(ptr, list, value, &check));
	if (check)
		return Result(ret, 1);

	Return(loop_parse_for_as_on_list_(ptr, list, value, &check));
	if (check)
		return Result(ret, 1);

	Return(loop_parse_for_as_equals_then_(ptr, list, value, &check));
	if (check)
		return Result(ret, 1);

	Return(loop_parse_for_as_across_(ptr, list, value, &check));
	if (check)
		return Result(ret, 1);

	Return(loop_parse_for_as_hash_(ptr, list, value, &check));
	if (check)
		return Result(ret, 1);

	Return(loop_parse_for_as_package_(ptr, list, value, &check));
	if (check)
		return Result(ret, 1);

	return 0;
}

static int loop_parse_for_as_clause_(Execute ptr, addr *root, addr *list)
{
	int check;
	addr args, vars, pos;

	/* loop */
	Return_getcdr(*list, &args);
	vars = Nil;
	if (! consp(args))
		goto error;
	for (;;) {
		/* push */
		Return(loop_parse_for_as_clause1_(ptr, &args, &pos, &check));
		if (! check)
			goto error;
		cons_heap(&vars, pos, vars);
		/* and */
		if (args == Nil)
			break;
		if (! consp_getcar(args, &pos))
			goto error;
		Return(loop_symbol_and_p_(pos, &check));
		if (! check)
			break;
		GetCdr(args, &args);
	}

	/* result */
	if (vars == Nil)
		goto error;
	GetConst(SYSTEM_LOOP_FOR_AS, &pos);
	cons_heap(&pos, pos, vars);
	cons_heap(root, pos, *root);
	return Result(list, args);

error:
	return fmte_("Invalid FOR-AS form ~S in loop.", *list, NULL);
}

static int loop_parse_variable_clause_update_(Execute ptr, addr *root, addr *list)
{
	int check;
	addr pos;

	if (! consp_getcar(*list, &pos))
		return 0;

	/* with */
	Return(loop_symbol_with_p_(pos, &check));
	if (check)
		return loop_parse_with_clause_(root, list);

	/* initial */
	Return(loop_symbol_initial_final_p_(pos, &check));
	if (check)
		return loop_parse_initial_final_clause_(root, list);

	/* for_as */
	Return(loop_symbol_for_as_p_(pos, &check));
	if (check)
		return loop_parse_for_as_clause_(ptr, root, list);

	return 0;
}

static int loop_parse_variable_clause_(Execute ptr, addr *root, addr *list)
{
	addr check;

	for (;;) {
		check = *root;
		Return(loop_parse_variable_clause_update_(ptr, root, list));
		if (check == *root)
			break;
	}

	return 0;
}

static int loop_parse_uncondition_result_(addr *ret, addr *list)
{
	int check;
	addr key, pos;

	Return_getcons(*list, &pos, list);
	Return(loop_symbol_do_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_DO, &key);
		Return(loop_parse_form_variables_(list, &pos));
		cons_heap(ret, key, pos);
		return 0;
	}

	Return(loop_symbol_return_p_(pos, &check));
	if (check) {
		if (! consp_getcons(*list, &pos, list))
			return fmte_("Invalid RETURN form ~S in loop.", *list, NULL);
		Return(loop_symbol_it_p_(pos, &check));
		if (check)
			GetConst(SYSTEM_IT_LOOP, &pos);
		GetConst(SYSTEM_LOOP_RETURN, &key);
		list_heap(ret, key, pos, NULL);
		return 0;
	}

	return fmte_("Invalid value ~S.", pos, NULL);
}

static int loop_parse_uncondition_clause_(addr *root, addr *list)
{
	addr pos;

	Return(loop_parse_uncondition_result_(&pos, list));
	cons_heap(root, pos, *root);

	return 0;
}

static int loop_condition_selectable_result_(addr *ret, addr *list);
static int loop_parse_condition_result_(addr *ret, addr *list)
{
	int check;
	addr args, pos, key, form, expr1, expr2;

	Return_getcons(*list, &pos, &args);
	Return(loop_symbol_if_when_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_IF, &key);
		goto next;
	}
	Return(loop_symbol_unless_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_UNLESS, &key);
		goto next;
	}
	goto error;
next:
	/* form */
	expr2 = Unbound;
	if (! consp_getcons(args, &form, &args))
		goto error;
	/* then */
	if (! consp(args))
		goto error;
	Return(loop_condition_selectable_result_(&expr1, &args));
	/* else */
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		goto error;
	Return(loop_symbol_else_p_(pos, &check));
	if (! check)
		goto result;
	GetCdr(args, &args);
	Return(loop_condition_selectable_result_(&expr2, &args));
	/* end */
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		goto error;
	Return(loop_symbol_end_p_(pos, &check));
	if (! check)
		goto result;
	GetCdr(args, &args);
	goto result;

result:
	list_heap(ret, key, form, expr1, expr2, NULL);
	return Result(list, args);

error:
	return fmte_("Invalid loop form ~S.", *list, NULL);
}

static int loop_parse_condition_clause_(addr *root, addr *list)
{
	addr pos;

	Return(loop_parse_condition_result_(&pos, list));
	cons_heap(root, pos, *root);

	return 0;
}

static int loop_parse_list_accumulation_result_(addr *ret, addr *list)
{
	int check;
	addr pos, key, args, form, into;

	/* first */
	Return_getcons(*list, &pos, &args);
	into = Unbound;
	Return(loop_symbol_collect_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_COLLECT, &key);
		goto next;
	}
	Return(loop_symbol_append_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_APPEND, &key);
		goto next;
	}
	Return(loop_symbol_nconc_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_NCONC, &key);
		goto next;
	}
	goto error;
next:
	/* form, it */
	if (! consp_getcons(args, &form, &args))
		goto error;
	Return(loop_symbol_it_p_(form, &check));
	if (check)
		GetConst(SYSTEM_IT_LOOP, &form);
	if (! consp_getcar(args, &pos))
		goto result;
	Return(loop_symbol_into_p_(pos, &check));
	if (! check)
		goto result;
	GetCdr(args, &args);
	if (! consp_getcons(args, &into, &args))
		goto error;
	else
		goto result;

result:
	list_heap(ret, key, form, into, NULL);
	return Result(list, args);

error:
	return fmte_("Invalid loop form ~S.", *list, NULL);
}

static int loop_parse_numeric_accumulation_result_(addr *ret, addr *list)
{
	int check;
	addr pos, key, args, form, into, type;

	/* first */
	Return_getcons(*list, &pos, &args);
	into = type = Unbound;
	Return(loop_symbol_count_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_COUNT, &key);
		goto next;
	}
	Return(loop_symbol_sum_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_SUM, &key);
		goto next;
	}
	Return(loop_symbol_maximize_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_MAXIMIZE, &key);
		goto next;
	}
	Return(loop_symbol_minimize_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_MINIMIZE, &key);
		goto next;
	}
	goto error;

next:
	/* form, it */
	if (! consp_getcons(args, &form, &args))
		goto error;
	Return(loop_symbol_it_p_(form, &check));
	if (check)
		GetConst(SYSTEM_IT_LOOP, &form);

	/* into */
	if (! consp_getcar(args, &pos))
		goto result;
	Return(loop_symbol_into_p_(pos, &check));
	if (! check)
		goto of_type;
	GetCdr(args, &args);
	if (! consp_getcons(args, &into, &args))
		goto error;
	goto of_type;

	/* of-type */
of_type:
	if (! consp_getcar(args, &pos))
		goto result;
	Return(loop_symbol_of_type_p_(pos, &check));
	if (! check)
		goto type_spec;
	GetCdr(args, &args);
	if (! consp_getcons(args, &type, &args))
		goto error;
	goto result;

	/* type-spec */
type_spec:
	if (! consp_getcar(args, &pos))
		goto error;
	Return(loop_symbol_form_main_p_(pos, &check));
	if (check)
		goto result;
	GetCons(args, &type, &args);
	goto result;

result:
	list_heap(ret, key, form, into, type, NULL);
	return Result(list, args);

error:
	return fmte_("Invalid loop form ~S.", *list, NULL);
}

static int loop_parse_accumulation_result_(addr *ret, addr *list)
{
	int check;
	addr pos;

	GetCar(*list, &pos);
	Return(loop_symbol_list_accumulation_p_(pos, &check));
	if (check)
		return loop_parse_list_accumulation_result_(ret, list);

	Return(loop_symbol_numeric_accumulation_p_(pos, &check));
	if (check)
		return loop_parse_numeric_accumulation_result_(ret, list);

	*ret = Nil;
	return fmte_("Invalid accumulation symbol ~S.", pos, NULL);
}

static int loop_parse_accumulation_clause_(addr *root, addr *list)
{
	addr pos;

	Return(loop_parse_accumulation_result_(&pos, list));
	cons_heap(root, pos, *root);

	return 0;
}

static int loop_parse_selectable_p_(addr *list, addr *pos, int *ret)
{
	int check;

	if (! consp_getcar(*list, pos)) {
		*pos = Nil;
		return Result(ret, 0);
	}

	/* condition */
	Return(loop_symbol_condition_p_(*pos, &check));
	if (check) {
		*ret = 1;
		return loop_parse_condition_result_(pos, list);
	}

	/* uncondition */
	Return(loop_symbol_uncondition_p_(*pos, &check));
	if (check) {
		*ret = 1;
		return loop_parse_uncondition_result_(pos, list);
	}

	/* accumulation */
	Return(loop_symbol_accumulation_p_(*pos, &check));
	if (check) {
		*ret = 1;
		return loop_parse_accumulation_result_(pos, list);
	}

	/* error */
	*pos = Nil;
	return Result(ret, 0);
}

static int loop_condition_selectable_result_(addr *ret, addr *list)
{
	int check;
	addr root, pos;

	root = Nil;
	for (;;) {
		Return(loop_parse_selectable_p_(list, &pos, &check));
		if (! check)
			break;
		cons_heap(&root, pos, root);
		/* and */
		if (! consp_getcar(*list, &pos))
			break;
		Return(loop_symbol_and_p_(pos, &check));
		if (! check)
			break;
		GetCdr(*list, list);
	}
	nreverse(ret, root);

	return 0;
}

static int loop_parse_termination_clause_(addr *root, addr *list)
{
	int check;
	addr symbol, pos, args, key, a, b;

	Return_getcons(*list, &symbol, &args);
	Return(loop_symbol_while_p_(symbol, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_WHILE, &key);
		goto next;
	}
	Return(loop_symbol_until_p_(symbol, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_UNTIL, &key);
		goto next;
	}
	Return(loop_symbol_repeat_p_(symbol, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_REPEAT, &key);
		goto next;
	}
	Return(loop_symbol_always_p_(symbol, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_ALWAYS, &key);
		goto next;
	}
	Return(loop_symbol_never_p_(symbol, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_NEVER, &key);
		goto next;
	}
	Return(loop_symbol_thereis_p_(symbol, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_THEREIS, &key);
		goto next;
	}
	goto error;

next:
	if (! consp_getcons(args, &pos, &args))
		goto error;
	Return(loop_symbol_repeat_p_(symbol, &check));
	if (check) {
		make_symbolchar(&a, "A");
		make_symbolchar(&b, "B");
		list_heap(&pos, key, pos, a, b, NULL);
	}
	else {
		list_heap(&pos, key, pos, NULL);
	}
	cons_heap(root, pos, *root);
	return Result(list, args);

error:
	return fmte_("Invalid loop form ~S.", *list, NULL);
}

static int loop_parse_main_clause_update_(addr *root, addr *list)
{
	int check;
	addr pos;

	if (! consp_getcar(*list, &pos))
		return 0;

	/* uncondition */
	Return(loop_symbol_uncondition_p_(pos, &check));
	if (check)
		return loop_parse_uncondition_clause_(root, list);

	/* condition */
	Return(loop_symbol_condition_p_(pos, &check));
	if (check)
		return loop_parse_condition_clause_(root, list);

	/* accumulation */
	Return(loop_symbol_accumulation_p_(pos, &check));
	if (check)
		return loop_parse_accumulation_clause_(root, list);

	/* termination */
	Return(loop_symbol_termination_p_(pos, &check));
	if (check)
		return loop_parse_termination_clause_(root, list);

	/* initial */
	Return(loop_symbol_initial_final_p_(pos, &check));
	if (check)
		return loop_parse_initial_final_clause_(root, list);

	return 0;
}

static int loop_parse_main_clause_(addr *root, addr *list)
{
	addr check;

	for (;;) {
		check = *root;
		Return(loop_parse_main_clause_update_(root, list));
		if (check == *root)
			break;
	}

	return 0;
}


/*
 *  main
 */
int loop_parse_common_(Execute ptr, addr *named, addr *vars, addr *main, addr *list)
{
	Return(loop_parse_named_clause_(ptr, named, list));
	Return(loop_parse_variable_clause_(ptr, vars, list));
	Return(loop_parse_main_clause_(main, list));
	nreverse(vars, *vars);
	nreverse(main, *main);

	return 0;
}


/************************************************************
 *  loop_special.c
 ************************************************************/

void loop_push_special(Execute ptr)
{
	addr pos;

	/* *loop-named* */
	GetConst(SYSTEM_LOOP_NAMED, &pos);
	pushspecial_control(ptr, pos, Nil);
	/* *loop-vars* */
	GetConst(SYSTEM_LOOP_VARS, &pos);
	pushspecial_control(ptr, pos, Nil);
	/* *loop-init* */
	GetConst(SYSTEM_LOOP_INIT, &pos);
	pushspecial_control(ptr, pos, Nil);
	/* *loop-final* */
	GetConst(SYSTEM_LOOP_FINAL, &pos);
	pushspecial_control(ptr, pos, Nil);
	/* *loop-form* */
	GetConst(SYSTEM_LOOP_FORM, &pos);
	pushspecial_control(ptr, pos, Nil);
	/* *loop-let* */
	GetConst(SYSTEM_LOOP_LET, &pos);
	pushspecial_control(ptr, pos, Nil);
}


/*
 *  named
 */
void setnamed_loop(Execute ptr, addr value)
{
	addr pos;
	GetConst(SYSTEM_LOOP_NAMED, &pos);
	setspecial_local(ptr, pos, value);
}

int getnamed_loop_(Execute ptr, addr *ret)
{
	addr pos;
	GetConst(SYSTEM_LOOP_NAMED, &pos);
	return getspecialcheck_local_(ptr, pos, ret);
}


/*
 *  function
 */
static int getexpand_loop_(Execute ptr, constindex index, addr *ret)
{
	addr symbol, pos;

	GetConstant(index, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &pos));
	nreverse(ret, pos);
	setspecial_local(ptr, symbol, Unbound);

	return 0;
}

static int getpush_loop_(Execute ptr, constindex index, addr value)
{
	addr symbol, list;

	GetConstant(index, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	cons_heap(&list, value, list);
	setspecial_local(ptr, symbol, list);

	return 0;
}


/*
 *  vars
 */
int getvars_expand_loop_(Execute ptr, addr *ret)
{
	return getexpand_loop_(ptr, CONSTANT_SYSTEM_LOOP_VARS, ret);
}

int push_vars_loop_(Execute ptr, addr value)
{
	return getpush_loop_(ptr, CONSTANT_SYSTEM_LOOP_VARS, value);
}


/*
 *  form
 */
int getform_expand_loop_(Execute ptr, addr *ret)
{
	return getexpand_loop_(ptr, CONSTANT_SYSTEM_LOOP_FORM, ret);
}

int push_form_loop_(Execute ptr, addr value)
{
	return getpush_loop_(ptr, CONSTANT_SYSTEM_LOOP_FORM, value);
}


/*
 *  initially
 */
int getinit_expand_loop_(Execute ptr, addr *ret)
{
	return getexpand_loop_(ptr, CONSTANT_SYSTEM_LOOP_INIT, ret);
}

int push_init_loop_(Execute ptr, addr value)
{
	return getpush_loop_(ptr, CONSTANT_SYSTEM_LOOP_INIT, value);
}


/*
 *  finallly
 */
int getfinal_expand_loop_(Execute ptr, addr *ret)
{
	return getexpand_loop_(ptr, CONSTANT_SYSTEM_LOOP_FINAL, ret);
}

int push_final_loop_(Execute ptr, addr value)
{
	return getpush_loop_(ptr, CONSTANT_SYSTEM_LOOP_FINAL, value);
}


/*
 *  let
 */
int push_let_loop_(Execute ptr, addr value)
{
	return getpush_loop_(ptr, CONSTANT_SYSTEM_LOOP_LET, value);
}

int getlet_loop_(Execute ptr, addr *ret)
{
	addr pos;
	GetConst(SYSTEM_LOOP_LET, &pos);
	return getspecialcheck_local_(ptr, pos, ret);
}


/************************************************************
 *  loop_symbol.c
 ************************************************************/

static int symbol_equal_char_p_(addr pos, const char *str, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);
	GetNameSymbol(pos, &pos);
	return string_equal_char_(pos, str, ret);
}

int loop_symbol_named_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "NAMED", ret);
}

int loop_symbol_with_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "WITH", ret);
}

/* initial-final */
int loop_symbol_initially_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "INITIALLY", ret);
}

int loop_symbol_finally_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "FINALLY", ret);
}

int loop_symbol_initial_final_p_(addr pos, int *ret)
{
	Return(loop_symbol_initially_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_finally_p_(pos, ret);
}

int loop_symbol_of_type_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "OF-TYPE", ret);
}

/* for-as */
int loop_symbol_for_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "FOR", ret);
}

int loop_symbol_as_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "AS", ret);
}

int loop_symbol_for_as_p_(addr pos, int *ret)
{
	Return(loop_symbol_for_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_as_p_(pos, ret);
}

/* uncondition */
int loop_symbol_do_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "DO", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "DOING", ret);
}

int loop_symbol_return_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "RETURN", ret);
}

int loop_symbol_uncondition_p_(addr pos, int *ret)
{
	Return(loop_symbol_do_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_return_p_(pos, ret);
}

/* condition */
int loop_symbol_if_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "IF", ret);
}

int loop_symbol_when_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "WHEN", ret);
}

int loop_symbol_if_when_p_(addr pos, int *ret)
{
	Return(loop_symbol_if_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_when_p_(pos, ret);
}

int loop_symbol_unless_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "UNLESS", ret);
}

int loop_symbol_condition_p_(addr pos, int *ret)
{
	Return(loop_symbol_if_p_(pos, ret));
	if (*ret)
		return 0;
	Return(loop_symbol_when_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_unless_p_(pos, ret);
}

/* accumulation */
int loop_symbol_collect_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "COLLECT", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "COLLECTING", ret);
}

int loop_symbol_append_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "APPEND", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "APPENDING", ret);
}

int loop_symbol_nconc_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "NCONC", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "NCONCING", ret);
}

int loop_symbol_count_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "COUNT", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "COUNTING", ret);
}

int loop_symbol_sum_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "SUM", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "SUMMING", ret);
}

int loop_symbol_maximize_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "MAXIMIZE", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "MAXIMIZING", ret);
}

int loop_symbol_minimize_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "MINIMIZE", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "MINIMIZING", ret);
}

int loop_symbol_list_accumulation_p_(addr pos, int *ret)
{
	Return(loop_symbol_collect_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_append_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_nconc_p_(pos, ret);
}

int loop_symbol_numeric_accumulation_p_(addr pos, int *ret)
{
	Return(loop_symbol_count_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_sum_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_maximize_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_minimize_p_(pos, ret);
}

int loop_symbol_accumulation_p_(addr pos, int *ret)
{
	Return(loop_symbol_list_accumulation_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_numeric_accumulation_p_(pos, ret);
}

/* termination */
int loop_symbol_repeat_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "REPEAT", ret);
}

int loop_symbol_always_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "ALWAYS", ret);
}

int loop_symbol_never_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "NEVER", ret);
}

int loop_symbol_thereis_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "THEREIS", ret);
}

int loop_symbol_while_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "WHILE", ret);
}

int loop_symbol_until_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "UNTIL", ret);
}

int loop_symbol_termination_p_(addr pos, int *ret)
{
	Return(loop_symbol_repeat_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_always_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_never_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_thereis_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_while_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_until_p_(pos, ret);
}

/* parse */
int loop_symbol_equal_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "=", ret);
}

int loop_symbol_and_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "AND", ret);
}

int loop_symbol_in_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "IN", ret);
}

int loop_symbol_on_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "ON", ret);
}

int loop_symbol_by_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "BY", ret);
}

int loop_symbol_then_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "THEN", ret);
}

int loop_symbol_across_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "ACROSS", ret);
}

int loop_symbol_being_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "BEING", ret);
}

int loop_symbol_each_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "EACH", ret);
}

int loop_symbol_the_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "THE", ret);
}

int loop_symbol_each_the_p_(addr pos, int *ret)
{
	Return(loop_symbol_each_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_the_p_(pos, ret);
}

int loop_symbol_of_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "OF", ret);
}

int loop_symbol_in_of_p_(addr pos, int *ret)
{
	Return(loop_symbol_in_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_of_p_(pos, ret);
}

int loop_symbol_hash_key_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "HASH-KEY", ret);
}

int loop_symbol_hash_keys_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "HASH-KEYS", ret);
}

int loop_symbol_hash_key2_p_(addr pos, int *ret)
{
	Return(loop_symbol_hash_key_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_hash_keys_p_(pos, ret);
}

int loop_symbol_hash_value_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "HASH-VALUE", ret);
}

int loop_symbol_hash_values_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "HASH-VALUES", ret);
}

int loop_symbol_hash_value2_p_(addr pos, int *ret)
{
	Return(loop_symbol_hash_value_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_hash_values_p_(pos, ret);
}

int loop_symbol_using_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "USING", ret);
}

int loop_symbol_symbol_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "SYMBOL", ret);
}

int loop_symbol_symbols_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "SYMBOLS", ret);
}

int loop_symbol_symbol2_p_(addr pos, int *ret)
{
	Return(loop_symbol_symbol_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_symbols_p_(pos, ret);
}

int loop_symbol_present_symbol_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "PRESENT-SYMBOL", ret);
}

int loop_symbol_present_symbols_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "PRESENT-SYMBOLS", ret);
}

int loop_symbol_present_symbol2_p_(addr pos, int *ret)
{
	Return(loop_symbol_present_symbol_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_present_symbols_p_(pos, ret);
}

int loop_symbol_external_symbol_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "EXTERNAL-SYMBOL", ret);
}

int loop_symbol_external_symbols_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "EXTERNAL-SYMBOLS", ret);
}

int loop_symbol_external_symbol2_p_(addr pos, int *ret)
{
	Return(loop_symbol_external_symbol_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_external_symbols_p_(pos, ret);
}

int loop_symbol_from_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "FROM", ret);
}

int loop_symbol_upfrom_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "UPFROM", ret);
}

int loop_symbol_downfrom_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "DOWNFROM", ret);
}

int loop_symbol_to_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "TO", ret);
}

int loop_symbol_upto_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "UPTO", ret);
}

int loop_symbol_downto_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "DOWNTO", ret);
}

int loop_symbol_above_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "ABOVE", ret);
}

int loop_symbol_below_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "BELOW", ret);
}

int loop_symbol_arithmetic1_p_(addr pos, int *ret)
{
	Return(loop_symbol_from_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_upfrom_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_downfrom_p_(pos, ret);
}

int loop_symbol_arithmetic2_p_(addr pos, int *ret)
{
	Return(loop_symbol_to_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_upto_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_downto_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_above_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_below_p_(pos, ret);
}

int loop_symbol_arithmetic_p_(addr pos, int *ret)
{
	Return(loop_symbol_arithmetic1_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_arithmetic2_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_by_p_(pos, ret);
}

int loop_symbol_it_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "IT", ret);
}

int loop_symbol_else_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "ELSE", ret);
}

int loop_symbol_end_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "END", ret);
}

int loop_symbol_into_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "INTO", ret);
}

/* main form */
int loop_symbol_form_main_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);

	Return(loop_symbol_uncondition_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_condition_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_accumulation_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_termination_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_initial_final_p_(pos, ret);
}

/* variables form */
int loop_symbol_form_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);

	Return(loop_symbol_form_main_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_with_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_for_as_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_else_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_end_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_and_p_(pos, ret);
}


/*
 *  eq
 */
static int loop_symbol_type_p(addr pos, constindex index)
{
	addr check;
	GetConstant(index, &check);
	return pos == check;
}

int loop_symbol_initially_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_INITIALLY);
}

int loop_symbol_finally_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FINALLY);
}

int loop_symbol_with_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_WITH);
}

int loop_symbol_for_as_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS);
}

int loop_symbol_do_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_DO);
}

int loop_symbol_return_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_RETURN);
}

int loop_symbol_collect_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_COLLECT);
}

int loop_symbol_append_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_APPEND);
}

int loop_symbol_nconc_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_NCONC);
}

int loop_symbol_count_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_COUNT);
}

int loop_symbol_sum_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_SUM);
}

int loop_symbol_maximize_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_MAXIMIZE);
}

int loop_symbol_minimize_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_MINIMIZE);
}

int loop_symbol_if_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_IF);
}

int loop_symbol_unless_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_UNLESS);
}

int loop_symbol_while_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_WHILE);
}

int loop_symbol_until_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_UNTIL);
}

int loop_symbol_always_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_ALWAYS);
}

int loop_symbol_never_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_NEVER);
}

int loop_symbol_thereis_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_THEREIS);
}

int loop_symbol_repeat_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_REPEAT);
}

int loop_symbol_for_as_arithmetic_up_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_ARITHMETIC_UP);
}

int loop_symbol_for_as_arithmetic_downto_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNTO);
}

int loop_symbol_for_as_arithmetic_downfrom_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNFROM);
}

int loop_symbol_for_as_in_list_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_IN_LIST);
}

int loop_symbol_for_as_on_list_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_ON_LIST);
}

int loop_symbol_for_as_equals_then_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_EQUALS_THEN);
}

int loop_symbol_for_as_across_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_ACROSS);
}

int loop_symbol_for_as_hash_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_HASH);
}

int loop_symbol_for_as_package_symbol_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_PACKAGE_SYMBOL);
}

int loop_symbol_for_as_package_present_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_PACKAGE_PRESENT);
}

int loop_symbol_for_as_package_external_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_PACKAGE_EXTERNAL);
}

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#ifdef __clang__
#pragma clang diagnostic pop
#endif
