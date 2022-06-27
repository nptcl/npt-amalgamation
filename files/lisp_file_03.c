/*
 *  npt -- ANSI Common Lisp Programming Language.
 *    https://github.com/nptcl/npt
 *    https://github.com/nptcl/npt-amalgamation
 *
 *  File: lisp_file_03.c
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

#include <float.h>
#include <memory.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lisp_file.h"


/************************************************************
 *  common_environment.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 25. Environment
 */

/* (defun decode-universal-time (time &optional zone) ...)
 *     -> second, minute, hour, day, month, year, day, daylight-p, zone
 *   time        integer
 *   zone        (or null (rational -24 24))
 *   second      (integer 0 59)  ;; ignoring leap seconds
 *   minute      (integer 0 59)
 *   hour        (integer 0 23)
 *   day         (integer 1 31)
 *   month       (integer 1 12)
 *   year        integer
 *   week        (integer 0 6)  ;; 0 means Monday.
 *   daylight-p  boolean
 */
static int function_decode_universal_time(Execute ptr, addr pos, addr zone)
{
	addr s, mi, h, d, m, y, w, daylight, z;

	Return(decode_universal_time_common_(ptr->local, pos, zone,
				&s, &mi, &h, &d, &m, &y, &w, &daylight, &z));
	setvalues_control(ptr, s, mi, h, d, m, y, w, daylight, z, NULL);
	return 0;
}

static void type_decode_universal_time(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Intplus);
	GetTypeTable(&values, TimeZone);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, DecodeUniversalTime);
	type_compiled_heap(args, values, ret);
}

static void defun_decode_universal_time(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DECODE_UNIVERSAL_TIME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_decode_universal_time);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_decode_universal_time(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun encode-universal-time
 *     (second minute hour day month year &optional zone) ...)
 *     -> universal-time
 *   second          (integer 0 59)
 *   minute          (integer 0 59)
 *   hour            (integer 0 23)
 *   day             (integer 1 31)
 *   month           (integer 1 12)
 *   year            (integer 0 *)
 *   zone            (rational -24 24)
 *   universal-time  (integer 0 *)
 */
static int function_encode_universal_time(Execute ptr, addr rest)
{
	Return(encode_universal_time_common_(ptr->local, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void type_encode_universal_time(addr *ret)
{
	addr args, values, plus, sec, hour, day, month, zone;

	GetTypeTable(&sec, TimeSecond);
	GetTypeTable(&hour, TimeHour);
	GetTypeTable(&day, TimeDay);
	GetTypeTable(&month, TimeMonth);
	GetTypeTable(&plus, Intplus);
	GetTypeTable(&zone, TimeZone);
	list_heap(&args, sec, sec, hour, day, month, plus, NULL);
	list_heap(&values, zone, NULL);
	typeargs_full(&args, args, values, Nil, Nil);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, ret);
}

static void defun_encode_universal_time(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ENCODE_UNIVERSAL_TIME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_encode_universal_time);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_encode_universal_time(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get-universal-time () ...) -> universal-time */
static int function_get_universal_time(Execute ptr)
{
	addr pos;

	Return(get_universal_time_call_(ptr->local, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_get_universal_time(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, Empty);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, ret);
}

static void defun_get_universal_time(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_UNIVERSAL_TIME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_get_universal_time);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get_universal_time(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get-decoded-time () ...)
 *   -> second, minute, hour, date, month, year, week, daylight-p, zone
 * (get-decoded-time) == (decode-universal-time (get-universal-time))
 */
static int function_get_decoded_time(Execute ptr)
{
	addr s, mi, h, d, m, y, w, daylight, z;

	Return(get_decoded_time_common_(ptr->local,
				&s, &mi, &h, &d, &m, &y, &w, &daylight, &z));
	setvalues_control(ptr, s, mi, h, d, m, y, w, daylight, z, NULL);
	return 0;
}

static void type_get_decoded_time(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, Empty);
	GetTypeValues(&values, DecodeUniversalTime);
	type_compiled_heap(args, values, ret);
}

static void defun_get_decoded_time(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_DECODED_TIME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_get_decoded_time);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get_decoded_time(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sleep (seconds) ...) -> null
 *   seconds  (real 0 *)
 */
static int function_sleep(Execute ptr, addr var)
{
	Return(sleep_common_(ptr, var));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_sleep(addr *ret)
{
	addr args, values;

	type2realf_ab_heap(Nil, 0, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_sleep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLEEP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_sleep);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_sleep(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun apropos (string-designator) ...) -> (values) */
static int function_apropos(Execute ptr, addr var, addr opt)
{
	Return(apropos_common_(ptr, var, (opt == Unbound)? Nil: opt));
	setvalues_nil_control(ptr);
	return 0;
}

static void type_apropos(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringDesignator);
	GetTypeTable(&values, PackageDesignatorNull);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Empty);
	type_compiled_heap(args, values, ret);
}

static void defun_apropos(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_APROPOS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_apropos);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_apropos(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun apropos-list (string-designator) ...) -> (values) */
static int function_apropos_list(Execute ptr, addr var, addr opt)
{
	Return(apropos_list_common_(ptr, var, (opt == Unbound)? Nil: opt, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_apropos_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringDesignator);
	GetTypeTable(&values, PackageDesignatorNull);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_apropos_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_APROPOS_LIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_apropos_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_apropos_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun describe (object &optional stream) ...) -> (values) */
static int function_describe(Execute ptr, addr var, addr opt)
{
	Return(describe_common_(ptr, var, opt));
	setvalues_nil_control(ptr);
	return 0;
}

static void type_describe(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, StreamDesignator);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Empty);
	type_compiled_heap(args, values, ret);
}

static void defun_describe(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DESCRIBE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_describe);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_describe(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro trace (&rest args) ...) -> list */
static int function_trace(Execute ptr, addr form, addr env)
{
	Return(trace_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defun_trace(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_TRACE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_trace);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro untrace (&rest args) ...) -> list */
static int function_untrace(Execute ptr, addr form, addr env)
{
	Return(untrace_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defun_untrace(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_UNTRACE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_untrace);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro step (form) ...) -> result */
static int function_step(Execute ptr, addr form, addr env)
{
	Return(step_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_step(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_STEP, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_step);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro time (form) ...) -> result) */
static int function_time(Execute ptr, addr form, addr env)
{
	Return(time_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_time(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_TIME, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_time);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defconstant internal-time-units-per-second [imlementation-dependency]) */
static void defconstant_internal_time_units_per_second(void)
{
	addr symbol, value;
	fixnum units;

	GetConst(COMMON_INTERNAL_TIME_UNITS_PER_SECOND, &symbol);
	get_internal_time_units_per_second(&units);
	fixnum_heap(&value, units);
	SetValueSymbol(symbol, value);
}


/* (defun get-internal-real-time () ...) -> (integer 0 *) */
static int function_get_internal_real_time(Execute ptr)
{
	addr pos;

	Return(get_internal_real_time_common_(ptr->local, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_get_internal_real_time(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_INTERNAL_REAL_TIME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_get_internal_real_time);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, GetInternalRealTime);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get-internal-run-time () ...) -> (integer 0 *) */
static int function_get_internal_run_time(Execute ptr)
{
	addr pos;

	get_internal_run_time_common(&pos);
	setresult_control(ptr, pos);
	return 0;
}

static void defun_get_internal_run_time(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_INTERNAL_RUN_TIME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_get_internal_run_time);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, GetInternalRealTime);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun disassemble (extended-function-designator) ...) -> nil */
static int function_disassemble(Execute ptr, addr var)
{
	Return(disassemble_common_(ptr, var));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_disassemble(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, FunctionDesignator);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_disassemble(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DISASSEMBLE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_disassemble);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_disassemble(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun room (&optional x) ...) -> null
 *   x  (member t nil :default)
 */
static int function_room(Execute ptr, addr var)
{
	Return(room_common_(ptr, var));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_room(addr *ret)
{
	addr args, values;

	GetConst(KEYWORD_DEFAULT, &args);
	type_member_heap(&args, Nil, T, args, NULL);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_room(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ROOM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_room);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_room(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ed (&optional x) ...) -> null */
static int function_ed(Execute ptr, addr var)
{
	Return(ed_common_(ptr, var));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_ed(addr *ret)
{
	addr args, values, type1, type2, type3;

	GetTypeTable(&type1, Null);
	GetTypeTable(&type2, PathnameDesignator);
	GetTypeTable(&type3, FunctionName);
	type3or_heap(type1, type2, type3, &args);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_ed(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ED, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_ed);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_ed(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun inspect (object) ...) -> null */
static int function_inspect(Execute ptr, addr var)
{
	Return(inspect_common_(ptr, var));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_inspect(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_inspect(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INSPECT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_inspect);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_inspect(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun dribble (&optional pathname) ...) -> null */
static int function_dribble(Execute ptr, addr var)
{
	Return(dribble_common_(ptr, (var == Unbound)? Nil: var));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_dribble(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_dribble(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DRIBBLE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_dribble);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_dribble(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun lisp-implementation-type () ...) -> (or string null)  */
static int function_lisp_implementation_type(Execute ptr)
{
	addr pos;

	implementation_type_common(&pos);
	setresult_control(ptr, pos);
	return 0;
}

static void defun_lisp_implementation_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LISP_IMPLEMENTATION_TYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_lisp_implementation_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun lisp-implementation-version () ...) -> (or string null)  */
static int function_lisp_implementation_version(Execute ptr)
{
	addr pos;

	implementation_version_common(&pos);
	setresult_control(ptr, pos);
	return 0;
}

static void defun_lisp_implementation_version(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LISP_IMPLEMENTATION_VERSION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_lisp_implementation_version);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun short-site-name () ...) -> (or string null) */
static int function_short_site_name(Execute ptr)
{
	addr pos;

	Return(short_site_name_common_(&pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_short_site_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SHORT_SITE_NAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_short_site_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun long-site-name () ...) -> (or string null) */
static int function_long_site_name(Execute ptr)
{
	addr pos;

	Return(long_site_name_common_(&pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_long_site_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LONG_SITE_NAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_long_site_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun machine-instance() ...) -> (or string null)  */
static int function_machine_instance(Execute ptr)
{
	addr pos;

	Return(machine_instance_common_(&pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_machine_instance(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MACHINE_INSTANCE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_machine_instance);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun machine-type() ...) -> (or string null)  */
static int function_machine_type(Execute ptr)
{
	addr pos;

	Return(machine_type_common_(&pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_machine_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MACHINE_TYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_machine_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun machine-version() ...) -> (or string null)  */
static int function_machine_version(Execute ptr)
{
	addr pos;

	Return(machine_version_common_(&pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_machine_version(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MACHINE_VERSION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_machine_version);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun software-type() ...) -> (or string null)  */
static int function_software_type(Execute ptr)
{
	addr pos;

	Return(software_type_common_(&pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_software_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SOFTWARE_TYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_software_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun software-version() ...) -> (or string null)  */
static int function_software_version(Execute ptr)
{
	addr pos;

	Return(software_version_common_(&pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_software_version(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SOFTWARE_VERSION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_software_version);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun user-homedir-pathname (&optinal host) ...) -> result
 *   host    (or string list (eql :unspecific)
 *   result  (or pathname null)
 */
static int function_user_homedir_pathname(Execute ptr, addr host)
{
	/* (declare (ignore host)) */
	Return(user_homedir_pathname_common_(ptr, &host));
	setresult_control(ptr, host);
	return 0;
}

static void type_user_homedir_pathname(addr *ret)
{
	addr args, values, type1, type2, type3;

	GetTypeTable(&type1, String);
	GetTypeTable(&type2, List);
	GetConst(KEYWORD_UNSPECIFIC, &type3);
	type_eql_heap(type3, &type3);
	type3or_heap(type1, type2, type3, &args);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, PathnameNull);
	type_compiled_heap(args, values, ret);
}

static void defun_user_homedir_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_USER_HOMEDIR_PATHNAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_user_homedir_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_user_homedir_pathname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_environment(void)
{
	SetPointerCall(defun, var1opt1, decode_universal_time);
	SetPointerCall(defun, dynamic, encode_universal_time);
	SetPointerCall(defun, empty, get_universal_time);
	SetPointerCall(defun, empty, get_decoded_time);
	SetPointerCall(defun, var1, sleep);
	SetPointerCall(defun, var1opt1, apropos);
	SetPointerCall(defun, var1opt1, apropos_list);
	SetPointerCall(defun, var1opt1, describe);
	SetPointerCall(defmacro, macro, trace);
	SetPointerCall(defmacro, macro, untrace);
	SetPointerCall(defmacro, macro, step);
	SetPointerCall(defmacro, macro, time);
	SetPointerCall(defun, empty, get_internal_real_time);
	SetPointerCall(defun, empty, get_internal_run_time);
	SetPointerCall(defun, var1, disassemble);
	SetPointerCall(defun, opt1, room);
	SetPointerCall(defun, opt1, ed);
	SetPointerCall(defun, var1, inspect);
	SetPointerCall(defun, opt1, dribble);
	SetPointerCall(defun, empty, lisp_implementation_type);
	SetPointerCall(defun, empty, lisp_implementation_version);
	SetPointerCall(defun, empty, short_site_name);
	SetPointerCall(defun, empty, long_site_name);
	SetPointerCall(defun, empty, machine_instance);
	SetPointerCall(defun, empty, machine_type);
	SetPointerCall(defun, empty, machine_version);
	SetPointerCall(defun, empty, software_type);
	SetPointerCall(defun, empty, software_version);
	SetPointerCall(defun, opt1, user_homedir_pathname);
}

void build_common_environment(void)
{
	defun_decode_universal_time();
	defun_encode_universal_time();
	defun_get_universal_time();
	defun_get_decoded_time();
	defun_sleep();
	defun_apropos();
	defun_apropos_list();
	defun_describe();
	defun_trace();
	defun_untrace();
	defmacro_step();
	defmacro_time();
	defconstant_internal_time_units_per_second();
	defun_get_internal_real_time();
	defun_get_internal_run_time();
	defun_disassemble();
	defun_room();
	defun_ed();
	defun_inspect();
	defun_dribble();
	defun_lisp_implementation_type();
	defun_lisp_implementation_version();
	defun_short_site_name();
	defun_long_site_name();
	defun_machine_instance();
	defun_machine_type();
	defun_machine_version();
	defun_software_type();
	defun_software_version();
	defun_user_homedir_pathname();
}


/************************************************************
 *  common_eval.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 3. Evaluation and Compilation
 */

/* (defmacro lambda (&whole right &rest args)  ...) */
static int function_lambda(Execute ptr, addr form, addr env)
{
	lambda_common(form, &form);
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_lambda(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_LAMBDA, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_lambda);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun compile (name &optional definition) ...)
 *     -> function, warnings-p, failure-p
 *   name        (or function-name null)
 *   definition  (or cons function)
 *   function    (or function-name function)
 *   warnings-p  boolean
 *   failure-p   boolean
 */
static int function_compile(Execute ptr, addr var, addr opt)
{
	addr x, y, z;
	Return(compile_common_(ptr, var, opt, &x, &y, &z));
	setvalues_control(ptr, x, y, z, NULL);
	return 0;
}

static void type_compile(addr *ret)
{
	addr args, values, x, y, type1, type2;

	/* args */
	GetTypeTable(&type1, FunctionName);
	GetTypeTable(&type2, Null);
	type2or_heap(type1, type2, &x);
	GetTypeTable(&type1, Cons);
	GetTypeTable(&type2, Function);
	type2or_heap(type1, type2, &y);
	typeargs_var1opt1(&args, x, y);
	/* values */
	GetTypeTable(&type1, FunctionName);
	GetTypeTable(&type2, Function);
	type2or_heap(type1, type2, &x);
	GetTypeTable(&y, Boolean);
	typevalues_values3(&values, x, y, y);
	/* result */
	type_compiled_heap(args, values, ret);
}

static void defun_compile(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPILE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_compile);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_compile(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eval (form) ...) -> result */
static int function_eval(Execute ptr, addr var)
{
	return eval_common_(ptr, var);
}

static void type_eval(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_eval(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EVAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_eval);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_eval(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* special-operator eval-when */
static void defspecial_eval_when(void)
{
	DefineSpecialOperator(COMMON_EVAL_WHEN);
}


/* special-operator load-time-value */
static void defspecial_load_time_value(void)
{
	DefineSpecialOperator(COMMON_LOAD_TIME_VALUE);
}


/* special-operator quote */
static void defspecial_quote(void)
{
	DefineSpecialOperator(COMMON_QUOTE);
}


/* (defun compiler-macro-function (name &optional env) ...)
 *     -> function
 *   name      function-name
 *   env       environment
 *   function  (or function null)
 */
static int function_compiler_macro_function(Execute ptr, addr var, addr env)
{
	Return(compiler_macro_function_common_(var, env, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_compiler_macro_function(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, FunctionName);
	GetTypeTable(&values, Environment);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, FunctionNull);
	type_compiled_heap(args, values, ret);
}

static void defun_compiler_macro_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPILER_MACRO_FUNCTION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_compiler_macro_function);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_compiler_macro_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf compiler-macro-function) (value name &optional env) ...)
 *     -> function
 *   values    (or function null)
 *   name      function-name
 *   env       environment
 *   function  (or function null)
 */
static int function_setf_compiler_macro_function(
		Execute ptr, addr value, addr var, addr env)
{
	Return(setf_compiler_macro_function_common_(value, var, env));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_compiler_macro_function(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&type, FunctionNull);
	GetTypeTable(&args, FunctionName);
	GetTypeTable(&values, Environment);
	typeargs_var2opt1(&args, type, args, values);
	GetTypeValues(&values, FunctionNull);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_compiler_macro_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPILER_MACRO_FUNCTION, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_setf_compiler_macro_function);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_compiler_macro_function(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defmacro define-compiler-macro (name lambda &body form) ...) -> name */
static int function_define_compiler_macro(Execute ptr, addr form, addr env)
{
	Return(define_compiler_macro_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_define_compiler_macro(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_COMPILER_MACRO, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_define_compiler_macro);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defmacro (name args &body body) ...) */
static int function_defmacro(Execute ptr, addr form, addr env)
{
	Return(defmacro_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defmacro(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFMACRO, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defmacro);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun macro-function (symbol &optional environment) ...) -> function) */
static int function_macro_function(Execute ptr, addr symbol, addr env)
{
	Return(macro_function_common_(symbol, env, &symbol));
	setresult_control(ptr, symbol);
	return 0;
}

static void type_macro_function(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&type, EnvironmentNull);
	typeargs_var1opt1(&args, args, type);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_macro_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MACRO_FUNCTION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_macro_function);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_macro_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf macro-function) (value symbol &optional env) ...) -> function
 *   The function accept the environment object, but ignored.
 *   If there is a non-nil environemnt argument,
 *     sbcl  -> error.
 *     clisp -> update global macro.
 *     ccl   -> update global macro.
 */
static int function_setf_macro_function(Execute ptr, addr value, addr symbol, addr env)
{
	Return(setmacro_symbol_(symbol, value));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_macro_function(addr *ret)
{
	addr args, values, type, env;

	GetTypeTable(&args, Function);
	GetTypeTable(&type, Symbol);
	GetTypeTable(&env, Environment);
	typeargs_var2opt1(&args, args, type, env);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_macro_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MACRO_FUNCTION, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_setf_macro_function);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_macro_function(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun macroexpand (form &optional env) ...) -> expansion, expansion-p */
static int function_macroexpand(Execute ptr, addr form, addr env)
{
	Return(macroexpand_common_(ptr, form, env, &form, &env));
	setvalues_control(ptr, form, env, NULL);
	return 0;
}

static void defun_macroexpand(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MACROEXPAND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_macroexpand);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroExpand);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun macroexpand-1 (form &optional env) ...) -> expansion, expansion-p */
static int function_macroexpand_1(Execute ptr, addr form, addr env)
{
	Return(macroexpand_1_common_(ptr, form, env, &form, &env));
	setvalues_control(ptr, form, env, NULL);
	return 0;
}

static void defun_macroexpand_1(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MACROEXPAND_1, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_macroexpand_1);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroExpand);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro define-symbol-macro (symbol expansion) ...) -> symbol */
static int function_define_symbol_macro(Execute ptr, addr form, addr env)
{
	Return(define_symbol_macro_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_define_symbol_macro(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_SYMBOL_MACRO, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_define_symbol_macro);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator symbol-macrolet */
static void defspecial_symbol_macrolet(void)
{
	DefineSpecialOperator(COMMON_SYMBOL_MACROLET);
}


/* (defun proclaim (declaration) ...) -> null */
static int function_proclaim(Execute ptr, addr var)
{
	Return(proclaim_common_(ptr, var));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_proclaim(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_proclaim(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PROCLAIM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_proclaim);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_proclaim(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro declaim (&rest declarations)  ...) */
static int function_declaim(Execute ptr, addr form, addr env)
{
	Return(declaim_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_declaim(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DECLAIM, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_declaim);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator locally */
static void defspecial_locally(void)
{
	DefineSpecialOperator(COMMON_LOCALLY);
}


/* special-operator the */
static void defspecial_the(void)
{
	DefineSpecialOperator(COMMON_THE);
}


/* (define-setf-expander the (type expr &environment env) ...) */
static void define_setf_expander_the(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_THE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_setf_the);
	SetSetfMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun special-operator-p (symbol) ...) -> boolean */
static int function_special_operator_p(Execute ptr, addr var)
{
	setbool_control(ptr, get_special_operator(var));
	return 0;
}

static void defun_special_operator_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SPECIAL_OPERATOR_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_special_operator_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Symbol_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun constantp (form &optional env) ...) -> boolean
 *   form  object
 *   env   (or environment null)
 */
static int function_constantp(Execute ptr, addr var, addr opt)
{
	Return(constantp_common_(ptr, var, opt, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_constantp(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&type, EnvironmentNull);
	typeargs_var1opt1(&args, args, type);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_constantp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONSTANTP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_constantp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_constantp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_eval(void)
{
	SetPointerCall(defmacro,  macro,     lambda);
	SetPointerCall(defun,     var1opt1,  compile);
	SetPointerCall(defun,     var1,      eval);
	SetPointerCall(defun,     var1opt1,  compiler_macro_function);
	SetPointerCall(defun,     var2opt1,  setf_compiler_macro_function);
	SetPointerCall(defmacro,  macro,     define_compiler_macro);
	SetPointerCall(defmacro,  macro,     defmacro);
	SetPointerCall(defun,     var1opt1,  macro_function);
	SetPointerCall(defun,     var2opt1,  setf_macro_function);
	SetPointerCall(defun,     var1opt1,  macroexpand);
	SetPointerCall(defun,     var1opt1,  macroexpand_1);
	SetPointerCall(defmacro,  macro,     define_symbol_macro);
	SetPointerCall(defun,     var1,      proclaim);
	SetPointerCall(defmacro,  macro,     declaim);
	SetPointerCall(defmacro,  macro,     setf_the);
	SetPointerCall(defun,     var1,      special_operator_p);
	SetPointerCall(defun,     var1opt1,  constantp);
}

void build_common_eval(void)
{
	defmacro_lambda();
	defun_compile();
	defun_eval();
	defspecial_eval_when();
	defspecial_load_time_value();
	defspecial_quote();
	defun_compiler_macro_function();
	defun_setf_compiler_macro_function();
	defmacro_define_compiler_macro();
	defmacro_defmacro();
	defun_macro_function();
	defun_setf_macro_function();
	defun_macroexpand();
	defun_macroexpand_1();
	defmacro_define_symbol_macro();
	defspecial_symbol_macrolet();
	defun_proclaim();
	defmacro_declaim();
	defspecial_locally();
	defspecial_the();
	define_setf_expander_the();
	defun_special_operator_p();
	defun_constantp();
}


/************************************************************
 *  common_filenames.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 19. Filenames
 */

/* (defun pathname (pathspec) ...) -> pathname */
static int function_pathname(Execute ptr, addr var)
{
	Return(pathname_common_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Pathname);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-pathname (&key host device directory name type version defaults case)
 *     ...) -> pathname
 *   host       (or string symbol)
 *   device     (or string symbol)  ;; (eql :unspecific))
 *   directory  (or list string (member :wild :wild-inferiors :unspecific))
 *   name       (or string cons (member nil :wild))
 *   type       (or string (member nil :wild :unspecific)))
 *   version    (or (integer 1 *) (member nil :wild :unspecific :newest))
 *   defaults   (or pathname null)  ;; default *default-pathname-defaults*
 *   case       (member :common :local)
 */
static int function_make_pathname(Execute ptr, addr rest)
{
	Return(make_pathname_common_(ptr, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void type_make_pathname(addr *ret)
{
	addr args, values, name, value, common, keylocal;
	addr key, key1, key2, key3, key4, key5, key6, key7, key8;

	KeyTypeTable(&key1, HOST, PathnameHost);
	KeyTypeTable(&key2, DEVICE, PathnameHost);
	KeyTypeTable(&key3, DIRECTORY, PathnameDirectory);
	KeyTypeTable(&key4, NAME, PathnameName);
	KeyTypeTable(&key5, TYPE, PathnameType);
	KeyTypeTable(&key6, VERSION, PathnameVersion);

	/* defaults   (or pathname null)  ;; default *default-pathname-defaults* */
	GetConst(KEYWORD_DEFAULTS, &name);
	GetTypeTable(&value, PathnameNull);
	cons_heap(&key7, name, value);
	/* case       (member :common :local) */
	GetConst(KEYWORD_CASE, &name);
	GetConst(KEYWORD_COMMON, &common);
	GetConst(KEYWORD_LOCAL, &keylocal);
	type_member_heap(&value, common, keylocal, NULL);
	cons_heap(&key8, name, value);
	/* key */
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, key8, NULL);
	/* type */
	typeargs_key(&args, key);
	GetTypeValues(&values, Pathname);
	type_compiled_heap(args, values, ret);
}

static void defun_make_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_PATHNAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_make_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_pathname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathnamep (object) ...) -> boolean */
static int function_pathnamep(Execute ptr, addr var)
{
	pathnamep_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_pathnamep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAMEP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_pathnamep);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-host (pathname &key case) ...) -> (or string symbol) */
static int function_pathname_host(Execute ptr, addr pos, addr rest)
{
	Return(pathname_host_common_(ptr, pos, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_pathname_host(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, PathnameCase);
	GetTypeTable(&values, PathnameHost);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_pathname_host(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_HOST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_pathname_host);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_host(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-device (pathname &key case) ...) -> (or string symbol) */
static int function_pathname_device(Execute ptr, addr pos, addr rest)
{
	Return(pathname_device_common_(ptr, pos, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_pathname_device(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, PathnameCase);
	GetTypeTable(&values, PathnameDevice);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_pathname_device(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_DEVICE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_pathname_device);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_device(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-directory (pathname &key case) ...) -> (or string symbol) */
static int function_pathname_directory(Execute ptr, addr pos, addr rest)
{
	Return(pathname_directory_common_(ptr, pos, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_pathname_directory(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, PathnameCase);
	GetTypeTable(&values, PathnameDirectory);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_pathname_directory(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_DIRECTORY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_pathname_directory);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_directory(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-name (pathname &key case) ...) -> (or string symbol) */
static int function_pathname_name(Execute ptr, addr pos, addr rest)
{
	Return(pathname_name_common_(ptr, pos, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_pathname_name(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, PathnameCase);
	GetTypeTable(&values, PathnameName);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_pathname_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_NAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_pathname_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-type (pathname &key case) ...) -> (or string symbol) */
static int function_pathname_type(Execute ptr, addr pos, addr rest)
{
	Return(pathname_type_common_(ptr, pos, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_pathname_type(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, PathnameCase);
	GetTypeTable(&values, PathnameType);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_pathname_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_TYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_pathname_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_type(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-version (pathname) ...) -> (or string symbol) */
static int function_pathname_version(Execute ptr, addr pos)
{
	Return(pathname_version_common_(ptr, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_pathname_version(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1(&args, args);
	GetTypeTable(&values, PathnameVersion);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_pathname_version(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_VERSION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_pathname_version);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_version(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun load-logical-pathname-translations (host) ...) -> boolean
 *   load-file:
 *     (merge-pathnames
 *       (make-pathname :name host)
 *       :defaults lisp-system::*load-logical-pathname-translations*)
 *   format:
 *     (logical-path1 physical-path1)
 *     (logical-path2 physical-path2)
 *     ...
 */
static int function_load_logical_pathname_translations(Execute ptr, addr pos)
{
	Return(load_logical_pathname_translations_common_(ptr, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_load_logical_pathname_translations(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, String);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_load_logical_pathname_translations(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOAD_LOGICAL_PATHNAME_TRANSLATIONS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_load_logical_pathname_translations);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_load_logical_pathname_translations(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun logical-pathname-translations (host) ...) -> list */
static int function_logical_pathname_translations(Execute ptr, addr host)
{
	Return(logical_pathname_translations_common_(host, &host));
	setresult_control(ptr, host);
	return 0;
}

static void type_logical_pathname_translations(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, String);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_logical_pathname_translations(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGICAL_PATHNAME_TRANSLATIONS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_logical_pathname_translations);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_logical_pathname_translations(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf logical-pathname-translations) (value host) ...) -> list */
static int function_setf_logical_pathname_translations(Execute ptr,
		addr value, addr host)
{
	Return(setf_logical_pathname_translations_common_(ptr, host, value));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_logical_pathname_translations(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	GetTypeTable(&values, String);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_logical_pathname_translations(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGICAL_PATHNAME_TRANSLATIONS, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_setf_logical_pathname_translations);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_logical_pathname_translations(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun logical-pathname (pathspec) ...) -> logical-pathname
 *   pathspec  (or logical-pathname string stream
 */
static int function_logical_pathname(Execute ptr, addr pos)
{
	Return(logical_pathname_common_(ptr, &pos, pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_logical_pathname(addr *ret)
{
	addr args, values, pathname, string, stream;

	GetTypeTable(&pathname, LogicalPathname);
	GetTypeTable(&string, String);
	GetTypeTable(&stream, Stream);
	type3or_heap(pathname, string, stream, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, LogicalPathname);
	type_compiled_heap(args, values, ret);
}

static void defun_logical_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGICAL_PATHNAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_logical_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_logical_pathname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defvar *default-pathname-defaults* [implementation-dependence]) */
static void defvar_default_pathname_defaults(void)
{
	default_pathname_defaults_common();
}


/* (defun namestring (pathname) ...) -> namestring */
static int function_namestring(Execute ptr, addr pos)
{
	Return(namestring_common_(ptr, &pos, pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_namestring(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NAMESTRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Namestring);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun file-namestring (pathname) ...) -> namestring */
static int function_file_namestring(Execute ptr, addr pos)
{
	Return(file_namestring_common_(ptr, &pos, pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_file_namestring(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILE_NAMESTRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_file_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Namestring);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun directory-namestring (pathname) ...) -> namestring */
static int function_directory_namestring(Execute ptr, addr pos)
{
	Return(directory_namestring_common_(ptr, &pos, pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_directory_namestring(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DIRECTORY_NAMESTRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_directory_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Namestring);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun host-namestring (pathname) ...) -> namestring */
static int function_host_namestring(Execute ptr, addr pos)
{
	Return(host_namestring_common_(ptr, &pos, pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_host_namestring(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_HOST_NAMESTRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_host_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Namestring);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun enough-namestring (pathname &optional defaults) ...) -> string */
static int function_enough_namestring(Execute ptr, addr pos, addr defaults)
{
	Return(enough_namestring_common_(ptr, &pos, pos, defaults));
	setresult_control(ptr, pos);
	return 0;
}

static void type_enough_namestring(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1opt1(&args, args, args);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, ret);
}

static void defun_enough_namestring(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ENOUGH_NAMESTRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_enough_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_enough_namestring(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parse-namestring (thing
 *     &optional host defaults
 *     &key start end junk-allowed) ...) -> (values pathname position)
 *   thing         (or string stream pathname)  ;; pathname-designator
 *   host          (or string symbol)  ;; (or valid-pathname-host null)
 *   defaults      (or string stream pathname)  ;; pathname-designator
 *   start         keyword-start
 *   end           keyword-end
 *   junk-allowed  t  ;; boolean
 */
static int function_parse_namestring(Execute ptr, addr thing, addr rest)
{
	Return(parse_namestring_common_(ptr, thing, rest, &thing, &rest));
	setvalues_control(ptr, thing, rest, NULL);
	return 0;
}

static void type_parse_namestring(addr *ret)
{
	addr args, values, type, key, key1, key2, key3;

	/* key */
	KeyTypeTable(&key1, START, KeywordStart);
	KeyTypeTable(&key2, END, KeywordEnd);
	KeyTypeTable(&key3, JUNK_ALLOWED, T);
	list_heap(&key, key1, key2, key3, NULL);
	/* type */
	GetTypeTable(&args, PathnameDesignator);
	GetTypeTable(&values, PathnameHost);
	typeargs_var1opt2key(&args, args, values, args, key);
	GetTypeTable(&values, PathnameNull);
	GetTypeTable(&type, Index);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_parse_namestring(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PARSE_NAMESTRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_parse_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_parse_namestring(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun wild-pathname-p (path &optional field) ...) -> boolean
 *   path   pathname-designator
 *   field  (member :host :device :directory :name :type :version nil)
 */
static int function_wild_pathname_p(Execute ptr, addr pos, addr field)
{
	Return(wild_pathname_p_common_(ptr, &pos, pos, field));
	setresult_control(ptr, pos);
	return 0;
}

static void type_wild_pathname_p(addr *ret)
{
	addr args, values, v1, v2, v3, v4, v5, v6;

	/* member */
	GetConst(KEYWORD_HOST, &v1);
	GetConst(KEYWORD_DEVICE, &v2);
	GetConst(KEYWORD_DIRECTORY, &v3);
	GetConst(KEYWORD_NAME, &v4);
	GetConst(KEYWORD_TYPE, &v5);
	GetConst(KEYWORD_VERSION, &v6);
	type_member_heap(&values, Nil, v1, v2, v3, v4, v5, v6, NULL);
	/* type */
	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_wild_pathname_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WILD_PATHNAME_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_wild_pathname_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_wild_pathname_p(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-match-p (path wildcard) ...) -> boolean
 *   path      pathname-designator
 *   wildcard  pathname-designator
 */
static int function_pathname_match_p(Execute ptr, addr pos, addr wild)
{
	Return(pathname_match_p_common_(ptr, &pos, pos, wild));
	setresult_control(ptr, pos);
	return 0;
}

static void type_pathname_match_p(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_pathname_match_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_MATCH_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_pathname_match_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_match_p(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun translate-pathname (source from to &key) ...) -> pathname
 *   source  pathname-designator
 *   to      pathname-designator
 *   from    pathname-designator
 */
static int function_translate_pathname(Execute ptr, addr pos, addr from, addr to)
{
	Return(translate_pathname_common_(ptr, &pos, pos, from, to));
	setresult_control(ptr, pos);
	return 0;
}

static void type_translate_pathname(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	typeargs_var3(&args, args, args, args);
	GetTypeValues(&values, Pathname);
	type_compiled_heap(args, values, ret);
}

static void defun_translate_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TRANSLATE_PATHNAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_translate_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_translate_pathname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun translate-logical-pathname (pathname &key) ...) -> physical-pathname) */
static int function_translate_logical_pathname(Execute ptr, addr pos)
{
	Return(translate_logical_pathname_common_(ptr, &pos, pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_translate_logical_pathname(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Pathname);
	type_compiled_heap(args, values, ret);
}

static void defun_translate_logical_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TRANSLATE_LOGICAL_PATHNAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_translate_logical_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_translate_logical_pathname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun merge-pathnames (pathname &optional defaults version) ...) -> pathname */
static int function_merge_pathnames(Execute ptr,
		addr pos, addr defaults, addr version)
{
	Return(merge_pathnames_common_(ptr, &pos, pos, defaults, version));
	setresult_control(ptr, pos);
	return 0;
}

static void type_merge_pathnames(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	GetTypeTable(&values, PathnameVersion);
	typeargs_var1opt2(&args, args, args, values);
	GetTypeValues(&values, Pathname);
	type_compiled_heap(args, values, ret);
}

static void defun_merge_pathnames(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MERGE_PATHNAMES, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt2(pos, p_defun_merge_pathnames);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_merge_pathnames(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_filenames(void)
{
	SetPointerCall(defun, var1, pathname);
	SetPointerCall(defun, dynamic, make_pathname);
	SetPointerCall(defun, var1, pathnamep);
	SetPointerCall(defun, var1dynamic, pathname_host);
	SetPointerCall(defun, var1dynamic, pathname_device);
	SetPointerCall(defun, var1dynamic, pathname_directory);
	SetPointerCall(defun, var1dynamic, pathname_name);
	SetPointerCall(defun, var1dynamic, pathname_type);
	SetPointerCall(defun, var1, pathname_version);
	SetPointerCall(defun, var1, load_logical_pathname_translations);
	SetPointerCall(defun, var1, logical_pathname_translations);
	SetPointerCall(defun, var2, setf_logical_pathname_translations);
	SetPointerCall(defun, var1, logical_pathname);
	SetPointerCall(defun, var1, namestring);
	SetPointerCall(defun, var1, file_namestring);
	SetPointerCall(defun, var1, directory_namestring);
	SetPointerCall(defun, var1, host_namestring);
	SetPointerCall(defun, var1opt1, enough_namestring);
	SetPointerCall(defun, var1dynamic, parse_namestring);
	SetPointerCall(defun, var1opt1, wild_pathname_p);
	SetPointerCall(defun, var2, pathname_match_p);
	SetPointerCall(defun, var3, translate_pathname);
	SetPointerCall(defun, var1, translate_logical_pathname);
	SetPointerCall(defun, var1opt2, merge_pathnames);
}

void build_common_filenames(void)
{
	defun_pathname();
	defun_make_pathname();
	defun_pathnamep();
	defun_pathname_host();
	defun_pathname_device();
	defun_pathname_directory();
	defun_pathname_name();
	defun_pathname_type();
	defun_pathname_version();
	defun_load_logical_pathname_translations();
	defun_logical_pathname_translations();
	defun_setf_logical_pathname_translations();
	defun_logical_pathname();
	defvar_default_pathname_defaults();
	defun_namestring();
	defun_file_namestring();
	defun_directory_namestring();
	defun_host_namestring();
	defun_enough_namestring();
	defun_parse_namestring();
	defun_wild_pathname_p();
	defun_pathname_match_p();
	defun_translate_pathname();
	defun_translate_logical_pathname();
	defun_merge_pathnames();
}


/************************************************************
 *  common_files.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 20. Files
 */

/* (defun directory (pathname &key) ...) -> list */
static int function_directory(Execute ptr, addr pos)
{
	Return(directory_files_(ptr, &pos, pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_directory(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_directory(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DIRECTORY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_directory);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_directory(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun probe-file (pathspec) ...) -> truename */
static int function_probe_file(Execute ptr, addr pos)
{
	Return(probe_file_files_(ptr, &pos, pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_probe_file(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PROBE_FILE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_probe_file);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Pathname);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ensure-directories-exist (pathspec &key verbose) ...)
 *     -> pathspec, created
 *   pathspec  pathname-designator
 *   verbose   t  ;; boolean
 *   created   boolean
 */
static int function_ensure_directories_exist(Execute ptr, addr pos, addr rest)
{
	if (GetKeyArgs(rest, KEYWORD_VERBOSE, &rest))
		rest = Nil;
	Return(ensure_directories_exist_files_(ptr, &pos, &rest, pos, rest != Nil));
	setvalues_control(ptr, pos, rest, NULL);

	return 0;
}

static void type_ensure_directories_exist(addr *ret)
{
	addr args, values, type;

	/* key */
	KeyTypeTable(&type, VERBOSE, T);
	conscar_heap(&type, type);
	/* type */
	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1key(&args, args, type);
	GetTypeTable(&values, Pathname);
	GetTypeTable(&type, Boolean);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_directories_exist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ENSURE_DIRECTORIES_EXIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_ensure_directories_exist);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_ensure_directories_exist(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun truename (pathspec) ...) -> pathname */
static int function_truename(Execute ptr, addr pos)
{
	Return(truename_files_(ptr, pos, &pos, 1));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_truename(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TRUENAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_truename);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Pathname);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun file-author (pathspec) ...) -> (or string null) */
static int function_file_author(Execute ptr, addr pos)
{
	Return(file_author_files_(ptr, &pos, pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_file_author(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1(&args, args);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, ret);
}

static void defun_file_author(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILE_AUTHOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_file_author);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_file_author(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun file-write-date (pathspec) ...) -> (or integer null) */
static int function_file_write_date(Execute ptr, addr pos)
{
	Return(file_write_date_files_(ptr, &pos, pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_file_write_date(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1(&args, args);
	GetTypeTable(&values, IntegerNull);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_file_write_date(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILE_WRITE_DATE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_file_write_date);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_file_write_date(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rename-file (file rename) ...) -> new from to */
static int function_rename_file(Execute ptr, addr file1, addr file2)
{
	addr file3;

	Return(rename_file_files_(ptr, &file1, &file2, &file3, file1, file2));
	setvalues_control(ptr, file1, file2, file3, NULL);

	return 0;
}

static void type_rename_file(addr *ret)
{
	addr args, values;

	GetTypeTable(&values, PathnameDesignator);
	typeargs_var2(&args, values, values);
	typevalues_values3(&values, values, values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_rename_file(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RENAME_FILE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_rename_file);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_rename_file(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun delete-file (file) ...) -> (eql t) */
static int function_delete_file(Execute ptr, addr pos)
{
	Return(delete_file_files_(ptr, pos));
	setresult_control(ptr, T);
	return 0;
}

static void type_delete_file(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1(&args, args);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(args, values, ret);
}

static void defun_delete_file(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DELETE_FILE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_delete_file);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_delete_file(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun file-error-pathname (condition) ...) -> pathname-designator */
static int function_file_error_pathname(Execute ptr, addr var)
{
	Return(file_error_pathname_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_file_error_pathname(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, FileError);
	typeargs_var1(&args, args);
	GetTypeTable(&values, T);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_file_error_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILE_ERROR_PATHNAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_file_error_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_file_error_pathname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_files(void)
{
	SetPointerCall(defun, var1, directory);
	SetPointerCall(defun, var1, probe_file);
	SetPointerCall(defun, var1dynamic, ensure_directories_exist);
	SetPointerCall(defun, var1, truename);
	SetPointerCall(defun, var1, file_author);
	SetPointerCall(defun, var1, file_write_date);
	SetPointerCall(defun, var2, rename_file);
	SetPointerCall(defun, var1, delete_file);
	SetPointerCall(defun, var1, file_error_pathname);
}

void build_common_files(void)
{
	defun_directory();
	defun_probe_file();
	defun_ensure_directories_exist();
	defun_truename();
	defun_file_author();
	defun_file_write_date();
	defun_rename_file();
	defun_delete_file();
	defun_file_error_pathname();
}


/************************************************************
 *  common_hashtables.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 18. Hash Tables
 */

/* (defun make-hash-table
 *     (&key test size rehash-size rehash-threshold) ...) -> hashtable
 *   test              (or symbol function)  ;; eq eql equal equalp
 *     -> `(member eq eql equal equalp ,#'eq ,#'eql ,#'equal ,#'equalp)
 *   size              index
 *   rehash-size       (or (integer 1 *) (float (1.0) *))
 *   rehash-threshold  (real 0 1)
 */
static int function_make_hash_table(Execute ptr, addr rest)
{
	Return(make_hash_table_common_(rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void type_make_hash_table(addr *ret)
{
	addr args, values, key, type;
	addr key1, key2, key3, key4;

	/* test */
	GetConst(KEYWORD_TEST, &key);
	GetTypeTable(&type, FunctionDesignator);
	cons_heap(&key1, key, type);
	/* size */
	GetConst(KEYWORD_SIZE, &key);
	GetTypeTable(&type, Index);
	cons_heap(&key2, key, type);
	/* rehash-size */
	GetConst(KEYWORD_REHASH_SIZE, &key);
	GetTypeTable(&type, RehashSize);
	cons_heap(&key3, key, type);
	/* rehash-threshold */
	GetConst(KEYWORD_REHASH_THRESHOLD, &key);
	GetTypeTable(&type, RehashThreshold);
	cons_heap(&key4, key, type);
	/* &key */
	list_heap(&args, key1, key2, key3, key4, NULL);
	typeargs_key(&args, args);
	/* values */
	GetTypeTable(&values, Hashtable);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_hash_table(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_HASH_TABLE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_make_hash_table);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_hash_table(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hash-table-p (object) ...) -> boolean */
static int function_hash_table_p(Execute ptr, addr var)
{
	setbool_control(ptr, hashtablep(var));
	return 0;
}

static void defun_hash_table_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_HASH_TABLE_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_hash_table_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hash-table-count (hash-table) ...) -> count
 *   count  (integer 0 *)
 */
static int function_hash_table_count(Execute ptr, addr var)
{
	hash_table_count_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_hash_table_count(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_HASH_TABLE_COUNT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_hash_table_count);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, HashTableCount);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hash-table-rehash-size (hash-table) ...) -> rehash-size
 *    rehash-size  (or (integer 1 *) (float (1.0) *))
 */
static int function_hash_table_rehash_size(Execute ptr, addr var)
{
	Return(hash_table_rehash_size_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_hash_table_rehash_size(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Hashtable);
	typeargs_var1(&args, args);
	GetTypeTable(&values, RehashSize);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_hash_table_rehash_size(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_HASH_TABLE_REHASH_SIZE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_hash_table_rehash_size);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_hash_table_rehash_size(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hash-table-rehash-threshold (hash-table) ...) -> threshold
 *   threshold  (real 0 1)
 */
static int function_hash_table_rehash_threshold(Execute ptr, addr var)
{
	hash_table_rehash_threshold_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_hash_table_rehash_threshold(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Hashtable);
	typeargs_var1(&args, args);
	GetTypeTable(&values, RehashThreshold);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_hash_table_rehash_threshold(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_HASH_TABLE_REHASH_THRESHOLD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_hash_table_rehash_threshold);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_hash_table_rehash_threshold(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hash-table-size (hash-table) ...) -> size
 *   size  (integer 0 *)
 */
static int function_hash_table_size(Execute ptr, addr var)
{
	hash_table_size_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_hash_table_size(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_HASH_TABLE_SIZE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_hash_table_size);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, HashTableCount);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hash-table-test (hash-table) ...) -> symbol */
static int function_hash_table_test(Execute ptr, addr var)
{
	hash_table_test_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_hash_table_test(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Hashtable);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_hash_table_test(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_HASH_TABLE_TEST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_hash_table_test);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_hash_table_test(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun gethash (key hash-table &optional default) ...) -> value, boolean */
static int function_gethash(Execute ptr, addr key, addr table, addr value)
{
	Return(gethash_common_(key, table, value, &key, &value));
	setvalues_control(ptr, key, value, NULL);
	return 0;
}

static void type_gethash(addr *ret)
{
	addr args, values, type1, type2;

	GetTypeTable(&args, Hashtable);
	GetTypeTable(&type1, T);
	GetTypeTable(&type2, Boolean);
	typeargs_var2opt1(&args, type1, args, type1);
	typevalues_values2(&values, type1, type2);
	type_compiled_heap(args, values, ret);
}

static void defun_gethash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GETHASH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_gethash);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_gethash(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf gethash) (value key hash-table) ...) -> value */
static int function_setf_gethash(Execute ptr,
		addr value, addr key, addr table, addr defvalue)
{
	/* defvalue is ignored. */
	Return(setf_gethash_common_(ptr->local, value, key, table));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_gethash(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Hashtable);
	GetTypeTable(&values, T);
	typeargs_var3opt1(&args, values, values, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_gethash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GETHASH, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var3opt1(pos, p_defun_setf_gethash);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_gethash(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun remhash (key hash-table) ...) -> boolean */
static int function_remhash(Execute ptr, addr key, addr table)
{
	Return(remhash_common_(key, table, &table));
	setresult_control(ptr, table);
	return 0;
}

static void type_remhash(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Hashtable);
	GetTypeTable(&values, T);
	typeargs_var2(&args, values, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_remhash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REMHASH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_remhash);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_remhash(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun maphash (function hash-table) ...) -> null */
static int function_maphash(Execute ptr, addr call, addr table)
{
	Return(maphash_common_(ptr, call, table));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_maphash(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Hashtable);
	GetTypeTable(&values, FunctionDesignator);
	typeargs_var2(&args, values, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_maphash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPHASH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_maphash);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_maphash(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-hash-table-iterator ((name hash-table) &body body) ...) */
static int function_with_hash_table_iterator(Execute ptr, addr form, addr env)
{
	Return(with_hash_table_iterator_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_hash_table_iterator(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_HASH_TABLE_ITERATOR, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_hash_table_iterator);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun clrhash (hash-table) ...) -> hash-table */
static int function_clrhash(Execute ptr, addr var)
{
	clear_hashtable(var);
	setresult_control(ptr, var);
	return 0;
}

static void type_clrhash(addr *ret)
{
	addr args, values;

	GetTypeTable(&values, Hashtable);
	typeargs_var1(&args, values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_clrhash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CLRHASH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_clrhash);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_clrhash(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sxhash (object) ...) -> hash-code
 *    hash-code  index  ;; fixnum
 */
static int function_sxhash(Execute ptr, addr var)
{
	Return(sxhash_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_sxhash(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeTable(&values, Index);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_sxhash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SXHASH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_sxhash);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_sxhash(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_hashtables(void)
{
	SetPointerCall(defun, dynamic, make_hash_table);
	SetPointerCall(defun, var1, hash_table_p);
	SetPointerCall(defun, var1, hash_table_count);
	SetPointerCall(defun, var1, hash_table_rehash_size);
	SetPointerCall(defun, var1, hash_table_rehash_threshold);
	SetPointerCall(defun, var1, hash_table_size);
	SetPointerCall(defun, var1, hash_table_test);
	SetPointerCall(defun, var2opt1, gethash);
	SetPointerCall(defun, var3opt1, setf_gethash);
	SetPointerCall(defun, var2, remhash);
	SetPointerCall(defun, var2, maphash);
	SetPointerCall(defmacro, macro, with_hash_table_iterator);
	SetPointerCall(defun, var1, clrhash);
	SetPointerCall(defun, var1, sxhash);
}

void build_common_hashtables(void)
{
	defun_make_hash_table();
	defun_hash_table_p();
	defun_hash_table_count();
	defun_hash_table_rehash_size();
	defun_hash_table_rehash_threshold();
	defun_hash_table_size();
	defun_hash_table_test();
	defun_gethash();
	defun_setf_gethash();
	defun_remhash();
	defun_maphash();
	defmacro_with_hash_table_iterator();
	defun_clrhash();
	defun_sxhash();
}


/************************************************************
 *  common_header.c
 ************************************************************/

/*
 *  clang function
 */
void defconstant_symbol(addr symbol, addr value)
{
	setspecial_symbol(symbol);
	SetValueSymbol(symbol, value);
	SetStatusValue(symbol, LISPSTATUS_READONLY, 1);
}

static void check_function_macro(addr symbol)
{
	addr check;

	GetFunctionSymbol(symbol, &check);
	if (check != Unbound)
		Abort("COMMON-LISP function error.");
	getmacro_symbol(symbol, &check);
	if (check != Unbound)
		Abort("COMMON-LISP macro-function error.");
}

void setfunction_common(addr symbol, addr value)
{
	check_function_macro(symbol);
	SetFunctionSymbol(symbol, value);
}

void setmacro_common(addr symbol, addr value)
{
	check_function_macro(symbol);
	setmacro_symbol(symbol, value);
}

void setsetfmacro_common(addr symbol, addr value)
{
	addr check;

	getsetfmacro_symbol(symbol, &check);
	if (check != Unbound)
		Abort("COMMON-ILSP setf-macro-function error.");
	setsetfmacro_symbol(symbol, value);
}

void define_special_operator(constindex index)
{
	addr symbol;
	GetConstant(index, &symbol);
	set_special_operator(symbol);
}


/*
 *  :start, :end
 */
static int getsize_keyword_start_(addr pos, addr *reta, size_t *rets)
{
	if (pos == Unbound) {
		fixnum_heap(reta, 0);
		*rets = 0;
	}
	else {
		Return(getindex_integer_(pos, rets));
		*reta = pos;
	}

	return 0;
}

static int getsize_keyword_end_(size_t size, addr pos, addr *reta, size_t *rets)
{
	if (pos == Unbound || pos == Nil) {
		make_index_integer_heap(reta, size);
		*rets = size;
	}
	else {
		Return(getindex_integer_(pos, rets));
		*reta = pos;
	}

	return 0;
}

static int keyword_start_end_addr_(size_t size,
		addr kstart, addr kend,
		addr astart, addr aend,
		size_t *pstart, size_t *pend)
{
	size_t start, end;

	Return(getsize_keyword_start_(astart, &astart, &start));
	Return(getsize_keyword_end_(size, aend, &aend, &end));
	if (size < start) {
		return fmte_("The ~S position ~S must be less than "
				"the sequence length.", kstart, astart, NULL);
	}
	if (size < end) {
		return fmte_("The ~S position ~S must be less than "
				"equal to the sequence length.", kend, aend, NULL);
	}
	if (end < start) {
		return fmte_("The ~S position ~S must be less than "
				"equal to the ~S position ~S.", kstart, astart, kend, aend, NULL);
	}
	*pstart = start;
	*pend = end;
	return 0;
}

static int keyword_start_end_const_(constindex cstart, constindex cend,
		size_t size, addr rest, size_t *pstart, size_t *pend)
{
	addr kstart, kend, astart, aend;

	GetConstant(cstart, &kstart);
	GetConstant(cend, &kend);
	if (getplist_safe(rest, kstart, &astart))
		astart = Unbound;
	if (getplist_safe(rest, kend, &aend))
		aend = Unbound;

	return keyword_start_end_addr_(size,
			kstart, kend, astart, aend, pstart, pend);
}

int keyword_start_end_(size_t size, addr rest, size_t *pstart, size_t *pend)
{
	return keyword_start_end_const_(CONSTANT_KEYWORD_START, CONSTANT_KEYWORD_END,
			size, rest, pstart, pend);
}

int keyword_start1_end1_(size_t size, addr rest, size_t *pstart, size_t *pend)
{
	return keyword_start_end_const_(CONSTANT_KEYWORD_START1, CONSTANT_KEYWORD_END1,
			size, rest, pstart, pend);
}

int keyword_start2_end2_(size_t size, addr rest, size_t *pstart, size_t *pend)
{
	return keyword_start_end_const_(CONSTANT_KEYWORD_START2, CONSTANT_KEYWORD_END2,
			size, rest, pstart, pend);
}

int keyword_start_end_value_(size_t size,
		addr astart, addr aend, size_t *pstart, size_t *pend)
{
	addr kstart, kend;

	GetConst(KEYWORD_START, &kstart);
	GetConst(KEYWORD_END, &kend);
	return keyword_start_end_addr_(size,
			kstart, kend, astart, aend, pstart, pend);
}


/************************************************************
 *  common_iteration.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 6. Iteration
 */

/* (defmacro do/do* (var end [declaration] [tag-statement]) ...) -> result */
static int function_do(Execute ptr, addr form, addr env)
{
	Return(do_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_do(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DO, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_do);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}

static int function_doa(Execute ptr, addr form, addr env)
{
	Return(doa_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_doa(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DOA, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_doa);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro dotimes (var count &optional result) [declaration] [body]) -> result */
static int function_dotimes(Execute ptr, addr form, addr env)
{
	Return(dotimes_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_dotimes(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DOTIMES, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_dotimes);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro dolist (var value &optional result) [declaration] [body]) -> result */
static int function_dolist(Execute ptr, addr form, addr env)
{
	Return(dolist_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_dolist(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DOLIST, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_dolist);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro loop (&rest args) ...) */
static int function_loop(Execute ptr, addr form, addr env)
{
	Return(loop_common_(ptr, &form, form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_loop(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_LOOP, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_loop);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/*
 *  function
 */
void init_common_iteration(void)
{
	SetPointerCall(defmacro,  macro,  do);
	SetPointerCall(defmacro,  macro,  doa);
	SetPointerCall(defmacro,  macro,  dotimes);
	SetPointerCall(defmacro,  macro,  dolist);
	SetPointerCall(defmacro,  macro,  loop);
}

void build_common_iteration(void)
{
	defmacro_do();
	defmacro_doa();
	defmacro_dotimes();
	defmacro_dolist();
	defmacro_loop();
}


/************************************************************
 *  common_numbers.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 12. Numbers
 */

/* (defun = (first &rest numbers) ...) -> boolean */
static int function_number_equal(Execute ptr, addr left, addr rest)
{
	int check;
	Return(number_equal_common_(ptr->local, left, rest, &check));
	setbool_control(ptr, check);
	return 0;
}

static void defun_number_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMBER_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_number_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Number_Equal);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun /= (first &rest numbers) ...) -> boolean */
static int function_number_not_equal(Execute ptr, addr left, addr rest)
{
	int check;
	Return(number_not_equal_common_(ptr->local, left, rest, &check));
	setbool_control(ptr, check);
	return 0;
}

static void defun_number_not_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMBER_NOT_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_number_not_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Number_Equal);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun < (first &rest numbers) ...) -> boolean */
static int function_number_less(Execute ptr, addr left, addr rest)
{
	int check;
	Return(number_less_common_(ptr->local, left, rest, &check));
	setbool_control(ptr, check);
	return 0;
}

static void defun_number_less(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMBER_LESS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_number_less);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Number_Compare);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun > (first &rest numbers) ...) -> boolean */
static int function_number_greater(Execute ptr, addr left, addr rest)
{
	int check;
	Return(number_greater_common_(ptr->local, left, rest, &check));
	setbool_control(ptr, check);
	return 0;
}

static void defun_number_greater(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMBER_GREATER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_number_greater);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Number_Compare);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun <= (first &rest numbers) ...) -> boolean */
static int function_number_less_equal(Execute ptr, addr left, addr rest)
{
	int check;
	Return(number_less_equal_common_(ptr->local, left, rest, &check));
	setbool_control(ptr, check);
	return 0;
}

static void defun_number_less_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMBER_LESS_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_number_less_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Number_Compare);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun >= (first &rest numbers) ...) -> boolean */
static int function_number_greater_equal(Execute ptr, addr left, addr rest)
{
	int check;
	Return(number_greater_equal_common_(ptr->local, left, rest, &check));
	setbool_control(ptr, check);
	return 0;
}

static void defun_number_greater_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMBER_GREATER_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_number_greater_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Number_Compare);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun max (real &rest real) ...) -> real */
static int function_max(Execute ptr, addr left, addr rest)
{
	Return(max_common_(ptr->local, left, rest, &left));
	setresult_control(ptr, left);
	return 0;
}

static void defun_max(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAX, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_max);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Max);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun min (real &rest real) ...) -> real */
static int function_min(Execute ptr, addr left, addr rest)
{
	Return(min_common_(ptr->local, left, rest, &left));
	setresult_control(ptr, left);
	return 0;
}

static void defun_min(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MIN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_min);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Max);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun minusp (real) ...) -> boolean */
static int function_minusp(Execute ptr, addr var)
{
	int check;

	Return(minusp_real_(var, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_minusp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MINUSP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_minusp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Minusp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun plusp (real) ...) -> boolean */
static int function_plusp(Execute ptr, addr var)
{
	int check;

	Return(plusp_real_(var, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_plusp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PLUSP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_plusp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Minusp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun zerop (real) ...) -> boolean */
static int function_zerop(Execute ptr, addr var)
{
	int check;

	Return(zerop_number_(var, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_zerop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ZEROP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_zerop);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Zerop);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun floor (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   integer
 *   remainder  real
 */
static int function_floor(Execute ptr, addr var, addr div)
{
	Return(floor_common_(ptr->local, var, div, &var, &div));
	setvalues_control(ptr, var, div, NULL);
	return 0;
}

static void defun_floor(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FLOOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_floor);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Floor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ffloor (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   real
 *   remainder  real
 */
static int function_ffloor(Execute ptr, addr var, addr div)
{
	Return(ffloor_common_(ptr->local, var, div, &var, &div));
	setvalues_control(ptr, var, div, NULL);
	return 0;
}

static void defun_ffloor(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FFLOOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_ffloor);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Ffloor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ceiling (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   integer
 *   remainder  real
 */
static int function_ceiling(Execute ptr, addr var, addr div)
{
	Return(ceiling_common_(ptr->local, var, div, &var, &div));
	setvalues_control(ptr, var, div, NULL);
	return 0;
}

static void defun_ceiling(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CEILING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_ceiling);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Floor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun fceiling (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   real
 *   remainder  real
 */
static int function_fceiling(Execute ptr, addr var, addr div)
{
	Return(fceiling_common_(ptr->local, var, div, &var, &div));
	setvalues_control(ptr, var, div, NULL);
	return 0;
}

static void defun_fceiling(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FCEILING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_fceiling);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Ffloor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun truncate (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   integer
 *   remainder  real
 */
static int function_truncate(Execute ptr, addr var, addr div)
{
	Return(truncate_common_(ptr->local, var, div, &var, &div));
	setvalues_control(ptr, var, div, NULL);
	return 0;
}

static void defun_truncate(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TRUNCATE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_truncate);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Floor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ftruncate (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   real
 *   remainder  real
 */
static int function_ftruncate(Execute ptr, addr var, addr div)
{
	Return(ftruncate_common_(ptr->local, var, div, &var, &div));
	setvalues_control(ptr, var, div, NULL);
	return 0;
}

static void defun_ftruncate(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FTRUNCATE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_ftruncate);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Ffloor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun round (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   integer
 *   remainder  real
 */
static int function_round(Execute ptr, addr var, addr div)
{
	Return(round_common_(ptr->local, var, div, &var, &div));
	setvalues_control(ptr, var, div, NULL);
	return 0;
}

static void defun_round(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ROUND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_round);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Floor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun fround (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   real
 *   remainder  real
 */
static int function_fround(Execute ptr, addr var, addr div)
{
	Return(fround_common_(ptr->local, var, div, &var, &div));
	setvalues_control(ptr, var, div, NULL);
	return 0;
}

static void defun_fround(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FROUND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_fround);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Ffloor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun cis (real) ...) -> number */
static int function_cis(Execute ptr, addr var)
{
	Return(cis_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_cis(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Real);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Number);
	type_compiled_heap(args, values, ret);
}

static void defun_cis(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CIS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_cis);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_cis(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sin (number) ...) -> number */
static int function_sin(Execute ptr, addr var)
{
	Return(sin_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_sin(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_sin);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun cos (number) ...) -> number */
static int function_cos(Execute ptr, addr var)
{
	Return(cos_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_cos(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_cos);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun tan (number) ...) -> number */
static int function_tan(Execute ptr, addr var)
{
	Return(tan_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_tan(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TAN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_tan);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sinh (number) ...) -> number */
static int function_sinh(Execute ptr, addr var)
{
	Return(sinh_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_sinh(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SINH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_sinh);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun cosh (number) ...) -> number */
static int function_cosh(Execute ptr, addr var)
{
	Return(cosh_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_cosh(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COSH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_cosh);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun tanh (number) ...) -> number */
static int function_tanh(Execute ptr, addr var)
{
	Return(tanh_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_tanh(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TANH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_tanh);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun asin (number) ...) -> number */
static int function_asin(Execute ptr, addr var)
{
	Return(asin_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_asin(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASIN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_asin);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun acos (number) ...) -> number */
static int function_acos(Execute ptr, addr var)
{
	Return(acos_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_acos(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ACOS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_acos);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun atan (number &optional real) ...) -> number */
static int function_atan(Execute ptr, addr var, addr opt)
{
	Return(atan_optional_common_(var, opt, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_atan(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Number);
	GetTypeTable(&values, Real);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Number);
	type_compiled_heap(args, values, ret);
}

static void defun_atan(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ATAN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_atan);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_atan(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun asinh (number) ...) -> number */
static int function_asinh(Execute ptr, addr var)
{
	Return(asinh_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_asinh(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASINH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_asinh);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun acosh (number) ...) -> number */
static int function_acosh(Execute ptr, addr var)
{
	Return(acosh_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_acosh(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ACOSH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_acosh);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun atanh (number) ...) -> number */
static int function_atanh(Execute ptr, addr var)
{
	Return(atanh_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_atanh(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ATANH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_atanh);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun exp (number) ...) -> number */
static int function_exp(Execute ptr, addr var)
{
	Return(exp_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_exp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EXP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_exp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun expt (number number) ...) -> number */
static int function_expt(Execute ptr, addr base, addr power)
{
	Return(expt_common_(ptr->local, &base, base, power));
	setresult_control(ptr, base);
	return 0;
}

static void type_expt(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Number);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Number);
	type_compiled_heap(args, values, ret);
}

static void defun_expt(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EXPT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_expt);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_expt(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun + (&rest number) ...) -> number */
static int function_plus(Execute ptr, addr rest)
{
	Return(plus_common_(ptr->local, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_plus(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PLUS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_plus);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Plus);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun - (&rest number) ...) -> number */
static int function_minus(Execute ptr, addr left, addr rest)
{
	Return(minus_common_(ptr->local, left, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_minus(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MINUS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_minus);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Minus);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun * (&rest number) ...) -> number */
static int function_asterisk(Execute ptr, addr rest)
{
	Return(asterisk_common_(ptr->local, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_asterisk(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASTERISK, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_asterisk);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Plus);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun / (number &rest number) ...) -> number */
static int function_slash(Execute ptr, addr left, addr rest)
{
	Return(slash_common_(ptr->local, left, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_slash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLASH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_slash);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Minus);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun 1+ (number) ...) -> number */
static int function_oneplus(Execute ptr, addr var)
{
	Return(oneplus_number_common_(ptr->local, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_oneplus(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ONE_PLUS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_oneplus);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, OnePlus);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun 1- (number) ...) -> number */
static int function_oneminus(Execute ptr, addr var)
{
	Return(oneminus_number_common_(ptr->local, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_oneminus(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ONE_MINUS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_oneminus);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, OnePlus);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun abs (number) ...) value
 *    value  (real 0 *)
 */
static int function_abs(Execute ptr, addr var)
{
	Return(abs_number_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_abs(addr *ret)
{
	addr args, values, aster;

	GetTypeTable(&args, Number);
	typeargs_var1(&args, args);
	fixnum_heap(&values, 0);
	GetTypeTable(&aster, Asterisk);
	type4_heap(LISPDECL_REAL, Nil, values, aster, aster, &values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_abs(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ABS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_abs);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_abs(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun evenp (integer) ...) -> boolean */
static int function_evenp(Execute ptr, addr var)
{
	int check;

	Return(evenp_integer_(var, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_evenp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EVENP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_evenp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Evenp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun oddp (integer) ...) -> boolean */
static int function_oddp(Execute ptr, addr var)
{
	int check;

	Return(evenp_integer_(var, &check));
	setbool_control(ptr, ! check);

	return 0;
}

static void defun_oddp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ODDP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_oddp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Evenp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun gcd (&rest integer) ...) -> (integer 0 *) */
static int function_gcd(Execute ptr, addr args)
{
	Return(gcd_number_(ptr->local, args, &args));
	setresult_control(ptr, args);
	return 0;
}

static void defun_gcd(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GCD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_gcd);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Gcd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun lcm (&rest integer) ...) -> (integer 0 *) */
static int function_lcm(Execute ptr, addr args)
{
	Return(lcm_number_(ptr->local, args, &args));
	setresult_control(ptr, args);
	return 0;
}

static void defun_lcm(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LCM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_lcm);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Gcd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro incf (place &optional number) ...) -> number) */
static int function_incf(Execute ptr, addr form, addr env)
{
	Return(incf_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_incf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_INCF, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_incf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro decf (place &optional number) ...) -> number) */
static int function_decf(Execute ptr, addr form, addr env)
{
	Return(decf_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_decf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DECF, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_decf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun log (number &optional number) ...) -> number */
static int function_log(Execute ptr, addr value, addr base)
{
	Return(log_common_(value, base, &value));
	setresult_control(ptr, value);
	return 0;
}

static void type_log(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Number);
	typeargs_var1opt1(&args, args, args);
	GetTypeValues(&values, Number);
	type_compiled_heap(args, values, ret);
}

static void defun_log(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOG, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_log);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_log(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mod (real divisor) ...) -> modules
 *   real     real
 *   divisor  real
 *   modules  real
 */
static int function_mod(Execute ptr, addr num, addr div)
{
	Return(mod_number_common_(ptr->local, num, div, &num));
	setresult_control(ptr, num);
	return 0;
}

static void defun_mod(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MOD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_mod);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mod);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rem (real divisor) ...) -> modules
 *   real     real
 *   divisor  real
 *   modules  real
 */
static int function_rem(Execute ptr, addr num, addr div)
{
	Return(rem_number_common_(ptr->local, num, div, &num));
	setresult_control(ptr, num);
	return 0;
}

static void defun_rem(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_rem);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mod);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun signum (number) ...) -> number */
static int function_signum(Execute ptr, addr var)
{
	Return(signum_number_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_signum(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIGNUM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_signum);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sqrt (number) ...) -> number */
static int function_sqrt(Execute ptr, addr var)
{
	Return(sqrt_number_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_sqrt(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SQRT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_sqrt);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun isqrt (natural) ...) -> natural
 *   natural  (integer 0 *)
 */
static int function_isqrt(Execute ptr, addr var)
{
	Return(isqrt_number_common_(ptr->local, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_isqrt(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Intplus);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, ret);
}

static void defun_isqrt(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ISQRT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_isqrt);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_isqrt(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-random-state (&optional state) ...) -> random-state
 *   state  (or random-state null (eql t))
 */
static int function_make_random_state(Execute ptr, addr opt)
{
	if (opt == Unbound)
		opt = Nil;
	Return(make_random_state_heap_(ptr, &opt, opt));
	setresult_control(ptr, opt);
	return 0;
}

static void type_make_random_state(addr *ret)
{
	addr args, values, type1, type2, type3;

	vector4_heap(&args, 3);
	GetTypeTable(&type1, RandomState);
	GetTypeTable(&type2, Null);
	GetTypeTable(&type3, EqlT);
	type3or_heap(type1, type2, type3, &args);
	typeargs_opt1(&args, args);
	typevalues_result(&values, type1);
	type_compiled_heap(args, values, ret);
}

static void defun_make_random_state(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_RANDOM_STATE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_make_random_state);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_random_state(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun random (limit &optional random-state) ...) -> value
 *   limit  (or (integer (0) *) (float (0) *))
 *   value  (or (integer 0 *) (float 0 *))
 */
static int function_random(Execute ptr, addr limit, addr state)
{
	Return(random_common_(ptr, limit, state, &limit));
	setresult_control(ptr, limit);
	return 0;
}

static void type_random(addr *ret)
{
	addr args, values, type1, type2;

	GetTypeTable(&args, RandomState);
	type2integer_ab_heap(T, 0, &type1);
	type2float_ab_heap(T, 0.0f, &type2);
	type2or_heap(type1, type2, &type1);
	typeargs_var1opt1(&args, type1, args);
	type2integer_ab_heap(Nil, 0, &type1);
	type2float_ab_heap(Nil, 0.0f, &type2);
	type2or_heap(type1, type2, &values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_random(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RANDOM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_random);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_random(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun random-state-p (object) ...) -> boolean */
static int function_random_state_p(Execute ptr, addr pos)
{
	setbool_control(ptr, GetType(pos) == LISPTYPE_RANDOM_STATE);
	return 0;
}

static void defun_random_state_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RANDOM_STATE_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_random_state_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defvar *random-state* random-state) */
static void defvar_random_state(void)
{
	addr symbol, value, type;

	/* symbol */
	GetConst(SPECIAL_RANDOM_STATE, &symbol);
	Error(make_random_state_heap_(NULL, &value, T));
	SetValueSymbol(symbol, value);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, RandomState);
	settype_value_symbol(symbol, type);
}


/* (defun numberp (object) ...) -> boolean */
static int function_numberp(Execute ptr, addr pos)
{
	setbool_control(ptr, numberp(pos));
	return 0;
}

static void defun_numberp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMBERP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_numberp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun complex (real &optional imag) ...) -> result
 *   real    real
 *   imag    real
 *   result  (or rational complex)
 */
static int function_complex(Execute ptr, addr real, addr imag)
{
	Return(complex_heap_(&real, real, imag));
	setresult_control(ptr, real);
	return 0;
}

static void type_complex_common(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Real);
	typeargs_var1opt1(&args, args, args);
	GetTypeTable(&type, Rational);
	GetTypeTable(&values, Complex);
	type2or_heap(type, values, &values);
	type_compiled_heap(args, values, ret);
}

static void defun_complex(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPLEX, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_complex);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_complex_common(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun complexp (object) ...) -> boolean */
static int function_complexp(Execute ptr, addr pos)
{
	setbool_control(ptr, GetType(pos) == LISPTYPE_COMPLEX);
	return 0;
}

static void defun_complexp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPLEXP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_complexp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun conjugate (number) ...) -> number */
static int function_conjugate(Execute ptr, addr var)
{
	Return(conjugate_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_conjugate(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONJUGATE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_conjugate);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun phase (number) ...) -> number */
static int function_phase(Execute ptr, addr var)
{
	Return(phase_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_phase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PHASE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_phase);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun realpart (number) ...) -> real */
static int function_realpart(Execute ptr, addr var)
{
	Return(realpart_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_realpart(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REALPART, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_realpart);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, RealPart);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun imagpart (number) ...) -> real */
static int function_imagpart(Execute ptr, addr var)
{
	Return(imagpart_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_imagpart(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_IMAGPART, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_imagpart);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, RealPart);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun upgraded-complex-part-type (typespec &optional environment) ...)
 *   -> typespec
 */
static int function_upgraded_complex_part_type(Execute ptr, addr pos, addr env)
{
	if (env == Unbound)
		env = Nil;
	Return(upgraded_complex_common_(ptr, env, pos, &pos));
	setresult_control(ptr, pos);

	return 0;
}

static void defun_upgraded_complex_part_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UPGRADED_COMPLEX_PART_TYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_upgraded_complex_part_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, UpgradedType);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun realp (object) ...) -> boolean */
static int function_realp(Execute ptr, addr pos)
{
	setbool_control(ptr, realp(pos));
	return 0;
}

static void defun_realp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REALP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_realp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun numerator (rational) ...) -> integer */
static int function_numerator(Execute ptr, addr var)
{
	Return(numerator_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_numerator(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Rational);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Integer);
	type_compiled_heap(args, values, ret);
}

static void defun_numerator(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMERATOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_numerator);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_numerator(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun denominator (rational) ...) -> positive-integer */
static int function_denominator(Execute ptr, addr var)
{
	Return(denominator_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_denominator(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Rational);
	typeargs_var1(&args, args);
	type2integer_ab_heap(T, 0, &values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_denominator(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DENOMINATOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_denominator);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_denominator(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rational (number) ...) -> rational
 *   number    real
 *   rational  rational
 */
static int function_rational(Execute ptr, addr var)
{
	Return(rational_common_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_rational(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RATIONAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_rational);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Rational);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rationalize (number) ...) -> rational
 *   number    real
 *   rational  rational
 */
static int function_rationalize(Execute ptr, addr var)
{
	Return(rationalize_common_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_rationalize(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RATIONALIZE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_rationalize);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Rational);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rationalp (object) ...) -> boolean */
static int function_rationalp(Execute ptr, addr pos)
{
	setbool_control(ptr, rationalp(pos));
	return 0;
}

static void defun_rationalp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RATIONALP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_rationalp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ash (integer count) ...) -> shifted
 *   integer  integer
 *   count    integer
 *   shifted  integer
 */
static int function_ash(Execute ptr, addr pos, addr count)
{
	Return(ash_integer_common_(ptr->local, pos, count, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_ash(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Integer);
	type_compiled_heap(args, values, ret);
}

static void defun_ash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_ash);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_ash(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun integer-length (integer) ...) -> bits
 *   integer  integer
 *   bits     a non-negative integer
 */
static int function_integer_length(Execute ptr, addr var)
{
	Return(integer_length_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_integer_length(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, ret);
}

static void defun_integer_length(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INTEGER_LENGTH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_integer_length);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_integer_length(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun integerp (object) ...) -> boolean */
static int function_integerp(Execute ptr, addr pos)
{
	setbool_control(ptr, integerp(pos));
	return 0;
}

static void defun_integerp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INTEGERP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_integerp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parse-integer (string &key start end radix junk-allowed) ...)
 *     -> integer, pos
 *   string        string
 *   start         keyword-start
 *   end           keyword-end
 *   radix         radix-integer  ;; default 10
 *   junk-allowed  t  ;; boolean, default nil
 *   integer       (or null integer)
 *   pos           index
 */
static int function_parse_integer(Execute ptr, addr var, addr rest)
{
	Return(parse_integer_common_(ptr->local, var, rest, &var, &rest));
	setvalues_control(ptr, var, rest, NULL);
	return 0;
}

static void type_parse_integer(addr *ret)
{
	addr args, values, type, key, key1, key2, key3, key4;

	/* key */
	GetConst(KEYWORD_START, &key1);
	GetTypeTable(&values, KeywordStart);
	cons_heap(&key1, key1, values);
	GetConst(KEYWORD_END, &key2);
	GetTypeTable(&values, KeywordEnd);
	cons_heap(&key2, key2, values);
	GetConst(KEYWORD_RADIX, &key3);
	GetTypeTable(&values, RadixInteger);
	cons_heap(&key3, key3, values);
	GetConst(KEYWORD_JUNK_ALLOWED, &key4);
	GetTypeTable(&values, T);
	cons_heap(&key4, key4, values);
	list_heap(&key, key1, key2, key3, key4, NULL);
	/* type */
	GetTypeTable(&args, String);
	typeargs_var1key(&args, args, key);
	GetTypeTable(&values, IntegerNull);
	GetTypeTable(&type, Index);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_parse_integer(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PARSE_INTEGER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_parse_integer);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_parse_integer(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun boole (op integer1 integer2) ...) -> integer */
static int function_boole(Execute ptr, addr op, addr a, addr b)
{
	Return(boole_common_(ptr->local, op, a, b, &a));
	setresult_control(ptr, a);
	return 0;
}

static void type_boole(addr *ret)
{
	addr args, values;

	/* (integer 0 (Boole_Size)) */
	type4integer_heap(Nil, 0, T, (fixnum)Boole_Size, &args);
	GetTypeTable(&values, Integer);
	typeargs_var3(&args, args, values, values);
	GetTypeValues(&values, Integer);
	type_compiled_heap(args, values, ret);
}

static void defun_boole(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BOOLE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_boole);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_boole(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun logand (&rest integer) ...) -> integer */
static int function_logand(Execute ptr, addr args)
{
	Return(logand_common_(ptr->local, args, &args));
	setresult_control(ptr, args);
	return 0;
}

static void defun_logand(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGAND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_logand);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Logand);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun logandc1 (integer integer) ...) -> integer */
static int function_logandc1(Execute ptr, addr a, addr b)
{
	Return(logandc1_common_(ptr->local, a, b, &a));
	setresult_control(ptr, a);
	return 0;
}

static void defun_logandc1(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGANDC1, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_logandc1);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Logandc1);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun logandc2 (integer integer) ...) -> integer */
static int function_logandc2(Execute ptr, addr a, addr b)
{
	Return(logandc2_common_(ptr->local, a, b, &a));
	setresult_control(ptr, a);
	return 0;
}

static void defun_logandc2(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGANDC2, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_logandc2);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Logandc1);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun logeqv (&rest integer) ...) -> integer */
static int function_logeqv(Execute ptr, addr args)
{
	Return(logeqv_common_(ptr->local, args, &args));
	setresult_control(ptr, args);
	return 0;
}

static void defun_logeqv(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGEQV, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_logeqv);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Logand);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun logior (&rest integer) ...) -> integer */
static int function_logior(Execute ptr, addr args)
{
	Return(logior_common_(ptr->local, args, &args));
	setresult_control(ptr, args);
	return 0;
}

static void defun_logior(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGIOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_logior);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Logand);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun lognand (integer integer) ...) -> integer */
static int function_lognand(Execute ptr, addr a, addr b)
{
	Return(lognand_common_(ptr->local, a, b, &a));
	setresult_control(ptr, a);
	return 0;
}

static void defun_lognand(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGNAND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_lognand);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Logandc1);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun lognor (integer integer) ...) -> integer */
static int function_lognor(Execute ptr, addr a, addr b)
{
	Return(lognor_common_(ptr->local, a, b, &a));
	setresult_control(ptr, a);
	return 0;
}

static void defun_lognor(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGNOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_lognor);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Logandc1);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun lognot (integer) ...) -> integer */
static int function_lognot(Execute ptr, addr a)
{
	Return(lognot_common_(ptr->local, a, &a));
	setresult_control(ptr, a);
	return 0;
}

static void type_lognot(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Integer);
	type_compiled_heap(args, values, ret);
}

static void defun_lognot(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGNOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_lognot);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_lognot(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun logorc1 (integer integer) ...) -> integer */
static int function_logorc1(Execute ptr, addr a, addr b)
{
	Return(logorc1_common_(ptr->local, a, b, &a));
	setresult_control(ptr, a);
	return 0;
}

static void defun_logorc1(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGORC1, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_logorc1);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Logandc1);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun logorc2 (integer integer) ...) -> integer */
static int function_logorc2(Execute ptr, addr a, addr b)
{
	Return(logorc2_common_(ptr->local, a, b, &a));
	setresult_control(ptr, a);
	return 0;
}

static void defun_logorc2(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGORC2, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_logorc2);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Logandc1);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun logxor (&rest integer) ...) -> integer */
static int function_logxor(Execute ptr, addr args)
{
	Return(logxor_common_(ptr->local, args, &args));
	setresult_control(ptr, args);
	return 0;
}

static void defun_logxor(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGXOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_logxor);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Logand);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun logbitp (index integer) ...) -> boolean
 *   index    (integer 0 *)
 *   integer  integer
 *   boolean  boolean
 */
static int function_logbitp(Execute ptr, addr index, addr pos)
{
	int check;

	Return(logbitp_common_(index, pos, &check));
	setbool_control(ptr, check);

	return 0;
}

static void type_logbitp(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Intplus);
	GetTypeTable(&values, Integer);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_logbitp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGBITP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_logbitp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_logbitp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun logcount (integer) ...) -> (integer 0 *) */
static int function_logcount(Execute ptr, addr pos)
{
	size_t size;

	Return(logcount_common_(pos, &size));
	setresult_control(ptr, intsizeh(size));

	return 0;
}

static void type_logcount(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, ret);
}

static void defun_logcount(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGCOUNT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_logcount);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_logcount(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun logtest (integer integer) ...) -> boolean */
static int function_logtest(Execute ptr, addr left, addr right)
{
	int check;

	Return(logtest_common_(ptr->local, left, right, &check));
	setbool_control(ptr, check);

	return 0;
}

static void type_logtest(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_logtest(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGTEST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_logtest);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_logtest(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun byte (size position) ...) -> byte
 *   size      (integer 0 *)
 *   position  (integer 0 *)
 *   byte      byte-specifier
 */
static int function_byte(Execute ptr, addr size, addr posi)
{
	Return(byte_common_(size, posi, &size));
	setresult_control(ptr, size);
	return 0;
}

static void type_byte_call(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Intplus);
	typeargs_var2(&args, args, args);
	GetTypeTable(&values, ByteSpec);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_byte(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BYTE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_byte);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_byte_call(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun byte-size (byte) ...) -> (integer 0 *) */
static int function_byte_size(Execute ptr, addr var)
{
	byte_size_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_byte_size(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BYTE_SIZE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_byte_size);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ByteSize);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun byte-position (byte) ...) -> (integer 0 *) */
static int function_byte_position(Execute ptr, addr var)
{
	byte_position_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_byte_position(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BYTE_POSITION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_byte_position);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ByteSize);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun deposit-field (newbyte bytespec integer) ...) -> result
 *   newbyte   integer
 *   bytespec  bytespec
 *   integer   integer
 *   result    integer
 */
static int function_deposit_field(Execute ptr, addr left, addr spec, addr right)
{
	Return(deposit_field_common_(ptr->local, &left, left, spec, right));
	setresult_control(ptr, left);
	return 0;
}

static void defun_deposit_field(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DEPOSIT_FIELD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_deposit_field);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, DepositField);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun dpb (integer bytespec integer) ...) -> integer */
static int function_dpb(Execute ptr, addr left, addr spec, addr right)
{
	Return(dpb_common_(ptr->local, &left, left, spec, right));
	setresult_control(ptr, left);
	return 0;
}

static void defun_dpb(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DPB, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dpb);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, DepositField);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ldb (bytespec integer) ...) -> (integer 0 *) */
static int function_ldb(Execute ptr, addr spec, addr var)
{
	Return(ldb_common_(ptr->local, &var, spec, var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_ldb(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LDB, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_ldb);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Ldb);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (define-setf-expander ldb
 *   (bytespec place &environment env) ...)
 *   -> (integer 0 *)
 */
static void define_setf_expander_ldb(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_LDB, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_setf_ldb);
	SetSetfMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun ldb-test (bytespec integer) ...) -> boolean */
static int function_ldb_test(Execute ptr, addr spec, addr var)
{
	int check;
	Return(ldb_test_common_(spec, var, &check));
	setbool_control(ptr, check);
	return 0;
}

static void type_ldb_test(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, ByteSpec);
	GetTypeTable(&values, Integer);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_ldb_test(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LDB_TEST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_ldb_test);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_ldb_test(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mask-field (bytespec integer) ...) -> (integer 0 *) */
static int function_mask_field(Execute ptr, addr spec, addr var)
{
	Return(mask_field_common_(ptr->local, &var, spec, var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_mask_field(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MASK_FIELD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_mask_field);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Ldb);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (define-setf-expander mask-field
 *   (bytespec place &environment env) ...)
 *   -> (integer 0 *)
 */
static void define_setf_expander_mask_field(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_MASK_FIELD, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_setf_mask_field);
	SetSetfMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (decode-float (float) ...) -> significand, exponent, sign
 *   float        float
 *   significand  float
 *   exopnent     integer
 *   sign         float
 */
static int function_decode_float(Execute ptr, addr var)
{
	addr sig, exp, sign;

	Return(decode_float_common_(var, &sig, &exp, &sign));
	setvalues_control(ptr, sig, exp, sign, NULL);
	return 0;
}

static void type_decode_float(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&values, Float);
	typeargs_var1(&args, values);
	GetTypeTable(&type, Integer);
	typevalues_values3(&values, values, type, values);
	type_compiled_heap(args, values, ret);
}

static void defun_decode_float(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DECODE_FLOAT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_decode_float);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_decode_float(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun scale-float (float integer) -> scaled
 *   float    float
 *   integer  integer  (not a non-negative integer)
 *   scaled   float
 */
static int function_scale_float(Execute ptr, addr var, addr scale)
{
	Return(scale_float_common_(var, scale, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_scale_float(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Float);
	GetTypeTable(&values, Integer);
	typeargs_var2(&args, args, values);
	type_compiled_heap(args, values, ret);
}

static void defun_scale_float(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SCALE_FLOAT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_scale_float);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_scale_float(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun float-radix (float) ...) -> integer */
static int function_float_radix(Execute ptr, addr var)
{
	float_radix_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_float_radix(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Float);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Integer);
	type_compiled_heap(args, values, ret);
}

static void defun_float_radix(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FLOAT_RADIX, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_float_radix);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_float_radix(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun float-sign (float &optional float) -> float */
static int function_float_sign(Execute ptr, addr var1, addr var2)
{
	Return(float_sign_common_(var1, var2, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void type_float_sign(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Float);
	typeargs_var1opt1(&args, args, args);
	GetTypeValues(&values, Float);
	type_compiled_heap(args, values, ret);
}

static void defun_float_sign(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FLOAT_SIGN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_float_sign);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_float_sign(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun float-digits (float) ...) -> digits
 *   float   float
 *   digits  a non-negative integer
 */
static int function_float_digits(Execute ptr, addr var)
{
	Return(float_digits_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_float_digits(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FLOAT_DIGITS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_float_digits);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, FloatDigits);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun float-precision (float) ...) -> digits
 *   float   float
 *   digits  a non-negative integer
 */
static int function_float_precision(Execute ptr, addr var)
{
	Return(float_precision_common_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_float_precision(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FLOAT_PRECISION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_float_precision);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, FloatDigits);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun integer-decode-float (float) ...) -> significand, exponent, intsign
 *   float        float
 *   significand  integer  ;; not float
 *   exopnent     integer
 *   intsign      (member -1 1)
 */
static int function_integer_decode_float(Execute ptr, addr var)
{
	addr sig, exp, sign;

	Return(integer_decode_float_common_(ptr, var, &sig, &exp, &sign));
	setvalues_control(ptr, sig, exp, sign, NULL);

	return 0;
}

static void type_integer_decode_float(addr *ret)
{
	addr args, values, sign, v1, v2;

	GetTypeTable(&args, Float);
	typeargs_var1(&args, args);
	GetTypeTable(&values, Integer);
	fixnum_heap(&v1, -1);
	fixnum_heap(&v2, 1);
	type_member_heap(&sign, v1, v2, NULL);
	typevalues_values3(&values, values, values, sign);
	type_compiled_heap(args, values, ret);
}

static void defun_integer_decode_float(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INTEGER_DECODE_FLOAT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_integer_decode_float);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_integer_decode_float(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}




/* (defun float (real &optional prototype) ...) -> float */
static int function_float(Execute ptr, addr var, addr type)
{
	Return(float_common_(&var, var, type));
	setresult_control(ptr, var);
	return 0;
}

static void type_float_function(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Real);
	GetTypeTable(&values, Float);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Float);
	type_compiled_heap(args, values, ret);
}

static void defun_float(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FLOAT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_float);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_float_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun floatp (object) ...) -> boolean */
static int function_floatp(Execute ptr, addr pos)
{
	setbool_control(ptr, floatp(pos));
	return 0;
}

static void defun_floatp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FLOATP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_floatp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun arithmetic-error-operands (arithmetic-error) ...) -> list */
static int function_arithmetic_error_operands(Execute ptr, addr var)
{
	Return(arithmetic_error_operands_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_arithmetic_error_operands(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, ArithmeticError);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_arithmetic_error_operands(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARITHMETIC_ERROR_OPERANDS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_arithmetic_error_operands);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_arithmetic_error_operands(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun arithmetic-error-operation (arithmetic-error) ...) -> list */
static int function_arithmetic_error_operation(Execute ptr, addr var)
{
	Return(arithmetic_error_operation_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_arithmetic_error_operation(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, ArithmeticError);
	typeargs_opt1(&args, args);
	GetTypeTable(&values, FunctionDesignator);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_arithmetic_error_operation(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARITHMETIC_ERROR_OPERATION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_arithmetic_error_operation);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_arithmetic_error_operation(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defconstant pi 3.14159265358979323844l0) */
static void defconstant_pi(void)
{
	addr symbol, value;

	GetConst(COMMON_PI, &symbol);
	/* $ echo 'scale=20; a(1)*4' | bc -l */
	long_float_heap(&value, LISP_PI_LONG);
	defconstant_symbol(symbol, value);
}


/* (defconstant boole-1 ...) */
static void defconstant_boole(constindex index, enum Boole_Index value)
{
	addr symbol, pos;

	GetConstant(index, &symbol);
	fixnum_heap(&pos, (fixnum)value);
	defconstant_symbol(symbol, pos);
}
static void defconstant_boole_1(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_1, Boole_1);
}
static void defconstant_boole_2(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_2, Boole_2);
}
static void defconstant_boole_and(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_AND, Boole_And);
}
static void defconstant_boole_andc1(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_ANDC1, Boole_AndC1);
}
static void defconstant_boole_andc2(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_ANDC2, Boole_AndC2);
}
static void defconstant_boole_c1(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_C1, Boole_C1);
}
static void defconstant_boole_c2(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_C2, Boole_C2);
}
static void defconstant_boole_clr(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_CLR, Boole_Clr);
}
static void defconstant_boole_eqv(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_EQV, Boole_Eqv);
}
static void defconstant_boole_ior(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_IOR, Boole_Ior);
}
static void defconstant_boole_nand(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_NAND, Boole_Nand);
}
static void defconstant_boole_nor(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_NOR, Boole_Nor);
}
static void defconstant_boole_orc1(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_ORC1, Boole_Orc1);
}
static void defconstant_boole_orc2(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_ORC2, Boole_Orc2);
}
static void defconstant_boole_set(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_SET, Boole_Set);
}
static void defconstant_boole_xor(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_XOR, Boole_Xor);
}


/* (defconstant most-positive-fixnum ...) */
static void defconstant_most_positive_fixnum(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_POSITIVE_FIXNUM, &symbol);
	GetConst(FIXNUM_MAX, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant most-negative-fixnum ...) */
static void defconstant_most_negative_fixnum(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_NEGATIVE_FIXNUM, &symbol);
	GetConst(FIXNUM_MIN, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant most-positive-short-float ...) */
static void defconstant_most_positive_short_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_POSITIVE_SHORT_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_MOST_POSITIVE, &value); /* single */
	defconstant_symbol(symbol, value);
}


/* (defconstant most-positive-single-float ...) */
static void defconstant_most_positive_single_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_POSITIVE_SINGLE_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_MOST_POSITIVE, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant most-positive-double-float ...) */
static void defconstant_most_positive_double_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_POSITIVE_DOUBLE_FLOAT, &symbol);
	double_float_heap(&value, DBL_MAX);
	defconstant_symbol(symbol, value);
}


/* (defconstant most-positive-long-float ...) */
static void defconstant_most_positive_long_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_POSITIVE_LONG_FLOAT, &symbol);
	long_float_heap(&value, LDBL_MAX);
	defconstant_symbol(symbol, value);
}


/* (defconstant most-negative-short-float ...) */
static void defconstant_most_negative_short_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_NEGATIVE_SHORT_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_MOST_NEGATIVE, &value); /* single */
	defconstant_symbol(symbol, value);
}


/* (defconstant most-negative-single-float ...) */
static void defconstant_most_negative_single_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_NEGATIVE_SINGLE_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_MOST_NEGATIVE, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant most-negative-double-float ...) */
static void defconstant_most_negative_double_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_NEGATIVE_DOUBLE_FLOAT, &symbol);
	double_float_heap(&value, -DBL_MAX);
	defconstant_symbol(symbol, value);
}


/* (defconstant most-negative-long-float ...) */
static void defconstant_most_negative_long_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_NEGATIVE_LONG_FLOAT, &symbol);
	long_float_heap(&value, -LDBL_MAX);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-short-float ...) */
static void defconstant_least_positive_short_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_SHORT_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_POSITIVE, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-normalized-short-float ...) */
static void defconstant_least_positive_normalized_short_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_POSITIVE_NORMALIZED, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-single-float ...) */
static void defconstant_least_positive_single_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_SINGLE_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_POSITIVE, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-normalized-single-float ...) */
static void defconstant_least_positive_normalized_single_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_POSITIVE_NORMALIZED, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-double-float ...) */
static void defconstant_least_positive_double_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_DOUBLE_FLOAT, &symbol);
	double_float_least_positive(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-normalized-double-float ...) */
static void defconstant_least_positive_normalized_double_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT, &symbol);
	double_float_least_positive_normalized(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-long-float ...) */
static void defconstant_least_positive_long_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_LONG_FLOAT, &symbol);
	long_float_least_positive(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-normalized-long-float ...) */
static void defconstant_least_positive_normalized_long_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_NORMALIZED_LONG_FLOAT, &symbol);
	long_float_least_positive_normalized(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-short-float ...) */
static void defconstant_least_negative_short_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_SHORT_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_NEGATIVE, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-normalized-short-float ...) */
static void defconstant_least_negative_normalized_short_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_NEGATIVE_NORMALIZED, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-single-float ...) */
static void defconstant_least_negative_single_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_SINGLE_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_NEGATIVE, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-normalized-single-float ...) */
static void defconstant_least_negative_normalized_single_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_NEGATIVE_NORMALIZED, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-double-float ...) */
static void defconstant_least_negative_double_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_DOUBLE_FLOAT, &symbol);
	double_float_least_negative(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-normalized-double-float ...) */
static void defconstant_least_negative_normalized_double_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT, &symbol);
	double_float_least_negative_normalized(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-long-float ...) */
static void defconstant_least_negative_long_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_LONG_FLOAT, &symbol);
	long_float_least_negative(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-normalized-long-float ...) */
static void defconstant_least_negative_normalized_long_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT, &symbol);
	long_float_least_negative_normalized(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant short-float-epsilon ...) */
static void defconstant_short_float_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_SHORT_FLOAT_EPSILON, &symbol);
	GetConst(SINGLE_FLOAT_EPSILON, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant short-float-negative-epsilon ...) */
static void defconstant_short_float_negative_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_SHORT_FLOAT_NEGATIVE_EPSILON, &symbol);
	GetConst(SINGLE_FLOAT_NEGATIVE_EPSILON, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant single-float-epsilon ...) */
static void defconstant_single_float_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_SINGLE_FLOAT_EPSILON, &symbol);
	GetConst(SINGLE_FLOAT_EPSILON, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant single-float-negative-epsilon ...) */
static void defconstant_single_float_negative_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_SINGLE_FLOAT_NEGATIVE_EPSILON, &symbol);
	GetConst(SINGLE_FLOAT_NEGATIVE_EPSILON, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant double-float-epsilon ...) */
static void defconstant_double_float_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_DOUBLE_FLOAT_EPSILON, &symbol);
	double_float_epsilon(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant double-float-negative-epsilon ...) */
static void defconstant_double_float_negative_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_DOUBLE_FLOAT_NEGATIVE_EPSILON, &symbol);
	double_float_negative_epsilon(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant long-float-epsilon ...) */
static void defconstant_long_float_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_LONG_FLOAT_EPSILON, &symbol);
	long_float_epsilon(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant long-float-negative-epsilon ...) */
static void defconstant_long_float_negative_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_LONG_FLOAT_NEGATIVE_EPSILON, &symbol);
	long_float_negative_epsilon(&value);
	defconstant_symbol(symbol, value);
}


/*
 *  function
 */
void init_common_numbers(void)
{
	SetPointerCall(defun, var1dynamic, number_equal);
	SetPointerCall(defun, var1dynamic, number_not_equal);
	SetPointerCall(defun, var1dynamic, number_less);
	SetPointerCall(defun, var1dynamic, number_greater);
	SetPointerCall(defun, var1dynamic, number_less_equal);
	SetPointerCall(defun, var1dynamic, number_greater_equal);
	SetPointerCall(defun, var1dynamic, max);
	SetPointerCall(defun, var1dynamic, min);
	SetPointerCall(defun, var1, minusp);
	SetPointerCall(defun, var1, plusp);
	SetPointerCall(defun, var1, zerop);
	SetPointerCall(defun, var1opt1, floor);
	SetPointerCall(defun, var1opt1, ffloor);
	SetPointerCall(defun, var1opt1, ceiling);
	SetPointerCall(defun, var1opt1, fceiling);
	SetPointerCall(defun, var1opt1, truncate);
	SetPointerCall(defun, var1opt1, ftruncate);
	SetPointerCall(defun, var1opt1, round);
	SetPointerCall(defun, var1opt1, fround);
	SetPointerCall(defun, var1, cis);
	SetPointerCall(defun, var1, sin);
	SetPointerCall(defun, var1, cos);
	SetPointerCall(defun, var1, tan);
	SetPointerCall(defun, var1, sinh);
	SetPointerCall(defun, var1, cosh);
	SetPointerCall(defun, var1, tanh);
	SetPointerCall(defun, var1, asin);
	SetPointerCall(defun, var1, acos);
	SetPointerCall(defun, var1opt1, atan);
	SetPointerCall(defun, var1, asinh);
	SetPointerCall(defun, var1, acosh);
	SetPointerCall(defun, var1, atanh);
	SetPointerCall(defun, var1, exp);
	SetPointerCall(defun, var2, expt);
	SetPointerCall(defun, dynamic, plus);
	SetPointerCall(defun, var1dynamic, minus);
	SetPointerCall(defun, dynamic, asterisk);
	SetPointerCall(defun, var1dynamic, slash);
	SetPointerCall(defun, var1, oneplus);
	SetPointerCall(defun, var1, oneminus);
	SetPointerCall(defun, var1, abs);
	SetPointerCall(defun, var1, evenp);
	SetPointerCall(defun, var1, oddp);
	SetPointerCall(defun, dynamic, gcd);
	SetPointerCall(defun, dynamic, lcm);
	SetPointerCall(defmacro, macro, incf);
	SetPointerCall(defmacro, macro, decf);
	SetPointerCall(defun, var1opt1, log);
	SetPointerCall(defun, var2, mod);
	SetPointerCall(defun, var2, rem);
	SetPointerCall(defun, var1, signum);
	SetPointerCall(defun, var1, sqrt);
	SetPointerCall(defun, var1, isqrt);
	SetPointerCall(defun, opt1, make_random_state);
	SetPointerCall(defun, var1opt1, random);
	SetPointerCall(defun, var1, random_state_p);
	SetPointerCall(defun, var1, numberp);
	SetPointerCall(defun, var1opt1, complex);
	SetPointerCall(defun, var1, complexp);
	SetPointerCall(defun, var1, conjugate);
	SetPointerCall(defun, var1, phase);
	SetPointerCall(defun, var1, realpart);
	SetPointerCall(defun, var1, imagpart);
	SetPointerCall(defun, var1opt1, upgraded_complex_part_type);
	SetPointerCall(defun, var1, realp);
	SetPointerCall(defun, var1, numerator);
	SetPointerCall(defun, var1, denominator);
	SetPointerCall(defun, var1, rational);
	SetPointerCall(defun, var1, rationalize);
	SetPointerCall(defun, var1, rationalp);
	SetPointerCall(defun, var2, ash);
	SetPointerCall(defun, var1, integer_length);
	SetPointerCall(defun, var1, integerp);
	SetPointerCall(defun, var1dynamic, parse_integer);
	SetPointerCall(defun, var3, boole);
	SetPointerCall(defun, dynamic, logand);
	SetPointerCall(defun, var2, logandc1);
	SetPointerCall(defun, var2, logandc2);
	SetPointerCall(defun, dynamic, logeqv);
	SetPointerCall(defun, dynamic, logior);
	SetPointerCall(defun, var2, lognand);
	SetPointerCall(defun, var2, lognor);
	SetPointerCall(defun, var1, lognot);
	SetPointerCall(defun, var2, logorc1);
	SetPointerCall(defun, var2, logorc2);
	SetPointerCall(defun, dynamic, logxor);
	SetPointerCall(defun, var2, logbitp);
	SetPointerCall(defun, var1, logcount);
	SetPointerCall(defun, var2, logtest);
	SetPointerCall(defun, var2, byte);
	SetPointerCall(defun, var1, byte_size);
	SetPointerCall(defun, var1, byte_position);
	SetPointerCall(defun, var3, deposit_field);
	SetPointerCall(defun, var3, dpb);
	SetPointerCall(defun, var2, ldb);
	SetPointerCall(defmacro, macro, setf_ldb);
	SetPointerCall(defun, var2, ldb_test);
	SetPointerCall(defun, var2, mask_field);
	SetPointerCall(defmacro, macro, setf_mask_field);
	SetPointerCall(defun, var1, decode_float);
	SetPointerCall(defun, var2, scale_float);
	SetPointerCall(defun, var1, float_radix);
	SetPointerCall(defun, var1opt1, float_sign);
	SetPointerCall(defun, var1, float_digits);
	SetPointerCall(defun, var1, float_precision);
	SetPointerCall(defun, var1, integer_decode_float);
	SetPointerCall(defun, var1opt1, float);
	SetPointerCall(defun, var1, floatp);
	SetPointerCall(defun, var1, arithmetic_error_operands);
	SetPointerCall(defun, var1, arithmetic_error_operation);
}

void build_common_numbers(void)
{
	defun_number_equal();
	defun_number_not_equal();
	defun_number_less();
	defun_number_greater();
	defun_number_less_equal();
	defun_number_greater_equal();
	defun_max();
	defun_min();
	defun_minusp();
	defun_plusp();
	defun_zerop();
	defun_floor();
	defun_ffloor();
	defun_ceiling();
	defun_fceiling();
	defun_truncate();
	defun_ftruncate();
	defun_round();
	defun_fround();
	defun_cis();
	defun_sin();
	defun_cos();
	defun_tan();
	defun_cosh();
	defun_tanh();
	defun_asinh();
	defun_asin();
	defun_acos();
	defun_atan();
	defun_sinh();
	defun_acosh();
	defun_atanh();
	defun_exp();
	defun_expt();
	defun_plus();
	defun_minus();
	defun_asterisk();
	defun_slash();
	defun_oneplus();
	defun_oneminus();
	defun_abs();
	defun_evenp();
	defun_oddp();
	defun_gcd();
	defun_lcm();
	defmacro_incf();
	defmacro_decf();
	defun_log();
	defun_mod();
	defun_rem();
	defun_signum();
	defun_sqrt();
	defun_isqrt();
	defun_make_random_state();
	defun_random();
	defun_random_state_p();
	defvar_random_state();
	defun_numberp();
	defun_complex();
	defun_complexp();
	defun_conjugate();
	defun_phase();
	defun_realpart();
	defun_imagpart();
	defun_upgraded_complex_part_type();
	defun_realp();
	defun_numerator();
	defun_denominator();
	defun_rational();
	defun_rationalize();
	defun_rationalp();
	defun_ash();
	defun_integer_length();
	defun_integerp();
	defun_parse_integer();
	defun_boole();
	defun_logand();
	defun_logandc1();
	defun_logandc2();
	defun_logeqv();
	defun_logior();
	defun_lognand();
	defun_lognor();
	defun_lognot();
	defun_logorc1();
	defun_logorc2();
	defun_logxor();
	defun_logbitp();
	defun_logcount();
	defun_logtest();
	defun_byte();
	defun_byte_size();
	defun_byte_position();
	defun_deposit_field();
	defun_dpb();
	defun_ldb();
	define_setf_expander_ldb();
	defun_ldb_test();
	defun_mask_field();
	define_setf_expander_mask_field();
	defun_decode_float();
	defun_scale_float();
	defun_float_radix();
	defun_float_sign();
	defun_float_digits();
	defun_float_precision();
	defun_integer_decode_float();
	defun_float();
	defun_floatp();
	defun_arithmetic_error_operands();
	defun_arithmetic_error_operation();

	defconstant_pi();
	defconstant_boole_1();
	defconstant_boole_2();
	defconstant_boole_and();
	defconstant_boole_andc1();
	defconstant_boole_andc2();
	defconstant_boole_c1();
	defconstant_boole_c2();
	defconstant_boole_clr();
	defconstant_boole_eqv();
	defconstant_boole_ior();
	defconstant_boole_nand();
	defconstant_boole_nor();
	defconstant_boole_orc1();
	defconstant_boole_orc2();
	defconstant_boole_set();
	defconstant_boole_xor();
	defconstant_most_positive_fixnum();
	defconstant_most_negative_fixnum();
	defconstant_most_positive_short_float();
	defconstant_most_positive_single_float();
	defconstant_most_positive_double_float();
	defconstant_most_positive_long_float();
	defconstant_most_negative_short_float();
	defconstant_most_negative_single_float();
	defconstant_most_negative_double_float();
	defconstant_most_negative_long_float();
	defconstant_least_positive_short_float();
	defconstant_least_positive_normalized_short_float();
	defconstant_least_positive_single_float();
	defconstant_least_positive_normalized_single_float();
	defconstant_least_positive_double_float();
	defconstant_least_positive_normalized_double_float();
	defconstant_least_positive_long_float();
	defconstant_least_positive_normalized_long_float();
	defconstant_least_negative_short_float();
	defconstant_least_negative_normalized_short_float();
	defconstant_least_negative_single_float();
	defconstant_least_negative_normalized_single_float();
	defconstant_least_negative_double_float();
	defconstant_least_negative_normalized_double_float();
	defconstant_least_negative_long_float();
	defconstant_least_negative_normalized_long_float();
	defconstant_short_float_epsilon();
	defconstant_short_float_negative_epsilon();
	defconstant_single_float_epsilon();
	defconstant_single_float_negative_epsilon();
	defconstant_double_float_epsilon();
	defconstant_double_float_negative_epsilon();
	defconstant_long_float_epsilon();
	defconstant_long_float_negative_epsilon();
}


/************************************************************
 *  common_objects.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 7. Objects
 */

/*
 *  import
 */
#define ImportMopPackage(x) Error(import_mop_package_(CONSTANT_COMMON_##x))

static int import_mop_package_(constindex index)
{
	addr package, symbol;

	GetConst(PACKAGE_COMMON_LISP, &package);
	GetConstant(index, &symbol);
	CheckType(symbol, LISPTYPE_SYMBOL);
	Return(import_package_(package, symbol));
	Return(export_package_(package, symbol));

	return 0;
}


/* (defgeneric function-keywords (method)) */
static void defgeneric_function_keywords(void)
{
	ImportMopPackage(FUNCTION_KEYWORDS);
}


/* (defun ensure-generic-function (name &key
 *     argument-precedence-order
 *     declare
 *     documentation
 *     environment
 *     lambda-list
 *     generic-function-class
 *     method-class
 *     method-combination
 */
static int function_ensure_generic_function(Execute ptr, addr name, addr rest)
{
	Return(ensure_generic_function_common_(ptr, name, rest, &name));
	setresult_control(ptr, name);
	return 0;
}

static void type_ensure_generic_function(addr *ret)
{
	addr args, values;
	addr key, key1, key2, key3, key4, key5, key6, key7, key8;

	KeyTypeTable(&key1, ARGUMENT_PRECEDENCE_ORDER, T);
	KeyTypeTable(&key2, DECLARE, T);
	KeyTypeTable(&key3, DOCUMENTATION, String);
	KeyTypeTable(&key4, ENVIRONMENT, EnvironmentNull);
	KeyTypeTable(&key5, LAMBDA_LIST, T);
	KeyTypeTable(&key6, GENERIC_FUNCTION_CLASS, T);
	KeyTypeTable(&key7, METHOD_CLASS, Class);
	KeyTypeTable(&key8, METHOD_COMBINATION, MethodCombination);
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, key8, NULL);
	/* type */
	GetTypeTable(&args, FunctionName);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_generic_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ENSURE_GENERIC_FUNCTION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_ensure_generic_function);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_ensure_generic_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
	ImportMopPackage(ENSURE_GENERIC_FUNCTION);
}


/* (defgeneric allocate-instance ...)  */
static void defgeneric_allocate_instance(void)
{
	ImportMopPackage(ALLOCATE_INSTANCE);
}


/* (defgeneric reinitialize-instance ...) */
static void defgeneric_reinitialize_instance(void)
{
	ImportMopPackage(REINITIALIZE_INSTANCE);
}


/* (defgeneric shared-initialize ...) */
static void defgeneric_shared_initialize(void)
{
	ImportMopPackage(SHARED_INITIALIZE);
}


/* defgeneric_update_instance_for_different_class(); */
static void defgeneric_update_instance_for_different_class(void)
{
	ImportMopPackage(UPDATE_INSTANCE_FOR_DIFFERENT_CLASS);
}


/* defgeneric_update_instance_for_redefined_class(); */
static void defgeneric_update_instance_for_redefined_class(void)
{
	ImportMopPackage(UPDATE_INSTANCE_FOR_REDEFINED_CLASS);
}


/* (defgeneric change-class ...) */
static void defgeneric_change_class(void)
{
	ImportMopPackage(CHANGE_CLASS);
}


/* (defun slot-boundp (object symbol) ...) -> t */
static int function_slot_boundp(Execute ptr, addr pos, addr name)
{
	addr call, clos;

	/* call slot-boundp-using-class */
	Return(clos_class_of_(pos, &clos));
	GetConst(CLOSNAME_SLOT_BOUNDP_USING_CLASS, &call);
	Return(getfunction_global_(call, &call));
	return funcall_control_(ptr, call, clos, pos, name, NULL);
}

static void defun_slot_boundp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLOT_BOUNDP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_slot_boundp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SlotBoundp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun slot-exists-p (object symbol) ...) -> t */
static int function_slot_exists_p(Execute ptr, addr pos, addr name)
{
	addr call, clos;

	/* call slot-exists-p-using-class */
	Return(clos_class_of_(pos, &clos));
	GetConst(CLOSNAME_SLOT_EXISTS_P_USING_CLASS, &call);
	Return(getfunction_global_(call, &call));
	return funcall_control_(ptr, call, clos, pos, name, NULL);
}

static void defun_slot_exists_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLOT_EXISTS_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_slot_exists_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SlotBoundp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun slot-makunbound (instance symbol) ...) -> t */
static int function_slot_makunbound(Execute ptr, addr pos, addr name)
{
	addr call, clos;

	/* call slot-makunbound-using-class */
	Return(clos_class_of_(pos, &clos));
	GetConst(CLOSNAME_SLOT_MAKUNBOUND_USING_CLASS, &call);
	Return(getfunction_global_(call, &call));
	return funcall_control_(ptr, call, clos, pos, name, NULL);
}

static void defun_slot_makunbound(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLOT_MAKUNBOUND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_slot_makunbound);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SlotBoundp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defgeneric slot-missing ...) */
static void defgeneric_slot_missing(void)
{
	ImportMopPackage(SLOT_MISSING);
}


/* (defgeneric slot-unbound ...) */
static void defgeneric_slot_unbound(void)
{
	ImportMopPackage(SLOT_UNBOUND);
}


/* (defun slot-value (object name) ...) -> t */
static int function_slot_value(Execute ptr, addr pos, addr name)
{
	addr call, clos;

	/* call slot-value-using-class */
	Return(clos_class_of_(pos, &clos));
	GetConst(CLOSNAME_SLOT_VALUE_USING_CLASS, &call);
	Return(getfunction_global_(call, &call));
	return funcall_control_(ptr, call, clos, pos, name, NULL);
}

static void type_slot_value(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Clos);
	GetTypeTable(&values, Symbol);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_slot_value(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLOT_VALUE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_slot_value);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_slot_value(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf slot-value) (value pos name) ...) -> value */
static int function_setf_slot_value(Execute ptr, addr value, addr pos, addr name)
{
	addr call, clos;

	/* call (setf slot-value-using-class) */
	Return(clos_class_of_(pos, &clos));
	GetConst(CLOSNAME_SLOT_VALUE_USING_CLASS, &call);
	Return(getsetf_global_(call, &call));
	return funcall_control_(ptr, call, value, clos, pos, name, NULL);
}

static void type_setf_slot_value(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Clos);
	GetTypeTable(&type, Symbol);
	typeargs_var3(&args, args, values, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_slot_value(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLOT_VALUE, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_setf_slot_value);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_slot_value(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defgeneric method-qualifiers ...) */
static void defgeneric_method_qualifiers_import(void)
{
	ImportMopPackage(METHOD_QUALIFIERS);
}


/* (defgeneric no-applicable-method ...) */
static void defgeneric_no_applicable_method(void)
{
	ImportMopPackage(NO_APPLICABLE_METHOD);
}


/* (defgeneric no-next-method ...) */
static void defgeneric_no_next_method(void)
{
	ImportMopPackage(NO_NEXT_METHOD);
}


/* (defgeneric remove-method ...) */
static void defgeneric_remove_method(void)
{
	ImportMopPackage(REMOVE_METHOD);
}


/* (defgeneric make-instance ...) */
static void defgeneric_make_instance(void)
{
	ImportMopPackage(MAKE_INSTANCE);
}


/* (defgeneric make-instances-obsolete ...) */
static void defgeneric_make_instances_obsolete(void)
{
	ImportMopPackage(MAKE_INSTANCES_OBSOLETE);
}


/* (defgeneric make-load-form ...) */
static void defgeneric_make_load_form_common(void)
{
	ImportMopPackage(MAKE_LOAD_FORM);
}


/* (defun make-load-form-saving-slots
 *     (object &key slot-names environment) ...)
 *     -> creation-form, initialization-form
 *   object               T
 *   slot-names           list
 *   environment          Environment
 *   creation-form        T
 *   initialization-form  T
 */
static int function_make_load_form_saving_slots(Execute ptr, addr var, addr rest)
{
	addr list, env;

	if (GetKeyArgs(rest, KEYWORD_SLOT_NAMES, &list))
		list = Unbound;
	if (GetKeyArgs(rest, KEYWORD_ENVIRONMENT, &env))
		env = Nil;
	Return(make_load_form_saving_slots_common_(ptr, var, list, env, &var, &list));
	setvalues_control(ptr, var, list, NULL);

	return 0;
}

static void type_make_load_form_saving_slots(addr *ret)
{
	addr args, values;
	addr key, key1, key2;

	KeyTypeTable(&key1, SLOT_NAMES, List);
	KeyTypeTable(&key2, ENVIRONMENT, EnvironmentNull);
	list_heap(&key, key1, key2, NULL);
	/* type */
	GetTypeTable(&values, T);
	typeargs_var1key(&args, values, key);
	typevalues_values2(&values, values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_load_form_saving_slots(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_LOAD_FORM_SAVING_SLOTS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_make_load_form_saving_slots);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_load_form_saving_slots(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-accessors ((entry*) instance declare* &body body) ...) -> t */
static int function_with_accessors(Execute ptr, addr form, addr env)
{
	Return(with_accessors_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_accessors(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_ACCESSORS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_accessors);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro with-slots ((entry*) instance declare* &body form) ...) -> t */
static int function_with_slots(Execute ptr, addr form, addr env)
{
	Return(with_slots_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_slots(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_SLOTS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_slots);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defclass (name ({superclass}*) ({slot}*) &rest option) -> class
 *    name        (or symbol (not null))  ;; nonnil-symbol
 *    superclass  (or symbol (not null))  ;; nonnil-symbol
 *    slot        (or symbol cons)
 *    option      &rest cons
 *    class       class
 */
static int function_defclass(Execute ptr, addr form, addr env)
{
	Return(defclass_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defclass(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFCLASS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defclass);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defgeneric (name lambda &rest args) ...) -> generic-function */
static int function_defgeneric(Execute ptr, addr form, addr env)
{
	Return(defgeneric_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defgeneric(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFGENERIC, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defgeneric);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defmethod
 *     (name qualifier* lambda declare* document* form*) -> method
 */
static int function_defmethod(Execute ptr, addr form, addr env)
{
	Return(defmethod_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defmethod(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFMETHOD, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defmethod);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun find-class (symbol &optional errorp env) ...) -> class
 *   symbol  symbol
 *   errorp  t  ;; boolean, default t
 *   env     (or environment null)  ;; default nil
 *   class   (or class null)
 */
static int function_find_class(Execute ptr, addr pos, addr errorp, addr env)
{
	if (errorp == Unbound)
		errorp = T;
	if (env == Unbound)
		env = Nil;
	Return(find_class_common_(pos, errorp != Nil, env, &pos));
	setresult_control(ptr, pos);

	return 0;
}

static void type_find_class(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	GetTypeTable(&type, EnvironmentNull);
	typeargs_var1opt2(&args, args, values, type);
	GetTypeValues(&values, ClassNull);
	type_compiled_heap(args, values, ret);
}

static void defun_find_class(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_CLASS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt2(pos, p_defun_find_class);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_find_class(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf find-class) (class symbol &optional errorp env) ...) -> class
 *   class   (or null class)
 *   symbol  symbol
 *   errorp  t
 *   env     (or null environment)
 */
static int function_setf_find_class(Execute ptr,
		addr clos, addr name, addr errorp, addr env)
{
	/* (declare (ignore errorp)) */
	if (env == Unbound)
		env = Nil;
	setf_find_class_common(clos, name, env);
	setresult_control(ptr, clos);

	return 0;
}

static void type_setf_find_class(addr *ret)
{
	addr args, values, type1, type2, class_null;

	GetTypeTable(&type1, Class);
	GetTypeTable(&type2, Null);
	type2or_heap(type1, type2, &class_null);
	GetTypeTable(&values, Symbol);
	GetTypeTable(&type1, T);
	GetTypeTable(&type2, EnvironmentNull);
	typeargs_var2opt2(&args, class_null, values, type1, type2);
	typevalues_result(&values, class_null);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_find_class(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_CLASS, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2opt2(pos, p_defun_setf_find_class);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_find_class(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defgeneric compute-applicable-methods ...) */
static void defgeneric_compute_applicable_methods(void)
{
	ImportMopPackage(COMPUTE_APPLICABLE_METHODS);
}


/* defmacro_define_method_combination(); */
static int function_define_method_combination(Execute ptr, addr form, addr env)
{
	Return(define_method_combination_common_(ptr->local, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_define_method_combination(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_METHOD_COMBINATION, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_define_method_combination);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defgeneric find-method ...) */
static void defgeneric_find_method(void)
{
	ImportMopPackage(FIND_METHOD);
}


/* (defgeneric add-method ...) */
static void defgeneric_add_method(void)
{
	ImportMopPackage(ADD_METHOD);
}


/* (defgeneric initialize-instance ...) */
static void defgeneric_initialize_instance(void)
{
	ImportMopPackage(INITIALIZE_INSTANCE);
}


/* (defgeneric class-name ...) */
static void defgeneric_class_name_import(void)
{
	ImportMopPackage(CLASS_NAME);
}


/* (defgeneric (setf class-name) ...) */
static void defgeneric_setf_class_name_import(void)
{
	/* do-nothing */
}


/* (defun class-of (object) ...) -> class */
static int function_class_of(Execute ptr, addr pos)
{
	Return(clos_class_of_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_class_of(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Class);
	type_compiled_heap(args, values, ret);
}

static void defun_class_of(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CLASS_OF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_class_of);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_class_of(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun unbound-slot-instance ...) */
static int function_unbound_slot_instance(Execute ptr, addr var)
{
	Return(unbound_slot_instance_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_unbound_slot_instance(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Condition);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_unbound_slot_instance(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNBOUND_SLOT_INSTANCE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_unbound_slot_instance);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_unbound_slot_instance(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
static void init_clos_objects(void)
{
	SetPointerCall(defun, var1dynamic, ensure_generic_function);
	SetPointerCall(defun, var2, slot_boundp);
	SetPointerCall(defun, var2, slot_exists_p);
	SetPointerCall(defun, var2, slot_makunbound);
	SetPointerCall(defun, var2, slot_value);
	SetPointerCall(defun, var3, setf_slot_value);
	SetPointerCall(defun, var1dynamic, make_load_form_saving_slots);
	SetPointerCall(defmacro, macro, with_accessors);
	SetPointerCall(defmacro, macro, with_slots);
	SetPointerCall(defmacro, macro, defclass);
	SetPointerCall(defmacro, macro, defgeneric);
	SetPointerCall(defmacro, macro, defmethod);
	SetPointerCall(defun, var1opt2, find_class);
	SetPointerCall(defun, var2opt2, setf_find_class);
	SetPointerCall(defmacro, macro, define_method_combination);
	SetPointerCall(defun, var1, class_of);
	SetPointerCall(defun, var1, unbound_slot_instance);
}

static void build_clos_objects(void)
{
	defgeneric_function_keywords();
	defun_ensure_generic_function();
	defgeneric_allocate_instance();
	defgeneric_reinitialize_instance();
	defgeneric_shared_initialize();
	defgeneric_update_instance_for_different_class();
	defgeneric_update_instance_for_redefined_class();
	defgeneric_change_class();
	defun_slot_boundp();
	defun_slot_exists_p();
	defun_slot_makunbound();
	defgeneric_slot_missing();
	defgeneric_slot_unbound();
	defun_slot_value();
	defun_setf_slot_value();
	defgeneric_method_qualifiers_import();
	defgeneric_no_applicable_method();
	defgeneric_no_next_method();
	defgeneric_remove_method();
	defgeneric_make_instance();
	defgeneric_make_instances_obsolete();
	defgeneric_make_load_form_common();
	defun_make_load_form_saving_slots();
	defmacro_with_accessors();
	defmacro_with_slots();
	defmacro_defclass();
	defmacro_defgeneric();
	defmacro_defmethod();
	defun_find_class();
	defun_setf_find_class();
	defgeneric_compute_applicable_methods();
	defmacro_define_method_combination();
	defgeneric_find_method();
	defgeneric_add_method();
	defgeneric_initialize_instance();
	defgeneric_class_name_import();
	defgeneric_setf_class_name_import();
	defun_class_of();
	defun_unbound_slot_instance();
}

void init_common_objects(void)
{
	/* metaobject protocol */
	init_metaobject_protocol();
	/* common-lisp objects */
	init_clos_objects();
}

void build_common_objects(void)
{
	/* metaobject protocol */
	build_metaobject_protocol();
	/* common-lisp objects */
	build_clos_objects();
}


/************************************************************
 *  common_packages.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 11. Packages
 */

/* (defun export (symbols &optional package) ...) -> (eql t)
 *   symbols  (or list symbol)
 *   package  (or string character symbol package) ;; package-designator
 */
static int function_export(Execute ptr, addr symbols, addr package)
{
	Return(export_common_(ptr, symbols, package));
	setresult_control(ptr, T);
	return 0;
}

static void defun_export(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EXPORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_export);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Export);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun find-symbol (string &optional package) ...) -> symbol, status
 *   package  package-designator
 *   status   (member :inherited :external :interal nil)
 */
static int function_find_symbol(Execute ptr, addr name, addr package)
{
	Return(find_symbol_common_(ptr, name, package, &name, &package));
	setvalues_control(ptr, name, package, NULL);
	return 0;
}

static void defun_find_symbol(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_SYMBOL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_find_symbol);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intern);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun find-package (name) ...) -> package */
static int function_find_package(Execute ptr, addr name)
{
	Return(find_package_(name, &name));
	setresult_control(ptr, name);
	return 0;
}

static void type_find_package(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringDesignator);
	GetTypeTable(&values, Package);
	type2or_heap(args, values, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, PackageNull);
	type_compiled_heap(args, values, ret);
}

static void defun_find_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_find_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_find_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun find-all-symbols (string) ...) -> symbols
 *   string   string-designator
 *   symbols  list
 */
static int function_find_all_symbols(Execute ptr, addr name)
{
	Return(find_allsymbols_package_(name, &name));
	setresult_control(ptr, name);
	return 0;
}

static void type_find_all_symbols(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringDesignator);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_find_all_symbols(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_ALL_SYMBOLS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_find_all_symbols);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_find_all_symbols(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun import (symbols &optional package) ...) -> (eql t)
 *   symbols  (or list symbol)
 *   package  package-designator
 */
static int function_import(Execute ptr, addr symbols, addr package)
{
	Return(import_common_(ptr, symbols, package));
	setresult_control(ptr, T);
	return 0;
}

static void defun_import(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_IMPORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_import);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Export);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun list-all-packages () ...) -> list */
static int function_list_all_packages(Execute ptr)
{
	addr list;

	Return(list_all_packages_(&list));
	setresult_control(ptr, list);

	return 0;
}

static void type_list_all_packages(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, Empty);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_list_all_packages(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LIST_ALL_PACKAGES, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_list_all_packages);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_list_all_packages(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rename-package (package name &optional nicknames) ...) -> value
 *   package    package-designator
 *   name       package-designator
 *   nicknames  list
 *   value      package
 */
static int function_rename_package(Execute ptr,
		addr package, addr name, addr nicknames)
{
	Return(rename_package_common_(ptr, package, name, nicknames, &package));
	setresult_control(ptr, package);
	return 0;
}

static void type_rename_package(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PackageDesignator);
	GetTypeTable(&values, List);
	typeargs_var2opt1(&args, args, args, values);
	GetTypeValues(&values, Package);
	type_compiled_heap(args, values, ret);
}

static void defun_rename_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RENAME_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_rename_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_rename_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun shadow (symbols &optional package) ...) -> (eql t)
 *   symbols  (or list string-designator)
 *   package  package-designator
 */
static int function_shadow(Execute ptr, addr symbols, addr package)
{
	Return(shadow_common_(ptr, symbols, package));
	setresult_control(ptr, T);
	return 0;
}

static void type_shadow(addr *ret)
{
	addr args, values, type1, type2;

	GetTypeTable(&type1, List);
	GetTypeTable(&type2, StringDesignator);
	type2or_heap(type1, type2, &args);
	GetTypeTable(&values, PackageDesignator);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(args, values, ret);
}

static void defun_shadow(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SHADOW, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_shadow);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_shadow(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun shadowing-import (symbols &optional package) ...) -> (eql t)
 *   symbols  (or list symbol)
 *   package  package-designator
 */
static int function_shadowing_import(Execute ptr, addr symbols, addr package)
{
	Return(shadowing_import_common_(ptr, symbols, package));
	setresult_control(ptr, T);
	return 0;
}

static void defun_shadowing_import(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SHADOWING_IMPORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_shadowing_import);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Export);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun delete-package (package) ...) -> booelan
 *   package  package-designator
 */
static int function_delete_package(Execute ptr, addr package)
{
	int check;

	Return(delete_package_(package, &check));
	setbool_control(ptr, ! check);

	return 0;
}

static void type_delete_package(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, PackageDesignator);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_delete_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DELETE_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_delete_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_delete_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-package (name &key nicknames use) ...) -> package
 *   name       string-designator
 *   nicknames  list
 *   use        list
 *   package    package
 */
static int function_make_package(Execute ptr, addr name, addr rest)
{
	Return(make_package_common_(ptr, name, rest, &name));
	setresult_control(ptr, name);
	return 0;
}

static void type_make_package(addr *ret)
{
	addr args, values, type1, type2, symbol, type, key;

	/* args */
	GetTypeTable(&args, StringDesignator);
	GetTypeTable(&type, List);
	GetConst(KEYWORD_NICKNAMES, &symbol);
	cons_heap(&type1, symbol, type);
	GetConst(KEYWORD_USE, &symbol);
	cons_heap(&type2, symbol, type);
	list_heap(&key, type1, type2, NULL);
	typeargs_var1key(&args, args, key);
	/* values */
	GetTypeValues(&values, Package);
	type_compiled_heap(args, values, ret);
}

static void defun_make_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_make_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-package-iterator ((name list &rest types) &body body) ...) */
static int function_with_package_iterator(Execute ptr, addr form, addr env)
{
	Return(with_package_iterator_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_package_iterator(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_PACKAGE_ITERATOR, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_package_iterator);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun unexport (symbols &optional package) ...) -> (eql t)
 *   symbols  (or list symbol)
 *   package  (or string character symbol package) ;; package-designator
 */
static int function_unexport(Execute ptr, addr symbols, addr package)
{
	Return(unexport_common_(ptr, symbols, package));
	setresult_control(ptr, T);
	return 0;
}

static void defun_unexport(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNEXPORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_unexport);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Export);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun unintern (symbol &optional package) ...) -> boolean
 *   package  package-designator
 */
static int function_unintern(Execute ptr, addr symbol, addr package)
{
	Return(unintern_common_(ptr, symbol, package, &package));
	setresult_control(ptr, package);
	return 0;
}

static void type_unintern(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, PackageDesignator);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_unintern(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNINTERN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_unintern);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_unintern(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro in-package (name) ...) */
static int function_in_package(Execute ptr, addr form, addr env)
{
	Return(in_package_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_in_package(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_IN_PACKAGE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_in_package);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun unuse-package (list &optional package) ...) -> t
 *    list     (or package-designator list)
 *    package  package-designator
 */
static int function_unuse_package(Execute ptr, addr unuse, addr package)
{
	Return(unuse_package_common_(ptr, unuse, package));
	setresult_control(ptr, T);
	return 0;
}

static void defun_unuse_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNUSE_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_unuse_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, UsePackage);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun use-package (list &optional package) ...) -> t
 *    list     (or package-designator list)
 *    package  package-designator
 */
static int function_use_package(Execute ptr, addr use, addr package)
{
	Return(use_package_common_(ptr, use, package));
	setresult_control(ptr, T);
	return 0;
}

static void defun_use_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_USE_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_use_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, UsePackage);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro defpackage (name &rest options) ...) -> package
 *   options ::= (:nicknames nickname*)* |
 *               (:documentation string) |
 *               (:use package-name*)* |
 *               (:shadow {symbol-name}*)* |
 *               (:shadowing-import-from package-name {symbol-name}*)* |
 *               (:import-from package-name {symbol-name}*)* |
 *               (:export {symbol-name}*)* |
 *               (:intern {symbol-name}*)* |
 *               (:size integer)
 */
static int function_defpackage(Execute ptr, addr form, addr env)
{
	Return(defpackage_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defpackage(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFPACKAGE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defpackage);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro do-symbols (var &optional package result) . tagbody) */
static int function_do_symbols(Execute ptr, addr form, addr env)
{
	Return(do_symbols_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_do_symbols(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DO_SYMBOLS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_do_symbols);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro do-external-symbols (var &optional package result) . tagbody) */
static int function_do_external_symbols(Execute ptr, addr form, addr env)
{
	Return(do_external_symbols_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_do_external_symbols(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DO_EXTERNAL_SYMBOLS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_do_external_symbols);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro do-all-symbols (var &optional result) . tagbody) */
static int function_do_all_symbols(Execute ptr, addr form, addr env)
{
	Return(do_all_symbols_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_do_all_symbols(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DO_ALL_SYMBOLS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_do_all_symbols);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun intern (string &optional package) ...) -> symbol, status
 *   package  package-designator
 *   status   (member :inherited :external :interal nil)
 */
static int function_intern(Execute ptr, addr name, addr package)
{
	Return(intern_common_(ptr, name, package, &name, &package));
	setvalues_control(ptr, name, package, NULL);
	return 0;
}

static void defun_intern(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INTERN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_intern);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intern);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-name (package) ...) -> name
 *   package  package-designator
 *   name     (or string null)
 */
static int function_package_name(Execute ptr, addr package)
{
	Return(getname_package_(package, &package));
	setresult_control(ptr, package);
	return 0;
}

static void type_package_name(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, PackageDesignator);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, ret);
}

static void defun_package_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_NAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_package_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-nicknames (package) ...) -> list */
static int function_package_nicknames(Execute ptr, addr package)
{
	Return(getnickname_package_(package, &package));
	setresult_control(ptr, package);
	return 0;
}

static void defun_package_nicknames(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_NICKNAMES, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_nicknames);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PackageNicknames);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-shadowing-symbols (package) ...) -> list */
static int function_package_shadowing_symbols(Execute ptr, addr package)
{
	Return(getshadow_package_(package, &package));
	setresult_control(ptr, package);
	return 0;
}

static void defun_package_shadowing_symbols(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_SHADOWING_SYMBOLS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_shadowing_symbols);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PackageNicknames);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-use-list (package) ...) -> list */
static int function_package_use_list(Execute ptr, addr package)
{
	Return(getuselist_package_(package, &package));
	setresult_control(ptr, package);
	return 0;
}

static void defun_package_use_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_USE_LIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_use_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PackageNicknames);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-used-by-list (package) ...) -> list */
static int function_package_used_by_list(Execute ptr, addr package)
{
	Return(getusedbylist_package_(package, &package));
	setresult_control(ptr, package);
	return 0;
}

static void defun_package_used_by_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_USED_BY_LIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_used_by_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PackageNicknames);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun packagep (object) ...) -> boolean */
static int function_packagep(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_PACKAGE);
	return 0;
}

static void defun_packagep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGEP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_packagep);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defvar *package*) */
static void defvar_package(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PACKAGE, &symbol);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, Package);
	settype_value_symbol(symbol, type);
}


/* (defun package-error-package (package-error) ...) -> package-designator */
static int function_package_error_package(Execute ptr, addr var)
{
	Return(package_error_package_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_package_error_package(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PackageError);
	typeargs_var1(&args, args);
	GetTypeTable(&values, PackageDesignator);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_package_error_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_ERROR_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_error_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_package_error_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_packages(void)
{
	SetPointerCall(defun, var1opt1, export);
	SetPointerCall(defun, var1opt1, find_symbol);
	SetPointerCall(defun, var1, find_package);
	SetPointerCall(defun, var1, find_all_symbols);
	SetPointerCall(defun, var1opt1, import);
	SetPointerCall(defun, empty, list_all_packages);
	SetPointerCall(defun, var2opt1, rename_package);
	SetPointerCall(defun, var1opt1, shadow);
	SetPointerCall(defun, var1opt1, shadowing_import);
	SetPointerCall(defun, var1, delete_package);
	SetPointerCall(defun, var1dynamic, make_package);
	SetPointerCall(defmacro, macro, with_package_iterator);
	SetPointerCall(defun, var1opt1, unexport);
	SetPointerCall(defun, var1opt1, unintern);
	SetPointerCall(defmacro, macro, in_package);
	SetPointerCall(defun, var1opt1, unuse_package);
	SetPointerCall(defun, var1opt1, use_package);
	SetPointerCall(defmacro, macro, defpackage);
	SetPointerCall(defmacro, macro, do_symbols);
	SetPointerCall(defmacro, macro, do_external_symbols);
	SetPointerCall(defmacro, macro, do_all_symbols);
	SetPointerCall(defun, var1opt1, intern);
	SetPointerCall(defun, var1, package_name);
	SetPointerCall(defun, var1, package_nicknames);
	SetPointerCall(defun, var1, package_shadowing_symbols);
	SetPointerCall(defun, var1, package_use_list);
	SetPointerCall(defun, var1, package_used_by_list);
	SetPointerCall(defun, var1, packagep);
	SetPointerCall(defun, var1, package_error_package);
}

void build_common_packages(void)
{
	defun_export();
	defun_find_symbol();
	defun_find_package();
	defun_find_all_symbols();
	defun_import();
	defun_list_all_packages();
	defun_rename_package();
	defun_shadow();
	defun_shadowing_import();
	defun_delete_package();
	defun_make_package();
	defmacro_with_package_iterator();
	defun_unexport();
	defun_unintern();
	defmacro_in_package();
	defun_unuse_package();
	defun_use_package();
	defmacro_defpackage();
	defmacro_do_symbols();
	defmacro_do_external_symbols();
	defmacro_do_all_symbols();
	defun_intern();
	defun_package_name();
	defun_package_nicknames();
	defun_package_shadowing_symbols();
	defun_package_use_list();
	defun_package_used_by_list();
	defun_packagep();
	defvar_package();
	defun_package_error_package();
}


/************************************************************
 *  common_printer.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 22. Printer
 */

/* (defun copy-pprint-dispatch (&optional table) ...) -> new-table */
static int function_copy_pprint_dispatch(Execute ptr, addr var)
{
	Return(copy_pprint_dispatch_common_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_copy_pprint_dispatch(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrintDispatchNull);
	typeargs_opt1(&args, args);
	GetTypeTable(&values, PrintDispatch);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_copy_pprint_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_PPRINT_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_copy_pprint_dispatch);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_copy_pprint_dispatch(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro formatter (string) ...) -> function */
static int function_formatter(Execute ptr, addr var, addr env)
{
	Return(formatter_common_(ptr->local, var, env, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defmacro_formatter(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_FORMATTER, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_formatter);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun pprint-dispatch (object &optional table) ...) -> function, boolean */
static int function_pprint_dispatch(Execute ptr, addr var, addr table)
{
	Return(pprint_dispatch_common_(ptr, var, table, &var, &table));
	setvalues_control(ptr, var, table, NULL);
	return 0;
}

static void type_pprint_dispatch(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&values, PrintDispatchNull);
	typeargs_var1opt1(&args, args, values);
	GetTypeTable(&values, FunctionDesignator);
	GetTypeTable(&type, Boolean);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_pprint_dispatch);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pprint_dispatch(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-fill (stream object &optional colon-p at-sign-p) ...) -> null
 *   stream     (or boolean stream)  ;; output-stream-designator
 *   object     t
 *   colon-p    t  ;; boolean
 *   at-sign-p  t  ;; boolean
 */
static int function_pprint_fill(Execute ptr,
		addr stream, addr pos, addr colon, addr atsign)
{
	Return(pprint_fill_common_(ptr, stream, pos, colon));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_pprint_fill(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT_FILL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt2(pos, p_defun_pprint_fill);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PprintFill);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-linear (stream object &optional colon-p at-sign-p) ...) -> null
 *   stream     (or boolean stream)  ;; output-stream-designator
 *   object     t
 *   colon-p    t  ;; boolean
 *   at-sign-p  t  ;; boolean
 */
static int function_pprint_linear(Execute ptr,
		addr stream, addr pos, addr colon, addr atsign)
{
	Return(pprint_linear_common_(ptr, stream, pos, colon));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_pprint_linear(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT_LINEAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt2(pos, p_defun_pprint_linear);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PprintFill);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-tabular (stream object &optional colon-p at-sign-p) ...) -> null
 *   stream     (or boolean stream)  ;; output-stream-designator
 *   object     t
 *   colon-p    t  ;; boolean
 *   at-sign-p  t  ;; boolean
 */
static int function_pprint_tabular(Execute ptr,
		addr stream, addr pos, addr colon, addr atsign, addr tabsize)
{
	Return(pprint_tabular_common_(ptr, stream, pos, colon, tabsize));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_pprint_tabular(addr *ret)
{
	/* (function (output-stream-designator t &optional t t (integer 0 *))
	 *           (values null &rest nil))
	 */
	addr args, values, type;

	GetTypeTable(&args, StreamDesignator);
	GetTypeTable(&values, T);
	GetTypeTable(&type, Intplus);
	typeargs_var2opt3(&args, args, values, values, values, type);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_tabular(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT_TABULAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt3(pos, p_defun_pprint_tabular);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pprint_tabular(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-indent (relative-to n &optional stream) ...) -> null
 *   relative-to  (member :block :current)
 *   n            real
 *   stream       output-stream-designator
 */
static int function_pprint_indent(Execute ptr, addr rel, addr n, addr stream)
{
	Return(pprint_indent_common_(ptr, rel, n, stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_pprint_indent(addr *ret)
{
	addr args, values, type, key1, key2;

	GetConst(KEYWORD_BLOCK, &key1);
	GetConst(KEYWORD_CURRENT, &key2);
	type_member_heap(&args, key1, key2, NULL);
	GetTypeTable(&values, Real);
	GetTypeTable(&type, StreamDesignator);
	typeargs_var2opt1(&args, args, values, type);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_indent(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT_INDENT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_pprint_indent);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pprint_indent(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro pprint-logical-block
 *      (stream-symbol object &key prefix per-line-prefix suffix)
 *      declaration* form*) -> null
 */
static int function_pprint_logical_block(Execute ptr, addr form, addr env)
{
	Return(pprint_logical_block_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_pprint_logical_block(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PPRINT_LOGICAL_BLOCK, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_pprint_logical_block);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun pprint-newline (kind &optional stream) ...) -> null
 *   kind    (member :linear :fill :miser :mandatory)
 *   stream  output-steram-designator
 */
static int function_pprint_newline(Execute ptr, addr kind, addr stream)
{
	Return(pprint_newline_common_(ptr, kind, stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_pprint_newline(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PprintNewline);
	GetTypeTable(&values, StreamDesignator);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_newline(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT_NEWLINE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_pprint_newline);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pprint_newline(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-tab (kind column colink &optional stream) ...) -> null
 *   kind    (member :line :section :line-relative :section-relative)
 *   column  (integer 0 *)
 *   colinc  (integer 0 *)
 *   stream  output-steram-designator
 */
static int function_pprint_tab(Execute ptr,
		addr kind, addr column, addr colinc, addr stream)
{
	Return(pprint_tab_common_(ptr, kind, column, colinc, stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_pprint_tab(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, PprintTabular);
	GetTypeTable(&values, Intplus);
	GetTypeTable(&type, StreamDesignator);
	typeargs_var3opt1(&args, args, values, values, type);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_tab(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT_TAB, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3opt1(pos, p_defun_pprint_tab);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pprint_tab(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro print-unreadable-object
 *     ((object stream &key type identity) &body form) ...) -> null
 *   object    t  ;; evaluated
 *   stream    output-stream-designator  ;; evaluated
 *   type      t  ;; boolean, evaluated
 *   identity  t  ;; boolean, evaluated
 */
static int function_print_unreadable_object(Execute ptr, addr form, addr env)
{
	Return(print_unreadable_object_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_print_unreadable_object(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PRINT_UNREADABLE_OBJECT, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_print_unreadable_object);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun set-pprint-dispatch (type function &optional priority table) ...) -> null
 *   type      type-specifier
 *   function  (or function-designator null)
 *   priority  real  ;; default 0
 *   table     print-dispatch  ;; default *print-pprint-dispatch*
 */
static int function_set_pprint_dispatch(Execute ptr,
		addr spec, addr call, addr priority, addr table)
{
	Return(set_pprint_dispatch_common_(ptr, spec, call, priority, table));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_set_pprint_dispatch(addr *ret)
{
	addr args, values, type, dispatch;

	GetTypeTable(&args, TypeSpec);
	GetTypeTable(&values, FunctionDesignator);
	GetTypeTable(&type, Null);
	type2or_heap(values, type, &values);
	GetTypeTable(&type, Real);
	GetTypeTable(&dispatch, PrintDispatch);
	typeargs_var2opt2(&args, args, values, type, dispatch);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_set_pprint_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET_PPRINT_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt2(pos, p_defun_set_pprint_dispatch);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_set_pprint_dispatch(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write (object &key
 *     array base case circle escape gensym length level lines miser-width
 *     pprint-dispatch pretty radix readably right-margin stream) ...) -> object
 *   array            t  ;; boolean
 *   base             (integer 2 36)  ;; radix-integer
 *   case             (member :upcase :downcase :capitalize)
 *   circle           t  ;; boolean
 *   escape           t  ;; boolean
 *   gensym           t  ;; boolean
 *   length           (or null (integer 0 *))
 *   level            (or null (integer 0 *))
 *   lines            (or null (integer 0 *))
 *   miser-width      (or null (integer 0 *))
 *   pprint-dispatch  dispatch-table
 *   pretty           t  ;; boolean
 *   radix            t  ;; boolean
 *   readably         t  ;; boolean
 *   right-margin     (or null (integer 0 *))
 *   stream           output-stream-designator
 */
static int function_write(Execute ptr, addr var, addr args)
{
	Return(write_common_(ptr, var, args));
	setresult_control(ptr, var);
	return 0;
}

static void type_write(addr *ret, int stream_p)
{
	addr args, values, key;
	addr key1, key2, key3, key4, key5, key6, key7, key8;
	addr key9, key10, key11, key12, key13, key14, key15, key16;

	/* key */
	KeyTypeTable(&key1, ARRAY, T);
	KeyTypeTable(&key2, BASE, RadixInteger);
	KeyTypeTable(&key3, CASE, PrintCase);
	KeyTypeTable(&key4, CIRCLE, T);
	KeyTypeTable(&key5, ESCAPE, T);
	KeyTypeTable(&key6, GENSYM, T);
	KeyTypeTable(&key7, LENGTH, IntplusNull);
	KeyTypeTable(&key8, LEVEL, IntplusNull);
	KeyTypeTable(&key9, LINES, IntplusNull);
	KeyTypeTable(&key10, MISER_WIDTH, IntplusNull);
	KeyTypeTable(&key11, PPRINT_DISPATCH, PrintDispatch);
	KeyTypeTable(&key12, PRETTY, T);
	KeyTypeTable(&key13, RADIX, T);
	KeyTypeTable(&key14, READABLY, T);
	KeyTypeTable(&key15, RIGHT_MARGIN, IntplusNull);
	if (stream_p) {
		KeyTypeTable(&key16, STREAM, StreamDesignator);
		list_heap(&key, key1, key2, key3, key4, key5, key6, key7, key8,
				key9, key10, key11, key12, key13, key14, key15, key16, NULL);
	}
	else {
		list_heap(&key, key1, key2, key3, key4, key5, key6, key7, key8,
				key9, key10, key11, key12, key13, key14, key15, NULL);
	}
	/* type */
	GetTypeTable(&args, T);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_write(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_write);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_write(&type, 1);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun prin1 (object &optional stream) ...) -> object
 *   stream  stream-designator  ;; default standard-output
 */
static int function_prin1(Execute ptr, addr var, addr stream)
{
	Return(prin1_common_(ptr, var, stream));
	setresult_control(ptr, var);
	return 0;
}

static void defun_prin1(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PRIN1, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_prin1);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Prin1);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun princ (object &optional stream) ...) -> object
 *   stream  stream-designator  ;; default standard-output
 */
static int function_princ(Execute ptr, addr var, addr stream)
{
	Return(princ_common_(ptr, var, stream));
	setresult_control(ptr, var);
	return 0;
}

static void defun_princ(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PRINC, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_princ);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Prin1);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun print (object &optional stream) ...) -> object
 *   stream  stream-designator  ;; default standard-output
 */
static int function_print(Execute ptr, addr var, addr stream)
{
	Return(print_common_(ptr, var, stream));
	setresult_control(ptr, var);
	return 0;
}

static void defun_print(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PRINT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_print);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Prin1);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint (object &optional stream) ...) -> (values)
 *   stream  stream-designator  ;; default standard-output
 */
static int function_pprint(Execute ptr, addr var, addr stream)
{
	Return(pprint_common_(ptr, var, stream));
	setvalues_nil_control(ptr);
	return 0;
}

static void type_pprint(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, StreamDesignator);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Nil);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_pprint);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pprint(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-to-string (object &key
 *     array base case circle escape gensym length level lines miser-width
 *     pprint-dispatch pretty radix readably right-margin) ...) -> string
 *   array            t  ;; boolean
 *   base             (integer 2 36)  ;; radix-integer
 *   case             (member :upcase :downcase :capitalize)
 *   circle           t  ;; boolean
 *   escape           t  ;; boolean
 *   gensym           t  ;; boolean
 *   length           (or null (integer 0 *))
 *   level            (or null (integer 0 *))
 *   lines            (or null (integer 0 *))
 *   miser-width      (or null (integer 0 *))
 *   pprint-dispatch  dispatch-table
 *   pretty           t  ;; boolean
 *   radix            t  ;; boolean
 *   readably         t  ;; boolean
 *   right-margin     (or null (integer 0 *))
 */
static int function_write_to_string(Execute ptr, addr var, addr args)
{
	Return(write_to_string_common_(ptr, var, args, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_write_to_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE_TO_STRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_write_to_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_write(&type, 0);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun prin1-to-string (object) ...) -> string */
static int function_prin1_to_string(Execute ptr, addr var)
{
	Return(prin1_to_string_common_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_prin1_to_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PRIN1_TO_STRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_prin1_to_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Prin1ToString);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun princ-to-string (object) ...) -> string */
static int function_princ_to_string(Execute ptr, addr var)
{
	Return(princ_to_string_common_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_princ_to_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PRINC_TO_STRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_princ_to_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Prin1ToString);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defvar *print-array* t) ;; boolean */
static void defvar_print_array(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_ARRAY, &symbol);
	SetValueSymbol(symbol, T);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-base* (integer 2 36)) */
static void defvar_print_base(void)
{
	addr symbol, value, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_BASE, &symbol);
	fixnum_heap(&value, 10);
	SetValueSymbol(symbol, value);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, RadixInteger);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-radix* t) ;; boolean */
static void defvar_print_radix(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_RADIX, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-case* (member :upcase :downcase :capitalize)) */
static void defvar_print_case(void)
{
	addr symbol, value, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_CASE, &symbol);
	GetConst(KEYWORD_UPCASE, &value);
	SetValueSymbol(symbol, value);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, PrintCase);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-circle* t) ;; boolean */
static void defvar_print_circle(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_CIRCLE, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-escape* t) ;; boolean */
static void defvar_print_escape(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_ESCAPE, &symbol);
	SetValueSymbol(symbol, T);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-gensym* t) ;; boolean */
static void defvar_print_gensym(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_GENSYM, &symbol);
	SetValueSymbol(symbol, T);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-level* index-null) */
static void defvar_print_level(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_LEVEL, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, IndexNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-length* index-null) */
static void defvar_print_length(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_LENGTH, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, IndexNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-lines* index-null) */
static void defvar_print_lines(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_LINES, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, IndexNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-miser-width* index-null) */
static void defvar_print_miser_width(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_MISER_WIDTH, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, IndexNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-pprint-dispatch* [pprint-dispatch-type]) */
static void defvar_print_pprint_dispatch(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_PPRINT_DISPATCH, &symbol);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, PrintDispatch);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-pretty* t) ;; boolean */
static void defvar_print_pretty(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_PRETTY, &symbol);
	SetValueSymbol(symbol, Nil); /* set T in buildlisp() */
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-readably* t) ;; boolean */
static void defvar_print_readably(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_READABLY, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-right-margin* index-null) */
static void defvar_print_right_margin(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_RIGHT_MARGIN, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, IndexNull);
	settype_value_symbol(symbol, type);
}


/* (defun print-not-readable-object (print-not-readable) ...) -> t */
static int function_print_not_readable_object(Execute ptr, addr var)
{
	Return(print_not_readable_object_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_print_not_readable_object(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrintNotReadable);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_print_not_readable_object(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PRINT_NOT_READABLE_OBJECT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_print_not_readable_object);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_print_not_readable_object(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun format (destination control-string &rest args) -> result
 *   destination     (or null (eql t) stream string)
 *   control-string  (or string function)
 *   args            (&rest t)
 *   result          (or null string)
 */
static int function_format(Execute ptr, addr var, addr format, addr args)
{
	Return(format_lisp_(ptr, var, format, args, &args));
	setresult_control(ptr, var == Nil? args: Nil);
	return 0;
}

static void type_format(addr *ret)
{
	addr args, values, type, call;

	GetTypeTable(&args, Format);
	GetTypeTable(&values, T);
	GetTypeTable(&type, String);
	GetTypeTable(&call, Function);
	type2or_heap(type, call, &type);
	typeargs_var2rest(&args, args, type, values);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, ret);
}

static void defun_format(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FORMAT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_format);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_format(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_printer(void)
{
	SetPointerCall(defun, opt1, copy_pprint_dispatch);
	SetPointerCall(defmacro, macro, formatter);
	SetPointerCall(defun, var1opt1, pprint_dispatch);
	SetPointerCall(defun, var2opt2, pprint_fill);
	SetPointerCall(defun, var2opt2, pprint_linear);
	SetPointerCall(defun, var2opt3, pprint_tabular);
	SetPointerCall(defun, var2opt1, pprint_indent);
	SetPointerCall(defmacro, macro, pprint_logical_block);
	SetPointerCall(defun, var1opt1, pprint_newline);
	SetPointerCall(defun, var3opt1, pprint_tab);
	SetPointerCall(defmacro, macro, print_unreadable_object);
	SetPointerCall(defun, var2opt2, set_pprint_dispatch);
	SetPointerCall(defun, var1dynamic, write);
	SetPointerCall(defun, var1opt1, prin1);
	SetPointerCall(defun, var1opt1, princ);
	SetPointerCall(defun, var1opt1, print);
	SetPointerCall(defun, var1opt1, pprint);
	SetPointerCall(defun, var1dynamic, write_to_string);
	SetPointerCall(defun, var1, prin1_to_string);
	SetPointerCall(defun, var1, princ_to_string);
	SetPointerCall(defun, var1, print_not_readable_object);
	SetPointerCall(defun, var2dynamic, format);
}

void build_common_printer(void)
{
	defun_copy_pprint_dispatch();
	defmacro_formatter();
	defun_pprint_dispatch();
	defun_pprint_fill();
	defun_pprint_linear();
	defun_pprint_tabular();
	defun_pprint_indent();
	defmacro_pprint_logical_block();
	defun_pprint_newline();
	defun_pprint_tab();
	defmacro_print_unreadable_object();
	defun_set_pprint_dispatch();
	defun_write();
	defun_prin1();
	defun_princ();
	defun_print();
	defun_pprint();
	defun_write_to_string();
	defun_prin1_to_string();
	defun_princ_to_string();
	defvar_print_array();
	defvar_print_base();
	defvar_print_radix();
	defvar_print_case();
	defvar_print_circle();
	defvar_print_escape();
	defvar_print_gensym();
	defvar_print_level();
	defvar_print_length();
	defvar_print_lines();
	defvar_print_miser_width();
	defvar_print_pprint_dispatch();
	defvar_print_pretty();
	defvar_print_readably();
	defvar_print_right_margin();
	defun_print_not_readable_object();
	defun_format();
}


/************************************************************
 *  common_reader.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 23. Reader
 */

/* (defun copy-readtable (&optional from to) ...) -> readtable
 *   from  (or readtable null)  ;; readtable-designator
 *   to    (or readtable null)
 */
static int function_copy_readtable(Execute ptr, addr from, addr to)
{
	Return(copy_readtable_common_(ptr, from, to, &from));
	setresult_control(ptr, from);
	return 0;
}

static void type_copy_readtable(addr *ret)
{
	addr args, values;

	/* (function (readtable-designator readtable-designator)
	 *   (values readtable &rest nil))
	 */
	GetTypeTable(&args, ReadtableDesignator);
	typeargs_opt2(&args, args, args);
	GetTypeTable(&values, Readtable);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_copy_readtable(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_READTABLE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt2(pos, p_defun_copy_readtable);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_copy_readtable(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-dispatch-macro-character
 *   (char &optional non-terminating-p readtable) ...) -> (eql t)
 *   char               character
 *   non-terminating-p  t  ;; boolean, default nil
 *   readtable          readtable   ;; default *readtable*
 */
static int function_make_dispatch_macro_character(Execute ptr,
		addr code, addr nonterm, addr readtable)
{
	Return(make_dispatch_macro_character_common_(ptr, code, nonterm, readtable));
	setresult_control(ptr, T);
	return 0;
}

static void type_make_dispatch_macro_character(addr *ret)
{
	addr args, values, type1, type2;

	/* (function (character &optional t readtable) ...)
	 *    -> (values (eql t) &rest nil)
	 */
	GetTypeTable(&args, Character);
	GetTypeTable(&type1, T);
	GetTypeTable(&type2, Readtable);
	typeargs_var1opt2(&args, args, type1, type2);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(args, values, ret);
}

static void defun_make_dispatch_macro_character(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_DISPATCH_MACRO_CHARACTER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt2(pos, p_defun_make_dispatch_macro_character);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_dispatch_macro_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read (&optional stream errorp eof recp) ...) -> object
 *   stream  input-stream-designator
 *   errorp  t  (boolean)
 *   eof     t
 *   recp    t  (boolean)
 */
static int function_read(Execute ptr, addr var, addr errorp, addr eof, addr recp)
{
	Return(read_common_(ptr, var, errorp, eof, recp, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_read(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt4(pos, p_defun_read);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Read);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read-preserving-whitespace (&optional stream errorp eof recp) ...) -> object
 *   stream  input-stream-designator
 *   errorp  t  (boolean)
 *   eof     t
 *   recp    t  (boolean)
 */
static int function_read_preserving_whitespace(Execute ptr,
		addr var, addr errorp, addr eof, addr recp)
{
	Return(read_preserving_whitespace_common_(ptr, var, errorp, eof, recp, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_read_preserving_whitespace(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_PRESERVING_WHITESPACE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt4(pos, p_defun_read_preserving_whitespace);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Read);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defn read-delimited-list (character &optional stream recursive-p) ...) -> list */
static int function_read_delimited_list(Execute ptr,
		addr code, addr stream, addr recp)
{
	return read_delimited_list_common_(ptr, code, stream, recp);
}

static void type_read_delimited_list(addr *ret)
{
	addr args, values, type1, type2;

	/* (function (character &optional stream t) ...)
	 *    -> (values list &rest nil)
	 */
	GetTypeTable(&args, Character);
	GetTypeTable(&type1, Stream);
	GetTypeTable(&type2, T);
	typeargs_var1opt2(&args, args, type1, type2);
	GetTypeTable(&values, List);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_read_delimited_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_DELIMITED_LIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt2(pos, p_defun_read_delimited_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_read_delimited_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read-from-string
 *    (string
 *     &optional eof-error-p eof-value
 *     &key start end preserve-whitespace) ...)
 *    -> (values object position)
 *   string               string
 *   eof-error-p          t  ;; boolean, default t
 *   eof-value            t  ;; default nil
 *   start                (integer 0 *)  ;; default 0
 *   end                  (or (integer 0 *) null)  ;; default nil
 *   preserve-whitespace  t  ;; boolean, default nil
 *   object               t
 *   position             (integer 0 *)
 */
static int function_read_from_string(Execute ptr, addr args)
{
	addr second;
	Return(read_from_string_common_(ptr, args, &args, &second));
	setvalues_control(ptr, args, second, NULL);
	return 0;
}

static void type_read_from_string(addr *ret)
{
	/* (function (string &optional t t
	 *    &key (start keyword-start)
	 *         (end keyword-end)
	 *         (preserve-whitespace t))
	 *    (values t index &rest nil))
	 */
	addr str, type, type1, type2, size, var, opt, key, start, end, pre;
	addr args, values;

	GetTypeTable(&str, String);
	GetTypeTable(&type, T);
	GetTypeTable(&size, Index);
	GetTypeTable(&type1, KeywordStart);
	GetTypeTable(&type2, KeywordEnd);
	conscar_heap(&var, str);
	list_heap(&opt, type, type, NULL);
	GetConst(KEYWORD_START, &start);
	GetConst(KEYWORD_END, &end);
	GetConst(KEYWORD_PRESERVE_WHITESPACE, &pre);
	cons_heap(&start, start, type1);
	cons_heap(&end, end, type2);
	cons_heap(&pre, pre, type);
	list_heap(&key, start, end, pre, NULL);
	typeargs_full(&args, var, opt, Nil, key);
	typevalues_values2(&values, type, size);
	type_compiled_heap(args, values, ret);
}

static void defun_read_from_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_FROM_STRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_read_from_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_read_from_string(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun readtable-case (readtable) ...) -> mode */
static int function_readtable_case(Execute ptr, addr var)
{
	Return(readtable_case_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_readtable_case(addr *ret)
{
	/* (function (readtable)
	 *   (values (member :upcase :downcase :preserve :invert) &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, Readtable);
	GetTypeTable(&values, CaseSensitivity);
	typeargs_var1(&args, args);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_readtable_case(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READTABLE_CASE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_readtable_case);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_readtable_case(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf readtable-case) (mode readtable) ...) -> mode */
static int function_setf_readtable_case(Execute ptr, addr value, addr var)
{
	Return(setf_readtable_case_common_(value, var));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_readtable_case(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&values, CaseSensitivity);
	GetTypeTable(&type, Readtable);
	typeargs_var2(&args, values, type);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_readtable_case(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READTABLE_CASE, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_setf_readtable_case);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_readtable_case(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun readtablep (object) ...) -> boolean */
static int function_readtablep(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_READTABLE);
	return 0;
}

static void defun_readtablep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READTABLEP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_readtablep);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get-dispatch-macro-character
 *   (disp-char sub-char &optional readtable) ...) -> function
 *   disp-char  character
 *   sub-char   character
 *   readtable  (or readtable null)  ;; readtable designator
 *   function   (or function null)
 */
static int function_get_dispatch_macro_character(Execute ptr,
		addr x, addr y, addr readtable)
{
	Return(get_dispatch_macro_character_common_(ptr, x, y, readtable, &x));
	setresult_control(ptr, x);
	return 0;
}

static void type_get_dispatch_macro_character(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Character);
	GetTypeTable(&type, ReadtableDesignator);
	typeargs_var2opt1(&args, args, args, type);
	GetTypeTable(&values, FunctionNull);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_get_dispatch_macro_character(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_DISPATCH_MACRO_CHARACTER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_get_dispatch_macro_character);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get_dispatch_macro_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun set-dispatch-macro-character
 *   (disp-char sub-char function &optional readtable) ...) -> (eql t)
 *   disp-char  character
 *   sub-char   character
 *   function   (or symbol function)  ;; function-designator
 *   readtable  (or readtable null)  ;; readtable designator
 */
static int function_set_dispatch_macro_character(Execute ptr,
		addr x, addr y, addr call, addr readtable)
{
	Return(set_dispatch_macro_character_common_(ptr, x, y, call, readtable));
	setresult_control(ptr, T);
	return 0;
}

static void type_set_dispatch_macro_character(addr *ret)
{
	addr args, values, type1, type2;

	GetTypeTable(&args, Character);
	GetTypeTable(&type1, FunctionDesignator);
	GetTypeTable(&type2, ReadtableDesignator);
	typeargs_var3opt1(&args, args, args, type1, type2);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(args, values, ret);
}

static void defun_set_dispatch_macro_character(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET_DISPATCH_MACRO_CHARACTER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3opt1(pos, p_defun_set_dispatch_macro_character);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_set_dispatch_macro_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get-macro-character (char &optional readtable) ...)
 *     (values function nonterm &rest nil))
 *   char       character
 *   readtable  (or readtable null)  ;; readtable-designator
 *   function   (or function nil)
 *   nonterm    boolean
 */
static int function_get_macro_character(Execute ptr, addr code, addr table)
{
	Return(get_macro_character_common_(ptr, code, table, &code, &table));
	setvalues_control(ptr, code, table, NULL);
	return 0;
}

static void type_get_macro_character(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Character);
	GetTypeTable(&type, ReadtableDesignator);
	typeargs_var1opt1(&args, args, type);
	GetTypeTable(&values, FunctionNull);
	GetTypeTable(&type, Boolean);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_get_macro_character(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_MACRO_CHARACTER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_get_macro_character);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get_macro_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defn set-macro-character
 *     (char function &optional nonterm readtable) ...)
 *     -> (eql t)
 *   char       character
 *   function   (or symbol function)  ;; function-designator
 *   nonterm    t  ;; boolean, default nil
 *   readtable  (or readtable null)  ;; readtable-designator
 */
static int function_set_macro_character(Execute ptr,
		addr code, addr call, addr nonterm, addr readtable)
{
	Return(set_macro_character_common_(ptr, code, call, nonterm, readtable));
	setresult_control(ptr, T);
	return 0;
}

static void type_set_macro_character(addr *ret)
{
	addr args, values, type1, type2, type3;

	GetTypeTable(&args, Character);
	GetTypeTable(&type1, FunctionDesignator);
	GetTypeTable(&type2, T);
	GetTypeTable(&type3, ReadtableDesignator);
	typeargs_var2opt2(&args, args, type1, type2, type3);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(args, values, ret);
}

static void defun_set_macro_character(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET_MACRO_CHARACTER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt2(pos, p_defun_set_macro_character);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_set_macro_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun set-syntax-from-char
 *     (to-char from-char &optional to-readtable from-readtable) ...)
 *     -> (eql t)
 *   to-char         character
 *   from-char       character
 *   to-readtable    readtable  ;; default *readtable*
 *   from-readtable  (or readtable null)  ;; readtable-designator, default nil
 */
static int function_set_syntax_from_char(Execute ptr, addr x, addr y, addr z, addr w)
{
	Return(set_syntax_from_char_common_(ptr, x, y, z, w));
	setresult_control(ptr, T);
	return 0;
}

static void type_set_syntax_from_char(addr *ret)
{
	addr args, values, type1, type2;

	GetTypeTable(&args, Character);
	GetTypeTable(&type1, Readtable);
	GetTypeTable(&type2, ReadtableDesignator);
	typeargs_var2opt2(&args, args, args, type1, type2);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(args, values, ret);
}

static void defun_set_syntax_from_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET_SYNTAX_FROM_CHAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt2(pos, p_defun_set_syntax_from_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_set_syntax_from_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-standard-io-syntax (&body form) ...) */
static int function_with_standard_io_syntax(Execute ptr, addr form, addr env)
{
	Return(with_standard_io_syntax_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_standard_io_syntax(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_STANDARD_IO_SYNTAX, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_standard_io_syntax);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defvar *read-base* 10) */
static void defvar_read_base(void)
{
	addr symbol, value, type;

	/* symbol */
	GetConst(SPECIAL_READ_BASE, &symbol);
	fixnum_heap(&value, 10);
	SetValueSymbol(symbol, value);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, RadixInteger);
	settype_value_symbol(symbol, type);
}


/* (defvar *read-default-float-format* 'single-float) */
static void defvar_read_default_float_format(void)
{
	addr symbol, value, type;

	/* symbol */
	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &symbol);
	GetConst(COMMON_SINGLE_FLOAT, &value);
	SetValueSymbol(symbol, value);
	setspecial_symbol(symbol);

	/* (member short-float single-float double-float long-float) */
	GetTypeTable(&type, FloatSymbol);
	settype_value_symbol(symbol, type);
}


/* (defvar *read-eval* t) */
static void defvar_read_eval(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_READ_EVAL, &symbol);
	SetValueSymbol(symbol, T);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *read-suppress* t) */
static void defvar_read_suppress(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_READ_SUPPRESS, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *readtable*) */
static void defvar_readtable(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_READTABLE, &symbol);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, Readtable);
	settype_value_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_reader(void)
{
	SetPointerCall(defun, opt2, copy_readtable);
	SetPointerCall(defun, var1opt2, make_dispatch_macro_character);
	SetPointerCall(defun, opt4, read);
	SetPointerCall(defun, opt4, read_preserving_whitespace);
	SetPointerCall(defun, var1opt2, read_delimited_list);
	SetPointerCall(defun, dynamic, read_from_string);
	SetPointerCall(defun, var1, readtable_case);
	SetPointerCall(defun, var2, setf_readtable_case);
	SetPointerCall(defun, var1, readtablep);
	SetPointerCall(defun, var2opt1, get_dispatch_macro_character);
	SetPointerCall(defun, var3opt1, set_dispatch_macro_character);
	SetPointerCall(defun, var1opt1, get_macro_character);
	SetPointerCall(defun, var2opt2, set_macro_character);
	SetPointerCall(defun, var2opt2, set_syntax_from_char);
	SetPointerCall(defmacro, macro, with_standard_io_syntax);
}

void build_common_reader(void)
{
	defun_copy_readtable();
	defun_make_dispatch_macro_character();
	defun_read();
	defun_read_preserving_whitespace();
	defun_read_delimited_list();
	defun_read_from_string();
	defun_readtable_case();
	defun_setf_readtable_case();
	defun_readtablep();
	defun_get_dispatch_macro_character();
	defun_set_dispatch_macro_character();
	defun_get_macro_character();
	defun_set_macro_character();
	defun_set_syntax_from_char();
	defmacro_with_standard_io_syntax();
	defvar_read_base();
	defvar_read_default_float_format();
	defvar_read_eval();
	defvar_read_suppress();
	defvar_readtable();
}


/************************************************************
 *  common_sequences.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 17. Sequences
 */

/* (defun copy-seq (sequence) ...) -> sequence */
static int function_copy_seq(Execute ptr, addr var)
{
	Return(copy_seq_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_copy_seq(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Sequence);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, ret);
}

static void defun_copy_seq(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_SEQ, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_copy_seq);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_copy_seq(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun elt (sequence index) ...) -> t */
static int function_elt(Execute ptr, addr var, addr index)
{
	Return(elt_common_(var, index, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_elt(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Sequence);
	GetTypeTable(&values, Index);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_elt(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ELT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_elt);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_elt(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf elt) (value sequence index) ...) -> t */
static int function_setf_elt(Execute ptr, addr value, addr pos, addr index)
{
	Return(setf_elt_common_(value, pos, index));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_elt(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Sequence);
	GetTypeTable(&values, Index);
	GetTypeTable(&type, T);
	typeargs_var3(&args, type, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_elt(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ELT, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_setf_elt);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_elt(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun fill (sequence item &key start end) ...) -> sequence
 *    item   t
 *    start  keyword-start
 *    end    keyword-end
 */
static int function_fill(Execute ptr, addr var, addr item, addr rest)
{
	Return(fill_common_(var, item, rest));
	setresult_control(ptr, var);
	return 0;
}

static void type_fill(addr *ret)
{
	addr args, values, key1, key2;

	/* key */
	GetConst(KEYWORD_START, &key1);
	GetTypeTable(&values, KeywordStart);
	cons_heap(&key1, key1, values);
	GetConst(KEYWORD_END, &key2);
	GetTypeTable(&values, KeywordEnd);
	cons_heap(&key2, key2, values);
	list_heap(&key1, key1, key2, NULL);
	/* type */
	GetTypeTable(&args, Sequence);
	GetTypeTable(&values, T);
	typeargs_var2key(&args, args, values, key1);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, ret);
}

static void defun_fill(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_fill);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_fill(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-sequence (type size &key initial-element) ...) -> sequence */
static int function_make_sequence(Execute ptr, addr type, addr size, addr rest)
{
	Return(make_sequence_common_(ptr, &type, type, size, rest));
	setresult_control(ptr, type);
	return 0;
}

static void type_make_sequence(addr *ret)
{
	addr args, values, key;

	/* key */
	GetConst(KEYWORD_INITIAL_ELEMENT, &key);
	GetTypeTable(&values, T);
	cons_heap(&key, key, values);
	list_heap(&key, key, NULL);
	/* type */
	GetTypeTable(&args, TypeSpec);
	GetTypeTable(&values, Index);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, ret);
}

static void defun_make_sequence(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_SEQUENCE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_make_sequence);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_sequence(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subseq (sequence start &optional end) ...) -> sequence
 *   start  keyword-start
 *   end    keyword-end
 */
static int function_subseq(Execute ptr, addr var, addr start, addr end)
{
	Return(subseq_common_(var, start, end, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_subseq(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Sequence);
	GetTypeTable(&values, KeywordStart);
	GetTypeTable(&type, KeywordEnd);
	typeargs_var2opt1(&args, args, values, type);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, ret);
}

static void defun_subseq(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBSEQ, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_subseq);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_subseq(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf subseq) (sequence sequence start &optional end) ...) -> sequence */
static int function_setf_subseq(Execute ptr,
		addr pos, addr root, addr start, addr end)
{
	Return(setf_subseq_common_(root, pos, start, end));
	setresult_control(ptr, pos);
	return 0;
}

static void type_setf_subseq(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Sequence);
	GetTypeTable(&values, KeywordStart);
	GetTypeTable(&type, KeywordEnd);
	typeargs_var3opt1(&args, args, args, values, type);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_subseq(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBSEQ, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var3opt1(pos, p_defun_setf_subseq);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_subseq(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun map (type function sequence &rest sequence) ...) -> sequence
 *   type      type-spec
 *   function  function-designator
 *   result    sequence
 */
static int function_map(Execute ptr, addr type, addr call, addr rest)
{
	Return(map_common_(ptr, &type, type, call, rest));
	setresult_control(ptr, type);
	return 0;
}

static void type_map(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, TypeSpec);
	GetTypeTable(&values, FunctionDesignator);
	GetTypeTable(&type, Sequence);
	typeargs_var3rest(&args, args, values, type, type);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, ret);
}

static void defun_map(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_map);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_map(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun map-into (sequence funcion &rest sequence) ...) -> sequence */
static int function_map_into(Execute ptr, addr var, addr call, addr rest)
{
	Return(map_into_common_(ptr, var, call, rest));
	setresult_control(ptr, var);
	return 0;
}

static void type_map_into(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Sequence);
	GetTypeTable(&values, FunctionDesignator);
	typeargs_var2rest(&args, args, values, args);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, ret);
}

static void defun_map_into(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAP_INTO, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_map_into);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_map_into(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun reduce (function sequence &key key from-end end initial-value) ...) -> t
 *   key            function-designator  ;; (or function-designator null)
 *   from-end       t  ;; boolean
 *   start          keyword-start
 *   end            keyword-end
 *   initial-value  t
 */
static int function_reduce(Execute ptr, addr call, addr pos, addr rest)
{
	Return(reduce_common_(ptr, &call, call, pos, rest));
	setresult_control(ptr, call);
	return 0;
}

static void type_reduce(addr *ret)
{
	addr args, values, type, key1, key2, key3, key4, key5;

	/* key */
	GetConst(KEYWORD_KEY, &key1);
	GetTypeTable(&type, FunctionDesignator);
	cons_heap(&key1, key1, type);
	GetConst(KEYWORD_FROM_END, &key2);
	GetTypeTable(&type, T);
	cons_heap(&key2, key2, type);
	GetConst(KEYWORD_START, &key3);
	GetTypeTable(&type, KeywordStart);
	cons_heap(&key3, key3, type);
	GetConst(KEYWORD_END, &key4);
	GetTypeTable(&type, KeywordEnd);
	cons_heap(&key4, key4, type);
	GetConst(KEYWORD_INITIAL_VALUE, &key5);
	GetTypeTable(&type, T);
	cons_heap(&key5, key5, type);
	list_heap(&type, key1, key2, key3, key4, key5, NULL);
	/* type */
	GetTypeTable(&args, FunctionDesignator);
	GetTypeTable(&values, Sequence);
	typeargs_var2key(&args, args, values, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_reduce(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REDUCE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_reduce);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_reduce(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun count (item sequence
 *     &key from-end start end key test test-not) ...) -> n
 *   from-end  t  ;; boolean
 *   start     keyword-start
 *   end       keyword-end
 *   key       function-designator
 *   test      function-designator
 *   test-not  function-designator
 */
static int function_count(Execute ptr, addr item, addr pos, addr rest)
{
	Return(count_common_(ptr, &item, item, pos, rest));
	setresult_control(ptr, item);
	return 0;
}

static void type_count(addr *ret)
{
	addr args, values, key;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Sequence);
	GetTypeTable(&key, CountKey);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, Index);
	type_compiled_heap(args, values, ret);
}

static void defun_count(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COUNT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_count);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_count(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun count-if (call sequence &key from-end start end key) ...) -> n */
static int function_count_if(Execute ptr, addr call, addr var, addr rest)
{
	Return(count_if_common_(ptr, &call, call, var, rest));
	setresult_control(ptr, call);
	return 0;
}

static void defun_count_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COUNT_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_count_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, CountIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun count-if (call sequence &key from-end start end key) ...) -> n */
static int function_count_if_not(Execute ptr, addr call, addr var, addr rest)
{
	Return(count_if_not_common_(ptr, &call, call, var, rest));
	setresult_control(ptr, call);
	return 0;
}

static void defun_count_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COUNT_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_count_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, CountIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun length (sequence) ...) -> index */
static int function_length(Execute ptr, addr var)
{
	size_t size;

	Return(length_sequence_(var, 1, &size));
	setresult_control(ptr, intsizeh(size));

	return 0;
}

static void type_length(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Sequence);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Index);
	type_compiled_heap(args, values, ret);
}

static void defun_length(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LENGTH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_length);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_length(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun reverse (sequence) ...) -> sequence */
static int function_reverse(Execute ptr, addr var)
{
	Return(reverse_sequence_heap_(&var, var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_reverse(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REVERSE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_reverse);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Reverse);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nreverse (sequence) ...) -> sequence */
static int function_nreverse(Execute ptr, addr var)
{
	Return(nreverse_sequence_(&var, var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_nreverse(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NREVERSE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_nreverse);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Reverse);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun merge (type sequence1 sequence2 call &key key) ...) -> sequence */
static int function_merge(Execute ptr,
		addr type, addr pos1, addr pos2, addr call, addr rest)
{
	Return(merge_common_(ptr, type, pos1, pos2, call, rest, &pos1));
	setresult_control(ptr, pos1);
	return 0;
}

static void type_merge(addr *ret)
{
	addr args, values, type, key;

	/* key */
	GetConst(KEYWORD_KEY, &key);
	GetTypeTable(&type, FunctionDesignator);
	cons_heap(&key, key, type);
	conscar_heap(&key, key);
	/* type */
	GetTypeTable(&args, TypeSpec);
	GetTypeTable(&values, Sequence);
	typeargs_var4key(&args, args, values, values, type, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, ret);
}

static void defun_merge(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MERGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var4dynamic(pos, p_defun_merge);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_merge(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sort (sequence call &key key) ...) -> sequence */
static int function_sort(Execute ptr, addr pos, addr call, addr rest)
{
	Return(sort_common_(ptr, pos, call, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_sort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_sort);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun stable-sort (sequence call &key key) ...) -> sequence */
static int function_stable_sort(Execute ptr, addr pos, addr call, addr rest)
{
	Return(stable_sort_common_(ptr, pos, call, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_stable_sort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STABLE_SORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_stable_sort);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun find (item sequence
 *     &key from-end test test-not start end key) ...) -> t
 */
static int function_find(Execute ptr, addr item, addr pos, addr rest)
{
	Return(find_common_(ptr, &pos, item, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void type_find(addr *ret)
{
	addr args, values, key;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Sequence);
	GetTypeTable(&key, CountKey);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_find(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_find);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_find(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun find-if (call sequence &key from-end start end key) ...) -> t */
static int function_find_if(Execute ptr, addr call, addr pos, addr rest)
{
	Return(find_if_common_(ptr, &pos, call, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_find_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_find_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, FindIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun find-if-not (call sequence &key from-end start end key) ...) -> t */
static int function_find_if_not(Execute ptr, addr call, addr pos, addr rest)
{
	Return(find_if_not_common_(ptr, &pos, call, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_find_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_find_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, FindIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun position (item sequence
 *     &key from-end test test-not start end key) ...) -> t
 */
static int function_position(Execute ptr, addr item, addr pos, addr rest)
{
	Return(position_common_(ptr, &pos, item, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void type_position(addr *ret)
{
	addr args, values, key;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Sequence);
	GetTypeTable(&key, CountKey);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, IndexNull);
	type_compiled_heap(args, values, ret);
}

static void defun_position(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_POSITION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_position);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_position(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun position-if (call sequence &key from-end start end key) ...) -> t */
static int function_position_if(Execute ptr, addr call, addr pos, addr rest)
{
	Return(position_if_common_(ptr, &pos, call, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_position_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_POSITION_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_position_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PositionIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun position-if-not (call sequence &key from-end start end key) ...) -> t */
static int function_position_if_not(Execute ptr, addr call, addr pos, addr rest)
{
	Return(position_if_not_common_(ptr, &pos, call, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_position_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_POSITION_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_position_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PositionIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun search (sequence1 sequence2
 *     &key from-end test test-not key start1 start2 end1 end2) ...) -> position
 */
static int function_search(Execute ptr, addr pos1, addr pos2, addr rest)
{
	Return(search_common_(ptr, &pos1, pos1, pos2, rest));
	setresult_control(ptr, pos1);
	return 0;
}

static void defun_search(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SEARCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_search);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Search);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mismatch (sequence1 sequence2
 *     &key from-end test test-not key start1 start2 end1 end2) ...) -> position
 */
static int function_mismatch(Execute ptr, addr pos1, addr pos2, addr rest)
{
	Return(mismatch_common_(ptr, &pos1, pos1, pos2, rest));
	setresult_control(ptr, pos1);
	return 0;
}

static void defun_mismatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MISMATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_mismatch);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Search);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun replace (sequence1 sequence2 &ekey start1 end1 start2 end2) ...)
 *     -> sequence1
 */
static int function_replace(Execute ptr, addr pos1, addr pos2, addr rest)
{
	Return(replace_common_(ptr, pos1, pos2, rest));
	setresult_control(ptr, pos1);
	return 0;
}

static void type_replace(addr *ret)
{
	addr args, values, key, key1, key2, key3, key4;

	/* key */
	KeyTypeTable(&key1, START1, KeywordStart);
	KeyTypeTable(&key2, START2, KeywordStart);
	KeyTypeTable(&key3, END1, KeywordEnd);
	KeyTypeTable(&key4, END2, KeywordEnd);
	list_heap(&key, key1, key2, key3, key4, NULL);

	/* type */
	GetTypeTable(&args, Sequence);
	typeargs_var2key(&args, args, args, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, ret);
}

static void defun_replace(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REPLACE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_replace);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_replace(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun substitute (newitem olditem sequence
 *     &key from-end test test-not start end count key) ...)
 *   -> sequence
 *   count  (or (integer 0) null)
 */
static int function_substitute(Execute ptr,
		addr item1, addr item2, addr pos, addr rest)
{
	Return(substitute_common_(ptr, &pos, item1, item2, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_substitute(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBSTITUTE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_substitute);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Substitute);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun substitute-if (item call sequence
 *     &key from-end start end count key) ...)
 *   -> sequence
 *   count  (or (integer 0) null)
 */
static int function_substitute_if(Execute ptr,
		addr item, addr call, addr pos, addr rest)
{
	Return(substitute_if_common_(ptr, &pos, item, call, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_substitute_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBSTITUTE_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_substitute_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SubstituteIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun substitute-if-not (item call sequence
 *     &key from-end start end count key) ...)
 *   -> sequence
 *   count  (or (integer 0) null)
 */
static int function_substitute_if_not(Execute ptr,
		addr item, addr call, addr pos, addr rest)
{
	Return(substitute_if_not_common_(ptr, &pos, item, call, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_substitute_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBSTITUTE_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_substitute_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SubstituteIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nsubstitute (newitem olditem sequence
 *     &key from-end test test-not start end count key) ...)
 *   -> sequence
 *   count  (or (integer 0) null)
 */
static int function_nsubstitute(Execute ptr,
		addr item1, addr item2, addr pos, addr rest)
{
	Return(nsubstitute_common_(ptr, item1, item2, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_nsubstitute(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSUBSTITUTE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_nsubstitute);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Substitute);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nsubstitute-if (item call sequence
 *     &key from-end start end count key) ...)
 *   -> sequence
 *   count  (or (integer 0) null)
 */
static int function_nsubstitute_if(Execute ptr,
		addr item, addr call, addr pos, addr rest)
{
	Return(nsubstitute_if_common_(ptr, item, call, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_nsubstitute_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSUBSTITUTE_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_nsubstitute_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SubstituteIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nsubstitute-if-not (item call sequence
 *     &key from-end start end count key) ...)
 *   -> sequence
 *   count  (or (integer 0) null)
 */
static int function_nsubstitute_if_not(Execute ptr,
		addr item, addr call, addr pos, addr rest)
{
	Return(nsubstitute_if_not_common_(ptr, item, call, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_nsubstitute_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSUBSTITUTE_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_nsubstitute_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SubstituteIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun concatenate (typespec &rest sequences) ...) -> result
 *   typespec   typespec
 *   sequences  &rest sequence
 *   result     sequence
 */
static int function_concatenate(Execute ptr, addr type, addr rest)
{
	Return(concatenate_common_(ptr, &type, type, rest));
	setresult_control(ptr, type);
	return 0;
}

static void type_concatenate(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, TypeSpec);
	GetTypeTable(&values, Sequence);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, ret);
}

static void defun_concatenate(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONCATENATE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_concatenate);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_concatenate(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun remove (item sequence
 *     &key from-end test test-not start end count key) ...) -> sequence
 */
static int function_remove(Execute ptr, addr item, addr pos, addr rest)
{
	Return(remove_common_(ptr, &pos, item, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_remove(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REMOVE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_remove);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Remove);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun remove-if (test sequence
 *     &key from-end start end count key) ...) -> sequence
 */
static int function_remove_if(Execute ptr, addr call, addr pos, addr rest)
{
	Return(remove_if_common_(ptr, &pos, call, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_remove_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REMOVE_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_remove_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, RemoveIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun remove-if-not (test sequence
 *     &key from-end start end count key) ...) -> sequence
 */
static int function_remove_if_not(Execute ptr, addr call, addr pos, addr rest)
{
	Return(remove_if_not_common_(ptr, &pos, call, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_remove_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REMOVE_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_remove_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, RemoveIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun delete (item sequence
 *     &key from-end test test-not start end count key) ...) -> sequence
 */
static int function_delete(Execute ptr, addr item, addr pos, addr rest)
{
	Return(delete_common_(ptr, &pos, item, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_delete(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DELETE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_delete);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Remove);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun delete-if (test sequence
 *     &key from-end start end count key) ...) -> sequence
 */
static int function_delete_if(Execute ptr, addr call, addr pos, addr rest)
{
	Return(delete_if_common_(ptr, &pos, call, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_delete_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DELETE_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_delete_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, RemoveIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun delete-if-not (test sequence
 *     &key from-end start end count key) ...) -> sequence
 */
static int function_delete_if_not(Execute ptr, addr call, addr pos, addr rest)
{
	Return(delete_if_not_common_(ptr, &pos, call, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_delete_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DELETE_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_delete_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, RemoveIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun remove-duplicates (sequence
 *     &key from-end test test-not start end key) ...) -> sequence
 */
static int function_remove_duplicates(Execute ptr, addr pos, addr rest)
{
	Return(remove_duplicates_common_(ptr, &pos, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_remove_duplicates(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REMOVE_DUPLICATES, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_remove_duplicates);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, RemoveDuplicates);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun delete-duplicates (sequence
 *     &key from-end test test-not start end key) ...) -> sequence
 */
static int function_delete_duplicates(Execute ptr, addr pos, addr rest)
{
	Return(delete_duplicates_common_(ptr, &pos, pos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_delete_duplicates(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DELETE_DUPLICATES, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_delete_duplicates);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, RemoveDuplicates);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_sequences(void)
{
	SetPointerCall(defun, var1, copy_seq);
	SetPointerCall(defun, var2, elt);
	SetPointerCall(defun, var3, setf_elt);
	SetPointerCall(defun, var2dynamic, fill);
	SetPointerCall(defun, var2dynamic, make_sequence);
	SetPointerCall(defun, var2opt1, subseq);
	SetPointerCall(defun, var3opt1, setf_subseq);
	SetPointerCall(defun, var2dynamic, map);
	SetPointerCall(defun, var2dynamic, map_into);
	SetPointerCall(defun, var2dynamic, reduce);
	SetPointerCall(defun, var2dynamic, count);
	SetPointerCall(defun, var2dynamic, count_if);
	SetPointerCall(defun, var2dynamic, count_if_not);
	SetPointerCall(defun, var1, length);
	SetPointerCall(defun, var1, reverse);
	SetPointerCall(defun, var1, nreverse);
	SetPointerCall(defun, var4dynamic, merge);
	SetPointerCall(defun, var2dynamic, sort);
	SetPointerCall(defun, var2dynamic, stable_sort);
	SetPointerCall(defun, var2dynamic, find);
	SetPointerCall(defun, var2dynamic, find_if);
	SetPointerCall(defun, var2dynamic, find_if_not);
	SetPointerCall(defun, var2dynamic, position);
	SetPointerCall(defun, var2dynamic, position_if);
	SetPointerCall(defun, var2dynamic, position_if_not);
	SetPointerCall(defun, var2dynamic, search);
	SetPointerCall(defun, var2dynamic, mismatch);
	SetPointerCall(defun, var2dynamic, replace);
	SetPointerCall(defun, var3dynamic, substitute);
	SetPointerCall(defun, var3dynamic, substitute_if);
	SetPointerCall(defun, var3dynamic, substitute_if_not);
	SetPointerCall(defun, var3dynamic, nsubstitute);
	SetPointerCall(defun, var3dynamic, nsubstitute_if);
	SetPointerCall(defun, var3dynamic, nsubstitute_if_not);
	SetPointerCall(defun, var1dynamic, concatenate);
	SetPointerCall(defun, var2dynamic, remove);
	SetPointerCall(defun, var2dynamic, remove_if);
	SetPointerCall(defun, var2dynamic, remove_if_not);
	SetPointerCall(defun, var2dynamic, delete);
	SetPointerCall(defun, var2dynamic, delete_if);
	SetPointerCall(defun, var2dynamic, delete_if_not);
	SetPointerCall(defun, var1dynamic, remove_duplicates);
	SetPointerCall(defun, var1dynamic, delete_duplicates);
}

void build_common_sequences(void)
{
	defun_copy_seq();
	defun_elt();
	defun_setf_elt();
	defun_fill();
	defun_make_sequence();
	defun_subseq();
	defun_setf_subseq();
	defun_map();
	defun_map_into();
	defun_reduce();
	defun_count();
	defun_count_if();
	defun_count_if_not();
	defun_length();
	defun_reverse();
	defun_nreverse();
	defun_merge();
	defun_sort();
	defun_stable_sort();
	defun_find();
	defun_find_if();
	defun_find_if_not();
	defun_position();
	defun_position_if();
	defun_position_if_not();
	defun_search();
	defun_mismatch();
	defun_replace();
	defun_substitute();
	defun_substitute_if();
	defun_substitute_if_not();
	defun_nsubstitute();
	defun_nsubstitute_if();
	defun_nsubstitute_if_not();
	defun_concatenate();
	defun_remove();
	defun_remove_if();
	defun_remove_if_not();
	defun_delete();
	defun_delete_if();
	defun_delete_if_not();
	defun_remove_duplicates();
	defun_delete_duplicates();
}


/************************************************************
 *  common_streams.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 21. Streams
 */

/* (defun input-stream-p (stream) ...) -> boolean */
static int function_input_stream_p(Execute ptr, addr pos)
{
	int check;

	Return(inputp_stream_(pos, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_input_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INPUT_STREAM_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_input_stream_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InputStreamP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun output-stream-p (stream) ...) -> boolean */
static int function_output_stream_p(Execute ptr, addr pos)
{
	int check;

	Return(outputp_stream_(pos, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_output_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_OUTPUT_STREAM_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_output_stream_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InputStreamP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun interactive-stream-p (stream) ...) -> boolean */
static int function_interactive_stream_p(Execute ptr, addr pos)
{
	int check;

	Return(interactivep_stream_(pos, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_interactive_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INTERACTIVE_STREAM_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_interactive_stream_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InputStreamP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun open-stream-p (stream) ...) -> boolean */
static int function_open_stream_p(Execute ptr, addr pos)
{
	int check = open_stream_p(pos);
	setbool_control(ptr, check);
	return 0;
}

static void defun_open_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_OPEN_STREAM_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_open_stream_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InputStreamP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun streamp (object) ...) -> boolean */
static int function_streamp(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_STREAM);
	return 0;
}

static void defun_streamp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STREAMP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_streamp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun stream-element-type (stream) ...) -> typespec  */
static int function_stream_element_type(Execute ptr, addr var)
{
	Return(element_type_stream_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_stream_element_type(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Stream);
	typeargs_var1(&args, args);
	GetTypeTable(&values, TypeSpec);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_stream_element_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STREAM_ELEMENT_TYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_stream_element_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_stream_element_type(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read-byte (stream &optional errorp value) ...) -> integer
 *   stream   input-stream
 *   errorp   t
 *   value    t
 *   integer  integer
 */
static int function_read_byte(Execute ptr, addr stream, addr errorp, addr value)
{
	Return(read_byte_common_(ptr, stream, errorp, value, &value));
	setresult_control(ptr, value);
	return 0;
}

static void type_read_byte(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, InputStream);
	GetTypeTable(&values, T);
	typeargs_var1opt2(&args, args, values, values);
	GetTypeValues(&values, Integer);
	type_compiled_heap(args, values, ret);
}

static void defun_read_byte(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_BYTE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt2(pos, p_defun_read_byte);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_read_byte(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-byte (byte stream) ...) -> byte
 *   byte    integer
 *   stream  output-stream
 */
static int function_write_byte(Execute ptr, addr value, addr stream)
{
	Return(write_byte_common_(ptr, value, stream));
	setresult_control(ptr, value);
	return 0;
}

static void type_write_byte(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Integer);
	GetTypeTable(&values, OutputStream);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Integer);
	type_compiled_heap(args, values, ret);
}

static void defun_write_byte(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE_BYTE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_write_byte);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_write_byte(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun peek-char (&optional type stream errorp value recp) ...) -> result
 *   type    (or character boolean)
 *   stream  input-stream
 *   errorp  t  ;; boolean
 *   value   t
 *   recp    t  ;; boolean
 *   result  t
 */
static int function_peek_char(Execute ptr, addr type, addr stream,
		addr errorp, addr value, addr recp)
{
	Return(peek_char_common_(ptr, type, stream, errorp, value, recp, &type));
	setresult_control(ptr, type);
	return 0;
}

static void type_peek_char(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Character);
	GetTypeTable(&values, Boolean);
	type2or_heap(args, values, &args);
	GetTypeTable(&values, StreamDesignator);
	GetTypeTable(&type, T);
	typeargs_opt5(&args, args, values, type, type, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_peek_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PEEK_CHAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt5(pos, p_defun_peek_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_peek_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read-char (&optional stream errorp value recp) ...) -> result
 *   stream  string-deginer  (input)
 *   errorp  t  ;; boolean
 *   value   t
 *   recp    t  ;; boolean
 *   result  t  ;; (or character t value)
 */
static int function_read_char(Execute ptr,
		addr stream, addr errorp, addr value, addr recp)
{
	Return(read_char_common_(ptr, stream, errorp, value, recp, &stream));
	setresult_control(ptr, stream);
	return 0;
}

static void defun_read_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_CHAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt4(pos, p_defun_read_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ReadChar);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read-char-no-hang (&optional stream errorp value recp) ...) -> result
 *   stream  string-deginer  (input)
 *   errorp  t  ;; boolean
 *   value   t
 *   recp    t  ;; boolean
 *   result  t  ;; (or character t value)
 */
static int function_read_char_no_hang(Execute ptr,
		addr stream, addr errorp, addr value, addr recp)
{
	Return(read_char_no_hang_common_(ptr, stream, errorp, value, recp, &stream));
	setresult_control(ptr, stream);
	return 0;
}

static void defun_read_char_no_hang(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_CHAR_NO_HANG, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt4(pos, p_defun_read_char_no_hang);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ReadChar);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun terpri (&optional stream) ...) -> null */
static int function_terpri(Execute ptr, addr stream)
{
	Return(terpri_common_(ptr, stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_terpri(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StreamDesignator);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_terpri(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TERPRI, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_terpri);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_terpri(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun fresh-line (&optional stream) ...) -> boolean */
static int function_fresh_line(Execute ptr, addr stream)
{
	Return(fresh_line_common_(ptr, stream, &stream));
	setresult_control(ptr, stream);
	return 0;
}

static void type_fresh_line(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StreamDesignator);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_fresh_line(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FRESH_LINE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_fresh_line);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_fresh_line(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun unread-char (character &optional stream) ...) -> null */
static int function_unread_char(Execute ptr, addr pos, addr stream)
{
	Return(unread_char_common_(ptr, pos, stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_unread_char(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Character);
	GetTypeTable(&values, StreamDesignator);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_unread_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNREAD_CHAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_unread_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_unread_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-char (character &optional stream) ...) -> character */
static int function_write_char(Execute ptr, addr pos, addr stream)
{
	Return(write_char_common_(ptr, pos, stream));
	setresult_control(ptr, pos);
	return 0;
}

static void type_write_char(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Character);
	GetTypeTable(&values, StreamDesignator);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Character);
	type_compiled_heap(args, values, ret);
}

static void defun_write_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE_CHAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_write_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_write_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read-line (&optional stream eof-p eof-value rec-p) ...)
 *     -> line, miss-p
 *  stream     stream-designator
 *  eof-p      t  ;; boolean
 *  eof-value  t
 *  rec-p      t  ;; boolean
 *  line       t  ;; (or string t)
 *  miss-p     boolean
 */
static int function_read_line(Execute ptr,
		addr stream, addr errorp, addr value, addr recp)
{
	Return(read_line_common_(ptr, stream, errorp, value, recp, &stream, &errorp));
	setvalues_control(ptr, stream, errorp, NULL);
	return 0;
}

static void type_read_line(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, StreamDesignator);
	GetTypeTable(&type, T);
	typeargs_opt4(&args, args, type, type, type);
	GetTypeTable(&values, Boolean);
	typevalues_values2(&values, type, values);
	type_compiled_heap(args, values, ret);
}

static void defun_read_line(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_LINE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt4(pos, p_defun_read_line);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_read_line(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-string (string &optional stream &key start end) ...) -> string */
static int function_write_string(Execute ptr, addr string, addr rest)
{
	Return(write_string_common_(ptr, string, rest));
	setresult_control(ptr, string);
	return 0;
}

static void defun_write_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE_STRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_write_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, WriteString);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-line (string &optional stream &key start end) ...) -> string */
static int function_write_line(Execute ptr, addr string, addr rest)
{
	Return(write_line_common_(ptr, string, rest));
	setresult_control(ptr, string);
	return 0;
}

static void defun_write_line(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE_LINE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_write_line);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, WriteString);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read-sequence (sequence stream &key start end) ...) -> position
 *   sequence  sequence
 *   stream    input-stream
 *   start     keyword-start
 *   end       keyword-end
 *   position  index
 */
static int function_read_sequence(Execute ptr, addr var, addr stream, addr rest)
{
	Return(read_sequence_common_(var, stream, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_read_sequence(addr *ret)
{
	addr args, values, key, type1, type2;

	/* key */
	KeyTypeTable(&type1, START, KeywordStart);
	KeyTypeTable(&type2, END, KeywordEnd);
	list_heap(&key, type1, type2, NULL);
	/* args */
	GetTypeTable(&type1, Sequence);
	GetTypeTable(&type2, InputStream);
	typeargs_var2key(&args, type1, type2, key);
	GetTypeValues(&values, Index);
	type_compiled_heap(args, values, ret);
}

static void defun_read_sequence(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_SEQUENCE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_read_sequence);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_read_sequence(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-sequence (sequence stream &key start end) ...) -> sequence */
static int function_write_sequence(Execute ptr, addr var, addr stream, addr rest)
{
	Return(write_sequence_common_(ptr->local, var, stream, rest));
	setresult_control(ptr, var);
	return 0;
}

static void type_write_sequence(addr *ret)
{
	addr args, values, key, type1, type2;

	/* key */
	KeyTypeTable(&type1, START, KeywordStart);
	KeyTypeTable(&type2, END, KeywordEnd);
	list_heap(&key, type1, type2, NULL);
	/* args */
	GetTypeTable(&type1, Sequence);
	GetTypeTable(&type2, OutputStream);
	typeargs_var2key(&args, type1, type2, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, ret);
}

static void defun_write_sequence(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE_SEQUENCE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_write_sequence);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_write_sequence(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun file-length (stream) ...) -> length
 *   stream  stream (associated with a file)
 *   length  (or Intplus null)
 */
static int function_file_length(Execute ptr, addr stream)
{
	Return(file_length_stream_(stream, &stream));
	setresult_control(ptr, stream);
	return 0;
}

static void type_file_length(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Stream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, IntplusNull);
	type_compiled_heap(args, values, ret);
}

static void defun_file_length(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILE_LENGTH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_file_length);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_file_length(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun file-position (stream &optional position) ...) -> result
 *   stream    stream
 *   position  (or index (eql :start) (eql :end))
 *   result    index-null
 */
static int function_file_position(Execute ptr, addr stream, addr pos)
{
	Return(file_position_common_(ptr, stream, pos, &stream));
	setresult_control(ptr, stream);
	return 0;
}

static void type_file_position(addr *ret)
{
	addr args, values, type, type2, type3;

	/* position-designator */
	GetTypeTable(&type, Index);
	GetConst(KEYWORD_START, &type2);
	type_eql_heap(type2, &type2);
	GetConst(KEYWORD_END, &type3);
	type_eql_heap(type3, &type3);
	type3or_heap(type, type2, type3, &type);
	/* type */
	GetTypeTable(&args, Stream);
	typeargs_var1opt1(&args, args, type);
	GetTypeValues(&values, IndexNull);
	type_compiled_heap(args, values, ret);
}

static void defun_file_position(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILE_POSITION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_file_position);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_file_position(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun file-string-length (stream object) ...) -> length
 *   stream  output-stream
 *   object  (or string character)
 *   length  index-null
 */
static int function_file_string_length(Execute ptr, addr stream, addr pos)
{
	Return(file_string_length_common_(stream, pos, &stream));
	setresult_control(ptr, stream);
	return 0;
}

static void type_file_string_length(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, OutputStream);
	GetTypeTable(&type, String);
	GetTypeTable(&values, Character);
	type2or_heap(type, values, &values);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, IndexNull);
	type_compiled_heap(args, values, ret);
}

static void defun_file_string_length(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILE_STRING_LENGTH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_file_string_length);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_file_string_length(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun open (filespec
 *     &key direction element-type if-exists if-does-not-exist external-format)
 *     ...) -> (or stream null)
 *   filespec  (or pathname-designator stream)
 */
static int function_open(Execute ptr, addr pos, addr rest)
{
	Return(open_common_(ptr, pos, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_open(addr *ret)
{
	addr args, values, key, key1, key2, key3, key4, key5;

	/* key */
	KeyTypeTable(&key1, DIRECTION, OpenDirection);
	KeyTypeTable(&key2, ELEMENT_TYPE, OpenElementType);
	KeyTypeTable(&key3, IF_EXISTS, OpenIfExists);
	KeyTypeTable(&key4, IF_DOES_NOT_EXIST, OpenIfDoesNotExist);
	KeyTypeTable(&key5, EXTERNAL_FORMAT, ExternalFormat);
	list_heap(&key, key1, key2, key3, key4, key5, NULL);
	/* type */
	GetTypeTable(&args, PathnameDesignator);
	GetTypeTable(&values, Stream);
	type2or_heap(args, values, &args);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, StreamNull);
	type_compiled_heap(args, values, ret);
}

static void defun_open(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_OPEN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_open);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_open(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun stream-external-format (stream) ...) -> format
 *   stream  stream
 *   format  external-format-designator
 */
static int function_stream_external_format(Execute ptr, addr stream)
{
	Return(external_format_stream_(stream, &stream));
	setresult_control(ptr, stream);
	return 0;
}

static void type_stream_external_format(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Stream);
	typeargs_var1(&args, args);
	GetTypeTable(&values, ExternalFormat);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_stream_external_format(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STREAM_EXTERNAL_FORMAT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_stream_external_format);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_stream_external_format(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-open-file ((stream filespec &rest options) ...) -> result */
static int function_with_open_file(Execute ptr, addr form, addr env)
{
	Return(with_open_file_common_(form, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_open_file(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_OPEN_FILE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_open_file);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun close (stream &key abort) ...) -> result */
static int function_close(Execute ptr, addr pos, addr rest)
{
	Return(close_common_(ptr, pos, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_close(addr *ret)
{
	addr args, values, key;

	/* key */
	KeyTypeTable(&key, ABORT, T);
	conscar_heap(&key, key);
	/* type */
	GetTypeTable(&args, Stream);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, T); /* any */
	type_compiled_heap(args, values, ret);
}

static void defun_close(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CLOSE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_close);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_close(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-open-stream ((var stream) decl* form*) ...) -> result */
static int function_with_open_stream(Execute ptr, addr form, addr env)
{
	Return(with_open_stream_common_(form, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defun_with_open_stream(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_OPEN_STREAM, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_open_stream);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun listen (input-stream) ...) -> boolean */
static int function_listen(Execute ptr, addr stream)
{
	Return(listen_common_(ptr, stream, &stream));
	setresult_control(ptr, stream);
	return 0;
}

static void type_listen(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, InputStream);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_listen(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LISTEN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_listen);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_listen(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun clear-input (&optional input-stream) ...) -> null */
static int function_clear_input(Execute ptr, addr stream)
{
	Return(clear_input_common_(ptr, stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_clear_input(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, InputStream);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_clear_input(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CLEAR_INPUT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_clear_input);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_clear_input(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun finish-output (&optional output-stream) ...) -> null */
static int function_finish_output(Execute ptr, addr stream)
{
	Return(finish_output_common_(ptr, stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_finish_output(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FINISH_OUTPUT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_finish_output);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, FinishOutput);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* (defun force-output (&optional output-stream) ...) -> null */
static int function_force_output(Execute ptr, addr stream)
{
	Return(force_output_common_(ptr, stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_force_output(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FORCE_OUTPUT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_force_output);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, FinishOutput);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun clear-output (&optional output-stream) ...) -> null */
static int function_clear_output(Execute ptr, addr stream)
{
	Return(clear_output_common_(ptr, stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_clear_output(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CLEAR_OUTPUT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_clear_output);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, FinishOutput);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun y-or-n-p (&optional control &rest args) ...) -> boolean
 *   control  (or string null)
 *   args     &rest t
 *   boolean  boolean
 */
static int function_y_or_n_p(Execute ptr, addr args)
{
	int check;

	Return(yes_or_no_p_common_(ptr, args, 0, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_y_or_n_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_Y_OR_N_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_y_or_n_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, YesOrNoP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun yes-or-no-p (&optional control &rest args) ...) -> boolean
 *   control  (or string null)
 *   args     &rest t
 *   boolean  boolean
 */
static int function_yes_or_no_p(Execute ptr, addr args)
{
	int check;

	Return(yes_or_no_p_common_(ptr, args, 1, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_yes_or_no_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_YES_OR_NO_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_yes_or_no_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, YesOrNoP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-synonym-stream (symbol) ...) -> synonym-stream */
static int function_make_synonym_stream(Execute ptr, addr var)
{
	Return(open_synonym_stream_(&var, var));
	setresult_control(ptr, var);
	return 0;
}

static void type_make_synonym_stream(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, SynonymStream);
	typeargs_var1(&args, args);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_synonym_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_SYNONYM_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_make_synonym_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_synonym_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun synonym-stream-symbol (synonym-stream) ...) -> symbol */
static int function_synonym_stream_symbol(Execute ptr, addr var)
{
	get_synonym_stream(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_synonym_stream_symbol(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, SynonymStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_synonym_stream_symbol(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYNONYM_STREAM_SYMBOL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_synonym_stream_symbol);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_synonym_stream_symbol(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-broadcast-stream (&rest streams) ...) -> broadcast-stream
 *   streams  (&rest (satisfies output-steram-p))
 */
static int function_make_broadcast_stream(Execute ptr, addr var)
{
	Return(open_broadcast_stream_(&var, var));
	setresult_control(ptr, var);
	return 0;
}

static void type_make_broadcast_stream(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, OutputStream);
	typeargs_rest(&args, args);
	GetTypeTable(&values, BroadcastStream);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_broadcast_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_BROADCAST_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_rest(pos, p_defun_make_broadcast_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_broadcast_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun broadcast-stream-streams (broadcast-stream) ...) -> list */
static int function_broadcast_stream_streams(Execute ptr, addr var)
{
	get_broadcast_stream(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_broadcast_stream_streams(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, BroadcastStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_broadcast_stream_streams(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BROADCAST_STREAM_STREAMS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_broadcast_stream_streams);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_broadcast_stream_streams(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-two-way-stream (input output) ...) -> two-way-stream */
static int function_make_two_way_stream(Execute ptr, addr input, addr output)
{
	open_twoway_stream(&input, input, output);
	setresult_control(ptr, input);
	return 0;
}

static void type_make_two_way_stream(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, InputStream);
	GetTypeTable(&values, OutputStream);
	typeargs_var2(&args, args, values);
	GetTypeTable(&values, TwoWayStream);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_two_way_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_TWO_WAY_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_make_two_way_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_two_way_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun two-way-stream-input-stream (two-way-stream) ...) -> input-stream */
static int function_two_way_stream_input_stream(Execute ptr, addr var)
{
	get_twoway_input_stream(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_two_way_stream_input_stream(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, TwoWayStream);
	typeargs_var1(&args, args);
	GetTypeTable(&values, InputStream);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_two_way_stream_input_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TWO_WAY_STREAM_INPUT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_two_way_stream_input_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_two_way_stream_input_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun two-way-stream-output-stream (two-way-stream) ...) -> output-stream */
static int function_two_way_stream_output_stream(Execute ptr, addr var)
{
	get_twoway_output_stream(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_two_way_stream_output_stream(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, TwoWayStream);
	typeargs_var1(&args, args);
	GetTypeTable(&values, OutputStream);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_two_way_stream_output_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TWO_WAY_STREAM_OUTPUT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_two_way_stream_output_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_two_way_stream_output_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-echo-stream (input output) ...) -> echo-stream */
static int function_make_echo_stream(Execute ptr, addr input, addr output)
{
	open_echo_stream(&input, input, output);
	setresult_control(ptr, input);
	return 0;
}

static void type_make_echo_stream(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, InputStream);
	GetTypeTable(&values, OutputStream);
	typeargs_var2(&args, args, values);
	GetTypeTable(&values, EchoStream);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_echo_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_ECHO_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_make_echo_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_echo_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun echo-stream-input-stream (echo-stream) ...) -> input-stream */
static int function_echo_stream_input_stream(Execute ptr, addr var)
{
	get_echo_input_stream(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_echo_stream_input_stream(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, EchoStream);
	typeargs_var1(&args, args);
	GetTypeTable(&values, InputStream);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_echo_stream_input_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ECHO_STREAM_INPUT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_echo_stream_input_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_echo_stream_input_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun echo-stream-output-stream (echo-stream) ...) -> output-stream */
static int function_echo_stream_output_stream(Execute ptr, addr var)
{
	get_echo_output_stream(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_echo_stream_output_stream(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, EchoStream);
	typeargs_var1(&args, args);
	GetTypeTable(&values, InputStream);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_echo_stream_output_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ECHO_STREAM_OUTPUT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_echo_stream_output_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_echo_stream_output_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-concatenated-stream (&rest streams) ...) -> concatenated-stream
 *   streams  (&rest (satisfies output-steram-p))
 */
static int function_make_concatenated_stream(Execute ptr, addr var)
{
	Return(open_concatenated_stream_(&var, var));
	setresult_control(ptr, var);
	return 0;
}

static void type_make_concatenated_stream(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, InputStream);
	typeargs_rest(&args, args);
	GetTypeTable(&values, ConcatenatedStream);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_concatenated_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_CONCATENATED_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_rest(pos, p_defun_make_concatenated_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_concatenated_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun concatenated-stream-streams (concatenated-stream) ...) -> list */
static int function_concatenated_stream_streams(Execute ptr, addr var)
{
	get_concatenated_stream(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_concatenated_stream_streams(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, ConcatenatedStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_concatenated_stream_streams(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONCATENATED_STREAM_STREAMS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_concatenated_stream_streams);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_concatenated_stream_streams(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-string-input-stream (string &optional start end) ...) -> stream
 *   string  string
 *   start   keyword-start
 *   end     keyword-end
 *   stream  string-stream
 */
static int function_make_string_input_stream(Execute ptr, addr var, addr x, addr y)
{
	Return(make_string_input_stream_common_(var, x, y, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_make_string_input_stream(addr *ret)
{
	addr args, values, type1, type2;

	/* key */
	GetTypeTable(&type1, KeywordStart);
	GetTypeTable(&type2, KeywordEnd);
	/* type */
	GetTypeTable(&args, String);
	typeargs_var1opt2(&args, args, type1, type2);
	GetTypeTable(&values, StringStream);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_string_input_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_STRING_INPUT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt2(pos, p_defun_make_string_input_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_string_input_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-string-output-stream (&key element-type) ...) -> string-stream */
static int function_make_string_output_stream(Execute ptr, addr rest)
{
	Return(make_string_output_stream_common_(ptr, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void type_make_string_output_stream(addr *ret)
{
	addr args, values;

	/* key */
	KeyTypeTable(&args, ELEMENT_TYPE, Symbol);
	list_heap(&args, args, NULL);
	/* type */
	typeargs_key(&args, args);
	GetTypeTable(&values, StringStream);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_string_output_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_STRING_OUTPUT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_make_string_output_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_string_output_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get-output-stream-string (string-stream) ...) -> simple-string */
static int function_get_output_stream_string(Execute ptr, addr var)
{
	Return(get_output_stream_string_common_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_get_output_stream_string(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, SimpleString);
	type_compiled_heap(args, values, ret);
}

static void defun_get_output_stream_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_OUTPUT_STREAM_STRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_get_output_stream_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get_output_stream_string(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-input-from-string (var string &key index start end) decl* form*
 *   var     symbol
 *   string  string
 *   index   t
 *   start   keyword-start
 *   end     keyword-end
 */
static int function_with_input_from_string(Execute ptr, addr form, addr env)
{
	Return(with_input_from_string_common_(form, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_input_from_string(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_INPUT_FROM_STRING, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_input_from_string);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro with-output-to-string
 *     (var &optional string-form &key element-type) decl* form*)
 */
static int function_with_output_to_string(Execute ptr, addr form, addr env)
{
	Return(with_output_to_string_common_(form, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_output_to_string(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_OUTPUT_TO_STRING, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_output_to_string);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun stream-error-stream (condition) -> stream */
static int function_stream_error_stream(Execute ptr, addr var)
{
	Return(stream_error_stream_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_stream_error_stream(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StreamError);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_stream_error_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STREAM_ERROR_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_stream_error_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_stream_error_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_streams(void)
{
	SetPointerCall(defun, var1, input_stream_p);
	SetPointerCall(defun, var1, output_stream_p);
	SetPointerCall(defun, var1, interactive_stream_p);
	SetPointerCall(defun, var1, open_stream_p);
	SetPointerCall(defun, var1, streamp);
	SetPointerCall(defun, var1, stream_element_type);
	SetPointerCall(defun, var1opt2, read_byte);
	SetPointerCall(defun, var2, write_byte);
	SetPointerCall(defun, opt5, peek_char);
	SetPointerCall(defun, opt4, read_char);
	SetPointerCall(defun, opt4, read_char_no_hang);
	SetPointerCall(defun, opt1, terpri);
	SetPointerCall(defun, opt1, fresh_line);
	SetPointerCall(defun, var1opt1, unread_char);
	SetPointerCall(defun, var1opt1, write_char);
	SetPointerCall(defun, opt4, read_line);
	SetPointerCall(defun, var1dynamic, write_string);
	SetPointerCall(defun, var1dynamic, write_line);
	SetPointerCall(defun, var2dynamic, read_sequence);
	SetPointerCall(defun, var2dynamic, write_sequence);
	SetPointerCall(defun, var1, file_length);
	SetPointerCall(defun, var1opt1, file_position);
	SetPointerCall(defun, var2, file_string_length);
	SetPointerCall(defun, var1dynamic, open);
	SetPointerCall(defun, var1, stream_external_format);
	SetPointerCall(defmacro, macro, with_open_file);
	SetPointerCall(defun, var1dynamic, close);
	SetPointerCall(defmacro, macro, with_open_stream);
	SetPointerCall(defun, opt1, listen);
	SetPointerCall(defun, opt1, clear_input);
	SetPointerCall(defun, opt1, finish_output);
	SetPointerCall(defun, opt1, force_output);
	SetPointerCall(defun, opt1, clear_output);
	SetPointerCall(defun, dynamic, y_or_n_p);
	SetPointerCall(defun, dynamic, yes_or_no_p);
	SetPointerCall(defun, var1, make_synonym_stream);
	SetPointerCall(defun, var1, synonym_stream_symbol);
	SetPointerCall(defun, rest, make_broadcast_stream);
	SetPointerCall(defun, var1, broadcast_stream_streams);
	SetPointerCall(defun, var2, make_two_way_stream);
	SetPointerCall(defun, var1, two_way_stream_input_stream);
	SetPointerCall(defun, var1, two_way_stream_output_stream);
	SetPointerCall(defun, var2, make_echo_stream);
	SetPointerCall(defun, var1, echo_stream_input_stream);
	SetPointerCall(defun, var1, echo_stream_output_stream);
	SetPointerCall(defun, rest, make_concatenated_stream);
	SetPointerCall(defun, var1, concatenated_stream_streams);
	SetPointerCall(defun, var1opt2, make_string_input_stream);
	SetPointerCall(defun, dynamic, make_string_output_stream);
	SetPointerCall(defun, var1, get_output_stream_string);
	SetPointerCall(defmacro, macro, with_input_from_string);
	SetPointerCall(defmacro, macro, with_output_to_string);
	SetPointerCall(defun, var1, stream_error_stream);
}

void build_common_streams(void)
{
	defun_input_stream_p();
	defun_output_stream_p();
	defun_interactive_stream_p();
	defun_open_stream_p();
	defun_streamp();
	defun_stream_element_type();
	defun_read_byte();
	defun_write_byte();
	defun_peek_char();
	defun_read_char();
	defun_read_char_no_hang();
	defun_terpri();
	defun_fresh_line();
	defun_unread_char();
	defun_write_char();
	defun_read_line();
	defun_write_string();
	defun_write_line();
	defun_read_sequence();
	defun_write_sequence();
	defun_file_length();
	defun_file_position();
	defun_file_string_length();
	defun_open();
	defun_stream_external_format();
	defmacro_with_open_file();
	defun_close();
	defun_with_open_stream();
	defun_listen();
	defun_clear_input();
	defun_finish_output();
	defun_force_output();
	defun_clear_output();
	defun_y_or_n_p();
	defun_yes_or_no_p();
	defun_make_synonym_stream();
	defun_synonym_stream_symbol();
	defun_make_broadcast_stream();
	defun_broadcast_stream_streams();
	defun_make_two_way_stream();
	defun_two_way_stream_input_stream();
	defun_two_way_stream_output_stream();
	defun_make_echo_stream();
	defun_echo_stream_input_stream();
	defun_echo_stream_output_stream();
	defun_make_concatenated_stream();
	defun_concatenated_stream_streams();
	defun_make_string_input_stream();
	defun_make_string_output_stream();
	defun_get_output_stream_string();
	defmacro_with_input_from_string();
	defmacro_with_output_to_string();
	defun_stream_error_stream();
}


/************************************************************
 *  common_strings.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 16. Strings
 */

/* (defun stringp (object) ...) -> boolean */
static int function_stringp(Execute ptr, addr var)
{
	setbool_control(ptr, stringp(var));
	return 0;
}

static void defun_stringp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRINGP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_stringp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun simple-string-p (object) ...) -> boolean */
static int function_simple_string_p(Execute ptr, addr var)
{
	simple_string_p_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_simple_string_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIMPLE_STRING_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_simple_string_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char (string index) ...) -> character
 *   string   string
 *   index    index
 */
static int function_char(Execute ptr, addr str, addr pos)
{
	Return(char_common_(str, pos, &str));
	setresult_control(ptr, str);
	return 0;
}

static void type_char(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, String);
	GetTypeTable(&type, Index);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, Character);
	type_compiled_heap(args, values, ret);
}

static void defun_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun schar (string index) ...) -> character
 *   string   string
 *   index    index
 */
static void type_schar(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, SimpleString);
	GetTypeTable(&type, Index);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, Character);
	type_compiled_heap(args, values, ret);
}

static void defun_schar(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SCHAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_schar(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf char) (character string index) ...) -> character */
static int function_setf_char(Execute ptr, addr value, addr pos, addr index)
{
	Return(setf_char_common_(value, pos, index));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_char(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Character);
	GetTypeTable(&values, String);
	GetTypeTable(&type, Index);
	typeargs_var3(&args, args, values, type);
	GetTypeValues(&values, Character);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_setf_char);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_char(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun (setf schar) (character simple-string index) ...) -> character */
static void type_setf_schar(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Character);
	GetTypeTable(&values, SimpleString);
	GetTypeTable(&type, Index);
	typeargs_var3(&args, args, values, type);
	GetTypeValues(&values, Character);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_schar(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SCHAR, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_setf_char);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_schar(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun string (x) ...) -> string
 *   x  (or string symbol character)  ;; string-designator
 */
static int function_string(Execute ptr, addr var)
{
	Return(string_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_string(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringDesignator);
	typeargs_var1(&args, args);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, ret);
}

static void defun_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_string(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-upcase (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static int function_string_upcase(Execute ptr, addr var, addr rest)
{
	Return(string_upcase_common_(var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_string_upcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_UPCASE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_string_upcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-downcase (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static int function_string_downcase(Execute ptr, addr var, addr rest)
{
	Return(string_downcase_common_(var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_string_downcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_DOWNCASE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_string_downcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-capitalize (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static int function_string_capitalize(Execute ptr, addr var, addr rest)
{
	Return(string_capitalize_common_(var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_string_capitalize(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_CAPITALIZE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_string_capitalize);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nstring-upcase (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static int function_nstring_upcase(Execute ptr, addr var, addr rest)
{
	Return(nstring_upcase_common_(var, rest));
	setresult_control(ptr, var);
	return 0;
}

static void defun_nstring_upcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSTRING_UPCASE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_nstring_upcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, NStringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nstring-downcase (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static int function_nstring_downcase(Execute ptr, addr var, addr rest)
{
	Return(nstring_downcase_common_(var, rest));
	setresult_control(ptr, var);
	return 0;
}

static void defun_nstring_downcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSTRING_DOWNCASE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_nstring_downcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, NStringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nstring-capitalize (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static int function_nstring_capitalize(Execute ptr, addr var, addr rest)
{
	Return(nstring_capitalize_common_(var, rest));
	setresult_control(ptr, var);
	return 0;
}

static void defun_nstring_capitalize(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSTRING_CAPITALIZE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_nstring_capitalize);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, NStringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-trim (sequence string) ...) -> string */
static int function_string_trim(Execute ptr, addr trim, addr pos)
{
	Return(string_trim_common_(trim, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_string_trim(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_TRIM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_string_trim);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringTrim);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static int function_string_left_trim(Execute ptr, addr trim, addr pos)
{
	Return(string_left_trim_common_(trim, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_string_left_trim(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_LEFT_TRIM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_string_left_trim);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringTrim);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static int function_string_right_trim(Execute ptr, addr trim, addr pos)
{
	Return(string_right_trim_common_(trim, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_string_right_trim(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_RIGHT_TRIM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_string_right_trim);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringTrim);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string= (string1 string2 &key start1 end1 start2 end2) ...) -> boolean
 *   string1   (or string symbol character)  ;;string-designator
 *   string2   (or string symbol character)  ;;string-designator
 *   start1    keyword-start
 *   end1      keyword-end
 *   start2    keyword-start
 *   end2      keyword-end
 *   boolean   boolean
 *   mismatch  indexnull
 */
static int function_string_eql(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_eql_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_eql(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_EQL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_eql);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringEqual);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string/= (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int function_string_not_eql(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_not_eql_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_not_eql(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_NOT_EQL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_not_eql);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string< (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int function_string_less(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_less_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_less(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_LESS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_less);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string> (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int function_string_greater(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_greater_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_greater(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_GREATER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_greater);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string<= (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int function_string_less_equal(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_less_equal_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_less_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_LESS_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_less_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string>= (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int function_string_greater_equal(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_greater_equal_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_greater_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_GREATER_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_greater_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-equal (string1 string2 &key start1 end1 start2 end2) ...)
 *   -> mismatch
 */
static int function_string_equal(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_equal_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringEqual);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-not-equal (string1 string2 &key start1 end1 start2 end2) ...)
 *   -> mismatch
 */
static int function_string_not_equal(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_not_equal_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_not_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_NOT_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_not_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-lessp (string1 string2 &key start1 end1 start2 end2) ...)
 *   -> mismatch
 */
static int function_string_lessp(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_lessp_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_lessp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_LESSP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_lessp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-greaterp (string1 string2 &key start1 end1 start2 end2) ...)
 *   -> mismatch
 */
static int function_string_greaterp(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_greaterp_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_greaterp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_GREATERP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_greaterp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-not-graeter (string1 string2 &key start1 end1 start2 end2) ...)
 *   -> mismatch
 */
static int function_string_not_greaterp(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_not_greaterp_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_not_greaterp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_NOT_GREATERP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_not_greaterp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string>= (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int function_string_not_lessp(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_not_lessp_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_not_lessp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_NOT_LESSP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_not_lessp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-string (size &key initial-element element-type) ...) -> string
 *   size             index
 *   initial-elemnet  character
 *   element-type     t   ;; type-specifier
 *   string           simple-string
 */
static int function_make_string(Execute ptr, addr var, addr rest)
{
	Return(make_string_common_(ptr, var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_make_string(addr *ret)
{
	/* (function (size &key
	 *             (initial-element character)
	 *             (element-type t))
	 *           (values simple-string &rest nil))
	 */
	addr args, values, symbol, type1, type2;

	/* args */
	GetTypeTable(&args, Index);
	GetTypeTable(&type1, Character);
	GetTypeTable(&type2, T);
	GetConst(KEYWORD_INITIAL_ELEMENT, &symbol);
	cons_heap(&type1, symbol, type1);
	GetConst(KEYWORD_ELEMENT_TYPE, &symbol);
	cons_heap(&type2, symbol, type2);
	list_heap(&type1, type1, type2, NULL);
	typeargs_var1key(&args, args, type1);
	/* values */
	GetTypeValues(&values, SimpleString);
	type_compiled_heap(args, values, ret);
}

static void defun_make_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_STRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_make_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_string(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_strings(void)
{
	SetPointerCall(defun, var1, stringp);
	SetPointerCall(defun, var1, simple_string_p);
	SetPointerCall(defun, var2, char);
	SetPointerCall(defun, var3, setf_char);
	SetPointerCall(defun, var1, string);
	SetPointerCall(defun, var1dynamic, string_upcase);
	SetPointerCall(defun, var1dynamic, string_downcase);
	SetPointerCall(defun, var1dynamic, string_capitalize);
	SetPointerCall(defun, var1dynamic, nstring_upcase);
	SetPointerCall(defun, var1dynamic, nstring_downcase);
	SetPointerCall(defun, var1dynamic, nstring_capitalize);
	SetPointerCall(defun, var2, string_trim);
	SetPointerCall(defun, var2, string_left_trim);
	SetPointerCall(defun, var2, string_right_trim);
	SetPointerCall(defun, var2dynamic, string_eql);
	SetPointerCall(defun, var2dynamic, string_not_eql);
	SetPointerCall(defun, var2dynamic, string_less);
	SetPointerCall(defun, var2dynamic, string_greater);
	SetPointerCall(defun, var2dynamic, string_less_equal);
	SetPointerCall(defun, var2dynamic, string_greater_equal);
	SetPointerCall(defun, var2dynamic, string_equal);
	SetPointerCall(defun, var2dynamic, string_not_equal);
	SetPointerCall(defun, var2dynamic, string_lessp);
	SetPointerCall(defun, var2dynamic, string_greaterp);
	SetPointerCall(defun, var2dynamic, string_not_greaterp);
	SetPointerCall(defun, var2dynamic, string_not_lessp);
	SetPointerCall(defun, var1dynamic, make_string);
}

void build_common_strings(void)
{
	defun_stringp();
	defun_simple_string_p();
	defun_char();
	defun_schar();
	defun_setf_char();
	defun_setf_schar();
	defun_string();
	defun_string_upcase();
	defun_string_downcase();
	defun_string_capitalize();
	defun_nstring_upcase();
	defun_nstring_downcase();
	defun_nstring_capitalize();
	defun_string_trim();
	defun_string_left_trim();
	defun_string_right_trim();
	defun_string_eql();
	defun_string_not_eql();
	defun_string_less();
	defun_string_greater();
	defun_string_less_equal();
	defun_string_greater_equal();
	defun_string_equal();
	defun_string_not_equal();
	defun_string_lessp();
	defun_string_greaterp();
	defun_string_not_greaterp();
	defun_string_not_lessp();
	defun_make_string();
}


/************************************************************
 *  common_structures.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 8. Structures
 */

/*
 *  (defmacro defstruct (name [doc] slots*) ...) -> symbol
 */
static int function_defstruct(Execute ptr, addr form, addr env)
{
	Return(defstruct_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defstruct(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFSTRUCT, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defstruct);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/*
 *  (defun copy-structure (structure) ...) -> structure
 */
static int function_copy_structure(Execute ptr, addr var)
{
	copy_structure_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_copy_structure(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StructureObject);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_copy_structure(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_STRUCTURE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_copy_structure);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_copy_structure(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_structures(void)
{
	SetPointerCall(defmacro, macro, defstruct);
	SetPointerCall(defun, var1, copy_structure);
}

void build_common_structures(void)
{
	defmacro_defstruct();
	defun_copy_structure();
}


/************************************************************
 *  common_symbols.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 10. Symbols
 */

/* (defvar *gensym-counter* 1) */
static void defvar_gensym_counter(void)
{
	addr symbol, value, type;

	/* symbol */
	GetConst(SPECIAL_GENSYM_COUNTER, &symbol);
	fixnum_heap(&value, 1);
	SetValueSymbol(symbol, value);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, Integer);
	settype_value_symbol(symbol, type);
}


/* (defun symbolp (object) ...) -> boolean */
static int function_symbolp(Execute ptr, addr var)
{
	setbool_control(ptr, symbolp(var));
	return 0;
}

static void defun_symbolp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOLP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_symbolp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun keywordp (object) ...) -> boolean */
static int function_keywordp(Execute ptr, addr var)
{
	setbool_control(ptr, keywordp(var));
	return 0;
}

static void defun_keywordp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_KEYWORDP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_keywordp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-symbol (string) ...) -> symbol */
static int function_make_symbol(Execute ptr, addr var)
{
	make_symbol_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_make_symbol(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, String);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_make_symbol(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_SYMBOL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_make_symbol);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_symbol(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun copy-symbol (symbol &optional boolean) ...) -> symbol */
static int function_copy_symbol(Execute ptr, addr var, addr opt)
{
	Return(copy_symbol_common_(var, opt, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_copy_symbol(addr *ret)
{
	addr args, values, boolean;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&boolean, Boolean);
	typeargs_var1opt1(&args, args, boolean);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_copy_symbol(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_SYMBOL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_copy_symbol);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_copy_symbol(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun gensym (&optional x) ...) -> symbol
 *   x  (or string Intplus)
 */
static int function_gensym(Execute ptr, addr opt)
{
	Return(gensym_common_(ptr, opt, &opt));
	setresult_control(ptr, opt);
	return 0;
}

static void type_gensym(addr *ret)
{
	addr args, values, pos;

	GetTypeTable(&args, String);
	GetTypeTable(&pos, Intplus);
	type2or_heap(args, pos, &args);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_gensym(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GENSYM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_gensym);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_gensym(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun gentemp (&optional prefix package) ...) -> symbol
 *   prefix   string
 *   package  (or package string symbol character)  ;; package-designator
 */
static int function_gentemp(Execute ptr, addr opt1, addr opt2)
{
	Return(gentemp_common_(ptr, opt1, opt2, &opt1));
	setresult_control(ptr, opt1);
	return 0;
}

static void type_gentemp(addr *ret)
{
	addr args, values, pos;

	GetTypeTable(&args, String);
	GetTypeTable(&pos, PackageDesignator);
	typeargs_opt2(&args, args, pos);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_gentemp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GENTEMP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt2(pos, p_defun_gentemp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_gentemp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun symbol-function (symbol) ...) -> function */
static int function_symbol_function(Execute ptr, addr var)
{
	Return(function_global_restart_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_symbol_function(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_symbol_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_FUNCTION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_symbol_function);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_symbol_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf symbol-function) (function symbol) ...) -> function */
static int function_setf_symbol_function(Execute ptr, addr value, addr symbol)
{
	Return(setf_symbol_function_common_(value, symbol));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_symbol_function(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Function);
	GetTypeTable(&type, Symbol);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_symbol_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_FUNCTION, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_setf_symbol_function);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_symbol_function(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun symbol-value (symbol) ...) -> object */
static int function_symbol_value(Execute ptr, addr var)
{
	Return(symbol_special_restart_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_symbol_value(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_symbol_value(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_VALUE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_symbol_value);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_symbol_value(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf symbol-value) (object symbol) ...) -> object */
static int function_setf_symbol_value(Execute ptr, addr value, addr symbol)
{
	Return(setf_symbol_value_common_(ptr, value, symbol));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_symbol_value(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&type, Symbol);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_symbol_value(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_VALUE, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_setf_symbol_value);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_symbol_value(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun symbol-plist (symbol) ...) -> list */
static int function_symbol_plist(Execute ptr, addr var)
{
	GetPlistSymbol(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_symbol_plist(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_symbol_plist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_PLIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_symbol_plist);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_symbol_plist(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf symbol-plist) (list symbol) ...) -> list */
static int function_setf_symbol_plist(Execute ptr, addr value, addr symbol)
{
	Return(setf_symbol_plist_common_(value, symbol));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_symbol_plist(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, List);
	GetTypeTable(&type, Symbol);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_symbol_plist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_PLIST, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_setf_symbol_plist);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_symbol_plist(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun symbol-name (symbol) ...) -> string */
static int function_symbol_name(Execute ptr, addr var)
{
	GetNameSymbol(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_symbol_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, ret);
}

static void defun_symbol_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_NAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_symbol_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_symbol_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun symbol-package (symbol) ...) -> contents
 *   contents  (or package null)
 */
static int function_symbol_package(Execute ptr, addr var)
{
	GetPackageSymbol(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_symbol_package(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, PackageNull);
	type_compiled_heap(args, values, ret);
}

static void defun_symbol_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_symbol_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_symbol_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get (symbol indicator &optional default) ...) -> object
 *   symbol     symbol
 *   indicator  object
 *   default    object  ;; default nil
 */
static int function_get(Execute ptr, addr var1, addr var2, addr opt)
{
	Return(get_common_(var1, var2, opt, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void type_get(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&type, T);
	typeargs_var2opt1(&args, args, type, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_get(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_get);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 * (defun (setf get) (symbol indicator &optional default) ...) -> object
 *   symbol     symbol
 *   indicator  object
 *   default    object  ;; default nil
 */
static int function_setf_get(Execute ptr,
		addr value, addr symbol, addr key, addr ignored)
{
	Return(setf_get_common_(value, symbol, key));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_get(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&type, T);
	typeargs_var3opt1(&args, type, args, type, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_get(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var3opt1(pos, p_defun_setf_get);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_get(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun remprop (symbol indicator) ...) -> boolean
 *   symbol     symbol
 *   indicator  object
 */
static int function_remprop(Execute ptr, addr symbol, addr key)
{
	Return(remprop_common_(symbol, key, &symbol));
	setresult_control(ptr, symbol);
	return 0;
}

static void type_remprop(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&type, T);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_remprop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REMPROP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_remprop);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_remprop(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun boundp (symbol) ...) -> boolean */
static int function_boundp(Execute ptr, addr var)
{
	getspecial_local(ptr, var, &var);
	setbool_control(ptr, var != Unbound);
	return 0;
}

static void defun_boundp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BOUNDP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_boundp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Symbol_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun makunbound (symbol) ...) -> symbol */
static int function_makunbound(Execute ptr, addr symbol)
{
	Return(makunbound_common_(ptr, symbol));
	setresult_control(ptr, symbol);
	return 0;
}

static void type_makunbound(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_makunbound(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKUNBOUND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_makunbound);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_makunbound(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun set (symbol value) ...) -> value */
static int function_set(Execute ptr, addr symbol, addr value)
{
	Return(set_common_(ptr, symbol, value));
	setresult_control(ptr, value);
	return 0;
}

static void type_set(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&type, T);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_set(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_set);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_set(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_symbols(void)
{
	SetPointerCall(defun, var1, symbolp);
	SetPointerCall(defun, var1, keywordp);
	SetPointerCall(defun, var1, make_symbol);
	SetPointerCall(defun, var1opt1, copy_symbol);
	SetPointerCall(defun, opt1, gensym);
	SetPointerCall(defun, opt2, gentemp);
	SetPointerCall(defun, var1, symbol_function);
	SetPointerCall(defun, var2, setf_symbol_function);
	SetPointerCall(defun, var1, symbol_value);
	SetPointerCall(defun, var2, setf_symbol_value);
	SetPointerCall(defun, var1, symbol_plist);
	SetPointerCall(defun, var2, setf_symbol_plist);
	SetPointerCall(defun, var1, symbol_name);
	SetPointerCall(defun, var1, symbol_package);
	SetPointerCall(defun, var2opt1, get);
	SetPointerCall(defun, var3opt1, setf_get);
	SetPointerCall(defun, var2, remprop);
	SetPointerCall(defun, var1, boundp);
	SetPointerCall(defun, var1, makunbound);
	SetPointerCall(defun, var2, set);
}

void build_common_symbols(void)
{
	defvar_gensym_counter();
	defun_symbolp();
	defun_keywordp();
	defun_make_symbol();
	defun_copy_symbol();
	defun_gensym();
	defun_gentemp();
	defun_symbol_function();
	defun_setf_symbol_function();
	defun_symbol_value();
	defun_setf_symbol_value();
	defun_symbol_plist();
	defun_setf_symbol_plist();
	defun_symbol_name();
	defun_symbol_package();
	defun_get();
	defun_setf_get();
	defun_remprop();
	defun_boundp();
	defun_makunbound();
	defun_set();
}


/************************************************************
 *  common_system.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 24. System Construction
 */

/* (defun compile-file
 *     (input-file &key output-file verbose print external-format)
 *     -> output-truename, warnings-p, failure-p
 *   input-file       pathname-designator ;; merge *default-pathname-defaults*
 *   output-file      pathname-designator
 *   verbose          T  ;; boolean, *compile-verbose*
 *   print            T  ;; boolean, *compile-print*
 *   external-format  external-format-designator
 *   output-truename  (or pathname null)  ;; truename
 *   warnings-p       boolean
 *   failure-p        boolean
 */
static int function_compile_file(Execute ptr, addr file, addr rest)
{
	addr x, y, z;

	Return(compile_file_common_(ptr, file, rest, &x, &y, &z));
	setvalues_control(ptr, x, y, z, NULL);

	return 0;
}

static void type_compile_file(addr *ret)
{
	addr args, values, key, key1, key2, key3, key4;
	addr type1, type2;

	/* key */
	KeyTypeTable(&key1, OUTPUT_FILE, PathnameDesignator);
	KeyTypeTable(&key2, VERBOSE, T);
	KeyTypeTable(&key3, PRINT, T);
	KeyTypeTable(&key4, EXTERNAL_FORMAT, ExternalFormat);
	list_heap(&key, key1, key2, key3, key4, NULL);

	/* type */
	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1key(&args, args, key);
	GetTypeTable(&type1, PathnameNull);
	GetTypeTable(&type2, Boolean);
	typevalues_values3(&values, type1, type2, type2);
	type_compiled_heap(args, values, ret);
}

static void defun_compile_file(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPILE_FILE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_compile_file);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_compile_file(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun compile-file-pathname
 *     (input-file &key output-file &allow-other-keys)
 *     -> pathname
 *   input-file       pathname-designator ;; merge *default-pathname-defaults*
 *   output-file      pathname-designator
 *   pathname         pathname
 */
static int function_compile_file_pathname(Execute ptr, addr var, addr rest)
{
	Return(compile_file_pathname_common_(ptr, var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_compile_file_pathname(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, Pathname);
	type_compiled_heap(args, values, ret);
}

static void defun_compile_file_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPILE_FILE_PATHNAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_compile_file_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_compile_file_pathname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun load
 *     (filespec &key verbose print if-does-not-exist external-format) ...)
 *     -> boolean
 *   filespec           (or stream pathname-designator)
 *   verbose            t  ;; boolean
 *   print              t  ;; boolean
 *   if-does-not-exist  t  ;; boolean
 *   external-format    t  ;; external-format-designator
 */
static int function_load(Execute ptr, addr filespec, addr rest)
{
	int check;

	Return(load_common_(ptr, filespec, rest, &check));
	setbool_control(ptr, check);

	return 0;
}

static void type_load(addr *ret)
{
	addr args, values, type, key1, key2, key3, key4, key5, key;

	/* args */
	GetTypeTable(&args, Stream);
	GetTypeTable(&type, PathnameDesignator);
	type2or_heap(args, type, &args);
	GetTypeTable(&type, T);
	GetConst(KEYWORD_VERBOSE, &key1);
	GetConst(KEYWORD_PRINT, &key2);
	GetConst(KEYWORD_IF_DOES_NOT_EXIST, &key3);
	GetConst(KEYWORD_EXTERNAL_FORMAT, &key4);
	GetConst(KEYWORD_TYPE, &key5);
	cons_heap(&key1, key1, type);
	cons_heap(&key2, key2, type);
	cons_heap(&key3, key3, type);
	cons_heap(&key4, key4, type);
	cons_heap(&key5, key5, type);
	list_heap(&key, key1, key2, key3, key4, key5, NULL);
	typeargs_var1key(&args, args, key);
	/* values */
	GetTypeValues(&values, Boolean);
	/* result */
	type_compiled_heap(args, values, ret);
}

static void defun_load(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOAD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_load);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_load(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-compilation-unit
 *     ((&key &allow-other-keys) &body body) ...)
 *     -> result
 */
static int function_with_compilation_unit(Execute ptr, addr form, addr env)
{
	Return(with_compilation_unit_common_(form, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_compilation_unit(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_COMPILATION_UNIT, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_compilation_unit);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defvar *features* list) */
static void defvar_features(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_FEATURES, &symbol);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, List);
	settype_value_symbol(symbol, type);
}


/* (defvar *compile-file-pathname* (or pathname null)) */
static void defvar_compile_file_pathname(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_COMPILE_FILE_PATHNAME, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, PathnameNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *compile-file-truename* (or pathname null)) */
static void defvar_compile_file_truename(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_COMPILE_FILE_TRUENAME, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, PathnameNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *load-pathname* (or pathname null)) */
static void defvar_load_pathname(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_LOAD_PATHNAME, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, PathnameNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *load-truename* (or pathname null)) */
static void defvar_load_truename(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_LOAD_TRUENAME, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, PathnameNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *compile-print* boolean) */
static void defvar_compile_print(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_COMPILE_PRINT, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *compile-verbose* boolean) */
static void defvar_compile_verbose(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_COMPILE_VERBOSE, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *load-print* boolean) */
static void defvar_load_print(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_LOAD_PRINT, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *load-verbose* boolean) */
static void defvar_load_verbose(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_LOAD_VERBOSE, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *modules* boolean) */
static void defvar_modules(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_MODULES, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, List);
	settype_value_symbol(symbol, type);
}


/* (defun provide (var) ...) -> null */
static int function_provide(Execute ptr, addr var)
{
	Return(provide_common_(ptr, var));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_provide(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringDesignator);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_provide(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PROVIDE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_provide);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_provide(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun require (var) ...) -> null */
static int function_require(Execute ptr, addr var, addr opt)
{
	Return(require_common_(ptr, var, opt));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_require(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, StringDesignator);
	GetTypeTable(&values, List);
	GetTypeTable(&type, PathnameDesignator);
	type2or_heap(values, type, &values);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_require(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REQUIRE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_require);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_require(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_system(void)
{
	SetPointerCall(defun, var1dynamic, compile_file);
	SetPointerCall(defun, var1dynamic, compile_file_pathname);
	SetPointerCall(defmacro, macro, with_compilation_unit);
	SetPointerCall(defun, var1dynamic, load);
	SetPointerCall(defun, var1, provide);
	SetPointerCall(defun, var1opt1, require);
}

void build_common_system(void)
{
	defun_compile_file();
	defun_compile_file_pathname();
	defun_load();
	defmacro_with_compilation_unit();
	defvar_features();
	defvar_compile_file_pathname();
	defvar_compile_file_truename();
	defvar_load_pathname();
	defvar_load_truename();
	defvar_compile_print();
	defvar_compile_verbose();
	defvar_load_print();
	defvar_load_verbose();
	defvar_modules();
	defun_provide();
	defun_require();
}


/************************************************************
 *  common_types.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 4. Types and Classes
 */

/* (defun coerce (object type) ...) -> result
 *   object  t
 *   type    type-spec
 *   result  t
 */
static int function_coerce(Execute ptr, addr pos, addr type)
{
	Return(coerce_common_(ptr, pos, type, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_coerce(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, TypeSpec);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_coerce(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COERCE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_coerce);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_coerce(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro deftype (name lambda &body body) ...) -> name
 *   name    symbol
 *   lambda  deftype-lambda-list
 *   body    body
 */
static int function_deftype(Execute ptr, addr form, addr env)
{
	Return(deftype_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_deftype(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFTYPE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_deftype);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun subtypep (type1 type2 &optional env) ...) -> value, valid
 *  type1  (or symbol list)  ;; type-specifier
 *  type2  (or symbol list)  ;; type-specifier
 *  env    (or environment null)
 *  value  boolean
 *  valid  boolean
 */
static int function_subtypep(Execute ptr, addr x, addr y, addr env)
{
	Return(subtypep_common_(ptr, x, y, env, &x, &y));
	setvalues_control(ptr, x, y, NULL);
	return 0;
}

static void type_subtypep(addr *ret)
{
	addr args, values, type, env;

	GetTypeTable(&type, TypeSpec);
	GetTypeTable(&env, EnvironmentNull);
	typeargs_var2opt1(&args, type, type, env);
	GetTypeTable(&values, Boolean);
	typevalues_values2(&values, values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_subtypep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBTYPEP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_subtypep);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_subtypep(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun type-of (object) ...) -> type-spec */
static int function_type_of(Execute ptr, addr pos)
{
	Return(type_of_common_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_type_of(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, TypeSymbol);
	type_compiled_heap(args, values, ret);
}

static void defun_type_of(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TYPE_OF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_type_of);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_type_of(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun typep (object type &optional env) ...) -> boolean
 *   type  type-specifier
 */
static int function_typep(Execute ptr, addr x, addr y, addr env)
{
	Return(typep_common_(ptr, x, y, env, &x));
	setresult_control(ptr, x);
	return 0;
}

static void type_typep(addr *ret)
{
	addr args, values, type, env;

	GetTypeTable(&args, T);
	GetTypeTable(&type, TypeSpec);
	GetTypeTable(&env, EnvironmentNull);
	typeargs_var2opt1(&args, args, type, env);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_typep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TYPEP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_typep);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_typep(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun type-error-datum (condition) -> object */
static int function_type_error_datum(Execute ptr, addr pos)
{
	Return(type_error_datum_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_type_error_datum(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, TypeError);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_type_error_datum(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TYPE_ERROR_DATUM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_type_error_datum);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_type_error_datum(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun type-error-expected-type (condition) -> type-spec */
static int function_type_error_expected_type(Execute ptr, addr pos)
{
	Return(type_error_expected_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_type_error_expected_type(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, TypeError);
	typeargs_var1(&args, args);
	GetTypeTable(&values, TypeSpec);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_type_error_expected_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TYPE_ERROR_EXPECTED_TYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_type_error_expected_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_type_error_expected_type(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_types(void)
{
	SetPointerCall(defun,     var2,      coerce);
	SetPointerCall(defmacro,  macro,     deftype);
	SetPointerCall(defun,     var2opt1,  subtypep);
	SetPointerCall(defun,     var1,      type_of);
	SetPointerCall(defun,     var2opt1,  typep);
	SetPointerCall(defun,     var1,      type_error_datum);
	SetPointerCall(defun,     var1,      type_error_expected_type);
}

void build_common_types(void)
{
	defun_coerce();
	defmacro_deftype();
	defun_subtypep();
	defun_type_of();
	defun_typep();
	defun_type_error_datum();
	defun_type_error_expected_type();
}


/************************************************************
 *  compile.c
 ************************************************************/

/*
 *  compile-file-pathname
 */
static void compile_file_pathname_from_input(addr input, addr *ret)
{
	addr host, device, directory, name, type, version;

	copy_pathname_heap(&input, input);
	GetHostPathname(input, &host);
	GetDirectoryPathname(input, &directory);
	GetNamePathname(input, &name);
	strvect_char_heap(&type, "fasl");

	if (pathname_logical_p(input)) {
		GetVersionPathname(input, &version);
		logical_pathname_heap(ret, host, directory, name, type, version);
	}
	else {
		GetDevicePathname(input, &device);
		pathname_heap(ret, host, device, directory, name, type);
	}
}

int compile_file_pathname_common_(Execute ptr, addr input, addr rest, addr *ret)
{
	int check;
	addr output, file;

	if (GetKeyArgs(rest, KEYWORD_OUTPUT_FILE, &output)) {
		/* input-file -> output-file */
		Return(pathname_designator_heap_(ptr, input, &input));
		compile_file_pathname_from_input(input, &file);
	}
	else {
		/* translate output-file */
		if (memory_stream_p(output))
			return Result(ret, output);
		Return(physical_pathname_heap_(ptr, output, &output));
		Return(merge_pathnames_clang_(ptr, output, Nil, Nil, &file));
	}

	/* wildcard */
	Return(wild_pathname_boolean_(file, Nil, &check));
	if (check) {
		*ret = Nil;
		return call_simple_file_error_va_(ptr, file,
				"Cannot return a wildcatd pathname, ~S.", file, NULL);
	}

	return Result(ret, file);
}


/*
 *  with-compilation-unit
 */
int with_compilation_unit_common_(addr form, addr *ret)
{
	/* (defmacro with-compilation-unit
	 *     ((&rest args &key &allow-other-keys) &body body)
	 *   `(lisp-system::with-compilation-unit
	 *      override ',args
	 *      (lambda () ,@body)))
	 */
	addr args, body, over, with, lambda;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &args, &body))
		goto error;
	if (! listp(args))
		goto error;
	if (GetKeyArgs(args, KEYWORD_OVERRIDE, &over))
		over = Nil;
	else
		over = (over == Nil)? Nil: T;

	GetConst(SYSTEM_WITH_COMPILATION_UNIT, &with);
	GetConst(COMMON_LAMBDA, &lambda);
	lista_heap(&lambda, lambda, Nil, body, NULL);
	quotelist_heap(&args, args);
	list_heap(ret, with, over, args, lambda, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("WITH-COMPILATION-UNIT form ~S "
			"must be a ((&key ...) &body ...) form.", form, NULL);
}

static int function_handler_delay_warning(Execute ptr, addr condition)
{
	int check;
	addr pos, list;

	/* push *delay-warning-list* */
	GetConst(SYSTEM_DELAY_WARNING_LIST, &pos);
	Return(getspecialcheck_local_(ptr, pos, &list));
	cons_heap(&list, condition, list);
	setspecial_local(ptr, pos, list);

	/* switch */
	GetConst(SYSTEM_DELAY_WARNING_SWITCH, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	if (pos == Nil)
		return 0;

	/* muffle-warning */
	GetConst(COMMON_MUFFLE_WARNING, &pos);
	Return(find_restart_control_(ptr, pos, condition, &condition, &check));
	if (! check)
		return call_control_error_(ptr);
	return invoke_restart_control_(ptr, condition, Nil);
}

static int handler_delay_warning_(Execute ptr)
{
	addr pos, call;

	GetConst(CONDITION_DELAY_WARNING, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_delay_warning);
	return pushhandler_common_(ptr, pos, call, 0);
}

static int with_compilation_unit_override(Execute ptr, addr pos)
{
	/* :override t */
	if (pos != Nil)
		return 1;

	/* :override nil */
	GetConst(SYSTEM_DELAY_WARNING_LIST, &pos);
	getspecial_local(ptr, pos, &pos);
	return pos == Unbound;
}

static int with_compilation_unit_call_(Execute ptr, addr call)
{
	Return(handler_delay_warning_(ptr));
	return funcall_control_(ptr, call, NULL);
}

static int function_finalize_delay_warning_(Execute ptr)
{
	addr list, x;

	GetConst(SYSTEM_DELAY_WARNING_LIST, &list);
	Return(getspecialcheck_local_(ptr, list, &list));
	nreverse(&list, list);

	GetConst(SYSTEM_DELAY_WARNING_SWITCH, &x);
	setspecial_local(ptr, x, Nil);

	while (list != Nil) {
		GetCons(list, &x, &list);
		Return(warning_restart_case_(ptr, x));
	}

	return 0;
}

static int with_compilation_unit_special_(Execute ptr, addr call)
{
	addr control, save;

	/* call */
	(void)with_compilation_unit_call_(ptr, call);

	/* unwind-protect */
	push_control(ptr, &control);
	save_execute_control(ptr, &save);
	normal_throw_control(ptr);
	if (function_finalize_delay_warning_(ptr))
		goto escape;
	restore_execute_control(ptr, save);
escape:
	return pop_control_(ptr, control);
}

int syscall_with_compilation_unit(Execute ptr, addr over, addr args, addr call)
{
	addr control, pos;

	if (! with_compilation_unit_override(ptr, over))
		return funcall_control_(ptr, call, NULL);

	push_control(ptr, &control);
	GetConst(SYSTEM_DELAY_WARNING_LIST, &pos);
	pushspecial_control(ptr, pos, Nil);
	GetConst(SYSTEM_DELAY_WARNING_SWITCH, &pos);
	pushspecial_control(ptr, pos, T);
	(void)with_compilation_unit_special_(ptr, call);
	return pop_control_(ptr, control);
}


/*
 *  initialize
 */
void init_compile(void)
{
	SetPointerCall(defun, var1, handler_delay_warning);
	init_compile_file();
	init_compile_read();
	init_compile_typedef();
	init_compile_write();
}


/************************************************************
 *  compile_array.c
 ************************************************************/

/*
 *  array-struct
 */
static int faslwrite_value_array_struct_(addr stream, const struct array_struct *str)
{
	byte v;
	size_t size;

	/* simple, adjustable, fillpointer, displaced */
	v = str->simple
		| (str->adjustable << 1U)
		| (str->fillpointer << 2U)
		| (str->displaced << 3U);
	Return(faslwrite_byte_(stream, v));
	/* type */
	v = (byte)str->type;
	Return(faslwrite_byte_(stream, v));
	/* element */
	v = (byte)str->element;
	Return(faslwrite_byte_(stream, v));
	/* bytesize */
	v = (byte)str->bytesize;
	Return(faslwrite_byte_(stream, v));
	/* size */
	size = str->size;
	Return(faslwrite_size_(stream, size));
	/* front */
	size = str->front;
	Return(faslwrite_size_(stream, size));
	/* dimension */
	size = str->dimension;
	Return(faslwrite_size_(stream, size));
	/* offset */
	size = str->offset;
	Return(faslwrite_size_(stream, size));

	return 0;
}

static int faslread_value_array_struct_(addr stream, struct array_struct *str)
{
	byte v;
	size_t size;

	/* simple, adjustable, fillpointer, displaced */
	Return(faslread_byte_(stream, &v));
	str->simple      = (v & 0x01) != 0;
	str->adjustable  = ((v >> 1U) & 0x01) != 0;
	str->fillpointer = ((v >> 2U) & 0x01) != 0;
	str->displaced   = ((v >> 3U) & 0x01) != 0;
	/* type */
	Return(faslread_byte_(stream, &v));
	str->type = (enum ARRAY_TYPE)v;
	/* element */
	Return(faslread_byte_(stream, &v));
	str->element = (unsigned)v;
	/* bytesize */
	Return(faslread_byte_(stream, &v));
	str->bytesize = (unsigned)v;
	/* size */
	Return(faslread_size_(stream, &size));
	str->size = size;
	/* front */
	Return(faslread_size_(stream, &size));
	str->front = size;
	/* dimension */
	Return(faslread_size_(stream, &size));
	str->dimension = size;
	/* offset */
	Return(faslread_size_(stream, &size));
	str->offset = size;

	return 0;
}


/*
 *  array-t
 */
static int faslwrite_value_array_t_(Execute ptr, addr stream, addr pos)
{
	addr value;
	struct array_struct *str;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	size = str->front;
	for (i = 0; i < size; i++) {
		Return(array_get_t_(pos, i, &value));
		Return(faslwrite_value_(ptr, stream, value));
	}

	return 0;
}

static int faslread_value_array_t_(Execute ptr, addr stream, addr pos)
{
	addr value;
	struct array_struct *str;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	Return(array_allocate_(NULL, pos, str));
	size = str->front;
	for (i = 0; i < size; i++) {
		Return(faslread_value_(ptr, stream, &value));
		Return(array_set_(pos, i, value));
	}

	return 0;
}


/*
 *  array-bit
 */
static int faslwrite_value_array_bit_(Execute ptr, addr stream, addr pos)
{
	int value;
	addr cons;
	struct array_struct *str;
	size_t size, i;
	LocalRoot local;
	LocalStack stack;

	str = ArrayInfoStruct(pos);
	size = str->front;
	local = ptr->local;
	push_local(local, &stack);
	bitcons_local(local, &cons, size);
	for (i = 0; i < size; i++) {
		Return(array_get_bit_(pos, i, &value));
		push_bitcons(local, cons, value);
	}

	/* write */
	bitmemory_cons_local(local, &cons, cons);
	Return(faslwrite_value_(ptr, stream, cons));
	rollback_local(local, stack);

	return 0;
}

static int faslread_value_array_bit_(Execute ptr, addr stream, addr pos)
{
	addr value;

	Return(faslread_value_(ptr, stream, &value));
	CheckType(value, LISPTYPE_BITVECTOR);
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, value);

	return 0;
}


/*
 *  array-memory
 */
static int faslwrite_value_array_memory_(Execute ptr, addr stream, addr pos)
{
	unsigned element;
	struct array_struct *str;
	struct array_value v;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	size = str->front;
	element = str->element;
	for (i = 0; i < size; i++) {
		Return(arrayinplace_get_(pos, i, &v));
		Return(faslwrite_buffer_(stream, &v.value.voidp, element));
	}

	return 0;
}

static int faslread_value_array_memory_(Execute ptr, addr stream, addr pos)
{
	unsigned element;
	struct array_struct *str;
	struct array_value v;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	Return(array_allocate_(NULL, pos, str));
	size = str->front;
	element = str->element;
	for (i = 0; i < size; i++) {
		Return(faslread_buffer_(stream, &v.value.voidp, element));
		Return(arrayinplace_set_(pos, i, &v));
	}

	return 0;
}


/*
 *  body
 */
static int faslwrite_value_array_body_(Execute ptr, addr stream, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return faslwrite_value_array_t_(ptr, stream, pos);

		case ARRAY_TYPE_BIT:
			return faslwrite_value_array_bit_(ptr, stream, pos);

		case ARRAY_TYPE_CHARACTER:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return faslwrite_value_array_memory_(ptr, stream, pos);

		default:
			return fmte_("Invalid array type.", NULL);
	}
}

static int faslread_value_array_body_(Execute ptr, addr stream, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return faslread_value_array_t_(ptr, stream, pos);

		case ARRAY_TYPE_BIT:
			return faslread_value_array_bit_(ptr, stream, pos);

		case ARRAY_TYPE_CHARACTER:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return faslread_value_array_memory_(ptr, stream, pos);

		default:
			return fmte_("Invalid array type.", NULL);
	}
}


/*
 *  dimension
 */
static int faslwrite_value_array_dimension_(Execute ptr, addr stream, addr pos)
{
	addr value;
	struct array_struct *str;
	size_t *size;

	str = ArrayInfoStruct(pos);
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &value);
	size = arraysize_ptr(value);
	Return(faslwrite_buffer_(stream, size, IdxSize * str->dimension));

	return 0;
}

static int faslread_value_array_dimension_(Execute ptr, addr stream, addr pos)
{
	addr value;
	struct array_struct *str;
	size_t *size;

	str = ArrayInfoStruct(pos);
	Return(arraysize_heap_(&value, str->dimension));
	size = arraysize_ptr(value);
	Return(faslread_buffer_(stream, size, IdxSize * str->dimension));
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, value);

	return 0;
}


/*
 *  info
 */
static int faslwrite_value_array_info_(Execute ptr, addr stream, addr pos)
{
	addr value;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &value);
	Return(faslwrite_value_(ptr, stream, value));
	if (2 <= str->dimension) {
		Return(faslwrite_value_array_dimension_(ptr, stream, pos));
	}

	return 0;
}

static int faslread_value_array_info_(Execute ptr, addr stream, addr pos)
{
	addr value;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Return(faslread_value_(ptr, stream, &value));
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, value);
	if (2 <= str->dimension) {
		Return(faslread_value_array_dimension_(ptr, stream, pos));
	}

	return 0;
}


/*
 *  array-write
 */
static int faslwrite_value_array_displaced_(Execute ptr, addr stream, addr pos)
{
	struct array_struct str;

	/* make struct */
	str = *ArrayInfoStruct(pos);
	str.displaced = 0;
	str.simple = str.adjustable == 0 && str.fillpointer == 0;
	str.offset = 0;

	/* write */
	Return(faslwrite_value_array_struct_(stream, &str));
	Return(faslwrite_value_array_info_(ptr, stream, pos));
	Return(faslwrite_value_array_body_(ptr, stream, pos));

	return 0;
}

static int faslwrite_value_array_normal_(Execute ptr, addr stream, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Return(faslwrite_value_array_struct_(stream, str));
	Return(faslwrite_value_array_info_(ptr, stream, pos));
	Return(faslwrite_value_array_body_(ptr, stream, pos));

	return 0;
}

int faslwrite_value_array_(Execute ptr, addr stream, addr pos)
{
	struct array_struct *str;

	CheckType(pos, LISPTYPE_ARRAY);
	Return(faslwrite_type_status_(stream, pos, FaslCode_array));
	str = ArrayInfoStruct(pos);
	if (str->displaced)
		return faslwrite_value_array_displaced_(ptr, stream, pos);
	else
		return faslwrite_value_array_normal_(ptr, stream, pos);
}


/*
 *  array-read
 */
int faslread_value_array_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	struct array_struct *str;

	Return(faslread_status_(stream, &status));
	array_empty_heap(&pos);
	str = ArrayInfoStruct(pos);
	Return(faslread_value_array_struct_(stream, str));
	Return(faslread_value_array_info_(ptr, stream, pos));
	Return(faslread_value_array_body_(ptr, stream, pos));
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/************************************************************
 *  compile_eval.c
 ************************************************************/

/*
 *  eval-when check
 */
static int compile_eval_execute_p_(Execute ptr, int *ret)
{
	return load_toplevel_p_eval_(ptr, ret);
}

static int compile_eval_compile_p_(Execute ptr, int *ret)
{
	Return(compile_toplevel_p_eval_(ptr, ret));
	if (*ret)
		return 0;

	return compile_time_too_eval_(ptr, ret);
}


/*
 *  execute
 */
static int compile_eval_execute_call_(Execute ptr, addr pos, addr *rtype)
{
	int check;
	LocalHold hold;

	/* parse */
	hold = LocalHold_array(ptr, 2);
	localhold_push(hold, pos);
	Return(parse_execute_(ptr, &pos, pos));

	/* optimize parse */
	localhold_set(hold, 0, pos);
	Return(optimize_parse_(ptr, pos, &pos, NULL));

	/* scope */
	localhold_set(hold, 0, pos);
	Return(eval_scope_(ptr, &pos, pos));
	if (rtype) {
		GetEvalScopeThe(pos, rtype);
		localhold_set(hold, 1, *rtype);
	}

	/* code */
	localhold_set(hold, 0, pos);
	Return(code_make_(ptr, &pos, pos));

	/* close *parse-declare* */
	set_parse_declare(ptr, Nil);

	/* load-value */
	localhold_set(hold, 0, pos);
	Return(load_value_code_(ptr, pos));

	/* execute */
	Return(compile_eval_execute_p_(ptr, &check));
	if (check) {
		localhold_set(hold, 0, pos);
		Return(eval_compile_file_(ptr, pos));
	}

	/* :compile-toplevel */
	Return(compile_eval_compile_p_(ptr, &check));
	if (check) {
		localhold_set(hold, 0, pos);
		Return(runcode_control_(ptr, pos));
	}
	localhold_end(hold);

	return 0;
}

static int compile_eval_execute_(Execute ptr, addr pos, addr *rtype)
{
	addr control;

	push_control(ptr, &control);
	(void)compile_eval_execute_call_(ptr, pos, rtype);
	return pop_control_(ptr, control);
}

static int compile_eval_push_output_(Execute ptr, addr *ret)
{
	addr symbol, stream;

	GetConst(SYSTEM_COMPILE_OUTPUT, &symbol);
	Return(open_io_memory_stream_(&stream, Nil, 0, 0, 0));
	pushspecial_control(ptr, symbol, stream);

	return Result(ret, stream);
}

static int compile_eval_value_call_(Execute ptr, addr pos)
{
	addr stream;

	/* push */
	Return(compile_eval_push_output_(ptr, &stream));
	begin_load_push(ptr);

	/* execute */
	Return(compile_eval_execute_(ptr, pos, NULL));
	Return(load_depend_code_(ptr, stream, pos));

	return 0;
}

int compile_eval_value_(Execute ptr, addr pos)
{
	addr control;

	push_control(ptr, &control);
	(void)compile_eval_value_call_(ptr, pos);
	return pop_control_(ptr, control);
}


/*
 *  write
 */
static int compile_output_break_(Execute ptr)
{
	addr pos;

	GetConst(SYSTEM_COMPILE_OUTPUT, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	return faslwrite_break_(pos);
}

static int compile_eval_output_(Execute ptr)
{
	addr pos;
	LocalHold hold;
	size_t size;

	hold = LocalHold_array(ptr, 1);
	Return(get_load_size_(ptr, &pos));
	GetIndex(pos, &size);
	if (size) {
		code_make_load_alloc(ptr, &pos, pos);
		localhold_set(hold, 0, pos);
		Return(eval_compile_file_(ptr, pos));
		Return(compile_output_break_(ptr));
	}
	localhold_end(hold);

	return 0;
}

static int compile_gensym_output_(Execute ptr)
{
	addr pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	Return(list_load_gensym_(ptr, &pos));
	if (pos != Nil) {
		code_make_load_gensym(ptr, &pos, pos);
		localhold_set(hold, 0, pos);
		Return(eval_compile_file_(ptr, pos));
		Return(compile_output_break_(ptr));
	}
	localhold_end(hold);

	return 0;
}

static int compile_depend_output_(Execute ptr)
{
	addr stream, depend;

	GetConst(SYSTEM_COMPILE_OUTPUT, &stream);
	Return(getspecialcheck_local_(ptr, stream, &stream));
	Return(get_depend_root_(ptr, &depend));
	return compile_depend_make_(ptr, stream, depend);
}


/*
 *  start
 */
static int compile_eval_start_(Execute ptr, addr stream)
{
	Return(eval_toplevel_loop_(ptr, stream));
	/* write */
	Return(compile_eval_output_(ptr));
	Return(compile_gensym_output_(ptr));
	Return(compile_depend_output_(ptr));

	return 0;
}


/*
 *  interface
 */
static int compile_eval_call_(Execute ptr, addr stream)
{
	init_parse_environment(ptr);
	push_toplevel_eval(ptr, T);
	push_compile_time_eval(ptr, Nil);
	push_compile_toplevel_eval(ptr, Nil);
	push_load_toplevel_eval(ptr, T);
	push_execute_eval(ptr, T);
	push_parse_declare(ptr, Nil);
	init_load_time_value(ptr);

	return compile_eval_start_(ptr, stream);
}

int compile_load_stream_(Execute ptr, addr stream)
{
	addr control;

	push_control(ptr, &control);
	(void)compile_eval_call_(ptr, stream);
	return pop_control_(ptr, control);
}


/*
 *  partial
 */
static int compile_partial_make_call_(Execute ptr, addr pos, addr *ret, addr *rtype)
{
	addr stream;

	/* push */
	Return(compile_eval_push_output_(ptr, &stream));
	begin_load_push(ptr);

	/* execute */
	GetTypeTable(rtype, T);
	Return(compile_eval_execute_(ptr, pos, rtype));
	return load_depend_partial_(ptr, stream, pos, ret);
}

static int compile_partial_make_(Execute ptr, addr pos, addr *ret, addr *rtype)
{
	addr control;

	push_control(ptr, &control);
	(void)compile_partial_make_call_(ptr, pos, ret, rtype);
	return pop_control_(ptr, control);
}

int compile_partial_(Execute ptr, addr pos, addr *ret, addr *rtype)
{
	addr depend;

	Return(compile_partial_make_(ptr, pos, &depend, rtype));
	get_index_load_depend(depend, ret);
	return push_load_push_(ptr, depend);
}


/*
 *  instance single
 */
static int compile_instance_execute_call_(Execute ptr, addr *ret, addr pos, addr index)
{
	addr stream, depend;

	/* push */
	Return(compile_eval_push_output_(ptr, &stream));
	begin_load_push(ptr);

	/* execute */
	Return(compile_eval_execute_(ptr, pos, NULL));

	/* depend */
	load_depend_heap(&depend, stream, pos, index);
	Return(end_load_push_(ptr, depend));

	return Result(ret, depend);
}

static int compile_instance_execute_(Execute ptr, addr *ret, addr pos, addr index)
{
	addr control;

	push_control(ptr, &control);
	gchold_push_special(ptr, pos);
	(void)compile_instance_execute_call_(ptr, ret, pos, index);
	return pop_control_(ptr, control);
}

static int compile_instance_instance_(Execute ptr, addr *ret, addr instance, addr init)
{
	addr funcall;

	GetConst(COMMON_FUNCALL, &funcall);
	list_heap(&init, funcall, init, instance, NULL);
	return compile_instance_execute_(ptr, ret, init, Nil);
}

int compile_instance_(Execute ptr, addr pos, addr make, addr init)
{
	addr instance, index;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_push(hold, pos);
	localhold_push(hold, make);
	localhold_push(hold, init);

	/* instance */
	Return(incf_load_size_(ptr, &index));
	Return(intern_load_table_(ptr, pos, index));
	load_depend_heap(&instance, Nil, pos, Nil);
	localhold_push(hold, instance);

	/* form */
	Return(compile_instance_execute_(ptr, &make, make, index));
	localhold_push(hold, make);
	if (init != Nil) {
		Return(compile_instance_instance_(ptr, &init, pos, init));
		localhold_push(hold, init);
	}

	Return(load_depend_instance_(ptr, instance, make, init));
	Return(push_load_push_(ptr, instance));
	localhold_end(hold);

	return 0;
}


/************************************************************
 *  compile_file.c
 ************************************************************/

void set_eval_compile_mode(Execute ptr, addr value)
{
	addr pos;

	GetConst(SYSTEM_COMPILE_CODE, &pos);
	pushspecial_control(ptr, pos, value);
}

int eval_compile_p(Execute ptr)
{
	addr pos;

	GetConst(SYSTEM_COMPILE_CODE, &pos);
	getspecial_local(ptr, pos, &pos);
	return (pos != Nil) && (pos != Unbound);
}

int eval_compile_file_(Execute ptr, addr pos)
{
	addr stream;

	Check(GetType(pos) != LISPTYPE_CODE, "type error");
	GetConst(SYSTEM_COMPILE_OUTPUT, &stream);
	Return(getspecialcheck_local_(ptr, stream, &stream));

	return faslwrite_value_(ptr, stream, pos);
}


/*
 *  handler-compile
 */
static int function_handler_compile(Execute ptr, addr condition)
{
	int check;
	addr value;

	/* warning */
	GetConst(CONDITION_WARNING, &value);
	Return(clos_subtype_p_(condition, value, &check));
	if (check) {
		GetConst(SYSTEM_COMPILE_WARNING, &value);
		setspecial_local(ptr, value, T);
	}

	/* style-warning */
	GetConst(CONDITION_STYLE_WARNING, &value);
	Return(clos_subtype_p_(condition, value, &check));
	if (check) {
		GetConst(SYSTEM_COMPILE_STYLE_WARNING, &value);
		setspecial_local(ptr, value, T);
	}

	return 0;
}

int handler_compile_(Execute ptr)
{
	addr pos, call;

	/* enable-compiler-macro */
	push_enable_compiler_macro(ptr, T);

	/* compile-warning */
	GetConst(SYSTEM_COMPILE_WARNING, &pos);
	pushspecial_control(ptr, pos, Nil);

	/* compile-style-warning */
	GetConst(SYSTEM_COMPILE_STYLE_WARNING, &pos);
	pushspecial_control(ptr, pos, Nil);

	/* handler-bind */
	GetConst(CONDITION_WARNING, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_compile);
	return pushhandler_common_(ptr, pos, call, 0);
}


/*
 *  compile-file
 */
struct compile_file_struct {
	unsigned warnings_p : 1;
	unsigned failure_p : 1;
	Execute ptr;
	LocalHold hold;
	addr input_file, output_file;
	addr input, output, verbose, print, format;
	addr result;
};

static int compile_file_write_(Execute ptr, struct compile_file_struct *str)
{
	addr input, output, verbose, print, format;

	Check(! streamp(str->input), "input error");
	Check(! streamp(str->output), "output error");
	input = str->input;
	output = str->output;
	verbose = str->verbose;
	print = str->print;
	format = str->format;

	/* fasl */
	Return(faslwrite_header_(output));
	Return(compile_load_(ptr, input, verbose, print, format));
	Return(faslwrite_footer_(output));
	return finish_output_stream_(output);
}

static int compile_file_execute_(Execute ptr, struct compile_file_struct *str)
{
	addr output, symbol, pos;

	Check(! streamp(str->input), "input error");
	Check(! streamp(str->output), "output error");
	output = str->output;

	/* variable */
	GetConst(SYSTEM_COMPILE_OUTPUT, &symbol);
	pushspecial_control(ptr, symbol, output);
	set_eval_compile_mode(ptr, T);

	/* compile */
	Return(compile_file_write_(ptr, str));
	GetPathnameStream(output, &pos);
	if (memory_stream_p(pos)) {
		pos = Nil;
	}
	else {
		Return(truename_files_(ptr, output, &pos, 0));
	}
	str->result = pos;
	localhold_set(str->hold, 0, pos);

	return 0;
}

static int compile_file_stream_p(addr stream)
{
	return streamp(stream) && (! memory_stream_p(stream));
}

static int compile_file_output_call_(Execute ptr, struct compile_file_struct *str)
{
	addr stream, output;

	output = str->output_file;
	Return(open_output_binary_stream_(ptr, &stream, output, FileOutput_supersede));
	if (stream == NULL) {
		return call_simple_file_error_va_(ptr, output,
				"Cannot open the output file, ~S.", output, NULL);
	}
	str->output = stream;
	(void)compile_file_execute_(ptr, str);
	return close_stream_unwind_protect_(ptr, stream);
}

static int compile_file_output_(Execute ptr, struct compile_file_struct *str)
{
	addr output, control;

	output = str->output_file;
	if (compile_file_stream_p(output)) {
		str->output = output;
		return compile_file_execute_(ptr, str);
	}

	/* open output */
	push_control(ptr, &control);
	(void)compile_file_output_call_(ptr, str);
	return pop_control_(ptr, control);
}

static int compile_file_input_call_(Execute ptr, struct compile_file_struct *str)
{
	addr stream, input, format;

	input = str->input_file;
	format = str->format;
	Return(open_input_stream_(ptr, &stream, input, format));
	if (stream == NULL) {
		return call_simple_file_error_va_(ptr, input,
				"Cannot open the input file, ~S.", input, NULL);
	}
	str->input = stream;
	(void)compile_file_output_(ptr, str);
	return close_stream_unwind_protect_(ptr, stream);
}

static int compile_file_input_(Execute ptr, struct compile_file_struct *str)
{
	addr input, control;

	input = str->input_file;
	if (compile_file_stream_p(input)) {
		str->input = input;
		return compile_file_output_(ptr, str);
	}

	/* open input */
	push_control(ptr, &control);
	(void)compile_file_input_call_(ptr, str);
	return pop_control_(ptr, control);
}

static int compile_file_handler_(Execute ptr, struct compile_file_struct *str)
{
	int check1, check2;
	addr pos;

	Return(handler_compile_(ptr));
	Return(compile_file_input_(ptr, str));

	/* warning */
	GetConst(SYSTEM_COMPILE_WARNING, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	check1 = (pos != Nil);

	/* style-warning */
	GetConst(SYSTEM_COMPILE_STYLE_WARNING, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	check2 = (pos != Nil);

	/* result */
	str->warnings_p = check1;
	str->failure_p = (check1 != 0) && (check2 == 0);

	return 0;
}

static int compile_file_call_(Execute ptr, struct compile_file_struct *str)
{
	addr control;
	LocalHold hold;

	hold = str->hold;
	localhold_set(hold, 1, str->input_file);
	localhold_set(hold, 2, str->output_file);
	localhold_set(hold, 3, str->input);
	localhold_set(hold, 4, str->output);
	push_control(ptr, &control);
	(void)compile_file_handler_(ptr, str);
	return pop_control_(ptr, control);
}

int compile_file_common_(Execute ptr, addr input, addr rest,
		addr *ret1, addr *ret2, addr *ret3)
{
	addr pos;
	LocalHold hold;
	struct compile_file_struct str;

	str.ptr = ptr;
	str.warnings_p = 0;
	str.failure_p = 0;
	str.input_file = input;
	str.input = Nil;
	str.output = Nil;
	str.result = Unbound;

	/* verbose */
	if (GetKeyArgs(rest, KEYWORD_VERBOSE, &pos))
		pos = Unbound;
	str.verbose = pos;

	/* print */
	if (GetKeyArgs(rest, KEYWORD_PRINT, &pos))
		pos = Unbound;
	str.print = pos;

	/* external-format */
	if (GetKeyArgs(rest, KEYWORD_EXTERNAL_FORMAT, &pos))
		pos = Unbound;
	str.format = pos;

	/* output */
	Return(compile_file_pathname_common_(ptr, input, rest, &pos));
	str.output_file = pos;

	/* call */
	hold = LocalHold_array(ptr, 5);
	str.hold = hold;
	Return(compile_file_call_(ptr, &str));
	localhold_end(hold);

	/* result */
	Check(str.result == Unbound, "result error");
	*ret1 = str.result;
	*ret2 = str.warnings_p? T: Nil;
	*ret3 = str.failure_p? T: Nil;

	return 0;
}


/*
 *  initialize
 */
void init_compile_file(void)
{
	SetPointerCall(defun, var1, handler_compile);
}


/************************************************************
 *  compile_load.c
 ************************************************************/

static int eval_compile_load_toplevel_(Execute ptr, addr stream, int *ret)
{
	enum FaslCode type;
	addr code;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (;;) {
		Return(faslread_type_(stream, &type));
		if (type == FaslCode_break) {
			*ret = 0;
			break;
		}
		if (type == FaslCode_eof) {
			*ret = 1;
			break;
		}
		Return(unread_byte_stream_(stream, (byte)type));

		Return(faslread_value_(ptr, stream, &code));
		localhold_set(hold, 0, code);
		CheckType(code, LISPTYPE_CODE);
		Return(runcode_control_(ptr, code));
	}
	localhold_end(hold);

	return 0;
}

static int eval_compile_load_loop_(Execute ptr, addr stream)
{
	int check;

	check = 0;
	for (;;) {
		Return(eval_compile_load_toplevel_(ptr, stream, &check));
		if (check)
			break;
	}

	return 0;
}

static int eval_compile_load_call_(Execute ptr, addr stream)
{
	int check;

	/* header */
	Return(faslread_header_(stream, &check));
	if (check)
		return fmte_("Invalid fasl header.", NULL);

	/* fasl body */
	Return(eval_compile_load_loop_(ptr, stream));

	/* footer */
	Return(faslread_footer_(stream, &check));
	if (check)
		return fmte_("Invalid fasl footer.", NULL);

	return 0;
}

int eval_compile_load_(Execute ptr, addr stream)
{
	addr control;

	push_control(ptr, &control);
	fasl_load_time_value(ptr);
	(void)eval_compile_load_call_(ptr, stream);
	return pop_control_(ptr, control);
}


/************************************************************
 *  compile_read.c
 ************************************************************/

/*
 *  read function
 */
static int faslread_magic_(addr stream, int *ret)
{
	char a[16], b[16];

	/* 0000FASL */
	Return(faslread_buffer_(stream, a, 8));
	if (memcmp(a, "\0\0\0\0FASL", 8) != 0)
		goto error;

	/* LISPNAME */
	Return(faslread_buffer_(stream, a, 8));
	memset(b, 0, 8);
	strncpy(b, LISPNAME, 8);
	if (memcmp(a, b, 8) != 0)
		goto error;

	/* OK */
	return Result(ret, 0);

error:
	return Result(ret, 1);
}

int faslread_header_(addr input, int *ret)
{
	int check;
	byte buffer[64];
	uint8_t x;
	uint16_t v, a, b, c;

	/* 0: magic number */
	Return(faslread_magic_(input, &check));
	if (check)
		goto error;
	/* 16: endian */
	Return(faslread_variable_(input, v, &check));
	if (check)
		goto error;
	if (v != 1) {
		Debug("endian error.");
		goto error;
	}
	/* 18: version */
	Return(faslread_variable_(input, a, &check));
	if (check)
		goto error;
	Return(faslread_variable_(input, b, &check));
	if (check)
		goto error;
	Return(faslread_variable_(input, c, &check));
	if (check)
		goto error;
	if (a != LISP_VERSION_A || b != LISP_VERSION_B || c != LISP_VERSION_C)
		goto error;
	/* 24: CPU arch */
	Return(faslread_variable_(input, x, &check));
	if (check)
		goto error;
#ifdef LISP_ARCH_64BIT
	if (x != 1) {
		Debug("This fasl file is not 64bit arch.");
		goto error;
	}
#else
	if (x != 0) {
		Debug("This fasl file is not 32bit arch.");
		goto error;
	}
#endif
	/* 25: fixnum size */
	Return(faslread_variable_(input, x, &check));
	if (check)
		goto error;
#ifdef LISP_64BIT
	if (x != 1) {
		Debug("This fasl file is not 64bit fixnum.");
		goto error;
	}
#else
	if (x != 0) {
		Debug("This fasl file is not 32bit fixnum.");
		goto error;
	}
#endif
	/* 26: padding */
	Return(faslread_buffer_check_(input, buffer, 6, &check));
	if (check)
		goto error;
	/* 32: end */
	return Result(ret, 0);

error:
	return Result(ret, 1);
}

int faslread_footer_(addr input, int *ret)
{
	byte buffer[8];

	/* zero */
	Return(faslread_buffer_(input, buffer, 8));
	if (memcmp(buffer, "\x00\x00\x00\x00" "\x00\x00\x00\x00", 8) != 0)
		goto error;

	/* fill */
	Return(faslread_buffer_(input, buffer, 8));
	if (memcmp(buffer, "\xFF\xFF\xFF\xFF" "\xFF\xFF\xFF\xFF", 8) != 0)
		goto error;

	/* ok */
	return Result(ret, 0);

error:
	return Result(ret, 1);
}


/*
 *  system
 */
static int faslread_error_(Execute ptr, addr stream, addr *ret)
{
	return fmte_("fasl read error.", NULL);
}

static int faslread_unbound_(Execute ptr, addr stream, addr *ret)
{
	return Result(ret, Unbound);
}


/*
 *  code
 */
static int faslread_code_operator_(Execute ptr, addr stream, addr *ret)
{
	enum FaslCode type;
	constindex index;
	addr car, cdr;

	/* type */
	Return(faslread_type_(stream, &type));
	Check(type < FaslCode_value, "type error");
	index = GetCompileRead(type);
	GetConstant(index, &car);

	/* result */
	Return(faslread_value_(ptr, stream, &cdr));
	cons_heap(ret, car, cdr);

	return 0;
}

static int faslread_value_code_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr vector, pos;
	size_t size, i;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));

	/* code */
	vector4_heap(&vector, size);
	for (i = 0; i < size; i++) {
		Return(faslread_code_operator_(ptr, stream, &pos));
		SetArrayA4(vector, i, pos);
	}

	/* object */
	code_heap(&pos, vector);
	faslread_status_update(pos, status);
	update_code(pos);

	return Result(ret, pos);
}


/*
 *  code input
 */
typedef int (*faslread_calltype)(Execute, addr, addr *);
static faslread_calltype FaslRead_Table[FaslCode_value];

int faslread_value_(Execute ptr, addr stream, addr *ret)
{
	enum FaslCode type;
	faslread_calltype call;

	Return(faslread_type_(stream, &type));
	Check(FaslCode_value <= type, "type error");
	call = FaslRead_Table[type];
	Check(call == NULL, "faslread call error.");
	return (*call)(ptr, stream, ret);
}


/*
 *  initialize
 */
void init_compile_read(void)
{
	FaslRead_Table[FaslCode_error] = faslread_error_;
	FaslRead_Table[FaslCode_unbound] = faslread_unbound_;
	FaslRead_Table[FaslCode_clos] = faslread_value_clos_;
	FaslRead_Table[FaslCode_code] = faslread_value_code_;
	FaslRead_Table[FaslCode_nil] = faslread_value_nil_;
	FaslRead_Table[FaslCode_t] = faslread_value_t_;
	FaslRead_Table[FaslCode_type] = faslread_value_type_;
	FaslRead_Table[FaslCode_cons] = faslread_value_cons_;
	FaslRead_Table[FaslCode_array] = faslread_value_array_;
	FaslRead_Table[FaslCode_vector2] = faslread_value_vector2_;
	FaslRead_Table[FaslCode_vector4] = faslread_value_vector4_;
#ifdef LISP_ARCH_64BIT
	FaslRead_Table[FaslCode_vector8] = faslread_value_vector8_;
#endif
	FaslRead_Table[FaslCode_character] = faslread_value_character_;
	FaslRead_Table[FaslCode_character7] = faslread_value_character7_;
	FaslRead_Table[FaslCode_string] = faslread_value_string_;
	FaslRead_Table[FaslCode_string7] = faslread_value_string7_;
	FaslRead_Table[FaslCode_hashtable] = faslread_value_hashtable_;
	FaslRead_Table[FaslCode_gensym] = faslread_value_gensym_;
	FaslRead_Table[FaslCode_symbol] = faslread_value_symbol_;
	FaslRead_Table[FaslCode_fixnum] = faslread_value_fixnum_;
	FaslRead_Table[FaslCode_bignum] = faslread_value_bignum_;
	FaslRead_Table[FaslCode_ratio] = faslread_value_ratio_;
	FaslRead_Table[FaslCode_single_float] = faslread_value_single_float_;
	FaslRead_Table[FaslCode_double_float] = faslread_value_double_float_;
	FaslRead_Table[FaslCode_long_float] = faslread_value_long_float_;
	FaslRead_Table[FaslCode_complex] = faslread_value_complex_;
	FaslRead_Table[FaslCode_callname] = faslread_value_callname_;
	FaslRead_Table[FaslCode_index] = faslread_value_index_;
	FaslRead_Table[FaslCode_package] = faslread_value_package_;
	FaslRead_Table[FaslCode_random_state] = faslread_value_random_state_;
	FaslRead_Table[FaslCode_pathname] = faslread_value_pathname_;
	FaslRead_Table[FaslCode_quote] = faslread_value_quote_;
	FaslRead_Table[FaslCode_bitvector] = faslread_value_bitvector_;
	FaslRead_Table[FaslCode_load] = faslread_value_load_time_value_;
	FaslRead_Table[FaslCode_paper] = faslread_value_paper_;
}


/************************************************************
 *  compile_stream.c
 ************************************************************/

/*
 *  buffer
 */
int faslwrite_buffer_(addr stream, const void *ptr, size_t size)
{
	size_t check;

	Return(write_binary_stream_(stream, ptr, size, &check));
	if (size != check)
		return fmte_("write-binary-stream size error.", NULL);

	return 0;
}

int faslread_buffer_check_(addr stream, void *ptr, size_t size, int *ret)
{
	size_t check;
	Return(read_binary_stream_(stream, ptr, size, &check));
	return Result(ret, (size != check));
}

int faslread_buffer_(addr stream, void *ptr, size_t size)
{
	int check;

	Return(faslread_buffer_check_(stream, ptr, size, &check));
	if (check)
		return fmte_("readforce-binary-stream error.", NULL);

	return 0;
}


/*
 *  type
 */
int faslwrite_type_(addr stream, enum FaslCode code)
{
	return write_unsigned8_stream_(stream, (byte)code);
}

int faslread_type_(addr stream, enum FaslCode *ret)
{
	int check;
	byte c;

	Return(read_unsigned8_stream_(stream, &c, &check));
	if (check) {
		*ret = FaslCode_error;
		return fmte_("read-byte-stream error.", NULL);
	}

	return Result(ret, (enum FaslCode)c);
}

int faslread_type_check_(addr stream, enum FaslCode value)
{
	enum FaslCode check;

	Return(faslread_type_(stream, &check));
	if (check != value)
		return fmte_("Invalid fasl format.", NULL);

	return 0;
}


/*
 *  status
 */
int faslwrite_status_(addr stream, addr pos)
{
	byte status, user;

	status = GetStatus(pos);
	user = GetUser(pos);
	status &= (1 << LISPSTATUS_READONLY);
	Return(faslwrite_byte_(stream, status));
	Return(faslwrite_byte_(stream, user));

	return 0;
}

int faslwrite_type_status_(addr stream, addr pos, enum FaslCode code)
{
	Return(faslwrite_type_(stream, code));
	Return(faslwrite_status_(stream, pos));

	return 0;
}

int faslread_status_(addr stream, FaslStatus *ret)
{
	byte status, user;

	Return(faslread_byte_(stream, &status));
	Return(faslread_byte_(stream, &user));
	if (ret) {
		ret->status = status;
		ret->user = user;
	}

	return 0;
}

void faslread_status_update(addr pos, FaslStatus v)
{
	if (v.status & (1 << LISPSTATUS_READONLY)) {
		SetStatusReadOnly(pos);
	}
	SetUser(pos, v.user);
}


/*
 *  byte
 */
int faslwrite_byte_(addr stream, byte value)
{
	return write_unsigned8_stream_(stream, value);
}

int faslread_byte_(addr stream, byte *ret)
{
	int check;

	Return(read_unsigned8_stream_(stream, ret, &check));
	if (check) {
		*ret = 0;
		return fmte_("read-byte-stream error.", NULL);
	}

	return 0;
}


/*
 *  size
 */
int faslwrite_size_(addr stream, size_t value)
{
	uint8_t v1;
	uint16_t v2;
	uint32_t v4;
#ifdef LISP_ARCH_64BIT
	uint64_t v8;
#endif

	if (value <= 0xFFUL) {
		v1 = (uint8_t)(value & 0xFFUL);
		Return(faslwrite_byte_(stream, sizeoft(v1)));
		return faslwrite_buffer_(stream, &v1, sizeoft(v1));
	}
	if (value <= 0xFFFFUL) {
		v2 = (uint16_t)(value & 0xFFFFUL);
		Return(faslwrite_byte_(stream, sizeoft(v2)));
		return faslwrite_buffer_(stream, &v2, sizeoft(v2));
	}
#ifdef LISP_ARCH_64BIT
	if (value <= 0xFFFFFFFFUL) {
		v4 = (uint32_t)(value & 0xFFFFFFFFUL);
		Return(faslwrite_byte_(stream, sizeoft(v4)));
		return faslwrite_buffer_(stream, &v4, sizeoft(v4));
	}
	v8 = (uint64_t)value;
	Return(faslwrite_byte_(stream, sizeoft(v8)));
	return faslwrite_buffer_(stream, &v8, sizeoft(v8));
#else
	v4 = (uint32_t)value;
	Return(faslwrite_byte_(stream, sizeoft(v4)));
	return faslwrite_buffer_(stream, &v4, sizeoft(v4));
#endif
}

int faslread_size_(addr stream, size_t *ret)
{
	byte size;
	uint8_t v1;
	uint16_t v2;
	uint32_t v4;
#ifdef LISP_ARCH_64BIT
	uint64_t v8;
#endif
	addr pos;

	Return(faslread_byte_(stream, &size));
	switch (size) {
		case 1:
			Return(faslread_buffer_(stream, &v1, size));
			return Result(ret, (size_t)v1);

		case 2:
			Return(faslread_buffer_(stream, &v2, size));
			return Result(ret, (size_t)v2);

		case 4:
			Return(faslread_buffer_(stream, &v4, size));
			return Result(ret, (size_t)v4);

#ifdef LISP_ARCH_64BIT
		case 8:
			Return(faslread_buffer_(stream, &v8, size));
			return Result(ret, (size_t)v8);
#endif

		default:
			*ret = 0;
			fixnum_heap(&pos, (fixnum)size);
			return fmte_("Invalid size_t length, ~A.", pos, NULL);
	}
}


/************************************************************
 *  compile_type.c
 ************************************************************/

/*
 *  clos
 */
static int faslwritetype_clos_(Execute ptr, addr stream, addr pos)
{
	GetArrayType(pos, 0, &pos);
	if (type_asterisk_p(pos)) {
		GetConst(COMMON_ASTERISK, &pos);
	}
	else {
		Return(stdget_class_name_(pos, &pos));
	}

	return faslwrite_value_symbol_(ptr, stream, pos);
}

static int faslreadtype_clos_(Execute ptr, addr stream, addr pos)
{
	addr value, check;

	Return(faslread_type_check_(stream, FaslCode_symbol));
	Return(faslread_value_symbol_(ptr, stream, &value));
	/* asterisk check */
	GetConst(COMMON_ASTERISK, &check);
	if (value != check) {
		Return(clos_find_class_(value, &value));
	}
	SetArrayType(pos, 0, value);

	return 0;
}


/*
 *  object
 */
static int faslwritetype_object_(Execute ptr, addr stream, addr pos, size_t size)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		GetArrayType(pos, i, &value);
		Return(faslwrite_value_(ptr, stream, value));
	}

	return 0;
}

static int faslreadtype_object_(Execute ptr, addr stream, addr pos, size_t size)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		Return(faslread_value_(ptr, stream, &value));
		SetArrayType(pos, i, value);
	}

	return 0;
}


/*
 *  interface
 */
int faslwrite_value_type_(Execute ptr, addr stream, addr pos)
{
	size_t size;

	CheckType(pos, LISPTYPE_TYPE);
	LenArrayType(pos, &size);
	Return(faslwrite_type_status_(stream, pos, FaslCode_type));
	Return(faslwrite_byte_(stream, (byte)LowLispDecl(pos)));
	Return(faslwrite_size_(stream, size));

	switch (RefLispDecl(pos)) {
		case LISPDECL_CLOS:
			return faslwritetype_clos_(ptr, stream, pos);

		default:
			return faslwritetype_object_(ptr, stream, pos, size);
	}
}

int faslread_value_type_(Execute ptr, addr stream, addr *ret)
{
	byte decl;
	FaslStatus status;
	addr pos;
	size_t size;

	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &decl));
	Return(faslread_size_(stream, &size));
	type_heap(&pos, (enum LISPDECL)decl, size);

	switch (RefLispDecl(pos)) {
		case LISPDECL_CLOS:
			Return(faslreadtype_clos_(ptr, stream, pos));
			break;

		default:
			Return(faslreadtype_object_(ptr, stream, pos, size));
			break;
	}
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/************************************************************
 *  compile_typedef.c
 ************************************************************/

enum FaslCode CompileWrite[p_size_code];
constindex CompileRead[FaslCode_size];

#define defwrite(x) (CompileWrite[p_##x##_code] = FaslCode_##x)
#define defread(x,y) (CompileRead[FaslCode_##x] = CONSTANT_CODE_##y)

static void init_compile_typedef_write(void)
{
	defwrite(nop);
	defwrite(begin);
	defwrite(begin_call);
	defwrite(end);
	defwrite(escape);
	defwrite(escape_not);
	defwrite(save);
	defwrite(restore);
	defwrite(normal);
	defwrite(revert);
	defwrite(revert_goto);

	defwrite(set);
	defwrite(push);
	defwrite(push_result);
	defwrite(push_values);
	defwrite(nil_set);
	defwrite(nil_push);
	defwrite(t_set);
	defwrite(t_push);

	defwrite(lexical);
	defwrite(lexical_set);
	defwrite(lexical_push);
	defwrite(lexical_rem);
	defwrite(special_set);
	defwrite(special_push);
	defwrite(special_rem);

	defwrite(declaim_special);
	defwrite(declaim_type_value);
	defwrite(declaim_type_function);
	defwrite(declaim_inline);
	defwrite(declaim_notinline);
	defwrite(declaim_compilation);
	defwrite(declaim_debug);
	defwrite(declaim_safety);
	defwrite(declaim_space);
	defwrite(declaim_speed);
	defwrite(declaim_declaration);

	defwrite(type_result);
	defwrite(type_lexical);
	defwrite(type_special);
	defwrite(type_global);
	defwrite(type_function);
	defwrite(type_setf);
	defwrite(let_lexical);
	defwrite(let_special);
	defwrite(leta_special);

	defwrite(setq_lexical);
	defwrite(setq_special);
	defwrite(setq_global);

	defwrite(function_set);
	defwrite(function_push);
	defwrite(setf_set);
	defwrite(setf_push);

	defwrite(defmacro);
	defwrite(deftype);
	defwrite(define_compiler_macro);
	defwrite(defun);

	defwrite(call_name);
	defwrite(call_result);
	defwrite(call_type);
	defwrite(call_key);
	defwrite(call_function);
	defwrite(call_setf);
	defwrite(call_lexical);

	defwrite(values_nil);
	defwrite(values_set);
	defwrite(the_set);
	defwrite(the_push);

	defwrite(if_unbound);
	defwrite(if_nil);
	defwrite(if_t);
	defwrite(goto);
	defwrite(go);
	defwrite(return_from);
	defwrite(catch);
	defwrite(throw_operator);
	defwrite(taginfo);
	defwrite(blockinfo);

	defwrite(handler_bind);
	defwrite(handler_case);
	defwrite(restart_bind);
	defwrite(restart_case);
	defwrite(restart_progn);

	defwrite(funcall);
	defwrite(nth_value);
	defwrite(progv);

	defwrite(pop);
	defwrite(pop_unbound);
	defwrite(getf);
	defwrite(rest_copy);
	defwrite(rest_bind);
	defwrite(allow_other_keys);
	defwrite(rest_null);
	defwrite(whole);

	defwrite(lambda);
	defwrite(lambda_name);
	defwrite(lambda_type);
	defwrite(lambda_doc);
	defwrite(lambda_form);
	defwrite(lambda_defun);
	defwrite(lambda_closure);
	defwrite(lambda_lexical);
	defwrite(lambda_cache);
	defwrite(lambda_cache_set);
	defwrite(macro);
	defwrite(macro_special);
	defwrite(macro_env);
	defwrite(macro_whole);

	defwrite(labels_make);
	defwrite(labels_lambda);

	defwrite(bind1_type);
	defwrite(bind1_special);
	defwrite(bind1_lexical);
	defwrite(bind2_type);
	defwrite(bind2_special);
	defwrite(bind2_lexical);

	defwrite(load_alloc);
	defwrite(load_gensym);
	defwrite(load_set);
	defwrite(reference_set);
	defwrite(reference_push);

	defwrite(step);
	defwrite(step_off);
	defwrite(step_begin);
	defwrite(step_end);

	defwrite(optcode_result_type);
	defwrite(optcode_car0_set);
	defwrite(optcode_car0_push);
	defwrite(optcode_car1_set);
	defwrite(optcode_car1_push);
	defwrite(optcode_cdr0_set);
	defwrite(optcode_cdr0_push);
	defwrite(optcode_cdr1_set);
	defwrite(optcode_cdr1_push);
	defwrite(optcode_cons);
}

static void init_compile_typedef_read(void)
{
	defread(nop, NOP);
	defread(begin, BEGIN);
	defread(begin_call, BEGIN_CALL);
	defread(end, END);
	defread(escape, ESCAPE);
	defread(escape_not, ESCAPE_NOT);
	defread(save, SAVE);
	defread(restore, RESTORE);
	defread(normal, NORMAL);
	defread(revert, REVERT);
	defread(revert_goto, REVERT_GOTO);

	defread(set, SET);
	defread(push, PUSH);
	defread(push_result, PUSH_RESULT);
	defread(push_values, PUSH_VALUES);
	defread(nil_set, NIL_SET);
	defread(nil_push, NIL_PUSH);
	defread(t_set, T_SET);
	defread(t_push, T_PUSH);

	defread(lexical, LEXICAL);
	defread(lexical_set, LEXICAL_SET);
	defread(lexical_push, LEXICAL_PUSH);
	defread(lexical_rem, LEXICAL_REM);
	defread(special_set, SPECIAL_SET);
	defread(special_push, SPECIAL_PUSH);
	defread(special_rem, SPECIAL_REM);

	defread(declaim_special, DECLAIM_SPECIAL);
	defread(declaim_type_value, DECLAIM_TYPE_VALUE);
	defread(declaim_type_function, DECLAIM_TYPE_FUNCTION);
	defread(declaim_inline, DECLAIM_INLINE);
	defread(declaim_notinline, DECLAIM_NOTINLINE);
	defread(declaim_compilation, DECLAIM_COMPILATION);
	defread(declaim_debug, DECLAIM_DEBUG);
	defread(declaim_safety, DECLAIM_SAFETY);
	defread(declaim_space, DECLAIM_SPACE);
	defread(declaim_speed, DECLAIM_SPEED);
	defread(declaim_declaration, DECLAIM_DECLARATION);

	defread(type_result, TYPE_RESULT);
	defread(type_lexical, TYPE_LEXICAL);
	defread(type_special, TYPE_SPECIAL);
	defread(type_global, TYPE_GLOBAL);
	defread(type_function, TYPE_FUNCTION);
	defread(type_setf, TYPE_SETF);
	defread(let_lexical, LET_LEXICAL);
	defread(let_special, LET_SPECIAL);
	defread(leta_special, LETA_SPECIAL);

	defread(setq_lexical, SETQ_LEXICAL);
	defread(setq_special, SETQ_SPECIAL);
	defread(setq_global, SETQ_GLOBAL);

	defread(function_set, FUNCTION_SET);
	defread(function_push, FUNCTION_PUSH);
	defread(setf_set, SETF_SET);
	defread(setf_push, SETF_PUSH);

	defread(defmacro, DEFMACRO);
	defread(deftype, DEFTYPE);
	defread(define_compiler_macro, DEFINE_COMPILER_MACRO);
	defread(defun, DEFUN);

	defread(call_name, CALL_NAME);
	defread(call_result, CALL_RESULT);
	defread(call_type, CALL_TYPE);
	defread(call_key, CALL_KEY);
	defread(call_function, CALL_FUNCTION);
	defread(call_setf, CALL_SETF);
	defread(call_lexical, CALL_LEXICAL);

	defread(values_nil, VALUES_NIL);
	defread(values_set, VALUES_SET);
	defread(the_set, THE_SET);
	defread(the_push, THE_PUSH);

	defread(if_unbound, IF_UNBOUND);
	defread(if_nil, IF_NIL);
	defread(if_t, IF_T);
	defread(goto, GOTO);
	defread(go, GO);
	defread(return_from, RETURN_FROM);
	defread(catch, CATCH);
	defread(throw_operator, THROW);
	defread(taginfo, TAGINFO);
	defread(blockinfo, BLOCKINFO);

	defread(handler_bind, HANDLER_BIND);
	defread(handler_case, HANDLER_CASE);
	defread(restart_bind, RESTART_BIND);
	defread(restart_case, RESTART_CASE);
	defread(restart_progn, RESTART_PROGN);

	defread(funcall, FUNCALL);
	defread(nth_value, NTH_VALUE);
	defread(progv, PROGV);

	defread(pop, POP);
	defread(pop_unbound, POP_UNBOUND);
	defread(getf, GETF);
	defread(rest_copy, REST_COPY);
	defread(rest_bind, REST_BIND);
	defread(allow_other_keys, ALLOW_OTHER_KEYS);
	defread(rest_null, REST_NULL);
	defread(whole, WHOLE);

	defread(lambda, LAMBDA);
	defread(lambda_name, LAMBDA_NAME);
	defread(lambda_type, LAMBDA_TYPE);
	defread(lambda_doc, LAMBDA_DOC);
	defread(lambda_form, LAMBDA_FORM);
	defread(lambda_defun, LAMBDA_DEFUN);
	defread(lambda_closure, LAMBDA_CLOSURE);
	defread(lambda_lexical, LAMBDA_LEXICAL);
	defread(lambda_cache, LAMBDA_CACHE);
	defread(lambda_cache_set, LAMBDA_CACHE_SET);
	defread(macro, MACRO);
	defread(macro_special, MACRO_SPECIAL);
	defread(macro_env, MACRO_ENV);
	defread(macro_whole, MACRO_WHOLE);

	defread(labels_make, LABELS_MAKE);
	defread(labels_lambda, LABELS_LAMBDA);

	defread(bind1_type, BIND1_TYPE);
	defread(bind1_special, BIND1_SPECIAL);
	defread(bind1_lexical, BIND1_LEXICAL);
	defread(bind2_type, BIND2_TYPE);
	defread(bind2_special, BIND2_SPECIAL);
	defread(bind2_lexical, BIND2_LEXICAL);

	defread(load_alloc, LOAD_ALLOC);
	defread(load_gensym, LOAD_GENSYM);
	defread(load_set, LOAD_SET);
	defread(reference_set, REFERENCE_SET);
	defread(reference_push, REFERENCE_PUSH);

	defread(step, STEP);
	defread(step_off, STEP_OFF);
	defread(step_begin, STEP_BEGIN);
	defread(step_end, STEP_END);

	defread(optcode_result_type, OPTCODE_RESULT_TYPE);
	defread(optcode_car0_set, OPTCODE_CAR0_SET);
	defread(optcode_car0_push, OPTCODE_CAR0_PUSH);
	defread(optcode_car1_set, OPTCODE_CAR1_SET);
	defread(optcode_car1_push, OPTCODE_CAR1_PUSH);
	defread(optcode_cdr0_set, OPTCODE_CDR0_SET);
	defread(optcode_cdr0_push, OPTCODE_CDR0_PUSH);
	defread(optcode_cdr1_set, OPTCODE_CDR1_SET);
	defread(optcode_cdr1_push, OPTCODE_CDR1_PUSH);
	defread(optcode_cons, OPTCODE_CONS);
}
#undef defwrite
#undef defread

enum FaslCode get_compile_write(pointer id)
{
	Check(p_size_code <= id, "p_size_code error");
	return CompileWrite[id];
}

constindex get_compile_read(enum FaslCode id)
{
	return CompileRead[id];
}

void init_compile_typedef(void)
{
	init_compile_typedef_write();
	init_compile_typedef_read();
}


/************************************************************
 *  compile_value.c
 ************************************************************/

/*
 *  nil
 */
int faslwrite_value_nil_(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_NIL);
	Return(faslwrite_type_(stream, FaslCode_nil));
	return 0;
}

int faslread_value_nil_(Execute ptr, addr stream, addr *ret)
{
	return Result(ret, Nil);
}


/*
 *  t
 */
int faslwrite_value_t_(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_T);
	Return(faslwrite_type_(stream, FaslCode_t));
	return 0;
}

int faslread_value_t_(Execute ptr, addr stream, addr *ret)
{
	return Result(ret, T);
}


/*
 *  clos
 */
int faslwrite_value_clos_(Execute ptr, addr stream, addr pos)
{
	size_t index;

	CheckType(pos, LISPTYPE_CLOS);
	Return(get_index_load_table_(ptr, pos, &index));
	Return(faslwrite_type_status_(stream, pos, FaslCode_clos));
	Return(faslwrite_size_(stream, index));

	return 0;
}

int faslread_value_clos_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	size_t index;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &index));
	Return(execute_load_get_(ptr, index, &pos));
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  cons
 */
int faslwrite_value_cons_(Execute ptr, addr stream, addr pos)
{
	addr car, cdr;

	CheckType(pos, LISPTYPE_CONS);
	Return(faslwrite_type_status_(stream, pos, FaslCode_cons));
	GetCons(pos, &car, &cdr);
	Return(faslwrite_value_(ptr, stream, car));
	Return(faslwrite_value_(ptr, stream, cdr));

	return 0;
}

int faslread_value_cons_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos, car, cdr;

	Return(faslread_status_(stream, &status));
	Return(faslread_value_(ptr, stream, &car));
	Return(faslread_value_(ptr, stream, &cdr));
	cons_heap(&pos, car, cdr);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  vector
 */
static int faslwrite_value_vector2_(Execute ptr, addr stream, addr pos)
{
	addr value;
	size_t size, i;

	Return(faslwrite_type_status_(stream, pos, FaslCode_vector2));
	LenArrayA2(pos, &size);
	Return(faslwrite_size_(stream, size));
	for (i = 0; i < size; i++) {
		GetArrayA2(pos, i, &value);
		Return(faslwrite_value_(ptr, stream, value));
	}

	return 0;
}

static int faslwrite_value_vector4_(Execute ptr, addr stream, addr pos)
{
	addr value;
	size_t size, i;

	Return(faslwrite_type_status_(stream, pos, FaslCode_vector4));
	LenArrayA4(pos, &size);
	Return(faslwrite_size_(stream, size));
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &value);
		Return(faslwrite_value_(ptr, stream, value));
	}

	return 0;
}

#ifdef LISP_ARCH_64BIT
static int faslwrite_value_vector8_(Execute ptr, addr stream, addr pos)
{
	addr value;
	size_t size, i;

	Return(faslwrite_type_status_(stream, pos, FaslCode_vector8));
	LenArrayA8(pos, &size);
	Return(faslwrite_size_(stream, size));
	for (i = 0; i < size; i++) {
		GetArrayA8(pos, i, &value);
		Return(faslwrite_value_(ptr, stream, value));
	}

	return 0;
}
#endif

int faslwrite_value_vector_(Execute ptr, addr stream, addr pos)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			return faslwrite_value_vector2_(ptr, stream, pos);

		case LISPSIZE_ARRAY4:
			return faslwrite_value_vector4_(ptr, stream, pos);

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			return faslwrite_value_vector8_(ptr, stream, pos);
#endif

		default:
			return fmte_("Invalid vector size.", NULL);
	}
}

int faslread_value_vector2_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos, value;
	size_t size, i;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));
	vector2_heap(&pos, size);
	for (i = 0; i < size; i++) {
		Return(faslread_value_(ptr, stream, &value));
		SetArrayA2(pos, i, value);
	}
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

int faslread_value_vector4_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos, value;
	size_t size, i;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));
	vector4_heap(&pos, size);
	for (i = 0; i < size; i++) {
		Return(faslread_value_(ptr, stream, &value));
		SetArrayA4(pos, i, value);
	}
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

#ifdef LISP_ARCH_64BIT
int faslread_value_vector8_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos, value;
	size_t size, i;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));
	vector8_heap(&pos, size);
	for (i = 0; i < size; i++) {
		Return(faslread_value_(ptr, stream, &value));
		SetArrayA8(pos, i, value);
	}
	faslread_status_update(pos, status);

	return Result(ret, pos);
}
#endif


/*
 *  character
 */
int faslwrite_value_character_(Execute ptr, addr stream, addr pos)
{
	unicode value;

	CheckType(pos, LISPTYPE_CHARACTER);
	GetCharacter(pos, &value);
	if (value < 0x80) {
		Return(faslwrite_type_status_(stream, pos, FaslCode_character7));
		Return(faslwrite_byte_(stream, (byte)value));
	}
	else {
		Return(faslwrite_type_status_(stream, pos, FaslCode_character));
		Return(faslwrite_buffer_(stream, &value, sizeoft(value)));
	}

	return 0;
}

int faslread_value_character_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	unicode value;
	addr pos;

	Return(faslread_status_(stream, &status));
	Return(faslread_buffer_(stream, &value, sizeoft(value)));
	character_heap(&pos, value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

int faslread_value_character7_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	byte value;
	addr pos;

	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &value));
	character_heap(&pos, (unicode)value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  string
 */
static int faslwrite_value_string7_p(Execute ptr, addr pos)
{
	const unicode *data;
	size_t size, i;

	CheckType(pos, LISPTYPE_STRING);
	strvect_posbodylen(pos, &data, &size);
	for (i = 0; i < size; i++) {
		if (0x80 <= data[i])
			return 0;
	}

	return 1;
}

static int faslwrite_value_string7_(Execute ptr, addr stream, addr pos)
{
	const unicode *data;
	size_t size, i;

	CheckType(pos, LISPTYPE_STRING);
	Return(faslwrite_type_status_(stream, pos, FaslCode_string7));
	strvect_posbodylen(pos, &data, &size);
	/* write */
	Return(faslwrite_size_(stream, size));
	for (i = 0; i < size; i++) {
		Return(faslwrite_byte_(stream, (byte)data[i]));
	}

	return 0;
}

static int faslwrite_value_string32_(Execute ptr, addr stream, addr pos)
{
	const unicode *data;
	size_t size;

	CheckType(pos, LISPTYPE_STRING);
	Return(faslwrite_type_status_(stream, pos, FaslCode_string));
	strvect_posbodylen(pos, &data, &size);
	/* write */
	Return(faslwrite_size_(stream, size));
	Return(faslwrite_buffer_(stream, data, sizeoft(unicode) * size));

	return 0;
}

int faslwrite_value_string_(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_STRING);
	if (faslwrite_value_string7_p(ptr, pos))
		return faslwrite_value_string7_(ptr, stream, pos);
	else
		return faslwrite_value_string32_(ptr, stream, pos);
}

int faslread_value_string_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	unicode *data;
	size_t size;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));

	strvect_heap(&pos, size);
	GetStringUnicode(pos, &data);
	Return(faslread_buffer_(stream, data, sizeoft(unicode) * size));
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

int faslread_value_string7_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	byte c;
	addr pos;
	unicode *data;
	size_t size, i;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));

	strvect_heap(&pos, size);
	GetStringUnicode(pos, &data);
	for (i = 0; i < size; i++) {
		Return(faslread_byte_(stream, &c));
		data[i] = (unicode)c;
	}
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

static int faslread_string32_local_(LocalRoot local, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	unicode *data;
	size_t size;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));

	strvect_local(local, &pos, size);
	GetStringUnicode(pos, &data);
	Return(faslread_buffer_(stream, data, sizeoft(unicode) * size));
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

static int faslread_string7_local_(LocalRoot local, addr stream, addr *ret)
{
	FaslStatus status;
	byte c;
	addr pos;
	unicode *data;
	size_t size, i;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));

	strvect_local(local, &pos, size);
	GetStringUnicode(pos, &data);
	for (i = 0; i < size; i++) {
		Return(faslread_byte_(stream, &c));
		data[i] = (unicode)c;
	}
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

static int faslread_string_local_(LocalRoot local, addr stream, addr *ret)
{
	enum FaslCode code;

	Return(faslread_type_(stream, &code));
	switch (code) {
		case FaslCode_string:
			return faslread_string32_local_(local, stream, ret);

		case FaslCode_string7:
			return faslread_string7_local_(local, stream, ret);

		default:
			*ret = Nil;
			return fmte_("Invalid faslcode.", NULL);
	}
}


/*
 *  hashtable
 */
static int faslwrite_value_hashtable_struct_(addr stream,
		const struct StructHashtable *str)
{
	double_float dvalue;

	Return(faslwrite_byte_(stream, (byte)str->resize_float_p));
	Return(faslwrite_byte_(stream, (byte)str->expand_p));
	Return(faslwrite_byte_(stream, (byte)str->test));
	Return(faslwrite_size_(stream, str->count));
	Return(faslwrite_size_(stream, str->size));
	Return(faslwrite_size_(stream, str->limit));
	Return(faslwrite_size_(stream, str->resize_integer));
	dvalue = str->resize_float;
	Return(faslwrite_buffer_(stream, &dvalue, sizeoft(dvalue)));
	dvalue = str->threshold;
	Return(faslwrite_buffer_(stream, &dvalue, sizeoft(dvalue)));

	return 0;
}

static int faslread_value_hashtable_struct_(addr stream,
		struct StructHashtable *str)
{
	byte v;
	size_t size;
	double_float dvalue;

	Return(faslread_byte_(stream, &v));
	str->resize_float_p = (v != 0);
	Return(faslread_byte_(stream, &v));
	str->expand_p = (v != 0);
	Return(faslread_byte_(stream, &v));
	str->test = (enum HASHTABLE_TEST)v;
	Return(faslread_size_(stream, &size));
	str->count = size;
	Return(faslread_size_(stream, &size));
	str->size = size;
	Return(faslread_size_(stream, &size));
	str->limit = size;
	Return(faslread_size_(stream, &size));
	str->resize_integer = size;
	Return(faslread_buffer_(stream, &dvalue, sizeoft(dvalue)));
	str->resize_float = dvalue;
	Return(faslread_buffer_(stream, &dvalue, sizeoft(dvalue)));
	str->threshold = dvalue;

	return 0;
}

int faslwrite_value_hashtable_(Execute ptr, addr stream, addr pos)
{
	addr loop, key, value;
	struct StructHashtable *str;
	size_t i;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(faslwrite_type_status_(stream, pos, FaslCode_hashtable));
	/* struct */
	str = PtrStructHashtable(pos);
	Return(faslwrite_value_hashtable_struct_(stream, str));
	/* interator */
	hash_iterator_heap(&loop, pos);
	for (i = 0; next_hash_iterator(loop, &key, &value); i++) {
		Return(faslwrite_value_(ptr, stream, key));
		Return(faslwrite_value_(ptr, stream, value));
	}
	Check(str->count < i, "count error.");

	return 0;
}

int faslread_value_hashtable_(Execute ptr, addr stream, addr *ret)
{
	addr pos, key, value, cons;
	struct StructHashtable data, *str;
	size_t size, i;

	/* hashtable */
	Return(faslread_value_hashtable_struct_(stream, &data));
	data.count = 0;
	hashtable_size_heap(&pos, data.size);
	str = PtrStructHashtable(pos);
	*str = data;

	/* iterator */
	size = data.count;
	for (i = 0; i < size; i++) {
		Return(faslread_value_(ptr, stream, &key));
		Return(faslread_value_(ptr, stream, &value));
		Return(intern_hashheap_(pos, key, &cons));
		SetCdr(cons, value);
	}

	return Result(ret, pos);
}


/*
 *  gensym
 */
static int faslwrite_value_gensym_(Execute ptr, addr stream, addr pos)
{
	size_t index;

	Check(! gensymp(pos), "type error");
	Return(get_index_load_table_(ptr, pos, &index));
	Return(faslwrite_type_status_(stream, pos, FaslCode_gensym));
	Return(faslwrite_size_(stream, index));

	return 0;
}

int faslread_value_gensym_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	size_t index;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &index));
	Return(execute_load_get_(ptr, index, &pos));
	faslread_status_update(pos, status);
	Check(! gensymp(pos), "type error");

	return Result(ret, pos);
}


/*
 *  symbol
 */
int faslwrite_value_symbol_(Execute ptr, addr stream, addr pos)
{
	addr value;

	CheckType(pos, LISPTYPE_SYMBOL);
	/* gensym */
	if (gensymp(pos))
		return faslwrite_value_gensym_(ptr, stream, pos);
	/* symbol */
	Return(faslwrite_type_(stream, FaslCode_symbol));
	/* package */
	GetPackageSymbol(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));
	/* name */
	GetNameSymbol(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));

	return 0;
}

int faslread_value_symbol_(Execute ptr, addr stream, addr *ret)
{
	addr package, name;

	Return(faslread_value_(ptr, stream, &package));
	Return(faslread_value_(ptr, stream, &name));
	Check(package == Nil, "package error");
	Return(intern_package_(package, name, ret, NULL));

	return 0;
}


/*
 *  fixnum
 */
int faslwrite_value_fixnum_(Execute ptr, addr stream, addr pos)
{
	fixnum value;

	CheckType(pos, LISPTYPE_FIXNUM);
	Return(faslwrite_type_status_(stream, pos, FaslCode_fixnum));
	GetFixnum(pos, &value);
	Return(faslwrite_buffer_(stream, &value, sizeoft(value)));

	return 0;
}

int faslread_value_fixnum_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	fixnum value;

	Return(faslread_status_(stream, &status));
	Return(faslread_buffer_(stream, &value, sizeoft(value)));
	fixnum_heap(&pos, value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  bignum
 */
int faslwrite_value_bignum_(Execute ptr, addr stream, addr pos)
{
	int sign;
	fixed *data;
	size_t size;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(faslwrite_type_status_(stream, pos, FaslCode_bignum));
	/* sign */
	GetSignBignum(pos, &sign);
	Return(faslwrite_byte_(stream, (byte)sign));
	/* size */
	GetSizeBignum(pos, &size);
	Return(faslwrite_size_(stream, size));
	/* data */
	GetDataBignum(pos, &data);
	Return(faslwrite_buffer_(stream, data, sizeoft(fixed) * size));

	return 0;
}

int faslread_value_bignum_(Execute ptr, addr stream, addr *ret)
{
	byte sign;
	FaslStatus status;
	addr pos;
	fixed *data;
	size_t size;

	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &sign));
	Return(faslread_size_(stream, &size));
	/* data */
	bignum_heap(&pos, (sign != 0), size);
	SetSizeBignum(pos, size);
	GetDataBignum(pos, &data);
	Return(faslread_buffer_(stream, data, sizeoft(fixed) * size));
	/* result */
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  ratio
 */
int faslwrite_value_ratio_(Execute ptr, addr stream, addr pos)
{
	int sign;
	addr numer, denom;

	CheckType(pos, LISPTYPE_RATIO);
	Return(faslwrite_type_status_(stream, pos, FaslCode_ratio));
	/* sign */
	GetSignRatio(pos, &sign);
	Return(faslwrite_byte_(stream, (byte)sign));
	/* numer/denom */
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	Return(faslwrite_value_bignum_(ptr, stream, numer));
	Return(faslwrite_value_bignum_(ptr, stream, denom));

	return 0;
}

int faslread_value_ratio_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	byte sign;
	addr pos, numer, denom;

	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &sign));
	/* numer */
	Return(faslread_type_check_(stream, FaslCode_bignum));
	Return(faslread_value_bignum_(ptr, stream, &numer));
	/* denom */
	Return(faslread_type_check_(stream, FaslCode_bignum));
	Return(faslread_value_bignum_(ptr, stream, &denom));
	/* result */
	make_ratio_heap(&pos, (sign != 0), numer, denom);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  single-float
 */
int faslwrite_value_single_float_(Execute ptr, addr stream, addr pos)
{
	single_float value;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	Return(faslwrite_type_status_(stream, pos, FaslCode_single_float));
	GetSingleFloat(pos, &value);
	Return(faslwrite_buffer_(stream, &value, sizeoft(value)));

	return 0;
}

int faslread_value_single_float_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	single_float value;

	Return(faslread_status_(stream, &status));
	Return(faslread_buffer_(stream, &value, sizeoft(value)));
	single_float_heap(&pos, value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  double-float
 */
int faslwrite_value_double_float_(Execute ptr, addr stream, addr pos)
{
	double_float value;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	Return(faslwrite_type_status_(stream, pos, FaslCode_double_float));
	GetDoubleFloat(pos, &value);
	Return(faslwrite_buffer_(stream, &value, sizeoft(value)));

	return 0;
}

int faslread_value_double_float_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	double_float value;

	Return(faslread_status_(stream, &status));
	Return(faslread_buffer_(stream, &value, sizeoft(value)));
	double_float_heap(&pos, value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  long-float
 */
int faslwrite_value_long_float_(Execute ptr, addr stream, addr pos)
{
	long_float value;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	Return(faslwrite_type_status_(stream, pos, FaslCode_long_float));
	GetLongFloat(pos, &value);
	Return(faslwrite_buffer_(stream, &value, sizeoft(value)));

	return 0;
}

int faslread_value_long_float_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	long_float value;

	Return(faslread_status_(stream, &status));
	Return(faslread_buffer_(stream, &value, sizeoft(value)));
	long_float_heap(&pos, value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  complex
 */
int faslwrite_value_complex_(Execute ptr, addr stream, addr pos)
{
	enum ComplexType type;
	addr value;

	CheckType(pos, LISPTYPE_COMPLEX);
	Return(faslwrite_type_status_(stream, pos, FaslCode_complex));
	/* type */
	type = GetTypeComplex(pos);
	Return(faslwrite_byte_(stream, (byte)type));
	/* real */
	GetRealComplex(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));
	/* imag */
	GetImagComplex(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));

	return 0;
}

int faslread_value_complex_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	byte type;
	addr real, imag, pos;

	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &type));
	Return(faslread_value_(ptr, stream, &real));
	Return(faslread_value_(ptr, stream, &imag));

	make_complex_unsafe(NULL, &pos, (enum ComplexType)type);
	SetRealComplex(pos, real);
	SetImagComplex(pos, imag);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  callname
 */
int faslwrite_value_callname_(Execute ptr, addr stream, addr pos)
{
	CallNameType type;
	addr value;

	CheckType(pos, LISPTYPE_CALLNAME);
	Return(faslwrite_type_status_(stream, pos, FaslCode_callname));
	GetCallNameType(pos, &type);
	GetCallName(pos, &value);
	Return(faslwrite_byte_(stream, (byte)type));
	Return(faslwrite_value_(ptr, stream, value));

	return 0;
}

int faslread_value_callname_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	byte type;
	addr value, pos;

	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &type));
	Return(faslread_value_(ptr, stream, &value));
	callname_heap(&pos, value, (CallNameType)type);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  index
 */
int faslwrite_value_index_(Execute ptr, addr stream, addr pos)
{
	size_t value;

	CheckType(pos, LISPTYPE_INDEX);
	Return(faslwrite_type_status_(stream, pos, FaslCode_index));
	GetIndex(pos, &value);
	Return(faslwrite_size_(stream, value));

	return 0;
}

int faslread_value_index_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	size_t value;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &value));
	index_heap(&pos, value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  package
 */
int faslwrite_value_package_(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	Return(faslwrite_type_(stream, FaslCode_package));
	getname_package_unsafe(pos, &pos);
	Return(strvect_value_heap_(&pos, pos));
	return faslwrite_value_(ptr, stream, pos);
}

int faslread_value_package_(Execute ptr, addr stream, addr *ret)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(faslread_string_local_(local, stream, &pos));
	Return(find_package_(pos, ret));
	rollback_local(local, stack);

	return 0;
}


/*
 *  random-state
 */
int faslwrite_value_random_state_(Execute ptr, addr stream, addr pos)
{
	struct random_state *str;

	CheckType(pos, LISPTYPE_RANDOM_STATE);
	Return(faslwrite_type_status_(stream, pos, FaslCode_random_state));
	str = struct_random_state(pos);
	Return(faslwrite_buffer_(stream, str, sizeoft(struct random_state)));

	return 0;
}

int faslread_value_random_state_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	struct random_state *str;

	Return(faslread_status_(stream, &status));
	random_state_heap(&pos);
	str = struct_random_state(pos);
	Return(faslread_buffer_(stream, str, sizeoft(struct random_state)));
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  pathname
 */
int faslwrite_value_pathname_(Execute ptr, addr stream, addr pos)
{
	int type;
	addr value;

	CheckType(pos, LISPTYPE_PATHNAME);
	Return(faslwrite_type_status_(stream, pos, FaslCode_pathname));
	/* type */
	GetLogicalPathname(pos, &type);
	Return(faslwrite_byte_(stream, (byte)type));
	/* array */
	GetHostPathname(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));
	GetDevicePathname(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));
	GetDirectoryPathname(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));
	GetNamePathname(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));
	GetTypePathname(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));
	GetVersionPathname(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));

	return 0;
}

int faslread_value_pathname_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	byte type;
	addr pos, value;

	/* type */
	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &type));
	make_pathname_alloc(NULL, &pos, (int)type);
	/* array */
	Return(faslread_value_(ptr, stream, &value));
	SetHostPathname(pos, value);
	Return(faslread_value_(ptr, stream, &value));
	SetDevicePathname(pos, value);
	Return(faslread_value_(ptr, stream, &value));
	SetDirectoryPathname(pos, value);
	Return(faslread_value_(ptr, stream, &value));
	SetNamePathname(pos, value);
	Return(faslread_value_(ptr, stream, &value));
	SetTypePathname(pos, value);
	Return(faslread_value_(ptr, stream, &value));
	SetVersionPathname(pos, value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  quote
 */
int faslwrite_value_quote_(Execute ptr, addr stream, addr pos)
{
	enum QuoteType type;
	addr value;

	CheckType(pos, LISPTYPE_QUOTE);
	Return(faslwrite_type_status_(stream, pos, FaslCode_quote));
	/* type */
	GetQuoteType(pos, &type);
	Return(faslwrite_byte_(stream, (byte)type));
	/* value */
	GetQuote(pos, QuoteIndex_Value, &value);
	Return(faslwrite_value_(ptr, stream, value));
	/* print */
	GetQuote(pos, QuoteIndex_Print, &value);
	Return(faslwrite_value_(ptr, stream, value));

	return 0;
}

int faslread_value_quote_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	byte type;
	addr value, print, pos;

	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &type));
	Return(faslread_value_(ptr, stream, &value));
	Return(faslread_value_(ptr, stream, &print));
	quote2_heap(&pos, (enum QuoteType)type, value, print);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  bitvector
 */
int faslwrite_value_bitvector_(Execute ptr, addr stream, addr pos)
{
	struct bitmemory_struct *str;

	CheckType(pos, LISPTYPE_BITVECTOR);
	Return(faslwrite_type_status_(stream, pos, FaslCode_bitvector));
	str = BitMemoryStruct(pos);
	Return(faslwrite_size_(stream, str->bitsize));
	Return(faslwrite_size_(stream, str->fixedsize));
	Return(faslwrite_buffer_(stream, str->data, sizeoft(fixed) * str->fixedsize));

	return 0;
}

int faslread_value_bitvector_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	struct bitmemory_struct *str, value;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &(value.bitsize)));
	Return(faslread_size_(stream, &(value.fixedsize)));
	bitmemory_unsafe(NULL, &pos, value.bitsize);
	str = BitMemoryStruct(pos);
	*str = value;
	Return(faslread_buffer_(stream, str->data, sizeoft(fixed) * str->fixedsize));
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  load
 */
int faslwrite_value_load_time_value_(Execute ptr, addr stream, addr pos)
{
	size_t index;

	CheckType(pos, LISPTYPE_LOAD_TIME_VALUE);
	get_index_load_time_value(pos, &index);
	Return(faslwrite_type_status_(stream, pos, FaslCode_load));
	Return(faslwrite_size_(stream, index));

	return 0;
}

int faslread_value_load_time_value_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	size_t index;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &index));
	Return(execute_load_get_(ptr, index, &pos));
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  paper
 */
int faslwrite_value_paper_(Execute ptr, addr stream, addr pos)
{
	addr value;
	size_t array, body, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_array(pos, &array);
	paper_len_body(pos, &body);
	Return(faslwrite_type_status_(stream, pos, FaslCode_paper));
	Return(faslwrite_size_(stream, array));
	Return(faslwrite_size_(stream, body));
	/* array */
	for (i = 0; i < array; i++) {
		paper_get_array(pos, i, &value);
		Return(faslwrite_value_(ptr, stream, value));
	}
	/* body */
	if (body) {
		posbody(pos, &value);
		Return(faslwrite_buffer_(stream, (const void *)value, body));
	}

	return 0;
}

int faslread_value_paper_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos, value;
	size_t array, body, i;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &array));
	Return(faslread_size_(stream, &body));
	Return(paper_arraybody_heap_(&pos, array, body));
	/* array */
	for (i = 0; i < array; i++) {
		Return(faslread_value_(ptr, stream, &value));
		paper_set_array(pos, i, value);
	}
	/* body */
	if (body) {
		posbody(pos, &value);
		Return(faslread_buffer_(stream, (void *)value, body));
	}
	/* result */
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/************************************************************
 *  compile_write.c
 ************************************************************/

/*
 *  write function
 */
static int faslwrite_magic_(addr stream)
{
	char a[16];

	memset(a, 0, 8);
	strncpy(a, LISPNAME, 8);
	Return(faslwrite_buffer_(stream, "\0\0\0\0FASL", 8));
	Return(faslwrite_buffer_(stream, a, 8));

	return 0;
}

static int faslwrite_byte16_(addr stream, uint16_t v)
{
	return faslwrite_buffer_(stream, &v, sizeoft(v));
}

int faslwrite_header_(addr stream)
{
	char buffer[32];

	/* 0: magic number */
	Return(faslwrite_magic_(stream));
	/* 16: endian */
	Return(faslwrite_byte16_(stream, 1));
	/* 18: version */
	Return(faslwrite_byte16_(stream, LISP_VERSION_A));
	Return(faslwrite_byte16_(stream, LISP_VERSION_B));
	Return(faslwrite_byte16_(stream, LISP_VERSION_C));
	/* 24: CPU arch */
#ifdef LISP_ARCH_64BIT
	Return(faslwrite_byte_(stream, 1));
#else
	Return(faslwrite_byte_(stream, 0));
#endif
	/* 25: fixnum size */
#ifdef LISP_64BIT
	Return(faslwrite_byte_(stream, 1));
#else
	Return(faslwrite_byte_(stream, 0));
#endif
	/* 26: padding */
	memset(buffer, 0xFF, 32);
	Return(faslwrite_buffer_(stream, buffer, 6));
	/* 32: end */

	return 0;
}

int faslwrite_footer_(addr stream)
{
	char buffer[8];

	Return(faslwrite_break_(stream));
	Return(faslwrite_type_(stream, FaslCode_eof));
	memset(buffer, 0x00, 8);
	Return(faslwrite_buffer_(stream, buffer, 8));
	memset(buffer, 0xFF, 8);
	Return(faslwrite_buffer_(stream, buffer, 8));

	return 0;
}

int faslwrite_break_(addr stream)
{
	return faslwrite_type_(stream, FaslCode_break);
}


/*
 *  unbound
 */
static int faslwrite_unbound_(Execute ptr, addr stream)
{
	return faslwrite_type_(stream, FaslCode_unbound);
}


/*
 *  table code
 */
static void code_value_heap(addr *ret, enum CodeValueType type, CodeValue x)
{
	switch (type) {
		case CodeValueType_Addr:
			*ret = x.pos;
			break;

		case CodeValueType_Index:
			index_heap(ret, x.index);
			break;

		case CodeValueType_Fixnum:
			fixnum_heap(ret, x.value);
			break;

		case CodeValueType_FixnumNull:
			if (x.pos == Nil)
				*ret = Nil;
			else
				fixnum_heap(ret, x.value);
			break;

		case CodeValueType_Character:
			character_heap(ret, x.character);
			break;

		case CodeValueType_Null:
		default:
			*ret = Nil;
			break;
	}
}

static int faslwrite_value_operator_(Execute ptr, addr stream, pointer id, CodeValue x)
{
	enum CodeValueType type;
	enum FaslCode code;
	addr value;

	Check(p_size_code < id, "pointer error");
	GetCodeValueArray(id, &type);
	code_value_heap(&value, type, x);
	code = GetCompileWrite(id);
	/* write */
	Return(faslwrite_type_(stream, code));
	return faslwrite_value_(ptr, stream, value);
}

static int faslwrite_value_code_(Execute ptr, addr stream, addr pos)
{
	struct code_struct *str;
	struct code_value *sys, *bind;
	size_t size, i;

	Check(GetType(pos) != LISPTYPE_CODE, "type error.");
	/* type */
	Return(faslwrite_type_status_(stream, pos, FaslCode_code));
	/* struct */
	str = StructCode(pos);
	sys = str->sys;
	size = str->size;
	Return(faslwrite_size_(stream, size));
	/* code */
	GetArrayCode(pos, Code_Array, &pos);
	for (i = 0; i < size; i++) {
		bind = sys + i;
		Return(faslwrite_value_operator_(ptr, stream, bind->id, bind->value));
	}

	return 0;
}


/*
 *  table value
 */
typedef int (*faslwrite_callvalue)(Execute, addr, addr);
static faslwrite_callvalue FaslWrite_Value[LISPTYPE_COMPILE];

int faslwrite_value_(Execute ptr, addr stream, addr pos)
{
	enum LISPTYPE type;
	faslwrite_callvalue call;

	/* unbound */
	if (pos == Unbound)
		return faslwrite_unbound_(ptr, stream);

	/* object */
	type = GetType(pos);
	Check(LISPTYPE_COMPILE <= type, "type error");
	call = FaslWrite_Value[type];
	if (call == NULL) {
		return fmte_("Cannot compile the value ~S.", pos, NULL);
	}

	return (*call)(ptr, stream, pos);
}


/*
 *  initialize
 */
void init_compile_write(void)
{
	FaslWrite_Value[LISPTYPE_NIL] = faslwrite_value_nil_;
	FaslWrite_Value[LISPTYPE_T] = faslwrite_value_t_;
	FaslWrite_Value[LISPTYPE_TYPE] = faslwrite_value_type_;
	FaslWrite_Value[LISPTYPE_CLOS] = faslwrite_value_clos_;
	FaslWrite_Value[LISPTYPE_CONS] = faslwrite_value_cons_;
	FaslWrite_Value[LISPTYPE_ARRAY] = faslwrite_value_array_;
	FaslWrite_Value[LISPTYPE_VECTOR] = faslwrite_value_vector_;
	FaslWrite_Value[LISPTYPE_CHARACTER] = faslwrite_value_character_;
	FaslWrite_Value[LISPTYPE_STRING] = faslwrite_value_string_;
	FaslWrite_Value[LISPTYPE_HASHTABLE] = faslwrite_value_hashtable_;
	FaslWrite_Value[LISPTYPE_SYMBOL] = faslwrite_value_symbol_;
	FaslWrite_Value[LISPTYPE_FIXNUM] = faslwrite_value_fixnum_;
	FaslWrite_Value[LISPTYPE_BIGNUM] = faslwrite_value_bignum_;
	FaslWrite_Value[LISPTYPE_RATIO] = faslwrite_value_ratio_;
	FaslWrite_Value[LISPTYPE_SINGLE_FLOAT] = faslwrite_value_single_float_;
	FaslWrite_Value[LISPTYPE_DOUBLE_FLOAT] = faslwrite_value_double_float_;
	FaslWrite_Value[LISPTYPE_LONG_FLOAT] = faslwrite_value_long_float_;
	FaslWrite_Value[LISPTYPE_COMPLEX] = faslwrite_value_complex_;
	FaslWrite_Value[LISPTYPE_CODE] = faslwrite_value_code_;
	FaslWrite_Value[LISPTYPE_CALLNAME] = faslwrite_value_callname_;
	FaslWrite_Value[LISPTYPE_INDEX] = faslwrite_value_index_;
	FaslWrite_Value[LISPTYPE_PACKAGE] = faslwrite_value_package_;
	FaslWrite_Value[LISPTYPE_RANDOM_STATE] = faslwrite_value_random_state_;
	FaslWrite_Value[LISPTYPE_PATHNAME] = faslwrite_value_pathname_;
	FaslWrite_Value[LISPTYPE_QUOTE] = faslwrite_value_quote_;
	FaslWrite_Value[LISPTYPE_BITVECTOR] = faslwrite_value_bitvector_;
	FaslWrite_Value[LISPTYPE_LOAD_TIME_VALUE] = faslwrite_value_load_time_value_;
	FaslWrite_Value[LISPTYPE_PAPER] = faslwrite_value_paper_;
}


/************************************************************
 *  condition.c
 ************************************************************/

/*
 *  condition for clang
 */
int conditionp_(addr pos, int *ret)
{
	addr super;

	if (GetType(pos) != LISPTYPE_CLOS)
		return Result(ret, 0);
	GetConst(CLOS_CONDITION, &super);
	return clos_subclass_p_(pos, super, ret);
}

int conditionp_debug(addr pos)
{
	int check = 0;
	Error(conditionp_(pos, &check));
	return check;
}

int condition_instance_p_(addr pos, int *ret)
{
	addr super;

	if (GetType(pos) != LISPTYPE_CLOS)
		return Result(ret, 0);
	GetConst(CLOS_CONDITION, &super);
	return clos_subtype_p_(pos, super, ret);
}

static void signal_restart(addr *ret)
{
	addr restart, pos, str;

	strvect_char_heap(&str, "Return to SIGNAL.");
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	setinteractive_restart(restart, Nil);
	setreport_restart(restart, str);
	settest_restart(restart, Nil);
	setescape_restart(restart, 1);
	*ret = restart;
}

static int signal_invoke_debugger_(Execute ptr, addr condition)
{
	addr control, restart;

	push_control(ptr, &control);
	signal_restart(&restart);
	pushrestart_control(ptr, restart);

	/* debugger */
	(void)invoke_debugger_(ptr, condition);
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

static int break_on_signals_p_(Execute ptr, addr condition, int *ret)
{
	addr pos;

	GetConst(SPECIAL_BREAK_ON_SIGNALS, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	Return(parse_type_(ptr, &pos, pos, Nil));
	return typep_asterisk_clang_(ptr, condition, pos, ret);
}

int signal_function_(Execute ptr, addr condition)
{
	int check;

	if (ptr == NULL)
		ptr = Execute_Thread;

	/* break-on-signals */
	Return(break_on_signals_p_(ptr, condition, &check));
	if (check)
		return signal_invoke_debugger_(ptr, condition);

	/* signal */
	return invoke_handler_control_(ptr, condition);
}

int error_function_(Execute ptr, addr condition)
{
	int check;

	if (ptr == NULL)
		ptr = Execute_Thread;
	gchold_push_local(ptr->local, condition);

	/* break-on-signals */
	Return(break_on_signals_p_(ptr, condition, &check));
	if (check) {
		Return(signal_invoke_debugger_(ptr, condition));
	}

	/* error */
	Return(invoke_handler_control_(ptr, condition));
	return invoke_debugger_(ptr, condition);
}

int callclang_error_(const char *str, ...)
{
	addr format, args;
	va_list va;

	strvect_char_heap(&format, str);
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return call_simple_error_(NULL, format, args);
}

static void warning_restart_make(addr *ret)
{
	addr inst, pos;

	GetConst(COMMON_MUFFLE_WARNING, &pos);
	restart_heap(&inst, pos);
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(inst, pos);
	setinteractive_restart(inst, Nil);
	strvect_char_heap(&pos, "Skip warning.");
	setreport_restart(inst, pos);
	settest_restart(inst, Nil);
	setescape_restart(inst, 1);
	*ret = inst;
}

int warning_restart_case_(Execute ptr, addr instance)
{
	int check;
	addr type, control, restart;

	/* type check */
	GetConst(CONDITION_WARNING, &type);
	Return(clos_subtype_p_(instance, type, &check));
	if (! check) {
		return call_type_error_va_(ptr, instance, type,
				"The instance ~S must be a WARNING type.", instance, NULL);
	}

	/* warn */
	if (ptr == NULL)
		ptr = Execute_Thread;
	push_control(ptr, &control);
	warning_restart_make(&restart);
	(void)restart1_control_(ptr, restart, signal_function_, instance);
	return pop_control_(ptr, control);
}

int callclang_warning_(const char *str, ...)
{
	addr format, args, instance;
	va_list va;

	strvect_char_heap(&format, str);
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	Return(instance_simple_warning_(&instance, format, args));
	return warning_restart_case_(NULL, instance);
}


/*
 *  initialize
 */
void build_condition(Execute ptr)
{
	build_condition_debugger(ptr);
}

void init_condition(void)
{
	init_condition_debugger();
}


/************************************************************
 *  condition_debugger.c
 ************************************************************/

/* enable-debugger */
static void enable_debugger_symbol(addr *ret)
{
	GetConst(SYSTEM_ENABLE_DEBUGGER, ret);
}

static void init_enable_debugger(void)
{
	addr symbol;
	enable_debugger_symbol(&symbol);
	SetValueSymbol(symbol, T);
}

void set_enable_debugger(Execute ptr, int value)
{
	addr symbol;
	enable_debugger_symbol(&symbol);
	setspecial_local(ptr, symbol, value? T: Nil);
}

static int enable_debugger_p_(Execute ptr, int *ret)
{
	addr symbol, pos;

	enable_debugger_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &pos));

	return Result(ret, pos != Nil);
}

/* index-debugger */
static void index_debugger_symbol(addr *ret)
{
	GetConst(SYSTEM_INDEX_DEBUGGER, ret);
}

static void init_index_debugger(void)
{
	addr symbol, value;

	index_debugger_symbol(&symbol);
	fixnum_heap(&value, 0);
	SetValueSymbol(symbol, value);
}

static int push_index_debugger_(Execute ptr)
{
	addr symbol, value;

	/* increment */
	index_debugger_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &value));
	Return(oneplus_integer_common_(ptr->local, value, &value));
	pushspecial_control(ptr, symbol, value);
	Return(format_string_(ptr, &value, "[~A]* ", value, NULL));

	/* push prompt */
	push_prompt(ptr, value, prompt_debugger);

	return 0;
}


/*
 *  (handler-bind
 *    ((warning #'function-handler-warning))
 *    ...)
 */
static int function_handler_warning(Execute ptr, addr condition)
{
	int check;
	addr pos, stream, format, args;

	Return(error_output_stream_(ptr, &stream));
	GetConst(CONDITION_SIMPLE_WARNING, &pos);
	Return(clos_subtype_p_(condition, pos, &check));
	if (check) {
		Return(simple_condition_format_control_(condition, &format));
		Return(simple_condition_format_arguments_(condition, &args));
		Return(format_stream_(ptr, stream, "~&WARNING: ", NULL));
		Return(format_lisp_(ptr, stream, format, args, &args));
		Return(fresh_line_stream_(stream, NULL));
	}
	else {
		Return(format_stream_(ptr, stream, "~&WARNING: ~S~%", condition, NULL));
	}

	return force_output_stream_(stream);
}

int handler_warning_(Execute ptr)
{
	addr pos, call;

	GetConst(CONDITION_WARNING, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_warning);
	return pushhandler_common_(ptr, pos, call, 0);
}


/*
 *  (handler-case
 *    ...
 *    ((system::savecore #'function-handler-empty)))
 */
static int function_handler_empty(Execute ptr, addr condition)
{
	/* do-nothing */
	return 0;
}

int handler_savecore_(Execute ptr)
{
	addr pos, call;

	GetConst(CONDITION_SAVECORE, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_empty);
	return pushhandler_common_(ptr, pos, call, 1);
}


/*
 *  (handler-case
 *    ...
 *    ((system::exit #'function-handler-empty)))
 */
int handler_exit_(Execute ptr)
{
	addr pos, call;

	GetConst(CONDITION_EXIT, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_empty);
	return pushhandler_common_(ptr, pos, call, 1);
}


/*
 *  debugger
 */
static int output_unbound_variable_(Execute ptr, addr stream, addr condition)
{
	Return(cell_error_name_(condition, &condition));
	return format_stream_(ptr, stream, "Unbound variable ~S.~%", condition, NULL);
}

static int output_undefined_function_(Execute ptr, addr stream, addr condition)
{
	Return(cell_error_name_(condition, &condition));
	return format_stream_(ptr, stream, "Undefined function ~S.~%", condition, NULL);
}

static int output_unbound_slot_(Execute ptr, addr stream, addr condition)
{
	addr instance, name;

	Return(unbound_slot_instance_(condition, &instance));
	Return(cell_error_name_(condition, &name));
	return format_stream_(ptr, stream,
			"The slot ~S is unbound in the ~S.~%", name, instance, NULL);
}

static int output_simple_error_(Execute ptr, addr stream, addr condition)
{
	addr control, arguments;

	Return(simple_condition_format_(condition, &control, &arguments));
	Return(format_stream_lisp_(ptr, stream, control, arguments));
	Return(fresh_line_stream_(stream, NULL));
	Return(terpri_stream_(stream));

	return 0;
}

static int output_type_error_(Execute ptr, addr stream, addr instance)
{
	addr datum, expected;

	Return(type_error_datum_(instance, &datum));
	Return(type_error_expected_(instance, &expected));
	if (GetType(expected) == LISPTYPE_TYPE) {
		Return(type_object_(&expected, expected));
	}
	return format_stream_(ptr, stream,
			"The value ~S must be a ~S type.~%", datum, expected, NULL);
}

static int output_condition_(Execute ptr, addr stream, addr condition)
{
	return 0;
}

static int condition_check_p_(constindex index, addr condition, int *ret)
{
	int check;
	addr super;

	Return(condition_instance_p_(condition, &check));
	if (! check)
		return Result(ret, 0);
	GetConstant(index, &super);
	return clos_subtype_p_(condition, super, ret);
}
#define ConditionCheck_(x,y,r) condition_check_p_(CONSTANT_CONDITION_##x,(y),(r))

static int output_debugger_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(ConditionCheck_(UNBOUND_VARIABLE, pos, &check));
	if (check)
		return output_unbound_variable_(ptr, stream, pos);
	Return(ConditionCheck_(UNDEFINED_FUNCTION, pos, &check));
	if (check)
		return output_undefined_function_(ptr, stream, pos);
	Return(ConditionCheck_(UNBOUND_SLOT, pos, &check));
	if (check)
		return output_unbound_slot_(ptr, stream, pos);
	Return(ConditionCheck_(SIMPLE_CONDITION, pos, &check));
	if (check)
		return output_simple_error_(ptr, stream, pos);
	Return(ConditionCheck_(TYPE_ERROR, pos, &check));
	if (check)
		return output_type_error_(ptr, stream, pos);
	Return(condition_instance_p_(pos, &check));
	if (check)
		return output_condition_(ptr, stream, pos);
	/* otherwise */
	return format_stream_(ptr, stream, "Invalid condition type ~S~%", pos, NULL);
}

static int invoke_standard_header_(Execute ptr, addr io, addr condition)
{
	addr pos;

	Return(clos_class_of_(condition, &pos));
	Return(stdget_class_name_(pos, &pos));
	(void)text_color_terme(ptr, print_color_bright_red);
	Return(finish_output_stream_(io));
	Return(format_stream_(ptr, io, "~&ERROR: ~S~%", pos, NULL));
	Return(finish_output_stream_(io));
	(void)text_color_terme(ptr, print_color_reset);
	Return(finish_output_stream_(io));
	Return(output_debugger_(ptr, io, condition));

	return 0;
}

static int output_restarts_debugger_(Execute ptr, addr io, addr list)
{
	int check;
	addr pos, symbol, name, str, id;
	size_t index;

	for (index = 0; list != Nil; index++) {
		GetCons(list, &pos, &list);
		CheckType(pos, LISPTYPE_RESTART);
		getname_restart(pos, &symbol);
		getreport_restart(pos, &name);
		if (name != Nil) {
			if (! stringp(name)) {
				open_output_string_stream(&str, 0);
				check = funcall1_control_(ptr, &name, name, str, NULL);
				if (check)
					return fmte_("Invalid restart report.", NULL);
				Return(string_stream_heap_(str, &name));
				close_output_string_stream(str);
			}
		}
		id = intsizeh(index);
		Return(format_stream_(ptr, io, "~2@A. ~16A ~A~%", id, symbol, name, NULL));
	}

	return 0;
}

static int eval_debugger_call_(Execute ptr, addr io, addr eval)
{
	Return(eval_execute_partial_(ptr, eval));
	return eval_loop_output_(ptr, io);
}

static int eval_debugger_(Execute ptr, addr io, addr eval)
{
	addr control;

	push_control(ptr, &control);
	(void)eval_debugger_call_(ptr, io, eval);
	return pop_control_(ptr, control);
}

static int eval_symbol_debugger_(Execute ptr, addr io, addr list, addr eval)
{
	int check;
	addr root, restart, x, y;

	/* eq check */
	root = list;
	while (root != Nil) {
		GetCons(root, &restart, &root);
		getname_restart(restart, &x);
		if (x == eval)
			return invoke_restart_interactively_control_(ptr, restart);
	}

	/* symbol= check */
	Return(string_designator_heap_(&y, eval, &check));
	if (check) {
		root = list;
		while (root != Nil) {
			GetCons(root, &restart, &root);
			getname_restart(restart, &x);
			Return(string_designator_equal_(x, y, &check));
			if (check)
				return invoke_restart_interactively_control_(ptr, restart);
		}
	}

	/* eval */
	return eval_debugger_(ptr, io, eval);
}

static int enter_debugger_symbol_p(addr pos, const char *key, int keyword)
{
	if (! symbolp(pos))
		return 0;
	if (keyword && (! keywordp(pos)))
		return 0;
	GetNameSymbol(pos, &pos);
	if (! strvectp(pos))
		return 0;
	return strvect_equalp_char(pos, key);
}

static int enter_debugger_help_(Execute ptr, addr io)
{
	Return(format_stream_(ptr, io, "~&Help:~%", NULL));
	Return(format_stream_(ptr, io, ":help   This message.~%", NULL));
	Return(format_stream_(ptr, io, "?       Run :help.~%", NULL));
	Return(format_stream_(ptr, io, ":show   Debugger information.~%", NULL));
	Return(format_stream_(ptr, io, ":stack  Stack-frame.~%", NULL));
	Return(format_stream_(ptr, io, ":exit   Exit debugger.~%", NULL));
	Return(format_stream_(ptr, io, "^D      Exit debugger.~%", NULL));

	return 0;
}

static int enter_debugger_call_(Execute ptr, addr io, addr list, int *ret)
{
	int eof;
	addr pos;
	size_t select, size;

	Return(clear_input_stream_(io));
	Return(read_prompt_(ptr, io, &eof, &pos));

	/* :exit, EOF */
	if (eof || enter_debugger_symbol_p(pos, "EXIT", 1)) {
		if (eof) {
			Return(terpri_stream_(io));
		}
		Return(finish_output_stream_(io));
		/* restart abort */
		GetConst(COMMON_ABORT, &pos);
		Return(eval_symbol_debugger_(ptr, io, list, pos));
		return Result(ret, 0);
	}

	/* show */
	if (enter_debugger_symbol_p(pos, "SHOW", 1))
		return Result(ret, -1);

	/* stack */
	if (enter_debugger_symbol_p(pos, "STACK", 1)) {
		Return(stack_frame_stream_(ptr, io));
		return Result(ret, 0);
	}

	/* help */
	if (enter_debugger_symbol_p(pos, "HELP", 0)
			|| enter_debugger_symbol_p(pos, "?", 0)) {
		Return(enter_debugger_help_(ptr, io));
		return Result(ret, 0);
	}

	/* check */
	if (! fixnump(pos)) {
		Return(eval_symbol_debugger_(ptr, io, list, pos));
		return Result(ret, 0);
	}
	if (GetIndex_integer(pos, &select)) {
		Return(format_stream_(ptr, io, "Illegal integer value ~A.~%", pos, NULL));
		return Result(ret, 0);
	}

	size = length_list_unsafe(list);
	if (size <= select) {
		Return(format_stream_(ptr, io, "Too large index value ~A.~%", pos, NULL));
		return Result(ret, 0);
	}
	/* execute */
	getnth_unsafe(list, select, &pos);
	Return(invoke_restart_interactively_control_(ptr, pos));
	return Result(ret, 0);
}

static int enter_debugger_abort_(Execute ptr, addr io, addr list, int *ret)
{
	addr control;

	push_control(ptr, &control);
	abort_restart_char_control(ptr, "Exit the debugger on the inside.");
	(void)enter_debugger_call_(ptr, io, list, ret);
	if (equal_control_restart(ptr, control)) {
		normal_throw_control(ptr);
		*ret = 0;
	}
	return pop_control_(ptr, control);
}

static int enter_debugger_(Execute ptr, addr io, addr condition, addr list)
{
	int check;

	/* restarts */
	Return(push_index_debugger_(ptr));

show:
	Return(invoke_standard_header_(ptr, io, condition));
	Return(output_restarts_debugger_(ptr, io, list));

loop:
	Return(enter_debugger_abort_(ptr, io, list, &check));
	switch (check) {
		case -1:
			goto show;
		case 0:
			goto loop;
		default:
			return 0;
	}
}

static int invoke_standard_debugger_(Execute ptr, addr condition)
{
	int check;
	addr io, list, control;

	/* output condition */
	Return(debug_io_stream_(ptr, &io));
	Return(compute_restarts_control_(ptr, condition, &list));

	/* no-debugger */
	Return(enable_debugger_p_(ptr, &check));
	if (! check) {
		Return(invoke_standard_header_(ptr, io, condition));
		Return(format_stream_(ptr, io, "~2&Debugger is not enabled.~%", NULL));
		abort_execute();
		return 0;
	}

	/* no-restart */
	if (list == Nil) {
		Return(invoke_standard_header_(ptr, io, condition));
		Return(format_stream_(ptr, io, "There is no restarts, abort.~%", NULL));
		abort_execute();
		return 0;
	}

	/* debugger */
	push_control(ptr, &control);
	gchold_push_special(ptr, list);
	(void)enter_debugger_(ptr, io, condition, list);
	return pop_control_(ptr, control);
}

static int invoke_debugger_hook_(Execute ptr, addr prior, addr condition)
{
	addr symbol, call, control;

	/* call function */
	call = prior;
	if (symbolp(call)) {
		Return(getfunction_global_(call, &call));
	}

	/* funcall */
	push_control(ptr, &control);
	GetConst(SPECIAL_DEBUGGER_HOOK, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	(void)funcall_control_(ptr, call, condition, prior, NULL);
	Return(pop_control_(ptr, control));

	/* invoke-debugger is not returned. */
	return invoke_standard_debugger_(ptr, condition);
}

int invoke_debugger_(Execute ptr, addr condition)
{
	addr symbol, prior;

	GetConst(SPECIAL_DEBUGGER_HOOK, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &prior));
	if (prior == Nil)
		return invoke_standard_debugger_(ptr, condition);
	else
		return invoke_debugger_hook_(ptr, prior, condition);
}


/*
 *  initialize
 */
void build_condition_debugger(Execute ptr)
{
	init_enable_debugger();
	init_index_debugger();
	set_enable_debugger(ptr, 1);
}

void init_condition_debugger(void)
{
	SetPointerCall(defun, var1, handler_warning);
	SetPointerCall(defun, var1, handler_empty);
}


/************************************************************
 *  condition_define.c
 ************************************************************/

static int instance_condition_(addr *ret, constindex condition)
{
	addr pos;
	GetConstant(condition, &pos);
	return clos_instance_heap_(pos, ret);
}

static int instance_condition1_(addr *ret, constindex index,
		constindex index1, addr pos1)
{
	addr instance;

	GetConstant(index, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, index1, pos1));
	return Result(ret, instance);
}

static int instance_condition2_(addr *ret, constindex index,
		constindex index1, addr pos1,
		constindex index2, addr pos2)
{
	addr instance;

	GetConstant(index, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, index1, pos1));
	Return(clos_setconst_(instance, index2, pos2));
	return Result(ret, instance);
}

static int instance_condition4_(addr *ret, constindex index,
		constindex index1, addr pos1,
		constindex index2, addr pos2,
		constindex index3, addr pos3,
		constindex index4, addr pos4)
{
	addr instance;

	GetConstant(index, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, index1, pos1));
	Return(clos_setconst_(instance, index2, pos2));
	Return(clos_setconst_(instance, index3, pos3));
	Return(clos_setconst_(instance, index4, pos4));
	return Result(ret, instance);
}


/* serious_condition (condition) */
int instance_serious_condition_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_SERIOUS_CONDITION);
}

int call_serious_condition_(Execute ptr)
{
	addr instance;
	Return(instance_serious_condition_(&instance));
	return error_function_(ptr, instance);
}


/* simple_condition (condition) :format-control :format-arguments*/
int instance_simple_condition_(addr *ret, addr control, addr args)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_SIMPLE_CONDITION,
			CONSTANT_CLOSNAME_FORMAT_CONTROL, control,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args);
}

int call_simple_condition_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_condition_(&instance, control, args));
	return signal_function_(ptr, instance);
}

int simple_condition_format_(addr condition, addr *control, addr *arguments)
{
	Return(ClosCheckConst_(condition, CLOSNAME_FORMAT_CONTROL, control));
	Return(ClosCheckConst_(condition, CLOSNAME_FORMAT_ARGUMENTS, arguments));
	return 0;
}

int simple_condition_format_control_(addr condition, addr *ret)
{
	return ClosCheckConst_(condition, CLOSNAME_FORMAT_CONTROL, ret);
}

int simple_condition_format_arguments_(addr condition, addr *ret)
{
	return ClosCheckConst_(condition, CLOSNAME_FORMAT_ARGUMENTS, ret);
}


/* simple_error (simple_condition) :format-control :format-arguments */
int instance_simple_error_(addr *ret, addr control, addr args)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_SIMPLE_ERROR,
			CONSTANT_CLOSNAME_FORMAT_CONTROL, control,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args);
}

int call_simple_error_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_error_(&instance, control, args));
	return error_function_(ptr, instance);
}


/* error (serious_condition) */
int instance_error_condition_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_ERROR);
}

int call_error_condition_(Execute ptr)
{
	addr instance;
	Return(instance_error_condition_(&instance));
	return error_function_(ptr, instance);
}


/* warning (condition) */
int instance_warning_condition_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_WARNING);
}

int call_warning_condition_(Execute ptr)
{
	addr instance;
	Return(instance_warning_condition_(&instance));
	return error_function_(ptr, instance);
}


/* simple_warning (simple_condition warning) :format-control :format-arguments */
int instance_simple_warning_(addr *ret, addr control, addr args)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_SIMPLE_WARNING,
			CONSTANT_CLOSNAME_FORMAT_CONTROL, control,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args);
}

int call_simple_warning_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_warning_(&instance, control, args));
	return signal_function_(ptr, instance);
}


/* storage_condition (serious_condition) */
int instance_storage_condition_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_STORAGE_CONDITION);
}

int call_storage_condition_(Execute ptr)
{
	addr instance;
	Return(instance_storage_condition_(&instance));
	return error_function_(ptr, instance);
}


/* arithmetic_error (error) :operation :operands */
int instance_arithmetic_error_(addr *ret, addr pos, addr list)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_ARITHMETIC_ERROR,
			CONSTANT_CLOSNAME_OPERATION, pos,
			CONSTANT_CLOSNAME_OPERANDS, list);
}

int call_arithmetic_error_(Execute ptr, addr pos, addr list)
{
	addr instance;
	Return(instance_arithmetic_error_(&instance, pos, list));
	return error_function_(ptr, instance);
}

int arithmetic_error_operation_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_OPERATION, ret);
}

int arithmetic_error_operands_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_OPERANDS, ret);
}


/* floating_point_inexact (arithmetic_error) :operation :operands */
int instance_float_inexact_(addr *ret, addr pos, addr list)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_FLOATING_POINT_INEXACT,
			CONSTANT_CLOSNAME_OPERATION, pos,
			CONSTANT_CLOSNAME_OPERANDS, list);
}

int call_float_inexact_(Execute ptr, addr pos, addr list)
{
	addr instance;
	Return(instance_float_inexact_(&instance, pos, list));
	return error_function_(ptr, instance);
}

int call_float_inexact_const_(Execute ptr, constindex id, addr list)
{
	addr pos;
	GetConstant(id, &pos);
	return call_float_inexact_(ptr, pos, list);
}

int call_float_inexact_va_(Execute ptr, constindex id, ...)
{
	addr list;
	va_list va;

	va_start(va, id);
	list_stdarg_alloc(NULL, &list, va);
	va_end(va);
	return call_float_inexact_const_(ptr, id, list);
}


/* floating_point_invalid_operation (arithmetic_error) :operation :operands */
int instance_float_invalid_(addr *ret, addr pos, addr list)
{
	return instance_condition2_(ret,
			CONSTANT_CONDITION_FLOATING_POINT_INVALID_OPERATION,
			CONSTANT_CLOSNAME_OPERATION, pos,
			CONSTANT_CLOSNAME_OPERANDS, list);
}

int call_float_invalid_(Execute ptr, addr pos, addr list)
{
	addr instance;
	Return(instance_float_invalid_(&instance, pos, list));
	return error_function_(ptr, instance);
}

int call_float_invalid_const_(Execute ptr, constindex id, addr list)
{
	addr pos;
	GetConstant(id, &pos);
	return call_float_invalid_(ptr, pos, list);
}


/* floating_point_overflow (arithmetic_error) :operation :operands */
int instance_float_overflow_(addr *ret, addr pos, addr list)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_FLOATING_POINT_OVERFLOW,
			CONSTANT_CLOSNAME_OPERATION, pos,
			CONSTANT_CLOSNAME_OPERANDS, list);
}

int call_float_overflow_(Execute ptr, addr pos, addr list)
{
	addr instance;
	Return(instance_float_overflow_(&instance, pos, list));
	return error_function_(ptr, instance);
}

int call_float_overflow_const_(Execute ptr, constindex id, addr list)
{
	addr pos;
	GetConstant(id, &pos);
	return call_float_overflow_(ptr, pos, list);
}

int call_float_overflow_va_(Execute ptr, constindex id, ...)
{
	addr list;
	va_list va;

	va_start(va, id);
	copylocal_list_stdarg(NULL, &list, va);
	va_end(va);
	return call_float_overflow_const_(ptr, id, list);
}


/* floating_point_underflow (arithmetic_error) :operation :operands */
int instance_float_underflow_(addr *ret, addr pos, addr list)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_FLOATING_POINT_UNDERFLOW,
			CONSTANT_CLOSNAME_OPERATION, pos,
			CONSTANT_CLOSNAME_OPERANDS, list);
}

int call_float_underflow_(Execute ptr, addr pos, addr list)
{
	addr instance;
	Return(instance_float_underflow_(&instance, pos, list));
	return error_function_(ptr, instance);
}

int call_float_underflow_const_(Execute ptr, constindex id, addr list)
{
	addr pos;
	GetConstant(id, &pos);
	return call_float_underflow_(ptr, pos, list);
}

int call_float_underflow_va_(Execute ptr, constindex id, ...)
{
	addr list;
	va_list va;

	va_start(va, id);
	copylocal_list_stdarg(NULL, &list, va);
	va_end(va);
	return call_float_underflow_const_(ptr, id, list);
}


/* division_by_zero (arithmetic_error) :operation :operands */
int instance_division_by_zero_(addr *ret, addr pos, addr list)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_DIVISION_BY_ZERO,
			CONSTANT_CLOSNAME_OPERATION, pos,
			CONSTANT_CLOSNAME_OPERANDS, list);
}

int call_division_by_zero_(Execute ptr, addr pos, addr list)
{
	addr instance;
	Return(instance_division_by_zero_(&instance, pos, list));
	return error_function_(ptr, instance);
}

int call_division_by_zero_const_(Execute ptr, constindex id, addr list)
{
	addr pos;
	GetConstant(id, &pos);
	return call_division_by_zero_(ptr, pos, list);
}

int call_division_by_zero_real1_(Execute ptr, constindex id, addr x)
{
	Return(number_throw_heap_(x, &x));
	list_heap(&x, x, NULL);
	return call_division_by_zero_const_(ptr, id, x);
}

int call_division_by_zero_real2_(Execute ptr, constindex id, addr x, addr y)
{
	Return(number_throw_heap_(x, &x));
	Return(number_throw_heap_(y, &y));
	list_heap(&x, x, y, NULL);
	return call_division_by_zero_const_(ptr, id, x);
}

int call_division_by_zero1_(Execute ptr, addr left)
{
	return call_division_by_zero_real1_(ptr, CONSTANT_COMMON_SLASH, left);
}

int call_division_by_zero2_(Execute ptr, addr left, addr right)
{
	return call_division_by_zero_real2_(ptr, CONSTANT_COMMON_SLASH, left, right);
}


/* cell_error (error) :name */
int instance_cell_error_(addr *ret, addr name)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_CELL_ERROR,
			CONSTANT_CLOSNAME_NAME, name);
}

int call_cell_error_(Execute ptr, addr name)
{
	addr instance;
	Return(instance_cell_error_(&instance, name));
	return error_function_(ptr, instance);
}

int cell_error_name_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_NAME, ret);
}


/* control_error (error) */
int instance_control_error_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_CONTROL_ERROR);
}

int call_control_error_(Execute ptr)
{
	addr instance;
	Return(instance_control_error_(&instance));
	return error_function_(ptr, instance);
}


/* stream_error (error) :stream */
int instance_stream_error_(addr *ret, addr stream)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_STREAM_ERROR,
			CONSTANT_CLOSNAME_STREAM, stream);
}

int call_stream_error_(Execute ptr, addr stream)
{
	addr instance;
	Return(instance_stream_error_(&instance, stream));
	return error_function_(ptr, instance);
}

int stream_error_stream_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_STREAM, ret);
}


/* end_of_file (stream_error) :stream */
int instance_end_of_file_(addr *ret, addr stream)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_END_OF_FILE,
			CONSTANT_CLOSNAME_STREAM, stream);
}

int call_end_of_file_(Execute ptr, addr stream)
{
	addr instance;
	Return(instance_end_of_file_(&instance, stream));
	return error_function_(ptr, instance);
}


/* reader_error (parse_error stream_error) :stream */
int instance_reader_error_(addr *ret, addr stream)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_READER_ERROR,
			CONSTANT_CLOSNAME_STREAM, stream);
}

int call_reader_error_(Execute ptr, addr stream)
{
	addr instance;
	Return(instance_reader_error_(&instance, stream));
	return error_function_(ptr, instance);
}


/* file_error (error) :pathname */
int instance_file_error_(addr *ret, addr pathname)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_FILE_ERROR,
			CONSTANT_CLOSNAME_PATHNAME, pathname);
}

int call_file_error_(Execute ptr, addr pathname)
{
	addr instance;
	Return(instance_file_error_(&instance, pathname));
	return error_function_(ptr, instance);
}

int file_error_pathname_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_PATHNAME, ret);
}


/* package_error (error) :package */
int instance_package_error_(addr *ret, addr package)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_PACKAGE_ERROR,
			CONSTANT_CLOSNAME_PACKAGE, package);
}

int call_package_error_(Execute ptr, addr package)
{
	addr instance;
	Return(instance_package_error_(&instance, package));
	return error_function_(ptr, instance);
}

int package_error_package_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_PACKAGE, ret);
}


/* parse_error (error) */
int instance_parse_error_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_PARSE_ERROR);
}

int call_parse_error_(Execute ptr)
{
	addr instance;
	Return(instance_parse_error_(&instance));
	return error_function_(ptr, instance);
}


/* print_not_readable (error) :object */
int instance_print_not_readable_(addr *ret, addr object)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_PRINT_NOT_READABLE,
			CONSTANT_CLOSNAME_OBJECT, object);
}

int call_print_not_readable_(Execute ptr, addr object)
{
	addr instance;
	Return(instance_print_not_readable_(&instance, object));
	return error_function_(ptr, instance);
}

int print_not_readable_object_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_OBJECT, ret);
}


/* program_error (error) */
int instance_program_error_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_PROGRAM_ERROR);
}

int call_program_error_(Execute ptr)
{
	addr instance;
	Return(instance_program_error_(&instance));
	return error_function_(ptr, instance);
}


/* style_warning (warning) */
int instance_style_warning_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_STYLE_WARNING);
}

int call_style_warning_(Execute ptr)
{
	addr instance;
	Return(instance_style_warning_(&instance));
	return error_function_(ptr, instance);
}


/* type_error (error) :datum :expected-type */
int instance_type_error_(addr *ret, addr datum, addr expected)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_TYPE_ERROR,
			CONSTANT_CLOSNAME_DATUM, datum,
			CONSTANT_CLOSNAME_EXPECTED_TYPE, expected);
}

int call_type_error_(Execute ptr, addr datum, addr expected)
{
	addr instance;

	copyheap(&datum, datum);
	copyheap(&expected, expected);
	Return(instance_type_error_(&instance, datum, expected));
	return error_function_(ptr, instance);
}

int call_type_error_const_(Execute ptr, addr datum, constindex expected)
{
	addr type;
	GetConstant(expected, &type);
	return call_type_error_(ptr, datum, type);
}

int type_error_datum_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_DATUM, ret);
}

int type_error_expected_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_EXPECTED_TYPE, ret);
}

int call_typep_error_(Execute ptr, addr value, addr type)
{
	int check;

	Return(typep_clang_(ptr, value, type, &check));
	if (! check) {
		copyheap(&value, value);
		type_copy_heap(&type, type);
		return call_type_error_(ptr, value, type);
	}

	return 0;
}

int call_typep_asterisk_error_(Execute ptr, addr value, addr type)
{
	int check;

	Return(typep_asterisk_clang_(ptr, value, type, &check));
	if (! check) {
		copyheap(&value, value);
		type_copy_heap(&type, type);
		return call_type_error_(ptr, value, type);
	}

	return 0;
}

int call_typep_unbound_error_(Execute ptr, addr value, addr type)
{
	return (value == Unbound)? 0: call_typep_error_(ptr, value, type);
}


/* simple_type_error (simple_condition type_error)
 *   :format-control :format-arguments :datum :expected-type */
int instance_simple_type_error_(addr *ret,
		addr control, addr args, addr datum, addr expected)
{
	return instance_condition4_(ret, CONSTANT_CONDITION_SIMPLE_TYPE_ERROR,
			CONSTANT_CLOSNAME_FORMAT_CONTROL, control,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args,
			CONSTANT_CLOSNAME_DATUM, datum,
			CONSTANT_CLOSNAME_EXPECTED_TYPE, expected);
}

int call_simple_type_error_(Execute ptr,
		addr control, addr args, addr datum, addr expected)
{
	addr instance;
	Return(instance_simple_type_error_(&instance, control, args, datum, expected));
	return error_function_(ptr, instance);
}

int call_type_error_va_(Execute ptr,
		addr datum, addr expected, const char *fmt, ...)
{
	addr control, args;
	va_list va;

	strvect_char_heap(&control, fmt);
	va_start(va, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	copylocal_object(NULL, &datum, datum);
	copylocal_object(NULL, &expected, expected);
	return call_simple_type_error_(ptr, control, args, datum, expected);
}

int call_type_error_fill_pointer_(Execute ptr, addr datum)
{
	addr expected;
	GetConst(COMMON_ARRAY, &expected);
	return call_type_error_va_(ptr, datum, expected,
			"The vector ~S don't have a fill-pointer.", datum, NULL);
}

int call_type_error_fill_pointer_zero_(Execute ptr, addr datum)
{
	addr expected;
	GetConst(COMMON_VECTOR, &expected);
	return call_type_error_va_(ptr, datum, expected,
			"The vector ~S fill-pointer is 0.", datum, NULL);
}

int call_type_error_adjustable_(Execute ptr, addr datum)
{
	addr expected;
	GetConst(COMMON_VECTOR, &expected);
	return call_type_error_va_(ptr, datum, expected,
			"The vector ~S is not adjustable.", datum, NULL);
}


/* unbound_slot (cell_error) :instance :name */
int instance_unbound_slot_(addr *ret, addr instance, addr name)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_UNBOUND_SLOT,
			CONSTANT_CLOSNAME_INSTANCE, instance,
			CONSTANT_CLOSNAME_NAME, name);
}

int unbound_slot_instance_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_INSTANCE, ret);
}

int call_unbound_slot_(Execute ptr, addr argument, addr name)
{
	addr instance;
	Return(instance_unbound_slot_(&instance, argument, name));
	return error_function_(ptr, instance);
}


/* unbound_variable (cell_error) :name */
int instance_unbound_variable_(addr *ret, addr name)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_UNBOUND_VARIABLE,
			CONSTANT_CLOSNAME_NAME, name);
}

int call_unbound_variable_(Execute ptr, addr name)
{
	addr instance;
	Return(instance_unbound_variable_(&instance, name));
	return error_function_(ptr, instance);
}


/* undefined_function (cell_error) :name */
int instance_undefined_function_(addr *ret, addr name)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_UNDEFINED_FUNCTION,
			CONSTANT_CLOSNAME_NAME, name);
}

int call_undefined_function_(Execute ptr, addr name)
{
	addr instance;
	Return(instance_undefined_function_(&instance, name));
	return error_function_(ptr, instance);
}


/* savecore */
int instance_savecore_condition_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_SAVECORE);
}

int call_savecore_condition_(Execute ptr)
{
	addr instance;
	Return(instance_savecore_condition_(&instance));
	return error_function_(ptr, instance);
}


/* exit */
int instance_exit_condition_(addr *ret, addr value)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_EXIT,
			CONSTANT_CLOSNAME_VALUE, value);
}

int call_exit_condition_(Execute ptr, addr value)
{
	addr instance;
	Return(instance_exit_condition_(&instance, value));
	return error_function_(ptr, instance);
}

int exit_condition_value_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_VALUE, ret);
}


/* simple_control_error */
int instance_simple_control_error_(addr *ret, addr control, addr args)
{
	addr instance;

	GetConst(CONDITION_SIMPLE_CONTROL_ERROR, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_CONTROL, control));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args));
	return Result(ret, instance);
}

int call_simple_control_error_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_control_error_(&instance, control, args));
	return error_function_(ptr, instance);
}

int call_simple_control_error_va_(Execute ptr, const char *fmt, ...)
{
	addr control, args;
	va_list va;

	strvect_char_heap(&control, fmt);
	va_start(va, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return call_simple_control_error_(ptr, control, args);
}


/* simple_file_error */
int instance_simple_file_error_(addr *ret, addr pathname, addr control, addr args)
{
	addr instance;

	GetConst(CONDITION_SIMPLE_FILE_ERROR, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_PATHNAME, pathname));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_CONTROL, control));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args));
	return Result(ret, instance);
}

int call_simple_file_error_(Execute ptr, addr pathname, addr control, addr args)
{
	addr instance;
	Return(instance_simple_file_error_(&instance, pathname, control, args));
	return error_function_(ptr, instance);
}

int call_simple_file_error_va_(Execute ptr, addr pathname, const char *fmt, ...)
{
	addr control, args;
	va_list va;

	strvect_char_heap(&control, fmt);
	va_start(va, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	copylocal_object(NULL, &pathname, pathname);
	return call_simple_file_error_(ptr, pathname, control, args);
}


/* simple_package_error */
int instance_simple_package_error_(addr *ret, addr control, addr args)
{
	addr instance;

	GetConst(CONDITION_SIMPLE_PACKAGE_ERROR, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_CONTROL, control));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args));
	return Result(ret, instance);
}

int call_simple_package_error_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_package_error_(&instance, control, args));
	return error_function_(ptr, instance);
}

int call_simple_package_error_va_(Execute ptr, const char *fmt, ...)
{
	addr control, args;
	va_list va;

	strvect_char_heap(&control, fmt);
	va_start(va, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return call_simple_package_error_(ptr, control, args);
}


/* simple_parse_error */
int instance_simple_parse_error_(addr *ret, addr control, addr args)
{
	addr instance;

	GetConst(CONDITION_SIMPLE_PARSE_ERROR, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_CONTROL, control));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args));
	return Result(ret, instance);
}

int call_simple_parse_error_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_parse_error_(&instance, control, args));
	return error_function_(ptr, instance);
}

int call_simple_parse_error_va_(Execute ptr, const char *fmt, ...)
{
	addr control, args;
	va_list va;

	strvect_char_heap(&control, fmt);
	va_start(va, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return call_simple_parse_error_(ptr, control, args);
}


/* simple_program_error */
int instance_simple_program_error_(addr *ret, addr control, addr args)
{
	addr instance;

	GetConst(CONDITION_SIMPLE_PROGRAM_ERROR, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_CONTROL, control));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args));
	return Result(ret, instance);
}

int call_simple_program_error_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_program_error_(&instance, control, args));
	return error_function_(ptr, instance);
}

int call_simple_program_error_stdarg_(Execute ptr, const char *fmt, va_list va)
{
	addr control, args;

	strvect_char_heap(&control, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	return call_simple_program_error_(ptr, control, args);
}

int call_simple_program_error_va_(Execute ptr, const char *fmt, ...)
{
	addr control, args;
	va_list va;

	strvect_char_heap(&control, fmt);
	va_start(va, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return call_simple_program_error_(ptr, control, args);
}


/* simple_reader_error */
int instance_simple_reader_error_(addr *ret, addr control, addr args)
{
	addr instance;

	GetConst(CONDITION_SIMPLE_READER_ERROR, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_CONTROL, control));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args));
	return Result(ret, instance);
}

int call_simple_reader_error_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_reader_error_(&instance, control, args));
	return error_function_(ptr, instance);
}

int call_simple_reader_error_va_(Execute ptr, const char *fmt, ...)
{
	addr control, args;
	va_list va;

	strvect_char_heap(&control, fmt);
	va_start(va, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return call_simple_reader_error_(ptr, control, args);
}


/* simple_style_warning */
int instance_simple_style_warning_(addr *ret, addr control, addr args)
{
	addr instance;

	GetConst(CONDITION_SIMPLE_STYLE_WARNING, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_CONTROL, control));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args));
	return Result(ret, instance);
}

int call_simple_style_warning_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_style_warning_(&instance, control, args));
	return warning_restart_case_(ptr, instance);
}

int call_simple_style_warning_va_(Execute ptr, const char *fmt, ...)
{
	addr control, args;
	va_list va;

	strvect_char_heap(&control, fmt);
	va_start(va, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return call_simple_style_warning_(ptr, control, args);
}


/************************************************************
 *  cons.c
 ************************************************************/

/*
 *  cons
 */
int consp_getcons(addr list, addr *car, addr *cdr)
{
	if (GetType(list) != LISPTYPE_CONS)
		return 0;
	GetCons(list, car, cdr);
	return 1;
}

int consp_getcar(addr list, addr *car)
{
	if (GetType(list) != LISPTYPE_CONS)
		return 0;
	GetCar(list, car);
	return 1;
}

int consp_getcdr(addr list, addr *cdr)
{
	if (GetType(list) != LISPTYPE_CONS)
		return 0;
	GetCdr(list, cdr);
	return 1;
}

int getcons_(addr list, addr *car, addr *cdr)
{
	if (! listp(list))
		return TypeError_(list, LIST);
	GetCons(list, car, cdr);
	return 0;
}

int getcar_(addr list, addr *car)
{
	if (! listp(list))
		return TypeError_(list, LIST);
	GetCar(list, car);
	return 0;
}

int getcdr_(addr list, addr *cdr)
{
	if (! listp(list))
		return TypeError_(list, LIST);
	GetCdr(list, cdr);
	return 0;
}

int setcons_(addr cons, addr car, addr cdr)
{
	if (! consp(cons))
		return TypeError_(cons, CONS);
	SetCons(cons, car, cdr);
	return 0;
}

int setcar_(addr cons, addr car)
{
	if (! consp(cons))
		return TypeError_(cons, CONS);
	SetCar(cons, car);
	return 0;
}

int setcdr_(addr cons, addr cdr)
{
	if (! consp(cons))
		return TypeError_(cons, CONS);
	SetCdr(cons, cdr);
	return 0;
}


/*
 *  list
 */
void list_stdarg_alloc(LocalRoot local, addr *ret, va_list args)
{
	addr left, right, next;

	left = va_arg(args, addr);
	if (left == NULL) {
		*ret = Nil;
		return;
	}
	conscar_alloc(local, &right, left);
	*ret = right;

	for (;;) {
		left = va_arg(args, addr);
		if (left == NULL)
			break;
		conscar_alloc(local, &next, left);
		SetCdr(right, next);
		right = next;
	}
}

void list_heap(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	list_stdarg_alloc(NULL, ret, args);
	va_end(args);
}

void list_local(LocalRoot local, addr *ret, ...)
{
	va_list args;

	Check(local == NULL, "local error");
	va_start(args, ret);
	list_stdarg_alloc(local, ret, args);
	va_end(args);
}

void list_alloc(LocalRoot local, addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	list_stdarg_alloc(local, ret, args);
	va_end(args);
}

void pushva_heap(addr *list, ...)
{
	addr root, pos;
	va_list args;

	root = *list;
	va_start(args, list);
	for (;;) {
		pos = va_arg(args, addr);
		if (pos == NULL)
			break;
		cons_heap(&root, pos, root);
	}
	va_end(args);
	*list = root;
}


/*
 *  list*
 */
/* `(list* ,first ,@cons) */
int lista_safe_alloc_(LocalRoot local, addr *ret, addr first, addr cons)
{
	addr pos, root;

	for (root = Nil; cons != Nil; first = pos) {
		if (GetType(cons) != LISPTYPE_CONS) {
			*ret = Nil;
			return fmte_("The argument ~S must be a list.", cons, NULL);
		}
		GetCons(cons, &pos, &cons);
		cons_alloc(local, &root, first, root);
	}

	/* nil */
	if (root == Nil)
		return Result(ret, first);

	/* loop */
	for (;;) {
		GetCdr(root, &pos);
		SetCdr(root, first);
		if (pos == Nil)
			break;
		first = root;
		root = pos;
	}

	return Result(ret, root);
}
int lista_safe_local_(LocalRoot local, addr *ret, addr first, addr cons)
{
	Check(local == NULL, "local error");
	return lista_safe_alloc_(local, ret, first, cons);
}
int lista_safe_heap_(addr *ret, addr first, addr cons)
{
	return lista_safe_alloc_(NULL, ret, first, cons);
}

static void lista_stdarg(LocalRoot local, addr *ret, va_list args, addr pos1)
{
	addr pos2, pos3, cons;

	/* nil */
	if (pos1 == NULL) {
		*ret = Nil;
		return;
	}

	/* dot list */
	pos2 = va_arg(args, addr);
	if (pos2 == NULL) {
		*ret = pos1;
		return;
	}

	/* result */
	conscar_alloc(local, &cons, pos1);
	*ret = cons;

	/* loop */
	for (;;) {
		pos3 = va_arg(args, addr);
		if (pos3 == NULL) {
			/* (pos1 . pos2) */
			SetCdr(cons, pos2);
			return;
		}

		/* (pos1 pos2 . ?) */
		conscar_alloc(local, &pos1, pos2);
		SetCdr(cons, pos1);
		cons = pos1;
		pos2 = pos3;
	}
}

void lista_stdarg_noerror(LocalRoot local, addr *ret, va_list args)
{
	addr pos1 = va_arg(args, addr);
	lista_stdarg(local, ret, args, pos1);
}

int lista_stdarg_safe_(LocalRoot local, addr *ret, va_list args)
{
	addr pos1;

	pos1 = va_arg(args, addr);
	/* nil */
	if (pos1 == NULL) {
		*ret = Nil;
		return fmte_("LIST* must be at least one argument.", NULL);
	}
	lista_stdarg(local, ret, args, pos1);

	return 0;
}

void lista_stdarg_alloc(LocalRoot local, addr *ret, va_list args)
{
	addr pos1, pos2, pos3, cons;

	pos1 = va_arg(args, addr);
	/* nil */
	Check(pos1 == NULL, "lista at least one argument.");
	if (pos1 == NULL) {
		*ret = Nil; /* error */
		return;
	}

	/* dot list */
	pos2 = va_arg(args, addr);
	if (pos2 == NULL) {
		*ret = pos1;
		return;
	}

	/* result */
	conscar_alloc(local, &cons, pos1);
	*ret = cons;

	/* loop */
	for (;;) {
		pos3 = va_arg(args, addr);
		if (pos3 == NULL) {
			/* (pos1 . pos2) */
			SetCdr(cons, pos2);
			return;
		}

		/* (pos1 pos2 . ?) */
		conscar_alloc(local, &pos1, pos2);
		SetCdr(cons, pos1);
		cons = pos1;
		pos2 = pos3;
	}
}

void lista_alloc(LocalRoot local, addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	lista_stdarg_alloc(local, ret, args);
	va_end(args);
}

void lista_local(LocalRoot local, addr *ret, ...)
{
	va_list args;

	Check(local == NULL, "local error");
	va_start(args, ret);
	lista_stdarg_alloc(local, ret, args);
	va_end(args);
}

void lista_heap(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	lista_stdarg_alloc(NULL, ret, args);
	va_end(args);
}


/*
 *  bind
 */
void List_bind(addr list, ...)
{
	addr pos, *ret;
	va_list args;

	va_start(args, list);
	for (;;) {
		ret = va_arg(args, addr *);
		if (ret == NULL) {
			Check(list != Nil, "list nil error");
			break;
		}
		Check(! consp(list), "list error");
		GetCons(list, &pos, &list);
		*ret = pos;
	}
	va_end(args);
}

int list_bind_(addr list, ...)
{
	addr pos, *ret;
	va_list args;

	pos = Nil;
	va_start(args, list);
	for (;;) {
		ret = va_arg(args, addr *);
		if (ret == NULL) {
			if (list != Nil)
				return fmte_("Too many list argument.", NULL);
			break;
		}
		if (list == Nil)
			return fmte_("Too few list argument.", NULL);
		Return_getcons(list, &pos, &list);
		*ret = pos;
	}
	va_end(args);

	return 0;
}

void Lista_bind(addr list, ...)
{
	addr *ret1, *ret2, pos;
	va_list args;

	va_start(args, list);
	ret1 = va_arg(args, addr *);
	if (ret1 == NULL) {
		Check(list != Nil, "list nil error");
		goto finish;
	}
	for (;;) {
		ret2 = va_arg(args, addr *);
		if (ret2 == NULL) {
			*ret1 = list;
			break;
		}
		Check(! consp(list), "list error");
		GetCons(list, &pos, &list);
		*ret1 = pos;
		ret1 = ret2;
	}
finish:
	va_end(args);
}

int lista_bind_(addr list, ...)
{
	addr *ret1, *ret2, pos;
	va_list args;

	pos = Nil;
	va_start(args, list);
	ret1 = va_arg(args, addr *);
	if (ret1 == NULL) {
		if (list != Nil)
			return fmte_("Too few argument.", NULL);
		goto finish;
	}
	for (;;) {
		ret2 = va_arg(args, addr *);
		if (ret2 == NULL) {
			*ret1 = list;
			break;
		}
		if (! consp(list))
			return fmte_("Too few argument.", NULL);
		Return_getcons(list, &pos, &list);
		*ret1 = pos;
		ret1 = ret2;
	}
finish:
	va_end(args);

	return 0;
}


/*
 *  copy-tree
 */
void copy_tree_alloc(LocalRoot local, addr *ret, addr cdr)
{
	addr car;

	if (GetType(cdr) != LISPTYPE_CONS) {
		*ret = cdr;
	}
	else {
		GetCons(cdr, &car, &cdr);
		copy_tree_alloc(local, &car, car);
		copy_tree_alloc(local, &cdr, cdr);
		cons_alloc(local, ret, car, cdr);
	}
}

void copy_tree_local(LocalRoot local, addr *ret, addr list)
{
	Check(local == NULL, "local error");
	copy_tree_alloc(local, ret, list);
}

void copy_tree_heap(addr *ret, addr list)
{
	copy_tree_alloc(NULL, ret, list);
}


/************************************************************
 *  cons_list.c
 ************************************************************/

/*
 *  nth
 */
void getnth_abort(addr cons, size_t index, addr *ret)
{
	for (; index; index--) {
		if (cons == Nil)
			break;
		if (! consp(cons))
			Abort("Type error");
		GetCdr(cons, &cons);
	}
	if (! consp(cons))
		Abort("Type error");
	GetCar(cons, ret);
}

int getnth_(addr cons, size_t index, addr *ret)
{
	for (; index; index--) {
		if (cons == Nil)
			break;
		Return_getcdr(cons, &cons);
	}
	return getcar_(cons, ret);
}

int getnth_large_(addr cons, addr index, addr *ret)
{
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	Return(bignum_counter_alloc_(local, &index, index));
	while (! zerop_bignum(index)) {
		if (cons == Nil) {
			*ret = Nil;
			goto finish;
		}
		Return_getcdr(cons, &cons);
		decf_bignum(index, 1);
	}
	Return_getcar(cons, ret);
finish:
	rollback_local(local, stack);
	return 0;
}

void getnth_unbound_unsafe(addr cons, size_t index, addr *ret)
{
	for (;;) {
		if (cons == Nil) {
			*ret = Unbound;
			return;
		}
		if (index-- == 0)
			break;
		GetCdr(cons, &cons);
	}
	GetCar(cons, ret);
}

void getnth_unsafe(addr cons, size_t index, addr *ret)
{
	for (; index; index--) {
		if (cons == Nil)
			break;
		GetCdr(cons, &cons);
	}
	GetCar(cons, ret);
}

int getnthcdr_(addr cons, size_t index, addr *ret)
{
	for (; index; index--) {
		if (cons == Nil)
			break;
		Return_getcdr(cons, &cons);
	}
	return Result(ret, cons);
}

int getnthcdr_large_(addr cons, addr index, addr *ret)
{
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	Return(bignum_counter_alloc_(local, &index, index));
	while (! zerop_bignum(index)) {
		if (cons == Nil) {
			*ret = Nil;
			goto finish;
		}
		Return_getcdr(cons, &cons);
		decf_bignum(index, 1);
	}
	*ret = cons;
finish:
	rollback_local(local, stack);
	return 0;
}

void getnthcdr_unsafe(addr cons, size_t index, addr *ret)
{
	while (index--) {
		if (cons == Nil) {
			*ret = Nil;
			return;
		}
		GetCdr(cons, &cons);
	}
	*ret = cons;
}

int setnth_(addr cons, size_t index, addr value)
{
	addr cdr;

	Return(getnthcdr_(cons, index, &cdr));
	if (! consp(cdr))
		return fmte_("Cannot (setf nth) ~S.", cons, NULL);
	SetCar(cdr, value);
	return 0;
}

void setnth_unsafe(addr cons, size_t index, addr value)
{
	addr cdr;

	getnthcdr_unsafe(cons, index, &cdr);
	Check(! consp(cdr), "type error");
	SetCar(cdr, value);
}


/*
 *  length
 */
size_t length_list_unsafe(addr list)
{
	size_t size;

	for (size = 0; list != Nil; size++) {
		GetCdr(list, &list);
	}

	return size;
}

int length_list_safe_(addr list, size_t *ret)
{
	size_t size;

	for (size = 0; list != Nil; size++) {
		if (GetType(list) != LISPTYPE_CONS) {
			*ret = 0;
			return fmte_("cdr position must be a list type.", NULL);
		}
		GetCdr(list, &list);
	}

	return Result(ret, size);
}

int length_list_p(addr list, size_t *ret)
{
	size_t i;

	for (i = 0; list != Nil; i++) {
		if (GetType(list) != LISPTYPE_CONS) {
			*ret = i;
			return 1;
		}
		GetCdr(list, &list);
	}
	*ret = i;

	return 0;
}


/*
 *  list
 */
int nconc2_safe_(addr left, addr right, addr *ret)
{
	addr check;

	if (left == Nil)
		return Result(ret, right);

	*ret = left;
	if (right == Nil)
		return 0;
	for (;;) {
		Return_getcdr(left, &check);
		if (! consp(check)) {
			SetCdr(left, right);
			break;
		}
		left = check;
	}

	return 0;
}

void nconc2_unsafe(addr left, addr right, addr *ret)
{
	addr check;

	if (left == Nil) {
		*ret = right;
		return;
	}
	*ret = left;
	if (right == Nil) {
		return;
	}
	for (;;) {
		GetCdr(left, &check);
		if (! consp(check)) {
			SetCdr(left, right);
			break;
		}
		left = check;
	}
}

int append2_safe_(addr left, addr right, addr *ret)
{
	addr root, pos;

	if (left == Nil)
		return Result(ret, right);

	if (right == Nil)
		return Result(ret, left);

	for (root = Nil; left != Nil; ) {
		Return_getcons(left, &pos, &left);
		cons_heap(&root, pos, root);
	}

	return nreconc_safe_(ret, root, right);
}

void append2_alloc_unsafe(LocalRoot local, addr list1, addr list2, addr *ret)
{
	addr stack, left;

	if (list1 == Nil) {
		*ret = list2;
		return;
	}
	if (list2 == Nil) {
		*ret = list1;
		return;
	}

	stack = Nil;
	do {
		GetCons(list1, &left, &list1);
		cons_alloc(local, &stack, left, stack);
	} while (list1 != Nil);

	for (;;) {
		GetCdr(stack, &list1);
		SetCdr(stack, list2);
		if (list1 == Nil)
			break;
		list2 = stack;
		stack = list1;
	}
	*ret = stack;
}

void append2_heap_unsafe(addr list1, addr list2, addr *ret)
{
	append2_alloc_unsafe(NULL, list1, list2, ret);
}

void append2_local_unsafe(LocalRoot local, addr list1, addr list2, addr *ret)
{
	Check(local == NULL, "local error");
	append2_alloc_unsafe(local, list1, list2, ret);
}

void butandlast_safe(addr *but, addr *last, addr list, size_t index)
{
	addr root, pos;
	size_t size;

	length_list_p(list, &size);
	if (size <= index) {
		*but = Nil;
		*last = list;
		return;
	}
	size -= index;
	for (root = Nil; size--; ) {
		GetCons(list, &pos, &list);
		cons_heap(&root, pos, root);
	}
	nreverse(but, root);
	*last = list;
}

int setlastcdr_safe_(addr list, addr cdr)
{
	addr check;

	for (;;) {
		Return_getcdr(list, &check);
		if (! consp(check)) {
			SetCdr(list, cdr);
			return 0;
		}
		list = check;
	}

	return 0;
}


/*
 *  find
 */
int find_list_eq_unsafe(addr key, addr cons)
{
	addr check;

	while (cons != Nil) {
		GetCons(cons, &check, &cons);
		if (check == key)
			return 1;
	}

	return 0;
}

int find_list_eq_safe_(addr key, addr cons, int *ret)
{
	addr check;

	while (cons != Nil) {
		Return_getcons(cons, &check, &cons);
		if (check == key)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int find_list_eql_unsafe(addr key, addr cons)
{
	addr check;

	while (cons != Nil) {
		GetCons(cons, &check, &cons);
		if (eql_function(check, key))
			return 1;
	}

	return 0;
}

int find_list_equal_safe_(addr key, addr cons, int *ret)
{
	int check;
	addr pos;

	*ret = 0;
	while (consp_getcons(cons, &pos, &cons)) {
		Return(equal_function_(pos, key, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int position_list_eq_unsafe(addr key, addr cons, size_t *ret)
{
	addr check;
	size_t i;

	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &check, &cons);
		if (check == key) {
			*ret = i;
			return 1;
		}
	}

	return 0;
}

int find_assoc_eq_unsafe(addr key, addr list, addr *ret)
{
	addr cons, x;

	while (list != Nil) {
		GetCons(list, &cons, &list);
		GetCar(cons, &x);
		if (key == x) {
			*ret = cons;
			return 1;
		}
	}

	*ret = Nil;
	return 0;
}


/*
 *  pushnew
 */
int pushnew_alloc(LocalRoot local, addr list, addr value, addr *ret)
{
	if (! find_list_eq_unsafe(value, list)) {
		cons_alloc(local, ret, value, list);
		return 1;
	}

	return 0;
}

int pushnew_local(LocalRoot local, addr list, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return pushnew_alloc(local, list, value, ret);
}

int pushnew_heap(addr list, addr value, addr *ret)
{
	return pushnew_alloc(NULL, list, value, ret);
}

static int find_list_equal_unsafe_(addr key, addr list, int *ret)
{
	int check;
	addr value;

	while (list != Nil) {
		GetCons(list, &value, &list);
		Return(equal_function_(value, key, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int pushnew_equal_heap_(addr list, addr value, addr *ret)
{
	int check;

	Return(find_list_equal_unsafe_(value, list, &check));
	if (! check)
		cons_heap(ret, value, list);

	return 0;
}


/*
 *  nreverse
 */
void nreconc_unsafe(addr *ret, addr cons, addr tail)
{
	addr next;

	/* nil */
	if (cons == Nil) {
		*ret = tail;
		return;
	}

	/* loop */
	for (;;) {
		GetCdr(cons, &next);
		SetCdr(cons, tail);
		if (next == Nil)
			break;
		tail = cons;
		cons = next;
	}
	*ret = cons;
}

int nreconc_safe_(addr *ret, addr cons, addr tail)
{
	addr next;

	/* nil */
	if (cons == Nil)
		return Result(ret, tail);

	/* loop */
	for (;;) {
		Return_getcdr(cons, &next);
		Return_setcdr(cons, tail);
		if (next == Nil)
			break;
		tail = cons;
		cons = next;
	}

	return Result(ret, cons);
}

void nreverse_list_unsafe(addr *ret, addr cons)
{
	addr tail, next;

	/* nil */
	if (cons == Nil) {
		*ret = Nil;
		return;
	}

	/* loop */
	for (tail = Nil; ; tail = cons, cons = next) {
		GetCdr(cons, &next);
		SetCdr(cons, tail);
		if (next == Nil)
			break;
	}
	*ret = cons;
}

int nreverse_list_safe_(addr *ret, addr cons)
{
	addr tail, next;

	/* nil */
	if (cons == Nil)
		return Result(ret, Nil);

	/* loop */
	for (tail = Nil; ; tail = cons, cons = next) {
		Return_getcdr(cons, &next);
		Return_setcdr(cons, tail);
		if (next == Nil)
			break;
	}

	return Result(ret, cons);
}


/*
 *  reverse
 */
void reverse_list_heap_unsafe(addr *ret, addr cons)
{
	reverse_list_alloc_unsafe(NULL, ret, cons);
}

void reverse_list_local_unsafe(LocalRoot local, addr *ret, addr cons)
{
	Check(local == NULL, "local error");
	reverse_list_alloc_unsafe(local, ret, cons);
}

void reverse_list_alloc_unsafe(LocalRoot local, addr *ret, addr cons)
{
	addr root, left;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &left, &cons);
		cons_alloc(local, &root, left, root);
	}
	*ret = root;
}

int reverse_list_heap_safe_(addr *ret, addr cons)
{
	addr root, left;

	for (root = Nil; cons != Nil; ) {
		Return_getcons(cons, &left, &cons);
		cons_heap(&root, left, root);
	}

	return Result(ret, root);
}


/*
 *  callname
 */
int pushnewlist_callname_alloc(LocalRoot local, addr list, addr callname, addr *ret)
{
	if (! find_list_callname_unsafe(callname, list)) {
		cons_alloc(local, ret, callname, list);
		return 1;
	}

	return 0;
}

int pushnewlist_callname_heap(addr list, addr callname, addr *ret)
{
	return pushnewlist_callname_alloc(NULL, list, callname, ret);
}

int find_list_callname_unsafe(addr callname, addr list)
{
	addr check;

	while (list != Nil) {
		GetCons(list, &check, &list);
		if (equal_callname(check, callname))
			return 1;
	}

	return 0;
}


/*
 *  copy-list
 */
void copy_list_alloc_unsafe(LocalRoot local, addr *ret, addr cons)
{
	addr root, left;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &left, &cons);
		cons_alloc(local, &root, left, root);
	}
	nreverse(ret, root);
}
void copy_list_local_unsafe(LocalRoot local, addr *ret, addr cons)
{
	Check(local == NULL, "local error");
	copy_list_alloc_unsafe(local, ret, cons);
}
void copy_list_heap_unsafe(addr *ret, addr cons)
{
	copy_list_alloc_unsafe(NULL, ret, cons);
}

void copy_list_alloc_safe(LocalRoot local, addr *ret, addr cons)
{
	addr root, last, pos;

	root = last = Nil;
	for (;;) {
		if (GetType(cons) == LISPTYPE_CONS) {
			GetCons(cons, &pos, &cons);
			cons_alloc(local, &root, pos, root);
		}
		else {
			last = cons;
			break;
		}
	}
	nreconc(ret, root, last);
}
void copy_list_local_safe(LocalRoot local, addr *ret, addr cons)
{
	Check(local == NULL, "local error");
	copy_list_alloc_safe(local, ret, cons);
}
void copy_list_heap_safe(addr *ret, addr cons)
{
	copy_list_alloc_safe(NULL, ret, cons);
}


/*
 *  delete / remove
 */
int delete_list_eq_unsafe(addr key, addr cons, addr *ret)
{
	int update;
	addr check, cons1, cons2;

	update = 0;
	*ret = cons;
	cons2 = Nil;
	while (cons != Nil) {
		GetCons(cons, &check, &cons1);
		if (eq_function(check, key)) {
			if (cons2 == Nil)
				*ret = cons1;
			else
				SetCdr(cons2, cons1);
			update = 1;
		}
		else {
			cons2 = cons;
		}
		cons = cons1;
	}

	return update;
}

int delete_list_eq_safe(addr key, addr cons, addr *ret)
{
	int update;
	addr check, cons1, cons2;

	update = 0;
	*ret = cons;
	cons2 = Nil;
	while (consp_getcons(cons, &check, &cons1)) {
		if (eq_function(check, key)) {
			if (cons2 == Nil)
				*ret = cons1;
			else
				SetCdr(cons2, cons1);
			update = 1;
		}
		else {
			cons2 = cons;
		}
		cons = cons1;
	}

	return update;
}

int delete_list_equal_unsafe_(addr key, addr cons, addr *root, int *ret)
{
	int check, update;
	addr value, cons1, cons2;

	update = 0;
	*root = cons;
	cons2 = Nil;
	while (cons != Nil) {
		GetCons(cons, &value, &cons1);
		Return(equal_function_(value, key, &check));
		if (check) {
			if (cons2 == Nil)
				*root = cons1;
			else
				SetCdr(cons2, cons1);
			update++;
		}
		else {
			cons2 = cons;
		}
		cons = cons1;
	}

	return Result(ret, update);
}

int delete1_list_eq_unsafe(addr key, addr cons, addr *ret)
{
	addr check, cons1, cons2;

	*ret = cons;
	cons2 = Nil;
	while (cons != Nil) {
		GetCons(cons, &check, &cons1);
		if (check == key) {
			if (cons2 == Nil)
				*ret = cons1;
			else
				SetCdr(cons2, cons1);
			return 1;
		}
		else {
			cons2 = cons;
		}
		cons = cons1;
	}

	return 0;
}

void remove_list_eq_unsafe_alloc(LocalRoot local,
		addr key, addr cons, addr *ret)
{
	addr result, check;

	for (result = Nil; cons != Nil; ) {
		GetCons(cons, &check, &cons);
		if (check != key)
			cons_alloc(local, &result, check, result);
	}
	nreverse(ret, result);
}

void remove_list_eq_unsafe_heap(addr key, addr cons, addr *ret)
{
	remove_list_eq_unsafe_alloc(NULL, key, cons, ret);
}

void remove_list_eq_unsafe_local(LocalRoot local,
		addr key, addr cons, addr *ret)
{
	Check(local == NULL, "local error");
	remove_list_eq_unsafe_alloc(local, key, cons, ret);
}

int remove_list_equal_safe_heap_(addr key, addr cons, addr *ret)
{
	int check;
	addr root, pos;

	root = Nil;
	while (consp_getcons(cons, &pos, &cons)) {
		Return(equal_function_(pos, key, &check));
		if (! check)
			cons_heap(&root, pos, root);
	}
	nreverse(ret, root);

	return 0;
}


/************************************************************
 *  cons_plist.c
 ************************************************************/

/* 0:find-value, 1:not-found(Nil) */
int getplist(addr plist, addr key, addr *ret)
{
	addr check;

	while (plist != Nil) {
		Check(GetType(plist) != LISPTYPE_CONS, "type left error");
		GetCons(plist, &check, &plist);
		Check(GetType(plist) != LISPTYPE_CONS, "type right error");
		if (check == key) {
			GetCar(plist, ret);
			return 0;
		}
		GetCdr(plist, &plist);
	}
	*ret = Nil;

	return 1;
}

int getplist_safe(addr plist, addr key, addr *ret)
{
	addr check;

	for (;;) {
		if (GetType(plist) != LISPTYPE_CONS)
			break;
		GetCons(plist, &check, &plist);
		if (check == key) {
			if (GetType(plist) != LISPTYPE_CONS)
				break;
			GetCar(plist, ret);
			return 0;
		}
		if (GetType(plist) != LISPTYPE_CONS)
			break;
		GetCdr(plist, &plist);
	}
	*ret = Nil;
	return 1;
}

/* 0:find-and-set, 1:make-new-cons */
int setplist_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	addr check, cons;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (check == key) {
			SetCar(cons, value);
			return 0;
		}
		GetCdr(cons, &cons);
	}
	cons_alloc(local, &plist, value, plist);
	cons_alloc(local, ret, key, plist);
	return 1;
}
int setplist_local(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setplist_alloc(local, plist, key, value, ret);
}
int setplist_heap(addr plist, addr key, addr value, addr *ret)
{
	return setplist_alloc(NULL, plist, key, value, ret);
}

/* 0:find-and-set, 1:make-new-cons */
int setplist_alloc_safe(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	addr check, cons;

	for (cons = plist; cons != Nil; ) {
		if (GetType(cons) != LISPTYPE_CONS)
			break;
		GetCons(cons, &check, &cons);
		if (check == key) {
			if (GetType(cons) != LISPTYPE_CONS)
				break;
			SetCar(cons, value);
			return 0;
		}
		if (GetType(cons) != LISPTYPE_CONS)
			break;
		GetCdr(cons, &cons);
	}
	cons_alloc(local, &plist, value, plist);
	cons_alloc(local, ret, key, plist);
	return 1;
}
int setplist_local_safe(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setplist_alloc_safe(local, plist, key, value, ret);
}
int setplist_heap_safe(addr plist, addr key, addr value, addr *ret)
{
	return setplist_alloc_safe(NULL, plist, key, value, ret);
}

int pushnewplist_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	addr cons, check;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (check == key) {
			GetCar(cons, &check);
			if (pushnew_alloc(local, check, value, &check)) {
				SetCar(cons, check);
			}
			return 0;
		}
		GetCdr(cons, &cons);
	}
	conscar_alloc(local, &value, value);
	cons_alloc(local, &plist, value, plist);
	cons_alloc(local, ret, key, plist);
	return 1;
}
int pushnewplist_local(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return pushnewplist_alloc(local, plist, key, value, ret);
}
int pushnewplist_heap(addr plist, addr key, addr value, addr *ret)
{
	return pushnewplist_alloc(NULL, plist, key, value, ret);
}

int remplist_safe_(addr plist, addr key, addr *value, enum RemPlist *ret)
{
	addr root, check;

	/* first */
	if (plist == Nil) {
		*value = Nil;
		return Result(ret, RemPlist_NotFound);
	}
	Return_getcons(plist, &check, &root);
	if (check == key) {
		Return_getcdr(root, value);
		return Result(ret, RemPlist_Update);
	}

	/* second */
	*value = plist;
	for (;;) {
		Return_getcdr(root, &plist);
		if (plist == Nil)
			break;

		Return_getcons(plist, &check, &plist);
		if (check == key) {
			Return_getcdr(plist, &plist);
			Return_setcdr(root, plist);
			return Result(ret, RemPlist_Delete);
		}
		root = plist;
	}

	return Result(ret, RemPlist_NotFound);
}

enum RemPlist remplist_check(addr plist, addr key, addr *ret)
{
	addr root, check;

	/* first */
	if (plist == Nil) {
		*ret = Nil;
		return RemPlist_NotFound;
	}
	CheckType2(plist, LISPTYPE_CONS, "type left error");
	GetCons(plist, &check, &root);
	CheckType2(root, LISPTYPE_CONS, "type right error");
	if (check == key) {
		GetCdr(root, ret);
		return RemPlist_Update;
	}

	/* second */
	*ret = plist;
	for (;;) {
		GetCdr(root, &plist);
		if (plist == Nil)
			break;

		CheckType2(plist, LISPTYPE_CONS, "type left error");
		GetCons(plist, &check, &plist);
		Check(GetType(plist) != LISPTYPE_CONS, "type right error");
		if (check == key) {
			GetCdr(plist, &plist);
			SetCdr(root, plist);
			return RemPlist_Delete;
		}
		root = plist;
	}

	return RemPlist_NotFound;
}

int remplist(addr plist, addr key, addr *ret)
{
	return remplist_check(plist, key, ret) == RemPlist_Update;
}

static int remplist_alloc_(LocalRoot local, addr list, addr key, addr *value, int *ret)
{
	int deletep;
	addr root, pos, next;

	deletep = 0;
	for (root = Nil; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return_getcons(list, &next, &list);
		if (pos != key) {
			cons_alloc(local, &root, pos, root);
			cons_alloc(local, &root, next, root);
		}
		else {
			deletep = 1;
		}
	}
	nreverse(value, root);

	return Result(ret, deletep);
}

int remplist_local_(LocalRoot local, addr list, addr key, addr *value, int *ret)
{
	CheckLocal(local);
	return remplist_alloc_(local, list, key, value, ret);
}

int remplist_heap_(addr list, addr key, addr *value, int *ret)
{
	return remplist_alloc_(NULL, list, key, value, ret);
}


/* 0:find-value, 1:not-found(Nil) */
int getplist_constant(addr plist, constindex index, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	Check(key == Unbound, "unbound error");
	return getplist(plist, key, ret);
}
int getplist_constant_safe(addr plist, constindex index, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	Check(key == Unbound, "unbound error");
	return getplist_safe(plist, key, ret);
}

/* 0:find-and-set, 1:make-new-cons */
int setplist_constant_alloc(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	return setplist_alloc(local, plist, key, value, ret);
}
int setplist_constant_local(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setplist_constant_alloc(local, plist, index, value, ret);
}
int setplist_constant_heap(addr plist,
		constindex index, addr value, addr *ret)
{
	return setplist_constant_alloc(NULL, plist, index, value, ret);
}

int remplist_constant(addr plist, constindex index, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	return remplist(plist, key, ret);
}

int getpplist(addr plist, addr key1, addr key2, addr *ret)
{
	(void)getplist(plist, key1, &plist);
	return getplist(plist, key2, ret);
}

static int setpplist_equal_alloc(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret,
		int (*equal)(addr, addr))
{
	addr cons, find, child, check;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (check == key1) {
			GetCar(cons, &child);
			for (find = child; find != Nil; ) {
				Check(GetType(find) != LISPTYPE_CONS, "type find left error");
				GetCons(find, &check, &find);
				Check(GetType(find) != LISPTYPE_CONS, "type find right error");
				if ((*equal)(check, key2)) {
					SetCar(find, value);
					return 0;
				}
				GetCdr(find, &find);
			}
			cons_alloc(local, &child, value, child);
			cons_alloc(local, &child, key2, child);
			SetCar(cons, child);
			return 0;
		}
		GetCdr(cons, &cons);
	}
	conscar_alloc(local, &value, value);
	cons_alloc(local, &value, key2, value);
	cons_alloc(local, &plist, value, plist);
	cons_alloc(local, ret, key1, plist);
	return 1;
}
int setpplist_alloc(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret)
{
	return setpplist_equal_alloc(local, plist, key1, key2, value, ret, eq_function);
}
int setpplist_local(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setpplist_alloc(local, plist, key1, key2, value, ret);
}
int setpplist_heap(addr plist, addr key1, addr key2, addr value, addr *ret)
{
	return setpplist_alloc(NULL, plist, key1, key2, value, ret);
}


/*
 *  callname
 */
/* 0:find-value, 1:not-found(Nil) */
static int equal_callname_plist(addr x, addr y)
{
	if (x == y)
		return 1;
	return (GetType(x) == LISPTYPE_CALLNAME)
		&& (GetType(y) == LISPTYPE_CALLNAME)
		&& equal_callname(x, y);
}

int getplist_callname(addr plist, addr callname, addr *ret)
{
	addr check;

	while (plist != Nil) {
		Check(GetType(plist) != LISPTYPE_CONS, "type left error");
		GetCons(plist, &check, &plist);
		Check(GetType(plist) != LISPTYPE_CONS, "type right error");
		if (equal_callname_plist(check, callname)) {
			GetCar(plist, ret);
			return 0;
		}
		GetCdr(plist, &plist);
	}
	*ret = Nil;

	return 1;
}

int setplist_callname_alloc(LocalRoot local,
		addr plist, addr callname, addr value, addr *ret)
{
	addr check, cons;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (equal_callname_plist(check, callname)) {
			SetCar(cons, value);
			return 0;
		}
		GetCdr(cons, &cons);
	}
	cons_alloc(local, &plist, value, plist);
	cons_alloc(local, ret, callname, plist);
	return 1;
}

int setplist_callname_local(LocalRoot local,
		addr plist, addr callname, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setplist_callname_alloc(local, plist, callname, value, ret);
}
int setplist_callname_heap(addr plist, addr callname, addr value, addr *ret)
{
	return setplist_callname_alloc(NULL, plist, callname, value, ret);
}

int pushnewplist_callname_alloc(LocalRoot local,
		addr plist, addr key, addr callname, addr *ret)
{
	addr cons, check;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (check == key) {
			GetCar(cons, &check);
			if (pushnewlist_callname_alloc(local, check, callname, &check)) {
				SetCar(cons, check);
			}
			return 0;
		}
		GetCdr(cons, &cons);
	}
	conscar_alloc(local, &callname, callname);
	cons_alloc(local, &plist, callname, plist);
	cons_alloc(local, ret, key, plist);
	return 1;
}
int pushnewplist_callname_local(LocalRoot local,
		addr plist, addr key, addr callname, addr *ret)
{
	Check(local == NULL, "local error");
	return pushnewplist_callname_alloc(local, plist, key, callname, ret);
}
int pushnewplist_callname_heap(addr plist,
		addr key, addr callname, addr *ret)
{
	return pushnewplist_callname_alloc(NULL, plist, key, callname, ret);
}

int getpplist_callname(addr plist, addr key, addr callname, addr *ret)
{
	(void)getplist(plist, key, &plist);
	return getplist_callname(plist, callname, ret);
}

int setpplist_callname_alloc(LocalRoot local,
		addr plist, addr key, addr callname, addr value, addr *ret)
{
	return setpplist_equal_alloc(local, plist, key, callname, value, ret,
			equal_callname_plist);
}
int setpplist_callname_local(LocalRoot local,
		addr plist, addr key, addr callname, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setpplist_callname_alloc(local, plist, key, callname, value, ret);
}
int setpplist_callname_heap(addr plist,
		addr key, addr callname, addr value, addr *ret)
{
	return setpplist_callname_alloc(NULL, plist, key, callname, value, ret);
}


/************************************************************
 *  constant.c
 ************************************************************/

/*
 *  build_constant
 */
void build_constant(void)
{
	addr array;
	size_t i;

	heap_array4(&array, LISPSYSTEM_CONSTANT, CONSTANT_SIZE);
	for (i = 0; i < CONSTANT_SIZE; i++)
		SetArrayA4(array, i, Unbound);
	SetLispRoot(CONST, array);
}


/*
 *  interface
 */
int specialconstant_(constindex index, const char *package, const char *name)
{
	addr symbol;

	Return(internchar_(package, name, &symbol, NULL));
	Return(setspecial_symbol_(symbol));
	SetConstant(index, symbol);

	return 0;
}

void gensymconstant(constindex index, const char *name)
{
	addr string, symbol;

	strvect_char_heap(&string, name);
	symbol_heap(&symbol);
	SetNameSymbol(symbol, string);
	SetConstant(index, symbol);
}

int keywordconstant_(constindex index, const char *name)
{
	addr symbol;

	Return(internchar_keyword_(name, &symbol, NULL));
	SetConstant(index, symbol);

	return 0;
}

int commonconstant_(constindex index, const char *name)
{
	addr symbol;

	Return(interncommon_(name, &symbol, NULL));
	SetConstant(index, symbol);

	return 0;
}


/*
 *  symbol.c
 */
#define SYMBOLCHAR_SIZE	256

static void copy_symbolchar(char *dst, const char *src, size_t size)
{
	int c;
	size_t i;

	for (i = 0; ; i++) {
		if ((SYMBOLCHAR_SIZE - 1) <= i) {
			Abort("buffer overflow.");
		}
		c = src[i];
		if (c == 0) break;
		dst[i] = (c == '_')? '-': c;
	}
	dst[i] = 0;
}

int symbolchar_common_(constindex index, const char *name)
{
	char buffer[SYMBOLCHAR_SIZE];
	addr symbol;

	copy_symbolchar(buffer, name, SYMBOLCHAR_SIZE);
	Return(interncommon_(name, &symbol, NULL));
	SetConstant(index, symbol);

	return 0;
}

int symbolchar_keyword_(constindex index, const char *name)
{
	char buffer[SYMBOLCHAR_SIZE];
	addr symbol;

	copy_symbolchar(buffer, name, SYMBOLCHAR_SIZE);
	Return(internchar_keyword_(name, &symbol, NULL));
	SetConstant(index, symbol);

	return 0;
}

void quotelist_heap(addr *ret, addr name)
{
	addr quote;
	GetConst(COMMON_QUOTE, &quote);
	list_heap(ret, quote, name, NULL);
}

void pushconst_heap(addr *ret, constindex index)
{
	addr pos;
	GetConstant(index, &pos);
	cons_heap(ret, pos, *ret);
}


/************************************************************
 *  control.c
 ************************************************************/

void init_control(void)
{
	init_callbind_control();
}


/************************************************************
 *  control_callbind.c
 ************************************************************/

/*
 *  call_compiled_function_
 */
typedef const struct callbind_struct *CallStruct;
typedef int (*callbind_control)(Execute, addr, CallStruct);
static callbind_control CallBindTable[CallBind_size];

static void getname_callbind(addr pos, addr *ret)
{
	if (functionp(pos)) {
		GetNameFunction(pos, ret);
	}
	else {
		*ret = pos;
	}
}

static int call_callbind_code_(Execute ptr, addr pos, CallStruct call)
{
	Abort("Cannot call callbind_code in control.");
	return 0;
}

static int call_callbind_macro_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.macro)(ptr, var1, var2);

toofew:
	getname_callbind(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_none_(Execute ptr, addr pos, CallStruct call)
{
	return (call->call.none)();
}

static int call_callbind_any_(Execute ptr, addr pos, CallStruct call)
{
	return (call->call.any)(ptr);
}

static int call_callbind_empty_(Execute ptr, addr pos, CallStruct call)
{
	addr check;

	GetControl(ptr->control, Control_Cons, &check);
	if (check != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.empty)(ptr);
}

static int call_callbind_rest_(Execute ptr, addr pos, CallStruct call)
{
	addr cons;
	getargs_list_control_heap(ptr, 0, &cons);
	return (call->call.rest)(ptr, cons);
}

static int call_callbind_dynamic_(Execute ptr, addr pos, CallStruct call)
{
	addr cons;
	GetControl(ptr->control, Control_Cons, &cons);
	return (call->call.dynamic)(ptr, cons);
}

static int call_callbind_var1_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.var1)(ptr, var1);
}

static int call_callbind_var2_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.var2)(ptr, var1, var2);

toofew:
	getname_callbind(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_var3_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var3, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	return (call->call.var3)(ptr, var1, var2, var3);

toofew:
	getname_callbind(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_var4_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var4, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	return (call->call.var4)(ptr, var1, var2, var3, var4);

toofew:
	getname_callbind(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_var5_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4, var5;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var4, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var5, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	return (call->call.var5)(ptr, var1, var2, var3, var4, var5);

toofew:
	getname_callbind(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_var6_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4, var5, var6;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var4, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var5, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var6, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	return (call->call.var6)(ptr, var1, var2, var3, var4, var5, var6);

toofew:
	getname_callbind(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_opt1_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		opt1 = Unbound;
	else
		Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.opt1)(ptr, opt1);
}

static int call_callbind_opt2_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, opt1, opt2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.opt2)(ptr, opt1, opt2);
}

static int call_callbind_opt3_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, opt1, opt2, opt3;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt3, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
finish:
	return (call->call.opt3)(ptr, opt1, opt2, opt3);
}

static int call_callbind_opt4_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, opt1, opt2, opt3, opt4;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = opt4 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = opt4 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = opt4 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt3, &cons);
	if (cons == Nil) {
		opt4 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt4, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
finish:
	return (call->call.opt4)(ptr, opt1, opt2, opt3, opt4);
}

static int call_callbind_opt5_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, opt1, opt2, opt3, opt4, opt5;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = opt4 = opt5 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = opt4 = opt5 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = opt4 = opt5 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt3, &cons);
	if (cons == Nil) {
		opt4 = opt5 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt4, &cons);
	if (cons == Nil) {
		opt5 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt5, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
finish:
	return (call->call.opt5)(ptr, opt1, opt2, opt3, opt4, opt5);
}

static int call_callbind_var1opt1_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var1opt1)(ptr, var1, opt1);
}

static int call_callbind_var2opt1_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var2opt1)(ptr, var1, var2, opt1);
}

static int call_callbind_var3opt1_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var3opt1)(ptr, var1, var2, var3, opt1);
}

static int call_callbind_var4opt1_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var4, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var4opt1)(ptr, var1, var2, var3, var4, opt1);
}

static int call_callbind_var5opt1_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4, var5, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var4, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var5, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var5opt1)(ptr, var1, var2, var3, var4, var5, opt1);
}

static int call_callbind_var1opt2_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, opt1, opt2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		opt1 = opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var1opt2)(ptr, var1, opt1, opt2);
}

static int call_callbind_var2opt2_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, opt1, opt2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		opt1 = opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var2opt2)(ptr, var1, var2, opt1, opt2);
}

static int call_callbind_var2opt3_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, opt1, opt2, opt3;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt3, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var2opt3)(ptr, var1, var2, opt1, opt2, opt3);
}

static int call_callbind_var1rest_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var1);
	getargs_list_control_heap(ptr, 1, &rest);
	return (call->call.var1rest)(ptr, var1, rest);
}

static int call_callbind_var2rest_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var2);
	getargs_list_control_heap(ptr, 2, &rest);
	return (call->call.var2rest)(ptr, var1, var2, rest);
}

static int call_callbind_var3rest_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var3);
	getargs_list_control_heap(ptr, 3, &rest);
	return (call->call.var3rest)(ptr, var1, var2, var3, rest);
}

static int call_callbind_var4rest_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var3);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var4);
	getargs_list_control_heap(ptr, 4, &rest);
	return (call->call.var4rest)(ptr, var1, var2, var3, var4, rest);
}

static int call_callbind_opt1rest_(Execute ptr, addr pos, CallStruct call)
{
	addr cons, opt1, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		rest = Nil;
	}
	else {
		Inline_getcar(cons, &opt1);
		getargs_list_control_heap(ptr, 1, &rest);
	}
	return (call->call.opt1rest)(ptr, opt1, rest);
}

static int call_callbind_var1dynamic_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var1);
	getargs_list_control_unsafe(ptr, 1, &rest);
	return (call->call.var1dynamic)(ptr, var1, rest);
}

static int call_callbind_var2dynamic_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var2);
	getargs_list_control_unsafe(ptr, 2, &rest);
	return (call->call.var2dynamic)(ptr, var1, var2, rest);
}

static int call_callbind_var3dynamic_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var3);
	getargs_list_control_unsafe(ptr, 3, &rest);
	return (call->call.var3dynamic)(ptr, var1, var2, var3, rest);
}

static int call_callbind_var4dynamic_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var4);
	getargs_list_control_unsafe(ptr, 4, &rest);
	return (call->call.var4dynamic)(ptr, var1, var2, var3, var4, rest);
}

static int call_callbind_opt1dynamic_(Execute ptr, addr pos, CallStruct call)
{
	addr cons, opt1, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		rest = Nil;
	}
	else {
		Inline_getcar(cons, &opt1);
		getargs_list_control_unsafe(ptr, 1, &rest);
	}
	return (call->call.opt1dynamic)(ptr, opt1, rest);
}


/*
 *  extend
 */
static int call_callbind_extend_macro_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.extend_macro)(var1, var2);

toofew:
	getname_callbind(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_extend_rest_(Execute ptr, addr pos, CallStruct call)
{
	addr cons;
	getargs_list_control_heap(ptr, 0, &cons);
	return (call->call.extend_rest)(cons);
}

static int call_callbind_extend_dynamic_(Execute ptr, addr pos, CallStruct call)
{
	addr cons;
	GetControl(ptr->control, Control_Cons, &cons);
	return (call->call.extend_dynamic)(cons);
}

static int call_callbind_extend_empty_(Execute ptr, addr pos, CallStruct call)
{
	addr check;

	GetControl(ptr->control, Control_Cons, &check);
	if (check != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.extend_empty)();
}

static int call_callbind_extend_any_(Execute ptr, addr pos, CallStruct call)
{
	return (call->call.extend_any)();
}

static int call_callbind_extend_var1_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.extend_var1)(var1);
}

static int call_callbind_extend_var2_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.extend_var2)(var1, var2);

toofew:
	getname_callbind(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_extend_var3_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var3, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	return (call->call.extend_var3)(var1, var2, var3);

toofew:
	getname_callbind(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_extend_var4_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var4, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	return (call->call.extend_var4)(var1, var2, var3, var4);

toofew:
	getname_callbind(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_extend_var5_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4, var5;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var4, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var5, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	return (call->call.extend_var5)(var1, var2, var3, var4, var5);

toofew:
	getname_callbind(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_extend_var6_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4, var5, var6;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var4, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var5, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var6, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	return (call->call.extend_var6)(var1, var2, var3, var4, var5, var6);

toofew:
	getname_callbind(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_extend_opt1_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		opt1 = Unbound;
	else
		Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.extend_opt1)(opt1);
}

static int call_callbind_extend_opt2_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, opt1, opt2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.extend_opt2)(opt1, opt2);
}

static int call_callbind_extend_opt3_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, opt1, opt2, opt3;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt3, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
finish:
	return (call->call.extend_opt3)(opt1, opt2, opt3);
}

static int call_callbind_extend_var1opt1_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.extend_var1opt1)(var1, opt1);
}

static int call_callbind_extend_var1opt2_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, opt1, opt2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		opt1 = opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.extend_var1opt2)(var1, opt1, opt2);
}

static int call_callbind_extend_var1opt3_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, opt1, opt2, opt3;

		GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt3, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.extend_var1opt3)(var1, opt1, opt2, opt3);
}

static int call_callbind_extend_var2opt1_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.extend_var2opt1)(var1, var2, opt1);
}

static int call_callbind_extend_var2opt2_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, opt1, opt2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		opt1 = opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.extend_var2opt2)(var1, var2, opt1, opt2);
}

static int call_callbind_extend_var2opt3_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, opt1, opt2, opt3;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt3, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.extend_var2opt3)(var1, var2, opt1, opt2, opt3);
}

static int call_callbind_extend_var3opt1_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.extend_var3opt1)(var1, var2, var3, opt1);
}

static int call_callbind_extend_var3opt2_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, opt1, opt2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil) {
		opt1 = opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.extend_var3opt2)(var1, var2, var3, opt1, opt2);
}

static int call_callbind_extend_var3opt3_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, opt1, opt2, opt3;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt3, &cons);
	if (cons != Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.extend_var3opt3)(var1, var2, var3, opt1, opt2, opt3);
}

static int call_callbind_extend_var1rest_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var1);
	getargs_list_control_heap(ptr, 1, &rest);
	return (call->call.extend_var1rest)(var1, rest);
}

static int call_callbind_extend_var2rest_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var2);
	getargs_list_control_heap(ptr, 2, &rest);
	return (call->call.extend_var2rest)(var1, var2, rest);
}

static int call_callbind_extend_var3rest_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var3);
	getargs_list_control_heap(ptr, 3, &rest);
	return (call->call.extend_var3rest)(var1, var2, var3, rest);
}

static int call_callbind_extend_var1dynamic_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var1);
	getargs_list_control_unsafe(ptr, 1, &rest);
	return (call->call.extend_var1dynamic)(var1, rest);
}

static int call_callbind_extend_var2dynamic_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var2);
	getargs_list_control_unsafe(ptr, 2, &rest);
	return (call->call.extend_var2dynamic)(var1, var2, rest);
}

static int call_callbind_extend_var3dynamic_(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		getname_callbind(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var3);
	getargs_list_control_unsafe(ptr, 3, &rest);
	return (call->call.extend_var3dynamic)(var1, var2, var3, rest);
}

int call_compiled_function_(Execute ptr, addr compiled)
{
	struct callbind_struct *str;
	callbind_control call;
	pointer p;

	p = StructFunction(compiled)->index;
	str = &(pointer_table[p]);
	Check(CallBind_size <= str->type, "index error");
	call = CallBindTable[str->type];
	Check(call == NULL, "call error. (build_control?)");
	return (*call)(ptr, compiled, str);
}

int call_callbind_function_(Execute ptr, addr name, struct callbind_struct *str)
{
	callbind_control call;

	Check(CallBind_size <= str->type, "index error");
	call = CallBindTable[str->type];
	Check(call == NULL, "call error. (build_control?)");
	return (*call)(ptr, name, str);
}

void init_callbind_control(void)
{
	CallBindTable[CallBind_code] = call_callbind_code_;
	CallBindTable[CallBind_macro] = call_callbind_macro_;
	CallBindTable[CallBind_none] = call_callbind_none_;
	CallBindTable[CallBind_any] = call_callbind_any_;
	CallBindTable[CallBind_empty] = call_callbind_empty_;
	CallBindTable[CallBind_rest] = call_callbind_rest_;
	CallBindTable[CallBind_dynamic] = call_callbind_dynamic_;
	CallBindTable[CallBind_var1] = call_callbind_var1_;
	CallBindTable[CallBind_var2] = call_callbind_var2_;
	CallBindTable[CallBind_var3] = call_callbind_var3_;
	CallBindTable[CallBind_var4] = call_callbind_var4_;
	CallBindTable[CallBind_var5] = call_callbind_var5_;
	CallBindTable[CallBind_var6] = call_callbind_var6_;
	CallBindTable[CallBind_opt1] = call_callbind_opt1_;
	CallBindTable[CallBind_opt2] = call_callbind_opt2_;
	CallBindTable[CallBind_opt3] = call_callbind_opt3_;
	CallBindTable[CallBind_opt4] = call_callbind_opt4_;
	CallBindTable[CallBind_opt5] = call_callbind_opt5_;
	CallBindTable[CallBind_var1opt1] = call_callbind_var1opt1_;
	CallBindTable[CallBind_var2opt1] = call_callbind_var2opt1_;
	CallBindTable[CallBind_var3opt1] = call_callbind_var3opt1_;
	CallBindTable[CallBind_var4opt1] = call_callbind_var4opt1_;
	CallBindTable[CallBind_var5opt1] = call_callbind_var5opt1_;
	CallBindTable[CallBind_var1opt2] = call_callbind_var1opt2_;
	CallBindTable[CallBind_var2opt2] = call_callbind_var2opt2_;
	CallBindTable[CallBind_var2opt3] = call_callbind_var2opt3_;
	CallBindTable[CallBind_var1rest] = call_callbind_var1rest_;
	CallBindTable[CallBind_var2rest] = call_callbind_var2rest_;
	CallBindTable[CallBind_var3rest] = call_callbind_var3rest_;
	CallBindTable[CallBind_var4rest] = call_callbind_var4rest_;
	CallBindTable[CallBind_opt1rest] = call_callbind_opt1rest_;
	CallBindTable[CallBind_var1dynamic] = call_callbind_var1dynamic_;
	CallBindTable[CallBind_var2dynamic] = call_callbind_var2dynamic_;
	CallBindTable[CallBind_var3dynamic] = call_callbind_var3dynamic_;
	CallBindTable[CallBind_var4dynamic] = call_callbind_var4dynamic_;
	CallBindTable[CallBind_opt1dynamic] = call_callbind_opt1dynamic_;

	CallBindTable[CallBind_extend_macro] = call_callbind_extend_macro_;
	CallBindTable[CallBind_extend_rest] = call_callbind_extend_rest_;
	CallBindTable[CallBind_extend_dynamic] = call_callbind_extend_dynamic_;
	CallBindTable[CallBind_extend_any] = call_callbind_extend_any_;
	CallBindTable[CallBind_extend_empty] = call_callbind_extend_empty_;
	CallBindTable[CallBind_extend_var1] = call_callbind_extend_var1_;
	CallBindTable[CallBind_extend_var2] = call_callbind_extend_var2_;
	CallBindTable[CallBind_extend_var3] = call_callbind_extend_var3_;
	CallBindTable[CallBind_extend_var4] = call_callbind_extend_var4_;
	CallBindTable[CallBind_extend_var5] = call_callbind_extend_var5_;
	CallBindTable[CallBind_extend_var6] = call_callbind_extend_var6_;
	CallBindTable[CallBind_extend_opt1] = call_callbind_extend_opt1_;
	CallBindTable[CallBind_extend_opt2] = call_callbind_extend_opt2_;
	CallBindTable[CallBind_extend_opt3] = call_callbind_extend_opt3_;
	CallBindTable[CallBind_extend_var1opt1] = call_callbind_extend_var1opt1_;
	CallBindTable[CallBind_extend_var1opt2] = call_callbind_extend_var1opt2_;
	CallBindTable[CallBind_extend_var1opt3] = call_callbind_extend_var1opt3_;
	CallBindTable[CallBind_extend_var2opt1] = call_callbind_extend_var2opt1_;
	CallBindTable[CallBind_extend_var2opt2] = call_callbind_extend_var2opt2_;
	CallBindTable[CallBind_extend_var2opt3] = call_callbind_extend_var2opt3_;
	CallBindTable[CallBind_extend_var3opt1] = call_callbind_extend_var3opt1_;
	CallBindTable[CallBind_extend_var3opt2] = call_callbind_extend_var3opt2_;
	CallBindTable[CallBind_extend_var3opt3] = call_callbind_extend_var3opt3_;
	CallBindTable[CallBind_extend_var1rest] = call_callbind_extend_var1rest_;
	CallBindTable[CallBind_extend_var2rest] = call_callbind_extend_var2rest_;
	CallBindTable[CallBind_extend_var3rest] = call_callbind_extend_var3rest_;
	CallBindTable[CallBind_extend_var1dynamic] = call_callbind_extend_var1dynamic_;
	CallBindTable[CallBind_extend_var2dynamic] = call_callbind_extend_var2dynamic_;
	CallBindTable[CallBind_extend_var3dynamic] = call_callbind_extend_var3dynamic_;
}


/************************************************************
 *  control_execute.c
 ************************************************************/
#undef LISP_DEBUG_TRACE


/*
 *  runcode
 */
#ifdef LISP_DEBUG_TRACE
#include <sys/time.h>
struct timeval prevtime = {0, 0};
void output_timestamp(void)
{
	struct timeval now, diff;
	gettimeofday(&now, NULL);
	timersub(&now, &prevtime, &diff);
	printf("[TRACE] %6ld: ", diff.tv_usec);
	prevtime = now;
}

static void output_trace(addr control, size_t point)
{
	addr code;
	struct control_struct *str;

	output_timestamp();
	str = StructControl(control);
	code = str->trace;
	getarray_code(code, &code);
	GetArrayA4(code, point, &code);
	infoprint(code);
}
#define OutputTrace(a,b) output_trace(a,b)
#else
#define OutputTrace(a,b)
#endif


/*
 *  runcode-control
 */
#ifdef LISP_DEBUG_FORCE_GC
#define LispForceGc(x)			gcsync((x), GcMode_Full)
#else
#define LispForceGc(x)			;
#endif

#ifdef LISP_DEBUG_TRACE
#define LispGcTrace()			printf("[TRACE] GC-SYNC\n")
#else
#define LispGcTrace()			;
#endif

#ifdef LISP_DEBUG_TRACE
#define LispBeginControl(x,y)	printf("[TRACE] BEGIN-%s: %p\n", (x), (void *)(y))
#define LispEndControl(x,y)		printf("[TRACE] END-%s: %p\n", (x), (void *)(y))
#else
#define LispBeginControl(x,y)	;
#define LispEndControl(x,y)		;
#endif

#ifdef LISP_GC_SYNC
#define LispGcCheck(x)			{ \
	if ((lisp_gcsync != GcMode_Off) || ((LISP_GC_SYNC % ControlCounter) == 0)) { \
		LispGcTrace(); \
		gcsync((x), GcMode_Default); \
	} \
}
#else
#define LispGcCheck(x)			{ \
	if (lisp_gcsync != GcMode_Off) { \
		LispGcTrace(); \
		gcsync((x), GcMode_Default); \
	} \
}
#endif

#ifdef LISP_DEBUG_TRACE
#define TraceControl(str, x) ((str)->trace = x)
#else
#define TraceControl(str, x)
#endif

int runcode_control_(Execute ptr, addr code)
{
	addr control;
	struct control_struct *str;
	struct code_struct *body;
	struct code_value *sys, *bind;
	size_t point, *index;

	/* code */
	CheckType(code, LISPTYPE_CODE);
	control = ptr->control;
	body = StructCode(code);
	sys = body->sys;

	/* control */
	str = StructControl(control);
	str->run_code = code;
	str->run_point = 0;
	index = &(str->run_point);

	LispBeginControl("EXECUTE", control);
	for (;;) {
		/* counter */
		ControlCounter++;
		LispForceGc(ptr);
		TraceControl(str, code);

		/* execute */
		point = (*index)++;
		bind = sys + point;
		if (bind->call == NULL)
			break;
		OutputTrace(control, point);
		(void)(bind->call)(ptr, bind->value);
	}
	LispGcCheck(ptr);
	LispEndControl("EXECUTE", control);

	return ptr->throw_value != throw_normal;
}

#ifdef LISP_DEBUG
static int runcode_control_check(Execute ptr)
{
	addr control, check;

	control = ptr->control;
	check = ptr->throw_control;
	while (control != Nil) {
		if (control == check)
			return 0;
		GetControl(control, Control_Next, &control);
	}

	return 1;
}
#endif

int revert_control_(Execute ptr)
{
	addr control, throw_control;

	/* throw */
	control = ptr->control;
	throw_control = ptr->throw_control;
	if (throw_control && (control != throw_control)) {
		Check(runcode_control_check(ptr), "throw_control error");
		return 1; /* escape */
	}
	ptr->throw_value = throw_normal;
	ptr->throw_handler = NULL;
	ptr->throw_control = NULL;

	/* block */
	if (ptr->throw_point_p == 0)
		return 0; /* normal */
	ptr->throw_point_p = 0;

	/* tagbody */
	(void)goto_control_(ptr, ptr->throw_point);
	ptr->throw_point = 0;

	return 0; /* normal */
}

int revert_goto_control_(Execute ptr, size_t index)
{
	return revert_control_(ptr)
		|| goto_control_(ptr, index);
}


/*
 *  (call ...) execute
 */
int execute_control_(Execute ptr, addr call)
{
	addr value;

	Check(ptr->control == Nil, "root error");
	Check(call == Unbound, "Function is Unbound.");

	/* generic function */
	if (GetType(call) != LISPTYPE_FUNCTION) {
		GetControl(ptr->control, Control_Cons, &value);
		return closrun_execute_(ptr, call, value);
	}

	/* closure */
	GetDataFunction(call, &value);
	if (value != Unbound)
		setdata_control(ptr, value);

	/* compiled function */
	if (StructFunction(call)->compiled)
		return call_compiled_function_(ptr, call);

	/* interpreted function */
	GetCodeFunction(call, &value);
	return runcode_control_(ptr, value);
}


/*
 *  checkargs
 */
static int checkargs_var_(Execute ptr, addr array, addr *args)
{
	addr value, type;

	GetArrayA2(array, 0, &array); /* var */
	while (array != Nil) {
		if (*args == Nil)
			return fmte_("Too few argument.", NULL);
		Return_getcons(*args, &value, args);
		GetCons(array, &type, &array);
		Return(call_typep_asterisk_error_(ptr, value, type));
	}

	return 0;
}

static int checkargs_opt_(Execute ptr, addr array, addr *args)
{
	addr value, type;

	GetArrayA2(array, 1, &array); /* opt */
	while (*args != Nil && array != Nil) {
		Return_getcons(*args, &value, args);
		GetCons(array, &type, &array);
		Return(call_typep_asterisk_error_(ptr, value, type));
	}

	return 0;
}

static void contargs_keyvalue(LocalRoot local, int keyvalue, addr cons, addr *ret)
{
	if (keyvalue == 0) {
		/* name */
		GetCar(cons, &cons);
		type_eql_local(local, cons, ret);
	}
	else {
		/* type */
		GetCdr(cons, ret);
	}
}

static void contargs_key(Execute ptr, int keyvalue, addr cons, addr *ret)
{
	LocalRoot local;
	addr pos, array;
	size_t size, i;

	/* &allow-other-keys */
	if (cons == T) {
		if (keyvalue)
			GetTypeTable(ret, T);
		else
			GetTypeTable(ret, Symbol);
		return;
	}

	/* &key */
	local = ptr->local;
	size = length_list_unsafe(cons);
	if (size == 1) {
		GetCar(cons, &pos);
		contargs_keyvalue(local, keyvalue, pos, ret);
		return;
	}

	/* or */
	vector4_local(local, &array, size);
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &pos, &cons);
		contargs_keyvalue(local, keyvalue, pos, &pos);
		SetArrayA4(array, i, pos);
	}
	type1_local(local, LISPDECL_OR, array, ret);
}

static int checkargs_restkey_(Execute ptr, addr array, addr args)
{
	int keyvalue;
	addr rest, key, value, type;

	GetArrayA2(array, 2, &rest);
	GetArrayA2(array, 3, &key);
	if (find_keyword_allow_other_keys(args))
		key = T;
	if (rest == Nil && key == Nil) {
		if (args != Nil)
			return fmte_("Too many argument.", NULL);
	}
	for (keyvalue = 0; args != Nil; keyvalue = (! keyvalue)) {
		Return_getcons(args, &value, &args);
		/* &rest */
		if (rest != Nil) {
			Return(call_typep_asterisk_error_(ptr, value, rest));
		}
		/* &key */
		if (key != Nil) {
			contargs_key(ptr, keyvalue, key, &type);
			Return(call_typep_asterisk_error_(ptr, value, type));
		}
	}

	/* error check */
	if (key != Nil && keyvalue)
		return fmte_("Invalid keyword argument.", NULL);

	return 0;
}

static int checkargs_execute_(Execute ptr, addr array, addr args)
{
	LocalRoot local;
	LocalStack stack;

	/* var */
	Return(checkargs_var_(ptr, array, &args));
	if (args == Nil)
		return 0;
	/* opt */
	Return(checkargs_opt_(ptr, array, &args));
	if (args == Nil)
		return 0;
	/* rest, key */
	local = ptr->local;
	push_local(local, &stack);
	Return(checkargs_restkey_(ptr, array, args));
	rollback_local(local, stack);

	return 0;
}

static int checkargs_control_(Execute ptr, addr call, addr args)
{
	addr type;

	/* asterisk check */
	gettype_function(call, &type);
	if (type == Nil || type_asterisk_p(type))
		return 0;
	Check(! type_function_p(type), "type error");
	GetArrayType(type, 0, &type); /* args */
	if (type_asterisk_p(type))
		return 0;

	/* type check */
	return checkargs_execute_(ptr, type, args);
}


/*
 *  apply
 */
static int apply_no_control_(Execute ptr, addr call, addr list)
{
	addr value;

	Check(ptr->control == Nil, "root error");
	Check(call == Unbound, "Function is Unbound.");

	/* generic function */
	if (GetType(call) != LISPTYPE_FUNCTION)
		return closrun_execute_(ptr, call, list);

	/* args */
	SetControl(ptr->control, Control_Cons, list);
	SetControl(ptr->control, Control_ConsTail, Nil);

	/* closure */
	GetDataFunction(call, &value);
	if (value != Unbound)
		setdata_control(ptr, value);

	/* compiled function */
	if (StructFunction(call)->compiled) {
		Return(checkargs_control_(ptr, call, list));
		return call_compiled_function_(ptr, call);
	}

	/* interpreted function */
	GetCodeFunction(call, &value);
	return runcode_control_(ptr, value);
}

int apply_control_(Execute ptr, addr call, addr list)
{
	addr control;

	push_control(ptr, &control);
	(void)apply_no_control_(ptr, call, list);
	return pop_control_(ptr, control);
}

int apply_named_control_(Execute ptr, addr call, addr list)
{
	addr control;

	push_control(ptr, &control);
	SetControl(control, Control_Call, call);
	(void)apply_no_control_(ptr, call, list);
	return pop_control_(ptr, control);
}

int applya_control_(Execute ptr, addr call, ...)
{
	addr control, list;
	va_list va;

	push_control(ptr, &control);
	va_start(va, call);
	lista_stdarg_alloc(ptr->local, &list, va);
	va_end(va);
	(void)apply_control_(ptr, call, list);
	return pop_control_(ptr, control);
}

int funcall_control_(Execute ptr, addr call, ...)
{
	addr control, list;
	va_list va;

	push_control(ptr, &control);
	va_start(va, call);
	list_stdarg_alloc(ptr->local, &list, va);
	va_end(va);
	(void)apply_no_control_(ptr, call, list);
	return pop_control_(ptr, control);
}


/*
 *  C language
 */
static int apply1_function_control_(Execute ptr, addr *ret, addr call)
{
	Check(call == Unbound, "type error");
	switch (GetType(call)) {
		case LISPTYPE_SYMBOL:
			return getfunction_global_(call, ret);

		case LISPTYPE_CALLNAME:
			return getglobalcheck_callname_(call, ret);

		case LISPTYPE_CLOS:
		case LISPTYPE_FUNCTION:
			return Result(ret, call);

		default:
			*ret = Nil;
			return fmte_("The object ~S cannot execute.", call, NULL);
	}
}

static int apply1_no_control_(Execute ptr, addr call, addr list)
{
	Return(apply1_function_control_(ptr, &call, call));
	return apply_no_control_(ptr, call, list);
}

int apply1_control_(Execute ptr, addr *ret, addr call, addr list)
{
	addr control;

	push_control(ptr, &control);
	(void)apply1_no_control_(ptr, call, list);
	Return(pop_control_(ptr, control));
	getresult_control(ptr, ret);

	return 0;
}

int applya1_control_(Execute ptr, addr *ret, addr call, ...)
{
	addr control, list;
	va_list va;

	push_control(ptr, &control);
	va_start(va, call);
	lista_stdarg_alloc(ptr->local, &list, va);
	va_end(va);

	(void)apply1_no_control_(ptr, call, list);
	Return(pop_control_(ptr, control));
	getresult_control(ptr, ret);

	return 0;
}

int funcall1_control_(Execute ptr, addr *ret, addr call, ...)
{
	addr control, list;
	va_list va;

	push_control(ptr, &control);
	va_start(va, call);
	list_stdarg_alloc(ptr->local, &list, va);
	va_end(va);

	(void)apply1_no_control_(ptr, call, list);
	Return(pop_control_(ptr, control));
	getresult_control(ptr, ret);

	return 0;
}


/************************************************************
 *  control_object.c
 ************************************************************/

/*
 *  control
 */
void *ptrbodycontrol_debug(addr pos)
{
	CheckType(pos, LISPTYPE_CONTROL);
	return PtrBodyControl_Low(pos);
}

struct control_struct *structcontrol_debug(addr pos)
{
	CheckType(pos, LISPTYPE_CONTROL);
	return StructControl_Low(pos);
}

void getcontrol_debug(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPTYPE_CONTROL);
	GetControl_Low(pos, index, ret);
}

void setcontrol_debug(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPTYPE_CONTROL);
	SetControl_Low(pos, index, value);
}


/*
 *  special
 */
static void special_local(Execute ptr, addr *ret, addr symbol)
{
	addr pos, value;

	Check(! symbolp(symbol), "type error");
	getspecial_unsafe(ptr, symbol, &value);
	local_array2(ptr->local, &pos, LISPSYSTEM_SPECIAL, Special_Size);
	SetArrayA2(pos, Special_Symbol, symbol);
	SetArrayA2(pos, Special_Value, value);
	*ret = pos;
}

static void getsymbol_special(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SPECIAL);
	GetArrayA2(pos, Special_Symbol, ret);
}

static void getvalue_special(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SPECIAL);
	GetArrayA2(pos, Special_Value, ret);
}


/*
 *  taginfo
 */
void taginfo_heap(addr *ret, addr control, addr tag, size_t point)
{
	addr pos;
	struct taginfo_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_TAGINFO,
			TagInfo_Size, sizeoft(struct taginfo_struct));
	str = StructTagInfo(pos);
	Check(GetStatusDynamic(tag), "dynamic error");
	SetNameTagInfo(pos, tag);
	str->open = 1;
	str->control = control;
	str->point = point;
	*ret = pos;
}

static void close_taginfo(addr pos)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	StructTagInfo(pos)->open = 0;
}

void *ptrtaginfo_debug(addr pos)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	return PtrTagInfo_Low(pos);
}

struct taginfo_struct *structtaginfo_debug(addr pos)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	return StructTagInfo_Low(pos);
}

void getnametaginfo_debug(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	GetNameTagInfo_Low(pos, ret);
}

void setnametaginfo_debug(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	SetNameTagInfo_Low(pos, value);
}


/*
 *  handler
 */
void handler_local(LocalRoot local, addr *ret, addr name, addr call, int esc)
{
	addr pos;

	local_array2(local, &pos, LISPSYSTEM_HANDLER, Handler_Size);
	SetNameHandler_Low(pos, name);
	SetCallHandler_Low(pos, call);
	setescape_handler(pos, esc);
	setdisable_handler(pos, 0);
	*ret = pos;
}

void getnamehandler_debug(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	GetNameHandler_Low(pos, ret);
}

void setnamehandler_debug(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	SetNameHandler_Low(pos, value);
}

void getcallhandler_debug(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	GetCallHandler_Low(pos, ret);
}

void setcallhandler_debug(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	SetCallHandler_Low(pos, value);
}

int getescape_handler(addr pos)
{
	byte c;

	CheckType(pos, LISPSYSTEM_HANDLER);
	c = (byte)GetUser(pos);
	return (int)GetBitByte(c, 0);
}

void setescape_handler(addr pos, int value)
{
	byte c;

	CheckType(pos, LISPSYSTEM_HANDLER);
	c = (byte)GetUser(pos);
	SetBitByte(c, 0, value);
	SetUser(pos, c);
}

int getdisable_handler(addr pos)
{
	byte c;

	CheckType(pos, LISPSYSTEM_HANDLER);
	c = (byte)GetUser(pos);
	return (int)GetBitByte(c, 1);
}

void setdisable_handler(addr pos, int value)
{
	byte c;

	CheckType(pos, LISPSYSTEM_HANDLER);
	c = (byte)GetUser(pos);
	SetBitByte(c, 1, value);
	SetUser(pos, c);
}

int checkhandler_control_(Execute ptr, addr pos, addr instance, int *ret)
{
	addr clos;

	CheckType(pos, LISPSYSTEM_HANDLER);

	/* ignore */
	if (getdisable_handler(pos))
		return Result(ret, 0);

	/* check */
	GetNameHandler(pos, &clos);
	if (clos == Nil)
		return Result(ret, 0);

	return typep_clang_(ptr, instance, clos, ret);
}


/*
 *  push control
 */
void push_control(Execute ptr, addr *ret)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct control_struct *str;

	/* local */
	local = ptr->local;
	push_local(local, &stack);

	/* object */
	local_smallsize(local, &pos,
			LISPTYPE_CONTROL,
			Control_Size,
			sizeoft(struct control_struct));
	str = StructControl(pos);
	clearpoint(str);
	str->lexical_reader = ptr->lexical_reader;
	str->lexical_vector = ptr->lexical_vector;
	str->stack = stack;

	/* push */
	SetControl(pos, Control_Next, ptr->control);
	*ret = ptr->control = pos;
}

void push_args_control(Execute ptr, addr *ret)
{
	addr prev, next, pos;

	prev = ptr->control;
	push_control(ptr, &next);
	GetControl(prev, Control_Cons, &pos);
	SetControl(next, Control_Cons, pos);
	*ret = next;
}


/*
 *  pop_control
 */
static void close_special_control(Execute ptr, addr pos)
{
	addr symbol, value;

	getsymbol_special(pos, &symbol);
	getvalue_special(pos, &value);
	setspecial_unsafe(ptr, symbol, value);
}

static void close_close_control(Execute ptr, addr control)
{
	addr list, pos;

	GetControl(control, Control_Close, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		switch (GetType(pos)) {
			case LISPSYSTEM_SPECIAL:
				close_special_control(ptr, pos);
				break;

			case LISPSYSTEM_TAGINFO:
				close_taginfo(pos);
				break;

			case LISPTYPE_RESTART:
				setenable_restart(pos, 0);
				break;

			default:
				Abort("Invalid control-close type.");
				break;
		}
	}
}

int pop_control_(Execute ptr, addr control)
{
	addr *lexical_reader, lexical_vector;
	LocalStack stack;
	struct control_struct *str;

	Check(ptr->control != control, "control error");
	str = StructControl(control);
	stack = str->stack;
	lexical_reader = str->lexical_reader;
	lexical_vector = str->lexical_vector;

	/* close */
	close_close_control(ptr, control);

	/* pop */
	GetControl(control, Control_Next, &control);
	ptr->control = control;
	ptr->lexical_reader = lexical_reader;
	ptr->lexical_vector = lexical_vector;
	rollback_local(ptr->local, stack);

	return ptr->throw_value != throw_normal;
}

int free_control_degrade_(Execute ptr, addr control)
{
	addr root;

	/* check */
#ifdef LISP_DEBUG
	for (root = ptr->control; root == control; ) {
		Check(root == Nil, "free_control check error");
		GetControl(root, Control_Next, &root);
	}
#endif

	/* rollback */
	do {
		root = ptr->control;
		Check(root == Nil, "free_control error");
		Return(pop_control_(ptr, root));
	} while (root != control);

	return 0;
}


/*
 *  data
 */
int stack_check_control(Execute ptr)
{
	LocalStack stack1, stack2;
	stack1 = StructControl(ptr->control)->stack;
	stack2 = ptr->local->stack;
	return stack1 != stack2;
}

static void pushclose_control(Execute ptr, addr pos)
{
	addr list;

	GetControl(ptr->control, Control_Close, &list);
	cons_local(ptr->local, &list, pos, list);
	SetControl(ptr->control, Control_Close, list);
}

void pushspecial_control(Execute ptr, addr pos, addr value)
{
	addr x;

	Check(! symbolp(pos), "type error");
	Check(stack_check_control(ptr), "stack error");
	special_local(ptr, &x, pos);
	pushclose_control(ptr, x);
	setspecial_unsafe(ptr, pos, value);
}

void pushtaginfo_control(Execute ptr, addr pos)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	pushclose_control(ptr, pos);
}

static void pushtable_control(Execute ptr, constindex index, addr pos)
{
	addr control, key, table, cons;
	LocalRoot local;

	local = ptr->local;
	control = ptr->control;
	GetConstant(index, &key);
	GetControl(control, Control_Table, &table);
	getplist(table, key, &cons);
	cons_local(local, &cons, pos, cons);
	if (setplist_local(local, table, key, cons, &table))
		SetControl(control, Control_Table, table);
}

void pushhandler_control(Execute ptr, addr pos)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	pushtable_control(ptr, CONSTANT_SYSTEM_HANDLER, pos);
}

void pushrestart_control(Execute ptr, addr pos)
{
	CheckType(pos, LISPTYPE_RESTART);
	pushclose_control(ptr, pos);
	pushtable_control(ptr, CONSTANT_SYSTEM_RESTART, pos);
}

int existspecial_control(Execute ptr, addr pos)
{
	addr list, check;

	GetControl(ptr->control, Control_Close, &list);
	while (list != Nil) {
		GetCons(list, &check, &list);
		if (GetType(check) != LISPSYSTEM_SPECIAL)
			continue;
		getsymbol_special(check, &check);
		if (check == pos)
			return 1;
	}

	return 0;
}


/*
 *  access
 */
void getdata_control(Execute ptr, addr *ret)
{
	GetControl(ptr->control, Control_Data, ret);
}

void getdata_code_control(Execute ptr, addr *ret)
{
	addr control;
	struct control_struct *str;

	control = ptr->control;
	for (;;) {
		str = StructControl(control);
		if (str->run_code)
			break;
		GetControl(control, Control_Next, &control);
		Check(control == Unbound, "revert-goto error.");
	}
	GetControl(control, Control_Data, ret);
}

void setdata_control(Execute ptr, addr value)
{
	SetControl(ptr->control, Control_Data, value);
}

int getcall_control(Execute ptr, addr *ret)
{
	addr control, pos;

	control = ptr->control;
	while (control != Nil) {
		GetControl(control, Control_Call, &pos);
		if (pos != Nil) {
			*ret = pos;
			return 1;
		}
		GetControl(control, Control_Next, &control);
	}

	*ret = Nil;
	return 0;
}

static int gettable_control(addr pos, constindex index, addr *ret)
{
	addr key;

	GetConstant(index, &key);
	GetControl(pos, Control_Table, &pos);

	return getplist(pos, key, ret) == 0;
}

static void settable_control(LocalRoot local, addr control, constindex index, addr value)
{
	addr key, table;

	GetConstant(index, &key);
	GetControl(control, Control_Table, &table);
	if (setplist_local(local, table, key, value, &table))
		SetControl(control, Control_Table, table);
}

int getcatch_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_COMMON_CATCH, ret);
}

int getcondition_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_COMMON_CONDITION, ret);
}

int gethandler_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_SYSTEM_HANDLER, ret);
}

int getrestart_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_SYSTEM_RESTART, ret);
}

void setcatch_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_COMMON_CATCH, value);
}

void sethandler_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_SYSTEM_HANDLER, value);
}

void setrestart_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_SYSTEM_RESTART, value);
}

void pushdebug_control(Execute ptr, addr pos)
{
	Check(! (pos == Nil || fixnump(pos)), "type error");
	settable_control(ptr->local, ptr->control, CONSTANT_CODE_BEGIN, pos);
}

int getdebug_control(Execute ptr, addr *ret)
{
	return gettable_control(ptr->control, CONSTANT_CODE_BEGIN, ret);
}

void save_control(Execute ptr)
{
	addr pos;
	save_execute_control(ptr, &pos);
	settable_control(ptr->local, ptr->control, CONSTANT_CODE_SAVE, pos);
}

int restore_control_(Execute ptr)
{
	addr pos;

	if (! gettable_control(ptr->control, CONSTANT_CODE_SAVE, &pos))
		return fmte_("Invalid code, RESTORE.", NULL);
	CheckType(pos, LISPSYSTEM_EXECUTE);
	restore_execute_control(ptr, pos);

	return 0;
}


/************************************************************
 *  control_operator.c
 ************************************************************/

/*
 *  arguments
 */
void setargs_nil_control(Execute ptr)
{
	addr root;

	root = ptr->control;
	Check(root == Nil, "control error");
	SetControl(root, Control_Cons, Nil);
	SetControl(root, Control_ConsTail, Nil);
}

void setargs_va_control(Execute ptr, ...)
{
	addr pos;
	va_list args;

	setargs_nil_control(ptr);
	va_start(args, ptr);
	for (;;) {
		pos = va_arg(args, addr);
		if (pos == NULL)
			break;
		pushargs_control(ptr, pos);
	}
	va_end(args);
}

static void pushargs_list_control(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		pushargs_control(ptr, pos);
	}
}

void setargs_list_control(Execute ptr, addr list)
{
	setargs_nil_control(ptr);
	pushargs_list_control(ptr, list);
}

void pushargs_control(Execute ptr, addr value)
{
	addr root, cons, next;

	root = ptr->control;
	Check(root == Nil, "control error");
	GetControl(root, Control_ConsTail, &cons);
	if (cons == Nil) {
		conscar_local(ptr->local, &cons, value);
		SetControl(root, Control_Cons, cons);
		SetControl(root, Control_ConsTail, cons);
	}
	else {
		conscar_local(ptr->local, &next, value);
		SetCdr(cons, next);
		SetControl(root, Control_ConsTail, next);
	}
}

int popargs_control_(Execute ptr, addr *ret)
{
	addr control, list;

	control = ptr->control;
	GetControl(control, Control_Cons, &list);
	if (list == Nil) {
		*ret = Unbound;
	}
	else {
		Return_getcons(list, ret, &list);
		SetControl(control, Control_Cons, list);
	}

	return 0;
}

void getargs_control(Execute ptr, size_t index, addr *ret)
{
	addr root;

	root = ptr->control;
	GetControl(root, Control_Cons, &root);
	getnth_unbound_unsafe(root, index, ret);
}

void getargs_tail_control(Execute ptr, addr *ret)
{
	addr cons;

	GetControl(ptr->control, Control_ConsTail, &cons);
	GetCar(cons, ret);
}

void getargs_list_control_unsafe(Execute ptr, size_t index, addr *ret)
{
	addr root;

	root = ptr->control;
	GetControl(root, Control_Cons, &root);
	getnthcdr_unsafe(root, index, ret);
}

void getargs_list_control_heap(Execute ptr, size_t index, addr *ret)
{
	addr cons;

	getargs_list_control_unsafe(ptr, index, &cons);
	copy_list_heap_unsafe(ret, cons);
}

void pushargs_allvalues(Execute ptr)
{
	int check;
	addr *values, pos, list;
	size_t size, sizer, i;

	sizer = ptr->sizer;
	check = EXECUTE_VALUES < sizer;
	size = check? EXECUTE_VALUES: sizer;

	values = ptr->values_reader;
	for (i = 0; i < size; i++) {
		pos = values[i];
		pushargs_control(ptr, pos);
	}
	if (! check)
		return;
	GetExecuteValuesList(ptr->values_vector, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		pushargs_control(ptr, pos);
	}
}


/*
 *  flow control
 */
int goto_control_(Execute ptr, size_t point)
{
	addr control;
	struct control_struct *str;

	control = ptr->control;
	for (;;) {
		str = StructControl(control);
		if (str->run_code)
			break;
		GetControl(control, Control_Next, &control);
		Check(control == Unbound, "revert-goto error.");
	}
	str->run_point = point;

	return 0;
}

int go_control_(Execute ptr, addr pos)
{
	struct taginfo_struct *str;

	/* find tag */
	CheckType(pos, LISPSYSTEM_TAGINFO);
	str = StructTagInfo(pos);
	if (str->open == 0) {
		GetNameTagInfo(pos, &pos);
		return fmte_("Tag ~S already closed.", pos, NULL);
	}

	/* rollback */
	ptr->throw_value = throw_tagbody;
	ptr->throw_handler = pos;
	ptr->throw_control = str->control;
	ptr->throw_point = str->point;
	ptr->throw_point_p = 1;
	return 1;
}

int return_from_control_(Execute ptr, addr pos)
{
	struct taginfo_struct *str;

	/* find name */
	CheckType(pos, LISPSYSTEM_TAGINFO);
	str = StructTagInfo(pos);
	if (str->open == 0) {
		GetNameTagInfo(pos, &pos);
		return fmte_("Block ~S already closed.", pos, NULL);
	}

	/* rollback */
	ptr->throw_value = throw_block;
	ptr->throw_handler = pos;
	ptr->throw_control = str->control;
	ptr->throw_point_p = 0;
	return 1;
}

void catch_control(Execute ptr, addr name)
{
	setcatch_control(ptr->local, ptr->control, name);
}

static int throw_find_control(Execute ptr, addr *ret, addr name)
{
	addr control, check;

	control = ptr->control;
	while (control != Nil) {
		if (getcatch_control(control, &check)) {
			if (check == name) {
				*ret = control;
				return 1;
			}
		}
		GetControl(control, Control_Next, &control);
	}
	*ret = 0;
	return 0;
}

int throw_control_(Execute ptr, addr name)
{
	addr next;

	/* find name */
	if (! throw_find_control(ptr, &next, name)) {
		return call_simple_control_error_va_(ptr,
				"Cannot find catch name ~S.", name, NULL);
	}
	/* rollback */
	ptr->throw_value = throw_catch;
	ptr->throw_handler = name;
	ptr->throw_control = next;
	ptr->throw_point_p = 0;
	return 1;
}


/*
 *  handler
 */
int pushhandler_common_(Execute ptr, addr name, addr call, int escape)
{
	addr pos;

	Return(parse_type_(ptr, &name, name, Nil));
	CheckType(name, LISPTYPE_TYPE);
	handler_local(ptr->local, &pos, name, call, escape);
	pushhandler_control(ptr, pos);

	return 0;
}

void reverse_handler_control(Execute ptr)
{
	addr control, list;

	control = ptr->control;
	gethandler_control(control, &list);
	nreverse(&list, list);
	sethandler_control(ptr->local, control, list);
}

void pushbind_restart_control(Execute ptr, addr list, int escape)
{
	addr name, form, inter, report, test, pos;

	/* A2 restart */
	List_bind(list, &name, &form, &inter, &report, &test, NULL);
	restart_heap(&pos, name);
	setfunction_restart(pos, form);
	setinteractive_restart(pos, inter);
	setreport_restart(pos, report);
	settest_restart(pos, test);
	setescape_restart(pos, escape);

	/* push handler */
	pushrestart_control(ptr, pos);
}

void reverse_restart_control(Execute ptr)
{
	addr control, list;

	control = ptr->control;
	getrestart_control(control, &list);
	nreverse(&list, list);
	setrestart_control(ptr->local, control, list);
}


/*
 *  find-condition
 */
int find_condition_control_(Execute ptr, addr instance, int *ret)
{
	int check;
	addr control, list, pos;

	for (control = ptr->control; control != Nil; ) {
		gethandler_control(control, &list);
		while (list != Nil) {
			GetCons(list, &pos, &list);
			Return(checkhandler_control_(ptr, pos, instance, &check));
			if (check) {
				return Result(ret, 1);
			}
		}
		GetControl(control, Control_Next, &control);
	}

	return Result(ret, 0);
}


/*
 *  invoke-handler
 */
static int wake_handler_call_(Execute ptr, addr next, addr instance, addr pos)
{
	addr call;

	/* call */
	GetCallHandler(pos, &call);
	Return(funcall_control_(ptr, call, instance, NULL));

	/* escape */
	if (getescape_handler(pos)) {
		ptr->throw_value = throw_handler_case;
		ptr->throw_handler = pos;
		ptr->throw_control = next;
		ptr->throw_point_p = 0;
		return 1;
	}

	return 0;
}

static int wake_handler_(Execute ptr, addr next, addr instance, addr pos)
{
	int check;

	setdisable_handler(pos, 1);
	check = wake_handler_call_(ptr, next, instance, pos);
	setdisable_handler(pos, 0);

	return check;
}

int invoke_handler_control_(Execute ptr, addr instance)
{
	int check;
	addr next, list, pos;

	next = ptr->control;
	while (next != Nil) {
		gethandler_control(next, &list);
		while (list != Nil) {
			GetCons(list, &pos, &list);
			Return(checkhandler_control_(ptr, pos, instance, &check));
			if (check) {
				Return(wake_handler_(ptr, next, instance, pos));
			}
		}
		GetControl(next, Control_Next, &next);
	}

	return 0;
}


/*
 *  invoke-restart
 */
static int check_enable_restart_control_(addr restart)
{
	if (! getenable_restart(restart)) {
		return call_simple_control_error_va_(NULL,
				"The restart ~S is already closed.", restart, NULL);
	}

	return 0;
}

static int rollback_restart_control_(addr control, addr restart, addr *ret)
{
	addr list, check;

	while (control != Nil) {
		getrestart_control(control, &list);
		while (list != Nil) {
			GetCons(list, &check, &list);
			if (check == restart)
				return Result(ret, control);
		}
		GetControl(control, Control_Next, &control);
	}
	*ret = 0;
	return fmte_("The restart ~S is invalid.", restart, NULL);
}

int invoke_restart_control_(Execute ptr, addr restart, addr args)
{
	int escape;
	addr call, next;

	if (symbolp(restart)) {
		Return(find_restart_control_error_(ptr, restart, Nil, &restart));
	}

	/* call */
	Return(check_enable_restart_control_(restart));
	getfunction_restart(restart, &call);
	Return(apply_control_(ptr, call, args));

	/* escape */
	escape = getescape_restart(restart);
	if (escape) {
		Return(rollback_restart_control_(ptr->control, restart, &next));
		ptr->throw_value = throw_restart_case;
		ptr->throw_handler = restart;
		ptr->throw_control = next;
		ptr->throw_point_p = 0;
		return 1;
	}

	return 0;
}

int invoke_restart_interactively_control_(Execute ptr, addr restart)
{
	addr args;

	if (symbolp(restart)) {
		Return(find_restart_control_error_(ptr, restart, Nil, &restart));
	}

	Return(check_enable_restart_control_(restart));
	getinteractive_restart(restart, &args);
	if (args != Nil) {
		Return(apply1_control_(ptr, &args, args, Nil));
	}

	return invoke_restart_control_(ptr, restart, args);
}


/*
 *  find-restart
 */
static int test_restart_control_(Execute ptr, addr restart, addr condition, int *ret)
{
	int check;
	addr test, list;

	/* associated */
	if (condition != Nil) {
		getassociated_restart(restart, &list);
		if (list != Nil) {
			check = find_list_eq_unsafe(condition, list);
			if (! check)
				return Result(ret, 0);
		}
	}

	/* :test */
	gettest_restart(restart, &test);
	if (test != Nil) {
		Return(funcall1_control_(ptr, &test, test, condition, NULL));
		if (test == Nil)
			return Result(ret, 0);
	}

	/* :test (constantly t) */
	return Result(ret, 1);
}

static int equal_restart_control_(Execute ptr,
		addr restart, addr symbol, addr condition, addr *value, int *ret)
{
	int check;
	addr name;

	getname_restart(restart, &name);
	if (name == symbol) {
		Return(test_restart_control_(ptr, restart, condition, &check));
		if (check) {
			*value = restart;
			return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int find_restart_stack_(Execute ptr,
		addr symbol, addr condition, addr *value, int *ret)
{
	int check;
	addr control, list, restart;

	control = ptr->control;
	while (control != Nil) {
		getrestart_control(control, &list);
		while (list != Nil) {
			GetCons(list, &restart, &list);
			Return(check_enable_restart_control_(restart));
			Return(equal_restart_control_(ptr,
						restart, symbol, condition, value, &check));
			if (check)
				return Result(ret, 1);
		}
		GetControl(control, Control_Next, &control);
	}

	/* not found */
	return Result(ret, 0);
}

int find_restart_control_(Execute ptr,
		addr name, addr condition, addr *value, int *ret)
{
	int check;

	/* name = restart */
	if (GetType(name) == LISPTYPE_RESTART) {
		if (getenable_restart(name)) {
			*value = name;
			return Result(ret, 1);
		}
		else {
			/* disable */
			*value = Nil;
			return Result(ret, 0);
		}
	}

	/* name = symbol */
	if (name == Nil) {
		*value = Nil;
		*ret = 0;
		return fmte_("The restart name ~S must not be a NIL.", name, NULL);
	}
	if (! symbolp(name)) {
		*value = Nil;
		*ret = 0;
		return fmte_("The argument ~S must be a symbol.", name, NULL);
	}
	if (condition != Nil) {
		Return(condition_instance_p_(condition, &check));
		if (! check) {
			*value = Nil;
			*ret = 0;
			return fmte_("The argument ~S "
					"must be a NIL or condition.", condition, NULL);
		}
	}

	return find_restart_stack_(ptr, name, condition, value, ret);
}

int find_restart_control_error_(Execute ptr, addr name, addr condition, addr *ret)
{
	int check;

	Return(find_restart_control_(ptr, name, condition, ret, &check));
	if (check == 0)
		return fmte_("The restart name ~S is not found.", name, NULL);

	return 0;
}

int compute_restarts_control_(Execute ptr, addr condition, addr *ret)
{
	int check;
	addr control, root, list, restart;
	LocalHold hold;

	if (condition != Nil) {
		Return(condition_instance_p_(condition, &check));
		if (! check) {
			*ret = Nil;
			return fmte_("The argument ~S "
					"must be a NIL or condition.", condition, NULL);
		}
	}
	hold = LocalHold_array(ptr, 1);
	control = ptr->control;
	for (root = Nil; control != Nil; ) {
		getrestart_control(control, &list);
		while (list != Nil) {
			GetCons(list, &restart, &list);
			Return(check_enable_restart_control_(restart));
			Return(test_restart_control_(ptr, restart, condition, &check));
			if (check) {
				pushnew_heap(root, restart, &root);
				localhold_set(hold, 0, root);
			}
		}
		GetControl(control, Control_Next, &control);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}


/*
 *  restart interface
 */
struct restart_call {
	union {
		int (*call_0)(Execute, void *);
		int (*call_1)(Execute, addr);
		int (*call_1r)(Execute, addr, addr *);
		int (*call_2)(Execute, addr, addr);
	} u;
	Execute ptr;
	addr restart;
	addr args[8];
	addr *ret;
	void *voidp;
};

static int restart_call_control_(struct restart_call *str,
		int (*call)(struct restart_call *))
{
	Execute ptr;
	addr control;

	/* execute */
	ptr = str->ptr;
	control = ptr->control;
	if (str->restart) {
		pushrestart_control(ptr, str->restart);
	}
	if ((*call)(str)) {
		if (! equal_control_restart(ptr, control)) {
			return 1;
		}
		/* restart abort */
		normal_throw_control(ptr);
		return 0;
	}

	/* free control */
	setresult_control(ptr, Nil);
	return 0;
}

/* restart */
int restart_control_(Execute ptr, int (*call)(Execute, void *), void *voidp)
{
	return restart0_control_(ptr, NULL, call, voidp);
}

/* restart0 */
static int restart0_control_adaptor(struct restart_call *str)
{
	return (str->u.call_0)(str->ptr, str->voidp);
}

int restart0_control_(Execute ptr, addr restart,
		int (*call)(Execute, void *), void *voidp)
{
	struct restart_call str;

	str.ptr = ptr;
	str.restart = restart;
	str.u.call_0 = call;
	str.voidp = voidp;
	return restart_call_control_(&str, restart0_control_adaptor);
}

/* restart1 */
static int restart1_control_adaptor(struct restart_call *str)
{
	return (str->u.call_1)(str->ptr, str->args[0]);
}

int restart1_control_(Execute ptr, addr restart,
		int (*call)(Execute, addr), addr v1)
{
	struct restart_call str;

	str.ptr = ptr;
	str.restart = restart;
	str.u.call_1 = call;
	str.args[0] = v1;
	return restart_call_control_(&str, restart1_control_adaptor);
}

/* restart1r */
static int restart1r_control_adaptor(struct restart_call *str)
{
	return (str->u.call_1r)(str->ptr, str->args[0], str->ret);
}

int restart1r_control_(Execute ptr, addr restart,
		int (*call)(Execute, addr, addr *), addr v1, addr *ret)
{
	struct restart_call str;

	str.ptr = ptr;
	str.restart = restart;
	str.u.call_1r = call;
	str.args[0] = v1;
	str.ret = ret;
	return restart_call_control_(&str, restart1r_control_adaptor);
}

/* restart2 */
static int restart2_control_adaptor(struct restart_call *str)
{
	return (str->u.call_2)(str->ptr, str->args[0], str->args[1]);
}

int restart2_control_(Execute ptr, addr restart,
		int (*call)(Execute, addr, addr), addr v1, addr v2)
{
	struct restart_call str;

	str.ptr = ptr;
	str.restart = restart;
	str.u.call_2 = call;
	str.args[0] = v1;
	str.args[1] = v2;
	return restart_call_control_(&str, restart2_control_adaptor);
}


/*
 *  code
 */
void set_taginfo_control(Execute ptr, addr list)
{
	addr control, pos, name, jump, lexical;
	size_t index;

	control = ptr->control;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &name, &jump, &lexical, NULL);
		GetIndex(jump, &index);
		taginfo_heap(&pos, control, name, index);
		GetIndex(lexical, &index);
		setlow_lexical_control(ptr, index, pos);
		pushtaginfo_control(ptr, pos);
	}
}

void set_blockinfo_control(Execute ptr, addr pos)
{
	addr name, lexical;
	size_t index;

	List_bind(pos, &name, &lexical, NULL);
	taginfo_heap(&pos, ptr->control, name, 0);
	GetIndex(lexical, &index);
	setlow_lexical_control(ptr, index, pos);
	pushtaginfo_control(ptr, pos);
}


/*
 *  catch / throw
 */
int catch_clang_(Execute ptr, pointer call, addr tag, addr value)
{
	addr pos, control;

	/* function */
	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, call);
	SetDataFunction(pos, value);
	/* execute */
	push_control(ptr, &control);
	catch_control(ptr, tag);
	if (apply_control_(ptr, pos, Nil)) {
		if (equal_control_catch(ptr, tag))
			normal_throw_control(ptr);
	}

	return pop_control_(ptr, control);
}


/*
 *  stack-frame
 */
static void stack_frame_list(addr control, addr *ret)
{
	addr list, pos;

	list = Nil;
	while (control != Nil) {
		GetControl(control, Control_Call, &pos);
		if (pos != Nil)
			cons_heap(&list, pos, list);
		GetControl(control, Control_Next, &control);
	}

	nreverse(ret, list);
}

int stack_frame_stream_(Execute ptr, addr stream)
{
	int ignore;
	addr list, pos, index;
	LocalHold hold;
	fixnum i;

	hold = LocalHold_array(ptr, 1);
	stack_frame_list(ptr->control, &list);
	Return(fresh_line_stream_(stream, &ignore));
	localhold_set(hold, 0, list);

	for (i = 0; list != Nil; i++) {
		GetCons(list, &pos, &list);
		fixnum_heap(&index, i);
		Return(format_stream_(ptr, stream, "~A: ~A~%", index, pos, NULL));
	}

	return 0;
}

int stack_frame_output_(Execute ptr)
{
	addr stream;
	Return(debug_io_stream_(ptr, &stream));
	return stack_frame_stream_(ptr, stream);
}


/************************************************************
 *  copy.c
 ************************************************************/

typedef void (*copyhard_calltype)(LocalRoot local, addr *ret, addr pos);
typedef void (*copylocal_calltype)(LocalRoot local, addr *ret, addr pos);
static copyhard_calltype TableCopy[LISPTYPE_SIZE];
static copylocal_calltype TableCopySoft[LISPTYPE_SIZE];


/*
 *  copy
 */
static void copyhard_error(LocalRoot local, addr *ret, addr pos)
{
	infoprint(pos);
	info("copy-error.");
	Abort("copy error");
}

static void copyhard_moveonly(LocalRoot local, addr *ret, addr pos)
{
	*ret = pos;
}

static void copyhard_type(LocalRoot local, addr *ret, addr pos)
{
	type_copy_alloc(local, ret, pos);
}

static void copyhard_cons(LocalRoot local, addr *ret, addr pos)
{
	addr left, right;

	CheckType(pos, LISPTYPE_CONS);
	GetCons(pos, &left, &right);
	copyhard_object(local, &left, left);
	copyhard_object(local, &right, right);
	cons_alloc(local, ret, left, right);
}

static void copyhard_vectorA2(LocalRoot local, addr *ret, addr left)
{
	addr right, pos;
	size_t size, i;

	CheckType(left, LISPTYPE_VECTOR);
	Check(GetStatusSize(left) != LISPSIZE_ARRAY2, "size error");
	LenArrayA2(left, &size);
	vector2_alloc(local, &right, size);
	for (i = 0; i < size; i++) {
		GetArrayA2(left, i, &pos);
		copyhard_object(local, &pos, pos);
		SetArrayA2(right, i, pos);
	}
	*ret = right;
}

static void copyhard_vectorA4(LocalRoot local, addr *ret, addr left)
{
	addr right, pos;
	size_t size, i;

	CheckType(left, LISPTYPE_VECTOR);
	Check(GetStatusSize(left) != LISPSIZE_ARRAY4, "size error");
	LenArrayA4(left, &size);
	vector4_alloc(local, &right, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(left, i, &pos);
		copyhard_object(local, &pos, pos);
		SetArrayA4(right, i, pos);
	}
	*ret = right;
}

#ifdef LISP_ARCH_64BIT
static void copyhard_vectorA8(LocalRoot local, addr *ret, addr left)
{
	addr right, pos;
	size_t size, i;

	CheckType(left, LISPTYPE_VECTOR);
	Check(GetStatusSize(left) != LISPSIZE_ARRAY8, "size error");
	LenArrayA8(left, &size);
	vector8_alloc(local, &right, size);
	for (i = 0; i < size; i++) {
		GetArrayA8(left, i, &pos);
		copyhard_object(local, &pos, pos);
		SetArrayA8(right, i, pos);
	}
	*ret = right;
}
#endif

static void copyhard_vector(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_VECTOR);
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			copyhard_vectorA2(local, ret, pos);
			break;

		case LISPSIZE_ARRAY4:
			copyhard_vectorA4(local, ret, pos);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			copyhard_vectorA8(local, ret, pos);
			break;
#endif

		default:
			Abort("size error");
			break;
	}
}

static void copyhard_character(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_CHARACTER);
	character_alloc(local, ret, RefCharacter(pos));
}

static void copyhard_string(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_STRING);
	strvect_copy_alloc(local, ret, pos);
}

static void copyhard_fixnum(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	fixnum_alloc(local, ret, RefFixnum(pos));
}

static void copyhard_bignum(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	bignum_copy_alloc(local, ret, pos);
}

static void copyhard_ratio(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_RATIO);
	ratio_copy_alloc(local, ret, pos);
}

static void copyhard_float(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	single_float_alloc(local, ret, RefSingleFloat(pos));
}

static void copyhard_double(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	double_float_alloc(local, ret, RefDoubleFloat(pos));
}

static void copyhard_long_double(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_LONG_FLOAT);
	long_float_alloc(local, ret, RefLongFloat(pos));
}

static void copyhard_complex(LocalRoot local, addr *ret, addr pos)
{
	enum ComplexType type;
	addr real, imag;

	CheckType(pos, LISPTYPE_COMPLEX);
	type = GetTypeComplex(pos);
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	copyhard_object(local, &real, real);
	copyhard_object(local, &imag, imag);

	make_complex_unsafe(local, &pos, type);
	SetRealComplex(pos, real);
	SetImagComplex(pos, imag);
	*ret = pos;
}

static void copyhard_callname(LocalRoot local, addr *ret, addr pos)
{
	CallNameType type;
	addr name;

	CheckType(pos, LISPTYPE_CALLNAME);
	GetCallName(pos, &name);
	copyhard_object(local, &name, name);
	GetCallNameType(pos, &type);
	callname_alloc(local, ret, name, type);
}

static void copyhard_random_state(LocalRoot local, addr *ret, addr pos)
{
	addr one;

	CheckType(pos, LISPTYPE_RANDOM_STATE);
	random_state_alloc(local, &one);
	copy_random_state(one, pos);
	*ret = one;
}

static void copyhard_pathname(LocalRoot local, addr *ret, addr pos)
{
	int i;
	addr one, child;

	Check(! pathnamep(pos), "type error");
	make_pathname_alloc(local, &one, pathname_logical_p(pos));
	for (i = 0; i < PATHNAME_INDEX_SIZE; i++) {
		GetArrayPathname(pos, (enum PATHNAME_INDEX)i, &child);
		copyhard_object(local, &child, child);
		SetArrayPathname(pos, (enum PATHNAME_INDEX)i, child);
	}
	*ret = one;
}

static void copyhard_paper(LocalRoot local, addr *ret, addr pos)
{
	addr one, value;
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_copy_body_alloc(local, &one, pos);
	paper_len_array(one, &size);
	for (i = 0; i < size; i++) {
		paper_get_array(pos, i, &value);
		copyhard_object(local, &value, value);
		paper_set_array(one, i, value);
	}
	*ret = one;
}

void copyhard_object(LocalRoot local, addr *ret, addr pos)
{
	int index;

	Check(pos == Unbound, "unbound error");
	index = (int)GetType(pos);
	Check(LISPTYPE_SIZE <= index, "index error");
	(TableCopy[index])(local, ret, pos);
}

static void init_copyhard_call(void)
{
	int i;

	for (i = 0; i < LISPTYPE_SIZE; i++)
		TableCopy[i] = copyhard_error;

	TableCopy[LISPTYPE_NIL] = copyhard_moveonly;
	TableCopy[LISPTYPE_T] = copyhard_moveonly;
	TableCopy[LISPTYPE_TYPE] = copyhard_type;
	TableCopy[LISPTYPE_CLOS] = copyhard_error;
	TableCopy[LISPTYPE_CONS] = copyhard_cons;
	TableCopy[LISPTYPE_ARRAY] = copyhard_error;
	TableCopy[LISPTYPE_VECTOR] = copyhard_vector;
	TableCopy[LISPTYPE_CHARACTER] = copyhard_character;
	TableCopy[LISPTYPE_STRING] = copyhard_string;
	TableCopy[LISPTYPE_HASHTABLE] = copyhard_error;
	TableCopy[LISPTYPE_READTABLE] = copyhard_error;
	TableCopy[LISPTYPE_SYMBOL] = copyhard_moveonly;
	TableCopy[LISPTYPE_FIXNUM] = copyhard_fixnum;
	TableCopy[LISPTYPE_BIGNUM] = copyhard_bignum;
	TableCopy[LISPTYPE_RATIO] = copyhard_ratio;
	TableCopy[LISPTYPE_SHORT_FLOAT] = copyhard_error;
	TableCopy[LISPTYPE_SINGLE_FLOAT] = copyhard_float;
	TableCopy[LISPTYPE_DOUBLE_FLOAT] = copyhard_double;
	TableCopy[LISPTYPE_LONG_FLOAT] = copyhard_long_double;
	TableCopy[LISPTYPE_COMPLEX] = copyhard_complex;
	TableCopy[LISPTYPE_CONTROL] = copyhard_error;
	TableCopy[LISPTYPE_CODE] = copyhard_error;
	TableCopy[LISPTYPE_CALLNAME] = copyhard_callname;
	TableCopy[LISPTYPE_FUNCTION] = copyhard_error;
	TableCopy[LISPTYPE_INDEX] = copyhard_error;
	TableCopy[LISPTYPE_PACKAGE] = copyhard_error;
	TableCopy[LISPTYPE_RANDOM_STATE] = copyhard_random_state;
	TableCopy[LISPTYPE_PATHNAME] = copyhard_pathname;
	TableCopy[LISPTYPE_STREAM] = copyhard_error;
	TableCopy[LISPTYPE_QUOTE] = copyhard_error;
	TableCopy[LISPTYPE_RESTART] = copyhard_error;
	TableCopy[LISPTYPE_PAPER] = copyhard_paper;
	TableCopy[LISPTYPE_EVAL] = copyhard_error;
}


/*
 *  checklocal
 */
static int copylocalp(LocalRoot local, addr pos)
{
	return local == NULL && GetStatusDynamic(pos);
}


/*
 *  copylocal
 */
static void copylocal_error(LocalRoot local, addr *ret, addr pos)
{
	Abort("copylocal error");
}

static void copylocal_type(LocalRoot local, addr *ret, addr pos)
{
	type_copy_alloc(local, ret, pos);
}

static void copylocal_cons(LocalRoot local, addr *ret, addr right)
{
	addr left;

	GetCons(right, &left, &right);
	copylocal_object(local, &left, left);
	copylocal_object(local, &right, right);
	cons_alloc(local, ret, left, right);
}

static void copylocal_vectorA2(LocalRoot local, addr *ret, addr left)
{
	addr right, pos;
	size_t size, i;

	LenArrayA2(left, &size);
	vector2_alloc(local, &right, size);
	for (i = 0; i < size; i++) {
		GetArrayA2(left, i, &pos);
		copylocal_object(local, &pos, pos);
		SetArrayA2(right, i, pos);
	}
	*ret = right;
}

static void copylocal_vectorA4(LocalRoot local, addr *ret, addr left)
{
	addr right, pos;
	size_t size, i;

	LenArrayA4(left, &size);
	vector4_alloc(local, &right, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(left, i, &pos);
		copylocal_object(local, &pos, pos);
		SetArrayA4(right, i, pos);
	}
	*ret = right;
}

#ifdef LISP_ARCH_64BIT
static void copylocal_vectorA8(LocalRoot local, addr *ret, addr left)
{
	addr right, pos;
	size_t size, i;

	LenArrayA8(left, &size);
	vector8_alloc(local, &right, size);
	for (i = 0; i < size; i++) {
		GetArrayA8(left, i, &pos);
		copylocal_object(local, &pos, pos);
		SetArrayA8(right, i, pos);
	}
	*ret = right;
}
#endif

static void copylocal_vector(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_VECTOR);
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			copylocal_vectorA2(local, ret, pos);
			break;

		case LISPSIZE_ARRAY4:
			copylocal_vectorA4(local, ret, pos);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			copylocal_vectorA8(local, ret, pos);
			break;
#endif

		default:
			Abort("size error");
			break;
	}
}

static void copylocal_character(LocalRoot local, addr *ret, addr pos)
{
	character_alloc(local, ret, RefCharacter(pos));
}

static void copylocal_string(LocalRoot local, addr *ret, addr pos)
{
	strvect_copy_alloc(local, ret, pos);
}

static void copylocal_fixnum(LocalRoot local, addr *ret, addr pos)
{
	fixnum_alloc(local, ret, RefFixnum(pos));
}

static void copylocal_bignum(LocalRoot local, addr *ret, addr pos)
{
	bignum_copy_alloc(local, ret, pos);
}

static void copylocal_ratio(LocalRoot local, addr *ret, addr pos)
{
	ratio_copy_alloc(local, ret, pos);
}

static void copylocal_single_float(LocalRoot local, addr *ret, addr pos)
{
	single_float_alloc(local, ret, RefSingleFloat(pos));
}

static void copylocal_double_float(LocalRoot local, addr *ret, addr pos)
{
	double_float_alloc(local, ret, RefDoubleFloat(pos));
}

static void copylocal_long_float(LocalRoot local, addr *ret, addr pos)
{
	long_float_alloc(local, ret, RefLongFloat(pos));
}

static void copylocal_complex(LocalRoot local, addr *ret, addr pos)
{
	enum ComplexType type;
	addr real, imag;

	CheckType(pos, LISPTYPE_COMPLEX);
	type = GetTypeComplex(pos);
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	if (copylocalp(local, real))
		copyhard_object(local, &real, real);
	if (copylocalp(local, imag))
		copyhard_object(local, &imag, imag);

	make_complex_unsafe(local, &pos, type);
	SetRealComplex(pos, real);
	SetImagComplex(pos, imag);
	*ret = pos;
}

static void copylocal_callname(LocalRoot local, addr *ret, addr pos)
{
	CallNameType type;
	addr name, one;

	GetCallName(pos, &name);
	GetCallNameType(pos, &type);
	if (copylocalp(local, name))
		copyhard_object(local, &name, name);
	make_callname_alloc(local, &one);
	SetCallName(one, name);
	SetCallNameType(one, type);
	*ret = one;
}

static void copylocal_pathname(LocalRoot local, addr *ret, addr pos)
{
	addr one, child;
	size_t i;

	make_pathname_alloc(local, &one, pathname_logical_p(pos));
	for (i = 0; i < PATHNAME_INDEX_SIZE; i++) {
		GetArrayPathname(pos, (enum PATHNAME_INDEX)i, &child);
		if (copylocalp(local, child))
			copyhard_object(local, &child, child);
		SetArrayPathname(one, (enum PATHNAME_INDEX)i, child);
	}
	*ret = one;
}

static void copylocal_paper(LocalRoot local, addr *ret, addr pos)
{
	addr one, value;
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_copy_body_alloc(local, &one, pos);
	paper_len_array(one, &size);
	for (i = 0; i < size; i++) {
		paper_get_array(pos, i, &value);
		copylocal_object(local, &value, value);
		paper_set_array(one, i, value);
	}
	*ret = one;
}

int copylocal_object(LocalRoot local, addr *ret, addr pos)
{
	int index;

	Check(pos == Unbound, "unbound error");
	if (copylocalp(local, pos)) {
		index = (int)GetType(pos);
		Check(LISPTYPE_SIZE <= index, "index error");
		(TableCopySoft[index])(local, ret, pos);
		return 1;
	}
	else {
		*ret = pos;
		return 0;
	}
}

void copylocal_list_stdarg(LocalRoot local, addr *ret, va_list args)
{
	addr left, right, next;

	left = va_arg(args, addr);
	if (left == NULL) {
		*ret = Nil;
		return;
	}
	copylocal_object(local, &left, left);
	conscar_alloc(local, &right, left);
	*ret = right;

	for (;;) {
		left = va_arg(args, addr);
		if (left == NULL)
			break;
		copylocal_object(local, &left, left);
		conscar_alloc(local, &next, left);
		SetCdr(right, next);
		right = next;
	}
}

int copyheap(addr *ret, addr pos)
{
	return copylocal_object(NULL, ret, pos);
}

addr copyheapr(addr pos)
{
	copyheap(&pos, pos);
	return pos;
}

static void init_copylocal_call(void)
{
	int i;

	for (i = 0; i < LISPTYPE_SIZE; i++)
		TableCopySoft[i] = copylocal_error;

	TableCopySoft[LISPTYPE_NIL] = copylocal_error;
	TableCopySoft[LISPTYPE_T] = copylocal_error;
	TableCopySoft[LISPTYPE_TYPE] = copylocal_type;
	TableCopySoft[LISPTYPE_CLOS] = copylocal_error;
	TableCopySoft[LISPTYPE_CONS] = copylocal_cons;
	TableCopySoft[LISPTYPE_ARRAY] = copylocal_error;
	TableCopySoft[LISPTYPE_VECTOR] = copylocal_vector;
	TableCopySoft[LISPTYPE_CHARACTER] = copylocal_character;
	TableCopySoft[LISPTYPE_STRING] = copylocal_string;
	TableCopySoft[LISPTYPE_HASHTABLE] = copylocal_error;
	TableCopySoft[LISPTYPE_READTABLE] = copylocal_error;
	TableCopySoft[LISPTYPE_SYMBOL] = copylocal_error;
	TableCopySoft[LISPTYPE_FIXNUM] = copylocal_fixnum;
	TableCopySoft[LISPTYPE_BIGNUM] = copylocal_bignum;
	TableCopySoft[LISPTYPE_RATIO] = copylocal_ratio;
	TableCopySoft[LISPTYPE_SHORT_FLOAT] = copylocal_error;
	TableCopySoft[LISPTYPE_SINGLE_FLOAT] = copylocal_single_float;
	TableCopySoft[LISPTYPE_DOUBLE_FLOAT] = copylocal_double_float;
	TableCopySoft[LISPTYPE_LONG_FLOAT] = copylocal_long_float;
	TableCopySoft[LISPTYPE_COMPLEX] = copylocal_complex;
	TableCopySoft[LISPTYPE_CONTROL] = copylocal_error;
	TableCopySoft[LISPTYPE_CODE] = copylocal_error;
	TableCopySoft[LISPTYPE_CALLNAME] = copylocal_callname;
	TableCopySoft[LISPTYPE_FUNCTION] = copylocal_error;
	TableCopySoft[LISPTYPE_INDEX] = copylocal_error;
	TableCopySoft[LISPTYPE_PACKAGE] = copylocal_error;
	TableCopySoft[LISPTYPE_RANDOM_STATE] = copylocal_error;
	TableCopySoft[LISPTYPE_PATHNAME] = copylocal_pathname;
	TableCopySoft[LISPTYPE_STREAM] = copylocal_error;
	TableCopySoft[LISPTYPE_QUOTE] = copylocal_error;
	TableCopySoft[LISPTYPE_RESTART] = copylocal_error;
	TableCopySoft[LISPTYPE_PAPER] = copylocal_paper;
	TableCopySoft[LISPTYPE_EVAL] = copylocal_error;
}


/*
 *  build
 */
void init_copy(void)
{
	init_copyhard_call();
	init_copylocal_call();
}


/************************************************************
 *  core.c
 ************************************************************/

#define CoreHeader (LISPNAME "CORE\0")
#define CoreHeaderSize (LISPNAMESIZE + 5)


/*
 *  save/load coreheader
 */
struct lisp_core_header {
	byte magic[CoreHeaderSize];
	uint16_t endian, a, b, c, arch;
};

static int savecore_header(filestream fm)
{
	struct lisp_core_header header;

	cleartype(header);
	memcpy(header.magic, CoreHeader, CoreHeaderSize);
	header.endian = 1;
	header.a = LISP_VERSION_A;
	header.b = LISP_VERSION_B;
	header.c = LISP_VERSION_C;
#ifdef LISP_64BIT
	header.arch = 1;
#else
	header.arch = 0;
#endif
	if (writecheck_filememory(fm, &header, sizeoft(header))) {
		Debug("writecheck error: header");
		return 1;
	}

	return 0;
}
static int loadcore_header(filestream fm)
{
	struct lisp_core_header header;

	if (readcheck_filememory(fm, &header, sizeoft(header))) {
		Debug("readcheck error: header");
		return 1;
	}
	if (memcmp(header.magic, CoreHeader, CoreHeaderSize) != 0) {
		Debug("loadcore_header: magic error");
		return 1;
	}
	if (header.endian != 1) {
		Debug("loadcore_header: endian error");
		return 1;
	}
	if (header.a != LISP_VERSION_A ||
			header.b != LISP_VERSION_B ||
			header.c != LISP_VERSION_C) {
		Debug("loadcore_header: version error");
		return 1;
	}
#ifdef LISP_64BIT
	if (header.arch != 1) {
		Debug("loadcore_header: arch error");
		return 1;
	}
#else
	if (header.arch != 0) {
		Debug("loadcore_header: arch error");
		return 1;
	}
#endif

	return 0;
}

/* save/load corefile */
static int savecore_file(filestream fm)
{
	if (savecore_header(fm)) {
		Debug("savecore_header error.");
		return 1;
	}
	if (save_lisp(fm)) {
		Debug("save_lisp error.");
		return 1;
	}

	return 0;
}
static int loadcore_file(filestream fm)
{
	if (loadcore_header(fm)) {
		Debug("loadcore_header error.");
		return 1;
	}
	if (load_lisp(fm)) {
		Debug("load_lisp error.");
		return 1;
	}

	return 0;
}


/* save/load root */
static int savecore_root(filestream fm)
{
	unsigned i;

	for (i = 0; i < LISPINDEX_SIZE; i++) {
		if (writeaddr_filememory(fm, lisp_root[i])) {
			Debug("writeaddr error: root");
			return 1;
		}
	}

	return 0;
}
static int loadcore_root(filestream fm)
{
	unsigned i;

	for (i = 0; i < LISPINDEX_SIZE; i++) {
		if (readaddr_filememory(fm, &(lisp_root[i]))) {
			Debug("readaddr error: root");
			return 1;
		}
	}

	/* nil, t */
	lisp_nil_object = lisp_root[LISPINDEX_NIL];
	lisp_t_object = lisp_root[LISPINDEX_T];
	Execute_Thread->control = lisp_nil_object;

	return 0;
}


/* clos */
static int savecore_clos(filestream fm)
{
	int i;
	addr value;
	addr array[] = {
		Clos_standard_class,
		Clos_standard_generic,
		Clos_standard_method,
		Clos_standard_combination,
		Clos_standard_specializer,
		NULL
	};
	for (i = 0; ; i++) {
		value = array[i];
		if (value == NULL)
			break;
		if (writeaddr_filememory(fm, value)) {
			Debug("writeaddr error: root");
			return 1;
		}
	}
	Check(i != 5, "index error");

	return 0;
}
static int loadcore_clos(filestream fm)
{
	int i;
	addr *value;
	addr *array[] = {
		&Clos_standard_class,
		&Clos_standard_generic,
		&Clos_standard_method,
		&Clos_standard_combination,
		&Clos_standard_specializer,
		NULL
	};
	for (i = 0; ; i++) {
		value = array[i];
		if (value == NULL)
			break;
		if (readaddr_filememory(fm, value)) {
			Debug("writeaddr error: root");
			return 1;
		}
	}
	Check(i != 5, "index error");

	return 0;
}


/*
 *  make-core
 */
static int savecore_execute_probe_file_(addr input, addr output, int *ret)
{
	int check;

	if (input == Nil)
		return Result(ret, 0);
	if (input == T)
		return Result(ret, 0);
	if (output == Nil)
		return Result(ret, 1);
	Return(pathname_equal_(input, output, &check));
	return Result(ret, ! check);
}

int savecore_execute_(Execute ptr, addr output, addr input, int exitp)
{
	int check;
	addr symbol, file_output, file_input;

	if (Index_Thread != 0)
		return fmte_("Thread Index must be 0.", NULL);
	if (count_execute() != 1)
		return fmte_("Any child thread must be destroyed.", NULL);

	/* (setq system::*core-output* output) */
	if (output == Nil) {
		file_output = Nil;
	}
	else {
		Return(name_physical_heap_(ptr, output, &file_output));
		Return(strvect_value_heap_(&file_output, file_output));
	}
	GetConst(SYSTEM_CORE_OUTPUT, &symbol);
	setspecial_symbol(symbol);
	SetValueSymbol(symbol, file_output);

	/* (setq system::*core-input* input) */
	if (input == Nil) {
		file_input = exitp? Nil: T;
	}
	else if (input == T) {
		file_input = file_output;
	}
	else {
		Return(name_physical_heap_(ptr, input, &file_input));
		Return(strvect_value_heap_(&file_input, file_input));
	}
	GetConst(SYSTEM_CORE_INPUT, &symbol);
	setspecial_symbol(symbol);
	SetValueSymbol(symbol, file_input);

	/* input check */
	Return(savecore_execute_probe_file_(input, output, &check));
	if (check) {
		Return(probe_file_files_(ptr, &file_input, input));
		if (file_input == Nil)
			return fmte_("File is not found, ~S.", input, NULL);
	}

	/* invoke */
	return call_savecore_condition_(ptr);
}

static int savecore_stream(Execute ptr, filestream fm)
{
	int check;
	addr pos;

	GetConst(SYSTEM_CORE_OUTPUT, &pos);
	GetValueSymbol(pos, &pos);
	if (pos == Nil)
		return 1;
	if (open_output_filememory_(ptr->local, fm, pos, FileOutput_supersede, &check))
		Abort("file open error.");
	if (check)
		Abort("file open error.");

	return 0;
}

static void savecore_result(Execute ptr)
{
	addr pos;

	GetConst(SYSTEM_CORE_OUTPUT, &pos);
	GetValueSymbol(pos, &pos);
	format_stdout_(ptr, "~&Core file: ~A~%", pos, NULL);
}

static int savecore_open(Execute ptr, filestream fm)
{
	int check;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	check = savecore_stream(ptr, fm);
	rollback_local(local, stack);

	return check;
}

int save_core(Execute ptr)
{
	struct filememory fm;

	/* open file */
	if (savecore_open(ptr, &fm))
		return 0;

	/* write file */
	gcexec(GcMode_Full);
	if (savecore_file(&fm)) {
		Debug("savecore_file error.");
		goto error;
	}

	/* root */
	if (savecore_root(&fm)) {
		Debug("savecore_root error.");
		goto error;
	}

	/* clos */
	if (savecore_clos(&fm)) {
		Debug("savecore_clos error.");
		goto error;
	}

	/* close file */
	if (close_filememory(&fm)) {
		Debug("close_filememory error.");
		return 1;
	}

	/* result output */
	savecore_result(ptr);
	return 0;

error:
	if (close_filememory(&fm)) {
		Debug("close_filememory error.");
	}
	return 1;
}

int load_core(const unicode *name, size_t size)
{
	struct filememory fm;

	if (input_unicode_filememory(&fm, name, size)) {
		/* Read error, try next core file */
		return -1;
	}
	if (loadcore_file(&fm)) {
		Debug("loadcore_file error.");
		goto error;
	}

	/* root */
	if (loadcore_root(&fm)) {
		Debug("loadcore_root error.");
		goto error;
	}

	/* clos */
	if (loadcore_clos(&fm)) {
		Debug("loadcore_clos error.");
		goto error;
	}

	/* stream */
	if (update_standard_stream()) {
		Debug("update_standard_stream error.");
		return 1;
	}

	/* close file */
	if (close_filememory(&fm)) {
		Debug("close_filememory error.");
		return 1;
	}
	return 0;

error:
	if (close_filememory(&fm)) {
		Debug("close_filememory error.");
	}
	return 1;
}

static int loadcore_reload_(Execute ptr, struct lispargv *argv)
{
	addr symbol, input;
	lispstringu file;
	unicode *str, c;
	size_t size, i;

	GetConst(SYSTEM_CORE_INPUT, &symbol);
	GetValueSymbol(symbol, &input);
	if (input == Unbound || input == Nil)
		return 0;
	if (input == T) {
		argv->reload = 1;
		argv->reload_core = NULL;
		return 0;
	}

	/* make lispstringu */
	if (! strvectp(input))
		return fmte_("The object ~S must be a strvect type.", input, NULL);
	strvect_length(input, &size);
	file = make_stringu(size + 1UL);
	if (file == NULL)
		return fmte_("make_stringu error.", NULL);
	str = file->ptr;
	for (i = 0; i < size; i++) {
		strvect_getc(input, i, &c);
		str[i] = c;
	}
	str[i] = 0;

	/* argv */
	argv->reload = 1;
	argv->reload_core = file;

	return 0;
}

int save_and_load_core_(Execute ptr, struct lispargv *argv, int *ret)
{
	Return(loadcore_reload_(ptr, argv));
	*ret = save_core(ptr);
	return 0;
}


/************************************************************
 *  core_store.c
 ************************************************************/

#ifdef LISP_DEBUG
#define LOAD_STORE_SIZE		3
#else
#define LOAD_STORE_SIZE		256
#endif

/*
 *  variable
 */
struct load_store_struct {
	struct load_store_struct *next;
	addr data[LOAD_STORE_SIZE];
	size_t size;
};

static struct load_store_struct *LoadStoreRoot;


/*
 *  function
 */
static struct load_store_struct *load_store_alloc(void)
{
	struct load_store_struct *ptr;

	ptr = (struct load_store_struct *)malloc(sizeoft(struct load_store_struct));
	if (ptr == NULL)
		return NULL;
	ptr->size = 0;

	return ptr;
}

int load_store_init(void)
{
	struct load_store_struct *ptr;

	Check(LoadStoreRoot != NULL, "load_store_init error.");
	ptr = load_store_alloc();
	if (ptr == NULL) {
		Debug("malloc error.");
		return 1;
	}
	ptr->next = NULL;
	LoadStoreRoot = ptr;
	return 0;
}

int load_store_push(addr pos)
{
	struct load_store_struct *ptr, *root;

	ptr = LoadStoreRoot;
	Check(ptr == NULL, "load_store_push error.");

	/* extend */
	if (LOAD_STORE_SIZE <= ptr->size) {
		root = load_store_alloc();
		if (root == NULL) {
			Debug("malloc error.");
			return 1;
		}
		root->next = ptr;
		LoadStoreRoot = ptr = root;
	}

	/* store */
	ptr->data[ptr->size++] = pos;
	return 0;
}

void load_store_error(void)
{
	struct load_store_struct *x, *y;

	for (x = LoadStoreRoot; x; x = y) {
		y = x->next;
		free(x);
	}
	LoadStoreRoot = NULL;
}

static void load_store_addr(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_CODE:
			update_code(pos);
			break;

		default:
			break;
	}
}

void load_store_exec(void)
{
	addr *data;
	struct load_store_struct *x, *y;
	size_t size, i;

	/* execute */
	for (x = LoadStoreRoot; x; x = y) {
		y = x->next;
		size = x->size;
		data = x->data;
		for (i = 0; i < size; i++)
			load_store_addr(data[i]);
	}

	/* free */
	load_store_error();
}


/************************************************************
 *  declare.c
 ************************************************************/


/*
 *  declaration object
 */
static void make_eval_declare(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_DECLARE, EVAL_DECLARE_SIZE,
			sizeoft(OptimizeType) * EVAL_OPTIMIZE_SIZE);
}
static void eval_declare_alloc_optimize(LocalRoot local, addr *ret, OptimizeType value)
{
	int i;
	addr pos;

	make_eval_declare(local, &pos);
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++)
		SetEvalDeclareOptimize(pos, i, value);
	*ret = pos;
}
void eval_declare_alloc(LocalRoot local, addr *ret)
{
	eval_declare_alloc_optimize(local, ret, -1);
}
void eval_declare_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	eval_declare_alloc(local, ret);
}
void eval_declare_heap(addr *ret)
{
	eval_declare_alloc(NULL, ret);
}

int empty_declare(addr pos)
{
	int i;
	addr check;
	OptimizeType value;

	Check(! eval_declare_p(pos), "type error");
	/* array */
	for (i = 0; i < EVAL_DECLARE_SIZE; i++) {
		GetEvalDeclare(pos, i, &check);
		if (check != Nil)
			return 0;
	}

	/* optimize */
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		GetEvalDeclareOptimize(pos, i, &value);
		if (0 <= value)
			return 0;
	}

	return 1;
}
int empty_nil_declare(addr pos)
{
	return pos == Nil || (eval_declare_p(pos) && empty_declare(pos));
}

void apply_array_declare(OptimizeType *array, addr pos)
{
	int i;
	OptimizeType value;

	if (pos == Nil)
		return;
	Check(! eval_declare_p(pos), "type error");
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		GetEvalDeclareOptimize(pos, i, &value);
		if (0 <= value)
			array[i] = value;
	}
}

addr refevaldeclare(addr pos, size_t index)
{
	Check(! eval_declare_p(pos), "type error");
	return RefEvalDeclare_Low(pos, index);
}
void getevaldeclare(addr pos, size_t index, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare_Low(pos, index, ret);
}
void setevaldeclare(addr pos, size_t index, addr value)
{
	Check(! eval_declare_p(pos), "type error");
	SetEvalDeclare_Low(pos, index, value);
}
OptimizeType refevaldeclareoptimize(addr pos, size_t index)
{
	Check(! eval_declare_p(pos), "type error");
	return RefEvalDeclareOptimize_Low(pos, index);
}
void getevaldeclareoptimize(addr pos, size_t index, OptimizeType *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclareOptimize_Low(pos, index, ret);
}
void setevaldeclareoptimize(addr pos, size_t index, OptimizeType value)
{
	Check(! eval_declare_p(pos), "type error");
	SetEvalDeclareOptimize_Low(pos, index, value);
}


/*
 *  access
 */
static void set_type_declare_heap(addr pos, addr symbol, addr type)
{
	addr list;

	Check(! eval_declare_p(pos), "type error");
	CheckSymbol(symbol);
	CheckType(type, LISPTYPE_TYPE);
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_VALUE, &list);
	if (setplist_heap(list, symbol, type, &list))
		SetEvalDeclare(pos, EVAL_DECLARE_TYPE_VALUE, list);
}
static void push_type_declare_heap(addr pos, addr symbol, addr type)
{
	addr list, temp;

	Check(! eval_declare_p(pos), "type error");
	CheckSymbol(symbol);
	CheckType(type, LISPTYPE_TYPE);
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_VALUE, &list);

	if (getplist(list, symbol, &temp)) {
		/* don't exist */
		set_type_declare_heap(pos, symbol, type);
	}
	else {
		/* already exists */
		type2and_heap(temp, type, &type);
		set_type_declare_heap(pos, symbol, type);
	}
}

static void set_ftype_declare_heap(addr pos, addr callname, addr type)
{
	addr list;

	Check(! eval_declare_p(pos), "type error");
	CheckType(callname, LISPTYPE_CALLNAME);
	CheckType(type, LISPTYPE_TYPE);
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_FUNCTION, &list);
	if (setplist_callname_heap(list, callname, type, &list))
		SetEvalDeclare(pos, EVAL_DECLARE_TYPE_FUNCTION, list);
}
static void push_ftype_declare_heap(addr pos, addr callname, addr type)
{
	addr list, temp;

	Check(! eval_declare_p(pos), "type error");
	CheckType(callname, LISPTYPE_CALLNAME);
	CheckType(type, LISPTYPE_TYPE);
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_FUNCTION, &list);

	if (getplist_callname(list, callname, &temp)) {
		/* don't exist */
		set_ftype_declare_heap(pos, callname, type);
	}
	else {
		/* already exists */
		type2and_heap(temp, type, &type);
		set_ftype_declare_heap(pos, callname, type);
	}
}

static void plist_constant_declare_heap(addr pos, addr symbol,
		constindex constant, enum EVAL_DECLARE declare)
{
	addr list, value;

	Check(! eval_declare_p(pos), "type error");
	CheckSymbol(symbol);
	GetConstant(constant, &value);
	GetEvalDeclare(pos, declare, &list);
	if (setplist_heap(list, symbol, value, &list))
		SetEvalDeclare(pos, declare, list);
}

static void plist_callname_declare_heap(addr pos, addr callname,
		constindex constant, enum EVAL_DECLARE declare)
{
	addr list, value;

	Check(! eval_declare_p(pos), "type error");
	CheckType(callname, LISPTYPE_CALLNAME);
	GetConstant(constant, &value);
	GetEvalDeclare(pos, declare, &list);
	if (setplist_callname_heap(list, callname, value, &list))
		SetEvalDeclare(pos, declare, list);
}

static void push_inline_declare_heap(addr pos, addr callname)
{
	plist_callname_declare_heap(pos, callname,
			CONSTANT_COMMON_INLINE,
			EVAL_DECLARE_INLINE);
}

static void push_notinline_declare_heap(addr pos, addr callname)
{
	plist_callname_declare_heap(pos, callname,
			CONSTANT_COMMON_NOTINLINE,
			EVAL_DECLARE_INLINE);
}

static void push_ignore_value_declare_heap(addr pos, addr symbol)
{
	plist_constant_declare_heap(pos, symbol,
			CONSTANT_COMMON_IGNORE,
			EVAL_DECLARE_IGNORE_VALUE);
}

static void push_ignorable_value_declare_heap(addr pos, addr symbol)
{
	plist_constant_declare_heap(pos, symbol,
			CONSTANT_COMMON_IGNORABLE,
			EVAL_DECLARE_IGNORE_VALUE);
}

static void push_ignore_function_declare_heap(addr pos, addr callname)
{
	plist_callname_declare_heap(pos, callname,
			CONSTANT_COMMON_IGNORE,
			EVAL_DECLARE_IGNORE_FUNCTION);
}

static void push_ignorable_function_declare_heap(addr pos, addr callname)
{
	plist_callname_declare_heap(pos, callname,
			CONSTANT_COMMON_IGNORABLE,
			EVAL_DECLARE_IGNORE_FUNCTION);
}

static void push_constant_declare_heap(addr pos, addr symbol,
		enum EVAL_DECLARE declare)
{
	addr list;

	Check(! eval_declare_p(pos), "type error");
	CheckSymbol(symbol);
	GetEvalDeclare(pos, declare, &list);
	if (pushnew_heap(list, symbol, &list))
		SetEvalDeclare(pos, declare, list);
}
static void push_special_declare_heap(addr pos, addr symbol)
{
	push_constant_declare_heap(pos, symbol, EVAL_DECLARE_SPECIAL);
}
static void push_dynamic_value_declare_heap(addr pos, addr symbol)
{
	push_constant_declare_heap(pos, symbol, EVAL_DECLARE_DYNAMIC_VALUE);
}

static void push_callname_declare_heap(addr pos, addr callname,
		enum EVAL_DECLARE declare)
{
	addr list;

	Check(! eval_declare_p(pos), "type error");
	CheckType(callname, LISPTYPE_CALLNAME);
	GetEvalDeclare(pos, declare, &list);
	if (pushnewlist_callname_heap(list, callname, &list))
		SetEvalDeclare(pos, declare, list);
}
static void push_dynamic_function_declare_heap(addr pos, addr callname)
{
	push_callname_declare_heap(pos, callname, EVAL_DECLARE_DYNAMIC_FUNCTION);
}

static void push_declaration_declare_heap(addr pos, addr symbol)
{
	push_constant_declare_heap(pos, symbol, EVAL_DECLARE_DECLARATION);
}

static int check_constant_declare(addr pos, addr symbol, enum EVAL_DECLARE declare)
{
	Check(! eval_declare_p(pos), "type error");
	if (! symbolp(symbol))
		return 0;
	GetEvalDeclare(pos, declare, &pos);
	return find_list_eq_unsafe(symbol, pos);
}
static int check_declaration_declare(addr pos, addr symbol)
{
	return check_constant_declare(pos, symbol, EVAL_DECLARE_DECLARATION);
}

OptimizeType get_optimize_declare(addr pos, enum EVAL_OPTIMIZE index)
{
	Check(! eval_declare_p(pos), "type error");
	return RefEvalDeclareOptimize(pos, index);
}

OptimizeType get_optimize_compilation_declare(addr pos)
{
	return get_optimize_declare(pos, EVAL_OPTIMIZE_COMPILATION);
}

OptimizeType get_optimize_debug_declare(addr pos)
{
	return get_optimize_declare(pos, EVAL_OPTIMIZE_DEBUG);
}

OptimizeType get_optimize_safety_declare(addr pos)
{
	return get_optimize_declare(pos, EVAL_OPTIMIZE_SAFETY);
}

OptimizeType get_optimize_space_declare(addr pos)
{
	return get_optimize_declare(pos, EVAL_OPTIMIZE_SPACE);
}

OptimizeType get_optimize_speed_declare(addr pos)
{
	return get_optimize_declare(pos, EVAL_OPTIMIZE_SPEED);
}


/*
 *  getall
 */
void getall_declaration_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_DECLARATION, ret);
}

void getall_inline_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_INLINE, ret);
}

void getall_special_declare(addr pos, addr *ret)
{
	if (pos == Nil) {
		*ret = Nil;
		return;
	}

	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_SPECIAL, ret);
}

void getall_type_value_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_VALUE, ret);
}

void getall_type_function_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_FUNCTION, ret);
}

const OptimizeType *getall_optimize_declare(addr pos)
{
	Check(! eval_declare_p(pos), "type error");
	return PtrEvalDeclare_Low(pos);
}

void getall_dynamic_value_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_DYNAMIC_VALUE, ret);
}

void getall_dynamic_function_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_DYNAMIC_FUNCTION, ret);
}

void getall_ignore_value_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_IGNORE_VALUE, ret);
}

void getall_ignore_function_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_IGNORE_FUNCTION, ret);
}


/*
 *  build_declare
 */
void getroot_declare(addr *ret)
{
	*ret = LispRoot(DECLARE);
	Check(! eval_declare_p(*ret), "type error");
}

void setroot_declare(addr pos)
{
	Check(! eval_declare_p(pos), "type error");
	SetLispRoot(DECLARE, pos);
}

void build_declare(void)
{
	addr pos;
	eval_declare_alloc_optimize(NULL, &pos, 1);
	setroot_declare(pos);
}

void push_declaration_declaim(addr symbol)
{
	addr eval;
	getroot_declare(&eval);
	push_declaration_declare_heap(eval, symbol);
}

void copy_optimize_declare(OptimizeType *array)
{
	int i;
	addr pos;
	OptimizeType value;

	getroot_declare(&pos);
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		GetEvalDeclareOptimize(pos, i, &value);
		array[i] = value;
	}
}

static void apply_optimize_declaim(enum EVAL_OPTIMIZE index, OptimizeType value)
{
	addr pos;
	getroot_declare(&pos);
	SetEvalDeclareOptimize(pos, index, value);
}
void apply_compilation_speed_declaim(OptimizeType value)
{
	apply_optimize_declaim(EVAL_OPTIMIZE_COMPILATION, value);
}
void apply_debug_declaim(OptimizeType value)
{
	apply_optimize_declaim(EVAL_OPTIMIZE_DEBUG, value);
}
void apply_safety_declaim(OptimizeType value)
{
	apply_optimize_declaim(EVAL_OPTIMIZE_SAFETY, value);
}
void apply_space_declaim(OptimizeType value)
{
	apply_optimize_declaim(EVAL_OPTIMIZE_SPACE, value);
}
void apply_speed_declaim(OptimizeType value)
{
	apply_optimize_declaim(EVAL_OPTIMIZE_SPEED, value);
}


/*
 *  parse-declaration
 */
static int check_callname_heap_(addr *ret, addr symbol)
{
	addr check;

	Return(parse_callname_error_(&symbol, symbol));
	GetCallName(symbol, &check);
	Return(check_variable_(check));

	return Result(ret, symbol);
}

static int decl_type_(Execute ptr, addr env, addr eval, addr cons)
{
	addr type, symbol;

	Return_getcons(cons, &type, &cons);
	Return(parse_type_(ptr, &type, type, env));
	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(check_variable_(symbol));
		push_type_declare_heap(eval, symbol, type);
	}

	return 0;
}

static int decl_ftype_(Execute ptr, addr env, addr eval, addr form)
{
	addr cons, type, symbol;

	if (! consp_getcons(form, &type, &cons))
		return fmte_("Invalid ftype form, ~S.", form, NULL);
	Return(parse_type_(ptr, &type, type, env));
	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(check_callname_heap_(&symbol, symbol));
		push_ftype_declare_heap(eval, symbol, type);
	}

	return 0;
}

static int decl_special_(addr eval, addr cons)
{
	addr symbol;

	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(check_variable_(symbol));
		push_special_declare_heap(eval, symbol);
	}

	return 0;
}

static int decl_inline_(addr eval, addr cons)
{
	addr symbol;

	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(check_callname_heap_(&symbol, symbol));
		push_inline_declare_heap(eval, symbol);
	}

	return 0;
}

static int decl_notinline_(addr eval, addr cons)
{
	addr symbol;

	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(check_callname_heap_(&symbol, symbol));
		push_notinline_declare_heap(eval, symbol);
	}

	return 0;
}

static int decl_declaration_(addr eval, addr cons)
{
	addr symbol;

	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(check_variable_(symbol));
		push_declaration_declare_heap(eval, symbol);
	}

	return 0;
}

static int function_callname_p_(addr *value, addr pos, int *ret)
{
	addr type, symbol, check;

	if (GetType(pos) != LISPTYPE_CONS)
		return Result(ret, 0);
	GetCons(pos, &type, &pos);
	if (GetType(pos) != LISPTYPE_CONS)
		return Result(ret, 0);
	GetCons(pos, &symbol, &pos);
	if (pos != Nil)
		return Result(ret, 0);
	GetConst(COMMON_FUNCTION, &check);
	if (check != type)
		return Result(ret, 0);
	Return(check_callname_heap_(value, symbol));

	return Result(ret, 1);
}

static int decl_ignore_(addr eval, addr cons)
{
	int check;
	addr symbol;

	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(function_callname_p_(&symbol, symbol, &check));
		if (check) {
			push_ignore_function_declare_heap(eval, symbol);
		}
		else {
			Return(check_variable_(symbol));
			push_ignore_value_declare_heap(eval, symbol);
		}
	}

	return 0;
}

static int decl_ignorable_(addr eval, addr cons)
{
	int check;
	addr symbol;

	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(function_callname_p_(&symbol, symbol, &check));
		if (check) {
			push_ignorable_function_declare_heap(eval, symbol);
		}
		else {
			Return(check_variable_(symbol));
			push_ignorable_value_declare_heap(eval, symbol);
		}
	}

	return 0;
}

static int decl_dynamic_extent_(addr eval, addr cons)
{
	int check;
	addr symbol;

	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(function_callname_p_(&symbol, symbol, &check));
		if (check) {
			push_dynamic_function_declare_heap(eval, symbol);
		}
		else {
			Return(check_variable_(symbol));
			push_dynamic_value_declare_heap(eval, symbol);
		}
	}

	return 0;
}

static int check_optimize_symbol_(addr symbol, enum EVAL_OPTIMIZE *ret, int *ignore)
{
	addr check;

	*ignore = 0;
	GetConst(COMMON_SAFETY, &check);
	if (check == symbol)
		return Result(ret, EVAL_OPTIMIZE_SAFETY);

	GetConst(COMMON_SPACE, &check);
	if (check == symbol)
		return Result(ret, EVAL_OPTIMIZE_SPACE);

	GetConst(COMMON_SPEED, &check);
	if (check == symbol)
		return Result(ret, EVAL_OPTIMIZE_SPEED);

	GetConst(COMMON_COMPILATION_SPEED, &check);
	if (check == symbol)
		return Result(ret, EVAL_OPTIMIZE_COMPILATION);

	GetConst(COMMON_DEBUG, &check);
	if (check == symbol)
		return Result(ret, EVAL_OPTIMIZE_DEBUG);

	*ignore = 1;
	*ret = EVAL_OPTIMIZE_SIZE;
	return fmtw_("Invalid optimize symbol ~S.", symbol, NULL);
}

static int decl_optimize_symbol_(addr eval, addr symbol)
{
	int ignore;
	enum EVAL_OPTIMIZE index;

	Return(check_optimize_symbol_(symbol, &index, &ignore));
	if (ignore)
		return 0;
	SetEvalDeclareOptimize(eval, (int)index, 3);

	return 0;
}

static int decl_optimize_cons_(addr eval, addr cons)
{
	int ignore;
	enum EVAL_OPTIMIZE index;
	addr symbol, value;
	fixnum check;

	Return_getcons(cons, &symbol, &cons);
	if (GetType(symbol) != LISPTYPE_SYMBOL)
		return fmte_("The optimize type ~S must be a symbol.", symbol, NULL);
	Return(check_optimize_symbol_(symbol, &index, &ignore));
	if (ignore)
		return 0;

	/* (speed) -> (speed 3) */
	if (cons == Nil) {
		SetEvalDeclareOptimize(eval, (int)index, 3);
		return 0;
	}

	/* (speed x) */
	Return_getcons(cons, &value, &cons);
	if (cons != Nil)
		return fmte_("Invalid optimize argument ~S.", cons, NULL);
	if (GetType(value) != LISPTYPE_FIXNUM)
		return fmte_("The optimize value ~S must be a fixnum.", value, NULL);
	GetFixnum(value, &check);
	if (check < 0 || 3 < check)
		return fmte_("The optimize value ~S must be between 0 and 3.", value, NULL);
	SetEvalDeclareOptimize(eval, (int)index, (int)check);

	return 0;
}

static int decl_optimize_(addr eval, addr cons)
{
	addr one;

	while (cons != Nil) {
		Return_getcons(cons, &one, &cons);
		switch (GetType(one)) {
			case LISPTYPE_SYMBOL:
				Return(decl_optimize_symbol_(eval, one));
				break;

			case LISPTYPE_CONS:
				Return(decl_optimize_cons_(eval, one));
				break;

			default:
				return fmte_("Invalid optimize argument ~S.", one, NULL);
		}
	}

	return 0;
}

static int declaration_p(addr eval, addr symbol)
{
	if (check_declaration_declare(eval, symbol))
		return 1;
	getroot_declare(&eval);
	return check_declaration_declare(eval, symbol);
}

static int decl_otherwise_(Execute ptr, addr env, addr eval, addr type, addr cons)
{
	addr symbol;

	/* declaration */
	if (declaration_p(eval, type)) {
		/* do nothing */
		return 0;
	}

	/* (type ...) */
	Return(parse_type_(ptr, &type, type, env));
	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(check_variable_(symbol));
		push_type_declare_heap(eval, symbol, type);
	}

	return 0;
}

static int push_declaim_(Execute ptr, addr env, addr eval, addr symbol, addr cons)
{
	addr check;

	/* type */
	GetConst(COMMON_TYPE, &check);
	if (check == symbol)
		return decl_type_(ptr, env, eval, cons);

	/* ftype */
	GetConst(COMMON_FTYPE, &check);
	if (check == symbol)
		return decl_ftype_(ptr, env, eval, cons);

	/* special */
	GetConst(COMMON_SPECIAL, &check);
	if (check == symbol)
		return decl_special_(eval, cons);

	/* inline */
	GetConst(COMMON_INLINE, &check);
	if (check == symbol)
		return decl_inline_(eval, cons);

	/* notinline */
	GetConst(COMMON_NOTINLINE, &check);
	if (check == symbol)
		return decl_notinline_(eval, cons);

	/* declaration */
	GetConst(COMMON_DECLARATION, &check);
	if (check == symbol)
		return decl_declaration_(eval, cons);

	/* optimize */
	GetConst(COMMON_OPTIMIZE, &check);
	if (check == symbol)
		return decl_optimize_(eval, cons);

	/* error/otherwise */
	GetConst(COMMON_DYNAMIC_EXTENT, &check);
	if (check == symbol)
		return fmte_("dynamic-extent don't allow in the declaim/proclaim form.", NULL);

	GetConst(COMMON_IGNORE, &check);
	if (check == symbol)
		return fmte_("ignore don't allow in the declaim/proclaim form.", NULL);

	GetConst(COMMON_IGNORABLE, &check);
	if (check == symbol)
		return fmte_("ignorable don't allow in the declaim/proclaim form.", NULL);

	return decl_otherwise_(ptr, env, eval, symbol, cons);
}

static int push_declare_(Execute ptr, addr env, addr eval, addr symbol, addr cons)
{
	addr check;

	/* type */
	GetConst(COMMON_TYPE, &check);
	if (check == symbol)
		return decl_type_(ptr, env, eval, cons);

	/* ftype */
	GetConst(COMMON_FTYPE, &check);
	if (check == symbol)
		return decl_ftype_(ptr, env, eval, cons);

	/* special */
	GetConst(COMMON_SPECIAL, &check);
	if (check == symbol)
		return decl_special_(eval, cons);

	/* inline */
	GetConst(COMMON_INLINE, &check);
	if (check == symbol)
		return decl_inline_(eval, cons);

	/* notinline */
	GetConst(COMMON_NOTINLINE, &check);
	if (check == symbol)
		return decl_notinline_(eval, cons);

	/* ignore */
	GetConst(COMMON_IGNORE, &check);
	if (check == symbol)
		return decl_ignore_(eval, cons);

	/* ignorable */
	GetConst(COMMON_IGNORABLE, &check);
	if (check == symbol)
		return decl_ignorable_(eval, cons);

	/* dynamic-extent */
	GetConst(COMMON_DYNAMIC_EXTENT, &check);
	if (check == symbol)
		return decl_dynamic_extent_(eval, cons);

	/* optimize */
	GetConst(COMMON_OPTIMIZE, &check);
	if (check == symbol)
		return decl_optimize_(eval, cons);

	/* error/otherwise */
	GetConst(COMMON_DECLARATION, &check);
	if (check == symbol)
		return fmte_("declaration don't allow in the declare form.", NULL);

	return decl_otherwise_(ptr, env, eval, symbol, cons);
}

static int parse_declare_form_(Execute ptr, addr env, addr decl, addr *ret,
		int (*call_)(Execute, addr ,addr, addr, addr))
{
	addr eval, car, tail;
	LocalHold hold;

	eval_declare_heap(&eval);
	hold = LocalHold_local(ptr);
	localhold_pushva(hold, eval, decl, NULL);
	while (decl != Nil) {
		Return_getcons(decl, &car, &decl);
		Return_getcons(car, &car, &tail);
		Return((*call_)(ptr, env, eval, car, tail));
	}
	localhold_end(hold);

	return Result(ret, eval);
}

int parse_declaim_heap_(Execute ptr, addr env, addr decl, addr *ret)
{
	return parse_declare_form_(ptr, env, decl, ret, push_declaim_);
}

int parse_declare_heap_(Execute ptr, addr env, addr decl, addr *ret)
{
	return parse_declare_form_(ptr, env, decl, ret, push_declare_);
}

int parse_optimize_heap_(addr decl, addr *value, int *ret)
{
	addr eval, pos, car, tail, optimize;

	eval_declare_heap(&eval);
	GetConst(COMMON_OPTIMIZE, &optimize);
	while (decl != Nil) {
		Return_getcons(decl, &pos, &decl);
		Return_getcons(pos, &car, &tail);
		if (car != optimize)
			return Result(ret, 1);
		Return(decl_optimize_(eval, tail));
	}
	*value = eval;
	return Result(ret, 0);
}


/*
 *  declare_body_documentation
 */
static int declare_split_(addr cons, addr *retdecl, addr *retbody)
{
	addr decl, car, cdr, declare;

	GetConst(COMMON_DECLARE, &declare);
	for (decl = Nil; cons != Nil; ) {
		Return_getcar(cons, &cdr);
		if (GetType(cdr) != LISPTYPE_CONS)
			break;
		Return_getcons(cdr, &car, &cdr);
		if (car != declare)
			break;
		while (cdr != Nil) {
			Return_getcons(cdr, &car, &cdr);
			cons_heap(&decl, car, decl);
		}
		Return_getcdr(cons, &cons);
	}
	nreverse(retdecl, decl);
	*retbody = cons;
	return 0;
}

int declare_body_form_(addr list, addr *retdecl, addr *retbody)
{
	addr declare, decl, car, cdr;

	GetConst(COMMON_DECLARE, &declare);
	for (decl = Nil; list != Nil; ) {
		Return_getcar(list, &cdr);
		if (! consp(cdr))
			break;
		Return_getcar(cdr, &car);
		if (car != declare)
			break;
		cons_heap(&decl, cdr, decl);
		Return_getcdr(list, &list);
	}
	nreverse(retdecl, decl);
	*retbody = list;
	return 0;
}

int declare_body_(Execute ptr, addr env, addr cons, addr *retdecl, addr *retbody)
{
	addr decl;

	Return(declare_split_(cons, &decl, retbody));
	if (decl != Nil)
		return parse_declare_heap_(ptr, env, decl, retdecl);
	*retdecl = Nil;
	return 0;
}

int declare_body_documentation_(Execute ptr, addr env,
		addr cons, addr *rdoc, addr *rdecl, addr *rbody)
{
	addr car, cdr;

	/* nil */
	if (cons == Nil) {
		*rdoc = *rdecl = *rbody = Nil;
		return 0;
	}

	/* (body) */
	Return_getcons(cons, &car, &cdr);
	if (cdr == Nil) {
		*rdoc = Nil;
		return declare_body_(ptr, env, cons, rdecl, rbody);
	}

	/* (doc . body) */
	if (stringp(car)) {
		*rdoc = car;
		return declare_body_(ptr, env, cdr, rdecl, rbody);
	}

	/* (decl . nil) */
	Return(declare_body_(ptr, env, cons, rdecl, &cdr));
	if (cdr == Nil) {
		*rdoc = *rbody = Nil;
		return 0;
	}

	/* ([decl] doc . body) */
	Return_getcons(cdr, &car, &cons);
	if (stringp(car)) {
		if (cons == Nil) {
			*rdoc = Nil;
			*rbody = cdr;
		}
		else {
			*rdoc = car;
			*rbody = cons;
		}
		return 0;
	}

	/* ([decl] . body) */
	*rdoc = Nil;
	*rbody = cdr;
	return 0;
}

int split_decl_body_doc_(addr list, addr *rdoc, addr *rdecl, addr *rbody)
{
	addr car, cdr;

	/* nil */
	if (list == Nil) {
		*rdoc = *rdecl = *rbody = Nil;
		return 0;
	}

	/* (body) */
	Return_getcons(list, &car, &cdr);
	if (cdr == Nil) {
		*rdoc = Nil;
		return declare_split_(list, rdecl, rbody);
	}

	/* (doc . body) */
	if (stringp(car)) {
		*rdoc = car;
		return declare_split_(cdr, rdecl, rbody);
	}

	/* (decl . nil) */
	Return(declare_split_(list, rdecl, &cdr));
	if (cdr == Nil) {
		*rdoc = *rbody = Nil;
		return 0;
	}

	/* ([decl] doc . body) */
	Return_getcons(cdr, &car, &list);
	if (stringp(car)) {
		if (list == Nil) {
			*rdoc = Nil;
			*rbody = cdr;
		}
		else {
			*rdoc = car;
			*rbody = list;
		}
		return 0;
	}

	/* ([decl] . body) */
	*rdoc = Nil;
	*rbody = cdr;
	return 0;
}


/*
 *  copy eval-declare
 */
static void copy_declare_type_v(LocalRoot local,
		enum EVAL_DECLARE type, addr pos, addr eval)
{
	addr list, key, value, root;

	GetEvalDeclare(pos, type, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &key, &list);
		GetCons(list, &value, &list);
		type_copy_alloc(local, &value, value);
		cons_alloc(local, &root, key, root);
		cons_alloc(local, &root, value, root);
	}
	nreverse(&root, root);
	SetEvalDeclare(eval, type, root);
}

static void copy_declare_type_f(LocalRoot local,
		enum EVAL_DECLARE type, addr pos, addr eval)
{
	addr list, key, value, root;

	GetEvalDeclare(pos, type, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &key, &list);
		GetCons(list, &value, &list);
		copy_callname_alloc(local, &key, key);
		type_copy_alloc(local, &value, value);
		cons_alloc(local, &root, key, root);
		cons_alloc(local, &root, value, root);
	}
	nreverse(&root, root);
	SetEvalDeclare(eval, type, root);
}

static void copy_declare_push_v(LocalRoot local,
		enum EVAL_DECLARE type, addr pos, addr eval)
{
	addr list, value, root;

	GetEvalDeclare(pos, type, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &value, &list);
		cons_alloc(local, &root, value, root);
	}
	nreverse(&root, root);
	SetEvalDeclare(eval, type, root);
}

static void copy_declare_push_f(LocalRoot local,
		enum EVAL_DECLARE type, addr pos, addr eval)
{
	addr list, value, root;

	GetEvalDeclare(pos, type, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &value, &list);
		copy_callname_alloc(local, &value, value);
		cons_alloc(local, &root, value, root);
	}
	nreverse(&root, root);
	SetEvalDeclare(eval, type, root);
}

static void copy_declare_plist_v(LocalRoot local,
		enum EVAL_DECLARE type, addr pos, addr eval)
{
	addr list, key, value, root;

	GetEvalDeclare(pos, type, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &key, &list);
		GetCons(list, &value, &list);
		cons_alloc(local, &root, key, root);
		cons_alloc(local, &root, value, root);
	}
	nreverse(&root, root);
	SetEvalDeclare(eval, type, root);
}

static void copy_declare_plist_f(LocalRoot local,
		enum EVAL_DECLARE type, addr pos, addr eval)
{
	addr list, key, value, root;

	GetEvalDeclare(pos, type, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &key, &list);
		GetCons(list, &value, &list);
		copy_callname_alloc(local, &key, key);
		cons_alloc(local, &root, key, root);
		cons_alloc(local, &root, value, root);
	}
	nreverse(&root, root);
	SetEvalDeclare(eval, type, root);
}

void copy_eval_declare_alloc(LocalRoot local, addr *ret, addr pos)
{
	int i;
	addr eval;
	OptimizeType value;

	make_eval_declare(local, &eval);
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		GetEvalDeclareOptimize(pos, i, &value);
		SetEvalDeclareOptimize(eval, i, value);
	}
	copy_declare_type_v(local, EVAL_DECLARE_TYPE_VALUE, pos, eval);
	copy_declare_type_f(local, EVAL_DECLARE_TYPE_FUNCTION, pos, eval);
	copy_declare_push_v(local, EVAL_DECLARE_SPECIAL, pos, eval);
	copy_declare_plist_f(local, EVAL_DECLARE_INLINE, pos, eval);
	copy_declare_plist_v(local, EVAL_DECLARE_IGNORE_VALUE, pos, eval);
	copy_declare_plist_f(local, EVAL_DECLARE_IGNORE_FUNCTION, pos, eval);
	copy_declare_push_v(local, EVAL_DECLARE_DYNAMIC_VALUE, pos, eval);
	copy_declare_push_f(local, EVAL_DECLARE_DYNAMIC_FUNCTION, pos, eval);
	copy_declare_push_v(local, EVAL_DECLARE_DECLARATION, pos, eval);
	*ret = eval;
}

void copy_eval_declare_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	copy_eval_declare_alloc(local, ret, pos);
}

void copy_eval_declare_heap(addr *ret, addr pos)
{
	copy_eval_declare_alloc(NULL, ret, pos);
}


/*
 *  debug
 */
static void set_index_optimize_declare(addr pos,
		OptimizeType value, enum EVAL_OPTIMIZE index)
{
	Check(! eval_declare_p(pos), "type error");
	Check(! (0 <= value && value <= 3), "range error");
	SetEvalDeclareOptimize(pos, index, value);
}
void set_optimize_compilation_declare(addr pos, OptimizeType value)
{
	set_index_optimize_declare(pos, value, EVAL_OPTIMIZE_COMPILATION);
}
void set_optimize_debug_declare(addr pos, OptimizeType value)
{
	set_index_optimize_declare(pos, value, EVAL_OPTIMIZE_DEBUG);
}
void set_optimize_safety_declare(addr pos, OptimizeType value)
{
	set_index_optimize_declare(pos, value, EVAL_OPTIMIZE_SAFETY);
}
void set_optimize_space_declare(addr pos, OptimizeType value)
{
	set_index_optimize_declare(pos, value, EVAL_OPTIMIZE_SPACE);
}
void set_optimize_speed_declare(addr pos, OptimizeType value)
{
	set_index_optimize_declare(pos, value, EVAL_OPTIMIZE_SPEED);
}


/*
 *  proclaim
 */
static int parse_proclaim_heap_(Execute ptr, addr env, addr car, addr *ret)
{
	addr eval, tail;
	LocalHold hold;

	eval_declare_heap(&eval);
	hold = LocalHold_local_push(ptr, eval);
	Return_getcons(car, &car, &tail);
	Return(push_declaim_(ptr, env, eval, car, tail));
	localhold_end(hold);

	return Result(ret, eval);
}

static int apply_type_value_proclaim_(addr pos)
{
	addr key, value;

	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_VALUE, &pos);
	while (pos != Nil) {
		GetCons(pos, &key, &pos);
		GetCons(pos, &value, &pos);
		CheckSymbol(key);
		CheckType(value, LISPTYPE_TYPE);
		Return(settype_value_symbol_(key, value));
	}

	return 0;
}

static int apply_type_function_proclaim_(addr pos)
{
	addr key, value, symbol;

	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_FUNCTION, &pos);
	while (pos != Nil) {
		GetCons(pos, &key, &pos);
		GetCons(pos, &value, &pos);
		CheckType(key, LISPTYPE_CALLNAME);
		CheckType(value, LISPTYPE_TYPE);

		GetCallName(key, &symbol);
		if (symbolp_callname(key)) {
			Return(settype_function_symbol_(symbol, value));
		}
		else {
			Return(settype_setf_symbol_(symbol, value));
		}
	}

	return 0;
}

static int apply_special_proclaim_(addr pos)
{
	addr symbol;

	GetEvalDeclare(pos, EVAL_DECLARE_SPECIAL, &pos);
	while (pos != Nil) {
		GetCons(pos, &symbol, &pos);
		CheckSymbol(symbol);
		Return(setspecial_symbol_(symbol));
	}

	return 0;
}

static void apply_inline_value_proclaim(addr key, addr value)
{
	addr check, symbol;

	GetCallName(key, &symbol);
	/* inline */
	GetConst(COMMON_INLINE, &check);
	if (check == value) {
		if (symbolp_callname(key))
			setinline_function_symbol(symbol);
		else
			setinline_setf_symbol(symbol);
	}
	/* notinline */
	GetConst(COMMON_NOTINLINE, &check);
	if (check == value) {
		if (symbolp_callname(key))
			setnotinline_function_symbol(symbol);
		else
			setnotinline_setf_symbol(symbol);
	}
}

static void apply_inline_proclaim(addr pos)
{
	addr key, value;

	GetEvalDeclare(pos, EVAL_DECLARE_INLINE, &pos);
	while (pos != Nil) {
		GetCons(pos, &key, &pos);
		GetCons(pos, &value, &pos);
		CheckType(key, LISPTYPE_CALLNAME);
		apply_inline_value_proclaim(key, value);
	}
}

static void apply_optimize_proclaim(addr pos)
{
	int i;
	OptimizeType value;

	for (i = 0; i < (int)EVAL_OPTIMIZE_SIZE; i++) {
		GetEvalDeclareOptimize(pos, i, &value);
		if (0 <= value)
			apply_optimize_declaim((enum EVAL_OPTIMIZE)i, value);
	}
}

static void apply_declaration_proclaim(addr pos)
{
	addr symbol;

	GetEvalDeclare(pos, EVAL_DECLARE_DECLARATION, &pos);
	while (pos != Nil) {
		GetCons(pos, &symbol, &pos);
		CheckSymbol(symbol);
		push_declaration_declaim(symbol);
	}
}

int proclaim_common_(Execute ptr, addr var)
{
	Return(parse_proclaim_heap_(ptr, Nil, var, &var));
	Return(apply_type_value_proclaim_(var));
	Return(apply_type_function_proclaim_(var));
	Return(apply_special_proclaim_(var));
	apply_inline_proclaim(var);
	apply_optimize_proclaim(var);
	apply_declaration_proclaim(var);

	return 0;
}

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#ifdef __clang__
#pragma clang diagnostic pop
#endif
