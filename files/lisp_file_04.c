/*
 *  npt -- ANSI Common Lisp Programming Language.
 *    https://github.com/nptcl/npt
 *    https://github.com/nptcl/npt-amalgamation
 *
 *  File: lisp_file_04.c
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
#include <math.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lisp_file.h"


/************************************************************
 *  degrade.c
 ************************************************************/

#define LISP_DEGRADE_RTONLY
#undef LISP_DEGRADE_RTONLY

#ifdef LISP_DEGRADE
/*
 *  prototype declaration
 */
int test_c99(void);
int test_arch(void);
int test_alloc(void);
int test_memory(void);
int test_heap(void);
int test_heap_memory(void);
int test_local(void);
int test_execute(void);
int test_character(void);
int test_character_check(void);
int test_character_name(void);
int test_character_queue(void);
int test_strvect(void);
int test_array(void);
int test_array_access(void);
int test_array_make(void);
int test_strtype(void);
int test_object(void);
int test_symbol(void);
int test_callname(void);
int test_function(void);
int test_cons(void);
int test_cons_list(void);
int test_cons_plist(void);
int test_gc_execute(void);
int test_sxhash(void);
int test_hashtable(void);
int test_sequence(void);
int test_sort(void);
int test_pathname(void);

int test_code_object(void);
int test_control(void);
int test_control_callbind(void);
int test_file_memory(void);
int test_encode(void);
int test_file(void);
int test_stream(void);
int test_integer(void);
int test_print(void);
int test_format_parse(void);
int test_format_print(void);
int test_format_function(void);
int test_format_float(void);
int test_format_radix(void);
int test_format(void);
int test_bignum_cons(void);
int test_bignum_data(void);
int test_bignum(void);
int test_ratio(void);
int test_ratio_equal(void);
int test_ratio_plus(void);
int test_ratio_multi(void);
int test_number(void);
int test_token(void);
int test_reader(void);
int test_quote(void);
int test_package(void);
int test_lambda(void);
int test_clos(void);
int test_clos_class(void);
int test_clos_type(void);
int test_clos_cache(void);
int test_clos_combination(void);
int test_clos_generic(void);
int test_clos_method(void);
int test_type_copy(void);
int test_type_memory(void);
int test_type_name(void);
int test_type_object(void);
int test_type_parse(void);
int test_type_function(void);
int test_type_symbol(void);
int test_type_table(void);
int test_type_typep(void);
int test_type_upgraded(void);
int test_type_value(void);
int test_subtypep_range(void);
int test_subtypep_optimize(void);
int test_subtypep_number(void);
int test_subtypep_atomic(void);
int test_subtypep_table(void);
int test_subtypep_compound(void);
int test_subtypep_andor(void);
int test_subtypep(void);
int test_declare(void);
int test_parse(void);
int test_parse_function(void);
int test_parse_macro(void);
int test_parse_object(void);
int test_scope(void);
int test_code_queue(void);
int test_code_make(void);
int test_eval(void);
int test_eval_copy(void);
int test_eval_table(void);
int test_eval_stack(void);
int test_equal(void);
int test_condition(void);
int test_bit(void);
int test_extern_type(void);
int test_extern_sequence(void);
int test_extern_object(void);
int test_extern_control(void);
int test_extern_function(void);
int test_extern_execute(void);
int test_extern_instance(void);
int test_extern_error(void);
int test_extern_print(void);
int test_optimize(void);
int test_loadrt(void);


/*
 *  degrade function
 */
void degrade_execute(void)
{
#ifdef LISP_DEGRADE_RTONLY
	DegradeCheck(test_loadrt);
#else
#if 0
#endif
	DegradeCheck(test_c99);
	DegradeCheck(test_arch);
	DegradeCheck(test_alloc);
	DegradeCheck(test_memory);
	DegradeCheck(test_local);
	DegradeCheck(test_heap_memory);
	DegradeCheck(test_heap);
	DegradeCheck(test_execute);
	DegradeCheck(test_object);
	DegradeCheck(test_symbol);
	DegradeCheck(test_callname);
	DegradeCheck(test_function);
	DegradeCheck(test_cons);
	DegradeCheck(test_cons_list);
	DegradeCheck(test_cons_plist);
	DegradeCheck(test_character);
	DegradeCheck(test_character_check);
	DegradeCheck(test_character_name);
	DegradeCheck(test_character_queue);
	DegradeCheck(test_strvect);
	DegradeCheck(test_array);
	DegradeCheck(test_array_make);
	DegradeCheck(test_array_access);
	DegradeCheck(test_strtype);
	DegradeCheck(test_gc_execute);
	DegradeCheck(test_sxhash);
	DegradeCheck(test_hashtable);
	DegradeCheck(test_sequence);
	DegradeCheck(test_sort);
	DegradeCheck(test_pathname);

	DegradeCheck(test_code_object);
	DegradeCheck(test_control);
	DegradeCheck(test_control_callbind);
	DegradeCheck(test_file_memory);
	DegradeCheck(test_encode);
	DegradeCheck(test_file);
	DegradeCheck(test_bignum_cons);
	DegradeCheck(test_bignum_data);
	DegradeCheck(test_bignum);
	DegradeCheck(test_ratio);
	DegradeCheck(test_ratio_equal);
	DegradeCheck(test_ratio_plus);
	DegradeCheck(test_ratio_multi);
	DegradeCheck(test_number);
	DegradeCheck(test_token);
	DegradeCheck(test_reader);
	DegradeCheck(test_quote);
	DegradeCheck(test_package);
	DegradeCheck(test_lambda);
	DegradeCheck(test_clos);
	DegradeCheck(test_clos_class);
	DegradeCheck(test_clos_type);
	DegradeCheck(test_clos_cache);
	DegradeCheck(test_clos_generic);
	DegradeCheck(test_clos_combination);
	DegradeCheck(test_clos_method);
	DegradeCheck(test_type_memory);
	DegradeCheck(test_type_table);
	DegradeCheck(test_type_symbol);
	DegradeCheck(test_type_parse);
	DegradeCheck(test_type_typep);
	DegradeCheck(test_type_name);
	DegradeCheck(test_type_value);
	DegradeCheck(test_type_object);
	DegradeCheck(test_type_copy);
	DegradeCheck(test_type_function);
	DegradeCheck(test_type_upgraded);
	DegradeCheck(test_subtypep_range);
	DegradeCheck(test_subtypep_optimize);
	DegradeCheck(test_subtypep_number);
	DegradeCheck(test_subtypep_atomic);
	DegradeCheck(test_subtypep_table);
	DegradeCheck(test_subtypep_compound);
	DegradeCheck(test_subtypep_andor);
	DegradeCheck(test_subtypep);
	DegradeCheck(test_equal);
	DegradeCheck(test_declare);
	DegradeCheck(test_parse);
	DegradeCheck(test_parse_function);
	DegradeCheck(test_parse_macro);
	DegradeCheck(test_parse_object);
	DegradeCheck(test_scope);
	DegradeCheck(test_code_queue);
	DegradeCheck(test_code_make);
	DegradeCheck(test_eval);
	DegradeCheck(test_eval_copy);
	DegradeCheck(test_eval_table);
	DegradeCheck(test_eval_stack);
	DegradeCheck(test_integer);
	DegradeCheck(test_bit);
	DegradeCheck(test_format_parse);
	DegradeCheck(test_format_print);
	DegradeCheck(test_format_function);
	DegradeCheck(test_format_float);
	DegradeCheck(test_format_radix);
	DegradeCheck(test_format);
	DegradeCheck(test_print);
	DegradeCheck(test_stream);
	DegradeCheck(test_condition);
	DegradeCheck(test_extern_type);
	DegradeCheck(test_extern_sequence);
	DegradeCheck(test_extern_object);
	DegradeCheck(test_extern_control);
	DegradeCheck(test_extern_function);
	DegradeCheck(test_extern_execute);
	DegradeCheck(test_extern_instance);
	DegradeCheck(test_extern_error);
	DegradeCheck(test_extern_print);
	DegradeCheck(test_optimize);
	DegradeCheck(test_loadrt);
#if 0
#endif
#endif
}

#endif


/************************************************************
 *  develop.c
 ************************************************************/

#ifdef LISP_DEGRADE

void degrade_execute(void);
static FILE *file = NULL;
int DegradeCount = 0;
int DegradeError = 0;

#define LISP_DEGRADE_WIDTH 60
static int DegradeSwitch = 1;
static int DegradePosition;


/* degrade */
int degrade_code(void (*init)(Execute), int (*call)(void))
{
	int errorp, finish;
	lisp_abort_calltype handler;
	Execute ptr;

	freelisp();
	alloclisp(0, 0);

	lisp_info_enable = 1;
	ptr = Execute_Thread;
	errorp = 0;
	finish = 0;
	handler = set_degrade_setjmp_handler();
	Lisp_degrade_Begin {
		lisp_initialize = 1;
		if (init)
			(*init)(ptr);
		errorp = (*call)();
		finish = 1;
	}
	Lisp_degrade_End;
	(void)set_abort_handler(handler);
	freelisp();
	lisp_info_enable = 1;

	/* result */
	if (finish == 0) {
		errorp = 1;
	}

	return errorp;
}

int degrade_printf(const char *fmt, ...)
{
	int result;
	va_list args;

	va_start(args, fmt);
	result = vfprintf(file, fmt, args);
	va_end(args);
	fflush(file);

	return result;
}

int degrade_test(int check, const char *name)
{
	DegradeCount++;
	if (check) {
		if (DegradeSwitch) {
			degrade_printf(".");
			DegradePosition++;
			if (LISP_DEGRADE_WIDTH <= DegradePosition) {
				degrade_printf("\n");
				DegradePosition = 0;
			}
		}
		else {
			degrade_printf("[OK] %7d: %s\n", DegradeCount, name);
		}
		return 0;
	}
	else {
		if (DegradeSwitch) {
			if (DegradePosition != 0) {
				degrade_printf("\n");
				DegradePosition = 0;
			}
		}
		degrade_printf("[ERROR] %7d: %s\n", DegradeCount, name);
		DegradeError++;
		return 1;
	}
}

static void degrade_freshline(void)
{
	if (DegradeSwitch && DegradePosition) {
		degrade_printf("\n");
	}
}

void degrade_title(const char *name)
{
	degrade_freshline();
	degrade_printf("[%s]\n", name);
}

int degrade_testcheck(int check)
{
	if (check) {
		DegradeError++;
	}

	return check;
}

void degrade_increment(void)
{
	DegradeError++;
}

void degrade_output_null(Execute ptr)
{
	addr stream, null;

	Error(open_broadcast_stream_(&null, Nil));
	GetConst(SPECIAL_STANDARD_OUTPUT, &stream);
	setspecial_local(ptr, stream, null);
	GetConst(SPECIAL_ERROR_OUTPUT, &stream);
	setspecial_local(ptr, stream, null);
	GetConst(SPECIAL_DEBUG_IO, &stream);
	setspecial_local(ptr, stream, null);
}

static void degrade_execute_call(void)
{
	int finish;
	lisp_abort_calltype handler;

	handler = set_abort_setjmp_handler();
	finish = 0;
	Lisp_abort_Begin {
		degrade_execute();
		finish = 1;
	}
	Lisp_abort_End;
	(void)set_abort_handler(handler);

	if (finish == 0)
		DegradeError++;
}

static int degradelisp_call(void)
{
	file = stdout;
	DegradeCount = 0;
	DegradeError = 0;
	DegradePosition = 0;

#ifdef LISP_DEBUG_FORCE_GC
	GcCounterForce = 0;
#endif
	degrade_printf("DEGRADE - start: %s\n", LISP_INFO);
	degrade_execute_call();
	degrade_freshline();
	degrade_printf("---\n");
	degrade_printf("DEGRADE - end: %s\n", LISP_INFO);

	if (DegradeError) {
		degrade_printf("ERROR!! (%d).\n", DegradeError);
		return 1;
	}
	else {
		degrade_printf("OK.\n");
		return 0;
	}
}

int degradelisp(void)
{
	int check;

	/* runcode */
	begin_terme();
	check = degradelisp_call();
	end_terme();

	return check;
}

#else

int degradelisp(void)
{
	info("degrade-mode is not implemented.");
	return 0;
}
#endif


/************************************************************
 *  document.c
 ************************************************************/

/*
 *  generic function
 */
static void defun_documentation_order(addr *ret, addr *rlist, addr *rindex)
{
	addr pos, object, doc_type, list, index;
	struct argument_struct *str;

	/* lambda */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = 2;

	/* var */
	GetConst(SYSTEM_OBJECT, &object);
	GetConst(SYSTEM_DOC_TYPE, &doc_type);
	list_heap(&list, object, doc_type, NULL);
	SetArgument(pos, ArgumentIndex_var, list);

	/* order */
	list_heap(&list, doc_type, object, NULL);
	index_heap(&object, 0);
	index_heap(&doc_type, 1);
	list_heap(&index, doc_type, object, NULL);

	/* result */
	*ret = pos;
	*rlist = list;
	*rindex = index;
}

static void defun_setf_documentation_order(addr *ret, addr *rlist, addr *rindex)
{
	addr pos, value, object, doc_type, list, index;
	struct argument_struct *str;

	/* lambda */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = 3;

	/* var */
	GetConst(SYSTEM_VALUE, &value);
	GetConst(SYSTEM_OBJECT, &object);
	GetConst(SYSTEM_DOC_TYPE, &doc_type);
	list_heap(&list, value, object, doc_type, NULL);
	SetArgument(pos, ArgumentIndex_var, list);

	/* order */
	list_heap(&list, doc_type, object, value, NULL);
	index_heap(&value, 0);
	index_heap(&object, 1);
	index_heap(&doc_type, 2);
	list_heap(&index, doc_type, object, value, NULL);

	/* result */
	*ret = pos;
	*rlist = list;
	*rindex = index;
}

static void method_type_documentation(addr *ret, enum TypeTable type, constindex index)
{
	addr args, values;

	gettypetable(type, &args);
	GetConstant(index, &values);
	type_eql_heap(values, &values);
	typeargs_var2(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, ret);
}
#define MethodTypeDocumentation(r,a,b) \
	method_type_documentation((r), TypeTable_##a, CONSTANT_COMMON_##b)

static int mop_argument_method_documentation_(addr *ret, constindex a, constindex b)
{
	addr pos, pos1, pos2, x, y;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* (object class) */
	GetConst(SYSTEM_OBJECT, &x);
	GetConstant(a, &y);
	Check(! closp(y), "type error, class");
	list_heap(&pos1, x, y, NULL);
	/* (doc-type (eql symbol)) */
	GetConst(SYSTEM_DOC_TYPE, &x);
	GetConstant(b, &y);
	Check(! symbolp(y), "type error, symbol.");
	Return(clos_intern_specializer_(y, &y));
	list_heap(&pos2, x, y, NULL);
	/* var */
	str->var = 2;
	list_heap(&pos1, pos1, pos2, NULL);
	SetArgument(pos, ArgumentIndex_var, pos1);
	/* result */
	return Result(ret, pos);
}
#define MopArgumentMethodDocumentation(r,b,c) { \
	Return(mop_argument_method_documentation_((r), \
				CONSTANT_CLOS_##b, CONSTANT_COMMON_##c)) \
}

static void method_type_setf_documentation(addr *ret,
		enum TypeTable type, constindex index)
{
	addr args, values, first;

	GetTypeTable(&first, T);
	gettypetable(type, &args);
	GetConstant(index, &values);
	type_eql_heap(values, &values);
	typeargs_var3(&args, first, args, values);
	typeargs_method(args);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, ret);
}
#define MethodTypeSetfDocumentation(r,a,b) \
	method_type_setf_documentation((r), TypeTable_##a, CONSTANT_COMMON_##b)

static int mop_argument_method_setf_documentation_(addr *ret,
		constindex a, constindex b)
{
	addr pos, pos1, pos2, pos3, x, y;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* (value T) */
	GetConst(SYSTEM_VALUE, &x);
	GetConst(CLOS_T, &y);
	Check(! closp(y), "type error, class");
	list_heap(&pos1, x, y, NULL);
	/* (object class) */
	GetConst(SYSTEM_OBJECT, &x);
	GetConstant(a, &y);
	Check(! closp(y), "type error, class");
	list_heap(&pos2, x, y, NULL);
	/* (doc-type (eql symbol)) */
	GetConst(SYSTEM_DOC_TYPE, &x);
	GetConstant(b, &y);
	Check(! symbolp(y), "type error, symbol.");
	Return(clos_intern_specializer_(y, &y));
	list_heap(&pos3, x, y, NULL);
	/* var */
	str->var = 3;
	list_heap(&pos1, pos1, pos2, pos3, NULL);
	SetArgument(pos, ArgumentIndex_var, pos1);
	/* result */
	return Result(ret, pos);
}
#define MopArgumentMethodSetfDocumentation(r,b,c) { \
	Return(mop_argument_method_setf_documentation_((r), \
				CONSTANT_CLOS_##b, CONSTANT_COMMON_##c)); \
}


/*
 *  (function (eql 't))
 */
static int method_documentation_function_t(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_function_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_function_t(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_function_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_function_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_function_t);
	MethodTypeDocumentation(&type, Function, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, FUNCTION, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_function_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_function_t);
	MethodTypeSetfDocumentation(&type, Function, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, FUNCTION, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (function (eql 'function))
 */
static int documentation_function_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_function_t);
	MethodTypeDocumentation(&type, Function, FUNCTION);
	/* method */
	MopArgumentMethodDocumentation(&pos, FUNCTION, FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_function_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_function_t);
	MethodTypeSetfDocumentation(&type, Function, FUNCTION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, FUNCTION, FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (list (eql 'function))
 */
static int method_documentation_list_function(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_function_setf_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_list_function(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_function_setf_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_list_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_list_function);
	MethodTypeDocumentation(&type, List, FUNCTION);
	/* method */
	MopArgumentMethodDocumentation(&pos, LIST, FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_list_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_list_function);
	MethodTypeSetfDocumentation(&type, List, FUNCTION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, LIST, FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (list (eql 'compiler-macro))
 */
static int method_documentation_list_compiler_macro(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_compiler_macro_setf_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_list_compiler_macro(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_compiler_macro_setf_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_list_compiler_macro_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_list_compiler_macro);
	MethodTypeDocumentation(&type, List, COMPILER_MACRO);
	/* method */
	MopArgumentMethodDocumentation(&pos, LIST, COMPILER_MACRO);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_list_compiler_macro_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_list_compiler_macro);
	MethodTypeSetfDocumentation(&type, List, COMPILER_MACRO);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, LIST, COMPILER_MACRO);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (symbol (eql 'function))
 */
static int method_documentation_symbol_function(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_function_symbol_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_symbol_function(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_function_symbol_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_symbol_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_function);
	MethodTypeDocumentation(&type, Symbol, FUNCTION);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_symbol_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_function);
	MethodTypeSetfDocumentation(&type, Symbol, FUNCTION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (symbol (eql 'compiler-macro))
 */
static int method_documentation_symbol_compiler_macro(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_compiler_macro_symbol_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_symbol_compiler_macro(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_compiler_macro_symbol_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_symbol_compiler_macro_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_compiler_macro);
	MethodTypeDocumentation(&type, Symbol, COMPILER_MACRO);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, COMPILER_MACRO);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_symbol_compiler_macro_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_compiler_macro);
	MethodTypeSetfDocumentation(&type, Symbol, COMPILER_MACRO);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, COMPILER_MACRO);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (symbol (eql 'setf))
 */
static int method_documentation_symbol_setf(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_defsetf_symbol_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_symbol_setf(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_defsetf_symbol_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_symbol_setf_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_setf);
	MethodTypeDocumentation(&type, Symbol, SETF);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, SETF);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_symbol_setf_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_setf);
	MethodTypeSetfDocumentation(&type, Symbol, SETF);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, SETF);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (method-combination (eql 't))
 */
static int method_documentation_method_combination_t(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_method_combination_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_method_combination_t(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_method_combination_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_method_combination_t_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_method_combination_t);
	MethodTypeDocumentation(&type, MethodCombination, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, METHOD_COMBINATION, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_method_combination_t_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_method_combination_t);
	MethodTypeSetfDocumentation(&type, MethodCombination, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, METHOD_COMBINATION, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (method-combination (eql 'method-combination))
 */
static int documentation_method_combination_method_combination_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_method_combination_t);
	MethodTypeDocumentation(&type, MethodCombination, METHOD_COMBINATION);
	/* method */
	MopArgumentMethodDocumentation(&pos, METHOD_COMBINATION, METHOD_COMBINATION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_method_combination_method_combination_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_method_combination_t);
	MethodTypeSetfDocumentation(&type, MethodCombination, METHOD_COMBINATION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, METHOD_COMBINATION, METHOD_COMBINATION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (symbol (eql 'method-combination))
 */
static int method_documentation_symbol_method_combination(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_method_combination_symbol_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_symbol_method_combination(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_method_combination_symbol_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_symbol_method_combination_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_method_combination);
	MethodTypeDocumentation(&type, Symbol, METHOD_COMBINATION);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, METHOD_COMBINATION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_symbol_method_combination_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_method_combination);
	MethodTypeSetfDocumentation(&type, Symbol, METHOD_COMBINATION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, METHOD_COMBINATION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (standard-method (eql 't))
 */
static int method_documentation_standard_method_t(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_standard_method_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_standard_method_t(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_standard_method_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_standard_method_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_standard_method_t);
	MethodTypeDocumentation(&type, StandardMethod, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, STANDARD_METHOD, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_standard_method_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_standard_method_t);
	MethodTypeSetfDocumentation(&type, StandardMethod, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, STANDARD_METHOD, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (package (eql 't))
 */
static int method_documentation_package_t(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_package_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_package_t(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_package_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_package_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_package_t);
	MethodTypeDocumentation(&type, Package, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, PACKAGE, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_package_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_package_t);
	MethodTypeSetfDocumentation(&type, Package, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, PACKAGE, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (standard-class (eql 't))
 */
static int method_documentation_standard_class_t(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_standard_class_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_standard_class_t(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_standard_class_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_standard_class_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_standard_class_t);
	MethodTypeDocumentation(&type, StandardClass, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, STANDARD_CLASS, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_standard_class_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_standard_class_t);
	MethodTypeSetfDocumentation(&type, StandardClass, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, STANDARD_CLASS, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (standard-class (eql 'type))
 */
static int documentation_standard_class_type_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_standard_class_t);
	MethodTypeDocumentation(&type, StandardClass, TYPE);
	/* method */
	MopArgumentMethodDocumentation(&pos, STANDARD_CLASS, TYPE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_standard_class_type_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_standard_class_t);
	MethodTypeSetfDocumentation(&type, StandardClass, TYPE);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, STANDARD_CLASS, TYPE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (structure-class (eql 't))
 */
static int method_documentation_structure_class_t(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_structure_class_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_structure_class_t(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_structure_class_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_structure_class_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_structure_class_t);
	MethodTypeDocumentation(&type, StructureClass, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, STRUCTURE_CLASS, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_structure_class_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_structure_class_t);
	MethodTypeSetfDocumentation(&type, StructureClass, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, STRUCTURE_CLASS, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (structure-class (eql 'type))
 */
static int documentation_structure_class_type_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_structure_class_t);
	MethodTypeDocumentation(&type, StructureClass, TYPE);
	/* method */
	MopArgumentMethodDocumentation(&pos, STRUCTURE_CLASS, TYPE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_structure_class_type_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_structure_class_t);
	MethodTypeSetfDocumentation(&type, StructureClass, TYPE);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, STRUCTURE_CLASS, TYPE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (symbol (eql 'type))
 */
static int method_documentation_symbol_type(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_type_symbol_get_(ptr, pos, doc_type, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_symbol_type(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_type_symbol_set_(ptr, pos, doc_type, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_symbol_type_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_type);
	MethodTypeDocumentation(&type, Symbol, TYPE);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, TYPE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_symbol_type_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_type);
	MethodTypeSetfDocumentation(&type, Symbol, TYPE);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, TYPE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (symbol (eql 'structure))
 */
static int method_documentation_symbol_structure(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_structure_symbol_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_symbol_structure(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_structure_symbol_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_symbol_structure_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_structure);
	MethodTypeDocumentation(&type, Symbol, STRUCTURE);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, STRUCTURE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_symbol_structure_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_structure);
	MethodTypeSetfDocumentation(&type, Symbol, STRUCTURE);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, STRUCTURE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (symbol (eql 'variable))
 */
static int method_documentation_symbol_variable(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_variable_symbol_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_symbol_variable(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_variable_symbol_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_symbol_variable_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_variable);
	MethodTypeDocumentation(&type, Symbol, VARIABLE);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, VARIABLE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_symbol_variable_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_variable);
	MethodTypeSetfDocumentation(&type, Symbol, VARIABLE);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, VARIABLE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (defgeneric documentation (object doc-type)
 *      (:argument-precedence-order doc-type object)) -> documentation
 */
static int defun_documentation_(Execute ptr)
{
	addr symbol, name, gen, var, order;

	GetConst(COMMON_DOCUMENTATION, &symbol);
	defun_documentation_order(&gen, &var, &order);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	Return(generic_order_(gen, var, order));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(documentation_function_t_(ptr, name, gen));
	Return(documentation_function_function_(ptr, name, gen));
	Return(documentation_list_function_(ptr, name, gen));
	Return(documentation_list_compiler_macro_(ptr, name, gen));
	Return(documentation_symbol_function_(ptr, name, gen));
	Return(documentation_symbol_compiler_macro_(ptr, name, gen));
	Return(documentation_symbol_setf_(ptr, name, gen));
	Return(documentation_method_combination_t_(ptr, name, gen));
	Return(documentation_method_combination_method_combination_(ptr, name, gen));
	Return(documentation_symbol_method_combination_(ptr, name, gen));
	Return(documentation_standard_method_t_(ptr, name, gen));
	Return(documentation_package_t_(ptr, name, gen));
	Return(documentation_standard_class_t_(ptr, name, gen));
	Return(documentation_standard_class_type_(ptr, name, gen));
	Return(documentation_structure_class_t_(ptr, name, gen));
	Return(documentation_structure_class_type_(ptr, name, gen));
	Return(documentation_symbol_type_(ptr, name, gen));
	Return(documentation_symbol_structure_(ptr, name, gen));
	Return(documentation_symbol_variable_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/*
 *  (defgeneric (setf documentation) (value object doc-type) ...)
 *      (:argument-precedence-order doc-type object value)) -> value
 */
static int defun_setf_documentation_(Execute ptr)
{
	addr symbol, name, gen, var, order;

	GetConst(COMMON_DOCUMENTATION, &symbol);
	defun_setf_documentation_order(&gen, &var, &order);
	setf_callname_heap(&name, symbol);
	Return(generic_make_(&gen, name, gen));
	Return(generic_order_(gen, var, order));
	setsetf_symbol(symbol, gen);
	/* method */
	Return(setf_documentation_function_t_(ptr, name, gen));
	Return(setf_documentation_function_function_(ptr, name, gen));
	Return(setf_documentation_list_function_(ptr, name, gen));
	Return(setf_documentation_list_compiler_macro_(ptr, name, gen));
	Return(setf_documentation_symbol_function_(ptr, name, gen));
	Return(setf_documentation_symbol_compiler_macro_(ptr, name, gen));
	Return(setf_documentation_symbol_setf_(ptr, name, gen));
	Return(setf_documentation_method_combination_t_(ptr, name, gen));
	Return(setf_documentation_method_combination_method_combination_(ptr, name, gen));
	Return(setf_documentation_symbol_method_combination_(ptr, name, gen));
	Return(setf_documentation_standard_method_t_(ptr, name, gen));
	Return(setf_documentation_package_t_(ptr, name, gen));
	Return(setf_documentation_standard_class_t_(ptr, name, gen));
	Return(setf_documentation_standard_class_type_(ptr, name, gen));
	Return(setf_documentation_structure_class_t_(ptr, name, gen));
	Return(setf_documentation_structure_class_type_(ptr, name, gen));
	Return(setf_documentation_symbol_type_(ptr, name, gen));
	Return(setf_documentation_symbol_structure_(ptr, name, gen));
	Return(setf_documentation_symbol_variable_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/*
 *  build
 */
void init_documentation(void)
{
	SetPointerType(var4, method_documentation_function_t);
	SetPointerType(var4, method_documentation_list_function);
	SetPointerType(var4, method_documentation_list_compiler_macro);
	SetPointerType(var4, method_documentation_symbol_function);
	SetPointerType(var4, method_documentation_symbol_compiler_macro);
	SetPointerType(var4, method_documentation_symbol_setf);
	SetPointerType(var4, method_documentation_method_combination_t);
	SetPointerType(var4, method_documentation_symbol_method_combination);
	SetPointerType(var4, method_documentation_standard_method_t);
	SetPointerType(var4, method_documentation_package_t);
	SetPointerType(var4, method_documentation_standard_class_t);
	SetPointerType(var4, method_documentation_structure_class_t);
	SetPointerType(var4, method_documentation_symbol_type);
	SetPointerType(var4, method_documentation_symbol_structure);
	SetPointerType(var4, method_documentation_symbol_variable);

	SetPointerType(var5, method_setf_documentation_function_t);
	SetPointerType(var5, method_setf_documentation_list_function);
	SetPointerType(var5, method_setf_documentation_list_compiler_macro);
	SetPointerType(var5, method_setf_documentation_symbol_function);
	SetPointerType(var5, method_setf_documentation_symbol_compiler_macro);
	SetPointerType(var5, method_setf_documentation_symbol_setf);
	SetPointerType(var5, method_setf_documentation_method_combination_t);
	SetPointerType(var5, method_setf_documentation_symbol_method_combination);
	SetPointerType(var5, method_setf_documentation_standard_method_t);
	SetPointerType(var5, method_setf_documentation_package_t);
	SetPointerType(var5, method_setf_documentation_standard_class_t);
	SetPointerType(var5, method_setf_documentation_structure_class_t);
	SetPointerType(var5, method_setf_documentation_symbol_type);
	SetPointerType(var5, method_setf_documentation_symbol_structure);
	SetPointerType(var5, method_setf_documentation_symbol_variable);
}

void build_documentation(Execute ptr)
{
	Error(defun_documentation_(ptr));
	Error(defun_setf_documentation_(ptr));
}


/************************************************************
 *  document_call.c
 ************************************************************/

/*
 *  function, t
 */
static int document_function_name_(addr pos, addr *ret)
{
	if (functionp(pos)) {
		GetNameFunction(pos, ret);
		return 0;
	}

	return stdget_generic_name_(pos, ret);
}

int document_function_get_(addr pos, addr *ret)
{
	addr value;

	Return(get_documentation_function_object_(pos, &value));
	if (value != Nil)
		return Result(ret, value);

	/* contents */
	Return(document_function_name_(pos, &pos));
	if (callnamep(pos))
		GetCallName(pos, &pos);
	if (pos == Nil)
		return Result(ret, Nil);

	return document_function_(pos, ret);
}

int document_function_set_(addr pos, addr value)
{
	return set_documentation_function_object_(pos, value);
}


/*
 *  (setf name), function
 */
int document_function_setf_get_(addr pos, addr *ret)
{
	addr value;

	Return(parse_callname_error_(&pos, pos));
	Return(getglobalcheck_callname_(pos, &value));
	Return(get_documentation_function_object_(value, &value));
	if (value != Nil)
		return Result(ret, value);

	/* contents */
	GetCallName(pos, &pos);
	return document_function_(pos, ret);
}

int document_function_setf_set_(addr pos, addr value)
{
	Return(parse_callname_error_(&pos, pos));
	Return(getglobalcheck_callname_(pos, &pos));
	Return(set_documentation_function_object_(pos, value));

	return 0;
}


/*
 *  symbol, function
 */
static int document_fdefinition_symbol_get_(addr pos, addr *ret)
{
	/* function */
	GetFunctionSymbol(pos, ret);
	if (*ret != Unbound)
		return 0;

	/* macro-function */
	getmacro_symbol(pos, ret);
	if (*ret != Unbound)
		return 0;

	/* error */
	return call_undefined_function_(NULL, pos);
}

int document_function_symbol_get_(addr pos, addr *ret)
{
	addr value;

	Return(document_fdefinition_symbol_get_(pos, &value));
	Return(get_documentation_function_object_(value, &value));
	if (value != Nil)
		return Result(ret, value);

	/* contents */
	return document_function_(pos, ret);
}

int document_function_symbol_set_(addr pos, addr value)
{
	Return(getfunction_global_(pos, &pos));
	Return(set_documentation_function_object_(pos, value));

	return 0;
}


/*
 *  symbol, compiler-macro
 */
int document_compiler_macro_symbol_get_(addr pos, addr *ret)
{
	get_compiler_macro_symbol(pos, &pos);
	if (pos != Nil) {
		Return(get_documentation_function_object_(pos, &pos));
	}

	return Result(ret, pos);
}

int document_compiler_macro_symbol_set_(addr pos, addr value)
{
	addr macro;

	get_compiler_macro_symbol(pos, &macro);
	if (macro == Nil)
		return fmte_("There is no compiler-macro, ~S.", pos, NULL);
	Return(set_documentation_function_object_(macro, value));

	return 0;
}


/*
 *  (setf name), compiler-macro
 */
int document_compiler_macro_setf_get_(addr pos, addr *ret)
{
	Return(parse_callname_error_(&pos, pos));
	GetCallName(pos, &pos);
	get_setf_compiler_macro_symbol(pos, &pos);
	if (pos != Nil) {
		Return(get_documentation_function_object_(pos, &pos));
	}

	return Result(ret, pos);
}

int document_compiler_macro_setf_set_(addr pos, addr value)
{
	addr macro;

	Return(parse_callname_error_(&pos, pos));
	GetCallName(pos, &pos);
	get_setf_compiler_macro_symbol(pos, &macro);
	if (macro == Nil)
		return fmte_("There is no compiler-macro, ~S.", pos, NULL);
	Return(set_documentation_function_object_(macro, value));

	return 0;
}


/*
 *  symbol, setf
 */
int document_defsetf_symbol_get_(addr pos, addr *ret)
{
	addr setf;

	/* define-setf-expander, defsetf */
	getsetfmacro_symbol(pos, &setf);
	/* setf-function */
	if (setf == Unbound) {
		Return(getsetf_global_(pos, &setf));
	}
	/* get documentation */
	Return(get_documentation_function_object_(setf, &setf));

	/* contents */
	if (setf != Nil)
		return Result(ret, setf);

	return document_function_(pos, ret);
}

int document_defsetf_symbol_set_(addr pos, addr value)
{
	addr setf;

	/* define-setf-expander, defsetf */
	getsetfmacro_symbol(pos, &setf);
	/* setf-function */
	if (setf == Unbound) {
		Return(getsetf_global_(pos, &setf));
	}
	/* set documentation */
	Return(set_documentation_function_object_(setf, value));

	return 0;
}


/*
 *  method-combination
 */
int document_method_combination_get_(addr pos, addr *ret)
{
	return stdget_longcomb_document_(pos, ret);
}

int document_method_combination_set_(addr pos, addr value)
{
	return stdset_longcomb_document_(pos, value);
}


/*
 *  symbol, method-combination
 */
int document_method_combination_symbol_get_(addr pos, addr *ret)
{
	Return(clos_find_combination_(pos, &pos));
	Return(stdget_longcomb_document_(pos, ret));

	return 0;
}

int document_method_combination_symbol_set_(addr pos, addr value)
{
	Return(clos_find_combination_(pos, &pos));
	Return(stdset_longcomb_document_(pos, value));

	return 0;
}


/*
 *  standard-method
 */
int document_standard_method_get_(addr pos, addr *ret)
{
	return methodget_document_(pos, ret);
}

int document_standard_method_set_(addr pos, addr value)
{
	return methodset_document_(pos, value);
}


/*
 *  package
 */
int document_package_get_(addr pos, addr *ret)
{
	getdocument_package(pos, ret);
	return 0;
}

int document_package_set_(addr pos, addr value)
{
	setdocument_package(pos, value);
	return 0;
}


/*
 *  standard-class
 */
int document_standard_class_get_(addr pos, addr *ret)
{
	addr value;

	Return(stdget_class_document_(pos, &value));
	if (value != Nil)
		return Result(ret, value);

	/* contents */
	Return(stdget_class_name_(pos, &pos));
	return document_type_(pos, ret);
}

int document_standard_class_set_(addr pos, addr value)
{
	return stdset_class_document_(pos, value);
}


/*
 *  structure-class
 */
int document_structure_class_get_(addr pos, addr *ret)
{
	addr value;

	Return(stdget_structure_documentation_(pos, &value));
	if (value != Nil)
		return Result(ret, value);

	/* contents */
	Return(stdget_structure_name_(pos, &pos));
	return document_type_(pos, ret);
}

int document_structure_class_set_(addr pos, addr value)
{
	return stdset_structure_documentation_(pos, value);
}


/*
 *  symbol, type
 */
int document_type_symbol_get_(Execute ptr, addr pos, addr doc_type, addr *ret)
{
	addr clos, type;

	/* clos */
	clos_find_class_nil(pos, &clos);
	if (clos != Nil) {
		GetConst(COMMON_DOCUMENTATION, &type);
		Return(document_fdefinition_symbol_get_(type, &type));
		return funcall1_control_(ptr, ret, type, clos, doc_type, NULL);
	}

	/* deftype */
	getdeftype(pos, &type);
	if (type == Nil) {
		*ret = Nil;
		return fmte_("The symbol ~S don't have a deftype function.", pos, NULL);
	}
	Return(get_documentation_function_object_(type, &type));

	/* contents */
	if (type != Nil)
		return Result(ret, type);

	return document_type_(pos, ret);
}

int document_type_symbol_set_(Execute ptr, addr pos, addr doc_type, addr value)
{
	addr clos, type;

	/* clos */
	clos_find_class_nil(pos, &clos);
	if (clos != Nil) {
		GetConst(COMMON_DOCUMENTATION, &type);
		Return(getsetf_global_(type, &type));
		return funcall_control_(ptr, type, value, clos, doc_type, NULL);
	}

	/* deftype */
	getdeftype(pos, &type);
	if (type == Nil)
		return fmte_("The symbol ~S don't have a deftype function.", pos, NULL);
	Return(set_documentation_function_object_(type, value));

	return 0;
}


/*
 *  symbol, structure
 */
int document_structure_symbol_get_(addr pos, addr *ret)
{
	addr value;

	Return(getdoc_structure_(pos, &value));
	if (value != Nil)
		return Result(ret, value);

	/* contents */
	return document_type_(pos, ret);
}

int document_structure_symbol_set_(addr pos, addr value)
{
	return setdoc_structure_(pos, value);
}


/*
 *  symbol, variable
 */
int document_variable_symbol_get_(addr pos, addr *ret)
{
	addr value;

	getdocument_variable_symbol(pos, &value);
	if (value != Nil)
		return Result(ret, value);

	/* contents */
	return document_variable_(pos, ret);
}

int document_variable_symbol_set_(addr pos, addr value)
{
	setdocument_variable_symbol(pos, value);
	return 0;
}


/************************************************************
 *  document_contents.c
 ************************************************************/

/*
 *  FUNCTION
 */
#ifdef LISP_DOCUMENTATION
static const char Document_FUNCTION_COMMON_LISP_CONS[] =
    "Syntax: cons object-1 object-2 => cons" "\n"
    "Creates a fresh cons, the car of which is object-1 and the cdr of which is object-2." "\n"
    ;

static const char Document_FUNCTION_COMMON_LISP_COPY_LIST[] =
    "Syntax: copy-list list => copy" "\n"
    "Returns a copy of list. If list is a dotted list, the resulting list will also be a dotted list." "\n"
    "Only the list structure of list is copied; the elements of the resulting list are the same as the corresponding elements of the given list." "\n"
    ;

static const char Document_FUNCTION_COMMON_LISP_FUNCTION_KEYWORDS[] =
    "Syntax: function-keywords method => keys, allow-other-keys-p" "\n"
    "Method Signatures: function-keywords (method standard-method)" "\n"
    "Returns the keyword parameter specifiers for a method." "\n"
    "Two values are returned: a list of the explicitly named keywords and a generalized boolean that states whether &allow-other-keys had been specified in the method definition." "\n"
    ;

static const char Document_FUNCTION_COMMON_LISP_LIST[] =
    "Syntax: list &rest objects => list" "\n"
    "Syntax: list* &rest objects+ => result" "\n"
    "list returns a list containing the supplied objects." "\n"
    "list* is like list except that the last argument to list becomes the car of the last cons constructed, while the last argument to list* becomes the cdr of the last cons constructed. Hence, any given call to list* always produces one fewer conses than a call to list with the same number of arguments." "\n"
    "If the last argument to list* is a list, the effect is to construct a new list which is similar, but which has additional elements added to the front corresponding to the preceding arguments of list*." "\n"
    "If list* receives only one object, that object is returned, regardless of whether or not it is a list." "\n"
    ;

static const char Document_FUNCTION_COMMON_LISP_NTH[] =
    "Syntax: nth n list => object" "\n"
    "Syntax: (setf (nth n list) new-object)" "\n"
    "nth locates the nth element of list, where the car of the list is the ``zeroth'' element. Specifically," "\n"
    "(nth n list) ==  (car (nthcdr n list))" "\n"
    "nth may be used to specify a place to setf. Specifically," "\n"
    "(setf (nth n list) new-object) ==  (setf (car (nthcdr n list)) new-object)" "\n"
    ;

static const char Document_FUNCTION_COMMON_LISP_POP[] =
    "Syntax: pop place => element" "\n"
    "pop reads the value of place, remembers the car of the list which was retrieved, writes the cdr of the list back into the place, and finally yields the car of the originally retrieved list." "\n"
    ;

static const char Document_FUNCTION_COMMON_LISP_PUSH[] =
    "Syntax: push item place => new-place-value" "\n"
    "push prepends item to the list that is stored in place, stores the resulting list in place, and returns the list." "\n"
    ;

static const char Document_FUNCTION_COMMON_LISP_TREE_EQUAL[] =
    "Syntax: tree-equal tree-1 tree-2 &key test test-not => generalized-boolean" "\n"
    "tree-equal tests whether two trees are of the same shape and have the same leaves. tree-equal returns true if tree-1 and tree-2 are both atoms and satisfy the test, or if they are both conses and the car of tree-1 is tree-equal to the car of tree-2 and the cdr of tree-1 is tree-equal to the cdr of tree-2. Otherwise, tree-equal returns false." "\n"
    "tree-equal recursively compares conses but not any other objects that have components." "\n"
    "The first argument to the :test or :test-not function is tree-1 or a car or cdr of tree-1; the second argument is tree-2 or a car or cdr of tree-2." "\n"
    ;

static const char Document_FUNCTION_LISP_SYSTEM_DLCALL[] =
    "Dynamic Link Call.";

static const char Document_FUNCTION_LISP_SYSTEM_DLFILE[] =
    "Dynamic Link Control.";

static struct DocumentStruct Document_FUNCTION_COMMON_LISP[] = {
    { "CONS", Document_FUNCTION_COMMON_LISP_CONS },
    { "COPY-LIST", Document_FUNCTION_COMMON_LISP_COPY_LIST },
    { "FUNCTION-KEYWORDS", Document_FUNCTION_COMMON_LISP_FUNCTION_KEYWORDS },
    { "LIST", Document_FUNCTION_COMMON_LISP_LIST },
    { "LIST*", Document_FUNCTION_COMMON_LISP_LIST },
    { "NTH", Document_FUNCTION_COMMON_LISP_NTH },
    { "POP", Document_FUNCTION_COMMON_LISP_POP },
    { "PUSH", Document_FUNCTION_COMMON_LISP_PUSH },
    { "TREE-EQUAL", Document_FUNCTION_COMMON_LISP_TREE_EQUAL },
    { NULL, NULL }
};

#define DocumentSize_FUNCTION_COMMON_LISP 9

static struct DocumentStruct Document_FUNCTION_LISP_SYSTEM[] = {
    { "DLCALL", Document_FUNCTION_LISP_SYSTEM_DLCALL },
    { "DLFILE", Document_FUNCTION_LISP_SYSTEM_DLFILE },
    { NULL, NULL }
};

#define DocumentSize_FUNCTION_LISP_SYSTEM 2

struct DocumentPackage Document_FUNCTION[] = {
    { LISP_COMMON, Document_FUNCTION_COMMON_LISP, DocumentSize_FUNCTION_COMMON_LISP },
    { LISP_SYSTEM, Document_FUNCTION_LISP_SYSTEM, DocumentSize_FUNCTION_LISP_SYSTEM },
    { NULL, NULL, 0 }
};
#else
struct DocumentPackage Document_FUNCTION[] = {
    { NULL, NULL, 0 }
};
#endif


/*
 *  VARIABLE
 */
#ifdef LISP_DOCUMENTATION
static const char Document_VARIABLE_COMMON_LISP_CALL_ARGUMENTS_LIMIT[] =
    "Constant Value: An integer not smaller than 50 and at least as great as the value of lambda-parameters-limit, the exact magnitude of which is implementation-dependent." "\n"
    "Description: The upper exclusive bound on the number of arguments that may be passed to a function." "\n"
    ;

static const char Document_VARIABLE_COMMON_LISP_LAMBDA_LIST_KEYWORDS[] =
    "Constant Value: a list, the elements of which are implementation-dependent, but which must contain at least the symbols &allow-other-keys, &aux, &body, &environment, &key, &optional, &rest, and &whole." "\n"
    "Description: A list of all the lambda list keywords used in the implementation, including the additional ones used only by macro definition forms." "\n"
    ;

static const char Document_VARIABLE_COMMON_LISP_LAMBDA_PARAMETERS_LIMIT[] =
    "Constant Value: implementation-dependent, but not smaller than 50." "\n"
    "Description: A positive integer that is the upper exclusive bound on the number of parameter names that can appear in a single lambda list." "\n"
    ;

static struct DocumentStruct Document_VARIABLE_COMMON_LISP[] = {
    { "CALL-ARGUMENTS-LIMIT", Document_VARIABLE_COMMON_LISP_CALL_ARGUMENTS_LIMIT },
    { "LAMBDA-LIST-KEYWORDS", Document_VARIABLE_COMMON_LISP_LAMBDA_LIST_KEYWORDS },
    { "LAMBDA-PARAMETERS-LIMIT", Document_VARIABLE_COMMON_LISP_LAMBDA_PARAMETERS_LIMIT },
    { NULL, NULL }
};

#define DocumentSize_VARIABLE_COMMON_LISP 3

struct DocumentPackage Document_VARIABLE[] = {
    { LISP_COMMON, Document_VARIABLE_COMMON_LISP, DocumentSize_VARIABLE_COMMON_LISP },
    { NULL, NULL, 0 }
};
#else
struct DocumentPackage Document_VARIABLE[] = {
    { NULL, NULL, 0 }
};
#endif


/*
 *  TYPE
 */
#ifdef LISP_DOCUMENTATION
static const char Document_TYPE_COMMON_LISP_ATOM[] =
    "Supertypes: atom, t" "\n"
    "It is equivalent to (not cons)." "\n"
    ;

static const char Document_TYPE_COMMON_LISP_CONS[] =
    "Class Precedence List cons, list, sequence, t" "\n"
    "A cons is a compound object having two components, called the car and cdr. These form a dotted pair. Each component can be any object." "\n"
    ;

static const char Document_TYPE_COMMON_LISP_LIST[] =
    "Class Precedence List: list, sequence, t" "\n"
    "A list is a chain of conses in which the car of each cons is an element of the list, and the cdr of each cons is either the next link in the chain or a terminating atom." "\n"
    "A proper list is a chain of conses terminated by the empty list, (), which is itself a proper list. A dotted list is a list which has a terminating atom that is not the empty list. A circular list is a chain of conses that has no termination because some cons in the chain is the cdr of a later cons." "\n"
    "Dotted lists and circular lists are also lists, but usually the unqualified term ``list'' within this specification means proper list. Nevertheless, the type list unambiguously includes dotted lists and circular lists." "\n"
    "For each element of a list there is a cons. The empty list has no elements and is not a cons." "\n"
    "The types cons and null form an exhaustive partition of the type list." "\n"
    ;

static const char Document_TYPE_COMMON_LISP_NULL[] =
    "Class Precedence List: null, symbol, list, sequence, t" "\n"
    "The only object of type null is nil, which represents the empty list and can also be notated ()." "\n"
    ;

static const char Document_TYPE_COMMON_LISP_STANDARD_CLASS[] =
    "Class Precedence List: standard-class, class, standard-object, t" "\n"
    "The class standard-class is the default class of classes defined by defclass." "\n"
    ;

static struct DocumentStruct Document_TYPE_COMMON_LISP[] = {
    { "ATOM", Document_TYPE_COMMON_LISP_ATOM },
    { "CONS", Document_TYPE_COMMON_LISP_CONS },
    { "LIST", Document_TYPE_COMMON_LISP_LIST },
    { "NULL", Document_TYPE_COMMON_LISP_NULL },
    { "STANDARD-CLASS", Document_TYPE_COMMON_LISP_STANDARD_CLASS },
    { NULL, NULL }
};

#define DocumentSize_TYPE_COMMON_LISP 5

struct DocumentPackage Document_TYPE[] = {
    { LISP_COMMON, Document_TYPE_COMMON_LISP, DocumentSize_TYPE_COMMON_LISP },
    { NULL, NULL, 0 }
};
#else
struct DocumentPackage Document_TYPE[] = {
    { NULL, NULL, 0 }
};
#endif



/************************************************************
 *  document_search.c
 ************************************************************/

static int document_search_equal_char_(addr pos, const char *str, int *ret)
{
	unicode x, y;
	size_t size, i;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &x));
		y = (unicode)str[i];
		if (y == 0)
			return Result(ret, 0);
		if (x != y)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int document_search_compare_char_(addr left, const char *right, int *ret)
{
	unicode x, y;
	size_t size, i;

	string_length(left, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(left, i, &x));
		y = (unicode)right[i];
		if (y == 0)
			return Result(ret, 1);
		if (x < y)
			return Result(ret, -1);
		if (x > y)
			return Result(ret, 1);
	}

	return Result(ret, right[i]? -1: 0);
}

static int document_search_result_(struct DocumentStruct *a, addr *ret)
{
	return string8_null_heap_(ret, a->value);
}

static int document_search_range_(
		addr key, size_t ai, size_t bi,
		struct DocumentStruct *root,
		addr *ret)
{
	int check;
	struct DocumentStruct *a, *b, *c;
	size_t ci;

	ci = (ai + bi) / 2UL;
	a = root + ai;
	b = root + bi;
	c = root + ci;
	/* a */
	Return(document_search_equal_char_(key, a->key, &check));
	if (check)
		return document_search_result_(a, ret);
	/* b */
	Return(document_search_equal_char_(key, b->key, &check));
	if (check)
		return document_search_result_(b, ret);
	/* range */
	if (bi <= ai)
		return Result(ret, Nil);
	/* compare */
	Return(document_search_compare_char_(key, c->key, &check));
	if (check < 0)
		return document_search_range_(key, ai + 1UL, ci, root, ret);
	else
		return document_search_range_(key, ci, bi - 1UL, root, ret);
}

static int document_search_symbol_(struct DocumentPackage *str, addr name, addr *ret)
{
	struct DocumentStruct *root;
	size_t x, y;

	x = 0;
	y = str->size;
	root = str->list;
	if (y == 0)
		return Result(ret, Nil);
	else
		return document_search_range_(name, x, y - 1UL, root, ret);
}

static int document_search_package_(addr pos,
		struct DocumentPackage *root,
		struct DocumentPackage **ret)
{
	int check;
	struct DocumentPackage *str;
	size_t i;

	Return(getname_package_(pos, &pos));
	for (i = 0; ; i++) {
		str = root + i;
		if (str->package == NULL)
			break;
		Return(document_search_equal_char_(pos, str->package, &check));
		if (check)
			return Result(ret, str);
	}

	return Result(ret, NULL);
}

static int document_search_(addr pos, addr *ret, struct DocumentPackage *root)
{
	addr pg;
	struct DocumentPackage *str;

	/* symbol, package */
	if (! symbolp(pos))
		return Result(ret, Nil);
	GetPackageSymbol(pos, &pg);
	GetNameSymbol(pos, &pos);
	if (pg == Nil)
		return Result(ret, Nil);

	Return(document_search_package_(pg, root, &str));
	if (str == NULL)
		return Result(ret, Nil);

	return document_search_symbol_(str, pos, ret);
}

int document_function_(addr pos, addr *ret)
{
	return document_search_(pos, ret, Document_FUNCTION);
}

int document_variable_(addr pos, addr *ret)
{
	return document_search_(pos, ret, Document_VARIABLE);
}

int document_type_(addr pos, addr *ret)
{
	return document_search_(pos, ret, Document_TYPE);
}


/************************************************************
 *  eastasian.c
 ************************************************************/

/*
 *  length
 */
int eastasian_length_(addr pos, size_t *ret, int *rerrp)
{
	int errorp;
	unicode c;
	unsigned v;
	size_t size, i, count;

	string_length(pos, &size);
	errorp = 0;
	count = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (UnicodeCount <= c) {
			errorp = 1;
			continue;
		}
		v = eastasian_width(c);
		if (v == 0) {
			errorp = 1;
			continue;
		}
		count += (size_t)v;
	}
	*ret = count;
	if (rerrp)
		*rerrp = errorp;

	return 0;
}


/*
 *  syscall
 */
static int eastasian_type_(addr pos, enum EastAsianType *ret)
{
	int check;

	if (characterp(pos)) {
		if (character_equalp_unicode(pos, 'N'))
			return Result(ret, EastAsian_N);
		if (character_equalp_unicode(pos, 'A'))
			return Result(ret, EastAsian_A);
		if (character_equalp_unicode(pos, 'H'))
			return Result(ret, EastAsian_H);
		if (character_equalp_unicode(pos, 'W'))
			return Result(ret, EastAsian_W);
		if (character_equalp_unicode(pos, 'F'))
			return Result(ret, EastAsian_F);
		return Result(ret, EastAsian_error);
	}
	if (symbolp(pos))
		GetNameSymbol(pos, &pos);
	if (stringp(pos)) {
		Return(string_equalp_char_(pos, "N", &check));
		if (check)
			return Result(ret, EastAsian_N);
		Return(string_equalp_char_(pos, "A", &check));
		if (check)
			return Result(ret, EastAsian_A);
		Return(string_equalp_char_(pos, "H", &check));
		if (check)
			return Result(ret, EastAsian_H);
		Return(string_equalp_char_(pos, "W", &check));
		if (check)
			return Result(ret, EastAsian_W);
		Return(string_equalp_char_(pos, "F", &check));
		if (check)
			return Result(ret, EastAsian_F);
		Return(string_equalp_char_(pos, "NA", &check));
		if (check)
			return Result(ret, EastAsian_NA);

		return Result(ret, EastAsian_error);
	}

	return Result(ret, EastAsian_error);
}

int eastasian_set_syscall_(addr pos, addr value, addr errorp, addr *ret)
{
	enum EastAsianType type;
	size_t size;

	/* value */
	if (GetIndex_fixnum(value, &size)) {
		if (errorp)
			return fmte_("Invalid integer value ~S.", value, NULL);
		else
			return Result(ret, Nil);
	}
	if (UINT_MAX <= size) {
		if (errorp)
			return fmte_("The value ~S is too large.", value, NULL);
		else
			return Result(ret, Nil);
	}

	/* set */
	Return(eastasian_type_(pos, &type));
	switch (type) {
		case EastAsian_N:
		case EastAsian_A:
		case EastAsian_H:
		case EastAsian_W:
		case EastAsian_F:
		case EastAsian_NA:
			EastAsianSymbol[type] = (unsigned)size;
			break;

		default:
			if (errorp)
				return fmte_("Inavlid eastasian type ~S.", pos, NULL);
			else
				return Result(ret, Nil);
	}

	return Result(ret, T);
}

static void eastasian_system_symbol(enum EastAsianType type, addr *ret)
{
	constindex index;

	switch (type) {
		case EastAsian_N: index = CONSTANT_SYSTEM_N; break;
		case EastAsian_A: index = CONSTANT_SYSTEM_A; break;
		case EastAsian_H: index = CONSTANT_SYSTEM_H; break;
		case EastAsian_W: index = CONSTANT_SYSTEM_W; break;
		case EastAsian_F: index = CONSTANT_SYSTEM_F; break;
		case EastAsian_NA: index = CONSTANT_SYSTEM_NA; break;
		default: index = CONSTANT_EMPTY; break;
	}
	if (index == CONSTANT_EMPTY)
		*ret = Nil;
	else
		GetConstant(index, ret);
}

int eastasian_get_syscall_(addr pos, addr *retsize, addr *retsymbol)
{
	enum EastAsianType type;

	Return(eastasian_type_(pos, &type));
	eastasian_system_symbol(type, retsymbol);
	if (*retsymbol == Nil)
		fixnum_heap(retsize, 0);
	else
		fixnum_heap(retsize, (fixnum)EastAsianSymbol[type]);

	return 0;
}

static void eastasian_width_character(addr pos, addr *ret, addr *retbool)
{
	unicode c;
	unsigned v;

	GetCharacter(pos, &c);
	if (UnicodeCount <= c)
		goto error;
	v = eastasian_width(c);
	if (v == 0)
		goto error;
	fixnum_heap(ret, (fixnum)v);
	*retbool = T;
	return;

error:
	fixnum_heap(ret, 0);
	*retbool = Nil;
}

static void eastasian_width_integer(addr pos, addr *ret, addr *retbool)
{
	unsigned v;
	size_t size;

	if (GetIndex_integer(pos, &size))
		goto error;
	if (UnicodeCount <= size)
		goto error;
	v = eastasian_width((unicode)size);
	if (v == 0)
		goto error;
	fixnum_heap(ret, (fixnum)v);
	*retbool = T;
	return;

error:
	fixnum_heap(ret, 0);
	*retbool = Nil;
}

static int eastasian_width_string_(addr pos, addr *ret, addr *retbool)
{
	int check;
	size_t size;

	Return(eastasian_length_(pos, &size, &check));
	make_index_integer_heap(ret, size);
	*retbool = check? Nil: T;

	return 0;
}

int eastasian_width_syscall_(addr pos, addr *ret, addr *retbool)
{
	if (characterp(pos)) {
		eastasian_width_character(pos, ret, retbool);
		return 0;
	}
	if (integerp(pos)) {
		eastasian_width_integer(pos, ret, retbool);
		return 0;
	}
	if (stringp(pos)) {
		return eastasian_width_string_(pos, ret, retbool);
	}

	/* error */
	fixnum_heap(ret, 0);
	*retbool = Nil;

	return 0;
}


/************************************************************
 *  eastasian_table.c
 ************************************************************/
/*  Auto generated by mk.eastasian.lisp
 *
 *  Unicode Consortium
 *    http://www.unicode.org/
 *
 *  Unicode Copyright and Terms of Use
 *    http://www.unicode.org/copyright.html
 *    http://www.unicode.org/license.html
 *
 *  East Asian Width
 *    http://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt
 */

unsigned EastAsianSymbol[EastAsian_Size];
const struct eastasian_struct EastAsianTable[EastAsianTable_Size] = {
    {  0x000000,  0x00001F,  EastAsian_N   },
    {  0x000020,  0x00007E,  EastAsian_NA  },
    {  0x00007F,  0x0000A0,  EastAsian_N   },
    {  0x0000A1,  0x0000A1,  EastAsian_A   },
    {  0x0000A2,  0x0000A3,  EastAsian_NA  },
    {  0x0000A4,  0x0000A4,  EastAsian_A   },
    {  0x0000A5,  0x0000A6,  EastAsian_NA  },
    {  0x0000A7,  0x0000A8,  EastAsian_A   },
    {  0x0000A9,  0x0000A9,  EastAsian_N   },
    {  0x0000AA,  0x0000AA,  EastAsian_A   },
    {  0x0000AB,  0x0000AB,  EastAsian_N   },
    {  0x0000AC,  0x0000AC,  EastAsian_NA  },
    {  0x0000AD,  0x0000AE,  EastAsian_A   },
    {  0x0000AF,  0x0000AF,  EastAsian_NA  },
    {  0x0000B0,  0x0000B4,  EastAsian_A   },
    {  0x0000B5,  0x0000B5,  EastAsian_N   },
    {  0x0000B6,  0x0000BA,  EastAsian_A   },
    {  0x0000BB,  0x0000BB,  EastAsian_N   },
    {  0x0000BC,  0x0000BF,  EastAsian_A   },
    {  0x0000C0,  0x0000C5,  EastAsian_N   },
    {  0x0000C6,  0x0000C6,  EastAsian_A   },
    {  0x0000C7,  0x0000CF,  EastAsian_N   },
    {  0x0000D0,  0x0000D0,  EastAsian_A   },
    {  0x0000D1,  0x0000D6,  EastAsian_N   },
    {  0x0000D7,  0x0000D8,  EastAsian_A   },
    {  0x0000D9,  0x0000DD,  EastAsian_N   },
    {  0x0000DE,  0x0000E1,  EastAsian_A   },
    {  0x0000E2,  0x0000E5,  EastAsian_N   },
    {  0x0000E6,  0x0000E6,  EastAsian_A   },
    {  0x0000E7,  0x0000E7,  EastAsian_N   },
    {  0x0000E8,  0x0000EA,  EastAsian_A   },
    {  0x0000EB,  0x0000EB,  EastAsian_N   },
    {  0x0000EC,  0x0000ED,  EastAsian_A   },
    {  0x0000EE,  0x0000EF,  EastAsian_N   },
    {  0x0000F0,  0x0000F0,  EastAsian_A   },
    {  0x0000F1,  0x0000F1,  EastAsian_N   },
    {  0x0000F2,  0x0000F3,  EastAsian_A   },
    {  0x0000F4,  0x0000F6,  EastAsian_N   },
    {  0x0000F7,  0x0000FA,  EastAsian_A   },
    {  0x0000FB,  0x0000FB,  EastAsian_N   },
    {  0x0000FC,  0x0000FC,  EastAsian_A   },
    {  0x0000FD,  0x0000FD,  EastAsian_N   },
    {  0x0000FE,  0x0000FE,  EastAsian_A   },
    {  0x0000FF,  0x000100,  EastAsian_N   },
    {  0x000101,  0x000101,  EastAsian_A   },
    {  0x000102,  0x000110,  EastAsian_N   },
    {  0x000111,  0x000111,  EastAsian_A   },
    {  0x000112,  0x000112,  EastAsian_N   },
    {  0x000113,  0x000113,  EastAsian_A   },
    {  0x000114,  0x00011A,  EastAsian_N   },
    {  0x00011B,  0x00011B,  EastAsian_A   },
    {  0x00011C,  0x000125,  EastAsian_N   },
    {  0x000126,  0x000127,  EastAsian_A   },
    {  0x000128,  0x00012A,  EastAsian_N   },
    {  0x00012B,  0x00012B,  EastAsian_A   },
    {  0x00012C,  0x000130,  EastAsian_N   },
    {  0x000131,  0x000133,  EastAsian_A   },
    {  0x000134,  0x000137,  EastAsian_N   },
    {  0x000138,  0x000138,  EastAsian_A   },
    {  0x000139,  0x00013E,  EastAsian_N   },
    {  0x00013F,  0x000142,  EastAsian_A   },
    {  0x000143,  0x000143,  EastAsian_N   },
    {  0x000144,  0x000144,  EastAsian_A   },
    {  0x000145,  0x000147,  EastAsian_N   },
    {  0x000148,  0x00014B,  EastAsian_A   },
    {  0x00014C,  0x00014C,  EastAsian_N   },
    {  0x00014D,  0x00014D,  EastAsian_A   },
    {  0x00014E,  0x000151,  EastAsian_N   },
    {  0x000152,  0x000153,  EastAsian_A   },
    {  0x000154,  0x000165,  EastAsian_N   },
    {  0x000166,  0x000167,  EastAsian_A   },
    {  0x000168,  0x00016A,  EastAsian_N   },
    {  0x00016B,  0x00016B,  EastAsian_A   },
    {  0x00016C,  0x0001CD,  EastAsian_N   },
    {  0x0001CE,  0x0001CE,  EastAsian_A   },
    {  0x0001CF,  0x0001CF,  EastAsian_N   },
    {  0x0001D0,  0x0001D0,  EastAsian_A   },
    {  0x0001D1,  0x0001D1,  EastAsian_N   },
    {  0x0001D2,  0x0001D2,  EastAsian_A   },
    {  0x0001D3,  0x0001D3,  EastAsian_N   },
    {  0x0001D4,  0x0001D4,  EastAsian_A   },
    {  0x0001D5,  0x0001D5,  EastAsian_N   },
    {  0x0001D6,  0x0001D6,  EastAsian_A   },
    {  0x0001D7,  0x0001D7,  EastAsian_N   },
    {  0x0001D8,  0x0001D8,  EastAsian_A   },
    {  0x0001D9,  0x0001D9,  EastAsian_N   },
    {  0x0001DA,  0x0001DA,  EastAsian_A   },
    {  0x0001DB,  0x0001DB,  EastAsian_N   },
    {  0x0001DC,  0x0001DC,  EastAsian_A   },
    {  0x0001DD,  0x000250,  EastAsian_N   },
    {  0x000251,  0x000251,  EastAsian_A   },
    {  0x000252,  0x000260,  EastAsian_N   },
    {  0x000261,  0x000261,  EastAsian_A   },
    {  0x000262,  0x0002C3,  EastAsian_N   },
    {  0x0002C4,  0x0002C4,  EastAsian_A   },
    {  0x0002C5,  0x0002C6,  EastAsian_N   },
    {  0x0002C7,  0x0002C7,  EastAsian_A   },
    {  0x0002C8,  0x0002C8,  EastAsian_N   },
    {  0x0002C9,  0x0002CB,  EastAsian_A   },
    {  0x0002CC,  0x0002CC,  EastAsian_N   },
    {  0x0002CD,  0x0002CD,  EastAsian_A   },
    {  0x0002CE,  0x0002CF,  EastAsian_N   },
    {  0x0002D0,  0x0002D0,  EastAsian_A   },
    {  0x0002D1,  0x0002D7,  EastAsian_N   },
    {  0x0002D8,  0x0002DB,  EastAsian_A   },
    {  0x0002DC,  0x0002DC,  EastAsian_N   },
    {  0x0002DD,  0x0002DD,  EastAsian_A   },
    {  0x0002DE,  0x0002DE,  EastAsian_N   },
    {  0x0002DF,  0x0002DF,  EastAsian_A   },
    {  0x0002E0,  0x0002FF,  EastAsian_N   },
    {  0x000300,  0x00036F,  EastAsian_A   },
    {  0x000370,  0x000377,  EastAsian_N   },
    {  0x00037A,  0x00037F,  EastAsian_N   },
    {  0x000384,  0x00038A,  EastAsian_N   },
    {  0x00038C,  0x00038C,  EastAsian_N   },
    {  0x00038E,  0x000390,  EastAsian_N   },
    {  0x000391,  0x0003A1,  EastAsian_A   },
    {  0x0003A3,  0x0003A9,  EastAsian_A   },
    {  0x0003AA,  0x0003B0,  EastAsian_N   },
    {  0x0003B1,  0x0003C1,  EastAsian_A   },
    {  0x0003C2,  0x0003C2,  EastAsian_N   },
    {  0x0003C3,  0x0003C9,  EastAsian_A   },
    {  0x0003CA,  0x000400,  EastAsian_N   },
    {  0x000401,  0x000401,  EastAsian_A   },
    {  0x000402,  0x00040F,  EastAsian_N   },
    {  0x000410,  0x00044F,  EastAsian_A   },
    {  0x000450,  0x000450,  EastAsian_N   },
    {  0x000451,  0x000451,  EastAsian_A   },
    {  0x000452,  0x00052F,  EastAsian_N   },
    {  0x000531,  0x000556,  EastAsian_N   },
    {  0x000559,  0x00058A,  EastAsian_N   },
    {  0x00058D,  0x00058F,  EastAsian_N   },
    {  0x000591,  0x0005C7,  EastAsian_N   },
    {  0x0005D0,  0x0005EA,  EastAsian_N   },
    {  0x0005EF,  0x0005F4,  EastAsian_N   },
    {  0x000600,  0x00070D,  EastAsian_N   },
    {  0x00070F,  0x00074A,  EastAsian_N   },
    {  0x00074D,  0x0007B1,  EastAsian_N   },
    {  0x0007C0,  0x0007FA,  EastAsian_N   },
    {  0x0007FD,  0x00082D,  EastAsian_N   },
    {  0x000830,  0x00083E,  EastAsian_N   },
    {  0x000840,  0x00085B,  EastAsian_N   },
    {  0x00085E,  0x00085E,  EastAsian_N   },
    {  0x000860,  0x00086A,  EastAsian_N   },
    {  0x000870,  0x00088E,  EastAsian_N   },
    {  0x000890,  0x000891,  EastAsian_N   },
    {  0x000898,  0x000983,  EastAsian_N   },
    {  0x000985,  0x00098C,  EastAsian_N   },
    {  0x00098F,  0x000990,  EastAsian_N   },
    {  0x000993,  0x0009A8,  EastAsian_N   },
    {  0x0009AA,  0x0009B0,  EastAsian_N   },
    {  0x0009B2,  0x0009B2,  EastAsian_N   },
    {  0x0009B6,  0x0009B9,  EastAsian_N   },
    {  0x0009BC,  0x0009C4,  EastAsian_N   },
    {  0x0009C7,  0x0009C8,  EastAsian_N   },
    {  0x0009CB,  0x0009CE,  EastAsian_N   },
    {  0x0009D7,  0x0009D7,  EastAsian_N   },
    {  0x0009DC,  0x0009DD,  EastAsian_N   },
    {  0x0009DF,  0x0009E3,  EastAsian_N   },
    {  0x0009E6,  0x0009FE,  EastAsian_N   },
    {  0x000A01,  0x000A03,  EastAsian_N   },
    {  0x000A05,  0x000A0A,  EastAsian_N   },
    {  0x000A0F,  0x000A10,  EastAsian_N   },
    {  0x000A13,  0x000A28,  EastAsian_N   },
    {  0x000A2A,  0x000A30,  EastAsian_N   },
    {  0x000A32,  0x000A33,  EastAsian_N   },
    {  0x000A35,  0x000A36,  EastAsian_N   },
    {  0x000A38,  0x000A39,  EastAsian_N   },
    {  0x000A3C,  0x000A3C,  EastAsian_N   },
    {  0x000A3E,  0x000A42,  EastAsian_N   },
    {  0x000A47,  0x000A48,  EastAsian_N   },
    {  0x000A4B,  0x000A4D,  EastAsian_N   },
    {  0x000A51,  0x000A51,  EastAsian_N   },
    {  0x000A59,  0x000A5C,  EastAsian_N   },
    {  0x000A5E,  0x000A5E,  EastAsian_N   },
    {  0x000A66,  0x000A76,  EastAsian_N   },
    {  0x000A81,  0x000A83,  EastAsian_N   },
    {  0x000A85,  0x000A8D,  EastAsian_N   },
    {  0x000A8F,  0x000A91,  EastAsian_N   },
    {  0x000A93,  0x000AA8,  EastAsian_N   },
    {  0x000AAA,  0x000AB0,  EastAsian_N   },
    {  0x000AB2,  0x000AB3,  EastAsian_N   },
    {  0x000AB5,  0x000AB9,  EastAsian_N   },
    {  0x000ABC,  0x000AC5,  EastAsian_N   },
    {  0x000AC7,  0x000AC9,  EastAsian_N   },
    {  0x000ACB,  0x000ACD,  EastAsian_N   },
    {  0x000AD0,  0x000AD0,  EastAsian_N   },
    {  0x000AE0,  0x000AE3,  EastAsian_N   },
    {  0x000AE6,  0x000AF1,  EastAsian_N   },
    {  0x000AF9,  0x000AFF,  EastAsian_N   },
    {  0x000B01,  0x000B03,  EastAsian_N   },
    {  0x000B05,  0x000B0C,  EastAsian_N   },
    {  0x000B0F,  0x000B10,  EastAsian_N   },
    {  0x000B13,  0x000B28,  EastAsian_N   },
    {  0x000B2A,  0x000B30,  EastAsian_N   },
    {  0x000B32,  0x000B33,  EastAsian_N   },
    {  0x000B35,  0x000B39,  EastAsian_N   },
    {  0x000B3C,  0x000B44,  EastAsian_N   },
    {  0x000B47,  0x000B48,  EastAsian_N   },
    {  0x000B4B,  0x000B4D,  EastAsian_N   },
    {  0x000B55,  0x000B57,  EastAsian_N   },
    {  0x000B5C,  0x000B5D,  EastAsian_N   },
    {  0x000B5F,  0x000B63,  EastAsian_N   },
    {  0x000B66,  0x000B77,  EastAsian_N   },
    {  0x000B82,  0x000B83,  EastAsian_N   },
    {  0x000B85,  0x000B8A,  EastAsian_N   },
    {  0x000B8E,  0x000B90,  EastAsian_N   },
    {  0x000B92,  0x000B95,  EastAsian_N   },
    {  0x000B99,  0x000B9A,  EastAsian_N   },
    {  0x000B9C,  0x000B9C,  EastAsian_N   },
    {  0x000B9E,  0x000B9F,  EastAsian_N   },
    {  0x000BA3,  0x000BA4,  EastAsian_N   },
    {  0x000BA8,  0x000BAA,  EastAsian_N   },
    {  0x000BAE,  0x000BB9,  EastAsian_N   },
    {  0x000BBE,  0x000BC2,  EastAsian_N   },
    {  0x000BC6,  0x000BC8,  EastAsian_N   },
    {  0x000BCA,  0x000BCD,  EastAsian_N   },
    {  0x000BD0,  0x000BD0,  EastAsian_N   },
    {  0x000BD7,  0x000BD7,  EastAsian_N   },
    {  0x000BE6,  0x000BFA,  EastAsian_N   },
    {  0x000C00,  0x000C0C,  EastAsian_N   },
    {  0x000C0E,  0x000C10,  EastAsian_N   },
    {  0x000C12,  0x000C28,  EastAsian_N   },
    {  0x000C2A,  0x000C39,  EastAsian_N   },
    {  0x000C3C,  0x000C44,  EastAsian_N   },
    {  0x000C46,  0x000C48,  EastAsian_N   },
    {  0x000C4A,  0x000C4D,  EastAsian_N   },
    {  0x000C55,  0x000C56,  EastAsian_N   },
    {  0x000C58,  0x000C5A,  EastAsian_N   },
    {  0x000C5D,  0x000C5D,  EastAsian_N   },
    {  0x000C60,  0x000C63,  EastAsian_N   },
    {  0x000C66,  0x000C6F,  EastAsian_N   },
    {  0x000C77,  0x000C8C,  EastAsian_N   },
    {  0x000C8E,  0x000C90,  EastAsian_N   },
    {  0x000C92,  0x000CA8,  EastAsian_N   },
    {  0x000CAA,  0x000CB3,  EastAsian_N   },
    {  0x000CB5,  0x000CB9,  EastAsian_N   },
    {  0x000CBC,  0x000CC4,  EastAsian_N   },
    {  0x000CC6,  0x000CC8,  EastAsian_N   },
    {  0x000CCA,  0x000CCD,  EastAsian_N   },
    {  0x000CD5,  0x000CD6,  EastAsian_N   },
    {  0x000CDD,  0x000CDE,  EastAsian_N   },
    {  0x000CE0,  0x000CE3,  EastAsian_N   },
    {  0x000CE6,  0x000CEF,  EastAsian_N   },
    {  0x000CF1,  0x000CF2,  EastAsian_N   },
    {  0x000D00,  0x000D0C,  EastAsian_N   },
    {  0x000D0E,  0x000D10,  EastAsian_N   },
    {  0x000D12,  0x000D44,  EastAsian_N   },
    {  0x000D46,  0x000D48,  EastAsian_N   },
    {  0x000D4A,  0x000D4F,  EastAsian_N   },
    {  0x000D54,  0x000D63,  EastAsian_N   },
    {  0x000D66,  0x000D7F,  EastAsian_N   },
    {  0x000D81,  0x000D83,  EastAsian_N   },
    {  0x000D85,  0x000D96,  EastAsian_N   },
    {  0x000D9A,  0x000DB1,  EastAsian_N   },
    {  0x000DB3,  0x000DBB,  EastAsian_N   },
    {  0x000DBD,  0x000DBD,  EastAsian_N   },
    {  0x000DC0,  0x000DC6,  EastAsian_N   },
    {  0x000DCA,  0x000DCA,  EastAsian_N   },
    {  0x000DCF,  0x000DD4,  EastAsian_N   },
    {  0x000DD6,  0x000DD6,  EastAsian_N   },
    {  0x000DD8,  0x000DDF,  EastAsian_N   },
    {  0x000DE6,  0x000DEF,  EastAsian_N   },
    {  0x000DF2,  0x000DF4,  EastAsian_N   },
    {  0x000E01,  0x000E3A,  EastAsian_N   },
    {  0x000E3F,  0x000E5B,  EastAsian_N   },
    {  0x000E81,  0x000E82,  EastAsian_N   },
    {  0x000E84,  0x000E84,  EastAsian_N   },
    {  0x000E86,  0x000E8A,  EastAsian_N   },
    {  0x000E8C,  0x000EA3,  EastAsian_N   },
    {  0x000EA5,  0x000EA5,  EastAsian_N   },
    {  0x000EA7,  0x000EBD,  EastAsian_N   },
    {  0x000EC0,  0x000EC4,  EastAsian_N   },
    {  0x000EC6,  0x000EC6,  EastAsian_N   },
    {  0x000EC8,  0x000ECD,  EastAsian_N   },
    {  0x000ED0,  0x000ED9,  EastAsian_N   },
    {  0x000EDC,  0x000EDF,  EastAsian_N   },
    {  0x000F00,  0x000F47,  EastAsian_N   },
    {  0x000F49,  0x000F6C,  EastAsian_N   },
    {  0x000F71,  0x000F97,  EastAsian_N   },
    {  0x000F99,  0x000FBC,  EastAsian_N   },
    {  0x000FBE,  0x000FCC,  EastAsian_N   },
    {  0x000FCE,  0x000FDA,  EastAsian_N   },
    {  0x001000,  0x0010C5,  EastAsian_N   },
    {  0x0010C7,  0x0010C7,  EastAsian_N   },
    {  0x0010CD,  0x0010CD,  EastAsian_N   },
    {  0x0010D0,  0x0010FF,  EastAsian_N   },
    {  0x001100,  0x00115F,  EastAsian_W   },
    {  0x001160,  0x001248,  EastAsian_N   },
    {  0x00124A,  0x00124D,  EastAsian_N   },
    {  0x001250,  0x001256,  EastAsian_N   },
    {  0x001258,  0x001258,  EastAsian_N   },
    {  0x00125A,  0x00125D,  EastAsian_N   },
    {  0x001260,  0x001288,  EastAsian_N   },
    {  0x00128A,  0x00128D,  EastAsian_N   },
    {  0x001290,  0x0012B0,  EastAsian_N   },
    {  0x0012B2,  0x0012B5,  EastAsian_N   },
    {  0x0012B8,  0x0012BE,  EastAsian_N   },
    {  0x0012C0,  0x0012C0,  EastAsian_N   },
    {  0x0012C2,  0x0012C5,  EastAsian_N   },
    {  0x0012C8,  0x0012D6,  EastAsian_N   },
    {  0x0012D8,  0x001310,  EastAsian_N   },
    {  0x001312,  0x001315,  EastAsian_N   },
    {  0x001318,  0x00135A,  EastAsian_N   },
    {  0x00135D,  0x00137C,  EastAsian_N   },
    {  0x001380,  0x001399,  EastAsian_N   },
    {  0x0013A0,  0x0013F5,  EastAsian_N   },
    {  0x0013F8,  0x0013FD,  EastAsian_N   },
    {  0x001400,  0x00169C,  EastAsian_N   },
    {  0x0016A0,  0x0016F8,  EastAsian_N   },
    {  0x001700,  0x001715,  EastAsian_N   },
    {  0x00171F,  0x001736,  EastAsian_N   },
    {  0x001740,  0x001753,  EastAsian_N   },
    {  0x001760,  0x00176C,  EastAsian_N   },
    {  0x00176E,  0x001770,  EastAsian_N   },
    {  0x001772,  0x001773,  EastAsian_N   },
    {  0x001780,  0x0017DD,  EastAsian_N   },
    {  0x0017E0,  0x0017E9,  EastAsian_N   },
    {  0x0017F0,  0x0017F9,  EastAsian_N   },
    {  0x001800,  0x001819,  EastAsian_N   },
    {  0x001820,  0x001878,  EastAsian_N   },
    {  0x001880,  0x0018AA,  EastAsian_N   },
    {  0x0018B0,  0x0018F5,  EastAsian_N   },
    {  0x001900,  0x00191E,  EastAsian_N   },
    {  0x001920,  0x00192B,  EastAsian_N   },
    {  0x001930,  0x00193B,  EastAsian_N   },
    {  0x001940,  0x001940,  EastAsian_N   },
    {  0x001944,  0x00196D,  EastAsian_N   },
    {  0x001970,  0x001974,  EastAsian_N   },
    {  0x001980,  0x0019AB,  EastAsian_N   },
    {  0x0019B0,  0x0019C9,  EastAsian_N   },
    {  0x0019D0,  0x0019DA,  EastAsian_N   },
    {  0x0019DE,  0x001A1B,  EastAsian_N   },
    {  0x001A1E,  0x001A5E,  EastAsian_N   },
    {  0x001A60,  0x001A7C,  EastAsian_N   },
    {  0x001A7F,  0x001A89,  EastAsian_N   },
    {  0x001A90,  0x001A99,  EastAsian_N   },
    {  0x001AA0,  0x001AAD,  EastAsian_N   },
    {  0x001AB0,  0x001ACE,  EastAsian_N   },
    {  0x001B00,  0x001B4C,  EastAsian_N   },
    {  0x001B50,  0x001B7E,  EastAsian_N   },
    {  0x001B80,  0x001BF3,  EastAsian_N   },
    {  0x001BFC,  0x001C37,  EastAsian_N   },
    {  0x001C3B,  0x001C49,  EastAsian_N   },
    {  0x001C4D,  0x001C88,  EastAsian_N   },
    {  0x001C90,  0x001CBA,  EastAsian_N   },
    {  0x001CBD,  0x001CC7,  EastAsian_N   },
    {  0x001CD0,  0x001CFA,  EastAsian_N   },
    {  0x001D00,  0x001F15,  EastAsian_N   },
    {  0x001F18,  0x001F1D,  EastAsian_N   },
    {  0x001F20,  0x001F45,  EastAsian_N   },
    {  0x001F48,  0x001F4D,  EastAsian_N   },
    {  0x001F50,  0x001F57,  EastAsian_N   },
    {  0x001F59,  0x001F59,  EastAsian_N   },
    {  0x001F5B,  0x001F5B,  EastAsian_N   },
    {  0x001F5D,  0x001F5D,  EastAsian_N   },
    {  0x001F5F,  0x001F7D,  EastAsian_N   },
    {  0x001F80,  0x001FB4,  EastAsian_N   },
    {  0x001FB6,  0x001FC4,  EastAsian_N   },
    {  0x001FC6,  0x001FD3,  EastAsian_N   },
    {  0x001FD6,  0x001FDB,  EastAsian_N   },
    {  0x001FDD,  0x001FEF,  EastAsian_N   },
    {  0x001FF2,  0x001FF4,  EastAsian_N   },
    {  0x001FF6,  0x001FFE,  EastAsian_N   },
    {  0x002000,  0x00200F,  EastAsian_N   },
    {  0x002010,  0x002010,  EastAsian_A   },
    {  0x002011,  0x002012,  EastAsian_N   },
    {  0x002013,  0x002016,  EastAsian_A   },
    {  0x002017,  0x002017,  EastAsian_N   },
    {  0x002018,  0x002019,  EastAsian_A   },
    {  0x00201A,  0x00201B,  EastAsian_N   },
    {  0x00201C,  0x00201D,  EastAsian_A   },
    {  0x00201E,  0x00201F,  EastAsian_N   },
    {  0x002020,  0x002022,  EastAsian_A   },
    {  0x002023,  0x002023,  EastAsian_N   },
    {  0x002024,  0x002027,  EastAsian_A   },
    {  0x002028,  0x00202F,  EastAsian_N   },
    {  0x002030,  0x002030,  EastAsian_A   },
    {  0x002031,  0x002031,  EastAsian_N   },
    {  0x002032,  0x002033,  EastAsian_A   },
    {  0x002034,  0x002034,  EastAsian_N   },
    {  0x002035,  0x002035,  EastAsian_A   },
    {  0x002036,  0x00203A,  EastAsian_N   },
    {  0x00203B,  0x00203B,  EastAsian_A   },
    {  0x00203C,  0x00203D,  EastAsian_N   },
    {  0x00203E,  0x00203E,  EastAsian_A   },
    {  0x00203F,  0x002064,  EastAsian_N   },
    {  0x002066,  0x002071,  EastAsian_N   },
    {  0x002074,  0x002074,  EastAsian_A   },
    {  0x002075,  0x00207E,  EastAsian_N   },
    {  0x00207F,  0x00207F,  EastAsian_A   },
    {  0x002080,  0x002080,  EastAsian_N   },
    {  0x002081,  0x002084,  EastAsian_A   },
    {  0x002085,  0x00208E,  EastAsian_N   },
    {  0x002090,  0x00209C,  EastAsian_N   },
    {  0x0020A0,  0x0020A8,  EastAsian_N   },
    {  0x0020A9,  0x0020A9,  EastAsian_H   },
    {  0x0020AA,  0x0020AB,  EastAsian_N   },
    {  0x0020AC,  0x0020AC,  EastAsian_A   },
    {  0x0020AD,  0x0020C0,  EastAsian_N   },
    {  0x0020D0,  0x0020F0,  EastAsian_N   },
    {  0x002100,  0x002102,  EastAsian_N   },
    {  0x002103,  0x002103,  EastAsian_A   },
    {  0x002104,  0x002104,  EastAsian_N   },
    {  0x002105,  0x002105,  EastAsian_A   },
    {  0x002106,  0x002108,  EastAsian_N   },
    {  0x002109,  0x002109,  EastAsian_A   },
    {  0x00210A,  0x002112,  EastAsian_N   },
    {  0x002113,  0x002113,  EastAsian_A   },
    {  0x002114,  0x002115,  EastAsian_N   },
    {  0x002116,  0x002116,  EastAsian_A   },
    {  0x002117,  0x002120,  EastAsian_N   },
    {  0x002121,  0x002122,  EastAsian_A   },
    {  0x002123,  0x002125,  EastAsian_N   },
    {  0x002126,  0x002126,  EastAsian_A   },
    {  0x002127,  0x00212A,  EastAsian_N   },
    {  0x00212B,  0x00212B,  EastAsian_A   },
    {  0x00212C,  0x002152,  EastAsian_N   },
    {  0x002153,  0x002154,  EastAsian_A   },
    {  0x002155,  0x00215A,  EastAsian_N   },
    {  0x00215B,  0x00215E,  EastAsian_A   },
    {  0x00215F,  0x00215F,  EastAsian_N   },
    {  0x002160,  0x00216B,  EastAsian_A   },
    {  0x00216C,  0x00216F,  EastAsian_N   },
    {  0x002170,  0x002179,  EastAsian_A   },
    {  0x00217A,  0x002188,  EastAsian_N   },
    {  0x002189,  0x002189,  EastAsian_A   },
    {  0x00218A,  0x00218B,  EastAsian_N   },
    {  0x002190,  0x002199,  EastAsian_A   },
    {  0x00219A,  0x0021B7,  EastAsian_N   },
    {  0x0021B8,  0x0021B9,  EastAsian_A   },
    {  0x0021BA,  0x0021D1,  EastAsian_N   },
    {  0x0021D2,  0x0021D2,  EastAsian_A   },
    {  0x0021D3,  0x0021D3,  EastAsian_N   },
    {  0x0021D4,  0x0021D4,  EastAsian_A   },
    {  0x0021D5,  0x0021E6,  EastAsian_N   },
    {  0x0021E7,  0x0021E7,  EastAsian_A   },
    {  0x0021E8,  0x0021FF,  EastAsian_N   },
    {  0x002200,  0x002200,  EastAsian_A   },
    {  0x002201,  0x002201,  EastAsian_N   },
    {  0x002202,  0x002203,  EastAsian_A   },
    {  0x002204,  0x002206,  EastAsian_N   },
    {  0x002207,  0x002208,  EastAsian_A   },
    {  0x002209,  0x00220A,  EastAsian_N   },
    {  0x00220B,  0x00220B,  EastAsian_A   },
    {  0x00220C,  0x00220E,  EastAsian_N   },
    {  0x00220F,  0x00220F,  EastAsian_A   },
    {  0x002210,  0x002210,  EastAsian_N   },
    {  0x002211,  0x002211,  EastAsian_A   },
    {  0x002212,  0x002214,  EastAsian_N   },
    {  0x002215,  0x002215,  EastAsian_A   },
    {  0x002216,  0x002219,  EastAsian_N   },
    {  0x00221A,  0x00221A,  EastAsian_A   },
    {  0x00221B,  0x00221C,  EastAsian_N   },
    {  0x00221D,  0x002220,  EastAsian_A   },
    {  0x002221,  0x002222,  EastAsian_N   },
    {  0x002223,  0x002223,  EastAsian_A   },
    {  0x002224,  0x002224,  EastAsian_N   },
    {  0x002225,  0x002225,  EastAsian_A   },
    {  0x002226,  0x002226,  EastAsian_N   },
    {  0x002227,  0x00222C,  EastAsian_A   },
    {  0x00222D,  0x00222D,  EastAsian_N   },
    {  0x00222E,  0x00222E,  EastAsian_A   },
    {  0x00222F,  0x002233,  EastAsian_N   },
    {  0x002234,  0x002237,  EastAsian_A   },
    {  0x002238,  0x00223B,  EastAsian_N   },
    {  0x00223C,  0x00223D,  EastAsian_A   },
    {  0x00223E,  0x002247,  EastAsian_N   },
    {  0x002248,  0x002248,  EastAsian_A   },
    {  0x002249,  0x00224B,  EastAsian_N   },
    {  0x00224C,  0x00224C,  EastAsian_A   },
    {  0x00224D,  0x002251,  EastAsian_N   },
    {  0x002252,  0x002252,  EastAsian_A   },
    {  0x002253,  0x00225F,  EastAsian_N   },
    {  0x002260,  0x002261,  EastAsian_A   },
    {  0x002262,  0x002263,  EastAsian_N   },
    {  0x002264,  0x002267,  EastAsian_A   },
    {  0x002268,  0x002269,  EastAsian_N   },
    {  0x00226A,  0x00226B,  EastAsian_A   },
    {  0x00226C,  0x00226D,  EastAsian_N   },
    {  0x00226E,  0x00226F,  EastAsian_A   },
    {  0x002270,  0x002281,  EastAsian_N   },
    {  0x002282,  0x002283,  EastAsian_A   },
    {  0x002284,  0x002285,  EastAsian_N   },
    {  0x002286,  0x002287,  EastAsian_A   },
    {  0x002288,  0x002294,  EastAsian_N   },
    {  0x002295,  0x002295,  EastAsian_A   },
    {  0x002296,  0x002298,  EastAsian_N   },
    {  0x002299,  0x002299,  EastAsian_A   },
    {  0x00229A,  0x0022A4,  EastAsian_N   },
    {  0x0022A5,  0x0022A5,  EastAsian_A   },
    {  0x0022A6,  0x0022BE,  EastAsian_N   },
    {  0x0022BF,  0x0022BF,  EastAsian_A   },
    {  0x0022C0,  0x002311,  EastAsian_N   },
    {  0x002312,  0x002312,  EastAsian_A   },
    {  0x002313,  0x002319,  EastAsian_N   },
    {  0x00231A,  0x00231B,  EastAsian_W   },
    {  0x00231C,  0x002328,  EastAsian_N   },
    {  0x002329,  0x00232A,  EastAsian_W   },
    {  0x00232B,  0x0023E8,  EastAsian_N   },
    {  0x0023E9,  0x0023EC,  EastAsian_W   },
    {  0x0023ED,  0x0023EF,  EastAsian_N   },
    {  0x0023F0,  0x0023F0,  EastAsian_W   },
    {  0x0023F1,  0x0023F2,  EastAsian_N   },
    {  0x0023F3,  0x0023F3,  EastAsian_W   },
    {  0x0023F4,  0x002426,  EastAsian_N   },
    {  0x002440,  0x00244A,  EastAsian_N   },
    {  0x002460,  0x0024E9,  EastAsian_A   },
    {  0x0024EA,  0x0024EA,  EastAsian_N   },
    {  0x0024EB,  0x00254B,  EastAsian_A   },
    {  0x00254C,  0x00254F,  EastAsian_N   },
    {  0x002550,  0x002573,  EastAsian_A   },
    {  0x002574,  0x00257F,  EastAsian_N   },
    {  0x002580,  0x00258F,  EastAsian_A   },
    {  0x002590,  0x002591,  EastAsian_N   },
    {  0x002592,  0x002595,  EastAsian_A   },
    {  0x002596,  0x00259F,  EastAsian_N   },
    {  0x0025A0,  0x0025A1,  EastAsian_A   },
    {  0x0025A2,  0x0025A2,  EastAsian_N   },
    {  0x0025A3,  0x0025A9,  EastAsian_A   },
    {  0x0025AA,  0x0025B1,  EastAsian_N   },
    {  0x0025B2,  0x0025B3,  EastAsian_A   },
    {  0x0025B4,  0x0025B5,  EastAsian_N   },
    {  0x0025B6,  0x0025B7,  EastAsian_A   },
    {  0x0025B8,  0x0025BB,  EastAsian_N   },
    {  0x0025BC,  0x0025BD,  EastAsian_A   },
    {  0x0025BE,  0x0025BF,  EastAsian_N   },
    {  0x0025C0,  0x0025C1,  EastAsian_A   },
    {  0x0025C2,  0x0025C5,  EastAsian_N   },
    {  0x0025C6,  0x0025C8,  EastAsian_A   },
    {  0x0025C9,  0x0025CA,  EastAsian_N   },
    {  0x0025CB,  0x0025CB,  EastAsian_A   },
    {  0x0025CC,  0x0025CD,  EastAsian_N   },
    {  0x0025CE,  0x0025D1,  EastAsian_A   },
    {  0x0025D2,  0x0025E1,  EastAsian_N   },
    {  0x0025E2,  0x0025E5,  EastAsian_A   },
    {  0x0025E6,  0x0025EE,  EastAsian_N   },
    {  0x0025EF,  0x0025EF,  EastAsian_A   },
    {  0x0025F0,  0x0025FC,  EastAsian_N   },
    {  0x0025FD,  0x0025FE,  EastAsian_W   },
    {  0x0025FF,  0x002604,  EastAsian_N   },
    {  0x002605,  0x002606,  EastAsian_A   },
    {  0x002607,  0x002608,  EastAsian_N   },
    {  0x002609,  0x002609,  EastAsian_A   },
    {  0x00260A,  0x00260D,  EastAsian_N   },
    {  0x00260E,  0x00260F,  EastAsian_A   },
    {  0x002610,  0x002613,  EastAsian_N   },
    {  0x002614,  0x002615,  EastAsian_W   },
    {  0x002616,  0x00261B,  EastAsian_N   },
    {  0x00261C,  0x00261C,  EastAsian_A   },
    {  0x00261D,  0x00261D,  EastAsian_N   },
    {  0x00261E,  0x00261E,  EastAsian_A   },
    {  0x00261F,  0x00263F,  EastAsian_N   },
    {  0x002640,  0x002640,  EastAsian_A   },
    {  0x002641,  0x002641,  EastAsian_N   },
    {  0x002642,  0x002642,  EastAsian_A   },
    {  0x002643,  0x002647,  EastAsian_N   },
    {  0x002648,  0x002653,  EastAsian_W   },
    {  0x002654,  0x00265F,  EastAsian_N   },
    {  0x002660,  0x002661,  EastAsian_A   },
    {  0x002662,  0x002662,  EastAsian_N   },
    {  0x002663,  0x002665,  EastAsian_A   },
    {  0x002666,  0x002666,  EastAsian_N   },
    {  0x002667,  0x00266A,  EastAsian_A   },
    {  0x00266B,  0x00266B,  EastAsian_N   },
    {  0x00266C,  0x00266D,  EastAsian_A   },
    {  0x00266E,  0x00266E,  EastAsian_N   },
    {  0x00266F,  0x00266F,  EastAsian_A   },
    {  0x002670,  0x00267E,  EastAsian_N   },
    {  0x00267F,  0x00267F,  EastAsian_W   },
    {  0x002680,  0x002692,  EastAsian_N   },
    {  0x002693,  0x002693,  EastAsian_W   },
    {  0x002694,  0x00269D,  EastAsian_N   },
    {  0x00269E,  0x00269F,  EastAsian_A   },
    {  0x0026A0,  0x0026A0,  EastAsian_N   },
    {  0x0026A1,  0x0026A1,  EastAsian_W   },
    {  0x0026A2,  0x0026A9,  EastAsian_N   },
    {  0x0026AA,  0x0026AB,  EastAsian_W   },
    {  0x0026AC,  0x0026BC,  EastAsian_N   },
    {  0x0026BD,  0x0026BE,  EastAsian_W   },
    {  0x0026BF,  0x0026BF,  EastAsian_A   },
    {  0x0026C0,  0x0026C3,  EastAsian_N   },
    {  0x0026C4,  0x0026C5,  EastAsian_W   },
    {  0x0026C6,  0x0026CD,  EastAsian_A   },
    {  0x0026CE,  0x0026CE,  EastAsian_W   },
    {  0x0026CF,  0x0026D3,  EastAsian_A   },
    {  0x0026D4,  0x0026D4,  EastAsian_W   },
    {  0x0026D5,  0x0026E1,  EastAsian_A   },
    {  0x0026E2,  0x0026E2,  EastAsian_N   },
    {  0x0026E3,  0x0026E3,  EastAsian_A   },
    {  0x0026E4,  0x0026E7,  EastAsian_N   },
    {  0x0026E8,  0x0026E9,  EastAsian_A   },
    {  0x0026EA,  0x0026EA,  EastAsian_W   },
    {  0x0026EB,  0x0026F1,  EastAsian_A   },
    {  0x0026F2,  0x0026F3,  EastAsian_W   },
    {  0x0026F4,  0x0026F4,  EastAsian_A   },
    {  0x0026F5,  0x0026F5,  EastAsian_W   },
    {  0x0026F6,  0x0026F9,  EastAsian_A   },
    {  0x0026FA,  0x0026FA,  EastAsian_W   },
    {  0x0026FB,  0x0026FC,  EastAsian_A   },
    {  0x0026FD,  0x0026FD,  EastAsian_W   },
    {  0x0026FE,  0x0026FF,  EastAsian_A   },
    {  0x002700,  0x002704,  EastAsian_N   },
    {  0x002705,  0x002705,  EastAsian_W   },
    {  0x002706,  0x002709,  EastAsian_N   },
    {  0x00270A,  0x00270B,  EastAsian_W   },
    {  0x00270C,  0x002727,  EastAsian_N   },
    {  0x002728,  0x002728,  EastAsian_W   },
    {  0x002729,  0x00273C,  EastAsian_N   },
    {  0x00273D,  0x00273D,  EastAsian_A   },
    {  0x00273E,  0x00274B,  EastAsian_N   },
    {  0x00274C,  0x00274C,  EastAsian_W   },
    {  0x00274D,  0x00274D,  EastAsian_N   },
    {  0x00274E,  0x00274E,  EastAsian_W   },
    {  0x00274F,  0x002752,  EastAsian_N   },
    {  0x002753,  0x002755,  EastAsian_W   },
    {  0x002756,  0x002756,  EastAsian_N   },
    {  0x002757,  0x002757,  EastAsian_W   },
    {  0x002758,  0x002775,  EastAsian_N   },
    {  0x002776,  0x00277F,  EastAsian_A   },
    {  0x002780,  0x002794,  EastAsian_N   },
    {  0x002795,  0x002797,  EastAsian_W   },
    {  0x002798,  0x0027AF,  EastAsian_N   },
    {  0x0027B0,  0x0027B0,  EastAsian_W   },
    {  0x0027B1,  0x0027BE,  EastAsian_N   },
    {  0x0027BF,  0x0027BF,  EastAsian_W   },
    {  0x0027C0,  0x0027E5,  EastAsian_N   },
    {  0x0027E6,  0x0027ED,  EastAsian_NA  },
    {  0x0027EE,  0x002984,  EastAsian_N   },
    {  0x002985,  0x002986,  EastAsian_NA  },
    {  0x002987,  0x002B1A,  EastAsian_N   },
    {  0x002B1B,  0x002B1C,  EastAsian_W   },
    {  0x002B1D,  0x002B4F,  EastAsian_N   },
    {  0x002B50,  0x002B50,  EastAsian_W   },
    {  0x002B51,  0x002B54,  EastAsian_N   },
    {  0x002B55,  0x002B55,  EastAsian_W   },
    {  0x002B56,  0x002B59,  EastAsian_A   },
    {  0x002B5A,  0x002B73,  EastAsian_N   },
    {  0x002B76,  0x002B95,  EastAsian_N   },
    {  0x002B97,  0x002CF3,  EastAsian_N   },
    {  0x002CF9,  0x002D25,  EastAsian_N   },
    {  0x002D27,  0x002D27,  EastAsian_N   },
    {  0x002D2D,  0x002D2D,  EastAsian_N   },
    {  0x002D30,  0x002D67,  EastAsian_N   },
    {  0x002D6F,  0x002D70,  EastAsian_N   },
    {  0x002D7F,  0x002D96,  EastAsian_N   },
    {  0x002DA0,  0x002DA6,  EastAsian_N   },
    {  0x002DA8,  0x002DAE,  EastAsian_N   },
    {  0x002DB0,  0x002DB6,  EastAsian_N   },
    {  0x002DB8,  0x002DBE,  EastAsian_N   },
    {  0x002DC0,  0x002DC6,  EastAsian_N   },
    {  0x002DC8,  0x002DCE,  EastAsian_N   },
    {  0x002DD0,  0x002DD6,  EastAsian_N   },
    {  0x002DD8,  0x002DDE,  EastAsian_N   },
    {  0x002DE0,  0x002E5D,  EastAsian_N   },
    {  0x002E80,  0x002E99,  EastAsian_W   },
    {  0x002E9B,  0x002EF3,  EastAsian_W   },
    {  0x002F00,  0x002FD5,  EastAsian_W   },
    {  0x002FF0,  0x002FFB,  EastAsian_W   },
    {  0x003000,  0x003000,  EastAsian_F   },
    {  0x003001,  0x00303E,  EastAsian_W   },
    {  0x00303F,  0x00303F,  EastAsian_N   },
    {  0x003041,  0x003096,  EastAsian_W   },
    {  0x003099,  0x0030FF,  EastAsian_W   },
    {  0x003105,  0x00312F,  EastAsian_W   },
    {  0x003131,  0x00318E,  EastAsian_W   },
    {  0x003190,  0x0031E3,  EastAsian_W   },
    {  0x0031F0,  0x00321E,  EastAsian_W   },
    {  0x003220,  0x003247,  EastAsian_W   },
    {  0x003248,  0x00324F,  EastAsian_A   },
    {  0x003250,  0x004DBF,  EastAsian_W   },
    {  0x004DC0,  0x004DFF,  EastAsian_N   },
    {  0x004E00,  0x00A48C,  EastAsian_W   },
    {  0x00A490,  0x00A4C6,  EastAsian_W   },
    {  0x00A4D0,  0x00A62B,  EastAsian_N   },
    {  0x00A640,  0x00A6F7,  EastAsian_N   },
    {  0x00A700,  0x00A7CA,  EastAsian_N   },
    {  0x00A7D0,  0x00A7D1,  EastAsian_N   },
    {  0x00A7D3,  0x00A7D3,  EastAsian_N   },
    {  0x00A7D5,  0x00A7D9,  EastAsian_N   },
    {  0x00A7F2,  0x00A82C,  EastAsian_N   },
    {  0x00A830,  0x00A839,  EastAsian_N   },
    {  0x00A840,  0x00A877,  EastAsian_N   },
    {  0x00A880,  0x00A8C5,  EastAsian_N   },
    {  0x00A8CE,  0x00A8D9,  EastAsian_N   },
    {  0x00A8E0,  0x00A953,  EastAsian_N   },
    {  0x00A95F,  0x00A95F,  EastAsian_N   },
    {  0x00A960,  0x00A97C,  EastAsian_W   },
    {  0x00A980,  0x00A9CD,  EastAsian_N   },
    {  0x00A9CF,  0x00A9D9,  EastAsian_N   },
    {  0x00A9DE,  0x00A9FE,  EastAsian_N   },
    {  0x00AA00,  0x00AA36,  EastAsian_N   },
    {  0x00AA40,  0x00AA4D,  EastAsian_N   },
    {  0x00AA50,  0x00AA59,  EastAsian_N   },
    {  0x00AA5C,  0x00AAC2,  EastAsian_N   },
    {  0x00AADB,  0x00AAF6,  EastAsian_N   },
    {  0x00AB01,  0x00AB06,  EastAsian_N   },
    {  0x00AB09,  0x00AB0E,  EastAsian_N   },
    {  0x00AB11,  0x00AB16,  EastAsian_N   },
    {  0x00AB20,  0x00AB26,  EastAsian_N   },
    {  0x00AB28,  0x00AB2E,  EastAsian_N   },
    {  0x00AB30,  0x00AB6B,  EastAsian_N   },
    {  0x00AB70,  0x00ABED,  EastAsian_N   },
    {  0x00ABF0,  0x00ABF9,  EastAsian_N   },
    {  0x00AC00,  0x00D7A3,  EastAsian_W   },
    {  0x00D7B0,  0x00D7C6,  EastAsian_N   },
    {  0x00D7CB,  0x00D7FB,  EastAsian_N   },
    {  0x00D800,  0x00DFFF,  EastAsian_N   },
    {  0x00E000,  0x00F8FF,  EastAsian_A   },
    {  0x00F900,  0x00FAFF,  EastAsian_W   },
    {  0x00FB00,  0x00FB06,  EastAsian_N   },
    {  0x00FB13,  0x00FB17,  EastAsian_N   },
    {  0x00FB1D,  0x00FB36,  EastAsian_N   },
    {  0x00FB38,  0x00FB3C,  EastAsian_N   },
    {  0x00FB3E,  0x00FB3E,  EastAsian_N   },
    {  0x00FB40,  0x00FB41,  EastAsian_N   },
    {  0x00FB43,  0x00FB44,  EastAsian_N   },
    {  0x00FB46,  0x00FBC2,  EastAsian_N   },
    {  0x00FBD3,  0x00FD8F,  EastAsian_N   },
    {  0x00FD92,  0x00FDC7,  EastAsian_N   },
    {  0x00FDCF,  0x00FDCF,  EastAsian_N   },
    {  0x00FDF0,  0x00FDFF,  EastAsian_N   },
    {  0x00FE00,  0x00FE0F,  EastAsian_A   },
    {  0x00FE10,  0x00FE19,  EastAsian_W   },
    {  0x00FE20,  0x00FE2F,  EastAsian_N   },
    {  0x00FE30,  0x00FE52,  EastAsian_W   },
    {  0x00FE54,  0x00FE66,  EastAsian_W   },
    {  0x00FE68,  0x00FE6B,  EastAsian_W   },
    {  0x00FE70,  0x00FE74,  EastAsian_N   },
    {  0x00FE76,  0x00FEFC,  EastAsian_N   },
    {  0x00FEFF,  0x00FEFF,  EastAsian_N   },
    {  0x00FF01,  0x00FF60,  EastAsian_F   },
    {  0x00FF61,  0x00FFBE,  EastAsian_H   },
    {  0x00FFC2,  0x00FFC7,  EastAsian_H   },
    {  0x00FFCA,  0x00FFCF,  EastAsian_H   },
    {  0x00FFD2,  0x00FFD7,  EastAsian_H   },
    {  0x00FFDA,  0x00FFDC,  EastAsian_H   },
    {  0x00FFE0,  0x00FFE6,  EastAsian_F   },
    {  0x00FFE8,  0x00FFEE,  EastAsian_H   },
    {  0x00FFF9,  0x00FFFC,  EastAsian_N   },
    {  0x00FFFD,  0x00FFFD,  EastAsian_A   },
    {  0x010000,  0x01000B,  EastAsian_N   },
    {  0x01000D,  0x010026,  EastAsian_N   },
    {  0x010028,  0x01003A,  EastAsian_N   },
    {  0x01003C,  0x01003D,  EastAsian_N   },
    {  0x01003F,  0x01004D,  EastAsian_N   },
    {  0x010050,  0x01005D,  EastAsian_N   },
    {  0x010080,  0x0100FA,  EastAsian_N   },
    {  0x010100,  0x010102,  EastAsian_N   },
    {  0x010107,  0x010133,  EastAsian_N   },
    {  0x010137,  0x01018E,  EastAsian_N   },
    {  0x010190,  0x01019C,  EastAsian_N   },
    {  0x0101A0,  0x0101A0,  EastAsian_N   },
    {  0x0101D0,  0x0101FD,  EastAsian_N   },
    {  0x010280,  0x01029C,  EastAsian_N   },
    {  0x0102A0,  0x0102D0,  EastAsian_N   },
    {  0x0102E0,  0x0102FB,  EastAsian_N   },
    {  0x010300,  0x010323,  EastAsian_N   },
    {  0x01032D,  0x01034A,  EastAsian_N   },
    {  0x010350,  0x01037A,  EastAsian_N   },
    {  0x010380,  0x01039D,  EastAsian_N   },
    {  0x01039F,  0x0103C3,  EastAsian_N   },
    {  0x0103C8,  0x0103D5,  EastAsian_N   },
    {  0x010400,  0x01049D,  EastAsian_N   },
    {  0x0104A0,  0x0104A9,  EastAsian_N   },
    {  0x0104B0,  0x0104D3,  EastAsian_N   },
    {  0x0104D8,  0x0104FB,  EastAsian_N   },
    {  0x010500,  0x010527,  EastAsian_N   },
    {  0x010530,  0x010563,  EastAsian_N   },
    {  0x01056F,  0x01057A,  EastAsian_N   },
    {  0x01057C,  0x01058A,  EastAsian_N   },
    {  0x01058C,  0x010592,  EastAsian_N   },
    {  0x010594,  0x010595,  EastAsian_N   },
    {  0x010597,  0x0105A1,  EastAsian_N   },
    {  0x0105A3,  0x0105B1,  EastAsian_N   },
    {  0x0105B3,  0x0105B9,  EastAsian_N   },
    {  0x0105BB,  0x0105BC,  EastAsian_N   },
    {  0x010600,  0x010736,  EastAsian_N   },
    {  0x010740,  0x010755,  EastAsian_N   },
    {  0x010760,  0x010767,  EastAsian_N   },
    {  0x010780,  0x010785,  EastAsian_N   },
    {  0x010787,  0x0107B0,  EastAsian_N   },
    {  0x0107B2,  0x0107BA,  EastAsian_N   },
    {  0x010800,  0x010805,  EastAsian_N   },
    {  0x010808,  0x010808,  EastAsian_N   },
    {  0x01080A,  0x010835,  EastAsian_N   },
    {  0x010837,  0x010838,  EastAsian_N   },
    {  0x01083C,  0x01083C,  EastAsian_N   },
    {  0x01083F,  0x010855,  EastAsian_N   },
    {  0x010857,  0x01089E,  EastAsian_N   },
    {  0x0108A7,  0x0108AF,  EastAsian_N   },
    {  0x0108E0,  0x0108F2,  EastAsian_N   },
    {  0x0108F4,  0x0108F5,  EastAsian_N   },
    {  0x0108FB,  0x01091B,  EastAsian_N   },
    {  0x01091F,  0x010939,  EastAsian_N   },
    {  0x01093F,  0x01093F,  EastAsian_N   },
    {  0x010980,  0x0109B7,  EastAsian_N   },
    {  0x0109BC,  0x0109CF,  EastAsian_N   },
    {  0x0109D2,  0x010A03,  EastAsian_N   },
    {  0x010A05,  0x010A06,  EastAsian_N   },
    {  0x010A0C,  0x010A13,  EastAsian_N   },
    {  0x010A15,  0x010A17,  EastAsian_N   },
    {  0x010A19,  0x010A35,  EastAsian_N   },
    {  0x010A38,  0x010A3A,  EastAsian_N   },
    {  0x010A3F,  0x010A48,  EastAsian_N   },
    {  0x010A50,  0x010A58,  EastAsian_N   },
    {  0x010A60,  0x010A9F,  EastAsian_N   },
    {  0x010AC0,  0x010AE6,  EastAsian_N   },
    {  0x010AEB,  0x010AF6,  EastAsian_N   },
    {  0x010B00,  0x010B35,  EastAsian_N   },
    {  0x010B39,  0x010B55,  EastAsian_N   },
    {  0x010B58,  0x010B72,  EastAsian_N   },
    {  0x010B78,  0x010B91,  EastAsian_N   },
    {  0x010B99,  0x010B9C,  EastAsian_N   },
    {  0x010BA9,  0x010BAF,  EastAsian_N   },
    {  0x010C00,  0x010C48,  EastAsian_N   },
    {  0x010C80,  0x010CB2,  EastAsian_N   },
    {  0x010CC0,  0x010CF2,  EastAsian_N   },
    {  0x010CFA,  0x010D27,  EastAsian_N   },
    {  0x010D30,  0x010D39,  EastAsian_N   },
    {  0x010E60,  0x010E7E,  EastAsian_N   },
    {  0x010E80,  0x010EA9,  EastAsian_N   },
    {  0x010EAB,  0x010EAD,  EastAsian_N   },
    {  0x010EB0,  0x010EB1,  EastAsian_N   },
    {  0x010F00,  0x010F27,  EastAsian_N   },
    {  0x010F30,  0x010F59,  EastAsian_N   },
    {  0x010F70,  0x010F89,  EastAsian_N   },
    {  0x010FB0,  0x010FCB,  EastAsian_N   },
    {  0x010FE0,  0x010FF6,  EastAsian_N   },
    {  0x011000,  0x01104D,  EastAsian_N   },
    {  0x011052,  0x011075,  EastAsian_N   },
    {  0x01107F,  0x0110C2,  EastAsian_N   },
    {  0x0110CD,  0x0110CD,  EastAsian_N   },
    {  0x0110D0,  0x0110E8,  EastAsian_N   },
    {  0x0110F0,  0x0110F9,  EastAsian_N   },
    {  0x011100,  0x011134,  EastAsian_N   },
    {  0x011136,  0x011147,  EastAsian_N   },
    {  0x011150,  0x011176,  EastAsian_N   },
    {  0x011180,  0x0111DF,  EastAsian_N   },
    {  0x0111E1,  0x0111F4,  EastAsian_N   },
    {  0x011200,  0x011211,  EastAsian_N   },
    {  0x011213,  0x01123E,  EastAsian_N   },
    {  0x011280,  0x011286,  EastAsian_N   },
    {  0x011288,  0x011288,  EastAsian_N   },
    {  0x01128A,  0x01128D,  EastAsian_N   },
    {  0x01128F,  0x01129D,  EastAsian_N   },
    {  0x01129F,  0x0112A9,  EastAsian_N   },
    {  0x0112B0,  0x0112EA,  EastAsian_N   },
    {  0x0112F0,  0x0112F9,  EastAsian_N   },
    {  0x011300,  0x011303,  EastAsian_N   },
    {  0x011305,  0x01130C,  EastAsian_N   },
    {  0x01130F,  0x011310,  EastAsian_N   },
    {  0x011313,  0x011328,  EastAsian_N   },
    {  0x01132A,  0x011330,  EastAsian_N   },
    {  0x011332,  0x011333,  EastAsian_N   },
    {  0x011335,  0x011339,  EastAsian_N   },
    {  0x01133B,  0x011344,  EastAsian_N   },
    {  0x011347,  0x011348,  EastAsian_N   },
    {  0x01134B,  0x01134D,  EastAsian_N   },
    {  0x011350,  0x011350,  EastAsian_N   },
    {  0x011357,  0x011357,  EastAsian_N   },
    {  0x01135D,  0x011363,  EastAsian_N   },
    {  0x011366,  0x01136C,  EastAsian_N   },
    {  0x011370,  0x011374,  EastAsian_N   },
    {  0x011400,  0x01145B,  EastAsian_N   },
    {  0x01145D,  0x011461,  EastAsian_N   },
    {  0x011480,  0x0114C7,  EastAsian_N   },
    {  0x0114D0,  0x0114D9,  EastAsian_N   },
    {  0x011580,  0x0115B5,  EastAsian_N   },
    {  0x0115B8,  0x0115DD,  EastAsian_N   },
    {  0x011600,  0x011644,  EastAsian_N   },
    {  0x011650,  0x011659,  EastAsian_N   },
    {  0x011660,  0x01166C,  EastAsian_N   },
    {  0x011680,  0x0116B9,  EastAsian_N   },
    {  0x0116C0,  0x0116C9,  EastAsian_N   },
    {  0x011700,  0x01171A,  EastAsian_N   },
    {  0x01171D,  0x01172B,  EastAsian_N   },
    {  0x011730,  0x011746,  EastAsian_N   },
    {  0x011800,  0x01183B,  EastAsian_N   },
    {  0x0118A0,  0x0118F2,  EastAsian_N   },
    {  0x0118FF,  0x011906,  EastAsian_N   },
    {  0x011909,  0x011909,  EastAsian_N   },
    {  0x01190C,  0x011913,  EastAsian_N   },
    {  0x011915,  0x011916,  EastAsian_N   },
    {  0x011918,  0x011935,  EastAsian_N   },
    {  0x011937,  0x011938,  EastAsian_N   },
    {  0x01193B,  0x011946,  EastAsian_N   },
    {  0x011950,  0x011959,  EastAsian_N   },
    {  0x0119A0,  0x0119A7,  EastAsian_N   },
    {  0x0119AA,  0x0119D7,  EastAsian_N   },
    {  0x0119DA,  0x0119E4,  EastAsian_N   },
    {  0x011A00,  0x011A47,  EastAsian_N   },
    {  0x011A50,  0x011AA2,  EastAsian_N   },
    {  0x011AB0,  0x011AF8,  EastAsian_N   },
    {  0x011C00,  0x011C08,  EastAsian_N   },
    {  0x011C0A,  0x011C36,  EastAsian_N   },
    {  0x011C38,  0x011C45,  EastAsian_N   },
    {  0x011C50,  0x011C6C,  EastAsian_N   },
    {  0x011C70,  0x011C8F,  EastAsian_N   },
    {  0x011C92,  0x011CA7,  EastAsian_N   },
    {  0x011CA9,  0x011CB6,  EastAsian_N   },
    {  0x011D00,  0x011D06,  EastAsian_N   },
    {  0x011D08,  0x011D09,  EastAsian_N   },
    {  0x011D0B,  0x011D36,  EastAsian_N   },
    {  0x011D3A,  0x011D3A,  EastAsian_N   },
    {  0x011D3C,  0x011D3D,  EastAsian_N   },
    {  0x011D3F,  0x011D47,  EastAsian_N   },
    {  0x011D50,  0x011D59,  EastAsian_N   },
    {  0x011D60,  0x011D65,  EastAsian_N   },
    {  0x011D67,  0x011D68,  EastAsian_N   },
    {  0x011D6A,  0x011D8E,  EastAsian_N   },
    {  0x011D90,  0x011D91,  EastAsian_N   },
    {  0x011D93,  0x011D98,  EastAsian_N   },
    {  0x011DA0,  0x011DA9,  EastAsian_N   },
    {  0x011EE0,  0x011EF8,  EastAsian_N   },
    {  0x011FB0,  0x011FB0,  EastAsian_N   },
    {  0x011FC0,  0x011FF1,  EastAsian_N   },
    {  0x011FFF,  0x012399,  EastAsian_N   },
    {  0x012400,  0x01246E,  EastAsian_N   },
    {  0x012470,  0x012474,  EastAsian_N   },
    {  0x012480,  0x012543,  EastAsian_N   },
    {  0x012F90,  0x012FF2,  EastAsian_N   },
    {  0x013000,  0x01342E,  EastAsian_N   },
    {  0x013430,  0x013438,  EastAsian_N   },
    {  0x014400,  0x014646,  EastAsian_N   },
    {  0x016800,  0x016A38,  EastAsian_N   },
    {  0x016A40,  0x016A5E,  EastAsian_N   },
    {  0x016A60,  0x016A69,  EastAsian_N   },
    {  0x016A6E,  0x016ABE,  EastAsian_N   },
    {  0x016AC0,  0x016AC9,  EastAsian_N   },
    {  0x016AD0,  0x016AED,  EastAsian_N   },
    {  0x016AF0,  0x016AF5,  EastAsian_N   },
    {  0x016B00,  0x016B45,  EastAsian_N   },
    {  0x016B50,  0x016B59,  EastAsian_N   },
    {  0x016B5B,  0x016B61,  EastAsian_N   },
    {  0x016B63,  0x016B77,  EastAsian_N   },
    {  0x016B7D,  0x016B8F,  EastAsian_N   },
    {  0x016E40,  0x016E9A,  EastAsian_N   },
    {  0x016F00,  0x016F4A,  EastAsian_N   },
    {  0x016F4F,  0x016F87,  EastAsian_N   },
    {  0x016F8F,  0x016F9F,  EastAsian_N   },
    {  0x016FE0,  0x016FE4,  EastAsian_W   },
    {  0x016FF0,  0x016FF1,  EastAsian_W   },
    {  0x017000,  0x0187F7,  EastAsian_W   },
    {  0x018800,  0x018CD5,  EastAsian_W   },
    {  0x018D00,  0x018D08,  EastAsian_W   },
    {  0x01AFF0,  0x01AFF3,  EastAsian_W   },
    {  0x01AFF5,  0x01AFFB,  EastAsian_W   },
    {  0x01AFFD,  0x01AFFE,  EastAsian_W   },
    {  0x01B000,  0x01B122,  EastAsian_W   },
    {  0x01B150,  0x01B152,  EastAsian_W   },
    {  0x01B164,  0x01B167,  EastAsian_W   },
    {  0x01B170,  0x01B2FB,  EastAsian_W   },
    {  0x01BC00,  0x01BC6A,  EastAsian_N   },
    {  0x01BC70,  0x01BC7C,  EastAsian_N   },
    {  0x01BC80,  0x01BC88,  EastAsian_N   },
    {  0x01BC90,  0x01BC99,  EastAsian_N   },
    {  0x01BC9C,  0x01BCA3,  EastAsian_N   },
    {  0x01CF00,  0x01CF2D,  EastAsian_N   },
    {  0x01CF30,  0x01CF46,  EastAsian_N   },
    {  0x01CF50,  0x01CFC3,  EastAsian_N   },
    {  0x01D000,  0x01D0F5,  EastAsian_N   },
    {  0x01D100,  0x01D126,  EastAsian_N   },
    {  0x01D129,  0x01D1EA,  EastAsian_N   },
    {  0x01D200,  0x01D245,  EastAsian_N   },
    {  0x01D2E0,  0x01D2F3,  EastAsian_N   },
    {  0x01D300,  0x01D356,  EastAsian_N   },
    {  0x01D360,  0x01D378,  EastAsian_N   },
    {  0x01D400,  0x01D454,  EastAsian_N   },
    {  0x01D456,  0x01D49C,  EastAsian_N   },
    {  0x01D49E,  0x01D49F,  EastAsian_N   },
    {  0x01D4A2,  0x01D4A2,  EastAsian_N   },
    {  0x01D4A5,  0x01D4A6,  EastAsian_N   },
    {  0x01D4A9,  0x01D4AC,  EastAsian_N   },
    {  0x01D4AE,  0x01D4B9,  EastAsian_N   },
    {  0x01D4BB,  0x01D4BB,  EastAsian_N   },
    {  0x01D4BD,  0x01D4C3,  EastAsian_N   },
    {  0x01D4C5,  0x01D505,  EastAsian_N   },
    {  0x01D507,  0x01D50A,  EastAsian_N   },
    {  0x01D50D,  0x01D514,  EastAsian_N   },
    {  0x01D516,  0x01D51C,  EastAsian_N   },
    {  0x01D51E,  0x01D539,  EastAsian_N   },
    {  0x01D53B,  0x01D53E,  EastAsian_N   },
    {  0x01D540,  0x01D544,  EastAsian_N   },
    {  0x01D546,  0x01D546,  EastAsian_N   },
    {  0x01D54A,  0x01D550,  EastAsian_N   },
    {  0x01D552,  0x01D6A5,  EastAsian_N   },
    {  0x01D6A8,  0x01D7CB,  EastAsian_N   },
    {  0x01D7CE,  0x01DA8B,  EastAsian_N   },
    {  0x01DA9B,  0x01DA9F,  EastAsian_N   },
    {  0x01DAA1,  0x01DAAF,  EastAsian_N   },
    {  0x01DF00,  0x01DF1E,  EastAsian_N   },
    {  0x01E000,  0x01E006,  EastAsian_N   },
    {  0x01E008,  0x01E018,  EastAsian_N   },
    {  0x01E01B,  0x01E021,  EastAsian_N   },
    {  0x01E023,  0x01E024,  EastAsian_N   },
    {  0x01E026,  0x01E02A,  EastAsian_N   },
    {  0x01E100,  0x01E12C,  EastAsian_N   },
    {  0x01E130,  0x01E13D,  EastAsian_N   },
    {  0x01E140,  0x01E149,  EastAsian_N   },
    {  0x01E14E,  0x01E14F,  EastAsian_N   },
    {  0x01E290,  0x01E2AE,  EastAsian_N   },
    {  0x01E2C0,  0x01E2F9,  EastAsian_N   },
    {  0x01E2FF,  0x01E2FF,  EastAsian_N   },
    {  0x01E7E0,  0x01E7E6,  EastAsian_N   },
    {  0x01E7E8,  0x01E7EB,  EastAsian_N   },
    {  0x01E7ED,  0x01E7EE,  EastAsian_N   },
    {  0x01E7F0,  0x01E7FE,  EastAsian_N   },
    {  0x01E800,  0x01E8C4,  EastAsian_N   },
    {  0x01E8C7,  0x01E8D6,  EastAsian_N   },
    {  0x01E900,  0x01E94B,  EastAsian_N   },
    {  0x01E950,  0x01E959,  EastAsian_N   },
    {  0x01E95E,  0x01E95F,  EastAsian_N   },
    {  0x01EC71,  0x01ECB4,  EastAsian_N   },
    {  0x01ED01,  0x01ED3D,  EastAsian_N   },
    {  0x01EE00,  0x01EE03,  EastAsian_N   },
    {  0x01EE05,  0x01EE1F,  EastAsian_N   },
    {  0x01EE21,  0x01EE22,  EastAsian_N   },
    {  0x01EE24,  0x01EE24,  EastAsian_N   },
    {  0x01EE27,  0x01EE27,  EastAsian_N   },
    {  0x01EE29,  0x01EE32,  EastAsian_N   },
    {  0x01EE34,  0x01EE37,  EastAsian_N   },
    {  0x01EE39,  0x01EE39,  EastAsian_N   },
    {  0x01EE3B,  0x01EE3B,  EastAsian_N   },
    {  0x01EE42,  0x01EE42,  EastAsian_N   },
    {  0x01EE47,  0x01EE47,  EastAsian_N   },
    {  0x01EE49,  0x01EE49,  EastAsian_N   },
    {  0x01EE4B,  0x01EE4B,  EastAsian_N   },
    {  0x01EE4D,  0x01EE4F,  EastAsian_N   },
    {  0x01EE51,  0x01EE52,  EastAsian_N   },
    {  0x01EE54,  0x01EE54,  EastAsian_N   },
    {  0x01EE57,  0x01EE57,  EastAsian_N   },
    {  0x01EE59,  0x01EE59,  EastAsian_N   },
    {  0x01EE5B,  0x01EE5B,  EastAsian_N   },
    {  0x01EE5D,  0x01EE5D,  EastAsian_N   },
    {  0x01EE5F,  0x01EE5F,  EastAsian_N   },
    {  0x01EE61,  0x01EE62,  EastAsian_N   },
    {  0x01EE64,  0x01EE64,  EastAsian_N   },
    {  0x01EE67,  0x01EE6A,  EastAsian_N   },
    {  0x01EE6C,  0x01EE72,  EastAsian_N   },
    {  0x01EE74,  0x01EE77,  EastAsian_N   },
    {  0x01EE79,  0x01EE7C,  EastAsian_N   },
    {  0x01EE7E,  0x01EE7E,  EastAsian_N   },
    {  0x01EE80,  0x01EE89,  EastAsian_N   },
    {  0x01EE8B,  0x01EE9B,  EastAsian_N   },
    {  0x01EEA1,  0x01EEA3,  EastAsian_N   },
    {  0x01EEA5,  0x01EEA9,  EastAsian_N   },
    {  0x01EEAB,  0x01EEBB,  EastAsian_N   },
    {  0x01EEF0,  0x01EEF1,  EastAsian_N   },
    {  0x01F000,  0x01F003,  EastAsian_N   },
    {  0x01F004,  0x01F004,  EastAsian_W   },
    {  0x01F005,  0x01F02B,  EastAsian_N   },
    {  0x01F030,  0x01F093,  EastAsian_N   },
    {  0x01F0A0,  0x01F0AE,  EastAsian_N   },
    {  0x01F0B1,  0x01F0BF,  EastAsian_N   },
    {  0x01F0C1,  0x01F0CE,  EastAsian_N   },
    {  0x01F0CF,  0x01F0CF,  EastAsian_W   },
    {  0x01F0D1,  0x01F0F5,  EastAsian_N   },
    {  0x01F100,  0x01F10A,  EastAsian_A   },
    {  0x01F10B,  0x01F10F,  EastAsian_N   },
    {  0x01F110,  0x01F12D,  EastAsian_A   },
    {  0x01F12E,  0x01F12F,  EastAsian_N   },
    {  0x01F130,  0x01F169,  EastAsian_A   },
    {  0x01F16A,  0x01F16F,  EastAsian_N   },
    {  0x01F170,  0x01F18D,  EastAsian_A   },
    {  0x01F18E,  0x01F18E,  EastAsian_W   },
    {  0x01F18F,  0x01F190,  EastAsian_A   },
    {  0x01F191,  0x01F19A,  EastAsian_W   },
    {  0x01F19B,  0x01F1AC,  EastAsian_A   },
    {  0x01F1AD,  0x01F1AD,  EastAsian_N   },
    {  0x01F1E6,  0x01F1FF,  EastAsian_N   },
    {  0x01F200,  0x01F202,  EastAsian_W   },
    {  0x01F210,  0x01F23B,  EastAsian_W   },
    {  0x01F240,  0x01F248,  EastAsian_W   },
    {  0x01F250,  0x01F251,  EastAsian_W   },
    {  0x01F260,  0x01F265,  EastAsian_W   },
    {  0x01F300,  0x01F320,  EastAsian_W   },
    {  0x01F321,  0x01F32C,  EastAsian_N   },
    {  0x01F32D,  0x01F335,  EastAsian_W   },
    {  0x01F336,  0x01F336,  EastAsian_N   },
    {  0x01F337,  0x01F37C,  EastAsian_W   },
    {  0x01F37D,  0x01F37D,  EastAsian_N   },
    {  0x01F37E,  0x01F393,  EastAsian_W   },
    {  0x01F394,  0x01F39F,  EastAsian_N   },
    {  0x01F3A0,  0x01F3CA,  EastAsian_W   },
    {  0x01F3CB,  0x01F3CE,  EastAsian_N   },
    {  0x01F3CF,  0x01F3D3,  EastAsian_W   },
    {  0x01F3D4,  0x01F3DF,  EastAsian_N   },
    {  0x01F3E0,  0x01F3F0,  EastAsian_W   },
    {  0x01F3F1,  0x01F3F3,  EastAsian_N   },
    {  0x01F3F4,  0x01F3F4,  EastAsian_W   },
    {  0x01F3F5,  0x01F3F7,  EastAsian_N   },
    {  0x01F3F8,  0x01F43E,  EastAsian_W   },
    {  0x01F43F,  0x01F43F,  EastAsian_N   },
    {  0x01F440,  0x01F440,  EastAsian_W   },
    {  0x01F441,  0x01F441,  EastAsian_N   },
    {  0x01F442,  0x01F4FC,  EastAsian_W   },
    {  0x01F4FD,  0x01F4FE,  EastAsian_N   },
    {  0x01F4FF,  0x01F53D,  EastAsian_W   },
    {  0x01F53E,  0x01F54A,  EastAsian_N   },
    {  0x01F54B,  0x01F54E,  EastAsian_W   },
    {  0x01F54F,  0x01F54F,  EastAsian_N   },
    {  0x01F550,  0x01F567,  EastAsian_W   },
    {  0x01F568,  0x01F579,  EastAsian_N   },
    {  0x01F57A,  0x01F57A,  EastAsian_W   },
    {  0x01F57B,  0x01F594,  EastAsian_N   },
    {  0x01F595,  0x01F596,  EastAsian_W   },
    {  0x01F597,  0x01F5A3,  EastAsian_N   },
    {  0x01F5A4,  0x01F5A4,  EastAsian_W   },
    {  0x01F5A5,  0x01F5FA,  EastAsian_N   },
    {  0x01F5FB,  0x01F64F,  EastAsian_W   },
    {  0x01F650,  0x01F67F,  EastAsian_N   },
    {  0x01F680,  0x01F6C5,  EastAsian_W   },
    {  0x01F6C6,  0x01F6CB,  EastAsian_N   },
    {  0x01F6CC,  0x01F6CC,  EastAsian_W   },
    {  0x01F6CD,  0x01F6CF,  EastAsian_N   },
    {  0x01F6D0,  0x01F6D2,  EastAsian_W   },
    {  0x01F6D3,  0x01F6D4,  EastAsian_N   },
    {  0x01F6D5,  0x01F6D7,  EastAsian_W   },
    {  0x01F6DD,  0x01F6DF,  EastAsian_W   },
    {  0x01F6E0,  0x01F6EA,  EastAsian_N   },
    {  0x01F6EB,  0x01F6EC,  EastAsian_W   },
    {  0x01F6F0,  0x01F6F3,  EastAsian_N   },
    {  0x01F6F4,  0x01F6FC,  EastAsian_W   },
    {  0x01F700,  0x01F773,  EastAsian_N   },
    {  0x01F780,  0x01F7D8,  EastAsian_N   },
    {  0x01F7E0,  0x01F7EB,  EastAsian_W   },
    {  0x01F7F0,  0x01F7F0,  EastAsian_W   },
    {  0x01F800,  0x01F80B,  EastAsian_N   },
    {  0x01F810,  0x01F847,  EastAsian_N   },
    {  0x01F850,  0x01F859,  EastAsian_N   },
    {  0x01F860,  0x01F887,  EastAsian_N   },
    {  0x01F890,  0x01F8AD,  EastAsian_N   },
    {  0x01F8B0,  0x01F8B1,  EastAsian_N   },
    {  0x01F900,  0x01F90B,  EastAsian_N   },
    {  0x01F90C,  0x01F93A,  EastAsian_W   },
    {  0x01F93B,  0x01F93B,  EastAsian_N   },
    {  0x01F93C,  0x01F945,  EastAsian_W   },
    {  0x01F946,  0x01F946,  EastAsian_N   },
    {  0x01F947,  0x01F9FF,  EastAsian_W   },
    {  0x01FA00,  0x01FA53,  EastAsian_N   },
    {  0x01FA60,  0x01FA6D,  EastAsian_N   },
    {  0x01FA70,  0x01FA74,  EastAsian_W   },
    {  0x01FA78,  0x01FA7C,  EastAsian_W   },
    {  0x01FA80,  0x01FA86,  EastAsian_W   },
    {  0x01FA90,  0x01FAAC,  EastAsian_W   },
    {  0x01FAB0,  0x01FABA,  EastAsian_W   },
    {  0x01FAC0,  0x01FAC5,  EastAsian_W   },
    {  0x01FAD0,  0x01FAD9,  EastAsian_W   },
    {  0x01FAE0,  0x01FAE7,  EastAsian_W   },
    {  0x01FAF0,  0x01FAF6,  EastAsian_W   },
    {  0x01FB00,  0x01FB92,  EastAsian_N   },
    {  0x01FB94,  0x01FBCA,  EastAsian_N   },
    {  0x01FBF0,  0x01FBF9,  EastAsian_N   },
    {  0x020000,  0x02FFFD,  EastAsian_W   },
    {  0x030000,  0x03FFFD,  EastAsian_W   },
    {  0x0E0001,  0x0E0001,  EastAsian_N   },
    {  0x0E0020,  0x0E007F,  EastAsian_N   },
    {  0x0E0100,  0x0E01EF,  EastAsian_A   },
    {  0x0F0000,  0x0FFFFD,  EastAsian_A   },
    {  0x100000,  0x10FFFD,  EastAsian_A   }
};

const enum EastAsianType EastAsianAscii[0x80] = {
    EastAsian_N, EastAsian_N, EastAsian_N, EastAsian_N,
    EastAsian_N, EastAsian_N, EastAsian_N, EastAsian_N,
    EastAsian_N, EastAsian_N, EastAsian_N, EastAsian_N,
    EastAsian_N, EastAsian_N, EastAsian_N, EastAsian_N,
    EastAsian_N, EastAsian_N, EastAsian_N, EastAsian_N,
    EastAsian_N, EastAsian_N, EastAsian_N, EastAsian_N,
    EastAsian_N, EastAsian_N, EastAsian_N, EastAsian_N,
    EastAsian_N, EastAsian_N, EastAsian_N, EastAsian_N,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_NA,
    EastAsian_NA, EastAsian_NA, EastAsian_NA, EastAsian_N
};


/************************************************************
 *  eastasian_unicode.c
 ************************************************************/

static int eastasian_check(const struct eastasian_struct *str, unicode x)
{
	return (str->a <= x) && (x <= str->b);
}

static enum EastAsianType eastasian_search(unicode x, size_t ai, size_t bi)
{
	size_t ci;
	const struct eastasian_struct *a, *b, *c;

	Check(EastAsianTable_Size <= ai, "size error: ai");
	Check(EastAsianTable_Size <= bi, "size error: bi");
	ci = (ai + bi) / 2;
	a = EastAsianTable + ai;
	b = EastAsianTable + bi;
	c = EastAsianTable + ci;
	if (eastasian_check(a, x))
		return a->c;
	if (eastasian_check(b, x))
		return b->c;
	if (bi <= ai)
		return EastAsian_error;
	if (x < c->a)
		return eastasian_search(x, ai + 1UL, ci);
	else
		return eastasian_search(x, ci, bi - 1UL);
}

enum EastAsianType eastasian_symbol(unicode x)
{
	if (x < 0x80)
		return EastAsianAscii[x];
	else
		return eastasian_search(x, 0, EastAsianTable_Size - 1UL);
}

unsigned eastasian_width(unicode x)
{
	return EastAsianSymbol[eastasian_symbol(x)];
}

void init_eastasian(void)
{
	EastAsianSymbol[EastAsian_error] = 0;
	EastAsianSymbol[EastAsian_N] = 1;
	EastAsianSymbol[EastAsian_A] = 2;
	EastAsianSymbol[EastAsian_H] = 1;
	EastAsianSymbol[EastAsian_W] = 2;
	EastAsianSymbol[EastAsian_F] = 2;
	EastAsianSymbol[EastAsian_NA] = 1;
}


/************************************************************
 *  encode.c
 ************************************************************/

typedef int (*read_char_calltype)(filestream , unicode *, int *);
typedef int (*read_hang_calltype)(filestream , unicode *, int *, int *);
typedef int (*write_char_calltype)(filestream , unicode);
typedef int (*length_char_calltype)(filestream , unicode);
static read_char_calltype read_char_call[EncodeType_size];
static read_hang_calltype read_hang_call[EncodeType_size];
static write_char_calltype write_char_call[EncodeType_size];
static length_char_calltype length_char_call[EncodeType_size];

/*
 *  Byte Order Mark
 */
static int readbom8_encode_filememory(filestream fm)
{
	byte c;
	int check;

	check = getc_filememory(fm, &c);
	if (check < 0)
		goto invalid;
	if (check)
		goto empty;
	if (c != 0xEF) {
		if (ungetc_filememory(fm, c))
			goto invalid; /* ungetc error */
		else
			goto empty; /* ok */
	}
	if (getc_filememory(fm, &c))
		goto invalid; /* invalid bom sequence */
	if (c != 0xBB)
		goto invalid;
	if (getc_filememory(fm, &c))
		goto invalid;
	if (c != 0xBF)
		goto invalid;
	goto exist;

invalid:
	return -1;
empty:
	return 0;
exist:
	return 1;
}

static int readbom16_encode_filememory(filestream fm)
{
	byte a, b;
	int check;

	/* 1:le, 2:be */
	check = getc_filememory(fm, &a);
	if (check < 0)
		goto invalid;
	if (check)
		goto empty;
	if (getc_filememory(fm, &b))
		goto invalid;

	if (a == 0xFF && b == 0xFE)
		goto little_endian;
	if (a == 0xFE && b == 0xFF)
		goto big_endian;

	if (ungetc_filememory(fm, b))
		goto invalid;
	if (ungetc_filememory(fm, a))
		goto invalid;
	goto empty;

invalid:
	return -1;
empty:
	return 0;
little_endian:
	return 1;
big_endian:
	return 2;
}

static int readbom32_encode_filememory(filestream fm)
{
	byte a, b, c, d;
	int check;

	/* 1:le, 2:be */
	check = getc_filememory(fm, &a);
	if (check < 0)
		goto invalid;
	if (check)
		goto empty;
	if (getc_filememory(fm, &b))
		goto invalid;
	if (getc_filememory(fm, &c))
		goto invalid;
	if (getc_filememory(fm, &d))
		goto invalid;

	if (a == 0xFF && b == 0xFE && c == 0x00 && d == 0x00)
		goto little_endian;
	if (a == 0x00 && b == 0x00 && c == 0xFE && d == 0xFF)
		goto big_endian;

	if (ungetc_filememory(fm, d))
		goto invalid;
	if (ungetc_filememory(fm, c))
		goto invalid;
	if (ungetc_filememory(fm, b))
		goto invalid;
	if (ungetc_filememory(fm, a))
		goto invalid;
	goto empty;

invalid:
	return -1;
empty:
	return 0;
little_endian:
	return 1;
big_endian:
	return 2;
}

static filestream begin_readbom_buffering(addr stream, addr *ret)
{
	filestream fm;
	addr mem;

	Check(! file_stream_p(stream), "type error");
	fm = PtrFileMemory(stream);
	if (fm->redirect) {
		GetPathnameStream(stream, &mem);
		Check(! memory_stream_p(mem), "type error");
		*ret = fm->pos;
		fm->pos = mem;
	}
	else {
		*ret = Nil;
	}

	return fm;
}

static void end_readbom_buffering(addr stream, addr prev)
{
	filestream fm;

	Check(! file_stream_p(stream), "type error");
	fm = PtrFileMemory(stream);
	if (fm->redirect) {
		fm->pos = prev;
	}
}

int readbom8_encode(addr stream)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_readbom_buffering(stream, &prev);
	check = readbom8_encode_filememory(fm);
	end_readbom_buffering(stream, prev);

	return check;
}

int readbom16_encode(addr stream)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_readbom_buffering(stream, &prev);
	check = readbom16_encode_filememory(fm);
	end_readbom_buffering(stream, prev);

	return check;
}

int readbom32_encode(addr stream)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_readbom_buffering(stream, &prev);
	check = readbom32_encode_filememory(fm);
	end_readbom_buffering(stream, prev);

	return check;
}

int writebom_encode_(addr stream)
{
	return write_char_stream_(stream, 0xFEFF);
}


/*
 *  read_char
 */
static int read_char_binary_(filestream fm, unicode *u, int *ret)
{
	return fmte_("Cannot execute read-char in binary stream.", NULL);
}

static int read_char_ascii_(filestream fm, unicode *u, int *ret)
{
	byte c;
	int check;

	check = getc_filememory(fm, &c);
	if (check < 0)
		return fmte_("getc error", NULL);
	if (check)
		return Result(ret, 1);
	if (0x80 <= c) {
		if (fm->encode.error)
			return fmte_("encode error", NULL);
		*u = fm->encode.code;
		return Result(ret, 0);
	}
	*u = (unicode)c;

	return Result(ret, 0);
}

static int read_char_utf8_(filestream fm, unicode *u, int *ret)
{
	int check;

	check = read_utf8_normal(fm, u);
	/* normal */
	if (check == 0)
		return Result(ret, 0);
	/* EOF */
	if (0 < check)
		return Result(ret, 1);
	/* file error */
	if (check == -1)
		return fmte_("read error.", NULL);
	/* encode error */
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	/* recovery */
	*u = fm->encode.code;

	return Result(ret, 0);
}

static int read_char_utf16_(filestream fm, unicode *u, int be, int *ret)
{
	int check;

	check = read_utf16_normal(fm, u, be);
	/* normal */
	if (check == 0)
		return Result(ret, 0);
	/* EOF */
	if (0 < check)
		return Result(ret, 1);
	/* file error */
	if (check == -1)
		return fmte_("read error.", NULL);
	/* encode error */
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	/* recovery */
	*u = fm->encode.code;

	return Result(ret, 0);
}

static int read_char_utf16le_(filestream fm, unicode *u, int *ret)
{
	return read_char_utf16_(fm, u, 0, ret);
}

static int read_char_utf16be_(filestream fm, unicode *u, int *ret)
{
	return read_char_utf16_(fm, u, 1, ret);
}

static int read_char_utf32_(filestream fm, unicode *u, int be, int *ret)
{
	int check;

	check = read_utf32_normal(fm, u, be);
	/* normal */
	if (check == 0)
		return Result(ret, 0);
	/* EOF */
	if (0 < check)
		return Result(ret, 1);
	/* file error */
	if (check == -1)
		return fmte_("read error.", NULL);
	/* encode error */
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	/* recovery */
	*u = fm->encode.code;

	return Result(ret, 0);
}

static int read_char_utf32le_(filestream fm, unicode *u, int *ret)
{
	return read_char_utf32_(fm, u, 0, ret);
}

static int read_char_utf32be_(filestream fm, unicode *u, int *ret)
{
	return read_char_utf32_(fm, u, 1, ret);
}

static int read_char_windows_(filestream fm, unicode *u, int *ret)
{
	return fmte_("Invalid external-format :windows.", NULL);
}

static void init_encode_read_char(void)
{
	read_char_call[EncodeType_binary] = read_char_binary_;
	read_char_call[EncodeType_ascii] = read_char_ascii_;
	read_char_call[EncodeType_utf8] = read_char_utf8_;
	read_char_call[EncodeType_utf16le] = read_char_utf16le_;
	read_char_call[EncodeType_utf16be] = read_char_utf16be_;
	read_char_call[EncodeType_utf32le] = read_char_utf32le_;
	read_char_call[EncodeType_utf32be] = read_char_utf32be_;
	read_char_call[EncodeType_windows] = read_char_windows_;
}

int read_char_encode_(filestream fm, unicode *c, int *ret)
{
	return (read_char_call[(int)fm->encode.type])(fm, c, ret);
}


/*
 *  read_hang
 */
static int read_hang_binary_(filestream fm, unicode *u, int *hang, int *ret)
{
	*u = 0;
	*hang = 0;
	*ret = 0;
	return fmte_("Cannot execute read-char-no-hang in binary stream.", NULL);
}

static int read_hang_ascii_(filestream fm, unicode *u, int *hang, int *ret)
{
	byte c;
	int check;
	size_t size;

	check = getc_nonblock_filememory(fm, &c, &size);
	if (check < 0) {
		*u = 0;
		*hang = 0;
		*ret = 0;
		return fmte_("getc_nonblock error", NULL);
	}
	if (check) {
		*u = 0;
		*hang = 0;
		return Result(ret, 1);
	}
	if (size == 0) {
		*u = 0;
		*hang = 1;
		return Result(ret, 0);
	}
	if (0x80 <= c) {
		if (fm->encode.error) {
			*u = 0;
			*hang = 0;
			*ret = 0;
			return fmte_("read error.", NULL);
		}
		*u = fm->encode.code;
		*hang = 0;
		return Result(ret, 0);
	}
	*u = (unicode)c;
	*hang = 0;
	return Result(ret, 0);
}

static int read_hang_utf8_(filestream fm, unicode *u, int *hang, int *ret)
{
	int check;

	check = read_utf8_nonblock(fm, u, hang);
	/* normal */
	if (check == 0)
		return Result(ret, 0);
	/* EOF */
	if (0 < check) {
		*u = 0;
		*hang = 0;
		return Result(ret, 1);
	}
	/* file error */
	if (check == -1) {
		*u = 0;
		*hang = 0;
		*ret = 0;
		return fmte_("read error.", NULL);
	}
	/* encode error */
	if (fm->encode.error) {
		*u = 0;
		*hang = 0;
		*ret = 0;
		return fmte_("encode error.", NULL);
	}
	/* recovery */
	*u = fm->encode.code;
	*hang = 0;
	return Result(ret, 0);
}

static int read_hang_utf16_(filestream fm,
		unicode *u, int *hang, int *ret, int be)
{
	int check;

	check = read_utf16_nonblock(fm, u, hang, be);
	/* normal */
	if (check == 0)
		return Result(ret, 0);
	/* EOF */
	if (0 < check) {
		*u = 0;
		*hang = 0;
		return Result(ret, 1);
	}
	/* file error */
	if (check == -1) {
		*u = 0;
		*hang = 0;
		*ret = 0;
		return fmte_("read error.", NULL);
	}
	/* encode error */
	if (fm->encode.error) {
		*u = 0;
		*hang = 0;
		*ret = 0;
		return fmte_("encode error.", NULL);
	}
	/* recovery */
	*u = fm->encode.code;
	*hang = 0;
	return Result(ret, 0);
}

static int read_hang_utf16le_(filestream fm, unicode *u, int *hang, int *ret)
{
	return read_hang_utf16_(fm, u, hang, ret, 0);
}

static int read_hang_utf16be_(filestream fm, unicode *u, int *hang, int *ret)
{
	return read_hang_utf16_(fm, u, hang, ret, 1);
}

static int read_hang_utf32_(filestream fm,
		unicode *u, int *hang, int *ret, int be)
{
	int check;

	check = read_utf32_nonblock(fm, u, hang, be);
	/* normal */
	if (check == 0)
		return Result(ret, 0);
	/* EOF */
	if (0 < check) {
		*u = 0;
		*hang = 0;
		return Result(ret, 1);
	}
	/* file error */
	if (check == -1) {
		*u = 0;
		*hang = 0;
		*ret = 0;
		return fmte_("read error.", NULL);
	}
	/* encode error */
	if (fm->encode.error) {
		*u = 0;
		*hang = 0;
		*ret = 0;
		return fmte_("encode error.", NULL);
	}
	/* recovery */
	*u = fm->encode.code;
	*hang = 0;
	return Result(ret, 0);
}

static int read_hang_utf32le_(filestream fm, unicode *u, int *hang, int *ret)
{
	return read_hang_utf32_(fm, u, hang, ret, 0);
}

static int read_hang_utf32be_(filestream fm, unicode *u, int *hang, int *ret)
{
	return read_hang_utf32_(fm, u, hang, ret, 1);
}

static int read_hang_windows_(filestream fm, unicode *u, int *hang, int *ret)
{
	*u = 0;
	*hang = 0;
	*ret = 0;
	return fmte_("Invalid external-format :windows.", NULL);
}

static void init_encode_read_hang(void)
{
	read_hang_call[EncodeType_binary] = read_hang_binary_;
	read_hang_call[EncodeType_ascii] = read_hang_ascii_;
	read_hang_call[EncodeType_utf8] = read_hang_utf8_;
	read_hang_call[EncodeType_utf16le] = read_hang_utf16le_;
	read_hang_call[EncodeType_utf16be] = read_hang_utf16be_;
	read_hang_call[EncodeType_utf32le] = read_hang_utf32le_;
	read_hang_call[EncodeType_utf32be] = read_hang_utf32be_;
	read_hang_call[EncodeType_windows] = read_hang_windows_;
}

int read_hang_encode_(filestream fm, unicode *c, int *hang, int *ret)
{
	return (read_hang_call[(int)fm->encode.type])(fm, c, hang, ret);
}


/*
 *  write_char
 */
static int write_char_binary_(filestream fm, unicode u)
{
	return fmte_("Cannot execute write-char in binary stream.", NULL);
}

static int write_char_ascii_(filestream fm, unicode u)
{
	if (u < 0x80) {
		if (putc_filememory(fm, (byte)u))
			return fmte_("putc error.", NULL);
		return 0;
	}
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	u = fm->encode.code;
	if (u < 0x80) {
		if (putc_filememory(fm, (byte)u))
			return fmte_("putc error.", NULL);
		return 0;
	}

	return fmte_("encode error.", NULL);
}

static int write_char_utf8_(filestream fm, unicode u)
{
	byte data[8];
	size_t size;

	if (! encode_utf8(u, data, &size)) {
		if (write_filememory(fm, data, size, &size))
			return fmte_("write error.", NULL);
		return 0;
	}
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	if (! encode_utf8(fm->encode.code, data, &size)) {
		if (write_filememory(fm, data, size, &size))
			return fmte_("write error.", NULL);
		return 0;
	}

	return fmte_("encode error.", NULL);
}

static int write_char_utf16_(filestream fm, unicode u, int be)
{
	byte data[8];
	size_t size;

	if (! encode_utf16(u, be, data, &size)) {
		if (write_filememory(fm, data, size, &size))
			return fmte_("write error.", NULL);
		return 0;
	}
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	if (! encode_utf16(fm->encode.code, be, data, &size)) {
		if (write_filememory(fm, data, size, &size))
			return fmte_("write error.", NULL);
		return 0;
	}

	return fmte_("encode error.", NULL);
}

static int write_char_utf16le_(filestream fm, unicode u)
{
	return write_char_utf16_(fm, u, 0);
}

static int write_char_utf16be_(filestream fm, unicode u)
{
	return write_char_utf16_(fm, u, 1);
}

static int write_char_utf32_(filestream fm, unicode u, int be)
{
	byte data[8];
	size_t size;

	if (! encode_utf32(u, be, data, &size)) {
		if (write_filememory(fm, data, size, &size))
			return fmte_("write error.", NULL);
		return 0;
	}
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	if (! encode_utf32(fm->encode.code, be, data, &size)) {
		if (write_filememory(fm, data, size, &size))
			return fmte_("write error.", NULL);
		return 0;
	}

	return fmte_("encode error.", NULL);
}

static int write_char_utf32le_(filestream fm, unicode u)
{
	return write_char_utf32_(fm, u, 0);
}

static int write_char_utf32be_(filestream fm, unicode u)
{
	return write_char_utf32_(fm, u, 1);
}

#if defined(LISP_ANSIC_WINDOWS)
static int write_char_windows_(filestream fm, unicode u)
{
	char output[8];
	wchar_t input[4];
	byte *ptr, c;
	size_t size;

	/* unicode -> UTF16 */
	if (! encode_utf16b(u, (byte16 *)input, &size))
		goto normal;
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	if (! encode_utf16b(fm->encode.code, (byte16 *)input, &size))
		goto normal;
	return fmte_("encode error.", NULL);

	/* UTF16 -> ANSI-code-page  */
normal:
	input[size] = 0;
	snprintf(output, 8, "%S", input);
	if (output[0] == 0)
		return fmte_("encode error.", NULL);
	for (ptr = (byte *)output; ; ptr++) {
		c = *ptr;
		if (c == 0)
			break;
		if (putc_filememory(fm, c))
			return fmte_("putc error.", NULL);
	}

	return 0;
}

#elif (defined LISP_WINDOWS)
static int write_char_windows_(filestream fm, unicode u)
{
	char output[8];
	wchar_t input[4];
	byte *ptr;
	int result, i;
	size_t size;

	/* unicode -> UTF16 */
	if (! encode_utf16b(u, (byte16 *)input, &size))
		goto normal;
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	if (! encode_utf16b(fm->encode.code, (byte16 *)input, &size))
		goto normal;
	return fmte_("encode error.", NULL);

	/* UTF16 -> ANSI-code-page  */
normal:
	input[size] = 0;
	result = WideCharToMultiByte(CP_THREAD_ACP,
			0,
			(LPCWSTR)input,
			(int)size,
			(LPSTR)output,
			8,
			NULL,
			NULL);
	if (result == 0)
		return fmte_("encode error.", NULL);
	ptr = (byte *)output;
	for (i = 0; i < result; i++) {
		if (putc_filememory(fm, ptr[i]))
			return fmte_("putc error.", NULL);
	}

	return 0;
}

#else
static int write_char_windows_(filestream fm, unicode u)
{
	return fmte_("This implementation cannot write a windows encode.", NULL);
}
#endif

static void init_encode_write_char(void)
{
	write_char_call[EncodeType_binary] = write_char_binary_;
	write_char_call[EncodeType_ascii] = write_char_ascii_;
	write_char_call[EncodeType_utf8] = write_char_utf8_;
	write_char_call[EncodeType_utf16le] = write_char_utf16le_;
	write_char_call[EncodeType_utf16be] = write_char_utf16be_;
	write_char_call[EncodeType_utf32le] = write_char_utf32le_;
	write_char_call[EncodeType_utf32be] = write_char_utf32be_;
	write_char_call[EncodeType_windows] = write_char_windows_;
}

int write_char_encode_(filestream fm, unicode u)
{
	return (write_char_call[(int)fm->encode.type])(fm, u);
}


/*
 *  length-char
 */
static int length_char_binary(filestream fm, unicode c)
{
	return -1;
}

static int length_char_ascii(filestream fm, unicode c)
{
	return (c < 0x80)? 1: -1;
}

static int length_unicode_utf8(unicode c, int *ret)
{
	if (c < 0x80)       { *ret = 1; return 0; }
	if (c < 0x0800)     { *ret = 2; return 0; }
	if (c < 0xD800)     { *ret = 3; return 0; }
	if (c < 0xE000)     { *ret = 0; return 1; } /* error */
	if (c < 0x010000)   { *ret = 3; return 0; }
#ifdef LISP_UTF8_SEQ5CHECK
	if (c < UnicodeCount) { *ret = 4; return 0; }
#else
	if (c < 0x200000)   { *ret = 4; return 0; }
	if (c < 0x04000000) { *ret = 5; return 0; }
	if (c < 0x80000000) { *ret = 6; return 0; }
#endif
	*ret = 0;
	return 1;
}

static int length_char_utf8(filestream fm, unicode c)
{
	int size;
	return length_unicode_utf8(c, &size)? -1: size;
}

static int length_unicode_utf16(unicode c, int *ret)
{
	if (c < 0xD800) { *ret = 1; return 0; }
	if (c < 0xE000) { *ret = 0; return 1; } /* surrogate pair */
	if (c < 0x010000) { *ret = 1; return 0; }
	if (c < UnicodeCount) { *ret = 2; return 0; }
	return 1;
}

static int length_char_utf16(filestream fm, unicode c)
{
	int size;
	return length_unicode_utf16(c, &size)? -1: (size * 2);
}

static int length_char_utf32(filestream fm, unicode c)
{
	return 4;
}

#if defined(LISP_ANSIC_WINDOWS)
static int length_char_windows(filestream fm, unicode c)
{
	char output[8];
	wchar_t input[4];
	size_t size;

	/* unicode -> UTF16 */
	if (! encode_utf16b(u, (byte16 *)input, &size))
		goto normal;
	if (fm->encode.error)
		return -1;
	if (! encode_utf16b(fm->encode.code, (byte16 *)input, &size))
		goto normal;
	return -1;

	/* UTF16 -> ANSI-code-page  */
normal:
	input[size] = 0;
	snprintf(output, 8, "%S", input);
	if (output[0] == 0)
		return -1;
	return (int)strlen(output);
}
#elif defined(LISP_WINDOWS)
static int length_char_windows(filestream fm, unicode c)
{
	char output[8];
	wchar_t input[4];
	int result;
	size_t size;

	/* unicode -> UTF16 */
	if (! encode_utf16b(c, (byte16 *)input, &size))
		goto normal;
	if (fm->encode.error)
		return -1;
	if (! encode_utf16b(fm->encode.code, (byte16 *)input, &size))
		goto normal;
	return -1;

	/* UTF16 -> ANSI-code-page  */
normal:
	input[size] = 0;
	result = WideCharToMultiByte(CP_THREAD_ACP,
			0,
			(LPCWSTR)input,
			(int)size,
			(LPSTR)output,
			8,
			NULL,
			NULL);
	if (result == 0)
		return -1;
	return (int)strlen(output);
}
#else
static int length_char_windows(filestream fm, unicode c)
{
	/* return fmte_("This implementation cannot use a windows encode.", NULL); */
	return -1;
}
#endif

static void init_encode_length_char(void)
{
	length_char_call[EncodeType_binary] = length_char_binary;
	length_char_call[EncodeType_ascii] = length_char_ascii;
	length_char_call[EncodeType_utf8] = length_char_utf8;
	length_char_call[EncodeType_utf16le] = length_char_utf16;
	length_char_call[EncodeType_utf16be] = length_char_utf16;
	length_char_call[EncodeType_utf32le] = length_char_utf32;
	length_char_call[EncodeType_utf32be] = length_char_utf32;
	length_char_call[EncodeType_windows] = length_char_windows;
}

static int length_char_operator(filestream fm, unicode c)
{
	/* Surrogate pair */
	if (UTF16range(c))
		return -1;
	/* Invalid unicode */
#ifdef LISP_UTF8_SEQ5CHECK
	if (UnicodeCount <= c)
		return -1;
#endif
	/* Length */
	return (length_char_call[(int)fm->encode.type])(fm, c);
}

int length_char_encode(filestream fm, unicode c)
{
	int check;

	check = length_char_operator(fm, c);
	if (check < 0 && fm->encode.error == 0)
		return length_char_operator(fm, fm->encode.code);
	else
		return check;
}

int length_string_encode_(filestream fm, addr pos, size_t *rsize, int *ret)
{
	int check;
	unicode c;
	size_t i, size, count;

	string_length(pos, &size);
	for (count = i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		check = length_char_encode(fm, c);
		if (check < 0)
			return Result(ret, 1);
		count += (size_t)check;
	}
	*rsize = count;

	return Result(ret, 0);
}


/*
 *  unicode buffer
 */
int UTF32_length_utf8(const unicode *ptr, size_t size, size_t *ret)
{
	int check;
	unicode c;
	size_t i, w;

	w = 0;
	for (i = 0; i < size; i++) {
		c = ptr[i];
		if (length_unicode_utf8(c, &check))
			return 1;
		w += (size_t)check;
	}
	*ret = w;

	return 0;
}

int UTF8_length_clang_(addr pos, size_t *rsize, int *ret)
{
	int check;
	unicode c;
	size_t size, i, w;

	string_length(pos, &size);
	w = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (length_unicode_utf8(c, &check)) {
			*rsize = 0;
			return Result(ret, 1);
		}
		w += (size_t)check;
	}
	*rsize = w;
	return Result(ret, 0);
}

int UTF32_length_utf16(const unicode *ptr, size_t size, size_t *ret)
{
	int check;
	unicode c;
	size_t i, w;

	w = 0;
	for (i = 0; i < size; i++) {
		c = ptr[i];
		if (length_unicode_utf16(c, &check))
			return 1;
		w += (size_t)check;
	}
	*ret = w;

	return 0;
}

int UTF16_length_clang_(addr pos, size_t *rsize, int *ret)
{
	int check;
	unicode c;
	size_t size, i, w;

	string_length(pos, &size);
	w = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (length_unicode_utf16(c, &check)) {
			*rsize = 0;
			return Result(ret, 1);
		}
		w += (size_t)check;
	}
	*rsize = w;
	return Result(ret, 0);
}

static int UTF8_make_(byte *dst, addr pos, int *ret)
{
	size_t size, i, w, check;
	unicode u;

	string_length(pos, &size);
	w = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (encode_utf8(u, dst + w, &check))
			return Result(ret, 1);
		w += check;
	}
	dst[w] = 0;

	return Result(ret, 0);
}

static int UTF16_make_(byte16 *dst, addr pos, int *ret)
{
	size_t size, i, w, check;
	unicode u;

	string_length(pos, &size);
	w = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (encode_utf16b(u, dst + w, &check))
			return Result(ret, 1);
		w += check;
	}
	dst[w++] = 0;
	dst[w] = 0;

	return Result(ret, 0);
}

int UTF8_buffer_clang_(LocalRoot local, addr *ret, addr string)
{
	int check;
	addr pos, body;
	size_t size;

	Return(UTF8_length_clang_(string, &size, &check));
	if (check)
		return Result(ret, Unbound);
	local_body(local, &pos, LISPSYSTEM_UNICODE, size + 1);
	posbody(pos, &body);
	Return(UTF8_make_((byte *)body, string, &check));
	if (check)
		return Result(ret, Unbound);

	return Result(ret, pos);
}

int UTF16_buffer_clang_(LocalRoot local, addr *ret, addr string)
{
	int check;
	addr pos, body;
	size_t size;

	Return(UTF16_length_clang_(string, &size, &check));
	if (check)
		return Result(ret, Unbound);
	local_body(local, &pos, LISPSYSTEM_UNICODE, (size + 1) * 2);
	posbody(pos, &body);
	Return(UTF16_make_((byte16 *)body, string, &check));
	if (check)
		return Result(ret, Unbound);

	return Result(ret, pos);
}


/*
 *  unicode string
 */
int UTF8_null_strlen(const byte *src, size_t *ret)
{
	return read_utf8_buffer(NULL, src, 0, ret);
}

int UTF8_size_strlen(const byte *src, size_t size, size_t *ret)
{
	if (size == 0) {
		*ret = 0;
		return 0;
	}

	return read_utf8_buffer(NULL, src, size, ret);
}

int UTF8_null_makeunicode(unicode *dst, const byte *src)
{
	return read_utf8_buffer(dst, src, 0, NULL);
}

int UTF8_size_makeunicode(unicode *dst, const byte *src, size_t size)
{
	if (size == 0)
		return 0;

	return read_utf8_buffer(dst, src, size, NULL);
}

int UTF16_null_strlen(const byte16 *src, size_t *ret)
{
	size_t count;
	byte16 c;

	for (count = 0; ; count++) {
		c = *(src++);
		if (c == 0)
			break;
		if (UTF16low(c))
			return 1;
		if (UTF16high(c)) {
			c = *(src++);
			if (! UTF16low(c))
				return 1;
		}
	}
	*ret = count;

	return 0;
}

int UTF16_size_strlen(const byte16 *src, size_t size, size_t *ret)
{
	size_t count, i;
	byte16 c;

	i = 0;
	count = 0;
	while (i < size) {
		c = src[i];
		if (UTF16low(c))
			return 1;
		if (UTF16high(c)) {
			i++;
			if (size <= i)
				return 1;
			c = src[i];
			if (! UTF16low(c))
				return 1;
		}
		i++;
		count++;
	}
	*ret = count;

	return 0;
}

int UTF16_null_makeunicode(unicode *dst, const byte16 *src)
{
	byte16 a, b;

	for (;;) {
		a = *(src++);
		if (a == 0)
			break;
		if (UTF16low(a))
			return 1;
		if (UTF16high(a)) {
			b = *(src++);
			if (! UTF16low(b))
				return 1;
			*(dst++) = UTF16unicode(a, b);
		}
		else {
			*(dst++) = (unicode)a;
		}
	}

	return 0;
}

int UTF16_size_makeunicode(unicode *dst, const byte16 *src, size_t size)
{
	size_t i;
	byte16 a, b;

	for (i = 0; i < size; i++) {
		a = src[i];
		if (UTF16low(a))
			return 1;
		if (UTF16high(a)) {
			i++;
			if (size <= i)
				return 1;
			b = src[i];
			if (! UTF16low(b))
				return 1;
			*(dst++) = UTF16unicode(a, b);
		}
		else {
			*(dst++) = (unicode)a;
		}
	}

	return 0;
}

int UTF32_null_strlen(const unicode *src, size_t *ret)
{
	unicode c;
	size_t i;

	for (i = 0; ; i++) {
		c = src[i];
		if (c == 0)
			break;
		if (UnicodeCount <= c)
			return 1;
		if (isSurrogatePair(c))
			return 1;
	}

	*ret = i;
	return 0;
}

int UTF32_size_strlen(const unicode *src, size_t size, size_t *ret)
{
	unicode c;
	size_t i;

	for (i = 0; i < size; i++) {
		c = src[i];
		if (UnicodeCount <= c)
			return 1;
		if (isSurrogatePair(c))
			return 1;
	}

	*ret = size;
	return 0;
}

int UTF32_null_makeunicode(unicode *dst, const unicode *src)
{
	unicode c;
	size_t i;

	for (i = 0; ; i++) {
		c = src[i];
		if (c == 0)
			break;
		if (UnicodeCount <= c)
			return 1;
		if (isSurrogatePair(c))
			return 1;
		dst[i] = c;
	}

	return 0;
}

int UTF32_size_makeunicode(unicode *dst, const unicode *src, size_t size)
{
	unicode c;
	size_t i;

	for (i = 0; ; i++) {
		c = src[i];
		if (c == 0)
			break;
		if (UnicodeCount <= c)
			return 1;
		if (isSurrogatePair(c))
			return 1;
		dst[i] = c;
	}

	return 0;
}

int UTF32_make_utf8(byte *dst, const unicode *src, size_t size)
{
	byte data[8];
	size_t i, ret;

	for (i = 0; i < size; i += ret) {
		if (encode_utf8(src[i], data, &ret))
			return 1;
		if (size < i + ret)
			return 1;
		memcpy(dst + i, data, ret);
	}

	return 0;
}

int UTF32_make_utf16(byte16 *dst, const unicode *src, size_t size)
{
	byte16 data[4];
	size_t i, ret;

	for (i = 0; i < size; i += ret) {
		if (encode_utf16b(src[i], data, &ret))
			return 1;
		if (size < i + ret)
			return 1;
		memcpy(dst + i, data, ret * 2UL);
	}

	return 0;
}


/*
 *  initialize
 */
void init_encode(void)
{
	init_encode_read_char();
	init_encode_read_hang();
	init_encode_write_char();
	init_encode_length_char();
}


/************************************************************
 *  encode_unicode.c
 ************************************************************/

enum read_unicode_result {
	read_unicode_result_normal,
	read_unicode_result_error,
	read_unicode_result_end,
	read_unicode_result_rollback
};

#define READ_UNICODE_DATA 8
struct read_unicode_struct {
	byte data[READ_UNICODE_DATA];
	filestream fm;
	int hang;
	unsigned rollback;
	const byte *src;
	size_t size, index;
};

static int read_unicode_rollback(struct read_unicode_struct *ptr)
{
	while (ptr->rollback) {
		ptr->rollback--;
		if (ungetc_filememory(ptr->fm, ptr->data[ptr->rollback]))
			return 1;
	}

	return 0;
}

static void read_unicode_push(struct read_unicode_struct *ptr, byte c)
{
	Check(READ_UNICODE_DATA <= ptr->rollback, "rollback error");
	ptr->data[ptr->rollback++] = c;
}


/*
 *  read-utf8
 */
#define read_utf8_macro() { \
	result = getcall(ptr, &c); \
	if (result == read_unicode_result_normal) { \
		read_unicode_push(ptr, c); \
	} \
	else if (result == read_unicode_result_end) { \
		goto file_error; \
	} \
	else if (result == read_unicode_result_rollback) { \
		goto rollback; \
	} \
	else { \
		goto unicode_error; \
	} \
}
static int read_utf8_call(struct read_unicode_struct *ptr,
		unicode *ret,
		enum read_unicode_result (*getcall)(struct read_unicode_struct *, byte *))
{
	byte c;
	enum read_unicode_result result;
	unicode value;

	result = getcall(ptr, &c);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, c);
	else if (result == read_unicode_result_end)
		goto end_of_file;
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	if (0x00 <= c && c <= 0x7F)
		goto sequence1;
	if (0xC2 <= c && c <= 0xDF)
		goto sequence2;
	if (0xE0 <= c && c <= 0xEF)
		goto sequence3;
	if (0xF0 <= c && c <= 0xF7)
		goto sequence4;
	if (0xF8 <= c && c <= 0xFB)
		goto sequence5;
	if (0xFC <= c && c <= 0xFD)
		goto sequence6;
	goto unicode_error;

sequence1:
	value = (unicode)c;
	goto normal;

sequence2:
	value = (0x1F & c) << 6;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c)
		goto unicode_error;
	value |= 0x3F & c;
	if (value < 0x80)
		goto range_error;
	goto normal;

sequence3:
	value = (0x0F & c) << 12;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c)
		goto unicode_error;
	value |= (0x3F & c) << 6;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c)
		goto unicode_error;
	value |= 0x3F & c;
	if (value < 0x0800)
		goto range_error;
	if (UTF16range(value))
		goto surrogate_error;
	goto normal;

sequence4:
	value = (0x07 & c) << 18;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c)
		goto unicode_error;
	value |= (0x3F & c) << 12;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c)
		goto unicode_error;
	value |= (0x3F & c) << 6;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c)
		goto unicode_error;
	value |= 0x3F & c;
	if (value < 0x010000)
		goto range_error;
#ifdef LISP_UTF8_SEQ5CHECK
	if (UnicodeCount <= value)
		goto range_error;
#endif
	goto normal;

sequence5:
#ifdef LISP_UTF8_SEQ5CHECK
	goto sequence_error;
#else
	value = (0x03 & c) << 24;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c)
		goto unicode_error;
	value |= (0x3F & c) << 18;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c)
		goto unicode_error;
	value |= (0x3F & c) << 12;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c)
		goto unicode_error;
	value |= (0x3F & c) << 6;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c)
		goto unicode_error;
	value |= 0x3F & c;
	if (value < 0x00200000 || 0x03FFFFFF < value)
		goto range_error;
	goto normal;
#endif

sequence6:
#ifdef LISP_UTF8_SEQ5CHECK
	goto sequence_error;
#else
	value = (0x01 & c) << 30;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c)
		goto unicode_error;
	value |= (0x3F & c) << 24;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c)
		goto unicode_error;
	value |= (0x3F & c) << 18;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c)
		goto unicode_error;
	value |= (0x3F & c) << 12;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c)
		goto unicode_error;
	value |= (0x3F & c) << 6;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c)
		goto unicode_error;
	value |= 0x3F & c;
	if (value < 0x04000000 || 0x7FFFFFFF < value)
		goto range_error;
	goto normal;
#endif

normal:
	*ret = value;
	return 0;

file_error:
	return -1;
unicode_error:
	return -2;
range_error:
	return -3;
#ifdef LISP_UTF8_SEQ5CHECK
sequence_error:
	return -4;
#endif
surrogate_error:
	return -5;
end_of_file:
	return 1;

rollback:
	if (read_unicode_rollback(ptr))
		goto file_error;
	ptr->hang = 1;
	return 0;
}


/* read_utf8_normal */
static enum read_unicode_result getc_utf8_normal(
		struct read_unicode_struct *ptr, byte *ret)
{
	int check;

	check = getc_filememory(ptr->fm, ret);
	if (check == 0)
		return read_unicode_result_normal;
	else if (0 < check)
		return read_unicode_result_end;
	else
		return read_unicode_result_error;
}

int read_utf8_normal(filestream fm, unicode *ret)
{
	struct read_unicode_struct str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	str.fm = fm;
	str.rollback = 0;

	return read_utf8_call(&str, ret, getc_utf8_normal);
}


/* read_utf8_nonblock */
static enum read_unicode_result getc_utf8_nonblock(
		struct read_unicode_struct *ptr, byte *ret)
{
	int check;
	size_t size;

	check = getc_nonblock_filememory(ptr->fm, ret, &size);
	if (check == 0)
		return size?
			read_unicode_result_normal:
			read_unicode_result_rollback;
	else if (0 < check)
		return read_unicode_result_end;
	else
		return read_unicode_result_error;
}

int read_utf8_nonblock(filestream fm, unicode *ret, int *hang)
{
	int check;
	struct read_unicode_struct str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	str.fm = fm;
	str.rollback = 0;
	str.hang = 0;

	check = read_utf8_call(&str, ret, getc_utf8_nonblock);
	*hang = str.hang;

	return check;
}


/* read_utf8_buffer */
static enum read_unicode_result getc_utf8_buffer(
		struct read_unicode_struct *ptr, byte *ret)
{
	if (ptr->size == 0) {
		*ret = ptr->src[ptr->index++];
		return *ret?
			read_unicode_result_normal:
			read_unicode_result_end;
	}
	else if (ptr->index < ptr->size) {
		*ret = ptr->src[ptr->index++];
		return read_unicode_result_normal;
	}
	else {
		return read_unicode_result_end;
	}
}

int read_utf8_buffer(unicode *dst, const byte *src, size_t size, size_t *ret)
{
	int check;
	struct read_unicode_struct str;
	unicode u;
	size_t count;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	str.index = 0;
	for (count = 0; ; count++) {
		str.rollback = 0;
		str.src = src;
		str.size = size;

		check = read_utf8_call(&str, &u, getc_utf8_buffer);
		if (check < 0)
			return check;
		if (check)
			break;
		if (dst)
			dst[count] = u;
	}
	if (ret)
		*ret = count;

	return 0;
}


/*
 *  read-utf16
 */
static int read_utf16_call(struct read_unicode_struct *ptr,
		unicode *ret,
		int big_endian_p,
		enum read_unicode_result (*getcall)(struct read_unicode_struct *, byte *))
{
	byte a, b;
	enum read_unicode_result result;
	uint16_t c, d;

	/* 1 byte */
	result = getcall(ptr, &a);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, a);
	else if (result == read_unicode_result_end)
		goto end_of_file;
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	/* 2 byte */
	result = getcall(ptr, &b);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, b);
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;
	if (big_endian_p)
		c = (a << 8) | b;
	else
		c = a | (b << 8);

	/* 1 character */
	if (UTF16low(c))
		goto unicode_error;
	if (! UTF16high(c)) {
		*ret = c;
		goto normal;
	}

	/* 3 byte */
	result = getcall(ptr, &a);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, a);
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	/* 4 byte */
	result = getcall(ptr, &b);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, b);
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	/* surrogate pair */
	if (big_endian_p)
		d = (a << 8) | b;
	else
		d = a | (b << 8);
	if (! UTF16low(d))
		goto unicode_error;
	*ret = UTF16unicode(c, d);
	goto normal;

normal:
	return 0;
file_error:
	return -1;
unicode_error:
	return -2;
end_of_file:
	return 1;

rollback:
	if (read_unicode_rollback(ptr))
		goto file_error;
	ptr->hang = 1;
	return 0;
}


/* read_utf16_normal */
int read_utf16_normal(filestream fm, unicode *ret, int be)
{
	struct read_unicode_struct str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	str.fm = fm;
	str.rollback = 0;

	return read_utf16_call(&str, ret, be, getc_utf8_normal);
}


/* read_utf16_nonblock */
int read_utf16_nonblock(filestream fm, unicode *ret, int *hang, int be)
{
	int check;
	struct read_unicode_struct str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	str.fm = fm;
	str.rollback = 0;
	str.hang = 0;

	check = read_utf16_call(&str, ret, be, getc_utf8_nonblock);
	*hang = str.hang;

	return check;
}


/*
 *  read-utf32
 */
static int read_utf32_call(struct read_unicode_struct *ptr,
		unicode *ret,
		int big_endian_p,
		enum read_unicode_result (*getcall)(struct read_unicode_struct *, byte *))
{
	byte a, b, c, d;
	enum read_unicode_result result;
	unicode value;

	/* 1 byte */
	result = getcall(ptr, &a);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, a);
	else if (result == read_unicode_result_end)
		goto end_of_file;
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	/* 2 byte */
	result = getcall(ptr, &b);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, b);
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	/* 3 byte */
	result = getcall(ptr, &c);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, c);
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	/* 4 byte */
	result = getcall(ptr, &d);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, d);
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	/* unicode */
	if (big_endian_p)
		value = (a << 24) | (b << 16) | (c << 8) | d;
	else
		value = (d << 24) | (c << 16) | (b << 8) | a;
	if (UTF16range(value))
		goto range_error;
#ifdef LISP_UTF8_SEQ5CHECK
	if (UnicodeCount <= value)
		goto range_error;
#endif
	*ret = value;
	return 0;

file_error:
	return -1;
range_error:
	return -2;
end_of_file:
	return 1;

rollback:
	if (read_unicode_rollback(ptr))
		goto file_error;
	ptr->hang = 1;
	return 0;
}


/* read_utf32_normal */
int read_utf32_normal(filestream fm, unicode *ret, int be)
{
	struct read_unicode_struct str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	str.fm = fm;
	str.rollback = 0;

	return read_utf32_call(&str, ret, be, getc_utf8_normal);
}


/* read_utf32_nonblock */
int read_utf32_nonblock(filestream fm, unicode *ret, int *hang, int be)
{
	int check;
	struct read_unicode_struct str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	str.fm = fm;
	str.rollback = 0;
	str.hang = 0;

	check = read_utf32_call(&str, ret, be, getc_utf8_nonblock);
	*hang = str.hang;

	return check;
}


/*
 *  encode-utf8
 */
int encode_utf8(unicode u, byte *dst, size_t *ret)
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
#ifdef LISP_UTF8_SEQ5CHECK
	if (u < UnicodeCount) {
		dst[w++] = 0xF0 | (u >> 18);
		dst[w++] = 0x80 | (0x3F & (u >> 12));
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
#else
	if (u < 0x200000) {
		dst[w++] = 0xF0 | (u >> 18);
		dst[w++] = 0x80 | (0x3F & (u >> 12));
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
	if (u < 0x04000000) {
		dst[w++] = 0xF8 | (u >> 24);
		dst[w++] = 0x80 | (0x3F & (u >> 18));
		dst[w++] = 0x80 | (0x3F & (u >> 12));
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
	if (u < 0x80000000) {
		dst[w++] = 0xFC | (u >> 30);
		dst[w++] = 0x80 | (0x3F & (u >> 24));
		dst[w++] = 0x80 | (0x3F & (u >> 18));
		dst[w++] = 0x80 | (0x3F & (u >> 12));
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
#endif
	/* error */

error:
	return 1;

normal:
	*ret = w;
	return 0;
}


/*
 *  encode-utf16
 */
int encode_utf16a(unicode u, byte16 *surrogate, byte16 *code)
{
	/* 1 byte, 2 byte */
	if (u < 0xD800) {
		*surrogate = 0;
		*code = (byte16)u;
		return 0;
	}
	/* surrogate pair */
	if (u < 0xE000) {
		return 1; /* error */
	}
	/* 2 byte */
	if (u < 0x010000) {
		*surrogate = 0;
		*code = (byte16)u;
		return 0;
	}
	/* 4 byte */
	if (u < UnicodeCount) {
		*code = 0xDC00 | (0x03FF & u);
		u = ((((u >> 16) & 0x1F) - 1) << 6) | (0x3F & (u >> 10));
		*surrogate = 0xD800 | u;
		return 0;
	}

	/* error */
	return 1;
}

int encode_utf16b(unicode u, byte16 *dst, size_t *ret)
{
	byte16 surrogate, code;

	if (encode_utf16a(u, &surrogate, &code))
		return 1;
	if (surrogate == 0) {
		dst[0] = code;
		*ret = 1;
	}
	else {
		dst[0] = surrogate;
		dst[1] = code;
		*ret = 2;
	}

	return 0;
}

int encode_utf16(unicode u, int big_endian_p, byte *dst, size_t *ret)
{
	byte16 high, code;
	unsigned i;

	if (encode_utf16a(u, &high, &code))
		return 1;
	i = 0;
	if (big_endian_p) {
		if (high) {
			dst[i++] = 0xFF & (high >> 8);
			dst[i++] = 0xFF & high;
		}
		dst[i++] = 0xFF & (code >> 8);
		dst[i++] = 0xFF & code;
	}
	else {
		if (high) {
			dst[i++] = 0xFF & high;
			dst[i++] = 0xFF & (high >> 8);
		}
		dst[i++] = 0xFF & code;
		dst[i++] = 0xFF & (code >> 8);
	}
	*ret = (size_t)i;

	return 0;
}


/*
 *  encode-utf32
 */
int encode_utf32check(unicode u)
{
	if (UTF16range(u))
		return 1;
#ifdef LISP_UTF8_SEQ5CHECK
	if (UnicodeCount <= u)
		return 1;
#endif
	return 0;
}

int encode_utf32(unicode u, int big_endian_p, byte *dst, size_t *ret)
{
	unsigned i;

	if (encode_utf32check(u))
		return 1;
	i = 0;
	if (big_endian_p) {
		dst[i++] = 0xFF & (u >> 24);
		dst[i++] = 0xFF & (u >> 16);
		dst[i++] = 0xFF & (u >> 8);
		dst[i++] = 0xFF & u;
	}
	else {
		dst[i++] = 0xFF & u;
		dst[i++] = 0xFF & (u >> 8);
		dst[i++] = 0xFF & (u >> 16);
		dst[i++] = 0xFF & (u >> 24);
	}
	*ret = (size_t)i;

	return 0;
}


/************************************************************
 *  env_code.c
 ************************************************************/

#define Fmt1(x) Return(format_stream_(ptr, stream, x, NULL))
#define Fmt2(x,y) Return(format_stream_(ptr, stream, x, y, NULL))

/*
 *  disassemble
 */
static int disassemble_code_(Execute ptr, addr stream, addr code);

static int disassmeble_code_p(addr car, addr cdr, addr *ret)
{
	addr check;

	/* lambda */
	GetConst(CODE_LAMBDA, &check);
	if (car == check) {
		*ret = cdr;
		return 1;
	}

	/* macro */
	GetConst(CODE_MACRO, &check);
	if (car == check) {
		*ret = cdr;
		return 1;
	}

	/* labels-lambda */
	GetConst(CODE_LABELS_LAMBDA, &check);
	if (car == check) {
		GetCdr(cdr, &cdr);
		GetCar(cdr, ret);
		return 1;
	}

	return 0;
}

static int disassemble_code_operator_(Execute ptr,
		size_t i, addr stream, addr car, addr cdr)
{
	addr index;

	/* otherwise */
	make_index_integer_heap(&index, i);
	Return(format_stream_(ptr, stream, "~5@A: ~20A ~S~%", index, car, cdr, NULL));

	/* execute */
	if (disassmeble_code_p(car, cdr, &cdr))
		return disassemble_code_(ptr, stream, cdr);

	return 0;
}

static int disassemble_code_body_(Execute ptr, addr stream, addr pos)
{
	addr car, cdr;
	size_t size, i;

	getarray_code(pos, &pos);
	lenarray(pos, &size);
	for (i = 0; i < size; i++) {
		getarray(pos, i, &car);
		GetCons(car, &car, &cdr);
		Return(disassemble_code_operator_(ptr, i, stream, car, cdr));
	}

	return 0;
}

static int disassemble_code_(Execute ptr, addr stream, addr code)
{
	if (code == Nil)
		return 0;
	/* code */
	CheckType(code, LISPTYPE_CODE);
	Return(format_stream_(ptr, stream, "CODE-BEGIN: ~A~20T~%", code, NULL));
	Return(disassemble_code_body_(ptr, stream, code));
	Return(format_stream_(ptr, stream, "CODE-END: ~A~%", code, NULL));

	return 0;
}

static int disassemble_interpreted_(Execute ptr, addr stream, addr pos)
{
	addr code;

	CheckType(pos, LISPTYPE_FUNCTION);
	GetCodeFunction(pos, &code);
	return disassemble_code_(ptr, stream, code);
}

static int disassemble_function_(Execute ptr, addr stream, addr pos)
{
	CheckType(stream, LISPTYPE_STREAM);
	CheckType(pos, LISPTYPE_FUNCTION);
	if (compiled_function_p(pos)) {
		Fmt2("Cannot disassemble COMPILED-FUNCTION ~S.~2%", pos);
	}
	else {
		Fmt2("INTERPRETED-FUNCTION ~S.~2%", pos);
		Return(disassemble_interpreted_(ptr, stream, pos));
	}

	return 0;
}

int disassemble_common_(Execute ptr, addr var)
{
	addr stream, check;

	Return(standard_output_stream_(ptr, &stream));
	if (symbolp(var)) {
		GetFunctionSymbol(var, &check);
		if (check == Unbound) {
			getmacro_symbol(var, &check);
			if (check == Unbound)
				return fmte_("Invalid argument ~S.", var, NULL);
		}
		var = check;
	}

	return disassemble_function_(ptr, stream, var);
}


/*
 *  trace
 */
int trace_common_(addr form, addr env, addr *ret)
{
	addr pos, value, list, add, quote;

	Return_getcdr(form, &form);

	/* all trace name */
	if (form == Nil) {
		GetConst(SYSTEM_TRACE_LIST, ret);
		return 0;
	}

	/* add trace */
	for (list = Nil; form != Nil; ) {
		if (! consp(form)) {
			Return(fmtw_("TRACE arguemnt ~S don't set a dotted list.", form, NULL));
			break;
		}
		GetCons(form, &pos, &form);
		if (parse_callname_heap(&value, pos)) {
			Return(fmtw_("TRACE argument ~S should be a function-name.", pos, NULL));
			continue;
		}
		cons_heap(&list, pos, list);
	}
	nreverse(&list, list);
	/* (lisp-system::trace-add 'list) */
	GetConst(SYSTEM_TRACE_ADD, &add);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&list, quote, list, NULL);
	list_heap(ret, add, list, NULL);

	return 0;
}


/*
 *  untrace
 */
int untrace_common_(addr form, addr env, addr *ret)
{
	addr pos, value, list, del, quote;

	Return_getcdr(form, &form);

	/* all delete */
	if (form == Nil) {
		GetConst(SYSTEM_TRACE_DEL, &pos);
		list_heap(ret, pos, T, NULL);
		return 0;
	}

	/* del trace */
	for (list = Nil; form != Nil; ) {
		if (! consp(form)) {
			Return(fmtw_("TRACE arguemnt ~S don't set a dotted list.", form, NULL));
			break;
		}
		GetCons(form, &pos, &form);
		if (parse_callname_heap(&value, pos)) {
			Return(fmtw_("TRACE argument ~S should be a function-name.", pos, NULL));
			continue;
		}
		cons_heap(&list, pos, list);
	}
	nreverse(&list, list);
	/* (lisp-system::trace-del 'list) */
	GetConst(SYSTEM_TRACE_DEL, &del);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&list, quote, list, NULL);
	list_heap(ret, del, list, NULL);

	return 0;
}


/*
 *  trace-add
 */
static int defun_trace_function_index_(Execute ptr, addr *ret)
{
	addr symbol, pos;

	GetConst(SYSTEM_TRACE_DEPTH, &symbol);
	getspecial_local(ptr, symbol, &pos);
	if (pos == Unbound) {
		fixnum_heap(&pos, 1);
	}
	else {
		Return(oneplus_integer_common_(ptr->local, pos, &pos));
	}
	pushspecial_control(ptr, symbol, pos);

	return Result(ret, pos);
}

static int defun_trace_function(Execute ptr, addr rest)
{
	int normalp;
	addr list, name, pos, index, stream;
	addr control, save;

	/* index */
	getdata_control(ptr, &list);
	GetCons(list, &name, &pos);
	Return(defun_trace_function_index_(ptr, &index));
	/* begin */
	Return(trace_output_stream_(ptr, &stream));
	cons_heap(&list, name, rest);
	Return(format_stream_(ptr, stream, "~&~A: ~S~%", index, list, NULL));
	/* call */
	push_control(ptr, &control);
	(void)apply_control_(ptr, pos, rest);
	normalp = (ptr->throw_value == throw_normal);
	/* end */
	save_execute_control(ptr, &save);
	normal_throw_control(ptr);
	if (normalp) {
		getvalues_list_control_heap(ptr, &list);
		Return(format_stream_(ptr, stream, "~&~A: Result => ~S~%", index, list, NULL));
	}
	else {
		Return(format_stream_(ptr, stream, "~&~A: Exit~%", index, NULL));
	}
	restore_execute_control(ptr, save);
	return pop_control_(ptr, control);
}

static void trace_add_make(Execute ptr, addr name, addr call, addr pos, addr *ret)
{
	addr type, trace;

	compiled_heap(&trace, call);
	setcompiled_rest(trace, p_defun_trace_function);
	gettype_function(pos, &type);
	settype_function(trace, type);
	settrace_function(trace);
	cons_heap(&name, name, pos);
	SetDataFunction(trace, name);
	*ret = trace;
}

static int trace_add_function_(Execute ptr, addr name, addr call, int *ret)
{
	addr pos;

	/* callname */
	if (GetStatusReadOnly(call)) {
		Return(fmtw_("The function ~S is constant.", call, NULL));
		return Result(ret, 1); /* error */
	}

	/* function */
	getglobal_callname(call, &pos);
	if (pos == Unbound) {
		Return(fmtw_("The function ~S is unbound.", call, NULL));
		return Result(ret, 1); /* error */
	}
	if (tracep_function(pos)) {
		Return(fmtw_("The function ~S is already traced.", pos, NULL));
		return Result(ret, 0); /* normal */
	}

	/* trade-add */
	trace_add_make(ptr, name, call, pos, &pos);
	Return(setglobal_callname_(call, pos));

	return Result(ret, 0); /* normal */
}

static int trace_add_push_(Execute ptr, addr name)
{
	addr symbol, list;

	GetConst(SYSTEM_TRACE_LIST, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	Return(pushnew_equal_heap_(list, name, &list));
	setspecial_local(ptr, symbol, list);

	return 0;
}

int trace_add_common_(Execute ptr, addr list, addr *ret)
{
	int check;
	addr root, name, pos;

	for (root = Nil; list != Nil; ) {
		GetCons(list, &name, &list);
		Return(parse_callname_error_(&pos, name));
		Return(trace_add_function_(ptr, name, pos, &check));
		if (check)
			continue;
		cons_heap(&root, name, root);
		Return(trace_add_push_(ptr, name));
	}
	nreverse(ret, root);

	return 0;
}


/*
 *  trace-del
 */
static void trace_del_object(Execute ptr, addr trace, addr *ret)
{
	GetDataFunction(trace, &trace);
	GetCdr(trace, ret);
}

static int trace_del_function_(Execute ptr, addr name, addr call, int *ret)
{
	addr pos;

	/* callname */
	if (GetStatusReadOnly(call)) {
		Return(fmtw_("The function ~S is constant.", call, NULL));
		return Result(ret, 1); /* error */
	}

	/* function */
	getglobal_callname(call, &pos);
	if (pos == Unbound) {
		Return(fmtw_("The function ~S is unbound.", call, NULL));
		return Result(ret, 1); /* error */
	}
	if (! tracep_function(pos)) {
		Return(fmtw_("The function ~S is not traced.", pos, NULL));
		return Result(ret, 0); /* normal */
	}

	/* trade-del */
	trace_del_object(ptr, pos, &pos);
	Return(setglobal_callname_(call, pos));

	return Result(ret, 0); /* normal */
}

static int trace_del_remove_(Execute ptr, addr name)
{
	int check;
	addr symbol, list;

	GetConst(SYSTEM_TRACE_LIST, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	Return(delete_list_equal_unsafe_(name, list, &list, &check));
	if (! check)
		return fmtw_("There is no function ~S in *trace-list*", name, NULL);
	setspecial_local(ptr, symbol, list);

	return 0;
}

int trace_del_common_(Execute ptr, addr list, addr *ret)
{
	int check;
	addr root, name, pos;

	/* all */
	if (list == T) {
		GetConst(SYSTEM_TRACE_LIST, &list);
		Return(getspecialcheck_local_(ptr, list, &list));
	}

	/* list */
	for (root = Nil; list != Nil; ) {
		GetCons(list, &name, &list);
		Return(parse_callname_error_(&pos, name));
		Return(trace_del_function_(ptr, name, pos, &check));
		if (check)
			continue;
		cons_heap(&root, name, root);
		Return(trace_del_remove_(ptr, name));
	}
	nreverse(ret, root);

	return 0;
}


/*
 *  initialize
 */
void init_environment_code(void)
{
	SetPointerType(rest, defun_trace_function);
}

void build_environment_code(void)
{
	addr symbol;
	GetConst(SYSTEM_TRACE_LIST, &symbol);
	SetValueSymbol(symbol, Nil);
}


/************************************************************
 *  env_describe.c
 ************************************************************/

/*
 *  t
 */
static int method_describe_object_t_(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	Return(format_stream_(ptr, stream, "Lisp Object: ~S~%", pos, NULL));
	setresult_control(ptr, Nil);
	return 0;
}


/*
 *  class
 */
static int method_describe_object_class_(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	Return(format_stream_(ptr, stream, "Class: ~S~%", pos, NULL));
	setresult_control(ptr, Nil);
	return 0;
}


/*
 *  standard-object
 */
static int method_describe_object_standard_object_(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	Return(format_stream_(ptr, stream, "Instance: ~S~%", pos, NULL));
	setresult_control(ptr, Nil);
	return 0;
}


/*
 *  structure-object
 */
static int method_describe_object_structure_object_(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	Return(format_stream_(ptr, stream, "Structure: ~S~%", pos, NULL));
	setresult_control(ptr, Nil);
	return 0;
}


/*
 *  defmethod
 */
static void method_type_describe_object(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Stream);
	typeargs_var2(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static int defmethod_describe_object_(Execute ptr, addr name, addr gen,
		pointer p, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p);
	method_type_describe_object(&type);
	settype_function(call, type);
	/* method */
	GetConstant(index, &pos);
	mop_argument_method_print_object(&pos, pos); /* print-object */
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  describe
 */
int describe_common_(Execute ptr, addr object, addr stream)
{
	addr call;

	/* stream */
	Return(output_stream_designator_(ptr, stream, &stream));
	Return(fresh_line_stream_(stream, NULL));
	/* call */
	GetConst(COMMON_DESCRIBE_OBJECT, &call);
	Return(getfunction_global_(call, &call));
	return funcall_control_(ptr, call, object, stream, NULL);
}


/*
 *  inspect
 */
static int exit_inspect_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);
	GetNameSymbol(pos, &pos);
	return string_equalp_char_va_(pos, ret, "Q", "QUIT", "E" "EXIT", NULL);
}

static int help_inspect_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);
	GetNameSymbol(pos, &pos);
	return string_equalp_char_va_(pos, ret, "?", "H", "HELP", NULL);
}

static int help_inspect_(Execute ptr, addr io)
{
	static const char *const message[] = {
		"Inspect help.",
		"---",
		"quit  Quit inspect.",
		"help  Output this message.",
		"---",
		NULL
	};
	int i;
	const char *str;

	for (i = 0; ; i++) {
		str = message[i];
		if (str == NULL)
			break;
		Return(print_ascii_stream_(io, str));
		Return(terpri_stream_(io));
		Return(force_output_stream_(io));
	}

	return 0;
}

static int eval_loop_inspect_(Execute ptr, addr io, addr pos, int *exit, int *exec)
{
	int check;

	Return(exit_inspect_p_(pos, &check));
	if (check) {
		*exit = 1;
		*exec = 0;
		return 0;
	}
	Return(help_inspect_p_(pos, &check));
	if (check) {
		*exit = 0;
		*exec = 0;
		return help_inspect_(ptr, io);
	}
	*exit = 0;
	*exec = 1;
	return 0;
}

static int inspect_common_call_(Execute ptr, addr object)
{
	addr io, symbol;

	Return(terminal_io_stream_(ptr, &io));
	Return(describe_common_(ptr, object, io));
	/* *inspected* */
	GetConst(SYSTEM_INSPECTED, &symbol);
	pushspecial_control(ptr, symbol, object);
	/* prompt */
	Return(eval_custom_loop_(ptr, io, eval_loop_inspect_, NULL));

	return 0;
}

int inspect_common_(Execute ptr, addr object)
{
	addr control, prompt;

	push_control(ptr, &control);
	strvect_char_heap(&prompt, "Inspect> ");
	push_prompt(ptr, prompt, prompt_inspect);
	(void)inspect_common_call_(ptr, object);
	return pop_control_(ptr, control);
}


/*
 *  defgeneric
 */
void init_environment_describe(void)
{
	SetPointerType_(var4, method_describe_object_t);
	SetPointerType_(var4, method_describe_object_class);
	SetPointerType_(var4, method_describe_object_standard_object);
	SetPointerType_(var4, method_describe_object_structure_object);
}

#define DefMethod_DescribeObject(ptr, name, gen, p, c) { \
	Return(defmethod_describe_object_((ptr), (name), (gen), \
				p_method_describe_object_##p, CONSTANT_CLOS_##c)); \
}
static int build_describe_object_method_(Execute ptr, addr name, addr gen)
{
	DefMethod_DescribeObject(ptr, name, gen, t, T);
	DefMethod_DescribeObject(ptr, name, gen, class, CLASS);
	DefMethod_DescribeObject(ptr, name, gen, standard_object, STANDARD_OBJECT);
	DefMethod_DescribeObject(ptr, name, gen, structure_object, STRUCTURE_OBJECT);
	return 0;
}

static int build_environment_describe_call_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_DESCRIBE_OBJECT, &symbol);
	mop_argument_generic_var2(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(build_describe_object_method_(ptr, name, gen));
	return common_method_finalize_(gen);
}

void build_environment_describe(Execute ptr)
{
	Error(build_environment_describe_call_(ptr));
}


/************************************************************
 *  env_time.c
 ************************************************************/

#define ENCODE_UNIVERSAL_1900	693961
#define ENCODE_UNIVERSAL_1970	719528

#if defined(LISP_UNIX)
#define LISP_SLEEP_INTERVAL		1000000
#define LISP_SLEEP_INTERVAL_F	1.0e6

#elif defined(LISP_WINDOWS)
#define LISP_SLEEP_INTERVAL		1000
#define LISP_SLEEP_INTERVAL_F	1.0e3

#else
#define LISP_SLEEP_INTERVAL		1
#define LISP_SLEEP_INTERVAL_F	1.0e0
#endif

/*
 *  decode-universal-time
 */
static int decode_universal_second_(addr sec, addr *rsec, addr *rmin, addr *rhour)
{
	size_t v;

	Return(getindex_integer_(sec, &v));
	fixnum_heap(rsec, (fixnum)(v % 60));
	v = v / 60;
	fixnum_heap(rmin, (fixnum)(v % 60));
	v = v / 60;
	Check(24 <= v, "hour error");
	fixnum_heap(rhour, (fixnum)v);

	return 0;
}

static void decode_universal_year4(size_t day, int leap, size_t *ryear, size_t *rday)
{
	size_t year1day;

	year1day = 365 + leap;
	if (day < year1day) {
		*ryear = 0;
		*rday = day;
		return;
	}
	day -= year1day;
	*ryear = day / 365 + 1;
	*rday = day % 365;
}

#define DECODE_UNIVERSAL_YEAR4DAY		(365*4 + 1)
static void decode_universal_year100(size_t day, int leap, size_t *ryear, size_t *rday)
{
	size_t year4day, y4, year;

	year4day = DECODE_UNIVERSAL_YEAR4DAY - 1 + leap;
	if (day < year4day) {
		decode_universal_year4(day, leap, ryear, rday);
		return;
	}
	day -= year4day;
	y4 = day / DECODE_UNIVERSAL_YEAR4DAY;
	day = day % DECODE_UNIVERSAL_YEAR4DAY;
	decode_universal_year4(day, 1, &year, rday);
	*ryear = (y4 + 1) * 4 + year;
}

#define DECODE_UNIVERSAL_YEAR100DAY		(365*100 + 25 - 1)
static void decode_universal_year400(size_t day, size_t *ryear, size_t *rday)
{
	size_t year100day, y100, year;

	year100day = DECODE_UNIVERSAL_YEAR100DAY + 1;
	if (day < year100day) {
		decode_universal_year100(day, 1, ryear, rday);
		return;
	}
	day -= year100day;
	y100 = day / DECODE_UNIVERSAL_YEAR100DAY;
	day = day % DECODE_UNIVERSAL_YEAR100DAY;
	decode_universal_year100(day, 0, &year, rday);
	*ryear = (y100 + 1) * 100 + year;
}

#define DECODE_UNIVERSAL_YEAR400DAY		(365*400 + 100 + 1 - 4)
static void decode_universal_year(size_t day, size_t *ryear, size_t *rday)
{
	size_t y400, year;

	y400 = day / DECODE_UNIVERSAL_YEAR400DAY;
	day = day % DECODE_UNIVERSAL_YEAR400DAY;
	decode_universal_year400(day, &year, rday);
	*ryear = y400 * 400 + year;
}

static int encode_universal_leap(size_t year)
{
	return ((year % 4) == 0) && (((year % 100) != 0) || ((year % 400) == 0));
}

static size_t encode_universal_month_day(size_t month, int leap)
{
	static size_t array[12] = {31,28,31,30,31,30,  31,31,30,31,30,31};

	Check(12 <= month, "month error");
	return ((month == 1) && leap)?
		(array[month] + 1):
		array[month];
}

static int decode_universal_month_(size_t year, size_t day, addr *rmonth, addr *rday)
{
	int leap;
	size_t i, v;

	leap = encode_universal_leap(year);
	for (i = 0; i < 12; i++) {
		v = encode_universal_month_day(i, leap);
		if (day < v) {
			fixnum_heap(rmonth, (fixnum)(i + 1));
			fixnum_heap(rday, (fixnum)(day + 1));
			return 0;
		}
		day -= v;
	}

	/* error */
	*rmonth = Nil;
	*rday = Nil;
	return fmte_("decode-universal-month error.", NULL);
}

static int decode_universal_time_zone_(LocalRoot local,
		struct universal_time_struct *u, addr value, addr zone)
{
	addr v, year, month, day, hour, minute, sec, week;
	size_t date, count;

	/* value, zone */
	fixnum_local(local, &v, 60 * 60);
	Return(multi_fixnum_rational_common_(local, v, zone, &v));
	Return(floor1_common_(local, &v, &week, v));
	Return(minus_ii_real_common_(local, value, v, &value));

	/* week */
	fixnum_local(local, &v, 24 * 60 * 60);
	Return(floor2_common_(local, &day, &sec, value, v));
	Return(getindex_integer_(day, &date));
	fixnum_heap(&week, (fixnum)(date % 7));

	/* day + 1900year */
	date += ENCODE_UNIVERSAL_1900;

	/* date, time */
	Return(decode_universal_second_(sec, &sec, &minute, &hour));
	decode_universal_year(date, &count, &date);
	Return(decode_universal_month_(count, date, &month, &day));
	make_index_integer_heap(&year, count);

	/* result */
	u->second = sec;
	u->minute = minute;
	u->hour = hour;
	u->date = day;
	u->month = month;
	u->year = year;
	u->week = week;
	u->daylight_p = Nil;
	u->zone = zone;

	return 0;
}

/* zone nil */
static int encode_universal_offset(fixnum *ret)
{
	time_t now, a, b;
	struct tm str;

	now = time(NULL);
	if (gmtime_arch(&str, &now))
		return 1;
	a = mktime(&str);
	if (localtime_arch(&str, &now))
		return 1;
	b = mktime(&str);
	*ret = (fixnum)(a - b);

	return 0;
}

static int decode_universal_default_(LocalRoot local,
		struct universal_time_struct *u, addr pos, fixnum offset)
{
	addr zone;
	fixnum_heap(&zone, offset);
	return decode_universal_time_zone_(local, u, pos, zone);
}

static void decode_universal_struct(LocalRoot local,
		struct universal_time_struct *u, struct tm *str, fixnum offset)
{
	int check;
	addr value;

	/* year */
	fixnum_heap(&value, (fixnum)(str->tm_year + 1900));
	u->year = value;

	/* month */
	fixnum_heap(&value, (fixnum)(str->tm_mon + 1));
	u->month = value;

	/* date */
	fixnum_heap(&value, (fixnum)str->tm_mday);
	u->date = value;

	/* hour */
	fixnum_heap(&value, (fixnum)str->tm_hour);
	u->hour = value;

	/* minute */
	fixnum_heap(&value, (fixnum)str->tm_min);
	u->minute = value;

	/* second */
	fixnum_heap(&value, (fixnum)str->tm_sec);
	u->second = value;

	/* week */
	check = str->tm_wday;
	fixnum_heap(&value, (check == 0)? 6: check - 1);
	u->week = value;

	/* daylight_p */
	u->daylight_p = str->tm_isdst? T: Nil;

	/* zone */
	fixnum_heap(&value, offset);
	u->zone = value;
}

static int decode_universal_diff_value_(LocalRoot local, addr *ret)
{
	addr symbol, value;

	/* 1900/01/01 - 1970/01/01 */
	GetConst(SYSTEM_ENCODE_UNIVERSAL_1970, &symbol);
	GetValueSymbol(symbol, &value);
	if (value == Unbound) {
		Return(bigcons_char_local_(local, &value, 10, "2208988800"));
		bignum_cons_heap(&value, signplus_bignum, value);
		Return(setvalue_symbol_(symbol, value));
	}

	return Result(ret, value);
}

static int decode_universal_diff_(LocalRoot local, addr value, addr *ret)
{
	addr right;

	Return(decode_universal_diff_value_(local, &right));
	Return(minus_ii_real_common_(local, value, right, ret));

	return 0;
}

static int decode_universal_time_nil_(LocalRoot local,
		struct universal_time_struct *u, addr time)
{
	size_t value;
	time_t timer;
	fixnum offset;
	struct tm str;
	addr pos;

	/* zone offset */
	if (encode_universal_offset(&offset)) {
		offset = 0;
		goto decode_default;
	}
	offset /= 60 * 60;

	/* universal time */
	Return(decode_universal_diff_(local, time, &pos));
	if (GetIndex_integer(pos, &value))
		goto decode_default;

	/* timer */
	timer = (time_t)value;
	if (timer < 0)
		goto decode_default;
	if (localtime_arch(&str, &timer))
		goto decode_default;
	if (str.tm_isdst < 0)
		goto decode_default;

	/* decode struct */
	decode_universal_struct(local, u, &str, offset);
	return 0;

decode_default:
	return decode_universal_default_(local, u, time, offset);
}

int decode_universal_time_call_(LocalRoot local,
		struct universal_time_struct *u, addr pos, addr zone)
{
	if (zone == Nil)
		return decode_universal_time_nil_(local, u, pos);
	else
		return decode_universal_time_zone_(local, u, pos, zone);
}


/*
 *  encode-universal-time
 */
static int encode_universal_month_(size_t day, size_t month, size_t year, size_t *ret)
{
	int leap;
	size_t sum, i;

	/* check */
	leap = encode_universal_leap(year);
	i = encode_universal_month_day(month, leap);
	if (i < day) {
		*ret = 0;
		return fmte_("Invalid day ~A.", intsizeh(day), NULL);
	}

	/* days */
	sum = 0;
	for (i = 0; i < month; i++)
		sum += encode_universal_month_day(i, leap);

	return Result(ret, sum);
}

static size_t encode_universal_year(size_t year)
{
	size_t year1, y4, y100, y400;

	if (year == 0)
		return 0;
	year1 = year - 1;
	y4 = (year1 / 4) + 1;
	y100 = (year1 / 100) + 1;
	y400 = (year1 / 400) + 1;

	return (year * 365) + y4 - y100 + y400;
}

static int encode_universal_day_(size_t day, size_t month, size_t year, size_t *ret)
{
	size_t size;

	Return(encode_universal_month_(day, month - 1, year, &size));
	size += (day - 1) + encode_universal_year(year);

	return Result(ret, size);
}

static int encode_universal_second_absolute_(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour, addr zone, size_t day)
{
	addr left, right;

	/* (* 24 day) -> (* fixnum integer) -> integer */
	fixnum_heap(&left, 24);
	make_index_integer_local(local, &right, day);
	Return(multi_ii_real_common_(local, left, right, &right));
	/* (+ hour zone) -> (+ fixnum rational) -> rational */
	if (zone != Unbound) {
		Return(plus_fixnum_rational_common_(local, hour, zone, &hour));
	}
	/* (+ hour right) -> (+ rational integer) -> rational */
	Return(plus_rational_common_(local, hour, right, &right));
	/* (* 60 right) -> (* fixnum rational) -> rational */
	fixnum_heap(&left, 60);
	Return(multi_fixnum_rational_common_(local, left, right, &right));
	/* (+ min right) -> (+ fixnum rational) -> rational */
	Return(plus_fixnum_rational_common_(local, min, right, &right));
	/* (* 60 right) -> (* fixnum rational) -> rational */
	Return(multi_fixnum_rational_common_(local, left, right, &right));
	/* (+ sec right) -> (+ fixnum rational) -> rational */
	Return(plus_fixnum_rational_common_(local, sec, right, &right));
	/* (floor right) */
	Return(floor1_common_(local, ret, &right, right));

	return 0;
}

static int encode_universal_time_absolute_(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour,
		size_t day, size_t month, size_t year, addr zone)
{
	Return(encode_universal_day_(day, month, year, &day));
	day -= ENCODE_UNIVERSAL_1900;
	return encode_universal_second_absolute_(local, ret, sec, min, hour, zone, day);
}

/* daylight */
static int encode_universal_diff_(LocalRoot local, addr *ret, addr value)
{
	addr right;

	Return(decode_universal_diff_value_(local, &right));
	Return(plus_ii_real_common_(local, value, right, ret));

	return 0;
}

static int encode_universal_time_standard_(LocalRoot local, addr *value,
		addr sec, addr min, addr hour, size_t day, int *ret)
{
	size_t size;
	time_t now, a, b;
	struct tm str;

	day -= ENCODE_UNIVERSAL_1970;
	Return(encode_universal_second_absolute_(local, &sec, sec, min, hour, Unbound, day));
	/* mktime */
	if (GetIndex_integer(sec, &size))
		return Result(ret, 1);  /* Too large */
	now = (time_t)size;
	if (now < 0)
		return Result(ret, 1);  /* Too large (2038 problem in 32bit arch) */
	if (gmtime_arch(&str, &now))
		return Result(ret, 1);  /* error gmtime */
	a = mktime(&str);
	if (localtime_arch(&str, &now))
		return Result(ret, 1);  /* error localtime */
	b = mktime(&str);
	now += (a - b);
	if (now < 0)
		return Result(ret, 1);
	size = (size_t)now;
	make_index_integer_local(local, &sec, size);
	Return(encode_universal_diff_(local, value, sec));

	return Result(ret, 0);
}

static int encode_universal_time_offset_(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour, size_t day)
{
	fixnum value;
	addr diff;

	day -= ENCODE_UNIVERSAL_1900;
	Return(encode_universal_second_absolute_(local, &sec, sec, min, hour, Unbound, day));
	if (encode_universal_offset(&value)) {
		*ret = Nil;
		return fmte_("encode-universal-offset error", NULL);
	}
	fixnum_heap(&diff, value);
	Return(plus_ii_real_common_(local, sec, diff, ret));

	return 0;
}

static int encode_universal_time_daylight_(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour,
		size_t day, size_t month, size_t year)
{
	int check;

	Return(encode_universal_day_(day, month, year, &day));
	Return(encode_universal_time_standard_(local, ret, sec, min, hour, day, &check));
	if (check)
		return encode_universal_time_offset_(local, ret, sec, min, hour, day);

	return 0;
}

static int encode_universal_time_year_(size_t year, addr year_error, size_t *ret)
{
	time_t now;
	struct tm str;
	size_t a, b;

	if (1900 <= year)
		return Result(ret, year);
	if (100 <= year) {
		*ret = 0;
		return fmte_("Invalid year ~A", year_error, NULL);
	}
	/* 0 - 99 */
	now = time(NULL);
	if (localtime_arch(&str, &now)) {
		*ret = 0;
		return fmte_("localtime error.", NULL);
	}
	a = (1900 + str.tm_year) / 100;
	b = str.tm_year % 100;
	if (b < 50) {
		if (b + 50 <= year) {
			if (a == 0) {
				*ret = 0;
				return fmte_("Too small year ~A", year_error, NULL);
			}
			a--;
		}
	}
	else {
		if (year < b - 50)
			a++;
	}

	return Result(ret, 100*a + year);
}

int encode_universal_time_call_(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour,
		addr day, addr month, addr year, addr zone)
{
	size_t d, m, y;

	Return(getindex_integer_(day, &d));
	Return(getindex_integer_(month, &m));
	Return(getindex_integer_(year, &y));
	Return(encode_universal_time_year_(y, year, &y));

	if (zone != Unbound) {
		return encode_universal_time_absolute_(local, ret,
				sec, min, hour, d, m, y, zone);
	}
	else {
		return encode_universal_time_daylight_(local, ret,
				sec, min, hour, d, m, y);
	}
}


/*
 *  get-universal-time
 */
int get_universal_time_call_(LocalRoot local, addr *ret)
{
	addr left, right;

	make_index_integer_heap(&left, (size_t)time(NULL));
	Return(decode_universal_diff_value_(local, &right));
	Return(plus_ii_real_common_(local, left, right, ret));

	return 0;
}


/*
 *  get-decoded-time
 */
int get_decoded_time_call_(LocalRoot local, struct universal_time_struct *u)
{
	addr pos;
	Return(get_universal_time_call_(local, &pos));
	return decode_universal_time_call_(local, u, pos, Nil);
}


/*
 *  internal-time-units-per-second
 */
#if defined(LISP_WINDOWS)
#include <windows.h>

void get_internal_time_units_per_second(fixnum *ret)
{
	*ret = LISP_SLEEP_INTERVAL;
}
int get_internal_real_time_common_(LocalRoot local, addr *ret)
{
#ifdef LISP_64BIT
	integer_fixed_heap(ret, signplus_bignum, (fixed)GetTickCount64());
#else
	ULONGLONG value;
	addr a, b;

	value = GetTickCount64();
	bignum_value_local(local, &a, signplus_bignum, (fixed)(value >> 32ULL));
	integer_fixed_local(local, &b, signplus_bignum, (fixed)(0xFFFFFFFFULL & value));
	shiftup_bignum_local(local, &a, a, 32);
	Return(plus_ii_real_common_(local, a, b, ret));
#endif
	return 0;
}

#elif defined(LISP_UNIX)
#include <sys/time.h>

void get_internal_time_units_per_second(fixnum *ret)
{
	struct timeval tv;

	if (gettimeofday(&tv, NULL) == 0)
		*ret = LISP_SLEEP_INTERVAL;
	else
		*ret = 1;
}
int get_internal_real_time_common_(LocalRoot local, addr *ret)
{
	struct timeval tv;
	addr high, low, value;

	if (gettimeofday(&tv, NULL) == 0) {
		make_index_integer_local(local, &high, (size_t)tv.tv_sec);
		fixnum_local(local, &low, (fixnum)tv.tv_usec);
		fixnum_local(local, &value, LISP_SLEEP_INTERVAL);
		Return(multi_ii_real_common_(local, high, value, &high));
		Return(plus_fi_real_common_(local, low, high, ret));
	}
	else {
		make_index_integer_heap(ret, (size_t)time(NULL));
	}

	return 0;
}

#else
void get_internal_time_units_per_second(fixnum *ret)
{
	*ret = 1;
}
int get_internal_real_time_common_(LocalRoot local, addr *ret)
{
	make_index_integer_heap(ret, (size_t)time(NULL));
	return 0;
}
#endif

void get_internal_run_time_common(addr *ret)
{
	size_t value;
	value = ControlCounter;
	make_index_integer_heap(ret, value);
}


/*
 *  sleep
 */
#if defined(LISP_UNIX)
#include <unistd.h>

#ifdef LISP_DEBUG
#define LISP_SLEEP_FIXNUM		3
#else
#define LISP_SLEEP_FIXNUM		86400
#endif

struct sleep_object_struct {
	volatile sig_atomic_t atomic;
};
#define SleepObjectStruct(x) ((struct sleep_object_struct *)PtrBodySS(x))

static void get_sleep_object(Execute ptr, addr *ret)
{
	addr pos;

	GetConst(COMMON_SLEEP, &pos);
	getspecial_local(ptr, pos, &pos);
	Check(pos == Unbound, "unbound error");
	CheckType(pos, LISPSYSTEM_SLEEP);
	*ret = pos;
}

static sig_atomic_t getatomic_sleep_object(Execute ptr)
{
	addr pos;
	get_sleep_object(ptr, &pos);
	return SleepObjectStruct(pos)->atomic;
}

static void setatomic_sleep_object(Execute ptr)
{
	addr pos;
	get_sleep_object(ptr, &pos);
	SleepObjectStruct(pos)->atomic = 1;
}

static int push_sleep_object_(Execute ptr)
{
	addr pos, symbol, value;
	LocalRoot local;

	/* object */
	local = ptr->local;
	local_smallsize(local, &pos, LISPSYSTEM_SLEEP,
			1, sizeoft(struct sleep_object_struct));
	SleepObjectStruct(pos)->atomic = 0;

	/* push */
	GetConst(COMMON_SLEEP, &symbol);
	Return(get_internal_real_time_common_(local, &value));
	SetArraySS(pos, 0, value);
	pushspecial_control(ptr, symbol, pos);

	return 0;
}

static int sleep_close_object(Execute ptr)
{
	/* do nothing */
	return 0;
}

static void time_sleep_object(Execute ptr, addr *ret)
{
	addr pos;
	get_sleep_object(ptr, &pos);
	GetArraySS(pos, 0, ret);
}

static int sleep_second_common(Execute ptr, fixnum value)
{
	sleep((unsigned int)value);
	return getatomic_sleep_object(ptr);
}

static int sleep_moment_common(Execute ptr, fixnum value)
{
	useconds_t usec;

	/* argument */
	usec = (useconds_t)value;
	if (usec == 0)
		return 0;
	if (LISP_SLEEP_INTERVAL <= usec)
		usec = LISP_SLEEP_INTERVAL - 1;

	/* sleep */
	if (usleep(usec) == -1) {
		if (errno == EINVAL) {
			Abort("usleep error");
		}
	}

	return getatomic_sleep_object(ptr);
}

#elif defined(LISP_WINDOWS)
#include <windows.h>

#ifdef LISP_DEBUG
#define LISP_SLEEP_FIXNUM		3
#else
#define LISP_SLEEP_FIXNUM		86400000
#endif

struct sleep_object_struct {
	HANDLE hEvent;
};
#define SleepObjectStruct(x) ((struct sleep_object_struct *)PtrBodySS(x))

static void get_sleep_object(Execute ptr, addr *ret)
{
	addr pos;

	GetConst(COMMON_SLEEP, &pos);
	getspecial_local(ptr, pos, &pos);
	Check(pos == Unbound, "unbound error");
	CheckType(pos, LISPSYSTEM_SLEEP);
	*ret = pos;
}

static void setatomic_sleep_object(Execute ptr)
{
	addr pos;
	get_sleep_object(ptr, &pos);
	SetEvent(SleepObjectStruct(pos)->hEvent);
}

static int push_sleep_object_(Execute ptr)
{
	addr pos, symbol, value;
	LocalRoot local;
	HANDLE handle;

	/* event */
	handle = CreateEvent(NULL, TRUE, FALSE, NULL);
	if (handle == NULL) {
		Abort("CreateEvent error.");
		return 0;
	}

	/* object */
	local = ptr->local;
	local_smallsize(local, &pos, LISPSYSTEM_SLEEP,
			1, sizeoft(struct sleep_object_struct));
	SleepObjectStruct(pos)->hEvent = handle;

	/* push */
	GetConst(COMMON_SLEEP, &symbol);
	Return(get_internal_real_time_common_(local, &value));
	SetArraySS(pos, 0, value);
	pushspecial_control(ptr, symbol, pos);

	return 0;
}

static int sleep_close_object(Execute ptr)
{
	addr pos;

	get_sleep_object(ptr, &pos);
	CloseHandle(SleepObjectStruct(pos)->hEvent);

	return 0;
}

static void time_sleep_object(Execute ptr, addr *ret)
{
	addr pos;
	get_sleep_object(ptr, &pos);
	GetArraySS(pos, 0, ret);
}

static int sleep_moment_common(Execute ptr, fixnum value)
{
	DWORD check;
	HANDLE handle;
	addr pos;

	if (value == 0)
		return 0;
	get_sleep_object(ptr, &pos);
	handle = SleepObjectStruct(pos)->hEvent;
	check = WaitForSingleObject(handle, (DWORD)value);
	if (check == WAIT_TIMEOUT)
		return 0;
	if (check == WAIT_OBJECT_0)
		return 1;

	/* error */
	Abort("WaitForSingleObject error.");
	return 0;
}

static int sleep_second_common(Execute ptr, fixnum value)
{
	return sleep_moment_common(ptr, (fixnum)(value * LISP_SLEEP_INTERVAL));
}
#endif

#if defined(LISP_UNIX) || defined(LISP_WINDOWS)
static int sleep_integer_common_(Execute ptr, addr var, int *ret)
{
	int check;
	LocalRoot local;
	addr wait;
	fixnum value;

	local = ptr->local;
	fixnum_heap(&wait, LISP_SLEEP_FIXNUM);
	Return(truncate2_common_(local, &var, &wait, var, wait));
	for (;;) {
		Return(plusp_integer_(var, &check));
		if (! check)
			break;
		if (sleep_second_common(ptr, LISP_SLEEP_FIXNUM))
			return Result(ret, 1);
		Return(oneminus_integer_common_(local, var, &var));
	}
	GetFixnum(wait, &value);
	return Result(ret, sleep_second_common(ptr, value));
}

static int sleep_execute_common_(Execute ptr, addr var, int *ret)
{
	int check;
	addr right;
	LocalRoot local;
	fixnum value;

	fixnum_heap(&right, LISP_SLEEP_INTERVAL);
	local = ptr->local;
	Return(truncate2_common_(local, &var, &right, var, right));
	Return(sleep_integer_common_(ptr, var, &check));
	if (check)
		return Result(ret, 1);
	GetFixnum(right, &value);
	return Result(ret, sleep_moment_common(ptr, value));
}


/*
 *  restart
 */
static int sleep_execute_diff_(Execute ptr, addr var, addr *ret)
{
	addr now, diff;
	LocalRoot local;

	local = ptr->local;
	Return(get_internal_real_time_common_(local, &now));
	time_sleep_object(ptr, &diff);

	/* diff */
	Return(minus_ii_real_common_(local, now, diff, &now));
	Return(minus_ii_real_common_(local, var, now, &var));

	return Result(ret, var);
}

#ifdef LISP_UNIX
static void sleep_signal_handler(int value)
{
	setatomic_sleep_object(Execute_Thread);
}

static int sleep_signal_restart_(Execute ptr, addr var, int *ret)
{
	int escape;

	if (signal(SIGINT, sleep_signal_handler) == SIG_ERR) {
		Abort("signal set error.");
		return 0;
	}
	escape = sleep_execute_common_(ptr, var, ret);
	if (signal(SIGINT, SIG_DFL) == SIG_ERR) {
		Abort("signal set default error.");
		return 0;
	}

	return escape;
}
#endif

#ifdef LISP_WINDOWS
static int sleep_signal_restart_(Execute ptr, addr var, int *ret)
{
	return sleep_execute_common_(ptr, var, ret);
}
#endif

static int sleep_execute_restart_(Execute ptr, addr var, addr *ret)
{
	int check;
	addr condition;
	LocalHold hold;

	check = 0;
	Return(sleep_signal_restart_(ptr, var, &check));
	if (! check)
		return 0;

	/* diff */
	Return(sleep_execute_diff_(ptr, var, &var));
	hold = LocalHold_local_push(ptr, var);
	*ret = var;

	/* invoke-debugger */
	strvect_char_heap(&condition, "Break SIGINT");
	Return(instance_simple_condition_(&condition, condition, Nil));
	localhold_push(hold, condition);
	Return(invoke_debugger_(ptr, condition));
	localhold_end(hold);

	return 0;
}

static void sleep_make_restart(addr *ret)
{
	static const char *message = "Return to sleep.";
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

static int sleep_break_restart_call_(Execute ptr, addr restart, addr var, addr *ret)
{
	addr control, save;

	/* restart */
	Return(push_sleep_object_(ptr));
	*ret = Nil;
	(void)restart1r_control_(ptr, restart, sleep_execute_restart_, var, ret);

	/* unwind-protect */
	push_control(ptr, &control);
	save_execute_control(ptr, &save);
	normal_throw_control(ptr);
	if (sleep_close_object(ptr))
		goto escape;
	restore_execute_control(ptr, save);
escape:
	return pop_control_(ptr, control);
}

static int sleep_break_restart_(Execute ptr, addr restart, addr var, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	(void)sleep_break_restart_call_(ptr, restart, var, ret);
	Return(pop_control_(ptr, control));

	return 0;
}

static int sleep_wait_common_(Execute ptr, addr var)
{
	int check;
	addr restart;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	localhold_set(hold, 0, var);
	for (;;) {
		sleep_make_restart(&restart);
		localhold_set(hold, 1, restart);
		Return(sleep_break_restart_(ptr, restart, var, &var));
		localhold_set(hold, 0, var);
		if (var == Nil)
			break;
		Return(minusp_integer_(var, &check));
		if (check)
			break;
	}
	localhold_end(hold);

	return 0;
}
#endif

#if defined(LISP_UNIX) || defined(LISP_WINDOWS)
static int sleep_value_integer_(LocalRoot local, addr var, addr *ret)
{
	addr right;

	fixnum_heap(&right, LISP_SLEEP_INTERVAL);
	Return(multi_ii_real_common_(local, var, right, ret));

	return 0;
}

static int sleep_value_ratio_(LocalRoot local, addr var, addr *ret)
{
	addr right;

	fixnum_heap(&right, LISP_SLEEP_INTERVAL);
	multi_rf_real_common(local, var, right, &var);
	Return(truncate1_common_(local, ret, &right, var));

	return 0;
}

static int sleep_value_single_float_(LocalRoot local, addr var, addr *ret)
{
	single_float value, moment;
	addr left, right;

	GetSingleFloat(var, &value);
	Return(float_truncate1_s_(value, &value, &moment));
	moment = (single_float)(moment * LISP_SLEEP_INTERVAL_F);
	Return(bignum_single_float_local_(local, value, &left, NULL));
	Return(bignum_single_float_local_(local, moment, &right, NULL));
	fixnum_heap(&var, LISP_SLEEP_INTERVAL);
	Return(multi_ii_real_common_(local, left, var, &left));
	Return(plus_ii_real_common_(local, left, right, ret));

	return 0;
}

static int sleep_value_double_float_(LocalRoot local, addr var, addr *ret)
{
	double_float value, moment;
	addr left, right;

	GetDoubleFloat(var, &value);
	Return(float_truncate1_d_(value, &value, &moment));
	moment = (double_float)(moment * LISP_SLEEP_INTERVAL_F);
	Return(bignum_double_float_local_(local, value, &left, NULL));
	Return(bignum_double_float_local_(local, moment, &right, NULL));
	fixnum_heap(&var, LISP_SLEEP_INTERVAL);
	Return(multi_ii_real_common_(local, left, var, &left));
	Return(plus_ii_real_common_(local, left, right, ret));

	return 0;
}

static int sleep_value_long_float_(LocalRoot local, addr var, addr *ret)
{
	long_float value, moment;
	addr left, right;

	GetLongFloat(var, &value);
	Return(float_truncate1_l_(value, &value, &moment));
	moment = (long_float)(moment * LISP_SLEEP_INTERVAL_F);
	Return(bignum_long_float_local_(local, value, &left, NULL));
	Return(bignum_long_float_local_(local, moment, &right, NULL));
	fixnum_heap(&var, LISP_SLEEP_INTERVAL);
	Return(multi_ii_real_common_(local, left, var, &left));
	Return(plus_ii_real_common_(local, left, right, ret));

	return 0;
}

static int sleep_value_common_(LocalRoot local, addr var, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			return sleep_value_integer_(local, var, ret);

		case LISPTYPE_RATIO:
			return sleep_value_ratio_(local, var, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return sleep_value_single_float_(local, var, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return sleep_value_double_float_(local, var, ret);

		case LISPTYPE_LONG_FLOAT:
			return sleep_value_long_float_(local, var, ret);

		default:
			*ret = Nil;
			return fmte_("Invalid value type ~S.", var, NULL);
	}
}
#endif

int sleep_common_(Execute ptr, addr var)
{
#if defined(LISP_UNIX) || defined(LISP_WINDOWS)
	Return(sleep_value_common_(ptr->local, var, &var));
	return sleep_wait_common_(ptr, var);
#else
	return fmte_("This implementation is not support SLEEP function.", NULL);
#endif
}


/************************************************************
 *  env_version.c
 ************************************************************/

/*
 *  lisp-implementation-type
 */
void implementation_type_common(addr *ret)
{
	strvect_char_heap(ret, LISPNAME);
}


/*
 *  lisp-implementation-version
 */
void implementation_version_common(addr *ret)
{
	char buffer[256];

	snprintf(buffer, 256, "%d.%d.%d [%s]",
			LISP_VERSION_A, LISP_VERSION_B, LISP_VERSION_C, LISP_REVISION);
	strvect_char_heap(ret, buffer);
}


/*
 *  short-site-name
 */
#if defined(LISP_UNIX)
#include <sys/utsname.h>
int short_site_name_common_(addr *ret)
{
	struct utsname data;

	if (uname(&data))
		return Result(ret, Nil);
	else
		return string8_null_heap_(ret, data.nodename);
}
#elif defined(LISP_WINDOWS)
static int windows_site_name_(addr *ret, COMPUTER_NAME_FORMAT format)
{
	BOOL check;
	WCHAR *data;
	DWORD size;
	LocalRoot local;
	LocalStack stack;

	size = 0;
	(void)GetComputerNameExW(format, NULL, &size);
	local = Local_Thread;
	push_local(local, &stack);
	data = (WCHAR *)lowlevel_local(local, size);
	check = GetComputerNameExW(format, NULL, &size);
	if (check == 0) {
		*ret = Nil;
	}
	else {
		Return(string16_size_heap_(ret, (const byte16 *)data, (size_t)size));
	}
	rollback_local(local, stack);

	return 0;
}
int short_site_name_common_(addr *ret)
{
	return windows_site_name_(ret, ComputerNameNetBIOS);
}
#else
int short_site_name_common_(addr *ret)
{
	return Result(ret, Nil);
}
#endif


/*
 *  long-site-name
 */
#if defined(LISP_UNIX)
#include <sys/utsname.h>
int long_site_name_common_(addr *ret)
{
	return short_site_name_common_(ret);
}
#elif defined(LISP_WINDOWS)
int long_site_name_common_(addr *ret)
{
	return windows_site_name_(ret, ComputerNamePhysicalDnsFullyQualified);
}
#else
int long_site_name_common_(addr *ret)
{
	return Result(ret, Nil);
}
#endif


/*
 *  machine-instance
 */
int machine_instance_common_(addr *ret)
{
	return short_site_name_common_(ret);
}


/*
 *  machine-type
 */
#if defined(LISP_UNIX)
int machine_type_common_(addr *ret)
{
	struct utsname data;

	if (uname(&data))
		return Result(ret, Nil);
	else
		return string8_null_heap_(ret, data.machine);
}
#elif defined(LISP_WINDOWS)
int machine_type_common_(addr *ret)
{
	const char *str;
	SYSTEM_INFO info;

	GetNativeSystemInfo(&info);
	/* winnt.h */
	switch (info.wProcessorArchitecture) {
		case 0: str = "INTEL"; break;
		case 1: str = "MIPS"; break;
		case 2: str = "ALPHA"; break;
		case 3: str = "PPC"; break;
		case 4: str = "SHX"; break;
		case 5: str = "ARM"; break;
		case 6: str = "IA64"; break;
		case 7: str = "ALPHA64"; break;
		case 8: str = "MSIL"; break;
		case 9: str = "AMD64"; break;
		case 10: str = "IA32-ON-WIN64"; break;
		case 11: str = "NEUTRAL"; break;
		case 12: str = "ARM64"; break;
		case 13: str = "ARM32-ON-WIN64"; break;
		case 14: str = "IA32-ON-ARM64"; break;
		default: str = NULL; break;
	}
	if (str == NULL)
		return Result(ret, Nil);
	strvect_char_heap(ret, str);
	return 0;
}
#else
int machine_type_common_(addr *ret)
{
	return Result(ret, Nil);
}
#endif


/*
 *  machine-version
 */
#if defined(LISP_UNIX)
int machine_version_common_(addr *ret)
{
	return machine_type_common_(ret);
}
#elif defined(LISP_WINDOWS)
int machine_version_common_(addr *ret)
{
	const char *str;
	SYSTEM_INFO info;

	GetNativeSystemInfo(&info);
	/* winnt.h */
	switch (info.dwProcessorType) {
		case 386: str = "INTEL-386"; break;
		case 486: str = "INTEL-486"; break;
		case 586: str = "INTEL-PENTIUM"; break;
		case 2200: str = "INTEL-IA64"; break;
		case 8664: str = "AMD-X8664"; break;
		case 4000: str = "MIPS-R4000"; break;
		case 21064: str = "ALPHA-21064"; break;
		case 601: str = "PPC-601"; break;
		case 603: str = "PPC-603"; break;
		case 604: str = "PPC-604"; break;
		case 620: str = "PPC-620"; break;
		case 10003: str = "HITACHI-SH3"; break;
		case 10004: str = "HITACHI-SH3E"; break;
		case 10005: str = "HITACHI-SH4"; break;
		case 821: str = "MOTOROLA-821"; break;
		case 103: str = "SHx-SH3"; break;
		case 104: str = "SHx-SH4"; break;
		case 2577: str = "STRONGARM"; break;
		case 1824: str = "ARM720"; break;
		case 2080: str = "ARM820"; break;
		case 2336: str = "ARM920"; break;
		case 70001: str = "ARM-7TDMI"; break;
		case 0x494f: str = "OPTIL"; break;
		default: str = NULL; break;
	}
	if (str == NULL)
		return Result(ret, Nil);
	strvect_char_heap(ret, str);
	return 0;
}
#else
int machine_version_common_(addr *ret)
{
	return Result(ret, Nil);
}
#endif


/*
 *  software-type
 */
#if defined(LISP_UNIX)
int software_type_common_(addr *ret)
{
	struct utsname data;

	if (uname(&data))
		return Result(ret, Nil);
	else
		return string8_null_heap_(ret, data.sysname);
}
#elif defined(LISP_WINDOWS)
int software_type_common_(addr *ret)
{
	strvect_char_heap(ret, "Windows");
	return 0;
}
#else
int software_type_common_(addr *ret)
{
	return Result(ret, Nil);
}
#endif


/*
 *  software-version
 */
#if defined(LISP_UNIX)
int software_version_common_(addr *ret)
{
	struct utsname data;

	if (uname(&data))
		return Result(ret, Nil);
	else
		return string8_null_heap_(ret, data.release);
}
#elif defined(LISP_WINDOWS)
int software_version_common_(addr *ret)
{
	BOOL (*local_GetFileVersionInfo)(LPCSTR, DWORD, DWORD, LPVOID);
	DWORD (*local_GetFileVersionInfoSize)(LPCSTR, LPDWORD);
	BOOL (*local_VerQueryValue)(LPCVOID, LPCSTR, LPVOID *, PUINT);
	BOOL check;
	HMODULE version_dll;
	FARPROC call;
	DWORD size, handle;
	struct LANGANDCODEPAGE {
		WORD wLanguage;
		WORD wCodePage;
	} *translate;
	void *ptr;
	char buffer[256];
	const char *pversion;
	UINT usize;

#ifdef LISP_32BIT
	return Result(ret, Nil);
#endif

	ptr = NULL;
	/* VERSION.DLL */
	version_dll = LoadLibraryA("VERSION.DLL");
	if (version_dll == NULL) {
		goto error_dll;
	}

	/* GetFileVersionInfo */
	call = GetProcAddress(version_dll, "GetFileVersionInfoA");
	if (call == NULL) {
		goto error_dll;
	}
	local_GetFileVersionInfo = (BOOL (*)(LPCSTR, DWORD, DWORD, LPVOID))call;

	/* GetFileVersionInfoSize */
	call = GetProcAddress(version_dll, "GetFileVersionInfoSizeA");
	if (call == NULL) {
		goto error_dll;
	}
	local_GetFileVersionInfoSize = (DWORD (*)(LPCSTR, LPDWORD))call;

	/* VerQueryValue */
	call = GetProcAddress(version_dll, "VerQueryValueA");
	if (call == NULL) {
		goto error_dll;
	}
	local_VerQueryValue = (BOOL (*)(LPCVOID, LPCSTR, LPVOID *, PUINT))call;

	/* size */
	handle = 0;
	size = local_GetFileVersionInfoSize("KERNEL32.DLL", &handle);
	if (size == 0) {
		goto error_dll;
	}

	/* version */
	ptr = malloc(size);
	if (ptr == NULL) {
		goto error_dll;
	}
	check = local_GetFileVersionInfo("KERNEL32.DLL", 0, size, ptr);
	if (check == 0) {
		goto error_dll;
	}

	/* query */
	check = local_VerQueryValue(ptr,
			"\\VarFileInfo\\Translation",
			(LPVOID *)&translate, &usize);
	if (check == 0) {
		goto error_dll;
	}
	snprintf(buffer, 256, "\\StringFileInfo\\%04x%04x\\ProductVersion",
			translate->wLanguage, translate->wCodePage);
	check = local_VerQueryValue(ptr, buffer, (LPVOID *)&pversion, &usize);
	if (check == 0) {
		goto error_dll;
	}

	/* result */
	Return(string8_null_heap_(ret, pversion));
	free(ptr);
	FreeLibrary(version_dll);
	return 0;

error_dll:
	free(ptr);
	if (version_dll)
		FreeLibrary(version_dll);
	return Result(ret, Nil);
}

#else
int software_version_common_(addr *ret)
{
	return Result(ret, Nil);
}
#endif


/*
 *  user-homedir-pathname
 */
static int default_user_homedir_pathname_common_(Execute ptr, addr *ret)
{
	return parse_pathname_char_heap_(ptr, ".", ret);
}

static int user_homedir_pathname_string_(LocalRoot local, addr pos, addr *ret)
{
	unicode c;
	addr one;
	size_t size, i;

	string_length(pos, &size);
	if (size == 0)
		goto no_update;
	Return(string_getc_(pos, size - 1, &c));
	if (c == '/' || c == '\\')
		goto no_update;
	strvect_local(local, &one, size + 1);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		Return(strvect_setc_(one, i, c));
	}
#ifdef LISP_WINDOWS
	Return(strvect_setc_(one, i, '\\'));
#else
	Return(strvect_setc_(one, i, '/'));
#endif
	return Result(ret, one);

no_update:
	return Result(ret, pos);
}

int user_homedir_pathname_common_(Execute ptr, addr *ret)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	GetConst(SYSTEM_SPECIAL_ENVIRONMENT, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Unbound || pos == Nil)
		goto error;
	Return(find_char_hashtable_(pos, "HOME", &pos));
	if (pos == Unbound)
		goto error;
	/* /home/name -> /home/name/ */
	local = ptr->local;
	push_local(local, &stack);
	Return(user_homedir_pathname_string_(local, pos, &pos));
	Return(physical_pathname_heap_(ptr, pos, ret));
	rollback_local(local, stack);
	return 0;

error:
	return default_user_homedir_pathname_common_(ptr, ret);
}


/************************************************************
 *  environment.c
 ************************************************************/

void init_environment(void)
{
	init_environment_code();
	init_environment_describe();
}

void build_environment(Execute ptr)
{
	build_environment_code();
	build_environment_describe(ptr);
}


/************************************************************
 *  equal.c
 ************************************************************/

static int array_get_unicode_check_(addr pos, size_t index, unicode *value, int *ret)
{
	addr x;
	struct array_value str;

	/* access */
	Return(arrayinplace_get_(pos, index, &str));

	/* character */
	if (str.type == ARRAY_TYPE_CHARACTER) {
		*value = str.value.character;
		return Result(ret, 1);
	}

	/* others */
	if (str.type != ARRAY_TYPE_T) {
		*value = 0;
		return Result(ret, 0);
	}

	/* object */
	x = str.value.object;
	if (! characterp(x)) {
		*value = 0;
		return Result(ret, 0);
	}

	/* character */
	GetCharacter(x, value);
	return Result(ret, 1);
}


/*
 *  eq
 */
int atom_function(addr pos)
{
	Check(pos == Unbound, "Unbound-variable");
	return GetType(pos) != LISPTYPE_CONS;
}

int eq_function(addr a, addr b)
{
	Check(a == Unbound, "Unbound-variable");
	Check(b == Unbound, "Unbound-variable");
	return a == b;
}

int eq_function_(addr a, addr b, int *ret)
{
	Check(a == Unbound, "Unbound-variable");
	Check(b == Unbound, "Unbound-variable");
	return Result(ret, a == b);
}


/*
 *  eql
 */
static int eql_function_fixnum(addr a, enum LISPTYPE type2, addr b)
{
	switch (type2) {
		case LISPTYPE_FIXNUM:
			return RefFixnum(a) == RefFixnum(b);

		case LISPTYPE_BIGNUM:
			return equal_fb_real(a, b);

		case LISPTYPE_RATIO:
			return equal_fr_real(a, b);

		default:
			return 0;
	}
}

static int eql_function_bignum(addr a, enum LISPTYPE type2, addr b)
{
	switch (type2) {
		case LISPTYPE_FIXNUM:
			return equal_bf_real(a, b);

		case LISPTYPE_BIGNUM:
			return equal_bb_real(a, b);

		case LISPTYPE_RATIO:
			return equal_br_real(a, b);

		default:
			return 0;
	}
}

static int eql_function_ratio(addr a, enum LISPTYPE type2, addr b)
{
	switch (type2) {
		case LISPTYPE_FIXNUM:
			return equal_rf_real(a, b);

		case LISPTYPE_BIGNUM:
			return equal_rb_real(a, b);

		case LISPTYPE_RATIO:
			return equal_rr_real(a, b);

		default:
			return 0;
	}
}

static int eql_function_single_float(addr a, addr b)
{
	single_float x, y;

	CheckType(a, LISPTYPE_SINGLE_FLOAT);
	CheckType(b, LISPTYPE_SINGLE_FLOAT);
	GetSingleFloat(a, &x);
	GetSingleFloat(b, &y);

	return (x == y) && (signbit(x) == signbit(y));
}

static int eql_function_double_float(addr a, addr b)
{
	double_float x, y;

	CheckType(a, LISPTYPE_DOUBLE_FLOAT);
	CheckType(b, LISPTYPE_DOUBLE_FLOAT);
	GetDoubleFloat(a, &x);
	GetDoubleFloat(b, &y);

	return (x == y) && (signbit(x) == signbit(y));
}

static int eql_function_long_float(addr a, addr b)
{
	long_float x, y;

	CheckType(a, LISPTYPE_LONG_FLOAT);
	CheckType(b, LISPTYPE_LONG_FLOAT);
	GetLongFloat(a, &x);
	GetLongFloat(b, &y);

	return (x == y) && (signbit(x) == signbit(y));
}

int eql_function(addr a, addr b)
{
	enum LISPTYPE type1, type2;

	Check(a == Unbound, "Unbound-variable");
	Check(b == Unbound, "Unbound-variable");

	/* eq */
	if (a == b)
		return 1;

	/* eql */
	type1 = GetType(a);
	type2 = GetType(b);
	switch (type1) {
		case LISPTYPE_FIXNUM:
			return eql_function_fixnum(a, type2, b);

		case LISPTYPE_BIGNUM:
			return eql_function_bignum(a, type2, b);

		case LISPTYPE_RATIO:
			return eql_function_ratio(a, type2, b);

		default:
			break;
	}

	if (type1 != type2)
		return 0;
	switch (type1) {
		case LISPTYPE_CHARACTER:
			return character_equal(a, b);

		case LISPTYPE_SINGLE_FLOAT:
			return eql_function_single_float(a, b);

		case LISPTYPE_DOUBLE_FLOAT:
			return eql_function_double_float(a, b);

		case LISPTYPE_LONG_FLOAT:
			return eql_function_long_float(a, b);

		case LISPTYPE_COMPLEX:
			return eql_complex(a, b);

		default:
			return 0;
	}
}

int eql_function_(addr left, addr right, int *ret)
{
	return Result(ret, eql_function(left, right));
}


/*
 *  equal
 */
static int equal_function_cons_(addr a, addr b, int *ret)
{
	int check;
	addr car1, car2, cdr1, cdr2;

	if (! consp(b))
		return Result(ret, 0);
	GetCons(a, &car1, &cdr1);
	GetCons(b, &car2, &cdr2);
	Return(equal_function_(car1, car2, &check));
	if (! check)
		return Result(ret, 0);
	else
		return equal_function_(cdr1, cdr2, ret);
}

static int equal_function_string_(addr a, addr b, int *ret)
{
	if (stringp(b))
		return string_equal_(a, b, ret);
	else
		return Result(ret, 0);
}

static int equal_function_bitvector_(addr a, addr b, int *ret)
{
	if (bitvectorp(b))
		return bitvector_equal_(a, b, ret);
	else
		return Result(ret, 0);
}

static int equal_function_array_(addr a, addr b, int *ret)
{
	switch (GetType(b)) {
		case LISPTYPE_ARRAY:
		case LISPTYPE_STRING:
			return equal_function_string_(b, a, ret);

		case LISPTYPE_BITVECTOR:
			return equal_function_bitvector_(b, a, ret);

		default:
			return Result(ret, 0);
	}
}

static int equal_function_pathname_(addr a, addr b, int *ret)
{
	if (! pathnamep(a))
		return Result(ret, 0);
	if (! pathnamep(b))
		return Result(ret, 0);

	return pathname_equal_(a, b, ret);
}

int equal_function_(addr a, addr b, int *ret)
{
	Check(a == Unbound, "Unbound-variable");
	Check(b == Unbound, "Unbound-variable");

	/* eql */
	if (eql_function(a, b))
		return Result(ret, 1);

	/* equal */
	switch (GetType(a)) {
		case LISPTYPE_CONS:
			return equal_function_cons_(a, b, ret);

		case LISPTYPE_STRING:
			return equal_function_string_(a, b, ret);

		case LISPTYPE_BITVECTOR:
			return equal_function_bitvector_(a, b, ret);

		case LISPTYPE_ARRAY:
			return equal_function_array_(a, b, ret);

		case LISPTYPE_PATHNAME:
			return equal_function_pathname_(a, b, ret);

		default:
			return Result(ret, 0);
	}
}


/*
 *  equalp
 */
static int equalp_function_number_(addr a, addr b, int *ret)
{
	if (numberp(b))
		return equal_number_(Local_Thread, a, b, ret);
	else
		return Result(ret, 0);
}

static int equalp_function_character_(addr a, addr b, int *ret)
{
	if (characterp(b))
		return Result(ret, character_equalp(a, b));
	else
		return Result(ret, 0);
}

static int equalp_function_cons_(addr a, addr b, int *ret)
{
	int check;
	addr car1, car2, cdr1, cdr2;

	if (! consp(b))
		return Result(ret, 0);
	GetCons(a, &car1, &cdr1);
	GetCons(b, &car2, &cdr2);
	Return(equalp_function_(car1, car2, &check));
	if (! check)
		return Result(ret, 0);
	else
		return equalp_function_(cdr1, cdr2, ret);
}

static int equalp_function_aa_(addr a, addr b, int *ret)
{
	int check;
	addr c, d;
	LocalRoot local;
	LocalStack stack;
	size_t size, i;

	/* rank, dimension */
	if (! array_equal_dimension(a, b))
		return Result(ret, 0);
	/* fill-pointer */
	array_get_rowlength(a, &size);
	array_get_rowlength(b, &i);
	if (size != i)
		return Result(ret, 0);
	/* body */
	local = Local_Thread;
	for (i = 0; i < size; i++) {
		push_local(local, &stack);
		Return(array_get_(local, a, i, &c));
		Return(array_get_(local, b, i, &d));
		Return(equalp_function_(c, d, &check));
		rollback_local(local, stack);
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int equalp_function_av_(addr a, addr b, int *ret)
{
	int check;
	addr c, d;
	LocalRoot local;
	LocalStack stack;
	size_t size, i;

	/* size check */
	if (! array_vector_p(a))
		return Result(ret, 0);
	array_get_rowlength(a, &size);
	lenarray(b, &i);
	if (size != i)
		return Result(ret, 0);
	/* body */
	local = Local_Thread;
	for (i = 0; i < size; i++) {
		push_local(local, &stack);
		Return(array_get_(local, a, i, &c));
		getarray(b, i, &d);
		Return(equalp_function_(c, d, &check));
		rollback_local(local, stack);
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int equalp_function_as_(addr a, addr b, int *ret)
{
	int check;
	unicode c, d;
	size_t size, i;

	/* string */
	if (array_stringp(a))
		return string_equalp_(a, b, ret);
	/* size check */
	if (! array_vector_p(a))
		return Result(ret, 0);
	array_get_rowlength(a, &size);
	strvect_length(b, &i);
	if (size != i)
		return Result(ret, 0);
	/* body */
	for (i = 0; i < size; i++) {
		Return(array_get_unicode_check_(a, i, &c, &check));
		if (! check)
			return Result(ret, 0);
		strvect_getc(b, i, &d);
		if (toUpperUnicode(c) != toUpperUnicode(d))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int equalp_function_ab_(addr a, addr b, int *ret)
{
	int check;
	addr c, d;
	size_t size, i;

	/* string */
	if (array_bvarrayp(a))
		return bitvector_equal_(a, b, ret);
	/* size check */
	if (! array_vector_p(a))
		return Result(ret, 0);
	array_get_rowlength(a, &size);
	bitmemory_length(b, &i);
	if (size != i)
		return Result(ret, 0);
	/* body */
	for (i = 0; i < size; i++) {
		Return(array_get_(NULL, a, i, &c));
		Return(bitmemory_get_(NULL, b, i, &d));
		Return(equalp_function_(c, d, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int equalp_function_array_(addr a, addr b, int *ret)
{
	switch (GetType(b)) {
		case LISPTYPE_ARRAY:
			return equalp_function_aa_(b, a, ret);

		case LISPTYPE_VECTOR:
			return equalp_function_av_(a, b, ret);

		case LISPTYPE_STRING:
			return equalp_function_as_(a, b, ret);

		case LISPTYPE_BITVECTOR:
			return equalp_function_ab_(a, b, ret);

		default:
			return Result(ret, 0);
	}
}

static int equalp_function_vv_(addr a, addr b, int *ret)
{
	int check;
	addr c, d;
	size_t size, i;

	lenarray(a, &size);
	lenarray(b, &i);
	if (size != i)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		getarray(a, i, &c);
		getarray(b, i, &d);
		Return(equalp_function_(c, d, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int equalp_function_vs_(addr a, addr b, int *ret)
{
	addr c;
	unicode d, e;
	size_t size, i;

	lenarray(a, &size);
	strvect_length(b, &i);
	if (size != i)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		getarray(a, i, &c);
		if (! characterp(c))
			return Result(ret, 0);
		GetCharacter(c, &d);
		strvect_getc(b, i, &e);
		if (toUpperUnicode(d) != toUpperUnicode(e))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int equalp_function_vb_(addr a, addr b, int *ret)
{
	int d, e;
	addr c;
	size_t size, i;

	lenarray(a, &size);
	bitmemory_length(b, &i);
	if (size != i)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		getarray(a, i, &c);
		if (bit_getint(c, &d))
			return Result(ret, 0);
		Return(bitmemory_getint_(b, i, &e));
		if (d != e)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int equalp_function_vector_(addr a, addr b, int *ret)
{
	switch (GetType(b)) {
		case LISPTYPE_VECTOR:
			return equalp_function_vv_(a, b, ret);

		case LISPTYPE_ARRAY:
			return equalp_function_av_(b, a, ret);

		case LISPTYPE_STRING:
			return equalp_function_vs_(a, b, ret);

		case LISPTYPE_BITVECTOR:
			return equalp_function_vb_(a, b, ret);

		default:
			return Result(ret, 0);
	}
}

static int equalp_function_string_(addr a, addr b, int *ret)
{
	switch (GetType(b)) {
		case LISPTYPE_STRING:
			return Result(ret, strvect_equalp(a, b));

		case LISPTYPE_VECTOR:
			return equalp_function_vs_(b, a, ret);

		case LISPTYPE_ARRAY:
			return equalp_function_as_(b, a, ret);

		default:
			return Result(ret, 0);
	}
}

static int equalp_function_bitvector_(addr a, addr b, int *ret)
{
	switch (GetType(b)) {
		case LISPTYPE_BITVECTOR:
			return Result(ret, bitmemory_equal(a, b));

		case LISPTYPE_VECTOR:
			return equalp_function_vb_(b, a, ret);

		case LISPTYPE_ARRAY:
			return equalp_function_ab_(b, a, ret);

		default:
			return Result(ret, 0);
	}
}

static int equalp_function_hashtable_(addr a, addr b, int *ret)
{
	if (hashtablep(b))
		return equalp_hashtable_(a, b, ret);
	else
		return Result(ret, 0);
}

static int equalp_function_structure_(addr a, addr b, int *ret)
{
	int check;

	Return(structure_instance_p_(b, &check));
	if (check)
		return equalp_structure_(a, b, ret);
	else
		return Result(ret, 0);
}

int equalp_function_(addr a, addr b, int *ret)
{
	Check(a == Unbound, "Unbound-variable");
	Check(b == Unbound, "Unbound-variable");

	/* eq */
	if (a == b)
		return Result(ret, 1);

	/* number */
	if (numberp(a))
		return equalp_function_number_(a, b, ret);

	/* equalp */
	switch (GetType(a)) {
		case LISPTYPE_CHARACTER:
			return equalp_function_character_(a, b, ret);

		case LISPTYPE_CONS:
			return equalp_function_cons_(a, b, ret);

		case LISPTYPE_ARRAY:
			return equalp_function_array_(a, b, ret);

		case LISPTYPE_VECTOR:
			return equalp_function_vector_(a, b, ret);

		case LISPTYPE_STRING:
			return equalp_function_string_(a, b, ret);

		case LISPTYPE_BITVECTOR:
			return equalp_function_bitvector_(a, b, ret);

		case LISPTYPE_PATHNAME:
			return equal_function_pathname_(a, b, ret);

		case LISPTYPE_HASHTABLE:
			return equalp_function_hashtable_(a, b, ret);

		case LISPTYPE_CLOS:
			return equalp_function_structure_(a, b, ret);

		default:
			return Result(ret, 0);
	}
}


/*
 *  equal for rt
 */
static int equalrt_function_character_(addr a, addr b, int *ret)
{
	if (characterp(b))
		return Result(ret, character_equal(a, b));
	else
		return Result(ret, 0);
}

static int equalrt_function_cons_(addr a, addr b, int *ret)
{
	int check;
	addr car1, car2, cdr1, cdr2;

	if (! consp(b))
		return Result(ret, 0);
	GetCons(a, &car1, &cdr1);
	GetCons(b, &car2, &cdr2);
	Return(equalrt_function_(car1, car2, &check));
	if (! check)
		return Result(ret, 0);
	else
		return equalrt_function_(cdr1, cdr2, ret);
}

static int equalrt_function_aa_(addr a, addr b, int *ret)
{
	int check;
	addr c, d;
	LocalRoot local;
	LocalStack stack;
	size_t size, i;

	/* rank, dimension */
	if (! array_equal_dimension(a, b))
		return Result(ret, 0);
	/* fill-pointer */
	array_get_rowlength(a, &size);
	array_get_rowlength(b, &i);
	if (size != i)
		return Result(ret, 0);
	/* body */
	local = Local_Thread;
	for (i = 0; i < size; i++) {
		push_local(local, &stack);
		Return(array_get_(local, a, i, &c));
		Return(array_get_(local, b, i, &d));
		Return(equalrt_function_(c, d, &check));
		rollback_local(local, stack);
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int equalrt_function_av_(addr a, addr b, int *ret)
{
	int check;
	addr c, d;
	LocalRoot local;
	LocalStack stack;
	size_t size, i;

	/* size check */
	if (! array_vector_p(a))
		return Result(ret, 0);
	array_get_rowlength(a, &size);
	lenarray(b, &i);
	if (size != i)
		return Result(ret, 0);
	/* body */
	local = Local_Thread;
	for (i = 0; i < size; i++) {
		push_local(local, &stack);
		Return(array_get_(local, a, i, &c));
		getarray(b, i, &d);
		Return(equalrt_function_(c, d, &check));
		rollback_local(local, stack);
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int equalrt_function_as_(addr a, addr b, int *ret)
{
	int check;
	unicode c, d;
	size_t size, i;

	/* string */
	if (array_stringp(a))
		return string_equal_(a, b, ret);
	/* size check */
	if (! array_vector_p(a))
		return Result(ret, 0);
	array_get_rowlength(a, &size);
	strvect_length(b, &i);
	if (size != i)
		return Result(ret, 0);
	/* body */
	for (i = 0; i < size; i++) {
		Return(array_get_unicode_check_(a, i, &c, &check));
		if (! check)
			return Result(ret, 0);
		Return(array_get_unicode_(a, i, &c));
		strvect_getc(b, i, &d);
		if (c != d)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int equalrt_function_array_(addr a, addr b, int *ret)
{
	switch (GetType(b)) {
		case LISPTYPE_ARRAY:
			return equalrt_function_aa_(b, a, ret);

		case LISPTYPE_VECTOR:
			return equalrt_function_av_(a, b, ret);

		case LISPTYPE_STRING:
			return equalrt_function_as_(a, b, ret);

		case LISPTYPE_BITVECTOR:
			return equalp_function_ab_(a, b, ret);

		default:
			return Result(ret, 0);
	}
}

static int equalrt_function_vv_(addr a, addr b, int *ret)
{
	int check;
	addr c, d;
	size_t size, i;

	lenarray(a, &size);
	lenarray(b, &i);
	if (size != i)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		getarray(a, i, &c);
		getarray(b, i, &d);
		Return(equalrt_function_(c, d, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int equalrt_function_vs_(addr a, addr b, int *ret)
{
	addr c;
	unicode d, e;
	size_t size, i;

	lenarray(a, &size);
	strvect_length(b, &i);
	if (size != i)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		getarray(a, i, &c);
		if (! characterp(c))
			return Result(ret, 0);
		GetCharacter(c, &d);
		strvect_getc(b, i, &e);
		if (d != e)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int equalrt_function_vector_(addr a, addr b, int *ret)
{
	switch (GetType(b)) {
		case LISPTYPE_VECTOR:
			return equalrt_function_vv_(a, b, ret);

		case LISPTYPE_STRING:
			return equalrt_function_vs_(a, b, ret);

		case LISPTYPE_ARRAY:
			return equalrt_function_av_(b, a, ret);

		case LISPTYPE_BITVECTOR:
			return equalp_function_vb_(a, b, ret);

		default:
			return Result(ret, 0);
	}
}

static int equalrt_function_string_(addr a, addr b, int *ret)
{
	switch (GetType(b)) {
		case LISPTYPE_STRING:
			return Result(ret, strvect_equal(a, b));

		case LISPTYPE_VECTOR:
			return equalrt_function_vs_(b, a, ret);

		case LISPTYPE_ARRAY:
			return equalrt_function_as_(b, a, ret);

		default:
			return Result(ret, 0);
	}
}

static int equalrt_function_bitvector_(addr a, addr b, int *ret)
{
	switch (GetType(b)) {
		case LISPTYPE_BITVECTOR:
			return Result(ret, bitmemory_equal(a, b));

		case LISPTYPE_VECTOR:
			return equalp_function_vb_(b, a, ret);

		case LISPTYPE_ARRAY:
			return equalp_function_ab_(b, a, ret);

		default:
			return Result(ret, 0);
	}
}

static int equalrt_function_hashtable_(addr a, addr b, int *ret)
{
	if (hashtablep(b))
		return equalrt_hashtable_(a, b, ret);
	else
		return Result(ret, 0);
}

static int equalrt_function_structure_(addr a, addr b, int *ret)
{
	int check;

	Return(structure_instance_p_(b, &check));
	if (check)
		return equalrt_structure_(a, b, ret);
	else
		return Result(ret, 0);
}

int equalrt_function_(addr a, addr b, int *ret)
{
	Check(a == Unbound, "Unbound-variable");
	Check(b == Unbound, "Unbound-variable");

	/* eql */
	if (eql_function(a, b))
		return Result(ret, 1);

	/* equalrt */
	switch (GetType(a)) {
		case LISPTYPE_CHARACTER:
			return equalrt_function_character_(a, b, ret);

		case LISPTYPE_CONS:
			return equalrt_function_cons_(a, b, ret);

		case LISPTYPE_ARRAY:
			return equalrt_function_array_(a, b, ret);

		case LISPTYPE_VECTOR:
			return equalrt_function_vector_(a, b, ret);

		case LISPTYPE_STRING:
			return equalrt_function_string_(a, b, ret);

		case LISPTYPE_BITVECTOR:
			return equalrt_function_bitvector_(a, b, ret);

		case LISPTYPE_PATHNAME:
			return equal_function_pathname_(a, b, ret);

		case LISPTYPE_HASHTABLE:
			return equalrt_function_hashtable_(a, b, ret);

		case LISPTYPE_CLOS:
			return equalrt_function_structure_(a, b, ret);

		default:
			return Result(ret, 0);
	}
}


/*
 *  for debug
 */
int equal_debug(addr left, addr right)
{
	int check;
	check = 0;
	Error(equal_function_(left, right, &check));
	return check;
}

int equalp_debug(addr left, addr right)
{
	int check;
	check = 0;
	Error(equalp_function_(left, right, &check));
	return check;
}

int equalrt_debug(addr left, addr right)
{
	int check;
	check = 0;
	Error(equalrt_function_(left, right, &check));
	return check;
}


/************************************************************
 *  eval.c
 ************************************************************/

void init_eval(void)
{
	init_eval_copy();
}


/************************************************************
 *  eval_copy.c
 ************************************************************/

/* single */
static void copy_eval_single(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	GetEvalParseType(eval, &type);
	GetEvalParse(eval, 0, &eval);
	eval_single_parse_alloc(local, ret, type, eval);
}

/* double */
static void copy_eval_double(LocalRoot local, addr *ret, addr eval)
{
	addr x, y;
	EvalParse type;

	GetEvalParseType(eval, &type);
	GetEvalParse(eval, 0, &x);
	GetEvalParse(eval, 1, &y);
	eval_parse2_alloc(local, ret, type, x, y);
}

/* declaim */
static void copy_eval_declaim_nil(LocalRoot local, addr *ret, addr pos)
{
	if (pos == Nil)
		*ret = Nil;
	else
		copy_eval_declare_alloc(local, ret, pos);
}

static void copy_eval_declaim(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;

	GetEvalParseType(eval, &type);
	GetEvalParse(eval, 0, &eval);
	copy_eval_declaim_nil(local, &eval, eval);
	eval_single_parse_alloc(local, ret, type, eval);
}

/* progn */
static void copy_eval_allcons(LocalRoot local, addr *ret, addr cons)
{
	addr root, pos;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		copy_eval_parse(local, &pos, pos);
		cons_alloc(local, &root, pos, root);
	}
	nreverse(ret, root);
}

static void copy_eval_progn(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, list;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_PROGN, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &list);
	copy_eval_allcons(local, &list, list);
	eval_parse2_alloc(local, ret, type, form, list);
}

/* let */
static void copy_eval_let_args(LocalRoot local, addr *ret, addr args)
{
	addr root, init, pos;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &pos, &args);
		GetCons(pos, &pos, &init);
		copy_eval_parse(local, &init, init);
		cons_alloc(local, &pos, pos, init);
		cons_alloc(local, &root, pos, root);
	}
	nreverse(ret, root);
}

static void copy_eval_let(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, args, decl, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LET && type != EVAL_PARSE_LETA, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &args);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &cons);
	copy_eval_let_args(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, cons);
	*ret = eval;
}

/* setq */
static void copy_eval_setq_args(LocalRoot local, addr *ret, addr cons)
{
	addr root, pos, value;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		GetCons(pos, &pos, &value);
		copy_eval_parse(local, &value, value);
		cons_alloc(local, &pos, pos, value);
		cons_alloc(local, &root, pos, root);
	}
	nreverse(ret, root);
}

static void copy_eval_setq(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, list;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_SETQ, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &list);
	copy_eval_setq_args(local, &list, list);
	eval_parse2_alloc(local, ret, EVAL_PARSE_SETQ, form, list);
}

/* defun */
static void copy_eval_ordinary_optional(LocalRoot local, addr *ret, addr cons)
{
	addr root, var, init, svar;

	for (root = Nil; cons != Nil; ) {
		/* (var init svar) */
		GetCons(cons, &svar, &cons);
		GetCons(svar, &var, &svar);
		GetCons(svar, &init, &svar);
		GetCar(svar, &svar);
		copy_eval_parse(local, &init, init);
		list_alloc(local, &svar, var, init, svar, NULL);
		cons_alloc(local, &root, svar, root);
	}
	nreverse(ret, root);
}

static void copy_eval_ordinary_key(LocalRoot local, addr *ret, addr cons)
{
	addr root, var, name, init, svar;

	for (root = Nil; cons != Nil; ) {
		/* (var name init svar) */
		GetCons(cons, &svar, &cons);
		GetCons(svar, &var, &svar);
		GetCons(svar, &name, &svar);
		GetCons(svar, &init, &svar);
		GetCar(svar, &svar);
		copy_eval_parse(local, &init, init);
		list_alloc(local, &svar, var, name, init, svar, NULL);
		cons_alloc(local, &root, svar, root);
	}
	nreverse(ret, root);
}

static void copy_eval_ordinary_aux(LocalRoot local, addr *ret, addr cons)
{
	addr root, var, init;

	for (root = Nil; cons != Nil; ) {
		/* (var init) */
		GetCons(cons, &init, &cons);
		GetCons(init, &var, &init);
		GetCar(init, &init);
		copy_eval_parse(local, &init, init);
		list_alloc(local, &init, var, init, NULL);
		cons_alloc(local, &root, init, root);
	}
	nreverse(ret, root);
}

static void copy_eval_ordinary(LocalRoot local, addr *ret, addr cons)
{
	addr var, opt, rest, key, allow, aux;

	List_bind(cons, &var, &opt, &rest, &key, &allow, &aux, NULL);
	copy_eval_ordinary_optional(local, &opt, opt);
	copy_eval_ordinary_key(local, &key, key);
	copy_eval_ordinary_aux(local, &aux, aux);
	list_alloc(local, ret, var, opt, rest, key, allow, aux, NULL);
}

static void copy_eval_defun(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr name, args, decl, doc, body, form;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DEFUN, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &name);
	GetEvalParse(eval, 2, &args);
	GetEvalParse(eval, 3, &decl);
	GetEvalParse(eval, 4, &doc);
	GetEvalParse(eval, 5, &body);

	copy_eval_ordinary(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_eval_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 6);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, name);
	SetEvalParse(eval, 2, args);
	SetEvalParse(eval, 3, decl);
	SetEvalParse(eval, 4, doc);
	SetEvalParse(eval, 5, body);
	*ret = eval;
}

/* defmacro */
static void copy_eval_defmacro(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr name, lambda;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DEFMACRO, "parse error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &lambda);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, lambda);
	*ret = eval;
}

/* macro-lambda */
static void copy_eval_macro_arguments(LocalRoot local, addr *ret, addr cons);
static void copy_eval_macro_var(LocalRoot local, addr *ret, addr list)
{
	addr root, var;

	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		if (consp(var))
			copy_eval_macro_arguments(local, &var, var);
		cons_alloc(local, &root, var, root);
	}
	nreverse(ret, root);
}

static void copy_eval_macro_rest(LocalRoot local, addr *ret, addr list)
{
	addr car, cdr;

	if (list != Nil) {
		GetCons(list, &car, &cdr);
		cons_heap(ret, car, cdr);
	}
}

static void copy_eval_macro_arguments(LocalRoot local, addr *ret, addr cons)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(cons, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	copy_eval_macro_var(local, &var, var);
	copy_eval_ordinary_optional(local, &opt, opt);
	copy_eval_macro_rest(local, &rest, rest);
	copy_eval_ordinary_key(local, &key, key);
	copy_eval_ordinary_aux(local, &aux, aux);
	list_alloc(local, ret, var, opt, rest, key, allow, aux, whole, env, NULL);
}

/* macro-lambda */
static void copy_eval_macro_lambda(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr args, decl, doc, body, call;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_MACRO_LAMBDA, "parse error");
	GetEvalParse(eval, 0, &args);
	GetEvalParse(eval, 1, &decl);
	GetEvalParse(eval, 2, &doc);
	GetEvalParse(eval, 3, &body);
	GetEvalParse(eval, 4, &call);

	copy_eval_macro_arguments(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_eval_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, doc);
	SetEvalParse(eval, 3, body);
	SetEvalParse(eval, 4, call);
	*ret = eval;
}

/* deftype */
static void copy_eval_deftype(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr name, args, decl, doc, body;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DEFTYPE, "parse error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &args);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &doc);
	GetEvalParse(eval, 4, &body);

	copy_eval_macro_arguments(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_eval_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, body);
	*ret = eval;
}

/* define-compiler-macro */
static void copy_eval_define_compiler_macro(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr name, args, decl, doc, body;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DEFINE_COMPILER_MACRO, "parse error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &args);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &doc);
	GetEvalParse(eval, 4, &body);

	copy_eval_macro_arguments(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_eval_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, body);
	*ret = eval;
}

/* destructuring-bind */
static void copy_eval_destructuring_bind(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, expr, lambda;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DESTRUCTURING_BIND, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &lambda);

	copy_eval_parse(local, &expr, expr);
	copy_eval_macro_lambda(local, &lambda, lambda);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, lambda);
	*ret = eval;
}

/* lambda */
static void copy_eval_lambda(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr args, decl, doc, cons, form;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LAMBDA, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &args);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &doc);
	GetEvalParse(eval, 4, &cons);

	copy_eval_ordinary(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, cons);
	*ret = eval;
}

/* if */
static void copy_eval_if(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, expr, then, last;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_IF, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &then);
	GetEvalParse(eval, 3, &last);

	copy_eval_parse(local, &expr, expr);
	copy_eval_parse(local, &then, then);
	copy_eval_parse(local, &last, last);

	eval_parse_alloc(local, &eval, type, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, then);
	SetEvalParse(eval, 3, last);
	*ret = eval;
}

/* unwind-protect */
static void copy_eval_unwind_protect(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, expr, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_UNWIND_PROTECT, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &cons);

	copy_eval_parse(local, &expr, expr);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* tagbody */
static void copy_eval_tagbody(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, tag, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_TAGBODY, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &tag);
	GetEvalParse(eval, 2, &cons);

	copy_eval_allcons(local, &tag, tag);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, tag);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* tag */
static void copy_eval_tag(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr tag;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_TAG, "parse error");
	GetEvalParse(eval, 0, &tag);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, tag);
	*ret = eval;
}

/* block */
static void copy_eval_block(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, name, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_BLOCK, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &name);
	GetEvalParse(eval, 2, &cons);

	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, name);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* return-from */
static void copy_eval_return_from(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, name, value;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_RETURN_FROM, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &name);
	GetEvalParse(eval, 2, &value);

	copy_eval_parse(local, &value, value);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, name);
	SetEvalParse(eval, 2, value);
	*ret = eval;
}

/* catch */
static void copy_eval_catch(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, tag, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_CATCH, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &tag);
	GetEvalParse(eval, 2, &cons);

	copy_eval_parse(local, &tag, tag);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, tag);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* throw */
static void copy_eval_throw(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, tag, result;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_THROW, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &tag);
	GetEvalParse(eval, 2, &result);

	copy_eval_parse(local, &tag, tag);
	copy_eval_parse(local, &result, result);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, tag);
	SetEvalParse(eval, 2, result);
	*ret = eval;
}

/* flet / labels */
static void copy_eval_flet_one(LocalRoot local, addr *ret, addr cons)
{
	addr name, args, decl, doc, body;

	List_bind(cons, &name, &args, &decl, &doc, &body, NULL);

	copy_eval_ordinary(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_eval_allcons(local, &body, body);

	list_alloc(local, ret, name, args, decl, doc, body, NULL);
}

static void copy_eval_flet_args(LocalRoot local, addr *ret, addr cons)
{
	addr root, pos;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		copy_eval_flet_one(local, &pos, pos);
		cons_alloc(local, &root, pos, root);
	}
	nreverse(ret, root);
}

static void copy_eval_flet(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, args, decl, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_FLET && type != EVAL_PARSE_LABELS, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &args);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &cons);

	copy_eval_flet_args(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, cons);
	*ret = eval;
}

/* the */
static void copy_eval_the(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, the, expr;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_THE, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &the);
	GetEvalParse(eval, 2, &expr);

	type_copy_alloc(local, &the, the);
	copy_eval_parse(local, &expr, expr);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, the);
	SetEvalParse(eval, 2, expr);
	*ret = eval;
}

/* when */
static void copy_eval_eval_when(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, cons, compile, load, exec, toplevel, mode;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_EVAL_WHEN, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &cons);
	GetEvalParse(eval, 2, &compile);
	GetEvalParse(eval, 3, &load);
	GetEvalParse(eval, 4, &exec);
	GetEvalParse(eval, 5, &toplevel);
	GetEvalParse(eval, 6, &mode);

	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 7);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, cons);
	SetEvalParse(eval, 2, compile);
	SetEvalParse(eval, 3, load);
	SetEvalParse(eval, 4, exec);
	SetEvalParse(eval, 5, toplevel);
	SetEvalParse(eval, 6, mode);
	*ret = eval;
}

/* values */
static void copy_eval_values(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_VALUES, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &eval);
	copy_eval_allcons(local, &eval, eval);
	eval_parse2_alloc(local, ret, type, form, eval);
}

/* locally */
static void copy_eval_locally(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, decl, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LOCALLY, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &decl);
	GetEvalParse(eval, 2, &cons);

	copy_eval_declaim_nil(local, &decl, decl);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* call */
static void copy_eval_call(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, call, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_CALL, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &call);
	GetEvalParse(eval, 2, &cons);

	copy_eval_parse(local, &call, call);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, call);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* multiple-value-bind */
static void copy_eval_multiple_value_bind(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, vars, expr, decl, doc, body;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_MULTIPLE_VALUE_BIND, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &vars);
	GetEvalParse(eval, 2, &expr);
	GetEvalParse(eval, 3, &decl);
	GetEvalParse(eval, 4, &doc);
	GetEvalParse(eval, 5, &body);

	copy_eval_parse(local, &expr, expr);
	copy_eval_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 6);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, vars);
	SetEvalParse(eval, 2, expr);
	SetEvalParse(eval, 3, decl);
	SetEvalParse(eval, 4, doc);
	SetEvalParse(eval, 5, body);
	*ret = eval;
}

/* multiple-value-call */
static void copy_eval_multiple_value_call(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, call, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_MULTIPLE_VALUE_CALL, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &call);
	GetEvalParse(eval, 2, &cons);

	copy_eval_parse(local, &call, call);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, call);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* multiple-value-prog1 */
static void copy_eval_multiple_value_prog1(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, call, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_MULTIPLE_VALUE_PROG1, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &call);
	GetEvalParse(eval, 2, &cons);

	copy_eval_parse(local, &call, call);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, call);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* nth-value */
static void copy_eval_nth_value(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, nth, expr;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_NTH_VALUE, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &nth);
	GetEvalParse(eval, 2, &expr);

	copy_eval_parse(local, &nth, nth);
	copy_eval_parse(local, &expr, expr);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, nth);
	SetEvalParse(eval, 2, expr);
	*ret = eval;
}

/* progv */
static void copy_eval_progv(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, symbols, values, body;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_PROGV, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &symbols);
	GetEvalParse(eval, 2, &values);
	GetEvalParse(eval, 3, &body);

	copy_eval_parse(local, &symbols, symbols);
	copy_eval_parse(local, &values, values);
	copy_eval_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, symbols);
	SetEvalParse(eval, 2, values);
	SetEvalParse(eval, 3, body);
	*ret = eval;
}


/*
 *  interface
 */
void copy_eval_parse_alloc(LocalRoot local, addr *ret, addr eval)
{
	copy_eval_parse(local, ret, eval);
}

void copy_eval_parse_local(LocalRoot local, addr *ret, addr eval)
{
	Check(local == NULL, "local error");
	copy_eval_parse_alloc(local, ret, eval);
}

void copy_eval_parse_heap(addr *ret, addr eval)
{
	copy_eval_parse_alloc(NULL, ret, eval);
}


/*
 *  initialize
 */
typedef void (*copy_eval_calltype)(LocalRoot, addr *, addr);
static copy_eval_calltype EvalCopyTable[EVAL_PARSE_SIZE];

void copy_eval_parse(LocalRoot local, addr *ret, addr pos)
{
	EvalParse type;
	copy_eval_calltype call;

	Check(! eval_parse_p(pos), "type error");
	GetEvalParseType(pos, &type);
	if (EVAL_PARSE_SIZE <= type)
		goto error;
	call = EvalCopyTable[type];
	if (call == NULL)
		goto error;
	(*call)(local, ret, pos);
	return;

error:
	*ret = Nil;
	infobit(pos);
	Abort("parse-error.");
}

void init_eval_copy(void)
{
	EvalCopyTable[EVAL_PARSE_NIL] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_T] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_CLOS] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_INTEGER] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_RATIONAL] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_COMPLEX] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_CHARACTER] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_ARRAY] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_VECTOR] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_BITVECTOR] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_STRING] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_SYMBOL] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_FLOAT] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_FUNCTION] = copy_eval_double;
	EvalCopyTable[EVAL_PARSE_PACKAGE] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_RANDOM_STATE] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_PATHNAME] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_ENVIRONMENT] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_PAPER] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_QUOTE] = copy_eval_double;
	EvalCopyTable[EVAL_PARSE_GO] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_DECLAIM] = copy_eval_declaim;
	EvalCopyTable[EVAL_PARSE_PROGN] = copy_eval_progn;
	EvalCopyTable[EVAL_PARSE_LET] = copy_eval_let;
	EvalCopyTable[EVAL_PARSE_LETA] = copy_eval_let;
	EvalCopyTable[EVAL_PARSE_SETQ] = copy_eval_setq;
	EvalCopyTable[EVAL_PARSE_DEFUN] = copy_eval_defun;
	EvalCopyTable[EVAL_PARSE_DEFMACRO] = copy_eval_defmacro;
	EvalCopyTable[EVAL_PARSE_MACRO_LAMBDA] = copy_eval_macro_lambda;
	EvalCopyTable[EVAL_PARSE_DEFTYPE] = copy_eval_deftype;
	EvalCopyTable[EVAL_PARSE_DEFINE_COMPILER_MACRO] = copy_eval_define_compiler_macro;
	EvalCopyTable[EVAL_PARSE_DESTRUCTURING_BIND] = copy_eval_destructuring_bind;
	EvalCopyTable[EVAL_PARSE_LAMBDA] = copy_eval_lambda;
	EvalCopyTable[EVAL_PARSE_IF] = copy_eval_if;
	EvalCopyTable[EVAL_PARSE_UNWIND_PROTECT] = copy_eval_unwind_protect;
	EvalCopyTable[EVAL_PARSE_TAGBODY] = copy_eval_tagbody;
	EvalCopyTable[EVAL_PARSE_TAG] = copy_eval_tag;
	EvalCopyTable[EVAL_PARSE_BLOCK] = copy_eval_block;
	EvalCopyTable[EVAL_PARSE_RETURN_FROM] = copy_eval_return_from;
	EvalCopyTable[EVAL_PARSE_CATCH] = copy_eval_catch;
	EvalCopyTable[EVAL_PARSE_THROW] = copy_eval_throw;
	EvalCopyTable[EVAL_PARSE_FLET] = copy_eval_flet;
	EvalCopyTable[EVAL_PARSE_LABELS] = copy_eval_flet;
	EvalCopyTable[EVAL_PARSE_THE] = copy_eval_the;
	EvalCopyTable[EVAL_PARSE_EVAL_WHEN] = copy_eval_eval_when;
	EvalCopyTable[EVAL_PARSE_VALUES] = copy_eval_values;
	EvalCopyTable[EVAL_PARSE_LOCALLY] = copy_eval_locally;
	EvalCopyTable[EVAL_PARSE_CALL] = copy_eval_call;
	EvalCopyTable[EVAL_PARSE_MULTIPLE_VALUE_BIND] = copy_eval_multiple_value_bind;
	EvalCopyTable[EVAL_PARSE_MULTIPLE_VALUE_CALL] = copy_eval_multiple_value_call;
	EvalCopyTable[EVAL_PARSE_MULTIPLE_VALUE_PROG1] = copy_eval_multiple_value_prog1;
	EvalCopyTable[EVAL_PARSE_NTH_VALUE] = copy_eval_nth_value;
	EvalCopyTable[EVAL_PARSE_PROGV] = copy_eval_progv;
	EvalCopyTable[EVAL_PARSE_LOAD_TIME_VALUE] = copy_eval_load_time_value;
	EvalCopyTable[EVAL_PARSE_STEP] = copy_eval_step;
}


/************************************************************
 *  eval_execute.c
 ************************************************************/

/*
 *  begin, end
 */
static void begin_eval(Execute ptr, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	init_parse_step(ptr);
	init_parse_environment(ptr);
	push_toplevel_eval(ptr, T);
	push_compile_time_eval(ptr, Nil);
	push_compile_toplevel_eval(ptr, Nil);
	push_load_toplevel_eval(ptr, T);
	push_execute_eval(ptr, T);
	push_parse_declare(ptr, Nil);
	disable_load_time_value(ptr);
	*ret = control;
}


/*
 *  eval
 */
static int eval_execute_scope_(Execute ptr, LocalHold hold, addr pos)
{
	/* optimize */
	localhold_set(hold, 0, pos);
	Return(optimize_parse_(ptr, pos, &pos, NULL));

	/* scope */
	localhold_set(hold, 0, pos);
	Return(eval_scope_(ptr, &pos, pos));

	/* code */
	localhold_set(hold, 0, pos);
	Return(code_make_(ptr, &pos, pos));

	/* execute */
	localhold_set(hold, 0, pos);
	return runcode_control_(ptr, pos);
}

static int eval_execute_parse_(Execute ptr, addr pos)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	Return(parse_execute_(ptr, &pos, pos));
	Return(eval_execute_scope_(ptr, hold, pos));
	localhold_end(hold);

	return 0;
}

static int eval_execute_(Execute ptr, addr pos, addr compiler_macro)
{
	addr control;

	push_control(ptr, &control);
	push_enable_compiler_macro(ptr, compiler_macro);
	(void)eval_execute_parse_(ptr, pos);
	return pop_control_(ptr, control);
}


/*
 *  interface
 */
static int eval_result_partial_call_(Execute ptr, LocalHold hold, addr pos, addr *ret)
{
	localhold_set(hold, 0, pos);
	Return(eval_execute_(ptr, pos, Nil));
	getresult_control(ptr, ret);
	localhold_set(hold, 1, *ret);

	return 0;
}

int eval_result_partial_(Execute ptr, addr pos, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	begin_eval(ptr, &control);
	(void)eval_result_partial_call_(ptr, hold, pos, &pos);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return Result(ret, pos);
}

static int eval_result_partial_form_call_(Execute ptr, addr pos, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	push_control(ptr, &control);
	set_eval_compile_mode(ptr, Nil); /* Don't run compile mode. */
	(void)eval_result_partial_call_(ptr, hold, pos, &pos);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return Result(ret, pos);
}

int eval_result_partial_form_(Execute ptr, addr pos, addr *ret)
{
	addr control;

	begin_eval(ptr, &control);
	(void)eval_result_partial_form_call_(ptr, pos, ret);
	return pop_control_(ptr, control);
}

int eval_result_compile_(Execute ptr, addr pos, addr *ret)
{
	addr control;

	begin_eval(ptr, &control);
	set_eval_compile_mode(ptr, Nil); /* Don't run compile mode. */
	gchold_push_special(ptr, pos);
	if (eval_execute_(ptr, pos, T))
		goto escape;
	getresult_control(ptr, ret);
escape:
	return pop_control_(ptr, control);
}

static int eval_result_macro_call_(Execute ptr, LocalHold hold, addr pos, addr *ret)
{
	localhold_set(hold, 0, pos);
	Return(eval_execute_scope_(ptr, hold, pos));
	getresult_control(ptr, ret);
	localhold_set(hold, 1, *ret);

	return 0;
}

int eval_result_macro_(Execute ptr, addr pos, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	localhold_push(hold, pos);
	push_control(ptr, &control);
	(void)eval_result_macro_call_(ptr, hold, pos, &pos);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return Result(ret, pos);
}


/*
 *  eval-stream
 */
static int eval_toplevel_execute_(Execute ptr, addr pos);

/* progn */
static int eval_toplevel_progn_(Execute ptr, addr list)
{
	addr pos;
	LocalHold hold;

	/* (progn) -> nil */
	if (list == Nil)
		return eval_toplevel_execute_(ptr, Nil);

	hold = LocalHold_local_push(ptr, list);
	/* (progn ...) */
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(eval_toplevel_execute_(ptr, pos));
	}
	localhold_end(hold);

	return 0;
}


/* locally */
static int eval_toplevel_locally_call_(Execute ptr, addr decl, addr body)
{
	addr list, control;

	if (decl == Nil)
		return eval_toplevel_progn_(ptr, body);

	/* addr *parse-declare* */
	Return(add_parse_declare_(ptr, decl, &list));
	push_control(ptr, &control);
	push_parse_declare(ptr, list);
	(void)eval_toplevel_progn_(ptr, body);
	return pop_control_(ptr, control);
}

static int eval_toplevel_locally_(Execute ptr, addr list)
{
	addr decl;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_push(hold, list);
	Return(parse_declare_body_(ptr, list, &decl, &list));
	localhold_pushva(hold, decl, list, NULL);
	Return(eval_toplevel_locally_call_(ptr, decl, list));
	localhold_end(hold);

	return 0;
}


/* eval-when */
static int eval_toplevel_eval_when_call_(Execute ptr, addr list)
{
	addr args, compile, load, exec;
	addr compile1, load1, exec1, ctime1;

	if (! consp_getcons(list, &args, &list))
		return fmte_("eval-when form must be (eval-when (...) . body).", NULL);

	/* arguments */
	Return(parse_eval_when_list_(args, &compile, &load, &exec));

	/* backup */
	Return(get_compile_toplevel_eval_(ptr, &compile1));
	Return(get_load_toplevel_eval_(ptr, &load1));
	Return(get_execute_eval_(ptr, &exec1));
	Return(get_compile_time_eval_(ptr, &ctime1));

	/* set variable */
	set_compile_toplevel_eval(ptr, compile);
	set_load_toplevel_eval(ptr, load);
	set_execute_eval(ptr, exec);

	/* discard */
	if (! parse_eval_when_process(ptr, compile, load, exec, T, ctime1))
		return eval_toplevel_execute_(ptr, Nil);

	/* execute */
	Return(eval_toplevel_progn_(ptr, list));

	/* rollback */
	set_compile_toplevel_eval(ptr, compile1);
	set_load_toplevel_eval(ptr, load1);
	set_execute_eval(ptr, exec1);
	set_compile_time_eval(ptr, ctime1);

	return 0;
}

static int eval_toplevel_eval_when_(Execute ptr, addr pos)
{
	addr control;

	push_control(ptr, &control);
	(void)eval_toplevel_eval_when_call_(ptr, pos);
	return pop_control_(ptr, control);
}


/* macrolet */
static int eval_toplevel_macrolet_(Execute ptr, addr list)
{
	addr args, decl, rollback;
	LocalHold hold;

	if (! consp_getcons(list, &args, &list))
		return fmte_("macrolet form must be (macrolet args . body).", NULL);

	/* local scope environment */
	Return(snapshot_envstack_(ptr, &rollback));
	Return(parse_macrolet_args_(ptr, args));

	/* decl */
	hold = LocalHold_local(ptr);
	Return(parse_declare_body_(ptr, list, &decl, &list));
	localhold_pushva(hold, decl, list, NULL);

	/* body */
	Return(eval_toplevel_locally_call_(ptr, decl, list));
	localhold_end(hold);
	return rollback_envstack_(ptr, rollback);
}


/* symbol-macrolet */
static int eval_toplevel_symbol_macrolet_(Execute ptr, addr list)
{
	addr args, decl, rollback;
	LocalHold hold;

	if (! consp_getcons(list, &args, &list)) {
		return fmte_("symbol-macrolet form must be "
				"(symbol-macrolet args . body).", NULL);
	}

	/* local scope environment */
	Return(snapshot_envstack_(ptr, &rollback));

	/* decl */
	hold = LocalHold_local(ptr);
	Return(parse_declare_body_(ptr, list, &decl, &list));
	localhold_pushva(hold, decl, list, NULL);

	/* args */
	Return(parse_symbol_macrolet_args_(ptr, args, decl));

	/* body */
	Return(eval_toplevel_locally_call_(ptr, decl, list));
	localhold_end(hold);
	return rollback_envstack_(ptr, rollback);
}


/* value */
static int eval_execute_value_(Execute ptr, addr value)
{
	addr pos;
	LocalHold hold;

	/* parse */
	hold = LocalHold_array(ptr, 2);
	localhold_set(hold, 1, value);
	Return(parse_execute_(ptr, &pos, value));

	/* optimize */
	localhold_set(hold, 0, pos);
	Return(optimize_parse_(ptr, pos, &pos, NULL));

	/* scope */
	localhold_set(hold, 0, pos);
	Return(eval_scope_(ptr, &pos, pos));

	/* code */
	localhold_set(hold, 0, pos);
	Return(code_make_(ptr, &pos, pos));

	/* close *parse-declare* */
	set_parse_declare(ptr, Nil);

	/* execute */
	localhold_set(hold, 0, pos);
	Return(runcode_control_(ptr, pos));
	localhold_end(hold);

	return 0;
}

static int eval_toplevel_value_(Execute ptr, addr pos)
{
	if (eval_compile_p(ptr))
		return compile_eval_value_(ptr, pos);
	else
		return eval_execute_value_(ptr, pos);
}


/* toplevel */
static int eval_toplevel_cons_(Execute ptr, addr cons)
{
	addr car, cdr, check;

	GetCons(cons, &car, &cdr);

	/* macro */
	Return(parse_macroexpand_(ptr, &check, cons));
	if (check != Unbound)
		return eval_toplevel_execute_(ptr, check);

	/* progn */
	GetConst(COMMON_PROGN, &check);
	if (car == check)
		return eval_toplevel_progn_(ptr, cdr);

	/* locally */
	GetConst(COMMON_LOCALLY, &check);
	if (car == check)
		return eval_toplevel_locally_(ptr, cdr);

	/* evan-when */
	GetConst(COMMON_EVAL_WHEN, &check);
	if (car == check)
		return eval_toplevel_eval_when_(ptr, cdr);

	/* macrolet */
	GetConst(COMMON_MACROLET, &check);
	if (car == check)
		return eval_toplevel_macrolet_(ptr, cdr);

	/* symbol-macrolet */
	GetConst(COMMON_SYMBOL_MACROLET, &check);
	if (car == check)
		return eval_toplevel_symbol_macrolet_(ptr, cdr);

	/* function */
	return eval_toplevel_value_(ptr, cons);
}

static int eval_toplevel_execute_call_(Execute ptr, addr pos)
{
	int check;
	addr value;

	if (consp(pos))
		return eval_toplevel_cons_(ptr, pos);

	if (! symbolp(pos))
		return eval_toplevel_value_(ptr, pos);

	/* symbol */
	Return(symbol_macrolet_envstack_p_(ptr, pos, &value, &check));
	if (check)
		return eval_toplevel_execute_(ptr, value);

	return eval_toplevel_value_(ptr, pos);
}

static int eval_toplevel_print_char_(Execute ptr, addr pos, addr stream, const char *str)
{
	int ignore;

	Return(fresh_line_stream_(stream, &ignore));
	Return(print_ascii_stream_(stream, str));
	Return(prin1_print_(ptr, stream, pos));
	Return(terpri_stream_(stream));
	Return(finish_output_stream_(stream));

	return 0;
}

static int eval_toplevel_print_output_(Execute ptr, addr pos, int load, int compile)
{
	addr symbol, value, stream;

	/* (setq *print-level* 2) */
	GetConst(SPECIAL_PRINT_LEVEL, &symbol);
	fixnum_heap(&value, 2);
	pushspecial_control(ptr, symbol, value);

	/* (setq *print-length* 3) */
	GetConst(SPECIAL_PRINT_LENGTH, &symbol);
	fixnum_heap(&value, 3);
	pushspecial_control(ptr, symbol, value);

	/* (format *standard-output* "~&;; Load: ~S~%") */
	Return(standard_output_stream_(ptr, &stream));
	if (load) {
		Return(eval_toplevel_print_char_(ptr, pos, stream, ";; Load: "));
	}

	/* (format *standard-output* "~&;; Compile: ~S~%") */
	if (compile) {
		Return(eval_toplevel_print_char_(ptr, pos, stream, ";; Compile: "));
	}

	return 0;
}

static int eval_toplevel_print_(Execute ptr, addr pos)
{
	int check1, check2;
	addr control, symbol, value;

	/* load */
	GetConst(SPECIAL_LOAD_PRINT, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &value));
	check1 = (value != Nil);

	/* compile-file */
	GetConst(SPECIAL_COMPILE_PRINT, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &value));
	check2 = (value != Nil);

	/* print */
	if (check1 || check2) {
		push_control(ptr, &control);
		(void)eval_toplevel_print_output_(ptr, pos, check1, check2);
		return pop_control_(ptr, control);
	}
	return 0;
}

static int eval_toplevel_execute_(Execute ptr, addr pos)
{
	LocalHold hold;

	hold = LocalHold_local_push(ptr, pos);
	Return(eval_toplevel_print_(ptr, pos));
	Return(eval_toplevel_execute_call_(ptr, pos));
	localhold_end(hold);

	return 0;
}

int eval_toplevel_loop_(Execute ptr, addr stream)
{
	int check;
	addr pos;

	for (;;) {
		Return(read_stream_(ptr, stream, &check, &pos));
		if (check)
			break;
		Return(eval_toplevel_execute_(ptr, pos));
	}

	return 0;
}

int eval_stream_toplevel_(Execute ptr, addr stream)
{
	addr control;

	begin_eval(ptr, &control);
	(void)eval_toplevel_loop_(ptr, stream);
	return pop_control_(ptr, control);
}

int eval_execute_partial_(Execute ptr, addr pos)
{
	addr control;

	begin_eval(ptr, &control);
	set_eval_compile_mode(ptr, Nil);
	(void)eval_toplevel_execute_(ptr, pos);
	return pop_control_(ptr, control);
}


/************************************************************
 *  eval_load.c
 ************************************************************/

/*
 *  verbose
 */
static int load_file_verbose_(Execute ptr, addr file,
		constindex index1, constindex index2,
		const char *str1, const char *str2)
{
	int ignore;
	addr pos, verbose, stream;

	/* filename */
	GetConstant(index1, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Unbound || pos == Nil)
		pos = file;

	/* verbose */
	GetConstant(index2, &verbose);
	getspecial_local(ptr, verbose, &verbose);
	if (verbose == Nil)
		return 0;
	if (verbose == Unbound)
		return 0;

	/* output */
	Return(standard_output_stream_(ptr, &stream));
	Return(fresh_line_stream_(stream, &ignore));
	Return(print_ascii_stream_(stream, str1));
	Return(prin1_print_(ptr, stream, pos));
	Return(print_ascii_stream_(stream, str2));
	Return(terpri_stream_(stream));
	Return(finish_output_stream_(stream));

	return 0;
}

static int compile_file_begin_(Execute ptr, addr file)
{
	return load_file_verbose_(ptr, file,
			CONSTANT_SPECIAL_COMPILE_FILE_PATHNAME,
			CONSTANT_SPECIAL_COMPILE_VERBOSE,
			";; Compiling file ", " ...");
}
static int compile_file_end_(Execute ptr, addr file)
{
	return load_file_verbose_(ptr, file,
			CONSTANT_SPECIAL_COMPILE_FILE_PATHNAME,
			CONSTANT_SPECIAL_COMPILE_VERBOSE,
			";; Wrote file ", ".");
}

static int load_common_begin_(Execute ptr, addr file)
{
	return load_file_verbose_(ptr, file,
			CONSTANT_SPECIAL_LOAD_PATHNAME,
			CONSTANT_SPECIAL_LOAD_VERBOSE,
			";; Loading file ", " ...");
}
static int load_common_end_(Execute ptr, addr file)
{
	return load_file_verbose_(ptr, file,
			CONSTANT_SPECIAL_LOAD_PATHNAME,
			CONSTANT_SPECIAL_LOAD_VERBOSE,
			";; Loaded file ", ".");
}


/*
 *  eval-load
 */
static int eval_load_fasl_p_(addr file, int *ret)
{
	int check;

	if (streamp(file))
		return Result(ret, 0);
	GetTypePathname(file, &file);
	if (! stringp(file))
		return Result(ret, 0);
	Return(string_equalp_char_(file, "fasl", &check));
	if (check)
		return Result(ret, 1);
	else
		return string_equalp_char_(file, "fas", ret);
}

static int eval_load_open_file_(Execute ptr, addr *ret,
		addr file, addr external, int binary)
{
	if (binary)
		return open_input_binary_stream_(ptr, ret, file);
	else
		return open_input_stream_(ptr, ret, file, external);
}

static int eval_load_open_(Execute ptr,
		addr file, addr external, int exist, int binary,
		int *openp, int *closep, addr *ret)
{
	addr stream;

	/* stream */
	if (streamp(file) && (! memory_stream_p(file))) {
		*openp = 1;
		*closep = 0;
		return Result(ret, file);
	}

	/* open pathname */
	Return(eval_load_open_file_(ptr, &stream, file, external, binary));
	if (stream != NULL) {
		*openp = 1;
		*closep = 1;
		return Result(ret, stream);
	}

	/* file-error */
	if (exist) {
		*ret = Nil;
		*openp = *closep = 0;
		return call_simple_file_error_va_(ptr, file,
				"Cannot open file ~S.", file, NULL);
	}

	/* file is not open */
	*openp = 0;
	*closep = 0;
	return Result(ret, Nil);
}

static int eval_load_fasl_call_(Execute ptr, addr file, int closep)
{
	int escape;

	gchold_push_local(ptr->local, file);
	escape = eval_compile_load_(ptr, file);
	if (! closep)
		return escape;

	return close_stream_unwind_protect_(ptr, file);
}

static int eval_load_fasl_(Execute ptr, int *ret, addr file, int exist)
{
	int openp, closep;
	addr control;

	Return(eval_load_open_(ptr, file, Unbound, exist, 1, &openp, &closep, &file));
	if (! openp)
		return Result(ret, 0);

	/* open */
	push_control(ptr, &control);
	(void)eval_load_fasl_call_(ptr, file, closep);
	Return(pop_control_(ptr, control));
	return Result(ret, 1);
}

static int eval_load_lisp_call_(Execute ptr, addr file, int closep)
{
	int escape;

	gchold_push_local(ptr->local, file);
	escape = eval_stream_toplevel_(ptr, file);
	if (! closep)
		return escape;

	return close_stream_unwind_protect_(ptr, file);
}

static int eval_load_lisp_(Execute ptr, int *ret, addr file, addr external, int exist)
{
	int openp, closep;
	addr control;

	Return(eval_load_open_(ptr, file, external, exist, 0, &openp, &closep, &file));
	if (! openp)
		return Result(ret, 0);

	/* open */
	push_control(ptr, &control);
	(void)eval_load_lisp_call_(ptr, file, closep);
	Return(pop_control_(ptr, control));
	return Result(ret, 1);
}

static int eval_load_check_nil_p(addr type)
{
	addr check;
	size_t size;

	/* nil */
	if (type == Nil)
		return 1;

	/* :unspecific */
	GetConst(KEYWORD_UNSPECIFIC, &check);
	if (type == check)
		return 1;

	/* "" */
	if (! stringp(type))
		return 0;
	string_length(type, &size);
	if (size == 0)
		return 1;

	/* otherwise */
	return 0;
}

static int eval_load_check_type_(Execute ptr, addr file, int loadp, addr *ret)
{
	addr pos;

	/* *. */
	Return(probe_file_files_(ptr, &pos, file));
	if (pos != Nil)
		return Result(ret, file);

	/* :type nil */
	GetTypePathname(file, &pos);
	if (! eval_load_check_nil_p(pos))
		return Result(ret, file);
	copy_pathname_heap(&file, file);

	/* *.fasl */
	if (! loadp) {
		strvect_char_heap(&pos, "fasl");
		SetTypePathname(file, pos);
		Return(probe_file_files_(ptr, &pos, file));
		if (pos != Nil)
			return Result(ret, file);
	}

	/* *.lisp */
	strvect_char_heap(&pos, "lisp");
	SetTypePathname(file, pos);
	Return(probe_file_files_(ptr, &pos, file));
	if (pos != Nil)
		return Result(ret, file);

	/* do nothing */
	return Result(ret, file);
}

static int eval_load_check_(
		Execute ptr, addr file, addr verbose, addr print, int loadp,
		constindex file_pathname,
		constindex file_truename,
		constindex file_verbose,
		constindex file_print,
		addr *ret)
{
	int check;
	addr symbol, pos, truename, value;

	/* wild-pathname-p */
	if (! streamp(file)) {
		Return(pathname_designator_heap_(ptr, file, &file));
		Return(wild_pathname_boolean_(file, Nil, &check));
		if (check ) {
			return call_simple_file_error_va_(ptr, file,
					"LOAD don't allow the wildcard filename ~S.", file, NULL);
		}
	}
	/* type */
	if (! streamp(file)) {
		Return(eval_load_check_type_(ptr, file, loadp, &file));
	}
	/* load-pathname */
	GetConstant(file_pathname, &symbol);
	if (streamp(file)) {
		GetPathnameStream(file, &value);
		if (memory_stream_p(value)) {
			value = Nil;
		}
		else if (value != Nil) {
			Return(physical_pathname_heap_(ptr, file, &value));
		}
	}
	else {
		Return(physical_pathname_heap_(ptr, file, &file));
		value = file;
	}
	pushspecial_control(ptr, symbol, value);
	/* load-truename */
	GetConstant(file_truename, &symbol);
	if (value != Nil) {
		Return(truename_files_(ptr, value, &truename, 0));
		pushspecial_control(ptr, symbol, truename);
	}
	else {
		pushspecial_control(ptr, symbol, Nil);
	}
	/* package */
	GetConst(SPECIAL_PACKAGE, &symbol);
	getspecial_local(ptr, symbol, &pos);
	pushspecial_control(ptr, symbol, pos);
	/* readtable */
	GetConst(SPECIAL_READTABLE, &symbol);
	getspecial_local(ptr, symbol, &pos);
	pushspecial_control(ptr, symbol, pos);
	/* verbose */
	if (verbose != Unbound) {
		GetConstant(file_verbose, &symbol);
		pushspecial_control(ptr, symbol, verbose);
	}
	/* print */
	if (print != Unbound) {
		GetConstant(file_print, &symbol);
		pushspecial_control(ptr, symbol, print);
	}

	/* result */
	return Result(ret, file);
}

static int eval_load_type_(Execute ptr, int *ret,
		addr file, addr external, int exist, int faslp)
{
	Return(load_common_begin_(ptr, file));
	if (faslp) {
		Return(eval_load_fasl_(ptr, ret, file, exist));
	}
	else {
		Return(eval_load_lisp_(ptr, ret, file, external, exist));
	}
	Return(load_common_end_(ptr, file));

	return 0;
}

static int eval_load_file_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist,
		addr external)
{
	int faslp;

	Return(eval_load_check_(ptr, file, verbose, print, 1,
				CONSTANT_SPECIAL_LOAD_PATHNAME,
				CONSTANT_SPECIAL_LOAD_TRUENAME,
				CONSTANT_SPECIAL_LOAD_VERBOSE,
				CONSTANT_SPECIAL_LOAD_PRINT,
				&file));
	Return(eval_load_fasl_p_(file, &faslp));
	return eval_load_type_(ptr, ret, file, external, exist, faslp);
}

int eval_load_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist, addr external)
{
	addr control;

	push_control(ptr, &control);
	set_eval_compile_mode(ptr, Nil);
	(void)eval_load_file_(ptr, ret, file, verbose, print, exist, external);
	return pop_control_(ptr, control);
}

static int eval_load_file_switch_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist,
		addr external, int faslp)
{
	Return(eval_load_check_(ptr, file, verbose, print, 1,
				CONSTANT_SPECIAL_LOAD_PATHNAME,
				CONSTANT_SPECIAL_LOAD_TRUENAME,
				CONSTANT_SPECIAL_LOAD_VERBOSE,
				CONSTANT_SPECIAL_LOAD_PRINT,
				&file));
	return eval_load_type_(ptr, ret, file, external, exist, faslp);
}

int eval_load_force_lisp_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist, addr external)
{
	addr control;

	push_control(ptr, &control);
	set_eval_compile_mode(ptr, Nil);
	(void)eval_load_file_switch_(ptr, ret, file, verbose, print, exist, external, 0);
	return pop_control_(ptr, control);
}

int eval_load_force_fasl_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist, addr external)
{
	addr control;

	push_control(ptr, &control);
	set_eval_compile_mode(ptr, Nil);
	(void)eval_load_file_switch_(ptr, ret, file, verbose, print, exist, external, 1);
	return pop_control_(ptr, control);
}


/*
 *  compile-file
 */
static int compile_load_lisp_call_(Execute ptr, addr file, int closep)
{
	int escape;

	gchold_push_local(ptr->local, file);
	escape = compile_load_stream_(ptr, file);
	if (! closep)
		return escape;

	return close_stream_unwind_protect_(ptr, file);
}

static int compile_load_lisp_(Execute ptr, int *ret, addr file, addr external, int exist)
{
	int openp, closep;
	addr control;

	Return(eval_load_open_(ptr, file, external, exist, 0, &openp, &closep, &file));
	if (! openp)
		return Result(ret, 0);

	/* open */
	push_control(ptr, &control);
	(void)compile_load_lisp_call_(ptr, file, closep);
	Return(pop_control_(ptr, control));
	return Result(ret, 1);
}

static int compile_load_file_(Execute ptr,
		addr file, addr verbose, addr print, addr external)
{
	int ignore;
	addr input;

	Return(eval_load_check_(ptr, file, verbose, print, 0,
				CONSTANT_SPECIAL_COMPILE_FILE_PATHNAME,
				CONSTANT_SPECIAL_COMPILE_FILE_TRUENAME,
				CONSTANT_SPECIAL_COMPILE_VERBOSE,
				CONSTANT_SPECIAL_COMPILE_PRINT,
				&input));
	Return(compile_file_begin_(ptr, file));
	Return(compile_load_lisp_(ptr, &ignore, input, external, 1));
	Return(compile_file_end_(ptr, file));

	return 0;
}

int compile_load_(Execute ptr, addr file, addr verbose, addr print, addr external)
{
	addr control;

	push_control(ptr, &control);
	(void)compile_load_file_(ptr, file, verbose, print, external);
	return pop_control_(ptr, control);
}


/************************************************************
 *  eval_main.c
 ************************************************************/

/*
 *  restart abort
 */
static void eval_main_restart_abort(addr *ret)
{
	abort_restart_char_heap(ret, "Return to eval-loop.");
}


/*
 *  eval-loop
 */
static void eval_loop_minus(Execute ptr, addr value)
{
	addr symbol;

	GetConst(COMMON_MINUS, &symbol);
	setspecial_local(ptr, symbol, value);
}

static void eval_loop_shift(Execute ptr, addr list)
{
	addr sym0, sym1, sym2, sym3, pos1, pos2, pos3;

	/* +, ++, +++ */
	GetConst(COMMON_MINUS, &sym0);
	GetConst(COMMON_PLUS,  &sym1);
	GetConst(COMMON_PLUS2, &sym2);
	GetConst(COMMON_PLUS3, &sym3);
	getspecial_local(ptr, sym0, &pos1);
	getspecial_local(ptr, sym1, &pos2);
	getspecial_local(ptr, sym2, &pos3);
	setspecial_local(ptr, sym1, pos1);
	setspecial_local(ptr, sym2, pos2);
	setspecial_local(ptr, sym3, pos3);

	/* *, **, *** */
	GetConst(COMMON_ASTERISK,  &sym1);
	GetConst(COMMON_ASTERISK2, &sym2);
	GetConst(COMMON_ASTERISK3, &sym3);
	GetCar(list, &pos1);
	getspecial_local(ptr, sym1, &pos2);
	getspecial_local(ptr, sym2, &pos3);
	setspecial_local(ptr, sym1, pos1);
	setspecial_local(ptr, sym2, pos2);
	setspecial_local(ptr, sym3, pos3);

	/* /, //, /// */
	GetConst(COMMON_SLASH,  &sym1);
	GetConst(COMMON_SLASH2, &sym2);
	GetConst(COMMON_SLASH3, &sym3);
	getspecial_local(ptr, sym1, &pos2);
	getspecial_local(ptr, sym2, &pos3);
	setspecial_local(ptr, sym1, list);
	setspecial_local(ptr, sym2, pos2);
	setspecial_local(ptr, sym3, pos3);
}

int eval_loop_output_(Execute ptr, addr stream)
{
	addr list, pos;

	getvalues_list_control_heap(ptr, &list);
	eval_loop_shift(ptr, list);
	/* format_stream_(ptr, stream, "~&~{~S~%~}~&", list, NULL); */
	Return(fresh_line_stream_(stream, NULL));
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(prin1_print_(ptr, stream, pos));
		Return(terpri_stream_(stream));
	}
	Return(fresh_line_stream_(stream, NULL));
	return force_output_stream_(stream);
}

static int eval_loop_stream_call_(Execute ptr, addr stream, addr pos)
{
	eval_loop_minus(ptr, pos);
	Return(eval_execute_partial_(ptr, pos));
	return eval_loop_output_(ptr, stream);
}

static int eval_loop_stream_(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_control(ptr, &control);
	(void)eval_loop_stream_call_(ptr, stream, pos);
	return pop_control_(ptr, control);
}

static void eval_loop_variable(Execute ptr)
{
	/* -
	 * +, ++, +++
	 * *, **, ***
	 * /, //, ///
	 */
	addr symbol;

	GetConst(COMMON_MINUS, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_PLUS, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_PLUS2, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_PLUS3, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_ASTERISK, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_ASTERISK2, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_ASTERISK3, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_SLASH, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_SLASH2, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_SLASH3, &symbol);
	pushspecial_control(ptr, symbol, Nil);
}

struct eval_loop_struct {
	eval_loop_calltype call;
	addr stream;
	int *ret, eof;
};

static int eval_loop_execute_(Execute ptr, void *voidp)
{
	int check, *ret;
	addr pos, stream;
	eval_loop_calltype call;
	struct eval_loop_struct *str;

	str = (struct eval_loop_struct *)voidp;
	stream = str->stream;
	call = str->call;
	ret = str->ret;

	/* read */
	Return(read_prompt_(ptr, stream, &check, &pos));

	/* EOF */
	if (check) {
		str->eof = 1;
		Return(fresh_line_stream_(stream, NULL));
		return Result(ret, 1);
	}

	/* calltype */
	Return((*call)(ptr, stream, pos, ret, &check));

	/* execute */
	if (check) {
		Return(eval_loop_stream_(ptr, stream, pos));
	}

	/* normal */
	return 0;
}

static int eval_loop_restart_(Execute ptr, addr stream,
		eval_loop_calltype call, int *ret, int *eof)
{
	addr control, restart;
	struct eval_loop_struct str;

	push_control(ptr, &control);
	str.stream = stream;
	str.call = call;
	str.ret = ret;
	str.eof = 0;
	eval_main_restart_abort(&restart);
	(void)restart0_control_(ptr, restart, eval_loop_execute_, (void *)&str);
	*eof = str.eof;
	return pop_control_(ptr, control);
}

int eval_custom_loop_(Execute ptr, addr stream, eval_loop_calltype call, int *ret)
{
	int exit, eof;

	eval_loop_variable(ptr);
	exit = 0;
	for (;;) {
		Return(eval_loop_restart_(ptr, stream, call, &exit, &eof));
		if (exit)
			break;
	}
	if (ret)
		*ret = eof;

	Return(terpri_stream_(stream));
	return finish_output_stream_(stream);
}

static int eval_main_execute(Execute ptr, addr stream, addr pos, int *exit, int *exec)
{
	*exit = 0;
	*exec = 1;
	return 0;
}

int eval_main_loop_(Execute ptr)
{
	addr stream, control;

	Return(terminal_io_stream_(ptr, &stream));
	push_control(ptr, &control);
	push_prompt_eval_loop(ptr);
	(void)eval_custom_loop_(ptr, stream, eval_main_execute, NULL);
	return pop_control_(ptr, control);
}

int eval_main_loop_toplevel_(Execute ptr)
{
#ifdef LISP_PROMPT_DISABLE
	return eval_main_loop_(ptr);
#else
	addr pos, io;
	unicode c;
	size_t size;

	/* loop */
	for (;;) {
		/* eval-loop */
		Return(eval_main_loop_(ptr));

		/* eval-loop-exit */
		GetConst(SYSTEM_EVAL_LOOP_EXIT, &pos);
		getspecial_local(ptr, pos, &pos);
		if (pos == Unbound || pos == Nil)
			break;

		/* prompt */
		strvect_char_heap(&pos, "Exit? ");
		Return(prompt_string_stream_(ptr, pos, 0, &pos));
		if (pos == Nil) {
			Return(terminal_io_stream_(ptr, &io));
			Return(terpri_stream_(io));
			Return(finish_output_stream_(io));
		}
		if (stringp(pos)) {
			string_length(pos, &size);
			if (size == 0)
				continue;
			Return(string_getc_(pos, 0, &c));
			if (toUpperUnicode(c) == 'Y')
				break;
		}
	}

	return 0;
#endif
}


/*
 *  eval_main_string
 */
static int evalcall_string_result_(Execute ptr, addr eval)
{
	addr stream;

	Return(open_input_string_stream_(&stream, eval));
	(void)eval_stream_toplevel_(ptr, stream);
	return close_stream_unwind_protect_(ptr, stream);
}

int eval_main_string_(Execute ptr, addr eval)
{
	addr control, restart;

	push_control(ptr, &control);
	eval_main_restart_abort(&restart);
	(void)restart1_control_(ptr, restart, evalcall_string_result_, eval);
	return pop_control_(ptr, control);
}


/*
 *  eval_main_load
 */
struct eval_main_load_struct {
	addr file;
	int exists;
	int *ret;
};

static int eval_main_load_execute_(Execute ptr, void *voidp)
{
	struct eval_main_load_struct *str;
	str = (struct eval_main_load_struct *)voidp;
	return eval_load_(ptr, str->ret, str->file, Nil, Nil, str->exists, Unbound);
}

int eval_main_load_(Execute ptr, addr file, int exists, int *ret)
{
	int check;
	addr control, restart;
	struct eval_main_load_struct str;

	push_control(ptr, &control);
	str.file = file;
	str.exists = exists;
	str.ret = ret? ret: &check;
	eval_main_restart_abort(&restart);
	(void)restart0_control_(ptr, restart, eval_main_load_execute_, (void *)&str);
	return pop_control_(ptr, control);
}


/************************************************************
 *  eval_object.c
 ************************************************************/

/*
 *  eval-object
 */
void eval_alloc(LocalRoot local, addr *ret, enum EVAL_TYPE type, byte array, byte body)
{
	alloc_smallsize(local, ret, LISPTYPE_EVAL, array, body);
	SetEvalType(*ret, type);
}
void eval_heap(addr *ret, enum EVAL_TYPE type, byte array, byte body)
{
	eval_alloc(NULL, ret, type, array, body);
}
void eval_local(LocalRoot local, addr *ret, enum EVAL_TYPE type, byte array, byte body)
{
	Check(local == NULL, "local error");
	eval_alloc(local, ret, type, array, body);
}

addr refeval(addr pos, size_t index)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	return RefEval_Low(pos, index);
}
void geteval(addr pos, size_t index, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	GetEval_Low(pos, index, ret);
}
void seteval(addr pos, size_t index, addr value)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	SetEval_Low(pos, index, value);
}
enum EVAL_TYPE refevaltype(addr pos)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	return RefEvalType_Low(pos);
}
void getevaltype(addr pos, enum EVAL_TYPE *ret)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	GetEvalType_Low(pos, ret);
}
void setevaltype(addr pos, enum EVAL_TYPE value)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	SetEvalType_Low(pos, value);
}


/*
 *  check
 */
int eval_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL;
}
int eval_declare_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_DECLARE;
}
int eval_declare_nil_p(addr pos)
{
	return pos == Nil || eval_declare_p(pos);
}
int eval_parse_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_PARSE;
}
int eval_scope_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_SCOPE;
}
int eval_stack_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_STACK;
}
int eval_table_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLE;
}
int eval_tablevalue_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLEVALUE;
}
int eval_tablefunction_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLEFUNCTION;
}
int eval_tabletagbody_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLETAGBODY;
}
int eval_tableblock_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLEBLOCK;
}
int eval_tablecall_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLECALL;
}
int eval_code_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_CODE;
}


/************************************************************
 *  eval_stack.c
 ************************************************************/

/*
 *  memory
 */
void eval_stack_alloc(LocalRoot local, addr *ret, enum EVAL_STACK_MODE type)
{
	int i;
	addr pos;
	LocalStack stack;
	struct eval_stack *str;

	/* local-stack */
	if (local)
		push_local(local, &stack);
	else
		stack = NULL;

	/* memory */
	Check(0xFF < sizeof(struct eval_stack), "struct size error");
	eval_alloc(local, &pos, EVAL_TYPE_STACK, EVAL_STACK_SIZE,
			sizeoft(struct eval_stack));
	SetEvalStackType(pos, type);

	/* struct */
	str = StructEvalStack(pos);
	str->stack = stack;
	str->globalp = 0;
	str->lexical = 0;
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++)
		str->optimize[i] = -1;
	*ret = pos;
}
void eval_stack_local(LocalRoot local, addr *ret, enum EVAL_STACK_MODE type)
{
	Check(local == NULL, "local error");
	eval_stack_alloc(local, ret, type);
}
void eval_stack_heap(addr *ret, enum EVAL_STACK_MODE type)
{
	/* for global stack */
	eval_stack_alloc(NULL, ret, type);
}


/*
 *  eval-stack
 */
struct eval_stack *structevalstack(addr pos)
{
	Check(! eval_stack_p(pos), "type error");
	return StructEvalStack_Low(pos);
}
enum EVAL_STACK_MODE refevalstacktype(addr pos)
{
	Check(! eval_stack_p(pos), "type error");
	return RefEvalStackType_Low(pos);
}
void getevalstacktype(addr pos, enum EVAL_STACK_MODE *ret)
{
	Check(! eval_stack_p(pos), "type error");
	GetEvalStackType_Low(pos, ret);
}
void setevalstacktype(addr pos, enum EVAL_STACK_MODE value)
{
	Check(! eval_stack_p(pos), "type error");
	SetEvalStackType_Low(pos, value);
}
void getevalstacknext(addr pos, addr *ret)
{
	Check(! eval_stack_p(pos), "type error");
	GetEvalStackNext_Low(pos, ret);
}
void setevalstacknext(addr pos, addr value)
{
	Check(! eval_stack_p(pos), "type error");
	SetEvalStackNext_Low(pos, value);
}
void getevalstacktable(addr pos, addr *ret)
{
	Check(! eval_stack_p(pos), "type error");
	GetEvalStackTable_Low(pos, ret);
}
void setevalstacktable(addr pos, addr value)
{
	Check(! eval_stack_p(pos), "type error");
	SetEvalStackTable_Low(pos, value);
}
void getevalstackscope(addr pos, addr *ret)
{
	Check(! eval_stack_p(pos), "type error");
	GetEvalStackScope_Low(pos, ret);
}
void setevalstackscope(addr pos, addr value)
{
	Check(! eval_stack_p(pos), "type error");
	SetEvalStackScope_Low(pos, value);
}
void getevalstacklexical(addr pos, addr *ret)
{
	Check(! eval_stack_p(pos), "type error");
	GetEvalStackLexical_Low(pos, ret);
}
void setevalstacklexical(addr pos, addr value)
{
	Check(! eval_stack_p(pos), "type error");
	SetEvalStackLexical_Low(pos, value);
}


/*
 *  eval-stack
 */
int eval_stack_lambda_lexical_p(addr stack)
{
	enum EVAL_STACK_MODE type;
	GetEvalStackType(stack, &type);
	return (type == EVAL_STACK_MODE_LAMBDA) || (type == EVAL_STACK_MODE_LEXICAL);
}

static void getstack_symbol(addr *ret)
{
	GetConst(SYSTEM_EVAL_SCOPE, ret);
}
static void getglobal_symbol(addr *ret)
{
	GetConst(SYSTEM_EVAL_SCOPE_GLOBAL, ret);
}

int getstack_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	getstack_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}
int getglobal_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	getglobal_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

int newstack_eval_(Execute ptr, enum EVAL_STACK_MODE type, addr *ret)
{
	addr stack, symbol, next;

	getstack_symbol(&symbol);
	eval_stack_local(ptr->local, &stack, type);
	Return(getspecialcheck_local_(ptr, symbol, &next));
	SetEvalStackNext(stack, next);
	setspecial_local(ptr, symbol, stack);
	if (ret)
		*ret = stack;

	return 0;
}

static int closestack_unsafe_(Execute ptr)
{
	addr symbol, eval;
	LocalStack stack;

	/* replace eval-stack */
	getstack_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &eval));
	Check(eval == Nil, "scope-stack is nil.");
	stack = StructEvalStack(eval)->stack;
	GetEvalStackNext(eval, &eval);
	setspecial_local(ptr, symbol, eval);

	/* free eval-stack */
	rollback_local(ptr->local, stack);

	return 0;
}

int freestack_eval_(Execute ptr, addr scope)
{
	addr symbol, pos;

	getstack_symbol(&symbol);
#ifdef LISP_DEBUG
	Return(getspecialcheck_local_(ptr, symbol, &pos));
	for (;;) {
		Check(pos == Nil, "stack error [check].");
		Check(pos == Unbound, "unbound error.");
		if (pos == scope)
			break;
		GetEvalStackNext(pos, &pos);
	}
#endif
	for (;;) {
		Return(getspecialcheck_local_(ptr, symbol, &pos));
		Check(pos == Nil, "stack error");
		Return(closestack_unsafe_(ptr));
		if (pos == scope)
			break;
	}

	return 0;
}

static void eval_stack_global_optimize(OptimizeType *optimize, addr list)
{
	int i;
	addr pos, root;
	OptimizeType value;

	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		root = list;
		while (root != Nil) {
			GetCons(root, &pos, &root);
			GetEvalDeclareOptimize(pos, i, &value);
			if (0 <= value) {
				optimize[i] = value;
				break;
			}
		}
	}
}

static void eval_stack_global_heap(Execute ptr, addr *ret)
{
	addr pos, list;
	struct eval_stack *str;

	/* object */
	eval_stack_heap(&pos, EVAL_STACK_MODE_NIL);
	str = StructEvalStack(pos);
	str->globalp = 1;

	/* parse-declare */
	get_nocheck_parse_declare(ptr, &list);
	if (list != Unbound)
		eval_stack_global_optimize(str->optimize, list);

	*ret = pos;
}

int begin_eval_stack_(Execute ptr)
{
	addr symbol, stack;

	/* Global stack must be a heap object. Don't use local function.  */
	eval_stack_global_heap(ptr, &stack);
	getglobal_symbol(&symbol);
	pushspecial_control(ptr, symbol, stack);
	getstack_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);
	/* new stack */
	return newstack_nil_(ptr, NULL);
}

void free_eval_stack(Execute ptr)
{
	addr symbol;

	getglobal_symbol(&symbol);
	setspecial_local(ptr, symbol, Nil);
	getstack_symbol(&symbol);
	setspecial_local(ptr, symbol, Nil);
}

int globalp_stack_eval(addr pos)
{
	Check(! eval_stack_p(pos), "type error");
	return StructEvalStack(pos)->globalp;
}

size_t increment_stack_eval(addr pos)
{
	Check(! eval_stack_p(pos), "type error");
	return (StructEvalStack(pos)->lexical)++;
}

size_t getlexical_stack_eval(addr pos)
{
	Check(! eval_stack_p(pos), "type error");
	return StructEvalStack(pos)->lexical;
}

void getlexical_index_heap(addr stack, addr *ret)
{
	size_t size;

	size = getlexical_stack_eval(stack);
	if (size == 0)
		*ret = Nil;
	else
		index_heap(ret, size);
}


/*
 *  declaim
 */
static void apply_pushnew_stack(LocalRoot local,
		addr stack, addr cons, constindex index)
{
	addr key, table, symbol;

	GetConstant(index, &key);
	GetEvalStackTable(stack, &table);
	while (cons != Nil) {
		GetCons(cons, &symbol, &cons);
		if (pushnewplist_alloc(local, table, key, symbol, &table))
			SetEvalStackTable(stack, table);
	}
}

static void apply_pushnew_callname_stack(LocalRoot local,
		addr stack, addr cons, constindex index)
{
	addr key, table, callname;

	GetConstant(index, &key);
	GetEvalStackTable(stack, &table);
	while (cons != Nil) {
		GetCons(cons, &callname, &cons);
		copylocal_object(local, &callname, callname);
		if (pushnewplist_callname_alloc(local, table, key, callname, &table))
			SetEvalStackTable(stack, table);
	}
}

static void apply_plist_stack(LocalRoot local,
		addr stack, addr cons, constindex index)
{
	addr key, table, symbol, value;

	GetConstant(index, &key);
	GetEvalStackTable(stack, &table);
	while (cons != Nil) {
		GetCons(cons, &symbol, &cons);
		Check(cons == Nil, "plist error");
		GetCons(cons, &value, &cons);
		copylocal_object(local, &value, value);
		if (setpplist_alloc(local, table, key, symbol, value, &table))
			SetEvalStackTable(stack, table);
	}
}

static void apply_plist_callname_stack(LocalRoot local,
		addr stack, addr cons, constindex index)
{
	addr key, table, callname, value;

	GetConstant(index, &key);
	GetEvalStackTable(stack, &table);
	while (cons != Nil) {
		GetCons(cons, &callname, &cons);
		Check(cons == Nil, "plist error");
		GetCons(cons, &value, &cons);
		copylocal_object(local, &callname, callname);
		copylocal_object(local, &value, value);
		if (setpplist_callname_alloc(local, table, key, callname, value, &table))
			SetEvalStackTable(stack, table);
	}
}

static void apply_optimize_stack(addr stack, addr declare)
{
	int i;
	OptimizeType *left, value;
	const OptimizeType *right;

	left = StructEvalStack(stack)->optimize;
	right = getall_optimize_declare(declare);
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		value = right[i];
		if (0 <= value)
			left[i] = value;
	}
}

static void apply_declare_switch(LocalRoot local,
		addr stack, addr declare, int declaimp)
{
	addr pos;

	/* special */
	getall_special_declare(declare, &pos);
	apply_pushnew_stack(local, stack, pos, CONSTANT_SYSTEM_TYPE_SPECIAL);
	/* inline, notinline */
	getall_inline_declare(declare, &pos);
	apply_plist_callname_stack(local, stack, pos, CONSTANT_SYSTEM_INLINE);
	/* type */
	getall_type_value_declare(declare, &pos);
	apply_plist_stack(local, stack, pos, CONSTANT_SYSTEM_TYPE_VALUE);
	/* ftype */
	getall_type_function_declare(declare, &pos);
	apply_plist_callname_stack(local, stack, pos, CONSTANT_SYSTEM_TYPE_FUNCTION);
	/* optimize */
	apply_optimize_stack(stack, declare);

	if (declaimp) {
		/* declaration */
		getall_declaration_declare(declare, &pos);
		apply_pushnew_stack(local, stack, pos, CONSTANT_SYSTEM_DECLARATION);
		/* dynamic-extent, ignore and ignorable are declaration only. */
	}
	else {
		/* dynamic-extent value */
		getall_dynamic_value_declare(declare, &pos);
		apply_pushnew_stack(local, stack, pos, CONSTANT_SYSTEM_DYNAMIC_VALUE);
		/* dynamic-extent function */
		getall_dynamic_function_declare(declare, &pos);
		apply_pushnew_callname_stack(local,
				stack, pos, CONSTANT_SYSTEM_DYNAMIC_FUNCTION);
		/* ignore, ignorable value */
		getall_ignore_value_declare(declare, &pos);
		apply_plist_stack(local, stack, pos, CONSTANT_SYSTEM_IGNORE_VALUE);
		/* ignore, ignorable function */
		getall_ignore_function_declare(declare, &pos);
		apply_plist_callname_stack(local, stack, pos, CONSTANT_SYSTEM_IGNORE_FUNCTION);
		/* declaration is proclamation only. */
	}
}

int apply_declaim_stack_(Execute ptr, addr declare)
{
	addr stack;

	if (declare == Nil)
		return 0;
	Return(getglobal_eval_(ptr, &stack));
	apply_declare_switch(NULL, stack, declare, 1);

	return 0;
}


/*
 *  declare
 */
void apply_declare_stack(LocalRoot local, addr stack, addr declare)
{
	if (declare == Nil)
		return;
	apply_declare_switch(local, stack, declare, 0);
}

/* for let* arguments */
static int apply_declare_global_p_(Execute ptr, addr symbol, addr list, int *ret)
{
	if (find_list_eq_unsafe(symbol, list))
		return Result(ret, 1); /* special */

	/* global */
	Return(getglobal_eval_(ptr, &list));
	GetEvalStackTable(list, &list);
	if (! GetPlistConst(list, SYSTEM_TYPE_SPECIAL, &list)) {
		if (find_list_eq_unsafe(symbol, list))
			return Result(ret, 1); /* special */
	}

	/* symbol */
	if (specialp_symbol(symbol))
		return Result(ret, 1); /* special */

	/* lexical */
	return Result(ret, 0); /* lexical */
}

static int apply_declare_symbol_stack_(Execute ptr, addr stack, addr symbol, addr list)
{
	int check;
	constindex index;
	addr key, table;

	Return(apply_declare_global_p_(ptr, symbol, list, &check));
	index = check?
		CONSTANT_SYSTEM_TYPE_SPECIAL:
		CONSTANT_SYSTEM_TYPE_LEXICAL;
	GetConstant(index, &key);
	GetEvalStackTable(stack, &table);
	if (pushnewplist_alloc(ptr->local, table, key, symbol, &table))
		SetEvalStackTable(stack, table);

	return 0;
}

static void apply_declare_dynamic_stack(LocalRoot local,
		addr stack, addr symbol, addr list)
{
	addr key, table;

	if (find_list_eq_unsafe(symbol, list)) {
		GetConst(SYSTEM_DYNAMIC_VALUE , &key);
		GetEvalStackTable(stack, &table);
		if (pushnewplist_alloc(local, table, key, symbol, &table))
			SetEvalStackTable(stack, table);
	}
}

static void apply_plistsymbol_stack(LocalRoot local,
		addr stack, addr symbol, addr list, constindex index)
{
	addr key, table;

	if (getplist(list, symbol, &list) == 0) {
		copylocal_object(local, &list, list);
		GetConstant(index, &key);
		GetEvalStackTable(stack, &table);
		if (setpplist_alloc(local, table, key, symbol, list, &table))
			SetEvalStackTable(stack, table);
	}
}

int apply_declare_value_stack_(Execute ptr, addr stack, addr symbol, addr declare)
{
	addr list;
	LocalRoot local;

	local = ptr->local;
	/* special */
	getall_special_declare(declare, &list);
	Return(apply_declare_symbol_stack_(ptr, stack, symbol, list));
	/* inline, notinline */
	if (declare == Nil)
		return 0;
	/* type */
	getall_type_value_declare(declare, &list);
	apply_plistsymbol_stack(local, stack, symbol, list, CONSTANT_SYSTEM_TYPE_VALUE);
	/* ftype */
	/* optimize */
	/* dynamic-extent value */
	getall_dynamic_value_declare(declare, &list);
	apply_declare_dynamic_stack(local, stack, symbol, list);
	/* dynamic-extent function */
	/* ignore, ignorable value */
	getall_ignore_value_declare(declare, &list);
	apply_plistsymbol_stack(local, stack, symbol, list, CONSTANT_SYSTEM_IGNORE_VALUE);
	/* ignore, ignorable function */
	/* declaration is proclamation only. */

	return 0;
}

int apply_declare_let_stack_(Execute ptr, addr stack, addr symbol, addr declare)
{
	getall_special_declare(declare, &declare);
	return apply_declare_symbol_stack_(ptr, stack, symbol, declare);
}

/* for labels arguments */
static void apply_pushcall_stack(LocalRoot local,
		addr stack, addr call, addr cons, constindex index)
{
	addr key, table;

	if (find_list_callname_unsafe(call, cons)) {
		copylocal_object(local, &call, call);
		copylocal_object(local, &cons, cons);
		GetConstant(index, &key);
		GetEvalStackTable(stack, &table);
		if (pushnewplist_callname_alloc(local, table, key, call, &table))
			SetEvalStackTable(stack, table);
	}
}

static void apply_plistcall_stack(LocalRoot local,
		addr stack, addr call, addr cons, constindex index)
{
	addr key, table;

	if (getplist_callname(cons, call, &cons) == 0) {
		copylocal_object(local, &call, call);
		copylocal_object(local, &cons, cons);
		GetConstant(index, &key);
		GetEvalStackTable(stack, &table);
		if (setpplist_callname_alloc(local, table, key, call, cons, &table))
			SetEvalStackTable(stack, table);
	}
}

void apply_declare_function_stack(LocalRoot local, addr stack, addr call, addr declare)
{
	addr list;

	if (declare == Nil)
		return;
	/* inline, notinline */
	getall_inline_declare(declare, &list);
	apply_plistcall_stack(local, stack, call, list, CONSTANT_SYSTEM_INLINE);
	/* special */
	/* type */
	/* ftype */
	getall_type_function_declare(declare, &list);
	apply_plistcall_stack(local, stack, call, list, CONSTANT_SYSTEM_TYPE_FUNCTION);
	/* optimize */
	/* dynamic-extent value */
	/* dynamic-extent function */
	getall_dynamic_function_declare(declare, &list);
	apply_pushcall_stack(local, stack, call, list, CONSTANT_SYSTEM_DYNAMIC_FUNCTION);
	/* ignore, ignorable value */
	/* ignore, ignorable function */
	getall_ignore_function_declare(declare, &list);
	apply_plistcall_stack(local, stack, call, list, CONSTANT_SYSTEM_IGNORE_FUNCTION);
	/* declaration is proclamation only. */
}


/*
 *  table scope
 */
int getvalue_scope_evalstack(addr stack, addr pos, addr *ret)
{
	Check(! eval_stack_p(stack), "type error");
	Check(! symbolp(pos), "name error");
	GetEvalStackScope(stack, &stack);
	return getvalue_evaltable(stack, pos, ret);
}

void setvalue_scope_evalstack(addr stack, addr pos)
{
	addr list;
#ifdef LISP_DEBUG
	addr name;
#endif

	Check(! eval_stack_p(stack), "type error");
	CheckTableValue(pos);
#ifdef LISP_DEBUG
	getname_tablevalue(pos, &name);
	Check(getvalue_scope_evalstack(stack, name, NULL), "duplicate error");
#endif
	evaltable_value_heap(&pos, pos);
	GetEvalStackScope(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackScope(stack, list);
}

int getfunction_scope_evalstack(addr stack, addr pos, addr *ret)
{
	Check(! eval_stack_p(stack), "type error");
	Check(! callnamep(pos), "name error");
	GetEvalStackScope(stack, &stack);
	return getfunction_evaltable(stack, pos, ret);
}

void setfunction_scope_evalstack(addr stack, addr pos)
{
	addr list;
#ifdef LISP_DEBUG
	addr name;
#endif

	Check(! eval_stack_p(stack), "type error");
	CheckTableFunction(pos);
#ifdef LISP_DEBUG
	getname_tablefunction(pos, &name);
	Check(getfunction_scope_evalstack(stack, name, NULL), "duplicate error");
#endif
	evaltable_function_heap(&pos, pos);
	GetEvalStackScope(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackScope(stack, list);
}

int gettagbody_scope_evalstack(addr stack, addr pos, addr *ret)
{
	Check(! eval_stack_p(stack), "type error");
	Check(! tagbody_tag_p(pos), "name error");
	GetEvalStackScope(stack, &stack);
	return gettagbody_evaltable(stack, pos, ret);
}

void settagbody_scope_evalstack(addr stack, addr pos)
{
	addr list;
#ifdef LISP_DEBUG
	addr name;
#endif

	Check(! eval_stack_p(stack), "type error");
	CheckTableTagBody(pos);
#ifdef LISP_DEBUG
	getname_tabletagbody(pos, &name);
	Check(gettagbody_scope_evalstack(stack, name, NULL), "duplicate error");
#endif
	evaltable_tagbody_heap(&pos, pos);
	GetEvalStackScope(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackScope(stack, list);
}

int getblock_scope_evalstack(addr stack, addr pos, addr *ret)
{
	Check(! eval_stack_p(stack), "type error");
	Check(! symbolp(pos), "name error");
	GetEvalStackScope(stack, &stack);
	return getblock_evaltable(stack, pos, ret);
}

void setblock_scope_evalstack(addr stack, addr pos)
{
	addr list;
#ifdef LISP_DEBUG
	addr name;
#endif

	Check(! eval_stack_p(stack), "type error");
	CheckTableBlock(pos);
#ifdef LISP_DEBUG
	getname_tableblock(pos, &name);
	Check(getblock_scope_evalstack(stack, name, NULL), "duplicate error");
#endif
	evaltable_block_heap(&pos, pos);
	GetEvalStackScope(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackScope(stack, list);
}


/*
 *  table lexical
 */
void setvalue_lexical_evalstack(addr stack, addr pos)
{
	addr list;

	Check(! eval_stack_p(stack), "type error");
	CheckTableValue(pos);
	evaltable_value_heap(&pos, pos);
	GetEvalStackLexical(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackLexical(stack, list);
}

void setfunction_lexical_evalstack(addr stack, addr pos)
{
	addr list;

	Check(! eval_stack_p(stack), "type error");
	CheckTableFunction(pos);
	evaltable_function_heap(&pos, pos);
	GetEvalStackLexical(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackLexical(stack, list);
}

void settagbody_lexical_evalstack(addr stack, addr pos)
{
	addr list;

	Check(! eval_stack_p(stack), "type error");
	CheckTableTagBody(pos);
	evaltable_tagbody_heap(&pos, pos);
	GetEvalStackLexical(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackLexical(stack, list);
}

void setblock_lexical_evalstack(addr stack, addr pos)
{
	addr list;

	Check(! eval_stack_p(stack), "type error");
	CheckTableBlock(pos);
	evaltable_block_heap(&pos, pos);
	GetEvalStackLexical(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackLexical(stack, list);
}

int find_plistlist_evalstack(addr stack, addr key, addr symbol)
{
	Check(! eval_stack_p(stack), "type error");

	GetEvalStackTable(stack, &stack);
	return getplist(stack, key, &stack) == 0
		&& find_list_eq_unsafe(symbol, stack);
}

int find_special_evalstack(addr stack, addr symbol)
{
	addr key;

	Check(! eval_stack_p(stack), "type error");
	Check(! symbolp(symbol), "type error");
	GetConst(SYSTEM_TYPE_SPECIAL, &key);
	return find_plistlist_evalstack(stack, key, symbol);
}

int find_global_special_evalstack(addr stack, addr symbol, addr *ret)
{
	addr list, key, x, y;

	Check(! eval_stack_p(stack), "type error");
	Check(! symbolp(symbol), "type error");

	GetConst(COMMON_SPECIAL, &key);
	GetEvalStackTable(stack, &list);
	if (getplist(list, key, ret))
		goto not_found;

	while (list != Nil) {
		GetCons(list, &x, &list);
		getname_tablevalue(x, &y);
		if (y == symbol) {
			*ret = x;
			return 1;
		}
	}

not_found:
	*ret = Nil;
	return 0;
}

void push_global_special_evalstack(addr stack, addr value)
{
	addr list, key;

	Check(! eval_stack_p(stack), "type error");
	CheckTableValue(value);

	GetConst(COMMON_SPECIAL, &key);
	GetEvalStackTable(stack, &list);
	if (setplist_heap(list, key, value, &list))
		SetEvalStackTable(stack, list);
}


/************************************************************
 *  eval_table.c
 ************************************************************/

/*
 *  tablevalue
 */
static void alloc_tablevalue(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_TABLEVALUE,
			TABLEVALUE_INDEX_SIZE,
			sizeoft(struct tablevalue));
}

void make_tablevalue(addr *ret, addr symbol)
{
	addr pos;
	struct tablevalue *ptr;

	alloc_tablevalue(NULL, &pos);
	ptr = StructTableValue(pos);
	clearpoint(ptr);
	SetEval(pos, TABLEVALUE_INDEX_NAME, symbol);
	*ret = pos;
}

static void copy_eval_table(addr src, size_t index, addr dst)
{
	addr value;

	GetEval(src, index, &value);
	copyhard_object(NULL, &value, value);
	SetEval(dst, index, value);
}

void copy_tablevalue(addr *ret, addr pos)
{
	addr one;
	size_t i, size;

	CheckTableValue(pos);
	alloc_tablevalue(NULL, &one);

	LenBodyEval(one, &size);
	memcpy(PtrBody_value(one), PtrBody_value(pos), size);

	for (i = 0; i < TABLEVALUE_INDEX_SIZE; i++)
		copy_eval_table(pos, i, one);
	*ret = one;
}

void getname_tablevalue(addr pos, addr *ret)
{
	CheckTableValue(pos);
	GetEval(pos, TABLEVALUE_INDEX_NAME, ret);
}
void setname_tablevalue(addr pos, addr value)
{
	CheckTableValue(pos);
	SetEval(pos, TABLEVALUE_INDEX_NAME, value);
}

void gettype_tablevalue(addr pos, addr *ret)
{
	CheckTableValue(pos);
	GetEval(pos, TABLEVALUE_INDEX_TYPE, ret);
}
void settype_tablevalue(addr pos, addr value)
{
	CheckTableValue(pos);
	SetEval(pos, TABLEVALUE_INDEX_TYPE, value);
}

int getspecialp_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->specialp;
}
void setspecialp_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->specialp = (value != 0);
}

int getdynamic_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->dynamic;
}
void setdynamic_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->dynamic = (value != 0);
}

enum IgnoreType getignore_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->ignore;
}
void setignore_tablevalue(addr pos, enum IgnoreType value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->ignore = value;
}

int getreference_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->reference;
}
void setreference_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->reference = (value != 0);
}

int getcheck_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->check;
}
void setcheck_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->check = (value != 0);
}

size_t getlexical_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->lexical;
}
void setlexical_tablevalue(addr pos, size_t value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->lexical = value;
}

size_t getlet_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->let;
}
void setlet_tablevalue(addr pos, size_t value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->let = value;
}

int getclosurep_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->closurep;
}
void setclosurep_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->closurep = (value != 0);
}

int getbasep_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->basep;
}
void setbasep_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->basep = (value != 0);
}

int getglobalp_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->globalp;
}
void setglobalp_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->globalp = (value != 0);
}

size_t getclosure_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->closure;
}
void setclosure_tablevalue(addr pos, size_t value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->closure = value;
}

void getvalue_tablevalue(Execute ptr, addr pos, addr *ret)
{
	size_t index = getlexical_tablevalue(pos);
	get_lexical_control(ptr, index, ret);
}

void setvalue_tablevalue(Execute ptr, addr pos, addr value)
{
	size_t index = getlexical_tablevalue(pos);
	set_lexical_control(ptr, index, value);
}


/*
 *  tablefunction
 */
static void heap_tablefunction(addr *ret)
{
	eval_heap(ret, EVAL_TYPE_TABLEFUNCTION,
			TABLEFUNCTION_INDEX_SIZE,
			sizeoft(struct tablefunction));
}

void make_tablefunction(addr *ret, addr call)
{
	addr pos;
	struct tablefunction *ptr;

	heap_tablefunction(&pos);
	ptr = StructTableFunction(pos);
	clearpoint(ptr);
	SetEval(pos, TABLEFUNCTION_INDEX_NAME, call);
	*ret = pos;
}

static void copy_tablefunction(addr *ret, addr pos)
{
	addr one, value;
	size_t size;

	CheckTableFunction(pos);
	heap_tablefunction(&one);

	LenBodyEval(one, &size);
	memcpy(PtrBody_function(one), PtrBody_function(pos), size);

	copy_eval_table(pos, TABLEFUNCTION_INDEX_NAME, one);
	copy_eval_table(pos, TABLEFUNCTION_INDEX_TYPE, one);
	GetEval(pos, TABLEFUNCTION_INDEX_REDIRECT, &value);
	SetEval(one, TABLEFUNCTION_INDEX_REDIRECT, value);
	*ret = one;
}

void make_redirect_tablefunction(addr *ret, addr value)
{
	addr pos;

	copy_tablefunction(&pos, value);
	SetEval(pos, TABLEFUNCTION_INDEX_REDIRECT, value);
	*ret = pos;
}

static int redirect_tablefunction(addr pos, addr *ret)
{
	CheckTableFunction(pos);
	GetEval(pos, TABLEFUNCTION_INDEX_REDIRECT, &pos);
	if (pos == Nil)
		return 0;
	*ret = pos;
	return 1;
}

void getname_tablefunction(addr pos, addr *ret)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos)) {
		getname_tablefunction(pos, ret);
		return;
	}
	GetEval(pos, TABLEFUNCTION_INDEX_NAME, ret);
}
void setname_tablefunction(addr pos, addr value)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos)) {
		setname_tablefunction(pos, value);
		return;
	}
	SetEval(pos, TABLEFUNCTION_INDEX_NAME, value);
}

void gettype_tablefunction(addr pos, addr *ret)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos)) {
		gettype_tablefunction(pos, ret);
		return;
	}
	GetEval(pos, TABLEFUNCTION_INDEX_TYPE, ret);
}
void settype_tablefunction(addr pos, addr value)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos)) {
		settype_tablefunction(pos, value);
		return;
	}
	SetEval(pos, TABLEFUNCTION_INDEX_TYPE, value);
}

int getglobalp_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos))
		return getglobalp_tablefunction(pos);

	return StructTableFunction(pos)->globalp;
}
void setglobalp_tablefunction(addr pos, int value)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos)) {
		setglobalp_tablefunction(pos, value);
		return;
	}
	StructTableFunction(pos)->globalp = (value != 0);
}

int getdynamic_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos))
		return getdynamic_tablefunction(pos);

	return StructTableFunction(pos)->dynamic;
}
void setdynamic_tablefunction(addr pos, int value)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos)) {
		setdynamic_tablefunction(pos, value);
		return;
	}
	StructTableFunction(pos)->dynamic = (value != 0);
}

int getreference_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos))
		return getreference_tablefunction(pos);

	return StructTableFunction(pos)->reference;
}
void setreference_tablefunction(addr pos, int value)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos)) {
		setreference_tablefunction(pos, value);
		return;
	}
	StructTableFunction(pos)->reference = (value != 0);
}

int getcheck_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos))
		return getcheck_tablefunction(pos);

	return StructTableFunction(pos)->check;
}
void setcheck_tablefunction(addr pos, int value)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos)) {
		setcheck_tablefunction(pos, value);
		return;
	}
	StructTableFunction(pos)->check = (value != 0);
}

enum IgnoreType getignore_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos))
		return getignore_tablefunction(pos);

	return StructTableFunction(pos)->ignore;
}
void setignore_tablefunction(addr pos, enum IgnoreType value)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos)) {
		setignore_tablefunction(pos, value);
		return;
	}
	StructTableFunction(pos)->ignore = value;
}

enum InlineType getinline_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos))
		return getinline_tablefunction(pos);

	return StructTableFunction(pos)->Inline;
}
void setinline_tablefunction(addr pos, enum InlineType value)
{
	CheckTableFunction(pos);
	if (redirect_tablefunction(pos, &pos)) {
		setinline_tablefunction(pos, value);
		return;
	}
	StructTableFunction(pos)->Inline = value;
}

size_t getlexical_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->lexical;
}

void setlexical_tablefunction(addr pos, size_t value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->lexical = value;
}

int getclosurep_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->closurep;
}

void setclosurep_tablefunction(addr pos, int value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->closurep = (value != 0);
}

size_t getclosure_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->closure;
}

void setclosure_tablefunction(addr pos, size_t value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->closure = value;
}


/*
 *  tabletagbody
 */
static void alloc_tabletagbody(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_TABLETAGBODY,
			TABLETAGBODY_INDEX_SIZE,
			sizeoft(struct tabletagbody));
}

void make_tabletagbody(addr *ret, addr tag)
{
	addr pos;
	struct tabletagbody *ptr;

	Check(! tagbody_tag_p(tag), "tag error");
	alloc_tabletagbody(NULL, &pos);
	ptr = StructTableTagBody(pos);
	clearpoint(ptr);
	SetEval(pos, TABLETAGBODY_INDEX_NAME, tag);
	*ret = pos;
}

void copy_tabletagbody(addr *ret, addr pos)
{
	addr one;
	size_t i, size;

	CheckTableTagBody(pos);
	alloc_tabletagbody(NULL, &one);

	LenBodyEval(one, &size);
	memcpy(PtrBody_tagbody(one), PtrBody_tagbody(pos), size);

	for (i = 0; i < TABLETAGBODY_INDEX_SIZE; i++)
		copy_eval_table(pos, i, one);
	*ret = one;
}

void getname_tabletagbody(addr pos, addr *ret)
{
	CheckTableTagBody(pos);
	GetEval(pos, TABLETAGBODY_INDEX_NAME, ret);
}
void setname_tabletagbody(addr pos, addr value)
{
	CheckTableTagBody(pos);
	Check(! tagbody_tag_p(value), "tag error");
	SetEval(pos, TABLETAGBODY_INDEX_NAME, value);
}

int getreference_tabletagbody(addr pos)
{
	CheckTableTagBody(pos);
	return StructTableTagBody(pos)->reference;
}
void setreference_tabletagbody(addr pos, int value)
{
	CheckTableTagBody(pos);
	StructTableTagBody(pos)->reference = (value != 0);
}

int equal_tabletagbody(addr left, addr right)
{
	CheckTableTagBody(left);
	CheckTableTagBody(right);
	GetEval(left, TABLETAGBODY_INDEX_NAME, &left);
	GetEval(right, TABLETAGBODY_INDEX_NAME, &right);
	return eql_function(left, right);
}

size_t getlexical_tabletagbody(addr pos)
{
	CheckTableTagBody(pos);
	return StructTableTagBody(pos)->lexical;
}

void setlexical_tabletagbody(addr pos, size_t value)
{
	CheckTableTagBody(pos);
	StructTableTagBody(pos)->lexical = value;
}

int getclosurep_tabletagbody(addr pos)
{
	CheckTableTagBody(pos);
	return StructTableTagBody(pos)->closurep;
}

void setclosurep_tabletagbody(addr pos, int value)
{
	CheckTableTagBody(pos);
	StructTableTagBody(pos)->closurep = (value != 0);
}

size_t getclosure_tabletagbody(addr pos)
{
	CheckTableTagBody(pos);
	return StructTableTagBody(pos)->closure;
}

void setclosure_tabletagbody(addr pos, size_t value)
{
	CheckTableTagBody(pos);
	StructTableTagBody(pos)->closure = value;
}

size_t getjump_tabletagbody(addr pos)
{
	CheckTableTagBody(pos);
	return StructTableTagBody(pos)->jump;
}

void setjump_tabletagbody(addr pos, size_t value)
{
	CheckTableTagBody(pos);
	StructTableTagBody(pos)->jump = value;
}


/*
 *  tableblock
 */
static void alloc_tableblock(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_TABLEBLOCK,
			TABLEBLOCK_INDEX_SIZE,
			sizeoft(struct tableblock));
}

void make_tableblock(addr *ret, addr tag)
{
	addr pos;
	struct tableblock *ptr;

	Check(! symbolp(tag), "tag error");
	alloc_tableblock(NULL, &pos);
	ptr = StructTableBlock(pos);
	clearpoint(ptr);
	SetEval(pos, TABLEBLOCK_INDEX_NAME, tag);
	*ret = pos;
}

void copy_tableblock(addr *ret, addr pos)
{
	addr one;
	size_t i, size;

	CheckTableBlock(pos);
	alloc_tableblock(NULL, &one);

	LenBodyEval(one, &size);
	memcpy(PtrBody_block(one), PtrBody_block(pos), size);

	for (i = 0; i < TABLEBLOCK_INDEX_SIZE; i++)
		copy_eval_table(pos, i, one);
	*ret = one;
}

void getname_tableblock(addr pos, addr *ret)
{
	CheckTableBlock(pos);
	GetEval(pos, TABLEBLOCK_INDEX_NAME, ret);
}
void setname_tableblock(addr pos, addr value)
{
	CheckTableBlock(pos);
	Check(! symbolp(value), "tag error");
	SetEval(pos, TABLEBLOCK_INDEX_NAME, value);
}

int getreference_tableblock(addr pos)
{
	CheckTableBlock(pos);
	return StructTableBlock(pos)->reference;
}
void setreference_tableblock(addr pos, int value)
{
	CheckTableBlock(pos);
	StructTableBlock(pos)->reference = (value != 0);
}

int equal_tableblock(addr left, addr right)
{
	CheckTableBlock(left);
	CheckTableBlock(right);
	GetEval(left, TABLEBLOCK_INDEX_NAME, &left);
	GetEval(right, TABLEBLOCK_INDEX_NAME, &right);
	return left == right;
}

size_t getlexical_tableblock(addr pos)
{
	CheckTableBlock(pos);
	return StructTableBlock(pos)->lexical;
}

void setlexical_tableblock(addr pos, size_t value)
{
	CheckTableBlock(pos);
	StructTableBlock(pos)->lexical = value;
}

int getclosurep_tableblock(addr pos)
{
	CheckTableBlock(pos);
	return StructTableBlock(pos)->closurep;
}

void setclosurep_tableblock(addr pos, int value)
{
	CheckTableBlock(pos);
	StructTableBlock(pos)->closurep = (value != 0);
}

size_t getclosure_tableblock(addr pos)
{
	CheckTableBlock(pos);
	return StructTableBlock(pos)->closure;
}

void setclosure_tableblock(addr pos, size_t value)
{
	CheckTableBlock(pos);
	StructTableBlock(pos)->closure = value;
}


/*
 *  evaltable
 */
static void evaltable_heap(addr *ret, enum EvalTable type, addr value)
{
	addr pos;
	struct eval_table *str;

	eval_heap(&pos, EVAL_TYPE_TABLE, 1, sizeoft(struct eval_table));
	SetEval(pos, 0, value);
	str = StructEvalTable(pos);
	str->type = type;
	*ret = pos;
}
void evaltable_value_heap(addr *ret, addr pos)
{
	CheckTableValue(pos);
	evaltable_heap(ret, EvalTable_Value, pos);
}
void evaltable_function_heap(addr *ret, addr pos)
{
	CheckTableFunction(pos);
	evaltable_heap(ret, EvalTable_Function, pos);
}
void evaltable_tagbody_heap(addr *ret, addr pos)
{
	CheckTableTagBody(pos);
	evaltable_heap(ret, EvalTable_TagBody, pos);
}
void evaltable_block_heap(addr *ret, addr pos)
{
	CheckTableBlock(pos);
	evaltable_heap(ret, EvalTable_Block, pos);
}

enum EvalTable gettype_evaltable(addr pos)
{
	CheckTableTable(pos);
	return StructEvalTable(pos)->type;
}

void get_evaltable(addr pos, addr *ret)
{
	CheckTableTable(pos);
	GetEval(pos, 0, ret);
}

int getvalue_evaltable(addr list, addr pos, addr *ret)
{
	addr var, check;

	Check(! symbolp(pos), "name error");
	while (list != Nil) {
		GetCons(list, &var, &list);
		if (gettype_evaltable(var) == EvalTable_Value) {
			get_evaltable(var, &var);
			getname_tablevalue(var, &check);
			if (check == pos) {
				if (ret)
					*ret = var;
				return 1;
			}
		}
	}

	return 0;
}

int getfunction_evaltable(addr list, addr pos, addr *ret)
{
	addr var, check;

	Check(! callnamep(pos), "name error");
	while (list != Nil) {
		GetCons(list, &var, &list);
		if (gettype_evaltable(var) == EvalTable_Function) {
			get_evaltable(var, &var);
			getname_tablefunction(var, &check);
			if (equal_callname(check, pos)) {
				if (ret)
					*ret = var;
				return 1;
			}
		}
	}

	return 0;
}

int gettagbody_evaltable(addr list, addr pos, addr *ret)
{
	addr var, check;

	Check(! tagbody_tag_p(pos), "name error");
	while (list != Nil) {
		GetCons(list, &var, &list);
		if (gettype_evaltable(var) == EvalTable_TagBody) {
			get_evaltable(var, &var);
			getname_tabletagbody(var, &check);
			if (eql_function(check, pos)) {
				if (ret)
					*ret = var;
				return 1;
			}
		}
	}

	return 0;
}

int getblock_evaltable(addr list, addr pos, addr *ret)
{
	addr var, check;

	Check(! symbolp(pos), "name error");
	while (list != Nil) {
		GetCons(list, &var, &list);
		if (gettype_evaltable(var) == EvalTable_Block) {
			get_evaltable(var, &var);
			getname_tableblock(var, &check);
			if (check == pos) {
				if (ret)
					*ret = var;
				return 1;
			}
		}
	}

	return 0;
}

int getclosurep_evaltable(addr table)
{
	addr pos;

	CheckTableTable(table);
	get_evaltable(table, &pos);
	switch (gettype_evaltable(table)) {
		case EvalTable_Value:
			return getclosurep_tablevalue(pos);

		case EvalTable_Function:
			return getclosurep_tablefunction(pos);

		case EvalTable_TagBody:
			return getclosurep_tabletagbody(pos);

		case EvalTable_Block:
			return getclosurep_tableblock(pos);

		default:
			Abort("getclosurep_evaltable error");
			return 0;
	}
}


/*
 *  tablecall
 */
static void alloc_tablecall(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_TABLECALL,
			TABLECALL_INDEX_SIZE,
			sizeoft(struct tablecall));
}

void make_tablecall(addr *ret)
{
	addr pos;
	struct tablecall *ptr;

	alloc_tablecall(NULL, &pos);
	ptr = StructTableCall(pos);
	clearpoint(ptr);
	*ret = pos;
}

int getcheck_tablecall(addr pos)
{
	CheckTableCall(pos);
	return StructTableCall(pos)->check;
}
void setcheck_tablecall(addr pos, int value)
{
	CheckTableCall(pos);
	StructTableCall(pos)->check = (value != 0);
}

void getvalue_tablecall(addr pos, addr *ret)
{
	CheckTableCall(pos);
	GetEval(pos, TABLECALL_INDEX_VALUE, ret);
}
void setvalue_tablecall(addr pos, addr value)
{
	CheckTableCall(pos);
	SetEval(pos, TABLECALL_INDEX_VALUE, value);
}

void gettype_tablecall(addr pos, addr *ret)
{
	CheckTableCall(pos);
	GetEval(pos, TABLECALL_INDEX_TYPE, ret);
}
void settype_tablecall(addr pos, addr value)
{
	CheckTableCall(pos);
	SetEval(pos, TABLECALL_INDEX_TYPE, value);
}


/************************************************************
 *  eval_value.c
 ************************************************************/

/*
 *  toplevel
 */
static void symbol_toplevel_eval(addr *ret)
{
	GetConst(SYSTEM_EVAL_TOPLEVEL, ret);
}

int get_toplevel_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_toplevel_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void set_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_toplevel_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

void push_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_toplevel_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

int toplevelp_eval(Execute ptr)
{
	addr pos;
	symbol_toplevel_eval(&pos);
	getspecial_local(ptr, pos, &pos);

	return pos != Unbound && pos != Nil;
}


/*
 *  compile-time
 */
static void symbol_compile_time_eval(Execute ptr, addr *ret)
{
	GetConst(SYSTEM_EVAL_COMPILE_TIME, ret);
}

int get_compile_time_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_compile_time_eval(ptr, &symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void set_compile_time_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_time_eval(ptr, &symbol);
	setspecial_local(ptr, symbol, value);
}

void push_compile_time_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_time_eval(ptr, &symbol);
	pushspecial_control(ptr, symbol, value);
}

int compile_time_too_eval_(Execute ptr, int *ret)
{
	addr value;
	Return(get_compile_time_eval_(ptr, &value));
	return Result(ret, value != Nil);
}


/*
 *  eval-when :compile-toplevel
 */
static void symbol_compile_toplevel_eval(addr *ret)
{
	GetConst(SYSTEM_EVAL_COMPILE_TOPLEVEL, ret);
}

int get_compile_toplevel_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_compile_toplevel_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void set_compile_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_toplevel_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

void push_compile_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_toplevel_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

int compile_toplevel_p_eval_(Execute ptr, int *ret)
{
	addr value;
	Return(get_compile_toplevel_eval_(ptr, &value));
	return Result(ret, value != Nil);
}


/*
 *  eval-when :load-toplevel
 */
static void symbol_load_toplevel_eval(addr *ret)
{
	GetConst(SYSTEM_EVAL_LOAD_TOPLEVEL, ret);
}

int get_load_toplevel_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_load_toplevel_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void set_load_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_load_toplevel_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

void push_load_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_load_toplevel_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

int load_toplevel_p_eval_(Execute ptr, int *ret)
{
	addr value;
	Return(get_load_toplevel_eval_(ptr, &value));
	return Result(ret, value != Nil);
}


/*
 *  eval-when :execute
 */
static void symbol_execute_eval(addr *ret)
{
	GetConst(SYSTEM_EVAL_EXECUTE, ret);
}

int get_execute_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_execute_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void set_execute_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_execute_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

void push_execute_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_execute_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

int executep_eval_(Execute ptr, int *ret)
{
	addr value;
	Return(get_execute_eval_(ptr, &value));
	return Result(ret, value != Nil);
}


/*
 *  parse-declare
 */
static void symbol_parse_declare(addr *ret)
{
	GetConst(SYSTEM_PARSE_DECLARE, ret);
}

void push_parse_declare(Execute ptr, addr value)
{
	addr symbol;
	symbol_parse_declare(&symbol);
	pushspecial_control(ptr, symbol, value);
}

int get_parse_declare_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_parse_declare(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void get_nocheck_parse_declare(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_parse_declare(&symbol);
	getspecial_local(ptr, symbol, ret);
}

void set_parse_declare(Execute ptr, addr value)
{
	addr symbol;
	symbol_parse_declare(&symbol);
	setspecial_local(ptr, symbol, value);
}

int add_parse_declare_(Execute ptr, addr value, addr *ret)
{
	addr symbol, list;

	symbol_parse_declare(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	cons_heap(&list, value, list);
	setspecial_local(ptr, symbol, list);
	if (ret)
		*ret = list;

	return 0;
}


/*
 *  compiler-macro
 */
static void enable_compiler_macro_symbol(addr *ret)
{
	GetConst(SYSTEM_ENABLE_COMPILER_MACRO, ret);
}

void push_enable_compiler_macro(Execute ptr, addr value)
{
	addr symbol;
	enable_compiler_macro_symbol(&symbol);
	pushspecial_control(ptr, symbol, value);
}

int enable_compiler_macro_p(Execute ptr)
{
	addr pos;

	enable_compiler_macro_symbol(&pos);
	getspecial_local(ptr, pos, &pos);

	return pos != Unbound && pos != Nil;
}


/************************************************************
 *  execute.c
 ************************************************************/

/* constant */
#ifdef LISP_DEBUG
#define EXECUTE_SIZE      2
#define EXECUTE_PLUS      3
#else
#define EXECUTE_SIZE      0x0100
#define EXECUTE_PLUS      0x0100
#endif

#define SizeLimit (1024UL * 1024UL)

/*
 *  execute
 */
static struct execute **Execute_Array = NULL;
static mutexlite Execute_Mutex;
static condlite Execute_Cond;
static size_t Execute_Size;
static size_t Execute_Position;
static size_t Execute_LocalSize;

#ifdef LISP_DEGRADE
struct execute ***Degrade_execute_Execute(void) { return &Execute_Array; }
size_t *Degrade_execute_Size(void) { return &Execute_Size; }
size_t *Degrade_execute_Position(void) { return &Execute_Position; }
#endif


/*
 *  Thread
 */
static void init_execute_local(void)
{
	lispd_make_threadlocal(&ThreadLocal_Execute);
	lispd_make_threadlocal(&ThreadLocal_Index);
	lispd_make_threadlocal(&ThreadLocal_Local);
}
static void free_execute_local(void)
{
	lispd_destroy_threadlocal(ThreadLocal_Execute);
	lispd_destroy_threadlocal(ThreadLocal_Index);
	lispd_destroy_threadlocal(ThreadLocal_Local);
}
void set_execute_local(struct execute *ptr)
{
	lispd_set_threadlocal(ThreadLocal_Execute, (void *)ptr);
	lispd_set_threadlocal(ThreadLocal_Index, (void *)&ptr->index);
	lispd_set_threadlocal(ThreadLocal_Local, (void *)ptr->local);
}

static struct execute *alloc_membit(size_t index)
{
	struct execute *bit;

	bit = malloctype(struct execute);
	if (bit == NULL) {
		Debug("malloctype error");
		return NULL;
	}
	if (lispd_make_mutexlite(&bit->mutex)) {
		Debug("lispd_make_mutexlite error");
		free(bit);
		return NULL;
	}
	clearpoint(bit);
	bit->state = ThreadState_Empty;
	bit->index = index;
	bit->result = 0;

	return bit;
}

static void free_membit(struct execute *bit)
{
	lispd_destroy_mutexlite(&bit->mutex);
	free(bit);
}

static struct execute **alloc_buffer_membit(size_t size)
{
	size_t i, k;
	struct execute **ptr, *bit;

	ptr = mallocsize(struct execute *, size);
	if (ptr == NULL) {
		Debug("mallocsize error");
		return NULL;
	}
	for (i = 0; i < size; i++) {
		bit = alloc_membit(i);
		if (bit == NULL) {
			Debug("alloc_membit error");
			for (k = 0; k < i; k++)
				free_membit(ptr[k]);
			free(ptr);
			return NULL;
		}
		ptr[i] = bit;
	}

	return ptr;
}

static void free_buffer_membit(struct execute **ptr)
{
	size_t i;

	if (ptr) {
		for (i = 0; i < Execute_Size; i++)
			free_membit(ptr[i]);
		free(ptr);
	}
}


/*
 *  initialize
 */
static int init_execute_main(struct execute **ptr, size_t size)
{
	struct execute *bit;
	jmp_buf *exec;
	LocalRoot local;

	exec = malloctype(jmp_buf);
	if (exec == NULL) {
		Debug("malloctype error");
		return 1;
	}

	local = make_local(size);
	if (local == NULL) {
		Debug("make_local error");
		free(exec);
		return 1;
	}

	bit = ptr[0];  /* main thread */
	/* thread info */
	bit->state = ThreadState_Run;
	bit->routine = 0;
	cleartype(bit->handle);
#ifdef LISP_THREAD_WINDOWS
	cleartype(bit->handleid);
#endif

	/* lisp info */
	bit->exec = exec;
	bit->local = local;
	bit->control = Unbound;

	/* values */
	init_execute_values(bit);

	/* threadlocal */
	set_execute_local(bit);

	return 0;
}

int init_execute(size_t size)
{
	struct execute **ptr;

	if (Execute_Array) {
		Debug("Execute_Array error.");
		return 1;
	}

	/* size check */
	if (size < SizeLimit)
		size = SizeLimit;

	/* execute structure */
	ptr = alloc_buffer_membit(EXECUTE_SIZE);
	if (ptr == NULL) {
		Debug("alloc_buffer_membit error");
		return 1;
	}

	/* sync object */
	if (lispd_make_mutexlite(&Execute_Mutex)) {
		Debug("lispd_make_mutexlite error");
		goto error1;
	}
	lispd_make_condlite(&Execute_Cond);

	/* threadlocal */
	init_execute_local();

	/* thread index 0 */
	if (init_execute_main(ptr, size)) {
		Debug("init_execute_main error");
		goto error3;
	}

	/* Global variables */
	Execute_Array = ptr;
	Execute_Size = EXECUTE_SIZE;
	Execute_Position = 1;
	Execute_LocalSize = size;

	return 0;

error3:
	free_buffer_membit(ptr);
	lispd_destroy_condlite(&Execute_Cond);
error1:
	lispd_destroy_mutexlite(&Execute_Mutex);
	return 1;
}

void free_execute(void)
{
	size_t i;
	struct execute *bit;

	if (Execute_Array == NULL)
		return;

	/* Execute_Array */
	for (i = 0; i < Execute_Size; i++) {
		bit = Execute_Array[i];
		if (bit->state != ThreadState_Empty) {
			free_local(bit->local);
			free(bit->exec);
		}
	}

	/* threadlocal */
	free_execute_local();

	/* Global variables */
	lispd_destroy_condlite(&Execute_Cond);
	lispd_destroy_mutexlite(&Execute_Mutex);
	free_buffer_membit(Execute_Array);
	Execute_Array = 0;
	Execute_Size = 0;
	Execute_Position = 0;
	Execute_LocalSize = 0;
}

int reload_execute(void)
{
	size_t size;

	size = Execute_LocalSize;
	free_execute();
	return init_execute(size);
}

static int extendmemory_execute(void)
{
	size_t size, i, k;
	struct execute **ptr, *bit;

	size = Execute_Size + EXECUTE_PLUS;
	ptr = reallocsize(Execute_Array, struct execute *, size);
	if (ptr == NULL) {
		Debug("reallocsize error");
		return 1;
	}
	for (i = Execute_Size; i < size; i++) {
		bit = alloc_membit(i);
		if (bit == NULL) {
			Debug("alloc_membit error");
			for (k = Execute_Size; k < i; k++)
				free_membit(ptr[k]);
			/* for realloc */
			Execute_Array = ptr;
			/* try recovery */
			ptr = reallocsize(Execute_Array, struct execute *, Execute_Size);
			if (ptr) Execute_Array = ptr;
			return 1;
		}
		ptr[i] = bit;
	}
	Execute_Array = ptr;
	Execute_Position = Execute_Size;
	Execute_Size = size;

	return 0;
}

static int findempty(struct execute **ret)
{
	size_t index;

	/* first try */
	for (index = Execute_Position; index < Execute_Size; index++) {
		if (Execute_Array[index]->state == ThreadState_Empty) {
			Execute_Position = index;
			*ret = Execute_Array[index];
			return 1;
		}
	}

	/* second try */
	for (index = 1; index < Execute_Position; index++) {
		if (Execute_Array[index]->state == ThreadState_Empty) {
			Execute_Position = index;
			*ret = Execute_Array[index];
			return 1;
		}
	}

	return 0;
}

static int findstate_execute(struct execute **ptr)
{
	lispd_lock_mutexlite(&Execute_Mutex);

	/* find state=ThreadState_Empty */
	if (findempty(ptr))
		goto finish;

	/* extend memory */
	if (extendmemory_execute()) {
		Debug("expandmemory error");
		lispd_unlock_mutexlite(&Execute_Mutex);
		return 1;
	}
	*ptr = Execute_Array[Execute_Position];

finish:
	(*ptr)->state = ThreadState_Run;
	lispd_unlock_mutexlite(&Execute_Mutex);

	return 0;
}

void setstate_execute(struct execute *ptr, enum ThreadState value)
{
	lispd_lock_mutexlite(&Execute_Mutex);
	ptr->state = value;
	lispd_unlock_mutexlite(&Execute_Mutex);
}

int make_execute(execfunction proc, struct execute **ret, size_t size)
{
	struct execute *ptr;

	/* size check */
	if (size < SizeLimit)
		size = SizeLimit;

	/* alloc */
	if (findstate_execute(&ptr)) {
		Debug("findstate_execute error");
		return 1;
	}

	/* thread */
	ptr->routine = proc;
	if (create_thread(proc, ptr)) {
		Debug("create_thread error");
		setstate_execute(ptr, ThreadState_Empty);
		return 1;
	}
	if (ret) *ret = ptr;

	return 0;
}

int join_execute(struct execute *ptr)
{
	int result;

	lispd_lock_mutexlite(&Execute_Mutex);
	switch (ptr->state) {
		case ThreadState_Empty:
			break;

		case ThreadState_Run:
			ptr->state = ThreadState_Join;
			goto join;

		case ThreadState_Finish:
			goto join;

		case ThreadState_Join:
			break;

		default:
			Debug("join_thread  state error");
			goto error;
	}
	lispd_unlock_mutexlite(&Execute_Mutex);
	return 0;

join:
	lispd_unlock_mutexlite(&Execute_Mutex);
	result = join_thread(&ptr->handle);
	if (result) {
		Debug("join_thread error");
		return 1;
	}
	setstate_execute(ptr, ThreadState_Empty);
	return 0;

error:
	lispd_unlock_mutexlite(&Execute_Mutex);
	return 1;
}

size_t count_execute(void)
{
	size_t i, count;

	count = 0;
	for (i = 0; i < Execute_Size; i++) {
		if (Execute_Array[i]->state != ThreadState_Empty)
			count++;
	}

	return count;
}

int joinindex_execute(size_t index)
{
	return join_execute(Execute_Array[index]);
}

struct execute *getexecute(size_t index)
{
	struct execute *result;

	lispd_lock_mutexlite(&Execute_Mutex);
	if (Execute_Size <= index) {
		lispd_unlock_mutexlite(&Execute_Mutex);
		Debug("index error");
		return NULL;
	}
	result = Execute_Array[index];
	lispd_unlock_mutexlite(&Execute_Mutex);

	return result;
}

int equal_control_restart(Execute ptr, addr control)
{
	return ptr->throw_value == throw_restart_case
		&& ptr->throw_control == control;
}

int equal_control_catch(Execute ptr, addr symbol)
{
	return ptr->throw_value == throw_catch
		&& ptr->throw_handler == symbol;
}


/*
 *  gc sync
 */
void gcstate_execute(enum GcMode mode)
{
	lisp_gcsync = mode;
}

static int gcstart_execute_check(struct execute *ptr)
{
	size_t i;

	for (i = 0; i < Execute_Size; i++) {
		if (Execute_Array[i]->state == ThreadState_Run)
			return 1;
	}

	return 0;
}

void gcstart_execute(struct execute *ptr)
{
	lispd_lock_mutexlite(&Execute_Mutex);
	ptr->state = ThreadState_GcWait;
	lispd_broadcast_condlite(&Execute_Cond);
	while (gcstart_execute_check(ptr))
		lispd_wait_condlite(&Execute_Cond, &Execute_Mutex);
	lispd_unlock_mutexlite(&Execute_Mutex);
}

void gcwait_execute(struct execute *ptr)
{
	lispd_lock_mutexlite(&Execute_Mutex);
	while (ptr->state != ThreadState_Run)
		lispd_wait_condlite(&Execute_Cond, &Execute_Mutex);
	lispd_unlock_mutexlite(&Execute_Mutex);
}

void gcend_execute(void)
{
	size_t i;
	struct execute *ptr;

	lispd_lock_mutexlite(&Execute_Mutex);
	for (i = 0; i < Execute_Size; i++) {
		ptr = Execute_Array[i];
		if (ptr->state == ThreadState_GcWait)
			ptr->state = ThreadState_Run;
	}
	lispd_broadcast_condlite(&Execute_Cond);
	lispd_unlock_mutexlite(&Execute_Mutex);
}

void foreach_execute(void (*call)(struct execute *))
{
	size_t i;
	struct execute *ptr;

	for (i = 0; i < Execute_Position; i++) {
		ptr = Execute_Array[i];
		if (ptr->state != ThreadState_Empty)
			call(ptr);
	}
}

int foreach_check_execute(int (*call)(struct execute *))
{
	size_t i;
	struct execute *ptr;

	for (i = 0; i < Execute_Position; i++) {
		ptr = Execute_Array[i];
		if (ptr->state != ThreadState_Empty) {
			if (call(ptr))
				return 1;
		}
	}

	return 0;
}


/************************************************************
 *  execute_object.c
 ************************************************************/

/*
 *  values
 */
#define PtrExecuteValues(x)		((addr *)PtrArrayA4(x))

static void execute_values_alloc(LocalRoot local, addr *ret, size_t size)
{
	local_array4(local, ret, LISPSYSTEM_VALUES, size);
}

void init_execute_values(struct execute *bit)
{
	int i;
	addr vector, *values;

	execute_values_alloc(bit->local, &vector, EXECUTE_VALUES + 1);
	values = PtrExecuteValues(vector);
	for (i = 0; i < EXECUTE_VALUES + 1; i++)
		values[i] = Unbound;
	bit->values_vector = vector;
	bit->values_reader = values;
	bit->sizer = 0;
}

void save_values_control(struct execute *ptr, addr *ret, size_t *rsize)
{
	addr pos, *reader;
	size_t i, size;

	size = ptr->sizer;
	if (EXECUTE_VALUES + 1 < size)
		size = EXECUTE_VALUES + 1;
	execute_values_alloc(ptr->local, &pos, size);
	reader = ptr->values_reader;
	for (i = 0; i < size; i++) {
		SetExecuteValues(pos, i, reader[i]);
	}
	*ret = pos;
	*rsize = size;
}

void restore_values_control(struct execute *ptr, addr pos, size_t size)
{
	addr vector, *reader;
	size_t i;

	if (EXECUTE_VALUES + 1 < size)
		size = EXECUTE_VALUES + 1;
	vector = ptr->values_vector;
	reader = PtrExecuteValues(pos);
	for (i = 0; i < size; i++) {
		SetExecuteValues(vector, i, reader[i]);
	}
	ptr->sizer = size;
}


/*
 *  throw
 */
void normal_throw_control(struct execute *ptr)
{
	ptr->throw_value = throw_normal;
	ptr->throw_handler = NULL;
	ptr->throw_control = NULL;
	ptr->throw_point = 0;
	ptr->throw_point_p = 0;
}

void save_throw_control(struct execute *ptr, struct execute_throw *save)
{
	save->throw_point_p = ptr->throw_point_p;
	save->throw_value = ptr->throw_value;
	save->throw_point = ptr->throw_point;
	save->throw_handler = ptr->throw_handler;
	save->throw_control = ptr->throw_control;
}

void restore_throw_control(struct execute *ptr, const struct execute_throw *save)
{
	ptr->throw_point_p = save->throw_point_p;
	ptr->throw_value = save->throw_value;
	ptr->throw_point = save->throw_point;
	ptr->throw_handler = save->throw_handler;
	ptr->throw_control = save->throw_control;
}


/*
 *  save
 */
static void execute_object_alloc(LocalRoot local, addr *ret, size_t size)
{
	local_smallsize(local, ret, LISPSYSTEM_EXECUTE, 1, size);
}

void save_execute_control(struct execute *ptr, addr *ret)
{
	addr pos, values;
	struct execute_throw *str;
	size_t size;

	/* object */
	execute_object_alloc(ptr->local, &pos, sizeoft(struct execute_throw));

	/* throw */
	str = (struct execute_throw *)PtrBodySS(pos);
	save_throw_control(ptr, str);

	/* values */
	save_values_control(ptr, &values, &size);
	str->size = size;
	SetArraySS(pos, 0, values);

	/* result */
	*ret = pos;
}

void restore_execute_control(struct execute *ptr, addr pos)
{
	addr values;
	struct execute_throw *str;
	size_t size;

	CheckType(pos, LISPSYSTEM_EXECUTE);
	str = (struct execute_throw *)PtrBodySS(pos);
	size = str->size;
	GetArraySS(pos, 0, &values);
	restore_throw_control(ptr, str);
	restore_values_control(ptr, values, size);
}


/*
 *  lexical
 */
#define PtrExecuteLexical(x)	((addr *)PtrArrayA4(x))
#define GetExecuteLexical		GetArrayA4

static void execute_lexical_alloc(LocalRoot local, addr *ret, size_t size)
{
	local_array4(local, ret, LISPSYSTEM_LEXICAL, size);
}

void lexical_control(struct execute *ptr, size_t size)
{
	addr pos;

	if (size == 0) {
		ptr->lexical_reader = NULL;
		ptr->lexical_vector = Nil;
	}
	else {
		execute_lexical_alloc(ptr->local, &pos, size);
		ptr->lexical_reader = PtrExecuteLexical(pos);
		ptr->lexical_vector = pos;
	}
}

void getlow_lexical_debug(struct execute *ptr, size_t index, addr *ret)
{
	addr pos;

	pos = ptr->lexical_vector;
	Check(pos == NULL, "lexical_vector error.");
	CheckType(pos, LISPSYSTEM_LEXICAL);
	GetExecuteLexical(pos, index, &pos);
	Check(ptr->lexical_reader[index] != pos, "lexical check error.");
	*ret = pos;
}

void setlow_lexical_debug(struct execute *ptr, size_t index, addr value)
{
	addr pos;

	pos = ptr->lexical_vector;
	Check(pos == NULL, "lexical_vector error.");
	CheckType(pos, LISPSYSTEM_LEXICAL);
	SetExecuteLexical(pos, index, value);
}

void get_lexical_control(struct execute *ptr, size_t index, addr *ret)
{
	addr pos;
	getlow_lexical_control(ptr, index, &pos);
	getvalue_reference(pos, ret);
}

void set_lexical_control(struct execute *ptr, size_t index, addr value)
{
	addr pos;

	getlow_lexical_control(ptr, index, &pos);
	if (GetType(pos) == LISPSYSTEM_REFERENCE)
		set_reference(pos, value);
	else
		setlow_lexical_control(ptr, index, value);
}

void reference_lexical_control(struct execute *ptr, size_t index)
{
	addr pos;

	getlow_lexical_control(ptr, index, &pos);
	if (GetType(pos) != LISPSYSTEM_REFERENCE) {
		reference_heap(&pos, pos);
		setlow_lexical_control(ptr, index, pos);
	}
}


/*
 *  closure
 */
struct closure_struct {
	size_t lexical;
};
#define ClosureStruct(x) ((struct closure_struct *)PtrBodySS(x))

void closure_heap(addr *ret, addr value, size_t lexical)
{
	addr pos;
	struct closure_struct *str;

	Check(GetType(value) == LISPSYSTEM_CLOSURE, "type error");
	heap_smallsize(&pos, LISPSYSTEM_CLOSURE, 1, sizeoft(struct closure_struct));
	SetArraySS(pos, 0, value);
	str = ClosureStruct(pos);
	str->lexical = lexical;
	*ret = pos;
}

void get_closure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_CLOSURE);
	GetArraySS(pos, 0, ret);
}

size_t lexical_closure(addr pos)
{
	CheckType(pos, LISPSYSTEM_CLOSURE);
	return ClosureStruct(pos)->lexical;
}


/*
 *  reference
 */
void reference_heap(addr *ret, addr value)
{
	Check(GetType(value) == LISPSYSTEM_REFERENCE, "type error");
	heap_array2(ret, LISPSYSTEM_REFERENCE, 1);
	SetArrayA2(*ret, 0, value);
}

void get_reference(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_REFERENCE);
	GetArrayA2(pos, 0, ret);
}

void set_reference(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_REFERENCE);
	SetArrayA2(pos, 0, value);
}

void getvalue_reference(addr pos, addr *ret)
{
	if (GetType(pos) == LISPSYSTEM_REFERENCE) {
		GetArrayA2(pos, 0, ret);
	}
	else {
		*ret = pos;
	}
}


/************************************************************
 *  execute_setjmp.c
 ************************************************************/

#ifdef LISP_ABORT_SETJMP
jmp_buf Lisp_abort_setjmp;
jmp_buf Lisp_degrade_setjmp;
#endif

/*
 *  abort
 */
static void abort_shutdown(void)
{
	exit_arch(1);
}

void abort_execute(void)
{
	/* handler */
	if (Lisp_abort_handler) {
		(*Lisp_abort_handler)();
	}

	/* default */
	(void)text_color_terme(NULL, print_color_bright_red);
	stderr_arch("\n\n");
	stderr_arch("**************\n");
	stderr_arch("  LISP ABORT  \n");
	stderr_arch("**************\n");
	(void)text_color_terme(NULL, print_color_reset);
	(void)end_terme();
	abort_shutdown();
}

lisp_abort_calltype set_abort_handler(lisp_abort_calltype call)
{
	lisp_abort_calltype ret;

	ret = Lisp_abort_handler;
	Lisp_abort_handler = call;
	return ret;
}

static void abort_setjmp_handler(void)
{
	Lisp_abort_throw();
}
lisp_abort_calltype set_abort_setjmp_handler(void)
{
	return set_abort_handler(abort_setjmp_handler);
}


/*
 *  degrade
 */
static void degrade_setjmp_handler(void)
{
	Lisp_degrade_throw();
}
lisp_abort_calltype set_degrade_setjmp_handler(void)
{
	return set_abort_handler(degrade_setjmp_handler);
}


/************************************************************
 *  execute_values.c
 ************************************************************/

void clear_values_execute(Execute ptr)
{
	addr values;
	size_t size;

	size = ptr->sizer;
	if (EXECUTE_VALUES < size)
		return;
	values = ptr->values_vector;
	for (; size < EXECUTE_VALUES; size++) {
		SetExecuteValues(values, size, Unbound);
	}
	SetExecuteValuesList(values, Unbound);
}

void setresult_control(Execute ptr, addr value)
{
	SetExecuteValues(ptr->values_vector, 0, value);
	ptr->sizer = 1;
}

void setbool_control(Execute ptr, int value)
{
	SetExecuteValues(ptr->values_vector, 0, value? T: Nil);
	ptr->sizer = 1;
}

static void pushvalues_control(addr values, size_t i, addr pos)
{
	addr list;

	Check(i < EXECUTE_VALUES, "values error");
	if (i == EXECUTE_VALUES) {
		conscar_heap(&list, pos);
	}
	else {
		GetExecuteValuesList(values, &list);
		cons_heap(&list, pos, list);
	}
	SetExecuteValuesList(values, list);
}

static void nreverse_values_control(addr values, size_t sizer)
{
	addr list;

	if (EXECUTE_VALUES < sizer) {
		GetExecuteValuesList(values, &list);
		nreverse(&list, list);
		SetExecuteValuesList(values, list);
	}
}

void setvalues_control(Execute ptr, ...)
{
	addr values, pos;
	va_list args;
	size_t i;

	setvalues_nil_control(ptr);
	va_start(args, ptr);
	values = ptr->values_vector;
	for (i = 0; ; i++) {
		pos = va_arg(args, addr);
		if (pos == NULL)
			break;
		Check(GetStatusDynamic(pos), "dynamic error");
		if (i < EXECUTE_VALUES - 1) {
			SetExecuteValues(values, i, pos);
		}
		else if (i == EXECUTE_VALUES - 1) {
			SetExecuteValues(values, i, pos);
			SetExecuteValuesList(values, Nil);
		}
		else {
			pushvalues_control(values, i, pos);
		}
	}
	va_end(args);
	ptr->sizer = i;
	nreverse_values_control(values, i);
}

void setvalues_nil_control(Execute ptr)
{
	ptr->sizer = 0;
}

void setvalues_list_control(Execute ptr, addr list)
{
	addr values, pos;
	size_t i;

	values = ptr->values_vector;
	for (i = 0; list != Nil; i++) {
		GetCons(list, &pos, &list);
		Check(GetStatusDynamic(pos), "dynamic error");
		if (i < EXECUTE_VALUES - 1) {
			SetExecuteValues(values, i, pos);
		}
		else if (i == EXECUTE_VALUES - 1) {
			SetExecuteValues(values, i, pos);
			SetExecuteValuesList(values, Nil);
		}
		else {
			pushvalues_control(values, i, pos);
		}
	}
	ptr->sizer = i;
	nreverse_values_control(values, i);
}

void getresult_control(Execute ptr, addr *ret)
{
	*ret = ptr->sizer? ptr->values_reader[0]: Nil;
}

void getvalues_control(Execute ptr, size_t index, addr *ret)
{
	addr list;

	if (ptr->sizer <= index) {
		*ret = Unbound;
		return;
	}
	if (index < EXECUTE_VALUES) {
		*ret = ptr->values_reader[index];
	}
	else {
		index -= EXECUTE_VALUES;
		GetExecuteValuesList(ptr->values_vector, &list);
		getnth_unsafe(list, index, ret);
	}
}

static void list_from_vector_control(LocalRoot local,
		addr *values, size_t size, addr list, addr *ret)
{
	size_t i;

	Check(size == 0, "size error");
	Check(EXECUTE_VALUES < size, "size error");
	for (i = size - 1; ; i--) {
		cons_alloc(local, &list, values[i], list);
		if (i <= 0)
			break;
	}
	*ret = list;
}

static void getvalues_list_control(Execute ptr, LocalRoot local, addr *ret)
{
	addr list;
	size_t size;

	size = ptr->sizer;
	if (size == 0) {
		*ret = Nil;
		return;
	}
	if (size <= EXECUTE_VALUES) {
		list_from_vector_control(local, ptr->values_reader, size, Nil, ret);
	}
	else {
		GetExecuteValuesList(ptr->values_vector, &list);
		copy_list_alloc_unsafe(local, &list, list);
		list_from_vector_control(local, ptr->values_reader, EXECUTE_VALUES, list, ret);
	}
}

void getvalues_list_control_local(Execute ptr, addr *ret)
{
	getvalues_list_control(ptr, ptr->local, ret);
}

void getvalues_list_control_heap(Execute ptr, addr *ret)
{
	getvalues_list_control(ptr, NULL, ret);
}

size_t lengthvalues_control(Execute ptr)
{
	return ptr->sizer;
}

void getvalues_root_control(Execute ptr, addr *ret)
{
	addr list;

	if (ptr->sizer <= EXECUTE_VALUES) {
		*ret = Unbound;
		return;
	}
	GetExecuteValuesList(ptr->values_vector, &list);
	GetCar(list, ret);
}

void getvalues_pop_control(Execute ptr, addr *ret)
{
	addr list;

	if (ptr->sizer <= EXECUTE_VALUES) {
		*ret = Unbound;
		return;
	}
	GetExecuteValuesList(ptr->values_vector, &list);
	GetCons(list, ret, &list);
	SetExecuteValuesList(ptr->values_vector, list);
	ptr->sizer--;
}

/************************************************************
 *  extern.c
 ************************************************************/

/*
 *  initialize
 */
void init_extern(void)
{
}


/************************************************************
 *  extern_control.c
 ************************************************************/

/*
 *  control
 */
void lisp_push_control(addr *ret)
{
	push_control(Execute_Thread, ret);
}

int lisp_pop_control_(addr control)
{
	if (GetType(control) != LISPTYPE_CONTROL)
		return fmte_("Invalid argument ~S.", control, NULL);

	return pop_control_(Execute_Thread, control);
}


/*
 *  special
 */
int lisp_push_special_(addr symbol, addr value)
{
	hold_value(symbol, &symbol);
	hold_value(value, &value);
	if (! symbolp(symbol))
		return fmte_("The argument ~S must be a symbol type.", symbol, NULL);
	if (value == NULL)
		value = Unbound;
	pushspecial_control(Execute_Thread, symbol, value);
	return 0;
}

int lisp_push_special8_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern8_(&symbol, NULL, name));
	return lisp_push_special_(symbol, value);
}

int lisp_push_special16_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern16_(&symbol, NULL, name));
	return lisp_push_special_(symbol, value);
}

int lisp_push_special32_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern32_(&symbol, NULL, name));
	return lisp_push_special_(symbol, value);
}

int lisp0_get_special_(addr *ret, addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol)) {
		*ret = Nil;
		return fmte_("The argument ~S must be a symbol type.", symbol, NULL);
	}
	getspecial_local(Execute_Thread, symbol, &symbol);
	return Result(ret, (symbol == Unbound)? NULL: symbol);
}

int lisp0_get_special8_(addr *ret, const void *name)
{
	addr symbol;
	Return(lisp0_intern8_(&symbol, NULL, name));
	return lisp0_get_special_(ret, symbol);
}

int lisp0_get_special16_(addr *ret, const void *name)
{
	addr symbol;
	Return(lisp0_intern16_(&symbol, NULL, name));
	return lisp0_get_special_(ret, symbol);
}

int lisp0_get_special32_(addr *ret, const void *name)
{
	addr symbol;
	Return(lisp0_intern32_(&symbol, NULL, name));
	return lisp0_get_special_(ret, symbol);
}

int lisp_get_special_(addr x, addr symbol)
{
	Return(lisp0_get_special_(&symbol, symbol));
	hold_set(x, symbol);
	return 0;
}

int lisp_get_special8_(addr x, const void *name)
{
	addr pos;

	Return(lisp0_get_special8_(&pos, name));
	hold_set(x, pos);
	return 0;
}

int lisp_get_special16_(addr x, const void *name)
{
	addr pos;

	Return(lisp0_get_special16_(&pos, name));
	hold_set(x, pos);
	return 0;
}

int lisp_get_special32_(addr x, const void *name)
{
	addr pos;

	Return(lisp0_get_special32_(&pos, name));
	hold_set(x, pos);
	return 0;
}

int lisp_set_special_(addr symbol, addr value)
{
	hold_value(symbol, &symbol);
	hold_value(value, &value);
	if (! symbolp(symbol))
		return fmte_("The argument ~S must be a symbol type.", symbol, NULL);
	if (value == NULL)
		value = Unbound;
	setspecial_local(Execute_Thread, symbol, value);
	return 0;
}

int lisp_set_special8_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern8_(&symbol, NULL, name));
	return lisp_set_special_(symbol, value);
}

int lisp_set_special16_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern16_(&symbol, NULL, name));
	return lisp_set_special_(symbol, value);
}

int lisp_set_special32_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern32_(&symbol, NULL, name));
	return lisp_set_special_(symbol, value);
}


/*
 *  defvar
 */
int lisp_defvar_(addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol))
		return TypeError_(symbol, SYMBOL);
	return setspecial_symbol_(symbol);
}

int lisp_defvar8_(const void *str)
{
	addr symbol;
	Return(lisp0_intern8_(&symbol, NULL, str));
	return lisp_defvar_(symbol);
}

int lisp_defvar16_(const void *str)
{
	addr symbol;
	Return(lisp0_intern16_(&symbol, NULL, str));
	return lisp_defvar_(symbol);
}

int lisp_defvar32_(const void *str)
{
	addr symbol;
	Return(lisp0_intern32_(&symbol, NULL, str));
	return lisp_defvar_(symbol);
}


/*
 *  throw
 */
void lisp_catch(addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol)) {
		lisp_abortf("Invalid catch symbol.");
		return;
	}
	catch_control(Execute_Thread, symbol);
}

int lisp_throw_(addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol))
		return fmte_("THROW argument ~S must be a symbol.", symbol, NULL);
	else
		return throw_control_(Execute_Thread, symbol);
}


/*
 *  handler
 */
int lisp_handler_bind_(addr name, addr call)
{
	hold_value(name, &name);
	hold_value(call, &call);
	return pushhandler_common_(Execute_Thread, name, call, 0);
}

int lisp_handler_case_(addr name, addr call)
{
	hold_value(name, &name);
	hold_value(call, &call);
	return pushhandler_common_(Execute_Thread, name, call, 1);
}

void lisp_handler_reverse(void)
{
	reverse_handler_control(Execute_Thread);
}


/*
 *  restart
 */
void lisp0_restart_make(addr *ret, addr name, addr call, int casep)
{
	addr restart;

	if (name == NULL)
		name = Nil;
	hold_value(name, &name);
	hold_value(call, &call);
	restart_heap(&restart, name);
	setfunction_restart(restart, call);
	setescape_restart(restart, casep);
	*ret = restart;
}

void lisp_restart_make(addr x, addr name, addr call, int casep)
{
	lisp0_restart_make(&name, name, call, casep);
	hold_set(x, name);
}

void lisp_restart_interactive(addr restart, addr call)
{
	hold_value(restart, &restart);
	if (! restartp(restart)) {
		Lisp_abort_type(restart, RESTART);
		return;
	}
	if (call == NULL)
		call = Nil;
	hold_value(call, &call);
	setinteractive_restart(restart, call);
}

void lisp_restart_report(addr restart, addr call)
{
	hold_value(restart, &restart);
	if (! restartp(restart)) {
		Lisp_abort_type(restart, RESTART);
		return;
	}
	if (call == NULL)
		call = Nil;
	hold_value(call, &call);
	setreport_restart(restart, call);
}

void lisp_restart_test(addr restart, addr call)
{
	hold_value(restart, &restart);
	if (! restartp(restart)) {
		Lisp_abort_type(restart, RESTART);
		return;
	}
	if (call == NULL)
		call = Nil;
	hold_value(call, &call);
	settest_restart(restart, call);
}

void lisp_restart_push(addr restart)
{
	hold_value(restart, &restart);
	pushrestart_control(Execute_Thread, restart);
}

void lisp_restart_reverse(void)
{
	reverse_restart_control(Execute_Thread);
}


/************************************************************
 *  extern_develop.c
 ************************************************************/

/*
 *  error
 */
void lisp_abort_type(addr value, constindex index)
{
	addr type;

	GetConstant(index, &type);
	lisp_abort8("type error: ~S must be a ~A type.", value, type, NULL);
}


/*
 *  list
 */
void lisp0_list_va_alloc(LocalRoot local, addr *ret, va_list args)
{
	addr x, y, next;

	x = va_arg(args, addr);
	if (x == NULL) {
		*ret = Nil;
		return;
	}
	hold_value(x, &x);
	conscar_alloc(local, &y, x);
	*ret = y;

	for (;;) {
		x = va_arg(args, addr);
		if (x == NULL)
			break;
		hold_value(x, &x);
		conscar_alloc(local, &next, x);
		SetCdr(y, next);
		y = next;
	}
}

void lisp0_lista_va_alloc(LocalRoot local, addr *ret, va_list args)
{
	addr pos1, pos2, pos3, cons;

	pos1 = va_arg(args, addr);
	/* nil */
	if (pos1 == NULL) {
		*ret = Nil; /* error */
		return;
	}
	hold_value(pos1, &pos1);

	/* dot list */
	pos2 = va_arg(args, addr);
	if (pos2 == NULL) {
		*ret = pos1;
		return;
	}
	hold_value(pos2, &pos2);

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
		hold_value(pos3, &pos3);

		/* (pos1 pos2 . ?) */
		conscar_alloc(local, &pos1, pos2);
		SetCdr(cons, pos1);
		cons = pos1;
		pos2 = pos3;
	}
}


/************************************************************
 *  extern_dlfile.c
 ************************************************************/
/*  Auto generated by mk.dlfile.lisp  */

#define LispdlSet(ptr, x) (ptr[lispdl_##x] = (void *)x)
void lisp_dlfile_make(lisp_dlfile_array ptr)
{
    /* variables */
    ptr[lispdl_nil] = Nil;
    ptr[lispdl_t] = T;

    /* extern_type.h */
    LispdlSet(ptr, lisp_hold_p);
    LispdlSet(ptr, lisp_hold_value);
    LispdlSet(ptr, lisp_hold_set);
    LispdlSet(ptr, Lisp_holdv);
    LispdlSet(ptr, lisp_hold);
    LispdlSet(ptr, Lisp_hold);
    LispdlSet(ptr, lisp0_nil);
    LispdlSet(ptr, lisp0_t);
    LispdlSet(ptr, lisp_nil);
    LispdlSet(ptr, lisp_t);
    LispdlSet(ptr, Lisp_nil);
    LispdlSet(ptr, Lisp_t);
    LispdlSet(ptr, lisp_nil_p);
    LispdlSet(ptr, lisp_t_p);
    LispdlSet(ptr, lisp_null_p);
    LispdlSet(ptr, lisp_character_p);
    LispdlSet(ptr, lisp_cons_p);
    LispdlSet(ptr, lisp_list_p);
    LispdlSet(ptr, lisp_string_p);
    LispdlSet(ptr, lisp_strvect_p);
    LispdlSet(ptr, lisp_symbol_p);
    LispdlSet(ptr, lisp_array_p);
    LispdlSet(ptr, lisp_vector_p);
    LispdlSet(ptr, lisp_fixnum_p);
    LispdlSet(ptr, lisp_bignum_p);
    LispdlSet(ptr, lisp_integer_p);
    LispdlSet(ptr, lisp_ratio_p);
    LispdlSet(ptr, lisp_rational_p);
    LispdlSet(ptr, lisp_single_float_p);
    LispdlSet(ptr, lisp_double_float_p);
    LispdlSet(ptr, lisp_long_float_p);
    LispdlSet(ptr, lisp_float_p);
    LispdlSet(ptr, lisp_real_p);
    LispdlSet(ptr, lisp_complex_p);
    LispdlSet(ptr, lisp_number_p);
    LispdlSet(ptr, lisp_clos_p);
    LispdlSet(ptr, lisp_hashtable_p);
    LispdlSet(ptr, lisp_readtable_p);
    LispdlSet(ptr, lisp_control_p);
    LispdlSet(ptr, lisp_callname_p);
    LispdlSet(ptr, lisp_function_p);
    LispdlSet(ptr, lisp_package_p);
    LispdlSet(ptr, lisp_random_state_p);
    LispdlSet(ptr, lisp_pathname_p);
    LispdlSet(ptr, lisp_stream_p);
    LispdlSet(ptr, lisp_restart_p);
    LispdlSet(ptr, lisp_environment_p);
    LispdlSet(ptr, lisp_bitvector_p);
    LispdlSet(ptr, lisp_print_dispatch_p);
    LispdlSet(ptr, lisp_paper_p);

    /* extern_sequence.h */
    LispdlSet(ptr, lisp0_cons);
    LispdlSet(ptr, lisp_cons);
    LispdlSet(ptr, lisp0_vector);
    LispdlSet(ptr, lisp_vector);
    LispdlSet(ptr, lisp0_list_va);
    LispdlSet(ptr, lisp0_lista_va);
    LispdlSet(ptr, lisp0_list);
    LispdlSet(ptr, lisp_list);
    LispdlSet(ptr, lisp0_lista);
    LispdlSet(ptr, lisp_lista);
    LispdlSet(ptr, lisp0_getelt_);
    LispdlSet(ptr, lisp_getelt_);
    LispdlSet(ptr, lisp_setelt_);
    LispdlSet(ptr, lisp_length_);
    LispdlSet(ptr, lisp0_reverse_);
    LispdlSet(ptr, lisp0_nreverse_);
    LispdlSet(ptr, lisp_reverse_);
    LispdlSet(ptr, lisp_nreverse_);
    LispdlSet(ptr, lisp0_car);
    LispdlSet(ptr, lisp0_cdr);
    LispdlSet(ptr, lisp0_carcdr);
    LispdlSet(ptr, lisp_car);
    LispdlSet(ptr, lisp_cdr);
    LispdlSet(ptr, lisp_carcdr);
    LispdlSet(ptr, lisp_setf_car);
    LispdlSet(ptr, lisp_setf_cdr);
    LispdlSet(ptr, lisp_setf_carcdr);
    LispdlSet(ptr, lisp0_string8_);
    LispdlSet(ptr, lisp0_string16_);
    LispdlSet(ptr, lisp0_string32_);
    LispdlSet(ptr, lisp_string8_);
    LispdlSet(ptr, lisp_string16_);
    LispdlSet(ptr, lisp_string32_);
    LispdlSet(ptr, lisp_string_getc_);
    LispdlSet(ptr, lisp_strvect_getc);
    LispdlSet(ptr, lisp_strvect_length);

    /* extern_object.h */
    LispdlSet(ptr, lisp0_character_);
    LispdlSet(ptr, lisp0_fixnum);
    LispdlSet(ptr, lisp0_float_);
    LispdlSet(ptr, lisp0_double_);
    LispdlSet(ptr, lisp0_long_double_);
    LispdlSet(ptr, lisp_character_);
    LispdlSet(ptr, lisp_fixnum);
    LispdlSet(ptr, lisp_float_);
    LispdlSet(ptr, lisp_double_);
    LispdlSet(ptr, lisp_long_double_);
    LispdlSet(ptr, lisp_zero_p);
    LispdlSet(ptr, lisp_plus_p);
    LispdlSet(ptr, lisp_minus_p);
    LispdlSet(ptr, lisp_get_character);
    LispdlSet(ptr, lisp_get_fixnum);
    LispdlSet(ptr, lisp_get_float_);
    LispdlSet(ptr, lisp_get_double_);
    LispdlSet(ptr, lisp_get_long_double_);
    LispdlSet(ptr, lisp0_package_);
    LispdlSet(ptr, lisp0_package8_);
    LispdlSet(ptr, lisp0_package16_);
    LispdlSet(ptr, lisp0_package32_);
    LispdlSet(ptr, lisp_package_);
    LispdlSet(ptr, lisp_package8_);
    LispdlSet(ptr, lisp_package16_);
    LispdlSet(ptr, lisp_package32_);
    LispdlSet(ptr, lisp_in_package_);
    LispdlSet(ptr, lisp_in_package8_);
    LispdlSet(ptr, lisp_in_package16_);
    LispdlSet(ptr, lisp_in_package32_);
    LispdlSet(ptr, lisp_push_and_in_package_);
    LispdlSet(ptr, lisp_push_and_in_package8_);
    LispdlSet(ptr, lisp_push_and_in_package16_);
    LispdlSet(ptr, lisp_push_and_in_package32_);
    LispdlSet(ptr, lisp0_intern_);
    LispdlSet(ptr, lisp0_intern8_);
    LispdlSet(ptr, lisp0_intern16_);
    LispdlSet(ptr, lisp0_intern32_);
    LispdlSet(ptr, lisp_intern_);
    LispdlSet(ptr, lisp_intern8_);
    LispdlSet(ptr, lisp_intern16_);
    LispdlSet(ptr, lisp_intern32_);
    LispdlSet(ptr, lisp0_reader_);
    LispdlSet(ptr, lisp0_reader8_);
    LispdlSet(ptr, lisp0_reader16_);
    LispdlSet(ptr, lisp0_reader32_);
    LispdlSet(ptr, lisp_reader_);
    LispdlSet(ptr, lisp_reader8_);
    LispdlSet(ptr, lisp_reader16_);
    LispdlSet(ptr, lisp_reader32_);
    LispdlSet(ptr, lisp0_pathname_);
    LispdlSet(ptr, lisp0_pathname8_);
    LispdlSet(ptr, lisp0_pathname16_);
    LispdlSet(ptr, lisp0_pathname32_);
    LispdlSet(ptr, lisp0_namestring_);
    LispdlSet(ptr, lisp_pathname_);
    LispdlSet(ptr, lisp_pathname8_);
    LispdlSet(ptr, lisp_pathname16_);
    LispdlSet(ptr, lisp_pathname32_);
    LispdlSet(ptr, lisp_namestring_);
    LispdlSet(ptr, lisp0_paper_);
    LispdlSet(ptr, lisp_paper_);
    LispdlSet(ptr, lisp_paper_gettype_);
    LispdlSet(ptr, lisp_paper_settype_);
    LispdlSet(ptr, lisp_paper_lenarray_);
    LispdlSet(ptr, lisp_paper_lenbody_);
    LispdlSet(ptr, lisp0_paper_getarray_);
    LispdlSet(ptr, lisp_paper_getarray_);
    LispdlSet(ptr, lisp_paper_setarray_);
    LispdlSet(ptr, lisp_paper_getbody_);
    LispdlSet(ptr, lisp_paper_setbody_);
    LispdlSet(ptr, lisp_paper_getmemory_);
    LispdlSet(ptr, lisp_paper_setmemory_);
    LispdlSet(ptr, lisp_paper_body_unsafe_);

    /* extern_init.h */
    LispdlSet(ptr, lisperror_stream);
    LispdlSet(ptr, lisperror_noeol);
    LispdlSet(ptr, lisperror_va);
    LispdlSet(ptr, lisperror);

    /* extern_execute.h */
    LispdlSet(ptr, lisp0_eval_);
    LispdlSet(ptr, lisp0_eval8_);
    LispdlSet(ptr, lisp0_eval16_);
    LispdlSet(ptr, lisp0_eval32_);
    LispdlSet(ptr, lisp_eval_);
    LispdlSet(ptr, lisp_eval8_);
    LispdlSet(ptr, lisp_eval16_);
    LispdlSet(ptr, lisp_eval32_);
    LispdlSet(ptr, lisp0_call_);
    LispdlSet(ptr, lisp_call_);
    LispdlSet(ptr, lisp0_funcall_);
    LispdlSet(ptr, lisp0_funcall8_);
    LispdlSet(ptr, lisp0_funcall16_);
    LispdlSet(ptr, lisp0_funcall32_);
    LispdlSet(ptr, lisp_funcall_);
    LispdlSet(ptr, lisp_funcall8_);
    LispdlSet(ptr, lisp_funcall16_);
    LispdlSet(ptr, lisp_funcall32_);
    LispdlSet(ptr, lisp0_apply_);
    LispdlSet(ptr, lisp0_apply8_);
    LispdlSet(ptr, lisp0_apply16_);
    LispdlSet(ptr, lisp0_apply32_);
    LispdlSet(ptr, lisp_apply_);
    LispdlSet(ptr, lisp_apply8_);
    LispdlSet(ptr, lisp_apply16_);
    LispdlSet(ptr, lisp_apply32_);
    LispdlSet(ptr, lisp_eval_control_);
    LispdlSet(ptr, lisp_eval_string_control_);
    LispdlSet(ptr, lisp_call_control_);
    LispdlSet(ptr, lisp_funcall_control_);
    LispdlSet(ptr, lisp_apply_control_);
    LispdlSet(ptr, lisp0_result_control);
    LispdlSet(ptr, lisp0_result2_control);
    LispdlSet(ptr, lisp0_values_control);
    LispdlSet(ptr, lisp0_nth_value_control);
    LispdlSet(ptr, lisp_result_control);
    LispdlSet(ptr, lisp_result2_control);
    LispdlSet(ptr, lisp_values_control);
    LispdlSet(ptr, lisp_nth_value_control);
    LispdlSet(ptr, lisp_set_result_control);
    LispdlSet(ptr, lisp_set_values_control);
    LispdlSet(ptr, lisp_set_values_nil_control);
    LispdlSet(ptr, lisp_set_values_list_control);
    LispdlSet(ptr, lisp_equal_control);
    LispdlSet(ptr, lisp_break_control);
    LispdlSet(ptr, lisp_escape_control);
    LispdlSet(ptr, lisp_reset_control);
    LispdlSet(ptr, lisp_escape_type_control);
    LispdlSet(ptr, lisp_save_control);
    LispdlSet(ptr, lisp_rollback_control);
    LispdlSet(ptr, lisp_eval_loop_);

    /* extern_control.h */
    LispdlSet(ptr, lisp_push_control);
    LispdlSet(ptr, lisp_pop_control_);
    LispdlSet(ptr, lisp_push_special_);
    LispdlSet(ptr, lisp_push_special8_);
    LispdlSet(ptr, lisp_push_special16_);
    LispdlSet(ptr, lisp_push_special32_);
    LispdlSet(ptr, lisp0_get_special_);
    LispdlSet(ptr, lisp0_get_special8_);
    LispdlSet(ptr, lisp0_get_special16_);
    LispdlSet(ptr, lisp0_get_special32_);
    LispdlSet(ptr, lisp_get_special_);
    LispdlSet(ptr, lisp_get_special8_);
    LispdlSet(ptr, lisp_get_special16_);
    LispdlSet(ptr, lisp_get_special32_);
    LispdlSet(ptr, lisp_set_special_);
    LispdlSet(ptr, lisp_set_special8_);
    LispdlSet(ptr, lisp_set_special16_);
    LispdlSet(ptr, lisp_set_special32_);
    LispdlSet(ptr, lisp_defvar_);
    LispdlSet(ptr, lisp_defvar8_);
    LispdlSet(ptr, lisp_defvar16_);
    LispdlSet(ptr, lisp_defvar32_);
    LispdlSet(ptr, lisp_catch);
    LispdlSet(ptr, lisp_throw_);
    LispdlSet(ptr, lisp_handler_bind_);
    LispdlSet(ptr, lisp_handler_case_);
    LispdlSet(ptr, lisp_handler_reverse);
    LispdlSet(ptr, lisp0_restart_make);
    LispdlSet(ptr, lisp_restart_make);
    LispdlSet(ptr, lisp_restart_interactive);
    LispdlSet(ptr, lisp_restart_report);
    LispdlSet(ptr, lisp_restart_test);
    LispdlSet(ptr, lisp_restart_push);
    LispdlSet(ptr, lisp_restart_reverse);

    /* extern_error.h */
    LispdlSet(ptr, lisp_abort);
    LispdlSet(ptr, lisp_abortf);
    LispdlSet(ptr, lisp_abort8);
    LispdlSet(ptr, lisp_abort16);
    LispdlSet(ptr, lisp_abort32);
    LispdlSet(ptr, lisp_set_abort_handler);
    LispdlSet(ptr, lisp_set_abort_setjmp_handler);
    LispdlSet(ptr, lisp_signal_);
    LispdlSet(ptr, lisp_error_);
    LispdlSet(ptr, lisp_error8_);
    LispdlSet(ptr, lisp_error16_);
    LispdlSet(ptr, lisp_error32_);
    LispdlSet(ptr, lisp_warn8_);
    LispdlSet(ptr, lisp_warn16_);
    LispdlSet(ptr, lisp_warn32_);

    /* extern_function.h */
    LispdlSet(ptr, lisp0_get_function);
    LispdlSet(ptr, lisp0_get_setf);
    LispdlSet(ptr, lisp_get_function);
    LispdlSet(ptr, lisp_get_setf);
    LispdlSet(ptr, lisp0_get_function_);
    LispdlSet(ptr, lisp0_get_function8_);
    LispdlSet(ptr, lisp0_get_function16_);
    LispdlSet(ptr, lisp0_get_function32_);
    LispdlSet(ptr, lisp_get_function_);
    LispdlSet(ptr, lisp_get_function8_);
    LispdlSet(ptr, lisp_get_function16_);
    LispdlSet(ptr, lisp_get_function32_);
    LispdlSet(ptr, lisp0_get_setf_);
    LispdlSet(ptr, lisp0_get_setf8_);
    LispdlSet(ptr, lisp0_get_setf16_);
    LispdlSet(ptr, lisp0_get_setf32_);
    LispdlSet(ptr, lisp_get_setf_);
    LispdlSet(ptr, lisp_get_setf8_);
    LispdlSet(ptr, lisp_get_setf16_);
    LispdlSet(ptr, lisp_get_setf32_);
    LispdlSet(ptr, lisp_compiled_macro);
    LispdlSet(ptr, lisp_compiled_rest);
    LispdlSet(ptr, lisp_compiled_dynamic);
    LispdlSet(ptr, lisp_compiled_any);
    LispdlSet(ptr, lisp_compiled_empty);
    LispdlSet(ptr, lisp_compiled_var1);
    LispdlSet(ptr, lisp_compiled_var2);
    LispdlSet(ptr, lisp_compiled_var3);
    LispdlSet(ptr, lisp_compiled_var4);
    LispdlSet(ptr, lisp_compiled_var5);
    LispdlSet(ptr, lisp_compiled_var6);
    LispdlSet(ptr, lisp_compiled_opt1);
    LispdlSet(ptr, lisp_compiled_opt2);
    LispdlSet(ptr, lisp_compiled_opt3);
    LispdlSet(ptr, lisp_compiled_var1opt1);
    LispdlSet(ptr, lisp_compiled_var1opt2);
    LispdlSet(ptr, lisp_compiled_var1opt3);
    LispdlSet(ptr, lisp_compiled_var2opt1);
    LispdlSet(ptr, lisp_compiled_var2opt2);
    LispdlSet(ptr, lisp_compiled_var2opt3);
    LispdlSet(ptr, lisp_compiled_var3opt1);
    LispdlSet(ptr, lisp_compiled_var3opt2);
    LispdlSet(ptr, lisp_compiled_var3opt3);
    LispdlSet(ptr, lisp_compiled_var1rest);
    LispdlSet(ptr, lisp_compiled_var2rest);
    LispdlSet(ptr, lisp_compiled_var3rest);
    LispdlSet(ptr, lisp_compiled_var1dynamic);
    LispdlSet(ptr, lisp_compiled_var2dynamic);
    LispdlSet(ptr, lisp_compiled_var3dynamic);
    LispdlSet(ptr, lisp0_compiled_function_);
    LispdlSet(ptr, lisp0_compiled_function8_);
    LispdlSet(ptr, lisp0_compiled_function16_);
    LispdlSet(ptr, lisp0_compiled_function32_);
    LispdlSet(ptr, lisp_compiled_function_);
    LispdlSet(ptr, lisp_compiled_function8_);
    LispdlSet(ptr, lisp_compiled_function16_);
    LispdlSet(ptr, lisp_compiled_function32_);
    LispdlSet(ptr, lisp_compiled_defun_);
    LispdlSet(ptr, lisp_compiled_defun8_);
    LispdlSet(ptr, lisp_compiled_defun16_);
    LispdlSet(ptr, lisp_compiled_defun32_);
    LispdlSet(ptr, lisp_compiled_defun_setf_);
    LispdlSet(ptr, lisp_compiled_defun_setf8_);
    LispdlSet(ptr, lisp_compiled_defun_setf16_);
    LispdlSet(ptr, lisp_compiled_defun_setf32_);
    LispdlSet(ptr, lisp_compiled_setvalue);
    LispdlSet(ptr, lisp_compiled_getvalue);

    /* extern_instance.h */
    LispdlSet(ptr, lisp0_find_class);
    LispdlSet(ptr, lisp0_find_class_);
    LispdlSet(ptr, lisp0_find_class8_);
    LispdlSet(ptr, lisp0_find_class16_);
    LispdlSet(ptr, lisp0_find_class32_);
    LispdlSet(ptr, lisp_find_class);
    LispdlSet(ptr, lisp_find_class_);
    LispdlSet(ptr, lisp_find_class8_);
    LispdlSet(ptr, lisp_find_class16_);
    LispdlSet(ptr, lisp_find_class32_);
    LispdlSet(ptr, lisp0_instance_);
    LispdlSet(ptr, lisp0_instance8_);
    LispdlSet(ptr, lisp0_instance16_);
    LispdlSet(ptr, lisp0_instance32_);
    LispdlSet(ptr, lisp_instance_);
    LispdlSet(ptr, lisp_instance8_);
    LispdlSet(ptr, lisp_instance16_);
    LispdlSet(ptr, lisp_instance32_);
    LispdlSet(ptr, lisp_slot_exists_);
    LispdlSet(ptr, lisp_slot_exists8_);
    LispdlSet(ptr, lisp_slot_exists16_);
    LispdlSet(ptr, lisp_slot_exists32_);
    LispdlSet(ptr, lisp_slot_boundp_);
    LispdlSet(ptr, lisp_slot_boundp8_);
    LispdlSet(ptr, lisp_slot_boundp16_);
    LispdlSet(ptr, lisp_slot_boundp32_);
    LispdlSet(ptr, lisp_slot_makunbound_);
    LispdlSet(ptr, lisp_slot_makunbound8_);
    LispdlSet(ptr, lisp_slot_makunbound16_);
    LispdlSet(ptr, lisp_slot_makunbound32_);
    LispdlSet(ptr, lisp0_slot_value_);
    LispdlSet(ptr, lisp0_slot_value8_);
    LispdlSet(ptr, lisp0_slot_value16_);
    LispdlSet(ptr, lisp0_slot_value32_);
    LispdlSet(ptr, lisp_slot_value_);
    LispdlSet(ptr, lisp_slot_value8_);
    LispdlSet(ptr, lisp_slot_value16_);
    LispdlSet(ptr, lisp_slot_value32_);
    LispdlSet(ptr, lisp_slot_setf_);
    LispdlSet(ptr, lisp_slot_setf8_);
    LispdlSet(ptr, lisp_slot_setf16_);
    LispdlSet(ptr, lisp_slot_setf32_);

    /* extern_print.h */
    LispdlSet(ptr, lisp_format8_);
    LispdlSet(ptr, lisp_format16_);
    LispdlSet(ptr, lisp_format32_);
    LispdlSet(ptr, lisp_stdout8_);
    LispdlSet(ptr, lisp_stdout16_);
    LispdlSet(ptr, lisp_stdout32_);
    LispdlSet(ptr, lisp_stderr8_);
    LispdlSet(ptr, lisp_stderr16_);
    LispdlSet(ptr, lisp_stderr32_);
    LispdlSet(ptr, lisp0_stringf8_);
    LispdlSet(ptr, lisp0_stringf16_);
    LispdlSet(ptr, lisp0_stringf32_);
    LispdlSet(ptr, lisp_stringf8_);
    LispdlSet(ptr, lisp_stringf16_);
    LispdlSet(ptr, lisp_stringf32_);

    /* extern_stream.h */
    LispdlSet(ptr, lisp0_stream_define);
    LispdlSet(ptr, lisp_stream_define);
    LispdlSet(ptr, lisp_stream_memory);
    LispdlSet(ptr, lisp0_getinfo_stream);
    LispdlSet(ptr, lisp_getinfo_stream);
    LispdlSet(ptr, lisp_setinfo_stream);
    LispdlSet(ptr, lisp_stream_calltype_close);
    LispdlSet(ptr, lisp_stream_calltype_read_byte);
    LispdlSet(ptr, lisp_stream_calltype_unread_byte);
    LispdlSet(ptr, lisp_stream_calltype_write_byte);
    LispdlSet(ptr, lisp_stream_calltype_read_char);
    LispdlSet(ptr, lisp_stream_calltype_read_hang);
    LispdlSet(ptr, lisp_stream_calltype_unread_char);
    LispdlSet(ptr, lisp_stream_calltype_write_char);
    LispdlSet(ptr, lisp_stream_calltype_getleft);
    LispdlSet(ptr, lisp_stream_calltype_setleft);
    LispdlSet(ptr, lisp_stream_calltype_inputp);
    LispdlSet(ptr, lisp_stream_calltype_outputp);
    LispdlSet(ptr, lisp_stream_calltype_interactivep);
    LispdlSet(ptr, lisp_stream_calltype_characterp);
    LispdlSet(ptr, lisp_stream_calltype_binaryp);
    LispdlSet(ptr, lisp_stream_calltype_element_type);
    LispdlSet(ptr, lisp_stream_calltype_external_format);
    LispdlSet(ptr, lisp_stream_calltype_file_length);
    LispdlSet(ptr, lisp_stream_calltype_file_position);
    LispdlSet(ptr, lisp_stream_calltype_file_position_start);
    LispdlSet(ptr, lisp_stream_calltype_file_position_end);
    LispdlSet(ptr, lisp_stream_calltype_file_position_set);
    LispdlSet(ptr, lisp_stream_calltype_file_charlen);
    LispdlSet(ptr, lisp_stream_calltype_file_strlen);
    LispdlSet(ptr, lisp_stream_calltype_listen);
    LispdlSet(ptr, lisp_stream_calltype_clear_input);
    LispdlSet(ptr, lisp_stream_calltype_finish_output);
    LispdlSet(ptr, lisp_stream_calltype_force_output);
    LispdlSet(ptr, lisp_stream_calltype_clear_output);
    LispdlSet(ptr, lisp_stream_calltype_exitpoint);
    LispdlSet(ptr, lisp_stream_calltype_termsize);
    LispdlSet(ptr, lisp_stream_calltype_error_close);
    LispdlSet(ptr, lisp_stream_calltype_error_read_byte);
    LispdlSet(ptr, lisp_stream_calltype_error_unread_byte);
    LispdlSet(ptr, lisp_stream_calltype_error_write_byte);
    LispdlSet(ptr, lisp_stream_calltype_error_read_char);
    LispdlSet(ptr, lisp_stream_calltype_error_read_hang);
    LispdlSet(ptr, lisp_stream_calltype_error_unread_char);
    LispdlSet(ptr, lisp_stream_calltype_error_write_char);
    LispdlSet(ptr, lisp_stream_calltype_error_getleft);
    LispdlSet(ptr, lisp_stream_calltype_error_setleft);
    LispdlSet(ptr, lisp_stream_calltype_error_inputp);
    LispdlSet(ptr, lisp_stream_calltype_error_outputp);
    LispdlSet(ptr, lisp_stream_calltype_error_interactivep);
    LispdlSet(ptr, lisp_stream_calltype_error_characterp);
    LispdlSet(ptr, lisp_stream_calltype_error_binaryp);
    LispdlSet(ptr, lisp_stream_calltype_error_element_type);
    LispdlSet(ptr, lisp_stream_calltype_error_external_format);
    LispdlSet(ptr, lisp_stream_calltype_error_file_length);
    LispdlSet(ptr, lisp_stream_calltype_error_file_position);
    LispdlSet(ptr, lisp_stream_calltype_error_file_position_start);
    LispdlSet(ptr, lisp_stream_calltype_error_file_position_end);
    LispdlSet(ptr, lisp_stream_calltype_error_file_position_set);
    LispdlSet(ptr, lisp_stream_calltype_error_file_charlen);
    LispdlSet(ptr, lisp_stream_calltype_error_file_strlen);
    LispdlSet(ptr, lisp_stream_calltype_error_listen);
    LispdlSet(ptr, lisp_stream_calltype_error_clear_input);
    LispdlSet(ptr, lisp_stream_calltype_error_finish_output);
    LispdlSet(ptr, lisp_stream_calltype_error_force_output);
    LispdlSet(ptr, lisp_stream_calltype_error_clear_output);
    LispdlSet(ptr, lisp_stream_calltype_error_exitpoint);
    LispdlSet(ptr, lisp_stream_calltype_error_termsize);

    /* extern_unicode.h */
    LispdlSet(ptr, lisp_eastasian_set);
    LispdlSet(ptr, lisp_eastasian_get);
    LispdlSet(ptr, lisp_eastasian_type_unicode);
    LispdlSet(ptr, lisp_eastasian_type_character);
    LispdlSet(ptr, lisp_eastasian_unicode);
    LispdlSet(ptr, lisp_eastasian_character_);
    LispdlSet(ptr, lisp_eastasian_string_);
    LispdlSet(ptr, lisp_eastasian_width_);
    LispdlSet(ptr, lisp_unicode_count);
    LispdlSet(ptr, lisp_utf8_encode);
    LispdlSet(ptr, lisp_utf16_range);
    LispdlSet(ptr, lisp_utf16_high);
    LispdlSet(ptr, lisp_utf16_low);
    LispdlSet(ptr, lisp_utf16_merge);

    /* End */
    ptr[lispdl_unbound] = Unbound;
    ptr[lispdl_end] = NULL;
}
#undef LispdlSet

/************************************************************
 *  extern_error.c
 ************************************************************/

/*
 *  abort
 */
void lisp_abort(void)
{
	abort_execute();
}

void lisp_abortf(const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
	lisperror_va(fmt, args);
	va_end(args);

	/* abort */
	lisp_abort();
}

static void lisp_abort_call(addr format, addr list)
{
	int check;
	addr stream;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(SPECIAL_ERROR_OUTPUT, &stream);
	getspecial_local(ptr, stream, &stream);
	if (stream == Unbound || (! streamp(stream))) {
		lisp_abortf("lisp_abort: Invalid error-output stream.");
		return;
	}
	/* output */
	check = fresh_line_stream_(stream, NULL);
	check |= format_stream_lisp_(ptr, stream, format, list);
	if (check) {
		lisp_abortf("lisp_abort: format error.");
		return;
	}

	/* abort */
	lisp_abort();
}

void lisp_abort8(const void *fmt, ...)
{
	addr format, list;
	va_list va;

	if (lisp0_string8_(&format, fmt)) {
		lisp_abortf("Invalid unicode format");
		return;
	}
	va_start(va, fmt);
	lisp0_list_va(&list, va);
	va_end(va);

	lisp_abort_call(format, list);
}

void lisp_abort16(const void *fmt, ...)
{
	addr format, list;
	va_list va;

	if (lisp0_string16_(&format, fmt)) {
		lisp_abortf("Invalid unicode format");
		return;
	}
	va_start(va, fmt);
	lisp0_list_va(&list, va);
	va_end(va);

	lisp_abort_call(format, list);
}

void lisp_abort32(const void *fmt, ...)
{
	addr format, list;
	va_list va;

	if (lisp0_string32_(&format, fmt)) {
		lisp_abortf("Invalid unicode format");
		return;
	}
	va_start(va, fmt);
	lisp0_list_va(&list, va);
	va_end(va);

	lisp_abort_call(format, list);
}

lisp_abort_calltype lisp_set_abort_handler(lisp_abort_calltype call)
{
	return set_abort_handler(call);
}

lisp_abort_calltype lisp_set_abort_setjmp_handler(void)
{
	return set_abort_setjmp_handler();
}


/*
 *  signal
 */
int lisp_signal_(addr condition)
{
	hold_value(condition, &condition);
	return signal_function_(Execute_Thread, condition);
}

int lisp_error_(addr condition)
{
	hold_value(condition, &condition);
	return error_function_(Execute_Thread, condition);
}


/*
 *  error
 */
int lisp_error8_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return call_simple_error_(NULL, format, args);
}

int lisp_error16_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return call_simple_error_(NULL, format, args);
}

int lisp_error32_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return call_simple_error_(NULL, format, args);
}


/*
 *  warn
 */
int lisp_warn8_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return call_simple_warning_(NULL, format, args);
}

int lisp_warn16_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return call_simple_warning_(NULL, format, args);
}

int lisp_warn32_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return call_simple_warning_(NULL, format, args);
}


/************************************************************
 *  extern_execute.c
 ************************************************************/

/*
 *  eval
 */
int lisp0_eval_(addr *ret, addr pos)
{
	hold_value(pos, &pos);
	Return(eval_execute_partial_(Execute_Thread, pos));
	if (ret)
		getresult_control(Execute_Thread, ret);

	return 0;
}

int lisp0_eval8_(addr *ret, const void *str)
{
	addr pos;

	Return(lisp0_reader8_(&pos, str));
	if (lisp_null_p(pos)) {
		Return(lisp0_string8_(&pos, str));
		return fmte_("Invalid eval string ~S.", str, NULL);
	}

	return lisp0_eval_(ret, pos);
}

int lisp0_eval16_(addr *ret, const void *str)
{
	addr pos;

	Return(lisp0_reader16_(&pos, str));
	if (lisp_null_p(pos)) {
		Return(lisp0_string16_(&pos, str));
		return fmte_("Invalid eval string ~S.", str, NULL);
	}

	return lisp0_eval_(ret, pos);
}

int lisp0_eval32_(addr *ret, const void *str)
{
	addr pos;

	Return(lisp0_reader32_(&pos, str));
	if (lisp_null_p(pos)) {
		Return(lisp0_string32_(&pos, str));
		return fmte_("Invalid eval string ~S.", str, NULL);
	}

	return lisp0_eval_(ret, pos);
}

int lisp_eval_(addr x, addr value)
{
	addr pos;

	Return(lisp0_eval_(&pos, value));
	hold_set_null(x, pos);
	return 0;
}

int lisp_eval8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_eval8_(&pos, str));
	hold_set_null(x, pos);
	return 0;
}

int lisp_eval16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_eval16_(&pos, str));
	hold_set_null(x, pos);
	return 0;
}

int lisp_eval32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_eval32_(&pos, str));
	hold_set_null(x, pos);
	return 0;
}


/*
 *  call
 */
int lisp0_call_(addr *ret, addr call, addr args)
{
	Return(lisp0_get_function_(&call, call));
	hold_value(call, &call);
	hold_value(args, &args);
	Return(apply1_control_(Execute_Thread, &call, call, args));
	if (ret)
		*ret = call;

	return 0;
}

int lisp_call_(addr x, addr call, addr args)
{
	Return(lisp0_get_function_(&call, call));
	hold_value(call, &call);
	hold_value(args, &args);
	Return(apply1_control_(Execute_Thread, &call, call, args));
	hold_set_null(x, call);

	return 0;
}


/*
 *  funcall
 */
int lisp0_funcall_(addr *ret, addr call, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp0_funcall8_(addr *ret, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function8_(&call, str));
	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp0_funcall16_(addr *ret, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function16_(&call, str));
	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp0_funcall32_(addr *ret, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function32_(&call, str));
	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_funcall_(addr x, addr call, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_funcall8_(addr x, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function8_(&call, str));
	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_funcall16_(addr x, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function16_(&call, str));
	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_funcall32_(addr x, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function32_(&call, str));
	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}


/*
 *  apply
 */
int lisp0_apply_(addr *ret, addr call, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function_(&call, call));
	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp0_apply8_(addr *ret, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function8_(&call, str));
	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp0_apply16_(addr *ret, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function16_(&call, str));
	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp0_apply32_(addr *ret, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function32_(&call, str));
	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_apply_(addr x, addr call, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function_(&call, call));
	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_apply8_(addr x, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function8_(&call, str));
	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_apply16_(addr x, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function16_(&call, str));
	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_apply32_(addr x, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function32_(&call, str));
	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}


/*
 *  lowlevel
 */
int lisp_eval_control_(addr eval)
{
	hold_value(eval, &eval);
	return eval_execute_partial_(Execute_Thread, eval);
}

int lisp_eval_string_control_(addr eval)
{
	addr pos;

	hold_value(eval, &eval);
	Return(lisp0_reader_(&pos, eval));
	if (pos == NULL)
		return fmte_("Invalid eval string ~S.", eval, NULL);

	return lisp_eval_control_(pos);
}

int lisp_call_control_(addr call, addr args)
{
	Return(lisp0_get_function_(&call, call));
	hold_value(call, &call);
	hold_value(args, &args);
	return apply_control_(Execute_Thread, call, args);
}

int lisp_funcall_control_(addr call, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);
	Return(lisp_call_control_(call, args));

	return 0;
}

int lisp_apply_control_(addr call, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);
	Return(lisp_call_control_(call, args));
	rollback_local(local, stack);

	return 0;
}


/*
 *  values
 */
void lisp0_result_control(addr *ret)
{
	getresult_control(Execute_Thread, ret);
}

void lisp0_result2_control(addr *ret1, addr *ret2)
{
	addr x, y;
	Execute ptr;

	ptr = Execute_Thread;
	getvalues_control(ptr, 0, &x);
	getvalues_control(ptr, 1, &y);
	*ret1 = (x == Unbound)? Nil: x;
	*ret2 = (y == Unbound)? Nil: y;
}

void lisp0_values_control(addr *ret)
{
	getvalues_list_control_heap(Execute_Thread, ret);
}

void lisp0_nth_value_control(addr *ret, size_t index)
{
	addr pos;
	getvalues_control(Execute_Thread, index, &pos);
	*ret = (pos == Unbound)? Nil: pos;
}

void lisp_result_control(addr x)
{
	addr pos;
	lisp0_result_control(&pos);
	hold_set(x, pos);
}

void lisp_result2_control(addr x, addr y)
{
	addr pos1, pos2;
	lisp0_result2_control(&pos1, &pos2);
	hold_set(x, pos1);
	hold_set(y, pos2);
}

void lisp_values_control(addr x)
{
	addr pos;
	lisp0_values_control(&pos);
	hold_set(x, pos);
}

void lisp_nth_value_control(addr x, size_t index)
{
	addr pos;
	lisp0_nth_value_control(&pos, index);
	hold_set(x, pos);
}

void lisp_set_result_control(addr value)
{
	hold_value(value, &value);
	setresult_control(Execute_Thread, value);
}

void lisp_set_values_control(addr first, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	/* list */
	va_start(va, first);
	lisp0_list_va_alloc(local, &args, va);
	hold_value(first, &first);
	cons_local(local, &args, first, args);

	/* setvalues */
	setvalues_list_control(ptr, args);
	rollback_local(local, stack);
}

void lisp_set_values_nil_control(void)
{
	setvalues_nil_control(Execute_Thread);
}

void lisp_set_values_list_control(addr list)
{
	hold_value(list, &list);
	setvalues_list_control(Execute_Thread, list);
}


/*
 *  escape
 */
int lisp_equal_control(addr control)
{
	Execute ptr;

	ptr = Execute_Thread;
	return (ptr->throw_value != throw_normal)
		&& (ptr->throw_control == control);
}

int lisp_break_control(void)
{
	Execute ptr;

	ptr = Execute_Thread;
	return (ptr->throw_value != throw_normal)
		&& (ptr->throw_control == ptr->control);
}

int lisp_escape_control(void)
{
	Execute ptr;
	ptr = Execute_Thread;
	return ptr->throw_value != throw_normal;
}

void lisp_reset_control(void)
{
	Execute ptr;
	ptr = Execute_Thread;
	normal_throw_control(ptr);
}

enum lisp_escape lisp_escape_type_control(void)
{
	Execute ptr;
	ptr = Execute_Thread;
	switch (ptr->throw_value) {
		case throw_normal:
			return lisp_escape_normal;

		case throw_tagbody:
			return lisp_escape_tagbody;

		case throw_block:
			return lisp_escape_block;

		case throw_catch:
			return lisp_escape_catch;

		case throw_handler_case:
			return lisp_escape_handler_case;

		case throw_restart_case:
			return lisp_escape_restart_case;

		default:
			lisp_abortf("Invalid escape type.", NULL);
			return lisp_escape_normal;
	}
}

void lisp_save_control(addr *ret)
{
	save_execute_control(Execute_Thread, ret);
}

void lisp_rollback_control(addr value)
{
	restore_execute_control(Execute_Thread, value);
}


/*
 *  system
 */
int lisp_eval_loop_(void)
{
	return eval_main_loop_(Execute_Thread);
}


/************************************************************
 *  extern_function.c
 ************************************************************/

/*
 *  function
 */
void lisp0_get_function(addr *ret, addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol)) {
		*ret = Nil;
		Lisp_abort_type(symbol, SYMBOL);
		return;
	}
	GetFunctionSymbol(symbol, &symbol);
	*ret = (symbol == Unbound)? NULL: symbol;
}

void lisp0_get_setf(addr *ret, addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol)) {
		*ret = Nil;
		Lisp_abort_type(symbol, SYMBOL);
		return;
	}
	getsetf_symbol(symbol, &symbol);
	*ret = (symbol == Unbound)? NULL: symbol;
}

void lisp_get_function(addr x, addr symbol)
{
	lisp0_get_function(&symbol, symbol);
	hold_set(x, symbol);
}

void lisp_get_setf(addr x, addr symbol)
{
	lisp0_get_setf(&symbol, symbol);
	hold_set(x, symbol);
}

int lisp0_get_function_(addr *ret, addr value)
{
	hold_value(value, &value);
	if (functionp(value))
		return Result(ret, value);
	if (symbolp(value))
		return getfunction_global_(value, ret);

	/* error */
	*ret = Nil;
	return TypeError_(value, SYMBOL);
}

int lisp0_get_function8_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern8_(&pos, NULL, str));
	return getfunction_global_(pos, ret);
}

int lisp0_get_function16_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern16_(&pos, NULL, str));
	return getfunction_global_(pos, ret);
}

int lisp0_get_function32_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern32_(&pos, NULL, str));
	return getfunction_global_(pos, ret);
}

int lisp_get_function_(addr x, addr value)
{
	Return(lisp0_get_function_(&value, value));
	hold_set(x, value);
	return 0;
}

int lisp_get_function8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_function8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_get_function16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_function16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_get_function32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_function32_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp0_get_setf_(addr *ret, addr value)
{
	hold_value(value, &value);
	if (functionp(value))
		return Result(ret, value);
	if (symbolp(value))
		return getsetf_global_(value, ret);

	/* error */
	*ret = Nil;
	return TypeError_(value, SYMBOL);
}

int lisp0_get_setf8_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern8_(&pos, NULL, str));
	return getsetf_global_(pos, ret);
}

int lisp0_get_setf16_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern16_(&pos, NULL, str));
	return getsetf_global_(pos, ret);
}

int lisp0_get_setf32_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern32_(&pos, NULL, str));
	return getsetf_global_(pos, ret);
}

int lisp_get_setf_(addr x, addr value)
{
	Return(lisp0_get_setf_(&value, value));
	hold_set(x, value);
	return 0;
}

int lisp_get_setf8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_setf8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_get_setf16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_setf16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_get_setf32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_setf32_(&pos, str));
	hold_set(x, pos);
	return 0;
}


/*
 *  compiled
 */
static void lisp_compiled_index_check(int index)
{
	addr pos;

	if (index < 0 || LISP_POINTER_EXTEND <= index) {
		fixnum_heap(&pos, (fixnum)index);
		lisp_abort8("Invalid index value ~S.", pos, NULL);
	}
}

void lisp_compiled_macro(int index, lisp_calltype_macro call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_macro(index, call);
}

void lisp_compiled_rest(int index, lisp_calltype_rest call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_rest(index, call);
}

void lisp_compiled_dynamic(int index, lisp_calltype_dynamic call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_dynamic(index, call);
}

void lisp_compiled_any(int index, lisp_calltype_any call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_any(index, call);
}

void lisp_compiled_empty(int index, lisp_calltype_empty call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_empty(index, call);
}

void lisp_compiled_var1(int index, lisp_calltype_var1 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var1(index, call);
}

void lisp_compiled_var2(int index, lisp_calltype_var2 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var2(index, call);
}

void lisp_compiled_var3(int index, lisp_calltype_var3 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var3(index, call);
}

void lisp_compiled_var4(int index, lisp_calltype_var4 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var4(index, call);
}

void lisp_compiled_var5(int index, lisp_calltype_var5 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var5(index, call);
}

void lisp_compiled_var6(int index, lisp_calltype_var6 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var6(index, call);
}

void lisp_compiled_opt1(int index, lisp_calltype_opt1 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_opt1(index, call);
}

void lisp_compiled_opt2(int index, lisp_calltype_opt2 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_opt2(index, call);
}

void lisp_compiled_opt3(int index, lisp_calltype_opt3 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_opt3(index, call);
}

void lisp_compiled_var1opt1(int index, lisp_calltype_var1opt1 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var1opt1(index, call);
}

void lisp_compiled_var1opt2(int index, lisp_calltype_var1opt2 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var1opt2(index, call);
}

void lisp_compiled_var1opt3(int index, lisp_calltype_var1opt3 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var1opt3(index, call);
}

void lisp_compiled_var2opt1(int index, lisp_calltype_var2opt1 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var2opt1(index, call);
}

void lisp_compiled_var2opt2(int index, lisp_calltype_var2opt2 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var2opt2(index, call);
}

void lisp_compiled_var2opt3(int index, lisp_calltype_var2opt3 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var2opt3(index, call);
}

void lisp_compiled_var3opt1(int index, lisp_calltype_var3opt1 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var3opt1(index, call);
}

void lisp_compiled_var3opt2(int index, lisp_calltype_var3opt2 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var3opt2(index, call);
}

void lisp_compiled_var3opt3(int index, lisp_calltype_var3opt3 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var3opt3(index, call);
}

void lisp_compiled_var1rest(int index, lisp_calltype_var1rest call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var1rest(index, call);
}

void lisp_compiled_var2rest(int index, lisp_calltype_var2rest call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var2rest(index, call);
}

void lisp_compiled_var3rest(int index, lisp_calltype_var3rest call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var3rest(index, call);
}

void lisp_compiled_var1dynamic(int index, lisp_calltype_var1dynamic call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var1dynamic(index, call);
}

void lisp_compiled_var2dynamic(int index, lisp_calltype_var2dynamic call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var2dynamic(index, call);
}

void lisp_compiled_var3dynamic(int index, lisp_calltype_var3dynamic call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var3dynamic(index, call);
}

static int lisp_compiled_function_set(addr pos, int index)
{
	switch (pointer_table[index].type) {
		case CallBind_extend_macro:
			setcompiled_extend_macro(pos, (pointer)index);
			break;

		case CallBind_extend_rest:
			setcompiled_extend_rest(pos, (pointer)index);
			break;

		case CallBind_extend_dynamic:
			setcompiled_extend_dynamic(pos, (pointer)index);
			break;

		case CallBind_extend_any:
			setcompiled_extend_any(pos, (pointer)index);
			break;

		case CallBind_extend_empty:
			setcompiled_extend_empty(pos, (pointer)index);
			break;

		case CallBind_extend_var1:
			setcompiled_extend_var1(pos, (pointer)index);
			break;

		case CallBind_extend_var2:
			setcompiled_extend_var2(pos, (pointer)index);
			break;

		case CallBind_extend_var3:
			setcompiled_extend_var3(pos, (pointer)index);
			break;

		case CallBind_extend_var4:
			setcompiled_extend_var4(pos, (pointer)index);
			break;

		case CallBind_extend_var5:
			setcompiled_extend_var5(pos, (pointer)index);
			break;

		case CallBind_extend_var6:
			setcompiled_extend_var6(pos, (pointer)index);
			break;

		case CallBind_extend_opt1:
			setcompiled_extend_opt1(pos, (pointer)index);
			break;

		case CallBind_extend_opt2:
			setcompiled_extend_opt2(pos, (pointer)index);
			break;

		case CallBind_extend_opt3:
			setcompiled_extend_opt3(pos, (pointer)index);
			break;

		case CallBind_extend_var1opt1:
			setcompiled_extend_var1opt1(pos, (pointer)index);
			break;

		case CallBind_extend_var1opt2:
			setcompiled_extend_var1opt2(pos, (pointer)index);
			break;

		case CallBind_extend_var1opt3:
			setcompiled_extend_var1opt3(pos, (pointer)index);
			break;

		case CallBind_extend_var2opt1:
			setcompiled_extend_var2opt1(pos, (pointer)index);
			break;

		case CallBind_extend_var2opt2:
			setcompiled_extend_var2opt2(pos, (pointer)index);
			break;

		case CallBind_extend_var2opt3:
			setcompiled_extend_var2opt3(pos, (pointer)index);
			break;

		case CallBind_extend_var3opt1:
			setcompiled_extend_var3opt1(pos, (pointer)index);
			break;

		case CallBind_extend_var3opt2:
			setcompiled_extend_var3opt2(pos, (pointer)index);
			break;

		case CallBind_extend_var3opt3:
			setcompiled_extend_var3opt3(pos, (pointer)index);
			break;

		case CallBind_extend_var1rest:
			setcompiled_extend_var1rest(pos, (pointer)index);
			break;

		case CallBind_extend_var2rest:
			setcompiled_extend_var2rest(pos, (pointer)index);
			break;

		case CallBind_extend_var3rest:
			setcompiled_extend_var3rest(pos, (pointer)index);
			break;

		case CallBind_extend_var1dynamic:
			setcompiled_extend_var1dynamic(pos, (pointer)index);
			break;

		case CallBind_extend_var2dynamic:
			setcompiled_extend_var2dynamic(pos, (pointer)index);
			break;

		case CallBind_extend_var3dynamic:
			setcompiled_extend_var3dynamic(pos, (pointer)index);
			break;

		default:
			return 1;
	}

	return 0;
}

int lisp0_compiled_function_(addr *ret, int index, addr symbol)
{
	addr pos;

	lisp_compiled_index_check(index);
	index += (int)p_size;
	if (symbol == NULL)
		symbol = Nil;
	hold_value(symbol, &symbol);

	if (! symbolp(symbol)) {
		*ret = Nil;
		return TypeError_(symbol, SYMBOL);
	}

	/* function */
	compiled_system(&pos, symbol);
	if (lisp_compiled_function_set(pos, index)) {
		*ret = Nil;
		fixnum_heap(&pos, (fixnum)index);
		return lisp_error8_("Invalid callbind type, index = ~A.", pos, NULL);
	}

	return Result(ret, pos);
}

int lisp0_compiled_function8_(addr *ret, int index, const void *str)
{
	addr pos;
	Return(lisp0_intern8_(&pos, NULL, str));
	return lisp0_compiled_function_(ret, index, pos);
}

int lisp0_compiled_function16_(addr *ret, int index, const void *str)
{
	addr pos;
	Return(lisp0_intern16_(&pos, NULL, str));
	return lisp0_compiled_function_(ret, index, pos);
}

int lisp0_compiled_function32_(addr *ret, int index, const void *str)
{
	addr pos;
	Return(lisp0_intern32_(&pos, NULL, str));
	return lisp0_compiled_function_(ret, index, pos);
}

int lisp_compiled_function_(addr x, int index, addr symbol)
{
	addr pos;
	Return(lisp0_compiled_function_(&pos, index, symbol));
	hold_set(x, pos);
	return 0;
}

int lisp_compiled_function8_(addr x, int index, const void *str)
{
	addr pos;
	Return(lisp0_compiled_function8_(&pos, index, str));
	hold_set(x, pos);
	return 0;
}

int lisp_compiled_function16_(addr x, int index, const void *str)
{
	addr pos;
	Return(lisp0_compiled_function16_(&pos, index, str));
	hold_set(x, pos);
	return 0;
}

int lisp_compiled_function32_(addr x, int index, const void *str)
{
	addr pos;
	Return(lisp0_compiled_function32_(&pos, index, str));
	hold_set(x, pos);
	return 0;
}

int lisp_compiled_defun_(int index, addr symbol)
{
	addr pos;
	hold_value(symbol, &symbol);
	Return(lisp0_compiled_function_(&pos, index, symbol));
	return setfunction_symbol_(symbol, pos);
}

int lisp_compiled_defun8_(int index, const void *str)
{
	addr symbol, pos;

	Return(lisp0_intern8_(&symbol, NULL, str));
	Return(lisp0_compiled_function_(&pos, index, symbol));
	hold_value(symbol, &symbol);
	return setfunction_symbol_(symbol, pos);
}

int lisp_compiled_defun16_(int index, const void *str)
{
	addr symbol, pos;

	Return(lisp0_intern16_(&symbol, NULL, str));
	Return(lisp0_compiled_function_(&pos, index, symbol));
	hold_value(symbol, &symbol);
	return setfunction_symbol_(symbol, pos);
}

int lisp_compiled_defun32_(int index, const void *str)
{
	addr symbol, pos;

	Return(lisp0_intern32_(&symbol, NULL, str));
	Return(lisp0_compiled_function_(&pos, index, symbol));
	hold_value(symbol, &symbol);
	return setfunction_symbol_(symbol, pos);
}

static int lisp0_compiled_function_setf_(addr *ret, addr *rcall, int index, addr call)
{
	addr pos;

	lisp_compiled_index_check(index);
	index += (int)p_size;
	if (call == NULL)
		call = Nil;
	hold_value(call, &call);

	Return(parse_callname_error_(&call, call));
	SetCallNameType(call, CALLNAME_SETF);

	/* function */
	compiled_system(&pos, call);
	if (lisp_compiled_function_set(pos, index)) {
		*ret = *rcall = Nil;
		fixnum_heap(&pos, (fixnum)index);
		return lisp_error8_("Invalid callbind type, index = ~A.", pos, NULL);
	}

	*rcall = call;
	return Result(ret, pos);
}

int lisp_compiled_defun_setf_(int index, addr call)
{
	addr pos;
	Return(lisp0_compiled_function_setf_(&pos, &call, index, call));
	return setglobal_callname_(call, pos);
}

int lisp_compiled_defun_setf8_(int index, const void *str)
{
	addr pos, call;

	Return(lisp0_intern8_(&call, NULL, str));
	Return(lisp0_compiled_function_setf_(&pos, &call, index, call));
	return setglobal_callname_(call, pos);
}

int lisp_compiled_defun_setf16_(int index, const void *str)
{
	addr pos, call;

	Return(lisp0_intern16_(&call, NULL, str));
	Return(lisp0_compiled_function_setf_(&pos, &call, index, call));
	return setglobal_callname_(call, pos);
}

int lisp_compiled_defun_setf32_(int index, const void *str)
{
	addr pos, call;

	Return(lisp0_intern32_(&call, NULL, str));
	Return(lisp0_compiled_function_setf_(&pos, &call, index, call));
	return setglobal_callname_(call, pos);
}

void lisp_compiled_setvalue(addr pos, addr value)
{
	if (value == NULL)
		value = Nil;
	hold_value(pos, &pos);
	hold_value(value, &value);
	if (! functionp(pos)) {
		lisp_abort8("Invalid function object ~S.", pos, NULL);
		return;
	}
	SetDataFunction(pos, value);
}

void lisp_compiled_getvalue(addr *ret)
{
	addr value;

	getdata_control(Execute_Thread, &value);
	if (value == Unbound)
		value = Nil;
	*ret = value;
}


/************************************************************
 *  extern_init.c
 ************************************************************/

int lisp_code = 0;
int lisp_result = 0;
FILE *lisp_stderr = NULL;

FILE *lisperror_stream(void)
{
	return (lisp_stderr == NULL)? stderr: lisp_stderr;
}

int lisperror_noeol(const char *fmt, ...)
{
	int check;
	FILE *file;
	va_list args;

	file = lisperror_stream();
	va_start(args, fmt);
	check = vfprintf(file, fmt, args);
	va_end(args);
	fflush(file);

	return check;
}

int lisperror_va(const char *fmt, va_list args)
{
	int check;
	FILE *file;

	file = lisperror_stream();
	check = vfprintf(file, fmt, args);
	fprintf(file, "\n");
	fflush(file);

	return check;
}

int lisperror(const char *fmt, ...)
{
	int check;
	va_list args;

	va_start(args, fmt);
	check = lisperror_va(fmt, args);
	va_end(args);

	return check;
}

void lisp_init(void)
{
	initlisp();
	lisp_code = 0;
	lisp_result = 1;
	lisp_stderr = NULL;
}

void lisp_free(void)
{
	freelisp();
	lisp_code = 0;
}

int lisp_alloc(size_t heap, size_t local)
{
	if (lisp_code) {
		lisperror("lisp internal error.");
		return 1;
	}
	if (alloclisp(heap, local)) {
		lisperror("alloclisp error.");
		lisp_code = 1;
	}
	lisp_result = 1;

	return lisp_code;
}


/*
 *  help
 */
static const char *lisp_main_help_message[] = {
	Lispname " -- ANSI Common Lisp Programming Language.",
	"",
	"USAGE:",
	"  " Lispname " [options] [inputs] [--] [arguments]",
	"",
	"OPTIONS:",
	"  --help             Print this message.",
	"  --version          Print the version infomation.",
	"  --core             Core mode.",
	"  --standalone       Standalone mode.",
#ifdef LISP_DEGRADE
	"  --degrade          Degrade mode.",
#endif
	"  --heap <size>      Heap memory size.",
	"  --local <size>     Local memory size.",
	"  --corefile <file>  Core file instead of default file used.",
	"  --initfile <file>  Init file instead of default file used.",
	"  --nocore           Don't load a default core file.",
	"  --noinit           Don't load a default init file.",
	"  --debugger         Enable debugger.",
	"  --nodebugger       Disable debugger.",
	"  --quit             Exit after load and eval processing.",
	"",
	"INPUTS:",
	"  --load <file>      Load source file.",
	"  --script <file>    Load script file.",
	"  --eval <cmd>       Execute command.",
	"",
	"If inputs aren't appeared, load from a standard-input.",
	"",
	NULL
};

int lisp_main_help(FILE *file)
{
	int i;
	const char *ptr;

	if (file == NULL) return 1;
	for (i = 0; ; i++) {
		ptr = lisp_main_help_message[i];
		if (ptr == NULL) break;
		fprintf(file, "%s\n", ptr);
	}
	lisp_result = 0;

	return 0;
}


/*
 *  version
 */
int lisp_main_version_text(FILE *file)
{
	if (file == NULL) return 1;
	fprintf(file, Lispname " Version %d.%d.%d\n",
			LISP_VERSION_A, LISP_VERSION_B, LISP_VERSION_C);
	fprintf(file, "-----\n");
	fprintf(file, "%-20s %s\n", "Memory size", LISP_ARCH_MODE);
	fprintf(file, "%-20s %s\n", "Fixnum size", LISP_FIXNUM_MODE);
	fprintf(file, "%-20s %s\n", "Lisp mode", LISP_MODE);
	fprintf(file, "%-20s %s\n", "Thread mode", LISP_THREAD_MODE);
	fprintf(file, "%-20s %d.%d.%d\n", "Version",
			LISP_VERSION_A, LISP_VERSION_B, LISP_VERSION_C);
	fprintf(file, "%-20s %s\n", "Build information", LISP_REVISION);
	fprintf(file, "-----\n");
	fprintf(file, "%-20s %s\n", "Execute mode", LISP_MODE_STRING);
	fprintf(file, "%-20s %s\n", "Release mode", LISP_DEBUG_STRING);
	fprintf(file, "%-20s %s\n", "Degrade mode", LISP_DEGRADE_STRING);
	fprintf(file, "%-20s %s\n", "Prompt mode", LISP_PROMPT_STRING);
#ifdef LISP_DEBUG_MEMORY
	fprintf(file, "%-20s %s\n", "Debug Memory", "true");
#endif
#ifdef LISP_DEBUG_FORCE_GC
	fprintf(file, "%-20s %s\n", "Force GC", "true");
#endif
#ifdef LISP_MEMORY_MALLOC
	fprintf(file, "%-20s %s\n", "Memory Malloc", "true");
#endif
	fprintf(file, "-----\n");

	lisp_result = 0;

	return 0;
}

int lisp_main_version_script(FILE *file)
{
	if (file == NULL) return 1;
	fprintf(file, "name\t" Lispname "\n");
	fprintf(file, "%s\t%s\n", "memory-size", LISP_ARCH_MODE);
	fprintf(file, "%s\t%s\n", "fixnum-size", LISP_FIXNUM_MODE);
	fprintf(file, "%s\t%s\n", "lisp-mode", LISP_MODE);
	fprintf(file, "%s\t%s\n", "thread-mode", LISP_THREAD_MODE);
	fprintf(file, "%s\t%d.%d.%d\n", "version",
			LISP_VERSION_A, LISP_VERSION_B, LISP_VERSION_C);
	fprintf(file, "%s\t%s\n", "build-information", LISP_REVISION);
	fprintf(file, "%s\t%s\n", "execute-mode", LISP_MODE_STRING);
	fprintf(file, "%s\t%s\n", "release-mode", LISP_DEBUG_STRING);
	fprintf(file, "%s\t%s\n", "degrade-mode", LISP_DEGRADE_STRING);
	fprintf(file, "%s\t%s\n", "prompt-mode", LISP_PROMPT_STRING);
#ifdef LISP_DEBUG_MEMORY
	fprintf(file, "%s\t%s\n", "debug-memory", "true");
#else
	fprintf(file, "%s\t%s\n", "debug-memory", "false");
#endif
#ifdef LISP_DEBUG_FORCE_GC
	fprintf(file, "%s\t%s\n", "force-gc", "enable");
#else
	fprintf(file, "%s\t%s\n", "force-gc", "disable");
#endif
#ifdef LISP_AMALGAMATION
	fprintf(file, "%s\t%s\n", "amalgamation", "true");
#else
	fprintf(file, "%s\t%s\n", "amalgamation", "false");
#endif
#ifdef LISP_MEMORY_MALLOC
	fprintf(file, "%s\t%s\n", "memory-malloc", "true");
#else
	fprintf(file, "%s\t%s\n", "memory-malloc", "false");
#endif
	fprintf(file, "%s\t%d\n", "pointer-extend", LISP_POINTER_EXTEND);
#ifdef LISP_TERME
	fprintf(file, "%s\t%s\n", "prompt-bright", LISP_TERME_COLOR1);
	fprintf(file, "%s\t%s\n", "prompt-color", LISP_TERME_COLOR2);
#endif
#ifdef LISP_DYNAMIC_LINK
	fprintf(file, "%s\t%s\n", "dynamic-link", "true");
#else
	fprintf(file, "%s\t%s\n", "dynamic-link", "false");
#endif
	lisp_result = 0;

	return 0;
}

int lisp_main_version(struct lispargv *ptr, FILE *file)
{
	if (ptr->version_script)
		return lisp_main_version_script(stdout);
	else
		return lisp_main_version_text(stdout);
}

int lisp_main_degrade(struct lispargv *ptr)
{
	lisp_result = degradelisp();
	return lisp_result;
}


/*
 *  execute
 */
#define EnvHome "HOME"
#define EnvLispHome (LISPNAME "_HOME")
#define EnvLispUser (LISPNAME "_USER")
#ifdef LISP_WINDOWS_OR_ANSI
#define EnvUserProfile "USERPROFILE"
#define EnvProgramData "ProgramData"
#define EnvProgramFiles "PROGRAMFILES"
#define EnvProgramFilesx86 "ProgramFiles(x86)"
#endif

#ifdef LISP_TERME_WINDOWS
#define LISP_WINDOWS_PATHNAME_LENGTH	1024
#endif

/* load core */
#ifndef LISP_WINDOWS_WIDE
static int lisp_argv_core_load(const char *name)
{
	int check;
	lispstringu file;

	file = char_stringu(name);
	if (file == NULL) {
		lisperror("char_stringu error.");
		return 1;
	}
	check = load_core(file->ptr, file->size);
	free_stringu(file);
	if (0 < check) {
		lisperror("load_core error.");
		return 1;
	}

	return check;
}
#define InitCoreLoad(x) { \
	int __result = lisp_argv_core_load(x); \
	if (__result == 0) { \
		return 0; \
	} \
	if (0 < __result) { \
		lisp_code = 1; \
		return 1; \
	} \
}
#endif

static int lisp_argv_core_env(lisptableu env, const char *key, const char *name)
{
	int check;
	lispstringu value, file;

	/* environment */
	value = findchar_tableu(env, key);
	if (value == NULL)
		return -1;
	/* load */
	file = concatchar_stringu(value, name);
	if (file == NULL) {
		lisperror("concatchar_stringu error.");
		return 1;
	}
	check = load_core(file->ptr, file->size);
	free_stringu(file);
	if (0 < check) {
		lisperror("load_core error.");
		return 1;
	}

	return check;
}
#define InitCoreEnv(p,x,y) { \
	int __result = lisp_argv_core_env((p),(x),(y)); \
	if (__result == 0) { \
		return 0; \
	} \
	if (0 < __result) { \
		lisp_code = 1; \
		return 1; \
	} \
}

#ifdef LISP_TERME_WINDOWS
static int lisp_argv_core_windows(const WCHAR *name)
{
	int check;
	errno_t err;
	WCHAR path[LISP_WINDOWS_PATHNAME_LENGTH];
	WCHAR data1[LISP_WINDOWS_PATHNAME_LENGTH];
	WCHAR data2[LISP_WINDOWS_PATHNAME_LENGTH];
	WCHAR data3[LISP_WINDOWS_PATHNAME_LENGTH];
	DWORD size;
	lispstringu file;

	size = GetModuleFileNameW(NULL, path, LISP_WINDOWS_PATHNAME_LENGTH);
	if (size == LISP_WINDOWS_PATHNAME_LENGTH)
		return -1;
	err = _wsplitpath_s(path,
			data1, LISP_WINDOWS_PATHNAME_LENGTH,
			data2, LISP_WINDOWS_PATHNAME_LENGTH,
			NULL, 0,
			NULL, 0);
	if (err)
		return -1;
	check = swprintf(data3, LISP_WINDOWS_PATHNAME_LENGTH,
			L"%s%s\\%s", data1, data2, name);
	if (check < 0)
		return -1;
	file = wchar_stringu((const byte16 *)data3);
	if (file == NULL)
		return -1;
	check = load_core(file->ptr, file->size);
	free_stringu(file);
	if (0 < check) {
		lisperror("load_core error.");
		return 1;
	}

	return check;
}

#define InitCodeEnvWindows(x) { \
	int __result = lisp_argv_core_windows(x); \
	if (__result == 0) { \
		return 0; \
	} \
	if (0 < __result) { \
		lisp_code = 1; \
		return 1; \
	} \
}
#endif

static int lisp_argv_core_default(struct lispargv *ptr)
{
	lisptableu env;

	env = ptr->env;
#ifdef LISP_TERME_WINDOWS
	InitCodeEnvWindows(LispnameW L".core");
#endif
#ifdef LISP_WINDOWS_WIDE
	InitCoreEnv(env, EnvLispHome, "\\" Lispname ".core");
	InitCoreEnv(env, EnvLispHome, "\\lib\\" Lispname ".core");
	InitCoreEnv(env, EnvUserProfile, "\\" Lispname ".core");
	InitCoreEnv(env, EnvProgramData, "\\" Lispname "\\" Lispname ".core");
	InitCoreEnv(env, EnvProgramFiles, "\\" Lispname "\\" Lispname ".core");
	InitCoreEnv(env, EnvProgramFilesx86, "\\" Lispname "\\" Lispname ".core");
#else
	InitCoreEnv(env, EnvLispHome, "/" Lispname ".core");
	InitCoreEnv(env, EnvLispHome, "/lib/" Lispname ".core");
	InitCoreEnv(env, EnvHome, "/." Lispname "/" Lispname ".core");
	InitCoreLoad("/usr/lib/" Lispname "/" Lispname ".core");
	InitCoreLoad("/usr/local/lib/" Lispname "/" Lispname ".core");
	InitCoreLoad("/opt/" Lispname "/" Lispname ".core");
	InitCoreLoad("/opt/lib/" Lispname "/" Lispname ".core");
#endif

	return 1;
}

static int lisp_argv_initcode(struct lispargv *ptr)
{
	lispstringu file;
	size_t heap, local;

	/* allocate */
	heap = ptr->heap;
	local = ptr->local;
	if (heap == 0)
		heap = LISP_MEMORY_HEAP;
	if (local == 0)
		local = LISP_MEMORY_LOCAL;
	if (alloclisp(heap, local)) {
		lisperror("lisp initialize error.");
		return 1;
	}

	/* --nocore */
	if (ptr->nocore)
		return 0;

	/* --corefile */
	file = ptr->core;
	if (file) {
		if (load_core(file->ptr, file->size)) {
			lisperror("Cannot read corefile.");
			return 1;
		}
		return 0;
	}

	/* default corefile */
	if (lisp_argv_core_default(ptr)) {
		lisperror("Cannot read all default corefiles.");
		return 1;
	}

	return 0;
}

int lisp_argv_init(struct lispargv *ptr)
{
	/* error */
	if (lisp_code) {
		lisp_result = 1;
		return 1;
	}

	/* run */
	if (lisp_argv_initcode(ptr)) {
		lisp_code = lisp_result = 1;
		return 1;
	}

	return 0;
}


/*
 *  lisp_argv_execute
 */
static int lispstringu_heap_(addr *ret, lispstringu str)
{
	return strvect_sizeu_heap_(ret, str->ptr, str->size - 1UL);
}

static int lisp_argv_load_(Execute ptr, lispstringu name, int error, int *ret)
{
	addr file;

	Return(lispstringu_heap_(&file, name));
	Return(pathname_designator_heap_(ptr, file, &file));
	return eval_main_load_(ptr, file, error, ret);
}

static int lisp_argv_script_(Execute ptr, lispstringu name)
{
	addr file, stream;

	/* open */
	Return(lispstringu_heap_(&file, name));
	Return(pathname_designator_heap_(ptr, file, &file));
	Return(open_input_utf8_stream_(ptr, &stream, file));
	if (stream == NULL)
		return fmte_("Cannot open file ~S.", file, NULL);
	script_header(stream);
	/* load */
	return eval_main_load_(ptr, stream, 1, NULL);
}

#ifndef LISP_WINDOWS_WIDE
static int lisp_argv_file_load_(Execute ptr, int *ret, const char *name)
{
	int check;
	lispstringu file;

	file = char_stringu(name);
	if (file == NULL)
		return fmte_("char_stringu error.", NULL);
	check = lisp_argv_load_(ptr, file, 0, ret);
	free_stringu(file);

	return check;
}
#define InitFileLoad(p,a,x) { \
	Return(lisp_argv_file_load_((p),(a),(x))); \
	if (*(a) == 0) return 0; \
}
#endif

static int lisp_argv_file_env_(Execute ptr, lisptableu env, int *ret,
		const char *key, const char *name)
{
	int check;
	lispstringu value, file;

	/* environment */
	value = findchar_tableu(env, key);
	if (value == NULL)
		return Result(ret, 0); /* next */
	/* load */
	file = concatchar_stringu(value, name);
	if (file == NULL)
		return fmte_("concatchar_stringu error.", NULL);
	check = lisp_argv_load_(ptr, file, 0, ret);
	free_stringu(file);

	return check;
}
#define InitFileEnv(p,e,a,x,y) { \
	Return(lisp_argv_file_env_((p),(e),(a),(x),(y))); \
	if (*(a) == 0) return 0; \
}

#ifdef LISP_TERME_WINDOWS
static int lisp_argv_file_windows_(Execute ptr, int *ret, const WCHAR *name)
{
	int check;
	errno_t err;
	WCHAR path[LISP_WINDOWS_PATHNAME_LENGTH];
	WCHAR data1[LISP_WINDOWS_PATHNAME_LENGTH];
	WCHAR data2[LISP_WINDOWS_PATHNAME_LENGTH];
	WCHAR data3[LISP_WINDOWS_PATHNAME_LENGTH];
	DWORD size;
	lispstringu file;

	size = GetModuleFileNameW(NULL, path, LISP_WINDOWS_PATHNAME_LENGTH);
	if (size == LISP_WINDOWS_PATHNAME_LENGTH)
		return -1;
	err = _wsplitpath_s(path,
			data1, LISP_WINDOWS_PATHNAME_LENGTH,
			data2, LISP_WINDOWS_PATHNAME_LENGTH,
			NULL, 0,
			NULL, 0);
	if (err)
		return -1;
	check = swprintf(data3, LISP_WINDOWS_PATHNAME_LENGTH,
			L"%s%s\\%s", data1, data2, name);
	if (check < 0)
		return -1;
	file = wchar_stringu((const byte16 *)data3);
	if (file == NULL)
		return -1;
	check = lisp_argv_load_(ptr, file, 0, ret);
	free_stringu(file);

	return check;
}

#define InitFileWindows(p,a,x) { \
	Return(lisp_argv_file_windows_((p),(a),(x))); \
	if (*(a) == 0) return 0; \
}
#endif

static int lisp_argv_load_default_(Execute ptr, struct lispargv *argv, int *a)
{
	lisptableu env;

	env = argv->env;
#ifdef LISP_TERME_WINDOWS
	InitFileWindows(ptr,a, LispnameW L".lisp");
#endif
#ifdef LISP_WINDOWS_WIDE
	InitFileEnv(ptr,env,a, EnvUserProfile, "\\" Lispname ".lisp");
	InitFileEnv(ptr,env,a, EnvLispHome, "\\" Lispname ".lisp");
	InitFileEnv(ptr,env,a, EnvLispHome, "\\lib\\" Lispname ".lisp");
	InitFileEnv(ptr,env,a, EnvProgramData, "\\" Lispname "\\" Lispname ".lisp");
	InitFileEnv(ptr,env,a, EnvProgramFiles, "\\" Lispname "\\" Lispname ".lisp");
	InitFileEnv(ptr,env,a, EnvProgramFilesx86, "\\" Lispname "\\" Lispname ".lisp");
#else
	InitFileEnv(ptr,env,a, EnvHome, "/." Lispname ".lisp");
	InitFileEnv(ptr,env,a, EnvHome, "/." Lispname "/" Lispname ".lisp");
	InitFileEnv(ptr,env,a, EnvLispHome, "/" Lispname ".lisp");
	InitFileEnv(ptr,env,a, EnvLispHome, "/lib/" Lispname ".lisp");
	InitFileLoad(ptr,a, "/usr/lib/" Lispname "/" Lispname ".lisp");
	InitFileLoad(ptr,a, "/usr/local/lib/" Lispname "/" Lispname ".lisp");
	InitFileLoad(ptr,a, "/opt/" Lispname "/" Lispname ".lisp");
	InitFileLoad(ptr,a, "/opt/lib/" Lispname "/" Lispname ".lisp");
#endif

	return 0;
}

static int lisp_argv_loadinit_call_(Execute ptr, struct lispargv *argv, int *ret)
{
	lispstringu file;

	/* --noinit */
	if (argv->noinit)
		return Result(ret, 0);  /* success */

	/* --initfile */
	file = argv->init;
	if (file)
		return lisp_argv_load_(ptr, file, 1, ret);
	else
		return lisp_argv_load_default_(ptr, argv, ret);
}

static int lisp_argv_loadinit_(Execute ptr, struct lispargv *argv, int *ret)
{
	int value, check;

	/* initialize */
	set_enable_debugger(ptr, 0);
	Return(lisp_argv_loadinit_call_(ptr, argv, &check));

	/* --debugger */
	value = argv->debuggerp? argv->debugger: consolep_file();
	set_enable_debugger(ptr, value);

	return Result(ret, check);
}

static int lisp_argv_eval_(Execute ptr, lispstringu str)
{
	addr pos;
	Return(lispstringu_heap_(&pos, str));
	return eval_main_string_(ptr, pos);
}

static int lisp_argv_inputs_(Execute ptr, struct lispargv *argv)
{
	lispstringu name;
	struct lispargv_string *data;
	size_t i, size;

	data = argv->input->data;
	size = argv->input->size;
	for (i = 0; i < size; i++) {
		name = data[i].value;
		switch (data[i].type) {
			case lispargv_load:
				Return(lisp_argv_load_(ptr, name, 1, NULL));
				continue;

			case lispargv_eval:
				Return(lisp_argv_eval_(ptr, name));
				continue;

			case lispargv_script:
				Return(lisp_argv_script_(ptr, name));
				continue;

			default:
				return fmte_("Invalid input type.", NULL);
		}
	}

	return 0;
}

static int lisp_argv_execute_(Execute ptr, struct lispargv *argv)
{
	int check;

	/* load initialize */
	check = 0;
	Return(lisp_argv_loadinit_(ptr, argv, &check));
	if (check) {
		lisp_result = 1;
		return 0;
	}

	/* load / eval */
	if (argv->input) {
		Return(lisp_argv_inputs_(ptr, argv));
	}

	/* call */
	if (argv->call) {
		check = (argv->call)(argv->call_ptr);
		if (ptr->throw_value != throw_normal)
			return 1;
		if (check) {
			lisp_result = 1;
			return 0;
		}
	}

	/* debugger */
	if (argv->quit == 0)
		return eval_main_loop_toplevel_(ptr);

	return 0;
}


/*
 *  lisp_argv_run
 */
static void lisp_argv_intern(addr table, constindex index)
{
	addr pos;

	GetConstant(index, &pos);
	setspecial_symbol(pos);
	SetValueSymbol(pos, table);
}

static int lisp_argv_environment_(struct lispargv *argv)
{
	enum HASHTABLE_TEST test;
	addr table, key, value, cons;
	lisptableu env;
	lispstringu k, v;
	struct lispkeyvalueu *kv;
	size_t size, i;

#ifdef LISP_WINDOWS
	test = HASHTABLE_TEST_EQUALP;
#else
	test = HASHTABLE_TEST_EQUAL;
#endif

	/* make hashtable */
	env = argv->env;
	if (env == NULL) {
		hashtable_heap(&table);
		settest_hashtable(table, test);
	}
	else {
		kv = env->table;
		size = env->size;
		hashtable_size_heap(&table, env->size);
		settest_hashtable(table, test);

		/* intern hashtable */
		for (i = 0; i < size; i++) {
			k = kv[i].key;
			v = kv[i].value;
			if (k->size == 0 || v->size == 0)
				return fmte_("lisp_argv_environment error.", NULL);
			Return(lispstringu_heap_(&key, k));
			Return(lispstringu_heap_(&value, v));
			Return(intern_hashheap_(table, key, &cons));
			SetCdr(cons, value);
		}
	}
	lisp_argv_intern(table, CONSTANT_SYSTEM_SPECIAL_ENVIRONMENT);

	return 0;
}

static int lisp_argv_arguments_copy_(addr array, size_t i, lispstringu str)
{
	addr pos;

	Return(lispstringu_heap_(&pos, str));
	setarray(array, i, pos);

	return 0;
}

static int lisp_argv_arguments_(struct lispargv *argv)
{
	addr pos;
	lisparrayu array;
	lispstringu *data;
	size_t size, comm, copy, i;

	array = argv->argv;
	data = array->ptr;
	size = array->size;
	comm = argv->start;
	if (size < comm)
		return fmte_("Invalid array size.", NULL);

	else if (size == 0) {
		vector_heap(&pos, 0);
	}
	else {
		copy = size - comm;
		vector_heap(&pos, copy + 1UL);
		Return(lisp_argv_arguments_copy_(pos, 0, data[0]));
		for (i = 0; i < copy; i++) {
			Return(lisp_argv_arguments_copy_(pos, i + 1, data[comm + i]));
		}
	}
	lisp_argv_intern(pos, CONSTANT_SYSTEM_SPECIAL_ARGUMENTS);

	return 0;
}

/* savecore */
static void lisp_argv_makunbound(constindex index)
{
	addr symbol;
	GetConstant(index, &symbol);
	SetValueSymbol(symbol, Unbound);
}

static int lisp_argv_core_(Execute ptr, struct lispargv *argv)
{
	int check;

	lisp_argv_makunbound(CONSTANT_SYSTEM_SPECIAL_ENVIRONMENT);
	lisp_argv_makunbound(CONSTANT_SYSTEM_SPECIAL_ARGUMENTS);
	Return(save_and_load_core_(ptr, argv, &check));
	lisp_result = check;

	return 0;
}

static int lisp_argv_exit_(Execute ptr)
{
	lisp_result = (int)ptr->result;
	return 0;
}

static int lisp_argv_switch_call_(Execute ptr, struct lispargv *argv)
{
	Return(handler_warning_(ptr));
	Return(handler_savecore_(ptr));
	Return(handler_exit_(ptr));
	Return(lisp_argv_environment_(argv));
	Return(lisp_argv_arguments_(argv));
	Return(lisp_argv_execute_(ptr, argv));

	return 0;
}

static int lisp_argv_condition_equal(addr type, addr clos)
{
	if (GetType(type) != LISPTYPE_TYPE)
		return 0;
	if (RefLispDecl(type) != LISPDECL_CLOS)
		return 0;
	GetArrayType(type, 0, &type);
	return type == clos;
}

static int lisp_argv_condition_p_(Execute ptr, constindex index, int *ret)
{
	addr pos, clos;

	if (ptr->throw_value != throw_handler_case)
		return Result(ret, 0);
	pos = ptr->throw_handler;
	GetConstant(index, &clos);
	GetNameHandler(pos, &pos);
	*ret = lisp_argv_condition_equal(pos, clos);
	return 0;
}

static int lisp_argv_savecore_p_(Execute ptr, int *ret)
{
	return lisp_argv_condition_p_(ptr, CONSTANT_CONDITION_SAVECORE, ret);
}

static int lisp_argv_exit_p_(Execute ptr, int *ret)
{
	return lisp_argv_condition_p_(ptr, CONSTANT_CONDITION_EXIT, ret);
}

static int lisp_argv_switch_(Execute ptr, struct lispargv *argv)
{
	int check;
	addr control;

	push_control(ptr, &control);
	if (lisp_argv_switch_call_(ptr, argv) == 0)
		return pop_control_(ptr, control);

	/* savecore */
	Return(lisp_argv_savecore_p_(ptr, &check));
	if (check) {
		normal_throw_control(ptr);
		Return(pop_control_(ptr, control));
		return lisp_argv_core_(ptr, argv);
	}

	/* exit */
	Return(lisp_argv_exit_p_(ptr, &check));
	if (check) {
		normal_throw_control(ptr);
		Return(pop_control_(ptr, control));
		return lisp_argv_exit_(ptr);
	}

	return fmte_("Invalid result.", NULL);
}

static void lisp_argv_terme(struct lispargv *argv)
{
	addr symbol;

	if (argv->terme_bright) {
		GetConst(SYSTEM_PROMPT_BRIGHT, &symbol);
		SetValueSymbol(symbol, T);
	}
	if (argv->terme_dark) {
		GetConst(SYSTEM_PROMPT_BRIGHT, &symbol);
		SetValueSymbol(symbol, Nil);
	}
	if (argv->terme_color) {
		GetConst(SYSTEM_PROMPT_COLOR, &symbol);
		SetValueSymbol(symbol, T);
	}
	if (argv->terme_monochrome) {
		GetConst(SYSTEM_PROMPT_COLOR, &symbol);
		SetValueSymbol(symbol, Nil);
	}
}

/* runcode */
static int lisp_argv_reload(Execute ptr, struct lispargv *argv)
{
	int check;
	lispstringu file;

	file = argv->reload_core;
	if (file == NULL)
		goto reload;
	check = load_core(file->ptr, file->size);
	if (0 < check) {
		lisperror("load_core error.");
		return 1;
	}
	free_stringu(file);

reload:
	argv->reload = 0;
	argv->reload_core = NULL;

	return 0;
}

static int lisp_argv_code(struct lispargv *argv)
{
	Execute ptr;

	ptr = getexecute(0);
	Check(ptr == NULL, "getexecute error.");
	ptr->result = 0;

	/* execute */
	if (argv->nocore) {
		buildlisp(ptr);
	}

execute:
	lisp_argv_terme(argv);
	if (lisp_argv_switch_(ptr, argv))
		goto abort;

	/* load core */
	if (argv->reload) {
		if (reloadlisp())
			goto abort;
		if (lisp_argv_reload(ptr, argv))
			goto abort;
		ptr = getexecute(0);
		Check(ptr == NULL, "getexecute error.");
		ptr->result = 0;
		goto execute;
	}

	/* result */
	lisp_result = ptr->result;
	return 0;

abort:
	abort_execute();
	return 1;
}

int lisp_argv_run(struct lispargv *ptr)
{
	/* error */
	if (lisp_code) {
		lisp_result = 1;
		return 1;
	}

	/* runcode */
	begin_terme();
	if (lisp_argv_code(ptr)) {
		lisp_code = lisp_result = 1;
	}
	end_terme();

	return lisp_code;
}


/************************************************************
 *  extern_instance.c
 ************************************************************/

/*
 *  find-class
 */
void lisp0_find_class(addr *ret, addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol)) {
		*ret = Nil;
		return;
	}
	clos_find_class_nil(symbol, ret);
}

int lisp0_find_class_(addr *ret, addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol)) {
		*ret = Nil;
		return TypeError_(symbol, SYMBOL);
	}
	return clos_find_class_(symbol, ret);
}

int lisp0_find_class8_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern8_(&pos, NULL, str));
	return clos_find_class_(pos, ret);
}

int lisp0_find_class16_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern16_(&pos, NULL, str));
	return clos_find_class_(pos, ret);
}

int lisp0_find_class32_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern32_(&pos, NULL, str));
	return clos_find_class_(pos, ret);
}

void lisp_find_class(addr x, addr symbol)
{
	lisp0_find_class(&symbol, symbol);
	hold_set(x, symbol);
}

int lisp_find_class_(addr x, addr symbol)
{
	Return(lisp0_find_class_(&symbol, symbol));
	hold_set(x, symbol);
	return 0;
}

int lisp_find_class8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_find_class8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_find_class16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_find_class16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_find_class32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_find_class32_(&pos, str));
	hold_set(x, pos);
	return 0;
}


/*
 *  make-instance
 */
int lisp0_instance_(addr *ret, addr clos, ...)
{
	addr list, call;
	va_list va;

	va_start(va, clos);
	lisp0_list_va(&list, va);
	va_end(va);
	hold_value(clos, &clos);
	cons_heap(&list, clos, list);

	GetConst(COMMON_MAKE_INSTANCE, &call);
	GetFunctionSymbol(call, &call);
	return apply1_control_(Execute_Thread, ret, call, list);
}

static int lisp0_instance_call_(addr *ret, const void *clos, va_list va,
		int (*call0)(addr *, const void *))
{
	addr control, list, pos;
	const void *str;

	lisp_push_control(&control);
	list = Lisp_hold();

	/* class */
	if ((*call0)(&pos, clos))
		goto escape;
	lisp_cons(list, pos, NULL);

	/* list */
	for (;;) {
		/* key */
		str = va_arg(va, void *);
		if (str == NULL)
			break;
		if ((*call0)(&pos, str))
			goto escape;
		lisp_cons(list, pos, list);
		/* value */
		pos = va_arg(va, addr);
		if (pos == NULL) {
			lisp_cons(list, Nil, list);
			break;
		}
		hold_value(pos, &pos);
		lisp_cons(list, pos, list);
	}
	if (lisp_nreverse_(list, list))
		goto escape;
	hold_value(list, &list);

	/* make-instance */
	GetConst(COMMON_MAKE_INSTANCE, &pos);
	GetFunctionSymbol(pos, &pos);
	if (apply1_control_(Execute_Thread, ret, pos, list))
		goto escape;
escape:
	return lisp_pop_control_(control);
}

static int lisp0_instance8_call_(addr *ret, const void *clos, va_list va)
{
	return lisp0_instance_call_(ret, clos, va, lisp0_reader8_);
}

static int lisp0_instance16_call_(addr *ret, const void *clos, va_list va)
{
	return lisp0_instance_call_(ret, clos, va, lisp0_reader16_);
}

static int lisp0_instance32_call_(addr *ret, const void *clos, va_list va)
{
	return lisp0_instance_call_(ret, clos, va, lisp0_reader32_);
}

int lisp0_instance8_(addr *ret, const void *clos, ...)
{
	int check;
	va_list va;

	va_start(va, clos);
	check = lisp0_instance8_call_(ret, clos, va);
	va_end(va);

	return check;
}

int lisp0_instance16_(addr *ret, const void *clos, ...)
{
	int check;
	va_list va;

	va_start(va, clos);
	check = lisp0_instance16_call_(ret, clos, va);
	va_end(va);

	return check;
}

int lisp0_instance32_(addr *ret, const void *clos, ...)
{
	int check;
	va_list va;

	va_start(va, clos);
	check = lisp0_instance32_call_(ret, clos, va);
	va_end(va);

	return check;
}

int lisp_instance_(addr x, addr clos, ...)
{
	addr list, call;
	va_list va;

	va_start(va, clos);
	lisp0_list_va(&list, va);
	va_end(va);
	hold_value(clos, &clos);
	cons_heap(&list, clos, list);

	GetConst(COMMON_MAKE_INSTANCE, &call);
	GetFunctionSymbol(call, &call);
	Return(apply1_control_(Execute_Thread, &list, call, list));
	hold_set(x, list);

	return 0;
}

int lisp_instance8_(addr x, const void *clos, ...)
{
	int check;
	addr pos;
	va_list va;

	va_start(va, clos);
	check = lisp0_instance8_call_(&pos, clos, va);
	va_end(va);
	if (check == 0)
		hold_set(x, pos);

	return check;
}

int lisp_instance16_(addr x, const void *clos, ...)
{
	int check;
	addr pos;
	va_list va;

	va_start(va, clos);
	check = lisp0_instance16_call_(&pos, clos, va);
	va_end(va);
	if (check == 0)
		hold_set(x, pos);

	return check;
}

int lisp_instance32_(addr x, const void *clos, ...)
{
	int check;
	addr pos;
	va_list va;

	va_start(va, clos);
	check = lisp0_instance32_call_(&pos, clos, va);
	va_end(va);
	if (check == 0)
		hold_set(x, pos);

	return check;
}


/*
 *  slot-exists-p
 */
int lisp_slot_exists_(addr instance, addr symbol, int *ret)
{
	hold_value(instance, &instance);
	hold_value(symbol, &symbol);
	*ret = clos_slot_exists_p(instance, symbol);
	return 0;
}

int lisp_slot_exists8_(addr instance, const void *str, int *ret)
{
	addr pos;
	Return(lisp0_reader8_(&pos, str));
	return lisp_slot_exists_(instance, pos, ret);
}

int lisp_slot_exists16_(addr instance, const void *str, int *ret)
{
	addr pos;
	Return(lisp0_reader16_(&pos, str));
	return lisp_slot_exists_(instance, pos, ret);
}

int lisp_slot_exists32_(addr instance, const void *str, int *ret)
{
	addr pos;
	Return(lisp0_reader32_(&pos, str));
	return lisp_slot_exists_(instance, pos, ret);
}


/*
 *  slot-boundp
 */
int lisp_slot_boundp_(addr instance, addr symbol, int *ret)
{
	hold_value(instance, &instance);
	hold_value(symbol, &symbol);
	return clos_slot_boundp_(instance, symbol, ret);
}

int lisp_slot_boundp8_(addr instance, const void *str, int *ret)
{
	addr pos;
	Return(lisp0_reader8_(&pos, str));
	return lisp_slot_boundp_(instance, pos, ret);
}

int lisp_slot_boundp16_(addr instance, const void *str, int *ret)
{
	addr pos;
	Return(lisp0_reader16_(&pos, str));
	return lisp_slot_boundp_(instance, pos, ret);
}

int lisp_slot_boundp32_(addr instance, const void *str, int *ret)
{
	addr pos;
	Return(lisp0_reader32_(&pos, str));
	return lisp_slot_boundp_(instance, pos, ret);
}


/*
 *  slot-makunbound
 */
int lisp_slot_makunbound_(addr instance, addr symbol)
{
	hold_value(instance, &instance);
	hold_value(symbol, &symbol);
	return clos_slot_makunbound_(instance, symbol);
}

int lisp_slot_makunbound8_(addr instance, const void *str)
{
	addr pos;
	Return(lisp0_reader8_(&pos, str));
	return lisp_slot_makunbound_(instance, pos);
}

int lisp_slot_makunbound16_(addr instance, const void *str)
{
	addr pos;
	Return(lisp0_reader16_(&pos, str));
	return lisp_slot_makunbound_(instance, pos);
}

int lisp_slot_makunbound32_(addr instance, const void *str)
{
	addr pos;
	Return(lisp0_reader32_(&pos, str));
	return lisp_slot_makunbound_(instance, pos);
}


/*
 *  slot-value
 */
int lisp0_slot_value_(addr *ret, addr instance, addr symbol)
{
	hold_value(instance, &instance);
	hold_value(symbol, &symbol);
	return clos_check_(instance, symbol, ret);
}

int lisp0_slot_value8_(addr *ret, addr instance, const void *str)
{
	addr pos;
	Return(lisp0_reader8_(&pos, str));
	return lisp0_slot_value_(ret, instance, pos);
}

int lisp0_slot_value16_(addr *ret, addr instance, const void *str)
{
	addr pos;
	Return(lisp0_reader16_(&pos, str));
	return lisp0_slot_value_(ret, instance, pos);
}

int lisp0_slot_value32_(addr *ret, addr instance, const void *str)
{
	addr pos;
	Return(lisp0_reader32_(&pos, str));
	return lisp0_slot_value_(ret, instance, pos);
}

int lisp_slot_value_(addr x, addr instance, addr symbol)
{
	Return(lisp0_slot_value_(&instance, instance, symbol));
	hold_set(x, instance);
	return 0;
}

int lisp_slot_value8_(addr x, addr instance, const void *str)
{
	Return(lisp0_slot_value8_(&instance, instance, str));
	hold_set(x, instance);
	return 0;
}

int lisp_slot_value16_(addr x, addr instance, const void *str)
{
	Return(lisp0_slot_value16_(&instance, instance, str));
	hold_set(x, instance);
	return 0;
}

int lisp_slot_value32_(addr x, addr instance, const void *str)
{
	Return(lisp0_slot_value32_(&instance, instance, str));
	hold_set(x, instance);
	return 0;
}


/*
 *  setf slot-value
 */
int lisp_slot_setf_(addr instance, addr symbol, addr value)
{
	hold_value(instance, &instance);
	hold_value(symbol, &symbol);
	hold_value(value, &value);
	return clos_set_(instance, symbol, value);
}

int lisp_slot_setf8_(addr instance, const void *str, addr value)
{
	addr pos;
	Return(lisp0_reader8_(&pos, str));
	return lisp_slot_setf_(instance, pos, value);
}

int lisp_slot_setf16_(addr instance, const void *str, addr value)
{
	addr pos;
	Return(lisp0_reader16_(&pos, str));
	return lisp_slot_setf_(instance, pos, value);
}

int lisp_slot_setf32_(addr instance, const void *str, addr value)
{
	addr pos;
	Return(lisp0_reader32_(&pos, str));
	return lisp_slot_setf_(instance, pos, value);
}


/************************************************************
 *  extern_object.c
 ************************************************************/

/*
 *  object
 */
int lisp0_character_(addr *ret, unicode value)
{
	return character_unicode_heap(ret, value);
}

void lisp0_fixnum(addr *ret, fixnum value)
{
	fixnum_heap(ret, value);
}

int lisp0_float_(addr *ret, float value)
{
	return single_float_check_heap_(ret, value);
}

int lisp0_double_(addr *ret, double value)
{
	return double_float_check_heap_(ret, value);
}

int lisp0_long_double_(addr *ret, long double value)
{
	return long_float_check_heap_(ret, value);
}

int lisp_character_(addr x, unicode value)
{
	addr pos;

	Return(character_unicode_heap(&pos, value));
	hold_set(x, pos);
	return 0;
}

void lisp_fixnum(addr x, fixnum value)
{
	addr pos;
	fixnum_heap(&pos, value);
	hold_set(x, pos);
}

int lisp_float_(addr x, float value)
{
	addr pos;

	Return(single_float_check_heap_(&pos, value));
	hold_set(x, pos);
	return 0;
}

int lisp_double_(addr x, double value)
{
	addr pos;

	Return(double_float_check_heap_(&pos, value));
	hold_set(x, pos);
	return 0;
}

int lisp_long_double_(addr x, long double value)
{
	addr pos;

	Return(long_float_check_heap_(&pos, value));
	hold_set(x, pos);
	return 0;
}

int lisp_zero_p(addr value)
{
	int check;

	hold_value(value, &value);
	if (zerop_numberp(value, &check))
		return 0;

	return check;
}

int lisp_plus_p(addr value)
{
	int check;

	hold_value(value, &value);
	if (plusp_realp(value, &check))
		return 0;

	return check;
}

int lisp_minus_p(addr value)
{
	int check;

	hold_value(value, &value);
	if (! realp(value))
		return 0;
	Error(minusp_real_(value, &check));
	return check;
}

void lisp_get_character(addr pos, unicode *ret)
{
	hold_value(pos, &pos);
	if (! characterp(pos)) {
		*ret = 0;
		Lisp_abort_type(pos, CHARACTER);
		return;
	}
	GetCharacter(pos, ret);
}

void lisp_get_fixnum(addr pos, fixnum *ret)
{
	hold_value(pos, &pos);
	if (! fixnump(pos)) {
		*ret = 0;
		Lisp_abort_type(pos, FIXNUM);
		return;
	}
	GetFixnum(pos, ret);
}

int lisp_get_float_(addr pos, float *ret)
{
	hold_value(pos, &pos);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return cast_ss_value_(pos, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return cast_ds_value_(pos, ret);

		case LISPTYPE_LONG_FLOAT:
			return cast_ls_value_(pos, ret);

		case LISPTYPE_FIXNUM:
			return Result(ret, single_float_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return single_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return single_float_ratio_(pos, ret);

		default:
			*ret = 0.0f;
			return fmte_("The argument ~S must be a real type.", pos, NULL);
	}
}

int lisp_get_double_(addr pos, double *ret)
{
	hold_value(pos, &pos);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return cast_sd_value_(pos, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return cast_dd_value_(pos, ret);

		case LISPTYPE_LONG_FLOAT:
			return cast_ld_value_(pos, ret);

		case LISPTYPE_FIXNUM:
			return Result(ret, double_float_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return double_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return double_float_ratio_(pos, ret);

		default:
			*ret = 0.0;
			return fmte_("The argument ~S must be a real type.", pos, NULL);
	}
}

int lisp_get_long_double_(addr pos, long double *ret)
{
	hold_value(pos, &pos);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return cast_sl_value_(pos, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return cast_dl_value_(pos, ret);

		case LISPTYPE_LONG_FLOAT:
			return cast_ll_value_(pos, ret);

		case LISPTYPE_FIXNUM:
			return Result(ret, long_float_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return long_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return long_float_ratio_(pos, ret);

		default:
			*ret = 0.0L;
			return fmte_("The argument ~S must be a real type.", pos, NULL);
	}
}


/*
 *  package
 */
int lisp0_package_(addr *ret, addr pos)
{
	hold_value(pos, &pos);
	return find_package_(pos, ret);
}

int lisp0_package8_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string8_null_local_(local, &x, (const char *)str));
	Return(find_package_(x, ret));
	rollback_local(local, stack);

	return 0;
}

int lisp0_package16_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string16_null_local_(local, &x, (const byte16 *)str));
	Return(find_package_(x, ret));
	rollback_local(local, stack);

	return 0;
}

int lisp0_package32_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string32_null_local_(local, &x, (const unicode *)str));
	Return(find_package_(x, ret));
	rollback_local(local, stack);

	return 0;
}

int lisp_package_(addr x, addr pos)
{
	Return(lisp0_package_(&pos, pos));
	hold_set(x, pos);
	return 0;
}

int lisp_package8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_package8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_package16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_package16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_package32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_package32_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_in_package_(addr value)
{
	addr symbol;

	GetConst(SPECIAL_PACKAGE, &symbol);
	value = holdv(value);
	if (! packagep(value))
		return fmte_("Value ~S must be a package object.", value, NULL);
	setspecial_local(Execute_Thread, symbol, value);

	return 0;
}

int lisp_in_package8_(const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x, value;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string8_null_local_(local, &x, (const char *)str));
	Return(find_package_(x, &value));
	Return(lisp_in_package_(value));
	rollback_local(local, stack);

	return 0;
}

int lisp_in_package16_(const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x, value;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string16_null_local_(local, &x, (const byte16 *)str));
	Return(find_package_(x, &value));
	Return(lisp_in_package_(value));
	rollback_local(local, stack);

	return 0;
}

int lisp_in_package32_(const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x, value;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string32_null_local_(local, &x, (const unicode *)str));
	Return(find_package_(x, &value));
	Return(lisp_in_package_(value));
	rollback_local(local, stack);

	return 0;
}

int lisp_push_and_in_package_(addr value)
{
	addr symbol;

	GetConst(SPECIAL_PACKAGE, &symbol);
	value = holdv(value);
	if (! packagep(value))
		return fmte_("Value ~S must be a package object.", value, NULL);
	pushspecial_control(Execute_Thread, symbol, value);

	return 0;
}

int lisp_push_and_in_package8_(const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x, value;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string8_null_local_(local, &x, (const char *)str));
	Return(find_package_(x, &value));
	rollback_local(local, stack);
	Return(lisp_push_and_in_package_(value));

	return 0;
}

int lisp_push_and_in_package16_(const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x, value;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string16_null_local_(local, &x, (const byte16 *)str));
	Return(find_package_(x, &value));
	rollback_local(local, stack);
	Return(lisp_push_and_in_package_(value));

	return 0;
}

int lisp_push_and_in_package32_(const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x, value;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string32_null_local_(local, &x, (const unicode *)str));
	Return(find_package_(x, &value));
	rollback_local(local, stack);
	Return(lisp_push_and_in_package_(value));

	return 0;
}


/*
 *  intern
 */
int lisp0_intern_(addr *ret, addr package, addr name)
{
	hold_value(package, &package);
	hold_value(name, &name);
	if (package == Nil || package == NULL) {
		Return(getpackage_(Execute_Thread, &package));
	}
	else if (! packagep(package)) {
		Return(lisp0_package_(&package, package));
	}

	return intern_package_(package, name, ret, NULL);
}

int lisp0_intern8_(addr *ret, const void *package, const void *name)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	if (package == NULL) {
		x = Nil;
	}
	else {
		Return(string8_null_local_(local, &x, (const char *)package));
	}
	Return(string8_null_heap_(&y, (const char *)name));
	Return(lisp0_intern_(ret, x, y));
	rollback_local(local, stack);

	return 0;
}

int lisp0_intern16_(addr *ret, const void *package, const void *name)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	if (package == NULL) {
		x = Nil;
	}
	else {
		Return(string16_null_local_(local, &x, (const byte16 *)package));
	}
	Return(string16_null_heap_(&y, (const byte16 *)name));
	Return(lisp0_intern_(ret, x, y));
	rollback_local(local, stack);

	return 0;
}

int lisp0_intern32_(addr *ret, const void *package, const void *name)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	if (package == NULL) {
		x = Nil;
	}
	else {
		Return(string32_null_local_(local, &x, (const unicode *)package));
	}
	Return(string32_null_heap_(&y, (const unicode *)name));
	Return(lisp0_intern_(ret, x, y));
	rollback_local(local, stack);

	return 0;
}

int lisp_intern_(addr x, addr package, addr name)
{
	addr pos;

	hold_value(package, &package);
	hold_value(name, &name);
	Return(lisp0_intern_(&pos, package, name));
	hold_set(x, pos);
	return 0;
}

int lisp_intern8_(addr x, const void *package, const void *name)
{
	addr pos;

	Return(lisp0_intern8_(&pos, package, name));
	hold_set(x, pos);
	return 0;
}

int lisp_intern16_(addr x, const void *package, const void *name)
{
	addr pos;

	Return(lisp0_intern16_(&pos, package, name));
	hold_set(x, pos);
	return 0;
}

int lisp_intern32_(addr x, const void *package, const void *name)
{
	addr pos;

	Return(lisp0_intern32_(&pos, package, name));
	hold_set(x, pos);
	return 0;
}


/*
 *  reader
 */
int lisp0_reader_(addr *ret, addr str)
{
	int check;
	addr value;

	hold_value(str, &str);
	Return(read_from_string_(Execute_Thread, &check, &value, str));
	return Result(ret, check? NULL: value);
}

int lisp0_reader8_(addr *ret, const void *str)
{
	addr pos;
	Return(string8_null_heap_(&pos, (const char *)str));
	return lisp0_reader_(ret, pos);
}

int lisp0_reader16_(addr *ret, const void *str)
{
	addr pos;
	Return(string16_null_heap_(&pos, (const byte16 *)str));
	return lisp0_reader_(ret, pos);
}

int lisp0_reader32_(addr *ret, const void *str)
{
	addr pos;
	Return(string32_null_heap_(&pos, (const unicode *)str));
	return lisp0_reader_(ret, pos);
}

int lisp_reader_(addr x, addr str)
{
	Return(lisp0_reader_(&str, str));
	hold_set(x, str);
	return 0;
}

int lisp_reader8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_reader8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_reader16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_reader16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_reader32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_reader32_(&pos, str));
	hold_set(x, pos);
	return 0;
}


/*
 *  pathname
 */
int lisp0_pathname_(addr *ret, addr name)
{
	hold_value(name, &name);
	return pathname_designator_heap_(Execute_Thread, name, ret);
}

int lisp0_pathname8_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string8_null_local_(local, &x, (const char *)str));
	Return(lisp0_pathname_(ret, x));
	rollback_local(local, stack);

	return 0;
}

int lisp0_pathname16_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string16_null_local_(local, &x, (const byte16 *)str));
	Return(lisp0_pathname_(ret, x));
	rollback_local(local, stack);

	return 0;
}

int lisp0_pathname32_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string32_null_local_(local, &x, (const unicode *)str));
	Return(lisp0_pathname_(ret, x));
	rollback_local(local, stack);

	return 0;
}

int lisp0_namestring_(addr *ret, addr path)
{
	hold_value(path, &path);
	return namestring_common_(Execute_Thread, ret, path);
}

int lisp_pathname_(addr x, addr name)
{
	Return(lisp0_pathname_(&name, name));
	hold_set(x, name);
	return 0;
}

int lisp_pathname8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_pathname8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_pathname16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_pathname16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_pathname32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_pathname32_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_namestring_(addr x, addr path)
{
	Return(lisp0_namestring_(&path, path));
	hold_set(x, path);
	return 0;
}


/*
 *  paper
 */
int lisp0_paper_(addr *ret, size_t array, size_t body)
{
	return paper_arraybody_heap_(ret, array, body);
}

int lisp_paper_(addr x, size_t array, size_t body)
{
	addr pos;

	Return(lisp0_paper_(&pos, array, body));
	hold_set(x, pos);
	return 0;
}

int lisp_paper_gettype_(addr x, byte *ret)
{
	hold_value(x, &x);
	if (! paperp(x)) {
		*ret = 0;
		return fmte_("Not paper object, ~S.", x, NULL);
	}
	paper_get_type(x, ret);

	return 0;
}

int lisp_paper_settype_(addr x, byte value)
{
	hold_value(x, &x);
	if (! paperp(x))
		return fmte_("Not paper object, ~S.", x, NULL);
	paper_set_type(x, value);

	return 0;
}

int lisp_paper_lenarray_(addr x, size_t *ret)
{
	hold_value(x, &x);
	if (! paperp(x)) {
		*ret = 0;
		return fmte_("Not paper object, ~S.", x, NULL);
	}
	paper_len_array(x, ret);

	return 0;
}

int lisp_paper_lenbody_(addr x, size_t *ret)
{
	hold_value(x, &x);
	if (! paperp(x)) {
		*ret = 0;
		return fmte_("Not paper object, ~S.", x, NULL);
	}
	paper_len_body(x, ret);

	return 0;
}

int lisp0_paper_getarray_(addr *ret, addr pos, size_t index)
{
	size_t size;

	hold_value(pos, &pos);
	if (! paperp(pos)) {
		*ret = Nil;
		return fmte_("Not paper object, ~S.", pos, NULL);
	}
	paper_len_array(pos, &size);
	if (size <= index) {
		*ret = Nil;
		return fmte_("paper size error, ~S.", pos, NULL);
	}
	paper_get_array(pos, index, ret);

	return 0;
}

int lisp_paper_getarray_(addr x, addr pos, size_t index)
{
	addr value;

	Return(lisp0_paper_getarray_(&value, pos, index));
	hold_set(x, value);
	return 0;
}

int lisp_paper_setarray_(addr x, size_t index, addr value)
{
	size_t size;

	hold_value(x, &x);
	if (! paperp(x))
		return fmte_("Not paper object, ~S.", x, NULL);
	paper_len_array(x, &size);
	if (size <= index)
		return fmte_("paper size error, ~S.", x, NULL);
	hold_value(value, &value);
	paper_set_array(x, index, value);

	return 0;
}

int lisp_paper_getbody_(addr x, size_t index, byte *ret)
{
	size_t size;

	hold_value(x, &x);
	if (! paperp(x)) {
		*ret = 0;
		return fmte_("Not paper object, ~S.", x, NULL);
	}
	paper_len_body(x, &size);
	if (size <= index) {
		*ret = 0;
		return fmte_("paper size error, ~S.", x, NULL);
	}
	paper_get_body(x, index, ret);

	return 0;
}

int lisp_paper_setbody_(addr x, size_t index, byte value)
{
	size_t size;

	hold_value(x, &x);
	if (! paperp(x))
		return fmte_("Not paper object, ~S.", x, NULL);
	paper_len_body(x, &size);
	if (size <= index)
		return fmte_("paper size error, ~S.", x, NULL);
	paper_set_body(x, index, value);

	return 0;
}

int lisp_paper_getmemory_(addr x, size_t a, size_t b, void *output, size_t *ret)
{
	hold_value(x, &x);
	if (! paperp(x))
		return fmte_("Not paper object, ~S.", x, NULL);
	paper_get_memory(x, a, b, output, ret);

	return 0;
}

int lisp_paper_setmemory_(addr x, size_t a, size_t b, const void *input, size_t *ret)
{
	hold_value(x, &x);
	if (! paperp(x))
		return fmte_("Not paper object, ~S.", x, NULL);
	paper_set_memory(x, a, b, input, ret);

	return 0;
}

int lisp_paper_body_unsafe_(addr x, byte **ptr, size_t *ret)
{
	size_t size;

	hold_value(x, &x);
	if (! paperp(x)) {
		if (ptr)
			*ptr = NULL;
		if (ret)
			*ret = 0;
		return fmte_("Not paper object, ~S.", x, NULL);
	}

	paper_len_body(x, &size);
	if (size == 0) {
		if (ptr)
			*ptr = NULL;
		if (ret)
			*ret = 0;
		return 0;
	}

	if (ptr) {
		paper_ptr_body_unsafe(x, (void **)&x);
		*ptr = (byte *)x;
	}
	if (ret)
		*ret = size;

	return 0;
}


/************************************************************
 *  extern_print.c
 ************************************************************/

/*
 *  format
 */
static int lisp_format_call_(addr stream, addr format, addr args)
{
	addr control;
	Execute ptr;
	LocalHold hold;

	if (stream == NULL)
		stream = T;
	ptr = Execute_Thread;
	lisp_push_control(&control);
	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, stream, format, args, NULL);
	Return(format_lisp_(ptr, stream, format, args, &args));
	localhold_end(hold);

	return lisp_pop_control_(control);
}

int lisp_format8_(addr stream, const void *str, ...)
{
	addr format, args;
	va_list va;

	hold_value(stream, &stream);
	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(stream, format, args);
}

int lisp_format16_(addr stream, const void *str, ...)
{
	addr format, args;
	va_list va;

	hold_value(stream, &stream);
	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(stream, format, args);
}

int lisp_format32_(addr stream, const void *str, ...)
{
	addr format, args;
	va_list va;

	hold_value(stream, &stream);
	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(stream, format, args);
}


/*
 *  stdout
 */
int lisp_stdout8_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(NULL, format, args);
}

int lisp_stdout16_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(NULL, format, args);
}

int lisp_stdout32_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(NULL, format, args);
}


/*
 *  stderr
 */
int lisp_stderr8_(const void *str, ...)
{
	addr format, args, stream;
	va_list va;

	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	Return(error_output_stream_(Execute_Thread, &stream));
	return lisp_format_call_(stream, format, args);
}

int lisp_stderr16_(const void *str, ...)
{
	addr format, args, stream;
	va_list va;

	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	Return(error_output_stream_(Execute_Thread, &stream));
	return lisp_format_call_(NULL, format, args);
}

int lisp_stderr32_(const void *str, ...)
{
	addr format, args, stream;
	va_list va;

	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	Return(error_output_stream_(Execute_Thread, &stream));
	return lisp_format_call_(NULL, format, args);
}


/*
 *  stringf
 */
static int lisp_format_string_call_(addr format, addr args, addr *ret)
{
	addr control;
	Execute ptr;
	LocalHold hold;

	ptr = Execute_Thread;
	lisp_push_control(&control);
	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, format, args, NULL);
	Return(format_lisp_(ptr, Nil, format, args, ret));
	localhold_end(hold);

	return lisp_pop_control_(control);
}

int lisp0_stringf8_(addr *ret, const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_string_call_(format, args, ret);
}

int lisp0_stringf16_(addr *ret, const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_string_call_(format, args, ret);
}

int lisp0_stringf32_(addr *ret, const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_string_call_(format, args, ret);
}

int lisp_stringf8_(addr x, const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	Return(lisp_format_string_call_(format, args, &args));
	hold_set(x, args);
	return 0;
}

int lisp_stringf16_(addr x, const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	Return(lisp_format_string_call_(format, args, &args));
	hold_set(x, args);
	return 0;
}

int lisp_stringf32_(addr x, const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	Return(lisp_format_string_call_(format, args, &args));
	hold_set(x, args);
	return 0;
}


/************************************************************
 *  extern_sequence.c
 ************************************************************/

/*
 *  sequence
 */
void lisp0_cons(addr *ret, addr car, addr cdr)
{
	/* car */
	if (car == NULL)
		car = Nil;
	else
		hold_value(car, &car);
	/* cdr */
	if (cdr == NULL)
		cdr = Nil;
	else
		hold_value(cdr, &cdr);
	/* cons */
	cons_heap(ret, car, cdr);
}

void lisp_cons(addr x, addr car, addr cdr)
{
	lisp0_cons(&car, car, cdr);
	hold_set(x, car);
}

void lisp0_vector(addr *ret, size_t size)
{
	vector_heap(ret, size);
}

void lisp_vector(addr x, size_t size)
{
	addr pos;
	vector_heap(&pos, size);
	hold_set(x, pos);
}

void lisp0_list_va(addr *ret, va_list args)
{
	lisp0_list_va_alloc(NULL, ret, args);
}

void lisp0_lista_va(addr *ret, va_list args)
{
	lisp0_lista_va_alloc(NULL, ret, args);
}

void lisp0_list(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	lisp0_list_va(ret, args);
	va_end(args);
}

void lisp_list(addr x, ...)
{
	addr pos;
	va_list args;

	va_start(args, x);
	lisp0_list_va(&pos, args);
	va_end(args);

	hold_set(x, pos);
}

void lisp0_lista(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	lisp0_lista_va(ret, args);
	va_end(args);
}

void lisp_lista(addr x, ...)
{
	addr pos;
	va_list args;

	va_start(args, x);
	lisp0_lista_va(&pos, args);
	va_end(args);

	hold_set(x, pos);
}

int lisp0_getelt_(addr *ret, addr pos, size_t index)
{
	hold_value(pos, &pos);
	return getelt_sequence_(NULL, pos, index, ret);
}

int lisp_getelt_(addr x, addr pos, size_t index)
{
	Return(lisp0_getelt_(&pos, pos, index));
	hold_set(x, pos);
	return 0;
}

int lisp_setelt_(addr pos, size_t index, addr value)
{
	hold_value(pos, &pos);
	hold_value(value, &value);
	return setelt_sequence_(pos, index, value);
}

int lisp_length_(addr pos, size_t *ret)
{
	hold_value(pos, &pos);
	return length_sequence_(pos, 1, ret);
}

int lisp0_reverse_(addr *ret, addr pos)
{
	hold_value(pos, &pos);
	return reverse_sequence_heap_(ret, pos);
}

int lisp0_nreverse_(addr *ret, addr pos)
{
	hold_value(pos, &pos);
	return nreverse_sequence_(ret, pos);
}

int lisp_reverse_(addr x, addr pos)
{
	Return(lisp0_reverse_(&pos, pos));
	hold_set(x, pos);
	return 0;
}

int lisp_nreverse_(addr x, addr pos)
{
	Return(lisp0_nreverse_(&pos, pos));
	hold_set(x, pos);
	return 0;
}


/*
 *  cons
 */
void lisp0_car(addr *ret, addr list)
{
	hold_value(list, &list);
	Check(! listp(list), "type error");
	GetCar(list, ret);
}

void lisp0_cdr(addr *ret, addr list)
{
	hold_value(list, &list);
	Check(! listp(list), "type error");
	GetCdr(list, ret);
}

void lisp0_carcdr(addr *car, addr *cdr, addr list)
{
	hold_value(list, &list);
	Check(! listp(list), "type error");
	GetCons(list, car, cdr);
}

void lisp_car(addr x, addr list)
{
	hold_value(list, &list);
	if (! listp(list)) {
		Lisp_abort_type(list, LIST);
		return;
	}
	GetCar(list, &list);
	hold_set(x, list);
}

void lisp_cdr(addr x, addr list)
{
	hold_value(list, &list);
	if (! listp(list)) {
		Lisp_abort_type(list, LIST);
		return;
	}
	GetCdr(list, &list);
	hold_set(x, list);
}

void lisp_carcdr(addr x, addr y, addr list)
{
	addr car, cdr;

	hold_value(list, &list);
	if (! listp(list)) {
		Lisp_abort_type(list, LIST);
		return;
	}
	GetCons(list, &car, &cdr);
	hold_set(x, car);
	hold_set(y, cdr);
}

void lisp_setf_car(addr cons, addr value)
{
	hold_value(cons, &cons);
	if (! consp(cons)) {
		Lisp_abort_type(cons, CONS);
		return;
	}
	hold_value(value, &value);
	SetCar(cons, value);
}

void lisp_setf_cdr(addr cons, addr value)
{
	hold_value(cons, &cons);
	if (! consp(cons)) {
		Lisp_abort_type(cons, CONS);
		return;
	}
	hold_value(value, &value);
	SetCdr(cons, value);
}

void lisp_setf_carcdr(addr cons, addr car, addr cdr)
{
	hold_value(cons, &cons);
	if (! consp(cons)) {
		Lisp_abort_type(cons, CONS);
		return;
	}
	hold_value(car, &car);
	hold_value(cdr, &cdr);
	SetCons(cons, car, cdr);
}


/*
 *  string
 */
int lisp0_string8_(addr *ret, const void *str)
{
	return string8_null_heap_(ret, (const char *)str);
}

int lisp0_string16_(addr *ret, const void *str)
{
	return string16_null_heap_(ret, (const byte16 *)str);
}

int lisp0_string32_(addr *ret, const void *str)
{
	return string32_null_heap_(ret, (const unicode *)str);
}

int lisp_string8_(addr x, const void *str)
{
	addr pos;

	Return(string8_null_heap_(&pos, (const char *)str));
	hold_set(x, pos);
	return 0;
}

int lisp_string16_(addr x, const void *str)
{
	addr pos;

	Return(string16_null_heap_(&pos, (const byte16 *)str));
	hold_set(x, pos);
	return 0;
}

int lisp_string32_(addr x, const void *str)
{
	addr pos;

	Return(string32_null_heap_(&pos, (const unicode *)str));
	hold_set(x, pos);
	return 0;
}

int lisp_string_getc_(addr pos, size_t i, unicode *c)
{
	hold_value(pos, &pos);
	return string_getc_(pos, i, c);
}


/*
 *  strvect
 */
int lisp_strvect_getc(addr pos, size_t i, unicode *c)
{
	const unicode *body;
	size_t size;

	hold_value(pos, &pos);
	if (! strvectp(pos))
		return -1; /* type error */
	strvect_posbodylen(pos, &body, &size);
	if (size <= i)
		return 1; /* size error */
	*c = body[i];
	return 0;
}

int lisp_strvect_length(addr pos, size_t *ret)
{
	hold_value(pos, &pos);
	if (! strvectp(pos))
		return -1; /* type error */
	GetStringSize(pos, ret);
	return 0;
}


/************************************************************
 *  extern_stream.c
 ************************************************************/

/*
 *  stream object
 */
static void lisp_stream_exnted_check(int index)
{
	addr pos;

	if (index < 0 || LISP_POINTER_EXTEND <= index) {
		fixnum_heap(&pos, (fixnum)index);
		lisp_abortf("Invalid index value %d.", index, NULL);
	}
}

void lisp0_stream_define(addr *ret, int index, size_t size)
{
	addr pos;

	lisp_stream_exnted_check(index);
	if (0xFFFF <= size) {
		*ret = Nil;
		lisp_abortf("Too large stream size %zu.", size);
		return;
	}
	stream_heap(&pos, (enum StreamType)StreamExtend_Index(index), size);
	PtrStructStream(pos)->closed = 0;
	*ret = pos;
}

void lisp_stream_define(addr x, int index, size_t size)
{
	addr pos;
	lisp0_stream_define(&pos, index, size);
	hold_set(x, pos);
}

void lisp_stream_memory(addr stream, void **ret)
{
	hold_value(stream, &stream);
	if (! extend_stream_p(stream)) {
		*ret = NULL;
		lisp_abortf("Invalid stream type.");
		return;
	}
	*ret = PtrDataStream(stream);
}

void lisp0_getinfo_stream(addr *ret, addr stream)
{
	hold_value(stream, &stream);
	if (! extend_stream_p(stream)) {
		*ret = Nil;
		lisp_abortf("Invalid stream type.");
		return;
	}
	GetInfoStream(stream, ret);
}

void lisp_getinfo_stream(addr x, addr stream)
{
	lisp0_getinfo_stream(&stream, stream);
	hold_set(x, stream);
}

void lisp_setinfo_stream(addr stream, addr value)
{
	hold_value(stream, &stream);
	if (! extend_stream_p(stream)) {
		lisp_abortf("Invalid stream type.");
		return;
	}
	hold_value(value, &value);
	SetInfoStream(stream, value);
}


/*
 *  extend stream
 */
#define LispStreamExtendCallType(name) \
	void lisp_stream_calltype_##name(int index, lisp_streamtype_##name call) { \
		lisp_stream_exnted_check(index); \
		Stream_##name[StreamExtend_Index(index)] = call; \
	}
LispStreamExtendCallType(close);
LispStreamExtendCallType(read_byte);
LispStreamExtendCallType(unread_byte);
LispStreamExtendCallType(write_byte);
LispStreamExtendCallType(read_char);
LispStreamExtendCallType(read_hang);
LispStreamExtendCallType(unread_char);
LispStreamExtendCallType(write_char);
LispStreamExtendCallType(getleft);
LispStreamExtendCallType(setleft);
LispStreamExtendCallType(inputp);
LispStreamExtendCallType(outputp);
LispStreamExtendCallType(interactivep);
LispStreamExtendCallType(characterp);
LispStreamExtendCallType(binaryp);
LispStreamExtendCallType(element_type);
LispStreamExtendCallType(external_format);
LispStreamExtendCallType(file_length);
LispStreamExtendCallType(file_position);
LispStreamExtendCallType(file_position_start);
LispStreamExtendCallType(file_position_end);
LispStreamExtendCallType(file_position_set);
LispStreamExtendCallType(file_charlen);
LispStreamExtendCallType(file_strlen);
LispStreamExtendCallType(listen);
LispStreamExtendCallType(clear_input);
LispStreamExtendCallType(finish_output);
LispStreamExtendCallType(force_output);
LispStreamExtendCallType(clear_output);
LispStreamExtendCallType(exitpoint);
LispStreamExtendCallType(termsize);

#define LispStreamExtendCallTypeError(name) \
	void lisp_stream_calltype_error_##name(int index) { \
		lisp_stream_exnted_check(index); \
		Stream_##name[StreamExtend_Index(index)] = name##_stream_error; \
	}
LispStreamExtendCallTypeError(close);
LispStreamExtendCallTypeError(read_byte);
LispStreamExtendCallTypeError(unread_byte);
LispStreamExtendCallTypeError(write_byte);
LispStreamExtendCallTypeError(read_char);
LispStreamExtendCallTypeError(read_hang);
LispStreamExtendCallTypeError(unread_char);
LispStreamExtendCallTypeError(write_char);
LispStreamExtendCallTypeError(getleft);
LispStreamExtendCallTypeError(setleft);
LispStreamExtendCallTypeError(inputp);
LispStreamExtendCallTypeError(outputp);
LispStreamExtendCallTypeError(interactivep);
LispStreamExtendCallTypeError(characterp);
LispStreamExtendCallTypeError(binaryp);
LispStreamExtendCallTypeError(element_type);
LispStreamExtendCallTypeError(external_format);
LispStreamExtendCallTypeError(file_length);
LispStreamExtendCallTypeError(file_position);
LispStreamExtendCallTypeError(file_position_start);
LispStreamExtendCallTypeError(file_position_end);
LispStreamExtendCallTypeError(file_position_set);
LispStreamExtendCallTypeError(file_charlen);
LispStreamExtendCallTypeError(file_strlen);
LispStreamExtendCallTypeError(listen);
LispStreamExtendCallTypeError(clear_input);
LispStreamExtendCallTypeError(finish_output);
LispStreamExtendCallTypeError(force_output);
LispStreamExtendCallTypeError(clear_output);
LispStreamExtendCallTypeError(exitpoint);
LispStreamExtendCallTypeError(termsize);


/************************************************************
 *  extern_type.c
 ************************************************************/

/*
 *  hold
 */
int lisp_hold_p(addr x)
{
	return holdp(x);
}

void lisp_hold_value(addr x, addr *ret)
{
	hold_value(x, ret);
}

void lisp_hold_set(addr x, addr value)
{
	hold_set(x, value);
}

addr Lisp_holdv(addr x)
{
	return holdv(x);
}

void lisp_hold(addr *ret, addr value)
{
	Hold_local(ret, value);
}

addr Lisp_hold(void)
{
	addr x;
	Hold_local(&x, Nil);
	return x;
}


/*
 *  nil, t
 */
void lisp0_nil(addr *ret)
{
	*ret = Nil;
}

void lisp0_t(addr *ret)
{
	*ret = T;
}

void lisp_nil(addr x)
{
	hold_set(x, Nil);
}

void lisp_t(addr x)
{
	hold_set(x, T);
}

addr Lisp_nil(void)
{
	return Nil;
}

addr Lisp_t(void)
{
	return T;
}


/*
 *  type
 */
static int lisp_type_p(addr x, enum LISPTYPE type)
{
	hold_value(x, &x);
	return GetType(x) == type;
}

int lisp_nil_p(addr x)
{
	hold_value(x, &x);
	return x == Nil;
}

int lisp_t_p(addr x)
{
	hold_value(x, &x);
	return x == T;
}

int lisp_null_p(addr x)
{
	hold_value(x, &x);
	return x == NULL;
}

int lisp_character_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_CHARACTER);
}

int lisp_cons_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_CONS);
}

int lisp_list_p(addr x)
{
	hold_value(x, &x);
	return listp(x);
}

int lisp_string_p(addr x)
{
	hold_value(x, &x);
	return stringp(x);
}

int lisp_strvect_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_STRING);
}

int lisp_symbol_p(addr x)
{
	hold_value(x, &x);
	return symbolp(x);
}

int lisp_array_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_ARRAY);
}

int lisp_vector_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_VECTOR);
}

int lisp_fixnum_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_FIXNUM);
}

int lisp_bignum_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_BIGNUM);
}

int lisp_integer_p(addr x)
{
	hold_value(x, &x);
	return integerp(x);
}

int lisp_ratio_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_RATIO);
}

int lisp_rational_p(addr x)
{
	hold_value(x, &x);
	return rationalp(x);
}

int lisp_single_float_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_SINGLE_FLOAT);
}

int lisp_double_float_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_DOUBLE_FLOAT);
}

int lisp_long_float_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_LONG_FLOAT);
}

int lisp_float_p(addr x)
{
	hold_value(x, &x);
	return floatp(x);
}

int lisp_real_p(addr x)
{
	hold_value(x, &x);
	return realp(x);
}

int lisp_complex_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_COMPLEX);
}

int lisp_number_p(addr x)
{
	hold_value(x, &x);
	return numberp(x);
}

int lisp_clos_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_CLOS);
}

int lisp_hashtable_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_HASHTABLE);
}

int lisp_readtable_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_READTABLE);
}

int lisp_control_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_CONTROL);
}

int lisp_callname_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_CALLNAME);
}

int lisp_function_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_FUNCTION);
}

int lisp_package_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_PACKAGE);
}

int lisp_random_state_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_RANDOM_STATE);
}

int lisp_pathname_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_PATHNAME);
}

int lisp_stream_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_STREAM);
}

int lisp_restart_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_RESTART);
}

int lisp_environment_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_ENVIRONMENT);
}

int lisp_bitvector_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_BITVECTOR);
}

int lisp_print_dispatch_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_PRINT_DISPATCH);
}

int lisp_paper_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_PAPER);
}


/************************************************************
 *  extern_unicode.c
 ************************************************************/

/*
 *  eastasian
 */
static enum EastAsianType lisp_eastasian_todevelop(enum LispEastAsianType type)
{
	switch (type) {
		case LispEastAsianType_N:
			return EastAsian_N;

		case LispEastAsianType_A:
			return EastAsian_A;

		case LispEastAsianType_H:
			return EastAsian_H;

		case LispEastAsianType_W:
			return EastAsian_W;

		case LispEastAsianType_F:
			return EastAsian_F;

		case LispEastAsianType_NA:
			return EastAsian_NA;

		default:
			return EastAsian_error;
	}
}

int lisp_eastasian_set(enum LispEastAsianType type, unsigned width)
{
	enum EastAsianType index;

	index = lisp_eastasian_todevelop(type);
	if (index == EastAsian_error)
		return 1;
	EastAsianSymbol[index] = width;
	return 0;
}

int lisp_eastasian_get(enum LispEastAsianType type, unsigned *ret)
{
	enum EastAsianType index;

	index = lisp_eastasian_todevelop(type);
	if (index == EastAsian_error)
		return 1;
	*ret = EastAsianSymbol[index];
	return 0;
}

static enum LispEastAsianType lisp_eastasian_toextern(enum EastAsianType type)
{
	switch (type) {
		case EastAsian_N:
			return LispEastAsianType_N;

		case EastAsian_A:
			return LispEastAsianType_A;

		case EastAsian_H:
			return LispEastAsianType_H;

		case EastAsian_W:
			return LispEastAsianType_W;

		case EastAsian_F:
			return LispEastAsianType_F;

		case EastAsian_NA:
			return LispEastAsianType_NA;

		default:
			return LispEastAsianType_error;
	}
}

enum LispEastAsianType lisp_eastasian_type_unicode(unicode c)
{
	enum EastAsianType type;
	type = eastasian_symbol(c);
	return lisp_eastasian_toextern(type);
}

enum LispEastAsianType lisp_eastasian_type_character(addr value)
{
	unicode c;

	hold_value(value, &value);
	if (! characterp(value))
		return LispEastAsianType_error;

	GetCharacter(value, &c);
	return lisp_eastasian_type_unicode(c);
}

unsigned lisp_eastasian_unicode(unicode c)
{
	return eastasian_width(c);
}

int lisp_eastasian_character_(addr value, unsigned *ret)
{
	unicode c;

	hold_value(value, &value);
	if (! characterp(value))
		return fmte_("Invalid character type ~S.", value, NULL);
	GetCharacter(value, &c);
	return Result(ret, eastasian_width(c));
}

int lisp_eastasian_string_(addr value, size_t *ret)
{
	hold_value(value, &value);
	if (! stringp(value))
		return fmte_("Invalid string type ~S.", value, NULL);
	return eastasian_length_(value, ret, NULL);
}

int lisp_eastasian_width_(addr value, size_t *ret)
{
	unicode c;

	hold_value(value, &value);
	if (characterp(value)) {
		GetCharacter(value, &c);
		return Result(ret, eastasian_width(c));
	}
	if (stringp(value))
		return eastasian_length_(value, ret, NULL);

	return fmte_("Invalid string type ~S.", value, NULL);
}


/* unicode */
int lisp_unicode_count(void)
{
	return UnicodeCount;
}


/* UTF-8 */
int lisp_utf8_encode(unicode c, void *ptr, size_t *ret)
{
	return encode_utf8(c, (byte *)ptr, ret);
}


/* UTF-16 */
int lisp_utf16_range(unicode c)
{
	return UTF16range(c);
}

int lisp_utf16_high(unicode c)
{
	return UTF16high(c);
}

int lisp_utf16_low(unicode c)
{
	return UTF16low(c);
}

unicode lisp_utf16_merge(byte16 high, byte16 low)
{
	return UTF16unicode(high, low);
}


/************************************************************
 *  file.c
 ************************************************************/

/*
 *  Common Function
 */
static int standard_constant_stream(addr *stream,
		enum StreamType type,
		int (*call)(filestream))
{
	addr pos;
	filestream fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	fm = PtrFileMemory(pos);
	if ((*call)(fm))
		return 1;
	force_open_stream(pos);
	*stream = pos;

	return 0;
}

static void encode_standard_stream(addr pos)
{
	filestream fm;
	struct FileEncode *encode;

	fm = PtrFileMemory(pos);
	encode = &(fm->encode);
	encode->type = EncodeType_utf8;
	encode->bom = EncodeBom_empty;
	encode->error = 0;
}

int make_standard_input(addr *stream)
{
	if (standard_constant_stream(stream,
				StreamType_BincharInput,
				standard_input_filememory))
		return 1;
	encode_standard_stream(*stream);

	return 0;
}

int make_standard_output(addr *stream)
{
	if (standard_constant_stream(stream,
				StreamType_BincharOutput,
				standard_output_filememory))
		return 1;
	encode_standard_stream(*stream);

	return 0;
}

int make_standard_error(addr *stream)
{
	if (standard_constant_stream(stream,
				StreamType_BincharOutput,
				standard_error_filememory))
		return 1;
	encode_standard_stream(*stream);

	return 0;
}

int update_standard_input(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return update_standard_input_filememory(PtrFileMemory(stream));
}

int update_standard_output(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return update_standard_output_filememory(PtrFileMemory(stream));
}

int update_standard_error(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return update_standard_error_filememory(PtrFileMemory(stream));
}

int script_header(addr stream)
{
	int check;
	byte a, b;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	Check(fm->redirect, "redirect error");
	/* read UTF-8 BOM */
	if (readbom8_encode(stream) < 0)
		return end_filememory(fm);
	/* #\# */
	check = getc_filememory(fm, &a);
	if (check)
		return check;
	if (a != '#')
		return ungetc_filememory(fm, a);
	/* #\! */
	check = getc_filememory(fm, &b);
	if (check)
		return check;
	if (b != '!') {
		ungetc_filememory(fm, b);
		return ungetc_filememory(fm, a);
	}
	/* ... \n */
	for (;;) {
		check = getc_filememory(fm, &a);
		if (check)
			return check;
		if (a == 0x0A || a == 0x0D)
			break;
	}

	return 0;
}


/*
 *  stream function
 */
void force_close_stream_file(addr stream)
{
	filestream fm;

	CheckFileStream(stream);
	if (! open_stream_p(stream))
		return;

	fm = PtrFileMemory(stream);
	(void)close_filememory(fm);
	force_close_stream(stream);
}

static int close_stream_abort_(addr stream)
{
	addr check;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(SYSTEM_CLOSE_ABORT, &check);
	getspecial_local(ptr, check, &check);
	if (check == Unbound || check == Nil)
		return 0;
	/* :abort t */
	Return(pathname_designator_heap_(ptr, stream, &check));
	return delete_file_files_(ptr, check);
}

int close_stream_file_(addr stream, addr *ret)
{
	int outputp;
	filestream fm;

	CheckFileStream(stream);
	if (! open_stream_p(stream))
		return Result(ret, T);

	/* buffering */
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return close_stream_buffering_(stream, ret);

	/* filememory */
	outputp = (fm->direct == filememory_output);
	if (close_filememory(fm)) {
		*ret = Nil;
		return fmte_("close error", NULL);
	}
	if (outputp) {
		Return(close_stream_abort_(stream));
	}

	return Result(ret, T);
}

int read_binary_file_(addr stream, void *pos, size_t size, size_t *ret)
{
	int check;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return read_binary_buffering_(stream, pos, size, ret);

	check = readf_filememory(fm, pos, size, ret);
	if (check < 0)
		return fmte_("read error", NULL);

	return 0;
}


/* read-byte */
static int read_byte_file_u8(filestream fm, addr *ret)
{
	int check;
	byte v;

	check = getc_filememory(fm, &v);
	if (check) {
		*ret = Nil;
		return check;
	}
	fixnum_heap(ret, (fixnum)v);

	return 0;
}

static int read_byte_file_u16(filestream fm, addr *ret)
{
	int check;
	uint16_t v;

	check = read_u16_filememory(fm, &v);
	if (check) {
		*ret = Nil;
		return check;
	}
	fixnum_heap(ret, (fixnum)v);

	return 0;
}

static int read_byte_file_u32(filestream fm, addr *ret)
{
	int check;
	uint32_t v;

	check = read_u32_filememory(fm, &v);
	if (check) {
		*ret = Nil;
		return check;
	}
#ifdef LISP_64BIT
	fixnum_heap(ret, (fixnum)v);
#else
	integer_fixed_heap(ret, signplus_bignum, (fixed)v);
#endif

	return 0;
}

#ifdef LISP_64BIT
static int read_byte_file_u64(filestream fm, addr *ret)
{
	int check;
	uint64_t v;

	check = read_u64_filememory(fm, &v);
	if (check) {
		*ret = Nil;
		return check;
	}
	integer_fixed_heap(ret, signplus_bignum, (fixed)v);

	return 0;
}
#endif

static int read_byte_file_s8(filestream fm, addr *ret)
{
	int check;
	signed char v;

	check = getc_filememory(fm, (byte *)&v);
	if (check) {
		*ret = Nil;
		return check;
	}
	fixnum_heap(ret, (fixnum)v);

	return 0;
}

static int read_byte_file_s16(filestream fm, addr *ret)
{
	int check;
	int16_t v;

	check = read_s16_filememory(fm, &v);
	if (check) {
		*ret = Nil;
		return check;
	}
	fixnum_heap(ret, (fixnum)v);

	return 0;
}

static int read_byte_file_s32(filestream fm, addr *ret)
{
	int check;
	int32_t v;

	check = read_s32_filememory(fm, &v);
	if (check) {
		*ret = Nil;
		return check;
	}
	fixnum_heap(ret, (fixnum)v);

	return 0;
}

#ifdef LISP_64BIT
static int read_byte_file_s64(filestream fm, addr *ret)
{
	int check;
	int64_t v;

	check = read_s64_filememory(fm, &v);
	if (check) {
		*ret = Nil;
		return check;
	}
	fixnum_heap(ret, (fixnum)v);

	return 0;
}
#endif

int read_byte_file_type(addr stream, addr *ret)
{
	filestream fm;

	fm = PtrFileMemory(stream);
	switch (fm->encode.type) {
		case EncodeType_binary:
			return read_byte_file_u8(fm, ret);

		case EncodeType_unsigned16:
			return read_byte_file_u16(fm, ret);

		case EncodeType_unsigned32:
			return read_byte_file_u32(fm, ret);

		case EncodeType_signed8:
			return read_byte_file_s8(fm, ret);

		case EncodeType_signed16:
			return read_byte_file_s16(fm, ret);

		case EncodeType_signed32:
			return read_byte_file_s32(fm, ret);

#ifdef LISP_ARCH_64BIT
		case EncodeType_unsigned64:
			return read_byte_file_u64(fm, ret);

		case EncodeType_signed64:
			return read_byte_file_s64(fm, ret);
#endif

		default:
			*ret = Nil;
			return -1;
	}
}

int read_byte_file_(addr stream, addr *value, int *ret)
{
	int check;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return read_byte_buffering_(stream, value, ret);

	check = read_byte_file_type(stream, value);
	if (check < 0)
		return fmte_("read-byte-file error", NULL);

	return Result(ret, check);
}

int unread_byte_file_(addr stream, byte c)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (ungetc_filememory(fm, c))
		return fmte_("unread_byte error", NULL);

	return 0;
}

int write_binary_file_(addr stream, const void *pos, size_t size, size_t *ret)
{
	int check;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return write_binary_buffering_(stream, pos, size, ret);

	check = write_filememory(fm, pos, size, ret);
	if (check)
		return fmte_("write error", NULL);

	return 0;
}

/* write-byte */
static int write_byte_file_u8_(filestream fm, addr pos)
{
	fixnum v;

	if (GetFixnum_unsigned(pos, &v))
		goto error;
	if (0xFF < v)
		goto error;
	if (putc_filememory(fm, (byte)v))
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}

static int write_byte_file_u16_(filestream fm, addr pos)
{
	int check;
	fixnum v;
	uint16_t u16;
	size_t size;

	if (GetFixnum_unsigned(pos, &v))
		goto error;
	if (0xFFFF < v)
		goto error;

	u16 = (uint16_t)v;
	check = write_filememory(fm, (const void *)&u16, 2, &size);
	if (check)
		goto error;
	if (size != 2)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}

#ifdef LISP_64BIT
static int write_byte_file_u32_(filestream fm, addr pos)
{
	int check;
	fixnum v;
	uint32_t u32;
	size_t size;

	if (GetFixnum_unsigned(pos, &v))
		goto error;
	if (0xFFFFFFFF < v)
		goto error;

	u32 = (uint32_t)v;
	check = write_filememory(fm, (const void *)&u32, 4, &size);
	if (check)
		goto error;
	if (size != 4)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}
#else
static int write_byte_file_u32_(filestream fm, addr pos)
{
	int check;
	fixed v;
	size_t size;

	if (getfixed1_integer(pos, &check, &v))
		goto error;
	if (check != signplus_bignum)
		goto error;

	check = write_filememory(fm, (const void *)&v, 4, &size);
	if (check)
		goto error;
	if (size != 4)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}
#endif

#ifdef LISP_64BIT
static int write_byte_file_u64_(filestream fm, addr pos)
{
	int check;
	fixed v;
	size_t size;

	if (getfixed1_integer(pos, &check, &v))
		goto error;
	if (check != signplus_bignum)
		goto error;

	check = write_filememory(fm, (const void *)&v, 8, &size);
	if (check)
		goto error;
	if (size != 8)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}
#endif

static int write_byte_file_s8_(filestream fm, addr pos)
{
	int check;
	fixnum v;
	int8_t u8;
	size_t size;

	if (GetFixnum_signed(pos, &v))
		goto error;
	if (v < -0x80 || 0x7F < v)
		goto error;

	u8 = (int8_t)v;
	check = write_filememory(fm, (const void *)&u8, 1, &size);
	if (check)
		goto error;
	if (size != 1)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}

static int write_byte_file_s16_(filestream fm, addr pos)
{
	int check;
	fixnum v;
	int16_t u16;
	size_t size;

	if (GetFixnum_signed(pos, &v))
		goto error;
	if (v < -0x8000 || 0x7FFF < v)
		goto error;

	u16 = (int16_t)v;
	check = write_filememory(fm, (const void *)&u16, 2, &size);
	if (check)
		goto error;
	if (size != 2)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}

#ifdef LISP_64BIT
static int write_byte_file_s32_(filestream fm, addr pos)
{
	int check;
	fixnum v;
	int32_t u32;
	size_t size;

	if (GetFixnum_signed(pos, &v))
		goto error;
	if (v < -0x80000000LL || 0x7FFFFFFFLL < v)
		goto error;

	u32 = (int32_t)v;
	check = write_filememory(fm, (const void *)&u32, 4, &size);
	if (check)
		goto error;
	if (size != 4)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}
#else
static int write_byte_file_s32_(filestream fm, addr pos)
{
	int check;
	fixnum v;
	size_t size;

	if (GetFixnum_signed(pos, &v))
		goto error;

	check = write_filememory(fm, (const void *)&v, 4, &size);
	if (check)
		goto error;
	if (size != 4)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}
#endif

#ifdef LISP_64BIT
static int write_byte_file_s64_(filestream fm, addr pos)
{
	int check;
	fixnum v;
	size_t size;

	if (GetFixnum_signed(pos, &v))
		goto error;

	check = write_filememory(fm, (const void *)&v, 8, &size);
	if (check)
		goto error;
	if (size != 8)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}
#endif

int write_byte_file_type_(filestream fm, addr pos)
{
	switch (fm->encode.type) {
		case EncodeType_binary:
			return write_byte_file_u8_(fm, pos);

		case EncodeType_unsigned16:
			return write_byte_file_u16_(fm, pos);

		case EncodeType_unsigned32:
			return write_byte_file_u32_(fm, pos);

		case EncodeType_signed8:
			return write_byte_file_s8_(fm, pos);

		case EncodeType_signed16:
			return write_byte_file_s16_(fm, pos);

		case EncodeType_signed32:
			return write_byte_file_s32_(fm, pos);

#ifdef LISP_ARCH_64BIT
		case EncodeType_unsigned64:
			return write_byte_file_u64_(fm, pos);

		case EncodeType_signed64:
			return write_byte_file_s64_(fm, pos);
#endif

		default:
			return fmte_("Invalid stream type.", NULL);
	}
}

int write_byte_file_(addr stream, addr pos)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return write_byte_buffering_(stream, pos);

	return write_byte_file_type_(fm, pos);
}


/*
 *  character
 */
int read_char_file_(addr stream, unicode *c, int *ret)
{
	int check;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return read_char_buffering_(stream, c, ret);

	Return(read_char_encode_(fm, c, &check));
	if (check < 0)
		return fmte_("read_char_encode error", NULL);

	return Result(ret, check? 1: 0);
}

int read_hang_file_(addr stream, unicode *c, int *hang, int *ret)
{
	int check;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return read_hang_buffering_(stream, c, hang, ret);

	Return(read_hang_encode_(fm, c, hang, &check));
	if (check < 0)
		return fmte_("read_hang_encode error", NULL);

	return Result(ret, check? 1: 0);
}

int write_char_file_(addr stream, unicode c)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return write_char_buffering_(stream, c);

	return write_char_encode_(fm, c);
}

static void element_type_unsigned_byte(fixnum v, addr *ret)
{
	addr type, value;

	GetConst(COMMON_UNSIGNED_BYTE, &type);
	fixnum_heap(&value, v);
	list_heap(ret, type, value, NULL);
}

static void element_type_signed_byte(fixnum v, addr *ret)
{
	addr type, value;

	GetConst(COMMON_SIGNED_BYTE, &type);
	fixnum_heap(&value, v);
	list_heap(ret, type, value, NULL);
}

int element_type_file_(addr stream, addr *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	switch (fm->encode.type) {
		case EncodeType_binary:
			element_type_unsigned_byte(8, ret);
			break;

		case EncodeType_unsigned16:
			element_type_unsigned_byte(16, ret);
			break;

		case EncodeType_unsigned32:
			element_type_unsigned_byte(32, ret);
			break;

		case EncodeType_unsigned64:
			element_type_unsigned_byte(64, ret);
			break;

		case EncodeType_signed8:
			element_type_signed_byte(8, ret);
			break;

		case EncodeType_signed16:
			element_type_signed_byte(16, ret);
			break;

		case EncodeType_signed32:
			element_type_signed_byte(32, ret);
			break;

		case EncodeType_signed64:
			element_type_signed_byte(64, ret);
			break;

		default:
			GetConst(COMMON_CHARACTER, ret);
			break;
	}

	return 0;
}

int external_format_file_(addr stream, addr *ret)
{
	enum EncodeBom bom;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	bom = (enum EncodeBom)fm->encode.bom;
	switch (fm->encode.type) {
		case EncodeType_ascii:
			GetConst(SYSTEM_ASCII, ret);
			break;

		case EncodeType_utf8:
			if (bom == EncodeBom_exist)
				GetConst(SYSTEM_UTF_8_BOM, ret);
			else
				GetConst(SYSTEM_UTF_8, ret);
			break;

		case EncodeType_utf16le:
			if (bom == EncodeBom_exist)
				GetConst(SYSTEM_UTF_16LE_BOM, ret);
			else
				GetConst(SYSTEM_UTF_16LE, ret);
			break;

		case EncodeType_utf16be:
			if (bom == EncodeBom_exist)
				GetConst(SYSTEM_UTF_16BE_BOM, ret);
			else
				GetConst(SYSTEM_UTF_16BE, ret);
			break;

		case EncodeType_utf32le:
			if (bom == EncodeBom_exist)
				GetConst(SYSTEM_UTF_32LE_BOM, ret);
			else
				GetConst(SYSTEM_UTF_32LE, ret);
			break;

		case EncodeType_utf32be:
			if (bom == EncodeBom_exist)
				GetConst(SYSTEM_UTF_32BE_BOM, ret);
			else
				GetConst(SYSTEM_UTF_32BE, ret);
			break;

		case EncodeType_windows:
			GetConst(SYSTEM_WINDOWS, ret);
			break;

		default:
			GetConst(KEYWORD_DEFAULT, ret);
			break;
	}

	return 0;
}

static int file_length_file_value_(filestream fm, size_t *value, int *ret)
{
	int check;

	if (flush_filememory(fm)) {
		*value = 0;
		*ret = 0;
		return fmte_("flush error.", NULL);
	}
	check = file_length_filememory(fm, value);
	if (check < 0) {
		*value = 0;
		*ret = 0;
		return fmte_("file-length error.", NULL);
	}

	return Result(ret, check);
}

static size_t file_length_division(filestream fm, size_t size)
{
	switch (fm->encode.type) {
		case EncodeType_unsigned16:
		case EncodeType_signed16:
			return size / 2;

		case EncodeType_unsigned32:
		case EncodeType_signed32:
			return size / 4;

		case EncodeType_unsigned64:
		case EncodeType_signed64:
			return size / 8;

		default:
			return size;
	}
}

static size_t file_length_multiple(filestream fm, size_t size)
{
	switch (fm->encode.type) {
		case EncodeType_unsigned16:
		case EncodeType_signed16:
			return size * 2;

		case EncodeType_unsigned32:
		case EncodeType_signed32:
			return size * 4;

		case EncodeType_unsigned64:
		case EncodeType_signed64:
			return size * 8;

		default:
			return size;
	}
}

int file_length_file_type_(filestream fm, size_t *value, int *ret)
{
	int check;
	size_t size;

	Return(file_length_file_value_(fm, &size, &check));
	if (check) {
		*value = 0;
		return Result(ret, check);
	}

	*value = file_length_division(fm, size);
	return Result(ret, 0);
}

int file_length_file_(addr stream, size_t *value, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return file_length_buffering_(stream, value, ret);

	return file_length_file_type_(fm, value, ret);
}

static int file_position_file_unread_(addr stream, size_t *ret)
{
	int check;
	struct StructStream *ptr;
	filestream fm;
	addr pos;

	ptr = PtrStructStream(stream);
	if (ptr->unread_check == 0)
		return Result(ret, 0);

	/* unread */
	fm = PtrFileMemory(stream);
	check = length_char_encode(fm, ptr->unread);
	if (check < 0) {
		*ret = 0;
		character_heap(&pos, ptr->unread);
		return fmte_("Invalid unread character ~S.", pos, NULL);
	}

	return Result(ret, (size_t)check);
}

int file_position_file_type_(addr stream, size_t *value, int *ret)
{
	int check;
	filestream fm;
	size_t size, unread;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	*value = 0;
	*ret = 0;

	/* file-memory position */
	check = file_position_filememory(fm, &size);
	if (check < 0)
		return fmte_("file-position error.", NULL);
	if (check)
		return Result(ret, 1);

	/* unread */
	Return(file_position_file_unread_(stream, &unread));
	size = file_length_division(fm, size);

	/* result */
	if (size < unread)
		return fmte_("The stream ~S position is a minus value.", stream, NULL);
	*value = size - unread;
	return Result(ret, 0);
}

int file_position_file_(addr stream, size_t *value, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return file_position_buffering_(stream, value, ret);

	return file_position_file_type_(stream, value, ret);
}

int file_position_start_file_type_(addr stream, int *ret)
{
	int check;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (flush_filememory(fm)) {
		*ret = 0;
		return fmte_("flush error.", NULL);
	}
	check = file_position_start_filememory(fm);
	if (check < 0) {
		*ret = 0;
		return fmte_("file-position-start error.", NULL);
	}
	if (check == 0) {
		PtrStructStream(stream)->unread_check = 0;
	}

	return Result(ret, check);
}

int file_position_start_file_(addr stream, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return file_position_start_buffering_(stream, ret);

	return file_position_start_file_type_(stream, ret);
}

int file_position_end_file_type_(addr stream, int *ret)
{
	int check;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (flush_filememory(fm)) {
		*ret = 0;
		return fmte_("flush error.", NULL);
	}
	check = file_position_end_filememory(fm);
	if (check < 0) {
		*ret = 0;
		return fmte_("file-position-end error.", NULL);
	}
	if (check == 0) {
		PtrStructStream(stream)->unread_check = 0;
	}

	return Result(ret, check);
}

int file_position_end_file_(addr stream, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return file_position_end_buffering_(stream, ret);

	return file_position_end_file_type_(stream, ret);
}

int file_position_set_file_type_(addr stream, size_t value, int *ret)
{
	int check;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (flush_filememory(fm)) {
		*ret = 0;
		return fmte_("flush error.", NULL);
	}
	value = file_length_multiple(fm, value);
	check = file_position_set_filememory(fm, value);
	if (check < 0) {
		*ret = 0;
		return fmte_("file-position-end error.", NULL);
	}
	if (check == 0) {
		PtrStructStream(stream)->unread_check = 0;
	}

	return Result(ret, check);
}

int file_position_set_file_(addr stream, size_t value, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return file_position_set_buffering_(stream, value, ret);

	return file_position_set_file_type_(stream, value, ret);
}

int file_charlen_file_(addr stream, unicode u, size_t *value, int *ret)
{
	int check;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = length_char_encode(fm, u);
	if (check < 0)
		return Result(ret, 1);
	*value = (size_t)check;

	return Result(ret, 0);
}

int file_strlen_file_(addr stream, addr pos, size_t *value, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	return length_string_encode_(fm, pos, value, ret);
}

int listen_file_(addr stream, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	if (PtrStructStream(stream)->unread_check)
		return Result(ret, 1);
	fm = PtrFileMemory(stream);

	/* eof */
	if (fm->mode ==  filememory_end)
		return Result(ret, 0);

	return Result(ret, fm->cache);
}

int clear_input_file_(addr stream)
{
	filestream fm;

	CheckFileStream(stream);
	PtrStructStream(stream)->unread_check = 0;
	fm = PtrFileMemory(stream);
	if (clear_input_filememory(fm))
		return fmte_("clear-input error.", NULL);

	return 0;
}

int finish_output_file_(addr stream)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return finish_output_buffering_(stream);

	if (flush_filememory(fm))
		return fmte_("flush-filememory error.", NULL);

	return 0;
}

int force_output_file_(addr stream)
{
	return finish_output_file_(stream);
}

int clear_output_file_(addr stream)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (clear_output_filememory(fm))
		return fmte_("clear-output error.", NULL);

	return 0;
}

int exitpoint_file_(addr stream)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return exitpoint_buffering_(stream);

	exitpoint_filememory(fm);
	return 0;
}

int termsize_file_(addr stream, size_t *value, int *ret)
{
	unsigned width;

	*ret = getwidth_arch(&width, NULL);
	*value = (size_t)width;

	return 0;
}


/*
 *  core
 */
int save_stream_file(addr pos)
{
	filestream fm;
	struct StructStream *ptr;

	fm = PtrFileMemory(pos);
	close_filememory(fm);
	ptr = PtrStructStream(pos);
	ptr->terpri = 0;
	ptr->unread_check = 0;
	ptr->closed = 1;

	return 0;
}

int save_stream_system(addr pos)
{
	if (PtrFileMemory(pos)->system == filememory_stream)
		return save_stream_file(pos);
	else
		return 0;
}


/************************************************************
 *  file_buffering.c
 ************************************************************/

/*
 *  file-memory
 */
int read_low_buffering(filestream fm, byte *pos, size_t size, size_t *ret)
{
	byte c;
	int check;
	addr stream;
	size_t i;

	stream = fm->pos;
	if (! read_memory_stream_p(stream)) {
		i = 0;
		goto error;
	}

	for (i = 0; i < size; i++) {
		if (read_byte_memory_stream(stream, &c, &check))
			goto error;
		if (check) {
			*ret = i;
			return i == 0; /* Normal or EOF */
		}
		pos[i] = c;
	}
	*ret = i;
	return 0;

error:
	*ret = i;
	return -1;
}

int write_low_buffering(filestream fm, const byte *pos, size_t size, size_t *ret)
{
	addr stream;
	size_t i;

	stream = fm->pos;
	if (! write_memory_stream_p(stream)) {
		*ret = 0;
		return 1;
	}

	for (i = 0; i < size; i++) {
		if (write_byte_memory_stream(stream, pos[i]))
			goto error;
	}
	*ret = i;
	return 0;

error:
	*ret = i;
	return 1;
}

int close_low_buffering(filestream fm)
{
	return 0;
}

int flush_low_buffering(filestream fm)
{
	return 0;
}

int read_ready_low_buffering(filestream fm)
{
	return 1; /* ready */
}

int file_length_low_buffering(filestream fm, size_t *ret)
{
	if (file_length_memory_stream(fm->pos, ret)) {
		*ret = 0;
		return 1;
	}

	return 0;
}

int file_position_low_buffering(filestream fm, size_t *ret)
{
	if (file_position_memory_stream(fm->pos, ret)) {
		*ret = 0;
		return 1;
	}

	return 0;
}

int file_position_start_low_buffering(filestream fm)
{
	return file_position_start_memory_stream(fm->pos);
}

int file_position_end_low_buffering(filestream fm)
{
	return file_position_end_memory_stream(fm->pos);
}

int file_position_set_low_buffering(filestream fm, size_t pos)
{
	return file_position_set_memory_stream(fm->pos, pos);
}


/*
 *  file
 */
static filestream begin_buffering_filememory(addr stream, addr *ret)
{
	addr mem;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	Check(fm->redirect == 0, "redirect error");
	GetPathnameStream(stream, &mem);
	Check(! memory_stream_p(mem), "type error");

	*ret = fm->pos;
	fm->pos = mem;

	return fm;
}

static void end_buffering_filememory(addr stream, addr prev)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	fm->pos = prev;
}

int close_stream_buffering_(addr stream, addr *ret)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	check = close_filememory(fm);
	end_buffering_filememory(stream, prev);

	if (check) {
		*ret = Nil;
		return fmte_("close error.", NULL);
	}
	force_close_stream(stream);

	return Result(ret, T);
}

int read_binary_buffering_(addr stream, void *pos, size_t size, size_t *ret)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	check = readf_filememory(fm, pos, size, ret);
	end_buffering_filememory(stream, prev);

	if (check < 0)
		return fmte_("read error", NULL);

	return 0;
}

int read_byte_buffering_(addr stream, addr *value, int *ret)
{
	int check;
	addr prev;

	(void)begin_buffering_filememory(stream, &prev);
	check = read_byte_file_type(stream, value);
	end_buffering_filememory(stream, prev);

	if (check < 0)
		return fmte_("read-byte-file error", NULL);

	return Result(ret, check);
}

int write_binary_buffering_(addr stream, const void *pos, size_t size, size_t *ret)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	check = write_filememory(fm, pos, size, ret);
	end_buffering_filememory(stream, prev);

	if (check)
		return fmte_("write error", NULL);

	return 0;
}

int write_byte_buffering_(addr stream, addr pos)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	check = write_byte_file_type_(fm, pos);
	end_buffering_filememory(stream, prev);

	return check;
}

int read_char_buffering_(addr stream, unicode *c, int *ret)
{
	int check, escape;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	escape = read_char_encode_(fm, c, &check);
	end_buffering_filememory(stream, prev);

	if (escape)
		return 1;
	if (check < 0)
		return fmte_("read_char_encode error", NULL);

	return Result(ret, check? 1: 0);
}

int read_hang_buffering_(addr stream, unicode *c, int *hang, int *ret)
{
	int check, escape;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	escape = read_hang_encode_(fm, c, hang, &check);
	end_buffering_filememory(stream, prev);

	if (escape)
		return 1;
	if (check < 0)
		return fmte_("read_hang_encode error", NULL);

	return Result(ret, check? 1: 0);
}

int write_char_buffering_(addr stream, unicode c)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	check = write_char_encode_(fm, c);
	end_buffering_filememory(stream, prev);

	return check;
}

int file_length_buffering_(addr stream, size_t *value, int *ret)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	check = file_length_file_type_(fm, value, ret);
	end_buffering_filememory(stream, prev);

	return check;
}

int file_position_buffering_(addr stream, size_t *value, int *ret)
{
	int check;
	addr prev;

	(void)begin_buffering_filememory(stream, &prev);
	check = file_position_file_type_(stream, value, ret);
	end_buffering_filememory(stream, prev);

	return check;
}

int file_position_start_buffering_(addr stream, int *ret)
{
	int check;
	addr prev;

	(void)begin_buffering_filememory(stream, &prev);
	check = file_position_start_file_type_(stream, ret);
	end_buffering_filememory(stream, prev);

	return check;
}

int file_position_end_buffering_(addr stream, int *ret)
{
	int check;
	addr prev;

	(void)begin_buffering_filememory(stream, &prev);
	check = file_position_end_file_type_(stream, ret);
	end_buffering_filememory(stream, prev);

	return check;
}

int file_position_set_buffering_(addr stream, size_t value, int *ret)
{
	int check;
	addr prev;

	(void)begin_buffering_filememory(stream, &prev);
	check = file_position_set_file_type_(stream, value, ret);
	end_buffering_filememory(stream, prev);

	return check;
}

int finish_output_buffering_(addr stream)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	check = flush_filememory(fm);
	end_buffering_filememory(stream, prev);

	if (check)
		return fmte_("flush-filememory error.", NULL);

	return check;
}

int exitpoint_buffering_(addr stream)
{
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	exitpoint_filememory(fm);
	end_buffering_filememory(stream, prev);

	return 0;
}


/************************************************************
 *  file_memory.c
 ************************************************************/

#ifdef LISP_ANSIC
#undef FILEMEMORY_RESTRICT
/*
 *  file-ansi
 *    ANCI-C file function
 */
#ifndef __FILE_ANSI_HEADER__
#define __FILE_ANSI_HEADER__


#ifdef LISP_ANSIC_WINDOWS
#define UTF_buffer_clang UTF16LE_buffer_clang
#define fopen_input(x) _wfopen(x, L"rb")
#define fopen_output(x) _wfopen(x, L"wb")
#define fopen_append(x) _wfopen(x, L"ab")
#else
#define UTF_buffer_clang UTF8_buffer_clang
#define fopen_input(x) fopen(x, "rb")
#define fopen_output(x) fopen(x, "wb")
#define fopen_append(x) fopen(x, "ab")
#endif

int init_file(void)
{
	return 0;
}

void free_file(void)
{
}

int consolep_file(void)
{
	return 0;
}

static inline int standard_input_arch(file_type *file)
{
	*file = stdin;
	return 0;
}

static inline int standard_output_arch(file_type *file)
{
	*file = stdout;
	return 0;
}

static inline int standard_error_arch(file_type *file)
{
	*file = stderr;
	return 0;
}

static inline int filename_encode_(LocalRoot local, addr name, const char **const ret)
{
	addr data;

	Check(! stringp(name), "name error");
	Return(UTF8_buffer_clang_(local, &data, name));
	if (data == Unbound) {
		*ret = NULL;
		return fmte_("Invalid UTF-8 encoding ~S.", name, NULL);
	}
	posbody(data, (addr *)ret);
	return 0;
}

static inline int open_input_chartype(file_type *ret, const char *name)
{
	file_type file;

	file = fopen_input(name);
	if (file == NULL)
		return 1;
	*ret = file;

	return 0;
}

static inline int open_input_unicode(file_type *ret, const unicode *name, size_t size)
{
	byte *ptr;
	size_t value;

	ptr = NULL;
	if (UTF32_length_utf8(name, size, &value))
		goto error;
	ptr = (byte *)malloc(value);
	if (ptr == NULL)
		goto error;
	if (UTF32_make_utf8(ptr, name, value))
		goto error;
	if (open_input_chartype(ret, (const char *)ptr))
		goto error;
	free(ptr);
	return 0;

error:
	free(ptr);
	return 1;
}

static inline int open_input_arch_(LocalRoot local,
		addr name, file_type *value, int *ret)
{
	LocalStack stack;
	const char *utf8;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf8));
	if (utf8 == NULL) {
		*ret = 1;
		goto finish;
	}
	if (open_input_chartype(value, utf8)) {
		*ret = 1;
		goto finish;
	}
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline int open_output_chartype(file_type *ret,
		const char *name, enum FileOutput mode)
{
	file_type file;

	switch (mode) {
		case FileOutput_supersede:
			file = fopen_output(name);
			break;

		case FileOutput_append:
			file = fopen_append(name);
			break;

		case FileOutput_overwrite:
			file = fopen_append(name);
			if (file == NULL)
				return 1;
			if (fseek(file, 0L, SEEK_SET))
				return 1;
			*ret = file;
			return 0;

		default:
			Debug("Invalid mode.");
			return 1;
	}
	if (file == NULL)
		return 1;
	*ret = file;

	return 0;
}

static inline int open_output_arch_(LocalRoot local,
		addr name, enum FileOutput mode, file_type *value, int *ret)
{
	LocalStack stack;
	const char *utf8;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf8));
	if (utf8 == NULL) {
		*ret = 1;
		goto finish;
	}
	if (open_output_chartype(value, utf8, mode)) {
		*ret = 1;
		goto finish;
	}
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline int open_io_chartype(file_type *ret,
		const char *name, enum FileOutput mode)
{
	file_type file;

	switch (mode) {
		case FileOutput_supersede:
		case FileOutput_append:
		case FileOutput_overwrite:
			file = fopen_append(name);
			break;

		default:
			Debug("Invalid mode.");
			return -1;
	}
	if (file == NULL)
		return 1;
	*ret = file;

	return 0;
}

static inline int open_io_arch_(LocalRoot local,
		addr name, enum FileOutput mode, file_type *value, int *ret)
{
	LocalStack stack;
	const char *utf8;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf8));
	if (utf8 == NULL) {
		*ret = 1;
		goto finish;
	}
	if (open_io_chartype(value, utf8, mode)) {
		*ret = 1;
		goto finish;
	}
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline int read_arch(file_type file, void *pos, size_t size, size_t *ret)
{
	size_t check;

	check = fread(pos, 1, size, file);
	if (ferror(file)) {
		Debug("fread error");
		*ret = 0;
		return -1;
	}
	if (check == 0 && feof(file)) {
		*ret = 0;
		return 1;
	}
	else {
		*ret = check;
		return 0;
	}
}

static inline int write_arch(file_type file, const void *pos, size_t size, size_t *ret)
{
	size_t check;

	check = fwrite(pos, 1, size, file);
	if (ferror(file)) {
		Debug("fwrite error");
		*ret = 0;
		return -1;
	}
	if (check == 0 && feof(file)) {
		*ret = 0;
		return 1;
	}
	else {
		*ret = check;
		return 0;
	}
}

static inline int close_arch(file_type file)
{
	if (file == stdin)
		return 0;
	if (file == stdout)
		return 0;
	if (file == stderr)
		return 0;

	if (fclose(file)) {
		Debug("close error");
		return 1;
	}

	return 0;
}

static inline int flush_arch(file_type file)
{
	if (fflush(file)) {
		Debug("fsync error");
		return 1;
	}

	return 0;
}

static inline int read_ready_arch(file_type file)
{
	Debug("read_ready_arch does not implement.");
	return 1;
}

static inline int file_length_arch(file_type file, size_t *ret)
{
	*ret = 0;
	return 1;
}

static inline int file_position_arch(file_type file, size_t *ret)
{
	fpos_t pos;

	if (fgetpos(file, &pos)) {
		*ret = 0;
		return 1;
	}
#if defined(__GNUC__) && (! defined(__clang__))
	*ret = (size_t)pos.__pos;
#else
	*ret = (size_t)pos;
#endif

	return 0;
}

static inline int file_position_start_arch(file_type file)
{
	return fseek(file, 0, SEEK_SET) != 0;
}

static inline int file_position_end_arch(file_type file)
{
	return fseek(file, 0, SEEK_END) != 0;
}

static inline int file_position_set_arch(file_type file, size_t pos)
{
	return fseek(file, (long)pos, SEEK_SET) != 0;
}

#endif

#endif

#ifdef LISP_UNIX
#undef FILEMEMORY_RESTRICT
/*
 *  file-unix
 *    UNIX file function
 */
#ifndef __FILE_UNIX_HEADER__
#define __FILE_UNIX_HEADER__

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#define create_chmod_644 (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)

int init_file(void)
{
	return 0;
}

void free_file(void)
{
}

int consolep_file(void)
{
	return isatty(STDIN_FILENO) &&
		isatty(STDOUT_FILENO) &&
		isatty(STDERR_FILENO);
}

static inline int standard_input_arch(file_type *file)
{
	*file = STDIN_FILENO;
	return 0;
}

static inline int standard_output_arch(file_type *file)
{
	*file = STDOUT_FILENO;
	return 0;
}

static inline int standard_error_arch(file_type *file)
{
	*file = STDERR_FILENO;
	return 0;
}

static inline int filename_encode_(LocalRoot local, addr name, const char **const ret)
{
	addr data;

	Check(! stringp(name), "name error");
	Return(UTF8_buffer_clang_(local, &data, name));
	if (data == Unbound) {
		*ret = NULL;
		return fmte_("Invalid UTF-8 encoding ~S.", name, NULL);
	}
	posbody(data, (addr *)ret);
	return 0;
}

static inline int open_input_chartype(file_type *ret, const char *name)
{
	file_type file;

retry:
	file = open(name, O_RDONLY);
	if (file < 0) {
		if (errno == EINTR)
			goto retry;
		return 1;
	}
	*ret = file;

	return 0;
}

static inline int open_input_unicode(file_type *ret, const unicode *name, size_t size)
{
	byte *ptr;
	size_t value;

	ptr = NULL;
	if (UTF32_length_utf8(name, size, &value))
		goto error;
	ptr = (byte *)malloc(value);
	if (ptr == NULL)
		goto error;
	if (UTF32_make_utf8(ptr, name, value))
		goto error;
	if (open_input_chartype(ret, (const char *)ptr))
		goto error;
	free(ptr);
	return 0;

error:
	free(ptr);
	return 1;
}

static inline int open_input_arch_(LocalRoot local,
		addr name, file_type *value, int *ret)
{
	LocalStack stack;
	const char *utf8;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf8));
	if (utf8 == NULL) {
		*ret = 1;
		goto finish;
	}
	if (open_input_chartype(value, utf8)) {
		*ret = 1;
		goto finish;
	}
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline file_type open_output_call(const char *name, enum FileOutput mode)
{
	switch (mode) {
		case FileOutput_supersede:
			return open(name, O_WRONLY | O_CREAT | O_TRUNC, create_chmod_644);

		case FileOutput_append:
			return open(name, O_WRONLY | O_CREAT | O_APPEND, create_chmod_644);

		case FileOutput_overwrite:
			return open(name, O_WRONLY | O_CREAT, create_chmod_644);

		default:
			Debug("Invalid mode.");
			return -1;
	}
}

static inline int open_output_arch_(LocalRoot local,
		addr name, enum FileOutput mode, file_type *value, int *ret)
{
	LocalStack stack;
	file_type file;
	const char *utf8;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf8));
	if (utf8 == NULL) {
		*ret = 1;
		goto finish;
	}
	file = open_output_call(utf8, mode);
	if (file < 0) {
		*ret = 1;
		goto finish;
	}
	*value = file;
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline file_type open_io_call(const char *name, enum FileOutput mode)
{
	switch (mode) {
		case FileOutput_supersede:
			return open(name, O_RDWR | O_CREAT | O_TRUNC, create_chmod_644);

		case FileOutput_append:
			return open(name, O_RDWR | O_CREAT | O_APPEND, create_chmod_644);

		case FileOutput_overwrite:
			return open(name, O_RDWR | O_CREAT, create_chmod_644);

		default:
			Debug("Invalid mode.");
			return -1;
	}
}

static inline int open_io_arch_(LocalRoot local,
		addr name, enum FileOutput mode, file_type *value, int *ret)
{
	LocalStack stack;
	file_type file;
	const char *utf8;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf8));
	if (utf8 == NULL) {
		*ret = 1;
		goto finish;
	}
	file = open_io_call(utf8, mode);
	if (file < 0) {
		*ret = 1;
		goto finish;
	}
	*value = file;
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline int read_arch(file_type file, void *pos, size_t size, size_t *ret)
{
	ssize_t check;

retry:
	check = read(file, pos, size);
	if (check < 0) {
		if (errno == EINTR)
			goto retry;
		Debug("read error");
		*ret = 0;
		return check;
	}
	if (check == 0) {
		*ret = 0;
		return 1;
	}
	else {
		*ret = (size_t)check;
		return 0;
	}
}

static inline int write_arch(file_type file, const void *pos, size_t size, size_t *ret)
{
	ssize_t check;

retry:
	check = write(file, pos, size);
	if (check < 0) {
		if (errno == EINTR)
			goto retry;
		Debug("write error");
		*ret = 0;
		return check;
	}
	if (check == 0) {
		*ret = 0;
		return 1;
	}
	else {
		*ret = (size_t)check;
		return 0;
	}
}

static inline int close_arch(file_type file)
{
	if (file == STDIN_FILENO)
		return 0;
	if (file == STDOUT_FILENO)
		return 0;
	if (file == STDERR_FILENO)
		return 0;

	if (close(file)) {
		Debug("close error");
		return 1;
	}

	return 0;
}

static inline int flush_arch(file_type file)
{
	if (fsync(file) && errno != EINVAL) {
		Debug("fsync error");
		return 1;
	}

	return 0;
}

static inline int read_ready_arch(file_type file)
{
	int result;
	fd_set fds;
	struct timeval timeout;

	FD_ZERO(&fds);
	FD_SET(file, &fds);
	memset(&timeout, 0, sizeof(timeout));

retry:
	result = select(file + 1, &fds, NULL, NULL, &timeout);
	if (result < 0) {
		if (errno == EINTR)
			goto retry;
		Debug("select error");
		return 1;
	}

	return result != 0;
}

static inline int file_length_arch(file_type file, size_t *ret)
{
	struct stat st;

	if (fstat(file, &st)) {
		*ret = 0;
		return 1;
	}
	else {
		*ret = (size_t)st.st_size;
		return 0;
	}
}

static inline int file_position_arch(file_type file, size_t *ret)
{
	off_t pos;

	pos = lseek(file, 0, SEEK_CUR);
	if (pos < 0) {
		*ret = 0;
		return 1;
	}
	*ret = (size_t)pos;

	return 0;
}

static inline int file_position_start_arch(file_type file)
{
	return lseek(file, 0, SEEK_SET) < 0;
}

static inline int file_position_end_arch(file_type file)
{
	return lseek(file, 0, SEEK_END) < 0;
}

static inline int file_position_set_arch(file_type file, size_t pos)
{
	return lseek(file, (off_t)pos, SEEK_SET) < 0;
}
#endif

#endif

#ifdef LISP_WINDOWS
#define FILEMEMORY_RESTRICT 0xFFFFFFFF
/*
 *  file-windows
 *    Windows file function
 */
#ifndef __FILE_WINDOWS_HEADER__
#define __FILE_WINDOWS_HEADER__

#include <windows.h>
#ifdef LISP_TERME_WINDOWS
#include "windows_arch.h"
#endif

static int fileio_input_console_p;
static int fileio_output_console_p;
static int fileio_error_console_p;
static file_type fileio_input;
static file_type fileio_output;
static file_type fileio_error;

static int init_fileio(file_type *file, DWORD type)
{
	HANDLE hFile;

	hFile = GetStdHandle(type);
	if (hFile == INVALID_HANDLE_VALUE) {
		Debug("GetStdHandle error");
		return 1;
	}
	*file = hFile;

	return 0;
}

static inline int standard_input_arch(file_type *ret)
{
	HANDLE file;
	DWORD ignore;

	if (init_fileio(&file, STD_INPUT_HANDLE))
		return 1;
	fileio_input_console_p = GetConsoleMode(file, &ignore);
	fileio_input = file;
	*ret = file;

	return 0;
}

static inline int standard_output_arch(file_type *ret)
{
	HANDLE file;
	DWORD ignore;

	if (init_fileio(&file, STD_OUTPUT_HANDLE))
		return 1;
	fileio_output_console_p = GetConsoleMode(file, &ignore);
	fileio_output = file;
	*ret = file;

	return 0;
}

static inline int standard_error_arch(file_type *ret)
{
	HANDLE file;
	DWORD ignore;

	if (init_fileio(&file, STD_ERROR_HANDLE))
		return 1;
	fileio_error_console_p = GetConsoleMode(file, &ignore);
	fileio_error = file;
	*ret = file;

	return 0;
}

int init_file(void)
{
	if (standard_input_arch(&fileio_input))
		return 1;
	if (standard_output_arch(&fileio_output))
		return 1;
	if (standard_error_arch(&fileio_error))
		return 1;

	return 0;
}

void free_file(void)
{
}

int consolep_file(void)
{
#ifdef LISP_TERME_WINDOWS
	return consolep_windows();
#else
	return fileio_input_console_p
		|| fileio_output_console_p
		|| fileio_error_console_p;
#endif
}

static inline int filename_encode_(LocalRoot local, addr name, LPCWSTR *ret)
{
	addr data;

	Check(! stringp(name), "name error");
	Return(UTF16_buffer_clang_(local, &data, name));
	if (data == Unbound) {
		*ret = NULL;
		return fmte_("Invalid UTF-16 encoding ~S.", name, NULL);
	}
	posbody(data, (addr *)ret);
	return 0;
}

static inline int open_input_chartype(file_type *ret, LPCWSTR name)
{
	file_type file;

	file = CreateFileW(
			name,
			GENERIC_READ,
			FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
			NULL,
			OPEN_EXISTING,
			FILE_ATTRIBUTE_NORMAL,
			NULL);
	if (file == INVALID_HANDLE_VALUE)
		return 1;
	*ret = file;

	return 0;
}

static inline int open_input_unicode(file_type *ret, const unicode *name, size_t size)
{
	byte16 *ptr;
	size_t value;

	ptr = NULL;
	if (UTF32_length_utf16(name, size, &value))
		goto error;
	ptr = (byte16 *)malloc(sizeoft(byte16) * value);
	if (ptr == NULL)
		goto error;
	if (UTF32_make_utf16(ptr, name, value))
		goto error;
	if (open_input_chartype(ret, (LPCWSTR)ptr))
		goto error;
	free(ptr);
	return 0;

error:
	free(ptr);
	return 1;
}

static inline int open_input_arch_(LocalRoot local,
		addr name, file_type *value, int *ret)
{
	LocalStack stack;
	LPCWSTR utf16;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf16));
	if (utf16 == NULL) {
		*ret = 1;
		goto finish;
	}
	if (open_input_chartype(value, utf16)) {
		*ret = 1;
		goto finish;
	}
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline int open_output_chartype(file_type *ret,
		LPCWSTR name, enum FileOutput mode)
{
	file_type file;

	switch (mode) {
		case FileOutput_supersede:
			file = CreateFileW(
					name,
					GENERIC_WRITE,
					FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
					NULL,
					CREATE_ALWAYS,
					FILE_ATTRIBUTE_NORMAL,
					NULL);
			break;

		case FileOutput_append:
			file = CreateFileW(
					name,
					FILE_APPEND_DATA,
					FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
					NULL,
					OPEN_ALWAYS,
					FILE_ATTRIBUTE_NORMAL,
					NULL);
			break;

		case FileOutput_overwrite:
			file = CreateFileW(
					name,
					GENERIC_WRITE,
					FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
					NULL,
					OPEN_ALWAYS,
					FILE_ATTRIBUTE_NORMAL,
					NULL);
			break;

		default:
			Debug("Invalid mode.");
			return 1;
	}
	if (file == INVALID_HANDLE_VALUE)
		return 1;
	*ret = file;

	return 0;
}

static inline int open_output_arch_(LocalRoot local,
		addr name, enum FileOutput mode, file_type *value, int *ret)
{
	LocalStack stack;
	LPCWSTR utf16;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf16));
	if (utf16 == NULL) {
		*ret = 1;
		goto finish;
	}
	if (open_output_chartype(value, utf16, mode)) {
		*ret = 1;
		goto finish;
	}
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline file_type open_io_chartype_append(LPCWSTR name)
{
	file_type file;
	LARGE_INTEGER zero, pos;

	file = CreateFileW(
			name,
			GENERIC_READ | GENERIC_WRITE,
			FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
			NULL,
			OPEN_ALWAYS,
			FILE_ATTRIBUTE_NORMAL,
			NULL);
	if (file == INVALID_HANDLE_VALUE)
		return file;

	/* file-position-end */
	zero.QuadPart = 0ULL;
	if (SetFilePointerEx(file, zero, &pos, FILE_END) == 0) {
		(void)CloseHandle(file);
		return INVALID_HANDLE_VALUE;
	}

	/* success */
	return file;
}

static inline int open_io_chartype(file_type *ret,
		LPCWSTR name, enum FileOutput mode)
{
	file_type file;

	switch (mode) {
		case FileOutput_supersede:
			file = CreateFileW(
					name,
					GENERIC_READ | GENERIC_WRITE,
					FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
					NULL,
					CREATE_ALWAYS,
					FILE_ATTRIBUTE_NORMAL,
					NULL);
			break;

		case FileOutput_append:
			file = open_io_chartype_append(name);
			break;

		case FileOutput_overwrite:
			file = CreateFileW(
					name,
					GENERIC_READ | GENERIC_WRITE,
					FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
					NULL,
					OPEN_ALWAYS,
					FILE_ATTRIBUTE_NORMAL,
					NULL);
			break;

		default:
			Debug("Invalid mode.");
			return 1;
	}
	if (file == INVALID_HANDLE_VALUE)
		return 1;
	*ret = file;

	return 0;
}

static inline int open_io_arch_(LocalRoot local,
		addr name, enum FileOutput mode, file_type *value, int *ret)
{
	LocalStack stack;
	LPCWSTR utf16;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf16));
	if (utf16 == NULL) {
		*ret = 1;
		goto finish;
	}
	if (open_io_chartype(value, utf16, mode)) {
		*ret = 1;
		goto finish;
	}
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline int read_arch(file_type file, void *pos, size_t size, size_t *ret)
{
	BOOL check;
	DWORD dsize;

	Check(0xFFFFFFFFULL < size, "size error");
	check = ReadFile(file, (LPVOID)pos, (DWORD)size, &dsize, NULL);
	if (check == 0) {
		Debug("ReadFile error");
		*ret = 0;
		return -1;
	}
	if (dsize == 0) {
		*ret = 0;
		return 1;
	}
	else {
		*ret = (size_t)dsize;
		return 0;
	}
}

static inline int write_arch(file_type file, const void *pos, size_t size, size_t *ret)
{
	BOOL check;
	DWORD dsize;

	check = WriteFile(file, (LPCVOID)pos, (DWORD)size, &dsize, NULL);
	if (check == 0) {
		Debug("WriteFile error");
		*ret = 0;
		return -1;
	}
	if (dsize == 0) {
		*ret = 0;
		return 1;
	}
	else {
		*ret = (size_t)dsize;
		return 0;
	}
}

static inline int close_arch(file_type file)
{
	if (file == fileio_input)
		return 0;
	if (file == fileio_output)
		return 0;
	if (file == fileio_error)
		return 0;

	if (CloseHandle(file) == 0) {
		Debug("CloseHandle error");
		return 1;
	}

	return 0;
}

static inline int flush_arch(file_type file)
{
	if (FlushFileBuffers(file) == 0) {
		if (file == fileio_output || file == fileio_error) {
			/* fails if hFile is a handle to the console output. */
			return 0;
		}
		Debug("FlushFileBuffers error");
		return 1;
	}

	return 0;
}

static inline int read_ready_arch(file_type file)
{
	return (file != fileio_input);
}

static int large_integer_value(PLARGE_INTEGER ptr, size_t *ret)
{
#ifdef LISP_64BIT
	*ret = (size_t)ptr->QuadPart;
	return 0;
#else
	if (ptr->HighPart != 0)
		return 1;
	*ret = (fixed)ptr->LowPart;
	return 0;
#endif
}

static inline int file_length_arch(file_type file, size_t *ret)
{
	LARGE_INTEGER size;

	if (! GetFileSizeEx(file, &size)) {
		*ret = 0;
		return 1;
	}
	if (large_integer_value(&size, ret)) {
		*ret = 0;
		return 1;
	}

	return 0;
}

static inline int file_position_arch(file_type file, size_t *ret)
{
	LARGE_INTEGER zero, pos;

	zero.QuadPart = 0ULL;
	if (! SetFilePointerEx(file, zero, &pos, FILE_CURRENT)) {
		*ret = 0;
		return 1;
	}
	if (large_integer_value(&pos, ret)) {
		*ret = 0;
		return 1;
	}

	return 0;
}

static inline int file_position_start_arch(file_type file)
{
	LARGE_INTEGER zero, pos;
	zero.QuadPart = 0ULL;
	return SetFilePointerEx(file, zero, &pos, FILE_BEGIN) == 0;
}

static inline int file_position_end_arch(file_type file)
{
	LARGE_INTEGER zero, pos;
	zero.QuadPart = 0ULL;
	return SetFilePointerEx(file, zero, &pos, FILE_END) == 0;
}

static inline int file_position_set_arch(file_type file, size_t value)
{
	LARGE_INTEGER zero, pos;
	zero.QuadPart = (LONGLONG)value;
	return SetFilePointerEx(file, zero, &pos, FILE_BEGIN) == 0;
}

#endif

#endif

#ifndef __FILE_ARCH_HEADER__
#define __FILE_ARCH_HEADER__


static inline int read_low(filestream fm, void *pos, size_t size, size_t *ret)
{
	if (fm->redirect)
		return read_low_buffering(fm, (byte *)pos, size, ret);
	else
		return read_arch(fm->file, pos, size, ret);
}

static inline int write_low(filestream fm, const void *pos, size_t size, size_t *ret)
{
	if (fm->redirect)
		return write_low_buffering(fm, (const byte *)pos, size, ret);
	else
		return write_arch(fm->file, pos, size, ret);
}

static inline int close_low(filestream fm)
{
	if (fm->redirect)
		return close_low_buffering(fm);
	else
		return close_arch(fm->file);
}

static inline int flush_low(filestream fm)
{
	if (fm->redirect)
		return flush_low_buffering(fm);
	else
		return flush_arch(fm->file);
}

static inline int read_ready_low(filestream fm)
{
	if (fm->redirect)
		return read_ready_low_buffering(fm);
	else
		return read_ready_arch(fm->file);
}

static inline int file_length_low(filestream fm, size_t *ret)
{
	if (fm->redirect)
		return file_length_low_buffering(fm, ret);
	else
		return file_length_arch(fm->file, ret);
}

static inline int file_position_low(filestream fm, size_t *ret)
{
	if (fm->redirect)
		return file_position_low_buffering(fm, ret);
	else
		return file_position_arch(fm->file, ret);
}

static inline int file_position_start_low(filestream fm)
{
	if (fm->redirect)
		return file_position_start_low_buffering(fm);
	else
		return file_position_start_arch(fm->file);
}

static inline int file_position_end_low(filestream fm)
{
	if (fm->redirect)
		return file_position_end_low_buffering(fm);
	else
		return file_position_end_arch(fm->file);
}

static inline int file_position_set_low(filestream fm, size_t pos)
{
	if (fm->redirect)
		return file_position_set_low_buffering(fm, pos);
	else
		return file_position_set_arch(fm->file, pos);
}
#endif



/*****************************************************************************
 *  filememory source
 *****************************************************************************/
#ifdef FILEMEMORY_RESTRICT
#define RESTRICTSIZE(x) (FILEMEMORY_RESTRICT <= (x)? FILEMEMORY_RESTRICT: (x))
#else
#define RESTRICTSIZE(x) (x)
#endif

static inline void init_filememory(filestream fm)
{
	fm->index = 0;
	fm->size = 0;
	fm->now = 0;
	fm->cache = 1;
	fm->readio = 0;
	fm->redirect = 0;
	fm->ungetc = 0;
	fm->system = filememory_stream;
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_auto;
	fm->encode.error = 1;
	fm->encode.create = 0;
	fm->encode.code = 0;
	fm->pos = NULL;
}

static inline void init_input_filememory(filestream fm, file_type file)
{
	init_filememory(fm);
	fm->file = file;
	fm->mode = filememory_normal;
	fm->direct = filememory_input;
}

static inline void init_output_filememory(filestream fm, file_type file)
{
	init_filememory(fm);
	fm->file = file;
	fm->mode = filememory_normal;
	fm->direct = filememory_output;
	fm->size = FILEMEMORY_SIZE;
}

static inline void init_io_filememory(filestream fm, file_type file)
{
	init_filememory(fm);
	fm->file = file;
	fm->mode = filememory_normal;
	fm->direct = filememory_io;
	fm->size = FILEMEMORY_SIZE;
}

/* low level open */
int input_unicode_filememory(filestream fm, const unicode *name, size_t size)
{
	file_type file;

	if (open_input_unicode(&file, name, size))
		return 1;
	init_input_filememory(fm, file);

	return 0;
}

int update_standard_input_filememory(filestream fm)
{
	return standard_input_arch(&(fm->file));
}

int update_standard_output_filememory(filestream fm)
{
	return standard_output_arch(&(fm->file));
}

int update_standard_error_filememory(filestream fm)
{
	return standard_error_arch(&(fm->file));
}

/* normal function */
int standard_input_filememory(filestream fm)
{
	file_type file;

	if (standard_input_arch(&file))
		return 1;
	init_input_filememory(fm, file);
	fm->system = filememory_stdin;
	fm->cache = 0;

	return 0;
}

int standard_output_filememory(filestream fm)
{
	file_type file;

	if (standard_output_arch(&file))
		return 1;
	init_output_filememory(fm, file);
	fm->system = filememory_stdout;
	fm->cache = 0;

	return 0;
}

int standard_error_filememory(filestream fm)
{
	file_type file;

	if (standard_error_arch(&file))
		return 1;
	init_output_filememory(fm, file);
	fm->system = filememory_stderr;
	fm->cache = 0;

	return 0;
}

int open_input_filememory_(LocalRoot local,
		filestream fm, addr name, int *ret)
{
	int check;
	file_type file;

	Check(! stringp(name), "name error");
	Return(open_input_arch_(local, name, &file, &check));
	if (check)
		return Result(ret, 1);
	init_input_filememory(fm, file);

	return Result(ret, 0);
}

int open_output_filememory_(LocalRoot local,
		filestream fm, addr name, enum FileOutput mode, int *ret)
{
	int check;
	file_type file;

	Check(! stringp(name), "name error");
	Return(open_output_arch_(local, name, mode, &file, &check));
	if (check)
		return Result(ret, 1);
	init_output_filememory(fm, file);

	return Result(ret, 0);
}

int open_io_filememory_(LocalRoot local,
		filestream fm, addr name, enum FileOutput mode, int *ret)
{
	int check;
	file_type file;

	Check(! stringp(name), "name error");
	Return(open_io_arch_(local, name, mode, &file, &check));
	if (check)
		return Result(ret, 1);
	init_io_filememory(fm, file);

	return Result(ret, 0);
}

void open_input_redirect_filememory_(filestream fm, addr pos)
{
	file_type file;

	Check(! memory_stream_p(pos), "stream error");
	cleartype(file);
	init_input_filememory(fm, file);
	fm->redirect = 1;
	fm->cache = getcache_memory_stream(pos);
}

void open_output_redirect_filememory_(filestream fm, addr pos)
{
	file_type file;

	Check(! memory_stream_p(pos), "stream error");
	cleartype(file);
	init_output_filememory(fm, file);
	fm->redirect = 1;
	fm->cache = getcache_memory_stream(pos);
}

void open_io_redirect_filememory_(filestream fm, addr pos)
{
	file_type file;

	Check(! memory_stream_p(pos), "stream error");
	cleartype(file);
	init_io_filememory(fm, file);
	fm->redirect = 1;
	fm->cache = getcache_memory_stream(pos);
}

int close_filememory(filestream fm)
{
	if (fm->mode == filememory_close) {
		Debug("file already closed.");
		return 1;
	}
	if (flush_filememory(fm)) {
		Debug("flush_filememory error");
		return 1;
	}
	if (close_low(fm)) {
		Debug("close_low error");
		fm->mode = filememory_error;
		return 1;
	}
	fm->mode = filememory_close;

	return 0;
}


/*
 *  read/write call
 */
#define FM_errorcheck(fm, check, name) { \
	if (check < 0) { \
		Debug(name "error"); \
		fm->mode = filememory_error; \
		return check; \
	} \
}

#define FM_end(fm) (fm->size <= fm->index)
#define FM_readforce(fm, s, r) (fm_readforce(fm, fm->buffer, s, r))
#define FM_writeforce(fm, s, r) (fm_writeforce(fm, fm->buffer, s, r))

static inline int fm_readforce(filestream fm, byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t count, rsize, diff;

	for (count = 0; count < size; count += rsize) {
		diff = size - count;
		check = read_low(fm, (void *)pos, RESTRICTSIZE(diff), &rsize);
		/* Error */
		if (check < 0) {
			Debug("read_low error");
			return check;
		}
		/* EOF */
		if (check) {
			if (count == 0)
				return check;
			break;
		}
		/* Next */
		pos += rsize;
	}
	*ret = count;

	return 0;
}

static inline int fm_writeforce(filestream fm,
		const byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t count, rsize, diff;

	for (count = 0; count < size; count += rsize) {
		diff = size - count;
		check = write_low(fm, (const void *)pos, RESTRICTSIZE(diff), &rsize);
		/* Error */
		if (check < 0) {
			Debug("write_low error");
			return check;
		}
		/* EOF */
		if (check) {
			if (count == 0)
				return check;
			break;
		}
		/* Next */
		pos += rsize;
	}
	*ret = count;

	return 0;
}

static inline int fm_readforce_nonblock(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t rsize, count;

	count = 0;
	for (;;) {
		/* ready check */
		check = read_ready_low(fm);
		if (check < 0) {
			Debug("read_ready_low error");
			return check;
		}
		if (check == 0) /* no ready */
			break;

		/* read_low */
		check = read_low(fm, pos, size, &rsize);
		if (check < 0)  {
			Debug("read_low error");
			return check;
		}
		if (check) {
			if (count == 0)
				return 1;
			break;
		}
		if (size <= rsize) {
			count += rsize;
			break;
		}
		pos += rsize;
		size -= rsize;
		count += rsize;
	}
	*ret = count;

	return 0;
}


/*
 *  flush
 */
static inline int flush_write_filememory(filestream fm)
{
	int check;
	size_t rsize;

	if (fm->index) {
		check = FM_writeforce(fm, fm->index, &rsize);
		FM_errorcheck(fm, check, "FM_writeforce");
		if (check) {
			fm->mode = filememory_end;
			return check;
		}
		fm->index = 0;
	}

	return 0;
}

static int flush_output_filememory(filestream fm)
{
	int check;

	check = flush_write_filememory(fm);
	FM_errorcheck(fm, check, "flush_write_filememory");
	if (check)
		fm->mode = filememory_end;
	flush_low(fm);

	return 0;
}

static int flush_end_filememory(filestream fm)
{
	switch (fm->direct) {
		case filememory_output:
			flush_low(fm);

		case filememory_io:
			if (! fm->readio)
				flush_low(fm);
			break;

		default:
			break;
	}

	return 0;
}

int flush_filememory(filestream fm)
{
	if (fm->mode == filememory_end)
		return flush_end_filememory(fm);

	switch (fm->direct) {
		case filememory_input:
			break;

		case filememory_output:
			return flush_output_filememory(fm);

		case filememory_io:
			if (! fm->readio)
				return flush_output_filememory(fm);
			break;

		default:
			Debug("direction error");
			return -1;
	}

	return 0;
}

static inline int flush_read_io_filememory(filestream fm)
{
	int check;

	if (fm->direct != filememory_io)
		return 0;
	if (fm->readio)
		return 0;
	check = flush_output_filememory(fm);
	if (check)
		return check;
	fm->readio = 1; /* read */
	fm->ungetc = 0;
	fm->index = 0;
	return file_position_set_low(fm, fm->now);
}

static inline int flush_write_io_filememory(filestream fm)
{
	if (fm->direct != filememory_io)
		return 0;
	if (fm->readio == 0)
		return 0;
	fm->readio = 0; /* write */
	fm->ungetc = 0;
	fm->index = 0;
	return file_position_set_low(fm, fm->now);
}


/*
 *  read
 */
static inline int fm_readnext_large(filestream fm, byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t rsize;

	check = fm_readforce(fm, pos, size, &rsize);
	FM_errorcheck(fm, check, "fm_readforce");
	if (check) {
		fm->mode = filememory_end;
		return check;
	}
	*ret = rsize;

	return 0;
}

static inline int fm_readnext_small(filestream fm, byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t rsize;

	check = FM_readforce(fm, FILEMEMORY_SIZE, &rsize);
	FM_errorcheck(fm, check, "FM_readforce");
	if (check) {
		fm->mode = filememory_end;
		return check;
	}
	if (rsize <= size) {
		memcpy(pos, fm->buffer, rsize);
		fm->index = 0;
		*ret = rsize;
	}
	else {
		memcpy(pos, fm->buffer, size);
		fm->index = size;
		fm->size = rsize;
		*ret = size;
	}

	return 0;
}

static inline int fm_readnext(filestream fm, byte *pos, size_t size, size_t *ret)
{
	if ((FILEMEMORY_SIZE / 2) < size)
		return fm_readnext_large(fm, pos, size, ret);
	else
		return fm_readnext_small(fm, pos, size, ret);
}

static inline int fm_readbuffer_call(filestream fm,
		byte *pos, size_t size, size_t *ret,
		int (*next)(filestream , byte *, size_t, size_t *))
{
	int check;
	size_t diff, result;

	/* Request size is smaller than fm.buffer. -> buffer */
	diff = fm->size - fm->index;
	if (size < diff) {
		memcpy(pos, fm->buffer + fm->index, size);
		fm->index += size;
		*ret = size;
		return 0;
	}

	/* Request size is equal to fm.buffer. -> next */
	memcpy(pos, fm->buffer + fm->index, diff);
	fm->index = 0;
	if (size == diff) {
		*ret = diff;
		return 0;
	}

	/* Request size is greater than fm.buffer. -> next */
	check = (*next)(fm, pos + diff, size - diff, &result);
	if (check < 0) {
		Debug("call next error");
		return check;
	}
	if (check) {
		*ret = diff;
		return 0; /* not EOF */
	}
	*ret = result + diff;

	return 0;
}

static inline int fm_readbuffer(filestream fm, byte *pos, size_t size, size_t *ret)
{
	return fm_readbuffer_call(fm, pos, size, ret, fm_readnext);
}

static inline int fm_readungetc_call(filestream fm,
		byte *pos, size_t size, size_t *ret,
		int (*next)(filestream , byte *, size_t, size_t *),
		int (*buffer)(filestream , byte *, size_t, size_t *))
{
	int check;
	size_t count, result;

	/* ungetc stack */
	for (count = 0; fm->ungetc && size; count++) {
		fm->ungetc--;
		*(pos++) = fm->ungetc_value[fm->ungetc];
		size--;
	}

	/* Success */
	if (size == 0) {
		*ret = count;
		return 0;
	}

	/* Read tail */
	if (fm->index == 0)
		check = (*next)(fm, pos, size, &result);
	else
		check = (*buffer)(fm, pos, size, &result);
	if (check < 0) {
		Debug("call function error");
		return check;
	}
	if (check) {
		*ret = 1;
		return 0;  /* not EOF. */
	}
	*ret = result + count;

	return 0;
}

static inline int fm_readungetc(filestream fm, byte *pos, size_t size, size_t *ret)
{
	return fm_readungetc_call(fm, pos, size, ret,
			fm_readnext,
			fm_readbuffer);
}

static int fm_readnocache(filestream fm, byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t count, result;

	/* ungetc stack */
	for (count = 0; fm->ungetc && size; count++) {
		fm->ungetc--;
		*(pos++) = fm->ungetc_value[fm->ungetc];
		size--;
	}

	/* Success */
	if (size == 0) {
		*ret = count;
		return 0;
	}

	/* Read tail */
	Check(fm->index, "index error");
	check = read_low(fm, (void *)pos, size, &result);
	if (check < 0) {
		Debug("call function error");
		return check;
	}
	if (check) {  /* EOF */
		*ret = count;
		return count == 0;
	}
	*ret = result + count;

	return 0;
}

static inline int fm_read_normal(filestream fm, byte *pos, size_t size, size_t *ret)
{
	if (! fm->cache)
		return fm_readnocache(fm, pos, size, ret);
	if (fm->ungetc)
		return fm_readungetc(fm, pos, size, ret);
	if (fm->index == 0)
		return fm_readnext(fm, pos, size, ret);
	else
		return fm_readbuffer(fm, pos, size, ret);
}

int read_filememory(filestream fm, void *dst, size_t size, size_t *ret)
{
	int check;

	if (fm->direct == filememory_output) {
		Debug("direction error");
		return -1;
	}
	if (flush_read_io_filememory(fm)) {
		Debug("flush_io error");
		return -1;
	}
	if (size == 0) {
		*ret = 0;
		return 0;
	}

	switch (fm->mode) {
		case filememory_normal:
			break;

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	/* read */
	check = fm_read_normal(fm, (byte *)dst, size, ret);
	if (check)
		return check;
	fm->now += *ret;

	return 0;
}

static inline int fm_read_normal_force(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t count, result;

	for (count = 0; count < size; count += result) {
		check = fm_read_normal(fm, pos + count, size - count, &result);
		FM_errorcheck(fm, check, "fm_read_normal");
		if (check) {
			if (count)
				break;
			fm->mode = filememory_end;
			return check;
		}
	}
	*ret = count;

	return 0;
}

int readf_filememory(filestream fm, void *dst, size_t size, size_t *ret)
{
	int check;

	if (fm->direct == filememory_output) {
		Debug("direction error");
		return -1;
	}
	if (flush_read_io_filememory(fm)) {
		Debug("flush_io error");
		return -1;
	}
	if (size == 0) {
		*ret = 0;
		return 0;
	}

	switch (fm->mode) {
		case filememory_normal:
			break;

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	/* read */
	check = fm_read_normal_force(fm, (byte *)dst, size, ret);
	if (check)
		return check;
	fm->now += *ret;

	return 0;
}


/*
 *  read-nonblock
 */
static inline int fm_readnext_nonblock_large(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t rsize;

	check = fm_readforce_nonblock(fm, pos, size, &rsize);
	FM_errorcheck(fm, check, "fm_readforce_nonblock");
	if (check) {
		fm->mode = filememory_end;
		return check;
	}
	*ret = rsize;

	return 0;
}

static inline int fm_readnext_nonblock_small(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t rsize;

	check = fm_readforce_nonblock(fm, fm->buffer, FILEMEMORY_SIZE, &rsize);
	FM_errorcheck(fm, check, "fm_readforce_nonblock");
	if (check) {
		fm->mode = filememory_end;
		return check;
	}
	if (rsize <= size) {
		memcpy(pos, fm->buffer, rsize);
		fm->index = 0;
		*ret = rsize;
	}
	else {
		memcpy(pos, fm->buffer, size);
		fm->index = size;
		fm->size = rsize;
		*ret = size;
	}

	return 0;
}

static inline int fm_readnext_nonblock(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	if ((FILEMEMORY_SIZE / 2) < size)
		return fm_readnext_nonblock_large(fm, pos, size, ret);
	else
		return fm_readnext_nonblock_small(fm, pos, size, ret);
}

static inline int fm_readbuffer_nonblock(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	return fm_readbuffer_call(fm, pos, size, ret, fm_readnext_nonblock);
}

static inline int fm_readungetc_nonblock(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	return fm_readungetc_call(fm, pos, size, ret,
			fm_readnext_nonblock,
			fm_readbuffer_nonblock);
}

static int readnocache_nonblock(filestream fm, byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t count, result;

	/* ungetc stack */
	for (count = 0; fm->ungetc && size; count++) {
		fm->ungetc--;
		*(pos++) = fm->ungetc_value[fm->ungetc];
		size--;
	}

	/* Success */
	if (size == 0) {
		*ret = count;
		return 0;
	}

	/* Read tail */
	Check(fm->index, "index error");
	check = read_ready_low(fm);
	if (check < 0) {
		Debug("read_ready_low error");
		return check;
	}
	if (check == 0) {
		/* no ready */
		result = 0;
	}
	else {
		/* ready */
		check = read_low(fm, (void *)pos, size, &result);
		if (check < 0) {
			Debug("call function error");
			return check;
		}
		if (check) {
			*ret = 1;
			return count == 0;
		}
	}
	*ret = result + count;

	return 0;
}

static inline int read_nonblock(filestream fm, byte *pos, size_t size, size_t *ret)
{
	if (! fm->cache)
		return readnocache_nonblock(fm, pos, size, ret);
	if (fm->ungetc)
		return fm_readungetc_nonblock(fm, pos, size, ret);
	if (fm->index == 0)
		return fm_readnext_nonblock(fm, pos, size, ret);
	else
		return fm_readbuffer_nonblock(fm, pos, size, ret);
}

int read_nonblock_filememory(filestream fm, void *dst, size_t size, size_t *ret)
{
	int check;

	if (fm->direct == filememory_output) {
		Debug("direction error");
		return -1;
	}
	if (flush_read_io_filememory(fm)) {
		Debug("flush_io error");
		return -1;
	}
	if (size == 0) {
		*ret = 0;
		return 0;
	}

	switch (fm->mode) {
		case filememory_normal:
			break;

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	/* read */
	check = read_nonblock(fm, (byte *)dst, size, ret);
	if (check)
		return check;
	fm->now += *ret;

	return 0;
}


/*
 *  getc
 */
static inline int fm_readbuffer_small(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	return fm_readbuffer_call(fm, pos, size, ret, fm_readnext_small);
}

static inline int fm_readungetc_small(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	return fm_readungetc_call(fm, pos, size, ret,
			fm_readnext_small,
			fm_readbuffer_small);
}

static inline int getc_normal(filestream fm, byte *pos)
{
	size_t dummy;

	if (! fm->cache)
		return fm_readnocache(fm, pos, 1, &dummy);
	if (fm->ungetc)
		return fm_readungetc_small(fm, pos, 1, &dummy);
	if (fm->index == 0)
		return fm_readnext_small(fm, pos, 1, &dummy);
	else
		return fm_readbuffer_small(fm, pos, 1, &dummy);
}

int getc_filememory(filestream fm, byte *pos)
{
	int check;

	if (fm->direct == filememory_output) {
		Debug("direction error");
		return -1;
	}
	if (flush_read_io_filememory(fm)) {
		Debug("flush_io error");
		return -1;
	}

	switch (fm->mode) {
		case filememory_normal:
			break;

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	/* read */
	check = getc_normal(fm, pos);
	if (check)
		return check;
	fm->now++;

	return 0;
}

static inline int fm_readbuffer_nonblock_small(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	return fm_readbuffer_call(fm, pos, size, ret, fm_readnext_nonblock_small);
}

static inline int fm_readungetc_nonblock_small(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	return fm_readungetc_call(fm, pos, size, ret,
			fm_readnext_nonblock_small,
			fm_readbuffer_nonblock_small);
}

static int getc_nonblock(filestream fm, byte *pos, size_t *ret)
{
	if (! fm->cache)
		return readnocache_nonblock(fm, pos, 1, ret);
	if (fm->ungetc)
		return fm_readungetc_nonblock_small(fm, pos, 1, ret);
	if (fm->index == 0)
		return fm_readnext_nonblock_small(fm, pos, 1, ret);
	else
		return fm_readbuffer_nonblock_small(fm, pos, 1, ret);
}

int getc_nonblock_filememory(filestream fm, byte *pos, size_t *ret)
{
	int check;

	if (fm->direct == filememory_output) {
		Debug("direction error");
		return -1;
	}
	if (flush_read_io_filememory(fm)) {
		Debug("flush_io error");
		return -1;
	}

	switch (fm->mode) {
		case filememory_normal:
			break;

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	/* read */
	check = getc_nonblock(fm, pos, ret);
	if (check)
		return check;
	fm->now++;

	return 0;
}

int ungetc_filememory(filestream fm, byte c)
{
	if (fm->direct == filememory_output) {
		Debug("direction error");
		return -1;
	}
	if (fm->now == 0) {
		return -1;
	}
	if (FILEMEMORY_UNGETC_SIZE <= fm->ungetc) {
		Debug("ungetc stack overflow.");
		return -1;
	}

	switch (fm->mode) {
		case filememory_normal:
			break;

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	/* unread */
	fm->ungetc_value[fm->ungetc++] = c;
	fm->now--;

	return 0;
}


/*
 *  write
 */
static int write_normal(filestream fm, const byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t index, rsize, diff;

	index = fm->index;
	/* large copy */
	if (FILEMEMORY_SIZE <= size) {
		check = flush_write_filememory(fm);
		FM_errorcheck(fm, check, "flush_write_filememory");
		if (check) { /* EOF */
			return check;
		}

		/* write from memory */
		check = fm_writeforce(fm, pos, size, &rsize);
		FM_errorcheck(fm, check, "fm_writeforce");
		if (check) {
			fm->mode = filememory_end;
			return check;
		}
		*ret = rsize;
		return 0;
	}

	/* small copy */
	if (FILEMEMORY_SIZE < (size + index)) {
		diff = FILEMEMORY_SIZE - index;
		memcpy(fm->buffer + index, pos, diff);

		check = FM_writeforce(fm, FILEMEMORY_SIZE, &rsize);
		FM_errorcheck(fm, check, "FM_writeforce");
		if (check) {
			fm->mode = filememory_end;
			return check;
		}

		rsize = size - diff;
		memcpy(fm->buffer, pos + diff, rsize);
		fm->index = rsize;
		*ret = size;
		return 0;
	}

	/* memory only */
	memcpy(fm->buffer + fm->index, pos, size);
	fm->index += size;
	*ret = size;

	return 0;
}

int write_filememory(filestream fm, const void *dst, size_t size, size_t *ret)
{
	int check;

	if (fm->direct == filememory_input) {
		Debug("direction error");
		return -1;
	}
	if (flush_write_io_filememory(fm)) {
		Debug("flush_io error");
		return -1;
	}
	if (size == 0) {
		*ret = 0;
		return 0;
	}

	switch (fm->mode) {
		case filememory_normal:
			break;

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	/* write */
	check = write_normal(fm, (byte *)dst, size, ret);
	if (check)
		return check;
	fm->now += *ret;

	return 0;
}

/* read type */
int read_s16_filememory(filestream fm, int16_t *ret)
{
	int check;
	union read_s16_union {
		byte a[2];
		int16_t v;
	} u;
	size_t size;

	check = readf_filememory(fm, (void *)u.a, 2, &size);
	if (check)
		return check;
	if (size != 2)
		return 1;

	*ret = u.v;
	return 0;
}

int read_s32_filememory(filestream fm, int32_t *ret)
{
	int check;
	union read_s32_union {
		byte a[4];
		int32_t v;
	} u;
	size_t size;

	check = readf_filememory(fm, (void *)u.a, 4, &size);
	if (check)
		return check;
	if (size != 4)
		return 1;

	*ret = u.v;
	return 0;
}

#ifdef LISP_ARCH_64BIT
int read_s64_filememory(filestream fm, int64_t *ret)
{
	int check;
	union read_s64_union {
		byte a[8];
		int64_t v;
	} u;
	size_t size;

	check = readf_filememory(fm, (void *)u.a, 8, &size);
	if (check)
		return check;
	if (size != 8)
		return 1;

	*ret = u.v;
	return 0;
}
#endif

int read_u16_filememory(filestream fm, uint16_t *ret)
{
	int check;
	union read_u16_union {
		byte a[2];
		uint16_t v;
	} u;
	size_t size;

	check = readf_filememory(fm, (void *)u.a, 2, &size);
	if (check)
		return check;
	if (size != 2)
		return 1;

	*ret = u.v;
	return 0;
}

int read_u32_filememory(filestream fm, uint32_t *ret)
{
	int check;
	union read_u32_union {
		byte a[4];
		uint32_t v;
	} u;
	size_t size;

	check = readf_filememory(fm, (void *)u.a, 4, &size);
	if (check)
		return check;
	if (size != 4)
		return 1;

	*ret = u.v;
	return 0;
}

#ifdef LISP_ARCH_64BIT
int read_u64_filememory(filestream fm, uint64_t *ret)
{
	int check;
	union read_u64_union {
		byte a[8];
		uint64_t v;
	} u;
	size_t size;

	check = readf_filememory(fm, (void *)u.a, 8, &size);
	if (check)
		return check;
	if (size != 8)
		return 1;

	*ret = u.v;
	return 0;
}
#endif

/* write type */
static int write_type_filememory(filestream fm, const void *p, size_t n)
{
	int check;
	size_t size;

	check = write_filememory(fm, p, n, &size);
	if (check)
		return check;
	if (size != n)
		return 1;

	return 0;
}

int write_s16_filememory(filestream fm, int16_t c)
{
	return write_type_filememory(fm, (const void *)&c, 2);
}

int write_s32_filememory(filestream fm, int32_t c)
{
	return write_type_filememory(fm, (const void *)&c, 4);
}

#ifdef LISP_ARCH_64BIT
int write_s64_filememory(filestream fm, int64_t c)
{
	return write_type_filememory(fm, (const void *)&c, 8);
}
#endif

int write_u16_filememory(filestream fm, uint16_t c)
{
	return write_type_filememory(fm, (const void *)&c, 2);
}

int write_u32_filememory(filestream fm, uint32_t c)
{
	return write_type_filememory(fm, (const void *)&c, 4);
}

#ifdef LISP_ARCH_64BIT
int write_u64_filememory(filestream fm, uint64_t c)
{
	return write_type_filememory(fm, (const void *)&c, 8);
}
#endif


/*
 *  putc
 */
static int fm_putcnormal(filestream fm, byte c)
{
	int check;
	size_t rsize;

	if (FILEMEMORY_SIZE <= fm->index) {
		check = FM_writeforce(fm, FILEMEMORY_SIZE, &rsize);
		FM_errorcheck(fm, check, "FM_writeforce");
		if (check) {
			fm->mode = filememory_end;
			return check;
		}
		fm->index = 0;
	}
	fm->buffer[fm->index++] = c;

	return 0;
}

int putc_filememory(filestream fm, byte c)
{
	int check;

	if (fm->direct == filememory_input) {
		Debug("direction error");
		return -1;
	}
	if (flush_write_io_filememory(fm)) {
		Debug("flush_io error");
		return -1;
	}

	switch (fm->mode) {
		case filememory_normal:
			break;

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	/* write */
	check = fm_putcnormal(fm, c);
	if (check)
		return check;
	fm->now++;

	return 0;
}

void exitpoint_filememory(filestream fm)
{
	if (fm->cache == 0)
		(void)flush_filememory(fm);
}

int end_filememory(filestream fm)
{
	if (fm->mode == filememory_end)
		return 1;
	if (fm->mode == filememory_error) {
		Debug("mode error");
		return -1;
	}

	return 0;
}

int error_filememory(filestream fm)
{
	return fm->mode == filememory_error;
}

int listen_filememory(filestream fm)
{
	int check;

	if (fm->direct != filememory_input) {
		Debug("direction error");
		return -1;
	}
	if (fm->mode == filememory_end)
		return 0;
	if (fm->mode != filememory_normal)
		return 1;
	if (fm->index == 0) {
		check = read_ready_low(fm);
#ifdef LISP_DEBUG
		if (check < 0)
			Debug("read_ready_low error");
#endif
		return check;
	}

	return 1;  /* ready */
}

int clear_input_filememory(filestream fm)
{
	switch (fm->direct) {
		case filememory_io:
			if (! fm->readio)
				return 0;
			/* FALLTHROUGH */
		case filememory_input:
			if (fm->mode == filememory_end)
				return 0;
			if (fm->mode != filememory_normal)
				return 1;
			if (fm->cache)
				return 0;
			/* clear buffer */
			fm->ungetc = 0;
			fm->index = fm->size = 0;
			return 0;

		default:
			Debug("direction error");
			return -1;
	}
}

int clear_output_filememory(filestream fm)
{
	switch (fm->direct) {
		case filememory_io:
			if (fm->readio)
				return 0;
			/* FALLTHROUGH */
		case filememory_output:
			if (fm->mode == filememory_end)
				return 0;
			if (fm->mode != filememory_normal)
				return 1;
			if (fm->cache)
				return 0;
			/* clear buffer */
			fm->index = 0;
			fm->size = FILEMEMORY_SIZE;
			return 0;

		default:
			Debug("direction error");
			return -1;
	}
}

int file_length_filememory(filestream fm, size_t *ret)
{
	if (fm->mode == filememory_normal || fm->mode == filememory_end)
		return file_length_low(fm, ret);
	else
		return 1;
}

static int file_position_input(filestream fm)
{
	return fm->direct == filememory_input ||
		(fm->direct == filememory_io && fm->readio);
}

int file_position_filememory(filestream fm, size_t *ret)
{
	int check;
	size_t size, unread;

	if (fm->mode != filememory_normal && fm->mode != filememory_end)
		return 1;
	check = file_position_low(fm, &size);
	if (check)
		return check;
	if (file_position_input(fm)) {
		/* input */
		size += fm->index;
		size -= fm->size;
		unread = (size_t)fm->ungetc;
		if (size < unread)
			return -1;
		*ret = size - unread;
	}
	else {
		/* output */
		*ret = size + fm->index;
	}

	return 0;
}

int file_position_start_filememory(filestream fm)
{
	int check;

	if (fm->mode == filememory_normal || fm->mode == filememory_end) {
		check = file_position_start_low(fm);
		if (check)
			return check;
		fm->index = 0;
		fm->ungetc = 0;
		fm->now = 0;
		return 0;
	}

	return 1;
}

int file_position_end_filememory(filestream fm)
{
	int check;
	size_t size;

	if (fm->mode == filememory_normal || fm->mode == filememory_end) {
		check = file_position_end_low(fm);
		if (check)
			return check;
		check = file_position_low(fm, &size);
		if (check)
			return check;
		fm->index = 0;
		fm->ungetc = 0;
		fm->now = size;
		return 0;
	}

	return 1;
}

int file_position_set_filememory(filestream fm, size_t pos)
{
	int check;

	if (fm->mode == filememory_normal || fm->mode == filememory_end) {
		check = file_position_set_low(fm, pos);
		if (check)
			return check;
		fm->index = 0;
		fm->ungetc = 0;
		fm->now = pos;
		return 0;
	}

	return 1;
}


/*
 *  core
 */
int readcheck_filememory(filestream fm, void *dst, size_t size)
{
	size_t check;
	return readf_filememory(fm, dst, size, &check) || size != check;
}
int writecheck_filememory(filestream fm, const void *dst, size_t size)
{
	size_t check;
	return write_filememory(fm, dst, size, &check) || size != check;
}

int readptr_filememory(filestream fm, void **ret)
{
	uintptr_t ptr;

	if (readcheck_filememory(fm, &ptr, sizeoft(uintptr_t))) {
		Debug("readaddr error: filememory");
		return 1;
	}
	if (ptr == (uintptr_t)Unbound)
		*ret = (void *)Unbound;
	else
		*ret = (void *)(heap_root + ptr);

	return 0;
}
int writeptr_filememory(filestream fm, const void *pos)
{
	uintptr_t ptr;

	if (pos == Unbound) {
		return writecheck_filememory(fm, &pos, sizeoft(addr));
	}
	ptr = (uintptr_t)pos;
	if (ptr < (uintptr_t)heap_root || (uintptr_t)heap_front < ptr) {
		Debug("writeaddr error: out of range.");
		return 1;
	}
	ptr -= (uintptr_t)heap_root;

	return writecheck_filememory(fm, &ptr, sizeoft(uintptr_t));
}

int readaddr_filememory(filestream fm, addr *ret)
{
	return readptr_filememory(fm, (void **)ret);
}
int writeaddr_filememory(filestream fm, addr pos)
{
	uintptr_t ptr;

	if (pos == Unbound) {
		return writecheck_filememory(fm, &pos, sizeoft(addr));
	}
	if (GetStatusDynamic(pos)) {
		Debug("writeaddr error: dynamic object.");
		return 1;
	}
	ptr = (uintptr_t)pos;
	if (ptr < (uintptr_t)heap_root || (uintptr_t)heap_front < ptr) {
		Debug("writeaddr error: out of range.");
		return 1;
	}
	ptr -= (uintptr_t)heap_root;

	return writecheck_filememory(fm, &ptr, sizeoft(uintptr_t));
}

int readsize_filememory(filestream fm, size_t *ret)
{
	return readcheck_filememory(fm, ret, sizeoft(size_t));
}

int writesize_filememory(filestream fm, size_t pos)
{
	return writecheck_filememory(fm, &pos, sizeoft(size_t));
}


/************************************************************
 *  file_open.c
 ************************************************************/

/*
 *  input
 */
static inline int inputstream_(Execute ptr,
		addr *ret, addr file, enum StreamType type)
{
	int check;
	addr pos, name;
	filestream fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	SetPathnameStream(pos, file);
	fm = PtrFileMemory(pos);

	/* stream */
	if (memory_stream_p(file)) {
		open_input_redirect_filememory_(fm, file);
		return Result(ret, pos);
	}

	/* pathname */
	Return(name_physical_local_(ptr, file, &name));
	Return(open_input_filememory_(ptr->local, fm, name, &check));
	if (check)
		return Result(ret, NULL);

	return Result(ret, pos);
}

int open_input_binary_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_binary;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_unsigned16_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned16;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_unsigned32_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned32;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_signed8_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed8;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_signed16_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed16;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_signed32_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed32;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

#ifdef LISP_64BIT
int open_input_unsigned64_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned64;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_signed64_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed64;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}
#endif

int open_input_ascii_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_ascii;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

static void open_input_close_filememory(addr file)
{
	filestream fm;
	addr prev, mem;

	fm = PtrFileMemory(file);
	if (fm->redirect == 0) {
		close_filememory(fm);
		return;
	}

	/* redirect */
	GetPathnameStream(file, &mem);
	Check(! memory_stream_p(mem), "type error");
	prev = fm->pos;
	fm->pos = mem;
	close_filememory(fm);
	fm->pos = prev;
}

int open_input_utf8_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom8_encode(file);
	if (check < 0) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_auto;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf8bom_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom8_encode(file);
	if (check < 0 || check == 0) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf16_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom16_encode(file);
	if (check < 0) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	if (check == 1)
		fm->encode.type = EncodeType_utf16le;
	else
		fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_auto;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf16le_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom16_encode(file);
	if (check != 0) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf16be_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom16_encode(file);
	if (check != 0) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf16lebom_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom16_encode(file);
	if (check != 1) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf16bebom_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom16_encode(file);
	if (check != 2) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf32_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom32_encode(file);
	if (check < 0) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	if (check == 1)
		fm->encode.type = EncodeType_utf32le;
	else
		fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_auto;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf32le_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom32_encode(file);
	if (check != 0) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf32be_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom32_encode(file);
	if (check != 0) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf32lebom_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom32_encode(file);
	if (check != 1) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf32bebom_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom32_encode(file);
	if (check != 2) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_stream_(Execute ptr, addr *ret, addr file, addr format)
{
	enum Stream_Open_External type;

	Return(open_external_format_(ptr, format, &type));
	switch (type) {
		case Stream_Open_External_Default:
		case Stream_Open_External_Utf8:
			return open_input_utf8_stream_(ptr, ret, file);

		case Stream_Open_External_Utf8Bom:
			return open_input_utf8bom_stream_(ptr, ret, file);

		case Stream_Open_External_Ascii:
			return open_input_ascii_stream_(ptr, ret, file);

		case Stream_Open_External_Utf16:
			return open_input_utf16_stream_(ptr, ret, file);

		case Stream_Open_External_Utf16Le:
			return open_input_utf16le_stream_(ptr, ret, file);

		case Stream_Open_External_Utf16Be:
			return open_input_utf16be_stream_(ptr, ret, file);

		case Stream_Open_External_Utf16LeBom:
			return open_input_utf16lebom_stream_(ptr, ret, file);

		case Stream_Open_External_Utf16BeBom:
			return open_input_utf16bebom_stream_(ptr, ret, file);

		case Stream_Open_External_Utf32:
			return open_input_utf32_stream_(ptr, ret, file);

		case Stream_Open_External_Utf32Le:
			return open_input_utf32le_stream_(ptr, ret, file);

		case Stream_Open_External_Utf32Be:
			return open_input_utf32be_stream_(ptr, ret, file);

		case Stream_Open_External_Utf32LeBom:
			return open_input_utf32lebom_stream_(ptr, ret, file);

		case Stream_Open_External_Utf32BeBom:
			return open_input_utf32bebom_stream_(ptr, ret, file);

		case Stream_Open_External_Error:
		default:
			*ret = Nil;
			return fmte_("Invalid external-format ~S.", format, NULL);
	};
}

int open_input_stream_error_(Execute ptr, addr *ret, addr file, addr format)
{
	addr pos;

	Return(open_input_stream_(ptr, &pos, file, format));
	if (pos == NULL) {
		*ret = Nil;
		return call_simple_file_error_va_(ptr, file,
				"Cannot open file, ~S.", file, NULL);
	}

	return Result(ret, pos);
}


/*
 *  output
 */
static int open_redirect_supersede_(addr file, enum FileOutput mode)
{
	int check;

	switch (mode) {
		case FileOutput_supersede:
			return clear_memory_stream_(file);

		case FileOutput_append:
			Return(file_position_end_stream_(file, &check));
			break;

		case FileOutput_overwrite:
			Return(file_position_start_stream_(file, &check));
			break;

		default:
			return clear_memory_stream_(file);
	}
	if (check)
		return fmte_("Cannot set a file-position ~S.", file, NULL);

	return 0;
}

static inline int outputstream_(Execute ptr,
		addr *ret, addr file, enum StreamType type, enum FileOutput mode)
{
	int check;
	addr pos, name;
	filestream fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	SetPathnameStream(pos, file);
	fm = PtrFileMemory(pos);

	/* stream */
	if (memory_stream_p(file)) {
		Return(open_redirect_supersede_(file, mode));
		open_output_redirect_filememory_(fm, file);
		return Result(ret, pos);
	}

	/* pathname */
	Return(name_physical_local_(ptr, file, &name));
	Return(open_output_filememory_(ptr->local, fm, name, mode, &check));
	if (check)
		return Result(ret, NULL);

	return Result(ret, pos);
}

int open_output_binary_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_binary;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_output_unsigned16_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned16;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_output_unsigned32_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned32;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_output_signed8_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed8;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_output_signed16_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed16;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_output_signed32_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed32;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

#ifdef LISP_64BIT
int open_output_unsigned64_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned64;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_output_signed64_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed64;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}
#endif

int open_output_ascii_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_ascii;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_output_utf8_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode, int bomp)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	if (bomp) {
		Return(writebom_encode_(file));
	}

	return Result(ret, file);
}

int open_output_utf16le_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode, int bomp)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	if (bomp) {
		Return(writebom_encode_(file));
	}

	return Result(ret, file);
}

int open_output_utf16be_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode, int bomp)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	if (bomp) {
		Return(writebom_encode_(file));
	}

	return Result(ret, file);
}

int open_output_utf32le_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode, int bomp)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	if (bomp) {
		Return(writebom_encode_(file));
	}

	return Result(ret, file);
}

int open_output_utf32be_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode, int bomp)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	if (bomp) {
		Return(writebom_encode_(file));
	}

	return Result(ret, file);
}


/*
 *  io
 */
static inline int iostream_(Execute ptr,
		addr *ret, addr file, enum StreamType type, enum FileOutput mode)
{
	int check;
	addr pos, name;
	filestream fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	SetPathnameStream(pos, file);
	fm = PtrFileMemory(pos);

	/* stream */
	if (memory_stream_p(file)) {
		Return(open_redirect_supersede_(file, mode));
		open_io_redirect_filememory_(fm, file);
		return Result(ret, pos);
	}

	/* pathname */
	Return(name_physical_local_(ptr, file, &name));
	Return(open_io_filememory_(ptr->local, fm, name, mode, &check));
	if (check)
		return Result(ret, NULL);

	return Result(ret, pos);
}

int open_io_binary_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_binary;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_unsigned16_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned16;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_unsigned32_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned32;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_signed8_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed8;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_signed16_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed16;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_signed32_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed32;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

#ifdef LISP_64BIT
int open_io_unsigned64_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned64;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_signed64_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed64;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}
#endif

int open_io_ascii_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_ascii;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_utf8_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_utf8bom_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);
	Return(writebom_encode_(file));

	return Result(ret, file);
}

int open_io_utf16_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_utf16le_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_utf16be_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_utf16lebom_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	Return(writebom_encode_(file));

	return Result(ret, file);
}

int open_io_utf16bebom_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	Return(writebom_encode_(file));

	return Result(ret, file);
}

int open_io_utf32_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_utf32le_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_utf32be_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_utf32lebom_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	Return(writebom_encode_(file));

	return Result(ret, file);
}

int open_io_utf32bebom_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	Return(writebom_encode_(file));

	return Result(ret, file);
}


/*
 *  probe
 */
static inline int probestream_(Execute ptr, addr *ret, addr file, enum StreamType type)
{
	int check;
	addr pos, name;
	filestream fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	SetPathnameStream(pos, file);
	fm = PtrFileMemory(pos);

	/* stream */
	if (memory_stream_p(file)) {
		open_input_redirect_filememory_(fm, file);
		return Result(ret, pos);
	}

	/* pathname */
	Return(name_physical_local_(ptr, file, &name));
	Return(open_input_filememory_(ptr->local, fm, name, &check));
	if (check)
		return Result(ret, NULL);
	if (close_filememory(fm))
		return Result(ret, NULL);

	return Result(ret, pos);
}

int open_probe_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(probestream_(ptr, &file, file, StreamType_Probe));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_ascii;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	/* close */
	fm->mode = filememory_close;
	force_close_stream(file);

	return Result(ret, file);
}


/************************************************************
 *  files.c
 ************************************************************/

#if defined(LISP_UNIX)
/*
 *  files-unix
 */
#include <dirent.h>
#include <pwd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

/*
 *  directory
 */
struct directory_struct {
	Execute ptr;
	LocalRoot local;
	addr pos, list, front, root;
};

static int init_directory_struct_(Execute ptr, struct directory_struct *str, addr pos)
{
	clearpoint(str);
	str->ptr = ptr;
	str->local = ptr->local;
	str->list = str->root = str->front = Nil;
	return pathname_designator_heap_(ptr, pos, &str->pos);
}

static int make_list_directory_pathname_(struct directory_struct *str,
		addr *ret, addr list)
{
	LocalRoot local;
	addr pos, one, version;

	local = str->local;
	pos = str->pos;
	make_pathname_alloc(local, &one, RefLogicalPathname(pos));
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_HOST, one);
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_DEVICE, one);
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_NAME, one);
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_TYPE, one);
	/* directory */
	reverse_list_local_unsafe(local, &list, list);
	copylocal_object(local, &list, list);
	SetDirectoryPathname(one, list);
	/* version */
	GetConst(KEYWORD_UNSPECIFIC, &version);
	SetVersionPathname(one, version);
	/* result */
	return physical_pathname_local_(str->ptr, one, ret);
}

static int make_directory_pathname_(struct directory_struct *str, addr *ret)
{
	return make_list_directory_pathname_(str, ret, str->list);
}

static int opendir_files(LocalRoot local, addr pos, DIR **ret)
{
	addr name;
	LocalStack stack;
	const char *clang;
	DIR *dir;

	push_local(local, &stack);
	Return(directory_name_pathname_local_(local, pos, &pos));
	Return(UTF8_buffer_clang_(local, &name, pos));
	if (name == Unbound) {
		*ret = NULL;
		return call_simple_file_error_va_(NULL, pos,
				"Cannot convert ~S to UTF-8 string.", pos, NULL);
	}
	clang = (const char *)posbodyr(name);
	dir = opendir(clang);
	rollback_local(local, stack);

	return Result(ret, dir);
}

static void merge_directory_files(LocalRoot local, addr path, addr defaults)
{
	copylocal_pathname_array(local, defaults, PATHNAME_INDEX_HOST, path);
	copylocal_pathname_array(local, defaults, PATHNAME_INDEX_DEVICE, path);
	copylocal_pathname_array(local, defaults, PATHNAME_INDEX_DIRECTORY, path);
}

static int directoryp_directory_files(Execute ptr, addr file, int *ret)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	const char *body;
	addr pos;
	struct stat st;

	local = ptr->local;
	push_local(local, &stack);
	Return(name_pathname_local_(ptr, file, &pos));
	Return(UTF8_buffer_clang_(local, &pos, pos));
	if (pos == Unbound) {
		*ret = 0;
		return call_simple_file_error_va_(NULL, file,
				"Cannot convert ~S to UTF-8 string.", file, NULL);
	}
	body = (const char *)posbodyr(pos);
	check = (! lstat(body, &st)) && S_ISDIR(st.st_mode);
	rollback_local(local, stack);

	return Result(ret, check);
}

static int files_path_directory_files(struct directory_struct *str,
		addr path, addr name, addr base, addr *ret)
{
	int check;
	addr list;
	LocalRoot local;

	Return(directoryp_directory_files(str->ptr, path, &check));
	if (check) {
		local = str->local;
		copy_list_local_unsafe(local, &list, str->list);
		cons_local(local, &list, name, list);
		Return(make_list_directory_pathname_(str, &path, list));
		SetNamePathname(path, Nil);
		SetTypePathname(path, Nil);
	}

	return Result(ret, path);
}

static int files_name_directory_files(struct directory_struct *str,
		addr base, addr name)
{
	int check;
	Execute ptr;
	addr path;

	ptr = str->ptr;
	Return(pathname_designator_local_(ptr, name, &path));
	merge_directory_files(str->local, path, base);
	Return(wildcard_pathname_(path, str->pos, 1, &check));
	if (check) {
		/* push heap */
		Return(files_path_directory_files(str, path, name, base, &path));
		Return(pathname_designator_heap_(ptr, path, &path));
		cons_heap(&str->root, path, str->root);
	}

	return 0;
}

static int files_push_directory_files(struct directory_struct *str,
		addr base, const char *name)
{
	LocalRoot local;
	LocalStack stack;
	addr path;

	if (strcmp(name, ".") == 0)
		return 0;
	if (strcmp(name, "..") == 0)
		return 0;
	local = str->local;
	push_local(local, &stack);
	Return(string8_null_local_(local, &path, name));
	Return(files_name_directory_files(str, base, path));
	rollback_local(local, stack);

	return 0;
}

static int files_directory_files(struct directory_struct *str, addr base)
{
	DIR *dir;
	const char *name;
	struct dirent *entry;

	Return(opendir_files(str->local, base, &dir));
	if (dir == NULL)
		return 0;
	for (;;) {
		/* The readdir_r() interface is deprecated. */
		entry = readdir(dir);
		if (entry == NULL)
			break;
		name = entry->d_name;
		Return(files_push_directory_files(str, base, name));
	}
	if (closedir(dir)) {
		return call_simple_file_error_va_(NULL, base, "closedir() error.", NULL);
	}

	return 0;
}

static int file_directory_files(struct directory_struct *str)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	Return(make_directory_pathname_(str, &base));
	Return(files_directory_files(str, base));
	rollback_local(str->local, stack);

	return 0;
}

static int wild_check_directory_files(struct directory_struct *str, addr name, int *ret)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	const char *ptr;
	addr pos, value;
	struct stat st;

	local = str->local;
	push_local(local, &stack);
	copy_list_local_unsafe(local, &pos, str->list);
	cons_local(local, &pos, name, pos);
	Return(make_list_directory_pathname_(str, &pos, pos));
	Return(directory_name_pathname_local_(local, pos, &pos));
	Return(UTF8_buffer_clang_(local, &value, pos));
	if (value == Unbound) {
		return call_simple_file_error_va_(NULL, pos,
				"Cannot convert ~S to UTF-8 string.", pos, NULL);
	}
	ptr = (const char *)posbodyr(value);
	check = (! lstat(ptr, &st)) && S_ISDIR(st.st_mode);
	rollback_local(local, stack);

	return Result(ret, check);
}

static int loop_directory_files(struct directory_struct *str);
static int wild_push_directory_files(struct directory_struct *str, const char *name)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr path, root, list, front;

	if (strcmp(name, ".") == 0)
		return 0;
	if (strcmp(name, "..") == 0)
		return 0;
	local = str->local;
	push_local(local, &stack);
	Return(string8_null_local_(local, &path, name));
	/* directory check */
	Return(wild_check_directory_files(str, path, &check));
	if (check) {
		/* backup list */
		list = str->list;
		front = str->front;
		/* new list */
		copy_list_local_unsafe(local, &root, list);
		cons_local(local, &root, path, root);
		str->list = root;
		/* find */
		Return(loop_directory_files(str));
		/* rollback list */
		str->list = list;
		str->front = front;
	}
	/* rollback local */
	rollback_local(local, stack);

	return 0;
}

static int wild_file_directory_files(struct directory_struct *str, addr base)
{
	DIR *dir;
	const char *name;
	struct dirent *entry;

	Return(opendir_files(str->local, base, &dir));
	if (dir == NULL)
		return 0;
	for (;;) {
		/* The readdir_r() interface is deprecated. */
		entry = readdir(dir);
		if (entry == NULL)
			break;
		name = entry->d_name;
		Return(wild_push_directory_files(str, name));
	}
	if (closedir(dir)) {
		return call_simple_file_error_va_(NULL, base, "closedir() error.", NULL);
	}

	return 0;
}

static int wild_directory_files(struct directory_struct *str)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	Return(make_directory_pathname_(str, &base));
	Return(wild_file_directory_files(str, base));
	rollback_local(str->local, stack);

	return 0;
}

static int inferiors_file_directory_files(struct directory_struct *str, addr name)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	Return(make_directory_pathname_(str, &base));
	Return(files_name_directory_files(str, base, name));
	rollback_local(str->local, stack);

	return 0;
}

static int inferiors_directory_files(struct directory_struct *str);
static int inferiors_push_directory_files(struct directory_struct *str,
		const char *name)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr path, root, list, front;

	if (strcmp(name, ".") == 0)
		return 0;
	if (strcmp(name, "..") == 0)
		return 0;
	local = str->local;
	push_local(local, &stack);
	Return(string8_null_local_(local, &path, name));
	Return(inferiors_file_directory_files(str, path));
	/* directory check */
	Return(wild_check_directory_files(str, path, &check));
	if (check) {
		/* backup list */
		list = str->list;
		front = str->front;
		/* new list */
		copy_list_local_unsafe(local, &root, list);
		cons_local(local, &root, path, root);
		str->list = root;
		/* find */
		Return(inferiors_directory_files(str));
		/* rollback list */
		str->list = list;
		str->front = front;
	}
	/* rollback local */
	rollback_local(local, stack);

	return 0;
}

static int inferiors_find_directory_files(struct directory_struct *str, addr base)
{
	DIR *dir;
	const char *name;
	struct dirent *entry;

	Return(opendir_files(str->local, base, &dir));
	if (dir == NULL)
		return 0;
	for (;;) {
		/* The readdir_r() interface is deprecated. */
		entry = readdir(dir);
		if (entry == NULL)
			break;
		name = entry->d_name;
		Return(inferiors_push_directory_files(str, name));
	}
	if (closedir(dir)) {
		return call_simple_file_error_va_(NULL, base, "closedir() error.", NULL);
	}

	return 0;
}

static int inferiors_directory_files(struct directory_struct *str)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	Return(make_directory_pathname_(str, &base));
	Return(inferiors_find_directory_files(str, base));
	rollback_local(str->local, stack);

	return 0;
}

static int loop_directory_files(struct directory_struct *str)
{
	addr name, check;

	/* file */
	if (str->front == Nil)
		return file_directory_files(str);

	/* wild */
	GetCons(str->front, &name, &str->front);
	GetConst(KEYWORD_WILD, &check);
	if (name == check)
		return wild_directory_files(str);

	/* wild-inferiors */
	GetConst(KEYWORD_WILD_INFERIORS, &check);
	if (name == check)
		return inferiors_directory_files(str);

	/* next */
	cons_local(str->local, &str->list, name, str->list);
	return loop_directory_files(str);
}

int directory_files_(Execute ptr, addr *ret, addr pos)
{
	addr list, x, y;
	struct directory_struct str;

	Return(init_directory_struct_(ptr, &str, pos));
	GetDirectoryPathname(str.pos, &list);
	if (list == Nil) {
		GetConst(KEYWORD_RELATIVE, &x);
		strvect_char_heap(&y, ".");
		list_heap(&list, x, y, NULL);
	}
	str.front = list;
	Return(loop_directory_files(&str));
	return Result(ret, str.root);
}


/*
 *  probe-file
 */
static int probe_file_boolean(const char *file)
{
	struct stat st;
	return lstat(file, &st) == 0;
}

static int probe_file_run_files(Execute ptr, addr *ret, addr pos)
{
	int check;
	addr value, name;
	const char *str;

	/* wildcard */
	Return(pathname_designator_heap_(ptr, pos, &pos));
	Return(wild_pathname_boolean_(pos, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot probe-file the wildcard pathname ~S.", pos, NULL);
	}

	/* filename */
	Return(truename_files_(ptr, pos, &pos, 0));
	if (pos == Nil)
		return Result(ret, Nil);

	/* check */
	Return(name_pathname_local_(ptr, pos, &name));
	Return(UTF8_buffer_clang_(ptr->local, &value, name));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot decode UTF-8 string ~S.", name, NULL);
	}
	str = (const char *)posbodyr(value);
	*ret = probe_file_boolean(str)? pos: Nil;

	return 0;
}

int probe_file_files_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(probe_file_run_files(ptr, ret, pos));
	rollback_local(local, stack);

	return 0;
}


/*
 *  ensure-directories-exist
 */
static int ensure_directires_exist_wild_files_(addr pos, int *ret)
{
	int check;
	addr field;

	/* directory */
	GetConst(KEYWORD_DIRECTORY, &field);
	Return(wild_pathname_boolean_(pos, field, &check));
	if (check)
		return Result(ret, 1);

	/* host */
	GetConst(KEYWORD_HOST, &field);
	Return(wild_pathname_boolean_(pos, field, &check));
	if (check)
		return Result(ret, 1);

	/* device */
	GetConst(KEYWORD_DEVICE, &field);
	Return(wild_pathname_boolean_(pos, field, &check));
	if (check)
		return Result(ret, 1);

	return Result(ret, 0);
}

static void merge_directory_pathname(LocalRoot local, addr *ret, addr pos, addr value)
{
	addr one;

	make_pathname_alloc(local, &one, RefLogicalPathname(pos));
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_HOST, one);
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_DEVICE, one);
	SetDirectoryPathname(one, value);
	*ret = one;
}

static int ensure_directories_exist_run_files(Execute ptr,
		addr *ret, addr pos, int verbose)
{
	LocalRoot local;
	LocalStack stack;
	const char *str;
	addr list, value, root, temp, result;
	mode_t mode;
	struct stat st;

	GetDirectoryPathname(pos, &list);
	if (! consp_getcons(list, &value, &list)) {
		return call_simple_file_error_va_(ptr, pos,
				"Invalid pathname directory ~S.", pos, NULL);
	}
	if (! consp(list)) {
		return call_simple_file_error_va_(ptr, pos,
				"Invalid pathname directory ~S.", pos, NULL);
	}
	result = Nil;
	local = ptr->local;
	conscar_local(local, &root, value);
	while (list != Nil) {
		GetCons(list, &value, &list);
		cons_local(local, &root, value, root);
		push_local(local, &stack);
		reverse_list_local_unsafe(local, &temp, root);
		merge_directory_pathname(local, &temp, pos, temp);
		/* directory check */
		Return(directory_name_pathname_local_(local, temp, &temp));
		Return(UTF8_buffer_clang_(local, &value, temp));
		if (value == Unbound) {
			return call_simple_file_error_va_(ptr, pos,
					"Cannot decode UTF-8 string ~S.", pos, NULL);
		}
		str = (const char *)posbodyr(value);
		/* already exist */
		if (! lstat(str, &st)) {
			if (! S_ISDIR(st.st_mode)) {
				return call_simple_file_error_va_(ptr, pos,
						"Cannot make directory ~S.", pos, NULL);
			}
			rollback_local(local, stack);
			continue;
		}
		/* mkdir 0x755 */
		mode = S_IRUSR | S_IWUSR | S_IXUSR |
			S_IRGRP | S_IXGRP |
			S_IROTH | S_IXOTH;
		if (mkdir(str, mode)) {
			return call_simple_file_error_va_(ptr, pos,
					"Cannot make directory ~S.", pos, NULL);
		}
		result = T;
		/* verbose */
		if (verbose) {
			Return(format_stdout_(ptr, "~&Creating directory: ~S~%", temp, NULL));
		}
		/* continue */
		rollback_local(local, stack);
	}

	return Result(ret, result);
}

int ensure_directories_exist_files_(Execute ptr,
		addr *ret1, addr *ret2, addr pos, int verbose)
{
	int check;
	LocalRoot local;
	LocalStack stack;

	/* filename */
	Return(physical_pathname_heap_(ptr, pos, &pos));
	/* wildcard */
	Return(ensure_directires_exist_wild_files_(pos, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot ENSURE-DIRECTIRIS-EXIST the wildcard pathname ~S.",
				pos, NULL);
	}
	/* loop */
	local = ptr->local;
	push_local(local, &stack);
	Return(ensure_directories_exist_run_files(ptr, ret2, pos, verbose));
	rollback_local(local, stack);
	/* result */
	return Result(ret1, pos);
}


/*
 *  file-author
 */
static int file_author_run_files(Execute ptr, addr *ret, addr pos)
{
	int check;
	LocalRoot local;
	char *str;
	addr value;
	long size;
	struct stat st;
	struct passwd pw, *ppw;

	/* filename */
	if (stringp(pos)) {
		Return(physical_pathname_local_(ptr, pos, &pos));
	}
	else {
		Return(physical_pathname_heap_(ptr, pos, &pos));
	}

	/* wildcard */
	Return(wild_pathname_boolean_(pos, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot file-author the wildcard pathname ~S.", pos, NULL);
	}
	/* file-author */
	local = ptr->local;
	Return(name_pathname_local_(ptr, pos, &value));
	if (UTF8_buffer_clang_(local, &value, value)) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot decode UTF-8 string ~S.", pos, NULL);
	}
	str = (char *)posbodyr(value);
	if (lstat(str, &st)) {
		return call_simple_file_error_va_(ptr, pos,
				"The file ~S is not exist.", pos, NULL);
	}
	size = sysconf(_SC_GETPW_R_SIZE_MAX);
	if (size < 0)
		size = 0x010000;
	str = (char *)lowlevel_local(local, (size_t)size);
	if (getpwuid_r(st.st_uid, &pw, str, (size_t)size, &ppw) || ppw == NULL)
		return Result(ret, Nil);
	else
		return string8_null_heap_(ret, ppw->pw_name);
}

int file_author_files_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(file_author_run_files(ptr, ret, pos));
	rollback_local(local, stack);

	return 0;
}


/*
 *  file-write-date
 */
static int file_write_date_run_files(Execute ptr, addr *ret, addr pos)
{
	int check;
	LocalRoot local;
	char *str;
	addr value, symbol;
	struct stat st;

	/* filename */
	if (stringp(pos)) {
		Return(physical_pathname_local_(ptr, pos, &pos));
	}
	else {
		Return(physical_pathname_heap_(ptr, pos, &pos));
	}

	/* wildcard */
	Return(wild_pathname_boolean_(pos, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot file-write-date the wildcard pathname ~S.", pos, NULL);
	}

	/* file-author */
	local = ptr->local;
	Return(name_pathname_local_(ptr, pos, &value));
	Return(UTF8_buffer_clang_(local, &value, value));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot decode UTF-8 string ~S.", pos, NULL);
	}
	str = (char *)posbodyr(value);
	if (lstat(str, &st)) {
		return call_simple_file_error_va_(ptr, pos,
				"The file ~S is not exist.", pos, NULL);
	}
	value = intsizeh((size_t)st.st_mtime);
	GetConst(SYSTEM_TIME1970, &symbol);
	GetValueSymbol(symbol, &symbol);
	Check(symbol == Unbound, "Unbound error, (must run build_pathnames).");
	Return(plus_ii_real_common_(local, symbol, value, ret));

	return 0;
}

int file_write_date_files_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(file_write_date_run_files(ptr, ret, pos));
	rollback_local(local, stack);

	return 0;
}


/*
 *  rename-file
 */
static int rename_file_run_files(Execute ptr,
		addr *ret1, addr *ret2, addr *ret3, addr pos, addr to)
{
	int check;
	LocalRoot local;
	addr file, from, value, true1, true2;
	const char *str1, *str2;

	Return(pathname_designator_heap_(ptr, pos, &file));
	Return(physical_pathname_heap_(ptr, file, &from));
	Return(physical_pathname_heap_(ptr, to, &to));
	Return(truename_files_(ptr, from, &true1, 0));
	Return(wild_pathname_boolean_(from, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, from,
				"Cannot rename wildcard pathname from ~S", from, NULL);
	}
	Return(wild_pathname_boolean_(to, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, to,
				"Cannot rename wildcard pathname to ~S", to, NULL);
	}
	/* filename */
	local = ptr->local;
	Return(name_pathname_local_(ptr, from, &value));
	Return(UTF8_buffer_clang_(local, &value, value));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, from,
				"Cannot decode UTF-8 string ~S.", from, NULL);
	}
	str1 = (const char *)posbodyr(value);
	Return(name_pathname_local_(ptr, to, &value));
	Return(UTF8_buffer_clang_(local, &value, value));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, to,
				"Cannot decode UTF-8 string ~S.", to, NULL);
	}
	str2 = (const char *)posbodyr(value);
	/* check */
	if (probe_file_boolean(str2)) {
		return call_simple_file_error_va_(ptr, to,
				"The file ~S is already exist.", to, NULL);
	}
	/* rename */
	if (rename(str1, str2)) {
		return call_simple_file_error_va_(ptr, to,
				"Cannot rename ~S to ~S.", from, to, NULL);
	}
	/* stream */
	if (streamp(pos))
		SetPathnameStream(pos, to);
	/* result */
	Return(truename_files_(ptr, to, &true2, 0));
	*ret1 = to;
	*ret2 = true1;
	*ret3 = true2;

	return 0;
}

int rename_file_files_(Execute ptr,
		addr *ret1, addr *ret2, addr *ret3, addr file, addr to)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(rename_file_run_files(ptr, ret1, ret2, ret3, file, to));
	rollback_local(local, stack);

	return 0;
}


/*
 *  delete-file
 */
static int delete_file_run_files(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	LocalRoot local;
	addr file, value;
	const char *str;

	Return(physical_pathname_heap_(ptr, pos, &file));
	Return(wild_pathname_boolean_(file, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot delete wildcard pathname ~S", pos, NULL);
	}
	if (! pathname_file_p(file)) {
		if (errorp) {
			return call_simple_file_error_va_(ptr, pos,
					"The argument ~S is not a file.", pos, NULL);
		}
		return Result(ret, 0);
	}
	/* filename */
	local = ptr->local;
	Return(name_pathname_local_(ptr, file, &value));
	Return(UTF8_buffer_clang_(local, &value, value));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, file,
				"Cannot decode UTF-8 string ~S.", file, NULL);
	}
	str = (const char *)posbodyr(value);
	/* delete */
	if (unlink(str)) {
		if (errorp) {
			return call_simple_file_error_va_(ptr, pos,
					"Cannot delete ~S.", file, NULL);
		}
		return Result(ret, 0);
	}
	/* stream */
	if (streamp(pos)) {
		Return(close_stream_(pos, NULL));
	}

	return Result(ret, 1);
}

static int delete_file_errorp(Execute ptr, addr pos, int errorp, int *ret)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(delete_file_run_files(ptr, pos, errorp, ret));
	rollback_local(local, stack);

	return 0;
}

int delete_file_files_(Execute ptr, addr pos)
{
	int check;
	return delete_file_errorp(ptr, pos, 1, &check);
}

int remove_file_common_(Execute ptr, addr pos, int errorp, int *ret)
{
	return delete_file_errorp(ptr, pos, errorp, ret);
}


/*
 *  remove-directory
 */
int remove_directory_common_(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	LocalRoot local;
	addr file, value;
	const char *str;

	Return(physical_pathname_heap_(ptr, pos, &file));
	Return(wild_pathname_boolean_(file, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot delete wildcard pathname ~S", pos, NULL);
	}
	if (! pathname_directory_p(file)) {
		if (errorp) {
			return call_simple_file_error_va_(ptr, pos,
					"The argument ~S is not a directory.", pos, NULL);
		}
		return Result(ret, 0);
	}
	/* filename */
	local = ptr->local;
	Return(name_pathname_local_(ptr, file, &value));
	Return(UTF8_buffer_clang_(local, &value, value));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, file,
				"Cannot decode UTF-8 string ~S.", file, NULL);
	}
	str = (const char *)posbodyr(value);
	/* delete */
	if (rmdir(str)) {
		if (errorp) {
			return call_simple_file_error_va_(ptr, pos,
					"Cannot delete ~S.", file, NULL);
		}
		return Result(ret, 0);
	}
	/* stream */
	if (streamp(pos)) {
		Return(close_stream_(pos, NULL));
	}

	return Result(ret, 1);
}


/*
 *  truename
 */
int truename_files_(Execute ptr, addr file, addr *ret, int errorp)
{
	int check;
	addr pos;
	const unicode *u;
	char *str;
	size_t s32, s8;
	LocalRoot local;
	LocalStack stack;

	/* wild-check */
	Return(physical_pathname_heap_(ptr, file, &pos));
	Return(wild_pathname_boolean_(pos, Nil, &check));
	if (check) {
		if (! errorp)
			goto error_nil;
		return call_simple_file_error_va_(ptr, file,
				"TRUENAME don't allow the wildcard filename ~S.", file, NULL);
	}
	Return(name_pathname_heap_(ptr, pos, &pos));

	/* realpath */
	strvect_posbodylen(pos, &u, &s32);
	if (UTF32_length_utf8(u, s32, &s8)) {
		if (! errorp)
			goto error_nil;
		return call_simple_file_error_va_(ptr, file,
				"Invalid unicode string ~S.", pos, NULL);
	}
	local = ptr->local;
	push_local(local, &stack);
	str = (char *)lowlevel_local(local, s8 + 1UL);
	if (UTF32_make_utf8((byte *)str, u, s32)) {
		if (! errorp)
			goto error_nil_rollback;
		return call_simple_file_error_va_(ptr, file,
				"Invalid unicode string ~S.", pos, NULL);
	}
	str[s8] = 0;

	/* API */
	str = realpath(str, NULL); /* malloc */
	if (str == NULL) {
		if (! errorp)
			goto error_nil_rollback;
		return call_simple_file_error_va_(ptr, file,
				"Cannot find the TRUENAME ~S file.", file, NULL);
	}

	/* make-pathname */
	Return(string8_null_heap_(&pos, str));
	Return(pathname_designator_heap_(ptr, pos, ret));
	rollback_local(local, stack);
	return 0;

error_nil_rollback:
	rollback_local(local, stack);
error_nil:
	return Result(ret, Nil);
}

#elif defined(LISP_WINDOWS)
/*
 *  files-windows
 */
#include <aclapi.h>
#include <pathcch.h>
#include <shlwapi.h>
#include <windows.h>

#pragma comment(lib, "advapi32.lib")
#pragma comment(lib, "pathcch.lib")
#pragma comment(lib, "shlwapi.lib")

/*
 *  directory
 */
struct directory_struct {
	Execute ptr;
	LocalRoot local;
	addr pos, list, front, root;
};

static int init_directory_struct_(Execute ptr, struct directory_struct *str, addr pos)
{
	clearpoint(str);
	str->ptr = ptr;
	str->local = ptr->local;
	str->list = str->root = str->front = Nil;
	return pathname_designator_heap_(ptr, pos, &str->pos);
}

static int make_list_directory_pathname_(struct directory_struct *str,
		addr *ret, addr list)
{
	LocalRoot local;
	addr pos, one, version;

	local = str->local;
	pos = str->pos;
	make_pathname_alloc(local, &one, RefLogicalPathname(pos));
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_HOST, one);
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_DEVICE, one);
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_NAME, one);
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_TYPE, one);
	/* directory */
	reverse_list_local_unsafe(local, &list, list);
	copylocal_object(local, &list, list);
	SetDirectoryPathname(one, list);
	/* version */
	GetConst(KEYWORD_UNSPECIFIC, &version);
	SetVersionPathname(one, version);
	/* result */
	return physical_pathname_local_(str->ptr, one, ret);
}

static int make_directory_pathname_(struct directory_struct *str, addr *ret)
{
	return make_list_directory_pathname_(str, ret, str->list);
}

static void find_pathname_files(LocalRoot local, addr pos, addr *ret)
{
	unicode *data1, *data2;
	addr one;
	size_t size, i;

	strvect_length(pos, &size);
	strvect_local(local, &one, size + 3ULL);
	data1 = (unicode *)PtrStringUnicode(pos);
	data2 = (unicode *)PtrStringUnicode(one);
	for (i = 0; i < size; i++)
		data2[i] = data1[i];
	data2[size + 0] = '*';
	data2[size + 1] = '.';
	data2[size + 2] = '*';
	*ret = one;
}

static int opendir_files(LocalRoot local,
		addr pos, LPWIN32_FIND_DATAW data, HANDLE *ret)
{
	addr name;
	LocalStack stack;
	const WCHAR *clang;
	HANDLE hFind;

	push_local(local, &stack);
	Return(directory_name_pathname_local_(local, pos, &pos));
	find_pathname_files(local, pos, &pos);
	Return(UTF16_buffer_clang_(local, &name, pos));
	if (name == Unbound) {
		*ret = NULL;
		return call_simple_file_error_va_(NULL, pos,
				"Cannot convert ~S to UTF-16 string.", pos, NULL);
	}
	clang = (const WCHAR *)posbodyr(name);
	hFind = FindFirstFileW(clang, data);
	rollback_local(local, stack);

	return Result(ret, hFind);
}

static void merge_directory_files(LocalRoot local, addr path, addr defaults)
{
	copylocal_pathname_array(local, defaults, PATHNAME_INDEX_HOST, path);
	copylocal_pathname_array(local, defaults, PATHNAME_INDEX_DEVICE, path);
	copylocal_pathname_array(local, defaults, PATHNAME_INDEX_DIRECTORY, path);
}

static int directoryp_directory_files(Execute ptr, addr file, int *ret)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	const WCHAR *body;
	addr pos;

	local = ptr->local;
	push_local(local, &stack);
	Return(name_pathname_local_(ptr, file, &pos));
	Return(UTF16_buffer_clang_(local, &pos, pos));
	if (pos == Unbound) {
		*ret = 0;
		return call_simple_file_error_va_(ptr, file,
				"Cannot convert ~S to UTF-16 string.", file, NULL);
	}
	body = (const WCHAR *)posbodyr(pos);
	check = PathIsDirectoryW(body);
	rollback_local(local, stack);

	return Result(ret, check);
}

static int files_path_directory_files(struct directory_struct *str,
		addr path, addr name, addr base, addr *ret)
{
	int check;
	addr list;
	LocalRoot local;

	Return(directoryp_directory_files(str->ptr, path, &check));
	if (check) {
		local = str->local;
		copy_list_local_unsafe(local, &list, str->list);
		cons_local(local, &list, name, list);
		Return(make_list_directory_pathname_(str, &path, list));
		SetNamePathname(path, Nil);
		SetTypePathname(path, Nil);
	}

	return Result(ret, path);
}

static int files_name_directory_files(struct directory_struct *str,
		addr base, addr name)
{
	int check;
	Execute ptr;
	addr path;

	ptr = str->ptr;
	Return(pathname_designator_local_(ptr, name, &path));
	merge_directory_files(str->local, path, base);
	Return(wildcard_pathname_(path, str->pos, 1, &check));
	if (check) {
		/* push heap */
		Return(files_path_directory_files(str, path, name, base, &path));
		Return(pathname_designator_heap_(ptr, path, &path));
		cons_heap(&str->root, path, str->root);
	}

	return 0;
}

static int files_push_directory_files(struct directory_struct *str,
		addr base, const WCHAR *name)
{
	LocalRoot local;
	LocalStack stack;
	addr path;

	if (wcscmp(name, L".") == 0)
		return 0;
	if (wcscmp(name, L"..") == 0)
		return 0;
	local = str->local;
	push_local(local, &stack);
	Return(string16_null_local_(local, &path, (const byte16 *)name));
	Return(files_name_directory_files(str, base, path));
	rollback_local(local, stack);

	return 0;
}

static int files_directory_files(struct directory_struct *str, addr base)
{
	HANDLE hFind;
	WIN32_FIND_DATAW data;

	Return(opendir_files(str->local, base, &data, &hFind));
	if (hFind == INVALID_HANDLE_VALUE)
		return 0;
	Return(files_push_directory_files(str, base, data.cFileName));
	while (FindNextFileW(hFind, &data)) {
		Return(files_push_directory_files(str, base, data.cFileName));
	}
	if (FindClose(hFind) == 0) {
		return call_simple_file_error_va_(NULL, base, "FindClose error.", NULL);
	}

	return 0;
}

static int file_directory_files(struct directory_struct *str)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	Return(make_directory_pathname_(str, &base));
	Return(files_directory_files(str, base));
	rollback_local(str->local, stack);

	return 0;
}

static int wild_check_directory_files(struct directory_struct *str, addr name, int *ret)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	const WCHAR *ptr;
	addr pos, value;

	local = str->local;
	push_local(local, &stack);
	copy_list_local_unsafe(local, &pos, str->list);
	cons_local(local, &pos, name, pos);
	Return(make_list_directory_pathname_(str, &pos, pos));
	Return(directory_name_pathname_local_(local, pos, &pos));
	Return(UTF16_buffer_clang_(local, &value, pos));
	if (value == Unbound) {
		return call_simple_file_error_va_(NULL, pos,
				"Cannot convert ~S to UTF-16 string.", pos, NULL);
	}
	ptr = (const WCHAR *)posbodyr(value);
	check = PathIsDirectoryW(ptr);
	rollback_local(local, stack);

	return Result(ret, check);
}

static int loop_directory_files(struct directory_struct *str);
static int wild_push_directory_files(struct directory_struct *str, const WCHAR *name)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr path, root, list, front;

	if (wcscmp(name, L".") == 0)
		return 0;
	if (wcscmp(name, L"..") == 0)
		return 0;
	local = str->local;
	push_local(local, &stack);
	Return(string16_null_local_(local, &path, (const byte16 *)name));
	/* directory check */
	Return(wild_check_directory_files(str, path, &check));
	if (check) {
		/* backup list */
		list = str->list;
		front = str->front;
		/* new list */
		copy_list_local_unsafe(local, &root, list);
		cons_local(local, &root, path, root);
		str->list = root;
		/* find */
		Return(loop_directory_files(str));
		/* rollback list */
		str->list = list;
		str->front = front;
	}
	/* rollback local */
	rollback_local(local, stack);

	return 0;
}

static int wild_file_directory_files(struct directory_struct *str, addr base)
{
	HANDLE hFind;
	WIN32_FIND_DATAW data;

	Return(opendir_files(str->local, base, &data, &hFind));
	if (hFind == INVALID_HANDLE_VALUE)
		return 0;
	Return(wild_push_directory_files(str, data.cFileName));
	while (FindNextFileW(hFind, &data)) {
		Return(wild_push_directory_files(str, data.cFileName));
	}
	if (FindClose(hFind) == 0) {
		return call_simple_file_error_va_(NULL, base, "FindClose error.", NULL);
	}

	return 0;
}

static int wild_directory_files(struct directory_struct *str)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	Return(make_directory_pathname_(str, &base));
	Return(wild_file_directory_files(str, base));
	rollback_local(str->local, stack);

	return 0;
}

static int inferiors_file_directory_files(struct directory_struct *str, addr name)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	Return(make_directory_pathname_(str, &base));
	Return(files_name_directory_files(str, base, name));
	rollback_local(str->local, stack);

	return 0;
}

static int inferiors_directory_files(struct directory_struct *str);
static int inferiors_push_directory_files(struct directory_struct *str,
		const WCHAR *name)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr path, root, list, front;

	if (wcscmp(name, L".") == 0)
		return 0;
	if (wcscmp(name, L"..") == 0)
		return 0;
	local = str->local;
	push_local(local, &stack);
	Return(string16_null_local_(local, &path, (const byte16 *)name));
	Return(inferiors_file_directory_files(str, path));
	/* directory check */
	Return(wild_check_directory_files(str, path, &check));
	if (check) {
		/* backup list */
		list = str->list;
		front = str->front;
		/* new list */
		copy_list_local_unsafe(local, &root, list);
		cons_local(local, &root, path, root);
		str->list = root;
		/* find */
		Return(inferiors_directory_files(str));
		/* rollback list */
		str->list = list;
		str->front = front;
	}
	/* rollback local */
	rollback_local(local, stack);

	return 0;
}

static int inferiors_find_directory_files(struct directory_struct *str, addr base)
{
	HANDLE hFind;
	WIN32_FIND_DATAW data;

	Return(opendir_files(str->local, base, &data, &hFind));
	if (hFind == INVALID_HANDLE_VALUE)
		return 0;
	Return(inferiors_push_directory_files(str, data.cFileName));
	while (FindNextFileW(hFind, &data)) {
		Return(inferiors_push_directory_files(str, data.cFileName));
	}
	if (FindClose(hFind) == 0) {
		return call_simple_file_error_va_(NULL, base, "FindClose error.", NULL);
	}

	return 0;
}

static int inferiors_directory_files(struct directory_struct *str)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	Return(make_directory_pathname_(str, &base));
	Return(inferiors_find_directory_files(str, base));
	rollback_local(str->local, stack);

	return 0;
}

static int loop_directory_files(struct directory_struct *str)
{
	addr name, check;

	/* file */
	if (str->front == Nil)
		return file_directory_files(str);

	/* wild */
	GetCons(str->front, &name, &str->front);
	GetConst(KEYWORD_WILD, &check);
	if (name == check)
		return wild_directory_files(str);

	/* wild-inferiors */
	GetConst(KEYWORD_WILD_INFERIORS, &check);
	if (name == check)
		return inferiors_directory_files(str);

	/* next */
	cons_local(str->local, &str->list, name, str->list);
	return loop_directory_files(str);
}

int directory_files_(Execute ptr, addr *ret, addr pos)
{
	addr list, x, y;
	struct directory_struct str;

	Return(init_directory_struct_(ptr, &str, pos));
	GetDirectoryPathname(str.pos, &list);
	if (list == Nil) {
		GetConst(KEYWORD_RELATIVE, &x);
		strvect_char_heap(&y, ".");
		list_heap(&list, x, y, NULL);
	}
	str.front = list;
	Return(loop_directory_files(&str));
	return Result(ret, str.root);
}


/*
 *  probe-file
 */
static int probe_file_boolean(const WCHAR *file)
{
	return PathFileExistsW(file) == TRUE;
}

static int probe_file_run_files(Execute ptr, addr *ret, addr pos)
{
	int check;
	addr value, name;
	const WCHAR *str;

	/* wildcard */
	Return(pathname_designator_heap_(ptr, pos, &pos));
	Return(wild_pathname_boolean_(pos, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot probe-file the wildcard pathname ~S.", pos, NULL);
	}

	/* filename */
	Return(truename_files_(ptr, pos, &pos, 0));
	if (pos == Nil)
		return Result(ret, Nil);

	/* check */
	Return(name_pathname_local_(ptr, pos, &name));
	Return(UTF16_buffer_clang_(ptr->local, &value, name));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot decode UTF-16 string ~S.", pos, NULL);
	}
	str = (const WCHAR *)posbodyr(value);
	*ret = probe_file_boolean(str)? pos: Nil;

	return 0;
}

int probe_file_files_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(probe_file_run_files(ptr, ret, pos));
	rollback_local(local, stack);

	return 0;
}


/*
 *  ensure-directories-exist
 */
static int ensure_directires_exist_wild_files_(addr pos, int *ret)
{
	int check;
	addr field;

	/* directory */
	GetConst(KEYWORD_DIRECTORY, &field);
	Return(wild_pathname_boolean_(pos, field, &check));
	if (check)
		return Result(ret, 1);

	/* host */
	GetConst(KEYWORD_HOST, &field);
	Return(wild_pathname_boolean_(pos, field, &check));
	if (check)
		return Result(ret, 1);

	/* device */
	GetConst(KEYWORD_DEVICE, &field);
	Return(wild_pathname_boolean_(pos, field, &check));
	if (check)
		return Result(ret, 1);

	return Result(ret, 0);
}

static void merge_directory_pathname(LocalRoot local, addr *ret, addr pos, addr value)
{
	addr one;

	make_pathname_alloc(local, &one, RefLogicalPathname(pos));
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_HOST, one);
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_DEVICE, one);
	SetDirectoryPathname(one, value);
	*ret = one;
}

static int ensure_directories_exist_run_files(Execute ptr,
		addr *ret, addr pos, int verbose)
{
	LocalRoot local;
	LocalStack stack;
	const WCHAR *str;
	addr list, value, root, temp, result;

	GetDirectoryPathname(pos, &list);
	if (! consp_getcons(list, &value, &list)) {
		return call_simple_file_error_va_(ptr, pos,
				"Invalid pathname directory ~S.", pos, NULL);
	}
	if (! consp(list)) {
		return call_simple_file_error_va_(ptr, pos,
				"Invalid pathname directory ~S.", pos, NULL);
	}
	result = Nil;
	local = ptr->local;
	conscar_local(local, &root, value);
	while (list != Nil) {
		GetCons(list, &value, &list);
		cons_local(local, &root, value, root);
		push_local(local, &stack);
		reverse_list_local_unsafe(local, &temp, root);
		merge_directory_pathname(local, &temp, pos, temp);
		/* directory check */
		Return(directory_name_pathname_local_(local, temp, &temp));
		Return(UTF16_buffer_clang_(local, &value, temp));
		if (value == Unbound) {
			return call_simple_file_error_va_(ptr, temp,
					"Cannot decode UTF-16 string ~S.", temp, NULL);
		}
		str = (const WCHAR *)posbodyr(value);
		/* already exist */
		if (PathFileExistsW(str)) {
			if (! PathIsDirectoryW(str)) {
				return call_simple_file_error_va_(ptr, pos,
						"Cannot make directory ~S.", pos, NULL);
			}
			rollback_local(local, stack);
			continue;
		}
		/* CreateDirectory */
		if (CreateDirectoryW(str, NULL) == 0) {
			return call_simple_file_error_va_(ptr, pos,
					"Cannot make directory ~S.", pos, NULL);
		}
		result = T;
		/* verbose */
		if (verbose) {
			Return(format_stdout_(ptr, "~&Creating directory: ~S~%", temp, NULL));
		}
		/* continue */
		rollback_local(local, stack);
	}

	return Result(ret, result);
}

int ensure_directories_exist_files_(Execute ptr,
		addr *ret1, addr *ret2, addr pos, int verbose)
{
	int check;
	LocalRoot local;
	LocalStack stack;

	/* filename */
	Return(physical_pathname_heap_(ptr, pos, &pos));
	/* wildcard */
	Return(ensure_directires_exist_wild_files_(pos, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot ENSURE-DIRECTIRIS-EXIST the wildcard pathname ~S.",
				pos, NULL);
	}
	/* loop */
	local = ptr->local;
	push_local(local, &stack);
	Return(ensure_directories_exist_run_files(ptr, ret2, pos, verbose));
	rollback_local(local, stack);
	/* result */
	return Result(ret1, pos);
}


/*
 *  file-author
 */
int file_author_files_(Execute ptr, addr *ret, addr pos)
{
	int check;
	BOOL result;
	DWORD size1, size2;
	LocalRoot local;
	WCHAR *str, *name, *domain;
	addr value;
	PSID owner;
	SID_NAME_USE use;
	PSECURITY_DESCRIPTOR psd;

	/* filename */
	if (stringp(pos)) {
		Return(physical_pathname_local_(ptr, pos, &pos));
	}
	else {
		Return(physical_pathname_heap_(ptr, pos, &pos));
	}

	/* wildcard */
	Return(wild_pathname_boolean_(pos, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot file-author the wildcard pathname ~S.", pos, NULL);
	}

	/* UTF-16 */
	local = ptr->local;
	Return(name_pathname_local_(ptr, pos, &value));
	Return(UTF16_buffer_clang_(local, &value, value));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot decode UTF-16 string ~S.", pos, NULL);
	}
	str = (WCHAR *)posbodyr(value);

	/* GetFileSecurity */
	size1 = 0;
	psd = NULL;
	result = GetFileSecurityW(str, OWNER_SECURITY_INFORMATION, psd, size1, &size1);
	psd = (PSECURITY_DESCRIPTOR)lowlevel_local(local, size1);
	result = GetFileSecurityW(str, OWNER_SECURITY_INFORMATION, psd, size1, &size1);
	if (result == 0)
		goto finish_nil;

	/* GetSecurityDescriptorOwner */
	result = 0;
	result = GetSecurityDescriptorOwner(psd, &owner, &result);
	if (result == 0) {
		return call_simple_file_error_va_(ptr, pos,
				"GetSecurityDescriptorOwner error.", NULL);
	}

	/* iookupAccountSid */
	use = SidTypeUnknown;
	size1 = size2 = 1;
	name = domain = NULL;
	result = LookupAccountSidW(NULL, owner, name, &size1, domain, &size2, &use);
	name = (WCHAR *)lowlevel_local(local, size1 * sizeof(DWORD));
	domain = (WCHAR *)lowlevel_local(local, size2 * sizeof(DWORD));
	result = LookupAccountSidW(NULL, owner, name, &size1, domain, &size2, &use);
	if (result == FALSE) {
		if (GetLastError() == ERROR_NONE_MAPPED)
			goto finish_nil;
		return call_simple_file_error_va_(ptr, pos, "LookupAccountSid error.", NULL);
	}

	/* result */
	return string16_null_heap_(ret, (const byte16 *)name);

finish_nil:
	return Result(ret, Nil);
}


/*
 *  file-write-date
 */
static int file_write_date_base_files(addr *ret, const FILETIME *file)
{
	ULONGLONG a, b;

	/* The FILETIME structure is a 64-bit value representing
	 * the number of 100-nanosecond intervals since January 1, 1601.
	 */
	a = (((1900ULL - 1601ULL) * 365ULL) + 74ULL - 2ULL) *
		24ULL * 60ULL * 60ULL * 10000000ULL;
	b = (((ULONGLONG)file->dwHighDateTime) << 32ULL) | file->dwLowDateTime;
	if (b < a)
		return 1;
	b = (b - a) / 10000000ULL;
#ifdef LISP_64BIT
	*ret = intsizeh((size_t)b);
#else
	bignum_value2_heap(ret, signplus_bignum,
			(fixed)(b >> 32ULL),
			(fixed)(b & 0xFFFFFFFFULL));
	bignum_result_heap(*ret, ret);
#endif

	return 0;
}

static int file_write_date_run_files(Execute ptr, addr *ret, addr pos)
{
	int check;
	LocalRoot local;
	HANDLE hFind;
	const WCHAR *str;
	addr value;
	WIN32_FIND_DATAW data;

	/* filename */
	if (stringp(pos)) {
		Return(physical_pathname_local_(ptr, pos, &pos));
	}
	else {
		Return(physical_pathname_heap_(ptr, pos, &pos));
	}

	/* wildcard */
	Return(wild_pathname_boolean_(pos, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot file-write-date the wildcard pathname ~S.", pos, NULL);
	}

	/* UTF-16 */
	local = ptr->local;
	Return(name_pathname_local_(ptr, pos, &value));
	Return(UTF16_buffer_clang_(local, &value, value));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot decode UTF-16 string ~S.", pos, NULL);
	}
	str = (const WCHAR *)posbodyr(value);

	/* FindFirstFile */
	hFind = FindFirstFileW(str, &data);
	if (hFind == INVALID_HANDLE_VALUE) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot find file ~S.", pos, NULL);
	}
	FindClose(hFind);

	/* result */
	if (file_write_date_base_files(ret, &data.ftLastWriteTime)) {
		return call_simple_file_error_va_(ptr, pos,
				"The file ~S timestamp must be after 1900 year.", pos, NULL);
	}

	return 0;
}

int file_write_date_files_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(file_write_date_run_files(ptr, ret, pos));
	rollback_local(local, stack);

	return 0;
}


/*
 *  rename-file
 */
static int rename_file_run_files(Execute ptr,
		addr *ret1, addr *ret2, addr *ret3, addr pos, addr to)
{
	int check;
	LocalRoot local;
	addr file, from, value, true1, true2;
	const WCHAR *str1, *str2;

	Return(pathname_designator_heap_(ptr, pos, &file));
	Return(physical_pathname_heap_(ptr, file, &from));
	Return(physical_pathname_heap_(ptr, to, &to));
	Return(truename_files_(ptr, from, &true1, 0));
	Return(wild_pathname_boolean_(from, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, from,
				"Cannot rename wildcard pathname from ~S", from, NULL);
	}
	Return(wild_pathname_boolean_(to, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, to,
				"Cannot rename wildcard pathname to ~S", to, NULL);
	}
	/* filename */
	local = ptr->local;
	Return(name_pathname_local_(ptr, from, &value));
	Return(UTF16_buffer_clang_(local, &value, value));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, from,
				"Cannot decode UTF-16 string ~S.", from, NULL);
	}
	str1 = (const WCHAR *)posbodyr(value);
	Return(name_pathname_local_(ptr, to, &value));
	Return(UTF16_buffer_clang_(local, &value, value));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, to,
				"Cannot decode UTF-16 string ~S.", to, NULL);
	}
	str2 = (const WCHAR *)posbodyr(value);
	/* check */
	if (probe_file_boolean(str2)) {
		return call_simple_file_error_va_(ptr, to,
				"The file ~S is already exist.", to, NULL);
	}
	/* rename */
	if (MoveFileW(str1, str2) == 0) {
		return call_simple_file_error_va_(ptr, to,
				"Cannot rename ~S to ~S.", from, to, NULL);
	}
	/* stream */
	if (streamp(pos))
		SetPathnameStream(pos, to);
	/* result */
	Return(truename_files_(ptr, to, &true2, 0));
	*ret1 = to;
	*ret2 = true1;
	*ret3 = true2;

	return 0;
}

int rename_file_files_(Execute ptr,
		addr *ret1, addr *ret2, addr *ret3, addr file, addr to)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(rename_file_run_files(ptr, ret1, ret2, ret3, file, to));
	rollback_local(local, stack);

	return 0;
}


/*
 *  delete-file
 */
static BOOL DeleteFileAsyncW(LPCWSTR name)
{
	WCHAR path[MAX_PATH];
	WCHAR temp[MAX_PATH];
	size_t size;

	size = wcslen(name);
	if (MAX_PATH <= size) {
		Debug("length error.");
		return 0;
	}
	wcsncpy(path, name, MAX_PATH);

	if (PathCchRemoveFileSpec(path, MAX_PATH) != S_OK) {
		Debug("PathCchRemoveFileSpec error.");
		return 0;
	}
	if (path[0] == 0) {
		wcscpy(path, L".");
	}

	if (! GetTempFileNameW(path, L".rm", 0, temp)) {
		Debug("GetTempFileNameW error.");
		return 0;
	}

	if (! MoveFileExW(name, temp, MOVEFILE_REPLACE_EXISTING)) {
		/* Debug("MoveFileExW error."); */
		return 0;
	}

	if (! DeleteFileW(temp)) {
		Debug("DeleteFileW error.");
		return 0;
	}

	return 1;
}

static int delete_file_run_files(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	LocalRoot local;
	addr file, value;
	const WCHAR *str;

	Return(physical_pathname_heap_(ptr, pos, &file));
	Return(wild_pathname_boolean_(file, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot delete wildcard pathname ~S", pos, NULL);
	}
	if (! pathname_file_p(file)) {
		if (errorp) {
			return call_simple_file_error_va_(ptr, pos,
					"The argument ~S is not a file.", pos, NULL);
		}
		return Result(ret, 0);
	}
	/* filename */
	local = ptr->local;
	Return(name_pathname_local_(ptr, file, &value));
	Return(UTF16_buffer_clang_(local, &value, value));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, file,
				"Cannot decode UTF-16 string ~S.", file, NULL);
	}
	str = (const WCHAR *)posbodyr(value);
	/* delete */
	if (DeleteFileAsyncW(str) == 0) {
		if (errorp) {
			return call_simple_file_error_va_(ptr, pos,
					"Cannot delete ~S.", file, NULL);
		}
		return Result(ret, 0);
	}
	/* stream */
	if (streamp(pos)) {
		Return(close_stream_(pos, NULL));
	}

	return Result(ret, 1);
}

static int delete_file_errorp(Execute ptr, addr pos, int errorp, int *ret)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(delete_file_run_files(ptr, pos, errorp, ret));
	rollback_local(local, stack);

	return 0;
}

int delete_file_files_(Execute ptr, addr pos)
{
	int check;
	return delete_file_errorp(ptr, pos, 1, &check);
}

int remove_file_common_(Execute ptr, addr pos, int errorp, int *ret)
{
	return delete_file_errorp(ptr, pos, errorp, ret);
}


/*
 *  remove-directory
 */
int remove_directory_common_(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	LocalRoot local;
	addr file, value;
	const WCHAR *str;

	Return(physical_pathname_heap_(ptr, pos, &file));
	Return(wild_pathname_boolean_(file, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot delete wildcard pathname ~S", pos, NULL);
	}
	if (! pathname_directory_p(file)) {
		if (errorp) {
			return call_simple_file_error_va_(ptr, pos,
					"The argument ~S is not a directory.", pos, NULL);
		}
		return Result(ret, 0);
	}
	/* filename */
	local = ptr->local;
	Return(name_pathname_local_(ptr, file, &value));
	Return(UTF16_buffer_clang_(local, &value, value));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, file,
				"Cannot decode UTF-16 string ~S.", file, NULL);
	}
	str = (const WCHAR *)posbodyr(value);
	/* delete */
	if (RemoveDirectoryW(str) == 0) {
		if (errorp) {
			return call_simple_file_error_va_(ptr, pos,
					"Cannot delete ~S.", file, NULL);
		}
		return Result(ret, 0);
	}
	/* stream */
	if (streamp(pos)) {
		Return(close_stream_(pos, NULL));
	}

	return Result(ret, 1);
}


/*
 *  truename
 */
int truename_files_(Execute ptr, addr file, addr *ret, int errorp)
{
	int check;
	addr pos;
	const unicode *u;
	wchar_t *str, *dst;
	size_t s32, s16;
	LocalRoot local;
	LocalStack stack;
	DWORD dcheck;

	/* wild-check */
	Return(physical_pathname_heap_(ptr, file, &pos));
	Return(wild_pathname_boolean_(pos, Nil, &check));
	if (check) {
		if (! errorp)
			goto error_nil;
		return call_simple_file_error_va_(ptr, file,
				"TRUENAME don't allow the wildcard filename ~S.", file, NULL);
	}
	Return(name_pathname_heap_(ptr, pos, &pos));

	/* realpath */
	strvect_posbodylen(pos, &u, &s32);
	if (UTF32_length_utf16(u, s32, &s16)) {
		if (! errorp)
			goto error_nil;
		return call_simple_file_error_va_(ptr, file,
				"Invalid unicode string ~S.", pos, NULL);
	}
	local = ptr->local;
	push_local(local, &stack);
	str = (wchar_t *)lowlevel_local(local, (s16 + 1UL) * sizeoft(wchar_t));
	if (UTF32_make_utf16((byte16 *)str, u, s32)) {
		if (! errorp)
			goto error_nil_rollback;
		return call_simple_file_error_va_(ptr, file,
				"Invalid unicode string ~S.", pos, NULL);
	}
	str[s16] = 0;

	/* API */
	dst = (wchar_t *)lowlevel_local(local, MAX_PATH * sizeoft(wchar_t));
	dcheck = GetFullPathNameW(str, MAX_PATH, dst, NULL);
	if (dcheck == 0) {
		if (! errorp)
			goto error_nil_rollback;
		return call_simple_file_error_va_(ptr, file,
				"Cannot find the TRUENAME ~S file.", file, NULL);
	}

	/* make-pathname */
	Return(string16_size_heap_(&pos, dst, (size_t)dcheck));
	Return(pathname_designator_heap_(ptr, pos, ret));
	rollback_local(local, stack);
	return 0;

error_nil_rollback:
	rollback_local(local, stack);
error_nil:
	return Result(ret, Nil);
}

#else
/*
 *  files-ansi
 */

int directory_files_(Execute ptr, addr *ret, addr pos)
{
	return fmte_("DIRECTORY function is not supported in ANSI-C mode.", NULL);
}

static int probe_file_boolean(const char *file)
{
	FILE *input;

	input = fopen(file, "r");
	if (input)
		fclose(input);

	return input != NULL;
}

static int probe_file_run_files(Execute ptr, addr *ret, addr pos)
{
	int check;
	addr value, name;
	const char *str;

	/* wildcard */
	Return(pathname_designator_heap_(ptr, pos, &pos));
	Return(wild_pathname_boolean_(pos, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot probe-file the wildcard pathname ~S.", pos, NULL);
	}

	/* filename */
	if (stringp(pos)) {
		Return(physical_pathname_local_(ptr, pos, &pos));
	}
	else {
		Return(physical_pathname_heap_(ptr, pos, &pos));
	}

	/* check */
	Return(name_pathname_local_(ptr, pos, &name));
	Return(UTF8_buffer_clang_(ptr->local, &value, name));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot decode UTF-8 string ~S.", name, NULL);
	}
	str = (const char *)posbodyr(value);
	*ret = probe_file_boolean(str)? pos: Nil;

	return 0;
}

int probe_file_files_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(probe_file_run_files(ptr, ret, pos));
	rollback_local(local, stack);

	return 0;
}

int ensure_directories_exist_files_(Execute ptr,
		addr *ret1, addr *ret2, addr pos, int verbose)
{
	return fmte_("ENSUER-DIRECTORIES-EXIST function is "
			"not supported in ANSI-C mode.", NULL);
}

int file_author_files_(Execute ptr, addr *ret, addr pos)
{
	return fmte_("FILE-AUTHOR function is not supported in ANSI-C mode.", NULL);
}

int file_write_date_files_(Execute ptr, addr *ret, addr pos)
{
	return fmte_("FILE-WRITE-DATE function is not supported in ANSI-C mode.", NULL);
}

static int rename_file_run_files(Execute ptr,
		addr *ret1, addr *ret2, addr *ret3, addr pos, addr to)
{
	int check;
	LocalRoot local;
	addr file, from, value, true1, true2;
	const char *str1, *str2;

	Return(pathname_designator_heap_(ptr, pos, &file));
	Return(physical_pathname_heap_(ptr, file, &from));
	Return(physical_pathname_heap_(ptr, to, &to));
	Return(truename_files_(ptr, from, &true1, 0));
	Return(wild_pathname_boolean_(from, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot rename wildcard pathname from ~S", from, NULL);
	}
	Return(wild_pathname_boolean_(to, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, to,
				"Cannot rename wildcard pathname to ~S", to, NULL);
	}
	/* filename */
	local = ptr->local;
	Return(name_pathname_local_(ptr, from, &value));
	Return(UTF8_buffer_clang_(local, &value, value));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, from,
				"Cannot decode UTF-8 string ~S.", from, NULL);
	}
	str1 = (const char *)posbodyr(value);
	Return(name_pathname_local_(ptr, to, &value));
	Return(UTF8_buffer_clang_(local, &value, value));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, to,
				"Cannot decode UTF-8 string ~S.", to, NULL);
	}
	str2 = (const char *)posbodyr(value);
	/* check */
	if (probe_file_boolean(str2)) {
		return call_simple_file_error_va_(ptr, to,
				"The file ~S is already exist.", to, NULL);
	}
	/* rename */
	if (rename(str1, str2)) {
		return call_simple_file_error_va_(ptr, to,
				"Cannot rename ~S to ~S.", from, to, NULL);
	}
	/* stream */
	if (streamp(pos))
		SetPathnameStream(pos, to);
	/* result */
	Return(truename_files_(ptr, to, &true2, 0));
	*ret1 = to;
	*ret2 = true1;
	*ret3 = true2;

	return 0;
}

int rename_file_files_(Execute ptr,
		addr *ret1, addr *ret2, addr *ret3, addr file, addr to)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(rename_file_run_files(ptr, ret1, ret2, ret3, file, to));
	rollback_local(local, stack);

	return 0;
}

int delete_file_files_(Execute ptr, addr pos)
{
	return fmte_("DELETE-FILE function is not supported in ANSI-C mode.", NULL);
}

int remove_file_common_(Execute ptr, addr pos, int errorp, int *ret)
{
	*ret = 0;
	return fmte_("REMOVE-FILE function is not supported in ANSI-C mode.", NULL);
}

int remove_directory_common_(Execute ptr, addr pos, int errorp, int *ret)
{
	*ret = 0;
	return fmte_("REMOVE-DIRECTORY function is not supported in ANSI-C mode.", NULL);
}

int truename_files_(Execute ptr, addr file, addr *ret, int errorp)
{
	if (! errorp)
		return Result(ret, Nil);
	return call_simple_file_error_va_(ptr, file,
			"TRUENAME is not support in ANSI-C mode.", NULL);
}

#endif


/************************************************************
 *  float_equal.c
 ************************************************************/

/*
 *  boolean
 */
int zerop_single_float(addr pos)
{
	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	return RefSingleFloat(pos) == 0.0f;
}

int zerop_double_float(addr pos)
{
	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	return RefDoubleFloat(pos) == 0.0;
}

int zerop_long_float(addr pos)
{
	CheckType(pos, LISPTYPE_LONG_FLOAT);
	return RefLongFloat(pos) == 0.0L;
}


/*
 *  equal
 */
int equal_fs_real(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	return single_float_fixnum(left) == RefSingleFloat(right);
}

int equal_fd_real(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	return double_float_fixnum(left) == RefDoubleFloat(right);
}

int equal_fl_real(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	return long_float_fixnum(left) == RefLongFloat(right);
}

int equal_bs_real_(addr left, addr right, int *ret)
{
	single_float x, y;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &x));
	GetSingleFloat(right, &y);

	return Result(ret, x == y);
}

int equal_bd_real_(addr left, addr right, int *ret)
{
	double_float x, y;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &x));
	GetDoubleFloat(right, &y);

	return Result(ret, x == y);
}

int equal_bl_real_(addr left, addr right, int *ret)
{
	long_float x, y;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &x));
	GetLongFloat(right, &y);

	return Result(ret, x == y);
}


/*
 *  compare
 */
int compare_fs_real(addr left, addr right)
{
	single_float value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value1 = single_float_fixnum(left);
	value2 = RefSingleFloat(right);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_fd_real(addr left, addr right)
{
	double_float value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value1 = double_float_fixnum(left);
	value2 = RefDoubleFloat(right);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_fl_real(addr left, addr right)
{
	long_float value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value1 = long_float_fixnum(left);
	value2 = RefLongFloat(right);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_sf_real(addr left, addr right)
{
	return -compare_fs_real(right, left);
}

int compare_df_real(addr left, addr right)
{
	return -compare_fd_real(right, left);
}

int compare_lf_real(addr left, addr right)
{
	return -compare_fl_real(right, left);
}

int compare_ss_real(addr left, addr right)
{
	single_float value1, value2;
	GetSingleFloat(left, &value1);
	GetSingleFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_sd_real(addr left, addr right)
{
	single_float value1;
	double_float value2;
	GetSingleFloat(left, &value1);
	GetDoubleFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_sl_real(addr left, addr right)
{
	single_float value1;
	long_float value2;
	GetSingleFloat(left, &value1);
	GetLongFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_ds_real(addr left, addr right)
{
	double_float value1;
	single_float value2;
	GetDoubleFloat(left, &value1);
	GetSingleFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_dd_real(addr left, addr right)
{
	double_float value1, value2;
	GetDoubleFloat(left, &value1);
	GetDoubleFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_dl_real(addr left, addr right)
{
	double_float value1;
	long_float value2;
	GetDoubleFloat(left, &value1);
	GetLongFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_ls_real(addr left, addr right)
{
	long_float value1;
	single_float value2;
	GetLongFloat(left, &value1);
	GetSingleFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_ld_real(addr left, addr right)
{
	long_float value1;
	double_float value2;
	GetLongFloat(left, &value1);
	GetDoubleFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_ll_real(addr left, addr right)
{
	long_float value1, value2;
	GetLongFloat(left, &value1);
	GetLongFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

static int compare_single_float(addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			return compare_ss_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_sd_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return compare_sl_real(left, right);

		default:
			break;
	}
	Abort("type error");
	return 0;
}

static int compare_double_float(addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			return compare_ds_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_dd_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return compare_dl_real(left, right);

		default:
			break;
	}
	Abort("type error");
	return 0;
}

static int compare_long_float(addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			return compare_ls_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_ld_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return compare_ll_real(left, right);

		default:
			break;
	}
	Abort("type error");
	return 0;
}

int compare_float(addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_SINGLE_FLOAT:
			return compare_single_float(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_double_float(left, right);

		case LISPTYPE_LONG_FLOAT:
			return compare_long_float(left, right);

		default:
			break;
	}
	Abort("type error");
	return 0;
}

int less_float_clang(addr left, addr right)
{
	return less_float(left, right);
}

int less_equal_float_clang(addr left, addr right)
{
	return less_equal_float(left, right);
}

int less_ss_clang(addr left, addr right)
{
	return less_ss_real(left, right);
}

int less_dd_clang(addr left, addr right)
{
	return less_dd_real(left, right);
}

int less_ll_clang(addr left, addr right)
{
	return less_ll_real(left, right);
}

int less_equal_ss_clang(addr left, addr right)
{
	return less_equal_ss_real(left, right);
}

int less_equal_dd_clang(addr left, addr right)
{
	return less_equal_dd_real(left, right);
}

int less_equal_ll_clang(addr left, addr right)
{
	return less_equal_ll_real(left, right);
}


/*
 *  compare
 */
static int compare_single_float_(addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			return Result(ret, compare_ss_real(left, right));

		case LISPTYPE_DOUBLE_FLOAT:
			return Result(ret, compare_sd_real(left, right));

		case LISPTYPE_LONG_FLOAT:
			return Result(ret, compare_sl_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, FLOAT);
	}
}

static int compare_double_float_(addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			return Result(ret, compare_ds_real(left, right));

		case LISPTYPE_DOUBLE_FLOAT:
			return Result(ret, compare_dd_real(left, right));

		case LISPTYPE_LONG_FLOAT:
			return Result(ret, compare_dl_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, FLOAT);
	}
}

static int compare_long_float_(addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			return Result(ret, compare_ls_real(left, right));

		case LISPTYPE_DOUBLE_FLOAT:
			return Result(ret, compare_ld_real(left, right));

		case LISPTYPE_LONG_FLOAT:
			return Result(ret, compare_ll_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, FLOAT);
	}
}

int compare_float_(addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_SINGLE_FLOAT:
			return compare_single_float_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_double_float_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return compare_long_float_(left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, FLOAT);
	}
}

int less_float_clang_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_float_(left, right, &check));
	return Result(ret, check < 0);
}

int less_equal_float_clang_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_float_(left, right, &check));
	return Result(ret, check <= 0);
}


/************************************************************
 *  float_multi.c
 ************************************************************/

/*
 *  multiple
 */
int multi_float_fs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = single_float_fixnum(left) * RefSingleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

int multi_float_fd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = double_float_fixnum(left) * RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int multi_float_fl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = long_float_fixnum(left) * RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int multi_float_fs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_fs_alloc_(local, left, right, ret);
}

int multi_float_fd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_fd_alloc_(local, left, right, ret);
}

int multi_float_fl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_fl_alloc_(local, left, right, ret);
}

int multi_float_fs_heap_(addr left, addr right, addr *ret)
{
	return multi_float_fs_alloc_(NULL, left, right, ret);
}

int multi_float_fd_heap_(addr left, addr right, addr *ret)
{
	return multi_float_fd_alloc_(NULL, left, right, ret);
}

int multi_float_fl_heap_(addr left, addr right, addr *ret)
{
	return multi_float_fl_alloc_(NULL, left, right, ret);
}

int multi_float_bs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	Return(single_float_bignum_(left, &value));
	value *= RefSingleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

int multi_float_bd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	Return(double_float_bignum_(left, &value));
	value *= RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int multi_float_bl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	Return(long_float_bignum_(left, &value));
	value *= RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int multi_float_bs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_bs_alloc_(local, left, right, ret);
}

int multi_float_bd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_bd_alloc_(local, left, right, ret);
}

int multi_float_bl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_bl_alloc_(local, left, right, ret);
}

int multi_float_bs_heap_(addr left, addr right, addr *ret)
{
	return multi_float_bs_alloc_(NULL, left, right, ret);
}

int multi_float_bd_heap_(addr left, addr right, addr *ret)
{
	return multi_float_bd_alloc_(NULL, left, right, ret);
}

int multi_float_bl_heap_(addr left, addr right, addr *ret)
{
	return multi_float_bl_alloc_(NULL, left, right, ret);
}

int multi_float_rs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	Return(single_float_ratio_(left, &value));
	value *= RefSingleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

int multi_float_rd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	Return(double_float_ratio_(left, &value));
	value *= RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int multi_float_rl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	Return(long_float_ratio_(left, &value));
	value *= RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
	return 0;
}

int multi_float_rs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_rs_alloc_(local, left, right, ret);
}

int multi_float_rd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_rd_alloc_(local, left, right, ret);
}

int multi_float_rl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_rl_alloc_(local, left, right, ret);
}

int multi_float_rs_heap_(addr left, addr right, addr *ret)
{
	return multi_float_rs_alloc_(NULL, left, right, ret);
}

int multi_float_rd_heap_(addr left, addr right, addr *ret)
{
	return multi_float_rd_alloc_(NULL, left, right, ret);
}

int multi_float_rl_heap_(addr left, addr right, addr *ret)
{
	return multi_float_rl_alloc_(NULL, left, right, ret);
}

int multi_float_ss_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefSingleFloat(left) * RefSingleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

int multi_float_sd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = ((double_float)RefSingleFloat(left)) * RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int multi_float_sl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefSingleFloat(left)) * RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int multi_float_ds_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) * ((double_float)RefSingleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int multi_float_dd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) * RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int multi_float_dl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefDoubleFloat(left)) * RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int multi_float_ls_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefLongFloat(left) * ((long_float)RefSingleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int multi_float_ld_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefLongFloat(left) * ((long_float)RefDoubleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int multi_float_ll_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = RefLongFloat(left) * RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int multi_float_ss_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_ss_alloc_(local, left, right, ret);
}

int multi_float_sd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_sd_alloc_(local, left, right, ret);
}

int multi_float_sl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_sl_alloc_(local, left, right, ret);
}

int multi_float_ds_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_ds_alloc_(local, left, right, ret);
}

int multi_float_dd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_dd_alloc_(local, left, right, ret);
}

int multi_float_dl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_dl_alloc_(local, left, right, ret);
}

int multi_float_ls_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_ls_alloc_(local, left, right, ret);
}

int multi_float_ld_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_ld_alloc_(local, left, right, ret);
}

int multi_float_ll_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_ll_alloc_(local, left, right, ret);
}

int multi_float_ss_heap_(addr left, addr right, addr *ret)
{
	return multi_float_ss_alloc_(NULL, left, right, ret);
}

int multi_float_sd_heap_(addr left, addr right, addr *ret)
{
	return multi_float_sd_alloc_(NULL, left, right, ret);
}

int multi_float_sl_heap_(addr left, addr right, addr *ret)
{
	return multi_float_sl_alloc_(NULL, left, right, ret);
}

int multi_float_ds_heap_(addr left, addr right, addr *ret)
{
	return multi_float_ds_alloc_(NULL, left, right, ret);
}

int multi_float_dd_heap_(addr left, addr right, addr *ret)
{
	return multi_float_dd_alloc_(NULL, left, right, ret);
}

int multi_float_dl_heap_(addr left, addr right, addr *ret)
{
	return multi_float_dl_alloc_(NULL, left, right, ret);
}

int multi_float_ls_heap_(addr left, addr right, addr *ret)
{
	return multi_float_ls_alloc_(NULL, left, right, ret);
}

int multi_float_ld_heap_(addr left, addr right, addr *ret)
{
	return multi_float_ld_alloc_(NULL, left, right, ret);
}

int multi_float_ll_heap_(addr left, addr right, addr *ret)
{
	return multi_float_ll_alloc_(NULL, left, right, ret);
}


/*
 *  inverse
 */
int inverse_single_float_alloc_(LocalRoot local, addr pos, addr *ret)
{
	single_float value;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	GetSingleFloat(pos, &value);
	if (value == 0.0f) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, pos);
	}
	value = 1.0f / value;
	Return_float_errorcheck1(CONSTANT_COMMON_SLASH, value, pos);
	single_float_alloc(local, ret, value);

	return 0;
}

int inverse_double_float_alloc_(LocalRoot local, addr pos, addr *ret)
{
	double_float value;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	GetDoubleFloat(pos, &value);
	if (value == 0.0) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, pos);
	}
	value = 1.0 / value;
	Return_float_errorcheck1(CONSTANT_COMMON_SLASH, value, pos);
	double_float_alloc(local, ret, value);

	return 0;
}

int inverse_long_float_alloc_(LocalRoot local, addr pos, addr *ret)
{
	long_float value;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, &value);
	if (value == 0.0L) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, pos);
	}
	value = 1.0L / value;
	Return_float_errorcheck1(CONSTANT_COMMON_SLASH, value, pos);
	long_float_alloc(local, ret, value);

	return 0;
}

int inverse_single_float_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return inverse_single_float_alloc_(local, pos, ret);
}

int inverse_double_float_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return inverse_double_float_alloc_(local, pos, ret);
}

int inverse_long_float_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return inverse_long_float_alloc_(local, pos, ret);
}

int inverse_single_float_heap_(addr pos, addr *ret)
{
	return inverse_single_float_alloc_(NULL, pos, ret);
}

int inverse_double_float_heap_(addr pos, addr *ret)
{
	return inverse_double_float_alloc_(NULL, pos, ret);
}

int inverse_long_float_heap_(addr pos, addr *ret)
{
	return inverse_long_float_alloc_(NULL, pos, ret);
}


/*
 *  division
 */
int div_float_fs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = single_float_fixnum(left) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

int div_float_fd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = double_float_fixnum(left) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int div_float_fl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = long_float_fixnum(left) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int div_float_fs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_fs_alloc_(local, left, right, ret);
}

int div_float_fd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_fd_alloc_(local, left, right, ret);
}

int div_float_fl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_fl_alloc_(local, left, right, ret);
}

int div_float_fs_heap_(addr left, addr right, addr *ret)
{
	return div_float_fs_alloc_(NULL, left, right, ret);
}

int div_float_fd_heap_(addr left, addr right, addr *ret)
{
	return div_float_fd_alloc_(NULL, left, right, ret);
}

int div_float_fl_heap_(addr left, addr right, addr *ret)
{
	return div_float_fl_alloc_(NULL, left, right, ret);
}

int div_float_sf_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &check);
	if (check == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefSingleFloat(left) / ((single_float)check);
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

int div_float_df_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type righterror");
	GetFixnum(right, &check);
	if (check == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefDoubleFloat(left) / ((double_float)check);
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int div_float_lf_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &check);
	if (check == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefLongFloat(left) / ((long_float)check);
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int div_float_sf_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_sf_alloc_(local, left, right, ret);
}

int div_float_df_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_df_alloc_(local, left, right, ret);
}

int div_float_lf_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_lf_alloc_(local, left, right, ret);
}

int div_float_sf_heap_(addr left, addr right, addr *ret)
{
	return div_float_sf_alloc_(NULL, left, right, ret);
}

int div_float_df_heap_(addr left, addr right, addr *ret)
{
	return div_float_df_alloc_(NULL, left, right, ret);
}

int div_float_lf_heap_(addr left, addr right, addr *ret)
{
	return div_float_lf_alloc_(NULL, left, right, ret);
}

int div_float_bs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(single_float_bignum_(left, &value2));
	value = value2 / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

int div_float_bd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(double_float_bignum_(left, &value2));
	value = value2 / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int div_float_bl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(long_float_bignum_(left, &value2));
	value = value2 / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int div_float_bs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_bs_alloc_(local, left, right, ret);
}

int div_float_bd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_bd_alloc_(local, left, right, ret);
}

int div_float_bl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_bl_alloc_(local, left, right, ret);
}

int div_float_bs_heap_(addr left, addr right, addr *ret)
{
	return div_float_bs_alloc_(NULL, left, right, ret);
}

int div_float_bd_heap_(addr left, addr right, addr *ret)
{
	return div_float_bd_alloc_(NULL, left, right, ret);
}

int div_float_bl_heap_(addr left, addr right, addr *ret)
{
	return div_float_bl_alloc_(NULL, left, right, ret);
}

int div_float_sb_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value, value2;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(single_float_bignum_(right, &value2));
	value = RefSingleFloat(left) / value2;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

int div_float_db_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value, value2;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(double_float_bignum_(right, &value2));
	value = RefDoubleFloat(left) / value2;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int div_float_lb_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value, value2;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	GetLongFloat(left, &value);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(long_float_bignum_(right, &value2));
	value = RefLongFloat(left) / value2;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int div_float_sb_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_sb_alloc_(local, left, right, ret);
}

int div_float_db_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_db_alloc_(local, left, right, ret);
}

int div_float_lb_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_lb_alloc_(local, left, right, ret);
}

int div_float_sb_heap_(addr left, addr right, addr *ret)
{
	return div_float_sb_alloc_(NULL, left, right, ret);
}

int div_float_db_heap_(addr left, addr right, addr *ret)
{
	return div_float_db_alloc_(NULL, left, right, ret);
}

int div_float_lb_heap_(addr left, addr right, addr *ret)
{
	return div_float_lb_alloc_(NULL, left, right, ret);
}

int div_float_rs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value, value2;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(single_float_ratio_(left, &value2));
	value = value2 / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

int div_float_rd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value, value2;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(double_float_ratio_(left, &value2));
	value = value2 / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int div_float_rl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value, value2;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(long_float_ratio_(left, &value2));
	value = value2 / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int div_float_rs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_rs_alloc_(local, left, right, ret);
}

int div_float_rd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_rd_alloc_(local, left, right, ret);
}

int div_float_rl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_rl_alloc_(local, left, right, ret);
}

int div_float_rs_heap_(addr left, addr right, addr *ret)
{
	return div_float_rs_alloc_(NULL, left, right, ret);
}

int div_float_rd_heap_(addr left, addr right, addr *ret)
{
	return div_float_rd_alloc_(NULL, left, right, ret);
}

int div_float_rl_heap_(addr left, addr right, addr *ret)
{
	return div_float_rl_alloc_(NULL, left, right, ret);
}

int div_float_sr_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value, value2;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(single_float_ratio_(right, &value2));
	value = RefSingleFloat(left) / value2;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

int div_float_dr_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value, value2;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(double_float_ratio_(right, &value2));
	value = RefDoubleFloat(left) / value2;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int div_float_lr_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value, value2;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(long_float_ratio_(right, &value2));
	value = RefLongFloat(left) / value2;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int div_float_sr_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_sr_alloc_(local, left, right, ret);
}

int div_float_dr_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_dr_alloc_(local, left, right, ret);
}

int div_float_lr_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_lr_alloc_(local, left, right, ret);
}

int div_float_sr_heap_(addr left, addr right, addr *ret)
{
	return div_float_sr_alloc_(NULL, left, right, ret);
}

int div_float_dr_heap_(addr left, addr right, addr *ret)
{
	return div_float_dr_alloc_(NULL, left, right, ret);
}

int div_float_lr_heap_(addr left, addr right, addr *ret)
{
	return div_float_lr_alloc_(NULL, left, right, ret);
}

int div_float_ss_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefSingleFloat(left) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

int div_float_sd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = ((double_float)RefSingleFloat(left)) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int div_float_sl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = ((long_float)RefSingleFloat(left)) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int div_float_ds_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float check;
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &check);
	if (check == 0.0f) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefDoubleFloat(left) / ((double_float)check);
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int div_float_dd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefDoubleFloat(left) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int div_float_dl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = ((long_float)RefDoubleFloat(left)) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int div_float_ls_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float check;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &check);
	if (check == 0.0f) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefLongFloat(left) / ((long_float)check);
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int div_float_ld_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float check;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &check);
	if (check == 0.0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefLongFloat(left) / ((long_float)check);
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int div_float_ll_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefLongFloat(left) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int div_float_ss_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_ss_alloc_(local, left, right, ret);
}

int div_float_sd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_sd_alloc_(local, left, right, ret);
}

int div_float_sl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_sl_alloc_(local, left, right, ret);
}

int div_float_ds_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_ds_alloc_(local, left, right, ret);
}

int div_float_dd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_dd_alloc_(local, left, right, ret);
}

int div_float_dl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_dl_alloc_(local, left, right, ret);
}

int div_float_ls_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_ls_alloc_(local, left, right, ret);
}

int div_float_ld_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_ld_alloc_(local, left, right, ret);
}

int div_float_ll_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_ll_alloc_(local, left, right, ret);
}

int div_float_ss_heap_(addr left, addr right, addr *ret)
{
	return div_float_ss_alloc_(NULL, left, right, ret);
}

int div_float_sd_heap_(addr left, addr right, addr *ret)
{
	return div_float_sd_alloc_(NULL, left, right, ret);
}

int div_float_sl_heap_(addr left, addr right, addr *ret)
{
	return div_float_sl_alloc_(NULL, left, right, ret);
}

int div_float_ds_heap_(addr left, addr right, addr *ret)
{
	return div_float_ds_alloc_(NULL, left, right, ret);
}

int div_float_dd_heap_(addr left, addr right, addr *ret)
{
	return div_float_dd_alloc_(NULL, left, right, ret);
}

int div_float_dl_heap_(addr left, addr right, addr *ret)
{
	return div_float_dl_alloc_(NULL, left, right, ret);
}

int div_float_ls_heap_(addr left, addr right, addr *ret)
{
	return div_float_ls_alloc_(NULL, left, right, ret);
}

int div_float_ld_heap_(addr left, addr right, addr *ret)
{
	return div_float_ld_alloc_(NULL, left, right, ret);
}

int div_float_ll_heap_(addr left, addr right, addr *ret)
{
	return div_float_ll_alloc_(NULL, left, right, ret);
}

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#ifdef __clang__
#pragma clang diagnostic pop
#endif
