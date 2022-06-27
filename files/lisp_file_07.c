/*
 *  npt -- ANSI Common Lisp Programming Language.
 *    https://github.com/nptcl/npt
 *    https://github.com/nptcl/npt-amalgamation
 *
 *  File: lisp_file_07.c
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
#include <float.h>
#include <inttypes.h>
#include <math.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "lisp_file.h"


/************************************************************
 *  parse_macro.c
 ************************************************************/

/*
 *  environment root
 */
void environment_symbol(addr *ret)
{
	GetConst(SYSTEM_EVAL_PARSE_ENVIRONMENT, ret);
}

void init_parse_environment(Execute ptr)
{
	addr symbol, pos;

	environment_symbol(&symbol);
	heap_array2(&pos, LISPSYSTEM_ENVROOT, 2); /* global, root */
	pushspecial_control(ptr, symbol, pos);
}

static int getobject_envroot_(Execute ptr, addr *ret)
{
	addr pos;
	environment_symbol(&pos);
	return getspecialcheck_local_(ptr, pos, ret);
}

static void getglobal_envroot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ENVROOT);
	GetArrayA2(pos, 0, ret);
}
static void setglobal_envroot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_ENVROOT);
	SetArrayA2(pos, 0, value);
}

static void getlocal_envroot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ENVROOT);
	GetArrayA2(pos, 1, ret);
}
static void setlocal_envroot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_ENVROOT);
	SetArrayA2(pos, 1, value);
}


/*
 *  environment stack
 */
enum EnvStack_Index {
	EnvStack_next,
	EnvStack_call,
	EnvStack_lambda,
	EnvStack_size
};

enum EnvStackType {
	EnvStackType_macro,     /* macrolet */
	EnvStackType_symbol,    /* symbol-macro */
	EnvStackType_function,  /* function */
	EnvStackType_variable,  /* variable */
};

struct envstack_struct {
	enum EnvStackType type;
};

static void setnext_envstack(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_ENVSTACK);
	SetArraySS(pos, EnvStack_next, value);
}
static void getnext_envstack(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ENVSTACK);
	GetArraySS(pos, EnvStack_next, ret);
}

static void setcall_envstack(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_ENVSTACK);
	SetArraySS(pos, EnvStack_call, value);
}
static void getcall_envstack(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ENVSTACK);
	GetArraySS(pos, EnvStack_call, ret);
}

static void setlambda_envstack(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_ENVSTACK);
	SetArraySS(pos, EnvStack_lambda, value);
}
static void getlambda_envstack(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ENVSTACK);
	GetArraySS(pos, EnvStack_lambda, ret);
}

static void settype_envstack(addr pos, enum EnvStackType value)
{
	struct envstack_struct *ptr;

	CheckType(pos, LISPSYSTEM_ENVSTACK);
	ptr = (struct envstack_struct *)PtrBodySS(pos);
	ptr->type = value;
}
static void gettype_envstack(addr pos, enum EnvStackType *ret)
{
	struct envstack_struct *ptr;

	CheckType(pos, LISPSYSTEM_ENVSTACK);
	ptr = (struct envstack_struct *)PtrBodySS(pos);
	*ret = ptr->type;
}

static void envstack_heap(addr *ret,
		addr next, addr call, addr lambda, enum EnvStackType type)
{
	addr pos;

	heap_smallsize(&pos, LISPSYSTEM_ENVSTACK,
			EnvStack_size, sizeoft(struct envstack_struct));
	setnext_envstack(pos, next);
	setcall_envstack(pos, call);
	setlambda_envstack(pos, lambda);
	settype_envstack(pos, type);
	*ret = pos;
}

int snapshot_envstack_(Execute ptr, addr *ret)
{
	addr root;

	Return(getobject_envroot_(ptr, &root));
	getlocal_envroot(root, ret); /* local */

	return 0;
}

static int push_global_envstack_(Execute ptr,
		addr name, addr lambda, enum EnvStackType type)
{
	addr root, pos, next;

	Return(getobject_envroot_(ptr, &root));
	getglobal_envroot(root, &next);
	envstack_heap(&pos, next, name, lambda, type);
	setglobal_envroot(root, pos);

	return 0;
}

static int push_local_envstack_(Execute ptr,
		addr name, addr lambda, enum EnvStackType type)
{
	addr root, pos, next;

	Return(getobject_envroot_(ptr, &root));
	getlocal_envroot(root, &next);
	envstack_heap(&pos, next, name, lambda, type);
	setlocal_envroot(root, pos);

	return 0;
}

int rollback_envstack_(Execute ptr, addr pos)
{
	addr root, local, next;

	Return(getobject_envroot_(ptr, &root));
	for (;;) {
		getlocal_envroot(root, &local); /* local */
		if (local == pos)
			break;
		if (local == Nil)
			return fmte_("environment stack error.", NULL);
		getnext_envstack(local, &next);
		setnext_envstack(local, Nil);
		setlocal_envroot(root, next); /* local */
	}

	return 0;
}

int defvar_envstack_(Execute ptr, addr name)
{
	/* global, defvar */
	return push_global_envstack_(ptr, name, Nil, EnvStackType_variable);
}

int lexical_envstack_(Execute ptr, addr name)
{
	/* local, let */
	return push_local_envstack_(ptr, name, Nil, EnvStackType_variable);
}

int defun_envstack_(Execute ptr, addr name)
{
	/* global, defun */
	CheckType(name, LISPTYPE_CALLNAME);
	return push_global_envstack_(ptr, name, Nil, EnvStackType_function);
}

int function_envstack_(Execute ptr, addr name)
{
	/* local, flet/labels */
	CheckType(name, LISPTYPE_CALLNAME);
	return push_local_envstack_(ptr, name, Nil, EnvStackType_function);
}

int defmacro_envstack_(Execute ptr, addr name, addr lambda)
{
	/* global, macrolet */
	return push_global_envstack_(ptr, name, lambda, EnvStackType_macro);
}

int macrolet_envstack_(Execute ptr, addr name, addr lambda)
{
	/* local, macrolet */
	return push_local_envstack_(ptr, name, lambda, EnvStackType_macro);
}

int define_symbol_macro_envstack_(Execute ptr, addr name, addr form)
{
	/* global, define-symbol-macro */
	return push_global_envstack_(ptr, name, form, EnvStackType_symbol);
}

int symbol_macrolet_envstack_(Execute ptr, addr name, addr form)
{
	/* local, symbol-macrolet */
	return push_local_envstack_(ptr, name, form, EnvStackType_symbol);
}

static int find_macro_environment_p(addr name, addr check)
{
	Check(! symbolp(name), "type error");
	CheckType(check, LISPTYPE_CALLNAME);
	if (! symbolp_callname(check))
		return 0;
	GetCallName(check, &check);

	return name == check;
}

static int find_macro_environment(addr name, addr pos, addr *ret)
{
	enum EnvStackType type;
	addr check;

	while (pos != Nil) {
		gettype_envstack(pos, &type);
		if (type == EnvStackType_function) {
			/* shadow function */
			getcall_envstack(pos, &check);
			if (find_macro_environment_p(name, check)) {
				*ret = Unbound;
				return 1;
			}
		}
		if (type == EnvStackType_macro) {
			/* macro */
			getcall_envstack(pos, &check);
			if (name == check) {
				getlambda_envstack(pos, ret);
				return 1;
			}
		}
		getnext_envstack(pos, &pos);
	}

	return 0;
}

static int find_symbol_environment(addr name, addr pos, addr *ret)
{
	enum EnvStackType type;
	addr check;

	while (pos != Nil) {
		gettype_envstack(pos, &type);
		if (type == EnvStackType_variable) {
			/* shadow symbol-macro */
			getcall_envstack(pos, &check);
			if (name == check) {
				*ret = Unbound;
				return 1;
			}
		}
		if (type == EnvStackType_symbol) {
			/* symbol-macro */
			getcall_envstack(pos, &check);
			if (name == check) {
				getlambda_envstack(pos, ret);
				return 1;
			}
		}
		getnext_envstack(pos, &pos);
	}

	return 0;
}

static int get_symbol_macrolet_envstack_(Execute ptr, addr name, addr *value, int *ret)
{
	addr root, pos;

	Return(getobject_envroot_(ptr, &root));
	/* local */
	getlocal_envroot(root, &pos);
	if (find_symbol_environment(name, pos, value))
		return Result(ret, 1);
	/* global */
	getglobal_envroot(root, &pos);
	if (find_symbol_environment(name, pos, value))
		return Result(ret, 1);

	return Result(ret, 0);
}

int symbol_macrolet_envstack_p_(Execute ptr, addr name, addr *value, int *ret)
{
	int check;
	addr pos;

	/* environment */
	Return(get_symbol_macrolet_envstack_(ptr, name, &pos, &check));
	if (check) {
		if (pos == Unbound)
			return Result(ret, 0);
		*value = pos;
		return Result(ret, 1);
	}

	/* global symbol */
	getsymbol_macro_symbol(name, &pos);
	if (pos == Unbound)
		return Result(ret, 0);
	*value = pos;
	return Result(ret, 1);
}

static void getglobal_environment(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	GetArrayA2(pos, 0, ret);
}
static void setglobal_environment(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	SetArrayA2(pos, 0, value);
}

static void getlocal_environment(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	GetArrayA2(pos, 1, ret);
}
static void setlocal_environment(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	SetArrayA2(pos, 1, value);
}

static void getcheck_environment(addr pos, int *ret)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	*ret = GetUser(pos)? 1: 0;
}
static void setcheck_environment(addr pos, int value)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	SetUser(pos, value? 1: 0);
}

int environment_heap_(Execute ptr, addr *ret)
{
	addr pos, global, local;

	/* envstack */
	Return(getobject_envroot_(ptr, &pos));
	getglobal_envroot(pos, &global);
	getlocal_envroot(pos, &local);

	/* environment */
	heap_array2(&pos, LISPTYPE_ENVIRONMENT, 2);
	setglobal_environment(pos, global);
	setlocal_environment(pos, local);
	setcheck_environment(pos, 1); /* dynamic-extent check */

	return Result(ret, pos);
}

void copy_environment(addr *ret, addr pos)
{
	*ret = pos; /* do nothing */
}

void close_environment(addr pos)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	setglobal_environment(pos, Nil);
	setlocal_environment(pos, Nil);
	setcheck_environment(pos, 0);
}

static int closep_environment(addr pos)
{
	int check;

	CheckType(pos, LISPTYPE_ENVIRONMENT);
	getcheck_environment(pos, &check);
	return check == 0;
}

static int close_check_environment_(addr pos)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	if (closep_environment(pos))
		return fmte_("The environment object ~S is already closed.", pos, NULL);

	return 0;
}


/*
 *  macroexpand
 */
static int macroexpand1_symbol_find_(addr symbol, addr env, addr *ret)
{
	addr list;

	if (env != Nil) {
		Return(close_check_environment_(env));
		/* local */
		getlocal_environment(env, &list);
		if (find_symbol_environment(symbol, list, ret))
			return 0;
		/* global */
		getglobal_environment(env, &list);
		if (find_symbol_environment(symbol, list, ret))
			return 0;
	}
	getsymbol_macro_symbol(symbol, ret);

	return 0;
}

int find_environment_(addr symbol, addr env, addr *ret)
{
	addr list;

	if (! symbolp(symbol))
		return Result(ret, Unbound);
	if (env != Nil) {
		Return(close_check_environment_(env));
		/* local */
		getlocal_environment(env, &list);
		if (find_macro_environment(symbol, list, ret))
			return 0;
		/* global */
		getglobal_environment(env, &list);
		if (find_macro_environment(symbol, list, ret))
			return 0;
	}
	getmacro_symbol(symbol, ret);

	return 0;
}

static int call_macroexpand_hook_(Execute ptr,
		addr *ret, addr call, addr cons, addr env)
{
	addr hook;

	GetConst(SPECIAL_MACROEXPAND_HOOK, &hook);
	Return(getspecialcheck_local_(ptr, hook, &hook));
	return funcall1_control_(ptr, ret, hook, call, cons, env, NULL);
}

static int macroexpand1_symbol_(Execute ptr,
		addr *value, addr symbol, addr env, int *ret)
{
	addr call, pos;

	Return(macroexpand1_symbol_find_(symbol, env, &pos));
	if (pos == Unbound)
		return Result(ret, 0);
	GetConst(SYSTEM_SYMBOL_MACRO_EXPANDER, &call);
	GetFunctionSymbol(call, &call);
	Return(call_macroexpand_hook_(ptr, value, call, pos, env));
	return Result(ret, 1);
}

static int macroexpand1_function_(Execute ptr,
		addr *value, addr form, addr env, int *ret)
{
	addr call;

	GetCar(form, &call);
	Return(find_environment_(call, env, &call));
	if (call == Unbound)
		return Result(ret, 0);
	Return(call_macroexpand_hook_(ptr, value, call, form, env));
	return Result(ret, 1);
}

int macroexpand1_(Execute ptr, addr *ret, addr form, addr env, int *result)
{
	if (symbolp(form))
		return macroexpand1_symbol_(ptr, ret, form, env, result);
	if (consp(form))
		return macroexpand1_function_(ptr, ret, form, env, result);
	*ret = form;
	*result = 0;
	return 0;
}

int macroexpand_(Execute ptr, addr *ret, addr form, addr env, int *result)
{
	int check, value;
	addr pos, fail;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	check = 0;
	fail = form;
	for (;;) {
		Return(macroexpand1_(ptr, &pos, form, env, &value));
		if (value == 0)
			break;
		check = 1;
		form = pos;
		localhold_set(hold, 0, form);
	}
	localhold_end(hold);
	*ret = check? pos: fail;
	*result = check;

	return 0;
}


/*
 *  macro eval-when
 */
#define ParseMacroCompile(pos, x) { \
	addr __check; \
	GetConst(COMMON_##x, &__check); \
	if (pos == __check) \
	return 1; \
}
static int parse_macro_compile_symbol(addr pos)
{
	ParseMacroCompile(pos, DEFCONSTANT);
	ParseMacroCompile(pos, DECLAIM);
	ParseMacroCompile(pos, DEFCLASS);
	ParseMacroCompile(pos, DEFINE_COMPILER_MACRO);
	ParseMacroCompile(pos, DEFINE_CONDITION);
	ParseMacroCompile(pos, DEFINE_MODIFY_MACRO);
	ParseMacroCompile(pos, DEFINE_SETF_EXPANDER);
	ParseMacroCompile(pos, DEFINE_SYMBOL_MACRO);
	ParseMacroCompile(pos, DEFMACRO);
	ParseMacroCompile(pos, DEFPACKAGE);
	ParseMacroCompile(pos, DEFSETF);
	ParseMacroCompile(pos, DEFSTRUCT);
	ParseMacroCompile(pos, DEFTYPE);
	ParseMacroCompile(pos, IN_PACKAGE);

	return 0;
}

static void parse_make_eval_when(addr compile, addr load, addr execute, addr *ret)
{
	addr list, key;

	list = Nil;
	if (compile != Nil) {
		GetConst(KEYWORD_COMPILE_TOPLEVEL, &key);
		cons_heap(&list, key, list);
	}
	if (load != Nil) {
		GetConst(KEYWORD_LOAD_TOPLEVEL, &key);
		cons_heap(&list, key, list);
	}
	if (execute != Nil) {
		GetConst(KEYWORD_EXECUTE, &key);
		cons_heap(&list, key, list);
	}

	*ret = list;
}

static int parse_macro_eval_when_(Execute ptr, addr expr, addr list, addr *ret)
{
	addr compile, load, exec, toplevel, mode, eval;

	/* compile */
	if (! eval_compile_p(ptr))
		goto return_throw;

	/* type */
	if (! consp(expr))
		goto return_throw;
	GetCar(expr, &expr);
	if (! parse_macro_compile_symbol(expr))
		goto return_throw;

	/* toplevel */
	Return(get_toplevel_eval_(ptr, &toplevel));
	if (toplevel == Nil)
		goto return_throw;

	/* :compile-toplevel */
	Return(get_compile_toplevel_eval_(ptr, &compile));
	if (compile != Nil)
		goto return_throw;

	/* compile-time-too */
	Return(get_compile_time_eval_(ptr, &mode));
	if (mode != Nil)
		goto return_throw;

	/* eval-when */
	Return(get_load_toplevel_eval_(ptr, &load));
	Return(get_execute_eval_(ptr, &exec));

	/* `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body) */
	GetConst(COMMON_EVAL_WHEN, &eval);
	parse_make_eval_when(compile, load, exec, &expr);
	list_heap(&list, eval, expr, list, NULL);

return_throw:
	return Result(ret, list);
}


/*
 *  parse-macroexpand
 */
static int parse_macroexpand_local_(Execute ptr, addr *ret, addr form, addr env)
{
	addr call, list;

	/* environment */
	if (env == Nil)
		goto unbound;
	Return(close_check_environment_(env));
	getlocal_environment(env, &list);

	/* local */
	GetCar(form, &call);
	if (! find_macro_environment(call, list, &call))
		goto unbound;
	if (call == Unbound)
		goto unbound;

	/* execute */
	return call_macroexpand_hook_(ptr, ret, call, form, env);

unbound:
	return Result(ret, Unbound);
}

static int parse_macroexpand_compile_(Execute ptr, addr *ret, addr form, addr env)
{
	int check;
	addr call;

	/* *compiler-macro* */
	if (! enable_compiler_macro_p(ptr))
		goto unbound;

	/* define-compiler-macro */
	GetCar(form, &call);
	get_compiler_macro_symbol(call, &call);
	if (call == Nil)
		goto unbound;

	/* execute */
	Return(call_macroexpand_hook_(ptr, &call, call, form, env));

	/* equal check */
	Return(equal_function_(form, call, &check));
	if (check)
		goto unbound;
	return Result(ret, call);

unbound:
	return Result(ret, Unbound);
}

static int parse_macroexpand_global1_(Execute ptr, addr *ret, addr form, addr env)
{
	addr call, list;

	/* environment */
	if (env == Nil)
		goto unbound;
	Return(close_check_environment_(env));
	getglobal_environment(env, &list);

	/* global */
	GetCar(form, &call);
	if (! find_macro_environment(call, list, &call))
		goto unbound;
	if (call == Unbound)
		goto unbound;

	/* execute */
	return call_macroexpand_hook_(ptr, ret, call, form, env);

unbound:
	return Result(ret, Unbound);
}

static int parse_macroexpand_global2_(Execute ptr, addr *ret, addr form, addr env)
{
	addr call;

	GetCar(form, &call);
	getmacro_symbol(call, &call);
	if (call == Unbound)
		return Result(ret, Unbound);

	return call_macroexpand_hook_(ptr, ret, call, form, env);
}

static int parse_macroexpand_function_(Execute ptr, addr *ret, addr form, addr env)
{
	addr pos;

	/* calltype */
	GetCar(form, &pos);
	if (! symbolp(pos))
		return Result(ret, Unbound);

	/* macrolet [env.local] */
	Return(parse_macroexpand_local_(ptr, &pos, form, env));
	if (pos != Unbound)
		return Result(ret, pos);

	/* define-compiler-macro */
	Return(parse_macroexpand_compile_(ptr, &pos, form, env));
	if (pos != Unbound)
		return Result(ret, pos);

	/* defmacro [env.global] */
	Return(parse_macroexpand_global1_(ptr, &pos, form, env));
	if (pos != Unbound)
		return Result(ret, pos);

	/* defmacro [symbol] */
	return parse_macroexpand_global2_(ptr, ret, form, env);
}

static int parse_macroexpand_symbol_(Execute ptr, addr *ret, addr form, addr env)
{
	int check;
	Return(macroexpand1_symbol_(ptr, &form, form, env, &check));
	return Result(ret, check? form: Unbound);
}

static int parse_macroexpand1_call_(Execute ptr, addr *ret, addr form, addr env)
{
	if (symbolp(form))
		return parse_macroexpand_symbol_(ptr, ret, form, env);
	if (consp(form))
		return parse_macroexpand_function_(ptr, ret, form, env);

	return Result(ret, Unbound);
}

static int parse_macroexpand1_(Execute ptr, addr *ret, addr form, addr env)
{
	addr value;

	Return(parse_macroexpand1_call_(ptr, &value, form, env));
	if (value == Unbound)
		return Result(ret, Unbound);

	return parse_macro_eval_when_(ptr, form, value, ret);
}

static int parse_macroexpand_loop_(Execute ptr, addr *ret, addr form, addr env)
{
	int check;
	addr pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (check = 0; ; check = 1) {
		Return(parse_macroexpand1_(ptr, &pos, form, env));
		if (pos == Unbound)
			break;
		form = pos;
		localhold_set(hold, 0, form);
	}
	localhold_end(hold);

	return Result(ret, check? form: Unbound);
}

int parse_macroexpand_(Execute ptr, addr *ret, addr form)
{
	addr env;
	LocalHold hold;

	/* macroexpand */
	Return(environment_heap_(ptr, &env));
	hold = LocalHold_local_push(ptr, env);
	Return(parse_macroexpand_loop_(ptr, ret, form, env));
	close_environment(env);
	localhold_end(hold);

	return 0;
}


/************************************************************
 *  parse_object.c
 ************************************************************/

/*
 *  access
 */
struct parse_struct *structevalparse(addr pos)
{
	Check(! eval_parse_p(pos), "type error");
	return StructEvalParse_Low(pos);
}
addr refevalparse(addr pos, size_t index)
{
	Check(! eval_parse_p(pos), "type error");
	return RefEvalParse_Low(pos, index);
}
void getevalparse(addr pos, size_t index, addr *ret)
{
	Check(! eval_parse_p(pos), "type error");
	GetEvalParse_Low(pos, index, ret);
}
void setevalparse(addr pos, size_t index, addr value)
{
	Check(! eval_parse_p(pos), "type error");
	SetEvalParse_Low(pos, index, value);
}
EvalParse refevalparsetype(addr pos)
{
	Check(! eval_parse_p(pos), "type error");
	return RefEvalParseType_Low(pos);
}
void getevalparsetype(addr pos, EvalParse *ret)
{
	Check(! eval_parse_p(pos), "type error");
	GetEvalParseType_Low(pos, ret);
}
void setevalparsetype(addr pos, EvalParse value)
{
	Check(! eval_parse_p(pos), "type error");
	SetEvalParseType_Low(pos, value);
}


/*
 *  memory
 */
void eval_parse_alloc(LocalRoot local, addr *ret, EvalParse type, byte array)
{
	addr pos;

	Check(0xFF < sizeof(struct parse_struct), "struct size error");
	eval_alloc(local, &pos, EVAL_TYPE_PARSE, array, sizeoft(struct parse_struct));
	SetEvalParseType(pos, type);

	*ret = pos;
}
void eval_parse_local(LocalRoot local, addr *ret, EvalParse type, byte array)
{
	Check(local == NULL, "local error");
	eval_parse_alloc(local, ret, type, array);
}
void eval_parse_heap(addr *ret, EvalParse type, byte array)
{
	eval_parse_alloc(NULL, ret, type, array);
}

void eval_single_parse_alloc(LocalRoot local, addr *ret, EvalParse type, addr value)
{
	eval_parse_alloc(local, ret, type, 1);
	SetEvalParse(*ret, 0, value);
}
void eval_single_parse_local(LocalRoot local, addr *ret, EvalParse type, addr value)
{
	Check(local == NULL, "local error");
	eval_single_parse_alloc(local, ret, type, value);
}
void eval_single_parse_heap(addr *ret, EvalParse type, addr value)
{
	eval_single_parse_alloc(NULL, ret, type, value);
}

void eval_parse2_alloc(LocalRoot local, addr *ret, EvalParse type, addr x, addr y)
{
	addr pos;

	eval_parse_alloc(local, &pos, type, 2);
	SetEvalParse(pos, 0, x);
	SetEvalParse(pos, 1, y);
	*ret = pos;
}
void eval_parse2_local(LocalRoot local, addr *ret, EvalParse type, addr x, addr y)
{
	Check(local == NULL, "local error");
	eval_parse2_alloc(local, ret, type, x, y);
}
void eval_parse2_heap(addr *ret, EvalParse type, addr x, addr y)
{
	eval_parse2_alloc(NULL, ret, type, x, y);
}


/************************************************************
 *  pathname.c
 ************************************************************/

/*
 *  parser table
 */
static int pathname_system_unix_p(addr pos)
{
	addr check;
	GetConst(SYSTEM_UNIX, &check);
	return pos == check;
}

static int pathname_system_windows_p(addr pos)
{
	addr check;
	GetConst(SYSTEM_WINDOWS, &check);
	return pos == check;
}


/*
 *  parse-namestring
 */
static int parser_struct_pathname_(struct fileparse *pa)
{
	addr host;

#ifdef LISP_DEBUG
	size_t size;
	string_length(pa->thing, &size);
	Check(pa->end < pa->start || size < pa->end, "size error");
#endif

	host = pa->host;
	if (host == Nil)
		GetHostPathname(pa->path, &host);

	/* unix */
	if (pathname_system_unix_p(host))
		return parser_unix_pathname_(pa);

	/* windows */
	if (pathname_system_windows_p(host))
		return parser_windows_pathname_(pa);

	/* logical pathname */
	if (stringp(host))
		return parser_logical_pathname_(pa);

	/* error */
	return fmte_("Unknown pathname-host ~S.", host, NULL);
}

static int defaults_pathname_alloc_(Execute ptr, addr *ret, addr defaults, int localp)
{
	if (defaults == Nil || defaults == Unbound) {
		GetConst(SPECIAL_DEFAULT_PATHNAME_DEFAULTS, &defaults);
		Return(getspecialcheck_local_(ptr, defaults, &defaults));
	}
	return pathname_designator_alloc_(ptr, defaults, ret, localp);
}

int defaults_pathname_heap_(Execute ptr, addr *ret, addr defaults)
{
	return defaults_pathname_alloc_(ptr, ret, defaults, 0);
}

static void parse_directory_pathname(addr pos)
{
	addr list, x, y;

	/* junk */
	if (pos == Nil)
		return;

	/* (:relative) -> nil */
	GetDirectoryPathname(pos, &list);
	if (! consp(list))
		return;
	if (! consp_getcons(list, &x, &list))
		return;
	if (list != Nil)
		return;
	GetConst(KEYWORD_RELATIVE, &y);
	if (x != y)
		return;
	SetDirectoryPathname(pos, Nil);
}

static int parse_pathname_full_alloc_(Execute ptr,
		addr thing, addr host, addr defaults, size_t start, size_t end, int junk,
		addr *ret, size_t *pos, int localp, int errorp)
{
	LocalStack stack;
	struct fileparse pa;

	/* argument */
	Return(defaults_pathname_alloc_(ptr, &defaults, defaults, localp));
	init_fileparse(&pa, ptr, localp);
	pa.thing = thing;
	pa.path = defaults;
	pa.host = host;
	pa.start = start;
	pa.end = end;
	pa.junk = junk? 1: 0;
	pa.errorp = errorp;

	/* logical-pathname */
	if (host == Nil && pathname_logical_p(defaults)) {
		GetHostPathname(defaults, &host);
		pa.host = host;
	}

	/* execute */
	push_localp(pa.local, &stack);
	Return(parser_struct_pathname_(&pa));
	parse_directory_pathname(pa.result);
	*ret = pa.result;
	*pos = pa.endpos;
	rollback_localp(pa.local, stack);

	return 0;
}

int parse_pathname_full_heap_(Execute ptr, addr thing, addr host,
		addr defaults, size_t start, size_t end, int junk, addr *ret, size_t *pos)
{
	return parse_pathname_full_alloc_(ptr,
			thing, host, defaults, start, end, junk, ret, pos, 0, 1);
}

static int parse_pathname_alloc_(Execute ptr, addr thing, addr *ret, int localp)
{
	size_t end;
	string_length(thing, &end);
	return parse_pathname_full_alloc_(ptr,
			thing, Nil, Nil, 0, end, 0, ret, &end, localp, 1);
}

static int parse_pathname_heap_(Execute ptr, addr thing, addr *ret)
{
	size_t end;
	string_length(thing, &end);
	return parse_pathname_full_heap_(ptr, thing, Nil, Nil, 0, end, 0, ret, &end);
}

static int parse_pathname_host_heap_(Execute ptr, addr thing, addr host, addr *ret)
{
	size_t end;
	string_length(thing, &end);
	return parse_pathname_full_heap_(ptr, thing, host, Nil, 0, end, 0, ret, &end);
}

int parse_pathname_setf_heap_(Execute ptr, addr thing, addr host, addr *ret)
{
	size_t end;
	string_length(thing, &end);
	return parse_pathname_full_alloc_(ptr,
			thing, host, Nil, 0, end, 0, ret, &end, 0, 0);
}

int parse_pathname_char_heap_(Execute ptr, const char *str, addr *ret)
{
	addr thing;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	strvect_char_local(local, &thing, str);
	Return(parse_pathname_heap_(ptr, thing, ret));
	rollback_local(local, stack);

	return 0;
}

int pathname_designator_alloc_(Execute ptr, addr pos, addr *ret, int localp)
{
	addr value;
	LocalRoot local;

	/* pathname */
	local = localp? ptr->local: NULL;
	if (pathnamep(pos)) {
		copylocal_object(local, ret, pos);
		return 0;
	}

	/* stream */
	if (streamp(pos)) {
		GetPathnameStream(pos, &value);
		Return(pathname_designator_alloc_(ptr, value, &value, localp));
		copylocal_object(local, ret, value);
		return 0;
	}

	/* string */
	if (stringp(pos)) {
		Return(parse_pathname_alloc_(ptr, pos, &value, localp));
		Return(pathname_designator_alloc_(ptr, value, &value, localp));
		return Result(ret, value);
	}

	/* type-error */
	return TypeError_(pos, PATHNAME);
}

int pathname_designator_heap_(Execute ptr, addr pos, addr *ret)
{
	return pathname_designator_alloc_(ptr, pos, ret, 0);
}

int pathname_designator_local_(Execute ptr, addr pos, addr *ret)
{
	return pathname_designator_alloc_(ptr, pos, ret, 1);
}


/*
 *  physical-pathname
 */
int physical_pathname_alloc_(Execute ptr, addr pos, addr *ret, int localp)
{
	int check;
	LocalRoot local;
	addr host, list, left, right, value;

	/* physical pathname */
	local = localp? ptr->local: NULL;
	Return(pathname_designator_alloc_(ptr, pos, &pos, localp));
	if (! RefLogicalPathname(pos)) {
		copylocal_object(local, ret, pos);
		return 0;
	}

	/* logical pathname */
	GetHostPathname(pos, &host);
	Return(gethost_logical_pathname_(host, &list));
	if (list == Nil)
		return fmte_("The logical-hostname ~S is not exist.", host, NULL);
	while (list != Nil) {
		GetCons(list, &right, &list);
		List_bind(right, &left, &value, NULL);
		Return(wildcard_pathname_(pos, left, 1, &check));
		if (check) {
			Return(translate_pathname_alloc_(ptr, &pos, pos, left, value, localp));
			return physical_pathname_alloc_(ptr, pos, ret, localp);
		}
	}
	return fmte_("The logical-pathname ~S don't match translate table.", pos, NULL);
}

int physical_pathname_heap_(Execute ptr, addr pos, addr *ret)
{
	return physical_pathname_alloc_(ptr, pos, ret, 0);
}

int physical_pathname_local_(Execute ptr, addr pos, addr *ret)
{
	return physical_pathname_alloc_(ptr, pos, ret, 1);
}


/*
 *  file-namestring
 */
static int file_namestring_filename_(LocalpRoot local, addr pos, addr queue)
{
	LocalRoot alloc;
	addr right, wild;

	/* name */
	alloc = local->local;
	GetConst(KEYWORD_WILD, &wild);
	GetNamePathname(pos, &right);
	if (right != Nil) {
		if (right == wild) {
			Return(pushchar_charqueue_local_(alloc, queue, "*"));
		}
		else if (right != Nil) {
			Return(pushstring_charqueue_local_(alloc, queue, right));
		}
	}

	/* type */
	GetTypePathname(pos, &right);
	if (right != Nil) {
		Return(push_charqueue_local_(alloc, queue, '.'));
		if (right == wild) {
			Return(pushchar_charqueue_local_(alloc, queue, "*"));
		}
		else {
			Return(pushstring_charqueue_local_(alloc, queue, right));
		}
	}

	return 0;
}

static int file_pathname_namestring_(LocalpRoot local, addr *ret, addr pos)
{
	addr host, queue;

	GetHostPathname(pos, &host);
	if (! pathname_system_unix_p(host) && ! pathname_system_windows_p(host))
		return fmte_("Unknown pathname-host ~S.", host, NULL);

	charqueue_local(local->local, &queue, 0);
	Return(file_namestring_filename_(local, pos, queue));
	make_charqueue_alloc(localp_alloc(local), queue, ret);

	return 0;
}

static int logical_namestring_version_(LocalpRoot local, addr queue, addr right)
{
	LocalRoot alloc;
	addr wild, newest;

	alloc = local->local;
	/* nil */
	if (right == Nil)
		return 0;
	/* newest */
	GetConst(KEYWORD_NEWEST, &newest);
	if (right == newest)
		return 0;
	/* others */
	Return(push_charqueue_local_(alloc, queue, '.'));
	GetConst(KEYWORD_WILD, &wild);
	if (right == wild)
		return pushchar_charqueue_local_(alloc, queue, "*");
	else if (integerp(right))
		return decimal_charqueue_integer_local_(alloc, right, queue);
	else
		return fmte_("Invalid version value ~S.", right, NULL);
}

static int file_logical_namestring_(LocalpRoot local, addr *ret, addr pos)
{
	addr queue, right;

	charqueue_local(local->local, &queue, 0);
	Return(file_namestring_filename_(local, pos, queue));
	GetVersionPathname(pos, &right);
	if (right != Nil) {
		Return(logical_namestring_version_(local, queue, right));
	}
	make_charqueue_alloc(localp_alloc(local), queue, ret);

	return 0;
}

static int file_name_pathname_alloc_(LocalpRoot local, addr pos, addr *ret)
{
	LocalStack stack;

	push_localp(local, &stack);
	if (RefLogicalPathname(pos)) {
		Return(file_logical_namestring_(local, ret, pos));
	}
	else {
		Return(file_pathname_namestring_(local, ret, pos));
	}
	rollback_localp(local, stack);

	return 0;
}

int file_name_pathname_heap_(LocalRoot local, addr pos, addr *ret)
{
	struct localp_struct buffer;

	Check(local == NULL, "local error");
	buffer.localp = 0;
	buffer.local = local;
	return file_name_pathname_alloc_(&buffer, pos, ret);
}

int file_name_pathname_local_(LocalRoot local, addr pos, addr *ret)
{
	struct localp_struct buffer;

	Check(local == NULL, "local error");
	buffer.localp = 1;
	buffer.local = local;
	return file_name_pathname_alloc_(&buffer, pos, ret);
}


/*
 *  directory-namestring
 */
static int directory_namestring_filename_(LocalpRoot local,
		addr pos, addr queue, unicode split, int logicalp)
{
	LocalRoot alloc;
	addr left, right, absolute, relative, wild, wildi, up;

	alloc = local->local;
	GetConst(KEYWORD_ABSOLUTE, &absolute);
	GetConst(KEYWORD_RELATIVE, &relative);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wildi);
	GetConst(KEYWORD_UP, &up);

	/* nil */
	GetDirectoryPathname(pos, &right);
	if (right == Nil) {
		if (logicalp) {
			Return(push_charqueue_local_(alloc, queue, split));
		}
		return 0;
	}

	/* cons */
	if (! consp(right))
		return fmte_("Invalid directory ~S.", right, NULL);
	GetCons(right, &left, &right);
	if (left == absolute) {
		if (! logicalp) {
			Return(push_charqueue_local_(alloc, queue, split));
		}
	}
	else if (left == relative) {
		if (logicalp) {
			Return(push_charqueue_local_(alloc, queue, split));
		}
	}
	else {
		return fmte_("Invalid directory type ~S.", left, NULL);
	}
	while (right != Nil) {
		GetCons(right, &left, &right);
		if (left == wild) {
			Return(pushchar_charqueue_local_(alloc, queue, "*"));
		}
		else if (left == wildi) {
			Return(pushchar_charqueue_local_(alloc, queue, "**"));
		}
		else if (left == up) {
			Return(pushchar_charqueue_local_(alloc, queue, ".."));
		}
		else {
			Return(pushstring_charqueue_local_(alloc, queue, left));
		}
		Return(push_charqueue_local_(alloc, queue, split));
	}

	return 0;
}

static int directory_pathname_namestring_(LocalpRoot local, addr *ret, addr pos)
{
	addr host, queue;

	charqueue_local(local->local, &queue, 0);
	GetHostPathname(pos, &host);
	if (pathname_system_unix_p(host)) {
		Return(directory_namestring_filename_(local, pos, queue, '/', 0));
	}
	else if (pathname_system_windows_p(host)) {
		Return(directory_namestring_filename_(local, pos, queue, '\\', 0));
	}
	else {
		return fmte_("Unknown pathname-host ~S.", host, NULL);
	}
	make_charqueue_alloc(localp_alloc(local), queue, ret);

	return 0;
}

static int directory_logical_namestring_(LocalpRoot local, addr *ret, addr pos)
{
	addr queue;

	charqueue_local(local->local, &queue, 0);
	Return(directory_namestring_filename_(local, pos, queue, ';', 1));
	make_charqueue_alloc(localp_alloc(local), queue, ret);

	return 0;
}

static int directory_name_pathname_alloc_(LocalpRoot local, addr pos, addr *ret)
{
	LocalStack stack;

	push_localp(local, &stack);
	if (RefLogicalPathname(pos)) {
		Return(directory_logical_namestring_(local, ret, pos));
	}
	else {
		Return(directory_pathname_namestring_(local, ret, pos));
	}
	rollback_localp(local, stack);

	return 0;
}

int directory_name_pathname_heap_(LocalRoot local, addr pos, addr *ret)
{
	struct localp_struct buffer;

	Check(local == NULL, "local error");
	buffer.localp = 0;
	buffer.local = local;
	return directory_name_pathname_alloc_(&buffer, pos, ret);
}

int directory_name_pathname_local_(LocalRoot local, addr pos, addr *ret)
{
	struct localp_struct buffer;

	Check(local == NULL, "local error");
	buffer.localp = 1;
	buffer.local = local;
	return directory_name_pathname_alloc_(&buffer, pos, ret);
}


/*
 *  namestring
 */
static int namestring_filename_(LocalpRoot local,
		addr pos, addr queue, unicode split, int logicalp)
{
	Return(directory_namestring_filename_(local, pos, queue, split, logicalp));
	return file_namestring_filename_(local, pos, queue);
}

static int namestring_unix_(LocalpRoot local, addr *ret, addr pos)
{
	addr queue;

	charqueue_local(local->local, &queue, 0);
	Return(namestring_filename_(local, pos, queue, '/', 0));
	make_charqueue_alloc(localp_alloc(local), queue, ret);

	return 0;
}

static int namestring_windows_(LocalpRoot local, addr *ret, addr pos)
{
	LocalRoot alloc;
	addr device, queue, universal, file, check;

	alloc = local->local;
	GetConst(SYSTEM_UNIVERSAL, &universal);
	GetConst(SYSTEM_DEVICE, &file);
	GetDevicePathname(pos, &device);
	charqueue_local(alloc, &queue, 0);
	if (device == universal) {
		Return(push_charqueue_local_(alloc, queue, '\\'));
		Return(namestring_filename_(local, pos, queue, '\\', 0));
	}
	else if (device == file) {
		Return(push_charqueue_local_(alloc, queue, '\\'));
		Return(push_charqueue_local_(alloc, queue, '\\'));
		Return(push_charqueue_local_(alloc, queue, '.'));
		Return(push_charqueue_local_(alloc, queue, '\\'));
		GetNamePathname(pos, &check);
		if (check != Nil) {
			Return(pushstring_charqueue_local_(alloc, queue, check));
		}
	}
	else if (device != Nil) {
		Return(pushstring_charqueue_local_(alloc, queue, device));
		Return(push_charqueue_local_(alloc, queue, ':'));
		Return(namestring_filename_(local, pos, queue, '\\', 0));
	}
	else {
		Return(namestring_filename_(local, pos, queue, '\\', 0));
	}
	make_charqueue_alloc(localp_alloc(local), queue, ret);

	return 0;
}

static int pathname_namestring_(LocalpRoot local, addr *ret, addr pos)
{
	addr host;

	GetHostPathname(pos, &host);
	if (pathname_system_unix_p(host))
		return namestring_unix_(local, ret, pos);
	else if (pathname_system_windows_p(host))
		return namestring_windows_(local, ret, pos);
	else
		return fmte_("Unknown pathname-host ~S.", host, NULL);
}

static int logical_namestring_(LocalpRoot local, addr *ret, addr pos)
{
	LocalRoot alloc;
	addr queue, right;

	alloc = local->local;
	charqueue_local(alloc, &queue, 0);
	/* host */
	GetHostPathname(pos, &right);
	Return(pushstring_charqueue_local_(alloc, queue, right));
	Return(pushchar_charqueue_local_(alloc, queue, ":"));
	/* directory, name, type */
	Return(namestring_filename_(local, pos, queue, ';', 1));
	/* version */
	GetVersionPathname(pos, &right);
	if (right != Nil) {
		Return(logical_namestring_version_(local, queue, right));
	}
	/* result */
	make_charqueue_alloc(localp_alloc(local), queue, ret);

	return 0;
}

static int name_pathname_alloc_(Execute ptr, LocalpRoot local, addr pos, addr *ret)
{
	LocalStack stack;

	push_localp(local, &stack);
	Return(pathname_designator_alloc_(ptr, pos, &pos, local->localp));
	if (RefLogicalPathname(pos)) {
		Return(logical_namestring_(local, ret, pos));
	}
	else {
		Return(pathname_namestring_(local, ret, pos));
	}
	rollback_localp(local, stack);

	return 0;
}

int name_pathname_heap_(Execute ptr, addr pos, addr *ret)
{
	struct localp_struct buffer;

	buffer.localp = 0;
	buffer.local = ptr->local;
	return name_pathname_alloc_(ptr, &buffer, pos, ret);
}

int name_pathname_local_(Execute ptr, addr pos, addr *ret)
{
	struct localp_struct buffer;

	buffer.localp = 1;
	buffer.local = ptr->local;
	return name_pathname_alloc_(ptr, &buffer, pos, ret);
}

int name_physical_heap_(Execute ptr, addr pos, addr *ret)
{
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	Return(physical_pathname_local_(ptr, pos, &pos));
	Return(name_pathname_heap_(ptr, pos, ret));
	rollback_local(local, stack);

	return 0;
}

int name_physical_local_(Execute ptr, addr pos, addr *ret)
{
	Return(physical_pathname_local_(ptr, pos, &pos));
	return name_pathname_local_(ptr, pos, ret);
}


/*
 *  merge-pathnames
 */
static void list_merge_directory(addr pos, addr defpath, addr *ret)
{
	addr cons, left;

	cons = Nil;
	while (defpath != Nil) {
		GetCons(defpath, &left, &defpath);
		cons_heap(&cons, left, cons);
	}
	while (pos != Nil) {
		GetCons(pos, &left, &pos);
		cons_heap(&cons, left, cons);
	}
	nreverse(ret, cons);
}

static int check_merge_directory(addr pos, addr defpath)
{
	addr relative;

	if (consp(pos) && consp(defpath)) {
		GetCar(pos, &pos);
		GetConst(KEYWORD_RELATIVE, &relative);
		return pos == relative;
	}

	return 0;
}

static void merge_directory(addr pos, addr defpath, addr *ret)
{
	if (check_merge_directory(pos, defpath)) {
		GetCdr(pos, &pos);
		list_merge_directory(pos, defpath, ret);
	}
	else if (pos == Nil) {
		*ret = defpath;
	}
	else {
		*ret = pos;
	}
}

static void merge_pathname_array(addr pos,
		enum PATHNAME_INDEX type, addr defpath, addr *ret)
{
	GetArrayPathname(pos, type, &pos);
	if (pos == Nil) {
		GetArrayPathname(defpath, type, ret);
	}
	else {
		*ret = pos;
	}
}

static void merge_pathname_object(addr pos, addr defaults, addr defver, addr *ret)
{
	addr check1, check2;
	addr host, device, directory, name, type, version;

	host = device = directory = name = type = version = Nil;
	/* If pathname does not specify a host, device, directory, name, or type,
	 * each such component is copied from default-pathname.
	 */
	merge_pathname_array(pos, PATHNAME_INDEX_HOST, defaults, &host);
	merge_pathname_array(pos, PATHNAME_INDEX_DEVICE, defaults, &device);
	merge_pathname_array(pos, PATHNAME_INDEX_NAME, defaults, &name);
	merge_pathname_array(pos, PATHNAME_INDEX_TYPE, defaults, &type);
	GetVersionPathname(pos, &version);

	/* If no version is supplied, default-version is used.
	 * If default-version is nil, the version component will remain unchanged.
	 */
	if (version == Nil)
		version = defver;

	/* If pathname does not specify a name, then the version, if not provided,
	 * will come from default-pathname, just like the other components.
	 */
	GetNamePathname(pos, &check1);
	if (check1 == Nil && version == Nil)
		GetVersionPathname(defaults, &version);

	/* If pathname does specify a name, then the version is not affected
	 * by default-pathname.
	 */
	if (check1 != Nil)
		GetVersionPathname(pos, &version);

	/* If this process leaves the version missing, the default-version is used.  */
	if (version == Nil)
		version = defver;

	/* directory */
	GetDirectoryPathname(pos, &check1);
	GetDirectoryPathname(defaults, &check2);
	merge_directory(check1, check2, &directory);

	/* make pathname */
	if (RefLogicalPathname(pos))
		logical_pathname_heap(ret, host, directory, name, type, version);
	else
		pathname_heap(ret, host, device, directory, name, type);
}

int merge_pathnames_clang_(Execute ptr,
		addr pos, addr defaults, addr defver, addr *ret)
{
	addr host;

	/* logical-pathname namestring */
	Return(defaults_pathname_heap_(ptr, &defaults, defaults));
	if (stringp(pos) && RefLogicalPathname(defaults)) {
		GetHostPathname(pos, &host);
		Return(parse_pathname_host_heap_(ptr, pos, host, &pos));
	}
	Return(pathname_designator_heap_(ptr, pos, &pos));

	/* version */
	if (defver == Unbound)
		defver = Nil;

	/* merge */
	merge_pathname_object(pos, defaults, defver, ret);

	return 0;
}


/*
 *  initialize
 */
void build_pathname(void)
{
	build_pathname_translate();
}


/************************************************************
 *  pathname_localp.c
 ************************************************************/

void push_localp(LocalpRoot local, LocalStack *ret)
{
	if (local->localp)
		*ret = NULL;
	else
		push_local(local->local, ret);
}

void rollback_localp(LocalpRoot local, LocalStack stack)
{
	if (! local->localp)
		rollback_local(local->local, stack);
}

LocalRoot localp_alloc(LocalpRoot local)
{
	return local->localp? local->local: NULL;
}


/************************************************************
 *  pathname_logical.c
 ************************************************************/

/*
 *  logical-pathname
 */
static int parser_logical_pathname_wordaster_p(unicode c)
{
	return isAlphanumeric(c) || (c == '-') || (c == '*');
}

static int parser_logical_host_(struct fileparse *pa, addr queue)
{
	int check;
	LocalRoot local;
	addr pos;

	local = localp_alloc(pa->local);
	make_charqueue_alloc(local, queue, &pos);
	Return(string_upper_p_(pos, &check));
	if (! check) {
		Return(string_upper_alloc_(local, pos, &pos));
	}
	pa->host = pos;

	return 0;
}

int parser_logical_pathname_(struct fileparse *pa)
{
	int absolute, relative, dp1, dp2;
	unicode c;
	LocalpRoot local;
	addr charqueue, queue, thing, temp;
	size_t i, size;

	/* initialize */
	local = pa->local;
	thing = pa->thing;
	if (! stringp(thing))
		return TypeError_(thing, STRING);
	charqueue_local(local->local, &charqueue, 0);
	pa->queue = charqueue;
	queue = Nil;
	size = pa->end;
	absolute = relative = dp1 = dp2 = 0;
	i = pa->start;
	pa->version = Nil;

	/* start */
	if (size <= i)
		goto finish;
	Return(string_getc_(thing, i++, &c));
	if (parser_logical_pathname_wordaster_p(c))
		goto next1;
	if (c == ';') {
		relative = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_RELATIVE);
		goto discard;
	}
	if (c == '.')
		goto dot_type2;
	goto error;

host:
	Return(parser_logical_host_(pa, charqueue));
	clear_charqueue(charqueue);
	if (size <= i)
		goto finish;
	Return(string_getc_(thing, i++, &c));
	if (parser_logical_pathname_wordaster_p(c))
		goto next2;
	if (c == ';') {
		relative = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_RELATIVE);
		goto discard;
	}
	if (c == '.')
		goto dot_type2;
	goto error;

next1:
	Return(push_charqueue_local_(local->local, charqueue, c));
	if (size <= i)
		goto finish_name;
	Return(string_getc_(thing, i++, &c));
	if (parser_logical_pathname_wordaster_p(c))
		goto next1;
	if (c == ';')
		goto path;
	if (c == ':')
		goto host;
	if (c == '.')
		goto dot_type1;
	goto error;

path:
	if (relative == 0 && absolute == 0) {
		absolute = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_ABSOLUTE);
	}
	make_charqueue_fileparse(pa, charqueue, &temp);
	clear_charqueue(charqueue);
	Return(pushdirectory_fileparse_(pa, &queue, temp));

discard:
	if (size <= i)
		goto finish;
	Return(string_getc_(thing, i++, &c));
	if (parser_logical_pathname_wordaster_p(c))
		goto next2;
	if (c == ';')
		goto discard;
	if (c == '.')
		goto dot_type2;
	goto error;

next2:
	Return(push_charqueue_local_(local->local, charqueue, c));
	if (size <= i)
		goto finish_name;
	Return(string_getc_(thing, i++, &c));
	if (parser_logical_pathname_wordaster_p(c))
		goto next2;
	if (c == ';')
		goto path;
	if (c == '.')
		goto dot_type1;
	goto error;

dot_type1:
	make_charqueue_fileparse(pa, charqueue, &pa->name);
	clear_charqueue(charqueue);

dot_type2:
	if (size <= i)
		goto finish_type;
	Return(string_getc_(thing, i++, &c));
	if (parser_logical_pathname_wordaster_p(c)) {
		Return(push_charqueue_local_(local->local, charqueue, c));
		goto dot_type2;
	}
	if (c == '.')
		goto dot_version1;
	goto error;

dot_version1:
	make_charqueue_fileparse(pa, charqueue, &pa->type);
	clear_charqueue(charqueue);

dot_version2:
	if (size <= i)
		goto finish_version;
	Return(string_getc_(thing, i++, &c));
	if (parser_logical_pathname_wordaster_p(c)) {
		Return(push_charqueue_local_(local->local, charqueue, c));
		goto dot_version2;
	}
	goto error;

finish_name:
	make_charqueue_fileparse(pa, charqueue, &pa->name);
	goto finish;

finish_type:
	make_charqueue_fileparse(pa, charqueue, &pa->type);
	goto finish;

finish_version:
	make_charqueue_fileparse(pa, charqueue, &pa->version);
	goto finish;

finish:
	nreverse(&pa->directory, queue);
	pa->endpos = i;
	return make_parse_logical_pathname_(pa);

error:
	if (pa->junk) {
		pa->result = Nil;
		pa->endpos = i? i - 1: 0;
		return 0;
	}

	return call_parse_error_(NULL);
}


/************************************************************
 *  pathname_object.c
 ************************************************************/

/*
 *  access
 */
void getarray_pathname(addr pos, enum PATHNAME_INDEX index, addr *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetArrayPathname_Low(pos, index, ret);
}

void setarray_pathname(addr pos, enum PATHNAME_INDEX index, addr value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetArrayPathname_Low(pos, index, value);
}

int reflogical_pathname(addr pos)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	return RefLogicalPathname_Low(pos);
}

void getlogical_pathname(addr pos, int *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetLogicalPathname_Low(pos, ret);
}

void setlogical_pathname(addr pos, int value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetLogicalPathname_Low(pos, value);
}

void gethost_pathname(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetHostPathname_Low(pos, ret);
}

void sethost_pathname(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetHostPathname_Low(pos, value);
}

void getdevice_pathname(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetDevicePathname_Low(pos, ret);
}

void setdevice_pathname(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDevicePathname_Low(pos, value);
}

void getdirectory_pathname(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetDirectoryPathname_Low(pos, ret);
}

void setdirectory_pathname(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDirectoryPathname_Low(pos, value);
}

void getname_pathname(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetNamePathname_Low(pos, ret);
}

void setname_pathname(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetNamePathname_Low(pos, value);
}

void gettype_pathname(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetTypePathname_Low(pos, ret);
}

void settype_pathname(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetTypePathname_Low(pos, value);
}

void getversion_pathname(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetVersionPathname_Low(pos, ret);
}

void setversion_pathname(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetVersionPathname_Low(pos, value);
}


/*
 *  pathname object
 */
static void directory_pathname_alloc(LocalRoot local, addr pos, addr *ret)
{
	addr absolute, wild, wild_inferiors;

	/* "string" -> (:absolute "string") */
	if (stringp(pos)) {
		GetConst(KEYWORD_ABSOLUTE, &absolute);
		list_alloc(local, ret, absolute, pos, NULL);
		return;
	}

	/* :wild -> (:absolute :wild-inferiors) */
	GetConst(KEYWORD_WILD, &wild);
	if (pos == wild) {
		GetConst(KEYWORD_ABSOLUTE, &absolute);
		GetConst(KEYWORD_WILD_INFERIORS, &wild_inferiors);
		list_alloc(local, ret, absolute, wild_inferiors, NULL);
		return;
	}

	/* :wild-inferiors -> (:absolute :wild-inferiors) */
	GetConst(KEYWORD_WILD_INFERIORS, &wild_inferiors);
	if (pos == wild_inferiors) {
		GetConst(KEYWORD_ABSOLUTE, &absolute);
		list_alloc(local, ret, absolute, wild_inferiors, NULL);
		return;
	}

	/* others */
	*ret = pos;
}

void make_pathname_alloc(LocalRoot local, addr *ret, int logical)
{
	alloc_array2(local, ret, LISPTYPE_PATHNAME, PATHNAME_INDEX_SIZE);
	SetLogicalPathname(*ret, logical);
}

static void setpathname_alloc(LocalRoot local, addr pos, addr host, addr device,
		addr directory, addr name, addr type, addr version)
{
	/* directory */
	directory_pathname_alloc(local, directory, &directory);

	/* set value */
	SetHostPathname(pos, host);
	SetDevicePathname(pos, device);
	SetDirectoryPathname(pos, directory);
	SetNamePathname(pos, name);
	SetTypePathname(pos, type);
	SetVersionPathname(pos, version);
}

void pathname_alloc(LocalRoot local, addr *ret,
		addr host, addr device, addr directory, addr name, addr type)
{
	addr version;

	make_pathname_alloc(local, ret, 0);
	GetConst(KEYWORD_UNSPECIFIC, &version);
	setpathname_alloc(local, *ret, host, device, directory, name, type, version);
}
void pathname_local(LocalRoot local, addr *ret,
		addr host, addr device, addr directory, addr name, addr type)
{
	Check(local == NULL, "local error");
	pathname_alloc(local, ret, host, device, directory, name, type);
}
void pathname_heap(addr *ret,
		addr host, addr device, addr directory, addr name, addr type)
{
	pathname_alloc(NULL, ret, host, device, directory, name, type);
}

void logical_pathname_alloc(LocalRoot local, addr *ret, addr host,
		addr directory, addr name, addr type, addr version)
{
	addr device;

	GetConst(KEYWORD_UNSPECIFIC, &device);
	make_pathname_alloc(local, ret, 1);
	setpathname_alloc(local, *ret, host, device, directory, name, type, version);
}
void logical_pathname_local(LocalRoot local, addr *ret, addr host,
		addr directory, addr name, addr type, addr version)
{
	Check(local == NULL, "local error");
	logical_pathname_alloc(local, ret, host, directory, name, type, version);
}
void logical_pathname_heap(addr *ret, addr host,
		addr directory, addr name, addr type, addr version)
{
	logical_pathname_alloc(NULL, ret, host, directory, name, type, version);
}

int pathnamep(addr pos)
{
	return GetType(pos) == LISPTYPE_PATHNAME;
}

int pathname_pathname_p(addr pos)
{
	return pathnamep(pos) && RefLogicalPathname(pos) == 0;
}

int pathname_logical_p(addr pos)
{
	return pathnamep(pos) && RefLogicalPathname(pos) != 0;
}

int pathname_file_p(addr pos)
{
	addr check;

	if (! pathnamep(pos))
		return 0;
	GetNamePathname(pos, &check);
	return check != Nil;
}

int pathname_directory_p(addr pos)
{
	addr name, type;

	if (! pathnamep(pos))
		return 0;
	GetNamePathname(pos, &name);
	GetTypePathname(pos, &type);

	return name == Nil && type == Nil;
}

void copylocal_pathname_array(LocalRoot local, addr a, int i, addr b)
{
	addr value;

	GetArrayPathname(a, (enum PATHNAME_INDEX)i, &value);
	copylocal_object(local, &value, value);
	SetArrayPathname(b, (enum PATHNAME_INDEX)i, value);
}

void copy_pathname_alloc(LocalRoot local, addr *ret, addr pos)
{
	int i;
	addr one;

	make_pathname_alloc(local, &one, RefLogicalPathname(pos));
	for (i = 0; i < PATHNAME_INDEX_SIZE; i++)
		copylocal_pathname_array(local, pos, i, one);
	*ret = one;
}

enum PathnameType {
	PathnameType_Unix,
	PathnameType_Windows,
	PathnameType_Logical,
	PathnameType_Error
};
static void make_pathname_environment(addr host, enum PathnameType *ret)
{
	addr check;

	/* unix */
	GetConst(SYSTEM_UNIX, &check);
	if (host == check) {
		*ret = PathnameType_Unix;
		return;
	}

	/* windows */
	GetConst(SYSTEM_WINDOWS, &check);
	if (host == check) {
		*ret = PathnameType_Windows;
		return;
	}

	/* logical */
	if (stringp(host)) {
		*ret = PathnameType_Logical;
		return;
	}

	/* error */
	*ret = PathnameType_Error;
}

int pathname_ignore_case_p(addr pos)
{
	enum PathnameType type;

	Check(! pathnamep(pos), "type left error");
	GetHostPathname(pos, &pos);
	make_pathname_environment(pos, &type);
	switch (type) {
		case PathnameType_Windows:
		case PathnameType_Logical:
			return 1;

		default:
			return 0;
	}
}

lisp_equal_calltype pathname_equal_function(addr pos)
{
	Check(! pathnamep(pos), "type left error");
	return pathname_ignore_case_p(pos)?
		equalp_function_:
		equal_function_;
}

static int make_pathname_environment_(addr host, enum PathnameType *ret)
{
	make_pathname_environment(host, ret);
	if (*ret == PathnameType_Error)
		return fmte_("Invalid host value ~S.", host, NULL);
	return 0;
}

int pathname_equal_(addr a, addr b, int *ret)
{
	int check;
	addr x, y;
	int (*equal)(addr, addr, int *);

	Check(! pathnamep(a), "type left error");
	Check(! pathnamep(b), "type right error");

	if (RefLogicalPathname(a) != RefLogicalPathname(b))
		return Result(ret, 0);

	/* host */
	GetHostPathname(a, &x);
	GetHostPathname(b, &y);
	Return(equalp_function_(x, y, &check));
	if (! check)
		return Result(ret, 0);
	equal = pathname_equal_function(a);

	/* device */
	GetDevicePathname(a, &x);
	GetDevicePathname(b, &y);
	Return((*equal)(x, y, &check));
	if (! check)
		return Result(ret, 0);

	/* directory */
	GetDirectoryPathname(a, &x);
	GetDirectoryPathname(b, &y);
	Return((*equal)(x, y, &check));
	if (! check)
		return Result(ret, 0);

	/* name */
	GetNamePathname(a, &x);
	GetNamePathname(b, &y);
	Return((*equal)(x, y, &check));
	if (! check)
		return Result(ret, 0);

	/* type */
	GetTypePathname(a, &x);
	GetTypePathname(b, &y);
	Return((*equal)(x, y, &check));
	if (! check)
		return Result(ret, 0);

	/* version */
	pathname_version(a, &x);
	pathname_version(b, &y);
	if (! eql_function(x, y))
		return Result(ret, 0);

	return Result(ret, 1);
}


/* make-pathname */
static int make_pathname_directory_keyword_p_(addr pos, int *ret)
{
	if (! stringp(pos))
		return Result(ret, 0);

	Return(string_equal_char_(pos, "*", ret));
	if (*ret)
		return 0;

	Return(string_equal_char_(pos, "**", ret));
	if (*ret)
		return 0;

	return string_equal_char_(pos, "..", ret);
}

static int make_pathname_directory_(addr *ret, addr list)
{
	int keywordp, check;
	addr root, pos, absolute, relative, wild, wildi, up;

	GetConst(KEYWORD_ABSOLUTE, &absolute);
	GetConst(KEYWORD_RELATIVE, &relative);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wildi);

	/* value */
	if (list == Nil || list == wild || list == wildi || stringp(list))
		return Result(ret, list);

	/* cons */
	if (! consp(list)) {
		*ret = Nil;
		return fmte_(":directory ~S must be a list or string type.", list, NULL);
	}
	GetConst(KEYWORD_UP, &up);
	Return_getcons(list, &pos, &root);
	if (pos != absolute && pos != relative) {
		*ret = Nil;
		return fmte_("The first argument of :directory ~S must be ~S or ~S.",
				pos, absolute, relative, NULL);
	}
	for (keywordp = 1; root != Nil; ) {
		Return_getcons(root, &pos, &root);
		Return(make_pathname_directory_keyword_p_(pos, &check));
		if (check)
			keywordp = 0;
		if (! stringp(pos) && pos != wild && pos != wildi && pos != up) {
			*ret = Nil;
			return fmte_("Invalid :directory argument ~S.", pos, NULL);
		}
	}
	if (keywordp)
		return Result(ret, list);

	/* rebuild */
	GetCons(list, &pos, &list);
	conscar_heap(&root, pos);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		/* "*" */
		Return(stringp_equal_char_(pos, "*", &check));
		if (check) {
			cons_heap(&root, wild, root);
			continue;
		}
		/* "**" */
		Return(stringp_equal_char_(pos, "**", &check));
		if (check) {
			cons_heap(&root, wildi, root);
			continue;
		}
		/* ".." */
		Return(stringp_equal_char_(pos, "..", &check));
		if (check) {
			cons_heap(&root, up, root);
			continue;
		}
		/* else */
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);

	return 0;
}

static int make_pathname_string_(addr *ret)
{
	int check;
	addr pos;

	pos = *ret;
	if (! stringp(pos))
		return Result(ret, pos);

	Return(string_upper_p_(pos, &check));
	if (check)
		return string_lower_heap_(pos, ret);

	Return(string_lower_p_(pos, &check));
	if (check)
		return string_upper_heap_(pos, ret);

	return  Result(ret, pos);
}

static int make_pathname_check_(addr pos, int *ret)
{
	int check;

	if (! stringp(pos))
		return Result(ret, 0);
	Return(string_upper_p_(pos, &check));
	if (check)
		return Result(ret, 1);

	return string_lower_p_(pos, ret);
}

static int make_pathname_list_p_(addr list, int *ret)
{
	int check;
	addr pos;

	/* string */
	if (stringp(list))
		return make_pathname_check_(list, ret);

	/* others */
	while (consp_getcons(list, &pos, &list)) {
		Return(make_pathname_check_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int make_pathname_list_(addr *ret, addr list)
{
	int check;
	addr root, pos;

	/* string */
	if (stringp(list)) {
		Return(make_pathname_string_(&list));
		return Result(ret, list);
	}

	/* atom */
	if (! consp(list))
		return Result(ret, list);

	/* cons */
	for (root = Nil; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(make_pathname_check_(pos, &check));
		if (check) {
			Return(make_pathname_string_(&pos));
		}
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);

	return 0;
}

static int make_pathname_case_(addr *directory, addr *name, addr *type)
{
	int check;

	/* directory */
	Return(make_pathname_list_p_(*directory, &check));
	if (check) {
		Return(make_pathname_list_(directory, *directory));
	}
	/* name, type */
	Return(make_pathname_string_(name));
	Return(make_pathname_string_(type));

	return 0;
}

static int pathname_type_lower_p(enum PathnameType ptype, int localp)
{
	return localp == 0 && ptype == PathnameType_Unix;
}

static int pathname_host_lower_p(addr host, int localp)
{
	enum PathnameType ptype;

	if (localp)
		return 0;
	Return(make_pathname_environment_(host, &ptype));
	return ptype == PathnameType_Unix;
}

static int make_pathname_logical_(addr *ret,
		addr host, addr device, addr directory,
		addr name, addr type, addr version)
{
	int check;
	addr unspec;

	/* host */
	Return(string_upper_p_(host, &check));
	if (! check) {
		Return(string_upper_heap_(host, &host));
	}

	/* defaults */
	GetConst(KEYWORD_UNSPECIFIC, &unspec);
	if (version == unspec)
		version = Nil;

	/* make */
	logical_pathname_heap(ret, host, directory, name, type, version);
	return 0;
}

int make_pathname_heap_(addr *ret,
		addr host, addr device, addr directory,
		addr name, addr type, addr version, int localp)
{
	enum PathnameType ptype;

	/* format */
	Return(make_pathname_directory_(&directory, directory));

	/* case */
	Return(make_pathname_environment_(host, &ptype));
	if (pathname_type_lower_p(ptype, localp)) {
		Return(make_pathname_case_(&directory, &name, &type));
	}

	/* pathname */
	if (ptype == PathnameType_Logical) {
		Return(make_pathname_logical_(ret,
					host, device, directory, name, type, version));
	}
	else {
		pathname_heap(ret, host, device, directory, name, type);
	}

	return 0;
}


/*
 *  pathname accessor
 */
int pathname_host_(addr pos, addr *ret, int ignore_localp)
{
	GetHostPathname(pos, &pos);
	return Result(ret, pos);
}

int pathname_device_(addr pos, addr *ret, int ignore_localp)
{
	GetDevicePathname(pos, &pos);
	return Result(ret, pos);
}

int pathname_directory_(addr pos, addr *ret, int localp)
{
	int check;
	addr host;

	GetHostPathname(pos, &host);
	GetDirectoryPathname(pos, &pos);
	if (pathname_host_lower_p(host, localp)) {
		Return(make_pathname_list_p_(pos, &check));
		if (check) {
			Return(make_pathname_list_(&pos, pos));
		}
	}

	return Result(ret, pos);
}

int pathname_name_(addr pos, addr *ret, int localp)
{
	addr host;

	GetHostPathname(pos, &host);
	GetNamePathname(pos, &pos);
	if (pathname_host_lower_p(host, localp)) {
		Return(make_pathname_string_(&pos));
	}

	return Result(ret, pos);
}

int pathname_type_(addr pos, addr *ret, int localp)
{
	addr host;

	GetHostPathname(pos, &host);
	GetTypePathname(pos, &pos);
	if (pathname_host_lower_p(host, localp)) {
		Return(make_pathname_string_(&pos));
	}

	return Result(ret, pos);
}

void pathname_version(addr pos, addr *ret)
{
	addr version;

	GetVersionPathname(pos, &version);
	if (RefLogicalPathname(pos) != 0 && version == Nil) {
		GetConst(KEYWORD_NEWEST, ret);
	}
	else {
		*ret = version;
	}
}


/************************************************************
 *  pathname_table.c
 ************************************************************/

void init_fileparse(struct fileparse *pa, Execute ptr, int localp)
{
	clearpoint(pa);
	pa->thing = pa->path = pa->queue = pa->result =
		pa->host = pa->device = pa->directory =
		pa->name = pa->type = pa->version = Nil;
	pa->ptr = ptr;
	pa->local_buffer.local = ptr->local;
	pa->local_buffer.localp = localp? 1: 0;
	pa->local = &pa->local_buffer;
}

void pathname_fileparse_alloc(struct fileparse *pa, int logical)
{
	LocalRoot local;

	local = localp_alloc(pa->local);
	if (logical) {
		logical_pathname_alloc(local, &pa->result,
				pa->host, pa->directory, pa->name, pa->type, pa->version);
	}
	else {
		pathname_alloc(local, &pa->result,
				pa->host, pa->device, pa->directory, pa->name, pa->type);
	}
}

int wild_value_pathname_(addr input, addr *ret)
{
	int check;

	Return(stringp_equal_char_(input, "*", &check));
	if (check) {
		GetConst(KEYWORD_WILD, ret);
		return 0;
	}

	return Result(ret, input);
}

static int wild_newest_value_pathname_(addr input, addr *ret)
{
	int check;

	Return(stringp_equal_char_(input, "*", &check));
	if (check) {
		GetConst(KEYWORD_WILD, ret);
		return 0;
	}

	Return(stringp_equalp_char_(input, "NEWEST", &check));
	if (check) {
		GetConst(KEYWORD_NEWEST, ret);
		return 0;
	}

	return Result(ret, input);
}

static int check_asterisk_logical_pathname_(addr pos)
{
	unicode a, b;
	size_t size, i;

	if (! stringp(pos))
		return 0;
	string_length(pos, &size);
	a = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &b));
		if (a == '*' && b == '*')
			return fmte_("Invalid wildcard string ~S.", pos, NULL);
	}

	return 0;
}

static int check_version_logical_pathname_(addr pos)
{
	unicode c;
	size_t size, i;

	if (! stringp(pos))
		return 0;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (! isDigitCase(c))
			return fmte_(":VERSION ~S must be a positive integer.", pos, NULL);
	}

	return 0;
}

static int check_parse_logical_pathname_host_(addr host, int errorp)
{
	addr check;

	if (! errorp)
		return 0;
	Return(gethost_logical_pathname_(host, &check));
	if (check == Nil) {
		GetTypeTable(&check, String);
		return call_type_error_va_(NULL,
				host, check, "There is no logical hostname ~S.", host, NULL);
	}

	return 0;
}

static int check_parse_logical_pathname_(struct fileparse *pa)
{
	int check;
	addr list, pos;
	size_t size;

	/* host */
	string_length(pa->host, &size);
	if (size == 0)
		return fmte_("Invalid host name ~S.", pa->host, NULL);
	Return(check_parse_logical_pathname_host_(pa->host, pa->errorp));

	/* directory */
	for (list = pa->directory; list != Nil; ) {
		GetCons(list, &pos, &list);
		Return(check_asterisk_logical_pathname_(pos));
	}
	/* name */
	Return(check_asterisk_logical_pathname_(pa->name));
	/* type */
	Return(check_asterisk_logical_pathname_(pa->type));
	/* version */
	pos = pa->version;
	Return(check_version_logical_pathname_(pos));
	if (stringp(pos)) {
		if (read_from_string_(pa->ptr, &check, &pos, pos))
			return fmte_("Cannot read ~S object.", pa->version, NULL);
		if (check)
			return fmte_("Cannot read ~S object.", pa->version, NULL);
		if (! integerp(pos))
			return fmte_("Invalid version object ~S.", pos, NULL);
		pa->version = pos;
	}

	return 0;
}

int make_parse_logical_pathname_(struct fileparse *pa)
{
	GetConst(KEYWORD_UNSPECIFIC, &pa->device);
	Return(wild_value_pathname_(pa->name, &pa->name));
	Return(wild_value_pathname_(pa->type, &pa->type));
	Return(wild_newest_value_pathname_(pa->version, &pa->version));
	if (pa->host == Nil)
		return fmte_("No logical-pathname host.", NULL);
	Return(check_parse_logical_pathname_(pa));
	pathname_fileparse_alloc(pa, 1);

	return 0;
}

int pushrange_pathname_(LocalpRoot local,
		addr queue, addr thing, size_t n1, size_t n2)
{
	unicode c;

	for (; n1 < n2; n1++) {
		Return(string_getc_(thing, n1, &c));
		Return(push_charqueue_local_(local->local, queue, c));
	}

	return 0;
}

void make_charqueue_fileparse(struct fileparse *pa, addr queue, addr *ret)
{
	make_charqueue_alloc(localp_alloc(pa->local), queue, ret);
}

int nametype_pathname_(struct fileparse *pa, size_t index)
{
	/*
	 *  index -> 1..size-1
	 *    "a.bc" -> ("a" "bc")
	 *    "abc." -> ("abc" "")   not nil
	 */
	addr pos, queue;
	size_t size;
	LocalpRoot local;

	local = pa->local;
	queue = pa->queue;
	pos = pa->name;
	string_length(pos, &size);
	Check(size <= index, "length error");

	/* name */
	clear_charqueue(queue);
	Return(pushrange_pathname_(local, queue, pos, 0, index));
	make_charqueue_fileparse(pa, queue, &pa->name);
	/* type */
	clear_charqueue(queue);
	Return(pushrange_pathname_(local, queue, pos, index + 1, size));
	make_charqueue_fileparse(pa, queue, &pa->type);

	return 0;
}

int pushdirectory_fileparse_(struct fileparse *pa, addr *list, addr name)
{
	/*
	 *  ".." -> :UP
	 *  "*"  -> :WILD
	 *  "**" -> :WILD-INFERIORS
	 */
	int check;

	Return(stringp_equal_char_(name, "..", &check));
	if (check) {
		GetConst(KEYWORD_UP, &name);
		goto next;
	}

	Return(stringp_equal_char_(name, "*", &check));
	if (check) {
		GetConst(KEYWORD_WILD, &name);
		goto next;
	}

	Return(stringp_equal_char_(name, "**", &check));
	if (check) {
		GetConst(KEYWORD_WILD_INFERIORS, &name);
		goto next;
	}

next:
	cons_alloc(localp_alloc(pa->local), list, name, *list);
	return 0;
}

void pushconstant_fileparse(struct fileparse *pa, addr *list, constindex index)
{
	addr value;
	GetConstant(index, &value);
	cons_alloc(localp_alloc(pa->local), list, value, *list);
}

int check_host_logical_pathname_(LocalpRoot local, addr queue, int *ret)
{
	addr key;
	make_charqueue_local(local->local, queue, &key);
	Return(gethost_logical_pathname_(key, &key));
	return Result(ret, key != Nil);
}

int check_drive_logical_pathname_(LocalpRoot local, int drive, int *ret)
{
	addr key;
	strvect_local(local->local, &key, 1);
	Return(strvect_setc_(key, 0, drive));
	Return(gethost_logical_pathname_(key, &key));
	return Result(ret, key != Nil);
}


/************************************************************
 *  pathname_translate.c
 ************************************************************/

/*
 *  logical-pathname table
 */
void table_logical_pathname(addr *ret)
{
	addr symbol;
	GetConst(SYSTEM_LOGICAL_PATHNAME, &symbol);
	GetValueSymbol(symbol, ret);
}

int gethost_logical_pathname_(addr key, addr *ret)
{
	addr list;
	table_logical_pathname(&list);
	return findnil_hashtable_(list, key, ret);
}

int sethost_logical_pathname_(addr key, addr value)
{
	addr list;

	table_logical_pathname(&list);
	Return(intern_hashheap_(list, key, &list));
	SetCdr(list, value);

	return 0;
}


/*
 *  translate-pathname
 */
/* struct */
struct wildcard_position {
	struct wildcard_position *next;
	size_t a, b;
};

struct translate_struct {
	unsigned ignore_case_p : 1;
	struct wildcard_position *root, *tail;
	addr pos, var, from, to, ret;
	size_t s1, n1, s2, n2;
};

typedef struct translate_struct *TranslateInfo;

static struct wildcard_position *make_wildcard_position(LocalpRoot local,
		size_t a, size_t b)
{
	struct wildcard_position *ptr;

	ptr = (struct wildcard_position *)lowlevel_local(local->local,
			sizeoft(struct wildcard_position));
	ptr->next = NULL;
	ptr->a = a;
	ptr->b = b;

	return ptr;
}


/*
 *  name, type
 */
static int translate_name_match_(LocalpRoot local, TranslateInfo ptr, int *ret);

#define TranslateCompareCharacter(p, x, y) \
	((p)->ignore_case_p? \
	 (toUpperUnicode(x) == toUpperUnicode(y)): \
	 ((x) == (y)))

static void translate_name_match_push(LocalpRoot local,
		struct translate_struct *ptr, size_t a, size_t b)
{
	struct wildcard_position *str;

	str = make_wildcard_position(local, a, b);
	if (ptr->root == NULL) {
		ptr->root = ptr->tail = str;
	}
	else {
		str->next = ptr->tail;
		ptr->tail = str;
	}
}

static int translate_name_match_diff_(TranslateInfo ptr, int *ret)
{
	unicode c;
	addr p2;
	size_t i, n2, s2;

	p2 = ptr->from;
	n2 = ptr->n2;
	s2 = ptr->s2;
	for (i = n2; i < s2; i++) {
		Return(string_getc_(p2, n2, &c));
		if (c != '*')
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int translate_name_match1_(LocalpRoot local, TranslateInfo ptr, int *ret)
{
	int check;
	struct translate_struct str;
	size_t n1;

	n1 = ptr->n1;
	str = *ptr;
	str.n1++;
	str.n2++;
	Return(translate_name_match_(local, &str, &check));
	ptr->root = str.root;
	ptr->tail = str.tail;
	if (check)
		translate_name_match_push(local, ptr, n1, n1+1UL);

	return Result(ret, check);
}

static int translate_name_match2_(LocalpRoot local,
		TranslateInfo ptr, unicode x, int *ret)
{
	unicode y;
	struct translate_struct str;

	Return(string_getc_(ptr->var, ptr->n1, &y));
	if (! TranslateCompareCharacter(ptr, x, y))
		return Result(ret, 0);

	str = *ptr;
	str.n1++;
	str.n2++;
	Return(translate_name_match_(local, &str, ret));
	ptr->root = str.root;
	ptr->tail = str.tail;

	return 0;
}

static int translate_name_match3_(LocalpRoot local, TranslateInfo ptr, int *ret)
{
	int check;
	size_t n1, s1, i;
	struct translate_struct str;

	n1 = ptr->n1;
	s1 = ptr->s1;
	str = *ptr;
	str.n2++;
	for (i = n1; i <= s1; i++) {
		str.n1 = i;
		Return(translate_name_match_(local, &str, &check));
		ptr->root = str.root;
		ptr->tail = str.tail;
		if (check) {
			translate_name_match_push(local, ptr, n1, i);
			return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int translate_name_match_(LocalpRoot local, TranslateInfo ptr, int *ret)
{
	unicode x;

	/* match */
	if (ptr->n1 == ptr->s1 && ptr->n2 == ptr->s2)
		return Result(ret, 1);

	/* unmatch */
	if (ptr->n2 == ptr->s2)
		return Result(ret, 0);

	/* check */
	if (ptr->n1 == ptr->s1)
		return translate_name_match_diff_(ptr, ret);

	/* (a ?) -> next */
	Return(string_getc_(ptr->from, ptr->n2, &x));
	if (x == '?')
		return translate_name_match1_(local, ptr, ret);

	/* (a a) -> next, (a b) -> false */
	if (x != '*')
		return translate_name_match2_(local, ptr, x, ret);

	/* (a *) */
	return translate_name_match3_(local, ptr, ret);
}

static int translate_name_push_wild_(LocalpRoot local,
		addr queue, addr var, struct wildcard_position *ptr)
{
	unicode c;
	size_t i;

	for (i = ptr->a; i < ptr->b; i++) {
		Return(string_getc_(var, i, &c));
		Return(push_charqueue_local_(local->local, queue, c));
	}

	return 0;
}

static int translate_name_wild_(LocalpRoot local, TranslateInfo ptr)
{
	unicode c;
	struct wildcard_position *str;
	addr var, to, queue;
	size_t size, i;

	var = ptr->var;
	to = ptr->to;
	charqueue_local(local->local, &queue, 0);
	string_length(to, &size);
	str = ptr->tail;
	for (i = 0; i < size; i++) {
		Return(string_getc_(to, i, &c));
		if (c == '*' || c == '?') {
			if (str) {
				Return(translate_name_push_wild_(local, queue, var, str));
				str = str->next;
			}
		}
		else {
			Return(push_charqueue_local_(local->local, queue, c));
		}
	}

	/* position check */
	if (str) {
		clear_charqueue(queue);
		Return(translate_name_push_wild_(local, queue, var, str));
		make_charqueue_alloc(localp_alloc(local), queue, &var);
		return fmte_("Cannot extract ~S pattern.", var, NULL);
	}

	/* result */
	make_charqueue_alloc(localp_alloc(local), queue, &ptr->ret);

	return 0;
}

static void translate_name_type(LocalRoot local, addr *var, addr *from, addr *to)
{
	addr wild;

	GetConst(KEYWORD_WILD, &wild);
	/* var */
	if (*var == Nil) {
		strvect_local(local, var, 0);
	}
	/* from */
	if (*from == Nil ||*from == wild) {
		strvect_char_local(local, from, "*");
	}
	/* to */
	if (*to == Nil || *to == wild) {
		strvect_char_local(local, to, "*");
	}
}

static void translate_name_struct(TranslateInfo ptr,
		addr pos, addr var, addr from, addr to)
{
	Check(! stringp(var), "type error");
	Check(! stringp(from), "type error");
	Check(! stringp(to), "type error");

	clearpoint(ptr);
	ptr->ignore_case_p = pathname_ignore_case_p(pos);
	ptr->pos = pos;
	ptr->var = var;
	ptr->from = from;
	ptr->to = to;
	string_length(var, &ptr->s1);
	string_length(from, &ptr->s2);
}

static int translate_name_(LocalpRoot local, addr *ret,
		addr pos, addr var, addr from, addr to)
{
	int check;
	LocalStack stack;
	struct translate_struct str;

	push_localp(local, &stack);
	translate_name_type(local->local, &var, &from, &to);
	translate_name_struct(&str, pos, var, from, to);

	/* wildcard */
	Return(translate_name_match_(local, &str, &check));
	if (! check)
		return fmte_("The string ~S doesn't match ~S.", var, from, NULL);

	/* replace */
	Return(translate_name_wild_(local, &str));

	/* result */
	*ret = str.ret;
	rollback_localp(local, stack);

	return 0;
}


/*
 *  directory
 */
static int translate_directory_list_(LocalpRoot local,
		addr *value, addr a, addr b,
		lisp_equal_calltype equal, int *ret)
{
	int check;
	addr a1, b1, var1, var2, wild, wilds;


	if (a == Nil && b == Nil)
		return Result(ret, 1);
	if (a == Nil || b == Nil)
		return Result(ret, 0);

	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wilds);
	Return_getcons(a, &var1, &a1);
	Return_getcons(b, &var2, &b1);
	/* ("str" *) -> next */
	if (var2 == wild) {
		Return(translate_directory_list_(local, value, a1, b1, equal, &check));
		if (check)
			list_local(local->local, value, *value, var2, var1, Nil, NULL);
		return Result(ret, check);
	}
	/* ("str" "s*r") -> next, ("str" "a*a") -> false */
	Return(wildcard_stringp_p_(var2, &check));
	if (check) {
		Return(wildcard_string_pathname_(var1, var2, equal, &check));
		if (! check)
			return Result(ret, 0);
		list_local(local->local, value, *value, var2, var1, Nil, NULL);
		return translate_directory_list_(local, value, a1, b1, equal, ret);
	}
	/* ("str" "str") -> next, ("str" "aaa") -> false */
	if (var2 != wilds) {
		Return(wildcard_eq_pathname_(var1, var2, equal, &check));
		if (! check)
			return Result(ret, 0);
		return translate_directory_list_(local, value, a1, b1, equal, ret);
	}
	/* ("str" **) */
	a1 = a;
	for (;;) {
		Return(translate_directory_list_(local, value, a, b1, equal, &check));
		if (check) {
			list_local(local->local, value, *value, var2, a1, a, NULL);
			return Result(ret, 1);
		}
		if (a == Nil)
			break;
		Return_getcdr(a, &a);
	}

	return Result(ret, 0);
}

static void translate_directory_wild_wild_(LocalpRoot local, addr *root, addr a, addr b)
{
	addr var;
	LocalRoot alloc;

	alloc = localp_alloc(local);
	while (a != b) {
		GetCons(a, &var, &a);
		cons_alloc(alloc, root, var, *root);
	}
}

static int translate_directory_wild_string_(LocalpRoot local,
		addr *root, addr pos, addr var, addr from)
{
	addr to;

	strvect_char_local(local->local, &to, "*");
	Return(translate_name_(local, &var, pos, var, from, to));
	cons_alloc(localp_alloc(local), root, var, *root);

	return 0;
}

static int translate_directory_wild_(LocalpRoot local, addr *root, addr *list, addr pos)
{
	int check;
	addr wild1, wild2, next, var, a, b;

	if (*list == Nil)
		return fmte_("Don't match wildcard FROM and TO.", NULL);
	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);
	List_bind(*list, &next, &var, &a, &b, NULL);
	if (var == wild1) {
		cons_alloc(localp_alloc(local), root, a, *root);
		goto final;
	}
	if (var == wild2) {
		translate_directory_wild_wild_(local, root, a, b);
		goto final;
	}
	Return(wildcard_stringp_p_(var, &check));
	if (check) {
		Return(translate_directory_wild_string_(local, root, pos, a, var));
		goto final;
	}
	cons_alloc(localp_alloc(local), root, var, *root);
	goto final;

final:
	return Result(list, next);
}

static int translate_directory_string_wild_(LocalpRoot local,
		addr *root, addr pos, addr var, addr to)
{
	addr from;

	strvect_char_local(local->local, &from, "*");
	Return(translate_name_(local, &var, pos, var, from, to));
	cons_alloc(localp_alloc(local), root, var, *root);

	return 0;
}

static int translate_directory_string_string_(LocalpRoot local,
		addr *root, addr pos, addr var, addr from, addr to)
{
	Return(translate_name_(local, &var, pos, var, from, to));
	cons_alloc(localp_alloc(local), root, var, *root);
	return 0;
}

static int translate_directory_string_(LocalpRoot local,
		addr *root, addr *list, addr pos, addr to)
{
	int check;
	addr wild1, wild2, next, var, a, b;

	if (*list == Nil)
		return fmte_("Don't match wildcard FROM and TO.", NULL);
	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);
	List_bind(*list, &next, &var, &a, &b, NULL);
	if (var == wild1 || var == wild2) {
		Return(translate_directory_string_wild_(local, root, pos, a, to));
		goto final;
	}
	Return(wildcard_stringp_p_(var, &check));
	if (check) {
		Return(translate_directory_string_string_(local, root, pos, a, var, to));
		goto final;
	}
	cons_alloc(localp_alloc(local), root, var, *root);
	goto final;

final:
	return Result(list, next);
}

static int translate_directory_replace_(LocalpRoot local,
		addr *root, addr *list, addr pos, addr to)
{
	int check;
	LocalRoot alloc;
	addr var, wild1, wild2;

	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);
	alloc = localp_alloc(local);
	while (to != Nil) {
		Return_getcons(to, &var, &to);
		if (var == wild1 || var == wild2) {
			Return(translate_directory_wild_(local, root, list, pos));
			continue;
		}
		Return(wildcard_stringp_p_(var, &check));
		if (check) {
			Return(translate_directory_string_(local, root, list, pos, var));
			continue;
		}
		cons_alloc(alloc, root, var, *root);
	}
	nreverse(root, *root);

	return 0;
}

static int translate_directory_(LocalpRoot local, addr *ret,
		addr pos, addr var, addr from, addr to,
		lisp_equal_calltype equal)
{
	int check;
	addr list;
	LocalStack stack;

	push_localp(local, &stack);
	list = Nil;
	Return(translate_directory_list_(local, &list, var, from, equal, &check));
	if (! check)
		return fmte_("Cannot translate ~S to ~S.", from, var, NULL);
	*ret = Nil;
	Return(translate_directory_replace_(local, ret, &list, pos, to));
	rollback_localp(local, stack);

	return 0;
}


/*
 *  version
 */
static int translate_version_(addr *ret, addr var, addr from, addr to)
{
	addr wild, unspec;

	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_UNSPECIFIC, &unspec);
	if (from == Nil)
		from = wild;
	if (to == Nil)
		to = wild;
	/* :unspecific */
	if (to == unspec)
		return Result(ret, unspec);

	/* value */
	if (from == wild) {
		if (to == wild)
			return Result(ret, var);
		return fmte_(":VERSION from-wildcard is *, but to-wildcard ~S "
				"is not *.", to, NULL);
	}
	if (! eql_function(var, to))
		return fmte_(":VERSION source ~S don't match to-wildcard ~S.", var, to, NULL);

	/* :unspecific */
	return Result(ret, unspec);
}


/*
 *  translate pathname
 */
static void translate_getdirectory(addr pos, addr *ret)
{
	addr list;

	GetDirectoryPathname(pos, &list);
	if (list != Nil) {
		*ret = list;
		return;
	}

	/* (:relative) */
	GetConst(KEYWORD_RELATIVE, &list);
	conscar_heap(ret, list);
}

static int translate_setdirectory_p(addr list)
{
	addr x, y;

	/* (:relative) */
	if (! consp_getcons(list, &x, &list))
		return 0;
	GetConst(KEYWORD_RELATIVE, &y);
	return (x == y) && (list == Nil);
}

static void translate_setdirectory(LocalRoot local, addr pos, addr value)
{
	if (translate_setdirectory_p(value)) {
		SetDirectoryPathname(pos, Nil);
	}
	else {
		copylocal_object(local, &value, value);
		SetDirectoryPathname(pos, value);
	}
}

static int translate_pathname_localp_(Execute ptr, LocalpRoot local,
		addr *ret, addr pos, addr from, addr to)
{
	int localp;
	LocalRoot alloc;
	addr one, a, b, c;
	lisp_equal_calltype equal;

	alloc = localp_alloc(local);
	localp = local->localp;
	/* argument */
	Return(pathname_designator_alloc_(ptr, pos, &pos, localp));
	Return(pathname_designator_alloc_(ptr, from, &from, localp));
	Return(pathname_designator_alloc_(ptr, to, &to, localp));
	/* one */
	make_pathname_alloc(alloc, &one, RefLogicalPathname(to));
	/* host */
	equal = pathname_equal_function(pos);
	copylocal_pathname_array(alloc, to, PATHNAME_INDEX_HOST, one);
	/* device */
	copylocal_pathname_array(alloc, to, PATHNAME_INDEX_DEVICE, one);
	/* directory */
	translate_getdirectory(pos, &a);
	translate_getdirectory(from, &b);
	translate_getdirectory(to, &c);
	Return(translate_directory_(local, &a, pos, a, b, c, equal));
	translate_setdirectory(alloc, one, a);
	/* name */
	GetNamePathname(pos, &a);
	GetNamePathname(from, &b);
	GetNamePathname(to, &c);
	Return(translate_name_(local, &a, pos, a, b, c));
	copylocal_object(alloc, &a, a);
	SetNamePathname(one, a);
	/* type */
	GetTypePathname(pos, &a);
	GetTypePathname(from, &b);
	GetTypePathname(to, &c);
	Return(translate_name_(local, &a, pos, a, b, c));
	copylocal_object(alloc, &a, a);
	SetTypePathname(one, a);
	/* version */
	GetVersionPathname(pos, &a);
	GetVersionPathname(from, &b);
	GetVersionPathname(to, &c);
	Return(translate_version_(&a, a, b, c));
	copylocal_object(alloc, &a, a);
	SetVersionPathname(one, a);
	/* result */
	return Result(ret, one);
}

int translate_pathname_alloc_(Execute ptr,
		addr *ret, addr pos, addr from, addr to, int localp)
{
	struct localp_struct buffer;

	buffer.localp = localp;
	buffer.local = ptr->local;
	return translate_pathname_localp_(ptr, &buffer, ret, pos, from, to);
}

int translate_pathname_heap_(Execute ptr, addr *ret, addr pos, addr from, addr to)
{
	return translate_pathname_alloc_(ptr, ret, pos, from, to, 0);
}


/*
 *  build
 */
void build_pathname_translate(void)
{
	addr value, symbol;

	/* logical-pathname */
	hashtable_heap(&value);
	settest_hashtable(value, HASHTABLE_TEST_EQUALP);
	GetConst(SYSTEM_LOGICAL_PATHNAME, &symbol);
	SetValueSymbol(symbol, value);

	/* universal time */
	value = intsizeh((70ULL*365 + 17ULL) * 24 * 60 * 60);
	GetConst(SYSTEM_TIME1970, &symbol);
	SetValueSymbol(symbol, value);
}


/************************************************************
 *  pathname_unix.c
 ************************************************************/

/*
 *  home directory
 */
static int parser_home_directory_p_pathname_(struct fileparse *pa, int *ret)
{
	addr list, x, check;
	unicode c;
	size_t size;

	/* (:relative "~" ...) */
	/* (:relative "~user" ...) */
	list = pa->directory;
	if (! consp_getcons(list, &x, &list))
		return Result(ret, 0);
	GetConst(KEYWORD_RELATIVE, &check);
	if (x != check)
		return Result(ret, 0);
	if (! consp_getcons(list, &x, &list))
		return Result(ret, 0);
	if (! stringp(x))
		return Result(ret, 0);
	string_length(x, &size);
	if (size == 0)
		return Result(ret, 0);
	Return(string_getc_(x, 0, &c));

	return Result(ret, c == '~');
}

#ifdef LISP_UNIX
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>

#ifdef LISP_DEBUG
#define PATHNAME_GETPWNAM_SIZE	(1UL << 1UL)
#else
#define PATHNAME_GETPWNAM_SIZE	(1UL << 16UL)
#endif

static int env_home_pathname_(struct fileparse *pa, addr x, addr *ret)
{
	addr pos;

	GetConst(SYSTEM_SPECIAL_ENVIRONMENT, &pos);
	getspecial_local(pa->ptr, pos, &pos);
	if (pos == Unbound)
		return Result(ret, Unbound);
	if (! hashtablep(pos))
		return Result(ret, Unbound);

	return find_char_hashtable_(pos, "HOME", ret);
}

static int strvect_home_pathname_(LocalRoot local, const byte *pw_dir, addr *ret)
{
	addr pos;
	size_t size;
	unicode *body;

	if (UTF8_null_strlen(pw_dir, &size))
		return Result(ret, Unbound); /* encode error */
	strvect_local(local, &pos, size);
	GetStringUnicode(pos, &body);
	if (UTF8_null_makeunicode(body, pw_dir))
		return Result(ret, Unbound); /* encode error */

	return Result(ret, pos);
}

static int passwd_home_pathname_(
		const char *name, struct fileparse *pa, addr x, addr *ret)
{
	int check;
	char *data;
	struct passwd pwd, *result;
	size_t size;
	LocalRoot local;
	LocalStack stack;

	local = pa->ptr->local;
	size = PATHNAME_GETPWNAM_SIZE;
	for (;;) {
		push_local(local, &stack);
		data = (char *)lowlevel_local(local, size);
		if (name)
			check = getpwnam_r(name, &pwd, data, size, &result);
		else
			check = getpwuid_r(getuid(), &pwd, data, size, &result);
		if (check == 0) {
			/* not found */
			if (result == NULL)
				return Result(ret, Unbound);
			/* ok */
			break;
		}
		if (check != ERANGE)
			return Result(ret, Unbound); /* error */

		/* retry */
		rollback_local(local, stack);
		size <<= 1;
		if (size == 0)
			return Result(ret, Unbound); /* size error */
	}

	return strvect_home_pathname_(local, (const byte *)pwd.pw_dir, ret);
}

static int uid_home_pathname_(struct fileparse *pa, addr x, addr *ret)
{
	return passwd_home_pathname_(NULL, pa, x, ret);
}

static int name_home_pathname_(struct fileparse *pa, addr x, addr *ret)
{
	const char *body;
	LocalRoot local;

	/* username */
	local = pa->ptr->local;
	Return(UTF8_buffer_clang_(local, &x, x));
	if (x == Unbound)
		return Result(ret, Unbound); /* encode error */
	posbody(x, (addr *)&body);
	body++; /* ~ */

	return passwd_home_pathname_(body, pa, x, ret);
}

static int string_home_pathname_(struct fileparse *pa, addr x, addr *ret)
{
	int check;

	Return(string_equal_char_(x, "~", &check));
	if (! check)
		return name_home_pathname_(pa, x, ret);

	Return(env_home_pathname_(pa, x, &x));
	if (x != Unbound)
		return Result(ret, x);

	return uid_home_pathname_(pa, x, ret);
}

static int split_home_pathname_(LocalpRoot localp, addr x, size_t i, addr *ret)
{
	addr car, cdr;
	unicode c;
	size_t size, a;
	LocalRoot local;

	/* ignore / */
	string_length(x, &size);
	for (;;) {
		/* end */
		if (size <= i)
			return Result(ret, Nil);

		/* slash */
		Return(string_getc_(x, i, &c));
		if (c != '/')
			break;
		i++;
	}

	/* next */
	a = i;
	for (;;) {
		if (size <= i)
			break;
		Return(string_getc_(x, i, &c));
		if (c == '/')
			break;
		i++;
	}
	if (a == i)
		return Result(ret, Nil);

	/* string */
	local = localp_alloc(localp);
	Return(strvect_subseq_alloc_(local, &car, x, a, i));
	Return(split_home_pathname_(localp, x, i, &cdr));
	cons_alloc(local, ret, car, cdr);

	return 0;
}

static int list_home_pathname_(struct fileparse *pa, addr x, addr *ret)
{
	LocalRoot local;
	LocalStack stack;
	LocalpRoot localp;

	localp = pa->local;
	local = pa->ptr->local;
	push_local(local, &stack);

	/* get home directory */
	Return(string_home_pathname_(pa, x, &x));
	if (x == Unbound) {
		rollback_local(local, stack);
		return Result(ret, Unbound);
	}

	/* split / */
	Return(split_home_pathname_(localp, x, 0, ret));

	/* heap only */
	if (! localp->localp)
		rollback_local(local, stack);

	/* ok */
	return 0;
}

static int make_home_pathname_(struct fileparse *pa, addr list, addr *ret)
{
	addr root, x;

	GetCdr(list, &list); /* :relative */
	GetCons(list, &x, &list); /* "~user" */
	Return(list_home_pathname_(pa, x, &x));
	if (x == Unbound)
		return Result(ret, Unbound);
	/* (:absolute ,@x . list) */
	GetConst(KEYWORD_ABSOLUTE, &root);
	cons_alloc(localp_alloc(pa->local), &root, root, x);
	return nconc2_safe_(root, list, ret);
}

static int parser_home_directory_pathname_(struct fileparse *pa)
{
	addr list;

	/* (:relative "~user" ...) -> (:absolute home path ...) */
	list = pa->directory;
	Return(make_home_pathname_(pa, list, &list));
	if (list != Unbound)
		pa->directory = list;

	return 0;
}
#else
static int parser_home_directory_pathname_(struct fileparse *pa)
{
	/* do nothing */
	return 0;
}
#endif


/*
 *  unix pathname
 */
static int parser_make_unix_pathname_(struct fileparse *pa)
{
	int check;

	GetConst(SYSTEM_UNIX, &pa->host);
	GetDevicePathname(pa->path, &pa->device);
	Return(wild_value_pathname_(pa->name, &pa->name));
	Return(wild_value_pathname_(pa->type, &pa->type));
	GetVersionPathname(pa->path, &pa->version);
	Return(parser_home_directory_p_pathname_(pa, &check));
	if (check) {
		Return(parser_home_directory_pathname_(pa));
	}
	pathname_fileparse_alloc(pa, 0);

	return 0;
}

int parser_unix_pathname_(struct fileparse *pa)
{
	int absolute, relative, logical, dp, check;
	unicode c;
	LocalpRoot local;
	addr charqueue, queue, thing, temp;
	size_t i, di, ni, size;
	struct fileparse backup;

	/* initialize */
	backup = *pa;
	local = pa->local;
	thing = pa->thing;
	if (! stringp(thing))
		return TypeError_(thing, STRING);
	charqueue_local(local->local, &charqueue, 0);
	pa->queue = charqueue;
	queue = Nil;
	size = pa->end;
	absolute = relative = logical = dp = 0;
	di = ni = 0;
	i = pa->start;

	/* start */
	if (size <= i)
		goto finish;
	Return(string_getc_(thing, i++, &c));
	if (c == '/') {
		absolute = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_ABSOLUTE);
		goto first;
	}
	if (c == '.') {
		di = ni;
		dp = 1;
	}
	if (c == ':')
		logical = 1;
	Return(push_charqueue_local_(local->local, charqueue, c));
	ni++;
	goto next1;

first:
	if (size <= i)
		goto finish;
	Return(string_getc_(thing, i++, &c));
	if (c == '/')
		goto first;
	if (c == '.') {
		di = ni;
		dp = 1;
	}
	Return(push_charqueue_local_(local->local, charqueue, c));
	ni++;
	goto next1;

next1:
	if (size <= i)
		goto name_finish;
	Return(string_getc_(thing, i++, &c));
	if (c == '/')
		goto next2;
	if (c == '.') {
		di = ni;
		dp = 1;
	}
	if (logical == 0 && c == ':') {
		Return(check_host_logical_pathname_(local, charqueue, &check));
		if (check) {
			/* parser logical */
			*pa = backup;
			return parser_logical_pathname_(pa);
		}
		logical = 1;
	}
	Return(push_charqueue_local_(local->local, charqueue, c));
	ni++;
	goto next1;

next2:
	if (absolute == 0 && relative == 0) {
		relative = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_RELATIVE);
	}
	make_charqueue_fileparse(pa, charqueue, &temp);
	clear_charqueue(charqueue);
	Return(pushdirectory_fileparse_(pa, &queue, temp));
	ni = di = dp = 0;
	goto first;

name_finish:
	make_charqueue_fileparse(pa, charqueue, &pa->name);
	if (di && dp) {
		Return(nametype_pathname_(pa, di));
	}
	goto finish;

finish:
	nreverse(&pa->directory, queue);
	pa->endpos = i;
	Return(parser_make_unix_pathname_(pa));
	return 0;
}


/************************************************************
 *  pathname_wildcard.c
 ************************************************************/

/*
 *  wild_pathname_boolean
 */
static int wild_pathname_string_(addr pos, int *ret)
{
	unicode c;
	size_t size, i;

	if (! stringp(pos))
		return Result(ret, 0);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (c == '*' || c == '?')
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int wild_pathname_boolean_(addr file, addr field, int *ret)
{
	int check;
	addr value, pos, wild1, wild2, path;

	Check(! pathnamep(file), "type error");
	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);

	/* skip host*/
	/* skip device */

	/* directory */
	GetConst(KEYWORD_DIRECTORY, &value);
	if (field == value || field == Nil) {
		GetDirectoryPathname(file, &pos);
		if (pos == wild1)
			return Result(ret, 1);
		while (consp(pos)) {
			GetCons(pos, &path, &pos);
			if (path == wild1)
				return Result(ret, 1);
			if (path == wild2)
				return Result(ret, 1);
			Return(wild_pathname_string_(path, &check));
			if (check)
				return Result(ret, 1);
		}
	}

	/* name */
	GetConst(KEYWORD_NAME, &value);
	if (field == value || field == Nil) {
		GetNamePathname(file, &pos);
		if (pos == wild1)
			return Result(ret, 1);
		Return(wild_pathname_string_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	/* type */
	GetConst(KEYWORD_TYPE, &value);
	if (field == value || field == Nil) {
		GetTypePathname(file, &pos);
		if (pos == wild1)
			return Result(ret, 1);
		Return(wild_pathname_string_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	/* version */
	GetConst(KEYWORD_VERSION, &value);
	if (field == value || field == Nil) {
		GetVersionPathname(file, &pos);
		if (pos == wild1)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}


/*
 *  wildcard-pathname-p
 */
static int wildcard_character_pathname_(
		addr p1, size_t n1, size_t s1,
		addr p2, size_t n2, size_t s2,
		int *ret)
{
	int check;
	unicode c1, c2;
	size_t i;

	if (n1 == s1 && n2 == s2)
		return Result(ret, 1);
	if (n1 == s1 || n2 == s2)
		return Result(ret, 0);
	Return(string_getc_(p1, n1, &c1));
	Return(string_getc_(p2, n2, &c2));
	/* (a ?) -> next */
	if (c2 == '?')
		return wildcard_character_pathname_(p1,n1+1,s1,  p2,n2+1,s2,  ret);
	/* (a a) -> next, (a b) -> false */
	if (c2 != '*') {
		if (c1 != c2)
			return Result(ret, 0);
		else
			return wildcard_character_pathname_(p1,n1+1,s1,  p2,n2+1,s2,  ret);
	}
	/* (a *) */
	n2++;
	for (i = n1; i <= s1; i++) {
		Return(wildcard_character_pathname_(p1,i,s1,  p2,n2,s2,  &check));
		if (check)
			return Result(ret, 1);
	}
	return Result(ret, 0);
}

static int wildcard_string_p_(addr pos, int *ret)
{
	unicode c;
	size_t size, i;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (c == '*' || c == '?')
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int wildcard_stringp_p_(addr pos, int *ret)
{
	if (! stringp(pos))
		return Result(ret, 0);
	else
		return wildcard_string_p_(pos, ret);
}

int wildcard_string_pathname_(addr a, addr b, lisp_equal_calltype equal, int *ret)
{
	int check, check1, check2;
	addr wild;
	size_t s1, s2;

	GetConst(KEYWORD_WILD, &wild);
	if (a == wild && b == wild)
		return Result(ret, 1);
	check1 = stringp(a);
	check2 = stringp(b);
	if (check1 && b == wild)
		return Result(ret, 1);
	if ((! check1) || (! check2))
		return Result(ret, 0);
	Return((*equal)(a, b, &check));
	if (check)
		return Result(ret, 1);
	Return(wildcard_string_p_(a, &check));
	if (check)
		return Result(ret, 0);
	string_length(a, &s1);
	string_length(b, &s2);
	return wildcard_character_pathname_(a, 0, s1, b, 0, s2, ret);
}

int wildcard_eq_pathname_(addr a, addr b, lisp_equal_calltype equal, int *ret)
{
	if (a == b)
		return Result(ret, 1);
	else
		return wildcard_string_pathname_(a, b, equal, ret);
}

static int wildcard_nil_pathname_(addr a, addr b, int wildp,
		lisp_equal_calltype equal, int *ret)
{
	addr wild;

	GetConst(KEYWORD_WILD, &wild);
	if (wildp && a == Nil)
		a = wild;
	if (wildp && b == Nil)
		b = wild;

	return wildcard_eq_pathname_(a, b, equal, ret);
}

static int wildcard_list_pathname_(addr a, addr b, lisp_equal_calltype equal, int *ret)
{
	int check;
	addr a1, b1, pos1, pos2, wild, wilds;

	if (a == Nil && b == Nil)
		return Result(ret, 1);
	if (a != Nil && b == Nil)
		return Result(ret, 0);
	GetConst(KEYWORD_WILD_INFERIORS, &wilds);
	if (a == Nil && b != Nil) {
		while (b != Nil) {
			Return_getcons(b, &pos2, &b);
			if (pos2 != wilds)
				return Result(ret, 0);
		}
		return Result(ret, 1);
	}
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wilds);
	Return_getcons(a, &pos1, &a1);
	Return_getcons(b, &pos2, &b1);
	/* ("str" *) -> next */
	if (pos2 == wild)
		return wildcard_list_pathname_(a1, b1, equal, ret);
	/* ("str" "str") -> next, ("str" "aaa") -> false */
	if (pos2 != wilds) {
		Return(wildcard_string_pathname_(pos1, pos2, equal, &check));
		if (! check)
			return Result(ret, 0);
		else
			return wildcard_list_pathname_(a1, b1, equal, ret);
	}
	/* ("str" **) */
	for (;;) {
		Return(wildcard_list_pathname_(a, b1, equal, &check));
		if (check)
			return Result(ret, 1);
		if (a == Nil)
			break;
		Return_getcdr(a, &a);
	}

	return Result(ret, 0);
}

static int wildcard_directory_p_(addr pos, int *ret)
{
	addr check;

	GetConst(KEYWORD_WILD, &check);
	if (pos == check)
		return Result(ret, 1);
	GetConst(KEYWORD_WILD_INFERIORS, &check);
	if (pos == check)
		return Result(ret, 1);

	return wildcard_stringp_p_(pos, ret);
}

static int wildcard_directory_pathname_(addr a, addr b,
		lisp_equal_calltype equal, int *ret)
{
	int check, check1, check2;
	addr car1, car2, cdr1, cdr2;

	/* right is nil */
	if (b == Nil)
		return Result(ret, 1);

	/* compare */
	cdr1 = a;
	cdr2 = b;
	check1 = 0;
	check2 = 1;
	for (;;) {
		if (cdr1 == Nil && cdr2 == Nil)
			return Result(ret, 1);
		if (cdr1 == Nil || cdr2 == Nil)
			break;
		Return_getcons(cdr1, &car1, &cdr1);
		Return_getcons(cdr2, &car2, &cdr2);
		Return(wildcard_directory_p_(car1, &check));
		if (check)
			check1 = 1;
		Return(wildcard_directory_p_(car2, &check));
		if (check)
			check2 = 1;
		Return((*equal)(car1, car2, &check));
		if (! check)
			break;
	}
	if (check1 || (! check2)) {
		return Result(ret, 0);
	}
	else {
		Return_getcdr(a, &a);
		Return_getcdr(b, &b);
		return wildcard_list_pathname_(a, b, equal, ret);
	}
}

static int wildcard_version_pathname(addr a, addr b)
{
	addr wild;

	GetConst(KEYWORD_WILD, &wild);
	if (a == Nil)
		a = wild;
	if (b == Nil)
		b = wild;
	if (eql_function(a, b))
		return 1;

	return b == wild;
}

int wildcard_pathname_(addr a, addr b, int wild, int *ret)
{
	int check;
	addr check1, check2;
	lisp_equal_calltype equal;

	Check(! pathnamep(a), "type left error");
	Check(! pathnamep(b), "type right error");
	if (RefLogicalPathname(a) != RefLogicalPathname(b))
		return Result(ret, 0);

	/* host */
	GetHostPathname(a, &check1);
	GetHostPathname(b, &check2);
	Return(equalp_function_(check1, check2, &check));
	if (! check)
		return Result(ret, 0);
	equal = pathname_equal_function(a);

	/* device */
	GetDevicePathname(a, &check1);
	GetDevicePathname(b, &check2);
	Return((*equal)(check1, check2, &check));
	if (! check)
		return Result(ret, 0);

	/* directory */
	GetDirectoryPathname(a, &check1);
	GetDirectoryPathname(b, &check2);
	Return(wildcard_directory_pathname_(check1, check2, equal, &check));
	if (! check)
		return Result(ret, 0);

	/* name */
	GetNamePathname(a, &check1);
	GetNamePathname(b, &check2);
	Return(wildcard_nil_pathname_(check1, check2, wild, equal, &check));
	if (! check)
		return Result(ret, 0);

	/* type */
	GetTypePathname(a, &check1);
	GetTypePathname(b, &check2);
	Return(wildcard_nil_pathname_(check1, check2, wild, equal, &check));
	if (! check)
		return Result(ret, 0);

	/* version */
	pathname_version(a, &check1);
	pathname_version(b, &check2);
	if (! wildcard_version_pathname(check1, check2))
		return Result(ret, 0);

	return Result(ret, 1);
}


/************************************************************
 *  pathname_windows.c
 ************************************************************/

/*
 *  windows pathname
 */
static int parser_make_windows_pathname_(struct fileparse *pa)
{
	GetConst(SYSTEM_WINDOWS, &pa->host);
	Return(wild_value_pathname_(pa->name, &pa->name));
	Return(wild_value_pathname_(pa->type, &pa->type));
	GetVersionPathname(pa->path, &pa->version);
	pathname_fileparse_alloc(pa, 0);

	return 0;
}

static int parser_windows_bsbs_p_(addr thing, size_t size, int *ret)
{
	unicode c;

	if (size < 2)
		return Result(ret, 0);
	Return(string_getc_(thing, 0, &c));
	if (c != '\\')
		return Result(ret, 0);
	Return(string_getc_(thing, 1, &c));

	return Result(ret, (c == '\\'));
}

static int parser_windows_bs_p_(addr thing, size_t size, unicode x, int *ret)
{
	unicode c;

	if (size < 4)
		return Result(ret, 0);
	Return(string_getc_(thing, 2, &c));
	if (c != x)
		return Result(ret, 0);
	Return(string_getc_(thing, 3, &c));

	return Result(ret, (c == '\\'));
}

static int parser_windows_unc_p_(addr thing, size_t size, int *ret)
{
	unicode c;

	if (size < 2)
		return Result(ret, 0);
	Return(string_getc_(thing, 2, &c));

	return Result(ret, (c != '\\'));
}

static int parser_windows_drive_p_(addr thing,
		size_t size, size_t i, unicode *value, int *ret)
{
	unicode c, drive;

	if ((size - i) < 2)
		return Result(ret, 0);
	Return(string_getc_(thing, i, &drive));
	if (! isAlphabetic(drive))
		return Result(ret, 0);
	Return(string_getc_(thing, i + 1, &c));
	if (c != ':')
		return Result(ret, 0);
	*value = drive;

	return Result(ret, 1);
}

static int parser_windows_drive_(struct fileparse *pa, unicode c)
{
	addr pos;

	strvect_alloc(localp_alloc(pa->local), &pos, 1);
	Return(strvect_setc_(pos, 0, toUpperUnicode(c)));
	pa->device = pos;

	return 0;
}

static int parser_windows_device_(struct fileparse *pa,
		addr thing, size_t size, size_t i)
{
	addr queue;

	/* drive */
	GetConst(SYSTEM_DEVICE, &pa->device);
	/* name */
	queue = pa->queue;
	Return(pushrange_pathname_(pa->local, queue, thing, i, size));
	make_charqueue_fileparse(pa, queue, &pa->name);
	clear_charqueue(queue);

	return 0;
}

static int parser_windows_slash_p(unicode x)
{
	return x == '/' || x == '\\';
}

int parser_windows_pathname_(struct fileparse *pa)
{
	int absolute, relative, logical, dp, check;
	unicode c;
	LocalpRoot local;
	addr charqueue, queue, thing, temp;
	size_t i, di, ni, size;
	struct fileparse backup;

	/* initialize */
	backup = *pa;
	local = pa->local;
	thing = pa->thing;
	if (! stringp(thing))
		return TypeError_(thing, STRING);
	charqueue_local(local->local, &charqueue, 0);
	pa->queue = charqueue;
	queue = Nil;
	size = pa->end;
	absolute = relative = logical = dp = 0;
	di = ni = 0;
	i = pa->start;

	/* Windows */
	Return(parser_windows_bsbs_p_(thing, size, &check));
	if (check) {
		logical = 1;
		Return(parser_windows_bs_p_(thing, size, '?', &check));
		if (check)
			goto question;
		Return(parser_windows_bs_p_(thing, size, '.', &check));
		if (check)
			goto dot;
		Return(parser_windows_unc_p_(thing, size, &check));
		if (check)
			goto universal;
	}

drive1:
	Return(parser_windows_drive_p_(thing, size, i, &c, &check));
	if (check)
		goto drive2;
	goto start;

question: /* ignore */
	i += 4;
	goto drive1;

dot:
	Return(parser_windows_device_(pa, thing, size, 4));
	goto finish;

universal: /* ignore */
	i += 1; /* not 2 */
	GetConst(SYSTEM_UNIVERSAL, &pa->device);
	goto start;

drive2:
	Return(check_drive_logical_pathname_(local, (int)c, &check));
	if (check) {
		/* parser logical */
		*pa = backup;
		return parser_logical_pathname_(pa);
	}
	logical = 1;
	Return(parser_windows_drive_(pa, c));
	i += 2;
	goto start;

	/* start */
start:
	if (size <= i)
		goto finish;
	Return(string_getc_(thing, i++, &c));
	if (parser_windows_slash_p(c)) {
		absolute = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_ABSOLUTE);
		goto first;
	}
	if (c == '.') {
		di = ni;
		dp = 1;
	}
	Return(push_charqueue_local_(local->local, charqueue, c));
	ni++;
	goto next1;

first:
	if (size <= i)
		goto finish;
	Return(string_getc_(thing, i++, &c));
	if (parser_windows_slash_p(c))
		goto first;
	if (c == '.') {
		di = ni;
		dp = 1;
	}
	if (c == ':')
		logical = 1;
	Return(push_charqueue_local_(local->local, charqueue, c));
	ni++;
	goto next1;

next1:
	if (size <= i)
		goto finish1;
	Return(string_getc_(thing, i++, &c));
	if (parser_windows_slash_p(c))
		goto next2;
	if (c == '.') {
		di = ni;
		dp = 1;
	}
	if (logical == 0 && c == ':') {
		Return(check_host_logical_pathname_(local, charqueue, &check));
		if (check) {
			/* parser logical */
			*pa = backup;
			return parser_logical_pathname_(pa);
		}
		logical = 1;
	}
	Return(push_charqueue_local_(local->local, charqueue, c));
	ni++;
	goto next1;

next2:
	if (absolute == 0 && relative == 0) {
		relative = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_RELATIVE);
	}
	make_charqueue_fileparse(pa, charqueue, &temp);
	clear_charqueue(charqueue);
	Return(pushdirectory_fileparse_(pa, &queue, temp));
	ni = di = dp = 0;
	goto first;

finish1:
	make_charqueue_fileparse(pa, charqueue, &pa->name);
	if (di && dp) {
		Return(nametype_pathname_(pa, di));
	}
	goto finish;

finish:
	nreverse(&pa->directory, queue);
	pa->endpos = i;
	return parser_make_windows_pathname_(pa);
}


/************************************************************
 *  pointer.c
 ************************************************************/

void clear_pointer(void)
{
#ifdef LISP_DEBUG
	int p;

	memset(pointer_table, 0, sizeoft(pointer_table));
	for (p = 0; p < SizePointer; p++) {
		SetPointer(p, error, NULL);
	}
#endif
}


/************************************************************
 *  print.c
 ************************************************************/

/*
 *  special variable
 */
static int getbool_print_(Execute ptr, int *ret, constindex index)
{
	addr pos;

	GetConstant(index, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));

	return Result(ret, pos != Nil);
}

static int readably_true_print_(Execute ptr, int *ret, constindex index)
{
	addr pos;

	/* variable */
	GetConstant(index, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	if (pos != Nil)
		return Result(ret, 1);

	/* *print-readably* */
	GetConst(SPECIAL_PRINT_READABLY, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));

	return Result(ret, pos != Nil);
}

static void pushbool_print(Execute ptr, constindex index, int value)
{
	addr symbol;
	GetConstant(index, &symbol);
	pushspecial_control(ptr, symbol, value? T: Nil);
}

/* print-array */
int array_print_(Execute ptr, int *ret)
{
	return readably_true_print_(ptr, ret, CONSTANT_SPECIAL_PRINT_ARRAY);
}

void push_array_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_ARRAY, value);
}

/* print-base */
int base_print_(Execute ptr, unsigned *ret)
{
	addr pos;
	fixnum value;

	GetConst(SPECIAL_PRINT_BASE, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	if (! fixnump(pos))
		return TypeError_(pos, FIXNUM);
	GetFixnum(pos, &value);
	if (! isBaseChar(value))
		return fmte_("The value ~S must be a number between 2 and 36.", pos, NULL);

	return Result(ret, (unsigned)value);
}

void push_base_print(Execute ptr, unsigned base)
{
	addr pos, value;

	Check(! isBaseChar(base), "base value error");
	fixnum_heap(&value, (fixnum)base);
	GetConst(SPECIAL_PRINT_BASE, &pos);
	pushspecial_control(ptr, pos, value);
}

/* print-radix */
int radix_print_(Execute ptr, int *ret)
{
	return getbool_print_(ptr, ret, CONSTANT_SPECIAL_PRINT_RADIX);
}

void push_radix_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_RADIX, value);
}

/* print-case */
int case_print_(Execute ptr, enum PrintCase *ret)
{
	addr pos, value;

	/* special */
	GetConst(SPECIAL_PRINT_CASE, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	/* upcase */
	GetConst(KEYWORD_UPCASE, &value);
	if (pos == value)
		return Result(ret, PrintCase_upcase);
	/* downcase */
	GetConst(KEYWORD_DOWNCASE, &value);
	if (pos == value)
		return Result(ret, PrintCase_downcase);
	/* capitalize */
	GetConst(KEYWORD_CAPITALIZE, &value);
	if (pos == value)
		return Result(ret, PrintCase_capitalize);

	/* error */
	*ret = PrintCase_unread;
	return fmte_("type error", NULL);
}

int push_case_print_(Execute ptr, enum PrintCase pcase)
{
	addr pos, value;

	switch (pcase) {
		case PrintCase_upcase:
			GetConst(KEYWORD_UPCASE, &value);
			break;

		case PrintCase_downcase:
			GetConst(KEYWORD_DOWNCASE, &value);
			break;

		case PrintCase_capitalize:
			GetConst(KEYWORD_CAPITALIZE, &value);
			break;

		default:
			return fmte_("type error", NULL);
	}
	GetConst(SPECIAL_PRINT_CASE, &pos);
	pushspecial_control(ptr, pos, value);
	return 0;
}

/* print-circle */
int circle_print_(Execute ptr, int *ret)
{
	return getbool_print_(ptr, ret, CONSTANT_SPECIAL_PRINT_CIRCLE);
}

void push_circle_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_CIRCLE, value);
}

/* print-escape */
int escape_print_(Execute ptr, int *ret)
{
	return readably_true_print_(ptr, ret, CONSTANT_SPECIAL_PRINT_ESCAPE);
}

void push_escape_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_ESCAPE, value);
}

/* print-gensym */
int gensym_print_(Execute ptr, int *ret)
{
	return readably_true_print_(ptr, ret, CONSTANT_SPECIAL_PRINT_GENSYM);
}

void push_gensym_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_GENSYM, value);
}

/* print-readably */
int readably_print_(Execute ptr, int *ret)
{
	return getbool_print_(ptr, ret, CONSTANT_SPECIAL_PRINT_READABLY);
}

void push_readably_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_READABLY, value);
}

/* print-pretty */
int pretty_print_(Execute ptr, int *ret)
{
	return getbool_print_(ptr, ret, CONSTANT_SPECIAL_PRINT_PRETTY);
}

void push_pretty_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_PRETTY, value);
}

/* print-level */
static int getindex_print_(Execute ptr, size_t *value, int *ret, constindex index)
{
	addr pos;

	/* special */
	GetConstant(index, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	/* value */
	if (pos == Nil) {
		*value = 0;
		return Result(ret, 0);
	}
	else {
		Return(getindex_fixnum_(pos, value));
		return Result(ret, 1);
	}
}

static int readably_index_print_(Execute ptr, size_t *value, int *ret, constindex index)
{
	addr pos;

	/* *print-readably* */
	GetConst(SPECIAL_PRINT_READABLY, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	if (pos != Nil) {
		*value = 0;
		return Result(ret, 0);
	}

	/* index */
	return getindex_print_(ptr, value, ret, index);
}

static int push_integer_print_(Execute ptr, constindex index, size_t value)
{
	addr symbol, pos;

	GetConstant(index, &symbol);
	Return(fixnum_index_heap_(&pos, value));
	pushspecial_control(ptr, symbol, pos);

	return 0;
}

static void push_nil_print(Execute ptr, constindex index)
{
	addr pos;
	GetConstant(index, &pos);
	pushspecial_control(ptr, pos, Nil);
}

int level_print_(Execute ptr, size_t *value, int *ret)
{
	return readably_index_print_(ptr, value, ret, CONSTANT_SPECIAL_PRINT_LEVEL);
}

int push_level_print_(Execute ptr, size_t value)
{
	return push_integer_print_(ptr, CONSTANT_SPECIAL_PRINT_LEVEL, value);
}

void push_level_nil_print(Execute ptr)
{
	push_nil_print(ptr, CONSTANT_SPECIAL_PRINT_LEVEL);
}

/* print-length */
int length_print_(Execute ptr, size_t *value, int *ret)
{
	return readably_index_print_(ptr, value, ret, CONSTANT_SPECIAL_PRINT_LENGTH);
}

int push_length_print_(Execute ptr, size_t value)
{
	return push_integer_print_(ptr, CONSTANT_SPECIAL_PRINT_LENGTH, value);
}

void push_length_nil_print(Execute ptr)
{
	push_nil_print(ptr, CONSTANT_SPECIAL_PRINT_LENGTH);
}

/* print-lines */
int lines_print_(Execute ptr, size_t *value, int *ret)
{
	return readably_index_print_(ptr, value, ret, CONSTANT_SPECIAL_PRINT_LINES);
}

int push_lines_print_(Execute ptr, size_t value)
{
	return push_integer_print_(ptr, CONSTANT_SPECIAL_PRINT_LINES, value);
}

void push_lines_nil_print(Execute ptr)
{
	push_nil_print(ptr, CONSTANT_SPECIAL_PRINT_LINES);
}

/* print-miser-width */
int miser_width_print_(Execute ptr, size_t *value, int *ret)
{
	return getindex_print_(ptr, value, ret, CONSTANT_SPECIAL_PRINT_MISER_WIDTH);
}

int push_miser_width_print_(Execute ptr, size_t value)
{
	return push_integer_print_(ptr, CONSTANT_SPECIAL_PRINT_MISER_WIDTH, value);
}

void push_miser_width_nil_print(Execute ptr)
{
	push_nil_print(ptr, CONSTANT_SPECIAL_PRINT_MISER_WIDTH);
}

/* print-right-margin */
int right_margin_print_(Execute ptr, addr stream, size_t *ret)
{
	int check;
	addr pos;
	size_t size;

	GetConst(SPECIAL_PRINT_RIGHT_MARGIN, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	if (pos == Nil) {
		Return(termsize_stream_(stream, &size, &check));
		if (check)
			size = PRINT_DEFAULT_WIDTH;
		*ret = size;
	}
	else {
		Return(getindex_fixnum_(pos, ret));
	}

	return 0;
}

int push_right_margin_print_(Execute ptr, size_t value)
{
	return push_integer_print_(ptr, CONSTANT_SPECIAL_PRINT_RIGHT_MARGIN, value);
}

void push_right_margin_nil_print(Execute ptr)
{
	push_nil_print(ptr, CONSTANT_SPECIAL_PRINT_RIGHT_MARGIN);
}

/* print-dispatch */
int pprint_dispatch_print_(Execute ptr, addr *ret)
{
	addr pos;
	GetConst(SPECIAL_PRINT_PPRINT_DISPATCH, &pos);
	return getspecialcheck_local_(ptr, pos, ret);
}

void push_pprint_dispatch(Execute ptr, addr value)
{
	addr pos;
	GetConst(SPECIAL_PRINT_PPRINT_DISPATCH, &pos);
	pushspecial_control(ptr, pos, value);
}


/*
 *  print-unreadable-object
 */
static int print_unreadable_call_(Execute ptr, addr stream, addr pos,
		int type, int identity, calltype_print call, addr body)
{
	char buffer[32];
	int first, check;
	addr value;

	/* print-not-readable */
	Return(readably_print_(ptr, &check));
	if (check)
		return call_print_not_readable_(ptr, pos);

	/* begin */
	first = 1;
	Return(print_ascii_stream_(stream, "#<"));
	/* type */
	if (type) {
		Return(type_value_(&value, pos));
		Return(type_object_(&value, value));
		Return(princ_print_(ptr, stream, value));
		first = 0;
	}
	/* call */
	if (call) {
		if (first == 0) {
			Return(write_char_stream_(stream, ' '));
		}
		Return((*call)(ptr, stream, pos));
		first = 0;
	}
	/* body */
	if (body) {
		if (first == 0) {
			Return(write_char_stream_(stream, ' '));
		}
		Return(apply1_control_(ptr, &body, body, Nil));
		first = 0;
	}
	/* identity */
	if (identity) {
		if (first == 0) {
			Return(write_char_stream_(stream, ' '));
		}
		snprintf(buffer, 32, "#x%zx", (size_t)pos);
		Return(print_ascii_stream_(stream, buffer));
	}
	/* end */
	return write_char_stream_(stream, '>');
}

int print_unreadable_object_(Execute ptr, addr stream, addr pos,
		int type, int identity, calltype_print call)
{
	return print_unreadable_call_(ptr, stream, pos, type, identity, call, NULL);
}

int print_unreadable_common_(Execute ptr, addr stream, addr pos,
		int type, int identity, addr body)
{
	Return(output_stream_designator_(ptr, stream, &stream));
	return print_unreadable_call_(ptr, stream, pos, type, identity, NULL, body);
}


/*
 *  initialize
 */
void build_print(Execute ptr)
{
	Error(build_print_object_(ptr));
	Error(build_print_dispatch_());
}

void init_print(void)
{
	init_print_function();
	init_print_object();
	init_print_write();
}


/************************************************************
 *  print_dispatch.c
 ************************************************************/

/*
 *  access
 */
void getlistprintdispatch(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PRINT_DISPATCH);
	GetListPrintDispatch_Low(pos, ret);
}

void setlistprintdispatch(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PRINT_DISPATCH);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetListPrintDispatch_Low(pos, value);
}

void gettypeprinttable(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	GetTypePrintTable_Low(pos, ret);
}

void settypeprinttable(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetTypePrintTable_Low(pos, value);
}

void getspecifierprinttable(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	GetSpecifierPrintTable_Low(pos, ret);
}

void setspecifierprinttable(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetSpecifierPrintTable_Low(pos, value);
}

void getfunctionprinttable(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	GetFunctionPrintTable_Low(pos, ret);
}

void setfunctionprinttable(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFunctionPrintTable_Low(pos, value);
}

void getpriorityprinttable(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	GetPriorityPrintTable_Low(pos, ret);
}

void setpriorityprinttable(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetPriorityPrintTable_Low(pos, value);
}


/*
 *  object
 */
int print_dispatch_p(addr pos)
{
	return GetType(pos) == LISPTYPE_PRINT_DISPATCH;
}

void pprint_dispatch_heap(addr *ret)
{
	heap_array2(ret, LISPTYPE_PRINT_DISPATCH, PrintDispatch_size);
}

static void print_table_heap(addr *ret)
{
	heap_array2(ret, LISPSYSTEM_PRINT_TABLE, PrintTable_size);
}

static void copy_print_table(addr *ret, addr var)
{
	addr x, pos;

	print_table_heap(&x);
	GetTypePrintTable(var, &pos);
	SetTypePrintTable(x, pos);
	GetSpecifierPrintTable(var, &pos);
	SetSpecifierPrintTable(x, pos);
	GetFunctionPrintTable(var, &pos);
	SetFunctionPrintTable(x, pos);
	GetPriorityPrintTable(var, &pos);
	SetPriorityPrintTable(x, pos);
	*ret = x;
}

static void copy_pprint_dispatch(addr var, addr *ret)
{
	addr list, root, x;

	/* copy list */
	CheckType(var, LISPTYPE_PRINT_DISPATCH);
	GetListPrintDispatch(var, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		copy_print_table(&x, x);
		cons_heap(&root, x, root);
	}
	nreverse(&root, root);

	/* make result */
	pprint_dispatch_heap(&x);
	SetListPrintDispatch(x, root);
	*ret = x;
}

static int find_print_dispatch_p_(LocalRoot local, addr a, addr b, int *ret)
{
	if (a == Nil)
		return Result(ret, 1);
	else
		return less_real_(local, a, b, ret);
}

static int find_print_dispatch_(Execute ptr, addr var, addr list, addr *ret)
{
	int check;
	addr pos, type, value, a, b;

	GetListPrintDispatch(list, &list);
	a = value = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetTypePrintTable(pos, &type);
		Return(typep_clang_(ptr, var, type, &check));
		if (check) {
			GetPriorityPrintTable(pos, &b);
			Return(find_print_dispatch_p_(ptr->local, a, b, &check));
			if (check) {
				a = b;
				value = pos;
			}
		}
	}

	return Result(ret, value);
}

int find_function_print_dispatch_(Execute ptr, addr var, addr table, addr *ret)
{
	Return(find_print_dispatch_(ptr, var, table, &var));
	if (var != Nil)
		GetFunctionPrintTable(var, &var);

	return Result(ret, var);
}

static int delete_print_dispatch_p_(LocalRoot local,
		addr pos, addr spec, addr priority, int *ret)
{
	int check;
	addr a;

	/* specifier */
	GetSpecifierPrintTable(pos, &a);
	Return(equal_function_(a, spec, &check));
	if (! check)
		return Result(ret, 0);
	/* priority */
	if (priority == Unbound)
		return Result(ret, 1);
	GetPriorityPrintTable(pos, &a);

	return equal_real_(local, a, priority, ret);
}

static int delete_print_dispatch_(LocalRoot local, addr spec, addr priority, addr table)
{
	int delp, check;
	addr root, list, pos;

	GetListPrintDispatch(table, &list);
	delp = 0;
	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(delete_print_dispatch_p_(local, pos, spec, priority, &check));
		if (check) {
			delp = 1;
			continue;
		}
		cons_heap(&root, pos, root);
	}
	if (delp) {
		nreverse(&root, root);
		SetListPrintDispatch(table, root);
	}

	return 0;
}

static void set_print_dispatch(addr spec, addr type,
		addr call, addr priority, addr table)
{
	addr x, list;

	/* print-table */
	print_table_heap(&x);
	SetTypePrintTable(x, type);
	SetSpecifierPrintTable(x, spec);
	SetFunctionPrintTable(x, call);
	SetPriorityPrintTable(x, priority);
	/* print-dispatch */
	GetListPrintDispatch(table, &list);
	cons_heap(&list, x, list);
	SetListPrintDispatch(table, list);
}


/*
 *  common
 */
int copy_pprint_dispatch_common_(Execute ptr, addr var, addr *ret)
{
	if (var == Unbound || var == Nil) {
		Return(pprint_dispatch_print_(ptr, &var));
	}
	copy_pprint_dispatch(var, ret);
	return 0;
}

int pprint_dispatch_common_(Execute ptr, addr var, addr table, addr *x, addr *y)
{
	if (table == Unbound) {
		Return(pprint_dispatch_print_(ptr, &table));
	}
	if (table == Nil) {
		GetConst(SYSTEM_EMPTY_PRINT_DISPATCH, &table);
		GetValueSymbol(table, &table);
	}
	CheckType(table, LISPTYPE_PRINT_DISPATCH);
	Return(find_print_dispatch_(ptr, var, table, &var));
	if (var != Nil) {
		GetFunctionPrintTable(var, x);
		*y = T;
	}
	else {
		GetConst(SYSTEM_WRITE_DEFAULT, &var);
		Return(getfunction_global_(var, x));
		*y = Nil;
	}

	return 0;
}

int set_pprint_dispatch_print_(LocalRoot local,
		addr spec, addr type, addr call, addr priority, addr table)
{
	CheckType(type, LISPTYPE_TYPE);
	CheckType(table, LISPTYPE_PRINT_DISPATCH);

	Check(call != Nil && (! functionp(call)), "type error: function");
	Return(delete_print_dispatch_(local, spec, priority, table));
	if (call != Nil) {
		if (priority == Unbound)
			fixnum_heap(&priority, 0);
		Check(! integerp(priority), "type error: priority");
		set_print_dispatch(spec, type, call, priority, table);
	}

	return 0;
}


/*
 *  *default-print-dispatch*
 */
static void build_print_dispatch_empty(void)
{
	addr symbol, pos;

	/* system::*empty-print-dispatch* */
	pprint_dispatch_heap(&pos);
	GetConst(SYSTEM_EMPTY_PRINT_DISPATCH, &symbol);
	SetValueSymbol(symbol, pos);
}

static void build_print_dispatch_table(addr dispatch)
{
	addr symbol, pos;

	copy_pprint_dispatch(dispatch, &pos);
	/* system::*default-print-dispatch* */
	GetConst(SYSTEM_DEFAULT_PRINT_DISPATCH, &symbol);
	SetValueSymbol(symbol, dispatch);
	/* common-lisp::*print-pprint-dispatch* */
	GetConst(SPECIAL_PRINT_PPRINT_DISPATCH, &symbol);
	SetValueSymbol(symbol, pos);
}

static int build_print_dispatch_cons_(LocalRoot local, addr dispatch)
{
	/* (system::set-pprint-dispatch
	 *   'cons
	 *   type #'pprint-fill -10 dispatch)
	 */
	addr spec, type, call, priority;

	GetConst(COMMON_CONS, &spec);
	GetTypeTable(&type, Cons);
	GetConst(COMMON_PPRINT_FILL, &call);
	GetFunctionSymbol(call, &call);
	fixnum_heap(&priority, -10);
	return set_pprint_dispatch_print_(local, spec, type, call, priority, dispatch);
}

static void make_print_dispatch_function(addr *ret, constindex name, pointer id)
{
	addr symbol, pos, type;

	/* function */
	GetConstant(name, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, id);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, DispatchFunction);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
	/* result */
	*ret = pos;
}

static int build_print_dispatch_vector_(LocalRoot local, addr dispatch)
{
	/* (system::set-pprint-dispatch
	 *  '(and vector (not string) (not bit-vector))
	 *  type #<FUNCTION> 0 dispatch)
	 */
	addr spec, type, call, priority;
	addr type1, type2, type3;

	/* spec */
	GetConst(COMMON_VECTOR, &type1);
	GetConst(COMMON_NOT, &type);
	GetConst(COMMON_STRING, &type2);
	list_heap(&type2, type, type2, NULL);
	GetConst(COMMON_BIT_VECTOR, &type3);
	list_heap(&type3, type, type3, NULL);
	GetConst(COMMON_AND, &type);
	list_heap(&spec, type, type1, type2, type3, NULL);
	/* type */
	GetTypeTable(&type1, Vector);
	GetTypeTable(&type3, Asterisk);
	type1not_heap(LISPDECL_STRING, type3, &type2);
	type1not_heap(LISPDECL_BIT_VECTOR, type3, &type3);
	type3and_heap(type1, type2, type3, &type);
	/* function */
	make_print_dispatch_function(&call,
			CONSTANT_SYSTEM_DISPATCH_VECTOR,
			p_pprint_dispatch_vector);
	/* set */
	fixnum_heap(&priority, 0);
	return set_pprint_dispatch_print_(local, spec, type, call, priority, dispatch);
}

static int build_print_dispatch_quote_(LocalRoot local, addr dispatch)
{
	/* (system::set-pprint-dispatch
	 *   '(cons (eql quote) (cons t null))
	 *   type #<FUNCTION> -4 dispatch)
	 */
	addr spec, type, call, priority;
	addr cons, car, eql, quote, null;

	/* spec */
	GetConst(COMMON_CONS, &cons);
	GetConst(COMMON_EQL, &eql);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_NULL, &null);
	list_heap(&spec, cons, T, null, NULL);
	list_heap(&car, eql, quote, NULL);
	list_heap(&spec, cons, car, spec, NULL);
	/* type */
	GetTypeTable(&type, T);
	GetTypeTable(&null, Null);
	type2_heap(LISPDECL_CONS, type, null, &type);
	type_eql_heap(quote, &car);
	type2_heap(LISPDECL_CONS, car, type, &type);
	/* function */
	make_print_dispatch_function(&call,
			CONSTANT_SYSTEM_DISPATCH_QUOTE,
			p_pprint_dispatch_quote);
	/* set */
	fixnum_heap(&priority, 0);
	return set_pprint_dispatch_print_(local, spec, type, call, priority, dispatch);
}

static int build_print_dispatch_call_(LocalRoot local, addr dispatch)
{
	/* (system::set-pprint-dispatch
	 *   '(cons (and symbol (satisfies fboundp)))
	 *   type #<FUNCTION> -5 dispatch)
	 */
	addr spec, type, call, priority;
	addr type1, type2, type3, fboundp;

	/* spec */
	GetConst(COMMON_SATISFIES, &type3);
	GetConst(COMMON_FBOUNDP, &fboundp);
	list_heap(&type3, type3, fboundp, NULL);
	GetConst(COMMON_AND, &type1);
	GetConst(COMMON_SYMBOL, &type2);
	list_heap(&type2, type1, type2, type3, NULL);
	GetConst(COMMON_CONS, &type1);
	list_heap(&spec, type1, type2, NULL);
	/* type */
	type_satisfies_heap(fboundp, &type3);
	GetTypeTable(&type2, Symbol);
	type2and_heap(type2, type3, &type1);
	GetTypeTable(&type2, Asterisk);
	type2_heap(LISPDECL_CONS, type1, type2, &type);
	/* function */
	make_print_dispatch_function(&call,
			CONSTANT_SYSTEM_DISPATCH_CALL,
			p_pprint_dispatch_call);
	/* set */
	fixnum_heap(&priority, -5);
	return set_pprint_dispatch_print_(local, spec, type, call, priority, dispatch);
}

static int build_print_dispatch_defun_(LocalRoot local, addr dispatch)
{
	/* (system::set-pprint-dispatch
	 *   '(cons (eql defun))
	 *   type #<FUNCTION> 0 dispatch)
	 */
	addr spec, type, call, priority;
	addr type1, type2, defun;

	/* spec */
	GetConst(COMMON_EQL, &type1);
	GetConst(COMMON_DEFUN, &defun);
	list_heap(&type2, type1, defun, NULL);
	GetConst(COMMON_CONS, &type1);
	list_heap(&spec, type1, type2, NULL);
	/* type */
	type_eql_heap(defun, &type1);
	GetTypeTable(&type2, Asterisk);
	type2_heap(LISPDECL_CONS, type1, type2, &type);
	/* function */
	make_print_dispatch_function(&call,
			CONSTANT_SYSTEM_DISPATCH_DEFUN,
			p_pprint_dispatch_defun);
	/* set */
	fixnum_heap(&priority, 0);
	return set_pprint_dispatch_print_(local, spec, type, call, priority, dispatch);
}

static int build_print_dispatch_let_type_(LocalRoot local,
		addr dispatch, constindex index)
{
	/* (system::set-pprint-dispatch
	 *   '(cons (eql let))
	 *   type #<FUNCTION> 0 dispatch)
	 */
	addr spec, type, call, priority, symbol;
	addr type1, type2, let;

	/* spec */
	GetConst(COMMON_EQL, &type1);
	GetConstant(index, &let);
	list_heap(&type2, type1, let, NULL);
	GetConst(COMMON_CONS, &type1);
	list_heap(&spec, type1, type2, NULL);
	/* type */
	type_eql_heap(let, &type1);
	GetTypeTable(&type2, Asterisk);
	type2_heap(LISPDECL_CONS, type1, type2, &type);
	/* function */
	GetConst(SYSTEM_DISPATCH_LET, &symbol);
	GetFunctionSymbol(symbol, &call);
	if (call == Unbound) {
		make_print_dispatch_function(&call,
				CONSTANT_SYSTEM_DISPATCH_LET,
				p_pprint_dispatch_let);
	}
	/* set */
	fixnum_heap(&priority, 0);
	return set_pprint_dispatch_print_(local, spec, type, call, priority, dispatch);
}

static int build_print_dispatch_let_(LocalRoot local, addr x)
{
	Return(build_print_dispatch_let_type_(local, x, CONSTANT_COMMON_LET));
	Return(build_print_dispatch_let_type_(local, x, CONSTANT_COMMON_LETA));
	Return(build_print_dispatch_let_type_(local, x, CONSTANT_COMMON_FLET));
	Return(build_print_dispatch_let_type_(local, x, CONSTANT_COMMON_LABELS));
	Return(build_print_dispatch_let_type_(local, x, CONSTANT_COMMON_MACROLET));
	Return(build_print_dispatch_let_type_(local, x, CONSTANT_COMMON_SYMBOL_MACROLET));
	Return(build_print_dispatch_let_type_(local, x, CONSTANT_COMMON_BLOCK));
	Return(build_print_dispatch_let_type_(local, x, CONSTANT_COMMON_TAGBODY));
	Return(build_print_dispatch_let_type_(local, x,
				CONSTANT_COMMON_DESTRUCTURING_BIND));
	Return(build_print_dispatch_let_type_(local, x,
				CONSTANT_COMMON_MULTIPLE_VALUE_BIND));

	return 0;
}

int build_print_dispatch_(void)
{
	LocalRoot local;
	addr dispatch;

	local = Local_Thread;
	pprint_dispatch_heap(&dispatch);
	/* build */
	Return(build_print_dispatch_cons_(local, dispatch));
	Return(build_print_dispatch_vector_(local, dispatch));
	Return(build_print_dispatch_quote_(local, dispatch));
	Return(build_print_dispatch_call_(local, dispatch));
	Return(build_print_dispatch_defun_(local, dispatch));
	Return(build_print_dispatch_let_(local, dispatch));
	/* set variable */
	build_print_dispatch_empty();
	build_print_dispatch_table(dispatch);

	return 0;
}


/************************************************************
 *  print_function.c
 ************************************************************/

/*
 *  pprint-fill
 */
static int pprint_logical_block_type_form_(Execute ptr, enum pprint_newline type)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream_(ptr, stream));
	/* body */
	Return(pprint_exit_common_(ptr, stream));
	for (;;) {
		Return(pprint_pop_common_(ptr, stream, &pos));
		Return(write_print_(ptr, stream, pos));
		Return(pprint_exit_common_(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, type, stream));
	}

	return 0;
}

static int pprint_logical_block_type_call_(Execute ptr, pointer type, addr stream)
{
	addr gensym;

	Return(gensym_pretty_stream_(stream, &gensym));
	(void)catch_clang_(ptr, type, gensym, stream);
	return close_pretty_stream_unwind_protect_(ptr, stream);
}

static int pprint_logical_block_type_(Execute ptr, pointer type)
{
	addr stream, control;

	/* stream */
	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	/* unwind-protect */
	push_control(ptr, &control);
	(void)pprint_logical_block_type_call_(ptr, type, stream);
	return pop_control_(ptr, control);
}

static int pprint_type_print_(Execute ptr,
		addr stream, addr list, int colon, pointer type)
{
	addr prefix, suffix, lambda;

	/* make-pprint-stream */
	prefix = suffix = Nil;
	if (colon) {
		strvect_char_heap(&prefix, "(");
		strvect_char_heap(&suffix, ")");
	}
	Return(open_pretty_stream_(ptr, &stream, stream, list, prefix, Nil, suffix));

	/* function */
	compiled_heap(&lambda, Nil);
	setcompiled_empty(lambda, type);
	SetDataFunction(lambda, stream);

	/* call */
	gchold_pushva_local(ptr->local, stream, lambda, NULL);
	return call_pretty_stream(ptr, stream, lambda);
}

static int pprint_list_common_(Execute ptr, addr stream, addr list, pointer type)
{
	return pprint_type_print_(ptr, stream, list, 1, type);
}

static int pprint_logical_block_fill_form_(Execute ptr)
{
	return pprint_logical_block_type_form_(ptr, pprint_newline_fill);
}

static int pprint_logical_block_fill_(Execute ptr)
{
	return pprint_logical_block_type_(ptr, p_pprint_logical_block_fill_form);
}

int pprint_fill_print_(Execute ptr, addr stream, addr list, int colon)
{
	/* (defun pprint-fill (*standard-output* list &optional (colon t) atsign)
	 *   (declare (ignore atsign))
	 *   (pprint-logical-block
	 *     (nil list :prefix (if colon "(" "") :suffix (if colon ")" ""))
	 *     (pprint-exit-if-list-exhausted)
	 *     (loop (write (pprint-pop))
	 *           (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space)
	 *           (pprint-newline :fill))))
	 */
	return pprint_type_print_(ptr, stream, list, colon, p_pprint_logical_block_fill);
}


/*
 *  pprint-linaer
 */
static int pprint_logical_block_linear_form_(Execute ptr)
{
	return pprint_logical_block_type_form_(ptr, pprint_newline_linear);
}

static int pprint_logical_block_linear_(Execute ptr)
{
	return pprint_logical_block_type_(ptr, p_pprint_logical_block_linear_form);
}

int pprint_linear_print_(Execute ptr, addr stream, addr list, int colon)
{
	/* (defun pprint-linear (*standard-output* list &optional (colon t) atsign)
	 *   (declare (ignore atsign))
	 *   (pprint-logical-block
	 *     (nil list :prefix (if colon "(" "") :suffix (if colon ")" ""))
	 *     (pprint-exit-if-list-exhausted)
	 *     (loop (write (pprint-pop))
	 *           (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space)
	 *           (pprint-newline :linear))))
	 */
	return pprint_type_print_(ptr, stream, list, colon, p_pprint_logical_block_linear);
}


/*
 *  pprint-tabular
 */
static int pprint_logical_block_tabular_form_(Execute ptr)
{
	addr cons, stream, pos;
	fixnum colinc;

	getdata_control(ptr, &cons);
	GetCons(cons, &stream, &pos);
	Check(! pretty_stream_p(stream), "type error");
	GetFixnum(pos, &colinc);
	Return(check_pretty_stream_(ptr, stream));
	/* body */
	Return(pprint_exit_common_(ptr, stream));
	for (;;) {
		Return(pprint_pop_common_(ptr, stream, &pos));
		Return(write_print_(ptr, stream, pos));
		Return(pprint_exit_common_(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_tab_section_relative_(ptr, stream, 0, colinc));
		Return(pprint_newline_print_(ptr, pprint_newline_fill, stream));
	}

	return 0;
}

static int pprint_logical_block_tabular_call_(Execute ptr, addr stream, addr cons)
{
	addr gensym;

	Return(gensym_pretty_stream_(stream, &gensym));
	(void)catch_clang_(ptr, p_pprint_logical_block_tabular_form, gensym, cons);
	return close_pretty_stream_unwind_protect_(ptr, stream);
}

static int pprint_logical_block_tabular_(Execute ptr)
{
	addr cons, stream, control;

	/* stream */
	getdata_control(ptr, &cons);
	GetCar(cons, &stream);
	Check(! pretty_stream_p(stream), "type error");
	/* unwind-protect */
	push_control(ptr, &control);
	(void)pprint_logical_block_tabular_call_(ptr, stream, cons);
	return pop_control_(ptr, control);
}

int pprint_tabular_print_(Execute ptr,
		addr stream, addr list, int colon, fixnum tabsize)
{
	/* (defun pprint-tabular
	 *   (*standard-output* list &optional (colon t) atsign (tabsize 16))
	 *   (declare (ignore atsign))
	 *   (pprint-logical-block
	 *     (nil list :prefix (if colon "(" "") :suffix (if colon ")" ""))
	 *     (pprint-exit-if-list-exhausted)
	 *     (loop (write (pprint-pop))
	 *           (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space)
	 *           (pprint-tab :section-relative 0 tabsize)
	 *           (pprint-newline :fill))))
	 */
	addr prefix, suffix, lambda, cons;

	/* make-pprint-stream */
	prefix = suffix = Nil;
	if (colon) {
		strvect_char_heap(&prefix, "(");
		strvect_char_heap(&suffix, ")");
	}
	Return(open_pretty_stream_(ptr, &stream, stream, list, prefix, Nil, suffix));

	/* closure */
	fixnum_heap(&cons, tabsize);
	cons_heap(&cons, stream, cons);

	/* function */
	compiled_heap(&lambda, Nil);
	setcompiled_empty(lambda, p_pprint_logical_block_tabular);
	SetDataFunction(lambda, cons);

	/* call */
	gchold_pushva_local(ptr->local, stream, lambda, NULL);
	return call_pretty_stream(ptr, stream, lambda);
}


/*
 *  dispatch-vector
 */
static int pprint_dispatch_vector2_(Execute ptr)
{
	addr cons, stream, pos, vector;
	size_t size, i;

	getdata_control(ptr, &cons);
	GetCons(cons, &stream, &vector);
	Check(! pretty_stream_p(stream), "type error");
	Check(! vectorp_sequence_debug(vector), "type error");
	Return(check_pretty_stream_(ptr, stream));
	/* body */
	Return(length_sequence_(vector, 1, &size));
	if (size == 0)
		return 0;
	i = 0;
	for (;;) {
		Return(pprint_pop_common_(ptr, stream, &pos));
		Return(getelt_sequence_(NULL, vector, i, &pos));
		Return(write_print_(ptr, stream, pos));
		i++;
		if (size <= i)
			break;
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_fill, stream));
	}

	return 0;
}

static int pprint_dispatch_vector1_call_(Execute ptr, addr stream, addr cons)
{
	addr gensym;

	Return(gensym_pretty_stream_(stream, &gensym));
	(void)catch_clang_(ptr, p_pprint_dispatch_vector2, gensym, cons);
	return close_pretty_stream_unwind_protect_(ptr, stream);
}

static int pprint_dispatch_vector1_(Execute ptr)
{
	addr cons, stream, control;

	/* stream */
	getdata_control(ptr, &cons);
	GetCar(cons, &stream);
	Check(! pretty_stream_p(stream), "type error");
	/* unwind-protect */
	push_control(ptr, &control);
	(void)pprint_dispatch_vector1_call_(ptr, stream, cons);
	return pop_control_(ptr, control);
}

static int pprint_dispatch_vector_(Execute ptr, addr stream, addr pos)
{
	/* (defun dispatch-vector (*standard-output* v)
	 *   (pprint-logical-block (nil nil :prefix "#(" :suffix ")")
	 *     (let ((end (length v)) (i 0))
	 *       (when (plusp end)
	 *         (loop (pprint-pop)
	 *               (write (aref v i))
	 *               (if (= (incf i) end) (return nil))
	 *               (write-char #\Space)
	 *               (pprint-newline :fill))))))
	 */
	addr prefix, suffix, lambda, cons;

	/* make-pprint-stream */
	strvect_char_heap(&prefix, "#(");
	strvect_char_heap(&suffix, ")");
	Return(open_pretty_stream_(ptr, &stream, stream, Nil, prefix, Nil, suffix));

	/* closure */
	cons_heap(&cons, stream, pos);

	/* function */
	compiled_heap(&lambda, Nil);
	setcompiled_empty(lambda, p_pprint_dispatch_vector1);
	SetDataFunction(lambda, cons);

	/* call */
	gchold_pushva_local(ptr->local, stream, lambda, NULL);
	return call_pretty_stream(ptr, stream, lambda);
}


/*
 *  dispatch-quote
 */
static int pprint_dispatch_quote2_(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream_(ptr, stream));
	/* body */
	Return(write_char_stream_(stream, '\''));
	Return(pprint_pop_common_(ptr, stream, &pos));  /* quote */
	Return(pprint_pop_common_(ptr, stream, &pos));
	Return(write_print_(ptr, stream, pos));

	return 0;
}

static int pprint_dispatch_quote1_(Execute ptr)
{
	return pprint_logical_block_type_(ptr, p_pprint_dispatch_quote2);
}

static int pprint_dispatch_quote_(Execute ptr, addr stream, addr list)
{
	/* (defun dispatch-quote (*standard-outupt* list)
	 *   (pprint-logical-block (nil list)
	 *     ;; name
	 *     (write-char #\')
	 *     (pprint-pop) ;; quote
	 *     (write (pprint-pop))))
	 */
	return pprint_type_print_(ptr, stream, list, 0, p_pprint_dispatch_quote1);
}


/*
 *  dispatch-call
 */
static int pprint_dispatch_call2_(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream_(ptr, stream));
	/* body */
	Return(pprint_pop_common_(ptr, stream, &pos));
	Return(write_print_(ptr, stream, pos));
	Return(pprint_exit_common_(ptr, stream));
	Return(write_char_stream_(stream, ' '));
	Return(pprint_newline_print_(ptr, pprint_newline_miser, stream));
	Return(pprint_indent_print_(ptr, 0, 0, stream));
	for (;;) {
		Return(pprint_pop_common_(ptr, stream, &pos));
		Return(write_print_(ptr, stream, pos));
		Return(pprint_exit_common_(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_linear, stream));
	}

	return 0;
}

static int pprint_dispatch_call1_(Execute ptr)
{
	return pprint_logical_block_type_(ptr, p_pprint_dispatch_call2);
}

static int pprint_dispatch_call_(Execute ptr, addr stream, addr list)
{
	/* (defun dispatch-call (*standard-outupt* list)
	 *   (pprint-logical-block (nil list :prefix "(" :suffix ")")
	 *     ;; name
	 *     (write (pprint-pop))
	 *     (pprint-exit-if-list-exhausted)
	 *     (write-char #\Space)
	 *     (pprint-newline :miser)
	 *     (pprint-indent :current 0)
	 *     ;; args
	 *     (loop (write (pprint-pop))
	 *           (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space)
	 *           (pprint-newline :linear))))
	 */
	return pprint_list_common_(ptr, stream, list, p_pprint_dispatch_call1);
}


/*
 *  dispatch-defun
 */
static int pprint_dispatch_defun6_(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream_(ptr, stream));
	/* body */
	Return(pprint_exit_common_(ptr, stream));
	for (;;) {
		Return(pprint_pop_common_(ptr, stream, &pos));
		Return(write_print_(ptr, stream, pos));
		Return(pprint_exit_common_(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_linear, stream));
	}

	return 0;
}

static int pprint_dispatch_defun5_(Execute ptr)
{
	return pprint_logical_block_type_(ptr, p_pprint_dispatch_defun6);
}

static int pprint_dispatch_defun4_(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream_(ptr, stream));
	/* body */
	Return(pprint_exit_common_(ptr, stream));
	for (;;) {
		Return(pprint_pop_common_(ptr, stream, &pos));
		Return(pprint_list_common_(ptr, stream, pos, p_pprint_dispatch_defun5));
		Return(pprint_exit_common_(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_fill, stream));
	}

	return 0;
}

static int pprint_dispatch_defun3_(Execute ptr)
{
	return pprint_logical_block_type_(ptr, p_pprint_dispatch_defun4);
}

static int pprint_dispatch_defun2_(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream_(ptr, stream));
	/* defun */
	Return(pprint_pop_common_(ptr, stream, &pos));
	Return(write_print_(ptr, stream, pos));
	Return(pprint_exit_common_(ptr, stream));
	Return(write_char_stream_(stream, ' '));
	Return(pprint_newline_print_(ptr, pprint_newline_miser, stream));
	/* name */
	Return(pprint_pop_common_(ptr, stream, &pos));
	Return(write_print_(ptr, stream, pos));
	Return(pprint_exit_common_(ptr, stream));
	Return(write_char_stream_(stream, ' '));
	Return(pprint_newline_print_(ptr, pprint_newline_miser, stream));
	/* args */
	Return(pprint_pop_common_(ptr, stream, &pos));
	Return(pprint_list_common_(ptr, stream, pos, p_pprint_dispatch_defun3));
	/* body */
	Return(pprint_indent_print_(ptr, 1, 1, stream));
	for (;;) {
		Return(pprint_exit_common_(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_linear, stream));
		Return(pprint_pop_common_(ptr, stream, &pos));
		Return(write_print_(ptr, stream, pos));
	}

	return 0;
}

static int pprint_dispatch_defun1_(Execute ptr)
{
	return pprint_logical_block_type_(ptr, p_pprint_dispatch_defun2);
}

static int pprint_dispatch_defun_(Execute ptr, addr stream, addr list)
{
	/* (defun dispatch-defun (*standard-output* list)
	 *   (pprint-logical-block (nil list :prefix "(" :suffix ")")
	 *     ;; defun
	 *     (write (pprint-pop))
	 *     (pprint-exit-if-list-exhausted)
	 *     (write-char #\Space)
	 *     (pprint-newline :miser)
	 *     ;; name
	 *     (write (pprint-pop))
	 *     (pprint-exit-if-list-exhausted)
	 *     (write-char #\Space)
	 *     (pprint-newline :miser)
	 *     ;; args
	 *     (pprint-logical-block (nil (pprint-pop) :prefix "(" :suffix ")")
	 *       (pprint-exit-if-list-exhausted)
	 *       (loop (pprint-logical-block (nil (pprint-pop) :prefix "(" :suffix ")")
	 *               (pprint-exit-if-list-exhausted)
	 *               (loop (write (pprint-pop))
	 *                     (pprint-exit-if-list-exhausted)
	 *                     (write-char #\Space)
	 *                     (pprint-newline :linear)))
	 *             (pprint-exit-if-list-exhausted)
	 *             (write-char #\Space)
	 *             (pprint-newline :fill)))
	 *     ;; body
	 *     (pprint-indent :block 1)
	 *     (loop (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space)
	 *           (pprint-newline :linear)
	 *           (write (pprint-pop)))))
	 */
	return pprint_list_common_(ptr, stream, list, p_pprint_dispatch_defun1);
}


/*
 *  dispatch-let
 */
static int pprint_dispatch_let2_(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream_(ptr, stream));
	/* let */
	Return(pprint_pop_common_(ptr, stream, &pos));
	Return(write_print_(ptr, stream, pos));
	Return(pprint_exit_common_(ptr, stream));
	Return(write_char_stream_(stream, ' '));
	Return(pprint_newline_print_(ptr, pprint_newline_miser, stream));
	/* args */
	Return(pprint_pop_common_(ptr, stream, &pos));
	Return(pprint_list_common_(ptr, stream, pos, p_pprint_dispatch_defun3));
	/* body */
	Return(pprint_indent_print_(ptr, 1, 1, stream));
	for (;;) {
		Return(pprint_exit_common_(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_linear, stream));
		Return(pprint_pop_common_(ptr, stream, &pos));
		Return(write_print_(ptr, stream, pos));
	}

	return 0;
}

static int pprint_dispatch_let1_(Execute ptr)
{
	return pprint_logical_block_type_(ptr, p_pprint_dispatch_let2);
}

static int pprint_dispatch_let_(Execute ptr, addr stream, addr list)
{
	/* (defun dispatch-let (*standard-output* list)
	 *   (pprint-logical-block (nil list :prefix "(" :suffix ")")
	 *     ;; let
	 *     (write (pprint-pop))
	 *     (pprint-exit-if-list-exhausted)
	 *     (write-char #\Space)
	 *     (pprint-newline :miser)
	 *     ;; args
	 *     (pprint-logical-block (nil (pprint-pop) :prefix "(" :suffix ")")
	 *       (pprint-exit-if-list-exhausted)
	 *       (loop (pprint-logical-block (nil (pprint-pop) :prefix "(" :suffix ")")
	 *               (pprint-exit-if-list-exhausted)
	 *               (loop (write (pprint-pop))
	 *                     (pprint-exit-if-list-exhausted)
	 *                     (write-char #\Space)
	 *                     (pprint-newline :linear)))
	 *             (pprint-exit-if-list-exhausted)
	 *             (write-char #\Space)
	 *             (pprint-newline :fill)))
	 *     ;; body
	 *     (pprint-indent :block 1)
	 *     (loop (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space)
	 *           (pprint-newline :linear)
	 *           (write (pprint-pop)))))
	 */
	return pprint_list_common_(ptr, stream, list, p_pprint_dispatch_let1);
}


/*
 *  initialize
 */
void init_print_function(void)
{
	/* pprint-fill */
	SetPointerType_(empty, pprint_logical_block_fill_form);
	SetPointerType_(empty, pprint_logical_block_fill);
	/* pprint-linear */
	SetPointerType_(empty, pprint_logical_block_linear_form);
	SetPointerType_(empty, pprint_logical_block_linear);
	/* pprint-tabular */
	SetPointerType_(empty, pprint_logical_block_tabular_form);
	SetPointerType_(empty, pprint_logical_block_tabular);
	/* dispatch-vector */
	SetPointerType_(empty, pprint_dispatch_vector2);
	SetPointerType_(empty, pprint_dispatch_vector1);
	SetPointerType_(var2, pprint_dispatch_vector);
	/* dispatch-quote */
	SetPointerType_(empty, pprint_dispatch_quote2);
	SetPointerType_(empty, pprint_dispatch_quote1);
	SetPointerType_(var2, pprint_dispatch_quote);
	/* dispatch-call */
	SetPointerType_(empty, pprint_dispatch_call2);
	SetPointerType_(empty, pprint_dispatch_call1);
	SetPointerType_(var2, pprint_dispatch_call);
	/* defun */
	SetPointerType_(empty, pprint_dispatch_defun6);
	SetPointerType_(empty, pprint_dispatch_defun5);
	SetPointerType_(empty, pprint_dispatch_defun4);
	SetPointerType_(empty, pprint_dispatch_defun3);
	SetPointerType_(empty, pprint_dispatch_defun2);
	SetPointerType_(empty, pprint_dispatch_defun1);
	SetPointerType_(var2, pprint_dispatch_defun);
	/* let */
	SetPointerType_(empty, pprint_dispatch_let2);
	SetPointerType_(empty, pprint_dispatch_let1);
	SetPointerType_(var2, pprint_dispatch_let);
}


/************************************************************
 *  print_object.c
 ************************************************************/

/*
 *  t
 */
static int method_print_clos_name_(Execute ptr, addr stream, addr pos)
{
	if (pos == Unbound)
		goto unbound;
	if (! closp(pos))
		goto unbound;
	stdget_class_name_check(pos, &pos);
	if (pos == Unbound)
		goto unbound;
	return princ_print_(ptr, stream, pos);
unbound:
	return print_ascii_stream_(stream, "Unbound");
}

static int method_print_clos_class_of_(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetClassOfClos(pos, &pos);
	return method_print_clos_name_(ptr, stream, pos);
}

static int method_print_object_t_body_(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_CLOS);
	return method_print_clos_class_of_(ptr, stream, pos);
}

static int method_print_object_t_(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	Return(print_unreadable_object_(ptr,
				stream, pos, 0, 1, method_print_object_t_body_));
	setresult_control(ptr, pos);
	return 0;
}


/*
 *  class
 */
static int method_print_object_class_(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	/* #<CLASS-OF CLASS-NAME> */
	Return(print_ascii_stream_(stream, "#<"));
	Return(method_print_clos_class_of_(ptr, stream, pos));
	Return(write_char_stream_(stream, ' '));
	Return(method_print_clos_name_(ptr, stream, pos));
	Return(write_char_stream_(stream, '>'));
	/* result */
	setresult_control(ptr, pos);

	return 0;
}


/*
 *  structure-object
 */
static int write_structure_(Execute ptr, addr stream, addr pos)
{
	int check;
	addr x, y, z;
	size_t size, i;

	/* class name */
	GetClassOfClos(pos, &x);
	if (x == Unbound)
		return print_ascii_stream_(stream, "#S(INVALID)");
	Return(structure_class_p_(x, &check));
	if (! check)
		return print_ascii_stream_(stream, "#S(INVALID)");
	Return(stdget_structure_name_(x, &x));
	Return(print_ascii_stream_(stream, "#S("));
	Return(write_print_(ptr, stream, x));
	/* slot */
	GetSlotClos(pos, &x);
	GetValueClos(pos, &y);
	LenSlotVector(x, &size);
	for (i = 0; i < size; i++) {
		Return(write_char_stream_(stream, ' '));
		GetSlotVector(x, i, &z);
		GetNameSlot(z, &z);
		GetNameSymbol(z, &z);
		Return(write_char_stream_(stream, ':'));
		Return(princ_print_(ptr, stream, z));
		Return(write_char_stream_(stream, ' '));
		GetClosValue(y, i, &z);
		if (z == Unbound) {
			Return(print_ascii_stream_(stream, "#<UNBOUND>"));
		}
		else {
			Return(write_print_(ptr, stream, z));
		}
	}
	Return(write_char_stream_(stream, ')'));

	return 0;
}

static int method_print_object_structure_object_(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	Return(print_structure_(ptr, stream, pos));
	setresult_control(ptr, pos);
	return 0;
}

int print_structure_(Execute ptr, addr stream, addr pos)
{
	return write_structure_(ptr, stream, pos);
}


/*
 *  generic-function
 */
static int method_print_object_generic_function_(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	addr class_of, name;

	Return(clos_class_of_(pos, &class_of));
	Return(stdget_class_name_(class_of, &class_of));
	Return(stdget_generic_name_(pos, &name));
	/* #<CLASS-OF CLASS-NAME> */
	Return(print_ascii_stream_(stream, "#<"));
	Return(princ_print_(ptr, stream, class_of));
	Return(write_char_stream_(stream, ' '));
	Return(princ_print_(ptr, stream, name));
	Return(write_char_stream_(stream, '>'));
	/* result */
	setresult_control(ptr, pos);

	return 0;
}


/*
 *  simple-condition
 */
static int method_print_object_simple_condition_call_(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	addr control, call, list;
	LocalHold hold;

	push_control(ptr, &control);
	hold = LocalHold_array(ptr, 1);
	/* flet-next-method */
	GetConst(CLOSNAME_FLET_NEXT_METHOD, &call);
	Return(getfunction_global_(call, &call));
	/* funcall */
	list_heap(&list, pos, stream, NULL);
	localhold_set(hold, 0, list);
	(void)funcall_control_(ptr, call, method, next, list, Nil, NULL);
	return pop_control_(ptr, control);
}

static int method_print_object_simple_condition_format_(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	addr control, arguments;

	Return(ClosCheckConst_(pos, CLOSNAME_FORMAT_CONTROL, &control));
	Return(ClosCheckConst_(pos, CLOSNAME_FORMAT_ARGUMENTS, &arguments));
	return format_stream_lisp_(ptr, stream, control, arguments);
}

static int method_print_object_simple_condition_(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	/*  (defmethod print-object ((inst simple-condition) stream)
	 *    (if *print-escape*
	 *      (flet-next-method method next (list inst stream) nil)
	 *      (format stream (simple-condition-format-control inst)
	 *                     (simple-condition-format-arguments inst))))
	 */
	addr escape;

	/* *print-escape* */
	GetConst(SPECIAL_PRINT_ESCAPE, &escape);
	Return(getspecialcheck_local_(ptr, escape, &escape));
	if (escape != Nil) {
		/* (call-next-method) */
		return method_print_object_simple_condition_call_(ptr,
				method, next, pos, stream);
	}
	/* format */
	Return(method_print_object_simple_condition_format_(ptr,
				method, next, pos, stream));
	setresult_control(ptr, pos);

	return 0;
}


/*
 *  defmethod
 */
static void method_type_print_object(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Stream);
	typeargs_var2(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static int defmethod_print_object_(Execute ptr, addr name, addr gen,
		pointer p, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p);
	method_type_print_object(&type);
	settype_function(call, type);
	/* method */
	GetConstant(index, &pos);
	mop_argument_method_print_object(&pos, pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  defgeneric
 */
void init_print_object(void)
{
	SetPointerType_(var4, method_print_object_t);
	SetPointerType_(var4, method_print_object_class);
	SetPointerType_(var4, method_print_object_structure_object);
	SetPointerType_(var4, method_print_object_generic_function);
	SetPointerType_(var4, method_print_object_simple_condition);
}

#define DefMethod_PrintObject1(ptr, name, gen, p, c) { \
	Return(defmethod_print_object_((ptr), (name), (gen), \
				p_method_print_object_##p, CONSTANT_CLOS_##c)); \
}
#define DefMethod_PrintObject2(ptr, name, gen, p, c) { \
	Return(defmethod_print_object_((ptr), (name), (gen), \
				p_method_print_object_##p, CONSTANT_CONDITION_##c)); \
}
static int build_print_object_method_(Execute ptr, addr name, addr gen)
{
	DefMethod_PrintObject1(ptr, name, gen, t, T);
	DefMethod_PrintObject1(ptr, name, gen, class, CLASS);
	DefMethod_PrintObject1(ptr, name, gen, structure_object, STRUCTURE_OBJECT);
	DefMethod_PrintObject1(ptr, name, gen, generic_function, GENERIC_FUNCTION);
	DefMethod_PrintObject2(ptr, name, gen, simple_condition, SIMPLE_CONDITION);

	return 0;
}

int build_print_object_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_PRINT_OBJECT, &symbol);
	mop_argument_generic_var2(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(build_print_object_method_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/************************************************************
 *  print_pretty.c
 ************************************************************/

enum print_pretty {
	print_pretty_newline_linear,
	print_pretty_newline_fill,
	print_pretty_newline_miser,
	print_pretty_newline_mandatory,
	print_pretty_newline_terpri,
	print_pretty_indent_block,
	print_pretty_indent_current,
	print_pretty_tabular_line,
	print_pretty_tabular_section,
	print_pretty_tabular_liner,
	print_pretty_tabular_sectionr
};

struct print_pretty_struct {
	unsigned colon : 1;
	enum print_pretty type;
	fixnum value, colinc;
};

static struct print_pretty_struct *struct_print_pretty(addr pos)
{
	CheckType(pos, LISPSYSTEM_PRINT_PRETTY);
	return (struct print_pretty_struct *)PtrBodySS(pos);
}

static void print_pretty_heap(addr *ret, enum print_pretty type)
{
	addr pos;
	struct print_pretty_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_PRINT_PRETTY,
			1UL, sizeoft(struct print_pretty_struct));
	str = struct_print_pretty(pos);
	str->type = type;
	*ret = pos;
}

static int print_pretty_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_PRINT_PRETTY;
}

static int pretty_print_newline_p(addr pos)
{
	if (! print_pretty_p(pos))
		return 0;

	switch (struct_print_pretty(pos)->type) {
		case print_pretty_newline_linear:
		case print_pretty_newline_fill:
		case print_pretty_newline_miser:
		case print_pretty_newline_mandatory:
		case print_pretty_newline_terpri:
			return 1;

		default:
			return 0;
	}
}

static int pretty_print_linear_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_newline_linear;
}

static int pretty_print_miser_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_newline_miser;
}

static int pretty_print_fill_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_newline_fill;
}

static int pretty_print_mandatory_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_newline_mandatory;
}

static int pretty_print_terpri_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_newline_terpri;
}

static int pretty_print_force_p(addr pos)
{
	struct print_pretty_struct *str;

	if (! print_pretty_p(pos))
		return 0;
	str = struct_print_pretty(pos);
	return str->type == print_pretty_newline_mandatory
		|| str->type == print_pretty_newline_terpri;
}

static int pretty_print_indent_block_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_indent_block;
}

static int pretty_print_indent_current_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_indent_current;
}

static int pretty_print_indent_p(addr pos)
{
	struct print_pretty_struct *str;

	if (! print_pretty_p(pos))
		return 0;
	str = struct_print_pretty(pos);
	return str->type == print_pretty_indent_block
		|| str->type == print_pretty_indent_current;
}

static int pretty_print_tabular_line_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_tabular_line;
}

static int pretty_print_tabular_section_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_tabular_section;
}

static int pretty_print_tabular_liner_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_tabular_liner;
}

static int pretty_print_tabular_sectionr_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_tabular_sectionr;
}

static int pretty_print_tabular_p(addr pos)
{
	struct print_pretty_struct *str;

	if (! print_pretty_p(pos))
		return 0;
	str = struct_print_pretty(pos);
	return str->type == print_pretty_tabular_line
		|| str->type == print_pretty_tabular_section
		|| str->type == print_pretty_tabular_liner
		|| str->type == print_pretty_tabular_sectionr;
}

static void fixnum_pretty_heap(addr *ret, enum print_pretty type, fixnum a)
{
	addr pos;
	struct print_pretty_struct *str;

	print_pretty_heap(&pos, type);
	str = struct_print_pretty(pos);
	str->value = a;
	*ret = pos;
}

static void size2_pretty_heap(addr *ret, enum print_pretty type, fixnum a, fixnum b)
{
	addr pos;
	struct print_pretty_struct *str;

	print_pretty_heap(&pos, type);
	str = struct_print_pretty(pos);
	str->value = a;
	str->colinc = b;
	*ret = pos;
}


/*
 *  common
 */
int expand_pprint_logical_block_common_(addr *ret, addr symbol, addr pos,
		addr prefix, addr perline, addr suffix, addr decl, addr body)
{
	/* `(let ((,symbol (system::make-pprint-stream
	 *                   ,symbol ,pos ,prefix ,perline ,suffix)))
	 *    ,@decl
	 *    (system::pprint-pretty
	 *      ,symbol
	 *      (lambda ()
	 *        (unwind-protect
	 *          (catch (system::pprint-gensym ,symbol)
	 *            (macrolet
	 *              ((pprint-exit-if-list-exhausted ()
	 *                 (list 'system::pprint-exit ',symbol))
	 *               (pprint-pop ()
	 *                 (list 'system::pprint-pop ',symbol)))
	 *              (system::pprint-check ,symbol)
	 *              ,@body))
	 *          (system::pprint-close ,symbol)))))
	 */
	addr let, flet, lambda, unwind, catchs, macrolet, list, quote;
	addr make, pretty, gensym, exit, pop, check, close, ppexit, pppop;
	addr x;

	/* (check-type symbol symbol) */
	if (! symbolp(symbol))
		return TypeError_(symbol, SYMBOL);

	/* symbol */
	if (symbol == T) {
		/* *terminal-io* */
		GetConst(SPECIAL_TERMINAL_IO, &symbol);
	}
	else if (symbol == Nil) {
		/* *standard-output* */
		GetConst(SPECIAL_STANDARD_OUTPUT, &symbol);
	}

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_FLET, &flet);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_CATCH, &catchs);
	GetConst(COMMON_MACROLET, &macrolet);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_PPRINT_EXIT_IF_LIST_EXHAUSTED, &ppexit);
	GetConst(COMMON_PPRINT_POP, &pppop);
	GetConst(SYSTEM_MAKE_PPRINT_STREAM, &make);
	GetConst(SYSTEM_PPRINT_PRETTY, &pretty);
	GetConst(SYSTEM_PPRINT_GENSYM, &gensym);
	GetConst(SYSTEM_PPRINT_EXIT, &exit);
	GetConst(SYSTEM_PPRINT_POP, &pop);
	GetConst(SYSTEM_PPRINT_CHECK, &check);
	GetConst(SYSTEM_PPRINT_CLOSE, &close);
	/* body */
	list_heap(&close, close, symbol, NULL);
	list_heap(&x, quote, symbol, NULL);
	list_heap(&exit, quote, exit, NULL);
	list_heap(&exit, list, exit, x, NULL);
	list_heap(&ppexit, ppexit, Nil, exit, NULL);
	list_heap(&pop, quote, pop, NULL);
	list_heap(&pop, list, pop, x, NULL);
	list_heap(&pppop, pppop, Nil, pop, NULL);
	list_heap(&x, ppexit, pppop, NULL);
	list_heap(&check, check, symbol, NULL);
	lista_heap(&macrolet, macrolet, x, check, body, NULL);
	list_heap(&gensym, gensym, symbol, NULL);
	list_heap(&catchs, catchs, gensym, macrolet, NULL);
	list_heap(&unwind, unwind, catchs, close, NULL);
	list_heap(&lambda, lambda, Nil, unwind, NULL);
	list_heap(&pretty, pretty, symbol, lambda, NULL);
	list_heap(&x, make, symbol, pos, prefix, perline, suffix, NULL);
	list_heap(&x, symbol, x, NULL);
	list_heap(&x, x, NULL);
	/* let */
	conscar_heap(&let, let);
	cons_heap(&let, x, let);
	while (decl != Nil) {
		Return_getcons(decl, &x, &decl);
		cons_heap(&let, x, let);
	}
	cons_heap(&let, pretty, let);
	nreverse(ret, let);

	return 0;
}

int pprint_throw_(Execute ptr, addr stream)
{
	Return(gensym_pretty_stream_(stream, &stream));
	return throw_control_(ptr, stream);
}

int pprint_exit_common_(Execute ptr, addr stream)
{
	addr pos;

	Return(root_pretty_stream_(stream, &pos));
	if (pos == Nil)
		return pprint_throw_(ptr, stream);

	return 0;
}

static int pprint_pop_atom_(Execute ptr, addr stream)
{
	int check;
	addr pos;

	Return(first_pretty_stream_(stream, &check));
	if (! check) {
		Return(print_ascii_stream_(stream, ". "));
	}
	Return(pop_pretty_stream_(stream, &pos, &check));
	Return(write_print_(ptr, stream, pos));

	return pprint_throw_(ptr, stream);
}

static int pprint_length_check_(Execute ptr, addr stream, int *ret)
{
	int check;
	size_t x, y;

	Return(length_print_(ptr, &x, &check));
	if (! check)
		return Result(ret, 0);
	Return(length_pretty_stream_(stream, &y));

	return Result(ret, x <= y);
}

int pprint_pop_common_(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr pos;

	Return(root_pretty_stream_(stream, &pos));
	/* atom */
	if (! listp(pos))
		return pprint_pop_atom_(ptr, stream);
	/* length */
	Return(pprint_length_check_(ptr, stream, &check));
	if (check) {
		Return(print_ascii_stream_(stream, "..."));
		return pprint_throw_(ptr, stream);
	}
	/* list */
	if (pos == Nil)
		return Result(ret, Nil);
	/* circle */
	Return(circle_print_(ptr, &check));
	if (check) {
		Return(first_pretty_stream_(stream, &check));
		if (! check) {
			Return(pprint_pop_circle_(ptr, stream, pos, &check));
			if (check)
				return pprint_throw_(ptr, stream);
		}
	}
	/* cons */
	return pop_pretty_stream_(stream, ret, &check);
}

int check_pretty_stream_(Execute ptr, addr stream)
{
	int check;
	addr root;
	size_t level, depth;

	/* *print-level* */
	Return(level_print_(ptr, &level, &check));
	if (check) {
		getdepth_print_write(ptr, &depth);
		if (level <= depth) {
			setlistp_pretty_stream(stream, 0);
			Return(write_char_stream_(stream, '#'));
			return pprint_throw_(ptr, stream);
		}
	}

	/* atom */
	Return(root_pretty_stream_(stream, &root));
	if (! listp(root)) {
		Return(write_print_(ptr, stream, root));
		return pprint_throw_(ptr, stream);
	}

	/* increment depth */
	setdepth_pretty_stream(ptr, stream, 1);

	/* circle */
	Return(circle_print_(ptr, &check));
	if (check) {
		if (consp(root)) {
			Return(pprint_check_circle_(ptr, root, &root, &check));
			if (check) {
				setlistp_pretty_stream(stream, 0);
				Return(print_string_stream_(stream, root));
				return pprint_throw_(ptr, stream);
			}
			if (root != Nil)
				setsharp_pretty_stream(stream, root);
		}
		return 0;
	}

	return 0;
}

static int pretty_common_p_(Execute ptr, addr stream, int *ret)
{
	int check;

	Return(pretty_print_(ptr, &check));
	if (! check)
		return Result(ret, 0);

	return Result(ret, pretty_stream_p(stream));
}

int pprint_indent_print_(Execute ptr, int block_p, fixnum n, addr stream)
{
	int check;
	enum print_pretty type;
	addr pos;

	Return(pretty_common_p_(ptr, stream, &check));
	if (! check)
		return 0;
	type = block_p? print_pretty_indent_block: print_pretty_indent_current;
	fixnum_pretty_heap(&pos, type, n);
	return push_pretty_stream_(stream, pos);
}

int pprint_newline_print_(Execute ptr, enum pprint_newline kind, addr stream)
{
	int check;
	addr pos;

	Return(pretty_common_p_(ptr, stream, &check));
	if (! check)
		return 0;
	switch (kind) {
		case pprint_newline_linear:
			print_pretty_heap(&pos, print_pretty_newline_linear);
			break;

		case pprint_newline_fill:
			print_pretty_heap(&pos, print_pretty_newline_fill);
			break;

		case pprint_newline_miser:
			print_pretty_heap(&pos, print_pretty_newline_miser);
			break;

		case pprint_newline_mandatory:
			print_pretty_heap(&pos, print_pretty_newline_mandatory);
			break;

		default:
			return fmte_("Invalid newline type.", NULL);
	}

	return push_pretty_stream_(stream, pos);
}

int pprint_newline_terpri_(addr stream)
{
	addr pos;

	print_pretty_heap(&pos, print_pretty_newline_terpri);
	return push_pretty_stream_(stream, pos);
}

int pprint_tab_print_(Execute ptr,
		addr stream, enum pprint_tabular kind, fixnum a, fixnum b)
{
	int check;
	addr pos;

	Return(pretty_common_p_(ptr, stream, &check));
	if (! check)
		return 0;
	switch (kind) {
		case pprint_tabular_line:
			size2_pretty_heap(&pos, print_pretty_tabular_line, a, b);
			break;

		case pprint_tabular_section:
			size2_pretty_heap(&pos, print_pretty_tabular_section, a, b);
			break;

		case pprint_tabular_line_relative:
			size2_pretty_heap(&pos, print_pretty_tabular_liner, a, b);
			break;

		case pprint_tabular_section_relative:
			size2_pretty_heap(&pos, print_pretty_tabular_sectionr, a, b);
			break;

		default:
			return fmte_("Invalid newline type.", NULL);
	}

	return push_pretty_stream_(stream, pos);
}

int pprint_tab_section_(Execute ptr, addr stream, fixnum column, fixnum colinc)
{
	return pprint_tab_print_(ptr, stream,
			pprint_tabular_section, column, colinc);
}

int pprint_tab_section_relative_(Execute ptr,
		addr stream, fixnum column, fixnum colinc)
{
	return pprint_tab_print_(ptr, stream,
			pprint_tabular_section_relative, column, colinc);
}

static int pprint_tab_output_(addr stream, fixnum size)
{
	for (; 0 < size; size--) {
		Return(write_char_stream_(stream, ' '));
	}

	return 0;
}

static fixnum pprint_colinc_division(fixnum size, fixnum colinc)
{
	return ((size / colinc) + 1) * colinc;
}

static void pprint_tab_absolute_size(fixnum *ret,
		fixnum column, fixnum colinc, fixnum base, fixnum now)
{
	fixnum plus;

	now -= base;
	if (now < column) {
		*ret = column - now;
		return;
	}
	if (colinc == 0) {
		*ret = 0;
		return;
	}
	plus = column + pprint_colinc_division(now - column, colinc);
	*ret = plus - now;
}

static void pprint_tab_relative_size(fixnum *ret,
		fixnum column, fixnum colinc, fixnum base, fixnum now)
{
	now += column - base;
	if (colinc) {
		now %= colinc;
		if (now)
			column += colinc - now;
	}
	*ret = (column < 0)? 0: column;
}

int pprint_tab_absolute_force_(addr stream,
		fixnum column, fixnum colinc, fixnum now)
{
	pprint_tab_absolute_size(&now, column, colinc, 0, now);
	return pprint_tab_output_(stream, now);
}

int pprint_tab_relative_force_(addr stream,
		fixnum column, fixnum colinc, fixnum now)
{
	pprint_tab_relative_size(&now, column, colinc, 0, now);
	return pprint_tab_output_(stream, now);
}


/*
 *  output
 */
static int write_char_white_(addr stream, size_t *white)
{
	for (; *white; (*white)--) {
		Return(write_char_stream_(stream, ' '));
	}

	return 0;
}

static int pretty_write_string_(addr stream, addr pos, size_t *white)
{
	unicode c;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (c == ' ') {
			(*white)++;
			continue;
		}
		Return(write_char_white_(stream, white));
		Return(write_char_stream_(stream, c));
	}

	return 0;
}

static void pretty_write_space(addr stream, addr pos, size_t *white)
{
	size_t value;
	GetIndex(pos, &value);
	*white += value;
}

static int pretty_write_(addr stream, addr list)
{
	addr pos;
	size_t white;

	nreverse(&list, list);
	white = 0;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (stringp(pos)) {
			Return(pretty_write_string_(stream, pos, &white));
			continue;
		}
		if (GetType(pos) == LISPTYPE_INDEX) {
			pretty_write_space(stream, pos, &white);
			continue;
		}
		if (pos == T) {
			Return(write_char_white_(stream, &white));
			Return(terpri_stream_(stream));
			white = 0;
			continue;
		}
		if (pos == Nil) {
			Return(terpri_stream_(stream));
			white = 0;
			continue;
		}
		return fmte_("Invalid pretty-output object ~S.", pos, NULL);
	}

	return write_char_white_(stream, &white);
}


/*
 *  block / section
 */
struct pretty_block {
	addr pretty, root, list, perline;
	unsigned miserp : 1;
	unsigned newlinep : 1;
	unsigned previous : 1;
	unsigned break_lines_p : 1;
	unsigned print_lines_p : 1;
	unsigned print_miser_p : 1;
	size_t print_lines, print_miser, print_margin;
	size_t indent, base, now, current, section, lines;
};

static void pretty_push_object(struct pretty_block *ptr, addr pos)
{
	cons_heap(&(ptr->root), pos, ptr->root);
}

static void pretty_push_terpri(struct pretty_block *ptr)
{
	pretty_push_object(ptr, T);
}

static void pretty_push_newline(struct pretty_block *ptr)
{
	pretty_push_object(ptr, Nil);
}

static int pretty_push_char_(struct pretty_block *ptr, const char *str)
{
	addr pos;
	size_t size;

	strvect_char_heap(&pos, str);
	pretty_push_object(ptr, pos);
	Return(eastasian_length_(pos, &size, NULL));
	ptr->now += size;

	return 0;
}

static int pretty_push_string_(struct pretty_block *ptr, addr pos, size_t *ret)
{
	size_t size;

	Check(! stringp(pos), "type error");
	pretty_push_object(ptr, pos);
	Return(eastasian_length_(pos, &size, NULL));
	ptr->now += size;
	if (ret)
		*ret = size;

	return 0;
}

static void pretty_push_size(struct pretty_block *ptr, size_t value)
{
	addr pos;
	index_heap(&pos, value);
	pretty_push_object(ptr, pos);
	ptr->now += value;
}

static void pretty_push_index(struct pretty_block *ptr, addr pos)
{
	size_t value;
	GetIndex(pos, &value);
	pretty_push_object(ptr, pos);
	ptr->now += value;
}

static int pretty_prefix_plus_(addr pos, size_t *ret)
{
	addr x;
	size_t size, value;

	/* prefix */
	size = 0;
	prefix_pretty_stream(pos, &x);
	if (x != Nil) {
		Return(eastasian_length_(x, &value, NULL));
		size += value;
	}

	/* sharp */
	sharp_pretty_stream(pos, &x);
	if (x != Nil) {
		Return(eastasian_length_(x, &value, NULL));
		size += value;
	}

	/* per-line-prefix */
	perline_pretty_stream(pos, &x);
	if (x != Nil) {
		Return(eastasian_length_(x, &value, NULL));
		size += value;
	}

	/* result */
	return Result(ret, size);
}

static int pretty_suffix_plus_(addr pos, size_t *size)
{
	addr x;
	size_t value;

	*size = 0;
	suffix_pretty_stream(pos, &x);
	if (x != Nil) {
		Return(eastasian_length_(x, &value, NULL));
		*size += value;
	}

	return 0;
}

static int pretty_tabular_plus_(struct pretty_block *ptr, addr pos, size_t *ret)
{
	fixnum value, column, colinc, now;
	struct print_pretty_struct *str;

	str = struct_print_pretty(pos);
	column = str->value;
	colinc = str->colinc;
	now = ptr->now;
	switch (str->type) {
		case print_pretty_tabular_line:
			pprint_tab_absolute_size(&value, column, colinc, 0, now);
			break;

		case print_pretty_tabular_section:
			pprint_tab_absolute_size(&value, column, colinc, ptr->section, now);
			break;

		case print_pretty_tabular_liner:
			pprint_tab_relative_size(&value, column, colinc, 0, now);
			break;

		case print_pretty_tabular_sectionr:
			pprint_tab_relative_size(&value, column, colinc, ptr->section, now);
			break;

		default:
			*ret = 0;
			return fmte_("Invalid tabular type ~S.", pos, NULL);
	}

	return Result(ret, (size_t)value);
}

static int pretty_front_stream_(struct pretty_block *ptr,
		addr pos, size_t *rsize, int *ret)
{
	int listp, check;
	addr list, x;
	size_t size, value, section;

	/* rollback */
	size = 0;
	section = ptr->now;
	/* prefix */
	listp = listp_pretty_stream(pos);
	if (listp) {
		Return(pretty_prefix_plus_(pos, &value));
		size += value;
		ptr->now += value;
	}
	ptr->section = ptr->now;
	/* loop */
	result_pretty_stream(pos, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		if (pretty_print_force_p(x)) {
			ptr->section = section;
			*rsize = size;
			return Result(ret, 1);
		}
		else if (pretty_print_newline_p(x)) {
			ptr->section = ptr->now;
			continue;
		}
		else if (pretty_print_indent_p(x)) {
			continue;
		}
		else if (pretty_print_tabular_p(x)) {
			Return(pretty_tabular_plus_(ptr, x, &value));
		}
		else if (stringp(x)) {
			Return(eastasian_length_(x, &value, NULL));
		}
		else if (pretty_stream_p(x)) {
			Return(pretty_front_stream_(ptr, x, &value, &check));
			if (check) {
				ptr->section = section;
				return Result(ret, 1);
			}
		}
		else {
			*rsize = 0;
			*ret = 0;
			return fmte_("Invalid print object ~S.", x, NULL);
		}
		size += value;
		ptr->now += value;
	}
	/* suffix */
	if (listp) {
		Return(pretty_suffix_plus_(pos, &value));
		size += value;
		ptr->now += value;
	}
	/* result */
	ptr->section = section;
	*rsize = size;
	return Result(ret, 0);
}

static int pretty_front_newline_(struct pretty_block *ptr,
		addr list, addr *next, size_t *rsize, int *ret)
{
	int check;
	addr x, cons;
	size_t size, value;

	size = 0;
	while (list != Nil) {
		cons = list;
		GetCons(list, &x, &list);
		if (pretty_print_force_p(x)) {
			return Result(ret, 1);
		}
		else if (pretty_print_newline_p(x)) {
			ptr->section = ptr->now;
			*rsize = size;
			*next = cons;
			return Result(ret, 0);
		}
		else if (pretty_print_indent_p(x)) {
			continue;
		}
		else if (pretty_print_tabular_p(x)) {
			Return(pretty_tabular_plus_(ptr, x, &value));
		}
		else if (stringp(x)) {
			Return(eastasian_length_(x, &value, NULL));
		}
		else if (pretty_stream_p(x)) {
			Return(pretty_front_stream_(ptr, x, &value, &check));
			if (check)
				return Result(ret, 1);
		}
		else {
			*next = NULL;
			*rsize = 0;
			*ret = 0;
			return fmte_("Invalid print object ~S.", x, NULL);
		}
		size += value;
		ptr->now += value;
	}
	*rsize = 0;
	*next = Nil;
	return Result(ret, 0);
}

static int pretty_tail_stream_(struct pretty_block *ptr,
		addr pos, size_t *rsize, int *ret)
{
	int listp, check;
	addr list, x;
	size_t size, value, section;

	/* rollback */
	size = 0;
	section = ptr->now;
	/* prefix */
	listp = listp_pretty_stream(pos);
	if (listp) {
		Return(pretty_prefix_plus_(pos, &value));
		size += value;
		ptr->now += value;
	}
	ptr->section = ptr->now;
	/* loop */
	result_pretty_stream(pos, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		if (pretty_print_force_p(x)) {
			ptr->section = section;
			*rsize = size;
			return Result(ret, 1);
		}
		else if (pretty_print_newline_p(x)) {
			ptr->section = ptr->now;
			continue;
		}
		else if (pretty_print_indent_p(x)) {
			continue;
		}
		else if (pretty_print_tabular_p(x)) {
			Return(pretty_tabular_plus_(ptr, x, &value));
		}
		else if (stringp(x)) {
			Return(eastasian_length_(x, &value, NULL));
		}
		else if (pretty_stream_p(x)) {
			Return(pretty_tail_stream_(ptr, x, &value, &check));
			if (check) {
				ptr->section = section;
				*rsize = size + value;
				return Result(ret, 1);
			}
		}
		else {
			*rsize = 0;
			*ret = 0;
			return fmte_("Invalid print object ~S.", x, NULL);
		}
		size += value;
		ptr->now += value;
	}
	/* suffix */
	if (listp) {
		Return(pretty_suffix_plus_(pos, &value));
		size += value;
		ptr->now += value;
	}
	/* result */
	ptr->section = section;
	*rsize = size;
	return Result(ret, 0);
}

static int pretty_tail_section_loop_(struct pretty_block *ptr,
		addr list, size_t *rsize, int *ret)
{
	int check;
	addr x;
	size_t size, value;

	size = 0;
	while (list != Nil) {
		GetCons(list, &x, &list);
		if (pretty_print_force_p(x)) {
			*rsize = size;
			return Result(ret, 1);
		}
		else if (pretty_print_newline_p(x)) {
			goto newline;
		}
		else if (pretty_print_indent_p(x)) {
			continue;
		}
		else if (pretty_print_tabular_p(x)) {
			Return(pretty_tabular_plus_(ptr, x, &value));
		}
		else if (stringp(x)) {
			Return(eastasian_length_(x, &value, NULL));
		}
		else if (pretty_stream_p(x)) {
			Return(pretty_tail_stream_(ptr, x, &value, &check));
			if (check) {
				*rsize = size + value;
				return Result(ret, 1);
			}
		}
		else {
			*rsize = 0;
			*ret = 0;
			return fmte_("Invalid print object ~S.", x, NULL);
		}
		size += value;
		ptr->now += value;
	}
	Return(pretty_suffix_plus_(ptr->pretty, &value));
	size += value;
	ptr->now += value;
	/* result */
newline:
	*rsize = size;
	return Result(ret, 0);
}

static int pretty_tail_section_(struct pretty_block *ptr,
		addr list, size_t *rsize, int *ret)
{
	int check;
	size_t now, section;

	now = ptr->now;
	section = ptr->section;
	Return(pretty_tail_section_loop_(ptr, list, rsize, &check));
	ptr->now = now;
	ptr->section = section;

	return Result(ret, check);
}

static int pretty_section_(struct pretty_block *ptr, int *ret)
{
	int check;
	addr list;
	size_t a, b, c;

	list = ptr->list;
	c = ptr->now;
	for (;;) {
		/* front */
		Return(pretty_front_newline_(ptr, list, &list, &a, &check));
		if (check)
			return Result(ret, 1);
		if (list == Nil)
			break;
		/* tail */
		GetCdr(list, &list);
		Return(pretty_tail_section_(ptr, list, &b, &check));
		if (check)
			return Result(ret, 1);
		/* check */
		if (ptr->print_margin < a + b + c)
			return Result(ret, 1);
		c += a;
	}

	return Result(ret, 0);
}

static int pretty_newline_(struct pretty_block *ptr)
{
	int check;
	size_t now;

	if (ptr->newlinep)
		return 0;
	now = ptr->now;
	Return(pretty_section_(ptr, &check));
	if (check)
		ptr->newlinep = 1;
	ptr->now = now;
	ptr->section = now;

	return 0;
}

static int pretty_output_perline_(struct pretty_block *ptr)
{
	addr list, x;

	for (list = ptr->perline; list != Nil; ) {
		GetCons(list, &x, &list);
		if (stringp(x)) {
			Return(pretty_push_string_(ptr, x, NULL));
		}
		else if (GetType(x) == LISPTYPE_INDEX) {
			pretty_push_index(ptr, x);
		}
		else {
			return fmte_("Invalid perline type ~S.", x, NULL);
		}
	}

	return 0;
}

static int pretty_output_lines_(struct pretty_block *ptr, int *ret)
{
	if (! ptr->print_lines_p)
		return Result(ret, 0);
	ptr->lines++;
	if (ptr->lines < ptr->print_lines)
		return Result(ret, 0);
	Return(pretty_push_char_(ptr, " .."));
	ptr->break_lines_p = 1;
	return Result(ret, 1);
}

static int pretty_output_terpri_(struct pretty_block *ptr)
{
	int check;

	Return(pretty_output_lines_(ptr, &check));
	if (check)
		return 0;
	/* terpri */
	pretty_push_terpri(ptr);
	ptr->now = 0;
	/* base */
	if (ptr->base)
		pretty_push_size(ptr, ptr->base);
	/* perline */
	Return(pretty_output_perline_(ptr));
	/* current */
	if (ptr->now < ptr->current)
		pretty_push_size(ptr, ptr->current - ptr->now);
	/* current */
	ptr->section = ptr->now;
	ptr->previous = 0;

	return 0;
}

static int pretty_output_newline_(struct pretty_block *ptr)
{
	int check;

	Return(pretty_output_lines_(ptr, &check));
	if (check)
		return 0;
	/* terpri */
	pretty_push_newline(ptr);
	ptr->now = 0;
	/* base */
	if (ptr->base)
		pretty_push_size(ptr, ptr->base);
	/* perline */
	Return(pretty_output_perline_(ptr));
	/* current */
	if (ptr->now < ptr->current)
		pretty_push_size(ptr, ptr->current - ptr->now);
	/* indent */
	if (ptr->miserp == 0 && ptr->indent)
		pretty_push_size(ptr, ptr->indent);
	/* current */
	ptr->section = ptr->now;
	ptr->previous = 0;

	return 0;
}

static int pretty_output_fill_(struct pretty_block *ptr, addr list)
{
	int check;
	size_t size;

	/* miser */
	if (ptr->miserp) {
		if (ptr->newlinep) {
			Return(pretty_output_newline_(ptr));
		}
		return 0;
	}

	/* previous section */
	if (ptr->previous) {
		return pretty_output_newline_(ptr);
	}

	/* normal */
	Return(pretty_tail_section_(ptr, list, &size, &check));
	if (ptr->print_margin < ptr->now + size) {
		Return(pretty_output_newline_(ptr));
	}

	return 0;
}

static void pretty_output_indent_block(struct pretty_block *ptr, addr pos)
{
	struct print_pretty_struct *str;

	str = struct_print_pretty(pos);
	ptr->indent = (size_t)str->value;
	if (ptr->indent < 0)
		ptr->indent = 0;
}

static void pretty_output_indent_current(struct pretty_block *ptr, addr pos)
{
	fixnum value;
	struct print_pretty_struct *str;

	str = struct_print_pretty(pos);
	value = ptr->now - ptr->current;
	value += str->value;
	if (value < 0)
		value = 0;
	ptr->indent = (size_t)value;
	if (ptr->indent < 0)
		ptr->indent = 0;
}

static void pretty_output_tabular_line(struct pretty_block *ptr, addr pos)
{
	fixnum value;
	struct print_pretty_struct *str;

	str = struct_print_pretty(pos);
	value = ptr->now;
	pprint_tab_absolute_size(&value, str->value, str->colinc, 0, value);
	pretty_push_size(ptr, (size_t)value);
}

static void pretty_output_tabular_section(struct pretty_block *ptr, addr pos)
{
	fixnum value;
	struct print_pretty_struct *str;

	str = struct_print_pretty(pos);
	value = ptr->now;
	pprint_tab_absolute_size(&value, str->value, str->colinc, ptr->section, value);
	pretty_push_size(ptr, (size_t)value);
}

static void pretty_output_tabular_liner(struct pretty_block *ptr, addr pos)
{
	fixnum value;
	struct print_pretty_struct *str;

	str = struct_print_pretty(pos);
	value = ptr->now;
	pprint_tab_relative_size(&value, str->value, str->colinc, 0, value);
	pretty_push_size(ptr, (size_t)value);
}

static void pretty_output_tabular_sectionr(struct pretty_block *ptr, addr pos)
{
	fixnum value;
	struct print_pretty_struct *str;

	str = struct_print_pretty(pos);
	value = ptr->now;
	pprint_tab_relative_size(&value, str->value, str->colinc, ptr->section, value);
	pretty_push_size(ptr, (size_t)value);
}

static int pretty_struct_(struct pretty_block *ptr, addr pretty);
static int pretty_output_(struct pretty_block *ptr)
{
	addr list, x;

	list = ptr->list;
	while (list != Nil) {
		/* lines */
		if (ptr->break_lines_p) {
			break;
		}
		/* list */
		GetCons(list, &x, &list);
		if (stringp(x)) {
			Return(pretty_push_string_(ptr, x, NULL));
			continue;
		}
		if (pretty_print_linear_p(x)) {
			if (ptr->newlinep) {
				Return(pretty_output_newline_(ptr));
			}
			continue;
		}
		if (pretty_print_miser_p(x)) {
			if (ptr->miserp && ptr->newlinep) {
				Return(pretty_output_newline_(ptr));
			}
			continue;
		}
		if (pretty_print_fill_p(x)) {
			Return(pretty_output_fill_(ptr, list));
			continue;
		}
		if (pretty_print_mandatory_p(x)) {
			Return(pretty_output_newline_(ptr));
			continue;
		}
		if (pretty_print_terpri_p(x)) {
			Return(pretty_output_terpri_(ptr));
			continue;
		}
		if (pretty_print_indent_block_p(x)) {
			pretty_output_indent_block(ptr, x);
			continue;
		}
		if (pretty_print_indent_current_p(x)) {
			pretty_output_indent_current(ptr, x);
			continue;
		}
		if (pretty_print_tabular_line_p(x)) {
			pretty_output_tabular_line(ptr, x);
			continue;
		}
		if (pretty_print_tabular_section_p(x)) {
			pretty_output_tabular_section(ptr, x);
			continue;
		}
		if (pretty_print_tabular_liner_p(x)) {
			pretty_output_tabular_liner(ptr, x);
			continue;
		}
		if (pretty_print_tabular_sectionr_p(x)) {
			pretty_output_tabular_sectionr(ptr, x);
			continue;
		}
		if (pretty_stream_p(x)) {
			Return(pretty_struct_(ptr, x));
			continue;
		}
		return fmte_("Invalid pretty-object ~S.", x, NULL);
	}

	return 0;
}

static void pretty_push_perline(struct pretty_block *ptr, addr pos)
{
	if (ptr->perline == Nil) {
		cons_heap(&(ptr->perline), pos, Nil);
	}
	else {
		cons_heap(&pos, pos, Nil);
		nconc2_unsafe(ptr->perline, pos, &(ptr->perline));
	}
}

static void pretty_push_perline_index(struct pretty_block *ptr, size_t size)
{
	addr pos;
	index_heap(&pos, size);
	pretty_push_perline(ptr, pos);
}

static int pretty_prefix_(struct pretty_block *ptr)
{
	addr pos;
	size_t size;

	/* copy-list */
	copy_list_heap_unsafe(&(ptr->perline), ptr->perline);

	/* per-line-prefix */
	perline_pretty_stream(ptr->pretty, &pos);
	if (pos != Nil) {
		Return(pretty_push_string_(ptr, pos, NULL));
		pretty_push_perline(ptr, pos);
	}

	/* sharp */
	sharp_pretty_stream(ptr->pretty, &pos);
	if (pos != Nil) {
		Return(pretty_push_string_(ptr, pos, &size));
		pretty_push_perline_index(ptr, size);
	}

	/* prefix */
	prefix_pretty_stream(ptr->pretty, &pos);
	if (pos != Nil) {
		Return(pretty_push_string_(ptr, pos, &size));
		pretty_push_perline_index(ptr, size);
	}

	/* current */
	ptr->current = ptr->now;
	ptr->section = ptr->now;

	return 0;
}

static int pretty_suffix_(struct pretty_block *ptr)
{
	addr pos;

	suffix_pretty_stream(ptr->pretty, &pos);
	if (pos != Nil) {
		Return(pretty_push_string_(ptr, pos, NULL));
	}

	return 0;
}

static void pretty_result(struct pretty_block *str, struct pretty_block *ptr)
{
	addr list, root, x;

	nreverse(&list, str->root);
	for (root = ptr->root; list != Nil; ) {
		GetCons(list, &x, &list);
		cons_heap(&root, x, root);
	}
	ptr->root = root;
}

static void pretty_miser(struct pretty_block *ptr)
{
	/* nil */
	if (! ptr->print_miser_p) {
		ptr->miserp = 0;
		return;
	}
	/* over */
	if (ptr->print_margin <= ptr->print_miser) {
		ptr->miserp = 1;
		return;
	}
	/* check */
	ptr->miserp = (ptr->print_margin - ptr->print_miser) <= ptr->current;
}

static int pretty_struct_(struct pretty_block *ptr, addr pretty)
{
	struct pretty_block str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	/* addr */
	str = *ptr;
	str.pretty = pretty;
	str.root = Nil;
	str.miserp = 0;
	str.newlinep = 0;
	str.previous = 0;
	result_pretty_stream(pretty, &(str.list));
	/* value */
	str.base = ptr->base;
	str.current = 0;
	str.section = 0;
	str.indent = 0;
	str.lines = 0;
	/* print */
	if (listp_pretty_stream(pretty)) {
		Return(pretty_prefix_(&str));
		pretty_miser(&str);
		Return(pretty_newline_(&str));
		Return(pretty_output_(&str));
		Return(pretty_suffix_(&str));
	}
	else {
		Return(pretty_output_(&str));
	}
	/* result */
	pretty_result(&str, ptr);
	ptr->now = str.now;
	ptr->previous = str.newlinep;

	return 0;
}


/*
 *  interface
 */
static int pprint_initialize_(struct pretty_block *str, Execute ptr, addr stream)
{
	int check;
	size_t size;

	/* pointer */
	str->pretty = Nil;
	str->root = Nil;
	str->list = Nil;
	str->perline = Nil;

	/* value */
	str->newlinep = 0;
	str->previous = 0;
	str->miserp = 0;
	Return(getleft_stream_(stream, &size));
	str->base = size;
	str->now = size;
	str->current = 0;
	str->section = 0;
	str->indent = 0;
	str->lines = 0;
	str->print_lines = 0;
	str->print_miser = 0;
	str->print_margin = 0;
	str->break_lines_p = 0;

	Return(lines_print_(ptr, &size, &check));
	str->print_lines_p = check;
	str->print_lines = check? size: 0;

	Return(miser_width_print_(ptr, &size, &check));
	str->print_miser_p = check;
	str->print_miser = check? size: 0;

	return right_margin_print_(ptr, stream, &(str->print_margin));
}

int pprint_output_(Execute ptr, addr stream, addr pretty)
{
	struct pretty_block str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	/* argument check */
	if (pretty_stream_p(stream))
		return fmte_("Invalid output-stream ~S.", stream, NULL);
	if (! pretty_stream_p(pretty))
		return fmte_("Invalid pretty-stream ~S.", pretty, NULL);
	/* pretty-start */
	Return(pprint_initialize_(&str, ptr, stream));
	Return(pretty_struct_(&str, pretty));
	/* output */
	Return(pretty_write_(stream, str.root));
	return exitpoint_stream_(stream);
}


/************************************************************
 *  print_write.c
 ************************************************************/

static calltype_print WriteCircleTable[LISPTYPE_SIZE];
static calltype_print WriteCallTable[LISPTYPE_SIZE];

static int write_circle_call_(Execute ptr, addr stream, addr pos);
static int write_print_call_(Execute ptr, addr stream, addr pos);

/*
 *  print-check object
 */
struct print_check_struct {
	unsigned first : 1;
	size_t index;
};

static struct print_check_struct *ptr_print_check(addr pos)
{
	CheckType(pos, LISPSYSTEM_PRINT_CHECK);
	return (struct print_check_struct *)PtrBodySS(pos);
}

static void set_object_print_check(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_PRINT_CHECK);
	SetArraySS(pos, 0, value);
}

static int get_first_print_check(addr pos)
{
	CheckType(pos, LISPSYSTEM_PRINT_CHECK);
	return ptr_print_check(pos)->first;
}

static void set_first_print_check(addr pos)
{
	CheckType(pos, LISPSYSTEM_PRINT_CHECK);
	ptr_print_check(pos)->first = 1;
}

static size_t get_index_print_check(addr pos)
{
	CheckType(pos, LISPSYSTEM_PRINT_CHECK);
	return ptr_print_check(pos)->index;
}

static void print_check_heap(addr *ret, addr value)
{
	addr pos;
	struct print_check_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_PRINT_CHECK, 1, sizeoft(struct print_check_struct));
	str = ptr_print_check(pos);
	str->first = 0;
	str->index = 0;
	set_object_print_check(pos, value);
	*ret = pos;
}


/*
 *  print-write object
 */
struct print_write_struct {
	size_t index, depth;
};

static struct print_write_struct *ptr_print_write(addr pos)
{
	CheckType(pos, LISPSYSTEM_PRINT_WRITE);
	return (struct print_write_struct *)PtrBodySS(pos);
}

static void set_table_print_write(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_PRINT_WRITE);
	SetArraySS(pos, 0, value);
}

static void get_table_print_write(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_PRINT_WRITE);
	GetArraySS(pos, 0, ret);
}

static void print_write_heap(addr *ret)
{
	addr pos, table;
	struct print_write_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_PRINT_WRITE, 1, sizeoft(struct print_write_struct));
	str = ptr_print_write(pos);
	str->index = 1;
	str->depth = 0;
	hashtable_heap(&table);
	settest_hashtable(table, HASHTABLE_TEST_EQ);
	set_table_print_write(pos, table);
	*ret = pos;
}

void push_write_object(Execute ptr)
{
	addr symbol, pos;

	GetConst(SYSTEM_PRINT_WRITE, &symbol);
	print_write_heap(&pos);
	pushspecial_control(ptr, symbol, pos);
}

static void print_write_object(Execute ptr, addr *ret)
{
	addr symbol, pos;

	GetConst(SYSTEM_PRINT_WRITE, &symbol);
	getspecial_local(ptr, symbol, &pos);
	if (pos == Unbound) {
		print_write_heap(&pos);
		pushspecial_control(ptr, symbol, pos);
	}
	CheckType(pos, LISPSYSTEM_PRINT_WRITE);
	*ret = pos;
}

void getdepth_print_write(Execute ptr, size_t *ret)
{
	addr pos;
	print_write_object(ptr, &pos);
	*ret = ptr_print_write(pos)->depth;
}

void setdepth_print_write(Execute ptr, size_t value)
{
	addr pos;
	print_write_object(ptr, &pos);
	ptr_print_write(pos)->depth = value;
}

static void increment_print_write(addr write, addr check)
{
	struct print_write_struct *str1;
	struct print_check_struct *str2;

	CheckType(write, LISPSYSTEM_PRINT_WRITE);
	CheckType(check, LISPSYSTEM_PRINT_CHECK);
	str1 = ptr_print_write(write);
	str2 = ptr_print_check(check);
	if (str2->index == 0)
		str2->index = str1->index++;
}

static int intern_print_write_(Execute ptr, addr pos, int *ret)
{
	int check;
	addr write, cons;

	print_write_object(ptr, &write);
	get_table_print_write(write, &cons);
	Return(internp_hashheap_(cons, pos, &cons, &check));
	if (check == 0) {
		/* make */
		print_check_heap(&pos, pos);
		SetCdr(cons, pos);
		return Result(ret, 1);
	}
	else {
		/* found */
		GetCdr(cons, &pos);
		increment_print_write(write, pos);
		return Result(ret, 0);
	}
}

static int find_print_write_(Execute ptr, addr key, addr *value, int *ret)
{
	addr pos;

	print_write_object(ptr, &pos);
	get_table_print_write(pos, &pos);
	Return(findnil_hashtable_(pos, key, value));
	CheckType(*value, LISPSYSTEM_PRINT_CHECK);

	return Result(ret, ptr_print_check(*value)->index == 0);
}

void write_check_all_clear(Execute ptr)
{
	addr pos, key, value;

	print_write_object(ptr, &pos);
	get_table_print_write(pos, &pos);
	/* loop */
	hash_iterator_heap(&pos, pos);
	while (next_hash_iterator(pos, &key, &value)) {
		CheckType(value, LISPSYSTEM_PRINT_CHECK);
		ptr_print_check(value)->first = 0;
	}
}


/*
 *  default
 */
static int WriteBody_error_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(print_ascii_stream_(stream, "INVALID-OBJECT"));
	Return(type_name_p_(pos, &pos, &check));
	if (check)
		return 0;
	Return(write_char_stream_(stream, ' '));
	return write_print_call_(ptr, stream, pos);
}

static int WriteCall_error_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 0, 1, WriteBody_error_);
}

static int WriteCall_system_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 1, 1, NULL);
}


/*
 *  cons
 */
static int WriteCheckCall_cons_(Execute ptr, addr pos)
{
	int lenp, levelp, check;
	addr x;
	size_t len, level, depth, i;

	Return(length_print_(ptr, &len, &lenp));
	Return(level_print_(ptr, &level, &levelp));
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth)
		return 0;

	/* intern */
	Return(intern_print_write_(ptr, pos, &check));
	if (check == 0)
		return 0;

	/* list */
	CheckType(pos, LISPTYPE_CONS);
	setdepth_print_write(ptr, depth + 1);
	for (i = 0; ; i++) {
		/* *print-length* */
		if (lenp && len <= i)
			break;
		/* cons */
		GetCons(pos, &x, &pos);
		Return(write_check_call_(ptr, x));
		if (! consp(pos))
			break;
		Return(intern_print_write_(ptr, pos, &check));
		if (check == 0)
			break;
	}
	setdepth_print_write(ptr, depth);

	return 0;
}

int pprint_pop_circle_(Execute ptr, addr stream, addr pos, int *ret)
{
	int check;
	addr x;
	size_t index;

	Return(find_print_write_(ptr, pos, &x, &check));
	if (check)
		return Result(ret, 0);
	/* found */
	if (get_first_print_check(x) == 0)
		return fmte_("Invalid loop object.", NULL);

	Return(print_ascii_stream_(stream, ". #"));
	index = get_index_print_check(x);
	Return(output_nosign_fixnum_(stream, index, 10, 1));
	/* #3# */
	Return(write_char_stream_(stream, '#'));
	return Result(ret, 1);
}

static int WriteCircle_find_(Execute ptr, addr stream, addr pos, int *ret)
{
	int check;
	addr x;
	size_t index;

	Return(find_print_write_(ptr, pos, &x, &check));
	if (check)
		return Result(ret, 0);
	/* found */
	Return(write_char_stream_(stream, '#'));
	index = get_index_print_check(x);
	Return(output_nosign_fixnum_(stream, index, 10, 1));
	/* first, second */
	if (get_first_print_check(x) == 0) {
		/* #3= (...) */
		Return(write_char_stream_(stream, '='));
		set_first_print_check(x);
		return Result(ret, 0);
	}
	else {
		/* #3# */
		Return(write_char_stream_(stream, '#'));
		return Result(ret, 1);
	}
}

int pprint_check_circle_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;
	addr x, stream;
	size_t index;

	CheckType(pos, LISPTYPE_CONS);
	Return(find_print_write_(ptr, pos, &x, &check));
	if (check) {
		*value = Nil;
		return Result(ret, 0);
	}

	/* found */
	open_output_string_stream(&stream, 0);
	Return(write_char_stream_(stream, '#'));
	index = get_index_print_check(x);
	Return(output_nosign_fixnum_(stream, index, 10, 1));
	/* first, second */
	if (get_first_print_check(x) == 0) {
		/* #3= (...) */
		Return(write_char_stream_(stream, '='));
		set_first_print_check(x);
		Return(string_stream_heap_(stream, value));
		return Result(ret, 0);
	}
	else {
		/* #3# */
		Return(write_char_stream_(stream, '#'));
		Return(string_stream_heap_(stream, value));
		return Result(ret, 1);
	}
}

static int WriteCircleCall_cons_check_(Execute ptr, addr pos, int *ret)
{
	if (! consp(pos))
		return Result(ret, 0);
	else
		return find_print_write_(ptr, pos, &pos, ret);
}

static int WriteCircleCall_cons_(Execute ptr, addr stream, addr pos)
{
	int lenp, levelp, check;
	addr x;
	size_t i, len, level, depth;

	Return(length_print_(ptr, &len, &lenp));
	Return(level_print_(ptr, &level, &levelp));
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth)
		return write_char_stream_(stream, '#');

	/* table */
	Return(WriteCircle_find_(ptr, stream, pos, &check));
	if (check)
		return 0;

	/* list */
	CheckType(pos, LISPTYPE_CONS);
	setdepth_print_write(ptr, depth + 1);
	Return(write_char_stream_(stream, '('));
	for (i = 0; ; i++) {
		/* *print-length* */
		if (lenp && len <= i) {
			Return(print_ascii_stream_(stream, "..."));
			break;
		}
		/* cons */
		GetCons(pos, &x, &pos);
		Return(write_circle_call_(ptr, stream, x));
		if (pos == Nil)
			break;
		Return(WriteCircleCall_cons_check_(ptr, pos, &check));
		if (check) {
			Return(write_char_stream_(stream, ' '));
		}
		else {
			Return(print_ascii_stream_(stream, " . "));
			Return(write_circle_call_(ptr, stream, pos));
			break;
		}
	}
	Return(write_char_stream_(stream, ')'));
	setdepth_print_write(ptr, depth);

	return 0;
}

static int WriteCall_cons_(Execute ptr, addr stream, addr pos)
{
	int lenp, levelp;
	addr x;
	size_t len, level, depth, i;

	Return(length_print_(ptr, &len, &lenp));
	Return(level_print_(ptr, &level, &levelp));
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth)
		return write_char_stream_(stream, '#');

	/* list */
	CheckType(pos, LISPTYPE_CONS);
	setdepth_print_write(ptr, depth + 1);
	Return(write_char_stream_(stream, '('));
	for (i = 0; ; i++) {
		/* *print-length* */
		if (lenp && len <= i) {
			Return(print_ascii_stream_(stream, "..."));
			break;
		}
		/* cons */
		GetCons(pos, &x, &pos);
		Return(write_print_call_(ptr, stream, x));
		if (pos == Nil)
			break;
		if (consp(pos)) {
			Return(write_char_stream_(stream, ' '));
		}
		else {
			Return(print_ascii_stream_(stream, " . "));
			Return(write_print_call_(ptr, stream, pos));
			break;
		}
	}
	Return(write_char_stream_(stream, ')'));
	setdepth_print_write(ptr, depth);

	return 0;
}


/*
 *  vector
 */
static int WriteCheckCall_vector_(Execute ptr, addr pos)
{
	int lenp, levelp, check;
	addr x;
	size_t len, level, depth, size, i;

	Return(length_print_(ptr, &len, &lenp));
	Return(level_print_(ptr, &level, &levelp));
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth)
		return 0;

	/* intern */
	Return(intern_print_write_(ptr, pos, &check));
	if (check == 0)
		return 0;

	/* list */
	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	setdepth_print_write(ptr, depth + 1);
	for (i = 0; i < size; i++) {
		/* *print-length* */
		if (lenp && len <= i)
			break;
		/* vector */
		getarray(pos, i, &x);
		Return(write_check_call_(ptr, x));
	}
	setdepth_print_write(ptr, depth);

	return 0;
}

static int WriteCircleCall_vector_default_(Execute ptr, addr stream, addr pos)
{
	int lenp, levelp, check;
	addr x;
	size_t len, level, depth, size, i;

	Return(length_print_(ptr, &len, &lenp));
	Return(level_print_(ptr, &level, &levelp));
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth)
		return write_char_stream_(stream, '#');

	/* table */
	Return(WriteCircle_find_(ptr, stream, pos, &check));
	if (check)
		return 0;

	/* list */
	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	setdepth_print_write(ptr, depth + 1);
	Return(print_ascii_stream_(stream, "#("));
	for (i = 0; i < size; i++) {
		if (i != 0) {
			Return(write_char_stream_(stream, ' '));
		}
		/* *print-length* */
		if (lenp && len <= i) {
			Return(print_ascii_stream_(stream, "..."));
			break;
		}
		/* vector */
		getarray(pos, i, &x);
		Return(write_circle_call_(ptr, stream, x));
	}
	Return(write_char_stream_(stream, ')'));
	setdepth_print_write(ptr, depth);

	return 0;
}

static int WriteCircleCall_vector_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(array_print_(ptr, &check));
	if (check)
		return WriteCircleCall_vector_default_(ptr, stream, pos);

	return print_unreadable_object_(ptr, stream, pos, 1, 1, NULL);
}

static int WriteCall_vector_default_(Execute ptr, addr stream, addr pos)
{
	int lenp, levelp;
	addr x;
	size_t len, level, depth, size, i;

	Return(length_print_(ptr, &len, &lenp));
	Return(level_print_(ptr, &level, &levelp));
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth)
		return write_char_stream_(stream, '#');

	/* list */
	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	setdepth_print_write(ptr, depth + 1);
	Return(print_ascii_stream_(stream, "#("));
	for (i = 0; i < size; i++) {
		if (i != 0) {
			Return(write_char_stream_(stream, ' '));
		}
		/* *print-length* */
		if (lenp && len <= i) {
			Return(print_ascii_stream_(stream, "..."));
			break;
		}
		/* vector */
		getarray(pos, i, &x);
		Return(write_print_call_(ptr, stream, x));
	}
	Return(write_char_stream_(stream, ')'));
	setdepth_print_write(ptr, depth);

	return 0;
}

static int WriteCall_vector_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(array_print_(ptr, &check));
	if (check)
		return WriteCall_vector_default_(ptr, stream, pos);

	return print_unreadable_object_(ptr, stream, pos, 1, 1, NULL);
}


/*
 *  array
 */
struct write_array_struct {
	Execute ptr;
	addr pos, stream;
	const size_t *data;
	size_t dimension, depth, index;
};

static void make_write_array(struct write_array_struct *str,
		Execute ptr, addr stream, addr pos, const size_t *data, size_t dimension)
{
	str->ptr = ptr;
	str->stream = stream;
	str->pos = pos;
	str->data = data;
	str->dimension = dimension;
	str->depth = 0;
	str->index = 0;
}

static int WriteArray_specialized_p(addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	return (str->dimension == 1)
		&& (str->type == ARRAY_TYPE_BIT || str->type == ARRAY_TYPE_CHARACTER);
}

static int WriteCheckCall_array_print_(struct write_array_struct *str)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = str->ptr->local;
	push_local(local, &stack);
	Return(array_get_(local, str->pos, str->index++, &pos));
	Return(write_check_call_(str->ptr, pos));
	rollback_local(local, stack);

	return 0;
}

static int WriteCheckCall_array_call_(struct write_array_struct *str)
{
	int lenp;
	const size_t *data;
	size_t len, i, loop, depth;

	/* output */
	depth = str->depth;
	if (str->dimension <= str->depth)
		return WriteCheckCall_array_print_(str);

	/* restrict */
	data = str->data;
	loop = data[depth];
	Return(length_print_(str->ptr, &len, &lenp));

	str->depth++;
	for (i = 0; i < loop; i++) {
		/* *print-length* */
		if (lenp && len <= i) {
			str->index += loop - i;
			break;
		}
		/* array */
		Return(WriteCheckCall_array_call_(str));
	}
	str->depth--;

	return 0;
}

static int WriteCheckCall_array_(Execute ptr, addr pos)
{
	int check;
	const size_t *data;
	size_t dimension, level, depth;
	struct write_array_struct str;

	/* *print-level* */
	Return(level_print_(ptr, &level, &check));
	getdepth_print_write(ptr, &depth);
	if (check && level <= depth)
		return 0;

	/* intern */
	Return(intern_print_write_(ptr, pos, &check));
	if (check == 0)
		return 0;

	/* specialized */
	if (WriteArray_specialized_p(pos))
		return 0;

	/* prefix */
	dimension = ArrayInfoStruct(pos)->dimension;
	data = array_ptrsize(pos);

	/* body */
	setdepth_print_write(ptr, depth + 1);
	make_write_array(&str, ptr, Nil, pos, data, dimension);
	Return(WriteCheckCall_array_call_(&str));
	setdepth_print_write(ptr, depth);

	return 0;
}

static int WriteArray_bit_(Execute ptr, addr stream, addr pos)
{
	int value;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(print_ascii_stream_(stream, "#*"));
	for (i = 0; i < size; i++) {
		Return(array_get_bit_(pos, i, &value));
		Return(write_char_stream_(stream, value? '1': '0'));
	}

	return 0;
}

static int WriteCall_string_(Execute ptr, addr stream, addr object);
static int WriteArray_specialized_(Execute ptr, addr stream, addr pos)
{
	switch (ArrayInfoStruct(pos)->type) {
		case ARRAY_TYPE_BIT:
			return WriteArray_bit_(ptr, stream, pos);

		case ARRAY_TYPE_CHARACTER:
			return WriteCall_string_(ptr, stream, pos);

		default:
			return fmte_("Invalid array type.", NULL);
	}
}

static int WriteCircleCall_array_print_(struct write_array_struct *str)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = str->ptr->local;
	push_local(local, &stack);
	Return(array_get_(local, str->pos, str->index++, &pos));
	Return(write_circle_call_(str->ptr, str->stream, pos));
	rollback_local(local, stack);

	return 0;
}

static int WriteCall_array_print_(struct write_array_struct *str)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = str->ptr->local;
	push_local(local, &stack);
	Return(array_get_(local, str->pos, str->index++, &pos));
	Return(write_print_call_(str->ptr, str->stream, pos));
	rollback_local(local, stack);

	return 0;
}

static int WriteCircleCall_array_call_(struct write_array_struct *str)
{
	int lenp;
	addr stream;
	const size_t *data;
	size_t len, i, loop, depth;

	/* output */
	depth = str->depth;
	if (str->dimension <= depth)
		return WriteCircleCall_array_print_(str);

	/* restrict */
	stream = str->stream;
	data = str->data;
	loop = data[depth];
	Return(length_print_(str->ptr, &len, &lenp));

	Return(write_char_stream_(stream, '('));
	str->depth++;
	for (i = 0; i < loop; i++) {
		if (i != 0) {
			Return(write_char_stream_(stream, ' '));
		}
		/* *print-length* */
		if (lenp && len <= i) {
			Return(print_ascii_stream_(stream, "..."));
			str->index += loop - i;
			break;
		}
		/* array */
		Return(WriteCircleCall_array_call_(str));
	}
	str->depth--;
	Return(write_char_stream_(stream, ')'));

	return 0;
}

static int WriteCall_array_call_(struct write_array_struct *str)
{
	int lenp;
	addr stream;
	const size_t *data;
	size_t len, i, loop, depth;

	/* output */
	depth = str->depth;
	if (str->dimension <= str->depth)
		return WriteCall_array_print_(str);

	/* restrict */
	stream = str->stream;
	data = str->data;
	loop = data[depth];
	Return(length_print_(str->ptr, &len, &lenp));

	Return(write_char_stream_(stream, '('));
	str->depth++;
	for (i = 0; i < loop; i++) {
		if (i != 0) {
			Return(write_char_stream_(stream, ' '));
		}
		/* *print-length* */
		if (lenp && len <= i) {
			Return(print_ascii_stream_(stream, "..."));
			str->index += loop - i;
			break;
		}
		/* array */
		Return(WriteCall_array_call_(str));
	}
	str->depth--;
	Return(write_char_stream_(stream, ')'));

	return 0;
}

static int WriteCircleCall_array_default_(Execute ptr, addr stream, addr pos)
{
	int check;
	const size_t *data;
	size_t dimension, level, depth;
	struct write_array_struct str;

	/* *print-level* */
	Return(level_print_(ptr, &level, &check));
	getdepth_print_write(ptr, &depth);
	if (check && level <= depth)
		return write_char_stream_(stream, '#');

	/* table */
	Return(WriteCircle_find_(ptr, stream, pos, &check));
	if (check)
		return 0;

	/* specialized */
	if (WriteArray_specialized_p(pos))
		return WriteArray_specialized_(ptr, stream, pos);

	/* prefix */
	Return(write_char_stream_(stream, '#'));
	dimension = ArrayInfoStruct(pos)->dimension;
	data = array_ptrsize(pos);
	if (dimension != 1) {
		Return(output_nosign_index_(stream, dimension, 10, 1));
		Return(write_char_stream_(stream, 'A'));
	}

	/* body */
	setdepth_print_write(ptr, depth + 1);
	make_write_array(&str, ptr, stream, pos, data, dimension);
	Return(WriteCircleCall_array_call_(&str));
	setdepth_print_write(ptr, depth);

	return 0;
}

static int WriteCircleCall_array_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(array_print_(ptr, &check));
	if (check)
		return WriteCircleCall_array_default_(ptr, stream, pos);

	return print_unreadable_object_(ptr, stream, pos, 1, 1, NULL);
}

static int WriteCall_array_default_(Execute ptr, addr stream, addr pos)
{
	int check;
	const size_t *data;
	size_t dimension, level, depth;
	struct write_array_struct str;

	/* *print-level* */
	Return(level_print_(ptr, &level, &check));
	getdepth_print_write(ptr, &depth);
	if (check && level <= depth)
		return write_char_stream_(stream, '#');

	/* specialized */
	if (WriteArray_specialized_p(pos))
		return WriteArray_specialized_(ptr, stream, pos);

	/* prefix */
	Return(write_char_stream_(stream, '#'));
	dimension = ArrayInfoStruct(pos)->dimension;
	data = array_ptrsize(pos);
	if (dimension != 1) {
		Return(output_nosign_index_(stream, dimension, 10, 1));
		Return(write_char_stream_(stream, 'A'));
	}

	/* body */
	setdepth_print_write(ptr, depth + 1);
	make_write_array(&str, ptr, stream, pos, data, dimension);
	Return(WriteCall_array_call_(&str));
	setdepth_print_write(ptr, depth);

	return 0;
}

static int WriteCall_array_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(array_print_(ptr, &check));
	if (check)
		return WriteCall_array_default_(ptr, stream, pos);

	return print_unreadable_object_(ptr, stream, pos, 1, 1, NULL);
}


/*
 *  symbol
 */
static int WriteSymbol_direct_norm_(addr stream, addr pos)
{
	size_t i, size;
	unicode u;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		Return(write_char_stream_(stream, u));
	}

	return 0;
}

static int WriteSymbol_downcase_norm_(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		Return(write_char_stream_(stream, toLowerUnicode(u)));
	}

	return 0;
}

static int WriteSymbol_upcase_norm_(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		Return(write_char_stream_(stream, toUpperUnicode(u)));
	}

	return 0;
}

static int WriteSymbol_up_cap_norm_(addr stream, addr pos)
{
	int check;
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	check = 1;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (isAlphanumeric(u)) {
			if (check) {
				Return(write_char_stream_(stream, u));
				check = 0;
			}
			else {
				Return(write_char_stream_(stream, toLowerUnicode(u)));
			}
		}
		else {
			Return(write_char_stream_(stream, u));
			check = 1;
		}
	}

	return 0;
}

static int WriteSymbol_down_cap_norm_(addr stream, addr pos)
{
	int check;
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	check = 1;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (isAlphanumeric(u)) {
			if (check) {
				Return(write_char_stream_(stream, toUpperUnicode(u)));
				check = 0;
			}
			else {
				Return(write_char_stream_(stream, u));
			}
		}
		else {
			Return(write_char_stream_(stream, u));
			check = 1;
		}
	}

	return 0;
}

static int WriteSymbol_check_invert_(addr pos, enum PrintCase *ret)
{
	enum PrintCase check;
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	check = PrintCase_unread;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (isUpperCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_upcase;
			}
			else if (check != PrintCase_upcase) {
				return Result(ret, PrintCase_preserve);
			}
		}
		else if (isLowerCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_downcase;
			}
			else if (check != PrintCase_downcase) {
				return Result(ret, PrintCase_preserve);
			}
		}
	}

	return Result(ret, check);
}

static int WriteSymbol_invert_norm_(addr stream, addr pos)
{
	enum PrintCase type;

	Return(WriteSymbol_check_invert_(pos, &type));
	switch (type) {
		case PrintCase_upcase:
			return WriteSymbol_downcase_norm_(stream, pos);

		case PrintCase_downcase:
			return WriteSymbol_upcase_norm_(stream, pos);

		default:
			return WriteSymbol_direct_norm_(stream, pos);
	}
}

static int WriteSymbol_check_escape(unicode c)
{
	return (c == ' ')
		|| (c == '`') || (c == ',')
		|| (c == '(') || (c == ')')
		|| (c == '|') || (c == '\\')
		|| (c == ':') || (c == ';')
		|| (c == '\'') || (c == '"');
}

static int WriteSymbol_direct_escape_(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (u == '\\' || u == '|') {
			Return(write_char_stream_(stream, '\\'));
		}
		Return(write_char_stream_(stream, u));
	}

	return 0;
}

static int WriteSymbol_check_upcase_escape_(addr pos, int *ret)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (WriteSymbol_check_escape(u))
			return Result(ret, 1);
		if (isLowerCase(u))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int WriteSymbol_up_up_output_(addr stream, addr pos)
{
	int check;

	Return(WriteSymbol_check_upcase_escape_(pos, &check));
	if (check) {
		Return(write_char_stream_(stream, '|'));
		Return(WriteSymbol_direct_escape_(stream, pos));
		Return(write_char_stream_(stream, '|'));
	}
	else {
		Return(WriteSymbol_direct_escape_(stream, pos));
	}

	return 0;
}

static int WriteSymbol_downcase_escape_(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		Return(write_char_stream_(stream, toLowerUnicode(u)));
	}

	return 0;
}

static int WriteSymbol_up_down_output_(addr stream, addr pos)
{
	int check;

	Return(WriteSymbol_check_upcase_escape_(pos, &check));
	if (check) {
		Return(write_char_stream_(stream, '|'));
		Return(WriteSymbol_direct_escape_(stream, pos));
		Return(write_char_stream_(stream, '|'));
	}
	else {
		Return(WriteSymbol_downcase_escape_(stream, pos));
	}

	return 0;
}

static int WriteSymbol_capitalize_escape_(addr stream, addr pos)
{
	int check;
	unicode u;
	size_t i, size;

	check = 1;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (isAlphanumeric(u)) {
			if (check) {
				Return(write_char_stream_(stream, toUpperUnicode(u)));
				check = 0;
			}
			else {
				Return(write_char_stream_(stream, toLowerUnicode(u)));
			}
		}
		else {
			Return(write_char_stream_(stream, u));
			check = 1;
		}
	}

	return 0;
}

static int WriteSymbol_up_cap_output_(addr stream, addr pos)
{
	int check;

	Return(WriteSymbol_check_upcase_escape_(pos, &check));
	if (check) {
		Return(write_char_stream_(stream, '|'));
		Return(WriteSymbol_direct_escape_(stream, pos));
		Return(write_char_stream_(stream, '|'));
	}
	else {
		Return(WriteSymbol_capitalize_escape_(stream, pos));
	}

	return 0;
}

static int WriteSymbol_escape_(Execute ptr,
		addr stream, addr pos, int (*call)(addr, addr))
{
	int exportp, check;
	addr package, value;

	Return(getpackage_(ptr, &value));
	GetPackageSymbol(pos, &package);
	if (package == Nil) {
		/* gensym */
		Return(gensym_print_(ptr, &check));
		if (check) {
			Return(print_ascii_stream_(stream, "#:"));
		}
		goto final;
	}

	Return(checksymbol_package_(pos, value, &check));
	if (check) {
		/* no package name */
		goto final;
	}

	if (keywordp(pos)) {
		Return(print_ascii_stream_(stream, ":"));
		goto final;
	}

	if (package == value) {
		goto final;
	}
	Return(externalp_package_(pos, value, &check));
	if (check) {
		/* package name */
		Return(exportp_package_(pos, package, &exportp));
		Return(getname_package_(package, &package));
		Return((*call)(stream, package));
		Return(print_ascii_stream_(stream, exportp? ":": "::"));
		goto final;
	}

final:
	/* symbol name */
	GetNameSymbol(pos, &pos);
	return (*call)(stream, pos);
}

static int WriteSymbol_check_downcase_escape_(addr pos, int *ret)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (WriteSymbol_check_escape(u))
			return Result(ret, 1);
		if (isUpperCase(u))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int WriteSymbol_upcase_escape_(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		Return(write_char_stream_(stream, toUpperUnicode(u)));
	}

	return 0;
}

static int WriteSymbol_down_up_output_(addr stream, addr pos)
{
	int check;

	Return(WriteSymbol_check_downcase_escape_(pos, &check));
	if (check) {
		Return(write_char_stream_(stream, '|'));
		Return(WriteSymbol_direct_escape_(stream, pos));
		Return(write_char_stream_(stream, '|'));
	}
	else {
		Return(WriteSymbol_upcase_escape_(stream, pos));
	}

	return 0;
}

static int WriteSymbol_down_down_output_(addr stream, addr pos)
{
	int check;

	Return(WriteSymbol_check_downcase_escape_(pos, &check));
	if (check) {
		Return(write_char_stream_(stream, '|'));
		Return(WriteSymbol_direct_escape_(stream, pos));
		Return(write_char_stream_(stream, '|'));
	}
	else {
		Return(WriteSymbol_direct_escape_(stream, pos));
	}

	return 0;
}

static int WriteSymbol_down_cap_output_(addr stream, addr pos)
{
	int check;

	Return(WriteSymbol_check_downcase_escape_(pos, &check));
	if (check) {
		Return(write_char_stream_(stream, '|'));
		Return(WriteSymbol_direct_escape_(stream, pos));
		Return(write_char_stream_(stream, '|'));
	}
	else {
		Return(WriteSymbol_capitalize_escape_(stream, pos));
	}

	return 0;
}

static int WriteSymbol_check_preserve_escape_(addr pos, int *ret)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (WriteSymbol_check_escape(u))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int WriteSymbol_preserve_output_(addr stream, addr pos)
{
	int check;

	Return(WriteSymbol_check_preserve_escape_(pos, &check));
	if (check) {
		Return(write_char_stream_(stream, '|'));
		Return(WriteSymbol_direct_escape_(stream, pos));
		Return(write_char_stream_(stream, '|'));
	}
	else {
		Return(WriteSymbol_direct_escape_(stream, pos));
	}

	return 0;
}

static int WriteSymbol_check_invert_escape_(addr pos, enum PrintCase *ret)
{
	enum PrintCase check;
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	check = PrintCase_unread;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (WriteSymbol_check_escape(u)) {
			return Result(ret, PrintCase_escape);
		}
		else if (isUpperCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_upcase;
			}
			else if (check != PrintCase_upcase) {
				return Result(ret, PrintCase_preserve);
			}
		}
		else if (isLowerCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_downcase;
			}
			else if (check != PrintCase_downcase) {
				return Result(ret, PrintCase_preserve);
			}
		}
	}

	return Result(ret, check);
}

static int WriteSymbol_invert_output_(addr stream, addr pos)
{
	enum PrintCase type;

	Return(WriteSymbol_check_invert_escape_(pos, &type));
	switch (type) {
		case PrintCase_upcase:
			return WriteSymbol_downcase_escape_(stream, pos);

		case PrintCase_downcase:
			return WriteSymbol_upcase_escape_(stream, pos);

		case PrintCase_escape:
			Return(write_char_stream_(stream, '|'));
			Return(WriteSymbol_direct_escape_(stream, pos));
			Return(write_char_stream_(stream, '|'));
			return 0;

		default:
			return WriteSymbol_direct_escape_(stream, pos);
	}
}

static int WriteSymbol_upcase_upcase_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(escape_print_(ptr, &check));
	if (check)
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_up_up_output_);
	else
		return WriteSymbol_direct_norm_(stream, pos);
}

static int WriteSymbol_upcase_downcase_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(escape_print_(ptr, &check));
	if (check)
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_up_down_output_);
	else
		return WriteSymbol_downcase_norm_(stream, pos);
}

static int WriteSymbol_upcase_capitalize_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(escape_print_(ptr, &check));
	if (check)
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_up_cap_output_);
	else
		return WriteSymbol_up_cap_norm_(stream, pos);
}

static int WriteSymbol_upcase_(Execute ptr, addr stream, addr pos)
{
	enum PrintCase value;

	Return(case_print_(ptr, &value));
	switch (value) {
		case PrintCase_upcase:
			return WriteSymbol_upcase_upcase_(ptr, stream, pos);

		case PrintCase_downcase:
			return WriteSymbol_upcase_downcase_(ptr, stream, pos);

		case PrintCase_capitalize:
			return WriteSymbol_upcase_capitalize_(ptr, stream, pos);

		default:
			return fmte_("printcase error", NULL);
	}
}

static int WriteSymbol_downcase_upcase_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(escape_print_(ptr, &check));
	if (check)
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_down_up_output_);
	else
		return WriteSymbol_upcase_norm_(stream, pos);
}

static int WriteSymbol_downcase_downcase_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(escape_print_(ptr, &check));
	if (check)
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_down_down_output_);
	else
		return WriteSymbol_direct_norm_(stream, pos);
}

static int WriteSymbol_downcase_capitalize_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(escape_print_(ptr, &check));
	if (check)
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_down_cap_output_);
	else
		return WriteSymbol_down_cap_norm_(stream, pos);
}

static int WriteSymbol_downcase_(Execute ptr, addr stream, addr pos)
{
	enum PrintCase value;

	Return(case_print_(ptr, &value));
	switch (value) {
		case PrintCase_upcase:
			return WriteSymbol_downcase_upcase_(ptr, stream, pos);

		case PrintCase_downcase:
			return WriteSymbol_downcase_downcase_(ptr, stream, pos);

		case PrintCase_capitalize:
			return WriteSymbol_downcase_capitalize_(ptr, stream, pos);

		default:
			return fmte_("printcase error", NULL);
	}
}

static int WriteSymbol_preserve_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(escape_print_(ptr, &check));
	if (check)
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_preserve_output_);
	else
		return WriteSymbol_direct_norm_(stream, pos);
}

static int WriteSymbol_invert_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(escape_print_(ptr, &check));
	if (check)
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_invert_output_);
	else
		return WriteSymbol_invert_norm_(stream, pos);
}

static int WriteCall_symbol_(Execute ptr, addr stream, addr pos)
{
	enum ReadTable_Case value;

	Return(readcase_readtable_(ptr, &value));
	switch (value) {
		case ReadTable_upcase:
			return WriteSymbol_upcase_(ptr, stream, pos);

		case ReadTable_downcase:
			return WriteSymbol_downcase_(ptr, stream, pos);

		case ReadTable_preserve:
			return WriteSymbol_preserve_(ptr, stream, pos);

		case ReadTable_invert:
			return WriteSymbol_invert_(ptr, stream, pos);

		default:
			return fmte_("*readtable* case error", NULL);
	}
}


/*
 *  type
 */
static int WriteCall_type_(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	Return(type_object_(&pos, pos));
	Return(print_ascii_stream_(stream, "#<TYPE "));
	Return(prin1_print_(ptr, stream, pos));
	Return(print_ascii_stream_(stream, ">"));

	return 0;
}


/*
 *  clos, structure
 */
static int WriteCall_clos_(Execute ptr, addr stream, addr pos)
{
	addr generic;

	Check(! closp(pos), "type error");
	GetConst(COMMON_PRINT_OBJECT, &generic);
	Return(getfunction_global_(generic, &generic));

	return funcall1_control_(ptr, &pos, generic, pos, stream, NULL);
}


/*
 *  character
 */
static int WriteCall_fixnum_value_(addr stream, fixnum value, unsigned base)
{
	/* zero */
	if (value == 0)
		return write_char_stream_(stream, '0');

	/* output */
	if (value < 0) {
		Return(write_char_stream_(stream, '-'));
	}

	return output_nosign_fixnum_(stream, value, base, 1);
}

static int WriteCall_character_name_(addr stream, unicode u)
{
	if (isStandardType(u)) {
		Return(write_char_stream_(stream, u));
	}
	else {
		Return(write_char_stream_(stream, 'u'));
		Return(WriteCall_fixnum_value_(stream, (fixnum)u, 16));
	}

	return 0;
}

static int WriteCall_character_string_(addr stream, addr string)
{
	unicode c;
	size_t i, size;

	string_length(string, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(string, i, &c));
		Return(write_char_stream_(stream, c));
	}

	return 0;
}

static int WriteCall_character_(Execute ptr, addr stream, addr object)
{
	int check;
	addr pos;
	unicode c;

	Return(escape_print_(ptr, &check));
	if (! check) {
		GetCharacter(object, &c);
		return write_char_stream_(stream, c);
	}

	Return(findtable_char_name_(&pos, object));
	if (pos != Nil) {
		/* found */
		Return(print_ascii_stream_(stream, "#\\"));
		Return(WriteCall_character_string_(stream, pos));
	}
	else {
		/* not found */
		Return(print_ascii_stream_(stream, "#\\"));
		GetCharacter(object, &c);
		Return(WriteCall_character_name_(stream, c));
	}

	return 0;
}


/*
 *  string
 */
static int WriteCall_string_(Execute ptr, addr stream, addr object)
{
	int check;
	unicode c;
	size_t size, i;

	string_length(object, &size);
	Return(escape_print_(ptr, &check));
	if (check) {
		Return(write_char_stream_(stream, '\"'));
		for (i = 0; i < size; i++) {
			Return(string_getc_(object, i, &c));
			if (c == '\"' || c == '\\') {
				Return(write_char_stream_(stream, '\\'));
			}
			Return(write_char_stream_(stream, c));
		}
		Return(write_char_stream_(stream, '\"'));
	}
	else {
		for (i = 0; i < size; i++) {
			Return(string_getc_(object, i, &c));
			Return(write_char_stream_(stream, c));
		}
	}

	return 0;
}


/*
 *  hash-table
 */
static int WriteBody_hashtable_(Execute ptr, addr stream, addr pos)
{
	addr value;
	size_t count;

	/* #<HASH-TABLE :TEST EQL :COUNT 123 0x1234...> */
	gettest_symbol_hashtable(pos, &value);
	getcount_hashtable(pos, &count);
	/* output */
	Return(print_ascii_stream_(stream, ":TEST "));
	Return(prin1_print_(ptr, stream, value));
	Return(print_ascii_stream_(stream, " :COUNT "));
	Return(output_nosign_index_(stream, count, 10, 0));

	return 0;
}

static int WriteCall_hashtable_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 1, 1, WriteBody_hashtable_);
}


/*
 *  fixnum
 */
static int WriteCall_radix_front_(addr stream, unsigned base)
{
	char buffer[8];

	Check(! isBaseChar(base), "base error");
	switch (base) {
		case 2:
			return print_ascii_stream_(stream, "#b");

		case 8:
			return print_ascii_stream_(stream, "#o");

		case 16:
			return print_ascii_stream_(stream, "#x");

		default:
			snprintf(buffer, 8, "#%ur", base);
			return print_ascii_stream_(stream, buffer);
	}
}

static int WriteCall_fixnum_(Execute ptr, addr stream, addr object)
{
	int radix;
	unsigned base;
	fixnum value;

	Return(base_print_(ptr, &base));
	Return(radix_print_(ptr, &radix));
	if (radix && base != 10) {
		Return(WriteCall_radix_front_(stream, base));
	}
	GetFixnum(object, &value);
	Return(WriteCall_fixnum_value_(stream, value, base));
	if (radix && base == 10) {
		Return(write_char_stream_(stream, '.'));
	}

	return 0;
}


/*
 *  bignum
 */
static int WriteCall_bignum_value_(LocalRoot local,
		addr stream, int sign, addr object, unsigned base)
{
	/* zero */
	if (zerop_bignum(object))
		return write_char_stream_(stream, '0');

	/* output */
	if (sign) {
		Return(write_char_stream_(stream, '-'));
	}

	return output_nosign_bignum_(local, stream, object, base, 1);
}

static int WriteCall_bignum_sign_(Execute ptr, addr stream, int sign, addr object)
{
	int radix;
	unsigned base;

	Return(base_print_(ptr, &base));
	Return(radix_print_(ptr, &radix));
	if (radix && base != 10) {
		Return(WriteCall_radix_front_(stream, base));
	}
	Return(WriteCall_bignum_value_(ptr->local, stream, sign, object, base));
	if (radix && base == 10) {
		Return(write_char_stream_(stream, '.'));
	}

	return 0;
}

static int WriteCall_bignum_(Execute ptr, addr stream, addr object)
{
	int sign;
	GetSignBignum(object, &sign);
	return WriteCall_bignum_sign_(ptr, stream, sign, object);
}


/*
 *  ratio
 */
static int WriteCall_ratio_(Execute ptr, addr stream, addr object)
{
	int sign, check;
	addr value;
	unsigned base;

	/* zero */
	if (zerop_ratio(object))
		return write_char_stream_(stream, '0');

	/* integer */
	GetDenomRatio(object, &value);
	if (equal_value_nosign_bignum(value, 1)) {
		GetSignRatio(object, &sign);
		GetNumerRatio(object, &value);
		return WriteCall_bignum_sign_(ptr, stream, sign, value);
	}

	/* ratio */
	Return(base_print_(ptr, &base));
	Return(radix_print_(ptr, &check));
	if (check) {
		Return(WriteCall_radix_front_(stream, base));
	}
	GetSignRatio(object, &sign);
	if (sign) {
		Return(write_char_stream_(stream, '-'));
	}

	return output_nosign_ratio_(ptr->local, stream, object, base, 1);
}


/*
 *  float
 */
static int WriteCall_single_float_(Execute ptr, addr stream, addr object)
{
	int markerp, marker, check;
	enum ReadTable_float type;
	single_float value;

	GetSingleFloat(object, &value);
	Return(float_readtable_(ptr, &type));
	markerp = (type != ReadTable_single);
	marker = markerp? 'F': 'E';
	Return(fmtfloat_princ_single_float_(stream, value, markerp, marker, &check));
	if (check)
		return fmte_("Invalid float value.", NULL);

	return 0;
}

static int WriteCall_double_float_(Execute ptr, addr stream, addr object)
{
	int markerp, marker, check;
	enum ReadTable_float type;
	double_float value;

	GetDoubleFloat(object, &value);
	Return(float_readtable_(ptr, &type));
	markerp = (type != ReadTable_double);
	marker = markerp? 'D': 'E';
	Return(fmtfloat_princ_double_float_(stream, value, markerp, marker, &check));
	if (check)
		return fmte_("Invalid float value.", NULL);

	return 0;
}

static int WriteCall_long_float_(Execute ptr, addr stream, addr object)
{
	int markerp, marker, check;
	enum ReadTable_float type;
	long_float value;

	GetLongFloat(object, &value);
	Return(float_readtable_(ptr, &type));
	markerp = (type != ReadTable_long);
	marker = markerp? 'L': 'E';
	Return(fmtfloat_princ_long_float_(stream, value, markerp, marker, &check));
	if (check)
		return fmte_("Invalid float value.", NULL);

	return 0;
}


/*
 *  complex
 */
static int WriteCall_complex_(Execute ptr, addr stream, addr object)
{
	addr real, imag;

	GetRealComplex(object, &real);
	GetImagComplex(object, &imag);
	Return(print_ascii_stream_(stream, "#C("));
	Return(write_print_call_(ptr, stream, real));
	Return(write_char_stream_(stream, ' '));
	Return(write_print_call_(ptr, stream, imag));
	Return(write_char_stream_(stream, ')'));

	return 0;
}


/*
 *  code
 */
static int WriteBody_code_(Execute ptr, addr stream, addr pos)
{
	return print_ascii_stream_(stream, "CODE");
}

static int WriteCall_code_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 0, 1, WriteBody_code_);
}


/*
 *  callname
 */
static int WriteBody_callname_(Execute ptr, addr stream, addr pos)
{
	name_callname_heap(pos, &pos);
	Return(print_ascii_stream_(stream, "CALLNAME "));
	return write_print_call_(ptr, stream, pos);
}

static int WriteCall_callname_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 0, 1, WriteBody_callname_);
}


/*
 *  function
 */
static int WriteBody_function_(Execute ptr, addr stream, addr pos)
{
	const char *name;
	struct function_struct *str;

	/* type */
	str = StructFunction(pos);
	if (str->macro) {
		if (str->compiled)
			name = "COMPILED-MACRO-FUNCTION ";
		else
			name = "MACRO-FUNCTION ";
	}
	else {
		if (str->compiled)
			name = "COMPILED-FUNCTION ";
		else
			name = "FUNCTION ";
	}
	Return(print_ascii_stream_(stream, name));

	/* name */
	GetNameFunction(pos, &pos);
	if (pos == Nil) {
		Return(print_ascii_stream_(stream, "LAMBDA"));
	}
	else {
		if (RefCallNameType(pos) == CALLNAME_SYMBOL) {
			GetCallName(pos, &pos);
			Return(write_print_call_(ptr, stream, pos));
		}
		else {
			GetCallName(pos, &pos);
			Return(print_ascii_stream_(stream, "(SETF "));
			Return(write_print_call_(ptr, stream, pos));
			Return(print_ascii_stream_(stream, ")"));
		}
	}

	return 0;
}

static int WriteCall_function_(Execute ptr, addr stream, addr pos)
{
	int ident;
	addr name;

	/* #<FUNCTION NAME> */
	GetNameFunction(pos, &name);
	ident = (name == Nil);
	return print_unreadable_object_(ptr, stream, pos, 0, ident, WriteBody_function_);
}


/*
 *  index
 */
static int WriteBody_index_(Execute ptr, addr stream, addr pos)
{
	LocalRoot local;
	LocalStack stack;
	size_t size;

	GetIndex(pos, &size);
	local = ptr->local;
	push_local(local, &stack);
	Return(write_print_call_(ptr, stream, intsizea(local, size)));
	rollback_local(local, stack);

	return 0;
}

static int WriteCall_index_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 0, 0, WriteBody_index_);
}


/*
 *  package
 */
static int WriteBody_package_(Execute ptr, addr stream, addr pos)
{
	Return(getname_package_(pos, &pos));
	if (stringp(pos))
		return print_string_stream_(stream, pos);
	else
		return write_print_call_(ptr, stream, pos);
}

static int WriteCall_package_(Execute ptr, addr stream, addr pos)
{
	/* #<PACKAGE NAME> */
	return print_unreadable_object_(ptr, stream, pos, 1, 0, WriteBody_package_);
}


/*
 *  random-state
 */
static int WriteBody_random_state_call_(Execute ptr, addr stream, addr pos)
{
	push_escape_print(ptr, 0);
	push_readably_print(ptr, 0);
	push_radix_print(ptr, 1);
	push_base_print(ptr, 16);
	Return(push_case_print_(ptr, PrintCase_upcase));
	make_bignum_random_state_local(ptr->local, pos, &pos);
	return WriteCall_bignum_(ptr, stream, pos);
}

static int WriteBody_random_state_(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_control(ptr, &control);
	(void)WriteBody_random_state_call_(ptr, stream, pos);
	return pop_control_(ptr, control);
}

static int WriteCall_random_state_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 1, 0, WriteBody_random_state_);
}


/*
 *  pathname
 */
static int WriteCall_pathname_(Execute ptr, addr stream, addr pos)
{
	int check;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(name_pathname_local_(ptr, pos, &pos));
	Return(escape_print_(ptr, &check));
	if (check) {
		Return(print_ascii_stream_(stream, "#P"));
	}
	Return(WriteCall_string_(ptr, stream, pos));
	rollback_local(local, stack);

	return 0;
}


/*
 *  stream
 */
static int WriteBody_pipe_stream_(Execute ptr, addr stream, addr pos)
{
	char data[64];
	enum StreamPipe type;
	const char *name;

	name = get_name_pipe_stream(pos);
	if (name) {
		Return(print_ascii_stream_(stream, "PIPE-STREAM "));
		Return(print_ascii_stream_(stream, name));
	}
	else {
		type = get_type_pipe_stream(pos);
		snprintc(data, 64, "%d", (int)type);
		Return(print_ascii_stream_(stream, "PIPE-STREAM "));
		Return(print_ascii_stream_(stream, data));
	}

	return 0;
}

static int WriteBody_stream_(Execute ptr, addr stream, addr pos)
{
	struct StructStream *str;

	str = PtrStructStream(pos);
	switch (str->type) {
		case StreamType_BinaryInput:
			return print_ascii_stream_(stream, "FILE-INPUT BINARY");

		case StreamType_BinaryOutput:
			return print_ascii_stream_(stream, "FILE-OUTPUT BINARY");

		case StreamType_BinaryIO:
			return print_ascii_stream_(stream, "FILE-IO BINARY");

		case StreamType_CharacterInput:
			return print_ascii_stream_(stream, "FILE-INPUT CHARACTER");

		case StreamType_CharacterOutput:
			return print_ascii_stream_(stream, "FILE-OUTPUT CHARACTER");

		case StreamType_CharacterIO:
			return print_ascii_stream_(stream, "FILE-IO CHARACTER");

		case StreamType_BincharInput:
			return print_ascii_stream_(stream, "FILE-INPUT SYSTEM");

		case StreamType_BincharOutput:
			return print_ascii_stream_(stream, "FILE-OUTPUT SYSTEM");

		case StreamType_BincharIO:
			return print_ascii_stream_(stream, "FILE-IO SYSTEM");

		case StreamType_Probe:
			return print_ascii_stream_(stream, "FILE-IO PROBE");

		case StreamType_StringInput:
			return print_ascii_stream_(stream, "STREAM STRING-INPUT");

		case StreamType_StringOutput:
			return print_ascii_stream_(stream, "STREAM STRING-OUTPUT");

		case StreamType_Synonym:
			return print_ascii_stream_(stream, "SYNONYM-STREAM");

		case StreamType_BroadCast:
			return print_ascii_stream_(stream, "BROADCAST-STREAM");

		case StreamType_Concatenated:
			return print_ascii_stream_(stream, "CONCATENATED-STREAM");

		case StreamType_TwoWay:
			return print_ascii_stream_(stream, "TWO-WAY-STREAM");

		case StreamType_Echo:
			return print_ascii_stream_(stream, "ECHO-STREAM");

		case StreamType_Prompt:
			return print_ascii_stream_(stream, "PROMPT-STREAM");

		case StreamType_Pretty:
			return print_ascii_stream_(stream, "PRETTY-STREAM");

		case StreamType_MemoryInput:
			return print_ascii_stream_(stream, "STREAM MEMORY-INPUT");

		case StreamType_MemoryOutput:
			return print_ascii_stream_(stream, "STREAM MEMORY-OUTPUT");

		case StreamType_MemoryIO:
			return print_ascii_stream_(stream, "STREAM MEMORY-IO");

		case StreamType_Pipe:
			return WriteBody_pipe_stream_(ptr, stream, pos);

		default:
			return print_ascii_stream_(stream, "STREAM");
	}
}

static int WriteCall_stream_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 0, 1, WriteBody_stream_);
}


/*
 *  quote
 */
static int WriteCall_quote_(Execute ptr, addr stream, addr pos)
{
	if (quote_back_p(pos)) {
		getprint_quote(pos, &pos);
		Return(write_char_stream_(stream, '`'));
		return write_print_call_(ptr, stream, pos);
	}
	if (quote_comma_p(pos)) {
		getprint_quote(pos, &pos);
		Return(write_char_stream_(stream, ','));
		return write_print_call_(ptr, stream, pos);
	}
	if (quote_atsign_p(pos)) {
		getprint_quote(pos, &pos);
		Return(print_ascii_stream_(stream, ",@"));
		return write_print_call_(ptr, stream, pos);
	}
	if (quote_dot_p(pos)) {
		getprint_quote(pos, &pos);
		Return(print_ascii_stream_(stream, ",."));
		return write_print_call_(ptr, stream, pos);
	}
	return print_unreadable_object_(ptr, stream, pos, 1, 1, NULL);
}


/*
 *  restart
 */
static int WriteBody_restart_(Execute ptr, addr stream, addr pos)
{
	getname_restart(pos, &pos);
	return write_print_call_(ptr, stream, pos);
}

static int WriteCall_restart_p_(Execute ptr, addr restart, int *ret)
{
	if (restart == Nil)
		return Result(ret, 1);

	return escape_print_(ptr, ret);
}

static int WriteCall_restart_(Execute ptr, addr stream, addr pos)
{
	int check;
	addr restart;

	/* #<RESTART NAME #xADDRESS> */
	getreport_restart(pos, &restart);
	Return(WriteCall_restart_p_(ptr, restart, &check));
	if (check)
		return print_unreadable_object_(ptr, stream, pos, 1, 1, WriteBody_restart_);
	else if (stringp(restart))
		return WriteCall_string_(ptr, stream, restart);
	else
		return funcall1_control_(ptr, &restart, restart, stream, NULL);
}


/*
 *  bitvector
 */
static int WriteCall_bitvector_default_(Execute ptr, addr stream, addr pos)
{
	int value;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(print_ascii_stream_(stream, "#*"));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &value));
		Return(write_char_stream_(stream, value? '1': '0'));
	}

	return 0;
}

static int WriteCall_bitvector_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(array_print_(ptr, &check));
	if (check)
		return WriteCall_bitvector_default_(ptr, stream, pos);

	return print_unreadable_object_(ptr, stream, pos, 1, 1, NULL);
}


/*
 *  paper
 */
static int WriteBody_paper_(Execute ptr, addr stream, addr pos)
{
	char str[8];
	byte c;

	paper_get_type(pos, &c);
	snprintc(str, 8, "%d", (int)c);
	return print_ascii_stream_(stream, str);
}

static int WriteCall_paper_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 1, 1, WriteBody_paper_);
}


/*
 *  byte
 */
static int WriteBody_bytespec_(Execute ptr, addr stream, addr pos)
{
	char data[256];
	struct bytespec_struct *str;

	str = ByteSpecStruct(pos);
	snprintf(data, 256, "SIZE:%zu POSITION:%zu", str->size, str->position);
	return print_ascii_stream_(stream, data);
}

static int WriteCall_bytespec_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 1, 0, WriteBody_bytespec_);
}


/*
 *  table
 */
int write_check_call_(Execute ptr, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_CONS:
			return WriteCheckCall_cons_(ptr, pos);

		case LISPTYPE_VECTOR:
			return WriteCheckCall_vector_(ptr, pos);

		case LISPTYPE_ARRAY:
			return WriteCheckCall_array_(ptr, pos);

		default:
			return 0;
	}
}

static int write_circle_call_(Execute ptr, addr stream, addr pos)
{
	int index;
	calltype_print call;

	index = (int)GetType(pos);
	call = WriteCircleTable[index];
	if (call)
		return (*call)(ptr, stream, pos);
	else
		return (WriteCallTable[index])(ptr, stream, pos);
}

static int write_print_call_(Execute ptr, addr stream, addr pos)
{
	int index = (int)GetType(pos);
	return (WriteCallTable[index])(ptr, stream, pos);
}


/*
 *  write print
 */
int write_default_print_(Execute ptr, addr stream, addr pos)
{
	int check;

	/* normal */
	Return(circle_print_(ptr, &check));
	if (! check)
		return write_print_call_(ptr, stream, pos);
	/* circle */
	if (discard_pretty_stream(stream))
		return 0;
	if (! push_pretty_stream_p(stream)) {
		push_write_object(ptr);
		Return(write_check_call_(ptr, pos));
	}
	return write_circle_call_(ptr, stream, pos);
}

static int write_pretty_print_(Execute ptr, addr stream, addr pos)
{
	addr dispatch;

	Return(pprint_dispatch_print_(ptr, &dispatch));
	Return(find_function_print_dispatch_(ptr, pos, dispatch, &dispatch));
	if (dispatch == Nil)
		return write_default_print_(ptr, stream, pos);
	else
		return funcall1_control_(ptr, &dispatch, dispatch, stream, pos, NULL);
}

int write_print_(Execute ptr, addr stream, addr pos)
{
	int check;

	gchold_push_local(ptr->local, stream);
	Return(pretty_print_(ptr, &check));
	if (check)
		return write_pretty_print_(ptr, stream, pos);
	else
		return write_default_print_(ptr, stream, pos);
}

int princ_print_(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_control(ptr, &control);
	push_escape_print(ptr, 0);
	push_readably_print(ptr, 0);
	(void)write_print_(ptr, stream, pos);
	return pop_control_(ptr, control);
}

int prin1_print_(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_control(ptr, &control);
	push_escape_print(ptr, 1);
	(void)write_print_(ptr, stream, pos);
	return pop_control_(ptr, control);
}

int print_print_(Execute ptr, addr stream, addr pos)
{
	Return(terpri_stream_(stream));
	Return(prin1_print_(ptr, stream, pos));
	return write_char_stream_(stream, ' ');
}

static int pprint_print_call_(Execute ptr, addr stream, addr pos)
{
	push_escape_print(ptr, 1);
	push_pretty_print(ptr, 1);
	Return(terpri_stream_(stream));
	return write_print_(ptr, stream, pos);
}

int pprint_print_(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_control(ptr, &control);
	(void)pprint_print_call_(ptr, stream, pos);
	return pop_control_(ptr, control);
}

static int write_string_heap_call_(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(write_print_(ptr, stream, pos));
	Return(string_stream_heap_(stream, ret));
	close_output_string_stream(stream);

	return 0;
}

int write_string_heap_(Execute ptr, addr *ret, addr pos)
{
	addr control;

	push_control(ptr, &control);
	(void)write_string_heap_call_(ptr, ret, pos);
	return pop_control_(ptr, control);
}

static int write_string_local_call_(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(write_print_(ptr, stream, pos));
	Return(string_stream_local_(ptr->local, stream, ret));
	close_output_string_stream(stream);

	return 0;
}

int write_string_local_(Execute ptr, addr *ret, addr pos)
{
	addr control;

	push_control(ptr, &control);
	(void)write_string_local_call_(ptr, ret, pos);
	return pop_control_(ptr, control);
}

int princ_string_heap_(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(princ_print_(ptr, stream, pos));
	Return(string_stream_heap_(stream, ret));
	close_output_string_stream(stream);

	return 0;
}

int princ_string_local_(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(princ_print_(ptr, stream, pos));
	Return(string_stream_local_(ptr->local, stream, ret));
	close_output_string_stream(stream);

	return 0;
}

int prin1_string_heap_(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(prin1_print_(ptr, stream, pos));
	Return(string_stream_heap_(stream, ret));
	close_output_string_stream(stream);

	return 0;
}

int prin1_string_local_(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(prin1_print_(ptr, stream, pos));
	Return(string_stream_local_(ptr->local, stream, ret));
	close_output_string_stream(stream);

	return 0;
}


/*
 *  initialize
 */
void init_print_write(void)
{
	int i;

	/* error */
	for (i = 0; i < LISPTYPE_SIZE; i++) {
		WriteCallTable[i] = WriteCall_error_;
		WriteCircleTable[i] = NULL;
	}

	/* cons */
	WriteCircleTable[LISPTYPE_CONS] = WriteCircleCall_cons_;
	WriteCallTable[LISPTYPE_CONS] = WriteCall_cons_;
	/* vector */
	WriteCircleTable[LISPTYPE_VECTOR] = WriteCircleCall_vector_;
	WriteCallTable[LISPTYPE_VECTOR] = WriteCall_vector_;
	/* array */
	WriteCircleTable[LISPTYPE_ARRAY] = WriteCircleCall_array_;
	WriteCallTable[LISPTYPE_ARRAY] = WriteCall_array_;
	/* object */
	WriteCallTable[LISPTYPE_NIL] = WriteCall_symbol_;
	WriteCallTable[LISPTYPE_T] = WriteCall_symbol_;
	WriteCallTable[LISPTYPE_TYPE] = WriteCall_type_;
	WriteCallTable[LISPTYPE_CLOS] = WriteCall_clos_;
	WriteCallTable[LISPTYPE_CHARACTER] = WriteCall_character_;
	WriteCallTable[LISPTYPE_STRING] = WriteCall_string_;
	WriteCallTable[LISPTYPE_HASHTABLE] = WriteCall_hashtable_;
	WriteCallTable[LISPTYPE_READTABLE] = WriteCall_system_;
	WriteCallTable[LISPTYPE_SYMBOL] = WriteCall_symbol_;
	WriteCallTable[LISPTYPE_FIXNUM] = WriteCall_fixnum_;
	WriteCallTable[LISPTYPE_BIGNUM] = WriteCall_bignum_;
	WriteCallTable[LISPTYPE_RATIO] = WriteCall_ratio_;
	WriteCallTable[LISPTYPE_SHORT_FLOAT] = WriteCall_error_;
	WriteCallTable[LISPTYPE_SINGLE_FLOAT] = WriteCall_single_float_;
	WriteCallTable[LISPTYPE_DOUBLE_FLOAT] = WriteCall_double_float_;
	WriteCallTable[LISPTYPE_LONG_FLOAT] = WriteCall_long_float_;
	WriteCallTable[LISPTYPE_COMPLEX] = WriteCall_complex_;
	WriteCallTable[LISPTYPE_CONTROL] = WriteCall_system_;
	WriteCallTable[LISPTYPE_CODE] = WriteCall_code_;
	WriteCallTable[LISPTYPE_CALLNAME] = WriteCall_callname_;
	WriteCallTable[LISPTYPE_FUNCTION] = WriteCall_function_;
	WriteCallTable[LISPTYPE_INDEX] = WriteCall_index_;
	WriteCallTable[LISPTYPE_PACKAGE] = WriteCall_package_;
	WriteCallTable[LISPTYPE_RANDOM_STATE] = WriteCall_random_state_;
	WriteCallTable[LISPTYPE_PATHNAME] = WriteCall_pathname_;
	WriteCallTable[LISPTYPE_STREAM] = WriteCall_stream_;
	WriteCallTable[LISPTYPE_QUOTE] = WriteCall_quote_;
	WriteCallTable[LISPTYPE_RESTART] = WriteCall_restart_;
	WriteCallTable[LISPTYPE_EVAL] = WriteCall_system_;
	WriteCallTable[LISPTYPE_ENVIRONMENT] = WriteCall_system_;
	WriteCallTable[LISPTYPE_BITVECTOR] = WriteCall_bitvector_;
	WriteCallTable[LISPTYPE_PRINT_DISPATCH] = WriteCall_system_;
	WriteCallTable[LISPTYPE_PAPER] = WriteCall_paper_;
	WriteCallTable[LISPTYPE_BYTESPEC] = WriteCall_bytespec_;

	WriteCallTable[LISPSYSTEM_CHARACTER2] = WriteCall_system_;
	WriteCallTable[LISPSYSTEM_CHARQUEUE] = WriteCall_system_;
	WriteCallTable[LISPSYSTEM_CHARBIT] = WriteCall_system_;
	WriteCallTable[LISPSYSTEM_SYMSTACK] = WriteCall_system_;
	WriteCallTable[LISPSYSTEM_BITTYPE] = WriteCall_system_;
	WriteCallTable[LISPSYSTEM_READLABEL] = WriteCall_system_;
	WriteCallTable[LISPSYSTEM_READINFO] = WriteCall_system_;
	WriteCallTable[LISPSYSTEM_READTYPE] = WriteCall_system_;
	WriteCallTable[LISPSYSTEM_BITCONS] = WriteCall_system_;
	WriteCallTable[LISPSYSTEM_BITBUFFER] = WriteCall_system_;
	WriteCallTable[LISPSYSTEM_HASHITERATOR] = WriteCall_system_;
	WriteCallTable[LISPSYSTEM_PACKAGEITERATOR] = WriteCall_system_;
	WriteCallTable[LISPSYSTEM_TAGINFO] = WriteCall_system_;
	WriteCallTable[LISPSYSTEM_ARRAY_DIMENSION] = WriteCall_system_;
	WriteCallTable[LISPSYSTEM_ARRAY_GENERAL] = WriteCall_system_;
	WriteCallTable[LISPSYSTEM_ARRAY_SPECIALIZED] = WriteCall_system_;
}


/************************************************************
 *  process.c
 ************************************************************/

/*
 *  defclass lisp-system::process
 */
static void process_defclass_slot(addr slots, size_t n, constindex index)
{
	addr slot, pos;

	slot_heap(&slot);
	GetConstant(index, &pos);
	Check(! symbolp(pos), "type error");
	SetNameSlot(slot, pos);
	SetSlotVector(slots, n, slot);
}

static void process_defclass_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, 14);
	process_defclass_slot(slots, 0, CONSTANT_KEYWORD_PROGRAM);
	process_defclass_slot(slots, 1, CONSTANT_KEYWORD_ARGS);
	process_defclass_slot(slots, 2, CONSTANT_KEYWORD_ENVIRONMENT);
	process_defclass_slot(slots, 3, CONSTANT_KEYWORD_WAIT);
	process_defclass_slot(slots, 4, CONSTANT_KEYWORD_SEARCH);
	process_defclass_slot(slots, 5, CONSTANT_KEYWORD_ELEMENT_TYPE);
	process_defclass_slot(slots, 6, CONSTANT_KEYWORD_EXTERNAL_FORMAT);
	process_defclass_slot(slots, 7, CONSTANT_KEYWORD_DIRECTORY);
	process_defclass_slot(slots, 8, CONSTANT_KEYWORD_INPUT);
	process_defclass_slot(slots, 9, CONSTANT_KEYWORD_OUTPUT);
	process_defclass_slot(slots, 10, CONSTANT_KEYWORD_ERROR);
	process_defclass_slot(slots, 11, CONSTANT_KEYWORD_IF_INPUT_DOES_NOT_EXIST);
	process_defclass_slot(slots, 12, CONSTANT_KEYWORD_IF_OUTPUT_EXISTS);
	process_defclass_slot(slots, 13, CONSTANT_KEYWORD_IF_ERROR_EXISTS);
	slotvector_set_location(slots);
	*ret = slots;
}

static int process_defclass_class_(LocalRoot local, addr slots)
{
	addr name, supers, metaclass, instance;

	/* name */
	GetConst(SYSTEM_PROCESS, &name);
	Check(! symbolp(name), "type error");
	/* supers */
	GetConst(CLOS_STANDARD_OBJECT, &supers);
	CheckType(supers, LISPTYPE_CLOS);
	conscar_heap(&supers, supers);
	/* metaclass */
	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* defclass */
	return clos_stdclass_supers_(local, &instance, metaclass, name, slots, supers);
}

static int process_defclass_(LocalRoot local)
{
	addr pos, slots;

	/* class check */
	GetConst(SYSTEM_PROCESS, &pos);
	clos_find_class_nil(pos, &pos);
	if (pos != Nil)
		return 0;

	/* defclass */
	process_defclass_slots(&slots);
	return process_defclass_class_(local, slots);
}


/*
 *  make-instance
 */
static int process_instance_environment_(addr pos, addr value)
{
	addr list, x;

	if (value == Unbound)
		return 0;
	if (! listp(value))
		return fmte_(":ENVIRONMENT argument ~S must be a list type.", value, NULL);

	/* string check */
	list = value;
	while (list != Nil) {
		Return_getcons(list, &x, &list);
		if (! stringp(x))
			return fmte_(":ENVIRONMENT value ~S must be a string type.", x, NULL);
	}

	return ClosSetConst_(pos, KEYWORD_ENVIRONMENT, value);
}

static int process_instance_wait_(addr pos, addr value)
{
	addr check;

	if (value == Unbound)
		value = T;
	GetConst(KEYWORD_PIPE, &check);
	if (value != T && value == check)
		return fmte_(":WAIT argument ~S must be a T or :PIPE.", value, NULL);

	return ClosSetConst_(pos, KEYWORD_WAIT, value);
}

static int process_instance_search_(addr pos, addr value)
{
	if (value == Unbound)
		value = Nil;
	if (value != Nil)
		value = T;

	return ClosSetConst_(pos, KEYWORD_SEARCH, value);
}

static int process_instance_element_type_(addr pos, addr value)
{
	addr key1, key2;

	GetConst(COMMON_CHARACTER, &key1);
	GetConst(COMMON_UNSIGNED_BYTE, &key2);
	if (value == Unbound) {
		value = key1;
	}
	if (value != key1 && value != key2) {
		return fmte_(":ELEMENT-TYPE argument ~S "
				"must be a CHARACTER or UNSIGNED-BYTE.", value, NULL);
	}

	return ClosSetConst_(pos, KEYWORD_ELEMENT_TYPE, value);
}

static int process_instance_external_format_(addr pos, addr value)
{
	if (value == Unbound) {
		GetConst(KEYWORD_DEFAULT, &value);
	}

	return ClosSetConst_(pos, KEYWORD_EXTERNAL_FORMAT, value);
}

static int process_instance_directory_(Execute ptr, addr pos, addr value)
{
	if (value == Unbound)
		return 0;

	Return(physical_pathname_heap_(ptr, value, &value));
	return ClosSetConst_(pos, KEYWORD_DIRECTORY, value);
}

static int process_instance_input_(Execute ptr, addr pos, addr value)
{
	if (value == Unbound) {
		value = Nil;
	}
	if (value == T) {
		Return(standard_input_stream_(ptr, &value));
	}
	if ((! streamp(value)) && value != Nil)
		return fmte_(":INPUT argument ~S must be a NIL or T or stream.", value, NULL);

	return ClosSetConst_(pos, KEYWORD_INPUT, value);
}

static int process_instance_output_(Execute ptr, addr pos, addr value)
{
	if (value == Unbound) {
		value = Nil;
	}
	if (value == T) {
		Return(standard_output_stream_(ptr, &value));
	}
	if ((! streamp(value)) && value != Nil)
		return fmte_(":OUTPUT argument ~S must be a NIL or T or stream.", value, NULL);

	return ClosSetConst_(pos, KEYWORD_OUTPUT, value);
}

static int process_instance_error_(Execute ptr, addr pos, addr value)
{
	if (value == Unbound) {
		value = Nil;
	}
	if (value == T) {
		Return(error_output_stream_(ptr, &value));
	}
	if ((! streamp(value)) && value != Nil)
		return fmte_(":ERROR argument ~S must be a NIL or T or stream.", value, NULL);

	return ClosSetConst_(pos, KEYWORD_ERROR, value);
}

static int process_instance_if_input_does_not_exist_(addr pos, addr value)
{
	addr key1, key2;

	GetConst(KEYWORD_ERROR, &key1);
	GetConst(KEYWORD_CREATE, &key2);
	if (value == Unbound) {
		value = Nil;
	}
	if (value != Nil && value != key1 && value != key2) {
		return fmte_(":IF-INPUT-DOES-NOT-EXIST argument ~S "
				"must be a :ERROR or :CREATE or NIL.", value, NULL);
	}

	return ClosSetConst_(pos, KEYWORD_IF_INPUT_DOES_NOT_EXIST, value);
}

static int process_instance_if_output_exists_(addr pos, addr value)
{
	addr key1, key2, key3;

	GetConst(KEYWORD_ERROR, &key1);
	GetConst(KEYWORD_SUPERSEDE, &key2);
	GetConst(KEYWORD_APPEND, &key3);
	if (value == Unbound) {
		value = key1;
	}
	if (value != Nil && value != key1 && value != key2 && value != key3) {
		return fmte_(":IF-OUTPUT-EXISTS argument ~S "
				"must be a :ERROR, :SUPERSEDE, :APPEND or NIL.",
				value, NULL);
	}

	return ClosSetConst_(pos, KEYWORD_IF_OUTPUT_EXISTS, value);
}

static int process_instance_if_error_exists_(addr pos, addr value)
{
	addr key1, key2, key3;

	GetConst(KEYWORD_ERROR, &key1);
	GetConst(KEYWORD_SUPERSEDE, &key2);
	GetConst(KEYWORD_APPEND, &key3);
	if (value == Unbound) {
		value = key1;
	}
	if (value != Nil && value != key1 && value != key2 && value != key3) {
		return fmte_(":IF-ERROR-EXISTS argument ~S "
				"must be a :ERROR, :SUPERSEDE, :APPEND or NIL.",
				value, NULL);
	}

	return ClosSetConst_(pos, KEYWORD_IF_ERROR_EXISTS, value);
}

static int process_eq_constant(addr key, addr value, addr *ret, constindex index)
{
	addr check;

	GetConstant(index, &check);
	if (key != check)
		return 0;
	if (*ret == Unbound)
		*ret = value;

	return 1;
}
#define ProcessEqConst(a,b,c,d) { \
	if (process_eq_constant((a),(b),(c),CONSTANT_KEYWORD_##d)) { \
		continue; \
	} \
}

static int process_instance_rest_(Execute ptr, addr pos, addr rest)
{
	addr key, value;
	addr env, wait, search, element, external, directory;
	addr input, output, error, ifinput, ifoutput, iferror;

	env = wait = search = element = external = directory = Unbound;
	input = output = error = ifinput = ifoutput = iferror = Unbound;
	while (rest != Nil) {
		Return_getcons(rest, &key, &rest);
		Return_getcons(rest, &value, &rest);
		ProcessEqConst(key, value, &env, ENVIRONMENT);
		ProcessEqConst(key, value, &wait, WAIT);
		ProcessEqConst(key, value, &search, SEARCH);
		ProcessEqConst(key, value, &element, ELEMENT_TYPE);
		ProcessEqConst(key, value, &external, EXTERNAL_FORMAT);
		ProcessEqConst(key, value, &directory, DIRECTORY);
		ProcessEqConst(key, value, &input, INPUT);
		ProcessEqConst(key, value, &output, OUTPUT);
		ProcessEqConst(key, value, &error, ERROR);
		ProcessEqConst(key, value, &ifinput, IF_INPUT_DOES_NOT_EXIST);
		ProcessEqConst(key, value, &ifoutput, IF_OUTPUT_EXISTS);
		ProcessEqConst(key, value, &iferror, IF_ERROR_EXISTS);
		return fmte_("Invalid key argument ~S.", key, NULL);
	}
	Return(process_instance_environment_(pos, env));
	Return(process_instance_wait_(pos, wait));
	Return(process_instance_search_(pos, search));
	Return(process_instance_element_type_(pos, element));
	Return(process_instance_external_format_(pos, external));
	Return(process_instance_directory_(ptr, pos, directory));
	Return(process_instance_input_(ptr, pos, input));
	Return(process_instance_output_(ptr, pos, output));
	Return(process_instance_error_(ptr, pos, error));
	Return(process_instance_if_input_does_not_exist_(pos, ifinput));
	Return(process_instance_if_output_exists_(pos, ifoutput));
	Return(process_instance_if_error_exists_(pos, iferror));

	return 0;
}

static int process_instance_(Execute ptr, addr var, addr args, addr rest, addr *ret)
{
	addr pos;

	/* make-instance */
	GetConst(SYSTEM_PROCESS, &pos);
	Return(clos_find_class_(pos, &pos));
	Return(clos_instance_heap_(pos, &pos));
	/* setf slot */
	Return(ClosSetConst_(pos, KEYWORD_PROGRAM, var));
	Return(ClosSetConst_(pos, KEYWORD_ARGS, args));
	/* rest */
	Return(process_instance_rest_(ptr, pos, rest));

	return Result(ret, pos);
}


/*
 *  run-process
 */
int run_process_(Execute ptr, addr var, addr args, addr rest, addr *ret)
{
	Return(process_defclass_(ptr->local));
	Return(process_instance_(ptr, var, args, rest, &var));
	return run_process_arch_(ptr, var, ret);
}


/*
 *  dynamic link
 */
static void dlfile_push_process(Execute ptr, addr pos)
{
	addr symbol, list;

	GetConst(SYSTEM_DLFILE, &symbol);
	getspecial_local(ptr, symbol, &list);
	if (list == Unbound)
		list = Nil;
	cons_heap(&list, pos, list);
	setspecial_local(ptr, symbol, list);
}

static int dlfile_remove_process_(Execute ptr, addr *ret)
{
	int update, openp;
	addr symbol, list, pos, check, value;

	GetConst(SYSTEM_DLFILE, &symbol);
	getspecial_local(ptr, symbol, &list);
	value = Nil;
	update = 0;
	if (list == Unbound) {
		update = 1;
		list = Nil;
	}
	while (consp_getcons(list, &pos, &list)) {
		Return(dlfile_check_arch_(pos, &check, &openp));
		if (check == Nil) {
			update = 1;
			continue;
		}
		if (! openp) {
			update = 1;
			continue;
		}
		cons_heap(&value, pos, value);
	}
	nreverse(&value, value);
	if (update) {
		copy_list_heap_unsafe(&list, value);
		setspecial_local(ptr, symbol, list);
	}

	if (ret)
		*ret = value;
	return 0;
}

static int dlfile_open_process_(Execute ptr, addr args, addr *ret)
{
	addr pos, list;
	LocalHold hold;

	if (! consp_getcons(args, &pos, &list)) {
		*ret = Nil;
		return fmte_("Invalid arguments, ~S.", args, NULL);
	}
	if (list != Nil) {
		*ret = Nil;
		return fmte_("Invalid arguments, ~S.", args, NULL);
	}
	hold = LocalHold_array(ptr, 1);
	Return(pathname_designator_heap_(ptr, pos, &pos));
	localhold_set(hold, 0, pos);
	Return(dlopen_arch_(ptr, pos, &pos));
	dlfile_push_process(ptr, pos);
	localhold_end(hold);

	return Result(ret, pos);
}

static int dlfile_close_process_(Execute ptr, addr args, addr *ret)
{
	int openp;
	addr pos, list, check;

	if (! consp_getcons(args, &pos, &list)) {
		*ret = Nil;
		return fmte_("Invalid arguments, ~S.", args, NULL);
	}
	if (list != Nil) {
		*ret = Nil;
		return fmte_("Invalid arguments, ~S.", args, NULL);
	}
	Return(dlfile_check_arch_(pos, &check, &openp));
	if (check == Nil) {
		*ret = Nil;
		return fmte_("Invalid arguments, ~S.", pos, NULL);
	}
	if (! openp) {
		Return(dlfile_remove_process_(ptr, NULL));
		return Result(ret, Nil);
	}
	else {
		Return(dlclose_arch_(ptr, pos, ret));
		return dlfile_remove_process_(ptr, NULL);
	}
}

static int dlfile_type_process_(addr args, enum CallBind_index *ret)
{
	addr pos, list;

	*ret = CallBind_extend_rest;
	if (args == Nil)
		return 0;
	if (! consp_getcons(args, &pos, &list))
		return fmte_("Invalid arguments, ~S.", args, NULL);
	if (list != Nil)
		return fmte_("Invalid arguments, ~S.", args, NULL);
	Return(string_designator_heap_(&pos, pos, NULL));
	return process_calltype_(pos, ret);
}

static int dlfile_call_process_(Execute ptr, addr args, addr *ret)
{
	int openp;
	enum CallBind_index type;
	addr list, paper, check, name;

	/* paper */
	if (! consp_getcons(args, &paper, &list)) {
		*ret = Nil;
		return fmte_("Invalid arguments, ~S.", args, NULL);
	}
	Return(dlfile_check_arch_(paper, &check, &openp));
	if (check == Nil) {
		*ret = Nil;
		return fmte_("Invalid arguments, ~S.", paper, NULL);
	}
	if (! openp) {
		*ret = Nil;
		return fmte_("dlfile is already closed, ~S.", paper, NULL);
	}

	/* name */
	if (! consp_getcons(list, &name, &list)) {
		*ret = Nil;
		return fmte_("Invalid arguments, ~S.", args, NULL);
	}
	Return(string_designator_heap_(&name, name, NULL));

	/* type */
	Return(dlfile_type_process_(list, &type));
	return dlsym_arch_(ptr, paper, name, type, ret);
}

static int dlfile_list_process_(Execute ptr, addr args, addr *ret)
{
	if (args != Nil) {
		*ret = Nil;
		return fmte_("Invalid arguments, ~S.", args, NULL);
	}

	return dlfile_remove_process_(ptr, ret);
}

static int dlfile_info_process_(Execute ptr, addr args, addr *ret, addr *retp)
{
	int openp;
	addr paper, check;

	Return_getcons(args, &paper, &args);
	Return(dlfile_check_arch_(paper, &check, &openp));
	if (check == Nil)
		*retp = Nil;
	else
		*retp = openp? T: Nil;

	return Result(ret, check);
}

int dlfile_process_(Execute ptr, addr type, addr args, addr *ret, addr *retp)
{
	int check;

	*retp = Unbound;

	/* :open */
	Return(string_designator_equalp_char_(type, "OPEN", &check));
	if (check)
		return dlfile_open_process_(ptr, args, ret);

	/* :close */
	Return(string_designator_equalp_char_(type, "CLOSE", &check));
	if (check)
		return dlfile_close_process_(ptr, args, ret);

	/* :call */
	Return(string_designator_equalp_char_(type, "CALL", &check));
	if (check)
		return dlfile_call_process_(ptr, args, ret);

	/* :list */
	Return(string_designator_equalp_char_(type, "LIST", &check));
	if (check)
		return dlfile_list_process_(ptr, args, ret);

	/* :info */
	Return(string_designator_equalp_char_(type, "INFO", &check));
	if (check)
		return dlfile_info_process_(ptr, args, ret, retp);

	/* error */
	*ret = Nil;
	return fmte_("Invalid argument, ~S.", type, NULL);
}

int dlcall_process_(Execute ptr, addr paper, addr args)
{
	return dlcall_arch_(ptr, paper, args);
}


/************************************************************
 *  process_arch.c
 ************************************************************/

#if defined(LISP_UNIX) && defined(LISP_DYNAMIC_LINK)

#include <dlfcn.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

static int run_process_utf8_(LocalRoot local, addr pos, char **ret)
{
	addr data;
	char *str;

	Return(UTF8_buffer_clang_(local, &data, pos));
	if (data == Unbound) {
		*ret = NULL;
		return fmte_("Invalid UTF8 format ~S.", pos, NULL);
	}
	posbody(data, (addr *)&str);

	return Result(ret, str);
}

static int run_process_list_utf8_(LocalRoot local, addr var, addr list, char ***ret)
{
	char **array;
	addr pos;
	size_t size, i;

	Return(length_list_safe_(list, &size));
	size++;
	array = (char **)lowlevel_local(local, (size + 1) * sizeoft(char *));
	Return(run_process_utf8_(local, var, &(array[0])));
	for (i = 1; i < size; i++) {
		GetCons(list, &pos, &list);
		Return(run_process_utf8_(local, pos, &(array[i])));
	}
	array[i] = 0;

	return Result(ret, array);
}

static int run_process_unix_(Execute ptr, addr var, addr args, addr *ret)
{
	int status;
	char *name;
	char **list;
	pid_t pid;
	LocalRoot local;

	local = ptr->local;
	if (! listp(args))
		conscar_local(local, &args, args);
	Return(run_process_utf8_(local, var, &name));
	Return(run_process_list_utf8_(local, var, args, &list));
	pid = fork();
	if (pid == -1)
		return fmte_("fork error", NULL);
	if (pid == 0) {
		/* child process */
		(void)execvp(name, list);
		return fmte_("execvp error", NULL);
	}

	/* wait */
	waitpid(pid, &status, 0);
	fixnum_heap(ret, (fixnum)status); /* heap */

	return 0;
}

int run_process_arch_(Execute ptr, addr instance, addr *ret)
{
	addr var, args;

	Return(ClosGetConst_(instance, KEYWORD_PROGRAM, &var));
	Return(ClosGetConst_(instance, KEYWORD_ARGS, &args));
	return run_process_unix_(ptr, var, args, ret);
}


/*
 *  dlfile
 */
#define LISP_PROCESS_FILE		(('S' << 8U) | 'O')
#define LISP_PROCESS_CALL		(('S' << 16U) | ('Y' << 8U) | 'M')

struct dlfile_struct {
	uint32_t magic;
	int openp;
	void *handle;
};

struct dlcall_struct {
	uint32_t magic;
	struct callbind_struct call;
};

int dlfile_check_arch_(addr pos, addr *ret, int *openp)
{
	struct dlfile_struct str;
	size_t size;

	*openp = 0;
	if (! paperp(pos))
		return Result(ret, Nil);
	paper_get_memory(pos, 0, sizeof(struct dlfile_struct), &str, &size);
	if (size != sizeof(struct dlfile_struct))
		return Result(ret, Nil);
	if (str.magic != LISP_PROCESS_FILE)
		return Result(ret, Nil);

	*openp = (str.openp != 0);
	paper_get_array(pos, 1, ret);
	return 0;
}

static void *dlopen_open_handle_(const char *utf8)
{
	int callp;
	void *handle;
	void *proc;
	lisp_dlfile_array array;
	int (*call)(lisp_dlfile_array);

	/* open */
	handle = dlopen(utf8, RTLD_LAZY);
	if (handle == NULL)
		return NULL;
	callp = 0;
	lisp_dlfile_make(array);

	/* lisp_dlfile_main */
	proc = dlsym(handle, "lisp_dlfile_main");
	if (proc) {
		call = (int (*)(lisp_dlfile_array))proc;
		if ((*call)(array))
			goto error;
		callp = 1;
	}

	/* lisp_somain */
	proc = dlsym(handle, "lisp_somain");
	if (proc) {
		call = (int (*)(lisp_dlfile_array))proc;
		if ((*call)(array))
			goto error;
		callp = 1;
	}

	/* error check */
	if (callp == 0)
		goto error;
	return handle;

error:
	dlclose(handle);
	return NULL;
}

int dlopen_arch_(Execute ptr, addr pos, addr *ret)
{
	addr file, paper;
	const char *utf8;
	void *handle;
	LocalRoot local;
	LocalStack stack;
	struct dlfile_struct str;

	/* dlopen */
	local = ptr->local;
	push_local(local, &stack);
	Return(physical_pathname_local_(ptr, pos, &pos));
	Return(name_pathname_heap_(ptr, pos, &file));
	gchold_push_local(local, file);

	Return(run_process_utf8_(local, file, (char **)&utf8));
	handle = dlopen_open_handle_(utf8);
	rollback_local(local, stack);
	if (handle == NULL) {
		*ret = Nil;
		return fmte_("dlopen error, ~S.", file, NULL);
	}

	/* paper */
	cleartype(str);
	str.magic = LISP_PROCESS_FILE;
	str.openp = 1;
	str.handle = handle;
	Return(paper_arraybody_heap_(&paper, 2, sizeof(struct dlfile_struct)));
	paper_set_array(paper, 0, file);
	paper_set_array(paper, 1, pos);
	paper_set_memory(paper, 0, sizeof(struct dlfile_struct), &str, NULL);
	return Result(ret, paper);
}

int dlclose_arch_(Execute ptr, addr pos, addr *ret)
{
	addr file;
	struct dlfile_struct *str;

	paper_ptr_body_unsafe(pos, (void **)&str);
	if (! str->openp)
		return Result(ret, Nil); /* already closed */

	if (dlclose(str->handle) != 0) {
		*ret = Nil;
		paper_get_array(pos, 0, &file);
		return fmte_("dlclose error, ~S.", file, NULL);
	}
	str->openp = 0;
	return Result(ret, T);
}

int dlsym_arch_(Execute ptr, addr pos, addr name, enum CallBind_index type, addr *ret)
{
	addr file, paper;
	const char *utf8;
	void *sym;
	LocalRoot local;
	LocalStack stack;
	struct dlfile_struct str;
	struct dlcall_struct call;

	/* dlfile */
	paper_get_memory(pos, 0, sizeof(struct dlfile_struct), &str, NULL);
	if (! str.openp) {
		*ret = Nil;
		paper_get_array(pos, 0, &file);
		return fmte_("dlfile is already closed, ~S.", file, NULL);
	}

	/* pointer */
	local = ptr->local;
	push_local(local, &stack);
	Return(run_process_utf8_(local, name, (char **)&utf8));
	sym = dlsym(str.handle, utf8);
	rollback_local(local, stack);
	if (sym == NULL) {
		*ret = Nil;
		return fmte_("dlsym error, ~S.", name, NULL);
	}

	/* call */
	cleartype(call);
	call.magic = LISP_PROCESS_CALL;
	call.call.type = type;
	call.call.call.pvoid = (void *)sym;
	Return(paper_arraybody_heap_(&paper, 2, sizeof(struct dlcall_struct)));
	paper_set_array(paper, 0, pos);
	paper_set_array(paper, 1, name);
	paper_set_memory(paper, 0, sizeof(struct dlcall_struct), &call, NULL);
	return Result(ret, paper);
}


/*
 *  dlcall
 */
static int dlcall_arch_callbind_(Execute ptr,
		addr name, addr args, struct callbind_struct *bind)
{
	addr control;

	push_control(ptr, &control);
	SetControl(ptr->control, Control_Cons, args);
	SetControl(ptr->control, Control_ConsTail, Nil);
	(void)call_callbind_function_(ptr, name, bind);
	return pop_control_(ptr, control);
}

int dlcall_arch_(Execute ptr, addr pos, addr args)
{
	addr dlfile, name;
	struct dlfile_struct *str;
	struct dlcall_struct *call;

	/* sym */
	paper_ptr_body_unsafe(pos, (void **)&call);
	if (call->magic != LISP_PROCESS_CALL)
		return fmte_("Invalid dlsym object, ~S.", pos, NULL);
	paper_get_array(pos, 0, &dlfile);
	paper_get_array(pos, 1, &name);

	/* dlfile */
	paper_ptr_body_unsafe(dlfile, (void **)&str);
	if (! str->openp)
		return fmte_("dlfile ~S is already closed.", dlfile, NULL);

	/* call */
	return dlcall_arch_callbind_(ptr, name, args, &(call->call));
}

#elif defined(LISP_WINDOWS)

#include <windows.h>

static int run_process_utf8_(LocalRoot local, addr pos, char **ret)
{
	addr data;
	char *str;

	Return(UTF8_buffer_clang_(local, &data, pos));
	if (data == Unbound) {
		*ret = NULL;
		return fmte_("Invalid UTF8 format ~S.", pos, NULL);
	}
	posbody(data, (addr *)&str);

	return Result(ret, str);
}


static int run_process_utf16_(LocalRoot local, addr pos, wchar_t **ret)
{
	addr data;
	wchar_t *str;

	Return(UTF16_buffer_clang_(local, &data, pos));
	if (data == Unbound) {
		*ret = NULL;
		return fmte_("Invalid UTF16 format ~S.", pos, NULL);
	}
	posbody(data, (addr *)&str);

	return Result(ret, str);
}

static int run_process_delimited_(LocalRoot local, addr *ret, addr x, unicode z)
{
	size_t size, a, b;
	addr y;
	unicode c;

	string_length(x, &size);
	strvect_local(local, &y, size + 2);
	a = b = 0;
	Return(strvect_setc_(y, b++, z));
	while (a < size) {
		Return(string_getc_(x, a++, &c));
		Return(strvect_setc_(y, b++, c));
	}
	Return(strvect_setc_(y, b, z));

	return Result(ret, y);
}

static int run_process_windows_pathname_(LocalRoot local,
		addr pos, addr *ret, size_t *rsize)
{
	int space;
	unicode c;
	size_t size, i;

	/* space check */
	string_length(pos, &size);
	space = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (c == ' ')
			space = 1;
		if (c == '"') {
			return fmte_("Don't include character #\\\" "
					"in Windows pathname ~S.", pos, NULL);
		}
	}

	/* encode */
	if (space) {
		Return(run_process_delimited_(local, ret, pos, '"'));
		*rsize = size + 2;
	}
	else {
		*ret = pos;
		*rsize = size;
	}

	return 0;
}

static int run_process_list_utf16_(LocalRoot local, addr var, addr list, wchar_t **ret)
{
	int first;
	addr root, x, y;
	unicode c;
	size_t size, value, a, b;

	Return(run_process_windows_pathname_(local, var, &x, &size));
	conscar_local(local, &root, x);
	while (list != Nil) {
		Return_getcons(list, &x, &list);
		Return(run_process_windows_pathname_(local, x, &x, &value));
		size += value + 1;
		cons_local(local, &root, x, root);
	}
	nreverse(&root, root);

	strvect_local(local, &y, size);
	a = 0;
	for (first = 1; root != Nil; first = 0) {
		GetCons(root, &x, &root);
		if (first == 0) {
			Return(strvect_setc_(y, a++, ' '));
		}
		strvect_length(x, &value);
		for (b = 0; b < value; b++) {
			strvect_getc(x, b, &c);
			Return(strvect_setc_(y, a++, c));
		}
	}

	return run_process_utf16_(local, y, ret);
}

static int run_process_windows_(Execute ptr, addr var, addr args, addr *ret)
{
	wchar_t *list;
	STARTUPINFOW sinfo;
	PROCESS_INFORMATION pinfo;
	HANDLE hProcess, hThread;
	DWORD status;
	LocalRoot local;

	local = ptr->local;
	if (! listp(args))
		conscar_local(local, &args, args);
	Return(run_process_list_utf16_(local, var, args, &list));
	cleartype(sinfo);
	cleartype(pinfo);
	sinfo.cb = sizeof(sinfo);
	if (! CreateProcessW(NULL, list, NULL, NULL,
				FALSE, 0, NULL, NULL, &sinfo, &pinfo)) {
		return fmte_("Cannot run process ~S.", var, NULL);
	}
	hProcess = pinfo.hProcess;
	hThread = pinfo.hThread;

	/* wait */
	WaitForSingleObject(hProcess, INFINITE);
	if (! GetExitCodeProcess(hProcess, &status)) {
		(void)CloseHandle(hProcess);
		(void)CloseHandle(hThread);
		return fmte_("GetExitCodeProcess error.", NULL);
	}
	fixnum_heap(ret, (fixnum)status); /* heap */

	/* Close */
	(void)CloseHandle(hProcess);
	(void)CloseHandle(hThread);

	return 0;
}

int run_process_arch_(Execute ptr, addr instance, addr *ret)
{
	addr var, args;

	Return(ClosGetConst_(instance, KEYWORD_PROGRAM, &var));
	Return(ClosGetConst_(instance, KEYWORD_ARGS, &args));
	return run_process_windows_(ptr, var, args, ret);
}


/*
 *  dlfile
 */
#define LISP_PROCESS_FILE		(('D' << 16U) | ('L' << 8U) | 'L')
#define LISP_PROCESS_CALL		(('F' << 16U) | ('A' << 8U) | 'R')

struct dlfile_struct {
	uint32_t magic;
	int openp;
	HMODULE handle;
};

struct dlcall_struct {
	uint32_t magic;
	struct callbind_struct call;
};

int dlfile_check_arch_(addr pos, addr *ret, int *openp)
{
	struct dlfile_struct str;
	size_t size;

	*openp = 0;
	if (! paperp(pos))
		return Result(ret, Nil);
	paper_get_memory(pos, 0, sizeof(struct dlfile_struct), &str, &size);
	if (size != sizeof(struct dlfile_struct))
		return Result(ret, Nil);
	if (str.magic != LISP_PROCESS_FILE)
		return Result(ret, Nil);

	*openp = (str.openp != 0);
	paper_get_array(pos, 1, ret);
	return 0;
}

static HMODULE dlopen_open_handle_(const WCHAR *utf16)
{
	int callp;
	HMODULE handle;
	FARPROC proc;
	lisp_dlfile_array array;
	int (*call)(lisp_dlfile_array);

	/* open */
	handle = LoadLibraryW(utf16);
	if (handle == NULL)
		return NULL;
	callp = 0;
	lisp_dlfile_make(array);

	/* lisp_dlfile_main */
	proc = GetProcAddress(handle, "lisp_dlfile_main");
	if (proc) {
		call = (int (*)(lisp_dlfile_array))proc;
		if ((*call)(array))
			goto error;
		callp = 1;
	}

	/* lisp_dllmain */
	proc = GetProcAddress(handle, "lisp_dllmain");
	if (proc) {
		call = (int (*)(lisp_dlfile_array))proc;
		if ((*call)(array))
			goto error;
		callp = 1;
	}

	/* error check */
	if (callp == 0)
		goto error;
	return handle;

error:
	FreeLibrary(handle);
	return NULL;
}

int dlopen_arch_(Execute ptr, addr pos, addr *ret)
{
	addr file, paper;
	const wchar_t *utf16;
	HMODULE handle;
	LocalRoot local;
	LocalStack stack;
	struct dlfile_struct str;

	/* dlopen */
	local = ptr->local;
	push_local(local, &stack);
	Return(physical_pathname_local_(ptr, pos, &pos));
	Return(name_pathname_heap_(ptr, pos, &file));
	gchold_push_local(local, file);

	Return(run_process_utf16_(local, file, (wchar_t **)&utf16));
	handle = dlopen_open_handle_(utf16);
	rollback_local(local, stack);
	if (handle == NULL) {
		*ret = Nil;
		return fmte_("LoadLibraryW error, ~S.", file, NULL);
	}

	/* paper */
	cleartype(str);
	str.magic = LISP_PROCESS_FILE;
	str.openp = 1;
	str.handle = handle;
	Return(paper_arraybody_heap_(&paper, 2, sizeof(struct dlfile_struct)));
	paper_set_array(paper, 0, file);
	paper_set_array(paper, 1, pos);
	paper_set_memory(paper, 0, sizeof(struct dlfile_struct), &str, NULL);
	return Result(ret, paper);
}

int dlclose_arch_(Execute ptr, addr pos, addr *ret)
{
	addr file;
	struct dlfile_struct *str;

	paper_ptr_body_unsafe(pos, (void **)&str);
	if (! str->openp)
		return Result(ret, Nil); /* already closed */

	if (FreeLibrary(str->handle) == 0) {
		*ret = Nil;
		paper_get_array(pos, 0, &file);
		return fmte_("FreeLibrary error, ~S.", file, NULL);
	}
	str->openp = 0;
	return Result(ret, T);
}

int dlsym_arch_(Execute ptr, addr pos, addr name, enum CallBind_index type, addr *ret)
{
	addr file, paper;
	const char *utf8;
	FARPROC sym;
	LocalRoot local;
	LocalStack stack;
	struct dlfile_struct str;
	struct dlcall_struct call;

	/* dlfile */
	paper_get_memory(pos, 0, sizeof(struct dlfile_struct), &str, NULL);
	if (! str.openp) {
		*ret = Nil;
		paper_get_array(pos, 0, &file);
		return fmte_("dlfile is already closed, ~S.", file, NULL);
	}

	/* pointer */
	local = ptr->local;
	push_local(local, &stack);
	Return(run_process_utf8_(local, name, (char **)&utf8));
	sym = GetProcAddress(str.handle, utf8);
	rollback_local(local, stack);
	if (sym == NULL) {
		*ret = Nil;
		return fmte_("GetProcAddress error, ~S.", name, NULL);
	}

	/* call */
	cleartype(call);
	call.magic = LISP_PROCESS_CALL;
	call.call.type = type;
	call.call.call.pvoid = (void *)sym;
	Return(paper_arraybody_heap_(&paper, 2, sizeof(struct dlcall_struct)));
	paper_set_array(paper, 0, pos);
	paper_set_array(paper, 1, name);
	paper_set_memory(paper, 0, sizeof(struct dlcall_struct), &call, NULL);
	return Result(ret, paper);
}


/*
 *  dlcall
 */
static int dlcall_arch_callbind_(Execute ptr,
	addr name, addr args, struct callbind_struct *bind)
{
	addr control;

	push_control(ptr, &control);
	SetControl(ptr->control, Control_Cons, args);
	SetControl(ptr->control, Control_ConsTail, Nil);
	(void)call_callbind_function_(ptr, name, bind);
	return pop_control_(ptr, control);
}

int dlcall_arch_(Execute ptr, addr pos, addr args)
{
	addr dlfile, name;
	struct dlfile_struct *str;
	struct dlcall_struct *call;

	/* sym */
	paper_ptr_body_unsafe(pos, (void **)&call);
	if (call->magic != LISP_PROCESS_CALL)
		return fmte_("Invalid dlsym object, ~S.", pos, NULL);
	paper_get_array(pos, 0, &dlfile);
	paper_get_array(pos, 1, &name);

	/* dlfile */
	paper_ptr_body_unsafe(dlfile, (void **)&str);
	if (! str->openp)
		return fmte_("dlfile ~S is already closed.", dlfile, NULL);

	/* call */
	return dlcall_arch_callbind_(ptr, name, args, &(call->call));
}
#else
int run_process_arch_(Execute ptr, addr instance, addr *ret)
{
	return fmte_("This implementation does not support RUN-PROGRAM.", NULL);
}

int dlfile_check_arch_(addr pos, addr *ret, int *openp)
{
	*ret = 0;
	*openp = 0;
	return fmte_("This implementation does not support DLFILE.", NULL);
}

int dlopen_arch_(Execute ptr, addr pos, addr *ret)
{
	*ret = Nil;
	return fmte_("This implementation does not support DLFILE.", NULL);
}

int dlclose_arch_(Execute ptr, addr pos, addr *ret)
{
	*ret = Nil;
	return fmte_("This implementation does not support DLFILE.", NULL);
}

int dlsym_arch_(Execute ptr, addr pos, addr name, enum CallBind_index type, addr *ret)
{
	*ret = Nil;
	return fmte_("This implementation does not support DLFILE.", NULL);
}

int dlcall_arch_(Execute ptr, addr pos, addr args)
{
	return fmte_("This implementation does not support DLCALL.", NULL);
}
#endif


/************************************************************
 *  process_calltype.c
 ************************************************************/

#define ProcessCallType(pos, ret, name) { \
	int __check; \
	Return(process_calltype_p_(pos, #name, &__check)); \
	if (__check) { \
		return Result(ret, CallBind_##name); \
	} \
}

static int process_calltype_equalp_(addr pos, const byte *str, int *ret)
{
	unicode x, y;
	size_t size, i;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &x));
		y = (unicode)str[i];
		if (y == 0)
			return Result(ret, 0);
		if (x == '-' && y == '_')
			continue;
		x = toUpperUnicode(x);
		y = toUpperUnicode(y);
		if (x != y)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int process_calltype_p_(addr pos, const char *str, int *ret)
{
	if (symbolp(pos))
		GetNameSymbol(pos, &pos);
	if (stringp(pos))
		return process_calltype_equalp_(pos, (const byte *)str, ret);

	return Result(ret, 0);
}

int process_calltype_(addr pos, enum CallBind_index *ret)
{
	ProcessCallType(pos, ret, error);
	ProcessCallType(pos, ret, code);
	ProcessCallType(pos, ret, macro);
	ProcessCallType(pos, ret, none);
	ProcessCallType(pos, ret, any);
	ProcessCallType(pos, ret, empty);
	ProcessCallType(pos, ret, rest);
	ProcessCallType(pos, ret, dynamic);
	ProcessCallType(pos, ret, var1);
	ProcessCallType(pos, ret, var2);
	ProcessCallType(pos, ret, var3);
	ProcessCallType(pos, ret, var4);
	ProcessCallType(pos, ret, var5);
	ProcessCallType(pos, ret, var6);
	ProcessCallType(pos, ret, opt1);
	ProcessCallType(pos, ret, opt2);
	ProcessCallType(pos, ret, opt3);
	ProcessCallType(pos, ret, opt4);
	ProcessCallType(pos, ret, opt5);
	ProcessCallType(pos, ret, var1opt1);
	ProcessCallType(pos, ret, var2opt1);
	ProcessCallType(pos, ret, var3opt1);
	ProcessCallType(pos, ret, var4opt1);
	ProcessCallType(pos, ret, var5opt1);
	ProcessCallType(pos, ret, var1opt2);
	ProcessCallType(pos, ret, var2opt2);
	ProcessCallType(pos, ret, var2opt3);
	ProcessCallType(pos, ret, var1rest);
	ProcessCallType(pos, ret, var2rest);
	ProcessCallType(pos, ret, var3rest);
	ProcessCallType(pos, ret, var4rest);
	ProcessCallType(pos, ret, opt1rest);
	ProcessCallType(pos, ret, var1dynamic);
	ProcessCallType(pos, ret, var2dynamic);
	ProcessCallType(pos, ret, var3dynamic);
	ProcessCallType(pos, ret, var4dynamic);
	ProcessCallType(pos, ret, opt1dynamic);

	ProcessCallType(pos, ret, extend_macro);
	ProcessCallType(pos, ret, extend_rest);
	ProcessCallType(pos, ret, extend_dynamic);
	ProcessCallType(pos, ret, extend_any);
	ProcessCallType(pos, ret, extend_empty);
	ProcessCallType(pos, ret, extend_var1);
	ProcessCallType(pos, ret, extend_var2);
	ProcessCallType(pos, ret, extend_var3);
	ProcessCallType(pos, ret, extend_var4);
	ProcessCallType(pos, ret, extend_var5);
	ProcessCallType(pos, ret, extend_var6);
	ProcessCallType(pos, ret, extend_opt1);
	ProcessCallType(pos, ret, extend_opt2);
	ProcessCallType(pos, ret, extend_opt3);
	ProcessCallType(pos, ret, extend_var1opt1);
	ProcessCallType(pos, ret, extend_var1opt2);
	ProcessCallType(pos, ret, extend_var1opt3);
	ProcessCallType(pos, ret, extend_var2opt1);
	ProcessCallType(pos, ret, extend_var2opt2);
	ProcessCallType(pos, ret, extend_var2opt3);
	ProcessCallType(pos, ret, extend_var3opt1);
	ProcessCallType(pos, ret, extend_var3opt2);
	ProcessCallType(pos, ret, extend_var3opt3);
	ProcessCallType(pos, ret, extend_var1rest);
	ProcessCallType(pos, ret, extend_var2rest);
	ProcessCallType(pos, ret, extend_var3rest);
	ProcessCallType(pos, ret, extend_var1dynamic);
	ProcessCallType(pos, ret, extend_var2dynamic);
	ProcessCallType(pos, ret, extend_var3dynamic);

	/* error */
	*ret = CallBind_error;
	return fmte_("Invalid calltype, ~S.", pos, NULL);
}


/************************************************************
 *  process_ed.c
 ************************************************************/

static int find_environment_char_(Execute ptr, const char *key, addr *ret)
{
	addr pos;

	GetConst(SYSTEM_SPECIAL_ENVIRONMENT, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Unbound)
		return Result(ret, Unbound);
	else
		return find_char_hashtable_(pos, key, ret);
}

static int find_ed_program_(Execute ptr, addr *ret)
{
	addr pos;

	/* *ed-program* */
	GetConst(SYSTEM_ED_PROGRAM, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos != Unbound)
		return Result(ret, pos);

	/* *environment* */
	return find_environment_char_(ptr, "EDITOR", ret);
}

int ed_process_(Execute ptr, addr file)
{
	addr call, status, rest;

	Return(find_ed_program_(ptr, &call));
	if (call == Unbound)
		strvect_char_heap(&call, LISP_ED_PROCESS_DEFAULT);

	/* rest */
	GetConst(KEYWORD_SEARCH, &rest);
	list_heap(&rest, rest, T, NULL);

	return run_process_(ptr, call, file, rest, &status);
}


/************************************************************
 *  prompt.c
 ************************************************************/

static void symbol_prompt(addr *ret)
{
	GetConst(SYSTEM_PROMPT, ret);
}

static void prompt_heap(addr *ret, addr value, PromptMode mode)
{
	addr pos;
	struct prompt_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_PROMPT, 1, sizeoft(struct prompt_struct));
	str = PtrPromptStruct(pos);
	str->mode = mode;
	SetArraySS(pos, 0, value);
	*ret = pos;
}

void push_prompt(Execute ptr, addr value, PromptMode mode)
{
	addr symbol, pos;

	symbol_prompt(&symbol);
	prompt_heap(&pos, value, mode);
	pushspecial_control(ptr, symbol, pos);
}

void push_prompt_eval_loop(Execute ptr)
{
	addr value;
	strvect_char_heap(&value, "* ");
	push_prompt(ptr, value, prompt_eval);
}

void get_prompt(Execute ptr, addr *value, PromptMode *mode)
{
	addr pos;

	symbol_prompt(&pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Unbound) {
		*value = Nil;
		*mode = prompt_eval;
		return;
	}
	if (GetType(pos) != LISPSYSTEM_PROMPT) {
		*value = Nil;
		*mode = prompt_eval;
		return;
	}

	GetArraySS(pos, 0, value);
	*mode = PtrPromptStruct(pos)->mode;
}

void getvalue_prompt(Execute ptr, addr *ret)
{
	PromptMode ignore;
	get_prompt(ptr, ret, &ignore);
}

void getmode_prompt(Execute ptr, PromptMode *ret)
{
	addr ignore;
	get_prompt(ptr, &ignore, ret);
}


/*
 *  read_prompt
 */
int read_prompt_(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control, symbol;

	/* *prompt-reading* */
	GetConst(SYSTEM_PROMPT_READING, &symbol);

	/* read-stream */
	push_control(ptr, &control);
	pushspecial_control(ptr, symbol, Nil);
	(void)read_stream_(ptr, stream, result, ret);
	return pop_control_(ptr, control);
}

int read_error_prompt_(Execute ptr, addr stream, addr *ret)
{
	int check;

	Return(read_prompt_(ptr, stream, &check, ret));
	if (check)
		return call_end_of_file_(ptr, stream);

	return 0;
}


/************************************************************
 *  prompt_arch.c
 ************************************************************/

#ifdef LISP_PROMPT_TERME

static int input_prompt_reading_(Execute ptr, addr *ret, PromptMode *mode)
{
	int errorp;
	addr symbol, str, value;
	size_t size, i;

	/* prompt */
	get_prompt(ptr, &str, mode);

	/* normal */
	GetConst(SYSTEM_PROMPT_READING, &symbol);
	getspecial_local(ptr, symbol, &value);

	/*  Unbound -> output  (ignore)
	 *  Nil     -> output  (first)
	 *  T       -> space   (reading)
	 */
	if (value == Unbound)
		return Result(ret, str);

	/* first */
	if (value == Nil)
		return Result(ret, str);

	/* reading */
	Return(eastasian_length_(str, &size, &errorp));
	if (errorp)
		string_length(str, &size);
	strvect_heap(&str, size);
	for (i = 0; i < size; i++)
		strvect_setc_unsafe(str, i, ' ');

	return Result(ret, str);
}

static int check_prompt_reading_p(addr pos)
{
	int check;
	unicode c;
	size_t size, i;

	if (! strvectp(pos))
		return 1;  /* EOF */
	strvect_length(pos, &size);
	check = 0;
	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &c);
		if (c == 0x0A || c == 0x0D)
			continue;
		if (! (c == ' ' || c == '\t')) {
			check = 1;
			break;
		}
	}

	return check;
}

static int check_prompt_reading_(Execute ptr, addr pos)
{
	addr symbol, value;

	GetConst(SYSTEM_PROMPT_READING, &symbol);
	getspecial_local(ptr, symbol, &value);
	if (value == Unbound)
		return 0;

	/* value == Nil */
	if (value == Nil) {
		if (! check_prompt_reading_p(pos))
			return 0;
	}

	/* value != Nil */
	setspecial_local(ptr, symbol, T);
	return 0;
}

int input_prompt_(Execute ptr, addr *ret)
{
	LocalHold hold;
	PromptMode mode;
	addr pos;

	hold = LocalHold_array(ptr, 2);
	Return(input_prompt_reading_(ptr, &pos, &mode));
	localhold_set(hold, 0, pos);
	Return(prompt_terme_(ptr, pos, mode));
	Return(readline_terme_(ptr, &pos));
	localhold_set(hold, 1, pos);
	Return(check_prompt_reading_(ptr, pos));
	localhold_end(hold);

	return Result(ret, pos);
}

int clear_prompt_(void)
{
	return clear_terme_(Execute_Thread);
}

#endif

#ifdef LISP_PROMPT_DISABLE

int input_prompt_(Execute ptr, addr *ret)
{
	return fmte_("input-prompt is not supported.", NULL);
}

int clear_prompt_(void)
{
	return 0;
}

#endif

#ifdef LISP_PROMPT_READLINE

/* readline */
#ifdef LISP_PROMPT_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

/* editline */
#ifdef LISP_PROMPT_EDITLINE
#if defined(HAVE_EDITLINE_EDITLINE_H)
#include <editline/editline.h>
#include <editline/history.h>
#elif defined(HAVE_EDITLINE_READLINE_H)
#include <editline/readline.h>
#include <editline/history.h>
#elif defined(HAVE_EDIT_READLINE_H)
#include <edit/readline.h>
#include <edit/history.h>
#elif defined(HAVE_EDIT_READLINE_READLINE_H)
#include <edit/readline/readline.h>
#include <edit/readline/history.h>
#else
#include <edit/readline/readline.h>
#include <edit/readline/history.h>
#endif
#endif

#define PROMPT_HISTORY_SIZE	64
static size_t Prompt_HistorySize = 0;

static int input_prompt_char_(Execute ptr, char **ret)
{
	addr value, data;

	getvalue_prompt(ptr, &value);
	if (value == Nil)
		return Result(ret, NULL);

	if (! stringp(value)) {
		*ret = NULL;
		return fmte_("The prompt value ~S must be a string type.", value, NULL);
	}
	Return(UTF8_buffer_clang_(ptr->local, &data, value));
	if (data == Unbound) {
		*ret = NULL;
		return fmte_("Invalid UTF-8 encoding ~S.", value, NULL);
	}
	posbody(data, (addr *)&data);

	return Result(ret, (char *)data);
}

static void input_prompt_history(char *value)
{
	if (value[0]) {
		add_history(value);
		Prompt_HistorySize++;
	}
	if (PROMPT_HISTORY_SIZE < Prompt_HistorySize) {
		free(remove_history(0));
	}
}

int input_prompt_(Execute ptr, addr *ret)
{
	int check;
	char *value;
	LocalRoot local;
	LocalStack stack;

	/* readline */
	local = ptr->local;
	push_local(local, &stack);
	Return(input_prompt_char_(ptr, &value));
	value = readline(value);
	rollback_local(local, stack);
	if (value == NULL)
		return Result(ret, Nil);

	/* result */
	input_prompt_history(value);
	check = string8_null_char1_heap_(ret, value, 0x0A);
	free(value);
	return check;
}

int clear_prompt_(void)
{
	return 0;
}

#endif

#ifdef LISP_PROMPT_EDITLINE

/* readline */
#ifdef LISP_PROMPT_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

/* editline */
#ifdef LISP_PROMPT_EDITLINE
#if defined(HAVE_EDITLINE_EDITLINE_H)
#include <editline/editline.h>
#include <editline/history.h>
#elif defined(HAVE_EDITLINE_READLINE_H)
#include <editline/readline.h>
#include <editline/history.h>
#elif defined(HAVE_EDIT_READLINE_H)
#include <edit/readline.h>
#include <edit/history.h>
#elif defined(HAVE_EDIT_READLINE_READLINE_H)
#include <edit/readline/readline.h>
#include <edit/readline/history.h>
#else
#include <edit/readline/readline.h>
#include <edit/readline/history.h>
#endif
#endif

#define PROMPT_HISTORY_SIZE	64
static size_t Prompt_HistorySize = 0;

static int input_prompt_char_(Execute ptr, char **ret)
{
	addr value, data;

	getvalue_prompt(ptr, &value);
	if (value == Nil)
		return Result(ret, NULL);

	if (! stringp(value)) {
		*ret = NULL;
		return fmte_("The prompt value ~S must be a string type.", value, NULL);
	}
	Return(UTF8_buffer_clang_(ptr->local, &data, value));
	if (data == Unbound) {
		*ret = NULL;
		return fmte_("Invalid UTF-8 encoding ~S.", value, NULL);
	}
	posbody(data, (addr *)&data);

	return Result(ret, (char *)data);
}

static void input_prompt_history(char *value)
{
	if (value[0]) {
		add_history(value);
		Prompt_HistorySize++;
	}
	if (PROMPT_HISTORY_SIZE < Prompt_HistorySize) {
		free(remove_history(0));
	}
}

int input_prompt_(Execute ptr, addr *ret)
{
	int check;
	char *value;
	LocalRoot local;
	LocalStack stack;

	/* readline */
	local = ptr->local;
	push_local(local, &stack);
	Return(input_prompt_char_(ptr, &value));
	value = readline(value);
	rollback_local(local, stack);
	if (value == NULL)
		return Result(ret, Nil);

	/* result */
	input_prompt_history(value);
	check = string8_null_char1_heap_(ret, value, 0x0A);
	free(value);
	return check;
}

int clear_prompt_(void)
{
	return 0;
}

#endif


/************************************************************
 *  prompt_for.c
 ************************************************************/

/*
 *  prompt-for
 */
static int prompt_for_input_call_(Execute ptr, addr io, addr *ret)
{
	addr value;

	Return(finish_output_stream_(io));
	Return(clear_input_stream_(io));
	Return(read_error_prompt_(ptr, io, &value));

	return Result(ret, value);
}

static int prompt_for_input_(Execute ptr, addr io, addr prompt, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	push_prompt(ptr, prompt, prompt_for);
	(void)prompt_for_input_call_(ptr, io, ret);
	return pop_control_(ptr, control);
}

static int prompt_for_module_(Execute ptr, LocalHold hold,
		addr io, addr type, addr prompt, addr *ret)
{
	int check;
	addr value;

	value = Nil;
	for (;;) {
		Return(prompt_for_input_(ptr, io, prompt, &value));
		localhold_set(hold, 1, value);
		if (type == T)
			break;
		Return(typep_clang_(ptr, value, type, &check));
		if (check)
			break;

		Return(format_string_(ptr, &prompt, "Please answer ~A type: ", type, NULL));
		localhold_set(hold, 2, prompt);
	}

	return Result(ret, value);
}

static int prompt_for_lisp_(Execute ptr, LocalHold hold,
		addr io, addr type, addr prompt, addr *ret)
{
	int check;
	addr value;

	/* output */
	Return(princ_print_(ptr, io, prompt));
	Return(finish_output_stream_(io));

	/* query */
	for (;;) {
		Return(clear_input_stream_(io));
		Return(read_common_(ptr, io, T, Nil, Nil, &value));
		localhold_set(hold, 1, value);
		if (type == T)
			break;
		Return(typep_clang_(ptr, value, type, &check));
		if (check)
			break;

		Return(format_stream_(ptr, io, "~%Please answer ~A type: ", type, NULL));
		Return(finish_output_stream_(io));
	}

	return Result(ret, value);
}

int prompt_for_stream_(Execute ptr, addr type, addr prompt, addr *ret)
{
	addr io;
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);

	/* type */
	if (type != T) {
		Return(parse_type_(ptr, &type, type, Nil));
		localhold_set(hold, 0, type);
	}

	/* input */
	Return(query_io_stream_(ptr, &io));
	if (use_prompt_stream(ptr, io)) {
		Return(prompt_for_module_(ptr, hold, io, type, prompt, ret));
	}
	else {
		Return(prompt_for_lisp_(ptr, hold, io, type, prompt, ret));
	}
	localhold_end(hold);

	return 0;
}


/*
 *  prompt-string
 */
static int prompt_string_input_call_(Execute ptr, addr io, int errorp, addr *ret)
{
	addr value, ignore;

	Return(finish_output_stream_(io));
	Return(clear_input_stream_(io));
	Return(read_line_common_(ptr, io, errorp? T: Nil, Nil, Nil, &value, &ignore));

	return Result(ret, value);
}

static int prompt_string_module_(Execute ptr,
		addr io, addr prompt, int errorp, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	push_prompt(ptr, prompt, prompt_for);
	(void)prompt_string_input_call_(ptr, io, errorp, ret);
	return pop_control_(ptr, control);
}

static int prompt_string_lisp_(Execute ptr,
		addr io, addr prompt, int errorp, addr *ret)
{
	addr ignore;

	/* output */
	Return(princ_print_(ptr, io, prompt));
	Return(finish_output_stream_(io));

	/* query */
	Return(clear_input_stream_(io));
	return read_line_common_(ptr, io, errorp? T: Nil, Nil, Nil, ret, &ignore);
}

int prompt_string_stream_(Execute ptr, addr prompt, int errorp, addr *ret)
{
	addr io;

	Return(query_io_stream_(ptr, &io));
	if (use_prompt_stream(ptr, io))
		return prompt_string_module_(ptr, io, prompt, errorp, ret);
	else
		return prompt_string_lisp_(ptr, io, prompt, errorp, ret);
}


/*
 *  yes-or-no-p
 */
static int yes_or_no_p_char_common_(Execute ptr, addr args, int *ret,
		const char *str_prompt,
		int (yes_)(addr, int *),
		int (no_)(addr, int *),
		const char *str_error)
{
	int check, answer;
	addr stream, control, prompt, pos;

	/* prompt */
	if (args != Nil) {
		open_output_string_stream(&stream, 0);
		GetCons(args, &control, &args);
		Return(format_lisp_(ptr, stream, control, args, &control));
		Return(print_ascii_stream_(stream, " "));
		Return(string_stream_heap_(stream, &prompt));
		Return(close_stream_(stream, NULL));
	}
	else {
		strvect_char_heap(&prompt, str_prompt);
	}

	/* loop */
	for (;;) {
		Return(prompt_string_stream_(ptr, prompt, 1, &pos));
		if (stringp(pos)) {
			/* yes */
			Return((*yes_)(pos, &check));
			if (check) {
				answer = 1;
				break;
			}
			/* no */
			Return((*no_)(pos, &check));
			if (check) {
				answer = 0;
				break;
			}
		}
		/* error */
		strvect_char_heap(&prompt, str_error);
	}

	return Result(ret, answer);
}

static int yes_or_no_p_yes_(addr pos, int *ret)
{
	return string_equalp_char_(pos, "yes", ret);
}

static int yes_or_no_p_no_(addr pos, int *ret)
{
	return string_equalp_char_(pos, "no", ret);
}

static int yes_or_no_p_y_(addr pos, int *ret)
{
	size_t size;
	unicode c;

	string_length(pos, &size);
	if (size == 0)
		return Result(ret, 0);
	Return(string_getc_(pos, 0, &c));
	return Result(ret, toUpperUnicode(c) == 'Y');
}

static int yes_or_no_p_n_(addr pos, int *ret)
{
	size_t size;
	unicode c;

	string_length(pos, &size);
	if (size == 0)
		return Result(ret, 0);
	Return(string_getc_(pos, 0, &c));
	return Result(ret, toUpperUnicode(c) == 'N');
}

int yes_or_no_p_common_(Execute ptr, addr args, int exactp, int *ret)
{
	if (exactp) {
		return yes_or_no_p_char_common_(ptr, args, ret,
				"(yes or no) ", yes_or_no_p_yes_, yes_or_no_p_no_,
				"Please answer yes or no: ");
	}
	else {
		return yes_or_no_p_char_common_(ptr, args, ret,
				"(y or n) ", yes_or_no_p_y_, yes_or_no_p_n_,
				"Please answer y or n: ");
	}

	return 0;
}


/************************************************************
 *  quote.c
 ************************************************************/
/*
 *  Reference:
 *    Common Lisp the Language, 2nd Edition
 *    Guy L. Steele, Thinking Machines, Inc., Digital Press, 1990.
 *    https://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html
 *
 *    Appendix C.  Backquote
 *    https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node367.html
 */

static int bq_simplify_p = 1;

void quote2_heap(addr *ret, enum QuoteType type, addr value, addr print)
{
	addr pos;

	heap_array2(&pos, LISPTYPE_QUOTE, QuoteIndex_Size);
	SetQuote(pos, QuoteIndex_Value, value);
	SetQuote(pos, QuoteIndex_Print, print);
	SetQuoteType(pos, type);
	*ret = pos;
}

static void quote_heap(addr *ret, enum QuoteType type, addr value)
{
	quote2_heap(ret, type, value, value);
}

void getvalue_quote(addr pos, addr *ret)
{
	Check(! quotep(pos), "type error");
	GetQuote(pos, QuoteIndex_Value, ret);
}

void getprint_quote(addr pos, addr *ret)
{
	Check(! quotep(pos), "type error");
	GetQuote(pos, QuoteIndex_Print, ret);
}

int quotep(addr pos)
{
	return GetType(pos) == LISPTYPE_QUOTE;
}

static int quote_type_p(addr pos, enum QuoteType type)
{
	return GetType(pos) == LISPTYPE_QUOTE
		&& RefQuoteType(pos) == type;
}

int quote_back_p(addr pos)
{
	return quote_type_p(pos, QuoteType_Back);
}

int quote_comma_p(addr pos)
{
	return quote_type_p(pos, QuoteType_Comma);
}

int quote_atsign_p(addr pos)
{
	return quote_type_p(pos, QuoteType_AtSign);
}

int quote_dot_p(addr pos)
{
	return quote_type_p(pos, QuoteType_Dot);
}

int quote_quote_p(addr pos)
{
	return quote_type_p(pos, QuoteExecute_Quote);
}

int quote_append_p(addr pos)
{
	return quote_type_p(pos, QuoteExecute_Append);
}

int quote_nconc_p(addr pos)
{
	return quote_type_p(pos, QuoteExecute_Nconc);
}

int quote_list_p(addr pos)
{
	return quote_type_p(pos, QuoteExecute_List);
}

int quote_lista_p(addr pos)
{
	return quote_type_p(pos, QuoteExecute_Lista);
}

int quote_clobberable_p(addr pos)
{
	return quote_type_p(pos, QuoteExecute_Clobberable);
}


/*
 *  bq_process
 */
static int bq_process_(addr pos, addr *ret);

static int bq_atom(addr pos)
{
	return (! quotep(pos)) && atom_function(pos);
}

static int bq_bracket_(addr pos, addr *ret)
{
	/* `(... atom) */
	if (bq_atom(pos)) {
		Return(bq_process_(pos, &pos));
		conscar_heap(&pos, pos);
		quote_heap(ret, QuoteExecute_List, pos);
		return 0;
	}

	/* `(... ,expr) */
	if (quote_comma_p(pos)) {
		getvalue_quote(pos, &pos);
		conscar_heap(&pos, pos);
		quote_heap(ret, QuoteExecute_List, pos);
		return 0;
	}

	/* `(... ,@expr) */
	if (quote_atsign_p(pos)) {
		getvalue_quote(pos, ret);
		return 0;
	}

	/* `(... ,.expr) */
	if (quote_dot_p(pos)) {
		getvalue_quote(pos, &pos);
		quote_heap(ret, QuoteExecute_Clobberable, pos);
		return 0;
	}

	/* others */
	Return(bq_process_(pos, &pos));
	conscar_heap(&pos, pos);
	quote_heap(ret, QuoteExecute_List, pos);
	return 0;
}

static int bq_process_list_(addr pos, addr *ret)
{
	addr root, car;

	root = Nil;
	for (;;) {
		Return_getcons(pos, &car, &pos);
		Return(bq_bracket_(car, &car));
		cons_heap(&root, car, root);

		/* nil */
		if (pos == Nil) {
			break;
		}

		/* dot list */
		if (bq_atom(pos)) {
			quote_heap(&pos, QuoteExecute_Quote, pos);
			break;
		}

		/* `(x . ,pos)  */
		if (quote_comma_p(pos)) {
			getvalue_quote(pos, &pos);
			break;
		}

		/* `(x . ,@pos) error */
		if (quote_atsign_p(pos)) {
			getvalue_quote(pos, &pos);
			return fmte_("Dotted ,@~S", pos, NULL);
		}

		/* `(x . ,.pos) error */
		if (quote_dot_p(pos)) {
			getvalue_quote(pos, &pos);
			return fmte_("Dotted ,.~S", pos, NULL);
		}

		/* `(x . [quote]) */
		if (quotep(pos))
			return fmte_("quote error ~S.", pos, NULL);
	}

	/* *bq-append* */
	if (pos != Nil)
		conscar_heap(&pos, pos);
	Return(nreconc_safe_(&root, root, pos));
	quote_heap(ret, QuoteExecute_Append, root);
	return 0;
}

static int bq_process_(addr pos, addr *ret)
{
	/* `atom */
	if (bq_atom(pos)) {
		quote_heap(ret, QuoteExecute_Quote, pos);
		return 0;
	}

	/* ``... */
	if (quote_back_p(pos)) {
		getvalue_quote(pos, &pos);
		return bq_process_(pos, ret);
	}

	/* ,expr */
	if (quote_comma_p(pos)) {
		getvalue_quote(pos, ret);
		return 0;
	}

	/* ,@expr error */
	if (quote_atsign_p(pos)) {
		getvalue_quote(pos, &pos);
		return fmte_(",@~S after `", pos, NULL);
	}

	/* ,.expr error */
	if (quote_dot_p(pos)) {
		getvalue_quote(pos, &pos);
		return fmte_(",.~S after `", pos, NULL);
	}

	/* [quote] */
	if (quotep(pos)) {
		return fmte_("quote error, ~S", pos, NULL);
	}

	/* list */
	return bq_process_list_(pos, ret);
}


/*
 *  bq_simplify
 */
static int bq_maptree_(int (*call)(addr, addr *), addr pos, addr *ret);

static int bq_maptree_quote_(int (*call)(addr, addr *), addr pos, addr *ret)
{
	enum QuoteType type;
	addr a, b;

	GetQuoteType(pos, &type);
	getvalue_quote(pos, &a);
	Return(bq_maptree_(call, a, &b));
	if (a == b)
		return Result(ret, pos);

	getprint_quote(pos, &a);
	quote2_heap(ret, type, b, a);
	return 0;
}

static int bq_maptree_cons_(int (*call)(addr, addr *), addr pos, addr *ret)
{
	addr car, cdr, a, b;

	Return_getcons(pos, &car, &cdr);
	Return((*call)(car, &a));
	Return(bq_maptree_(call, cdr, &b));
	if (car == a && cdr == b)
		*ret = pos;
	else
		cons_heap(ret, a, b);

	return 0;
}

static int bq_maptree_(int (*call)(addr, addr *), addr pos, addr *ret)
{
	if (bq_atom(pos)) {
		return (*call)(pos, ret);
	}
	if (quotep(pos)) {
		return bq_maptree_quote_(call, pos, ret);
	}
	else {
		return bq_maptree_cons_(call, pos, ret);
	}
}

static int bq_null_or_quoted(addr pos)
{
	return pos == Nil || quote_quote_p(pos);
}

static void getvalue_null_or_quoted(addr pos, addr *ret)
{
	if (pos == Nil)
		*ret = Nil;
	else
		getvalue_quote(pos, ret);
}

static int quote_nil_p(addr pos)
{
	if (! quote_quote_p(pos))
		return 0;
	getvalue_quote(pos, &pos);
	return pos == Nil;
}

static int bq_splicing_frob(addr pos)
{
	return quote_atsign_p(pos) || quote_dot_p(pos);
}

static int bq_attach_concat_(addr pos, addr result, addr *ret,
		enum QuoteType QuoteType_value,
		int (*quote_call_p)(addr))
{
	/* (append '(a b c) '(d e f g)) => (a b c d e f g) */
	if (bq_null_or_quoted(pos) && bq_null_or_quoted(result)) {
		getvalue_null_or_quoted(pos, &pos);
		getvalue_null_or_quoted(result, &result);
		Return(append2_safe_(pos, result, &pos));
		quote_heap(ret, QuoteExecute_Quote, pos);
		return 0;
	}

	/* (append item nil) */
	if (result == Nil || quote_nil_p(result)) {
		if (bq_splicing_frob(pos)) {
			conscar_heap(&pos, pos);
			quote_heap(ret, QuoteType_value, pos);
		}
		else {
			*ret = pos;
		}
		return 0;
	}

	/* (append item '(append a b c)) -> (append item a b c) */
	if ((*quote_call_p)(result)) {
		getvalue_quote(result, &result);
		cons_heap(&pos, pos, result);
		quote_heap(ret, QuoteType_value, pos);
		return 0;
	}

	/* otherwise */
	list_heap(&pos, pos, result, NULL);
	quote_heap(ret, QuoteType_value, pos);
	return 0;
}

static int bq_attach_append_(addr pos, addr result, addr *ret)
{
	return bq_attach_concat_(pos, result, ret, QuoteExecute_Append, quote_append_p);
}

static int bq_attach_nconc_(addr pos, addr result, addr *ret)
{
	return bq_attach_concat_(pos, result, ret, QuoteExecute_Nconc, quote_nconc_p);
}

static int bq_notany_splicing_frob_(addr pos, int *ret)
{
	addr cons;

	getvalue_quote(pos, &cons);
	while (cons != Nil) {
		Return_getcons(cons, &pos, &cons);
		if (bq_splicing_frob(pos))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int bq_every_null_or_quoted_(addr list, int *ret)
{
	addr pos;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! bq_null_or_quoted(pos))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int bq_attach_conses_mapcar_(addr list, addr result, addr *ret)
{
	addr root, pos;

	for (root = Nil; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		getvalue_null_or_quoted(pos, &pos);
		cons_heap(&root, pos, root);
	}
	getvalue_null_or_quoted(result, &result);
	Return(nreconc_safe_(&root, root, result));
	quote_heap(ret, QuoteExecute_Quote, root);

	return 0;
}

/* (list* ,@pos result) */
static int bq_attach_conses_p_(addr pos, addr result, int *ret)
{
	if (! bq_null_or_quoted(result))
		return Result(ret, 0);
	else
		return bq_every_null_or_quoted_(pos, ret);
}

static int bq_attach_conses_(addr pos, addr result, addr *ret)
{
	int check;

	/* (list* 'a 'b 'c 'd) -> '(a b c . d) */
	Return(bq_attach_conses_p_(pos, result, &check));
	if (check) {
		return bq_attach_conses_mapcar_(pos, result, ret);
	}

	/* (list* a b c nil) -> (list a b c) */
	if (result == Nil || quote_nil_p(result)) {
		quote_heap(ret, QuoteExecute_List, pos);
		return 0;
	}

	/* (list* a b c (list* d e f g)) -> (list* a b c d e f g) */
	if (quote_lista_p(result)) {
		getvalue_quote(result, &result);
		Return(append2_safe_(pos, result, &pos));
		quote_heap(ret, QuoteExecute_Lista, pos);
		return 0;
	}

	/* (list* a b c (list d e f g)) -> (list a b c d e f g) */
	if (quote_list_p(result)) {
		getvalue_quote(result, &result);
		Return(append2_safe_(pos, result, &pos));
		quote_heap(ret, QuoteExecute_List, pos);
		return 0;
	}

	/* otherwise */
	conscar_heap(&result, result);
	Return(append2_safe_(pos, result, &pos));
	quote_heap(ret, QuoteExecute_Lista, pos);

	return 0;
}

static int bq_attach_conses_lista_(addr pos, addr root, addr *ret)
{
	addr but, last;

	getvalue_quote(pos, &pos);
	butandlast_safe(&but, &last, pos, 1);
	GetCar(last, &last);
	Return(bq_attach_append_(last, root, &last));
	return bq_attach_conses_(but, last, ret);
}

static int bq_frob(addr pos)
{
	return quote_comma_p(pos)
		|| quote_atsign_p(pos)
		|| quote_dot_p(pos);
}

static int bq_simplify_quote_p(addr pos, addr *ret)
{
	addr car;

	/* (quote (x)) => x */
	if (! quote_quote_p(pos))
		return 0;
	getvalue_quote(pos, &pos);
	if (! consp(pos))
		return 0;
	GetCons(pos, &car, &pos);
	if (bq_frob(car))
		return 0;
	if (pos != Nil)
		return 0;
	*ret = car;

	return 1;
}

static int bq_simplify_list_p_(addr pos, int *ret)
{
	if (! quote_list_p(pos))
		return Result(ret, 0);
	else
		return bq_notany_splicing_frob_(pos, ret);
}

static int bq_simplify_lista_p_(addr pos, int *ret)
{
	if (! quote_lista_p(pos))
		return Result(ret, 0);
	else
		return bq_notany_splicing_frob_(pos, ret);
}

static int bq_simplify_args_(addr args, addr *ret)
{
	int check;
	addr pos, root;

	Return(reverse_list_heap_safe_(&args, args));
	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &pos, &args);
		/* (append atom root) */
		if (bq_atom(pos)) {
			Return(bq_attach_append_(pos, root, &root));
			continue;
		}

		/* (append (list a b c) root) -> (list* a b c root) */
		Return(bq_simplify_list_p_(pos, &check));
		if (check) {
			getvalue_quote(pos, &pos);
			Return(bq_attach_conses_(pos, root, &root));
			continue;
		}

		/* (append (list* a b c) root) -> (list* a b (append c root)) */
		Return(bq_simplify_lista_p_(pos, &check));
		if (check) {
			Return(bq_attach_conses_lista_(pos, root, &root));
			continue;
		}

		/* (append (quote (x)) root) -> (list* (quote x) root) */
		if (bq_simplify_quote_p(pos, &pos)) {
			quote_heap(&pos, QuoteExecute_Quote, pos);
			conscar_heap(&pos, pos);
			Return(bq_attach_conses_(pos, root, &root));
			continue;
		}

		/* (append (clobberable x) root) -> (nconc x foo) */
		if (quote_clobberable_p(pos)) {
			getvalue_quote(pos, &pos);
			Return(bq_attach_nconc_(pos, root, &root));
			continue;
		}

		/* otherwise */
		Return(bq_attach_append_(pos, root, &root));
	}

	return Result(ret, root);
}

static int bq_simplify_(addr pos, addr *ret)
{
	enum QuoteType type;
	addr value, print;

	/* atom */
	if (bq_atom(pos)) {
		return Result(ret, pos);
	}

	/* quote_quote */
	if (quote_quote_p(pos)) {
		goto append;
	}

	/* quote */
	if (quotep(pos)) {
		GetQuoteType(pos, &type);
		getvalue_quote(pos, &value);
		getprint_quote(pos, &print);
		Return(bq_maptree_(bq_simplify_, value, &value));
		quote2_heap(&pos, type, value, print);
		goto append;
	}

	/* list */
	Return(bq_maptree_(bq_simplify_, pos, &pos));
	goto append;

append:
	/* (append ...) */
	if (quote_append_p(pos)) {
		getvalue_quote(pos, &pos);
		return bq_simplify_args_(pos, ret);
	}

	return Result(ret, pos);
}


/*
 *  bq_remove_tokens
 */
static int bq_remove_tokens_(addr pos, addr *ret);
static int bq_remove_tokens_list_(constindex index, addr pos, addr *ret)
{
	addr common;

	getvalue_quote(pos, &pos);
	GetConstant(index, &common);
	Return(bq_maptree_(bq_remove_tokens_, pos, &pos));
	cons_heap(ret, common, pos);

	return 0;
}

static int bq_remove_tokens_args_(constindex index, addr pos, addr *ret)
{
	addr common;

	getvalue_quote(pos, &pos);
	GetConstant(index, &common);
	Return(bq_maptree_(bq_remove_tokens_, pos, &pos));
	list_heap(ret, common, pos, NULL);

	return 0;
}

static int quote_lista_cons_p(addr pos)
{
	/* (list* a b) */
	if (! quote_lista_p(pos))
		return 0;
	getvalue_quote(pos, &pos);
	if (! consp(pos))
		return 0;
	GetCdr(pos, &pos);
	if (! consp(pos))
		return 0;
	GetCdr(pos, &pos);

	return pos == Nil;
}

static int bq_remove_tokens_cons_(addr pos, addr *ret)
{
	addr common;

	/* (conc a b) */
	getvalue_quote(pos, &pos);
	Return(bq_maptree_(bq_remove_tokens_, pos, &pos));
	GetConst(COMMON_CONS, &common);
	cons_heap(ret, common, pos);

	return 0;
}

static int bq_remove_tokens_(addr pos, addr *ret)
{
	/* (list ...) */
	if (quote_list_p(pos)) {
		return bq_remove_tokens_list_(CONSTANT_COMMON_LIST, pos, ret);
	}

	/* (append ...) */
	if (quote_append_p(pos)) {
		return bq_remove_tokens_list_(CONSTANT_COMMON_APPEND, pos, ret);
	}

	/* (nconc ...) */
	if (quote_nconc_p(pos)) {
		return bq_remove_tokens_list_(CONSTANT_COMMON_NCONC, pos, ret);
	}

	/* (list* a b) -> (conc a b) */
	if (quote_lista_cons_p(pos)) {
		return bq_remove_tokens_cons_(pos, ret);
	}

	/* (list* ...) */
	if (quote_lista_p(pos)) {
		return bq_remove_tokens_list_(CONSTANT_COMMON_LISTA, pos, ret);
	}

	/* (quote x) */
	if (quote_quote_p(pos)) {
		return bq_remove_tokens_args_(CONSTANT_COMMON_QUOTE, pos, ret);
	}

	/* (clobberable x) -> x */
	if (quote_clobberable_p(pos)) {
		getvalue_quote(pos, &pos);
		return bq_remove_tokens_(pos, ret);
	}

	/* x */
	if (bq_atom(pos)) {
		return Result(ret, pos);
	}

	/* (a . b) */
	return bq_maptree_(bq_remove_tokens_, pos, ret);
}


/*
 *  interface
 */
static int bq_completely_process_(addr pos, addr *ret)
{
	Return(bq_process_(pos, &pos));
	if (bq_simplify_p) {
		Return(bq_simplify_(pos, &pos));
	}
	return bq_remove_tokens_(pos, ret);
}

int quote_back_heap_(addr *ret, addr form)
{
	addr pos, value;

	Return(bq_completely_process_(form, &value));
	quote2_heap(&pos, QuoteType_Back, value, form);
	return Result(ret, pos);
}

void quote_comma_heap(addr *ret, addr form)
{
	quote_heap(ret, QuoteType_Comma, form);
}

void quote_atsign_heap(addr *ret, addr form)
{
	quote_heap(ret, QuoteType_AtSign, form);
}

void quote_dot_heap(addr *ret, addr form)
{
	quote_heap(ret, QuoteType_Dot, form);
}


/************************************************************
 *  random.c
 ************************************************************/
/*
 *  xorshift+
 */

#define MASK32BIT 0xFFFFFFFFUL
#define MASK64BIT 0xFFFFFFFFFFFFFFFFULL

static uint32_t xorshift128_32bit(uint32_t *x, uint32_t *y, uint32_t *z, uint32_t *w)
{
	/*
	 *  Xorshift RNGs
	 *  George Marsaglia, The Florida State University,
	 *  Journal of Statistical Software, 2003.
	 *  http://www.jstatsoft.org/v08/i14/paper
	 */
	uint32_t v;

	v = (*x ^ (*x << 11/*a*/));
	*x = *y;
	*y = *z;
	*z = *w;
	*w = (v ^ (v >> 8/*b*/)) ^ (*w ^ (*w >> 19/*c*/));

	return *w;
}

static uint64_t xorshift128plus_64bit(uint64_t *s0, uint64_t *s1)
{
	/*
	 *  Further scramblings of Marsaglia's xorshift generators
	 *  Sebastiano Vigna, Universit`a degli Studi di Milano, Italy,
	 *  arXiv:1404.0390v3 [cs.DS] 23 May 2016.
	 *  https://arxiv.org/abs/1404.0390
	 *  http://vigna.di.unimi.it/ftp/papers/xorshiftplus.pdf
	 *
	 *  Table I.
	 *  a23-b17-c26: s34-r30+64-w61 (failures)
	 *  a23-b18-c5 : s38-r20+70-w65 (weight)
	 */
	uint64_t x, y, z;

	x = *s0;
	y = *s1;
	z = x + y;
	*s0 = y;
	x ^= x << 23/*a*/;
	*s1 = x ^ y ^ (x >> 18/*b*/) ^ (y >> 5/*c*/);

	return z;
}

/* random */
uint32_t random_number_32bit(struct random_state *state)
{
	return xorshift128_32bit(
			&state->seed.u32[0], &state->seed.u32[1],
			&state->seed.u32[2], &state->seed.u32[3]);
}

uint64_t random_number_64bit(struct random_state *state)
{
	return xorshift128plus_64bit(&state->seed.u64[0], &state->seed.u64[1]);
}

/* 0 ... value */
uint32_t random_equal_32bit(struct random_state *state, uint32_t value)
{
	int shift;
	uint32_t check, result;

	/* shift */
	if (value == 0UL) return 0UL;
	check = (value >> 1UL);
	for (shift = 1; check; shift++)
		check >>= 1UL;

	/* generate */
	check = (32 <= shift)? MASK32BIT: (1UL << shift) - 1UL;
	do {
		result = check & random_number_32bit(state);
	} while (value < result);

	return result;
}

uint64_t random_equal_64bit(struct random_state *state, uint64_t value)
{
	int shift;
	uint64_t check, result;

	/* shift */
	if (value == 0ULL) return 0ULL;
	check = (value >> 1ULL);
	for (shift = 1; check; shift++)
		check >>= 1ULL;

	/* generate */
	check = (64 <= shift)? MASK64BIT: (1ULL << shift) - 1ULL;
	do {
		result = check & random_number_64bit(state);
	} while (value < result);

	return result;
}

/* 0 ... value-1 */
uint32_t random_less_32bit(struct random_state *state, uint32_t value)
{
	if (value <= 1UL) return 0UL;
	return random_equal_32bit(state, value - 1UL);
}

uint64_t random_less_64bit(struct random_state *state, uint64_t value)
{
	if (value <= 1UL) return 0UL;
	return random_equal_64bit(state, value - 1UL);
}

/* seed */
void random_seed_buffer(struct random_state *state, const void *ptr, size_t size)
{
	sequence_md5encode(ptr, size, state->seed.u32);
}

void random_seed_string(struct random_state *state, const char *word)
{
	sequence_md5encode(word, strlen(word), state->seed.u32);
}

/* check */
int random_state_equal(struct random_state *a, struct random_state *b)
{
	return memcmp(&(a->seed.u32), &(b->seed.u32),
			(size_t)(sizeof(uint32_t) * 4)) == 0;
}


/************************************************************
 *  random_float.c
 ************************************************************/

/*
 *  random float
 */
static char *strfloat_front(char *ptr, const char *data)
{
	for (;;) {
		*ptr = *data;
		if (*ptr == 0)
			break;
		data++;
		ptr++;
	}

	return ptr;
}

static char *strfloat_start(char *ptr)
{
	return strfloat_front(ptr, "0x0.");
}

static void strfloat_end(char *ptr)
{
	strfloat_front(ptr, "p0");
}


/*
 *  32bit float
 */
#define RANDOM_PRINTF32		8

static char *strfloat_32bit(char *ptr, uint32_t value)
{
	char buffer[32];
	snprintf(buffer, 32, "%08" PRIX32, value);
	return strfloat_front(ptr, buffer);
}


/* [32bit] (23+1) * 2 = 48 -> 64 -> 2 times */
#define RANDOM_FLOAT32_TIMES	2
#define RANDOM_FLOAT32_BUFFER	(RANDOM_PRINTF32*RANDOM_FLOAT32_TIMES)
#define RANDOM_FLOAT32_DATA		(4+2+1 + RANDOM_FLOAT32_BUFFER)
float float_random_32bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	char data[RANDOM_FLOAT32_DATA];
	char *ptr;
	unsigned i;
	uint32_t bit;
	float value;

	ptr = strfloat_start(data);
	for (i = 0; i < RANDOM_FLOAT32_TIMES; i++) {
		bit = random_number_32bit(state);
		ptr = strfloat_32bit(ptr, bit);
	}
	strfloat_end(ptr);
	sscanc(data, "%A", &value);

	return value;
}


/* [32bit] (52+1) * 2 = 106 -> 128 -> 4 times */
#define RANDOM_DOUBLE32_TIMES	4
#define RANDOM_DOUBLE32_BUFFER	(RANDOM_PRINTF32*RANDOM_DOUBLE32_TIMES)
#define RANDOM_DOUBLE32_DATA	(4+2+1 + RANDOM_DOUBLE32_BUFFER)
double double_random_32bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	unsigned i;
	char data[RANDOM_DOUBLE32_DATA];
	char *ptr;
	uint32_t bit;
	double value;

	ptr = strfloat_start(data);
	for (i = 0; i < RANDOM_DOUBLE32_TIMES; i++) {
		bit = random_number_32bit(state);
		ptr = strfloat_32bit(ptr, bit);
	}
	strfloat_end(ptr);
	sscanc(data, "%lA", &value);

	return value;
}


/*
 *  IEEE754 binary128
 *    (112+1) * 2 = 226 -> 256 -> 8 times
 *  Intel long-double
 *     (64+0) * 2 = 128 -> 128 -> 4 times
 */
#define RANDOM_LONG32_TIMES		8
#define RANDOM_LONG32_BUFFER	(RANDOM_PRINTF32*RANDOM_LONG32_TIMES)
#define RANDOM_LONG32_DATA		(4+2+1 + RANDOM_LONG32_BUFFER)
long double long_random_32bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	unsigned i;
	char data[RANDOM_LONG32_DATA];
	char *ptr;
	uint32_t bit;
	long double value;

	ptr = strfloat_start(data);
	for (i = 0; i < RANDOM_LONG32_TIMES; i++) {
		bit = random_number_32bit(state);
		ptr = strfloat_32bit(ptr, bit);
	}
	strfloat_end(ptr);
	sscanc(data, "%LA", &value);

	return value;
}


/*
 *  64bit float
 */
#define RANDOM_PRINTF64		16

static char *strfloat_64bit(char *ptr, uint64_t value)
{
	char buffer[32];
	snprintf(buffer, 32, "%016" PRIX64, value);
	return strfloat_front(ptr, buffer);
}


/* [64bit] (23+1) * 2 = 48 -> 64 -> 1 times */
#define RANDOM_FLOAT64_TIMES	1
#define RANDOM_FLOAT64_BUFFER	(RANDOM_PRINTF64*RANDOM_FLOAT64_TIMES)
#define RANDOM_FLOAT64_DATA		(4+2+1 + RANDOM_FLOAT64_BUFFER)
float float_random_64bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	char data[RANDOM_FLOAT64_DATA];
	char *ptr;
	unsigned i;
	uint64_t bit;
	float value;

	ptr = strfloat_start(data);
	for (i = 0; i < RANDOM_FLOAT64_TIMES; i++) {
		bit = random_number_64bit(state);
		ptr = strfloat_64bit(ptr, bit);
	}
	strfloat_end(ptr);
	sscanc(data, "%A", &value);

	return value;
}


/* [64bit] (52+1) * 2 = 106 -> 128 -> 2 times */
#define RANDOM_DOUBLE64_TIMES	2
#define RANDOM_DOUBLE64_BUFFER	(RANDOM_PRINTF64*RANDOM_DOUBLE64_TIMES)
#define RANDOM_DOUBLE64_DATA	(4+2+1 + RANDOM_DOUBLE64_BUFFER)
double double_random_64bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	unsigned i;
	char data[RANDOM_DOUBLE64_DATA];
	char *ptr;
	uint64_t bit;
	double value;

	ptr = strfloat_start(data);
	for (i = 0; i < RANDOM_DOUBLE64_TIMES; i++) {
		bit = random_number_64bit(state);
		ptr = strfloat_64bit(ptr, bit);
	}
	strfloat_end(ptr);
	sscanc(data, "%lA", &value);

	return value;
}


/*
 *  IEEE754 binary128
 *    (112+1) * 2 = 226 -> 256 -> 4 times
 *  Intel long-double
 *     (64+0) * 2 = 128 -> 128 -> 2 times
 */
#define RANDOM_LONG64_TIMES		4
#define RANDOM_LONG64_BUFFER	(RANDOM_PRINTF64*RANDOM_LONG64_TIMES)
#define RANDOM_LONG64_DATA		(4+2+1 + RANDOM_LONG64_BUFFER)
long double long_random_64bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	unsigned i;
	char data[RANDOM_LONG64_DATA];
	char *ptr;
	uint64_t bit;
	long double value;

	ptr = strfloat_start(data);
	for (i = 0; i < RANDOM_LONG64_TIMES; i++) {
		bit = random_number_64bit(state);
		ptr = strfloat_64bit(ptr, bit);
	}
	strfloat_end(ptr);
	sscanc(data, "%LA", &value);

	return value;
}


/************************************************************
 *  random_state.c
 ************************************************************/

#define zeroset(p,s) memset((void *)(p), 0, (size_t)(s))
#define readmd5(m,p,s) read_md5encode((m), (const void *)(p), (size_t)(s))

#define PtrBodyRandomState(x)	((struct random_state *)PtrBodyB2(x))
#define RANDOM_DEVICE "/dev/urandom"
#define RANDOM_DEVICE_SIZE 256

static int RandomState_Init = 0;
static mutexlite RandomState_Mutex;
static volatile int RandomState_Counter = 0;

struct random_state *struct_random_state(addr pos)
{
	CheckType(pos, LISPTYPE_RANDOM_STATE);
	return PtrBodyRandomState(pos);
}

#if defined LISP_UNIX
#include <fcntl.h>
#include <unistd.h>
#include <sys/time.h>

/* hostname */
static size_t gethostname_buffer(char *ptr, size_t size)
{
	int result;

	result = gethostname(ptr, size);
	if (result < 0) {
		size--;
		ptr[size] = '\0';
	}
	else {
		size = strlen(ptr);
	}

	return size;
}

#define GETHOSTNAME_SIZE 256
static void read_hostname_seed(struct md5encode *md5)
{
	volatile char buffer[GETHOSTNAME_SIZE];
	size_t size;

	size = gethostname_buffer((char *)buffer, GETHOSTNAME_SIZE);
	readmd5(md5, buffer, size);
	zeroset(buffer, GETHOSTNAME_SIZE);
}

static int read_device_urandom(struct md5encode *md5)
{
	volatile unsigned char buffer[RANDOM_DEVICE_SIZE];
	int file, check;
	size_t size;

retry:
	file = open(RANDOM_DEVICE, O_RDONLY | O_NONBLOCK);
	if (file < 0) {
		if (errno == EINTR)
			goto retry;
		Debug("file " RANDOM_DEVICE " is not exist.");
		return 1;
	}
	check = readf_unix(file, (void *)buffer, RANDOM_DEVICE_SIZE, &size);
	if (check) {
		close(file);
		Debug("read error");
		return -1;
	}
	readmd5(md5, buffer, size);
	zeroset(buffer, RANDOM_DEVICE_SIZE);
	if (close(file) < 0) {
		Debug("close error");
		return -1;
	}

	return 0;
}

static void random_seed_os(struct md5encode *md5)
{
	volatile int value;
	volatile pid_t pid;
	volatile struct timeval now;

	/* hostname */
	read_hostname_seed(md5);
	/* time */
	gettimeofday((struct timeval *)&now, NULL);
	readmd5(md5, &now, sizeof(now));
	zeroset(&now, sizeof(now));
	/* process id */
	pid = getpid();
	readmd5(md5, &pid, sizeof(pid));
	zeroset(&pid, sizeof(pid));
	/* thread id */
	value = (int)Index_Thread;
	readmd5(md5, &value, sizeof(value));
	value = 0;
	/* read /dev/urandom */
	if (read_device_urandom(md5))
		Abort("Cannot read " RANDOM_DEVICE ".");
}

#elif defined LISP_WINDOWS
#include <windows.h>
#include <ntsecapi.h>

#define GETHOSTNAME_SIZE 256
#define gethostwin(p,s) GetComputerNameExA(ComputerNameDnsFullyQualified,(p),(s))
static int read_hostname_seed(struct md5encode *md5)
{
	volatile char buffer[GETHOSTNAME_SIZE];
	BOOL result;
	DWORD size;
	char *ptr;
	LocalRoot local;
	LocalStack stack;

	size = GETHOSTNAME_SIZE;
	result = gethostwin((LPSTR)buffer, &size);
	if (result == 0) {
		local = Local_Thread;
		push_local(local, &stack);
		ptr = (char *)lowlevel_local(local, size + 1UL);
		result = gethostwin(ptr, &size);
		readmd5(md5, ptr, size);
		zeroset(buffer, GETHOSTNAME_SIZE);
		rollback_local(local, stack);
		if (result == 0) {
			Debug("GetComputerName error");
			return 1;
		}
	}
	else {
		ptr = (char *)buffer;
		readmd5(md5, ptr, size);
		zeroset(buffer, GETHOSTNAME_SIZE);
	}

	return 0;
}

#define RTLGENRANDOM_SIZE 256
static BOOLEAN rtlgenrandom(PVOID buffer, ULONG length)
{
	typedef BOOLEAN (WINAPI *apicalltype)(PVOID, ULONG);
	HMODULE hModule;
	BOOLEAN result;
	apicalltype call;

	hModule = LoadLibraryA("Advapi32.dll");
	if (hModule == NULL) {
		Debug("LoadLibrary Advapi32 error");
		return FALSE;
	}
	call = (apicalltype)GetProcAddress(hModule, "SystemFunction036");
	if (call == NULL) {
		Debug("GetProcAddress SystemFunction036 error");
		FreeLibrary(hModule);
		return FALSE;
	}
	result = (*call)(buffer, length);
	FreeLibrary(hModule);

	return result;
}

static int read_windows_random(struct md5encode *md5)
{
	volatile unsigned char buffer[RTLGENRANDOM_SIZE];
	BOOLEAN result;

	result = rtlgenrandom((PVOID)buffer, RTLGENRANDOM_SIZE);
	if (result == FALSE) {
		Debug("RtlGenRandom error");
		return 1;
	}
	readmd5(md5, buffer, RTLGENRANDOM_SIZE);
	SecureZeroMemory((PVOID)buffer, RTLGENRANDOM_SIZE);

	return 0;
}

static void random_seed_os(struct md5encode *md5)
{
	volatile DWORD value;
	volatile SYSTEMTIME time;

	/* hostname */
	if (read_hostname_seed(md5))
		Abort("Cannot get hostname.");
	/* time */
	GetSystemTime((LPSYSTEMTIME)&time);
	readmd5(md5, &time, sizeof(time));
	zeroset(&time, sizeof(time));
	/* process id */
	value = GetCurrentProcessId();
	readmd5(md5, &value, sizeof(value));
	value = 0;
	/* thread id */
	value = GetCurrentThreadId();
	readmd5(md5, &value, sizeof(value));
	value = 0;
	/* RtlGenRandom */
	if (read_windows_random(md5))
		Abort("Cannot get random number.");
}
#else

static int read_device_urandom(struct md5encode *md5)
{
	volatile unsigned char buffer[RANDOM_DEVICE_SIZE];
	FILE *file;
	size_t size;

	file = fopen(RANDOM_DEVICE, "rb");
	if (file == NULL) {
		/* Device is not exist, OK. */
		return 1;
	}
	size = fread((void *)buffer, RANDOM_DEVICE_SIZE, 1, file);
	if (size == 0) {
		fclose(file);
		Debug("fread error");
		return -1;
	}
	readmd5(md5, buffer, size);
	zeroset(buffer, RANDOM_DEVICE_SIZE);
	if (fclose(file) < 0) {
		Debug("fclose error");
		return -1;
	}

	return 0;
}

static void random_seed_os(struct md5encode *md5)
{
	volatile time_t now;

	/* time */
	time((time_t *)&now);
	readmd5(md5, &now, sizeof(now));
	zeroset(&now, sizeof(now));
	/* read /dev/urandom */
	if (read_device_urandom(md5) < 0)
		Abort("Cannot read random device.");
}
#endif


/*
 *  interface
 */
int init_random_state(void)
{
	if (RandomState_Init) {
		Debug("RandomState_Init error.");
		return 1;
	}
	if (lispd_make_mutexlite(&RandomState_Mutex)) {
		Debug("lispd_make_mutexlite error.");
		return 1;
	}
	RandomState_Init = 1;

	return 0;
}

void free_random_state(void)
{
	if (RandomState_Init) {
		lispd_destroy_mutexlite(&RandomState_Mutex);
		RandomState_Init = 0;
	}
}

static void make_random_seed(struct md5encode *md5)
{
	volatile const void *ptr;
	void (*call)(struct md5encode *);

	/* environment */
	random_seed_os(md5);
	/* counter */
	lispd_lock_mutexlite(&RandomState_Mutex);
	RandomState_Counter++;
	lispd_unlock_mutexlite(&RandomState_Mutex);
	readmd5(md5, &RandomState_Counter, sizeof(RandomState_Counter));
	/* function pointer */
	call = make_random_seed;
	memcpy((void *)&ptr, (const void *)&call, sizeof(ptr));
	readmd5(md5, &ptr, sizeof(ptr));
	ptr = NULL;
}

void random_state_alloc(LocalRoot local, addr *ret)
{
	addr pos;

	Check(0xFF <= sizeoft(struct random_state), "size error");
	alloc_body2(local, &pos, LISPTYPE_RANDOM_STATE, sizeoft(struct random_state));
	memset(struct_random_state(pos), 0, sizeoft(struct random_state));
	*ret = pos;
}
void random_state_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	random_state_alloc(local, ret);
}
void random_state_heap(addr *ret)
{
	random_state_alloc(NULL, ret);
}

static void make_randomly_seed(struct random_state *ptr)
{
	volatile uint8_t result[MD5ENCODE_SIZE];
	struct md5encode md5;

	clear_md5encode(&md5);
	make_random_seed(&md5);
	calc_md5encode(&md5, (void *)result);
	clear_md5encode(&md5);
	memcpy(ptr, (const void *)result, sizeoft(result));
	zeroset(result, MD5ENCODE_SIZE);
}

void copy_random_state(addr left, addr right)
{
	struct random_state *ptr1, *ptr2;

	CheckType2(left, LISPTYPE_RANDOM_STATE, "type left error");
	CheckType2(right, LISPTYPE_RANDOM_STATE, "type right error");
	ptr1 = struct_random_state(left);
	ptr2 = struct_random_state(right);
	memcpy(ptr1, ptr2, sizeoft(struct random_state));
}

void randomly_random_state(addr left)
{
	struct random_state *ptr;

	CheckType(left, LISPTYPE_RANDOM_STATE);
	Check(sizeof(struct random_state) != MD5ENCODE_SIZE, "size error");
	ptr = struct_random_state(left);
	make_randomly_seed(ptr);
}

int constant_random_state_(Execute ptr, addr left)
{
	addr right;

	Check(ptr == NULL, "execute error");
	GetConst(SPECIAL_RANDOM_STATE, &right);
	Return(getspecialcheck_local_(ptr, right, &right));
	if (GetType(right) != LISPTYPE_RANDOM_STATE)
		return TypeError_(right, RANDOM_STATE);
	copy_random_state(left, right);

	return 0;
}

int make_random_state_heap_(Execute ptr, addr *ret, addr state)
{
	addr pos;

	random_state_heap(&pos);
	if (state == T) {
		randomly_random_state(pos);
	}
	else if (state == Nil) {
		Return(constant_random_state_(ptr, pos));
	}
	else {
		if (GetType(state) != LISPTYPE_RANDOM_STATE) {
			*ret = Nil;
			return TypeError_(state, RANDOM_STATE);
		}
		copy_random_state(pos, state);
	}

	return Result(ret, pos);
}

#ifdef LISP_64BIT
#define RANDOM_STATE_SIZE	2
#define RANDOM_STATE_UNION	u64
#else
#define RANDOM_STATE_SIZE	4
#define RANDOM_STATE_UNION	u32
#endif
void make_bignum_random_state_alloc(LocalRoot local, addr pos, addr *ret)
{
	int i;
	struct random_state *state;

	CheckType(pos, LISPTYPE_RANDOM_STATE);
	state = struct_random_state(pos);
	alloc_bignum(local, &pos, RANDOM_STATE_SIZE);
	for (i = 0; i < RANDOM_STATE_SIZE; i++)
		setfixed_bignum(pos, i, state->seed.RANDOM_STATE_UNION[i]);
	*ret = pos;
}

void make_bignum_random_state_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	make_bignum_random_state_alloc(local, pos, ret);
}

void make_bignum_random_state_heap(addr pos, addr *ret)
{
	make_bignum_random_state_alloc(NULL, pos, ret);
}

int equal_random_state_addr(addr left, addr right)
{
	struct random_state *state1, *state2;

	CheckType(left, LISPTYPE_RANDOM_STATE);
	CheckType(right, LISPTYPE_RANDOM_STATE);
	state1 = struct_random_state(left);
	state2 = struct_random_state(right);

	return random_state_equal(state1, state2);
}


/*
 *  sysctl
 */
int random_state_integer_(addr pos, addr *ret)
{
	addr value;

	if (GetType(pos) != LISPTYPE_RANDOM_STATE) {
		*ret = Nil;
		return TypeError_(pos, RANDOM_STATE);
	}

	make_bignum_random_state_heap(pos, &value);
	sizepress_bignum(value);
	return integer_result_heap_(value, ret);
}

static int random_state_set_value_(LocalRoot local, addr pos, addr value)
{
	addr spec, ignore;
	fixed *data;
	struct random_state *str;
	size_t size, i;

	CheckType(pos, LISPTYPE_RANDOM_STATE);
	Check(! integerp(value), "type error");

	/* (ldb (byte 128 0) value) */
	bytespec_heap(&spec, 128UL, 0UL);
	Return(ldb_common_(local, &value, spec, value));

	/* bignum */
	if (fixnump(value))
		bignum_fixnum_heap(&value, value);
	GetSizeBignum(value, &size);
	GetRootDataBignum(value, &ignore, &data);

	/* set */
	str = struct_random_state(pos);
	clearpoint(str);
	for (i = 0; i < size; i++) {
#ifdef LISP_64BIT
		str->seed.u64[i] = (uint64_t)data[i];
#else
		str->seed.u32[i] = (uint32_t)data[i];
#endif
	}

	return 0;
}

int random_state_make_(LocalRoot local, addr value, addr *ret)
{
	addr pos;

	if (! integerp(value)) {
		*ret = Nil;
		return TypeError_(value, INTEGER);
	}
	random_state_heap(&pos);
	Return(random_state_set_value_(local, pos, value));

	return Result(ret, pos);
}

int random_state_write_(LocalRoot local, addr pos, addr value)
{

	if (GetType(pos) != LISPTYPE_RANDOM_STATE)
		return TypeError_(pos, RANDOM_STATE);
	if (! integerp(value))
		return TypeError_(pos, INTEGER);

	return random_state_set_value_(local, pos, value);
}


/************************************************************
 *  ratio.c
 ************************************************************/

int ratiop(addr pos)
{
	return GetType(pos) == LISPTYPE_RATIO;
}

void setnumer_ratio(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RATIO);
	SetNumerRatio_Low(pos, value);
}

void getnumer_ratio(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RATIO);
	GetNumerRatio_Low(pos, ret);
}

void setdenom_ratio(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RATIO);
	SetDenomRatio_Low(pos, value);
}

void getdenom_ratio(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RATIO);
	GetDenomRatio_Low(pos, ret);
}

void setsign_ratio(addr pos, int sign)
{
	CheckType(pos, LISPTYPE_RATIO);
	SetSignRatio_Low(pos, sign);
}

void getsign_ratio(addr pos, int *ret)
{
	CheckType(pos, LISPTYPE_RATIO);
	GetSignRatio_Low(pos, ret);
}

int refsign_ratio(addr pos)
{
	CheckType(pos, LISPTYPE_RATIO);
	return RefSignRatio_Low(pos);
}

int getfixnum_ratio(addr pos, fixnum *ret)
{
	int sign;
	addr denom;
	fixed value;

	CheckType(pos, LISPTYPE_RATIO);
	GetDenomRatio(pos, &denom);
	if (! equal_value_nosign_bignum(denom, 1))
		return 1;
	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &pos);
	if (RefSizeBignum(pos) != 1)
		return 1;
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

int getfixed1_ratio(addr pos, int *sign, fixed *ret)
{
	addr denom;
	fixed value;

	CheckType(pos, LISPTYPE_RATIO);
	GetDenomRatio(pos, &denom);
	if (! equal_value_nosign_bignum(denom, 1))
		return 1;
	GetNumerRatio(pos, &pos);
	if (RefSizeBignum(pos) != 1)
		return 1;
	GetSignRatio(pos, sign);
	getfixed_bignum(pos, 0, &value);

	return 0;
}


/*
 *  reduction
 */
static void reduction_single(addr numer_root, addr denom_root)
{
	fixed *numer, *denom;
	fixed a, b, n;

	GetRootDataBignum(numer_root, &numer_root, &numer);
	GetRootDataBignum(denom_root, &denom_root, &denom);

	/* Euclidean Algorithm */
	Check(*numer == 0, "reduction error");
	Check(*denom == 0, "division-by-zero error");
	a = *numer;
	b = *denom;
	if (a == b) {
		*numer = *denom = 1;
		return;
	}
	if (a < b) {
		n = a; a = b; b = n;
	}
	while (b) {
		n = a % b;
		a = b;
		b = n;
	}
	*numer /= a;
	*denom /= a;
}

void euclidean_bignum(LocalRoot local, addr numer, addr denom)
{
	int compare;
	addr a, b, n;
	size_t size;

	/* Euclidean Algorithm */
	Check(local == NULL, "local error");
	Check(zerop_bignum(numer), "reduction error");
	Check(zerop_bignum(denom), "division-by-zero error");
	compare = compare_bigdata(numer, denom);
	if (compare == 0) {
		setvalue_bignum(numer, SignPlus, 1);
		return;
	}
	if (compare < 0) {
		bignum_copy_local(local, &a, denom);
		bignum_copy_local(local, &b, numer);
	}
	else {
		bignum_copy_local(local, &a, numer);
		bignum_copy_local(local, &b, denom);
	}

	GetSizeBignum(b, &size);
	bignum_local(local, &n, SignPlus, size);
	while (! zerop_bignum(b)) {
		setrem_noexpand_bigdata(local, n, a, b);
		copy_bignum(local, a, b, 0);
		copy_bignum(local, b, n, 0);
	}
	copy_noexpand_bignum(numer, a);
}

static void reduction_multiple(LocalRoot local, addr numer, addr denom)
{
	int compare;
	addr a, b, n;
	size_t size;

	/* Euclidean Algorithm */
	Check(local == NULL, "local error");
	Check(zerop_bignum(numer), "reduction error");
	Check(zerop_bignum(denom), "division-by-zero error");
	compare = compare_bigdata(numer, denom);
	if (compare == 0) {
		setvalue_bignum(numer, SignPlus, 1);
		setvalue_bignum(denom, SignPlus, 1);
		return;
	}
	if (compare < 0) {
		bignum_copy_local(local, &a, denom);
		bignum_copy_local(local, &b, numer);
	}
	else {
		bignum_copy_local(local, &a, numer);
		bignum_copy_local(local, &b, denom);
	}

	GetSizeBignum(b, &size);
	bignum_local(local, &n, SignPlus, size);
	while (! zerop_bignum(b)) {
		setrem_noexpand_bigdata(local, n, a, b);
		copy_bignum(local, a, b, 0);
		copy_bignum(local, b, n, 0);
	}
	letdiv_noexpand_bigdata(local, numer, a);
	letdiv_noexpand_bigdata(local, denom, a);
}

void reduction_local(LocalRoot local, addr numer, addr denom)
{
	size_t size1, size2;

	Check(local == NULL, "local error");
	Check(zerop_bignum(denom), "division-by-zero error");
	if (zerop_bignum(numer))
		return;
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	if (size1 == 1 && size2 == 1)
		reduction_single(numer, denom);
	else
		reduction_multiple(local, numer, denom);
}


/*
 *  ratio object
 */
void make_ratio_heap(addr *ret, int sign, addr numer, addr denom)
{
	addr pos;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(GetStatusDynamic(numer), "dynamic numer error");
	Check(GetStatusDynamic(denom), "dynamic denom error");

	heap_array2(&pos, LISPTYPE_RATIO, 2);
	SetSignRatio(pos, sign);
	SetNumerRatio(pos, numer);
	SetDenomRatio(pos, denom);
	*ret = pos;
}

void make_ratio_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	addr pos;

	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");

	local_array2(local, &pos, LISPTYPE_RATIO, 2);
	SetSignRatio(pos, sign);
	SetNumerRatio(pos, numer);
	SetDenomRatio(pos, denom);
	*ret = pos;
}

void make_ratio_alloc(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	if (local)
		make_ratio_local(local, ret, sign, numer, denom);
	else
		make_ratio_heap(ret, sign, numer, denom);
}

void make_ratio_alloc_unsafe(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	make_ratio_alloc(local, ret, sign, numer, denom);
}

static void make_copy_ratio_heap(addr *ret, int sign, addr numer, addr denom)
{
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");

	bignum_copy_heap(&numer, numer);
	bignum_copy_heap(&denom, denom);
	make_ratio_heap(ret, sign, numer, denom);
}

static void make_copy_ratio_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");

	bignum_copy_local(local, &numer, numer);
	bignum_copy_local(local, &denom, denom);
	make_ratio_local(local, ret, sign, numer, denom);
}

void ratio_reduction_heap(LocalRoot local, addr *ret, int sign, addr numer, addr denom)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");
	Check(RefSignBignum(numer) != SignPlus, "sign numer error");
	Check(RefSignBignum(denom) != SignPlus, "sign denom error");

	if (zerop_bignum(numer)) {
		fixnum_heap(ret, 0);
		return;
	}

	push_local(local, &stack);
	reduction_local(local, numer, denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		SetSignBignum(numer, sign);
		integer_bignum_heap(ret, numer);
		goto finish;
	}
	make_copy_ratio_heap(ret, sign, numer, denom);

finish:
	rollback_local(local, stack);
}

void ratio_reduction_local(LocalRoot local, addr *ret, int sign, addr numer, addr denom)
{
	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");
	Check(RefSignBignum(numer) != SignPlus, "sign numer error");
	Check(RefSignBignum(denom) != SignPlus, "sign denom error");

	if (zerop_bignum(numer)) {
		fixnum_local(local, ret, 0);
		return;
	}

	reduction_local(local, numer, denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		SetSignBignum(numer, sign);
		integer_bignum_local(local, ret, numer);
		return;
	}
	make_copy_ratio_local(local, ret, sign, numer, denom);
}

void ratio_noreduction_heap(addr *ret, int sign, addr numer, addr denom)
{
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");
	Check(RefSignBignum(numer) != SignPlus, "sign numer error");
	Check(RefSignBignum(denom) != SignPlus, "sign denom error");

	if (zerop_bignum(numer)) {
		fixnum_heap(ret, 0);
		return;
	}
	if (equal_value_nosign_bignum(denom, 1)) {
		SetSignBignum(numer, sign);
		integer_bignum_heap(ret, numer);
		return;
	}
	make_copy_ratio_heap(ret, sign, numer, denom);
}

void ratio_noreduction_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");
	Check(RefSignBignum(numer) != SignPlus, "sign numer error");
	Check(RefSignBignum(denom) != SignPlus, "sign denom error");

	if (zerop_bignum(numer)) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (equal_value_nosign_bignum(denom, 1)) {
		SetSignBignum(numer, sign);
		integer_bignum_local(local, ret, numer);
		return;
	}
	make_copy_ratio_local(local, ret, sign, numer, denom);
}

void ratio_reduction_nocopy_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");
	Check(RefSignBignum(numer) != SignPlus, "sign numer error");
	Check(RefSignBignum(denom) != SignPlus, "sign denom error");

	if (zerop_bignum(numer)) {
		fixnum_local(local, ret, 0);
		return;
	}

	push_local(local, &stack);
	reduction_local(local, numer, denom);
	rollback_local(local, stack);
	if (equal_value_nosign_bignum(denom, 1)) {
		SetSignBignum(numer, sign);
		integer_bignum_local(local, ret, numer);
		return;
	}
	make_ratio_local(local, ret, sign, numer, denom);
}

void make_ratio_reduction_heap(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");
	Check(RefSignBignum(numer) != SignPlus, "sign numer error");
	Check(RefSignBignum(denom) != SignPlus, "sign denom error");

	push_local(local, &stack);
	reduction_local(local, numer, denom);
	make_copy_ratio_heap(ret, sign, numer, denom);
	rollback_local(local, stack);
}

void make_ratio_reduction_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");
	Check(RefSignBignum(numer) != SignPlus, "sign numer error");
	Check(RefSignBignum(denom) != SignPlus, "sign denom error");

	push_local(local, &stack);
	reduction_local(local, numer, denom);
	rollback_local(local, stack);
	make_ratio_local(local, ret, sign, numer, denom);
}

void ratio_reduction_value_local(LocalRoot local, addr *ret,
		int sign, fixed numer, fixed denom)
{
	addr num, den;

	Check(local == NULL, "local error");
	bignum_value_local(local, &num, SignPlus, numer);
	bignum_value_local(local, &den, SignPlus, denom);
	make_ratio_reduction_local(local, ret, sign, num, den);
}

void ratio_reduction_value_heap(LocalRoot local, addr *ret,
		int sign, fixed numer, fixed denom)
{
	addr num, den;
	LocalStack stack;

	Check(local == NULL, "local error");
	push_local(local, &stack);
	bignum_value_local(local, &num, SignPlus, numer);
	bignum_value_local(local, &den, SignPlus, denom);
	make_ratio_reduction_heap(local, ret, sign, num, den);
	rollback_local(local, stack);
}

void ratio_noreduction_value_local(LocalRoot local, addr *ret,
		int sign, fixed numer, fixed denom)
{
	addr num, den;

	Check(local == NULL, "local error");
	bignum_value_local(local, &num, SignPlus, numer);
	bignum_value_local(local, &den, SignPlus, denom);
	make_ratio_local(local, ret, sign, num, den);
}

void ratio_noreduction_value_heap(addr *ret,
		int sign, fixed numer, fixed denom)
{
	addr num, den;

	bignum_value_heap(&num, SignPlus, numer);
	bignum_value_heap(&den, SignPlus, denom);
	make_ratio_heap(ret, sign, num, den);
}

void ratio_zero_alloc(LocalRoot local, addr *ret)
{
	addr numer, denom;

	bignum_zero_alloc(local, &numer);
	bignum_value_alloc(local, &denom, SignPlus, 1);
	make_ratio_alloc(local, ret, SignPlus, numer, denom);
}

void ratio_zero_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	ratio_zero_alloc(local, ret);
}

void ratio_zero_heap(addr *ret)
{
	ratio_zero_alloc(NULL, ret);
}

void ratio_copy_nosign_alloc(LocalRoot local, addr *ret, addr pos)
{
	addr numer, denom;

	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	bignum_copy_alloc(local, &numer, numer);
	bignum_copy_alloc(local, &denom, denom);
	make_ratio_alloc(local, ret, SignPlus, numer, denom);
}

void ratio_copy_nosign_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	ratio_copy_nosign_alloc(local, ret, pos);
}

void ratio_copy_nosign_heap(addr *ret, addr pos)
{
	ratio_copy_nosign_alloc(NULL, ret, pos);
}

void ratio_copy_alloc(LocalRoot local, addr *ret, addr pos)
{
	int sign;
	addr numer, denom;

	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	bignum_copy_alloc(local, &numer, numer);
	bignum_copy_alloc(local, &denom, denom);
	make_ratio_alloc(local, ret, sign, numer, denom);
}

void ratio_copy_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	ratio_copy_alloc(local, ret, pos);
}

void ratio_copy_heap(addr *ret, addr pos)
{
	ratio_copy_alloc(NULL, ret, pos);
}

void ratio_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	if (GetStatusDynamic(pos))
		ratio_copy_heap(ret, pos);
	else
		*ret = pos;
}

void ratio_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		ratio_copy_local(local, ret, pos);
}

void ratio_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	if (local)
		ratio_throw_local(local, pos, ret);
	else
		ratio_throw_heap(pos, ret);
}

void ratio_result_noreduction_local(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	addr numer, denom;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (zerop_bignum(numer)) {
		fixnum_local(local, ret, 0);
	}
	else if (equal_value_nosign_bignum(denom, 1)) {
		bignum_copy_local(local, &numer, numer);
		GetSignRatio(pos, &sign);
		SetSignBignum(numer, sign);
		bignum_result_local(local, numer, ret);
	}
	else {
		ratio_throw_local(local, pos, ret);
	}
}

void ratio_result_noreduction_heap(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	addr numer, denom;
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (zerop_bignum(numer)) {
		fixnum_heap(ret, 0);
	}
	else if (equal_value_nosign_bignum(denom, 1)) {
		push_local(local, &stack);
		bignum_copy_local(local, &numer, numer);
		GetSignRatio(pos, &sign);
		SetSignBignum(numer, sign);
		bignum_result_heap(numer, ret);
		rollback_local(local, stack);
	}
	else {
		ratio_throw_heap(pos, ret);
	}
}

int zerop_ratio(addr left)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type error");
	GetNumerRatio(left, &left);
	return zerop_bignum(left);
}

int plusp_ratio(addr left)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type error");
	if (IsMinus(RefSignRatio(left)))
		return 0;
	return ! zerop_ratio(left);
}

int minusp_ratio(addr left)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type error");
	if (IsPlus(RefSignRatio(left)))
		return 0;
	return ! zerop_ratio(left);
}


/*
 *  cast integer
 */
void cast_fixnum_ratio_local(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	addr numer, denom;
	fixed value;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_FIXNUM);
	castfixed_fixnum(pos, &sign, &value);
	bignum_value_local(local, &numer, SignPlus, value);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_local(local, ret, sign, numer, denom);
}

void cast_bignum_ratio_local(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	addr numer, denom;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_BIGNUM);
	GetSignBignum(pos, &sign);
	bignum_copy_nosign_local(local, &numer, pos);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_local(local, ret, sign, numer, denom);
}


/*
 *  cast float
 */
static size_t hexfloat_fixed_exponent(fixed value)
{
	size_t i;

	for (i = 0; value; i += 4UL) {
		value >>= 4UL;
	}

	return i;
}

static size_t hexfloat_exponent(addr pos)
{
	size_t size;
	fixed *data;

	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);
	Check(size == 0, "size error");
	if (size == 1) {
		size =  hexfloat_fixed_exponent(data[0]);
	}
	else {
		size--;
		size = size * BIGNUM_FULLBIT + hexfloat_fixed_exponent(data[size]);
	}
	Check(size < 4UL, "size error");

	return size - 4UL;
}

#define OVERFLOW_EXPONENT		20000
static int diff_exponent_ratio_(size_t *size1, size_t *size2, addr pos)
{
	if (*size1 < *size2) {
		*size2 -= *size1;
		*size1 = 0;
	}
	else {
		*size1 -= *size2;
		*size2 = 0;
	}
	if (OVERFLOW_EXPONENT < *size1) {
		*size1 = *size2 = 0;
		return call_float_overflow_va_(NULL, CONSTANT_COMMON_COERCE, pos, NULL);
	}
	if (OVERFLOW_EXPONENT < *size2) {
		*size1 = *size2 = 0;
		return call_float_underflow_va_(NULL, CONSTANT_COMMON_COERCE, pos, NULL);
	}

	return 0;
}

#define HexToChar(x) (((x) < 10)? ('0' + (x)): ('A' - 10 + (x)))
static char *hexadecimal_fixed(char *ptr, fixed v, int first, size_t *bit)
{
	unsigned i, n, index, size;

	size = BIGNUM_FULLBIT / 4UL;
	for (i = 0; i < size; i++) {
		index = size - i - 1;
		n = (v >> (index * 4UL)) & 0x0F;
		if (first == 0 || n) {
			*(ptr++) = HexToChar(n);
			if (first) {
				*(ptr++) = '.';
				first = 0;
			}
			if (*bit <= 4UL) {
				*bit = 0;
				break;
			}
			*bit -= 4UL;
		}
	}
	Check(first, "first error");

	return ptr;
}

static char *hexadecimal_ratio(char *ptr, addr pos, size_t bit)
{
	size_t size, i, index;
	fixed *data;

	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);
	Check(size == 0, "size error");
	for (i = 0; i < size; i++) {
		index = size - i - 1;
		ptr = hexadecimal_fixed(ptr, data[index], (i == 0), &bit);
		if (bit == 0) break;
	}

	return ptr;
}

static void float_string_ratio(int sign, addr pos, char *str, size_t exp, size_t bit)
{
	*(str++) = IsPlus(sign)? '+': '-';
	*(str++) = '0';
	*(str++) = 'x';
	str = hexadecimal_ratio(str, pos, bit);
	*(str++) = 'p';
	snprintf(str, 8, "%d", (int)exp); /* less than OVERFLOW_EXPONENT */
}

int single_float_ratio_(addr pos, single_float *ret)
{
	char str1[64], str2[64];
	int sign;
	addr numer, denom;
	size_t size1, size2;
	single_float v1, v2;

	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (zerop_bignum(denom)) {
		*ret = 0.0f;
		return call_division_by_zero_real1_(NULL, CONSTANT_COMMON_COERCE, pos);
	}
	if (zerop_bignum(numer))
		return Result(ret, sign? -0.0f: +0.0f);
	size1 = hexfloat_exponent(numer);
	size2 = hexfloat_exponent(denom);
	Return(diff_exponent_ratio_(&size1, &size2, pos));
	float_string_ratio(sign, numer, str1, size1, LISP_FLOAT_SINGLE_FRACTION);
	float_string_ratio(SignPlus, denom, str2, size2, LISP_FLOAT_SINGLE_FRACTION);
	Return(check_strtof_(str1, pos, &v1));
	Return(check_strtof_reverse_(str2, pos, &v2));
	v1 /= v2;
	Return_float_errorcheck1(CONSTANT_COMMON_COERCE, v1, pos);

	return Result(ret, v1);
}

int double_float_ratio_(addr pos, double_float *ret)
{
	char str1[64], str2[64];
	int sign;
	addr numer, denom;
	size_t size1, size2;
	double_float v1, v2;

	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (zerop_bignum(denom)) {
		*ret = 0.0;
		return call_division_by_zero_real1_(NULL, CONSTANT_COMMON_COERCE, pos);
	}
	if (zerop_bignum(numer))
		return Result(ret, sign? -0.0: +0.0);
	size1 = hexfloat_exponent(numer);
	size2 = hexfloat_exponent(denom);
	Return(diff_exponent_ratio_(&size1, &size2, pos));
	float_string_ratio(sign, numer, str1, size1, LISP_FLOAT_DOUBLE_FRACTION);
	float_string_ratio(SignPlus, denom, str2, size2, LISP_FLOAT_DOUBLE_FRACTION);
	Return(check_strtod_(str1, pos, &v1));
	Return(check_strtod_reverse_(str2, pos, &v2));
	v1 /= v2;
	Return_float_errorcheck1(CONSTANT_COMMON_COERCE, v1, pos);

	return Result(ret, v1);
}

int long_float_ratio_(addr pos, long_float *ret)
{
	char str1[64], str2[64];
	int sign;
	addr numer, denom;
	size_t size1, size2;
	long_float v1, v2;

	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (zerop_bignum(denom)) {
		*ret = 0.0L;
		return call_division_by_zero_real1_(NULL, CONSTANT_COMMON_COERCE, pos);
	}
	if (zerop_bignum(numer))
		return Result(ret, sign? -0.0L: +0.0L);
	size1 = hexfloat_exponent(numer);
	size2 = hexfloat_exponent(denom);
	Return(diff_exponent_ratio_(&size1, &size2, pos));
	float_string_ratio(sign, numer, str1, size1, LISP_FLOAT_LONG_FRACTION);
	float_string_ratio(SignPlus, denom, str2, size2, LISP_FLOAT_LONG_FRACTION);
	Return(check_strtold_(str1, pos, &v1));
	Return(check_strtold_reverse_(str2, pos, &v2));
	v1 /= v2;
	Return_float_errorcheck1(CONSTANT_COMMON_COERCE, v1, pos);

	return Result(ret, v1);
}

int single_float_ratio_alloc_(LocalRoot local, addr *ret, addr pos)
{
	single_float value;

	CheckType(pos, LISPTYPE_RATIO);
	Return(single_float_ratio_(pos, &value));
	single_float_alloc(local, ret, value);

	return 0;
}
int single_float_ratio_local_(LocalRoot local, addr *ret, addr pos)
{
	CheckLocal(local);
	return single_float_ratio_alloc_(local, ret, pos);
}
int single_float_ratio_heap_(addr *ret, addr pos)
{
	return single_float_ratio_alloc_(NULL, ret, pos);
}

int double_float_ratio_alloc_(LocalRoot local, addr *ret, addr pos)
{
	double_float value;

	CheckType(pos, LISPTYPE_RATIO);
	Return(double_float_ratio_(pos, &value));
	double_float_alloc(local, ret, value);

	return 0;
}
int double_float_ratio_local_(LocalRoot local, addr *ret, addr pos)
{
	CheckLocal(local);
	return double_float_ratio_alloc_(local, ret, pos);
}
int double_float_ratio_heap_(addr *ret, addr pos)
{
	return double_float_ratio_alloc_(NULL, ret, pos);
}

int long_float_ratio_alloc_(LocalRoot local, addr *ret, addr pos)
{
	long_float value;

	CheckType(pos, LISPTYPE_RATIO);
	Return(long_float_ratio_(pos, &value));
	long_float_alloc(local, ret, value);

	return 0;
}
int long_float_ratio_local_(LocalRoot local, addr *ret, addr pos)
{
	CheckLocal(local);
	return long_float_ratio_alloc_(local, ret, pos);
}
int long_float_ratio_heap_(addr *ret, addr pos)
{
	return long_float_ratio_alloc_(NULL, ret, pos);
}


/*
 *  abs
 */
void abs_ratio_alloc(LocalRoot local, addr left, addr *ret)
{
	int sign;

	CheckType(left, LISPTYPE_RATIO);
	GetSignRatio(left, &sign);
	if (IsPlus(sign))
		ratio_throw_alloc(local, left, ret);
	else
		ratio_copy_nosign_alloc(local, ret, left);
}

void abs_ratio_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	abs_ratio_alloc(local, left, ret);
}

void abs_ratio_heap(addr left, addr *ret)
{
	abs_ratio_alloc(NULL, left, ret);
}


/*
 *  output
 */
int output_nosign_ratio_(LocalRoot local,
		addr stream, addr pos, unsigned base, int upperp)
{
	addr numer, denom;

	Check(! isBaseChar(base), "base error");
	/* zero */
	GetNumerRatio(pos, &numer);
	if (zerop_bignum(numer))
		return write_char_stream_(stream, '0');

	/* integer */
	GetDenomRatio(pos, &denom);
	if (equal_value_nosign_bignum(denom, 1))
		return output_nosign_bignum_(local, stream, numer, base, upperp);

	/* ratio */
	Return(output_nosign_bignum_(local, stream, numer, base, upperp));
	Return(write_char_stream_(stream, '/'));
	Return(output_nosign_bignum_(local, stream, denom, base, upperp));

	return 0;
}


/************************************************************
 *  ratio_equal.c
 ************************************************************/

int equal_value_nosign_ratio(addr pos, fixed numer, fixed denom)
{
	addr pos1, pos2;

	CheckType(pos, LISPTYPE_RATIO);
	GetNumerRatio(pos, &pos1);
	GetDenomRatio(pos, &pos2);
	return equal_value_nosign_bignum(pos1, numer)
		&& equal_value_nosign_bignum(pos2, denom);
}

int equal_value_ratio(addr pos, int sign, fixed numer, fixed denom)
{
	CheckType(pos, LISPTYPE_RATIO);
	return (RefSignRatio(pos) == sign)
		&& equal_value_nosign_ratio(pos, numer, denom);
}

int equal_fr_real(addr left, addr right)
{
	int sign;
	addr pos;
	fixed value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	/* denom == 1 */
	GetDenomRatio(right, &pos);
	if (! equal_value_nosign_bignum(pos, 1))
		return 0;
	/* numer */
	GetNumerRatio(right, &pos);
	castfixed_fixnum(left, &sign, &value);
	if (! equal_value_nosign_bignum(pos, value))
		return 0;
	/* sign */
	return RefSignRatio(right) == sign;
}

int equal_br_real(addr left, addr right)
{
	addr pos;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	/* denom == 1 */
	GetDenomRatio(right, &pos);
	if (! equal_value_nosign_bignum(pos, 1))
		return 0;
	/* numer */
	GetNumerRatio(right, &pos);
	if (! equal_bigdata(left, pos))
		return 0;
	/* sign */
	return RefSignBignum(left) == RefSignRatio(right);
}

int equal_rr_real(addr left, addr right)
{
	addr pos1, pos2;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	/* sign */
	if (RefSignRatio(left) != RefSignRatio(right))
		return 0;
	/* numer */
	GetNumerRatio(left, &pos1);
	GetNumerRatio(right, &pos2);
	if (! equal_bb_real(pos1, pos2))
		return 0;
	/* denom */
	GetDenomRatio(left, &pos1);
	GetDenomRatio(right, &pos2);
	return equal_bb_real(pos1, pos2);
}

static void split_single_float(single_float value, int *rs, int *re, single_float *rv)
{
	int exp;

	if (value < 0.0f) {
		*rs = SignMinus;
		value = -value;
	}
	else {
		*rs = SignPlus;
	}
	value = frexpf(value, &exp);
	while (! IsIntegerFloat(value)) {
		value *= 2.0f;
		exp--;
	}
	*re = exp;
	*rv = value;
}

static void split_double_float(double_float value, int *rs, int *re, double_float *rv)
{
	int exp;

	if (value < 0) {
		*rs = SignMinus;
		value = -value;
	}
	else {
		*rs = SignPlus;
	}
	value = frexp(value, &exp);
	while (! IsIntegerDouble(value)) {
		value *= 2.0;
		exp--;
	}
	*re = exp;
	*rv = value;
}

static void split_long_float(long_float value, int *rs, int *re, long_float *rv)
{
	int exp;

	if (value < 0.0L) {
		*rs = SignMinus;
		value = -value;
	}
	else {
		*rs = SignPlus;
	}
	value = frexpl(value, &exp);
	while (! IsIntegerLongFloat(value)) {
		value *= 2.0L;
		exp--;
	}
	*re = exp;
	*rv = value;
}

static void rational_return_local(LocalRoot local,
		int sign, int exp, addr numer, addr *ret)
{
	addr denom;

	if (exp == 0) {
		SetSignBignum(numer, sign);
		bignum_result_local(local, numer, ret);
	}
	else if (0 < exp) {
		SetSignBignum(numer, sign);
		shiftup_bignum_local(local, ret, numer, (size_t)exp);
	}
	else {
		power2_bigdata_alloc(local, &denom, (size_t)-exp);
		ratio_reduction_nocopy_local(local, ret, sign, numer, denom);
	}
}

static int rational_float_single_local_(LocalRoot local, single_float value, addr *ret)
{
	int sign, exponent;
	addr numer;

	split_single_float(value, &sign, &exponent, &value);
	Return(bignum_single_float_local_(local, value, &numer, NULL));
	rational_return_local(local, sign, exponent, numer, ret);

	return 0;
}

static int rational_float_double_local_(LocalRoot local, double_float value, addr *ret)
{
	int sign, exponent;
	addr numer;

	split_double_float(value, &sign, &exponent, &value);
	Return(bignum_double_float_local_(local, value, &numer, NULL));
	rational_return_local(local, sign, exponent, numer, ret);

	return 0;
}

static int rational_float_long_local_(LocalRoot local, long_float value, addr *ret)
{
	int sign, exponent;
	addr numer;

	split_long_float(value, &sign, &exponent, &value);
	Return(bignum_long_float_local_(local, value, &numer, NULL));
	rational_return_local(local, sign, exponent, numer, ret);

	return 0;
}

static int equal_ratio_type_(addr left, addr right, int *ret)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, equal_fr_real(right, left));

		case LISPTYPE_BIGNUM:
			return Result(ret, equal_br_real(right, left));

		case LISPTYPE_RATIO:
			return Result(ret, equal_rr_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, INTEGER);
	}
}

int equal_rs_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	LocalStack stack;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	push_local(local, &stack);
	Return(rational_float_single_local_(local, RefSingleFloat(right), &right));
	Return(equal_ratio_type_(left, right, &check));
	rollback_local(local, stack);

	return Result(ret, check);
}

int equal_rd_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	LocalStack stack;

	push_local(local, &stack);
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	Return(rational_float_double_local_(local, RefDoubleFloat(right), &right));
	Return(equal_ratio_type_(left, right, &check));
	rollback_local(local, stack);

	return Result(ret, check);
}

int equal_rl_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	LocalStack stack;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	push_local(local, &stack);
	Return(rational_float_long_local_(local, RefLongFloat(right), &right));
	Return(equal_ratio_type_(left, right, &check));
	rollback_local(local, stack);

	return Result(ret, check);
}

static int compare_fixed_bignum(fixed left, addr right)
{
	fixed value;

	if (1 < RefSizeBignum(right))
		return -1;
	GetRootBignum(right, &right);
	value = PtrDataBignum(right)[0];
	if (left < value)
		return -1;
	if (left > value)
		return 1;

	return 0;
}

static int compare_fixed_ratio_nosign(LocalRoot local, fixed left, addr right)
{
	int result;
	addr numer, pos;
	LocalStack stack;
	size_t size;

	/* denom == 1 */
	GetNumerRatio(right, &numer);
	GetDenomRatio(right, &right);
	if (equal_value_nosign_bignum(right, 1))
		return compare_fixed_bignum(left, numer);

	/* compare */
	push_local(local, &stack);
	GetSizeBignum(numer, &size);
	alloc_bignum(local, &pos, size + 1);
	setmultivalue_bigdata(pos, right, left);
	Check(IsMinus(RefSignBignum(pos)), "sign pos error");
	Check(IsMinus(RefSignBignum(numer)), "sign numer error");
	result = compare_bigdata(pos, numer);
	rollback_local(local, stack);

	return result;
}

int compare_fr_real(LocalRoot local, addr left, addr right)
{
	int sign1, sign2, result;
	fixed value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	castfixed_fixnum(left, &sign1, &value);
	result = zerop_ratio(right);
	if (value == 0) {
		if (result)
			return 0;
		GetSignRatio(right, &sign2);
		return IsPlus(sign2)? -1: 1;
	}
	if (result) {
		return IsPlus(sign1)? 1: -1;
	}
	GetSignRatio(right, &sign2);
	if (IsPlus(sign1) && IsMinus(sign2))
		return 1;
	if (IsMinus(sign1) && IsPlus(sign2))
		return -1;
	result = compare_fixed_ratio_nosign(local, value, right);

	return IsPlus(sign1)? result: -result;
}

int compare_rf_real(LocalRoot local, addr left, addr right)
{
	return -compare_fr_real(local, right, left);
}

static int compare_bigdata_ratio_nosign(LocalRoot local, addr left, addr right)
{
	int result;
	addr numer;
	LocalStack stack;

	GetNumerRatio(right, &numer);
	GetDenomRatio(right, &right);
	if (equal_value_nosign_bignum(right, 1))
		return compare_bigdata(left, numer);

	push_local(local, &stack);
	multi_bb_nosign_bignum_local(local, left, right, &left);
	result = compare_bigdata(left, numer);
	rollback_local(local, stack);

	return result;
}

int compare_br_real(LocalRoot local, addr left, addr right)
{
	int sign1, sign2, result, check;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	check = zerop_ratio(right);
	if (zerop_bignum(left)) {
		if (check)
			return 0;
		GetSignRatio(right, &sign2);
		return IsPlus(sign2)? -1: 1;
	}
	if (check) {
		GetSignBignum(left, &sign1);
		return IsPlus(sign1)? 1: -1;
	}
	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	if (IsPlus(sign1) && IsMinus(sign2))
		return 1;
	if (IsMinus(sign1) && IsPlus(sign2))
		return -1;
	result = compare_bigdata_ratio_nosign(local, left, right);

	return IsPlus(sign1)? result: -result;
}

int compare_rb_real(LocalRoot local, addr left, addr right)
{
	return -compare_br_real(local, right, left);
}

static int compare_ratio_local(LocalRoot local, addr left, addr right)
{
	int result;
	addr denom1, denom2;
	LocalStack stack;

	GetDenomRatio(left, &denom1);
	GetDenomRatio(right, &denom2);
	push_local(local, &stack);
	bignum_copy_local(local, &denom1, denom1);
	bignum_copy_local(local, &denom2, denom2);
	reduction_local(local, denom1, denom2);
	/* cross multiple */
	GetNumerRatio(left, &left);
	GetNumerRatio(right, &right);
	multi_bb_nosign_bignum_local(local, left, denom2, &left);
	multi_bb_nosign_bignum_local(local, right, denom1, &right);
	result = compare_bigdata(left, right);
	rollback_local(local, stack);

	return result;
}

int compare_rr_real(LocalRoot local, addr left, addr right)
{
	int check1, check2, sign1, sign2;
	addr denom1, denom2;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	/* zero check */
	check1 = zerop_ratio(left);
	check2 = zerop_ratio(right);
	if (check1 && check2)
		return 0;
	if (check1) {
		GetSignRatio(right, &sign2);
		return IsPlus(sign2)? -1: 1;
	}
	if (check2) {
		GetSignRatio(left, &sign1);
		return IsPlus(sign1)? 1: -1;
	}

	/* sign check */
	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	if (IsPlus(sign1) && IsMinus(sign2))
		return 1;
	if (IsMinus(sign1) && IsPlus(sign2))
		return -1;

	/* denom check */
	GetDenomRatio(left, &denom1);
	GetDenomRatio(right, &denom2);
	if (equal_bb_real(denom1, denom2)) {
		GetNumerRatio(left, &left);
		GetNumerRatio(right, &right);
		check1 = compare_bigdata(left, right);
		return IsPlus(sign1)? check1: -check1;
	}

	/* compare */
	check1 = compare_ratio_local(local, left, right);
	return IsPlus(sign1)? check1: -check1;
}

int compare_rs_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	LocalStack stack;

	push_local(local, &stack);
	Return(rational_float_single_local_(local, RefSingleFloat(right), &right));
	Return(compare_ratio_real_(local, left, right, &check));
	rollback_local(local, stack);

	return Result(ret, check);
}

int compare_rd_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	LocalStack stack;

	push_local(local, &stack);
	Return(rational_float_double_local_(local, RefDoubleFloat(right), &right));
	Return(compare_ratio_real_(local, left, right, &check));
	rollback_local(local, stack);

	return Result(ret, check);
}

int compare_rl_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	LocalStack stack;

	push_local(local, &stack);
	Return(rational_float_long_local_(local, RefLongFloat(right), &right));
	Return(compare_ratio_real_(local, left, right, &check));
	rollback_local(local, stack);

	return Result(ret, check);
}

int compare_sr_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_rs_real_(local, right, left, &check));
	return Result(ret, -check);
}

int compare_dr_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_rd_real_(local, right, left, &check));
	return Result(ret, -check);
}

int compare_lr_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_rl_real_(local, right, left, &check));
	return Result(ret, -check);
}


/************************************************************
 *  ratio_multi.c
 ************************************************************/

static int equal_rv_nosign(addr left, fixed value)
{
	addr check;

	Check(GetType(left) != LISPTYPE_RATIO, "type error");
	GetDenomRatio(left, &check);
	if (! equal_value_nosign_bignum(check, 1))
		return 0;
	GetNumerRatio(left, &check);

	return equal_value_nosign_bignum(check, value);
}

static void multi_rv_result(LocalRoot local,
		addr left, fixed value, addr *rnumer, addr *rdenom)
{
	addr right, numer, denom;

	/*
	 *  numer   value
	 *  ----- * -----
	 *  denom     1
	 */
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	bignum_value_local(local, &right, SignPlus, value);
	bignum_copy_local(local, &denom, denom);
	reduction_local(local, right, denom);
	multi_bigdata_alloc(local, right, numer, &numer);
	*rnumer = numer;
	*rdenom = denom;
}

static void multi_rv_ratio(LocalRoot local,
		int sign, addr left, fixed value, addr *ret)
{
	addr numer, denom;

	multi_rv_result(local, left, value, &numer, &denom);
	make_ratio_reduction_local(local, ret, sign, numer, denom);
}

static void multi_rv_local(LocalRoot local,
		int sign, addr left, fixed value, addr *ret)
{
	addr numer, denom;

	multi_rv_result(local, left, value, &numer, &denom);
	ratio_noreduction_local(local, ret, sign, numer, denom);
}

static void multi_rv_common(LocalRoot local,
		int sign, addr left, fixed value, addr *ret)
{
	LocalStack stack;
	addr numer, denom;

	push_local(local, &stack);
	multi_rv_result(local, left, value, &numer, &denom);
	ratio_noreduction_heap(ret, sign, numer, denom);
	rollback_local(local, stack);
}

void multi_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* ratio */
	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
		return;
	}
	GetSignRatio(left, &sign1);
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			cast_fixnum_ratio_local(local, right, ret);
		else
			sigrev_fixnum_ratio_local(local, right, ret);
		return;
	}

	/* fixnum */
	castfixed_fixnum(right, &sign2, &value);
	if (value == 0) {
		ratio_zero_local(local, ret);
		return;
	}
	if (value == 1) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}

	/* multiple */
	sign1 = SignMulti(sign1, sign2);
	multi_rv_ratio(local, sign1, left, value, ret);
}

void multi_rf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_local(local, ret, 0);
		return;
	}
	GetSignRatio(left, &sign1);
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			fixnum_throw_local(local, right, ret);
		else
			sigrev_fixnum_integer_local(local, right, ret);
		return;
	}

	/* fixnum */
	castfixed_fixnum(right, &sign2, &value);
	if (value == 0) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (value == 1) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}

	/* multiple */
	sign1 = SignMulti(sign1, sign2);
	multi_rv_local(local, sign1, left, value, ret);
}

void multi_rf_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_heap(ret, 0);
		return;
	}
	GetSignRatio(left, &sign1);
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			fixnum_throw_heap(right, ret);
		else
			sigrev_fixnum_integer_common(right, ret);
		return;
	}

	/* fixnum */
	castfixed_fixnum(right, &sign2, &value);
	if (value == 0) {
		fixnum_heap(ret, 0);
		return;
	}
	if (value == 1) {
		if (IsPlus(sign2))
			ratio_throw_heap(left, ret);
		else
			sign_reverse_ratio_common(left, ret);
		return;
	}

	/* multiple */
	sign1 = SignMulti(sign1, sign2);
	multi_rv_common(local, sign1, left, value, ret);
}

static void multi_rb_result(LocalRoot local,
		addr left, addr right, addr *rnumer, addr *rdenom)
{
	addr numer, denom;

	/*
	 *  numer   right
	 *  ----- * -----
	 *  denom     1
	 */
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	bignum_copy_local(local, &right, right);
	bignum_copy_local(local, &denom, denom);
	reduction_local(local, right, denom);
	multi_bigdata_alloc(local, right, numer, &numer);
	*rnumer = numer;
	*rdenom = denom;
}

static void multi_rb_ratio(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	multi_rb_result(local, left, right, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void multi_rb_local(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	multi_rb_result(local, left, right, &left, &right);
	ratio_noreduction_local(local, ret, sign, left, right);
}

static void multi_rb_common(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	multi_rb_result(local, left, right, &left, &right);
	ratio_noreduction_heap(ret, sign, left, right);
	rollback_local(local, stack);
}

void multi_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* ratio */
	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
		return;
	}
	GetSignRatio(left, &sign1);
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			cast_bignum_ratio_local(local, right, ret);
		else
			sigrev_bignum_ratio_local(local, right, ret);
		return;
	}

	/* bignum */
	if (zerop_bignum(right)) {
		ratio_zero_local(local, ret);
		return;
	}
	GetSignBignum(right, &sign2);
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}

	/* multiple */
	sign1 = SignMulti(sign1, sign2);
	multi_rb_ratio(local, sign1, left, right, ret);
}

void multi_rb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_local(local, ret, 0);
		return;
	}
	GetSignRatio(left, &sign1);
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			bignum_throw_local(local, right, ret);
		else
			sigrev_bignum_integer_local(local, right, ret);
		return;
	}

	/* bignum */
	if (zerop_bignum(right)) {
		fixnum_local(local, ret, 0);
		return;
	}
	GetSignBignum(right, &sign2);
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}

	/* multiple */
	sign1 = SignMulti(sign1, sign2);
	multi_rb_local(local, sign1, left, right, ret);
}

void multi_rb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_heap(ret, 0);
		return;
	}
	GetSignRatio(left, &sign1);
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			bignum_throw_heap(right, ret);
		else
			sigrev_bignum_integer_common(right, ret);
		return;
	}

	/* bignum */
	if (zerop_bignum(right)) {
		fixnum_heap(ret, 0);
		return;
	}
	GetSignBignum(right, &sign2);
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_heap(left, ret);
		else
			sign_reverse_ratio_common(left, ret);
		return;
	}

	/* multiple */
	sign1 = SignMulti(sign1, sign2);
	multi_rb_common(local, sign1, left, right, ret);
}

static void div_rb_result(LocalRoot local,
		addr left, addr right, addr *rnumer, addr *rdenom)
{
	addr numer, denom;

	/*
	 *  numer           numer     1
	 *  ----- / right = ----- * -----
	 *  denom           denom   right
	 */
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	bignum_copy_local(local, &right, right);
	bignum_copy_local(local, &numer, numer);
	reduction_local(local, numer, right);
	multi_bigdata_alloc(local, denom, right, &denom);
	*rnumer = numer;
	*rdenom = denom;
}

static void div_rb_ratio(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_rb_result(local, left, right, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void div_rb_local(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_rb_result(local, left, right, &left, &right);
	ratio_noreduction_local(local, ret, sign, left, right);
}

static void div_rb_common(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	div_rb_result(local, left, right, &left, &right);
	ratio_noreduction_heap(ret, sign, left, right);
	rollback_local(local, stack);
}

static void multi_rr_result(LocalRoot local,
		addr left, addr right, addr *rnumer, addr *rdenom)
{
	addr numer1, numer2, denom1, denom2;

	/*
	 *  numer1   numer2
	 *  ------ * ------
	 *  denom1   denom2
	 */
	GetNumerRatio(left, &numer1);
	GetDenomRatio(left, &denom1);
	GetNumerRatio(right, &numer2);
	GetDenomRatio(right, &denom2);
	bignum_copy_local(local, &numer1, numer1);
	bignum_copy_local(local, &denom1, denom1);
	bignum_copy_local(local, &numer2, numer2);
	bignum_copy_local(local, &denom2, denom2);
	reduction_local(local, numer1, denom2);
	reduction_local(local, numer2, denom1);
	multi_bigdata_alloc(local, numer1, numer2, &numer1);
	multi_bigdata_alloc(local, denom1, denom2, &denom1);
	*rnumer = numer1;
	*rdenom = denom1;
}

static void multi_rr_ratio(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	multi_rr_result(local, left, right, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void multi_rr_local(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	multi_rr_result(local, left, right, &left, &right);
	ratio_noreduction_local(local, ret, sign, left, right);
}

static void multi_rr_common(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	multi_rr_result(local, left, right, &left, &right);
	ratio_noreduction_heap(ret, sign, left, right);
	rollback_local(local, stack);
}

static inline int inverse_ratio_p(addr pos)
{
	GetNumerRatio(pos, &pos);
	return equal_value_nosign_bignum(pos, 1);
}

void multi_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign, sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	sign = SignMulti(sign1, sign2);

	/* left */
	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
		return;
	}
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			ratio_throw_local(local, right, ret);
		else
			sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (inverse_ratio_p(left)) {
		GetDenomRatio(left, &left);
		div_rb_ratio(local, sign, right, left, ret);
		return;
	}

	/* right */
	if (zerop_ratio(right)) {
		ratio_zero_local(local, ret);
		return;
	}
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}
	if (inverse_ratio_p(right)) {
		GetDenomRatio(right, &right);
		div_rb_ratio(local, sign, left, right, ret);
		return;
	}

	/* multiple */
	multi_rr_ratio(local, sign, left, right, ret);
}

void multi_rr_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign, sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	sign = SignMulti(sign1, sign2);

	/* left */
	if (zerop_ratio(left)) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			ratio_throw_local(local, right, ret);
		else
			sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (inverse_ratio_p(left)) {
		GetDenomRatio(left, &left);
		div_rb_local(local, sign, right, left, ret);
		return;
	}

	/* right */
	if (zerop_ratio(right)) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}
	if (inverse_ratio_p(right)) {
		GetDenomRatio(right, &right);
		div_rb_local(local, sign, left, right, ret);
		return;
	}

	/* multiple */
	multi_rr_local(local, sign, left, right, ret);
}

void multi_rr_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign, sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	sign = SignMulti(sign1, sign2);

	/* left */
	if (zerop_ratio(left)) {
		fixnum_heap(ret, 0);
		return;
	}
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			ratio_throw_heap(right, ret);
		else
			sign_reverse_ratio_common(right, ret);
		return;
	}
	if (inverse_ratio_p(left)) {
		GetDenomRatio(left, &left);
		div_rb_common(local, sign, right, left, ret);
		return;
	}

	/* right */
	if (zerop_ratio(right)) {
		fixnum_heap(ret, 0);
		return;
	}
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_heap(left, ret);
		else
			sign_reverse_ratio_common(left, ret);
		return;
	}
	if (inverse_ratio_p(right)) {
		GetDenomRatio(right, &right);
		div_rb_common(local, sign, left, right, ret);
		return;
	}

	/* multiple */
	multi_rr_common(local, sign, left, right, ret);
}


/*
 *  division
 */
static inline void inverse_value_ratio(LocalRoot local,
		addr *ret, int sign, fixed value)
{
	ratio_noreduction_value_local(local, ret, sign, 1, value);
}

static inline void inverse_value_local(LocalRoot local,
		addr *ret, int sign, fixed value)
{
	if (value == 1)
		fixnum_local(local, ret, IsPlus(sign)? 1: -1);
	else
		ratio_noreduction_value_local(local, ret, sign, 1, value);
}

static inline void inverse_value_common(addr *ret, int sign, fixed value)
{
	if (value == 1)
		fixnum_heap(ret, IsPlus(sign)? 1: -1);
	else
		ratio_noreduction_value_heap(ret, sign, 1, value);
}

static void div_rv_result(LocalRoot local,
		addr left, fixed value, addr *rnumer, addr *rdenom)
{
	addr right, numer, denom;

	/*
	 *  numer           numer     1
	 *  ----- / value = ----- * -----
	 *  denom           denom   value
	 */
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	bignum_value_local(local, &right, SignPlus, value);
	bignum_copy_local(local, &numer, numer);
	reduction_local(local, numer, right);
	multi_bigdata_alloc(local, denom, right, &denom);
	*rnumer = numer;
	*rdenom = denom;
}

static void div_rv_ratio(LocalRoot local,
		int sign, addr left, fixed value, addr *ret)
{
	addr numer, denom;

	div_rv_result(local, left, value, &numer, &denom);
	make_ratio_reduction_local(local, ret, sign, numer, denom);
}

static void div_rv_local(LocalRoot local,
		int sign, addr left, fixed value, addr *ret)
{
	addr numer, denom;

	div_rv_result(local, left, value, &numer, &denom);
	ratio_noreduction_local(local, ret, sign, numer, denom);
}

static void div_rv_common(LocalRoot local, int sign, addr left, fixed value, addr *ret)
{
	LocalStack stack;
	addr numer, denom;

	push_local(local, &stack);
	div_rv_result(local, left, value, &numer, &denom);
	ratio_noreduction_heap(ret, sign, numer, denom);
	rollback_local(local, stack);
}

int div_rf_ratio_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	GetSignRatio(left, &sign1);
	castfixed_fixnum(right, &sign2, &value);
	sign1 = SignMulti(sign1, sign2);
	if (value == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		inverse_value_ratio(local, ret, sign1, value);
		return 0;
	}

	/* fixnum */
	if (value == 1) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return 0;
	}

	/* division */
	div_rv_ratio(local, sign1, left, value, ret);

	return 0;
}

int div_rf_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	GetSignRatio(left, &sign1);
	castfixed_fixnum(right, &sign2, &value);
	sign1 = SignMulti(sign1, sign2);
	if (value == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		inverse_value_local(local, ret, sign1, value);
		return 0;
	}

	/* fixnum */
	if (value == 1) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return 0;
	}

	/* division */
	div_rv_local(local, sign1, left, value, ret);

	return 0;
}

int div_rf_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	GetSignRatio(left, &sign1);
	castfixed_fixnum(right, &sign2, &value);
	sign1 = SignMulti(sign1, sign2);
	if (value == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		inverse_value_common(ret, sign1, value);
		return 0;
	}

	/* fixnum */
	if (value == 1) {
		if (IsPlus(sign2))
			ratio_throw_heap(left, ret);
		else
			sign_reverse_ratio_common(left, ret);
		return 0;
	}

	/* division */
	div_rv_common(local, sign1, left, value, ret);

	return 0;
}

static void ratio_sign_inverse_ratio(LocalRoot local, addr *ret, int sign, addr pos)
{
	addr numer, denom;

	Check(local == NULL, "local error");
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	bignum_throw_local(local, numer, &numer);
	bignum_throw_local(local, denom, &denom);
	make_ratio_local(local, ret, sign, denom, numer);
}

static void ratio_sign_inverse_local(LocalRoot local, addr *ret, int sign, addr pos)
{
	addr numer, denom;

	Check(local == NULL, "local error");
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (equal_value_nosign_bignum(numer, 1)) {
		bignum_copy_nosign_local(local, &pos, denom);
		SetSignBignum(pos, sign);
		bignum_result_local(local, pos, ret);
	}
	else {
		bignum_throw_local(local, numer, &numer);
		bignum_throw_local(local, denom, &denom);
		make_ratio_local(local, ret, sign, denom, numer);
	}
}

static void ratio_sign_inverse_common(LocalRoot local, addr *ret, int sign, addr pos)
{
	addr numer, denom;
	LocalStack stack;

	Check(local == NULL, "local error");
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (equal_value_nosign_bignum(numer, 1)) {
		push_local(local, &stack);
		bignum_copy_nosign_local(local, &pos, denom);
		SetSignBignum(pos, sign);
		bignum_result_heap(pos, ret);
		rollback_local(local, stack);
	}
	else {
		bignum_throw_heap(numer, &numer);
		bignum_throw_heap(denom, &denom);
		make_ratio_heap(ret, sign, denom, numer);
	}
}

static void div_vr_result(LocalRoot local,
		fixed value, addr right, addr *rnumer, addr *rdenom)
{
	addr left, numer, denom;

	/*
	 *          numer     value   denom
	 *  value / -----  =  ----- * -----
	 *          denom       1     numer
	 */
	GetNumerRatio(right, &numer);
	GetDenomRatio(right, &denom);
	bignum_value_local(local, &left, SignPlus, value);
	bignum_copy_local(local, &numer, numer);
	reduction_local(local, left, numer);
	multi_bigdata_alloc(local, left, denom, &denom);
	*rnumer = numer;
	*rdenom = denom;
}

static void div_vr_ratio(LocalRoot local,
		int sign, fixed value, addr right, addr *ret)
{
	addr numer, denom;

	div_vr_result(local, value, right, &numer, &denom);
	make_ratio_reduction_local(local, ret, sign, denom, numer);
}

static void div_vr_local(LocalRoot local,
		int sign, fixed value, addr right, addr *ret)
{
	addr numer, denom;

	div_vr_result(local, value, right, &numer, &denom);
	ratio_noreduction_local(local, ret, sign, denom, numer);
}

static void div_vr_common(LocalRoot local,
		int sign, fixed value, addr right, addr *ret)
{
	addr numer, denom;
	LocalStack stack;

	push_local(local, &stack);
	div_vr_result(local, value, right, &numer, &denom);
	ratio_noreduction_heap(ret, sign, denom, numer);
	rollback_local(local, stack);
}

int div_fr_ratio_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	castfixed_fixnum(left, &sign1, &value);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* fixnum */
	if (value == 0) {
		ratio_zero_local(local, ret);
		return 0;
	}
	if (value == 1) {
		ratio_sign_inverse_ratio(local, ret, sign1, right);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			cast_fixnum_ratio_local(local, left, ret);
		else
			sigrev_fixnum_ratio_local(local, left, ret);
		return 0;
	}

	/* division */
	div_vr_ratio(local, sign1, value, right, ret);

	return 0;
}

int div_fr_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	castfixed_fixnum(left, &sign1, &value);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* fixnum */
	if (value == 0) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (value == 1) {
		ratio_sign_inverse_local(local, ret, sign1, right);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			fixnum_throw_local(local, left, ret);
		else
			sigrev_fixnum_integer_local(local, left, ret);
		return 0;
	}

	/* division */
	div_vr_local(local, sign1, value, right, ret);

	return 0;
}

int div_fr_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	castfixed_fixnum(left, &sign1, &value);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* fixnum */
	if (value == 0) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (value == 1) {
		ratio_sign_inverse_common(local, ret, sign1, right);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			fixnum_throw_heap(left, ret);
		else
			sigrev_fixnum_integer_common(left, ret);
		return 0;
	}

	/* division */
	div_vr_common(local, sign1, value, right, ret);

	return 0;
}

static void bignum_throw_ratio_local(LocalRoot local, addr pos, addr *ret)
{
	if (IsMinus(RefSignBignum(pos))) {
		bignum_copy_local(local, ret, pos);
		SetSignBignum(*ret, SignPlus);
	}
	else {
		bignum_throw_local(local, pos, ret);
	}
}

static void bignum_throw_ratio_common(addr pos, addr *ret)
{
	if (IsMinus(RefSignBignum(pos))) {
		bignum_copy_heap(ret, pos);
		SetSignBignum(*ret, SignPlus);
	}
	else {
		bignum_throw_heap(pos, ret);
	}
}

static inline void bignum_sign_inverse_ratio(LocalRoot local,
		addr *ret, int sign, addr denom)
{
	addr numer;

	Check(local == NULL, "local error");
	bignum_value_local(local, &numer, SignPlus, 1);
	bignum_throw_ratio_local(local, denom, &denom);
	make_ratio_local(local, ret, sign, numer, denom);
}

static inline void bignum_sign_inverse_local(LocalRoot local,
		addr *ret, int sign, addr denom)
{
	addr numer;

	Check(local == NULL, "local error");
	if (equal_value_nosign_bignum(denom, 1)) {
		fixnum_local(local, ret, IsPlus(sign)? 1: -1);
	}
	else {
		bignum_value_local(local, &numer, SignPlus, 1);
		bignum_throw_ratio_local(local, denom, &denom);
		make_ratio_local(local, ret, sign, numer, denom);
	}
}

static inline void bignum_sign_inverse_common(addr *ret, int sign, addr denom)
{
	addr numer;

	if (equal_value_nosign_bignum(denom, 1)) {
		fixnum_heap(ret, IsPlus(sign)? 1: -1);
	}
	else {
		bignum_value_heap(&numer, SignPlus, 1);
		bignum_throw_ratio_common(denom, &denom);
		make_ratio_heap(ret, sign, numer, denom);
	}
}

int div_rb_ratio_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		bignum_sign_inverse_ratio(local, ret, sign1, right);
		return 0;
	}

	/* bignum */
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return 0;
	}

	/* division */
	div_rb_ratio(local, sign1, left, right, ret);

	return 0;
}

int div_rb_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		bignum_sign_inverse_local(local, ret, sign1, right);
		return 0;
	}

	/* bignum */
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return 0;
	}

	/* division */
	div_rb_local(local, sign1, left, right, ret);

	return 0;
}

int div_rb_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		bignum_sign_inverse_common(ret, sign1, right);
		return 0;
	}

	/* bignum */
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_heap(left, ret);
		else
			sign_reverse_ratio_common(left, ret);
		return 0;
	}

	/* division */
	div_rb_common(local, sign1, left, right, ret);

	return 0;
}

static void div_br_result(LocalRoot local,
		addr left, addr right, addr *rnumer, addr *rdenom)
{
	addr numer, denom;

	/*
	 *          numer  left    denom
	 *  left / ----- = ----- * -----
	 *          denom    1     numer
	 */
	GetNumerRatio(right, &numer);
	GetDenomRatio(right, &denom);
	bignum_copy_local(local, &left, left);
	bignum_copy_local(local, &numer, numer);
	reduction_local(local, left, numer);
	multi_bigdata_alloc(local, left, denom, &denom);
	*rnumer = denom;
	*rdenom = numer;
}

static void div_br_ratio(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_br_result(local, left, right, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void div_br_local(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_br_result(local, left, right, &left, &right);
	ratio_noreduction_local(local, ret, sign, left, right);
}

static void div_br_common(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	div_br_result(local, left, right, &left, &right);
	ratio_noreduction_heap(ret, sign, left, right);
	rollback_local(local, stack);
}

int div_br_ratio_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* bignum */
	if (zerop_bignum(left)) {
		ratio_zero_local(local, ret);
		return 0;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		ratio_sign_inverse_ratio(local, ret, sign1, right);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			cast_bignum_ratio_local(local, left, ret);
		else
			sigrev_bignum_ratio_local(local, left, ret);
		return 0;
	}

	/* division */
	div_br_ratio(local, sign1, left, right, ret);

	return 0;
}

int div_br_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* bignum */
	if (zerop_bignum(left)) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		ratio_sign_inverse_local(local, ret, sign1, right);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			bignum_throw_local(local, left, ret);
		else
			sigrev_bignum_integer_local(local, left, ret);
		return 0;
	}

	/* division */
	div_br_local(local, sign1, left, right, ret);

	return 0;
}

int div_br_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = 0;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* bignum */
	if (zerop_bignum(left)) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		ratio_sign_inverse_common(local, ret, sign1, right);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			bignum_throw_heap(left, ret);
		else
			sigrev_bignum_integer_common(left, ret);
		return 0;
	}

	/* division */
	div_br_common(local, sign1, left, right, ret);

	return 0;
}

static void div_rr_result(LocalRoot local,
		addr left, addr right, addr *rnumer, addr *rdenom)
{
	addr numer1, numer2, denom1, denom2;

	/*
	 *  numer1   numer2   numer1  denom2
	 *  ------ / ------ = ----- * ------
	 *  denom1   denom2   denom1  numer2
	 */
	GetNumerRatio(left, &numer1);
	GetDenomRatio(left, &denom1);
	GetNumerRatio(right, &denom2);
	GetDenomRatio(right, &numer2);
	bignum_copy_local(local, &numer1, numer1);
	bignum_copy_local(local, &denom1, denom1);
	bignum_copy_local(local, &numer2, numer2);
	bignum_copy_local(local, &denom2, denom2);
	reduction_local(local, numer1, denom2);
	reduction_local(local, numer2, denom1);
	multi_bigdata_alloc(local, numer1, numer2, &numer1);
	multi_bigdata_alloc(local, denom1, denom2, &denom1);
	*rnumer = numer1;
	*rdenom = denom1;
}

static void div_rr_ratio(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_rr_result(local, left, right, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void div_rr_local(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_rr_result(local, left, right, &left, &right);
	ratio_noreduction_local(local, ret, sign, left, right);
}

static void div_rr_common(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	div_rr_result(local, left, right, &left, &right);
	ratio_noreduction_heap(ret, sign, left, right);
	rollback_local(local, stack);
}

static void div_bir_result(LocalRoot local,
		addr left, addr right, addr *rnumer, addr *rdenom)
{
	addr numer, denom;

	/*
	 *    1     numer     1     denom
	 *  ----- / ----- = ----- * -----
	 *  left    denom   left    numer
	 */
	GetNumerRatio(right, &numer);
	GetDenomRatio(right, &denom);
	bignum_copy_local(local, &left, left);
	bignum_copy_local(local, &denom, denom);
	reduction_local(local, left, denom);
	multi_bigdata_alloc(local, left, numer, &numer);
	*rnumer = denom;
	*rdenom = numer;
}

static void div_bir_ratio(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_bir_result(local, left, right, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void div_bir_local(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_bir_result(local, left, right, &left, &right);
	ratio_noreduction_local(local, ret, sign, left, right);
}

static void div_bir_common(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	div_bir_result(local, left, right, &left, &right);
	ratio_noreduction_heap(ret, sign, left, right);
	rollback_local(local, stack);
}

int div_rr_ratio_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		ratio_sign_inverse_ratio(local, ret, sign1, right);
		return 0;
	}
	if (inverse_ratio_p(left)) {
		GetDenomRatio(left, &left);
		div_bir_ratio(local, sign1, left, right, ret);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return 0;
	}
	if (inverse_ratio_p(right)) {
		GetDenomRatio(right, &right);
		div_rb_ratio(local, sign1, left, right, ret);
		return 0;
	}

	/* division */
	div_rr_ratio(local, sign1, left, right, ret);

	return 0;
}

int div_rr_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		ratio_sign_inverse_local(local, ret, sign1, right);
		return 0;
	}
	if (inverse_ratio_p(left)) {
		GetDenomRatio(left, &left);
		div_bir_local(local, sign1, left, right, ret);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return 0;
	}
	if (inverse_ratio_p(right)) {
		GetDenomRatio(right, &right);
		div_rb_local(local, sign1, left, right, ret);
		return 0;
	}

	/* division */
	div_rr_local(local, sign1, left, right, ret);

	return 0;
}

int div_rr_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		ratio_sign_inverse_common(local, ret, sign1, right);
		return 0;
	}
	if (inverse_ratio_p(left)) {
		GetDenomRatio(left, &left);
		div_bir_common(local, sign1, left, right, ret);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_heap(left, ret);
		else
			sign_reverse_ratio_common(left, ret);
		return 0;
	}
	if (inverse_ratio_p(right)) {
		GetDenomRatio(right, &right);
		div_rb_common(local, sign1, left, right, ret);
		return 0;
	}

	/* division */
	div_rr_common(local, sign1, left, right, ret);

	return 0;
}


/*
 *  division - integer
 */
int div_ff_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	fixed value1, value2;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	castfixed_fixnum(left, &sign1, &value1);
	castfixed_fixnum(right, &sign2, &value2);
	sign1 = SignMulti(sign1, sign2);
	if (value2 == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (value1 == 0) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (value1 == 1) {
		inverse_value_common(ret, sign1, value2);
		return 0;
	}

	/* right */
	if (value2 == 1) {
		if (IsPlus(sign2))
			fixnum_throw_heap(left, ret);
		else
			sigrev_fixnum_integer_common(left, ret);
		return 0;
	}

	push_local(local, &stack);
	bignum_value_alloc(local, &left, SignPlus, value1);
	bignum_value_alloc(local, &right, SignPlus, value2);
	ratio_reduction_heap(local, ret, sign1, left, right);
	rollback_local(local, stack);

	return 0;
}

int div_ff_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	fixed value1, value2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	castfixed_fixnum(left, &sign1, &value1);
	castfixed_fixnum(right, &sign2, &value2);
	sign1 = SignMulti(sign1, sign2);
	if (value2 == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (value1 == 0) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (value1 == 1) {
		inverse_value_local(local, ret, sign1, value2);
		return 0;
	}

	/* right */
	if (value2 == 1) {
		if (IsPlus(sign2))
			fixnum_throw_local(local, left, ret);
		else
			sigrev_fixnum_bignum_local(local, left, ret);
		return 0;
	}

	bignum_value_alloc(local, &left, SignPlus, value1);
	bignum_value_alloc(local, &right, SignPlus, value2);
	ratio_reduction_local(local, ret, sign1, left, right);

	return 0;
}

int div_fb_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	fixed value1;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	castfixed_fixnum(left, &sign1, &value1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (value1 == 0) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (value1 == 1) {
		bignum_sign_inverse_common(ret, sign1, right);
		return 0;
	}

	/* right */
	if (equal_value_nosign_bignum(right, 1)) {
		fixnum_throw_heap(left, ret);
		return 0;
	}

	push_local(local, &stack);
	bignum_value_alloc(local, &left, SignPlus, value1);
	bignum_copy_alloc(local, &right, right);
	SetSignBignum(right, SignPlus);
	ratio_reduction_heap(local, ret, sign1, left, right);
	rollback_local(local, stack);

	return 0;
}

int div_fb_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	fixed value1;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	castfixed_fixnum(left, &sign1, &value1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (value1 == 0) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (value1 == 1) {
		bignum_sign_inverse_local(local, ret, sign1, right);
		return 0;
	}

	/* right */
	if (equal_value_nosign_bignum(right, 1)) {
		fixnum_throw_local(local, left, ret);
		return 0;
	}

	bignum_value_alloc(local, &left, SignPlus, value1);
	bignum_copy_alloc(local, &right, right);
	SetSignBignum(right, SignPlus);
	ratio_reduction_local(local, ret, sign1, left, right);

	return 0;
}

int div_bf_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	fixed value2;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	GetSignBignum(left, &sign1);
	castfixed_fixnum(right, &sign2, &value2);
	sign1 = SignMulti(sign1, sign2);
	if (value2 == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (zerop_bignum(left)) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		inverse_value_common(ret, sign2, value2);
		return 0;
	}

	/* right */
	if (value2 == 1) {
		if (IsPlus(sign2))
			bignum_throw_heap(left, ret);
		else
			sigrev_bignum_integer_common(left, ret);
		return 0;
	}

	push_local(local, &stack);
	bignum_copy_alloc(local, &left, left);
	SetSignBignum(left, SignPlus);
	bignum_value_alloc(local, &right, SignPlus, value2);
	ratio_reduction_heap(local, ret, sign1, left, right);
	rollback_local(local, stack);

	return 0;
}

int div_bf_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	fixed value2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	GetSignBignum(left, &sign1);
	castfixed_fixnum(right, &sign2, &value2);
	sign1 = SignMulti(sign1, sign2);
	if (value2 == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (zerop_bignum(left)) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		inverse_value_local(local, ret, sign2, value2);
		return 0;
	}

	/* right */
	if (value2 == 1) {
		if (IsPlus(sign2))
			bignum_throw_local(local, left, ret);
		else
			sigrev_bignum_bignum_local(local, left, ret);
		return 0;
	}

	bignum_copy_alloc(local, &left, left);
	SetSignBignum(left, SignPlus);
	bignum_value_alloc(local, &right, SignPlus, value2);
	ratio_reduction_local(local, ret, sign1, left, right);

	return 0;
}

int div_bb_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (zerop_bignum(left)) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		bignum_sign_inverse_common(ret, sign1, left);
		return 0;
	}

	/* right */
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign1))
			return integer_copy_heap_(left, ret);
		else
			return integer_copysign_heap_(SignMinus, left, ret);
	}

	push_local(local, &stack);
	bignum_copy_nosign_alloc(local, &left, left);
	bignum_copy_nosign_alloc(local, &right, right);
	ratio_reduction_heap(local, ret, sign1, left, right);
	rollback_local(local, stack);

	return 0;
}

int div_bb_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (zerop_bignum(left)) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		bignum_sign_inverse_local(local, ret, sign1, left);
		return 0;
	}

	/* right */
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign1))
			return integer_copy_local_(local, left, ret);
		else
			return integer_copysign_local_(local, SignMinus, left, ret);
	}

	bignum_copy_nosign_alloc(local, &left, left);
	bignum_copy_nosign_alloc(local, &right, right);
	ratio_reduction_local(local, ret, sign1, left, right);

	return 0;
}


/*
 *  inverse
 */
int inverse_fixnum_ratio_local_(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	fixed value;

	CheckType(pos, LISPTYPE_FIXNUM);
	castfixed_fixnum(pos, &sign, &value);
	if (value == 0) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, pos);
	}
	ratio_noreduction_value_local(local, ret, sign, 1, value);

	return 0;
}

int inverse_bignum_ratio_local_(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	addr numer, denom;

	CheckType(pos, LISPTYPE_BIGNUM);
	if (zerop_bignum(pos)) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, pos);
	}
	GetSignBignum(pos, &sign);
	bignum_value_local(local, &numer, SignPlus, 1);
	bignum_copy_local(local, &denom, pos);
	make_ratio_local(local, ret, sign, numer, denom);

	return 0;
}

int inverse_ratio_local_(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	addr numer, denom;

	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, pos);
	}
	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	bignum_copy_local(local, &numer, numer);
	bignum_copy_local(local, &denom, denom);
	make_ratio_local(local, ret, sign, denom, numer);

	return 0;
}

int inverse_fixnum_common_(addr left, addr *ret)
{
	int sign;
	fixed value;

	castfixed_fixnum(left, &sign, &value);
	if (value == 0) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, left);
	}
	if (value == 1) {
		fixnum_throw_heap(left, ret);
		return 0;
	}
	inverse_value_common(ret, sign, value);

	return 0;
}

int inverse_bignum_common_(addr left, addr *ret)
{
	int sign;

	if (zerop_bignum(left)) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, left);
	}
	if (equal_rv_nosign(left, 1)) {
		bignum_throw_heap(left, ret);
		return 0;
	}
	GetSignBignum(left, &sign);
	bignum_sign_inverse_common(ret, sign, left);

	return 0;
}

int inverse_ratio_common_(LocalRoot local, addr left, addr *ret)
{
	int sign;

	if (zerop_ratio(left)) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, left);
	}
	GetSignRatio(left, &sign);
	ratio_sign_inverse_common(local, ret, sign, left);

	return 0;
}


/************************************************************
 *  ratio_plus.c
 ************************************************************/

void sign_reverse_ratio_inplace(addr pos)
{
	int sign;

	CheckType(pos, LISPTYPE_RATIO);
	GetSignRatio(pos, &sign);
	sign = SignNot(sign);
	SetSignRatio(pos, sign);
}

void sign_reverse_ratio_local(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	addr numer, denom;

	Check(local == NULL, "local error");
	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (! GetStatusDynamic(pos)) {
		bignum_copy_local(local, &numer, numer);
		bignum_copy_local(local, &denom, denom);
	}
	make_ratio_local(local, ret, SignNot(sign), numer, denom);
}

void sign_reverse_ratio_common(addr pos, addr *ret)
{
	int sign;
	addr numer, denom;

	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (GetStatusDynamic(pos)) {
		bignum_copy_heap(&numer, numer);
		bignum_copy_heap(&denom, denom);
	}
	make_ratio_heap(ret, SignNot(sign), numer, denom);
}

/* ratio - fixnum */
static void plus_rv_data_ratio(LocalRoot local,
		int sign, addr left, fixed right, addr *ret)
{
	addr numer, denom, pos;
	size_t size1, size2;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		/* numer + right */
		plusvalue_bigdata_alloc(local, numer, SignPlus, right, &numer);
	}
	else {
		/* denom * right */
		GetSizeBignum(numer, &size1);
		GetSizeBignum(denom, &size2);
		size2++;
		size1 = (size1 < size2)? size2: size1;
		alloc_bignum(local, &pos, size1 + 1UL);
		setmultivalue_bigdata(pos, denom, right);
		/* numer + (denom * right) */
		letplus_noexpand_bigdata(pos, numer);
		bignum_throw_local(local, pos, &numer);
	}
	bignum_throw_local(local, denom, &denom);
	make_ratio_local(local, ret, sign, numer, denom);
}

static void plus_rv_data_local(LocalRoot local,
		int sign, addr left, fixed right, addr *ret)
{
	addr numer, denom, pos;
	size_t size1, size2;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		plusvalue_bigdata_alloc(local, numer, sign, right, &pos);
		bignum_result_local(local, pos, ret);
		return;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	size2++;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1 + 1UL);
	setmultivalue_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	letplus_noexpand_bigdata(pos, numer);
	bignum_throw_local(local, pos, &numer);
	bignum_throw_local(local, denom, &denom);
	make_ratio_local(local, ret, sign, numer, denom);
}

static void plus_rv_data_common(LocalRoot local,
		int sign, addr left, fixed right, addr *ret)
{
	addr numer, denom, pos;
	size_t size1, size2;
	LocalStack stack;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	push_local(local, &stack);
	if (equal_value_nosign_bignum(denom, 1)) {
		plusvalue_bigdata_alloc(local, numer, sign, right, &pos);
		bignum_result_heap(pos, ret);
		goto finish;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	size2++;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1 + 1UL);
	setmultivalue_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	letplus_noexpand_bigdata(pos, numer);
	bignum_throw_heap(pos, &numer);
	bignum_throw_heap(denom, &denom);
	make_ratio_heap(ret, sign, numer, denom);

finish:
	rollback_local(local, stack);
}

static void minus_rv_data_ratio(LocalRoot local,
		int sign, addr left, fixed right, addr *ret)
{
	int check;
	addr numer, denom, pos;
	size_t size1, size2;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		minusvalue_bigdata_alloc(local, numer, sign, right, &numer);
		GetSignBignum(numer, &sign);
		SetSignBignum(numer, SignPlus);
		bignum_throw_local(local, denom, &denom);
		make_ratio_local(local, ret, sign, numer, denom);
		return;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	size2++;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1); /* no +1UL */
	setmultivalue_bigdata(pos, denom, right);

	/* numer - (denom * right) */
	check = letminus_noexpand_bigdata(pos, numer);
	bignum_throw_local(local, pos, &numer);
	bignum_throw_local(local, denom, &denom);
	make_ratio_local(local, ret, (check? sign: SignNot(sign)), numer, denom);
}

static void minus_rv_data_local(LocalRoot local,
		int sign, addr left, fixed right, addr *ret)
{
	int check;
	addr numer, denom, pos;
	size_t size1, size2;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		minusvalue_bigdata_alloc(local, numer, sign, right, &pos);
		bignum_result_local(local, pos, ret);
		return;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	size2++;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1); /* no +1UL */
	setmultivalue_bigdata(pos, denom, right);

	/* numer - (denom * right) */
	check = letminus_noexpand_bigdata(pos, numer);
	bignum_throw_local(local, pos, &numer);
	bignum_throw_local(local, denom, &denom);
	make_ratio_local(local, ret, (check? sign: SignNot(sign)), numer, denom);
}

static void minus_rv_data_common(LocalRoot local,
		int sign, addr left, fixed right, addr *ret)
{
	int check;
	addr numer, denom, pos;
	size_t size1, size2;
	LocalStack stack;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	push_local(local, &stack);
	if (equal_value_nosign_bignum(denom, 1)) {
		minusvalue_bigdata_alloc(local, numer, sign, right, &pos);
		bignum_result_heap(pos, ret);
		goto finish;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	size2++;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1); /* no +1UL */
	setmultivalue_bigdata(pos, denom, right);

	/* numer - (denom * right) */
	check = letminus_noexpand_bigdata(pos, numer);
	bignum_throw_heap(pos, &numer);
	bignum_throw_heap(denom, &denom);
	make_ratio_heap(ret, (check? sign: SignNot(sign)), numer, denom);

finish:
	rollback_local(local, stack);
}

static void plus_rv_ratio(LocalRoot local,
		addr left, int sign2, fixed right, addr *ret)
{
	int sign1, check1, check2;

	if (right == 0) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rv_data_ratio(local, sign1, left, right, ret);
	else
		minus_rv_data_ratio(local, sign1, left, right, ret);
}

static void plus_rv_local(LocalRoot local,
		addr left, int sign2, fixed right, addr *ret)
{
	int sign1, check1, check2;

	if (right == 0) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rv_data_local(local, sign1, left, right, ret);
	else
		minus_rv_data_local(local, sign1, left, right, ret);
}

static void plus_rv_common(LocalRoot local,
		addr left, int sign2, fixed right, addr *ret)
{
	int sign1, check1, check2;

	Check(local == NULL, "local error");
	if (right == 0) {
		ratio_throw_heap(left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rv_data_common(local, sign1, left, right, ret);
	else
		minus_rv_data_common(local, sign1, left, right, ret);
}

void plus_rv_ratio_local(LocalRoot local, addr left, fixnum right, addr *ret)
{
	int sign;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");

	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
	}
	else {
		castfixed(right, &sign, &value);
		plus_rv_ratio(local, left, sign, value, ret);
	}
}

void plus_rv_real_local(LocalRoot local, addr left, fixnum right, addr *ret)
{
	int sign;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");

	if (zerop_ratio(left)) {
		fixnum_local(local, ret, right);
	}
	else {
		castfixed(right, &sign, &value);
		plus_rv_local(local, left, sign, value, ret);
	}
}

void plus_rv_real_common(LocalRoot local, addr left, fixnum right, addr *ret)
{
	int sign;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");

	if (zerop_ratio(left)) {
		fixnum_heap(ret, right);
	}
	else {
		castfixed(right, &sign, &value);
		plus_rv_common(local, left, sign, value, ret);
	}
}

void plus_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
	}
	else {
		castfixed_fixnum(right, &sign, &value);
		plus_rv_ratio(local, left, sign, value, ret);
	}
}

void plus_rf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	if (zerop_ratio(left)) {
		fixnum_throw_local(local, right, ret);
	}
	else {
		castfixed_fixnum(right, &sign, &value);
		plus_rv_local(local, left, sign, value, ret);
	}
}

void plus_rf_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	if (zerop_ratio(left)) {
		fixnum_throw_heap(right, ret);
	}
	else {
		castfixed_fixnum(right, &sign, &value);
		plus_rv_common(local, left, sign, value, ret);
	}
}

static void minus_rv_ratio(LocalRoot local,
		addr left, int sign2, fixed right, addr *ret, int reverse)
{
	int sign1, check1, check2;

	GetSignRatio(left, &sign1);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if (reverse) sign1 = SignNot(sign1);

	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rv_data_ratio(local, sign1, left, right, ret);
	else
		plus_rv_data_ratio(local, sign1, left, right, ret);
}

static void minus_rv_local(LocalRoot local,
		addr left, int sign2, fixed right, addr *ret, int reverse)
{
	int sign1, check1, check2;

	GetSignRatio(left, &sign1);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if (reverse) sign1 = SignNot(sign1);

	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rv_data_local(local, sign1, left, right, ret);
	else
		plus_rv_data_local(local, sign1, left, right, ret);
}

static void minus_rv_common(LocalRoot local,
		addr left, int sign2, fixed right, addr *ret, int reverse)
{
	int sign1, check1, check2;

	Check(local == NULL, "local error");
	GetSignRatio(left, &sign1);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if (reverse) sign1 = SignNot(sign1);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rv_data_common(local, sign1, left, right, ret);
	else
		plus_rv_data_common(local, sign1, left, right, ret);
}

void sigrev_fixnum_ratio_local(LocalRoot local, addr pos, addr *ret)
{
	cast_fixnum_ratio_local(local, pos, ret);
	sign_reverse_ratio_inplace(*ret);
}

void sigrev_bignum_ratio_local(LocalRoot local, addr pos, addr *ret)
{
	cast_bignum_ratio_local(local, pos, ret);
	sign_reverse_ratio_inplace(*ret);
}

void minus_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	if (zerop_ratio(left)) {
		sigrev_fixnum_ratio_local(local, right, ret);
		return;
	}
	castfixed_fixnum(right, &sign, &value);
	if (value == 0) {
		ratio_throw_local(local, left, ret);
		return;
	}

	minus_rv_ratio(local, left, sign, value, ret, 0);
}

void minus_rf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	if (zerop_ratio(left)) {
		sigrev_fixnum_integer_local(local, right, ret);
		return;
	}
	castfixed_fixnum(right, &sign, &value);
	if (value == 0) {
		ratio_throw_local(local, left, ret);
		return;
	}

	minus_rv_local(local, left, sign, value, ret, 0);
}

void minus_rf_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	if (zerop_ratio(left)) {
		sigrev_fixnum_integer_common(right, ret);
		return;
	}
	castfixed_fixnum(right, &sign, &value);
	if (value == 0) {
		ratio_throw_heap(left, ret);
		return;
	}

	minus_rv_common(local, left, sign, value, ret, 0);
}

void minus_fr_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	castfixed_fixnum(left, &sign, &value);
	if (value == 0) {
		sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		cast_fixnum_ratio_local(local, left, ret);
		return;
	}

	/* right - left = -(left - right) */
	minus_rv_ratio(local, right, sign, value, ret, 1);
}

void minus_fr_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	castfixed_fixnum(left, &sign, &value);
	if (value == 0) {
		sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		fixnum_throw_local(local, left, ret);
		return;
	}

	/* right - left = -(left - right) */
	minus_rv_local(local, right, sign, value, ret, 1);
}

void minus_fr_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	castfixed_fixnum(left, &sign, &value);
	if (value == 0) {
		sign_reverse_ratio_common(right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		fixnum_throw_heap(left, ret);
		return;
	}

	/* right - left = -(left - right) */
	minus_rv_common(local, right, sign, value, ret, 1);
}


/* ratio - bignum */
static void plus_rb_ratio(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	addr pos, numer, denom;
	size_t size1, size2, size3;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		plus_bigdata_alloc(local, numer, right, &numer);
		bignum_throw_local(local, denom, &denom);
		make_ratio_local(local, ret, sign, numer, denom);
		return;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	GetSizeBignum(right, &size3);
	size2 += size3;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1 + 1UL);
	setmulti_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	letplus_noexpand_bigdata(pos, numer);
	bignum_throw_local(local, pos, &numer);
	bignum_throw_local(local, denom, &denom);
	make_ratio_local(local, ret, sign, numer, denom);
}

static void plus_rb_local(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	addr pos, numer, denom;
	size_t size1, size2, size3;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		plus_bigdata_alloc(local, numer, right, &pos);
		SetSignBignum(pos, sign);
		bignum_result_local(local, pos, ret);
		return;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	GetSizeBignum(right, &size3);
	size2 += size3;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1 + 1UL);
	setmulti_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	letplus_noexpand_bigdata(pos, numer);
	bignum_throw_local(local, pos, &numer);
	bignum_throw_local(local, denom, &denom);
	make_ratio_local(local, ret, sign, numer, denom);
}

static void plus_rb_common(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	addr pos, numer, denom;
	size_t size1, size2, size3;
	LocalStack stack;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	push_local(local, &stack);
	if (equal_value_nosign_bignum(denom, 1)) {
		plus_bigdata_alloc(local, numer, right, &pos);
		SetSignBignum(pos, sign);
		bignum_result_heap(pos, ret);
		goto finish;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	GetSizeBignum(right, &size3);
	size2 += size3;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1 + 1UL);
	setmulti_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	letplus_noexpand_bigdata(pos, numer);
	bignum_throw_heap(pos, &numer);
	bignum_throw_heap(denom, &denom);
	make_ratio_heap(ret, sign, numer, denom);

finish:
	rollback_local(local, stack);
}

static void minus_rb_ratio(LocalRoot local,
		int sign, addr left, addr right, addr *ret, int reverse)
{
	int check;
	addr pos, numer, denom;
	size_t size1, size2, size3;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		check = minuscheck_bigdata_alloc(local, numer, right, &numer);
		sign = check? SignNot(sign): sign;
		sign = reverse? SignNot(sign): sign;
		bignum_throw_local(local, denom, &denom);
		make_ratio_local(local, ret, sign, numer, denom);
		return;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	GetSizeBignum(right, &size3);
	size2 += size3;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1);
	setmulti_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	check = letminus_noexpand_bigdata(pos, numer);
	bignum_throw_local(local, pos, &numer);
	bignum_throw_local(local, denom, &denom);
	sign = check? sign: SignNot(sign);
	sign = reverse? SignNot(sign): sign;
	make_ratio_local(local, ret, sign, numer, denom);
}

static void minus_rb_local(LocalRoot local,
		int sign, addr left, addr right, addr *ret, int reverse)
{
	int check;
	addr pos, numer, denom;
	size_t size1, size2, size3;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		check = minuscheck_bigdata_alloc(local, numer, right, &pos);
		sign = check? SignNot(sign): sign;
		sign = reverse? SignNot(sign): sign;
		SetSignBignum(pos, sign);
		bignum_result_local(local, pos, ret);
		return;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	GetSizeBignum(right, &size3);
	size2 += size3;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1);
	setmulti_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	check = letminus_noexpand_bigdata(pos, numer);
	bignum_throw_local(local, pos, &numer);
	bignum_throw_local(local, denom, &denom);
	sign = check? sign: SignNot(sign);
	sign = reverse? SignNot(sign): sign;
	make_ratio_local(local, ret, sign, numer, denom);
}

static void minus_rb_common(LocalRoot local,
		int sign, addr left, addr right, addr *ret, int reverse)
{
	int check;
	addr pos, numer, denom;
	size_t size1, size2, size3;
	LocalStack stack;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	push_local(local, &stack);
	if (equal_value_nosign_bignum(denom, 1)) {
		check = minuscheck_bigdata_alloc(local, numer, right, &pos);
		sign = check? SignNot(sign): sign;
		sign = reverse? SignNot(sign): sign;
		SetSignBignum(pos, sign);
		bignum_result_heap(pos, ret);
		goto finish;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	GetSizeBignum(right, &size3);
	size2 += size3;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1);
	setmulti_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	check = letminus_noexpand_bigdata(pos, numer);
	bignum_throw_heap(pos, &numer);
	bignum_throw_heap(denom, &denom);
	sign = check? sign: SignNot(sign);
	sign = reverse? SignNot(sign): sign;
	make_ratio_heap(ret, sign, numer, denom);

finish:
	rollback_local(local, stack);
}

void plus_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_ratio(left)) {
		cast_bignum_ratio_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rb_ratio(local, sign1, left, right, ret);
	else
		minus_rb_ratio(local, sign1, left, right, ret, 0);
}

void plus_rb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_ratio(left)) {
		bignum_throw_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rb_local(local, sign1, left, right, ret);
	else
		minus_rb_local(local, sign1, left, right, ret, 0);
}

void plus_rb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_ratio(left)) {
		bignum_throw_heap(right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		ratio_throw_heap(left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rb_common(local, sign1, left, right, ret);
	else
		minus_rb_common(local, sign1, left, right, ret, 0);
}

void minus_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_ratio(left)) {
		sigrev_bignum_ratio_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rb_ratio(local, sign1, left, right, ret, 0);
	else
		plus_rb_ratio(local, sign1, left, right, ret);
}

void minus_rb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_ratio(left)) {
		sigrev_bignum_integer_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rb_local(local, sign1, left, right, ret, 0);
	else
		plus_rb_local(local, sign1, left, right, ret);
}

void minus_rb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_ratio(left)) {
		sigrev_bignum_integer_common(right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		ratio_throw_heap(left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rb_common(local, sign1, left, right, ret, 0);
	else
		plus_rb_common(local, sign1, left, right, ret);
}

void minus_br_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_bignum(left)) {
		sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		cast_bignum_ratio_local(local, left, ret);
		return;
	}

	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rb_ratio(local, sign1, right, left, ret, 1);
	else
		plus_rb_ratio(local, sign1, right, left, ret);
}

void minus_br_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_bignum(left)) {
		sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		bignum_throw_local(local, left, ret);
		return;
	}

	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rb_local(local, sign1, right, left, ret, 1);
	else
		plus_rb_local(local, sign1, right, left, ret);
}

void minus_br_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_bignum(left)) {
		sign_reverse_ratio_common(right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		bignum_throw_heap(left, ret);
		return;
	}

	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rb_common(local, sign1, right, left, ret, 1);
	else
		plus_rb_common(local, sign1, right, left, ret);
}


/* ratio - ratio */
static void plus_rr_data_result(LocalRoot local,
		addr left, addr right, addr *rnumer, addr *rdenom)
{
	addr numer1, numer2, denom1, denom2, reduct1, reduct2;

	GetNumerRatio(left, &numer1);
	GetDenomRatio(left, &denom1);
	GetNumerRatio(right, &numer2);
	GetDenomRatio(right, &denom2);
	if (equal_bigdata(denom1, denom2)) {
		/*
		 *  numer1 + numer2
		 *  ---------------
		 *      denom1
		 */
		plus_bigdata_alloc(local, numer1, numer2, &numer1);
		bignum_copy_local(local, &denom1, denom1);
	}
	else {
		/*
		 *  numer1   numer2
		 *  ------ + ------
		 *  denom1   denom2
		 */

		/*
		 *  (denom1 denom2) reduction-> (reduct1 reduct2)
		 */
		bignum_copy_local(local, &reduct1, denom1);
		bignum_copy_local(local, &reduct2, denom2);
		reduction_local(local, reduct1, reduct2);

		/*
		 *  numer1*reduct2    numer2*reduct1    numer1*reduct2 + numer2*reduct1
		 *  -------------- + ---------------- = -------------------------------
		 *  denom1*reduct2   (denom2*reduct1)           denom1 * reduct2
		 */
		multi_bigdata_alloc(local, denom1, reduct2, &denom1);
		multi_bigdata_alloc(local, numer1, reduct2, &numer1);
		multi_bigdata_alloc(local, numer2, reduct1, &numer2);
		plus_bigdata_alloc(local, numer1, numer2, &numer1);
	}
	*rnumer = numer1;
	*rdenom = denom1;
}

static void plus_rr_data_ratio(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	plus_rr_data_result(local, left, right, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void plus_rr_data_local(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	plus_rr_data_result(local, left, right, &left, &right);
	ratio_reduction_local(local, ret, sign, left, right);
}

static void plus_rr_data_common(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	plus_rr_data_result(local, left, right, &left, &right);
	ratio_reduction_heap(local, ret, sign, left, right);
	rollback_local(local, stack);
}

static void minus_rr_data_result(LocalRoot local,
		int sign, addr left, addr right,
		int *rsign, addr *rnumer, addr *rdenom)
{
	int check;
	addr numer1, numer2, denom1, denom2, reduct1, reduct2;

	GetNumerRatio(left, &numer1);
	GetDenomRatio(left, &denom1);
	GetNumerRatio(right, &numer2);
	GetDenomRatio(right, &denom2);
	if (equal_bigdata(denom1, denom2)) {
		/*
		 *  numer1 - numer2
		 *  --------------
		 *      denom1
		 */
		check = minuscheck_bigdata_alloc(local, numer1, numer2, &numer1);
		if (check) sign = SignNot(sign);
		bignum_copy_local(local, &denom1, denom1);
	}
	else {
		/*
		 *  numer1   numer2
		 *  ------ - ------
		 *  denom1   denom2
		 */

		/*
		 *  (denom1 denom2) reduction-> (reduct1 reduct2)
		 */
		bignum_copy_local(local, &reduct1, denom1);
		bignum_copy_local(local, &reduct2, denom2);
		reduction_local(local, reduct1, reduct2);

		/*
		 *  numer1*reduct2    numer2*reduct1    numer1*reduct2 - numer2*reduct1
		 *  -------------- - ---------------- = -------------------------------
		 *  denom1*reduct2   (denom2*reduct1)           denom1 * reduct2
		 */
		multi_bigdata_alloc(local, denom1, reduct2, &denom1);
		multi_bigdata_alloc(local, numer1, reduct2, &numer1);
		multi_bigdata_alloc(local, numer2, reduct1, &numer2);
		check = minuscheck_bigdata_alloc(local, numer1, numer2, &numer1);
		if (check) sign = SignNot(sign);
	}
	*rsign = sign;
	*rnumer = numer1;
	*rdenom = denom1;
}

static void minus_rr_data_ratio(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	minus_rr_data_result(local, sign, left, right, &sign, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void minus_rr_data_local(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	minus_rr_data_result(local, sign, left, right, &sign, &left, &right);
	ratio_reduction_local(local, ret, sign, left, right);
}

static void minus_rr_data_common(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	minus_rr_data_result(local, sign, left, right, &sign, &left, &right);
	ratio_reduction_heap(local, ret, sign, left, right);
	rollback_local(local, stack);
}

void plus_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_ratio(left)) {
		ratio_throw_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rr_data_ratio(local, sign1, left, right, ret);
	else
		minus_rr_data_ratio(local, sign1, left, right, ret);
}

void plus_rr_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_ratio(left)) {
		ratio_throw_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rr_data_local(local, sign1, left, right, ret);
	else
		minus_rr_data_local(local, sign1, left, right, ret);
}

void plus_rr_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_ratio(left)) {
		ratio_throw_heap(right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		ratio_throw_heap(left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rr_data_common(local, sign1, left, right, ret);
	else
		minus_rr_data_common(local, sign1, left, right, ret);
}

void minus_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_ratio(left)) {
		sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rr_data_ratio(local, sign1, left, right, ret);
	else
		plus_rr_data_ratio(local, sign1, left, right, ret);
}

void minus_rr_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_ratio(left)) {
		sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rr_data_local(local, sign1, left, right, ret);
	else
		plus_rr_data_local(local, sign1, left, right, ret);
}

void minus_rr_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_ratio(left)) {
		sign_reverse_ratio_common(right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		ratio_throw_heap(left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rr_data_common(local, sign1, left, right, ret);
	else
		plus_rr_data_common(local, sign1, left, right, ret);
}


/************************************************************
 *  rational.c
 ************************************************************/

int rationalp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_FIXNUM
		|| type == LISPTYPE_BIGNUM
		|| type == LISPTYPE_RATIO;
}

/*
 *  throw
 */
int rational_result_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (rationalp(pos)) {
		ratio_result_noreduction_local(local, pos, ret);
		return 0;
	}
	else {
		return integer_result_local_(local, pos, ret);
	}
}

int rational_result_heap_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (rationalp(pos)) {
		ratio_result_noreduction_heap(local, pos, ret);
		return 0;
	}
	else {
		return integer_result_heap_(pos, ret);
	}
}

int rational_throw_alloc_(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_RATIO:
			ratio_throw_alloc(local, pos, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, RATIONAL);
	}

	return 0;
}

int rational_throw_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return rational_throw_alloc_(local, pos, ret);
}

int rational_throw_heap_(addr pos, addr *ret)
{
	return rational_throw_alloc_(NULL, pos, ret);
}

int rational_copy_alloc_(LocalRoot local, addr pos, addr *ret)
{
	if (ratiop(pos)) {
		ratio_copy_alloc(local, ret, pos);
		return 0;
	}
	else {
		return integer_copy_alloc_(local, pos, ret);
	}
}

int rational_copy_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return rational_copy_alloc_(local, pos, ret);
}

int rational_copy_heap_(addr pos, addr *ret)
{
	return rational_copy_alloc_(NULL, pos, ret);
}


/*
 *  float
 */
int single_float_rational_(addr pos, single_float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, single_float_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return single_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return single_float_ratio_(pos, ret);

		default:
			*ret = 0.0f;
			return TypeError_(pos, RATIONAL);
	}
}

int double_float_rational_(addr pos, double_float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, double_float_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return double_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return double_float_ratio_(pos, ret);

		default:
			*ret = 0.0;
			return TypeError_(pos, RATIONAL);
	}
}

int long_float_rational_(addr pos, long_float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, long_float_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return long_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return long_float_ratio_(pos, ret);

		default:
			*ret = 0.0L;
			return TypeError_(pos, RATIONAL);
	}
}


/*
 *  numerator
 */
int numerator_common_(addr pos, addr *ret)
{
	int sign;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_heap(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_throw_heap(pos, ret);
			return 0;

		case LISPTYPE_RATIO:
			GetSignRatio(pos, &sign);
			GetNumerRatio(pos, &pos);
			bignum_copy_heap(&pos, pos);
			SetSignBignum(pos, sign);
			return Result(ret, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, RATIONAL);
	}
}


/*
 *  denominator
 */
int denominator_common_(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			fixnum_heap(ret, 1);
			return 0;

		case LISPTYPE_RATIO:
			GetDenomRatio(pos, &pos);
			bignum_copy_heap(ret, pos);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, RATIONAL);
	}
}


/************************************************************
 *  rational_equal.c
 ************************************************************/

/*
 *  rational
 */
int plusp_rational_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, plusp_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, plusp_bignum(pos));

		case LISPTYPE_RATIO:
			return Result(ret, plusp_ratio(pos));

		default:
			*ret = 0;
			return TypeError_(pos, RATIONAL);
	}
}

int minusp_rational_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, minusp_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, minusp_bignum(pos));

		case LISPTYPE_RATIO:
			return Result(ret, minusp_ratio(pos));

		default:
			*ret = 0;
			return TypeError_(pos, RATIONAL);
	}
}

int zerop_rational_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, zerop_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, zerop_bignum(pos));

		case LISPTYPE_RATIO:
			return Result(ret, zerop_ratio(pos));

		default:
			*ret = 0;
			return TypeError_(pos, RATIONAL);
	}
}

static inline int equal_fixnum_rational_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, equal_ff_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, equal_fb_real(left, right));

		case LISPTYPE_RATIO:
			return Result(ret, equal_fr_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, RATIONAL);
	}
}

static inline int equal_bignum_rational_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, equal_bf_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, equal_bb_real(left, right));

		case LISPTYPE_RATIO:
			return Result(ret, equal_br_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, RATIONAL);
	}
}

static inline int equal_ratio_rational_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, equal_rf_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, equal_rb_real(left, right));

		case LISPTYPE_RATIO:
			return Result(ret, equal_rr_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, RATIONAL);
	}
}

int equal_rational_(addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return equal_fixnum_rational_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return equal_bignum_rational_(left, right, ret);

		case LISPTYPE_RATIO:
			return equal_ratio_rational_(left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, RATIONAL);
	}
}

int not_equal_rational_(addr left, addr right, int *ret)
{
	int check;
	Return(equal_rational_(left, right, &check));
	return Result(ret, ! check);
}

static inline int compare_fixnum_rational_(LocalRoot local,
		addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, compare_ff_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, compare_fb_real(left, right));

		case LISPTYPE_RATIO:
			return Result(ret, compare_fr_real(local, left, right));

		default:
			*ret = 0;
			return TypeError_(right, RATIONAL);
	}
}

static inline int compare_bignum_rational_(LocalRoot local,
		addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, compare_bf_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, compare_bb_real(left, right));

		case LISPTYPE_RATIO:
			return Result(ret, compare_br_real(local, left, right));

		default:
			*ret = 0;
			return TypeError_(right, RATIONAL);
	}
}

static inline int compare_ratio_rational_(LocalRoot local,
		addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, compare_rf_real(local, left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, compare_rb_real(local, left, right));

		case LISPTYPE_RATIO:
			return Result(ret, compare_rr_real(local, left, right));

		default:
			*ret = 0;
			return TypeError_(right, RATIONAL);
	}
}

int compare_rational_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return compare_fixnum_rational_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return compare_bignum_rational_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return compare_ratio_rational_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, RATIONAL);
	}
}

int less_rational_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_rational_(local, left, right, &check));
	return Result(ret, check < 0);
}

int less_equal_rational_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_rational_(local, left, right, &check));
	return Result(ret, check <= 0);
}

int greater_rational_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_rational_(local, left, right, &check));
	return Result(ret, check > 0);
}

int greater_equal_rational_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_rational_(local, left, right, &check));
	return Result(ret, check >= 0);
}

int less_rational_debug(LocalRoot local, addr left, addr right)
{
	int check;
	check = 0;
	Error(less_rational_(local, left, right, &check));
	return check;
}

int less_equal_rational_debug(LocalRoot local, addr left, addr right)
{
	int check;
	check = 0;
	Error(less_equal_rational_(local, left, right, &check));
	return check;
}


/************************************************************
 *  rational_multi.c
 ************************************************************/

/*
 *  multi
 */
int multi_fixnum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_fb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_fr_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int multi_bignum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_bb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_br_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int multi_ratio_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_rf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_rb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_rr_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int multi_single_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_sr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int multi_double_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_dr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int multi_long_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_lr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int multi_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return multi_fixnum_rational_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_bignum_rational_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_ratio_rational_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

int multi_fixnum_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_ff_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_fb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_fr_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int multi_bignum_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_bf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_bb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_br_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int multi_ratio_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_rf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_rb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_rr_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int multi_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return multi_fixnum_rational_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_bignum_rational_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_ratio_rational_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}


/*
 *  div
 */
int div_fixnum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_ff_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_fb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_fr_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int div_rational_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_FIXNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_ff_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bf_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rf_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

int div_bignum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_bf_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_br_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int div_rational_bignum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_BIGNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fb_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rb_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

int div_ratio_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_rf_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_rb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rr_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int div_rational_ratio_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_RATIO);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fr_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_br_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rr_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

int div_single_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_sr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int div_rational_single_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fs_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bs_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rs_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

int div_double_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_dr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int div_rational_double_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fd_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bd_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rd_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

int div_long_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_lr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int div_rational_long_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_LONG_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fl_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bl_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

static int div_fixnum_rational_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_ff_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_fb_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_fr_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int div_bignum_rational_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_bf_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bb_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_br_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int div_ratio_rational_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_rf_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_rb_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rr_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int div_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fixnum_rational_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bignum_rational_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_ratio_rational_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}


/*
 *  inverse
 */
int inverse_rational_common_(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return inverse_fixnum_common_(pos, ret);

		case LISPTYPE_BIGNUM:
			return inverse_bignum_common_(pos, ret);

		case LISPTYPE_RATIO:
			return inverse_ratio_common_(local, pos, ret);

		default:
			*ret = Nil;
			return TypeError_(pos, RATIONAL);
	}
}


/************************************************************
 *  rational_plus.c
 ************************************************************/

/*
 *  sign-reverse
 */
int sign_reverse_rational_common_(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			sigrev_fixnum_integer_common(pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			sigrev_bignum_integer_common(pos, ret);
			break;

		case LISPTYPE_RATIO:
			sign_reverse_ratio_common(pos, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, RATIONAL);
	}

	return 0;
}

int sign_reverse_rational_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			sigrev_fixnum_integer_local(local, pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			sigrev_bignum_integer_local(local, pos, ret);
			break;

		case LISPTYPE_RATIO:
			sign_reverse_ratio_local(local, pos, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, RATIONAL);
	}

	return 0;
}


/*
 *  oneplus
 */
int oneplus_rational_common_(LocalRoot local, addr value, addr *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, 1, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, 1, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_rv_real_common(local, value, 1, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(value, RATIONAL);
	}
}

int oneminus_rational_common_(LocalRoot local, addr value, addr *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, -1, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, -1, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_rv_real_common(local, value, -1, ret);
			return 0;

		default:
			*ret = 0;
			return TypeError_(value, RATIONAL);
	}
}


/*
 *  plus
 */
int plus_fixnum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_fb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_fr_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int plus_bignum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_br_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int plus_ratio_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_rf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_rb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_rr_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int plus_single_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_sr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int plus_double_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_dr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int plus_long_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_lr_heap_(left, right, ret);

		default:
			return TypeError_(right, RATIONAL);
	}
}

int plus_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fixnum_rational_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bignum_rational_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_ratio_rational_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

static int plus_fixnum_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_fb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_fr_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int plus_bignum_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_br_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int plus_ratio_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_rf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_rb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_rr_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int plus_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fixnum_rational_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bignum_rational_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_ratio_rational_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

/*
 *  minus
 */
int minus_fixnum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_fb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_fr_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = 0;
			return TypeError_(right, RATIONAL);
	}
}

int minus_rational_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_FIXNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_rf_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

int minus_bignum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_bb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_br_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int minus_rational_bignum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_BIGNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_fb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_bb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_rb_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

int minus_ratio_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_rf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_rb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_rr_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int minus_rational_ratio_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_RATIO);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_fr_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_br_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_rr_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

int minus_single_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_sr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int minus_rational_single_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_float_fs_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_bs_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_rs_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

int minus_double_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_dr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int minus_rational_double_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_float_fd_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_bd_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_rd_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

int minus_long_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_lr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int minus_rational_long_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_LONG_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_float_fl_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_bl_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_rl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

static int minus_fixnum_rational_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_fb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_fr_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int minus_bignum_rational_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_bf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_bb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_br_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int minus_ratio_rational_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_rf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_rb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_rr_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int minus_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_fixnum_rational_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_bignum_rational_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_ratio_rational_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

int minus_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_fixnum_rational_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_bignum_rational_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_ratio_rational_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}


/************************************************************
 *  reader.c
 ************************************************************/

/*
 *  pushchar_readtable
 */
static int charmode_readtable_(addr pos, unicode c, unicode *ret)
{
	switch (*PtrCaseReadtable(pos)) {
		case ReadTable_upcase:
			c = toUpperUnicode(c);
			break;

		case ReadTable_downcase:
			c =  toLowerUnicode(c);
			break;

		case ReadTable_preserve:
			break;

		case ReadTable_invert:
			if ('a' <= c && c <= 'z') {
				c = c - 'a' + 'A';
				break;
			}
			if ('A' <= c && c <= 'Z') {
				c = c - 'A' + 'a';
				break;
			}
			break;

		default:
			*ret = 0;
			return fmte_("Unknown readtable-case type.", NULL);
	}

	return Result(ret, c);
}

static int tokenmode_readtable_(Execute ptr, int *ret)
{
	unsigned base;
	size_t size;
	addr queue;

	/* escape */
	if (getescape_readinfo(ptr))
		return Result(ret, TokenType_symbol);

	/* empty (for keyword) */
	getqueue_readinfo(ptr, &queue);
	getsize_charqueue(queue, &size);
	if (size == 0)
		return Result(ret, TokenType_empty);

	Return(getreadbase_(ptr, &base));
	return Result(ret, tokentype(base, queue));
}

static int setpackage_readtable_(Execute ptr)
{
	int type;
	addr queue, package;

	Return(tokenmode_readtable_(ptr, &type));
	switch (type) {
		case TokenType_empty: /* for keyword */
			setpackage_readinfo(ptr, T);
			break;

		case TokenType_symbol:
		case TokenType_potential:
			getqueue_readinfo(ptr, &queue);
			make_charqueue_heap(queue, &package);
			setpackage_readinfo(ptr, package);
			clear_charqueue(queue);
			break;

		default:
			return fmte_("Package token type error.", NULL);
	}

	return 0;
}

static int pushchar_readtable_(Execute ptr, addr pos, unicode c, int escape)
{
	unsigned bitescape;
	enum ReadInfo_State bitstate;
	struct readinfo_struct *str;
	addr queue;

	getqueue_readinfo(ptr, &queue);
	str = getreadinfo_struct(ptr);
	bitescape = str->escape;
	bitstate = str->state;
	/* mode0 : readsymbol
	 * modd1 : read colon
	 * mode2 : read colon second
	 * mode3 : readsymol next
	 *
	 * mode0 - aaa
	 * mode1 - :
	 * mode1 - aaa:
	 * mode2 - ::
	 * mode2 - aaa::
	 * mode2 - :aaa
	 * mode2 - ::aaa
	 * mode2 - aaa:bbb
	 * mode2 - aaa::bbb
	 */
	if (escape) {
		if (bitescape == 0)
			setescape_readinfo(ptr, 1);
		if (bitstate == ReadInfo_State_Colon1)
			setstate_readinfo(ptr, ReadInfo_State_Colon2);
		return push_charqueue_local_(ptr->local, queue, c);
	}

	switch (bitstate) {
		case ReadInfo_State_First:
			str->unexport = 0;
			if (c == ':') {
				setstate_readinfo(ptr, ReadInfo_State_Colon1);
				return setpackage_readtable_(ptr);
			}
			break;

		case ReadInfo_State_Colon1:
			setstate_readinfo(ptr, ReadInfo_State_Colon2);
			if (c != ':') break;
			str->unexport = 1;
			return 0;

		case ReadInfo_State_Colon2:
		case ReadInfo_State_Gensym:
			if (c == ':')
				return fmte_("colon error", NULL);
			break;

		default:
			return fmte_("mode error", NULL);
	}

	/* push char */
	if (! escape) {
		Return(charmode_readtable_(pos, c, &c));
	}
	return push_charqueue_local_(ptr->local, queue, c);
}


/*
 *  readtable
 */
int readtable_typetable_(addr pos, unicode c, enum ReadTable_Type *ret)
{
	Return(readtype_readtable_(pos, c, &pos));
	if (pos == Nil)
		return Result(ret, ReadTable_Type_illegal);
	else
		return Result(ret, ReadTypeStruct(pos)->type);
}

int readtable_result_(Execute ptr,
		addr *token, addr stream, addr table, enum ReadTable_Result *ret)
{
	enum ReadTable_Type type;
	int check;
	addr pos;
	unicode x, y, z;

step1:
	Return(read_char_stream_(stream, &x, &check));
	if (check)
		goto eof;

	/* step2 */
	Return(readtable_typetable_(table, x, &type));
	switch (type) {
		case ReadTable_Type_illegal:
			goto illegal_error;

		case ReadTable_Type_whitespace:
			/* step3 */
			goto step1;

		case ReadTable_Type_macro_term:
		case ReadTable_Type_macro_nonterm:
			Return(unread_char_stream_(stream, x));
			goto macro; /* return one value */

		case ReadTable_Type_escape_single:
			/* step5 */
			Return(read_char_stream_(stream, &y, &check));
			if (check)
				goto error;
			Return(pushchar_readtable_(ptr, table, y, 1));
			goto step8;

		case ReadTable_Type_escape_multiple:
			/* step6 */
			goto step9;

		case ReadTable_Type_constituent:
			/* step7 */
			Return(pushchar_readtable_(ptr, table, x, 0));
			break;

		default:
			goto error;
	}

step8:
	Return(read_char_stream_(stream, &y, &check));
	if (check)
		goto step10;
	Return(readtable_typetable_(table, y, &type));
	switch (type) {
		case ReadTable_Type_constituent:
		case ReadTable_Type_macro_nonterm:
			Return(pushchar_readtable_(ptr, table, y, 0));
			goto step8;

		case ReadTable_Type_escape_single:
			Return(read_char_stream_(stream, &z, &check));
			if (check)
				goto error;
			Return(pushchar_readtable_(ptr, table, z, 1));
			goto step8;

		case ReadTable_Type_escape_multiple:
			goto step9;

		case ReadTable_Type_illegal:
			x = y;
			goto illegal_error;

		case ReadTable_Type_macro_term:
			Return(unread_char_stream_(stream, y));
			goto step10;

		case ReadTable_Type_whitespace:
			if (getpreserving_readinfo(ptr)) {
				Return(unread_char_stream_(stream, y));
			}
			goto step10;

		default:
			goto error;
	}

step9:
	Return(read_char_stream_(stream, &y, &check));
	if (check)
		goto error;
	Return(readtable_typetable_(table, y, &type));
	switch (type) {
		case ReadTable_Type_macro_term:
		case ReadTable_Type_macro_nonterm:
		case ReadTable_Type_constituent:
		case ReadTable_Type_whitespace:
			Return(pushchar_readtable_(ptr, table, y, 1));
			goto step9;

		case ReadTable_Type_escape_single:
			Return(read_char_stream_(stream, &z, &check));
			if (check)
				goto error;
			Return(pushchar_readtable_(ptr, table, z, 1));
			goto step9;

		case ReadTable_Type_escape_multiple:
			goto step8;

		case ReadTable_Type_illegal:
			x = y;
			goto illegal_error;

		default:
			goto error;
	}

step10:
	Return(maketoken_(ptr, token));
	goto final;

illegal_error:
	character_heap(&pos, x);
	return fmte_("Invalid character, ~S.", pos, NULL);

error:
	return fmte_("readtable error", NULL);

final:
	return Result(ret, ReadTable_Result_normal);
macro:
	return Result(ret, ReadTable_Result_macro);
eof:
	return Result(ret, ReadTable_Result_eof);
}

int readtable_novalue_(Execute ptr, int *ret, addr *token, addr stream, addr table)
{
	enum ReadTable_Result value;
	int check;
	addr pos;
	unicode u;

	/* read */
	clear_readinfo(ptr);
	Return(readtable_result_(ptr, &pos, stream, table, &value));
	switch (value) {
		case ReadTable_Result_normal:
			*token = pos;
			return Result(ret, 0);

		case ReadTable_Result_eof:
			return Result(ret, 1);

		default:
			break;
	}

	/* macro execute */
	Return(read_char_stream_(stream, &u, &check));
	if (check)
		return fmte_("eof error", NULL);

	Return(macro_character_execute_(ptr, &check, &pos, u, stream, table));
	if (check) {
		*token = pos;
		return Result(ret, 0);
	}
	else {
		/* return no value */
		return Result(ret, -1);
	}
}

static int readtable_front(Execute ptr,
		int *result, addr *ret, addr stream, addr table)
{
	int check;

	for (;;) {
		Return(readtable_novalue_(ptr, &check, ret, stream, table));
		if (0 <= check) {
			break;
		}
	}

	/* eof */
	if (check) {
		*result = 1;
		return 0;
	}

	/* suppress */
	Return(read_suppress_p_(ptr, &check));
	if (check)
		*ret = Nil;

	/* normal */
	return Result(result, 0);
}


/*
 *  read
 */
int read_call_(Execute ptr, addr stream, int *result, addr *ret)
{
	addr table;
	Return(getreadtable_(ptr, &table));
	return readtable_front(ptr, result, ret, stream, table);
}

static int read_stream_call_(Execute ptr, LocalHold hold,
		addr stream, int *result, addr *ret)
{
	addr info;

	pushreadinfo(ptr, &info);
	Return(read_call_(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

int read_stream_(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)read_stream_call_(ptr, hold, stream, result, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

static int read_preserving_call_(Execute ptr, LocalHold hold,
		addr stream, int *result, addr *ret)
{
	addr info;

	pushreadinfo(ptr, &info);
	ReadInfoStruct(info)->preserving = 1;
	Return(read_call_(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

int read_preserving_(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)read_preserving_call_(ptr, hold, stream, result, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

static int read_recursive_call_(Execute ptr, LocalHold hold,
		addr stream, int *result, addr *ret)
{
	addr info;

	Return(pushreadinfo_recursive_(ptr, &info));
	Return(read_call_(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

int read_recursive_(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)read_recursive_call_(ptr, hold, stream, result, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

int read_from_string_(Execute ptr, int *result, addr *ret, addr pos)
{
	addr stream;
	LocalHold hold;

	Return(open_input_string_stream_(&stream, pos));
	hold = LocalHold_local_push(ptr, stream);
	Return(read_stream_(ptr, stream, result, ret));
	localhold_end(hold);
	close_input_string_stream(stream);

	return 0;
}

int readstring_debug(addr *ret, const char *code)
{
	int result;
	addr stream;

	open_input_char_stream(&stream, code);
	if (read_stream_(Execute_Thread, stream, &result, ret))
		Abort("Cannot catch a system signal.");
	close_input_string_stream(stream);

	return result;
}

addr readr_debug(const char *code)
{
	addr pos;
	if (readstring_debug(&pos, code))
		return Nil;
	else
		return pos;
}


/*****************************************************************************
 *  initialize
 *****************************************************************************/
static int build_reader_special_(void)
{
	addr pos, symbol;

	Return(readtable_heap_(&pos));
	GetConst(SPECIAL_READTABLE, &symbol);
	SetValueSymbol(symbol, pos);

	return 0;
}

void build_reader(void)
{
	build_reader_dispatch();
	Error(build_reader_special_());
}

void init_reader(void)
{
	init_reader_dispatch();
	init_reader_token();
}


/************************************************************
 *  reader_dispatch.c
 ************************************************************/

/*****************************************************************************
 *  macro character
 *****************************************************************************/
/* (defun double-quote-reader (stream character) ...) -> * */
static int function_reader_double_quote(Execute ptr, addr pos, addr code)
{
	Return(double_quote_reader_(ptr->local, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_double_quote_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DOUBLE_QUOTE_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_double_quote);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun single-quote-reader (stream character) ...) -> * */
static int function_reader_single_quote(Execute ptr, addr pos, addr code)
{
	Return(single_quote_reader_(ptr, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_single_quote_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SINGLE_QUOTE_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_single_quote);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parensis-open-reader (stream character) ...) -> * */
static int function_reader_parensis_open(Execute ptr, addr stream, addr code)
{
	return parensis_open_reader_(ptr, stream);
}

static void defun_parensis_open_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PARENSIS_OPEN_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_parensis_open);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parensis-close-reader (stream character) ...) -> * */
static int function_reader_parensis_close(Execute ptr, addr stream, addr code)
{
	return parensis_close_reader_();
}

static void defun_parensis_close_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PARENSIS_CLOSE_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_parensis_close);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun semicolon-reader (stream character) ...) -> * */
static int function_reader_semicolon(Execute ptr, addr stream, addr code)
{
	Return(semicolon_reader_(stream));
	setvalues_nil_control(ptr);
	return 0;
}

static void defun_semicolon_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SEMICOLON_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_semicolon);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun backquote-reader (stream character) ...) -> * */
static int function_reader_backquote(Execute ptr, addr stream, addr code)
{
	Return(backquote_reader_(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_backquote_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BACKQUOTE_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_backquote);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun comma-reader (stream character) ...) -> * */
static int function_reader_comma(Execute ptr, addr stream, addr code)
{
	Return(comma_reader_(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_comma_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_COMMA_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_comma);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sharp-reader (stream character) ...) -> * */
static int function_reader_sharp(Execute ptr, addr stream, addr code)
{
	return sharp_reader_(ptr, stream, code);
}

static void defun_sharp_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SHARP_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_sharp);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
	/* (setq dispatch-function #'sharp-reader) */
	GetConst(SYSTEM_DISPATCH_FUNCTION, &symbol);
	SetFunctionSymbol(symbol, pos);
}


/* build */
static void reader_dispatch_function(void)
{
	defun_double_quote_reader(); /* " */
	defun_single_quote_reader(); /* ' */
	defun_parensis_open_reader(); /* ( */
	defun_parensis_close_reader();  /* ) */
	defun_semicolon_reader(); /* ; */
	defun_backquote_reader(); /* ` */
	defun_comma_reader(); /* , */
	defun_sharp_reader(); /* # */
}


/*****************************************************************************
 *  dispatch character
 *****************************************************************************/
/* (defun error-dispatch (stream code arg) ...) -> * */
static int function_dispatch_error(Execute ptr, addr stream, addr code, addr arg)
{
	return error_dispatch_(code);
}

static void defun_error_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ERROR_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_error);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun equal-dispatch (stream code arg) ...) -> * */
static int function_dispatch_equal(Execute ptr, addr stream, addr code, addr arg)
{
	Return(equal_dispatch_(ptr, stream, code, arg, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_equal_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EQUAL_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_equal);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sharp-dispatch (stream code arg) ...) -> * */
static int function_dispatch_sharp(Execute ptr, addr stream, addr code, addr arg)
{
	Return(sharp_dispatch_(ptr, arg, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_sharp_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SHARP_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_sharp);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun single-quote-dispatch (stream code arg) ...) -> * */
static int function_dispatch_single_quote(Execute ptr,
		addr stream, addr code, addr arg)
{
	Return(single_quote_dispatch_(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_single_quote_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SINGLE_QUOTE_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_single_quote);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parensis-open-dispatch (stream code arg) ...) -> * */
static int function_dispatch_parensis_open(Execute ptr,
		addr stream, addr code, addr arg)
{
	Return(parensis_open_dispatch_(ptr, stream, arg, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_parensis_open_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PARENSIS_OPEN_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_parensis_open);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parensis-close-dispatch (stream code arg) ...) -> * */
static int function_dispatch_parensis_close(Execute ptr,
		addr stream, addr code, addr arg)
{
	return parensis_close_dispatch_();
}

static void defun_parensis_close_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PARENSIS_CLOSE_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_parensis_close);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun asterisk-dispatch (stream code arg) ...) -> * */
static int function_dispatch_asterisk(Execute ptr, addr stream, addr code, addr arg)
{
	Return(asterisk_dispatch_(ptr, stream, code, arg, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_asterisk_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ASTERISK_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_asterisk);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun colon-dispatch (stream code arg) ...) -> * */
static int function_dispatch_colon(Execute ptr, addr stream, addr code, addr arg)
{
	Return(colon_dispatch_(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_colon_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_COLON_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_colon);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun less-dispatch (stream code arg) ...) -> * */
static int function_dispatch_less(Execute ptr, addr stream, addr code, addr arg)
{
	return less_dispatch_();
}

static void defun_less_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_LESS_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_less);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun backslash-dispatch (stream code arg) ...) -> * */
static int function_dispatch_backslash(Execute ptr, addr stream, addr code, addr arg)
{
	Return(backslash_dispatch_(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_backslash_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BACKSLASH_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_backslash);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun or-dispatch (stream code arg) ...) -> (values) */
static int function_dispatch_or(Execute ptr, addr stream, addr code, addr arg)
{
	Return(or_dispatch_(stream));
	setvalues_nil_control(ptr);
	return 0;
}

static void defun_or_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_OR_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_or);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun plus-dispatch (stream code arg) ...) -> * */
static int function_dispatch_plus(Execute ptr, addr stream, addr code, addr arg)
{
	return plus_dispatch_(ptr, stream);
}

static void defun_plus_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PLUS_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_plus);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun minus-dispatch (stream code arg) ...) -> * */
static int function_dispatch_minus(Execute ptr, addr stream, addr code, addr arg)
{
	return minus_dispatch_(ptr, stream);
}

static void defun_minus_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MINUS_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_minus);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun dot-dispatch (stream code arg) ...) -> * */
static int function_dispatch_dot(Execute ptr, addr stream, addr code, addr arg)
{
	Return(dot_dispatch_(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_dot_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DOT_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_dot);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun radix-dispatch (stream code arg) ...) -> * */
static int function_dispatch_radix(Execute ptr, addr stream, addr code, addr arg)
{
	Return(radix_dispatch_(ptr, stream, arg, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_radix_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_RADIX_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_radix);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun binary-dispatch (stream code arg) ...) -> * */
static int function_dispatch_binary(Execute ptr, addr stream, addr code, addr arg)
{
	Return(binary_dispatch_(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_binary_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BINARY_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_binary);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun octal-dispatch (stream code arg) ...) -> * */
static int function_dispatch_octal(Execute ptr, addr stream, addr code, addr arg)
{
	Return(octal_dispatch_(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_octal_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_OCTAL_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_octal);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hexadecimal-dispatch (stream code arg) ...) -> * */
static int function_dispatch_hexadecimal(Execute ptr, addr stream, addr code, addr arg)
{
	Return(hexadecimal_dispatch_(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_hexadecimal_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_HEXADECIMAL_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_hexadecimal);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun complex-dispatch (stream code arg) ...) -> * */
static int function_dispatch_complex(Execute ptr, addr stream, addr code, addr arg)
{
	Return(complex_dispatch_(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_complex_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_COMPLEX_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_complex);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-dispatch (stream code arg) ...) -> * */
static int function_dispatch_array(Execute ptr, addr stream, addr code, addr arg)
{
	Return(array_dispatch_(ptr, stream, arg, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_array_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ARRAY_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_array);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-dispatch (stream code arg) ...) -> * */
static int function_dispatch_pathname(Execute ptr, addr stream, addr code, addr arg)
{
	Return(pathname_dispatch_(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_pathname_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PATHNAME_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_pathname);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun structure-dispatch (stream code arg) ...) -> structure-object */
static int function_dispatch_structure(Execute ptr, addr stream, addr code, addr arg)
{
	Return(structure_dispatch_(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_structure_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_STRUCTURE_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_structure);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static void reader_dispatch_sharp(void)
{
	defun_error_dispatch();           /* whitespace */
	defun_equal_dispatch();           /* #= */
	defun_sharp_dispatch();           /* ## */
	defun_single_quote_dispatch();    /* #' */
	defun_parensis_open_dispatch();   /* #( */
	defun_parensis_close_dispatch();  /* #) */
	defun_asterisk_dispatch();        /* #* */
	defun_colon_dispatch();           /* #: */
	defun_less_dispatch();            /* #< */
	defun_backslash_dispatch();       /* #\ */
	defun_or_dispatch();              /* #| */
	defun_plus_dispatch();            /* #+ */
	defun_minus_dispatch();           /* #- */
	defun_dot_dispatch();             /* #. */
	defun_radix_dispatch();           /* #R */
	defun_binary_dispatch();          /* #B */
	defun_octal_dispatch();           /* #O */
	defun_hexadecimal_dispatch();     /* #X */
	defun_complex_dispatch();         /* #C */
	defun_array_dispatch();           /* #A */
	defun_pathname_dispatch();        /* #P */
	defun_structure_dispatch();       /* #S */
}


/*****************************************************************************
 *  build-readtable
 *****************************************************************************/
static void reader_dispatch_constant(void)
{
	addr symbol, gensym, name;

	/* (setq readtable-dot (make-symbol "READTABLE-DOT")) */
	GetConst(SYSTEM_READTABLE_DOT, &symbol);
	symbol_heap(&gensym);
	GetNameSymbol(symbol, &name);
	SetNameSymbol(gensym, name);
	SetValueSymbol(symbol, gensym);
	SetStatusReadOnly(symbol);
}

void build_reader_dispatch(void)
{
	reader_dispatch_function();
	reader_dispatch_sharp();
	reader_dispatch_constant();
}

void init_reader_dispatch(void)
{
	SetPointerCall(defun, var2, reader_double_quote);
	SetPointerCall(defun, var2, reader_single_quote);
	SetPointerCall(defun, var2, reader_parensis_open);
	SetPointerCall(defun, var2, reader_parensis_close);
	SetPointerCall(defun, var2, reader_semicolon);
	SetPointerCall(defun, var2, reader_backquote);
	SetPointerCall(defun, var2, reader_comma);
	SetPointerCall(defun, var2, reader_sharp);
	SetPointerCall(defun, var3, dispatch_error);
	SetPointerCall(defun, var3, dispatch_equal);
	SetPointerCall(defun, var3, dispatch_sharp);
	SetPointerCall(defun, var3, dispatch_single_quote);
	SetPointerCall(defun, var3, dispatch_parensis_open);
	SetPointerCall(defun, var3, dispatch_parensis_close);
	SetPointerCall(defun, var3, dispatch_asterisk);
	SetPointerCall(defun, var3, dispatch_colon);
	SetPointerCall(defun, var3, dispatch_less);
	SetPointerCall(defun, var3, dispatch_backslash);
	SetPointerCall(defun, var3, dispatch_or);
	SetPointerCall(defun, var3, dispatch_plus);
	SetPointerCall(defun, var3, dispatch_minus);
	SetPointerCall(defun, var3, dispatch_dot);
	SetPointerCall(defun, var3, dispatch_radix);
	SetPointerCall(defun, var3, dispatch_binary);
	SetPointerCall(defun, var3, dispatch_octal);
	SetPointerCall(defun, var3, dispatch_hexadecimal);
	SetPointerCall(defun, var3, dispatch_complex);
	SetPointerCall(defun, var3, dispatch_array);
	SetPointerCall(defun, var3, dispatch_pathname);
	SetPointerCall(defun, var3, dispatch_structure);
}


/************************************************************
 *  reader_function.c
 ************************************************************/

/*****************************************************************************
 *  reader macro
 *****************************************************************************/
/*
 *  reader "
 */
int double_quote_reader_(LocalRoot local, addr stream, addr *ret)
{
	int escape, check;
	unicode c;
	addr queue;
	LocalStack stack;

	push_local(local, &stack);
	charqueue_local(local, &queue, 0);
	escape = 0;
	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check)
			return fmte_("The string token must terminate by \".", NULL);
		if (escape) {
			Return(push_charqueue_local_(local, queue, c));
			escape = 0;
			continue;
		}
		if (c == '\"') {
			break;
		}
		if (c == '\\') {
			escape = 1;
		}
		else {
			Return(push_charqueue_local_(local, queue, c));
		}
	}
	make_charqueue_heap(queue, ret);

	return 0;
}


/*
 *  reader '
 */
static int quote_macro_reader_(Execute ptr, constindex index, addr stream, addr *ret)
{
	int check;
	addr pos, quote;

	/* readtable */
	Return(read_recursive_(ptr, stream, &check, &pos));
	if (check)
		return Result(ret, Unbound);

	/* ([index] pos) */
	GetConstant(index, &quote);
	list_heap(ret, quote, pos, NULL);

	return 0;
}

int single_quote_reader_(Execute ptr, addr stream, addr *ret)
{
	Return(quote_macro_reader_(ptr, CONSTANT_COMMON_QUOTE, stream, ret));
	if (stream == Unbound)
		return fmte_("After character ' must be an object, but EOF.", NULL);

	return 0;
}


/*
 *  reader (
 */
static int read_delimited_read(Execute ptr, addr queue, addr dot, addr pos, int *mode)
{
	addr root;

	switch (*mode) {
		case 0:
			if (dot == pos) {
				rootqueue(queue, &root);
				if (root == Nil) /* (. */
					return 1; /* error */
				*mode = 1; /* (a b . */
				return 2; /* continue */
			}
			return 0;

		case 1:
			if (dot == pos)  /* (a b . . */
				return 1;  /* error */
			dotqueue_readlabel(ptr, queue, pos);
			*mode = 2;  /* (a b . c */
			return 2; /* continue */

		default: /* (a b . c d */
			return 1;  /* error */
	}
}

static int read_delimited_execute(Execute ptr, addr stream, unicode limit)
{
	enum ReadTable_Type type;
	int mode, check;
	unicode c;
	addr table, pos, root, dotsym, queue;
	LocalHold hold;

	mode = 0;
	GetConst(SYSTEM_READTABLE_DOT, &dotsym);
	Return(getreadtable_(ptr, &table));
	queue_heap(&queue);
	hold = LocalHold_array(ptr, 1);
	localhold_push(hold, queue);
	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check)
			return call_end_of_file_(ptr, stream);
		Return(readtable_typetable_(table, c, &type));
		if (type == ReadTable_Type_whitespace) {
			/* discard character */
			continue;
		}
		if (c == limit) {
			/* discard limit character */
			break;
		}
		Return(unread_char_stream_(stream, c));
		Return(readtable_novalue_(ptr, &check, &pos, stream, table));
		if (0 < check)
			return fmte_("read error", NULL);
		if (check < 0)
			continue;
		localhold_set(hold, 0, pos);

		/* dot */
		check = read_delimited_read(ptr, queue, dotsym, pos, &mode);
		if (check == 1)
			return fmte_("dot no allowed here", NULL);
		if (check == 2)
			continue;
		pushqueue_readlabel(ptr, queue, pos);
	}
	localhold_end(hold);

	if (mode == 1) /* (a b . ) */
		return fmte_("dot no allowed here", NULL);

	rootqueue(queue, &root);
	setresult_control(ptr, root);

	return 0;
}

static int read_delimited_list_call_(Execute ptr, addr stream, unicode limit, int recp)
{
	addr info;
	struct readinfo_struct *str;

	/* push */
	/* code */
	if (recp) {
		Return(pushreadinfo_recursive_(ptr, &info));
	}
	else {
		pushreadinfo(ptr, &info);
	}
	str = ReadInfoStruct(info);
	str->dot = 1;
	return read_delimited_execute(ptr, stream, limit);
}

int read_delimited_list_(Execute ptr, addr stream, unicode limit, int recp)
{
	addr control;

	push_control(ptr, &control);
	(void)read_delimited_list_call_(ptr, stream, limit, recp);
	return pop_control_(ptr, control);
}

int parensis_open_reader_(Execute ptr, addr stream)
{
	return read_delimited_list_(ptr, stream, ')', 1);
}


/*
 *  reader )
 */
int parensis_close_reader_(void)
{
	return fmte_("unmatch close parenthiesis ).", NULL);
}


/*
 *  reader ;
 */
int semicolon_reader_(addr stream)
{
	int check;
	unicode c;

	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check || c == '\n')
			break;
	}

	return 0;
}


/*
 *  reader `
 */
static int backquote_read_reader_call_(Execute ptr, LocalHold hold,
		addr stream, int *result, addr *ret)
{
	addr info;

	Return(pushreadinfo_recursive_(ptr, &info));
	ReadInfoStruct(info)->backquote++;
	Return(read_call_(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

static int backquote_read_reader_(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)backquote_read_reader_call_(ptr, hold, stream, result, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

int backquote_reader_(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr pos;

	Return(backquote_read_reader_(ptr, stream, &check, &pos));
	if (check)
		return fmte_("After backquote ` must be an object.", NULL);

	return quote_back_heap_(ret, pos);
}


/*
 *  reader ,
 */
static int comma_read_reader_call_(Execute ptr, LocalHold hold,
		addr stream, int *result, addr *ret)
{
	addr info;

	Return(pushreadinfo_recursive_(ptr, &info));
	ReadInfoStruct(info)->backquote--;
	Return(read_call_(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

static int comma_read_reader_(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)comma_read_reader_call_(ptr, hold, stream, result, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

int comma_reader_(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr pos;
	unicode c;

	/* check */
	getreadinfo(ptr, &pos);
	if (ReadInfoStruct(pos)->backquote == 0)
		return fmte_("The comma , is not inside backquote.", NULL);

	/* read */
	Return(read_char_stream_(stream, &c, &check));
	if (check)
		return fmte_("After comma , must be a character or an object.", NULL);
	if (c == '@') {
		Return(comma_read_reader_(ptr, stream, &check, &pos));
		if (check)
			return fmte_("After ,@ must be an object.", NULL);
		quote_atsign_heap(&pos, pos);
	}
	else if (c == '.') {
		Return(comma_read_reader_(ptr, stream, &check, &pos));
		if (check)
			return fmte_("After ,. must be an object.", NULL);
		quote_dot_heap(&pos, pos);
	}
	else {
		Return(unread_char_stream_(stream, c));
		Return(comma_read_reader_(ptr, stream, &check, &pos));
		if (check)
			return fmte_("After comma , must be an object.", NULL);
		quote_comma_heap(&pos, pos);
	}

	return Result(ret, pos);
}


/*
 *  reader #
 */
static int sharp_parameter_reader_(LocalRoot local, addr stream, addr *ret, unicode *rc)
{
	int check;
	unicode c;
	addr cons;
	LocalStack stack;

	/* no digit */
	Return(read_char_stream_(stream, &c, &check));
	if (check) {
		*rc = 0;
		return Result(ret, NULL);
	}
	if (! isDigitCase(c)) {
		*rc = c;
		return Result(ret, Nil);
	}

	/* parse digit */
	push_local(local, &stack);
	bigcons_local(local, &cons);
	for (;;) {
		push_bigcons(local, cons, 10, (unsigned)(c - '0'));
		Return(read_char_stream_(stream, &c, &check));
		if (check) {
			*rc = 0;
			return Result(ret, NULL);
		}
		if (! isDigitCase(c)) {
			*rc = c;
			break;
		}
	}
	integer_cons_heap(ret, signplus_bignum, cons);
	rollback_local(local, stack);

	return 0;
}

int sharp_reader_(Execute ptr, addr stream, addr code)
{
	addr arg, pos, code2;
	unicode x, y;

	/* #[integer][code] */
	Return(sharp_parameter_reader_(ptr->local, stream, &arg, &y));
	if (arg == NULL)
		return fmte_("Invalid dispatch character form ~S.", code, NULL);
	character_heap(&code2, y);
	y = toUpperUnicode(y);

	/* macro character */
	Return(getreadtable_(ptr, &pos));
	GetDispatchReadtable(pos, &pos);
	GetCharacter(code, &x);
	Return(findnil_character2_hashtable_(pos, x, y, &pos));
	if (pos == Nil)
		return fmte_("There is no macro character ~S-~S.", code, code2, NULL);

	return funcall_control_(ptr, pos, stream, code2, arg, NULL);
}


/*****************************************************************************
 *  dispatch macro
 *****************************************************************************/
/*
 *  dispatch # whitespace
 */
/* (defun error-dispatch (stream code arg) ...) -> * */
int error_dispatch_(addr code)
{
	return fmte_("don't allow ~S dispatch character.", code, NULL);
}


/*
 *  dispatch #n=
 */
static void equal_finalize_dispatch(addr pos, addr value)
{
	ReadInfoStruct(pos)->replace = value != Nil? 1: 0;
}

static int equal_read_dispatch_call_(Execute ptr, addr stream, int *result, addr *ret)
{
	int escape;
	addr pos, value;
	struct readinfo_struct *str;

	/* readinfo */
	getreadinfo(ptr, &pos);
	str = ReadInfoStruct(pos);
	value = str->replace? T: Nil;
	str->replace = 1;

	/* code */
	escape = read_recursive_(ptr, stream, result, ret);
	equal_finalize_dispatch(pos, value);
	return escape;
}

static int equal_read_dispatch_(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	(void)equal_read_dispatch_call_(ptr, stream, result, ret);
	return pop_control_(ptr, control);
}

int equal_dispatch_(Execute ptr, addr stream, addr x, addr y, addr *ret)
{
	int check;
	addr pos, label;

	Return(pushlabel_readinfo_(ptr, y, &label));
	Return(equal_read_dispatch_(ptr, stream, &check, &pos));
	if (check)
		return fmte_("After dispatch character ~S must be an object.", x, NULL);
	Return(closelabel_readlabel_(ptr, label, pos));

	return Result(ret, pos);
}


/*
 *  dispatch #n#
 */
int sharp_dispatch_(Execute ptr, addr y, addr *ret)
{
	addr pos;

	getreadinfo(ptr, &pos);
	GetReadInfo(pos, ReadInfo_Label, &pos);
	Check(! consp(pos), "type error");
	GetCar(pos, &pos);
	if (! find_readlabel(y, pos, &pos))
		return fmte_("The #n# label ~S is not exist.", y, NULL);

	return Result(ret, pos);
}


/*
 *  dispatch #'
 */
int single_quote_dispatch_(Execute ptr, addr stream, addr *ret)
{
	Return(quote_macro_reader_(ptr, CONSTANT_COMMON_FUNCTION, stream, ret));
	if (stream == Unbound)
		return fmte_("After character #' must be a function-designator, but EOF.", NULL);

	return 0;
}


/*
 *  dispatch #(
 */
static void parensis_open_normal_dispatch(addr cons, size_t size, addr *ret)
{
	addr vector, pos;
	size_t i, index;

	vector_heap(&vector, size);
	for (i = 0; i < size; i++) {
		index = size - i - 1;
		GetCons(cons, &pos, &cons);
		setarray(vector, index, pos);
	}
	*ret = vector;
}

static void parensis_open_limit_dispatch(addr cons, size_t size, addr *ret)
{
	addr vector, pos;
	size_t i;

	vector_heap(&vector, size);
	nreverse(&cons, cons);
	pos = Nil;
	for (i = 0; i < size; i++) {
		if (cons != Nil)
			GetCons(cons, &pos, &cons);
		setarray(vector, i, pos);
	}
	*ret = vector;
}

int parensis_open_dispatch_(Execute ptr, addr stream, addr y, addr *ret)
{
	enum ReadTable_Type type;
	int check;
	unicode c;
	addr table, root, pos;
	LocalRoot local;
	LocalStack stack;
	size_t size, limit;

	/* parameter */
	if (y != Nil && GetIndex_integer(y, &limit))
		return fmte_("Too large dispatch parameter ~S.", y, NULL);

	/* read list */
	local = ptr->local;
	push_local(local, &stack);
	Return(getreadtable_(ptr, &table));
	root = Nil;
	size = 0;
	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check)
			return call_end_of_file_(ptr, stream);
		Return(readtable_typetable_(table, c, &type));
		if (type == ReadTable_Type_whitespace)
			continue;
		if (c == ')')
			break;
		Return(unread_char_stream_(stream, c));
		Return(read_recursive_(ptr, stream, &check, &pos));
		if (check)
			return fmte_("read error", NULL);
		cons_local(local, &root, pos, root);
		size++;

		/* size check */
		if (y != Nil && limit < size)
			return fmte_("Too many vector parameter.", NULL);
	}

	/* make vector */
	if (y == Nil) {
		parensis_open_normal_dispatch(root, size, &root);
	}
	else {
		if (root == Nil) {
			if (limit == 0) {
				vector_heap(&root, 0);
			}
			else {
				return fmte_("The vector don't initialize "
						"because body is empty #n().", NULL);
			}
		}
		else {
			parensis_open_limit_dispatch(root, limit, &root);
		}
	}
	vector_readlabel(ptr, root);
	rollback_local(local, stack);

	return Result(ret, root);
}


/*
 *  dispatch #)
 */
int parensis_close_dispatch_(void)
{
	return fmte_("unmatch close parenthiesis ).", NULL);
}


/*
 *  dispatch #*
 */
static int asterisk_bitcons_dispatch_(Execute ptr, addr stream, addr code, addr *ret)
{
	int check;
	unicode c;
	addr pos, cons;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	bitcons_local(local, &cons, 0);
	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check)
			break;
		if (c == '0') {
			push_bitcons(local, cons, 0);
		}
		else if (c == '1') {
			push_bitcons(local, cons, 1);
		}
		else {
			Return(unread_char_stream_(stream, c));
			break;
		}
	}
	bitmemory_cons_heap(&pos, cons);
	rollback_local(local, stack);

	return Result(ret, pos);
}

static int asterisk_size_dispatch_(Execute ptr, addr stream, size_t size, addr *ret)
{
	int last, check;
	unicode c;
	addr pos;
	size_t i;

	/* read bit-vector */
	bitmemory_heap(&pos, size);
	last = 0;
	for (i = 0; ; i++) {
		if (size <= i)
			return fmte_("Too large bit-vector.", NULL);
		Return(read_char_stream_(stream, &c, &check));
		if (check)
			break;
		if (c == '0') {
			Return(bitmemory_setint_(pos, i, 0));
			last = 0;
		}
		else if (c == '1') {
			Return(bitmemory_setint_(pos, i, 1));
			last = 1;
		}
		else {
			Return(unread_char_stream_(stream, c));
			break;
		}
	}

	/* fill last value */
	for (; i < size; i++) {
		Return(bitmemory_setint_(pos, i, last));
	}

	/* result */
	return Result(ret, pos);
}

int asterisk_dispatch_(Execute ptr, addr stream, addr x, addr y, addr *ret)
{
	size_t size;

	if (y == Nil) {
		Return(asterisk_bitcons_dispatch_(ptr, stream, x, ret));
	}
	else {
		if (GetIndex_integer(y, &size))
			return fmte_("The index size ~S is too large.", y, NULL);
		Return(asterisk_size_dispatch_(ptr, stream, size, ret));
	}

	return 0;
}


/*
 *  dispatch #:
 */
static int colon_object_dispatch_(Execute ptr, addr stream, addr *ret)
{
	enum ReadTable_Result value;
	addr table;

	Return(getreadtable_(ptr, &table));
	setstate_readinfo(ptr, ReadInfo_State_Gensym);
	Return(readtable_result_(ptr, ret, stream, table, &value));
	switch (value) {
		case ReadTable_Result_normal:
			break;

		case ReadTable_Result_eof:
			return fmte_("After character #: must be an object, but EOF.", NULL);

		case ReadTable_Result_macro:
			return fmte_("After character #: don't allow a macro character.", NULL);

		default:
			return fmte_("Invalid result.", NULL);
	}

	return 0;
}

static int colon_dispatch_call_(Execute ptr, addr stream, addr *ret)
{
	addr pos;
	Return(pushreadinfo_recursive_(ptr, &pos));
	return colon_object_dispatch_(ptr, stream, ret);
}

int colon_dispatch_(Execute ptr, addr stream, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	(void)colon_dispatch_call_(ptr, stream, ret);
	return pop_control_(ptr, control);
}


/*
 *  dispatch #<
 */
int less_dispatch_(void)
{
	return fmte_("Cannot read #< dispatch character.", NULL);
}


/*
 *  dispatch #\
 */
int backslash_dispatch_(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr table, pos;

	Return(getreadtable_(ptr, &table));
	Return(unread_char_stream_(stream, '\\'));
	Return(read_recursive_(ptr, stream, &check, &pos));
	if (check)
		return fmte_("Cannot read character name.", NULL);
	Return(read_suppress_p_(ptr, &check));
	if (check) {
		setresult_control(ptr, Nil);
		return 0;
	}
	if (! symbolp(pos))
		return fmte_("Invalid character type ~S.", pos, NULL);
	Return(find_name_char_(&pos, pos));
	if (pos == Nil)
		return fmte_("The character name ~S is not found.", pos, NULL);

	return Result(ret, pos);
}


/*
 *  dispatch #|
 */
int or_dispatch_(addr stream)
{
	int check;
	unicode u;
	size_t count;

	count = 0;
start:
	Return(read_char_stream_(stream, &u, &check));
	if (check)
		goto finish;
	if (u == '#')
		goto begin;
	if (u == '|')
		goto end;
	goto start;

begin:
	Return(read_char_stream_(stream, &u, &check));
	if (check)
		goto finish;
	if (u == '|')
		count++;
	goto start;

end:
	Return(read_char_stream_(stream, &u, &check));
	if (check)
		goto finish;
	if (u == '|')
		goto end;
	if (u != '#')
		goto start;
	if (count == 0)
		goto finish;
	count--;
	goto start;

finish:
	return 0;
}


/*
 *  dispatch #+
 */
static int feature_read_dispatch_call_(Execute ptr, LocalHold hold,
		addr stream, int *result, addr *ret)
{
	addr symbol, keyword;

	GetConst(SPECIAL_PACKAGE, &symbol);
	GetConst(PACKAGE_KEYWORD, &keyword);
	pushspecial_control(ptr, symbol, keyword);
	Return(read_recursive_(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

static int feature_read_dispatch_(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control;
	LocalHold hold;

	/* (let ((*package* (find-package "KEYWORD")))
	 *   (read))
	 */
	/* push */
	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	feature_read_dispatch_call_(ptr, hold, stream, result, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

static int feature_eq_dispatch(addr pos, constindex index1, constindex index2)
{
	addr check;

	GetConstant(index1, &check);
	if (check == pos)
		return 1;
	GetConstant(index2, &check);
	return check == pos;
}
static int feature_not_dispatch(addr pos)
{
	return feature_eq_dispatch(pos, CONSTANT_KEYWORD_NOT, CONSTANT_COMMON_NOT);
}
static int feature_and_dispatch(addr pos)
{
	return feature_eq_dispatch(pos, CONSTANT_KEYWORD_AND, CONSTANT_COMMON_AND);
}
static int feature_or_dispatch(addr pos)
{
	return feature_eq_dispatch(pos, CONSTANT_KEYWORD_OR, CONSTANT_COMMON_OR);
}

static int feature_check_dispatch_(addr list, addr pos, int *ret);
static int feature_cons_dispatch_(addr list, addr cons, int *ret)
{
	int check;
	addr car, cdr;

	Return_getcons(cons, &car, &cdr);
	/* not */
	if (feature_not_dispatch(car)) {
		if (! singlep(cdr))
			return fmte_("The feature ~S must be a (not x) form.", cons, NULL);
		GetCar(cdr, &car);
		Return(feature_check_dispatch_(list, car, &check));
		return Result(ret, ! check);
	}
	/* and */
	if (feature_and_dispatch(car)) {
		while (cdr != Nil) {
			Return_getcons(cdr, &car, &cdr);
			Return(feature_check_dispatch_(list, car, &check));
			if (! check)
				return Result(ret, 0);
		}
		return Result(ret, 1);
	}
	/* or */
	if (feature_or_dispatch(car)) {
		while (cdr != Nil) {
			Return_getcons(cdr, &car, &cdr);
			Return(feature_check_dispatch_(list, car, &check));
			if (check)
				return Result(ret, 1);
		}
		return Result(ret, 0);
	}
	/* error */
	return fmte_("Invalid feature operator ~S.", car, NULL);
}

static int feature_check_dispatch_(addr list, addr pos, int *ret)
{
	if (symbolp(pos))
		return find_list_eq_safe_(pos, list, ret);
	else if (consp(pos))
		return feature_cons_dispatch_(list, pos, ret);
	else
		return fmte_("Invalid feature ~S.", pos, NULL);
}

static int feature_ignore_dispatch_(Execute ptr, addr stream, int *result)
{
	/* (let ((*read-suppress* t)) ...) */
	addr control, symbol;

	push_control(ptr, &control);
	GetConst(SPECIAL_READ_SUPPRESS, &symbol);
	pushspecial_control(ptr, symbol, T);
	(void)read_recursive_(ptr, stream, result, &stream);
	return pop_control_(ptr, control);
}

int plus_dispatch_(Execute ptr, addr stream)
{
	int check;
	addr feature, form, list;
	LocalHold hold;

	/* read feature, read form */
	Return(feature_read_dispatch_(ptr, stream, &check, &feature));
	if (check)
		return fmte_("After dispatch #+ must be a feature form.", NULL);

	/* check *features* */
	GetConst(SPECIAL_FEATURES, &list);
	Return(getspecialcheck_local_(ptr, list, &list));
	hold = LocalHold_local_push(ptr, feature);
	Return(feature_check_dispatch_(list, feature, &check));
	if (check) {
		Return(read_recursive_(ptr, stream, &check, &form));
		localhold_end(hold);
		if (check)
			return fmte_("After dispatch #+feature must be a object.", NULL);
		setresult_control(ptr, form);
	}
	else {
		Return(feature_ignore_dispatch_(ptr, stream, &check));
		localhold_end(hold);
		if (check)
			return fmte_("After dispatch #+feature must be a object.", NULL);
		setvalues_nil_control(ptr);
	}

	return 0;
}


/*
 *  dispatch #-
 */
int minus_dispatch_(Execute ptr, addr stream)
{
	int check;
	addr feature, form, list;
	LocalHold hold;

	/* read feature, read form */
	Return(feature_read_dispatch_(ptr, stream, &check, &feature));
	if (check)
		return fmte_("After dispatch #- must be a feature form.", NULL);

	/* check *features* */
	GetConst(SPECIAL_FEATURES, &list);
	Return(getspecialcheck_local_(ptr, list, &list));
	hold = LocalHold_local_push(ptr, feature);
	Return(feature_check_dispatch_(list, feature, &check));
	if (! check) {
		Return(read_recursive_(ptr, stream, &check, &form));
		localhold_end(hold);
		if (check)
			return fmte_("After dispatch #-feature must be a object.", NULL);
		setresult_control(ptr, form);
	}
	else {
		Return(feature_ignore_dispatch_(ptr, stream, &check));
		localhold_end(hold);
		if (check)
			return fmte_("After dispatch #-feature must be a object.", NULL);
		setvalues_nil_control(ptr);
	}

	return 0;
}


/*
 *  dispatch #.
 */
int dot_dispatch_(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr eval;
	LocalHold hold;

	GetConst(SPECIAL_READ_EVAL, &eval);
	Return(getspecialcheck_local_(ptr, eval, &eval));
	if (eval == Nil) {
		return call_simple_reader_error_va_(ptr,
				"The dispatch #. don't read when *read-eval* is nil.", NULL);
	}
	Return(read_recursive_(ptr, stream, &check, &eval));
	if (check)
		return fmte_("After dispatch #. must be a object.", NULL);
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);

	hold = LocalHold_local_push(ptr, eval);
	Return(eval_result_partial_form_(ptr, eval, &eval));
	localhold_end(hold);

	return Result(ret, eval);
}


/*
 *  dispatch #R
 */
static int radix_execute_dispatch_call_(Execute ptr, LocalHold hold,
		addr stream, fixnum base, int *result, addr *ret)
{
	addr symbol, value;

	GetConst(SPECIAL_READ_BASE, &symbol);
	fixnum_heap(&value, base);
	pushspecial_control(ptr, symbol, value);
	Return(read_recursive_(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

static int radix_execute_dispatch_(Execute ptr, addr stream, fixnum base,
		int *result, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)radix_execute_dispatch_call_(ptr, hold, stream, base, result, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

static int radix_read_dispatch_(Execute ptr, addr stream, fixnum base, addr *ret)
{
	int check;
	addr pos;

	Return(radix_execute_dispatch_(ptr, stream, base, &check, &pos));
	if (check)
		return fmte_("After radix dispatch #<n>r must be an integer.", NULL);
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);
	if (! rationalp(pos))
		return fmte_("The radix value ~S must be an integer.", pos, NULL);

	return Result(ret, pos);
}

int radix_dispatch_(Execute ptr, addr stream, addr y, addr *ret)
{
	int check;
	fixnum value;

	GetFixnum_signed(y, &value);
	if (! isBaseChar(value)) {
		Return(read_suppress_p_(ptr, &check));
		if (! check)
			return fmte_("The radix ~S must be a number between 2 and 36.", y, NULL);
	}

	return radix_read_dispatch_(ptr, stream, value, ret);
}


/*
 *  dispatch #B
 */
int binary_dispatch_(Execute ptr, addr stream, addr *ret)
{
	return radix_read_dispatch_(ptr, stream, 2, ret);
}


/*
 *  dispatch #O
 */
int octal_dispatch_(Execute ptr, addr stream, addr *ret)
{
	return radix_read_dispatch_(ptr, stream, 8, ret);
}


/*
 *  dispatch #X
 */
int hexadecimal_dispatch_(Execute ptr, addr stream, addr *ret)
{
	return radix_read_dispatch_(ptr, stream, 16, ret);
}


/*
 *  dispatch #C
 */
int complex_dispatch_(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr form, pos, real, imag;

	Return(read_recursive_(ptr, stream, &check, &form));
	if (check)
		return fmte_("After complex dispatch must be a (real imag) form.", NULL);
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);
	pos = form;
	if (! consp_getcons(pos, &real, &pos))
		goto error;
	if (! consp_getcons(pos, &imag, &pos))
		goto error;
	if (pos != Nil)
		goto error;
	if (! realp(real))
		goto error;
	if (! realp(imag))
		goto error;
	Return(complex_heap_(&pos, real, imag));
	return Result(ret, pos);

error:
	return fmte_("The complex dispatch ~S must be a (real imag) form.", form, NULL);
}


/*
 *  dispatch #A
 */
int array_dispatch_(Execute ptr, addr stream, addr y, addr *ret)
{
	int check, ignore;
	addr form;

	Return(read_suppress_p_(ptr, &ignore));
	if (y == Nil && (! ignore))
		return fmte_("There is no rank parameter at the #<n>a dispatch.", NULL);
	Return(read_recursive_(ptr, stream, &check, &form));
	if (ignore)
		return Result(ret, Nil);
	if (check)
		return fmte_("After array dispatch must be an initial-contents form.", NULL);
	Return(array_contents_heap_(&form, y, form));
	Return(array_readlabel_(ptr, form));

	return Result(ret, form);
}


/*
 *  dispatch #P
 */
int pathname_dispatch_(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr pos;

	Return(read_recursive_(ptr, stream, &check, &pos));
	if (check)
		return fmte_("After #P must be a pathname-designator.", NULL);
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);
	Return(pathname_designator_heap_(ptr, pos, &pos));

	return Result(ret, pos);
}


/*
 *  dispatch #S
 */
int structure_dispatch_(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr pos, rest;
	LocalHold hold;

	Return(read_recursive_(ptr, stream, &check, &pos));
	if (check)
		goto error;
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);
	if (! consp_getcons(pos, &pos, &rest))
		goto error;

	hold = LocalHold_local_push(ptr, pos);
	Return(structure_reader_(ptr, pos, rest, &pos));
	localhold_end(hold);
	return Result(ret, pos);

error:
	return fmte_("After #S must be (name key value ...) form.", NULL);
}


/************************************************************
 *  reader_info.c
 ************************************************************/

void get_readinfo(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_READINFO);
	GetReadInfo_Low(pos, index, ret);
}

void set_readinfo(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_READINFO);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetReadInfo_Low(pos, index, value);
}

struct readinfo_struct *struct_readinfo(addr pos)
{
	CheckType(pos, LISPSYSTEM_READINFO);
	return ReadInfoStruct_Low(pos);
}


/*
 *  readinfo
 */
static void readinfo_local(LocalRoot local, addr *ret)
{
	addr pos, value;
	struct readinfo_struct *str;

	/* readinfo */
	local_smallsize(local, &pos, LISPSYSTEM_READINFO,
			ReadInfo_Size, sizeoft(struct readinfo_struct));
	str = ReadInfoStruct(pos);
	clearpoint(str);
	/* charqueue */
	charqueue_local(local, &value, 0);
	SetReadInfo(pos, ReadInfo_Queue, value);
	/* label */
	consnil_heap(&value);
	SetReadInfo(pos, ReadInfo_Label, value);
	/* result */
	*ret = pos;
}

static void readinfo_symbol(addr *ret)
{
	GetConst(SYSTEM_READINFO_SPECIAL, ret);
}

void getreadinfo(Execute ptr, addr *ret)
{
	addr symbol;

	readinfo_symbol(&symbol);
	getspecial_local(ptr, symbol, ret);
	Check(*ret == Unbound, "unbound error");
	CheckType(*ret, LISPSYSTEM_READINFO);
}

struct readinfo_struct *getreadinfo_struct(Execute ptr)
{
	addr pos;
	getreadinfo(ptr, &pos);
	return ReadInfoStruct(pos);
}

void pushreadinfo(Execute ptr, addr *ret)
{
	addr symbol, info;

	readinfo_symbol(&symbol);
	readinfo_local(ptr->local, &info);
	pushspecial_control(ptr, symbol, info);
	*ret = info;
}

int pushreadinfo_recursive_(Execute ptr, addr *ret)
{
	unsigned preserving, replace;
	addr symbol, info, label;
	struct readinfo_struct *str;
	size_t backquote;

	/* outside read */
	readinfo_symbol(&symbol);
	getspecial_local(ptr, symbol, &info);
	if (info == Unbound)
		return fmte_("Outside read don't accept recursive-p parameter.", NULL);
	str = ReadInfoStruct(info);
	preserving = str->preserving;
	replace = str->replace;
	backquote = str->backquote;
	GetReadInfo(info, ReadInfo_Label, &label);

	/* push readinfo */
	readinfo_local(ptr->local, &info);
	str = ReadInfoStruct(info);
	str->preserving = preserving;
	str->replace = replace;
	str->backquote = backquote;
	str->recursive = 1;
	SetReadInfo(info, ReadInfo_Label, label);
	pushspecial_control(ptr, symbol, info);
	return Result(ret, info);
}

void getpackage_readinfo(Execute ptr, addr *ret)
{
	addr pos;
	getreadinfo(ptr, &pos);
	GetReadInfo(pos, ReadInfo_Package, ret);
}

void setpackage_readinfo(Execute ptr, addr value)
{
	addr pos;
	getreadinfo(ptr, &pos);
	SetReadInfo(pos, ReadInfo_Package, value);
}

void getqueue_readinfo(Execute ptr, addr *ret)
{
	addr pos;
	getreadinfo(ptr, &pos);
	GetReadInfo(pos, ReadInfo_Queue, ret);
}

unsigned getpreserving_readinfo(Execute ptr)
{
	addr pos;
	getreadinfo(ptr, &pos);
	return ReadInfoStruct(pos)->preserving;
}

unsigned getescape_readinfo(Execute ptr)
{
	addr pos;
	getreadinfo(ptr, &pos);
	return ReadInfoStruct(pos)->escape;
}

void setescape_readinfo(Execute ptr, unsigned value)
{
	addr pos;

	Check(1U < value, "value error");
	getreadinfo(ptr, &pos);
	ReadInfoStruct(pos)->escape = value;
}

unsigned getdot_readinfo(Execute ptr)
{
	addr pos;
	getreadinfo(ptr, &pos);
	return ReadInfoStruct(pos)->dot;
}

unsigned getreplace_readinfo(Execute ptr, addr *label)
{
	unsigned ret;
	addr pos;

	getreadinfo(ptr, &pos);
	ret = ReadInfoStruct(pos)->replace;
	if (ret) {
		GetReadInfo(pos, ReadInfo_Label, &pos);
		GetCar(pos, label);
	}

	return ret;
}

enum ReadInfo_State getstate_readinfo(Execute ptr)
{
	addr pos;
	getreadinfo(ptr, &pos);
	return ReadInfoStruct(pos)->state;
}

void setstate_readinfo(Execute ptr, enum ReadInfo_State value)
{
	addr pos;
	getreadinfo(ptr, &pos);
	ReadInfoStruct(pos)->state = value;
}

void clear_readinfo(Execute ptr)
{
	addr pos, value;
	struct readinfo_struct *str;

	getreadinfo(ptr, &pos);
	/* package */
	SetReadInfo(pos, ReadInfo_Package, Nil);
	/* queue */
	GetReadInfo(pos, ReadInfo_Queue, &value);
	clear_charqueue(value);
	/* state, escape */
	str = ReadInfoStruct(pos);
	str->escape = 0;
	str->state = ReadInfo_State_First;
}


/************************************************************
 *  reader_label.c
 ************************************************************/

/*
 *  readlabel
 */
enum ReadLabel_Index {
	ReadLabel_Label,
	ReadLabel_Value,
	ReadLabel_List,
	ReadLabel_Size
};

#define RefReadLabel	RefArrayA2
#define GetReadLabel	GetArrayA2
#define SetReadLabel	SetArrayA2


/*
 *  readlabel
 */
static void gensym_readlabel(addr pos)
{
	CheckType(pos, LISPSYSTEM_READLABEL);
	SetUser(pos, 1);
}

static void normal_readlabel(addr pos)
{
	CheckType(pos, LISPSYSTEM_READLABEL);
	SetUser(pos, 0);
}

static int gensymp_readlabel(addr pos)
{
	CheckType(pos, LISPSYSTEM_READLABEL);
	return GetUser(pos);
}

static int readlabel_heap_(Execute ptr, addr *ret, addr label)
{
	addr pos, gensym;

	Check(! integerp(label), "label error");
	heap_array2(&pos, LISPSYSTEM_READLABEL, ReadLabel_Size);
	gensym_readlabel(pos);
	Return(make_gensym_char_(ptr, "READ-LABEL", label, &gensym));
	SetReadLabel(pos, ReadLabel_Label, label);
	SetReadLabel(pos, ReadLabel_Value, gensym);

	return Result(ret, pos);
}

static int gensym_check_readlabel(addr label, addr check)
{
	addr value;

	if (gensymp_readlabel(label)) {
		GetReadLabel(label, ReadLabel_Value, &value);
		if (value == check)
			return 1;
	}

	return 0;
}

static void push_replace_readlabel(addr label, addr pos)
{
	addr list;

	GetReadLabel(label, ReadLabel_List, &list);
	cons_heap(&list, pos, list);
	SetReadLabel(label, ReadLabel_List, list);
}

static void queue_readlabel(Execute ptr, addr queue, addr pos)
{
	addr list, label, check;

	if (getreplace_readinfo(ptr, &list)) {
		while (list != Nil) {
			GetCons(list, &label, &list);
			if (gensym_check_readlabel(label, pos)) {
				tailqueue(queue, &check);
				push_replace_readlabel(label, check);
			}
		}
	}
}

void dotqueue_readlabel(Execute ptr, addr queue, addr pos)
{
	dotqueue(queue, pos);
	queue_readlabel(ptr, queue, pos);
}

void pushqueue_readlabel(Execute ptr, addr queue, addr pos)
{
	pushqueue_heap(queue, pos);
	queue_readlabel(ptr, queue, pos);
}

int find_readlabel(addr key, addr list, addr *ret)
{
	addr label, check;

	while (list != Nil) {
		GetCons(list, &label, &list);
		GetReadLabel(label, ReadLabel_Label, &check);
		if (eql_function(key, check)) {
			if (ret)
				GetReadLabel(label, ReadLabel_Value, ret);
			return 1;
		}
	}

	return 0;
}

int pushlabel_readinfo_(Execute ptr, addr value, addr *ret)
{
	addr cons, label, next;

	getreadinfo(ptr, &cons);
	GetReadInfo(cons, ReadInfo_Label, &cons);
	Check(! consp(cons), "type error");
	GetCar(cons, &next);
	if (find_readlabel(value, next, NULL))
		return fmte_("The #n= label ~S already exists.", value, NULL);
	Return(readlabel_heap_(ptr, &label, value));
	cons_heap(&next, label, next);
	SetCar(cons, next);

	return Result(ret, label);
}

static void replace_cons_readlabel(Execute ptr, addr pos, addr left, addr right)
{
	addr car, cdr;

	GetCons(pos, &car, &cdr);
	if (car == left)
		SetCar(pos, right);
	if (cdr == left)
		SetCdr(pos, right);
}

static void replace_vector_readlabel(Execute ptr, addr pos, addr left, addr right)
{
	addr check;
	size_t size, i;

	lenarray(pos, &size);
	for (i = 0; i < size; i++) {
		getarray(pos, i, &check);
		if (check == left)
			setarray(pos, i, right);
	}
}

static void replace_array_readlabel(Execute ptr, addr pos, addr left, addr right)
{
	addr check;
	struct array_struct *str;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	if (str->type != ARRAY_TYPE_T) return;
	/* general array */
	size = str->size;
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	for (i = 0; i < size; i++) {
		arraygen_get(pos, i, &check);
		if (check == left)
			arraygen_set(pos, i, right);
	}
}

static void replace_readlabel(Execute ptr, addr replace, addr left, addr right)
{
	switch (GetType(replace)) {
		case LISPTYPE_CONS:
			replace_cons_readlabel(ptr, replace, left, right);
			break;

		case LISPTYPE_VECTOR:
			replace_vector_readlabel(ptr, replace, left, right);
			break;

		case LISPTYPE_ARRAY:
			replace_array_readlabel(ptr, replace, left, right);
			break;

		default:
			break;
	}
}

int closelabel_readlabel_(Execute ptr, addr label, addr pos)
{
	addr gensym, list, replace;

	Check(! gensymp_readlabel(label), "gensymp error");
	/* error #n= #n# */
	GetReadLabel(label, ReadLabel_Value, &gensym);
	if (pos == gensym)
		return fmte_("The #n= value don't replace self value #n#.", NULL);
	/* replace */
	GetReadLabel(label, ReadLabel_List, &list);
	while (list != Nil) {
		GetCons(list, &replace, &list);
		replace_readlabel(ptr, replace, gensym, pos);
	}
	/* result */
	SetReadLabel(label, ReadLabel_List, Nil);
	SetReadLabel(label, ReadLabel_Value, pos);
	normal_readlabel(label);

	return 0;
}


/*
 *  vector
 */
static int vector_find_readlabel(addr key, addr vector)
{
	addr check;
	size_t size, i;

	lenarray(vector, &size);
	for (i = 0; i < size; i++) {
		getarray(vector, i, &check);
		if (key == check)
			return 1;
	}

	return 0;
}

void vector_readlabel(Execute ptr, addr pos)
{
	addr list, label, value;

	if (getreplace_readinfo(ptr, &list)) {
		while (list != Nil) {
			GetCons(list, &label, &list);
			if (gensymp_readlabel(label)) {
				GetReadLabel(label, ReadLabel_Value, &value);
				if (vector_find_readlabel(value, pos))
					push_replace_readlabel(label, pos);
			}
		}
	}
}


/*
 *  array
 */
static int array_find_readlabel_(addr key, addr array, int *ret)
{
	addr check;
	size_t size, i;

	array_get_rowlength(array, &size);
	for (i = 0; i < size; i++) {
		Return(array_get_t_(array, i, &check));
		if (key == check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int array_readlabel_(Execute ptr, addr pos)
{
	int check;
	addr list, label, value;

	if (getreplace_readinfo(ptr, &list)) {
		while (list != Nil) {
			GetCons(list, &label, &list);
			if (gensymp_readlabel(label)) {
				GetReadLabel(label, ReadLabel_Value, &value);
				Return(array_find_readlabel_(value, pos, &check));
				if (check)
					push_replace_readlabel(label, pos);
			}
		}
	}

	return 0;
}


/************************************************************
 *  reader_table.c
 ************************************************************/

/*
 *  access
 */
void getarray_readtable(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_READTABLE);
	GetArrayReadtable_Low(pos, ret);
}

void setarray_readtable(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_READTABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetArrayReadtable_Low(pos, value);
}

void gettable_readtable(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_READTABLE);
	GetTableReadtable_Low(pos, ret);
}

void settable_readtable(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_READTABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetTableReadtable_Low(pos, value);
}

void getdispatch_readtable(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_READTABLE);
	GetDispatchReadtable_Low(pos, ret);
}

void setdispatch_readtable(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_READTABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDispatchReadtable_Low(pos, value);
}

void *ptr_readtable(addr pos)
{
	CheckType(pos, LISPTYPE_READTABLE);
	return PtrReadtable_Low(pos);
}

enum ReadTable_Case *ptrcase_readtable(addr pos)
{
	CheckType(pos, LISPTYPE_READTABLE);
	return (enum ReadTable_Case *)PtrCaseReadtable_Low(pos);
}


/*
 *  object
 */
int readtable_heap_(addr *ret)
{
	addr pos, one;

	heap_smallsize(&pos, LISPTYPE_READTABLE,
			READTABLE_SIZE, sizeoft(enum ReadTable_Case));
	/* case */
	*PtrCaseReadtable(pos) = ReadTable_upcase;
	/* array */
	make_array_readtype(&one);
	SetArrayReadtable(pos, one);
	/* table */
	make_table_readtype(&one);
	SetTableReadtable(pos, one);
	/* dispatch */
	Return(make_dispatch_readtype_(&one));
	SetDispatchReadtable(pos, one);
	/* result */
	return Result(ret, pos);
}

static void copy_array_readtable(addr from, addr to)
{
	int i;
	addr one;

	for (i = 0; i < 0x80; i++) {
		GetArrayA2(from, i, &one);
		if (one != Nil)
			copy_readtype(&one, one);
		SetArrayA2(to, i, one);
	}
}

static int copy_table_readtable_(addr from, addr to)
{
	addr table, list, car, cdr, cell;
	size_t size, i;

	CheckType(from, LISPTYPE_HASHTABLE);
	CheckType(to, LISPTYPE_HASHTABLE);
	getsize_hashtable(from, &size);

	/* (maphash
	 *   (lambda (key value)
	 *     (setf (gethash key to) (copy-readtype value)))
	 *   from)
	 */
	GetTableHash(from, &table);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &cdr, &list);
			GetCons(cdr, &car, &cdr);
			copy_readtype(&cdr, cdr);
			Return(intern_hashheap_(to, car, &cell));
			SetCdr(cell, cdr);
		}
	}

	return 0;
}

static int copy_dispatch_readtable_(addr from, addr to)
{
	addr table, list, car, cdr, cell;
	size_t size, i;

	CheckType(from, LISPTYPE_HASHTABLE);
	CheckType(to, LISPTYPE_HASHTABLE);
	getsize_hashtable(from, &size);

	/* (maphash
	 *   (lambda (key value)
	 *     (setf (gethash key to) value))
	 *   copy)
	 */
	GetTableHash(from, &table);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &cdr, &list);
			GetCons(cdr, &car, &cdr);
			Return(intern_hashheap_(to, car, &cell));
			SetCdr(cell, cdr);
		}
	}

	return 0;
}

int copy_readtable_(addr from, addr to)
{
	addr a, b;

	/* case */
	*PtrCaseReadtable(to) = *PtrCaseReadtable(from);
	/* array */
	GetArrayReadtable(from, &a);
	GetArrayReadtable(to, &b);
	copy_array_readtable(a, b);
	/* table */
	GetTableReadtable(from, &a);
	GetTableReadtable(to, &b);
	clear_hashtable_heap(b);
	Return(copy_table_readtable_(a, b));
	/* dispatch */
	GetDispatchReadtable(from, &a);
	GetDispatchReadtable(to, &b);
	clear_hashtable_heap(b);
	return copy_dispatch_readtable_(a, b);
}

int copy_readtable_heap_(addr from, addr *ret)
{
	addr to, a, b;

	heap_smallsize(&to, LISPTYPE_READTABLE,
			READTABLE_SIZE, sizeoft(enum ReadTable_Case));
	/* case */
	*PtrCaseReadtable(to) = *PtrCaseReadtable(from);
	/* array */
	GetArrayReadtable(from, &a);
	array_readtype_heap(&b);
	copy_array_readtable(a, b);
	SetArrayReadtable(to, b);
	/* table */
	GetTableReadtable(from, &a);
	make_table_readtype(&b);
	Return(copy_table_readtable_(a, b));
	SetTableReadtable(to, b);
	/* dispatch */
	GetDispatchReadtable(from, &a);
	dispatch_readtype_heap(&b);
	Return(copy_dispatch_readtable_(a, b));
	SetDispatchReadtable(to, b);
	/* result */
	return Result(ret, to);
}

int copy_default_readtable_(addr pos)
{
	int i;
	addr one;

	/* case */
	*PtrCaseReadtable(pos) = ReadTable_upcase;
	/* array */
	GetArrayReadtable(pos, &one);
	for (i = 0; i < 0x80; i++)
		SetArrayA2(one, i, Nil);
	default_array_readtype(one);
	/* table */
	GetTableReadtable(pos, &one);
	clear_hashtable_heap(one);
	/* dispatch */
	GetDispatchReadtable(pos, &one);
	clear_hashtable_heap(one);
	return default_dispatch_readtype_(one, '#');
}

static int setreadtype_readtable_(addr pos, unicode code, addr type)
{
	addr key;

	if (code < 0x80) {
		GetArrayReadtable(pos, &pos);
		SetArrayA2(pos, code, type);
	}
	else {
		GetTableReadtable(pos, &pos);
		character_heap(&key, code);
		Return(intern_hashheap_(pos, key, &pos));
		SetCdr(pos, type);
	}

	return 0;
}

int make_dispatch_macro_character_(addr pos, addr character, int nonterm)
{
	unicode code;
	addr type, call;
	enum ReadTable_Type value;

	CheckType(pos, LISPTYPE_READTABLE);
	/* object */
	GetCharacter(character, &code);
	value = nonterm? ReadTable_Type_macro_nonterm: ReadTable_Type_macro_term;
	make_readtype(&type, value, code, 1);
	/* function */
	GetConst(SYSTEM_DISPATCH_FUNCTION, &call);
	GetFunctionSymbol(call, &call);
	Check(call == Unbound, "unbound error.");
	SetReadType(type, call);
	/* add readtable */
	return setreadtype_readtable_(pos, code, type);
}

#define DefaultDispatch(u,a,b) { \
	if (u == a) { \
		GetConst(SYSTEM_##b##_DISPATCH, &pos); \
		GetFunctionSymbol(pos, ret); \
		return; \
	} \
}
static void get_default_dispatch_sharp(addr code, addr *ret)
{
	addr pos;
	unicode u;

	GetCharacter(code, &u);
	u = toUpperUnicode(u);
	DefaultDispatch(u, 0x08, ERROR); /* backspace */
	DefaultDispatch(u, 0x09, ERROR); /* htab */
	DefaultDispatch(u, 0x0A, ERROR); /* newline */
	DefaultDispatch(u, 0x0C, ERROR); /* page */
	DefaultDispatch(u, 0x0D, ERROR); /* return */
	DefaultDispatch(u, 0x20, ERROR); /* space */
	DefaultDispatch(u, '=',  EQUAL);
	DefaultDispatch(u, '#',  SHARP);
	DefaultDispatch(u, '\'', SINGLE_QUOTE);
	DefaultDispatch(u, '(',  PARENSIS_OPEN);
	DefaultDispatch(u, ')',  PARENSIS_CLOSE);
	DefaultDispatch(u, '*',  ASTERISK);
	DefaultDispatch(u, ':',  COLON);
	DefaultDispatch(u, '<',  LESS);
	DefaultDispatch(u, '\\', BACKSLASH);
	DefaultDispatch(u, '|',  OR);
	DefaultDispatch(u, '+',  PLUS);
	DefaultDispatch(u, '-',  MINUS);
	DefaultDispatch(u, '.',  DOT);
	DefaultDispatch(u, 'A',  ARRAY);
	DefaultDispatch(u, 'B',  BINARY);
	DefaultDispatch(u, 'C',  COMPLEX);
	DefaultDispatch(u, 'O',  OCTAL);
	DefaultDispatch(u, 'P',  PATHNAME);
	DefaultDispatch(u, 'R',  RADIX);
	DefaultDispatch(u, 'S',  STRUCTURE);
	DefaultDispatch(u, 'X',  HEXADECIMAL);
	*ret = Nil;
}

int get_default_dispatch_macro_(addr code1, addr code2, addr *ret)
{
	unicode u;

	GetCharacter(code1, &u);
	if (u != '#')
		return fmte_("The character ~S is not dispatch macro.", code1, NULL);
	get_default_dispatch_sharp(code2, ret);

	return 0;
}


/*
 *  macro_character_execute
 */
static int readtype_unicode_readtable_(addr hash, unicode c, addr *ret)
{
	addr pos, value;

	/* find hashtable */
	Return(find_unicode_hashtable_(hash, c, &value));
	if (value != Unbound)
		return Result(ret, value);

	/* new object */
	character_heap(&pos, c);
	make_readtype(&value, ReadTable_Type_constituent, c, 0);
	Return(intern_hashheap_(hash, pos, &pos));
	SetCdr(pos, value);

	return Result(ret, value);
}

int readtype_readtable_(addr pos, unicode c, addr *ret)
{
	if (c < 0x80) {
		GetArrayReadtable(pos, &pos);
		GetArrayA2(pos, c, ret);
	}
	else {
		GetTableReadtable(pos, &pos);
		Return(readtype_unicode_readtable_(pos, c, ret));
	}

	return 0;
}

static int macro_character_call_call_(Execute ptr, LocalHold hold,
		int *result, addr *ret, addr call, addr stream, addr code)
{
	Return(funcall_control_(ptr, call, stream, code, NULL));
	if (lengthvalues_control(ptr) == 0) {
		*result = 0;
	}
	else {
		getresult_control(ptr, ret);
		localhold_set(hold, 0, *ret);
		*result = 1;
	}

	return 0;
}

static int macro_character_call_(Execute ptr, int *result, addr *ret,
		addr call, addr stream, addr code)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)macro_character_call_call_(ptr, hold, result, ret, call, stream, code);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

int macro_character_execute_(Execute ptr, int *result, addr *ret,
		unicode c, addr stream, addr table)
{
	addr call, code;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	character_heap(&code, c);
	localhold_push(hold, code);

	Return(readtype_readtable_(table, c, &call));
	localhold_push(hold, call);

	if (call == Nil)
		goto error;
	GetReadType(call, &call);
	localhold_push(hold, call);
	if (call == Nil)
		goto error;
	*result = 0;

	Return(macro_character_call_(ptr, result, ret, call, stream, code));
	localhold_end(hold);
	return 0;

error:
	*result = 0;
	return fmte_("Character ~S don't have a macro code.", code, NULL);
}

int get_dispatch_macro_character_(addr pos, unicode u1, unicode u2, addr *ret)
{
	addr check;

	CheckType(pos, LISPTYPE_READTABLE);
	/* dispatch check */
	Return(readtype_readtable_(pos, u1, &check));
	if (check == Nil)
		goto error;
	if (! dispatch_readtype(check))
		goto error;
	/* find */
	GetDispatchReadtable(pos, &check);
	u2 = toUpperUnicode(u2);
	return findnil_character2_hashtable_(check, u1, u2, ret);

error:
	character_heap(&check, u1);
	return fmte_("The character ~S is not dispatch macro.", check, NULL);
}

int rem_dispatch_macro_character_(addr pos, unicode u1, unicode u2)
{
	int check;
	addr value, key;

	CheckType(pos, LISPTYPE_READTABLE);
	/* dispatch check */
	Return(readtype_readtable_(pos, u1, &value));
	if (value == Nil)
		goto error;
	if (! dispatch_readtype(value))
		goto error;
	/* delete */
	GetDispatchReadtable(pos, &value);
	u2 = toUpperUnicode(u2);
	Return(findcons_character2_hashtable_(value, u1, u2, &key));
	if (key != Nil) {
		GetCar(key, &key);
		Return(delete_hashtable_(value, key, &check));
	}
	return 0;

error:
	character_heap(&value, u1);
	return fmte_("The character ~S is not dispatch macro.", value, NULL);
}

int set_dispatch_macro_character_(addr pos, unicode u1, unicode u2, addr call)
{
	addr check, cons;

	CheckType(pos, LISPTYPE_READTABLE);
	/* dispatch check */
	Return(readtype_readtable_(pos, u1, &check));
	if (check == Nil)
		goto error;
	if (! dispatch_readtype(check))
		goto error;
	/* intern */
	GetDispatchReadtable(pos, &check);
	u2 = toUpperUnicode(u2);
	Return(findcons_character2_hashtable_(check, u1, u2, &cons));
	if (cons != Nil) {
		SetCdr(cons, call);
	}
	else {
		character2_heap(&cons, u1, u2);
		Return(intern_hashheap_(check, cons, &cons));
		SetCdr(cons, call);
	}
	return 0;

error:
	character_heap(&check, u1);
	return fmte_("The character ~S is not dispatch macro.", check, NULL);
}

#define DefaultTermMacro(u,a,b) { \
	if (u == a) { \
		GetConst(SYSTEM_##b##_READER, &pos); \
		GetFunctionSymbol(pos, ret); \
		*nonterm = 0; \
		return; \
	} \
}
#define DefaultNonTermMacro(u,a,b) { \
	if (u == a) { \
		GetConst(SYSTEM_##b##_READER, &pos); \
		GetFunctionSymbol(pos, ret); \
		*nonterm = 1; \
		return; \
	} \
}
void get_default_macro_character(unicode u, addr *ret, int *nonterm)
{
	addr pos;

	DefaultTermMacro(u, '"',  DOUBLE_QUOTE);
	DefaultTermMacro(u, '\'', SINGLE_QUOTE);
	DefaultTermMacro(u, '(',  PARENSIS_OPEN);
	DefaultTermMacro(u, ')',  PARENSIS_CLOSE);
	DefaultTermMacro(u, ';',  SEMICOLON);
	DefaultTermMacro(u, '`',  BACKQUOTE);
	DefaultTermMacro(u, ',',  COMMA);
	DefaultNonTermMacro(u, '#', SHARP);
	*ret = Nil;
	*nonterm = 0;
}

int get_macro_character_(addr pos, unicode u, addr *ret, int *nonterm)
{
	addr type;
	struct readtype_struct *str;

	CheckType(pos, LISPTYPE_READTABLE);
	Return(readtype_readtable_(pos, u, &type));
	if (type == Nil) {
		*ret = Nil;
		*nonterm = 0;
		return 0;
	}

	str = ReadTypeStruct(type);
	if (str->type == ReadTable_Type_macro_term) {
		GetReadType(type, ret);
		*nonterm = 0;
	}
	else if (str->type == ReadTable_Type_macro_nonterm) {
		GetReadType(type, ret);
		*nonterm = 1;
	}
	else {
		*ret = Nil;
		*nonterm = 0;
	}

	return 0;
}

int set_macro_character_(addr pos, unicode u, int nonterm, addr call)
{
	addr type;
	enum ReadTable_Type value;

	CheckType(pos, LISPTYPE_READTABLE);
	value = nonterm? ReadTable_Type_macro_nonterm: ReadTable_Type_macro_term;
	make_readtype(&type, value, u, 0);
	SetReadType(type, call);
	/* add readtable */
	return setreadtype_readtable_(pos, u, type);
}

static int setreadtype_default_(addr pos,
		unicode u, enum ReadTable_Type type, addr call)
{
	addr one;

	make_readtype(&one, type, u, 0);
	SetReadType(one, call);
	return setreadtype_readtable_(pos, u, one);
}

static int setdispatch_default_(addr pos, unicode u, addr call)
{
	addr one;

	make_readtype(&one, ReadTable_Type_macro_nonterm, u, 1);
	SetReadType(one, call);
	Return(setreadtype_readtable_(pos, u, one));

	GetDispatchReadtable(pos, &one);
	return default_dispatch_readtype_(one, u);
}

int set_syntax_from_default_(unicode u1, unicode u2, addr to)
{
	addr pos;

	Return(delete_readtype_(to, u1));
	if (readtype_whitespace(u2))
		return setreadtype_default_(to, u1, ReadTable_Type_whitespace, Nil);
	if (readtype_constituent(u2))
		return setreadtype_default_(to, u1, ReadTable_Type_constituent, Nil);
	if (u2 == '\\')
		return setreadtype_default_(to, u1, ReadTable_Type_escape_single, Nil);
	if (u2 == '|')
		return setreadtype_default_(to, u1, ReadTable_Type_escape_multiple, Nil);
	if (readtype_termmacro(u2, &pos))
		return setreadtype_default_(to, u1, ReadTable_Type_macro_term, pos);
	if (readtype_sharpmacro(u2, &pos))
		return setdispatch_default_(to, u1, pos);

	/* delete only */
	return 0;
}

static int copy_dispatch_macro_(unicode u1, unicode u2, addr to, addr from)
{
	addr table, list, car, cdr;
	size_t size, i;

	CheckType(from, LISPTYPE_HASHTABLE);
	CheckType(to, LISPTYPE_HASHTABLE);
	getsize_hashtable(from, &size);

	/* (maphash ...) */
	GetTableHash(from, &table);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &cdr, &list);
			GetCons(cdr, &car, &cdr);
			if (refcharacter2a(car) == u2) {
				character2_heap(&car, u1, refcharacter2b(car));
				Return(intern_hashheap_(to, car, &car));
				SetCdr(car, cdr);
			}
		}
	}

	return 0;
}

int set_syntax_from_char_(unicode u1, unicode u2, addr to, addr from)
{
	addr one, type, call;
	struct readtype_struct *str;

	Return(delete_readtype_(to, u1));
	Return(readtype_readtable_(from, u2, &type));
	if (type != Nil) {
		str = ReadTypeStruct(type);
		GetReadType(type, &call);
		/* set-readtype */
		make_readtype(&one, str->type, u1, str->dispatch);
		Return(setreadtype_readtable_(to, u1, one));
		SetReadType(one, call);
		/* set-dispatch */
		if (str->dispatch) {
			GetDispatchReadtable(to, &to);
			GetDispatchReadtable(from, &from);
			Return(copy_dispatch_macro_(u1, u2, to, from));
		}
	}

	return 0;
}

int float_readtable_(Execute ptr, enum ReadTable_float *ret)
{
	addr pos, check;

	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	GetConst(COMMON_SINGLE_FLOAT, &check);
	if (check == pos)
		return Result(ret, ReadTable_single);
	GetConst(COMMON_DOUBLE_FLOAT, &check);
	if (check == pos)
		return Result(ret, ReadTable_double);
	GetConst(COMMON_LONG_FLOAT, &check);
	if (check == pos)
		return Result(ret, ReadTable_long);
	GetConst(COMMON_SHORT_FLOAT, &check);
	if (check == pos)
		return Result(ret, ReadTable_short);

	*ret = ReadTable_single;
	return fmte_("Invalid *read-default-float-format* value ~S.", pos, NULL);
}

int readcase_readtable_(Execute ptr, enum ReadTable_Case *ret)
{
	addr pos;
	Return(getreadtable_(ptr, &pos));
	return Result(ret, getcase_readtable(pos));
}

enum ReadTable_Case getcase_readtable(addr pos)
{
	Check(GetType(pos) != LISPTYPE_READTABLE, "type error");
	return *PtrCaseReadtable(pos);
}

void setcase_readtable(addr pos, enum ReadTable_Case mode)
{
	Check(GetType(pos) != LISPTYPE_READTABLE, "type error");
	*PtrCaseReadtable(pos) = mode;
}

int getreadtable_(Execute ptr, addr *ret)
{
	addr pos;

	GetConst(SPECIAL_READTABLE, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	if (GetType(pos) != LISPTYPE_READTABLE) {
		*ret = Nil;
		return TypeError_(pos, READTABLE);
	}

	return Result(ret, pos);
}


/************************************************************
 *  reader_token.c
 ************************************************************/

/*
 *  chartable
 */
struct chartable {
	unsigned chartype : 1;
	unsigned exponent : 1;
};
static struct chartable Reader_CharTable[0x80];

void init_reader_token(void)
{
	static const char *const str1 =
		"!\"#$%&'(),;<=>?[\\]^_`{|}~.+-*/@"
		"0123456789"
		"abcdefghijklmnopqrstuvwxyz"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	static const char *const str2 = "defslDEFSL";
	const char *str;

	/* Illegal */
	cleartype(Reader_CharTable);
	/* char */
	for (str = str1; *str; str++)
		Reader_CharTable[(int)*str].chartype = 1;
	/* exponent */
	for (str = str2; *str; str++)
		Reader_CharTable[(int)*str].exponent = 1;
}


/*
 *  tokentype
 */
static int checktable_char(unicode c)
{
	return (c < 0x80) && (Reader_CharTable[c].chartype);
}

static int checktable_base(unsigned base, unicode c)
{
	return ! getvalue_digit(base, c, &base);
}

static int checktable_sign(unicode c)
{
	return (c == '+') || (c == '-');
}

static int checktable_exponent(unicode c)
{
	return (c < 0x80) && (Reader_CharTable[c].exponent);
}

static int checktable_potential(unsigned base, unicode c)
{
	return checktable_base(base, c)
		|| c == '+' || c == '-' || c == '/'
		|| c == '.' || c == '^' || c == '_'
		|| checktable_exponent(c);
}

static int checktable_firstpotential(unsigned base, unicode c)
{
	return checktable_base(base, c)
		|| c == '+' || c == '-' || c == '.' || c == '^' || c == '_';
}

static int checktable_isdigit(unicode c)
{
	return (c < 0x80) && isDigitCase(c);
}

static int checktable_isalpha(unicode c)
{
	return (c < 0x80) && isAlphabetic(c);
}

static int checktable_force(unicode c)
{
	return (0x80 <= c);
}

/* for debug */
/* #define OUTPUTSTATE(x, c)  printf("[%s:%c]\n", x, c) */
#define OUTPUTSTATE(x, c)  /*do nothing*/

#define ReadtableNext() { \
	getchar_charqueue(queue, i++, &c); \
	if (c && !checktable_char(c)) { \
		goto error; \
	} \
	if (!pot) { \
		pot = checktable_base(base, c); \
	} \
}

#define checkbasegoto(base, digit, c, label) { \
	if (checktable_base(base, c)) { \
		if (digit) { \
			digit = checktable_isdigit(c); \
		} \
		goto label; \
	} \
}

enum TokenType tokentype(unsigned base, addr queue)
{
	unicode c;
	size_t i;
	int first, pot, digit;

	/* init */
	i = 0;
	pot = 0;
	digit = 1;

	/* first */
	ReadtableNext();
	first = checktable_firstpotential(base, c);
	OUTPUTSTATE("first", c);
	if (c == 0)
		goto error;
	if (checktable_sign(c))
		goto digit1;
	checkbasegoto(base, digit, c, digit2);
	if (checktable_isdigit(c))
		goto float1;
	if (c == '.')
		goto dot1;
	goto symbol;

	/* dot check */
dot1:
	ReadtableNext();
	OUTPUTSTATE("dot1", c);
	if (c == 0)
		goto token_dot;
	if (checktable_isdigit(c))
		goto float6;
	if (c == '.')
		goto dot2;
	goto symbol;

dot2:
	ReadtableNext();
	OUTPUTSTATE("dot2", c);
	if (c == '.')
		goto dot2;
	if (c == 0)
		goto error;
	goto symbol;

	/* integer */
digit1:
	ReadtableNext();
	OUTPUTSTATE("digit1", c);
	checkbasegoto(base, digit, c, digit2);
	if (checktable_isdigit(c))
		goto float1;
	if (c == '.')
		goto float5;
	goto symbol_sign;

digit2:
	ReadtableNext();
	OUTPUTSTATE("digit2", c);
	if (c == 0)
		goto token_digit;
	if (c == '.')
		goto digit3;
	if (c == '/')
		goto ratio1;
	checkbasegoto(base, digit, c, digit2);
	if (checktable_isdigit(c))
		goto float1;
	if (checktable_exponent(c))
		goto exponent1;
	goto symbol;

digit3:
	ReadtableNext();
	OUTPUTSTATE("digit3", c);
	if (c == 0)
		goto token_decimal;
	if (checktable_isdigit(c))
		goto float4;
	if (checktable_exponent(c))
		goto exponent3;
	goto symbol;

	/* ratio */
ratio1:
	ReadtableNext();
	OUTPUTSTATE("ratio1", c);
	if (checktable_base(base, c))
		goto ratio2;
	goto symbol;

ratio2:
	ReadtableNext();
	OUTPUTSTATE("ratio2", c);
	if (c == 0)
		goto token_ratio;
	if (checktable_base(base, c))
		goto ratio2;
	goto symbol;

	/* float */
float1:
	ReadtableNext();
	OUTPUTSTATE("float1", c);
	if (checktable_isdigit(c))
		goto float1;
	if (checktable_exponent(c))
		goto exponent1;
	if (c == '.')
		goto float3;
	goto symbol;

float3:
	ReadtableNext();
	OUTPUTSTATE("float3", c);
	if (checktable_exponent(c))
		goto exponent1;
	if (checktable_isdigit(c))
		goto float4;
	if (c == 0)
		goto token_decimal;
	goto symbol;

float4:
	ReadtableNext();
	OUTPUTSTATE("float4", c);
	if (c == 0)
		goto check_float;
	if (checktable_exponent(c))
		goto exponent1;
	if (checktable_isdigit(c))
		goto float4;
	goto symbol;

float5:
	ReadtableNext();
	OUTPUTSTATE("float5", c);
	if (checktable_isdigit(c))
		goto float6;
	goto symbol;

float6:
	ReadtableNext();
	OUTPUTSTATE("float6", c);
	if (c == 0)
		goto check_float;
	if (checktable_isdigit(c))
		goto float6;
	if (checktable_exponent(c))
		goto exponent1;
	goto symbol;

check_float:
	if (digit)
		goto token_float;
	goto token_potential;

	/* exponent */
exponent1:
	ReadtableNext();
	OUTPUTSTATE("exponent1", c);
	if (checktable_sign(c))
		goto exponent2;
	if (checktable_isdigit(c))
		goto exponent3;
	goto symbol_exponent;

exponent2:
	ReadtableNext();
	OUTPUTSTATE("exponent2", c);
	if (checktable_isdigit(c))
		goto exponent3;
	goto symbol_sign;

exponent3:
	ReadtableNext();
	OUTPUTSTATE("exponent3", c);
	if (c == 0)
		goto check_float;
	if (checktable_isdigit(c))
		goto exponent3;
	goto symbol;

	/* symbol */
symbol:
	OUTPUTSTATE("symbol", c);
	if (first == 0)
		goto token_symbol;
	goto potential_symbol;

potential:
	ReadtableNext();
	OUTPUTSTATE("potential", c);
potential_symbol:
	if (c == 0)
		goto token_potential;
	if (checktable_sign(c))
		goto potential_sign;
	if (checktable_base(base, c))
		goto potential;
	if (checktable_isalpha(c))
		goto potential_marker;
	if (checktable_potential(base, c))
		goto potential;
	goto token_symbol;

	/* symbol-sign */
symbol_sign:
	OUTPUTSTATE("symbol_sign", c);
	if (first == 0)
		goto token_symbol;
	goto potential_sign_symbol;

potential_sign:
	ReadtableNext();
	OUTPUTSTATE("potential", c);
potential_sign_symbol:
	if (c == 0)
		goto token_symbol;
	if (checktable_sign(c))
		goto potential_sign;
	if (checktable_base(base, c))
		goto potential;
	if (checktable_isalpha(c))
		goto potential_marker;
	if (checktable_potential(base, c))
		goto potential;
	goto token_symbol;

	/* symbol-marker */
symbol_exponent:
	OUTPUTSTATE("symbol_exponent", c);
	if (first == 0)
		goto token_symbol;
	goto potential_marker_symbol;

potential_marker:
	ReadtableNext();
	OUTPUTSTATE("potential_marker", c);
potential_marker_symbol:
	if (c == 0)
		goto token_potential;
	if (checktable_base(base, c))
		goto potential;
	if (checktable_isalpha(c))
		goto token_symbol;
	if (checktable_sign(c))
		goto potential_sign;
	if (checktable_potential(base, c))
		goto potential;
	goto token_symbol;

	/* token */
token_digit:
	OUTPUTSTATE("token_digit", '-');
	return TokenType_integer;

token_decimal:
	OUTPUTSTATE("token_decimal", '-');
	return TokenType_decimal;

token_ratio:
	OUTPUTSTATE("token_ratio", '-');
	return TokenType_ratio;

token_float:
	OUTPUTSTATE("token_float", '-');
	return TokenType_float;

token_dot:
	OUTPUTSTATE("token_dot", '-');
	return TokenType_dot;

token_symbol:
	OUTPUTSTATE("token_symbol", '-');
	return TokenType_symbol;

token_potential:
	OUTPUTSTATE("token_potential", '-');
	return pot? TokenType_potential: TokenType_symbol;

error:
	if (checktable_force(c))
		goto force;
	OUTPUTSTATE("error", '-');
	return TokenType_error;

force:
	OUTPUTSTATE("force_unicode", '-');
	return TokenType_symbol;
}


/*
 *  maketoken
 */
int getreadbase_(Execute ptr, unsigned *ret)
{
	addr one;
	fixnum value;

	GetConst(SPECIAL_READ_BASE, &one);
	Return(getspecialcheck_local_(ptr, one, &one));
	Check(GetType(one) != LISPTYPE_FIXNUM, "type error");
	GetFixnum(one, &value);
	if (! isBaseChar(value)) {
		*ret = 0;
		fixnum_heap(&one, (fixnum)value);
		return fmte_("base ~a must be a number between 2 and 36.", one, NULL);
	}

	return Result(ret, (unsigned)value);
}

static int maketoken_intern_(Execute ptr,
		addr package, addr name, int unexport, addr *ret)
{
	int check;
	addr value;

	/* keyword */
	if (package == T) {
		GetConst(PACKAGE_KEYWORD, &package);
		return intern_package_(package, name, ret, NULL);
	}

	/* package::name */
	if (unexport)
		return intern_package_(package, name, ret, NULL);

	/* package:name, unexport */
	Return(exportp_name_package_(package, name, &value, &check));
	if (check)
		return Result(ret, value);

	/* package:name, export, OK */
	if (value != Unbound) {
		*ret = Nil;
		return fmte_("The symbol ~S is not exported in ~S.", name, package, NULL);
	}

	/* package:name, unbound */
	Return(getpackage_(ptr, &value));
	if (package == value)
		return intern_package_(package, name, ret, NULL);

	/* package:name, error */
	*ret = Nil;
	return fmte_("Cannot intern the symbol ~S in ~S.", name, package, NULL);
}

static int maketoken_package_(Execute ptr, addr *ret, addr queue, addr package)
{
	enum TokenType token;
	struct readinfo_struct *str;
	unsigned base;
	addr name;

	str = getreadinfo_struct(ptr);
	if (str->escape) {
		/* escapemonde, make force symbol */
		make_charqueue_heap(queue, &name);
		/* intern package - name */
		return maketoken_intern_(ptr, package, name, str->unexport, ret);
	}

	Return(getreadbase_(ptr, &base));
	token = tokentype(base, queue);
	if (token == TokenType_symbol || token == TokenType_potential) {
		make_charqueue_heap(queue, &name);
		/* intern package - symbol */
		return maketoken_intern_(ptr, package, name, str->unexport, ret);
	}
	else {
		*ret = Nil;
		return fmte_("Token-type error", NULL);
	}
}

static int maketoken_normal_(Execute ptr, addr *ret)
{
	unsigned base;
	addr package, queue, name;

	/* table have package */
	getpackage_readinfo(ptr, &package);
	getqueue_readinfo(ptr, &queue);
	if (package != Nil)
		return maketoken_package_(ptr, ret, queue, package);

	/* no package */
	if (getescape_readinfo(ptr)) {
		/* escapemode, make force symbol */
		make_charqueue_heap(queue, &name);
		/* intern name */
		return intern_default_package_(ptr, name, ret, NULL);
	}

	Return(getreadbase_(ptr, &base));
	switch (tokentype(base, queue)) {
		case TokenType_symbol:
		case TokenType_potential:
			make_charqueue_heap(queue, &name);
			/* intern *package* - symbol */
			Return(intern_default_package_(ptr, name, ret, NULL));
			break;

		case TokenType_decimal:
			maketoken_integer(ptr->local, queue, 10, ret);
			break;

		case TokenType_integer:
			maketoken_integer(ptr->local, queue, base, ret);
			break;

		case TokenType_float:
			return maketoken_float_(ptr, queue, ret);

		case TokenType_ratio:
			maketoken_ratio(ptr->local, queue, base, ret);
			break;

		case TokenType_dot:
			if (getdot_readinfo(ptr) == 0) {
				*ret = Nil;
				return fmte_("dot no allowed here.", NULL);
			}
			GetConst(SYSTEM_READTABLE_DOT, ret);
			break;

		case TokenType_error:
		default:
			*ret = Nil;
			return fmte_("token error", NULL);
	}

	return 0;
}

static int maketoken_gensym_(Execute ptr, addr *ret)
{
	unsigned base;
	addr queue, symbol, name;

	/* no package */
	getqueue_readinfo(ptr, &queue);
	if (getescape_readinfo(ptr)) {
		/* escapemode, make force symbol */
		make_charqueue_heap(queue, &name);
		symbol_heap(&symbol);
		SetNameSymbol(symbol, name);
		return Result(ret, symbol);
	}

	Return(getreadbase_(ptr, &base));
	switch (tokentype(base, queue)) {
		case TokenType_symbol:
		case TokenType_potential:
			make_charqueue_heap(queue, &name);
			symbol_heap(&symbol);
			SetNameSymbol(symbol, name);
			return Result(ret, symbol);

		default:
			*ret = Nil;
			return fmte_("token error (gensym)", NULL);
	}
}

int read_suppress_p_(Execute ptr, int *ret)
{
	addr pos;

	/* *read-suppress* */
	GetConst(SPECIAL_READ_SUPPRESS, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	return Result(ret, pos != Nil);
}

int maketoken_(Execute ptr, addr *ret)
{
	int check;

	/* *read-suppress* */
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);

	/* make token */
	if (getstate_readinfo(ptr) != ReadInfo_State_Gensym)
		return maketoken_normal_(ptr, ret);
	else
		return maketoken_gensym_(ptr, ret);
}


/************************************************************
 *  reader_type.c
 ************************************************************/

/*
 *  access
 */
void *ptr_readtype(addr pos)
{
	CheckType(pos, LISPSYSTEM_READTYPE);
	return PtrReadType_Low(pos);
}

struct readtype_struct *struct_readtype(addr pos)
{
	CheckType(pos, LISPSYSTEM_READTYPE);
	return ReadTypeStruct_Low(pos);
}

void get_readtype(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_READTYPE);
	GetReadType_Low(pos, ret);
}

void set_readtype(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_READTYPE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetReadType_Low(pos, value);
}


/*
 *  readtype
 */
int dispatch_readtype(addr pos)
{
	CheckType(pos, LISPSYSTEM_READTYPE);
	return ReadTypeStruct(pos)->dispatch;
}

void make_readtype(addr *ret,
		enum ReadTable_Type type, unicode code, unsigned dispatch)
{
	addr pos;
	struct readtype_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_READTYPE, 1, sizeoft(struct readtype_struct));
	str = ReadTypeStruct(pos);
	str->type = type;
	str->code = code;
	str->dispatch = dispatch;
	*ret = pos;
}

void copy_readtype(addr *ret, addr copy)
{
	addr pos;
	struct readtype_struct *str;

	str = ReadTypeStruct(copy);
	make_readtype(&pos, str->type, str->code, str->dispatch);
	GetReadType(copy, &copy);
	SetReadType(pos, copy);
	*ret = pos;
}

static void array_readtype(addr array, enum ReadTable_Type type, unicode code)
{
	addr pos;
	make_readtype(&pos, type, code, 0);
	SetArrayA2(array, (size_t)code, pos);
}

static void macro_readtype(addr array,
		enum ReadTable_Type type,
		unicode code,
		unsigned dispatch,
		constindex index)
{
	addr pos, symbol;

	make_readtype(&pos, type, code, dispatch);
	GetConstant(index, &symbol);
	GetFunctionSymbol(symbol, &symbol);
	SetReadType(pos, symbol);
	SetArrayA2(array, (size_t)code, pos);
}

#define TermReadType(a,b,c) \
	macro_readtype(a,ReadTable_Type_macro_term,b,0,CONSTANT_SYSTEM_##c##_READER)
#define DispatchReadType(a,b,c) \
	macro_readtype(a,ReadTable_Type_macro_nonterm,b,1,CONSTANT_SYSTEM_##c##_READER)

static void default_macro_readtype(addr array)
{
	TermReadType(array, '"',  DOUBLE_QUOTE);
	TermReadType(array, '\'', SINGLE_QUOTE);
	TermReadType(array, '(',  PARENSIS_OPEN);
	TermReadType(array, ')',  PARENSIS_CLOSE);
	TermReadType(array, ';',  SEMICOLON);
	TermReadType(array, '`',  BACKQUOTE);
	TermReadType(array, ',',  COMMA);
	DispatchReadType(array, '#', SHARP);
}

static const char *const Default_WhiteSpace =
"\x09"      /* Horizontal Tab */
"\x20"      /* Space */
"\x0C"      /* Page */
"\x0A"      /* Linefeed, Newline */
"\x0D";     /* Return */

static const char *const Default_Constituent =
"0123456789"
"abcdefghijklmnopqrstuvwxyz"
"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
"!$%&*+-./:<=>?@[]^_{}~"
"\x08"  /* Backspace */
"\x7F"; /* Delete (Rubout) */

void default_array_readtype(addr array)
{
	const char *str;

	/* whitespace */
	for (str = Default_WhiteSpace; *str; str++)
		array_readtype(array, ReadTable_Type_whitespace, (unicode)*str);
	/* constituent */
	for (str = Default_Constituent; *str; str++)
		array_readtype(array, ReadTable_Type_constituent, (unicode)*str);
	/* escape */
	array_readtype(array, ReadTable_Type_escape_single, '\\');
	array_readtype(array, ReadTable_Type_escape_multiple, '|');
	/* macro */
	default_macro_readtype(array);
}

static int dispatch_character_(addr pos, unicode a, unicode b, constindex index)
{
	addr key, value, cons;

	/* hash */
	Check(isLowerCase(b), "case error");
	character2_heap(&key, a, b);
	Return(intern_hashheap_(pos, key, &cons));
	/* value */
	GetConstant(index, &value);
	GetFunctionSymbol(value, &value);
	Check(value == Unbound, "unbound error");
	/* set */
	SetCdr(cons, value);

	return 0;
}

#define DispatchCharacter(a,u,b,c) \
	/**/ Return(dispatch_character_(a,u,b,CONSTANT_SYSTEM_##c##_DISPATCH))

int default_dispatch_readtype_(addr pos, unicode u)
{
	DispatchCharacter(pos, u, 0x08, ERROR); /* backspace */
	DispatchCharacter(pos, u, 0x09, ERROR); /* htab */
	DispatchCharacter(pos, u, 0x0A, ERROR); /* newline */
	DispatchCharacter(pos, u, 0x0C, ERROR); /* page */
	DispatchCharacter(pos, u, 0x0D, ERROR); /* return */
	DispatchCharacter(pos, u, 0x20, ERROR); /* space */
	DispatchCharacter(pos, u, '=',  EQUAL);
	DispatchCharacter(pos, u, '#',  SHARP);
	DispatchCharacter(pos, u, '\'', SINGLE_QUOTE);
	DispatchCharacter(pos, u, '(',  PARENSIS_OPEN);
	DispatchCharacter(pos, u, ')',  PARENSIS_CLOSE);
	DispatchCharacter(pos, u, '*',  ASTERISK);
	DispatchCharacter(pos, u, ':',  COLON);
	DispatchCharacter(pos, u, '<',  LESS);
	DispatchCharacter(pos, u, '\\', BACKSLASH);
	DispatchCharacter(pos, u, '|',  OR);
	DispatchCharacter(pos, u, '+',  PLUS);
	DispatchCharacter(pos, u, '-',  MINUS);
	DispatchCharacter(pos, u, '.',  DOT);
	DispatchCharacter(pos, u, 'A',  ARRAY);
	DispatchCharacter(pos, u, 'B',  BINARY);
	DispatchCharacter(pos, u, 'C',  COMPLEX);
	DispatchCharacter(pos, u, 'O',  OCTAL);
	DispatchCharacter(pos, u, 'P',  PATHNAME);
	DispatchCharacter(pos, u, 'R',  RADIX);
	DispatchCharacter(pos, u, 'S',  STRUCTURE);
	DispatchCharacter(pos, u, 'X',  HEXADECIMAL);

	return 0;
}

void array_readtype_heap(addr *ret)
{
	vector2_heap(ret, 0x80);
}

void dispatch_readtype_heap(addr *ret)
{
	addr pos;
	hashtable_heap(&pos);
	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	*ret = pos;
}

void make_array_readtype(addr *ret)
{
	addr pos;
	array_readtype_heap(&pos);
	default_array_readtype(pos);
	*ret = pos;
}

void make_table_readtype(addr *ret)
{
	addr pos;
	hashtable_heap(&pos);
	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	*ret = pos;
}

int make_dispatch_readtype_(addr *ret)
{
	addr pos;
	dispatch_readtype_heap(&pos);
	Return(default_dispatch_readtype_(pos, '#'));
	return Result(ret, pos);
}

int readtype_whitespace(unicode u)
{
	if (0x80 <= u) return 0;
	return strchr(Default_WhiteSpace, (int)u) != NULL;
}

int readtype_constituent(unicode u)
{
	if (0x80 <= u) return 0;
	return strchr(Default_Constituent, (int)u) != NULL;
}

#define ReadTypeTermMacro(u,a,b) { \
	if (u == a) { \
		GetConst(SYSTEM_##b##_READER, &pos); \
		GetFunctionSymbol(pos, ret); \
		return 1; \
	} \
}
int readtype_termmacro(unicode u, addr *ret)
{
	addr pos;

	ReadTypeTermMacro(u, '"',  DOUBLE_QUOTE);
	ReadTypeTermMacro(u, '\'', SINGLE_QUOTE);
	ReadTypeTermMacro(u, '(',  PARENSIS_OPEN);
	ReadTypeTermMacro(u, ')',  PARENSIS_CLOSE);
	ReadTypeTermMacro(u, ';',  SEMICOLON);
	ReadTypeTermMacro(u, '`',  BACKQUOTE);
	ReadTypeTermMacro(u, ',',  COMMA);

	return 0;
}

int readtype_sharpmacro(unicode u, addr *ret)
{
	addr pos;

	if (u == '#') {
		GetConst(SYSTEM_SHARP_READER, &pos);
		GetFunctionSymbol(pos, ret);
		return 1;
	}

	return 0;
}

static int delete_dispatch_macro_(addr pos, unicode u)
{
	int check;
	addr table, list, car, cdr;
	size_t size, i;

	CheckType(pos, LISPTYPE_HASHTABLE);
	getsize_hashtable(pos, &size);

	/* (maphash
	 *   (lambda (key value)
	 *     (if (eq (car key) u)
	 *       (remhash key pos)))
	 *   pos)
	 */
	GetTableHash(pos, &table);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &cdr, &list);
			GetCar(cdr, &car);
			if (refcharacter2a(car) == u) {
				Return(delete_hashtable_(pos, car, &check));
			}
		}
	}

	return 0;
}

int delete_readtype_(addr pos, unicode c)
{
	int check;
	addr value;

	Return(readtype_readtable_(pos, c, &value));
	if (value == Nil)
		return 0;
	if (dispatch_readtype(value)) {
		GetDispatchReadtable(pos, &value);
		Return(delete_dispatch_macro_(value, c));
	}
	/* delete */
	if (c < 0x80) {
		GetArrayReadtable(pos, &pos);
		SetArrayA2(pos, c, Nil);
	}
	else {
		GetTableReadtable(pos, &pos);
		Return(findcons_unicode_hashtable_(pos, c, &value));
		if (value != Nil) {
			GetCar(value, &value);
			Return(delete_hashtable_(pos, value, &check));
		}
	}

	return 0;
}


/************************************************************
 *  real.c
 ************************************************************/

int floatp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_SINGLE_FLOAT
		|| type == LISPTYPE_DOUBLE_FLOAT
		|| type == LISPTYPE_LONG_FLOAT
		|| type == LISPTYPE_SHORT_FLOAT;
}

int realp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_FIXNUM
		|| type == LISPTYPE_BIGNUM
		|| type == LISPTYPE_RATIO
		|| type == LISPTYPE_SINGLE_FLOAT
		|| type == LISPTYPE_DOUBLE_FLOAT
		|| type == LISPTYPE_LONG_FLOAT
		|| type == LISPTYPE_SHORT_FLOAT;
}

int real_result_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (floatp(pos))
		return float_result_local_(local, pos, ret);
	else
		return rational_result_local_(local, pos, ret);
}

int real_result_heap_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (floatp(pos))
		return float_result_heap_(pos, ret);
	else
		return rational_result_heap_(local, pos, ret);
}

int real_throw_alloc_(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_RATIO:
			ratio_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			single_float_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_throw_alloc(local, pos, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}

	return 0;
}

int real_throw_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return real_throw_alloc_(local, pos, ret);
}

int real_throw_heap_(addr pos, addr *ret)
{
	return real_throw_alloc_(NULL, pos, ret);
}

int real_copy_alloc_(LocalRoot local, addr pos, addr *ret)
{
	if (floatp(pos)) {
		float_copy_alloc(local, pos, ret);
		return 0;
	}
	else {
		return rational_copy_alloc_(local, pos, ret);
	}
}

int real_copy_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return real_copy_alloc_(local, pos, ret);
}

int real_copy_heap_(addr pos, addr *ret)
{
	return real_copy_alloc_(NULL, pos, ret);
}

int cast_double_float_unsafe_(addr value, double_float *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			*ret = (double_float)RefFixnum(value);
			break;

		case LISPTYPE_BIGNUM:
			return double_float_bignum_(value, ret);

		case LISPTYPE_RATIO:
			return double_float_ratio_(value, ret);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = (double_float)RefSingleFloat(value);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = RefDoubleFloat(value);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = (double_float)RefLongFloat(value);
			break;

		default:
			*ret = 0.0;
			return TypeError_(value, REAL);
	}

	return 0;
}


/*
 *  build_real
 */
void build_real(void)
{
	build_real_common();
}


/************************************************************
 *  real_ceiling.c
 ************************************************************/

/*
 *  common
 */
static int ceiling1_float_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	Return(float_ceiling1_s_(v, &v, &r));
	single_float_integer_heap(local, quot, v);
	single_float_heap(rem, r);

	return 0;
}

static int ceiling1_double_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	Return(float_ceiling1_d_(v, &v, &r));
	double_float_integer_heap(local, quot, v);
	double_float_heap(rem, r);

	return 0;
}

static int ceiling1_long_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	Return(float_ceiling1_l_(v, &v, &r));
	long_float_integer_heap(local, quot, v);
	long_float_heap(rem, r);

	return 0;
}

int ceiling1_common_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_heap(left, quot);
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_heap(left, quot);
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_RATIO:
			return float_ceiling1_ratio_(local, quot, rem, left);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling1_float_(local, quot, rem, left);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling1_double_(local, quot, rem, left);

		case LISPTYPE_LONG_FLOAT:
			return ceiling1_long_(local, quot, rem, left);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}

	return 0;
}

static int fceiling1_float_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	Return(float_ceiling1_s_(v, &v, &r));
	single_float_heap(quot, v);
	single_float_heap(rem, r);

	return 0;
}

static int fceiling1_double_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	Return(float_ceiling1_d_(v, &v, &r));
	double_float_heap(quot, v);
	double_float_heap(rem, r);

	return 0;
}

static int fceiling1_long_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	Return(float_ceiling1_l_(v, &v, &r));
	long_float_heap(quot, v);
	long_float_heap(rem, r);

	return 0;
}

int fceiling1_common_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_heap(quot, left);
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_BIGNUM:
			Return(single_float_bignum_heap_(quot, left));
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_RATIO:
			return float_fceiling1_ratio_(local, quot, rem, left);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling1_float_(local, quot, rem, left);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling1_double_(local, quot, rem, left);

		case LISPTYPE_LONG_FLOAT:
			return fceiling1_long_(local, quot, rem, left);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}

	return 0;
}

static int ceiling_ff_common_(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return float_ceiling_fixnum_(quot, rem, a, b);
}

static int ceiling_fb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_ceiling_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ceiling_fr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_ceiling_br_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ceiling_fs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ceiling_fd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_fl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_fixnum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ceiling_ff_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ceiling_fb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ceiling_fr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling_fs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling_fd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ceiling_fl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ceiling_bf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_ceiling_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ceiling_bs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &a));
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ceiling_bd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_bl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &a));
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_bignum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ceiling_bf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_ceiling_bignum_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_ceiling_br_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling_bs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling_bd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ceiling_bl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ceiling_rf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_ceiling_rb_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ceiling_rs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_ratio_(left, &a));
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ceiling_rd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_ratio_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_rl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_ratio_(left, &a));
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_ratio_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ceiling_rf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_ceiling_rb_ratio_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_ceiling_rr_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling_rs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling_rd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ceiling_rl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ceiling_sf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ceiling_sb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	Return(single_float_bignum_(right, &b));
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ceiling_sr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	Return(single_float_ratio_(right, &b));
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ceiling_ss_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ceiling_sd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_sl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_single_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ceiling_sf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ceiling_sb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ceiling_sr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling_ss_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling_sd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ceiling_sl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ceiling_df_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_db_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	Return(double_float_bignum_(right, &b));
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_dr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	Return(double_float_ratio_(right, &b));
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_ds_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_dd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_dl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_double_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ceiling_df_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ceiling_db_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ceiling_dr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling_ds_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling_dd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ceiling_dl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ceiling_lf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_lb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	Return(long_float_bignum_(right, &b));
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_lr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	Return(long_float_ratio_(right, &b));
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_ls_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_ld_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_ll_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_long_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ceiling_lf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ceiling_lb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ceiling_lr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling_ls_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling_ld_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ceiling_ll_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

int ceiling2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return ceiling_fixnum_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ceiling_bignum_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ceiling_ratio_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling_single_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling_double_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ceiling_long_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}
}

static int fceiling_ff_common_(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return float_fceiling_fixnum_(quot, rem, a, b);
}

static int fceiling_fb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_fceiling_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fceiling_fr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_fceiling_br_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fceiling_fs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fceiling_fd_common_(addr *quot, addr *rem, addr left, addr right)

{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_fl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_fixnum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fceiling_ff_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fceiling_fb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fceiling_fr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling_fs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling_fd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fceiling_fl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fceiling_bf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_fceiling_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fceiling_bs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &a));
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fceiling_bd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_bl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &a));
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_bignum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fceiling_bf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_fceiling_bignum_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_fceiling_br_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling_bs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling_bd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fceiling_bl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fceiling_rf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_fceiling_rb_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fceiling_rs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_ratio_(left, &a));
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fceiling_rd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_ratio_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_rl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_ratio_(left, &a));
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_ratio_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fceiling_rf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_fceiling_rb_ratio_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_fceiling_rr_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling_rs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling_rd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fceiling_rl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fceiling_sf_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fceiling_sb_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	Return(single_float_bignum_(right, &b));
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fceiling_sr_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	Return(single_float_ratio_(right, &b));
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fceiling_ss_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fceiling_sd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_sl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_single_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fceiling_sf_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fceiling_sb_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fceiling_sr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling_ss_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling_sd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fceiling_sl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fceiling_df_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_db_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	Return(double_float_bignum_(right, &b));
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_dr_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	Return(double_float_ratio_(right, &b));
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_ds_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_dd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_dl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_double_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fceiling_df_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fceiling_db_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fceiling_dr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling_ds_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling_dd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fceiling_dl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fceiling_lf_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_lb_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	Return(long_float_bignum_(right, &b));
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_lr_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	Return(long_float_ratio_(right, &b));
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_ls_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_ld_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_ll_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_long_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fceiling_lf_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fceiling_lb_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fceiling_lr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling_ls_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling_ld_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fceiling_ll_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

int fceiling2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return fceiling_fixnum_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fceiling_bignum_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fceiling_ratio_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling_single_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling_double_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fceiling_long_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}
}

int ceiling_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		return ceiling1_common_(local, ret1, ret2, var);
	else
		return ceiling2_common_(local, ret1, ret2, var, div);
}

int fceiling_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		return fceiling1_common_(local, ret1, ret2, var);
	else
		return fceiling2_common_(local, ret1, ret2, var, div);
}


/************************************************************
 *  real_common.c
 ************************************************************/

/*
 *  constant
 */
#ifdef FLT_TRUE_MIN
static void single_float_least_positive(addr *ret)
{
	single_float_heap(ret, FLT_TRUE_MIN);
}
void double_float_least_positive(addr *ret)
{
	double_float_heap(ret, DBL_TRUE_MIN);
}
void long_float_least_positive(addr *ret)
{
	long_float_heap(ret, LDBL_TRUE_MIN);
}
static void single_float_least_negative(addr *ret)
{
	single_float_heap(ret, -FLT_TRUE_MIN);
}
void double_float_least_negative(addr *ret)
{
	double_float_heap(ret, -DBL_TRUE_MIN);
}
void long_float_least_negative(addr *ret)
{
	long_float_heap(ret, -LDBL_TRUE_MIN);
}
#else
static void single_float_least_positive(addr *ret)
{
	const static single_float v = 1.40129846E-45F;
	single_float_heap(ret, v == 0.0f? FLT_MIN: v);
}
void double_float_least_positive(addr *ret)
{
	const static double_float v = 4.9406564584124654E-324;
	double_float_heap(ret, v == 0.0? DBL_MIN: v);
}
void long_float_least_positive(addr *ret)
{
	const static long_float v = 3.6451995318824746025E-4951L;
	long_float_heap(ret, v == 0.0L? LDBL_MIN: v);
}
static void single_float_least_negative(addr *ret)
{
	const static single_float v = 1.40129846E-45F;
	single_float_heap(ret, v == 0.0f? -FLT_MIN: -v);
}
void double_float_least_negative(addr *ret)
{
	const static double_float v = 4.9406564584124654E-324;
	double_float_heap(ret, v == 0.0? -DBL_MIN: -v);
}
void long_float_least_negative(addr *ret)
{
	const static long_float v = 3.6451995318824746025E-4951L;
	long_float_heap(ret, v == 0.0L? -LDBL_MIN: -v);
}
#endif

static void single_float_least_positive_normalized(addr *ret)
{
	single_float_heap(ret, FLT_MIN);
}
void double_float_least_positive_normalized(addr *ret)
{
	double_float_heap(ret, DBL_MIN);
}
void long_float_least_positive_normalized(addr *ret)
{
	long_float_heap(ret, LDBL_MIN);
}
static void single_float_least_negative_normalized(addr *ret)
{
	single_float_heap(ret, -FLT_MIN);
}
void double_float_least_negative_normalized(addr *ret)
{
	double_float_heap(ret, -DBL_MIN);
}
void long_float_least_negative_normalized(addr *ret)
{
	long_float_heap(ret, -LDBL_MIN);
}

static void single_float_epsilon(addr *ret)
{
	volatile single_float x, y, a, b;

	x = FLT_EPSILON;
	a = 1.0f;
	b = 1.0f + x;
	if (a == b) {
		for (;;) {
			x *= 2.0f;
			b = 1.0f + x;
			if (a != b)
				break;
		}
	}
	else {
		for (;;) {
			y = x * 0.5f;
			b = 1.0f + y;
			if (a == b)
				break;
			x = y;
		}
	}
	single_float_heap(ret, x);
}

static void single_float_negative_epsilon(addr *ret)
{
	volatile single_float x, y, a, b;

	x = FLT_EPSILON;
	a = 1.0f;
	b = 1.0f - x;
	if (a == b) {
		for (;;) {
			x *= 2.0f;
			b = 1.0f - x;
			if (a != b)
				break;
		}
	}
	else {
		for (;;) {
			y = x * 0.5f;
			b = 1.0f - y;
			if (a == b)
				break;
			x = y;
		}
	}
	single_float_heap(ret, x);
}

void double_float_epsilon(addr *ret)
{
	volatile double_float x, y, a, b;

	x = DBL_EPSILON;
	a = 1.0;
	b = 1.0 + x;
	if (a == b) {
		for (;;) {
			x *= 2.0;
			b = 1.0 + x;
			if (a != b)
				break;
		}
	}
	else {
		for (;;) {
			y = x * 0.5;
			b = 1.0 + y;
			if (a == b)
				break;
			x = y;
		}
	}
	double_float_heap(ret, x);
}

void double_float_negative_epsilon(addr *ret)
{
	volatile double_float x, y, a, b;

	x = DBL_EPSILON;
	a = 1.0;
	b = 1.0 - x;
	if (a == b) {
		for (;;) {
			x *= 2.0;
			b = 1.0 - x;
			if (a != b)
				break;
		}
	}
	else {
		for (;;) {
			y = x * 0.5;
			b = 1.0 - y;
			if (a == b)
				break;
			x = y;
		}
	}
	double_float_heap(ret, x);
}

void long_float_epsilon(addr *ret)
{
	volatile long_float x, y, a, b;

	x = LDBL_EPSILON;
	a = 1.0L;
	b = 1.0L + x;
	if (a == b) {
		for (;;) {
			x *= 2.0L;
			b = 1.0L + x;
			if (a != b)
				break;
		}
	}
	else {
		for (;;) {
			y = x * 0.5L;
			b = 1.0L + y;
			if (a == b)
				break;
			x = y;
		}
	}
	long_float_heap(ret, x);
}

void long_float_negative_epsilon(addr *ret)
{
	volatile long_float x, y, a, b;

	x = LDBL_EPSILON;
	a = 1.0L;
	b = 1.0L - x;
	if (a == b) {
		for (;;) {
			x *= 2.0L;
			b = 1.0L - x;
			if (a != b)
				break;
		}
	}
	else {
		for (;;) {
			y = x * 0.5L;
			b = 1.0L - y;
			if (a == b)
				break;
			x = y;
		}
	}
	long_float_heap(ret, x);
}

static void build_index_max(void)
{
	addr pos;

	make_indexmax_alloc(NULL, &pos);
	SetConstant(CONSTANT_INDEX_MAX, pos);
}

static void build_float_max(void)
{
	addr value;

	single_float_heap(&value, FLT_MAX);
	SetConstant(CONSTANT_SINGLE_FLOAT_MOST_POSITIVE, value);
	single_float_heap(&value, -FLT_MAX);
	SetConstant(CONSTANT_SINGLE_FLOAT_MOST_NEGATIVE, value);
}

static void build_float_min(void)
{
	addr value;

	/* positive */
	single_float_least_positive(&value);
	SetConstant(CONSTANT_SINGLE_FLOAT_LEAST_POSITIVE, value);
	single_float_least_positive_normalized(&value);
	SetConstant(CONSTANT_SINGLE_FLOAT_LEAST_POSITIVE_NORMALIZED, value);
	/* negative */
	single_float_least_negative(&value);
	SetConstant(CONSTANT_SINGLE_FLOAT_LEAST_NEGATIVE, value);
	single_float_least_negative_normalized(&value);
	SetConstant(CONSTANT_SINGLE_FLOAT_LEAST_NEGATIVE_NORMALIZED, value);
}

static void build_float_epsilon(void)
{
	addr value;

	/* positive */
	single_float_epsilon(&value);
	SetConstant(CONSTANT_SINGLE_FLOAT_EPSILON, value);
	/* negative */
	single_float_negative_epsilon(&value);
	SetConstant(CONSTANT_SINGLE_FLOAT_NEGATIVE_EPSILON, value);
}

void build_real_common(void)
{
	build_index_max();
	build_float_max();
	build_float_min();
	build_float_epsilon();
}


/*
 *  common-lisp
 */
static int unbound_float_cast_(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_heap(ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return single_float_bignum_heap_(ret, pos);

		case LISPTYPE_RATIO:
			return single_float_ratio_heap_(ret, pos);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
			return Result(ret, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}
}

static int single_from_double_cast_(addr *ret, addr pos)
{
	double_float v, a;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	GetDoubleFloat(pos, &v);
	a = fabs(v);
	if (FLT_MAX < a) {
		*ret = Nil;
		return call_float_overflow_va_(NULL, CONSTANT_COMMON_FLOAT, pos, NULL);
	}
	single_float_heap(ret, (single_float)v);

	return 0;
}

static int single_from_long_cast_(addr *ret, addr pos)
{
	long_float v, a;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, &v);
	a = fabsl(v);
	if (FLT_MAX < a) {
		*ret = Nil;
		return call_float_overflow_va_(NULL, CONSTANT_COMMON_FLOAT, pos, NULL);
	}
	single_float_heap(ret, (single_float)v);

	return 0;
}

static int double_from_long_cast_(addr *ret, addr pos)
{
	long_float v, a;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, &v);
	a = fabsl(v);
	if (DBL_MAX < a) {
		*ret = Nil;
		return call_float_overflow_va_(NULL, CONSTANT_COMMON_FLOAT, pos, NULL);
	}
	double_float_heap(ret, (double_float)v);

	return 0;
}

static int single_float_cast_(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_heap(ret, pos);
			break;

		case LISPTYPE_BIGNUM:
			return single_float_bignum_heap_(ret, pos);

		case LISPTYPE_RATIO:
			return single_float_ratio_heap_(ret, pos);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			*ret = pos;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			return single_from_double_cast_(ret, pos);

		case LISPTYPE_LONG_FLOAT:
			return single_from_long_cast_(ret, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}

	return 0;
}

static int double_float_cast_(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			double_float_fixnum_heap(ret, pos);
			break;

		case LISPTYPE_BIGNUM:
			return double_float_bignum_heap_(ret, pos);

		case LISPTYPE_RATIO:
			return double_float_ratio_heap_(ret, pos);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			double_float_heap(ret, (double_float)RefSingleFloat(pos));
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = pos;
			break;

		case LISPTYPE_LONG_FLOAT:
			return double_from_long_cast_(ret, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}

	return 0;
}

static int long_float_cast_(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			long_float_fixnum_heap(ret, pos);
			break;

		case LISPTYPE_BIGNUM:
			return long_float_bignum_heap_(ret, pos);

		case LISPTYPE_RATIO:
			return long_float_ratio_heap_(ret, pos);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			long_float_heap(ret, (long_float)RefSingleFloat(pos));
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			long_float_heap(ret, (long_float)RefDoubleFloat(pos));
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = pos;
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}

	return 0;
}

int float_common_(addr *ret, addr pos, addr type)
{
	if (type == Unbound)
		return unbound_float_cast_(ret, pos);

	switch (GetType(type)) {
		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return single_float_cast_(ret, pos);

		case LISPTYPE_DOUBLE_FLOAT:
			return double_float_cast_(ret, pos);

		case LISPTYPE_LONG_FLOAT:
			return long_float_cast_(ret, pos);

		default:
			*ret = Nil;
			return TypeError_(type, FLOAT);
	}
}


/************************************************************
 *  real_decode.c
 ************************************************************/

#define real_decode_inexact_(ptr, x,y) \
	call_float_inexact_va_(ptr, CONSTANT_COMMON_##x, (y), NULL)

/*
 *  decode-float
 */
static void decode_single_float(addr pos, addr *ret, addr *rexp, addr *rsign)
{
	int exp;
	single_float v;

	GetSingleFloat(pos, &v);
	v = frexpf(v, &exp);
	single_float_heap(ret, fabsf(v));
	fixnum_heap(rexp, (fixnum)exp);
	single_float_heap(rsign, signbit(v)? -1.0f: 1.0f);
}

static void decode_double_float(addr pos, addr *ret, addr *rexp, addr *rsign)
{
	int exp;
	double_float v;

	GetDoubleFloat(pos, &v);
	v = frexp(v, &exp);
	double_float_heap(ret, fabs(v));
	fixnum_heap(rexp, (fixnum)exp);
	double_float_heap(rsign, signbit(v)? -1.0: 1.0);
}

static void decode_long_float(addr pos, addr *ret, addr *rexp, addr *rsign)
{
	int exp;
	long_float v;

	GetLongFloat(pos, &v);
	v = frexpl(v, &exp);
	long_float_heap(ret, fabsl(v));
	fixnum_heap(rexp, (fixnum)exp);
	long_float_heap(rsign, signbit(v)? -1.0L: 1.0L);
}

int decode_float_common_(addr pos, addr *ret, addr *rexp, addr *rsign)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			decode_single_float(pos, ret, rexp, rsign);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			decode_double_float(pos, ret, rexp, rsign);
			break;

		case LISPTYPE_LONG_FLOAT:
			decode_long_float(pos, ret, rexp, rsign);
			break;

		default:
			*ret = *rexp = *rsign = 0;
			return TypeError_(pos, FLOAT);
	}

	return 0;
}


/*
 *  scale-float
 */
int scale_float_common_(addr pos, addr scale, addr *ret)
{
	fixnum fixnum_value;
	long n;
	single_float vs;
	double_float vd;
	long_float vl;

	/* scale */
	switch (GetType(scale)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(scale, &fixnum_value);
			if (fixnum_value < LONG_MIN || LONG_MAX < fixnum_value) {
				*ret = Nil;
				return fmte_("Scaling factor is too large ~A.", scale, NULL);
			}
			n = (long)fixnum_value;
			break;

		case LISPTYPE_BIGNUM:
			*ret = Nil;
			return fmte_("Scaling factor ~A must be a fixnum type.", scale, NULL);

		default:
			*ret = Nil;
			return TypeError_(scale, INTEGER);
	}

	/* scale-float */
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(pos, &vs);
			return single_float_check_heap_(ret, scalblnf(vs, n));

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(pos, &vd);
			return double_float_check_heap_(ret, scalbln(vd, n));

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(pos, &vl);
			return long_float_check_heap_(ret, scalblnl(vl, n));

		default:
			*ret = Nil;
			return TypeError_(pos, FLOAT);
	}
}


/*
 *  float-radix
 */
void float_radix_common(addr pos, addr *ret)
{
	fixnum_heap(ret, FLT_RADIX);
}


/*
 *  float-sign
 */
static int float_sign1_common_(addr pos, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(pos, &vs);
			single_float_heap(ret, signbit(vs)? -1.0f: 1.0f);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(pos, &vd);
			double_float_heap(ret, signbit(vd)? -1.0: 1.0);
			break;

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(pos, &vl);
			long_float_heap(ret, signbit(vl)? -1.0L: 1.0L);
			break;

		default:
			*ret = 0;
			return TypeError_(pos, FLOAT);
	}

	return 0;
}

#define copysign_ss(x,y)  copysignf((y), (x))
#define copysign_dd(x,y)  copysign((y), (x))
#define copysign_ll(x,y)  copysignl((y), (x))
#define copysign_code(a, b) { \
	if (signbit(a)) { \
		return signbit(b)? (b): -(b); \
	} \
	else { \
		return signbit(b)? -(b): (b); \
	} \
}

static inline double_float copysign_sd(single_float a, double_float b)
{
	copysign_code(a, b);
}
static inline long_float copysign_sl(single_float a, long_float b)
{
	copysign_code(a, b);
}
static inline single_float copysign_ds(double_float a, single_float b)
{
	copysign_code(a, b);
}
static inline long_float copysign_dl(double_float a, long_float b)
{
	copysign_code(a, b);
}
static inline single_float copysign_ls(long_float a, single_float b)
{
	copysign_code(a, b);
}
static inline double_float copysign_ld(long_float a, double_float b)
{
	copysign_code(a, b);
}

static int float_sign2_single_(single_float left, addr opt, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(opt)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(opt, &vs);
			return single_float_check_heap_(ret, copysign_ss(left, vs));

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(opt, &vd);
			return double_float_check_heap_(ret, copysign_sd(left, vd));

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(opt, &vl);
			return long_float_check_heap_(ret, copysign_sl(left, vl));

		default:
			*ret = 0;
			return TypeError_(opt, FLOAT);
	}
}

static int float_sign2_double_(double_float left, addr opt, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(opt)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(opt, &vs);
			return single_float_check_heap_(ret, copysign_ds(left, vs));

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(opt, &vd);
			return double_float_check_heap_(ret, copysign_dd(left, vd));

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(opt, &vl);
			return long_float_check_heap_(ret, copysign_dl(left, vl));

		default:
			*ret = 0;
			return TypeError_(opt, FLOAT);
	}
}

static int float_sign2_long_(long_float left, addr opt, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(opt)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(opt, &vs);
			return single_float_check_heap_(ret, copysign_ls(left, vs));

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(opt, &vd);
			return double_float_check_heap_(ret, copysign_ld(left, vd));

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(opt, &vl);
			return long_float_check_heap_(ret, copysign_ll(left, vl));

		default:
			*ret = 0;
			return TypeError_(opt, FLOAT);
	}
}

static int float_sign2_common_(addr pos, addr opt, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(pos, &vs);
			return float_sign2_single_(vs, opt, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(pos, &vd);
			return float_sign2_double_(vd, opt, ret);

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(pos, &vl);
			return float_sign2_long_(vl, opt, ret);

		default:
			*ret = 0;
			return TypeError_(pos, FLOAT);
	}
}

int float_sign_common_(addr pos, addr opt, addr *ret)
{
	if (opt == Unbound)
		return float_sign1_common_(pos, ret);
	else
		return float_sign2_common_(pos, opt, ret);
}


/*
 *  float-digits
 */
int float_digits_common_(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			fixnum_heap(ret, FLT_MANT_DIG);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fixnum_heap(ret, DBL_MANT_DIG);
			break;

		case LISPTYPE_LONG_FLOAT:
			fixnum_heap(ret, LDBL_MANT_DIG);
			break;

		default:
			*ret = 0;
			return TypeError_(pos, FLOAT);
	}

	return 0;
}


/*
 *  float-precision
 */
static int float_precision_single(single_float v, int *ret)
{
	switch (fpclassify(v)) {
		case FP_NORMAL:
			*ret = FLT_MANT_DIG;
			return 0;

		case FP_ZERO:
			*ret = 0;
			return 0;

		case FP_SUBNORMAL:
			*ret = FLT_MANT_DIG - (FLT_MIN_EXP - 1) + ilogbf(v);
			return 0;

		default:
			return 1;
	}
}

static int float_precision_double(double_float v, int *ret)
{
	switch (fpclassify(v)) {
		case FP_NORMAL:
			*ret = DBL_MANT_DIG;
			return 0;

		case FP_ZERO:
			*ret = 0;
			return 0;

		case FP_SUBNORMAL:
			*ret = DBL_MANT_DIG - (DBL_MIN_EXP - 1) + ilogb(v);
			return 0;

		default:
			return 1;
	}
}

static int float_precision_long(long_float v, int *ret)
{
	switch (fpclassify(v)) {
		case FP_NORMAL:
			*ret = LDBL_MANT_DIG;
			return 0;

		case FP_ZERO:
			*ret = 0;
			return 0;

		case FP_SUBNORMAL:
			*ret = LDBL_MANT_DIG - (LDBL_MIN_EXP - 1) + ilogbl(v);
			return 0;

		default:
			return 1;
	}
}

static int float_precision_float(addr pos, int *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(pos, &vs);
			return float_precision_single(vs, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(pos, &vd);
			return float_precision_double(vd, ret);

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(pos, &vl);
			return float_precision_long(vl, ret);

		default:
			return -1;
	}
}

int float_precision_common_(Execute ptr, addr pos, addr *ret)
{
	int size, check;

	check = float_precision_float(pos, &size);
	if (check < 0)
		return TypeError_(pos, FLOAT);
	if (check)
		return real_decode_inexact_(ptr, FLOAT_PRECISION, pos);

	fixnum_heap(ret, (fixnum)size);
	return 0;
}


/*
 *  integer-decode-float
 */
static int integer_decode_float_single_value_(LocalRoot local,
		single_float v, addr *value, int *rexp, int *rsign, int *ret)
{
	int e, p, check;

	*rsign = signbit(v)? -1: 1;
	v = frexpf(fabsf(v), &e);
	if (float_precision_single(v, &p))
		return Result(ret, 1);
	v = ldexpf(v, p);
	*rexp = e - p;
	Return(bignum_single_float_local_(local, v, value, &check));
	if (check)
		return Result(ret, 1);

	return Result(ret, 0);
}

static int integer_decode_float_double_value_(LocalRoot local,
		double_float v, addr *value, int *rexp, int *rsign, int *ret)
{
	int e, p, check;

	*rsign = signbit(v)? -1: 1;
	v = frexp(fabs(v), &e);
	if (float_precision_double(v, &p))
		return Result(ret, 1);
	v = ldexp(v, p);
	*rexp = e - p;
	Return(bignum_double_float_local_(local, v, value, &check));
	if (check)
		return Result(ret, 1);

	return Result(ret, 0);
}

static int integer_decode_float_long_value_(LocalRoot local,
		long_float v, addr *value, int *rexp, int *rsign, int *ret)
{
	int e, p, check;

	*rsign = signbit(v)? -1: 1;
	v = frexpl(fabsl(v), &e);
	if (float_precision_long(v, &p))
		return Result(ret, 1);
	v = ldexpl(v, p);
	*rexp = e - p;
	Return(bignum_long_float_local_(local, v, value, &check));
	if (check)
		return Result(ret, 1);

	return Result(ret, 0);
}

static int integer_decode_float_single_(Execute ptr,
		addr pos, addr *ret, addr *rexp, addr *rsign)
{
	int e, sign, check;
	single_float v;
	addr temp;
	LocalRoot local;
	LocalStack stack;

	GetSingleFloat(pos, &v);
	if (v == 0.0f) {
		fixnum_heap(ret, 0);
		fixnum_heap(rexp, 0);
		fixnum_heap(rsign, signbit(v)? -1: 1);
		return 0;
	}

	local = ptr->local;
	push_local(local, &stack);
	Return(integer_decode_float_single_value_(local, v, &temp, &e, &sign, &check));
	if (check) {
		*ret = *rexp = *rsign = 0;
		return real_decode_inexact_(ptr, INTEGER_DECODE_FLOAT, pos);
	}
	bignum_result_heap(temp, ret);
	rollback_local(local, stack);
	fixnum_heap(rexp, (fixnum)e);
	fixnum_heap(rsign, (fixnum)sign);

	return 0;
}

static int integer_decode_float_double_(Execute ptr,
		addr pos, addr *ret, addr *rexp, addr *rsign)
{
	int e, sign, check;
	double_float v;
	addr temp;
	LocalRoot local;
	LocalStack stack;

	GetDoubleFloat(pos, &v);
	if (v == 0.0) {
		fixnum_heap(ret, 0);
		fixnum_heap(rexp, 0);
		fixnum_heap(rsign, signbit(v)? -1: 1);
		return 0;
	}

	local = ptr->local;
	push_local(local, &stack);
	Return(integer_decode_float_double_value_(local, v, &temp, &e, &sign, &check));
	if (check) {
		*ret = *rexp = *rsign = 0;
		return real_decode_inexact_(ptr, INTEGER_DECODE_FLOAT, pos);
	}
	bignum_result_heap(temp, ret);
	rollback_local(local, stack);
	fixnum_heap(rexp, (fixnum)e);
	fixnum_heap(rsign, (fixnum)sign);

	return 0;
}

static int integer_decode_float_long_(Execute ptr,
		addr pos, addr *ret, addr *rexp, addr *rsign)
{
	int e, sign, check;
	long_float v;
	addr temp;
	LocalRoot local;
	LocalStack stack;

	GetLongFloat(pos, &v);
	if (v == 0.0L) {
		fixnum_heap(ret, 0);
		fixnum_heap(rexp, 0);
		fixnum_heap(rsign, signbit(v)? -1: 1);
		return 0;
	}

	local = ptr->local;
	push_local(local, &stack);
	Return(integer_decode_float_long_value_(local, v, &temp, &e, &sign, &check));
	if (check) {
		*ret = *rexp = *rsign = 0;
		return real_decode_inexact_(ptr, INTEGER_DECODE_FLOAT, pos);
	}
	bignum_result_heap(temp, ret);
	rollback_local(local, stack);
	fixnum_heap(rexp, (fixnum)e);
	fixnum_heap(rsign, (fixnum)sign);

	return 0;
}

int integer_decode_float_common_(Execute ptr,
		addr pos, addr *ret, addr *rexp, addr *rsign)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return integer_decode_float_single_(ptr, pos, ret, rexp, rsign);

		case LISPTYPE_DOUBLE_FLOAT:
			return integer_decode_float_double_(ptr, pos, ret, rexp, rsign);

		case LISPTYPE_LONG_FLOAT:
			return integer_decode_float_long_(ptr, pos, ret, rexp, rsign);

		default:
			*ret = *rexp = *rsign = 0;
			return TypeError_(pos, FLOAT);
	}
}


/*
 *  rational
 */
static int rational_ash_common_(LocalRoot local,
		addr pos, int sign2, size_t size, addr *ret)
{
	int sign1;

	CheckType(pos, LISPTYPE_BIGNUM);
	GetSignBignum(pos, &sign1);
	if (IsPlus(sign2))
		shiftup_bigdata_alloc(local, &pos, pos, size);
	else
		shiftdown_bigdata_alloc(local, &pos, pos, size);
	SetSignBignum(pos, sign1);
	bignum_result_heap(pos, ret);

	return 0;
}

static int rational_float_common_(LocalRoot local, addr *ret, addr pos, int e, int sign)
{
	addr denom;

	sign = (sign < 0)? SignMinus: SignPlus;
	if (e < 0) {
		/* ratio: (/ pos (ash 1 (- e))) */
		power2_bigdata_alloc(local, &denom, (size_t)-e);
		ratio_reduction_heap(local, ret, sign, pos, denom);
		return 0;
	}

	/* integer: (ash pos e) */
	return rational_ash_common_(local, pos, sign, (size_t)e, ret);
}

static int rational_single_common_(Execute ptr, addr pos, addr *ret)
{
	int e, sign, check;
	single_float v;
	LocalRoot local;
	LocalStack stack;

	GetSingleFloat(pos, &v);
	if (v == 0.0f) {
		fixnum_heap(ret, 0);
		return 0;
	}

	local = ptr->local;
	push_local(local, &stack);
	Return(integer_decode_float_single_value_(local, v, &pos, &e, &sign, &check));
	if (check) {
		*ret = 0;
		return real_decode_inexact_(ptr, INTEGER_DECODE_FLOAT, pos);
	}
	Return(rational_float_common_(local, ret, pos, e, sign));
	rollback_local(local, stack);

	return 0;
}

static int rational_double_common_(Execute ptr, addr pos, addr *ret)
{
	int e, sign, check;
	double_float v;
	LocalRoot local;
	LocalStack stack;

	GetDoubleFloat(pos, &v);
	if (v == 0.0) {
		fixnum_heap(ret, 0);
		return 0;
	}

	local = ptr->local;
	push_local(local, &stack);
	Return(integer_decode_float_double_value_(local, v, &pos, &e, &sign, &check));
	if (check) {
		*ret = 0;
		return real_decode_inexact_(ptr, INTEGER_DECODE_FLOAT, pos);
	}
	Return(rational_float_common_(local, ret, pos, e, sign));
	rollback_local(local, stack);

	return 0;
}

static int rational_long_common_(Execute ptr, addr pos, addr *ret)
{
	int e, sign, check;
	long_float v;
	LocalRoot local;
	LocalStack stack;

	GetLongFloat(pos, &v);
	if (v == 0.0L) {
		fixnum_heap(ret, 0);
		return 0;
	}

	local = ptr->local;
	push_local(local, &stack);
	Return(integer_decode_float_long_value_(local, v, &pos, &e, &sign, &check));
	if (check) {
		*ret = 0;
		return real_decode_inexact_(ptr, INTEGER_DECODE_FLOAT, pos);
	}
	Return(rational_float_common_(local, ret, pos, e, sign));
	rollback_local(local, stack);

	return 0;
}

int rational_common_(Execute ptr, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_heap(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_throw_heap(pos, ret);
			return 0;

		case LISPTYPE_RATIO:
			ratio_throw_heap(pos, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return rational_single_common_(ptr, pos, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return rational_double_common_(ptr, pos, ret);

		case LISPTYPE_LONG_FLOAT:
			return rational_long_common_(ptr, pos, ret);

		default:
			*ret = 0;
			return TypeError_(pos, REAL);
	}
}


/*
 *  rationalize
 *
 *  The algorithm described in CLISP.
 *    CLISP, Bruno Haible.
 *      http://clisp.org/
 *      https://sourceforge.net/p/clisp/clisp/ci/default/tree/src/realelem.d
 *
 *  Lisp code.
 *    CMUCL  [src/code/float.lisp]
 *      https://www.cons.org/cmucl/
 *      https://gitlab.common-lisp.net/cmucl/cmucl/blob/21d/src/code/float.lisp
 *    SBCL  [src/code/float.lisp]
 *      http://www.sbcl.org/
 *      https://sourceforge.net/p/sbcl/sbcl/ci/sbcl-1.5.0/tree/src/code/float.lisp
 */
static int rationalize_copy_(addr *ret, addr pos)
{
	int sign;
	fixed value;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			castfixed_fixnum(pos, &sign, &value);
			bignum_value_heap(ret, SignPlus, value);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_copy_nosign_heap(ret, pos);
			return 0;

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

static int rationalize_build_ratio_(addr *ret, addr numer, addr denom)
{
	int sign1, sign2;

	Return(getsign_integer_(numer, &sign1));
	Return(getsign_integer_(denom, &sign2));
	sign1 = SignMulti(sign1, sign2);
	Return(rationalize_copy_(&numer, numer));
	Return(rationalize_copy_(&denom, denom));
	make_ratio_alloc_unsafe(NULL, ret, sign1, numer, denom);

	return 0;
}

static int rationalize_multi2_(LocalRoot local, addr *ret, addr frac)
{
	addr value;

	/* (* 2 frac) */
	fixnum_heap(&value, 2);
	return multi_ii_real_common_(local, frac, value, ret);
}

static int rationalize_letdenom_(LocalRoot local, addr expo, addr *ret)
{
	addr one;

	/* (ash 1 (- 1 expo)) */
	fixnum_heap(&one, 1);
	Return(minus_ii_real_common_(local, one, expo, &expo));
	return ash_integer_common_(local, one, expo, ret);
}

static int rationalize_ab_(LocalRoot local, addr *ra, addr *fb, addr frac, addr expo)
{
	addr a, b;

	Return(rationalize_multi2_(local, &frac, frac));
	Return(oneminus_integer_common_(local, frac, &a));
	Return(oneplus_integer_common_(local, frac, &b));
	Return(rationalize_letdenom_(local, expo, &expo));
	Return(rationalize_build_ratio_(ra, a, expo));
	Return(rationalize_build_ratio_(fb, b, expo));

	return 0;
}

static int rationalize_let_(LocalRoot local, addr z, addr x1, addr x0, addr *ret)
{
	Return(multi_ii_real_common_(local, z, x1, &z));
	Return(plus_ii_real_common_(local, z, x0, ret));
	return 0;
}

static int rationalize_minus1_(LocalRoot local, addr x, addr *ret)
{
	/* (- x 1) */
	addr one;
	fixnum_heap(&one, 1);
	return minus_ii_real_common_(local, x, one, ret);
}

static int rationalize_psetf_(LocalRoot local, addr x, addr k, addr *ret)
{
	/* (/ (- x k)) */
	Return(minus_rational_common_(local, x, k, &x));
	Return(inverse_rational_common_(local, x, ret));
	return 0;
}

static int rationalize_zero_check_(addr frac, addr expo, int *ret)
{
	Return(zerop_integer_(frac, ret));
	if (*ret)
		return 0;

	return zerop_or_plusp_integer_(expo, ret);
}

static int rationalize_float_(Execute ptr, addr x, addr *ret)
{
	int check;
	addr frac, expo, sign, v1, v2;
	addr a, b, c, p0, q0, p1, q1, top, bot, k, p2, q2;
	LocalRoot local;

	local = ptr->local;
	/* multiple-value-bind */
	Return(integer_decode_float_common_(ptr, x, &frac, &expo, &sign));
	/* cond */
	Return(rationalize_zero_check_(frac, expo, &check));
	if (check) {
		Return(ash_integer_common_(local, frac, expo, ret));
		Return(minusp_integer_(sign, &check));
		if (check) {
			Return(sign_reverse_integer_common_(*ret, ret));
		}
		return 0;
	}
	/* a, b */
	Return(rationalize_ab_(local, &a, &b, frac, expo));
	/* p0, q0, p1, q1 */
	fixnum_heap(&p0, 0);
	fixnum_heap(&q0, 1);
	fixnum_heap(&p1, 1);
	fixnum_heap(&q1, 0);
	/* do */
	for (;;) {
		Return(ceiling1_common_(local, &c, &v1, a));
		/* result */
		Return(less_rational_(local, c, b, &check));
		if (check) {
			Return(rationalize_let_(local, c, p1, p0, &top));
			Return(rationalize_let_(local, c, q1, q0, &bot));
			Return(minusp_integer_(sign, &check));
			if (check) {
				Return(sign_reverse_integer_common_(top, &top));
			}
			return rationalize_build_ratio_(ret, top, bot);
		}
		/* body */
		Return(rationalize_minus1_(local, c, &k));
		Return(rationalize_let_(local, k, p1, p0, &p2));
		Return(rationalize_let_(local, k, q1, q0, &q2));
		Return(rationalize_psetf_(local, b, k, &v1));
		Return(rationalize_psetf_(local, a, k, &v2));
		a = v1;
		b = v2;
		p0 = p1;
		q0 = q1;
		p1 = p2;
		q1 = q2;
	}
}

int rationalize_common_(Execute ptr, addr pos, addr *ret)
{
	int check;

	/* zerop */
	Return(zerop_real_(pos, &check));
	if (check) {
		fixnum_heap(ret, 0);
		return 0;
	}

	/* rational */
	if (rationalp(pos))
		return rational_throw_heap_(pos, ret);

	/* float */
	if (floatp(pos)) {
		Return(rationalize_float_(ptr, pos, &pos));
		ratio_result_noreduction_heap(ptr->local, pos, ret);
		return 0;
	}

	*ret = Nil;
	return TypeError_(pos, REAL);
}


/*
 *  CMUCL  [src/code/float.lisp]
 *    https://www.cons.org/cmucl/
 *    https://gitlab.common-lisp.net/cmucl/cmucl/blob/21d/src/code/float.lisp
 */
/*
 * ;;; -*- Mode: Lisp; Package: KERNEL; Log: code.log -*-
 * ;;;
 * ;;; **********************************************************************
 * ;;; This code was written as part of the CMU Common Lisp project at
 * ;;; Carnegie Mellon University, and has been placed in the public domain.
 * ;;;
 */
/* ;;; RATIONALIZE  --  Public
 * ;;;
 * ;;; The algorithm here is the method described in CLISP.  Bruno Haible has
 * ;;; graciously given permission to use this algorithm.  He says, "You can use
 * ;;; it, if you present the following explanation of the algorithm."
 * ;;;
 * ;;; Algorithm (recursively presented):
 * ;;;   If x is a rational number, return x.
 * ;;;   If x = 0.0, return 0.
 * ;;;   If x < 0.0, return (- (rationalize (- x))).
 * ;;;   If x > 0.0:
 * ;;;     Call (integer-decode-float x). It returns a m,e,s=1 (mantissa,
 * ;;;     exponent, sign).
 * ;;;     If m = 0 or e >= 0: return x = m*2^e.
 * ;;;     Search a rational number between a = (m-1/2)*2^e and b = (m+1/2)*2^e
 * ;;;     with smallest possible numerator and denominator.
 * ;;;     Note 1: If m is a power of 2, we ought to take a = (m-1/4)*2^e.
 * ;;;       But in this case the result will be x itself anyway, regardless of
 * ;;;       the choice of a. Therefore we can simply ignore this case.
 * ;;;     Note 2: At first, we need to consider the closed interval [a,b].
 * ;;;       but since a and b have the denominator 2^(|e|+1) whereas x itself
 * ;;;       has a denominator <= 2^|e|, we can restrict the seach to the open
 * ;;;       interval (a,b).
 * ;;;     So, for given a and b (0 < a < b) we are searching a rational number
 * ;;;     y with a <= y <= b.
 * ;;;     Recursive algorithm fraction_between(a,b):
 * ;;;       c := (ceiling a)
 * ;;;       if c < b
 * ;;;         then return c       ; because a <= c < b, c integer
 * ;;;         else
 * ;;;           ; a is not integer (otherwise we would have had c = a < b)
 * ;;;           k := c-1          ; k = floor(a), k < a < b <= k+1
 * ;;;           return y = k + 1/fraction_between(1/(b-k), 1/(a-k))
 * ;;;                             ; note 1 <= 1/(b-k) < 1/(a-k)
 * ;;;
 * ;;; You can see that we are actually computing a continued fraction expansion.
 * ;;;
 * ;;; Algorithm (iterative):
 * ;;;   If x is rational, return x.
 * ;;;   Call (integer-decode-float x). It returns a m,e,s (mantissa,
 * ;;;     exponent, sign).
 * ;;;   If m = 0 or e >= 0, return m*2^e*s. (This includes the case x = 0.0.)
 * ;;;   Create rational numbers a := (2*m-1)*2^(e-1) and b := (2*m+1)*2^(e-1)
 * ;;;   (positive and already in lowest terms because the denominator is a
 * ;;;   power of two and the numerator is odd).
 * ;;;   Start a continued fraction expansion
 * ;;;     p[-1] := 0, p[0] := 1, q[-1] := 1, q[0] := 0, i := 0.
 * ;;;   Loop
 * ;;;     c := (ceiling a)
 * ;;;     if c >= b
 * ;;;       then k := c-1, partial_quotient(k), (a,b) := (1/(b-k),1/(a-k)),
 * ;;;            goto Loop
 * ;;;   finally partial_quotient(c).
 * ;;;   Here partial_quotient(c) denotes the iteration
 * ;;;     i := i+1, p[i] := c*p[i-1]+p[i-2], q[i] := c*q[i-1]+q[i-2].
 * ;;;   At the end, return s * (p[i]/q[i]).
 * ;;;   This rational number is already in lowest terms because
 * ;;;   p[i]*q[i-1]-p[i-1]*q[i] = (-1)^i.
 * ;;;
 */
/* (defun rationalize (x)
 *   "Converts any REAL to a RATIONAL.  Floats are converted to a simple rational
 *   representation exploiting the assumption that floats are only accurate to
 *   their precision.  RATIONALIZE (and also RATIONAL) preserve the invariant:
 *       (= x (float (rationalize x) x))"
 *   (number-dispatch ((x real))
 *     (((foreach single-float double-float #+long-float long-float
 *                #+double-double double-double-float))
 *      ;; This is a fairly straigtforward implementation of the iterative
 *      ;; algorithm above.
 *      (multiple-value-bind (frac expo sign)
 *          (integer-decode-float x)
 *        (cond ((or (zerop frac) (>= expo 0))
 *               (if (minusp sign)
 *                   (- (ash frac expo))
 *                   (ash frac expo)))
 *              (t
 *               ;; expo < 0 and (2*m-1) and (2*m+1) are coprime to 2^(1-e),
 *               ;; so build the fraction up immediately, without having to do
 *               ;; a gcd.
 *               (let ((a (build-ratio (- (* 2 frac) 1) (ash 1 (- 1 expo))))
 *                     (b (build-ratio (+ (* 2 frac) 1) (ash 1 (- 1 expo))))
 *                     (p0 0)
 *                     (q0 1)
 *                     (p1 1)
 *                     (q1 0))
 *                 (do ((c (ceiling a) (ceiling a)))
 *                     ((< c b)
 *                      (let ((top (+ (* c p1) p0))
 *                            (bot (+ (* c q1) q0)))
 *                        (build-ratio (if (minusp sign)
 *                                         (- top)
 *                                         top)
 *                                     bot)))
 *                   (let* ((k (- c 1))
 *                          (p2 (+ (* k p1) p0))
 *                          (q2 (+ (* k q1) q0)))
 *                     (psetf a (/ (- b k))
 *                            b (/ (- a k)))
 *                     (setf p0 p1
 *                           q0 q1
 *                           p1 p2
 *                           q1 q2))))))))
 *     ((rational) x)))
 */


/************************************************************
 *  real_division.c
 ************************************************************/

static int division_by_zero_f_(constindex index, fixnum a, fixnum b)
{
	addr left, right;

	fixnum_heap(&left, a);
	fixnum_heap(&right, b);
	return call_division_by_zero_real2_(NULL, index, left, right);
}

static int division_by_zero_s_(constindex index, single_float a, single_float b)
{
	addr left, right;

	single_float_heap(&left, a);
	single_float_heap(&right, b);
	return call_division_by_zero_real2_(NULL, index, left, right);
}

static int division_by_zero_d_(constindex index, double_float a, double_float b)
{
	addr left, right;

	double_float_heap(&left, a);
	double_float_heap(&right, b);
	return call_division_by_zero_real2_(NULL, index, left, right);
}

static int division_by_zero_l_(constindex index, long_float a, long_float b)
{
	addr left, right;

	long_float_heap(&left, a);
	long_float_heap(&right, b);
	return call_division_by_zero_real2_(NULL, index, left, right);
}


/*
 *  floor
 */
int float_floor_s_(single_float a, single_float b, single_float *q, single_float *r)
{
	single_float m;

	if (b == 0.0f) {
		*q = *r = 0.0f;
		return division_by_zero_s_(CONSTANT_COMMON_FLOOR, a, b);
	}
	if (a == 0.0f) {
		*q = *r = 0.0f;
		return 0;
	}

	m = fmodf(a, b);
	if (m == 0.0f) {
		*q = a / b;
		*r = 0.0f;
		return 0;
	}
	if (0.0f < a) {
		if (b < 0.0f)
			goto floor2;
	}
	else if (0.0f < b) {
		goto floor2;
	}
	*q = floorf(a / b);
	*r = m;
	return 0;

floor2:
	*q = floorf(a / b);
	*r = b + m;
	return 0;
}

int float_floor_d_(double_float a, double_float b, double_float *q, double_float *r)
{
	double_float m;

	if (b == 0.0) {
		*q = *r = 0.0;
		return division_by_zero_d_(CONSTANT_COMMON_FLOOR, a, b);
	}
	if (a == 0.0) {
		*q = *r = 0.0;
		return 0;
	}

	m = fmod(a, b);
	if (m == 0.0) {
		*q = a / b;
		*r = 0.0;
		return 0;
	}
	if (0.0 < a) {
		if (b < 0.0)
			goto floor2;
	}
	else if (0.0 < b) {
		goto floor2;
	}
	*q = floor(a / b);
	*r = m;
	return 0;

floor2:
	*q = floor(a / b);
	*r = b + m;
	return 0;
}

int float_floor_l_(long_float a, long_float b, long_float *q, long_float *r)
{
	long_float m;

	if (b == 0.0L) {
		*q = *r = 0.0L;
		return division_by_zero_l_(CONSTANT_COMMON_FLOOR, a, b);
	}
	if (a == 0.0L) {
		*q = *r = 0.0L;
		return 0;
	}

	m = fmodl(a, b);
	if (m == 0.0L) {
		*q = a / b;
		*r = 0.0L;
		return 0;
	}
	if (0.0L < a) {
		if (b < 0.0L)
			goto floor2;
	}
	else if (0.0L < b) {
		goto floor2;
	}
	*q = floorl(a / b);
	*r = m;
	return 0;

floor2:
	*q = floorl(a / b);
	*r = b + m;
	return 0;
}

int float_floor1_s_(single_float a, single_float *q, single_float *r)
{
	single_float m;

	if (a == 0.0f) {
		*q = *r = 0.0f;
		return 0;
	}

	m = fmodf(a, 1.0f);
	if (m == 0.0f) {
		*q = a;
		*r = 0.0f;
		return 0;
	}
	if (0.0f < a)
		*r = m;
	else
		*r = 1.0f + m;

	*q = floorf(a);
	return 0;
}

int float_floor1_d_(double_float a, double_float *q, double_float *r)
{
	double_float m;

	if (a == 0.0) {
		*q = *r = 0.0;
		return 0;
	}

	m = fmod(a, 1.0);
	if (m == 0.0) {
		*q = a;
		*r = 0.0;
		return 0;
	}
	if (0.0 < a)
		*r = m;
	else
		*r = 1.0 + m;

	*q = floor(a);
	return 0;
}

int float_floor1_l_(long_float a, long_float *q, long_float *r)
{
	long_float m;

	if (a == 0.0L) {
		*q = *r = 0.0L;
		return 0;
	}

	m = fmodl(a, 1.0L);
	if (m == 0.0L) {
		*q = a;
		*r = 0.0L;
		return 0;
	}
	if (0.0L < a)
		*r = m;
	else
		*r = 1.0L + m;

	*q = floorl(a);
	return 0;
}


/*
 *  ceiling
 */
int float_ceiling_s_(single_float a, single_float b, single_float *q, single_float *r)
{
	single_float m;

	if (b == 0.0f) {
		*q = *r = 0.0f;
		return division_by_zero_s_(CONSTANT_COMMON_CEILING, a, b);
	}
	if (a == 0.0f) {
		*q = *r = 0.0f;
		return 0;
	}

	m = fmodf(a, b);
	if (m == 0.0f) {
		*q = a / b;
		*r = 0.0f;
		return 0;
	}
	if (0.0f < a) {
		if (b < 0.0f)
			goto ceiling2;
	}
	else if (0.0f < b) {
		goto ceiling2;
	}

	*q = ceilf(a / b);
	*r = m - b;
	return 0;

ceiling2:
	*q = ceilf(a / b);
	*r = m;
	return 0;
}

int float_ceiling_d_(double_float a, double_float b, double_float *q, double_float *r)
{
	double_float m;

	if (b == 0.0) {
		*q = *r = 0.0;
		return division_by_zero_d_(CONSTANT_COMMON_CEILING, a, b);
	}
	if (a == 0.0) {
		*q = *r = 0.0;
		return 0;
	}

	m = fmod(a, b);
	if (m == 0.0) {
		*q = a / b;
		*r = 0.0;
		return 0;
	}
	if (0.0 < a) {
		if (b < 0.0)
			goto ceiling2;
	}
	else if (0.0 < b) {
		goto ceiling2;
	}

	*q = ceil(a / b);
	*r = m - b;
	return 0;

ceiling2:
	*q = ceil(a / b);
	*r = m;
	return 0;
}

int float_ceiling_l_(long_float a, long_float b, long_float *q, long_float *r)
{
	long_float m;

	if (b == 0.0L) {
		*q = *r = 0.0L;
		return division_by_zero_l_(CONSTANT_COMMON_CEILING, a, b);
	}
	if (a == 0.0L) {
		*q = *r = 0.0L;
		return 0;
	}

	m = fmodl(a, b);
	if (m == 0.0L) {
		*q = a / b;
		*r = 0.0L;
		return 0;
	}
	if (0.0L < a) {
		if (b < 0.0L)
			goto ceiling2;
	}
	else if (0.0L < b) {
		goto ceiling2;
	}

	*q = ceill(a / b);
	*r = m - b;
	return 0;

ceiling2:
	*q = ceill(a / b);
	*r = m;
	return 0;
}

int float_ceiling1_s_(single_float a, single_float *q, single_float *r)
{
	single_float m;

	if (a == 0.0f) {
		*q = *r = 0.0f;
		return 0;
	}

	m = fmodf(a, 1.0f);
	if (m == 0.0f) {
		*q = a;
		*r = 0.0f;
		return 0;
	}
	if (0.0f < a)
		*r = m - 1.0f;
	else
		*r = m;

	*q = ceilf(a);
	return 0;
}

int float_ceiling1_d_(double_float a, double_float *q, double_float *r)
{
	double_float m;

	if (a == 0.0) {
		*q = *r = 0.0;
		return 0;
	}

	m = fmod(a, 1.0);
	if (m == 0.0) {
		*q = a;
		*r = 0.0;
		return 0;
	}
	if (0.0 < a)
		*r = m - 1.0;
	else
		*r = m;

	*q = ceil(a);
	return 0;
}

int float_ceiling1_l_(long_float a, long_float *q, long_float *r)
{
	long_float m;

	if (a == 0.0L) {
		*q = *r = 0.0L;
		return 0;
	}

	m = fmodl(a, 1.0L);
	if (m == 0.0L) {
		*q = a;
		*r = 0.0L;
		return 0;
	}
	if (0.0L < a)
		*r = m - 1.0L;
	else
		*r = m;

	*q = ceill(a);
	return 0;
}


/*
 *  truncate
 */
int float_truncate_s_(single_float a, single_float b,
		single_float *q, single_float *r)
{
	single_float m;

	if (b == 0.0f) {
		*q = *r = 0.0f;
		return division_by_zero_s_(CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (a == 0.0f) {
		*q = *r = 0.0f;
		return 0;
	}

	m = fmodf(a, b);
	if (m == 0.0f) {
		*q = a / b;
		*r = 0.0f;
		return 0;
	}
	*r = m;

	if (0.0f < a) {
		if (b < 0.0f)
			goto ceiling;
	}
	else if (0.0f < b) {
		goto ceiling;
	}
	*q = floorf(a / b);
	return 0;

ceiling:
	*q = ceilf(a / b);
	return 0;
}

int float_truncate_d_(double_float a, double_float b,
		double_float *q, double_float *r)
{
	double_float m;

	if (b == 0.0) {
		*q = *r = 0.0;
		return division_by_zero_d_(CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (a == 0.0) {
		*q = *r = 0.0;
		return 0;
	}

	m = fmod(a, b);
	if (m == 0.0) {
		*q = a / b;
		*r = 0.0;
		return 0;
	}
	*r = m;

	if (0.0 < a) {
		if (b < 0.0)
			goto ceiling;
	}
	else if (0.0 < b) {
		goto ceiling;
	}
	*q = floor(a / b);
	return 0;

ceiling:
	*q = ceil(a / b);
	return 0;
}

int float_truncate_l_(long_float a, long_float b,
		long_float *q, long_float *r)
{
	long_float m;

	if (b == 0.0L) {
		*q = *r = 0.0L;
		return division_by_zero_l_(CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (a == 0.0L) {
		*q = *r = 0.0L;
		return 0;
	}

	m = fmodl(a, b);
	if (m == 0.0L) {
		*q = a / b;
		*r = 0.0L;
		return 0;
	}
	*r = m;

	if (0.0L < a) {
		if (b < 0.0L)
			goto ceiling;
	}
	else if (0.0L < b) {
		goto ceiling;
	}
	*q = floorl(a / b);
	return 0;

ceiling:
	*q = ceill(a / b);
	return 0;
}

int float_truncate1_s_(single_float a, single_float *q, single_float *r)
{
	single_float m;

	if (a == 0.0f) {
		*q = *r = 0.0f;
		return 0;
	}

	m = fmodf(a, 1.0f);
	if (m == 0.0f) {
		*q = a;
		*r = 0.0f;
		return 0;
	}
	*q = (0.0f < a)? floorf(a): ceilf(a);
	*r = m;
	return 0;
}

int float_truncate1_d_(double_float a, double_float *q, double_float *r)
{
	double_float m;

	if (a == 0.0) {
		*q = *r = 0.0;
		return 0;
	}

	m = fmod(a, 1.0);
	if (m == 0.0) {
		*q = a;
		*r = 0.0;
		return 0;
	}
	*q = (0.0 < a)? floor(a): ceil(a);
	*r = m;
	return 0;
}

int float_truncate1_l_(long_float a, long_float *q, long_float *r)
{
	long_float m;

	if (a == 0.0L) {
		*q = *r = 0.0L;
		return 0;
	}

	m = fmodl(a, 1.0L);
	if (m == 0.0L) {
		*q = a;
		*r = 0.0L;
		return 0;
	}
	*q = (0.0L < a)? floorl(a): ceill(a);
	*r = m;
	return 0;
}


/*
 *  round
 */
static int float_round_even_s(single_float f)
{
	return fmodf(f, 2.0f) == 0.0f;
}

int float_round_s_(single_float a, single_float b, single_float *q, single_float *r)
{
	single_float i, f;

	if (b == 0.0f) {
		*q = *r = 0.0f;
		return division_by_zero_s_(CONSTANT_COMMON_ROUND, a, b);
	}
	f = modff(a / b, &i);
	if (0.0f <= f) {
		if (f < 0.5f)
			return float_floor_s_(a, b, q, r);
		else if (0.5f < f)
			return float_ceiling_s_(a, b, q, r);
		else if (float_round_even_s(i))
			return float_floor_s_(a, b, q, r);
		else
			return float_ceiling_s_(a, b, q, r);
	}
	else {
		if (-0.5f < f)
			return float_ceiling_s_(a, b, q, r);
		else if (f < -0.5f)
			return float_floor_s_(a, b, q, r);
		else if (float_round_even_s(i))
			return float_ceiling_s_(a, b, q, r);
		else
			return float_floor_s_(a, b, q, r);
	}
}

static int float_round_even_d(double_float f)
{
	return fmod(f, 2.0) == 0.0;
}

int float_round_d_(double_float a, double_float b, double_float *q, double_float *r)
{
	double_float i, f;

	if (b == 0.0) {
		*q = *r = 0.0;
		return division_by_zero_d_(CONSTANT_COMMON_ROUND, a, b);
	}
	f = modf(a / b, &i);
	if (0.0 <= f) {
		if (f < 0.5)
			return float_floor_d_(a, b, q, r);
		else if (0.5 < f)
			return float_ceiling_d_(a, b, q, r);
		else if (float_round_even_d(i))
			return float_floor_d_(a, b, q, r);
		else
			return float_ceiling_d_(a, b, q, r);
	}
	else {
		if (-0.5 < f)
			return float_ceiling_d_(a, b, q, r);
		else if (f < -0.5)
			return float_floor_d_(a, b, q, r);
		else if (float_round_even_d(i))
			return float_ceiling_d_(a, b, q, r);
		else
			return float_floor_d_(a, b, q, r);
	}
}

static int float_round_even_l(long_float f)
{
	return fmodl(f, 2.0L) == 0.0L;
}

int float_round_l_(long_float a, long_float b, long_float *q, long_float *r)
{
	long_float i, f;

	if (b == 0.0L) {
		*q = *r = 0.0L;
		return division_by_zero_l_(CONSTANT_COMMON_ROUND, a, b);
	}
	f = modfl(a / b, &i);
	if (0.0L <= f) {
		if (f < 0.5L)
			return float_floor_l_(a, b, q, r);
		else if (0.5L < f)
			return float_ceiling_l_(a, b, q, r);
		else if (float_round_even_l(i))
			return float_floor_l_(a, b, q, r);
		else
			return float_ceiling_l_(a, b, q, r);
	}
	else {
		if (-0.5L < f)
			return float_ceiling_l_(a, b, q, r);
		else if (f < -0.5L)
			return float_floor_l_(a, b, q, r);
		else if (float_round_even_l(i))
			return float_ceiling_l_(a, b, q, r);
		else
			return float_floor_l_(a, b, q, r);
	}
}

int float_round1_s_(single_float a, single_float *q, single_float *r)
{
	single_float i, f;

	f = modff(a, &i);
	if (0.0f <= f) {
		if (f < 0.5f)
			return float_floor1_s_(a, q, r);
		else if (0.5f < f)
			return float_ceiling1_s_(a, q, r);
		else if (float_round_even_s(i))
			return float_floor1_s_(a, q, r);
		else
			return float_ceiling1_s_(a, q, r);
	}
	else {
		if (-0.5f < f)
			return float_ceiling1_s_(a, q, r);
		else if (f < -0.5f)
			return float_floor1_s_(a, q, r);
		else if (float_round_even_s(i))
			return float_ceiling1_s_(a, q, r);
		else
			return float_floor1_s_(a, q, r);
	}
}

int float_round1_d_(double_float a, double_float *q, double_float *r)
{
	double_float i, f;

	f = modf(a, &i);
	if (0.0 <= f) {
		if (f < 0.5)
			return float_floor1_d_(a, q, r);
		else if (0.5 < f)
			return float_ceiling1_d_(a, q, r);
		else if (float_round_even_d(i))
			return float_floor1_d_(a, q, r);
		else
			return float_ceiling1_d_(a, q, r);
	}
	else {
		if (-0.5 < f)
			return float_ceiling1_d_(a, q, r);
		else if (f < -0.5)
			return float_floor1_d_(a, q, r);
		else if (float_round_even_d(i))
			return float_ceiling1_d_(a, q, r);
		else
			return float_floor1_d_(a, q, r);
	}
}

int float_round1_l_(long_float a, long_float *q, long_float *r)
{
	long_float i, f;

	f = modfl(a, &i);
	if (0.0L <= f) {
		if (f < 0.5L)
			return float_floor1_l_(a, q, r);
		else if (0.5L < f)
			return float_ceiling1_l_(a, q, r);
		else if (float_round_even_l(i))
			return float_floor1_l_(a, q, r);
		else
			return float_ceiling1_l_(a, q, r);
	}
	else {
		if (-0.5L < f)
			return float_ceiling1_l_(a, q, r);
		else if (f < -0.5L)
			return float_floor1_l_(a, q, r);
		else if (float_round_even_l(i))
			return float_ceiling1_l_(a, q, r);
		else
			return float_floor1_l_(a, q, r);
	}
}


/*
 *  fixnum
 */
int float_floor_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem)
{
	fixnum q, r;

	if (b == 0) {
		*quot = *rem = 0;
		return division_by_zero_f_(CONSTANT_COMMON_FLOOR, a, b);
	}
	Check(a == FIXNUM_MIN && b == -1, "floor fixnum overflow.");

	q = a / b;
	r = a % b;
	if (r == 0) {
		*quot = q;
		*rem = 0;
		return 0;
	}
	if (0 < a) {
		if (b < 0)
			goto floor2;
	}
	else if (0 < b) {
		goto floor2;
	}
	*quot = q;
	*rem = r;
	return 0;

floor2:
	*quot = q - 1;
	*rem = r + b;
	return 0;
}

int float_ceiling_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem)
{
	fixnum q, r;

	if (b == 0) {
		*quot = *rem = 0;
		return division_by_zero_f_(CONSTANT_COMMON_CEILING, a, b);
	}
	Check(a == FIXNUM_MIN && b == -1, "ceiling fixnum overflow.");

	q = a / b;
	r = a % b;
	if (r == 0) {
		*quot = q;
		*rem = 0;
		return 0;
	}
	if (0 < a) {
		if (b < 0)
			goto ceiling2;
	}
	else if (0 < b) {
		goto ceiling2;
	}
	*quot = q + 1;
	*rem = r - b;
	return 0;

ceiling2:
	*quot = q;
	*rem = r;
	return 0;
}

int float_truncate_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem)
{
	if (b == 0) {
		*quot = *rem = 0;
		return division_by_zero_f_(CONSTANT_COMMON_TRUNCATE, a, b);
	}
	Check(a == FIXNUM_MIN && b == -1, "truncate fixnum overflow.");

	*quot = a / b;
	*rem = a % b;
	return 0;
}

int float_round_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem)
{
	fixnum q, r, b2;

	/* error */
	if (b == 0) {
		*quot = *rem = 0;
		return division_by_zero_f_(CONSTANT_COMMON_ROUND, a, b);
	}

	/* |b| = 1 */
	if (b == 1) {
		*quot = a;
		*rem = 0;
		return 0;
	}
	if (b == -1) {
		Check(a == FIXNUM_MIN && b == -1, "round fixnum overflow.");
		*quot = -a;
		*rem = 0;
		return 0;
	}

	/* r = 0 */
	q = a / b;
	r = a % b;
	if (r == 0) {
		*quot = q;
		*rem = 0;
		return 0;
	}

	/* plus, minus */
	if (0 < a) {
		if (0 < b) {
			/* a:plus, b:plus, q:plus, r:plus */
			b2 = b / 2;
			if (r < b2)
				return float_truncate_f_(a, b, quot, rem);
			if (b2 < r)
				return float_ceiling_f_(a, b, quot, rem);
			if (b % 2)
				return float_truncate_f_(a, b, quot, rem);
			else if (q % 2)
				return float_ceiling_f_(a, b, quot, rem);
			else
				return float_truncate_f_(a, b, quot, rem);
		}
		else {
			/* a:plus, b:minus, q:minus, r:plus */
			b2 = -(b / 2);
			if (r < b2)
				return float_truncate_f_(a, b, quot, rem);
			if (b2 < r)
				return float_floor_f_(a, b, quot, rem);
			if (b % 2)
				return float_truncate_f_(a, b, quot, rem);
			else if (q % 2)
				return float_floor_f_(a, b, quot, rem);
			else
				return float_truncate_f_(a, b, quot, rem);
		}
	}
	else {
		if (0 < b) {
			/* a:minus, b:plus, q:minus, r:minus */
			b2 = -(b / 2);
			if (b2 < r)
				return float_truncate_f_(a, b, quot, rem);
			if (r < b2)
				return float_floor_f_(a, b, quot, rem);
			if (b % 2)
				return float_truncate_f_(a, b, quot, rem);
			else if (q % 2)
				return float_floor_f_(a, b, quot, rem);
			else
				return float_truncate_f_(a, b, quot, rem);
		}
		else {
			/* a:minus, b:minus, q:plus, r:minus */
			b2 = b / 2;
			if (r < b2)
				return float_ceiling_f_(a, b, quot, rem);
			if (b2 < r)
				return float_truncate_f_(a, b, quot, rem);
			if (b % 2)
				return float_truncate_f_(a, b, quot, rem);
			else if (q % 2)
				return float_ceiling_f_(a, b, quot, rem);
			else
				return float_truncate_f_(a, b, quot, rem);
		}
	}
}

int float_floor_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_heap(quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_floor_f_(a, b, &a, &b));
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}

	return 0;
}

int float_ceiling_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_heap(quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_ceiling_f_(a, b, &a, &b));
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}

	return 0;
}

int float_truncate_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_heap(quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_truncate_f_(a, b, &a, &b));
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}

	return 0;
}

int float_round_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_heap(quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_round_f_(a, b, &a, &b));
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}

	return 0;
}

int float_ffloor_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		single_float_heap(quot, -((single_float)FIXNUM_UMIN));
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_floor_f_(a, b, &a, &b));
		single_float_heap(quot, (single_float)a);
		fixnum_heap(rem, b);
	}

	return 0;
}

int float_fceiling_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		single_float_heap(quot, -((single_float)FIXNUM_UMIN));
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_ceiling_f_(a, b, &a, &b));
		single_float_heap(quot, (single_float)a);
		fixnum_heap(rem, b);
	}

	return 0;
}

int float_ftruncate_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		single_float_heap(quot, -((single_float)FIXNUM_UMIN));
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_truncate_f_(a, b, &a, &b));
		single_float_heap(quot, (single_float)a);
		fixnum_heap(rem, b);
	}

	return 0;
}

int float_fround_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		single_float_heap(quot, -((single_float)FIXNUM_UMIN));
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_round_f_(a, b, &a, &b));
		single_float_heap(quot, (single_float)a);
		fixnum_heap(rem, b);
	}

	return 0;
}


/*
 *  bignum
 */
struct divrem_struct {
	LocalRoot local;
	LocalStack stack;
	addr quot, rem;
	addr a, b, pos;
	int sign;
};

static void divrem_struct_initialize(LocalRoot local,
		struct divrem_struct *ptr, addr a, addr b)
{
	push_local(local, &(ptr->stack));
	ptr->local = local;
	ptr->quot = ptr->rem = NULL;
	ptr->a = a;
	ptr->b = b;
	ptr->pos = NULL;
}

static void divrem_struct_initialize1(LocalRoot local,
		struct divrem_struct *ptr, addr pos)
{
	addr numer, denom;

	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	divrem_struct_initialize(local, ptr, numer, denom);
	ptr->pos = pos;
}

static void divrem_struct_call(struct divrem_struct *ptr)
{
	int sign, sign1, sign2;
	addr q, r;

	if (ptr->quot == NULL) {
		GetSignBignum(ptr->a, &sign1);
		GetSignBignum(ptr->b, &sign2);
		sign = SignMulti(sign1, sign2);
		divrem_bigdata_local(ptr->local, &q, &r, ptr->a, ptr->b);
		SetSignBignum(q, sign);
		SetSignBignum(r, sign1);
		ptr->quot = q;
		ptr->rem = r;
		ptr->sign = sign;
	}
}

static void divrem_struct_integer(struct divrem_struct *ptr, addr *quot, addr *rem)
{
	bignum_result_heap(ptr->quot, quot);
	*rem = ptr->rem;
	rollback_local(ptr->local, ptr->stack);
#ifdef LISP_DEBUG
	ptr->local = (LocalRoot)Unbound;
#endif
}

static void rem_struct_integer(struct divrem_struct *ptr, addr *rem)
{
	*rem = ptr->rem;
	rollback_local(ptr->local, ptr->stack);
#ifdef LISP_DEBUG
	ptr->local = (LocalRoot)Unbound;
#endif
}

static int divrem_struct_float_(struct divrem_struct *ptr, addr *quot, addr *rem)
{
	Return(single_float_bignum_heap_(quot, ptr->quot));
	*rem = ptr->rem;
	rollback_local(ptr->local, ptr->stack);
#ifdef LISP_DEBUG
	ptr->local = (LocalRoot)Unbound;
#endif

	return 0;
}

static void divrem_struct_close(struct divrem_struct *ptr)
{
	rollback_local(ptr->local, ptr->stack);
#ifdef LISP_DEBUG
	ptr->local = (LocalRoot)Unbound;
#endif
}

static void float_floor_bb(struct divrem_struct *ptr)
{
	divrem_struct_call(ptr);
	if (zerop_bignum(ptr->rem)) {
		fixnum_heap(&(ptr->rem), 0);
		return;
	}
	if (IsMinus(ptr->sign)) {
		plus_bv_bignum_local(ptr->local, ptr->quot, -1, &(ptr->quot));
		plus_bb_bignum_local(ptr->local, ptr->b, ptr->rem, &(ptr->rem));
	}
	bignum_result_heap(ptr->rem, &(ptr->rem));
}

int float_floor_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_floor_bb(&str);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

int float_ffloor_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FFLOOR, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_floor_bb(&str);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}

static void float_ceiling_bb(struct divrem_struct *ptr)
{
	divrem_struct_call(ptr);
	if (zerop_bignum(ptr->rem)) {
		fixnum_heap(&(ptr->rem), 0);
		return;
	}
	if (IsPlus(ptr->sign)) {
		plus_bv_bignum_local(ptr->local, ptr->quot, 1, &(ptr->quot));
		minus_bb_bignum_local(ptr->local, ptr->rem, ptr->b, &(ptr->rem));
	}
	bignum_result_heap(ptr->rem, &(ptr->rem));
}

int float_ceiling_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_CEILING, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_ceiling_bb(&str);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

int float_fceiling_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FCEILING, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_ceiling_bb(&str);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}

static void float_truncate_bb(struct divrem_struct *ptr)
{
	divrem_struct_call(ptr);
	bignum_result_heap(ptr->rem, &(ptr->rem));
}

int float_truncate_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_truncate_bb(&str);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

int float_ftruncate_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FTRUNCATE, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_truncate_bb(&str);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}

static int float_round_bb_check(struct divrem_struct *ptr)
{
	int sign, check;
	addr b;
	LocalRoot local;
	LocalStack stack;

	divrem_struct_call(ptr);
	if (zerop_bignum(ptr->rem))
		return 0;

	local = ptr->local;
	push_local(local, &stack);
	division2_bigdata_alloc(local, &b, ptr->b);
	GetSignBignum(ptr->quot, &sign);
	if (IsPlus(sign)) {
		check = compare_bigdata(ptr->rem, b);
		if (check == 0) {
			if (evenp_bignum(ptr->b))
				check = evenp_bignum(ptr->quot)? -1: 1;
			else
				check = -1;
		}
	}
	else {
		check = compare_bigdata(b, ptr->rem);
		if (check == 0) {
			if (evenp_bignum(ptr->b))
				check = evenp_bignum(ptr->quot)? 1: -1;
			else
				check = 1;
		}

	}
	rollback_local(local, stack);

	return check;
}

static void float_round_bb(struct divrem_struct *ptr)
{
	int check;

	Check(zerop_bignum(ptr->a), "zero error: a");
	Check(zerop_bignum(ptr->b), "zero error: b");

	check = float_round_bb_check(ptr);
	if (check < 0)
		float_floor_bb(ptr);
	else if (0 < check)
		float_ceiling_bb(ptr);
	else
		fixnum_heap(&(ptr->rem), 0);
}

int float_round_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_ROUND, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_round_bb(&str);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

int float_fround_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FROUND, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_round_bb(&str);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}


/*
 *  ratio
 */
static void float_floor1_nosign(struct divrem_struct *ptr, int sign1, int sign2)
{
	int check;
	addr quot, rem, temp;

	check = compare_bigdata(ptr->a, ptr->b);
	Check(check == 0, "ratio error");
	if (check <= 0) {
		bignum_zero_local(ptr->local, &quot);
		ratio_copy_nosign_heap(&rem, ptr->pos);
		SetSignRatio(rem, sign2);
	}
	else {
		divrem_struct_call(ptr);
		Check(zerop_bignum(ptr->rem), "ratio zero error");
		/* quot */
		bignum_copy_nosign_local(ptr->local, &quot, ptr->quot);
		SetSignBignum(quot, sign1);
		/* rem */
		bignum_throw_heap(ptr->rem, &rem);
		bignum_throw_heap(ptr->b, &temp);
		make_ratio_alloc_unsafe(NULL, &rem, sign2, rem, temp);
	}
	ptr->quot = quot;
	ptr->rem = rem;
}

static void float_ceiling1_nosign(struct divrem_struct *ptr, int sign1, int sign2)
{
	int check;
	addr quot, rem, temp;

	check = compare_bigdata(ptr->a, ptr->b);
	Check(check == 0, "ratio error");
	if (check <= 0) {
		bignum_value_local(ptr->local, &quot, sign1, 1);
		minus_bigdata_alloc(NULL, ptr->b, ptr->a, &rem);
		bignum_throw_heap(ptr->b, &temp);
		make_ratio_alloc_unsafe(NULL, &rem, sign2, rem, temp);
	}
	else {
		divrem_struct_call(ptr);
		Check(zerop_bignum(ptr->rem), "ratio zero error");
		/* quot */
		plusvalue_bigdata_alloc(ptr->local, ptr->quot, sign1, 1, &quot);
		/* rem */
		minus_bigdata_alloc(NULL, ptr->b, ptr->rem, &rem);
		bignum_throw_heap(ptr->b, &temp);
		make_ratio_alloc_unsafe(NULL, &rem, sign2, rem, temp);
	}
	ptr->quot = quot;
	ptr->rem = rem;
}

static void float_floor1_r(struct divrem_struct *ptr)
{
	int sign;

	GetSignRatio(ptr->pos, &sign);
	if (IsPlus(sign))
		float_floor1_nosign(ptr, SignPlus, SignPlus);
	else
		float_ceiling1_nosign(ptr, SignMinus, SignPlus);
}

int float_floor1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	float_floor1_r(&str);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

static void float_ceiling1_r(struct divrem_struct *ptr)
{
	int sign;

	GetSignRatio(ptr->pos, &sign);
	if (IsPlus(sign))
		float_ceiling1_nosign(ptr, SignPlus, SignMinus);
	else
		float_floor1_nosign(ptr, SignMinus, SignMinus);
}

int float_ceiling1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	float_ceiling1_r(&str);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

int float_truncate1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	int sign;
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	GetSignRatio(pos, &sign);
	float_floor1_nosign(&str, sign, sign);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

static int float_round1_half(addr numer, addr denom)
{
	return equal_value_nosign_bignum(numer, 1)
		&& equal_value_nosign_bignum(denom, 2);
}

static void float_round1_r(struct divrem_struct *ptr)
{
	addr denom2;

	divrem_struct_call(ptr);
	if (float_round1_half(ptr->rem, ptr->b)) {
		/* rem = 1/2 */
		if (plusp_ratio(ptr->pos)) {
			if (evenp_bignum(ptr->quot))
				float_floor1_r(ptr);
			else
				float_ceiling1_r(ptr);
		}
		else {
			if (evenp_bignum(ptr->quot))
				float_ceiling1_r(ptr);
			else
				float_floor1_r(ptr);
		}
	}
	else {
		/* otherwise */
		division2_bigdata_alloc(ptr->local, &denom2, ptr->b);
		if (plusp_ratio(ptr->pos)) {
			if (compare_bb_real(ptr->rem, denom2) <= 0)
				float_floor1_r(ptr);
			else
				float_ceiling1_r(ptr);
		}
		else {
			if (compare_bb_real(ptr->rem, denom2) <= 0)
				float_ceiling1_r(ptr);
			else
				float_floor1_r(ptr);
		}
	}
}

int float_round1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	float_round1_r(&str);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

int float_ffloor1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	float_floor1_r(&str);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}

int float_fceiling1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	float_ceiling1_r(&str);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}

int float_ftruncate1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	int sign;
	struct divrem_struct str;

	Check(local == NULL, "local error");
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	GetSignRatio(pos, &sign);
	float_floor1_nosign(&str, sign, sign);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}

int float_fround1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	float_round1_r(&str);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}


/*
 *  bignum-ratio
 */
static int float_floor_br_plus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	addr numer, denom;

	/*   a     b.numer       a * b.denom
	 *  --- / ---------  =  -------------
	 *   1     b.denom         b.numer
	 */
	GetNumerRatio(b, &numer);
	GetDenomRatio(b, &denom);
	bignum_copy_nosign_local(local, &a, a);
	bignum_copy_nosign_local(local, &b, numer);
	reduction_local(local, a, b);
	multi_bigdata_alloc(local, a, denom, &a);
	if (equal_value_nosign_bignum(b, 1)) {
		SetSignBignum(a, sign);
		*ret = a;
		return 0;
	}
	else {
		divrem_bigdata_local(local, ret, &b, a, b);
		SetSignBignum(*ret, sign);
		return 1;
	}
}

static int float_floor_br_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = float_floor_br_plus(local, ret, sign, a, b);
	if (check)
		plus_bv_bignum_local(local, *ret, -1, ret);

	return check;
}

static void remainder_br_ratio(LocalRoot local, addr *ret, addr a, addr b, addr q)
{
	/* remainder = a - quotient*b */
	multi_br_ratio_local(local, q, b, &b);
	minus_br_ratio_local(local, a, b, ret);
}

static void float_floor_br(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignBignum(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = float_floor_br_plus(local, &q, sign, a, b);
	else
		check = float_floor_br_minus(local, &q, sign, a, b);
	if (check) {
		remainder_br_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_floor_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_floor_br(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static int float_ceiling_br_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = float_floor_br_plus(local, ret, sign, a, b);
	if (check)
		plus_bv_bignum_local(local, *ret, 1, ret);

	return check;
}

static void float_ceiling_br(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignBignum(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = float_ceiling_br_minus(local, &q, sign, a, b);
	else
		check = float_floor_br_plus(local, &q, sign, a, b);
	if (check) {
		remainder_br_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_ceiling_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_CEILING, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_ceiling_br(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static void float_truncate_br(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r;

	GetSignBignum(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (float_floor_br_plus(local, &q, sign, a, b)) {
		remainder_br_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_truncate_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_truncate_br(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static int float_round_br_check(LocalRoot local, addr a, addr b)
{
	int check, sign1, sign2;
	addr numer, denom;
	LocalStack stack;
	struct divrem_struct str;

	Check(zerop_bignum(a), "zero error: left");
	Check(zerop_ratio(b), "zero error: right");
	GetSignBignum(a, &sign1);
	GetSignRatio(b, &sign2);
	GetNumerRatio(b, &numer);
	GetDenomRatio(b, &denom);

	push_local(local, &stack);
	multi_bigdata_alloc(local, a, denom, &a);
	bignum_copy_nosign_alloc(local, &b, numer);
	SetSignBignum(a, sign1);
	SetSignBignum(b, sign2);

	divrem_struct_initialize(local, &str, a, b);
	check = float_round_bb_check(&str);
	divrem_struct_close(&str);
	rollback_local(local, stack);

	return check;
}

static void float_round_br(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int check;

	check = float_round_br_check(local, a, b);
	if (check < 0)
		float_floor_br(local, quot, rem, a, b);
	else
		float_ceiling_br(local, quot, rem, a, b);
}

int float_round_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_ROUND, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_round_br(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

int float_ffloor_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FFLOOR, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_floor_br(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_fceiling_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FCEILING, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_ceiling_br(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_ftruncate_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FTRUNCATE, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_truncate_br(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_fround_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FROUND, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_round_br(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}


/*
 *  ratio-bignum
 */
static int float_floor_rb_plus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	addr numer, denom;

	/*   a.numer     b         a.numer
	 *  --------- / ---  =  -------------
	 *   a.denom     1       a.denom * b
	 */
	GetNumerRatio(a, &numer);
	GetDenomRatio(a, &denom);
	bignum_copy_nosign_local(local, &a, numer);
	bignum_copy_nosign_local(local, &b, b);
	reduction_local(local, a, b);
	multi_bigdata_alloc(local, b, denom, &b);
	if (equal_value_nosign_bignum(b, 1)) {
		SetSignBignum(a, sign);
		*ret = a;
		return 0;
	}
	else {
		divrem_bigdata_local(local, ret, &b, a, b);
		SetSignBignum(*ret, sign);
		return 1;
	}
}

static int float_floor_rb_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = float_floor_rb_plus(local, ret, sign, a, b);
	if (check)
		plus_bv_bignum_local(local, *ret, -1, ret);

	return check;
}

static void remainder_rb_ratio(LocalRoot local, addr *ret, addr a, addr b, addr q)
{
	/* remainder = a - quotient*b */
	multi_bb_bignum_local(local, q, b, &b);
	minus_rb_ratio_local(local, a, b, ret);
}

static void float_floor_rb(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = float_floor_rb_plus(local, &q, sign, a, b);
	else
		check = float_floor_rb_minus(local, &q, sign, a, b);
	if (check) {
		remainder_rb_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_floor_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_floor_rb(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static int float_ceiling_rb_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = float_floor_rb_plus(local, ret, sign, a, b);
	if (check)
		plus_bv_bignum_local(local, *ret, 1, ret);

	return check;
}

static void float_ceiling_rb(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = float_ceiling_rb_minus(local, &q, sign, a, b);
	else
		check = float_floor_rb_plus(local, &q, sign, a, b);
	if (check) {
		remainder_rb_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_ceiling_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_CEILING, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_ceiling_rb(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static void float_truncate_rb(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (float_floor_rb_plus(local, &q, sign, a, b)) {
		remainder_rb_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_truncate_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_truncate_rb(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static int float_round_rb_check(LocalRoot local, addr a, addr b)
{
	int check, sign1, sign2;
	addr numer, denom;
	LocalStack stack;
	struct divrem_struct str;

	Check(zerop_ratio(a), "zero error: left");
	Check(zerop_bignum(b), "zero error: right");
	GetSignRatio(a, &sign1);
	GetSignBignum(b, &sign2);
	GetNumerRatio(a, &numer);
	GetDenomRatio(a, &denom);

	push_local(local, &stack);
	bignum_copy_nosign_alloc(local, &a, numer);
	multi_bigdata_alloc(local, b, denom, &b);
	SetSignBignum(a, sign1);
	SetSignBignum(b, sign2);

	divrem_struct_initialize(local, &str, a, b);
	check = float_round_bb_check(&str);
	divrem_struct_close(&str);
	rollback_local(local, stack);

	return check;
}

static void float_round_rb(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int check;

	check = float_round_rb_check(local, a, b);
	if (check < 0)
		float_floor_rb(local, quot, rem, a, b);
	else
		float_ceiling_rb(local, quot, rem, a, b);
}

int float_round_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_ROUND, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_round_rb(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

int float_ffloor_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FFLOOR, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_floor_rb(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_fceiling_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FCEILING, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_ceiling_rb(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_ftruncate_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FTRUNCATE, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_truncate_rb(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_fround_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FROUND, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_round_rb(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}


/*
 *  ratio-ratio
 */
static int float_floor_rr_plus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	addr numer1, denom1, numer2, denom2;

	/*   a.numer     b.numer       a.numer * b.denom
	 *  --------- / ---------  =  -------------------
	 *   a.denom     b.denom       a.denom * b.numer
	 */
	GetNumerRatio(a, &numer1);
	GetDenomRatio(a, &denom1);
	GetNumerRatio(b, &numer2);
	GetDenomRatio(b, &denom2);
	bignum_copy_nosign_local(local, &numer1, numer1);
	bignum_copy_nosign_local(local, &denom1, denom1);
	bignum_copy_nosign_local(local, &numer2, numer2);
	bignum_copy_nosign_local(local, &denom2, denom2);
	reduction_local(local, numer1, numer2);
	reduction_local(local, denom1, denom2);
	multi_bigdata_alloc(local, numer1, denom2, &a);
	multi_bigdata_alloc(local, denom1, numer2, &b);
	if (equal_value_nosign_bignum(b, 1)) {
		SetSignBignum(a, sign);
		*ret = a;
		return 0;
	}
	else {
		divrem_bigdata_local(local, ret, &b, a, b);
		SetSignBignum(*ret, sign);
		return 1;
	}
}

static int float_floor_rr_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = float_floor_rr_plus(local, ret, sign, a, b);
	if (check)
		plus_bv_bignum_local(local, *ret, -1, ret);

	return check;
}

static void remainder_rr_ratio(LocalRoot local, addr *ret, addr a, addr b, addr q)
{
	/* remainder = a - quotient*b */
	multi_br_ratio_local(local, q, b, &b);
	minus_rr_ratio_local(local, a, b, ret);
}

static void float_floor_rr(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = float_floor_rr_plus(local, &q, sign, a, b);
	else
		check = float_floor_rr_minus(local, &q, sign, a, b);
	if (check) {
		remainder_rr_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_floor_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_floor_rr(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static int float_ceiling_rr_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = float_floor_rr_plus(local, ret, sign, a, b);
	if (check)
		plus_bv_bignum_local(local, *ret, 1, ret);

	return check;
}

static void float_ceiling_rr(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = float_ceiling_rr_minus(local, &q, sign, a, b);
	else
		check = float_floor_rr_plus(local, &q, sign, a, b);
	if (check) {
		remainder_rr_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_ceiling_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_CEILING, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_ceiling_rr(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static void float_truncate_rr(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (float_floor_rr_plus(local, &q, sign, a, b)) {
		remainder_rr_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_truncate_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_truncate_rr(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static int float_round_rr_check(LocalRoot local, addr a, addr b)
{
	int check, sign1, sign2;
	addr numer1, denom1, numer2, denom2, reduct1, reduct2;
	LocalStack stack;
	struct divrem_struct str;

	Check(zerop_ratio(a), "zero error: left");
	Check(zerop_ratio(b), "zero error: right");

	GetSignRatio(a, &sign1);
	GetSignRatio(b, &sign2);
	GetNumerRatio(a, &numer1);
	GetDenomRatio(a, &denom1);
	GetNumerRatio(b, &numer2);
	GetDenomRatio(b, &denom2);

	push_local(local, &stack);
	if (equal_bigdata(denom1, denom2)) {
		/*
		 *  numer1 + numer2
		 *  ---------------
		 *      denom1
		 */
		bignum_copy_nosign_alloc(local, &a, numer1);
		bignum_copy_nosign_alloc(local, &b, numer2);
	}
	else {
		/*
		 *  numer1   numer2
		 *  ------ + ------
		 *  denom1   denom2
		 */

		/*
		 *  (denom1 denom2) reduction-> (reduct1 reduct2)
		 */
		bignum_copy_local(local, &reduct1, denom1);
		bignum_copy_local(local, &reduct2, denom2);
		reduction_local(local, reduct1, reduct2);

		/*
		 *  numer1*reduct2    numer2*reduct1    numer1*reduct2 + numer2*reduct1
		 *  -------------- + ---------------- = -------------------------------
		 *  denom1*reduct2   (denom2*reduct1)           denom1 * reduct2
		 */
		multi_bigdata_alloc(local, numer1, reduct2, &a);
		multi_bigdata_alloc(local, numer2, reduct1, &b);
	}

	SetSignBignum(a, sign1);
	SetSignBignum(b, sign2);
	divrem_struct_initialize(local, &str, a, b);
	check = float_round_bb_check(&str);
	divrem_struct_close(&str);
	rollback_local(local, stack);

	return check;
}

static void float_round_rr(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int check;

	check = float_round_rr_check(local, a, b);
	if (check < 0)
		float_floor_rr(local, quot, rem, a, b);
	else
		float_ceiling_rr(local, quot, rem, a, b);
}

int float_round_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_ROUND, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_round_rr(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

int float_ffloor_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FFLOOR, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_floor_rr(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_fceiling_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FCEILING, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_ceiling_rr(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_ftruncate_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FTRUNCATE, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_truncate_rr(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_fround_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FROUND, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_round_rr(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}


/*
 *  mod -> floor
 */
int float_mod_fixnum_(addr *ret, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		fixnum_heap(ret, 0);
	}
	else {
		Return(float_floor_f_(a, b, &a, &b));
		fixnum_heap(ret, b);
	}

	return 0;
}

int float_mod_bignum_(LocalRoot local, addr *ret, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_floor_bb(&str);
	rem_struct_integer(&str, ret);

	return 0;
}

int float_mod_br_ratio_(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	push_local(local, &stack);
	float_floor_br(local, &a, ret, a, b);
	rollback_local(local, stack);

	return 0;
}

int float_mod_rb_ratio_(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	push_local(local, &stack);
	float_floor_rb(local, &a, ret, a, b);
	rollback_local(local, stack);

	return 0;
}

int float_mod_rr_ratio_(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	push_local(local, &stack);
	float_floor_rr(local, &a, ret, a, b);
	rollback_local(local, stack);

	return 0;
}

int mod_number_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	enum MathType type;
	single_float vs, igs;
	double_float vd, igd;
	long_float vl, igl;
	struct mathreal2_struct str;

	Return(getmathreal2_addr_(&str, a, b, &type));
	switch (type) {
		case MathType_single:
			Return(float_floor_s_(str.v.s.a, str.v.s.b, &igs, &vs));
			return single_float_check_heap_(ret, vs);

		case MathType_double:
			Return(float_floor_d_(str.v.d.a, str.v.d.b, &igd, &vd));
			return double_float_check_heap_(ret, vd);

		case MathType_long:
			Return(float_floor_l_(str.v.l.a, str.v.l.b, &igl, &vl));
			return long_float_check_heap_(ret, vl);

		case MathType_rational:
			return mod_rational_common_(local, a, b, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}


/*
 *  rem -> truncate
 */
int float_rem_fixnum_(addr *ret, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		fixnum_heap(ret, 0);
	}
	else {
		Return(float_truncate_f_(a, b, &a, &b));
		fixnum_heap(ret, b);
	}

	return 0;
}

int float_rem_bignum_(LocalRoot local, addr *ret, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_truncate_bb(&str);
	rem_struct_integer(&str, ret);

	return 0;
}

int float_rem_br_ratio_(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	push_local(local, &stack);
	float_truncate_br(local, &a, ret, a, b);
	rollback_local(local, stack);

	return 0;
}

int float_rem_rb_ratio_(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	push_local(local, &stack);
	float_truncate_rb(local, &a, ret, a, b);
	rollback_local(local, stack);

	return 0;
}

int float_rem_rr_ratio_(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	push_local(local, &stack);
	float_truncate_rr(local, &a, ret, a, b);
	rollback_local(local, stack);

	return 0;
}

int rem_number_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	enum MathType type;
	single_float vs, igs;
	double_float vd, igd;
	long_float vl, igl;
	struct mathreal2_struct str;

	Return(getmathreal2_addr_(&str, a, b, &type));
	switch (type) {
		case MathType_single:
			Return(float_truncate_s_(str.v.s.a, str.v.s.b, &igs, &vs));
			return single_float_check_heap_(ret, vs);

		case MathType_double:
			Return(float_truncate_d_(str.v.d.a, str.v.d.b, &igd, &vd));
			return double_float_check_heap_(ret, vd);

		case MathType_long:
			Return(float_truncate_l_(str.v.l.a, str.v.l.b, &igl, &vl));
			return long_float_check_heap_(ret, vl);

		case MathType_rational:
			return rem_rational_common_(local, a, b, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}


/*
 *  integer-heap
 */
void single_float_integer_heap(LocalRoot local, addr *ret, single_float v)
{
	addr pos;
	LocalStack stack;

	push_local(local, &stack);
	bignum_single_float_unsafe(local, v, 0, &pos);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

void double_float_integer_heap(LocalRoot local, addr *ret, double_float v)
{
	addr pos;
	LocalStack stack;

	push_local(local, &stack);
	bignum_double_float_unsafe(local, v, 0, &pos);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

void long_float_integer_heap(LocalRoot local, addr *ret, long_float v)
{
	addr pos;
	LocalStack stack;

	push_local(local, &stack);
	bignum_long_float_unsafe(local, v, 0, &pos);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#ifdef __clang__
#pragma clang diagnostic pop
#endif
