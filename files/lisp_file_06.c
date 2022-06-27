/*
 *  npt -- ANSI Common Lisp Programming Language.
 *    https://github.com/nptcl/npt
 *    https://github.com/nptcl/npt-amalgamation
 *
 *  File: lisp_file_06.c
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
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lisp_file.h"


/************************************************************
 *  loop_variables.c
 ************************************************************/

/*
 *  with
 */
static int loop_with_list_bind_(Execute ptr,
		addr list, addr *var, addr *type, addr *value)
{
	Return(list_bind_(list, var, type, value, NULL));
	if (*value == Unbound)
		return loop_bind_initial_list_(ptr, *var, *type, value);

	return 0;
}

static void loop_with_single_bind(addr *ret, addr var, addr type, addr value)
{
	addr qvar, dbind, bind, let, declare, dtype;

	Check(value == Unbound, "unbound error");
	if (! consp(var)) {
		/* `(let ((,var ,value))
		 *    (declare (type ,var ,type))
		 *    ,form)
		 */
		GetConst(COMMON_LET, &let);
		if (value == Nil) {
			list_heap(&value, var, NULL);
		}
		else {
			list_heap(&value, var, value, NULL);
			list_heap(&value, value, NULL);
		}
		if (type == Unbound) {
			list_heap(ret, let, value, NULL);
		}
		else {
			GetConst(COMMON_DECLARE, &declare);
			GetConst(COMMON_TYPE, &dtype);
			list_heap(&dtype, dtype, type, var, NULL);
			list_heap(&declare, declare, dtype, NULL);
			list_heap(ret, let, value, declare, NULL);
		}
	}
	else {
		/* `(destructuring-bind ,var (lisp-system::loop-bind ,var ,type ,value)
		 *    ,form)
		 */
		if (type == Unbound)
			type = T;
		GetConst(COMMON_DESTRUCTURING_BIND, &dbind);
		GetConst(SYSTEM_LOOP_BIND, &bind);
		quotelist_heap(&qvar, var);
		quotelist_heap(&type, type);
		list_heap(&bind, bind, qvar, type, value, NULL);
		list_heap(ret, dbind, var, bind, NULL);
	}
}

static int loop_with_single_(Execute ptr, addr list)
{
	addr var, type, value;

	Return(loop_with_list_bind_(ptr, list, &var, &type, &value));
	loop_with_single_bind(&var, var, type, value);

	return push_let_loop_(ptr, var);
}

static int loop_with_all_variable(addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCar(pos, &pos);
		if (consp(pos))
			return 0;
	}

	return 1;
}

static int loop_with_let_(Execute ptr, addr list)
{
	/* `(let ((,var1 ,value1)
	 *        (,var2 ,value2))
	 *    (declare (type ,var1 ,type1))
	 *    (declare (type ,var2 ,type2))
	 *    ,form)
	 */
	addr args, var, type, value, pos, right, root;
	addr let, declare, dtype;

	/* args */
	args = Nil;
	for (right = list; right != Nil; ) {
		GetCons(right, &pos, &right);
		Return(loop_with_list_bind_(ptr, pos, &var, &type, &value));
		Check(value == Unbound, "unbound error");
		list_heap(&pos, var, value, NULL);
		cons_heap(&args, pos, args);
	}
	/* let */
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_TYPE, &dtype);
	conscar_heap(&root, let);
	cons_heap(&root, args, root);
	for (right = list; right != Nil; ) {
		GetCons(right, &pos, &right);
		Return(loop_with_list_bind_(ptr, pos, &var, &type, &value));
		if (type != Unbound) {
			list_heap(&var, dtype, var, type, NULL);
			list_heap(&var, declare, var, NULL);
			cons_heap(&root, var, root);
		}
	}
	nreverse(&root, root);

	return push_let_loop_(ptr, root);
}

static int loop_with_gensym_form_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(loop_with_single_(ptr, pos));
	}

	return 0;
}

static int loop_with_gensym_(Execute ptr, addr list)
{
	/* `(let ((#:g1 ,value1)
	 *        (#:g2 ,value2))
	 *    (let ((,var1 #:g1))
	 *      (declare ...)
	 *      ...
	 *  [or]
	 *    (destructuring-bind ,var1 (lisp-system::loop-bind ,var1 ,type1 ,#g1)
	 *      ...
	 */
	addr args, pos, var, type, value, right, letlist;
	addr g, glist, let;

	/* args */
	args = glist = letlist = Nil;
	for (right = list; right != Nil; ) {
		GetCons(right, &pos, &right);
		Return(loop_with_list_bind_(ptr, pos, &var, &type, &value));
		Check(value == Unbound, "unbound error");
		Return(make_gensym_(ptr, &g));
		/* let args */
		list_heap(&pos, g, value, NULL);
		cons_heap(&letlist, pos, letlist);
		/* gensym list */
		list_heap(&value, g, value, NULL);
		cons_heap(&args, value, args);
		list_heap(&pos, var, type, g, NULL);
		cons_heap(&glist, pos, glist);
	}
	nreverse(&letlist, letlist);
	nreverse(&args, args);
	nreverse(&glist, glist);
	/* expand */
	GetConst(COMMON_LET, &let);
	list_heap(&let, let, letlist, NULL);
	Return(push_let_loop_(ptr, let));
	Return(loop_with_gensym_form_(ptr, glist));

	return 0;
}

static int loop_with_multiple_(Execute ptr, addr list)
{
	if (loop_with_all_variable(list))
		return loop_with_let_(ptr, list);
	else
		return loop_with_gensym_(ptr, list);
}

static int loop_variables_with_(Execute ptr, addr pos)
{
	if (singlep(pos)) {
		/* let */
		GetCar(pos, &pos);
		return loop_with_single_(ptr, pos);
	}
	else {
		/* and */
		return loop_with_multiple_(ptr, pos);
	}
}


/*
 *  for-as-up
 */
static int loop_variables_for_as_up_form_(Execute ptr, addr list)
{
	/* `(if ,var
	 *    (incf ,var ,by)
	 *    (setq ,var ,a2))
	 * `(unless (,less ,var ,g1)
	 *    (go end-loop))
	 */
	int check;
	addr var, a1, a2, b1, b2, by, g1, g2, pos;
	addr if_symbol, incf, setq, unless, less, go, end_loop;
	addr x, y;

	Return(list_bind_(list, &var, &a1, &a2, &b1, &b2, &by, &g1, &g2, NULL));
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_INCF, &incf);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	/* increment */
	if (a1 == Unbound)
		fixnum_heap(&a2, 0);
	if (by == Unbound)
		fixnum_heap(&by, 1);
	list_heap(&x, incf, var, by, NULL);
	list_heap(&y, setq, var, a2, NULL);
	list_heap(&x, if_symbol, var, x, y, NULL);
	Return(push_form_loop_(ptr, x));

	/* loop */
	if (b1 != Unbound) {
		Return(loop_symbol_below_p_(b1, &check));
		if (check) {
			/* `(unless (< ,var ,g1) (go end-loop)) */
			GetConst(COMMON_NUMBER_LESS, &less);
		}
		else {
			/* `(unless (<= ,var ,g1) (go end-loop)) */
			GetConst(COMMON_NUMBER_LESS_EQUAL, &less);
		}
		list_heap(&less, less, var, g1, NULL);
		list_heap(&go, go, end_loop, NULL);
		list_heap(&pos, unless, less, go, NULL);
		Return(push_form_loop_(ptr, pos));
	}

	return 0;
}

static int loop_variables_for_as_up_init_(Execute ptr, addr list)
{
	/* `(let (,var ,g1)
	 *    ...)
	 * `(setq ,g1 ,b2)
	 */
	addr var, a1, a2, b1, b2, by, g1, g2, x;

	/* let */
	Return(list_bind_(list, &var, &a1, &a2, &b1, &b2, &by, &g1, &g2, NULL));
	Return(push_vars_loop_(ptr, var));
	Return(push_vars_loop_(ptr, g1));

	/* setq */
	if (b1 != Unbound) {
		GetConst(COMMON_SETQ, &x);
		list_heap(&x, x, g1, b2, NULL);
		Return(push_init_loop_(ptr, x));
	}

	return 0;
}

static int loop_variables_for_as_up_(Execute ptr, addr list)
{
	Return(loop_variables_for_as_up_init_(ptr, list));
	Return(loop_variables_for_as_up_form_(ptr, list));

	return 0;
}


/*
 *  for-as down
 */
static int loop_variables_for_as_down_form_(Execute ptr, addr list)
{
	/* `(if ,var
	 *    (defc ,var ,by)
	 *    (setq ,var ,a2))
	 * `(unless (,greater ,var ,g1)
	 *    (go end-loop))
	 */
	int check;
	addr var, a1, a2, b1, b2, by, g1, g2, pos;
	addr if_symbol, decf, setq, unless, greater, go, end_loop;
	addr x, y;

	Return(list_bind_(list, &var, &a1, &a2, &b1, &b2, &by, &g1, &g2, NULL));
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_DECF, &decf);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	/* increment */
	if (a1 == Unbound)
		fixnum_heap(&a2, 0);
	if (by == Unbound)
		fixnum_heap(&by, 1);
	list_heap(&x, decf, var, by, NULL);
	list_heap(&y, setq, var, a2, NULL);
	list_heap(&x, if_symbol, var, x, y, NULL);
	Return(push_form_loop_(ptr, x));

	/* loop */
	if (b1 != Unbound) {
		Return(loop_symbol_above_p_(b1, &check));
		if (check) {
			/* `(unless (< ,var ,g1) (go end-loop)) */
			GetConst(COMMON_NUMBER_GREATER, &greater);
		}
		else {
			/* `(unless (<= ,var ,g1) (go end-loop)) */
			GetConst(COMMON_NUMBER_GREATER_EQUAL, &greater);
		}
		list_heap(&greater, greater, var, g1, NULL);
		list_heap(&go, go, end_loop, NULL);
		list_heap(&pos, unless, greater, go, NULL);
		Return(push_form_loop_(ptr, pos));
	}

	return 0;
}

static int loop_variables_for_as_down_(Execute ptr, addr list)
{
	Return(loop_variables_for_as_up_init_(ptr, list));
	Return(loop_variables_for_as_down_form_(ptr, list));

	return 0;
}


/*
 *  in-list
 */
static int loop_destructuring_bind_tree_(Execute ptr,
		addr var, addr *gtree, addr *glist)
{
	addr g, a, b, x, y;

	if (var == Nil) {
		*gtree = Nil;
	}
	else if (! consp(var)) {
		Check(! symbolp(var), "type error");
		Return(make_gensym_(ptr, &g));
		*gtree = g;
		cons_heap(glist, var, *glist);
		cons_heap(glist, g, *glist);
	}
	else {
		GetCons(var, &a, &b);
		Return(loop_destructuring_bind_tree_(ptr, a, &x, glist));
		Return(loop_destructuring_bind_tree_(ptr, b, &y, glist));
		cons_heap(gtree, x, y);
	}

	return 0;
}

static int loop_destructuring_bind_(Execute ptr,
		addr var, addr type, addr value, addr *ret)
{
	/* `(destructuring-bind ,glist (loop-bind ',var ',type ,value)
	 *    (setq ,var1 ,g2 ,var2 ,g2 ...))
	 */
	addr dbind, lbind, setq, gtree, glist;

	GetConst(COMMON_DESTRUCTURING_BIND, &dbind);
	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_LOOP_BIND, &lbind);

	gtree = glist = Nil;
	Return(loop_destructuring_bind_tree_(ptr, var, &gtree, &glist));
	nreverse(&glist, glist);
	if (type == Unbound)
		type = T;
	quotelist_heap(&var, var);
	quotelist_heap(&type, type);
	list_heap(&lbind, lbind, var, type, value, NULL);
	cons_heap(&setq, setq, glist);
	list_heap(ret, dbind, gtree, lbind, setq, NULL);

	return 0;
}

static int loop_destructuring_setq_value_(Execute ptr,
		addr var, addr type, addr value, addr *ret1, addr *ret2)
{
	addr pos;

	/* destructuring-bind */
	if (consp(var)) {
		Return(loop_destructuring_bind_(ptr, var, type, value, ret1));
		*ret2 = Nil;
		return 0;
	}

	/* setq */
	GetConst(COMMON_SETQ, &pos);
	list_heap(ret1, pos, var, value, NULL);

	/* `(check-type ,var ,type) */
	if (type != Unbound) {
		GetConst(COMMON_CHECK_TYPE, &pos);
		list_heap(ret2, pos, var, type, NULL);
	}
	else {
		*ret2 = Nil;
	}

	return 0;
}

static int loop_destructuring_setq_(Execute ptr, addr var, addr type, addr value)
{
	addr x, y;

	Return(loop_destructuring_setq_value_(ptr, var, type, value, &x, &y));
	Return(push_form_loop_(ptr, x));
	if (y != Nil) {
		Return(push_form_loop_(ptr, y));
	}

	return 0;
}

static int loop_destructuring_setq_expr_(Execute ptr,
		addr var, addr type, addr value, addr *ret)
{
	addr x, y, progn;

	Return(loop_destructuring_setq_value_(ptr, var, type, value, &x, &y));
	if (y == Nil)
		return Result(ret, x);

	/* progn */
	GetConst(COMMON_PROGN, &progn);
	list_heap(ret, progn, x, y, NULL);

	return 0;
}

static int loop_variables_for_as_in_list_form_(Execute ptr,
		addr var, addr type, addr step, addr g)
{
	/* `(unless ,g
	 *    (go end-loop))
	 *  (for-setq ,var ,type (car ,g))
	 *  (setq ,g (funcall ,step ,g))
	 *  or
	 *  (setq ,g (cdr ,g))
	 */
	addr unless, go, end_loop, setq, funcall, car, cdr;
	addr x;

	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	/* `(unless ,g (go end-loop)) */
	list_heap(&go, go, end_loop, NULL);
	list_heap(&x, unless, g, go, NULL);
	Return(push_form_loop_(ptr, x));

	/* (SETQ ,var ,type (car ,g)) */
	GetConst(COMMON_CAR, &car);
	list_heap(&x, car, g, NULL);
	Return(loop_destructuring_setq_(ptr, var, type, x));

	/* update */
	if (step != Unbound) {
		/* `(setq ,g (funcall ,step ,g)) */
		GetConst(COMMON_FUNCALL, &funcall);
		list_heap(&funcall, funcall, step, g, NULL);
		list_heap(&x, setq, g, funcall, NULL);
	}
	else {
		/* `(setq ,g (cdr ,g)) */
		GetConst(COMMON_CDR, &cdr);
		list_heap(&cdr, cdr, g, NULL);
		list_heap(&x, setq, g, cdr, NULL);
	}
	Return(push_form_loop_(ptr, x));

	return 0;
}

static int loop_initialy_setq_(Execute ptr, addr var, addr value)
{
	/* (setq ,var ,value) */
	addr pos;

	GetConst(COMMON_SETQ, &pos);
	list_heap(&pos, pos, var, value, NULL);
	return push_init_loop_(ptr, pos);
}

static int push_vars_tree_loop_(Execute ptr, addr tree)
{
	addr car, cdr;

	if (tree == Nil)
		return 0;
	if (! consp(tree))
		return push_vars_loop_(ptr, tree);
	GetCons(tree, &car, &cdr);
	Return(push_vars_tree_loop_(ptr, car));
	Return(push_vars_tree_loop_(ptr, cdr));

	return 0;
}

static int loop_variables_for_as_in_list_(Execute ptr, addr list)
{
	addr var, type, value, step, g;

	Return(list_bind_(list, &var, &type, &value, &step, &g, NULL));
	Return(push_vars_tree_loop_(ptr, var));
	Return(push_vars_tree_loop_(ptr, g));
	Return(loop_initialy_setq_(ptr, g, value));
	Return(loop_variables_for_as_in_list_form_(ptr, var, type, step, g));

	return 0;
}


/*
 *  on-list
 */
static int loop_variables_for_as_on_list_form_(Execute ptr,
		addr var, addr type, addr step, addr g)
{
	/* `(unless ,g
	 *    (go end-loop))
	 *  (for-setq ,var ,type ,g)
	 *  (setq ,g (funcall ,step ,g))
	 *  or
	 *  (setq ,g (cdr ,g))
	 */
	addr unless, go, end_loop, setq, funcall, cdr;
	addr x;

	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	/* `(unless ,g (go end-loop)) */
	list_heap(&go, go, end_loop, NULL);
	list_heap(&x, unless, g, go, NULL);
	Return(push_form_loop_(ptr, x));

	/* (SETQ ,var ,type ,g) */
	Return(loop_destructuring_setq_(ptr, var, type, g));

	/* update */
	if (step != Unbound) {
		/* `(setq ,g (funcall ,step ,g)) */
		GetConst(COMMON_FUNCALL, &funcall);
		list_heap(&funcall, funcall, step, g, NULL);
		list_heap(&x, setq, g, funcall, NULL);
	}
	else {
		/* `(setq ,g (cdr ,g)) */
		GetConst(COMMON_CDR, &cdr);
		list_heap(&cdr, cdr, g, NULL);
		list_heap(&x, setq, g, cdr, NULL);
	}
	Return(push_form_loop_(ptr, x));

	return 0;
}

static int loop_variables_for_as_on_list_(Execute ptr, addr list)
{
	addr var, type, value, step, g;

	Return(list_bind_(list, &var, &type, &value, &step, &g, NULL));
	Return(push_vars_tree_loop_(ptr, var));
	Return(push_vars_tree_loop_(ptr, g));
	Return(loop_initialy_setq_(ptr, g, value));
	Return(loop_variables_for_as_on_list_form_(ptr, var, type, step, g));

	return 0;
}


/*
 *  equals-then
 */
static int loop_variables_for_as_equals_then_form_(Execute ptr,
		addr var, addr type, addr value, addr then, addr g)
{
	/* `(if ,g
	 *    (for-setq ,var ,type ,then)
	 *    (for-setq ,var ,type ,value))
	 *  (setq ,g t)
	 */
	addr if_symbol, setq, pos;

	if (then == Unbound)
		return loop_destructuring_setq_(ptr, var, type, value);

	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_SETQ, &setq);

	Return(loop_destructuring_setq_expr_(ptr, var, type, then, &then));
	Return(loop_destructuring_setq_expr_(ptr, var, type, value, &value));
	list_heap(&pos, if_symbol, g, then, value, NULL);
	Return(push_form_loop_(ptr, pos));
	list_heap(&pos, setq, g, T, NULL);
	Return(push_form_loop_(ptr, pos));

	return 0;
}

static int loop_variables_for_as_equals_then_(Execute ptr, addr list)
{
	addr var, type, value, then, g;

	Return(list_bind_(list, &var, &type, &value, &then, &g, NULL));
	Return(push_vars_tree_loop_(ptr, var));
	Return(push_vars_tree_loop_(ptr, g));
	Return(loop_variables_for_as_equals_then_form_(ptr, var, type, value, then, g));

	return 0;
}


/*
 *  across
 */
static int loop_variables_for_as_across_form_(Execute ptr, addr list)
{
	/* `(when ,a
	 *    (go across-update))
	 *  (setq ,a ,array)
	 *  (setq ,b 0)
	 *  (setq ,c (length ,a))
	 *  across-update
	 *  (unless (< ,b ,c)
	 *    (go end-loop))
	 *  (for-setq ,var ,type (elt ,a ,b))
	 *  (incf ,b)
	 */
	addr var, type, array, a, b, c, x;
	addr when, setq, length, unless, less, go, end_loop, elt, incf, label;

	Return(list_bind_(list, &var, &type, &array, &a, &b, &c, NULL));
	GetConst(COMMON_WHEN, &when);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_LENGTH, &length);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_NUMBER_LESS, &less);
	GetConst(COMMON_GO, &go);
	GetConst(COMMON_ELT, &elt);
	GetConst(COMMON_INCF, &incf);
	GetConst(SYSTEM_END_LOOP, &end_loop);
	make_symbolchar(&label, "ACROSS-UPDATE");

	/* when */
	list_heap(&x, go, label, NULL);
	list_heap(&x, when, a, x, NULL);
	Return(push_form_loop_(ptr, x));

	/* setq */
	list_heap(&x, setq, a, array, NULL);
	Return(push_form_loop_(ptr, x));
	fixnum_heap(&x, 0);
	list_heap(&x, setq, b, x, NULL);
	Return(push_form_loop_(ptr, x));
	list_heap(&x, length, a, NULL);
	list_heap(&x, setq, c, x, NULL);
	Return(push_form_loop_(ptr, x));

	/* label */
	Return(push_form_loop_(ptr, label));

	/* (unless (< b c) (go end-loop)) */
	list_heap(&go, go, end_loop, NULL);
	list_heap(&less, less, b, c, NULL);
	list_heap(&x, unless, less, go, NULL);
	Return(push_form_loop_(ptr, x));

	/* (for-setq ,var ,type `(elt ,a ,b)) */
	list_heap(&x, elt, a, b, NULL);
	Return(loop_destructuring_setq_(ptr, var, type, x));

	/* expr2: `(incf ,b) */
	list_heap(&x, incf, b, NULL);
	Return(push_form_loop_(ptr, x));

	return 0;
}

static int loop_variables_for_as_across_(Execute ptr, addr list)
{
	addr var, type, array, a, b, c;

	Return(list_bind_(list, &var, &type, &array, &a, &b, &c, NULL));
	Return(push_vars_tree_loop_(ptr, var));
	Return(push_vars_tree_loop_(ptr, a));
	Return(push_vars_tree_loop_(ptr, b));
	Return(push_vars_tree_loop_(ptr, c));
	return loop_variables_for_as_across_form_(ptr, list);

}


/*
 *  hash
 */
static int loop_variables_for_as_hash_form_(Execute ptr, addr list)
{
	/* `(unless ,g
	 *    (setq ,g (lisp-system::make-hash-iterator ,table)))
	 *  (multiple-value-bind (,check ,key ,value) (lisp-system::next-hash-iterator ,g)
	 *    (declare (ignorable key value))
	 *    (unless ,check (go end-loop))
	 *    (for-setq ,var ,type ,key)
	 *    (setq ,use ,value))
	 */
	addr var, type, keyp, table, use, g, x, y, z, a, b;
	addr key, value, check;
	addr unless, setq, make, mvbind, next, declare, ignorable, go, end_loop;

	Return(list_bind_(list, &var, &type, &keyp, &table, &use, &g, NULL));

	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_MAKE_HASH_ITERATOR, &make);

	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(SYSTEM_NEXT_HASH_ITERATOR, &next);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	/* (unless ...) */
	list_heap(&x, make, table, NULL);
	list_heap(&x, setq, g, x, NULL);
	list_heap(&x, unless, g, x, NULL);
	Return(push_form_loop_(ptr, x));

	/* (multiple-value-bind ...) */
	Return(make_gensym_(ptr, &check));
	Return(make_gensym_(ptr, &key));
	Return(make_gensym_(ptr, &value));
	list_heap(&z, check, key, value, NULL);
	list_heap(&next, next, g, NULL);
	list_heap(&ignorable, ignorable, key, value, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(&go, go, end_loop, NULL);
	list_heap(&unless, unless, check, go, NULL);
	if (keyp != Nil) {
		a = key;
		b = value;
	}
	else {
		a = value;
		b = key;
	}
	Return(loop_destructuring_setq_expr_(ptr, var, type, a, &x));
	if (use != Unbound) {
		list_heap(&y, setq, use, b, NULL);
		list_heap(&x, mvbind, z, next, declare, unless, x, y, NULL);
	}
	else {
		list_heap(&x, mvbind, z, next, declare, unless, x, NULL);
	}

	return push_form_loop_(ptr, x);
}

static int loop_variables_for_as_hash_(Execute ptr, addr list)
{
	addr var, type, keyp, table, use, g;

	Return(list_bind_(list, &var, &type, &keyp, &table, &use, &g, NULL));
	Return(push_vars_tree_loop_(ptr, var));
	Return(push_vars_tree_loop_(ptr, g));
	return loop_variables_for_as_hash_form_(ptr, list);
}


/*
 *  package
 */
static int loop_variables_for_as_package_init_(Execute ptr, addr pos, addr list)
{
	/* `(unless ,g
	 *    (setq ,g (lisp-system::make-hash-iterator
	 *               (find-package ,package)
	 *               ,internal ,external ,inherited)))
	 */
	addr var, type, package, g, x;
	addr unless, setq, make, find, internal, external, inherited;

	Return(list_bind_(list, &var, &type, &package, &g, NULL));
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_MAKE_PACKAGE_ITERATOR, &make);
	GetConst(COMMON_FIND_PACKAGE, &find);

	internal = external = inherited = Nil;
	if (loop_symbol_for_as_package_present_p(pos)) {
		internal = external = T;
		inherited = Nil;
	}
	if (loop_symbol_for_as_package_symbol_p(pos)) {
		internal = external = inherited = T;
	}
	if (loop_symbol_for_as_package_external_p(pos)) {
		external = T;
		internal = inherited = Nil;
	}

	/* (unless ...) */
	list_heap(&x, find, package, NULL);
	list_heap(&x, make, x, internal, external, inherited, NULL);
	list_heap(&x, setq, g, x, NULL);
	list_heap(&x, unless, g, x, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_variables_for_as_package_form_(Execute ptr, addr list)
{
	/* `(multiple-value-bind (,check ,symbol)
	 *    (lisp-system::next-package-iterator ,g)
	 *    (unless ,check (go end-loop))
	 *    (for-setq ,var ,type ,symbol))
	 */
	addr var, type, package, g, x, y, symbol, check;
	addr mvbind, next, unless, go, end_loop;

	Return(list_bind_(list, &var, &type, &package, &g, NULL));
	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(SYSTEM_NEXT_PACKAGE_ITERATOR, &next);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	/* (multiple-value-bind ...) */
	Return(make_gensym_(ptr, &check));
	Return(make_gensym_(ptr, &symbol));
	list_heap(&y, check, symbol, NULL);
	list_heap(&next, next, g, NULL);
	list_heap(&go, go, end_loop, NULL);
	list_heap(&unless, unless, check, go, NULL);
	Return(loop_destructuring_setq_expr_(ptr, var, type, symbol, &x));
	list_heap(&x, mvbind, y, next, unless, x, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_variables_for_as_package_(Execute ptr, addr pos, addr list)
{
	addr var, type, package, g;

	Return(list_bind_(list, &var, &type, &package, &g, NULL));
	Return(push_vars_tree_loop_(ptr, var));
	Return(push_vars_tree_loop_(ptr, g));
	Return(loop_variables_for_as_package_init_(ptr, pos, list));
	Return(loop_variables_for_as_package_form_(ptr, list));

	return 0;
}


/*
 *  initially
 */
static int loop_variables_initially_(Execute ptr, addr list)
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
static int loop_variables_finally_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(push_final_loop_(ptr, pos));
	}

	return 0;
}


/*
 *  interface
 */
static int loop_variables_for_as_type_(Execute ptr, addr list)
{
	addr pos;

	GetCons(list, &pos, &list);
	/* up */
	if (loop_symbol_for_as_arithmetic_up_p(pos))
		return loop_variables_for_as_up_(ptr, list);
	/* downto */
	if (loop_symbol_for_as_arithmetic_downto_p(pos))
		return loop_variables_for_as_down_(ptr, list);
	/* downfrom */
	if (loop_symbol_for_as_arithmetic_downfrom_p(pos))
		return loop_variables_for_as_down_(ptr, list);
	/* in-list */
	if (loop_symbol_for_as_in_list_p(pos))
		return loop_variables_for_as_in_list_(ptr, list);
	/* on-list */
	if (loop_symbol_for_as_on_list_p(pos))
		return loop_variables_for_as_on_list_(ptr, list);
	/* equals-then */
	if (loop_symbol_for_as_equals_then_p(pos))
		return loop_variables_for_as_equals_then_(ptr, list);
	/* across */
	if (loop_symbol_for_as_across_p(pos))
		return loop_variables_for_as_across_(ptr, list);
	/* hash */
	if (loop_symbol_for_as_hash_p(pos))
		return loop_variables_for_as_hash_(ptr, list);
	/* package */
	if (loop_symbol_for_as_package_symbol_p(pos))
		return loop_variables_for_as_package_(ptr, pos, list);
	if (loop_symbol_for_as_package_present_p(pos))
		return loop_variables_for_as_package_(ptr, pos, list);
	if (loop_symbol_for_as_package_external_p(pos))
		return loop_variables_for_as_package_(ptr, pos, list);

	/* error */
	return fmte_("Invalid variables-clause ~S.", pos, NULL);
}

static int loop_variables_for_as_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(loop_variables_for_as_type_(ptr, pos));
	}

	return 0;
}

static int loop_variables_car_(Execute ptr, addr list)
{
	addr pos;

	GetCons(list, &pos, &list);
	/* with */
	if (loop_symbol_with_p(pos))
		return loop_variables_with_(ptr, list);
	/* for-as */
	if (loop_symbol_for_as_p(pos))
		return loop_variables_for_as_(ptr, list);
	/* initially */
	if (loop_symbol_initially_p(pos))
		return loop_variables_initially_(ptr, list);
	/* finally */
	if (loop_symbol_finally_p(pos))
		return loop_variables_finally_(ptr, list);

	/* error */
	return fmte_("Invalid variables-clause ~S.", pos, NULL);
}

int loop_variables_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(loop_variables_car_(ptr, pos));
	}

	return 0;
}


/************************************************************
 *  main_argv.c
 ************************************************************/

#ifdef LISP_WINMAIN_WIDE
#include <windows.h>
#endif

/*
 *  constant
 */
#define LispArgv_MinusMinus     "--"
#define LispArgv_Help           "--help"
#define LispArgv_Version        "--version"
#define LispArgv_VersionScript  "--version-script"
#define LispArgv_Core           "--core"
#define LispArgv_Standalone     "--standalone"
#define LispArgv_Build          "--build"
#define LispArgv_Degrade        "--degrade"
#define LispArgv_Heap           "--heap"
#define LispArgv_Local          "--local"
#define LispArgv_Corefile       "--corefile"
#define LispArgv_Initfile       "--initfile"
#define LispArgv_Nocore         "--nocore"
#define LispArgv_Noinit         "--noinit"
#define LispArgv_Debugger       "--debugger"
#define LispArgv_Nodebugger     "--nodebugger"
#define LispArgv_Quit           "--quit"
#define LispArgv_Script         "--script"
#define LispArgv_Load           "--load"
#define LispArgv_Eval           "--eval"
#define LispArgv_Bright         "--bright"
#define LispArgv_Dark           "--dark"
#define LispArgv_Color          "--color"
#define LispArgv_Monochrome     "--monochrome"
#define LispArgv_equal(a,b) equalchar_stringu((a), LispArgv_##b)


/*
 *  inputs
 */
static int mainparse_inputs_string(lispstringu s, enum lispargv_execute *ret)
{
	if (LispArgv_equal(s, Load)) {
		*ret = lispargv_load;
		return 1;
	}
	if (LispArgv_equal(s, Eval)) {
		*ret = lispargv_eval;
		return 1;
	}
	if (LispArgv_equal(s, Script)) {
		*ret = lispargv_script;
		return 1;
	}
	if (LispArgv_equal(s, MinusMinus)) {
		*ret = lispargv_minusminus;
		return 1;
	}

	return 0;
}

static int mainparse_inputs_length(lisparrayu argv, size_t index, size_t *ret)
{
	enum lispargv_execute type;
	lispstringu s, *ptr;
	size_t allsize, size;

	allsize = argv->size;
	ptr = argv->ptr;
	for (size = 0; index < allsize; size++) {
		s = ptr[index++];
		if (! mainparse_inputs_string(s, &type)) {
			lisperror_noeol("Invalid argument.");
			output_stringu(s, lisperror_stream());
			lisperror(".");
			return 1;
		}
		if (type == lispargv_minusminus)
			break;
		if (allsize <= index) {
			lisperror_noeol("After ");
			output_stringu(s, lisperror_stream());
			lisperror(" must be at least one argument.");
			return 1;
		}
		index++;
	}
	*ret = size;

	return 0;
}

static int mainparse_inputs_copy(struct lispargv_input *input,
		lisparrayu argv, size_t index, size_t *ret)
{
	enum lispargv_execute type;
	lispstringu s, *ptr;
	struct lispargv_string *data;
	size_t allsize, size;

	data = input->data;
	allsize = argv->size;
	ptr = argv->ptr;
	for (size = 0; index < allsize; size++) {
		s = ptr[index];
		if (! mainparse_inputs_string(s, &type))
			return 1;
		if (type == lispargv_minusminus) {
			index++;
			break;
		}
		index++; /* key */
		if (allsize <= index)
			return 1;
		s = copy_stringu(ptr[index]);
		index++; /* value */
		if (s == NULL)
			return 1;
		data[size].type = type;
		data[size].value = s;
	}
	*ret = index;

	return 0;
}

static struct lispargv_input *make_lispargv_input(size_t size)
{
	struct lispargv_string *data;
	struct lispargv_input *ptr;

	ptr = (struct lispargv_input *)malloc(sizeoft(struct lispargv_input));
	if (ptr == NULL)
		return NULL;
	data = (struct lispargv_string *)malloc(sizeoft(struct lispargv_string) * size);
	if (data == NULL) {
		free(ptr);
		return NULL;
	}
	memset(data, 0, sizeoft(struct lispargv_string) * size);
	ptr->data = data;
	ptr->size = size;

	return ptr;
}

static int mainparse_inputs(struct lispargv *ptr)
{
	lisparrayu argv;
	struct lispargv_input *input;
	size_t index, size;

	argv = ptr->argv;
	index = ptr->index;
	/* length */
	if (mainparse_inputs_length(argv, index, &size))
		return 1;
	/* make input */
	input = make_lispargv_input(size);
	if (input == NULL)
		return 1;
	ptr->input = input;
	/* copy input */
	if (mainparse_inputs_copy(input, argv, index, &size))
		return 1;
	/* copy command */
	if (argv->size < size)
		return 1;
	ptr->start = size;

	return 0;
}


/*
 *  parse-arguments
 */
static int lispargs_heap(struct lispargv *ptr, size_t index)
{
	lisparrayu a;
	size_t size;

	a = ptr->argv;
	if (a->size <= index + 1UL) {
		lisperror("After --heap must be at least one argument.");
		return 1;
	}
	if (getsize_stringu(a->ptr[index + 1UL], &size)) {
		lisperror("Memory size format error.");
		return 1;
	}
	if (size < 1024UL * 1024UL) {
		lisperror("Memory size must be at least 1MByte.");
		return 1;
	}
	ptr->heap = size;

	return 0;
}

static int lispargs_local(struct lispargv *ptr, size_t index)
{
	lisparrayu a;
	size_t size;

	a = ptr->argv;
	if (a->size <= index + 1UL) {
		lisperror("After --local must be at least one argument.");
		return 1;
	}
	if (getsize_stringu(a->ptr[index + 1UL], &size)) {
		lisperror("Stack size format error.");
		return 1;
	}
	if (size < 1024UL * 1024UL) {
		lisperror("Stack size must be at least 1MByte.");
		return 1;
	}
	ptr->local = size;

	return 0;
}

static int lispargs_corefile(struct lispargv *ptr, size_t index)
{
	lispstringu s;
	lisparrayu a;

	a = ptr->argv;
	if (a->size <= index + 1UL) {
		lisperror("After --corefile must be at least one argument.");
		return 1;
	}
	if (ptr->mode_standalone) {
		lisperror("Cannot load a core file in standalone mode.");
		return 1;
	}
	s = copy_stringu(a->ptr[index + 1UL]);
	if (s == NULL) {
		lisperror("copy_string error.");
		return 1;
	}
	ptr->core = s;

	return 0;
}

static int lispargs_initfile(struct lispargv *ptr, size_t index)
{
	lispstringu s;
	lisparrayu a;

	a = ptr->argv;
	if (a->size <= index + 1UL) {
		lisperror("After --initfile must be at least one argument.");
		return 1;
	}
	s = copy_stringu(a->ptr[index + 1UL]);
	if (s == NULL) {
		lisperror("copy_string error.");
		return 1;
	}
	ptr->init = s;

	return 0;
}


/*
 *  mode
 */
enum LispMode {
	LispMode_Core,
	LispMode_Standalone,
	LispMode_Degrade,
	LispMode_Help,
	LispMode_Version,
	LispMode_Size
};

static void mainparse_mode(struct lispargv *ptr, enum LispMode mode)
{
	ptr->mode_help = 0;
	ptr->mode_version = 0;
	ptr->mode_core = 0;
	ptr->mode_standalone = 0;
	ptr->mode_degrade = 0;
	switch (mode) {
		case LispMode_Help:
			ptr->mode_help = 1;
			break;

		case LispMode_Version:
			ptr->mode_version = 1;
			break;

		case LispMode_Degrade:
			ptr->mode_degrade = 1;
			ptr->nocore = 0;
			ptr->noinit = 1;
			ptr->debugger = 1;
			ptr->debuggerp = 0;
			ptr->quit = 0;
			break;

		case LispMode_Core:
			ptr->mode_core = 1;
			ptr->nocore = 0;
			ptr->noinit = 0;
			ptr->debugger = 1;
			ptr->debuggerp = 0;
			ptr->quit = 0;
			break;

		case LispMode_Standalone:
		default:
			ptr->mode_standalone = 1;
			ptr->nocore = 1;
			ptr->noinit = 0;
			ptr->debugger = 1;
			ptr->debuggerp = 0;
			ptr->quit = 0;
			break;
	}
}

static int mainparse_initmode(struct lispargv *ptr)
{
#ifdef LISP_MODE_CORE
	mainparse_mode(ptr, LispMode_Core);
#endif
#ifdef LISP_MODE_STANDALONE
	mainparse_mode(ptr, LispMode_Standalone);
#endif
#ifdef LISP_MODE_DEGRADE
	mainparse_mode(ptr, LispMode_Degrade);
	if (ptr->argv->size == 1)
		return 1;
#endif
	return 0;
}

static int mainparse_modecheck(struct lispargv *ptr)
{
	lisparrayu a;
	lispstringu s;

	a = ptr->argv;
	if (a->size != 2)
		return 0;
	s = a->ptr[1];
	if (LispArgv_equal(s, Degrade)) {
		mainparse_mode(ptr, LispMode_Degrade);
		return 1;
	}
	if (LispArgv_equal(s, Help)) {
		mainparse_mode(ptr, LispMode_Help);
		return 1;
	}
	if (LispArgv_equal(s, Version)) {
		mainparse_mode(ptr, LispMode_Version);
		return 1;
	}
	if (LispArgv_equal(s, VersionScript)) {
		mainparse_mode(ptr, LispMode_Version);
		ptr->version_script = 1;
		return 1;
	}

	return 0;
}


/*
 *  mainparse_loop
 */
static int mainparse_loop(struct lispargv *ptr)
{
	lisparrayu a;
	lispstringu s, *argv;
	size_t i, size;

	a = ptr->argv;
	argv = a->ptr;
	size = a->size;
	for (i = 1; i < size; i++) {
		s = argv[i];
		if (LispArgv_equal(s, Heap)) {
			if (lispargs_heap(ptr, i++))
				return 1;
			continue;
		}
		if (LispArgv_equal(s, Local)) {
			if (lispargs_local(ptr, i++))
				return 1;
			continue;
		}
		if (LispArgv_equal(s, Corefile)) {
			if (lispargs_corefile(ptr, i++))
				return 1;
			continue;
		}
		if (LispArgv_equal(s, Initfile)) {
			if (lispargs_initfile(ptr, i++))
				return 1;
			continue;
		}
		if (LispArgv_equal(s, Nocore)) {
			ptr->nocore = 1;
			continue;
		}
		if (LispArgv_equal(s, Noinit)) {
			ptr->noinit = 1;
			continue;
		}
		if (LispArgv_equal(s, Debugger)) {
			ptr->debugger = 1;
			ptr->debuggerp = 1;
			continue;
		}
		if (LispArgv_equal(s, Nodebugger)) {
			ptr->debugger = 0;
			ptr->debuggerp = 1;
			continue;
		}
		if (LispArgv_equal(s, Quit)) {
			ptr->quit = 1;
			continue;
		}
		if (LispArgv_equal(s, Bright)) {
			ptr->terme_bright = 1;
			ptr->terme_dark = 0;
			continue;
		}
		if (LispArgv_equal(s, Dark)) {
			ptr->terme_bright = 0;
			ptr->terme_dark = 1;
			continue;
		}
		if (LispArgv_equal(s, Color)) {
			ptr->terme_color = 1;
			ptr->terme_monochrome = 0;
			continue;
		}
		if (LispArgv_equal(s, Monochrome)) {
			ptr->terme_color = 0;
			ptr->terme_monochrome = 1;
			continue;
		}
		if (LispArgv_equal(s, Script)) {
			if (ptr->debuggerp == 0) {
				ptr->debuggerp = 1;
				ptr->debugger = 0;
			}
			ptr->quit = 1;
			goto inputs;
		}
		if (LispArgv_equal(s, Load) || LispArgv_equal(s, Eval) ||
				LispArgv_equal(s, MinusMinus)) {
			goto inputs;
		}
		if (LispArgv_equal(s, Core)) {
			if (i == 1) {
				mainparse_mode(ptr, LispMode_Core);
				continue;
			}
			lisperror("--core must be a first argument.");
			return 1;
		}
		if (LispArgv_equal(s, Standalone) || LispArgv_equal(s, Build)) {
			if (i == 1) {
				mainparse_mode(ptr, LispMode_Standalone);
				continue;
			}
			lisperror("--standalone must be a first argument.");
			return 1;
		}
		if (LispArgv_equal(s, Degrade)) {
			lisperror("--degrade must be only one argument.");
			return 1;
		}
		if (LispArgv_equal(s, Help)) {
			lisperror("--help must be only one argument.");
			return 1;
		}
		if (LispArgv_equal(s, Version)) {
			lisperror("--version must be only one argument.");
			return 1;
		}
		if (LispArgv_equal(s, VersionScript)) {
			lisperror("--version-script must be only one argument.");
			return 1;
		}
		lisperror_noeol("Invalid argument, ");
		output_stringu(s, lisperror_stream());
		lisperror(".");
		return 1;
	}
inputs:
	ptr->index = i;

	return 0;
}


/*
 *  mainparse
 */
static int mainparse(struct lispargv *ptr)
{
	/* default mode */
	if (mainparse_initmode(ptr))
		return 0;
	/* first argument */
	if (mainparse_modecheck(ptr))
		return 0;
	/* loop */
	if (mainparse_loop(ptr))
		return 1;
	/* inputs */
	if (mainparse_inputs(ptr))
		return 1;

	return 0;
}


/*
 *  argv, env
 */
static int argv_argv_main(lisparrayu *ret, int argc, char *argv[])
{
	lisparrayu a;

	a = arrayu_argv_utf8(argc, (const byte *const *)argv);
	if (a == NULL)
		return 1;
	*ret = a;

	return 0;
}

#ifdef LISP_WINMAIN_WIDE
static int argv_argv_windows(lisparrayu *ret)
{
	int argc;
	LPWSTR *ptr;
	lisparrayu a;

	ptr = CommandLineToArgvW(GetCommandLineW(), &argc);
	if (ptr == NULL)
		return 1;
	a = arrayu_argv_utf16(argc, (const byte16 *const *)ptr);
	if (a == NULL) {
		(void)LocalFree((HLOCAL)ptr);
		return 1;
	}
	if (LocalFree((HLOCAL)ptr)) {
		free_arrayu(a);
		return 1;
	}
	*ret = a;

	return 0;
}
#endif


/*
 *  environment
 */
static int argv_env_main(lisptableu *ret, char *env[])
{
	lisptableu a;

	if (env == NULL) {
		*ret = NULL;
		return 0;
	}
	a = tableu_env_main((const byte *const *)env);
	if (a == NULL)
		return 1;
	*ret = a;

	return 0;
}

#ifdef LISP_WINMAIN_WIDE
static int argv_env_windows(lisptableu *ret)
{
	LPWCH ptr = NULL;
	lisptableu a;

	ptr = GetEnvironmentStringsW();
	if (ptr == NULL)
		return 1;
	a = tableu_env_windows((const byte16 *)ptr);
	FreeEnvironmentStringsW(ptr);
	if (a == NULL)
		return 1;
	*ret = a;

	return 0;
}
#endif


/*
 *  lispargv
 */
static struct lispargv *make_lispargv(void)
{
	struct lispargv *ptr;

	ptr = (struct lispargv *)malloc(sizeoft(struct lispargv));
	if (ptr == NULL)
		return NULL;
	memset(ptr, 0, sizeoft(struct lispargv));

	return ptr;
}

static void lispargv_free_input(struct lispargv_input *ptr)
{
	size_t i, size;
	struct lispargv_string *data;

	if (ptr) {
		size = ptr->size;
		data = ptr->data;
		for (i = 0; i < size; i++)
			free_stringu(data[i].value);
		free(data);
		ptr->data = NULL;
		free(ptr);
	}
}

void lispargv_free(struct lispargv *ptr)
{
	if (ptr) {
		free_stringu(ptr->core);
		free_stringu(ptr->init);
		free_stringu(ptr->reload_core);
		free_arrayu(ptr->argv);
		free_tableu(ptr->env);
		lispargv_free_input(ptr->input);
		free(ptr);
	}
}


/*
 *  interface
 */
struct lispargv *lispargv_main_force(int argc, char *argv[], char *env[])
{
	lisparrayu a;
	lisptableu e;
	struct lispargv *ptr;

	/* object */
	ptr = make_lispargv();
	if (ptr == NULL) {
		lisperror("make_lispargv error.");
		return NULL;
	}

	/* argv */
	if (argv_argv_main(&a, argc, argv)) {
		lisperror("parse argv error");
		goto error;
	}
	ptr->argv = a;

	/* env */
	if (argv_env_main(&e, env)) {
		lisperror("parse environment error");
		goto error;
	}
	ptr->env = e;

	/* parse */
	if (argc && mainparse(ptr))
		goto error;
	return ptr;

error:
	lispargv_free(ptr);
	return NULL;
}

#ifdef LISP_WINMAIN_WIDE
struct lispargv *lispargv_windows(void)
{
	lisparrayu a;
	lisptableu e;
	struct lispargv *ptr;

	/* object */
	ptr = make_lispargv();
	if (ptr == NULL) {
		lisperror("make_lispargv error.");
		return NULL;
	}

	/* argv */
	if (argv_argv_windows(&a)) {
		lisperror("parse argv error");
		goto error;
	}
	ptr->argv = a;

	/* env */
	if (argv_env_windows(&e)) {
		lisperror("parse environment error");
		goto error;
	}
	ptr->env = e;

	/* parse */
	if (mainparse(ptr))
		goto error;
	return ptr;

error:
	lispargv_free(ptr);
	return NULL;
}
#endif

struct lispargv *lispargv_main(int argc, char *argv[], char *env[])
{
#ifdef LISP_WINMAIN_WIDE
	return lispargv_windows();
#else
	return lispargv_main_force(argc, argv, env);
#endif
}


/************************************************************
 *  main_string.c
 ************************************************************/
/*
 *  main-string
 */

/*
 *  getsize_stringu
 */
static int strsize_stringu(const unicode *str, const unicode **endp, size_t *ret)
{
	unicode c;
	size_t value;

	/* trim space */
	for (;;) {
		c = *str;
		if (! isSpaceUnicode(c))
			break;
		str++;
	}
	if (! isDigitCase(c))
		return 1;

	/* parse-integer */
	value = 0;
	for (;;) {
		c = *str;
		if (! isDigitCase(c))
			break;
		if (value) {
			if ((SIZE_MAX / 10) < value) {
				*ret = SIZE_MAX;
				return 2;
			}
			value *= 10;
		}
		if (value > (SIZE_MAX - c)) {
			*ret = SIZE_MAX;
			return 2;
		}
		value += c - '0';
		str++;
	}
	if (endp) *endp = str;
	*ret = value;

	return 0;
}

static int getunit_stringu(const unicode *str, int *type)
{
	unicode c, check;

	c = *(str++);
	if (c == 0) {
		*type = 0;
		return 0;
	}
	c = toUpperUnicode(c);
	if (c == 'K' || c == 'M' || c == 'G' || c == 'T' || c == 'P' || c == 'E') {
		for (;;) {
			check = *str;
			if (! isSpaceUnicode(check))
				break;
			str++;
		}
		if (check != 0)
			return 1;
	}
	else {
		return 1;
	}
	*type = (int)c;

	return 0;
}

static int unitloop(size_t *value, int loop)
{
	int i;
	size_t v;

	v = *value;
	for (i = 0; i < loop; i++) {
		if ((SIZE_MAX / 1024UL) < v) {
			return 1;
		}
		v *= 1024UL;
	}
	*value = v;

	return 0;
}

int getsize_stringu(lispstringu str, size_t *ret)
{
	int check;
	const unicode *next;
	size_t size;

	check = strsize_stringu(str->ptr, &next, &size);
	if (check == 2) {
		lisperror("Number is too large.");
		return 1;
	}
	if (check) {
		lisperror("Invalid memory argument.");
		return 1;
	}
	if (size == 0) {
		lisperror("Memory size must not be a zero.");
		return 1;
	}
	if (getunit_stringu(next, &check)) {
		lisperror("Invalid unit string.");
		return 1;
	}
	switch (check) {
		case 'K':
			if (unitloop(&size, 1))
				goto error;
			break;

		case 'M':
			if (unitloop(&size, 2))
				goto error;
			break;

		case 'G':
			if (unitloop(&size, 3))
				goto error;
			break;

		case 'T':
			if (unitloop(&size, 4))
				goto error;
			break;

		case 'P':
			if (unitloop(&size, 5))
				goto error;
			break;

		case 'E':
			if (unitloop(&size, 6))
				goto error;
			break;

		default:
			break;
	}
	*ret = size;
	return 0;

error:
	lisperror("Number is too large.");
	return 1;
}


/*
 *  lispstringu
 */
lispstringu make_stringu(size_t size)
{
	lispstringu ptr;
	unicode *data;

	ptr = (lispstringu)malloc(sizeoft(struct lispstringu_struct));
	if (ptr == NULL)
		return NULL;
	data = (unicode *)malloc(sizeoft(unicode) * size);
	if (data == NULL) {
		free(ptr);
		return NULL;
	}
	ptr->ptr = data;
	ptr->size = size;

	return ptr;
}

lispstringu char_stringu(const char *str)
{
	lispstringu ptr;
	unicode *a;
	const byte *b;
	size_t size, i;

	size = strlen(str);
	b = (const byte *)str;
	ptr = make_stringu(size + 1UL);
	a = ptr->ptr;
	for (i = 0; i < size; i++)
		a[i] = (unicode)b[i];
	a[i] = 0;

	return ptr;
}

lispstringu wchar_stringu(const byte16 *str)
{
	lispstringu ptr;
	unicode *a;
	size_t size, i;

	if (UTF16_null_strlen(str, &size))
		return NULL;
	ptr = make_stringu(size + 1UL);
	a = ptr->ptr;
	for (i = 0; i < size; i++)
		a[i] = (unicode)str[i];
	a[i] = 0;

	return ptr;
}

lispstringu copy_stringu(lispstringu ptr)
{
	lispstringu copy;

	copy = make_stringu(ptr->size);
	if (copy == NULL)
		return NULL;
	memcpy(copy->ptr, ptr->ptr, sizeoft(unicode) * ptr->size);

	return copy;
}

lispstringu concatchar_stringu(lispstringu a, const char *b)
{
	unicode *pa;
	const byte *pb;
	lispstringu copy;
	size_t i, sizea, sizeb;

	sizea = a->size;
	if (sizea == 0)
		return NULL;
	sizeb = strlen(b);
	copy = make_stringu(sizea + sizeb);
	if (copy == NULL)
		return NULL;
	sizea--;
	pa = copy->ptr;
	pb = (const byte *)b;
	memcpy(pa, a->ptr, sizeoft(unicode) * sizea);
	for (i = 0; i < sizeb; i++)
		pa[sizea + i] = (unicode)pb[i];
	pa[sizea + i] = 0;

	return copy;
}

void output_stringu(lispstringu ptr, FILE *file)
{
	unicode *data, u;
	size_t i, size;

	size = ptr->size;
	data = ptr->ptr;
	for (i = 0; i < size; i++) {
		u = data[i];
		if (u)
			fprintf(file, "%c", (u < 0x80 && isgraph((int)u))? (int)u: '.');
	}
}

void free_stringu(lispstringu ptr)
{
	if (ptr) {
		free(ptr->ptr);
		ptr->ptr = NULL;
		free(ptr);
	}
}

int equal_stringu(lispstringu a, lispstringu b)
{
	return (a->size == b->size) && (memcmp(a->ptr, b->ptr, a->size) == 0);
}

int equalchar_stringu(lispstringu a, const char *b)
{
	const unicode *c;
	const byte *d;
	size_t i, size;

	size = strlen(b) + 1UL;
	if (a->size != size)
		return 0;
	c = a->ptr;
	d = (const byte *)b;
	for (i = 0; i < size; i++) {
		if (c[i] != (unicode)d[i])
			return 0;
	}

	return 1;
}


/*
 *  lisparrayu
 */
static lispstringu *stringu_call(int argc, const void *const *argv,
		int (*len)(const void *, size_t *),
		int (*make)(unicode *, const void *))
{
	int i, k;
	const void *str;
	lispstringu x, *r;
	size_t size;

	r = (lispstringu *)malloc(sizeoft(lispstringu) * argc);
	if (r == NULL)
		return NULL;
	for (i = 0; i < argc; i++) {
		str = argv[i];
		if ((*len)(str, &size))
			goto error;
		x = make_stringu(size + 1UL);
		if (x == NULL)
			goto error;
		if ((*make)(x->ptr, str)) {
			free_stringu(x);
			goto error;
		}
		x->ptr[size] = 0;
		r[i] = x;
	}
	return r;

error:
	for (k = 0; k < i; i++) {
		free_stringu(r[k]);
		goto error;
	}
	free(r);
	return NULL;
}

static int stringu_utf8_strlen(const void *ptr, size_t *ret)
{
	return UTF8_null_strlen((const byte *)ptr, ret);
}
static int stringu_utf8_make(unicode *data, const void *ptr)
{
	return UTF8_null_makeunicode(data, (const byte *)ptr);
}
static lispstringu *stringu_utf8(int argc, const void *const *argv)
{
	return stringu_call(argc, argv, stringu_utf8_strlen, stringu_utf8_make);
}

static int stringu_utf16_strlen(const void *ptr, size_t *ret)
{
	return UTF16_null_strlen((const byte16 *)ptr, ret);
}
static int stringu_utf16_make(unicode *data, const void *ptr)
{
	return UTF16_null_makeunicode(data, (const byte16 *)ptr);
}
static lispstringu *stringu_utf16(int argc, const void *const *argv)
{
	return stringu_call(argc, argv, stringu_utf16_strlen, stringu_utf16_make);
}

static lisparrayu arrayu_argv_call(int argc, const void *const *argv,
		lispstringu *(*call)(int, const void *const *))
{
	lisparrayu ptr;
	lispstringu *data;

	ptr = (lisparrayu)malloc(sizeoft(struct lisparrayu_struct));
	if (ptr == NULL)
		return NULL;
	data = (*call)(argc, argv);
	if (data == NULL) {
		free(ptr);
		return NULL;
	}
	ptr->ptr = data;
	ptr->size = (size_t)argc;

	return ptr;
}
lisparrayu arrayu_argv_utf8(int argc, const byte *const *argv)
{
	return arrayu_argv_call(argc, (const void *const *)argv, stringu_utf8);
}
lisparrayu arrayu_argv_utf16(int argc, const byte16 *const *argv)
{
	return arrayu_argv_call(argc, (const void *const *)argv, stringu_utf16);
}

void free_arrayu(lisparrayu ptr)
{
	lispstringu *v;
	size_t size, i;

	if (ptr) {
		v = ptr->ptr;
		size = ptr->size;
		for (i = 0; i < size; i++) {
			free_stringu(v[i]);
			v[i] = NULL;
		}
		free(v);
		ptr->ptr = NULL;
		free(ptr);
	}
}


/*
 *  envirnment - main
 */
static lisptableu make_tableu(size_t size)
{
	struct lispkeyvalueu *table;
	lisptableu ptr;

	ptr = (lisptableu)malloc(sizeoft(struct lisptableu_struct));
	if (ptr == NULL)
		return NULL;
	table = (struct lispkeyvalueu *)calloc(size, sizeoft(struct lispkeyvalueu));
	if (table == NULL) {
		free(ptr);
		return NULL;
	}
	ptr->table = table;
	ptr->size = size;

	return ptr;
}

void free_tableu(lisptableu ptr)
{
	struct lispkeyvalueu *table;
	size_t size, i;

	if (ptr) {
		table = ptr->table;
		size = ptr->size;
		for (i = 0; i < size; i++) {
			free_stringu(table[i].key);
			free_stringu(table[i].value);
		}
		free(table);
		ptr->table = NULL;
		free(ptr);
	}
}

static int findchar_stringu(lispstringu str, unicode v, size_t *ret)
{
	unicode *u;
	size_t size, i;

	u = str->ptr;
	size = str->size;
	for (i = 0; i < size; i++) {
		if (u[i] == v) {
			*ret = i;
			return 1;
		}
	}

	return 0;
}

static int split_keyvalue_main(lispstringu str, lispstringu *key, lispstringu *value)
{
	lispstringu k, v;
	size_t a, b, s;

	s = str->size;
	if (s == 0) {
		k = make_stringu(1);
		if (k == NULL)
			return 1;
		v = make_stringu(1);
		if (v == NULL) {
			free_stringu(k);
			return 1;
		}
		a = b = 0;
		goto result;
	}
	s--;
	if (findchar_stringu(str, '=', &a))
		b = s - a - 1UL;
	else {
		a = s - 1UL;
		b = 0;
	}
	k = make_stringu(a + 1UL);
	if (k == NULL)
		return 1;
	v = make_stringu(b + 1UL);
	if (v == NULL) {
		free_stringu(k);
		return 1;
	}
	memcpy(k->ptr, str->ptr, a * sizeoft(unicode));
	memcpy(v->ptr, str->ptr+a+1, b * sizeoft(unicode));

result:
	k->ptr[a] = 0;
	v->ptr[b] = 0;
	*key = k;
	*value = v;

	return 0;
}

lisptableu tableu_env_main(const byte *const *env)
{
	int i, size;
	lisparrayu a;
	lispstringu *pa, k, v;
	lisptableu b;
	struct lispkeyvalueu *pb;

	for (size = 0; env[size]; size++)
		continue;
	a = arrayu_argv_utf8(size, env);
	if (a == NULL)
		return NULL;
	b = make_tableu((size_t)size);
	if (b == NULL) {
		free_arrayu(a);
		return NULL;
	}
	pa = a->ptr;
	pb = b->table;
	for (i = 0; i < size; i++) {
		if (split_keyvalue_main(pa[i], &k, &v))
			goto error;
		pb[i].key = k;
		pb[i].value = v;
	}
	return b;

error:
	free_arrayu(a);
	free_tableu(b);
	return NULL;
}


/*
 *  envirnment - windows
 */
static lispstringu *stringu_windows(int argc, const void *const *env)
{
	int i, k;
	lispstringu x, *r;
	const byte16 *str;
	size_t size;

	str = (const byte16 *)env;
	r = (lispstringu *)malloc(sizeoft(lispstringu) * argc);
	if (r == NULL)
		return NULL;
	for (i = 0; i < argc; i++) {
		if (UTF16_null_strlen(str, &size))
			goto error;
		x = make_stringu(size + 1UL);
		if (x == NULL)
			goto error;
		if (UTF16_null_makeunicode(x->ptr, str)) {
			free_stringu(x);
			goto error;
		}
		x->ptr[size] = 0;
		r[i] = x;
		while (*str) str++;
		str++;
	}
	return r;

error:
	for (k = 0; k < i; i++) {
		free_stringu(r[k]);
		goto error;
	}
	free(r);
	return NULL;
}

lisptableu tableu_env_windows(const byte16 *env)
{
	int i, size;
	const byte16 *p;
	lisparrayu a;
	lispstringu *pa, k, v;
	lisptableu b;
	struct lispkeyvalueu *pb;

	/* length */
	size = 0;
	for (p = env; *p; p++) {
		while (*p) p++;
		size++;
	}

	/* lisparrayu */
	a = arrayu_argv_call(size, (const void *const *)env, stringu_windows);
	if (a == NULL)
		return NULL;
	b = make_tableu((size_t)size);
	if (b == NULL) {
		free_arrayu(a);
		return NULL;
	}
	pa = a->ptr;
	pb = b->table;
	for (i = 0; i < size; i++) {
		if (split_keyvalue_main(pa[i], &k, &v))
			goto error;
		pb[i].key = k;
		pb[i].value = v;
	}
	return b;

error:
	free_arrayu(a);
	free_tableu(b);
	return NULL;
}

lispstringu findchar_tableu(lisptableu env, const char *key)
{
	struct lispkeyvalueu *ptr;
	size_t size, i;

	ptr = env->table;
	size = env->size;
	for (i = 0; i < size; i++) {
		if (equalchar_stringu(ptr[i].key, key))
			return ptr[i].value;
	}

	return NULL;
}


/************************************************************
 *  make.c
 ************************************************************/

/*
 *  code
 */
static code_make_calltype CodeMakeTable[EVAL_PARSE_SIZE];

void set_code_make_struct(struct code_make_struct *str, Execute ptr, addr code)
{
	str->escape = 0;
	str->ptr = ptr;
	str->local = ptr->local;
	str->code = code;
}

int code_make_execute_(CodeMake ptr, addr scope)
{
	EvalParse type;
	code_make_calltype call_;

	Check(! eval_scope_p(scope), "type error");
	GetEvalScopeType(scope, &type);
	call_ = CodeMakeTable[type];
	if (call_ == NULL)
		return fmte_("Invalid scope type.", NULL);

	return (*call_)(ptr, scope);
}

int code_make_(Execute ptr, addr *ret, addr scope)
{
	addr code;
	LocalRoot local;
	LocalStack stack;
	struct code_make_struct str;

	local = ptr->local;
	push_local(local, &stack);
	code_queue_local(local, &code);
	set_code_make_struct(&str, ptr, code);
	Return(code_make_execute_set_(&str, scope));
	code_queue_pop(&str, ret);
	rollback_local(local, stack);

	return 0;
}


/*
 *  table
 */
void init_make(void)
{
	CodeMakeTable[EVAL_PARSE_NIL] = code_make_nil_;
	CodeMakeTable[EVAL_PARSE_T] = code_make_t_;
	CodeMakeTable[EVAL_PARSE_CLOS] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_INTEGER] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_RATIONAL] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_COMPLEX] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_CHARACTER] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_ARRAY] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_VECTOR] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_BITVECTOR] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_STRING] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_SYMBOL] = code_make_symbol_;
	CodeMakeTable[EVAL_PARSE_FLOAT] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_DECLAIM] = code_make_declaim_;
	CodeMakeTable[EVAL_PARSE_PACKAGE] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_RANDOM_STATE] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_PATHNAME] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_ENVIRONMENT] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_PAPER] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_LEXICAL] = code_make_lexical_;
	CodeMakeTable[EVAL_PARSE_PROGN] = code_make_progn_;
	CodeMakeTable[EVAL_PARSE_LET] = code_make_let_;
	CodeMakeTable[EVAL_PARSE_LETA] = code_make_leta_;
	CodeMakeTable[EVAL_PARSE_SETQ] = code_make_setq_;
	CodeMakeTable[EVAL_PARSE_DEFUN] = code_make_defun_;
	CodeMakeTable[EVAL_PARSE_DEFMACRO] = code_make_defmacro_;
	CodeMakeTable[EVAL_PARSE_MACRO_LAMBDA] = code_make_macro_lambda_;
	CodeMakeTable[EVAL_PARSE_DEFTYPE] = code_make_deftype_;
	CodeMakeTable[EVAL_PARSE_DEFINE_COMPILER_MACRO] = code_make_define_compiler_macro_;
	CodeMakeTable[EVAL_PARSE_DESTRUCTURING_BIND] = code_make_destructuring_bind_;
	CodeMakeTable[EVAL_PARSE_QUOTE] = code_make_value2_;
	CodeMakeTable[EVAL_PARSE_FUNCTION] = code_make_function_;
	CodeMakeTable[EVAL_PARSE_LAMBDA] = code_make_lambda_;
	CodeMakeTable[EVAL_PARSE_IF] = code_make_if_;
	CodeMakeTable[EVAL_PARSE_UNWIND_PROTECT] = code_make_unwind_protect_;
	CodeMakeTable[EVAL_PARSE_TAGBODY] = code_make_tagbody_;
	CodeMakeTable[EVAL_PARSE_GO] = code_make_go_;
	CodeMakeTable[EVAL_PARSE_BLOCK] = code_make_block_;
	CodeMakeTable[EVAL_PARSE_RETURN_FROM] = code_make_return_from_;
	CodeMakeTable[EVAL_PARSE_CATCH] = code_make_catch_;
	CodeMakeTable[EVAL_PARSE_THROW] = code_make_throw_;
	CodeMakeTable[EVAL_PARSE_FLET] = code_make_flet_;
	CodeMakeTable[EVAL_PARSE_LABELS] = code_make_labels_;
	CodeMakeTable[EVAL_PARSE_THE] = code_make_the_;
	CodeMakeTable[EVAL_PARSE_EVAL_WHEN] = code_make_eval_when_;
	CodeMakeTable[EVAL_PARSE_VALUES] = code_make_values_;
	CodeMakeTable[EVAL_PARSE_LOCALLY] = code_make_locally_;
	CodeMakeTable[EVAL_PARSE_CALL] = code_make_call_;
	CodeMakeTable[EVAL_PARSE_MULTIPLE_VALUE_BIND] = code_make_multiple_value_bind_;
	CodeMakeTable[EVAL_PARSE_MULTIPLE_VALUE_CALL] = code_make_multiple_value_call_;
	CodeMakeTable[EVAL_PARSE_MULTIPLE_VALUE_PROG1] = code_make_multiple_value_prog1_;
	CodeMakeTable[EVAL_PARSE_NTH_VALUE] = code_make_nth_value_;
	CodeMakeTable[EVAL_PARSE_PROGV] = code_make_progv_;
	CodeMakeTable[EVAL_PARSE_LOAD_TIME_VALUE] = code_make_load_time_value_;
	CodeMakeTable[EVAL_PARSE_STEP] = code_make_step_;
}


/************************************************************
 *  make_call.c
 ************************************************************/

/*
 *  specialized call
 */
static int code_make_specialize_common_p(addr call)
{
	addr common;

	if (RefEvalScopeType(call) != EVAL_PARSE_FUNCTION)
		return 0;
	GetEvalScopeIndex(call, 0, &call);
	getname_tablefunction(call, &call);
	if (RefCallNameType(call) != CALLNAME_SYMBOL)
		return 0;
	GetCallName(call, &call);
	GetPackageSymbol(call, &call);
	GetConst(PACKAGE_COMMON_LISP, &common);

	return call == common;
}

static int code_make_specialize_symbol_p(addr call, constindex index)
{
	addr left, right;

	if (RefEvalScopeType(call) != EVAL_PARSE_FUNCTION)
		return 0;
	GetConstant(index, &left);
	GetEvalScopeIndex(call, 0, &call);
	getname_tablefunction(call, &call);
	GetCallName(call, &right);
	return RefCallNameType(call) == CALLNAME_SYMBOL && left == right;
}
#define CodeMakeSpeciailizedSymbolP(x,y) \
	code_make_specialize_symbol_p((x),CONSTANT_SYSTEM_##y)

static int code_make_specialize_allcons_(CodeMake ptr, addr args, addr escape)
{
	addr pos;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		getvalue_tablecall(pos, &pos);
		Return(code_make_execute_set_(ptr, pos));
		code_jump_escape_wake(ptr, escape);
	}

	return 0;
}

static int code_make_specialize_body_(CodeMake ptr, addr scope, int *ret)
{
	addr args, escape, normal, finish;
	fixnum id;

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &normal);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* body */
	GetEvalScopeIndex(scope, 1, &args);
	Return(code_make_specialize_allcons_(ptr, args, escape));
	code_queue_goto(ptr, normal);

	/* escape */
	code_queue_push_label(ptr, escape);
	CodeQueue_cons(ptr, REVERT_GOTO, normal);
	code_make_end(ptr, id);
	code_queue_goto(ptr, finish);

	/* normal */
	code_queue_push_label(ptr, normal);
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);

	/* finish */
	code_queue_push_label(ptr, finish);

	return Result(ret, 1);
}

static int code_make_specialize_restart_progn_(CodeMake ptr, addr scope, int *ret)
{
	addr args, list, call;
	addr escape, normal, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 1, &args);
	Return_getcons(args, &list, &args);
	Return_getcons(args, &call, &args);
	if (args != Nil)
		return fmte_("Invalid restart-progn call, ~S.", args, NULL);
	getvalue_tablecall(list, &list);
	getvalue_tablecall(call, &call);

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &normal);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* body */
	Return(code_make_execute_set_(ptr, list));
	code_jump_escape_wake(ptr, escape);
	CodeQueue_single(ptr, RESTART_PROGN);
	Return(code_make_execute_set_(ptr, call));
	code_jump_escape_wake(ptr, escape);
	code_queue_goto(ptr, normal);

	/* escape */
	code_queue_push_label(ptr, escape);
	CodeQueue_cons(ptr, REVERT_GOTO, normal);
	code_make_end(ptr, id);
	code_queue_goto(ptr, finish);

	/* normal */
	code_queue_push_label(ptr, normal);
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);

	/* finish */
	code_queue_push_label(ptr, finish);

	return Result(ret, 1);
}

static int code_make_specialize_type_body_(CodeMake ptr,
		addr scope, constindex type, addr escape)
{
	addr args, pos;

	GetEvalScopeIndex(scope, 1, &args);
	while (args != Nil) {
		GetCons(args, &pos, &args);
		getvalue_tablecall(pos, &pos);
		Return(code_make_execute_push_(ptr, pos));
		code_jump_escape_wake(ptr, escape);
	}
	code_queue_single(ptr, type);
	code_jump_escape_wake(ptr, escape);

	return 0;
}

static int code_make_specialize_type_(CodeMake ptr,
		addr scope, constindex type, int *ret)
{
	addr escape;

	code_queue_make_label(ptr, &escape);
	Return(code_make_specialize_type_body_(ptr, scope, type, escape));
	code_queue_push_label(ptr, escape);

	return Result(ret, 1);
}

static int code_make_specialize_handler_bind_(CodeMake ptr, addr scope, int *ret)
{
	return code_make_specialize_type_(ptr, scope, CONSTANT_CODE_HANDLER_BIND, ret);
}

static int code_make_specialize_handler_case_(CodeMake ptr, addr scope, int *ret)
{
	return code_make_specialize_type_(ptr, scope, CONSTANT_CODE_HANDLER_CASE, ret);
}

static int code_make_specialize_restart_bind_(CodeMake ptr, addr scope, int *ret)
{
	return code_make_specialize_type_(ptr, scope, CONSTANT_CODE_RESTART_BIND, ret);
}

static int code_make_specialize_restart_case_(CodeMake ptr, addr scope, int *ret)
{
	return code_make_specialize_type_(ptr, scope, CONSTANT_CODE_RESTART_CASE, ret);
}

static int code_make_specialize_(CodeMake ptr, addr scope, int *ret)
{
	addr call;

	GetEvalScopeIndex(scope, 0, &call);

	/* common-lisp */
	if (code_make_specialize_common_p(call))
		return optimize_common_(ptr, scope, ret);

	/* lisp-system::handler */
	if (CodeMakeSpeciailizedSymbolP(call, HANDLER))
		return code_make_specialize_body_(ptr, scope, ret);

	/* lisp-system::restart */
	if (CodeMakeSpeciailizedSymbolP(call, RESTART))
		return code_make_specialize_body_(ptr, scope, ret);

	/* lisp-system::restart-progn */
	if (CodeMakeSpeciailizedSymbolP(call, RESTART_PROGN))
		return code_make_specialize_restart_progn_(ptr, scope, ret);

	/* lisp-system::handler-bind */
	if (CodeMakeSpeciailizedSymbolP(call, HANDLER_BIND))
		return code_make_specialize_handler_bind_(ptr, scope, ret);

	/* lisp-system::handler-case */
	if (CodeMakeSpeciailizedSymbolP(call, HANDLER_CASE))
		return code_make_specialize_handler_case_(ptr, scope, ret);

	/* lisp-system::restart-bind */
	if (CodeMakeSpeciailizedSymbolP(call, RESTART_BIND))
		return code_make_specialize_restart_bind_(ptr, scope, ret);

	/* lisp-system::restart-case */
	if (CodeMakeSpeciailizedSymbolP(call, RESTART_CASE))
		return code_make_specialize_restart_case_(ptr, scope, ret);

	/* lisp-system::optimize-check */
	if (CodeMakeSpeciailizedSymbolP(call, OPTIMIZE_CHECK))
		return optimize_check_code_(ptr, scope, ret);

	return Result(ret, 0);
}


/*
 *  call
 */
static int code_make_call_push_escape_(CodeMake ptr, addr value, addr escape)
{
	unsigned escape_p;

	escape_p = ptr->escape;
	ptr->escape = 0;
	Return(code_make_execute_push_(ptr, value));
	if (ptr->escape)
		code_jump_escape_wake(ptr, escape);
	ptr->escape |= escape_p;

	return 0;
}

static int code_make_call_args_push_(CodeMake ptr, addr pos, addr escape)
{
	addr value;

	/* value */
	getvalue_tablecall(pos, &value);
	Return(code_make_call_push_escape_(ptr, value, escape));

	/* type */
	if (getcheck_tablecall(pos)) {
		gettype_tablecall(pos, &value);
		if (! type_astert_p(value)) {
			CodeQueue_cons(ptr, CALL_TYPE, value);
			code_jump_escape_wake(ptr, escape);
		}
	}

	return 0;
}

static int code_make_call_args_type_(CodeMake ptr, addr args, addr escape)
{
	addr pos;

	while (consp_getcons(args, &pos, &args)) {
		Return(code_make_call_args_push_(ptr, pos, escape));
	}

	return 0;
}

static int code_make_call_args_count_(CodeMake ptr,
		addr list, addr args, addr escape, addr *ret)
{
	addr pos;

	while (list != Nil) {
		GetCdr(list, &list);
		if (! consp_getcons(args, &pos, &args))
			break;
		Return(code_make_call_args_push_(ptr, pos, escape));
	}

	return Result(ret, args);
}

static int code_make_call_type(addr pos, addr *ret)
{
	GetEvalScopeThe(pos, &pos);
	if (! type_function_p(pos))
		return 1;
	GetArrayType(pos, 0, &pos); /* args */
	if (type_asterisk_p(pos))
		return 1;
	*ret = pos;
	return 0;
}

static int code_make_call_args_value_(CodeMake ptr, addr pos, addr key, addr escape)
{
	addr value;

	getvalue_tablecall(pos, &value);

	/* setmode */
	Return(code_make_execute_set_(ptr, value));
	code_jump_escape_wake(ptr, escape);

	/* type check */
	if (getcheck_tablecall(pos)) {
		gettype_tablecall(pos, &value);
		if (! type_astert_p(value)) {
			CodeQueue_cons(ptr, TYPE_RESULT, value);
			code_jump_escape_wake(ptr, escape);
		}
	}

	/* key */
	CodeQueue_cons(ptr, CALL_KEY, key);
	code_jump_escape_wake(ptr, escape);

	/* push */
	CodeQueue_single(ptr, PUSH_RESULT);

	return 0;
}

static int code_make_call_args_key_(CodeMake ptr, addr key, addr args, addr escape)
{
	int kv;
	addr pos;

	for (kv = 0; args != Nil; kv = ! kv) {
		GetCons(args, &pos, &args);
		if (kv == 0) {
			Return(code_make_call_args_push_(ptr, pos, escape));
		}
		else {
			Return(code_make_call_args_value_(ptr, pos, key, escape));
		}
	}

	return 0;
}

static int code_make_call_args_(CodeMake ptr, addr first, addr args, addr escape)
{
	addr var, opt, key;

	/* type */
	if (code_make_call_type(first, &first))
		return code_make_call_args_type_(ptr, args, escape);

	GetArrayA2(first, 0, &var);
	GetArrayA2(first, 1, &opt);
	GetArrayA2(first, 3, &key);

	/* var, &optional */
	Return(code_make_call_args_count_(ptr, var, args, escape, &args));
	Return(code_make_call_args_count_(ptr, opt, args, escape, &args));
	if (! consp(args))
		return 0;

	/* not key */
	if (! consp(key))
		return code_make_call_args_type_(ptr, args, escape);

	/* key */
	Return(code_make_call_args_key_(ptr, key, args, escape));

	return 0;
}

static void code_make_call_global(CodeMake ptr, addr pos)
{
	addr symbol;

	getname_tablefunction(pos, &pos);
	GetCallName(pos, &symbol);
	if (symbolp_callname(pos)) {
		CodeQueue_cons(ptr, CALL_FUNCTION, symbol);
	}
	else {
		CodeQueue_cons(ptr, CALL_SETF, symbol);
	}
}

static void code_make_call_lexical(CodeMake ptr, addr pos)
{
	index_heap(&pos, getlexical_tablefunction(pos));
	CodeQueue_cons(ptr, CALL_LEXICAL, pos);
}

static void code_make_call_function(CodeMake ptr, addr table)
{
	if (getglobalp_tablefunction(table))
		code_make_call_global(ptr, table);
	else
		code_make_call_lexical(ptr, table);
}

static int code_make_call_first_(CodeMake ptr, addr pos, addr escape)
{
	addr table;

	switch (RefEvalScopeType(pos)) {
		case EVAL_PARSE_FUNCTION:
			GetEvalScopeIndex(pos, 0, &table);
			code_make_call_function(ptr, table);
			code_jump_escape_wake(ptr, escape);
			return 0;

		case EVAL_PARSE_LAMBDA:
			/* execute */
			Return(code_make_execute_set_(ptr, pos));
			code_jump_escape_wake(ptr, escape);
			/* call */
			CodeQueue_single(ptr, CALL_RESULT);
			code_jump_escape_wake(ptr, escape);
			return 0;

		default:
			return fmte_("Invaild call type.", NULL);
	}
}

static int code_make_call_name_(CodeMake ptr, addr pos)
{
	switch (RefEvalScopeType(pos)) {
		case EVAL_PARSE_FUNCTION:
			GetEvalScopeIndex(pos, 0, &pos);
			getname_tablefunction(pos, &pos);
			CodeQueue_cons(ptr, CALL_NAME, pos);
			return 0;

		case EVAL_PARSE_LAMBDA:
			GetConst(COMMON_LAMBDA, &pos);
			CodeQueue_cons(ptr, CALL_NAME, pos);
			return 0;

		default:
			return fmte_("Invaild call type.", NULL);
	}
}

static int code_make_call_call_(CodeMake ptr, addr scope)
{
	int check;
	addr first, args, escape;
	fixnum id;

	Return(code_make_specialize_(ptr, scope, &check));
	if (check)
		return 0;

	/* call */
	GetEvalScopeIndex(scope, 0, &first);
	GetEvalScopeIndex(scope, 1, &args);

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_make_begin(ptr, &id);

	/* execute */
	Return(code_make_call_name_(ptr, first));
	Return(code_make_call_args_(ptr, first, args, escape));
	Return(code_make_call_first_(ptr, first, escape));

	/* end */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);

	return 0;
}

int code_make_call_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_call_call_);
}


/************************************************************
 *  make_function.c
 ************************************************************/

int code_allcons_(CodeMake ptr, addr cons, addr escape)
{
	int make_label_p, escape_p;
	addr pos;
	modeswitch mode;

	/* nil */
	if (cons == Nil)
		return code_make_nil_(ptr, NULL);

	/* escape */
	make_label_p = (escape == NULL);
	if (make_label_p)
		code_queue_make_label(ptr, &escape);

	/* butlast */
	escape_p = ptr->escape;
	code_queue_remmode(ptr, &mode);
	for (;;) {
		GetCons(cons, &pos, &cons);
		if (cons == Nil)
			break;

		code_escape_clear(ptr);
		Return(code_make_execute_(ptr, pos));
		if (code_escape_get(ptr)) {
			code_jump_escape(ptr, escape);
			escape_p = 1;
		}
	}
	code_queue_rollback(ptr, &mode);

	/* last */
	code_escape_clear(ptr);
	Return(code_make_execute_(ptr, pos));
	if (code_escape_get(ptr)) {
		code_jump_escape(ptr, escape);
		escape_p = 1;
	}

	/* escape */
	if (make_label_p)
		code_queue_push_label(ptr, escape);

	/* result */
	ptr->escape = escape_p;
	return 0;
}

int code_allcons_set_(CodeMake ptr, addr cons, addr escape)
{
	modeswitch mode;

	code_queue_setmode(ptr, &mode);
	Return(code_allcons_(ptr, cons, escape));
	code_queue_rollback(ptr, &mode);

	return 0;
}

int code_allcons_rem_(CodeMake ptr, addr cons, addr escape)
{
	int make_label_p, escape_p;
	addr pos;
	modeswitch mode;

	/* nil */
	if (cons == Nil)
		return 0;

	/* escape */
	make_label_p = (escape == NULL);
	if (make_label_p)
		code_queue_make_label(ptr, &escape);

	/* cons */
	escape_p = ptr->escape;
	code_queue_remmode(ptr, &mode);
	for (;;) {
		GetCons(cons, &pos, &cons);
		if (cons == Nil)
			break;

		code_escape_clear(ptr);
		Return(code_make_execute_(ptr, pos));
		if (code_escape_get(ptr)) {
			code_jump_escape(ptr, escape);
			escape_p = 1;
		}
	}

	/* last */
	code_escape_clear(ptr);
	Return(code_make_execute_(ptr, pos));
	if (code_escape_get(ptr)) {
		code_jump_escape(ptr, escape);
		escape_p = 1;
	}
	code_queue_rollback(ptr, &mode);

	/* escape */
	if (make_label_p)
		code_queue_push_label(ptr, escape);

	/* result */
	ptr->escape = escape_p;
	return 0;
}


/* function */
static void code_function_object(CodeMake ptr, addr pos)
{
	CheckTypeCodeQueue(ptr->code);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(ptr, SET, pos);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(ptr, PUSH, pos);
			break;

		case CodeQueue_ModeRemove:
		default:
			break;
	}
}

static void code_function_global(CodeMake ptr, addr pos)
{
	int symbolp;
	constindex index;
	addr symbol;

	getname_tablefunction(pos, &pos);
	symbolp = symbolp_callname(pos);
	GetCallName(pos, &symbol);

	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			index = ConstantCode(symbolp, FUNCTION_SET, SETF_SET);
			code_queue_cons(ptr, index, symbol);
			code_escape_wake(ptr);
			break;

		case CodeQueue_ModePush:
			index = ConstantCode(symbolp, FUNCTION_PUSH, SETF_PUSH);
			code_queue_cons(ptr, index, symbol);
			code_escape_wake(ptr);
			break;

		case CodeQueue_ModeRemove:
		default:
			return;
	}
}

static void code_function_lexical(CodeMake ptr, addr pos)
{
	index_heap(&pos, getlexical_tablefunction(pos));
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(ptr, LEXICAL_SET, pos);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(ptr, LEXICAL_PUSH, pos);
			break;

		case CodeQueue_ModeRemove:
		default:
			return;
	}
}

static int code_make_function_call_(CodeMake ptr, addr scope)
{
	GetEvalScopeIndex(scope, 0, &scope);
	if (functionp(scope))
		code_function_object(ptr, scope);
	else if (getglobalp_tablefunction(scope))
		code_function_global(ptr, scope);
	else
		code_function_lexical(ptr, scope);

	return 0;
}

int code_make_function_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_function_call_);
}


/* lambda */
static void code_ordinary_bind_value(CodeMake ptr, addr pos, addr escape)
{
	addr value;

	code_make_type_value(ptr, pos, escape);
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &value);
		CodeQueue_cons(ptr, LETA_SPECIAL, value);
	}
	else {
		index_heap(&value, getlexical_tablevalue(pos));
		CodeQueue_cons(ptr, SETQ_LEXICAL, value);
	}
}

static void code_ordinary_bind_var(CodeMake ptr, addr list, addr escape)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		CodeQueue_single(ptr, POP);
		code_jump_escape_wake(ptr, escape);
		code_ordinary_bind_value(ptr, pos, escape);
	}
}

static int code_ordinary_bind_init_(CodeMake ptr,
		addr var, addr init, addr svar, addr escape)
{
	addr label, finish;

	/* label */
	code_queue_make_label(ptr, &label);
	code_queue_make_label(ptr, &finish);

	/* if-exists */
	code_queue_if_unbound(ptr, label);
	code_ordinary_bind_value(ptr, var, escape);
	if (svar != Nil) {
		CodeQueue_single(ptr, T_SET);
		code_ordinary_bind_value(ptr, svar, escape);
	}
	code_queue_goto(ptr, finish);

	/* if-does-not-exist */
	code_queue_push_label(ptr, label);
	Return(code_make_execute_set_(ptr, init));
	code_jump_escape_wake(ptr, escape);

	code_ordinary_bind_value(ptr, var, escape);
	if (svar != Nil) {
		CodeQueue_single(ptr, NIL_SET);
		code_ordinary_bind_value(ptr, svar, escape);
	}

	/* finish */
	code_queue_push_label(ptr, finish);

	return 0;
}

static int code_ordinary_bind_opt_(CodeMake ptr, addr list, addr escape)
{
	addr pos, var, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, &svar, NULL);
		CodeQueue_single(ptr, POP_UNBOUND);
		code_jump_escape_wake(ptr, escape);
		Return(code_ordinary_bind_init_(ptr, var, init, svar, escape));
	}

	return 0;
}

static void code_ordinary_bind_rest(CodeMake ptr, addr pos, addr escape)
{
	if (pos != Nil) {
		CodeQueue_single(ptr, REST_COPY);
		code_ordinary_bind_value(ptr, pos, escape);
	}
}

static int code_ordinary_bind_key_(CodeMake ptr, addr list, addr escape)
{
	addr pos, var, name, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &name, &init, &svar, NULL);
		CodeQueue_cons(ptr, GETF, name);
		code_jump_escape_wake(ptr, escape);
		Return(code_ordinary_bind_init_(ptr, var, init, svar, escape));
	}

	return 0;
}

static void code_ordinary_bind_allow(CodeMake ptr,
		addr rest, addr list, addr allow, addr escape)
{
	addr name, keys;

	if (rest != Nil || allow != Nil)
		return;
	/* check */
	keys = Nil;
	while (list != Nil) {
		GetCons(list, &name, &list);
		GetCdr(name, &name); /* var */
		GetCar(name, &name); /* name */
		cons_heap(&keys, name, keys);
	}
	if (keys != Nil) {
		CodeQueue_cons(ptr, ALLOW_OTHER_KEYS, keys);
		code_jump_escape_wake(ptr, escape);
	}
}

static int code_ordinary_bind_aux_(CodeMake ptr, addr list, addr escape)
{
	addr pos, var, init;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, NULL);
		Return(code_make_execute_set_(ptr, init));
		code_jump_escape_wake(ptr, escape);
		code_ordinary_bind_value(ptr, var, escape);
	}

	return 0;
}


/*
 *  lambda
 */
static void code_lambda_lexical(CodeMake ptr, addr scope)
{
	addr list;

	GetEvalScopeIndex(scope, EvalLambda_Lexical, &list);
	if (list != Nil) {
		CodeQueue_cons(ptr, LAMBDA_LEXICAL, list);
	}
}

static int code_lambda_args_(CodeMake ptr, addr scope, addr escape)
{
	addr list, var, opt, rest, key, allow, aux;

	GetEvalScopeIndex(scope, EvalLambda_Args, &list);
	List_bind(list, &var, &opt, &rest, &key, &allow, &aux, NULL);
	code_ordinary_bind_var(ptr, var, escape);
	Return(code_ordinary_bind_opt_(ptr, opt, escape));
	code_ordinary_bind_rest(ptr, rest, escape);
	if (rest == Nil && key == Nil) {
		CodeQueue_cons(ptr, REST_NULL, allow);
		code_jump_escape_wake(ptr, escape);
	}
	Return(code_ordinary_bind_key_(ptr, key, escape));
	code_ordinary_bind_allow(ptr, rest, key, allow, escape);
	Return(code_ordinary_bind_aux_(ptr, aux, escape));

	return 0;
}

static int code_lambda_body_(CodeMake ptr, addr scope, addr escape)
{
	addr list;
	GetEvalScopeIndex(scope, EvalLambda_Cons, &list);
	return code_allcons_set_(ptr, list, escape);
}

/* closure */
static int code_lambda_closure_table_(addr pos, addr *ret)
{
	enum EvalTable type;
	addr x, y, z;
	size_t src, dst;

	CheckTableTable(pos);
	type = gettype_evaltable(pos);
	get_evaltable(pos, &pos);
	switch (type) {
		case EvalTable_Value:
			src = getclosure_tablevalue(pos); /* from */
			dst = getlexical_tablevalue(pos); /* to */
			break;

		case EvalTable_Function:
			src = getclosure_tablefunction(pos); /* from */
			dst = getlexical_tablefunction(pos); /* to */
			break;

		case EvalTable_TagBody:
			src = getclosure_tabletagbody(pos); /* from */
			dst = getlexical_tabletagbody(pos); /* to */
			break;

		case EvalTable_Block:
			src = getclosure_tableblock(pos); /* from */
			dst = getlexical_tableblock(pos); /* to */
			break;

		default:
			*ret = Nil;
			return fmte_("Invalid eval-table type.", NULL);
	}

	/* result */
	fixnum_heap(&x, (fixnum)type);
	index_heap(&y, src);
	index_heap(&z, dst);
	list_heap(ret, x, y, z, NULL);

	return 0;
}

static int code_lambda_closure_list_(addr list, addr *ret)
{
	addr root, pos;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(code_lambda_closure_table_(pos, &pos));
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);

	return 0;
}

static void code_lambda_self(addr scope, addr *ret)
{
	addr call, pos, x, y, z;

	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	GetEvalScopeIndex(scope, EvalLambda_Table, &pos);
	if ((call != Nil) && (pos != Nil) && getreference_tablefunction(pos)) {
		fixnum_heap(&x, EvalTable_Self);
		index_heap(&y, 0);
		index_heap(&z, getlexical_tablefunction(pos));
		list_heap(ret, x, y, z, NULL);
	}
	else {
		*ret = Nil;
	}
}

static int code_lambda_closure_(addr scope, addr *ret)
{
	addr list, pos;

	/* closure */
	GetEvalScopeIndex(scope, EvalLambda_Clos, &list);
	Return(code_lambda_closure_list_(list, &list));

	/* self */
	code_lambda_self(scope, &pos);
	if (pos != Nil)
		cons_heap(&list, pos, list);

	/* result */
	return Result(ret, list);
}

static int code_lambda_info_(CodeMake ptr, addr scope)
{
	addr pos;

	/* name */
	GetEvalScopeIndex(scope, EvalLambda_Call, &pos);
	if (pos != Nil)
		CodeQueue_cons(ptr, LAMBDA_NAME, pos);

	/* type */
	GetEvalScopeIndex(scope, EvalLambda_The, &pos);
	if (pos != Nil)
		CodeQueue_cons(ptr, LAMBDA_TYPE, pos);

	/* documentation */
	GetEvalScopeIndex(scope, EvalLambda_Doc, &pos);
	if (pos != Nil)
		CodeQueue_cons(ptr, LAMBDA_DOC, pos);

	/* form */
	GetEvalScopeIndex(scope, EvalLambda_Form, &pos);
	if (pos != Nil)
		CodeQueue_cons(ptr, LAMBDA_FORM, pos);

	/* defun */
	GetEvalScopeIndex(scope, EvalLambda_Defun, &pos);
	if (pos != Nil)
		CodeQueue_cons(ptr, LAMBDA_DEFUN, pos);

	/* closure */
	Return(code_lambda_closure_(scope, &pos));
	if (pos != Nil)
		CodeQueue_cons(ptr, LAMBDA_CLOSURE, pos);

	return 0;
}

static int code_lambda_function_(CodeMake ptr, addr scope)
{
	addr pos, escape;
	fixnum id;

	/* begin */
	code_queue_push_code(ptr);
	code_make_begin_call(ptr, &id);
	code_queue_make_label(ptr, &escape);

	/* body */
	code_lambda_lexical(ptr, scope);
	Return(code_lambda_args_(ptr, scope, escape));
	Return(code_lambda_body_(ptr, scope, escape));

	/* end */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_pop(ptr, &pos);

	/* result */
	CodeQueue_cons(ptr, LAMBDA, pos);
	Return(code_lambda_info_(ptr, scope));
	code_queue_ifpush(ptr);

	return 0;
}

static int code_make_lambda_cache_p_(addr scope, int *ret)
{
	OptimizeType value;
	struct scope_struct *str;
	addr pos;

	/* closure */
	Return(code_lambda_closure_(scope, &pos));
	if (pos != Nil)
		return Result(ret, 0);

	/* optimize */
	str = StructEvalScope(scope);
	value = str->optimize[EVAL_OPTIMIZE_SPEED];
	return Result(ret, value < 0 || 1 <= value);
}

static int code_make_lambda_call_(CodeMake ptr, addr scope)
{
	int check;
	addr gensym, label;
	modeswitch mode;

	/* rem mode */
	if (code_queue_remp(ptr))
		return 0;

	/* closure check */
	Return(code_make_lambda_cache_p_(scope, &check));
	if (! check)
		return code_lambda_function_(ptr, scope);

	/* cache */
	make_symbolchar(&gensym, "LAMBDA-CACHE");

	/* code */
	code_queue_make_label(ptr, &label);
	code_queue_setmode(ptr, &mode);
	CodeQueue_double(ptr, LAMBDA_CACHE, label, gensym);
	Return(code_lambda_function_(ptr, scope));
	CodeQueue_cons(ptr, LAMBDA_CACHE_SET, gensym);
	code_queue_push_label(ptr, label);
	code_queue_rollback(ptr, &mode);
	code_queue_ifpush(ptr);

	return 0;
}

int code_make_lambda_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_lambda_call_);
}


/* defun */
static int code_lambda_set_(CodeMake ptr, addr scope)
{
	modeswitch mode;

	code_queue_setmode(ptr, &mode);
	Return(code_lambda_function_(ptr, scope));
	code_queue_rollback(ptr, &mode);

	return 0;
}

static int code_make_defun_call_(CodeMake ptr, addr scope)
{
	addr escape;

	code_queue_make_label(ptr, &escape);
	Return(code_lambda_set_(ptr, scope));
	CodeQueue_single(ptr, DEFUN);
	code_jump_escape_wake(ptr, escape);
	code_queue_ifpush(ptr);
	code_queue_push_label(ptr, escape);

	return 0;
}

int code_make_defun_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_defun_call_);
}


/* macro-lambda */
static void code_macro_special(CodeMake, addr);

static void code_macro_special_value(CodeMake ptr, addr pos)
{
	if (pos == Nil)
		return;
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &pos);
		CodeQueue_cons(ptr, MACRO_SPECIAL, pos);
	}
}

static void code_macro_special_variable(CodeMake ptr, addr pos)
{
	if (consp(pos))
		code_macro_special(ptr, pos);
	else
		code_macro_special_value(ptr, pos);
}

static void code_macro_special_var(CodeMake ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		code_macro_special_variable(ptr, pos);
	}
}

static void code_macro_special_car(CodeMake ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCar(pos, &pos);
		code_macro_special_variable(ptr, pos);
	}
}

static void code_macro_special_rest(CodeMake ptr, addr pos)
{
	/* (var . &rest) */
	if (pos != Nil) {
		GetCar(pos, &pos);
		code_macro_special_variable(ptr, pos);
	}
}

static void code_macro_special(CodeMake ptr, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	code_macro_special_value(ptr, env);
	code_macro_special_value(ptr, whole);
	code_macro_special_var(ptr, var);
	code_macro_special_car(ptr, opt);
	code_macro_special_rest(ptr, rest);
	code_macro_special_car(ptr, key);
	code_macro_special_car(ptr, aux);
}

static int code_macro_bind_(CodeMake, addr, addr);

static void code_macro_bind_value(CodeMake ptr, addr pos, addr escape)
{
	addr value;

	code_make_type_value(ptr, pos, escape);
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &value);
		CodeQueue_cons(ptr, SETQ_SPECIAL, value);
		code_jump_escape_wake(ptr, escape);
	}
	else {
		index_heap(&value, getlexical_tablevalue(pos));
		CodeQueue_cons(ptr, SETQ_LEXICAL, value);
	}
}

static int code_macro_bind_push_(CodeMake ptr, addr pos, addr escape)
{
	fixnum id;
	addr finish;

	/* begin */
	code_queue_make_label(ptr, &finish);
	code_make_begin_call(ptr, &id);

	/* bind */
	Return(code_macro_bind_(ptr, pos, finish));

	/* end */
	code_queue_push_label(ptr, finish);
	code_make_end(ptr, id);
	code_jump_escape_wake(ptr, escape);

	return 0;
}

static int code_macro_bind_variable_(CodeMake ptr, addr pos, addr escape)
{
	if (consp(pos)) {
		return code_macro_bind_push_(ptr, pos, escape);
	}
	else {
		code_macro_bind_value(ptr, pos, escape);
		return 0;
	}
}

static int code_macro_bind_var_(CodeMake ptr, addr list, addr escape)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		CodeQueue_single(ptr, POP);
		code_jump_escape_wake(ptr, escape);
		Return(code_macro_bind_variable_(ptr, pos, escape));
	}

	return 0;
}

static int code_macro_bind_init_(CodeMake ptr,
		addr var, addr init, addr svar, addr escape)
{
	addr label, finish;

	/* label */
	code_queue_make_label(ptr, &label);
	code_queue_make_label(ptr, &finish);

	/* if-exists */
	code_queue_if_unbound(ptr, label);
	Return(code_macro_bind_variable_(ptr, var, escape));
	if (svar != Nil) {
		CodeQueue_single(ptr, T_SET);
		code_macro_bind_value(ptr, svar, escape);
	}
	code_queue_goto(ptr, finish);

	/* if-does-not-exist */
	code_queue_push_label(ptr, label);
	Return(code_make_execute_set_(ptr, init));
	code_jump_escape_wake(ptr, escape);

	Return(code_macro_bind_variable_(ptr, var, escape));
	if (svar != Nil) {
		CodeQueue_single(ptr, NIL_SET);
		code_macro_bind_value(ptr, svar, escape);
	}

	/* finish */
	code_queue_push_label(ptr, finish);

	return 0;
}

static int code_macro_bind_opt_(CodeMake ptr, addr list, addr escape)
{
	addr pos, var, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, &svar, NULL);
		CodeQueue_single(ptr, POP_UNBOUND);
		code_jump_escape_wake(ptr, escape);
		Return(code_macro_bind_init_(ptr, var, init, svar, escape));
	}

	return 0;
}

static int code_macro_bind_rest_(CodeMake ptr, addr list, addr escape)
{
	if (list != Nil) {
		GetCar(list, &list);
		CodeQueue_single(ptr, REST_BIND);
		Return(code_macro_bind_variable_(ptr, list, escape));
	}

	return 0;
}

static int code_macro_bind_key_(CodeMake ptr, addr list, addr escape)
{
	addr pos, var, name, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &name, &init, &svar, NULL);
		CodeQueue_cons(ptr, GETF, name);
		code_jump_escape_wake(ptr, escape);
		Return(code_macro_bind_init_(ptr, var, init, svar, escape));
	}

	return 0;
}

static int code_macro_bind_aux_(CodeMake ptr, addr list, addr escape)
{
	addr pos, var, init;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, NULL);
		Return(code_make_execute_set_(ptr, init));
		code_jump_escape_wake(ptr, escape);
		code_macro_bind_value(ptr, var, escape);
	}

	return 0;
}

static int code_macro_bind_list_(CodeMake ptr, addr args, addr escape)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	Return(code_macro_bind_var_(ptr, var, escape));
	Return(code_macro_bind_opt_(ptr, opt, escape));
	if (rest == Nil && key == Nil) {
		CodeQueue_cons(ptr, REST_NULL, allow);
		code_jump_escape_wake(ptr, escape);
	}
	Return(code_macro_bind_rest_(ptr, rest, escape));
	Return(code_macro_bind_key_(ptr, key, escape));
	code_ordinary_bind_allow(ptr, rest, key, allow, escape);
	Return(code_macro_bind_aux_(ptr, aux, escape));

	return 0;
}

static void code_macro_bind_whole(CodeMake ptr, addr pos, addr escape)
{
	if (pos != Nil) {
		code_macro_bind_value(ptr, pos, escape);
	}
	CodeQueue_single(ptr, WHOLE);
}

static int code_macro_bind_(CodeMake ptr, addr args, addr escape)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	code_macro_bind_whole(ptr, whole, escape);
	Return(code_macro_bind_list_(ptr, args, escape));

	return 0;
}

static void code_macro_env(CodeMake ptr, addr env, addr escape)
{
	if (env != Nil) {
		CodeQueue_single(ptr, MACRO_ENV);
		code_jump_escape_wake(ptr, escape);
		code_macro_bind_value(ptr, env, escape);
	}
}

static void code_macro_whole(CodeMake ptr, addr whole, addr escape)
{
	CodeQueue_single(ptr, POP);
	code_jump_escape_wake(ptr, escape);

	if (whole != Nil) {
		code_macro_bind_value(ptr, whole, escape);
	}
	CodeQueue_single(ptr, MACRO_WHOLE);
	code_jump_escape_wake(ptr, escape);
}

static int code_macro_args_(CodeMake ptr, addr scope, addr escape)
{
	addr list, var, opt, rest, key, allow, aux, whole, env;

	GetEvalScopeIndex(scope, EvalLambda_Args, &list);
	code_macro_special(ptr, list);

	List_bind(list, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	code_macro_env(ptr, env, escape);
	code_macro_whole(ptr, whole, escape);
	Return(code_macro_bind_list_(ptr, list, escape));

	return 0;
}

static int code_macro_function_(CodeMake ptr, addr scope)
{
	addr pos, escape;
	fixnum id;

	/* begin */
	code_queue_push_code(ptr);
	code_make_begin_call(ptr, &id);
	code_queue_make_label(ptr, &escape);

	/* body */
	code_lambda_lexical(ptr, scope);
	Return(code_macro_args_(ptr, scope, escape));
	Return(code_lambda_body_(ptr, scope, escape));

	/* end */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_pop(ptr, &pos);

	/* result */
	CodeQueue_cons(ptr, MACRO, pos);
	Return(code_lambda_info_(ptr, scope));
	code_queue_ifpush(ptr);

	return 0;
}

int code_make_macro_lambda_(CodeMake ptr, addr scope)
{
	return code_macro_function_(ptr, scope);
}


/* defmacro */
int code_make_defmacro_(CodeMake ptr, addr scope)
{
	addr name, lambda, escape;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &lambda);

	/* macdro-lambda */
	code_queue_make_label(ptr, &escape);
	Return(code_make_execute_set_(ptr, lambda));
	code_jump_escape_wake(ptr, escape);

	/* defmacro */
	CodeQueue_cons(ptr, DEFMACRO, name);
	code_jump_escape_wake(ptr, escape);
	code_queue_ifpush(ptr);

	/* result */
	code_queue_push_label(ptr, escape);
	return 0;
}


/* deftype */
int code_make_deftype_(CodeMake ptr, addr scope)
{
	addr call, doc, escape;

	Return(code_macro_function_(ptr, scope));
	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	GetEvalScopeIndex(scope, EvalLambda_Doc, &doc);
	CodeQueue_double(ptr, DEFTYPE, call, doc);
	code_queue_make_label(ptr, &escape);
	code_jump_escape_wake(ptr, escape);
	code_queue_ifpush(ptr);
	code_queue_push_label(ptr, escape);

	return 0;
}


/* define-compiler-macro */
int code_make_define_compiler_macro_(CodeMake ptr, addr scope)
{
	addr call, doc, escape;

	Return(code_macro_function_(ptr, scope));
	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	GetEvalScopeIndex(scope, EvalLambda_Doc, &doc);
	CodeQueue_double(ptr, DEFINE_COMPILER_MACRO, call, doc);
	code_queue_make_label(ptr, &escape);
	code_jump_escape_wake(ptr, escape);
	code_queue_ifpush(ptr);
	code_queue_push_label(ptr, escape);

	return 0;
}


/* destructuring-bind */
static int code_bind_args_(CodeMake ptr, addr list, addr escape)
{
	code_macro_special(ptr, list);
	return code_macro_bind_(ptr, list, escape);
}

static int code_make_bind_execute_(CodeMake ptr, addr scope, addr escape)
{
	addr expr, args, body, free, pos;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &args);
	GetEvalScopeIndex(scope, 3, &body);
	GetEvalScopeIndex(scope, 4, &free);

	GetConst(COMMON_DESTRUCTURING_BIND, &pos);
	CodeQueue_cons(ptr, CALL_NAME, pos);

	Return(code_make_execute_set_(ptr, expr));
	code_jump_escape_wake(ptr, escape);
	Return(code_make_free_(ptr, free, escape));
	Return(code_bind_args_(ptr, args, escape));
	Return(code_allcons_set_(ptr, body, escape));

	return 0;
}

static int code_make_destructuring_bind_call_(CodeMake ptr, addr scope)
{
	addr escape, finish;
	fixnum id;

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* body */
	Return(code_make_bind_execute_(ptr, scope, escape));
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);
	code_queue_goto(ptr, finish);

	/* escape */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_push_label(ptr, finish);

	return 0;
}

int code_make_destructuring_bind_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_destructuring_bind_call_);
}


/* flet */
static void code_make_type_function(CodeMake ptr, addr pos, addr escape)
{
	addr type;

	if (getcheck_tablefunction(pos)) {
		gettype_tablefunction(pos, &type);
		if ((! type_astert_p(type)) && (! type_function_aster_p(type))) {
			CodeQueue_cons(ptr, TYPE_RESULT, type);
			code_jump_escape_wake(ptr, escape);
		}
	}
}

static int code_make_flet_args_(CodeMake ptr, addr args, addr escape)
{
	addr pos, value;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		GetCons(pos, &pos, &value);

		Return(code_lambda_set_(ptr, value));
		code_make_type_function(ptr, pos, escape);
		index_heap(&value, getlexical_tablefunction(pos));
		CodeQueue_cons(ptr, SETQ_LEXICAL, value);
	}

	return 0;
}

static int code_make_flet_call_(CodeMake ptr, addr scope)
{
	addr args, body, free, escape;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &body);
	GetEvalScopeIndex(scope, 3, &free);

	code_queue_make_label(ptr, &escape);
	Return(code_make_free_(ptr, free, escape));
	Return(code_make_flet_args_(ptr, args, escape));
	Return(code_allcons_(ptr, body, escape));
	code_queue_push_label(ptr, escape);

	return 0;
}

int code_make_flet_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_flet_call_);
}


/* labels */
static int code_lambda_labels_(CodeMake ptr, addr index, addr scope)
{
	addr pos, escape;
	fixnum id;

	/* code_lambda_function_ */
	code_queue_push_code(ptr);
	code_make_begin_call(ptr, &id);
	code_queue_make_label(ptr, &escape);

	/* callname */
	GetEvalScopeIndex(scope, EvalLambda_Call, &pos);
	CodeQueue_cons(ptr, CALL_NAME, pos);

	/* body */
	code_lambda_lexical(ptr, scope);
	Return(code_lambda_args_(ptr, scope, escape));
	Return(code_lambda_body_(ptr, scope, escape));

	/* end */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_pop(ptr, &pos);

	/* result */
	CodeQueue_double(ptr, LABELS_LAMBDA, index, pos);
	Return(code_lambda_info_(ptr, scope));
	code_queue_ifpush(ptr);

	return 0;
}

static int code_make_labels_args_(CodeMake ptr, addr args, addr escape)
{
	addr pos, scope, index;
	modeswitch mode;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		GetCons(pos, &pos, &scope);

		/* labels arguments */
		index_heap(&index, getlexical_tablefunction(pos));
		code_queue_setmode(ptr, &mode);
		Return(code_lambda_labels_(ptr, index, scope));
		code_queue_rollback(ptr, &mode);
		/* type check */
		code_make_type_function(ptr, pos, escape);
	}

	return 0;
}

static void code_make_labels_lexical(CodeMake ptr, addr args)
{
	addr pos, list, index;

	list = Nil;
	while (args != Nil) {
		GetCons(args, &pos, &args);
		GetCar(pos, &pos);

		index_heap(&index, getlexical_tablefunction(pos));
		cons_heap(&list, index, list);
	}
	CodeQueue_cons(ptr, LABELS_MAKE, list);
}

static int code_make_labels_call_(CodeMake ptr, addr scope)
{
	addr args, body, free, escape;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &body);
	GetEvalScopeIndex(scope, 3, &free);

	code_queue_make_label(ptr, &escape);
	Return(code_make_free_(ptr, free, escape));
	code_make_labels_lexical(ptr, args);
	Return(code_make_labels_args_(ptr, args, escape));
	Return(code_allcons_(ptr, body, escape));
	code_queue_push_label(ptr, escape);

	return 0;
}

int code_make_labels_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_labels_call_);
}


/************************************************************
 *  make_queue.c
 ************************************************************/

/*
 *  code_stack
 */
struct code_stack {
	unsigned finish : 1;
	LocalStack stack;
	size_t size;
};

enum CodeStack_Mode {
	CodeStack_Root,
	CodeStack_Result,
	CodeStack_Size
};

#define RefCodeStack		RefArraySS
#define GetCodeStack		GetArraySS
#define SetCodeStack		SetArraySS
#define PtrCodeStack(x)		PtrBodySSa(x, CodeStack_Size)
#define StructCodeStack(p)	((struct code_stack *)PtrCodeStack(p))

static void alloc_code_stack(LocalRoot local, addr *ret)
{
	alloc_smallsize(local, ret, LISPSYSTEM_EVALSTACK,
			CodeStack_Size,
			sizeoft(struct code_stack));
}

static void code_stack_local(LocalRoot local, addr *ret)
{
	addr pos;
	struct code_stack *ptr;
	LocalStack stack;

	Check(local == NULL, "local error");
	push_local(local, &stack);
	alloc_code_stack(local, &pos);
	ptr = StructCodeStack(pos);
	clearpoint(ptr);
	ptr->stack = stack;
	*ret = pos;
}

static void free_code_stack(LocalRoot local, addr pos)
{
	rollback_local(local, StructCodeStack(pos)->stack);
}

static void push_code_stack(LocalRoot local, addr pos, addr value)
{
	addr root;
	struct code_stack *ptr;

	ptr = StructCodeStack(pos);
	Check(ptr->finish, "finish error");
	GetCodeStack(pos, CodeStack_Root, &root);
	cons_local(local, &root, value, root);
	SetCodeStack(pos, CodeStack_Root, root);
	ptr->size++;
}

static void finish_code_stack(LocalRoot local, addr pos)
{
	addr root;
	struct code_stack *ptr;

	ptr = StructCodeStack(pos);
	Check(ptr->finish, "finish error");
	GetCodeStack(pos, CodeStack_Root, &root);
	nreverse(&root, root);
	SetCodeStack(pos, CodeStack_Root, Nil);
	SetCodeStack(pos, CodeStack_Result, root);
	ptr->finish = 1;
}


/*
 *  code_queue
 */
static void alloc_code_queue(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_CODE,
			CodeQueue_Size,
			sizeoft(struct code_queue));
}

void code_queue_local(LocalRoot local, addr *ret)
{
	addr pos, stack;
	struct code_queue *ptr;

	Check(local == NULL, "local error");
	alloc_code_queue(local, &pos);
	/* array */
	code_stack_local(local, &stack);
	SetCodeQueue(pos, CodeQueue_Code, stack);
	/* body */
	ptr = StructCodeQueue(pos);
	clearpoint(ptr);
	ptr->mode = CodeQueue_ModeSet;
	/* result */
	*ret = pos;
}

enum CodeQueue_Mode code_queue_mode(CodeMake ptr)
{
	addr code;

	code = ptr->code;
	CheckTypeCodeQueue(code);
	return StructCodeQueue(code)->mode;
}

int code_queue_setp(CodeMake ptr)
{
	return code_queue_mode(ptr) == CodeQueue_ModeSet;
}

int code_queue_pushp(CodeMake ptr)
{
	return code_queue_mode(ptr) == CodeQueue_ModePush;
}

int code_queue_remp(CodeMake ptr)
{
	return code_queue_mode(ptr) == CodeQueue_ModeRemove;
}

static void code_queue_save(CodeMake ptr, modeswitch *mode)
{
	CheckTypeCodeQueue(ptr->code);
	mode->mode = code_queue_mode(ptr);
}

void code_queue_rollback(CodeMake ptr, modeswitch *mode)
{
	addr code;

	code = ptr->code;
	CheckTypeCodeQueue(code);
	StructCodeQueue(code)->mode = mode->mode;
}

static void code_queue_savevalue(CodeMake ptr,
		modeswitch *mode, enum CodeQueue_Mode value)
{
	addr code;

	code = ptr->code;
	code_queue_save(ptr, mode);
	StructCodeQueue(code)->mode = value;
}

void code_queue_setmode(CodeMake ptr, modeswitch *mode)
{
	code_queue_savevalue(ptr, mode, CodeQueue_ModeSet);
}
void code_queue_pushmode(CodeMake ptr, modeswitch *mode)
{
	code_queue_savevalue(ptr, mode, CodeQueue_ModePush);
}
void code_queue_remmode(CodeMake ptr, modeswitch *mode)
{
	code_queue_savevalue(ptr, mode, CodeQueue_ModeRemove);
}

static void code_queue_add(CodeMake ptr, addr value)
{
	addr code, stack;
	LocalRoot local;

	local = ptr->local;
	code = ptr->code;

	CheckTypeCodeQueue(code);
	Check(GetStatusDynamic(value), "dynamic error");
	GetCodeQueue(code, CodeQueue_Code, &stack);
	Check(stack == Nil, "stack error");
	push_code_stack(local, stack, value);
}

void code_queue_add2(CodeMake ptr, addr x, addr y)
{
	cons_heap(&y, x, y);
	code_queue_add(ptr, y);
}

void code_queue_push(CodeMake ptr, addr pos, ...)
{
	addr list;
	va_list args;

	va_start(args, pos);
	list_stdarg_alloc(NULL, &list, args);
	va_end(args);
	cons_heap(&list, pos, list);
	code_queue_add(ptr, list);
}

void code_queue_list(CodeMake ptr, constindex index, ...)
{
	addr pos, list;
	va_list args;

	GetConstant(index, &pos);
	va_start(args, index);
	list_stdarg_alloc(NULL, &list, args);
	va_end(args);
	cons_heap(&list, pos, list);
	code_queue_add(ptr, list);
}

void code_queue_single(CodeMake ptr, constindex index)
{
	addr pos;

	GetConstant(index, &pos);
	conscar_heap(&pos, pos);
	code_queue_add(ptr, pos);
}

void code_queue_cons(CodeMake ptr, constindex x, addr y)
{
	addr pos;
	GetConstant(x, &pos);
	code_queue_add2(ptr, pos, y);
}

void code_queue_double(CodeMake ptr, constindex x, addr y, addr z)
{
	addr pos;
	GetConstant(x, &pos);
	code_queue_push(ptr, pos, y, z, NULL);
}

void code_queue_index(CodeMake ptr, constindex x, size_t y)
{
	addr pos1, pos2;

	GetConstant(x, &pos1);
	index_heap(&pos2, y);
	code_queue_add2(ptr, pos1, pos2);
}

void code_queue_ifpush(CodeMake ptr)
{
	if (code_queue_pushp(ptr))
		CodeQueue_single(ptr, PUSH_RESULT);
}


/*
 *  stack
 */
static struct code_stack *code_queue_push_struct(CodeMake ptr)
{
	addr code, stack, one, pos;
	struct code_queue *queue;
	LocalRoot local;

	local = ptr->local;
	code = ptr->code;
	CheckTypeCodeQueue(ptr->code);
	/* new stack */
	code_stack_local(local, &one);
	/* push */
	GetCodeQueue(code, CodeQueue_Code, &pos);
	GetCodeQueue(code, CodeQueue_Stack, &stack);
	cons_local(local, &stack, pos, stack);
	SetCodeQueue(code, CodeQueue_Stack, stack);
	SetCodeQueue(code, CodeQueue_Code, one);
	queue = StructCodeQueue(code);
	queue->size++;
	/* result */
	return StructCodeStack(one);
}

void code_queue_push_code(CodeMake ptr)
{
	(void)code_queue_push_struct(ptr);
}

#define CodeQueueGoto(pos, x) { \
	addr __check; \
	GetConst(x, &__check); \
	if (pos == __check) { \
		return 1; \
	} \
}
static int code_queue_pop_goto_p(addr pos)
{
	if (GetType(pos) != LISPTYPE_CONS)
		return 0;
	GetCar(pos, &pos);
	CodeQueueGoto(pos, CODE_ESCAPE);
	CodeQueueGoto(pos, CODE_ESCAPE_NOT);
	CodeQueueGoto(pos, CODE_REVERT_GOTO);
	CodeQueueGoto(pos, CODE_GOTO);
	CodeQueueGoto(pos, CODE_IF_UNBOUND);
	CodeQueueGoto(pos, CODE_IF_NIL);
	CodeQueueGoto(pos, CODE_IF_T);

	return 0;
}

static int code_queue_pop_lambda_cache_p(addr pos)
{
	if (GetType(pos) != LISPTYPE_CONS)
		return 0;
	GetCar(pos, &pos);
	CodeQueueGoto(pos, CODE_LAMBDA_CACHE);

	return 0;
}

static int code_queue_pop_tag_p(addr pos, addr *ret)
{
	addr check, left;

	if (GetType(pos) != LISPTYPE_CONS)
		return 0;
	GetCar(pos, &left);
	GetConst(CODE_TAG, &check);
	if (left != check)
		return 0;
	GetCdr(pos, ret);

	return 1;
}

static int code_queue_pop_label_p(addr pos)
{
	return GetType(pos) == LISPTYPE_INDEX;
}

static void code_queue_pop_label(LocalRoot local,
		addr body, addr *rlabel, size_t *rsize)
{
	addr label, pos, index;
	size_t size;

	label = Nil;
	size = 0;
	while (body != Nil) {
		GetCons(body, &pos, &body);
		/* label */
		if (code_queue_pop_label_p(pos)) {
			index_local(local, &index, size);
			cons_local(local, &pos, pos, index);
			cons_local(local, &label, pos, label);
			continue;
		}
		/* tag */
		if (code_queue_pop_tag_p(pos, &pos)) {
			setjump_tabletagbody(pos, size);
			continue;
		}
		else {
			size++;
		}
	}
	*rlabel = label;
	*rsize = size;
}

static size_t code_queue_pop_find(addr label, addr right)
{
	addr left;
	size_t index;

	GetIndex(right, &index);
	while (label != Nil) {
		GetCons(label, &left, &label);
		GetCons(left, &left, &right);
		if (RefIndex(left) == index)
			return RefIndex(right);
	}
	Abort("code_queue_pop_find error");

	return 0;
}

static void code_queue_pop_replace(addr label, addr pos, addr *ret)
{
	addr cdr;
	size_t value;

	GetCons(pos, &pos, &cdr);
	value = code_queue_pop_find(label, cdr);
	index_heap(&cdr, value);
	cons_heap(ret, pos, cdr);
}

static void code_queue_pop_lambda_cache_replace(addr label, addr list, addr *ret)
{
	addr pos, cdr, tail;
	size_t value;

	Lista_bind(list, &pos, &cdr, &tail, NULL);
	value = code_queue_pop_find(label, cdr);
	index_heap(&cdr, value);
	lista_heap(ret, pos, cdr, tail, NULL);
}

static void code_queue_pop_goto(LocalRoot local,
		addr list, addr label, addr array, size_t size)
{
	addr pos;
	size_t i;

	/* code */
	for (i = 0; list != Nil; ) {
		GetCons(list, &pos, &list);
		if (code_queue_pop_label_p(pos))
			continue;
		if (code_queue_pop_tag_p(pos, &pos))
			continue;
		if (code_queue_pop_goto_p(pos))
			code_queue_pop_replace(label, pos, &pos);
		else if (code_queue_pop_lambda_cache_p(pos))
			code_queue_pop_lambda_cache_replace(label, pos, &pos);
		SetArrayA4(array, i++, pos);
	}
}

static void code_queue_pop_tag(addr list, addr *ret)
{
	addr root, pos, name, jump, lexical;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		/* (name jump lexical) */
		CheckTableTagBody(pos);
		getname_tabletagbody(pos, &name);
		index_heap(&jump, getjump_tabletagbody(pos));
		index_heap(&lexical, getlexical_tabletagbody(pos));
		list_heap(&pos, name, jump, lexical, NULL);
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);
}

static void code_queue_pop_block(addr pos, addr *ret)
{
	addr name, lexical;

	/* (name lexical) */
	CheckTableBlock(pos);
	getname_tableblock(pos, &name);
	index_heap(&lexical, getlexical_tableblock(pos));
	list_heap(ret, name, lexical, NULL);
}

static void code_queue_pop_info(addr array, size_t size)
{
	addr cons, car, cdr, key1, key2;
	size_t i;

	GetConst(CODE_TAGINFO, &key1);
	GetConst(CODE_BLOCKINFO, &key2);
	for (i = 0; i < size; i++) {
		GetArrayA4(array, i, &cons);
		GetCons(cons, &car, &cdr);
		if (car == key1) {
			code_queue_pop_tag(cdr, &cdr);
			SetCdr(cons, cdr);
			continue;
		}
		if (car == key2) {
			code_queue_pop_block(cdr, &cdr);
			SetCdr(cons, cdr);
			continue;
		}
	}
}

static int code_queue_pop_delete_p(addr list)
{
	addr x, y;
	size_t check1, check2;

	/* goto */
	if (! consp_getcons(list, &x, &list))
		return 0;
	if (! code_queue_pop_goto_p(x))
		return 0;
	GetCdr(x, &x);
	GetIndex(x, &check1);

	/* tag */
	if (! consp_getcar(list, &y))
		return 0;
	if (! indexp(y))
		return 0;
	GetIndex(y, &check2);

	return check1 == check2;
}

static void code_queue_pop_delete(addr list, addr *ret)
{
	addr list1, list2;

	*ret = list;
	list2 = Nil;
	while (list != Nil) {
		GetCdr(list, &list1);
		if (code_queue_pop_delete_p(list)) {
			if (list2 == Nil)
				*ret = list1;
			else
				SetCdr(list2, list1);
		}
		else {
			list2 = list;
		}
		list = list1;
	}
}

static void code_queue_pop_make(LocalRoot local, addr cons, addr *ret)
{
	addr label, array;
	size_t size;

	code_queue_pop_delete(cons, &cons);
	code_queue_pop_label(local, cons, &label, &size);
	vector4_heap(&array, size);
	code_queue_pop_goto(local, cons, label, array, size);
	code_queue_pop_info(array, size);
	*ret = array;
}

static void code_queue_pop_code(LocalRoot local, addr stack, addr *ret)
{
	addr pos;

	/* free stack */
	Check(StructCodeStack(stack)->finish == 0, "finish error");
	GetCodeStack(stack, CodeStack_Result, &pos);
	code_queue_pop_make(local, pos, &pos);
	free_code_stack(local, stack);

	/* make code */
	code_heap(&pos, pos);
	*ret = pos;
}

void code_queue_pop(CodeMake ptr, addr *ret)
{
	addr code, pos, left, right;
	struct code_queue *queue;
	LocalRoot local;

	local = ptr->local;
	code = ptr->code;

	CheckTypeCodeQueue(ptr->code);
	/* close stack */
	GetCodeQueue(code, CodeQueue_Code, &pos);
	finish_code_stack(local, pos);
	/* pop stack */
	GetCodeQueue(code, CodeQueue_Stack, &right);
	GetCons(right, &left, &right);
	SetCodeQueue(code, CodeQueue_Stack, right);
	SetCodeQueue(code, CodeQueue_Code, left);
	queue = StructCodeQueue(code);
	queue->size--;
	/* push operator */
	code_queue_pop_code(local, pos, ret);
}


/*
 *  code
 */
int code_make_execute_set_(CodeMake ptr, addr scope)
{
	modeswitch mode;

	code_queue_setmode(ptr, &mode);
	Return(code_make_execute_(ptr, scope));
	code_queue_rollback(ptr, &mode);

	return 0;
}

int code_make_execute_push_(CodeMake ptr, addr scope)
{
	modeswitch mode;

	code_queue_pushmode(ptr, &mode);
	Return(code_make_execute_(ptr, scope));
	code_queue_rollback(ptr, &mode);

	return 0;
}

int code_make_execute_rem_(CodeMake ptr, addr scope)
{
	modeswitch mode;

	code_queue_remmode(ptr, &mode);
	Return(code_make_execute_(ptr, scope));
	code_queue_rollback(ptr, &mode);

	return 0;
}

void code_make_single(CodeMake ptr, constindex set, constindex push)
{
	CheckTypeCodeQueue(ptr->code);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			code_queue_single(ptr, set);
			break;

		case CodeQueue_ModePush:
			code_queue_single(ptr, push);
			break;

		case CodeQueue_ModeRemove:
		default:
			break;
	}
}

void code_make_object(CodeMake ptr, addr value)
{
	CheckTypeCodeQueue(ptr->code);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(ptr, SET, value);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(ptr, PUSH, value);
			break;

		case CodeQueue_ModeRemove:
		default:
			break;
	}
}


/*
 *  label
 */
void code_queue_make_label(CodeMake ptr, addr *ret)
{
	struct code_queue *str = StructCodeQueue(ptr->code);
	index_heap(ret, str->label++);
}

void code_queue_push_label(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	code_queue_add(ptr, label);
}

void code_queue_if_unbound(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, IF_UNBOUND, label);
}

void code_queue_if_nil(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, IF_NIL, label);
}

void code_queue_if_t(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, IF_T, label);
}

void code_queue_goto(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, GOTO, label);
}


/*
 *  begin / end
 */
void code_escape_clear(CodeMake ptr)
{
	ptr->escape = 0;
}

void code_escape_wake(CodeMake ptr)
{
	ptr->escape = 1;
}

int code_escape_get(CodeMake ptr)
{
	return ptr->escape;
}

#ifdef LISP_DEBUG
static fixnum code_make_begin_index = 0;
#endif

void code_make_begin(CodeMake ptr, fixnum *ret)
{
#ifdef LISP_DEBUG
	addr pos;

	fixnum_heap(&pos, code_make_begin_index);
	CodeQueue_cons(ptr, BEGIN, pos);
	*ret = code_make_begin_index;
	code_make_begin_index++;
#else
	CodeQueue_single(ptr, BEGIN);
	*ret = 0;
#endif
}

void code_make_begin_call(CodeMake ptr, fixnum *ret)
{
#ifdef LISP_DEBUG
	addr pos;

	fixnum_heap(&pos, code_make_begin_index);
	CodeQueue_cons(ptr, BEGIN_CALL, pos);
	*ret = code_make_begin_index;
	code_make_begin_index++;
#else
	CodeQueue_single(ptr, BEGIN_CALL);
	*ret = 0;
#endif
}

void code_make_end(CodeMake ptr, fixnum value)
{
#ifdef LISP_DEBUG
	addr pos;
	fixnum_heap(&pos, value);
	CodeQueue_cons(ptr, END, pos);
#else
	CodeQueue_single(ptr, END);
#endif
}

void code_jump_escape(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, ESCAPE, label);
}

void code_jump_escape_not(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, ESCAPE_NOT, label);
}

void code_jump_escape_wake(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, ESCAPE, label);
	code_escape_wake(ptr);
}

void code_jump_escape_not_wake(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, ESCAPE_NOT, label);
	code_escape_wake(ptr);
}


/************************************************************
 *  make_value.c
 ************************************************************/

int code_make_debug_(CodeMake ptr, addr scope, int (*call)(CodeMake, addr))
{
	addr value;

	if (scope == NULL)
		goto normal;
	if (! scope_step_p(scope))
		goto normal;
	GetEvalScopeValue(scope, &value);
	if (value == Nil)
		goto normal;

	/* step */
	CodeQueue_cons(ptr, STEP, value);
	Return((*call)(ptr, scope));
	CodeQueue_single(ptr, STEP_OFF);
	return 0;

	/* normal */
normal:
	return (*call)(ptr, scope);
}


/* nil */
int code_make_nil_(CodeMake ptr, addr scope)
{
	if (scope == NULL)
		goto normal;
	if (! scope_step_p(scope))
		goto normal;
	CodeQueue_cons(ptr, STEP, Nil);
	code_make_single(ptr, CONSTANT_CODE_NIL_SET, CONSTANT_CODE_NIL_PUSH);
	CodeQueue_single(ptr, STEP_OFF);
	return 0;

normal:
	code_make_single(ptr, CONSTANT_CODE_NIL_SET, CONSTANT_CODE_NIL_PUSH);
	return 0;
}


/* t */
static int code_make_t_call_(CodeMake ptr, addr scope)
{
	code_make_single(ptr, CONSTANT_CODE_T_SET, CONSTANT_CODE_T_PUSH);
	return 0;
}

int code_make_t_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_t_call_);
}


/* value */
static int code_make_value_call_(CodeMake ptr, addr scope)
{
	CheckTypeCodeQueue(ptr->code);
	GetEvalScopeValue(scope, &scope);
	code_make_object(ptr, scope);

	return 0;
}
int code_make_value_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_value_call_);
}

static int code_make_value2_call_(CodeMake ptr, addr scope)
{
	CheckTypeCodeQueue(ptr->code);
	GetEvalScopeIndex(scope, 0, &scope);
	code_make_object(ptr, scope);

	return 0;
}
int code_make_value2_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_value2_call_);
}


/* symbol */
static int code_symbol_special_p(addr pos)
{
	return getspecialp_tablevalue(pos) || getglobalp_tablevalue(pos);
}

static void code_symbol_set(CodeMake ptr, addr pos, addr table)
{
	if (code_symbol_special_p(table)) {
		CodeQueue_cons(ptr, SPECIAL_SET, pos);
		code_escape_wake(ptr);
	}
	else {
		index_heap(&pos, getlexical_tablevalue(table));
		CodeQueue_cons(ptr, LEXICAL_SET, pos);
	}
}

static void code_symbol_push(CodeMake ptr, addr pos, addr table)
{
	if (code_symbol_special_p(table)) {
		CodeQueue_cons(ptr, SPECIAL_PUSH, pos);
		code_escape_wake(ptr);
	}
	else {
		index_heap(&pos, getlexical_tablevalue(table));
		CodeQueue_cons(ptr, LEXICAL_PUSH, pos);
	}
}

static void code_symbol_remove(CodeMake ptr, addr pos, addr table)
{
	if (code_symbol_special_p(table)) {
		CodeQueue_cons(ptr, SPECIAL_REM, pos);
		code_escape_wake(ptr);
	}
	else {
		index_heap(&pos, getlexical_tablevalue(table));
		CodeQueue_cons(ptr, LEXICAL_REM, pos);
	}
}

static int code_make_symbol_call_(CodeMake ptr, addr scope)
{
	addr symbol, table;

	GetEvalScopeValue(scope, &symbol);
	GetEvalScopeIndex(scope, 0, &table);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			code_symbol_set(ptr, symbol, table);
			break;

		case CodeQueue_ModePush:
			code_symbol_push(ptr, symbol, table);
			break;

		case CodeQueue_ModeRemove:
		default:
			code_symbol_remove(ptr, symbol, table);
			break;
	}

	return 0;
}

int code_make_symbol_(CodeMake ptr, addr scope)
{
	addr symbol;

	/* keyword */
	GetEvalScopeValue(scope, &symbol);
	if (keywordp(symbol))
		return code_make_value_(ptr, scope);

	/* symbol */
	return code_make_debug_(ptr, scope, code_make_symbol_call_);
}


/* declaim */
static void code_declaim_special(CodeMake ptr, addr cons)
{
	addr pos;

	getall_special_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		CodeQueue_cons(ptr, DECLAIM_SPECIAL, pos);
		code_escape_wake(ptr);
	}
}

static void code_declaim_type_value(CodeMake ptr, addr cons)
{
	addr key, value;

	getall_type_value_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		CodeQueue_double(ptr, DECLAIM_TYPE_VALUE, key, value);
		code_escape_wake(ptr);
	}
}

static void code_declaim_type_function(CodeMake ptr, addr cons)
{
	addr key, value;

	getall_type_function_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		CodeQueue_double(ptr, DECLAIM_TYPE_FUNCTION, key, value);
		code_escape_wake(ptr);
	}
}

static void code_declaim_inline(CodeMake ptr, addr cons)
{
	addr key, value, check1, check2;

	getall_inline_declare(cons, &cons);
	GetConst(COMMON_INLINE, &check1);
	GetConst(COMMON_NOTINLINE, &check2);
	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		if (value == check1)
			CodeQueue_cons(ptr, DECLAIM_INLINE, key);
		if (value == check2)
			CodeQueue_cons(ptr, DECLAIM_NOTINLINE, key);
		code_escape_wake(ptr);
	}
}

static void code_declaim_optimize(CodeMake ptr, addr declare)
{
	addr pos;
	OptimizeType optimize;

	/* compilation-speed */
	optimize = get_optimize_compilation_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		CodeQueue_cons(ptr, DECLAIM_COMPILATION, pos);
	}
	/* debug */
	optimize = get_optimize_debug_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		CodeQueue_cons(ptr, DECLAIM_DEBUG, pos);
	}
	/* safety */
	optimize = get_optimize_safety_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		CodeQueue_cons(ptr, DECLAIM_SAFETY, pos);
	}
	/* space */
	optimize = get_optimize_space_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		CodeQueue_cons(ptr, DECLAIM_SPACE, pos);
	}
	/* speed */
	optimize = get_optimize_speed_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		CodeQueue_cons(ptr, DECLAIM_SPEED, pos);
	}
}

static void code_declaim_declaration(CodeMake ptr, addr cons)
{
	addr pos;

	getall_declaration_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		CodeQueue_cons(ptr, DECLAIM_DECLARATION, pos);
	}
}

int code_make_declaim_(CodeMake ptr, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	code_declaim_special(ptr, scope);
	code_declaim_type_value(ptr, scope);
	code_declaim_type_function(ptr, scope);
	code_declaim_inline(ptr, scope);
	code_declaim_optimize(ptr, scope);
	code_declaim_declaration(ptr, scope);
	return code_make_nil_(ptr, NULL);
}


/*
 *  lexical
 */
int code_make_lexical_(CodeMake ptr, addr scope)
{
	addr list, pos;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &pos);
	GetEvalScopeIndex(scope, 1, &list);
	if (list == Nil)
		return code_make_execute_(ptr, pos);

	/* lexical */
	code_make_begin(ptr, &id);
	CodeQueue_cons(ptr, LEXICAL, list);
	Return(code_make_execute_(ptr, pos));
	code_make_end(ptr, id);

	return 0;
}


/*
 *  progn
 */
int code_make_progn_(CodeMake ptr, addr scope)
{
	addr form, list;

	GetEvalScopeValue(scope, &form);
	GetEvalScopeIndex(scope, 0, &list);
	if (form != Nil && scope_step_p(scope)) {
		CodeQueue_cons(ptr, STEP, form);
		Return(code_allcons_(ptr, list, NULL));
		CodeQueue_single(ptr, STEP_OFF);
	}
	else {
		Return(code_allcons_(ptr, list, NULL));
	}

	return 0;
}


/* let */
static void code_make_free_value(CodeMake ptr, addr pos, addr type, addr escape)
{
	if (type_astert_p(type))
		return;
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &pos);
		CodeQueue_double(ptr, TYPE_SPECIAL, pos, type);
		code_jump_escape_wake(ptr, escape);
	}
	else if (getglobalp_tablevalue(pos)) {
		getname_tablevalue(pos, &pos);
		CodeQueue_double(ptr, TYPE_GLOBAL, pos, type);
		code_jump_escape_wake(ptr, escape);
	}
	else {
		/* lexical */
		index_heap(&pos, getlexical_tablevalue(pos));
		/* CodeQueue_double(ptr, TYPE_LEXICAL, pos, type); */
		/* code_jump_escape_wake(ptr, escape); */
	}
}

static void code_make_free_function(CodeMake ptr, addr pos, addr type, addr escape)
{
	addr symbol;

	if (type_astert_p(type) || type_function_aster_p(type))
		return;
	if (! getglobalp_tablefunction(pos)) {
		/* lexical */
		index_heap(&pos, getlexical_tablefunction(pos));
		/* CodeQueue_double(ptr, TYPE_LEXICAL, pos, type); */
		/* code_jump_escape_wake(ptr, escape); */
		return;
	}

	getname_tablefunction(pos, &pos);
	GetCallName(pos, &symbol);
	if (symbolp_callname(pos)) {
		CodeQueue_double(ptr, TYPE_FUNCTION, symbol, type);
		code_jump_escape_wake(ptr, escape);
	}
	else {
		CodeQueue_double(ptr, TYPE_SETF, symbol, type);
		code_jump_escape_wake(ptr, escape);
	}
}

int code_make_free_(CodeMake ptr, addr list, addr escape)
{
	addr pos, type;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &type);
		if (eval_tablevalue_p(pos)) {
			code_make_free_value(ptr, pos, type, escape);
			continue;
		}
		if (eval_tablefunction_p(pos)) {
			code_make_free_function(ptr, pos, type, escape);
			continue;
		}

		return fmte_("Invalid type object in code-make.", NULL);
	}

	return 0;
}

void code_make_type_value(CodeMake ptr, addr pos, addr escape)
{
	addr type;

	if (getcheck_tablevalue(pos)) {
		gettype_tablevalue(pos, &type);
		if (! type_astert_p(type)) {
			CodeQueue_cons(ptr, TYPE_RESULT, type);
			code_jump_escape_wake(ptr, escape);
		}
	}
}

static int code_make_let_args_(CodeMake ptr, addr args, addr escape)
{
	addr list, pos, value, index;

	/* value */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &value);

		Return(code_make_execute_set_(ptr, value));
		code_jump_escape_wake(ptr, escape);
		code_make_type_value(ptr, pos, escape);

		index_heap(&value, getlet_tablevalue(pos));
		CodeQueue_cons(ptr, SETQ_LEXICAL, value);
	}

	/* bind */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCar(pos, &pos);

		index_heap(&index, getlet_tablevalue(pos));
		if (getspecialp_tablevalue(pos)) {
			getname_tablevalue(pos, &value);
			CodeQueue_double(ptr, LET_SPECIAL, index, value);
		}
		else {
			index_heap(&value, getlexical_tablevalue(pos));
			CodeQueue_double(ptr, LET_LEXICAL, index, value);
		}
	}

	return 0;
}

static int code_make_let_call_(CodeMake ptr, addr scope)
{
	addr args, body, free, alloc, escape, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &body);
	GetEvalScopeIndex(scope, 3, &free);
	GetEvalScopeIndex(scope, 4, &alloc);

	if (alloc == Nil) {
		code_queue_make_label(ptr, &escape);
		Return(code_make_free_(ptr, free, escape));
		Return(code_make_let_args_(ptr, args, escape));
		Return(code_allcons_(ptr, body, escape));
		code_queue_push_label(ptr, escape);
	}
	else {
		/* begin */
		code_queue_make_label(ptr, &escape);
		code_queue_make_label(ptr, &finish);
		code_make_begin(ptr, &id);

		/* body */
		Return(code_make_free_(ptr, free, escape));
		Return(code_make_let_args_(ptr, args, escape));
		Return(code_allcons_set_(ptr, body, escape));
		code_make_end(ptr, id);
		code_queue_ifpush(ptr);
		code_queue_goto(ptr, finish);

		/* escape */
		code_queue_push_label(ptr, escape);
		code_make_end(ptr, id);
		code_queue_push_label(ptr, finish);
	}

	return 0;
}

int code_make_let_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_let_call_);
}


/* let* */
static int code_make_leta_args_(CodeMake ptr, addr list, addr escape)
{
	addr pos, value;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &value);

		Return(code_make_execute_set_(ptr, value));
		code_jump_escape_wake(ptr, escape);
		code_make_type_value(ptr, pos, escape);

		if (getspecialp_tablevalue(pos)) {
			getname_tablevalue(pos, &value);
			CodeQueue_cons(ptr, LETA_SPECIAL, value);
		}
		else {
			index_heap(&value, getlexical_tablevalue(pos));
			CodeQueue_cons(ptr, SETQ_LEXICAL, value);
		}
	}

	return 0;
}

static int code_make_leta_call_(CodeMake ptr, addr scope)
{
	addr args, body, free, alloc, escape, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &body);
	GetEvalScopeIndex(scope, 3, &free);
	GetEvalScopeIndex(scope, 4, &alloc);
	if (alloc == Nil) {
		code_queue_make_label(ptr, &escape);
		Return(code_make_free_(ptr, free, escape));
		Return(code_make_leta_args_(ptr, args, escape));
		Return(code_allcons_(ptr, body, escape));
		code_queue_push_label(ptr, escape);
	}
	else {
		/* begin */
		code_queue_make_label(ptr, &escape);
		code_queue_make_label(ptr, &finish);
		code_make_begin(ptr, &id);

		/* body */
		Return(code_make_free_(ptr, free, escape));
		Return(code_make_leta_args_(ptr, args, escape));
		Return(code_allcons_set_(ptr, body, escape));
		code_make_end(ptr, id);
		code_queue_ifpush(ptr);
		code_queue_goto(ptr, finish);

		/* escape */
		code_queue_push_label(ptr, escape);
		code_make_end(ptr, id);
		code_queue_push_label(ptr, finish);
	}

	return 0;
}

int code_make_leta_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_leta_call_);
}

/* setq */
static int code_setq_loop_(CodeMake ptr, addr list, addr escape)
{
	addr pos, value, symbol;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &value);

		Return(code_make_execute_set_(ptr, value));
		code_jump_escape_wake(ptr, escape);
		code_make_type_value(ptr, pos, escape);

		getname_tablevalue(pos, &symbol);
		if (getspecialp_tablevalue(pos)) {
			CodeQueue_cons(ptr, SETQ_SPECIAL, symbol);
			code_jump_escape_wake(ptr, escape);
		}
		else if (getglobalp_tablevalue(pos)) {
			CodeQueue_cons(ptr, SETQ_GLOBAL, symbol);
			code_jump_escape_wake(ptr, escape);
		}
		else {
			index_heap(&value, getlexical_tablevalue(pos));
			CodeQueue_cons(ptr, SETQ_LEXICAL, value);
		}
	}

	return 0;
}

static int code_make_setq_call_(CodeMake ptr, addr scope)
{
	addr list, escape;

	/* nil */
	GetEvalScopeIndex(scope, 0, &list);
	if (list == Nil)
		return code_make_nil_(ptr, NULL);

	/* setq */
	code_queue_make_label(ptr, &escape);
	Return(code_setq_loop_(ptr, list, escape));
	code_queue_ifpush(ptr);
	code_queue_push_label(ptr, escape);

	return 0;
}

int code_make_setq_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_setq_call_);
}


/* values */
static int code_values_set_(CodeMake ptr, addr cons)
{
	addr pos, escape;
	fixnum id;

	/* nil */
	if (cons == Nil) {
		CodeQueue_single(ptr, VALUES_NIL);
		return 0;
	}

	/* list */
	code_queue_make_label(ptr, &escape);
	code_make_begin(ptr, &id);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		Return(code_make_execute_push_(ptr, pos));
		code_jump_escape_wake(ptr, escape);
	}
	CodeQueue_single(ptr, VALUES_SET);
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);

	return 0;
}

static int code_values_push_(CodeMake ptr, addr cons)
{
	addr pos, escape;

	/* nil */
	if (cons == Nil)
		return code_make_nil_(ptr, NULL);

	/* list */
	code_queue_make_label(ptr, &escape);
	GetCons(cons, &pos, &cons);
	Return(code_make_execute_push_(ptr, pos));
	code_jump_escape_wake(ptr, escape);
	Return(code_allcons_rem_(ptr, cons, escape));
	code_queue_push_label(ptr, escape);

	return 0;
}

static int code_make_values_call_(CodeMake ptr, addr scope)
{
	GetEvalScopeIndex(scope, 0, &scope);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			return code_values_set_(ptr, scope);

		case CodeQueue_ModePush:
			return code_values_push_(ptr, scope);

		case CodeQueue_ModeRemove:
		default:
			return code_allcons_(ptr, scope, NULL);
	}
}

int code_make_values_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_values_call_);
}


/* the */
static int code_make_the_call_(CodeMake ptr, addr scope)
{
	addr expr, type, check, escape;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &check);
	if (check == Nil)
		return code_make_execute_(ptr, expr);

	GetEvalScopeThe(scope, &type);
	get_type_optimized(&type, type);
	get_type_subtypep(&type, type);

	code_queue_make_label(ptr, &escape);
	if (code_queue_pushp(ptr)) {
		Return(code_make_execute_set_(ptr, expr));
		code_jump_escape_wake(ptr, escape);
		CodeQueue_cons(ptr, THE_PUSH, type);
	}
	else {
		Return(code_make_execute_set_(ptr, expr));
		code_jump_escape_wake(ptr, escape);
		CodeQueue_cons(ptr, THE_SET, type);
	}
	code_queue_push_label(ptr, escape);

	return 0;
}

int code_make_the_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_the_call_);
}


/* eval-when */
static int code_make_eval_when_call_(CodeMake ptr, addr scope)
{
	addr cons, compile, load, exec, toplevel, mode;

	GetEvalScopeIndex(scope, 0, &cons);
	GetEvalScopeIndex(scope, 1, &compile);
	GetEvalScopeIndex(scope, 2, &load);
	GetEvalScopeIndex(scope, 3, &exec);
	GetEvalScopeIndex(scope, 4, &toplevel);
	GetEvalScopeIndex(scope, 5, &mode);

	return code_allcons_(ptr, cons, NULL);
}

int code_make_eval_when_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_eval_when_call_);
}


/* locally */
static int code_make_locally_call_(CodeMake ptr, addr scope)
{
	addr cons, free, escape;

	GetEvalScopeIndex(scope, 1, &cons);
	GetEvalScopeIndex(scope, 2, &free);

	code_queue_make_label(ptr, &escape);
	Return(code_make_free_(ptr, free, escape));
	Return(code_allcons_(ptr, cons, escape));
	code_queue_push_label(ptr, escape);

	return 0;
}

int code_make_locally_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_locally_call_);
}


/* if */
static int code_if_not_p(addr scope)
{
	addr call, check1, check2;

	if (RefEvalScopeType(scope) != EVAL_PARSE_CALL)
		return 0;
	/* args */
	GetEvalScopeIndex(scope, 1, &call);
	if (! singlep(call))
		return 0;
	/* (not x) or (null x) */
	GetEvalScopeIndex(scope, 0, &call);
	if (RefEvalScopeType(scope) != EVAL_PARSE_FUNCTION)
		return 0;
	GetEvalScopeIndex(call, 0, &call);
	getname_tablefunction(call, &call);
	if (! symbolp_callname(call))
		return 0;
	GetCallName(call, &call);
	GetConst(COMMON_NOT, &check1);
	GetConst(COMMON_NULL, &check2);

	return call == check1 || call == check2;
}

static int code_if_true_(CodeMake ptr, addr then, addr last, addr escape)
{
	int check;
	addr label;

	check = (RefEvalScopeType(last) == EVAL_PARSE_NIL);
	/* (if expr then else) */
	code_queue_make_label(ptr, &label);
	code_queue_if_nil(ptr, label);
	Return(code_make_execute_(ptr, then));
	code_jump_escape_wake(ptr, escape);
	code_queue_goto(ptr, escape);
	code_queue_push_label(ptr, label);
	if (check)
		return code_make_nil_(ptr, NULL);
	else
		return code_make_execute_(ptr, last);
}

static int code_if_false_(CodeMake ptr, addr then, addr last, addr escape)
{
	int check;
	addr label;

	check = (RefEvalScopeType(last) == EVAL_PARSE_NIL);
	/* (if (not expr) then else) */
	code_queue_make_label(ptr, &label);
	code_queue_if_t(ptr, label);
	Return(code_make_execute_(ptr, then));
	code_jump_escape_wake(ptr, escape);
	code_queue_goto(ptr, escape);
	code_queue_push_label(ptr, label);
	if (check)
		return code_make_nil_(ptr, NULL);
	else
		return code_make_execute_(ptr, last);
}

static int code_make_if_call_(CodeMake ptr, addr scope)
{
	addr expr, then, last, escape;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &then);
	GetEvalScopeIndex(scope, 2, &last);

	/* expr */
	code_queue_make_label(ptr, &escape);
	Return(code_make_execute_set_(ptr, expr));
	code_jump_escape_wake(ptr, escape);

	/* then, else */
	if (code_if_not_p(expr)) {
		Return(code_if_false_(ptr, then, last, escape));
	}
	else {
		Return(code_if_true_(ptr, then, last, escape));
	}
	code_queue_push_label(ptr, escape);

	return 0;
}

int code_make_if_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_if_call_);
}


/* unwind-protect */
static int code_make_unwind_protect_call_(CodeMake ptr, addr scope)
{
	addr form, cons, escape;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &form);
	GetEvalScopeIndex(scope, 1, &cons);

	code_queue_make_label(ptr, &escape);
	/* form */
	Return(code_make_execute_(ptr, form));
	/* protect */
	code_make_begin(ptr, &id);
	CodeQueue_single(ptr, SAVE);
	CodeQueue_single(ptr, NORMAL);
	Return(code_allcons_rem_(ptr, cons, escape));
	CodeQueue_single(ptr, RESTORE);
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);

	return 0;
}

int code_make_unwind_protect_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_unwind_protect_call_);
}


/* tagbody */
static int code_tagbody_rem_(CodeMake ptr, addr list)
{
	addr pos, escape;

	code_queue_make_label(ptr, &escape);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (RefEvalScopeType(pos) != EVAL_PARSE_TAG) {
			Return(code_make_execute_rem_(ptr, pos));
			code_jump_escape_wake(ptr, escape);
		}
	}
	code_make_nil_(ptr, NULL);
	code_queue_push_label(ptr, escape);

	return 0;
}

static int code_tagbody_cons_(CodeMake ptr, addr cons, addr escape)
{
	addr pos;

	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		if (RefEvalScopeType(pos) == EVAL_PARSE_TAG) {
			GetEvalScopeValue(pos, &pos);
			CodeQueue_cons(ptr, TAG, pos);
		}
		else {
			Return(code_make_execute_rem_(ptr, pos));
			code_jump_escape_wake(ptr, escape);
		}
	}

	return 0;
}

static int code_tagbody_body_(CodeMake ptr, addr tag, addr cons)
{
	addr escape, finish;
	fixnum id;

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* body */
	CodeQueue_cons(ptr, TAGINFO, tag);
	Return(code_tagbody_cons_(ptr, cons, escape));
	code_make_end(ptr, id);
	Return(code_make_nil_(ptr, NULL));
	code_queue_goto(ptr, finish);

	/* escape */
	code_queue_push_label(ptr, escape);
	CodeQueue_single(ptr, REVERT);
	code_make_end(ptr, id);
	code_queue_push_label(ptr, finish);

	return 0;
}

static int code_make_tagbody_call_(CodeMake ptr, addr scope)
{
	addr tag, cons;

	/*  code: tagbody
	 *    (code::taginfo tag1 tag2 ...)
	 *    ,@progn...
	 *    nil
	 */
	GetEvalScopeIndex(scope, 0, &tag);
	GetEvalScopeIndex(scope, 1, &cons);
	if (tag == Nil)
		return code_tagbody_rem_(ptr, cons);
	else
		return code_tagbody_body_(ptr, tag, cons);
}

int code_make_tagbody_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_tagbody_call_);
}


/* go */
int code_make_go_(CodeMake ptr, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	index_heap(&scope, getlexical_tabletagbody(scope));
	CodeQueue_cons(ptr, GO, scope);
	code_escape_wake(ptr);

	return 0;
}


/* block */
static int code_make_block_call_(CodeMake ptr, addr scope)
{
	addr name, cons, escape, normal, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &cons);
	if (name == Nil)
		return code_allcons_(ptr, cons, NULL);

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &normal);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* body */
	CodeQueue_cons(ptr, BLOCKINFO, name);
	Return(code_allcons_(ptr, cons, escape));
	code_queue_goto(ptr, normal);

	/* escape */
	code_queue_push_label(ptr, escape);
	CodeQueue_cons(ptr, REVERT_GOTO, normal);
	code_make_end(ptr, id);
	code_queue_goto(ptr, finish);

	/* normal */
	code_queue_push_label(ptr, normal);
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);

	/* finish */
	code_queue_push_label(ptr, finish);

	return 0;
}

int code_make_block_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_block_call_);
}


/* return-from */
static int code_make_return_from_call_(CodeMake ptr, addr scope)
{
	addr pos, form, escape;

	GetEvalScopeIndex(scope, 0, &pos);
	GetEvalScopeIndex(scope, 1, &form);

	/* form */
	code_queue_make_label(ptr, &escape);
	Return(code_make_execute_set_(ptr, form));
	code_jump_escape_wake(ptr, escape);

	/* name */
	index_heap(&pos, getlexical_tableblock(pos));
	CodeQueue_cons(ptr, RETURN_FROM, pos);
	code_escape_wake(ptr);
	code_queue_push_label(ptr, escape);

	return 0;
}

int code_make_return_from_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_return_from_call_);
}


/* catch */
static int code_make_catch_call_(CodeMake ptr, addr scope)
{
	addr name, cons, escape, normal, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &cons);

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &normal);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* name */
	Return(code_make_execute_set_(ptr, name));
	code_jump_escape_wake(ptr, escape);
	CodeQueue_single(ptr, CATCH);
	code_escape_wake(ptr);

	/* body */
	Return(code_allcons_set_(ptr, cons, escape));
	code_queue_goto(ptr, normal);

	/* escape */
	code_queue_push_label(ptr, escape);
	CodeQueue_cons(ptr, REVERT_GOTO, normal);
	code_make_end(ptr, id);
	code_queue_goto(ptr, finish);

	/* normal */
	code_queue_push_label(ptr, normal);
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);

	/* finish */
	code_queue_push_label(ptr, finish);

	return 0;
}

int code_make_catch_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_catch_call_);
}


/* throw */
static int code_make_throw_call_(CodeMake ptr, addr scope)
{
	addr name, form, escape;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &form);

	code_queue_make_label(ptr, &escape);
	code_make_begin(ptr, &id);
	/* name */
	Return(code_make_execute_push_(ptr, name));
	code_jump_escape_wake(ptr, escape);
	/* value */
	Return(code_make_execute_set_(ptr, form));
	code_jump_escape_wake(ptr, escape);
	/* throw */
	CodeQueue_single(ptr, THROW);
	code_escape_wake(ptr);
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);

	return 0;
}

int code_make_throw_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_throw_call_);
}


/* multiple-value-bind */
static void code_make_multiple_value_bind_index(CodeMake ptr,
		size_t i, addr pos, addr escape)
{
	addr index, value;

	/* type */
	index_heap(&index, i);
	if (getcheck_tablevalue(pos)) {
		gettype_tablevalue(pos, &value);
		if (! type_astert_p(value)) {
			CodeQueue_double(ptr, BIND1_TYPE, index, value);
			code_jump_escape_wake(ptr, escape);
		}
	}

	/* bind */
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &value);
		CodeQueue_double(ptr, BIND1_SPECIAL, index, value);
	}
	else {
		index_heap(&value, getlexical_tablevalue(pos));
		CodeQueue_double(ptr, BIND1_LEXICAL, index, value);
	}
}

static void code_make_multiple_value_bind_list(CodeMake ptr, addr pos, addr escape)
{
	addr value;

	/* type */
	if (getcheck_tablevalue(pos)) {
		gettype_tablevalue(pos, &value);
		if (! type_astert_p(value)) {
			CodeQueue_cons(ptr, BIND2_TYPE, value);
			code_jump_escape_wake(ptr, escape);
		}
	}

	/* bind */
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &value);
		CodeQueue_cons(ptr, BIND2_SPECIAL, value);
	}
	else {
		index_heap(&value, getlexical_tablevalue(pos));
		CodeQueue_cons(ptr, BIND2_LEXICAL, value);
	}
}

static void code_make_multiple_value_bind_args(CodeMake ptr, addr list, addr escape)
{
	addr pos;
	size_t i;

	for (i = 0; list != Nil; i++) {
		GetCons(list, &pos, &list);
		if (i < EXECUTE_VALUES)
			code_make_multiple_value_bind_index(ptr, i, pos, escape);
		else
			code_make_multiple_value_bind_list(ptr, pos, escape);
	}
}

static int code_make_multiple_value_bind_call_(CodeMake ptr, addr scope)
{
	fixnum id;
	addr args, expr, cons, free, alloc, escape, finish;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 1, &expr);
	GetEvalScopeIndex(scope, 4, &cons);
	GetEvalScopeIndex(scope, 5, &free);
	GetEvalScopeIndex(scope, 6, &alloc); /* allocate */

	if (alloc == Nil) {
		code_queue_make_label(ptr, &escape);
		/* expr */
		Return(code_make_execute_set_(ptr, expr));
		code_jump_escape_wake(ptr, escape);
		/* bind */
		Return(code_make_free_(ptr, free, escape));
		code_make_multiple_value_bind_args(ptr, args, escape);
		Return(code_allcons_(ptr, cons, escape));
		/* result */
		code_queue_push_label(ptr, escape);
	}
	else {
		/* begin */
		code_queue_make_label(ptr, &escape);
		code_queue_make_label(ptr, &finish);
		code_make_begin(ptr, &id);

		/* expr */
		Return(code_make_execute_set_(ptr, expr));
		code_jump_escape_wake(ptr, escape);

		/* bind */
		Return(code_make_free_(ptr, free, escape));
		code_make_multiple_value_bind_args(ptr, args, escape);
		Return(code_allcons_set_(ptr, cons, escape));
		code_make_end(ptr, id);
		code_queue_ifpush(ptr);
		code_queue_goto(ptr, finish);

		/* escape */
		code_queue_push_label(ptr, escape);
		code_make_end(ptr, id);
		code_queue_push_label(ptr, finish);
	}

	return 0;
}

int code_make_multiple_value_bind_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_multiple_value_bind_call_);
}


/* multiple-value-call */
static int code_make_multiple_value_call_body_(CodeMake ptr,
		addr call, addr args, addr escape)
{
	addr pos;

	/* call */
	Return(code_make_execute_push_(ptr, call));
	code_jump_escape_wake(ptr, escape);

	/* args */
	while (args != Nil) {
		GetCons(args, &pos, &args);
		Return(code_make_execute_set_(ptr, pos));
		code_jump_escape_wake(ptr, escape);
		CodeQueue_single(ptr, PUSH_VALUES);
	}

	/* call */
	CodeQueue_single(ptr, FUNCALL);
	code_jump_escape_wake(ptr, escape);

	return 0;
}

static int code_make_multiple_value_call_call_(CodeMake ptr, addr scope)
{
	addr call, args, escape, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &call);
	GetEvalScopeIndex(scope, 1, &args);

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* body */
	Return(code_make_multiple_value_call_body_(ptr, call, args, escape));
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);
	code_queue_goto(ptr, finish);

	/* escape */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_push_label(ptr, finish);

	return 0;
}

int code_make_multiple_value_call_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_multiple_value_call_call_);
}


/* multiple-value-prog1 */
static int code_make_multiple_value_prog1_call_(CodeMake ptr, addr scope)
{
	addr expr, cons, escape;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &cons);

	/* expr */
	Return(code_make_execute_(ptr, expr));
	/* cons */
	code_queue_make_label(ptr, &escape);
	code_make_begin(ptr, &id);
	CodeQueue_single(ptr, SAVE);
	Return(code_allcons_rem_(ptr, cons, escape));
	CodeQueue_single(ptr, RESTORE);
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);

	return 0;
}

int code_make_multiple_value_prog1_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_multiple_value_prog1_call_);
}


/* nth-value */
static int code_make_nth_value_call_(CodeMake ptr, addr scope)
{
	addr nth, expr, escape, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &nth);
	GetEvalScopeIndex(scope, 1, &expr);

	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* nth */
	Return(code_make_execute_push_(ptr, nth));
	code_jump_escape_wake(ptr, escape);

	/* expr */
	Return(code_make_execute_set_(ptr, expr));
	code_jump_escape_wake(ptr, escape);

	/* result */
	CodeQueue_single(ptr, NTH_VALUE);
	code_escape_wake(ptr);
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);
	code_queue_goto(ptr, finish);

	/* escape */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_push_label(ptr, finish);

	return 0;
}

int code_make_nth_value_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_nth_value_call_);
}


/* progv */
static int code_make_progv_call_(CodeMake ptr, addr scope)
{
	addr symbols, values, body, escape, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &symbols);
	GetEvalScopeIndex(scope, 1, &values);
	GetEvalScopeIndex(scope, 2, &body);

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* symbols */
	Return(code_make_execute_push_(ptr, symbols));
	code_jump_escape_wake(ptr, escape);

	/* values */
	Return(code_make_execute_push_(ptr, values));
	code_jump_escape_wake(ptr, escape);

	/* progv */
	CodeQueue_single(ptr, PROGV);
	code_jump_escape_wake(ptr, escape);

	/* body */
	Return(code_allcons_set_(ptr, body, escape));
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);
	code_queue_goto(ptr, finish);

	/* escape */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_push_label(ptr, finish);

	return 0;
}

int code_make_progv_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_progv_call_);
}


/* load-time-value */
static void code_make_reference(CodeMake ptr, addr value)
{
	CheckTypeCodeQueue(ptr->code);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(ptr, REFERENCE_SET, value);
			code_escape_wake(ptr);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(ptr, REFERENCE_PUSH, value);
			code_escape_wake(ptr);
			break;

		case CodeQueue_ModeRemove:
		default:
			break;
	}
}

int code_make_load_time_value_(CodeMake ptr, addr scope)
{
	addr pos, value, index;

	GetEvalScopeIndex(scope, 0, &value);
	GetEvalScopeIndex(scope, 1, &index);
	load_time_value_heap(&pos, value, index);
	code_make_reference(ptr, pos);

	return 0;
}


/*
 *  step
 */
int code_make_step_(CodeMake ptr, addr scope)
{
	addr expr;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &expr);
	code_make_begin(ptr, &id);
	CodeQueue_single(ptr, STEP_BEGIN);
	Return(code_make_execute_set_(ptr, expr));
	CodeQueue_single(ptr, STEP_END);
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);

	return 0;
}


/************************************************************
 *  math_exp.c
 ************************************************************/

/*
 *  exp
 */
struct mathcall_struct {
	void (*complex_f)(single_float, single_float, single_float *, single_float *);
	void (*complex_d)(double_float, double_float, double_float *, double_float *);
	void (*complex_l)(long_float, long_float, long_float *, long_float *);
	single_float (*call_s)(single_float);
	double_float (*call_d)(double_float);
	long_float (*call_l)(long_float);
	int (*range_f)(single_float);
	int (*range_d)(double_float);
	int (*range_l)(long_float);
};

static int call_complex_common_(struct mathcall_struct *ptr, addr pos, addr *ret)
{
	single_float af, bf;
	double_float ad, bd;
	long_float al, bl;
	addr real, imag;

	CheckType(pos, LISPTYPE_COMPLEX);
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	switch (GetTypeComplex(pos)) {
		case ComplexType_single:
			af = RefSingleFloat(real);
			bf = RefSingleFloat(imag);
			(ptr->complex_f)(af, bf, &af, &bf);
			return real_complex_single_heap_(ret, af, bf);

		case ComplexType_double:
			ad = RefDoubleFloat(real);
			bd = RefDoubleFloat(imag);
			(ptr->complex_d)(ad, bd, &ad, &bd);
			return real_complex_double_heap_(ret, ad, bd);

		case ComplexType_long:
			al = RefLongFloat(real);
			bl = RefLongFloat(imag);
			(ptr->complex_l)(al, bl, &al, &bl);
			return real_complex_long_heap_(ret, al, bl);

		case ComplexType_rational:
			Return(single_float_rational_(real, &af));
			Return(single_float_rational_(imag, &bf));
			(ptr->complex_f)(af, bf, &af, &bf);
			return real_complex_single_heap_(ret, af, bf);

		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}

	return 0;
}

static int forcef_complex_common_(struct mathcall_struct *ptr,
		single_float a, addr *ret)
{
	single_float real, imag;
	(ptr->complex_f)(a, 0.0f, &real, &imag);
	return real_complex_single_heap_(ret, real, imag);
}

static int forced_complex_common_(struct mathcall_struct *ptr,
		double_float a, addr *ret)
{
	double_float real, imag;
	(ptr->complex_d)(a, 0.0, &real, &imag);
	return real_complex_double_heap_(ret, real, imag);
}

static int forcel_complex_common_(struct mathcall_struct *ptr,
		long_float a, addr *ret)
{
	long_float real, imag;
	(ptr->complex_l)(a, 0.0L, &real, &imag);
	return real_complex_long_heap_(ret, real, imag);
}

static int call_common_(struct mathcall_struct *ptr, addr pos, addr *ret)
{
	enum MathType type;
	struct mathtype_struct str;

	Return(getmathtype_float_(&str, pos, &type));
	switch (type) {
		case MathType_single:
			return single_float_check_heap_(ret, (ptr->call_s)(str.v.s));

		case MathType_double:
			return double_float_check_heap_(ret, (ptr->call_d)(str.v.d));

		case MathType_long:
			return long_float_check_heap_(ret, (ptr->call_l)(str.v.l));

		case MathType_complex:
			return call_complex_common_(ptr, pos, ret);

		case MathType_error:
		default:
			*ret = Nil;
			return TypeError_(pos, NUMBER);
	}
}

static int call_range_common_(struct mathcall_struct *ptr, addr pos, addr *ret)
{
	enum MathType type;
	single_float vs;
	double_float vd;
	long_float vl;
	struct mathtype_struct str;

	Return(getmathtype_float_(&str, pos, &type));
	switch (type) {
		case MathType_single:
			vs = str.v.s;
			if ((ptr->range_f)(vs))
				return single_float_check_heap_(ret, (ptr->call_s)(vs));
			else
				return forcef_complex_common_(ptr, vs, ret);
			break;

		case MathType_double:
			vd = str.v.d;
			if ((ptr->range_d)(vd))
				return double_float_check_heap_(ret, (ptr->call_d)(vd));
			else
				return forced_complex_common_(ptr, vd, ret);
			break;

		case MathType_long:
			vl = str.v.l;
			if ((ptr->range_l)(vl))
				return long_float_check_heap_(ret, (ptr->call_l)(vl));
			else
				return forcel_complex_common_(ptr, vl, ret);
			break;

		case MathType_complex:
			return call_complex_common_(ptr, pos, ret);

		case MathType_error:
		default:
			*ret = Nil;
			return TypeError_(pos, NUMBER);
	}

	return 0;
}

int exp_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = cexp_f;
	str.complex_d = cexp_d;
	str.complex_l = cexp_l;
	str.call_s = expf;
	str.call_d = exp;
	str.call_l = expl;
	return call_common_(&str, pos, ret);
}

int sin_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = csin_f;
	str.complex_d = csin_d;
	str.complex_l = csin_l;
	str.call_s = sinf;
	str.call_d = sin;
	str.call_l = sinl;
	return call_common_(&str, pos, ret);
}

int cos_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = ccos_f;
	str.complex_d = ccos_d;
	str.complex_l = ccos_l;
	str.call_s = cosf;
	str.call_d = cos;
	str.call_l = cosl;
	return call_common_(&str, pos, ret);
}

int tan_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = ctan_f;
	str.complex_d = ctan_d;
	str.complex_l = ctan_l;
	str.call_s = tanf;
	str.call_d = tan;
	str.call_l = tanl;
	return call_common_(&str, pos, ret);
}

int sinh_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = csinh_f;
	str.complex_d = csinh_d;
	str.complex_l = csinh_l;
	str.call_s = sinhf;
	str.call_d = sinh;
	str.call_l = sinhl;
	return call_common_(&str, pos, ret);
}

int cosh_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = ccosh_f;
	str.complex_d = ccosh_d;
	str.complex_l = ccosh_l;
	str.call_s = coshf;
	str.call_d = cosh;
	str.call_l = coshl;
	return call_common_(&str, pos, ret);
}

int tanh_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = ctanh_f;
	str.complex_d = ctanh_d;
	str.complex_l = ctanh_l;
	str.call_s = tanhf;
	str.call_d = tanh;
	str.call_l = tanhl;
	return call_common_(&str, pos, ret);
}

static int asinf_range(single_float v)  { return -1.0f <= v && v <= 1.0f; }
static int asind_range(double_float v)  { return -1.0  <= v && v <= 1.0;  }
static int asinl_range(long_float v)    { return -1.0L <= v && v <= 1.0L; }

int asin_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = casin_f;
	str.complex_d = casin_d;
	str.complex_l = casin_l;
	str.call_s = asinf;
	str.call_d = asin;
	str.call_l = asinl;
	str.range_f = asinf_range;
	str.range_d = asind_range;
	str.range_l = asinl_range;
	return call_range_common_(&str, pos, ret);
}

int acos_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = cacos_f;
	str.complex_d = cacos_d;
	str.complex_l = cacos_l;
	str.call_s = acosf;
	str.call_d = acos;
	str.call_l = acosl;
	str.range_f = asinf_range; /* asin */
	str.range_d = asind_range; /* asin */
	str.range_l = asinl_range; /* asin */
	return call_range_common_(&str, pos, ret);
}

int atan_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = catan_f;
	str.complex_d = catan_d;
	str.complex_l = catan_l;
	str.call_s = atanf;
	str.call_d = atan;
	str.call_l = atanl;
	return call_common_(&str, pos, ret);
}

int asinh_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = casinh_f;
	str.complex_d = casinh_d;
	str.complex_l = casinh_l;
	str.call_s = asinhf;
	str.call_d = asinh;
	str.call_l = asinhl;
	return call_common_(&str, pos, ret);
}

static int acoshf_range(single_float v)  { return 1.0f <= v; }
static int acoshd_range(double_float v)  { return 1.0  <= v; }
static int acoshl_range(long_float v)    { return 1.0L <= v; }

int acosh_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = cacosh_f;
	str.complex_d = cacosh_d;
	str.complex_l = cacosh_l;
	str.call_s = acoshf;
	str.call_d = acosh;
	str.call_l = acoshl;
	str.range_f = acoshf_range;
	str.range_d = acoshd_range;
	str.range_l = acoshl_range;
	return call_range_common_(&str, pos, ret);
}

int atanh_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = catanh_f;
	str.complex_d = catanh_d;
	str.complex_l = catanh_l;
	str.call_s = atanhf;
	str.call_d = atanh;
	str.call_l = atanhl;
	str.range_f = asinf_range; /* asin */
	str.range_d = asind_range; /* asin */
	str.range_l = asinl_range; /* asin */
	return call_range_common_(&str, pos, ret);
}


/*
 *  cis
 */
static inline void cis_f(single_float x, single_float *Re, single_float *Im)
{
	*Re = cosf(x);
	*Im = sinf(x);
}

static inline void cis_d(double_float x, double_float *Re, double_float *Im)
{
	*Re = cos(x);
	*Im = sin(x);
}

static inline void cis_l(long_float x, long_float *Re, long_float *Im)
{
	*Re = cosl(x);
	*Im = sinl(x);
}

int cis_common_(addr pos, addr *ret)
{
	enum MathType type;
	single_float single1, single2;
	double_float double1, double2;
	long_float long1, long2;
	struct mathtype_struct str;

	Return(getmathtype_float_(&str, pos, &type));
	switch (type) {
		case MathType_single:
			cis_f(str.v.s, &single1, &single2);
			return complex_single_heap_(ret, single1, single2);

		case MathType_double:
			cis_d(str.v.d, &double1, &double2);
			return complex_double_heap_(ret, double1, double2);

		case MathType_long:
			cis_l(str.v.l, &long1, &long2);
			return complex_long_heap_(ret, long1, long2);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}
}


/*
 *  tan2
 */
int atan2_common_(addr left, addr right, addr *ret)
{
	enum MathType type;
	struct mathreal2_struct str;

	Return(getmathreal2_float_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			return single_float_check_heap_(ret, atan2f(str.v.s.a, str.v.s.b));

		case MathType_double:
			return double_float_check_heap_(ret, atan2(str.v.d.a, str.v.d.b));

		case MathType_long:
			return long_float_check_heap_(ret, atan2l(str.v.l.a, str.v.l.b));

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}

int atan_optional_common_(addr var, addr opt, addr *ret)
{
	if (opt == Unbound)
		return atan_common_(var, ret);
	else
		return atan2_common_(var, opt, ret);
}


/*
 *  log
 */
static int log_natural_complex_(addr value, addr *ret)
{
	single_float reals, imags;
	double_float reald, imagd;
	long_float reall, imagl;

	CheckType(value, LISPTYPE_COMPLEX);
	switch (GetTypeComplex(value)) {
		case ComplexType_rational:
		case ComplexType_single:
			Return(single_float_complex_(value, &reals, &imags));
			clog_f(reals, imags, &reals, &imags);
			return real_complex_single_heap_(ret, reals, imags);

		case ComplexType_double:
			Return(double_float_complex_(value, &reald, &imagd));
			clog_d(reald, imagd, &reald, &imagd);
			return real_complex_double_heap_(ret, reald, imagd);

		case ComplexType_long:
			Return(long_float_complex_(value, &reall, &imagl));
			clog_l(reall, imagl, &reall, &imagl);
			return real_complex_long_heap_(ret, reall, imagl);

		case ComplexType_error:
		default:
			*ret = Nil;
			return TypeError_(value, COMPLEX);
	}
}

int log_natural_common_(addr value, addr *ret)
{
	enum MathType type;
	struct mathreal2_struct str;
	single_float reals, imags;
	double_float reald, imagd;
	long_float reall, imagl;

	Return(getmathcomplex1_log_(&str, value, &type));
	switch (type) {
		case MathType_single:
			clog_f(str.v.s.a, str.v.s.b, &reals, &imags);
			return real_complex_single_heap_(ret, reals, imags);

		case MathType_double:
			clog_d(str.v.d.a, str.v.d.b, &reald, &imagd);
			return real_complex_double_heap_(ret, reald, imagd);

		case MathType_long:
			clog_l(str.v.l.a, str.v.l.b, &reall, &imagl);
			return real_complex_long_heap_(ret, reall, imagl);

		case MathType_complex:
			return log_natural_complex_(value, ret);

		case MathType_rational:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}

int log_base_common_(addr value, addr base, addr *ret)
{
	enum MathType type;
	single_float reals, imags;
	double_float reald, imagd;
	long_float reall, imagl;
	struct mathcomplex2_struct str;

	Return(getmathcomplex2_float_(&str, value, base, &type));
	switch (type) {
		case MathType_single:
			Return(clogb_f_(str.v.s.a, str.v.s.b, str.v.s.c, str.v.s.d, &reals, &imags));
			return real_complex_single_heap_(ret, reals, imags);

		case MathType_double:
			Return(clogb_d_(str.v.d.a, str.v.d.b, str.v.d.c, str.v.d.d, &reald, &imagd));
			return real_complex_double_heap_(ret, reald, imagd);

		case MathType_long:
			Return(clogb_l_(str.v.l.a, str.v.l.b, str.v.l.c, str.v.l.d, &reall, &imagl));
			return real_complex_long_heap_(ret, reall, imagl);

		case MathType_complex:
		case MathType_rational:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}

int log_common_(addr value, addr base, addr *ret)
{
	if (base == Unbound)
		return log_natural_common_(value, ret);
	else
		return log_base_common_(value, base, ret);
}


/*
 *  phase
 */
static int phase_complex_common_(addr pos, addr *ret)
{
	single_float sr, si;
	double_float dr, di;
	long_float lr, li;

	CheckType(pos, LISPTYPE_COMPLEX);
	switch (GetTypeComplex(pos)) {
		case ComplexType_rational:
		case ComplexType_single:
			Return(single_float_complex_(pos, &sr, &si));
			single_float_heap(ret, atan2f(si, sr));
			break;

		case ComplexType_double:
			Return(double_float_complex_(pos, &dr, &di));
			double_float_heap(ret, atan2(di, dr));
			break;

		case ComplexType_long:
			Return(long_float_complex_(pos, &lr, &li));
			long_float_heap(ret, atan2l(li, lr));
			break;

		case ComplexType_error:
		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}

	return 0;
}

static int phase_rational_common_(addr pos, addr *ret)
{
	int check;

	Return(zerop_rational_(pos, &check));
	if (check) {
		single_float_heap(ret, 0.0f);
		return 0;
	}
	Return(minusp_rational_(pos, &check));
	single_float_heap(ret, check? LISP_PI_SINGLE: 0.0f);

	return 0;
}

static int phase_single_common_(addr pos, addr *ret)
{
	single_float value;

	GetSingleFloat(pos, &value);
	single_float_heap(ret, signbit(value)? LISP_PI_SINGLE: 0.0f);

	return 0;
}

static int phase_double_common_(addr pos, addr *ret)
{
	double_float value;

	GetDoubleFloat(pos, &value);
	double_float_heap(ret, signbit(value)? LISP_PI_DOUBLE: 0.0);

	return 0;
}

static int phase_long_common_(addr pos, addr *ret)
{
	long_float value;

	GetLongFloat(pos, &value);
	long_float_heap(ret, signbit(value)? LISP_PI_LONG: 0.0L);

	return 0;
}

int phase_common_(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_COMPLEX:
			return phase_complex_common_(pos, ret);

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			return phase_rational_common_(pos, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return phase_single_common_(pos, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return phase_double_common_(pos, ret);

		case LISPTYPE_LONG_FLOAT:
			return phase_long_common_(pos, ret);

		default:
			*ret = Nil;
			return TypeError_(pos, NUMBER);
	}

	return 0;
}


/************************************************************
 *  math_gcd.c
 ************************************************************/

/*
 *  gcd
 */
static int gcd_buffer_size_(addr pos, size_t *ret)
{
	size_t size;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, 1);

		case LISPTYPE_BIGNUM:
			GetSizeBignum(pos, &size);
			return Result(ret, size);

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

static int gcd_first_number_(addr pos, addr *ret)
{
	int check;

	Check(! integerp(pos), "type error");
	Return(minusp_integer_(pos, &check));
	if (check)
		return sign_reverse_integer_common_(pos, ret);
	else
		return integer_throw_heap_(pos, ret);
}

static int copy_noexpand_integer_(addr left, addr right)
{
	int sign;
	fixed value;

	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			castfixed_fixnum(right, &sign, &value);
			setvalue_bignum(left, sign, value);
			break;

		case LISPTYPE_BIGNUM:
			copy_noexpand_bignum(left, right);
			break;

		default:
			return TypeError_(right, INTEGER);
	}

	return 0;
}

static int gcd_loop_number_(LocalRoot local, addr left, addr first, addr args)
{
	int check;
	addr right;
	LocalStack stack;

	Return(copy_noexpand_integer_(left, first));
	while (args != Nil) {
		GetCons(args, &right, &args);
		Return(zerop_integer_(right, &check));
		if (check)
			continue;
		push_local(local, &stack);
		Return(bignum_integer_local_(local, &right, right));
		euclidean_bignum(local, left, right);
		rollback_local(local, stack);
	}

	return 0;
}

int gcd_number_(LocalRoot local, addr args, addr *ret)
{
	int check;
	addr left, right, pos, first_left, first_right;
	LocalStack stack;
	size_t count, value, size;

	/* check */
	first_left = first_right = Unbound;
	count = 0;
	size = 0;
	for (right = args; right != Nil; ) {
		Return_getcons(right, &left, &right);
		Check(! integerp(left), "type error");
		/* ignore */
		Return(zerop_integer_(left, &check));
		if (check)
			continue;
		/* first */
		if (first_left == Unbound) {
			first_left = left;
			first_right = right;
		}
		/* max size */
		Return(gcd_buffer_size_(left, &value));
		if (size < value)
			size = value;
		/* count */
		count++;
	}

	/* no argument */
	if (count == 0) {
		fixnum_heap(ret, 0);
		return 0;
	}

	/* only one argument */
	if (count == 1)
		return gcd_first_number_(first_left, ret);

	/* second */
	push_local(local, &stack);
	bignum_local(local, &pos, SignPlus, size);
	Return(gcd_loop_number_(local, pos, first_left, first_right));
	SetSignBignum(pos, SignPlus);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);

	return 0;
}


/*
 *  lcm
 */
static int lcm_calc_number_(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;
	addr temp;

	/* (lcm a b) ==  (/ (abs (* a b)) (gcd a b)) */
	push_local(local, &stack);
	Return(bignum_integer_local_(local, &left, left));
	Return(bignum_integer_local_(local, &right, right));
	/* (* a b) */
	multi_bb_bignum_local(local, left, right, &temp);
	/* (gcd a b) */
	euclidean_bignum(local, left, right);
	/* (/ temp left) */
	letdiv_noexpand_bigdata(local, temp, left);
	bignum_throw_heap(temp, ret);
	/* result */
	rollback_local(local, stack);

	return 0;
}

static int lcm_loop_number_(LocalRoot local, addr left, addr args, addr *ret)
{
	int check;
	addr right;

	while (args != Nil) {
		Return_getcons(args, &right, &args);
		Check(! integerp(right), "type error");
		Return(zerop_integer_(right, &check));
		if (check) {
			fixnum_heap(ret, 0);
			return 0;
		}
		Return(lcm_calc_number_(local, &left, left, right));
	}

	return Result(ret, left);
}

int lcm_number_(LocalRoot local, addr args, addr *ret)
{
	int check;
	addr left;

	if (args == Nil) {
		fixnum_heap(ret, 1);
		return 0;
	}

	/* only one argument */
	Return_getcons(args, &left, &args);
	if (args == Nil)
		return gcd_first_number_(left, ret);

	/* zero */
	Check(! integerp(left), "type error");
	Return(zerop_integer_(left, &check));
	if (check) {
		fixnum_heap(ret, 0);
		return 0;
	}

	/* loop */
	return lcm_loop_number_(local, left, args, ret);
}


/************************************************************
 *  math_isqrt.c
 ************************************************************/
/*
 *  Kaiheihou  (extraction of square root)
 */

static void isqrt_shiftup(addr pos, unsigned shift)
{
	fixed *data, x, carry;
	size_t size, i;

	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);
	carry = 0;
	for (i = 0; i < size; i++) {
		x = data[i];
		data[i] = (x << shift) | carry;
		carry = x >> (BIGNUM_FULLBIT - shift);
	}
	if (carry) {
		Check(RefAllocBignum(pos) <= size, "size error");
		data[i] = carry;
		SetSizeBignum(pos, size + 1);
	}
}

static void isqrt_letbit2(addr pos, unsigned a, unsigned b)
{
	fixed *data;
	GetDataBignum(pos, &data);
	data[0] |= (a << 1) | b;
}

static void isqrt_letbit1(addr pos, unsigned a)
{
	fixed *data;
	GetDataBignum(pos, &data);
	data[0] |= a;
}

static void isqrt_letbit1_remove(addr pos)
{
	fixed *data;
	GetDataBignum(pos, &data);
	data[0] >>= 1;
	data[0] <<= 1;
}

static void isqrt_compare(addr x, addr y, addr z)
{
	isqrt_shiftup(x, 1);
	isqrt_shiftup(y, 1);
	isqrt_letbit1(y, 1);
	if (compare_bigdata(y, z) <= 0) {
		letminus_noexpand_bigdata(z, y);
		setplusvalue_bigdata(y, y, SignPlus, 1);
		isqrt_letbit1(x, 1);
	}
	else {
		isqrt_letbit1_remove(y);
	}
}

static void isqrt_loop(addr var, size_t size, addr x, addr y, addr z)
{
	unsigned a, b;

	Check(size % 2, "size error");
	while (size) {
		a = getbit_bignum(var, --size);
		Check(size == 0, "size zero error");
		b = getbit_bignum(var, --size);
		isqrt_shiftup(z, 2);
		isqrt_letbit2(z, a, b);
		isqrt_compare(x, y, z);
	}
}

static int isqrt_buffer_(addr var, addr x, addr y, addr z)
{
	unsigned a, b;
	fixed value;
	size_t size;

	Return(integer_length_value_(var, &size));
	Check(size == 0, "size error");
	if (size % 2) {
		value = (fixed)getbit_bignum(var, --size);
	}
	else {
		a = getbit_bignum(var, --size);
		b = getbit_bignum(var, --size);
		value = (fixed)((a << 1) | b);
	}
	setvalue_bignum(x, SignPlus, 1);
	setvalue_bignum(y, SignPlus, 2); /* #b10 */
	setvalue_bignum(z, SignPlus, value);
	setminusvalue_bigdata(z, z, SignPlus, 1);
	isqrt_loop(var, size, x, y, z);

	return 0;
}

static int isqrt_bignum_(LocalRoot local, addr var, addr *ret)
{
	addr x, y, z;
	size_t size;

	/* local buffer */
	GetSizeBignum(var, &size);
	size = (size >> 1) + 1;
	bignum_local(local, &x, SignPlus, size);
	bignum_local(local, &y, SignPlus, size);
	bignum_local(local, &z, SignPlus, size);

	/* result */
	Return(isqrt_buffer_(var, x, y, z));
	*ret = x;

	return 0;
}

int isqrt_number_common_(LocalRoot local, addr var, addr *ret)
{
	int check;
	LocalStack stack;

	/* zero */
	Check(minusp_integer_debug(var), "minum error");
	Return(plusp_integer_(var, &check));
	if (! check) {
		fixnum_heap(ret, 0);
		return 0;
	}

	/* isqrt */
	push_local(local, &stack);
	if (fixnump(var))
		bignum_fixnum_local(local, &var, var);
	Return(isqrt_bignum_(local, var, &var));
	bignum_result_heap(var, ret);
	rollback_local(local, stack);

	return 0;
}


/************************************************************
 *  math_power.c
 ************************************************************/

/*
 *  expt
 */
static void expr_multi_integer_heap(addr a, addr b, addr *ret)
{
	if (a == NULL)
		*ret = b;
	else
		multi_bigdata_alloc(NULL, a, b, ret);
}

static void expt_integer_heap(addr *ret, addr base, size_t power)
{
	unsigned i, size;
	addr v;
	size_t check;

	CheckType(base, LISPTYPE_BIGNUM);
	/* high bit */
	check = power;
	for (size = 0; check; size++)
		check >>= 1;
	Check(size == 0, "power error");
	/* power */
	v = NULL;
	for (i = 0; i < size; i++) {
		expr_multi_integer_heap(v, v, &v);
		if ((power >> (size - i - 1)) & 1)
			expr_multi_integer_heap(v, base, &v);
	}
	if (v == NULL)
		bignum_value_heap(ret, SignPlus, 1);
	else
		bignum_throw_heap(v, ret);
}

static int bignum_if_fixnum_local_(LocalRoot local, addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			bignum_fixnum_local(local, ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return Result(ret, pos);

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

static void expr_inverse_bignum_heap(int sign, addr denom, addr *ret)
{
	addr numer;

	CheckType(denom, LISPTYPE_BIGNUM);
	bignum_value_heap(&numer, SignPlus, 1);
	make_ratio_alloc_unsafe(NULL, ret, sign, numer, denom);
}

static int expt_integer_common_(LocalRoot local, addr *ret, addr base, addr power)
{
	int sign, inverse, check;
	size_t size;

	CheckLocal(local);
	Return(bignum_if_fixnum_local_(local, &base, base));
	Return(getindex_sign_integer_(power, &inverse, &size, &check));
	if (check) {
		*ret = Nil;
		return fmte_("Too large expt power ~A.", power, NULL);
	}

	/* sign, inverse */
	GetSignBignum(base, &sign);
	if (IsMinus(sign))
		sign = (size & 1)? SignMinus: SignPlus;

	/* result */
	expt_integer_heap(&base, base, size);
	if (inverse) {
		expr_inverse_bignum_heap(sign, base, &base);
		ratio_result_noreduction_heap(local, base, ret);
	}
	else {
		SetSignBignum(base, sign);
		bignum_result_heap(base, ret);
	}

	return 0;
}

static int expt_ratio_common_(LocalRoot local, addr *ret, addr base, addr power)
{
	int check, sign, inverse;
	addr numer, denom;
	size_t size;

	CheckLocalType(local, base, LISPTYPE_RATIO);
	Return(getindex_sign_integer_(power, &inverse, &size, &check));
	if (check) {
		*ret = Nil;
		return fmte_("Too large expt power ~A.", power, NULL);
	}

	/* sign, inverse */
	GetSignRatio(base, &sign);
	if (IsMinus(sign))
		sign = (size & 1)? SignMinus: SignPlus;

	/* result */
	GetNumerRatio(base, &numer);
	GetDenomRatio(base, &denom);
	expt_integer_heap(&numer, numer, size);
	expt_integer_heap(&denom, denom, size);
	if (inverse)
		make_ratio_alloc_unsafe(NULL, &base, sign, denom, numer);
	else
		make_ratio_alloc_unsafe(NULL, &base, sign, numer, denom);
	ratio_result_noreduction_heap(local, base, ret);

	return 0;
}

static int expt_float_common_(addr *ret, addr base, addr power)
{
	single_float v1, v2;

	Return(single_float_rational_(base, &v1));
	Return(single_float_rational_(power, &v2));
	expt_f(v1, 0.0f, v2, 0.0f, &v1, &v2);
	return complex_single_heap_(ret, v1, v2);
}

static int expt_rr_common_(LocalRoot local, addr *ret, addr base, addr power)
{
	/* ff, fb, bf, bb -> integer
	 * rf, rb         -> ratio
	 * fr, br, rr     -> float
	 */
	int check;
	LocalStack stack;

	Return(zerop_rational_(power, &check));
	if (check) {
		fixnum_heap(ret, 1);
		return 0;
	}
	Return(zerop_rational_(base, &check));
	if (check) {
		Return(minusp_rational_(power, &check));
		if (check) {
			*ret = Nil;
			return call_division_by_zero_real2_(NULL,
					CONSTANT_COMMON_EXPT, base, power);
		}
		fixnum_heap(ret, 0);
		return 0;
	}

	push_local(local, &stack);
	if (ratiop(power)) {
		Return(expt_float_common_(ret, base, power));
	}
	else if (ratiop(base)) {
		Return(expt_ratio_common_(local, ret, base, power));
	}
	else {
		Return(expt_integer_common_(local, ret, base, power));
	}
	rollback_local(local, stack);

	return 0;
}

static int expt_single_common_(struct mathcomplex2_struct *ptr, addr *ret)
{
	single_float real, imag;
	expt_f(ptr->v.s.a, ptr->v.s.b, ptr->v.s.c, ptr->v.s.d, &real, &imag);
	return real_complex_single_heap_(ret, real, imag);
}

static int expt_double_common_(struct mathcomplex2_struct *ptr, addr *ret)
{
	double_float real, imag;
	expt_d(ptr->v.d.a, ptr->v.d.b, ptr->v.d.c, ptr->v.d.d, &real, &imag);
	return real_complex_double_heap_(ret, real, imag);
}

static int expt_long_common_(struct mathcomplex2_struct *ptr, addr *ret)
{
	long_float real, imag;
	expt_l(ptr->v.l.a, ptr->v.l.b, ptr->v.l.c, ptr->v.l.d, &real, &imag);
	return real_complex_long_heap_(ret, real, imag);
}

static int expt_force_single_(LocalRoot local, addr *ret, addr base, addr power)
{
	enum MathType type;
	struct mathcomplex2_struct str;

	Return(getmathcomplex2_float_(&str, base, power, &type));
	switch (type) {
		case MathType_single:
			return expt_single_common_(&str, ret);

		default:
			*ret = 0;
			return fmte_("Type error", NULL);
	}
}

static int expr_multi_complex_heap_(LocalRoot local, addr a, addr b, addr *ret)
{
	if (a == NULL)
		return Result(ret, b);
	else
		return multi_number_heap_(local, a, b, ret);
}

static int expt_complex_heap_(LocalRoot local, addr *ret, addr base, size_t power)
{
	unsigned i, size;
	addr v;
	size_t check;

	CheckLocalType(local, base, LISPTYPE_COMPLEX);
	/* high bit */
	check = power;
	for (size = 0; check; size++)
		check >>= 1;
	Check(size == 0, "power error");
	/* power */
	v = NULL;
	for (i = 0; i < size; i++) {
		Return(expr_multi_complex_heap_(local, v, v, &v));
		if ((power >> (size - i - 1)) & 1) {
			Return(expr_multi_complex_heap_(local, v, base, &v));
		}
	}
	if (v == NULL)
		fixnum_heap(ret, 1);
	else
		*ret = v;

	return 0;
}

static int expt_complex_integer_(LocalRoot local, addr *ret, addr base, addr power)
{
	int inverse, check;
	size_t size;

	CheckLocal(local);
	CheckType(base, LISPTYPE_COMPLEX);
	Check(! integerp(power), "type error");
	Return(getindex_sign_integer_(power, &inverse, &size, &check));
	if (check) {
		*ret = Nil;
		return fmte_("Too large expt power ~A.", power, NULL);
	}

	Return(expt_complex_heap_(local, ret, base, size));
	if (inverse)
		return inverse_complex_common_(local, *ret, ret);

	return 0;
}

static int expt_rational_common_(LocalRoot local, addr *ret, addr base, addr power)
{
	if (complexp(power) || ratiop(power)) {
		/* float */
		return expt_force_single_(local, ret, base, power);
	}
	else if (complexp(base)) {
		/* complex - integer */
		return expt_complex_integer_(local, ret, base, power);
	}
	else {
		/* rational */
		return expt_rr_common_(local, ret, base, power);
	}
}

int expt_common_(LocalRoot local, addr *ret, addr base, addr power)
{
	enum MathType type;
	struct mathcomplex2_struct str;

	Return(getmathcomplex2_addr_(&str, base, power, &type));
	switch (type) {
		case MathType_single:
			return expt_single_common_(&str, ret);

		case MathType_double:
			return expt_double_common_(&str, ret);

		case MathType_long:
			return expt_long_common_(&str, ret);

		case MathType_rational:
			return expt_rational_common_(local, ret, base, power);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = 0;
			return fmte_("type error", NULL);
	}
}


/************************************************************
 *  math_type.c
 ************************************************************/

/*
 *  getmathtype
 */
int getmathtype_float_(struct mathtype_struct *ptr, addr pos, enum MathType *ret)
{
	enum MathType type;

	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			type = MathType_single;
			ptr->v.s = RefSingleFloat(pos);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			type = MathType_double;
			ptr->v.d = RefDoubleFloat(pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			type = MathType_long;
			ptr->v.l = RefLongFloat(pos);
			break;

		case LISPTYPE_COMPLEX:
			type = MathType_complex;
			break;

		case LISPTYPE_FIXNUM:
			type = MathType_single;
			ptr->v.s = single_float_fixnum(pos);
			break;

		case LISPTYPE_BIGNUM:
			type = MathType_single;
			Return(single_float_bignum_(pos, &(ptr->v.s)));
			break;

		case LISPTYPE_RATIO:
			type = MathType_single;
			Return(single_float_ratio_(pos, &(ptr->v.s)));
			break;

		default:
			type = MathType_error;
			break;
	}
	ptr->type = type;
	return Result(ret, type);
}


/*
 *  getmathreal2
 */
static void getmathreal2_type_value(addr pos, enum MathType *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*ret = MathType_single;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = MathType_double;
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = MathType_long;
			break;

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			*ret = MathType_rational;
			break;

		case LISPTYPE_COMPLEX:
		default:
			*ret = MathType_error;
			break;
	}
}

static int getmathreal2_type_single_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathreal2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
			return Result(ret, type);

		case MathType_rational:
			return Result(ret, MathType_single);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, REAL);
	}
}

static int getmathreal2_type_double_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathreal2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_rational:
			return Result(ret, MathType_double);

		case MathType_long:
			return Result(ret, MathType_long);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, REAL);
	}
}

static int getmathreal2_type_long_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathreal2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
		case MathType_rational:
			return Result(ret, MathType_long);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, REAL);
	}
}

static int getmathreal2_type_rational_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathreal2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
		case MathType_rational:
			return Result(ret, type);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, REAL);
	}
}

static int getmathreal2_type_(addr x, addr y, enum MathType *ret)
{
	enum MathType type;

	getmathreal2_type_value(x, &type);
	switch (type) {
		case MathType_single:
			return getmathreal2_type_single_(y, ret);

		case MathType_double:
			return getmathreal2_type_double_(y, ret);

		case MathType_long:
			return getmathreal2_type_long_(y, ret);

		case MathType_rational:
			return getmathreal2_type_rational_(y, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(x, REAL);
	}
}

static int getmathreal2_single1_(addr pos, single_float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return cast_ss_value_(pos, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return cast_ds_value_(pos, ret);

		case LISPTYPE_LONG_FLOAT:
			return cast_ls_value_(pos, ret);

		case LISPTYPE_FIXNUM:
			*ret = single_float_fixnum(pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return single_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return single_float_ratio_(pos, ret);

		default:
			*ret = 0;
			return TypeError_(pos, REAL);
	}
}

static int getmathreal2_double1_(addr pos, double_float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return cast_sd_value_(pos, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return cast_dd_value_(pos, ret);

		case LISPTYPE_LONG_FLOAT:
			return cast_ld_value_(pos, ret);

		case LISPTYPE_FIXNUM:
			*ret = double_float_fixnum(pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return double_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return double_float_ratio_(pos, ret);

		default:
			*ret = 0;
			return TypeError_(pos, REAL);
	}
}

static int getmathreal2_long1_(addr pos, long_float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return cast_sl_value_(pos, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return cast_dl_value_(pos, ret);

		case LISPTYPE_LONG_FLOAT:
			return cast_ll_value_(pos, ret);

		case LISPTYPE_FIXNUM:
			*ret = long_float_fixnum(pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return long_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return long_float_ratio_(pos, ret);

		default:
			*ret = 0;
			return TypeError_(pos, REAL);
	}
}

static int getmathreal2_single_(struct mathreal2_struct *ptr, addr x, addr y)
{
	single_float value;

	Return(getmathreal2_single1_(x, &value));
	ptr->v.s.a = value;
	Return(getmathreal2_single1_(y, &value));
	ptr->v.s.b = value;

	return 0;
}

static int getmathreal2_double_(struct mathreal2_struct *ptr, addr x, addr y)
{
	double_float value;

	Return(getmathreal2_double1_(x, &value));
	ptr->v.d.a = value;
	Return(getmathreal2_double1_(y, &value));
	ptr->v.d.b = value;

	return 0;
}

static int getmathreal2_long_(struct mathreal2_struct *ptr, addr x, addr y)
{
	long_float value;

	Return(getmathreal2_long1_(x, &value));
	ptr->v.l.a = value;
	Return(getmathreal2_long1_(y, &value));
	ptr->v.l.b = value;

	return 0;
}

int getmathreal2_float_(struct mathreal2_struct *ptr,
		addr x, addr y, enum MathType *ret)
{
	enum MathType type;

	Return(getmathreal2_type_(x, y, &type));
	switch (type) {
		case MathType_single:
			Return(getmathreal2_single_(ptr, x, y));
			break;

		case MathType_double:
			Return(getmathreal2_double_(ptr, x, y));
			break;

		case MathType_long:
			Return(getmathreal2_long_(ptr, x, y));
			break;

		case MathType_rational:
			Return(getmathreal2_single_(ptr, x, y));
			type = MathType_single;
			break;

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return fmte_("type error", NULL);
	}
	ptr->type = type;

	return Result(ret, type);
}

static void getmathreal2_rational(struct mathreal2_struct *ptr, addr x, addr y)
{
	ptr->v.a.x = x;
	ptr->v.a.y = y;
}

int getmathreal2_addr_(struct mathreal2_struct *ptr,
		addr x, addr y, enum MathType *ret)
{
	enum MathType type;

	Return(getmathreal2_type_(x, y, &type));
	switch (type) {
		case MathType_single:
			Return(getmathreal2_single_(ptr, x, y));
			break;

		case MathType_double:
			Return(getmathreal2_double_(ptr, x, y));
			break;

		case MathType_long:
			Return(getmathreal2_long_(ptr, x, y));
			break;

		case MathType_rational:
			getmathreal2_rational(ptr, x, y);
			break;

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return fmte_("type error", NULL);
	}
	ptr->type = type;

	return Result(ret, type);
}


/*
 *  getmathcomplex1
 */
static void getmathcomplex_type_complex(addr pos, enum MathType *ret)
{
	switch (GetTypeComplex(pos)) {
		case ComplexType_single:
			*ret = MathType_single;
			break;

		case ComplexType_double:
			*ret = MathType_double;
			break;

		case ComplexType_long:
			*ret = MathType_long;
			break;

		case ComplexType_rational:
			*ret = MathType_rational;
			break;

		case ComplexType_error:
		default:
			*ret = MathType_error;
			break;
	}
}

static void getmathcomplex1_log_type(addr pos, enum MathType *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*ret = MathType_single;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = MathType_double;
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = MathType_long;
			break;

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			*ret = MathType_rational;
			break;

		case LISPTYPE_COMPLEX:
			*ret = MathType_complex;
			break;

		default:
			*ret = MathType_error;
			break;
	}
}

static void getmathcomplex1_inverse_type(addr pos, enum MathType *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*ret = MathType_single;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = MathType_double;
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = MathType_long;
			break;

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			*ret = MathType_rational;
			break;

		case LISPTYPE_COMPLEX:
			getmathcomplex_type_complex(pos, ret);
			break;

		default:
			*ret = MathType_error;
			break;
	}
}

static int getmathcomplex1_complex_(struct mathreal2_struct *ptr,
		addr pos, enum MathType *ret)
{
	switch (GetTypeComplex(pos)) {
		case ComplexType_single:
			ptr->type = MathType_single;
			Return(single_float_complex_(pos, &(ptr->v.s.a), &(ptr->v.s.b)));
			break;

		case ComplexType_double:
			ptr->type = MathType_double;
			Return(double_float_complex_(pos, &(ptr->v.d.a), &(ptr->v.d.b)));
			break;

		case ComplexType_long:
			ptr->type = MathType_long;
			Return(long_float_complex_(pos, &(ptr->v.l.a), &(ptr->v.l.b)));
			break;

		case ComplexType_rational:
			ptr->type = MathType_single;
			Return(single_float_complex_(pos, &(ptr->v.s.a), &(ptr->v.s.b)));
			break;

		case ComplexType_error:
		default:
			*ret = MathType_error;
			return TypeError_(pos, COMPLEX);
	}

	return Result(ret, ptr->type);
}

int getmathcomplex1_log_(struct mathreal2_struct *ptr,
		addr pos, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex1_log_type(pos, &type);
	switch (type) {
		case MathType_single:
			Return(cast_ss_value_(pos, &(ptr->v.s.a)));
			ptr->v.s.b = 0.0f;
			break;

		case MathType_double:
			Return(cast_dd_value_(pos, &(ptr->v.d.a)));
			ptr->v.d.b = 0.0;
			break;

		case MathType_long:
			Return(cast_ll_value_(pos, &(ptr->v.l.a)));
			ptr->v.l.b = 0.0L;
			break;

		case MathType_rational:
			Return(single_float_rational_(pos, &(ptr->v.s.a)));
			ptr->v.s.b = 0.0f;
			type = MathType_single;
			break;

		case MathType_complex:
			break;

		case MathType_error:
		default:
			*ret = MathType_error;
			return fmte_("type error", NULL);
	}
	ptr->type = type;

	return Result(ret, type);
}

int getmathcomplex1_inverse_(struct mathreal2_struct *ptr,
		addr pos, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex1_inverse_type(pos, &type);
	switch (type) {
		case MathType_single:
			Return(cast_ss_value_(pos, &(ptr->v.s.a)));
			ptr->v.s.b = 0.0f;
			break;

		case MathType_double:
			Return(cast_dd_value_(pos, &(ptr->v.d.a)));
			ptr->v.d.b = 0.0;
			break;

		case MathType_long:
			Return(cast_ll_value_(pos, &(ptr->v.l.a)));
			ptr->v.l.b = 0.0L;
			break;

		case MathType_rational:
			ptr->v.a.x = pos;
			break;

		case MathType_complex:
			Return(getmathcomplex1_complex_(ptr, pos, &type));
			break;

		case MathType_error:
		default:
			*ret = MathType_error;
			return fmte_("type error", NULL);
	}
	ptr->type = type;

	return Result(ret, type);
}

int getmathcomplex1_sqrt_(struct mathreal2_struct *ptr,
		addr pos, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex1_log_type(pos, &type);
	switch (type) {
		case MathType_single:
			Return(cast_ss_value_(pos, &(ptr->v.s.a)));
			ptr->v.s.b = 0.0f;
			break;

		case MathType_double:
			Return(cast_dd_value_(pos, &(ptr->v.d.a)));
			ptr->v.d.b = 0.0;
			break;

		case MathType_long:
			Return(cast_ll_value_(pos, &(ptr->v.l.a)));
			ptr->v.l.b = 0.0L;
			break;

		case MathType_rational:
			Return(single_float_rational_(pos, &(ptr->v.s.a)));
			ptr->v.s.b = 0.0f;
			type = MathType_single;
			break;

		case MathType_complex:
			Return(getmathcomplex1_complex_(ptr, pos, &type));
			break;

		case MathType_error:
		default:
			*ret = MathType_error;
			return fmte_("type error", NULL);
	}
	ptr->type = type;

	return Result(ret, type);
}


/*
 *  getmathcomplex2
 */
static void getmathcomplex2_type_value(addr pos, enum MathType *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*ret = MathType_single;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = MathType_double;
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = MathType_long;
			break;

		case LISPTYPE_COMPLEX:
			getmathcomplex_type_complex(pos, ret);
			break;

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			*ret = MathType_rational;
			break;

		default:
			*ret = MathType_error;
			break;
	}
}

static int getmathcomplex2_type_single_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
			return Result(ret, type);

		case MathType_rational:
			return Result(ret, MathType_single);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, NUMBER);
	}
}

static int getmathcomplex2_type_double_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_rational:
			return Result(ret, MathType_double);

		case MathType_long:
			return Result(ret, MathType_long);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, NUMBER);
	}
}

static int getmathcomplex2_type_long_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
		case MathType_rational:
			return Result(ret, MathType_long);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, NUMBER);
	}
}

static int getmathcomplex2_type_rational_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
		case MathType_rational:
			return Result(ret, type);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, NUMBER);
	}
}

static int getmathcomplex2_type_(addr x, addr y, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex2_type_value(x, &type);
	switch (type) {
		case MathType_single:
			return getmathcomplex2_type_single_(y, ret);

		case MathType_double:
			return getmathcomplex2_type_double_(y, ret);

		case MathType_long:
			return getmathcomplex2_type_long_(y, ret);

		case MathType_rational:
			return getmathcomplex2_type_rational_(y, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(x, NUMBER);
	}
}

static int getmathcomplex2_single1_(addr pos, single_float *re, single_float *im)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			Return(cast_ss_value_(pos, re));
			*im = 0.0f;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			Return(cast_ds_value_(pos, re));
			*im = 0.0f;
			break;

		case LISPTYPE_LONG_FLOAT:
			Return(cast_ls_value_(pos, re));
			*im = 0.0f;
			break;

		case LISPTYPE_COMPLEX:
			return single_float_complex_(pos, re, im);

		case LISPTYPE_FIXNUM:
			*re = single_float_fixnum(pos);
			*im = 0.0f;
			break;

		case LISPTYPE_BIGNUM:
			Return(single_float_bignum_(pos, re));
			*im = 0.0f;
			return 0;

		case LISPTYPE_RATIO:
			Return(single_float_ratio_(pos, re));
			*im = 0.0f;
			return 0;

		default:
			*re = 0.0f;
			*im = 0.0f;
			return TypeError_(pos, NUMBER);
	}

	return 0;
}

static int getmathcomplex2_double1_(addr pos, double_float *re, double_float *im)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			Return(cast_sd_value_(pos, re));
			*im = 0.0;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			Return(cast_dd_value_(pos, re));
			*im = 0.0;
			break;

		case LISPTYPE_LONG_FLOAT:
			Return(cast_ld_value_(pos, re));
			*im = 0.0;
			break;

		case LISPTYPE_COMPLEX:
			return double_float_complex_(pos, re, im);

		case LISPTYPE_FIXNUM:
			*re = double_float_fixnum(pos);
			*im = 0.0;
			break;

		case LISPTYPE_BIGNUM:
			Return(double_float_bignum_(pos, re));
			*im = 0.0;
			break;

		case LISPTYPE_RATIO:
			Return(double_float_ratio_(pos, re));
			*im = 0.0;
			break;

		default:
			*re = 0.0;
			*im = 0.0;
			return TypeError_(pos, NUMBER);
	}

	return 0;
}

static int getmathcomplex2_long1_(addr pos, long_float *re, long_float *im)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			Return(cast_sl_value_(pos, re));
			*im = 0.0L;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			Return(cast_dl_value_(pos, re));
			*im = 0.0L;
			break;

		case LISPTYPE_LONG_FLOAT:
			Return(cast_ll_value_(pos, re));
			*im = 0.0L;
			break;

		case LISPTYPE_COMPLEX:
			return long_float_complex_(pos, re, im);

		case LISPTYPE_FIXNUM:
			*re = long_float_fixnum(pos);
			*im = 0.0L;
			break;

		case LISPTYPE_BIGNUM:
			Return(long_float_bignum_(pos, re));
			*im = 0.0L;
			break;

		case LISPTYPE_RATIO:
			Return(long_float_ratio_(pos, re));
			*im = 0.0L;
			break;

		default:
			*re = 0.0L;
			*im = 0.0L;
			return TypeError_(pos, NUMBER);
	}

	return 0;
}

static int getmathcomplex2_single_(struct mathcomplex2_struct *ptr, addr x, addr y)
{
	single_float real, imag;

	Return(getmathcomplex2_single1_(x, &real, &imag));
	ptr->v.s.a = real;
	ptr->v.s.b = imag;
	Return(getmathcomplex2_single1_(y, &real, &imag));
	ptr->v.s.c = real;
	ptr->v.s.d = imag;

	return 0;
}

static int getmathcomplex2_double_(struct mathcomplex2_struct *ptr, addr x, addr y)
{
	double_float real, imag;

	Return(getmathcomplex2_double1_(x, &real, &imag));
	ptr->v.d.a = real;
	ptr->v.d.b = imag;
	Return(getmathcomplex2_double1_(y, &real, &imag));
	ptr->v.d.c = real;
	ptr->v.d.d = imag;

	return 0;
}

static int getmathcomplex2_long_(struct mathcomplex2_struct *ptr, addr x, addr y)
{
	long_float real, imag;

	Return(getmathcomplex2_long1_(x, &real, &imag));
	ptr->v.l.a = real;
	ptr->v.l.b = imag;
	Return(getmathcomplex2_long1_(y, &real, &imag));
	ptr->v.l.c = real;
	ptr->v.l.d = imag;

	return 0;
}

int getmathcomplex2_float_(struct mathcomplex2_struct *ptr,
		addr x, addr y, enum MathType *ret)
{
	enum MathType type;

	Return(getmathcomplex2_type_(x, y, &type));
	switch (type) {
		case MathType_single:
			Return(getmathcomplex2_single_(ptr, x, y));
			break;

		case MathType_double:
			Return(getmathcomplex2_double_(ptr, x, y));
			break;

		case MathType_long:
			Return(getmathcomplex2_long_(ptr, x, y));
			break;

		case MathType_rational:
			Return(getmathcomplex2_single_(ptr, x, y));
			type = MathType_single;
			break;

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return fmte_("type error", NULL);
	}
	ptr->type = type;

	return Result(ret, type);
}


/*
 *  getmathcomplex2_rational
 */
static void getmathcomplex2_rational(struct mathcomplex2_struct *ptr, addr x, addr y)
{
	ptr->v.a.x = x;
	ptr->v.a.y = y;
}

int getmathcomplex2_addr_(struct mathcomplex2_struct *ptr,
		addr x, addr y, enum MathType *ret)
{
	enum MathType type;

	Return(getmathcomplex2_type_(x, y, &type));
	switch (type) {
		case MathType_single:
			Return(getmathcomplex2_single_(ptr, x, y));
			break;

		case MathType_double:
			Return(getmathcomplex2_double_(ptr, x, y));
			break;

		case MathType_long:
			Return(getmathcomplex2_long_(ptr, x, y));
			break;

		case MathType_rational:
			getmathcomplex2_rational(ptr, x, y);
			break;

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return fmte_("type error", NULL);
	}
	ptr->type = type;

	return Result(ret, type);
}


/************************************************************
 *  md5encode.c
 ************************************************************/
/*
 *  md5encode.c
 *
 *  [RFC1321] The MD5 Message-Digest Algorithm
 *  https://www.ietf.org/rfc/rfc1321.txt
 */

static const uint32_t InitialWordA = 0x67452301;
static const uint32_t InitialWordB = 0xEFCDAB89;
static const uint32_t InitialWordC = 0x98BADCFE;
static const uint32_t InitialWordD = 0x10325476;
static const uint32_t CalcT[64 + 1] = {
	0x00000000,
	0xD76AA478, 0xE8C7B756, 0x242070DB, 0xC1BDCEEE,
	0xF57C0FAF, 0x4787C62A, 0xA8304613, 0xFD469501,
	0x698098D8, 0x8B44F7AF, 0xFFFF5BB1, 0x895CD7BE,
	0x6B901122, 0xFD987193, 0xA679438E, 0x49B40821,
	0xF61E2562, 0xC040B340, 0x265E5A51, 0xE9B6C7AA,
	0xD62F105D, 0x02441453, 0xD8A1E681, 0xE7D3FBC8,
	0x21E1CDE6, 0xC33707D6, 0xF4D50D87, 0x455A14ED,
	0xA9E3E905, 0xFCEFA3F8, 0x676F02D9, 0x8D2A4C8A,
	0xFFFA3942, 0x8771F681, 0x6D9D6122, 0xFDE5380C,
	0xA4BEEA44, 0x4BDECFA9, 0xF6BB4B60, 0xBEBFBC70,
	0x289B7EC6, 0xEAA127FA, 0xD4EF3085, 0x04881D05,
	0xD9D4D039, 0xE6DB99E5, 0x1FA27CF8, 0xC4AC5665,
	0xF4292244, 0x432AFF97, 0xAB9423A7, 0xFC93A039,
	0x655B59C3, 0x8F0CCC92, 0xFFEFF47D, 0x85845DD1,
	0x6FA87E4F, 0xFE2CE6E0, 0xA3014314, 0x4E0811A1,
	0xF7537E82, 0xBD3AF235, 0x2AD7D2BB, 0xEB86D391
};

#define CalcF(x,y,z) (((x) & (y)) | ((~(x)) & (z)))
#define CalcG(x,y,z) (((x) & (z)) | ((y) & (~(z))))
#define CalcH(x,y,z) ((x) ^ (y) ^ (z))
#define CalcI(x,y,z) ((y) ^ ((x) | (~(z))))
#define CalcR32(v,s) (((v) << (s)) | ((v) >> (32 - (s))))
#define Calc(op,a,b,c,d,k,s,i) { \
	a += Calc##op(b,c,d) + x[k] + CalcT[i]; \
	a = CalcR32(a, s); \
	a += b; \
}

void clear_md5encode(struct md5encode *ptr)
{
	ptr->a = InitialWordA;
	ptr->b = InitialWordB;
	ptr->c = InitialWordC;
	ptr->d = InitialWordD;
	ptr->size = 0;
	ptr->pos = 0;
}

static void calcblock(struct md5encode *ptr)
{
	uint32_t a, b, c, d, *x;

	a = ptr->a;
	b = ptr->b;
	c = ptr->c;
	d = ptr->d;
	x = ptr->x;

	/* Round 1. */
	Calc(F, a,b,c,d,  0,  7,  1);
	Calc(F, d,a,b,c,  1, 12,  2);
	Calc(F, c,d,a,b,  2, 17,  3);
	Calc(F, b,c,d,a,  3, 22,  4);
	Calc(F, a,b,c,d,  4,  7,  5);
	Calc(F, d,a,b,c,  5, 12,  6);
	Calc(F, c,d,a,b,  6, 17,  7);
	Calc(F, b,c,d,a,  7, 22,  8);
	Calc(F, a,b,c,d,  8,  7,  9);
	Calc(F, d,a,b,c,  9, 12, 10);
	Calc(F, c,d,a,b, 10, 17, 11);
	Calc(F, b,c,d,a, 11, 22, 12);
	Calc(F, a,b,c,d, 12,  7, 13);
	Calc(F, d,a,b,c, 13, 12, 14);
	Calc(F, c,d,a,b, 14, 17, 15);
	Calc(F, b,c,d,a, 15, 22, 16);

	/* Round 2. */
	Calc(G, a,b,c,d,  1,  5, 17);
	Calc(G, d,a,b,c,  6,  9, 18);
	Calc(G, c,d,a,b, 11, 14, 19);
	Calc(G, b,c,d,a,  0, 20, 20);
	Calc(G, a,b,c,d,  5,  5, 21);
	Calc(G, d,a,b,c, 10,  9, 22);
	Calc(G, c,d,a,b, 15, 14, 23);
	Calc(G, b,c,d,a,  4, 20, 24);
	Calc(G, a,b,c,d,  9,  5, 25);
	Calc(G, d,a,b,c, 14,  9, 26);
	Calc(G, c,d,a,b,  3, 14, 27);
	Calc(G, b,c,d,a,  8, 20, 28);
	Calc(G, a,b,c,d, 13,  5, 29);
	Calc(G, d,a,b,c,  2,  9, 30);
	Calc(G, c,d,a,b,  7, 14, 31);
	Calc(G, b,c,d,a, 12, 20, 32);

	/* Round 3. */
	Calc(H, a,b,c,d,  5,  4, 33);
	Calc(H, d,a,b,c,  8, 11, 34);
	Calc(H, c,d,a,b, 11, 16, 35);
	Calc(H, b,c,d,a, 14, 23, 36);
	Calc(H, a,b,c,d,  1,  4, 37);
	Calc(H, d,a,b,c,  4, 11, 38);
	Calc(H, c,d,a,b,  7, 16, 39);
	Calc(H, b,c,d,a, 10, 23, 40);
	Calc(H, a,b,c,d, 13,  4, 41);
	Calc(H, d,a,b,c,  0, 11, 42);
	Calc(H, c,d,a,b,  3, 16, 43);
	Calc(H, b,c,d,a,  6, 23, 44);
	Calc(H, a,b,c,d,  9,  4, 45);
	Calc(H, d,a,b,c, 12, 11, 46);
	Calc(H, c,d,a,b, 15, 16, 47);
	Calc(H, b,c,d,a,  2, 23, 48);

	/* Round 4. */
	Calc(I, a,b,c,d,  0,  6, 49);
	Calc(I, d,a,b,c,  7, 10, 50);
	Calc(I, c,d,a,b, 14, 15, 51);
	Calc(I, b,c,d,a,  5, 21, 52);
	Calc(I, a,b,c,d, 12,  6, 53);
	Calc(I, d,a,b,c,  3, 10, 54);
	Calc(I, c,d,a,b, 10, 15, 55);
	Calc(I, b,c,d,a,  1, 21, 56);
	Calc(I, a,b,c,d,  8,  6, 57);
	Calc(I, d,a,b,c, 15, 10, 58);
	Calc(I, c,d,a,b,  6, 15, 59);
	Calc(I, b,c,d,a, 13, 21, 60);
	Calc(I, a,b,c,d,  4,  6, 61);
	Calc(I, d,a,b,c, 11, 10, 62);
	Calc(I, c,d,a,b,  2, 15, 63);
	Calc(I, b,c,d,a,  9, 21, 64);

	ptr->a += a;
	ptr->b += b;
	ptr->c += c;
	ptr->d += d;
}

void read_md5encode(struct md5encode *ptr, const void *from, size_t len)
{
	int pos, j, k;
	size_t i;
	uint32_t *x;
	const uint8_t *byte;

	pos = ptr->pos;
	if (pos < 0) {
		fprintf(stderr, "md5encode is already finished.\n");
		abort();
	}
	x = ptr->x;
	j = pos / 4;
	k = pos % 4;
	byte = (const uint8_t *)from;
	for (i = 0;  i < len; i++) {
		if (64 <= pos) {
			calcblock(ptr);
			pos = 0;
			j = k = 0;
		}
		if (k == 0) x[j] = 0;
		x[j] |= byte[i] << (8 * k);
		k++;
		if (4 <= k) {
			j++; k = 0;
		}
		pos++;
	}
	ptr->size += len;
	ptr->pos = pos;
}

static void wordtobyte(uint32_t value, uint8_t *result)
{
	int i;

	for (i = 0; i < 4; i++) {
		result[i] = 0xFF & value;
		value >>= 8;
	}
}

static void calcfinal(struct md5encode *ptr)
{
	size_t size, len, pos, k;
	uint8_t padding[64 + 8];

	/* padding */
	size = ptr->size;
	len = 64 - ((size + 8) % 64);
	if (len != 0)
		padding[0] = 0x80;
	for (pos = 1; pos < len; pos++)
		padding[pos] = 0;
	padding[pos++] = 0xFF & (size << 3);
	size >>= (8 - 3);
	for (k = 1; k < 8; k++, pos++, size >>= 8)
		padding[pos] = 0xFF & size;

	/* read padding */
	read_md5encode(ptr, padding, pos);
	calcblock(ptr);
}

void calc_md5encode(struct md5encode *ptr, void *result)
{
	uint8_t *byte;

	if (0 <= ptr->pos) {
		calcfinal(ptr);
		ptr->pos = -1;
	}

	byte = (uint8_t *)result;
	wordtobyte(ptr->a, byte);
	wordtobyte(ptr->b, byte + 4);
	wordtobyte(ptr->c, byte + 8);
	wordtobyte(ptr->d, byte + 12);
}

void sequence_md5encode(const void *from, size_t len, void *result)
{
	struct md5encode md5;

	clear_md5encode(&md5);
	read_md5encode(&md5, from, len);
	calc_md5encode(&md5, result);
}

void string_md5encode(const char *from, void *result)
{
	sequence_md5encode(from, strlen(from), result);
}


/************************************************************
 *  memory.c
 ************************************************************/

/*
 *  type
 */
enum LISPTYPE gettype(addr pos)
{
	if (pos == Unbound)
		return LISPSYSTEM_UNBOUND;
	if (pos == Nil)
		return LISPTYPE_NIL;
	if (pos == T)
		return LISPTYPE_T;

	return GetType(pos);
}


/*
 *  size class
 */
static int size2_memory(addr pos)
{
	enum LISPSIZE x = GetStatusSize(pos);
	return x == LISPSIZE_ARRAY2 || x == LISPSIZE_BODY2 || x == LISPSIZE_SMALLSIZE;
}

size_t getobjectlength(addr pos)
{
	Check(pos == Unbound, "unbound error");
	Check(GetType(pos) == LISPSYSTEM_SPACE, "type space error");
	Check(GetType(pos) == LISPSYSTEM_SPACE1, "type space1 error");
	Check(GetType(pos) == LISPSYSTEM_RESERVED, "type reserved error");

	return size2_memory(pos)? *PtrValue2L(pos): *PtrValueL(pos);
}

size_t getmemorylength(addr pos)
{
	size_t size;

	Check(pos == Unbound, "unbound error");
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
			return getobjectlength(pos);
	}

	return size;
}

int valid_header(addr pos)
{
	return (unsigned)GetType(pos) < LISPSYSTEM_CHECK;
}


/*
 *  lenarray
 */
void lenarrayA2(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2, "type error");
	*ret = GetLenArrayA2(pos);
}
void lenarraySS(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	*ret = GetLenArraySS(pos);
}
void lenarrayA4(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4, "type error");
	*ret = GetLenArrayA4(pos);
}
void lenarrayAB(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	*ret = GetLenArrayAB(pos);
}
#ifdef LISP_ARCH_64BIT
void lenarrayA8(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "type error");
	*ret = (size_t)GetLenArrayA8(pos);
}
#endif
void lenarray(addr pos, size_t *ret)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			*ret = GetLenArrayA2(pos);
			break;

		case LISPSIZE_SMALLSIZE:
			*ret = GetLenArraySS(pos);
			break;

		case LISPSIZE_ARRAYBODY:
			*ret = GetLenArrayAB(pos);
			break;

		case LISPSIZE_ARRAY4:
			*ret = GetLenArrayA4(pos);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			*ret = (size_t)GetLenArrayA8(pos);
			break;
#endif

		default:
			Abort("type error");
			break;
	}
}

size_t lenarrayA2r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2, "type error");
	return GetLenArrayA2(pos);
}
size_t lenarraySSr(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	return GetLenArraySS(pos);
}
size_t lenarrayA4r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4, "type error");
	return GetLenArrayA4(pos);
}
size_t lenarrayABr(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	return GetLenArrayAB(pos);
}
#ifdef LISP_ARCH_64BIT
size_t lenarrayA8r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "type error");
	return (size_t)GetLenArrayA8(pos);
}
#endif
size_t lenarrayr(addr pos)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			return GetLenArrayA2(pos);

		case LISPSIZE_SMALLSIZE:
			return GetLenArraySS(pos);

		case LISPSIZE_ARRAYBODY:
			return GetLenArrayAB(pos);

		case LISPSIZE_ARRAY4:
			return GetLenArrayA4(pos);

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			return (size_t)GetLenArrayA8(pos);
#endif
		default:
			Abort("type error");
			break;
	}
	return 0;
}


/*
 *  lenbody
 */
void lenbodyB2(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY2, "type error");
	*ret = GetLenBodyB2(pos);
}
void lenbodySS(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	*ret = GetLenBodySS(pos);
}
void lenbodyB4(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY4, "type error");
	*ret = GetLenBodyB4(pos);
}
void lenbodyAB(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	*ret = GetLenBodyAB(pos);
}
#ifdef LISP_ARCH_64BIT
void lenbodyB8(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY8, "type error");
	*ret = GetLenBodyB8(pos);
}
#endif
void lenbody(addr pos, size_t *ret)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_BODY2:
			*ret = GetLenBodyB2(pos);
			break;

		case LISPSIZE_SMALLSIZE:
			*ret = GetLenBodySS(pos);
			break;

		case LISPSIZE_BODY4:
			*ret = GetLenBodyB4(pos);
			break;

		case LISPSIZE_ARRAYBODY:
			*ret = GetLenBodyAB(pos);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_BODY8:
			*ret = (size_t)GetLenBodyB8(pos);
			break;
#endif

		default:
			Abort("type error");
			break;
	}
}

size_t lenbodyB2r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY2, "type error");
	return GetLenBodyB2(pos);
}
size_t lenbodySSr(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	return GetLenBodySS(pos);
}
size_t lenbodyB4r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY4, "type error");
	return GetLenBodyB4(pos);
}
size_t lenbodyABr(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	return GetLenBodyAB(pos);
}
#ifdef LISP_ARCH_64BIT
size_t lenbodyB8r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY8, "type error");
	return GetLenBodyB8(pos);
}
#endif
size_t lenbodyr(addr pos)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_BODY2:
			return GetLenBodyB2(pos);

		case LISPSIZE_SMALLSIZE:
			return GetLenBodySS(pos);

		case LISPSIZE_BODY4:
			return GetLenBodyB4(pos);

		case LISPSIZE_ARRAYBODY:
			return GetLenBodyAB(pos);

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_BODY8:
			return (size_t)GetLenBodyB8(pos);
#endif

		default:
			Abort("type error");
			break;
	}

	return 0;
}


/*
 *  posbody
 */
void posbodySSa(addr pos, size_t array, addr *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	Check(array != GetLenArraySS(pos), "array length error");
	*ret = PtrBodySSa(pos, array);
}
void posbodyABa(addr pos, size_t array, addr *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	Check(array != GetLenArrayAB(pos), "array length error");
	*ret = PtrBodyABa(pos, array);
}
void posbodyB2(addr pos, addr *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY2, "type error");
	*ret = PtrBodyB2(pos);
}
void posbodySS(addr pos, addr *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	*ret = PtrBodySS(pos);
}
void posbodyB4(addr pos, addr *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY4, "type error");
	*ret = PtrBodyB4(pos);
}
void posbodyAB(addr pos, addr *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	*ret = PtrBodyAB(pos);
}
#ifdef LISP_ARCH_64BIT
void posbodyB8(addr pos, addr *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY8, "type error");
	*ret = PtrBodyB8(pos);
}
#endif
void posbody(addr pos, addr *ret)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_BODY2:
			PosBodyB2(pos, ret);
			break;

		case LISPSIZE_SMALLSIZE:
			PosBodySS(pos, ret);
			break;

		case LISPSIZE_BODY4:
			PosBodyB4(pos, ret);
			break;

		case LISPSIZE_ARRAYBODY:
			PosBodyAB(pos, ret);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_BODY8:
			PosBodyB8(pos, ret);
			break;
#endif

		default:
			Abort("size error");
			break;
	}
}

addr posbodySSar(addr pos, size_t array)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	Check(array != GetLenArraySS(pos), "array length error");
	return PtrBodySSa(pos, array);
}
addr posbodyABar(addr pos, size_t array)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	Check(array != GetLenArrayAB(pos), "array length error");
	return PtrBodyABa(pos, array);
}
addr posbodyB2r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY2, "type error");
	return PtrBodyB2(pos);
}
addr posbodySSr(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	return PtrBodySS(pos);
}
addr posbodyB4r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY4, "type error");
	return PtrBodyB4(pos);
}
addr posbodyABr(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	return PtrBodyAB(pos);
}
#ifdef LISP_ARCH_64BIT
addr posbodyB8r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY8, "type error");
	return PtrBodyB8(pos);
}
#endif
addr posbodyr(addr pos)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_BODY2:
			return PtrBodyB2(pos);

		case LISPSIZE_SMALLSIZE:
			return PtrBodySS(pos);

		case LISPSIZE_BODY4:
			return PtrBodyB4(pos);

		case LISPSIZE_ARRAYBODY:
			return PtrBodyAB(pos);

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_BODY8:
			return PtrBodyB8(pos);
#endif

		default:
			Abort("size error");
			break;
	}

	return NULL;
}


/*
 *  posbodylen
 */
void posbodylenSSa(addr pos, size_t array, addr *body, size_t *len)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	Check(array != GetLenArraySS(pos), "array length error");
	*body = PtrBodySSa(pos, array);
	*len = GetLenBodySS(pos);
}
void posbodylenABa(addr pos, size_t array, addr *body, size_t *len)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	Check(array != GetLenArrayAB(pos), "array length error");
	*body = PtrBodyABa(pos, array);
	*len = GetLenBodyAB(pos);
}
void posbodylenB2(addr pos, addr *body, size_t *len)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY2, "size error");
	*body = PtrBodyB2(pos);
	*len = GetLenBodyB2(pos);
}
void posbodylenSS(addr pos, addr *body, size_t *len)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "size error");
	*body = PtrBodySS(pos);
	*len = GetLenBodySS(pos);
}
void posbodylenB4(addr pos, addr *body, size_t *len)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY4, "size error");
	*body = PtrBodyB4(pos);
	*len = GetLenBodyB4(pos);
}
void posbodylenAB(addr pos, addr *body, size_t *len)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "size error");
	*body = PtrBodyAB(pos);
	*len = GetLenBodyAB(pos);
}
#ifdef LISP_ARCH_64BIT
void posbodylenB8(addr pos, addr *body, size_t *len)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY8, "size error");
	*body = PtrBodyB8(pos);
	*len = GetLenBodyB8(pos);
}
#endif
void posbodylen(addr pos, addr *body, size_t *len)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_BODY2:
			*body = PtrBodyB2(pos);
			*len = GetLenBodyB2(pos);
			break;

		case LISPSIZE_SMALLSIZE:
			*body = PtrBodySS(pos);
			*len = GetLenBodySS(pos);
			break;

		case LISPSIZE_BODY4:
			*body = PtrBodyB4(pos);
			*len = GetLenBodyB4(pos);
			break;

		case LISPSIZE_ARRAYBODY:
			*body = PtrBodyAB(pos);
			*len = GetLenBodyAB(pos);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_BODY8:
			*body = PtrBodyB8(pos);
			*len = (size_t)GetLenBodyB8(pos);
			break;
#endif

		default:
			Abort2("size error: %d", (int)GetStatusSize(pos));
			break;
	}
}


/*
 *  getarray
 */
void getarrayA2(addr pos, size_t index, addr *value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2, "size error");
	Check(GetLenArrayA2(pos) <= index, "length error");
	*value = PtrArrayA2(pos)[index];
}
void getarraySS(addr pos, size_t index, addr *value)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "size error");
	Check(GetLenArraySS(pos) <= index, "length error");
	*value = PtrArraySS(pos)[index];
}
void getarrayA4(addr pos, size_t index, addr *value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4, "size error");
	Check(GetLenArrayA4(pos) <= index, "length error");
	*value = PtrArrayA4(pos)[index];
}
void getarrayAB(addr pos, size_t index, addr *value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "size error");
	Check(GetLenArrayAB(pos) <= index, "length error");
	*value = PtrArrayAB(pos)[index];
}
#ifdef LISP_ARCH_64BIT
void getarrayA8(addr pos, size_t index, addr *value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "size error");
	Check(GetLenArrayA8(pos) <= index, "length error");
	*value = PtrArrayA8(pos)[index];
}
#endif
void getarray(addr pos, size_t index, addr *value)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			getarrayA2(pos, index, value);
			break;

		case LISPSIZE_SMALLSIZE:
			getarraySS(pos, index, value);
			break;

		case LISPSIZE_ARRAY4:
			getarrayA4(pos, index, value);
			break;

		case LISPSIZE_ARRAYBODY:
			getarrayAB(pos, index, value);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			getarrayA8(pos, index, value);
			break;
#endif

		default:
			Abort("type error");
			break;
	}
}

addr refarrayA2(addr pos, size_t index)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2, "size error");
	Check(GetLenArrayA2(pos) <= index, "length error");
	return PtrArrayA2(pos)[index];
}
addr refarraySS(addr pos, size_t index)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "size error");
	Check(GetLenArraySS(pos) <= index, "length error");
	return PtrArraySS(pos)[index];
}
addr refarrayA4(addr pos, size_t index)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4, "size error");
	Check(GetLenArrayA4(pos) <= index, "length error");
	return PtrArrayA4(pos)[index];
}
addr refarrayAB(addr pos, size_t index)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "size error");
	Check(GetLenArrayAB(pos) <= index, "length error");
	return PtrArrayAB(pos)[index];
}
#ifdef LISP_ARCH_64BIT
addr refarrayA8(addr pos, size_t index)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "size error");
	Check(GetLenArrayA8(pos) <= index, "length error");
	return PtrArrayA8(pos)[index];
}
#endif
addr refarray(addr pos, size_t index)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			return PtrArrayA2(pos)[index];

		case LISPSIZE_SMALLSIZE:
			return refarraySS(pos, index);

		case LISPSIZE_ARRAY4:
			return refarrayA4(pos, index);

		case LISPSIZE_ARRAYBODY:
			return refarrayAB(pos, index);

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			return refarrayA8(pos, index);
#endif

		default:
			Abort("type error");
			break;
	}

	return NULL;
}


/*
 *  setarray
 */
int checkdynamic(addr pos, addr value)
{
	int check;

	check = (value != Unbound) &&
		(value != NULL) &&
		(! GetStatusDynamic(pos)) &&
		GetStatusDynamic(value);
	if (check) {
		/* for breakpoint */
		Debug("checkdynamic error.");
		return 1;
	}
	return 0;
}

void setarray_chain(addr *ptr, addr value)
{
	byte *p;

	/* decrement */
	if (*ptr != Unbound) {
		p = (byte *)PtrChain(*ptr);
		Check(*p == 0, "Chain decrement error");
		if (*p != 0xFF)
			(*p)--;
	}

	/* increment */
	if (value != Unbound) {
		p = (byte *)PtrChain(value);
		if (*p != 0xFF)
			(*p)++;
	}

	/* setq */
	*ptr = value;
}

#if 0
#define SetArray_chain(array, pos, index, value) \
	setarray_chain(array(pos) + index, value)
#else
#define SetArray_chain(array, pos, index, value) \
	(array(pos)[index] = value)
#endif

void setarrayA2(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2, "size error");
	Check(GetStatusReadOnly(pos), "readonly error");
	Check(GetLenArrayA2(pos) <= index, "length error");
	CheckDynamic(pos, value);
	SetArray_chain(PtrArrayA2, pos, index, value);
}
void setarraySS(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "size error");
	Check(GetStatusReadOnly(pos), "readonly error");
	Check(GetLenArraySS(pos) <= index, "length error");
	CheckDynamic(pos, value);
	SetArray_chain(PtrArraySS, pos, index, value);
}
void setarrayA4(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4, "size error");
	Check(GetStatusReadOnly(pos), "readonly error");
	Check(GetLenArrayA4(pos) <= index, "length error");
	CheckDynamic(pos, value);
	SetArray_chain(PtrArrayA4, pos, index, value);
}
void setarrayAB(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "size error");
	Check(GetStatusReadOnly(pos), "readonly error");
	Check(GetLenArrayAB(pos) <= index, "length error");
	CheckDynamic(pos, value);
	SetArray_chain(PtrArrayAB, pos, index, value);
}
#ifdef LISP_ARCH_64BIT
void setarrayA8(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "size error");
	Check(GetStatusReadOnly(pos), "readonly error");
	Check(GetLenArrayA8(pos) <= index, "length error");
	CheckDynamic(pos, value);
	SetArray_chain(PtrArrayA8, pos, index, value);
}
#endif
void setarray(addr pos, size_t index, addr value)
{
	Check(GetStatusReadOnly(pos), "readonly error");
	Check(lenarrayr(pos) <= index, "length error");
	CheckDynamic(pos, value);

	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			SetArray_chain(PtrArrayA2, pos, index, value);
			break;

		case LISPSIZE_SMALLSIZE:
			SetArray_chain(PtrArraySS, pos, index, value);
			break;

		case LISPSIZE_ARRAY4:
			SetArray_chain(PtrArrayA4, pos, index, value);
			break;

		case LISPSIZE_ARRAYBODY:
			SetArray_chain(PtrArrayAB, pos, index, value);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			SetArray_chain(PtrArrayA8, pos, index, value);
			break;
#endif

		default:
			Abort("type error");
			break;
	}
}

void setarrayA2_force(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2, "size error");
	Check(GetLenArrayA2(pos) <= index, "length error");
	SetArray_chain(PtrArrayA2, pos, index, value);
}
void setarraySS_force(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "size error");
	Check(GetLenArraySS(pos) <= index, "length error");
	SetArray_chain(PtrArraySS, pos, index, value);
}
void setarrayA4_force(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4, "size error");
	Check(GetLenArrayA4(pos) <= index, "length error");
	SetArray_chain(PtrArrayA4, pos, index, value);
}
void setarrayAB_force(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "size error");
	Check(GetLenArrayAB(pos) <= index, "length error");
	SetArray_chain(PtrArrayAB, pos, index, value);
}
#ifdef LISP_ARCH_64BIT
void setarrayA8_force(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "size error");
	Check(GetLenArrayA8(pos) <= index, "length error");
	SetArray_chain(PtrArrayA8, pos, index, value);
}
#endif


/*
 *  object
 */
void nilarray2(addr pos, size_t size)
{
	size_t i;
	addr *array;

	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2 &&
			GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "size error");
	array = PtrArrayA2(pos);
	for (i = 0; i < size; i++)
		array[i] = Nil;
}
void nilarray4(addr pos, size_t size)
{
	size_t i;
	addr *array;

	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4 &&
			GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "size error");
	array = PtrArrayA4(pos);
	for (i = 0; i < size; i++)
		array[i] = Nil;
}
#ifdef LISP_ARCH_64BIT
void nilarray8(addr pos, size_t size)
{
	size_t i;
	addr *array;

	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "size error");
	array = PtrArrayA8(pos);
	for (i = 0; i < size; i++)
		array[i] = Nil;
}
#endif

void unboundarray2(addr pos, size_t size)
{
	size_t i;
	addr *array;

	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2 &&
			GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "size error");
	array = PtrArrayA2(pos);
	for (i = 0; i < size; i++)
		array[i] = Unbound;
}
void unboundarray4(addr pos, size_t size)
{
	size_t i;
	addr *array;

	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4 &&
			GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "size error");
	array = PtrArrayA4(pos);
	for (i = 0; i < size; i++)
		array[i] = Unbound;
}
#ifdef LISP_ARCH_64BIT
void unboundarray8(addr pos, size_t size)
{
	size_t i;
	addr *array;

	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "size error");
	array = PtrArrayA8(pos);
	for (i = 0; i < size; i++)
		array[i] = Unbound;
}
#endif

size_t size_split(size_t size)
{
	AlignSize8Front(size, &size);
	return size;
}


/************************************************************
 *  mop.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 7. Objects
 *    Common Lisp Object System - Metaobject Protocol
 */

int mop_export_symbol_(addr symbol)
{
	addr package;

	Check(! symbolp(symbol), "type error");
	GetConst(PACKAGE_CLOS, &package);
	return export_package_(package, symbol);
}


/*
 *  type
 */
static void mop_argument_generic_var(addr *ret, unsigned n)
{
	addr pos;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = n;
	*ret = pos;
}

void mop_argument_generic_var1(addr *ret)
{
	mop_argument_generic_var(ret, 1);
}
void mop_argument_generic_var2(addr *ret)
{
	mop_argument_generic_var(ret, 2);
}
void mop_argument_generic_var3(addr *ret)
{
	mop_argument_generic_var(ret, 3);
}
void mop_argument_generic_var4(addr *ret)
{
	mop_argument_generic_var(ret, 4);
}
void mop_argument_generic_var5(addr *ret)
{
	mop_argument_generic_var(ret, 5);
}

void mop_argument_generic_var1opt1(addr *ret)
{
	addr pos;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = 1;
	str->opt = 1;
	*ret = pos;
}

void mop_argument_generic_var3opt1(addr *ret)
{
	addr pos;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = 3;
	str->opt = 1;
	*ret = pos;
}

void mop_argument_generic_var1rest(addr *ret)
{
	addr pos;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = 1;
	str->rest = 1;
	*ret = pos;
}

void mop_argument_generic_var2rest(addr *ret)
{
	addr pos;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = 2;
	str->rest = 1;
	*ret = pos;
}

static void mop_argument_generic_varnrest1key0(addr *ret, unsigned var)
{
	addr pos;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = var;
	str->keyp = 1;
	str->rest = 1;
	str->allow = 1;
	*ret = pos;
}

void mop_argument_generic_var1rest1key0(addr *ret)
{
	mop_argument_generic_varnrest1key0(ret, 1);
}

void mop_argument_generic_var2rest1key0(addr *ret)
{
	mop_argument_generic_varnrest1key0(ret, 2);
}

void mop_argument_generic_var4rest1key0(addr *ret)
{
	mop_argument_generic_varnrest1key0(ret, 4);
}

void mop_argument_method_var(addr *ret, constindex index)
{
	addr pos;
	GetConstant(index, &pos);
	list_heap(ret, Nil, pos, NULL);
}

void mop_argument_method_var1(addr *ret, constindex var1)
{
	addr pos, list;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 1;
	mop_argument_method_var(&list, var1);
	list_heap(&list, list, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

void mop_argument_method_var1opt1(addr *ret, constindex var1, constindex opt1)
{
	addr pos, list;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 1;
	mop_argument_method_var(&list, var1);
	list_heap(&list, list, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* opt */
	str->opt = 1;
	mop_argument_method_var(&list, opt1);
	list_heap(&list, list, NULL);
	SetArgument(pos, ArgumentIndex_opt, list);
	/* result */
	*ret = pos;
}

void mop_argument_method_var1rest(addr *ret, constindex var1)
{
	addr pos;
	mop_argument_method_var1(&pos, var1);
	ArgumentStruct(pos)->rest = 1;
	*ret = pos;
}

void mop_argument_method_var2(addr *ret, constindex var1, constindex var2)
{
	addr pos, list, arg1, arg2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	mop_argument_method_var(&arg1, var1);
	mop_argument_method_var(&arg2, var2);
	list_heap(&list, arg1, arg2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

void mop_argument_method_var2rest(addr *ret, constindex var1, constindex var2)
{
	addr pos;
	mop_argument_method_var2(&pos, var1, var2);
	ArgumentStruct(pos)->rest = 1;
	*ret = pos;
}

void mop_argument_method_print_object(addr *ret, addr clos)
{
	addr pos, pos1, pos2;
	struct argument_struct *str;

	Check(! closp(clos), "type error");
	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	list_heap(&pos1, Nil, clos, NULL);
	ArgumentMethod_var(&pos2, T);
	list_heap(&pos1, pos1, pos2, NULL);
	SetArgument(pos, ArgumentIndex_var, pos1);
	/* result */
	*ret = pos;
}


/************************************************************
 *  mop_class.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 7. Objects
 *    Common Lisp Object System - Metaobject Protocol
 */

/***********************************************************************
 *  referenced-class
 ***********************************************************************/
/* (defun system::referenced-class (symbol) ...) -> class */
static int function_referenced_class(Execute ptr, addr symbol)
{
	addr pos;

	/* find-class */
	clos_find_class_nil(symbol, &pos);
	if (pos != Nil) {
		setresult_control(ptr, pos);
		return 0;
	}

	/* forward-referenced-class */
	GetConst(CLOS_FORWARD_REFERENCED_CLASS, &pos);
	Return(clos_instance_heap_(pos, &pos));
	Return(stdset_class_name_(pos, symbol));
	setresult_control(ptr, pos);

	return 0;
}

static void type_referenced_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Class);
	type_compiled_heap(args, values, ret);
}

static void defun_referenced_class_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_REFERENCED_CLASS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_referenced_class);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_referenced_class(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  ensure-class
 ***********************************************************************/
/* (defun system::ensure-class
 *     (name &rest args &key &allow-other-keys) ...)
 *     -> class
 *   name   symbol
 *   args   t
 *   class  class
 */
static int function_ensure_class(Execute ptr, addr name, addr rest)
{
	addr symbol, clos, check;

	/* class check */
	clos_find_class_nil(name, &clos);
	if (clos != Nil) {
		GetConst(COMMON_CLASS_NAME, &symbol);
		Return(funcall1_control_(ptr, &check, symbol, clos, NULL));
		if (check != name)
			clos = Nil;
	}

	/* call */
	GetConst(CLOSNAME_ENSURE_CLASS_USING_CLASS, &symbol);
	Return(getfunction_global_(symbol, &symbol));
	return applya_control_(ptr, symbol, clos, name, rest, NULL);
}

static void type_ensure_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, Class);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_class_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_ENSURE_CLASS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_ensure_class);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_ensure_class(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  ensure-class-using-class
 ***********************************************************************/
/* (defmethod ensure-class-using-class
 *     ((inst null) name
 *      &key metaclass direct-superclasses &allow-other-keys) ...)
 *     -> class
 *   inst                 null
 *   name                 symbol
 *   metaclass            class
 *   direct-superclasses  list
 *   class                class
 */
static int method_ensure_class_using_class_null(Execute ptr,
		addr method, addr next, addr clos, addr name, addr rest)
{
	Check(clos != Nil, "type error");
	Return(clos_ensure_class_(ptr, name, rest, &clos));
	setresult_control(ptr, clos);

	return 0;
}

static void method_type_ensure_class_using_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Symbol);
	typeargs_var2rest_allow(&args, args, values, args);
	typeargs_method(args);
	GetTypeValues(&values, Class);
	type_compiled_heap(args, values, ret);
}

static void argument_method_ensure_class_using_class(addr *ret, constindex type)
{
	addr pos, key, key1, key2;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	str->var = 2;
	str->keyp = 1;
	str->key = 2;
	str->allow = 1;
	/* var */
	GetConstant(type, &key1);
	list_heap(&key1, Nil, key1, NULL);
	GetConst(CLOS_T, &key2);
	list_heap(&key2, Nil, key2, NULL);
	list_heap(&key, key1, key2, NULL);
	SetArgument(pos, ArgumentIndex_var, key);
	/* key */
	GetConst(CLOSKEY_METACLASS, &key1);
	conscar_heap(&key1, key1);
	GetConst(CLOSKEY_DIRECT_SUPERCLASSES, &key2);
	conscar_heap(&key2, key2);
	list_heap(&key, key1, key2, NULL);
	SetArgument(pos, ArgumentIndex_key, key);
	/* result */
	*ret = pos;
}

static int defmethod_ensure_class_using_class_null_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_ensure_class_using_class_null);
	method_type_ensure_class_using_class(&type);
	settype_function(call, type);
	/* method */
	argument_method_ensure_class_using_class(&pos, CONSTANT_CLOS_NULL);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defmethod ensure-class-using-class
 *     ((inst standard-class) name
 *      &key metaclass direct-superclasses direct-slots &allow-other-keys) ...)
 *     -> class
 *   inst                 class
 *   name                 symbol
 *   metaclass            class
 *   direct-superclasses  list
 *   direct-slots         list
 *   class                class
 */
static int method_ensure_class_using_class_class(Execute ptr,
		addr method, addr next, addr clos, addr name, addr rest)
{
	CheckType(clos, LISPTYPE_CLOS);
	Return(clos_ensure_class_redefine_(ptr, clos, name, rest));
	setresult_control(ptr, clos);

	return 0;
}

static int defmethod_ensure_class_using_class_class_(Execute ptr,
		addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_ensure_class_using_class_class);
	method_type_ensure_class_using_class(&type);
	settype_function(call, type);
	/* method */
	argument_method_ensure_class_using_class(&pos, CONSTANT_CLOS_CLASS);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric ensure-class-using-class
 *     (class name &rest initargs &key &allow-other-keys) ...)
 *   -> class
 */
static int defgeneric_ensure_class_using_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_ENSURE_CLASS_USING_CLASS, &symbol);
	mop_argument_generic_var2rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_ensure_class_using_class_null_(ptr, name, gen));
	Return(defmethod_ensure_class_using_class_class_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  allocate-instance
 ***********************************************************************/
/* (defmethod allocate-instance
 *     ((class standard-class) &rest args &key)) -> instance
 */
static int method_allocate_instance_standard(Execute ptr,
		addr method, addr next, addr clos, addr rest)
{
	Return(allocate_instance_standard_(ptr, clos, &clos));
	setresult_control(ptr, clos);
	return 0;
}

static int method_allocate_instance_structure(Execute ptr,
		addr method, addr next, addr clos, addr rest)
{
	Return(allocate_instance_structure_(ptr, clos, &clos));
	setresult_control(ptr, clos);
	return 0;
}

static void method_type_allocate_instance(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1rest_allow(&args, args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void argument_method_allocate_instance(addr *ret, constindex type)
{
	addr pos, key;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	str->var = 1;
	str->keyp = 1;
	str->allow = 1;
	/* var */
	GetConstant(type, &key);
	list_heap(&key, Nil, key, NULL);
	list_heap(&key, key, NULL);
	SetArgument(pos, ArgumentIndex_var, key);
	/* result */
	*ret = pos;
}

static int defmethod_allocate_instance_(Execute ptr,
		addr name, addr gen, constindex index, pointer id)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3dynamic(call, id);
	method_type_allocate_instance(&type);
	settype_function(call, type);
	/* method */
	argument_method_allocate_instance(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric allocate-instance (class &rest args &key)) -> instance */
static int defgeneric_allocate_instance_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_ALLOCATE_INSTANCE, &symbol);
	mop_argument_generic_var1rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_allocate_instance_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS,
				p_method_allocate_instance_standard));
	Return(defmethod_allocate_instance_(ptr, name, gen,
				CONSTANT_CLOS_STRUCTURE_CLASS,
				p_method_allocate_instance_structure));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  initialize-instance
 ***********************************************************************/
/* (defmethod initialize-instance
 *     ((object standard-object) &rest args &key)) -> instance
 */
static int method_initialize_instance_stdobject(Execute ptr,
		addr method, addr next, addr pos, addr rest)
{
	Return(initialize_instance_stdobject_(ptr, pos, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int defmethod_initialize_instance_stdobject_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3dynamic(call, p_method_initialize_instance_stdobject);
	method_type_allocate_instance(&type);
	settype_function(call, type);
	/* method */
	argument_method_allocate_instance(&pos, CONSTANT_CLOS_STANDARD_OBJECT);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric initialize-instance (class &rest args &key)) -> instance */
static int defgeneric_initialize_instance_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_INITIALIZE_INSTANCE, &symbol);
	mop_argument_generic_var1rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_initialize_instance_stdobject_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  reinitialize-instance
 ***********************************************************************/
/* (defmethod reinitialize-instance
 *     ((object standard-object) &rest args &key)) -> instance
 */
static int method_reinitialize_instance_stdobject(Execute ptr,
		addr method, addr next, addr pos, addr rest)
{
	Return(reinitialize_instance_stdobject_(ptr, pos, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int defmethod_reinitialize_instance_stdobject_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3dynamic(call, p_method_reinitialize_instance_stdobject);
	method_type_allocate_instance(&type);
	settype_function(call, type);
	/* method */
	argument_method_allocate_instance(&pos, CONSTANT_CLOS_STANDARD_OBJECT);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric reinitialize-instance (class &rest args &key)) -> instance */
static int defgeneric_reinitialize_instance_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_REINITIALIZE_INSTANCE, &symbol);
	mop_argument_generic_var1rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_reinitialize_instance_stdobject_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  shared-initialize
 ***********************************************************************/
/* (defmethod shared-initialize
 *     ((object standard-object) name &rest args &key)) -> instance
 */
static int method_shared_initialize_stdobject(Execute ptr,
		addr method, addr next, addr pos, addr name, addr rest)
{
	Return(shared_initialize_stdobject_(ptr, pos, name, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void method_type_shared_initialize(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	GetTypeTable(&type, EqlT);
	type2or_heap(values, type, &type);
	typeargs_var2rest_allow(&args, args, type, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void argument_method_shared_initialize(addr *ret, constindex type)
{
	addr pos, key, key1, key2;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	str->var = 2;
	str->keyp = 1;
	str->allow = 1;
	/* var */
	GetConstant(type, &key1);
	list_heap(&key1, Nil, key1, NULL);
	GetConst(CLOS_T, &key2);
	list_heap(&key2, Nil, key2, NULL);
	list_heap(&key, key1, key2, NULL);
	SetArgument(pos, ArgumentIndex_var, key);
	/* result */
	*ret = pos;
}

static int defmethod_shared_initialize_stdobject_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_shared_initialize_stdobject);
	method_type_shared_initialize(&type);
	settype_function(call, type);
	/* method */
	argument_method_shared_initialize(&pos, CONSTANT_CLOS_STANDARD_OBJECT);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric shared-initialize (class &rest args &key)) -> instance */
static int defgeneric_shared_initialize_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_SHARED_INITIALIZE, &symbol);
	mop_argument_generic_var2rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_shared_initialize_stdobject_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  make-instance
 ***********************************************************************/
/* (defmethod make-instance
 *     ((class symbol) &rest initargs &key) ...)
 *   -> instance
 */
static int method_make_instance_symbol(Execute ptr,
		addr method, addr next, addr var, addr rest)
{
	addr symbol;

	Check(! symbolp(var), "type error");
	Return(clos_find_class_(var, &var));
	/* call generic-function */
	GetConst(COMMON_MAKE_INSTANCE, &symbol);
	Return(getfunction_global_(symbol, &symbol));
	return applya_control_(ptr, symbol, var, rest, NULL);
}

static void type_make_instance_symbol(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static int defmethod_make_instance_symbol_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3dynamic(call, p_method_make_instance_symbol);
	type_make_instance_symbol(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1rest(&pos, SYMBOL);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defmethod make-instance
 *     ((class standard-class) &rest initargs &key) ...)
 *   -> instance
 */
static int method_make_instance_stdclass(Execute ptr,
		addr method, addr next, addr rest)
{
	Return(make_instance_stdclass_(ptr, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void method_type_make_instance_stdclass(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StandardClass);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static int defmethod_make_instance_stdclass_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var2dynamic(call, p_method_make_instance_stdclass);
	method_type_make_instance_stdclass(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1rest(&pos, STANDARD_CLASS);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defmethod make-instance
 *     ((class structure-class) &rest initargs &key) ...)
 *   -> instance
 */
static int method_make_instance_structure(Execute ptr,
		addr method, addr next, addr rest)
{
	Return(make_instance_structure_(ptr, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void method_type_make_instance_structure(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StructureClass);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static int defmethod_make_instance_structure_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var2dynamic(call, p_method_make_instance_structure);
	method_type_make_instance_structure(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1rest(&pos, STRUCTURE_CLASS);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric make-instance
 *      (class &rest initargs &key allow-other-keys) ...)
 *    -> instance
 */
static int defgeneric_make_instance_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_MAKE_INSTANCE, &symbol);
	mop_argument_generic_var1rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_make_instance_symbol_(ptr, name, gen));
	Return(defmethod_make_instance_stdclass_(ptr, name, gen));
	Return(defmethod_make_instance_structure_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  make-instances-obsolete
 ***********************************************************************/
/* (defmethod make-instances-obsolete ((var symbol)) ...) */
static int method_make_instances_obsolete_symbol(Execute ptr,
		addr method, addr next, addr var)
{
	addr call;

	GetConst(COMMON_MAKE_INSTANCES_OBSOLETE, &call);
	Return(getfunction_global_(call, &call));
	Return(clos_find_class_(var, &var));
	return funcall_control_(ptr, call, var, NULL);
}

static void method_type_make_instances_obsolete_symbol(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, Class);
	type_compiled_heap(args, values, ret);
}

static int defmethod_make_instances_obsolete_symbol_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_make_instances_obsolete_symbol);
	method_type_make_instances_obsolete_symbol(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1(&pos, SYMBOL);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defmethod make-instances-obsolete ((var standard-class)) ...) */
static int method_make_instances_obsolete_stdclass(Execute ptr,
		addr method, addr next, addr var)
{
	setresult_control(ptr, var);
	return 0;
}

static void method_type_make_instances_obsolete_stdclass(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StandardClass);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, Class);
	type_compiled_heap(args, values, ret);
}

static int defmethod_make_instances_obsolete_stdclass_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_make_instances_obsolete_stdclass);
	method_type_make_instances_obsolete_stdclass(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1(&pos, STANDARD_CLASS);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_make_instances_obsolete_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_MAKE_INSTANCES_OBSOLETE, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_make_instances_obsolete_symbol_(ptr, name, gen));
	Return(defmethod_make_instances_obsolete_stdclass_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  make-load-form
 ***********************************************************************/
static int method_make_load_form_class(Execute ptr,
		addr method, addr next, addr var, addr env)
{
	addr call, find;

	/* (class-name var) */
	GetConst(COMMON_CLASS_NAME, &call);
	Return(getfunction_global_(call, &call));
	Return(funcall1_control_(ptr, &var, call, var, NULL));
	/* (find-class (quote var)) */
	GetConst(COMMON_FIND_CLASS, &find);
	quotelist_heap(&var, var);
	list_heap(&var, find, var, NULL);
	setresult_control(ptr, var);

	return 0;
}

static void method_type_make_load_form_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Environment);
	typeargs_var1opt1(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static int defmethod_make_load_form_class_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3opt1(call, p_method_make_load_form_class);
	method_type_make_load_form_class(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1opt1(&pos, CLASS, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defmethod_make_load_form_condition_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3opt1(call, p_method_make_load_form_class);
	method_type_make_load_form_class(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1opt1(&pos, CONDITION, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int method_make_load_form_object(Execute ptr,
		addr method, addr next, addr var, addr env)
{
	return fmte_("There is no function to make form ~S.", var, NULL);
}

static int defmethod_make_load_form_standard_object_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3opt1(call, p_method_make_load_form_object);
	method_type_make_load_form_class(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1opt1(&pos, STANDARD_OBJECT, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defmethod_make_load_form_structure_object_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3opt1(call, p_method_make_load_form_object);
	method_type_make_load_form_class(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1opt1(&pos, STRUCTURE_OBJECT, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_make_load_form_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_MAKE_LOAD_FORM, &symbol);
	mop_argument_generic_var1opt1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_make_load_form_class_(ptr, name, gen));
	Return(defmethod_make_load_form_condition_(ptr, name, gen));
	Return(defmethod_make_load_form_standard_object_(ptr, name, gen));
	Return(defmethod_make_load_form_structure_object_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-missing
 ***********************************************************************/
static int method_slot_missing(Execute ptr, addr method, addr next, addr rest)
{
	addr c, obj, name, op, value;
	Return(lista_bind_(rest, &c, &obj, &name, &op, &value, NULL));
	return fmte_("The class ~S has no slot ~S name ~S operation.", c, name, op, NULL);
}

static void method_type_slot_missing(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Symbol);
	typeargs_var4opt1(&args, args, args, values, values, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_slot_missing(addr *ret)
{
	addr pos, list, type;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 4;
	str->opt = 1;
	ArgumentMethod_var(&type, T);
	list_heap(&list, type, type, type, type, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_slot_missing_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var2dynamic(call, p_method_slot_missing);
	method_type_slot_missing(&type);
	settype_function(call, type);
	/* method */
	method_argument_slot_missing(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static void mop_argument_generic_var4opt1(addr *ret)
{
	addr pos;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = 4;
	str->opt = 1;
	*ret = pos;
}

static int defgeneric_slot_missing_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_SLOT_MISSING, &symbol);
	mop_argument_generic_var4opt1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_slot_missing_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-unbound
 ***********************************************************************/
static int method_slot_unbound(Execute ptr, addr method, addr next, addr rest)
{
	addr clos, obj, name;
	Return(list_bind_(rest, &clos, &obj, &name, NULL));
	return call_unbound_slot_(ptr, obj, name);
}

static void method_type_slot_unbound(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Symbol);
	typeargs_var3(&args, args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_slot_unbound(addr *ret)
{
	addr pos, list, type;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 3;
	ArgumentMethod_var(&type, T);
	list_heap(&list, type, type, type, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_slot_unbound_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var2dynamic(call, p_method_slot_unbound);
	method_type_slot_unbound(&type);
	settype_function(call, type);
	/* method */
	method_argument_slot_unbound(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_slot_unbound_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_SLOT_UNBOUND, &symbol);
	mop_argument_generic_var3(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_slot_unbound_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  update-instance-for-different-class
 ***********************************************************************/
static int method_update_instance_for_different_class(
		Execute ptr, addr method, addr next,
		addr previous, addr current, addr rest)
{
	Return(clos_change_method_(ptr, previous, current, rest));
	setresult_control(ptr, Nil);
	return 0;
}

static void method_type_update_instance_for_different_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var2rest_allow(&args, args, args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_update_instance_for_different_class(addr *ret)
{
	mop_argument_method_var2rest(ret,
			CONSTANT_CLOS_STANDARD_OBJECT,
			CONSTANT_CLOS_STANDARD_OBJECT);
}

static int defmethod_update_instance_for_different_class_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_update_instance_for_different_class);
	method_type_update_instance_for_different_class(&type);
	settype_function(call, type);
	/* method */
	method_argument_update_instance_for_different_class(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_update_instance_for_different_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_UPDATE_INSTANCE_FOR_DIFFERENT_CLASS, &symbol);
	mop_argument_generic_var2rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_update_instance_for_different_class_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  update-instance-for-redefined-class
 ***********************************************************************/
static int method_update_instance_for_redefined_class(
		Execute ptr, addr method, addr next, addr rest)
{
	addr pos, add, del, prop, args;

	Return(lista_bind_(rest, &pos, &add, &del, &prop, &args, NULL));
	Return(clos_redefine_method_(ptr, pos, add, del, prop, args));
	setresult_control(ptr, Nil);

	return 0;
}

static void method_type_update_instance_for_redefined_class(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&type, List);
	typeargs_var4rest_allow(&args, args, type, type, type, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_update_instance_for_redefined_class(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 4;
	ArgumentMethod_var(&type1, STANDARD_OBJECT);
	ArgumentMethod_var(&type2, T);
	list_heap(&list, type1, type2, type2, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* rest */
	str->rest = 1;
	/* result */
	*ret = pos;
}

static int defmethod_update_instance_for_redefined_class_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var2dynamic(call, p_method_update_instance_for_redefined_class);
	method_type_update_instance_for_redefined_class(&type);
	settype_function(call, type);
	/* method */
	method_argument_update_instance_for_redefined_class(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_update_instance_for_redefined_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_UPDATE_INSTANCE_FOR_REDEFINED_CLASS, &symbol);
	mop_argument_generic_var4rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_update_instance_for_redefined_class_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-boundp-using-class
 ***********************************************************************/
static int method_slot_boundp_using_class_standard(Execute ptr,
		addr method, addr next, addr clos, addr pos, addr name)
{
	int check;

	Return(clos_version_check_(ptr, pos, clos));
	Return(slot_boundp_using_class_common_(ptr, clos, pos, name, &check));
	setbool_control(ptr, check);

	return 0;
}

static int method_slot_boundp_using_class_structure(Execute ptr,
		addr method, addr next, addr clos, addr pos, addr name)
{
	int check;

	Return(slot_boundp_using_class_common_(ptr, clos, pos, name, &check));
	setbool_control(ptr, check);

	return 0;
}

static void method_argument_slot_boundp_using_class(addr *ret, constindex index)
{
	addr pos, list, type1, type2, type3;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 3;
	mop_argument_method_var(&type1, index);
	ArgumentMethod_var(&type2, T);
	ArgumentMethod_var(&type3, SYMBOL);
	list_heap(&list, type1, type2, type3, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_slot_boundp_using_class_(Execute ptr,
		addr name, addr gen, constindex index, pointer id)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var5(call, id);
	GetTypeCompiled(&type, SlotBoundp_Method);
	settype_function(call, type);
	/* method */
	method_argument_slot_boundp_using_class(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_slot_boundp_using_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_BOUNDP_USING_CLASS, &symbol);
	mop_argument_generic_var3(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_slot_boundp_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS,
				p_method_slot_boundp_using_class_standard));
	Return(defmethod_slot_boundp_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STRUCTURE_CLASS,
				p_method_slot_boundp_using_class_structure));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-exists-p-using-class
 ***********************************************************************/
static int method_slot_exists_p_using_class_standard(Execute ptr,
		addr method, addr next, addr clos, addr pos, addr name)
{
	Return(clos_version_check_(ptr, pos, clos));
	setbool_control(ptr, clos_slot_exists_p(pos, name));
	return 0;
}

static int method_slot_exists_p_using_class_structure(Execute ptr,
		addr method, addr next, addr clos, addr pos, addr name)
{
	setbool_control(ptr, clos_slot_exists_p(pos, name));
	return 0;
}

static int defmethod_slot_exists_p_using_class_(Execute ptr,
		addr name, addr gen, constindex index, pointer id)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var5(call, id);
	GetTypeCompiled(&type, SlotBoundp_Method);
	settype_function(call, type);
	/* method */
	method_argument_slot_boundp_using_class(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_slot_exists_p_using_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_EXISTS_P_USING_CLASS, &symbol);
	mop_argument_generic_var3(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_slot_exists_p_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS,
				p_method_slot_exists_p_using_class_standard));
	Return(defmethod_slot_exists_p_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STRUCTURE_CLASS,
				p_method_slot_exists_p_using_class_structure));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-makunbound-using-class
 ***********************************************************************/
static int method_slot_makunbound_using_class_standard(Execute ptr,
		addr method, addr next, addr clos, addr pos, addr name)
{
	Return(clos_version_check_(ptr, pos, clos));
	Return(slot_makunbound_using_class_(ptr, clos, pos, name));
	setresult_control(ptr, pos);
	return 0;
}

static int method_slot_makunbound_using_class_structure(Execute ptr,
		addr method, addr next, addr clos, addr pos, addr name)
{
	Return(slot_makunbound_using_class_(ptr, clos, pos, name));
	setresult_control(ptr, pos);
	return 0;
}

static int defmethod_slot_makunbound_using_class_(Execute ptr,
		addr name, addr gen, constindex index, pointer id)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var5(call, id);
	GetTypeCompiled(&type, SlotBoundp_Method);
	settype_function(call, type);
	/* method */
	method_argument_slot_boundp_using_class(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_slot_makunbound_using_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_MAKUNBOUND_USING_CLASS, &symbol);
	mop_argument_generic_var3(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_slot_makunbound_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS,
				p_method_slot_makunbound_using_class_standard));
	Return(defmethod_slot_makunbound_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STRUCTURE_CLASS,
				p_method_slot_makunbound_using_class_structure));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-value-using-class
 ***********************************************************************/
static int method_slot_value_using_class_standard(Execute ptr,
		addr method, addr next, addr clos, addr pos, addr name)
{
	Return(clos_version_check_(ptr, pos, clos));
	Return(slot_value_using_class_common_(ptr, clos, pos, name, &pos));
	setresult_control(ptr, pos);

	return 0;
}

static int method_slot_value_using_class_structure(Execute ptr,
		addr method, addr next, addr clos, addr pos, addr name)
{
	Return(slot_value_using_class_common_(ptr, clos, pos, name, &pos));
	setresult_control(ptr, pos);

	return 0;
}

static int defmethod_slot_value_using_class_(Execute ptr,
		addr name, addr gen, constindex index, pointer id)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var5(call, id);
	GetTypeCompiled(&type, SlotBoundp_Method);
	settype_function(call, type);
	/* method */
	method_argument_slot_boundp_using_class(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_slot_value_using_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_VALUE_USING_CLASS, &symbol);
	mop_argument_generic_var3(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_slot_value_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS,
				p_method_slot_value_using_class_standard));
	Return(defmethod_slot_value_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STRUCTURE_CLASS,
				p_method_slot_value_using_class_structure));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  (setf slot-value-using-class)
 ***********************************************************************/
static int method_setf_slot_value_using_class_standard(Execute ptr,
		addr method, addr next, addr rest)
{
	addr value, clos, pos, name;

	Return(list_bind_(rest, &value, &clos, &pos, &name, NULL));
	Return(clos_version_check_(ptr, pos, clos));
	Return(setf_slot_value_using_class_common_(ptr, clos, pos, name, value));
	setresult_control(ptr, value);

	return 0;
}

static int method_setf_slot_value_using_class_structure(Execute ptr,
		addr method, addr next, addr rest)
{
	addr value, clos, pos, name;

	Return(list_bind_(rest, &value, &clos, &pos, &name, NULL));
	Return(setf_slot_value_using_class_common_(ptr, clos, pos, name, value));
	setresult_control(ptr, value);

	return 0;
}

static void method_type_setf_slot_value_using_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Symbol);
	typeargs_var4(&args, args, args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_setf_slot_value_using_class(addr *ret, constindex index)
{
	addr pos, list, type1, type2, type3;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 4;
	mop_argument_method_var(&type1, index);
	ArgumentMethod_var(&type2, T);
	ArgumentMethod_var(&type3, SYMBOL);
	list_heap(&list, type2, type1, type2, type3, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_setf_slot_value_using_class_(Execute ptr,
		addr name, addr gen, constindex index, pointer id)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var2dynamic(call, id);
	method_type_setf_slot_value_using_class(&type);
	settype_function(call, type);
	/* method */
	method_argument_setf_slot_value_using_class(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_setf_slot_value_using_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_VALUE_USING_CLASS, &symbol);
	mop_argument_generic_var4(&gen);
	setf_callname_heap(&name, symbol);
	Return(generic_make_(&gen, name, gen));
	setsetf_symbol(symbol, gen);
	/* method */
	Return(defmethod_setf_slot_value_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS,
				p_method_setf_slot_value_using_class_standard));
	Return(defmethod_setf_slot_value_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STRUCTURE_CLASS,
				p_method_setf_slot_value_using_class_structure));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  change-class
 ***********************************************************************/
static int method_change_class_stdclass(Execute ptr,
		addr method, addr next, addr pos, addr clos, addr rest)
{
	Return(clos_change_class_(ptr, pos, clos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void method_type_change_class_stdclass(addr *ret)
{
	addr args, values, rest;

	GetTypeTable(&args, StandardObject);
	GetTypeTable(&values, StandardClass);
	GetTypeTable(&rest, T);
	typeargs_var2rest_allow(&args, args, values, rest);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_change_class_stdclass(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	str->rest = 1;
	str->keyp = 1;
	ArgumentMethod_var(&type1, STANDARD_OBJECT);
	ArgumentMethod_var(&type2, STANDARD_CLASS);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_change_class_stdclass_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_change_class_stdclass);
	method_type_change_class_stdclass(&type);
	settype_function(call, type);
	/* method */
	method_argument_change_class_stdclass(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int method_change_class_symbol(Execute ptr,
		addr method, addr next, addr pos, addr clos, addr rest)
{
	addr call;

	GetConst(COMMON_CHANGE_CLASS, &call);
	Return(getfunction_global_(call, &call));
	Return(clos_find_class_(clos, &clos));
	return applya_control_(ptr, call, pos, clos, rest, NULL);
}

static void method_type_change_class_symbol(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Symbol);
	typeargs_var2rest_allow(&args, args, values, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_change_class_symbol(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	str->rest = 1;
	str->keyp = 1;
	ArgumentMethod_var(&type1, T);
	ArgumentMethod_var(&type2, SYMBOL);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_change_class_symbol_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_change_class_symbol);
	method_type_change_class_symbol(&type);
	settype_function(call, type);
	/* method */
	method_argument_change_class_symbol(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_change_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_CHANGE_CLASS, &symbol);
	mop_argument_generic_var2rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_change_class_stdclass_(ptr, name, gen));
	Return(defmethod_change_class_symbol_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  function
 ***********************************************************************/
void init_mop_class(void)
{
	SetPointerCall(defun, var1, referenced_class);
	SetPointerCall(defun, var1dynamic, ensure_class);
	SetPointerType(var4dynamic, method_ensure_class_using_class_null);
	SetPointerType(var4dynamic, method_ensure_class_using_class_class);
	SetPointerType(var3dynamic, method_allocate_instance_standard);
	SetPointerType(var3dynamic, method_allocate_instance_structure);
	SetPointerType(var3dynamic, method_initialize_instance_stdobject);
	SetPointerType(var3dynamic, method_reinitialize_instance_stdobject);
	SetPointerType(var4dynamic, method_shared_initialize_stdobject);
	SetPointerType(var3dynamic, method_make_instance_symbol);
	SetPointerType(var2dynamic, method_make_instance_stdclass);
	SetPointerType(var2dynamic, method_make_instance_structure);
	SetPointerType(var3, method_make_instances_obsolete_symbol);
	SetPointerType(var3, method_make_instances_obsolete_stdclass);
	SetPointerType(var3opt1, method_make_load_form_class);
	SetPointerType(var3opt1, method_make_load_form_object);
	SetPointerType(var2dynamic, method_slot_missing);
	SetPointerType(var2dynamic, method_slot_unbound);
	SetPointerType(var4dynamic, method_update_instance_for_different_class);
	SetPointerType(var2dynamic, method_update_instance_for_redefined_class);
	SetPointerType(var5, method_slot_boundp_using_class_standard);
	SetPointerType(var5, method_slot_boundp_using_class_structure);
	SetPointerType(var5, method_slot_exists_p_using_class_standard);
	SetPointerType(var5, method_slot_exists_p_using_class_structure);
	SetPointerType(var5, method_slot_makunbound_using_class_standard);
	SetPointerType(var5, method_slot_makunbound_using_class_structure);
	SetPointerType(var5, method_slot_value_using_class_standard);
	SetPointerType(var5, method_slot_value_using_class_structure);
	SetPointerType(var2dynamic, method_setf_slot_value_using_class_standard);
	SetPointerType(var2dynamic, method_setf_slot_value_using_class_structure);
	SetPointerType(var4dynamic, method_change_class_stdclass);
	SetPointerType(var4dynamic, method_change_class_symbol);
}

int build_mop_class_(Execute ptr)
{
	defun_referenced_class_mop();
	defun_ensure_class_mop();
	Return(defgeneric_ensure_class_using_class_mop_(ptr));
	Return(defgeneric_allocate_instance_mop_(ptr));
	Return(defgeneric_initialize_instance_mop_(ptr));
	Return(defgeneric_reinitialize_instance_mop_(ptr));
	Return(defgeneric_shared_initialize_mop_(ptr));
	Return(defgeneric_make_instance_mop_(ptr));
	Return(defgeneric_make_instances_obsolete_mop_(ptr));
	Return(defgeneric_make_load_form_(ptr));
	Return(defgeneric_slot_missing_mop_(ptr));
	Return(defgeneric_slot_unbound_mop_(ptr));
	Return(defgeneric_update_instance_for_different_class_mop_(ptr));
	Return(defgeneric_update_instance_for_redefined_class_mop_(ptr));
	Return(defgeneric_slot_boundp_using_class_mop_(ptr));
	Return(defgeneric_slot_exists_p_using_class_mop_(ptr));
	Return(defgeneric_slot_makunbound_using_class_mop_(ptr));
	Return(defgeneric_slot_value_using_class_mop_(ptr));
	Return(defgeneric_setf_slot_value_using_class_mop_(ptr));
	Return(defgeneric_change_class_mop_(ptr));

	return 0;
}


/************************************************************
 *  mop_common.c
 ************************************************************/

void init_metaobject_protocol(void)
{
	init_mop_class();
	init_mop_reader();
	init_mop_generic();
	init_mop_protocols();
}

void build_metaobject_protocol(void)
{
	Execute ptr;

	ptr = Execute_Thread;
	Error(build_mop_class_(ptr));
	Error(build_mop_reader_(ptr));
	Error(build_mop_generic_(ptr));
	Error(build_mop_protocols_(ptr));
}


/************************************************************
 *  mop_generic.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 7. Objects
 *    Common Lisp Object System - Metaobject Protocol
 */

/***********************************************************************
 *  no-applicable-method
 ***********************************************************************/
static int method_no_applicable_method(Execute ptr,
		addr method, addr next, addr gen, addr rest)
{
	return fmte_("There is no applicable methods in ~S.", gen, NULL);
}

static void method_type_no_applicable_method(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1rest(&args, args, args);
	typeargs_method(args);
	GetTypeTable(&values, Asterisk);
	type_compiled_heap(args, values, ret);
}

static void method_argument_no_applicable_method(addr *ret)
{
	addr pos, list, type1;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 1;
	str->rest = 1;
	ArgumentMethod_var(&type1, T);
	list_heap(&list, type1, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_no_applicable_method_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3rest(call, p_method_no_applicable_method);
	method_type_no_applicable_method(&type);
	settype_function(call, type);
	/* method */
	method_argument_no_applicable_method(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

int defgeneric_no_applicable_method_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_NO_APPLICABLE_METHOD, &symbol);
	mop_argument_generic_var1rest(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_no_applicable_method_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  no-next-method
 ***********************************************************************/
static int method_no_next_method(Execute ptr,
		addr method, addr next, addr gen, addr no_next, addr rest)
{
	return fmte_("There is no methods after ~S method in ~S.", no_next, gen, NULL);
}

static void method_type_no_next_method(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var2rest(&args, args, args, args);
	typeargs_method(args);
	GetTypeTable(&values, Asterisk);
	type_compiled_heap(args, values, ret);
}

static void method_argument_no_next_method(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	str->rest = 1;
	ArgumentMethod_var(&type1, STANDARD_GENERIC_FUNCTION);
	ArgumentMethod_var(&type2, STANDARD_METHOD);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_no_next_method_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4rest(call, p_method_no_next_method);
	method_type_no_next_method(&type);
	settype_function(call, type);
	/* method */
	method_argument_no_next_method(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

int defgeneric_no_next_method_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_NO_NEXT_METHOD, &symbol);
	mop_argument_generic_var2rest(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_no_next_method_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  ensure-generic-function-using-class
 ***********************************************************************/
static int method_ensure_generic_function_class(Execute ptr,
		addr method, addr next, addr clos, addr name, addr rest)
{
	Return(mop_generic_change_(ptr, clos, name, rest));
	setresult_control(ptr, clos);
	return 0;
}

static void method_type_ensure_generic_function_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, T);
	typeargs_var2rest(&args, args, values, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_ensure_generic_function_class(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	str->rest = 1;
	str->keyp = 1;
	ArgumentMethod_var(&type1, GENERIC_FUNCTION);
	ArgumentMethod_var(&type2, T);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_ensure_generic_function_class_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_ensure_generic_function_class);
	method_type_ensure_generic_function_class(&type);
	settype_function(call, type);
	/* method */
	method_argument_ensure_generic_function_class(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int method_ensure_generic_function_null(Execute ptr,
		addr method, addr next, addr clos, addr name, addr rest)
{
	Check(clos != Nil, "error");
	Return(mop_generic_new_(ptr, name, rest, &name));
	setresult_control(ptr, name);
	return 0;
}

static void method_type_ensure_generic_function_null(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Null);
	GetTypeTable(&values, T);
	typeargs_var2rest(&args, args, values, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_ensure_generic_function_null(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	str->rest = 1;
	str->keyp = 1;
	ArgumentMethod_var(&type1, NULL);
	ArgumentMethod_var(&type2, T);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_ensure_generic_function_null_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_ensure_generic_function_null);
	method_type_ensure_generic_function_null(&type);
	settype_function(call, type);
	/* method */
	method_argument_ensure_generic_function_null(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_ensure_generic_function_using_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_ENSURE_GENERIC_FUNCTION_USING_CLASS, &symbol);
	mop_argument_generic_var2rest(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_ensure_generic_function_class_(ptr, name, gen));
	Return(defmethod_ensure_generic_function_null_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  find-method-combination
 ***********************************************************************/
static int method_find_method_combination(Execute ptr,
		addr method, addr next, addr gen, addr symbol, addr list)
{
	Return(mop_find_method_combination_(symbol, list, &gen));
	setresult_control(ptr, gen);
	return 0;
}

static void method_type_find_method_combination(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, GenericFunction);
	GetTypeTable(&values, Symbol);
	GetTypeTable(&type, List);
	typeargs_var3(&args, args, values, type);
	typeargs_method(args);
	GetTypeValues(&values, MethodCombination);
	type_compiled_heap(args, values, ret);
}

static void method_argument_find_method_combination(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 3;
	ArgumentMethod_var(&type1, STANDARD_GENERIC_FUNCTION);
	ArgumentMethod_var(&type2, T);
	list_heap(&list, type1, type2, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_find_method_combination_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var5(call, p_method_find_method_combination);
	method_type_find_method_combination(&type);
	settype_function(call, type);
	/* method */
	method_argument_find_method_combination(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int find_method_combination_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_FIND_METHOD_COMBINATION, &symbol);
	mop_argument_generic_var3(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_find_method_combination_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  ensure-method
 ***********************************************************************/
/* `(defun ensure-method (name
 *      &key lambda-list qualifiers specializers function) ...)
 *      -> method
 *    name           function-name
 *    lambda-list    list
 *    qualifiers     list
 *    specializers   list
 *    function       function
 */
static int function_ensure_method(Execute ptr, addr name, addr rest)
{
	addr lambda, qua, spec, call;

	/* arguments */
	if (GetKeyArgs(rest, CLOSKEY_LAMBDA_LIST, &lambda))
		lambda = Nil;
	if (GetKeyArgs(rest, CLOSKEY_QUALIFIERS, &qua))
		qua = Nil;
	if (GetKeyArgs(rest, CLOSKEY_SPECIALIZERS, &spec))
		spec = Nil;
	if (GetKeyArgs(rest, CLOSKEY_FUNCTION, &call))
		return fmte_("Invalid ensure-method argument :function ~S.", call, NULL);

	/* add method */
	Return(ensure_method_common_(ptr, &name, name, lambda, qua, spec, call));
	setresult_control(ptr, name);

	return 0;
}

static void type_ensure_method(addr *ret)
{
	addr args, values, key, key1, key2, key3, key4;

	/* key */
	keytypetable(CONSTANT_CLOSKEY_LAMBDA_LIST, TypeTable_T, &key1);
	keytypetable(CONSTANT_CLOSKEY_QUALIFIERS, TypeTable_List, &key2);
	keytypetable(CONSTANT_CLOSKEY_SPECIALIZERS, TypeTable_List, &key3);
	keytypetable(CONSTANT_CLOSKEY_FUNCTION, TypeTable_Function, &key4);
	list_heap(&key, key1, key2, key3, key4, NULL);
	/* type */
	GetTypeTable(&args, FunctionName);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_method_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_ENSURE_METHOD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_function_ensure_method);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_ensure_method(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  function-keywords
 ***********************************************************************/
static int method_function_keywords(Execute ptr, addr method, addr next, addr var)
{
	int allow;

	Return(stdget_method_lambda_list_(var, &var));
	Return(argument_method_keywords_heap_(var, &var, &allow));
	setvalues_control(ptr, var, (allow? T: Nil), NULL);

	return 0;
}

static void method_type_function_keywords(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Method);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeTable(&values, List);
	GetTypeTable(&type, Boolean);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, ret);
}

static int defmethod_function_keywords_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_function_keywords);
	method_type_function_keywords(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_METHOD);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_function_keywords_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_FUNCTION_KEYWORDS, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_function_keywords_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  flet-method-p
 ***********************************************************************/
/* (defun clos::flet-method-p (var) ...) -> boolean */
static int function_flet_method_p(Execute ptr, addr var)
{
	setbool_control(ptr, var != Nil);
	return 0;
}

static void defun_flet_method_p_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_FLET_METHOD_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_flet_method_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  flet-next-method
 ***********************************************************************/
/* (defun clos::flet-next-method (method next args rest) ...) -> t */
static int call_no_next_method_(Execute ptr, addr gen, addr method, addr args)
{
	addr call;

	GetConst(COMMON_NO_NEXT_METHOD, &call);
	Return(getfunction_global_(call, &call));
	return applya_control_(ptr, call, gen, method, args, NULL);
}

static int function_flet_next_method(Execute ptr,
		addr method, addr next, addr args, addr rest)
{
	addr call, gen;
	LocalRoot local;

	if (rest == Nil)
		rest = args;
	if (next == Nil) {
		Return(stdget_method_generic_function_(method, &gen));
		return call_no_next_method_(ptr, gen, method, rest);
	}
	Return_getcons(next, &method, &next);
	Return(stdget_method_function_(method, &call));
	/* call method */
	local = ptr->local;
	lista_local(local, &rest, method, next, rest, NULL);
	return apply_control_(ptr, call, rest);
}

static void type_flet_next_method(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var4(&args, args, values, values, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_flet_next_method_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_FLET_NEXT_METHOD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var4(pos, p_defun_flet_next_method);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_flet_next_method(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  method-combination-instance
 ***********************************************************************/
static int function_method_combination_instance(Execute ptr, addr var)
{
	clos_find_combination_nil(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_method_combination_instance(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void define_method_combination_instance_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_METHOD_COMBINATION_INSTANCE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_method_combination_instance);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_method_combination_instance(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  define-method-combination-short
 ***********************************************************************/
static int function_ensure_method_combination_short(Execute ptr, addr var, addr rest)
{
	addr doc, ident, oper;

	if (GetKeyArgs(rest, KEYWORD_DOCUMENTATION, &doc))
		doc = Nil;
	if (GetKeyArgs(rest, KEYWORD_IDENTITY_WITH_ONE_ARGUMENT, &ident))
		ident = Nil;
	if (GetKeyArgs(rest, KEYWORD_OPERATOR, &oper))
		oper = var;
	Return(ensure_define_combination_short_common_(var, doc, ident, oper));
	setresult_control(ptr, var);

	return 0;
}

static void type_ensure_method_combination_short(addr *ret)
{
	addr args, values, key, key1, key2, key3;

	/* key */
	KeyTypeTable(&key1, DOCUMENTATION, String);
	KeyTypeTable(&key2, IDENTITY_WITH_ONE_ARGUMENT, T);
	KeyTypeTable(&key3, OPERATOR, Symbol);
	list_heap(&key, key1, key2, key3, NULL);
	/* type */
	GetTypeTable(&args, Symbol);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_define_combination_short_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_ENSURE_METHOD_COMBINATION_SHORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_ensure_method_combination_short);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_ensure_method_combination_short(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  define-method-combination-long
 ***********************************************************************/
static int function_ensure_method_combination_long(Execute ptr,
		addr name, addr lambda, addr spec, addr rest)
{
	addr args, gen, doc, form, decl;

	if (GetKeyArgs(rest, KEYWORD_ARGUMENTS, &args))
		args = Nil;
	if (GetKeyArgs(rest, KEYWORD_GENERIC_FUNCTION, &gen))
		gen = Nil;
	if (GetKeyArgs(rest, KEYWORD_DOCUMENTATION, &doc))
		doc = Nil;
	if (GetKeyArgs(rest, CLOSKEY_FORM, &form))
		form = Nil;
	if (GetKeyArgs(rest, CLOSKEY_DECLARE, &decl))
		decl = Nil;
	Return(ensure_define_combination_long_common_(name,
				lambda, spec, args, gen, doc, form, decl));
	setresult_control(ptr, name);

	return 0;
}

static void type_ensure_method_combination_long(addr *ret)
{
	addr args, values, key, key1, key2, key3, key4;

	/* key */
	KeyTypeTable(&key1, ARGUMENTS, T);
	KeyTypeTable(&key2, GENERIC_FUNCTION, T);
	KeyTypeTable(&key3, DOCUMENTATION, String);
	keytypetable(CONSTANT_CLOSKEY_FORM, TypeTable_T, &key4);
	list_heap(&key, key1, key2, key3, key4, NULL);
	/* type */
	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var3key(&args, args, values, values, key);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_define_combination_long_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_ENSURE_METHOD_COMBINATION_LONG, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_ensure_method_combination_long);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_ensure_method_combination_long(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  qualifiers-elt
 ***********************************************************************/
/* (defun qualifiers-elt (symbol vector index order required) ...) -> result
 *   symbol   symbol
 *   vector   vector
 *   index    index
 *   order    keyword
 *   require  t
 *   result   list
 */
static int qualifiers_elt_order_(addr symbol, addr order, int *ret)
{
	addr value;

	GetConst(KEYWORD_MOST_SPECIFIC_FIRST, &value);
	if (value == order)
		return Result(ret, 0);
	GetConst(KEYWORD_MOST_SPECIFIC_LAST, &value);
	if (value == order)
		return Result(ret, 1);
	/* error */
	*ret = 0;
	return fmte_("Invalid :order ~S in the qualifiers ~S.", order, symbol, NULL);
}

static int function_qualifiers_elt(Execute ptr,
		addr symbol, addr pos, addr index, addr order, addr req)
{
	int check;
	size_t size;

	Return(getindex_integer_(index, &size));
	getarray(pos, size, &pos);
	if (req != Nil && pos == Nil)
		return fmte_("The qualifier ~S must be at least one method.", symbol, NULL);
	Return(qualifiers_elt_order_(symbol, order, &check));
	if (check) {
		Return(reverse_list_heap_safe_(&pos, pos));
	}
	setresult_control(ptr, pos);

	return 0;
}

static void type_qualifiers_elt(addr *ret)
{
	addr args, values, type1, type2, type3;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, Vector);
	GetTypeTable(&type1, Index);
	GetTypeTable(&type2, Keyword);
	GetTypeTable(&type3, T);
	typeargs_var5(&args, args, values, type1, type2, type3);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_qualifiers_elt_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_QUALIFIERS_ELT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var5(pos, p_defun_qualifiers_elt);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_qualifiers_elt(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  combination-binding
 ***********************************************************************/
static int function_combination_binding(Execute ptr, addr var)
{
	Return(stdget_longcomb_binding_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_combination_binding(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_combination_binding_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_COMBINATION_BINDING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_combination_binding);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_combination_binding(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  macro-make-method
 ***********************************************************************/
static int function_macro_make_method(Execute ptr, addr gen, addr form)
{
	/* `(macro-method-lambda
	 *    ,gen
	 *    (lambda (#:method #:next &rest #:args)
	 *      (declare (ignore #:method #:next #:args))
	 *      ,form))
	 */
	addr make, lambda, method, next, args, rest, declare, ignore;

	GetConst(CLOSNAME_MACRO_METHOD_LAMBDA, &make);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORE, &ignore);
	GetConst(AMPERSAND_REST, &rest);
	make_symbolchar(&method, "METHOD");
	make_symbolchar(&next, "NEXT");
	make_symbolchar(&args, "ARGS");
	list_heap(&ignore, ignore, method, next, args, NULL);
	list_heap(&declare, declare, ignore, NULL);
	list_heap(&method, method, next, rest, args, NULL);
	list_heap(&lambda, lambda, method, declare, form, NULL);
	list_heap(&form, make, gen, lambda, NULL);
	setresult_control(ptr, form);

	return 0;
}

static void type_macro_make_method(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, T);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_macro_make_method(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_MACRO_MAKE_METHOD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_macro_make_method);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_macro_make_method(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  macro-call-method
 ***********************************************************************/
static int function_macro_call_method(Execute ptr, addr car, addr cdr, addr symbol)
{
	/* `(let ((#:method ,car))
	 *    (apply (method-function ,method) ,method (list ,@cdr) ,symbol))
	 */
	addr let, apply, methodf, list, method;
	addr args, root, pos;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_APPLY, &apply);
	GetConst(CLOSNAME_METHOD_FUNCTION, &methodf);
	GetConst(COMMON_LIST, &list);
	make_symbolchar(&method, "METHOD");
	/* args */
	list_heap(&args, method, car, NULL);
	list_heap(&args, args, NULL);
	/* list */
	conscar_heap(&root, list);
	while (cdr != Nil) {
		Return_getcons(cdr, &pos, &cdr);
		cons_heap(&root, pos, root);
	}
	nreverse(&root, root);
	/* apply */
	list_heap(&methodf, methodf, method, NULL);
	list_heap(&apply, apply, methodf, method, root, symbol, NULL);
	list_heap(&let, let, args, apply, NULL);
	/* result */
	setresult_control(ptr, let);

	return 0;
}

static void type_macro_call_method(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	GetTypeTable(&type, Symbol);
	typeargs_var3(&args, args, values, type);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_macro_call_method(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_MACRO_CALL_METHOD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_macro_call_method);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_macro_call_method(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  macro-method-lambda
 ***********************************************************************/
static int function_macro_method_lambda(Execute ptr, addr gen, addr call)
{
	addr make, clos;

	/* make-instance */
	Return(stdget_generic_method_class_(gen, &clos));
	GetConst(COMMON_MAKE_INSTANCE, &make);
	Return(getfunction_global_(make, &make));
	Return(funcall_control_(ptr, make, clos, NULL));
	getresult_control(ptr, &clos);
	Return(stdset_method_function_(clos, call));
	setresult_control(ptr, clos);

	return 0;
}

static void type_macro_method_lambda(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, T);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_macro_method_lambda(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_MACRO_METHOD_LAMBDA, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_macro_method_lambda);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_macro_method_lambda(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  compute-applicable-methods
 ***********************************************************************/
static int method_compute_applicable_methods_std(Execute ptr,
		addr method, addr next, addr clos, addr args)
{
	Return(generic_compute_applicable_methods_(ptr->local, clos, args, &args));
	setresult_control(ptr, args);
	return 0;
}

static void method_type_compute_applicable_methods_std(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, GenericFunction);
	GetTypeTable(&values, List);
	typeargs_var2(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void method_argument_compute_applicable_methods_std(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	ArgumentMethod_var(&type1, STANDARD_GENERIC_FUNCTION);
	ArgumentMethod_var(&type2, T);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_compute_applicable_methods_std_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4(call, p_method_compute_applicable_methods_std);
	method_type_compute_applicable_methods_std(&type);
	settype_function(call, type);
	/* method */
	method_argument_compute_applicable_methods_std(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_compute_applicable_methods_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_COMPUTE_APPLICABLE_METHODS, &symbol);
	mop_argument_generic_var2(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	Return(defmethod_compute_applicable_methods_std_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  find-method
 ***********************************************************************/
static int method_find_method_std(Execute ptr,
		addr method, addr next, addr clos, addr qua, addr spec, addr errorp)
{
	if (errorp == Unbound)
		errorp = T;
	Return(generic_find_method_(ptr, clos, qua, spec, errorp, &qua));
	setresult_control(ptr, qua);

	return 0;
}

static void method_type_find_method_std(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, GenericFunction);
	GetTypeTable(&values, List);
	GetTypeTable(&type, Boolean);
	typeargs_var3opt1(&args, args, values, values, type);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_find_method_std(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 3;
	str->opt = 1;
	ArgumentMethod_var(&type1, STANDARD_GENERIC_FUNCTION);
	ArgumentMethod_var(&type2, T);
	list_heap(&list, type1, type2, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_find_method_std_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var5opt1(call, p_method_find_method_std);
	method_type_find_method_std(&type);
	settype_function(call, type);
	/* method */
	method_argument_find_method_std(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_find_method_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_FIND_METHOD, &symbol);
	mop_argument_generic_var3opt1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	Return(defmethod_find_method_std_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  add-method
 ***********************************************************************/
static int method_add_method_std(Execute ptr,
		addr method, addr next, addr gen, addr met)
{
	Return(method_add_method_(ptr, gen, met));
	setresult_control(ptr, gen);
	return 0;
}

static void method_type_add_method_std(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, GenericFunction);
	GetTypeTable(&values, Method);
	typeargs_var2(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, GenericFunction);
	type_compiled_heap(args, values, ret);
}

static void method_argument_add_method_std(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	ArgumentMethod_var(&type1, STANDARD_GENERIC_FUNCTION);
	ArgumentMethod_var(&type2, METHOD);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_add_method_std_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4(call, p_method_add_method_std);
	method_type_add_method_std(&type);
	settype_function(call, type);
	/* method */
	method_argument_add_method_std(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_add_method_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_ADD_METHOD, &symbol);
	mop_argument_generic_var2(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	Return(defmethod_add_method_std_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  intern
 ***********************************************************************/
static int method_remove_method_std(Execute ptr,
		addr method, addr next, addr gen, addr met)
{
	Return(method_remove_method_(ptr, gen, met));
	setresult_control(ptr, gen);
	return 0;
}

static void method_argument_remove_method_std(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	ArgumentMethod_var(&type1, STANDARD_GENERIC_FUNCTION);
	ArgumentMethod_var(&type2, T);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_remove_method_std_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4(call, p_method_remove_method_std);
	method_type_add_method_std(&type);
	settype_function(call, type);
	/* method */
	method_argument_remove_method_std(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_remove_method_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_REMOVE_METHOD, &symbol);
	mop_argument_generic_var2(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	Return(defmethod_remove_method_std_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  function
 ***********************************************************************/
void init_mop_generic(void)
{
	SetPointerType(var3rest, method_no_applicable_method);
	SetPointerType(var4rest, method_no_next_method);
	SetPointerType(var4dynamic, method_ensure_generic_function_class);
	SetPointerType(var4dynamic, method_ensure_generic_function_null);
	SetPointerType(var5, method_find_method_combination);
	SetPointerType(var1dynamic, function_ensure_method);
	SetPointerType(var3, method_function_keywords);
	SetPointerCall(defun, var1, flet_method_p);
	SetPointerCall(defun, var4, flet_next_method);
	SetPointerCall(defun, var1, method_combination_instance);
	SetPointerCall(defun, var1dynamic, ensure_method_combination_short);
	SetPointerCall(defun, var3dynamic, ensure_method_combination_long);
	SetPointerCall(defun, var5, qualifiers_elt);
	SetPointerCall(defun, var1, combination_binding);
	SetPointerCall(defun, var2, macro_make_method);
	SetPointerCall(defun, var3, macro_call_method);
	SetPointerCall(defun, var2, macro_method_lambda);
	SetPointerType(var4, method_compute_applicable_methods_std);
	SetPointerType(var5opt1, method_find_method_std);
	SetPointerType(var4, method_add_method_std);
	SetPointerType(var4, method_remove_method_std);
}

int build_mop_generic_(Execute ptr)
{
	/* defclass */
	Return(defgeneric_no_applicable_method_mop_(ptr));
	Return(defgeneric_no_next_method_mop_(ptr));
	/* defgeneric */
	Return(defgeneric_ensure_generic_function_using_class_mop_(ptr));
	Return(find_method_combination_mop_(ptr));
	defun_ensure_method_mop();
	Return(defgeneric_function_keywords_mop_(ptr));
	/* defmethod */
	defun_flet_method_p_mop();
	defun_flet_next_method_mop();
	/* define-method-combination */
	define_method_combination_instance_mop();
	defun_ensure_define_combination_short_mop();
	defun_ensure_define_combination_long_mop();
	defun_qualifiers_elt_mop();
	defun_combination_binding_mop();
	defun_macro_make_method();
	defun_macro_call_method();
	defun_macro_method_lambda();
	/* common */
	Return(defgeneric_compute_applicable_methods_mop_(ptr));
	Return(defgeneric_find_method_mop_(ptr));
	Return(defgeneric_add_method_mop_(ptr));
	Return(defgeneric_remove_method_mop_(ptr));

	return 0;
}


/************************************************************
 *  mop_protocols.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 7. Objects
 *    Common Lisp Object System - Metaobject Protocol
 */

/***********************************************************************
 *  make-method-lambda
 ***********************************************************************/
static int method_make_method_lambda_std(Execute ptr,
		addr method, addr next, addr gen, addr mclass, addr list, addr env)
{
	method_make_method_lambda(list, env, &list);
	setresult_control(ptr, list);
	return 0;
}

static void method_type_make_method_lambda_std(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	GetTypeTable(&type, EnvironmentNull);
	typeargs_var4(&args, args, args, values, type);
	typeargs_method(args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void method_argument_make_method_lambda_std(addr *ret)
{
	addr pos, list, type1, type2, type3;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 4;
	ArgumentMethod_var(&type1, STANDARD_GENERIC_FUNCTION);
	ArgumentMethod_var(&type2, STANDARD_METHOD);
	ArgumentMethod_var(&type3, T);
	list_heap(&list, type1, type2, type3, type3, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_make_method_lambda_std_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var6(call, p_method_make_method_lambda_std);
	method_type_make_method_lambda_std(&type);
	settype_function(call, type);
	/* method */
	method_argument_make_method_lambda_std(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_make_method_lambda_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_MAKE_METHOD_LAMBDA, &symbol);
	mop_argument_generic_var4(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* no-method */
	Return(defmethod_make_method_lambda_std_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  function
 ***********************************************************************/
void init_mop_protocols(void)
{
	SetPointerType(var6, method_make_method_lambda_std);
}

int build_mop_protocols_(Execute ptr)
{
	Return(defgeneric_make_method_lambda_(ptr));

	return 0;
}


/************************************************************
 *  mop_reader.c
 ************************************************************/

/***********************************************************************
 *  class-name
 ***********************************************************************/
/* (defmethod class-name (class) ...) -> symbol */
static int method_class_name(Execute ptr, addr method, addr next, addr var)
{
	Return(stdget_class_name_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void method_type_class_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Class);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static int defmethod_class_name_(Execute ptr, addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_class_name);
	method_type_class_name(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric class-name (class)) -> symbol */
static int defgeneric_class_name_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_CLASS_NAME, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_name_(ptr, name, gen,
				CONSTANT_CLOS_STRUCTURE_CLASS));
	Return(defmethod_class_name_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_name_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_name_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_name_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  (setf class-name)
 ***********************************************************************/
/* (defmethod (setf class-name) (t class) ...) -> t */
static int method_setf_class_name(Execute ptr,
		addr method, addr next, addr symbol, addr pos)
{
	Return(stdset_class_name_(pos, symbol));
	setresult_control(ptr, symbol);
	return 0;
}

static void method_type_setf_class_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var2(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void method_argument_setf_class_name(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	ArgumentMethod_var(&type1, SYMBOL);
	ArgumentMethod_var(&type2, STANDARD_CLASS);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_setf_class_name_class_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4(call, p_method_setf_class_name);
	method_type_setf_class_name(&type);
	settype_function(call, type);
	/* method */
	method_argument_setf_class_name(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric (setf class-name) (t class)) -> t */
static int defgeneric_setf_class_name_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_CLASS_NAME, &symbol);
	mop_argument_generic_var2(&gen);
	setf_callname_heap(&name, symbol);
	Return(generic_make_(&gen, name, gen));
	setsetf_symbol(symbol, gen);
	/* method */
	Return(defmethod_setf_class_name_class_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  class-slots
 ***********************************************************************/
static int make_slot_definition_call(Execute ptr)
{
	addr value;

	getdata_control(ptr, &value);
	setresult_control(ptr, value);

	return 0;
}

static void make_slot_definition_function(addr value, addr *ret)
{
	addr pos;

	compiled_system(&pos, Nil);
	setcompiled_empty(pos, p_make_slot_definition_call);
	SetDataFunction(pos, value);
	*ret = pos;
}

static int make_slot_definition_(addr slot, addr *ret)
{
	addr clos, key, value, check;

	GetConst(CLOS_STANDARD_SLOT_DEFINITION, &clos);
	Return(clos_instance_heap_(clos, &clos));
	/* slot-definition-name */
	GetNameSlot(slot, &value);
	GetConst(CLOSNAME_NAME, &key);
	Return(clos_set_(clos, key, value));
	/* slot-definition-type */
	GetTypeSlot(slot, &value);
	if (GetType(value) == LISPTYPE_TYPE) {
		Return(type_object_(&value, value));
	}
	GetConst(CLOSNAME_TYPE, &key);
	Return(clos_set_(clos, key, value));
	/* slot-definition-allocation */
	if (slot_instance_p(slot))
		GetConst(KEYWORD_INSTANCE, &value);
	else
		GetConst(KEYWORD_CLASS, &value);
	GetConst(CLOSNAME_ALLOCATION, &key);
	Return(clos_set_(clos, key, value));
	/* slot-definition-initargs */
	GetArgsSlot(slot, &value);
	GetConst(CLOSNAME_INITARGS, &key);
	Return(clos_set_(clos, key, value));
	/* slot-definition-initform */
	GetFormSlot(slot, &value);
	if (value != Unbound) {
		GetConst(CLOSNAME_INITFORM, &key);
		Return(clos_set_(clos, key, value));
		/* slot-definition-initfunction */
		GetFunctionSlot(slot, &check);
		if (check == Nil)
			make_slot_definition_function(value, &value);
		else
			value = check;
		GetConst(CLOSNAME_INITFUNCTION, &key);
		Return(clos_set_(clos, key, value));
	}
	/* result */
	return Result(ret, clos);
}

static int list_from_slot_vector_(addr pos, addr *ret)
{
	addr root, slot;
	size_t size, i;

	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	LenSlotVector(pos, &size);
	root = Nil;
	for (i = 0; i < size; i++) {
		GetSlotVector(pos, i, &slot);
		Return(make_slot_definition_(slot, &slot));
		cons_heap(&root, slot, root);
	}
	nreverse(ret, root);

	return 0;
}

/* (defmethod class-slots (class) ...) -> t */
static int method_class_slots(Execute ptr, addr method, addr next, addr var)
{
	Return(stdget_class_slots_(var, &var));
	Return(list_from_slot_vector_(var, &var));
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_class_slots_(Execute ptr, addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_class_slots);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric class-slots (class)) -> t */
int defgeneric_class_slots_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_SLOTS, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_slots_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_slots_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_slots_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_slots_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  class-direct-slots
 ***********************************************************************/
/* (defmethod class-direct-slots (class) ...) -> t */
static int method_class_direct_slots(Execute ptr, addr method, addr next, addr var)
{
	Return(stdget_class_direct_slots_(var, &var));
	Return(list_from_slot_vector_(var, &var));
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_class_direct_slots_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_class_direct_slots);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric class-direct-slots (class)) -> t */
int defgeneric_class_direct_slots_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_DIRECT_SLOTS, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_direct_slots_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_direct_slots_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_direct_slots_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_direct_slots_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  class-default-initargs
 ***********************************************************************/
/* (defmethod class-default-initargs (class) ...) -> t */
static int method_class_default_initargs(Execute ptr, addr method, addr next, addr var)
{
	Return(stdget_class_default_initargs_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_class_default_initargs_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_class_default_initargs);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric class-default-initargs (class)) -> t */
int defgeneric_class_default_initargs_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_DEFAULT_INITARGS, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  class-direct-default-initargs
 ***********************************************************************/
/* (defmethod class-direct-default-initargs (class) ...) -> t */
static int method_class_direct_default_initargs(Execute ptr,
		addr method, addr next, addr var)
{
	Return(stdget_class_direct_default_initargs_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_class_direct_default_initargs_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_class_direct_default_initargs);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric class-direct-default-initargs (class)) -> t */
int defgeneric_class_direct_default_initargs_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_DIRECT_DEFAULT_INITARGS, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_direct_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_direct_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_direct_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_direct_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  class-precedence-list
 ***********************************************************************/
/* (defmethod class-precedence-list (class) ...) -> t */
static int method_class_precedence_list(Execute ptr,
		addr method, addr next, addr var)
{
	Return(stdget_class_precedence_list_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_class_precedence_list_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_class_precedence_list);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric class-precedence-list (class)) -> t */
int defgeneric_class_precedence_list_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_PRECEDENCE_LIST, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_precedence_list_(ptr, name, gen,
				CONSTANT_CLOS_STRUCTURE_CLASS));
	Return(defmethod_class_precedence_list_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_precedence_list_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_precedence_list_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_precedence_list_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  class-direct-superclasses
 ***********************************************************************/
/* (defmethod class-direct-superclasses (class) ...) -> t */
static int method_class_direct_superclasses(Execute ptr,
		addr method, addr next, addr var)
{
	Return(stdget_class_direct_superclasses_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_class_direct_superclasses_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_class_direct_superclasses);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric class-direct-superclasses (class)) -> t */
int defgeneric_class_direct_superclasses_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_DIRECT_SUPERCLASSES, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_direct_superclasses_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_direct_superclasses_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_direct_superclasses_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_direct_superclasses_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  class-direct-subclasses
 ***********************************************************************/
/* (defmethod class-direct-subclasses (class) ...) -> t */
static int method_class_direct_subclasses(Execute ptr,
		addr method, addr next, addr var)
{
	Return(stdget_class_direct_subclasses_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_class_direct_subclasses_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_class_direct_subclasses);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric class-direct-subclasses (class)) -> t */
int defgeneric_class_direct_subclasses_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_DIRECT_SUBCLASSES, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_direct_subclasses_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_direct_subclasses_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_direct_subclasses_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_direct_subclasses_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  class-finalized-p
 ***********************************************************************/
/* (defmethod class-finalized-p (class) ...) -> t */
static int method_class_finalized_p(Execute ptr,
		addr method, addr next, addr var)
{
	Return(stdget_class_finalized_p_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_class_finalized_p_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_class_finalized_p);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric class-finalized-p (class)) -> t */
int defgeneric_class_finalized_p_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_FINALIZED_P, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_finalized_p_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_finalized_p_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_finalized_p_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_finalized_p_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  class-prototype
 ***********************************************************************/
/* (defmethod class-prototype (class) ...) -> t */
static int method_class_prototype(Execute ptr,
		addr method, addr next, addr var)
{
	Return(stdget_class_prototype_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_class_prototype_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_class_prototype);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric class-prototype (class)) -> t */
int defgeneric_class_prototype_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_PROTOTYPE, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_prototype_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_prototype_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_prototype_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_prototype_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-definition-name
 ***********************************************************************/
/* (defmethod slot-definition-name
 *     ((inst standard-slot-definition)) ...) -> symbol
 */
static int method_slot_definition_name(Execute ptr, addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSNAME_NAME, &key);
	Return(clos_check_(var, key, &var));
	setresult_control(ptr, var);

	return 0;
}

static void method_type_slot_definition_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static int defmethod_slot_definition_name_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_slot_definition_name);
	method_type_slot_definition_name(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric slot-definition-name (class)) -> symbol */
int defgeneric_slot_definition_name_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_NAME, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_slot_definition_name_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-definition-type
 ***********************************************************************/
/* (defmethod slot-definition-type
 *     ((inst standard-slot-definition)) ...) -> type
 */
static int method_slot_definition_type(Execute ptr, addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSNAME_TYPE, &key);
	Return(clos_check_(var, key, &var));
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_slot_definition_type_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_slot_definition_type);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric slot-definition-type (class)) -> type */
int defgeneric_slot_definition_type_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_TYPE, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_slot_definition_type_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-definition-allocation
 ***********************************************************************/
/* (defmethod slot-definition-allocation
 *     ((inst standard-slot-definition)) ...) -> symbol
 */
static int method_slot_definition_allocation(Execute ptr,
		addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSNAME_ALLOCATION, &key);
	Return(clos_check_(var, key, &var));
	setresult_control(ptr, var);

	return 0;
}

static void method_type_slot_definition_allocation(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static int defmethod_slot_definition_allocation_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_slot_definition_allocation);
	method_type_slot_definition_allocation(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric slot-definition-allocation (class)) -> symbol */
int defgeneric_slot_definition_allocation_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_ALLOCATION, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_slot_definition_allocation_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-definition-initargs
 ***********************************************************************/
/* (defmethod slot-definition-initargs
 *     ((inst standard-slot-definition)) ...) -> t
 */
static int method_slot_definition_initargs(Execute ptr,
		addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSNAME_INITARGS, &key);
	Return(clos_check_(var, key, &var));
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_slot_definition_initargs_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_slot_definition_initargs);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric slot-definition-initargs (class)) -> t */
int defgeneric_slot_definition_initargs_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_INITARGS, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_slot_definition_initargs_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-definition-initform
 ***********************************************************************/
/* (defmethod slot-definition-initform
 *     ((inst standard-slot-definition)) ...) -> t
 */
static int method_slot_definition_initform(Execute ptr,
		addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSNAME_INITFORM, &key);
	Return(clos_check_(var, key, &var));
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_slot_definition_initform_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_slot_definition_initform);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric slot-definition-initform (class)) -> t */
int defgeneric_slot_definition_initform_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_INITFORM, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_slot_definition_initform_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-definition-initfunction
 ***********************************************************************/
/* (defmethod slot-definition-initfunction
 *     ((inst standard-slot-definition)) ...) -> t
 */
static int method_slot_definition_initfunction(Execute ptr,
		addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSNAME_INITFUNCTION, &key);
	Return(clos_check_(var, key, &var));
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_slot_definition_initfunction_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_slot_definition_initfunction);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric slot-definition-initfunction (class)) -> t */
int defgeneric_slot_definition_initfunction_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_INITFUNCTION, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_slot_definition_initfunction_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  generic-function-name
 ***********************************************************************/
/* (defmethod generic-function-name (clos) ...) -> symbol */
static int method_generic_function_name(Execute ptr,
		addr method, addr next, addr var)
{
	Return(stdget_generic_name_(var, &var));
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_generic_function_name_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_generic_function_name);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric generic-function-name (clos)) -> symbol */
int defgeneric_generic_function_name_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_NAME, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_generic_function_name_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  (setf generic-function-name)
 ***********************************************************************/
/* (defmethod (setf generic-function-name) (t class) ...) -> t */
static int method_setf_generic_function_name(Execute ptr,
		addr method, addr next, addr var, addr clos)
{
	addr ignore;

	Return(parse_callname_error_(&ignore, var));
	Return(stdset_generic_name_(clos, var));
	setresult_control(ptr, var);

	return 0;
}

static void method_type_setf_generic_function_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, T);
	typeargs_var2(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_setf_generic_function_name(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	ArgumentMethod_var(&type1, T);
	ArgumentMethod_var(&type2, STANDARD_GENERIC_FUNCTION);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_setf_generic_function_name_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4(call, p_method_setf_generic_function_name);
	method_type_setf_generic_function_name(&type);
	settype_function(call, type);
	/* method */
	method_argument_setf_generic_function_name(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric (setf generic-function-name) (t class)) -> t */
int defgeneric_setf_generic_function_name_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_NAME, &symbol);
	mop_argument_generic_var2(&gen);
	setf_callname_heap(&name, symbol);
	Return(generic_make_(&gen, name, gen));
	setsetf_symbol(symbol, gen);
	/* method */
	Return(defmethod_setf_generic_function_name_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  generic-function-methods
 ***********************************************************************/
/* (defmethod generic-function-methods (clos) ...) -> symbol */
static int method_generic_function_methods(Execute ptr,
		addr method, addr next, addr var)
{
	addr root, list, pos;
	size_t size, i;

	Return(stdget_generic_vector_(var, &var));
	lenarray(var, &size);
	root = Nil;
	for (i = 0; i < size; i++) {
		getarray(var, i, &list);
		while (list != Nil) {
			Return_getcons(list, &pos, &list);
			cons_heap(&root, pos, root);
		}
	}
	nreverse(&root, root);
	setresult_control(ptr, root);

	return 0;
}

static int defmethod_generic_function_methods_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_generic_function_methods);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric generic-function-methods (clos)) -> symbol */
int defgeneric_generic_function_methods_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_METHODS, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_generic_function_methods_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  generic-function-lambda-list
 ***********************************************************************/
static int method_generic_function_lambda_list(Execute ptr,
		addr method, addr next, addr var)
{
	Return(stdget_generic_lambda_list_(var, &var));
	Check(argumentp(var), "type error");
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_generic_function_lambda_list_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_generic_function_lambda_list);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric generic-function-lambda-list (clos)) -> symbol */
int defgeneric_generic_function_lambda_list_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_LAMBDA_LIST, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_generic_function_lambda_list_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  generic-function-argument-precedence-order
 ***********************************************************************/
static int method_generic_function_argument_precedence_order(Execute ptr,
		addr method, addr next, addr var)
{
	Return(stdget_generic_argument_precedence_order_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_generic_function_argument_precedence_order_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_generic_function_argument_precedence_order);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric generic-function-argument-precedence-order (clos)) -> symbol */
int defgeneric_generic_function_argument_precedence_order_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_ARGUMENT_PRECEDENCE_ORDER, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_generic_function_argument_precedence_order_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  generic-function-declarations
 ***********************************************************************/
static int method_generic_function_declarations(Execute ptr,
		addr method, addr next, addr var)
{
	Return(stdget_generic_declarations_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_generic_function_declarations_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_generic_function_declarations);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric generic-function-declarations (clos)) -> symbol */
int defgeneric_generic_function_declarations_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_DECLARATIONS, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_generic_function_declarations_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  generic-function-method-class
 ***********************************************************************/
static int method_generic_function_method_class(Execute ptr,
		addr method, addr next, addr var)
{
	Return(stdget_generic_method_class_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_generic_function_method_class_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_generic_function_method_class);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric generic-function-method-class (clos)) -> symbol */
int defgeneric_generic_function_method_class_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_METHOD_CLASS, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_generic_function_method_class_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  generic-function-method-combination
 ***********************************************************************/
static int method_generic_function_method_combination(Execute ptr,
		addr method, addr next, addr var)
{
	Return(stdget_generic_method_combination_(var, &var));
	if (var == Nil)
		GetConst(CLOS_COMBINATION_STANDARD, &var);
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_generic_function_method_combination_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_generic_function_method_combination);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric generic-function-method-combination (clos)) -> symbol */
int defgeneric_generic_function_method_combination_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_METHOD_COMBINATION, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_generic_function_method_combination_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  method-function
 ***********************************************************************/
static int method_method_function(Execute ptr, addr method, addr next, addr var)
{
	Return(stdget_method_function_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_method_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_method_function);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_METHOD);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric method-function (clos)) -> symbol */
int defgeneric_method_function_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_METHOD_FUNCTION, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_method_function_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  method-generic-function
 ***********************************************************************/
static int method_method_generic_function(Execute ptr,
		addr method, addr next, addr var)
{
	Return(stdget_method_generic_function_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_method_generic_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_method_generic_function);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_METHOD);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric method-generic-function (clos)) -> symbol */
int defgeneric_method_generic_function_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_METHOD_GENERIC_FUNCTION, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_method_generic_function_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  method-lambda-list
 ***********************************************************************/
static int method_method_lambda_list(Execute ptr, addr method, addr next, addr var)
{
	Return(stdget_method_lambda_list_(var, &var));
	if (argumentp(var)) {
		Return(argument_method_lambda_heap_(&var, var));
	}
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_method_lambda_list_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_method_lambda_list);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_METHOD);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric method-lambda-list (clos)) -> symbol */
int defgeneric_method_lambda_list_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_METHOD_LAMBDA_LIST, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_method_lambda_list_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  method-specializers
 ***********************************************************************/
static int method_method_specializers(Execute ptr, addr method, addr next, addr var)
{
	Return(stdget_method_specializers_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_method_specializers_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_method_specializers);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_METHOD);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric method-specializers (clos)) -> symbol */
int defgeneric_method_specializers_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_METHOD_SPECIALIZERS, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_method_specializers_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  method-qualifiers
 ***********************************************************************/
static int method_method_qualifiers(Execute ptr, addr method, addr next, addr var)
{
	Return(stdget_method_qualifiers_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_method_qualifiers_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_method_qualifiers);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_METHOD);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defgeneric method-qualifiers (clos)) -> symbol */
static int defgeneric_method_qualifiers_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_METHOD_QUALIFIERS, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_method_qualifiers_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  accessor-method-slot-definition
 ***********************************************************************/
static int method_accessor_method_slot_definition(Execute ptr,
		addr method, addr next, addr var)
{
	return fmte_("There is no accessor-method in ~S.", NULL);
}

static int defmethod_accessor_method_slot_definition_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_accessor_method_slot_definition);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_METHOD);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

int defgeneric_accessor_method_slot_definition_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_ACCESSOR_METHOD_SLOT_DEFINITION, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_accessor_method_slot_definition_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  function
 ***********************************************************************/
static void init_require_mop_reader(void)
{
	/* Classes */
	SetPointerType(empty, make_slot_definition_call);
	SetPointerType(var3, method_class_slots);
	SetPointerType(var3, method_class_direct_slots);
	SetPointerType(var3, method_class_default_initargs);
	SetPointerType(var3, method_class_direct_default_initargs);
	SetPointerType(var3, method_class_precedence_list);
	SetPointerType(var3, method_class_direct_superclasses);
	SetPointerType(var3, method_class_direct_subclasses);
	SetPointerType(var3, method_class_finalized_p);
	SetPointerType(var3, method_class_prototype);
	/* Slot definitions */
	SetPointerType(var3, method_slot_definition_name);
	SetPointerType(var3, method_slot_definition_type);
	SetPointerType(var3, method_slot_definition_allocation);
	SetPointerType(var3, method_slot_definition_initargs);
	SetPointerType(var3, method_slot_definition_initform);
	SetPointerType(var3, method_slot_definition_initfunction);
	/* Generic functions */
	SetPointerType(var3, method_generic_function_name);
	SetPointerType(var4, method_setf_generic_function_name);
	SetPointerType(var3, method_generic_function_methods);
	SetPointerType(var3, method_generic_function_lambda_list);
	SetPointerType(var3, method_generic_function_argument_precedence_order);
	SetPointerType(var3, method_generic_function_declarations);
	SetPointerType(var3, method_generic_function_method_class);
	SetPointerType(var3, method_generic_function_method_combination);
	/* Methods */
	SetPointerType(var3, method_method_function);
	SetPointerType(var3, method_method_generic_function);
	SetPointerType(var3, method_method_lambda_list);
	SetPointerType(var3, method_method_specializers);
	SetPointerType(var3, method_accessor_method_slot_definition);
}

void init_mop_reader(void)
{
	/* Classes */
	SetPointerType(var3, method_class_name);
	SetPointerType(var4, method_setf_class_name);
	/* Method */
	SetPointerType(var3, method_method_qualifiers);
	/* Require */
	init_require_mop_reader();
}

int build_mop_reader_(Execute ptr)
{
	/* Classes */
	Return(defgeneric_class_name_(ptr));
	Return(defgeneric_setf_class_name_(ptr));
	/* Method */
	Return(defgeneric_method_qualifiers_(ptr));

	return 0;
}


/************************************************************
 *  number.c
 ************************************************************/

int numberp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_FIXNUM
		|| type == LISPTYPE_BIGNUM
		|| type == LISPTYPE_RATIO
		|| type == LISPTYPE_SINGLE_FLOAT
		|| type == LISPTYPE_DOUBLE_FLOAT
		|| type == LISPTYPE_LONG_FLOAT
		|| type == LISPTYPE_COMPLEX
		|| type == LISPTYPE_SHORT_FLOAT;
}

int number_result_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (complexp(pos))
		return complex_result_local_(local, pos, ret);
	else
		return rational_result_local_(local, pos, ret);
}

int number_result_heap_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (complexp(pos))
		return complex_result_heap_(local, pos, ret);
	else
		return rational_result_heap_(local, pos, ret);
}

int number_throw_alloc_(LocalRoot local, addr pos, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return complex_throw_alloc_(local, pos, ret);

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}

	return 0;
}

int number_throw_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return number_throw_alloc_(local, pos, ret);
}

int number_throw_heap_(addr pos, addr *ret)
{
	return number_throw_alloc_(NULL, pos, ret);
}

int number_copy_alloc_(LocalRoot local, addr pos, addr *ret)
{
	if (complexp(pos))
		return complex_copy_alloc_(local, pos, ret);
	else
		return real_copy_alloc_(local, pos, ret);
}

int number_copy_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return number_copy_alloc_(local, pos, ret);
}

int number_copy_heap_(addr pos, addr *ret)
{
	return number_copy_alloc_(NULL, pos, ret);
}


/*
 *  abs
 */
int abs_number_common_(addr left, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			abs_fixnum_integer_common(left, ret);
			break;

		case LISPTYPE_BIGNUM:
			abs_bignum_integer_common(left, ret);
			break;

		case LISPTYPE_RATIO:
			abs_ratio_heap(left, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			abs_floats_heap(left, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			abs_floatd_heap(left, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			abs_floatl_heap(left, ret);
			break;

		case LISPTYPE_COMPLEX:
			return abs_complex_common_(left, ret);

		default:
			return TypeError_(left, NUMBER);
	}

	return 0;
}


/*
 *  signum
 */
static void signum_single_common(addr pos, addr *ret)
{
	single_float value;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	GetSingleFloat(pos, &value);
	if (value == 0.0f)
		*ret = pos;
	else
		single_float_heap(ret, (value < 0.0f)? -1.0f: 1.0f);
}

static void signum_double_common(addr pos, addr *ret)
{
	double_float value;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	GetDoubleFloat(pos, &value);
	if (value == 0.0)
		*ret = pos;
	else
		double_float_heap(ret, (value < 0.0)? -1.0: 1.0);
}

static void signum_long_common(addr pos, addr *ret)
{
	long_float value;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, &value);
	if (value == 0.0L)
		*ret = pos;
	else
		long_float_heap(ret, (value < 0.0L)? -1.0L: 1.0L);
}

static void signum_fixnum_common(addr pos, addr *ret)
{
	fixnum value;

	CheckType(pos, LISPTYPE_FIXNUM);
	GetFixnum(pos, &value);
	if (value == 0)
		*ret = pos;
	else
		fixnum_heap(ret, (value < 0)? -1: 1);
}

static void signum_bignum_common(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	if (zerop_bignum(pos))
		*ret = pos;
	else
		fixnum_heap(ret, minusp_bignum(pos)? -1: 1);
}

static void signum_ratio_common(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos))
		*ret = pos;
	else
		fixnum_heap(ret, minusp_ratio(pos)? -1: 1);
}

int signum_number_common_(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			signum_single_common(pos, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			signum_double_common(pos, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			signum_long_common(pos, ret);
			break;

		case LISPTYPE_COMPLEX:
			return signum_complex_common_(pos, ret);

		case LISPTYPE_FIXNUM:
			signum_fixnum_common(pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			signum_bignum_common(pos, ret);
			break;

		case LISPTYPE_RATIO:
			signum_ratio_common(pos, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, NUMBER);
	}

	return 0;
}


/*
 *  sqrt
 */
static int sqrt_single_common_(struct mathreal2_struct *ptr, addr *ret)
{
	single_float real, imag;

	real = ptr->v.s.a;
	imag = ptr->v.s.b;
	if (0.0f <= real && imag == 0.0f) {
		return single_float_check_heap_(ret, sqrtf(real));
	}
	else {
		csqrt_f(real, imag, &real, &imag);
		return complex_single_heap_(ret, real, imag);
	}
}

static int sqrt_double_common_(struct mathreal2_struct *ptr, addr *ret)
{
	double_float real, imag;

	real = ptr->v.d.a;
	imag = ptr->v.d.b;
	if (0.0 <= real && imag == 0.0) {
		return double_float_check_heap_(ret, sqrt(real));
	}
	else {
		csqrt_d(real, imag, &real, &imag);
		return complex_double_heap_(ret, real, imag);
	}
}

static int sqrt_long_common_(struct mathreal2_struct *ptr, addr *ret)
{
	long_float real, imag;

	real = ptr->v.l.a;
	imag = ptr->v.l.b;
	if (0.0L <= real && imag == 0.0L) {
		return long_float_check_heap_(ret, sqrtl(real));
	}
	else {
		csqrt_l(real, imag, &real, &imag);
		return complex_long_heap_(ret, real, imag);
	}
}

int sqrt_number_common_(addr pos, addr *ret)
{
	enum MathType type;
	struct mathreal2_struct str;

	Return(getmathcomplex1_sqrt_(&str, pos, &type));
	switch (type) {
		case MathType_single:
			return sqrt_single_common_(&str, ret);

		case MathType_double:
			return sqrt_double_common_(&str, ret);

		case MathType_long:
			return sqrt_long_common_(&str, ret);

		case MathType_complex:
		case MathType_rational:
		case MathType_error:
		default:
			*ret = Nil;
			return TypeError_(pos, NUMBER);
	}
}


/************************************************************
 *  number_equal.c
 ************************************************************/

int zerop_numberp(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			*ret = zerop_fixnum(pos);
			break;

		case LISPTYPE_BIGNUM:
			*ret = zerop_bignum(pos);
			break;

		case LISPTYPE_RATIO:
			*ret = zerop_ratio(pos);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			*ret = zerop_single_float(pos);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = zerop_double_float(pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = zerop_long_float(pos);
			break;

		case LISPTYPE_COMPLEX:
			return zerop_complex_(pos, ret);

		default:
			*ret = 0;
			return 1;
	}

	return 0;
}

int zerop_number_(addr pos, int *ret)
{
	if (zerop_numberp(pos, ret))
		return TypeError_(pos, NUMBER);

	return 0;
}

static int equal_fixnum_number_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = equal_ff_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			*ret = equal_fb_real(left, right);
			break;

		case LISPTYPE_RATIO:
			*ret = equal_fr_real(left, right);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			*ret = equal_fs_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = equal_fd_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = equal_fl_real(left, right);
			break;

		case LISPTYPE_COMPLEX:
			return equal_fc_number_(left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int equal_bignum_number_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = equal_bf_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			*ret = equal_bb_real(left, right);
			break;

		case LISPTYPE_RATIO:
			*ret = equal_br_real(left, right);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return equal_bs_real_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_bd_real_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return equal_bl_real_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return equal_bc_number_(left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int equal_ratio_number_(LocalRoot local, addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = equal_rf_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			*ret = equal_rb_real(left, right);
			break;

		case LISPTYPE_RATIO:
			*ret = equal_rr_real(left, right);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return equal_rs_real_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_rd_real_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return equal_rl_real_(local, left, right, ret);

		case LISPTYPE_COMPLEX:
			return equal_rc_number_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int equal_single_float_number_(LocalRoot local, addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = equal_sf_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			return equal_sb_real_(left, right, ret);

		case LISPTYPE_RATIO:
			return equal_sr_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = equal_ss_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = equal_sd_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = equal_sl_real(left, right);
			break;

		case LISPTYPE_COMPLEX:
			return equal_sc_number_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int equal_double_float_number_(LocalRoot local, addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = equal_df_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			return equal_db_real_(left, right, ret);

		case LISPTYPE_RATIO:
			return equal_dr_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = equal_ds_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = equal_dd_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = equal_dl_real(left, right);
			break;

		case LISPTYPE_COMPLEX:
			return equal_dc_number_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int equal_long_float_number_(LocalRoot local, addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = equal_lf_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			return equal_lb_real_(left, right, ret);

		case LISPTYPE_RATIO:
			return equal_lr_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = equal_ls_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = equal_ld_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = equal_ll_real(left, right);
			break;

		case LISPTYPE_COMPLEX:
			return equal_lc_number_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int equal_complex_number_(LocalRoot local, addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_COMPLEX);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return equal_cf_number_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return equal_cb_number_(left, right, ret);

		case LISPTYPE_RATIO:
			return equal_cr_number_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return equal_cs_number_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_cd_number_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return equal_cl_number_(local, left, right, ret);

		case LISPTYPE_COMPLEX:
			return equal_complex_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, NUMBER);
	}
}

int equal_number_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return equal_fixnum_number_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return equal_bignum_number_(left, right, ret);

		case LISPTYPE_RATIO:
			return equal_ratio_number_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return equal_single_float_number_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_double_float_number_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return equal_long_float_number_(local, left, right, ret);

		case LISPTYPE_COMPLEX:
			return equal_complex_number_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, NUMBER);
	}
}

int not_equal_number_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(equal_number_(local, left, right, &check));
	return Result(ret, ! check);
}


/************************************************************
 *  number_multi.c
 ************************************************************/

/*
 *  multiple
 */
static int multi_fixnum_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_ff_real_common(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_fb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_fr_real_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_fs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_fd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_fl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return multi_fc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int multi_bignum_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_bf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_bb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_br_real_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_bs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_bd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_bl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return multi_bc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int multi_ratio_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_rf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_rb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_rr_real_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_rs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_rd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_rl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return multi_rc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int multi_single_float_number_heap_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_sr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_ss_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_sd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_sl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return multi_sc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int multi_double_float_number_heap_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_dr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_ds_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_dd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_dl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return multi_dc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int multi_long_float_number_heap_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_lr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_ls_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_ld_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_ll_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return multi_lc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int multi_complex_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_COMPLEX);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_cf_number_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_cb_number_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_cr_number_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_cs_number_common_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_cd_number_common_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_cl_number_common_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return multi_cc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

int multi_number_heap_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return multi_fixnum_number_heap_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_bignum_number_heap_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_ratio_number_heap_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_single_float_number_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_double_float_number_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_long_float_number_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return multi_complex_number_heap_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, NUMBER);
	}
}


/*
 *  inverse
 */
int inverse_number_heap_(LocalRoot local, addr left, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return inverse_fixnum_common_(left, ret);

		case LISPTYPE_BIGNUM:
			return inverse_bignum_common_(left, ret);

		case LISPTYPE_RATIO:
			return inverse_ratio_common_(local, left, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return inverse_single_float_heap_(left, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return inverse_double_float_heap_(left, ret);

		case LISPTYPE_LONG_FLOAT:
			return inverse_long_float_heap_(left, ret);

		case LISPTYPE_COMPLEX:
			return inverse_complex_common_(local, left, ret);

		default:
			*ret = Nil;
			return TypeError_(left, NUMBER);
	}
}


/*
 *  division
 */
static int div_fixnum_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_ff_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_fb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_fr_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_fs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_fd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_fl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return div_fc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int div_bignum_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_bf_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_br_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_bs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_bd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_bl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return div_bc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int div_ratio_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_rf_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_rb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rr_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_rs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_rd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_rl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return div_rc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int div_single_float_number_heap_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_sr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ss_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_sd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_sl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return div_sc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int div_double_float_number_heap_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_dr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ds_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_dl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return div_dc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int div_long_float_number_heap_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_lr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ls_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_ld_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ll_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return div_lc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int div_complex_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_COMPLEX);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_cf_number_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_cb_number_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_cr_number_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_cs_number_common_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_cd_number_common_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_cl_number_common_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return div_cc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

int div_number_heap_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fixnum_number_heap_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bignum_number_heap_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_ratio_number_heap_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_single_float_number_heap_(left, right ,ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_double_float_number_heap_(left, right ,ret);

		case LISPTYPE_LONG_FLOAT:
			return div_long_float_number_heap_(left, right ,ret);

		case LISPTYPE_COMPLEX:
			return div_complex_number_heap_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, NUMBER);
	}
}


/************************************************************
 *  number_plus.c
 ************************************************************/

/*
 *  1+, 1-
 */
int oneplus_number_common_(LocalRoot local, addr value, addr *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, 1, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, 1, ret);
			break;

		case LISPTYPE_RATIO:
			plus_rv_real_common(local, value, 1, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_sv_heap_(value, 1.0f, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_dv_heap_(value, 1.0, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_lv_heap_(value, 1.0L, ret);

		case LISPTYPE_COMPLEX:
			return oneplus_complex_heap_(local, value, ret);

		default:
			*ret = Nil;
			return TypeError_(value, NUMBER);
	}

	return 0;
}

int oneminus_number_common_(LocalRoot local, addr value, addr *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, -1, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, -1, ret);
			break;

		case LISPTYPE_RATIO:
			plus_rv_real_common(local, value, -1, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_sv_heap_(value, -1.0f, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_dv_heap_(value, -1.0, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_lv_heap_(value, -1.0L, ret);

		case LISPTYPE_COMPLEX:
			return oneminus_complex_heap_(local, value, ret);

		default:
			*ret = Nil;
			return TypeError_(value, NUMBER);
	}

	return 0;
}

int sign_reverse_number_common_(addr left, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			sigrev_fixnum_integer_common(left, ret);
			break;

		case LISPTYPE_BIGNUM:
			sigrev_bignum_integer_common(left, ret);
			break;

		case LISPTYPE_RATIO:
			sign_reverse_ratio_common(left, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			sign_reverse_floats_heap(left, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			sign_reverse_floatd_heap(left, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			sign_reverse_floatl_heap(left, ret);
			break;

		case LISPTYPE_COMPLEX:
			return sign_reverse_complex_common_(left, ret);

		default:
			*ret = Nil;
			return TypeError_(left, NUMBER);
	}

	return 0;
}


/*
 *  plus
 */
static int plus_fixnum_number_heap_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_real_common(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_fb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_fr_real_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_fs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_fd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_fl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return plus_fc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int plus_bignum_number_heap_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_br_real_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_bs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_bd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_bl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return plus_bc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int plus_ratio_number_heap_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_rf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_rb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_rr_real_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_rs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_rd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_rl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return plus_rc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int plus_single_float_number_heap_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_sr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_ss_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_sd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_sl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return plus_sc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int plus_double_float_number_heap_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_dr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_ds_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_dd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_dl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return plus_dc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int plus_long_float_number_heap_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_lr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_ls_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_ld_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_ll_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return plus_lc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int plus_complex_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_COMPLEX);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_cf_number_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_cb_number_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_cr_number_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_cs_number_common_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_cd_number_common_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_cl_number_common_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return plus_cc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

int plus_number_heap_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fixnum_number_heap_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bignum_number_heap_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_ratio_number_heap_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_single_float_number_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_double_float_number_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_long_float_number_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return plus_complex_number_heap_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, NUMBER);
	}
}


/*
 *  minus
 */
static int minus_fixnum_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_common(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_fb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_fr_real_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_fs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_fd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_fl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return minus_fc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int minus_bignum_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_bf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_bb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_br_real_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_bs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_bd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_bl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return minus_bc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int minus_ratio_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_rf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_rb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_rr_real_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_rs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_rd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_rl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return minus_rc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int minus_single_float_number_heap_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_sr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ss_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_sd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_sl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return minus_sc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int minus_double_float_number_heap_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_dr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ds_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_dd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_dl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return minus_dc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int minus_long_float_number_heap_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_lr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ls_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_ld_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_ll_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return minus_lc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int minus_complex_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_COMPLEX);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_cf_number_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_cb_number_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_cr_number_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_cs_number_common_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_cd_number_common_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_cl_number_common_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return minus_cc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

int minus_number_heap_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_fixnum_number_heap_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_bignum_number_heap_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_ratio_number_heap_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_single_float_number_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_double_float_number_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_long_float_number_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return minus_complex_number_heap_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, NUMBER);
	}
}


/************************************************************
 *  number_random.c
 ************************************************************/

/*
 *  random
 */
#ifdef LISP_32BIT
#define single_float_random float_random_32bit
#define double_float_random double_random_32bit
#define long_float_random long_random_32bit
static fixed random_full_fixed(struct random_state *state)
{
	return (fixed)random_number_32bit(state);
}
static fixed random_equal_fixed(struct random_state *state, fixed value)
{
	return (fixed)random_equal_32bit(state, (uint32_t)value);
}
static fixed random_less_fixed(struct random_state *state, fixed value)
{
	return (fixed)random_less_32bit(state, (uint32_t)value);
}
#else
#define single_float_random float_random_64bit
#define double_float_random double_random_64bit
#define long_float_random long_random_64bit
static fixed random_full_fixed(struct random_state *state)
{
	return (fixed)random_number_64bit(state);
}
static fixed random_equal_fixed(struct random_state *state, fixed value)
{
	return (fixed)random_equal_64bit(state, (uint64_t)value);
}
static fixed random_less_fixed(struct random_state *state, fixed value)
{
	return (fixed)random_less_64bit(state, (uint64_t)value);
}
#endif


/*
 *  random fixnum
 */
static void random_fixnum_common(struct random_state *state, addr pos, addr *ret)
{
	int ignore;
	fixed value;

	castfixed_fixnum(pos, &ignore, &value);
	Check(value <= 0, "Invalid fixnum value.");
	value = random_less_fixed(state, value);
	fixnum_heap(ret, (fixnum)value);
}


/*
 *  random bignum
 */
static void random_bignum1_common(LocalRoot local,
		struct random_state *state, addr pos, addr *ret)
{
	fixed value, *data;
	LocalStack stack;

	GetDataBignum(pos, &data);
	value = random_less_fixed(state, data[0]);
	push_local(local, &stack);
	bignum_value_local(local, &pos, SignPlus, value);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

static int random_insertbuffer(LocalRoot local, struct random_state *state, addr pos)
{
	fixed *data, v1, v2;
	size_t size, i, index;

	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);
	index = size - 1;
	v1 = data[index];
	v2 = random_equal_fixed(state, v1);
	if (v1 == v2) {
		for (i = 1; i < size; i++) {
			index = size - i - 1;
			v1 = data[index];
			v2 = random_full_fixed(state);
			if (v1 < v2)
				return 1;
			if (v1 > v2)
				goto tail;
		}
		return 0;
	}

tail:
	data[index] = v2;
	for (i = 0; i < index; i++)
		data[i] = random_full_fixed(state);

	return 0;
}

static void random_bigdata_common(LocalRoot local,
		struct random_state *state, addr pos, addr *ret)
{
	addr value;
	LocalStack stack;

	push_local(local, &stack);
	plus_bv_bignum_local(local, pos, -1, &value);
	while (random_insertbuffer(local, state, value))
		continue;
	sizepress_bignum(value);
	bignum_result_heap(value, ret);
	rollback_local(local, stack);
}

static void random_bignum_common(LocalRoot local,
		struct random_state *state, addr pos, addr *ret)
{
	size_t size;

	CheckLocal(local);
	CheckType(pos, LISPTYPE_BIGNUM);
	Check(! plusp_bignum(pos), "Invalid bignum value.");
	GetSizeBignum(pos, &size);
	if (size == 1)
		random_bignum1_common(local, state, pos, ret);
	else
		random_bigdata_common(local, state, pos, ret);
}


/*
 *  random float
 */
static void random_single_common(struct random_state *state, addr pos, addr *ret)
{
	single_float value, check;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	GetSingleFloat(pos, &value);
	for (;;) {
		check = single_float_random(state) * value;
		if (check < value)
			break;
	}
	single_float_heap(ret, check);
}

static void random_double_common(struct random_state *state, addr pos, addr *ret)
{
	double_float value, check;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	GetDoubleFloat(pos, &value);
	for (;;) {
		check = double_float_random(state) * value;
		if (check < value)
			break;
	}
	double_float_heap(ret, check);
}

static void random_long_common(struct random_state *state, addr pos, addr *ret)
{
	long_float value, check;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, &value);
	for (;;) {
		check = long_float_random(state) * value;
		if (check < value)
			break;
	}
	long_float_heap(ret, check);
}


/*
 *  common
 */
int random_number_common_(LocalRoot local, addr pos, addr state, addr *ret)
{
	struct random_state *ptr;

	CheckType(state, LISPTYPE_RANDOM_STATE);
	ptr = struct_random_state(state);
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			random_fixnum_common(ptr, pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			random_bignum_common(local, ptr, pos, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			random_single_common(ptr, pos, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			random_double_common(ptr, pos, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			random_long_common(ptr, pos, ret);
			break;

		default:
			*ret = Nil;
			return fmte_("The random number ~S "
					"must be an integer or float.", pos, NULL);
	}

	return 0;
}


/************************************************************
 *  object.c
 ************************************************************/

#define FIXNUM_CACHE		1024

/*
 *  alloc
 */
void alloc_cons(LocalRoot local, addr *ret)
{
	if (local)
		local_cons(local, ret);
	else
		heap_cons(ret);
}

void alloc_symbol(LocalRoot local, addr *ret)
{
	if (local)
		local_symbol(local, ret);
	else
		heap_symbol(ret);
}

void alloc_array2_memory(LocalRoot local, addr *ret, enum LISPTYPE type, byte16 array)
{
	if (local)
		local_array2(local, ret, type, array);
	else
		heap_array2(ret, type, array);
}

void alloc_array4_memory(LocalRoot local, addr *ret, enum LISPTYPE type, byte32 array)
{
	if (local)
		local_array4(local, ret, type, array);
	else
		heap_array4(ret, type, array);
}

void alloc_body2_memory(LocalRoot local, addr *ret, enum LISPTYPE type, byte16 body)
{
	if (local)
		local_body2(local, ret, type, body);
	else
		heap_body2(ret, type, body);
}

void alloc_body4_memory(LocalRoot local, addr *ret, enum LISPTYPE type, byte32 body)
{
	if (local)
		local_body4(local, ret, type, body);
	else
		heap_body4(ret, type, body);
}

void alloc_smallsize_memory(LocalRoot local,
		addr *ret, enum LISPTYPE type, byte array, byte body)
{
	if (local)
		local_smallsize(local, ret, type, array, body);
	else
		heap_smallsize(ret, type, array, body);
}

void alloc_arraybody_memory(LocalRoot local,
		addr *ret, enum LISPTYPE type, byte16 array, byte16 body)
{
	if (local)
		local_arraybody(local, ret, type, array, body);
	else
		heap_arraybody(ret, type, array, body);
}

void alloc_array(LocalRoot local, addr *ret, enum LISPTYPE type, size_t array)
{
	if (local)
		local_array(local, ret, type, array);
	else
		heap_array(ret, type, array);
}

void alloc_body(LocalRoot local, addr *ret, enum LISPTYPE type, size_t body)
{
	if (local)
		local_body(local, ret, type, body);
	else
		heap_body(ret, type, body);
}

#ifdef LISP_ARCH_64BIT
void alloc_array8(LocalRoot local, addr *ret, enum LISPTYPE type, size_t array)
{
	if (local)
		local_array8(local, ret, type, array);
	else
		heap_array8(ret, type, array);
}

void alloc_body8(LocalRoot local, addr *ret, enum LISPTYPE type, size_t body)
{
	if (local)
		local_body8(local, ret, type, body);
	else
		heap_body8(ret, type, body);
}
#endif

#ifdef LISP_DEBUG
void alloc_array2_debug(LocalRoot local, addr *ret, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFUL < array, "size error");
	alloc_array2_memory(local, ret, type, (byte16)array);
}
void alloc_array4_debug(LocalRoot local, addr *ret, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	alloc_array4_memory(local, ret, type, (byte32)array);
}
void alloc_body2_debug(LocalRoot local, addr *ret, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFUL < body, "size error");
	alloc_body2_memory(local, ret, type, (byte16)body);
}
void alloc_body4_debug(LocalRoot local, addr *ret, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFFFFFUL < body, "size error");
	alloc_body4_memory(local, ret, type, (byte32)body);
}
void alloc_smallsize_debug(LocalRoot local,
		addr *ret, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFUL < array, "array size error");
	Check(0xFFUL < body, "body size error");
	alloc_smallsize_memory(local, ret, type, (byte)array, (byte)body);
}
void alloc_arraybody_debug(LocalRoot local,
		addr *ret, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFFFUL < array, "array size error");
	Check(0xFFFFUL < body, "body size error");
	alloc_arraybody_memory(local, ret, type, (byte16)array, (byte16)body);
}
#endif


/*
 *  init/free
 */
void build_object(void)
{
	addr pos;

	/* fixnum cache */
	heap_array4(&pos, LISPSYSTEM_FIXNUM_CACHE, (FIXNUM_CACHE * 2) + 1);
	SetConstant(CONSTANT_FIXNUM_CACHE, pos);

	/* fixnum max/min */
	make_fixnum_heap(&pos, FIXNUM_MAX);
	SetConstant(CONSTANT_FIXNUM_MAX, pos);
	make_fixnum_heap(&pos, FIXNUM_MIN);
	SetConstant(CONSTANT_FIXNUM_MIN, pos);
}


/*
 *  system object
 */
void nil_heap(void)
{
	addr pos, name;

	Nil = Unbound;
	heap_symbol(&pos); /* Don't use symbol_heap. */
	Nil = pos;
	strvect_char_heap(&name, "NIL");
	SetArrayA2_force(pos, SYMBOL_INDEX_SPECIAL, Nil);
	SetArrayA2_force(pos, SYMBOL_INDEX_CDR, Nil);
	SetArrayA2_force(pos, SYMBOL_INDEX_VALUE, Nil);
	SetValueSymbol(pos, Nil);
	SetArrayA2_force(pos, SYMBOL_INDEX_FUNCTION, Unbound);
	SetArrayA2_force(pos, SYMBOL_INDEX_NAME, name);
	SetArrayA2_force(pos, SYMBOL_INDEX_PACKAGE, Nil); /* "COMMON-LISP" */
	SetArrayA2_force(pos, SYMBOL_INDEX_PLIST, Nil);
	SetArrayA2_force(pos, SYMBOL_INDEX_INFO, Nil);
	SetType(pos, LISPTYPE_NIL);
	SetStatusValue(pos, LISPSTATUS_SYSTEM, 1);
	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	SetChain(pos, 0xFF);
}

void t_heap(void)
{
	addr pos, name;

	heap_symbol(&pos); /* Don't use symbol_heap. */
	T = pos;
	strvect_char_heap(&name, "T");
	SetValueSymbol(pos, T);
	SetArrayA2_force(pos, SYMBOL_INDEX_FUNCTION, Unbound);
	SetArrayA2_force(pos, SYMBOL_INDEX_NAME, name);
	SetArrayA2_force(pos, SYMBOL_INDEX_PACKAGE, Nil); /* "COMMON-LISP" */
	SetType(pos, LISPTYPE_T);
	SetStatusValue(pos, LISPSTATUS_SYSTEM, 1);
	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	SetChain(pos, 0xFF);
}

/* cons */
void consnil_heap(addr *ret)
{
	heap_cons(ret);
}
void conscar_heap(addr *ret, addr left)
{
	heap_cons(ret);
	SetCar_Low(*ret, left);
}
void conscdr_heap(addr *ret, addr right)
{
	heap_cons(ret);
	SetCdr_Low(*ret, right);
}
void cons_heap(addr *ret, addr left, addr right)
{
	heap_cons(ret);
	SetCons_Low(*ret, left, right);
}

void consnil_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	local_cons(local, ret);
}
void conscar_local(LocalRoot local, addr *ret, addr left)
{
	Check(local == NULL, "local error");
	local_cons(local, ret);
	SetCar_Low(*ret, left);
}
void conscdr_local(LocalRoot local, addr *ret, addr right)
{
	Check(local == NULL, "local error");
	local_cons(local, ret);
	SetCdr_Low(*ret, right);
}
void cons_local(LocalRoot local, addr *ret, addr left, addr right)
{
	Check(local == NULL, "local error");
	local_cons(local, ret);
	SetCons_Low(*ret, left, right);
}

void consnil_alloc(LocalRoot local, addr *ret)
{
	if (local)
		consnil_local(local, ret);
	else
		consnil_heap(ret);
}
void conscar_alloc(LocalRoot local, addr *ret, addr left)
{
	if (local)
		conscar_local(local, ret, left);
	else
		conscar_heap(ret, left);
}
void conscdr_alloc(LocalRoot local, addr *ret, addr right)
{
	if (local)
		conscdr_local(local, ret, right);
	else
		conscdr_heap(ret, right);
}
void cons_alloc(LocalRoot local, addr *ret, addr left, addr right)
{
	if (local)
		cons_local(local, ret, left, right);
	else
		cons_heap(ret, left, right);
}

addr refconscar_unsafe(addr pos)
{
	Check(! IsList(pos), "type error");
	return RefCar_Low(pos);
}
addr refconscdr_unsafe(addr pos)
{
	Check(! IsList(pos), "type error");
	return RefCdr_Low(pos);
}
void getconscar_unsafe(addr pos, addr *ret)
{
	Check(! IsList(pos), "type error");
	GetCar_Low(pos, ret);
}
void getconscdr_unsafe(addr pos, addr *ret)
{
	Check(! IsList(pos), "type error");
	GetCdr_Low(pos, ret);
}
void getcons_unsafe(addr pos, addr *left, addr *right)
{
	Check(! IsList(pos), "type error");
	GetCons_Low(pos, left, right);
}
void setconscar_unsafe(addr pos, addr value)
{
	Check(! IsCons(pos), "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetCar_Low(pos, value);
}
void setconscdr_unsafe(addr pos, addr value)
{
	Check(! IsCons(pos), "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetCdr_Low(pos, value);
}
void setcons_unsafe(addr pos, addr left, addr right)
{
	Check(! IsCons(pos), "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetCons_Low(pos, left, right);
}

void setconscar_force(addr pos, addr value)
{
	Check(! IsCons(pos), "type error");
	SetCar_force(pos, value);
}
void setconscdr_force(addr pos, addr value)
{
	Check(! IsCons(pos), "type error");
	SetCdr_force(pos, value);
}
void setcons_force(addr pos, addr left, addr right)
{
	Check(! IsArray(pos), "type error");
	SetCar_force(pos, left);
	SetCdr_force(pos, right);
}

/* list */
int listp(addr pos)
{
	return IsList(pos);
}

int consp(addr pos)
{
	return GetType(pos) == LISPTYPE_CONS;
}

int singlep(addr pos)
{
	if (GetType(pos) == LISPTYPE_CONS) {
		GetCdr(pos, &pos);
		return pos == Nil;
	}

	return 0;
}

/* vector */
void vector2_heap(addr *ret, size_t size)
{
	Check(0xFFFFUL < size, "size error");
	heap_array2(ret, LISPTYPE_VECTOR, size);
}
void vector2_local(LocalRoot local, addr *ret, size_t size)
{
	Check(local == NULL, "local error");
	Check(0xFFFFUL < size, "size error");
	local_array2(local, ret, LISPTYPE_VECTOR, size);
}
void vector2_alloc(LocalRoot local, addr *ret, size_t size)
{
	Check(0xFFFFUL < size, "size error");
	if (local)
		vector2_local(local, ret, size);
	else
		vector2_heap(ret, size);
}

void vector4_heap(addr *ret, size_t size)
{
	Check(0xFFFFFFFFUL < size, "size error");
	heap_array4(ret, LISPTYPE_VECTOR, size);
}
void vector4_local(LocalRoot local, addr *ret, size_t size)
{
	Check(local == NULL, "local error");
	Check(0xFFFFFFFFUL < size, "size error");
	local_array4(local, ret, LISPTYPE_VECTOR, size);
}
void vector4_alloc(LocalRoot local, addr *ret, size_t size)
{
	Check(0xFFFFFFFFUL < size, "size error");
	if (local)
		vector4_local(local, ret, size);
	else
		vector4_heap(ret, size);
}

#ifdef LISP_ARCH_64BIT
void vector8_heap(addr *ret, size_t size)
{
	heap_array8(ret, LISPTYPE_VECTOR, size);
}
void vector8_local(LocalRoot local, addr *ret, size_t size)
{
	Check(local == NULL, "local error");
	local_array8(local, ret, LISPTYPE_VECTOR, size);
}
void vector8_alloc(LocalRoot local, addr *ret, size_t size)
{
	if (local)
		vector8_local(local, ret, size);
	else
		vector8_heap(ret, size);
}
#endif

void vector_heap(addr *ret, size_t size)
{
	heap_array(ret, LISPTYPE_VECTOR, size);
}
void vector_local(LocalRoot local, addr *ret, size_t size)
{
	Check(local == NULL, "local error");
	local_array(local, ret, LISPTYPE_VECTOR, size);
}
void vector_alloc(LocalRoot local, addr *ret, size_t size)
{
	if (local)
		vector_local(local, ret, size);
	else
		vector_heap(ret, size);
}

void vector_type_heap(addr *ret, addr pos, size_t size)
{
	CheckType(pos, LISPTYPE_VECTOR);
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			vector2_heap(ret, size);
			break;

		case LISPSIZE_ARRAY4:
			vector4_heap(ret, size);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			vector8_heap(ret, size);
			break;
#endif

		default:
			Abort("Invalid vector type.");
			return;
	}
}

/* copy vector */
void copy_vector4_alloc(LocalRoot local, addr *ret, addr pos)
{
	addr array, one;
	size_t size, i;

	Check(GetType(pos) != LISPTYPE_VECTOR, "type error");
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4, "size error");
	LenArrayA4(pos, &size);
	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &one);
		SetArrayA4(array, i, one);
	}
	*ret = pos;
}

void copy_vector4_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	copy_vector4_alloc(local, ret, pos);
}

void copy_vector4_heap(addr *ret, addr pos)
{
	copy_vector4_alloc(NULL, ret, pos);
}

void copy_vector_alloc(LocalRoot local, addr *ret, addr pos)
{
	addr array, one;
	size_t size, i;

	Check(GetType(pos) != LISPTYPE_VECTOR, "type error");
	lenarray(pos, &size);

	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			vector2_alloc(local, &array, size);
			break;

		case LISPSIZE_ARRAY4:
			vector4_alloc(local, &array, size);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			vector8_alloc(local, &array, size);
			break;
#endif
		default:
			Abort("size error");
			return;
	}

	for (i = 0; i < size; i++) {
		getarray(pos, i, &one);
		setarray(array, i, one);
	}
	*ret = array;
}

void copy_vector_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	copy_vector_alloc(local, ret, pos);
}

void copy_vector_heap(addr *ret, addr pos)
{
	copy_vector_alloc(NULL, ret, pos);
}

/* fixnum */
int fixnump(addr pos)
{
	return GetType(pos) == LISPTYPE_FIXNUM;
}

void make_fixnum_heap(addr *ret, fixnum value)
{
	addr pos;

	heap_body2(&pos, LISPTYPE_FIXNUM, sizeof(value));
	SetFixnum_Low(pos, value);
	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	*ret = pos;
}

#define fixnum_cache_p(v) (-FIXNUM_CACHE <= (v) && (v) <= FIXNUM_CACHE)

void fixnum_heap(addr *ret, fixnum value)
{
	addr cache, pos;
	size_t index;

	/* make object */
	if (! fixnum_cache_p(value)) {
		make_fixnum_heap(ret, value);
		return;
	}

	/* cache */
	index = value + FIXNUM_CACHE;
	GetConst(FIXNUM_CACHE, &cache);
	GetArrayA4(cache, index, &pos);

	/* cache hit */
	if (pos != Nil) {
		*ret = pos;
		return;
	}

	/* add cache */
	make_fixnum_heap(&pos, value);
	SetArrayA4(cache, index, pos);
	*ret = pos;
}

void fixnum_local(LocalRoot local, addr *ret, fixnum value)
{
	Check(local == NULL, "local error");
	local_body2(local, ret, LISPTYPE_FIXNUM, sizeof(value));
	SetFixnum_Low(*ret, value);
}

void fixnum_alloc(LocalRoot local, addr *ret, fixnum value)
{
	if (local)
		fixnum_local(local, ret, value);
	else
		fixnum_heap(ret, value);
}

addr fixnumh(fixnum value)
{
	addr pos;
	fixnum_heap(&pos, value);
	return pos;
}

addr fixnuml(fixnum value)
{
	addr pos;
	fixnum_local(Local_Thread, &pos, value);
	return pos;
}

addr fixnuma(LocalRoot local, fixnum value)
{
	addr pos;
	fixnum_alloc(local, &pos, value);
	return pos;
}

const fixnum *ptrfixnum(addr pos)
{
	Check(GetType(pos) != LISPTYPE_FIXNUM, "type error");
	return PtrFixnum_Low(pos);
}
fixnum reffixnum(addr pos)
{
	Check(GetType(pos) != LISPTYPE_FIXNUM, "type error");
	return RefFixnum_Low(pos);
}
void getfixnum(addr pos, fixnum *ret)
{
	Check(GetType(pos) != LISPTYPE_FIXNUM, "type error");
	GetFixnum_Low(pos, ret);
}
void setfixnum(addr pos, fixnum value)
{
	Check(GetType(pos) != LISPTYPE_FIXNUM, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFixnum_Low(pos, value);
}

int fixnumequal(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type error");
	return RefBodyB2(left, fixnum) == RefBodyB2(right, fixnum);
}

int fixnumcompare(addr left, addr right)
{
	fixnum value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type error");
	GetFixnum_Low(left, &value1);
	GetFixnum_Low(right, &value2);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;

	return 0;
}

/* index */
int indexp(addr pos)
{
	return GetType(pos) == LISPTYPE_INDEX;
}

void index_heap(addr *ret, size_t value)
{
	heap_body2(ret, LISPTYPE_INDEX, sizeof(value));
	SetIndex_Low(*ret, value);
}

void index_local(LocalRoot local, addr *ret, size_t value)
{
	Check(local == NULL, "local error");
	local_body2(local, ret, LISPTYPE_INDEX, sizeof(value));
	SetIndex_Low(*ret, value);
}

void index_alloc(LocalRoot local, addr *ret, size_t value)
{
	if (local)
		index_local(local, ret, value);
	else
		index_heap(ret, value);
}

const size_t *ptrindex(addr pos)
{
	Check(GetType(pos) != LISPTYPE_INDEX, "type error");
	return PtrIndex_Low(pos);
}
size_t refindex(addr pos)
{
	Check(GetType(pos) != LISPTYPE_INDEX, "type error");
	return RefIndex_Low(pos);
}
void getindex(addr pos, size_t *ret)
{
	Check(GetType(pos) != LISPTYPE_INDEX, "type error");
	GetIndex_Low(pos, ret);
}
void setindex(addr pos, size_t value)
{
	Check(GetType(pos) != LISPTYPE_INDEX, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetIndex_Low(pos, value);
}
void incindex(addr pos, size_t value)
{
	Check(GetType(pos) != LISPTYPE_INDEX, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	IncIndex_Low(pos, value);
}
void decindex(addr pos, size_t value)
{
	Check(GetType(pos) != LISPTYPE_INDEX, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	DecIndex_Low(pos, value);
}

/* float */
int short_float_p(addr value)
{
	enum LISPTYPE type = GetType(value);
	return type == LISPTYPE_SHORT_FLOAT
		|| type == LISPTYPE_SINGLE_FLOAT;
}

int single_float_p(addr value)
{
	return GetType(value) == LISPTYPE_SINGLE_FLOAT;
}

void single_float_heap(addr *ret, single_float value)
{
	heap_body2(ret, LISPTYPE_SINGLE_FLOAT, sizeof(value));
	SetSingleFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}

void single_float_local(LocalRoot local, addr *ret, single_float value)
{
	Check(local == NULL, "local error");
	local_body2(local, ret, LISPTYPE_SINGLE_FLOAT, sizeof(value));
	SetSingleFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}

void single_float_alloc(LocalRoot local, addr *ret, single_float value)
{
	if (local)
		single_float_local(local, ret, value);
	else
		single_float_heap(ret, value);
}

const single_float *ptrsinglefloat(addr pos)
{
	Check(GetType(pos) != LISPTYPE_SINGLE_FLOAT, "type error");
	return PtrSingleFloat_Low(pos);
}
single_float refsinglefloat(addr pos)
{
	Check(GetType(pos) != LISPTYPE_SINGLE_FLOAT, "type error");
	return RefSingleFloat_Low(pos);
}
void getsinglefloat(addr pos, single_float *ret)
{
	Check(GetType(pos) != LISPTYPE_SINGLE_FLOAT, "type error");
	GetSingleFloat_Low(pos, ret);
}
void setsinglefloat(addr pos, single_float value)
{
	Check(GetType(pos) != LISPTYPE_SINGLE_FLOAT, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetSingleFloat_Low(pos, value);
}

int double_float_p(addr value)
{
	return GetType(value) == LISPTYPE_DOUBLE_FLOAT;
}

void double_float_heap(addr *ret, double_float value)
{
	heap_body2(ret, LISPTYPE_DOUBLE_FLOAT, sizeof(value));
	SetDoubleFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}

void double_float_local(LocalRoot local, addr *ret, double_float value)
{
	Check(local == NULL, "local error");
	local_body2(local, ret, LISPTYPE_DOUBLE_FLOAT, sizeof(value));
	SetDoubleFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}

void double_float_alloc(LocalRoot local, addr *ret, double_float value)
{
	if (local)
		double_float_local(local, ret, value);
	else
		double_float_heap(ret, value);
}

const double_float *ptrdoublefloat(addr pos)
{
	Check(GetType(pos) != LISPTYPE_DOUBLE_FLOAT, "type error");
	return PtrDoubleFloat_Low(pos);
}
double_float refdoublefloat(addr pos)
{
	Check(GetType(pos) != LISPTYPE_DOUBLE_FLOAT, "type error");
	return RefDoubleFloat_Low(pos);
}
void getdoublefloat(addr pos, double_float *ret)
{
	Check(GetType(pos) != LISPTYPE_DOUBLE_FLOAT, "type error");
	GetDoubleFloat_Low(pos, ret);
}
void setdoublefloat(addr pos, double_float value)
{
	Check(GetType(pos) != LISPTYPE_DOUBLE_FLOAT, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDoubleFloat_Low(pos, value);
}

int long_float_p(addr value)
{
	return GetType(value) == LISPTYPE_LONG_FLOAT;
}

void long_float_heap(addr *ret, long_float value)
{
	heap_body2(ret, LISPTYPE_LONG_FLOAT, sizeof(value));
	SetLongFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}

void long_float_local(LocalRoot local, addr *ret, long_float value)
{
	Check(local == NULL, "local error");
	local_body2(local, ret, LISPTYPE_LONG_FLOAT, sizeof(value));
	SetLongFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}

void long_float_alloc(LocalRoot local, addr *ret, long_float value)
{
	if (local)
		long_float_local(local, ret, value);
	else
		long_float_heap(ret, value);
}

const long_float *ptrlongfloat(addr pos)
{
	Check(GetType(pos) != LISPTYPE_LONG_FLOAT, "type error");
	return PtrLongFloat_Low(pos);
}
long_float reflongfloat(addr pos)
{
	Check(GetType(pos) != LISPTYPE_LONG_FLOAT, "type error");
	return RefLongFloat_Low(pos);
}
void getlongfloat(addr pos, long_float *ret)
{
	Check(GetType(pos) != LISPTYPE_LONG_FLOAT, "type error");
	GetLongFloat_Low(pos, ret);
}
void setlongfloat(addr pos, long_float value)
{
	Check(GetType(pos) != LISPTYPE_LONG_FLOAT, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetLongFloat_Low(pos, value);
}

/* inplace */
addr singleh(single_float value)
{
	addr pos;
	single_float_heap(&pos, value);
	return pos;
}

addr doubleh(double_float value)
{
	addr pos;
	double_float_heap(&pos, value);
	return pos;
}

addr longh(long_float value)
{
	addr pos;
	long_float_heap(&pos, value);
	return pos;
}


/* queue */
void queue_heap(addr *ret)
{
	consnil_heap(ret);
}
void queue_local(LocalRoot local, addr *ret)
{
	consnil_local(local, ret);
}
void queue_alloc(LocalRoot local, addr *ret)
{
	consnil_alloc(local, ret);
}

void pushqueue_alloc(LocalRoot local, addr pos, addr insert)
{
	addr right, make;

	GetCdr(pos, &right);
	consnil_alloc(local, &make);
	SetCar(make, insert);
	if (right == Nil) {
		SetCons(pos, make, make);
	}
	else {
		SetCdr(right, make);
		SetCdr(pos, make);
	}
}
void pushqueue_heap(addr pos, addr insert)
{
	pushqueue_alloc(NULL, pos, insert);
}
void pushqueue_local(LocalRoot local, addr pos, addr insert)
{
	Check(local == NULL, "localroot error");
	pushqueue_alloc(local, pos, insert);
}
void dotqueue(addr pos, addr right)
{
	addr cons;

	GetCdr(pos, &cons);
	if (cons == NULL) {
		Abort("dotqueue error");
		return;
	}
	SetCdr(cons, right);
}
void clearqueue(addr pos)
{
	SetCons(pos, Nil, Nil);
}

void rootqueue(addr pos, addr *ret)
{
	GetCar(pos, ret);
}
void tailqueue(addr pos, addr *ret)
{
	GetCdr(pos, ret);
}
int firstqueue(addr pos, addr *ret)
{
	addr cons;

	rootqueue(pos, &cons);
	if (cons == Nil) return 1;
	GetCar(cons, ret);

	return 0;
}
int lastqueue(addr pos, addr *ret)
{
	addr cons;

	tailqueue(pos, &cons);
	if (cons == Nil) return 1;
	GetCar(cons, ret);

	return 0;
}
int nthqueue(addr pos, size_t index, addr *ret)
{
	size_t i;
	addr right;

	GetCar(pos, &right);
	for (i = 0; i < index; i++) {
		GetCdr(right, &right);
		if (right == Nil) return 1;
	}
	GetCar(right, ret);

	return 0;
}


/************************************************************
 *  optimize.c
 ************************************************************/

void save_optimize_value(const struct optimize_struct *str,
		struct optimize_value *save)
{
	memcpy(save, &(str->value), sizeoft(struct optimize_value));
}
void rollback_optimize_value(struct optimize_struct *str,
		const struct optimize_value *save)
{
	memcpy(str->value.local, save->local, sizeoft(OptimizeType) * EVAL_OPTIMIZE_SIZE);
}

int optimize_declare_value(struct optimize_struct *str, enum EVAL_OPTIMIZE index)
{
	struct optimize_value *opt;

	opt = &(str->value);
	OptimizeType value;
	value = opt->local[index];
	return (value < 0)? opt->declaim[index]: value;
}
int optimize_speed_on(struct optimize_struct *str)
{
	/* (on -1 1 2 3) (off 0) */
	return optimize_declare_value(str, EVAL_OPTIMIZE_SPEED) != 0;
}
int optimize_evaltype(addr pos, EvalParse type)
{
	return eval_parse_p(pos) && RefEvalParseType(pos) == type;
}
int optimize_evaltype_on(struct optimize_struct *str, EvalParse type)
{
	return optimize_speed_on(str) && optimize_evaltype(str->pos, type);
}

static void optimize_initialize_declare(struct optimize_value *value)
{
	int i;

	copy_optimize_declare(value->declaim);
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++)
		value->local[i] = -1;
}

void optimize_initialize(struct optimize_struct *str, Execute ptr, addr pos)
{
	clearpoint(str);
	optimize_initialize_declare(&(str->value));
	str->ptr = ptr;
	str->local = ptr->local;
	copy_eval_parse_local(ptr->local, &(str->pos), pos);
}

int optimize_extract_(struct optimize_struct *str, optimize_call call)
{
	int update, check;

	update = 0;
	for (;;) {
		Return((*call)(str, &check));
		if (! check)
			break;
		update = 1;
	}
	str->update |= update;

	return 0;
}


/************************************************************
 *  optimize_call.c
 ************************************************************/

/*
 *  implicit
 */
#define Return_or_checkparse_implicit(call, str, pos, ret) { \
	Return(call(str, pos, ret)); \
	if (*ret) { \
		return 0;  \
	} \
}

#define Return_check_checkparse_implicit(call, str, pos, ret) { \
	Return(call(str, pos, ret)); \
	if (*ret == 0) { \
		return 0;  \
	} \
}

static int optimize_lisptype_on(OptimizeInfo *str, addr pos, enum LISPTYPE type)
{
	return optimize_speed_on(str) && GetType(pos) == type;
}

/* (10 20 30) -> (30) */
static int checkparse_implicit3_(OptimizeInfo *str, addr list, int *ret)
{
	addr value;

	if (! optimize_lisptype_on(str, list, LISPTYPE_CONS))
		return Result(ret, 0);
	GetCdr(list, &value);
	if (value == Nil)
		return Result(ret, 0);
	while (list != Nil) {
		GetCons(list, &value, &list);
		if (! optimize_value_and_function(value))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int optparse_implicit3_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	addr x;

	Return_check_checkparse_implicit(checkparse_implicit3_, str, list, ret);
	if (list == Nil)
		return Result(ret, 0);
	for (x = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
	}
	conscar_local(str->local, value, x);

	return Result(ret, 1);
}

/* (10 (call1) 20 30 (call2)) -> ((call1) (call2)) */
static int checkparse_implicit4_(OptimizeInfo *str, addr list, int *ret)
{
	int update1, update2, valuep;
	addr check;

	if (! optimize_lisptype_on(str, list, LISPTYPE_CONS))
		return Result(ret, 0);
	update1 = update2 = 0;
	while (list != Nil) {
		GetCons(list, &check, &list);
		valuep = optimize_value_and_function(check);
		if (list == Nil && ! valuep)
			update1 = 1;
		if (list != Nil && valuep)
			update2 = 1;
	}

	return Result(ret, update1 && update2);
}

static int optparse_implicit4_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	addr root, check;
	LocalRoot local;

	Return_check_checkparse_implicit(checkparse_implicit4_, str, list, ret);
	local = str->local;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &check, &list);
		if (! optimize_value_and_function(check))
			cons_local(local, &root, check, root);
	}
	nreverse(value, root);

	return Result(ret, 1);
}

/* (10 (call1) 20 (call2) 30 40) -> ((call1) (call2) 40) */
static int checkparse_implicit5_(OptimizeInfo *str, addr list, int *ret)
{
	int update1, update2, update3, valuep;
	addr check;

	if (! optimize_lisptype_on(str, list, LISPTYPE_CONS))
		return Result(ret, 0);
	update1 = update2 = update3 = 0;
	while (list != Nil) {
		GetCons(list, &check, &list);
		valuep = optimize_value_and_function(check);
		if (list == Nil && valuep)
			update1 = 1;
		if (list != Nil && valuep)
			update2 = 1;
		if (list != Nil && ! valuep)
			update3 = 1;
	}

	return Result(ret, update1 && update2 && update3);
}

static int optparse_implicit5_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	addr root, pos;
	LocalRoot local;

	Return_check_checkparse_implicit(checkparse_implicit5_, str, list, ret);
	local = str->local;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		if (list == Nil || ! optimize_value_and_function(pos))
			cons_local(local, &root, pos, root);
	}
	nreverse(value, root);

	return Result(ret, 1);
}

/* (x (progn y) z) -> (x y z) */
static int checkparse_implicit6_(OptimizeInfo *str, addr list, int *ret)
{
	addr pos;

	if (! optimize_lisptype_on(str, list, LISPTYPE_CONS))
		return Result(ret, 0);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (optimize_evaltype(pos, EVAL_PARSE_PROGN))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static void optparse_implicit6_next(LocalRoot local, addr list, addr *value)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (optimize_evaltype(pos, EVAL_PARSE_PROGN)) {
			GetEvalParse(pos, 1, &pos);
			optparse_implicit6_next(local, pos, value);
			continue;
		}
		cons_local(local, value, pos, *value);
	}
}

static int optparse_implicit6_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	addr root;

	Return_check_checkparse_implicit(checkparse_implicit6_, str, list, ret);
	root = Nil;
	optparse_implicit6_next(str->local, list, &root);
	nreverse(value, root);

	return Result(ret, 1);
}

/* (...) */
int checkparse_implicit_all_(OptimizeInfo *str, addr list, int *ret)
{
	int check;
	addr pos;

	/* Don't check optimize. */
	if (! consp(list))
		return Result(ret, 0);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(checkparse_inplace_(str, pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int optparse_implicit_all_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	int update, check;
	addr root, pos;
	LocalRoot local;

	Return_check_checkparse_implicit(checkparse_implicit_all_, str, list, ret);
	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		Return(optparse_inplace_(str, pos, &pos, &check));
		update |= check;
		cons_local(local, &root, pos, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(value, root);

	return Result(ret, 1);
}

/* implicit */
int checkparse_implicit_(OptimizeInfo *str, addr pos, int *ret)
{
	Return_or_checkparse_implicit(checkparse_implicit3_, str, pos, ret);
	Return_or_checkparse_implicit(checkparse_implicit4_, str, pos, ret);
	Return_or_checkparse_implicit(checkparse_implicit5_, str, pos, ret);
	Return_or_checkparse_implicit(checkparse_implicit6_, str, pos, ret);
	Return_or_checkparse_implicit(checkparse_implicit_all_, str, pos, ret);

	return Result(ret, 0);
}

#define Return_or_optparse_implicit(call, str, var, value, check) { \
	Return(call(str, var, &var, ret)); \
	if (*ret) { \
		*value = var; \
		return 0;  \
	} \
}
static int optparse_implicit_call_(OptimizeInfo *str, addr var, addr *value, int *ret)
{
	Return_or_optparse_implicit(optparse_implicit3_, str, var, value, check);
	Return_or_optparse_implicit(optparse_implicit4_, str, var, value, check);
	Return_or_optparse_implicit(optparse_implicit5_, str, var, value, check);
	Return_or_optparse_implicit(optparse_implicit6_, str, var, value, check);
	Return_or_optparse_implicit(optparse_implicit_all_, str, var, value, check);

	return Result(ret, 0);
}

int optparse_implicit_(OptimizeInfo *str, addr pos, addr *value, int *ret)
{
	int check, result;
	addr var;

	Return(checkparse_implicit_(str, pos, &check));
	if (! check) {
		*value = pos;
		return Result(ret, 0);
	}

	result = 0;
	var = pos;
	for (;;) {
		Return(optparse_implicit_call_(str, var, &var, &check));
		if (! check)
			break;
		result = 1;
	}
	if (result) {
		*value = var;
		return Result(ret, 1);
	}
	else {
		*value = pos;
		return Result(ret, 0);
	}
}


/*
 *  progn
 */
/* (progn) -> nil */
static int checkparse_progn1_(OptimizeInfo *str, int *ret)
{
	addr list;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);
	return Result(ret, list == Nil);
}

static int optparse_progn1_(OptimizeInfo *str, int *ret)
{
	Return_check_optparse(checkparse_progn1_, str, ret);
	eval_single_parse_local(str->local, &str->pos, EVAL_PARSE_NIL, Nil);
	return Result(ret, 1);
}

/* (progn x) -> x */
static int checkparse_progn2_(OptimizeInfo *str, int *ret)
{
	addr list;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);

	return Result(ret, singlep(list));
}

static int optparse_progn2_(OptimizeInfo *str, int *ret)
{
	addr list;

	Return_check_optparse(checkparse_progn2_, str, ret);
	GetEvalParse(str->pos, 1, &list);
	GetCar(list, &(str->pos));

	return Result(ret, 1);
}

/* (progn 10 20 30) -> 30 */
static int checkparse_progn3_(OptimizeInfo *str, int *ret)
{
	addr list;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);
	return checkparse_implicit3_(str, list, ret);
}

static int optparse_progn3_(OptimizeInfo *str, int *ret)
{
	int check;
	addr list;

	Return_check_optparse(checkparse_progn3_, str, ret);
	GetEvalParse(str->pos, 1, &list);
	Return(optparse_implicit3_(str, list, &list, &check));
	if (! check)
		return Result(ret, 0);
	GetCar(list, &list);
	str->pos = list;

	return Result(ret, 1);
}

/* (progn 10 (call1) 20 30 (call2)) -> (progn (call1) (call2)) */
static int checkparse_progn4_(OptimizeInfo *str, int *ret)
{
	addr list;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);
	return checkparse_implicit4_(str, list, ret);
}

static int optparse_progn4_(OptimizeInfo *str, int *ret)
{
	int check;
	addr form, list;

	Return_check_optparse(checkparse_progn4_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 1, &list);
	Return(optparse_implicit4_(str, list, &list, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse2_local(str->local, &list, EVAL_PARSE_PROGN, form, list);
	str->pos = list;

	return Result(ret, 1);
}

/* (progn 10 (call1) 20 (call2) 30 40) -> (progn (call1) (call2) 40) */
static int checkparse_progn5_(OptimizeInfo *str, int *ret)
{
	addr list;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);
	return checkparse_implicit5_(str, list, ret);
}

static int optparse_progn5_(OptimizeInfo *str, int *ret)
{
	int check;
	addr form, list;

	Return_check_optparse(checkparse_progn5_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 1, &list);
	Return(optparse_implicit5_(str, list, &list, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse2_local(str->local, &list, EVAL_PARSE_PROGN, form, list);
	str->pos = list;

	return Result(ret, 1);
}

/* (progn x (progn y) z) -> (progn x y z) */
static int checkparse_progn6_(OptimizeInfo *str, int *ret)
{
	addr list;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);
	return checkparse_implicit6_(str, list, ret);
}

static int optparse_progn6_(OptimizeInfo *str, int *ret)
{
	int check;
	addr form, list;

	Return_check_optparse(checkparse_progn6_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 1, &list);
	Return(optparse_implicit6_(str, list, &list, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse2_local(str->local, &list, EVAL_PARSE_PROGN, form, list);
	str->pos = list;

	return Result(ret, 1);
}

/* (progn ...) */
static int checkparse_progn_all_(OptimizeInfo *str, int *ret)
{
	addr list;

	/* Don't check optimize. */
	list = str->pos;
	if (! optimize_evaltype(list, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(list, 1, &list);
	return checkparse_implicit_all_(str, list, ret);
}

static int optparse_progn_all_(OptimizeInfo *str, int *ret)
{
	int ignore;
	addr form, list;

	Return_check_optparse(checkparse_progn_all_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 1, &list);
	Return(optparse_implicit_all_(str, list, &list, &ignore));
	eval_parse2_local(str->local, &list, EVAL_PARSE_PROGN, form, list);
	str->pos = list;

	return Result(ret, 1);
}


/* optparse-progn */
int checkparse_progn_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_progn1_, str, ret);
	Return_or_optparse(checkparse_progn2_, str, ret);
	Return_or_optparse(checkparse_progn3_, str, ret);
	Return_or_optparse(checkparse_progn4_, str, ret);
	Return_or_optparse(checkparse_progn5_, str, ret);
	Return_or_optparse(checkparse_progn6_, str, ret);
	Return_or_optparse(checkparse_progn_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_progn_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_progn1_));
	Return(optimize_extract_(str, optparse_progn2_));
	Return(optimize_extract_(str, optparse_progn3_));
	Return(optimize_extract_(str, optparse_progn4_));
	Return(optimize_extract_(str, optparse_progn5_));
	Return(optimize_extract_(str, optparse_progn6_));
	Return(optimize_extract_(str, optparse_progn_all_));

	return 0;
}
int optparse_progn_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_progn_run_);
}


/*
 *  let
 */
static int optimize_lettype(addr pos)
{
	EvalParse type;

	if (! eval_parse_p(pos))
		return 0;
	GetEvalParseType(pos, &type);
	return type == EVAL_PARSE_LET || type == EVAL_PARSE_LETA;
}

static int optimize_lettype_on(OptimizeInfo *str)
{
	return optimize_speed_on(str) && optimize_lettype(str->pos);
}

/* (let nil . nil) -> nil */
static int checkparse_let1_(OptimizeInfo *str, int *ret)
{
	addr pos, args, body;

	if (! optimize_lettype_on(str))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 1, &args); /* args */
	GetEvalParse(pos, 3, &body); /* body */

	return Result(ret, args == Nil && body == Nil);
}

static int optparse_let1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_let1_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* (let (aaa bbb (ccc)) . nil) -> nil */
static int checkparse_let2_(OptimizeInfo *str, int *ret)
{
	addr pos, args, body, x;

	if (! optimize_lettype_on(str))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 1, &args); /* args */
	if (args == Nil)
		return Result(ret, 0);
	GetEvalParse(pos, 3, &body); /* body */
	if (body != Nil)
		return Result(ret, 0);
	while (args != Nil) {
		GetCons(args, &x, &args);
		GetCdr(x, &x); /* (var . init) */
		if (RefEvalParseType(x) != EVAL_PARSE_NIL)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int optparse_let2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_let2_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* let-args */
static int checkparse_let_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, value;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_lettype(pos))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* args */
	while (pos != Nil) {
		GetCons(pos, &value, &pos);
		GetCdr(value, &value); /* (var . init) */
		Return(checkparse_inplace_(str, value, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int optparse_let_args_(OptimizeInfo *str, int *ret)
{
	int update, check;
	EvalParse type;
	addr pos, form, args, decl, body, var, init, root;
	LocalRoot local;

	Return_check_optparse(checkparse_let_args_, str, ret);
	pos = str->pos;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &body);

	local = str->local;
	update = 0;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		Return(optparse_inplace_(str, init, &init, &check));
		update |= check;
		cons_local(local, &var, var, init);
		cons_local(local, &root, var, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(&args, root);

	eval_parse_local(local, &pos, type, 4);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* let-body */
static int checkparse_let_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_lettype(pos))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &decl); /* decl */
	GetEvalParse(pos, 3, &body); /* body */
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_let_body_(OptimizeInfo *str, int *ret)
{
	int check;
	EvalParse type;
	addr pos, form, args, decl, body;

	Return_check_optparse(checkparse_let_body_, str, ret);
	pos = str->pos;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, type, 4);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-let */
int checkparse_let_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_let1_, str, ret);
	Return_or_optparse(checkparse_let2_, str, ret);
	Return_or_optparse(checkparse_let_args_, str, ret);
	Return_or_optparse(checkparse_let_body_, str, ret);

	return Result(ret, 0);
}

static int optparse_let_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_let1_));
	Return(optimize_extract_(str, optparse_let2_));
	Return(optimize_extract_(str, optparse_let_args_));
	Return(optimize_extract_(str, optparse_let_body_));

	return 0;
}
int optparse_let_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_let_run_);
}


/*
 *  setq
 */
/* (setq) -> nil */
static int checkparse_setq1_(OptimizeInfo *str, int *ret)
{
	addr list;

	if (! optimize_evaltype_on(str, EVAL_PARSE_SETQ))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);
	return Result(ret, list == Nil);
}

static int optparse_setq1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_setq1_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* setq-all */
static int checkparse_setq_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr list, x;

	/* Don't check optimize. */
	list = str->pos;
	if (! optimize_evaltype(list, EVAL_PARSE_SETQ))
		return Result(ret, 0);
	GetEvalParse(list, 1, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		GetCdr(x, &x); /* (var . expr) */
		Return(checkparse_inplace_(str, x, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int optparse_setq_all_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr form, list, root, var, expr;
	LocalRoot local;

	Return_check_optparse(checkparse_setq_all_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 1, &list);
	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		GetCons(var, &var, &expr);
		Return(optparse_inplace_(str, expr, &expr, &check));
		update |= check;
		cons_local(local, &var, var, expr);
		cons_local(local, &root, var, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(&list, root);
	eval_parse2_local(local, &list, EVAL_PARSE_SETQ, form, list);
	str->pos = list;

	return Result(ret, 1);
}

/* optparse-setq */
int checkparse_setq_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_setq1_, str, ret);
	Return_or_optparse(checkparse_setq_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_setq_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_setq1_));
	Return(optimize_extract_(str, optparse_setq_all_));

	return 0;
}
int optparse_setq_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_setq_run_);
}


/*
 *  destructuring-bind
 */
/* args */
int checkparse_destructuring_bind_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, expr, lambda;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DESTRUCTURING_BIND))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &lambda);

	Return(checkparse_inplace_(str, expr, &check));
	if (check)
		return Result(ret, 1);

	return checkparse_inplace_(str, lambda, ret);
}

int optparse_destructuring_bind_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, form, expr, lambda;

	Return_check_optparse(checkparse_destructuring_bind_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &lambda);

	update = 0;
	Return(optparse_inplace_(str, expr, &expr, &check));
	update |= check;
	Return(optparse_inplace_(str, lambda, &lambda, &check));
	update |= check;
	if (! update)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DESTRUCTURING_BIND, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, lambda);
	str->pos = pos;

	return Result(ret, 1);
}


/*
 *  if
 */
/* (if nil a b) -> b */
static int checkparse_if1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_IF))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &pos);
	return Result(ret, optimize_evaltype(pos, EVAL_PARSE_NIL));
}

static int optparse_if1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_if1_, str, ret);
	GetEvalParse(str->pos, 3, &pos);
	str->pos = pos;

	return Result(ret, 1);
}

/* (if x a b) -> a */
static int checkparse_if2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_IF))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &pos);
	if (optimize_evaltype(pos, EVAL_PARSE_NIL))
		return Result(ret, 0);

	return Result(ret, optimize_value_and_function(pos));
}

static int optparse_if2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_if2_, str, ret);
	GetEvalParse(str->pos, 2, &pos);
	str->pos = pos;

	return Result(ret, 1);
}

/* all */
static int checkparse_if_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, value;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_IF))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);
	GetEvalParse(pos, 2, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);
	GetEvalParse(pos, 3, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);

	return Result(ret, 0);
}

static int optparse_if_all_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, form, expr, ifthen, ifelse;

	Return_check_optparse(checkparse_if_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &ifthen);
	GetEvalParse(pos, 3, &ifelse);

	update = 0;
	/* expr */
	Return(optparse_inplace_(str, expr, &expr, &check));
	update |= check;
	/* then */
	Return(optparse_inplace_(str, ifthen, &ifthen, &check));
	update |= check;
	/* else */
	Return(optparse_inplace_(str, ifelse, &ifelse, &check));
	update |= check;
	/* result */
	if (! update)
		return Result(ret, 0);

	eval_parse_local(str->local, &pos, EVAL_PARSE_IF, 4);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, ifthen);
	SetEvalParse(pos, 3, ifelse);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-if */
int checkparse_if_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_if1_, str, ret);
	Return_or_optparse(checkparse_if2_, str, ret);
	Return_or_optparse(checkparse_if_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_if_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_if1_));
	Return(optimize_extract_(str, optparse_if2_));
	Return(optimize_extract_(str, optparse_if_all_));

	return 0;
}
int optparse_if_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_if_run_);
}


/*
 *  unwind-protect
 */
/* (unwind-protect value . tail) -> (progn ,@tail value) */
static int checkparse_unwind_protect1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_UNWIND_PROTECT))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &pos);
	return Result(ret, optimize_value_and_function(pos));
}

static int optparse_unwind_protect1_(OptimizeInfo *str, int *ret)
{
	addr pos, form, expr, list, root;
	LocalRoot local;

	Return_check_optparse(checkparse_unwind_protect1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &list);
	local = str->local;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		cons_local(local, &root, pos, root);
	}
	cons_local(local, &root, expr, root);
	nreverse(&list, root);
	/* progn */
	eval_parse2_local(local, &pos, EVAL_PARSE_PROGN, form, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* (unwind-protect form . all-value) -> form */
static int checkparse_unwind_protect2_(OptimizeInfo *str, int *ret)
{
	addr list, x;

	if (! optimize_evaltype_on(str, EVAL_PARSE_UNWIND_PROTECT))
		return Result(ret, 0);
	GetEvalParse(str->pos, 2, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		if (! optimize_value_and_function(x))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int optparse_unwind_protect2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_unwind_protect2_, str, ret);
	GetEvalParse(str->pos, 1, &pos); /* form */
	str->pos = pos;

	return Result(ret, 1);
}

/* all */
static int checkparse_unwind_protect_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, value;

	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_UNWIND_PROTECT))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);
	GetEvalParse(pos, 2, &value);
	return checkparse_implicit_all_(str, value, ret);
}

static int optparse_unwind_protect_all_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, form, expr, list;

	Return_check_optparse(checkparse_unwind_protect_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &list);
	update = 0;
	/* expr */
	Return(optparse_inplace_(str, expr, &expr, &check));
	update |= check;
	/* list */
	Return(optparse_implicit_all_(str, list, &list, &check));
	update |= check;

	/* update */
	if (! update)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_UNWIND_PROTECT, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-unwind-protect */
int checkparse_unwind_protect_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_unwind_protect1_, str, ret);
	Return_or_optparse(checkparse_unwind_protect2_, str, ret);
	Return_or_optparse(checkparse_unwind_protect_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_unwind_protect_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_unwind_protect1_));
	Return(optimize_extract_(str, optparse_unwind_protect2_));
	Return(optimize_extract_(str, optparse_unwind_protect_all_));

	return 0;
}
int optparse_unwind_protect_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_unwind_protect_run_);
}


/*
 *  tagbody
 */
/* (tagbody) -> nil */
static int checkparse_tagbody1_(OptimizeInfo *str, int *ret)
{
	addr pos, tag, body;
	size_t size1, size2;

	if (! optimize_evaltype_on(str, EVAL_PARSE_TAGBODY))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 1, &tag);
	GetEvalParse(pos, 2, &body);
	if (body == Nil)
		return Result(ret, 1);
	size1 = length_list_unsafe(tag);
	size2 = length_list_unsafe(body);

	return Result(ret, size1 == size2);
}

static int optparse_tagbody1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_tagbody1_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* (tagbody (call) (call2)) -> (progn (call) (call2) nil) */
static int checkparse_tagbody2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_TAGBODY))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &pos);
	return Result(ret, pos == Nil);
}

static int optparse_tagbody2_(OptimizeInfo *str, int *ret)
{
	addr pos, root, form, list;
	LocalRoot local;

	Return_check_optparse(checkparse_tagbody2_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 2, &list);
	local = str->local;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		cons_local(local, &root, pos, root);
	}
	eval_single_parse_local(local, &pos, EVAL_PARSE_NIL, Nil);
	cons_local(local, &root, pos, root);
	nreverse(&root, root);
	/* progn */
	eval_parse2_local(local, &pos, EVAL_PARSE_PROGN, form, root);
	str->pos = pos;

	return Result(ret, 1);
}

/* all */
static int checkparse_tagbody_all_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_TAGBODY))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &pos);
	/* Don't use checkparse_implicit. */
	return checkparse_implicit_all_(str, pos, ret);
}

static int optparse_tagbody_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, tag, body;

	Return_check_optparse(checkparse_tagbody_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &tag);
	GetEvalParse(pos, 2, &body);

	Return(optparse_implicit_all_(str, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_TAGBODY, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, tag);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-tagbody */
int checkparse_tagbody_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_tagbody1_, str, ret);
	Return_or_optparse(checkparse_tagbody2_, str, ret);
	Return_or_optparse(checkparse_tagbody_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_tagbody_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_tagbody1_));
	Return(optimize_extract_(str, optparse_tagbody2_));
	Return(optimize_extract_(str, optparse_tagbody_all_));

	return 0;
}
int optparse_tagbody_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_tagbody_run_);
}


/*
 *  block / return-from
 */
/* (block name) -> nil */
static int checkparse_block1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_BLOCK))
		return Result(ret, 0);
	GetEvalParse(str->pos, 2, &pos);
	return Result(ret, pos == Nil);
}

static int optparse_block1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_block1_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* (block name ... x) -> x */
static int checkparse_block2_(OptimizeInfo *str, int *ret)
{
	addr list, check;

	if (! optimize_evaltype_on(str, EVAL_PARSE_BLOCK))
		return Result(ret, 0);
	GetEvalParse(str->pos, 2, &list);
	if (list == Nil)
		return Result(ret, 0);
	while (list != Nil) {
		GetCons(list, &check, &list);
		if (! optimize_value_and_function(check))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int optparse_block2_(OptimizeInfo *str, int *ret)
{
	addr list, x;

	Return_check_optparse(checkparse_block2_, str, ret);
	GetEvalParse(str->pos, 2, &list);
	if (list == Nil)
		return Result(ret, 0);
	x = str->pos;
	while (list != Nil) {
		GetCons(list, &x, &list);
	}
	str->pos = x;

	return Result(ret, 1);
}

/* all */
static int checkparse_block_all_(OptimizeInfo *str, int *ret)
{
	addr list;

	/* Don't check optimize. */
	list = str->pos;
	if (! optimize_evaltype(list, EVAL_PARSE_BLOCK))
		return Result(ret, 0);
	GetEvalParse(list, 2, &list);
	return checkparse_implicit_declare_(str, Nil, list, ret);
}

static int optparse_block_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, name, body;

	Return_check_optparse(checkparse_block_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &name);
	GetEvalParse(pos, 2, &body);

	Return(optparse_implicit_declare_(str, Nil, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_BLOCK, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, name);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* (return-from name expr) -> (return-from name expr) */
static int checkparse_return_from_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_RETURN_FROM))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &pos);
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_return_from_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, name, expr;

	Return_check_optparse(checkparse_return_from_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &name);
	GetEvalParse(pos, 2, &expr);

	Return(optparse_inplace_(str, expr, &expr, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_RETURN_FROM, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, name);
	SetEvalParse(pos, 2, expr);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-block */
int checkparse_block_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_block1_, str, ret);
	Return_or_optparse(checkparse_block2_, str, ret);
	Return_or_optparse(checkparse_block_all_, str, ret);
	Return_or_optparse(checkparse_return_from_, str, ret);

	return Result(ret, 0);
}

static int optparse_block_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_block1_));
	Return(optimize_extract_(str, optparse_block2_));
	Return(optimize_extract_(str, optparse_block_all_));
	Return(optimize_extract_(str, optparse_return_from_));

	return 0;
}
int optparse_block_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_block_run_);
}


/*
 *  catch / throw
 */
/* (catch name) -> (progn name nil) */
static int checkparse_catch1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_CATCH))
		return Result(ret, 0);
	GetEvalParse(str->pos, 2, &pos);
	return Result(ret, pos == Nil);
}

static int optparse_catch1_(OptimizeInfo *str, int *ret)
{
	addr pos, form, name, list;
	LocalRoot local;

	Return_check_optparse(checkparse_catch1_, str, ret);
	/* (name nil) */
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 1, &name);
	local = str->local;
	eval_single_parse_local(local, &pos, EVAL_PARSE_NIL, Nil);
	conscar_local(local, &list, pos);
	cons_local(local, &list, name, list);
	/* (progn name nil) */
	eval_parse2_local(local, &pos, EVAL_PARSE_PROGN, form, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* (catch name ... x) -> (progn name x) */
static int checkparse_catch2_(OptimizeInfo *str, int *ret)
{
	addr pos, check;

	if (! optimize_evaltype_on(str, EVAL_PARSE_CATCH))
		return Result(ret, 0);
	GetEvalParse(str->pos, 2, &pos);
	if (pos == Nil)
		return Result(ret, 0);
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		if (! optimize_value_and_function(check))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int optparse_catch2_(OptimizeInfo *str, int *ret)
{
	addr pos, form, name, list;
	LocalRoot local;

	Return_check_optparse(checkparse_catch2_, str, ret);
	/* (name lastcar) */
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &name);
	GetEvalParse(pos, 2, &list);
	if (list == Nil)
		return Result(ret, 0);
	for (pos = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
	}
	local = str->local;
	conscar_local(local, &list, pos);
	cons_local(local, &list, name, list);
	/* (progn name lastcar) */
	eval_parse2_local(local, &pos, EVAL_PARSE_PROGN, form, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* all */
static int checkparse_catch_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, value;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_CATCH))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);
	GetEvalParse(pos, 2, &value);
	return checkparse_implicit_declare_(str, Nil, value, ret);
}

static int optparse_catch_all_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, form, name, list;

	Return_check_optparse(checkparse_catch_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &name);
	GetEvalParse(pos, 2, &list);

	update = 0;
	/* name */
	Return(optparse_inplace_(str, name, &name, &check));
	update |= check;
	/* list */
	Return(optparse_implicit_declare_(str, Nil, list, &list, &check));
	update |= check;
	/* result */
	if (! update)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_CATCH, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, name);
	SetEvalParse(pos, 2, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* throw */
static int checkparse_throw_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, value;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_THROW))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);
	GetEvalParse(pos, 2, &value);
	return checkparse_inplace_(str, value, ret);
}

static int optparse_throw_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, form, name, expr;

	Return_check_optparse(checkparse_throw_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &name);
	GetEvalParse(pos, 2, &expr);

	update = 0;
	/* name */
	Return(optparse_inplace_(str, name, &name, &check));
	update |= check;
	/* expr */
	Return(optparse_inplace_(str, expr, &expr, &check));
	update |= check;
	/* result */
	if (! update)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_THROW, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, name);
	SetEvalParse(pos, 2, expr);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-catch */
int checkparse_catch_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_catch1_, str, ret);
	Return_or_optparse(checkparse_catch2_, str, ret);
	Return_or_optparse(checkparse_catch_all_, str, ret);
	Return_or_optparse(checkparse_throw_, str, ret);

	return Result(ret, 0);
}

static int optparse_catch_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_catch1_));
	Return(optimize_extract_(str, optparse_catch2_));
	Return(optimize_extract_(str, optparse_catch_all_));
	Return(optimize_extract_(str, optparse_throw_));

	return 0;
}
int optparse_catch_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_catch_run_);
}


/*
 *  the
 */
/* (the type expr) -> (the [type] expr) */
static int checkparse_the1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_THE))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &pos); /* type */

	/* type-error */
	Return(check_delay_type_(str->ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	/* return ! type_optimized_or_subtypep(pos); */
	return Result(ret, ! type_optimized_p(pos));
}

static int optparse_the1_(OptimizeInfo *str, int *ret)
{
	int ignore;
	addr pos, form, type, expr;
	LocalRoot local;

	Return_check_optparse(checkparse_the1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &type);
	GetEvalParse(pos, 2, &expr);

	local = str->local;
	Return(type_optimize_local_(local, type, &type, &ignore));
	eval_parse_local(local, &pos, EVAL_PARSE_THE, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, type);
	SetEvalParse(pos, 2, expr);
	str->pos = pos;

	return Result(ret, 1);
}

/* expr */
static int checkparse_the2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_THE))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &pos); /* expr */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_the2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, type, expr;

	Return_check_optparse(checkparse_the2_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &type);
	GetEvalParse(pos, 2, &expr);

	Return(optparse_inplace_(str, expr, &expr, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_THE, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, type);
	SetEvalParse(pos, 2, expr);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-the */
int checkparse_the_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_the1_, str, ret);
	Return_or_optparse(checkparse_the2_, str, ret);

	return Result(ret, 0);
}

static int optparse_the_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_the1_));
	Return(optimize_extract_(str, optparse_the2_));

	return 0;
}
int optparse_the_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_the_run_);
}


/*
 *  eval-when
 */
/* (eval-when cons) -> nil */
static int checkparse_eval_when1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_EVAL_WHEN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &pos); /* body */
	return Result(ret, pos == Nil);
}

static int optparse_eval_when1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_eval_when1_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* all */
static int checkparse_eval_when_all_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_EVAL_WHEN))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* body */
	return checkparse_implicit_declare_(str, Nil, pos, ret);
}

static int optparse_eval_when_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, cons, compile, load, exec, toplevel, mode;

	Return_check_optparse(checkparse_eval_when_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &cons);
	GetEvalParse(pos, 2, &compile);
	GetEvalParse(pos, 3, &load);
	GetEvalParse(pos, 4, &exec);
	GetEvalParse(pos, 5, &toplevel);
	GetEvalParse(pos, 6, &mode);

	Return(optparse_implicit_declare_(str, Nil, cons, &cons, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_EVAL_WHEN, 7);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, cons);
	SetEvalParse(pos, 2, compile);
	SetEvalParse(pos, 3, load);
	SetEvalParse(pos, 4, exec);
	SetEvalParse(pos, 5, toplevel);
	SetEvalParse(pos, 6, mode);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-eval-when */
int checkparse_eval_when_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_eval_when1_, str, ret);
	Return_or_optparse(checkparse_eval_when_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_eval_when_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_eval_when1_));
	Return(optimize_extract_(str, optparse_eval_when_all_));

	return 0;
}
int optparse_eval_when_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_eval_when_run_);
}


/*
 *  locally
 */
/* (locally ...) -> (progn ...) */
static int checkparse_locally1_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	if (! optimize_evaltype_on(str, EVAL_PARSE_LOCALLY))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 2, &body); /* body */
	return Result(ret, empty_nil_declare(decl) && body != Nil);
}

static int optparse_locally1_(OptimizeInfo *str, int *ret)
{
	addr form, pos;

	Return_check_optparse(checkparse_locally1_, str, ret);
	GetEvalParse(str->pos, 0, &form); /* form */
	GetEvalParse(str->pos, 2, &pos); /* body */
	eval_parse2_local(str->local, &pos, EVAL_PARSE_PROGN, form, pos);
	str->pos = pos;

	return Result(ret, 1);
}

/* (locally (declare ...)) -> nil */
static int checkparse_locally2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_LOCALLY))
		return Result(ret, 0);
	GetEvalParse(str->pos, 2, &pos); /* body */
	return Result(ret, pos == Nil);
}

static int optparse_locally2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_locally2_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* all */
static int checkparse_locally_all_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_LOCALLY))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_locally_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, decl, body;

	Return_check_optparse(checkparse_locally_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_LOCALLY, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-locally */
int checkparse_locally_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_locally1_, str, ret);
	Return_or_optparse(checkparse_locally2_, str, ret);
	Return_or_optparse(checkparse_locally_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_locally_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_locally1_));
	Return(optimize_extract_(str, optparse_locally2_));
	Return(optimize_extract_(str, optparse_locally_all_));

	return 0;
}
int optparse_locally_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_locally_run_);
}


/*
 *  call
 */
/* first argument */
static int checkparse_call1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_CALL))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* call */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_call1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, call, cons;

	Return_check_optparse(checkparse_call1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &call);
	GetEvalParse(pos, 2, &cons);

	Return(optparse_inplace_(str, call, &call, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_CALL, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, call);
	SetEvalParse(pos, 2, cons);
	str->pos = pos;

	return Result(ret, 1);
}

/* all */
static int checkparse_call_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr list, value;

	/* Don't check optimize. */
	list = str->pos;
	if (! optimize_evaltype(list, EVAL_PARSE_CALL))
		return Result(ret, 0);
	GetEvalParse(list, 2, &list); /* cons */
	while (list != Nil) {
		GetCons(list, &value, &list);
		Return(checkparse_inplace_(str, value, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int optparse_call_all_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, form, call, list, root, x;
	LocalRoot local;

	Return_check_optparse(checkparse_call_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &call);
	GetEvalParse(pos, 2, &list);

	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		Return(optparse_inplace_(str, x, &x, &check));
		update |= check;
		cons_local(local, &root, x, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(&list, root);

	eval_parse_local(str->local, &pos, EVAL_PARSE_CALL, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, call);
	SetEvalParse(pos, 2, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-call */
int checkparse_call_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_call1_, str, ret);
	Return_or_optparse(checkparse_call_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_call_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_call1_));
	Return(optimize_extract_(str, optparse_call_all_));

	return 0;
}
int optparse_call_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_call_run_);
}


/*
 *  progv
 */
/* symbols */
static int checkparse_progv1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_PROGV))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* symbols */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_progv1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, symbols, values, body;

	Return_check_optparse(checkparse_progv1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &symbols);
	GetEvalParse(pos, 2, &values);
	GetEvalParse(pos, 3, &body);

	Return(optparse_inplace_(str, symbols, &symbols, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_PROGV, 4);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, symbols);
	SetEvalParse(pos, 2, values);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* values */
static int checkparse_progv2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_PROGV))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &pos); /* values */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_progv2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, symbols, values, body;

	Return_check_optparse(checkparse_progv2_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &symbols);
	GetEvalParse(pos, 2, &values);
	GetEvalParse(pos, 3, &body);

	Return(optparse_inplace_(str, values, &values, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_PROGV, 4);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, symbols);
	SetEvalParse(pos, 2, values);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_progv3_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_PROGV))
		return Result(ret, 0);
	GetEvalParse(pos, 3, &pos); /* body */
	return checkparse_implicit_all_(str, pos, ret);
}

static int optparse_progv3_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, symbols, values, body;

	Return_check_optparse(checkparse_progv3_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &symbols);
	GetEvalParse(pos, 2, &values);
	GetEvalParse(pos, 3, &body);

	Return(optparse_implicit_all_(str, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_PROGV, 4);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, symbols);
	SetEvalParse(pos, 2, values);
	SetEvalParse(pos, 3, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-progv */
int checkparse_progv_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_progv1_, str, ret);
	Return_or_optparse(checkparse_progv2_, str, ret);
	Return_or_optparse(checkparse_progv3_, str, ret);

	return Result(ret, 0);
}

static int optparse_progv_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_progv1_));
	Return(optimize_extract_(str, optparse_progv2_));
	Return(optimize_extract_(str, optparse_progv3_));

	return 0;
}
int optparse_progv_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_progv_run_);
}


/************************************************************
 *  optimize_common.c
 ************************************************************/

static OptimizeType get_optimize_scope(addr scope, enum EVAL_OPTIMIZE index)
{
	OptimizeType value;
	addr root;

	value = StructEvalScope(scope)->optimize[index];
	if (0 <= value)
		return value;
	getroot_declare(&root);
	return get_optimize_declare(root, index);
}

static int optimize_common_p(addr scope)
{
	OptimizeType value;
	value = get_optimize_scope(scope, EVAL_OPTIMIZE_SPEED);
	return 1 <= value;
}


/*
 *  result-type
 */
static int optcode_result_type_code(Execute ptr, CodeValue x)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return(typep_clang_(ptr, pos, x.pos, &check));
	if (! check)
		return call_type_error_(ptr, pos, x.pos);

	return 0;
}


/*
 *  car0
 */
static int optcode_car0_set_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	GetCar(pos, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static int optcode_car0_push_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	GetCar(pos, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

static int optimize_common_carcdr0_const_(CodeMake ptr,
		addr pos, int *ret, constindex index)
{
	addr escape;

	/* expr */
	code_queue_make_label(ptr, &escape);
	Return(code_make_execute_set_(ptr, pos));
	code_jump_escape_wake(ptr, escape);
	/* operator */
	code_queue_single(ptr, index);
	code_queue_push_label(ptr, escape);

	return Result(ret, 1);
}

static int optimize_common_carcdr0_rem_(CodeMake ptr, addr pos, int *ret)
{
	Return(code_make_execute_rem_(ptr, pos));
	return Result(ret, 1);
}

static int optimize_common_car0_(CodeMake ptr, addr pos, int *ret)
{
	/* no check */
	getvalue_tablecall(pos, &pos);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			return optimize_common_carcdr0_const_(ptr, pos, ret,
					CONSTANT_CODE_OPTCODE_CAR0_SET);

		case CodeQueue_ModePush:
			return optimize_common_carcdr0_const_(ptr, pos, ret,
					CONSTANT_CODE_OPTCODE_CAR0_PUSH);

		case CodeQueue_ModeRemove:
		default:
			return optimize_common_carcdr0_rem_(ptr, pos, ret);
	}
}


/*
 *  car1
 */
static int optcode_car1_set_code(Execute ptr, CodeValue x)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return(typep_clang_(ptr, pos, x.pos, &check));
	if (! check)
		return call_type_error_(ptr, pos, x.pos);
	GetCar(pos, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static int optcode_car1_push_code(Execute ptr, CodeValue x)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return(typep_clang_(ptr, pos, x.pos, &check));
	if (! check)
		return call_type_error_(ptr, pos, x.pos);
	GetCar(pos, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

static int optimize_common_car1_const_(CodeMake ptr,
		addr pos, int *ret, constindex index)
{
	addr type, escape;

	gettype_tablecall(pos, &type);
	getvalue_tablecall(pos, &pos);
	/* expr */
	code_queue_make_label(ptr, &escape);
	Return(code_make_execute_set_(ptr, pos));
	code_jump_escape_wake(ptr, escape);
	/* operator */
	code_queue_cons(ptr, index, type);
	code_queue_push_label(ptr, escape);

	return Result(ret, 1);
}

static int optimize_common_car1_(CodeMake ptr, addr pos, int *ret)
{
	/* type check */
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			return optimize_common_car1_const_(ptr, pos, ret,
					CONSTANT_CODE_OPTCODE_CAR1_SET);

		case CodeQueue_ModePush:
			return optimize_common_car1_const_(ptr, pos, ret,
					CONSTANT_CODE_OPTCODE_CAR1_PUSH);

		case CodeQueue_ModeRemove:
		default:
			return optimize_common_car1_const_(ptr, pos, ret,
					CONSTANT_CODE_OPTCODE_RESULT_TYPE);
	}
}


/*
 *  car
 */
static int optimize_common_car_(CodeMake ptr, addr scope, int *ret)
{
	addr args, pos;

	if (! optimize_common_p(scope))
		return Result(ret, 0);
	/* first argument */
	GetEvalScopeIndex(scope, 1, &args);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (args != Nil)
		return Result(ret, 0);
	Check(! eval_tablecall_p(pos), "type error");
	if (! getcheck_tablecall(pos))
		return optimize_common_car0_(ptr, pos, ret);
	else
		return optimize_common_car1_(ptr, pos, ret);
}


/*
 *  cdr0
 */
static int optcode_cdr0_set_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	GetCdr(pos, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static int optcode_cdr0_push_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	GetCdr(pos, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

static int optimize_common_cdr0_(CodeMake ptr, addr pos, int *ret)
{
	/* no check */
	getvalue_tablecall(pos, &pos);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			return optimize_common_carcdr0_const_(ptr, pos, ret,
					CONSTANT_CODE_OPTCODE_CDR0_SET);

		case CodeQueue_ModePush:
			return optimize_common_carcdr0_const_(ptr, pos, ret,
					CONSTANT_CODE_OPTCODE_CDR0_PUSH);

		case CodeQueue_ModeRemove:
		default:
			return optimize_common_carcdr0_rem_(ptr, pos, ret);
	}
}


/*
 *  cdr1
 */
static int optcode_cdr1_set_code(Execute ptr, CodeValue x)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return(typep_clang_(ptr, pos, x.pos, &check));
	if (! check)
		return call_type_error_(ptr, pos, x.pos);
	GetCdr(pos, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static int optcode_cdr1_push_code(Execute ptr, CodeValue x)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return(typep_clang_(ptr, pos, x.pos, &check));
	if (! check)
		return call_type_error_(ptr, pos, x.pos);
	GetCdr(pos, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

static int optimize_common_cdr1_(CodeMake ptr, addr pos, int *ret)
{
	/* type check */
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			return optimize_common_car1_const_(ptr, pos, ret,
					CONSTANT_CODE_OPTCODE_CDR1_SET);

		case CodeQueue_ModePush:
			return optimize_common_car1_const_(ptr, pos, ret,
					CONSTANT_CODE_OPTCODE_CDR1_PUSH);

		case CodeQueue_ModeRemove:
		default:
			return optimize_common_car1_const_(ptr, pos, ret,
					CONSTANT_CODE_OPTCODE_RESULT_TYPE);
	}
}


/*
 *  cdr
 */
static int optimize_common_cdr_(CodeMake ptr, addr scope, int *ret)
{
	addr args, pos;

	if (! optimize_common_p(scope))
		return Result(ret, 0);
	/* first argument */
	GetEvalScopeIndex(scope, 1, &args);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (args != Nil)
		return Result(ret, 0);
	Check(! eval_tablecall_p(pos), "type error");
	if (! getcheck_tablecall(pos))
		return optimize_common_cdr0_(ptr, pos, ret);
	else
		return optimize_common_cdr1_(ptr, pos, ret);
}


/*
 *  cons
 */
static int optcode_cons_code(Execute ptr, CodeValue x)
{
	addr list, car, cdr;

	getargs_list_control_unsafe(ptr, 0, &list);
	GetCons(list, &car, &list);
	GetCar(list, &cdr);
	cons_heap(&list, car, cdr);
	setresult_control(ptr, list);

	return 0;
}

static int optimize_common_cons0_(CodeMake ptr, addr car, addr cdr, int *ret)
{
	addr escape, finish;
	fixnum id;

	getvalue_tablecall(car, &car);
	getvalue_tablecall(cdr, &cdr);

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* arguments */
	Return(code_make_execute_push_(ptr, car));
	code_jump_escape_wake(ptr, escape);
	Return(code_make_execute_push_(ptr, cdr));
	code_jump_escape_wake(ptr, escape);

	/* cons */
	CodeQueue_single(ptr, OPTCODE_CONS);
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);
	code_queue_goto(ptr, finish);

	/* end */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_push_label(ptr, finish);

	return Result(ret, 1);
}

static int optimize_common_cons_rem_(CodeMake ptr, addr car, addr cdr, int *ret)
{
	addr escape;

	getvalue_tablecall(car, &car);
	getvalue_tablecall(cdr, &cdr);
	/* arguments */
	code_queue_make_label(ptr, &escape);
	Return(code_make_execute_(ptr, car));
	code_jump_escape_wake(ptr, escape);
	Return(code_make_execute_(ptr, cdr));
	code_queue_push_label(ptr, escape);

	return Result(ret, 1);
}

static int optimize_common_cons_(CodeMake ptr, addr scope, int *ret)
{
	addr args, car, cdr;

	if (! optimize_common_p(scope))
		return Result(ret, 0);
	/* first argument */
	GetEvalScopeIndex(scope, 1, &args);
	if (! consp_getcons(args, &car, &args))
		return Result(ret, 0);
	if (args == Nil)
		return Result(ret, 0);
	if (! consp_getcons(args, &cdr, &args))
		return Result(ret, 0);
	if (args != Nil)
		return Result(ret, 0);
	Check(! eval_tablecall_p(car), "type error");
	Check(! eval_tablecall_p(cdr), "type error");
	if (! code_queue_remp(ptr))
		return optimize_common_cons0_(ptr, car, cdr, ret);
	else
		return optimize_common_cons_rem_(ptr, car, cdr, ret);
}


/*
 *  optimize-common
 */
static void optimize_common_symbol(addr scope, addr *ret)
{
	addr call;

	GetEvalScopeIndex(scope, 0, &call); /* first */
	Check(RefEvalScopeType(call) != EVAL_PARSE_FUNCTION, "type error");
	GetEvalScopeIndex(call, 0, &call);
	getname_tablefunction(call, &call);
	Check(RefCallNameType(call) != CALLNAME_SYMBOL, "callname error");
	GetCallName(call, ret);
}

int optimize_common_(CodeMake ptr, addr scope, int *ret)
{
	addr symbol, check;

	optimize_common_symbol(scope, &symbol);
	/* car */
	GetConst(COMMON_CAR, &check);
	if (symbol == check)
		return optimize_common_car_(ptr, scope, ret);
	/* cdr */
	GetConst(COMMON_CDR, &check);
	if (symbol == check)
		return optimize_common_cdr_(ptr, scope, ret);
	/* cons */
	GetConst(COMMON_CONS, &check);
	if (symbol == check)
		return optimize_common_cons_(ptr, scope, ret);

	return Result(ret, 0);
}


/*
 *  optimize-check code
 */
static int optimize_check_code1_(CodeMake ptr, addr scope, addr pos, int *ret)
{
	Check(! eval_tablecall_p(pos), "type error");
	getvalue_tablecall(pos, &pos);
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeValue(pos, &pos);
	if (! strvect_designator_equalp_char(pos, "SCOPE"))
		return Result(ret, 0);

	/* fixnum */
	fixnum_heap(&pos, (fixnum)optimize_common_p(scope));
	code_make_object(ptr, pos);

	return Result(ret, 1);
}

static int optimize_check_code2_(CodeMake ptr,
		addr scope, addr pos, addr args, int *ret)
{
	addr list, symbol, value;

	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (args != Nil)
		return Result(ret, 0);
	Check(! eval_tablecall_p(pos), "type error");
	getvalue_tablecall(pos, &pos);
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeValue(pos, &pos);
	if (! strvect_designator_equalp_char(pos, "LIST"))
		return Result(ret, 0);

	list = Nil;
	/* compilation-speed */
	GetConst(COMMON_COMPILATION_SPEED, &symbol);
	fixnum_heap(&value, (fixnum)get_optimize_scope(scope, EVAL_OPTIMIZE_COMPILATION));
	cons_heap(&value, symbol, value);
	cons_heap(&list, value, list);
	/* debug */
	GetConst(COMMON_DEBUG, &symbol);
	fixnum_heap(&value, (fixnum)get_optimize_scope(scope, EVAL_OPTIMIZE_DEBUG));
	cons_heap(&value, symbol, value);
	cons_heap(&list, value, list);
	/* safety */
	GetConst(COMMON_SAFETY, &symbol);
	fixnum_heap(&value, (fixnum)get_optimize_scope(scope, EVAL_OPTIMIZE_SAFETY));
	cons_heap(&value, symbol, value);
	cons_heap(&list, value, list);
	/* space */
	GetConst(COMMON_SPACE, &symbol);
	fixnum_heap(&value, (fixnum)get_optimize_scope(scope, EVAL_OPTIMIZE_SPACE));
	cons_heap(&value, symbol, value);
	cons_heap(&list, value, list);
	/* speed */
	GetConst(COMMON_SPEED, &symbol);
	fixnum_heap(&value, (fixnum)get_optimize_scope(scope, EVAL_OPTIMIZE_SPEED));
	cons_heap(&value, symbol, value);
	cons_heap(&list, value, list);
	/* object */
	code_make_object(ptr, list);

	return Result(ret, 1);
}

int optimize_check_code_(CodeMake ptr, addr scope, int *ret)
{
	addr args, pos;

	/* (lisp-system:optimize-check scope) */
	GetEvalScopeIndex(scope, 1, &args);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (args == Nil)
		return optimize_check_code1_(ptr, scope, pos, ret);
	else
		return optimize_check_code2_(ptr, scope, pos, args, ret);
}


/*
 *  initialize
 */
#define defcode(x,y) defcode_constant(CONSTANT_CODE_##x, p_##y)
#define initcode(x,y) { \
	SetPointer_code(p_##x, x); \
	CodeValueArray[p_##x] = (byte)CodeValueType_##y; \
}

void init_optimize_common(void)
{
	initcode(optcode_result_type_code,  Addr);
	initcode(optcode_car0_set_code,     Null);
	initcode(optcode_car0_push_code,    Null);
	initcode(optcode_car1_set_code,     Addr);
	initcode(optcode_car1_push_code,    Addr);
	initcode(optcode_cdr0_set_code,     Null);
	initcode(optcode_cdr0_push_code,    Null);
	initcode(optcode_cdr1_set_code,     Addr);
	initcode(optcode_cdr1_push_code,    Addr);
	initcode(optcode_cons_code,         Null);
}

void build_optimize_common(void)
{
	defcode(OPTCODE_RESULT_TYPE,  optcode_result_type_code);
	defcode(OPTCODE_CAR0_SET,     optcode_car0_set_code);
	defcode(OPTCODE_CAR0_PUSH,    optcode_car0_push_code);
	defcode(OPTCODE_CAR1_SET,     optcode_car1_set_code);
	defcode(OPTCODE_CAR1_PUSH,    optcode_car1_push_code);
	defcode(OPTCODE_CDR0_SET,     optcode_cdr0_set_code);
	defcode(OPTCODE_CDR0_PUSH,    optcode_cdr0_push_code);
	defcode(OPTCODE_CDR1_SET,     optcode_cdr1_set_code);
	defcode(OPTCODE_CDR1_PUSH,    optcode_cdr1_push_code);
	defcode(OPTCODE_CONS,         optcode_cons_code);
}

#undef defcode
#undef initcode


/************************************************************
 *  optimize_define.c
 ************************************************************/

/*
 *  lambda-ordinary
 */
/* &optional */
static int checkparse_opt_(OptimizeInfo *str, addr list, int *ret)
{
	int check;
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCdr(pos, &pos); /* var */
		GetCar(pos, &pos); /* init */
		Return(checkparse_inplace_(str, pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int optparse_opt_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	int update, check;
	addr root, x, var, init, svar;
	LocalRoot local;

	/* opt -> (var init svar) */
	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		List_bind(x, &var, &init, &svar, NULL);
		Return(optparse_inplace_(str, init, &init, &check));
		update |= check;
		list_local(local, &x, var, init, svar, NULL);
		cons_local(local, &root, x, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(value, root);

	return Result(ret, 1);
}

/* &key */
static int checkparse_key_(OptimizeInfo *str, addr list, int *ret)
{
	int check;
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCdr(pos, &pos); /* var */
		GetCdr(pos, &pos); /* name */
		GetCar(pos, &pos); /* init */
		Return(checkparse_inplace_(str, pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int optparse_key_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	int update, check;
	addr root, x, var, name, init, svar;
	LocalRoot local;

	/* key -> (var name init svar) */
	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		List_bind(x, &var, &name, &init, &svar, NULL);
		Return(optparse_inplace_(str, init, &init, &check));
		update |= check;
		list_local(local, &x, var, name, init, svar, NULL);
		cons_local(local, &root, x, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(value, root);

	return Result(ret, 1);
}

/* &aux */
static int checkparse_aux_(OptimizeInfo *str, addr list, int *ret)
{
	return checkparse_opt_(str, list, ret);
}

static int optparse_aux_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	int update, check;
	addr root, x, var, init;
	LocalRoot local;

	/* aux -> (var init) */
	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		List_bind(x, &var, &init, NULL);
		Return(optparse_inplace_(str, init, &init, &check));
		update |= check;
		list_local(local, &x, var, init, NULL);
		cons_local(local, &root, x, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(value, root);

	return Result(ret, 1);
}

/* interface */
static int checkparse_lambda_ordinary_(OptimizeInfo *str, addr args, int *ret)
{
	int check;
	addr var, opt, rest, key, allow, aux;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);

	Return(checkparse_opt_(str, opt, &check));
	if (check)
		return Result(ret, 1);
	Return(checkparse_key_(str, key, &check));
	if (check)
		return Result(ret, 1);

	return checkparse_aux_(str, aux, ret);
}

static int optparse_lambda_ordinary_(
		OptimizeInfo *str, addr args, addr *value, int *ret)
{
	int update, check;
	addr var, opt, rest, key, allow, aux;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	update = 0;
	/* opt */
	Return(checkparse_opt_(str, opt, &check));
	if (check) {
		Return(optparse_opt_(str, opt, &opt, &check));
		update |= check;
	}
	/* key */
	Return(checkparse_key_(str, key, &check));
	if (check) {
		Return(optparse_key_(str, key, &key, &check));
		update |= check;
	}
	/* aux */
	Return(checkparse_aux_(str, aux, &check));
	if (check) {
		Return(optparse_aux_(str, aux, &aux, &check));
		update |= check;
	}
	/* result */
	if (! update)
		return Result(ret, 0);
	list_local(str->local, value, var, opt, rest, key, allow, aux, NULL);

	return Result(ret, 1);
}


/*
 *  lambda
 */
/* args */
static int checkparse_lambda_args_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_LAMBDA))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos);
	return checkparse_lambda_ordinary_(str, pos, ret);
}

static int optparse_lambda_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, args, decl, doc, body, form;

	Return_check_optparse(checkparse_lambda_args_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	Return(optparse_lambda_ordinary_(str, args, &args, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_LAMBDA, 5);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_lambda_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_LAMBDA))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &decl); /* decl */
	GetEvalParse(pos, 4, &body); /* body */
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_lambda_body_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, args, decl, doc, body, form;

	Return_check_optparse(checkparse_lambda_body_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_LAMBDA, 5);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-lambda */
int checkparse_lambda_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_lambda_args_, str, ret);
	Return_or_optparse(checkparse_lambda_body_, str, ret);

	return Result(ret, 0);
}

static int optparse_lambda_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_lambda_args_));
	Return(optimize_extract_(str, optparse_lambda_body_));

	return 0;
}
int optparse_lambda_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_lambda_run_);
}


/*
 *  defun
 */
/* args */
static int checkparse_defun_args_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFUN))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &pos); /* args */
	return checkparse_lambda_ordinary_(str, pos, ret);
}

static int optparse_defun_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, name, args, decl, doc, body, form;

	Return_check_optparse(checkparse_defun_args_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &name);
	GetEvalParse(pos, 2, &args);
	GetEvalParse(pos, 3, &decl);
	GetEvalParse(pos, 4, &doc);
	GetEvalParse(pos, 5, &body);

	Return(optparse_lambda_ordinary_(str, args, &args, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFUN, 6);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, name);
	SetEvalParse(pos, 2, args);
	SetEvalParse(pos, 3, decl);
	SetEvalParse(pos, 4, doc);
	SetEvalParse(pos, 5, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_defun_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFUN))
		return Result(ret, 0);
	GetEvalParse(pos, 3, &decl); /* decl */
	GetEvalParse(pos, 5, &body); /* body */
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_defun_body_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, name, args, decl, doc, body, form;

	Return_check_optparse(checkparse_defun_body_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &name);
	GetEvalParse(pos, 2, &args);
	GetEvalParse(pos, 3, &decl);
	GetEvalParse(pos, 4, &doc);
	GetEvalParse(pos, 5, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFUN, 6);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, name);
	SetEvalParse(pos, 2, args);
	SetEvalParse(pos, 3, decl);
	SetEvalParse(pos, 4, doc);
	SetEvalParse(pos, 5, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-defun */
int checkparse_defun_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_defun_args_, str, ret);
	Return_or_optparse(checkparse_defun_body_, str, ret);

	return Result(ret, 0);
}

static int optparse_defun_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_defun_args_));
	Return(optimize_extract_(str, optparse_defun_body_));

	return 0;
}
int optparse_defun_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_defun_run_);
}


/*
 *  macro-lambda
 */
static int checkparse_lambda_macro_(OptimizeInfo *str, addr args, int *ret);
static int checkparse_macro_var_(OptimizeInfo *str, addr list, int *ret)
{
	int check;
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (! consp(pos))
			continue;
		Return(checkparse_lambda_macro_(str, pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int optparse_lambda_macro_(OptimizeInfo *str, addr pos, addr *value, int *ret);
static int optparse_macro_var_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	int update, check;
	addr root, var;
	LocalRoot local;

	/* var */
	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		if (consp(var)) {
			Return(optparse_lambda_macro_(str, var, &var, &check));
			update |= check;
		}
		cons_local(local, &root, var, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(value, root);

	return Result(ret, 1);
}

static int checkparse_lambda_macro_(OptimizeInfo *str, addr args, int *ret)
{
	int check;
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	/* var */
	Return(checkparse_macro_var_(str, var, &check));
	if (check)
		return Result(ret, 1);
	/* opt */
	Return(checkparse_opt_(str, opt, &check));
	if (check)
		return Result(ret, 1);
	/* key */
	Return(checkparse_key_(str, key, &check));
	if (check)
		return Result(ret, 1);
	/* aux */
	return checkparse_aux_(str, aux, ret);
}

static int optparse_lambda_macro_(OptimizeInfo *str, addr args, addr *value, int *ret)
{
	int update, check;
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	update = 0;
	/* var */
	Return(checkparse_macro_var_(str, var, &check));
	if (check) {
		Return(optparse_macro_var_(str, var, &var, &check));
		update |= check;
	}
	/* opt */
	Return(checkparse_opt_(str, opt, &check));
	if (check) {
		Return(optparse_opt_(str, opt, &opt, &check));
		update |= check;
	}
	/* key */
	Return(checkparse_key_(str, key, &check));
	if (check) {
		Return(optparse_key_(str, key, &key, &check));
		update |= check;
	}
	/* aux */
	Return(checkparse_opt_(str, aux, &check));
	if (check) {
		Return(optparse_aux_(str, aux, &aux, &check));
		update |= check;
	}
	/* result */
	if (! update)
		return Result(ret, 0);
	list_local(str->local, value, var, opt, rest, key, allow, aux, whole, env, NULL);

	return Result(ret, 1);
}


/*
 *  defmacro
 */
/* args */
static int checkparse_defmacro_args_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MACRO_LAMBDA))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &pos); /* args */
	return checkparse_lambda_macro_(str, pos, ret);
}

static int optparse_defmacro_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, args, decl, doc, body, call;

	Return_check_optparse(checkparse_defmacro_args_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &doc);
	GetEvalParse(pos, 3, &body);
	GetEvalParse(pos, 4, &call);

	Return(optparse_lambda_macro_(str, args, &args, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MACRO_LAMBDA, 5);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, doc);
	SetEvalParse(pos, 3, body);
	SetEvalParse(pos, 4, call);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_defmacro_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MACRO_LAMBDA))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 3, &body); /* body */
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_defmacro_body_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, args, decl, doc, body, call;

	Return_check_optparse(checkparse_defmacro_body_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &doc);
	GetEvalParse(pos, 3, &body);
	GetEvalParse(pos, 4, &call);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MACRO_LAMBDA, 5);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, doc);
	SetEvalParse(pos, 3, body);
	SetEvalParse(pos, 4, call);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-defmacro */
int checkparse_defmacro_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_defmacro_args_, str, ret);
	Return_or_optparse(checkparse_defmacro_body_, str, ret);

	return Result(ret, 0);
}

static int optparse_defmacro_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_defmacro_args_));
	Return(optimize_extract_(str, optparse_defmacro_body_));

	return 0;
}
int optparse_defmacro_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_defmacro_run_);
}


/*
 *  deftype
 */
/* args */
static int checkparse_deftype_args_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFTYPE))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* args */
	return checkparse_lambda_macro_(str, pos, ret);
}

static int optparse_deftype_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, name, args, decl, doc, body;

	Return_check_optparse(checkparse_deftype_args_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	Return(optparse_lambda_macro_(str, args, &args, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFTYPE, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_deftype_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFTYPE))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &decl); /* decl */
	GetEvalParse(pos, 4, &body); /* body */
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_deftype_body_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, name, args, decl, doc, body;

	Return_check_optparse(checkparse_deftype_body_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFTYPE, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-deftype */
int checkparse_deftype_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_deftype_args_, str, ret);
	Return_or_optparse(checkparse_deftype_body_, str, ret);

	return Result(ret, 0);
}

static int optparse_deftype_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_deftype_args_));
	Return(optimize_extract_(str, optparse_deftype_body_));

	return 0;
}
int optparse_deftype_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_deftype_run_);
}


/*
 *  define-compiler-macro
 */
/* args */
static int checkparse_define_compiler_macro_args_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFINE_COMPILER_MACRO))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* args */
	return checkparse_lambda_macro_(str, pos, ret);
}

static int optparse_define_compiler_macro_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, name, args, decl, doc, body;

	Return_check_optparse(checkparse_define_compiler_macro_args_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	Return(optparse_lambda_macro_(str, args, &args, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFINE_COMPILER_MACRO, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_define_compiler_macro_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFINE_COMPILER_MACRO))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &decl); /* decl */
	GetEvalParse(pos, 4, &body); /* body */
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_define_compiler_macro_body_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, name, args, decl, doc, body;

	Return_check_optparse(checkparse_define_compiler_macro_body_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFINE_COMPILER_MACRO, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-define-compiler-macro */
int checkparse_define_compiler_macro_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_define_compiler_macro_args_, str, ret);
	Return_or_optparse(checkparse_define_compiler_macro_body_, str, ret);

	return Result(ret, 0);
}

static int optparse_define_compiler_macro_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_define_compiler_macro_args_));
	Return(optimize_extract_(str, optparse_define_compiler_macro_body_));

	return 0;
}
int optparse_define_compiler_macro_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_define_compiler_macro_run_);
}


/*
 *  flet / labels
 */
static int optimize_fletlabels(OptimizeInfo *str)
{
	addr pos;
	EvalParse type;

	pos = str->pos;
	if (! eval_parse_p(pos))
		return 0;
	GetEvalParseType(pos, &type);

	return type == EVAL_PARSE_FLET || type == EVAL_PARSE_LABELS;
}

static int optimize_fletlabels_on(OptimizeInfo *str)
{
	return optimize_speed_on(str) && optimize_fletlabels(str);
}

/* (flet ()) -> nil */
static int checkparse_flet1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_fletlabels_on(str))
		return Result(ret, 0);
	GetEvalParse(str->pos, 3, &pos);
	return Result(ret, pos == Nil);
}

static int optparse_flet1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_flet1_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* (flet () values... x) -> x */
static int optimize_flet_value(addr pos)
{
	return optimize_value_only(pos);
}

static int checkparse_flet2_(OptimizeInfo *str, int *ret)
{
	addr pos, check;

	if (! optimize_fletlabels_on(str))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 2, &check);
	if (! empty_nil_declare(check))
		return Result(ret, 0);
	GetEvalParse(pos, 3, &pos);
	if (pos == Nil)
		return Result(ret, 0);
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		if (! optimize_flet_value(check))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int optparse_flet2_(OptimizeInfo *str, int *ret)
{
	addr list, x;

	Return_check_optparse(checkparse_flet2_, str, ret);
	GetEvalParse(str->pos, 3, &list);
	if (list == Nil)
		return Result(ret, 0);
	for (x = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
	}
	str->pos = x;

	return Result(ret, 1);
}

/* (flet () ...) -> (progn ...) */
static int checkparse_flet3_(OptimizeInfo *str, int *ret)
{
	addr pos, check;

	if (! optimize_fletlabels_on(str))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 1, &check);
	if (check != Nil)
		return Result(ret, 0);
	GetEvalParse(pos, 2, &check);
	if (! empty_nil_declare(check))
		return Result(ret, 0);
	GetEvalParse(pos, 3, &check);
	return Result(ret, check != Nil);
}

static int optparse_flet3_(OptimizeInfo *str, int *ret)
{
	addr form, pos;

	Return_check_optparse(checkparse_flet3_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 3, &pos);
	eval_parse2_local(str->local, &pos, EVAL_PARSE_PROGN, form, pos);
	str->pos = pos;

	return Result(ret, 1);
}

/* (flet () (declare ...) ...) -> (locally (declare ...) ...) */
static int checkparse_flet4_(OptimizeInfo *str, int *ret)
{
	addr pos, check;

	if (! optimize_fletlabels_on(str))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 1, &check);
	if (check != Nil)
		return Result(ret, 0);
	GetEvalParse(pos, 2, &check);
	if (empty_nil_declare(check))
		return Result(ret, 0);
	GetEvalParse(pos, 3, &check);
	return Result(ret, check != Nil);
}

static int optparse_flet4_(OptimizeInfo *str, int *ret)
{
	addr pos, form, decl, cons;

	Return_check_optparse(checkparse_flet4_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &cons);

	eval_parse_local(str->local, &pos, EVAL_PARSE_LOCALLY, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, cons);
	str->pos = pos;

	return Result(ret, 1);
}

/* flet-args */
static int checkparse_flet_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, value, call, args, decl, doc, body;

	/* Don't check optimize. */
	if (! optimize_fletlabels(str))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &pos);
	while (pos != Nil) {
		GetCons(pos, &value, &pos);
		List_bind(value, &call, &args, &decl, &doc, &body, NULL);
		Return(checkparse_lambda_ordinary_(str, args, &check));
		if (check)
			return Result(ret, 1);
		Return(checkparse_implicit_declare_(str, decl, body, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int optparse_flet_one_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	int update, check, check1, check2;
	addr root, call, args, decl, doc, body, x;
	LocalRoot local;

	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		List_bind(x, &call, &args, &decl, &doc, &body, NULL);

		check1 = check2 = 0;
		Return(checkparse_lambda_ordinary_(str, args, &check));
		if (check) {
			Return(optparse_lambda_ordinary_(str, args, &args, &check1));
		}
		Return(checkparse_implicit_declare_(str, decl, body, &check));
		if (check) {
			Return(optparse_implicit_declare_(str, decl, body, &body, &check2));
		}
		if (check1 || check2) {
			list_local(local, &x, call, args, decl, doc, body, NULL);
			update = 1;
		}
		cons_local(local, &root, x, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(value, root);

	return Result(ret, 1);
}

static int optparse_flet_args_(OptimizeInfo *str, int *ret)
{
	int check;
	EvalParse type;
	addr pos, form, args, decl, body;

	Return_check_optparse(checkparse_flet_args_, str, ret);
	pos = str->pos;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &body);

	Return(optparse_flet_one_(str, args, &args, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, type, 4);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* flet-body */
static int checkparse_flet_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	if (! optimize_fletlabels(str))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &body);
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_flet_body_(OptimizeInfo *str, int *ret)
{
	int check;
	EvalParse type;
	addr pos, form, args, decl, body;

	Return_check_optparse(checkparse_flet_body_, str, ret);
	pos = str->pos;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, type, 4);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-flet */
int checkparse_flet_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_flet1_, str, ret);
	Return_or_optparse(checkparse_flet2_, str, ret);
	Return_or_optparse(checkparse_flet3_, str, ret);
	Return_or_optparse(checkparse_flet4_, str, ret);
	Return_or_optparse(checkparse_flet_args_, str, ret);
	Return_or_optparse(checkparse_flet_body_, str, ret);

	return Result(ret, 0);
}

static int optparse_flet_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_flet1_));
	Return(optimize_extract_(str, optparse_flet2_));
	Return(optimize_extract_(str, optparse_flet3_));
	Return(optimize_extract_(str, optparse_flet4_));
	Return(optimize_extract_(str, optparse_flet_args_));
	Return(optimize_extract_(str, optparse_flet_body_));

	return 0;
}
int optparse_flet_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_flet_run_);
}


/************************************************************
 *  optimize_parse.c
 ************************************************************/

static int checkparse_all_(OptimizeInfo *str, int *ret);
static int optparse_all_(OptimizeInfo *str, int *ret);

int checkparse_inplace_(OptimizeInfo *str, addr pos, int *ret)
{
	int check;
	OptimizeInfo save;

	save = *str;
	str->pos = pos;
	Return(checkparse_all_(str, &check));
	*str = save;

	return Result(ret, check);
}

int optparse_inplace_(OptimizeInfo *str, addr pos, addr *value, int *ret)
{
	int check;
	OptimizeInfo save;

	save = *str;
	str->pos = pos;
	Return(optparse_all_(str, &check));
	*value = check? str->pos: pos;
	*str = save;

	return Result(ret, check);
}

int checkparse_implicit_declare_(
		OptimizeInfo *str, addr decl, addr cons, int *ret)
{
	int check;
	OptimizeInfo save;

	save = *str;
	if (decl != Nil)
		apply_array_declare(str->value.local, decl);
	Return(checkparse_implicit_(str, cons, &check));
	*str = save;

	return Result(ret, check);
}

int optparse_implicit_declare_(OptimizeInfo *str,
		addr decl, addr cons, addr *value, int *ret)
{
	int check;
	OptimizeInfo save;

	save = *str;
	if (decl != Nil)
		apply_array_declare(str->value.local, decl);
	Return(optparse_implicit_(str, cons, value, &check));
	*str = save;

	return Result(ret, check);
}

int optparse_run_(OptimizeInfo *str, int *ret, int (*call)(OptimizeInfo *))
{
	int update, result;
	addr pos;

	update = str->update;
	pos = str->pos;
	for (result = 0; ; result |= str->update) {
		str->update = 0;
		Return((*call)(str));

		if (str->update == 0)
			break;
	}

	if (result) {
		str->update = 1;
		return Result(ret, 1);
	}
	else {
		str->pos = pos;
		str->update = update;
		return Result(ret, 0);
	}
}


/*
 *  optimize-check
 */
/* (lisp-system::optimize-check parse) -> 0 / 1 */
static int checkparse_optimize_check_(OptimizeInfo *str, addr *value, int *ret)
{
	int check;
	addr call, left, right;

	/* call */
	call = str->pos;
	if (! optimize_evaltype(call, EVAL_PARSE_CALL))
		goto skip;
	GetEvalParse(call, 1, &left);
	/* function */
	if (! optimize_evaltype(left, EVAL_PARSE_FUNCTION))
		goto skip;
	/* function name */
	GetEvalParse(left, 1, &left);
	GetCallName(left, &left);
	GetConst(SYSTEM_OPTIMIZE_CHECK, &right);
	if (left != right)
		goto skip;
	/* argument */
	GetEvalParse(call, 2, &right);
	if (! consp_getcons(right, &left, &right))
		goto skip;
	/* PARSE */
	if (! optimize_evaltype(left, EVAL_PARSE_SYMBOL))
		goto skip;
	GetEvalParse(left, 0, &left);
	Return(string_designator_equalp_char_(left, "PARSE", &check));
	if (! check)
		goto skip;
	/* result */
	*value = right;
	return Result(ret, 1);

skip:
	return Result(ret, 0);
}

static int checkparse_check1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr list;

	Return(checkparse_optimize_check_(str, &list, &check));
	if (! check)
		return Result(ret, 0);

	return Result(ret, list == Nil);
}

static int optparse_check1_(OptimizeInfo *str, int *ret)
{
	addr x;

	Return_check_optparse(checkparse_check1_, str, ret);
	fixnum_heap(&x, optimize_speed_on(str)? 1: 0);
	eval_single_parse_local(str->local, &str->pos, EVAL_PARSE_INTEGER, x);

	return Result(ret, 1);
}

/* (lisp-system::optimize-check parse list) -> (...) */
static int checkparse_check2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr list, pos;

	Return(checkparse_optimize_check_(str, &list, &check));
	if (! check)
		return Result(ret, 0);
	if (list == Nil)
		return Result(ret, 0);
	if (! consp_getcons(list, &pos, &list))
		return Result(ret, 0);
	if (list != Nil)
		return Result(ret, 0);
	if (! optimize_evaltype(pos, EVAL_PARSE_SYMBOL))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &pos);
	return string_designator_equalp_char_(pos, "LIST", ret);
}

static int optparse_check2_(OptimizeInfo *str, int *ret)
{
	addr symbol, x, list, quote, form;

	Return_check_optparse(checkparse_check2_, str, ret);
	list = Nil;
	/* compilation-speed */
	GetConst(COMMON_COMPILATION_SPEED, &symbol);
	fixnum_heap(&x, (fixnum)optimize_declare_value(str, EVAL_OPTIMIZE_COMPILATION));
	cons_heap(&x, symbol, x);
	cons_heap(&list, x, list);
	/* debug */
	GetConst(COMMON_DEBUG, &symbol);
	fixnum_heap(&x, (fixnum)optimize_declare_value(str, EVAL_OPTIMIZE_DEBUG));
	cons_heap(&x, symbol, x);
	cons_heap(&list, x, list);
	/* safety */
	GetConst(COMMON_SAFETY, &symbol);
	fixnum_heap(&x, (fixnum)optimize_declare_value(str, EVAL_OPTIMIZE_SAFETY));
	cons_heap(&x, symbol, x);
	cons_heap(&list, x, list);
	/* space */
	GetConst(COMMON_SPACE, &symbol);
	fixnum_heap(&x, (fixnum)optimize_declare_value(str, EVAL_OPTIMIZE_SPACE));
	cons_heap(&x, symbol, x);
	cons_heap(&list, x, list);
	/* speed */
	GetConst(COMMON_SPEED, &symbol);
	fixnum_heap(&x, (fixnum)optimize_declare_value(str, EVAL_OPTIMIZE_SPEED));
	cons_heap(&x, symbol, x);
	cons_heap(&list, x, list);
	/* quote */
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&form, quote, list, NULL);
	eval_parse2_heap(&list, EVAL_PARSE_QUOTE, form, list);
	str->pos = list;

	return Result(ret, 1);
}

/* optparse-check */
static int checkparse_check_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_check1_, str, ret);
	Return_or_optparse(checkparse_check2_, str, ret);

	return Result(ret, 0);
}

static int optparse_check_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_check1_));
	Return(optimize_extract_(str, optparse_check2_));

	return 0;
}
static int optparse_check_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_check_run_);
}


/*
 *  optimize_value
 */
static int optimize_value_function(addr pos, int functionp);
static int optimize_value_values(addr list, int functionp)
{
	addr pos;

	GetEvalParse(list, 1, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (! optimize_value_function(pos, functionp))
			return 0;
	}

	return 1;
}

static int optmize_value_the(addr pos, int functionp)
{
	GetEvalParse(pos, 2, &pos); /* expr */
	return optimize_value_function(pos, functionp);
}

static int optimize_value_function(addr pos, int functionp)
{
	if (! eval_parse_p(pos))
		return 0;
	switch (RefEvalParseType(pos)) {
		case EVAL_PARSE_NIL:
		case EVAL_PARSE_T:
		case EVAL_PARSE_CLOS:
		case EVAL_PARSE_INTEGER:
		case EVAL_PARSE_RATIONAL:
		case EVAL_PARSE_COMPLEX:
		case EVAL_PARSE_CHARACTER:
		case EVAL_PARSE_ARRAY:
		case EVAL_PARSE_VECTOR:
		case EVAL_PARSE_BITVECTOR:
		case EVAL_PARSE_STRING:
		case EVAL_PARSE_FLOAT:
		case EVAL_PARSE_PATHNAME:
		case EVAL_PARSE_QUOTE:
		case EVAL_PARSE_LAMBDA:
			return 1;

		case EVAL_PARSE_FUNCTION:
			return functionp;

		case EVAL_PARSE_THE:
			return optmize_value_the(pos, functionp);

		case EVAL_PARSE_VALUES:
			return optimize_value_values(pos, functionp);

		default:
			return 0;
	}
}

int optimize_value_and_function(addr pos)
{
	return optimize_value_function(pos, 1);
}

int optimize_value_only(addr pos)
{
	return optimize_value_function(pos, 0);
}


/*
 *  optimize-parse
 */
static int checkparse_all_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_check_, str, ret);
	Return_or_optparse(checkparse_progn_, str, ret);
	Return_or_optparse(checkparse_let_, str, ret);
	Return_or_optparse(checkparse_setq_, str, ret);
	Return_or_optparse(checkparse_defun_, str, ret);
	Return_or_optparse(checkparse_defmacro_, str, ret);
	Return_or_optparse(checkparse_deftype_, str, ret);
	Return_or_optparse(checkparse_define_compiler_macro_, str, ret);
	Return_or_optparse(checkparse_destructuring_bind_, str, ret);
	Return_or_optparse(checkparse_lambda_, str, ret);
	Return_or_optparse(checkparse_if_, str, ret);
	Return_or_optparse(checkparse_unwind_protect_, str, ret);
	Return_or_optparse(checkparse_tagbody_, str, ret);
	Return_or_optparse(checkparse_block_, str, ret);
	Return_or_optparse(checkparse_catch_, str, ret);
	Return_or_optparse(checkparse_flet_, str, ret);
	Return_or_optparse(checkparse_the_, str, ret);
	Return_or_optparse(checkparse_eval_when_, str, ret);
	Return_or_optparse(checkparse_values_, str, ret);
	Return_or_optparse(checkparse_locally_, str, ret);
	Return_or_optparse(checkparse_call_, str, ret);
	Return_or_optparse(checkparse_multiple_value_bind_, str, ret);
	Return_or_optparse(checkparse_multiple_value_call_, str, ret);
	Return_or_optparse(checkparse_multiple_value_prog1_, str, ret);
	Return_or_optparse(checkparse_nth_value_, str, ret);
	Return_or_optparse(checkparse_progv_, str, ret);

	return Result(ret, 0);
}

static int optparse_all_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_check_));
	Return(optimize_extract_(str, optparse_progn_));
	Return(optimize_extract_(str, optparse_let_));
	Return(optimize_extract_(str, optparse_setq_));
	Return(optimize_extract_(str, optparse_defun_));
	Return(optimize_extract_(str, optparse_defmacro_));
	Return(optimize_extract_(str, optparse_deftype_));
	Return(optimize_extract_(str, optparse_define_compiler_macro_));
	Return(optimize_extract_(str, optparse_destructuring_bind_));
	Return(optimize_extract_(str, optparse_lambda_));
	Return(optimize_extract_(str, optparse_if_));
	Return(optimize_extract_(str, optparse_unwind_protect_));
	Return(optimize_extract_(str, optparse_tagbody_));
	Return(optimize_extract_(str, optparse_block_));
	Return(optimize_extract_(str, optparse_catch_));
	Return(optimize_extract_(str, optparse_flet_));
	Return(optimize_extract_(str, optparse_the_));
	Return(optimize_extract_(str, optparse_eval_when_));
	Return(optimize_extract_(str, optparse_values_));
	Return(optimize_extract_(str, optparse_locally_));
	Return(optimize_extract_(str, optparse_call_));
	Return(optimize_extract_(str, optparse_multiple_value_bind_));
	Return(optimize_extract_(str, optparse_multiple_value_call_));
	Return(optimize_extract_(str, optparse_multiple_value_prog1_));
	Return(optimize_extract_(str, optparse_nth_value_));
	Return(optimize_extract_(str, optparse_progv_));

	return 0;
}
static int optparse_all_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_all_run_);
}

int optimize_parse_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	OptimizeInfo str;

	local = ptr->local;
	push_local(local, &stack);
	optimize_initialize(&str, ptr, pos);
	Return(optparse_all_(&str, &check));
	if (check)
		copy_eval_parse_heap(value, str.pos);
	else
		*value = pos;
	rollback_local(local, stack);

	if (ret)
		return Result(ret, check);
	else
		return 0;
}


/************************************************************
 *  optimize_values.c
 ************************************************************/

/*
 *  values
 */
int checkparse_values_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_VALUES))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos);
	return checkparse_implicit_all_(str, pos, ret);
}

int optparse_values_(OptimizeInfo *str, int *ret)
{
	int check;
	addr form, pos;

	Return_check_optparse(checkparse_values_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 1, &pos);
	Return(optparse_implicit_all_(str, pos, &pos, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse2_local(str->local, &pos, EVAL_PARSE_VALUES, form, pos);
	str->pos = pos;

	return Result(ret, 1);
}


/*
 *  multiple-value-bind
 */
/* expr */
static int checkparse_multiple_value_bind1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_BIND))
		return Result(ret, 0);
	GetEvalParse(str->pos, 2, &pos); /* expr */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_multiple_value_bind1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, vars, expr, decl, doc, body;

	Return_check_optparse(checkparse_multiple_value_bind1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &vars);
	GetEvalParse(pos, 2, &expr);
	GetEvalParse(pos, 3, &decl);
	GetEvalParse(pos, 4, &doc);
	GetEvalParse(pos, 5, &body);

	Return(optparse_inplace_(str, expr, &expr, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_BIND, 6);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, vars);
	SetEvalParse(pos, 2, expr);
	SetEvalParse(pos, 3, decl);
	SetEvalParse(pos, 4, doc);
	SetEvalParse(pos, 5, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_multiple_value_bind2_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_BIND))
		return Result(ret, 0);
	GetEvalParse(pos, 3, &decl);
	GetEvalParse(pos, 5, &body);
	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_multiple_value_bind2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, vars, expr, decl, doc, body;

	Return_check_optparse(checkparse_multiple_value_bind2_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &vars);
	GetEvalParse(pos, 2, &expr);
	GetEvalParse(pos, 3, &decl);
	GetEvalParse(pos, 4, &doc);
	GetEvalParse(pos, 5, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_BIND, 6);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, vars);
	SetEvalParse(pos, 2, expr);
	SetEvalParse(pos, 3, decl);
	SetEvalParse(pos, 4, doc);
	SetEvalParse(pos, 5, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-multiple-value-bind */
int checkparse_multiple_value_bind_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_multiple_value_bind1_, str, ret);
	Return_or_optparse(checkparse_multiple_value_bind2_, str, ret);

	return Result(ret, 0);
}

static int optparse_multiple_value_bind_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_multiple_value_bind1_));
	Return(optimize_extract_(str, optparse_multiple_value_bind2_));

	return 0;
}
int optparse_multiple_value_bind_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_multiple_value_bind_run_);
}


/*
 *  multiple-value-call
 */
/* expr */
static int checkparse_multiple_value_call1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_CALL))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* expr */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_multiple_value_call1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, call, body;

	Return_check_optparse(checkparse_multiple_value_call1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &call);
	GetEvalParse(pos, 2, &body);

	Return(optparse_inplace_(str, call, &call, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_CALL, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, call);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_multiple_value_call2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_CALL))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &pos); /* body */
	return checkparse_implicit_all_(str, pos, ret);
}

static int optparse_multiple_value_call2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, call, body;

	Return_check_optparse(checkparse_multiple_value_call2_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &call);
	GetEvalParse(pos, 2, &body);

	Return(optparse_implicit_all_(str, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_CALL, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, call);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-multiple-value-call */
int checkparse_multiple_value_call_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_multiple_value_call1_, str, ret);
	Return_or_optparse(checkparse_multiple_value_call2_, str, ret);

	return Result(ret, 0);
}

static int optparse_multiple_value_call_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_multiple_value_call1_));
	Return(optimize_extract_(str, optparse_multiple_value_call2_));

	return 0;
}
int optparse_multiple_value_call_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_multiple_value_call_run_);
}


/*
 *  multiple-value-prog1
 */
/* expr */
static int checkparse_multiple_value_prog1_1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_PROG1))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* expr */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_multiple_value_prog1_1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, expr, body;

	Return_check_optparse(checkparse_multiple_value_prog1_1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &body);

	Return(optparse_inplace_(str, expr, &expr, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_PROG1, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_multiple_value_prog1_2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_PROG1))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &pos); /* body */
	return checkparse_implicit_all_(str, pos, ret);
}

static int optparse_multiple_value_prog1_2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, expr, body;

	Return_check_optparse(checkparse_multiple_value_prog1_2_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &body);

	Return(optparse_implicit_all_(str, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_PROG1, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-multiple-value-prog1 */
int checkparse_multiple_value_prog1_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_multiple_value_prog1_1_, str, ret);
	Return_or_optparse(checkparse_multiple_value_prog1_2_, str, ret);

	return Result(ret, 0);
}

static int optparse_multiple_value_prog1_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_multiple_value_prog1_1_));
	Return(optimize_extract_(str, optparse_multiple_value_prog1_2_));

	return 0;
}
int optparse_multiple_value_prog1_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_multiple_value_prog1_run_);
}


/*
 *  nth-value
 */
/* nth */
static int checkparse_nth_value_1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_NTH_VALUE))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* nth */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_nth_value_1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, nth, expr;

	Return_check_optparse(checkparse_nth_value_1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &nth);
	GetEvalParse(pos, 2, &expr);

	Return(optparse_inplace_(str, nth, &nth, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_NTH_VALUE, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, nth);
	SetEvalParse(pos, 2, expr);
	str->pos = pos;

	return Result(ret, 1);
}

/* expr */
static int checkparse_nth_value_2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_NTH_VALUE))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &pos); /* expr */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_nth_value_2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, nth, expr;

	Return_check_optparse(checkparse_nth_value_2_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &nth);
	GetEvalParse(pos, 2, &expr);

	Return(optparse_inplace_(str, expr, &expr, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_NTH_VALUE, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, nth);
	SetEvalParse(pos, 2, expr);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-nth-value */
int checkparse_nth_value_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_nth_value_1_, str, ret);
	Return_or_optparse(checkparse_nth_value_2_, str, ret);

	return Result(ret, 0);
}

static int optparse_nth_value_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_nth_value_1_));
	Return(optimize_extract_(str, optparse_nth_value_2_));

	return 0;
}
int optparse_nth_value_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_nth_value_run_);
}


/************************************************************
 *  package.c
 ************************************************************/
#define LISP_PACKAGE_COUNT_CLOS 147
#define LISP_PACKAGE_COUNT_CODE 138
#define LISP_PACKAGE_COUNT_COMMON 964
#define LISP_PACKAGE_COUNT_KEYWORD 243
#define LISP_PACKAGE_COUNT_RT 13
#define LISP_PACKAGE_COUNT_SYSTEM 432

#define LISP_PACKAGE_HASHSIZE        16

/*
 *  package object
 */
static int find_package_direct_(addr pos, addr *ret)
{
	addr table;
	PackageTable(&table);
	return findnil_hashtable_(table, pos, ret);
}

static int find_package_local_(addr pos, addr *ret)
{
	addr table, name;
	LocalRoot local;
	LocalStack stack;
	unicode c;

	PackageTable(&table);
	GetCharacter(pos, &c);

	/* findvalue */
	local = Local_Thread;
	push_local(local, &stack);
	strvect_local(local, &name, 1);
	Return(strvect_setc_(name, 0, c));
	Return(findnil_hashtable_(table, name, ret));
	rollback_local(local, stack);

	return 0;
}

int find_package_(addr pos, addr *ret)
{
	addr type;

	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
			return Result(ret, pos);

		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			GetNameSymbol(pos, &pos);
			return find_package_direct_(pos, ret);

		case LISPTYPE_ARRAY:
			if (! strarrayp(pos))
				goto error;
			return find_package_direct_(pos, ret);

		case LISPTYPE_STRING:
			return find_package_direct_(pos, ret);

		case LISPTYPE_CHARACTER:
			return find_package_local_(pos, ret);

		default:
			goto error;
	}

error:
	*ret = Nil;
	GetTypeTable(&type, PackageDesignator);
	return call_type_error_(NULL, pos, type);
}

int find_char_package_(const char *name, addr *ret)
{
	addr pos;
	PackageTable(&pos);
	return findnil_char_hashtable_(pos, name, ret);
}

static int append_root_package_(addr name, addr package)
{
	addr table;

	PackageTable(&table);
	Return(intern_hashheap_(table, name, &name));
	SetCdr(name, package);  /* (name . package) */

	return 0;
}

int package_size_heap_(addr *ret, addr name, size_t size)
{
	addr pos, table;

	/* name check */
	Return(string_designator_heap_(&name, name, NULL));
	Return(find_package_direct_(name, &pos));
	if (pos != Nil) {
		return call_simple_package_error_va_(NULL,
				"The package name ~S is already used.", name, NULL);
	}

	/* make package */
	heap_array2(&pos, LISPTYPE_PACKAGE, PACKAGE_INDEX_SIZE);
	SetUser(pos, 0);  /* not readonly */
	if (size)
		hashtable_size_heap(&table, size);
	else
		hashtable_heap(&table);
	settest_hashtable(table, HASHTABLE_TEST_EQUAL);
	SetPackage(pos, PACKAGE_INDEX_TABLE, table);
	SetPackage(pos, PACKAGE_INDEX_NAME, name);

	/* append root */
	Return(append_root_package_(name, pos));

	return Result(ret, pos);
}

int package_heap_(addr *ret, addr name)
{
	return package_size_heap_(ret, name, 0);
}

static int package_char_heap_(addr *ret, const char *name)
{
	addr key;
	strvect_char_heap(&key, name);
	return package_heap_(ret, key);
}

static void package_root_heap(addr *ret)
{
	addr pos;

	hashtable_size_heap(&pos, LISP_PACKAGE_HASHSIZE);
	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	*ret = pos;
}

static int intern_common_constant_(addr package, const char *str, addr symbol)
{
	int check;
	addr pos, name;

	/* set table */
	strvect_char_heap(&name, str);
	Return(intern_bitpackage_(package, name, &pos, &check));
	SetBitTypeSymbol(pos, symbol);

	/* set symbol */
	SetStatusValue(symbol, LISPSTATUS_READONLY, 0);
	SetPackageSymbol(symbol, package);
	SetStatusValue(symbol, LISPSTATUS_READONLY, 1);
	return symbol_export_package_(package, symbol);
}

static int intern_common_default_(void)
{
	addr common;

	Return(find_char_package_(LISP_COMMON, &common));
	Return(intern_common_constant_(common, "NIL", Nil));
	Return(intern_common_constant_(common, "T", T));
	SetConstant(CONSTANT_COMMON_NIL, Nil);
	SetConstant(CONSTANT_COMMON_T, T);

	return 0;
}

static int intern_package_symbol_(void)
{
	addr symbol;

	/* common-lisp::*package* */
	Return(internchar_(LISP_COMMON, "*PACKAGE*", &symbol, NULL));
	setspecial_symbol(symbol);
	SetConstant(CONSTANT_SPECIAL_PACKAGE, symbol);

	/* common-lisp-user package */
	Return(find_char_package_(LISP_COMMON_USER, &symbol));
	Check(symbol == Nil, LISP_COMMON_USER " package is not found.");
	SetConstant(CONSTANT_PACKAGE_COMMON_LISP_USER, symbol);

	return 0;
}

static int set_default_package_(addr package)
{
	addr symbol;

	/* setq *package* */
	GetConst(SPECIAL_PACKAGE, &symbol);
	/* not special-stack */
	return setvalue_symbol_(symbol, package);
}

static int build_package_settings_(void)
{
	addr package, common, cons, name;

	/* COMMON-LISP */
	Return(find_char_package_(LISP_COMMON, &common));
	strvect_char_heap(&name, "CL");
	list_heap(&cons, name, NULL);
	Return(append_nicknames_package_(common, cons));

	/* COMMON-LISP-USER */
	Return(find_char_package_(LISP_COMMON_USER, &package));
	strvect_char_heap(&name, "CL-USER");
	list_heap(&cons, name, NULL);
	Return(append_nicknames_package_(package, cons));
	Return(use_package_(package, common));

	return 0;
}

static void set_gentemp_package(void)
{
	addr pos;
	fixnum_heap(&pos, 1);
	SetConst(PACKAGE_GENTEMP, pos);
}

static void build_default_use_package(void)
{
	addr name, list;

	strvect_char_heap(&name, "COMMON-LISP");
	list_heap(&list, name, NULL);
	SetConstant(CONSTANT_PACKAGE_DEFAULT_USE, list);
}

static int import_exit_and_quit_package_(addr package)
{
	addr symbol;

	/* (import 'lisp-system::exit 'common-lisp-user) */
	GetConst(SYSTEM_EXIT, &symbol);
	Return(import_package_(package, symbol));
	/* (import 'lisp-system::quit 'common-lisp-user) */
	GetConst(SYSTEM_QUIT, &symbol);
	Return(import_package_(package, symbol));

	return 0;
}

static int system_package_(const char *name, size_t size, constindex index)
{
	addr pos;

	strvect_char_heap(&pos, name);
	Return(package_size_heap_(&pos, pos, size + 1));
	SetConstant(index, pos);

	return 0;
}

static int build_package_nicknames_(void)
{
	addr pos, table, name, cons;
#ifdef LISP_DEBUG
	addr check;
#endif

	/* package */
	GetConst(PACKAGE_SYSTEM, &pos);
	CheckType(pos, LISPTYPE_PACKAGE);

	/* push nicknames */
#ifdef LISP_DEBUG
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &check);
	Check(check != Nil, "nicknames error.");
#endif
	strvect_char_heap(&name, LISPNAME);
	push_list_nicknames_package(pos, name);

	/* intern */
	PackageTable(&table);
	Return(intern_hashheap_(table, name, &cons));
#ifdef LISP_DEBUG
	GetCdr(cons, &check);
	Check(check != Nil, "package table error.");
#endif
	SetCdr(cons, pos);

	return 0;
}

#define SystemPackage(x,y,z) { \
	Return(system_package_(LISP_##x, LISP_PACKAGE_COUNT_##y, CONSTANT_PACKAGE_##z)); \
}

static int build_package_value_(void)
{
	addr root, user;

	/* package root */
	package_root_heap(&root);
	SetLispRoot(PACKAGE, root);

	/* make package */
	SystemPackage(COMMON, COMMON, COMMON_LISP);
	SystemPackage(KEYWORD, KEYWORD, KEYWORD);
	SystemPackage(SYSTEM, SYSTEM, SYSTEM);
	SystemPackage(CODE, CODE, CODE);
	SystemPackage(CLOS, CLOS, CLOS);
	SystemPackage(RT, RT, RT);
	Return(build_package_nicknames_());
	Return(package_char_heap_(&user, LISP_COMMON_USER));

	/* symbol setting */
	Return(intern_common_default_());
	Return(intern_symbol_header_());
	Return(intern_package_symbol_());
	Return(build_package_settings_());
	Return(set_default_package_(user));
	set_gentemp_package();
	build_default_use_package();
	Return(import_exit_and_quit_package_(user));

	return 0;
}

void build_package(void)
{
	Error(build_package_value_());
}

int getpackage_(Execute ptr, addr *ret)
{
	addr pos, type;

	GetConst(SPECIAL_PACKAGE, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	if (GetType(pos) != LISPTYPE_PACKAGE) {
		GetTypeTable(&type, Package);
		return call_type_error_(NULL, pos, type);
	}

	return Result(ret, pos);
}


/*
 *  make_package
 */
int append_nicknames_package_(addr pos, addr right)
{
	addr table, left, cons, check;

	if (right != Nil) {
		PackageTable(&table);
		while (right != Nil) {
			/* intern nickname */
			GetCons(right, &left, &right);
			Return(string_designator_heap_(&left, left, NULL));
			Return(intern_hashheap_(table, left, &cons));
			GetCdr(cons, &check);
			/* if name duplicates, check has value. */
			if (check == Nil) {
				SetCdr(cons, pos);
				/* push nickname */
				push_list_nicknames_package(pos, left);
			}
		}
	}

	return 0;
}


/*
 *  rename_package
 */
static int check_renameone_package_(
		addr table, addr name, addr root, addr right, int *ret)
{
	int check;
	addr cons;

	Return(string_designator_heap_(&name, name, NULL));
	Return(findcons_hashtable_(table, name, &cons));
	if (cons == Nil)
		return Result(ret, 0);

	/* If the argument name already registed in the table,
	 *    check unregisted a name and nicknames in package.
	 */
	Return(string_designator_heap_(&root, root, NULL));
	/* The name may unregist in table. */
	Return(string_equal_(name, root, &check));
	if (check)
		return Result(ret, 0);
	while (right != Nil) {
		GetCons(right, &root, &right);
		Return(string_designator_heap_(&root, root, NULL));
		/* The nickname may unregist in table. */
		Return(string_equal_(name, root, &check));
		if (check)
			return Result(ret, 0);
	}

	/* conflict */
	return Result(ret, 1);
}

static int check_rename_package_(addr pos, addr name, addr right)
{
	int check;
	addr table, root, roots, left;

	PackageTable(&table);
	GetPackage(pos, PACKAGE_INDEX_NAME, &root);
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &roots);

	Return(check_renameone_package_(table, name, root, roots, &check));
	if (check)
		return fmte_("Package rename ~S is conflict.", name, NULL);
	while (right != Nil) {
		GetCons(right, &left, &right);
		Return(check_renameone_package_(table, left, root, roots, &check));
		if (check)
			return fmte_("Package rename nickname ~S is conflict.", left, NULL);
	}

	return 0;
}

int delete_renameone_package_(addr table, addr name)
{
	int check;
	Return(string_designator_heap_(&name, name, NULL));
	return delete_hashtable_(table, name, &check);
}

static int delete_allnames_package_(addr pos)
{
	addr table, name, left, right;

	/* name */
	PackageTable(&table);
	GetPackage(pos, PACKAGE_INDEX_NAME, &name);
	Return(delete_renameone_package_(table, name));

	/* nicknames */
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		Return(delete_renameone_package_(table, left));
	}

	/* index */
	SetPackage(pos, PACKAGE_INDEX_NAME, Nil);
	SetPackage(pos, PACKAGE_INDEX_NICKNAME, Nil);

	return 0;
}

static int intern_renameone_package_(addr pos, addr table, addr name, int nickname)
{
	addr cons, check;

	Return(string_designator_heap_(&name, name, NULL));
	Return(intern_hashheap_(table, name, &cons));
	GetCdr(cons, &check);
	if (check == Nil) {
		SetCdr(cons, pos);

		/* nickname */
		if (nickname == 0) {
			SetPackage(pos, PACKAGE_INDEX_NAME, name);
		}
		else {
			push_list_nicknames_package(pos, name);
		}
	}

	return 0;
}

static int intern_allnames_package_(addr pos, addr name, addr right)
{
	addr table, left;

	PackageTable(&table);
	Return(intern_renameone_package_(pos, table, name, 0));
	while (right != Nil) {
		GetCons(right, &left, &right);
		Return(intern_renameone_package_(pos, table, left, 1));
	}

	return 0;
}

int rename_package_(addr pos, addr name, addr right, addr *ret)
{
	Return(package_designator_update_p_(pos, &pos));
	/* check conflict */
	Return(check_rename_package_(pos, name, right));
	/* delete name and nicknames */
	Return(delete_allnames_package_(pos));
	/* intern name and nicknames */
	Return(intern_allnames_package_(pos, name, right));
	/* result */
	return Result(ret, pos);
}


/*
 *  find-symbol
 */
int find_symbol_package_(addr package, addr name,
		addr *value, enum PACKAGE_TYPE *ret)
{
	Check(! stringp(name), "type error");
	Return(package_designator_(package, &package));
	Return(find_bitpackage_(package, name, &name));
	if (name == Nil) {
		*value = Nil;
		return Result(ret, PACKAGE_TYPE_NIL);
	}
	GetBitTypeSymbol(name, value);

	return Result(ret, StructBitType(name)->intern);
}


/*
 *  find_allsymbols
 */
static int push_basesymbol_package_(addr key, addr left, addr name, addr *cons)
{
	int check;
	addr value;

	GetPackage(left, PACKAGE_INDEX_NAME, &value);
	Return(string_equal_(key, value, &check));
	if (check) {
		GetPackage(left, PACKAGE_INDEX_TABLE, &left);
		Return(findnil_hashtable_(left, name, &left));
		if (left != Nil && StructBitType(left)->base) {
			GetBitTypeSymbol(left, &left);
			cons_heap(cons, left, *cons);
		}
	}

	return 0;
}

int find_allsymbols_package_(addr name, addr *ret)
{
	addr array, left, right, key, cons;
	size_t i, size;

	Return(string_designator_heap_(&name, name, NULL));
	cons = Nil;
	PackageTable(&array);
	GetTableHash(array, &array);
	LenArrayHash(array, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(array, i, &right);
		while (right != Nil) {
			GetCons(right, &left, &right);
			GetCons(left, &key, &left);
			Return(push_basesymbol_package_(key, left, name, &cons));
		}
	}

	return Result(ret, cons);
}


/*
 *  list_all_packages
 */
static int pushbase_package_(addr key, addr package, addr *cons)
{
	int check;
	addr value;

	GetPackage(package, PACKAGE_INDEX_NAME, &value);
	Return(string_equal_(key, value, &check));
	if (check)
		cons_heap(cons, package, *cons);

	return 0;
}

int list_all_packages_(addr *ret)
{
	addr array, left, right, key, cons;
	size_t i, size;

	cons = Nil;
	PackageTable(&array);
	GetTableHash(array, &array);
	LenArrayHash(array, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(array, i, &right);
		while (right != Nil) {
			GetCons(right, &left, &right);
			GetCons(left, &key, &left);
			Return(pushbase_package_(key, left, &cons));
		}
	}

	return Result(ret, cons);
}


/*
 *  in-package
 */
int in_package_(Execute ptr, addr package, addr *ret)
{
	addr symbol;

	GetConst(SPECIAL_PACKAGE, &symbol);
	Return(package_designator_(package, &package));
	setspecial_local(ptr, symbol, package);
	if (ret)
		*ret = package;

	return 0;
}


/*
 *  for C language
 */
int externalp_package_(addr symbol, addr package, int *ret)
{
	addr name, left, right;

	Check(! symbolp(symbol), "type error");
	CheckType(package, LISPTYPE_PACKAGE);

	/* export check */
	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &right);
	Return(findnil_hashtable_(right, name, &right));
	if (right == Nil)
		return Result(ret, 1);

	/* table */
	GetBitTypeSymbol(right, &left);
	return Result(ret, left != symbol);
}

int exportp_package_(addr symbol, addr package, int *ret)
{
	addr name, left, right;

	Check(! symbolp(symbol), "type error");
	CheckType(package, LISPTYPE_PACKAGE);

	/* export check */
	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &right);
	Return(findnil_hashtable_(right, name, &right));
	if (right == Nil)
		return Result(ret, 0);

	/* table */
	GetBitTypeSymbol(right, &left);
	return Result(ret, (left == symbol) && (int)StructBitType(right)->expt);
}

int exportp_name_package_(addr package, addr name, addr *value, int *ret)
{
	addr right;

	Check(! stringp(name), "type error");
	Return(package_designator_(package, &package));

	/* export check */
	GetPackage(package, PACKAGE_INDEX_TABLE, &right);
	Return(findnil_hashtable_(right, name, &right));
	if (right == Nil) {
		*value = Unbound;
		return Result(ret, 0);
	}

	/* table */
	GetBitTypeSymbol(right, value);
	return Result(ret, (int)StructBitType(right)->expt);
}

int checksymbol_package_(addr symbol, addr package, int *ret)
{
	enum PACKAGE_TYPE type;
	addr check, name;

	GetNameSymbol(symbol, &name);
	Return(find_symbol_package_(package, name, &check, &type));

	return Result(ret, type != PACKAGE_TYPE_NIL && check == symbol);
}

void keyword_packagetype(enum PACKAGE_TYPE type, addr *ret)
{
	switch (type) {
		case PACKAGE_TYPE_INTERNAL:
			GetConst(KEYWORD_INTERNAL, ret);
			break;

		case PACKAGE_TYPE_EXTERNAL:
			GetConst(KEYWORD_EXTERNAL, ret);
			break;

		case PACKAGE_TYPE_INHERITED:
			GetConst(KEYWORD_INHERITED, ret);
			break;

		default:
			*ret = Nil;
			break;
	}
}


/*
 *  initialize
 */
void init_package(void)
{
	init_package_designator();
	init_package_intern();
	init_package_make();
}


/************************************************************
 *  package_bittype.c
 ************************************************************/

static void alloc_bitpackage(addr *ret, addr symbol, enum PACKAGE_TYPE type)
{
	addr bit;
	struct bittype_struct *str;

	heap_smallsize(&bit, LISPSYSTEM_BITTYPE, 1, sizeof(struct bittype_struct));
	SetBitTypeSymbol(bit, symbol);
	str = StructBitType(bit);
	clearpoint(str);
	str->intern = type;
	*ret = bit;
}

static void make_bitpackage(addr *ret, addr name, addr package)
{
	addr bit, symbol;

	symbol_heap(&symbol);
	SetNameSymbol(symbol, name);
	SetPackageSymbol(symbol, package);
	alloc_bitpackage(&bit, symbol, PACKAGE_TYPE_INTERNAL);
	SetBitTypeBase(bit, 1);
	*ret = bit;
}

void make_bitpackage_symbol(addr *ret, addr *symbol, addr name, addr package)
{
	make_bitpackage(ret, name, package);
	GetBitTypeSymbol(*ret, symbol);
}

void internbitpackage(addr *ret, addr symbol)
{
	addr bit;

	alloc_bitpackage(&bit, symbol, PACKAGE_TYPE_INTERNAL);
	SetBitTypeBase(bit, 1);
	*ret = bit;
}

void importbitpackage(addr *ret, addr symbol)
{
	addr bit;

	alloc_bitpackage(&bit, symbol, PACKAGE_TYPE_INTERNAL);
	SetBitTypeImport(bit, 1);
	*ret = bit;
}

void inheritedbitpackage(addr *ret, addr symbol)
{
	addr bit;

	alloc_bitpackage(&bit, symbol, PACKAGE_TYPE_INHERITED);
	SetBitTypeInherit(bit, 1);
	*ret = bit;
}

void shadowintern_bitpackage(addr bit, addr name, addr package)
{
	addr symbol;
	struct bittype_struct *str;

	symbol_heap(&symbol);
	SetNameSymbol(symbol, name);
	SetPackageSymbol(symbol, package);
	SetBitTypeSymbol(bit, symbol);
	str = StructBitType(bit);
	str->intern = PACKAGE_TYPE_INTERNAL;
	str->base = 1;
	str->inherit = 0;
}

void shadowimport_bitpackage(addr bit, addr symbol)
{
	struct bittype_struct *str;

	SetBitTypeSymbol(bit, symbol);
	str = StructBitType(bit);
	str->intern = PACKAGE_TYPE_INTERNAL;
	str->base = 0;
	str->import = 1;
	str->inherit = 0;
}

static int intern_read_bitpackage_(addr package, addr name, addr *value, int *ret)
{
	addr table, cons;

	Check(! get_readonly_package(package), "readonly error.");
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	Return(findcons_hashtable_(table, name, &cons));
	if (cons == Nil) {
		*value = Nil;
		*ret = 0;
		return call_simple_package_error_va_(NULL,
				"Cannot intern the symbol ~S because ~S is a readonly package.",
				name, package, NULL);
	}
	GetCdr(cons, value);
	Check(*value == Nil, "bitpackage error.");
	return Result(ret, 0); /* exist */
}

static int intern_write_bitpackage_(addr package, addr name, addr *value, int *ret)
{
	addr table, cons, bit;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	Return(intern_hashheap_(table, name, &cons));
	GetCdr(cons, &bit);
	if (bit == Nil) {
		make_bitpackage(&bit, name, package);
		SetCdr(cons, bit);
		*value = bit;
		return Result(ret, 1); /* new */
	}
	*value = bit;
	return Result(ret, 0); /* exist */
}

int intern_bitpackage_(addr package, addr name, addr *value, int *ret)
{
	if (get_readonly_package(package))
		return intern_read_bitpackage_(package, name, value, ret);
	else
		return intern_write_bitpackage_(package, name, value, ret);
}

int find_bitpackage_(addr package, addr name, addr *ret)
{
	GetPackage(package, PACKAGE_INDEX_TABLE, &package);
	return findnil_hashtable_(package, name, ret);
}

int find_char_bitpackage_(addr package, const char *name, addr *ret)
{
	GetPackage(package, PACKAGE_INDEX_TABLE, &package);
	return findnil_char_hashtable_(package, name, ret);
}


/************************************************************
 *  package_common.c
 ************************************************************/

/*
 *  gentemp
 */
int make_gentemp_(Execute ptr, addr prefix, addr package, addr *ret)
{
	enum PACKAGE_TYPE type;
	int keyword;
	addr value, queue, name, gentemp;
	LocalRoot local;
	LocalStack stack;

	/* package check */
	if (package == NULL) {
		Return(getpackage_(ptr, &package));
	}
	else {
		Return(package_designator_(package, &package));
	}
	GetConst(PACKAGE_KEYWORD, &value);
	keyword = (value == package);

	/* symbol-name */
	GetConst(PACKAGE_GENTEMP, &value);
	Check(! integerp(value), "type error");

	local = ptr->local;
	for (;;) {
		/* make symbol-name */
		push_local(local, &stack);
		charqueue_local(local, &queue, 1 + 16);
		if (prefix == NULL) {
			Return(pushchar_charqueue_local_(local, queue, "T"));
		}
		else {
			Return(pushstring_charqueue_local_(local, queue, prefix));
		}
		Return(decimal_charqueue_integer_local_(local, value, queue));
		make_charqueue_local(local, queue, &name);
		Return(find_symbol_package_(package, name, &gentemp, &type));
		if (type == PACKAGE_TYPE_NIL)
			make_charqueue_heap(queue, &name);
		rollback_local(local, stack);

		/* (1+ *gentemp-counter*) */
		Return(oneplus_integer_common_(local, value, &value));
		SetConst(PACKAGE_GENTEMP, value);

		/* check intern */
		if (type == PACKAGE_TYPE_NIL)
			break;
	}

	/* gentemp */
	Return(intern_package_(package, name, &gentemp, NULL));
	if (keyword) {
		Return(export_package_(package, gentemp));
	}

	return Result(ret, gentemp);
}


/*
 *  iterator
 */
static int syscall_do_symbols_check_(Execute ptr, addr call, addr package)
{
	addr table, list, bit;
	size_t size, i;

	Return(package_designator_(package, &package));
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetTableHash(table, &table);
	LenArrayHash(table, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &bit, &list);
			GetCdr(bit, &bit);
			GetBitTypeSymbol(bit, &bit);
			Return(funcall1_control_(ptr, &bit, call, bit, NULL));
		}
	}

	return 0;
}

int do_symbols_package_(Execute ptr, addr call, addr package)
{
	return syscall_do_symbols_check_(ptr, call, package);
}

int do_external_symbols_package_(Execute ptr, addr call, addr package)
{
	addr table, list, bit;
	size_t size, i;

	Return(package_designator_(package, &package));
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetTableHash(table, &table);
	LenArrayHash(table, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &bit, &list);
			GetCdr(bit, &bit);
			if (StructBitType(bit)->intern == PACKAGE_TYPE_EXTERNAL) {
				GetBitTypeSymbol(bit, &bit);
				Return(funcall1_control_(ptr, &bit, call, bit, NULL));
			}
		}
	}

	return 0;
}

int do_all_symbols_package_(Execute ptr, addr call)
{
	int check;
	addr array, left, right, key, value;
	size_t i, size;

	PackageTable(&array);
	GetTableHash(array, &array);
	LenArrayHash(array, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(array, i, &right);
		while (right != Nil) {
			GetCons(right, &left, &right);
			GetCons(left, &key, &left);
			GetPackage(left, PACKAGE_INDEX_NAME, &value);
			Return(string_equal_(key, value, &check));
			if (check) {
				Return(syscall_do_symbols_check_(ptr, call, left));
			}
		}
	}

	return 0;
}

int all_symbols_package_(addr package, addr *ret)
{
	addr table, list, bit, root;
	size_t size, i;

	Return(package_designator_(package, &package));
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetTableHash(table, &table);
	LenArrayHash(table, &size);
	root = Nil;
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &bit, &list);
			GetCdr(bit, &bit);
			GetBitTypeSymbol(bit, &bit);
			cons_heap(&root, bit, root);
		}
	}

	return Result(ret, root);
}


/************************************************************
 *  package_defpackage.c
 ************************************************************/

/*
 *  find string
 */
static int defpackage_find_list_(addr x, addr list, int *ret)
{
	int check;
	addr y;

	while (list != Nil) {
		GetCons(list, &y, &list);
		Return(string_designator_equal_(x, y, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int defpackage_find_import_(addr x, addr list, int *ret)
{
	int check;
	addr row, y;

	while (list != Nil) {
		GetCons(list, &row, &list);
		GetCdr(row, &row); /* package-name */
		while (row != Nil) {
			GetCons(row, &y, &row);
			Return(string_designator_equal_(x, y, &check));
			if (check)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}


/*****************************************************************************
 *  Function LISP-SYSTEM::DEFPACKAGE
 *****************************************************************************/
/*
 *  restart
 */
static void defpackage_restart_import_symbol(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Intern the symbol.");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int defpackage_import_symbol_(addr package, addr name, addr *ret)
{
	enum PACKAGE_TYPE type;
	addr pos, restart, control;
	Execute ptr;

	/* find symbol */
	Return(find_symbol_package_(package, name, &pos, &type));
	if (type != PACKAGE_TYPE_NIL)
		return Result(ret, pos);

	/* restart */
	ptr = Execute_Thread;
	defpackage_restart_import_symbol(&restart);
	push_control(ptr, &control);
	pushrestart_control(ptr, restart);

	*ret = Nil;
	(void)call_simple_package_error_va_(ptr,
			"The symbol ~S is not exist in the ~S package.",
			name, package, NULL);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* continue */
	if (ptr->throw_handler == restart) {
		normal_throw_control(ptr);
		Return(intern_package_(package, name, ret, NULL));
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}


/*
 *  defpackage
 */
static int defpackage_update_nicknames_(addr pos, addr list)
{
	addr name;

	getname_package_unsafe(pos, &name);
	return rename_package_(pos, name, list, &pos);
}

static int defpackage_update_shadowing_(addr pos, addr list)
{
	addr child, package, key;

	while (list != Nil) {
		GetCons(list, &child, &list);
		GetCons(child, &package, &child);
		Return(package_designator_(package, &package));
		while (child != Nil) {
			GetCons(child, &key, &child);
			Return(string_designator_heap_(&key, key, NULL));
			Return(defpackage_import_symbol_(package, key, &key));
			Return(shadowing_import_package_(pos, key));
		}
	}

	return 0;
}

static int defpackage_update_import_(LocalRoot local, addr pos, addr list)
{
	addr child, package, args, symbol;
	LocalStack stack;

	while (list != Nil) {
		GetCons(list, &child, &list);
		GetCons(child, &package, &child);
		Return(package_designator_(package, &package));
		push_local(local, &stack);
		for (args = Nil; child != Nil; ) {
			GetCons(child, &symbol, &child);
			Return(string_designator_heap_(&symbol, symbol, NULL));
			Return(defpackage_import_symbol_(package, symbol, &symbol));
			cons_local(local, &args, symbol, args);
		}
		nreverse(&args, args);
		Return(import_package_(pos, args));
		rollback_local(local, stack);
	}

	return 0;
}

static int defpackage_update_intern_(addr pos, addr list)
{
	addr name;

	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(intern_package_table_(pos, name, &name, NULL));
	}

	return 0;
}

static int defpackage_export_(addr pos, addr list)
{
	enum PACKAGE_TYPE type;
	addr name, root, symbol;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(intern_package_(pos, name, &symbol, &type));
		cons_heap(&root, symbol, root);
	}

	return export_package_(pos, root);
}

static int defpackage_rest_nicknames_(addr pg, addr rest, addr *ret)
{
	int check;
	addr list, pos, type;

	if (GetKeyArgs(rest, KEYWORD_NICKNAMES, &list))
		return Result(ret, Unbound);

	*ret = list;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designator_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_(NULL, pos, type);
		}

		/* nickname check */
		Return(find_package_(pos, &pos));
		if (pos != Nil && pos != pg) {
			return call_simple_package_error_va_(NULL,
					":NICKNAMES ~S is already used.", pos, NULL);
		}
	}

	return 0;
}

static int defpackage_rest_use_(addr rest, addr *ret)
{
	addr list, pos, type;

	if (GetKeyArgs(rest, KEYWORD_USE, &list))
		return Result(ret, Nil);

	*ret = list;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		if (! package_designator_p(pos)) {
			GetTypeTable(&type, PackageDesignator);
			return call_type_error_(NULL, pos, type);
		}
	}

	return 0;
}

static int defpackage_rest_string_(addr rest, constindex index, addr *ret)
{
	addr list, pos, type;

	if (getplist_constant_safe(rest, index, &list))
		return Result(ret, Nil);

	*ret = list;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! string_designator_p(pos)) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_(NULL, pos, type);
		}
	}

	return 0;
}

static int defpackage_rest_import_line_(addr list)
{
	addr pos, type;

	/* package */
	Return_getcons(list, &pos, &list);
	if (! package_designator_p(pos)) {
		GetTypeTable(&type, PackageDesignator);
		return call_type_error_(NULL, pos, type);
	}

	/* string */
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! string_designator_p(pos)) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_(NULL, pos, type);
		}
	}

	return 0;
}

static int defpackage_rest_import_(addr rest, constindex index, addr *ret)
{
	addr list, pos;

	if (getplist_constant_safe(rest, index, &list))
		return Result(ret, Nil);

	*ret = list;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(defpackage_rest_import_line_(pos));
	}

	return 0;
}

static int defpackage_disjoin_shadow_(addr shadow,
		addr intern, addr import, addr shadowing)
{
	int check;
	addr pos;

	while (shadow != Nil) {
		Return_getcons(shadow, &pos, &shadow);
		Check(! string_designator_p(pos), "type error");

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":SHADOW ~S already exists in :INTERN.", pos, NULL);
		}

		/* import-from check */
		Return(defpackage_find_import_(pos, import, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":SHADOW ~S already exists in :IMPORT-FROM.", pos, NULL);
		}

		/* shadowing-import-from check */
		Return(defpackage_find_import_(pos, shadowing, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":SHADOW ~S already exists in :SHADOWING-IMPORT-FROM.", pos, NULL);
		}
	}

	return 0;
}

static int defpackage_disjoin_import_list_(addr list,
		addr shadow, addr intern, addr shadowing)
{
	int check;
	addr pos;

	/* package name */
	Return_getcdr(list, &list);

	/* symbols */
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Check(! string_designator_p(pos), "type error");

		/* shadow check */
		Return(defpackage_find_list_(pos, shadow, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":IMPORT-FROM ~S already exists in :SHADOW.", pos, NULL);
		}

		/* shadowing-import-from check */
		Return(defpackage_find_import_(pos, shadowing, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":IMPORT-FROM ~S already exists in :SHADOWING-IMPORT-FROM.",
					pos, NULL);
		}

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":IMPORT-FROM ~S already exists in :INTERN.", pos, NULL);
		}
	}

	return 0;
}

static int defpackage_disjoin_import_(addr import,
		addr shadow, addr intern, addr shadowing)
{
	addr list;

	while (import != Nil) {
		Return_getcons(import, &list, &import);
		Return(defpackage_disjoin_import_list_(list, shadow, intern, shadowing));
	}

	return 0;
}

static int defpackage_disjoin_shadowing_list_(addr list,
		addr shadow, addr intern, addr import)
{
	int check;
	addr pos;

	/* package name */
	Return_getcdr(list, &list);

	/* symbols */
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Check(! string_designator_p(pos), "type error");

		/* shadow check */
		Return(defpackage_find_list_(pos, shadow, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":SHADOWING-IMPORT-FROM ~S already exists in :SHADOW.",
					pos, NULL);
		}

		/* import-from check */
		Return(defpackage_find_import_(pos, import, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":SHADOWING-IMPORT-FROM ~S already exists in :IMPORT-FROM.",
					pos, NULL);
		}

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":SHADOWING-IMPORT-FROM ~S already exists in :INTERN.",
					pos, NULL);
		}
	}

	return 0;
}

static int defpackage_disjoin_shadowing_(addr shadowing,
		addr shadow, addr intern, addr import)
{
	addr list;

	while (shadowing != Nil) {
		Return_getcons(shadowing, &list, &shadowing);
		Return(defpackage_disjoin_shadowing_list_(list, shadow, intern, import));
	}

	return 0;
}

static int defpackage_disjoin_intern_(addr intern,
		addr shadow, addr import, addr shadowing, addr expt)
{
	int check;
	addr pos;

	while (intern != Nil) {
		Return_getcons(intern, &pos, &intern);
		Check(! string_designator_p(pos), "type error");

		/* shadow check */
		Return(defpackage_find_list_(pos, shadow, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":INTERN ~S already exists in :SHADOW.", pos, NULL);
		}

		/* shadowing-import-from check */
		Return(defpackage_find_import_(pos, shadowing, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":INTERN ~S already exists in :SHADOWING-IMPORT-FROM.",
					pos, NULL);
		}

		/* import-from check */
		Return(defpackage_find_import_(pos, import, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":INTERN~S already exists in :IMPORT-FROM.", pos, NULL);
		}

		/* export check */
		Return(defpackage_find_list_(pos, expt, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":INTERN ~S already exists in :EXPORT.", pos, NULL);
		}
	}

	return 0;
}

static int defpackage_disjoin_export_(addr expt, addr intern)
{
	int check;
	addr pos;

	while (expt != Nil) {
		Return_getcons(expt, &pos, &expt);
		Check(! string_designator_p(pos), "type error");

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":EXPORT ~S already exists in :INTERN.", pos, NULL);
		}
	}

	return 0;
}

static int defpackage_update_(Execute ptr, addr pos, addr rest)
{
	addr names, use, shadow, shadowing, import, expt, intern;

	/* &rest */
	Return(defpackage_rest_nicknames_(pos, rest, &names));
	Return(defpackage_rest_use_(rest, &use));
	Return(defpackage_rest_string_(rest, CONSTANT_KEYWORD_SHADOW, &shadow));
	Return(defpackage_rest_import_(rest,
				CONSTANT_KEYWORD_SHADOWING_IMPORT_FROM, &shadowing));
	Return(defpackage_rest_import_(rest,
				CONSTANT_KEYWORD_IMPORT_FROM, &import));
	Return(defpackage_rest_string_(rest, CONSTANT_KEYWORD_EXPORT, &expt));
	Return(defpackage_rest_string_(rest, CONSTANT_KEYWORD_INTERN, &intern));

	/* Check disjoin */
	Return(defpackage_disjoin_shadow_(shadow, intern, import, shadowing));
	Return(defpackage_disjoin_import_(import, shadow, intern, shadowing));
	Return(defpackage_disjoin_shadowing_(shadowing, shadow, intern, import));
	Return(defpackage_disjoin_intern_(intern, shadow, import, shadowing, expt));
	Return(defpackage_disjoin_export_(expt, intern));

	/*
	 *  The order is
	 *    0. :nicknames
	 *    1. :shadow and :shadowing-import-from.
	 *    2. :use.
	 *    3. :import-from and :intern.
	 *    4. :export.
	 */
	/* nicknames */
	if (names != Unbound) {
		Return(defpackage_update_nicknames_(pos, names));
	}

	/* shadow, shadowing-symbols */
	if (shadow != Nil) {
		Return(shadow_package_(pos, shadow));
	}
	if (shadowing != Nil) {
		Return(defpackage_update_shadowing_(pos, shadowing));
	}

	/* use */
	if (use != Nil) {
		Return(use_package_(pos, use));
	}

	/* import-from */
	if (import != Nil) {
		Return(defpackage_update_import_(ptr->local, pos, import));
	}

	/* intern */
	if (intern != Nil) {
		Return(defpackage_update_intern_(pos, intern));
	}

	/* export */
	if (expt != Nil) {
		Return(defpackage_export_(pos, expt));
	}

	return 0;
}

static int defpackage_make_(Execute ptr, addr pos, addr rest)
{
	int check;
	addr control, save;

	if (! defpackage_update_(ptr, pos, rest))
		return 0;

	/* escape */
	push_control(ptr, &control);
	save_execute_control(ptr, &save);
	normal_throw_control(ptr);
	if (delete_package_(pos, &check))
		goto escape;
	restore_execute_control(ptr, save);
escape:
	return pop_control_(ptr, control);
}


/* size */
static int defpackage_size_(addr rest, int *sizep, size_t *ret)
{
	addr value;

	if (GetKeyArgs(rest, KEYWORD_SIZE, &value)) {
		*sizep = 0;
		*ret = 0;
		return 0;
	}

	*sizep = 1;
	if (GetIndex_integer(value, ret)) {
		*ret = 0;
		return fmte_(":SIZE ~S is too large.", value, NULL);
	}

	return 0;
}


/* documentation */
static int defpackage_documentation_(addr rest, addr *ret)
{
	addr pos;

	/* keyword */
	if (GetKeyArgs(rest, KEYWORD_DOCUMENTATION, &pos))
		return Result(ret, Nil);

	/* type check */
	if (! stringp(pos)) {
		*ret = Nil;
		return TypeError_(pos, STRING);
	}

	return Result(ret, pos);
}


/*
 *  defpackage
 */
static int defpackage_execute_make_(Execute ptr,
		addr var, addr rest, int sizep, size_t size, addr *ret)
{
	addr pos;

	if (sizep) {
		Return(package_size_heap_(&pos, var, size));
	}
	else {
		Return(package_heap_(&pos, var));
	}

	Return(defpackage_make_(ptr, pos, rest));
	return Result(ret, pos);
}

static int defpackage_execute_update_(Execute ptr,
		addr pos, addr rest, int sizep, size_t size)
{
	addr hash;

	if (sizep) {
		GetPackage(pos, PACKAGE_INDEX_TABLE, &hash);
		Return(force_resize_hashtable_(hash, size));
	}

	return defpackage_update_(ptr, pos, rest);
}

int defpackage_execute_(Execute ptr, addr var, addr rest, addr *ret)
{
	int sizep;
	addr doc, pos;
	size_t size;

	/* name */
	Return(string_designator_heap_(&var, var, NULL));

	/* :SIZE */
	Return(defpackage_size_(rest, &sizep, &size));

	/* :DOCUMENTATION */
	Return(defpackage_documentation_(rest, &doc));

	/* package */
	Return(find_package_(var, &pos));
	if (pos == Nil) {
		Return(defpackage_execute_make_(ptr, var, rest, sizep, size, &pos));
	}
	else {
		Return(package_designator_update_p_(pos, &pos));
		Return(defpackage_execute_update_(ptr, pos, rest, sizep, size));
	}
	setdocument_package(pos, doc);
	return Result(ret, pos);
}


/*****************************************************************************
 *  Macro COMMON-LISP:DEFPACKAGE
 *****************************************************************************/
static int defpackage_package_designator_common_(addr *value, addr pos, int *ret)
{
	/* type check */
	if (! package_designator_p(pos)) {
		*ret = 0;
		return 0;
	}

	/* object */
	if (packagep(pos))
		getname_package_unsafe(pos, &pos);
	return string_designator_heap_(value, pos, ret);
}

static int defpackage_nicknames_common_(addr *ret, addr info, addr list)
{
	int check;
	addr pos, type;

	*ret = Nil;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(string_designator_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_va_(NULL, pos, type,
					":NICKNAME ~S must be a string-designator.", pos, NULL);
		}
		cons_heap(&info, pos, info);
	}

	return Result(ret, info);
}

static int defpackage_documentation_common_(addr *ret, addr info, addr list)
{
	addr doc, check, type;

	*ret = Nil;
	if (! consp_getcons(list, &doc, &check)) {
		return fmte_(":DOCUMENTATION option ~S don't allow a dotted list.", list, NULL);
	}

	if (! stringp(doc)) {
		GetTypeTable(&type, String);
		return call_type_error_va_(NULL, doc, type,
				":DOCUMENTATION ~S must be a string.", doc, NULL);
	}

	if (check != Nil) {
		return fmte_(":DOCUMENTATION argument ~S must be a single list.", list, NULL);
	}

	if (info != Nil) {
		return call_simple_program_error_va_(NULL,
				":DOCUMENTATION option don't accept "
				"multiple defines ~S and ~S.", info, doc, NULL);
	}

	return Result(ret, doc);
}

static int defpackage_use_common_(addr *ret, addr info, addr list)
{
	int check;
	addr pos, type;

	*ret = Nil;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(defpackage_package_designator_common_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, PackageDesignator);
			return call_type_error_va_(NULL, pos, type,
					":USE ~S must be a package-designator.", pos, NULL);
		}
		cons_heap(&info, pos, info);
	}

	return Result(ret, info);
}

static int defpackage_shadow_common_(addr *ret,
		addr shadow, addr shadowing, addr import, addr intern, addr list)
{
	int check;
	addr pos, type;

	*ret = Nil;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designator_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_va_(NULL, pos, type,
					":SHADOW ~S must be a string-designator.", pos, NULL);
		}

		/* shadowing-import-from check */
		Return(defpackage_find_import_(pos, shadowing, &check));
		if (check) {
			Return(fmtw_(":SHADOW ~S "
						"already exists in :SHADOWING-IMPORT-FROM.", pos, NULL));
		}

		/* import-from check */
		Return(defpackage_find_import_(pos, import, &check));
		if (check) {
			Return(fmtw_(":SHADOW ~S already exists in :IMPORT-FROM.", pos, NULL));
		}

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			Return(fmtw_(":SHADOW ~S already exists in :INTERN.", pos, NULL));
		}

		/* push */
		cons_heap(&shadow, pos, shadow);
	}

	return Result(ret, shadow);
}

static int defpackage_shadowing_common_(addr *ret,
		addr shadow, addr shadowing, addr import, addr intern, addr list)
{
	int check;
	addr pos, type, row;

	/* package name */
	*ret = Nil;
	Return_getcons(list, &pos, &list);
	Return(defpackage_package_designator_common_(&pos, pos, &check));
	if (! check) {
		GetTypeTable(&type, PackageDesignator);
		return call_type_error_va_(NULL, pos, type,
				":SHADOWING-IMPORT-FROM first argument ~S "
				"must be a package-designator.", pos, NULL);
	}
	conscar_heap(&row, pos);

	/* symbols */
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designator_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_va_(NULL, pos, type,
					":SHADOWING-IMPORT-FROM ~S "
					"must be a string-designator.", pos, NULL);
		}

		/* shadow check */
		Return(defpackage_find_list_(pos, shadow, &check));
		if (check) {
			Return(fmtw_(":SHADOWING-IMPORT-FROM ~S "
						"already exists in :SHADOW.", pos, NULL));
		}

		/* import-from check */
		Return(defpackage_find_import_(pos, import, &check));
		if (check) {
			Return(fmtw_(":SHADOWING-IMPORT-FROM ~S "
						"already exists in :IMPORT-FROM.", pos, NULL));
		}

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			Return(fmtw_(":SHADOWING-IMPORT-FROM ~S "
						"already exists in :INTERN.", pos, NULL));
		}

		/* push */
		cons_heap(&row, pos, row);
	}

	nreverse(&row, row);
	cons_heap(ret, row, shadowing);
	return 0;
}

static int defpackage_import_common_(addr *ret,
		addr shadow, addr shadowing, addr import, addr intern, addr list)
{
	int check;
	addr pos, type, row;

	/* package name */
	*ret = Nil;
	Return_getcons(list, &pos, &list);
	Return(defpackage_package_designator_common_(&pos, pos, &check));
	if (! check) {
		GetTypeTable(&type, PackageDesignator);
		return call_type_error_va_(NULL, pos, type,
				":IMPORT-FROM first argument ~S "
				"must be a package-designator.", pos, NULL);
	}
	conscar_heap(&row, pos);

	/* symbols */
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designator_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_va_(NULL, pos, type,
					":IMPORT-FROM ~S must be a string-designator.", pos, NULL);
		}

		/* shadow check */
		Return(defpackage_find_list_(pos, shadow, &check));
		if (check) {
			Return(fmtw_(":IMPORT-FROM ~S already exists in :SHADOW.", pos, NULL));
		}

		/* shadowing-import-from check */
		Return(defpackage_find_import_(pos, shadowing, &check));
		if (check) {
			Return(fmtw_(":IMPORT-FROM ~S "
						"already exists in :SHADOWING-IMPORT-FROM.", pos, NULL));
		}

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			Return(fmtw_(":IMPORT-FROM ~S already exists in :INTERN.", pos, NULL));
		}

		/* push */
		cons_heap(&row, pos, row);
	}

	nreverse(&row, row);
	cons_heap(ret, row, import);
	return 0;
}

static int defpackage_export_common_(addr *ret, addr expt, addr intern, addr list)
{
	int check;
	addr pos, type;

	*ret = Nil;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designator_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_va_(NULL, pos, type,
					":EXPORT ~S must be a string-designator.", pos, NULL);
		}

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			Return(fmtw_(":EXPORT ~S already exists in :INTERN.", pos, NULL));
		}

		/* push */
		cons_heap(&expt, pos, expt);
	}

	return Result(ret, expt);
}

static int defpackage_intern_common_(addr *ret,
		addr shadow, addr shadowing, addr import, addr expt, addr intern, addr list)
{
	int check;
	addr pos, type;

	*ret = Nil;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designator_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_va_(NULL, pos, type,
					":INTERN ~S must be a string-designator.", pos, NULL);
		}

		/* shadow check */
		Return(defpackage_find_list_(pos, shadow, &check));
		if (check) {
			Return(fmtw_(":INTERN ~S already exists in :SHADOW.", pos, NULL));
		}

		/* shadowing-import-from check */
		Return(defpackage_find_import_(pos, shadowing, &check));
		if (check) {
			Return(fmtw_(":INTERN ~S "
						"already exists in :SHADOWING-IMPORT-FROM.", pos, NULL));
		}

		/* import-from check */
		Return(defpackage_find_import_(pos, import, &check));
		if (check) {
			Return(fmtw_(":INTERN ~S already exists in :IMPORT-FROM.", pos, NULL));
		}

		/* export check */
		Return(defpackage_find_list_(pos, expt, &check));
		if (check) {
			Return(fmtw_(":INTERN ~S already exists in :EXPORT.", pos, NULL));
		}

		/* push */
		cons_heap(&intern, pos, intern);
	}

	return Result(ret, intern);
}

static int defpackage_size_common_(addr *ret, addr info, addr list)
{
	int check;
	addr size, value, type;

	*ret = Nil;
	if (! consp_getcons(list, &size, &value)) {
		return fmte_(":SIZE option ~S don't allow a dotted list.", list, NULL);
	}

	if (! integerp(size)) {
		GetTypeTable(&type, Intplus);
		return call_type_error_va_(NULL, size, type,
				":SIZE ~S must be a positive integer.", size, NULL);
	}

	Return(minusp_integer_(size, &check));
	if (check) {
		GetTypeTable(&type, Intplus);
		return call_type_error_va_(NULL, size, type,
				":SIZE ~S must be a positive integer.", size, NULL);
	}

	if (value != Nil) {
		return fmte_(":SIZE argument ~S must be a single list.", list, NULL);
	}

	if (info != Nil) {
		return call_simple_program_error_va_(NULL,
				":SIZE option don't accept "
				"multiple defines ~S and ~S.", info, size, NULL);
	}

	return Result(ret, size);
}

static int defpackage_expand_common_(addr name, addr form, addr *ret)
{
	addr args, key, list, quote;
	addr knick, kdoc, kuse, kshadow, kshadowing, kimport, kexport, kintern, ksize;
	addr nicknames, doc, use, shadow, shadowing, import, expt, intern, size;

	GetConst(KEYWORD_NICKNAMES, &knick);
	GetConst(KEYWORD_DOCUMENTATION, &kdoc);
	GetConst(KEYWORD_USE, &kuse);
	GetConst(KEYWORD_SHADOW, &kshadow);
	GetConst(KEYWORD_SHADOWING_IMPORT_FROM, &kshadowing);
	GetConst(KEYWORD_IMPORT_FROM, &kimport);
	GetConst(KEYWORD_EXPORT, &kexport);
	GetConst(KEYWORD_INTERN, &kintern);
	GetConst(KEYWORD_SIZE, &ksize);

	nicknames = doc = use = shadow = shadowing = import = expt = intern = size = Nil;
	for (args = form; args != Nil; ) {
		if (! consp_getcons(args, &list, &args)) {
			return fmte_("The defpackage option ~S "
					"don't allow a dotted list.", form, NULL);
		}
		if (! consp_getcons(list, &key, &list)) {
			return call_simple_program_error_va_(NULL,
					"The defpackage option ~S must be a cons.", list, NULL);
		}
		if (key == knick) {
			Return(defpackage_nicknames_common_(&nicknames, nicknames, list));
		}
		else if (key == kdoc) {
			Return(defpackage_documentation_common_(&doc, doc, list));
		}
		else if (key == kuse) {
			Return(defpackage_use_common_(&use, use, list));
		}
		else if (key == kshadow) {
			Return(defpackage_shadow_common_(&shadow,
						shadow, shadowing, import, intern, list));
		}
		else if (key == kshadowing) {
			Return(defpackage_shadowing_common_(&shadowing,
						shadow, shadowing, import, intern, list));
		}
		else if (key == kimport) {
			Return(defpackage_import_common_(&import,
						shadow, shadowing, import, intern, list));
		}
		else if (key == kexport) {
			Return(defpackage_export_common_(&expt, expt, intern, list));
		}
		else if (key == kintern) {
			Return(defpackage_intern_common_(&intern,
						shadow, shadowing, import, expt, intern, list));
		}
		else if (key == ksize) {
			Return(defpackage_size_common_(&size, size, list));
		}
		else {
			return call_simple_program_error_va_(NULL,
					"Invalid defpackage option ~S.", key, NULL);
		}
	}

	/* lisp-system::defpackage */
	nreverse(&nicknames, nicknames);
	nreverse(&use, use);
	nreverse(&shadow, shadow);
	nreverse(&shadowing, shadowing);
	nreverse(&import, import);
	nreverse(&intern, intern);
	GetConst(SYSTEM_DEFPACKAGE, &form);
	GetConst(COMMON_QUOTE, &quote);
	/* (lisp-system::defpackage name ...) */
	list = Nil;
	cons_heap(&list, form, list);
	cons_heap(&list, name, list);
	/* :SIZE */
	if (size != Nil) {
		cons_heap(&list, ksize, list);
		cons_heap(&list, size, list);
	}
	/* :DOCUMENTATION */
	if (doc != Nil) {
		cons_heap(&list, kdoc, list);
		cons_heap(&list, doc, list);
	}
	/* :NICKNAMES */
	if (nicknames != Nil) {
		cons_heap(&list, knick, list);
		list_heap(&nicknames, quote, nicknames, NULL);
		cons_heap(&list, nicknames, list);
	}
	/* :USE */
	if (use != Nil) {
		cons_heap(&list, kuse, list);
		list_heap(&use, quote, use, NULL);
		cons_heap(&list, use, list);
	}
	/* :SHADOW */
	if (shadow != Nil) {
		cons_heap(&list, kshadow, list);
		list_heap(&shadow, quote, shadow, NULL);
		cons_heap(&list, shadow, list);
	}
	/* :SHADOWING-IMPORT-FROM */
	if (shadowing != Nil) {
		cons_heap(&list, kshadowing, list);
		list_heap(&shadowing, quote, shadowing, NULL);
		cons_heap(&list, shadowing, list);
	}
	/* :IMPORT-FROM */
	if (import != Nil) {
		cons_heap(&list, kimport, list);
		list_heap(&import, quote, import, NULL);
		cons_heap(&list, import, list);
	}
	/* :EXPORT */
	if (expt != Nil) {
		cons_heap(&list, kexport, list);
		list_heap(&expt, quote, expt, NULL);
		cons_heap(&list, expt, list);
	}
	/* :INTERN */
	if (intern != Nil) {
		cons_heap(&list, kintern, list);
		list_heap(&intern, quote, intern, NULL);
		cons_heap(&list, intern, list);
	}
	/* result */
	nreverse(ret, list);
	return 0;
}

int defpackage_common_(addr form, addr env, addr *ret)
{
	int check;
	addr name, type;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &name, &form)) {
		return fmte_("DEFPACKAGE argument ~S "
				"must be (name &rest options).", form, NULL);
	}
	Return(string_designator_heap_(&name, name, &check));
	if (! check) {
		GetTypeTable(&type, StringDesignator);
		return call_type_error_va_(NULL, name, type,
				"DEFPACKAGE name ~S must be a string-designator.", name, NULL);
	}

	Return(defpackage_expand_common_(name, form, &form));
	return Result(ret, form);
}


/************************************************************
 *  package_delete.c
 ************************************************************/

/*
 *  name error
 */
static void delete_package_name_restart(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Ignore");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PROGRAM_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int delete_package_name_call_(addr pos, addr *ret)
{
	addr check;

	Return(find_package_(pos, &check));
	if (check != Nil)
		return Result(ret, check);

	*ret = Nil;
	return call_simple_package_error_va_(NULL, "No such a package ~S.", pos, NULL);
}

static int delete_package_name_(addr pos, addr *ret)
{
	addr restart, control;
	Execute ptr;

	ptr = Execute_Thread;
	delete_package_name_restart(&restart);
	push_control(ptr, &control);
	pushrestart_control(ptr, restart);
	if (delete_package_name_call_(pos, ret)) {
		if (ptr->throw_control == control) {
			normal_throw_control(ptr);
			*ret = Nil;
		}
	}

	return pop_control_(ptr, control);
}


/*
 *  use-package error
 */
static void delete_package_used_restart(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "unuse-package.");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PROGRAM_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int delete_package_used_call_(addr pos)
{
	addr list;

	GetPackage(pos, PACKAGE_INDEX_USED, &list);
	if (list != Nil) {
		return call_simple_package_error_va_(NULL,
				"Package ~S is used by ~S.", pos, list, NULL);
	}

	return 0;
}

static int delete_package_used_unuse_(addr pos)
{
	addr list, x;

	GetPackage(pos, PACKAGE_INDEX_USED, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		Return(unuse_package_(x, pos));
	}
#ifdef LISP_DEBUG
	GetPackage(pos, PACKAGE_INDEX_USED, &list);
	Check(list != Nil, "used-list error.");
#endif

	return 0;
}

static int delete_package_used_(addr pos)
{
	addr restart, control;
	Execute ptr;

	ptr = Execute_Thread;
	delete_package_used_restart(&restart);
	push_control(ptr, &control);
	pushrestart_control(ptr, restart);
	if (delete_package_used_call_(pos)) {
		if (ptr->throw_control == control) {
			normal_throw_control(ptr);
			(void)delete_package_used_unuse_(pos);
		}
	}

	return pop_control_(ptr, control);
}


/*
 *  delete_package
 */
static int allunintern_uselist_package_(addr pos)
{
	addr left, right, root;

	GetPackage(pos, PACKAGE_INDEX_EXPORT, &root);
	GetPackage(pos, PACKAGE_INDEX_USE, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		delete_list_used_package(left, pos);
	}

	return 0;
}

static void all_unintern_package(addr pos)
{
	addr table, array, left, right;
	size_t i, size;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &table);

	/* all unintern */
	GetTableHash(table, &array);
	LenArrayHash(array, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(array, i, &right);
		while (right != Nil) {
			GetCons(right, &left, &right);
			GetCdr(left, &left);

			/* package nil */
			if (StructBitType(left)->base) {
				GetBitTypeSymbol(left, &left);
				SetPackageSymbol(left, Nil);
			}
		}
	}
	clear_hashtable(table);

	SetPackage(pos, PACKAGE_INDEX_USE, Nil);
	SetPackage(pos, PACKAGE_INDEX_SHADOW, Nil);
	SetPackage(pos, PACKAGE_INDEX_EXPORT, Nil);
}

/*
 *  return 0:  delete package.
 *  return 1:  package name is nil.
 */
int delete_package_(addr pos, int *ret)
{
	int check;
	addr name, right, table;

	/* name check */
	Return(delete_package_name_(pos, &pos));
	if (pos == Nil) {
		/* Do nothing */
		return Result(ret, 1);
	}
	CheckType(pos, LISPTYPE_PACKAGE);

	/* delete check */
	GetPackage(pos, PACKAGE_INDEX_NAME, &name);
	if (name == Nil) {
		/* package object already deleted. */
		return Result(ret, 1);
	}

	/* readonly */
	Return(package_designator_update_p_(pos, &pos));

	/* used-by-list */
	Return(delete_package_used_(pos));

	/* all symbon unintern in use-list. */
	Return(allunintern_uselist_package_(pos));

	/* all symbol unintern in my package. */
	all_unintern_package(pos);

	/* delete name and nickname */
	PackageTable(&table);
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &right);
	Return(delete_hashtable_(table, name, &check));
	while (right != Nil) {
		GetCons(right, &name, &right);
		Return(delete_hashtable_(table, name, &check));
	}
	SetPackage(pos, PACKAGE_INDEX_NICKNAME, Nil);
	SetPackage(pos, PACKAGE_INDEX_NAME, Nil);

	return Result(ret, 0);
}


/************************************************************
 *  package_designator.c
 ************************************************************/

/*
 *  check
 */
int package_designator_p(addr pos)
{
	return packagep(pos) || string_designator_p(pos);
}

int package_designator_equal_(addr left, addr right, int *ret)
{
	if (packagep(left))
		GetPackage(left, PACKAGE_INDEX_NAME, &left);
	if (packagep(right))
		GetPackage(right, PACKAGE_INDEX_NAME, &right);

	return string_designator_equal_(left, right, ret);
}


/*
 *  package-designator
 */
static int function_package_designator_interactive(Execute ptr)
{
	addr prompt, pos;

	strvect_char_heap(&prompt, "Input package name: ");
	Return(prompt_for_stream_(ptr, T, prompt, &pos));
	list_heap(&pos, pos, NULL);
	setresult_control(ptr, pos);

	return 0;
}

static void compiled_use_value_interactive_package_designator(addr *ret)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, p_defun_package_designator_interactive);
	*ret = pos;
}

static void restart_use_value_package_designator(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_USE_VALUE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Input another package.");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_VALUES, &pos);
	setfunction_restart(restart, pos);
	/* interactive */
	compiled_use_value_interactive_package_designator(&pos);
	setinteractive_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int restart_package_designator_(addr pos, addr *ret)
{
	addr restart, control;
	Execute ptr;

	ptr = Execute_Thread;
	restart_use_value_package_designator(&restart);
	push_control(ptr, &control);
	gchold_push_force_local(ptr->local, pos);
	pushrestart_control(ptr, restart);

	(void)call_simple_package_error_va_(NULL, "No such a package ~S.", pos, NULL);
	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* use_value */
	normal_throw_control(ptr);
	getresult_control(ptr, ret);

escape:
	return pop_control_(ptr, control);
}

int package_designator_(addr pos, addr *ret)
{
	addr x;

	for (;;) {
		Return(find_package_(pos, &x));
		if (x != Nil)
			break;
		Return(restart_package_designator_(pos, &pos));
	}

	return Result(ret, x);
}

int package_designator_update_p_(addr pos, addr *ret)
{
	Return(package_designator_(pos, &pos));
	if (get_readonly_package(pos)) {
		return call_simple_package_error_va_(NULL,
				"~S is a readonly package.", pos, NULL);
	}

	return Result(ret, pos);
}

void init_package_designator(void)
{
	SetPointerCall(defun, empty, package_designator_interactive);
}


/************************************************************
 *  package_export.c
 ************************************************************/

/****************************************************************************
 *  Function EXPORT
 ****************************************************************************/
/*
 *  restart conflict
 *    shadow    Make ~S accessible (shadowing ~S).
 *    unintern  Make ~S accessible (unintern ~S).
 *    ignore    Ignore export.
 */
static int restart_shadow_export_package_(Execute ptr,
		addr *ret, addr symbol1, addr symbol2)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_SHADOW, &pos);
	restart_heap(&restart, pos);
	/* report */
	Return(format_string_(ptr, &pos,
				"Keep ~S accessible. (shadowing ~S)",
				symbol2, symbol1, NULL));
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	return Result(ret, restart);
}

static int restart_unintern_export_package_(Execute ptr,
		addr *ret, addr symbol1, addr symbol2)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_UNINTERN, &pos);
	restart_heap(&restart, pos);
	/* report */
	Return(format_string_(ptr, &pos,
				"Make ~S accessible. (unintern ~S)",
				symbol1, symbol2, NULL));
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	return Result(ret, restart);
}

static void restart_ignore_export_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_IGNORE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Ignore export.");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int restart_conflict_export_package_(
		addr package1, addr symbol1,
		addr package2, addr symbol2)
{
	addr shadow, unintern, ignore, control;
	Execute ptr;

	ptr = Execute_Thread;
	Return(restart_shadow_export_package_(ptr, &shadow, symbol1, symbol2));
	Return(restart_unintern_export_package_(ptr, &unintern, symbol1, symbol2));
	restart_ignore_export_package(&ignore);

	push_control(ptr, &control);
	pushrestart_control(ptr, ignore);
	pushrestart_control(ptr, unintern);
	pushrestart_control(ptr, shadow);
	(void)call_simple_package_error_va_(ptr,
			"The symbol ~S occer conflict between ~S and ~S.",
			symbol1, package1, package2, NULL);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* shadow */
	if (ptr->throw_handler == shadow) {
		normal_throw_control(ptr);
		Return(shadow_package_(package2, symbol1));
		goto escape;
	}

	/* unintern */
	if (ptr->throw_handler == unintern) {
		normal_throw_control(ptr);
		Return(unintern_package_(package2, symbol2, NULL));
		goto escape;
	}

	/* ignore */
	if (ptr->throw_handler == ignore) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}


/*
 *  restart accessible
 *    import  Import the symbol.
 *    ignore  Ignore export.
 */
static void restart_import_export_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_IMPORT, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Import the symbol.");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int restart_pop_export_package_(Execute ptr,
		addr control, addr import, addr ignore,
		addr package, addr symbol)
{
	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* import */
	if (ptr->throw_handler == import) {
		normal_throw_control(ptr);
		Return(import_package_(package, symbol));
		goto escape;
	}

	/* ignore */
	if (ptr->throw_handler == ignore) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}

static int restart_exist_export_package_(addr package, addr symbol)
{
	addr pos1, pos2, control;
	Execute ptr;

	ptr = Execute_Thread;
	restart_import_export_package(&pos1);
	restart_ignore_export_package(&pos2);

	push_control(ptr, &control);
	pushrestart_control(ptr, pos2);
	pushrestart_control(ptr, pos1);
	(void)call_simple_package_error_va_(ptr,
			"There is no symbol ~S in package ~S.",
			symbol, package, NULL);
	return restart_pop_export_package_(ptr, control, pos1, pos2, package, symbol);
}

static int restart_access_export_package_(addr package, addr symbol)
{
	addr pos1, pos2, control;
	Execute ptr;

	ptr = Execute_Thread;
	restart_import_export_package(&pos1);
	restart_ignore_export_package(&pos2);

	push_control(ptr, &control);
	pushrestart_control(ptr, pos2);
	pushrestart_control(ptr, pos1);
	(void)call_simple_package_error_va_(ptr,
			"The symbol ~S is not accessible in package ~S.",
			symbol, package, NULL);
	return restart_pop_export_package_(ptr, control, pos1, pos2, package, symbol);
}

static int test_conflict_export_package_(addr package, addr symbol,
		addr *ret, addr *rvalue)
{
	addr name, pos, bit, list, check;
	struct bittype_struct *str;

	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_USED, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(find_bitpackage_(pos, name, &bit));
		if (bit == Nil)
			continue;
		str = StructBitType(bit);
		if (str->shadow)
			continue;
		GetBitTypeSymbol(bit, &check);
		if (symbol == check)
			continue;

		if (rvalue)
			*rvalue = check;
		return Result(ret, pos);
	}

	return Result(ret, Nil);
}

static int check_export_package_(addr package, addr symbol)
{
	addr name, bit, pos, sym;

	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &bit);
	Return(findnil_hashtable_(bit, name, &bit));

	/* exist check */
	if (bit == Nil)
		return restart_exist_export_package_(package, symbol);

	/* accessible check */
	GetBitTypeSymbol(bit, &pos);
	if (pos != symbol)
		return restart_access_export_package_(package, symbol);

	/* already exported */
	if (StructBitType(bit)->expt)
		return 0;

	/* conflict check */
	Return(test_conflict_export_package_(package, symbol, &pos, &sym));
	if (pos != Nil)
		return restart_conflict_export_package_(package, symbol, pos, sym);

	return 0;
}


/*
 *  export
 */
static int intern_export_package_(addr package, addr symbol, addr name)
{
	addr left, right, bit;

	GetPackage(package, PACKAGE_INDEX_USED, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		GetPackage(left, PACKAGE_INDEX_TABLE, &left);
		Return(intern_hashheap_(left, name, &left));
		GetCdr(left, &bit);
		if (bit == Nil) {
			inheritedbitpackage(&bit, symbol);
			SetCdr(left, bit);
		}
		/* If left != Nil, the symbol may be a shadowing symbol. */
	}

	return 0;
}

static int execute_export_package_(addr package, addr symbol)
{
	addr table, bit, name, pos;
	struct bittype_struct *str;

	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	Return(findnil_hashtable_(table, name, &bit));

	/* exist check */
	if (bit == Nil)
		return 0; /* error, ignore */

	/* accessible check */
	GetBitTypeSymbol(bit, &pos);
	if (pos != symbol)
		return 0; /* error, ignore */

	/* already exported */
	str = StructBitType(bit);
	if (str->expt) {
		/* symbol is already exported. */
		return 0;
	}

	/* conflict check */
	Return(test_conflict_export_package_(package, symbol, &pos, NULL));
	if (pos != Nil)
		return 0; /* error, ignore */

	/* export */
	if (str->inherit) {
		/* If the symbol type is inherited, the type change to import.  */
		str->inherit = 0;
		str->import = 1;
		str->expt = 1;
		str->intern = PACKAGE_TYPE_EXTERNAL;
	}
	else {
		/* export symbol */
		str->expt = 1;
		str->intern = PACKAGE_TYPE_EXTERNAL;
	}
	Return(intern_export_package_(package, symbol, name));
	Return(push_list_export_package_(package, name));

	return 0;
}

int symbol_export_package_(addr package, addr symbol)
{
	Check(! symbolp(symbol), "type error");
	Return(check_export_package_(package, symbol));
	return execute_export_package_(package, symbol);
}

static int list_export_package_(addr package, addr args)
{
	addr list, pos, type;

	/* type check */
	Check(! listp(args), "type error");
	list = args;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! symbolp(pos)) {
			GetTypeTable(&type, Symbol);
			return call_type_error_va_(NULL, pos, type,
					"EXPORT ~S must be a symbol type.", pos, NULL);
		}
	}

	/* check */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(check_export_package_(package, pos));
	}

	/* export */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(execute_export_package_(package, pos));
	}

	return 0;
}

int export_package_(addr package, addr pos)
{
	addr type;

	Return(package_designator_update_p_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			return symbol_export_package_(package, pos);

		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return list_export_package_(package, pos);

		default:
			GetTypeTable(&type, SymbolList);
			return call_type_error_va_(NULL, pos, type,
					"EXPORT ~S must be a symbol or list.", pos, NULL);
	}
}


/****************************************************************************
 *  Function UNEXPORT
 ****************************************************************************/
static int check_unexport_package_(addr package, addr symbol)
{
	addr table, name;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetNameSymbol(symbol, &name);
	Return(findnil_hashtable_(table, name, &name));
	if (name == Nil)
		goto error;
	GetBitTypeSymbol(name, &name);
	if (symbol != name)
		goto error;

	return 0;

error:
	return call_simple_package_error_va_(NULL,
			"The symbol ~S is not accesible in the ~S package.",
			symbol, package, NULL);
}

static int execute_used_unexport_package_(addr package, addr symbol)
{
	int check;
	addr pos, list, table;

	GetNameSymbol(symbol, &symbol);
	GetPackage(package, PACKAGE_INDEX_USED, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetPackage(pos, PACKAGE_INDEX_TABLE, &table);
		Return(findnil_hashtable_(table, symbol, &pos));
		if (pos != Nil) {
			if (StructBitType(pos)->inherit) {
				Return(delete_hashtable_(table, symbol, &check));
			}
		}
	}

	return 0;
}

static int execute_type_unexport_package_(addr package, addr symbol)
{
	addr table, name, bit;
	struct bittype_struct *str;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetNameSymbol(symbol, &name);
	Return(findnil_hashtable_(table, name, &bit));
	str = StructBitType(bit);
	if (str->expt) {
		str->intern = PACKAGE_TYPE_INTERNAL;
		str->expt = 0;
		Return(delete_list_export_package_(package, name));
	}

	return 0;
}

static int symbol_unexport_package_(addr package, addr symbol)
{
	Return(check_unexport_package_(package, symbol));
	Return(execute_used_unexport_package_(package, symbol));
	Return(execute_type_unexport_package_(package, symbol));

	return 0;
}

static int list_unexport_package_(addr package, addr args)
{
	addr list, pos, type;

	/* type check */
	Check(! listp(args), "type error");
	list = args;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! symbolp(pos)) {
			GetTypeTable(&type, Symbol);
			return call_type_error_va_(NULL, pos, type,
					"UNEXPORT ~S must be a symbol type.", pos, NULL);
		}
	}

	/* conflict check */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(check_unexport_package_(package, pos));
	}

	/* unexport */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(symbol_unexport_package_(package, pos));
	}

	return 0;
}

static int package_designator_unexport_package_(addr pos, addr *ret)
{
	addr check;

	Return(package_designator_update_p_(pos, &pos));

	/* KEYWORD */
	GetConst(PACKAGE_KEYWORD, &check);
	if (pos == check)
		goto error;

	/* COMMON-LISP */
	GetConst(PACKAGE_COMMON_LISP, &check);
	if (pos == check)
		goto error;

	/* normal */
	return Result(ret, pos);

error:
	*ret = Nil;
	return fmte_("UNEXPORT can't unexport the ~S package.", pos, NULL);
}

int unexport_package_(addr package, addr pos)
{
	addr type;

	Return(package_designator_unexport_package_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			return symbol_unexport_package_(package, pos);

		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return list_unexport_package_(package, pos);

		default:
			GetTypeTable(&type, SymbolList);
			return call_type_error_va_(NULL, pos, type,
					"UNEXPORT ~S must be a symbol or list.", pos, NULL);
	}
}


/************************************************************
 *  package_import.c
 ************************************************************/

/*
 *  restart
 */
static int restart_force_import_package_(Execute ptr,
		addr *ret, addr symbol1, addr symbol2)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_IMPORT, &pos);
	restart_heap(&restart, pos);
	/* report */
	Return(format_string_(ptr, &pos,
				"Import ~S and unintern ~S.", symbol1, symbol2, NULL));
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	return Result(ret, restart);
}

static void restart_ignore_import_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_IGNORE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Ignore import.");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int restart_import_package_(Execute ptr,
		addr package, addr symbol1, addr symbol2)
{
	addr force, ignore, control;

	Return(restart_force_import_package_(ptr, &force, symbol1, symbol2));
	restart_ignore_import_package(&ignore);

	push_control(ptr, &control);
	pushrestart_control(ptr, ignore);
	pushrestart_control(ptr, force);
	(void)call_simple_package_error_va_(ptr,
			"IMPORT symbol ~S and ~S occer conflict.", symbol1, symbol2, NULL);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* force import */
	if (ptr->throw_handler == force) {
		normal_throw_control(ptr);
		Return(unintern_package_(package, symbol2, NULL));
		goto escape;
	}

	/* ignore */
	if (ptr->throw_handler == ignore) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}


/*
 *  import
 */
static int exist_import_package(addr symbol, addr bit, addr *ret)
{
	enum PACKAGE_TYPE type;
	struct bittype_struct *str;
	addr check;

	GetBitTypeIntern(bit, &type);
	if (type == PACKAGE_TYPE_INHERITED) {
		/* inherited -> import */
		str = StructBitType(bit);
		str->intern = PACKAGE_TYPE_INTERNAL;
		str->import = 1;
		str->inherit = 0;
		*ret = bit;
		return 0;
	}
	else {
		/* conflict or shadowing-symbol */
		*ret = bit;
		GetBitTypeSymbol(bit, &check);
		return check != symbol;
	}
}

int import_bitpackage_(addr package, addr symbol, addr *value, int *ret)
{
	addr table, name, bit, check;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetNameSymbol(symbol, &name);

	/* intern check */
	Return(findnil_hashtable_(table, name, &bit));
	if (bit != Nil) {
		*ret = exist_import_package(symbol, bit, value);
		return 0;
	}

	/* intern hashtable */
	GetPackageSymbol(symbol, &check);
	if (check == Nil) {
		/* intern */
		Return(intern_hashheap_(table, name, &check));
		internbitpackage(&bit, symbol);
		SetCdr(check, bit);
		/* set package */
		SetPackageSymbol(symbol, package);
	}
	else {
		/* import */
		Return(intern_hashheap_(table, name, &check));
		importbitpackage(&bit, symbol);
		SetCdr(check, bit);
	}

	*value = bit;
	return Result(ret, 0);
}

static int execute1_import_package_(Execute ptr, addr package, addr table, addr pos)
{
	addr type, bit;

	/* type check */
	if (! symbolp(pos)) {
		GetTypeTable(&type, Symbol);
		return call_type_error_va_(NULL, pos, type,
				"IMPORT ~S must be a symbol type.", pos, NULL);
	}

	/* intern check */
	GetNameSymbol(pos, &bit);
	Return(findnil_hashtable_(table, bit, &bit));
	if (bit != Nil) {
		GetBitTypeSymbol(bit, &bit);
		if (pos != bit) {
			Return(restart_import_package_(ptr, package, pos, bit));
		}
	}

	return 0;
}

static int execute_import_package_(Execute ptr, addr package, addr list)
{
	int check;
	addr table, pos;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(execute1_import_package_(ptr, package, table, pos));
		Return(import_bitpackage_(package, pos, &pos, &check));
	}

	return 0;
}

static int list_import_package_(addr package, addr args)
{
	addr control;
	Execute ptr;
	LocalHold hold;

	ptr = Execute_Thread;
	push_control(ptr, &control);
	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, package, args, NULL);
	(void)execute_import_package_(ptr, package, args);
	return pop_control_(ptr, control);
}

static int symbol_import_package_(addr package, addr pos)
{
	list_heap(&pos, pos, NULL);
	return list_import_package_(package, pos);
}

int import_package_(addr package, addr pos)
{
	addr type;

	Return(package_designator_update_p_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			return symbol_import_package_(package, pos);

		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return list_import_package_(package, pos);

		default:
			GetTypeTable(&type, SymbolList);
			return call_type_error_va_(NULL, pos, type,
					"IMPORT ~S must be a symbol or list.", pos, NULL);
	}
}


/************************************************************
 *  package_intern.c
 ************************************************************/

/****************************************************************************
 *  Function INTERN
 ****************************************************************************/
int intern_package_table_(addr package, addr name, addr *value, enum PACKAGE_TYPE *ret)
{
	int check;
	enum PACKAGE_TYPE result;
	addr keyword, bit;

	/* intern */
	CheckType(package, LISPTYPE_PACKAGE);
	Return(intern_bitpackage_(package, name, &bit, &check));
	result = check? PACKAGE_TYPE_NIL: StructBitType(bit)->intern;
	GetBitTypeSymbol(bit, value);

	/* keyword */
	GetConst(PACKAGE_KEYWORD, &keyword);
	if (keyword == package) {
		Return(setkeyword_package_(*value));
	}

	/* result */
	if (ret)
		return Result(ret, result);

	return 0;
}

int intern_package_(addr package, addr name, addr *value, enum PACKAGE_TYPE *ret)
{
	Check(package == NULL, "null error");
	Check(package == Nil, "nil error");
	Check(! stringp(name), "type error");

	Return(package_designator_(package, &package));
	return intern_package_table_(package, name, value, ret);
}

int intern_char_package_(addr package, const char *name,
		addr *value, enum PACKAGE_TYPE *ret)
{
	addr symbol;

	Check(package == NULL, "null error");
	Check(package == Nil, "nil error");
	CheckType(package, LISPTYPE_PACKAGE);

	/* find symbol */
	Return(find_char_bitpackage_(package, name, &symbol));
	if (symbol != Nil) {
		GetBitTypeSymbol(symbol, value);
		if (ret)
			return Result(ret, StructBitType(symbol)->intern);
		return 0;
	}

	/* intern */
	strvect_char_heap(&symbol, name);
	return intern_package_(package, symbol, value, ret);
}


/****************************************************************************
 *  Function UNINTERN
 ****************************************************************************/
/*
 *  Recovery conflict.
 */
static int recovery_update_unintern_package_(addr package, addr symbol, addr var)
{
	addr name, hash, bit, cons;

	Return(delete_list_shadow_package_(package, symbol));
	Return(push_list_shadow_package_(package, var));

	/* Update package */
	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &hash);
	Return(findcons_hashtable_(hash, name, &cons));
	Check(cons == Nil, "find error.");
	importbitpackage(&bit, var);
	SetBitTypeShadow(bit, 1);
	SetCdr(cons, bit);

	return 0;
}

static int recovery_check_name_unintern_package_(addr symbol, addr var)
{
	int check;
	addr x, y;

	GetNameSymbol(symbol, &x);
	GetNameSymbol(var, &y);
	Return(string_equal_(x, y, &check));
	if (! check)
		return fmte_("The argument ~S is not a conflict symbol.", var, NULL);

	return 0;
}

static int recovery_check_intern_unintern_package_(addr package, addr symbol)
{
	addr name, hash, bit;
	struct bittype_struct *str;

	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &hash);
	Return(findnil_hashtable_(hash, name, &bit));
	if (bit == Nil)
		goto error;
	str = StructBitType(bit);
	if (str->shadow == 0)
		goto error;
	GetBitTypeSymbol(bit, &bit);
	if (bit != symbol)
		goto error;
	return 0;

error:
	return fmte_("Invalid closure data.", NULL);
}

static int recovery_check_inherit_unintern_package_(addr package, addr var)
{
	addr name, list, pos;

	GetNameSymbol(var, &name);
	GetPackage(package, PACKAGE_INDEX_USE, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
		Return(findnil_hashtable_(pos, name, &pos));
		if (pos == Nil)
			continue;
		if (StructBitType(pos)->expt == 0)
			continue;

		return 0;
	}

	return fmte_("The package don't occure conflict.", NULL);
}

static int recovery_unintern_package_(addr package, addr symbol, addr var)
{
	CheckType(package, LISPTYPE_PACKAGE);
	Check(! symbolp(symbol), "type error");
	Check(! symbolp(var), "type error");

	/* check */
	Return(recovery_check_name_unintern_package_(symbol, var));
	Return(recovery_check_intern_unintern_package_(package, symbol));
	Return(recovery_check_inherit_unintern_package_(package, var));
	Return(unexport_package_(package, symbol));

	/* update */
	return recovery_update_unintern_package_(package, symbol, var);
}


/*
 *  restart
 */
static int function_unintern_call(Execute ptr, addr var, addr ignore)
{
	addr list, package, symbol;

	/* type check */
	if (! symbolp(var))
		return TypeError_(var, SYMBOL);

	/* closure */
	getdata_control(ptr, &list);
	List_bind(list, &package, &symbol, NULL);

	/* make shadow */
	return recovery_unintern_package_(package, symbol, var);
}

static int export_symbol_unintern_package_(addr pos, addr name, addr *ret)
{
	GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
	Return(findnil_hashtable_(pos, name, &pos));
	if (pos != Nil && StructBitType(pos)->expt) {
		GetBitTypeSymbol(pos, ret);
		return 0;
	}

	return Result(ret, Unbound);
}

static int list_export_unintern_package_(addr package, addr symbol, addr *ret)
{
	addr list, name, pos, root;

	GetPackage(package, PACKAGE_INDEX_USE, &list);
	GetNameSymbol(symbol, &name);
	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(export_symbol_unintern_package_(pos, name, &pos));
		if (pos != Unbound)
			cons_heap(&root, pos, root);
	}

	return Result(ret, root);
}

static int function_unintern_input(Execute ptr)
{
	addr list, package, symbol, stream, pos;
	LocalHold hold;

	/* closure */
	getdata_control(ptr, &list);
	List_bind(list, &package, &symbol, NULL);

	hold = LocalHold_array(ptr, 1);
loop:
	Return(list_export_unintern_package_(package, symbol, &list));
	localhold_set(hold, 0, list);
	Return(query_io_stream_(ptr, &stream));
	Return(format_stream_(ptr, stream,
				"~2&Select making shadowing-symbols.~2%~5T~S~2%",
				list, NULL));
	strvect_char_heap(&pos, "Input symbol: ");
	Return(prompt_for_stream_(ptr, T, pos, &pos));
	if (! find_list_eq_unsafe(pos, list)) {
		Return(format_stream_(ptr, stream,
					"~%ERROR: Invalid input ~S.~%Please input again.~2%",
					pos, NULL));
		goto loop;
	}
	Return(format_stream_(ptr, stream, "~%Select ~S symbol.~2%", pos, NULL));

	/* result */
	list_heap(&list, pos, NULL);
	setresult_control(ptr, list);

	return 0;
}

static void compiled_call_unintern_package_(addr *ret)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_var1opt1(pos, p_defun_unintern_call);
	*ret = pos;
}

static void compiled_input_unintern_package_(addr *ret)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, p_defun_unintern_input);
	*ret = pos;
}

static void restart_shadow_unintern_package_(addr *ret, addr package, addr symbol)
{
	addr restart, pos, list;

	/* closure */
	list_heap(&list, package, symbol, NULL);
	/* name */
	GetConst(COMMON_SHADOW, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Make an another symbol shadowing.");
	setreport_restart(restart, pos);
	/* function */
	compiled_call_unintern_package_(&pos);
	SetDataFunction(pos, list);
	setfunction_restart(restart, pos);
	/* interactive */
	compiled_input_unintern_package_(&pos);
	SetDataFunction(pos, list);
	setinteractive_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static void restart_continue_unintern_package_(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_IGNORE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Ignore");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PROGRAM_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int restart_unintern_package_(addr package, addr symbol, int *ret)
{
	addr pos1, pos2, control;
	Execute ptr;

	ptr = Execute_Thread;
	restart_shadow_unintern_package_(&pos1, package, symbol);
	restart_continue_unintern_package_(&pos2);

	push_control(ptr, &control);
	pushrestart_control(ptr, pos2);
	pushrestart_control(ptr, pos1);
	*ret = 0;
	(void)call_simple_package_error_va_(ptr,
			"Name conflict occured ~S in the ~S package.",
			symbol, package, NULL);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* shadow */
	if (ptr->throw_handler == pos1) {
		normal_throw_control(ptr);
		*ret = 1;
		goto escape;
	}

	/* continue */
	if (ptr->throw_handler == pos2) {
		normal_throw_control(ptr);
		*ret = 0;
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}


/*
 *  unintern
 */
static int check_export_unintern_package_(addr pos, addr name, int *ret)
{
	int check;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
	Return(findnil_hashtable_(pos, name, &pos));
	check = (pos != Nil && StructBitType(pos)->expt);

	return Result(ret, check);
}

static int check_shadowing_unintern_package_(addr package, addr name, int *ret)
{
	int loop, check;
	addr pos, list;

	GetPackage(package, PACKAGE_INDEX_USE, &list);
	loop = 0;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(check_export_unintern_package_(pos, name, &check));
		if (check) {
			if (loop)
				return Result(ret, 1);
			loop = 1;
		}
	}

	return Result(ret, 0);
}

static int check_unintern_package_(addr package, addr symbol, int *ret, int *value)
{
	int check;
	addr bit, name, pos;

	/* exist check */
	GetPackage(package, PACKAGE_INDEX_TABLE, &pos);
	GetNameSymbol(symbol, &name);
	Return(findnil_hashtable_(pos, name, &bit));
	*value = 0;
	/* no symbol */
	if (bit == Nil)
		return Result(ret, 0);
	/* other symbol */
	GetBitTypeSymbol(bit, &pos);
	if (pos != symbol)
		return Result(ret, 0);
	/* inherit symbol */
	if (StructBitType(bit)->inherit)
		return Result(ret, 0);

	/* conflict check */
	if (StructBitType(bit)->shadow) {
		Return(check_shadowing_unintern_package_(package, name, &check));
		if (check) {
			Return(restart_unintern_package_(package, symbol, &check));
			*value = 1;
			return Result(ret, check);
		}
	}

	return Result(ret, 1);
}

static int remove_unintern_package_(addr package, addr symbol, int *ret)
{
	int check;
	addr name, table, bit, pos;
	struct bittype_struct *str;

	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	Return(findnil_hashtable_(table, name, &bit));
	Check(bit == Nil, "hashtable error");
	str = StructBitType(bit);
	/* base */
	if (str->base) {
		GetBitTypeSymbol(bit, &pos);
		SetPackageSymbol(pos, Nil); /* gensym */
	}
	Return(delete_hashtable_(table, name, &check));

	return Result(ret, StructBitType(bit)->shadow);
}

static int intern_inherited_unintern_package_(addr package, addr name)
{
	addr pos, table, list, bit, cons;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetPackage(package, PACKAGE_INDEX_USE, &list);

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
		Return(findnil_hashtable_(pos, name, &bit));
		if (bit == Nil)
			continue;
		if (StructBitType(bit)->expt == 0)
			continue;

		/* intern inherited */
		GetBitTypeSymbol(bit, &pos);
		inheritedbitpackage(&bit, pos);
		Return(intern_hashheap_(table, name, &cons));
		SetCdr(cons, bit);
		return 0;
	}

	return 0;
}

static int symbol_unintern_package_(addr package, addr symbol)
{
	int check;
	addr name;

	GetNameSymbol(symbol, &name);
	Return(remove_unintern_package_(package, symbol, &check));
	if (check) {
		/* shadowing-symbols */
		Return(intern_inherited_unintern_package_(package, name));
		Return(delete_list_shadow_package_(package, symbol));
	}

	return 0;
}

int unintern_package_(addr package, addr symbol, int *ret)
{
	int check, value;

	Check(! symbolp(symbol), "type error");
	Return(package_designator_update_p_(package, &package));

	/* check */
	Return(check_unintern_package_(package, symbol, &check, &value));
	if (ret)
		*ret = check;
	if (value)
		return 0;
	if (! check)
		return 0;

	/* unintern */
	return symbol_unintern_package_(package, symbol);
}


/****************************************************************************
 *  Interface
 ****************************************************************************/
int setkeyword_package_(addr pos)
{
	addr check, package;

	GetConst(PACKAGE_KEYWORD, &package);
	Return(export_package_(package, pos));
	GetValueSymbol(pos, &check);
	if (check == Unbound) {
		SetValueSymbol(pos, pos);
		SetStatusReadOnly(pos);
	}

	return 0;
}

int intern_default_package_(Execute ptr, addr name,
		addr *value, enum PACKAGE_TYPE *ret)
{
	addr package;
	Return(getpackage_(ptr, &package));
	return intern_package_(package, name, value, ret);
}

int internchar_(const char *pname, const char *sname,
		addr *value, enum PACKAGE_TYPE *ret)
{
	addr package, name;

	Check(pname == NULL, "argument package error");
	Check(sname == NULL, "argument name error");

	/* find package */
	Return(find_char_package_(pname, &package));
	if (package == Nil) {
		strvect_char_heap(&name, pname);
		return fmte_("No such a package ~S.", name, NULL);
	}

	return intern_char_package_(package, sname, value, ret);
}

int internchar_default_(Execute ptr, const char *name,
		addr *value, enum PACKAGE_TYPE *ret)
{
	addr package;
	Return(getpackage_(ptr, &package));
	return intern_char_package_(package, name, value, ret);
}

int internchar_null_(Execute ptr, const char *pname, const char *sname,
		addr *value, enum PACKAGE_TYPE *ret)
{
	if (pname)
		return internchar_(pname, sname, value, ret);
	else
		return internchar_default_(ptr, sname, value, ret);
}

int internchar_keyword_(const char *name, addr *value, enum PACKAGE_TYPE *ret)
{
	enum PACKAGE_TYPE type;
	addr pos;

	GetConst(PACKAGE_KEYWORD, &pos);
	Return(intern_char_package_(pos, name, &pos, &type));
	Return(setkeyword_package_(pos));
	*value = pos;

	if (ret)
		return Result(ret, type);
	else
		return 0;
}

int interncommon_(const char *name, addr *value, enum PACKAGE_TYPE *ret)
{
	enum PACKAGE_TYPE type;
	addr package, pos;

	GetConst(PACKAGE_COMMON_LISP, &package);
	Return(intern_char_package_(package, name, &pos, &type));
	Return(export_package_(package, pos));
	*value = pos;

	if (ret)
		return Result(ret, type);
	else
		return 0;
}


/****************************************************************************
 *  Debug
 ****************************************************************************/
void internchar_debug(const char *pname, const char *sname, addr *value)
{
	Error(internchar_(pname, sname, value, NULL));
}

void internchar_keyword_debug(const char *name, addr *value)
{
	Error(internchar_keyword_(name, value, NULL));
}

void interncommon_debug(const char *name, addr *value)
{
	Error(interncommon_(name, value, NULL));
}

addr interncharr_debug(const char *pname, const char *sname)
{
	addr pos;
	pos = NULL;
	Error(internchar_(pname, sname, &pos, NULL));
	return pos;
}

addr interncharr_null_debug(Execute ptr, const char *pname, const char *sname)
{
	addr pos;
	pos = NULL;
	Error(internchar_null_(ptr, pname, sname, &pos, NULL));
	return pos;
}

addr interncommonr_debug(const char *name)
{
	addr pos;
	pos = NULL;
	Error(interncommon_(name, &pos, NULL));
	return pos;
}


/*
 *  initialize
 */
void init_package_intern(void)
{
	SetPointerCall(defun, var1opt1, unintern_call);
	SetPointerCall(defun, empty, unintern_input);
}


/************************************************************
 *  package_iterator.c
 ************************************************************/

int package_iterator_alloc_(LocalRoot local, addr *ret,
		addr list, int internal, int external, int inherited)
{
	addr pos, package, table;
	struct StructPackageIterator *str;

	/* object */
	alloc_smallsize(local, &pos,
			LISPSYSTEM_PACKAGEITERATOR,
			PackageIterator_Size,
			sizeoft(struct StructPackageIterator));
	str = PtrStructPackageIterator(pos);
	clearpoint(str);
	str->internal = (internal != 0);
	str->external = (external != 0);
	str->inherited = (inherited != 0);
	str->finish = 0;

	/* no-package */
	if (list == Nil) {
		str->finish = 1;
		return Result(ret, pos);
	}

	/* package or list */
	if (listp(list)) {
		Return_getcons(list, &package, &list);
	}
	else {
		package = list;
		list = Nil;
	}

	/* package -> hash-iterator */
	Return(package_designator_(package, &package));
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	hash_iterator_alloc(local, &table, table);

	/* result */
	SetPackageIterator(pos, PackageIterator_List, list);
	SetPackageIterator(pos, PackageIterator_Table, table);
	SetPackageIterator(pos, PackageIterator_Package, package);

	return Result(ret, pos);
}

int package_iterator_local_(LocalRoot local, addr *ret,
		addr list, int internal, int external, int inherited)
{
	Check(local == NULL, "local error");
	return package_iterator_alloc_(local, ret, list, internal, external, inherited);
}

int package_iterator_heap_(addr *ret,
		addr list, int internal, int external, int inherited)
{
	return package_iterator_alloc_(NULL, ret, list, internal, external, inherited);
}

static enum PACKAGE_TYPE hash_package_iterator(addr pos, addr *rets, addr *retp)
{
	enum PACKAGE_TYPE type;
	addr table, key, value;
	struct StructPackageIterator *str;
	struct bittype_struct *bit;

	str = PtrStructPackageIterator(pos);
	GetPackageIterator(pos, PackageIterator_Table, &table);
	while (next_hash_iterator(table, &key, &value)) {
		bit = StructBitType(value);
		type = bit->intern;
		if ((str->internal && type == PACKAGE_TYPE_INTERNAL) ||
				(str->external && type == PACKAGE_TYPE_EXTERNAL) ||
				(str->inherited && type == PACKAGE_TYPE_INHERITED)) {
			GetBitTypeSymbol(value, rets);
			GetPackageIterator(pos, PackageIterator_Package, retp);
			return type;
		}
	}

	return PACKAGE_TYPE_NIL;
}

static int forward_package_iterator_(addr pos)
{
	addr list, raw, table, package;

	GetPackageIterator(pos, PackageIterator_List, &list);
	if (list == Nil) {
		PtrStructPackageIterator(pos)->finish = 1;
		return 0;
	}
	Return_getcons(list, &package, &list);
	Return(package_designator_(package, &package));
	GetPackage(package, PACKAGE_INDEX_TABLE, &raw);
	GetPackageIterator(pos, PackageIterator_Table, &table);
	set_hash_iterator(table, raw);
	SetPackageIterator(pos, PackageIterator_List, list);
	SetPackageIterator(pos, PackageIterator_Package, package);

	return 0;
}

int next_package_iterator_(addr pos, addr *rets, addr *retp, enum PACKAGE_TYPE *ret)
{
	enum PACKAGE_TYPE type;
	struct StructPackageIterator *str;

	CheckType(pos, LISPSYSTEM_PACKAGEITERATOR);
	str = PtrStructPackageIterator(pos);
	while (! str->finish) {
		type = hash_package_iterator(pos, rets, retp);
		if (type != PACKAGE_TYPE_NIL)
			return Result(ret, type);
		Return(forward_package_iterator_(pos));
	}

	return Result(ret, PACKAGE_TYPE_NIL);
}


/************************************************************
 *  package_make.c
 ************************************************************/

/*
 *  restart name
 *    continue  Use (find-package name).
 *    input     Use input name.
 */
static void restart_continue_name_make_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Return the existing package.");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static void compiled_input_name_make_package(addr *ret)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, p_defun_make_package_input);
	*ret = pos;
}

static void restart_input_name_make_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(SYSTEM_INPUT, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Input another package name.");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_VALUES, &pos);
	setfunction_restart(restart, pos);
	/* interactive */
	compiled_input_name_make_package(&pos);
	setinteractive_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int function_make_package_input(Execute ptr)
{
	addr type, prompt, pos;

	GetTypeTable(&type, PackageDesignator);
	strvect_char_heap(&prompt, "Input another package name: ");
	Return(prompt_for_stream_(ptr, type, prompt, &pos));
	list_heap(&pos, pos, NULL);
	setresult_control(ptr, pos);

	return 0;
}

static int error_make_package_(addr name, addr *ret)
{
	addr table, pos;

	PackageTable(&table);
	Return(findcons_hashtable_(table, name, &pos));
	if (pos != Nil) {
		return call_simple_package_error_va_(NULL,
				"Package ~S already exists.", name, NULL);
	}

	/* name ok */
	*ret = name;
	return 0;
}

enum MakePackageType {
	MakePackageName,
	MakePackageObject,
	MakePackageLoop
};

static int throw_name_make_package_(Execute ptr, addr name,
		addr pos1, addr pos2, addr *value, enum MakePackageType *ret)
{
	if (ptr->throw_handler == pos1) {
		normal_throw_control(ptr);
		*ret = MakePackageObject;
		return find_package_(name, value);
	}
	if (ptr->throw_handler == pos2) {
		normal_throw_control(ptr);
		*ret = MakePackageLoop;
		getresult_control(ptr, value);
		return 0;
	}

	/* invalid operator */
	*ret = MakePackageName;
	return 1;
}

static int loop_name_make_package_(Execute ptr,
		addr name, addr *value, enum MakePackageType *ret)
{
	addr pos1, pos2, control;

	restart_continue_name_make_package(&pos1);
	restart_input_name_make_package(&pos2);
	push_control(ptr, &control);
	pushrestart_control(ptr, pos2);
	pushrestart_control(ptr, pos1);
	*value = Nil;
	*ret = MakePackageName;
	if (error_make_package_(name, value)) {
		if (ptr->throw_control == control)
			(void)throw_name_make_package_(ptr, name, pos1, pos2, value, ret);
	}

	return pop_control_(ptr, control);
}

static int name_make_package_(Execute ptr, LocalHold hold,
		addr name, addr *value, int *ret)
{
	enum MakePackageType type;

	do {
		Return(string_designator_heap_(&name, name, NULL));
		localhold_set(hold, 0, name);
		Return(loop_name_make_package_(ptr, name, &name, &type));
		localhold_set(hold, 0, name);
	}
	while (type == MakePackageLoop);

	*ret = (type == MakePackageObject);
	return Result(value, name);
}


/*
 *  restart nicknames
 *    continue  Remove list.
 */
static void restart_continue_nicknames_make_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Remove names of conflict in :nicknames list.");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int error_nicknames_make_package_(Execute ptr, addr remove)
{
	addr restart, control;

	restart_continue_nicknames_make_package(&restart);
	push_control(ptr, &control);
	pushrestart_control(ptr, restart);
	(void)call_simple_package_error_va_(NULL,
			"Nicknames ~S already exist.", remove, NULL);
	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	normal_throw_control(ptr);
escape:
	return pop_control_(ptr, control);
}

static int remove_nicknames_make_package_(Execute ptr, addr list, addr *ret)
{
	addr pos, cons, table, root;

	PackageTable(&table);
	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(string_designator_heap_(&pos, pos, NULL));
		Return(findcons_hashtable_(table, pos, &cons));
		if (cons == Nil)
			cons_heap(&root, pos, root);
	}

	return Result(ret, root);
}

static int conflict_nicknames_make_package_(addr list, addr *ret)
{
	addr pos, cons, table, root;

	PackageTable(&table);
	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(string_designator_heap_(&pos, pos, NULL));
		Return(findcons_hashtable_(table, pos, &cons));
		if (cons != Nil) {
			GetCdr(cons, &cons);
			cons_heap(&root, cons, root);
		}
	}

	return Result(ret, root);
}

static int nicknames_make_package_(Execute ptr, addr list, addr *ret)
{
	addr remove;

	Return(conflict_nicknames_make_package_(list, &remove));
	if (remove != Nil) {
		Return(error_nicknames_make_package_(ptr, remove));
		Return(remove_nicknames_make_package_(ptr, list, &list));
	}

	return Result(ret, list);
}


/*
 *  restart use
 *    continue    Ignore :use list.
 *    shadow      Make shadowing symbols.
 */
static void restart_continue_use_make_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Ignore :use list.");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static void restart_shadow_use_make_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_SHADOW, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Make newly symbols shadowing.");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int error_use_make_package_(Execute ptr,
		addr list, addr shadow,
		addr *rlist, addr *rshadow)
{
	addr pos1, pos2, control;

	restart_continue_use_make_package(&pos1);
	restart_shadow_use_make_package(&pos2);

	push_control(ptr, &control);
	pushrestart_control(ptr, pos2);
	pushrestart_control(ptr, pos1);

	(void)call_simple_package_error_va_(NULL,
			"Symbols ~S conflict occured.", shadow, NULL);
	*rlist = list;
	*rshadow = shadow;

	/* others */
	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* continue */
	if (ptr->throw_handler == pos1) {
		normal_throw_control(ptr);
		*rlist = *rshadow = Nil;
		goto escape;
	}

	/* shadowing */
	if (ptr->throw_handler == pos2) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}

static int pushnew_use_make_package_(addr pos, addr name, addr list, addr *ret)
{
	enum PACKAGE_TYPE type;

	Return(find_symbol_package_(pos, name, &pos, &type));
	Check(pos == Nil, "find_symbol error");
	pushnew_heap(list, pos, &list);

	return Result(ret, list);
}

static int equal_use_make_package_(addr pos1, addr pos2, addr x, addr y, int *ret)
{
	int check;
	enum PACKAGE_TYPE ignore;

	Return(string_equal_(x, y, &check));
	if (! check)
		return Result(ret, 0);

	Return(find_symbol_package_(pos1, x, &x, &ignore));
	Return(find_symbol_package_(pos2, y, &y, &ignore));
	return Result(ret, x != y);
}

static int conflict_use_make_package_(addr pos1, addr pos2, addr shadow, addr *ret)
{
	int check;
	addr exp1, exp2, x, y, loop;

	GetPackage(pos1, PACKAGE_INDEX_EXPORT, &exp1);
	GetPackage(pos2, PACKAGE_INDEX_EXPORT, &exp2);
	while (exp1 != Nil) {
		GetCons(exp1, &x, &exp1);
		for (loop = exp2; loop != Nil; ) {
			GetCons(loop, &y, &loop);
			Return(equal_use_make_package_(pos1, pos2, x, y, &check));
			if (check) {
				Return(pushnew_use_make_package_(pos1, x, shadow, &shadow));
				Return(pushnew_use_make_package_(pos2, y, shadow, &shadow));
			}
		}
	}

	return Result(ret, shadow);
}

static int check_use_make_package_(addr list, addr *ret)
{
	addr x, y, loop, shadow;

	shadow = Nil;
	while (list != Nil) {
		GetCons(list, &x, &list);
		Return(package_designator_(x, &x));
		for (loop = list; loop != Nil; ) {
			GetCons(loop, &y, &loop);
			Return(package_designator_(y, &y));
			Return(conflict_use_make_package_(x, y, shadow, &shadow));
		}
	}

	return Result(ret, shadow);
}

static int use_make_package_(Execute ptr, LocalHold hold,
		addr list, addr *ret, addr *rshadow)
{
	addr shadow;

	Return(check_use_make_package_(list, &shadow));
	if (shadow == Nil) {
		*rshadow = Nil;
		return Result(ret, list);
	}

	/* error */
	localhold_set(hold, 3, shadow);
	Return(error_use_make_package_(ptr, list, shadow, &list, &shadow));
	localhold_set(hold, 2, list);
	localhold_set(hold, 3, shadow);

	*rshadow = shadow;
	return Result(ret, list);
}


/*
 *  make-package
 */
static int append_exportname_package_(addr pos, addr left, addr name)
{
	addr bit, cons, one;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &one);
	Return(intern_hashheap_(one, name, &cons));

	/* duplicate check (if same package in use list.) */
	GetCdr(cons, &one);
	if (one == Nil) {
		/* make bitpackage */
		Return(findnil_hashtable_(left, name, &one));
		Check(one == Nil, "export nil error");
		Check(StructBitType(one)->expt == 0, "export error");
		GetBitTypeSymbol(one, &one);
		inheritedbitpackage(&bit, one);
		/* push package */
		SetCdr(cons, bit);
	}

	return 0;
}

static void pushnew_list_package(addr package, enum PACKAGE_INDEX index, addr pos)
{
	addr list;

	GetPackage(package, index, &list);
	if (find_list_eq_unsafe(pos, list) == 0) {
		cons_heap(&list, pos, list);
		SetPackage(package, index, list);
	}
}

static int append_usepackage_package_(addr pos, addr list)
{
	addr pg, hash, expt, name;

	while (list != Nil) {
		/* intern export */
		GetCons(list, &pg, &list);
		Return(package_designator_(pg, &pg));
		GetPackage(pg, PACKAGE_INDEX_EXPORT, &expt);
		GetPackage(pg, PACKAGE_INDEX_TABLE, &hash);
		while (expt != Nil) {
			GetCons(expt, &name, &expt);
			Return(append_exportname_package_(pos, hash, name));
		}

		/* push use-list, used-by-list */
		pushnew_list_package(pos, PACKAGE_INDEX_USE, pg);
		pushnew_list_package(pg, PACKAGE_INDEX_USED, pos);
	}

	return 0;
}

int make_package_(Execute ptr, addr name, addr names, addr use, addr *ret)
{
	int check;
	addr pos, shadow;
	LocalHold hold;

	hold = LocalHold_array(ptr, 4);
	localhold_set(hold, 0, name);
	localhold_set(hold, 1, names);
	localhold_set(hold, 2, use);

	/* name */
	Return(name_make_package_(ptr, hold, name, &name, &check));
	if (check)
		return Result(ret, name);

	/* nicknames */
	Return(nicknames_make_package_(ptr, names, &names));
	localhold_set(hold, 1, names);

	/* use-package */
	Return(use_make_package_(ptr, hold, use, &use, &shadow));

	/* make package */
	Return(package_heap_(&pos, name));
	if (shadow != Nil) {
		Return(shadow_package_(pos, shadow));
	}
	Return(append_nicknames_package_(pos, names));
	Return(append_usepackage_package_(pos, use));

	localhold_end(hold);

	return Result(ret, pos);
}


/*
 *  initialize
 */
void init_package_make(void)
{
	SetPointerCall(defun, empty, make_package_input);
}


/************************************************************
 *  package_object.c
 ************************************************************/

int packagep(addr pos)
{
	return GetType(pos) == LISPTYPE_PACKAGE;
}


/*
 *  package function
 */
int getname_package_(addr pos, addr *ret)
{
	Return(package_designator_(pos, &pos));
	GetPackage(pos, PACKAGE_INDEX_NAME, ret);
	return 0;
}

int getnickname_package_(addr pos, addr *ret)
{
	Return(package_designator_(pos, &pos));
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, ret);
	return 0;
}

int getuselist_package_(addr pos, addr *ret)
{
	Return(package_designator_(pos, &pos));
	GetPackage(pos, PACKAGE_INDEX_USE, ret);
	return 0;
}

int getusedbylist_package_(addr pos, addr *ret)
{
	Return(package_designator_(pos, &pos));
	GetPackage(pos, PACKAGE_INDEX_USED, ret);
	return 0;
}

int getshadow_package_(addr pos, addr *ret)
{
	Return(package_designator_(pos, &pos));
	GetPackage(pos, PACKAGE_INDEX_SHADOW, ret);
	return 0;
}

void getdocument_package(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_DOCUMENT, ret);
}

void setdocument_package(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	SetPackage(pos, PACKAGE_INDEX_DOCUMENT, value);
}


void getname_package_unsafe(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_NAME, ret);
}

void getnickname_package_unsafe(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, ret);
}

void getuselist_package_unsafe(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_USE, ret);
}

void getusedbylist_package_unsafe(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_USED, ret);
}

void getshadow_package_unsafe(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_SHADOW, ret);
}

void getexport_package_unsafe(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, ret);
}

int get_readonly_package(addr pos)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	return GetUser(pos);
}

void set_readonly_package(addr pos, int value)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	SetUser(pos, value != 0);
}


/*
 *  push package list
 */
static void push_list_eq_package(addr package, addr pos, enum PACKAGE_INDEX index)
{
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	GetPackage(package, index, &list);
#ifdef LISP_DEBUG
	if (find_list_eq_unsafe(pos, list)) {
		Abort("push_list_eq_package error.");
	}
#endif
	cons_heap(&list, pos, list);
	SetPackage(package, index, list);
}

void push_list_nicknames_package(addr package, addr pos)
{
	push_list_eq_package(package, pos, PACKAGE_INDEX_NICKNAME);
}

void push_list_use_package(addr package, addr pos)
{
	push_list_eq_package(package, pos, PACKAGE_INDEX_USE);
}

void push_list_used_package(addr package, addr pos)
{
	push_list_eq_package(package, pos, PACKAGE_INDEX_USED);
}

#ifdef LISP_DEBUG
static int find_list_string_package_(addr name, addr list, int *ret)
{
	int check;
	addr pos;

	Check(! stringp(name), "type error");
	Check(! listp(list), "type error");
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(string_equal_(pos, name, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
#endif

int push_list_export_package_(addr package, addr name)
{
#ifdef LISP_DEBUG
	int check;
#endif
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! stringp(name), "type error");
	GetPackage(package, PACKAGE_INDEX_EXPORT, &list);
#ifdef LISP_DEBUG
	Return(find_list_string_package_(name, list, &check));
	if (check) {
		Abort("push_list_string_package_ error.");
	}
#endif
	cons_heap(&list, name, list);
	SetPackage(package, PACKAGE_INDEX_EXPORT, list);

	return 0;
}

#ifdef LISP_DEBUG
static int find_list_shadow_package_(addr symbol, addr list, int *ret)
{
	int check;
	addr pos;

	Check(! symbolp(symbol), "type error");
	Check(! listp(list), "type error");
	GetNameSymbol(symbol, &symbol);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetNameSymbol(pos, &pos);
		Return(string_equal_(pos, symbol, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
#endif

int push_list_shadow_package_(addr package, addr symbol)
{
#ifdef LISP_DEBUG
	int check;
#endif
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! symbolp(symbol), "type error");
	GetPackage(package, PACKAGE_INDEX_SHADOW, &list);
#ifdef LISP_DEBUG
	Return(find_list_shadow_package_(symbol, list, &check));
	if (check) {
		Abort("push_list_shadow_package_ error.");
	}
#endif
	cons_heap(&list, symbol, list);
	SetPackage(package, PACKAGE_INDEX_SHADOW, list);

	return 0;
}


/*
 *  delete package list
 */
#ifdef LISP_DEBUG
static void delete_list_eq_package(addr package, enum PACKAGE_INDEX index, addr pos)
{
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	GetPackage(package, index, &list);

	/* delete */
	if (delete1_list_eq_unsafe(pos, list, &list) == 0) {
		/* Abnormal error */
		Abort("delete error.");
	}

	/* check */
	if (find_list_eq_unsafe(pos, list)) {
		/* Abnormal error */
		Abort("check error.");
	}

	SetPackage(package, index, list);
}
#else
static void delete_list_eq_package(addr package, enum PACKAGE_INDEX index, addr pos)
{
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	GetPackage(package, index, &list);
	(void)delete1_list_eq_unsafe(pos, list, &list);
	SetPackage(package, index, list);
}
#endif

void delete_list_use_package(addr package, addr pos)
{
	delete_list_eq_package(package, PACKAGE_INDEX_USE, pos);
}

void delete_list_used_package(addr package, addr pos)
{
	delete_list_eq_package(package, PACKAGE_INDEX_USED, pos);
}


/*
 *  delete export list
 */
static int delete1_list_string_package_(addr name, addr list, addr *value, int *ret)
{
	int check;
	addr pos, list1, list2;

	Check(! listp(list), "type error");
	*value = list;
	list2 = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list1);
		Return(string_equal_(pos, name, &check));
		if (check) {
			if (list2 == Nil)
				*value = list1;
			else
				SetCdr(list2, list1);
			return Result(ret, 1);
		}
		else {
			list2 = list;
		}
		list = list1;
	}

	return Result(ret, 0);
}

#ifdef LISP_DEBUG
int delete_list_export_package_(addr package, addr name)
{
	int check;
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! stringp(name), "type error");
	GetPackage(package, PACKAGE_INDEX_EXPORT, &list);

	/* delete */
	Return(delete1_list_string_package_(name, list, &list, &check));
	if (! check) {
		/* Abnormal error */
		return fmte_("There is no ~S in export list.", name, NULL);
	}

	/* check */
	Return(find_list_string_package_(name, list, &check));
	if (check) {
		/* Abnormal error */
		return fmte_("Invalid export list.", NULL);
	}

	SetPackage(package, PACKAGE_INDEX_EXPORT, list);

	return 0;
}
#else
int delete_list_export_package_(addr package, addr name)
{
	int ignore;
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! stringp(name), "type error");
	GetPackage(package, PACKAGE_INDEX_EXPORT, &list);
	Return(delete1_list_string_package_(name, list, &list, &ignore));
	SetPackage(package, PACKAGE_INDEX_EXPORT, list);

	return 0;
}
#endif


/*
 *  delete shadowing-symbols
 */
static int delete1_list_shadow_package_(addr symbol, addr list, addr *value, int *ret)
{
	int check;
	addr pos, list1, list2;

	Check(! symbolp(symbol), "type error");
	Check(! listp(list), "type error");
	GetNameSymbol(symbol, &symbol);
	*value = list;
	list2 = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list1);
		GetNameSymbol(pos, &pos);
		Return(string_equal_(pos, symbol, &check));
		if (check) {
			if (list2 == Nil)
				*value = list1;
			else
				SetCdr(list2, list1);
			return Result(ret, 1);
		}
		else {
			list2 = list;
		}
		list = list1;
	}

	return Result(ret, 0);
}

#ifdef LISP_DEBUG
int delete_list_shadow_package_(addr package, addr symbol)
{
	int check;
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! symbolp(symbol), "type error");
	GetPackage(package, PACKAGE_INDEX_SHADOW, &list);

	/* delete */
	Return(delete1_list_shadow_package_(symbol, list, &list, &check));
	if (! check) {
		/* Abnormal error */
		return fmte_("There is no ~S in shadow list.", symbol, NULL);
	}

	/* check */
	Return(find_list_shadow_package_(symbol, list, &check));
	if (check) {
		/* Abnormal error */
		return fmte_("Invalid shadow list.", NULL);
	}

	SetPackage(package, PACKAGE_INDEX_SHADOW, list);

	return 0;
}
#else
int delete_list_shadow_package_(addr package, addr symbol)
{
	int ignore;
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! symbolp(symbol), "type error");
	GetPackage(package, PACKAGE_INDEX_SHADOW, &list);
	Return(delete1_list_shadow_package_(symbol, list, &list, &ignore));
	SetPackage(package, PACKAGE_INDEX_SHADOW, list);

	return 0;
}
#endif


/************************************************************
 *  package_shadow.c
 ************************************************************/

/****************************************************************************
 *  Function SHADOW
 ****************************************************************************/
static int symbol_shadow_package_(addr package, addr pos)
{
	int check;
	addr bit;
	struct bittype_struct *str;

	Return(string_designator_heap_(&pos, pos, NULL));
	Return(intern_bitpackage_(package, pos, &bit, &check));
	str = StructBitType(bit);
	if (str->inherit) {
		/* change type to intern from inherit. */
		shadowintern_bitpackage(bit, pos, package);
	}
	if (! str->shadow) {
		GetBitTypeSymbol(bit, &pos);
		Return(push_list_shadow_package_(package, pos));
		SetBitTypeShadow(bit, 1);
	}

	return 0;
}

static int list_shadow_package_(addr package, addr args)
{
	addr list, pos, type;

	/* type check */
	Check(! listp(args), "type error");
	list = args;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! string_designator_p(pos)) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_va_(NULL, pos, type,
					"SHADOW ~S must be a string-designator.", pos, NULL);
		}
	}

	/* shadow */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(symbol_shadow_package_(package, pos));
	}

	return 0;
}

int shadow_package_(addr package, addr pos)
{
	addr type;

	Return(package_designator_update_p_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_T:
		case LISPTYPE_CHARACTER:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
			return symbol_shadow_package_(package, pos);

		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return list_shadow_package_(package, pos);

		default:
			GetTypeTable(&type, StringDesignatorList);
			return call_type_error_va_(NULL, pos, type,
					"SHADOW ~S must be a string-designator or list.", pos, NULL);
	}
}


/****************************************************************************
 *  Function SHADOWING-IMPORT
 ****************************************************************************/
static int export_shadowing_import_package_(addr pos, addr sym, addr sym0)
{
	addr bit, check;

	CheckType(pos, LISPTYPE_PACKAGE);
	Check(! symbolp(sym), "type error");
	Check(! symbolp(sym0), "type error");

	GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
	GetNameSymbol(sym, &bit);
	Return(findnil_hashtable_(pos, bit, &bit));
	if (bit == Nil)
		return 0;
	if (StructBitType(bit)->expt == 0)
		return 0;
	GetBitTypeSymbol(bit, &check);
	if (check != sym0)
		return 0;
	SetBitTypeSymbol(bit, sym);

	return 0;
}

static int inherited_shadowing_import_package_(addr package, addr sym, addr sym0)
{
	addr list, pos;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! symbolp(sym), "type error");
	Check(! symbolp(sym0), "type error");

	GetPackage(package, PACKAGE_INDEX_USED, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(export_shadowing_import_package_(pos, sym, sym0));
	}

	return 0;
}

static int symbol_shadowing_import_package_(addr package, addr symbol)
{
	int check;
	addr hash, name, bit, pos;
	struct bittype_struct *str;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! symbolp(symbol), "type error");

	GetPackage(package, PACKAGE_INDEX_TABLE, &hash);
	GetNameSymbol(symbol, &name);
	Return(findnil_hashtable_(hash, name, &bit));

	/* Import symbol. */
	if (bit == Nil) {
		Return(import_bitpackage_(package, symbol, &bit, &check));
		Return(push_list_shadow_package_(package, symbol));
		SetBitTypeShadow(bit, 1);
		return 0;
	}

	/* Push into shadowing-symbols */
	str = StructBitType(bit);
	if (str->shadow == 0) {
		Return(push_list_shadow_package_(package, symbol));
		str->shadow = 1;
	}

	/* If already intern the same symbol. */
	GetBitTypeSymbol(bit, &pos);
	if (pos == symbol)
		return 0;

	/* Change the non-inherited symbol to the new symbol. */
	if (str->expt == 0) {
		Return(delete_hashtable_(hash, name, &check));
		return import_bitpackage_(package, symbol, &bit, &check);
	}

	/* Inherited symbol. */
	SetBitTypeSymbol(bit, symbol);
	return inherited_shadowing_import_package_(package, symbol, pos);
}

static int list_shadowing_import_package_(addr package, addr args)
{
	addr list, pos, type;

	/* type check */
	Check(! listp(args), "type error");
	list = args;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! symbolp(pos)) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_va_(NULL, args, type,
					"SHADOWING-IMPORT ~S must be a symbol.", pos, NULL);
		}
	}

	/* shadowing-import */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(symbol_shadowing_import_package_(package, pos));
	}

	return 0;
}

int shadowing_import_package_(addr package, addr pos)
{
	addr type;

	Return(package_designator_update_p_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			return symbol_shadowing_import_package_(package, pos);

		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return list_shadowing_import_package_(package, pos);

		default:
			GetTypeTable(&type, StringList);
			return call_type_error_va_(NULL, pos, type,
					"SHADOWING-IMPORT ~S must be a symbol or list.", pos, NULL);
	}
}


/************************************************************
 *  package_use.c
 ************************************************************/

/****************************************************************************
 *  Function USE-PACKAGE
 ****************************************************************************/
/*
 *  restart
 */
static int shadow_execute_use_package_(addr pg, addr name)
{
	enum PACKAGE_TYPE ignore;
	addr pos;

	Return(intern_package_(pg, name, &pos, &ignore));
	return shadow_package_(pg, pos);
}

static int restart_shadow_report_use_package_(
		Execute ptr, addr pg, addr name, addr *ret)
{
	enum PACKAGE_TYPE type;
	addr pos;

	Return(find_symbol_package_(pg, name, &pos, &type));
	if (type == PACKAGE_TYPE_NIL)
		pos = name;

	return format_string_(ptr, ret,
			"Use the original ~S, and make shadowing.",
			pos, NULL);
}

static int restart_shadow_use_package_(Execute ptr, addr pg, addr name, addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_SHADOW, &pos);
	restart_heap(&restart, pos);
	/* report */
	Return(restart_shadow_report_use_package_(ptr, pg, name, &pos));
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	return Result(ret, restart);
}

static int unintern_execute_use_package_(addr pg, addr name)
{
	enum PACKAGE_TYPE ignore1;
	int ignore2;
	addr pos;

	Return(intern_package_(pg, name, &pos, &ignore1));
	return unintern_package_(pg, pos, &ignore2);
}

static int restart_unintern_report_use_package_(
		Execute ptr, addr pg, addr name, addr *ret)
{
	enum PACKAGE_TYPE type;
	addr pos;

	Return(find_symbol_package_(pg, name, &pos, &type));
	if (type == PACKAGE_TYPE_NIL)
		pos = name;

	return format_string_(ptr, ret,
			"Use the inherited ~S, and unintern the original symbol.",
			pos, NULL);
}

static int restart_unintern_use_package_(Execute ptr, addr pg, addr name, addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_UNINTERN, &pos);
	restart_heap(&restart, pos);
	/* report */
	Return(restart_unintern_report_use_package_(ptr, pg, name, &pos));
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	return Result(ret, restart);
}

static int shadow_use_package_(addr pg, addr name)
{
	Execute ptr;
	addr restart1, restart2, control;

	ptr = Execute_Thread;
	Return(restart_unintern_use_package_(ptr, pg, name, &restart1));
	Return(restart_shadow_use_package_(ptr, pg, name, &restart2));

	push_control(ptr, &control);
	pushrestart_control(ptr, restart2);
	pushrestart_control(ptr, restart1);

	(void)call_simple_package_error_va_(NULL,
			"The name ~S causes a conflict in the ~S package.",
			name, pg, NULL);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* unintern */
	if (ptr->throw_handler == restart1) {
		normal_throw_control(ptr);
		Return(unintern_execute_use_package_(pg, name));
		goto escape;
	}

	/* shadow */
	if (ptr->throw_handler == restart2) {
		normal_throw_control(ptr);
		Return(shadow_execute_use_package_(pg, name));
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}


/*
 *  use-package
 */
static int package_designator_use_package_(addr pos, addr *ret)
{
	addr keyword;

	Return(package_designator_(pos, &pos));
	GetConst(PACKAGE_KEYWORD, &keyword);
	if (pos == keyword) {
		*ret = Nil;
		return fmte_("Cannot use the ~S package.", pos, NULL);
	}

	return Result(ret, pos);
}

static int check_already_use_package(addr package, addr pos)
{
	addr list;

	if (package == pos)
		return 1;
	GetPackage(package, PACKAGE_INDEX_USE, &list);
	return find_list_eq_unsafe(pos, list);
}

static int check_conflict_use_package_(addr package, addr pos, addr name)
{
	enum PACKAGE_TYPE type;
	addr hash, bit;

	/* package check */
	GetPackage(package, PACKAGE_INDEX_TABLE, &hash);
	Return(findnil_hashtable_(hash, name, &bit));
	if (bit == Nil)
		return 0;
	if (StructBitType(bit)->shadow)
		return 0;

	/* symbol check */
	Return(find_symbol_package_(pos, name, &pos, &type));
	Check(type == PACKAGE_TYPE_NIL, "find_symbol error.");
	GetBitTypeSymbol(bit, &bit);
	if (bit != pos) {
		Return(shadow_use_package_(package, name));
	}

	return 0;
}

static int check_use_package_(addr package, addr pos)
{
	addr list, name;

	/* fiest check */
	if (check_already_use_package(package, pos))
		return 0;

	/* loop */
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &list);
	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(check_conflict_use_package_(package, pos, name));
	}

	return 0;
}

static int check_name_use_package_(addr package, addr hash1, addr hash2, addr name)
{
	enum PACKAGE_TYPE type;
	addr bit1, bit2, pos;

	/* left */
	Return(findnil_hashtable_(hash1, name, &bit1));
	if (bit1 == Nil)
		return 0;
	if (StructBitType(bit1)->expt == 0)
		return 0;

	/* right */
	Return(findnil_hashtable_(hash2, name, &bit2));
	Check(bit2 == Nil, "hashtable error.");
	if (bit2 == Nil)
		return 0;
	if (StructBitType(bit2)->expt == 0)
		return 0;

	/* package */
	Return(find_symbol_package_(package, name, &pos, &type));
	if (type == PACKAGE_TYPE_NIL) {
		GetBitTypeSymbol(bit1, &bit1);
		GetBitTypeSymbol(bit2, &bit2);
		if (bit1 != bit2)
			return shadow_use_package_(package, name);
	}

	return 0;
}

static int check_export_use_package_(addr package, addr hash1, addr pos)
{
	addr hash2, list, name;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &hash2);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &list);
	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(check_name_use_package_(package, hash1, hash2, name));
	}

	return 0;
}

static int check_loop_use_package_(addr package, addr pos, addr list)
{
	addr hash1, check;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &hash1);
	while (list != Nil) {
		GetCons(list, &check, &list);
		Return(package_designator_use_package_(check, &check));
		if (check == pos)
			continue;
		if (check == package)
			continue;

		Return(check_export_use_package_(package, hash1, check));
	}

	return 0;
}

static int check_list_use_package_(addr package, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(package_designator_use_package_(pos, &pos));
		if (package == pos)
			continue;
		Return(check_loop_use_package_(package, pos, list));
	}

	return 0;
}

static int intern_use_package_(addr hash1, addr hash2, addr name)
{
	addr cons, bit, symbol;

	Return(intern_hashheap_(hash1, name, &cons));
	GetCdr(cons, &bit);
	if (bit != Nil)
		return 0;

	Return(findnil_hashtable_(hash2, name, &bit));
	Check(bit == Nil, "use-package error");
	GetBitTypeSymbol(bit, &symbol);
	inheritedbitpackage(&bit, symbol);
	SetCdr(cons, bit);

	return 0;
}

static int execute_use_package_(addr package, addr pos)
{
	addr hash1, hash2, list, name;

	/* ignore */
	Return(package_designator_use_package_(pos, &pos));
	if (check_already_use_package(package, pos))
		return 0;

	/* execute */
	GetPackage(package, PACKAGE_INDEX_TABLE, &hash1);
	GetPackage(pos, PACKAGE_INDEX_TABLE, &hash2);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &list);
	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(intern_use_package_(hash1, hash2, name));
	}
	push_list_use_package(package, pos);
	push_list_used_package(pos, package);

	return 0;
}

static int package_use_package_(addr package, addr pos)
{
	Return(package_designator_use_package_(pos, &pos));
	Return(check_use_package_(package, pos));
	Return(execute_use_package_(package, pos));

	return 0;
}

static int list_use_package_(addr package, addr args)
{
	addr list, pos, type;

	/* type check */
	Check(! listp(args), "type error");
	list = args;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! package_designator_p(pos)) {
			GetTypeTable(&type, PackageDesignator);
			return call_type_error_va_(NULL, args, type,
					"USE-PACKAGE ~S must be a package-designator.", pos, NULL);
		}
	}

	/* conflict check */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(package_designator_use_package_(pos, &pos));
		Return(check_use_package_(package, pos));
	}
	Return(check_list_use_package_(package, args));

	/* use-package */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(execute_use_package_(package, pos));
	}

	return 0;
}

int use_package_(addr package, addr pos)
{
	addr type;

	Return(package_designator_update_p_(package, &package));
	Return(package_designator_use_package_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
		case LISPTYPE_T:
		case LISPTYPE_CHARACTER:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
			return package_use_package_(package, pos);

		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return list_use_package_(package, pos);

		default:
			GetTypeTable(&type, PackageDesignatorList);
			return call_type_error_va_(NULL, pos, type,
					"USE-PACKAGE ~S must be a package-designator or list.", pos, NULL);
	}
}


/****************************************************************************
 *  Function UNUSE-PACKAGE
 ****************************************************************************/
static int check_uselist_package(addr package, addr pos)
{
	addr list;
	GetPackage(package, PACKAGE_INDEX_USE, &list);
	return find_list_eq_unsafe(pos, list);
}

static int execute_unuse_package_(addr package, addr pos)
{
	int check;
	addr hash, list, name, bit;

	Return(package_designator_(pos, &pos));
	if (check_uselist_package(package, pos) == 0)
		return 0;

	GetPackage(package, PACKAGE_INDEX_TABLE, &hash);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &list);
	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(findnil_hashtable_(hash, name, &bit));
		Check(bit == Nil, "unuse-package error");
		if (StructBitType(bit)->inherit) {
			Return(delete_hashtable_(hash, name, &check));
		}
	}
	delete_list_use_package(package, pos);
	delete_list_used_package(pos, package);

	return 0;
}

static int list_unuse_package_(addr package, addr args)
{
	addr list, pos, type;

	/* type check */
	Check(! listp(args), "type error");
	list = args;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! package_designator_p(pos)) {
			GetTypeTable(&type, PackageDesignator);
			return call_type_error_va_(NULL, args, type,
					"UNUSE-PACKAGE ~S must be a package-designator.", pos, NULL);
		}
	}

	/* unuse-package */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(execute_unuse_package_(package, pos));
	}

	return 0;
}

int unuse_package_(addr package, addr pos)
{
	addr type;

	Return(package_designator_update_p_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
		case LISPTYPE_T:
		case LISPTYPE_CHARACTER:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
			return execute_unuse_package_(package, pos);

		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return list_unuse_package_(package, pos);

		default:
			GetTypeTable(&type, PackageDesignatorList);
			return call_type_error_va_(NULL, pos, type,
					"UNUSE-PACKAGE ~S must be a package-designator or list.", pos, NULL);
	}
}


/************************************************************
 *  paper.c
 ************************************************************/

void paper_array_alloc(LocalRoot local, addr *ret, size_t array)
{
	alloc_array(local, ret, LISPTYPE_PAPER, array);
	SetUser(*ret, 0);
}

void paper_array_local(LocalRoot local, addr *ret, size_t array)
{
	CheckLocal(local);
	paper_array_alloc(local, ret, array);
}

void paper_array_heap(addr *ret, size_t array)
{
	paper_array_alloc(NULL, ret, array);
}

void paper_body_alloc(LocalRoot local, addr *ret, size_t body)
{
	alloc_body(local, ret, LISPTYPE_PAPER, body);
	SetUser(*ret, 0);
}

void paper_body_local(LocalRoot local, addr *ret, size_t body)
{
	CheckLocal(local);
	paper_body_alloc(local, ret, body);
}

void paper_body_heap(addr *ret, size_t body)
{
	paper_body_alloc(NULL, ret, body);
}

static int paper_arraybody(LocalRoot local, addr *ret, size_t array, size_t body)
{
	if (array == 0) {
		paper_body_alloc(local, ret, body);
		return 0;
	}
	if (body == 0) {
		paper_array_alloc(local, ret, array);
		return 0;
	}
	if (array <= 0xFFULL && body <= 0xFFULL) {
		alloc_smallsize(local, ret, LISPTYPE_PAPER, array, body);
		SetUser(*ret, 0);
		return 0;
	}
	if (array <= 0xFFFFULL && body <= 0xFFFFULL) {
		alloc_arraybody(local, ret, LISPTYPE_PAPER, array, body);
		SetUser(*ret, 0);
		return 0;
	}
	*ret = Nil;
	return 1;
}

int paper_arraybody_alloc_(LocalRoot local, addr *ret, size_t array, size_t body)
{
	if (paper_arraybody(local, ret, array, body))
		return fmte_("Invalid paper size.", NULL);

	return 0;
}

int paper_arraybody_local_(LocalRoot local, addr *ret, size_t array, size_t body)
{
	CheckLocal(local);
	return paper_arraybody_alloc_(local, ret, array, body);
}

int paper_arraybody_heap_(addr *ret, size_t array, size_t body)
{
	return paper_arraybody_alloc_(NULL, ret, array, body);
}

int paperp(addr pos)
{
	return GetType(pos) == LISPTYPE_PAPER;
}

int paper_array_p(addr pos)
{
	if (! paperp(pos))
		return 0;
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
		case LISPSIZE_ARRAY4:
		case LISPSIZE_ARRAY8:
		case LISPSIZE_SMALLSIZE:
		case LISPSIZE_ARRAYBODY:
			return 1;

		default:
			return 0;
	}
}

int paper_body_p(addr pos)
{
	if (! paperp(pos))
		return 0;
	switch (GetStatusSize(pos)) {
		case LISPSIZE_SMALLSIZE:
		case LISPSIZE_ARRAYBODY:
		case LISPSIZE_BODY2:
		case LISPSIZE_BODY4:
		case LISPSIZE_BODY8:
			return 1;

		default:
			return 0;
	}
}

void paper_copy_body_alloc(LocalRoot local, addr *ret, addr pos)
{
#ifdef LISP_DEBUG
	int check;
#endif
	addr x, src, dst;
	size_t array, body;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_array(pos, &array);
	paper_len_body(pos, &body);
#ifdef LISP_DEBUG
	check = paper_arraybody(local, &x, array, body);
	Check(check, "paper_arraybody error.");
#else
	(void)paper_arraybody_alloc_(local, &x, array, body);
#endif
	*ret = x;
	if (body == 0)
		return;

	/* copy */
	posbody(pos, &src);
	posbody(x, &dst);
	memcpy(dst, src, body);
}

void paper_get_type(addr pos, byte *ret)
{
	CheckType(pos, LISPTYPE_PAPER);
	*ret = GetUser(pos);
}

void paper_set_type(addr pos, byte value)
{
	CheckType(pos, LISPTYPE_PAPER);
	SetUser(pos, value);
}

void paper_len_array(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_PAPER);
	if (paper_array_p(pos))
		lenarray(pos, ret);
	else
		*ret = 0;
}

void paper_len_body(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_PAPER);
	if (paper_body_p(pos))
		lenbody(pos, ret);
	else
		*ret = 0;
}

void paper_get_array(addr pos, size_t index, addr *ret)
{
	Check(! paper_array_p(pos), "type error");
	getarray(pos, index, ret);
}

void paper_set_array(addr pos, size_t index, addr value)
{
	Check(! paper_array_p(pos), "type error");
	setarray(pos, index, value);
}

void paper_get_body(addr pos, size_t index, byte *ret)
{
	addr body;
#ifdef LISP_DEBUG
	size_t size;

	Check(! paper_body_p(pos), "type error");
	lenbody(pos, &size);
	Check(size <= index, "size error");
#endif
	posbody(pos, &body);
	*ret = body[index];
}

void paper_set_body(addr pos, size_t index, byte value)
{
	addr body;
#ifdef LISP_DEBUG
	size_t size;

	Check(! paper_body_p(pos), "type error");
	lenbody(pos, &size);
	Check(size <= index, "size error");
#endif
	posbody(pos, &body);
	body[index] = value;
}

void paper_ptr_body_unsafe(addr pos, void **ret)
{
	Check(! paper_body_p(pos), "type error");
	posbody(pos, (addr *)ret);
}

void paper_get_memory(addr pos, size_t a, size_t b, void *ptr, size_t *ret)
{
	addr body;
	size_t size;

	Check(! paper_body_p(pos), "type error");
	if (b <= a)
		goto zero;
	lenbody(pos, &size);
	if (size <= a)
		goto zero;
	b = (size < b)? size: b;
	size = b - a;
	posbody(pos, &body);
	memcpy(ptr, body + a, size);
	if (ret)
		*ret = size;
	return;

zero:
	if (ret)
		*ret = 0;
}

void paper_set_memory(addr pos, size_t a, size_t b, const void *ptr, size_t *ret)
{
	addr body;
	size_t size;

	Check(! paper_body_p(pos), "type error");
	if (b <= a)
		goto zero;
	lenbody(pos, &size);
	if (size <= a)
		goto zero;
	b = (size < b)? size: b;
	size = b - a;
	posbody(pos, &body);
	memcpy(body + a, ptr, size);
	if (ret)
		*ret = size;
	return;

zero:
	if (ret)
		*ret = 0;
}


/*
 *  syscall
 */
int paper_length_body_(addr pos, addr *ret)
{
	size_t size;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_body(pos, &size);
	make_index_integer_heap(ret, size);

	return 0;
}

int paper_length_array_(addr pos, addr *ret)
{
	size_t size;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_array(pos, &size);
	make_index_integer_heap(ret, size);

	return 0;
}

int paper_list_body_(addr pos, addr *ret)
{
	byte c;
	addr list, x;
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_body(pos, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		paper_get_body(pos, i, &c);
		fixnum_heap(&x, (fixnum)c);
		cons_heap(&list, x, list);
	}
	nreverse(ret, list);

	return 0;
}

int paper_list_array_(addr pos, addr *ret)
{
	addr list, x;
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_array(pos, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		paper_get_array(pos, i, &x);
		cons_heap(&list, x, list);
	}
	nreverse(ret, list);

	return 0;
}

int paper_vector_body_(addr pos, addr *ret)
{
	byte c;
	addr array;
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_body(pos, &size);
	Return(array_unsigned8_heap_(&array, size));
	for (i = 0; i < size; i++) {
		paper_get_body(pos, i, &c);
		Return(array_set_unsigned8_(array, i, c));
	}
	*ret = array;

	return 0;
}

int paper_vector_array_(addr pos, addr *ret)
{
	addr vector, x;
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_array(pos, &size);
	vector_heap(&vector, size);
	for (i = 0; i < size; i++) {
		paper_get_array(pos, i, &x);
		setarray(vector, i, x);
	}
	*ret = vector;

	return 0;
}

int paper_get_type_(addr pos, addr *ret)
{
	byte c;

	CheckType(pos, LISPTYPE_PAPER);
	paper_get_type(pos, &c);
	fixnum_heap(ret, (fixnum)c);

	return 0;
}

int paper_set_type_(addr pos, addr second)
{
	byte c;

	CheckType(pos, LISPTYPE_PAPER);
	if (GetByte_integer(second, &c))
		return fmte_("Invalid paper type, ~S.", second, NULL);
	paper_set_type(pos, c);

	return 0;
}

int paper_get_array_(addr pos, addr index, addr *ret)
{
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_array(pos, &size);
	Return(getindex_integer_(index, &i));
	if (size <= i) {
		*ret = Nil;
		return fmte_("Too large index, ~S.", index, NULL);
	}
	paper_get_array(pos, i, ret);

	return 0;
}

int paper_set_array_(addr pos, addr index, addr value)
{
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_array(pos, &size);
	Return(getindex_integer_(index, &i));
	if (size <= i)
		return fmte_("Too large index, ~S.", index, NULL);
	paper_set_array(pos, i, value);

	return 0;
}

int paper_get_body_(addr pos, addr index, addr *ret)
{
	byte c;
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_body(pos, &size);
	Return(getindex_integer_(index, &i));
	if (size <= i) {
		*ret = Nil;
		return fmte_("Too large index, ~S.", index, NULL);
	}
	paper_get_body(pos, i, &c);
	fixnum_heap(ret, (fixnum)c);

	return 0;
}

int paper_set_body_(addr pos, addr index, addr value)
{
	byte c;
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_body(pos, &size);
	Return(getindex_integer_(index, &i));
	if (size <= i)
		return fmte_("Too large index, ~S.", index, NULL);
	if (GetByte_integer(value, &c))
		return fmte_("Invalid paper value, ~S.", value, NULL);
	paper_set_body(pos, i, c);

	return 0;
}


/************************************************************
 *  parse.c
 ************************************************************/

int check_variable_(addr symbol)
{
	if (! symbolp(symbol))
		return fmte_("The variable ~S must be a symbol.", symbol, NULL);
	if (GetStatusReadOnly(symbol))
		return fmte_("The variable ~S don't allow constant symbol.", symbol, NULL);

	return 0;
}

int check_function_variable_(addr symbol)
{
	addr check;

	if (symbolp(symbol)) {
		if (GetStatusReadOnly(symbol))
			return fmte_("The variable ~S don't allow constant symbol.", symbol, NULL);
	}
	else if (callnamep(symbol)) {
		GetCallName(symbol, &check);
		if (! symbolp(check))
			return fmte_("The variable ~S must be a symbol.", check, NULL);
		if (constantp_callname(symbol))
			return fmte_("The variable ~S don't allow constant symbol.", check, NULL);
	}
	else {
		return fmte_("The ~S don't allow variable.", symbol, NULL);
	}

	return 0;
}

int tagbody_tag_p(addr pos)
{
	/*
	 * Common Lisp the Language, 2nd Edition
	 * 7.8.5. The ``Program Feature''
	 * a symbol or an integer, in which case it is called a tag, ...
	 */
	return symbolp(pos) || integerp(pos);
}


/************************************************************
 *  parse_function.c
 ************************************************************/

/*
 *  declare
 */
int parse_declare_body_(Execute ptr, addr cons, addr *retdecl, addr *retbody)
{
	addr env;
	LocalHold hold;

	Return(environment_heap_(ptr, &env));
	hold = LocalHold_local_push(ptr, env);
	Return(declare_body_(ptr, env, cons, retdecl, retbody));
	close_environment(env);
	localhold_end(hold);

	return 0;
}

static int parse_declare_body_documentation_(Execute ptr,
		addr cons, addr *rdoc, addr *rdecl, addr *rbody)
{
	addr env;
	LocalHold hold;

	Return(environment_heap_(ptr, &env));
	hold = LocalHold_local_push(ptr, env);
	Return(declare_body_documentation_(ptr, env, cons, rdoc, rdecl, rbody));
	close_environment(env);
	localhold_end(hold);

	return 0;
}

static int parse_parse_type_(Execute ptr, addr *ret, addr type)
{
	addr env;
	LocalHold hold;

	Return(environment_heap_(ptr, &env));
	hold = LocalHold_local_push(ptr, env);
	Return(parse_type_values_(ptr, ret, type, env));
	close_environment(env);
	localhold_end(hold);

	return 0;
}


/*
 *  eval-parse
 */
static int parse_allcons_(Execute ptr, addr *ret, addr cons)
{
	addr root, pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	localhold_set(hold, 0, cons);
	for (root = Nil; cons != Nil; ) {
		Return_getcons(cons, &pos, &cons);
		Return(parse_execute_(ptr, &pos, pos));
		cons_heap(&root, pos, root);
		localhold_set(hold, 1, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int localhold_parse_execute_(LocalHold hold, Execute ptr, addr *ret, addr pos)
{
	Return(parse_execute_(ptr, ret, pos));
	localhold_push(hold, *ret);
	return 0;
}

static int localhold_parse_allcons_(LocalHold hold, Execute ptr, addr *ret, addr cons)
{
	Return(parse_allcons_(ptr, ret, cons));
	localhold_push(hold, *ret);
	return 0;
}

/* progn */
static int parse_progn_(Execute ptr, addr *ret, addr cons)
{
	addr list;

	GetCdr(cons, &list);
	Return(parse_allcons_(ptr, &list, list));
	eval_parse2_heap(ret, EVAL_PARSE_PROGN, cons, list);

	return 0;
}

/* let */
static int parse_letone_(addr one, addr *rets, addr *retv)
{
	addr symbol, value;

	/* symbol */
	if (symbolp(one)) {
		*rets = one;
		*retv = Nil;
		return 0;
	}

	/* not cons */
	if (! consp(one)) {
		*rets = *retv = Nil;
		return fmte_("Invalid let argument ~S.", one, NULL);
	}

	/* (symbol) */
	GetCons(one, &symbol, &one);
	if (one == Nil) {
		*rets = symbol;
		*retv = Nil;
		return 0;
	}

	/* (symbol . value) */
	if (! consp(one)) {
		*rets = *retv = Nil;
		return fmte_("Invalid let argument ~S.", one, NULL);
	}

	/* (symbol value . tail) */
	GetCons(one, &value, &one);
	if (one != Nil) {
		*rets = *retv = Nil;
		return fmte_("Invalid let argument ~S.", one, NULL);
	}

	/* (symbol value) */
	*rets = symbol;
	*retv = value;
	return 0;
}

static int parse_let_arg_(Execute ptr, addr *ret, addr args)
{
	addr cons, one, symbol, value;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (cons = Nil; args != Nil; ) {
		Return_getcons(args, &one, &args);
		Return(parse_letone_(one, &symbol, &value));
		Return(check_variable_(symbol));
		Return(parse_self_(ptr, value));
		cons_heap(&one, symbol, value);
		cons_heap(&cons, one, cons);
		localhold_set(hold, 0, cons);
	}
	localhold_end(hold);
	nreverse(ret, cons);

	/* macro */
	args = *ret;
	while (args != Nil) {
		GetCons(args, &one, &args);
		GetCar(one, &symbol);
		Return(lexical_envstack_(ptr, symbol));
	}

	return 0;
}

static int parse_let_(Execute ptr, addr *ret, addr cons)
{
	addr rollback, cdr, args, decl, eval;
	LocalHold hold;

	/* args, decl, body */
	GetCdr(cons, &cdr);
	if (! consp(cdr))
		return fmte_("let form must be a (let args . body).", NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return_getcons(cdr, &args, &cdr);
	Return(parse_let_arg_(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_(ptr, cdr, &decl, &cdr));
	localhold_pushva(hold, decl, cdr, NULL);
	Return(localhold_parse_allcons_(hold, ptr, &cdr, cdr));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LET, 4);
	SetEvalParse(eval, 0, cons);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, cdr);
	return Result(ret, eval);
}

/* let* */
static int parse_leta_arg_(Execute ptr, addr *ret, addr args)
{
	addr cons, one, symbol, value;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (cons = Nil; args != Nil; ) {
		Return_getcons(args, &one, &args);
		Return(parse_letone_(one, &symbol, &value));
		Return(check_variable_(symbol));
		Return(parse_self_(ptr, value));
		cons_heap(&one, symbol, value);
		cons_heap(&cons, one, cons);
		localhold_set(hold, 0, cons);
		/* macro */
		Return(lexical_envstack_(ptr, symbol));
	}
	localhold_end(hold);
	nreverse(ret, cons);

	return 0;
}

static int parse_leta_(Execute ptr, addr *ret, addr cons)
{
	addr rollback, cdr, args, decl, eval;
	LocalHold hold;

	/* args, decl, body */
	GetCdr(cons, &cdr);
	if (! consp(cdr))
		return fmte_("let* form must be a (let* args . body).", NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return_getcons(cdr, &args, &cdr);
	Return(parse_leta_arg_(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_(ptr, cdr, &decl, &cdr));
	localhold_pushva(hold, decl, cdr, NULL);
	Return(localhold_parse_allcons_(hold, ptr, &cdr, cdr));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LETA, 4);
	SetEvalParse(eval, 0, cons);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, cdr);
	return Result(ret, eval);
}

/* setq */
static int parse_setq_symbol_p_(Execute ptr, addr list, int *ret)
{
	int check;
	addr symbol, value;

	while (list != Nil) {
		Return_getcons(list, &symbol, &list);
		Return(check_variable_(symbol));
		Return(symbol_macrolet_envstack_p_(ptr, symbol, &value, &check));
		if (check)
			return Result(ret, 1);
		Return_getcons(list, &symbol, &list);
	}

	return Result(ret, 0);
}

static int parse_setq_macrolet_(Execute ptr, addr *ret, addr cons)
{
	int check;
	addr progn, root, setq, setf, var, value;

	/* symbol-macrolet
	 *  `(progn
	 *     (setq var1 value1)
	 *     (setf expand2 value2)
	 *     ...)
	 */
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_SETF, &setf);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &var, &cons);
		GetCons(cons, &value, &cons);
		Return(symbol_macrolet_envstack_p_(ptr, var, &var, &check));
		if (check)
			list_heap(&var, setf, var, value, NULL);
		else
			list_heap(&var, setq, var, value, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(&root, root);
	GetConst(COMMON_PROGN, &progn);
	cons_heap(&progn, progn, root);

	return parse_execute_(ptr, ret, progn);
}

static int parse_setq_symbol_(Execute ptr, addr *ret, addr cons)
{
	addr cdr, root, one, symbol;
	LocalHold hold;

	/* parse */
	GetCdr(cons, &cdr);
	hold = LocalHold_array(ptr, 1);
	symbol = NULL;
	for (root = Nil; cdr != Nil; ) {
		Return_getcons(cdr, &one, &cdr);
		if (symbol == NULL) {
			Return(check_variable_(one));
			symbol = one;
		}
		else {
			Return(parse_self_(ptr, one));
			cons_heap(&one, symbol, one);
			cons_heap(&root, one, root);
			localhold_set(hold, 0, root);
			symbol = NULL;
		}
	}
	localhold_end(hold);
	if (symbol != NULL)
		return fmte_("setq symbol ~S don't have a value argument.", symbol, NULL);
	nreverse(&root, root);

	/* eval */
	eval_parse2_heap(ret, EVAL_PARSE_SETQ, cons, root);
	return 0;
}

static int parse_setq_(Execute ptr, addr *ret, addr cons)
{
	int check;
	addr args;

	GetCdr(cons, &args);
	Return(parse_setq_symbol_p_(ptr, args, &check));
	if (check)
		return parse_setq_macrolet_(ptr, ret, args);
	else
		return parse_setq_symbol_(ptr, ret, cons);
}

/* defun */
static int check_variable_env_(Execute ptr, addr x)
{
	Return(check_variable_(x));
	Return(lexical_envstack_(ptr, x));
	return 0;
}

static inline int check_variable_notnil_(Execute ptr, addr x)
{
	if (x != Nil)
		return check_variable_env_(ptr, x);
	return 0;
}

static int parse_var_(Execute ptr, addr *ret, addr cons)
{
	addr root, var;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &var, &cons);
		Return(check_variable_env_(ptr, var));
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);

	return 0;
}

static int parse_optional_(Execute ptr, addr *ret, addr cons)
{
	addr root, pos, var, init;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		/* (var init svar) */
		GetCons(pos, &var, &pos);
		GetCons(pos, &init, &pos);
		GetCar(pos, &pos);
		Return(check_variable_env_(ptr, var));
		Return(parse_self_(ptr, init));
		Return(check_variable_notnil_(ptr, pos));
		/* push */
		list_heap(&pos, var, init, pos, NULL);
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int parse_key_(Execute ptr, addr *ret, addr cons)
{
	addr root, pos, var, name, init;
	LocalHold hold;

	if (cons == T)
		return Result(ret, Nil);
	hold = LocalHold_array(ptr, 1);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		/* (var name init svar) */
		GetCons(pos, &var, &pos);
		GetCons(pos, &name, &pos);
		GetCons(pos, &init, &pos);
		GetCar(pos, &pos);
		Return(check_variable_env_(ptr, var));
		Return(parse_self_(ptr, init));
		Return(check_variable_notnil_(ptr, pos));
		/* push */
		list_heap(&pos, var, name, init, pos, NULL);
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int parse_aux_(Execute ptr, addr *ret, addr cons)
{
	addr root, pos, var;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		/* (var init) */
		GetCons(pos, &var, &pos);
		GetCar(pos, &pos);
		Return(check_variable_env_(ptr, var));
		Return(parse_self_(ptr, pos));
		/* push */
		list_heap(&pos, var, pos, NULL);
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	nreverse(ret, root);
	localhold_end(hold);

	return 0;
}

static int parse_ordinary_cons_(Execute ptr, addr *ret, addr args)
{
	addr var, opt, rest, key, allow, aux;
	LocalHold hold;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	hold = LocalHold_local(ptr);
	/* var */
	Return(parse_var_(ptr, &var, var));
	localhold_push(hold, var);
	/* opt */
	Return(parse_optional_(ptr, &opt, opt));
	localhold_push(hold, opt);
	/* rest */
	Return(check_variable_notnil_(ptr, rest));
	/* key */
	Return(parse_key_(ptr, &key, key));
	localhold_push(hold, key);
	/* aux */
	Return(parse_aux_(ptr, &aux, aux));
	localhold_push(hold, aux);
	/* result */
	localhold_end(hold);
	list_heap(ret, var, opt, rest, key, allow, aux, NULL);

	return 0;
}

int parse_ordinary_(Execute ptr, addr *ret, addr args)
{
	Return(lambda_ordinary_(ptr->local, &args, args));
	return parse_ordinary_cons_(ptr, ret, args);
}

static void parse_implicit_block(addr *ret, addr name, addr list)
{
	addr block;

	GetConst(COMMON_BLOCK, &block);
	if (callnamep(name))
		GetCallName(name, &name);
	lista_heap(&list, block, name, list, NULL);
	conscar_heap(ret, list);
}

static int parse_defun_(Execute ptr, addr *ret, addr cons)
{
	addr eval, name, args, decl, doc, body, form;
	LocalHold hold;

	/* (eval::defun name args decl doc body form) */
	List_bind(cons, &name, &args, &decl, &doc, &body, &form, NULL);

	/* parse */
	hold = LocalHold_local(ptr);
	Return(parse_ordinary_cons_(ptr, &args, args));
	localhold_push(hold, args);
	parse_implicit_block(&body, name, body);
	localhold_push(hold, body);
	Return(localhold_parse_allcons_(hold, ptr, &body, body));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_DEFUN, 6);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, name);
	SetEvalParse(eval, 2, args);
	SetEvalParse(eval, 3, decl);
	SetEvalParse(eval, 4, doc);
	SetEvalParse(eval, 5, body);

	return Result(ret, eval);
}

/* defmacro */
static int parse_macro_lambda_list_(Execute ptr, addr *ret, addr args);

static int parse_macro_variable_(Execute ptr, addr *ret, addr var)
{
	if (consp(var)) {
		return parse_macro_lambda_list_(ptr, ret, var);
	}
	else {
		Return(check_variable_env_(ptr, var));
		return Result(ret, var);
	}
}

static int parse_macro_var_(Execute ptr, addr *ret, addr cons)
{
	addr root, var;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &var, &cons);
		Return(parse_macro_variable_(ptr, &var, var));
		cons_heap(&root, var, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int parse_macro_optional_(Execute ptr, addr *ret, addr cons)
{
	addr root, pos, var, init;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		/* (var init svar) */
		GetCons(pos, &var, &pos);
		GetCons(pos, &init, &pos);
		GetCar(pos, &pos);
		Return(parse_macro_variable_(ptr, &var, var));
		localhold_set(hold, 1, var);
		Return(parse_self_(ptr, init));
		Return(check_variable_notnil_(ptr, pos));
		/* push */
		list_heap(&pos, var, init, pos, NULL);
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static inline int parse_macro_rest_(Execute ptr, addr *ret, addr rest)
{
	addr car, cdr;

	if (rest == Nil)
		return Result(ret, Nil);

	/* (var . &rest) (var . &body) (var . nil) */
	GetCons(rest, &car, &cdr);
	Return(parse_macro_variable_(ptr, &car, car));
	cons_heap(ret, car, cdr);

	return 0;
}

static int parse_macro_key_(Execute ptr, addr *ret, addr cons)
{
	addr root, pos, var, name, init;
	LocalHold hold;

	if (cons == T)
		return Result(ret, Nil);
	hold = LocalHold_array(ptr, 2);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		/* (var name init svar) */
		GetCons(pos, &var, &pos);
		GetCons(pos, &name, &pos);
		GetCons(pos, &init, &pos);
		GetCar(pos, &pos);
		Return(parse_macro_variable_(ptr, &var, var));
		localhold_set(hold, 1, var);
		Return(parse_self_(ptr, init));
		Return(check_variable_notnil_(ptr, pos));
		/* push */
		list_heap(&pos, var, name, init, pos, NULL);
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int parse_macro_lambda_list_(Execute ptr, addr *ret, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;
	LocalHold hold;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	hold = LocalHold_local(ptr);
	/* var */
	Return(parse_macro_var_(ptr, &var, var));
	localhold_push(hold, var);
	/* opt */
	Return(parse_macro_optional_(ptr, &opt, opt));
	localhold_push(hold, opt);
	/* rest */
	Return(parse_macro_rest_(ptr, &rest, rest));
	localhold_push(hold, rest);
	/* key */
	Return(parse_macro_key_(ptr, &key, key));
	localhold_push(hold, key);
	/* aux */
	Return(parse_aux_(ptr, &aux, aux));
	localhold_push(hold, aux);
	/* others */
	Return(check_variable_notnil_(ptr, whole));
	Return(check_variable_notnil_(ptr, env));
	/* result */
	localhold_end(hold);
	list_heap(ret, var, opt, rest, key, allow, aux, whole, env, NULL);

	return 0;
}

static int make_macro_function_(Execute ptr, addr *ret, addr *reval,
		addr name, addr args, addr decl, addr doc, addr cons)
{
	addr eval, call;

	callname_heap(&call, name, CALLNAME_SYMBOL);
	eval_parse_heap(&eval, EVAL_PARSE_MACRO_LAMBDA, 5);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, doc);
	SetEvalParse(eval, 3, cons);
	SetEvalParse(eval, 4, call);
	if (reval)
		*reval = eval;

	return eval_result_macro_(ptr, eval, ret);
}

static int parse_defmacro_(Execute ptr, addr *ret, addr cons)
{
	addr rollback, eval, name, args, decl, doc, body, lambda, macro;
	LocalHold hold;

	/* (eval::defmacro name args decl doc body) */
	List_bind(cons, &name, &args, &decl, &doc, &body, NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return(parse_macro_lambda_list_(ptr, &args, args));
	localhold_push(hold, args);
	Return(localhold_parse_allcons_(hold, ptr, &body, body));
	Return(make_macro_function_(ptr, &lambda, &macro, name, args, decl, doc, body));
	localhold_push(hold, lambda);
	localhold_push(hold, macro);
	Return(defmacro_envstack_(ptr, name, lambda));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* defmacro */
	eval_parse_heap(&eval, EVAL_PARSE_DEFMACRO, 2);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, macro);

	return Result(ret, eval);
}

/* macro-lambda */
static int parse_macro_lambda_(Execute ptr, addr *ret, addr cons)
{
	addr rollback, eval, args, decl, doc;
	LocalHold hold;

	/* (macro-lambda args . body) */
	if (! consp(cons)) {
		return fmte_("MACRO-LAMBDA argument ~S "
				"must be (lambda-list . form).", cons, NULL);
	}

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	GetCons(cons, &args, &cons);
	Return(lambda_macro_(ptr->local, &args, args, Nil));
	localhold_push(hold, args);
	Return(parse_macro_lambda_list_(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_documentation_(ptr, cons, &doc, &decl, &cons));
	localhold_pushva(hold, doc, decl, cons, NULL);
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* macro-lambda */
	eval_parse_heap(&eval, EVAL_PARSE_MACRO_LAMBDA, 5);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, doc);
	SetEvalParse(eval, 3, cons);
	SetEvalParse(eval, 4, Nil);

	return Result(ret, eval);
}

/* deftype */
static int parse_deftype_(Execute ptr, addr *ret, addr cons)
{
	addr rollback, eval, name, args, decl, doc, body;
	LocalHold hold;

	/* (eval::deftype name args decl doc body) */
	List_bind(cons, &name, &args, &decl, &doc, &body, NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return(parse_macro_lambda_list_(ptr, &args, args));
	localhold_push(hold, args);
	parse_implicit_block(&body, name, body);
	localhold_push(hold, body);
	Return(localhold_parse_allcons_(hold, ptr, &body, body));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* deftype */
	eval_parse_heap(&eval, EVAL_PARSE_DEFTYPE, 5);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, body);
	return Result(ret, eval);
}

/* define-compiler-macro */
static int parse_define_compiler_macro_(Execute ptr, addr *ret, addr cons)
{
	addr rollback, eval, name, args, decl, doc, body;
	LocalHold hold;

	/* (eval::define-compiler-macro name args decl doc body) */
	List_bind(cons, &name, &args, &decl, &doc, &body, NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return(parse_macro_lambda_list_(ptr, &args, args));
	localhold_push(hold, args);
	parse_implicit_block(&body, name, body);
	localhold_push(hold, body);
	Return(localhold_parse_allcons_(hold, ptr, &body, body));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* define-compiler-macro */
	eval_parse_heap(&eval, EVAL_PARSE_DEFINE_COMPILER_MACRO, 5);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, body);

	return Result(ret, eval);
}

/* destructuring-bind */
static int parse_destructuring_bind_(Execute ptr, addr *ret, addr cons)
{
	addr rollback, eval, lambda, args, expr, decl, body, form;
	LocalHold hold;

	/* (eval::destructuring-bind args expr decl body) */
	List_bind(cons, &args, &expr, &decl, &body, &form, NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return(parse_macro_lambda_list_(ptr, &args, args));
	localhold_push(hold, args);
	Return(localhold_parse_allcons_(hold, ptr, &body, body));
	Return(localhold_parse_self_(hold, ptr, expr));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* lambda */
	eval_parse_heap(&lambda, EVAL_PARSE_MACRO_LAMBDA, 5);
	SetEvalParse(lambda, 0, args);
	SetEvalParse(lambda, 1, decl);
	SetEvalParse(lambda, 2, Nil);
	SetEvalParse(lambda, 3, body);
	SetEvalParse(lambda, 4, Nil);

	/* destructuring-bind */
	eval_parse_heap(&eval, EVAL_PARSE_DESTRUCTURING_BIND, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, lambda);

	return Result(ret, eval);
}

/* symbol-macrolet */
static int check_symbol_macrolet_(addr pos, addr decl)
{
	if (specialp_symbol(pos))
		return 1;
	if (! eval_declare_p(decl))
		return 0;
	getall_special_declare(decl, &decl);
	return find_list_eq_unsafe(pos, decl);

}

static int parse_symbol_macrolet_push_(Execute ptr, addr list, addr decl)
{
	addr pos, expr;

	/* parse */
	if (! consp_getcons(list, &pos, &list))
		goto error1;
	Return(check_function_variable_(pos));
	if (! consp_getcons(list, &expr, &list))
		goto error1;
	if (list != Nil)
		goto error1;
	/* special check */
	if (check_symbol_macrolet_(pos, decl))
		goto error2;
	/* push symbol-macro */
	return symbol_macrolet_envstack_(ptr, pos, expr);

error1:
	return fmte_("The symbol-macrolet arguemnt ~A "
			"must be a (symbol expansion) form.", list, NULL);

error2:
	return call_simple_program_error_va_(ptr,
			"The symbol ~S cannot declare the special.", pos, NULL);
}

int parse_symbol_macrolet_args_(Execute ptr, addr args, addr decl)
{
	addr pos;

	while (args != Nil) {
		if (! consp_getcons(args, &pos, &args))
			goto error;
		Return(parse_symbol_macrolet_push_(ptr, pos, decl));
	}
	return 0;

error:
	return fmte_("The symbol-macrolet arguemnt ~A "
			"must be a (symbol expansion) form.", args, NULL);
}

static int parse_symbol_macrolet_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, args, decl, rollback;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &args, &cons)) {
		return fmte_("symbol-macrolet form must be "
				"(symbol-macrolet args . body).", NULL);
	}
	/* local scope environment */
	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	/* decl */
	Return(parse_declare_body_(ptr, cons, &decl, &cons));
	localhold_pushva(hold, decl, cons, NULL);
	/* args */
	Return(parse_symbol_macrolet_args_(ptr, args, decl));
	/* body */
	Return(parse_allcons_(ptr, &cons, cons));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LOCALLY, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	return Result(ret, eval);
}

/* macrolet */
static int parse_macrolet_one_(Execute ptr, addr cons)
{
	addr rollback, name, args, doc, decl;
	LocalHold hold;

	/* parse */
	if (! consp_getcons(cons, &name, &cons))
		goto error;
	if (! symbolp(name))
		return fmte_("The name ~S must be a symbol.", name, NULL);
	Return(check_function_variable_(name));
	if (! consp_getcons(cons, &args, &cons))
		goto error;

	/* make macro-function */
	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return(lambda_macro_(ptr->local, &args, args, Nil));
	Return(parse_macro_lambda_list_(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_documentation_(ptr, cons, &doc, &decl, &cons));
	localhold_pushva(hold, doc, decl, cons, NULL);
	parse_implicit_block(&cons, name, cons);
	localhold_push(hold, cons);
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	Return(make_macro_function_(ptr, &cons, NULL, name, args, decl, doc, cons));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* add environment */
	Return(macrolet_envstack_(ptr, name, cons));

	return 0;

error:
	return fmte_("macrolet argument must be (name (...) . body) form.", NULL);
}

int parse_macrolet_args_(Execute ptr, addr args)
{
	addr pos;

	while (args != Nil) {
		Return_getcons(args, &pos, &args);
		Return(parse_macrolet_one_(ptr, pos));
	}

	return 0;
}

static int parse_macrolet_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, args, decl, rollback;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &args, &cons))
		return fmte_("macrolet form must be (macrolet args . body).", NULL);
	/* local scope environment */
	Return(snapshot_envstack_(ptr, &rollback));
	Return(parse_macrolet_args_(ptr, args));
	/* arguments */
	hold = LocalHold_local(ptr);
	Return(parse_declare_body_(ptr, cons, &decl, &cons));
	localhold_pushva(hold, decl, cons, NULL);
	Return(parse_allcons_(ptr, &cons, cons));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* macrolet -> locally */
	eval_parse_heap(&eval, EVAL_PARSE_LOCALLY, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	return Result(ret, eval);
}

/* quote */
static int parse_quote_(Execute ptr, addr *ret, addr cons)
{
	addr args, value;

	GetCdr(cons, &args);
	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args != Nil)
		goto error;

	/* eval */
	Return(load_value_(ptr, value));
	eval_parse2_heap(ret, EVAL_PARSE_QUOTE, cons, value);
	return 0;

error:
	return fmte_("QUOTE form must have one argument, ~S.", cons, NULL);
}

/* function */
static int parse_lambda_(Execute ptr, addr *ret, addr form)
{
	addr rollback, cons, eval, args, doc, decl;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &args, &cons))
		return fmte_("function lambda must be (lambda (...) body) form.", NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	/* args */
	Return(parse_ordinary_(ptr, &args, args));
	localhold_push(hold, args);
	/* doc, decl */
	Return(parse_declare_body_documentation_(ptr, cons, &doc, &decl, &cons));
	localhold_pushva(hold, doc, decl, NULL);
	/* cons */
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LAMBDA, 5);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, cons);

	return Result(ret, eval);
}

static int parse_function_argument_(Execute ptr, addr *ret, addr value)
{
	addr check, symbol, call;

	/* symbol function */
	if (! parse_callname_heap(&call, value)) {
		Return(check_function_variable_(call));
		eval_parse2_heap(ret, EVAL_PARSE_FUNCTION, value, call);
		return 0;
	}

	/* lambda function */
	if (! consp(value))
		return fmte_("FUNCTION ~S must be a fdefinition form.", value, NULL);
	GetConst(COMMON_LAMBDA, &symbol);
	GetCar(value, &check);
	if (check == symbol)
		return parse_lambda_(ptr, ret, value);

	/* others */
	return fmte_("FUNCTION ~S must be a fdefinition form.", value, NULL);
}

static int parse_function_(Execute ptr, addr *ret, addr cons)
{
	addr args, value;

	if (! consp_getcons(cons, &value, &args))
		goto error;
	if (args != Nil)
		goto error;

	return parse_function_argument_(ptr, ret, value);

error:
	return fmte_("FUNCTION form ~S must have one argument.", cons, NULL);
}

/* if */
static int parse_if_(Execute ptr, addr *ret, addr form)
{
	addr args, eval, expr, then, last;
	LocalHold hold;

	GetCdr(form, &args);
	if (! consp_getcons(args, &expr, &args))
		goto error;
	if (! consp_getcons(args, &then, &args))
		goto error;
	if (args == Nil) {
		last = Nil;
	}
	else {
		if (! consp_getcons(args, &last, &args))
			goto error;
		if (args != Nil)
			goto error;
	}

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, expr));
	Return(localhold_parse_self_(hold, ptr, then));
	Return(localhold_parse_self_(hold, ptr, last));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_IF, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, then);
	SetEvalParse(eval, 3, last);
	return Result(ret, eval);

error:
	return fmte_("IF form must be (if expr then &optnioal else).", NULL);
}

/* unwind-protect */
static int parse_unwind_protect_(Execute ptr, addr *ret, addr form)
{
	addr expr, args, eval;
	LocalHold hold;

	GetCdr(form, &args);
	if (! consp_getcons(args, &expr, &args)) {
		return fmte_("UNWIND-PROTECT form must be "
				"a (unwind-protect form . body).", NULL);
	}
	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, expr));
	Return(localhold_parse_allcons_(hold, ptr, &args, args));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_UNWIND_PROTECT, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, args);

	return Result(ret, eval);
}

/* tagbody */
static int parse_tagbody_findtag(addr key, addr cons)
{
	addr check;

	while (cons != Nil) {
		GetCons(cons, &check, &cons);
		GetEvalParse(check, 0, &check);
		if (eql_function(check, key))
			return 1;
	}

	return 0;
}

static void parse_tagbody_maketag(addr *ret, addr pos)
{
	eval_parse_heap(ret, EVAL_PARSE_TAG, 1);
	SetEvalParse(*ret, 0, pos);
}

static int parse_tagbody_check_(Execute ptr, addr cons, addr *rtag, addr *rbody)
{
	addr tag, body, pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	for (tag = body = Nil; cons != Nil; ) {
		Return_getcons(cons, &pos, &cons);
		if (consp(pos)) {
			Return(parse_self_(ptr, pos));
			cons_heap(&body, pos, body);
		}
		else if (tagbody_tag_p(pos)) {
			if (parse_tagbody_findtag(pos, tag))
				return fmte_("The tag ~S is already exists.", pos, NULL);
			parse_tagbody_maketag(&pos, pos);
			cons_heap(&tag, pos, tag);
			cons_heap(&body, pos, body);
			localhold_set(hold, 1, tag);
		}
		else {
			return fmte_("The tag ~S must be a symbol or integer.", pos, NULL);
		}
		localhold_set(hold, 0, body);
	}
	localhold_end(hold);
	nreverse(rtag, tag);
	nreverse(rbody, body);

	return 0;
}

static int parse_tagbody_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, tag;

	GetCdr(form, &cons);
	Return(parse_tagbody_check_(ptr, cons, &tag, &cons));
	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_TAGBODY, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, tag);
	SetEvalParse(eval, 2, cons);

	return Result(ret, eval);
}

/* go */
static int parse_go_(addr *ret, addr cons)
{
	addr tag;

	if (! consp_getcons(cons, &tag, &cons))
		return fmte_("GO form must be (go tag).", NULL);
	if (cons != Nil)
		return fmte_("GO form must be (go tag).", NULL);
	if (! tagbody_tag_p(tag))
		return fmte_("The tag ~S must be a symbol or integer.", tag, NULL);
	/* eval */
	eval_single_parse_heap(ret, EVAL_PARSE_GO, tag);

	return 0;
}

/* block */
static int parse_block_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, name;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &name, &cons))
		return fmte_("BLOCK form must be (block name . body).", NULL);
	if (! symbolp(name))
		return fmte_("BLOCK name ~S must be a symbol type.", name, NULL);
	Return(parse_allcons_(ptr, &cons, cons));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_BLOCK, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, name);
	SetEvalParse(eval, 2, cons);

	return Result(ret, eval);
}

/* return-from */
static int parse_return_from_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, name, value;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &name, &cons))
		goto error;
	if (cons == Nil) {
		value = Nil;
	}
	else {
		if (! consp_getcons(cons, &value, &cons))
			goto error;
		if (cons != Nil)
			goto error;
	}
	Return(parse_self_(ptr, value));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_RETURN_FROM, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, name);
	SetEvalParse(eval, 2, value);
	return Result(ret, eval);

error:
	return fmte_("RETURN-FROM form must be (return-from name [value]).", NULL);
}

/* catch */
static int parse_catch_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, tag;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &tag, &cons))
		return fmte_("CATCH form must be (catch tag . body).", NULL);
	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, tag));
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_CATCH, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, tag);
	SetEvalParse(eval, 2, cons);

	return Result(ret, eval);
}

/* throw */
static int parse_throw_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, tag, result;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &tag, &cons))
		goto error;
	if (! consp_getcons(cons, &result, &cons))
		goto error;
	if (cons != Nil)
		goto error;
	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, tag));
	Return(localhold_parse_self_(hold, ptr, result));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_THROW, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, tag);
	SetEvalParse(eval, 2, result);
	return Result(ret, eval);

error:
	return fmte_("THROW form must be (throw tag result).", NULL);
}

/* flet */
static int parse_flet_one_(Execute ptr, addr *ret, addr cons)
{
	addr name, call, args, doc, decl;
	LocalHold hold;

	if (! consp_getcons(cons, &name, &cons))
		goto error;
	hold = LocalHold_local(ptr);
	Return(parse_callname_error_(&call, name));
	localhold_push(hold, call);
	Return(check_function_variable_(call));
	if (! consp_getcons(cons, &args, &cons))
		goto error;

	Return(parse_ordinary_(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_documentation_(ptr, cons, &doc, &decl, &cons));
	localhold_pushva(hold, doc, decl, cons, NULL);
	parse_implicit_block(&cons, call, cons);
	localhold_push(hold, cons);
	Return(parse_allcons_(ptr, &cons, cons));
	localhold_push(hold, cons);
	localhold_end(hold);
	list_heap(ret, call, args, decl, doc, cons, NULL);
	return 0;

error:
	return fmte_("flet/labels argument must be (name (...) . body) form.", NULL);
}

static int parse_flet_args_(Execute ptr, addr *ret, addr args)
{
	addr root, pos;
	LocalHold hold;

	/* flet */
	hold = LocalHold_array(ptr, 1);
	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &pos, &args);
		Return(parse_flet_one_(ptr, &pos, pos));
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	/* macro */
	args = *ret;
	while (args != Nil) {
		GetCons(args, &pos, &args);
		GetCar(pos, &pos); /* call */
		Return(function_envstack_(ptr, pos));
	}

	return 0;
}

static int parse_flet_(Execute ptr, addr *ret, addr form)
{
	addr cons, rollback, eval, args, decl;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &args, &cons))
		return fmte_("FLET form must be (flet args . body).", NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return(parse_flet_args_(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_(ptr, cons, &decl, &cons));
	localhold_pushva(hold, decl, cons, NULL);
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_FLET, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, cons);

	return Result(ret, eval);
}

/* labels */
static int parse_labels_args_(Execute ptr, addr *ret, addr args)
{
	addr root, pos;
	LocalHold hold;

	/* macro */
	root = args;
	while (root != Nil) {
		GetCons(root, &pos, &root);
		Return_getcar(pos, &pos); /* call */
		Return(parse_callname_error_(&pos, pos));
		Return(function_envstack_(ptr, pos));
	}

	/* labels */
	hold = LocalHold_array(ptr, 1);
	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &pos, &args);
		Return(parse_flet_one_(ptr, &pos, pos));
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int parse_labels_(Execute ptr, addr *ret, addr form)
{
	addr cons, rollback, eval, args, decl;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &args, &cons))
		return fmte_("labels form must be (labels args . body).", NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return(parse_labels_args_(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_(ptr, cons, &decl, &cons));
	localhold_pushva(hold, decl, cons, NULL);
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LABELS, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, cons);

	return Result(ret, eval);
}

/* the */
static int parse_the_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, type, expr;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &type, &cons))
		goto error;
	if (! consp_getcons(cons, &expr, &cons))
		goto error;
	if (cons != Nil)
		goto error;

	hold = LocalHold_local(ptr);
	Return(parse_parse_type_(ptr, &type, type));
	localhold_push(hold, type);
	Return(localhold_parse_self_(hold, ptr, expr));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_THE, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, type);
	SetEvalParse(eval, 2, expr);
	return Result(ret, eval);

error:
	return fmte_("the form must be (the type expr).", NULL);
}

/* eval-when */
int parse_eval_when_list_(addr list, addr *rcompile, addr *rload, addr *rexec)
{
	addr compile1, compile2, load1, load2, exec1, exec2;
	addr pos;

	/* constant */
	GetConst(KEYWORD_COMPILE_TOPLEVEL, &compile1);
	GetConst(KEYWORD_LOAD_TOPLEVEL, &load1);
	GetConst(KEYWORD_EXECUTE, &exec1);
	GetConst(COMMON_COMPILE, &compile2);
	GetConst(COMMON_LOAD, &load2);
	GetConst(COMMON_EVAL, &exec2);

	/* check */
	*rcompile = *rload = *rexec = Nil;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (pos == compile1 || pos == compile2) {
			*rcompile = T;
		}
		else if (pos == load1 || pos == load2) {
			*rload = T;
		}
		else if (pos == exec1 || pos == exec2) {
			*rexec = T;
		}
		else {
			return fmte_("Invalid situation ~S in EVAL-WHEN.", pos, NULL);
		}
	}

	return 0;
}

int parse_eval_when_process(Execute ptr,
		addr compile, addr load, addr exec, addr toplevel, addr mode)
{
	/* not toplevel */
	if (toplevel == Nil) {
		return exec != Nil;
	}

	/* not compile */
	if (! eval_compile_p(ptr)) {
		return exec != Nil;
	}

	/* Discard */
	if (compile == Nil && load == Nil) {
		return exec != Nil
			&& mode != Nil; /* compile-time-too */
	}

	/* Process */
	if (compile != Nil && load != Nil) {
		set_compile_time_eval(ptr, T); /* compile-time-too */
		return 1;
	}
	if (compile == Nil && load != Nil && exec != Nil) {
		set_compile_time_eval(ptr, toplevel);
		return 1;
	}
	if (compile == Nil && load != Nil && exec == Nil) {
		set_compile_time_eval(ptr, Nil); /* not-compile-time */
		return 1;
	}

	/* Evaluate */
	return 1;
}

static int parse_eval_when_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, list;
	addr compile, load, exec, toplevel, mode, value;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &list, &cons))
		return fmte_("eval-when form must be (eval-when (...) . body).", NULL);

	/* arguments */
	Return(parse_eval_when_list_(list, &compile, &load, &exec));
	Return(get_toplevel_eval_(ptr, &toplevel));
	Return(get_compile_time_eval_(ptr, &value));

	/* discard */
	if (! parse_eval_when_process(ptr, compile, load, exec, toplevel, value)) {
		eval_single_parse_heap(ret, EVAL_PARSE_NIL, Nil);
		return 0;
	}

	/* body */
	Return(get_compile_time_eval_(ptr, &mode));
	Return(parse_allcons_(ptr, &cons, cons));
	set_compile_time_eval(ptr, value);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_EVAL_WHEN, 7);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, cons);
	SetEvalParse(eval, 2, compile);   /* :compile-toplevel */
	SetEvalParse(eval, 3, load);      /* :load-toplevel */
	SetEvalParse(eval, 4, exec);      /* :execute */
	SetEvalParse(eval, 5, toplevel);  /* toplevel */
	SetEvalParse(eval, 6, mode);      /* compile-time */
	return Result(ret, eval);
}

/* values */
static int parse_values_(Execute ptr, addr *ret, addr form)
{
	addr cons;

	GetCdr(form, &cons);
	Return(parse_allcons_(ptr, &cons, cons));
	eval_parse2_heap(ret, EVAL_PARSE_VALUES, form, cons);

	return 0;
}

/* locally */
static int parse_locally_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, decl;
	LocalHold hold;

	GetCdr(form, &cons);
	hold = LocalHold_array(ptr, 3);
	localhold_set(hold, 0, cons);
	Return(parse_declare_body_(ptr, cons, &decl, &cons));
	localhold_set(hold, 1, decl);
	localhold_set(hold, 2, cons);
	Return(parse_allcons_(ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LOCALLY, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	return Result(ret, eval);
}

/* declaim */
static int parse_declaim_(addr *ret, addr args)
{
#ifdef LISP_DEBUG
	addr right;

	CheckType(args, LISPTYPE_CONS);
	GetCdr(args, &right);
	Check(right != Nil, "argument error");
#endif
	GetCar(args, &args);
	eval_single_parse_heap(ret, EVAL_PARSE_DECLAIM, args);

	return 0;
}

/* call */
static int parse_call_(Execute ptr, addr *ret, addr form)
{
	addr call, cons, eval;
	LocalHold hold;

	GetCons(form, &call, &cons);
	hold = LocalHold_array(ptr, 2);
	localhold_set(hold, 0, call);
	localhold_set(hold, 1, cons);
	Return(parse_function_argument_(ptr, &call, call));
	localhold_set(hold, 0, call);
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_CALL, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, call);
	SetEvalParse(eval, 2, cons);

	return Result(ret, eval);
}

/* multiple-value-bind */
static int parse_multiple_value_bind_(Execute ptr, addr *ret, addr cons)
{
	addr form, eval, vars, expr, decl, doc, body;
	LocalHold hold;

	GetCdr(cons, &cons);
	if (! consp(cons))
		goto error;
	Return_getcons(cons, &vars, &cons);
	Return_getcons(cons, &expr, &cons);
	Return_getcons(cons, &decl, &cons);
	Return_getcons(cons, &doc, &cons);
	Return_getcons(cons, &body, &cons);
	Return_getcons(cons, &form, &cons);
	if (cons != Nil)
		goto error;

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, expr));
	Return(localhold_parse_allcons_(hold, ptr, &body, body));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_MULTIPLE_VALUE_BIND, 6);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, vars);
	SetEvalParse(eval, 2, expr);
	SetEvalParse(eval, 3, decl);
	SetEvalParse(eval, 4, doc);
	SetEvalParse(eval, 5, body);
	return Result(ret, eval);

error:
	return fmte_("The form ~S must be (system::multiple-value-bind "
			"(vars expr decl doc form).", cons, NULL);
}

/* multiple-value-call */
static int parse_multiple_value_call_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, expr;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &expr, &cons))
		goto error;

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, expr));
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_MULTIPLE_VALUE_CALL, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, cons);
	return Result(ret, eval);

error:
	return fmte_("The form ~S "
			"must be (multiple-value-call function . body).", cons, NULL);
}

/* multiple-value-prog1 */
static int parse_multiple_value_prog1_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, expr;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &expr, &cons))
		goto error;

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, expr));
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_MULTIPLE_VALUE_PROG1, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, cons);
	return Result(ret, eval);

error:
	return fmte_("The form ~S "
			"must be (multiple-value-prog1 first-form . body).", cons, NULL);
}

/* nth-value */
static int parse_nth_value_(Execute ptr, addr *ret, addr list)
{
	addr eval, next, nth, expr, form;
	LocalHold hold;

	if (! consp_getcons(list, &nth, &next))
		goto error;
	if (! consp_getcons(next, &expr, &next))
		goto error;
	if (! consp_getcons(next, &form, &next))
		goto error;
	if (next != Nil)
		goto error;

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, nth));
	Return(localhold_parse_self_(hold, ptr, expr));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_NTH_VALUE, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, nth);
	SetEvalParse(eval, 2, expr);
	return Result(ret, eval);

error:
	return fmte_("The form ~S must be (nth-value nth expr).", list, NULL);
}

/* progv */
static int parse_progv_(Execute ptr, addr *ret, addr form)
{
	addr eval, symbols, values, body;
	LocalHold hold;

	GetCdr(form, &body);
	if (! consp_getcons(body, &symbols, &body))
		goto error;
	if (! consp_getcons(body, &values, &body))
		goto error;

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, symbols));
	Return(localhold_parse_self_(hold, ptr, values));
	Return(localhold_parse_allcons_(hold, ptr, &body, body));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_PROGV, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, symbols);
	SetEvalParse(eval, 2, values);
	SetEvalParse(eval, 3, body);
	return Result(ret, eval);

error:
	return fmte_("The form ~S must be (progv symbols values . body).", form, NULL);
}

/* symbol */
static int parse_symbol_(Execute ptr, addr *ret, addr pos)
{
	int check;
	addr value;

	/* symbol macro */
	Return(symbol_macrolet_envstack_p_(ptr, pos, &value, &check));
	if (check)
		return parse_execute_(ptr, ret, value);

	/* symbol */
	eval_single_parse_heap(ret, EVAL_PARSE_SYMBOL, pos);
	return 0;
}

/* backquote */
static int parse_backquote_(Execute ptr, addr *ret, addr pos)
{
	if (! quote_back_p(pos))
		return fmte_("Invalid quote type.", NULL);
	getvalue_quote(pos, &pos);
	return parse_execute_(ptr, ret, pos);
}

/* parse_cons */
static int parse_cons_check_constant(addr call, constindex index)
{
	addr check;
	GetConstant(index, &check);
	return check == call;
}
#define ParseConsConstant(x, y) parse_cons_check_constant((x), CONSTANT_##y)

static int parse_cons_(Execute ptr, addr *ret, addr cons)
{
	addr call, check, args;

	/* macro */
	Return(parse_macroexpand_(ptr, &check, cons));
	if (check != Unbound) {
		return parse_execute_(ptr, ret, check);
	}

	/* operator */
	GetCons(cons, &call, &args);
	if (ParseConsConstant(call, COMMON_PROGN)) {
		return parse_progn_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_LOCALLY)) {
		return parse_locally_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_MACROLET)) {
		return parse_macrolet_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_SYMBOL_MACROLET)) {
		return parse_symbol_macrolet_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_EVAL_WHEN)) {
		return parse_eval_when_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_LET)) {
		return parse_let_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_LETA)) {
		return parse_leta_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_SETQ)) {
		return parse_setq_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_QUOTE)) {
		return parse_quote_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_FUNCTION)) {
		return parse_function_(ptr, ret, args);
	}
	if (ParseConsConstant(call, COMMON_IF)) {
		return parse_if_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_UNWIND_PROTECT)) {
		return parse_unwind_protect_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_TAGBODY)) {
		return parse_tagbody_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_GO)) {
		return parse_go_(ret, args);
	}
	if (ParseConsConstant(call, COMMON_BLOCK)) {
		return parse_block_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_RETURN_FROM)) {
		return parse_return_from_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_CATCH)) {
		return parse_catch_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_THROW)) {
		return parse_throw_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_FLET)) {
		return parse_flet_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_LABELS)) {
		return parse_labels_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_THE)) {
		return parse_the_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_VALUES)) {
		return parse_values_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, SYSTEM_DECLAIM)) {
		return parse_declaim_(ret, args);
	}
	if (ParseConsConstant(call, SYSTEM_DEFUN)) {
		return parse_defun_(ptr, ret, args);
	}
	if (ParseConsConstant(call, SYSTEM_DEFMACRO)) {
		return parse_defmacro_(ptr, ret, args);
	}
	if (ParseConsConstant(call, SYSTEM_DEFTYPE)) {
		return parse_deftype_(ptr, ret, args);
	}
	if (ParseConsConstant(call, SYSTEM_DEFINE_COMPILER_MACRO)) {
		return parse_define_compiler_macro_(ptr, ret, args);
	}
	if (ParseConsConstant(call, SYSTEM_DESTRUCTURING_BIND)) {
		return parse_destructuring_bind_(ptr, ret, args);
	}
	if (ParseConsConstant(call, SYSTEM_MACRO_LAMBDA)) {
		return parse_macro_lambda_(ptr, ret, args);
	}
	if (ParseConsConstant(call, SYSTEM_MULTIPLE_VALUE_BIND)) {
		return parse_multiple_value_bind_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_MULTIPLE_VALUE_CALL)) {
		return parse_multiple_value_call_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_MULTIPLE_VALUE_PROG1)) {
		return parse_multiple_value_prog1_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, SYSTEM_NTH_VALUE)) {
		return parse_nth_value_(ptr, ret, args);
	}
	if (ParseConsConstant(call, COMMON_PROGV)) {
		return parse_progv_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_LOAD_TIME_VALUE)) {
		return parse_load_time_value_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, SYSTEM_STEP)) {
		return parse_step_(ptr, ret, args);
	}

	return parse_call_(ptr, ret, cons);
}

static int parse_clos_(Execute ptr, addr *ret, addr pos)
{
	Return(load_value_(ptr, pos));
	eval_single_parse_heap(ret, EVAL_PARSE_CLOS, pos);
	return 0;
}

static void parse_array(addr *ret, addr pos)
{
	if (strarrayp(pos))
		eval_single_parse_heap(ret, EVAL_PARSE_STRING, pos);
	else
		eval_single_parse_heap(ret, EVAL_PARSE_ARRAY, pos);
}

static int parse_switch_(Execute ptr, addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_CONS:
			return parse_cons_(ptr, ret, pos);

		case LISPTYPE_NIL:
			eval_single_parse_heap(ret, EVAL_PARSE_NIL, Nil);
			break;

		case LISPTYPE_T:
			eval_single_parse_heap(ret, EVAL_PARSE_T, T);
			break;

		case LISPTYPE_CLOS:
			return parse_clos_(ptr, ret, pos);

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			eval_single_parse_heap(ret, EVAL_PARSE_INTEGER, pos);
			break;

		case LISPTYPE_RATIO:
			eval_single_parse_heap(ret, EVAL_PARSE_RATIONAL, pos);
			break;

		case LISPTYPE_COMPLEX:
			eval_single_parse_heap(ret, EVAL_PARSE_COMPLEX, pos);
			break;

		case LISPTYPE_CHARACTER:
			eval_single_parse_heap(ret, EVAL_PARSE_CHARACTER, pos);
			break;

		case LISPTYPE_ARRAY:
			parse_array(ret, pos);
			break;

		case LISPTYPE_VECTOR:
			eval_single_parse_heap(ret, EVAL_PARSE_VECTOR, pos);
			break;

		case LISPTYPE_BITVECTOR:
			eval_single_parse_heap(ret, EVAL_PARSE_BITVECTOR, pos);
			break;

		case LISPTYPE_STRING:
			eval_single_parse_heap(ret, EVAL_PARSE_STRING, pos);
			break;

		case LISPTYPE_SYMBOL:
			return parse_symbol_(ptr, ret, pos);

		case LISPTYPE_FUNCTION:
			eval_parse2_heap(ret, EVAL_PARSE_FUNCTION, pos, pos);
			break;

		case LISPTYPE_PACKAGE:
			eval_single_parse_heap(ret, EVAL_PARSE_PACKAGE, pos);
			break;

		case LISPTYPE_RANDOM_STATE:
			eval_single_parse_heap(ret, EVAL_PARSE_RANDOM_STATE, pos);
			break;

		case LISPTYPE_PATHNAME:
			eval_single_parse_heap(ret, EVAL_PARSE_PATHNAME, pos);
			break;

		case LISPTYPE_ENVIRONMENT:
			eval_single_parse_heap(ret, EVAL_PARSE_ENVIRONMENT, pos);
			break;

		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
		case LISPTYPE_SHORT_FLOAT:
			eval_single_parse_heap(ret, EVAL_PARSE_FLOAT, pos);
			break;

		case LISPTYPE_QUOTE:
			return parse_backquote_(ptr, ret, pos);

		case LISPTYPE_PAPER:
			eval_single_parse_heap(ret, EVAL_PARSE_PAPER, pos);
			break;

		default:
			return fmte_("parse-error: ~S.", pos, NULL);
	}

	return 0;
}

int parse_execute_(Execute ptr, addr *ret, addr pos)
{
	addr toplevel;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	localhold_set(hold, 0, pos);

	/* toplevel */
	Return(get_toplevel_eval_(ptr, &toplevel));
	if (toplevel == Nil) {
		Return(parse_switch_(ptr, ret, pos));
		goto finish;
	}

	/* parse */
	set_toplevel_eval(ptr, Nil);
	Return(parse_switch_(ptr, ret, pos));
	set_toplevel_eval(ptr, toplevel);

finish:
	localhold_end(hold);
	return 0;
}

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#ifdef __clang__
#pragma clang diagnostic pop
#endif
