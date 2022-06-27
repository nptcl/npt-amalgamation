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

#include <ctype.h>
#include <errno.h>
#include <locale.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lisp_file.h"


/************************************************************
 *  structure_access.c
 ************************************************************/

static int stdget_structure_constant_(addr pos, addr *ret,
		enum Clos_structure_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_structure_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STRUCTURE_CLASS, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}

static int stdset_structure_constant_(addr pos, addr value,
		enum Clos_structure_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_structure_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STRUCTURE_CLASS, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
		return 0;
	}
	else {
		GetConstant(index2, &check);
		return clos_set_(pos, check, value);
	}
}
#define StdGetStructure_(p,r,a,b) \
	stdget_structure_constant_((p), (r), Clos_structure_##a, CONSTANT_CLOSNAME_##b)
#define StdSetStructure_(p,r,a,b) \
	stdset_structure_constant_((p), (r), Clos_structure_##a, CONSTANT_CLOSNAME_##b)

int stdget_structure_name_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, name, NAME);
}
int stdset_structure_name_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, name, NAME);
}

int stdget_structure_direct_slots_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, direct_slots, DIRECT_SLOTS);
}
int stdset_structure_direct_slots_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, direct_slots, DIRECT_SLOTS);
}

int stdget_structure_slots_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, slots, SLOTS);
}
int stdset_structure_slots_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, slots, SLOTS);
}

int stdget_structure_documentation_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, documentation, DOCUMENTATION);
}
int stdset_structure_documentation_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, documentation, DOCUMENTATION);
}

int stdget_structure_include_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, include, INCLUDE);
}
int stdset_structure_include_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, include, INCLUDE);
}

int stdget_structure_precedence_list_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, precedence_list, CLASS_PRECEDENCE_LIST);
}
int stdset_structure_precedence_list_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, precedence_list, CLASS_PRECEDENCE_LIST);
}

int stdget_structure_value_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, value, VALUE);
}
int stdset_structure_value_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, value, VALUE);
}

int stdget_structure_predicate_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, predicate, PREDICATE);
}
int stdset_structure_predicate_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, predicate, PREDICATE);
}

int stdget_structure_access_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, access, ACCESS);
}
int stdset_structure_access_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, access, ACCESS);
}

int stdget_structure_copier_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, copier, COPIER);
}
int stdset_structure_copier_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, copier, COPIER);
}

int stdget_structure_constructor_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, constructor, CONSTRUCTOR);
}
int stdset_structure_constructor_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, constructor, CONSTRUCTOR);
}


/************************************************************
 *  structure_change.c
 ************************************************************/

/*
 *  instance
 */
static int structure_change1_instance_(struct defstruct *str)
{
	addr instance, change, list;

	/* swap */
	instance = str->instance;
	Return(structure_instance1_(str));
	change = str->instance;
	str->instance = instance;
	str->change = change;
	Check(! structure_class_p_debug(change), "type error");
	Check(! structure_class_p_debug(instance), "type error");

	/* swap */
	clos_swap(instance, change);

	/* precedence-list */
	Return(stdget_structure_precedence_list_(change, &list));
	Return(stdset_structure_precedence_list_(instance, list));

	return 0;
}

static void structure_change2_instance(struct defstruct *str)
{
	addr instance, change, list;

	/* swap */
	instance = str->instance;
	structure_instance2(str);
	change = str->instance;
	str->instance = instance;
	str->change = change;
	Check(! structure_object_p(change), "type error");
	Check(! structure_object_p(instance), "type error");

	/* swap */
	structure_swap(instance, change);

	/* precedence-list */
	GetPrecedenceStructure(change, &list);
	SetPrecedenceStructure(instance, list);
}

static void structure_change3_instance(struct defstruct *str)
{
	structure_change2_instance(str);
}


/*
 *  include
 */
static int structure_change1_include_(struct defstruct *str)
{
	addr x, y;

	Return(stdget_structure_include_(str->instance, &x));
	Return(stdget_structure_include_(str->change, &y));
	if (x == y)
		return 0;

	return fmte_(":INCLUDE value ~S must be a ~S.", x, y, NULL);
}

static int structure_change2_include_(struct defstruct *str)
{
	addr x, y;

	GetIncludeStructure(str->instance, &x);
	GetIncludeStructure(str->change, &y);
	if (x == y)
		return 0;

	return fmte_(":INCLUDE value ~S must be a ~S.", x, y, NULL);
}

static int structure_change3_include_(struct defstruct *str)
{
	return structure_change2_include_(str);
}


/*
 *  slots
 */
static int structure_change_slots_(addr instance, addr slots1, addr slots2)
{
	int check;
	addr x, y, list;
	size_t size1, size2, i;

	/* length */
	LenSlotVector(slots1, &size1);
	LenSlotVector(slots2, &size2);
	if (size1 != size2)
		goto error;

	/* slots */
	for (i = 0; i < size1; i++) {
		GetSlotVector(slots1, i, &x);
		GetSlotVector(slots2, i, &y);
		GetNameSlot(x, &x);
		GetNameSlot(y, &y);
		GetNameSymbol(x, &x);
		GetNameSymbol(y, &y);
		Return(string_equal_(x, y, &check));
		if (! check)
			goto error;
	}
	return 0;

error:
	list = Nil;
	for (i = 0; i < size1; i++) {
		GetSlotVector(slots1, i, &x);
		GetNameSlot(x, &x);
		cons_heap(&list, x, list);
	}
	return fmte_("Cannot change slots ~S in ~S.", list, instance, NULL);
}

static int structure_change1_slots_(struct defstruct *str)
{
	addr slots1, slots2;

	Return(structure_define1_slots_(str));
	Return(stdget_structure_direct_slots_(str->instance, &slots1));
	Return(stdget_structure_direct_slots_(str->change, &slots2));

	return structure_change_slots_(str->instance, slots1, slots2);
}

static int structure_change2_slots_(struct defstruct *str)
{
	addr slots1, slots2;

	Return(structure_define2_slots_(str));
	GetSlotsStructure(str->instance, &slots1);
	GetSlotsStructure(str->change, &slots2);

	return structure_change_slots_(str->instance, slots1, slots2);
}

static int structure_change3_slots_(struct defstruct *str)
{
	addr slots1, slots2;

	Return(structure_define3_slots_(str));
	GetSlotsStructure(str->instance, &slots1);
	GetSlotsStructure(str->change, &slots2);

	return structure_change_slots_(str->instance, slots1, slots2);
}


/*
 *  call
 */
static int structure_change1_call_(struct defstruct *str)
{
	Return(structure_delete1_call_(str->change));
	return structure_define1_call_(str);
}

static int structure_change2_call_(struct defstruct *str)
{
	Return(structure_delete2_call_(str->change));
	return structure_define2_call_(str);
}

static int structure_change3_call_(struct defstruct *str)
{
	Return(structure_delete3_call_(str->change));
	return structure_define3_call_(str);
}


/*
 *  copier
 */
static int structure_change1_copier_(struct defstruct *str)
{
	Return(structure_delete1_copier_(str->change));
	return structure_define1_copier_(str);
}

static int structure_change2_copier_(struct defstruct *str)
{
	Return(structure_delete2_copier_(str->change));
	return structure_define2_copier_(str);
}

static int structure_change3_copier_(struct defstruct *str)
{
	Return(structure_delete3_copier_(str->change));
	return structure_define3_copier_(str);
}


/*
 *  predicate
 */
static int structure_change1_predicate_(struct defstruct *str)
{
	Return(structure_delete1_predicate_(str->change));
	return structure_define1_predicate_(str);
}

static int structure_change2_predicate_(struct defstruct *str)
{
	Return(structure_delete2_predicate_(str->change));
	return structure_define2_predicate_(str);
}

static int structure_change3_predicate_(struct defstruct *str)
{
	Return(structure_delete3_predicate_(str->change));
	return structure_define3_predicate_(str);
}


/*
 *  constructor
 */
static int structure_change1_constructor_(struct defstruct *str)
{
	Return(structure_delete1_constructor_(str->change));
	return structure_define1_constructor_(str);
}

static int structure_change2_constructor_(struct defstruct *str)
{
	Return(structure_delete2_constructor_(str->change));
	return structure_define2_constructor_(str);
}

static int structure_change3_constructor_(struct defstruct *str)
{
	Return(structure_delete3_constructor_(str->change));
	return structure_define3_constructor_(str);
}


/*
 *  print-object
 */
static int structure_change1_print_(struct defstruct *str)
{
	Return(structure_delete1_print_(str->ptr, str->instance));
	return structure_define1_print_(str);
}

static int structure_change2_print_(struct defstruct *str)
{
	return structure_define2_print_(str);
}

static int structure_change3_print_(struct defstruct *str)
{
	return structure_define3_print_(str);
}


/*
 *  change
 */
static int structure_change1_execute_(struct defstruct *str)
{
	Return(structure_change1_instance_(str));
	Return(structure_change1_include_(str));
	Return(structure_change1_slots_(str));
	Return(structure_change1_call_(str));
	Return(structure_change1_copier_(str));
	Return(structure_change1_predicate_(str));
	Return(structure_change1_constructor_(str));
	Return(structure_change1_print_(str));

	return 0;
}

static int structure_change2_execute_(struct defstruct *str)
{
	structure_change2_instance(str);
	Return(structure_change2_include_(str));
	Return(structure_change2_slots_(str));
	Return(structure_change2_call_(str));
	Return(structure_change2_copier_(str));
	Return(structure_change2_predicate_(str));
	Return(structure_change2_constructor_(str));
	Return(structure_change2_print_(str));

	return 0;
}

static int structure_change3_execute_(struct defstruct *str)
{
	structure_change3_instance(str);
	Return(structure_change3_include_(str));
	Return(structure_change3_slots_(str));
	Return(structure_change3_call_(str));
	Return(structure_change3_copier_(str));
	Return(structure_change3_predicate_(str));
	Return(structure_change3_constructor_(str));
	Return(structure_change3_print_(str));

	return 0;
}

int structure_change1_(struct defstruct *str)
{
	Execute ptr;
	addr control;

	ptr = str->ptr;
	push_control(ptr, &control);
	(void)structure_change1_execute_(str);
	return pop_control_(ptr, control);
}

int structure_change2_(struct defstruct *str)
{
	Execute ptr;
	addr control;

	ptr = str->ptr;
	push_control(ptr, &control);
	(void)structure_change2_execute_(str);
	return pop_control_(ptr, control);
}

int structure_change3_(struct defstruct *str)
{
	Execute ptr;
	addr control;

	ptr = str->ptr;
	push_control(ptr, &control);
	(void)structure_change3_execute_(str);
	return pop_control_(ptr, control);
}


/************************************************************
 *  structure_define.c
 ************************************************************/

/*
 *  ensure-structure
 */
static int ensure_structure_class_p(struct defstruct *str)
{
	addr pos;

	if (! structure_get_class(str->name, &pos))
		return 0;
	str->instance = pos;
	return 1;
}

static int ensure_structure_object_p(struct defstruct *str)
{
	addr pos;

	if (! structure_get_object(str->name, &pos))
		return 0;
	str->instance = pos;
	return 1;
}

static int ensure_structure_call_(struct defstruct *str)
{
	/* class */
	if (str->type_p == 0) {
		if (ensure_structure_class_p(str))
			return structure_change1_(str);
		else
			return structure_define1_(str);
	}

	/* (:type list) */
	if (str->type_list_p) {
		if (ensure_structure_object_p(str))
			return structure_change2_(str);
		else
			return structure_define2_(str);
	}

	/* (:type vector) */
	if (str->type_vector_p) {
		if (ensure_structure_object_p(str))
			return structure_change3_(str);
		else
			return structure_define3_(str);
	}

	/* error */
	return fmte_("Invalid structure type.", NULL);
}

int ensure_structure_common_(Execute ptr, addr name, addr slots, addr args)
{
	struct defstruct str;
	LocalHold hold;

	Check(! symbolp(name), "type error");
	Check(! listp(slots), "type error");
	Return(ensure_structure_struct_(&str, ptr, name, slots, args));

	hold = LocalHold_array(ptr, 2);
	localhold_defstruct(&str, hold);
	Return(structure_arguments_(&str, hold));
	Return(ensure_structure_call_(&str));
	localhold_end(hold);

	return 0;
}


/*
 *  initialize
 */
void init_structure_define(void)
{
	init_structure_define1();
	init_structure_define2();
	init_structure_define3();
}


/************************************************************
 *  structure_define1.c
 ************************************************************/

/*
 *  make-instance
 */
static int structure_instance1_include_(struct defstruct *str, addr instance)
{
	addr list, pos;

	/* include */
	if (str->include_p) {
		Return(stdset_structure_include_(instance, str->iname));
	}

	/* precedence-list */
	list = Nil;
	for (pos = instance; pos != Nil; ) {
		cons_heap(&list, pos, list);
		Return(stdget_structure_include_(pos, &pos));
	}
	GetConst(CLOS_STRUCTURE_OBJECT, &pos);
	cons_heap(&list, pos, list);
	GetConst(CLOS_T, &pos);
	cons_heap(&list, pos, list);
	nreverse(&list, list);
	Return(stdset_structure_precedence_list_(instance, list));

	return 0;
}

int structure_instance1_(struct defstruct *str)
{
	addr clos, instance;

	/* structure */
	GetConst(CLOS_STRUCTURE_CLASS, &clos);
	Return(clos_instance_heap_(clos, &instance));
	SetClassOfClos(instance, clos);
	/* name */
	Return(stdset_structure_name_(instance, str->name));
	/* documentation */
	if (str->doc != Nil) {
		Return(stdset_structure_documentation_(instance, str->doc));
	}
	/* include, precedence-list */
	Return(structure_instance1_include_(str, instance));
	/* check */
	Check(str->type_p, "type error");
	Check(str->type_list_p, "type error");
	Check(str->type_vector_p, "type error");
	Check(str->named_p, "named error");
	/* result */
	str->instance = instance;

	return 0;
}


/*
 *  slots
 */
static int structure_define1_slots_pushnew_(LocalRoot local,
		addr *push, addr value, addr list, int *ret)
{
	int check;
	addr root, x;

	Check(! stringp(value), "type error");
	for (root = list; list != Nil; ) {
		Return_getcons(list, &x, &list);
		Return(string_equal_(x, value, &check));
		if (check)
			return Result(ret, 0);
	}
	cons_local(local, push, value, root);

	return Result(ret, 1);
}

static int structure_define1_slots_find_(addr name, addr list, addr *ret)
{
	int check;
	addr pos, value;

	Check(! stringp(name), "type error");
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		GetNameSlot(pos, &value);
		GetNameSymbol(value, &value);
		Return(string_equal_(name, value, &check));
		if (check)
			return Result(ret, pos);
	}

	return 0;
}

static int structure_define1_slots_get_(struct defstruct *str, size_t *ret)
{
	addr pos;
	size_t value;

	*ret = 0;
	value = 0;
	if (str->include_p) {
		pos = str->iname;
		Return(stdget_structure_value_(pos, &pos));
		if (pos != Nil) {
			Return(getindex_integer_(pos, &value));
		}
	}
	value += str->offset;

	return Result(ret, value);
}

static int structure_define1_slots_set_(struct defstruct *str, size_t value)
{
	addr pos, instance;

	/* value */
	instance = str->instance;
	str->size_all = value;
	make_index_integer_heap(&pos, value);
	Return(stdset_structure_value_(instance, pos));

	return 0;
}

static int structure_define1_slots_include_(struct defstruct *str,
		addr *slots, addr *root, size_t *ret)
{
	LocalRoot local;
	addr list, pos, name, args;
	size_t size, i;

	local = str->ptr->local;
	*root = *slots = Nil;
	*ret = 0;
	size = 0;
	if (str->include_p) {
		pos = str->iname;
		Return(stdget_structure_slots_(pos, &list));
		args = str->iargs;
		LenSlotVector(list, &size);
		for (i = 0; i < size; i++) {
			GetSlotVector(list, i, &pos);
			slot_copy_heap(&pos, pos);
			GetNameSlot(pos, &name);
			GetNameSymbol(name, &name);
			cons_local(local, root, name, *root);
			Return(structure_define1_slots_find_(name, args, &pos));
			Check(! slotp(pos), "type error");
			cons_local(local, slots, pos, *slots);
			/* location */
			SetLocationSlot(pos, i);
		}
	}

	return Result(ret, size);
}

static int structure_define1_slots_slots_(struct defstruct *str,
		addr *slots, addr *root, addr *rdirect, size_t *ret)
{
	int check;
	LocalRoot local;
	addr list, pos, name, direct;
	size_t value, size, size_direct;

	local = str->ptr->local;
	Return(structure_define1_slots_get_(str, &value));

	/* size */
	*ret = 0;
	size = 0;
	if (str->include_p) {
		Return(stdget_structure_slots_(str->iname, &pos));
		LenSlotVector(pos, &size);
	}

	/* loop */
	direct = Nil;
	size_direct = 0;
	list = str->slots;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		GetNameSlot(pos, &name);
		GetNameSymbol(name, &name);
		Return(structure_define1_slots_pushnew_(local, root, name, *root, &check));
		if (check) {
			/* slots */
			cons_local(local, slots, pos, *slots);
			SetLocationSlot(pos, size++);
			SetAccessSlot(pos, value++);
			/* direct-slots */
			cons_local(local, &direct, pos, direct);
			size_direct++;
		}
	}

	/* result */
	Return(structure_define1_slots_set_(str, value));
	*rdirect = direct;
	return Result(ret, size_direct);
}

static int structure_define1_slots_array_(struct defstruct *str, addr slots, size_t size)
{
	addr instance, array, pos;
	size_t i;

	instance = str->instance;
	slot_vector_heap(&array, size);
	while (slots != Nil) {
		Return_getcons(slots, &pos, &slots);
		GetLocationSlot(pos, &i);
		SetClassSlot(pos, instance);
		SetSlotVector(array, i, pos);
	}
	str->slots = array;
	Return(stdset_structure_slots_(instance, array));

	return 0;
}

static int structure_define1_slots_direct_(struct defstruct *str, addr list, size_t size)
{
	addr instance, array, pos;
	size_t i;

	/* direct-slots array */
	instance = str->instance;
	slot_vector_heap(&array, size);
	nreverse(&list, list);
	for (i = 0; list != Nil; i++) {
		Return_getcons(list, &pos, &list);
		SetClassSlot(pos, instance);
		SetSlotVector(array, i, pos);
	}
	Return(stdset_structure_direct_slots_(instance, array));

	return 0;
}

static int structure_define1_slots_call_(struct defstruct *str)
{
	addr slots, root, direct;
	size_t size, size_direct;

	/* make slots */
	Return(structure_define1_slots_include_(str, &slots, &root, &size));
	Return(structure_define1_slots_slots_(str, &slots, &root, &direct, &size_direct));
	size += size_direct;
	str->size = size;

	/* set slots */
	Return(structure_define1_slots_array_(str, slots, size));
	Return(structure_define1_slots_direct_(str, direct, size_direct));

	return 0;
}

int structure_define1_slots_(struct defstruct *str)
{
	LocalRoot local;
	LocalStack stack;

	local = str->ptr->local;
	push_local(local, &stack);
	Return(structure_define1_slots_call_(str));
	rollback_local(local, stack);

	return 0;
}


/*
 *  accessor
 */
/* reader1 */
static int function_structure_reader1(Execute ptr, addr var)
{
	int check;
	addr slot, pos;
	size_t index;

	/* closure */
	getdata_control(ptr, &slot);
	/* check-type */
	GetClassSlot(slot, &pos);
	Return(typep_structure_(var, pos, &check));
	if (! check)
		return fmte_("The structure ~S must be a ~S type.", pos, var, NULL);
	/* result */
	GetLocationSlot(slot, &index);
	GetValueClos(var, &var);
	GetClosValue(var, index, &var);
	setresult_control(ptr, var);

	return 0;
}

static void type_structure_reader1(addr *ret, addr instance)
{
	addr args, values;

	type_clos_heap(instance, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_reader1(addr instance, addr slot, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_reader1);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, slot);
	/* type */
	type_structure_reader1(&type, instance);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* writer1 */
static int function_structure_writer1(Execute ptr, addr value, addr var)
{
	int check;
	addr slot, pos;

	/* closure */
	getdata_control(ptr, &slot);
	/* check-type */
	GetClassSlot(slot, &pos);
	Return(typep_structure_(var, pos, &check));
	if (! check)
		return fmte_("The structure ~S must be a ~S type.", pos, var, NULL);
	/* result */
	Return(structure_write1_(ptr, var, slot, value));
	setresult_control(ptr, value);

	return 0;
}

static void type_structure_writer1(addr *ret, addr instance)
{
	addr args, values;

	GetTypeTable(&args, T);
	type_clos_heap(instance, &values);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_writer1(addr instance, addr slot, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_setf_system(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var2(pos, p_defun_structure_writer1);
	setsetf_symbol(symbol, pos);
	SetDataFunction(pos, slot);
	/* type */
	type_structure_writer1(&type, instance);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/*
 *  call
 */
static int structure_define1_callname_(struct defstruct *str, addr *ret, addr pos)
{
	addr name;

	Check(! slotp(pos), "type error");
	GetNameSlot(pos, &pos);
	Check(! symbolp(pos), "type error");
	GetNameSymbol(pos, &pos);
	if (str->conc_name == Unbound) {
		Return(stdget_structure_name_(str->instance, &name));
		GetNameSymbol(name, &name);
		Return(string_concat_hyphen_heap_(ret, name, pos));
	}
	else if (str->conc_name == Nil) {
		*ret = pos;
	}
	else {
		Check(! stringp(str->conc_name), "type error");
		Return(string_concat_heap_(ret, str->conc_name, pos));
	}

	return 0;
}

static int structure_define1_intern_(struct defstruct *str,
		addr package, addr pos, addr *ret)
{
	addr symbol, call, instance, cons;

	/* callname */
	Return(structure_define1_callname_(str, &symbol, pos));
	Return(intern_package_(package, symbol, &symbol, NULL));
	Return(parse_callname_error_(&call, symbol));

	/* push access */
	GetNameSlot(pos, &pos);
	cons_heap(&pos, pos, symbol);
	instance = str->instance;
	Return(stdget_structure_access_(instance, &cons));
	cons_heap(&cons, pos, cons);
	Return(stdset_structure_access_(instance, cons));

	/* result */
	return Result(ret, call);
}

int structure_define1_call_(struct defstruct *str)
{
	addr instance, package, slots, pos, call, readonly;
	size_t size, i;

	instance = str->instance;
	Check(! structure_class_p_debug(instance), "type error");
	Return(getpackage_(str->ptr, &package));
	slots = str->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(structure_define1_intern_(str, package, pos, &call));

		defun_structure_reader1(instance, pos, call);
		GetReadOnlySlot(pos, &readonly);
		if (readonly == Nil)
			defun_structure_writer1(instance, pos, call);
	}

	return 0;
}


/*
 *  constructor
 */
static int function_structure_constructor1(Execute ptr, addr args)
{
	addr pos;

	getdata_control(ptr, &pos);
	SetErrorpStructureType(pos, 1);
	Return(make_structure1_(ptr, &pos, pos, args, 1));
	setresult_control(ptr, pos);

	return 0;
}

static void type_structure_constructor1(addr *ret, addr data)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetInstanceStructureType(data, &data);
	type_clos_heap(data, &values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_constructor1(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_dynamic(pos, p_defun_structure_constructor1);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	type_structure_constructor1(&type, data);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* default */
static int structure_constructor1_push_(struct defstruct *str, addr symbol)
{
	addr instance, list;

	Check(! symbolp(symbol), "type error");
	instance = str->instance;
	Return(stdget_structure_constructor_(instance, &list));
	cons_heap(&list, symbol, list);
	Return(stdset_structure_constructor_(instance, list));

	return 0;
}

static int structure_constructor1_default_(struct defstruct *str, addr symbol)
{
	addr pos, call;

	Return(parse_callname_error_(&call, symbol));
	Return(structure_constructor1_push_(str, symbol));
	structure_type(str, str->slots, &pos);
	if (str->type_p)
		return fmte_("Invalid structure type.", NULL);
	defun_structure_constructor1(pos, call);

	return 0;
}

static int structure_constructor1_make_(struct defstruct *str)
{
	addr name;

	/* name */
	Return(stdget_structure_name_(str->instance, &name));
	GetNameSymbol(name, &name);
	Return(string_concat_char1_heap_(&name, "MAKE-", name));
	Return(intern_default_package_(str->ptr, name, &name, NULL));
	/* make */
	return structure_constructor1_default_(str, name);
}

static int structure_constructor1_lambda_(struct defstruct *str, addr list)
{
	addr symbol, name, pos, type;

	/* (symbol function) */
	Return(list_bind_(list, &symbol, &pos, NULL));
	Return(parse_callname_error_(&name, symbol));
	Return(structure_constructor1_push_(str, symbol));
	SetNameFunction(pos, name);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeTable(&type, Function);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);

	return 0;
}

int structure_define1_constructor_(struct defstruct *str)
{
	addr list, pos, g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	for (list = str->constructor; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		if (pos == g) {
			Return(structure_constructor1_make_(str));
		}
		else if (symbolp(pos)) {
			Return(structure_constructor1_default_(str, pos));
		}
		else if (consp(pos)) {
			Return(structure_constructor1_lambda_(str, pos));
		}
		else {
			return fmte_("Invalid constructor parameter ~S.", pos, NULL);
		}
	}

	return 0;
}


/*
 *  copier
 */
static int function_structure_copier1(Execute ptr, addr var)
{
	copy_structure_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_structure_copier1(addr *ret, addr instance)
{
	addr args, values;

	type_clos_heap(instance, &values);
	typeargs_var1(&args, values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_copier1(addr instance, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_copier1);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_structure_copier1(&type, instance);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static int structure_define1_copier_callname_(struct defstruct *str, addr *ret)
{
	addr name;

	if (! str->copier_p) {
		Return(stdget_structure_name_(str->instance, &name));
		GetNameSymbol(name, &name);
		Return(string_concat_char1_heap_(&name, "COPY-", name));
	}
	else if (str->copier == Nil) {
		return Result(ret, Nil);
	}
	else {
		Check(! stringp(str->copier), "type error");
		name = str->copier;
	}

	return intern_default_package_(str->ptr, name, ret, NULL);
}

int structure_define1_copier_(struct defstruct *str)
{
	addr symbol;

	Return(structure_define1_copier_callname_(str, &symbol));
	Return(stdset_structure_copier_(str->instance, symbol));
	if (symbol == Nil)
		return 0;

	Return(parse_callname_error_(&symbol, symbol));
	if (str->type_p)
		return fmte_("Invalid structure type.", NULL);
	defun_structure_copier1(str->instance, symbol);

	return 0;
}


/*
 *  predicate
 */
static int function_structure_predicate1(Execute ptr, addr var)
{
	int check;
	addr instance;

	getdata_control(ptr, &instance);
	Return(typep_structure_(var, instance, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_structure_predicate1(addr instance, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_predicate1);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, instance);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static int structure_define1_predicate_callname_(struct defstruct *str, addr *ret)
{
	addr name;

	if (! str->predicate_p) {
		Return(stdget_structure_name_(str->instance, &name));
		GetNameSymbol(name, &name);
		Return(string_concat_char2_heap_(&name, name, "-P"));
	}
	else if (str->predicate == Nil) {
		return Result(ret, Nil);
	}
	else {
		Check(! stringp(str->predicate), "type error");
		name = str->predicate;
	}

	return intern_default_package_(str->ptr, name, ret, NULL);
}

int structure_define1_predicate_(struct defstruct *str)
{
	addr instance, symbol;

	instance = str->instance;
	Return(structure_define1_predicate_callname_(str, &symbol));
	Return(stdset_structure_predicate_(instance, symbol));
	if (symbol == Nil)
		return 0;

	Return(parse_callname_error_(&symbol, symbol));
	if (str->type_p)
		return fmte_("Invalid structure type.", NULL);
	defun_structure_predicate1(instance, symbol);

	return 0;
}


/*
 *  printer
 */
/* default */
static int method_defstruct_default(Execute ptr,
		addr method, addr next, addr var, addr stream)
{
	int check;

	Return(structure_instance_p_(var, &check));
	if (! check)
		return fmte_("Invalid structure type ~S.", var, NULL);
	Return(print_structure_(ptr, stream, var));
	setresult_control(ptr, var);

	return 0;
}

static int structure_print_default_method_(struct defstruct *str, addr name, addr *ret)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	GetCallName(name, &name);
	setcompiled_var4(call, p_method_defstruct_default);
	GetTypeCompiled(&type, PrintObject_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_print_object(&pos, str->instance);
	Return(method_instance_lambda_(str->ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));

	return Result(ret, pos);
}

static int structure_print_add_method_(struct defstruct *str, addr name, addr method)
{
	addr generic;
	Execute ptr;

	ptr = str->ptr;
	Return(getglobalcheck_callname_(name, &generic));
	Check(! clos_generic_p_debug(generic), "type error");
	return method_add_method_(ptr, generic, method);
}

static int structure_print_default_(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	Return(parse_callname_error_(&name, name));
	Return(structure_print_default_method_(str, name, &method));
	return structure_print_add_method_(str, name, method);
}


/* object */
static int method_defstruct_object(Execute ptr,
		addr method, addr next, addr var, addr stream)
{
	int check;
	addr call;

	Return(structure_instance_p_(var, &check));
	if (! check)
		return fmte_("Invalid structure type ~S.", var, NULL);
	getdata_control(ptr, &call);
	Return(apply1_control_(ptr, &call, call, Nil));
	Return(funcall1_control_(ptr, &call, call, var, stream, NULL));
	setresult_control(ptr, var);

	return 0;
}

static int structure_print_object_method_(struct defstruct *str, addr name, addr *ret)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	GetCallName(name, &name);
	setcompiled_var4(call, p_method_defstruct_object);
	GetTypeCompiled(&type, PrintObject_Method);
	settype_function(call, type);
	SetDataFunction(call, str->print_object);
	/* method */
	mop_argument_method_print_object(&pos, str->instance);
	Return(method_instance_lambda_(str->ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));

	return Result(ret, pos);
}

static int structure_print_object_(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	Return(parse_callname_error_(&name, name));
	Return(structure_print_object_method_(str, name, &method));
	return structure_print_add_method_(str, name, method);
}


/* function */
static int method_defstruct_function(Execute ptr,
		addr method, addr next, addr var, addr stream)
{
	int check;
	addr call, pos;

	Return(structure_instance_p_(var, &check));
	if (! check)
		return fmte_("Invalid structure type ~S.", var, NULL);
	GetConst(SPECIAL_PRINT_LEVEL, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	getdata_control(ptr, &call);
	Return(apply1_control_(ptr, &call, call, Nil));
	Return(funcall1_control_(ptr, &call, call, var, stream, pos, NULL));
	setresult_control(ptr, var);

	return 0;
}

static int structure_print_function_method_(struct defstruct *str, addr name, addr *ret)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	GetCallName(name, &name);
	setcompiled_var4(call, p_method_defstruct_function);
	GetTypeCompiled(&type, PrintObject_Method);
	settype_function(call, type);
	SetDataFunction(call, str->print_function);
	/* method */
	mop_argument_method_print_object(&pos, str->instance);
	Return(method_instance_lambda_(str->ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));

	return Result(ret, pos);
}

static int structure_print_function_(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	Return(parse_callname_error_(&name, name));
	Return(structure_print_function_method_(str, name, &method));
	return structure_print_add_method_(str, name, method);
}


/* define */
static int structure_print_default_p(struct defstruct *str)
{
	addr g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	if (str->print_object_p && str->print_object == g)
		return 1;
	if (str->print_function_p && str->print_function == g)
		return 1;

	return 0;
}

int structure_define1_print_(struct defstruct *str)
{
	if (str->type_p && str->print_object_p)
		return fmte_("Can't make print-object on :TYPE structure.", NULL);
	if (str->type_p && str->print_function_p)
		return fmte_("Can't make print-function on :TYPE structure.", NULL);
	if (structure_print_default_p(str))
		return structure_print_default_(str);
	else if (str->print_object_p)
		return structure_print_object_(str);
	else if (str->print_function_p)
		return structure_print_function_(str);

	return 0;
}

int structure_define1_(struct defstruct *str)
{
	/* make instance */
	Return(structure_instance1_(str));
	Check(! structure_class_p_debug(str->instance), "type error");
	clos_define_class(str->name, str->instance);

	/* settings */
	Return(structure_define1_slots_(str));
	Return(structure_define1_call_(str));
	Return(structure_define1_copier_(str));
	Return(structure_define1_predicate_(str));
	Return(structure_define1_constructor_(str));
	Return(structure_define1_print_(str));

	return 0;
}


/*
 *  initialize
 */
void init_structure_define1(void)
{
	SetPointerCall(defun, var1, structure_reader1);
	SetPointerCall(defun, var2, structure_writer1);
	SetPointerCall(defun, dynamic, structure_constructor1);
	SetPointerCall(defun, var1, structure_copier1);
	SetPointerCall(defun, var1, structure_predicate1);
	SetPointerType(var4, method_defstruct_default);
	SetPointerType(var4, method_defstruct_object);
	SetPointerType(var4, method_defstruct_function);
}


/************************************************************
 *  structure_define2.c
 ************************************************************/

/*
 *  make-instance
 */
static void structure_instance2_include(struct defstruct *str, addr instance)
{
	addr list, pos;

	/* include */
	if (str->include_p) {
		SetIncludeStructure(instance, str->iname);
	}

	/* precedence-list */
	list = Nil;
	for (pos = instance; pos != Nil; ) {
		cons_heap(&list, pos, list);
		GetIncludeStructure(pos, &pos);
	}
	nreverse(&list, list);
	SetPrecedenceStructure(instance, list);
}

void structure_instance2(struct defstruct *str)
{
	addr instance;

	/* structure */
	structure_heap(&instance);
	/* name */
	SetNameStructure(instance, str->name);
	/* documentation */
	if (str->doc != Nil) {
		SetDocStructure(instance, str->doc);
	}
	/* include, precedence-list */
	structure_instance2_include(str, instance);
	/* type */
	Check(! str->type_p, "type error");
	if (str->type_list_p) {
		set_list_p_structure(instance, 1);
	}
	if (str->type_vector_p) {
		set_vector_p_structure(instance, 1);
		settype_structure(instance, str->type1, str->type2);
		SetSpecializedStructure(instance, str->type_vector);
	}
	/* named */
	set_named_p_structure(instance, str->named_p);
	/* result */
	str->instance = instance;
}


/*
 *  slots
 */
static int structure_define2_slots_exists_p_(addr include, addr name, int *ret)
{
	int check;
	addr slots, pos;
	size_t size, i;

	if (include == Nil)
		return Result(ret, 0);

	GetSlotsStructure(include, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		GetNameSlot(pos, &pos);
		GetNameSymbol(pos, &pos);
		Return(string_equal_(name, pos, &check));
		if (check)
			return Result(ret, 1);
	}

	GetIncludeStructure(include, &include);
	return structure_define2_slots_exists_p_(include, name, ret);
}

static int structure_define2_slots_include_(struct defstruct *str)
{
	int check;
	addr include, list, pos, name;

	if (! str->include_p)
		return 0;
	list = str->iargs;
	include = str->iname;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		GetNameSlot(pos, &name);
		GetNameSymbol(name, &name);
		Return(structure_define2_slots_exists_p_(include, name, &check));
		if (! check)
			return fmte_("The slot ~S is not included in structure.", pos, NULL);
	}

	return 0;
}

static void structure_define2_slots_named(addr name, size_t size_all, addr *ret)
{
	addr pos, symbol, type;

	slot_heap(&pos);
	GetConst(SYSTEM_STRUCTURE_NAMED, &symbol);
	SetNameSlot(pos, symbol);
	SetFormSlot(pos, name);
	SetLocationSlot(pos, size_all);
	SetAccessSlot(pos, size_all);
	GetTypeTable(&type, T);
	SetTypeSlot(pos, type);
	*ret = pos;
}

static int structure_define2_slots_copy_(addr pos, size_t size_all, addr *ret)
{
	slot_copy_heap(&pos, pos);
	SetLocationSlot(pos, size_all);
	SetAccessSlot(pos, size_all);
	return Result(ret, pos);
}

static int structure_define2_slots_direct_(struct defstruct *str)
{
	addr instance, direct, list, pos;
	size_t size, size_all, i;

	/* slot vector */
	list = str->slots;
	Return(length_list_safe_(list, &size));
	if (str->named_p)
		size++;
	slot_vector_heap(&direct, size);
	instance = str->instance;
	str->size = size;

	/* location */
	size_all = 0;
	if (str->include_p)
		size_all += get_size_all_structure(str->iname);
	size_all += str->offset;

	/* named */
	i = 0;
	if (str->named_p) {
		str->named_index = size_all;
		structure_define2_slots_named(str->name, size_all, &pos);
		SetSlotVector(direct, i++, pos);
		size_all++;
	}

	/* slots */
	while (i < size) {
		Return_getcons(list, &pos, &list);
		Return(structure_define2_slots_copy_(pos, size_all, &pos));
		SetSlotVector(direct, i++, pos);
		size_all++;
	}
	str->size_all = size_all;

	/* instance */
	SetDirectStructure(instance, direct);
	set_size_structure(instance, str->size);
	set_size_all_structure(instance, str->size_all);

	return 0;
}

static int structure_define2_slots_arguments_(addr list, addr name1, addr *ret)
{
	int check;
	addr pos, name2;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		GetNameSlot(pos, &name2);
		GetNameSymbol(name2, &name2);
		Return(string_equal_(name1, name2, &check));
		if (check)
			return Result(ret, pos);
	}

	return Result(ret, Unbound);
}

static int structure_define2_slots_update_(struct defstruct *str, addr pos, addr *ret)
{
	addr name, make;
	size_t value;

	/* include arguments */
	if (str->include_p) {
		GetNameSlot(pos, &name);
		GetNameSymbol(name, &name);
		Return(structure_define2_slots_arguments_(str->iargs, name, &make));
		if (make != Unbound)
			pos = make;
	}

	/* copy */
	slot_copy_heap(&make, pos);
	GetLocationSlot(make, &value);
	SetLocationSlot(pos, value);
	GetAccessSlot(make, &value);
	SetAccessSlot(pos, value);

	return Result(ret, make);
}

static int structure_define2_slots_effective_(struct defstruct *str)
{
	addr instance, direct, super, slots, pos;
	size_t size1, size2, i, k;

	/* no include */
	instance = str->instance;
	GetDirectStructure(instance, &direct);
	if (! str->include_p)
		goto no_include;

	/* include */
	GetSlotsStructure(str->iname, &super);
	LenSlotVector(super, &size1);
	if (size1 == 0)
		goto no_include;

	/* copy */
	size2 = str->size;
	slot_vector_heap(&slots, size1 + size2);
	for (i = 0; i < size1; i++) {
		GetSlotVector(super, i, &pos);
		Return(structure_define2_slots_update_(str, pos, &pos));
		SetSlotVector(slots, i, pos);
	}
	for (k = 0; k < size2; k++, i++) {
		GetSlotVector(direct, k, &pos);
		slot_copy_heap(&pos, pos);
		SetSlotVector(slots, i, pos);
	}
	SetSlotsStructure(instance, slots);
	str->slots = slots;
	return 0;

no_include:
	SetSlotsStructure(instance, direct);
	str->slots = direct;
	return 0;
}

int structure_define2_slots_(struct defstruct *str)
{
	Return(structure_define2_slots_include_(str));
	Return(structure_define2_slots_direct_(str));
	Return(structure_define2_slots_effective_(str));

	return 0;
}

int structure_define3_slots_(struct defstruct *str)
{
	return structure_define2_slots_(str);
}


/* reader2 */
static int function_structure_reader2(Execute ptr, addr var)
{
	int check;
	addr data, slot;
	size_t index;

	/* closure */
	getdata_control(ptr, &data);
	CheckType(data, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_list_p(data, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-list.", var, NULL);
	/* access */
	GetSlotStructureType(data, &slot);
	GetAccessSlot(slot, &index);
	Return(getnth_(var, index, &var));
	setresult_control(ptr, var);

	return 0;
}

static void type_structure_reader2(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_reader2(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_reader2);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	type_structure_reader2(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* reader3 */
static int function_structure_reader3(Execute ptr, addr var)
{
	int check;
	addr data, slot, type;

	/* closure */
	getdata_control(ptr, &data);
	CheckType(data, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_vector_p(ptr, data, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-vector.", var, NULL);
	/* access */
	GetSlotStructureType(data, &slot);
	GetVectorStructureType(data, &type);
	Return(structure_getarray_(var, slot, &var));
	setresult_control(ptr, var);

	return 0;
}

static void type_structure_reader3(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Vector);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_reader3(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_reader3);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	type_structure_reader3(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* writer2 */
static int function_structure_writer2(Execute ptr, addr value, addr var)
{
	int check;
	addr data, slot;

	/* closure */
	getdata_control(ptr, &data);
	CheckType(data, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_list_p(data, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-list.", var, NULL);
	/* access */
	GetSlotStructureType(data, &slot);
	Return(structure_write2_(ptr, var, slot, value));
	setresult_control(ptr, value);

	return 0;
}

static void type_structure_writer2(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_writer2(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_setf_system(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var2(pos, p_defun_structure_writer2);
	setsetf_symbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	type_structure_writer2(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}

/* writer3 */
static int function_structure_writer3(Execute ptr, addr value, addr var)
{
	int check;
	addr data, slot, vector_type;

	/* closure */
	getdata_control(ptr, &data);
	CheckType(data, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_vector_p(ptr, data, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-vector.", var, NULL);
	/* access */
	GetSlotStructureType(data, &slot);
	GetVectorStructureType(data, &vector_type);
	Return(structure_write3_(ptr, var, slot, vector_type, value));
	setresult_control(ptr, value);

	return 0;
}

static void type_structure_writer3(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Vector);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_writer3(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_setf_system(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var2(pos, p_defun_structure_writer3);
	setsetf_symbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	type_structure_writer3(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/*
 *  call
 */
static int structure_define2_callname_(struct defstruct *str, addr *ret, addr pos)
{
	addr name;

	Check(! slotp(pos), "type error");
	GetNameSlot(pos, &pos);
	Check(! symbolp(pos), "type error");
	GetNameSymbol(pos, &pos);
	if (str->conc_name == Unbound) {
		GetNameStructure(str->instance, &name);
		GetNameSymbol(name, &name);
		Return(string_concat_hyphen_heap_(ret, name, pos));
	}
	else if (str->conc_name == Nil) {
		*ret = pos;
	}
	else {
		Check(! stringp(str->conc_name), "type error");
		Return(string_concat_heap_(ret, str->conc_name, pos));
	}

	return 0;
}

static int structure_define2_intern_(struct defstruct *str,
		addr package, addr pos, addr *ret)
{
	addr symbol, call, instance, cons;

	/* callname */
	Return(structure_define2_callname_(str, &symbol, pos));
	Return(intern_package_(package, symbol, &symbol, NULL));
	Return(parse_callname_error_(&call, symbol));

	/* push access */
	GetNameSlot(pos, &pos);
	cons_heap(&pos, pos, symbol);
	instance = str->instance;
	GetAccessStructure(instance, &cons);
	cons_heap(&cons, pos, cons);
	SetAccessStructure(instance, cons);

	/* result */
	return Result(ret, call);
}

int structure_define2_call_(struct defstruct *str)
{
	addr package, type, slots, pos, call, readonly;
	size_t size, i;

	Return(getpackage_(str->ptr, &package));
	slots = str->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(structure_define2_intern_(str, package, pos, &call));

		structure_type(str, pos, &type);
		defun_structure_reader2(type, call);
		GetReadOnlySlot(pos, &readonly);
		if (readonly == Nil)
			defun_structure_writer2(type, call);
	}

	return 0;
}

int structure_define3_call_(struct defstruct *str)
{
	addr package, type, slots, pos, call, readonly;
	size_t size, i;

	Return(getpackage_(str->ptr, &package));
	slots = str->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(structure_define2_intern_(str, package, pos, &call));

		structure_type(str, pos, &type);
		defun_structure_reader3(type, call);
		GetReadOnlySlot(pos, &readonly);
		if (readonly == Nil)
			defun_structure_writer3(type, call);
	}

	return 0;
}


/*
 *  constructor
 */
/* list */
static int function_structure_constructor2(Execute ptr, addr args)
{
	addr pos;

	getdata_control(ptr, &pos);
	SetErrorpStructureType(pos, 1);
	Return(make_structure2_(ptr, &pos, pos, args, 1));
	setresult_control(ptr, pos);

	return 0;
}

static void type_structure_constructor2(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_constructor2(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_dynamic(pos, p_defun_structure_constructor2);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	type_structure_constructor2(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* vector */
static int function_structure_constructor3(Execute ptr, addr args)
{
	addr pos;

	getdata_control(ptr, &pos);
	SetErrorpStructureType(pos, 1);
	Return(make_structure3_(ptr, &pos, pos, args, 1));
	setresult_control(ptr, pos);

	return 0;
}

static void type_structure_constructor3(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Vector);
	typeargs_rest(&args, args);
	GetTypeValues(&values, Vector);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_constructor3(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_dynamic(pos, p_defun_structure_constructor3);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	type_structure_constructor3(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* default */
static int structure_constructor2_push_(struct defstruct *str, addr symbol)
{
	addr instance, list;

	Check(! symbolp(symbol), "type error");
	instance = str->instance;
	GetConstructorStructure(instance, &list);
	cons_heap(&list, symbol, list);
	SetConstructorStructure(instance, list);

	return 0;
}

static int structure_constructor2_default_(struct defstruct *str, addr symbol)
{
	addr pos, call;

	Return(parse_callname_error_(&call, symbol));
	Return(structure_constructor2_push_(str, symbol));
	structure_type(str, str->slots, &pos);
	if (str->type_list_p)
		defun_structure_constructor2(pos, call);
	else if (str->type_vector_p)
		defun_structure_constructor3(pos, call);
	else
		return fmte_("Invalid structure type.", NULL);

	return 0;
}

static int structure_constructor2_make_(struct defstruct *str)
{
	addr name;

	/* name */
	GetNameStructure(str->instance, &name);
	GetNameSymbol(name, &name);
	Return(string_concat_char1_heap_(&name, "MAKE-", name));
	Return(intern_default_package_(str->ptr, name, &name, NULL));
	/* make */
	return structure_constructor2_default_(str, name);
}

static int structure_constructor2_lambda_(struct defstruct *str, addr list)
{
	addr symbol, name, pos, type;

	/* (symbol function) */
	Return(list_bind_(list, &symbol, &pos, NULL));
	Return(parse_callname_error_(&name, symbol));
	Return(structure_constructor2_push_(str, symbol));
	SetNameFunction(pos, name);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeTable(&type, Function);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);

	return 0;
}

int structure_define2_constructor_(struct defstruct *str)
{
	addr list, pos, g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	for (list = str->constructor; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		if (pos == g) {
			Return(structure_constructor2_make_(str));
		}
		else if (symbolp(pos)) {
			Return(structure_constructor2_default_(str, pos));
		}
		else if (consp(pos)) {
			Return(structure_constructor2_lambda_(str, pos));
		}
		else {
			return fmte_("Invalid constructor parameter ~S.", pos, NULL);
		}
	}

	return 0;
}

int structure_define3_constructor_(struct defstruct *str)
{
	return structure_define2_constructor_(str);
}


/*
 *  copier
 */
/* list */
static int function_structure_copier2(Execute ptr, addr var)
{
	int check;
	addr type;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_list_p(type, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-list.", var, NULL);
	/* copy */
	copy_list_heap_safe(&var, var);
	setresult_control(ptr, var);

	return 0;
}

static void type_structure_copier2(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_copier2(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_copier2);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	type_structure_copier2(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* vector */
static int function_structure_copier3(Execute ptr, addr var)
{
	int check;
	addr type;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_vector_p(ptr, type, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-vector.", var, NULL);
	/* copy */
	Return(vector_copy_heap_(&var, var));
	setresult_control(ptr, var);

	return 0;
}

static void type_structure_copier3(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Vector);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Vector);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_copier3(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_copier3);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	type_structure_copier3(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* define */
static int structure_define2_copier_callname_(struct defstruct *str, addr *ret)
{
	addr name;

	if (! str->copier_p) {
		GetNameStructure(str->instance, &name);
		GetNameSymbol(name, &name);
		Return(string_concat_char1_heap_(&name, "COPY-", name));
	}
	else if (str->copier == Nil) {
		return Result(ret, Nil);
	}
	else {
		Check(! stringp(str->copier), "type error");
		name = str->copier;
	}

	return intern_default_package_(str->ptr, name, ret, NULL);
}

int structure_define2_copier_(struct defstruct *str)
{
	addr symbol;

	Return(structure_define2_copier_callname_(str, &symbol));
	SetCopierStructure(str->instance, symbol);
	if (symbol == Nil)
		return 0;

	Return(parse_callname_error_(&symbol, symbol));
	if (str->type_list_p)
		defun_structure_copier2(str, symbol);
	else if (str->type_vector_p)
		defun_structure_copier3(str, symbol);
	else
		return fmte_("Invalid structure type.", NULL);

	return 0;
}

int structure_define3_copier_(struct defstruct *str)
{
	return structure_define2_copier_(str);
}


/*
 *  predicate
 */
/* list */
static int function_structure_predicate2(Execute ptr, addr var)
{
	int check;
	addr type;

	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	Return(structure_type_list_p(type, var, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_structure_predicate2(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_predicate2);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* vector */
static int function_structure_predicate3(Execute ptr, addr var)
{
	int check;
	addr type;

	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	Return(structure_type_vector_p(ptr, type, var, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_structure_predicate3(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_predicate3);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* define */
static int structure_define2_predicate_callname_(struct defstruct *str, addr *ret)
{
	addr name;

	if (! str->predicate_p) {
		GetNameStructure(str->instance, &name);
		GetNameSymbol(name, &name);
		Return(string_concat_char2_heap_(&name, name, "-P"));
	}
	else if (str->predicate == Nil) {
		return Result(ret, Nil);
	}
	else {
		Check(! stringp(str->predicate), "type error");
		name = str->predicate;
	}

	return intern_default_package_(str->ptr, name, ret, NULL);
}

int structure_define2_predicate_(struct defstruct *str)
{
	addr instance, symbol;

	instance = str->instance;
	Return(structure_define2_predicate_callname_(str, &symbol));
	SetPredicateStructure(instance, symbol);
	if (symbol == Nil)
		return 0;

	Return(parse_callname_error_(&symbol, symbol));
	if (str->type_list_p)
		defun_structure_predicate2(str, symbol);
	else if (str->type_vector_p)
		defun_structure_predicate3(str, symbol);
	else
		return fmte_("Invalid structure type.", NULL);

	return 0;
}

int structure_define3_predicate_(struct defstruct *str)
{
	return structure_define2_predicate_(str);
}


/*
 *  print
 */
int structure_define2_print_(struct defstruct *str)
{
	if (str->type_p && str->print_object_p)
		return fmte_("Can't make print-object on :TYPE structure.", NULL);
	if (str->type_p && str->print_function_p)
		return fmte_("Can't make print-function on :TYPE structure.", NULL);

	return 0;
}

int structure_define3_print_(struct defstruct *str)
{
	return structure_define2_print_(str);
}


/*
 *  define
 */
int structure_define2_(struct defstruct *str)
{
	/* make instance */
	structure_instance2(str);
	Check(! structure_object_p(str->instance), "type error");
	setstructure_symbol(str->name, str->instance);

	/* settings */
	Return(structure_define2_slots_(str));
	Return(structure_define2_call_(str));
	Return(structure_define2_copier_(str));
	Return(structure_define2_predicate_(str));
	Return(structure_define2_constructor_(str));
	Return(structure_define2_print_(str));

	return 0;
}

int structure_define3_(struct defstruct *str)
{
	/* make instance */
	structure_instance2(str);
	Check(! structure_object_p(str->instance), "type error");
	setstructure_symbol(str->name, str->instance);

	/* settings */
	Return(structure_define3_slots_(str));
	Return(structure_define3_call_(str));
	Return(structure_define3_copier_(str));
	Return(structure_define3_predicate_(str));
	Return(structure_define3_constructor_(str));
	Return(structure_define3_print_(str));

	return 0;
}


/*
 *  initialize
 */
void init_structure_define2(void)
{
	SetPointerCall(defun, var1, structure_reader2);
	SetPointerCall(defun, var2, structure_writer2);
	SetPointerCall(defun, dynamic, structure_constructor2);
	SetPointerCall(defun, var1, structure_copier2);
	SetPointerCall(defun, var1, structure_predicate2);
}

void init_structure_define3(void)
{
	SetPointerCall(defun, var1, structure_reader3);
	SetPointerCall(defun, var2, structure_writer3);
	SetPointerCall(defun, dynamic, structure_constructor3);
	SetPointerCall(defun, var1, structure_copier3);
	SetPointerCall(defun, var1, structure_predicate3);
}


/************************************************************
 *  structure_defstruct.c
 ************************************************************/

void localhold_defstruct(struct defstruct *str, LocalHold hold)
{
	localhold_pushva_force(hold, str->instance, str->env, str->doc, str->slots,
			str->name, str->conc_name, str->copier, str->predicate,
			str->constructor, str->iname, str->iargs,
			str->print_object, str->print_function,
			str->type_vector, str->initial_offset, NULL);
}

void defstruct_clean(struct defstruct *str)
{
	clearpoint(str);
	str->instance = Unbound;
	str->env = Unbound;
	str->doc = Unbound;
	str->slots = Unbound;
	str->name = Unbound;
	str->conc_name = Unbound;
	str->copier = Nil;
	str->predicate = Nil;
	str->constructor = Nil;
	str->iname = Nil;
	str->iargs = Nil;
	str->print_object = Unbound;
	str->print_function = Unbound;
	str->type_vector = Unbound;
	str->initial_offset = Unbound;
	str->change = Nil;
}


/************************************************************
 *  structure_delete.c
 ************************************************************/

/*
 *  name
 */
static int structure_delete1_instance_(addr name)
{
	if (! symbolp(name))
		return TypeError_(name, SYMBOL);
	clos_define_class(name, Nil);

	return 0;
}

static int structure_delete2_instance_(addr name)
{
	if (! symbolp(name))
		return TypeError_(name, SYMBOL);
	remstructure_symbol(name);

	return 0;
}

static int structure_delete3_instance_(addr name)
{
	return structure_delete2_instance_(name);
}


/*
 *  call
 */
int structure_delete1_call_(addr instance)
{
	addr list, pos;

	Check(! structure_class_p_debug(instance), "type error");
	Return(stdget_structure_access_(instance, &list));
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCdr(pos, &pos);
		/* reader */
		SetFunctionSymbol(pos, Unbound);
		/* writer (setf ...) */
		remsetf_symbol(pos);
	}

	return 0;
}

int structure_delete2_call_(addr instance)
{
	addr list, pos;

	CheckType(instance, LISPSYSTEM_STRUCTURE);
	GetAccessStructure(instance, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCdr(pos, &pos);
		/* reader */
		SetFunctionSymbol(pos, Unbound);
		/* writer (setf ...) */
		remsetf_symbol(pos);
	}

	return 0;
}

int structure_delete3_call_(addr instance)
{
	return structure_delete2_call_(instance);
}


/*
 *  copier
 */
int structure_delete1_copier_(addr instance)
{
	addr symbol;

	Check(! structure_class_p_debug(instance), "type error");
	Return(stdget_structure_copier_(instance, &symbol));
	if (symbol != Nil) {
		SetFunctionSymbol(symbol, Unbound);
	}

	return 0;
}

int structure_delete2_copier_(addr instance)
{
	addr symbol;

	CheckType(instance, LISPSYSTEM_STRUCTURE);
	GetCopierStructure(instance, &symbol);
	if (symbol != Nil) {
		SetFunctionSymbol(symbol, Unbound);
	}

	return 0;
}

int structure_delete3_copier_(addr instance)
{
	return structure_delete2_copier_(instance);
}


/*
 *  predicate
 */
int structure_delete1_predicate_(addr instance)
{
	addr symbol;

	Check(! structure_class_p_debug(instance), "type error");
	Return(stdget_structure_predicate_(instance, &symbol));
	if (symbol != Nil) {
		SetFunctionSymbol(symbol, Unbound);
	}

	return 0;
}

int structure_delete2_predicate_(addr instance)
{
	addr symbol;

	CheckType(instance, LISPSYSTEM_STRUCTURE);
	GetPredicateStructure(instance, &symbol);
	if (symbol != Nil) {
		SetFunctionSymbol(symbol, Unbound);
	}

	return 0;
}

int structure_delete3_predicate_(addr instance)
{
	return structure_delete2_predicate_(instance);
}


/*
 *  constructor
 */
int structure_delete1_constructor_(addr instance)
{
	addr list, symbol;

	Check(! structure_class_p_debug(instance), "type error");
	Return(stdget_structure_constructor_(instance, &list));
	while (list != Nil) {
		GetCons(list, &symbol, &list);
		SetFunctionSymbol(symbol, Unbound);
	}

	return 0;
}

int structure_delete2_constructor_(addr instance)
{
	addr list, symbol;

	CheckType(instance, LISPSYSTEM_STRUCTURE);
	GetConstructorStructure(instance, &list);
	while (list != Nil) {
		GetCons(list, &symbol, &list);
		SetFunctionSymbol(symbol, Unbound);
	}

	return 0;
}

int structure_delete3_constructor_(addr instance)
{
	return structure_delete2_constructor_(instance);
}


/*
 *  print-object
 */
static int structure_delete1_method_(Execute ptr, addr instance, addr gen, addr *ret)
{
	addr list, type1, type2;

	/* (structure T) */
	type1 = instance;
	GetConst(CLOS_T, &type2);
	list_heap(&list, type1, type2, NULL);

	/* find-method */
	return method_find_method_nil_(ptr, gen, Nil, list, ret);
}

int structure_delete1_print_(Execute ptr, addr instance)
{
	addr gen, method;

	Check(! structure_class_p_debug(instance), "type error");
	GetConst(COMMON_PRINT_OBJECT, &gen);
	GetFunctionSymbol(gen, &gen);
	Return(structure_delete1_method_(ptr, instance, gen, &method));
	if (method != Nil) {
		Return(method_remove_method_(ptr, gen, method));
	}

	return 0;
}


/*
 *  delete
 */
static int structure_delete1_(Execute ptr, addr name, addr instance)
{
	Return(structure_delete1_instance_(name));
	Return(structure_delete1_call_(instance));
	Return(structure_delete1_copier_(instance));
	Return(structure_delete1_predicate_(instance));
	Return(structure_delete1_constructor_(instance));
	Return(structure_delete1_print_(ptr, instance));

	return 0;
}

static int structure_delete2_(Execute ptr, addr name, addr instance)
{
	Return(structure_delete2_instance_(name));
	Return(structure_delete2_call_(instance));
	Return(structure_delete2_copier_(instance));
	Return(structure_delete2_predicate_(instance));
	Return(structure_delete2_constructor_(instance));

	return 0;
}

static int structure_delete3_(Execute ptr, addr name, addr instance)
{
	Return(structure_delete3_instance_(name));
	Return(structure_delete3_call_(instance));
	Return(structure_delete3_copier_(instance));
	Return(structure_delete3_predicate_(instance));
	Return(structure_delete3_constructor_(instance));

	return 0;
}

int structure_delete_(Execute ptr, addr name, int *ret)
{
	addr pos;

	/* type */
	if (! symbolp(name))
		return TypeError_(name, SYMBOL);

	/* class */
	if (structure_get_class(name, &pos)) {
		Return(structure_delete1_(ptr, name, pos));
		return Result(ret, 1);
	}

	/* object */
	if (structure_get_object(name, &pos)) {
		if (structure_list_p(pos)) {
			Return(structure_delete2_(ptr, name, pos));
		}
		if (structure_vector_p(pos)) {
			Return(structure_delete3_(ptr, name, pos));
		}
		return Result(ret, 1);
	}

	/* error */
	return Result(ret, 0);
}


/************************************************************
 *  structure_direct.c
 ************************************************************/

/*
 *  structure-type
 */
struct structure_type_struct *ptrstructuretype(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	return PtrStructureType_Low(pos);
}

void getinstancestructuretype(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetInstanceStructureType_Low(pos, ret);
}

void setinstancestructuretype(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetInstanceStructureType_Low(pos, value);
}

void getnamestructuretype(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetNameStructureType_Low(pos, ret);
}

void setnamestructuretype(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetNameStructureType_Low(pos, value);
}

void getslotstructuretype(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetSlotStructureType_Low(pos, ret);
}

void setslotstructuretype(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetSlotStructureType_Low(pos, value);
}

void getvectorstructuretype(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetVectorStructureType_Low(pos, ret);
}

void setvectorstructuretype(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetVectorStructureType_Low(pos, value);
}

int refnamedstructuretype(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	return RefNamedStructureType_Low(pos);
}

void getnamedstructuretype(addr pos, int *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetNamedStructureType_Low(pos, ret);
}

void setnamedstructuretype(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetNamedStructureType_Low(pos, value);
}

int referrorpstructuretype(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	return RefErrorpStructureType_Low(pos);
}

void geterrorpstructuretype(addr pos, int *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetErrorpStructureType_Low(pos, ret);
}

void seterrorpstructuretype(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetErrorpStructureType_Low(pos, value);
}

static void structure_type_heap_unsafe(addr *ret)
{
	heap_smallsize(ret, LISPSYSTEM_STRUCTURE_TYPE,
			StructureType_size, sizeoft(struct structure_type_struct));
}

/*
 *  access
 */
static int structure_getdirect_(addr vector, size_t index, addr *ret)
{
	return getelt_sequence_(NULL, vector, index, ret);
}

int structure_getarray_(addr vector, addr slot, addr *ret)
{
	size_t index;
	GetAccessSlot(slot, &index);
	return structure_getdirect_(vector, index, ret);
}

static int structure_setcheck_error_(Execute ptr, addr type, addr value)
{
	int check;

	CheckType(type, LISPTYPE_TYPE);
	Return(typep_clang_(ptr, value, type, &check));
	if (! check) {
		return call_type_error_va_(ptr, value, type,
				"The value ~S don't match ~A type.", value, type, NULL);
	}

	return 0;
}

int structure_write1_(Execute ptr, addr instance, addr slot, addr value)
{
	addr type, var;
	size_t index;

	/* check */
	GetTypeSlot(slot, &type);
	if (value != Unbound) {
		Return(structure_setcheck_error_(ptr, type, value));
	}
	else {
		value = Nil;
	}

	/* write */
	GetLocationSlot(slot, &index);
	GetValueClos(instance, &var);
	SetClosValue(var, index, value);
	return 0;
}

int structure_write2_(Execute ptr, addr list, addr slot, addr value)
{
	addr type;
	size_t index;

	/* check */
	GetTypeSlot(slot, &type);
	Return(structure_setcheck_error_(ptr, type, value));

	/* write */
	GetAccessSlot(slot, &index);
	return setnth_(list, index, value);
}

int structure_write3_(Execute ptr, addr vector, addr slot, addr type1, addr value)
{
	addr type2;
	size_t index;

	/* check */
	GetTypeSlot(slot, &type2);
	Return(structure_setcheck_error_(ptr, type1, value));
	Return(structure_setcheck_error_(ptr, type2, value));

	/* write */
	GetAccessSlot(slot, &index);
	return setelt_sequence_(vector, index, value);
}


/*
 *  structure-type
 */
void structure_type_heap(addr *ret)
{
	addr pos;
	structure_type_heap_unsafe(&pos);
	clearpoint(PtrStructureType(pos));
	*ret = pos;
}

static void structure_type_parameter(addr *ret,
		addr instance, addr name, addr slot, addr vector,
		size_t size, size_t size_all, unsigned named, size_t named_index,
		enum ARRAY_TYPE type1, int type2)
{
	addr pos;
	struct structure_type_struct *str;

	structure_type_heap_unsafe(&pos);
	SetInstanceStructureType(pos, instance);
	SetNameStructureType(pos, name);
	SetSlotStructureType(pos, slot);
	SetVectorStructureType(pos, vector);
	str = PtrStructureType(pos);
	str->size = size;
	str->size_all = size_all;
	str->named = named;
	str->named_index = named_index;
	str->errorp = 0;
	str->type1 = type1;
	str->type2 = type2;
	*ret = pos;
}

void structure_type(struct defstruct *str, addr slot, addr *ret)
{
	structure_type_parameter(ret,
			str->instance, str->name, slot, str->type_vector,
			str->size, str->size_all, str->named_p, str->named_index,
			str->type1, str->type2);
}

int structure_type_list_p(addr type, addr var, int *ret)
{
	struct structure_type_struct *str;
	size_t size;

	/* listp */
	str = PtrStructureType(type);
	if (length_list_p(var, &size))
		return Result(ret, 0);
	/* length */
	if (size < str->size_all)
		return Result(ret, 0);
	/* check */
	if (str->named) {
		GetNameStructureType(type, &type);
		Return(getnth_(var, str->named_index, &var));
		return Result(ret, var == type);
	}

	return Result(ret, 1);
}

int structure_type_vector_p(Execute ptr, addr type, addr var, int *ret)
{
	struct structure_type_struct *str;
	size_t size;

	/* vectorp */
	if (! vector_type_p(var)) {
		*ret = 0;
		return 0;
	}
	Return(length_sequence_(var, 1, &size));
	/* length */
	str = PtrStructureType(type);
	if (size < str->size_all) {
		*ret = 0;
		return 0;
	}
	/* check */
	if (str->named) {
		Return(structure_getdirect_(var, str->named_index, &var));
		GetNameStructureType(type, &type);
		*ret = (var == type);
		return 0;
	}
	*ret = 1;
	return 0;
}


/************************************************************
 *  structure_make.c
 ************************************************************/

/*
 *  find
 */
static int make_structure_dynamic_find_(addr key, addr slots, int *ret)
{
	int check;
	addr value;
	size_t size, i;

	Check(! symbolp(key), "type error");
	Check(! slot_vector_p(slots), "type error");
	GetNameSymbol(key, &key);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &value);
		GetNameSlot(value, &value);
		GetNameSymbol(value, &value);
		Return(string_equal_(key, value, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int make_structure_dynamic_p_(int errorp, addr key, addr slots, int *ret)
{
	int check;

	if (! errorp)
		return Result(ret, 0);
	Return(make_structure_dynamic_find_(key, slots, &check));
	return Result(ret, ! check);
}

static int make_structure_dynamic_(addr instance, addr slots, addr list, int errorp)
{
	int check;
	addr key;

	while (list != Nil) {
		if (! consp_getcons(list, &key, &list))
			return fmte_("Invalid keyword-argumets ~S.", list, NULL);
		if (! consp(list))
			return fmte_("There is no value in the key ~S arguemnts.", key, NULL);
		if (! symbolp(key))
			return fmte_("The key ~S must be a symbol type.", key, NULL);
		Return(make_structure_dynamic_p_(errorp, key, slots, &check));
		if (check) {
			return fmte_("There is no slot ~S "
					"in the structure ~S.", key, instance, NULL);
		}
		GetCdr(list, &list);
	}

	return 0;
}

static int make_structure_find_(addr key, addr list, addr *value, int *ret)
{
	int check;
	addr left, right, g;

	Check(! slotp(key), "type error");
	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	GetNameSlot(key, &key);
	GetNameSymbol(key, &key);
	while (list != Nil) {
		Return_getcons(list, &left, &list);
		Return_getcons(list, &right, &list);
		Check(! symbolp(left), "type error");
		if (right == g)
			continue;
		GetNameSymbol(left, &left);
		Return(string_equal_(key, left, &check));
		if (check) {
			*value = right;
			return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}


/*
 *  init
 */
static int make_structure_value_(Execute ptr, addr slot, addr *ret)
{
	addr pos;

	/* initfunction */
	GetFunctionSlot(slot, &pos);
	if (pos != Nil)
		return apply1_control_(ptr, ret, pos, Nil);

	/* initform */
	GetFormSlot(slot, ret);
	return 0;
}

static int make_structure1_init_(Execute ptr, addr clos, addr args, int initp)
{
	int check;
	addr slots, slot, pos;
	size_t size, i;

	GetSlotClos(clos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		if (! initp) {
			Return(structure_write1_(ptr, clos, slot, Unbound));
			continue;
		}
		Return(make_structure_find_(slot, args, &pos, &check));
		if (! check) {
			Return(make_structure_value_(ptr, slot, &pos));
		}
		Return(structure_write1_(ptr, clos, slot, pos));
	}

	return 0;
}

static int make_structure2_init_(Execute ptr, addr list, addr slots, addr args)
{
	int check;
	addr slot, pos;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		Return(make_structure_find_(slot, args, &pos, &check));
		if (! check) {
			Return(make_structure_value_(ptr, slot, &pos));
		}
		Return(structure_write2_(ptr, list, slot, pos));
	}

	return 0;
}

static int make_structure3_init_(Execute ptr,
		addr vector, addr slots, addr vector_type, addr args)
{
	int check;
	addr slot, pos;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		Return(make_structure_find_(slot, args, &pos, &check));
		if (! check) {
			Return(make_structure_value_(ptr, slot, &pos));
		}
		Return(structure_write3_(ptr, vector, slot, vector_type, pos));
	}

	return 0;
}


/*
 *  make
 */
int make_structure1_(Execute ptr, addr *ret, addr pos, addr args, int initp)
{
	int errorp;
	addr instance, slots, clos;
	LocalHold hold;

	/* variables */
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	GetErrorpStructureType(pos, &errorp);
	Return(make_structure_dynamic_(instance, slots, args, errorp));
	/* make */
	clos_heap(&clos, slots);
	SetClassOfClos(clos, instance);

	hold = LocalHold_local_push(ptr, clos);
	Return(make_structure1_init_(ptr, clos, args, initp));
	localhold_end(hold);

	return Result(ret, clos);
}

static void make_structure2_nil(addr *ret, size_t size)
{
	addr list;
	size_t i;

	list = Nil;
	for (i = 0; i < size; i++)
		cons_heap(&list, Nil, list);
	*ret = list;
}

int make_structure2_(Execute ptr, addr *ret, addr pos, addr args, int initp)
{
	addr instance, slots, list;
	struct structure_type_struct *str;
	LocalHold hold;

	/* type */
	str = PtrStructureType(pos);
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	/* make */
	Return(make_structure_dynamic_(instance, slots, args, str->errorp));
	make_structure2_nil(&list, str->size_all);

	if (initp) {
		hold = LocalHold_local_push(ptr, list);
		Return(make_structure2_init_(ptr, list, slots, args));
		localhold_end(hold);
	}

	return Result(ret, list);
}

static int make_structure3_vector_(addr *ret,
		enum ARRAY_TYPE type1, int type2, size_t size)
{
	struct array_struct *str;
	addr pos;

	/* vector */
	if (type1 == ARRAY_TYPE_T) {
		vector_heap(ret, size);
		return 0;
	}

	/* array */
	array_empty_heap(&pos);
	str = ArrayInfoStruct(pos);
	str->type = type1;
	str->bytesize = type2;
	str->dimension = 1;
	str->size = str->front = size;
	array_set_type(pos);
	array_set_element_size(pos);
	array_set_simple(pos);
	Return(array_make_memory_(pos, Nil, Nil, Nil, Nil));
	Return(array_make_clear_(pos));
	return Result(ret, pos);
}

int make_structure3_(Execute ptr, addr *ret, addr pos, addr args, int initp)
{
	addr instance, slots, vector, type;
	struct structure_type_struct *str;
	LocalHold hold;

	/* type */
	str = PtrStructureType(pos);
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	GetVectorStructureType(pos, &type);
	/* make */
	Return(make_structure_dynamic_(instance, slots, args, str->errorp));
	Return(make_structure3_vector_(&vector, str->type1, str->type2, str->size_all));

	if (initp) {
		hold = LocalHold_local_push(ptr, vector);
		Return(make_structure3_init_(ptr, vector, slots, type, args));
		localhold_end(hold);
	}

	return Result(ret, vector);
}


/*
 *  common
 */
int make_structure1_common_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp)
{
	addr pos, value;
	LocalHold hold;

	structure_type_heap(&pos);
	SetInstanceStructureType(pos, instance);
	Return(stdget_structure_slots_(instance, &value));
	SetSlotStructureType(pos, value);
	SetErrorpStructureType(pos, errorp);

	hold = LocalHold_local_push(ptr, pos);
	Return(make_structure1_(ptr, ret, pos, rest, initp));
	localhold_end(hold);

	return 0;
}

int make_structure2_common_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp)
{
	int named;
	addr pos, name, slots;
	size_t size, size_all;
	struct structure_type_struct *str;
	LocalHold hold;

	/* value */
	GetNameStructure(instance, &name);
	GetSlotsStructure(instance, &slots);
	named = structure_named_p(instance);
	size_all = get_size_all_structure(instance);
	/* type */
	structure_type_heap(&pos);
	SetInstanceStructureType(pos, instance);
	SetNameStructureType(pos, name);
	SetSlotStructureType(pos, slots);
	LenSlotVector(slots, &size);
	str = PtrStructureType(pos);
	str->errorp = (errorp != 0);
	str->named = (named != 0);
	str->size = size;
	str->size_all = size_all;

	hold = LocalHold_local_push(ptr, pos);
	Return(make_structure2_(ptr, ret, pos, rest, initp));
	localhold_end(hold);

	return 0;
}

int make_structure3_common_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp)
{
	enum ARRAY_TYPE type1;
	int named, type2;
	addr pos, name, slots, type;
	size_t size, size_all;
	struct structure_type_struct *str;
	LocalHold hold;

	/* value */
	GetNameStructure(instance, &name);
	GetSlotsStructure(instance, &slots);
	GetSpecializedStructure(instance, &type);
	named = structure_named_p(instance);
	size_all = get_size_all_structure(instance);
	gettype_structure(instance, &type1, &type2);

	/* type */
	structure_type_heap(&pos);
	SetInstanceStructureType(pos, instance);
	SetNameStructureType(pos, name);
	SetSlotStructureType(pos, slots);
	SetVectorStructureType(pos, type);
	LenSlotVector(slots, &size);
	str = PtrStructureType(pos);
	str->errorp = (errorp != 0);
	str->named = (named != 0);
	str->size = size;
	str->size_all = size_all;
	str->type1 = type1;
	str->type2 = type2;

	hold = LocalHold_local_push(ptr, pos);
	Return(make_structure3_(ptr, ret, pos, rest, initp));
	localhold_end(hold);

	return 0;
}


/************************************************************
 *  structure_object.c
 ************************************************************/

int structure_object_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_STRUCTURE;
}

struct structure_struct *ptrstructure(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	return PtrStructure_Low(pos);
}

void getnamestructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetNameStructure_Low(pos, ret);
}

void setnamestructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetNameStructure_Low(pos, value);
}

void getslotsstructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetSlotsStructure_Low(pos, ret);
}

void setslotsstructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetSlotsStructure_Low(pos, value);
}

void getdirectstructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetDirectStructure_Low(pos, ret);
}

void setdirectstructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetDirectStructure_Low(pos, value);
}

void getdocstructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetDocStructure_Low(pos, ret);
}

void setdocstructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetDocStructure_Low(pos, value);
}

void getincludestructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetIncludeStructure_Low(pos, ret);
}

void setincludestructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetIncludeStructure_Low(pos, value);
}

void getprecedencestructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetPrecedenceStructure_Low(pos, ret);
}

void setprecedencestructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetPrecedenceStructure_Low(pos, value);
}

void getspecializedstructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetSpecializedStructure_Low(pos, ret);
}

void setspecializedstructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetSpecializedStructure_Low(pos, value);
}

void getpredicatestructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetPredicateStructure_Low(pos, ret);
}

void setpredicatestructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetPredicateStructure_Low(pos, value);
}

void getaccessstructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetAccessStructure_Low(pos, ret);
}

void setaccessstructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetAccessStructure_Low(pos, value);
}

void getcopierstructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetCopierStructure_Low(pos, ret);
}

void setcopierstructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetCopierStructure_Low(pos, value);
}

void getconstructorstructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetConstructorStructure_Low(pos, ret);
}

void setconstructorstructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetConstructorStructure_Low(pos, value);
}

void structure_heap(addr *ret)
{
	addr pos;
	struct structure_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_STRUCTURE,
			Structure_size, sizeoft(struct structure_struct));
	str = PtrStructure_Low(pos);
	clearpoint(str);
	*ret = pos;
}

int structure_named_p(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	return PtrStructure(pos)->named_p;
}

int structure_list_p(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	return PtrStructure(pos)->list_p;
}

int structure_vector_p(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	return PtrStructure(pos)->vector_p;
}

void set_named_p_structure(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	PtrStructure(pos)->named_p = (value != 0);
}

void set_list_p_structure(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	PtrStructure(pos)->list_p = (value != 0);
}

void set_vector_p_structure(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	PtrStructure(pos)->vector_p = (value != 0);
}

size_t get_size_structure(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	return PtrStructure(pos)->size;
}

size_t get_size_all_structure(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	return PtrStructure(pos)->size_all;
}

size_t get_offset_structure(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	return PtrStructure(pos)->offset;
}

void set_size_structure(addr pos, size_t value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	PtrStructure(pos)->size = value;
}

void set_size_all_structure(addr pos, size_t value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	PtrStructure(pos)->size_all = value;
}

void set_offset_structure(addr pos, size_t value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	PtrStructure(pos)->offset = value;
}

void gettype_structure(addr pos, enum ARRAY_TYPE *rtype1, int *rtype2)
{
	struct structure_struct *str;

	str = PtrStructure(pos);
	*rtype1 = str->type1;
	*rtype2 = str->type2;
}

void settype_structure(addr pos, enum ARRAY_TYPE type1, int type2)
{
	struct structure_struct *str;

	str = PtrStructure(pos);
	str->type1 = type1;
	str->type2 = type2;
}

void structure_swap(addr x, addr y)
{
	int i;
	addr array[Structure_size], z;
	struct structure_struct *strx, *stry, str;

	CheckType(x, LISPSYSTEM_STRUCTURE);
	CheckType(y, LISPSYSTEM_STRUCTURE);
	strx = PtrStructure(x);
	stry = PtrStructure(y);

	/* x -> temp */
	str = *strx;
	for (i = 0; i < Structure_size; i++) {
		GetArraySS(x, i, &z);
		array[i] = z;
	}

	/* y -> x */
	*strx = *stry;
	for (i = 0; i < Structure_size; i++) {
		GetArraySS(y, i, &z);
		SetArraySS(x, i, z);
	}

	/* temp -> y */
	*stry = str;
	for (i = 0; i < Structure_size; i++) {
		z = array[i];
		SetArraySS(y, i, z);
	}
}


/************************************************************
 *  structure_parse.c
 ************************************************************/

static int ensure_structure_constructor_(addr args, addr *result, int *ret)
{
	addr key, value, keyword, root;

	GetConst(KEYWORD_CONSTRUCTOR, &keyword);
	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &key, &args);
		Return_getcons(args, &value, &args);
		if (key != keyword)
			continue;
		cons_heap(&root, value, root);
	}
	nreverse(result, root);

	return Result(ret, root != Nil);
}

static int ensure_structure_vector_type(addr type, addr *ret)
{
	addr car, cdr, check;

	/* (vector upgraded) */
	if (! consp(type))
		return 0;
	GetCons(type, &car, &cdr);
	GetConst(COMMON_VECTOR, &check);
	if (car != check)
		return 0;
	if (! consp(cdr))
		return 0;
	GetCons(cdr, &car, &cdr);
	if (cdr != Nil)
		return 0;
	*ret = car;
	return 1;
}

static int ensure_structure_upgraded_(struct defstruct *str,
		addr pos, enum ARRAY_TYPE *rtype1, int *rtype2)
{
	Execute ptr;
	LocalHold hold;

	ptr = str->ptr;
	hold = LocalHold_array(ptr, 2);
	localhold_set(hold, 0, pos);
	Return(parse_type_(ptr, &pos, pos, Nil));
	localhold_set(hold, 1, pos);
	Return(upgraded_array_value_(pos, rtype1, rtype2));
	localhold_end(hold);

	return 0;
}

static int ensure_structure_vector_(struct defstruct *str, addr pos)
{
	enum ARRAY_TYPE type1;
	int type2;
	addr check;

	/* list */
	GetConst(COMMON_LIST, &check);
	if (pos == check) {
		str->type_p = 1;
		str->type_list_p = 1;
		return 0;
	}

	/* vector */
	GetConst(COMMON_VECTOR, &check);
	if (pos == check) {
		str->type_p = 1;
		str->type_vector_p = 1;
		Return(parse_type_(str->ptr, &pos, pos, Nil));
		str->type_vector = pos;
		str->type1 = ARRAY_TYPE_T;
		str->type2 = 0;
		return 0;
	}

	/* specialized array */
	if (ensure_structure_vector_type(pos, &pos)) {
		str->type_p = 1;
		str->type_vector_p = 1;
		Return(parse_type_(str->ptr, &pos, pos, Nil));
		str->type_vector = pos;
		Return(ensure_structure_upgraded_(str, pos, &type1, &type2));
		str->type1 = type1;
		str->type2 = type2;
		return 0;
	}

	Check(pos == NULL, "null error");
	return fmte_("Invalid type specifier, ~S.", pos, NULL);
}

int ensure_structure_struct_(struct defstruct *str,
		Execute ptr, addr name, addr slots, addr args)
{
	int check;
	addr pos, value;

	defstruct_clean(str);
	str->ptr = ptr;
	str->slots = slots;
	str->name = name;
	/* :documentation */
	if (GetKeyArgs(args, KEYWORD_DOCUMENTATION, &pos))
		pos = Nil;
	str->doc = pos;
	/* :conc-name */
	if (! GetKeyArgs(args, KEYWORD_CONC_NAME, &pos)) {
		str->conc_name_p = 1;
		str->conc_name = pos;
	}
	/* :type */
	if (! GetKeyArgs(args, KEYWORD_TYPE, &pos)) {
		Return(ensure_structure_vector_(str, pos));
	}
	/* :initial-offset */
	if (! GetKeyArgs(args, KEYWORD_INITIAL_OFFSET, &pos)) {
		str->initial_offset_p = 1;
		str->initial_offset = pos;
		Return(getindex_integer_(pos, &(str->offset)));
	}
	/* :named */
	if (! GetKeyArgs(args, KEYWORD_NAMED, &pos)) {
		str->named_p = (pos != Nil);
	}
	/* :copier */
	if (! GetKeyArgs(args, KEYWORD_COPIER, &pos)) {
		str->copier_p = 1;
		str->copier = pos;
	}
	/* :predicate */
	if (! GetKeyArgs(args, KEYWORD_PREDICATE, &pos)) {
		str->predicate_p = 1;
		str->predicate = pos;
	}
	/* :include */
	if (! GetKeyArgs(args, KEYWORD_INCLUDE, &pos)) {
		if (! consp(pos))
			return fmte_("Invalid :include format ~S.", pos, NULL);
		GetCons(pos, &pos, &value);
		str->include_p = 1;
		str->iname = pos;
		str->iargs = value;
	}
	/* :print-object */
	if (! GetKeyArgs(args, KEYWORD_PRINT_OBJECT, &pos)) {
		str->print_object_p = 1;
		str->print_object = pos;
	}
	/* :print-function */
	if (! GetKeyArgs(args, KEYWORD_PRINT_FUNCTION, &pos)) {
		str->print_function_p = 1;
		str->print_function = pos;
	}
	/* :constructor */
	Return(ensure_structure_constructor_(args, &pos, &check));
	if (check) {
		str->constructor_p = 1;
		str->constructor = pos;
	}

	return 0;
}


/*
 *  check-instance
 */
static int structure_slots_heap_(addr list, addr *ret)
{
	addr pos, name, init, type, readonly, root;

	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		Return(list_bind_(pos, &name, &init, &type, &readonly, NULL));
		slot_heap(&pos);
		SetNameSlot(pos, name);
		SetFormSlot(pos, Nil);
		SetTypeSlot(pos, type);
		SetFunctionSlot(pos, init);
		SetReadOnlySlot(pos, readonly);
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);

	return 0;
}

static int structure_slots_make_(struct defstruct *str, LocalHold hold)
{
	addr list;

	/* slots: 0 */
	Return(structure_slots_heap_(str->slots, &list));
	localhold_set(hold, 0, list);
	str->slots = list;

	/* iargs: 1 */
	Return(structure_slots_heap_(str->iargs, &list));
	localhold_set(hold, 1, list);
	str->iargs = list;

	return 0;
}

static int structure_check_slots_(addr list)
{
	int check;
	addr pos, a, b, tail;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		GetNameSlot(pos, &a);
		Check(! symbolp(a), "type error");
		GetNameSymbol(a, &a);
		for (tail = list; tail != Nil; ) {
			Return_getcons(tail, &pos, &tail);
			GetNameSlot(pos, &b);
			Check(! symbolp(b), "type error");
			GetNameSymbol(b, &b);
			Return(string_equal_(a, b, &check));
			if (check) {
				return call_simple_program_error_va_(NULL,
						"The slot name ~S is duplicated in the defstruct.", a, NULL);
			}
		}
	}

	return 0;
}

static int structure_check_predicate_(struct defstruct *str)
{
	if (str->type_p && (! str->named_p)) {
		/* no-predicate */
		if (! str->predicate_p) {
			str->predicate_p = 1;
			str->predicate = Nil;
			return 0;
		}
		if (str->predicate == Nil) {
			return 0;
		}
		return fmte_("DEFSTRUCT ~S is defined :PREDICATE, "
				"but the structure is not named.", str->name, NULL);
	}
	if (str->predicate_p && str->predicate == T) {
		str->predicate_p = 0;
		return 0;
	}

	return 0;
}

static int structure_check1_include_(struct defstruct *str)
{
	int check;
	addr instance;

	/* instance check */
	if (! structure_get_class(str->iname, &instance))
		return fmte_(":INCLUDE ~S structure don't exist.", str->iname, NULL);
	Return(structure_class_p_(instance, &check));
	if (! check)
		return fmte_(":INCLUDE ~S must be structure type.", instance, NULL);

	/* instance */
	str->iname = instance;
	return 0;
}

static int structure_check2_include_(struct defstruct *str)
{
	addr instance;

	/* instance check */
	if (! structure_get_object(str->iname, &instance))
		return fmte_(":INCLUDE ~S structure don't exist.", str->iname, NULL);
	if (! structure_object_p(instance))
		return fmte_(":INCLUDE ~S must be structure type.", instance, NULL);

	/* list check */
	if (! structure_list_p(instance))
		return fmte_(":TYPE option is LIST, but :INCLUDE type is not LIST.", NULL);

	/* instance */
	str->iname = instance;
	return 0;
}

static int structure_check3_include_(struct defstruct *str)
{
	enum ARRAY_TYPE type1;
	int type2;
	addr instance, x, y;

	/* instance check */
	if (! structure_get_object(str->iname, &instance))
		return fmte_(":INCLUDE ~S structure don't exist.", str->iname, NULL);
	if (! structure_object_p(instance))
		return fmte_(":INCLUDE ~S must be structure type.", instance, NULL);

	/* vector check */
	if (! structure_vector_p(instance))
		return fmte_(":TYPE option is VECTOR, but :INCLUDE type is not VECTOR.", NULL);

	/* upgraded-type */
	gettype_structure(instance, &type1, &type2);
	if (str->type1 != type1 || str->type2 != type2) {
		x = str->type_vector;
		GetSpecializedStructure(instance, &y);
		return fmte_(":TYPE ~A is not in the include ~A type.", x, y, NULL);
	}

	/* instance */
	str->iname = instance;
	return 0;
}

static int structure_check_include_(struct defstruct *str)
{
	if (! str->include_p)
		return 0;
	if (str->type_list_p)
		return structure_check2_include_(str);
	else if (str->type_vector_p)
		return structure_check3_include_(str);
	else
		return structure_check1_include_(str);
}

static int structure_find_slots_(struct defstruct *str,
		addr instance, addr name, addr *ret)
{
	int check;
	addr slots, pos, value;
	size_t size, i;

	Check(! structure_class_object_p_debug(instance), "type error");
	Check(! symbolp(name), "type error");

	/* find */
	GetNameSymbol(name, &name);
	if (str->type_p) {
		GetSlotsStructure(instance, &slots);
	}
	else {
		Return(stdget_structure_slots_(instance, &slots));
	}
	Check(! slot_vector_p(slots), "type error");
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		GetNameSlot(pos, &value);
		GetNameSymbol(value, &value);
		Return(string_equal_(name, value, &check));
		if (check)
			return Result(ret, pos);
	}

	return Result(ret, Unbound);
}

static int structure_check_include_slots_(struct defstruct *str)
{
	addr name, list, pos, instance;

	if (! str->include_p)
		return 0;
	instance = str->iname;
	for (list = str->slots; list != Nil; ) {
		GetCons(list, &pos, &list);
		GetNameSlot(pos, &name);
		Return(structure_find_slots_(str, instance, name, &pos));
		if (pos != Unbound) {
			return call_simple_program_error_va_(NULL,
					"The slot ~S already exist in :INCLUDE structure.", name, NULL);
		}
	}

	return 0;
}

static int structure_check_include_arguments_(struct defstruct *str)
{
	int result;
	addr name, list, instance, a, b, x, y, gensym;

	if (! str->include_p)
		return 0;
	instance = str->iname;
	GetConst(SYSTEM_STRUCTURE_GENSYM, &gensym);
	for (list = str->iargs; list != Nil; ) {
		GetCons(list, &a, &list);
		GetNameSlot(a, &name);
		Return(structure_find_slots_(str, instance, name, &b));
		if (b == Unbound) {
			return fmte_("The :include argument ~S don't exist "
					"in :INCLUDE structure.", name, NULL);
		}
		/* form */
		GetFunctionSlot(a, &x);
		if (x == gensym) {
			SetFunctionSlot(a, Nil);
		}
		/* type */
		GetTypeSlot(a, &x);
		GetTypeSlot(b, &y);
		if (x == gensym) {
			SetTypeSlot(a, y);
		}
		else {
			Return(parse_type_(str->ptr, &x, x, Nil));
			SetTypeSlot(a, x);
			Return(subtypep_check_(str->ptr, x, y, Nil, &result, NULL));
			if (! result) {
				return fmte_("The slot ~S type ~A is not "
						"in the include ~A type.", name, x, y, NULL);
			}
		}
		/* readonly */
		GetReadOnlySlot(a, &x);
		GetReadOnlySlot(b, &y);
		if (x == gensym) {
			SetReadOnlySlot(a, y);
		}
		else if (x == Nil && y == T) {
			return fmte_("The slot ~S is read-only "
					"but include slot is not read-only.", name, NULL);
		}
	}

	return 0;
}
static int structure_check_named_(struct defstruct *str)
{
	if (! str->named_p)
		return 0;
	if (! str->type_vector_p)
		return 0;
	if (str->type1 == ARRAY_TYPE_T)
		return 0;
	return fmte_("Cannot set :NAMED value at ~S vector.", str->type_vector, NULL);
}

static int structure_check_print_(struct defstruct *str)
{
	if (str->print_function_p && str->print_object_p) {
		return fmte_("The defstruct option must be have "
				"either :PRINT-OBJECT or :PRINT-FUNCTION, "
				"but there are both options", NULL);
	}

	return 0;
}

static int structure_slots_value_(struct defstruct *str)
{
	addr list, pos, check, g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	for (list = str->slots; list != Nil; ) {
		GetCons(list, &pos, &list);
		/* init */
		GetFunctionSlot(pos, &check);
		if (check == g) {
			SetFunctionSlot(pos, Nil);
		}

		/* type */
		GetTypeSlot(pos, &check);
		if (check == g) {
			GetTypeTable(&check, T);
		}
		else {
			Return(parse_type_(str->ptr, &check, check, Nil));
		}
		SetTypeSlot(pos, check);

		/* readonly */
		GetReadOnlySlot(pos, &check);
		if (check == g) {
			SetReadOnlySlot(pos, Nil);
		}
	}

	return 0;
}

int structure_arguments_(struct defstruct *str, LocalHold hold)
{
	Return(structure_slots_make_(str, hold));
	Return(structure_check_slots_(str->slots));
	Return(structure_check_slots_(str->iargs));
	Return(structure_check_predicate_(str));
	Return(structure_check_include_(str));
	Return(structure_check_include_slots_(str));
	Return(structure_check_include_arguments_(str));
	Return(structure_check_named_(str));
	Return(structure_check_print_(str));
	Return(structure_slots_value_(str));

	return 0;
}


/************************************************************
 *  strvect.c
 ************************************************************/

/*
 *  buffer compare
 */
#define equal_code(p1, p2, s1, s2) { \
	size_t i; \
	if (s1 != s2) return 0; \
	for (i = 0; i < s1; i++) { \
		if ((unicode)p1[i] != (unicode)p2[i]) { \
			return 0; \
		} \
	} \
	return 1; \
}

#define equalp_code(p1, p2, s1, s2) { \
	unicode a, b; \
	size_t i; \
	if (s1 != s2) return 0; \
	for (i = 0; i < s1; i++) { \
		a = p1[i]; \
		b = p2[i]; \
		a = toUpperUnicode(a); \
		b = toUpperUnicode(b); \
		if (a != b) return 0; \
	} \
	return 1; \
}

#define compare_code(p1, p2, s1, s2) { \
	unicode a, b; \
	size_t i; \
	if (s1 < s2) return -1; \
	if (s1 > s2) return 1; \
	for (i = 0; i < s1; i++) { \
		a = p1[i]; \
		b = p2[i]; \
		if (a < b) return -1; \
		if (a > b) return 1; \
	} \
	return 0; \
}

#define comparep_code(p1, p2, s1, s2) { \
	unicode a, b; \
	size_t i; \
	if (s1 < s2) return -1; \
	if (s1 > s2) return 1; \
	for (i = 0; i < s1; i++) { \
		a = p1[i]; \
		b = p2[i]; \
		a = toUpperUnicode(a); \
		b = toUpperUnicode(b); \
		if (a < b) return -1; \
		if (a > b) return 1; \
	} \
	return 0; \
}

static int memu_equal(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	if (s1 != s2)
		return 0;
	return memcmp(p1, p2, s1 * sizeoft(unicode)) == 0;
}

static int memu_compare(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	if (s1 < s2)
		return -1;
	if (s1 > s2)
		return 1;
	return memcmp(p1, p2, s1 * sizeoft(unicode));
}

static int memu_equalp(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	equalp_code(p1, p2, s1, s2);
}

static int memu_comparep(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	comparep_code(p1, p2, s1, s2);
}

static int memu1_equal(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	equal_code(p1, p2, s1, s2);
}

static int memu1_equalp(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	equalp_code(p1, p2, s1, s2);
}

static int memu1_compare(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	compare_code(p1, p2, s1, s2);
}

static int memu1_comparep(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	comparep_code(p1, p2, s1, s2);
}


/*
 *  strvect
 */
void strvect_alloc(LocalRoot local, addr *ret, size_t len)
{
	addr pos;

	alloc_body(local, &pos, LISPTYPE_STRING, StringBodyLength(len));
	SetStringSize(pos, len);
	*ret = pos;
}
void strvect_local(LocalRoot local, addr *ret, size_t len)
{
	addr pos;

	Check(local == NULL, "local error");
	local_body(local, &pos, LISPTYPE_STRING, StringBodyLength(len));
	SetStringSize(pos, len);
	*ret = pos;
}
void strvect_heap(addr *ret, size_t len)
{
	addr pos;

	heap_body(&pos, LISPTYPE_STRING, StringBodyLength(len));
	SetStringSize(pos, len);
	*ret = pos;
}

void strvect_copy_alloc(LocalRoot local, addr *ret, addr value)
{
	addr pos;
	unicode *dst;
	const unicode *src;
	size_t size;

	CheckType(value, LISPTYPE_STRING);
	/* source */
	GetStringSize(value, &size);
	GetStringUnicode(value, (const unicode **)&src);
	/* destination */
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&dst);
	/* copy */
	memcpy(dst, src, sizeoft(unicode) * size);
	/* result */
	*ret = pos;
}
void strvect_copy_local(LocalRoot local, addr *ret, addr value)
{
	CheckLocal(local);
	strvect_copy_alloc(local, ret, value);
}
void strvect_copy_heap(addr *ret, addr value)
{
	strvect_copy_alloc(NULL, ret, value);
}

int strvect_character_alloc_(LocalRoot local, addr *ret, addr pos)
{
	unicode c;
	GetCharacter(pos, &c);
	return strvect_sizeu_alloc_(local, ret, &c, 1);
}
int strvect_character_local_(LocalRoot local, addr *ret, addr pos)
{
	unicode c;
	Check(local == NULL, "local error");
	GetCharacter(pos, &c);
	return strvect_sizeu_local_(local, ret, &c, 1);
}
int strvect_character_heap_(addr *ret, addr pos)
{
	unicode c;
	GetCharacter(pos, &c);
	return strvect_sizeu_heap_(ret, &c, 1);
}

void strvect_length(addr pos, size_t *ret)
{
	Check(GetType(pos) != LISPTYPE_STRING, "type left error");
	GetStringSize(pos, ret);
}

void strvect_posbodylen(addr pos, const unicode **body, size_t *len)
{
	Check(GetType(pos) != LISPTYPE_STRING, "type error");
	GetStringSize(pos, len);
	GetStringUnicode(pos, body);
}

enum CHARACTER_TYPE unicode_character_type(enum CHARACTER_TYPE type, unicode u)
{
	if (type == CHARACTER_TYPE_EMPTY) {
		return character_type(u);
	}
	if (isStandardType(u)) {
		return type;
	}
	if (isBaseType(u)) {
		if (type == CHARACTER_TYPE_STANDARD)
			return CHARACTER_TYPE_BASE;
		return type;
	}
	if (isExtendedType(u)) {
		if (type != CHARACTER_TYPE_INVALID && type != CHARACTER_TYPE_EXTENDED)
			return CHARACTER_TYPE_EXTENDED;
		return type;
	}
	return CHARACTER_TYPE_INVALID;
}

int strvect_character_type_(addr pos, enum CHARACTER_TYPE *ret)
{
	enum CHARACTER_TYPE type;
	const unicode *body;
	size_t i, size;

	strvect_posbodylen(pos, &body, &size);
	type = CHARACTER_TYPE_EMPTY;
	for (i = 0; i < size; i++) {
		type = unicode_character_type(type, body[i]);
		if (type == CHARACTER_TYPE_INVALID)
			return fmte_("Invalid character code.", NULL);
	}

	return Result(ret, type);
}

int strvectp(addr pos)
{
	return GetType(pos) == LISPTYPE_STRING;
}

int strvect_base_p_(addr pos, int *ret)
{
	enum CHARACTER_TYPE type;

	if (! strvectp(pos))
		return Result(ret, 0);
	Return(strvect_character_type_(pos, &type));
	switch (type) {
		case CHARACTER_TYPE_EMPTY:
		case CHARACTER_TYPE_STANDARD:
		case CHARACTER_TYPE_BASE:
			return Result(ret, 1);

		default:
			return Result(ret, 0);
	}
}

int strvect_simple_p(addr pos)
{
	CheckType(pos, LISPTYPE_STRING);
	return 0;
}

void strvect_char_alloc(LocalRoot local, addr *ret, const char *arg)
{
	addr pos;
	unicode *destroy;
	size_t size, i;

	size = strlen(arg);
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = (unicode)arg[i];
	*ret = pos;
}
void strvect_char_local(LocalRoot local, addr *ret, const char *arg)
{
	Check(local == NULL, "local error");
	strvect_char_alloc(local, ret, arg);
}
void strvect_char_heap(addr *ret, const char *arg)
{
	strvect_char_alloc(NULL, ret, arg);
}

addr stringh(const char *arg) /* for debug */
{
	addr pos;
	strvect_char_heap(&pos, arg);
	return pos;
}

int strvect_sizeu_alloc_(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	addr pos;
	unicode *destroy;

	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	memcpy(destroy, arg, sizeoft(unicode) * size);
	return Result(ret, pos);
}
int strvect_sizeu_local_(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	Check(local == NULL, "local error");
	return strvect_sizeu_alloc_(local, ret, arg, size);
}
int strvect_sizeu_heap_(addr *ret, const unicode *arg, size_t size)
{
	return strvect_sizeu_alloc_(NULL, ret, arg, size);
}


/*
 *  strvect_equal
 */
int strvect_equal_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_equal(body, right, size1, size2);
}

int strvect_equalp_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_equalp(body, right, size1, size2);
}

int strvect_equal_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_equal(body1, (const byte *)body2, size1, size2);
}

int strvect_equalp_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_equalp(body1, (const byte *)body2, size1, size2);
}

int strvect_equal(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_equal_binary(left, body, size);
}

int strvect_equalp(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_equalp_binary(left, body, size);
}

int strvect_character_equal(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type right error");
	strvect_posbodylen(left, &body, &size);

	return size == 1 && body[0] == RefCharacter(right);
}

int strvect_character_equalp(addr left, addr right)
{
	const unicode *body;
	unicode a, b;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type right error");
	strvect_posbodylen(left, &body, &size);
	if (size != 1)
		return 0;
	a = body[0];
	GetCharacter(right, &b);

	return toUpperUnicode(a) == toUpperUnicode(b);
}


/*
 *  strvect_compare
 */
int strvect_compare_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_compare(body, right, size1, size2);
}

int strvect_comparep_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_comparep(body, right, size1, size2);
}

int strvect_compare_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_compare(body1, (const byte *)body2, size1, size2);
}

int strvect_comparep_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_comparep(body1, (const byte *)body2, size1, size2);
}

int strvect_compare(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_compare_binary(left, body, size);
}

int strvect_comparep(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_comparep_binary(left, body, size);
}

int strvect_designator_equal_char(addr left, const char *right)
{
	if (symbolp(left))
		GetNameSymbol(left, &left);
	if (characterp(left))
		return character_equal_char(left, right);
	if (strvectp(left))
		return strvect_equal_char(left, right);

	return 0;
}

int strvect_designator_equalp_char(addr left, const char *right)
{
	if (symbolp(left))
		GetNameSymbol(left, &left);
	if (characterp(left))
		return character_equalp_char(left, right);
	if (strvectp(left))
		return strvect_equalp_char(left, right);

	return 0;
}


/*
 *  getc/setc
 */
void strvect_getc(addr pos, size_t index, unicode *c)
{
	const unicode *body;
#ifdef LISP_DEBUG
	size_t size;
#endif

	Check(GetType(pos) != LISPTYPE_STRING, "type error");
#ifdef LISP_DEBUG
	strvect_length(pos, &size);
	Check(size <= index, "size error");
#endif
	GetStringUnicode(pos, &body);
	*c =  body[index];
}

void strvect_setc_unsafe(addr pos, size_t index, unicode c)
{
	unicode *destroy;
#ifdef LISP_DEBUG
	size_t size;
#endif

	Check(GetType(pos) != LISPTYPE_STRING, "type error");
#ifdef LISP_DEBUG
	strvect_length(pos, &size);
	Check(size <= index, "size error");
#endif
	GetStringUnicode(pos, (const unicode **)&destroy);
	destroy[index] = c;
}

int strvect_setc_(addr pos, size_t index, unicode c)
{
	unicode *destroy;
#ifdef LISP_DEBUG
	size_t size;
#endif

	Check(GetType(pos) != LISPTYPE_STRING, "type error");
#ifdef LISP_DEBUG
	strvect_length(pos, &size);
	Check(size <= index, "size error");
#endif

	if (character_type(c) == CHARACTER_TYPE_INVALID)
		return fmte_("Invalid character code.", NULL);
	GetStringUnicode(pos, (const unicode **)&destroy);
	destroy[index] = c;

	return 0;
}

int strvect_setall_(addr pos, unicode c)
{
	unicode *destroy;
	size_t size, i;

	strvect_length(pos, &size);
	if (size == 0)
		return 0;
	if (character_type(c) == CHARACTER_TYPE_INVALID)
		return fmte_("Invalid character code.", NULL);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = c;

	return 0;
}

int strvect_get_(LocalRoot local, addr pos, size_t index, addr *ret)
{
	unicode c;
	size_t size;

	CheckType(pos, LISPTYPE_STRING);
	strvect_length(pos, &size);
	if (size <= index) {
		*ret = 0;
		return fmte_("Out of range ~S.", intsizeh(index), NULL);
	}
	strvect_getc(pos, index, &c);
	character_alloc(local, ret, c);

	return 0;
}

int strvect_aref_(LocalRoot local, addr pos, addr args, addr *ret)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_STRING);
	if (! consp(args)) {
		*ret = 0;
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	}
	GetCons(args, &arg, &args);
	if (args != Nil) {
		*ret = 0;
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	}
	if (GetIndex_integer(arg, &index)) {
		*ret = 0;
		return fmte_("Invalid index arg ~S.", arg, NULL);
	}

	return strvect_get_(local, pos, index, ret);
}

int strvect_set_(addr pos, size_t index, addr value)
{
	size_t size;

	CheckType(pos, LISPTYPE_STRING);
	if (! characterp(value))
		return fmte_("SETF arg ~S must be a character type.", value, NULL);
	strvect_length(pos, &size);
	if (size <= index)
		return fmte_("Out of range ~S.", intsizeh(index), NULL);

	return strvect_setc_(pos, index, RefCharacter(value));
}

int strvect_setf_aref_(addr pos, addr args, addr value)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_STRING);
	if (GetStatusReadOnly(pos))
		return fmte_("The object ~S is constant.", pos, NULL);
	if (! consp(args))
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	GetCons(args, &arg, &args);
	if (args != Nil)
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	if (GetIndex_integer(arg, &index))
		return fmte_("Invalid index arg ~S.", arg, NULL);

	return strvect_set_(pos, index, value);
}

int strvect_fill_(addr pos, addr item, addr start, addr end)
{
	size_t index1, index2;
	unicode c;

	/* argument */
	if (! characterp(item))
		return fmte_("FILL tem ~S must be a character type.", item, NULL);
	GetCharacter(item, &c);
	strvect_length(pos, &index1);
	Return(size_start_end_sequence_(start, end, index1, &index1, &index2, NULL));

	/* fill */
	for (; index1 < index2; index1++)
		strvect_setc_unsafe(pos, index1, c);

	return 0;
}

int strvect_subseq_alloc_(LocalRoot local, addr *ret, addr pos, size_t x, size_t y)
{
	unicode *data1;
	const unicode *data2;
	addr root;
	size_t diff;

	Check(y < x, "index error");
	diff = y - x;
	strvect_alloc(local, &root, diff);
	GetStringUnicode(root, &data1);
	GetStringUnicode(pos, &data2);
	memcpy(data1, data2 + x, diff * sizeoft(unicode));

	return Result(ret, root);
}

int strvect_subseq_index_(addr *ret, addr pos, size_t index1, size_t index2)
{
	return strvect_subseq_alloc_(NULL, ret, pos, index1, index2);
}

int strvect_subseq_(addr *ret, addr pos, addr start, addr end)
{
	size_t index1, index2;

	strvect_length(pos, &index1);
	Return(size_start_end_sequence_(start, end, index1, &index1, &index2, NULL));
	return strvect_subseq_index_(ret, pos, index1, index2);
}

int strvect_setget_(addr pos1, size_t index1, addr pos2, size_t index2)
{
	unicode value;

	strvect_getc(pos2, index2, &value);
	return strvect_setc_(pos1, index1, value);
}

int strvect_reverse_(LocalRoot local, addr *ret, addr pos)
{
	unicode c;
	addr one;
	size_t size, x, y;

	strvect_length(pos, &size);
	strvect_alloc(local, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		strvect_getc(pos, x, &c);
		Return(strvect_setc_(one, y, c));
	}

	return Result(ret, one);
}

int strvect_nreverse_(addr *ret, addr pos)
{
	unicode a, b;
	size_t size, x, y;

	strvect_length(pos, &size);
	if (size <= 1)
		return 0;
	x = 0;
	y = size - 1;
	while (x < y) {
		strvect_getc(pos, x, &a);
		strvect_getc(pos, y, &b);
		Return(strvect_setc_(pos, x, b));
		Return(strvect_setc_(pos, y, a));
		x++;
		y--;
	}

	return Result(ret, pos);
}


/*
 *  make
 */
int strvect_char1_heap_(addr *ret, const char *arg, unicode c)
{
	addr pos;
	unicode *destroy;
	size_t size, i;

	size = strlen(arg);
	strvect_heap(&pos, size + 1ULL);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = (unicode)arg[i];
	destroy[i] = c;
	return Result(ret, pos);
}

int strvect_size1_heap_(addr *ret, const char *arg, size_t size)
{
	addr pos;
	unicode *destroy;
	size_t i;

	strvect_alloc(NULL, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = (unicode)arg[i];
	return Result(ret, pos);
}


/************************************************************
 *  subtypep.c
 ************************************************************/

/*
 *  result
 */
static void subtypep_result_keyword(SubtypepResult value, addr *ret)
{
	switch (value) {
		case SUBTYPEP_INCLUDE:
			GetConst(SYSTEM_INCLUDE, ret);
			break;

		case SUBTYPEP_EXCLUDE:
			GetConst(SYSTEM_EXCLUDE, ret);
			break;

		case SUBTYPEP_FALSE:
			GetConst(SYSTEM_FALSE, ret);
			break;

		case SUBTYPEP_INVALID:
		default:
			GetConst(SYSTEM_INVALID, ret);
			break;
	}
}

static void subtypep_result_values(SubtypepResult value, int *ret, int *validp)
{
	int x, y;

	switch (value) {
		case SUBTYPEP_INCLUDE:
			x = 1; y = 1;
			break;

		case SUBTYPEP_FALSE:
		case SUBTYPEP_EXCLUDE:
			x = 0; y = 1;
			break;

		case SUBTYPEP_INVALID:
		default:
			x = 0; y = 0;
			break;
	}

	*ret = x;
	if (validp)
		*validp = y;
}


/*
 *  subtypep-atomic
 */
static int subtypep_parse_throw_(Execute ptr, LocalHold hold,
		addr x, addr y, addr env, addr *rx, addr *ry)
{
	localhold_set(hold, 0, x);
	localhold_set(hold, 1, y);
	localhold_set(hold, 2, env);

	Return(parse_type_(ptr, rx, x, env));
	localhold_set(hold, 0, x);
	Return(parse_type_(ptr, ry, y, env));
	localhold_set(hold, 1, y);

	return 0;
}

static int subtypep_extend_atomic_(Execute ptr,
		addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_throw_(ptr, hold, x, y, env, &x, &y));
	Return(subtypep_table_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}


/*
 *  subtypep-atomic-not
 */
static int subtypep_parse_optimize_(Execute ptr, LocalHold hold,
		addr x, addr y, addr env, addr *rx, addr *ry)
{
	LocalRoot local;

	localhold_set(hold, 0, x);
	localhold_set(hold, 1, y);
	localhold_set(hold, 2, env);

	Return(parse_type_(ptr, &x, x, env));
	localhold_set(hold, 0, x);
	Return(parse_type_(ptr, &y, y, env));
	localhold_set(hold, 1, y);

	local = ptr->local;
	Return(type_optimize_throw_heap_(local, x, rx));
	Return(type_optimize_throw_heap_(local, y, ry));

	return 0;
}

static int subtypep_extend_atomic_not_(Execute ptr,
		addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_optimize_(ptr, hold, x, y, env, &x, &y));
	Return(subtypep_atomic_not_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}


/*
 *  subtypep-compound
 */
static int subtypep_extend_compound_(Execute ptr,
		addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_optimize_(ptr, hold, x, y, env, &x, &y));
	Return(subtypep_compound_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}


/*
 *  subtypep-force-number
 */
static int subtypep_parse_force_number_(Execute ptr, LocalHold hold,
		addr x, addr y, addr env, addr *rx, addr *ry)
{
	LocalRoot local;

	localhold_set(hold, 0, x);
	localhold_set(hold, 1, y);
	localhold_set(hold, 2, env);

	Return(parse_type_(ptr, &x, x, env));
	localhold_set(hold, 0, x);
	Return(parse_type_(ptr, &y, y, env));
	localhold_set(hold, 1, y);

	local = ptr->local;
	Return(type_subtypep_throw_heap_(local, x, rx));
	Return(type_subtypep_throw_heap_(local, y, ry));

	return 0;
}

static int subtypep_extend_force_number_(Execute ptr,
		addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_force_number_(ptr, hold, x, y, env, &x, &y));
	Return(subtypep_compound_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}


/*
 *  subtypep-normal
 */
static int subtypep_parse_normal_type_(Execute ptr, addr x, addr env, addr *ret)
{
	LocalRoot local;

	local = ptr->local;
	Return(parse_type_(ptr, &x, x, env));
	if (subtypep_number_p(x)) {
		Return(type_subtypep_throw_heap_(local, x, &x));
	}
	else {
		Return(type_optimize_throw_heap_(local, x, &x));
	}

	return Result(ret, x);
}

static int subtypep_parse_normal_(Execute ptr, LocalHold hold,
		addr x, addr y, addr env, addr *rx, addr *ry)
{

	localhold_set(hold, 0, x);
	localhold_set(hold, 1, y);
	localhold_set(hold, 2, env);

	Return(subtypep_parse_normal_type_(ptr, x, env, rx));
	localhold_set(hold, 0, *rx);
	Return(subtypep_parse_normal_type_(ptr, y, env, ry));
	localhold_set(hold, 1, *ry);

	return 0;
}

static int subtypep_extend_normal_(Execute ptr,
		addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_normal_(ptr, hold, x, y, env, &x, &y));
	Return(subtypep_compound_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}


/*
 *  system:subtypep!
 */
static int subtypep_extend_value_(addr pos, enum SubtypepExtend *ret)
{
	addr check;

	if (pos == Nil || pos == Unbound)
		return Result(ret, SubtypepExtend_Normal);

	/* normal */
	GetConst(SYSTEM_SUBTYPEP_NORMAL, &check);
	if (pos == check)
		return Result(ret, SubtypepExtend_Normal);

	/* atomic */
	GetConst(SYSTEM_SUBTYPEP_ATOMIC, &check);
	if (pos == check)
		return Result(ret, SubtypepExtend_Atomic);

	/* atomic-not */
	GetConst(SYSTEM_SUBTYPEP_ATOMIC_NOT, &check);
	if (pos == check)
		return Result(ret, SubtypepExtend_AtomicNot);

	/* compound */
	GetConst(SYSTEM_SUBTYPEP_COMPOUND, &check);
	if (pos == check)
		return Result(ret, SubtypepExtend_Compound);

	/* force-number */
	GetConst(SYSTEM_SUBTYPEP_FORCE_NUMBER, &check);
	if (pos == check)
		return Result(ret, SubtypepExtend_ForceNumber);

	*ret = SubtypepExtend_Normal;
	return fmte_("Invalid subtypep! type ~S.", pos, NULL);
}

static int subtypep_extend_output_(Execute ptr, addr *ret)
{
	enum SubtypepExtend value;
	addr pos;

	/* *subtypep!* */
	GetConst(SYSTEM_SUBTYPEP_VALUE, &pos);
	getspecial_local(ptr, pos, &pos);

	/* normal */
	Return(subtypep_extend_value_(pos, &value));
	if (value == SubtypepExtend_Normal) {
		GetConst(SYSTEM_SUBTYPEP_NORMAL, &pos);
	}

	return Result(ret, pos);
}

static int subtypep_extend_type_(Execute ptr, addr pos, enum SubtypepExtend *ret)
{
	if (pos != Nil)
		return subtypep_extend_value_(pos, ret);

	/* *subtypep!* */
	GetConst(SYSTEM_SUBTYPEP_VALUE, &pos);
	getspecial_local(ptr, pos, &pos);
	return subtypep_extend_value_(pos, ret);
}

static int subtypep_extend_switch_(Execute ptr, addr x, addr y, addr env,
		enum SubtypepExtend type, SubtypepResult *ret)
{
	switch (type) {
		case SubtypepExtend_Normal:
			return subtypep_extend_normal_(ptr, x, y, env, ret);

		case SubtypepExtend_Atomic:
			return subtypep_extend_atomic_(ptr, x, y, env, ret);

		case SubtypepExtend_AtomicNot:
			return subtypep_extend_atomic_not_(ptr, x, y, env, ret);

		case SubtypepExtend_Compound:
			return subtypep_extend_compound_(ptr, x, y, env, ret);

		case SubtypepExtend_ForceNumber:
			return subtypep_extend_force_number_(ptr, x, y, env, ret);

		default:
			return subtypep_extend_normal_(ptr, x, y, env, ret);
	}
}

int subtypep_extend_(Execute ptr, addr x, addr y, addr env, addr check, addr *ret)
{
	enum SubtypepExtend type;
	SubtypepResult value;

	/* output */
	if (check == T)
		return subtypep_extend_output_(ptr, ret);

	/* subtypep */
	Return(subtypep_extend_type_(ptr, check, &type));
	Return(subtypep_extend_switch_(ptr, x, y, env, type, &value));
	subtypep_result_keyword(value, ret);
	return 0;
}


/*
 *  interface
 */
int subtypep_scope_(Execute ptr, addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_normal_(ptr, hold, x, y, env, &x, &y));
	if (type_asterisk_p(y))
		return Result(ret, SUBTYPEP_INCLUDE);
	if (type_asterisk_p(x))
		return Result(ret, SUBTYPEP_FALSE);

	Return(subtypep_compound_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}

int subtypep_check_(Execute ptr, addr x, addr y, addr env, int *ret, int *validp)
{
	SubtypepResult value;
	Return(subtypep_extend_normal_(ptr, x, y, env, &value));
	subtypep_result_values(value, ret, validp);

	return 0;
}


/*
 *  initialize
 */
void init_subtypep(void)
{
	init_subtypep_table();
}


/************************************************************
 *  subtypep_andor.c
 ************************************************************/

/*
 *  reduce
 */
#define Return_subtypep_reduce_p(ptr, pos, ret, call) { \
	int __check; \
	Return(call(ptr, pos, &__check)); \
	if (__check) { \
		return Result(ret, 1); \
	} \
}

#define Return_subtypep_reduce(ptr, pos, value, ret, call) { \
	int __check; \
	Return(call(ptr, pos, value, &__check)); \
	if (__check) { \
		return Result(ret, 1); \
	} \
}

static int subtypep_reduce_p_(Execute ptr, addr pos, int *ret);
static int subtypep_reduce_(Execute ptr, addr pos, addr *value, int *ret);


/* (and ...) */
static int subtypep_reduce_vector_all_p_(Execute ptr, addr pos, int *ret)
{
	int check;
	addr vect;
	size_t size, i;

	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &vect);
		Return(subtypep_reduce_p_(ptr, vect, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and_all_p_(Execute ptr, addr pos, int *ret)
{
	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_vector_all_p_(ptr, pos, ret);
}

static int subtypep_reduce_vector_all_(Execute ptr,
		addr pos, addr *value, int *ret, enum LISPDECL make)
{
	int check;
	addr dst, vect;
	size_t size, i;
	LocalRoot local;

	local = ptr->local;
	LenArrayA4(pos, &size);
	vector4_local(local, &dst, size);

	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &vect);
		Return(subtypep_reduce_(ptr, vect, &vect, &check));
		SetArrayA4(dst, i, vect);
	}

	/* make */
	type1_local(local, make, dst, value);
	return Result(ret, 1);
}

static int subtypep_reduce_and_all_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and_all_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_vector_all_(ptr, pos, value, ret, LISPDECL_AND);
}


/* (and) -> t */
static int subtypep_reduce_and1_p_(Execute ptr, addr pos, int *ret)
{
	size_t size;

	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	return Result(ret, size == 0);
}

static int subtypep_reduce_and1_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and1_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetTypeTable(value, T);
	return Result(ret, 1);
}


/* (and type) -> type */
static int subtypep_reduce_and2_p_(Execute ptr, addr pos, int *ret)
{
	size_t size;

	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	return Result(ret, size == 1);
}

static int subtypep_reduce_and2_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and2_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	GetArrayA4(pos, 0, value);
	return Result(ret, 1);
}


/* (and ... nil ...) -> nil */
static int subtypep_reduce_find_p_(Execute ptr, addr pos,
		enum LISPDECL type, enum LISPDECL find, int *ret)
{
	addr check;
	size_t size, i;

	if (RefLispDecl(pos) != type)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &check);
		if (RefLispDecl(check) == find)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and3_p_(Execute ptr, addr pos, int *ret)
{
	return subtypep_reduce_find_p_(ptr, pos, LISPDECL_AND, LISPDECL_NIL, ret);
}

static int subtypep_reduce_and3_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and3_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetTypeTable(value, Nil);
	return Result(ret, 1);
}


/* (and ... t ...) -> (and ...)  remove t */
static int subtypep_reduce_and4_p_(Execute ptr, addr pos, int *ret)
{
	return subtypep_reduce_find_p_(ptr, pos, LISPDECL_AND, LISPDECL_T, ret);
}

static int subtypep_reduce_remove_(Execute ptr, addr pos, addr *value, int *ret,
		enum LISPDECL equal, enum LISPDECL make)
{
	int exist;
	addr check, dst;
	size_t size, count, i;
	LocalRoot local;

	/* length */
	LenArrayA4(pos, &size);
	count = 0;
	exist = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &check);
		if (RefLispDecl(check) == equal)
			exist = 1;
		else
			count++;
	}
	if (exist == 0)
		return Result(ret, 0);

	/* replace */
	local = ptr->local;
	vector4_local(local, &dst, count);
	count = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &check);
		if (RefLispDecl(check) == equal)
			continue;
		SetArrayA4(dst, count++, check);
	}
	type1_local(local, make, dst, value);

	return Result(ret, 1);
}

static int subtypep_reduce_and4_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and4_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_remove_(ptr, pos, value, ret, LISPDECL_T, LISPDECL_AND);
}


/* (and [exclude]) -> nil */
static int subtypep_reduce_and5_p_(Execute ptr, addr pos, int *ret)
{
	SubtypepResult check;
	addr left, right;
	size_t size, x, y;

	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	for (x = 0; x < size; x++) {
		GetArrayA4(pos, x, &left);
		for (y = x + 1; y < size; y++) {
			GetArrayA4(pos, y, &right);
			Return(subtypep_compound_(ptr, left, right, &check));
			if (check == SUBTYPEP_EXCLUDE)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and5_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and5_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetTypeTable(value, Nil);
	return Result(ret, 1);
}


/* (and ...) include remove */
static int subtypep_reduce_and6_index_(Execute ptr, addr pos, size_t *value, int *ret)
{
	SubtypepResult check;
	addr left, right;
	size_t size, x, y;

	LenArrayA4(pos, &size);
	if (size <= 1)
		return Result(ret, 0);

	for (x = 0; x < size; x++) {
		GetArrayA4(pos, x, &left);
		for (y = 0; y < size; y++) {
			GetArrayA4(pos, y, &right);
			if (x == y)
				continue;
			Return(subtypep_compound_(ptr, left, right, &check));
			if (check == SUBTYPEP_INCLUDE) {
				if (value)
					*value = y;
				return Result(ret, 1);
			}
		}
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and6_p_(Execute ptr, addr pos, int *ret)
{
	int check;

	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	Return(subtypep_reduce_and6_index_(ptr, pos, NULL, &check));
	return Result(ret, check);
}

static int subtypep_reduce_and6_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;
	addr dst, vect;
	size_t size, index, i, count;
	LocalRoot local;

	Return(subtypep_reduce_and6_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	index = 0;
	Return(subtypep_reduce_and6_index_(ptr, pos, &index, &check));
	if (! check)
		return Result(ret, 0);

	/* new type */
	local = ptr->local;
	vector4_local(local, &dst, size - 1ULL);
	type1_local(local, LISPDECL_AND, dst, value);
	count = 0;
	for (i = 0; i < size; i++) {
		if (index != i) {
			GetArrayA4(pos, i, &vect);
			SetArrayA4(dst, count++, vect);
		}
	}

	return Result(ret, 1);
}


/* (and ... (or ...)) */
static int subtypep_reduce_andor_p_(Execute ptr, addr pos, size_t index, int *ret)
{
	SubtypepResult check;
	addr vector, left, right;
	size_t size1, size2, x, y;

	GetArrayA4(pos, index, &vector);
	GetArrayType(vector, 0, &vector);
	LenArrayA4(vector, &size1);
	LenArrayA4(pos, &size2);

	for (x = 0; x < size1; x++) {
		GetArrayA4(vector, x, &right);
		for (y = 0; y < size2; y++) {
			if (y == index)
				continue;
			GetArrayA4(pos, y, &left);
			Return(subtypep_compound_(ptr, left, right, &check));
			if (check == SUBTYPEP_EXCLUDE)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and7_p_(Execute ptr, addr pos, int *ret)
{
	int check;
	addr vect;
	size_t size, i;

	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &vect);
		if (RefLispDecl(vect) == LISPDECL_OR) {
			Return(subtypep_reduce_andor_p_(ptr, pos, i, &check));
			if (check)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int subtypep_reduce_andor_size_(Execute ptr,
		addr pos, size_t index, size_t *rsize, int *ret)
{
	int remove;
	SubtypepResult check;
	addr vector, left, right;
	size_t count, size1, size2, x, y;

	GetArrayA4(pos, index, &vector);
	GetArrayType(vector, 0, &vector);
	LenArrayA4(vector, &size1);
	LenArrayA4(pos, &size2);

	count = 0;
	for (x = 0; x < size1; x++) {
		GetArrayA4(vector, x, &right);
		remove = 0;
		for (y = 0; y < size2; y++) {
			if (y == index)
				continue;
			GetArrayA4(pos, y, &left);
			Return(subtypep_compound_(ptr, left, right, &check));
			if (check == SUBTYPEP_EXCLUDE) {
				remove = 1;
				break;
			}
		}
		if (! remove)
			count++;
	}

	*rsize = count;
	return Result(ret, size1 != count);
}

static int subtypep_reduce_andor_set_(Execute ptr,
		addr pos, size_t index, addr dst, size_t size)
{
	int remove;
	SubtypepResult check;
	addr vector, left, right;
	size_t count, size1, size2, x, y;

	GetArrayA4(pos, index, &vector);
	GetArrayType(vector, 0, &vector);
	LenArrayA4(vector, &size1);
	LenArrayA4(pos, &size2);

	count = 0;
	for (x = 0; x < size1; x++) {
		GetArrayA4(vector, x, &right);
		remove = 0;
		for (y = 0; y < size2; y++) {
			if (y == index)
				continue;
			GetArrayA4(pos, y, &left);
			Return(subtypep_compound_(ptr, left, right, &check));
			if (check == SUBTYPEP_EXCLUDE) {
				remove = 1;
				break;
			}
		}
		if (! remove) {
			SetArrayA4(dst, count, right);
			count++;
		}
	}

	return 0;
}

static int subtypep_reduce_andor_or_(Execute ptr,
		addr pos, size_t index, addr *value, int *ret)
{
	int check;
	addr dst;
	size_t size;
	LocalRoot local;

	Return(subtypep_reduce_andor_size_(ptr, pos, index, &size, &check));
	if (! check)
		return Result(ret, 0);

	/* value */
	local = ptr->local;
	vector4_local(local, &dst, size);
	type1_local(local, LISPDECL_OR, dst, value);
	Return(subtypep_reduce_andor_set_(ptr, pos, index, dst, size));

	return Result(ret, 1);
}

static int subtypep_reduce_andor_(Execute ptr, addr pos, addr *value)
{
	int check;
	addr dst, x, y;
	size_t size, i;
	LocalRoot local;

	/* value */
	local = ptr->local;
	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	vector4_local(local, &dst, size);
	type1_local(local, LISPDECL_AND, dst, value);

	/* remove */
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &x);
		if (RefLispDecl(x) == LISPDECL_OR) {
			Return(subtypep_reduce_andor_or_(ptr, pos, i, &y, &check));
			if (check)
				x = y;
		}
		SetArrayA4(dst, i, x);
	}

	return 0;
}

static int subtypep_reduce_and7_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and7_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	*ret = 1;
	return subtypep_reduce_andor_(ptr, pos, value);
}


/* and */
static int subtypep_reduce_and_p_(Execute ptr, addr pos, int *ret)
{
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and_all_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and1_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and2_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and3_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and4_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and5_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and6_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and7_p_);

	return Result(ret, 0);
}

static int subtypep_reduce_and_(Execute ptr, addr pos, addr *value, int *ret)
{
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and_all_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and1_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and2_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and3_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and4_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and5_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and6_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and7_);

	return Result(ret, 0);
}


/* (or ...) */
static int subtypep_reduce_or_all_p_(Execute ptr, addr pos, int *ret)
{
	if (RefLispDecl(pos) != LISPDECL_OR)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_vector_all_p_(ptr, pos, ret);
}

static int subtypep_reduce_or_all_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_or_all_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_vector_all_(ptr, pos, value, ret, LISPDECL_OR);
}


/* (or) -> nil */
static int subtypep_reduce_or1_p_(Execute ptr, addr pos, int *ret)
{
	size_t size;

	if (RefLispDecl(pos) != LISPDECL_OR)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	return Result(ret, size == 0);
}

static int subtypep_reduce_or1_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_or1_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetTypeTable(value, Nil);
	return Result(ret, 1);
}


/* (or type) -> type */
static int subtypep_reduce_or2_p_(Execute ptr, addr pos, int *ret)
{
	size_t size;

	if (RefLispDecl(pos) != LISPDECL_OR)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	return Result(ret, size == 1);
}

static int subtypep_reduce_or2_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_or2_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	GetArrayA4(pos, 0, value);
	return Result(ret, 1);
}


/* (or ... t ...) -> t */
static int subtypep_reduce_or3_p_(Execute ptr, addr pos, int *ret)
{
	return subtypep_reduce_find_p_(ptr, pos, LISPDECL_OR, LISPDECL_T, ret);
}

static int subtypep_reduce_or3_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_or3_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetTypeTable(value, T);
	return Result(ret, 1);
}


/* (or ... nil ...) -> (or ...)  remove nil */
static int subtypep_reduce_or4_p_(Execute ptr, addr pos, int *ret)
{
	return subtypep_reduce_find_p_(ptr, pos, LISPDECL_OR, LISPDECL_NIL, ret);
}

static int subtypep_reduce_or4_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_or4_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_remove_(ptr, pos, value, ret, LISPDECL_NIL, LISPDECL_OR);
}


/* or */
static int subtypep_reduce_or_p_(Execute ptr, addr pos, int *ret)
{
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or_all_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or1_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or2_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or3_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or4_p_);

	return Result(ret, 0);
}

static int subtypep_reduce_or_(Execute ptr, addr pos, addr *value, int *ret)
{
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_or_all_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_or1_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_or2_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_or3_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_or4_);

	return Result(ret, 0);
}


/* reduce interface */
static int subtypep_reduce_p_(Execute ptr, addr pos, int *ret)
{
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or_p_);

	return Result(ret, 0);
}

static int subtypep_reduce_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check, loop, update;
	addr x;

	update = 0;
start:
	loop = 0;

	/* and */
	Return(subtypep_reduce_and_(ptr, pos, &x, &check));
	if (check) {
		loop = 1;
		pos = x;
	}

	/* or */
	Return(subtypep_reduce_or_(ptr, pos, &x, &check));
	if (check) {
		loop = 1;
		pos = x;
	}

	/* result */
	if (loop) {
		update = 1;
		goto start;
	}
	*value = pos;
	return Result(ret, update);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  and      -        any      any
 *  or       -        all      all
 *  -        and      all      any
 *  -        or       any      all
 *
 *  and      and      any/all  any/any
 *  and      or       any/any  any/all
 *  or       and      all/all  all/any
 *  or       or       all/any  all/all
 */
/*
 *  include
 */
static int subtypep_any_type_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(x, &size);
	include = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(x, i, &pos);
		Return(subtypep_compound_(ptr, pos, y, &result));
		if (result == SUBTYPEP_INCLUDE)
			include = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_type_any_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(y, &size);
	include = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &pos);
		Return(subtypep_compound_(ptr, x, pos, &result));
		if (result == SUBTYPEP_INCLUDE)
			include = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_all_type_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(x, &size);
	include = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(x, i, &pos);
		Return(subtypep_compound_(ptr, pos, y, &result));
		if (result != SUBTYPEP_INCLUDE)
			include = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_type_all_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(y, &size);
	include = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &pos);
		Return(subtypep_compound_(ptr, x, pos, &result));
		if (result != SUBTYPEP_INCLUDE)
			include = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}


/*
 *  exclude
 */
static int subtypep_any_type_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(x, &size);
	exclude = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(x, i, &pos);
		Return(subtypep_compound_(ptr, pos, y, &result));
		if (result == SUBTYPEP_EXCLUDE)
			exclude = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_type_any_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(y, &size);
	exclude = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &pos);
		Return(subtypep_compound_(ptr, x, pos, &result));
		if (result == SUBTYPEP_EXCLUDE)
			exclude = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_all_type_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(x, &size);
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(x, i, &pos);
		Return(subtypep_compound_(ptr, pos, y, &result));
		if (result != SUBTYPEP_EXCLUDE)
			exclude = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_type_all_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(y, &size);
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &pos);
		Return(subtypep_compound_(ptr, x, pos, &result));
		if (result != SUBTYPEP_EXCLUDE)
			exclude = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}


/*
 *  any/all include
 */
static int subtypep_any_any_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(y, &size);
	include = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &value);
		Return(subtypep_any_type_include_(ptr, x, value, &result));
		if (result == SUBTYPEP_INCLUDE)
			include = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_all_any_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(x, &size);
	include = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(x, i, &value);
		Return(subtypep_type_any_include_(ptr, value, y, &result));
		if (result != SUBTYPEP_INCLUDE)
			include = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_any_all_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(y, &size);
	include = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &value);
		Return(subtypep_any_type_include_(ptr, x, value, &result));
		if (result != SUBTYPEP_INCLUDE)
			include = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_all_all_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(y, &size);
	include = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &value);
		Return(subtypep_all_type_include_(ptr, x, value, &result));
		if (result != SUBTYPEP_INCLUDE)
			include = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}


/*
 *  any/all include
 */
static int subtypep_any_any_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(y, &size);
	exclude = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &value);
		Return(subtypep_any_type_exclude_(ptr, x, value, &result));
		if (result == SUBTYPEP_EXCLUDE)
			exclude = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_all_any_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(x, &size);
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(x, i, &value);
		Return(subtypep_type_any_exclude_(ptr, value, y, &result));
		if (result != SUBTYPEP_EXCLUDE)
			exclude = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_any_all_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(y, &size);
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &value);
		Return(subtypep_any_type_exclude_(ptr, x, value, &result));
		if (result != SUBTYPEP_EXCLUDE)
			exclude = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_all_all_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(y, &size);
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &value);
		Return(subtypep_all_type_exclude_(ptr, x, value, &result));
		if (result != SUBTYPEP_EXCLUDE)
			exclude = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  and      and      any/all  any/any
 */
static int subtypep_and_and_vector_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult result;

	/* include */
	Return(subtypep_any_all_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_any_any_exclude_(ptr, x, y, ret);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  and      or       any/any  any/all
 */
static int subtypep_and_or_vector_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult result;

	/* include */
	Return(subtypep_any_any_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_any_all_exclude_(ptr, x, y, ret);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  or       and      all/all  all/any
 */
static int subtypep_or_and_vector_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult result;

	/* include */
	Return(subtypep_all_all_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_all_any_exclude_(ptr, x, y, ret);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  or       or       all/any  all/all
 */
static int subtypep_or_or_vector_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult result;

	/* include */
	Return(subtypep_all_any_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_all_all_exclude_(ptr, x, y, ret);
}


/*
 *  interface
 */
static int subtypep_vector_call_(Execute ptr,
		addr x, addr y, SubtypepResult *ret,
		int (*call)(Execute, addr, addr, SubtypepResult *))
{
	int check;

	/* reduce */
	Return(subtypep_reduce_(ptr, x, &x, &check));
	if (check)
		return subtypep_compound_(ptr, x, y, ret);
	Return(subtypep_reduce_(ptr, y, &y, &check));
	if (check)
		return subtypep_compound_(ptr, x, y, ret);

	/* vector */
	GetArrayType(x, 0, &x);
	GetArrayType(y, 0, &y);
	return (*call)(ptr, x, y, ret);
}

int subtypep_and_and_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_vector_call_(ptr, x, y, ret, subtypep_and_and_vector_);
}

int subtypep_and_or_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_vector_call_(ptr, x, y, ret, subtypep_and_or_vector_);
}

int subtypep_or_and_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_vector_call_(ptr, x, y, ret, subtypep_or_and_vector_);
}

int subtypep_or_or_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_vector_call_(ptr, x, y, ret, subtypep_or_or_vector_);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  and      -        any      any
 */
int subtypep_and_type_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;
	SubtypepResult result;

	/* reduce */
	Return(subtypep_reduce_(ptr, x, &x, &check));
	if (check)
		return subtypep_compound_(ptr, x, y, ret);
	GetArrayType(x, 0, &x);

	/* include */
	Return(subtypep_any_type_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_any_type_exclude_(ptr, x, y, ret);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  or       -        all      all
 */
int subtypep_or_type_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;
	SubtypepResult result;

	/* reduce */
	Return(subtypep_reduce_(ptr, x, &x, &check));
	if (check)
		return subtypep_compound_(ptr, x, y, ret);
	GetArrayType(x, 0, &x);

	/* include */
	Return(subtypep_all_type_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_all_type_exclude_(ptr, x, y, ret);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  -        and      all      any
 */
int subtypep_type_and_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;
	SubtypepResult result;

	/* reduce */
	Return(subtypep_reduce_(ptr, y, &y, &check));
	if (check)
		return subtypep_compound_(ptr, x, y, ret);
	GetArrayType(y, 0, &y);

	/* include */
	Return(subtypep_type_all_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_type_any_exclude_(ptr, x, y, ret);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  -        or       any      all
 */
int subtypep_type_or_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;
	SubtypepResult result;

	/* reduce */
	Return(subtypep_reduce_(ptr, y, &y, &check));
	if (check)
		return subtypep_compound_(ptr, x, y, ret);
	GetArrayType(y, 0, &y);

	/* include */
	Return(subtypep_type_any_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_type_all_exclude_(ptr, x, y, ret);
}


/************************************************************
 *  subtypep_atomic.c
 ************************************************************/

int ReturnReverse(SubtypepResult *ret, SubtypepResult check)
{
	switch (check) {
		case SUBTYPEP_INCLUDE:
			return ReturnFalse(ret);

		default:
			return Result(ret, check);
	}
}


/*
 *  type
 */
int subtypep_call_invalid_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return ReturnInvalid(ret);
}


/*
 *  clos
 */
int subtypep_call_clos_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;

	if (RefLispDecl(x) != LISPDECL_CLOS)
		return ReturnExclude(ret);
	GetArrayType(x, 0, &x);
	GetArrayType(y, 0, &y);
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);

	Return(clos_subclass_p_(x, y, &check));
	if (check)
		return ReturnInclude(ret);

	return ReturnFalse(ret);
}


/*
 *  asterisk
 */
int subtypep_call_asterisk_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_call_t_(ptr, x, y, ret);
}


/*
 *  nil
 */
int subtypep_call_nil_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	if (RefLispDecl(x) == LISPDECL_NIL)
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}


/*
 *  t
 */
int subtypep_call_t_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return ReturnInclude(ret);
}


/*
 *  null
 */
int subtypep_call_null_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NULL:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  eqltype
 */
int subtypep_call_eqltype_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	if (RefLispDecl(x) == RefLispDecl(y))
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}


/*
 *  symbol
 */
int subtypep_call_symbol_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NULL:
		case LISPDECL_SYMBOL:
		case LISPDECL_KEYWORD:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  keyword
 */
int subtypep_call_keyword_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_KEYWORD:
			return ReturnInclude(ret);

		case LISPDECL_SYMBOL:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  pathname
 */
int subtypep_call_pathname_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_PATHNAME:
		case LISPDECL_LOGICAL_PATHNAME:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  logical-pathname
 */
int subtypep_call_logical_pathname_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_LOGICAL_PATHNAME:
			return ReturnInclude(ret);

		case LISPDECL_PATHNAME:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  array
 */
static int subtypep_call_aa_integer_integer_(addr x, addr y, SubtypepResult *ret)
{
	size_t size1, size2;

	Return(getindex_integer_(x, &size1));
	Return(getindex_integer_(y, &size2));

	return ReturnIncludeExclude(ret, (size1 == size2));
}

static int subtypep_call_aa_vector_integer_(addr x, addr y, SubtypepResult *ret)
{
	size_t size1, size2;

	LenArrayA4(x, &size1);
	Return(getindex_integer_(y, &size2));

	return ReturnIncludeExclude(ret, (size1 == size2));
}

static int subtypep_call_aa_integer_vector_(addr x, addr y, SubtypepResult *ret)
{
	addr check;
	size_t size1, size2, i;

	Return(getindex_integer_(x, &size1));
	LenArrayA4(y, &size2);
	if (size1 != size2)
		return ReturnExclude(ret);

	for (i = 0; i < size1; i++) {
		GetArrayA4(y, i, &check);
		if (! type_asterisk_p(check))
			return ReturnFalse(ret);
	}

	return ReturnInclude(ret);
}

static int subtypep_call_aa_vector_vector_(addr x, addr y, SubtypepResult *ret)
{
	addr check1, check2;
	size_t size1, size2, i, dim1, dim2;

	LenArrayA4(x, &size1);
	LenArrayA4(y, &size2);
	if (size1 != size2)
		return ReturnExclude(ret);

	for (i = 0; i < size1; i++) {
		/* asterisk */
		GetArrayA4(y, i, &check2);
		if (type_asterisk_p(check2))
			continue;
		GetArrayA4(x, i, &check1);
		if (type_asterisk_p(check1))
			return ReturnFalse(ret);

		/* dimension */
		Return(getindex_integer_(check1, &dim1));
		Return(getindex_integer_(check2, &dim2));
		if (dim1 != dim2)
			return ReturnExclude(ret);
	}

	return ReturnInclude(ret);
}

static int subtypep_call_aa_integer_(addr x, addr y, SubtypepResult *ret)
{
	switch (GetType(x)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			return subtypep_call_aa_integer_integer_(x, y, ret);

		case LISPTYPE_VECTOR:
			return subtypep_call_aa_vector_integer_(x, y, ret);

		default:
			return ReturnInvalid(ret);
	}
}

static int subtypep_call_aa_vector_(addr x, addr y, SubtypepResult *ret)
{
	switch (GetType(x)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			return subtypep_call_aa_integer_vector_(x, y, ret);

		case LISPTYPE_VECTOR:
			return subtypep_call_aa_vector_vector_(x, y, ret);

		default:
			return ReturnInvalid(ret);
	}
}

static int subtypep_call_array_array_dimension_(addr x, addr y, SubtypepResult *ret)
{
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);

	switch (GetType(y)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			return subtypep_call_aa_integer_(x, y, ret);

		case LISPTYPE_VECTOR:
			return subtypep_call_aa_vector_(x, y, ret);

		default:
			return ReturnInvalid(ret);
	}
}

static int subtypep_call_array_array_equal_(addr x, addr y, SubtypepResult *ret)
{
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);

	if (upgraded_array0_equal(x, y))
		return ReturnInclude(ret);

	return ReturnExclude(ret);
}

static int subtypep_call_array_array_(addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult value1, value2;
	addr type1, type2, dim1, dim2;

	GetArrayType(x, 0, &type1);
	GetArrayType(y, 0, &type2);
	GetArrayType(x, 1, &dim1);
	GetArrayType(y, 1, &dim2);

	/* subtypep */
	Return(subtypep_call_array_array_equal_(type1, type2, &value1));
	ReturnSecondThrow(ret, value1);
	Return(subtypep_call_array_array_dimension_(dim1, dim2, &value2));
	ReturnSecondThrow(ret, value2);

	/* include */
	if (value1 == SUBTYPEP_INCLUDE && value2 == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* reverse */
	if (value1 == SUBTYPEP_INCLUDE) {
		Return(subtypep_call_array_array_equal_(type2, type1, &value1));
		return ReturnSecondValue(ret, value1);
	}
	if (value2 == SUBTYPEP_INCLUDE) {
		Return(subtypep_call_array_array_dimension_(dim2, dim1, &value2));
		return ReturnSecondValue(ret, value2);
	}

	return ReturnFalse(ret);
}

int subtypep_call_array_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_ARRAY:
		case LISPDECL_SIMPLE_ARRAY:
			return subtypep_call_array_array_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  simple-array
 */
static int subtypep_call_simple_array_array_(addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult check;
	Return(subtypep_call_array_array_(y, x, &check));
	return ReturnReverse(ret, check);
}

int subtypep_call_simple_array_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_SIMPLE_ARRAY:
			return subtypep_call_array_array_(x, y, ret);

		case LISPDECL_ARRAY:
			return subtypep_call_simple_array_array_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  character
 */
int subtypep_call_character_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_CHARACTER:
		case LISPDECL_BASE_CHAR:
		case LISPDECL_STANDARD_CHAR:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  base-char
 */
int subtypep_call_base_char_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_BASE_CHAR:
		case LISPDECL_STANDARD_CHAR:
			return ReturnInclude(ret);

		case LISPDECL_CHARACTER:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  standard-char
 */
int subtypep_call_standard_char_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_STANDARD_CHAR:
			return ReturnInclude(ret);

		case LISPDECL_BASE_CHAR:
		case LISPDECL_CHARACTER:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  real range
 */
static int subtypep_real_less_(addr x, addr y, int *ret)
{
	if (! range_any_right_p(x))
		return Result(ret, 0);

	return range_right_right_less_equal_(x, y, ret);
}

static int subtypep_real_greater_(addr x, addr y, int *ret)
{
	if (! range_left_any_p(x))
		return Result(ret, 0);

	return range_left_left_greater_equal_(x, y, ret);
}

static int subtypep_real_range_(addr x, addr y, int *ret)
{
	if (! range_between_p(x))
		return Result(ret, 0);

	return range_in_between_(x, y, ret);
}

static int subtypep_realcheck_(addr x, addr y, int *ret)
{
	addr check1, check2;

	if (range_asterisk_p(y))
		return Result(ret, 1);
	if (range_asterisk_p(x))
		return Result(ret, 0);

	GetArrayType(y, 0, &check1);
	if (type_asterisk_p(check1))
		return subtypep_real_less_(x, y, ret);

	GetArrayType(y, 2, &check2);
	if (type_asterisk_p(check2))
		return subtypep_real_greater_(x, y, ret);

	return subtypep_real_range_(x, y, ret);
}

static int realexclude_left_(addr x, addr y, int *ret)
{
	if (! range_any_right_p(x))
		return Result(ret, 0);
	if (! range_left_any_p(y))
		return Result(ret, 0);

	return range_right_left_less_(x, y, ret);
}

static int realexclude_right_(addr x, addr y, int *ret)
{
	if (! range_left_any_p(x))
		return Result(ret, 0);
	if (! range_any_right_p(y))
		return Result(ret, 0);

	return range_left_right_greater_(x, y, ret);
}

static int subtypep_realexlucde_(addr x, addr y, int *ret)
{
	int check;

	Return(realexclude_left_(x, y, &check));
	if (check)
		return Result(ret, 1);

	return realexclude_right_(x, y, ret);
}

static int subtypep_realparameter_(addr x, addr y, SubtypepResult *ret)
{
	int check;

	Return(subtypep_realcheck_(x, y, &check));
	if (check)
		return ReturnInclude(ret);

	Return(subtypep_realexlucde_(x, y, &check));
	if (check)
		return ReturnExclude(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_realexclude_(addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult check;
	Return(subtypep_realparameter_(y, x, &check));
	return ReturnReverse(ret, check);
}


/*
 *  integer
 */
int subtypep_call_integer_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_RATIONAL:
		case LISPDECL_REAL:
			return subtypep_realexclude_(x, y, ret);

		case LISPDECL_INTEGER:
			return subtypep_realparameter_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  rational
 */
static int subtypep_call_rational_ratio_(addr y, SubtypepResult *ret)
{
	if (range_asterisk_p(y))
		return ReturnInclude(ret);
	else
		return ReturnFalse(ret);
}

int subtypep_call_rational_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_RATIO:
			return subtypep_call_rational_ratio_(y, ret);

		case LISPDECL_REAL:
			return subtypep_realexclude_(x, y, ret);

		case LISPDECL_RATIONAL:
		case LISPDECL_INTEGER:
			return subtypep_realparameter_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  real
 */
int subtypep_call_real_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	enum LISPDECL type;

	GetLispDecl(x, &type);
	switch (type) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_RATIO:
			return subtypep_call_rational_ratio_(y, ret);

		case LISPDECL_INTEGER:
		case LISPDECL_RATIONAL:
		case LISPDECL_REAL:
		case LISPDECL_FLOAT:
		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
		case LISPDECL_SHORT_FLOAT:
			return subtypep_realparameter_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  float
 */
int subtypep_call_float_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	enum LISPDECL type;

	GetLispDecl(x, &type);
	switch (type) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_REAL:
			return subtypep_realexclude_(x, y, ret);

		case LISPDECL_FLOAT:
		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
		case LISPDECL_SHORT_FLOAT:
			return subtypep_realparameter_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  float range
 */
static int subtypep_float_type_(addr x, addr y,
		SubtypepResult *ret, enum LISPDECL check)
{
	enum LISPDECL type;

	GetLispDecl(x, &type);
	if (type == check)
		return subtypep_realparameter_(x, y, ret);

	switch (type) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_REAL:
		case LISPDECL_FLOAT:
			return subtypep_realexclude_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  short-float
 */
int subtypep_call_short_float_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_float_type_(x, y, ret, LISPDECL_SHORT_FLOAT);
}


/*
 *  single-float
 */
int subtypep_call_single_float_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_float_type_(x, y, ret, LISPDECL_SINGLE_FLOAT);
}


/*
 *  double-float
 */
int subtypep_call_double_float_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_float_type_(x, y, ret, LISPDECL_DOUBLE_FLOAT);
}


/*
 *  long-float
 */
int subtypep_call_long_float_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_float_type_(x, y, ret, LISPDECL_LONG_FLOAT);
}


/*
 *  number
 */
int subtypep_call_number_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	enum LISPDECL type;

	GetLispDecl(x, &type);
	switch (type) {
		case LISPDECL_NUMBER:
		case LISPDECL_COMPLEX:
		case LISPDECL_RATIO:
		case LISPDECL_INTEGER:
		case LISPDECL_RATIONAL:
		case LISPDECL_REAL:
		case LISPDECL_FLOAT:
		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
		case LISPDECL_SHORT_FLOAT:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  ratio
 */
int subtypep_call_ratio_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NUMBER:
		case LISPDECL_REAL:
		case LISPDECL_RATIONAL:
			return ReturnFalse(ret);

		case LISPDECL_RATIO:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  stream
 */
int subtypep_call_stream_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_STREAM:
		case LISPDECL_BROADCAST_STREAM:
		case LISPDECL_CONCATENATED_STREAM:
		case LISPDECL_ECHO_STREAM:
		case LISPDECL_FILE_STREAM:
		case LISPDECL_STRING_STREAM:
		case LISPDECL_SYNONYM_STREAM:
		case LISPDECL_TWO_WAY_STREAM:
		case LISPDECL_PROMPT_STREAM:
		case LISPDECL_PRETTY_STREAM:
		case LISPDECL_MEMORY_STREAM:
		case LISPDECL_PIPE_STREAM:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  stream object
 */
int subtypep_call_stream_type_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	enum LISPDECL type1, type2;

	GetLispDecl(x, &type1);
	GetLispDecl(y, &type2);
	if (type1 == type2)
		return ReturnInclude(ret);
	if (type1 == LISPDECL_STREAM)
		return ReturnFalse(ret);

	return ReturnExclude(ret);
}


/************************************************************
 *  subtypep_check.c
 ************************************************************/

/*
 *  range
 */
static int subtypep_number_count(addr pos, int *count);

static int subtypep_number_count_optimized(addr pos, int *count)
{
	get_type_optimized(&pos, pos);
	return subtypep_number_count(pos, count);
}

static int subtypep_number_count_subtypep(addr pos, int *count)
{
	get_type_subtypep(&pos, pos);
	return subtypep_number_count(pos, count);
}

static int subtypep_number_count_not(addr pos, int *count)
{
	GetArrayType(pos, 0, &pos);
	return subtypep_number_count(pos, count);
}

static int subtypep_number_count_andor(addr pos, int *count)
{
	addr value;
	size_t size, i;

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &value);
		if (subtypep_number_count(value, count))
			return 1;
	}

	return 0;
}

static int subtypep_number_count_range(addr pos, int *count)
{
	if (! range_asterisk_p(pos))
		(*count)++;

	return (2 <= *count);
}

static int subtypep_number_count(addr pos, int *count)
{
	switch (RefLispDecl(pos)) {
		case LISPDECL_OPTIMIZED:
			return subtypep_number_count_optimized(pos, count);

		case LISPDECL_SUBTYPEP:
			return subtypep_number_count_subtypep(pos, count);

		case LISPDECL_NOT:
			return subtypep_number_count_not(pos, count);

		case LISPDECL_AND:
		case LISPDECL_OR:
			return subtypep_number_count_andor(pos, count);

		case LISPDECL_INTEGER:
		case LISPDECL_RATIONAL:
		case LISPDECL_REAL:
		case LISPDECL_FLOAT:
		case LISPDECL_SHORT_FLOAT:
		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
			return subtypep_number_count_range(pos, count);

		default:
			return 0;
	}
}


/*
 *  or
 */
static int subtypep_number_begin(addr pos);

static int subtypep_number_begin_optimized(addr pos)
{
	get_type_optimized(&pos, pos);
	return subtypep_number_begin(pos);
}

static int subtypep_number_begin_subtypep(addr pos)
{
	get_type_subtypep(&pos, pos);
	return subtypep_number_begin(pos);
}

static int subtypep_number_begin_not(addr pos)
{
	GetArrayType(pos, 0, &pos);
	return subtypep_number_begin(pos);
}

static int subtypep_number_begin_or(addr pos)
{
	int count;
	addr value;
	size_t size, i;

	count = 0;
	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &value);
		if (subtypep_number_count(value, &count))
			return 1;
	}

	return 0;
}

static int subtypep_number_begin(addr pos)
{
	switch (RefLispDecl(pos)) {
		case LISPDECL_OPTIMIZED:
			return subtypep_number_begin_optimized(pos);

		case LISPDECL_SUBTYPEP:
			return subtypep_number_begin_subtypep(pos);

		case LISPDECL_NOT:
			return subtypep_number_begin_not(pos);

		case LISPDECL_AND:
		case LISPDECL_OR:
			return subtypep_number_begin_or(pos);

		default:
			return 0;
	}
}


/*
 *  interface
 */
int subtypep_number_p(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	return subtypep_number_begin(pos);
}


/************************************************************
 *  subtypep_compound.c
 ************************************************************/

/*
 *  subtypep_lisptype_
 */
static int subtypep_lisptype_normal_(Execute ptr, addr x, addr y,
		SubtypepResult *ret, call_type_subtypep call)
{
	int not2;
	SubtypepResult value;

	Return((*call)(ptr, x, y, &value));
	GetNotDecl(y, &not2);
	switch (value) {
		case SUBTYPEP_INCLUDE:
			return Result(ret, not2? SUBTYPEP_EXCLUDE: SUBTYPEP_INCLUDE);

		case SUBTYPEP_EXCLUDE:
			return Result(ret, not2? SUBTYPEP_INCLUDE: SUBTYPEP_EXCLUDE);

		default:
			return Result(ret, value);
	}
}

static int subtypep_lisptype_not_(Execute ptr, addr x, addr y,
		SubtypepResult *ret, call_type_subtypep call)
{
	int not2;
	SubtypepResult value;

	Return((*call)(ptr, y, x, &value));  /* reverse */
	GetNotDecl(y, &not2);
	switch (value) {
		case SUBTYPEP_INCLUDE:
			return Result(ret, not2? SUBTYPEP_INCLUDE: SUBTYPEP_EXCLUDE);

		case SUBTYPEP_EXCLUDE:
			return Result(ret, SUBTYPEP_FALSE);

		default:
			return Result(ret, value);
	}
}

static int subtypep_lisptype_(Execute ptr, addr x, addr y,
		SubtypepResult *ret, call_type_subtypep call)
{
	if (RefNotDecl(x))
		return subtypep_lisptype_not_(ptr, x, y, ret, call);
	else
		return subtypep_lisptype_normal_(ptr, x, y, ret, call);
}


/*
 *  subtypep_eql
 */
static int subtypep_eql_eql_(addr x, addr y, SubtypepResult *ret)
{
	GetArrayType(x, 0, &x);
	GetArrayType(y, 0, &y);
	if (eql_function(x, y))
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_eql_type_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;

	/* (subtypep '(eql x) '(satisfies y)) */
	type_getvalues1(y, &y);
	if (RefLispDecl(y) == LISPDECL_SATISFIES)
		return ReturnInvalid(ret);

	/* (subtypep '(eql x) y) */
	GetArrayType(x, 0, &x);
	Return(typep_table_(ptr, x, y, &check));
	if (check)
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_type_eql_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;

	/* (subtypep '(satisfies x) '(eql y)) */
	type_getvalues1(x, &x);
	if (RefLispDecl(x) == LISPDECL_SATISFIES)
		return ReturnInvalid(ret);

	/* (subtypep x '(eql x)) */
	GetArrayType(y, 0, &y);
	Return(typep_table_(ptr, y, x, &check));
	if (check)
		return ReturnFalse(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_eql_call_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check1, check2;

	check1 = (RefLispDecl(x) == LISPDECL_EQL);
	check2 = (RefLispDecl(y) == LISPDECL_EQL);
	if (check1 && check2)
		return subtypep_eql_eql_(x, y, ret);
	if (check1)
		return subtypep_eql_type_(ptr, x, y, ret);
	if (check2)
		return subtypep_type_eql_(ptr, x, y, ret);
	Abort("type error");
	return ReturnInvalid(ret);
}

static int subtypep_eql_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_lisptype_(ptr, x, y, ret, subtypep_eql_call_);
}


/*
 *  subtypep_values
 */
static size_t getsize_values(addr pos)
{
	addr check;
	size_t size;

	/* var */
	GetArrayType(pos, 0, &check);
	size = length_list_unsafe(check);
	/* opt */
	GetArrayType(pos, 1, &check);
	size += length_list_unsafe(check);

	return size;
}

static int subtypep_values_var_size(addr x, addr y)
{
	addr check;
	size_t size1, size2;

	GetArrayType(x, 0, &check);
	size1 = length_list_unsafe(check);
	GetArrayType(y, 0, &check);
	size2 = length_list_unsafe(check);

	return size1 < size2;
}

static int gettype_values_(addr pos, size_t index, addr *ret)
{
	addr check;
	size_t size;

	/* var */
	GetArrayType(pos, 0, &check);
	size = length_list_unsafe(check);
	if (index < size)
		return getnth_(check, index, ret);
	index -= size;

	/* opt */
	GetArrayType(pos, 1, &check);
	size = length_list_unsafe(check);
	if (index < size)
		return getnth_(check, index, ret);

	/* rest */
	GetArrayType(pos, 2, ret);

	return 0;
}

static int subtypep_boolean_(Execute ptr, addr x, addr y, int *ret)
{
	SubtypepResult value;
	Return(subtypep_compound_(ptr, x, y, &value));
	return Result(ret, value == SUBTYPEP_INCLUDE);
}

static int subtypep_values_values_(Execute ptr, addr x, addr y, int *ret)
{
	int check;
	addr check1, check2;
	size_t size1, size2, size, i;

	Check(RefLispDecl(x) != LISPDECL_VALUES, "decl left error");
	Check(RefLispDecl(y) != LISPDECL_VALUES, "decl right error");
	Check(RefNotDecl(x), "left not error");
	Check(RefNotDecl(y), "right not error");

	/* var */
	if (subtypep_values_var_size(x, y))
		return Result(ret, 0);

	/* size */
	size1 = getsize_values(x);
	size2 = getsize_values(y);
	size = (size1 > size2)? size1: size2;
	size++; /* &rest check */

	/* check */
	for (i = 0; i < size; i++) {
		Return(gettype_values_(x, i, &check1));
		Return(gettype_values_(y, i, &check2));
		Return(subtypep_boolean_(ptr, check1, check2, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static void subtypep_values_local(addr pos, addr *ret)
{
	LocalRoot local;
	addr rest;

	local = Local_Thread;
	conscar_local(local, &pos, pos);
	GetTypeTable(&rest, Null);
	type_values_local(local, pos, Nil, rest, Nil, ret);
}

static int subtypep_values_type_(Execute ptr, addr x, addr y, int *ret)
{
	Check(RefLispDecl(x) != LISPDECL_VALUES, "decl left error");
	Check(RefNotDecl(x), "left not error");
	subtypep_values_local(y, &y);
	return subtypep_values_values_(ptr, x, y, ret);
}

static int subtypep_type_values_(Execute ptr, addr x, addr y, int *ret)
{
	Check(RefLispDecl(y) != LISPDECL_VALUES, "decl right error");
	Check(RefNotDecl(y), "right not error");
	subtypep_values_local(x, &x);
	return subtypep_values_values_(ptr, x, y, ret);
}

static int subtypep_values_call_(Execute ptr, addr x, addr y, int *ret)
{
	int check1, check2;

	check1 = (RefLispDecl(x) == LISPDECL_VALUES);
	check2 = (RefLispDecl(y) == LISPDECL_VALUES);
	if (check1 && check2)
		return subtypep_values_values_(ptr, x, y, ret);
	if (check1)
		return subtypep_values_type_(ptr, x, y, ret);
	if (check2)
		return subtypep_type_values_(ptr, x, y, ret);
	Abort("type error");
	return Result(ret, 0);
}

static int subtypep_values_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int value;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	Return(subtypep_values_call_(ptr, x, y, &value));
	rollback_local(local, stack);
	return Result(ret, value? SUBTYPEP_INCLUDE: SUBTYPEP_FALSE);
}


/*
 *  subtypep_call
 */
int subtypep_atomic_not_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_lisptype_(ptr, x, y, ret, subtypep_table_);
}

static int subtypep_clos_left_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int not1;

	GetNotDecl(y, &not1);
	if (not1)
		return Result(ret, SUBTYPEP_INVALID);
	else
		return subtypep_atomic_not_(ptr, x, y, ret);
}


/* left */
static int subtypep_satisfies_left_(addr x, addr y, SubtypepResult *ret)
{
	if (RefLispDecl(y) == LISPDECL_T)
		return ReturnInclude(ret);
	else
		return ReturnInvalid(ret);
}

static int subtypep_left_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	Check(GetType(x) != LISPTYPE_TYPE, "type left error");
	switch (RefLispDecl(x)) {
		case LISPDECL_EQL:
			return subtypep_eql_(ptr, x, y, ret);

		case LISPDECL_VALUES:
			return subtypep_values_(ptr, x, y, ret);

		case LISPDECL_SATISFIES:
			return subtypep_satisfies_left_(x, y, ret);

		case LISPDECL_NIL:
			return ReturnInclude(ret);

		case LISPDECL_T:
			return ReturnFalse(ret);

		case LISPDECL_CLOS:
			return subtypep_clos_left_(ptr, x, y, ret);

		case LISPDECL_INVALID:
			return ReturnInvalid(ret);

		case LISPDECL_AND:
		case LISPDECL_OR:
		case LISPDECL_MEMBER:
		case LISPDECL_NOT:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The type illegal in this context.", NULL);

		default:
			return subtypep_atomic_not_(ptr, x, y, ret);
	}
}

/* right */
static int subtypep_satisfies_right_(addr x, addr y, SubtypepResult *ret)
{
	if (RefLispDecl(x) == LISPDECL_NIL)
		return ReturnInclude(ret);
	else
		return ReturnInvalid(ret);
}

static int subtypep_nil_right_(addr x, SubtypepResult *ret)
{
	if (RefLispDecl(x) == LISPDECL_NIL)
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_right_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	Check(GetType(y) != LISPTYPE_TYPE, "type right error");
	switch (RefLispDecl(y)) {
		case LISPDECL_EQL:
			return subtypep_eql_(ptr, x, y, ret);

		case LISPDECL_VALUES:
			return subtypep_values_(ptr, x, y, ret);

		case LISPDECL_SATISFIES:
			return subtypep_satisfies_right_(x, y, ret);

		case LISPDECL_NIL:
			return subtypep_nil_right_(x, ret);

		case LISPDECL_T:
			return ReturnInclude(ret);

		case LISPDECL_INVALID:
			return ReturnInvalid(ret);

		case LISPDECL_AND:
		case LISPDECL_OR:
		case LISPDECL_MEMBER:
		case LISPDECL_NOT:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The type illegal in this context.", NULL);

		default:
			return subtypep_left_(ptr, x, y, ret);
	}
}

int subtypep_compound_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	enum LISPDECL type1, type2;
	int and1, and2, or1, or2;

	CheckType(x, LISPTYPE_TYPE);
	CheckType(y, LISPTYPE_TYPE);
	GetLispDecl(x, &type1);
	GetLispDecl(y, &type2);
	and1 = (type1 == LISPDECL_AND);
	and2 = (type2 == LISPDECL_AND);
	or1 = (type1 == LISPDECL_OR);
	or2 = (type2 == LISPDECL_OR);

	if (and1 && and2)
		return subtypep_and_and_(ptr, x, y, ret);
	if (and1 && or2)
		return subtypep_and_or_(ptr, x, y, ret);
	if (or1 && and2)
		return subtypep_or_and_(ptr, x, y, ret);
	if (or1 && or2)
		return subtypep_or_or_(ptr, x, y, ret);

	if (and1)
		return subtypep_and_type_(ptr, x, y, ret);
	if (or1)
		return subtypep_or_type_(ptr, x, y, ret);
	if (and2)
		return subtypep_type_and_(ptr, x, y, ret);
	if (or2)
		return subtypep_type_or_(ptr, x, y, ret);

	return subtypep_right_(ptr, x, y, ret);
}


/************************************************************
 *  subtypep_number.c
 ************************************************************/

/*
 *  coerce
 */
static int real_extract_integer_local_(LocalRoot local, addr pos, addr *ret)
{
	addr rem;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			return Result(ret, pos);

		case LISPTYPE_RATIO:
		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
			return truncate1_common_(local, ret, &rem, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_rational_local_(LocalRoot local, addr pos, addr *ret)
{
	addr rem;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			return Result(ret, pos);

		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
			return truncate1_common_(local, ret, &rem, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_single_local_(LocalRoot local, addr pos, addr *ret)
{
	single_float value;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_local(local, ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return single_float_bignum_local_(local, ret, pos);

		case LISPTYPE_RATIO:
			return single_float_ratio_local_(local, ret, pos);

		case LISPTYPE_SINGLE_FLOAT:
			return Result(ret, pos);

		case LISPTYPE_DOUBLE_FLOAT:
			Return(cast_ds_value_(pos, &value));
			single_float_local(local, ret, value);
			return 0;

		case LISPTYPE_LONG_FLOAT:
			Return(cast_ls_value_(pos, &value));
			single_float_local(local, ret, value);
			return 0;

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_double_local_(LocalRoot local, addr pos, addr *ret)
{
	double_float value;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			double_float_fixnum_local(local, ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return double_float_bignum_local_(local, ret, pos);

		case LISPTYPE_RATIO:
			return double_float_ratio_local_(local, ret, pos);

		case LISPTYPE_SINGLE_FLOAT:
			Return(cast_sd_value_(pos, &value));
			double_float_local(local, ret, value);
			return 0;

		case LISPTYPE_DOUBLE_FLOAT:
			return Result(ret, pos);

		case LISPTYPE_LONG_FLOAT:
			Return(cast_ld_value_(pos, &value));
			double_float_local(local, ret, value);
			return 0;

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_long_local_(LocalRoot local, addr pos, addr *ret)
{
	long_float value;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			long_float_fixnum_local(local, ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return long_float_bignum_local_(local, ret, pos);

		case LISPTYPE_RATIO:
			return long_float_ratio_local_(local, ret, pos);

		case LISPTYPE_SINGLE_FLOAT:
			Return(cast_sl_value_(pos, &value));
			long_float_local(local, ret, value);
			return 0;

		case LISPTYPE_DOUBLE_FLOAT:
			Return(cast_dl_value_(pos, &value));
			long_float_local(local, ret, value);
			return 0;

		case LISPTYPE_LONG_FLOAT:
			return Result(ret, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_float_local_(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_local(local, ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return single_float_bignum_local_(local, ret, pos);

		case LISPTYPE_RATIO:
			return single_float_ratio_local_(local, ret, pos);

		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
			return Result(ret, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_lispdecl_local_(LocalRoot local,
		enum LISPDECL type, addr pos, addr *ret)
{
	if (type_asterisk_p(pos))
		return Result(ret, pos);

	switch (type) {
		case LISPDECL_INTEGER:
			return real_extract_integer_local_(local, pos, ret);

		case LISPDECL_RATIONAL:
			return real_extract_rational_local_(local, pos, ret);

		case LISPDECL_SHORT_FLOAT:
		case LISPDECL_SINGLE_FLOAT:
			return real_extract_single_local_(local, pos, ret);

		case LISPDECL_DOUBLE_FLOAT:
			return real_extract_double_local_(local, pos, ret);

		case LISPDECL_LONG_FLOAT:
			return real_extract_long_local_(local, pos, ret);

		case LISPDECL_FLOAT:
			return real_extract_float_local_(local, pos, ret);

		case LISPDECL_REAL:
			return Result(ret, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid real type.", NULL);
	}
}


/*
 *  real_filter
 */
static int type_range_left_(LocalRoot local,
		addr *ret, enum LISPDECL type, addr left1, addr left2)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	Return(real_extract_lispdecl_local_(local, type, left2, &left2));
	type4_local(local, type, left1, left2, aster, aster, ret);

	return 0;
}

static int type_range_right_(LocalRoot local,
		addr *ret, enum LISPDECL type, addr right1, addr right2)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	Return(real_extract_lispdecl_local_(local, type, right2, &right2));
	type4_local(local, type, aster, aster, right1, right2, ret);

	return 0;
}

static int type_range_not_(LocalRoot local, addr *ret, enum LISPDECL type,
		addr left1, addr left2, addr right1, addr right2)
{
	addr pos;

	vector4_local(local, &pos, 2);
	Return(type_range_left_(local, &right1, type, (right1 == Nil)? T: Nil, right2));
	Return(type_range_right_(local, &left1, type, (left1 == Nil)? T: Nil, left2));
	SetArrayA4(pos, 0, right1);
	SetArrayA4(pos, 1, left1);
	type1_local(local, LISPDECL_OR, pos, ret);

	return 0;
}

static int real_filter_not_range_(LocalRoot local,
		addr *ret, addr type, enum LISPDECL decl)
{
	int aster1, aster2;
	addr left1, left2, right1, right2;

	GetArrayType(type, 0, &left1);
	GetArrayType(type, 2, &right1);
	aster1 = type_asterisk_p(left1);
	aster2 = type_asterisk_p(right1);
	if (aster1 && aster2)
		return Result(ret, Nil);

	GetArrayType(type, 3, &right2);
	if (aster1)
		return type_range_left_(local, ret, decl, (right1 == Nil)? T: Nil, right2);

	GetArrayType(type, 1, &left2);
	if (aster2)
		return type_range_right_(local, ret, decl, (left1 == Nil)? T: Nil, left2);

	return type_range_not_(local, ret, decl, left1, left2, right1, right2);
}

static int real_filter_not_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	enum LISPDECL right;

	GetLispDecl(type, &right);
	if (right == LISPDECL_NUMBER)
		return Result(ret, Nil);
	if (decl_subtypep_real(decl, right))
		return real_filter_not_range_(local, ret, type, decl);
	type4aster_local(local, decl, ret);

	return 0;
}

static int real_filter_normal_(LocalRoot local,
		addr *ret, addr type, enum LISPDECL decl)
{
	enum LISPDECL right;
	addr left1, left2, right1, right2;

	GetLispDecl(type, &right);
	if (right == LISPDECL_NUMBER) {
		type4aster_local(local, decl, ret);
		return 0;
	}
	if (decl_subtypep_real(decl, right)) {
		GetArrayType(type, 0, &left1);
		GetArrayType(type, 1, &left2);
		GetArrayType(type, 2, &right1);
		GetArrayType(type, 3, &right2);
		Return(real_extract_lispdecl_local_(local, decl, left2, &left2));
		Return(real_extract_lispdecl_local_(local, decl, right2, &right2));
		type4_local(local, decl, left1, left2, right1, right2, ret);
		return 0;
	}

	return Result(ret, Nil);
}

static int real_filter_type_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	if (RefNotDecl(type))
		return real_filter_not_(local, ret, type, decl);
	else
		return real_filter_normal_(local, ret, type, decl);
}

static void vector4_andor(LocalRoot local,
		addr *ret, addr src, size_t size, enum LISPDECL decl)
{
	addr dst, pos;
	size_t i;

	if (size == 0) {
		*ret = Nil;
		return;
	}
	vector4_local(local, &dst, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(src, i, &pos);
		SetArrayA4(dst, i, pos);
	}
	type1_local(local, decl, dst, ret);
}

static int real_filter_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl);
static int real_filter_and_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	addr temp, pos;
	size_t i, size, count;

	/* copy temporary */
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	vector4_local(local, &temp, size);
	for (count = i = 0; i < size; i++) {
		GetArrayA4(type, i, &pos);
		Return(real_filter_(local, &pos, pos, decl));
		if (pos == Nil)
			return Result(ret, Nil);
		SetArrayA4(temp, count++, pos);
	}

	/* make type-or */
	vector4_andor(local, ret, temp, count, LISPDECL_AND);
	return 0;
}

static int real_filter_or_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	addr temp, pos;
	size_t i, size, count;

	/* copy temporary */
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	vector4_local(local, &temp, size);
	for (count = i = 0; i < size; i++) {
		GetArrayA4(type, i, &pos);
		Return(real_filter_(local, &pos, pos, decl));
		if (pos != Nil) {
			SetArrayA4(temp, count++, pos);
		}
	}

	/* make type-or */
	vector4_andor(local, ret, temp, count, LISPDECL_OR);
	return 0;
}

static int real_filter_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	switch (RefLispDecl(type)) {
		case LISPDECL_AND:
			return real_filter_and_(local, ret, type, decl);

		case LISPDECL_OR:
			return real_filter_or_(local, ret, type, decl);

		default:
			return real_filter_type_(local, ret, type, decl);
	}
}


/*
 *  merge_range
 */
static void merge_range_cons(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	Check(RefLispDecl(type) != decl, "type error");
	Check(RefNotDecl(type), "not error");
	conscar_local(local, ret, type);
}

/* merge-range-and */
static int make_range_left_right_(LocalRoot local, addr *ret, addr left, addr right)
{
	enum LISPDECL decl;
	addr left1, left2, right1, right2;

	range_left_value(left, &left1, &left2);
	range_right_value(right, &right1, &right2);
	GetLispDecl(left, &decl);
	Return(real_extract_lispdecl_local_(local, decl, left2, &left2));
	Return(real_extract_lispdecl_local_(local, decl, right2, &right2));
	type4_local(local, decl, left1, left2, right1, right2, ret);

	return 0;
}

static int make_range_left_aster_(LocalRoot local, addr *ret, addr left)
{
	addr left1, left2;

	range_left_value(left, &left1, &left2);
	return type_range_left_(local, ret, RefLispDecl(left), left1, left2);
}

static int make_range_aster_right_(LocalRoot local, addr *ret, addr right)
{
	addr right1, right2;

	range_right_value(right, &right1, &right2);
	return type_range_right_(local, ret, RefLispDecl(right), right1, right2);
}

/* (10 *) (20 *) */
static int range_and_left_left_(addr *ret, addr left, addr right)
{
	int check;
	Return(range_left_left_less_(left, right, &check));
	return Result(ret, check? right: left);
}

/* (* 20) (10 *) */
static int range_and_right_left_(LocalRoot local, addr *ret, addr left, addr right)
{
	int check;

	Return(range_right_left_greater_equal_(left, right, &check));
	if (check)
		return make_range_left_right_(local, ret, right, left);

	return Result(ret, Nil);
}

/* (10 20) (15 *) */
static int range_and_between_left_(LocalRoot local, addr *ret, addr left, addr right)
{
	int check;

	Return(range_left_left_less_equal_(right, left, &check));
	if (check)
		return Result(ret, left);

	Return(range_left_right_less_equal_(right, left, &check));
	if (check)
		return make_range_left_right_(local, ret, right, left);

	return Result(ret, Nil);
}

static int range_and_left_(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(left))
		return Result(ret, right);
	if (range_left_p(left))
		return range_and_left_left_(ret, left, right);
	if (range_right_p(left))
		return range_and_right_left_(local, ret, left, right);
	if (range_between_p(left))
		return range_and_between_left_(local, ret, left, right);

	*ret = Nil;
	return fmte_("type error", NULL);
}

/* (10 *) (* 20) */
static int range_and_left_right_(LocalRoot local, addr *ret, addr left, addr right)
{
	int check;

	Return(range_left_right_less_equal_(left, right, &check));
	if (check)
		return make_range_left_right_(local, ret, left, right);

	return Result(ret, Nil);
}

/* (* 10) (* 20) */
static int range_and_right_right_(addr *ret, addr left, addr right)
{
	int check;
	Return(range_right_right_less_(left, right, &check));
	return Result(ret, check? left: right);
}

/* (10 30) (* 20) */
static int range_and_between_right_(LocalRoot local, addr *ret, addr left, addr right)
{
	int check;

	Return(range_right_right_less_equal_(left, right, &check));
	if (check)
		return Result(ret, left);

	Return(range_left_right_less_equal_(left, right, &check));
	if (check)
		return make_range_left_right_(local, ret, left, right);

	return Result(ret, Nil);
}

static int range_and_right_(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(left))
		return Result(ret, right);
	if (range_left_p(left))
		return range_and_left_right_(local, ret, left, right);
	if (range_right_p(left))
		return range_and_right_right_(ret, left, right);
	if (range_between_p(left))
		return range_and_between_right_(local, ret, left, right);

	*ret = Nil;
	return fmte_("type error", NULL);
}

/* (10 40) (20 30) */
static int range_and_between_between_(LocalRoot local, addr *ret, addr left, addr right)
{
	int check;

	Return(range_between_in_(left, right, &check));
	if (check)
		return Result(ret, right);
	Return(range_between_in_(right, left, &check));
	if (check)
		return Result(ret, left);
	Return(range_between_left_(left, right, &check));
	if (check)
		return make_range_left_right_(local, ret, right, left);
	Return(range_between_right_(left, right, &check));
	if (check)
		return make_range_left_right_(local, ret, left, right);

	return Result(ret, Nil);
}

static int range_and_between_(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(left))
		return Result(ret, right);
	if (range_left_p(left))
		return range_and_between_left_(local, ret, right, left);
	if (range_right_p(left))
		return range_and_between_right_(local, ret, right, left);
	if (range_between_p(left))
		return range_and_between_between_(local, ret, left, right);

	*ret = Nil;
	return fmte_("type error", NULL);
}

static int range_and_(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(right))
		return Result(ret, left);
	if (range_left_p(right))
		return range_and_left_(local, ret, left, right);
	if (range_right_p(right))
		return range_and_right_(local, ret, left, right);
	if (range_between_p(right))
		return range_and_between_(local, ret, left, right);

	*ret = Nil;
	return fmte_("type error", NULL);
}

static int map_range_and_(LocalRoot local, addr *ret, addr list, addr right)
{
	addr result, left;

	for (result = Nil; list != Nil; ) {
		GetCons(list, &left, &list);
		Return(range_and_(local, &left, left, right));
		if (left != Nil)
			cons_local(local, &result, left, result);
	}
	nreverse(ret, result);

	return 0;
}

static int merge_range_andplus_(LocalRoot local, addr *ret, addr left, addr right)
{
	addr type;

	while (right != Nil) {
		GetCons(right, &type, &right);
		Return(map_range_and_(local, &left, left, type));
		if (left == Nil)
			break;
	}

	return Result(ret, left);
}

static int merge_range_type_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl);
static int range_and_otherwise_(LocalRoot local,
		addr *ret, addr array, enum LISPDECL decl)
{
	addr left, right;
	size_t i, size;

	LenArrayA4(array, &size);
	left = Nil;
	for (i = 0; i < size; i++) {
		GetArrayA4(array, i, &right);
		Return(merge_range_type_(local, &right, right, decl));
		if (right == Nil) {
			left = Nil;
			break;
		}
		else if (right == T) {
			continue;
		}
		else if (left == Nil) {
			left = right;
		}
		else {
			Return(merge_range_andplus_(local, &left, left, right));
			if (left == Nil)
				break;
		}
	}

	return Result(ret, left);
}

static int merge_range_and_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	size_t size;

	Check(RefNotDecl(type), "not error");
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	if (size == 0)
		return Result(ret, T);
	if (size == 1) {
		GetArrayA4(type, 0, &type);
		conscar_local(local, ret, type);
		return 0;
	}

	return range_and_otherwise_(local, ret, type, decl);
}

/* merge-range-or */
typedef int (*extpairtype)(LocalRoot, addr x, addr y, addr *value, int *ret);
static int extpaircall_right_(LocalRoot local,
		extpairtype call, addr left, addr cons, addr *value, int *ret)
{
	int check;
	addr right;

	while (cons != Nil) {
		GetCons(cons, &right, &cons);
		if (left != right) {
			Return((*call)(local, left, right, &right, &check));
			if (check) {
				*value = right;
				return Result(ret, check);
			}
		}
	}

	return Result(ret, 0);
}

static void extpaircall_pushlist(LocalRoot local, addr *ret, addr list, addr result)
{
	addr one;

	while (list != Nil) {
		GetCons(list, &one, &list);
		cons_local(local, &result, one, result);
	}
	*ret = result;
}

static int extpaircall_left_(
		LocalRoot local, extpairtype call, addr right, addr *value, int *ret)
{
	int update, check;
	addr left, result, pos, cons;

	result = Nil;
	update = 0;
	for (cons = right; cons != Nil; ) {
		GetCons(cons, &left, &cons);
		if (update) {
			cons_local(local, &result, left, result);
		}
		else {
			Return(extpaircall_right_(local, call, left, right, &pos, &check));
			if (check < 0)
				extpaircall_pushlist(local, &result, pos, result);
			else
				cons_local(local, &result, check? pos: left, result);
			if (check)
				update = 1;
		}
	}
	if (update)
		nreverse(value, result);
	return Result(ret, update);
}

static int extpaircall_(LocalRoot local, extpairtype call, addr *cons, int *update)
{
	int loop, check;
	addr pos;

	pos = *cons;
	loop = 0;
	for (;;) {
		Return(extpaircall_left_(local, call, pos, &pos, &check));
		if (! check)
			break;
		loop = 1;
	}
	if (loop) {
		*cons = pos;
		*update = 1;
	}

	return 0;
}

/* check only */
static int range_or_check_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	Check(RefLispDecl(left) != RefLispDecl(right), "type error");
	Check(RefNotDecl(left) || RefNotDecl(right), "not error");
	return Result(ret, 0);
}

/* (? ?) (* *) -> delete */
static int range_or_aster_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	if (range_asterisk_p(right)) {
		*value = Nil;
		return Result(ret, -1); /* list */
	}

	return Result(ret, 0);
}

/* (20 ?) (10 *) -> delete */
static int range_or_left_left_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_left_any_p(left))
		return Result(ret, 0);
	if (! range_left_p(right))
		return Result(ret, 0);
	Return(range_left_left_less_equal_(right, left, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	*value = Nil;
	return Result(ret, -1);
}

/* (? 10) (* 20) -> delete */
static int range_or_right_right_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_any_right_p(left))
		return Result(ret, 0);
	if (! range_right_p(right))
		return Result(ret, 0);
	Return(range_right_right_less_equal_(left, right, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	*value = Nil;
	return Result(ret, -1);
}

/* (10 *) (* 20) -> (10 20) */
static int range_or_left_right_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_left_p(left))
		return Result(ret, 0);
	if (! range_right_p(right))
		return Result(ret, 0);
	Return(range_connect_right_left_(right, left, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	type4aster_local(local, RefLispDecl(left), value);
	return Result(ret, 1);
}

/* (10 30) (20 *) -> (10 *) */
static int range_or_range_left_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_between_p(left))
		return Result(ret, 0);
	if (! range_left_p(right))
		return Result(ret, 0);
	Return(range_connect_between_left_(left, right, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	Return(make_range_left_aster_(local, value, left));
	return Result(ret, 1);
}

/* (10 30) (* 20) -> (* 30) */
static int range_or_range_right_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_between_p(left))
		return Result(ret, 0);
	if (! range_right_p(right))
		return Result(ret, 0);
	Return(range_connect_between_right_(left, right, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	Return(make_range_aster_right_(local, value, left));
	return Result(ret, 1);
}

/* (21 22) (10 30) -> delete */
static int range_or_range_range_in_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_between_p(left))
		return Result(ret, 0);
	if (! range_between_p(right))
		return Result(ret, 0);
	Return(range_between_in_(right, left, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	*value = Nil;
	return Result(ret, -1);
}

/* (10 30) (20 40) -> (10 30) */
static int range_or_range_range_left_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_between_p(left))
		return Result(ret, 0);
	if (! range_between_p(right))
		return Result(ret, 0);
	Return(range_between_left_(left, right, &check));
	if (! check)
		return Result(ret, 0);
	Return(range_between_right_(left, right, &check));
	if (check)
		return Result(ret, 0);

	/* true */
	Return(make_range_left_right_(local, value, left, right));
	return Result(ret, 1);
}

/* (20 40) (10 30) -> (10 40) */
static int range_or_range_range_right_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_between_p(left))
		return Result(ret, 0);
	if (! range_between_p(right))
		return Result(ret, 0);
	Return(range_between_left_(left, right, &check));
	if (check)
		return Result(ret, 0);
	Return(range_between_right_(left, right, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	Return(make_range_left_right_(local, value, right, left));
	return Result(ret, 1);
}

#define Return_extpaircall(a,b,c,d) Return(extpaircall_((a),(b),(c),(d)))
static int merge_range_orplus_(LocalRoot local, addr *ret, addr left, addr right)
{
	int update, result;

	append2_local_unsafe(local, left, right, &left);
	for (result = 0; ; result |= update) {
		update = 0;
		Return_extpaircall(local, range_or_check_, &left, &update);
		Return_extpaircall(local, range_or_aster_, &left, &update);
		Return_extpaircall(local, range_or_left_left_, &left, &update);
		Return_extpaircall(local, range_or_right_right_, &left, &update);
		Return_extpaircall(local, range_or_left_right_, &left, &update);
		Return_extpaircall(local, range_or_range_left_, &left, &update);
		Return_extpaircall(local, range_or_range_right_, &left, &update);
		Return_extpaircall(local, range_or_range_range_in_, &left, &update);
		Return_extpaircall(local, range_or_range_range_left_, &left, &update);
		Return_extpaircall(local, range_or_range_range_right_, &left, &update);
		if (update == 0)
			break;
	}

	return Result(ret, left);
}

static int range_or_otherwise_(LocalRoot local,
		addr *ret, addr array, enum LISPDECL decl)
{
	addr left, right;
	size_t size, i;

	LenArrayA4(array, &size);
	left = Nil;
	for (i = 0; i < size; i++) {
		GetArrayA4(array, i, &right);
		Return(merge_range_type_(local, &right, right, decl));
		if (right == Nil) {
			continue;
		}
		if (right == T) {
			left = T;
			break;
		}
		if (left == Nil) {
			left = right;
		}
		else {
			Return(merge_range_orplus_(local, &left, left, right));
			if (left == T)
				break;
		}
	}

	return Result(ret, left);
}

static int merge_range_or_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	size_t size;

	Check(RefNotDecl(type), "not error");
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	if (size == 0)
		return Result(ret, Nil);
	if (size == 1) {
		GetArrayA4(type, 0, &type);
		conscar_local(local, ret, type);
		return 0;
	}

	return range_or_otherwise_(local, ret, type, decl);
}

static int merge_range_type_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	switch (RefLispDecl(type)) {
		case LISPDECL_AND:
			return merge_range_and_(local, ret, type, decl);

		case LISPDECL_OR:
			return merge_range_or_(local, ret, type, decl);

		default:
			merge_range_cons(local, ret, type, decl);
			return 0;
	}
}

static void type_or_cons(LocalRoot local, addr *ret, addr cons)
{
	addr array, pos;
	size_t i, size;

	array = cons;
	for (size = 0; array != Nil; size++) {
		GetCdr(array, &array);
	}
	vector4_local(local, &array, size);
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &pos, &cons);
		SetArrayA4(array, i, pos);
	}
	type1_local(local, LISPDECL_OR, array, ret);
}

static int make_merge_range_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	Return(merge_range_type_(local, &type, type, decl));
	if (type == Nil)
		return Result(ret, Nil);
	if (type == T) {
		type4aster_local(local, decl, ret);
		return 0;
	}
	if (singlep(type)) {
		GetCar(type, ret);
		return 0;
	}
	if (GetType(type) == LISPTYPE_CONS) {
		type_or_cons(local, ret, type);
		return 0;
	}

	*ret = Nil;
	return fmte_("type error", NULL);
}

static int merge_range_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	int ignore;

	Return(real_filter_(local, &type, type, decl));
	if (type != Nil) {
		Return(type_optimize_local_(local, type, &type, &ignore));
		get_type_optimized(&type, type);
	}
	if ((type == Nil) || (RefLispDecl(type) == LISPDECL_NIL))
		return Result(ret, Nil);

	return make_merge_range_(local, ret, type, decl);
}


/*
 *  real_extract
 */
static const enum LISPDECL RealFilterDecls[] = {
	LISPDECL_INTEGER,
	LISPDECL_RATIONAL,
	LISPDECL_SHORT_FLOAT,
	LISPDECL_SINGLE_FLOAT,
	LISPDECL_DOUBLE_FLOAT,
	LISPDECL_LONG_FLOAT,
	LISPDECL_EMPTY
};

static int real_filter_range_list_(LocalRoot local, addr type,
		addr *value, size_t *ret)
{
	addr cons, check;
	size_t i, size;
	enum LISPDECL decl;

	cons = Nil;
	for (i = size = 0; ; i++) {
		decl = RealFilterDecls[i];
		if (decl == LISPDECL_EMPTY)
			break;
		Return(merge_range_(local, &check, type, decl));
		if (check != Nil) {
			cons_local(local, &cons, check, cons);
			size++;
		}
	}
	*value = cons;

	return Result(ret, size);
}

static void real_reject(LocalRoot local, addr *ret, addr type)
{
	addr pos;

	/* (and (not real) [type]) */
	vector4_local(local, &pos, 2);
	SetArrayA4(pos, 1, type);
	type4aster_local(local, LISPDECL_REAL, &type);
	SetNotDecl(type, 1);
	SetArrayA4(pos, 0, type);
	type1_local(local, LISPDECL_AND, pos, ret);
}

static void copy_cons_to_vector4_local(LocalRoot local,
		addr *ret, addr cons, size_t size)
{
	addr array, pos;
	size_t i;

	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		GetCons(cons, &pos, &cons);
		SetArrayA4(array, i, pos);
	}
	*ret = array;
}

static int make_real_filter_(LocalRoot local, addr *ret, addr type)
{
	addr pos;
	size_t size;

	Return(real_filter_range_list_(local, type, &pos, &size));
	if (size == 0)
		return Result(ret, type);
	real_reject(local, &type, type);
	cons_local(local, &pos, type, pos);
	nreverse(&pos, pos);
	copy_cons_to_vector4_local(local, &pos, pos, size + 1UL);
	type1_local(local, LISPDECL_OR, pos, ret);

	return 0;
}

static int real_extract_(LocalRoot local, addr *ret, addr type)
{
	int ignore;

	Return(type_optimize_local_(local, type, &type, &ignore));
	get_type_optimized(&type, type);
	Return(make_real_filter_(local, &type, type));
	Return(type_optimize_local_(local, type, &type, &ignore));
	get_type_optimized(ret, type);

	return 0;
}

int real_extract_local_(LocalRoot local, addr *ret, addr type)
{
	CheckLocal(local);
	CheckType(type, LISPTYPE_TYPE);
	if (RefLispDecl(type) == LISPDECL_SUBTYPEP) {
		*ret = type;
	}
	else {
		Return(real_extract_(local, &type, type));
		type1_local(local, LISPDECL_SUBTYPEP, type, ret);
	}

	return 0;
}

int real_extract_heap_(LocalRoot local, addr *ret, addr type)
{
	LocalStack stack;

	CheckLocal(local);
	push_local(local, &stack);
	Return(real_extract_local_(local, &type, type));
	type_copy_heap(ret, type);
	rollback_local(local, stack);

	return 0;
}

int type_subtypep_p(addr type)
{
	CheckType(type, LISPTYPE_TYPE);
	return RefLispDecl(type) == LISPDECL_SUBTYPEP;
}

int type_optimized_or_subtypep(addr type)
{
	return type_optimized_p(type) || type_subtypep_p(type);
}

void get_type_subtypep(addr *ret, addr type)
{
	if (type_subtypep_p(type)) {
		GetArrayType(type, 0, ret);
	}
	else {
		*ret = type;
	}
}

int type_subtypep_throw_heap_(LocalRoot local, addr type, addr *ret)
{
	Return(real_extract_heap_(local, &type, type));
	get_type_subtypep(ret, type);
	return 0;
}


/************************************************************
 *  subtypep_optimize.c
 ************************************************************/

/*
 *  macro
 */
static int type_optimize_(LocalRoot local, addr type, addr *value, int *ret);
static int check_optimize_(addr type, int *ret);
typedef int (*extractcalltype)(addr *, addr);

#define CheckNotDecl(x, type) (RefLispDecl(x) == type && RefNotDecl(x))

#define Return_or_optimize(call, type, ret) { \
	Return(call(type, ret)); \
	if (*ret) { \
		return 0; \
	} \
};

#define Return_check_optimize(call, type, ret) { \
	Return(call(type, ret)); \
	if (*ret == 0) { \
		return 0; \
	} \
};

#define extractcall(local, call, pos, update) { \
	int __check = 0; \
	addr __value = 0; \
	Check(GetType(pos) != LISPTYPE_TYPE, "type error"); \
	for (;;) { \
		Return(call(local, pos, &__value, &__check)); \
		if (! __check) { \
			break; \
		} \
		update = 1; \
		pos = __value; \
	} \
}

#define extractcallnot(local, call, pos, update) { \
	int __check = 0; \
	addr __value = 0; \
	Check(GetType(pos) != LISPTYPE_TYPE, "type error"); \
	for (;;) { \
		Return(call(local, pos, &__value, &__check)); \
		if (! __check) { \
			break; \
		}\
		update = 1; \
		if (RefNotDecl(pos)) { \
			type_revnotdecl(__value); \
		} \
		pos = __value; \
	} \
}


/*
 *  optimize
 */
static int check_type_delay_(addr pos, int *ret)
{
	if (RefLispDecl(pos) != LISPDECL_DELAY)
		return Result(ret, 0);
	GetArrayType(pos, 1, &pos);
	if (pos == Nil)
		return Result(ret, 1);

	return check_optimize_(pos, ret);
}

static int optimize_type_delay_(LocalRoot local, addr pos, addr *value, int *ret)
{
	Return_check_optimize(check_type_delay_, pos, ret);
	Return(get_delay_type_(Execute_Thread, pos, &pos));
	return type_optimize_(local, pos, value, ret);
}

static int check_optimized_(addr right, int *ret)
{
	enum LISPDECL type;

	GetLispDecl(right, &type);
	*ret = (type == LISPDECL_OPTIMIZED || type == LISPDECL_SUBTYPEP);

	return 0;
}

static int optimize_optimized_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_optimized_, right, ret);

	if (! RefNotDecl(right)) {
		GetArrayType(right, 0, value);
	}
	else {
		GetArrayType(right, 0, &right);
		type_copy_unsafe_local(local, &right, right);
		type_revnotdecl(right);
		*value = right;
	}

	return Result(ret, 1);
}

static int check_not_asterisk_(addr right, int *ret)
{
	*ret = CheckNotDecl(right, LISPDECL_ASTERISK);
	return 0;
}
static int optimize_not_asterisk_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_not_asterisk_, right, ret);

	/* error */
	return fmte_("Don't allow to use (not *).", NULL);
}

static int check_not_nil_(addr right, int *ret)
{
	*ret = CheckNotDecl(right, LISPDECL_NIL);
	return 0;
}
static int optimize_not_nil_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_not_nil_, right, ret);
	type0_local(local, LISPDECL_T, value);
	return Result(ret, 1);
}

static int check_not_t_(addr right, int *ret)
{
	*ret = CheckNotDecl(right, LISPDECL_T);
	return 0;
}
static int optimize_not_t_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_not_t_, right, ret);
	type0_local(local, LISPDECL_NIL, value);
	return Result(ret, 1);
}

/* (mod size) -> (integer 0 (size)) */
static int check_mod_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_MOD);
	return 0;
}
static int optimize_mod_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr left, pos;

	Return_check_optimize(check_mod_, right, ret);
	GetArrayType(right, 0, &left);
	Check(! integerp(left), "type error");
	Check(! plusp_integer_debug(left), "plusp error");
	fixnum_local(local, &pos, 0);
	type4_local(local, LISPDECL_INTEGER, Nil, pos, T, left, value);

	return Result(ret, 1);
}

/* atom -> (not cons) */
static int check_atom_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_ATOM);
	return 0;
}
static int optimize_atom_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr left;

	Return_check_optimize(check_atom_, right, ret);
	type2aster_localall(local, LISPDECL_CONS, &left);
	SetNotDecl(left, 1);
	*value = left;

	return Result(ret, 1);
}

/* list -> (or null cons) */
static int check_list_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_LIST);
	return 0;
}
static int optimize_list_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr pos, array;

	Return_check_optimize(check_list_, right, ret);
	vector4_local(local, &array, 2);
	/* null */
	type0_local(local, LISPDECL_NULL, &pos);
	SetArrayA4(array, 0, pos);
	/* cons */
	type2aster_localall(local, LISPDECL_CONS, &pos);
	SetArrayA4(array, 1, pos);
	/* result */
	type1_local(local, LISPDECL_OR, array, value);

	return Result(ret, 1);
}

/* boolean -> (or null (eql t)) */
static int check_boolean_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_BOOLEAN);
	return 0;
}
static int optimize_boolean_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr pos, array;

	Return_check_optimize(check_boolean_, right, ret);
	vector4_local(local, &array, 2);
	/* null */
	type0_local(local, LISPDECL_NULL, &pos);
	SetArrayA4(array, 0, pos);
	/* (eql t) */
	type_eql_local(local, T, &pos);
	SetArrayA4(array, 1, pos);
	/* result */
	type1_local(local, LISPDECL_OR, array, value);

	return Result(ret, 1);
}

/*
 *  (... type *) -> (array type 1)
 *  (... type size) -> (array type (size))
 */
static int extract_vector(LocalRoot local,
		addr *ret, enum LISPDECL decl, addr type, addr size)
{
	addr array;

	if (type_asterisk_p(size)) {
		fixnum_local(local, &size, 1);
		type2_local(local, decl, type, size, ret);
		return 1;
	}
	if (GetType(size) == LISPTYPE_FIXNUM) {
		vector4_local(local, &array, 1);
		SetArrayA4(array, 0, size);
		type2_local(local, decl, type, array, ret);
		return 1;
	}

	return 0;
}

/* sequence -> (or null cons (array * 1)) */
static int check_sequence_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_SEQUENCE);
	return 0;
}

static int optimize_sequence_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr pos, array, type;

	Return_check_optimize(check_sequence_, right, ret);
	vector4_local(local, &array, 3);
	/* null */
	type0_local(local, LISPDECL_NULL, &pos);
	SetArrayA4(array, 0, pos);
	/* cons */
	type2aster_localall(local, LISPDECL_CONS, &pos);
	SetArrayA4(array, 1, pos);
	/* (array * 1) */
	type0_local(local, LISPDECL_ASTERISK, &type);
	fixnum_local(local, &pos, 1);
	type2_local(local, LISPDECL_ARRAY, type, pos, &pos);
	SetArrayA4(array, 2, pos);
	/* result */
	type1_local(local, LISPDECL_OR, array, value);

	return Result(ret, 1);
}

/* (vector type size) -> (array type (size)) */
static int check_vector_(addr right, int *ret)
{
	if (RefLispDecl(right) != LISPDECL_VECTOR)
		return Result(ret, 0);
	GetArrayType(right, 1, &right);
	*ret = (type_asterisk_p(right) || GetType(right) == LISPTYPE_FIXNUM);

	return 0;
}
static int optimize_vector_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_vector_, right, ret);
	GetArrayType(right, 0, &type);
	GetArrayType(right, 1, &right);
	extract_vector(local, value, LISPDECL_ARRAY, type, right);

	return Result(ret, 1);
}

/* (simple-vector size) -> (simple-array t (size)) */
static int check_vector_type(enum LISPDECL decl, addr right)
{
	if (RefLispDecl(right) != decl)
		return 0;
	GetArrayType(right, 0, &right);
	return type_asterisk_p(right) || GetType(right) == LISPTYPE_FIXNUM;
}
static int check_simple_vector_(addr right, int *ret)
{
	*ret = check_vector_type(LISPDECL_SIMPLE_VECTOR, right);
	return 0;
}
static int optimize_simple_vector_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_simple_vector_, right, ret);
	upgraded_array_t_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, value, LISPDECL_SIMPLE_ARRAY, type, right);

	return Result(ret, 1);
}

/* (bit-vector size) -> (array bit (size)) */
static int check_bit_vector_(addr right, int *ret)
{
	*ret = check_vector_type(LISPDECL_BIT_VECTOR, right);
	return 0;
}
static int optimize_bit_vector_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_bit_vector_, right, ret);
	upgraded_array_bit_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, value, LISPDECL_ARRAY, type, right);

	return Result(ret, 1);
}

/* (simple-bit-vector size) -> (simple-array bit (size)) */
static int check_simple_bit_vector_(addr right, int *ret)
{
	*ret = check_vector_type(LISPDECL_SIMPLE_BIT_VECTOR, right);
	return 0;
}
static int optimize_simple_bit_vector_(
		LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_simple_bit_vector_, right, ret);
	upgraded_array_bit_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, value, LISPDECL_SIMPLE_ARRAY, type, right);

	return Result(ret, 1);
}

/* extended-char -> (and character (not base-char)) */
static int check_extended_char_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_EXTENDED_CHAR);
	return 0;
}
static int optimize_extended_char_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr array;

	Return_check_optimize(check_extended_char_, right, ret);
	vector4_local(local, &array, 2);
	/* character */
	type0_local(local, LISPDECL_CHARACTER, &right);
	SetArrayA4(array, 0, right);
	/* (not base-char) */
	type0_local(local, LISPDECL_BASE_CHAR, &right);
	SetNotDecl(right, 1);
	SetArrayA4(array, 1, right);
	/* result */
	type1_local(local, LISPDECL_AND, array, value);

	return Result(ret, 1);
}

/* (string size) -> (vector character size) */
static void extract_string(LocalRoot local, addr *value, addr right, addr type)
{
	GetArrayType(right, 0, &right);
	type2_local(local, LISPDECL_VECTOR, type, right, value);
}

static int check_string_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_STRING);
	return 0;
}
static int optimize_string_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_string_, right, ret);
	upgraded_array_character_local(local, &type);
	extract_string(local, value, right, type);

	return Result(ret, 1);
}

/* (base-string size) -> (vector base-char size) */
static int check_base_string_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_BASE_STRING);
	return 0;
}
static int optimize_base_string_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_base_string_, right, ret);
	upgraded_array_character_local(local, &type);
	extract_string(local, value, right, type);

	return Result(ret, 1);
}

/* (simple-string size) -> (simple-array character (size)) */
static int check_simple_string_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_SIMPLE_STRING);
	return 0;
}
static int optimize_simple_string_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_simple_string_, right, ret);
	upgraded_array_character_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, value, LISPDECL_SIMPLE_ARRAY, type, right);

	return Result(ret, 1);
}

/* (simple-base-string size) -> (simple-array base-char (size)) */
static int check_simple_base_string_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_SIMPLE_BASE_STRING);
	return 0;
}
static int optimize_simple_base_string_(
		LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_simple_base_string_, right, ret);
	upgraded_array_character_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, value, LISPDECL_SIMPLE_ARRAY, type, right);

	return Result(ret, 1);
}

/* (signed-byte *) -> integer */
/* (signed-byte size) -> (integer -2^size-1 2^(size-1)-1) */
static int check_signed_byte_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_SIGNED_BYTE);
	return 0;
}
static int optimize_signed_byte_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr x, y;

	Return_check_optimize(check_signed_byte_, type, ret);

	/* asterisk */
	GetArrayType(type, 0, &y);
	if (type_asterisk_p(y)) {
		/* (signed-byte *) */
		type4aster_localall(local, LISPDECL_INTEGER, value);
		return Result(ret, 1);
	}

	/*  (let ((v (ash 1 (1- value))))
	 *    `(integer ,(- v) ,(1- v)))
	 */
	fixnum_heap(&x, 1);
	Return(oneminus_integer_common_(local, y, &y));
	Return(ash_integer_common_(local, x, y, &y));
	Return(sign_reverse_integer_common_(y, &x));
	Return(oneminus_integer_common_(local, y, &y));
	Return(integer_result_local_(local, x, &x));
	Return(integer_result_local_(local, y, &y));
	type4_local(local, LISPDECL_INTEGER, Nil, x, Nil, y, value);

	return Result(ret, 1);
}

/* (unsigned-byte *) -> (integer 0 *) */
/* (unsigned-byte size) -> (integer 0 2^size-1) */
static int check_unsigned_byte_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_UNSIGNED_BYTE);
	return 0;
}
static int optimize_unsigned_byte_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr x, y;

	Return_check_optimize(check_unsigned_byte_, type, ret);

	/* asterisk */
	GetArrayType(type, 0, &y);
	if (type_asterisk_p(y)) {
		/* (unsigned-byte *) */
		fixnum_local(local, &x, 0);
		type4_local(local, LISPDECL_INTEGER, Nil, x, y, y, value);
		return Result(ret, 1);
	}

	/*  (let ((v (ash 1 value)))
	 *    `(integer 0 (,v)))
	 */
	fixnum_heap(&x, 1);
	Return(ash_integer_common_(local, x, y, &y));
	Return(oneminus_integer_common_(local, y, &y));
	Return(integer_result_local_(local, y, &y));
	fixnum_heap(&x, 0);
	type4_local(local, LISPDECL_INTEGER, Nil, x, Nil, y, value);

	return Result(ret, 1);
}

/* bit -> (integer 0 1) */
static int check_bit_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_BIT);
	return 0;
}
static int optimize_bit_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr left;

	Return_check_optimize(check_bit_, right, ret);
	fixnum_local(local, &left, 0);
	fixnum_local(local, &right, 1);
	type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, value);

	return Result(ret, 1);
}

/* fixnum -> (integer most-negative-fixnum most-positive-fixnum) */
static int check_fixnum_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_FIXNUM);
	return 0;
}
static int optimize_fixnum_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr left;

	Return_check_optimize(check_fixnum_, right, ret);
	GetConst(FIXNUM_MIN, &left);
	GetConst(FIXNUM_MAX, &right);
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type left error");
	type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, value);

	return Result(ret, 1);
}

/* bignum -> (and integer (not fixnum)) */
static int check_bignum_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_BIGNUM);
	return 0;
}
static int optimize_bignum_(LocalRoot local, addr right, addr *value, int *ret)
{
	int ignore;
	addr array, pos;

	Return_check_optimize(check_bignum_, right, ret);
	vector4_local(local, &array, 2);
	/* integer */
	type4aster_localall(local, LISPDECL_INTEGER, &pos);
	SetArrayA4(array, 0, pos);
	/* (not fixnum) */
	type0_local(local, LISPDECL_FIXNUM, &pos);
	SetNotDecl(pos, 1);
	Return(type_optimize_(local, pos, &pos, &ignore));
	SetArrayA4(array, 1, pos);
	/* bignum */
	type1_local(local, LISPDECL_AND, array, value);

	return Result(ret, 1);
}

/* (integer (10) (20)) -> (integer 9 19) */
static int check_integer_p(addr pos)
{
	return (! type_asterisk_p(pos)) && (pos != Nil);
}

static int check_integer_(addr type, int *ret)
{
	addr pos;

	if (RefLispDecl(type) != LISPDECL_INTEGER)
		return Result(ret, 0);

	/* left */
	GetArrayType(type, 0, &pos);
	if (check_integer_p(pos))
		return Result(ret, 1);

	/* right */
	GetArrayType(type, 2, &pos);
	if (check_integer_p(pos))
		return Result(ret, 1);

	/* else */
	return Result(ret, 0);
}

static int optimize_integer_less_p(addr x, addr y, int *ret)
{
	if (integerp(x) && integerp(y))
		return less_integer_(y, x, ret);

	return Result(ret, 0);
}

static int optimize_integer_(LocalRoot local, addr type, addr *value, int *ret)
{
	int check;
	addr a1, a2, v1, v2, pos;

	Return_check_optimize(check_integer_, type, ret);
	GetArrayType(type, 0, &a1);
	GetArrayType(type, 1, &v1);
	GetArrayType(type, 2, &a2);
	GetArrayType(type, 3, &v2);

	/* left */
	GetArrayType(type, 0, &pos);
	if (check_integer_p(pos)) {
		Return(oneplus_integer_common_(local, v1, &v1));
		Return(integer_result_local_(local, v1, &v1));
		a1 = Nil;
	}

	/* right */
	GetArrayType(type, 2, &pos);
	if (check_integer_p(pos)) {
		Return(oneminus_integer_common_(local, v2, &v2));
		Return(integer_result_local_(local, v2, &v2));
		a2 = Nil;
	}

	/* result */
	Return(optimize_integer_less_p(v1, v2, &check));
	if (check)
		type0_local(local, LISPDECL_NIL, value);
	else
		type4_local(local, LISPDECL_INTEGER, a1, v1, a2, v2, value);
	type_setnotobject(*value, type);

	return Result(ret, 1);
}

/* (eql nil) -> null */
static int check_eql_(addr right, int *ret)
{
	if (RefLispDecl(right) != LISPDECL_EQL)
		return Result(ret, 0);
	GetArrayType(right, 0, &right);
	return Result(ret, right == Nil);
}
static int optimize_eql_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_eql_, right, ret);
	type0_local(local, LISPDECL_NULL, value);
	return Result(ret, 1);
}

/* (eql 10) -> (integer 10 10) */
static void optimize_eql_range_object(
		LocalRoot local, enum LISPDECL decl, addr pos, addr *value)
{
	type4_local(local, decl, Nil, pos, Nil, pos, value);
}

static int optimize_eql_range_type_(LocalRoot local, addr pos, addr *value, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			optimize_eql_range_object(local, LISPDECL_INTEGER, pos, value);
			break;

		case LISPTYPE_RATIO:
			optimize_eql_range_object(local, LISPDECL_RATIONAL, pos, value);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			optimize_eql_range_object(local, LISPDECL_SINGLE_FLOAT, pos, value);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			optimize_eql_range_object(local, LISPDECL_DOUBLE_FLOAT, pos, value);
			break;

		case LISPTYPE_LONG_FLOAT:
			optimize_eql_range_object(local, LISPDECL_LONG_FLOAT, pos, value);
			break;

		default:
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int check_eql_range_(addr right, int *ret)
{
	enum LISPTYPE type;

	if (RefLispDecl(right) != LISPDECL_EQL)
		return Result(ret, 0);
	GetArrayType(right, 0, &right);
	type = GetType(right);
	*ret = type == LISPTYPE_FIXNUM
		|| type == LISPTYPE_BIGNUM
		|| type == LISPTYPE_RATIO
		|| type == LISPTYPE_SINGLE_FLOAT
		|| type == LISPTYPE_DOUBLE_FLOAT
		|| type == LISPTYPE_LONG_FLOAT;
	return 0;
}
static int optimize_eql_range_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_eql_range_, right, ret);
	GetArrayType(right, 0, &right);
	return optimize_eql_range_type_(local, right, value, ret);
}

/* (member) -> nil */
static int check_member1_(addr right, int *ret)
{
	if (RefLispDecl(right) != LISPDECL_MEMBER)
		return Result(ret, 0);
	GetArrayType(right, 0, &right);
	return Result(ret, LenArrayA4r(right) == 0);
}
static int optimize_member1_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_member1_, right, ret);
	type0_local(local, LISPDECL_NIL, value);
	return Result(ret, 1);
}

/* (member arg) -> (eql arg) */
static int check_member2_(addr right, int *ret)
{
	if (RefLispDecl(right) != LISPDECL_MEMBER)
		return Result(ret, 0);
	GetArrayType(right, 0, &right);
	return Result(ret, LenArrayA4r(right) == 1);
}
static int optimize_member2_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_member2_, right, ret);
	GetArrayType(right, 0, &right);
	GetArrayA4(right, 0, &right);
	type_eql_local(local, right, value);
	return Result(ret, 1);
}

/* (member ...) -> (or (eql arg1) (eql arg2) ...) */
static int check_member3_(addr right, int *ret)
{
	if (RefLispDecl(right) != LISPDECL_MEMBER)
		return Result(ret, 0);
	GetArrayType(right, 0, &right);
	return Result(ret, 2 <= LenArrayA4r(right));
}
static int optimize_member3_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr array, child;
	size_t i, size;

	Return_check_optimize(check_member3_, right, ret);
	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &child);
		type_eql_local(local, child, &child);
		SetArrayA4(array, i, child);
	}
	type1_local(local, LISPDECL_OR, array, value);

	return Result(ret, 1);
}

/* (not x) -> x.not */
static int check_not_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_NOT);
	return 0;
}
static int optimize_not_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_not_, right, ret);
	if (RefNotDecl(right)) {
		/* not not */
		GetArrayType(right, 0, value);
	}
	else {
		/* not */
		GetArrayType(right, 0, &right);
		type_copy_unsafe_local(local, &right, right);
		type_revnotdecl(right);
		*value = right;
	}

	return Result(ret, 1);
}

/* (not (and ... )) -> (or (not ...) (not ...) ...) */
/* (not (or ... )) -> (and (not ...) (not ...) ...) */
static int optimize_result_(LocalRoot local, addr pos, addr *value, int *ret)
{
	int check;
	addr opt;

	Return(type_optimize_(local, pos, &opt, &check));
	*value = check? opt: pos;

	if (ret)
		return Result(ret, check);
	else
		return 0;
}

static int extract_not_andor_(LocalRoot local,
		addr *value, addr right, enum LISPDECL decl)
{
	addr array, pos;
	size_t size, i;

	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &pos);
		type_copy_unsafe_local(local, &pos, pos);
		type_revnotdecl(pos);
		Return(optimize_result_(local, pos, &pos, NULL));
		SetArrayA4(array, i, pos);
	}
	type1_local(local, decl, array, value);

	return 0;
}

static int extract_array_andor_(LocalRoot local, addr right, addr *value, int *ret)
{
	int update, check;
	addr array, temp, pos;
	size_t size, i;

	GetArrayType(right, 0, &array);
	LenArrayA4(array, &size);
	vector4_local(local, &temp, size);
	update = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(array, i, &pos);
		Return(optimize_result_(local, pos, &pos, &check));
		update |= check;
		SetArrayA4(temp, i, pos);
	}

	if (update) {
		type_copy_unsafe_local(local, &right, right);
		vector4_local(local, &array, size);
		for (i = 0; i < size; i++) {
			GetArrayA4(temp, i, &pos);
			SetArrayA4(array, i, pos);
		}
		SetArrayType(right, 0, array);
		*value = right;
	}

	return Result(ret, update);
}

static int extract_andor_(LocalRoot local,
		addr right, addr *value, int *ret,
		enum LISPDECL fromdecl, enum LISPDECL todecl)
{
	if (RefLispDecl(right) != fromdecl)
		return Result(ret, 0);
	if (RefNotDecl(right)) {
		Return(extract_not_andor_(local, value, right, todecl));
		return Result(ret, 1);
	}

	return extract_array_andor_(local, right, value, ret);
}

static int check_andor_(enum LISPDECL decl, addr right, int *ret)
{
	int check;
	addr pos;
	size_t size, i;

	if (RefLispDecl(right) != decl)
		return Result(ret, 0);
	if (RefNotDecl(right))
		return Result(ret, 1);
	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &pos);
		Return(check_optimize_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int check_and_(addr right, int *ret)
{
	return check_andor_(LISPDECL_AND, right, ret);
}
static int optimize_and_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_and_, right, ret);
	return extract_andor_(local, right, value, ret, LISPDECL_AND, LISPDECL_OR);
}

static int check_or_(addr right, int *ret)
{
	return check_andor_(LISPDECL_OR, right, ret);
}
static int optimize_or_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_or_, right, ret);
	return extract_andor_(local, right, value, ret, LISPDECL_OR, LISPDECL_AND);
}


/*
 *  and
 */
static int normlispdecl(addr pos, enum LISPDECL type)
{
	return RefLispDecl(pos) == type && (! RefNotDecl(pos));
}

static int check_typeand(addr type, addr *array, size_t *size)
{
	if (! normlispdecl(type, LISPDECL_AND))
		return 1;
	GetArrayType(type, 0, array);
	LenArrayA4(*array, size);
	return 0;
}

/* (and) -> t */
static int check_and1_(addr type, int *ret)
{
	if (! normlispdecl(type, LISPDECL_AND))
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	return Result(ret, LenArrayA4r(type) == 0);
}
static int optimize_and1_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_and1_, type, ret);
	type0_local(local, LISPDECL_T, value);
	return Result(ret, 1);
}

/* (and type) -> type */
static int check_and2_(addr type, int *ret)
{
	if (! normlispdecl(type, LISPDECL_AND))
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	return Result(ret, LenArrayA4r(type) == 1);
}
static int optimize_and2_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_and2_, type, ret);
	GetArrayType(type, 0, &type);
	GetArrayA4(type, 0, value);
	return Result(ret, 1);
}

/* (and ... nil ...) -> nil */
static int check_and_vector_(enum LISPDECL decl, addr type, int *ret)
{
	addr check;
	size_t size, i;

	if (check_typeand(type, &type, &size))
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, decl))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
static int check_and3_(addr type, int *ret)
{
	return check_and_vector_(LISPDECL_NIL, type, ret);
}
static int optimize_and3_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_and3_, type, ret);
	type0_local(local, LISPDECL_NIL, value);
	return Result(ret, 1);
}

/* (and ... t ...) -> (and ...)  remove t */
static void remove_type_vector(LocalRoot local,
		enum LISPDECL decl, enum LISPDECL checktype,
		addr array, size_t size1, size_t size2, addr *value)
{
	addr pos, check;
	size_t i, k;

	vector4_local(local, &pos, size2);
	k = 0;
	for (i = 0; i < size1; i++) {
		GetArrayA4(array, i, &check);
		if (! normlispdecl(check, checktype)) {
			Check(size2 <= k, "size2 error1");
			SetArrayA4(pos, k++, check);
		}
	}
	Check(k != size2, "size2 error2");
	type1_local(local, decl, pos, value);
}

static int check_and4_(addr type, int *ret)
{
	return check_and_vector_(LISPDECL_T, type, ret);
}
static int optimize_and4_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr check;
	size_t size, i, count;

	Return_check_optimize(check_and4_, type, ret);
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	count = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, LISPDECL_T))
			count++;
	}
	Check(count == 0, "size error");
	remove_type_vector(local, LISPDECL_AND, LISPDECL_T,
			type, size, size - count, value);

	return Result(ret, 1);
}

/* (and ... (and ...) ...) -> (and ...) */
static int count_andor(addr type, enum LISPDECL decl, size_t *index)
{
	int result;
	addr check;
	size_t size, i;

	Check(! normlispdecl(type, decl), "type error");
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	result = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, decl)) {
			result = 1;
			count_andor(check, decl, index);
		}
		else {
			(*index)++;
		}
	}

	return result;
}

static void replace_andor(addr type, enum LISPDECL decl, addr array, size_t *index)
{
	addr check;
	size_t size, i;

	Check(! normlispdecl(type, decl), "type error");
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, decl)) {
			replace_andor(check, decl, array, index);
		}
		else {
			SetArrayA4(array, (*index)++, check);
		}
	}
}

static int check_andor_type_(enum LISPDECL decl, addr type, int *ret)
{
	addr pos;
	size_t size, i;

	if (! normlispdecl(type, decl))
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &pos);
		if (normlispdecl(pos, decl))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
static int check_and5_(addr type, int *ret)
{
	return check_andor_type_(LISPDECL_AND, type, ret);
}
static int optimize_and5_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr array;
	size_t size;

	/* check */
	Return_check_optimize(check_and5_, type, ret);
	GetArrayType(type, 0, &array);
	size = 0;
	count_andor(type, LISPDECL_AND, &size);
	Check(size == 0, "size error");

	/* make type */
	vector4_local(local, &array, size);
	type1_local(local, LISPDECL_AND, array, value);
	size = 0;
	replace_andor(type, LISPDECL_AND, array, &size);

	return Result(ret, 1);
}


/*
 *  or
 */
/* (or) -> nil */
static int check_typeor(addr type, addr *array, size_t *size)
{
	if (! normlispdecl(type, LISPDECL_OR))
		return 1;
	GetArrayType(type, 0, array);
	LenArrayA4(*array, size);
	return 0;
}

static int check_or1_(addr type, int *ret)
{
	if (! normlispdecl(type, LISPDECL_OR))
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	return Result(ret, LenArrayA4r(type) == 0);
}
static int optimize_or1_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_or1_, type, ret);
	type0_local(local, LISPDECL_NIL, value);
	return Result(ret, 1);
}

/* (or type) -> type */
static int check_or2_(addr type, int *ret)
{
	if (! normlispdecl(type, LISPDECL_OR))
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	return Result(ret, LenArrayA4r(type) == 1);
}
static int optimize_or2_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_or2_, type, ret);
	GetArrayType(type, 0, &type);
	GetArrayA4(type, 0, value);
	return Result(ret, 1);
}

/* (or ... t ...) -> t */
static int check_or_vector_(enum LISPDECL decl, addr type, int *ret)
{
	addr check;
	size_t size, i;

	if (check_typeor(type, &type, &size))
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, decl))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
static int check_or3_(addr type, int *ret)
{
	return check_or_vector_(LISPDECL_T, type, ret);
}
static int optimize_or3_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_or3_, type, ret);
	type0_local(local, LISPDECL_T, value);
	return Result(ret, 1);
}

/* (or ... nil ...) -> (or ...)  remove nil */
static int check_or4_(addr type, int *ret)
{
	return check_or_vector_(LISPDECL_NIL, type, ret);
}
static int optimize_or4_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr check;
	size_t size, i, count;

	Return_check_optimize(check_or4_, type, ret);
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	count = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, LISPDECL_NIL))
			count++;
	}
	Check(count == 0, "size error");
	remove_type_vector(local, LISPDECL_OR, LISPDECL_NIL,
			type, size, size - count, value);

	return Result(ret, 1);
}

/* (or ... (or ...) ...) -> (or ...) */
static int check_or5_(addr type, int *ret)
{
	return check_andor_type_(LISPDECL_OR, type, ret);
}
static int optimize_or5_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr array;
	size_t size;

	/* check */
	Return_check_optimize(check_or5_, type, ret);
	GetArrayType(type, 0, &array);
	LenArrayA4(array, &size);
	size = 0;
	count_andor(type, LISPDECL_OR, &size);
	Check(size == 0, "size error");

	/* make type */
	vector4_local(local, &array, size);
	type1_local(local, LISPDECL_OR, array, value);
	size = 0;
	replace_andor(type, LISPDECL_OR, array, &size);

	return Result(ret, 1);
}

/* range check */
static int range_valid_p_(addr type, int *ret)
{
	addr left1, left2, right1, right2;
	LocalRoot local;

	local = Local_Thread;
	GetArrayType(type, 0, &left1);
	GetArrayType(type, 1, &left2);
	GetArrayType(type, 2, &right1);
	GetArrayType(type, 3, &right2);
	if (type_asterisk_p(left1) || type_asterisk_p(right1))
		return Result(ret, 1);
	if (left1 == Nil && right1 == Nil)
		return less_equal_real_(local, left2, right2, ret);
	else
		return less_real_(local, left2, right2, ret);
}
static int check_range_(addr right, int *ret)
{
	int check;

	if (! type_range_p(right))
		return Result(ret, 0);
	Return(range_valid_p_(right, &check));
	return Result(ret, ! check);
}
static int optimize_range_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_range_, right, ret);
	if (RefNotDecl(right))
		type0_local(local, LISPDECL_T, value);
	else
		type0_local(local, LISPDECL_NIL, value);

	return Result(ret, 1);
}


/*
 *  wake optimize
 */
static int extract_values_var_(LocalRoot local, addr right, addr *value)
{
	addr root, left;

	for (root = Nil; right != Nil; ) {
		GetCons(right, &left, &right);
		Return(optimize_result_(local, left, &left, NULL));
		cons_local(local, &root, left, root);
	}
	nreverse(value, root);

	return 0;
}

static int extract_values_rest_(LocalRoot local, addr right, addr *value)
{
	int ignore;

	if (right == Nil)
		return 0;
	else
		return optimize_result_(local, right, value, &ignore);
}

static int check_some_(addr right, int *ret)
{
	int check;
	addr pos;

	while (right != Nil) {
		GetCons(right, &pos, &right);
		Return(check_optimize_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int check_values_rest_(addr right, int *ret)
{
	if (right == Nil)
		return Result(ret, 0);
	else
		return check_optimize_(right, ret);
}

static int check_values_(addr right, int *ret)
{
	int check;
	addr value;

	if (RefLispDecl(right) != LISPDECL_VALUES)
		return Result(ret, 0);
	GetArrayType(right, 0, &value);
	Return(check_some_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayType(right, 1, &value);
	Return(check_some_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayType(right, 2, &value);
	return check_values_rest_(value, ret);
}

static int optimize_values_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr var, opt, rest;

	/* extract */
	Return_check_optimize(check_values_, right, ret);
	GetArrayType(right, 0, &var);
	GetArrayType(right, 1, &opt);
	GetArrayType(right, 2, &rest);
	Return(extract_values_var_(local, var, &var));
	Return(extract_values_var_(local, opt, &opt));
	Return(extract_values_rest_(local, rest, &rest));

	/* result */
	type_copy_unsafe_local(local, &right, right);
	SetArrayType(right, 0, var);
	SetArrayType(right, 1, opt);
	SetArrayType(right, 2, rest);
	*value = right;

	return Result(ret, 1);
}

static int extract_function_key_(LocalRoot local, addr right, addr *value)
{
	addr root, left, key, type;

	if (right == T)
		return Result(value, T);
	for (root = Nil; right != Nil; ) {
		GetCons(right, &left, &right);
		GetCons(left, &key, &type);
		Return(optimize_result_(local, type, &type, NULL));
		cons_local(local, &left, key, type);
		cons_local(local, &root, left, root);
	}
	nreverse(value, root);

	return 0;
}

static int extract_function_(LocalRoot local, addr right, addr *value)
{
	addr var, opt, rest, key;

	/* extract */
	if (type_asterisk_p(right))
		return 0;
	GetArrayA2(right, 0, &var);
	GetArrayA2(right, 1, &opt);
	GetArrayA2(right, 2, &rest);
	GetArrayA2(right, 3, &key);
	Return(extract_values_var_(local, var, &var));
	Return(extract_values_var_(local, opt, &opt));
	Return(extract_values_rest_(local, rest, &rest));
	Return(extract_function_key_(local, key, &key));

	/* result */
	vector2_local(local, &right, 4);
	SetArrayA2(right, 0, var);
	SetArrayA2(right, 1, opt);
	SetArrayA2(right, 2, rest);
	SetArrayA2(right, 3, key);

	return Result(value, right);
}

static int check_function_key_(addr right, int *ret)
{
	int check;
	addr type;

	if (right == T)
		return Result(ret, 0);
	while (right != Nil) {
		GetCons(right, &type, &right);
		GetCdr(type, &type);
		Return(check_optimize_(type, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
static int check_function_args_(addr right, int *ret)
{
	int check;
	addr value;

	if (type_asterisk_p(right))
		return Result(ret, 0);
	GetArrayA2(right, 0, &value);
	Return(check_some_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayA2(right, 1, &value);
	Return(check_some_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayA2(right, 2, &value);
	Return(check_values_rest_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayA2(right, 3, &value);
	return check_function_key_(value, ret);
}
static int check_function_(addr right, int *ret)
{
	int check;
	enum LISPDECL decl;
	addr value;

	decl = RefLispDecl(right);
	if (decl != LISPDECL_FUNCTION && decl != LISPDECL_COMPILED_FUNCTION)
		return Result(ret, 0);
	GetArrayType(right, 0, &value);
	Return(check_function_args_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayType(right, 1, &value);

	return check_optimize_(value, ret);
}
static int optimize_function_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr args, values;

	Return_check_optimize(check_function_, right, ret);
	GetArrayType(right, 0, &args);
	GetArrayType(right, 1, &values);
	Return(extract_function_(local, args, &args));
	Return(optimize_result_(local, values, &values, NULL));
	type_copydecl_unsafe_local(local, &right, right);
	SetArrayType(right, 0, args);
	SetArrayType(right, 1, values);
	*value = right;

	return Result(ret, 1);
}

static int check_cons_(addr right, int *ret)
{
	int check;
	addr value;

	if (RefLispDecl(right) != LISPDECL_CONS)
		return Result(ret, 0);
	GetArrayType(right, 0, &value);
	if (! type_asterisk_p(value)) {
		Return(check_optimize_(value, &check));
		if (check)
			return Result(ret, 1);
	}
	GetArrayType(right, 1, &value);
	if (! type_asterisk_p(value)) {
		Return(check_optimize_(value, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
static int optimize_cons(LocalRoot local, addr right, addr *value, int *ret)
{
	addr car, cdr;

	Return_check_optimize(check_cons_, right, ret);
	GetArrayType(right, 0, &car);
	GetArrayType(right, 1, &cdr);
	if (! type_asterisk_p(car)) {
		Return(optimize_result_(local, car, &car, NULL));
	}
	if (! type_asterisk_p(cdr)) {
		Return(optimize_result_(local, cdr, &cdr, NULL));
	}
	type2_local(local, LISPDECL_CONS, car, cdr, value);

	return Result(ret, 1);
}


/*
 *  type-optimize
 */
static int check_optimize_(addr type, int *ret)
{
	Return_or_optimize(check_type_delay_, type, ret);
	Return_or_optimize(check_optimized_, type, ret);
	Return_or_optimize(check_not_asterisk_, type, ret);
	Return_or_optimize(check_not_nil_, type, ret);
	Return_or_optimize(check_not_t_, type, ret);
	Return_or_optimize(check_mod_, type, ret);
	Return_or_optimize(check_atom_, type, ret);
	Return_or_optimize(check_list_, type, ret);
	Return_or_optimize(check_boolean_, type, ret);
	Return_or_optimize(check_sequence_, type, ret);
	Return_or_optimize(check_vector_, type, ret);
	Return_or_optimize(check_simple_vector_, type, ret);
	Return_or_optimize(check_bit_vector_, type, ret);
	Return_or_optimize(check_simple_bit_vector_, type, ret);
	Return_or_optimize(check_extended_char_, type, ret);
	Return_or_optimize(check_string_, type, ret);
	Return_or_optimize(check_base_string_, type, ret);
	Return_or_optimize(check_simple_string_, type, ret);
	Return_or_optimize(check_simple_base_string_, type, ret);
	Return_or_optimize(check_signed_byte_, type, ret);
	Return_or_optimize(check_unsigned_byte_, type, ret);
	Return_or_optimize(check_bit_, type, ret);
	Return_or_optimize(check_fixnum_, type, ret);
	Return_or_optimize(check_bignum_, type, ret);
	Return_or_optimize(check_integer_, type, ret);
	Return_or_optimize(check_eql_, type, ret);
	Return_or_optimize(check_eql_range_, type, ret);
	Return_or_optimize(check_member1_, type, ret);
	Return_or_optimize(check_member2_, type, ret);
	Return_or_optimize(check_member3_, type, ret);
	Return_or_optimize(check_not_, type, ret);
	Return_or_optimize(check_and_, type, ret);
	Return_or_optimize(check_or_, type, ret);
	Return_or_optimize(check_and1_, type, ret);
	Return_or_optimize(check_and2_, type, ret);
	Return_or_optimize(check_and3_, type, ret);
	Return_or_optimize(check_and4_, type, ret);
	Return_or_optimize(check_and5_, type, ret);
	Return_or_optimize(check_or1_, type, ret);
	Return_or_optimize(check_or2_, type, ret);
	Return_or_optimize(check_or3_, type, ret);
	Return_or_optimize(check_or4_, type, ret);
	Return_or_optimize(check_or5_, type, ret);
	Return_or_optimize(check_range_, type, ret);
	Return_or_optimize(check_values_, type, ret);
	Return_or_optimize(check_function_, type, ret);
	Return_or_optimize(check_cons_, type, ret);

	return Result(ret, 0);
}

static int type_optimize_(LocalRoot local, addr type, addr *value, int *ret)
{
	int update, loop;

	CheckType(type, LISPTYPE_TYPE);
	for (loop = 0; ; loop |= update) {
		update = 0;
		/* extract */
		extractcall(local, optimize_type_delay_, type, update);
		extractcall(local, optimize_optimized_, type, update);
		extractcall(local, optimize_not_asterisk_, type, update);
		extractcall(local, optimize_not_nil_, type, update);
		extractcall(local, optimize_not_t_, type, update);
		extractcallnot(local, optimize_mod_, type, update);
		extractcallnot(local, optimize_atom_, type, update);
		extractcallnot(local, optimize_list_, type, update);
		extractcallnot(local, optimize_boolean_, type, update);
		extractcallnot(local, optimize_sequence_, type, update);
		extractcallnot(local, optimize_vector_, type, update);
		extractcallnot(local, optimize_simple_vector_, type, update);
		extractcallnot(local, optimize_bit_vector_, type, update);
		extractcallnot(local, optimize_simple_bit_vector_, type, update);
		extractcallnot(local, optimize_extended_char_, type, update);
		extractcallnot(local, optimize_string_, type, update);
		extractcallnot(local, optimize_base_string_, type, update);
		extractcallnot(local, optimize_simple_string_, type, update);
		extractcallnot(local, optimize_simple_base_string_, type, update);
		extractcallnot(local, optimize_signed_byte_, type, update);
		extractcallnot(local, optimize_unsigned_byte_, type, update);
		extractcallnot(local, optimize_bit_, type, update);
		extractcallnot(local, optimize_fixnum_, type, update);
		extractcallnot(local, optimize_bignum_, type, update);
		extractcallnot(local, optimize_integer_, type, update);
		extractcallnot(local, optimize_eql_, type, update);
		extractcallnot(local, optimize_eql_range_, type, update);
		extractcallnot(local, optimize_member1_, type, update);
		extractcallnot(local, optimize_member2_, type, update);
		extractcallnot(local, optimize_member3_, type, update);
		extractcall(local, optimize_not_, type, update);
		extractcall(local, optimize_and_, type, update);
		extractcall(local, optimize_or_, type, update);
		extractcall(local, optimize_and1_, type, update);
		extractcall(local, optimize_and2_, type, update);
		extractcall(local, optimize_and3_, type, update);
		extractcall(local, optimize_and4_, type, update);
		extractcall(local, optimize_and5_, type, update);
		extractcall(local, optimize_or1_, type, update);
		extractcall(local, optimize_or2_, type, update);
		extractcall(local, optimize_or3_, type, update);
		extractcall(local, optimize_or4_, type, update);
		extractcall(local, optimize_or5_, type, update);
		extractcall(local, optimize_range_, type, update);
		extractcallnot(local, optimize_values_, type, update);
		extractcallnot(local, optimize_function_, type, update);
		extractcallnot(local, optimize_cons, type, update);
		if (update == 0)
			break;
	}
	*value = type;

	return Result(ret, loop);
}

int type_optimize_local_(LocalRoot local, addr type, addr *value, int *ret)
{
	CheckLocal(local);
	CheckType(type, LISPTYPE_TYPE);
	if (RefLispDecl(type) == LISPDECL_OPTIMIZED) {
		*value = type;
		return Result(ret, 0);
	}

	Return(type_optimize_(local, type, &type, ret));
	type1_local(local, LISPDECL_OPTIMIZED, type, value);
	return 0;
}

int type_optimize_heap_(LocalRoot local, addr type, addr *value, int *ret)
{
	LocalStack stack;

	CheckLocal(local);
	CheckType(type, LISPTYPE_TYPE);
	push_local(local, &stack);
	Return(type_optimize_local_(local, type, &type, ret));
	type_copy_heap(value, type);
	rollback_local(local, stack);

	return 0;
}

int type_optimized_p(addr type)
{
	CheckType(type, LISPTYPE_TYPE);
	return RefLispDecl(type) == LISPDECL_OPTIMIZED;
}

void get_type_optimized(addr *ret, addr type)
{
	if (type_optimized_p(type)) {
		GetArrayType(type, 0, ret);
	}
	else {
		*ret = type;
	}
}

int type_optimize_throw_heap_(LocalRoot local, addr type, addr *ret)
{
	int check;

	Return(type_optimize_heap_(local, type, &type, &check));
	get_type_optimized(ret, type);

	return 0;
}


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
	Return(subtypep_call_cons_t_(ptr, cdr1, cdr2, &value2));

	if (value1 == SUBTYPEP_INCLUDE && value2 == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);
	if (value1 == SUBTYPEP_INVALID || value2 == SUBTYPEP_INVALID)
		return ReturnInvalid(ret);
	if (value1 == SUBTYPEP_EXCLUDE || value2 == SUBTYPEP_EXCLUDE)
		return ReturnExclude(ret);

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
	TypeSubtypep[LISPDECL_PIPE_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_BYTESPEC] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_PRINT_DISPATCH] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_PAPER] = subtypep_call_eqltype_;
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
	fixed *data;
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

static int Symbol_Init = 0;
static rwlocklite Symbol_Mutex;

int init_symbol(void)
{
	if (Symbol_Init) {
		Debug("Symbol_Init error.");
		return 1;
	}
	if (lispd_make_rwlocklite(&Symbol_Mutex)) {
		Debug("lispd_make_mutexlite error");
		return 1;
	}
	Symbol_Init = 1;

	return 0;
}

void free_symbol(void)
{
	if (Symbol_Init) {
		lispd_destroy_rwlocklite(&Symbol_Mutex);
		Symbol_Init = 0;
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

void getstructure_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_COMMON_STRUCTURE, ret);
}

void setstructure_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(GetType(value) != LISPSYSTEM_STRUCTURE, "type error");
	setinfo_nocheck_constant(symbol, CONSTANT_COMMON_STRUCTURE, value);
}

void remstructure_symbol(addr symbol)
{
	CheckSymbol(symbol);
	reminfo_nocheck_constant(symbol, CONSTANT_COMMON_STRUCTURE);
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
	lispd_rdlock_rwlocklite(&Symbol_Mutex);
	GetSpecialSymbol_Low(symbol, &stack);
	size = GetLenArrayA4(stack);
	if (index < size) {
		*ret = stack;
		lispd_unrdlock_rwlocklite(&Symbol_Mutex);
		return;
	}

	/* write lock */
	lispd_unrdlock_rwlocklite(&Symbol_Mutex);
	lispd_wrlock_rwlocklite(&Symbol_Mutex);
	/* reload */
	GetSpecialSymbol_Low(symbol, &stack);
	size = GetLenArrayA4(stack);
	if (index < size) {
		*ret = stack;
		lispd_unwrlock_rwlocklite(&Symbol_Mutex);
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
	lispd_unwrlock_rwlocklite(&Symbol_Mutex);
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
	Return(defconstant_syscode_(symbol, value, doc));
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


/* (defun in-package (string-designator) ...) -> package */
static int syscall_in_package(Execute ptr, addr name)
{
	Return(in_package_syscode_(ptr, name, &name));
	setresult_control(ptr, name);
	return 0;
}

static void type_syscall_in_package(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringDesignator);
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

	GetTypeTable(&args, PackageDesignator);
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
 *   name                    string-designator
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
	Return(defpackage_syscode_(ptr, var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_defpackage(addr *ret)
{
	addr args, values;
	addr key, key1, key2, key3, key4, key5, key6, key7, key8, key9, key10;

	GetTypeTable(&args, StringDesignator);
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
	Return(do_symbols_syscode_(ptr, call, package));
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
	Return(do_external_symbols_syscode_(ptr, call, package));
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
	Return(prompt_for_syscode_(ptr, type, args, &type));
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
	Return(print_unreadable_call_syscode_(ptr, stream, pos, type, identity, body));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_print_unreadable_call(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, StreamDesignator);
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
	Return(write_default_syscode_(ptr, stream, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_write_default(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StreamDesignator);
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
	Return(structure_constructor_syscode_(ptr, symbol, rest, &rest));
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
	Return(loop_bind_syscode_(ptr, a, b, c, &a));
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
	Return(pprint_gensym_syscode_(stream, &stream));
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
	Return(pprint_exit_syscode_(ptr, stream));
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
	Return(pprint_pop_syscode_(ptr, stream, &stream));
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
	Return(pprint_check_syscode_(ptr, stream));
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
	Return(pprint_close_syscode_(ptr, stream));
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
	Return(pprint_pretty_syscode_(ptr, stream, call));
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
	Return(set_slots_syscode_(var, slots, values));
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
	Return(intern_eql_specializer_syscode_(var, &var));
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


/* (defun defgeneric-define (symbol &rest &key &allow-other-keys) ...) -> instance */
static int syscall_defgeneric_define(Execute ptr, addr var, addr args)
{
	Return(defgeneric_define_syscode_(ptr, var, args, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_defgeneric_define(addr *ret)
{
	addr args, values;
	addr key, key1, key2, key3, key4, key5, key6, key7;

	KeyTypeTable(&key1, ARGUMENT_PRECEDENCE_ORDER, T);
	KeyTypeTable(&key2, DECLARE, T);
	KeyTypeTable(&key3, DOCUMENTATION, String);
	KeyTypeTable(&key4, LAMBDA_LIST, T);
	KeyTypeTable(&key5, GENERIC_FUNCTION_CLASS, T);
	KeyTypeTable(&key6, METHOD_CLASS, T);
	KeyTypeTable(&key7, METHOD_COMBINATION, T);
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, NULL);
	/* type */
	GetTypeTable(&args, T);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, GenericFunction);
	type_compiled_heap(args, values, ret);
}

static void defun_defgeneric_define(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFGENERIC_DEFINE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_defgeneric_define);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_defgeneric_define(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun defgeneric-method (instance &rest args) ...) -> instance */
static int syscall_defgeneric_method(Execute ptr, addr var, addr args)
{
	Return(defgeneric_method_syscode_(var, args));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_defgeneric_method(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, GenericFunction);
	GetTypeTable(&values, Method);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, GenericFunction);
	type_compiled_heap(args, values, ret);
}

static void defun_defgeneric_method(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFGENERIC_METHOD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_defgeneric_method);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_defgeneric_method(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun condition-restarts-push (condition restarts) ...) -> null */
static int syscall_condition_restarts_push(Execute ptr, addr var, addr list)
{
	Return(condition_restarts_push_syscode_(var, list));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_condition_restarts_push(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_condition_restarts_push(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_CONDITION_RESTARTS_PUSH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_condition_restarts_push);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_condition_restarts_push(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun condition-restarts-pop (condition restarts) ...) -> null */
static int syscall_condition_restarts_pop(Execute ptr, addr var, addr list)
{
	Return(condition_restarts_pop_syscode_(var, list));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_condition_restarts_pop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_CONDITION_RESTARTS_POP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_condition_restarts_pop);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_condition_restarts_push(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun condition-restarts-make (type &rest args) ...) -> null */
static int syscall_condition_restarts_make(Execute ptr, addr var, addr list)
{
	Return(condition_restarts_make_syscode_(ptr, var, list, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_condition_restarts_make(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);

	/* values condition */
	GetConst(CLOS_CONDITION, &values);
	CheckType(values, LISPTYPE_CLOS);
	type_clos_heap(values, &values);

	/* compiled-function */
	type_compiled_heap(args, values, ret);
}

static void defun_condition_restarts_make(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_CONDITION_RESTARTS_MAKE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_syscall_condition_restarts_make);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_condition_restarts_make(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-restart (name function &key
 *     interactive-function report-function test-function escape)
 *   ...) -> restart
 */
static int syscall_make_restart(Execute ptr, addr var, addr call, addr list)
{
	Return(make_restart_syscode_(var, call, list, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_make_restart(addr *ret)
{
	addr args, call, values;
	addr key, key1, key2, key3, key4;
	addr type, type1, type2, type3;

	/* (or function null string) */
	GetTypeTable(&type1, Function);
	GetTypeTable(&type2, Null);
	GetTypeTable(&type3, String);
	type3or_heap(type1, type2, type3, &type);

	/* compiled */
	GetTypeTable(&args, Symbol);
	GetTypeTable(&call, Function);
	KeyTypeTable(&key1, INTERACTIVE_FUNCTION, FunctionNull);
	GetConst(KEYWORD_REPORT_FUNCTION, &key2);
	cons_heap(&key2, key2, type);
	KeyTypeTable(&key3, TEST_FUNCTION, FunctionNull);
	KeyTypeTable(&key4, ESCAPE, T);
	list_heap(&key, key1, key2, key3, key4, NULL);
	typeargs_var2key(&args, args, call, key);
	GetTypeTable(&values, Restart);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_restart(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_RESTART, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_make_restart);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_make_restart(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_syscall_common(void)
{
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
	SetPointerSysCall(defun, var1dynamic, defgeneric_define);
	SetPointerSysCall(defun, var1dynamic, defgeneric_method);
	SetPointerSysCall(defun, var2, condition_restarts_push);
	SetPointerSysCall(defun, var2, condition_restarts_pop);
	SetPointerSysCall(defun, var1rest, condition_restarts_make);
	SetPointerSysCall(defun, var2dynamic, make_restart);
}

void build_syscall_common(void)
{
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
	defun_defgeneric_define();
	defun_defgeneric_method();
	defun_condition_restarts_push();
	defun_condition_restarts_pop();
	defun_condition_restarts_make();
	defun_make_restart();
}


/************************************************************
 *  syscall_function.c
 ************************************************************/

/* (defun abort-lisp () ...) -> null */
static int syscall_abort_lisp(Execute ptr)
{
	Abort("syscall-abort-lisp");
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_abort_lisp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ABORT_LISP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_syscall_abort_lisp);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeTable(&type, CompiledFunction);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hello () ...) -> null */
static int syscall_hello(Execute ptr)
{
	Return(hello_syscode_(ptr));
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


/* (defun savecore (file &key input (exit t)) ...) -> null
 *   file    pathname-designator
 *   input   pathname-designator
 */
static int syscall_savecore(Execute ptr, addr file, addr rest)
{
	Return(savecore_syscode_(ptr, file, rest));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_savecore(addr *ret)
{
	addr args, values, key, key1, key2;

	/* key */
	KeyTypeTable(&key1, INPUT, PathnameDesignatorBoolean);
	KeyTypeTable(&key2, EXIT, T);
	list_heap(&key, key1, key2, NULL);
	/* type */
	GetTypeTable(&args, PathnameDesignatorNull);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_savecore(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SAVECORE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_savecore);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_savecore(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun loadcore (file &key output (exit t)) ...) -> null
 *  file    pathname-designator
 *  output  pathname-designator
 */
static int syscall_loadcore(Execute ptr, addr file, addr rest)
{
	Return(loadcore_syscode_(ptr, file, rest));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_loadcore(addr *ret)
{
	addr args, values, key, key1, key2;

	/* key */
	KeyTypeTable(&key1, OUTPUT, PathnameDesignatorNull);
	KeyTypeTable(&key2, EXIT, T);
	list_heap(&key, key1, key2, NULL);
	/* type */
	GetTypeTable(&args, PathnameDesignatorBoolean);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_loadcore(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_LOADCORE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_loadcore);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_loadcore(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-export-list (package-designator) ...) -> list */
static int syscall_package_export_list(Execute ptr, addr var)
{
	Return(package_export_list_syscode_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_package_export_list(addr *ret)
{
	/* (function (package_designator) (values list &rest nil)) */
	addr args, values;

	GetTypeTable(&args, PackageDesignator);
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
	Return(simple_sort_syscode_(ptr, pos, call, rest));
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
	Return(bubble_sort_syscode_(ptr, pos, call, rest));
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
	Return(quick_sort_syscode_(ptr, pos, call, rest));
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
	Return(merge_sort_syscode_(ptr, pos, call, rest));
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
	Return(make_character_syscode_(var, &var));
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
	Return(make_fixnum_syscode_(var, &var));
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
	Return(make_bignum_syscode_(var, &var));
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
	Return(make_ratio_syscode_(numer, denom, &numer));
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


/* (defun eastasian-set (string-designator intplus &optional error) ...) -> boolean) */
static int syscall_eastasian_set(Execute ptr, addr var, addr value, addr errorp)
{
	Return(eastasian_set_syscode_(var, value, errorp, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_eastasian_set(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, StringDesignator);
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


/* (defun eastasian-get (string-designator) ...) -> (values IntplusNull symbol) */
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

	GetTypeTable(&args, StringDesignator);
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
	Return(remove_file_syscode_(ptr, var, opt, &var));
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
	Return(remove_directory_syscode_(ptr, var, opt, &var));
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
	Return(declare_parse_syscode_(form, &form));
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
	Return(parse_type_syscode_(ptr, var, &var));
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

	GetTypeTable(&args, Unsigned8);
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


/* (defun sysctl (object &rest args) ...) -> (values &rest t) */
static int syscall_sysctl(Execute ptr, addr var, addr args)
{
	return sysctl_syscode_(ptr, var, args);
}

static void type_syscall_sysctl(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&type, T);
	typeargs_var1rest(&args, type, type);
	typevalues_rest(&values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_sysctl(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SYSCTL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_sysctl);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_sysctl(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun terme (object &rest args) ...) -> (values &rest t) */
static int syscall_terme(Execute ptr, addr var, addr args)
{
	return terme_syscode_(ptr, var, args);
}

static void defun_terme(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_TERME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_terme);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_sysctl(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun fpclassify (float) -> symbol */
static int syscall_fpclassify(Execute ptr, addr var)
{
	addr type, sign;

	fpclassify_syscode(var, &type, &sign);
	setvalues_control(ptr, type, sign, NULL);

	return 0;
}

static void type_syscall_fpclassify(addr *ret)
{
	addr args, type1, type2, values;

	GetTypeTable(&args, Float);
	typeargs_var1(&args, args);
	GetTypeTable(&type1, Symbol);
	GetTypeTable(&type2, IntegerNull);
	typevalues_values2(&values, type1, type2);
	type_compiled_heap(args, values, ret);
}

static void defun_fpclassify(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_FPCLASSIFY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_fpclassify);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_fpclassify(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-paper (array body &key fill type) ...)
 *   array  (or null (integer 0 *))
 *   body   (or null (integer 0 *))
 *   fill   (or boolean (integer 0 #xFF))  ;; default t
 *   type   (integer 0 #xFF)
 *   paper  paper
 */
static int syscall_make_paper(Execute ptr, addr array, addr body, addr rest)
{
	Return(make_paper_syscode_(array, body, rest, &array));
	setresult_control(ptr, array);
	return 0;
}

static void type_syscall_make_paper(addr *ret)
{
	addr args, values, type1, type2, type3;
	addr key, key1, key2;

	/* array, body */
	GetTypeTable(&args, IntplusNull);
	/* :fill */
	GetTypeTable(&type1, Boolean);
	GetTypeTable(&type2, Unsigned8);
	type2or_heap(type1, type2, &type3);
	GetConst(KEYWORD_FILL, &key1);
	cons_heap(&key1, key1, type3);
	/* :type */
	KeyTypeTable(&key2, TYPE, Unsigned8);
	/* &key */
	list_heap(&key, key1, key2, NULL);
	/* function */
	typeargs_var2key(&args, args, args, key);
	GetTypeValues(&values, Paper);
	type_compiled_heap(args, values, ret);
}

static void defun_make_paper(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_PAPER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_make_paper);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_make_paper(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun info-paper (paper symbol &optional second) ...) -> t
 *   paper   paper
 *   symbol  (member list vector type length)
 *   second  t
 */
static int syscall_info_paper(Execute ptr, addr pos, addr symbol, addr body_p)
{
	Return(info_paper_syscode_(pos, symbol, body_p, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_syscall_info_paper(addr *ret)
{
	addr args, values, type;
	addr m, m1, m2, m3, m4;

	GetTypeTable(&args, Paper);
	GetConst(COMMON_LIST, &m1);
	GetConst(COMMON_VECTOR, &m2);
	GetConst(COMMON_TYPE, &m3);
	GetConst(COMMON_LENGTH, &m4);
	type_member_heap(&m, m1, m2, m3, m4, NULL);
	GetTypeTable(&type, T);
	typeargs_var2opt1(&args, args, m, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_info_paper(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_INFO_PAPER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_syscall_info_paper);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_info_paper(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-paper (paper index &optional value) -> t
 *   paper  paper
 *   index  (integer 0 *)
 *   value  t
 */
static int syscall_array_paper(Execute ptr, addr pos, addr index, addr value)
{
	Return(array_paper_syscode_(pos, index, value, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_syscall_array_paper(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Paper);
	GetTypeTable(&values, Intplus);
	GetTypeTable(&type, T);
	typeargs_var2opt1(&args, args, values, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_array_paper(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ARRAY_PAPER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_syscall_array_paper);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_array_paper(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun body-paper (paper index &optional value) -> (unsigned-byte 8)
 *   paper  paper
 *   index  (integer 0 *)
 *   value  (unsigned-byte 8)
 */
static int syscall_body_paper(Execute ptr, addr pos, addr index, addr value)
{
	Return(body_paper_syscode_(pos, index, value, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_syscall_body_paper(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Paper);
	GetTypeTable(&values, Intplus);
	GetTypeTable(&type, Unsigned8);
	typeargs_var2opt1(&args, args, values, type);
	typevalues_result(&values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_body_paper(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BODY_PAPER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_syscall_body_paper);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_body_paper(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun dlfile (type &rest args) -> *
 *   type  symbol
 */
static int syscall_dlfile(Execute ptr, addr pos, addr args)
{
	Return(dlfile_syscode_(ptr, pos, args, &pos, &args));
	if (args != Unbound)
		setvalues_control(ptr, pos, args, NULL);
	else
		setresult_control(ptr, pos);
	return 0;
}

static void type_syscall_dlfile(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1rest(&args, args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_dlfile(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DLFILE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_dlfile);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_dlfile(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun dlcall (paper &rest args) -> (values &rest t)
 *   paper  paper
 */
static int syscall_dlcall(Execute ptr, addr paper, addr args)
{
	return dlcall_syscode_(ptr, paper, args);
}

static void type_syscall_dlcall(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Paper);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	typevalues_rest(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_dlcall(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DLCALL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_dlcall);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_dlcall(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_syscall_function(void)
{
	SetPointerSysCall(defun, empty, abort_lisp);
	SetPointerSysCall(defun, empty, hello);
	SetPointerSysCall(defun, dynamic, infobit);
	SetPointerSysCall(defun, dynamic, infoprint);
	SetPointerSysCall(defun, dynamic, gc);
	SetPointerSysCall(defun, var1dynamic, savecore);
	SetPointerSysCall(defun, var1dynamic, loadcore);
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
	SetPointerSysCall(defun, var1dynamic, sysctl);
	SetPointerSysCall(defun, var1dynamic, terme);
	SetPointerSysCall(defun, var1, fpclassify);
	SetPointerSysCall(defun, var2dynamic, make_paper);
	SetPointerSysCall(defun, var2opt1, info_paper);
	SetPointerSysCall(defun, var2opt1, array_paper);
	SetPointerSysCall(defun, var2opt1, body_paper);
	SetPointerSysCall(defun, var1dynamic, dlfile);
	SetPointerSysCall(defun, var1dynamic, dlcall);
}

void build_syscall_function(void)
{
	defun_abort_lisp();
	defun_hello();
	defun_infobit();
	defun_infoprint();
	defun_gc();
	defun_savecore();
	defun_loadcore();
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
	defun_sysctl();
	defun_terme();
	defun_fpclassify();
	defun_make_paper();
	defun_info_paper();
	defun_array_paper();
	defun_body_paper();
	defun_dlfile();
	defun_dlcall();
}


/************************************************************
 *  syscode_common.c
 ************************************************************/

/* defconstant */
int defconstant_syscode_(addr symbol, addr value, addr doc)
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
int defpackage_syscode_(Execute ptr, addr var, addr rest, addr *ret)
{
	return defpackage_execute_(ptr, var, rest, ret);
}


/* do-symbols */
int do_symbols_syscode_(Execute ptr, addr call, addr package)
{
	return do_symbols_package_(ptr, call, package);
}


/* do-external-symbols */
int do_external_symbols_syscode_(Execute ptr, addr call, addr package)
{
	return do_external_symbols_package_(ptr, call, package);
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
int prompt_for_syscode_(Execute ptr, addr type, addr args, addr *ret)
{
	addr format;
	LocalHold hold;

	if (args == Nil) {
		strvect_char_heap(&format, "Input> ");
	}
	else {
		Return_getcons(args, &format, &args);
		Return(format_string_lisp_(ptr, format, args, &format));
	}

	hold = LocalHold_local_push(ptr, format);
	Return(prompt_for_stream_(ptr, type, format, &format));
	localhold_end(hold);
	*ret = format;

	return 0;
}


/* print-unreadable-call */
int print_unreadable_call_syscode_(Execute ptr,
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

	Return(output_stream_designator_(ptr, stream, &stream));
	hold = LocalHold_local_push(ptr, stream);
	Return(write_default_print_(ptr, stream, var));
	localhold_end(hold);

	return Result(ret, var);
}

int write_default_syscode_(Execute ptr, addr stream, addr var, addr *ret)
{
	addr control;

	Return(output_stream_designator_(ptr, stream, &stream));
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
int structure_constructor_syscode_(Execute ptr, addr symbol, addr rest, addr *ret)
{
	return structure_constructor_common_(ptr, symbol, rest, ret);
}


/* loop-bind */
int loop_bind_syscode_(Execute ptr, addr a, addr b, addr c, addr *ret)
{
	return loop_bind_common_(ptr, a, b, c, ret);
}


/* make-pprint-stream */
int make_pprint_stream_syscode_(Execute ptr, addr *ret,
		addr stream, addr object, addr prefix, addr perline, addr suffix)
{
	return open_pretty_stream_(ptr, ret, stream, object, prefix, perline, suffix);
}


/* pprint-gensym */
int pprint_gensym_syscode_(addr stream, addr *ret)
{
	Check(! pretty_stream_p(stream), "type error");
	return gensym_pretty_stream_(stream, ret);
}


/* pprint-exit */
int pprint_exit_syscode_(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	return pprint_exit_common_(ptr, stream);
}


/* pprint-pop */
int pprint_pop_syscode_(Execute ptr, addr stream, addr *ret)
{
	Check(! pretty_stream_p(stream), "type error");
	return pprint_pop_common_(ptr, stream, ret);
}


/* pprint-check */
int pprint_check_syscode_(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	return check_pretty_stream_(ptr, stream);
}


/* pprint-close */
int pprint_close_syscode_(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	return close_pretty_stream_(ptr, stream);
}


/* pprint-pretty */
int pprint_pretty_syscode_(Execute ptr, addr stream, addr call)
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
int set_slots_syscode_(addr var, addr slots, addr values)
{
	return set_slots_syscall_(var, slots, values);
}


/* intern-eql-specializer */
int intern_eql_specializer_syscode_(addr var, addr *ret)
{
	return clos_intern_specializer_(var, ret);
}


/* defgeneric */
int defgeneric_define_syscode_(Execute ptr, addr name, addr args, addr *ret)
{
	return system_generic_define_(ptr, name, args, ret);
}

int defgeneric_method_syscode_(addr inst, addr args)
{
	return system_generic_method_(inst, args);
}


/* condition-restarts-push */
static int condition_restarts_check_restarts_(addr list)
{
	addr pos;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! restartp(pos))
			return fmte_("The object ~S must be a restart type.", pos, NULL);
	}

	return 0;
}

int condition_restarts_push_syscode_(addr condition, addr restarts)
{
	addr pos, list;

	Return(condition_restarts_check_restarts_(restarts));
	while (restarts != Nil) {
		GetCons(restarts, &pos, &restarts);
		getassociated_restart(pos, &list);
		cons_heap(&list, condition, list);
		setassociated_restart(pos, list);
	}

	return 0;
}


/* condition-restarts-pop */
int condition_restarts_pop_syscode_(addr condition, addr restarts)
{
	addr pos, list, check;

	Return(condition_restarts_check_restarts_(restarts));
	while (restarts != Nil) {
		GetCons(restarts, &pos, &restarts);
		getassociated_restart(pos, &list);
		if (consp_getcons(list, &check, &list)) {
			if (check == condition)
				setassociated_restart(pos, list);
		}
	}

	return 0;
}


/* condition-restarts-make */
static int condition_restarts_make_name_(addr var, addr *ret)
{
	addr check;

	/* signal -> simple-condition */
	GetConst(COMMON_SIGNAL, &check);
	if (var == check) {
		GetConst(COMMON_SIMPLE_CONDITION, ret);
		return 0;
	}

	/* error -> simple-error */
	GetConst(COMMON_ERROR, &check);
	if (var == check) {
		GetConst(COMMON_SIMPLE_ERROR, ret);
		return 0;
	}

	/* cerror -> simple-error */
	GetConst(COMMON_CERROR, &check);
	if (var == check) {
		GetConst(COMMON_SIMPLE_ERROR, ret);
		return 0;
	}

	/* warn -> simple-warning */
	GetConst(COMMON_WARN, &check);
	if (var == check) {
		GetConst(COMMON_SIMPLE_WARNING, ret);
		return 0;
	}

	/* error */
	*ret = Nil;
	return fmte_("Invalid argument, ~S.", var, NULL);
}

static int condition_restarts_make_string_(Execute ptr,
		addr var, addr car, addr cdr, addr *ret)
{
	addr key1, key2, make, list;

	Return(condition_restarts_make_name_(var, &var));
	GetConst(KEYWORD_FORMAT_CONTROL, &key1);
	GetConst(KEYWORD_FORMAT_ARGUMENTS, &key2);
	list_heap(&list, var, key1, car, key2, cdr, NULL);

	/* `(make-condition 'simple-condition
	 *    :format-control ',car
	 *    :format-arguments ',cdr)
	 */
	GetConst(COMMON_MAKE_CONDITION, &make);
	Return(getfunction_global_(make, &make));
	return apply1_control_(ptr, ret, make, list);
}

int condition_restarts_make_syscode_(Execute ptr, addr var, addr list, addr *ret)
{
	addr car, cdr, make;

	Return_getcons(list, &car, &cdr);

	/* string */
	if (stringp(car))
		return condition_restarts_make_string_(ptr, var, car, cdr, ret);

	/* instance */
	if (closp(car)) {
		if (cdr != Nil)
			return fmte_("Invalid condition form, ~S.", list, NULL);
		return Result(ret, car);
	}

	/* `(make-condition ,list) */
	GetConst(COMMON_MAKE_CONDITION, &make);
	Return(getfunction_global_(make, &make));
	return apply1_control_(ptr, ret, make, list);
}


/* make-restart */
int make_restart_syscode_(addr var, addr call, addr rest, addr *ret)
{
	addr inter, report, test, escape, pos;

	Check(! symbolp(var), "type error");
	Check(! functionp(call), "type error");
	if (GetKeyArgs(rest, KEYWORD_INTERACTIVE_FUNCTION, &inter))
		inter = Nil;
	if (GetKeyArgs(rest, KEYWORD_REPORT_FUNCTION, &report))
		report = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST_FUNCTION, &test))
		test = Nil;
	if (GetKeyArgs(rest, KEYWORD_ESCAPE, &escape))
		escape = Nil;

	/* restart */
	restart_heap(&pos, var);
	setfunction_restart(pos, call);
	setinteractive_restart(pos, inter);
	setreport_restart(pos, report);
	settest_restart(pos, test);
	setescape_restart(pos, escape != Nil);

	return Result(ret, pos);
}


/************************************************************
 *  syscode_function.c
 ************************************************************/

/* hello */
int hello_syscode_(Execute ptr)
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
int savecore_syscode_(Execute ptr, addr output, addr rest)
{
	addr input, exitp;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	if (GetKeyArgs(rest, KEYWORD_EXIT, &exitp))
		exitp = T;
	if (GetKeyArgs(rest, KEYWORD_INPUT, &input))
		input = Nil;
	localhold_set(hold, 0, output);
	localhold_set(hold, 1, input);

	if (output != Nil) {
		Return(pathname_designator_heap_(ptr, output, &output));
		localhold_set(hold, 0, output);
	}
	if (input != Nil && input != T) {
		Return(pathname_designator_heap_(ptr, input, &input));
		localhold_set(hold, 1, input);
	}
	Return(savecore_execute_(ptr, output, input, (exitp != Nil)));
	localhold_end(hold);

	return 0;
}


/* loadcore */
int loadcore_syscode_(Execute ptr, addr input, addr rest)
{
	addr output, exitp;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	if (GetKeyArgs(rest, KEYWORD_EXIT, &exitp))
		exitp = T;
	if (GetKeyArgs(rest, KEYWORD_OUTPUT, &output))
		output = Nil;
	localhold_set(hold, 0, output);
	localhold_set(hold, 1, input);

	if (output != Nil) {
		Return(pathname_designator_heap_(ptr, output, &output));
		localhold_set(hold, 0, output);
	}
	if (input != Nil && input != T) {
		Return(pathname_designator_heap_(ptr, input, &input));
		localhold_set(hold, 1, input);
	}
	Return(savecore_execute_(ptr, output, input, (exitp != Nil)));
	localhold_end(hold);

	return 0;
}


/* package-export-list */
int package_export_list_syscode_(addr var, addr *ret)
{
	Return(package_designator_(var, &var));
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
int simple_sort_syscode_(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	return simple_sort_sequence_(ptr, pos, call, key);
}


/* bubble-sort */
int bubble_sort_syscode_(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	return bubble_sort_sequence_(ptr, pos, call, key);
}


/* quick-sort */
int quick_sort_syscode_(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	return quick_sort_sequence_(ptr, pos, call, key);
}


/* merge-sort */
int merge_sort_syscode_(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
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
	*ret = short_float_p(var)? T: Nil;
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
int make_character_syscode_(addr var, addr *ret)
{
	unicode c;

	/* integer */
	if (integerp(var)) {
		Return(getunicode_integer_(var, &c));
		if (isBaseType(c)) {
			make_character_heap(ret, c);
			return 0;
		}
		if (isExtendedType(c)) {
			return make_extended_char_heap_(ret, c);
		}
		goto error;
	}

	/* character */
	if (characterp(var)) {
		GetCharacter(var, &c);
		make_character_heap(ret, c);
		return 0;
	}

	/* type-error*/
error:
	*ret = Nil;
	return TypeError_(var, CHARACTER);
}


/* make-fixnum */
int make_fixnum_syscode_(addr var, addr *ret)
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
int make_bignum_syscode_(addr var, addr *ret)
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

int make_ratio_syscode_(addr numer, addr denom, addr *ret)
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
	Return(parse_type_(ptr, &x, x, Nil));
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
int remove_file_syscode_(Execute ptr, addr var, addr opt, addr *ret)
{
	int check;
	Return(remove_file_common_(ptr, var, (opt != Nil), &check));
	return Result(ret, check? T: Nil);
}


/* remove-directory */
int remove_directory_syscode_(Execute ptr, addr var, addr opt, addr *ret)
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

int declare_parse_syscode_(addr form, addr *ret)
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
int parse_type_syscode_(Execute ptr, addr var, addr *ret)
{
	Return(parse_type_(ptr, &var, var, Nil));
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


/* sysctl */
int sysctl_syscode_(Execute ptr, addr var, addr args)
{
	return sysctl_values_(ptr, var, args);
}


/* terme */
int terme_syscode_(Execute ptr, addr var, addr args)
{
	return terme_values_(ptr, var, args);
}


/* fpclassify */
void fpclassify_syscode(addr var, addr *rtype, addr *rsign)
{
	fpclassify_float(var, rtype, rsign);
}


/* make-paper */
static int make_paper_index_syscode_(addr pos, size_t *ret)
{
	if (pos == Nil)
		return Result(ret, 0);
	else
		return getindex_integer_(pos, ret);
}

static int make_paper_fill_syscode_(addr rest, int *fill, byte *c)
{
	if (GetKeyArgs(rest, KEYWORD_FILL, &rest)) {
		*fill = 0;
		*c = 0;
		return 0;
	}

	if (rest == Nil) {
		*fill = 0;
		*c = 0;
		return 0;
	}

	if (rest == T) {
		*fill = 1;
		*c = 0;
		return 0;
	}

	/* (unsigned-byte 8) */
	*fill = 1;
	if (GetByte_integer(rest, c))
		return fmte_("Invalid :fill value, ~S.", rest, NULL);

	return 0;
}

static int make_paper_type_syscode_(addr rest, byte *ret)
{
	if (GetKeyArgs(rest, KEYWORD_TYPE, &rest)) {
		*ret = 0;
		return 0;
	}

	/* (unsigned-byte 8) */
	if (GetByte_integer(rest, ret))
		return fmte_("Invalid :type value, ~S.", rest, NULL);

	return 0;
}

int make_paper_syscode_(addr array, addr body, addr rest, addr *ret)
{
	byte c, user;
	int fill;
	addr pos, mem;
	size_t x, y;

	Return(make_paper_index_syscode_(array, &x));
	Return(make_paper_index_syscode_(body, &y));
	Return(make_paper_fill_syscode_(rest, &fill, &c));
	Return(make_paper_type_syscode_(rest, &user));
	Return(paper_arraybody_heap_(&pos, x, y));
	if (fill && y) {
		posbody(pos, &mem);
		memset((void *)mem, (int)c, y);
	}
	paper_set_type(pos, user);

	return Result(ret, pos);
}


/* info-paper */
int info_paper_syscode_(addr pos, addr symbol, addr second, addr *ret)
{
	int body_p;
	addr value;

	if (second == Unbound)
		second = Nil;  /* default */
	body_p = (second != Nil);

	/* length */
	GetConst(COMMON_LENGTH, &value);
	if (symbol == value) {
		if (body_p)
			return paper_length_body_(pos, ret);
		else
			return paper_length_array_(pos, ret);
	}

	/* list */
	GetConst(COMMON_LIST, &value);
	if (symbol == value) {
		if (body_p)
			return paper_list_body_(pos, ret);
		else
			return paper_list_array_(pos, ret);
	}

	/* vector */
	GetConst(COMMON_VECTOR, &value);
	if (symbol == value) {
		if (body_p)
			return paper_vector_body_(pos, ret);
		else
			return paper_vector_array_(pos, ret);
	}

	/* type */
	GetConst(COMMON_TYPE, &value);
	if (symbol == value) {
		if (second == Nil)
			return paper_get_type_(pos, ret);
		else {
			*ret = second;
			return paper_set_type_(pos, second);
		}
	}

	/* error */
	return Result(ret, Nil);
}


/* array-paper */
int array_paper_syscode_(addr pos, addr index, addr value, addr *ret)
{
	if (value == Unbound) {
		return paper_get_array_(pos, index, ret);
	}
	else {
		*ret = value;
		return paper_set_array_(pos, index, value);
	}
}


/* body-paper */
int body_paper_syscode_(addr pos, addr index, addr value, addr *ret)
{
	if (value == Unbound) {
		return paper_get_body_(pos, index, ret);
	}
	else {
		*ret = value;
		return paper_set_body_(pos, index, value);
	}
}

/* dlfile */
int dlfile_syscode_(Execute ptr, addr type, addr args, addr *ret, addr *retp)
{
	return dlfile_process_(ptr, type, args, ret, retp);
}

/* dlcall */
int dlcall_syscode_(Execute ptr, addr paper, addr args)
{
	return dlcall_process_(ptr, paper, args);
}


/************************************************************
 *  sysctl.c
 ************************************************************/

/*
 *  recovery
 */
static int sysctl_recovery_no_applicable_method_(Execute ptr)
{
	addr pos;

	/* fmakunbound */
	GetConst(COMMON_NO_APPLICABLE_METHOD, &pos);
	SetFunctionSymbol(pos, Unbound);

	/* recovery */
	Return(defgeneric_no_applicable_method_mop_(ptr));
	setvalues_control(ptr, T, T, NULL);

	return 0;
}

static int sysctl_recovery_no_next_method_(Execute ptr)
{
	addr pos;

	/* fmakunbound */
	GetConst(COMMON_NO_NEXT_METHOD, &pos);
	SetFunctionSymbol(pos, Unbound);

	/* recovery */
	Return(defgeneric_no_next_method_mop_(ptr));
	setvalues_control(ptr, T, T, NULL);

	return 0;
}

static int sysctl_recovery_(Execute ptr, addr args)
{
	int check;
	addr pos;

	if (! consp_getcons(args, &pos, &args))
		goto error;

	/* no-applicable-method */
	Return(string_designator_equalp_char_(pos, "no-applicable-method", &check));
	if (check)
		return sysctl_recovery_no_applicable_method_(ptr);

	/* no-next-method */
	Return(string_designator_equalp_char_(pos, "no-next-method", &check));
	if (check)
		return sysctl_recovery_no_next_method_(ptr);

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}


/*
 *  clos
 */
static int sysctl_clos_slots_(Execute ptr, addr pos)
{
	clos_getslots_heap(pos, &pos);
	setvalues_control(ptr, pos, T, NULL);
	return 0;
}

static int sysctl_clos_object_(Execute ptr, addr pos, addr args)
{
	int check;
	addr car;

	if (! consp_getcons(args, &car, &args))
		goto error;

	/* slots */
	Return(string_designator_equalp_char_(car, "slots", &check));
	if (check)
		return sysctl_clos_slots_(ptr, pos);

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}

static int sysctl_clos_(Execute ptr, addr args)
{
	addr pos;

	if (! consp_getcons(args, &pos, &args))
		goto error;
	if (! closp(pos))
		goto error;
	return sysctl_clos_object_(ptr, pos, args);

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}


/*
 *  memory-stream
 */
static int sysctl_memory_stream_size_(Execute ptr, addr pos)
{
	size_t size;

	getsize_memory_stream(pos, &size);
	make_index_integer_heap(&pos, size);
	setvalues_control(ptr, pos, T, NULL);

	return 0;
}

static int sysctl_memory_stream_array_(Execute ptr, addr pos)
{
	size_t size;

	getarray_memory_stream(pos, &size);
	make_index_integer_heap(&pos, size);
	setvalues_control(ptr, pos, T, NULL);

	return 0;
}

static int sysctl_memory_stream_cache_(Execute ptr, addr pos)
{
	int cache;

	cache = getcache_memory_stream(pos);
	setvalues_control(ptr, cache? T: Nil, T, NULL);

	return 0;
}

static int sysctl_memory_stream_object_(Execute ptr, addr pos, addr args)
{
	int check;
	addr car;

	if (! consp_getcons(args, &car, &args))
		goto error;

	/* size */
	Return(string_designator_equalp_char_(car, "size", &check));
	if (check)
		return sysctl_memory_stream_size_(ptr, pos);

	/* array */
	Return(string_designator_equalp_char_(car, "array", &check));
	if (check)
		return sysctl_memory_stream_array_(ptr, pos);

	/* cache */
	Return(string_designator_equalp_char_(car, "cache", &check));
	if (check)
		return sysctl_memory_stream_cache_(ptr, pos);

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}

static int sysctl_memory_stream_(Execute ptr, addr args)
{
	addr pos;

	if (! consp_getcons(args, &pos, &args))
		goto error;
	if (! memory_stream_p(pos))
		goto error;
	return sysctl_memory_stream_object_(ptr, pos, args);

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}


/*
 *  structure
 */
static int sysctl_structure_check_(Execute ptr, addr pos)
{
	int check;

	check = structure_get_object(pos, &pos);
	setvalues_control(ptr, check? T: Nil, T, NULL);
	return 0;
}

static int sysctl_structure_delete_(Execute ptr, addr pos)
{
	int check;

	Return(structure_delete_(ptr, pos, &check));
	pos = check? T: Nil;
	setvalues_control(ptr, pos, T, NULL);

	return 0;
}

static int sysctl_structure_type_(Execute ptr, addr pos)
{
	Return(structure_get_type_(pos, &pos));
	setvalues_control(ptr, pos, T, NULL);

	return 0;
}

static int sysctl_structure_(Execute ptr, addr args)
{
	int check;
	addr name, pos;

	if (! consp_getcons(args, &pos, &args))
		goto error;
	if (! consp_getcons(args, &name, &args))
		goto error;

	/* check */
	Return(string_designator_equalp_char_(pos, "check", &check));
	if (check)
		return sysctl_structure_check_(ptr, name);

	/* delete */
	Return(string_designator_equalp_char_(pos, "delete", &check));
	if (check)
		return sysctl_structure_delete_(ptr, name);

	/* type */
	Return(string_designator_equalp_char_(pos, "type", &check));
	if (check)
		return sysctl_structure_type_(ptr, name);

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}


/*
 *  random-state
 */
static int sysctl_random_state_integer_(Execute ptr, addr args)
{
	addr value;

	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args != Nil)
		goto error;
	Return(random_state_integer_(value, &value));
	setvalues_control(ptr, value, T, NULL);
	return 0;

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}

static int sysctl_random_state_make_(Execute ptr, addr args)
{
	addr value;

	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args != Nil)
		goto error;
	Return(random_state_make_(ptr->local, value, &value));
	setvalues_control(ptr, value, T, NULL);
	return 0;

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}

static int sysctl_random_state_write_(Execute ptr, addr args)
{
	addr pos, value;

	if (! consp_getcons(args, &pos, &args))
		goto error;
	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args != Nil)
		goto error;
	Return(random_state_write_(ptr->local, pos, value));
	setvalues_control(ptr, pos, T, NULL);
	return 0;

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}

static int sysctl_random_state_(Execute ptr, addr args)
{
	int check;
	addr pos;

	if (! consp_getcons(args, &pos, &args))
		goto error;

	/* integer */
	Return(string_designator_equalp_char_(pos, "integer", &check));
	if (check)
		return sysctl_random_state_integer_(ptr, args);

	/* make */
	Return(string_designator_equalp_char_(pos, "make", &check));
	if (check)
		return sysctl_random_state_make_(ptr, args);

	/* write */
	Return(string_designator_equalp_char_(pos, "write", &check));
	if (check)
		return sysctl_random_state_write_(ptr, args);

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}


/*
 *  stream
 */
static int sysctl_stream_pipe_make_(Execute ptr, addr args)
{
	fixnum value;
	addr pos;

	if (! consp_getcons(args, &pos, &args))
		goto error;
	if (args != Nil)
		goto error;
	if (! fixnump(pos))
		goto error;
	GetFixnum(pos, &value);
	if (StreamPipe_Size <= value)
		goto error;
	open_pipe_stream(&pos, (enum StreamPipe)value);
	setvalues_control(ptr, pos, T, NULL);
	return 0;

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}

static int sysctl_stream_pipe_type_(Execute ptr, addr args)
{
	enum StreamPipe type;
	addr stream, pos;
	fixnum value;

	/* (sysctl 'stream 'pipe 'type x 10) */
	if (! consp_getcons(args, &stream, &args))
		goto error;
	if (! pipe_stream_p(stream))
		goto error;
	if (consp_getcons(args, &pos, &args)) {
		/* set */
		if (! fixnump(pos))
			goto error;
		GetFixnum(pos, &value);
		type = (enum StreamPipe)value;
		if (StreamPipe_Size <= type)
			goto error;
		set_type_pipe_stream(stream, type);
		setvalues_control(ptr, pos, T, NULL);
	}
	else {
		/* get */
		value = (fixnum)get_type_pipe_stream(stream);
		fixnum_heap(&pos, value);
		setvalues_control(ptr, pos, T, NULL);
	}
	return 0;

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}

static int sysctl_stream_pipe_(Execute ptr, addr args)
{
	int check;
	addr pos;

	if (! consp_getcons(args, &pos, &args))
		goto error;

	/* make */
	Return(string_designator_equalp_char_(pos, "make", &check));
	if (check)
		return sysctl_stream_pipe_make_(ptr, args);

	/* type */
	Return(string_designator_equalp_char_(pos, "type", &check));
	if (check)
		return sysctl_stream_pipe_type_(ptr, args);

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}

static int sysctl_stream_(Execute ptr, addr args)
{
	int check;
	addr pos;

	if (! consp_getcons(args, &pos, &args))
		goto error;

	/* pipe */
	Return(string_designator_equalp_char_(pos, "pipe", &check));
	if (check)
		return sysctl_stream_pipe_(ptr, args);

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}


/*
 *  package
 */
static int sysctl_package_readonly_(Execute ptr, addr args)
{
	int check;
	addr pos, value;

	/* package */
	if (! consp_getcons(args, &pos, &args))
		goto error;
	Return(package_designator_(pos, &pos));

	/* get */
	if (args == Nil) {
		value = get_readonly_package(pos)? T: Nil;
		setvalues_control(ptr, value, T, NULL);
		return 0;
	}

	/* set */
	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args != Nil)
		goto error;
	check = (value != Nil);
	set_readonly_package(pos, check);
	setvalues_control(ptr, value, T, NULL);
	return 0;

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}

static int sysctl_package_(Execute ptr, addr args)
{
	int check;
	addr pos;

	if (! consp_getcons(args, &pos, &args))
		goto error;

	/* readonly */
	Return(string_designator_equalp_char_(pos, "readonly", &check));
	if (check)
		return sysctl_package_readonly_(ptr, args);

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}


/*
 *  require
 */
static int sysctl_require_append_(Execute ptr, addr args)
{
	int check, forcep;
	addr pos, opt;

	/* require */
	if (! consp_getcons(args, &pos, &args))
		goto error;
	if (args == Nil) {
		forcep = 0;
	}
	else {
		if (! consp_getcons(args, &opt, &args))
			goto error;
		if (args != Nil)
			goto error;
		forcep = (opt != Nil);
	}

	/* get */
	Return(require_append_(ptr, pos, forcep, &check));
	setvalues_control(ptr, (check? T: Nil), T, NULL);
	return 0;

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}

static int sysctl_require_delete_(Execute ptr, addr args)
{
	int check, forcep;
	addr pos, opt;

	/* require */
	if (! consp_getcons(args, &pos, &args))
		goto error;
	if (args == Nil) {
		forcep = 0;
	}
	else {
		if (! consp_getcons(args, &opt, &args))
			goto error;
		if (args != Nil)
			goto error;
		forcep = (opt != Nil);
	}

	/* get */
	Return(require_delete_(ptr, pos, forcep, &check));
	setvalues_control(ptr, (check? T: Nil), T, NULL);
	return 0;

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}

static int sysctl_require_(Execute ptr, addr args)
{
	int check;
	addr pos;

	if (! consp_getcons(args, &pos, &args))
		goto error;

	/* append */
	Return(string_designator_equalp_char_(pos, "append", &check));
	if (check)
		return sysctl_require_append_(ptr, args);

	/* delete */
	Return(string_designator_equalp_char_(pos, "delete", &check));
	if (check)
		return sysctl_require_delete_(ptr, args);

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}


/*
 *  sysctl
 */
int sysctl_values_(Execute ptr, addr pos, addr args)
{
	int check;

	/* memory-stream object */
	if (memory_stream_p(pos))
		return sysctl_memory_stream_object_(ptr, pos, args);

	/* clos object */
	if (closp(pos))
		return sysctl_clos_object_(ptr, pos, args);

	/* memory-stream */
	Return(string_designator_equalp_char_(pos, "memory-stream", &check));
	if (check)
		return sysctl_memory_stream_(ptr, args);

	/* clos */
	Return(string_designator_equalp_char_(pos, "clos", &check));
	if (check)
		return sysctl_clos_(ptr, args);

	/* recovery */
	Return(string_designator_equalp_char_(pos, "recovery", &check));
	if (check)
		return sysctl_recovery_(ptr, args);

	/* structure */
	Return(string_designator_equalp_char_(pos, "structure", &check));
	if (check)
		return sysctl_structure_(ptr, args);

	/* random-state */
	Return(string_designator_equalp_char_(pos, "random-state", &check));
	if (check)
		return sysctl_random_state_(ptr, args);

	/* stream */
	Return(string_designator_equalp_char_(pos, "stream", &check));
	if (check)
		return sysctl_stream_(ptr, args);

	/* package */
	Return(string_designator_equalp_char_(pos, "package", &check));
	if (check)
		return sysctl_package_(ptr, args);

	/* require */
	Return(string_designator_equalp_char_(pos, "require", &check));
	if (check)
		return sysctl_require_(ptr, args);

	/* error */
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}


/************************************************************
 *  terme.c
 ************************************************************/

/*
 *  interface
 */
void init_terme(void)
{
	if (terme_arch_init()) {
		Abort("terme_arch_init error.");
	}
	terme_input_init();
	terme_output_init();
}

static void build_terme_object(void)
{
	addr symbol, root, value;

	/* root */
	terme_root_build(&root);

	/* data */
	terme_data_build(&value);
	terme_set(root, terme_root_data, value);

	/* screen */
	terme_screen_build(&value);
	terme_set(root, terme_root_screen, value);

	/* display */
	terme_display_build(&value);
	terme_set(root, terme_root_display, value);

	/* history */
	terme_history_build(&value);
	terme_set(root, terme_root_history, value);

	/* special */
	GetConst(SYSTEM_SPECIAL_TERME, &symbol);
	SetValueSymbol(symbol, root);
}

void build_terme(void)
{
	build_terme_object();
	terme_arch_build();
}

int begin_terme(void)
{
	return terme_arch_begin();
}

int end_terme(void)
{
	return terme_arch_end();
}

int prompt_terme_(Execute ptr, addr pos, PromptMode mode)
{
	return terme_prompt_set_(ptr, pos, mode);
}

int readline_terme_(Execute ptr, addr *ret)
{
	return terme_readline_(ptr, ret);
}

int clear_terme_(Execute ptr)
{
	if (terme_input_clear())
		return fmte_("terme_input_clear error", NULL);
	return 0;
}

int text_color_terme(Execute ptr, PrintColor value)
{
	return text_color_arch_terme(ptr, value);
}


/************************************************************
 *  terme_arch.c
 ************************************************************/

static unsigned terme_arch_x;
static unsigned terme_arch_y;
static int terme_arch_textmode_p;
static int terme_arch_enable_p;

#if defined(LISP_TERME_UNIX)
#include <sys/ioctl.h>
#include <sys/select.h>
#include <sys/types.h>
#include <sys/time.h>
#include <termios.h>
#include <unistd.h>

#define TERME_UNIX_ESCAPE		9990

/*
 *  terme-init
 */
static int terme_unix_escape_p;
static int terme_unix_signal_value;
static struct timeval terme_unix_escape_timeval;
static struct termios terme_unix_default_v;

static int terme_unix_get(struct termios *ret)
{
	return tcgetattr(STDIN_FILENO, ret);
}

static int terme_unix_set(struct termios *ret)
{
	return tcsetattr(STDIN_FILENO, TCSAFLUSH, ret);
}

static int terme_unix_init(void)
{
	if (terme_unix_get(&terme_unix_default_v)) {
		Debug("terme_unix_get error.");
		return 1;
	}
	terme_unix_escape_p = 0;
	terme_unix_signal_value = 0;
	cleartype(terme_unix_escape_timeval);

	return 0;
}


/*
 *  terme-signal
 */
static int terme_unix_signal_p_(int *ret)
{
	if (terme_unix_signal_value) {
		terme_unix_signal_value = 0;
		return Result(ret, 1);
	}

	return Result(ret, 0);
}

static void terme_unix_handler(int sig)
{
	terme_unix_signal_value = 1;
}

static int terme_unix_signal(void)
{
	struct sigaction act;

	act.sa_handler = terme_unix_handler;
	sigemptyset(&act.sa_mask);
	act.sa_flags = 0;
	if (sigaction(SIGWINCH, &act, NULL))
		return 1;

	return 0;
}


/*
 *  terme-begin
 */
static int terme_unix_textmode_init;
static struct termios terme_unix_textmode_v;
static struct termios terme_unix_switch_v;

static int terme_unix_termios(void)
{
	struct termios v;

	/* backup */
	terme_unix_textmode_init = 0;
	if (terme_unix_get(&v)) {
		Debug("terme_unix_get error.");
		return 1;
	}
	terme_unix_textmode_init = 1;
	terme_unix_textmode_v = v;

	/* set terminal */
	v.c_iflag &= ~(PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
	v.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
	v.c_oflag &= ~OPOST;
	v.c_cflag &= ~(CSIZE | PARENB);
	v.c_cflag |= CS8;
	v.c_cc[VMIN] = 1;
	v.c_cc[VTIME] = 0;
	terme_unix_switch_v = v;

	return 0;
}

static int terme_unix_begin(void)
{
	/* terminal size */
	if (terme_arch_size_update())
		return 1;

	/* sigaction */
	if (terme_unix_signal())
		return 1;

	/* termios */
	if (terme_unix_termios())
		return 1;

	return 0;
}

static int terme_unix_end(void)
{
	if (terme_unix_textmode_init)
		return terme_unix_set(&terme_unix_textmode_v);
	return 0;
}


/*
 *  terme-switch
 */
static int terme_unix_textmode(void)
{
	struct termios v;

	if (terme_unix_get(&v))
		return 1;
	if (terme_unix_set(&terme_unix_textmode_v))
		return 1;
	terme_unix_switch_v = v;

	return 0;
}

static int terme_unix_rawmode(void)
{
	if (terme_unix_set(&terme_unix_switch_v))
		return 1;
	cleartype(terme_unix_switch_v);

	return 0;
}


/*
 *  input / output
 */
static int terme_unix_select_value(int *ret, long tv_sec, long tv_usec)
{
	int fd, reti;
	fd_set fdset;
	struct timeval tm;

	fd = STDIN_FILENO;
	FD_ZERO(&fdset);
	FD_SET(fd, &fdset);
	tm.tv_sec = tv_sec;
	tm.tv_usec = tv_usec;
	reti = select(fd + 1, &fdset, NULL, NULL, &tm);
	if (reti == 0) {
		/* timeout */
		*ret = 0;
		return 0;
	}
	if (0 < reti) {
		/* can read */
		*ret = 1;
		return 0;
	}
	if (errno == EINTR) {
		/* signal */
		*ret = 0;
		return -1;
	}
	else {
		/* error */
		*ret = 0;
		return 1;
	}
}

static int terme_unix_select(int *ret)
{
	return terme_unix_select_value(ret, 0, 0);
}

static int terme_unix_wait_integer(int *ret, int value)
{
	if (value <= 0)
		value = 0;
	return terme_unix_select_value(ret, (long)value, 0);
}

static int terme_unix_wait_float(int *ret, double value)
{
	long lx, ly;
	double x, y;

	if (value <= 0.0)
		return terme_unix_select_value(ret, 0, 0);

	/* usec = (expt 10 -6) sec */
	x = trunc(value);
	if (((double)LONG_MAX) < x) {
		*ret = 0;
		return 1;
	}
	value -= x;
	value *= 1.0e6;
	y = trunc(value);
	lx = (long)x;
	ly = (long)y;
	return terme_unix_select_value(ret, lx, ly);
}

static int terme_unix_read(void *data, size_t size, size_t *ret)
{
	ssize_t check;

	check = read(STDIN_FILENO, data, size);
	if (0 <= check) {
		*ret = (size_t)check;
		return 0;
	}
	if (errno == EINTR) {
		*ret = 0;
		return -1;
	}
	else {
		*ret = 0;
		return 1;
	}
}

static int terme_unix_write(const void *data, size_t size, size_t *ret)
{
	ssize_t check;

retry:
	check = write(STDOUT_FILENO, data, size);
	if (check < 0) {
		if (errno == EINTR)
			goto retry;
		return -1;
	}
	*ret = (size_t)check;

	return 0;
}


/*
 *  Ctrl + C
 */
static int terme_unix_terminal_sigint_(void)
{
	return kill(getpid(), SIGINT);
}


/*
 *  Ctrl + Z
 */
static int terme_unix_terminal_stop_(void)
{
	return kill(getpid(), SIGTSTP);
}


/*
 *  escape
 */
static int terme_unix_escape_begin(void)
{
	int check;

	if (terme_unix_escape_p)
		return 0;
	check = gettimeofday(&terme_unix_escape_timeval, NULL);
	if (check)
		return 1;
	terme_unix_escape_p = 1;

	return 0;
}
static int terme_unix_escape_end(int *ret)
{
	int check;
	struct timeval *a, *b, now, diff;

	if (terme_unix_escape_p == 0)
		goto error;
	a = &now;
	b = &terme_unix_escape_timeval;
	check = gettimeofday(a, NULL);
	if (check)
		goto error;

	/* minus */
	if (timercmp(a, b, <))
		goto normal;

	/* diff */
	timersub(a, b, &diff);
	if (diff.tv_sec != 0)
		goto normal;
	if (diff.tv_usec < TERME_UNIX_ESCAPE)
		goto normal;

	/* escape */
	terme_unix_escape_p = 0;
	*ret = 1;
	return 0;

normal:
	terme_unix_escape_p = 0;
	*ret = 0;
	return 0;

error:
	terme_unix_escape_p = 0;
	*ret = 0;
	return 1;
}


/*
 *  call begin-default
 */
static int terme_unix_begin_update_(addr *ret, void (*call)(struct termios *))
{
	addr pos;
	size_t size;
	struct termios v;

	/* flush */
	if (terme_finish_output()) {
		*ret = Nil;
		return fmte_("terme_finish_output error.", NULL);
	}

	/* backup */
	if (terme_unix_get(&v)) {
		*ret = Nil;
		return fmte_("terme_unix_get error.", NULL);
	}
	paper_body_heap(&pos, sizeoft(v));
	paper_set_memory(pos, 0, sizeoft(v), (const void *)&v, &size);
	Check(size != sizeoft(v), "size error");

	/* set terminal */
	(*call)(&v);
	if (terme_unix_set(&v)) {
		*ret = Nil;
		return fmte_("terme_unix_set error.", NULL);
	}

	return Result(ret, pos);
}

static void terme_unix_begin_default_call(struct termios *ptr)
{
	*ptr = terme_unix_default_v;
}

static int terme_unix_begin_default_(addr *ret)
{
	return terme_unix_begin_update_(ret, terme_unix_begin_default_call);
}


/*
 *  call begin-rawmode
 */
static void terme_unix_begin_rawmode_call(struct termios *ptr)
{
	ptr->c_iflag &= ~(PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
	ptr->c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
	ptr->c_oflag &= ~OPOST;
	ptr->c_cflag &= ~(CSIZE | PARENB);
	ptr->c_cflag |= CS8;
	ptr->c_cc[VMIN] = 1;
	ptr->c_cc[VTIME] = 0;
}

static int terme_unix_begin_rawmode_(addr *ret)
{
	return terme_unix_begin_update_(ret, terme_unix_begin_rawmode_call);
}


/*
 *  call begin-restore
 */
static int terme_unix_restore_(addr pos)
{
	size_t size;
	struct termios v;

	paper_get_memory(pos, 0, sizeoft(v), (void *)&v, &size);
	Check(size != sizeoft(v), "size error");
	if (terme_unix_set(&v))
		return fmte_("terme_unix_set error.", NULL);

	return 0;
}

#elif defined(LISP_TERME_WINDOWS)
#include "windows_terme.h"
#endif


/*
 *  terme-init
 */
int terme_arch_init(void)
{
	terme_arch_x = 0;
	terme_arch_y = 0;
	terme_arch_textmode_p = 0;
	terme_arch_enable_p = 0;

#if defined(LISP_TERME_UNIX)
	return terme_unix_init();
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_init();
#else
	return 0;
#endif
}


/*
 *  window size
 */
int terme_arch_size_update(void)
{
#if defined(LISP_TERME_UNIX)
	return getwidth_arch(&terme_arch_x, &terme_arch_y);
#elif defined(LISP_TERME_WINDOWS)
	return getwidth_arch(&terme_arch_x, &terme_arch_y);
#else
	terme_arch_x = 0;
	terme_arch_y = 0;
	return 0;
#endif
}

void terme_arch_size_get(unsigned *ret_x, unsigned *ret_y)
{
	if (ret_x)
		*ret_x = terme_arch_x;
	if (ret_y)
		*ret_y = terme_arch_y;
}


/*
 *  terme-signal
 */
int terme_arch_signal_p_(int *ret)
{
	if (! terme_arch_enable_p)
		return Result(ret, 0);
#if defined(LISP_TERME_UNIX)
	return terme_unix_signal_p_(ret);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_signal_p_(ret);
#else
	return Result(ret, 0);
#endif
}


/*
 *  terme-begin
 */
int terme_arch_begin(void)
{
#if defined(LISP_TERME_UNIX)
	terme_arch_textmode_p = 1;
	terme_arch_enable_p = 1;
	return terme_unix_begin();
#elif defined(LISP_TERME_WINDOWS)
	terme_arch_textmode_p = 1;
	terme_arch_enable_p = 1;
	return terme_windows_begin();
#elif defined(LISP_TERME)
	terme_arch_textmode_p = 1;
	terme_arch_enable_p = 0;
	return 0;
#else
	terme_arch_enable_p = 0;
	return 0;
#endif
}


/*
 *  terme-end
 */
int terme_arch_end(void)
{
	if (! terme_arch_enable_p)
		return 0;
	terme_arch_enable_p = 0;

#if defined(LISP_TERME_UNIX)
	return terme_unix_end();
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_end();
#else
	return 0;
#endif
}


/*
 *  terme-switch
 */
int terme_arch_textmode(int *ret)
{
	int check;

	if (ret == NULL)
		ret = &check;
	if (! terme_arch_enable_p) {
		*ret = 0;
		return 0;
	}
	if (terme_arch_textmode_p) {
		*ret = 0;
		return 0;
	}
	terme_arch_textmode_p = 1;
#if defined(LISP_TERME_UNIX)
	if (terme_unix_textmode()) {
		*ret = 0;
		return 0;
	}
	*ret = 1;
#elif defined(LISP_TERME_WINDOWS)
	if (terme_windows_textmode()) {
		*ret = 0;
		return 0;
	}
	*ret = 1;
#else
	*ret = 0;
#endif

	return 0;
}

int terme_arch_rawmode(int *ret)
{
	int check;

	if (ret == NULL)
		ret = &check;
	if (! terme_arch_enable_p) {
		*ret = 0;
		return 0;
	}
	if (! terme_arch_textmode_p) {
		*ret = 0;
		return 0;
	}
	terme_arch_textmode_p = 0;
#if defined(LISP_TERME_UNIX)
	if (terme_unix_rawmode()) {
		*ret = 0;
		return 0;
	}
	*ret = 1;
#elif defined(LISP_TERME_WINDOWS)
	if (terme_windows_rawmode()) {
		*ret = 0;
		return 0;
	}
	*ret = 1;
#else
	*ret = 0;
#endif

	return 0;
}


/*
 *  terme-build
 */
void terme_arch_build(void)
{
#ifdef LISP_TERME_WINDOWS
	terme_windows_build();
#endif
}


/*
 *  wait
 */
int terme_arch_select(int *ret)
{
	if (! terme_arch_enable_p) {
		*ret = 0;
		return 0;
	}
#if defined(LISP_TERME_UNIX)
	return terme_unix_select(ret);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_select(ret);
#else
	*ret = 0;
	return 0;
#endif
}

int terme_arch_wait_integer(int *ret, int value)
{
	if (! terme_arch_enable_p) {
		*ret = 0;
		return 0;
	}
#if defined(LISP_TERME_UNIX)
	return terme_unix_wait_integer(ret, value);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_wait_integer(ret, value);
#else
	*ret = 0;
	return 0;
#endif
}

int terme_arch_wait_float(int *ret, double value)
{
	if (! terme_arch_enable_p) {
		*ret = 0;
		return 0;
	}
#if defined(LISP_TERME_UNIX)
	return terme_unix_wait_float(ret, value);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_wait_float(ret, value);
#else
	*ret = 0;
	return 0;
#endif
}

int terme_arch_read(void *data, size_t size, size_t *ret)
{
	if (! terme_arch_enable_p) {
		*ret = 0;
		return 1;
	}
#if defined(LISP_TERME_UNIX)
	return terme_unix_read(data, size, ret);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_read(data, size, ret);
#else
	*ret = 0;
	return 1;
#endif
}

int terme_arch_write(const void *data, size_t size, size_t *ret)
{
	if (! terme_arch_enable_p) {
		*ret = 0;
		return 1;
	}
#if defined(LISP_TERME_UNIX)
	return terme_unix_write(data, size, ret);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_write(data, size, ret);
#else
	*ret = 0;
	return 1;
#endif
}


/*
 *  Ctrl + C
 */
int terme_arch_terminal_sigint_(void)
{
	if (! terme_arch_enable_p)
		return 0;
#ifdef LISP_TERME_UNIX
	return terme_unix_terminal_sigint_();
#else
	return 0;
#endif
}


/*
 *  Ctrl + Z
 */
int terme_arch_terminal_stop_(void)
{
	if (! terme_arch_enable_p)
		return 0;
#ifdef LISP_TERME_UNIX
	return terme_unix_terminal_stop_();
#else
	return 0;
#endif
}


/*
 *  enable
 */
int terme_arch_enable(void)
{
	return terme_arch_enable_p;
}


/*
 *  escape
 */
int terme_arch_escape_begin(void)
{
#if defined(LISP_TERME_UNIX)
	return terme_unix_escape_begin();
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_escape_begin();
#else
	return 0;
#endif
}

int terme_arch_escape_end(int *ret)
{
#if defined(LISP_TERME_UNIX)
	return terme_unix_escape_end(ret);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_escape_end(ret);
#else
	*ret = 0;
	return 0;
#endif
}


/*
 *  font
 */
int text_color_arch_terme(Execute ptr, PrintColor value)
{
	if (! terme_arch_enable_p)
		return 0;

	return terme_text_color(ptr, value)
		|| terme_finish_output();
}


/*
 *  call begin-default
 */
int terme_arch_begin_default_(addr *ret)
{
#if defined(LISP_TERME_UNIX)
	return terme_unix_begin_default_(ret);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_begin_default_(ret);
#else
	*ret = Nil;
	return fmte_("TERME is not enabled.", NULL);
#endif
}


/*
 *  call begin-rawmode
 */
int terme_arch_begin_rawmode_(addr *ret)
{
#if defined(LISP_TERME_UNIX)
	return terme_unix_begin_rawmode_(ret);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_begin_rawmode_(ret);
#else
	*ret = Nil;
	return fmte_("TERME is not enabled.", NULL);
#endif
}


/*
 *  call begin-restore
 */
int terme_arch_restore_(addr pos)
{
#if defined(LISP_TERME_UNIX)
	return terme_unix_restore_(pos);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_restore_(pos);
#else
	return fmte_("TERME is not enabled.", NULL);
#endif
}


/************************************************************
 *  terme_data.c
 ************************************************************/

#define TERME_DATA_SIZE		4096

/*
 *  string
 */
struct terme_data_character {
	unsigned width : 2;
	unicode c : 21;
};

static void terme_string_build(addr *ret)
{
	addr pos;

	heap_body(&pos, LISPSYSTEM_TERME,
			sizeoft(struct terme_data_character) * TERME_DATA_SIZE);
	terme_set_type(pos, terme_type_string);
	*ret = pos;
}

static struct terme_data_character *struct_data_character(addr pos)
{
	Check(! terme_string_p(pos), "type error");
	return (struct terme_data_character *)terme_pointer(pos);
}


/*
 *  data
 */
enum terme_data_index {
	terme_data_array,
	terme_data_size
};

struct terme_data_struct {
	unsigned alloc, size, now;
};

static struct terme_data_struct *struct_terme_data(addr pos)
{
	Check(! terme_data_p(pos), "type error");
	return (struct terme_data_struct *)terme_pointer(pos);
}

void terme_data_build(addr *ret)
{
	addr pos, value;
	struct terme_data_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_TERME,
			terme_data_size,
			sizeoft(struct terme_data_struct));
	terme_set_type(pos, terme_type_data);
	terme_string_build(&value);
	terme_set(pos, terme_data_array, value);

	str = struct_terme_data(pos);
	str->alloc = TERME_DATA_SIZE;
	str->size = 0;
	str->now = 0;

	*ret = pos;
}

static void terme_data_get_body(addr pos, addr *ret)
{
	Check(! terme_data_p(pos), "type error");
	terme_get(pos, terme_data_array, ret);
}

int terme_data_clear_(Execute ptr)
{
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	str->size = 0;
	str->now = 0;

	return 0;
}


/*
 *  insert
 */
static int terme_data_shift_right_(addr pos, unicode c, unsigned width)
{
	int i, diff, now, size, src, dst;
	addr array;
	struct terme_data_struct *str;
	struct terme_data_character *body;

	/* data */
	str = struct_terme_data(pos);
	now = str->now;
	size = str->size;
	if (size <= now)
		return 0;

	/* string */
	terme_data_get_body(pos, &array);
	body = struct_data_character(array);

	/* loop */
	diff = size - now;
	for (i = 0; i < diff; i++) {
		dst = size - i;
		src = dst - 1;
		body[dst] = body[src];
	}

	return 0;
}

static void terme_data_insert_set(addr pos, unsigned now, unicode c, unsigned width)
{
	addr array;
	struct terme_data_character *body;

	terme_data_get_body(pos, &array);
	body = struct_data_character(array);
	body += now;
	body->c = c;
	body->width = width;
}

int terme_data_insert_(Execute ptr, unicode c, unsigned *rwidth, int *ret)
{
	unsigned width;
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	if (str->alloc <= str->size) {
		*rwidth = 0;
		return Result(ret, 1);
	}
	if (str->size < str->now) {
		*rwidth = 0;
		return Result(ret, 1);
	}

	width = eastasian_width(c);
	Return(terme_data_shift_right_(pos, c, width));
	terme_data_insert_set(pos, str->now, c, width);
	str->size++;

	/* result */
	*rwidth = width;
	return Result(ret, 0);
}

int terme_data_next_(Execute ptr)
{
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	if (str->now < str->size)
		str->now++;

	return 0;
}

int terme_data_push_(Execute ptr, unicode c, unsigned *rwidth, int *ret)
{
	int check;
	unsigned width;

	Return(terme_data_insert_(ptr, c, &width, &check));
	if (check) {
		*rwidth = 0;
		return Result(ret, 1);
	}

	*rwidth = width;
	*ret = 0;
	return terme_data_next_(ptr);
}

int terme_data_make_(Execute ptr, addr *ret, int eol)
{
	unsigned size, i;
	addr pos, array, value;
	struct terme_data_struct *str;
	struct terme_data_character *body;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	size = str->size;
	strvect_heap(&value, size + (eol? 1ULL :0ULL));
	terme_data_get_body(pos, &array);
	body = struct_data_character(array);

	for (i = 0; i < size; i++) {
		Return(strvect_setc_(value, i, body[i].c));
	}
	if (eol) {
		Return(strvect_setc_(value, i, 0x0A));
	}

	return Result(ret, value);
}

int terme_data_copy_(Execute ptr, addr value)
{
	addr pos, array;
	struct terme_data_struct *str;
	struct terme_data_character *body;
	unicode c;
	size_t size, i;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	terme_data_get_body(pos, &array);
	body = struct_data_character(array);

	strvect_length(value, &size);
	if (str->alloc < size)
		size = str->alloc;
	for (i = 0; i < size; i++) {
		strvect_getc(value, i, &c);
		body[i].c = c;
		body[i].width = eastasian_width(c);
	}
	str->size = (unsigned)size;
	str->now = 0;

	return 0;
}

int terme_data_size_(Execute ptr, unsigned *ret)
{
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	return Result(ret, str->size);
}


/*
 *  access
 */
void terme_data_get_value(addr pos, unsigned *rnow, unsigned *rsize)
{
	struct terme_data_struct *str;

	str = struct_terme_data(pos);
	if (rnow)
		*rnow = str->now;
	if (rsize)
		*rsize = str->size;
}

int terme_data_get_character_(addr pos, unsigned i, unicode *retc, unsigned *retw)
{
	addr array;
	struct terme_data_struct *str;
	struct terme_data_character *body;

	str = struct_terme_data(pos);
	if (str->size <= i) {
		if (retc)
			*retc = 0;
		if (retw)
			*retw = 0;
		return terme_fmte_("terme_data_get_character_ error.", NULL);
	}

	terme_data_get_body(pos, &array);
	body = struct_data_character(array);
	body += i;
	if (retc)
		*retc = body->c;
	if (retw)
		*retw = body->width;
	return 0;
}


/*
 *  operator
 */
int terme_data_left_(Execute ptr, unsigned *ret)
{
	unsigned width;
	addr pos, array;
	struct terme_data_struct *str;
	struct terme_data_character *body;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	if (str->now == 0)
		return Result(ret, 0); /* error */

	/* width */
	terme_data_get_body(pos, &array);
	body = struct_data_character(array);
	str->now--;
	width = body[str->now].width;

	/* move */
	return Result(ret, width);
}

int terme_data_right_(Execute ptr, unsigned *ret)
{
	unsigned width;
	addr pos, array;
	struct terme_data_struct *str;
	struct terme_data_character *body;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	if (str->size <= str->now)
		return Result(ret, 0); /* error */

	/* width */
	terme_data_get_body(pos, &array);
	body = struct_data_character(array);
	width = body[str->now].width;
	str->now++;

	/* move */
	return Result(ret, width);
}

int terme_data_first_(Execute ptr)
{
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	str->now = 0;

	return 0;
}

int terme_data_last_(Execute ptr)
{
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	str->now = str->size;

	return 0;
}

static void terme_data_shift_left_(addr pos, unsigned x, unsigned size, unsigned *ret)
{
	addr array;
	struct terme_data_character *body;

	terme_data_get_body(pos, &array);
	body = struct_data_character(array);
	*ret = body[x].width;
	for (; x < size; x++)
		body[x] = body[x + 1];
}

static void terme_data_delete_index(addr pos, unsigned index, unsigned *ret)
{
	struct terme_data_struct *str;

	str = struct_terme_data(pos);
	if (str->size <= index) {
		*ret = 0;
		return;
	}

	/* shift */
	terme_data_shift_left_(pos, index, str->size, ret);
	str->size--;
}

int terme_data_delete_(Execute ptr, int *ret)
{
	unsigned width;
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	if (str->size <= str->now)
		return Result(ret, 0);

	terme_data_delete_index(pos, str->now, &width);
	return Result(ret, 1);
}

int terme_data_backspace_(Execute ptr, unsigned *ret)
{
	unsigned width;
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	if (str->now == 0)
		return Result(ret, 0);

	/* backspace */
	terme_data_delete_index(pos, str->now - 1U, &width);
	if (width == 0)
		return Result(ret, 0);
	str->now--;

	return Result(ret, width);
}

static int terme_data_rmleft_shift_(addr pos, unsigned index, unsigned size)
{
	unsigned i;
	addr array;
	struct terme_data_character *body;

	terme_data_get_body(pos, &array);
	body = struct_data_character(array);
	for (i = 0; i < size; i++)
		body[i] = body[i + index];

	return 0;
}

int terme_data_rmleft_(Execute ptr, int *ret)
{
	unsigned size;
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	/* do nothing */
	if (str->now == 0)
		return Result(ret, 0);

	/* all delete */
	if (str->now == str->size) {
		str->size = 0;
		str->now = 0;
		return Result(ret, 1);
	}

	/* shift */
	size = str->size - str->now;
	Return(terme_data_rmleft_shift_(pos, str->now, size));
	str->size = size;
	str->now = 0;

	return Result(ret, 1);
}

int terme_data_rmright_(Execute ptr, int *ret)
{
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	if (str->now < 0 || str->size <= str->now)
		return Result(ret, 0);

	/* shift */
	str->size = str->now;
	return Result(ret, 1);
}


/************************************************************
 *  terme_display.c
 ************************************************************/

#ifdef LISP_DEBUG
#define TERME_DISPLAY_ALLOC_X	3
#define TERME_DISPLAY_ALLOC_Y	3
#else
#define TERME_DISPLAY_ALLOC_X	64
#define TERME_DISPLAY_ALLOC_Y	64
#endif
#define TERME_DISPLAY_SIZE(x, y) (((x / y) + 1) * y)

/*
 *  line
 */
struct terme_line_body {
	unsigned wide : 1;
	unsigned ignore : 1;
	PromptMode mode : 5;
	unicode c : 21;
};
struct terme_line_root {
	unsigned alloc, size, now;
};

static struct terme_line_root *struct_terme_line(addr pos)
{
	Check(! terme_line_p(pos), "type error");
	return (struct terme_line_root *)terme_pointer(pos);
}

static struct terme_line_body *struct_terme_body(addr pos)
{
	Check(! terme_line_p(pos), "type error");
	return (struct terme_line_body *)
		(terme_pointer(pos) + sizeoft(struct terme_line_root));
}

static void terme_line_heap(addr *ret, unsigned alloc)
{
	addr pos;
	size_t size;
	struct terme_line_root *str;

	size = sizeoft(struct terme_line_root);
	size += sizeoft(struct terme_line_body) * alloc;
	heap_body(&pos, LISPSYSTEM_TERME, size);
	terme_set_type(pos, terme_type_line);
	str = struct_terme_line(pos);
	str->alloc = alloc;
	str->size = 0;
	str->now = 0;
	*ret = pos;
}

static void terme_line_resize_x(addr *ret, addr src_line,
		unsigned alloc_x, unsigned new_x)
{
	addr new_line;
	struct terme_line_root *str;
	struct terme_line_body *body1, *body2;

	str = struct_terme_line(src_line);
	if (alloc_x <= str->alloc) {
		*ret = src_line;
		return;
	}

	/* new object */
	terme_line_heap(&new_line, alloc_x);
	body1 = struct_terme_body(src_line);
	body2 = struct_terme_body(new_line);

	/* copy */
	memcpy(body2, body1, sizeoft(struct terme_line_body) * str->size);

	/* set array */
	str = struct_terme_line(new_line);
	str->alloc = alloc_x;
	str->size = new_x;
}

static void terme_line_resize(addr *ret, addr pos, unsigned alloc_x, unsigned new_x)
{
	struct terme_line_root *str;

	if (pos == Nil)
		terme_line_heap(&pos, alloc_x);
	else
		terme_line_resize_x(&pos, pos, alloc_x, new_x);
	str = struct_terme_line(pos);
	str->size = new_x;
	str->now = 0;
	*ret = pos;
}


/*
 *  display
 */
struct terme_display_struct {
	unsigned alloc_x, alloc_y;
	unsigned size_x, size_y;
	unsigned now_x, now_y;
};

static struct terme_display_struct *struct_terme_display(addr pos)
{
	Check(! terme_display_p(pos), "type error");
	return (struct terme_display_struct *)terme_pointer(pos);
}

void terme_display_build(addr *ret)
{
	addr pos;
	struct terme_display_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_TERME, 1, sizeoft(struct terme_display_struct));
	terme_set_type(pos, terme_type_display);
	str = struct_terme_display(pos);
	str->alloc_x = 0;
	str->alloc_y = 0;
	str->size_x = 0;
	str->size_y = 0;
	str->now_x = 0;
	str->now_y = 0;
	*ret = pos;
}

static void terme_display_resize_y(addr pos, unsigned new_y)
{
	unsigned alloc_y, src_y, y;
	addr new_array, src_array, value;
	struct terme_display_struct *str;

	/* new array */
	alloc_y = TERME_DISPLAY_SIZE(new_y, TERME_DISPLAY_ALLOC_Y);
	terme_get(pos, 0, &src_array);
	vector_heap(&new_array, alloc_y);

	/* copy */
	str = struct_terme_display(pos);
	src_y = str->size_y;
	for (y = 0; y < src_y; y++) {
		getarray(src_array, y, &value);
		setarray(new_array, y, value);
	}

	/* set array */
	terme_set(pos, 0, new_array);
	str->alloc_y = alloc_y;
	str->size_y = new_y;
}

static void terme_display_resize_x(addr pos, unsigned new_x)
{
	unsigned alloc_x, size_y, y;
	addr array, value;
	struct terme_display_struct *str;

	alloc_x = TERME_DISPLAY_SIZE(new_x, TERME_DISPLAY_ALLOC_X);
	terme_get(pos, 0, &array);

	/* copy */
	str = struct_terme_display(pos);
	size_y = str->size_y;
	for (y = 0; y < size_y; y++) {
		getarray(array, y, &value);
		terme_line_resize(&value, value, alloc_x, new_x);
		setarray(array, y, value);
	}

	/* set array */
	str->alloc_x = alloc_x;
	str->size_x = new_x;
}

static void terme_display_resize(addr pos, unsigned x, unsigned y)
{
	struct terme_display_struct *str;

	/* y */
	str = struct_terme_display(pos);
	if (str->alloc_y < y)
		terme_display_resize_y(pos, y);
	str->size_y = y;

	/* x */
	terme_display_resize_x(pos, x);
	str->size_x = x;

	/* cursor */
	str->now_x = 0;
	str->now_y = 0;
}

int terme_display_clear_(Execute ptr)
{
	unsigned x, y;
	addr pos;

	Return(terme_root_display_(ptr, &pos));
	terme_arch_size_get(&x, &y);
	terme_display_resize(pos, x, y);

	return 0;
}


/*
 *  operator
 */
static void terme_line_ignore(addr pos, unsigned x, PromptMode mode)
{
	unsigned first, last, i;
	struct terme_line_root *str;
	struct terme_line_body *body, *cursor;

	str = struct_terme_line(pos);
	if (x <= str->now)
		return;
	first = (str->now < str->size)? str->now: str->size;
	last = (x < str->size)? x: str->size;
	body = struct_terme_body(pos);

	for (i = first; i <= last; i++) {
		cursor = body + i;
		cursor->wide = 0;
		cursor->ignore = 1;
		cursor->mode = mode;
		cursor->c = 0;
	}
}

static void terme_line_write_char(addr pos, unsigned x,
		unicode c, unsigned width, PromptMode mode)
{
	int wide_p;
	struct terme_line_root *str;
	struct terme_line_body *body;

	str = struct_terme_line(pos);
	terme_line_ignore(pos, x, mode);

	/* character */
	wide_p = (width != 1);
	body = struct_terme_body(pos);
	body += str->now;
	body->wide = wide_p;
	body->ignore = 0;
	body->mode = mode;
	body->c = c;
	str->now = x + 1;

	/* eastasian width */
	if (! wide_p)
		return;
	if (str->size <= str->now)
		return;
	body++;
	body->wide = 0;
	body->ignore = 1;
	body->mode = mode;
	body->c = 0;
	str->now++;
}

int terme_display_write_char_(Execute ptr, unicode c, unsigned width, PromptMode mode)
{
	addr pos, array, line;
	struct terme_display_struct *str;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	if (str->size_y <= str->now_y) {
		str->now_x += width;
		return 0;
	}
	terme_get(pos, 0, &array);
	getarray(array, str->now_y, &line);
	terme_line_write_char(line, str->now_x, c, width, mode);
	str->now_x += width;

	return 0;
}

int terme_display_terpri_(Execute ptr)
{
	addr pos;
	struct terme_display_struct *str;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	if (str->now_y < str->size_y)
		str->now_y++;
	str->now_x = 0;

	return 0;
}

int terme_display_delete_line_right_(Execute ptr)
{
	addr pos;
	struct terme_display_struct *str;
	struct terme_line_root *root;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	if (str->size_y <= str->now_y)
		return 0;

	/* line */
	terme_get(pos, 0, &pos);
	getarray(pos, str->now_y, &pos);
	root = struct_terme_line(pos);
	if (str->now_x < root->now)
		root->now = str->now_x;

	return 0;
}

int terme_display_left_(Execute ptr, int n)
{
	unsigned value;
	addr pos;
	struct terme_display_struct *str;

	if (n <= 0)
		return 0;
	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	value = (unsigned)n;
	if (str->now_x <= value)
		str->now_x = 0;
	else
		str->now_x -= value;

	return 0;
}

int terme_display_right_(Execute ptr, int n)
{
	addr pos;
	struct terme_display_struct *str;

	if (n <= 0)
		return 0;
	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	str->now_x += (unsigned)n;

	return 0;
}

int terme_display_up_(Execute ptr, int n)
{
	addr pos;
	unsigned value;
	struct terme_display_struct *str;

	if (n <= 0)
		return 0;
	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	value = (unsigned)n;
	if (str->now_y <= value)
		str->now_y = 0;
	else
		str->now_y -= value;

	return 0;
}

int terme_display_down_(Execute ptr, int n)
{
	addr pos;
	struct terme_display_struct *str;

	if (n <= 0)
		return 0;
	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	str->now_y += (unsigned)n;
	if (str->now_y > str->size_y)
		str->now_y = str->size_y;

	return 0;
}

int terme_display_first_up_(Execute ptr, int n)
{
	unsigned value;
	addr pos;
	struct terme_display_struct *str;

	if (n <= 0)
		return 0;
	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	value = (unsigned)n;
	if (str->now_y <= value)
		str->now_y = 0;
	else
		str->now_y -= value;
	str->now_x = 0;

	return 0;
}

int terme_display_first_down_(Execute ptr, int n)
{
	addr pos;
	struct terme_display_struct *str;

	if (n <= 0)
		return 0;
	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	str->now_y += (unsigned)n;
	if (str->now_y > str->size_y)
		str->now_y = str->size_y;
	str->now_x = 0;

	return 0;
}

int terme_display_delete_line_(Execute ptr)
{
	addr pos;
	struct terme_display_struct *str;
	struct terme_line_root *root;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	if (str->size_y <= str->now_y)
		return 0;

	/* line */
	terme_get(pos, 0, &pos);
	getarray(pos, str->now_y, &pos);
	root = struct_terme_line(pos);
	root->now = 0;

	return 0;
}

int terme_display_getwidth_(Execute ptr, unsigned *ret)
{
	addr pos;
	struct terme_display_struct *str;
	struct terme_line_body *body;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	if (str->size_y <= str->now_y || str->size_x <= str->now_x)
		return Result(ret, 0);

	/* line */
	terme_get(pos, 0, &pos);
	getarray(pos, str->now_y, &pos);
	body = struct_terme_body(pos);
	body += str->now_x;
	return Result(ret, body->wide? 2: 1);
}

int terme_display_previous_(Execute ptr, int *ret)
{
	addr pos;
	struct terme_display_struct *str;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);

	/* out of range */
	if (str->size_y <= str->now_y)
		return Result(ret, -1);

	/* previous line */
	if (str->now_x == 0) {
		return Result(ret, str->now_y? 0: -1);
	}

	/* current line */
	return Result(ret, 1);
}

int terme_display_getlast_(Execute ptr, unsigned *ret)
{
	addr pos;
	struct terme_display_struct *str;
	struct terme_line_root *root;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	if (str->size_y <= str->now_y)
		return Result(ret, 0);

	/* line */
	terme_get(pos, 0, &pos);
	getarray(pos, str->now_y, &pos);
	root = struct_terme_line(pos);
	return Result(ret, root->now);
}

int terme_display_delete_page_(Execute ptr)
{
	unsigned y;
	addr pos, array, line;
	struct terme_display_struct *str;
	struct terme_line_root *root;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	terme_get(pos, 0, &array);
	for (y = 0; y < str->now_y; y++) {
		getarray(array, y, &line);
		root = struct_terme_line(line);
		root->now = 0;
	}
	str->now_x = 0;
	str->now_y = 0;

	return 0;
}


/*
 *  restore
 */
int terme_display_restore_(Execute ptr, unsigned *rx, unsigned *ry)
{
	*rx = *ry = 0;
	return 0;
}


/************************************************************
 *  terme_error.c
 ************************************************************/

/*
 *  error
 */
static int terme_fmte_va_(addr format, addr args)
{
	int mode, check;

	if (terme_arch_textmode(&mode)) {
		Abort("terme_arch_textmode error.");
		return 0;
	}
	check = call_simple_error_(NULL, format, args);
	if (mode && terme_arch_rawmode(NULL)) {
		Abort("terme_arch_rawmode error.");
		return 0;
	}

	return check;
}

int terme_fmte_(const char *str, ...)
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


/************************************************************
 *  terme_escape.c
 ************************************************************/

static int terme_color_enable(Execute ptr)
{
	addr symbol, pos;

	GetConst(SYSTEM_PROMPT_COLOR, &symbol);
	if (ptr == NULL) {
		GetValueSymbol(symbol, &pos);
	}
	else {
		getspecial_local(ptr, symbol, &pos);
	}

	return pos == Unbound || pos != Nil;
}

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

	return 0;
}

static int terme_escape_operator_(const char *str)
{
	if (terme_escape_operator(str))
		return fmte_("terme_escape_operator error.", NULL);
	return 0;
}

int terme_font(Execute ptr, PrintFont value)
{
	const char *str;

	if (! terme_color_enable(ptr))
		return 0;
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

int terme_text_color(Execute ptr, PrintColor value)
{
	const char *str;

	if (! terme_color_enable(ptr))
		return 0;
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

int terme_back_color(Execute ptr, PrintColor value)
{
	const char *str;

	if (! terme_color_enable(ptr))
		return 0;
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

static int terme_cursor_move_character(int n, byte c)
{
	char data[64];

	if (n <= 0)
		return 0;
	if (n == 1) {
		if (terme_escape_operator("\x1B["))
			return 1;
		return terme_write_byte(c);
	}
	if (n <= 255) {
		snprintf(data, 64, "\x1B[%d", n);
		if (terme_escape_operator(data))
			return 1;
		return terme_write_byte(c);
	}

	/* over 256 */
	snprintf(data, 64, "\x1B[255");
	if (terme_escape_operator(data))
		return 1;
	if (terme_write_byte(c))
		return 1;

	return terme_cursor_move_character(n - 255, c);
}

int terme_cursor_left(int n)
{
	return terme_cursor_move_character(n, 'D');
}

int terme_cursor_right(int n)
{
	return terme_cursor_move_character(n, 'C');
}

int terme_cursor_up(int n)
{
	return terme_cursor_move_character(n, 'A');
}

int terme_cursor_down(int n)
{
	return terme_cursor_move_character(n, 'B');
}

int terme_cursor_move_x(int x)
{
	char data[64];
	snprintf(data, 64, "\x1B[%dG", x + 1);
	return terme_escape_operator(data);
}

int terme_cursor_move(int x, int y)
{
	char data[64];
	snprintf(data, 64, "\x1B[%d;%dH", y + 1, x + 1);
	return terme_escape_operator(data);
}

int terme_cursor_first_up(int n)
{
	return terme_cursor_move_character(n, 'F');
}

int terme_cursor_first_down(int n)
{
	return terme_cursor_move_character(n, 'E');
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

int terme_cursor_delete_page_left(void)
{
	return terme_escape_operator("\x1B[1J")
		|| terme_escape_operator("\x1B[1;1H");
}

int terme_cursor_delete_page_right(void)
{
	return terme_escape_operator("\x1B[J");
}

int terme_cursor_delete_page(void)
{
	return terme_escape_operator("\x1B[2J")
		|| terme_escape_operator("\x1B[1;1H");
}

int terme_cursor_scroll_up(int n)
{
	return terme_cursor_move_character(n, 'T');
}

int terme_cursor_scroll_down(int n)
{
	return terme_cursor_move_character(n, 'S');
}


/*
 *  font
 *    (terme 'terme-font nil)
 *    (terme 'terme-font 'code 3)
 *    (terme 'terme-font 'code 'italic)
 *    (terme 'terme-font 'fore 'red)
 *    (terme 'terme-font 'back 'red)
 *    (terme 'terme-font 'code 'italic 'fore 'red 'back 'black)
 *    (terme 'terme-font 'palfore 10 'palback 20)
 *    (terme 'terme-font 'rgbfore 30 40 50 'rgbback 60 70 80)
 */
struct terme_font_struct {
	const char *name;
	int value;
};

static struct terme_font_struct terme_struct_code[] = {
	{ "RESET",           0 },
	{ "BOLD",            1 },
	{ "FAINT",           2 },
	{ "ITALIC",          3 },
	{ "UNDERLINE",       4 },
	{ "SLOW-BLINK",      5 },
	{ "RAPID-BLINK",     6 },
	{ "REVERSE",         7 },
	{ "HIDE",            8 },
	{ "STRIKE",          9 },
	{ NULL,              0 }
};

static struct terme_font_struct terme_struct_color[] = {
	{ "BLACK",           0 },
	{ "RED",             1 },
	{ "GREEN",           2 },
	{ "YELLOW",          3 },
	{ "BLUE",            4 },
	{ "MAGENTA",         5 },
	{ "CYAN",            6 },
	{ "WHITE",           7 },
	{ "DEFAULT",         9 },
	{ NULL,              0 }
};

static struct terme_font_struct terme_struct_dark[] = {
	{ "DARK-BLACK",      0 },
	{ "DARK-RED",        1 },
	{ "DARK-GREEN",      2 },
	{ "DARK-YELLOW",     3 },
	{ "DARK-BLUE",       4 },
	{ "DARK-MAGENTA",    5 },
	{ "DARK-CYAN",       6 },
	{ "DARK-WHITE",      7 },
	{ NULL,              0 }
};

static struct terme_font_struct terme_struct_bright[] = {
	{ "BRIGHT-BLACK",    0 },
	{ "BRIGHT-RED",      1 },
	{ "BRIGHT-GREEN",    2 },
	{ "BRIGHT-YELLOW",   3 },
	{ "BRIGHT-BLUE",     4 },
	{ "BRIGHT-MAGENTA",  5 },
	{ "BRIGHT-CYAN",     6 },
	{ "BRIGHT-WHITE",    7 },
	{ NULL,              0 }
};

static struct terme_font_struct terme_struct_not_color[] = {
	{ "NOT-BLACK",       0 },
	{ "NOT-RED",         1 },
	{ "NOT-GREEN",       2 },
	{ "NOT-YELLOW",      3 },
	{ "NOT-BLUE",        4 },
	{ "NOT-MAGENTA",     5 },
	{ "NOT-CYAN",        6 },
	{ "NOT-WHITE",       7 },
	{ "NOT-DEFAULT",     9 },
	{ NULL,              0 }
};

static int terme_struct_find_(struct terme_font_struct *array,
		addr pos, int *ret, int *value)
{
	int i, check;
	struct terme_font_struct *str;

	if (symbolp(pos))
		GetNameSymbol(pos, &pos);
	if (! stringp(pos))
		goto error;
	for (i = 0; ; i++) {
		str = array + i;
		if (str->name == NULL)
			break;
		Return(string_equal_char_(pos, str->name, &check));
		if (check) {
			if (value)
				*value = str->value;
			return Result(ret, 1);
		}
	}

error:
	return Result(ret, 0);
}

static int terme_font_bright_mode(Execute ptr)
{
	addr pos;

	GetConst(SYSTEM_PROMPT_BRIGHT, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Unbound)
		pos = Nil;

	return pos != Nil;
}

static int terme_struct_integer_(struct terme_font_struct *array, addr pos, int *ret)
{
	int value, check;
	Return(terme_struct_find_(array, pos, &check, &value));
	return Result(ret, check? value: -1);
}
int terme_color_symbol_(Execute ptr, addr pos, int *ret, int *brightp)
{
	if (brightp)
		*brightp = terme_font_bright_mode(ptr);
	return terme_struct_integer_(terme_struct_color, pos, ret);
}
int terme_color_not_symbol_(Execute ptr, addr pos, int *ret, int *brightp)
{
	if (brightp)
		*brightp = ! terme_font_bright_mode(ptr);
	return terme_struct_integer_(terme_struct_not_color, pos, ret);
}
int terme_color_dark_(addr pos, int *ret)
{
	return terme_struct_integer_(terme_struct_dark, pos, ret);
}
int terme_color_bright_(addr pos, int *ret)
{
	return terme_struct_integer_(terme_struct_bright, pos, ret);
}

static int terme_font_parser_list_code_(addr list, addr *ret)
{
	int check;
	addr pos;

	Return_getcons(list, &pos, ret);

	/* integer */
	if (integerp(pos)) {
		Return(getint_unsigned_(pos, &check));
		if (0xFF < check)
			return fmte_("Too large integer check, ~S.", pos, NULL);
		return 0;
	}

	/* name */
	Return(terme_struct_find_(terme_struct_code, pos, &check, NULL));
	if (check)
		return 0;

	/* error */
	return fmte_("Invalid value, ~S.", pos, NULL);
}

static int terme_font_parser_list_color_(addr list, addr *ret)
{
	int check;
	addr pos;

	Return_getcons(list, &pos, ret);

	/* color */
	Return(terme_color_symbol_(NULL, pos, &check, NULL));
	if (0 <= check)
		return 0;

	/* not-color */
	Return(terme_color_not_symbol_(NULL, pos, &check, NULL));
	if (0 <= check)
		return 0;

	/* dark */
	Return(terme_color_dark_(pos, &check));
	if (0 <= check)
		return 0;

	/* bright */
	Return(terme_color_bright_(pos, &check));
	if (0 <= check)
		return 0;

	/* error */
	return fmte_("Invalid value, ~S.", pos, NULL);
}

static int terme_font_parser_list_palette_(addr list, addr *ret, int *value)
{
	int check;
	addr pos;

	Return_getcons(list, &pos, ret);
	/* integer */
	if (! integerp(pos))
		return fmte_("Invalid value, ~S.", pos, NULL);
	Return(getint_unsigned_(pos, &check));
	if (0xFF < check)
		return fmte_("Too large integer value, ~S.", pos, NULL);
	if (value)
		*value = check;

	return 0;
}

static int terme_font_parser_list_rgb_(addr list, addr *ret)
{
	Return(terme_font_parser_list_palette_(list, &list, NULL));
	Return(terme_font_parser_list_palette_(list, &list, NULL));
	Return(terme_font_parser_list_palette_(list, &list, NULL));
	return Result(ret, list);
}

static int terme_font_parser_list_(addr list, addr pos, addr *ret)
{
	int check;

	if (symbolp(pos))
		GetNameSymbol(pos, &pos);
	if (! stringp(pos))
		goto error;

	/* code */
	Return(string_equal_char_(pos, "CODE", &check));
	if (check)
		return terme_font_parser_list_code_(list, ret);

	/* fore */
	Return(string_equal_char_(pos, "FORE", &check));
	if (check)
		return terme_font_parser_list_color_(list, ret);

	/* back */
	Return(string_equal_char_(pos, "BACK", &check));
	if (check)
		return terme_font_parser_list_color_(list, ret);

	/* palfore */
	Return(string_equal_char_(pos, "PALFORE", &check));
	if (check)
		return terme_font_parser_list_palette_(list, ret, NULL);

	/* palback */
	Return(string_equal_char_(pos, "PALBACK", &check));
	if (check)
		return terme_font_parser_list_palette_(list, ret, NULL);

	/* rgbfore */
	Return(string_equal_char_(pos, "RGBFORE", &check));
	if (check)
		return terme_font_parser_list_rgb_(list, ret);

	/* rgbback */
	Return(string_equal_char_(pos, "RGBBACK", &check));
	if (check)
		return terme_font_parser_list_rgb_(list, ret);

	/* error */
error:
	return fmte_("Invalid operator, ~S.", pos, NULL);
}

int terme_font_parser_(addr args)
{
	addr list, pos;

	list = args;
	if (list == Nil)
		return 0; /* reset */
	Return_getcons(list, &pos, &list);
	if (pos == Nil) {
		if (list != Nil)
			return fmte_("Invalid font format, ~S.", args, NULL);
		/* reset */
		return 0;
	}

	/* loop */
	list = args;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(terme_font_parser_list_(list, pos, &list));
	}

	return 0;
}


/* update */
static int terme_font_update_single_(int value)
{
	char data[64];
	snprintf(data, 64, "%d", value);
	return terme_escape_operator_(data);
}

static int terme_font_update_list_code_(addr list, addr *ret)
{
	int check, value;
	addr pos;

	Return_getcons(list, &pos, ret);

	/* integer */
	if (integerp(pos)) {
		Return(getint_unsigned_(pos, &check));
		if (0xFF < check)
			return fmte_("Too large integer check, ~S.", pos, NULL);
		return terme_font_update_single_(check);
	}

	/* name */
	Return(terme_struct_find_(terme_struct_code, pos, &check, &value));
	if (check)
		return terme_font_update_single_(value);

	/* error */
	return fmte_("Invalid value, ~S.", pos, NULL);
}

static int terme_font_update_list_color_mode_(Execute ptr,
		addr list, addr *ret, int dark, int bright)
{
	int check, brightp;
	addr pos;

	Return_getcons(list, &pos, ret);

	/* color */
	Return(terme_color_symbol_(ptr, pos, &check, &brightp));
	if (0 <= check)
		return terme_font_update_single_(check + (brightp? bright: dark));

	/* not-color */
	Return(terme_color_not_symbol_(ptr, pos, &check, &brightp));
	if (0 <= check)
		return terme_font_update_single_(check + (brightp? bright: dark));

	/* dark */
	Return(terme_color_dark_(pos, &check));
	if (0 <= check)
		return terme_font_update_single_(check + dark);

	/* bright */
	Return(terme_color_bright_(pos, &check));
	if (0 <= check)
		return terme_font_update_single_(check + bright);

	/* error */
	return fmte_("Invalid value, ~S.", pos, NULL);
}

static int terme_font_update_list_fore_(Execute ptr, addr list, addr *ret)
{
	return terme_font_update_list_color_mode_(ptr, list, ret, 30, 90);
}

static int terme_font_update_list_back_(Execute ptr, addr list, addr *ret)
{
	return terme_font_update_list_color_mode_(ptr, list, ret, 40, 100);
}

static int terme_font_update_list_palette_(addr list, addr *ret, int id)
{
	int value;

	Return(terme_font_parser_list_palette_(list, ret, &value));
	Return(terme_font_update_single_(id));
	Return(terme_escape_operator_(";5;"));
	Return(terme_font_update_single_(value));

	return 0;
}

static int terme_font_update_list_palfore_(addr list, addr *ret)
{
	return terme_font_update_list_palette_(list, ret, 38);
}

static int terme_font_update_list_palback_(addr list, addr *ret)
{
	return terme_font_update_list_palette_(list, ret, 48);
}

static int terme_font_update_list_rgb_(addr list, addr *ret, int id)
{
	int r, g, b;

	Return(terme_font_parser_list_palette_(list, &list, &r));
	Return(terme_font_parser_list_palette_(list, &list, &g));
	Return(terme_font_parser_list_palette_(list, &list, &b));
	Return(terme_font_update_single_(id));
	Return(terme_escape_operator_(";2;"));
	Return(terme_font_update_single_(r));
	Return(terme_escape_operator_(";"));
	Return(terme_font_update_single_(g));
	Return(terme_escape_operator_(";"));
	Return(terme_font_update_single_(b));

	return Result(ret, list);
}

static int terme_font_update_list_rgbfore_(addr list, addr *ret)
{
	return terme_font_update_list_rgb_(list, ret, 38);
}

static int terme_font_update_list_rgbback_(addr list, addr *ret)
{
	return terme_font_update_list_rgb_(list, ret, 48);
}

static int terme_font_update_list_(Execute ptr, addr list, addr pos, addr *ret)
{
	int check;

	if (symbolp(pos))
		GetNameSymbol(pos, &pos);
	if (! stringp(pos))
		goto error;

	/* code */
	Return(string_equal_char_(pos, "CODE", &check));
	if (check)
		return terme_font_update_list_code_(list, ret);

	/* fore */
	Return(string_equal_char_(pos, "FORE", &check));
	if (check)
		return terme_font_update_list_fore_(ptr, list, ret);

	/* back */
	Return(string_equal_char_(pos, "BACK", &check));
	if (check)
		return terme_font_update_list_back_(ptr, list, ret);

	/* palfore */
	Return(string_equal_char_(pos, "PALFORE", &check));
	if (check)
		return terme_font_update_list_palfore_(list, ret);

	/* palback */
	Return(string_equal_char_(pos, "PALBACK", &check));
	if (check)
		return terme_font_update_list_palback_(list, ret);

	/* rgbfore */
	Return(string_equal_char_(pos, "RGBFORE", &check));
	if (check)
		return terme_font_update_list_rgbfore_(list, ret);

	/* rgbback */
	Return(string_equal_char_(pos, "RGBBACK", &check));
	if (check)
		return terme_font_update_list_rgbback_(list, ret);

	/* error */
error:
	return fmte_("Invalid operator, ~S.", pos, NULL);
}

int terme_font_update_(Execute ptr, addr args)
{
	int semi;
	addr list, pos;

	list = args;
	if (list == Nil)
		return terme_escape_operator_("\x1B[0m");
	Return_getcons(list, &pos, &list);
	if (pos == Nil)
		return terme_escape_operator_("\x1B[0m");

	/* loop */
	Return(terme_escape_operator_("\x1B["));
	list = args;
	semi = 0;
	while (list != Nil) {
		if (semi) {
			Return(terme_escape_operator_(";"));
		}
		Return_getcons(list, &pos, &list);
		Return(terme_font_update_list_(ptr, list, pos, &list));
		semi = 1;
	}
	Return(terme_escape_operator_("m"));

	return 0;
}


/************************************************************
 *  terme_function.c
 ************************************************************/

/*
 *  enable
 */
int terme_call_enable_p(void)
{
	return terme_arch_enable();
}

static int terme_call_enable_(void)
{
	if (! terme_arch_enable())
		return fmte_("TERME is not enabled.", NULL);
	return 0;
}


/*
 *  input
 */
int terme_call_input_(addr args, addr *rtype, addr *rvalue)
{
	/* (terme 'terme-input &optional (blocking t)) */
	int int_value;
	double_float float_value;
	addr pos;

	Return(terme_call_enable_());
	if (args == Nil) {
		terme_input_infinite(rtype, rvalue);
		return 0;
	}
	Return_getcons(args, &pos, &args);
	if (args != Nil) {
		*rtype = *rvalue = Nil;
		return fmte_("Invalid arguments, ~S.", args, NULL);
	}
	if (pos == T) {
		terme_input_infinite(rtype, rvalue);
		return 0;
	}
	if (pos == Nil) {
		terme_input_integer(0, rtype, rvalue);
		return 0;
	}
	if (integerp(pos)) {
		Return(getint_unsigned_(pos, &int_value));
		terme_input_integer(int_value, rtype, rvalue);
		return 0;
	}
	if (floatp(pos)) {
		Return(cast_double_float_unsafe_(pos, &float_value));
		terme_input_float((double)float_value, rtype, rvalue);
		return 0;
	}

	*rtype = *rvalue = Nil;
	return fmte_("Invalid blocking type, ~S.", pos, NULL);
}


/*
 *  output
 */
static int terme_call_output_character_(unicode c)
{
	addr pos;

	if (! isBaseType(c)) {
		fixnum_heap(&pos, (fixnum)c);
		return fmte_("Invalid code, ~S.", NULL);
	}
	if (terme_output_char(c))
		return fmte_("terme_output_char error.", NULL);

	return 0;
}

static int terme_call_output_string_(addr x)
{
	unicode c;
	size_t size, i;

	string_length(x, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(x, i, &c));
		Return(terme_call_output_character_(c));
	}

	return 0;
}

static int terme_call_output_object_(addr x)
{
	unicode c;
	fixnum v;

	switch (GetType(x)) {
		case LISPTYPE_CHARACTER:
			GetCharacter(x, &c);
			return terme_call_output_character_(c);

		case LISPTYPE_FIXNUM:
			GetFixnum(x, &v);
			if (v < 0)
				return fmte_("Invalid value, ~S.", x, NULL);
			return terme_call_output_character_((unicode)v);

		default:
			return fmte_("Invalid value, ~S.", x, NULL);
	}
}

static int terme_call_output_array_t_(addr x, size_t front)
{
	addr value;
	size_t i;

	for (i = 0; i < front; i++) {
		Return(array_get_t_(x, i, &value));
		Return(terme_call_output_object_(value));
	}

	return 0;
}

static int terme_call_output_array_character_(addr x, size_t front)
{
	unicode c;
	size_t i;

	for (i = 0; i < front; i++) {
		Return(array_get_unicode_(x, i, &c));
		Return(terme_call_output_character_(c));
	}

	return 0;
}

static int terme_call_output_array_signed_(const struct array_value *str, int *ret)
{
	int8_t i8;
	int16_t i16;
	int32_t i32;
#ifdef LISP_64BIT
	int64_t i64;
#endif

	switch (str->size) {
		case 8:
			i8 = str->value.signed8;
			if (i8 < 0)
				goto error;
			Return(terme_call_output_character_((unicode)i8));
			break;

		case 16:
			i16 = str->value.signed16;
			if (i16 < 0)
				goto error;
			Return(terme_call_output_character_((unicode)i16));
			break;

		case 32:
			i32 = str->value.signed32;
			if (i32 < 0 || isBaseType((uint32_t)i32))
				goto error;
			Return(terme_call_output_character_((unicode)i32));
			break;

#ifdef LISP_64BIT
		case 64:
			i64 = str->value.signed64;
			if (i64 < 0 || isBaseType((uint64_t)i64))
				goto error;
			Return(terme_call_output_character_((unicode)i64));
			break;
#endif
		default:
			goto error;
	}
	return Result(ret, 0);

error:
	return Result(ret, 1);
}

static int terme_call_output_array_unsigned_(const struct array_value *str, int *ret)
{
	uint8_t u8;
	uint16_t u16;
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (str->size) {
		case 8:
			u8 = str->value.unsigned8;
			Return(terme_call_output_character_((unicode)u8));
			break;

		case 16:
			u16 = str->value.unsigned16;
			Return(terme_call_output_character_((unicode)u16));
			break;

		case 32:
			u32 = str->value.unsigned32;
			if (isBaseType(u32))
				goto error;
			Return(terme_call_output_character_((unicode)u32));
			break;

#ifdef LISP_64BIT
		case 64:
			u64 = str->value.unsigned64;
			if (isBaseType(u64))
				goto error;
			Return(terme_call_output_character_((unicode)u64));
			break;
#endif
		default:
			goto error;
	}
	return Result(ret, 0);

error:
	return Result(ret, 1);
}

static int terme_call_output_array_value_(addr x, size_t front)
{
	int check;
	size_t i;
	struct array_value str;

	for (i = 0; i < front; i++) {
		Return(arrayinplace_get_(x, i, &str));
		check = 1;
		switch (str.type) {
			case ARRAY_TYPE_SIGNED:
				Return(terme_call_output_array_signed_(&str, &check));
				break;

			case ARRAY_TYPE_UNSIGNED:
				Return(terme_call_output_array_unsigned_(&str, &check));
				break;

			default:
				break;
		}
		if (check) {
			Return(array_get_(NULL, x, i, &x));
			return fmte_("Invalid value, ~S.", x, NULL);
		}
	}

	return 0;
}

static int terme_call_output_array_(addr x)
{
	struct array_struct *str;
	size_t front;

	str = ArrayInfoStruct(x);
	if (str->dimension != 1)
		return fmte_("Array ~S dimension must be a 1.", x, NULL);

	front = str->front;
	switch (str->type) {
		case ARRAY_TYPE_T:
			return terme_call_output_array_t_(x, front);

		case ARRAY_TYPE_CHARACTER:
			return terme_call_output_array_character_(x, front);

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return terme_call_output_array_value_(x, front);

		default:
			return fmte_("Invalid array type, ~S.", x, NULL);
	}
}

static int terme_call_output_vector_(addr x)
{
	addr value;
	size_t i, size;

	lenarray(x, &size);
	for (i = 0; i < size; i++) {
		getarray(x, i, &value);
		Return(terme_call_output_object_(value));
	}

	return 0;
}

static int terme_call_output_call_(addr x)
{
	unicode c;
	fixnum intvalue;

	/* flush */
	if (x == Nil) {
		if (terme_finish_output())
			return fmte_("terme_finish_output error.", NULL);
		return 0;
	}

	/* output */
	if (characterp(x)) {
		GetCharacter(x, &c);
		return terme_call_output_character_(c);
	}
	if (stringp(x)) {
		return terme_call_output_string_(x);
	}
	if (integerp(x)) {
		Return(getfixnum_unsigned_(x, &intvalue));
		return terme_call_output_character_((unicode)intvalue);
	}
	if (arrayp(x)) {
		return terme_call_output_array_(x);
	}
	if (GetType(x) == LISPTYPE_VECTOR) {
		return terme_call_output_vector_(x);
	}

	return fmte_("Invalid output value, ~S.", x, NULL);
}

int terme_call_output_(addr args)
{
	/* (terme 'terme-output &rest args) */
	addr x;

	Return(terme_call_enable_());
	if (args == Nil) {
		if (terme_finish_output())
			return fmte_("terme_finish_output error.", NULL);
		return 0;
	}

	while (args != Nil) {
		Return_getcons(args, &x, &args);
		Return(terme_call_output_call_(x));
	}

	return 0;
}


/*
 *  move
 */
static int terme_call_unsigned_(addr pos, int *ret)
{
	if (pos == Nil)
		return Result(ret, 0);
	else
		return getint_unsigned_(pos, ret);
}

static int terme_call_move_absolute_(addr pos_x, addr pos_y)
{
	int x, y, check;

	if (pos_x == Nil)
		return fmte_("Invalid x-position, ~S.", pos_x, NULL);
	Return(terme_call_unsigned_(pos_x, &x));
	if (pos_y == Nil) {
		check = terme_cursor_move_x(x);
	}
	else {
		Return(terme_call_unsigned_(pos_y, &y));
		check = terme_cursor_move(x, y);
	}
	if (check)
		return fmte_("terme_cursor error.", NULL);

	return 0;
}

static int terme_call_signed_(addr pos, int *ret)
{
	if (pos == Nil)
		return Result(ret, 0);
	else
		return getint_signed_(pos, ret);
}

static int terme_call_move_relative_(addr pos_x, addr pos_y)
{
	int check, x, y;

	Return(terme_call_signed_(pos_x, &x));
	Return(terme_call_signed_(pos_y, &y));

	/* x */
	check = 0;
	if (x) {
		if (x < 0)
			check = terme_cursor_left(-x);
		if (x > 0)
			check = terme_cursor_right(x);
	}
	if (check)
		return fmte_("terme_cursor x error.", NULL);

	/* y */
	check = 0;
	if (y) {
		if (x < 0)
			check = terme_cursor_up(-x);
		if (x > 0)
			check = terme_cursor_down(x);
	}
	if (check)
		return fmte_("terme_cursor y error.", NULL);

	return 0;
}

int terme_call_move_(addr args)
{
	addr x, y, pos, check;

	Return(terme_call_enable_());
	/* x, y */
	Return_getcons(args, &x, &args);
	Return_getcons(args, &y, &args);
	Return_getcar(args, &pos);
	/* relative */
	GetConst(KEYWORD_RELATIVE, &check);
	if (pos == check)
		return terme_call_move_relative_(x, y);
	/* absolute */
	GetConst(KEYWORD_ABSOLUTE, &check);
	if (pos == check)
		return terme_call_move_absolute_(x, y);

	/* error */
	return fmte_("Value ~S must be a (member :relative :absolute).", pos, NULL);
}


/*
 *  clear
 */
static int terme_call_clear_all_(void)
{
	if (terme_cursor_delete_page())
		return fmte_("terme_cursor_delete_page error.", NULL);
	return 0;
}

static int terme_call_clear_before_(void)
{
	if (terme_cursor_delete_page_left())
		return fmte_("terme_cursor_delete_page_left error.", NULL);
	return 0;
}

static int terme_call_clear_after_(void)
{
	if (terme_cursor_delete_page_right())
		return fmte_("terme_cursor_delete_page_right error.", NULL);
	return 0;
}

int terme_call_clear_(addr args)
{
	/* (terme 'terme-clear &optional x)
	 *   x  (member :before :after nil)
	 */
	addr pos, check;

	Return(terme_call_enable_());
	/* all */
	if (args == Nil)
		return terme_call_clear_all_();
	Return_getcons(args, &pos, &args);
	if (args != Nil)
		return fmte_("Invalid arguments, ~S.", args, NULL);
	if (pos == Nil)
		return terme_call_clear_all_();

	/* :before */
	GetConst(KEYWORD_BEFORE, &check);
	if (pos == check)
		return terme_call_clear_before_();

	/* :after */
	GetConst(KEYWORD_AFTER, &check);
	if (pos == check)
		return terme_call_clear_after_();

	return fmte_("Invalid operator, ~S.", pos, NULL);
}


/*
 *  delete
 */
static int terme_call_delete_all_(void)
{
	if (terme_cursor_delete_line())
		return fmte_("terme_cursor_delete_line error.", NULL);
	return 0;
}

static int terme_call_delete_before_(void)
{
	if (terme_cursor_delete_line_left())
		return fmte_("terme_cursor_delete_line_left error.", NULL);
	return 0;
}

static int terme_call_delete_after_(void)
{
	if (terme_cursor_delete_line_right())
		return fmte_("terme_cursor_delete_line_right error.", NULL);
	return 0;
}

int terme_call_delete_(addr args)
{
	/* (terme 'terme-delete &optional x)
	 *   x  (member :before :after nil)
	 */
	addr pos, check;

	Return(terme_call_enable_());
	/* all */
	if (args == Nil)
		return terme_call_delete_all_();
	Return_getcar(args, &pos);
	if (pos == Nil)
		return terme_call_delete_all_();

	/* :before */
	GetConst(KEYWORD_BEFORE, &check);
	if (pos == check)
		return terme_call_delete_before_();

	/* :after */
	GetConst(KEYWORD_AFTER, &check);
	if (pos == check)
		return terme_call_delete_after_();

	return fmte_("Invalid operator, ~S.", pos, NULL);
}


/*
 *  font
 */
int terme_call_font_(Execute ptr, addr args)
{
	Return(terme_call_enable_());
	Return(terme_font_parser_(args));
	Return(terme_font_update_(ptr, args));
	return 0;
}


/*
 *  size
 */
int terme_call_size_(addr *rx, addr *ry)
{
	unsigned x, y;

	Return(terme_call_enable_());
	if (terme_arch_size_update())
		return fmte_("terme_arch_size_update error.", NULL);
	terme_arch_size_get(&x, &y);
	fixnum_heap(rx, (fixnum)x);
	fixnum_heap(ry, (fixnum)y);

	return 0;
}


/*
 *  scroll
 */
static int terme_call_scroll_up_(int value)
{
	if (terme_cursor_scroll_up(value))
		return fmte_("terme_cursor_scroll_up error.", NULL);
	return 0;
}

static int terme_call_scroll_down_(int value)
{
	if (terme_cursor_scroll_down(value))
		return fmte_("terme_cursor_scroll_down error.", NULL);
	return 0;
}

int terme_call_scroll_(addr args)
{
	int value;
	addr pos;

	Return(terme_call_enable_());
	Return_getcons(args, &pos, &args);
	if (args != Nil)
		return fmte_("Invalid arguments, ~S.", args, NULL);
	Return(getint_signed_(pos, &value));
	if (value < 0)
		return terme_call_scroll_up_(-value);
	if (value > 0)
		return terme_call_scroll_down_(value);

	return 0;
}


/*
 *  begin
 */
int terme_call_begin_(addr args, addr *ret)
{
	addr pos, check;

	if (args == Nil)
		goto raw_mode;
	Return_getcons(args, &pos, &args);
	if (args != Nil)
		goto error;
	if (pos == Nil)
		goto raw_mode;
	GetConst(KEYWORD_DEFAULT, &check);
	if (pos == check)
		goto default_mode;
error:
	*ret = Nil;
	return fmte_("Invalid arguments, ~S.", args, NULL);

default_mode:
	return terme_arch_begin_default_(ret);
raw_mode:
	return terme_arch_begin_rawmode_(ret);
}


/*
 *  end
 */
int terme_call_end_(addr pos)
{
	addr type;

	/* argument */
	if (! paperp(pos)) {
		GetConst(SYSTEM_PAPER, &type);
		return call_type_error_va_(NULL, pos, type,
				"Object ~S must be a PAPER type.", pos, NULL);
	}

	/* flush */
	if (terme_finish_output())
		return fmte_("terme_finish_output error.", NULL);

	/* rollback */
	return terme_arch_restore_(pos);
}


/*
 *  signal
 */
static int terme_call_signal_sigint_(void)
{
	if (terme_arch_terminal_sigint_())
		return fmte_("kill error.", NULL);

	return 0;
}

static int terme_call_signal_stop_(void)
{
	if (terme_arch_terminal_stop_())
		return fmte_("kill error.", NULL);

	return 0;
}

int terme_call_signal_(addr args)
{
	int check;
	addr pos;

	Return(terme_call_enable_());
	/* arguments */
	if (args == Nil)
		return fmte_("Invalid arguments.", NULL);
	Return_getcons(args, &pos, &args);
	if (args != Nil)
		return fmte_("Invalid arguments, ~S.", args, NULL);

	/* sigint */
	Return(string_designator_equalp_char_(pos, "SIGINT", &check));
	if (check)
		return terme_call_signal_sigint_();

	/* stop */
	Return(string_designator_equalp_char_(pos, "STOP", &check));
	if (check)
		return terme_call_signal_stop_();

	/* error */
	return fmte_("Invalid arguments, ~S.", pos, NULL);
}


/************************************************************
 *  terme_history.c
 ************************************************************/

#define TERME_HISTORY_SIZE	64

struct terme_history_struct {
	int now, index, size;
};

static struct terme_history_struct *struct_terme_history(addr pos)
{
	Check(! terme_history_p(pos), "type error");
	return (struct terme_history_struct *)terme_pointer(pos);
}

void terme_history_build(addr *ret)
{
	addr pos, array;
	struct terme_history_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_TERME, 1, sizeoft(struct terme_history_struct));
	terme_set_type(pos, terme_type_history);
	str = struct_terme_history(pos);
	str->now = 0;
	str->index = 0;
	str->size = TERME_HISTORY_SIZE;

	/* array */
	vector_heap(&array, str->size);
	terme_set(pos, 0, array);
	*ret = pos;
}

int terme_history_clear_(Execute ptr)
{
	addr pos;
	struct terme_history_struct *str;

	Return(terme_root_history_(ptr, &pos));
	str = struct_terme_history(pos);
	str->now = 0;

	return 0;
}


/*
 *  return
 */
int terme_history_return_(Execute ptr)
{
	addr value, pos, array;
	struct terme_history_struct *str;

	/* data */
	Return(terme_data_make_(ptr, &value, 0));

	/* object */
	Return(terme_root_history_(ptr, &pos));
	str = struct_terme_history(pos);
	terme_get(pos, 0, &array);
	setarray(array, str->index, value);

	/* index */
	str->now = 0;
	str->index = (str->index + 1) % str->size;

	return 0;
}


/*
 *  switch
 */
static int terme_history_save_(Execute ptr, addr pos)
{
	addr array, value;
	struct terme_history_struct *str;

	str = struct_terme_history(pos);
	terme_get(pos, 0, &array);
	Return(terme_data_make_(ptr, &value, 0));
	setarray(array, str->index, value);

	return 0;
}

static int terme_history_get_(Execute ptr, int now, addr *value, int *ret)
{
	addr pos, array;
	struct terme_history_struct *str;

	Return(terme_root_history_(ptr, &pos));
	str = struct_terme_history(pos);
	terme_get(pos, 0, &array);
	if (now < 0 || str->size <= now) {
		*value = Nil;
		return Result(ret, 0);
	}
	now = (str->size + str->index - now) % str->size;
	getarray(array, now, value);

	return Result(ret, 1);
}

static int terme_history_update_(Execute ptr, int now, int *ret)
{
	int check;
	addr pos;

	Return(terme_history_get_(ptr, now, &pos, &check));
	if ((! check) || pos == Nil)
		return Result(ret, 0);
	Return(terme_data_copy_(ptr, pos));

	return Result(ret, 1);
}

int terme_history_select_(Execute ptr, int diff, int *ret)
{
	int check, now;
	addr pos;
	struct terme_history_struct *str;

	Return(terme_root_history_(ptr, &pos));
	str = struct_terme_history(pos);
	if (str->now == 0) {
		Return(terme_history_save_(ptr, pos));
	}
	now = str->now + diff;
	Return(terme_history_update_(ptr, now, &check));
	if (! check)
		return Result(ret, 0);
	str->now = now;

	/* update */
	return Result(ret, 1);
}


/************************************************************
 *  terme_input.c
 ************************************************************/

#ifdef LISP_DEBUG
#define TERME_INPUT_SIZE		3
#else
#define TERME_INPUT_SIZE		4096
#endif
#define TERME_INPUT_UNBYTE		64
#define TERME_INPUT_WAIT		0.01

enum terme_blocking_type {
	terme_blocking_infinite,
	terme_blocking_integer,
	terme_blocking_float
};

union terme_blocking_value {
	int integer_value;
	double float_value;
};

struct terme_blocking {
	enum terme_blocking_type type;
	union terme_blocking_value wait;
};

typedef struct terme_blocking TermeBlocking;

/* buffer */
static byte terme_input_buffer[TERME_INPUT_SIZE];
static size_t terme_input_size;
static size_t terme_input_now;
/* sequence */
static byte terme_input_unbyte[TERME_INPUT_UNBYTE];
static int terme_input_unbyte_size;
static int terme_input_unbyte_now;

void terme_input_init(void)
{
	terme_input_size = 0;
	terme_input_now = 0;
	terme_input_unbyte_size = 0;
	terme_input_unbyte_now = 0;
}

#define TERME_CLEAR_INPUT_STDIN		4096
static int terme_input_clear_stdin(void)
{
	byte data[TERME_CLEAR_INPUT_STDIN];
	int check;
	size_t ignore;

	for (;;) {
		check = terme_arch_select(&check);
		if (check < 0)
			continue;
		if (! check)
			break;

		check = terme_arch_read(data, TERME_CLEAR_INPUT_STDIN, &ignore);
		if (check < 0)
			continue;
		if (check)
			return 1;
	}

	return 0;
}

int terme_input_clear(void)
{
	terme_input_size = 0;
	terme_input_now = 0;
	terme_input_unbyte_size = 0;
	terme_input_unbyte_now = 0;
	return terme_input_clear_stdin();
}


/*
 *  unbyte
 */
static void terme_unbyte_set(byte *data, int size)
{
	Check(TERME_INPUT_UNBYTE <= size, "size error");
	memcpy(terme_input_unbyte, data, (size_t)size);
	terme_input_unbyte_size = size;
	terme_input_unbyte_now = 0;
}

static void terme_unbyte_clear(void)
{
	terme_input_unbyte_size = 0;
	terme_input_unbyte_now = 0;
}

static void terme_unbyte_value(byte c)
{
	terme_input_unbyte[0] = c;
	terme_input_unbyte_size = 1;
	terme_input_unbyte_now = 0;
}

static void terme_unbyte_pop(byte *value, int *ret)
{
	if (terme_input_unbyte_size == 0) {
		*value = 0;
		*ret = 0;
		return;
	}

	*value = terme_input_unbyte[terme_input_unbyte_now++];
	if (terme_input_unbyte_size <= terme_input_unbyte_now) {
		terme_input_unbyte_size = 0;
		terme_input_unbyte_now = 0;
	}
	*ret = 1;
}

static int terme_getc_buffering(void)
{
	int check;
	size_t size;

	terme_input_now = 0;
	check = terme_arch_read(terme_input_buffer, TERME_INPUT_SIZE, &size);
	if (check < 0) {
		terme_input_size = 0;
		return -1;
	}
	if (check) {
		terme_input_size = 0;
		return 1;
	}
	else {
		terme_input_size = size;
		return 0;
	}
}

static int terme_getc_wait(TermeBlocking *blocking, int *ret)
{
	switch (blocking->type) {
		case terme_blocking_integer:
			return terme_arch_wait_integer(ret, blocking->wait.integer_value);

		case terme_blocking_float:
			return terme_arch_wait_float(ret, blocking->wait.float_value);

		case terme_blocking_infinite:
		default:
			*ret = 1;
			return 0;
	}
}

static int terme_getc_blocking(TermeBlocking *blocking, byte *value, int *ret)
{
	int check, readp;

	/* unbyte */
	terme_unbyte_pop(value, &check);
	if (check) {
		*ret = 1;
		return 0;
	}

	/* read */
retry:
	if (terme_input_now < terme_input_size) {
		*ret = 1;
		*value = terme_input_buffer[terme_input_now];
		terme_input_now++;
		return 0;
	}

	/* blocking */
	check = terme_getc_wait(blocking, &readp);
	if (check < 0)
		return -1;
	if (readp == 0) {
		*ret = 0;
		return 0;
	}

	/* buffering */
	check = terme_getc_buffering();
	if (! check)
		goto retry;

	return check;
}

static int terme_getc_escape(byte *value, int *ret)
{
	TermeBlocking wait;

	wait.type = terme_blocking_float;
	wait.wait.float_value = TERME_INPUT_WAIT;
	return terme_getc_blocking(&wait, value, ret);
}


/*
 *  table
 */
#define TERME_INPUT_BUFFER	8
#define TERME_INPUT_ESCAPE	32

struct terme_input_data {
	unsigned error : 1;
	unsigned signal : 1;
	unsigned readp : 1;
	int16_t escape[TERME_INPUT_ESCAPE];
	byte data[TERME_INPUT_BUFFER];
	byte c;
	unsigned index, escape_size;
};
typedef struct terme_input_data TermeInputData;

/*  0x4F 'O'
 *    Up         ^[OA
 *    Down       ^[OB
 *    Right      ^[OC
 *    Left       ^[OD
 *    PF1        ^[OP
 *    PF2        ^[OQ
 *    PF3        ^[OR
 *    PF4        ^[OS
 *  0x5B '[':  \[[value1;value2;value3...X
 *    F1         ^[[11~  ^[OP
 *    F2         ^[[12~  ^[OQ
 *    F3         ^[[13~  ^[OR
 *    F4         ^[[14~  ^[OS
 *    F5         ^[[15~  ^[Ot
 *    F6         ^[[17~  ^[Ou
 *    F7         ^[[18~  ^[Ov
 *    F8         ^[[19~  ^[Ol
 *    F9         ^[[20~  ^[Ow
 *    F10        ^[[21~  ^[Ox
 *    F11        ^[[23~
 *    F12        ^[[24~
 *    Home       ^[[1~
 *    Insert     ^[[2~
 *    End        ^[[4~
 *    Page Up    ^[[5~
 *    Page Down  ^[[6~
 */
static void terme_input_getc_escape(TermeInputData *str)
{
	byte c;
	int check, readp;

	check = terme_getc_escape(&c, &readp);
	if (check < 0) {
		str->signal = 1;
		return;
	}
	if (check) {
		str->error = 1;
		return;
	}
	if (readp == 0) {
		str->readp = 1;
		return;
	}
	str->c = c;
}

static void terme_input_getc(TermeBlocking *blocking, TermeInputData *str)
{
	byte c;
	int check, readp;

	check = terme_getc_blocking(blocking, &c, &readp);
	if (check < 0) {
		str->signal = 1;
		return;
	}
	if (check) {
		str->error = 1;
		return;
	}
	if (readp == 0) {
		str->readp = 1;
		return;
	}
	Check(TERME_INPUT_BUFFER <= str->index, "size error");
	str->data[str->index++] = c;
	str->c = c;
}

#define terme_table_getc(blocking, str, c) { \
	terme_input_getc(blocking, &str); \
	if (str.signal) goto signal; \
	if (str.error) goto error; \
	if (str.readp) goto hang; \
	c = str.c; \
}

static void terme_table_setbyte1(TermeKeyboard *ret,
		TermeInputData *str,
		TermeEscape type)
{
	int16_t value;

	value = str->escape[0];
	if (value < 0)
		value = 1;
	ret->c = (unicode)value;
	ret->type = type;
}

static void terme_table_escape(TermeBlocking *blocking, TermeKeyboard *ret)
{
	byte c;
	char data[16];
	int16_t type;
	int check, datai;
	TermeInputData str;

	str.signal = 0;
	str.error = 0;
	str.readp = 0;
	str.index = 0;
	str.escape_size = 0;
	datai = 0;

	terme_arch_escape_begin();
	terme_input_getc_escape(&str);
	if (str.signal)
		goto signal;
	if (str.error)
		goto error;
	if (str.readp) {
		terme_arch_escape_end(&check);
		if (check)
			goto escape0;
		terme_table_getc(blocking, str, c);
	}
	else {
		c = str.c;
		terme_arch_escape_end(&check);
		if (check)
			goto escape1;
		Check(TERME_INPUT_BUFFER <= str.index, "size error");
		str.data[str.index++] = c;
	}
	if (c == 0x4F) /* O */
		goto third_4F;
	if (c == 0x5B) /* [ */
		goto third_5B;
	goto escape1;

third_4F: /* O */
	terme_table_getc(blocking, str, c);
	if (0x50 <= c && c <= 0x53) /* PF1 - PF4 */
		goto program;
	goto invalid;

third_5B: /* [ */
	if (8 <= datai)
		goto parse_error;
	if (TERME_INPUT_ESCAPE <= str.escape_size)
		goto parse_error;

	/* read char */
	terme_table_getc(blocking, str, c);

	/* digit */
	if (isdigit(c)) {
		data[datai++] = c;
		goto third_5B;
	}

	/* not digit */
	if (datai == 0) {
		check = -1;
	}
	else {
		data[datai++] = 0;
		check = atoi(data);
		datai = 0;
		if (check < 0 || 0xFF <= check)
			goto parse_error;
	}
	str.escape[str.escape_size++] = (int16_t)check;

	/* separator */
	if (c == ';' || c == ':')
		goto third_5B;

	/* operator */
	if (c == 0x7E) /* ~ */
		goto forth_7E;
	if (c == 0x41) /* A */
		goto escape_up;
	if (c == 0x42) /* B */
		goto escape_down;
	if (c == 0x43) /* C */
		goto escape_right;
	if (c == 0x44) /* D */
		goto escape_left;
	goto invalid;

forth_7E:
	type = str.escape[0];
	if (type < 0)
		type = 0;
	if (type == 1)
		goto escape_home;
	if (type == 2)
		goto escape_insert;
	if (type == 4)
		goto escape_end;
	if (type == 5)
		goto escape_page_up;
	if (type == 6)
		goto escape_page_down;
	if (11 <= type && type <= 19)
		goto function1;
	goto invalid;

escape_up:
	terme_table_setbyte1(ret, &str, terme_escape_up);
	goto finish;

escape_down:
	terme_table_setbyte1(ret, &str, terme_escape_down);
	goto finish;

escape_right:
	terme_table_setbyte1(ret, &str, terme_escape_right);
	goto finish;

escape_left:
	terme_table_setbyte1(ret, &str, terme_escape_left);
	goto finish;

escape_home:
	ret->type = terme_escape_home;
	goto finish;

escape_insert:
	ret->type = terme_escape_insert;
	goto finish;

escape_end:
	ret->type = terme_escape_end;
	goto finish;

escape_page_up:
	ret->type = terme_escape_page_up;
	goto finish;

escape_page_down:
	ret->type = terme_escape_page_down;
	goto finish;

program:
	ret->type = terme_escape_function;
	ret->c = (c - 0x50) + 1; /* PF1 -> 1 */
	goto finish;

function1:
	ret->type = terme_escape_function;
	ret->c = (type - 10); /* F1 -> 1 */
	goto finish;

escape0:
	ret->type = terme_escape_escape;
	terme_unbyte_clear();
	return;

escape1:
	ret->type = terme_escape_escape;
	terme_unbyte_clear();
	terme_unbyte_value(c);
	return;

invalid:
	terme_unbyte_clear();
	ret->type = terme_escape_ignore;
	return;

finish:
	terme_unbyte_clear();
	return;

signal:
	terme_unbyte_set(str.data, str.index);
	ret->type = terme_escape_signal;
	return;

error:
	terme_unbyte_set(str.data, str.index);
	ret->type = terme_escape_error;
	return;

hang:
	terme_unbyte_set(str.data, str.index);
	ret->type = terme_escape_hang;
	return;

parse_error:
	terme_unbyte_clear();
	return;
}


/*
 *  UTF-8 table
 */
static int terme_table_utf8(TermeBlocking *blocking, unicode *value, int *ret)
{
	byte c;
	unicode merge;
	TermeInputData str;

	str.signal = 0;
	str.error = 0;
	str.readp = 0;
	str.index = 0;

	terme_table_getc(blocking, str, c);
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
	merge = (unicode)c;
	goto normal;

sequence2:
	merge = (0x1F & c) << 6;
	terme_table_getc(blocking, str, c);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= 0x3F & c;
	if (merge < 0x80)
		goto invalid;
	goto normal;

sequence3:
	merge = (0x0F & c) << 12;
	terme_table_getc(blocking, str, c);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= (0x3F & c) << 6;
	terme_table_getc(blocking, str, c);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= 0x3F & c;
	if (merge < 0x0800)
		goto invalid;
	if (isSurrogatePair(merge))
		goto invalid;
	goto normal;

sequence4:
	merge = (0x07 & c) << 18;
	terme_table_getc(blocking, str, c);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= (0x3F & c) << 12;
	terme_table_getc(blocking, str, c);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= (0x3F & c) << 6;
	terme_table_getc(blocking, str, c);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= 0x3F & c;
	if (merge < 0x010000)
		goto invalid;
	if (UnicodeCount <= merge)
		goto size_error;
	goto normal;

normal:
	terme_unbyte_clear();
	*value = merge;
	*ret = 1;
	return 0;

invalid:
	terme_unbyte_value(c);
	return 1;

size_error:
	terme_unbyte_clear();
	return 1;

signal:
	terme_unbyte_set(str.data, str.index);
	return -1;

hang:
	terme_unbyte_set(str.data, str.index);
	*ret = 0;
	return 0;

error:
	terme_unbyte_set(str.data, str.index);
	return 1;
}

static void terme_table_wait(TermeBlocking *blocking, TermeKeyboard *ret)
{
	int check, readp;
	unicode c;

	terme_input_unbyte_now = 0;
	check = terme_table_utf8(blocking, &c, &readp);
	if (check < 0) {
		ret->type = terme_escape_signal;
		ret->c = 0;
		return;
	}
	if (check) {
		ret->type = terme_escape_error;
		ret->c = 0;
		return;
	}
	if (! readp) {
		ret->type = terme_escape_hang;
		ret->c = 0;
		return;
	}
	if (c != 0x1B) {
		ret->type = terme_escape_code;
		ret->c = c;
		return;
	}

	/* escape sequence */
	terme_input_unbyte_now = 0;
	terme_table_escape(blocking, ret);
}

void terme_table_infinite(TermeKeyboard *ret)
{
	TermeBlocking blocking;
	blocking.type = terme_blocking_infinite;
	terme_table_wait(&blocking, ret);
}


/*
 *  values
 */
static void terme_input_value(TermeBlocking *blocking, addr *rtype, addr *rvalue)
{
	TermeKeyboard v;

	terme_table_wait(blocking, &v);
	*rvalue = Nil;
	switch (v.type) {
		case terme_escape_hang:
			GetConst(SYSTEM_TERME_HANG, rtype);
			break;

		case terme_escape_code:
			GetConst(SYSTEM_TERME_CODE, rtype);
			fixnum_heap(rvalue, (fixnum)v.c);
			break;

		case terme_escape_up:
			GetConst(SYSTEM_TERME_UP, rtype);
			fixnum_heap(rvalue, (fixnum)v.c);
			break;

		case terme_escape_down:
			GetConst(SYSTEM_TERME_DOWN, rtype);
			fixnum_heap(rvalue, (fixnum)v.c);
			break;

		case terme_escape_left:
			GetConst(SYSTEM_TERME_LEFT, rtype);
			fixnum_heap(rvalue, (fixnum)v.c);
			break;

		case terme_escape_right:
			GetConst(SYSTEM_TERME_RIGHT, rtype);
			fixnum_heap(rvalue, (fixnum)v.c);
			break;

		case terme_escape_page_up:
			GetConst(SYSTEM_TERME_PAGE_UP, rtype);
			break;

		case terme_escape_page_down:
			GetConst(SYSTEM_TERME_PAGE_DOWN, rtype);
			break;

		case terme_escape_home:
			GetConst(SYSTEM_TERME_HOME, rtype);
			break;

		case terme_escape_end:
			GetConst(SYSTEM_TERME_END, rtype);
			break;

		case terme_escape_insert:
			GetConst(SYSTEM_TERME_INSERT, rtype);
			break;

		case terme_escape_function:
			GetConst(SYSTEM_TERME_FUNCTION, rtype);
			fixnum_heap(rvalue, (fixnum)v.c);
			break;

		case terme_escape_signal:
			GetConst(SYSTEM_TERME_SIGNAL, rtype);
			break;

		case terme_escape_escape:
			GetConst(SYSTEM_TERME_ESCAPE, rtype);
			break;

		default:
			*rtype = Nil;
			break;
	}
}

void terme_input_infinite(addr *rtype, addr *rvalue)
{
	TermeBlocking blocking;
	blocking.type = terme_blocking_infinite;
	terme_input_value(&blocking, rtype, rvalue);
}

void terme_input_integer(int wait, addr *rtype, addr *rvalue)
{
	TermeBlocking blocking;

	if (wait < 0)
		wait = 0;
	blocking.type = terme_blocking_integer;
	blocking.wait.integer_value = wait;
	terme_input_value(&blocking, rtype, rvalue);
}

void terme_input_float(double wait, addr *rtype, addr *rvalue)
{
	TermeBlocking blocking;

	if (wait < 0.0)
		wait = 0;
	blocking.type = terme_blocking_float;
	blocking.wait.float_value = wait;
	terme_input_value(&blocking, rtype, rvalue);
}


/************************************************************
 *  terme_object.c
 ************************************************************/

byte *terme_pointer(addr pos)
{
	CheckType(pos, LISPSYSTEM_TERME);
	posbody(pos, &pos);
	return (byte *)pos;
}

void terme_get(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_TERME);
	getarray(pos, index, ret);
}

void terme_set(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_TERME);
	setarray(pos, index, value);
}

enum terme_type terme_get_type(addr pos)
{
	CheckType(pos, LISPSYSTEM_TERME);
	return (enum terme_type)GetUser(pos);
}

void terme_set_type(addr pos, enum terme_type type)
{
	CheckType(pos, LISPSYSTEM_TERME);
	SetUser(pos, (int)type);
}

int termep(addr pos)
{
	return GetType(pos) == LISPSYSTEM_TERME;
}

int terme_root_p(addr pos)
{
	return termep(pos) && (terme_get_type(pos) == terme_type_root);
}

int terme_data_p(addr pos)
{
	return termep(pos) && (terme_get_type(pos) == terme_type_data);
}

int terme_string_p(addr pos)
{
	return termep(pos) && (terme_get_type(pos) == terme_type_string);
}

int terme_screen_p(addr pos)
{
	return termep(pos) && (terme_get_type(pos) == terme_type_screen);
}

int terme_display_p(addr pos)
{
	return termep(pos) && (terme_get_type(pos) == terme_type_display);
}

int terme_line_p(addr pos)
{
	return termep(pos) && (terme_get_type(pos) == terme_type_line);
}

int terme_history_p(addr pos)
{
	return termep(pos) && (terme_get_type(pos) == terme_type_history);
}


/*
 *  terme-root
 */
struct terme_root_struct {
	enum prompt_mode mode;
};

static struct terme_root_struct *terme_root_body(addr pos)
{
	Check(! terme_root_p(pos), "type error");
	return (struct terme_root_struct *)terme_pointer(pos);
}

void terme_root_build(addr *ret)
{
	addr root;
	struct terme_root_struct *str;

	heap_smallsize(&root, LISPSYSTEM_TERME,
			terme_root_size,
			sizeoft(struct terme_root_struct));
	terme_set_type(root, terme_type_root);
	str = terme_root_body(root);
	str->mode = prompt_input;
	*ret = root;
}

static int terme_root_special_(Execute ptr, addr *ret)
{
	addr symbol;
	GetConst(SYSTEM_SPECIAL_TERME, &symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

static int terme_root_get_(Execute ptr, enum terme_root_index index, addr *ret)
{
	addr pos;

	Return(terme_root_special_(ptr, &pos));
	terme_get(pos, (size_t)index, ret);

	return 0;
}

int terme_root_data_(Execute ptr, addr *ret)
{
	return terme_root_get_(ptr, terme_root_data, ret);
}

int terme_root_screen_(Execute ptr, addr *ret)
{
	return terme_root_get_(ptr, terme_root_screen, ret);
}

int terme_root_display_(Execute ptr, addr *ret)
{
	return terme_root_get_(ptr, terme_root_display, ret);
}

int terme_root_history_(Execute ptr, addr *ret)
{
	return terme_root_get_(ptr, terme_root_history, ret);
}


/*
 *  prompt
 */
int terme_prompt_set_(Execute ptr, addr value, enum prompt_mode mode)
{
	addr pos;
	struct terme_root_struct *str;

	Return(terme_root_special_(ptr, &pos));
	terme_set(pos, terme_root_prompt, value);
	str = terme_root_body(pos);
	str->mode = mode;

	return 0;
}

int terme_prompt_get_(Execute ptr, addr *value, enum prompt_mode *mode)
{
	addr pos;
	struct terme_root_struct *str;

	Return(terme_root_special_(ptr, &pos));
	if (value) {
		terme_get(pos, terme_root_prompt, value);
	}
	if (mode) {
		str = terme_root_body(pos);
		*mode = str->mode;
	}

	return 0;
}


/************************************************************
 *  terme_output.c
 ************************************************************/

#ifdef LISP_DEBUG
#define TERME_OUTPUT_SIZE	3
#else
#define TERME_OUTPUT_SIZE	4096
#endif

static byte terme_output_buffer[TERME_OUTPUT_SIZE];
static size_t terme_output_size;
static size_t terme_output_x;

void terme_output_init(void)
{
	terme_output_size = 0;
	terme_output_x = 0;
}

int terme_finish_output(void)
{
	int check;
	byte *data;
	size_t size, retu;

	size = terme_output_size;
	data = terme_output_buffer;
	while (size) {
		check = terme_arch_write(data, size, &retu);
		if (check)
			return 1; /* error */
		if (size <= retu)
			break;
		size -= retu;
		data += retu;
	}
	terme_output_size = 0;

	return 0;
}

void terme_clear_output(void)
{
	terme_output_size = 0;
	terme_output_x = 0;
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

int terme_write_char(unicode c, unsigned width)
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

int terme_output_char(unicode c)
{
	byte data[8];
	size_t size;

	if (terme_write_utf8(c, data, &size)) {
		/* encode error */
		return 0;
	}

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
 *  terme_prompt.c
 ************************************************************/

/*
 *  default
 */
static int readline_default_terme_newline_(addr pos, addr *ret)
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

static int readline_default_terme_(Execute ptr, addr *ret)
{
	int check;
	addr input, output, prompt, pos;

	GetConst(STREAM_STDIN, &input);
	GetConst(STREAM_STDOUT, &output);
	Return(terme_prompt_get_(ptr, &prompt, NULL));
	Return(fresh_line_stream_(output, NULL));
	if (prompt != Nil) {
		Return(princ_print_(ptr, output, prompt));
	}
	Return(finish_output_stream_(output));
	Return(clear_input_stream_(input));
	Return(read_line_stream_(ptr, &pos, &check, input, 0, Nil, 0));
	if (pos == Nil)
		return Result(ret, Nil);
	Return(readline_default_terme_newline_(pos, &pos));
	return Result(ret, pos);
}


/*
 *  Ctrl + C
 *    continue restart
 *    sigint restart
 */
static void terme_readline_sigint_continue(addr *ret)
{
	static const char *message = "Continue Ctrl + C.";
	addr restart, pos;

	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	setinteractive_restart(restart, Nil);
	strvect_char_heap(&pos, message);
	setreport_restart(restart, pos);
	settest_restart(restart, Nil);
	setescape_restart(restart, 1);
	*ret = restart;
}

static int terme_readline_sigint_kill_(addr *ret)
{
	static const char *message = "Kill SIGINT.";
	enum PACKAGE_TYPE ignore;
	addr restart, pos;

	Return(internchar_(LISP_SYSTEM, "SIGINT", &pos, &ignore));
	restart_heap(&restart, pos);
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	setinteractive_restart(restart, Nil);
	strvect_char_heap(&pos, message);
	setreport_restart(restart, pos);
	settest_restart(restart, Nil);
	setescape_restart(restart, 1);

	return Result(ret, restart);
}

static int terme_readline_sigint_(TermeKeyboard *str)
{
	int mode;

	if (terme_arch_textmode(&mode))
		return terme_fmte_("terme_arch_textmode error.", NULL);
	if (terme_arch_terminal_sigint_())
		return terme_fmte_("kill error.", NULL);
	if (mode && terme_arch_rawmode(NULL))
		return terme_fmte_("terme_arch_rawmode error.", NULL);

	/* ignore */
	str->type = terme_escape_ignore;
	return 0;
}

static int terme_readline_ctrl_c_(Execute ptr, TermeKeyboard *str)
{
	addr restart1, restart2, control;

	terme_readline_sigint_continue(&restart1);
	Return(terme_readline_sigint_kill_(&restart2));

	push_control(ptr, &control);
	pushrestart_control(ptr, restart2);
	pushrestart_control(ptr, restart1);
	(void)terme_fmte_("Ctrl + C", NULL);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* continue */
	if (ptr->throw_handler == restart1) {
		normal_throw_control(ptr);
		str->type = terme_escape_ignore;
		goto escape;
	}

	/* kill */
	if (ptr->throw_handler == restart2) {
		normal_throw_control(ptr);
		Return(terme_readline_sigint_(str));
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}


/*
 *  readline
 */
static int terme_readline_loop_(Execute ptr, TermeKeyboard *, addr *, int *);

static int terme_readline_signal_(Execute ptr)
{
	int check;

	check = 0;
	Return(terme_arch_signal_p_(&check));
	if (check)
		return terme_screen_update_(ptr);

	return 0;
}

static int terme_readline_hang_(Execute pt, addr *value, int *ret)
{
	*value = 0;
	return Result(ret, 1);
}

static int terme_readline_ctrl_z_(TermeKeyboard *str)
{
	int mode;

	if (terme_arch_textmode(&mode))
		return terme_fmte_("terme_arch_textmode error.", NULL);
	if (terme_arch_terminal_stop_())
		return terme_fmte_("kill error.", NULL);
	if (mode && terme_arch_rawmode(NULL))
		return terme_fmte_("terme_arch_rawmode error.", NULL);

	/* ignore */
	str->type = terme_escape_ignore;
	return 0;
}

static int terme_readline_control_(Execute ptr,
		TermeKeyboard *str, addr *value, int *ret)
{
	switch (str->c) {
		case 0x03:	/* C */
			Return(terme_readline_ctrl_c_(ptr, str));
			break;

		case 0x04:  /* D */
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

		case 0x09:  /* I, tabular */
			str->type = terme_escape_tabular;
			break;

		case 0x12:  /* R, search */
			str->type = terme_escape_search;
			break;

#ifdef LISP_DEBUG
		case 0x1D:  /* ], refresh */
			str->type = terme_escape_refresh;
			break;
#endif

		case 0x1A:  /* Z */
			Return(terme_readline_ctrl_z_(str));
			break;

		case 0x7F:  /* DEL, backspace */
			str->type = terme_escape_backspace;
			break;

		default:
			str->type = terme_escape_ignore;
			break;
	}

	return terme_readline_loop_(ptr, str, value, ret);
}

static int terme_readline_character_(Execute ptr, unicode c, addr *value, int *ret)
{
	int check;
	unsigned width;

	/* value */
	Return(terme_data_insert_(ptr, c, &width, &check));
	if (check)  /* buffer overflow */
		return Result(ret, 0);

	/* screen */
	Return(terme_screen_push_(ptr));

	/* next */
	return terme_data_next_(ptr);
}

static int terme_readline_code_(Execute ptr, TermeKeyboard *str, addr *value, int *ret)
{
	if (str->c < 0x20 || str->c == 0x7F)
		return terme_readline_control_(ptr, str, value, ret);
	else
		return terme_readline_character_(ptr, str->c, value, ret);
}

static int terme_readline_up_down_(Execute ptr, int diff)
{
	int check;

	/* select */
	Return(terme_history_select_(ptr, diff, &check));
	if (! check)
		return 0;

	/* screen */
	Return(terme_screen_history_(ptr));
	/* data */
	Return(terme_data_last_(ptr));

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
	unsigned width;

	Return(terme_data_left_(ptr, &width));
	if (width == 0)
		return 0;

	return terme_screen_left_(ptr, width);
}

static int terme_readline_right_(Execute ptr)
{
	unsigned width;

	Return(terme_data_right_(ptr, &width));
	if (width == 0)
		return 0;

	return terme_screen_right_(ptr, width);
}

static int terme_readline_return_(Execute ptr, addr *value, int *ret)
{
	/* last */
	Return(terme_screen_last_(ptr));
	Return(terme_data_last_(ptr));
	/* history */
	Return(terme_history_return_(ptr));
	/* result */
	Return(terme_data_make_(ptr, value, 1));
	return Result(ret, 1);
}

static int terme_readline_delete_(Execute ptr, addr *value, int *ret)
{
	unsigned size;
	int check;

	/* exit */
	Return(terme_data_size_(ptr, &size));
	if (size == 0) {
		*value = Nil;
		return Result(ret, 1);
	}

	/* delete */
	*value = Nil;
	*ret = 0;
	Return(terme_data_delete_(ptr, &check));
	if (! check)
		return 0;

	return terme_screen_delete_(ptr);
}

static int terme_readline_backspace_(Execute ptr)
{
	unsigned width;

	Return(terme_data_backspace_(ptr, &width));
	if (width == 0)
		return 0;

	return terme_screen_backspace_(ptr, width);
}

static int terme_readline_first_(Execute ptr)
{
	Return(terme_data_first_(ptr));
	return terme_screen_first_(ptr);
}

static int terme_readline_last_(Execute ptr)
{
	Return(terme_screen_last_(ptr));
	return terme_data_last_(ptr);
}

static int terme_readline_update_(Execute ptr)
{
	return terme_screen_update_(ptr);
}

static int terme_readline_rmleft_(Execute ptr)
{
	int check;

	Return(terme_data_rmleft_(ptr, &check));
	if (! check)
		return 0;

	return terme_screen_rmleft_(ptr);
}

static int terme_readline_rmright_(Execute ptr)
{
	int check;

	Return(terme_data_rmright_(ptr, &check));
	if (! check)
		return 0;

	return terme_screen_rmright_(ptr);
}

static int terme_readline_loop_(Execute ptr, TermeKeyboard *str, addr *value, int *ret)
{
	*ret = 0;
	switch (str->type) {
		case terme_escape_error:
			return fmte_("readline error", NULL);

		case terme_escape_ignore:
			break;

		case terme_escape_signal:
			return terme_readline_signal_(ptr);

		case terme_escape_hang:
			return terme_readline_hang_(ptr, value, ret);

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
			return terme_readline_return_(ptr, value, ret);

		case terme_escape_delete:
			return terme_readline_delete_(ptr, value, ret);

		case terme_escape_backspace:
			return terme_readline_backspace_(ptr);

		case terme_escape_first:
			return terme_readline_first_(ptr);

		case terme_escape_last:
			return terme_readline_last_(ptr);

		case terme_escape_update:
			return terme_readline_update_(ptr);

#ifdef LISP_DEBUG
		case terme_escape_refresh:
			return terme_screen_refresh_(ptr);
#endif

		case terme_escape_rmleft:
			return terme_readline_rmleft_(ptr);

		case terme_escape_rmright:
			return terme_readline_rmright_(ptr);

		case terme_escape_home:
		case terme_escape_end:
		case terme_escape_insert:
		case terme_escape_page_up:
		case terme_escape_page_down:
		case terme_escape_function:
		case terme_escape_tabular:
		case terme_escape_search:
		case terme_escape_escape:
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

	/* begin */
	Return(terme_screen_prompt_(ptr));

	/* loop */
	pos = Nil;
	for (;;) {
		terme_table_infinite(&str);
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

static int terme_readline_module_(Execute ptr, addr *ret)
{
	int check, mode;

	Return(terme_data_clear_(ptr));
	Return(terme_screen_clear_(ptr));
	Return(terme_display_clear_(ptr));
	Return(terme_history_clear_(ptr));

	/* readline */
	if (terme_arch_rawmode(&mode)) {
		*ret = Nil;
		return terme_fmte_("terme_arch_rawmode error.", NULL);
	}
	check = terme_readline_call_(ptr, ret);
	if (mode && terme_arch_textmode(NULL)) {
		*ret = Nil;
		return terme_fmte_("terme_arch_textmode error.", NULL);
	}

	return check;
}

int terme_readline_(Execute ptr, addr *ret)
{
	if (terme_arch_enable())
		return terme_readline_module_(ptr, ret);
	else
		return readline_default_terme_(ptr, ret);
}


/************************************************************
 *  terme_screen.c
 ************************************************************/

/*
 *  object
 */
struct terme_screen_struct {
	unsigned window_x, window_y;
	unsigned prompt_x, prompt_y;
	unsigned now_x, now_y, last_y;
};

static struct terme_screen_struct *struct_terme_screen(addr pos)
{
	Check(! terme_screen_p(pos), "type error");
	return (struct terme_screen_struct *)terme_pointer(pos);
}

void terme_screen_build(addr *ret)
{
	addr pos;
	struct terme_screen_struct *str;

	heap_body(&pos, LISPSYSTEM_TERME, sizeoft(struct terme_screen_struct));
	terme_set_type(pos, terme_type_screen);
	str = struct_terme_screen(pos);
	str->window_x = 0;
	str->window_y = 0;
	str->prompt_x = 0;
	str->prompt_y = 0;
	str->now_x = 0;
	str->now_y = 0;
	str->last_y = 0;
	*ret = pos;
}


/*
 *  clear
 */
int terme_screen_clear_(Execute ptr)
{
	unsigned screen_x, screen_y;
	addr pos;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &pos));
	terme_arch_size_get(&screen_x, &screen_y);
	str = struct_terme_screen(pos);
	str->window_x = screen_x;
	str->window_y = screen_y;
	str->prompt_x = 0;
	str->prompt_y = 0;
	str->now_x = 0;
	str->now_y = 0;
	str->last_y = 0;

	return 0;
}


/*
 *  prompt
 */
static int terme_screen_write_char_(Execute ptr,
		unicode c, unsigned width, PromptMode mode)
{
	int output_p;
	unsigned next;
	addr pos;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &pos));
	str = struct_terme_screen(pos);
	output_p = (str->now_y < str->window_y);

	/* eol */
	next = str->now_x + width;
	if (str->window_x < next) {
		if (output_p) {
			Return(terme_write_terpri_(ptr));
			Return(terme_write_delete_line_right_(ptr));
		}
		str->now_x = 0;
		str->now_y++;
	}

	/* output */
	if (output_p) {
		Return(terme_write_char_(ptr, c, width, mode));
	}
	str->now_x++;

	/* width */
	if (1 < width) {
		if (output_p) {
			Return(terme_write_right_(ptr, mode));
		}
		str->now_x++;
	}

	return 0;
}

static int terme_screen_prompt_string_(Execute ptr, addr pos, PromptMode mode)
{
	unsigned width;
	unicode c;
	size_t size, i;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		width = eastasian_width(c);
		Return(terme_screen_write_char_(ptr, c, width, mode));
	}

	return 0;
}

static PrintColor terme_prompt_color_bright(PromptMode mode)
{
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
}

static PrintColor terme_prompt_color_dark(PromptMode mode)
{
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

static PrintColor terme_prompt_color(Execute ptr, PromptMode mode)
{
	addr pos;

	GetConst(SYSTEM_PROMPT_BRIGHT, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Nil)
		return terme_prompt_color_dark(mode);

	/* unbound or (not nil) */
	return terme_prompt_color_bright(mode);
}

static int terme_screen_prompt_output_(Execute ptr)
{
	PromptMode mode;
	PrintColor color;
	addr pos;

	/* prompt */
	Return(terme_prompt_get_(ptr, &pos, &mode));
	if (pos == Nil)
		return 0;

	/* color */
	if (terme_font(ptr, print_font_reset))
		return terme_fmte_("terme_font error.", NULL);
	color = terme_prompt_color(ptr, mode);
	if (terme_text_color(ptr, color))
		return terme_fmte_("terme_text_color error.", NULL);

	/* output */
	Return(terme_screen_prompt_string_(ptr, pos, mode));

	/* reset */
	if (terme_font(ptr, print_font_reset))
		return terme_fmte_("terme_font error.", NULL);

	return 0;
}

static int terme_screen_prompt_position_(Execute ptr)
{
	addr pos;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &pos));
	str = struct_terme_screen(pos);
	str->prompt_x = str->now_x;
	str->prompt_y = str->now_y;

	return 0;
}

int terme_screen_prompt_(Execute ptr)
{
	addr pos;

	/* fresh-line */
	Return(terminal_io_stream_(ptr, &pos));
	Return(fresh_line_stream_(pos, NULL));

	/* screen */
	Return(terme_screen_prompt_output_(ptr));
	Return(terme_screen_prompt_position_(ptr));
	return terme_write_flush_();
}


/*
 *  push
 */
static int terme_screen_push_next_(addr data, addr screen, unsigned *rx, unsigned *ry)
{
	unsigned now, width, next, x, y;
	struct terme_screen_struct *str;

	/* width */
	terme_data_get_value(data, &now, NULL);
	Return(terme_data_get_character_(data, now, NULL, &width));

	/* screen */
	str = struct_terme_screen(screen);
	x = str->now_x;
	y = str->now_y;
	next = x + width;
	if (str->window_x < next) {
		x = 0;
		y++;
	}
	x += width;

	/* result */
	*rx = x;
	*ry = y;
	return 0;
}

static int terme_screen_push_first_(Execute ptr, addr screen)
{
	struct terme_screen_struct *str;

	str = struct_terme_screen(screen);
	if (str->now_x < str->window_x) {
		Return(terme_write_delete_line_right_(ptr));
	}

	return 0;
}

static int terme_screen_output_(Execute ptr, addr data, addr screen, PromptMode mode)
{
	unsigned now, size, width;
	unicode c;
	struct terme_screen_struct *str;

	str = struct_terme_screen(screen);
	terme_data_get_value(data, &now, &size);
	for (; now < size; now++) {
		Return(terme_data_get_character_(data, now, &c, &width));
		Return(terme_screen_write_char_(ptr, c, width, mode));
	}
	if (str->last_y < str->now_y)
		str->last_y = str->now_y;

	return 0;
}

static int terme_screen_move_(Execute ptr, unsigned x, unsigned y)
{
	addr screen;
	unsigned now_x, now_y;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	now_x = str->now_x;
	now_y = str->now_y;

	/* up */
	if (y < now_y) {
		Return(terme_write_up_(ptr, now_y - y));
	}

	/* down */
	if (y > now_y) {
		Return(terme_write_down_(ptr, y - now_y));
	}

	/* left */
	if (x < now_x) {
		Return(terme_write_left_(ptr, now_x - x));
	}

	/* right */
	if (x > now_x) {
		Return(terme_write_right_(ptr, x - now_x));
	}

	/* result */
	str->now_x = x;
	str->now_y = y;
	return 0;
}

int terme_screen_push_(Execute ptr)
{
	unsigned x, y;
	addr data, screen;

	Return(terme_root_data_(ptr, &data));
	Return(terme_root_screen_(ptr, &screen));

	/* next position */
	x = y = 0;
	Return(terme_screen_push_next_(data, screen, &x, &y));
	/* line delete */
	Return(terme_screen_push_first_(ptr, screen));
	/* output */
	Return(terme_screen_output_(ptr, data, screen, prompt_input));
	/* move cursor */
	Return(terme_screen_move_(ptr, x, y));

	return terme_write_flush_();
}


/*
 *  history
 */
static int terme_screen_history_delete_(Execute ptr, addr screen)
{
	unsigned y, now_x, now_y;
	struct terme_screen_struct *str;

	str = struct_terme_screen(screen);
	if (str->last_y <= str->now_y)
		return 0;
	now_x = str->now_x;
	now_y = str->now_y;
	str->now_x = 0;
	for (y = str->now_y; y < str->last_y; y++) {
		Return(terme_write_first_down_(ptr, 1));
		Return(terme_write_delete_line_(ptr));
		str->now_y++;
	}

	return terme_screen_move_(ptr, now_x, now_y);
}

int terme_screen_history_(Execute ptr)
{
	addr data, screen;

	Return(terme_root_data_(ptr, &data));
	Return(terme_root_screen_(ptr, &screen));
	/* move (0, 0) */
	Return(terme_screen_move_(ptr, 0, 0));
	/* output prompt */
	Return(terme_write_delete_line_(ptr));
	Return(terme_screen_prompt_output_(ptr));
	/* output data */
	Return(terme_screen_output_(ptr, data, screen, prompt_input));
	/* delete last line */
	Return(terme_screen_history_delete_(ptr, screen));
	/* flush */
	return terme_write_flush_();
}


/*
 *  left, right
 */
static int terme_screen_left_line_(Execute ptr, unsigned width)
{
	addr screen;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	Check(str->now_x < width, "width error.");
	Return(terme_write_left_(ptr, width));
	str->now_x -= width;

	return 0;
}

static int terme_screen_left_previous_(Execute ptr, unsigned width)
{
	unsigned move;
	addr screen;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	if (str->now_y == 0)
		return 0;
	/* first up */
	Return(terme_write_first_up_(ptr, 1));
	str->now_x = 0;
	str->now_y--;
	/* right */
	Return(terme_display_getlast_(ptr, &move));
	if (width < move) {
		move -= width;
		Return(terme_write_right_(ptr, move));
		str->now_x = move;
	}
	/* left */
	return terme_write_flush_();
}

static int terme_screen_left_output_(Execute ptr, unsigned width, int *ret)
{
	int check;

	Return(terme_display_previous_(ptr, &check));
	if (check < 0)
		return Result(ret, 0);
	if (check) {
		Return(terme_screen_left_line_(ptr, width));
	}
	else {
		Return(terme_screen_left_previous_(ptr, width));
	}

	return Result(ret, 1);
}

int terme_screen_left_(Execute ptr, unsigned width)
{
	int check;

	Return(terme_screen_left_output_(ptr, width, &check));
	if (! check)
		return 0;

	return terme_write_flush_();
}

static int terme_screen_right_next_(Execute ptr)
{
	unsigned now, size, width, next;
	addr data, screen;
	struct terme_screen_struct *str;

	/* data */
	Return(terme_root_data_(ptr, &data));
	terme_data_get_value(data, &now, &size);
	if (size <= now)
		return terme_write_flush_();
	Return(terme_data_get_character_(data, now, NULL, &width));

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	next = str->now_x + width;
	if (str->window_x < next) {
		Return(terme_write_first_down_(ptr, 1));
		str->now_x = 0;
		str->now_y++;
	}

	return terme_write_flush_();
}

int terme_screen_right_(Execute ptr, unsigned width)
{
	unsigned next;
	addr screen;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	next = str->now_x + width;
	if (str->window_x <= next) {
		Return(terme_write_first_down_(ptr, 1));
		str->now_x = 0;
		str->now_y++;
	}
	else {
		Return(terme_write_right_(ptr, width));
		str->now_x += width;
	}

	return terme_screen_right_next_(ptr);
}

static int terme_screen_delete_last_(Execute ptr, addr screen)
{
	unsigned y, now_y, last_y;
	struct terme_screen_struct *str;

	str = struct_terme_screen(screen);
	now_y = str->now_y;
	last_y = str->last_y;
	for (y = now_y; y < last_y; y++) {
		Return(terme_write_first_down_(ptr, 1));
		str->now_x = 0;
		str->now_y++;
		Return(terme_write_delete_line_right_(ptr));
	}

	return 0;
}

int terme_screen_delete_(Execute ptr)
{
	unsigned now_x, now_y;
	addr data, screen;
	struct terme_screen_struct *str;

	Return(terme_root_data_(ptr, &data));
	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	now_x = str->now_x;
	now_y = str->now_y;

	/* line delete */
	Return(terme_screen_push_first_(ptr, screen));
	/* output */
	Return(terme_screen_output_(ptr, data, screen, prompt_input));
	/* delete last line */
	Return(terme_screen_delete_last_(ptr, screen));
	/* move */
	Return(terme_screen_move_(ptr, now_x, now_y));
	/* flush */
	return terme_write_flush_();
}

int terme_screen_backspace_(Execute ptr, unsigned width)
{
	int check;

	/* left */
	Return(terme_screen_left_output_(ptr, width, &check));
	if (! check)
		return 0;

	/* delete */
	return terme_screen_delete_(ptr);
}

int terme_screen_first_(Execute ptr)
{
	addr screen;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	Return(terme_screen_move_(ptr, str->prompt_x, str->prompt_y));
	return terme_write_flush_();
}

static int terme_screen_move_char_(addr screen,
		unsigned width, unsigned *rx, unsigned *ry)
{
	unsigned next;
	struct terme_screen_struct *str;

	str = struct_terme_screen(screen);
	if (str->window_y <= *ry)
		return 0;

	/* eol */
	next = *rx + width;
	if (str->window_x < next) {
		*rx = 0;
		(*ry)++;
	}

	/* move */
	*rx += width;

	return 0;
}

static int terme_screen_last_position_(Execute ptr, unsigned *rx, unsigned *ry)
{
	unsigned now, size, width, x, y;
	addr screen, data;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	Return(terme_root_data_(ptr, &data));
	str = struct_terme_screen(screen);
	terme_data_get_value(data, &now, &size);
	x = str->now_x;
	y = str->now_y;
	for (; now < size; now++) {
		Return(terme_data_get_character_(data, now, NULL, &width));
		Return(terme_screen_move_char_(screen, width, &x, &y));
	}
	*rx = x;
	*ry = y;

	return 0;
}

int terme_screen_last_(Execute ptr)
{
	unsigned x, y;

	x = y = 0;
	Return(terme_screen_last_position_(ptr, &x, &y));
	Return(terme_screen_move_(ptr, x, y));
	return terme_write_flush_();
}

static int terme_screen_update_output_(Execute ptr, addr screen, PromptMode mode)
{
	unsigned now, size, width;
	unicode c;
	addr data;
	struct terme_screen_struct *str;

	Return(terme_root_data_(ptr, &data));
	str = struct_terme_screen(screen);
	terme_data_get_value(data, NULL, &size);
	for (now = 0; now < size; now++) {
		Return(terme_data_get_character_(data, now, &c, &width));
		Return(terme_screen_write_char_(ptr, c, width, mode));
	}
	if (str->last_y < str->now_y)
		str->last_y = str->now_y;

	return 0;
}

int terme_screen_update_(Execute ptr)
{
	unsigned x, y;
	addr screen;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	x = str->now_x;
	y = str->now_y;
	str->now_x = 0;
	str->now_y = 0;
	Return(terme_write_delete_page_(ptr));
	Return(terme_screen_prompt_output_(ptr));
	Return(terme_screen_update_output_(ptr, screen, prompt_input));
	Return(terme_screen_move_(ptr, x, y));
	return terme_write_flush_();
}

int terme_screen_refresh_(Execute ptr)
{
	unsigned x, y;
	addr screen;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	x = str->now_x;
	y = str->now_y;
	str->now_x = 0;
	str->now_y = 0;
	Return(terme_write_delete_page_(ptr));
	Return(terme_display_restore_(ptr, &x, &y));
	Return(terme_screen_move_(ptr, x, y));
	return terme_write_flush_();
}

int terme_screen_rmleft_(Execute ptr)
{
	addr data, screen;

	Return(terme_root_data_(ptr, &data));
	Return(terme_root_screen_(ptr, &screen));
	/* move (0, 0) */
	Return(terme_screen_move_(ptr, 0, 0));
	/* output prompt */
	Return(terme_write_delete_line_(ptr));
	Return(terme_screen_prompt_output_(ptr));
	/* output data */
	Return(terme_screen_output_(ptr, data, screen, prompt_input));
	/* delete last line */
	Return(terme_screen_history_delete_(ptr, screen));
	/* flush */
	return terme_screen_first_(ptr);
}

int terme_screen_rmright_(Execute ptr)
{
	unsigned x, y;
	addr screen;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	x = str->now_x;
	y = str->now_y;
	Return(terme_write_delete_line_right_(ptr));
	Return(terme_screen_history_delete_(ptr, screen));
	Return(terme_screen_move_(ptr, x, y));
	return terme_write_flush_();
}


/************************************************************
 *  terme_values.c
 ************************************************************/

#ifdef LISP_TERME_WINDOWS
#include "windows_values.h"
#endif

static int terme_values_input_(Execute ptr, addr args)
{
	addr x, y;

	Return(terme_call_input_(args, &x, &y));
	setvalues_control(ptr, x, y, NULL);
	return 0;
}

static int terme_values_output_(Execute ptr, addr args)
{
	Return(terme_call_output_(args));
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_move_(Execute ptr, addr args)
{
	Return(terme_call_move_(args));
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_clear_(Execute ptr, addr args)
{
	Return(terme_call_clear_(args));
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_delete_(Execute ptr, addr args)
{
	Return(terme_call_delete_(args));
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_font_(Execute ptr, addr args)
{
	Return(terme_call_font_(ptr, args));
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_size_(Execute ptr, addr args)
{
	addr x, y;

	if (args != Nil)
		return fmte_("Invalid arguments, ~S.", args, NULL);
	Return(terme_call_size_(&x, &y));
	setvalues_control(ptr, x, y, NULL);

	return 0;
}

static int terme_values_scroll_(Execute ptr, addr args)
{
	Return(terme_call_scroll_(args));
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_begin_(Execute ptr, addr args)
{
	Return(terme_call_begin_(args, &args));
	setresult_control(ptr, args);
	return 0;
}

static int terme_values_end_(Execute ptr, addr args)
{
	addr pos;

	Return_getcons(args, &pos, &args);
	if (args != Nil)
		return fmte_("Invalid arguments, ~S.", args, NULL);
	Return(terme_call_end_(pos));
	setresult_control(ptr, Nil);

	return 0;
}

static int terme_values_enable_(Execute ptr, addr args)
{
	int check;

	/* arguments */
	if (args != Nil)
		return fmte_("Invalid arguments, ~S.", args, NULL);

	/* enable-p */
	check = terme_call_enable_p();
	setbool_control(ptr, check);

	return 0;
}

static int terme_values_signal_(Execute ptr, addr args)
{
	Return(terme_call_signal_(args));
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_operator_(Execute ptr, addr var, addr args, int *ret)
{
	addr check;

	*ret = 1;

	/* input */
	GetConst(SYSTEM_TERME_INPUT, &check);
	if (var == check)
		return terme_values_input_(ptr, args);

	/* output */
	GetConst(SYSTEM_TERME_OUTPUT, &check);
	if (var == check)
		return terme_values_output_(ptr, args);

	/* move */
	GetConst(SYSTEM_TERME_MOVE, &check);
	if (var == check)
		return terme_values_move_(ptr, args);

	/* clear */
	GetConst(SYSTEM_TERME_CLEAR, &check);
	if (var == check)
		return terme_values_clear_(ptr, args);

	/* delete */
	GetConst(SYSTEM_TERME_DELETE, &check);
	if (var == check)
		return terme_values_delete_(ptr, args);

	/* font */
	GetConst(SYSTEM_TERME_FONT, &check);
	if (var == check)
		return terme_values_font_(ptr, args);

	/* size */
	GetConst(SYSTEM_TERME_SIZE, &check);
	if (var == check)
		return terme_values_size_(ptr, args);

	/* scroll */
	GetConst(SYSTEM_TERME_SCROLL, &check);
	if (var == check)
		return terme_values_scroll_(ptr, args);

	/* begin */
	GetConst(SYSTEM_TERME_BEGIN, &check);
	if (var == check)
		return terme_values_begin_(ptr, args);

	/* end */
	GetConst(SYSTEM_TERME_END, &check);
	if (var == check)
		return terme_values_end_(ptr, args);

	/* enable */
	GetConst(SYSTEM_TERME_ENABLE, &check);
	if (var == check)
		return terme_values_enable_(ptr, args);

	/* signal */
	GetConst(SYSTEM_TERME_SIGNAL, &check);
	if (var == check)
		return terme_values_signal_(ptr, args);

	/* error */
	return Result(ret, 0);
}

int terme_values_(Execute ptr, addr var, addr args)
{
	int check;

#ifdef LISP_TERME_WINDOWS
	Return(windows_values_(ptr, var, args, &check));
	if (check)
		return 0;
#endif

	Return(terme_values_operator_(ptr, var, args, &check));
	if (check)
		return 0;

	return fmte_("Invalid keyword ~S.", var, NULL);
}


/************************************************************
 *  terme_write.c
 ************************************************************/

int terme_write_flush_(void)
{
	if (terme_finish_output())
		return terme_fmte_("terme_finish_output error.", NULL);

	return 0;
}

int terme_write_char_(Execute ptr, unicode c, unsigned width, PromptMode mode)
{
	/* display */
	Return(terme_display_write_char_(ptr, c, width, mode));

	/* terminal */
	if (terme_write_char(c, width))
		return terme_fmte_("terme_write_char error.", NULL);

	return 0;
}

int terme_write_terpri_(Execute ptr)
{
	/* display */
	Return(terme_display_terpri_(ptr));

	/* terminal */
	if (terme_terpri())
		return terme_fmte_("terme_tepri error.", NULL);

	return 0;
}

int terme_write_delete_line_right_(Execute ptr)
{
	/* display */
	Return(terme_display_delete_line_right_(ptr));

	/* terminal */
	if (terme_cursor_delete_line_right())
		return terme_fmte_("terme_cursor_delete_line_right error.", NULL);

	return 0;
}

int terme_write_left_(Execute ptr, int n)
{
	/* display */
	Return(terme_display_left_(ptr, n));

	/* terminal */
	if (terme_cursor_left(n))
		return terme_fmte_("terme_cursor_left error.", NULL);

	return 0;
}

int terme_write_right_(Execute ptr, int n)
{
	/* display */
	Return(terme_display_right_(ptr, n));

	/* terminal */
	if (terme_cursor_right(n))
		return terme_fmte_("terme_cursor_right error.", NULL);

	return 0;
}

int terme_write_up_(Execute ptr, int n)
{
	/* display */
	Return(terme_display_up_(ptr, n));

	/* terminal */
	if (terme_cursor_up(n))
		return terme_fmte_("terme_cursor_up error.", NULL);

	return 0;
}

int terme_write_down_(Execute ptr, int n)
{
	/* display */
	Return(terme_display_down_(ptr, n));

	/* terminal */
	if (terme_cursor_down(n))
		return terme_fmte_("terme_cursor_down error.", NULL);

	return 0;
}

int terme_write_first_up_(Execute ptr, int n)
{
	/* display */
	Return(terme_display_first_up_(ptr, n));

	/* terminal */
	if (terme_cursor_first_up(n))
		return terme_fmte_("terme_cursor_first_up error.", NULL);

	return 0;
}

int terme_write_first_down_(Execute ptr, int n)
{
	/* display */
	Return(terme_display_first_down_(ptr, n));

	/* terminal */
	if (terme_cursor_first_down(n))
		return terme_fmte_("terme_cursor_first_down error.", NULL);

	return 0;
}

int terme_write_delete_line_(Execute ptr)
{
	/* display */
	Return(terme_display_delete_line_(ptr));

	/* terminal */
	if (terme_cursor_delete_line())
		return terme_fmte_("terme_cursor_delete_line error.", NULL);

	return 0;
}

int terme_write_delete_page_(Execute ptr)
{
	/* display */
	Return(terme_display_delete_page_(ptr));

	/* terminal */
	if (terme_cursor_delete_page())
		return terme_fmte_("terme_cursor_delete_page error.", NULL);

	return 0;
}


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
 *  Unix
 */
#ifdef LISP_THREAD_UNIX

/*
 *  Unix semaphore
 */
int lispd_trylock_semunix(semunix *sem)
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

int lispd_get_semunix(semunix *sem)
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
int decl_function_p(LispDecl type)
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

int type_delay_p(addr pos)
{
	return GetType(pos) == LISPTYPE_TYPE && RefLispDecl(pos) == LISPDECL_DELAY;
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
	init_type_delay();
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
 *  type_call.c
 ************************************************************/

/*
 *  typep-function
 */
static int typep_function_args_(Execute ptr, addr x, addr y, int *ret)
{
	GetArrayType(x, 0, &x);  /* args */
	GetArrayType(y, 0, &y);  /* args */
	if (type_asterisk_p(y))
		return Result(ret, 1);
	if (type_asterisk_p(x))
		return Result(ret, 0);

	/* subtypep */
	return Result(ret, -1);
}

static int typep_function_values_(Execute ptr, addr x, addr y, int *ret)
{
	GetArrayType(x, 1, &x);  /* values */
	GetArrayType(y, 1, &y);  /* values */
	if (type_asterisk_p(y))
		return Result(ret, 1);
	if (type_asterisk_p(x))
		return Result(ret, 0);

	/* subtypep */
	return Result(ret, -1);
}

static int typep_function_call_(Execute ptr, addr x, addr y, int *ret)
{
	LispDecl dx, dy;
	int notp, check;

	/* not */
	GetNotDecl(x, &notp);
	if (notp)
		goto subtypep;
	GetNotDecl(y, &notp);
	if (notp)
		goto subtypep;
	GetLispDecl(x, &dx);
	GetLispDecl(y, &dy);

	/* subtypep */
	if (dy == LISPDECL_SUBTYPEP) {
		get_type_subtypep(&y, y);
		return typep_function_call_(ptr, x, y, ret);
	}
	if (dx == LISPDECL_SUBTYPEP) {
		get_type_subtypep(&x, x);
		return typep_function_call_(ptr, x, y, ret);
	}

	/* optimized */
	if (dy == LISPDECL_OPTIMIZED) {
		get_type_optimized(&y, y);
		return typep_function_call_(ptr, x, y, ret);
	}
	if (dx == LISPDECL_OPTIMIZED) {
		get_type_optimized(&x, x);
		return typep_function_call_(ptr, x, y, ret);
	}

	/* delay */
	if (dy == LISPDECL_DELAY) {
		Return(get_delay_type_(ptr, y, &y));
		return typep_function_call_(ptr, y, y, ret);
	}
	if (dx == LISPDECL_DELAY) {
		Return(get_delay_type_(ptr, x, &x));
		return typep_function_call_(ptr, x, y, ret);
	}

	/* function */
	if (! decl_function_p(dy))
		goto subtypep;
	if (! decl_function_p(dx))
		goto subtypep;
	if (dx == LISPDECL_FUNCTION && dy == LISPDECL_COMPILED_FUNCTION)
		return Result(ret, 0);

	/* arguments */
	Return(typep_function_args_(ptr, x, y, &check));
	if (check < 0)
		goto subtypep;
	if (! check)
		return Result(ret, 0);

	/* values */
	Return(typep_function_values_(ptr, x, y, &check));
	if (check < 0)
		goto subtypep;
	return Result(ret, check);

subtypep:
	return subtypep_check_(ptr, x, y, Nil, ret, NULL);
}

static int typep_function_type_(Execute ptr, addr value, addr y, int *ret)
{
	addr x;

	gettype_function(value, &x);
	if (x == Nil) {
		if (compiled_function_p(value))
			GetTypeTable(&x, CompiledFunction);
		else
			GetTypeTable(&x, Function);
	}
	CheckType(x, LISPTYPE_TYPE);

	return typep_function_call_(ptr, x, y, ret);
}


/*
 *  function
 */
int typep_function_(Execute ptr, addr value, addr type, int *ret)
{
	addr check;

	GetArrayType(type, 2, &check);
	if (check == Nil) {
		*ret = 0;
		return fmte_("The cons type (FUNCTION ? ?) don't accept.", NULL);
	}
	if (! functionp(value))
		return Result(ret, 0);

	return typep_function_type_(ptr, value, type, ret);
}


/*
 *  compiled-function
 */
int typep_compiled_function_(Execute ptr, addr value, addr type, int *ret)
{
	addr check;

	GetArrayType(type, 2, &check);
	if (check == Nil) {
		*ret = 0;
		return fmte_("The cons type (COMPILED-FUNCTION ? ?) don't accept.", NULL);
	}
	if (! compiled_function_p(value))
		return Result(ret, 0);

	return typep_function_type_(ptr, value, type, ret);
}


/************************************************************
 *  type_coerce.c
 ************************************************************/

static int coerce_type_(Execute ptr, addr pos, addr type, addr *ret);

/*
 *  type
 */
static int coerce_error_(Execute ptr, addr pos, addr type)
{
	copyheap(&pos, pos);
	copyheap(&type, type);
	Return(type_object_(&type, type));
	return call_type_error_va_(ptr, pos, type,
			"Cannot covert value ~A to a ~S type.", pos, type, NULL);
}

static int coerce_typep_(Execute ptr, addr pos, addr value, addr type, addr *ret)
{
	int check;

	Return(typep_clang_(ptr, value, type, &check));
	if (! check)
		return coerce_error_(ptr, pos, type);

	return Result(ret, value);
}


/*
 *  float
 */
static int coerce_fixnum_single_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_FIXNUM);
	single_float_fixnum_heap(&value, pos);
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_bignum_single_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(single_float_bignum_heap_(&value, pos));
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_ratio_single_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_RATIO);
	Return(single_float_ratio_heap_(&value, pos));
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_float_(Execute ptr, addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
			return coerce_typep_(ptr, pos, pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_single_(ptr, pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_single_(ptr, pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_single_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}


/*
 *  single-float
 */
static int coerce_double_single_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	single_float v;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	Return(cast_ds_value_(pos, &v));
	single_float_heap(&value, v);
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_long_single_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	single_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	Return(cast_ls_value_(pos, &v));
	single_float_heap(&value, v);
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_single_(Execute ptr, addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return coerce_typep_(ptr, pos, pos, type, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return coerce_double_single_(ptr, pos, type, ret);

		case LISPTYPE_LONG_FLOAT:
			return coerce_long_single_(ptr, pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_single_(ptr, pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_single_(ptr, pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_single_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}


/*
 *  double-float
 */
static int coerce_single_double_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	double_float v;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	Return(cast_sd_value_(pos, &v));
	double_float_heap(&value, v);
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_long_double_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	double_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	Return(cast_ld_value_(pos, &v));
	double_float_heap(&value, v);
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_fixnum_double_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_FIXNUM);
	double_float_fixnum_heap(&value, pos);
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_bignum_double_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(double_float_bignum_heap_(&value, pos));
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_ratio_double_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_RATIO);
	Return(double_float_ratio_heap_(&value, pos));
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_double_(Execute ptr, addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return coerce_single_double_(ptr, pos, type, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return coerce_typep_(ptr, pos, pos, type, ret);

		case LISPTYPE_LONG_FLOAT:
			return coerce_long_double_(ptr, pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_double_(ptr, pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_double_(ptr, pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_double_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}


/*
 *  long-float
 */
static int coerce_single_long_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	long_float v;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	Return(cast_sl_value_(pos, &v));
	long_float_heap(&value, v);
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_double_long_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	long_float v;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	Return(cast_dl_value_(pos, &v));
	long_float_heap(&value, v);
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_fixnum_long_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_FIXNUM);
	long_float_fixnum_heap(&value, pos);
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_bignum_long_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(long_float_bignum_heap_(&value, pos));
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_ratio_long_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_RATIO);
	Return(long_float_ratio_heap_(&value, pos));
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_long_(Execute ptr, addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return coerce_single_long_(ptr, pos, type, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return coerce_double_long_(ptr, pos, type, ret);

		case LISPTYPE_LONG_FLOAT:
			return coerce_typep_(ptr, pos, pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_long_(ptr, pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_long_(ptr, pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_long_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}


/*
 *  complex
 */
static int coerce_complex_complex_(Execute ptr, addr pos, addr type, addr *ret)
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

static int coerce_complex_real_(Execute ptr, addr pos, addr type, addr *ret)
{
	GetArrayType(type, 0, &type);
	if (! type_asterisk_p(type)) {
		Return(coerce_type_(ptr, pos, type, &pos));
	}

	return complex_heap_(ret, pos, fixnumh(0));
}

static int coerce_complex_(Execute ptr, addr pos, addr type, addr *ret)
{
	if (complexp(pos))
		return coerce_complex_complex_(ptr, pos, type, ret);
	if (realp(pos))
		return coerce_complex_real_(ptr, pos, type, ret);
	else
		return coerce_typep_(ptr, pos, pos, type, ret);
}


/*
 *  charcter
 */
static int coerce_unicode_character_(Execute ptr,
		addr pos, unicode c, addr type, addr *ret)
{
	addr value;
	character_heap(&value, c);
	return coerce_typep_(ptr, pos, value, type, ret);
}

static int coerce_string_character_(Execute ptr, addr pos, addr type, addr *ret)
{
	unicode c;
	size_t size;

	string_length(pos, &size);
	if (size != 1)
		return coerce_error_(ptr, pos, type);
	Return(string_getc_(pos, 0, &c));
	return coerce_unicode_character_(ptr, pos, c, type, ret);
}

static int coerce_symbol_character_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr name;
	unicode c;
	size_t size;

	GetNameSymbol(pos, &name);
	string_length(name, &size);
	if (size != 1)
		return coerce_error_(ptr, pos, type);
	Return(string_getc_(name, 0, &c));
	return coerce_unicode_character_(ptr, pos, c, type, ret);
}

static int coerce_character_(Execute ptr, addr pos, addr type, addr *ret)
{
	/* (or symbol string character) */
	if (stringp(pos))
		return coerce_string_character_(ptr, pos, type, ret);
	else if (symbolp(pos))
		return coerce_symbol_character_(ptr, pos, type, ret);
	else
		return coerce_typep_(ptr, pos, pos, type, ret);
}


/*
 *  function
 */
static int coerce_function_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr call;

	if (symbolp(pos)) {
		Return(getfunction_global_(pos, &call));
		return coerce_typep_(ptr, pos, call, type, ret);
	}
	else {
		return coerce_typep_(ptr, pos, pos, type, ret);
	}
}


/*
 *  list
 */
static int coerce_vector_list_(Execute ptr, addr pos, addr type, addr *ret)
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

	return coerce_typep_(ptr, pos, list, type, ret);
}

static int coerce_string_list_(Execute ptr, addr pos, addr type, addr *ret)
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

	return coerce_typep_(ptr, pos, list, type, ret);
}

static int coerce_array_list_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr list, x;
	size_t size, i;

	/* not vector */
	if (! array_vector_p(pos))
		return coerce_typep_(ptr, pos, type, type, ret);

	/* cast list */
	list = Nil;
	Return(array_get_vector_length_(pos, 1, &size));
	for (i = 0; i < size; i++) {
		Return(array_get_(NULL, pos, i, &x));
		cons_heap(&list, x, list);
	}
	nreverse(&list, list);

	return coerce_typep_(ptr, pos, list, type, ret);
}

static int coerce_bitvector_list_(Execute ptr, addr pos, addr type, addr *ret)
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

	return coerce_typep_(ptr, pos, list, type, ret);
}

static int coerce_list_(Execute ptr, addr pos, addr type, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_VECTOR:
			return coerce_vector_list_(ptr, pos, type, ret);

		case LISPTYPE_STRING:
			return coerce_string_list_(ptr, pos, type, ret);

		case LISPTYPE_ARRAY:
			return coerce_array_list_(ptr, pos, type, ret);

		case LISPTYPE_BITVECTOR:
			return coerce_bitvector_list_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}


/*
 *  array
 */
/* array.bit -> array.t */
static int coerce_aa_bit_t_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		fixnum_heap(&value, (fixnum)bit);
		Return(array_set_(array, i, value));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array.character -> array.t */
static int coerce_aa_character_t_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		character_heap(&value, c);
		Return(array_set_(array, i, value));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array.signed8 -> array.t */
static int coerce_aa_signed8_t_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array.signed16 -> array.t */
static int coerce_aa_signed16_t_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array.signed32 -> array.t */
static int coerce_aa_signed32_t_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.signed64 -> array.t */
static int coerce_aa_signed64_t_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}
#endif

/* array.signed -> array.t */
static int coerce_aa_signed_t_(Execute ptr,
		addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_signed8_t_(ptr, pos, type, ret);

		case 16:
			return coerce_aa_signed16_t_(ptr, pos, type, ret);

		case 32:
			return coerce_aa_signed32_t_(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_signed64_t_(ptr, pos, type, ret);
#endif

		default:
			return coerce_error_(ptr, pos, type);
	}
}

/* array.unsigned8 -> array.t */
static int coerce_aa_unsigned8_t_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array.unsigned16 -> array.t */
static int coerce_aa_unsigned16_t_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array.unsigned32 -> array.t */
static int coerce_aa_unsigned32_t_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
#ifdef LISP_64BIT
		fixnum_heap(&value, (fixnum)v);
#else
		integer_fixed_heap(&value, SignPlus, (fixed)v);
#endif
		Return(array_set_(array, i, value));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.unsigned64 -> array.t */
static int coerce_aa_unsigned64_t_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		integer_fixed_heap(&value, SignPlus, (fixed)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}
#endif

/* array.unsigned -> array.t */
static int coerce_aa_unsigned_t_(Execute ptr,
		addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_unsigned8_t_(ptr, pos, type, ret);

		case 16:
			return coerce_aa_unsigned16_t_(ptr, pos, type, ret);

		case 32:
			return coerce_aa_unsigned32_t_(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_unsigned64_t_(ptr, pos, type, ret);
#endif

		default:
			return coerce_error_(ptr, pos, type);
	}
}

/* array.single-float -> array.t */
static int coerce_aa_single_t_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		single_float_heap(&value, v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array.double-float -> array.t */
static int coerce_aa_double_t_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		double_float_heap(&value, v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array.long-float -> array.t */
static int coerce_aa_long_t_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		long_float_heap(&value, v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array -> array.t */
static int coerce_aa_t_(Execute ptr, addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return coerce_typep_(ptr, pos, pos, type, ret);

		case ARRAY_TYPE_BIT:
			return coerce_aa_bit_t_(ptr, pos, type, ret);

		case ARRAY_TYPE_CHARACTER:
			return coerce_aa_character_t_(ptr, pos, type, ret);

		case ARRAY_TYPE_SIGNED:
			return coerce_aa_signed_t_(ptr, pos, type, str->bytesize, ret);

		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_unsigned_t_(ptr, pos, type, str->bytesize, ret);

		case ARRAY_TYPE_SINGLE_FLOAT:
			return coerce_aa_single_t_(ptr, pos, type, ret);

		case ARRAY_TYPE_DOUBLE_FLOAT:
			return coerce_aa_double_t_(ptr, pos, type, ret);

		case ARRAY_TYPE_LONG_FLOAT:
			return coerce_aa_long_t_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}

/* array.t -> bitvector */
static int coerce_aa_bitvector_(Execute ptr, addr pos, addr type, addr *ret)
{
	int v, check;
	addr vector;
	size_t size, i;

	array_get_rowlength(pos, &size);
	bitmemory_unsafe(NULL, &vector, size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_bit_(pos, i, &v, &check));
		if (check)
			return coerce_error_(ptr, pos, type);
		Return(bitmemory_setint_(vector, i, v));
	}

	return coerce_typep_(ptr, pos, vector, type, ret);
}

/* array.* -> array.bit */
static int coerce_aa_type_bit_(Execute ptr, addr pos, addr type, addr *ret)
{
	int v, check;
	addr array;
	size_t size, i;

	/* bit-vector */
	if (array_vector_p(pos))
		return coerce_aa_bitvector_(ptr, pos, type, ret);

	/* array.bit */
	array_get_rowlength(pos, &size);
	Return(array_coerce_bit_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_bit_(pos, i, &v, &check));
		if (check)
			return coerce_error_(ptr, pos, type);
		Return(array_set_bit_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array -> array.bit */
static int coerce_aa_bit_(Execute ptr, addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_type_bit_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}

/* array.t -> string */
static int coerce_aa_string_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(strvect_setc_(vector, i, v));
	}

	return coerce_typep_(ptr, pos, vector, type, ret);
}

/* array.t -> array.character */
static int coerce_aa_t_character_(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	unicode v;
	addr array;
	size_t size, i;

	/* bit-vector */
	if (array_vector_p(pos))
		return coerce_aa_string_(ptr, pos, type, ret);

	/* array.bit */
	array_get_rowlength(pos, &size);
	Return(array_coerce_character_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_character_(pos, i, &v, &check));
		if (check)
			return coerce_error_(ptr, pos, type);
		Return(array_set_character_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array -> array.character */
static int coerce_aa_character_(Execute ptr, addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return coerce_aa_t_character_(ptr, pos, type, ret);

		case ARRAY_TYPE_CHARACTER:
		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}

/* array.* -> array.signed8 */
static int coerce_aa_signed8_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_signed8_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array.* -> array.signed16 */
static int coerce_aa_signed16_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_signed16_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array.* -> array.signed32 */
static int coerce_aa_signed32_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_signed32_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.* -> array.signed64 */
static int coerce_aa_signed64_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_signed64_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}
#endif

/* array.* -> array.signed */
static int coerce_aa_type_signed_(Execute ptr,
		addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_signed8_(ptr, pos, type, ret);

		case 16:
			return coerce_aa_signed16_(ptr, pos, type, ret);

		case 32:
			return coerce_aa_signed32_(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_signed64_(ptr, pos, type, ret);
#endif

		default:
			return coerce_error_(ptr, pos, type);
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

static int coerce_aa_signed_(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (type_second_size(type, &size))
		return coerce_typep_(ptr, pos, pos, type, ret);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_type_signed_(ptr, pos, type, size, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}

/* array.* -> array.unsigned8 */
static int coerce_aa_unsigned8_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_unsigned8_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array.* -> array.unsigned16 */
static int coerce_aa_unsigned16_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_unsigned16_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array.* -> array.unsigned32 */
static int coerce_aa_unsigned32_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_unsigned32_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.* -> array.unsigned64 */
static int coerce_aa_unsigned64_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_unsigned64_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}
#endif

/* array.* -> array.unsigned */
static int coerce_aa_type_unsigned_(Execute ptr,
		addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_unsigned8_(ptr, pos, type, ret);

		case 16:
			return coerce_aa_unsigned16_(ptr, pos, type, ret);

		case 32:
			return coerce_aa_unsigned32_(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_unsigned64_(ptr, pos, type, ret);
#endif

		default:
			return coerce_error_(ptr, pos, type);
	}
}

/* array -> array.unsigned */
static int coerce_aa_unsigned_(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (type_second_size(type, &size))
		return coerce_typep_(ptr, pos, pos, type, ret);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_type_unsigned_(ptr, pos, type, size, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}

/* array.float -> array.single */
static int coerce_aa_type_single_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_single_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array -> array.single-float */
static int coerce_aa_single_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_aa_type_single_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}

/* array.float -> array.double */
static int coerce_aa_type_double_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_double_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array -> array.double-float */
static int coerce_aa_double_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_aa_type_double_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}

/* array.float -> array.long */
static int coerce_aa_type_long_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_long_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* array -> array.long-float */
static int coerce_aa_long_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_aa_type_long_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}

/* array -> array */
static int coerce_aa_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep_(ptr, pos, pos, type, ret);

	/* (array upg .) */
	switch (LowLispDecl(upg)) {
		case LISPDECL_T:
			return coerce_aa_t_(ptr, pos, type, ret);

		case LISPDECL_BIT:
			return coerce_aa_bit_(ptr, pos, type, ret);

		case LISPDECL_CHARACTER:
			return coerce_aa_character_(ptr, pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_aa_signed_(ptr, pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_aa_unsigned_(ptr, pos, type, ret);

		case LISPDECL_SINGLE_FLOAT:
			return coerce_aa_single_(ptr, pos, type, ret);

		case LISPDECL_DOUBLE_FLOAT:
			return coerce_aa_double_(ptr, pos, type, ret);

		case LISPDECL_LONG_FLOAT:
			return coerce_aa_long_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}

/* vector -> array.bit */
static int coerce_av_bit_(Execute ptr, addr pos, addr type, addr *ret)
{
	int v, check;
	addr vector;
	size_t size, i;

	lenarray(pos, &size);
	bitmemory_unsafe(NULL, &vector, size);
	for (i = 0; i < size; i++) {
		Return(vector_coerce_bit_(pos, i, &v, &check));
		if (check)
			return coerce_error_(ptr, pos, type);
		Return(bitmemory_setint_(vector, i, v));
	}

	return coerce_typep_(ptr, pos, vector, type, ret);
}

/* vector -> array.character */
static int coerce_av_character_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(strvect_setc_(vector, i, v));
	}

	return coerce_typep_(ptr, pos, vector, type, ret);
}

/* vector -> array.signed8 */
static int coerce_av_signed8_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_signed8_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* vector -> array.signed16 */
static int coerce_av_signed16_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_signed16_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* vector -> array.signed32 */
static int coerce_av_signed32_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_signed32_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* vector -> array.signed64 */
static int coerce_av_signed64_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_signed64_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}
#endif

/* vector -> array.signed */
static int coerce_av_signed_(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep_(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_av_signed8_(ptr, pos, type, ret);

		case 16:
			return coerce_av_signed16_(ptr, pos, type, ret);

		case 32:
			return coerce_av_signed32_(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_av_signed64_(ptr, pos, type, ret);
#endif

		default:
			return coerce_error_(ptr, pos, type);
	}
}

/* vector -> array.unsigned8 */
static int coerce_av_unsigned8_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_unsigned8_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* vector -> array.unsigned16 */
static int coerce_av_unsigned16_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_unsigned16_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* vector -> array.unsigned32 */
static int coerce_av_unsigned32_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_unsigned32_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* vector -> array.unsigned64 */
static int coerce_av_unsigned64_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_unsigned64_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}
#endif

/* vector -> array.unsigned */
static int coerce_av_unsigned_(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep_(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_av_unsigned8_(ptr, pos, type, ret);

		case 16:
			return coerce_av_unsigned16_(ptr, pos, type, ret);

		case 32:
			return coerce_av_unsigned32_(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_av_unsigned64_(ptr, pos, type, ret);
#endif

		default:
			return coerce_error_(ptr, pos, type);
	}
}

/* vector -> array.single-float */
static int coerce_av_single_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_single_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* vector -> array.double-float */
static int coerce_av_double_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_double_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* vector -> array.long-float */
static int coerce_av_long_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_long_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* vector -> array */
static int coerce_av_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep_(ptr, pos, pos, type, ret);

	/* (array upg .) */
	switch (LowLispDecl(upg)) {
		case LISPDECL_T:
			return coerce_typep_(ptr, pos, pos, type, ret);

		case LISPDECL_BIT:
			return coerce_av_bit_(ptr, pos, type, ret);

		case LISPDECL_CHARACTER:
			return coerce_av_character_(ptr, pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_av_signed_(ptr, pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_av_unsigned_(ptr, pos, type, ret);

		case LISPDECL_SINGLE_FLOAT:
			return coerce_av_single_(ptr, pos, type, ret);

		case LISPDECL_DOUBLE_FLOAT:
			return coerce_av_double_(ptr, pos, type, ret);

		case LISPDECL_LONG_FLOAT:
			return coerce_av_long_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}

/* string -> array.t */
static int coerce_as_t_(Execute ptr, addr pos, addr type, addr *ret)
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

	return coerce_typep_(ptr, pos, vector, type, ret);
}

/* string -> array */
static int coerce_as_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep_(ptr, pos, pos, type, ret);

	/* (array upg .) */
	switch (LowLispDecl(upg)) {
		case LISPDECL_T:
			return coerce_as_t_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}

/* bit-vector -> array.t */
static int coerce_ab_t_(Execute ptr, addr pos, addr type, addr *ret)
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

	return coerce_typep_(ptr, pos, vector, type, ret);
}

/* bit-vector -> array.signed8 */
static int coerce_ab_signed8_(Execute ptr, addr pos, addr type, addr *ret)
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

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* bit-vector -> array.signed16 */
static int coerce_ab_signed16_(Execute ptr, addr pos, addr type, addr *ret)
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

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* bit-vector -> array.signed32 */
static int coerce_ab_signed32_(Execute ptr, addr pos, addr type, addr *ret)
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

	return coerce_typep_(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* bit-vector -> array.signed64 */
static int coerce_ab_signed64_(Execute ptr, addr pos, addr type, addr *ret)
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

	return coerce_typep_(ptr, pos, array, type, ret);
}
#endif

/* bit-vector -> array.signed */
static int coerce_ab_signed_(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep_(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_ab_signed8_(ptr, pos, type, ret);

		case 16:
			return coerce_ab_signed16_(ptr, pos, type, ret);

		case 32:
			return coerce_ab_signed32_(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_ab_signed64_(ptr, pos, type, ret);
#endif

		default:
			return coerce_error_(ptr, pos, type);
	}
}

/* bit-vector -> array.unsigned8 */
static int coerce_ab_unsigned8_(Execute ptr, addr pos, addr type, addr *ret)
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

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* bit-vector -> array.unsigned16 */
static int coerce_ab_unsigned16_(Execute ptr, addr pos, addr type, addr *ret)
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

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* bit-vector -> array.unsigned32 */
static int coerce_ab_unsigned32_(Execute ptr, addr pos, addr type, addr *ret)
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

	return coerce_typep_(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* bit-vector -> array.unsigned64 */
static int coerce_ab_unsigned64_(Execute ptr, addr pos, addr type, addr *ret)
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

	return coerce_typep_(ptr, pos, array, type, ret);
}
#endif

/* bit-vector -> array.unsigned */
static int coerce_ab_unsigned_(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep_(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_ab_unsigned8_(ptr, pos, type, ret);

		case 16:
			return coerce_ab_unsigned16_(ptr, pos, type, ret);

		case 32:
			return coerce_ab_unsigned32_(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_ab_unsigned64_(ptr, pos, type, ret);
#endif

		default:
			return coerce_error_(ptr, pos, type);
	}
}

/* bit-vector -> array */
static int coerce_ab_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep_(ptr, pos, pos, type, ret);

	/* (array upg .) */
	switch (LowLispDecl(upg)) {
		case LISPDECL_T:
			return coerce_ab_t_(ptr, pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_ab_signed_(ptr, pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_ab_unsigned_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}

/* list -> array.bit */
static int coerce_al_t_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr vector, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	vector_heap(&vector, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		setarray(vector, i, value);
	}

	return coerce_typep_(ptr, pos, vector, type, ret);
}

/* list -> array.bit */
static int coerce_al_bit_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(bitmemory_setint_(vector, i, v));
	}

	return coerce_typep_(ptr, pos, vector, type, ret);
}

/* list -> array.character */
static int coerce_al_character_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(strvect_setc_(vector, i, v));
	}

	return coerce_typep_(ptr, pos, vector, type, ret);
}

/* list -> array.signed8 */
static int coerce_al_signed8_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_signed8_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* list -> array.signed16 */
static int coerce_al_signed16_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_signed16_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* list -> array.signed32 */
static int coerce_al_signed32_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_signed32_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* list -> array.signed64 */
static int coerce_al_signed64_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_signed64_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}
#endif

/* list -> array.signed */
static int coerce_al_signed_(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep_(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_al_signed8_(ptr, pos, type, ret);

		case 16:
			return coerce_al_signed16_(ptr, pos, type, ret);

		case 32:
			return coerce_al_signed32_(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_al_signed64_(ptr, pos, type, ret);
#endif

		default:
			return coerce_error_(ptr, pos, type);
	}
}

/* list -> array.unsigned8 */
static int coerce_al_unsigned8_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_unsigned8_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* list -> array.unsigned16 */
static int coerce_al_unsigned16_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_unsigned16_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* list -> array.unsigned32 */
static int coerce_al_unsigned32_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_unsigned32_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* list -> array.unsigned64 */
static int coerce_al_unsigned64_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_unsigned64_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}
#endif

/* list -> array.unsigned */
static int coerce_al_unsigned_(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep_(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_al_unsigned8_(ptr, pos, type, ret);

		case 16:
			return coerce_al_unsigned16_(ptr, pos, type, ret);

		case 32:
			return coerce_al_unsigned32_(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_al_unsigned64_(ptr, pos, type, ret);
#endif

		default:
			return coerce_error_(ptr, pos, type);
	}
}

/* list -> array.single-float */
static int coerce_al_single_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_single_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* list -> array.double-float */
static int coerce_al_double_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_double_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* list -> array.long-float */
static int coerce_al_long_(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_error_(ptr, pos, type);
		Return(array_set_long_(array, i, v));
	}

	return coerce_typep_(ptr, pos, array, type, ret);
}

/* list -> array */
static int coerce_al_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_al_t_(ptr, pos, type, ret);

	/* (array upg .) */
	switch (LowLispDecl(upg)) {
		case LISPDECL_T:
			return coerce_al_t_(ptr, pos, type, ret);

		case LISPDECL_BIT:
			return coerce_al_bit_(ptr, pos, type, ret);

		case LISPDECL_CHARACTER:
			return coerce_al_character_(ptr, pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_al_signed_(ptr, pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_al_unsigned_(ptr, pos, type, ret);

		case LISPDECL_SINGLE_FLOAT:
			return coerce_al_single_(ptr, pos, type, ret);

		case LISPDECL_DOUBLE_FLOAT:
			return coerce_al_double_(ptr, pos, type, ret);

		case LISPDECL_LONG_FLOAT:
			return coerce_al_long_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}

/* ? -> array */
static int coerce_array_(Execute ptr, addr pos, addr type, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return coerce_aa_(ptr, pos, type, ret);

		case LISPTYPE_VECTOR:
			return coerce_av_(ptr, pos, type, ret);

		case LISPTYPE_STRING:
			return coerce_as_(ptr, pos, type, ret);

		case LISPTYPE_BITVECTOR:
			return coerce_ab_(ptr, pos, type, ret);

		case LISPTYPE_CONS:
		case LISPTYPE_NIL:
			return coerce_al_(ptr, pos, type, ret);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
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
	CoerceTable[LISPDECL_FLOAT] = coerce_float_;
	CoerceTable[LISPDECL_SHORT_FLOAT] = coerce_single_; /* single */
	CoerceTable[LISPDECL_SINGLE_FLOAT] = coerce_single_;
	CoerceTable[LISPDECL_DOUBLE_FLOAT] = coerce_double_;
	CoerceTable[LISPDECL_LONG_FLOAT] = coerce_long_;
	CoerceTable[LISPDECL_COMPLEX] = coerce_complex_;
	CoerceTable[LISPDECL_CHARACTER] = coerce_character_;
	CoerceTable[LISPDECL_BASE_CHAR] = coerce_character_;
	CoerceTable[LISPDECL_STANDARD_CHAR] = coerce_character_;
	CoerceTable[LISPDECL_EXTENDED_CHAR] = coerce_character_;
	CoerceTable[LISPDECL_FUNCTION] = coerce_function_;
	CoerceTable[LISPDECL_COMPILED_FUNCTION] = coerce_function_;
	/* list */
	CoerceTable[LISPDECL_LIST] = coerce_list_;
	CoerceTable[LISPDECL_CONS] = coerce_list_;
	/* array */
	CoerceTable[LISPDECL_ARRAY] = coerce_array_;
	CoerceTable[LISPDECL_SIMPLE_ARRAY] = coerce_array_;
}

static int coerce_table_(Execute ptr, addr pos, addr type, addr *ret)
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

static int coerce_optimize_(Execute ptr, addr pos, addr type, addr *ret)
{
	int ignore;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	Return(type_optimize_local_(local, type, &type, &ignore));
	get_type_optimized(&type, type);
	Return(coerce_table_(ptr, pos, type, ret));
	rollback_local(local, stack);

	return 0;
}

static int coerce_type_call_(Execute ptr, addr pos, addr type, addr *ret)
{
	addr check;

	CheckType(type, LISPTYPE_TYPE);
	if (! RefNotDecl(type)) {
		Return(coerce_table_(ptr, pos, type, &check));
		if (check != Unbound)
			return Result(ret, check);
	}
	Return(coerce_optimize_(ptr, pos, type, &check));
	if (check != Unbound)
		return Result(ret, check);

	return coerce_typep_(ptr, pos, pos, type, ret);
}

static int coerce_type_(Execute ptr, addr pos, addr type, addr *ret)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva(hold, pos, type, NULL);
	Return(coerce_type_call_(ptr, pos, type, ret));
	localhold_end(hold);

	return 0;
}

static int coerce_parse_(Execute ptr, addr pos, addr type, addr *ret)
{
	Return(parse_type_(ptr, &type, type, Nil));
	return coerce_type_(ptr, pos, type, ret);
}

static int coerce_execute_(Execute ptr, addr pos, addr type, addr *ret)
{
	switch (GetType(type)) {
		case LISPTYPE_NIL:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_CONS:
			return coerce_parse_(ptr, pos, type, ret);

		case LISPTYPE_TYPE:
			return coerce_type_(ptr, pos, type, ret);

		case LISPTYPE_T:
			return Result(ret, pos);

		default:
			return coerce_typep_(ptr, pos, pos, type, ret);
	}
}

int coerce_common_(Execute ptr, addr pos, addr type, addr *ret)
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
DefTypeTable(0,  PIPE_STREAM,          PipeStream           );
DefTypeTable(0,  QUOTE,                Quote                );
DefTypeTable(0,  BYTESPEC,             ByteSpec             );
DefTypeTable(0,  PRINT_DISPATCH,       PrintDispatch        );
DefTypeTable(0,  PAPER,                Paper                );
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

static void typetable_stringdesignator(void)
{
	/* (or string symbol character) */
	addr type1, type2, type3, pos;

	GetTypeTable(&type1, String);
	GetTypeTable(&type2, Symbol);
	GetTypeTable(&type3, Character);
	type3or_heap(type1, type2, type3, &pos);
	SetTypeTable(StringDesignator, pos);
}

static void typetable_packagedesignator(void)
{
	/* (or package string symbol character) */
	addr type1, type2, type3, type4, pos;

	GetTypeTable(&type1, Package);
	GetTypeTable(&type2, String);
	GetTypeTable(&type3, Symbol);
	GetTypeTable(&type4, Character);
	type4or_heap(type1, type2, type3, type4, &pos);
	SetTypeTable(PackageDesignator, pos);
}

static void typetable_packagedesignatornull(void)
{
	SetTypeTableNull(PackageDesignator);
}

static void typetable_functiondesignator(void)
{
	/* (or function symbol) */
	addr type1, type2, pos;

	GetTypeTable(&type1, Function);
	GetTypeTable(&type2, Symbol);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(FunctionDesignator, pos);
}

static void typetable_restartdesignator(void)
{
	/* (or restart (and symbol (not null))) */
	addr type1, type2, type3, pos;

	GetTypeTable(&type1, Restart);
	GetTypeTable(&type2, Symbol);
	type0not_heap(LISPDECL_NULL, &type3);
	type2and_heap(type2, type3, &type2);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(RestartDesignator, pos);
}

static void typetable_pathnamedesignator(void)
{
	/* (or pathname string stream) */
	addr type1, type2, type3, pos;

	GetTypeTable(&type1, Pathname);
	GetTypeTable(&type2, String);
	GetTypeTable(&type3, Stream);
	type3or_heap(type1, type2, type3, &pos);
	SetTypeTable(PathnameDesignator, pos);
}

static void typetable_pathnamedesignatornull(void)
{
	/* (or pathname-designator null) */
	addr type1, type2, pos;

	GetTypeTable(&type1, PathnameDesignator);
	GetTypeTable(&type2, Null);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(PathnameDesignatorNull, pos);
}

static void typetable_pathnamedesignatorboolean(void)
{
	/* (or pathname-designator boolean) */
	addr type1, type2, pos;

	GetTypeTable(&type1, PathnameDesignator);
	GetTypeTable(&type2, Boolean);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(PathnameDesignatorBoolean, pos);
}

static void typetable_streamdesignator(void)
{
	/* (or stream boolean) */
	addr type1, type2, pos;

	GetTypeTable(&type1, Stream);
	GetTypeTable(&type2, Boolean);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(StreamDesignator, pos);
}

static void typetable_readtabledesignator(void)
{
	/* (or readtable null) */
	addr type1, type2, pos;

	GetTypeTable(&type1, Readtable);
	GetTypeTable(&type2, Null);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(ReadtableDesignator, pos);
}

static void typetable_conditiondesignator(void)
{
	/* (or string symbol condition) */
	addr type1, type2, type3, pos;

	GetTypeTable(&type1, String);
	GetTypeTable(&type2, Symbol);
	GetTypeTable(&type3, Condition);
	type3or_heap(type1, type2, type3, &pos);
	SetTypeTable(ConditionDesignator, pos);
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
	/* &key (:key      [function-designator])
	 *      (:test     [function-designator])
	 *      (:test-not [function-designator])
	 */
	addr key, key1, key2, key3, type;

	GetConst(KEYWORD_KEY, &key1);
	GetConst(KEYWORD_TEST, &key2);
	GetConst(KEYWORD_TEST_NOT, &key3);
	GetTypeTable(&type, FunctionDesignator);
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
	KeyTypeTable(&key4, KEY, FunctionDesignator);
	KeyTypeTable(&key5, TEST, FunctionDesignator);
	KeyTypeTable(&key6, TEST_NOT, FunctionDesignator);
	list_heap(&key, key1, key2, key3, key4, key5, key6, NULL);
	SetTypeTable(CountKey, key);
}

static void typetable_countifkey(void)
{
	addr key, key1, key2, key3, key4;

	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, START, KeywordStart);
	KeyTypeTable(&key3, END, KeywordEnd);
	KeyTypeTable(&key4, KEY, FunctionDesignator);
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

static void typetable_string_designator_list(void)
{
	typetable_orlist(TypeTable_StringDesignator, TypeTable_StringDesignatorList);
}

static void typetable_package_designator_list(void)
{
	typetable_orlist(TypeTable_PackageDesignator, TypeTable_PackageDesignatorList);
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

static void typetable_generic_function(void)
{
	addr pos;

	GetConst(CLOS_GENERIC_FUNCTION, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(GenericFunction, pos);
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

static void typeargs_packagedesignator(void)
{
	addr pos;
	GetTypeTable(&pos, PackageDesignator);
	typeargs_var1(&pos, pos);
	SetTypeArgs(PackageDesignator, pos);
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
	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1key(&args, args, key);
	SetTypeArgs(PathnameCase, args);
}

static void typeargs_error(void)
{
	addr args, type;

	GetTypeTable(&args, ConditionDesignator);
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
DefTypeValues(Paper);
DefTypeValues(MethodCombination);
DefTypeValues(GenericFunction);


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
	/* (function (string-designator &key (start keyword-start)
	 *                                 (end keyword-end))
	 *           (values string &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, StringDesignator);
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
	/* (function (sequence string-designator) (values string &rest nil)) */
	addr args, values, type;

	GetTypeTable(&args, Sequence);
	GetTypeTable(&type, StringDesignator);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(StringTrim, args);
}

static void typecompiled_stringequal(void)
{
	/* (function (string-designator string-designator &key
	 *             (start1 keyword-start)
	 *             (end1   keyword-end)
	 *             (start2 keyword-start)
	 *             (end2   keyword-end))
	 *           (values boolean &rest null))
	 */
	addr args, values;

	GetTypeTable(&args, StringDesignator);
	GetTypeTable(&values, KeyStart2End2);
	typeargs_var2key(&args, args, args, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(StringEqual, args);
}

static void typecompiled_stringmismatch(void)
{
	/* (function (string-designator string-designator &key
	 *             (start1 keyword-start)
	 *             (end1   keyword-end)
	 *             (start2 keyword-start)
	 *             (end2   keyword-end))
	 *           (values keyword-end &rest null))
	 */
	addr args, values;

	GetTypeTable(&args, StringDesignator);
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

	/* (function (&optional input-stream-designator t t t) (values t &rest nil)) */
	GetTypeTable(&args, StreamDesignator);
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
	GetTypeTable(&call, FunctionDesignator);
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
	/* (function (function-designator sequence &rest sequence)
	 *   (values boolean &rest nil))
	 */
	addr args, values, call, sequence;

	GetTypeTable(&call, FunctionDesignator);
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
	 *   ((or list symbol) &optional package-designator)
	 *   (values (eql t) &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, SymbolList);
	GetTypeTable(&values, PackageDesignator);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Export, args);
}

static void typecompiled_usepackage(void)
{
	/*  (function
	 *    ((or list package-designator) &optional package-designator)
	 *    (values (eql t) &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, List);
	GetTypeTable(&values, PackageDesignator);
	type2or_heap(args, values, &args);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(UsePackage, args);
}

static void typecompiled_intern(void)
{
	/*  (function
	 *    (string &optional package-designator)
	 *    (values symbol (member :internal :external :inherited nil) &rest nil))
	 */
	addr args, values, symbol, key1, key2, key3, status;

	/* args */
	GetTypeTable(&args, String);
	GetTypeTable(&values, PackageDesignator);
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
	 *    (package-designator)
	 *    (values list &rest nil))
	 */
	addr args, values;

	GetTypeArgs(&args, PackageDesignator);
	GetTypeTable(&values, List);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(PackageNicknames, args);
}

static void typecompiled_prin1(void)
{
	/*  (function
	 *    (t &optional stream-designator)
	 *    (values t &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, StreamDesignator);
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
	/* (function (function-designator list &key (key function-designator))
	 *           (values list &rest nil))
	 */
	addr args, values, type, key;

	GetTypeTable(&type, FunctionDesignator);
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
	/* (function (function-designator list &rest list)
	 *           (values list &rest nil))
	 */
	addr args, values, type;

	GetTypeTable(&type, FunctionDesignator);
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
	/* (function (function package-designator) (values &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Function);
	GetTypeTable(&values, PackageDesignator);
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
	/* (function (function-designator sequence
	 *     &key (:from-end t)
	 *          (:start (integer 0 *))
	 *          (:end (or (integer 0 *) null))
	 *          (:key function-designator))
	 *     (values index &rest nil))
	 */
	addr args, values, key;

	GetTypeTable(&args, FunctionDesignator);
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
	GetTypeTable(&type, FunctionDesignator);
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
	/* (function (function-designator sequence
	 *     &key (:from-end t)
	 *          (:start (integer 0 *))
	 *          (:end (or (integer 0 *) null))
	 *          (:key function-designator))
	 *     (values t &rest nil))
	 */
	addr args, values, key;

	GetTypeTable(&args, FunctionDesignator);
	GetTypeTable(&values, Sequence);
	GetTypeTable(&key, CountIfKey);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(FindIf, args);
}

static void typecompiled_positionif(void)
{
	/* (function (function-designator sequence
	 *     &key (:from-end t)
	 *          (:start (integer 0 *))
	 *          (:end (or (integer 0 *) null))
	 *          (:key function-designator))
	 *     (values index-null &rest nil))
	 */
	addr args, values, key;

	GetTypeTable(&args, FunctionDesignator);
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
	KeyTypeTable(&key2, TEST, FunctionDesignator);
	KeyTypeTable(&key3, TEST_NOT, FunctionDesignator);
	KeyTypeTable(&key4, KEY, FunctionDesignator);
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
	KeyTypeTable(&key2, TEST, FunctionDesignator);
	KeyTypeTable(&key3, TEST_NOT, FunctionDesignator);
	KeyTypeTable(&key4, KEY, FunctionDesignator);
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
	KeyTypeTable(&key2, KEY, FunctionDesignator);
	KeyTypeTable(&key3, START, KeywordStart);
	KeyTypeTable(&key4, END, KeywordEnd);
	KeyTypeTable(&key5, COUNT, IntegerNull);
	list_heap(&key, key1, key2, key3, key4, key5, NULL);

	/* type */
	GetTypeTable(&args, T);
	GetTypeTable(&values, FunctionDesignator);
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
	KeyTypeTable(&key2, TEST, FunctionDesignator);
	KeyTypeTable(&key3, TEST_NOT, FunctionDesignator);
	KeyTypeTable(&key4, KEY, FunctionDesignator);
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
	KeyTypeTable(&key2, KEY, FunctionDesignator);
	KeyTypeTable(&key3, START, KeywordStart);
	KeyTypeTable(&key4, END, KeywordEnd);
	KeyTypeTable(&key5, COUNT, IntegerNull);
	list_heap(&key, key1, key2, key3, key4, key5, NULL);

	/* type */
	GetTypeTable(&args, FunctionDesignator);
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
	KeyTypeTable(&key2, TEST, FunctionDesignator);
	KeyTypeTable(&key3, TEST_NOT, FunctionDesignator);
	KeyTypeTable(&key4, KEY, FunctionDesignator);
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
	/* (function (pathname-designator) (values string &rest nil)) */
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1(&args, args);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Namestring, args);
}

static void typecompiled_pathname(void)
{
	/* (function (pathname-designator) (values pathname &rest nil)) */
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
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
	/* (function (&optional stream-designator t t t) (values t &rest nil)) */
	addr args, values;

	GetTypeTable(&args, StreamDesignator);
	GetTypeTable(&values, T);
	typeargs_opt4(&args, args, values, values, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(ReadChar, args);
}

static void typecompiled_writestring(void)
{
	/* (function (string &optional stream-designator &key start end)
	 *           (values string &rest nil))
	 */
	addr args, values, var, opt, key, key1, key2;

	/* var */
	GetTypeTable(&var, String);
	conscar_heap(&var, var);
	/* opt */
	GetTypeTable(&opt, StreamDesignator);
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
	/* (function (output-stream-designator t &optional t t)
	 *           (values null &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, StreamDesignator);
	GetTypeTable(&values, T);
	typeargs_var2opt2(&args, args, values, values, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(PprintFill, args);
}

static void typecompiled_dispatch_function(void)
{
	/* (function (output-stream-designator t) (values T &rest nil)) */
	addr args, values;

	GetTypeTable(&args, StreamDesignator);
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

	GetTypeTable(&args, PathnameDesignator);
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
	typetable_PipeStream();
	typetable_Quote();
	typetable_ByteSpec();
	typetable_PrintDispatch();
	typetable_Paper();
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
	typetable_stringdesignator();
	typetable_packagedesignator();
	typetable_packagedesignatornull();
	typetable_functiondesignator();
	typetable_restartdesignator();
	typetable_pathnamedesignator();
	typetable_pathnamedesignatornull();
	typetable_pathnamedesignatorboolean();
	typetable_streamdesignator();
	typetable_readtabledesignator();
	typetable_conditiondesignator();
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
	typetable_string_designator_list();
	typetable_package_designator_list();

	typetable_method();
	typetable_class();
	typetable_classnull();
	typetable_standardclass();
	typetable_standardobject();
	typetable_structureclass();
	typetable_structureobject();
	typetable_standard_method();
	typetable_methodcombination();
	typetable_generic_function();

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
	typeargs_packagedesignator();
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
	typevalues_Paper();
	typevalues_MethodCombination();
	typevalues_GenericFunction();

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

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#ifdef __clang__
#pragma clang diagnostic pop
#endif
