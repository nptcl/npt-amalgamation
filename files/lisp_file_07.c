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

#include <float.h>
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "lisp_file.h"


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
		Return(princ_print(ptr, stream, value));
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
		Return(callclang_apply(ptr, &body, body, Nil));
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

static int find_print_dispatch(Execute ptr, addr var, addr list, addr *ret)
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

int find_function_print_dispatch(Execute ptr, addr var, addr table, addr *ret)
{
	Return(find_print_dispatch(ptr, var, table, &var));
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
	Return(find_print_dispatch(ptr, var, table, &var));
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
static int pprint_logical_block_type_form(Execute ptr, enum pprint_newline type)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* body */
	Return(pprint_exit_common(ptr, stream));
	for (;;) {
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(write_print(ptr, stream, pos));
		Return(pprint_exit_common(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, type, stream));
	}

	return 0;
}

static int pprint_logical_block_type_call_(Execute ptr, pointer type, addr stream)
{
	addr gensym;

	Return(gensym_pretty_stream_(stream, &gensym));
	(void)catch_clang(ptr, type, gensym, stream);
	return close_pretty_stream_unwind_protect_(ptr, stream);
}

static int pprint_logical_block_type(Execute ptr, pointer type)
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

static int pprint_type_print(Execute ptr,
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

static int pprint_list_common(Execute ptr, addr stream, addr list, pointer type)
{
	return pprint_type_print(ptr, stream, list, 1, type);
}

static int pprint_logical_block_fill_form(Execute ptr)
{
	return pprint_logical_block_type_form(ptr, pprint_newline_fill);
}

static int pprint_logical_block_fill(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_logical_block_fill_form);
}

int pprint_fill_print(Execute ptr, addr stream, addr list, int colon)
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
	return pprint_type_print(ptr, stream, list, colon, p_pprint_logical_block_fill);
}


/*
 *  pprint-linaer
 */
static int pprint_logical_block_linear_form(Execute ptr)
{
	return pprint_logical_block_type_form(ptr, pprint_newline_linear);
}

static int pprint_logical_block_linear(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_logical_block_linear_form);
}

int pprint_linear_print(Execute ptr, addr stream, addr list, int colon)
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
	return pprint_type_print(ptr, stream, list, colon, p_pprint_logical_block_linear);
}


/*
 *  pprint-tabular
 */
static int pprint_logical_block_tabular_form(Execute ptr)
{
	addr cons, stream, pos;
	fixnum colinc;

	getdata_control(ptr, &cons);
	GetCons(cons, &stream, &pos);
	Check(! pretty_stream_p(stream), "type error");
	GetFixnum(pos, &colinc);
	Return(check_pretty_stream(ptr, stream));
	/* body */
	Return(pprint_exit_common(ptr, stream));
	for (;;) {
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(write_print(ptr, stream, pos));
		Return(pprint_exit_common(ptr, stream));
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
	(void)catch_clang(ptr, p_pprint_logical_block_tabular_form, gensym, cons);
	return close_pretty_stream_unwind_protect_(ptr, stream);
}

static int pprint_logical_block_tabular(Execute ptr)
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

int pprint_tabular_print(Execute ptr,
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
static int pprint_dispatch_vector2(Execute ptr)
{
	addr cons, stream, pos, vector;
	size_t size, i;

	getdata_control(ptr, &cons);
	GetCons(cons, &stream, &vector);
	Check(! pretty_stream_p(stream), "type error");
	Check(! vectorp_sequence_debug(vector), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* body */
	Return(length_sequence_(vector, 1, &size));
	if (size == 0)
		return 0;
	i = 0;
	for (;;) {
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(getelt_sequence_(NULL, vector, i, &pos));
		Return(write_print(ptr, stream, pos));
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
	(void)catch_clang(ptr, p_pprint_dispatch_vector2, gensym, cons);
	return close_pretty_stream_unwind_protect_(ptr, stream);
}

static int pprint_dispatch_vector1(Execute ptr)
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

static int pprint_dispatch_vector(Execute ptr, addr stream, addr pos)
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
static int pprint_dispatch_quote2(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* body */
	Return(write_char_stream_(stream, '\''));
	Return(pprint_pop_common(ptr, stream, &pos));  /* quote */
	Return(pprint_pop_common(ptr, stream, &pos));
	Return(write_print(ptr, stream, pos));

	return 0;
}

static int pprint_dispatch_quote1(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_dispatch_quote2);
}

static int pprint_dispatch_quote(Execute ptr, addr stream, addr list)
{
	/* (defun dispatch-quote (*standard-outupt* list)
	 *   (pprint-logical-block (nil list)
	 *     ;; name
	 *     (write-char #\')
	 *     (pprint-pop) ;; quote
	 *     (write (pprint-pop))))
	 */
	return pprint_type_print(ptr, stream, list, 0, p_pprint_dispatch_quote1);
}


/*
 *  dispatch-call
 */
static int pprint_dispatch_call2(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* body */
	Return(pprint_pop_common(ptr, stream, &pos));
	Return(write_print(ptr, stream, pos));
	Return(pprint_exit_common(ptr, stream));
	Return(write_char_stream_(stream, ' '));
	Return(pprint_newline_print_(ptr, pprint_newline_miser, stream));
	Return(pprint_indent_print_(ptr, 0, 0, stream));
	for (;;) {
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(write_print(ptr, stream, pos));
		Return(pprint_exit_common(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_linear, stream));
	}

	return 0;
}

static int pprint_dispatch_call1(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_dispatch_call2);
}

static int pprint_dispatch_call(Execute ptr, addr stream, addr list)
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
	return pprint_list_common(ptr, stream, list, p_pprint_dispatch_call1);
}


/*
 *  dispatch-defun
 */
static int pprint_dispatch_defun6(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* body */
	Return(pprint_exit_common(ptr, stream));
	for (;;) {
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(write_print(ptr, stream, pos));
		Return(pprint_exit_common(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_linear, stream));
	}

	return 0;
}

static int pprint_dispatch_defun5(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_dispatch_defun6);
}

static int pprint_dispatch_defun4(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* body */
	Return(pprint_exit_common(ptr, stream));
	for (;;) {
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(pprint_list_common(ptr, stream, pos, p_pprint_dispatch_defun5));
		Return(pprint_exit_common(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_fill, stream));
	}

	return 0;
}

static int pprint_dispatch_defun3(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_dispatch_defun4);
}

static int pprint_dispatch_defun2(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* defun */
	Return(pprint_pop_common(ptr, stream, &pos));
	Return(write_print(ptr, stream, pos));
	Return(pprint_exit_common(ptr, stream));
	Return(write_char_stream_(stream, ' '));
	Return(pprint_newline_print_(ptr, pprint_newline_miser, stream));
	/* name */
	Return(pprint_pop_common(ptr, stream, &pos));
	Return(write_print(ptr, stream, pos));
	Return(pprint_exit_common(ptr, stream));
	Return(write_char_stream_(stream, ' '));
	Return(pprint_newline_print_(ptr, pprint_newline_miser, stream));
	/* args */
	Return(pprint_pop_common(ptr, stream, &pos));
	Return(pprint_list_common(ptr, stream, pos, p_pprint_dispatch_defun3));
	/* body */
	Return(pprint_indent_print_(ptr, 1, 1, stream));
	for (;;) {
		Return(pprint_exit_common(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_linear, stream));
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(write_print(ptr, stream, pos));
	}

	return 0;
}

static int pprint_dispatch_defun1(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_dispatch_defun2);
}

static int pprint_dispatch_defun(Execute ptr, addr stream, addr list)
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
	return pprint_list_common(ptr, stream, list, p_pprint_dispatch_defun1);
}


/*
 *  dispatch-let
 */
static int pprint_dispatch_let2(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* let */
	Return(pprint_pop_common(ptr, stream, &pos));
	Return(write_print(ptr, stream, pos));
	Return(pprint_exit_common(ptr, stream));
	Return(write_char_stream_(stream, ' '));
	Return(pprint_newline_print_(ptr, pprint_newline_miser, stream));
	/* args */
	Return(pprint_pop_common(ptr, stream, &pos));
	Return(pprint_list_common(ptr, stream, pos, p_pprint_dispatch_defun3));
	/* body */
	Return(pprint_indent_print_(ptr, 1, 1, stream));
	for (;;) {
		Return(pprint_exit_common(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_linear, stream));
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(write_print(ptr, stream, pos));
	}

	return 0;
}

static int pprint_dispatch_let1(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_dispatch_let2);
}

static int pprint_dispatch_let(Execute ptr, addr stream, addr list)
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
	return pprint_list_common(ptr, stream, list, p_pprint_dispatch_let1);
}


/*
 *  initialize
 */
void init_print_function(void)
{
	/* pprint-fill */
	SetPointerType(empty, pprint_logical_block_fill_form);
	SetPointerType(empty, pprint_logical_block_fill);
	/* pprint-linear */
	SetPointerType(empty, pprint_logical_block_linear_form);
	SetPointerType(empty, pprint_logical_block_linear);
	/* pprint-tabular */
	SetPointerType(empty, pprint_logical_block_tabular_form);
	SetPointerType(empty, pprint_logical_block_tabular);
	/* dispatch-vector */
	SetPointerType(empty, pprint_dispatch_vector2);
	SetPointerType(empty, pprint_dispatch_vector1);
	SetPointerType(var2, pprint_dispatch_vector);
	/* dispatch-quote */
	SetPointerType(empty, pprint_dispatch_quote2);
	SetPointerType(empty, pprint_dispatch_quote1);
	SetPointerType(var2, pprint_dispatch_quote);
	/* dispatch-call */
	SetPointerType(empty, pprint_dispatch_call2);
	SetPointerType(empty, pprint_dispatch_call1);
	SetPointerType(var2, pprint_dispatch_call);
	/* defun */
	SetPointerType(empty, pprint_dispatch_defun6);
	SetPointerType(empty, pprint_dispatch_defun5);
	SetPointerType(empty, pprint_dispatch_defun4);
	SetPointerType(empty, pprint_dispatch_defun3);
	SetPointerType(empty, pprint_dispatch_defun2);
	SetPointerType(empty, pprint_dispatch_defun1);
	SetPointerType(var2, pprint_dispatch_defun);
	/* let */
	SetPointerType(empty, pprint_dispatch_let2);
	SetPointerType(empty, pprint_dispatch_let1);
	SetPointerType(var2, pprint_dispatch_let);
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
	return princ_print(ptr, stream, pos);
unbound:
	return print_ascii_stream_(stream, "Unbound");
}

static int method_print_clos_class_of_(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetClassOfClos(pos, &pos);
	return method_print_clos_name_(ptr, stream, pos);
}

static int method_print_object_t_body(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_CLOS);
	return method_print_clos_class_of_(ptr, stream, pos);
}

static int method_print_object_t(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	Return(print_unreadable_object_(ptr,
				stream, pos, 0, 1, method_print_object_t_body));
	setresult_control(ptr, pos);
	return 0;
}


/*
 *  class
 */
static int method_print_object_class(Execute ptr,
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
static int write_structure(Execute ptr, addr stream, addr pos)
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
	Return(write_print(ptr, stream, x));
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
		Return(princ_print(ptr, stream, z));
		Return(write_char_stream_(stream, ' '));
		GetClosValue(y, i, &z);
		if (z == Unbound) {
			Return(print_ascii_stream_(stream, "#<UNBOUND>"));
		}
		else {
			Return(write_print(ptr, stream, z));
		}
	}
	Return(write_char_stream_(stream, ')'));

	return 0;
}

static int method_print_object_structure_object(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	Return(print_structure(ptr, stream, pos));
	setresult_control(ptr, pos);
	return 0;
}

int print_structure(Execute ptr, addr stream, addr pos)
{
	return write_structure(ptr, stream, pos);
}


/*
 *  generic_function
 */
static int method_print_object_generic_function(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	addr class_of, call, name;

	Return(clos_class_of_(pos, &class_of));
	Return(stdget_class_name_(class_of, &class_of));
	Return(stdget_generic_name_(pos, &call));
	/* #<CLASS-OF CLASS-NAME> */
	Return(print_ascii_stream_(stream, "#<"));
	Return(princ_print(ptr, stream, class_of));
	Return(write_char_stream_(stream, ' '));
	GetCallName(call, &name);
	if (setfp_callname(call)) {
		Return(print_ascii_stream_(stream, "(SETF "));
		Return(prin1_print(ptr, stream, name));
		Return(write_char_stream_(stream, ')'));
	}
	else {
		Return(princ_print(ptr, stream, name));
	}
	Return(write_char_stream_(stream, '>'));
	/* result */
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
	SetPointerType(var4, method_print_object_t);
	SetPointerType(var4, method_print_object_class);
	SetPointerType(var4, method_print_object_structure_object);
	SetPointerType(var4, method_print_object_generic_function);
}

#define DefMethod_PrintObject(ptr, name, gen, p, c) { \
	Return(defmethod_print_object_((ptr), (name), (gen), \
				p_method_print_object_##p, CONSTANT_CLOS_##c)); \
}
static int build_print_object_method_(Execute ptr, addr name, addr gen)
{
	DefMethod_PrintObject(ptr, name, gen, t, T);
	DefMethod_PrintObject(ptr, name, gen, class, CLASS);
	DefMethod_PrintObject(ptr, name, gen, structure_object, STRUCTURE_OBJECT);
	DefMethod_PrintObject(ptr, name, gen, generic_function, GENERIC_FUNCTION);

	return 0;
}

int build_print_object_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_PRINT_OBJECT, &symbol);
	mop_argument_generic_var2(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
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

int pprint_throw(Execute ptr, addr stream)
{
	Return(gensym_pretty_stream_(stream, &stream));
	return throw_control_(ptr, stream);
}

int pprint_exit_common(Execute ptr, addr stream)
{
	addr pos;

	Return(root_pretty_stream_(stream, &pos));
	if (pos == Nil)
		return pprint_throw(ptr, stream);

	return 0;
}

static int pprint_pop_atom(Execute ptr, addr stream)
{
	int check;
	addr pos;

	Return(first_pretty_stream_(stream, &check));
	if (! check) {
		Return(print_ascii_stream_(stream, ". "));
	}
	Return(pop_pretty_stream_(stream, &pos, &check));
	Return(write_print(ptr, stream, pos));

	return pprint_throw(ptr, stream);
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

int pprint_pop_common(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr pos;

	Return(root_pretty_stream_(stream, &pos));
	/* atom */
	if (! listp(pos))
		return pprint_pop_atom(ptr, stream);
	/* length */
	Return(pprint_length_check_(ptr, stream, &check));
	if (check) {
		Return(print_ascii_stream_(stream, "..."));
		return pprint_throw(ptr, stream);
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
				return pprint_throw(ptr, stream);
		}
	}
	/* cons */
	return pop_pretty_stream_(stream, ret, &check);
}

int check_pretty_stream(Execute ptr, addr stream)
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
			return pprint_throw(ptr, stream);
		}
	}

	/* atom */
	Return(root_pretty_stream_(stream, &root));
	if (! listp(root)) {
		Return(write_print(ptr, stream, root));
		return pprint_throw(ptr, stream);
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
				return pprint_throw(ptr, stream);
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

static int WriteCall_error(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 0, 1, WriteBody_error_);
}

static int WriteCall_system(Execute ptr, addr stream, addr pos)
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
	Return(prin1_print(ptr, stream, pos));
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

	return callclang_funcall(ptr, &pos, generic, pos, stream, NULL);
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
	Return(prin1_print(ptr, stream, value));
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
		return callclang_funcall(ptr, &restart, restart, stream, NULL);
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
	Return(find_function_print_dispatch(ptr, pos, dispatch, &dispatch));
	if (dispatch == Nil)
		return write_default_print_(ptr, stream, pos);
	else
		return callclang_funcall(ptr, &dispatch, dispatch, stream, pos, NULL);
}

int write_print(Execute ptr, addr stream, addr pos)
{
	int check;

	gchold_push_local(ptr->local, stream);
	Return(pretty_print_(ptr, &check));
	if (check)
		return write_pretty_print_(ptr, stream, pos);
	else
		return write_default_print_(ptr, stream, pos);
}

int princ_print(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_control(ptr, &control);
	push_escape_print(ptr, 0);
	push_readably_print(ptr, 0);
	(void)write_print(ptr, stream, pos);
	return pop_control_(ptr, control);
}

int prin1_print(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_control(ptr, &control);
	push_escape_print(ptr, 1);
	(void)write_print(ptr, stream, pos);
	return pop_control_(ptr, control);
}

int print_print(Execute ptr, addr stream, addr pos)
{
	Return(terpri_stream_(stream));
	Return(prin1_print(ptr, stream, pos));
	return write_char_stream_(stream, ' ');
}

static int pprint_print_call_(Execute ptr, addr stream, addr pos)
{
	push_escape_print(ptr, 1);
	push_pretty_print(ptr, 1);
	Return(terpri_stream_(stream));
	return write_print(ptr, stream, pos);
}

int pprint_print(Execute ptr, addr stream, addr pos)
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
	Return(write_print(ptr, stream, pos));
	Return(string_stream_heap_(stream, ret));
	close_output_string_stream(stream);

	return 0;
}

int write_string_heap(Execute ptr, addr *ret, addr pos)
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
	Return(write_print(ptr, stream, pos));
	Return(string_stream_local_(ptr->local, stream, ret));
	close_output_string_stream(stream);

	return 0;
}

int write_string_local(Execute ptr, addr *ret, addr pos)
{
	addr control;

	push_control(ptr, &control);
	(void)write_string_local_call_(ptr, ret, pos);
	return pop_control_(ptr, control);
}

int princ_string_heap(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(princ_print(ptr, stream, pos));
	Return(string_stream_heap_(stream, ret));
	close_output_string_stream(stream);

	return 0;
}

int princ_string_local(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(princ_print(ptr, stream, pos));
	Return(string_stream_local_(ptr->local, stream, ret));
	close_output_string_stream(stream);

	return 0;
}

int prin1_string_heap(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(prin1_print(ptr, stream, pos));
	Return(string_stream_heap_(stream, ret));
	close_output_string_stream(stream);

	return 0;
}

int prin1_string_local(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(prin1_print(ptr, stream, pos));
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
		WriteCallTable[i] = WriteCall_error;
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
	WriteCallTable[LISPTYPE_READTABLE] = WriteCall_system;
	WriteCallTable[LISPTYPE_SYMBOL] = WriteCall_symbol_;
	WriteCallTable[LISPTYPE_FIXNUM] = WriteCall_fixnum_;
	WriteCallTable[LISPTYPE_BIGNUM] = WriteCall_bignum_;
	WriteCallTable[LISPTYPE_RATIO] = WriteCall_ratio_;
	WriteCallTable[LISPTYPE_SHORT_FLOAT] = WriteCall_error;
	WriteCallTable[LISPTYPE_SINGLE_FLOAT] = WriteCall_single_float_;
	WriteCallTable[LISPTYPE_DOUBLE_FLOAT] = WriteCall_double_float_;
	WriteCallTable[LISPTYPE_LONG_FLOAT] = WriteCall_long_float_;
	WriteCallTable[LISPTYPE_COMPLEX] = WriteCall_complex_;
	WriteCallTable[LISPTYPE_CONTROL] = WriteCall_system;
	WriteCallTable[LISPTYPE_CODE] = WriteCall_system;
	WriteCallTable[LISPTYPE_CALLNAME] = WriteCall_callname_;
	WriteCallTable[LISPTYPE_FUNCTION] = WriteCall_function_;
	WriteCallTable[LISPTYPE_INDEX] = WriteCall_index_;
	WriteCallTable[LISPTYPE_PACKAGE] = WriteCall_package_;
	WriteCallTable[LISPTYPE_RANDOM_STATE] = WriteCall_random_state_;
	WriteCallTable[LISPTYPE_PATHNAME] = WriteCall_pathname_;
	WriteCallTable[LISPTYPE_STREAM] = WriteCall_stream_;
	WriteCallTable[LISPTYPE_QUOTE] = WriteCall_quote_;
	WriteCallTable[LISPTYPE_RESTART] = WriteCall_restart_;
	WriteCallTable[LISPTYPE_EVAL] = WriteCall_system;
	WriteCallTable[LISPTYPE_ENVIRONMENT] = WriteCall_system;
	WriteCallTable[LISPTYPE_BITVECTOR] = WriteCall_bitvector_;
	WriteCallTable[LISPTYPE_PRINT_DISPATCH] = WriteCall_system;
	WriteCallTable[LISPTYPE_BYTESPEC] = WriteCall_bytespec_;

	WriteCallTable[LISPSYSTEM_CHARACTER2] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_CHARQUEUE] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_CHARBIT] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_SYMSTACK] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_BITTYPE] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_READLABEL] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_READINFO] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_READTYPE] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_BITCONS] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_BITBUFFER] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_HASHITERATOR] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_PACKAGEITERATOR] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_TAGINFO] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_ARRAY_DIMENSION] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_ARRAY_GENERAL] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_ARRAY_SPECIALIZED] = WriteCall_system;
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


/************************************************************
 *  process_arch.c
 ************************************************************/

#if defined(LISP_POSIX)

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

static int run_process_posix_(Execute ptr, addr var, addr args, addr *ret)
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
	return run_process_posix_(ptr, var, args, ret);
}

#elif defined(LISP_WINDOWS)

#include <windows.h>

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
	HANDLE child;
	DWORD status;
	LocalRoot local;

	local = ptr->local;
	if (! listp(args))
		conscar_local(local, &args, args);
	Return(run_process_list_utf16_(local, var, args, &list));
	cleartype(sinfo);
	cleartype(pinfo);
	if (! CreateProcessW(NULL, list, NULL, NULL,
				FALSE, 0, NULL, NULL, &sinfo, &pinfo)) {
		return fmte_("Cannot run process ~S.", var, NULL);
	}
	child = pinfo.hProcess;
	if (! CloseHandle(pinfo.hThread))
		return fmte_("CloseHandle error.", NULL);

	/* wait */
	WaitForSingleObject(child, INFINITE);
	if (! GetExitCodeProcess(child, &status))
		return fmte_("GetExitCodeProcess error.", NULL);
	fixnum_heap(ret, (fixnum)status); /* heap */

	return 0;
}

int run_process_arch_(Execute ptr, addr instance, addr *ret)
{
	addr var, args;

	Return(ClosGetConst_(instance, KEYWORD_PROGRAM, &var));
	Return(ClosGetConst_(instance, KEYWORD_ARGS, &args));
	return run_process_windows_(ptr, var, args, ret);
}

#else
int run_process_arch_(Execute ptr, addr instance, addr *ret)
{
	return fmte_("This implementation does not support RUN-PROGRAM.", NULL);
}
#endif


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

static void prompt_heap(addr *ret, addr value, enum prompt_mode mode)
{
	addr pos;
	struct prompt_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_PROMPT, 1, sizeoft(struct prompt_struct));
	str = PtrPromptStruct(pos);
	str->mode = mode;
	SetArraySS(pos, 0, value);
	*ret = pos;
}

void push_prompt(Execute ptr, addr value, enum prompt_mode mode)
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

void get_prompt(Execute ptr, addr *value, enum prompt_mode *mode)
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
	enum prompt_mode ignore;
	get_prompt(ptr, ret, &ignore);
}

void getmode_prompt(Execute ptr, enum prompt_mode *ret)
{
	addr ignore;
	get_prompt(ptr, &ignore, ret);
}


/************************************************************
 *  prompt_arch.c
 ************************************************************/

#ifdef LISP_PROMPT_TERME

int input_prompt_(Execute ptr, addr *ret)
{
	enum prompt_mode mode;
	addr pos;

	get_prompt(ptr, &pos, &mode);
	Return(prompt_terme_(ptr, pos, mode));
	Return(readline_terme_(ptr, ret));

	return 0;
}

#endif

#ifdef LISP_PROMPT_DISABLE

int input_prompt_(Execute ptr, addr *ret)
{
	return fmte_("input-prompt is not supported.", NULL);
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

#endif


/************************************************************
 *  prompt_for.c
 ************************************************************/

/*
 *  prompt-for
 */
static int prompt_for_call_(Execute ptr, addr io, addr *ret)
{
	addr value;

	Return(finish_output_stream_(io));
	Return(clear_input_stream_(io));
	Return(read_common_(ptr, io, T, Nil, Nil, &value));

	return Result(ret, value);
}

static int prompt_for_string_(Execute ptr, addr io, addr prompt, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	push_prompt(ptr, prompt, prompt_for);
	(void)prompt_for_call_(ptr, io, ret);
	return pop_control_(ptr, control);
}

static int prompt_for_module_(Execute ptr, LocalHold hold,
		addr io, addr type, addr prompt, addr *ret)
{
	int check;
	addr value;

	for (;;) {
		Return(prompt_for_string_(ptr, io, prompt, &value));
		localhold_set(hold, 1, value);
		if (type == T)
			break;
		Return(typep_clang_(ptr, value, type, &check));
		if (check)
			break;

		Return(format_string(ptr, &prompt, "Please answer ~A type: ", type, NULL));
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
	Return(princ_print(ptr, io, prompt));
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

		Return(format_stream(ptr, io, "~%Please answer ~A type: ", type, NULL));
		Return(finish_output_stream_(io));
	}

	return Result(ret, value);
}

int prompt_for_stream(Execute ptr, addr type, addr prompt, addr *ret)
{
	addr io;
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);

	/* type */
	if (type != T) {
		Return(parse_type(ptr, &type, type, Nil));
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
 *  yes-or-no-p
 */
static int yes_or_no_p_check1_(Execute ptr, addr io, addr pos, int *retry, int *ret)
{
	int check;

	Return(string_equalp_char_(pos, "yes", &check));
	if (check) {
		*retry = 0;
		return Result(ret, 1);
	}
	Return(string_equalp_char_(pos, "no", &check));
	if (check) {
		*retry = 0;
		return Result(ret, 0);
	}

	*retry = 1;
	return format_stream(ptr, io, "~%Please answer yes or no: ", NULL);
}

static int yes_or_no_p_check2_(Execute ptr, addr io, addr pos, int *retry, int *ret)
{
	unicode c;
	size_t size;

	string_length(pos, &size);
	if (size != 0) {
		Return(string_getc_(pos, 0, &c));
		if (toUpperUnicode(c) == 'Y') {
			*retry = 0;
			return Result(ret, 1);
		}
		if (toUpperUnicode(c) == 'N') {
			*retry = 0;
			return Result(ret, 0);
		}
	}

	*retry = 1;
	return format_stream(ptr, io, "~%Please answer y or n: ", NULL);
}

static int yes_or_no_p_check_(Execute ptr, addr io, addr pos,
		int exactp, int *retry, int *ret)
{
	if (exactp)
		return yes_or_no_p_check1_(ptr, io, pos, retry, ret);
	else
		return yes_or_no_p_check2_(ptr, io, pos, retry, ret);
}

int yes_or_no_p_common(Execute ptr, addr args, int exactp, int *ret)
{
	int miss, check;
	addr control, stream, pos;

	/* output */
	Return(query_io_stream_(ptr, &stream));
	if (args != Nil) {
		GetCons(args, &control, &args);
		Return(fresh_line_stream_(stream, NULL));
		Return(format_lisp(ptr, stream, control, args, &control));
		Return(print_ascii_stream_(stream, " "));
	}
	Return(print_ascii_stream_(stream, exactp? "(yes or no) ": "(y or n) "));
	Return(finish_output_stream_(stream));

	/* query */
	*ret = 0;
	for (;;) {
		Return(clear_input_stream_(stream));
		Return(read_line_stream_(ptr, &pos, &miss, stream, 1, Unbound, 0));
		if (pos == Unbound)
			return fmte_("*query-io* don't read yes/or question.", NULL);
		Return(yes_or_no_p_check_(ptr, stream, pos, exactp, &check, ret));
		if (! check)
			break;
		Return(finish_output_stream_(stream));
	}

	return 0;
}


/************************************************************
 *  question.c
 ************************************************************/

/*
 *  memory-stream
 */
static int question_memory_stream_size_(Execute ptr, addr pos, addr args)
{
	size_t size;

	getsize_memory_stream(pos, &size);
	make_index_integer_heap(&pos, size);
	setresult_control(ptr, pos);

	return 0;
}

static int question_memory_stream_array_(Execute ptr, addr pos, addr args)
{
	size_t size;

	getarray_memory_stream(pos, &size);
	make_index_integer_heap(&pos, size);
	setresult_control(ptr, pos);

	return 0;
}

static int question_memory_stream_cache_(Execute ptr, addr pos, addr args)
{
	int cache;

	cache = getcache_memory_stream(pos);
	fixnum_heap(&pos, (fixnum)cache);
	setresult_control(ptr, pos);

	return 0;
}

static int question_memory_stream_(Execute ptr, addr pos, addr args)
{
	int check;
	addr type;

	Return_getcar(args, &type);
	/* size */
	Return(string_designer_equalp_char_(type, "size", &check));
	if (check)
		return question_memory_stream_size_(ptr, pos, args);

	/* array */
	Return(string_designer_equalp_char_(type, "array", &check));
	if (check)
		return question_memory_stream_array_(ptr, pos, args);

	/* cache */
	Return(string_designer_equalp_char_(type, "cache", &check));
	if (check)
		return question_memory_stream_cache_(ptr, pos, args);

	/* error */
	setresult_control(ptr, Nil);
	return 0;
}


/*
 *  question
 */
int question_values_(Execute ptr, addr pos, addr args)
{
	if (memory_stream_p(pos))
		return question_memory_stream_(ptr, pos, args);

	/* error */
	setresult_control(ptr, Nil);
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

static int InitRandomState = 0;
static mutexlite RandomStateMutex;

struct random_state *struct_random_state(addr pos)
{
	CheckType(pos, LISPTYPE_RANDOM_STATE);
	return PtrBodyRandomState(pos);
}

#if defined LISP_POSIX
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

	file = open(RANDOM_DEVICE, O_RDONLY | O_NONBLOCK);
	if (file < 0) {
		Debug("file " RANDOM_DEVICE " is not exist.");
		return 1;
	}
	check = readforce_posix(file, (void *)buffer, RANDOM_DEVICE_SIZE, &size);
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
	if (InitRandomState) {
		Debug("InitRandomState error.");
		return 1;
	}
	if (lispd_make_mutexlite(&RandomStateMutex)) {
		Debug("lispd_make_mutexlite error.");
		return 1;
	}
	InitRandomState = 1;

	return 0;
}

void free_random_state(void)
{
	if (InitRandomState) {
		lispd_destroy_mutexlite(&RandomStateMutex);
		InitRandomState = 0;
	}
}

static void make_random_seed(struct md5encode *md5)
{
	volatile const void *ptr;
	void (*call)(struct md5encode *);
	static volatile int counter = 0;

	/* environment */
	random_seed_os(md5);
	/* counter */
	lispd_lock_mutexlite(&RandomStateMutex);
	counter++;
	lispd_unlock_mutexlite(&RandomStateMutex);
	readmd5(md5, &counter, sizeof(counter));
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
	bigtype value;

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
	bigtype value;

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
	bigtype *numer, *denom;
	bigtype a, b, n;

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
		int sign, bigtype numer, bigtype denom)
{
	addr num, den;

	Check(local == NULL, "local error");
	bignum_value_local(local, &num, SignPlus, numer);
	bignum_value_local(local, &den, SignPlus, denom);
	make_ratio_reduction_local(local, ret, sign, num, den);
}

void ratio_reduction_value_heap(LocalRoot local, addr *ret,
		int sign, bigtype numer, bigtype denom)
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
		int sign, bigtype numer, bigtype denom)
{
	addr num, den;

	Check(local == NULL, "local error");
	bignum_value_local(local, &num, SignPlus, numer);
	bignum_value_local(local, &den, SignPlus, denom);
	make_ratio_local(local, ret, sign, num, den);
}

void ratio_noreduction_value_heap(addr *ret,
		int sign, bigtype numer, bigtype denom)
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
	bigtype value;

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
static size_t hexfloat_bigtype_exponent(bigtype value)
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
	bigtype *data;

	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);
	Check(size == 0, "size error");
	if (size == 1) {
		size =  hexfloat_bigtype_exponent(data[0]);
	}
	else {
		size--;
		size = size * BIGNUM_FULLBIT + hexfloat_bigtype_exponent(data[size]);
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
static char *hexadecimal_bigtype(char *ptr, bigtype v, int first, size_t *bit)
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
	bigtype *data;

	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);
	Check(size == 0, "size error");
	for (i = 0; i < size; i++) {
		index = size - i - 1;
		ptr = hexadecimal_bigtype(ptr, data[index], (i == 0), &bit);
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

int equal_value_nosign_ratio(addr pos, bigtype numer, bigtype denom)
{
	addr pos1, pos2;

	CheckType(pos, LISPTYPE_RATIO);
	GetNumerRatio(pos, &pos1);
	GetDenomRatio(pos, &pos2);
	return equal_value_nosign_bignum(pos1, numer)
		&& equal_value_nosign_bignum(pos2, denom);
}

int equal_value_ratio(addr pos, int sign, bigtype numer, bigtype denom)
{
	CheckType(pos, LISPTYPE_RATIO);
	return (RefSignRatio(pos) == sign)
		&& equal_value_nosign_ratio(pos, numer, denom);
}

int equal_fr_real(addr left, addr right)
{
	int sign;
	addr pos;
	bigtype value;

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

static int compare_bigtype_bignum(bigtype left, addr right)
{
	bigtype value;

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

static int compare_bigtype_ratio_nosign(LocalRoot local, bigtype left, addr right)
{
	int result;
	addr numer, pos;
	LocalStack stack;
	size_t size;

	/* denom == 1 */
	GetNumerRatio(right, &numer);
	GetDenomRatio(right, &right);
	if (equal_value_nosign_bignum(right, 1))
		return compare_bigtype_bignum(left, numer);

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
	bigtype value;

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
	result = compare_bigtype_ratio_nosign(local, value, right);

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

static int equal_rv_nosign(addr left, bigtype value)
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
		addr left, bigtype value, addr *rnumer, addr *rdenom)
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
		int sign, addr left, bigtype value, addr *ret)
{
	addr numer, denom;

	multi_rv_result(local, left, value, &numer, &denom);
	make_ratio_reduction_local(local, ret, sign, numer, denom);
}

static void multi_rv_local(LocalRoot local,
		int sign, addr left, bigtype value, addr *ret)
{
	addr numer, denom;

	multi_rv_result(local, left, value, &numer, &denom);
	ratio_noreduction_local(local, ret, sign, numer, denom);
}

static void multi_rv_common(LocalRoot local,
		int sign, addr left, bigtype value, addr *ret)
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
	bigtype value;

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
	bigtype value;

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
	bigtype value;

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
		addr *ret, int sign, bigtype value)
{
	ratio_noreduction_value_local(local, ret, sign, 1, value);
}

static inline void inverse_value_local(LocalRoot local,
		addr *ret, int sign, bigtype value)
{
	if (value == 1)
		fixnum_local(local, ret, IsPlus(sign)? 1: -1);
	else
		ratio_noreduction_value_local(local, ret, sign, 1, value);
}

static inline void inverse_value_common(addr *ret, int sign, bigtype value)
{
	if (value == 1)
		fixnum_heap(ret, IsPlus(sign)? 1: -1);
	else
		ratio_noreduction_value_heap(ret, sign, 1, value);
}

static void div_rv_result(LocalRoot local,
		addr left, bigtype value, addr *rnumer, addr *rdenom)
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
		int sign, addr left, bigtype value, addr *ret)
{
	addr numer, denom;

	div_rv_result(local, left, value, &numer, &denom);
	make_ratio_reduction_local(local, ret, sign, numer, denom);
}

static void div_rv_local(LocalRoot local,
		int sign, addr left, bigtype value, addr *ret)
{
	addr numer, denom;

	div_rv_result(local, left, value, &numer, &denom);
	ratio_noreduction_local(local, ret, sign, numer, denom);
}

static void div_rv_common(LocalRoot local, int sign, addr left, bigtype value, addr *ret)
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
	bigtype value;

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
	bigtype value;

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
	bigtype value;

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
		bigtype value, addr right, addr *rnumer, addr *rdenom)
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
		int sign, bigtype value, addr right, addr *ret)
{
	addr numer, denom;

	div_vr_result(local, value, right, &numer, &denom);
	make_ratio_reduction_local(local, ret, sign, denom, numer);
}

static void div_vr_local(LocalRoot local,
		int sign, bigtype value, addr right, addr *ret)
{
	addr numer, denom;

	div_vr_result(local, value, right, &numer, &denom);
	ratio_noreduction_local(local, ret, sign, denom, numer);
}

static void div_vr_common(LocalRoot local,
		int sign, bigtype value, addr right, addr *ret)
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
	bigtype value;

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
	bigtype value;

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
	bigtype value;

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
	bigtype value1, value2;
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
	bigtype value1, value2;

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
	bigtype value1;
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
	bigtype value1;

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
	bigtype value2;
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
	bigtype value2;

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
	bigtype value;

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
	bigtype value;

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
		int sign, addr left, bigtype right, addr *ret)
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
		int sign, addr left, bigtype right, addr *ret)
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
		int sign, addr left, bigtype right, addr *ret)
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
		int sign, addr left, bigtype right, addr *ret)
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
		int sign, addr left, bigtype right, addr *ret)
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
		int sign, addr left, bigtype right, addr *ret)
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
		addr left, int sign2, bigtype right, addr *ret)
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
		addr left, int sign2, bigtype right, addr *ret)
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
		addr left, int sign2, bigtype right, addr *ret)
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
	bigtype value;

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
	bigtype value;

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
	bigtype value;

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
	bigtype value;

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
	bigtype value;

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
	bigtype value;

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
		addr left, int sign2, bigtype right, addr *ret, int reverse)
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
		addr left, int sign2, bigtype right, addr *ret, int reverse)
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
		addr left, int sign2, bigtype right, addr *ret, int reverse)
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
	bigtype value;

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
	bigtype value;

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
	bigtype value;

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
	bigtype value;

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
	bigtype value;

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
	bigtype value;

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
			goto illegal_error;

		default:
			goto error;
	}

step10:
	Return(maketoken_(ptr, token));
	goto final;

illegal_error:
	return fmte_("Illegal character error", NULL);

error:
	return fmte_("readtable error", NULL);

final:
	return Result(ret, ReadTable_Result_normal);
macro:
	return Result(ret, ReadTable_Result_macro);
eof:
	return Result(ret, ReadTable_Result_eof);
}

int readtable_novalue(Execute ptr, int *ret, addr *token, addr stream, addr table)
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

	Return(macro_character_execute(ptr, &check, &pos, u, stream, table));
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
		Return(readtable_novalue(ptr, &check, ret, stream, table));
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
int read_call(Execute ptr, addr stream, int *result, addr *ret)
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
	Return(read_call(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

int read_stream(Execute ptr, addr stream, int *result, addr *ret)
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
	Return(read_call(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

int read_preserving(Execute ptr, addr stream, int *result, addr *ret)
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
	Return(read_call(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

int read_recursive(Execute ptr, addr stream, int *result, addr *ret)
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

int read_from_string(Execute ptr, int *result, addr *ret, addr pos)
{
	addr stream;
	LocalHold hold;

	Return(open_input_string_stream_(&stream, pos));
	hold = LocalHold_local_push(ptr, stream);
	Return(read_stream(ptr, stream, result, ret));
	localhold_end(hold);
	close_input_string_stream(stream);

	return 0;
}

int readstring_debug(addr *ret, const char *code)
{
	int result;
	addr stream;

	open_input_char_stream(&stream, code);
	if (read_stream(Execute_Thread, stream, &result, ret))
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
	Return(double_quote_reader(ptr->local, pos, &pos));
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
	Return(single_quote_reader(ptr, pos, &pos));
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
	return parensis_open_reader(ptr, stream);
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
	return parensis_close_reader();
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
	Return(backquote_reader(ptr, stream, &code));
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
	Return(comma_reader(ptr, stream, &code));
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
	return sharp_reader(ptr, stream, code);
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
	return error_dispatch(code);
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
	Return(equal_dispatch(ptr, stream, code, arg, &code));
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
	Return(sharp_dispatch(ptr, arg, &code));
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
	Return(single_quote_dispatch(ptr, stream, &code));
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
	Return(parensis_open_dispatch(ptr, stream, arg, &code));
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
	return parensis_close_dispatch();
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
	Return(colon_dispatch(ptr, stream, &code));
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
	return less_dispatch();
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
	Return(backslash_dispatch(ptr, stream, &code));
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
	return plus_dispatch(ptr, stream);
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
	return minus_dispatch(ptr, stream);
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
	Return(dot_dispatch(ptr, stream, &code));
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
	Return(radix_dispatch(ptr, stream, arg, &code));
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
	Return(binary_dispatch(ptr, stream, &code));
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
	Return(octal_dispatch(ptr, stream, &code));
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
	Return(hexadecimal_dispatch(ptr, stream, &code));
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
	Return(complex_dispatch(ptr, stream, &code));
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
	Return(array_dispatch(ptr, stream, arg, &code));
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
	Return(pathname_dispatch(ptr, stream, &code));
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
	Return(structure_dispatch(ptr, stream, &code));
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
int double_quote_reader(LocalRoot local, addr stream, addr *ret)
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
static int quote_macro_reader(Execute ptr, constindex index, addr stream, addr *ret)
{
	int check;
	addr pos, quote;

	/* readtable */
	Return(read_recursive(ptr, stream, &check, &pos));
	if (check)
		return Result(ret, Unbound);

	/* ([index] pos) */
	GetConstant(index, &quote);
	list_heap(ret, quote, pos, NULL);

	return 0;
}

int single_quote_reader(Execute ptr, addr stream, addr *ret)
{
	Return(quote_macro_reader(ptr, CONSTANT_COMMON_QUOTE, stream, ret));
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
		Return(readtable_novalue(ptr, &check, &pos, stream, table));
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

int read_delimited_list(Execute ptr, addr stream, unicode limit, int recp)
{
	addr control;

	push_control(ptr, &control);
	(void)read_delimited_list_call_(ptr, stream, limit, recp);
	return pop_control_(ptr, control);
}

int parensis_open_reader(Execute ptr, addr stream)
{
	return read_delimited_list(ptr, stream, ')', 1);
}


/*
 *  reader )
 */
int parensis_close_reader(void)
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
	Return(read_call(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

static int backquote_read_reader(Execute ptr, addr stream, int *result, addr *ret)
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

int backquote_reader(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr pos;

	Return(backquote_read_reader(ptr, stream, &check, &pos));
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
	Return(read_call(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

static int comma_read_reader(Execute ptr, addr stream, int *result, addr *ret)
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

int comma_reader(Execute ptr, addr stream, addr *ret)
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
		Return(comma_read_reader(ptr, stream, &check, &pos));
		if (check)
			return fmte_("After ,@ must be an object.", NULL);
		quote_atsign_heap(&pos, pos);
	}
	else if (c == '.') {
		Return(comma_read_reader(ptr, stream, &check, &pos));
		if (check)
			return fmte_("After ,. must be an object.", NULL);
		quote_dot_heap(&pos, pos);
	}
	else {
		Return(unread_char_stream_(stream, c));
		Return(comma_read_reader(ptr, stream, &check, &pos));
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

int sharp_reader(Execute ptr, addr stream, addr code)
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

	return funcall_control(ptr, pos, stream, code2, arg, NULL);
}


/*****************************************************************************
 *  dispatch macro
 *****************************************************************************/
/*
 *  dispatch # whitespace
 */
/* (defun error-dispatch (stream code arg) ...) -> * */
int error_dispatch(addr code)
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
	escape = read_recursive(ptr, stream, result, ret);
	equal_finalize_dispatch(pos, value);
	return escape;
}

static int equal_read_dispatch(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	(void)equal_read_dispatch_call_(ptr, stream, result, ret);
	return pop_control_(ptr, control);
}

int equal_dispatch(Execute ptr, addr stream, addr x, addr y, addr *ret)
{
	int check;
	addr pos, label;

	Return(pushlabel_readinfo_(ptr, y, &label));
	Return(equal_read_dispatch(ptr, stream, &check, &pos));
	if (check)
		return fmte_("After dispatch character ~S must be an object.", x, NULL);
	Return(closelabel_readlabel_(ptr, label, pos));

	return Result(ret, pos);
}


/*
 *  dispatch #n#
 */
int sharp_dispatch(Execute ptr, addr y, addr *ret)
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
int single_quote_dispatch(Execute ptr, addr stream, addr *ret)
{
	Return(quote_macro_reader(ptr, CONSTANT_COMMON_FUNCTION, stream, ret));
	if (stream == Unbound)
		return fmte_("After character #' must be a function-designer, but EOF.", NULL);

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

int parensis_open_dispatch(Execute ptr, addr stream, addr y, addr *ret)
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
		Return(read_recursive(ptr, stream, &check, &pos));
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
int parensis_close_dispatch(void)
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

int colon_dispatch(Execute ptr, addr stream, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	(void)colon_dispatch_call_(ptr, stream, ret);
	return pop_control_(ptr, control);
}


/*
 *  dispatch #<
 */
int less_dispatch(void)
{
	return fmte_("Cannot read #< dispatch character.", NULL);
}


/*
 *  dispatch #\
 */
int backslash_dispatch(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr table, pos;

	Return(getreadtable_(ptr, &table));
	Return(unread_char_stream_(stream, '\\'));
	Return(read_recursive(ptr, stream, &check, &pos));
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
	Return(read_recursive(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

static int feature_read_dispatch(Execute ptr, addr stream, int *result, addr *ret)
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

static int feature_ignore_dispatch(Execute ptr, addr stream, int *result)
{
	/* (let ((*read-suppress* t)) ...) */
	addr control, symbol;

	push_control(ptr, &control);
	GetConst(SPECIAL_READ_SUPPRESS, &symbol);
	pushspecial_control(ptr, symbol, T);
	(void)read_recursive(ptr, stream, result, &stream);
	return pop_control_(ptr, control);
}

int plus_dispatch(Execute ptr, addr stream)
{
	int check;
	addr feature, form, list;
	LocalHold hold;

	/* read feature, read form */
	Return(feature_read_dispatch(ptr, stream, &check, &feature));
	if (check)
		return fmte_("After dispatch #+ must be a feature form.", NULL);

	/* check *features* */
	GetConst(SPECIAL_FEATURES, &list);
	Return(getspecialcheck_local_(ptr, list, &list));
	hold = LocalHold_local_push(ptr, feature);
	Return(feature_check_dispatch_(list, feature, &check));
	if (check) {
		Return(read_recursive(ptr, stream, &check, &form));
		localhold_end(hold);
		if (check)
			return fmte_("After dispatch #+feature must be a object.", NULL);
		setresult_control(ptr, form);
	}
	else {
		Return(feature_ignore_dispatch(ptr, stream, &check));
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
int minus_dispatch(Execute ptr, addr stream)
{
	int check;
	addr feature, form, list;
	LocalHold hold;

	/* read feature, read form */
	Return(feature_read_dispatch(ptr, stream, &check, &feature));
	if (check)
		return fmte_("After dispatch #- must be a feature form.", NULL);

	/* check *features* */
	GetConst(SPECIAL_FEATURES, &list);
	Return(getspecialcheck_local_(ptr, list, &list));
	hold = LocalHold_local_push(ptr, feature);
	Return(feature_check_dispatch_(list, feature, &check));
	if (! check) {
		Return(read_recursive(ptr, stream, &check, &form));
		localhold_end(hold);
		if (check)
			return fmte_("After dispatch #-feature must be a object.", NULL);
		setresult_control(ptr, form);
	}
	else {
		Return(feature_ignore_dispatch(ptr, stream, &check));
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
int dot_dispatch(Execute ptr, addr stream, addr *ret)
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
	Return(read_recursive(ptr, stream, &check, &eval));
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
	Return(read_recursive(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

static int radix_execute_dispatch(Execute ptr, addr stream, fixnum base,
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

static int radix_read_dispatch(Execute ptr, addr stream, fixnum base, addr *ret)
{
	int check;
	addr pos;

	Return(radix_execute_dispatch(ptr, stream, base, &check, &pos));
	if (check)
		return fmte_("After radix dispatch #<n>r must be an integer.", NULL);
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);
	if (! rationalp(pos))
		return fmte_("The radix value ~S must be an integer.", pos, NULL);

	return Result(ret, pos);
}

int radix_dispatch(Execute ptr, addr stream, addr y, addr *ret)
{
	int check;
	fixnum value;

	GetFixnum_signed(y, &value);
	if (! isBaseChar(value)) {
		Return(read_suppress_p_(ptr, &check));
		if (! check)
			return fmte_("The radix ~S must be a number between 2 and 36.", y, NULL);
	}

	return radix_read_dispatch(ptr, stream, value, ret);
}


/*
 *  dispatch #B
 */
int binary_dispatch(Execute ptr, addr stream, addr *ret)
{
	return radix_read_dispatch(ptr, stream, 2, ret);
}


/*
 *  dispatch #O
 */
int octal_dispatch(Execute ptr, addr stream, addr *ret)
{
	return radix_read_dispatch(ptr, stream, 8, ret);
}


/*
 *  dispatch #X
 */
int hexadecimal_dispatch(Execute ptr, addr stream, addr *ret)
{
	return radix_read_dispatch(ptr, stream, 16, ret);
}


/*
 *  dispatch #C
 */
int complex_dispatch(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr form, pos, real, imag;

	Return(read_recursive(ptr, stream, &check, &form));
	if (check)
		return fmte_("After complex dispatch must be a (real imag) form.", NULL);
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);
	pos = form;
	if (! consp(pos))
		goto error;
	GetCons(pos, &real, &pos);
	if (! consp(pos))
		goto error;
	GetCons(pos, &imag, &pos);
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
int array_dispatch(Execute ptr, addr stream, addr y, addr *ret)
{
	int check, ignore;
	addr form;

	Return(read_suppress_p_(ptr, &ignore));
	if (y == Nil && (! ignore))
		return fmte_("There is no rank parameter at the #<n>a dispatch.", NULL);
	Return(read_recursive(ptr, stream, &check, &form));
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
int pathname_dispatch(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr pos;

	Return(read_recursive(ptr, stream, &check, &pos));
	if (check)
		return fmte_("After #P must be a pathname-designer.", NULL);
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);
	Return(pathname_designer_heap_(ptr, pos, &pos));

	return Result(ret, pos);
}


/*
 *  dispatch #S
 */
int structure_dispatch(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr pos, rest;
	LocalHold hold;

	Return(read_recursive(ptr, stream, &check, &pos));
	if (check)
		goto error;
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);
	if (! consp(pos))
		goto error;
	GetCons(pos, &pos, &rest);

	hold = LocalHold_local_push(ptr, pos);
	Return(structure_constructor_common(ptr, pos, rest, &pos));
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
	Return(funcall_control(ptr, call, stream, code, NULL));
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

static int macro_character_call(Execute ptr, int *result, addr *ret,
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

int macro_character_execute(Execute ptr, int *result, addr *ret,
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

	Return(macro_character_call(ptr, result, ret, call, stream, code));
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
	single_float_heap(ret, FLT_EPSILON * 0.5f);
}

static void single_float_negative_epsilon(addr *ret)
{
	single_float_heap(ret, FLT_EPSILON * 0.25f);
}

void double_float_epsilon(addr *ret)
{
	double_float_heap(ret, DBL_EPSILON * 0.5);
}

void double_float_negative_epsilon(addr *ret)
{
	double_float_heap(ret, DBL_EPSILON * 0.25);
}

void long_float_epsilon(addr *ret)
{
	long_float_heap(ret, LDBL_EPSILON * 0.5L);
}

void long_float_negative_epsilon(addr *ret)
{
	long_float_heap(ret, LDBL_EPSILON * 0.25L);
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
	double_float v;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	GetDoubleFloat(pos, &v);
	v = fabs(v);
	if (v < FLT_MIN) {
		*ret = Nil;
		return call_float_underflow_va_(NULL, CONSTANT_COMMON_FLOAT, pos, NULL);
	}
	if (FLT_MAX < v) {
		*ret = Nil;
		return call_float_overflow_va_(NULL, CONSTANT_COMMON_FLOAT, pos, NULL);
	}
	single_float_heap(ret, (single_float)v);

	return 0;
}

static int single_from_long_cast_(addr *ret, addr pos)
{
	long_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, &v);
	v = fabsl(v);
	if (v < FLT_MIN) {
		*ret = Nil;
		return call_float_underflow_va_(NULL, CONSTANT_COMMON_FLOAT, pos, NULL);
	}
	if (FLT_MAX < v) {
		*ret = Nil;
		return call_float_overflow_va_(NULL, CONSTANT_COMMON_FLOAT, pos, NULL);
	}
	single_float_heap(ret, (single_float)v);

	return 0;
}

static int double_from_long_cast_(addr *ret, addr pos)
{
	long_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, &v);
	v = fabsl(v);
	if (v < DBL_MIN) {
		*ret = Nil;
		return call_float_underflow_va_(NULL, CONSTANT_COMMON_FLOAT, pos, NULL);
	}
	if (DBL_MAX < v) {
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
			if (fixnum_value < LONG_MIN || LONG_MAX < fixnum_value)
				return fmte_("Scaling factor is too large ~A.", scale, NULL);
			n = (long)fixnum_value;
			break;

		case LISPTYPE_BIGNUM:
			return fmte_("Scaling factor ~A must be a fixnum type.", scale, NULL);

		default:
			*ret = 0;
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
			*ret = 0;
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
static void rational_float_common(LocalRoot local,
		addr *ret, addr pos, int e, int sign)
{
	addr denom;

	sign = (sign < 0)? SignMinus: SignPlus;
	if (e < 0) {
		/* ratio: (/ pos (ash 1 (- e))) */
		power2_bigdata_alloc(local, &denom, (size_t)-e);
		ratio_reduction_heap(local, ret, sign, pos, denom);
	}
	else {
		/* integer: (ash pos e) */
		ash_bignum_common(local, pos, sign, (size_t)e, ret);
	}
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
	rational_float_common(local, ret, pos, e, sign);
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
	rational_float_common(local, ret, pos, e, sign);
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
	rational_float_common(local, ret, pos, e, sign);
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
	if (rationalp(pos))
		return rational_throw_heap_(pos, ret);

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


/************************************************************
 *  real_equal.c
 ************************************************************/

int plusp_realp(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			*ret = plusp_fixnum(pos);
			break;

		case LISPTYPE_BIGNUM:
			*ret = plusp_bignum(pos);
			break;

		case LISPTYPE_RATIO:
			*ret = plusp_ratio(pos);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			*ret = plusp_single_float(pos);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = plusp_double_float(pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = plusp_long_float(pos);
			break;

		default:
			*ret = 0;
			return 1;
	}

	return 0;
}

int plusp_real_(addr pos, int *ret)
{
	if (plusp_realp(pos, ret))
		return TypeError_(pos, REAL);

	return 0;
}

int minusp_realp(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			*ret = minusp_fixnum(pos);
			break;

		case LISPTYPE_BIGNUM:
			*ret = minusp_bignum(pos);
			break;

		case LISPTYPE_RATIO:
			*ret = minusp_ratio(pos);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			*ret = minusp_single_float(pos);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = minusp_double_float(pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = minusp_long_float(pos);
			break;

		default:
			*ret = 0;
			return 1;
	}

	return 0;
}

int minusp_real_(addr pos, int *ret)
{
	if (minusp_realp(pos, ret))
		return TypeError_(pos, REAL);

	return 0;
}

int zerop_real_(addr pos, int *ret)
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

		default:
			*ret = 0;
			return TypeError_(pos, REAL);
	}

	return 0;
}

int equal_fixnum_real_(addr left, addr right, int *ret)
{
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

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int equal_bignum_real_(addr left, addr right, int *ret)
{
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

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int equal_ratio_real_(LocalRoot local, addr left, addr right, int *ret)
{
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

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int equal_single_float_real_(LocalRoot local, addr left, addr right, int *ret)
{
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

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int equal_double_float_real_(LocalRoot local, addr left, addr right, int *ret)
{
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

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int equal_long_float_real_(LocalRoot local, addr left, addr right, int *ret)
{
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

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int equal_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return equal_fixnum_real_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return equal_bignum_real_(left, right, ret);

		case LISPTYPE_RATIO:
			return equal_ratio_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return equal_single_float_real_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_double_float_real_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return equal_long_float_real_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, REAL);
	}
}

int not_equal_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(equal_real_(local, left, right, &check));
	return Result(ret, !check);
}

static int compare_fixnum_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = compare_ff_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			*ret = compare_fb_real(left, right);
			break;

		case LISPTYPE_RATIO:
			*ret = compare_fr_real(local, left, right);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			*ret = compare_fs_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = compare_fd_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = compare_fl_real(left, right);
			break;

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

static int compare_bignum_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = compare_bf_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			*ret = compare_bb_real(left, right);
			break;

		case LISPTYPE_RATIO:
			*ret = compare_br_real(local, left, right);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return compare_bs_real_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_bd_real_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return compare_bl_real_(left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int compare_ratio_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = compare_rf_real(local, left, right);
			break;

		case LISPTYPE_BIGNUM:
			*ret = compare_rb_real(local, left, right);
			break;

		case LISPTYPE_RATIO:
			*ret = compare_rr_real(local, left, right);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return compare_rs_real_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_rd_real_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return compare_rl_real_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

static int compare_single_float_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = compare_sf_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			return compare_sb_real_(left, right, ret);

		case LISPTYPE_RATIO:
			return compare_sr_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = compare_ss_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = compare_sd_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = compare_sl_real(left, right);
			break;

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

static int compare_double_float_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = compare_df_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			return compare_db_real_(left, right, ret);

		case LISPTYPE_RATIO:
			return compare_dr_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = compare_ds_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = compare_dd_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = compare_dl_real(left, right);
			break;

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

static int compare_long_float_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = compare_lf_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			return compare_lb_real_(left, right, ret);

		case LISPTYPE_RATIO:
			return compare_lr_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = compare_ls_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = compare_ld_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = compare_ll_real(left, right);
			break;

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int compare_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return compare_fixnum_real_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return compare_bignum_real_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return compare_ratio_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return compare_single_float_real_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_double_float_real_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return compare_long_float_real_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, REAL);
	}
}

int less_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_real_(local, left, right, &check));
	return Result(ret, check < 0);
}

int less_equal_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_real_(local, left, right, &check));
	return Result(ret, check <= 0);
}

int greater_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_real_(local, left, right, &check));
	return Result(ret, check > 0);
}

int greater_equal_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_real_(local, left, right, &check));
	return Result(ret, check >= 0);
}


/*
 *  debug
 */
int plusp_real_debug(addr pos)
{
	int check;

	Check(! realp(pos), "type error");
	check = 0;
	Error(plusp_real_(pos, &check));

	return check;
}

int minusp_real_debug(addr pos)
{
	int check;

	Check(! realp(pos), "type error");
	check = 0;
	Error(minusp_real_(pos, &check));

	return check;
}

int zerop_real_debug(addr pos)
{
	int check;

	Check(! realp(pos), "type error");
	check = 0;
	Error(zerop_real_(pos, &check));

	return check;
}

int equal_fixnum_real_debug(addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(equal_fixnum_real_(left, right, &check));

	return check;
}

int equal_bignum_real_debug(addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(equal_bignum_real_(left, right, &check));

	return check;
}

int equal_ratio_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(equal_ratio_real_(local, left, right, &check));

	return check;
}

int equal_single_float_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(equal_single_float_real_(local, left, right, &check));

	return check;
}

int equal_double_float_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(equal_double_float_real_(local, left, right, &check));

	return check;
}

int equal_long_float_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(equal_long_float_real_(local, left, right, &check));

	return check;
}

int equal_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(equal_real_(local, left, right, &check));

	return check;
}

int less_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(less_real_(local, left, right, &check));

	return check;
}

int less_equal_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(less_equal_real_(local, left, right, &check));

	return check;
}

int greater_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(greater_real_(local, left, right, &check));

	return check;
}

int greater_equal_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(greater_equal_real_(local, left, right, &check));

	return check;
}


/************************************************************
 *  real_floor.c
 ************************************************************/

/*
 *  common
 */
static int floor1_float_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	Return(float_floor1_s_(v, &v, &r));
	single_float_integer_heap(local, quot, v);
	single_float_heap(rem, r);

	return 0;
}

static int floor1_double_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	Return(float_floor1_d_(v, &v, &r));
	double_float_integer_heap(local, quot, v);
	double_float_heap(rem, r);

	return 0;
}

static int floor1_long_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	Return(float_floor1_l_(v, &v, &r));
	long_float_integer_heap(local, quot, v);
	long_float_heap(rem, r);

	return 0;
}

int floor1_common_(LocalRoot local, addr *quot, addr *rem, addr left)
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
			return float_floor1_ratio_(local, quot, rem, left);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return floor1_float_(local, quot, rem, left);

		case LISPTYPE_DOUBLE_FLOAT:
			return floor1_double_(local, quot, rem, left);

		case LISPTYPE_LONG_FLOAT:
			return floor1_long_(local, quot, rem, left);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}

	return 0;
}

static int ffloor1_float_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	Return(float_floor1_s_(v, &v, &r));
	single_float_heap(quot, v);
	single_float_heap(rem, r);

	return 0;
}

static int ffloor1_double_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	Return(float_floor1_d_(v, &v, &r));
	double_float_heap(quot, v);
	double_float_heap(rem, r);

	return 0;
}

static int ffloor1_long_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	Return(float_floor1_l_(v, &v, &r));
	long_float_heap(quot, v);
	long_float_heap(rem, r);

	return 0;
}

int ffloor1_common_(LocalRoot local, addr *quot, addr *rem, addr left)
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
			return float_ffloor1_ratio_(local, quot, rem, left);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ffloor1_float_(local, quot, rem, left);

		case LISPTYPE_DOUBLE_FLOAT:
			return ffloor1_double_(local, quot, rem, left);

		case LISPTYPE_LONG_FLOAT:
			return ffloor1_long_(local, quot, rem, left);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}

	return 0;
}

static int floor_ff_common_(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return float_floor_fixnum_(quot, rem, a, b);
}

static int floor_fb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_floor_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int floor_fr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_floor_br_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int floor_fs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	Return(float_floor_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int floor_fd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	Return(float_floor_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int floor_fl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int floor_fixnum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return floor_ff_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return floor_fb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return floor_fr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return floor_fs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return floor_fd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return floor_fl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int floor_bf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_floor_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int floor_bs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &a));
	b = RefSingleFloat(right);
	Return(float_floor_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int floor_bd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_floor_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int floor_bl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &a));
	b = RefLongFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int floor_bignum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return floor_bf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_floor_bignum_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_floor_br_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return floor_bs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return floor_bd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return floor_bl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int floor_rf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_floor_rb_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int floor_rs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_ratio_(left, &a));
	b = RefSingleFloat(right);
	Return(float_floor_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int floor_rd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_ratio_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_floor_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int floor_rl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_ratio_(left, &a));
	b = RefLongFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int floor_ratio_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return floor_rf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_floor_rb_ratio_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_floor_rr_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return floor_rs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return floor_rd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return floor_rl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int floor_sf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	Return(float_floor_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int floor_sb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	Return(single_float_bignum_(right, &b));
	Return(float_floor_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int floor_sr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	Return(single_float_ratio_(right, &b));
	Return(float_floor_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int floor_ss_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	Return(float_floor_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int floor_sd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_floor_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int floor_sl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int floor_single_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return floor_sf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return floor_sb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return floor_sr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return floor_ss_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return floor_sd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return floor_sl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int floor_df_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	Return(float_floor_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int floor_db_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	Return(double_float_bignum_(right, &b));
	Return(float_floor_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int floor_dr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	Return(double_float_ratio_(right, &b));
	Return(float_floor_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int floor_ds_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	Return(float_floor_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int floor_dd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_floor_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int floor_dl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int floor_double_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return floor_df_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return floor_db_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return floor_dr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return floor_ds_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return floor_dd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return floor_dl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int floor_lf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int floor_lb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	Return(long_float_bignum_(right, &b));
	Return(float_floor_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int floor_lr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	Return(long_float_ratio_(right, &b));
	Return(float_floor_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int floor_ls_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int floor_ld_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int floor_ll_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int floor_long_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return floor_lf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return floor_lb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return floor_lr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return floor_ls_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return floor_ld_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return floor_ll_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

int floor2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return floor_fixnum_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return floor_bignum_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return floor_ratio_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return floor_single_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return floor_double_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return floor_long_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}
}

static int ffloor_ff_common_(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return float_ffloor_fixnum_(quot, rem, a, b);
}

static int ffloor_fb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_ffloor_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ffloor_fr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_ffloor_br_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ffloor_fs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	Return(float_floor_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ffloor_fd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	Return(float_floor_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ffloor_fl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ffloor_fixnum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ffloor_ff_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ffloor_fb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ffloor_fr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ffloor_fs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ffloor_fd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ffloor_fl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ffloor_bf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_ffloor_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ffloor_bs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &a));
	b = RefSingleFloat(right);
	Return(float_floor_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ffloor_bd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_floor_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ffloor_bl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &a));
	b = RefLongFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ffloor_bignum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ffloor_bf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_ffloor_bignum_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_ffloor_br_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ffloor_bs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ffloor_bd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ffloor_bl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ffloor_rf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_ffloor_rb_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ffloor_rs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_ratio_(left, &a));
	b = RefSingleFloat(right);
	Return(float_floor_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ffloor_rd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_ratio_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_floor_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ffloor_rl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_ratio_(left, &a));
	b = RefLongFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ffloor_ratio_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ffloor_rf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_ffloor_rb_ratio_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_ffloor_rr_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ffloor_rs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ffloor_rd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ffloor_rl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ffloor_sf_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	Return(float_floor_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ffloor_sb_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	Return(single_float_bignum_(right, &b));
	Return(float_floor_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ffloor_sr_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	Return(single_float_ratio_(right, &b));
	Return(float_floor_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ffloor_ss_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	Return(float_floor_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ffloor_sd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_floor_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ffloor_sl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ffloor_single_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ffloor_sf_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ffloor_sb_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ffloor_sr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ffloor_ss_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ffloor_sd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ffloor_sl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ffloor_df_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	Return(float_floor_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ffloor_db_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	Return(double_float_bignum_(right, &b));
	Return(float_floor_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ffloor_dr_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	Return(double_float_ratio_(right, &b));
	Return(float_floor_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ffloor_ds_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	Return(float_floor_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ffloor_dd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_floor_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ffloor_dl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ffloor_double_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ffloor_df_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ffloor_db_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ffloor_dr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ffloor_ds_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ffloor_dd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ffloor_dl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ffloor_lf_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ffloor_lb_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	Return(long_float_bignum_(right, &b));
	Return(float_floor_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ffloor_lr_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	Return(long_float_ratio_(right, &b));
	Return(float_floor_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ffloor_ls_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ffloor_ld_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ffloor_ll_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	Return(float_floor_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ffloor_long_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ffloor_lf_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ffloor_lb_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ffloor_lr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ffloor_ls_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ffloor_ld_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ffloor_ll_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

int ffloor2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return ffloor_fixnum_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ffloor_bignum_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ffloor_ratio_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ffloor_single_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ffloor_double_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ffloor_long_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}
}

int floor_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		return floor1_common_(local, ret1, ret2, var);
	else
		return floor2_common_(local, ret1, ret2, var, div);
}

int ffloor_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		return ffloor1_common_(local, ret1, ret2, var);
	else
		return ffloor2_common_(local, ret1, ret2, var, div);
}


/*
 *  mod
 */
static int mod_ff_common_(addr *ret, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return float_mod_fixnum_(ret, a, b);
}

static int mod_fb_common_(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_mod_bignum_(local, ret, left, right));
	rollback_local(local, stack);

	return 0;
}

static int mod_fr_common_(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_mod_br_ratio_(local, ret, left, right));
	rollback_local(local, stack);

	return 0;
}

static int mod_bf_common_(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_mod_bignum_(local, ret, left, right));
	rollback_local(local, stack);

	return 0;
}

static int mod_rf_common_(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_mod_rb_ratio_(local, ret, left, right));
	rollback_local(local, stack);

	return 0;
}

static int mod_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return mod_ff_common_(ret, left, right);

		case LISPTYPE_BIGNUM:
			return mod_fb_common_(local, ret, left, right);

		case LISPTYPE_RATIO:
			return mod_fr_common_(local, ret, left, right);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int mod_bignum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return mod_bf_common_(local, ret, left, right);

		case LISPTYPE_BIGNUM:
			return float_mod_bignum_(local, ret, left, right);

		case LISPTYPE_RATIO:
			return float_mod_br_ratio_(local, ret, left, right);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int mod_ratio_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return mod_rf_common_(local, ret, left, right);

		case LISPTYPE_BIGNUM:
			return float_mod_rb_ratio_(local, ret, left, right);

		case LISPTYPE_RATIO:
			return float_mod_rr_ratio_(local, ret, left, right);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int mod_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return mod_fixnum_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return mod_bignum_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return mod_ratio_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}


/************************************************************
 *  real_multi.c
 ************************************************************/

/*
 *  multiple
 */
int multi_fixnum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_fs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_fd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_fl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_bignum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_bs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_bd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_bl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_ratio_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
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

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_rs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_rd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_rl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_single_real_common_(LocalRoot local, addr left, addr right, addr *ret)
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

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_double_real_common_(LocalRoot local, addr left, addr right, addr *ret)
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

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_long_real_common_(LocalRoot local, addr left, addr right, addr *ret)
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

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return multi_fixnum_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_bignum_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_ratio_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_single_real_common_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_double_real_common_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_long_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int multi_fixnum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_fs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_fd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_fl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_bignum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_bs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_bd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_bl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_ratio_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
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

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_rs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_rd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_rl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_single_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_sf_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_sb_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_sr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_ss_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_sd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_sl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_double_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_df_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_db_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_dr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_ds_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_dd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_dl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_long_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_lf_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_lb_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_lr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_ls_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_ld_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_ll_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return multi_fixnum_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_bignum_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_ratio_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_single_real_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_double_real_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_long_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}


/*
 *  division
 */
int div_fixnum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
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

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_FIXNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_ff_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bf_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rf_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sf_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_df_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_lf_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_bignum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
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

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_bignum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_BIGNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fb_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rb_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sb_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_db_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_lb_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_ratio_real_common_(LocalRoot local, addr left, addr right, addr *ret)
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

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_ratio_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_RATIO);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fr_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_br_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rr_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sr_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dr_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_lr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_single_real_common_(addr left, addr right, addr *ret)
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

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_single_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fs_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bs_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rs_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ss_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_ds_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ls_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_double_real_common_(addr left, addr right, addr *ret)
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

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_double_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fd_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bd_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rd_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sd_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ld_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_long_real_common_(addr left, addr right, addr *ret)
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

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_long_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_LONG_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fl_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bl_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rl_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sl_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dl_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ll_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fixnum_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bignum_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_ratio_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_single_real_common_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_double_real_common_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_long_real_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_fixnum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_ff_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_fb_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_fr_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_fs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_fd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_fl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_fixnum_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_FIXNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_ff_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bf_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rf_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sf_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_df_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_lf_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_bignum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_bf_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bb_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_br_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_bs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_bd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_bl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_bignum_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_BIGNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fb_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bb_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rb_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sb_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_db_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_lb_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_ratio_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_rf_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_rb_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rr_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_rs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_rd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_rl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_ratio_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_RATIO);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fr_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_br_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rr_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sr_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dr_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_lr_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_single_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_sf_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_sb_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_sr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ss_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_sd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_sl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_single_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fs_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bs_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rs_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ss_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_ds_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ls_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_double_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_df_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_db_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_dr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ds_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_dl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_double_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fd_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bd_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rd_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sd_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ld_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_long_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_lf_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_lb_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_lr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ls_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_ld_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ll_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_long_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_LONG_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fl_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bl_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rl_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sl_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dl_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ll_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fixnum_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bignum_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_ratio_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_single_real_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_double_real_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_long_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#ifdef __clang__
#pragma clang diagnostic pop
#endif
