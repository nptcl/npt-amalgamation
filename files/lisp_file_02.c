/*
 *  npt -- ANSI Common Lisp Programming Language.
 *    https://github.com/nptcl/npt
 *    https://github.com/nptcl/npt-amalgamation
 *
 *  File: lisp_file_02.c
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

#include <complex.h>
#include <errno.h>
#include <math.h>
#include <stdarg.h>
#include <string.h>

#include "lisp_file.h"


/************************************************************
 *  call_packages.c
 ************************************************************/

/*
 *  export
 */
int export_common_(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return export_package_(pg, symbols);
}


/*
 *  find-package
 */
int find_symbol_common_(Execute ptr, addr name, addr pg, addr *ret, addr *state)
{
	enum PACKAGE_TYPE type;

	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}
	Return(find_symbol_package_(pg, name, &name, &type));
	if (type == PACKAGE_TYPE_NIL) {
		*ret = Nil;
		*state = Nil;
	}
	else {
		*ret = name;
		keyword_packagetype(type, state);
	}

	return 0;
}


/*
 *  import
 */
int import_common_(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}
	return import_package_(pg, symbols);
}


/*
 *  rename-package
 */
int rename_package_common_(Execute ptr, addr pg, addr name, addr names, addr *ret)
{
	if (names == Unbound)
		names = Nil;

	return rename_package_(pg, name, names, ret);
}


/*
 *  shadow
 */
int shadow_common_(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return shadow_package_(pg, symbols);
}


/*
 *  shadowing-import
 */
int shadowing_import_common_(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return shadowing_import_package_(pg, symbols);
}


/*
 *  make-package
 */
int make_package_common_(Execute ptr, addr name, addr rest, addr *ret)
{
	addr nicknames, use;

	/* &key */
	if (GetKeyArgs(rest, KEYWORD_NICKNAMES, &nicknames)) {
		nicknames = Nil;
	}
	if (GetKeyArgs(rest, KEYWORD_USE, &use)) {
		GetConst(PACKAGE_DEFAULT_USE, &use);
	}
	/* make-package */
	return make_package_(ptr, name, nicknames, use, ret);
}


/*
 *  with-package-iterator
 */
static int with_package_iterator_expand_common_(Execute ptr,
		addr name, addr table, int inter, int exter, int inherit,
		addr body, addr *ret)
{
	/* (let ((inst (make-package-iterator table inter exter inherit)))
	 *   (declare (ignorable inst))
	 *   (macrolet ((name () (list (quote next-package-iterator) (quote inst))))
	 *     (declare (ignorable name))
	 *     . body))
	 */
	addr let, declare, ignorable, macrolet, list, quote, make, next;
	addr inst, a, b, c, let1, let2, let3;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MACROLET, &macrolet);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_MAKE_PACKAGE_ITERATOR, &make);
	GetConst(SYSTEM_NEXT_PACKAGE_ITERATOR, &next);
	Return(make_gensym_(ptr, &inst));

	a = inter? T: Nil;
	b = exter? T: Nil;
	c = inherit? T: Nil;
	list_heap(&let1, make, table, a, b, c, NULL);
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

static int with_package_iterator_check_common_(addr rest,
		int *inter, int *exter, int *inherit)
{
	int a, b, c;
	addr list, type, key1, key2, key3;

	a = b = c = 0;
	GetConst(KEYWORD_INTERNAL, &key1);
	GetConst(KEYWORD_EXTERNAL, &key2);
	GetConst(KEYWORD_INHERITED, &key3);
	for (list = rest; list != Nil; ) {
		if (! consp_getcons(list, &type, &list)) {
			return call_simple_package_error_va_(NULL,
					"with-package-iterator symbol-types ~S must be "
					":internal, :external, :inherit list.", rest, NULL);
		}
		if (type == key1)
			a = 1;
		else if (type == key2)
			b = 1;
		else if (type == key3)
			c = 1;
		else {
			return call_simple_package_error_va_(NULL,
					"with-package-iterator symbol-type ~S must be a "
					":internal, :external or :inherit value.", type, NULL);
		}
	}
	if (a == 0 && b == 0 && c == 0) {
		return call_simple_package_error_va_(NULL,
				"with-package-iterator symbol-types ~S must be at least "
				":internal, :external :inherit value.", rest, NULL);
	}
	*inter = a;
	*exter = b;
	*inherit = c;

	return 0;
}

int with_package_iterator_common_(Execute ptr, addr form, addr env, addr *ret)
{
	int inter, exter, inherit;
	addr args, name, list, check;

	/* args */
	Return_getcdr(form, &args);
	if (! consp_getcons(args, &name, &args))
		goto error;
	if (! consp_getcons(name, &name, &list))
		goto error;
	if (! consp_getcons(list, &list, &check))
		goto error;
	inter = exter = inherit = 0;
	Return(with_package_iterator_check_common_(check, &inter, &exter, &inherit));
	/* ((name list &rest ...) . args) */
	return with_package_iterator_expand_common_(ptr,
			name, list, inter, exter, inherit, args, ret);

error:
	return fmte_("with-package-iterator form ~S must be "
			"((name package &rest type) &body body)" , form, NULL);
}


/*
 *  unexport
 */
int unexport_common_(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return unexport_package_(pg, symbols);
}


/*
 *  unintern
 */
int unintern_common_(Execute ptr, addr symbol, addr pg, addr *ret)
{
	int check;

	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}
	Return(unintern_package_(pg, symbol, &check));
	return Result(ret, check? T: Nil);
}


/*
 *  in-package
 */
int in_package_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, name, symbol, quote;

	/* argument */
	Return_getcdr(form, &args);
	if (! consp_getcons(args, &name, &args))
		goto error;
	if (args != Nil)
		goto error;
	if (! string_designator_p(name))
		goto error;

	/* toplevel */
	if (toplevelp_eval(ptr)) {
		Return(in_package_(ptr, name, NULL));
	}

	/* in-package `(lisp-system::in-package ',name) */
	GetConst(SYSTEM_IN_PACKAGE, &symbol);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&name, quote, name, NULL);
	list_heap(ret, symbol, name, NULL);
	return 0;

error:
	return fmte_("in-package ~S must be (string-designator) form.", form, NULL);
}


/*
 *  unuse-package
 */
int unuse_package_common_(Execute ptr, addr unuse, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return unuse_package_(pg, unuse);
}


/*
 *  use-package
 */
int use_package_common_(Execute ptr, addr use, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return use_package_(pg, use);
}


/*
 *  do-symbols
 */
static int do_symbols_const_common_(addr form, addr *ret, constindex index)
{
	/* `(block nil
	 *    (system::do-symbols
	 *      (lambda (,var)
	 *        (declare (ignorable ,var))
	 *        ,@decl
	 *        (tagbody ,@body))
	 *      ,package)
	 *    (let (,var)
	 *      (declare (ignorable ,var)
	 *      ,result)))
	 */
	addr check, var, list, package, result, decl, body;
	addr declare, ignorable, lambda, tagbody, block, let;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &check, &body))
		goto error;
	if (! consp_getcons(check, &var, &check))
		goto error;
	if (check == Nil) {
		GetConst(SPECIAL_PACKAGE, &package);
		result = Nil;
		goto expand;
	}
	if (! consp_getcons(check, &package, &check))
		goto error;
	if (check == Nil) {
		result = Nil;
		goto expand;
	}
	if (! consp_getcons(check, &result, &check))
		goto error;
	if (check != Nil)
		goto error;

expand:
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_LET, &let);
	list_heap(&ignorable, ignorable, var, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	Return(declare_body_form_(body, &decl, &body));
	cons_heap(&tagbody, tagbody, body);
	conscar_heap(&tagbody, tagbody);
	Return(nconc2_safe_(decl, tagbody, &tagbody));
	conscar_heap(&list, var);
	lista_heap(&lambda, lambda, list, declare, tagbody, NULL);
	GetConstant(index, &check);
	list_heap(&check, check, lambda, package, NULL);
	list_heap(&result, let, list, declare, result, NULL);
	list_heap(ret, block, Nil, check, result, NULL);
	return 0;

error:
	GetConstant(index, &check);
	return fmte_("The ~A ~S must be "
			"((var &optional package result) &rest body) form.", check, form, NULL);
}

int do_symbols_common_(addr form, addr env, addr *ret)
{
	return do_symbols_const_common_(form, ret, CONSTANT_SYSTEM_DO_SYMBOLS);
}


/*
 *  do-external-symbols
 */
int do_external_symbols_common_(addr form, addr env, addr *ret)
{
	return do_symbols_const_common_(form, ret, CONSTANT_SYSTEM_DO_EXTERNAL_SYMBOLS);
}


/*
 *  do-all-symbols
 */
int do_all_symbols_common_(addr form, addr env, addr *ret)
{
	/* `(block nil
	 *    (system::do-all-symbols
	 *      (lambda (,var)
	 *        (declare (ignorable ,var))
	 *        ,@decl
	 *        (tagbody ,@body)))
	 *    (let (,var)
	 *      (declare (ignorable ,var)
	 *      ,result)))
	 */
	addr check, var, list, result, decl, body;
	addr declare, ignorable, lambda, tagbody, block, let;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &check, &body))
		goto error;
	if (! consp_getcons(check, &var, &check))
		goto error;
	if (check == Nil) {
		result = Nil;
		goto expand;
	}
	if (! consp_getcons(check, &result, &check))
		goto error;
	if (check == Nil) {
		goto expand;
	}
	if (check != Nil)
		goto error;
expand:
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_LET, &let);
	list_heap(&ignorable, ignorable, var, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	Return(declare_body_form_(body, &decl, &body));
	cons_heap(&tagbody, tagbody, body);
	conscar_heap(&tagbody, tagbody);
	Return(nconc2_safe_(decl, tagbody, &tagbody));
	conscar_heap(&list, var);
	lista_heap(&lambda, lambda, list, declare, tagbody, NULL);
	GetConst(SYSTEM_DO_ALL_SYMBOLS, &check);
	list_heap(&check, check, lambda, NULL);
	list_heap(&result, let, list, declare, result, NULL);
	list_heap(ret, block, Nil, check, result, NULL);
	return 0;

error:
	GetConst(SYSTEM_DO_ALL_SYMBOLS, &check);
	return fmte_("The ~A ~S must be "
			"((var &optional package result) &rest body) form.", check, form, NULL);
}


/*
 *  intern
 */
int intern_common_(Execute ptr, addr name, addr pg, addr *ret, addr *sec)
{
	enum PACKAGE_TYPE type;

	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}
	Return(intern_package_(pg, name, &name, &type));
	if (name == Nil) {
		*ret = Nil;
		*sec = Nil;
	}
	else {
		*ret = name;
		keyword_packagetype(type, sec);
	}

	return 0;
}


/************************************************************
 *  call_printer.c
 ************************************************************/

/*
 *  formatter
 */
static int formatter_call_common_(Execute ptr, addr stream, addr args)
{
	addr format;

	getdata_control(ptr, &format);
	Return(format_execute_(ptr, stream, format, args, &args));
	setresult_control(ptr, args);

	return 0;
}

int formatter_common_(LocalRoot local, addr var, addr env, addr *ret)
{
	addr pos, type;

	/* macro */
	Return_getcdr(var, &var);
	if (! singlep(var))
		goto error;
	GetCar(var, &var);
	if (! stringp(var))
		goto error;
	/* function */
	compiled_heap(&pos, Nil);
	setcompiled_var1dynamic(pos, p_formatter_call_common);
	GetTypeCompiled(&type, FormatterFunction);
	settype_function(pos, type);
	/* format */
	Return(format_parse_heap_(local, &var, var));
	SetDataFunction(pos, var);
	/* result */
	return Result(ret, pos);

error:
	return fmte_("FORMATTER argument must be a (FORMATTER string) form.", NULL);
}


/*
 *  pprint-fill
 */
int pprint_fill_common_(Execute ptr, addr stream, addr list, addr colon)
{
	if (colon == Unbound)
		colon = T;
	Return(output_stream_designator_(ptr, stream, &stream));
	return pprint_fill_print_(ptr, stream, list, colon != Nil);
}


/*
 *  pprint-linear
 */
int pprint_linear_common_(Execute ptr, addr stream, addr list, addr colon)
{
	if (colon == Unbound)
		colon = T;
	Return(output_stream_designator_(ptr, stream, &stream));
	return pprint_linear_print_(ptr, stream, list, colon != Nil);
}


/*
 *  pprint-tabular
 */
int pprint_tabular_common_(Execute ptr,
		addr stream, addr list, addr colon, addr tabsize)
{
	fixnum size;

	if (colon == Unbound)
		colon = T;
	if (tabsize == Unbound) {
		size = 16;
	}
	else {
		Return(getfixnum_unsigned_(tabsize, &size));
	}
	Return(output_stream_designator_(ptr, stream, &stream));

	return pprint_tabular_print_(ptr, stream, list, colon != Nil, size);
}


/*
 *  pprint-indent
 */
int pprint_indent_common_(Execute ptr, addr rel, addr n, addr stream)
{
	int block_p;
	addr block, current;
	fixnum value;

	Return(output_stream_designator_(ptr, stream, &stream));
	GetConst(KEYWORD_BLOCK, &block);
	GetConst(KEYWORD_CURRENT, &current);
	if (rel == block) {
		block_p = 1;
	}
	else if (rel == current) {
		block_p = 0;
	}
	else {
		return fmte_("The first argument ~S "
				"must be a (MEMBER :BLOCK :CURRENT).", rel, NULL);
	}
	if (! fixnump(n))
		return fmte_("Too large indent value ~S.", n, NULL);
	GetFixnum(n, &value);

	return pprint_indent_print_(ptr, block_p, value, stream);
}


/*
 *  pprint-logical-block
 */
int pprint_logical_block_common_(addr form, addr env, addr *ret)
{
	addr args, list, stream, pos, decl;
	addr key, key1, key2, key3, value, value1, value2, value3;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &list, &args))
		goto error;
	/* first argument */
	if (! consp_getcons(list, &stream, &list))
		goto error;
	if (! consp_getcons(list, &pos, &list))
		goto error;
	GetConst(KEYWORD_PREFIX, &key1);
	GetConst(KEYWORD_PER_LINE_PREFIX, &key2);
	GetConst(KEYWORD_SUFFIX, &key3);
	value1 = value2 = value3 = Unbound;
	while (list != Nil) {
		Return_getcons(list, &key, &list);
		Return_getcons(list, &value, &list);
		if (key == key1) {
			if (value1 == Unbound)
				value1 = value;
		}
		else if (key == key2) {
			if (value2 == Unbound)
				value2 = value;
		}
		else if (key == key3) {
			if (value3 == Unbound)
				value3 = value;
		}
		else {
			goto error;
		}
	}
	/* result */
	Return(declare_body_form_(args, &decl, &args));
	if (value1 == Unbound)
		value1 = Nil;
	if (value2 == Unbound)
		value2 = Nil;
	if (value3 == Unbound)
		value3 = Nil;
	return expand_pprint_logical_block_common_(ret,
			stream, pos, value1, value2, value3, decl, args);

error:
	return fmte_("PPRINT-LOGICAL-BLOCK form ~S must be "
			"((stream object &key prefix per-line-prefix suffix) "
			"declaration* form*)", form, NULL);
}


/*
 *  pprint-newline
 */
static int pprint_newline_symbol_common_(addr kind, enum pprint_newline *ret)
{
	addr check;

	GetConst(KEYWORD_LINEAR, &check);
	if (check == kind)
		return Result(ret, pprint_newline_linear);
	GetConst(KEYWORD_FILL, &check);
	if (check == kind)
		return Result(ret, pprint_newline_fill);
	GetConst(KEYWORD_MISER, &check);
	if (check == kind)
		return Result(ret, pprint_newline_miser);
	GetConst(KEYWORD_MANDATORY, &check);
	if (check == kind)
		return Result(ret, pprint_newline_mandatory);

	/* error */
	*ret = pprint_newline_linear;
	return fmte_("PPRINT-NEWLINE first argument ~S must be "
			"(member :linear :fill :miser :mandatory)", kind, NULL);
}

int pprint_newline_common_(Execute ptr, addr kind, addr stream)
{
	enum pprint_newline value;

	Return(pprint_newline_symbol_common_(kind, &value));
	Return(output_stream_designator_(ptr, stream, &stream));

	return pprint_newline_print_(ptr, value, stream);
}


/*
 *  pprint-tab
 */
static int pprint_tab_symbol_common_(addr kind, enum pprint_tabular *ret)
{
	addr check;

	GetConst(KEYWORD_LINE, &check);
	if (check == kind)
		return Result(ret, pprint_tabular_line);
	GetConst(KEYWORD_SECTION, &check);
	if (check == kind)
		return Result(ret, pprint_tabular_section);
	GetConst(KEYWORD_LINE_RELATIVE, &check);
	if (check == kind)
		return Result(ret, pprint_tabular_line_relative);
	GetConst(KEYWORD_SECTION_RELATIVE, &check);
	if (check == kind)
		return Result(ret, pprint_tabular_section_relative);

	/* error */
	*ret = pprint_tabular_line;
	return fmte_("PPRINT-TAB first argument ~S must be "
			"(member :line :section :line-relative :section-relative)", kind, NULL);
}

int pprint_tab_common_(Execute ptr, addr kind, addr column, addr colinc, addr stream)
{
	enum pprint_tabular value;
	fixnum a, b;

	Return(pprint_tab_symbol_common_(kind, &value));
	Return(getfixnum_unsigned_(column, &a));
	Return(getfixnum_unsigned_(colinc, &b));
	Return(output_stream_designator_(ptr, stream, &stream));

	return pprint_tab_print_(ptr, stream, value, a, b);
}


/*
 *  print-unreadable-object
 */
static void print_unreadable_object_expand_common(addr *ret,
		addr pos, addr stream, addr type, addr identity, addr body)
{
	/* `(lisp-system::print-unreadable-call
	 *     ,stream ,object ,type ,identity ,(when body `(lambda () ,@body)))
	 */
	addr call, lambda;

	if (body != Nil) {
		GetConst(COMMON_LAMBDA, &lambda);
		lista_heap(&body, lambda, Nil, body, NULL);
	}
	GetConst(SYSTEM_PRINT_UNREADABLE_CALL, &call);
	list_heap(ret, call, stream, pos, type, identity, body, NULL);
}

int print_unreadable_object_common_(addr form, addr env, addr *ret)
{
	addr args, list, stream, pos;
	addr key, key1, key2, value, value1, value2;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &list, &args))
		goto error;
	/* first argument */
	if (! consp_getcons(list, &pos, &list))
		goto error;
	if (! consp_getcons(list, &stream, &list))
		goto error;
	GetConst(KEYWORD_TYPE, &key1);
	GetConst(KEYWORD_IDENTITY, &key2);
	value1 = value2 = Unbound;
	while (list != Nil) {
		Return_getcons(list, &key, &list);
		Return_getcons(list, &value, &list);
		if (key == key1) {
			if (value1 == Unbound)
				value1 = value;
		}
		else if (key == key2) {
			if (value2 == Unbound)
				value2 = value;
		}
		else {
			goto error;
		}
	}
	/* result */
	if (value1 == Unbound) value1 = Nil;
	if (value2 == Unbound) value2 = Nil;
	print_unreadable_object_expand_common(ret, pos, stream, value1, value2, args);
	return 0;

error:
	return fmte_("PRINT-UNREADABLE-OBJECT form ~S must be "
			"((object stream &key type identity) &body form)", form, NULL);
}


/*
 *  set-pprint-dispatch
 */
int set_pprint_dispatch_common_(Execute ptr,
		addr spec, addr call, addr priority, addr table)
{
	addr type;

	Return(parse_type_(ptr, &type, spec, Nil));
	if (call != Nil && (! functionp(call)))
		Return(getfunction_global_(call, &call));
	if (table == Unbound) {
		Return(pprint_dispatch_print_(ptr, &table));
	}

	return set_pprint_dispatch_print_(ptr->local, spec, type, call, priority, table);
}


/*
 *  write
 */
static void write_special_common(Execute ptr, constindex index, addr pos)
{
	addr symbol;
	GetConstant(index, &symbol);
	pushspecial_control(ptr, symbol, pos);
}
#define WriteSpecialCommon(p, a, b) \
	write_special_common((p), CONSTANT_SPECIAL_PRINT_##a, (b))

static void write_keyword_common(Execute ptr, addr args)
{
	addr pos;

	/* print-array */
	if (! GetKeyArgs(args, KEYWORD_ARRAY, &pos)) {
		WriteSpecialCommon(ptr, ARRAY, pos);
	}
	/* print-base */
	if (! GetKeyArgs(args, KEYWORD_BASE, &pos)) {
		WriteSpecialCommon(ptr, BASE, pos);
	}
	/* print-radix */
	if (! GetKeyArgs(args, KEYWORD_RADIX, &pos)) {
		WriteSpecialCommon(ptr, RADIX, pos);
	}
	/* print-case */
	if (! GetKeyArgs(args, KEYWORD_CASE, &pos)) {
		WriteSpecialCommon(ptr, CASE, pos);
	}
	/* print-circle */
	if (! GetKeyArgs(args, KEYWORD_CIRCLE, &pos)) {
		WriteSpecialCommon(ptr, CIRCLE, pos);
	}
	/* print-escape */
	if (! GetKeyArgs(args, KEYWORD_ESCAPE, &pos)) {
		WriteSpecialCommon(ptr, ESCAPE, pos);
	}
	/* print-gensym */
	if (! GetKeyArgs(args, KEYWORD_GENSYM, &pos)) {
		WriteSpecialCommon(ptr, GENSYM, pos);
	}
	/* print-readably */
	if (! GetKeyArgs(args, KEYWORD_READABLY, &pos)) {
		WriteSpecialCommon(ptr, READABLY, pos);
	}
	/* print-pretty */
	if (! GetKeyArgs(args, KEYWORD_PRETTY, &pos)) {
		WriteSpecialCommon(ptr, PRETTY, pos);
	}
	/* print-level */
	if (! GetKeyArgs(args, KEYWORD_LEVEL, &pos)) {
		WriteSpecialCommon(ptr, LEVEL, pos);
	}
	/* print-length */
	if (! GetKeyArgs(args, KEYWORD_LENGTH, &pos)) {
		WriteSpecialCommon(ptr, LENGTH, pos);
	}
	/* print-lines */
	if (! GetKeyArgs(args, KEYWORD_LINES, &pos)) {
		WriteSpecialCommon(ptr, LINES, pos);
	}
	/* print-miser-width */
	if (! GetKeyArgs(args, KEYWORD_MISER_WIDTH, &pos)) {
		WriteSpecialCommon(ptr, MISER_WIDTH, pos);
	}
	/* print-right-margin */
	if (! GetKeyArgs(args, KEYWORD_RIGHT_MARGIN, &pos)) {
		WriteSpecialCommon(ptr, RIGHT_MARGIN, pos);
	}
	/* print-pprint-dispatch */
	if (! GetKeyArgs(args, KEYWORD_PPRINT_DISPATCH, &pos)) {
		WriteSpecialCommon(ptr, PPRINT_DISPATCH, pos);
	}
}

static int write_common_call_(Execute ptr, addr stream, addr var)
{
	Return(write_print_(ptr, stream, var));
	return exitpoint_stream_(stream);
}

int write_common_(Execute ptr, addr var, addr args)
{
	addr stream, control;

	if (GetKeyArgs(args, KEYWORD_STREAM, &stream))
		stream = Unbound;
	Return(output_stream_designator_(ptr, stream, &stream));

	/* write */
	push_control(ptr, &control);
	write_keyword_common(ptr, args);
	(void)write_common_call_(ptr, stream, var);
	return pop_control_(ptr, control);
}


/*
 *  prin1
 */
int prin1_common_(Execute ptr, addr var, addr stream)
{
	Return(output_stream_designator_(ptr, stream, &stream));
	Return(prin1_print_(ptr, stream, var));
	return exitpoint_stream_(stream);
}


/*
 *  princ
 */
int princ_common_(Execute ptr, addr var, addr stream)
{
	Return(output_stream_designator_(ptr, stream, &stream));
	Return(princ_print_(ptr, stream, var));
	return exitpoint_stream_(stream);
}


/*
 *  print
 */
int print_common_(Execute ptr, addr var, addr stream)
{
	Return(output_stream_designator_(ptr, stream, &stream));
	Return(print_print_(ptr, stream, var));
	return exitpoint_stream_(stream);
}


/*
 *  pprint
 */
int pprint_common_(Execute ptr, addr var, addr stream)
{
	Return(output_stream_designator_(ptr, stream, &stream));
	Return(pprint_print_(ptr, stream, var));
	return exitpoint_stream_(stream);
}


/*
 *  write-to-string
 */
static int write_to_string_common_call_(Execute ptr, addr stream, addr var, addr *ret)
{
	Return(write_print_(ptr, stream, var));
	Return(string_stream_heap_(stream, ret));
	close_output_string_stream(stream);

	return 0;
}

int write_to_string_common_(Execute ptr, addr var, addr args, addr *ret)
{
	addr stream, control;

	open_output_string_stream(&stream, 0);
	push_control(ptr, &control);
	write_keyword_common(ptr, args);
	(void)write_to_string_common_call_(ptr, stream, var, ret);
	return pop_control_(ptr, control);
}


/*
 *  prin1-to-string
 */
int prin1_to_string_common_(Execute ptr, addr var, addr *ret)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(prin1_print_(ptr, stream, var));
	Return(string_stream_heap_(stream, ret));
	close_output_string_stream(stream);

	return 0;
}


/*
 *  princ-to-string
 */
int princ_to_string_common_(Execute ptr, addr var, addr *ret)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(princ_print_(ptr, stream, var));
	Return(string_stream_heap_(stream, ret));
	close_output_string_stream(stream);

	return 0;
}


/*
 *  initialize
 */
void init_call_printer(void)
{
	SetPointerType_(var1dynamic, formatter_call_common);
}


/************************************************************
 *  call_reader.c
 ************************************************************/

/*
 *  copy-readtable
 */
int copy_readtable_common_(Execute ptr, addr from, addr to, addr *ret)
{
	int check1, check2;

	/* argument */
	if (from == Unbound) {
		GetConst(SPECIAL_READTABLE, &from);
		Return(getspecialcheck_local_(ptr, from, &from));
	}
	if (to == Unbound) {
		to = Nil;
	}

	/* make, copy */
	check1 = (from == Nil);
	check2 = (to == Nil);
	if (check1 && check2) {
		Return(readtable_heap_(&from));
		return Result(ret, from);
	}
	else if (check1) {
		Return(copy_default_readtable_(to));
		return Result(ret, to);
	}
	else if (check2) {
		Return(copy_readtable_heap_(from, &from));
		return Result(ret, from);
	}
	else {
		Return(copy_readtable_(from, to));
		return Result(ret, to);
	}
}


/*
 *  make-dispatch-macro-character
 */
int make_dispatch_macro_character_common_(Execute ptr,
		addr code, addr nonterm, addr readtable)
{
	if (nonterm == Unbound) {
		nonterm = Nil;
	}
	if (readtable == Unbound) {
		GetConst(SPECIAL_READTABLE, &readtable);
		Return(getspecialcheck_local_(ptr, readtable, &readtable));
	}

	return make_dispatch_macro_character_(readtable, code, nonterm != Nil);
}


/*
 *  read
 */
int read_common_(Execute ptr, addr stream, addr errorp, addr eof, addr recp, addr *ret)
{
	int check;
	addr pos;

	/* stream */
	if (stream == Unbound) {
		GetConst(SPECIAL_STANDARD_INPUT, &stream);
		Return(getspecialcheck_local_(ptr, stream, &stream));
	}
	Return(input_stream_designator_(ptr, stream, &stream));

	/* errorp */
	if (errorp == Unbound)
		errorp = T;
	if (eof == Unbound)
		eof = Nil;

	/* recursive-p */
	if (recp == Unbound)
		recp = Nil;

	/* read */
	if (recp == Nil) {
		Return(read_stream_(ptr, stream, &check, &pos));
		if (check) {
			if (errorp != Nil)
				return call_end_of_file_(ptr, stream);
			pos = eof;
		}
	}
	else {
		Return(read_recursive_(ptr, stream, &check, &pos));
		if (check) {
			if (errorp != Nil)
				return fmte_("End-of-file occured by recursive-p read.", NULL);
			pos = eof;
		}
	}
	*ret = pos;

	return 0;
}


/*
 *  read-preserving-whitespace
 */
int read_preserving_whitespace_common_(Execute ptr,
		addr stream, addr errorp, addr eof, addr recp, addr *ret)
{
	int check;
	addr pos;

	/* stream */
	if (stream == Unbound) {
		GetConst(SPECIAL_STANDARD_INPUT, &stream);
		Return(getspecialcheck_local_(ptr, stream, &stream));
	}
	Return(input_stream_designator_(ptr, stream, &stream));

	/* errorp */
	if (errorp == Unbound)
		errorp = T;
	if (eof == Unbound)
		eof = Nil;

	/* recursive-p */
	if (recp == Unbound)
		recp = Nil;

	/* read */
	if (recp == Nil) {
		Return(read_preserving_(ptr, stream, &check, &pos));
		if (check) {
			if (errorp != Nil)
				return call_end_of_file_(ptr, stream);
			pos = eof;
		}
	}
	else {
		Return(read_recursive_(ptr, stream, &check, &pos));
		if (check) {
			if (errorp != Nil)
				return fmte_("End-of-file occured by recursive-p read.", NULL);
			pos = eof;
		}
	}
	*ret = pos;

	return 0;
}


/*
 *  read-delimited-list
 */
int read_delimited_list_common_(Execute ptr, addr code, addr stream, addr recp)
{
	unicode c;

	if (stream == Unbound) {
		GetConst(SPECIAL_STANDARD_INPUT, &stream);
		Return(getspecialcheck_local_(ptr, stream, &stream));
	}
	if (recp == Unbound) {
		recp = Nil;
	}

	GetCharacter(code, &c);
	return read_delimited_list_(ptr, stream, c, recp != Nil);
}


/*
 *  read-from-string
 */
static int read_from_string_execute_common_(Execute ptr, addr string,
		int eofp, addr eof, size_t start, size_t end, int preserve,
		addr *ret, addr *sec)
{
	int check;
	addr stream, pos;
	size_t size;

	Return(open_input_string_stream2_(&stream, string, start, end));
	if (preserve) {
		Return(read_preserving_(ptr, stream, &check, &pos));
	}
	else {
		Return(read_stream_(ptr, stream, &check, &pos));
	}
	if (check) {
		if (eofp) {
			*ret = *sec = Nil;
			return call_end_of_file_(ptr, stream);
		}
		pos = eof;
	}
	getindex_input_stream(stream, &size);
	Return(close_stream_(stream, NULL));

	/* result */
	*ret = pos;
	make_index_integer_heap(sec, size);

	return 0;
}

int read_from_string_common_(Execute ptr, addr args, addr *ret, addr *sec)
{
	int eofp, preserve;
	addr str, eof, pos, key;
	size_t start, end;

	/* string */
	if (! consp_getcons(args, &str, &args))
		goto error;
	if (! stringp(str))
		return fmte_("The read-from-string argument ~S must be a string.", str, NULL);
	if (args == Nil)
		goto default_string;
	/* eof-error-p */
	if (! consp_getcons(args, &pos, &args))
		goto error;
	eofp = (pos != Nil);
	if (args == Nil)
		goto default_eofp;
	/* eof-value */
	if (! consp_getcons(args, &eof, &args))
		goto error;
	if (args == Nil)
		goto default_eof;
	if (! consp(args))
		goto error;
	/* key start */
	GetConst(KEYWORD_START, &key);
	if (getplist_safe(args, key, &pos)) {
		start = 0;
	}
	else {
		if (GetIndex_integer(pos, &start))
			return fmte_("Too large start value ~S.", pos, NULL);
	}
	/* key end */
	GetConst(KEYWORD_END, &key);
	if (getplist_safe(args, key, &pos) || pos == Nil) {
		string_length(str, &end);
	}
	else {
		if (GetIndex_integer(pos, &end))
			return fmte_("Too large end value ~S.", pos, NULL);
	}
	/* key preserving-whitespace */
	GetConst(KEYWORD_PRESERVE_WHITESPACE, &key);
	if (getplist_safe(args, key, &pos))
		preserve = 0;
	else
		preserve = (pos != Nil);
	/* execute */
	goto execute;

default_string:
	eofp = 1;
default_eofp:
	eof = Nil;
default_eof:
	start = 0;
	string_length(str, &end);
	preserve = 0;
execute:
	return read_from_string_execute_common_(ptr,
			str, eofp, eof, start, end, preserve, ret, sec);

error:
	return fmte_("Invalid read-from-string argument.", NULL);
}


/*
 *  readtable-case
 */
int readtable_case_common_(addr var, addr *ret)
{
	constindex index;

	switch (getcase_readtable(var)) {
		case ReadTable_upcase:
			index = CONSTANT_KEYWORD_UPCASE;
			break;

		case ReadTable_downcase:
			index = CONSTANT_KEYWORD_DOWNCASE;
			break;

		case ReadTable_preserve:
			index = CONSTANT_KEYWORD_PRESERVE;
			break;

		case ReadTable_invert:
			index = CONSTANT_KEYWORD_INVERT;
			break;

		default:
			return fmte_("Invalid case type.", NULL);
	}
	GetConstant(index, ret);

	return 0;
}


/*
 *  (setf readtable-case)
 */
int setf_readtable_case_common_(addr value, addr var)
{
	addr key;

	GetConst(KEYWORD_UPCASE, &key);
	if (value == key) {
		setcase_readtable(var, ReadTable_upcase);
		return 0;
	}

	GetConst(KEYWORD_DOWNCASE, &key);
	if (value == key) {
		setcase_readtable(var, ReadTable_downcase);
		return 0;
	}

	GetConst(KEYWORD_PRESERVE, &key);
	if (value == key) {
		setcase_readtable(var, ReadTable_preserve);
		return 0;
	}

	GetConst(KEYWORD_INVERT, &key);
	if (value == key) {
		setcase_readtable(var, ReadTable_invert);
		return 0;
	}

	/* error */
	return fmte_("Invalid case sensitivity mode ~S.", value, NULL);
}


/*
 *  get-dispatch-macro-character
 */
int get_dispatch_macro_character_common_(Execute ptr,
		addr x, addr y, addr readtable, addr *ret)
{
	unicode a, b;

	if (readtable == Nil) {
		/* standard readtable */
		return get_default_dispatch_macro_(x, y, ret);
	}
	if (readtable == Unbound) {
		/* *readtable* */
		GetConst(SPECIAL_READTABLE, &readtable);
		Return(getspecialcheck_local_(ptr, readtable, &readtable));
	}
	GetCharacter(x, &a);
	GetCharacter(y, &b);
	return get_dispatch_macro_character_(readtable, a, b, ret);
}


/*
 *  set-dispatch-macro-character
 */
int set_dispatch_macro_character_common_(Execute ptr,
		addr x, addr y, addr call, addr readtable)
{
	unicode a, b;

	if (readtable == Nil) {
		/* standard readtable */
		return fmte_("set-dispatch-macro-character "
				"don't update a standard readtable.", NULL);
	}
	if (readtable == Unbound) {
		/* *readtable* */
		GetConst(SPECIAL_READTABLE, &readtable);
		Return(getspecialcheck_local_(ptr, readtable, &readtable));
	}
	GetCharacter(x, &a);
	GetCharacter(y, &b);
	if (symbolp(call)) {
		Return(function_global_restart_(ptr, call, &call));
	}

	return set_dispatch_macro_character_(readtable, a, b, call);
}


/*
 *  get-macro-character
 */
int get_macro_character_common_(Execute ptr,
		addr code, addr readtable, addr *ret, addr *sec)
{
	int check;
	unicode c;

	GetCharacter(code, &c);
	if (readtable == Nil) {
		/* standard readtable */
		get_default_macro_character(c, ret, &check);
		*sec = check? T: Nil;
		return 0;
	}
	if (readtable == Unbound) {
		/* *readtable* */
		GetConst(SPECIAL_READTABLE, &readtable);
		Return(getspecialcheck_local_(ptr, readtable, &readtable));
	}

	Return(get_macro_character_(readtable, c, ret, &check));
	*sec = check? T: Nil;
	return 0;
}


/*
 *  set-macro-character
 */
int set_macro_character_common_(Execute ptr,
		addr code, addr call, addr nonterm, addr readtable)
{
	unicode c;

	if (nonterm == Unbound) {
		nonterm = Nil;
	}
	if (readtable == Nil) {
		/* standard readtable */
		return fmte_("set-macro-character don't update a standard readtable.", NULL);
	}
	if (readtable == Unbound) {
		/* *readtable* */
		GetConst(SPECIAL_READTABLE, &readtable);
		Return(getspecialcheck_local_(ptr, readtable, &readtable));
	}

	GetCharacter(code, &c);
	if (symbolp(call)) {
		Return(function_global_restart_(ptr, call, &call));
	}

	return set_macro_character_(readtable, c, nonterm != Nil, call);
}


/*
 *  set-syntax-from-char
 */
int set_syntax_from_char_common_(Execute ptr, addr x, addr y, addr z, addr w)
{
	unicode a, b;

	if (z == Unbound) {
		/* *readtable* */
		GetConst(SPECIAL_READTABLE, &z);
		Return(getspecialcheck_local_(ptr, z, &z));
	}
	if (w == Unbound) {
		w = Nil;
	}

	GetCharacter(x, &a);
	GetCharacter(y, &b);
	if (w == Nil)
		return set_syntax_from_default_(a, b, z);
	else
		return set_syntax_from_char_(a, b, z, w);
}


/*
 *  with-standard-io-syntax
 */
int with_standard_io_syntax_common_(addr form, addr env, addr *ret)
{
	addr args, symbol, value, find, copy, quote;

	args = Nil;
	/* (*package* (find-package :common-lisp-user)) */
	GetConst(SPECIAL_PACKAGE, &symbol);
	GetConst(COMMON_FIND_PACKAGE, &find);
	GetConst(KEYWORD_COMMON_LISP_USER, &value);
	list_heap(&value, find, value, NULL);
	list_heap(&value, symbol, value, NULL);
	cons_heap(&args, value, args);

	/* (*print-array* t) */
	GetConst(SPECIAL_PRINT_ARRAY, &symbol);
	list_heap(&value, symbol, T, NULL);
	cons_heap(&args, value, args);

	/* (*print-base* 10) */
	GetConst(SPECIAL_PRINT_BASE, &symbol);
	fixnum_heap(&value, 10);
	list_heap(&value, symbol, value, NULL);
	cons_heap(&args, value, args);

	/* (*print-case* :upcase) */
	GetConst(SPECIAL_PRINT_CASE, &symbol);
	GetConst(KEYWORD_UPCASE, &value);
	list_heap(&value, symbol, value, NULL);
	cons_heap(&args, value, args);

	/* (*print-circle* nil) */
	GetConst(SPECIAL_PRINT_CIRCLE, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*print-escape* t) */
	GetConst(SPECIAL_PRINT_ESCAPE, &symbol);
	list_heap(&value, symbol, T, NULL);
	cons_heap(&args, value, args);

	/* (*print-gensym* t) */
	GetConst(SPECIAL_PRINT_GENSYM, &symbol);
	list_heap(&value, symbol, T, NULL);
	cons_heap(&args, value, args);

	/* (*print-length* nil) */
	GetConst(SPECIAL_PRINT_LENGTH, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*print-level* nil) */
	GetConst(SPECIAL_PRINT_LEVEL, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*print-lines* nil) */
	GetConst(SPECIAL_PRINT_LINES, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*print-miser-width* nil) */
	GetConst(SPECIAL_PRINT_MISER_WIDTH, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*print-pprint-dispatch*
	 *   (copy-pprint-dispatch system::*default-print-dispatch*))
	 */
	GetConst(SPECIAL_PRINT_PPRINT_DISPATCH, &symbol);
	GetConst(COMMON_COPY_PPRINT_DISPATCH, &copy);
	GetConst(SYSTEM_DEFAULT_PRINT_DISPATCH, &value);
	list_heap(&value, copy, value, NULL);
	list_heap(&value, symbol, value, NULL);
	cons_heap(&args, value, args);

	/* (*print-pretty* nil) */
	GetConst(SPECIAL_PRINT_PRETTY, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*print-radix* nil) */
	GetConst(SPECIAL_PRINT_RADIX, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*print-readably* t) */
	GetConst(SPECIAL_PRINT_READABLY, &symbol);
	list_heap(&value, symbol, T, NULL);
	cons_heap(&args, value, args);

	/* (*print-right-margin* nil) */
	GetConst(SPECIAL_PRINT_RIGHT_MARGIN, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*read-base* 10) */
	GetConst(SPECIAL_READ_BASE, &symbol);
	fixnum_heap(&value, 10);
	list_heap(&value, symbol, value, NULL);
	cons_heap(&args, value, args);

	/* (*read-default-float-format* 'single-float) */
	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &symbol);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_SINGLE_FLOAT, &value);
	list_heap(&value, quote, value, NULL);
	list_heap(&value, symbol, value, NULL);
	cons_heap(&args, value, args);

	/* (*read-eval* t) */
	GetConst(SPECIAL_READ_EVAL, &symbol);
	list_heap(&value, symbol, T, NULL);
	cons_heap(&args, value, args);

	/* (*read-suppress* nil) */
	GetConst(SPECIAL_READ_SUPPRESS, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*readtable* (copy-readtable nil)) */
	GetConst(SPECIAL_READTABLE, &symbol);
	GetConst(COMMON_COPY_READTABLE, &value);
	list_heap(&value, value, Nil, NULL);
	list_heap(&value, symbol, value, NULL);
	cons_heap(&args, value, args);

	/* `(let ,args ,@form) */
	nreverse(&args, args);
	GetConst(COMMON_LET, &symbol);
	Return_getcdr(form, &form);
	lista_heap(ret, symbol, args, form, NULL);

	return 0;
}


/************************************************************
 *  call_sequences.c
 ************************************************************/

/*
 *  copy-seq
 */
int copy_seq_common_(addr var, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_NIL:
			break;

		case LISPTYPE_CONS:
			copy_list_heap_safe(ret, var);
			break;

		case LISPTYPE_VECTOR:
			copy_vector_heap(ret, var);
			break;

		case LISPTYPE_STRING:
			return string_heap_(ret, var);

		case LISPTYPE_ARRAY:
			return array_copy_heap_(ret, var);

		case LISPTYPE_BITVECTOR:
			bitmemory_copy_heap(ret, var);
			break;

		default:
			*ret = Nil;
			return TypeError_(var, SEQUENCE);
	}

	return 0;
}


/*
 *  elt
 */
int elt_common_(addr var, addr index, addr *ret)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return fmte_("Too large index ~S.", index, NULL);
	return getelt_sequence_(NULL, var, size, ret);
}


/*
 *  (setf elt)
 */
int setf_elt_common_(addr value, addr pos, addr index)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return fmte_("Too large index ~S.", index, NULL);
	return setelt_sequence_(pos, size, value);
}


/*
 *  fill
 */
static int list_fill_sequence_(addr list, addr item, addr start, addr end)
{
	size_t index1, index2;

	/* argument */
	Return(list_start_end_sequence_(&list, NULL, start, end, &index1, &index2));

	/* fill */
	for (;;) {
		if (end != Nil) {
			if (index2 <= index1)
				break;
			if (list == Nil) {
				return fmte_(":END ~A "
						"must be less than equal to list length.", end, NULL);
			}
		}
		else if (list == Nil) {
			break;
		}
		if (! consp(list))
			return fmte_("Don't accept the dotted list ~S.", list, NULL);
		SetCar(list, item);
		GetCdr(list, &list);
		index1++;
	}

	return 0;
}

static int vector_fill_sequence_(addr pos, addr item, addr start, addr end)
{
	size_t index1, index2;

	/* argument */
	lenarray(pos, &index1);
	Return(size_start_end_sequence_(start, end, index1, &index1, &index2, NULL));

	/* fill */
	for (; index1 < index2; index1++)
		setarray(pos, index1, item);

	return 0;
}

static int fill_call_common_(addr var, addr item, addr start, addr end)
{
	switch (GetType(var)) {
		case LISPTYPE_NIL:
			return 0;

		case LISPTYPE_CONS:
			return list_fill_sequence_(var, item, start, end);

		case LISPTYPE_VECTOR:
			return vector_fill_sequence_(var, item, start, end);

		case LISPTYPE_STRING:
			return strvect_fill_(var, item, start, end);

		case LISPTYPE_ARRAY:
			return array_fill_(var, item, start, end);

		case LISPTYPE_BITVECTOR:
			return bitmemory_fill_(var, item, start, end);

		default:
			return TypeError_(var, SEQUENCE);
	}
}

int fill_common_(addr var, addr item, addr rest)
{
	addr start, end;

	if (GetKeyArgs(rest, KEYWORD_START, &start))
		start = fixnumh(0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Nil;

	return fill_call_common_(var, item, start, end);
}


/*
 *  subseq
 */
static int list_subseq_sequence_(addr *ret, addr list, addr start, addr end)
{
	int check;
	addr root, pos;
	struct sequence_range range;

	Return(build_sequence_range_(&range, list, start, end));
	root = Nil;
	for (;;) {
		Return(getnext_sequence_range_(&range, &pos, &check));
		if (check)
			break;
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);

	return 0;
}

static int vector_subseq_sequence_(addr *ret, addr vector, addr start, addr end)
{
	size_t index1, index2, i;
	addr root, item;

	/* argument */
	lenarray(vector, &index1);
	Return(size_start_end_sequence_(start, end, index1, &index1, &index2, NULL));

	/* subseq */
	vector_heap(&root, index2 - index1);
	for (i = 0; index1 < index2; index1++, i++) {
		getarray(vector, index1, &item);
		setarray(root, i, item);
	}

	return Result(ret, root);
}

int subseq_common_(addr var, addr start, addr end, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return list_subseq_sequence_(ret, var, start, end);

		case LISPTYPE_VECTOR:
			return vector_subseq_sequence_(ret, var, start, end);

		case LISPTYPE_STRING:
			return strvect_subseq_(ret, var, start, end);

		case LISPTYPE_ARRAY:
			return array_subseq_(ret, var, start, end);

		case LISPTYPE_BITVECTOR:
			return bitmemory_subseq_(ret, var, start, end);

		default:
			*ret = Nil;
			return TypeError_(var, SEQUENCE);
	}
}

int setf_subseq_common_(addr root, addr pos, addr start, addr end)
{
	int ignore;
	struct array_value value;
	struct sequence_range range1, range2;

	Return(build_sequence_range_(&range1, root, start, end));
	Return(build_sequence_range_(&range2, pos, Nil, Nil));
	for (;;) {
		if (endp_sequence_range(&range1))
			break;
		if (endp_sequence_range(&range2))
			break;
		Return(getinplace_sequence_range_(&range2, &value));
		Return(setinplace_sequence_range_(NULL, &range1, &value)); /* heap */
		Return(next_sequence_range_(&range1, &ignore));
		Return(next_sequence_range_(&range2, &ignore));
	}

	return 0;
}


/*
 *  reduce
 */
struct reduce_struct {
	unsigned valuep : 1;
	unsigned fromp : 1;
	Execute ptr;
	LocalRoot local;
	addr pos, call, key, start, end, from, value;
	struct sequence_range range;
};

static int key_reduce_sequence_(struct reduce_struct *str, addr *ret, addr value)
{
	if (str->key != Nil)
		return funcall1_control_(str->ptr, ret, str->key, value, NULL);
	else
		return Result(ret, value);
}

static int throw_reduce_sequence_(struct reduce_struct *str, int *result, addr *ret)
{
	int check;
	addr value, pos;
	struct sequence_range *range;
	LocalHold hold;

	value = str->value;
	range = &(str->range);
	save_sequence_range(range);

	/* empty sequence */
	Return(getnext_sequence_range_(range, &pos, &check));
	if (check) {
		if (value == Unbound) {
			Return(apply1_control_(str->ptr, ret, str->call, Nil));
		}
		else {
			*ret = value;
		}
		return Result(result, 1);
	}

	/* single value */
	if (endp_sequence_range(range) && value == Unbound) {
		hold = LocalHold_local_push(str->ptr, pos);
		Return(key_reduce_sequence_(str, ret, pos));
		localhold_end(hold);
		return Result(result, 1);
	}

	/* multiple value */
	load_sequence_range(range);
	return Result(result, 0);
}

static int value_reduce_sequence_(struct reduce_struct *str, addr *ret)
{
	int check;
	Execute ptr;
	addr pos1, pos2, call;
	struct sequence_range *range;
	LocalHold hold;

	ptr = str->ptr;
	range = &(str->range);
	pos1 = str->value;
	call = str->call;

	/* first */
	hold = LocalHold_array(str->ptr, 2);
	if (pos1 == Unbound) {
		Return(getnext_sequence_range_(range, &pos1, &check)); /* ignore */
		localhold_set(hold, 0, pos1);
		Return(key_reduce_sequence_(str, &pos1, pos1));
	}
	localhold_set(hold, 0, pos1);

	/* loop */
	for (;;) {
		Return(getnext_sequence_range_(range, &pos2, &check));
		if (check)
			break;
		localhold_set(hold, 1, pos2);
		Return(key_reduce_sequence_(str, &pos2, pos2));
		localhold_set(hold, 1, pos2);
		Return(funcall1_control_(ptr, &pos1, call, pos1, pos2, NULL));
		localhold_set(hold, 0, pos1);
	}
	localhold_end(hold);

	return Result(ret, pos1);
}

static int reverse_vector_reduce_sequence_(struct reduce_struct *str, addr *ret)
{
	int check;
	Execute ptr;
	addr pos1, pos2, call;
	struct sequence_range *range;
	LocalHold hold;

	ptr = str->ptr;
	range = &(str->range);
	pos2 = str->value;
	call = str->call;

	/* first */
	reverse_sequence_range(range);
	hold = LocalHold_array(str->ptr, 2);
	if (pos2 == Unbound) {
		Return(getnext_reverse_sequence_range_(range, &pos2, &check)); /* ignore */
		localhold_set(hold, 1, pos2);
		Return(key_reduce_sequence_(str, &pos2, pos2));
	}
	localhold_set(hold, 1, pos2);

	/* loop */
	for (;;) {
		Return(getnext_reverse_sequence_range_(range, &pos1, &check));
		if (check)
			break;
		localhold_set(hold, 0, pos1);
		Return(key_reduce_sequence_(str, &pos1, pos1));
		localhold_set(hold, 0, pos1);
		Return(funcall1_control_(ptr, &pos2, call, pos1, pos2, NULL));
		localhold_set(hold, 1, pos2);
	}
	localhold_end(hold);

	return Result(ret, pos2);
}

static int switch_reduce_sequence_(struct reduce_struct *str, addr *ret)
{
	int check;

	Return(throw_reduce_sequence_(str, &check, ret));
	if (check)
		return 0;
	else if (str->range.listp)
		return value_reduce_sequence_(str, ret);
	else if (str->fromp)
		return reverse_vector_reduce_sequence_(str, ret);
	else
		return value_reduce_sequence_(str, ret);
}

static int reverse_reduce_sequence_(struct reduce_struct *str, addr *ret)
{
	LocalRoot local;
	LocalStack stack;
	struct sequence_range *range;

	range = &(str->range);
	local = str->local;
	push_local(local, &stack);
	Return(build_sequence_range_vector_(local, range, str->pos, str->start, str->end));
	Return(switch_reduce_sequence_(str, ret));
	rollback_local(local, stack);

	return 0;
}

int reduce_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	int listp;
	unsigned fromp;
	addr key, start, end, from, value;
	struct reduce_struct str;
	struct sequence_range *range;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Unbound;
	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_INITIAL_VALUE, &value))
		value = Unbound;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
	fromp = (from != Nil);
	range = &(str.range);
	str.valuep = (value != Unbound);
	str.ptr = ptr;
	str.local = ptr->local;
	str.call = call;
	str.pos = pos;
	str.key = key;
	str.start = start;
	str.end = end;
	str.fromp = fromp;
	str.from = from;
	str.value = value;

	if (fromp && listp)
		return reverse_reduce_sequence_(&str, ret);
	else {
		Return(build_sequence_range_(range, pos, start, end));
		return switch_reduce_sequence_(&str, ret);
	}
}


/*
 *  sort
 */
int sort_common_(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	return quick_sort_sequence_(ptr, pos, call, key);
}


/*
 *  stable-sort
 */
int stable_sort_common_(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	return merge_sort_sequence_(ptr, pos, call, key);
}


/*
 *  replace
 */
static int list_replace_sequence_(LocalRoot local,
		struct sequence_range *range1,
		struct sequence_range *range2)
{
	int check;
	LocalStack stack;
	addr pos, value;
	size_t size, i;

	push_local(local, &stack);
	size = range2->size;
	vector_local(local, &pos, size);
	for (i = 0; ; i++) {
		Return(getnext_sequence_range_(range2, &value, &check));
		if (check)
			break;
		setarray(pos, i, value);
	}

	for (i = 0; i < size; i++) {
		if (endp_sequence_range(range1))
			break;
		getarray(pos, i, &value);
		Return(set_sequence_range_(range1, value));
		Return(next_sequence_range_(range1, &check));
	}
	rollback_local(local, stack);

	return 0;
}

static int forward_replace_sequence_(
		struct sequence_range *range1,
		struct sequence_range *range2)
{
	int check;
	addr value;

	for (;;) {
		Return(getnext_sequence_range_(range2, &value, &check));
		if (check)
			break;
		if (endp_sequence_range(range1))
			break;
		Return(set_sequence_range_(range1, value));
		Return(next_sequence_range_(range1, &check));
	}

	return 0;
}

static int eq_replace_sequence_(LocalRoot local,
		struct sequence_range *range1,
		struct sequence_range *range2)
{
	if (range1->start == range2->start)
		return 0;
	if (range2->end < range1->start || range2->end < range1->start)
		return forward_replace_sequence_(range1, range2);
	else
		return list_replace_sequence_(local, range1, range2);
}

int replace_common_(Execute ptr, addr pos1, addr pos2, addr rest)
{
	LocalRoot local;
	addr start1, start2, end1, end2;
	struct sequence_range range1, range2;

	if (GetKeyArgs(rest, KEYWORD_START1, &start1))
		fixnum_heap(&start1, 0);
	if (GetKeyArgs(rest, KEYWORD_START2, &start2))
		fixnum_heap(&start2, 0);
	if (GetKeyArgs(rest, KEYWORD_END1, &end1))
		end1 = Unbound;
	if (GetKeyArgs(rest, KEYWORD_END2, &end2))
		end2 = Unbound;

	Return(build_sequence_range_endp_(&range1, pos1, start1, end1));
	Return(build_sequence_range_endp_(&range2, pos2, start2, end2));
	local = ptr->local;
	if (range1.listp && range2.listp)
		return list_replace_sequence_(local, &range1, &range2);
	else if (pos1 != pos2)
		return forward_replace_sequence_(&range1, &range2);
	else
		return eq_replace_sequence_(local, &range1, &range2);
}


/*
 *  concatenate
 */
static int list_concatenate_sequence_(addr *ret, addr type, addr rest)
{
	int check;
	enum LISPDECL decl;
	addr root, value, one;
	size_t size, i;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_CONS && decl != LISPDECL_LIST)
		return Result(ret, Unbound);

	/* concatenate */
	root = Nil;
	while (rest != Nil) {
		Return_getcons(rest, &value, &rest);
		Return(listp_sequence_(value, &check));
		if (check) {
			while (value != Nil) {
				Return_getcons(value, &one, &value);
				cons_heap(&root, one, root);
			}
		}
		else {
			Return(length_sequence_(value, 1, &size));
			for (i = 0; i < size; i++) {
				Return(getelt_sequence_(NULL, value, i, &one));
				cons_heap(&root, one, root);
			}
		}
	}
	nreverse(ret, root);
	return 0;
}

static int length_concatenate_sequence_(addr rest, size_t *ret)
{
	addr pos;
	size_t size, value;

	for (size = 0; rest != Nil; size += value) {
		Return_getcons(rest, &pos, &rest);
		Return(length_sequence_(pos, 1, &value));
	}

	return Result(ret, size);
}

static int value_concatenate_sequence_(addr root, addr rest)
{
	addr pos;
	size_t index, size, i;
	struct array_value value;

	for (index = 0; rest != Nil; ) {
		GetCons(rest, &pos, &rest);
		Return(length_sequence_(pos, 1, &size));
		for (i = 0; i < size; i++) {
			Return(getelt_inplace_sequence_(pos, i, &value));
			Return(setelt_inplace_sequence_(NULL, root, index++, &value));
		}
	}

	return 0;
}

static int vector_concatenate_sequence_(addr *ret, addr type, addr rest)
{
	addr root;
	size_t size;

	/* type check */
	if (LowLispDecl(type) != LISPDECL_VECTOR)
		return Result(ret, Unbound);

	/* concatenate */
	Return(length_concatenate_sequence_(rest, &size));
	Return(array_upgraded_merge_sequence_(&root, type, size));
	Return(value_concatenate_sequence_(root, rest));

	return Result(ret, root);
}

static int simple_vector_concatenate_sequence_(addr *ret, addr type, addr rest)
{
	addr root;
	size_t size;

	/* type check */
	if (LowLispDecl(type) != LISPDECL_SIMPLE_VECTOR)
		return Result(ret, Unbound);

	/* concatenate */
	Return(length_concatenate_sequence_(rest, &size));
	vector_heap(&root, size);
	Return(value_concatenate_sequence_(root, rest));

	return Result(ret, root);
}

static int string_concatenate_sequence_(addr *ret, addr type, addr rest)
{
	addr root;
	size_t size;

	/* type check */
	if (! type_string_p(type))
		return Result(ret, Unbound);

	/* concatenate */
	Return(length_concatenate_sequence_(rest, &size));
	strvect_heap(&root, size);
	Return(value_concatenate_sequence_(root, rest));

	return Result(ret, root);
}

static int array_concatenate_sequence_(addr *ret, addr type, addr rest)
{
	enum LISPDECL decl;
	addr root;
	size_t size;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY)
		return Result(ret, Unbound);

	/* concatenate */
	Return(length_concatenate_sequence_(rest, &size));
	Return(array_upgraded_merge_sequence_(&root, type, size));
	Return(value_concatenate_sequence_(root, rest));

	return Result(ret, root);
}

static int bitvector_concatenate_sequence_(addr *ret, addr type, addr rest)
{
	enum LISPDECL decl;
	addr root;
	size_t size;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_BIT_VECTOR && decl != LISPDECL_SIMPLE_BIT_VECTOR)
		return Result(ret, Unbound);

	/* concatenate */
	Return(length_concatenate_sequence_(rest, &size));
	bitmemory_unsafe(NULL, &root, size);
	Return(value_concatenate_sequence_(root, rest));

	return Result(ret, root);
}

static int type_concatenate_sequence_(Execute ptr, addr *ret, addr type, addr rest)
{
	addr pos;

	/* list */
	Return(list_concatenate_sequence_(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* vector */
	Return(vector_concatenate_sequence_(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* simple-vector */
	Return(simple_vector_concatenate_sequence_(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* string */
	Return(string_concatenate_sequence_(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* array */
	Return(array_concatenate_sequence_(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* bitvector */
	Return(bitvector_concatenate_sequence_(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* error */
	*ret = Nil;
	return call_type_error_va_(ptr, type, Nil,
			"Invalid type-specifier ~S.", type, NULL);
}

int concatenate_common_(Execute ptr, addr *ret, addr type, addr rest)
{
	addr check;

	Return(parse_type_(ptr, &check, type, Nil));
	Return(type_concatenate_sequence_(ptr, ret, check, rest));

	return call_typep_asterisk_error_(ptr, *ret, check);
}


/************************************************************
 *  call_streams.c
 ************************************************************/

/*
 *  read-byte
 */
int read_byte_common_(Execute ptr,
		addr stream, addr errorp, addr value, addr *ret)
{
	int check;

	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;

	Return(read_byte_stream_(stream, ret, &check));
	if (! check)
		return 0;

	/* EOF */
	if (errorp != Nil) {
		*ret = Nil;
		return call_end_of_file_(ptr, stream);
	}
	return Result(ret, value);
}


/*
 *  write-byte
 */
int write_byte_common_(Execute ptr, addr value, addr stream)
{
	Return(write_byte_stream_(stream, value));
	return exitpoint_stream_(stream);
}


/*
 *  peek-char
 */
int peek_char_common_(Execute ptr, addr type, addr stream,
		addr errorp, addr value, addr recp, addr *ret)
{
	if (type == Unbound)
		type = Nil;
	if (stream == Unbound) {
		Return(standard_input_stream_(ptr, &stream));
	}
	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	if (recp == Unbound)
		recp = Nil;
	return peek_char_stream_(ptr, ret,
			type, stream, errorp != Nil, value, recp != Nil);
}


/*
 *  read-char
 */
static int call_end_of_file_recursive_(Execute ptr, addr pos, int recp)
{
	return call_end_of_file_(ptr, pos);
}

int read_char_common_(Execute ptr,
		addr stream, addr errorp, addr value, addr recp, addr *ret)
{
	int check;
	unicode c;

	if (stream == Unbound) {
		Return(standard_input_stream_(ptr, &stream));
	}
	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	if (recp == Unbound)
		recp = Nil;
	Return(input_stream_designator_(ptr, stream, &stream));

	/* read-char */
	Return(read_char_stream_(stream, &c, &check));
	if (check) {
		if (errorp != Nil)
			return call_end_of_file_recursive_(ptr, stream, recp != Nil);
		else
			return Result(ret, value);
	}
	character_heap(ret, c);
	return 0;
}


/*
 *  read-char-no-hang
 */
int read_char_no_hang_common_(Execute ptr,
		addr stream, addr errorp, addr value, addr recp, addr *ret)
{
	int hang, check;
	unicode c;

	if (stream == Unbound) {
		Return(standard_input_stream_(ptr, &stream));
	}
	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	if (recp == Unbound)
		recp = Nil;
	Return(input_stream_designator_(ptr, stream, &stream));

	/* read-char */
	Return(read_hang_stream_(stream, &c, &hang, &check));
	if (check) {
		if (errorp != Nil)
			return call_end_of_file_recursive_(ptr, stream, recp != Nil);
		else
			return Result(ret, value);
	}
	if (hang)
		return Result(ret, Nil);
	character_heap(ret, c);
	return 0;
}


/*
 *  terpri
 */
int terpri_common_(Execute ptr, addr stream)
{
	if (stream == Unbound) {
		Return(standard_output_stream_(ptr, &stream));
	}
	else {
		Return(output_stream_designator_(ptr, stream, &stream));
	}
	Return(terpri_stream_(stream));
	return exitpoint_stream_(stream);
}


/*
 *  fresh-line
 */
int fresh_line_common_(Execute ptr, addr stream, addr *ret)
{
	int check;

	if (stream == Unbound) {
		Return(standard_output_stream_(ptr, &stream));
	}
	else {
		Return(output_stream_designator_(ptr, stream, &stream));
	}
	Return(fresh_line_stream_(stream, &check));
	Return(exitpoint_stream_(stream));

	return Result(ret, check? T: Nil);
}


/*
 *  unread-char
 */
int unread_char_common_(Execute ptr, addr pos, addr stream)
{
	unicode c;

	if (stream == Unbound) {
		Return(standard_input_stream_(ptr, &stream));
	}
	else {
		Return(input_stream_designator_(ptr, stream, &stream));
	}
	GetCharacter(pos, &c);
	return unread_char_stream_(stream, c);
}


/*
 *  write-char
 */
int write_char_common_(Execute ptr, addr pos, addr stream)
{
	unicode c;

	Return(output_stream_designator_(ptr, stream, &stream));
	GetCharacter(pos, &c);
	Return(write_char_stream_(stream, c));
	return exitpoint_stream_(stream);
}


/*
 *  read-line
 */
int read_line_common_(Execute ptr,
		addr stream, addr errorp, addr value, addr recp,
		addr *ret, addr *sec)
{
	int miss;

	if (stream == Unbound) {
		Return(standard_input_stream_(ptr, &stream));
	}
	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	if (recp == Unbound)
		recp = Nil;
	Return(read_line_stream_(ptr, ret,
				&miss, stream, errorp != Nil, value, recp != Nil));
	*sec = miss? T: Nil;

	return 0;
}


/*
 *  write-string
 */
int write_string_common_(Execute ptr, addr string, addr rest)
{
	Return(write_string_stream_(ptr, string, rest, &string));
	return exitpoint_stream_(string);
}


/*
 *  write-line
 */
int write_line_common_(Execute ptr, addr string, addr rest)
{
	Return(write_string_stream_(ptr, string, rest, &string));
	Return(terpri_stream_(string));
	return exitpoint_stream_(string);
}


/*
 *  read-sequence
 */
int read_sequence_common_(addr var, addr stream, addr rest, addr *ret)
{
	size_t start, end;

	Return(length_sequence_(var, 0, &start));
	Return(keyword_start_end_(start, rest, &start, &end));
	return read_sequence_stream_(ret, var, stream, start, end);
}


/*
 *  write-sequence
 */
int write_sequence_common_(LocalRoot local, addr var, addr stream, addr rest)
{
	size_t start, end;

	Return(length_sequence_(var, 0, &start));
	Return(keyword_start_end_(start, rest, &start, &end));
	Return(write_sequence_stream_(local, var, stream, start, end));
	return exitpoint_stream_(stream);
}


/*
 *  file-position
 */
int file_position_common_(Execute ptr, addr stream, addr pos, addr *ret)
{
	int check;
	addr value;
	size_t size;

	/* get file-position */
	if (pos == Unbound) {
		Return(file_position_stream_(stream, &size, &check));
		if (check)
			return Result(ret, Nil);
		else
			return Result(ret, intsizeh(size));
	}

	/* set start */
	GetConst(KEYWORD_START, &value);
	if (pos == value) {
		Return(file_position_start_stream_(stream, &check));
		goto return_result;
	}

	/* set end */
	GetConst(KEYWORD_END, &value);
	if (pos == value) {
		Return(file_position_end_stream_(stream, &check));
		goto return_result;
	}

	/* set index */
	Return(getindex_integer_(pos, &size));
	Return(file_position_set_stream_(stream, size, &check));

return_result:
	return Result(ret, check? Nil: T);
}


/*
 *  file-string-length
 */
int file_string_length_common_(addr stream, addr pos, addr *ret)
{
	int check;
	unicode c;
	size_t size;

	if (characterp(pos)) {
		GetCharacter(pos, &c);
		Return(file_charlen_stream_(stream, c, &size, &check));
	}
	else {
		Return(file_strlen_stream_(stream, pos, &size, &check));
	}
	if (check) {
		return Result(ret, Nil);
	}
	else {
		make_index_integer_heap(&pos, size);
		return Result(ret, pos);
	}
}


/*
 *  open
 */
static int open_common_direction_(addr value, enum Stream_Open_Direction *ret)
{
	addr check;

	/* default */
	if (value == Unbound)
		return Result(ret, Stream_Open_Direction_Input);

	/* :input */
	GetConst(KEYWORD_INPUT, &check);
	if (value == check)
		return Result(ret, Stream_Open_Direction_Input);

	/* :output */
	GetConst(KEYWORD_OUTPUT, &check);
	if (value == check)
		return Result(ret, Stream_Open_Direction_Output);

	/* :io */
	GetConst(KEYWORD_IO, &check);
	if (value == check)
		return Result(ret, Stream_Open_Direction_Io);

	/* :probe */
	GetConst(KEYWORD_PROBE, &check);
	if (value == check)
		return Result(ret, Stream_Open_Direction_Probe);

	/* error */
	*ret = Stream_Open_Direction_Input;
	return fmte_("Invalid :direction value ~S.", value, NULL);
}

static int open_common_newest_p(addr pos)
{
	addr version, x, y;

	/* version == :newest */
	GetVersionPathname(pos, &version);
	GetConst(KEYWORD_NEWEST, &y);
	if (version == y)
		return 1;

	/* logical-pathname.version == Nil */
	GetHostPathname(pos, &x);
	if (! stringp(x))
		return 0;

	return version == Nil;
}

static int open_common_ifexists_(addr value, addr pos, enum Stream_Open_IfExists *ret)
{
	addr check;

	/* default */
	if (value == Unbound) {
		/* binary-stream */
		if (streamp(pos))
			return Result(ret, Stream_Open_IfExists_Supersede);
		/* pathname */
		return Result(ret, open_common_newest_p(pos)?
				Stream_Open_IfExists_NewVersion:
				Stream_Open_IfExists_Error);
	}

	/* :error */
	GetConst(KEYWORD_ERROR, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfExists_Error);

	/* :supersede */
	GetConst(KEYWORD_SUPERSEDE, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfExists_Supersede);

	/* :append */
	GetConst(KEYWORD_APPEND, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfExists_Append);

	/* :overwrite */
	GetConst(KEYWORD_OVERWRITE, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfExists_Overwrite);

	/* :rename */
	GetConst(KEYWORD_RENAME, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfExists_Rename);

	/* :rename-and-delete */
	GetConst(KEYWORD_RENAME_AND_DELETE, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfExists_RenameAndDelete);

	/* :new-version */
	GetConst(KEYWORD_NEW_VERSION, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfExists_NewVersion);

	/* nil */
	if (value == Nil)
		return Result(ret, Stream_Open_IfExists_Nil);

	/* others */
	*ret = Stream_Open_IfExists_Error;
	return fmte_("Invalid :if-exists value ~S.", value, NULL);
}

static int open_common_ifdoesnot_(addr value,
		enum Stream_Open_Direction direction,
		enum Stream_Open_IfExists exists,
		enum Stream_Open_IfDoesNot *ret)
{
	addr check;

	/* default */
	if (value == Unbound) {
		/* :input     -> :error
		 * :overwrite -> :error
		 * :append    -> :error
		 * :output    -> :create
		 * :io        -> :create
		 * :probe     -> nil
		 */
		if (direction == Stream_Open_Direction_Input)
			return Result(ret, Stream_Open_IfDoesNot_Error);
		if (exists == Stream_Open_IfExists_Overwrite)
			return Result(ret, Stream_Open_IfDoesNot_Error);
		if (exists == Stream_Open_IfExists_Append)
			return Result(ret, Stream_Open_IfDoesNot_Error);
		if (direction == Stream_Open_Direction_Output)
			return Result(ret, Stream_Open_IfDoesNot_Create);
		if (direction == Stream_Open_Direction_Io)
			return Result(ret, Stream_Open_IfDoesNot_Create);
		if (direction == Stream_Open_Direction_Probe)
			return Result(ret, Stream_Open_IfDoesNot_Nil);
		*ret = Stream_Open_IfDoesNot_Error;
		return fmte_("Invalid :if-does-not-exist default value.", NULL);
	}

	/* :error */
	GetConst(KEYWORD_ERROR, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfDoesNot_Error);

	/* :create */
	GetConst(KEYWORD_CREATE, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfDoesNot_Create);

	/* nil */
	if (value == Nil)
		return Result(ret, Stream_Open_IfDoesNot_Nil);

	/* others */
	*ret = Stream_Open_IfDoesNot_Error;
	return fmte_("Invalid :if-does-not-exist value ~S.", value, NULL);
}

static int open_pathname_designator_(Execute ptr, addr pos, addr *ret)
{
	addr value;

	if (memory_stream_p(pos))
		return Result(ret, pos);

	if (streamp(pos)) {
		GetPathnameStream(pos, &value);
		if (memory_stream_p(value))
			return Result(ret, value);
	}

	return pathname_designator_heap_(ptr, pos, ret);
}

int open_common_(Execute ptr, addr pos, addr rest, addr *ret)
{
	addr value;
	enum Stream_Open_Direction direction;
	enum Stream_Open_Element element;
	enum Stream_Open_IfExists exists;
	enum Stream_Open_IfDoesNot doesnot;
	enum Stream_Open_External external;

	/* argument */
	Return(open_pathname_designator_(ptr, pos, &pos));
	if (GetKeyArgs(rest, KEYWORD_DIRECTION, &value))
		value = Unbound;
	Return(open_common_direction_(value, &direction));
	if (GetKeyArgs(rest, KEYWORD_ELEMENT_TYPE, &value))
		value = Unbound;
	Return(open_element_stream_(ptr, value, &element));
	if (GetKeyArgs(rest, KEYWORD_IF_EXISTS, &value))
		value = Unbound;
	Return(open_common_ifexists_(value, pos, &exists));
	if (GetKeyArgs(rest, KEYWORD_IF_DOES_NOT_EXIST, &value))
		value = Unbound;
	Return(open_common_ifdoesnot_(value, direction, exists, &doesnot));
	if (GetKeyArgs(rest, KEYWORD_EXTERNAL_FORMAT, &value))
		value = Unbound;
	Return(open_external_format_(ptr, value, &external));
	if (external == Stream_Open_External_Error)
		return fmte_("Invalid external-format ~S.", value, NULL);

	/* result */
	return open_stream_(ptr, ret, pos, direction, element, exists, doesnot, external);
}


/*
 *  with-open-file
 */
int with_open_file_common_(addr form, addr *ret)
{
	/* (let ((var (open file . args)))
	 *   ,@decl
	 *   (unwind-protect
	 *     (progn . body)
	 *     (when var  ;; valid check.
	 *       (close var))))
	 */
	addr args, var, file, body, decl, root;
	addr let, open, protect, progn, when, close;

	/* argument */
	Return_getcdr(form, &form);
	if (! consp(form))
		goto error;
	GetCons(form, &args, &body);
	if (! consp(args))
		goto error;
	GetCons(args, &var, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &file, &args);
	Return(declare_body_form_(body, &decl, &body));

	/* expand */
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_OPEN, &open);
	GetConst(COMMON_UNWIND_PROTECT, &protect);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_WHEN, &when);
	GetConst(COMMON_CLOSE, &close);
	list_heap(&close, close, var, NULL);
	list_heap(&when, when, var, close, NULL);
	cons_heap(&progn, progn, body);
	list_heap(&protect, protect, progn, when, NULL);
	lista_heap(&args, open, file, args, NULL);
	list_heap(&args, var, args, NULL);
	conscar_heap(&args, args);
	conscar_heap(&root, let);
	cons_heap(&root, args, root);
	while (decl != Nil) {
		GetCons(decl, &var, &decl);
		cons_heap(&root, var, root);
	}
	cons_heap(&root, protect, root);
	nreverse(ret, root);
	return 0;

error:
	return fmte_("WITH-OPEN-FILE argument must be "
			"a ((var file options*) ...) form.", form, NULL);
}


/*
 *  close
 */
int close_common_(Execute ptr, addr pos, addr rest, addr *ret)
{
	addr abort;

	if (GetKeyArgs(rest, KEYWORD_ABORT, &abort))
		abort = Nil;
	if (abort != Nil) {
		GetConst(SYSTEM_CLOSE_ABORT, &abort);
		pushspecial_control(ptr, abort, T);
	}

	return close_stream_(pos, ret);
}


/*
 *  with-open-stream
 */
int with_open_stream_common_(addr form, addr *ret)
{
	/* `(let ((,var ,stream))
	 *   ,@decl
	 *   (unwind-protect
	 *     (progn ,@body)
	 *     (close ,var)))
	 */
	addr args, var, stream, body, decl, root;
	addr let, protect, progn, close;

	/* argument */
	Return_getcdr(form, &form);
	if (! consp(form))
		goto error;
	GetCons(form, &args, &body);
	if (! consp(args))
		goto error;
	GetCons(args, &var, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &stream, &args);
	if (args != Nil)
		goto error;
	Return(declare_body_form_(body, &decl, &body));

	/* expand */
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_UNWIND_PROTECT, &protect);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_CLOSE, &close);
	list_heap(&close, close, var, NULL);
	cons_heap(&progn, progn, body);
	list_heap(&protect, protect, progn, close, NULL);
	list_heap(&args, var, stream, NULL);
	conscar_heap(&args, args);
	conscar_heap(&root, let);
	cons_heap(&root, args, root);
	while (decl != Nil) {
		GetCons(decl, &var, &decl);
		cons_heap(&root, var, root);
	}
	cons_heap(&root, protect, root);
	nreverse(ret, root);
	return 0;

error:
	return fmte_("WITH-OPEN-STREAM argument must be "
			"a ((var stream) ...) form.", form, NULL);
}


/*
 *  listen
 */
int listen_common_(Execute ptr, addr stream, addr *ret)
{
	int check;

	if (stream == Unbound) {
		Return(standard_input_stream_(ptr, &stream));
	}
	Return(listen_stream_(stream, &check));

	return Result(ret, check? T: Nil);
}


/*
 *  clear-input
 */
int clear_input_common_(Execute ptr, addr stream)
{
	if (stream == Unbound) {
		Return(standard_input_stream_(ptr, &stream));
	}
	return clear_input_stream_(stream);
}


/*
 *  finish-output
 */
int finish_output_common_(Execute ptr, addr stream)
{
	if (stream == Unbound) {
		Return(standard_output_stream_(ptr, &stream));
	}
	return finish_output_stream_(stream);
}


/*
 *  force-output
 */
int force_output_common_(Execute ptr, addr stream)
{
	if (stream == Unbound) {
		Return(standard_output_stream_(ptr, &stream));
	}
	return force_output_stream_(stream);
}


/*
 *  clear-output
 */
int clear_output_common_(Execute ptr, addr stream)
{
	if (stream == Unbound) {
		Return(standard_output_stream_(ptr, &stream));
	}
	return clear_output_stream_(stream);
}


/*
 *  make-string-input-stream
 */
int make_string_input_stream_common_(addr var, addr x, addr y, addr *ret)
{
	size_t start, end;

	string_length(var, &start);
	Return(keyword_start_end_value_(start, x, y, &start, &end));
	Return(open_input_string_stream2_(ret, var, start, end));

	return 0;
}


/*
 *  make-string-output-stream
 */
int make_string_output_stream_common_(Execute ptr, addr rest, addr *ret)
{
	int check;
	addr type, pos;

	if (! GetKeyArgs(rest, KEYWORD_ELEMENT_TYPE, &pos)) {
		GetTypeTable(&type, Character);
		Return(parse_type_(ptr, &pos, pos, Nil));
		Return(subtypep_check_(ptr, pos, type, Nil, &check, NULL));
		if (! check)
			return fmte_(":ELEMENT-TYPE ~S must be a character type.", pos, NULL);
	}
	open_output_string_stream(ret, 0);

	return 0;
}


/*
 * get-output-stream-string
 */
int get_output_stream_string_common_(Execute ptr, addr var, addr *ret)
{
	addr type;

	if (getstreamtype(var) != StreamType_StringOutput) {
		GetTypeTable(&type, StringStream);
		return call_type_error_va_(ptr, var, type,
				"The stream must be a output-string-stream.", NULL);
	}

	Return(string_stream_heap_(var, ret));
	clear_output_string_stream(var);
	return 0;
}


/*
 *  with-input-from-string
 */
static int with_input_from_string_noindex_common_(addr *ret,
		addr var, addr string, addr start, addr end, addr body)
{
	/* `(let ((,var (make-string-input-stream ,string :start ,start :end ,end)))
	 *    ,@decl
	 *    (unwind-protect
	 *      (progn ,@form)
	 *      (close ,var)))
	 */
	addr let, make, unwind, progn, close, decl, pos;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_MAKE_STRING_INPUT_STREAM, &make);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_CLOSE, &close);
	Return(declare_body_form_(body, &decl, &body));
	list_heap(&close, close, var, NULL);
	cons_heap(&progn, progn, body);
	list_heap(&unwind, unwind, progn, close, NULL);
	if (end == Unbound)
		list_heap(&make, make, string, start, NULL);
	else
		list_heap(&make, make, string, start, end, NULL);
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
}

static int with_input_from_string_index_common_(addr *ret,
		addr var, addr string, addr index, addr start, addr end, addr body)
{
	/* `(let ((,var (make-string-input-stream ,string :start ,start :end ,end)))
	 *    ,@decl
	 *    (unwind-protect
	 *      (progn ,@form)
	 *      (setf ,index (lisp-system::end-input-stream ,var))
	 *      (close ,var)))
	 */
	addr let, make, unwind, progn, setf, end_input, close, decl, pos;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_MAKE_STRING_INPUT_STREAM, &make);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_SETF, &setf);
	GetConst(SYSTEM_END_INPUT_STREAM, &end_input);
	GetConst(COMMON_CLOSE, &close);
	Return(declare_body_form_(body, &decl, &body));
	list_heap(&close, close, var, NULL);
	list_heap(&end_input, end_input, var, NULL);
	list_heap(&setf, setf, index, end_input, NULL);
	cons_heap(&progn, progn, body);
	list_heap(&unwind, unwind, progn, setf, close, NULL);
	if (end == Unbound)
		list_heap(&make, make, string, start, NULL);
	else
		list_heap(&make, make, string, start, end, NULL);
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
}

static int with_input_from_string_key_(addr list, addr *start, addr *end, addr *index)
{
	addr key, value, kstart, kend, kindex;

	GetConst(KEYWORD_INDEX, &kindex);
	GetConst(KEYWORD_START, &kstart);
	GetConst(KEYWORD_END, &kend);
	*start = *end = *index = Unbound;
	while (list != Nil) {
		Return_getcons(list, &key, &list);
		Return_getcons(list, &value, &list);
		if (key == kindex) {
			if (*index == Unbound)
				*index = value;
		}
		else if (key == kstart) {
			if (*start == Unbound)
				*start = value;
		}
		else if (key == kend) {
			if (*end == Unbound)
				*end = value;
		}
		else {
			return fmte_("Invaild key argument ~S.", key, NULL);
		}
	}
	if (*start == Unbound)
		fixnum_heap(start, 0);

	return 0;
}

int with_input_from_string_common_(addr form, addr *ret)
{
	addr args, body, var, string, start, end, index;

	/* argument */
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &args, &body))
		goto error;
	if (! consp_getcons(args, &var, &args))
		goto error;
	if (! consp_getcons(args, &string, &args))
		goto error;
	/* make form */
	Return(with_input_from_string_key_(args, &start, &end, &index));
	if (index == Unbound) {
		return with_input_from_string_noindex_common_(ret,
				var, string, start, end, body);
	}
	else {
		return with_input_from_string_index_common_(ret,
				var, string, index, start, end, body);
	}

error:
	return fmte_("WITH-INPUT-FROM-STRING form ~S must be a "
			"((var string ...) &body body).", form, NULL);
}


/*
 *  with-output-to-string
 */
static int with_output_to_string_normal_common_(addr *ret,
		addr var, addr args, addr body)
{
	/* `(let ((,var (make-string-output-stream ,@args)))
	 *    ,@decl
	 *    (unwind-protect
	 *      (progn ,@body
	 *             (get-output-stream-string ,var))
	 *      (close ,var)))
	 */
	addr let, make, unwind, progn, get, close, decl, pos;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_MAKE_STRING_OUTPUT_STREAM, &make);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_GET_OUTPUT_STREAM_STRING, &get);
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
	lista_heap(&make, make, args, NULL);
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
}

static int with_output_to_string_extend_common_(addr *ret,
		addr var, addr string, addr args, addr body)
{
	/* `(let ((,var (lisp-system::make-extend-output-stream string ,@args)))
	 *    ,@decl
	 *    (unwind-protect
	 *      (progn ,@body)
	 *      (close ,var)))
	 */
	addr let, make, unwind, progn, close, decl, pos;

	GetConst(COMMON_LET, &let);
	GetConst(SYSTEM_MAKE_EXTEND_OUTPUT_STREAM, &make);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_CLOSE, &close);
	Return(declare_body_form_(body, &decl, &body));
	list_heap(&close, close, var, NULL);
	cons_heap(&progn, progn, body);
	list_heap(&unwind, unwind, progn, close, NULL);
	lista_heap(&make, make, string, args, NULL);
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
}

int with_output_to_string_common_(addr form, addr *ret)
{
	addr args, var, string, body;

	/* argument */
	Return_getcdr(form, &args);
	if (! consp_getcons(args, &args, &body))
		goto error;
	if (! consp_getcons(args, &var, &args))
		goto error;
	if (! consp_getcons(args, &string, &args))
		string = Nil;
	if (string == Nil) {
		Return(with_output_to_string_normal_common_(ret, var, args, body));
	}
	else {
		Return(with_output_to_string_extend_common_(ret, var, string, args, body));
	}
	return 0;

error:
	return fmte_("WITH-OUTPUT-TO-STRING form ~S must be a "
			"((var &optional string &key element-type) &body body).", form, NULL);
}


/************************************************************
 *  call_strings.c
 ************************************************************/

/*
 *  simple-string-p
 */
void simple_string_p_common(addr var, addr *ret)
{
	int check;

	switch (GetType(var)) {
		case LISPTYPE_STRING:
			check = 1;
			break;

		case LISPTYPE_ARRAY:
			check = array_simple_p(var) && array_stringp(var);
			break;

		default:
			check = 0;
			break;
	}
	*ret = check? T: Nil;
}


/*
 *  char
 */
int char_common_(addr str, addr pos, addr *ret)
{
	unicode c;
	size_t index, size;

	if (GetIndex_integer(pos, &index))
		goto error;
	if (GetType(str) == LISPTYPE_STRING) {
		strvect_length(str, &size);
		if (size <= index)
			goto error;
		strvect_getc(str, index, &c);
	}
	else if (strarrayp(str)) {
		strarray_length_buffer(str, &size); /* Don't use strarray_length */
		if (size <= index)
			goto error;
		Return(strarray_getc_(str, index, &c));
	}
	else {
		return TypeError_(str, STRING);
	}
	character_heap(ret, c);
	return 0;

error:
	*ret = Nil;
	return fmte_("Too large index value ~S at ~S.", pos, str, NULL);
}


/*
 *  (setf char)
 */
int setf_char_common_(addr value, addr str, addr pos)
{
	size_t size, index;
	unicode c;

	if (GetIndex_integer(pos, &index))
		goto error;
	GetCharacter(value, &c);
	switch (GetType(str)) {
		case LISPTYPE_STRING:
			strvect_length(str, &size);
			if (size <= index)
				goto error;
			return strvect_setc_(str, index, c);

		case LISPTYPE_ARRAY:
			if (! array_stringp(str))
				return TypeError_(str, STRING);
			strarray_length_buffer(str, &size); /* Don't use strarray_length */
			if (size <= index)
				goto error;
			return array_set_character_(str, index, c);

		default:
			break;
	}
	return TypeError_(str, STRING);

error:
	return fmte_("Too large index value ~S at ~S.", pos, str, NULL);
}


/*
 *  string
 */
int string_common_(addr var, addr *ret)
{
	int check;

	Return(string_designator_heap_(ret, var, &check));
	if (! check)
		return TypeError_(var, STRING);

	return 0;
}


/*
 *  string-upcase
 */
static int string_case_common_(addr var, addr rest, addr *ret,
		int (*call)(size_t, size_t, addr, addr, size_t *))
{
	int check;
	addr pos;
	unicode c;
	size_t start, end, size, i;

	Return(string_designator_heap_(&var, var, &check));
	if (! check)
		return TypeError_(var, STRING);
	string_length(var, &size);
	Return(keyword_start_end_(size, rest, &start, &end));
	strvect_heap(&pos, size);

	/* start */
	for (i = 0; i < start; i++) {
		Return(string_getc_(var, i, &c));
		Return(strvect_setc_(pos, i, c));
	}
	/* case */
	Return((*call)(i, end, var, pos, &i));
	/* end */
	for (; i < size; i++) {
		Return(string_getc_(var, i, &c));
		Return(strvect_setc_(pos, i, c));
	}

	/* result */
	return Result(ret, pos);
}

static int string_upcase_call_common_(
		size_t i, size_t end, addr var, addr pos, size_t *ret)
{
	unicode c;

	for (; i < end; i++) {
		Return(string_getc_(var, i, &c));
		Return(strvect_setc_(pos, i, toUpperUnicode(c)));
	}

	return Result(ret, i);
}

int string_upcase_common_(addr var, addr rest, addr *ret)
{
	return string_case_common_(var, rest, ret, string_upcase_call_common_);
}


/*
 *  string-downcase
 */
static int string_downcase_call_common_(
		size_t i, size_t end, addr var, addr pos, size_t *ret)
{
	unicode c;

	for (; i < end; i++) {
		Return(string_getc_(var, i, &c));
		Return(strvect_setc_(pos, i, toLowerUnicode(c)));
	}

	return Result(ret, i);
}

int string_downcase_common_(addr var, addr rest, addr *ret)
{
	return string_case_common_(var, rest, ret, string_downcase_call_common_);
}


/*
 *  string-capitalize
 */
static int string_capitalize_call_common_(
		size_t i, size_t end, addr var, addr pos, size_t *ret)
{
	int alphabet, mode;
	unicode c;

	mode = alphabet = 0;
	for (; i < end; i++) {
		Return(string_getc_(var, i, &c));
		if (mode == 0) {
			/* not alphabet */
			if (isAlphanumeric(c)) {
				alphabet = 1; /* upper */
				mode = 1;
			}
			else {
				alphabet = 0; /* not alphabet */
			}
		}
		else {
			/* alphabet */
			if (isAlphanumeric(c)) {
				alphabet = 2; /* lower */
			}
			else {
				alphabet = 0; /* not alphabet */
				mode = 0;
			}
		}
		switch (alphabet) {
			case 1: c = toUpperUnicode(c); break;
			case 2: c = toLowerUnicode(c); break;
			default: break;
		}
		Return(strvect_setc_(pos, i, c));
	}

	return Result(ret, i);
}

int string_capitalize_common_(addr var, addr rest, addr *ret)
{
	return string_case_common_(var, rest, ret, string_capitalize_call_common_);
}


/*
 *  nstring-upcase
 */
static int nstring_case_common_(addr var, addr rest,
		int (*call)(size_t, size_t, addr, addr, size_t *))
{
	size_t start, end, size;

	if (GetStatusReadOnly(var))
		return fmte_("Cannot update the constant object ~S.", var, NULL);
	string_length(var, &size);
	Return(keyword_start_end_(size, rest, &start, &end));
	return (*call)(start, end, var, var, &size);
}

int nstring_upcase_common_(addr var, addr rest)
{
	return nstring_case_common_(var, rest, string_upcase_call_common_);
}


/*
 *  nstring-downcase
 */
int nstring_downcase_common_(addr var, addr rest)
{
	return nstring_case_common_(var, rest, string_downcase_call_common_);
}


/*
 *  nstring-capitalize
 */
int nstring_capitalize_common_(addr var, addr rest)
{
	return nstring_case_common_(var, rest, string_capitalize_call_common_);
}


/*
 *  string-trim
 */
static int string_trim_string_common_(addr string, unicode c, int *ret)
{
	unicode check;
	size_t i, size;

	string_length(string, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(string, i, &check));
		if (check == c)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int string_trim_list_common_(addr list, unicode c, int *ret)
{
	unicode check;
	addr pos;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (GetType(pos) == LISPTYPE_CHARACTER) {
			GetCharacter(pos, &check);
			if (check == c)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int string_trim_vector_common_(addr vector, unicode c, int *ret)
{
	unicode check;
	addr pos;
	size_t size, i;

	lenarray(vector, &size);
	for (i = 0; i < size; i++) {
		getarray(vector, i, &pos);
		if (GetType(pos) == LISPTYPE_CHARACTER) {
			GetCharacter(pos, &check);
			if (check == c)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int string_trim_array_common_(addr pos, unicode c, int *ret)
{
	unicode check;
	size_t size, i;

	if (array_stringp(pos))
		return string_trim_string_common_(pos, c, ret);
	if (! array_vector_p(pos))
		return TypeError_(pos, SEQUENCE);
	Return(array_get_vector_length_(pos, 1, &size));
	for (i = 0; i < size; i++) {
		Return(array_get_unicode_(pos, i, &check));
		if (check == c)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int string_trim_sequence_common_(addr seq, unicode c, int *ret)
{
	switch (GetType(seq)) {
		case LISPTYPE_NIL:
			return Result(ret, 0);

		case LISPTYPE_CONS:
			return string_trim_list_common_(seq, c, ret);

		case LISPTYPE_STRING:
			return string_trim_string_common_(seq, c, ret);

		case LISPTYPE_VECTOR:
			return string_trim_vector_common_(seq, c, ret);

		case LISPTYPE_ARRAY:
			return string_trim_array_common_(seq, c, ret);

		default:
			return TypeError_(seq, SEQUENCE);
	}
}

static int string_trim_start_common_(addr seq, addr var, size_t *start, size_t end)
{
	int check;
	unicode c;
	size_t i;

	for (i = *start; i < end; i++) {
		Return(string_getc_(var, i, &c));
		Return(string_trim_sequence_common_(seq, c, &check));
		if (! check)
			break;
	}

	return Result(start, i);
}

static int string_trim_end_common_(addr seq, addr var, size_t start, size_t *end)
{
	int check;
	unicode c;
	size_t i;

	for (i = *end; start < i; i--) {
		Return(string_getc_(var, i - 1, &c));
		Return(string_trim_sequence_common_(seq, c, &check));
		if (! check)
			break;
	}

	return Result(end, i);
}

int string_trim_common_(addr trim, addr pos, addr *ret)
{
	int check;
	unicode c;
	size_t start, end, size, i;

	Return(string_designator_heap_(&pos, pos, &check));
	if (! check)
		return TypeError_(pos, STRING);
	start = 0;
	string_length(pos, &end);
	Return(string_trim_start_common_(trim, pos, &start, end));
	if (end <= start)
		goto null_string;
	Return(string_trim_end_common_(trim, pos, start, &end));
	if (end <= start)
		goto null_string;
	/* new string */
	size = end - start;
	strvect_heap(&trim, size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i + start, &c));
		Return(strvect_setc_(trim, i, c));
	}
	return Result(ret, trim);

null_string:
	strvect_heap(ret, 0);
	return 0;
}


/*
 *  string-left-trim
 */
int string_left_trim_common_(addr trim, addr pos, addr *ret)
{
	int check;
	unicode c;
	size_t start, end, size, i;

	Return(string_designator_heap_(&pos, pos, &check));
	if (! check)
		return TypeError_(pos, STRING);
	start = 0;
	string_length(pos, &end);
	Return(string_trim_start_common_(trim, pos, &start, end));
	if (end <= start)
		goto null_string;
	/* new string */
	size = end - start;
	strvect_heap(&trim, size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i + start, &c));
		Return(strvect_setc_(trim, i, c));
	}
	return Result(ret, trim);

null_string:
	strvect_heap(ret, 0);
	return 0;
}


/*
 *  string-right-trim
 */
int string_right_trim_common_(addr trim, addr pos, addr *ret)
{
	int check;
	unicode c;
	size_t start, end, size, i;

	Return(string_designator_heap_(&pos, pos, &check));
	if (! check)
		return TypeError_(pos, STRING);
	start = 0;
	string_length(pos, &end);
	Return(string_trim_end_common_(trim, pos, start, &end));
	if (end <= start)
		goto null_string;
	/* new string */
	size = end - start;
	strvect_heap(&trim, size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i + start, &c));
		Return(strvect_setc_(trim, i, c));
	}
	return Result(ret, trim);

null_string:
	strvect_heap(ret, 0);
	return 0;
}


/*
 *  string=
 */
int string_eql_common_(addr var1, addr var2, addr rest, addr *ret)
{
	int check;
	size_t size1, size2, start1, start2, end1, end2;
	size_t diff1, diff2, i;
	unicode a, b;

	Return(string_designator_heap_(&var1, var1, &check));
	if (! check)
		return TypeError_(var1, STRING);
	Return(string_designator_heap_(&var2, var2, &check));
	if (! check)
		return TypeError_(var2, STRING);
	string_length(var1, &size1);
	Return(keyword_start1_end1_(size1, rest, &start1, &end1));
	string_length(var2, &size2);
	Return(keyword_start2_end2_(size2, rest, &start2, &end2));
	diff1 = end1 - start1;
	diff2 = end2 - start2;
	if (diff1 != diff2)
		return Result(ret, Nil);
	for (i = 0; i < diff1; i++) {
		Return(string_getc_(var1, start1 + i, &a));
		Return(string_getc_(var2, start2 + i, &b));
		if (a != b)
			return Result(ret, Nil);
	}

	return Result(ret, T);
}


/*
 *  string/=
 */
static int string_call_common_(addr var1, addr var2, addr rest, addr *ret,
		int (*callu)(unicode, unicode),
		int (*calli)(size_t, size_t))
{
	int check;
	size_t size1, size2, start1, start2, end1, end2;
	size_t diff1, diff2, i;
	unicode a, b;

	Return(string_designator_heap_(&var1, var1, &check));
	if (! check)
		return TypeError_(var1, STRING);
	Return(string_designator_heap_(&var2, var2, &check));
	if (! check)
		return TypeError_(var2, STRING);
	string_length(var1, &size1);
	Return(keyword_start1_end1_(size1, rest, &start1, &end1));
	string_length(var2, &size2);
	Return(keyword_start2_end2_(size2, rest, &start2, &end2));
	diff1 = end1 - start1;
	diff2 = end2 - start2;
	for (i = 0; i < diff1 && i < diff2; i++) {
		Return(string_getc_(var1, start1 + i, &a));
		Return(string_getc_(var2, start2 + i, &b));
		if (a != b) {
			if (callu(a, b))
				goto finish;
			else
				goto finish_nil;
		}
	}
	if (calli(diff1, diff2))
		goto finish;

finish_nil:
	return Result(ret, Nil);

finish:
	make_index_integer_heap(ret, start1 + i);
	return 0;
}

static int string_not_equal1_common(unicode a, unicode b)
{
	return a != b;
}
static int string_not_equal2_common(size_t a, size_t b)
{
	return a != b;
}
int string_not_eql_common_(addr var1, addr var2, addr rest, addr *ret)
{
	return string_call_common_(var1, var2, rest, ret,
			string_not_equal1_common,
			string_not_equal2_common);
}


/*
 *  string<
 */
static int string_less1_common(unicode a, unicode b)
{
	return a < b;
}
static int string_less2_common(size_t a, size_t b)
{
	return a < b;
}
int string_less_common_(addr var1, addr var2, addr rest, addr *ret)
{
	return string_call_common_(var1, var2, rest, ret,
			string_less1_common,
			string_less2_common);
}


/*
 *  string>
 */
static int string_greater1_common(unicode a, unicode b)
{
	return a > b;
}
static int string_greater2_common(size_t a, size_t b)
{
	return a > b;
}
int string_greater_common_(addr var1, addr var2, addr rest, addr *ret)
{
	return string_call_common_(var1, var2, rest, ret,
			string_greater1_common,
			string_greater2_common);
}


/*
 *  string<=
 */
static int string_less_equal1_common(unicode a, unicode b)
{
	return a <= b;
}
static int string_less_equal2_common(size_t a, size_t b)
{
	return a <= b;
}
int string_less_equal_common_(addr var1, addr var2, addr rest, addr *ret)
{
	return string_call_common_(var1, var2, rest, ret,
			string_less_equal1_common,
			string_less_equal2_common);
}


/*
 *  string>=
 */
static int string_greater_equal1_common(unicode a, unicode b)
{
	return a >= b;
}
static int string_greater_equal2_common(size_t a, size_t b)
{
	return a >= b;
}
int string_greater_equal_common_(addr var1, addr var2, addr rest, addr *ret)
{
	return string_call_common_(var1, var2, rest, ret,
			string_greater_equal1_common,
			string_greater_equal2_common);
}


/*
 *  string-equal
 */
int string_equal_common_(addr var1, addr var2, addr rest, addr *ret)
{
	int check;
	size_t size1, size2, start1, start2, end1, end2;
	size_t diff1, diff2, i;
	unicode a, b;

	Return(string_designator_heap_(&var1, var1, &check));
	if (! check)
		return TypeError_(var1, STRING);
	Return(string_designator_heap_(&var2, var2, &check));
	if (! check)
		return TypeError_(var2, STRING);
	string_length(var1, &size1);
	Return(keyword_start1_end1_(size1, rest, &start1, &end1));
	string_length(var2, &size2);
	Return(keyword_start2_end2_(size2, rest, &start2, &end2));
	diff1 = end1 - start1;
	diff2 = end2 - start2;
	if (diff1 != diff2)
		return Result(ret, Nil);
	for (i = 0; i < diff1; i++) {
		Return(string_getc_(var1, start1 + i, &a));
		Return(string_getc_(var2, start2 + i, &b));
		if (toUpperUnicode(a) != toUpperUnicode(b))
			return Result(ret, Nil);
	}

	return Result(ret, T);
}


/*
 *  string-not-equal
 */
static int string_callp_common_(addr var1, addr var2, addr rest, addr *ret,
		int (*callu)(unicode, unicode),
		int (*calli)(size_t, size_t))
{
	int check;
	size_t size1, size2, start1, start2, end1, end2;
	size_t diff1, diff2, i;
	unicode a, b;

	Return(string_designator_heap_(&var1, var1, &check));
	if (! check)
		return TypeError_(var1, STRING);
	Return(string_designator_heap_(&var2, var2, &check));
	if (! check)
		return TypeError_(var2, STRING);
	string_length(var1, &size1);
	Return(keyword_start1_end1_(size1, rest, &start1, &end1));
	string_length(var2, &size2);
	Return(keyword_start2_end2_(size2, rest, &start2, &end2));
	diff1 = end1 - start1;
	diff2 = end2 - start2;
	for (i = 0; i < diff1 && i < diff2; i++) {
		Return(string_getc_(var1, start1 + i, &a));
		Return(string_getc_(var2, start2 + i, &b));
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a != b) {
			if (callu(a, b))
				goto finish;
			else
				goto finish_nil;
		}
	}
	if (calli(diff1, diff2))
		goto finish;

finish_nil:
	return Result(ret, Nil);

finish:
	make_index_integer_heap(ret, start1 + i);
	return 0;
}

int string_not_equal_common_(addr var1, addr var2, addr rest, addr *ret)
{
	return string_callp_common_(var1, var2, rest, ret,
			string_not_equal1_common,
			string_not_equal2_common);
}


/*
 *  string-lessp
 */
int string_lessp_common_(addr var1, addr var2, addr rest, addr *ret)
{
	return string_callp_common_(var1, var2, rest, ret,
			string_less1_common,
			string_less2_common);
}


/*
 *  string-greaterp
 */
int string_greaterp_common_(addr var1, addr var2, addr rest, addr *ret)
{
	return string_callp_common_(var1, var2, rest, ret,
			string_greater1_common,
			string_greater2_common);
}


/*
 *  string-not-greaterp
 */
int string_not_greaterp_common_(addr var1, addr var2, addr rest, addr *ret)
{
	return string_callp_common_(var1, var2, rest, ret,
			string_less_equal1_common,
			string_less_equal2_common);
}


/*
 *  string-not-lessp
 */
int string_not_lessp_common_(addr var1, addr var2, addr rest, addr *ret)
{
	return string_callp_common_(var1, var2, rest, ret,
			string_greater_equal1_common,
			string_greater_equal2_common);
}


/*
 *  make-string
 */
int make_string_common_(Execute ptr, addr var, addr rest, addr *ret)
{
	int check;
	addr symbol, value, type;
	unicode c;
	size_t size;

	/* size */
	if (GetIndex_integer(var, &size))
		return fmte_("Too large index value ~S.", var, NULL);

	/* initial-elemnet */
	value = NULL;
	c = 0;
	GetConst(KEYWORD_INITIAL_ELEMENT, &symbol);
	if (getplist_safe(rest, symbol, &value) == 0) {
		if (GetType(value) != LISPTYPE_CHARACTER)
			return fmte_("Invalid :initial-element ~S.", value, NULL);
		GetCharacter(value, &c);
	}

	/* element-type */
	GetConst(KEYWORD_ELEMENT_TYPE, &symbol);
	if (getplist_safe(rest, symbol, &type) == 0) {
		GetTypeTable(&symbol, Character);
		Return(parse_type_(ptr, &type, type, Nil));
		Return(subtypep_check_(ptr, type, symbol, Nil, &check, NULL));
		if (! check) {
			return fmte_(":element-type ~S "
					"must be a subtype of character.", type, NULL);
		}
		/* type check */
		if (value == NULL)
			character_heap(&value, c);
		Return(typep_clang_(ptr, value, type, &check));
		if (! check) {
			return fmte_("The initial-element ~S "
					"must be a ~S type.", value, type, NULL);
		}
	}

	/* make-string */
	strvect_heap(&var, size);
	Return(strvect_setall_(var, c));
	return Result(ret, var);
}


/************************************************************
 *  call_structures.c
 ************************************************************/

/* defstruct-slots */
static int defstruct_parse_slot_(struct defstruct *str, addr pos,
		addr *rname, addr *rinit, addr *rtype, addr *rreadonly)
{
	addr gensym, list, name, init, type, readonly, key, value, key1, key2;

	/* name */
	GetConst(SYSTEM_STRUCTURE_GENSYM, &gensym);
	name = init = type = readonly = gensym;
	if (! consp_getcons(pos, &name, &list)) {
		name = pos;
		goto finish;
	}

	/* (name) */
	if (! symbolp(name))
		return fmte_("DEFSTRUCT slot-name ~S must be a symbol.", name, NULL);
	if (list == Nil)
		goto finish;

	/* (name init) */
	if (! consp_getcons(list, &init, &list))
		return fmte_("Invalid DEFSTRUCT slot-option ~S.", pos, NULL);
	if (list == Nil)
		goto finish;

	/* options */
	GetConst(KEYWORD_TYPE, &key1);
	GetConst(KEYWORD_READ_ONLY, &key2);
	while (list != Nil) {
		if (! consp_getcons(list, &key, &list))
			return fmte_("Invalid DEFSTRUCT slot-option key ~S.", list, NULL);
		if (! consp_getcons(list, &value, &list))
			return fmte_("Invalid DEFSTRUCT slot-option value ~S.", list, NULL);
		/* :type */
		if (key == key1) {
			if (type == gensym)
				type = value;
			continue;
		}
		/* :read-only */
		if (key == key2) {
			if (readonly == gensym) {
				readonly = (value == Nil)? Nil: T;
			}
			continue;
		}
		/* error */
		return fmte_("Invalid DEFSTRUCT slot-option ~S.", key, NULL);
	}

finish:
	if (! symbolp(name))
		return fmte_("DEFSTRUCT slot-name ~S must be a symbol.", name, NULL);
	*rname = name;
	*rinit = init;
	*rtype = type;
	*rreadonly = readonly;
	return 0;
}

static int defstruct_parse_slots_result_(struct defstruct *str, addr list, addr *ret)
{
	addr root, pos, name, init, type, readonly;
	LocalHold hold;

	name = init = type = readonly = NULL;
	hold = LocalHold_array(str->ptr, 1);
	for (root = Nil; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(defstruct_parse_slot_(str, pos, &name, &init, &type, &readonly));
		list_heap(&pos, name, init, type, readonly, NULL);
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int defstruct_parse_slots_(struct defstruct *str, addr list)
{
	Return(defstruct_parse_slots_result_(str, list, &list));
	str->slots = list;
	return 0;
}


/* defstruct-name */
static int defstruct_parse_name_option1_(constindex index, addr option, addr *ret)
{
	addr key, check, tail;

	GetConstant(index, &key);
	if (key == option) {
		/* :keyword */
		return Result(ret, Unbound);
	}
	if (! consp_getcons(option, &check, &tail)) {
		/* :others */
		return Result(ret, NULL);
	}
	if (key != check) {
		/* (:others ...) */
		return Result(ret, NULL);
	}
	if (tail == Nil) {
		/* (:keyword) */
		return Result(ret, Unbound);
	}
	if (! consp_getcons(tail, &check, &tail)) {
		/* (:keyword . xxx) */
		goto error;
	}
	if (tail != Nil) {
		/* (:keyword name . xxx) */
		goto error;
	}
	/* (:keyword value) */
	return Result(ret, check);

error:
	*ret = NULL;
	return fmte_("Invalid DEFSTRUCT option ~S.", option, NULL);
}
#define defstruct_option1_(x,y,z) \
	defstruct_parse_name_option1_(CONSTANT_KEYWORD_##x,(y),(z))

static int defstruct_parse_conc_name_(struct defstruct *str, addr pos, int *ret)
{
	int check;

	Return(defstruct_option1_(CONC_NAME, pos, &pos));
	if (pos == NULL)
		return Result(ret, 0);
	if (str->conc_name_p)
		return fmte_("DEFSTRUCT :CONC-NAME is already exist.", NULL);
	str->conc_name_p = 1;
	if (pos == Unbound || pos == Nil) {
		str->conc_name = Nil;
		return Result(ret, 1);
	}
	Return(string_designator_heap_(&pos, pos, &check));
	if (check) {
		str->conc_name = pos;
		return Result(ret, 1);
	}

	return fmte_("DEFSTRUCT :CONC-NAME ~S must be a string-designator.", pos, NULL);
}

static int defstruct_parse_copier_(struct defstruct *str, addr pos, int *ret)
{
	int check;

	Return(defstruct_option1_(COPIER, pos, &pos));
	if (pos == NULL)
		return Result(ret, 0);
	if (str->copier_p)
		return fmte_("DEFSTRUCT :COPIER is already exist.", NULL);
	if (pos == Unbound) {
		pos = T;
		goto store;
	}
	if (pos == Nil)
		goto store;
	Return(string_designator_heap_(&pos, pos, &check));
	if (check)
		goto store;
	return fmte_("DEFSTRUCT :COPIER ~S must be a symbol.", pos, NULL);

store:
	str->copier_p = 1;
	str->copier = pos;
	return Result(ret, 1);
}

static int defstruct_parse_predicate_(struct defstruct *str, addr pos, int *ret)
{
	int check;

	Return(defstruct_option1_(PREDICATE, pos, &pos));
	if (pos == NULL)
		return Result(ret, 0);
	if (str->predicate_p)
		return fmte_("DEFSTRUCT :PREDICATE is already exist.", NULL);
	if (pos == Unbound) {
		pos = T;
		goto store;
	}
	if (pos == Nil)
		goto store;
	Return(string_designator_heap_(&pos, pos, &check));
	if (check)
		goto store;
	return fmte_("DEFSTRUCT :PREDICATE ~S must be a symbol.", pos, NULL);

store:
	str->predicate_p = 1;
	str->predicate = pos;
	return Result(ret, 1);
}

static int defstruct_parse_constructor2_(addr option, addr *ret1, addr *ret2)
{
	addr key, pos1, pos2, tail;

	GetConstant(CONSTANT_KEYWORD_CONSTRUCTOR, &key);
	if (key == option) {
		/* :constructor */
		*ret1 = Unbound;
		*ret2 = Unbound;
		return 0;
	}
	if (! consp_getcons(option, &pos1, &tail)) {
		/* :others */
		return Result(ret1, NULL);
	}
	if (key != pos1) {
		/* (:others ...) */
		return Result(ret1, NULL);
	}
	if (tail == Nil) {
		/* (:constructor) */
		*ret1 = Unbound;
		*ret2 = Unbound;
		return 0;
	}
	if (! consp_getcons(tail, &pos1, &tail)) {
		/* (:constructor . xxx) */
		goto error;
	}
	if (tail == Nil) {
		/* (:constructor ret1) */
		*ret1 = pos1;
		*ret2 = Unbound;
		return 0;
	}
	if (! consp_getcons(tail, &pos2, &tail)) {
		/* (:constructor pos1 . xxx) */
		goto error;
	}
	if (tail != Nil) {
		/* (:constructor name . xxx) */
		goto error;
	}
	/* (:constructor pos1 pos2) */
	*ret1 = pos1;
	*ret2 = pos2;
	return 0;

error:
	*ret1 = NULL;
	return fmte_("Invalid DEFSTRUCT option ~S.", option, NULL);
}

static int defstruct_parse_constructor_(struct defstruct *str, addr pos, int *ret)
{
	addr args, g;

	Return(defstruct_parse_constructor2_(pos, &pos, &args));
	if (pos == NULL)
		return Result(ret, 0);
	if (pos == Unbound) {
		GetConst(SYSTEM_STRUCTURE_GENSYM, &pos);
	}
	if (! symbolp(pos))
		return fmte_(":CONSTRUCTOR name ~S must be a symbol.", pos, NULL);
	str->constructor_p = 1;
	if (pos == Nil) {
		str->constructor = Nil;
		return Result(ret, 1);
	}
	if (args != Unbound) {
		GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
		quotelist_heap(&g, g);
		Return(argument_boa_heap_(str->ptr->local, &args, args, g));
		cons_heap(&pos, pos, args);
	}
	cons_heap(&(str->constructor), pos, str->constructor);

	return Result(ret, 1);
}

static int defstruct_parse_include2_(addr option, addr *ret1, addr *ret2)
{
	addr key, check, tail;

	if (! consp_getcons(option, &check, &tail)) {
		/* :others */
		return Result(ret1, NULL);
	}
	GetConstant(CONSTANT_KEYWORD_INCLUDE, &key);
	if (key != check) {
		/* (:others ...) */
		return Result(ret1, NULL);
	}
	if (! consp_getcons(tail, ret1, ret2)) {
		/* (:include . xxx) */
		goto error;
	}
	return 0;

error:
	*ret1 = *ret2 = NULL;
	return fmte_("DEFSTRUCT :INCLUDE option ~S "
			"must be a (:include name . slots) form.", option, NULL);
}

static int defstruct_parse_include_(struct defstruct *str, addr pos, int *ret)
{
	addr args;

	Return(defstruct_parse_include2_(pos, &pos, &args));
	if (pos == NULL)
		return Result(ret, 0);
	if (! symbolp(pos))
		return fmte_(":INCLUDE name ~S must be a symbol.", pos, NULL);
	if (str->include_p)
		return fmte_("DEFSTRUCT :INCLUDE is already exist.", NULL);
	str->include_p = 1;
	str->iname = pos;
	str->iargs = args;

	return Result(ret, 1);
}

static int defstruct_parse_print_object1_(constindex index, addr option, addr *ret)
{
	addr key, check, tail;

	GetConstant(index, &key);
	if (key == option) {
		/* :option */
		return Result(ret, Unbound);
	}
	if (! consp_getcons(option, &check, &tail)) {
		/* :others */
		return Result(ret, NULL);
	}
	if (key != check) {
		/* (:others ...) */
		return Result(ret, NULL);
	}
	if (tail == Nil) {
		/* (:print-object) */
		return Result(ret, Unbound);
	}
	if (! consp_getcons(tail, &check, &tail)) {
		/* (:print-object . xxx) */
		goto error;
	}
	if (tail != Nil) {
		/* (:print-object name . xxx) */
		goto error;
	}
	/* (:print-object value) */
	return Result(ret, check);

error:
	return fmte_("Invalid DEFSTRUCT option ~S.", option, NULL);
}

static int defstruct_parse_print_object_(struct defstruct *str, addr pos, int *ret)
{
	Return(defstruct_parse_print_object1_(CONSTANT_KEYWORD_PRINT_OBJECT, pos, &pos));
	if (pos == NULL)
		return Result(ret, 0);
	if (pos == Unbound)
		GetConst(SYSTEM_STRUCTURE_GENSYM, &pos);
	if (str->print_object_p)
		return fmte_("DEFSTRUCT :PRINT-OBJECT is already exist.", NULL);
	if (str->print_function_p)
		return fmte_("DEFSTRUCT :PRINT-FUNCTION is already exist.", NULL);
	str->print_object_p = 1;
	str->print_object = pos;

	return Result(ret, 1);
}

static int defstruct_parse_print_function_(struct defstruct *str, addr pos, int *ret)
{
	Return(defstruct_parse_print_object1_(CONSTANT_KEYWORD_PRINT_FUNCTION, pos, &pos));
	if (pos == NULL)
		return Result(ret, 0);
	if (pos == Unbound)
		GetConst(SYSTEM_STRUCTURE_GENSYM, &pos);
	if (str->print_object_p)
		return fmte_("DEFSTRUCT :PRINT-OBJECT is already exist.", NULL);
	if (str->print_function_p)
		return fmte_("DEFSTRUCT :PRINT-FUNCTION is already exist.", NULL);
	str->print_function_p = 1;
	str->print_function = pos;

	return Result(ret, 1);
}

static int defstruct_parse_type_(struct defstruct *str, addr option, int *ret)
{
	addr key, check, pos, a, b;

	/* parse */
	if (! consp_getcons(option, &check, &pos))
		return Result(ret, 0);
	GetConst(KEYWORD_TYPE, &key);
	if (key != check)
		return Result(ret, 0);
	if (! consp_getcons(pos, &pos, &check))
		goto error;
	if (check != Nil)
		goto error;
	if (str->type_p)
		return fmte_("DEFSTRUCT :TYPE already exists.", NULL);
	/* list */
	GetConst(COMMON_LIST, &check);
	if (pos == check) {
		str->type_p = 1;
		str->type_list_p = 1;
		return Result(ret, 1);
	}
	/* vector */
	GetConst(COMMON_VECTOR, &check);
	if (pos == check) {
		str->type_p = 1;
		str->type_vector_p = 1;
		str->type_vector = T;
		return Result(ret, 1);
	}
	/* (vector type) */
	if (! consp_getcons(pos, &a, &b))
		goto type_error;
	GetConst(COMMON_VECTOR, &check);
	if (a != check)
		goto type_error;
	if (! consp_getcons(b, &a, &b))
		goto type_error;
	if (b != Nil)
		goto type_error;
	str->type_p = 1;
	str->type_vector_p = 1;
	str->type_vector = a;
	return Result(ret, 1);

error:
	return fmte_("DEFSTRUCT :TYPE must be a (:type type) form.", option, NULL);

type_error:
	return fmte_("Invalid :TYPE argument ~S.", pos, NULL);
}

static int defstruct_parse_named_(struct defstruct *str, addr option, int *ret)
{
	addr key;

	GetConst(KEYWORD_NAMED, &key);
	if (option != key)
		return Result(ret, 0);
	if (str->named_p)
		return fmte_("DEFSTRUCT :named already exists.", NULL);
	str->named_p = 1;

	return Result(ret, 1);
}

static int defstruct_parse_initial_offset_(struct defstruct *str, addr option, int *ret)
{
	addr key, check, pos;
	size_t size;

	/* parse */
	if (! consp_getcons(option, &check, &pos))
		return Result(ret, 0);
	GetConst(KEYWORD_INITIAL_OFFSET, &key);
	if (key != check)
		return Result(ret, 0);
	if (! consp_getcons(pos, &check, &pos))
		goto error;
	if (pos != Nil)
		goto error;
	if (str->initial_offset_p)
		return fmte_("DEFSTRUCT :INITIAL-OFFSET already exists.", NULL);
	Return(getindex_integer_(check, &size));
	str->initial_offset_p = 1;
	str->initial_offset = check;

	return Result(ret, 1);

error:
	return fmte_("DEFSTRUCT :INITIAL-OFFSET must be a "
			"(:initial-offset offset) form.", option, NULL);
}

static int defstruct_parse_name_(struct defstruct *str, addr name)
{
	int check;
	addr list, pos, error_check;
	LocalHold hold;

	if (symbolp(name)) {
		str->name = name;
		return 0;
	}
	if (! consp_getcons(name, &name, &list))
		return fmte_("DEFSTRUCT name ~S must be symbol or list.", name, NULL);
	if (! symbolp(name))
		return fmte_("DEFSTRUCT name ~S must be a symbol.", name, NULL);
	str->name = name;
	str->constructor = Nil;
	/* loop */
	while (list != Nil) {
		if (! consp_getcons(list, &pos, &list))
			return fmte_("DEFSTRUCT name option ~S must be a list.", list, NULL);
		Return(defstruct_parse_conc_name_(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_constructor_(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_copier_(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_predicate_(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_include_(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_print_object_(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_print_function_(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_type_(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_named_(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_initial_offset_(str, pos, &check));
		if (check)
			continue;
		return fmte_("Invalid DEFSTRUCT option ~S.", pos, NULL);
	}

	/* parse slots */
	hold = LocalHold_local(str->ptr);
	localhold_defstruct(str, hold);
	if (str->include_p) {
		Return(defstruct_parse_slots_result_(str, str->iargs, &(str->iargs)));
	}

	/* parse-type */
	if (str->type_vector_p) {
		Return(parse_type_(str->ptr, &error_check, str->type_vector, str->env));
	}
	localhold_end(hold);

	/* named check */
	if (str->named_p && (! str->type_p))
		return fmte_("There is :NAMED option but no :TYPE option.", NULL);

	/* initial-offset check */
	if (str->initial_offset_p && (! str->type_p))
		return fmte_("There is :INITIAL-OFFSET option but no :TYPE option.", NULL);

	return 0;
}

static void defstruct_parse_document(struct defstruct *str, addr pos, addr *ret)
{
	addr a, b;

	if (! consp_getcons(pos, &a, &b)) {
		str->doc = NULL;
		*ret = pos;
		return;
	}
	if (! stringp(a)) {
		str->doc = NULL;
		*ret = pos;
		return;
	}
	str->doc = a;
	*ret = b;
}

static int defstruct_parse_(struct defstruct *str, addr form)
{
	addr args, name;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &name, &args)) {
		return fmte_("DEFSTRUCT form ~S "
				"must be a (defstruct name [doc] {slot}*", form, NULL);
	}
	Return(defstruct_parse_name_(str, name));
	defstruct_parse_document(str, args, &args);
	return defstruct_parse_slots_(str, args);
}

static void defstruct_slots_list(addr *ret, addr slots, addr first)
{
	addr root, pos, name, init, type, readonly;
	addr list, lambda, gensym;

	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(SYSTEM_STRUCTURE_GENSYM, &gensym);
	root = Nil;
	if (first != Unbound)
		cons_heap(&root, first, root);
	while (slots != Nil) {
		GetCons(slots, &pos, &slots);
		List_bind(pos, &name, &init, &type, &readonly, NULL);
		quotelist_heap(&name, name);
		quotelist_heap(&type, type);
		quotelist_heap(&readonly, readonly);
		if (init == gensym)
			quotelist_heap(&init, init);
		else
			list_heap(&init, lambda, Nil, init, NULL);
		list_heap(&pos, list, name, init, type, readonly, NULL);
		cons_heap(&root, pos, root);
	}
	if (root == Nil) {
		*ret = Nil;
	}
	else {
		nreverse(&root, root);
		cons_heap(ret, list, root);
	}
}

static int defstruct_constructor_body_(addr *ret, addr name, addr cons)
{
	addr root, symbol, keyword, package, call;

	/* (:slot1 slot1 :slot2 slot2 ...) */
	GetConst(PACKAGE_KEYWORD, &package);
	Return(argument_boa_variables_heap_(&cons, cons));
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &symbol, &cons);
		GetNameSymbol(symbol, &keyword);
		Return(intern_package_(package, keyword, &keyword, NULL));
		cons_heap(&root, keyword, root);
		cons_heap(&root, symbol, root);
	}
	nreverse(&root, root);

	/* (lisp-system::structure-constructor 'name ...) */
	GetConst(SYSTEM_STRUCTURE_CONSTRUCTOR, &call);
	quotelist_heap(&name, name);
	lista_heap(ret, call, name, root, NULL);

	return 0;
}

static int defstruct_constructor_lambda_(addr *ret, addr cons, addr symbol)
{
	/* (lambda (...)
	 *   (lisp-system::structure-constructor 'name
	 *     :slot1 slot1 :slot2 slot2 ...))
	 */
	addr name, lambda, args, body, list;

	GetCons(cons, &name, &cons);
	Return(argument_boa_lambda_heap_(&args, cons));
	Return(defstruct_constructor_body_(&body, symbol, cons));
	GetConst(COMMON_LAMBDA, &lambda);
	list_heap(&args, lambda, args, body, NULL);
	/* (list 'name (lambda...)) */
	GetConst(COMMON_LIST, &list);
	quotelist_heap(&name, name);
	list_heap(ret, list, name, args, NULL);

	return 0;
}

static int defstruct_make_constructor_(struct defstruct *str, addr *ret, addr root)
{
	int check;
	addr list, symbol, keyword, pos;

	check = str->constructor_p;
	list = str->constructor;
	symbol = str->name;
	GetConst(KEYWORD_CONSTRUCTOR, &keyword);
	if (! check) {
		cons_heap(&root, keyword, root);
		GetConst(SYSTEM_STRUCTURE_GENSYM, &pos);
		quotelist_heap(&pos, pos);
		cons_heap(&root, pos, root);
		return Result(ret, root);
	}
	while (list != Nil) {
		GetCons(list, &pos, &list);
		cons_heap(&root, keyword, root);
		if (consp(pos)) {
			Return(defstruct_constructor_lambda_(&pos, pos, symbol));
		}
		else {
			quotelist_heap(&pos, pos);
		}
		cons_heap(&root, pos, root);
	}

	return Result(ret, root);
}

static void defstruct_make_print_object(addr *ret, addr pos)
{
	addr symbol, g;

	/* gensym */
	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	if (pos == g) {
		quotelist_heap(ret, g);
		return;
	}

	/* symbol */
	if (symbolp(pos)) {
		GetConst(COMMON_FUNCTION, &symbol);
		list_heap(&pos, symbol, pos, NULL);
	}

	/* lambda */
	GetConst(COMMON_LAMBDA, &symbol);
	list_heap(ret, symbol, Nil, pos, NULL);
}

static int defstruct_make_(struct defstruct *str, addr *ret)
{
	/* `(ensure-structure
	 *    ',name
	 *    (list (list ',name (lambda () ,init) ',type ',readonly)
	 *          (list ...))
	 *    :documentation ,doc
	 *    :conc-name ',conc-name
	 */
	addr root, pos;

	/* ensure-structure */
	root = Nil;
	GetConst(SYSTEM_ENSURE_STRUCTURE, &pos);
	cons_heap(&root, pos, root);
	/* name, slots */
	quotelist_heap(&pos, str->name);
	cons_heap(&root, pos, root);
	defstruct_slots_list(&pos, str->slots, Unbound);
	cons_heap(&root, pos, root);
	/* :documentation */
	if (str->doc) {
		GetConst(KEYWORD_DOCUMENTATION, &pos);
		cons_heap(&root, pos, root);
		cons_heap(&root, str->doc, root);
	}
	/* :conc-name */
	if (str->conc_name_p) {
		GetConst(KEYWORD_CONC_NAME, &pos);
		cons_heap(&root, pos, root);
		quotelist_heap(&pos, str->conc_name);
		cons_heap(&root, pos, root);
	}
	/* :type */
	if (str->type_list_p) {
		GetConst(KEYWORD_TYPE, &pos);
		cons_heap(&root, pos, root);
		GetConst(COMMON_LIST, &pos);
		quotelist_heap(&pos, pos);
		cons_heap(&root, pos, root);
	}
	if (str->type_vector_p) {
		GetConst(KEYWORD_TYPE, &pos);
		cons_heap(&root, pos, root);
		GetConst(COMMON_VECTOR, &pos);
		list_heap(&pos, pos, str->type_vector, NULL);
		quotelist_heap(&pos, pos);
		cons_heap(&root, pos, root);
	}
	/* :named */
	if (str->named_p) {
		GetConst(KEYWORD_NAMED, &pos);
		cons_heap(&root, pos, root);
		cons_heap(&root, T, root);
	}
	/* :initial-offset */
	if (str->initial_offset_p) {
		GetConst(KEYWORD_INITIAL_OFFSET, &pos);
		cons_heap(&root, pos, root);
		quotelist_heap(&pos, str->initial_offset);
		cons_heap(&root, pos, root);
	}
	/* :copier */
	if (str->copier_p && str->copier != T) {
		GetConst(KEYWORD_COPIER, &pos);
		cons_heap(&root, pos, root);
		quotelist_heap(&pos, str->copier);
		cons_heap(&root, pos, root);
	}
	/* :predicate */
	if (str->predicate_p) {
		GetConst(KEYWORD_PREDICATE, &pos);
		cons_heap(&root, pos, root);
		quotelist_heap(&pos, str->predicate);
		cons_heap(&root, pos, root);
	}
	/* :include */
	if (str->include_p) {
		/* :include (list 'iname ...) */
		GetConst(KEYWORD_INCLUDE, &pos);
		cons_heap(&root, pos, root);
		quotelist_heap(&pos, str->iname);
		defstruct_slots_list(&pos, str->iargs, pos);
		cons_heap(&root, pos, root);
	}
	if (str->print_object_p) {
		/* :print-object ... */
		GetConst(KEYWORD_PRINT_OBJECT, &pos);
		cons_heap(&root, pos, root);
		defstruct_make_print_object(&pos, str->print_object);
		cons_heap(&root, pos, root);
	}
	if (str->print_function_p) {
		/* :print-function ... */
		GetConst(KEYWORD_PRINT_FUNCTION, &pos);
		cons_heap(&root, pos, root);
		defstruct_make_print_object(&pos, str->print_function);
		cons_heap(&root, pos, root);
	}
	/* :constructor */
	Return(defstruct_make_constructor_(str, &root, root));
	/* result */
	nreverse(ret, root);

	return 0;
}

int defstruct_common_(Execute ptr, addr form, addr env, addr *ret)
{
	struct defstruct str;
	LocalHold hold;

	defstruct_clean(&str);
	str.ptr = ptr;
	str.env = env;
	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, form, env, NULL);
	Return(defstruct_parse_(&str, form));
	Return(defstruct_make_(&str, &form));
	localhold_end(hold);

	return Result(ret, form);
}


/************************************************************
 *  call_symbols.c
 ************************************************************/

/*
 *  make-symbol
 */
void make_symbol_common(addr var, addr *ret)
{
	addr pos;

	symbol_heap(&pos);
	SetNameSymbol(pos, var);
	*ret = pos;
}


/*
 *  copy-symbol
 */
static void copy_symbol_type_common(addr var, addr symbol)
{
	addr type;

	/* value */
	gettype_value_symbol(var, &type);
	if (type != Nil)
		settype_value_symbol(symbol, type);

	/* function */
	gettype_function_symbol(var, &type);
	if (type != Nil)
		settype_function_symbol(symbol, type);

	/* setf */
	gettype_setf_symbol(var, &type);
	if (type != Nil)
		settype_setf_symbol(symbol, type);
}

int copy_symbol_common_(addr var, addr opt, addr *ret)
{
	addr symbol, pos;

	if (opt == Unbound)
		opt = Nil;
	GetNameSymbol(var, &pos);
	Return(string_heap_(&pos, pos));
	symbol_heap(&symbol);
	SetNameSymbol(symbol, pos);

	if (opt != Nil) {
		/* symbol-value */
		GetValueSymbol(var, &pos);
		SetValueSymbol(symbol, pos);
		/* symbol-function */
		GetFunctionSymbol(var, &pos);
		SetFunctionSymbol(symbol, pos);
		/* symbol-setf */
		getsetf_symbol(var, &pos);
		if (pos != Unbound)
			setsetf_symbol(symbol, pos);
		/* property-list */
		GetPlistSymbol(var, &pos);
		copy_list_heap_unsafe(&pos, pos);
		SetPlistSymbol(symbol, pos);
		/* copy-type */
		copy_symbol_type_common(var, symbol);
	}

	return Result(ret, symbol);
}


/*
 *  gensym
 */
int gensym_common_(Execute ptr, addr opt, addr *ret)
{
	if (opt == Unbound)
		return make_gensym_(ptr, ret);
	else if (stringp(opt))
		return make_gensym_prefix_(ptr, opt, ret);
	else if (integerp(opt))
		return make_gensym_integer_(ptr, opt, ret);
	else
		return fmte_("type-error.", NULL);
}


/*
 *  gentemp
 */
int gentemp_common_(Execute ptr, addr opt1, addr opt2, addr *ret)
{
	if (opt1 == Unbound)
		opt1 = NULL;
	if (opt2 == Unbound)
		opt2 = NULL;
	return make_gentemp_(ptr, opt1, opt2, ret);
}


/*
 *  (setf symbol-function)
 */
int setf_symbol_function_common_(addr value, addr symbol)
{
	if (GetStatusReadOnly(symbol))
		return fmte_("The symbol ~S is readonly.", symbol, NULL);
	Return(remtype_function_symbol_(symbol));
	SetFunctionSymbol(symbol, value);

	return 0;
}


/*
 *  (setf symbol-value)
 */
int setf_symbol_value_common_(Execute ptr, addr value, addr symbol)
{
	if (GetStatusReadOnly(symbol))
		return fmte_("The symbol ~S is readonly.", symbol, NULL);
	setspecial_local(ptr, symbol, value);

	return 0;
}


/*
 *  (setf symbol-plist)
 */
int setf_symbol_plist_common_(addr value, addr symbol)
{
	SetPlistSymbol(symbol, value);
	return 0;
}


/*
 *  get
 */
int get_common_(addr var1, addr var2, addr opt, addr *ret)
{
	int check;

	if (opt == Unbound)
		opt = Nil;
	GetPlistSymbol(var1, &var1);
	check = getplist_safe(var1, var2, &var1);

	return Result(ret, check == 0? var1: opt);
}


/*
 *  (setf get)
 */
int setf_get_common_(addr value, addr symbol, addr key)
{
	addr list;

	GetPlistSymbol(symbol, &list);
	if (setplist_heap_safe(list, key, value, &list)) {
		SetPlistSymbol(symbol, list);
	}

	return 0;
}


/*
 *  remprop
 */
int remprop_common_(addr symbol, addr key, addr *ret)
{
	enum RemPlist value;
	addr list;

	GetPlistSymbol(symbol, &list);
	Return(remplist_safe_(list, key, &list, &value));
	switch (value) {
		case RemPlist_Delete:
			return Result(ret, T);

		case RemPlist_Update:
			SetPlistSymbol(symbol, list);
			return Result(ret, T);

		case RemPlist_NotFound:
		default:
			return Result(ret, Nil);

	}
}


/*
 *  makunbound
 */
int makunbound_common_(Execute ptr, addr symbol)
{
	if (GetStatusReadOnly(symbol))
		return fmte_("The symbol ~S is readonly.", symbol, NULL);
	setspecial_local(ptr, symbol, Unbound);
	return 0;
}


/*
 *  set
 */
int set_common_(Execute ptr, addr symbol, addr value)
{
	if (GetStatusReadOnly(symbol))
		return fmte_("The symbol ~S is readonly.", symbol, NULL);
	setspecial_local(ptr, symbol, value);
	return 0;
}


/************************************************************
 *  call_system.c
 ************************************************************/

/*
 *  load
 */
enum LoadType {
	LoadType_unbound,
	LoadType_lisp,
	LoadType_fasl
};

static int load_common_type_(Execute ptr, addr rest, enum LoadType *ret)
{
	int check;
	addr pos;

	if (GetKeyArgs(rest, KEYWORD_TYPE, &pos))
		goto unbound;
	if (! symbolp(pos))
		goto unbound;
	GetNameSymbol(pos, &pos);

	/* lisp */
	Return(string_equalp_char_(pos, "LISP", &check));
	if (check)
		return Result(ret, LoadType_lisp);

	/* fasl */
	Return(string_equalp_char_(pos, "FASL", &check));
	if (check)
		return Result(ret, LoadType_fasl);

	/* unbound */
unbound:
	return Result(ret, LoadType_unbound);
}

static int load_common_switch_(Execute ptr, addr file,
		enum LoadType type, addr x, addr y, int e, addr z, int *ret)
{
	switch (type) {
		case LoadType_lisp:
			return eval_load_force_lisp_(ptr, ret, file, x, y, e, z);

		case LoadType_fasl:
			return eval_load_force_fasl_(ptr, ret, file, x, y, e, z);

		case LoadType_unbound:
		default:
			return eval_load_(ptr, ret, file, x, y, e, z);
	}
}

int load_common_(Execute ptr, addr file, addr rest, int *ret)
{
	enum LoadType type;
	int e;
	addr x, y, z, pos;

	/* :verbose */
	if (GetKeyArgs(rest, KEYWORD_VERBOSE, &x))
		x = Unbound;

	/* :print */
	if (GetKeyArgs(rest, KEYWORD_PRINT, &y))
		y = Unbound;

	/* :if-does-not-exists */
	if (GetKeyArgs(rest, KEYWORD_IF_DOES_NOT_EXIST, &pos))
		pos = T;
	e = (pos != Nil);

	/* :external-format */
	if (GetKeyArgs(rest, KEYWORD_EXTERNAL_FORMAT, &z))
		z = Unbound;

	/* :type */
	Return(load_common_type_(ptr, rest, &type));

	*ret = 0;
	return load_common_switch_(ptr, file, type, x, y, e, z, ret);
}


/************************************************************
 *  call_types.c
 ************************************************************/

/* type-of */
int type_of_common_(addr pos, addr *ret)
{
	Return(type_value_(&pos, pos));
	Return(type_object_(ret, pos));
	return 0;
}


/* typep */
int typep_common_(Execute ptr, addr x, addr y, addr env, addr *ret)
{
	int check;

	if (env == Unbound)
		env = Nil;
	Return(parse_type_(ptr, &y, y, env));
	Return(typep_clang_(ptr, x, y, &check));
	*ret = check? T: Nil;

	return 0;
}


/* subtypep */
int subtypep_common_(Execute ptr, addr x, addr y, addr env, addr *v1, addr *v2)
{
	int check, validp;

	if (env == Unbound)
		env = Nil;
	Return(subtypep_check_(ptr, x, y, env, &check, &validp));
	*v1 = check? T: Nil;
	*v2 = validp? T: Nil;

	return 0;
}


/************************************************************
 *  callname.c
 ************************************************************/

/*
 *  access
 */
addr refcallname(addr pos)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	return RefCallName_Low(pos);
}
void getcallname(addr pos, addr *value)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	GetCallName_Low(pos, value);
}
void setcallname(addr pos, addr value)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetCallName_Low(pos, value);
}

CallNameType refcallnametype(addr pos)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	return RefCallNameType_Low(pos);
}
void getcallnametype(addr pos, CallNameType *value)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	GetCallNameType_Low(pos, value);
}
void setcallnametype(addr pos, CallNameType value)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetCallNameType_Low(pos, value);
}


/*
 *  allocate
 */
void make_callname_alloc(LocalRoot local, addr *ret)
{
	alloc_array2(local, ret, LISPTYPE_CALLNAME, 1);
}

void callname_alloc(LocalRoot local, addr *ret, addr name, CallNameType type)
{
	addr pos;

	Check(! symbolp(name), "name error.");
	make_callname_alloc(local, &pos);
	SetCallName_Low(pos, name);
	SetCallNameType_Low(pos, type);
	*ret = pos;
}
void callname_local(LocalRoot local, addr *ret, addr name, CallNameType type)
{
	Check(local == NULL, "local error");
	callname_alloc(local, ret, name, type);
}
void callname_heap(addr *ret, addr name, CallNameType type)
{
	callname_alloc(NULL, ret, name, type);
}

void setf_callname_alloc(LocalRoot local, addr *ret, addr symbol)
{
	Check(! symbolp(symbol), "type error");
	callname_alloc(local, ret, symbol, CALLNAME_SETF);
}
void setf_callname_local(LocalRoot local, addr *ret, addr symbol)
{
	Check(! symbolp(symbol), "type error");
	callname_local(local, ret, symbol, CALLNAME_SETF);
}
void setf_callname_heap(addr *ret, addr symbol)
{
	Check(! symbolp(symbol), "type error");
	callname_heap(ret, symbol, CALLNAME_SETF);
}


/*
 *  copy
 */
void copy_callname_alloc(LocalRoot local, addr *ret, addr pos)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	callname_alloc(local, ret, RefCallName_Low(pos), RefCallNameType_Low(pos));
}
void copy_callname_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	copy_callname_alloc(local, ret, pos);
}
void copy_callname_heap(addr *ret, addr pos)
{
	copy_callname_alloc(NULL, ret, pos);
}


/*
 *  parse
 */
CallNameType parse_callname(addr name, addr *ret)
{
	CallNameType type;
	addr setf, cons;

	Check(name == Unbound, "unbound error.");
	switch (GetType(name)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			type = CALLNAME_SYMBOL;
			break;

		case LISPTYPE_CONS: /* (setf name) */
			GetConst(COMMON_SETF, &setf);
			GetCons(name, &name, &cons);
			if (name != setf)
				goto error;
			Check(cons == Unbound, "unbound cons error.");
			if (GetType(cons) != LISPTYPE_CONS)
				goto error;
			GetCons(cons, &name, &cons);
			if (! symbolp(name))
				goto error;
			if (cons != Nil)
				goto error;
			type = CALLNAME_SETF;
			break;

		default:
			goto error;
	}
	*ret = name;
	return type;

error:
	return CALLNAME_ERROR;
}

int parse_callname_alloc(LocalRoot local, addr *ret, addr name)
{
	CallNameType type;

	type = parse_callname(name, &name);
	if (type == CALLNAME_ERROR)
		return 1;
	callname_alloc(local, ret, name, type);

	return 0;
}
int parse_callname_local(LocalRoot local, addr *ret, addr name)
{
	Check(local == NULL, "local error");
	return parse_callname_alloc(local, ret, name);
}
int parse_callname_heap(addr *ret, addr name)
{
	return parse_callname_alloc(NULL, ret, name);
}
void parse_callname_abort(LocalRoot local, addr *ret, addr name)
{
	if (parse_callname_alloc(local, ret, name))
		Abort("Invalid function name.");
}
int parse_callname_error_(addr *ret, addr name)
{
	if (parse_callname_heap(ret, name))
		return fmte_("Invalid function name ~S.", name, NULL);
	return 0;
}


/*
 *  boolean
 */
int callnamep(addr pos)
{
	return GetType(pos) == LISPTYPE_CALLNAME;
}

int symbolp_callname(addr call)
{
	CheckType(call, LISPTYPE_CALLNAME);
	return RefCallNameType(call) == CALLNAME_SYMBOL;
}

int setfp_callname(addr call)
{
	CheckType(call, LISPTYPE_CALLNAME);
	return RefCallNameType(call) == CALLNAME_SETF;
}

int constantp_callname(addr pos)
{
	CheckType(pos, LISPTYPE_CALLNAME);
	GetCallName(pos, &pos);
	return GetStatusReadOnly(pos);
}

int function_name_p(addr name)
{
	if (GetType(name) == LISPTYPE_CALLNAME)
		return 1;
	return parse_callname(name, &name) != CALLNAME_ERROR;
}

int equal_callname(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_CALLNAME, "type left error");
	Check(GetType(right) != LISPTYPE_CALLNAME, "type right error");
	return (RefCallNameType_Low(left) == RefCallNameType_Low(right))
		&& (RefCallName_Low(left) == RefCallName_Low(right));
}


/*
 *  function
 */
void getglobal_callname(addr pos, addr *ret)
{
	CallNameType type;

	CheckType(pos, LISPTYPE_CALLNAME);
	GetCallNameType(pos, &type);
	GetCallName(pos, &pos);
	switch (type) {
		case CALLNAME_SYMBOL:
			GetFunctionSymbol(pos, ret);
			break;

		case CALLNAME_SETF:
			getsetf_symbol(pos, ret);
			break;

		case CALLNAME_ERROR:
		default:
			Abort("Invalid function name.");
			break;
	}
}

int getglobalcheck_callname_(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CALLNAME);
	getglobal_callname(pos, ret);
	if (*ret == Unbound) {
		name_callname_heap(pos, &pos);
		return call_undefined_function_(NULL, pos);
	}

	return 0;
}

int setglobal_callname_(addr pos, addr value)
{
	CallNameType type;

	CheckType(pos, LISPTYPE_CALLNAME);
	GetCallNameType(pos, &type);
	GetCallName(pos, &pos);
	Return(alldelete_function_(pos));
	switch (type) {
		case CALLNAME_SYMBOL:
			return setfunction_symbol_(pos, value);

		case CALLNAME_SETF:
			return setsetf_symbol_(pos, value);

		case CALLNAME_ERROR:
		default:
			Abort("Invalid function name.");
			return 0;
	}
}

int remtype_global_callname_(addr pos)
{
	CallNameType type;

	CheckType(pos, LISPTYPE_CALLNAME);
	GetCallNameType(pos, &type);
	GetCallName(pos, &pos);
	switch (type) {
		case CALLNAME_SYMBOL:
			return remtype_function_symbol_(pos);

		case CALLNAME_SETF:
			return remtype_setf_symbol_(pos);

		case CALLNAME_ERROR:
		default:
			Abort("Invalid function name.");
			return 0;
	}
}

static CallNameType callnametype(addr pos, addr *value)
{
	CallNameType type;

	if (GetType(pos) == LISPTYPE_CALLNAME) {
		GetCallName_Low(pos, value);
		GetCallNameType_Low(pos, &type);
		return type;
	}
	else {
		return parse_callname(pos, value);
	}
}

void getglobal_parse_callname(addr pos, addr *value)
{
	Check(pos == Unbound, "unbound error");
	switch (callnametype(pos, &pos)) {
		case CALLNAME_SYMBOL:
			GetFunctionSymbol(pos, value);
			break;

		case CALLNAME_SETF:
			getsetf_symbol(pos, value);
			break;

		case CALLNAME_ERROR:
		default:
			*value = NULL;
			Abort("Invalid function name.");
			break;
	}
}

int getglobalcheck_parse_callname_(addr pos, addr *ret)
{
	getglobal_parse_callname(pos, ret);
	if (*ret == Unbound) {
		name_callname_heap(pos, &pos);
		return call_undefined_function_(NULL, pos);
	}

	return 0;
}

int setglobal_parse_callname_(addr pos, addr value)
{
	Check(pos == Unbound, "unbound error");
	switch (callnametype(pos, &pos)) {
		case CALLNAME_SYMBOL:
			return setfunction_symbol_(pos, value);

		case CALLNAME_SETF:
			return setsetf_symbol_(pos, value);

		case CALLNAME_ERROR:
		default:
			Abort("Invalid function name.");
			return 0;
	}
}


/*
 *  name
 */
void name_callname_alloc(LocalRoot local, addr pos, addr *ret)
{
	CallNameType type;
	addr setf;

	CheckType(pos, LISPTYPE_CALLNAME);
	GetCallNameType_Low(pos, &type);
	switch (type) {
		case CALLNAME_SYMBOL:
			GetCallName_Low(pos, ret);
			break;

		case CALLNAME_SETF:
			GetConst(COMMON_SETF, &setf);
			GetCallName_Low(pos, &pos);
			list_alloc(local, ret, setf, pos, NULL);
			break;

		case CALLNAME_ERROR:
		default:
			Abort("Invalid function name.");
			break;
	}
}

void name_callname_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	name_callname_alloc(local, pos, ret);
}

void name_callname_heap(addr pos, addr *ret)
{
	name_callname_alloc(NULL, pos, ret);
}


/************************************************************
 *  character.c
 ************************************************************/

/*
 *  character
 */
void make_character_heap(addr *ret, unicode value)
{
	addr pos;
	heap_body2(&pos, LISPTYPE_CHARACTER, sizeoft(unicode));
	setcharacter_unsafe(pos, value);
	*ret = pos;
}

void character_alloc(LocalRoot local, addr *ret, unicode value)
{
	if (local)
		character_local(local, ret, value);
	else
		character_heap(ret, value);
}

void character_local(LocalRoot local, addr *ret, unicode value)
{
	addr pos;

	Check(local == NULL, "local error");
	local_body2(local, &pos, LISPTYPE_CHARACTER, sizeoft(unicode));
	setcharacter_unsafe(pos, value);
	*ret = pos;
}

#define character_cache_p(v) ((v) < LISP_CHARACTER_CACHE)
void character_heap(addr *ret, unicode value)
{
	addr cache, pos;

	/* make object */
	if (! character_cache_p(value)) {
		make_character_heap(ret, value);
		return;
	}

	/* cache */
	GetConst(CHARACTER_CACHE, &cache);
	Check(cache == Unbound, "Unbound error, (build_character).");
	GetArrayA4(cache, (size_t)value, &pos);

	/* cache hit */
	if (pos != Nil) {
		*ret = pos;
		return;
	}

	/* add cache */
	make_character_heap(&pos, value);
	SetArrayA4(cache, (size_t)value, pos);
	*ret = pos;
}

addr characterh(unicode value) /* for debug */
{
	addr pos;
	character_heap(&pos, value);
	return pos;
}

int character_unicode_heap(addr *ret, unicode c)
{
	addr pos;

	if (UnicodeCount <= c || isSurrogatePair(c)) {
		*ret = Nil;
		make_index_integer_heap(&pos, (size_t)c);
		return fmte_("Invalid unicode ~X.", pos, NULL);
	}
	character_heap(ret, c);
	return 0;
}

int make_extended_char_heap_(addr *ret, unicode c)
{
	addr pos;

	if (! isExtendedType(c)) {
		*ret = Nil;
		make_index_integer_heap(&pos, (size_t)c);
		return fmte_("Invalid extended-char code ~X.", pos, NULL);
	}

	/* heap */
	heap_body2(&pos, LISPTYPE_CHARACTER, sizeoft(unicode));
	SetCharacter_Low(pos, c);
	return Result(ret, pos);
}

const unicode *ptrcharacter(addr pos)
{
	Check(GetType(pos) != LISPTYPE_CHARACTER, "type error");
	return PtrCharacter_Low(pos);
}

unicode refcharacter(addr pos)
{
	Check(GetType(pos) != LISPTYPE_CHARACTER, "type error");
	return RefCharacter_Low(pos);
}

void getcharacter(addr pos, unicode *value)
{
	Check(GetType(pos) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(pos, value);
}

enum CHARACTER_TYPE character_type(unicode c)
{
	if (isStandardType(c))
		return CHARACTER_TYPE_STANDARD;
	if (isBaseType(c))
		return CHARACTER_TYPE_BASE;
	if (isExtendedType(c))
		return CHARACTER_TYPE_EXTENDED;

	return CHARACTER_TYPE_INVALID;
}

enum CHARACTER_TYPE ref_character_type(addr pos)
{
	unicode c;

	Check(GetType(pos) != LISPTYPE_CHARACTER, "type error");
	GetCharacter(pos, &c);
	return character_type(c);
}

void get_character_type(addr pos, enum CHARACTER_TYPE *ret)
{
	unicode c;

	Check(GetType(pos) != LISPTYPE_CHARACTER, "type error");
	GetCharacter(pos, &c);
	*ret = character_type(c);
}

int isvalidunicode(unicode c)
{
	return character_type(c) != CHARACTER_TYPE_INVALID;
}

void setcharacter_unsafe(addr pos, unicode value)
{
	Check(GetType(pos) != LISPTYPE_CHARACTER, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	Check(character_type(value) == CHARACTER_TYPE_INVALID, "Invaild character code.");
	SetCharacter_Low(pos, value);
}

int standard_char_p(addr pos)
{
	return GetType(pos) == LISPTYPE_CHARACTER && isstandardtype(RefCharacter(pos));
}

int base_char_p(addr pos)
{
	return GetType(pos) == LISPTYPE_CHARACTER && isbasetype(RefCharacter(pos));
}

int extended_char_p(addr pos)
{
	return GetType(pos) == LISPTYPE_CHARACTER && isextendedtype(RefCharacter(pos));
}

int characterp(addr pos)
{
	return GetType(pos) == LISPTYPE_CHARACTER;
}

int unicode_equalp(unicode left, unicode right)
{
	return toUpperUnicode(left) == toUpperUnicode(right);
}

#define ReturnCompare(a, b) { \
	if ((a) < (b)) return -1; \
	if ((a) > (b)) return 1; \
	return 0; \
}

int unicode_comparep(unicode left, unicode right)
{
	left = toUpperUnicode(left);
	right = toUpperUnicode(right);
	ReturnCompare(left, right);
}

int character_equal(addr left, addr right)
{
	unicode a, b;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &a);
	GetCharacter_Low(right, &b);

	return a == b;
}

int character_equalp(addr left, addr right)
{
	unicode a, b;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &a);
	GetCharacter_Low(right, &b);

	return toUpperUnicode(a) == toUpperUnicode(b);
}

int character_equal_char(addr left, const char *right)
{
	unicode a, b;

	CheckType(left, LISPTYPE_CHARACTER);
	b = right[0];
	if (b == 0 || right[1] != 0)
		return 0;
	GetCharacter(left, &a);
	return a == b;
}

int character_equalp_char(addr left, const char *right)
{
	unicode a, b;

	CheckType(left, LISPTYPE_CHARACTER);
	b = right[0];
	if (b == 0 || right[1] != 0)
		return 0;
	GetCharacter(left, &a);
	a = toUpperUnicode(a);
	b = toUpperUnicode(b);
	return a == b;
}

int character_compare(addr left, addr right)
{
	unicode a, b;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &a);
	GetCharacter_Low(right, &b);
	ReturnCompare(a, b);
}

int character_comparep(addr left, addr right)
{
	unicode a, b;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &a);
	GetCharacter_Low(right, &b);
	a = toUpperUnicode(a);
	b = toUpperUnicode(b);
	ReturnCompare(a, b);
}

int character_unicode_equal(addr left, unicode right)
{
	unicode c;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &c);

	return c == right;
}

int character_unicode_equalp(addr left, unicode right)
{
	unicode c;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &c);

	return toUpperUnicode(c) == toUpperUnicode(right);
}

int character_unicode_compare(addr left, unicode right)
{
	unicode c;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &c);
	ReturnCompare(c, right);
}

int character_unicode_comparep(addr left, unicode right)
{
	unicode c;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &c);
	c = toUpperUnicode(c);
	right = toUpperUnicode(right);
	ReturnCompare(c, right);
}


/* equal */
int character_equal_unicode(addr left, unicode right)
{
	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	return RefCharacter(left) == right;
}

int character_equalp_unicode(addr left, unicode right)
{
	unicode c;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	GetCharacter(left, &c);
	return toUpperUnicode(c) == toUpperUnicode(right);
}


/* character2 */
#define PtrCharacter2(x) ((unicode *)PtrBodyB2(x))
void character2_heap(addr *ret, unicode a, unicode b)
{
	addr pos;
	unicode *ptr;

	heap_body2(&pos, LISPSYSTEM_CHARACTER2, sizeoft(unicode) * 2);
	ptr = PtrCharacter2(pos);
	ptr[0] = a;
	ptr[1] = b;
	*ret = pos;
}

unicode refcharacter2a(addr pos)
{
	return PtrCharacter2(pos)[0];
}

unicode refcharacter2b(addr pos)
{
	return PtrCharacter2(pos)[1];
}

void getcharacter2a(addr pos, unicode *ret)
{
	*ret = PtrCharacter2(pos)[0];
}

void getcharacter2b(addr pos, unicode *ret)
{
	*ret = PtrCharacter2(pos)[1];
}

void setcharacter2a(addr pos, unicode value)
{
	PtrCharacter2(pos)[0] = value;
}

void setcharacter2b(addr pos, unicode value)
{
	PtrCharacter2(pos)[1] = value;
}


int character2_equal_unicode(addr left, unicode a, unicode b)
{
	CheckType(left, LISPSYSTEM_CHARACTER2);
	return refcharacter2a(left) == a && refcharacter2b(left) == b;
}

int character2_equalp_unicode(addr left, unicode a, unicode b)
{
	unicode c, d;

	CheckType(left, LISPSYSTEM_CHARACTER2);
	getcharacter2a(left, &c);
	getcharacter2b(left, &d);
	a = toUpperUnicode(a);
	b = toUpperUnicode(b);
	c = toUpperUnicode(c);
	d = toUpperUnicode(d);
	return a == c && b == d;
}


/*
 *  character table
 */
static void build_character_cache(void)
{
	addr pos;

	/* character cache */
	heap_array4(&pos, LISPSYSTEM_CHARACTER_CACHE, LISP_CHARACTER_CACHE);
	SetConst(CHARACTER_CACHE, pos);
}

void build_character(void)
{
	build_character_cache();
	build_character_name();
}


/************************************************************
 *  character_check.c
 ************************************************************/

/*
 *  character check
 */
int isbasechar(unicode x)
{
	return isBaseChar(x);
}

int isuppercase(unicode x)
{
	return isUpperCase(x);
}

int islowercase(unicode x)
{
	return isLowerCase(x);
}

int isdigitcase(unicode x)
{
	return isDigitCase(x);
}

int isalphabetic(unicode x)
{
	return isAlphabetic(x);
}

int isalphanumeric(unicode x)
{
	return isAlphanumeric(x);
}

int isgraphunicode(unicode x)
{
	if (x < 0x80)
		return _isGraphUnicode(x);
	else
		return isBaseType(x);
}

int isspaceunicode(unicode x)
{
	return isSpaceUnicode(x);
}

unicode toupperunicode(unicode x)
{
	return toUpperUnicode(x);
}

unicode tolowerunicode(unicode x)
{
	return toLowerUnicode(x);
}


/*
 *  character type
 */
int issurrogatepair(unicode x)
{
	return isSurrogatePair(x);
}

int isbaserange(unicode x)
{
	return isBaseRange(x);
}

int isstandardtype(unicode x)
{
	return isStandardType(x);
}

int isbasetype(unicode x)
{
	return isBaseType(x);
}

int isextendedtype(unicode x)
{
	return isExtendedType(x);
}


/************************************************************
 *  character_name.c
 ************************************************************/

/*
 *  character table
 */
int findtable_unicode_name_(addr *ret, unicode c)
{
	addr table;
	table = LispRoot(CHAR_NAME);
	return findnil_unicode_hashtable_(table, c, ret);
}

int findtable_char_name_(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_CHARACTER);
	return findtable_unicode_name_(ret, RefCharacter(pos));
}

int findtable_name_char_(addr *ret, addr name)
{
	addr table;

	Check(! stringp(name), "type error");
	table = LispRoot(NAME_CHAR);
	return findnil_hashtable_(table, name, ret);
}

static int unicode_code_p(unicode c)
{
	return ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F');
}

static int unicode_code_(addr name, size_t size, unicode *value, int *ret)
{
	char buffer[16];
	unsigned long ul;
	unicode c;
	size_t i, k;

	if (size < 2)
		return Result(ret, 1);
	Return(string_getc_(name, 0, &c));
	if (toUpperUnicode(c) != 'U')
		return Result(ret, 1);
	if (12 < size)
		return Result(ret, 1);
	for (k = 0, i = 1; i < size; i++) {
		Return(string_getc_(name, i, &c));
		if (! unicode_code_p(c))
			return Result(ret, 1);
		buffer[k++] = (char)c;
	}
	buffer[k] = 0;
	errno = 0;
	ul = strtoul(buffer, NULL, 16);
	if (errno == ERANGE)
		return Result(ret, 1);
	if (0xFFFFFFFFUL < ul)
		return Result(ret, 1);
	*value = (unicode)(0xFFFFFFFFUL & ul);

	return Result(ret, 0);
}

int find_name_char_(addr *ret, addr name)
{
	int check;
	unicode c;
	size_t size;

	if (symbolp(name)) {
		GetNameSymbol(name, &name);
	}
	Check(! stringp(name), "type error");
	string_length(name, &size);
	if (size == 0)
		return Result(ret, Nil);
	if (size == 1) {
		/* form #\a */
		Return(string_getc_(name, 0, &c));
		character_heap(ret, c);
		return 0;
	}
	Return(unicode_code_(name, size, &c, &check));
	if (! check) {
		/* for #\u123 */
		character_heap(ret, c);
		return 0;
	}

	return findtable_name_char_(ret, name);
}


/*
 *  build
 */
static int defnametable_(addr getname, addr getchar, unicode c, const char *name)
{
	int check;
	addr pos, string, cons;

	character_heap(&pos, c);
	strvect_char_heap(&string, name);
	Return(internp_hashheap_(getname, pos, &cons, &check));
	if (check == 0) {
		/* not found */
		SetCdr(cons, string);
	}
	Return(internp_hashheap_(getchar, string, &cons, &check));
	if (check == 0) {
		/* not found */
		SetCdr(cons, pos);
	}

	return 0;
}

static int defnametable_group_(addr getname, addr getchar)
{
	Return(defnametable_(getname, getchar, 0x07, "Bell"));
	Return(defnametable_(getname, getchar, 0x08, "Backspace"));
	Return(defnametable_(getname, getchar, 0x09, "Tab"));
	Return(defnametable_(getname, getchar, 0x0A, "Newline"));
	Return(defnametable_(getname, getchar, 0x0A, "Linefeed"));
	Return(defnametable_(getname, getchar, 0x0C, "Page"));
	Return(defnametable_(getname, getchar, 0x0D, "Return"));
	Return(defnametable_(getname, getchar, 0x20, "Space"));
	Return(defnametable_(getname, getchar, 0x7F, "Rubout"));
	return 0;
}

void build_character_name(void)
{
	addr getname, getchar;

	hashtable_heap(&getname);
	hashtable_heap(&getchar);
	settest_hashtable(getname, HASHTABLE_TEST_EQL);
	settest_hashtable(getchar, HASHTABLE_TEST_EQUALP);
	Error(defnametable_group_(getname, getchar));
	SetLispRoot(CHAR_NAME, getname);
	SetLispRoot(NAME_CHAR, getchar);
}


/************************************************************
 *  character_queue.c
 ************************************************************/

#define LISP_CHARQUEUESIZE           64

/*
 *  charqueue
 */
static void charbit_size(size_t max, size_t *rsize, size_t *rmax)
{
	size_t size;

	if (0xFFFF < max)
		goto maxsize;
	size = IdxSize + max*sizeoft(unicode);
	if (0xFFFF < size)
		goto maxsize;

	*rmax = max;
	*rsize = size;
	return;

maxsize:
	max = 0x3900;
	size = IdxSize + max*sizeoft(unicode);
	Check(0xFFFF < size, "size error");
	*rmax = max;
	*rsize = size;
}

static void charbit_alloc(LocalRoot local, addr *ret, size_t max, size_t *rmax)
{
	size_t size;

	charbit_size(max, &size, &max);
	if (rmax)
		*rmax = max;
	alloc_arraybody(local, ret, LISPSYSTEM_CHARBIT, 1, (byte16)size);
	SetCharBitSize(*ret, 0);
}

static void charbit_push(addr pos, unicode c)
{
	size_t index;

	GetCharBitSize(pos, &index);
	SetCharBitChar(pos, index, c);
	SetCharBitSize(pos, index + 1);
}

void charqueue_alloc(LocalRoot local, addr *ret, size_t max)
{
	addr pos, root;
	struct charqueue_struct *str;

	if (max == 0)
		max = LISP_CHARQUEUESIZE;
	alloc_smallsize(local, &pos, LISPSYSTEM_CHARQUEUE,
			2, sizeoft(struct charqueue_struct));
	str = StructCharQueue(pos);
	str->size = 0;

	charbit_alloc(local, &root, max, &max);
	str->max = max;

	SetCharQueueRoot(pos, root);
	SetCharQueueTail(pos, root);
	*ret = pos;
}
void charqueue_local(LocalRoot local, addr *ret, size_t max)
{
	Check(local == NULL, "local error");
	charqueue_alloc(local, ret, max);
}
void charqueue_heap(addr *ret, size_t max)
{
	charqueue_alloc(NULL, ret, max);
}

void getsize_charqueue(addr pos, size_t *ret)
{
	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	GetCharQueueSize(pos, ret);
}

void getchar_charqueue(addr pos, size_t index, unicode *ret)
{
	addr root;
	size_t size, quot, rem;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	GetCharQueueSize(pos, &size);
	if (size <= index) {
		*ret =  0;
		return;
	}

	GetCharQueueRoot(pos, &root);
	GetCharQueueMax(pos, &size);
	quot = index / size;
	rem = index % size;
	for (; quot; quot--)
		GetCharBitNext(root, &root);
	Check(root == Nil, "next error");
	GetCharBitChar(root, rem, ret);
}

int push_charqueue_alloc_(LocalRoot local, addr pos, unicode c)
{
	addr tail, next;
	size_t size, max;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	if (! isvalidunicode(c)) {
		fixnum_heap(&pos, (fixnum)c);
		return fmte_("Invalid unicode character, ~A.", pos, NULL);
	}

	GetCharQueueMax(pos, &max);
	GetCharQueueTail(pos, &tail);
	GetCharBitSize(tail, &size);
	if (max <= size) {
		GetCharBitNext(tail, &next);
		if (next == Nil) {
			/* new bit */
			charbit_alloc(local, &next, max, NULL);
			SetCharBitNext(tail, next);
			SetCharQueueTail(pos, next);
		}
		else {
			/* reuse bit */
			SetCharBitSize(next, 0);
			SetCharQueueTail(pos, next);
		}
		tail = next;
	}

	/* push */
	charbit_push(tail, c);
	IncCharQueueSize(pos);

	return 0;
}
int push_charqueue_local_(LocalRoot local, addr pos, unicode c)
{
	Check(local == NULL, "local error");
	return push_charqueue_alloc_(local, pos, c);
}
int push_charqueue_heap_(addr pos, unicode c)
{
	return push_charqueue_alloc_(NULL, pos, c);
}

void make_charqueue_alloc(LocalRoot local, addr pos, addr *ret)
{
	int readonly;
	addr string, next;
	size_t size, bit, index, i;
	unicode *right;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	GetCharQueueSize(pos, &size);
	strvect_alloc(local, &string, size);

	readonly = GetStatusReadOnly(string);
	if (readonly)
		SetStatusValue(string, LISPSTATUS_READONLY, 0);

	GetCharQueueRoot(pos, &next);
	for (index = 0; index < size; ) {
		GetCharBitSize(next, &bit);
		right = (unicode *)PtrCharBitChar(next);
		for (i = 0; i < bit; i++)
			strvect_setc_unsafe(string, index++, right[i]);
		GetCharBitNext(next, &next);
	}

	if (readonly)
		SetStatusValue(string, LISPSTATUS_READONLY, 1);

	*ret = string;
}
void make_charqueue_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	make_charqueue_alloc(local, pos, ret);
}
void make_charqueue_heap(addr pos, addr *ret)
{
	make_charqueue_alloc(NULL, pos, ret);
}

void clear_charqueue(addr pos)
{
	addr root;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	SetCharQueueSize(pos, 0);
	GetCharQueueRoot(pos, &root);
	SetCharQueueTail(pos, root);
	SetCharBitSize(root, 0);
}

void free_charqueue(addr pos)
{
	addr root;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	SetCharQueueSize(pos, 0);
	GetCharQueueRoot(pos, &root);
	SetCharQueueTail(pos, root);
	SetCharBitSize(root, 0);

	SetCharBitNext(root, Nil);
}

int position_charqueue(addr pos, size_t size)
{
	size_t check, max, now;
	addr root;

	/* size check */
	GetCharQueueSize(pos, &check);
	if (check < size)
		return 1;
	/* set size */
	GetCharQueueMax(pos, &max);
	GetCharQueueRoot(pos, &root);
	for (now = size; max < now; now -= max) {
		Check(root == Nil, "root error");
		GetCharBitNext(root, &root);
	}
	SetCharBitSize(root, now);
	SetCharQueueSize(pos, size);
	SetCharQueueTail(pos, root);
	GetCharBitNext(root, &root);
	if (root != Nil) {
		SetCharQueueSize(pos, 0);
	}

	return 0;
}

int pushstring_charqueue_alloc_(LocalRoot local, addr pos, addr push)
{
	unicode c;
	size_t i, size;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type charqueue error");
	Check(! stringp(push), "type string error");
	string_length(push, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(push, i, &c));
		Return(push_charqueue_alloc_(local, pos, c));
	}

	return 0;
}
int pushstring_charqueue_local_(LocalRoot local, addr pos, addr push)
{
	Check(local == NULL, "local error");
	return pushstring_charqueue_alloc_(local, pos, push);
}
int pushstring_charqueue_heap_(addr pos, addr push)
{
	return pushstring_charqueue_alloc_(NULL, pos, push);
}

int pushchar_charqueue_alloc_(LocalRoot local, addr pos, const char *str)
{
	const byte *ptr;
	unicode u;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	ptr = (const byte *)str;
	for (;;) {
		u = (unicode)*ptr;
		if (u == 0)
			break;
		ptr++;
		Return(push_charqueue_alloc_(local, pos, u));
	}

	return 0;
}
int pushchar_charqueue_local_(LocalRoot local, addr pos, const char *str)
{
	Check(local == NULL, "local error");
	return pushchar_charqueue_alloc_(local, pos, str);
}
int pushchar_charqueue_heap_(addr pos, const char *str)
{
	return pushchar_charqueue_alloc_(NULL, pos, str);
}


/************************************************************
 *  clos.c
 ************************************************************/

static int Clos_standard_ignore = 0;

/*
 *  access
 */
struct clos_struct *struct_clos(addr pos)
{
	CheckType(pos, LISPTYPE_CLOS);
	return ClosStruct_Low(pos);
}

void getclassof_clos(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetClassOfClos_Low(pos, ret);
}

void setclassof_clos(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_CLOS);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetClassOfClos_Low(pos, value);
}

void getslot_clos(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetSlotClos_Low(pos, ret);
}

void setslot_clos(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_CLOS);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetSlotClos_Low(pos, value);
}

void getvalue_clos(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetValueClos_Low(pos, ret);
}

void setvalue_clos(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_CLOS);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetValueClos_Low(pos, value);
}

void getfuncall_clos(addr pos, int *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetFuncallClos_Low(pos, ret);
}

void setfuncall_clos(addr pos, int value)
{
	CheckType(pos, LISPTYPE_CLOS);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFuncallClos_Low(pos, value);
}

void getversion_clos(addr pos, fixnum *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetVersionClos_Low(pos, ret);
}

void setversion_clos(addr pos, fixnum value)
{
	CheckType(pos, LISPTYPE_CLOS);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetVersionClos_Low(pos, value);
}

void getslotvector(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	GetSlotVector_Low(pos, index, ret);
}

void setslotvector(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetSlotVector_Low(pos, index, value);
}

void lenslotvector(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	LenSlotVector_Low(pos, ret);
}

void getclosvalue(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_CLOS_VALUE);
	GetClosValue_Low(pos, index, ret);
}

void setclosvalue(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_CLOS_VALUE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetClosValue_Low(pos, index, value);
}

void lenclosvalue(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_CLOS_VALUE);
	LenClosValue_Low(pos, ret);
}

void clos_standard_ignore(int value)
{
	Clos_standard_ignore = value;
}

int clos_standard_class_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_class_p_Low(pos);
}

int clos_standard_generic_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_generic_p_Low(pos);
}

int clos_standard_method_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_method_p_Low(pos);
}

int clos_standard_combination_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_combination_p_Low(pos);
}

int clos_standard_specializer_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_specializer_p_Low(pos);
}


/*
 *  allocate
 */
/* slot-vector */
void slot_vector_alloc(LocalRoot local, addr *ret, size_t size)
{
	alloc_array4(local, ret, LISPSYSTEM_SLOT_VECTOR, size);
}
void slot_vector_local(LocalRoot local, addr *ret, size_t size)
{
	CheckLocal(local);
	slot_vector_alloc(local, ret, size);
}
void slot_vector_heap(addr *ret, size_t size)
{
	slot_vector_alloc(NULL, ret, size);
}

void slot_vector_copy_alloc(LocalRoot local, addr *ret, addr pos)
{
	addr vector, value;
	size_t size, i;

	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	LenSlotVector(pos, &size);
	slot_vector_alloc(local, &vector, size);
	for (i = 0; i < size; i++) {
		GetSlotVector(pos, i, &value);
		slot_copy_alloc(local, &value, value);
		SetSlotVector(vector, i, value);
	}
	*ret = vector;
}
void slot_vector_copy_local(LocalRoot local, addr *ret, addr pos)
{
	CheckLocal(local);
	slot_vector_copy_alloc(local, ret, pos);
}
void slot_vector_copy_heap(addr *ret, addr pos)
{
	slot_vector_copy_alloc(NULL, ret, pos);
}

void slot_vector_copyheap_alloc(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	if (local || (! GetStatusDynamic(pos))) {
		*ret = pos;
		return;
	}

	/* local -> heap */
	slot_vector_copy_alloc(local, ret, pos);
}

void slot_vector_clear(addr pos)
{
	addr slot;
	size_t size, i;

	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	LenSlotVector(pos, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(pos, i, &slot);
		SetClassSlot(slot, Nil);
	}
}


/* clos */
static inline void clos_value_unsafe(LocalRoot local, addr *ret, size_t size)
{
	size_t i;

	alloc_array4(local, ret, LISPSYSTEM_CLOS_VALUE, size);
	for (i = 0; i < size; i++) {
		SetClosValue_Low(*ret, i, Unbound);
	}
}

void clos_value_heap(addr *ret, size_t size)
{
	clos_value_unsafe(NULL, ret, size);
}

static void clos_value_copy_alloc(LocalRoot local, addr *ret, addr pos, size_t size)
{
	addr instance, value;
	size_t i;

	alloc_array4(local, &instance, LISPSYSTEM_CLOS_VALUE, size);
	for (i = 0; i < size; i++) {
		GetClosValue_Low(pos, i, &value);
		SetClosValue_Low(instance, i, value);
	}
	*ret = instance;
}

static inline void clos_unsafe(LocalRoot local, addr *ret)
{
	alloc_smallsize(local, ret,
			LISPTYPE_CLOS, CLOS_INDEX_SIZE, sizeoft(struct clos_struct));
}

void clos_alloc(LocalRoot local, addr *ret, addr slots)
{
	addr pos, value;
	size_t size;

	CheckType(slots, LISPSYSTEM_SLOT_VECTOR);
	clos_unsafe(local, &pos);

	/* value */
	LenSlotVector_Low(slots, &size);
	clos_value_unsafe(local, &value, size);

	/* clos */
	SetClassOfClos_Low(pos, Unbound);
	SetSlotClos_Low(pos, slots);
	SetValueClos_Low(pos, value);
	SetFuncallClos_Low(pos, 0);
	SetVersionClos_Low(pos, 0);

	/* result */
	*ret = pos;
}
void clos_local(LocalRoot local, addr *ret, addr slots)
{
	CheckLocal(local);
	clos_alloc(local, ret, slots);
}
void clos_heap(addr *ret, addr slots)
{
	clos_alloc(NULL, ret, slots);
}

void clos_destroy(addr pos)
{
	CheckType(pos, LISPTYPE_CLOS);
	SetClassOfClos_Low(pos, Nil);
	SetSlotClos_Low(pos, Nil);
	SetValueClos_Low(pos, Nil);
	SetFuncallClos_Low(pos, 0);
	SetVersionClos_Low(pos, 0);
}

void clos_swap(addr a, addr b)
{
	int i1, i2;
	fixnum f1, f2;
	addr v1, v2;

	CheckType(a, LISPTYPE_CLOS);
	CheckType(b, LISPTYPE_CLOS);
	/* class-of */
	GetClassOfClos_Low(a, &v1);
	GetClassOfClos_Low(b, &v2);
	SetClassOfClos_Low(a, v2);
	SetClassOfClos_Low(b, v1);
	/* slot */
	GetSlotClos_Low(a, &v1);
	GetSlotClos_Low(b, &v2);
	SetSlotClos_Low(a, v2);
	SetSlotClos_Low(b, v1);
	/* value */
	GetValueClos_Low(a, &v1);
	GetValueClos_Low(b, &v2);
	SetValueClos_Low(a, v2);
	SetValueClos_Low(b, v1);
	/* funcall */
	GetFuncallClos_Low(a, &i1);
	GetFuncallClos_Low(b, &i2);
	SetFuncallClos_Low(a, i2);
	SetFuncallClos_Low(b, i1);
	/* version */
	GetVersionClos_Low(a, &f1);
	GetVersionClos_Low(b, &f2);
	SetVersionClos_Low(a, f2);
	SetVersionClos_Low(b, f1);
}

void clos_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	int y;
	fixnum z;
	addr value, x;

	CheckType(pos, LISPTYPE_CLOS);
	clos_unsafe(local, &value);

	/* class-of */
	GetClassOfClos_Low(pos, &x);
	SetClassOfClos_Low(value, x);
	/* slot */
	GetSlotClos_Low(pos, &x);
	SetSlotClos_Low(value, x);
	/* value */
	GetValueClos_Low(pos, &x);
	SetValueClos_Low(value, x);
	/* funcall */
	GetFuncallClos_Low(pos, &y);
	SetFuncallClos_Low(value, y);
	/* version */
	GetVersionClos_Low(pos, &z);
	SetVersionClos_Low(value, z);
	/* result */
	*ret = value;
}

void clos_allcopy_alloc(LocalRoot local, addr pos, addr *ret)
{
	addr instance, x;
	size_t size;

	/* clos */
	clos_copy_alloc(local, pos, &instance);
	/* slot */
	GetSlotClos_Low(pos, &x);
	LenSlotVector(x, &size);
	slot_vector_copy_alloc(local, &x, x);
	SetSlotClos_Low(instance, x);
	/* value */
	GetValueClos_Low(pos, &x);
	clos_value_copy_alloc(local, &x, x, size);
	SetValueClos_Low(instance, x);
	/* result */
	*ret = instance;
}

void clos_getslots_heap(addr pos, addr *ret)
{
	addr list, slots, x;
	size_t size, i;

	CheckType(pos, LISPTYPE_CLOS);
	GetSlotClos_Low(pos, &slots);
	LenSlotVector(slots, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &x);
		GetNameSlot(x, &x);
		cons_heap(&list, x, list);
	}
	nreverse(ret, list);
}


/*
 *  control
 */
int closp(addr pos)
{
	return GetType(pos) == LISPTYPE_CLOS;
}

int slot_vector_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_SLOT_VECTOR;
}

int clos_value_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_CLOS_VALUE;
}

int clos_funcall_p(addr pos)
{
	int check;

	CheckType(pos, LISPTYPE_CLOS);
	GetFuncallClos_Low(pos, &check);

	return check;
}

int slot_class_p(addr pos)
{
	int check;

	CheckType(pos, LISPSYSTEM_SLOT);
	GetAllocationSlot_Low(pos, &check);

	return check != 0;
}

int slot_instance_p(addr pos)
{
	int check;

	CheckType(pos, LISPSYSTEM_SLOT);
	GetAllocationSlot_Low(pos, &check);

	return check == 0;
}

void clos_set_funcall(addr pos)
{
	CheckType(pos, LISPTYPE_CLOS);
	SetFuncallClos_Low(pos, 1);
}

void slot_set_class(addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	SetAllocationSlot_Low(pos, 1);
}

void slot_set_instance(addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	SetAllocationSlot_Low(pos, 0);
}

int slot_set_allocation_(addr pos, addr value)
{
	addr check;

	CheckType(pos, LISPSYSTEM_SLOT);

	/* instance */
	GetConst(KEYWORD_INSTANCE, &check);
	if (check == value) {
		slot_set_instance(pos);
		return 0;
	}

	/* class */
	GetConst(KEYWORD_CLASS, &check);
	if (check == value) {
		slot_set_class(pos);
		return 0;
	}

	/* error */
	return fmte_("Invalid :allocation value ~S.", value, NULL);
}

int clos_errorp(addr pos, size_t index, constindex name)
{
	addr key, slot, vector, check;
	size_t size, i;

	CheckType(pos, LISPTYPE_CLOS);
	GetConstant(name, &key);
	Check(! symbolp(key), "type error");
	GetSlotClos_Low(pos, &slot);
	GetValueClos_Low(pos, &vector);
	LenSlotVector(slot, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slot, i, &pos);
		GetNameSlot(pos, &check);
		Check(! symbolp(check), "type error");
		if (check == key)
			return index != i;
	}

	return 1;
}

int clos_getp(addr pos, addr key, addr *ret)
{
	addr slot, vector, check;
	size_t size, i;

	CheckType(pos, LISPTYPE_CLOS);
	Check(! symbolp(key), "type error");
	GetSlotClos_Low(pos, &slot);
	GetValueClos_Low(pos, &vector);
	LenSlotVector(slot, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slot, i, &pos);
		GetNameSlot(pos, &check);
		Check(! symbolp(check), "type error");
		if (check == key) {
			GetClosValue(vector, i, ret);
			return 1;
		}
	}

	return 0;
}

int clos_setp(addr pos, addr key, addr value)
{
	addr slot, vector, check;
	size_t size, i;

	CheckType(pos, LISPTYPE_CLOS);
	Check(! symbolp(key), "type error");
	GetSlotClos_Low(pos, &slot);
	GetValueClos_Low(pos, &vector);
	LenSlotVector(slot, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slot, i, &pos);
		GetNameSlot(pos, &check);
		Check(! symbolp(check), "type error");
		if (check == key) {
			SetClosValue(vector, i, value);
			return 1;
		}
	}

	return 0;
}

int clos_checkp_(addr pos, addr key, addr *value, int *ret)
{
	addr check;

	CheckType(pos, LISPTYPE_CLOS);
	if (! clos_getp(pos, key, &check))
		return Result(ret, 0);
	if (check == Unbound) {
		*ret = 0;
		return call_unbound_slot_(NULL, pos, key);
	}
	*value = check;
	return Result(ret, 1);
}

int clos_get_(addr pos, addr key, addr *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	if (! clos_getp(pos, key, ret)) {
		*ret = Nil;
		return fmte_("The slot name ~S don't exist in the ~S.", key, pos, NULL);
	}

	return 0;
}

int clos_set_(addr pos, addr key, addr value)
{
	CheckType(pos, LISPTYPE_CLOS);
	if (! clos_setp(pos, key, value))
		return fmte_("The slot name ~S don't exist in the ~S.", key, pos, NULL);

	return 0;
}

int clos_check_(addr pos, addr key, addr *ret)
{
	int check;

	CheckType(pos, LISPTYPE_CLOS);
	Return(clos_checkp_(pos, key, ret, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("The slot name ~S don't exist in the ~S.", key, pos, NULL);
	}

	return 0;
}

void clos_getelt(addr pos, size_t index, addr *ret)
{
	addr vector;
#ifdef LISP_DEBUG
	size_t size;
#endif

	CheckType(pos, LISPTYPE_CLOS);
	GetValueClos_Low(pos, &vector);
#ifdef LISP_DEBUG
	LenClosValue(vector, &size);
	Check(size <= index, "size error");
#endif
	GetClosValue(vector, index, ret);
}

void clos_setelt(addr pos, size_t index, addr value)
{
	addr vector;
#ifdef LISP_DEBUG
	size_t size;
#endif

	CheckType(pos, LISPTYPE_CLOS);
	GetValueClos_Low(pos, &vector);
#ifdef LISP_DEBUG
	LenClosValue(vector, &size);
	Check(size <= index, "size error");
#endif
	SetClosValue(vector, index, value);
}

int clos_checkelt_(addr pos, size_t index, addr *ret)
{
	addr check;

	CheckType(pos, LISPTYPE_CLOS);
	clos_getelt(pos, index, &check);
	if (check == Unbound) {
		*ret = Nil;
		return call_unbound_slot_(NULL, pos, check);
	}
	else {
		return Result(ret, check);
	}
}

int clos_getconst_(addr pos, constindex index, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	return clos_get_(pos, key, ret);
}

int clos_setconst_(addr pos, constindex index, addr value)
{
	addr key;
	GetConstant(index, &key);
	return clos_set_(pos, key, value);
}

int clos_checkconst_(addr pos, constindex index, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	return clos_check_(pos, key, ret);
}


/*
 *  check
 */
int clos_slot_exists_p(addr pos, addr name)
{
	CheckType(pos, LISPTYPE_CLOS);
	return clos_getp(pos, name, &name);
}

int clos_slot_boundp_nil(addr pos, addr name)
{
	CheckType(pos, LISPTYPE_CLOS);
	if (clos_getp(pos, name, &name))
		return name != Unbound;
	else
		return -1; /* error no slot. */
}

int clos_slot_boundp_(addr pos, addr name, int *ret)
{
	int check;

	CheckType(pos, LISPTYPE_CLOS);
	check = clos_slot_boundp_nil(pos, name);
	if (check < 0) {
		*ret = 0;
		return fmte_("The pos object ~S have no ~S slot.", pos, name, NULL);
	}

	return Result(ret, check);
}

int clos_slot_makunbound_nil_(addr pos, addr name, int *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	if (clos_slot_exists_p(pos, name)) {
		Return(clos_set_(pos, name, Unbound));
		return Result(ret, 0);
	}
	/* error */
	return Result(ret, 1);
}

int clos_slot_makunbound_(addr pos, addr name)
{
	int check;

	CheckType(pos, LISPTYPE_CLOS);
	Return(clos_slot_makunbound_nil_(pos, name, &check));
	if (check)
		return fmte_("The slot have no ~S.", name, NULL);

	return 0;
}


/*
 *  table
 */
/* clos */
void clos_find_class_nil(addr name, addr *ret)
{
	Check(! symbolp(name), "type name error");
	getclass_symbol(name, ret);
}

int clos_find_class_(addr name, addr *ret)
{
	Check(! symbolp(name), "type name error");
	clos_find_class_nil(name, ret);
	if (*ret == Nil)
		return fmte_("No class named ~S.", name, NULL);

	return 0;
}

void clos_define_class(addr name, addr value)
{
	Check(! symbolp(name), "type name error");
	Check((! closp(value)) && (value != Nil), "type value error");
	if (value == Nil)
		remclass_symbol(name);
	else
		setclass_symbol(name, value);
}

/* generic */
void clos_find_generic_nil(addr name, addr *ret)
{
	Check(! function_name_p(name), "type error");

	getglobal_parse_callname(name, &name);
	if (name == Unbound || (! closp(name)))
		*ret = Nil;
	else
		*ret = name;
}

int clos_find_generic_(addr name, addr *ret)
{
	clos_find_generic_nil(name, ret);
	if (*ret == Nil)
		return fmte_("No generic function named ~S.", name, NULL);

	return 0;
}

int clos_define_generic_(addr name, addr value)
{
	Check(! function_name_p(name), "type name error");
	return setglobal_parse_callname_(name, value);
}

/* method-combination */
void clos_find_combination_nil(addr name, addr *ret)
{
	Check(! symbolp(name), "type name error");
	getcombination_symbol(name, ret);
}

int clos_find_combination_(addr name, addr *ret)
{
	clos_find_combination_nil(name, ret);
	if (*ret == Nil)
		return fmte_("No method combination named ~S.", name, NULL);

	return 0;
}

void clos_define_combination(addr name, addr value)
{
	Check(! symbolp(name), "type name error");
	setcombination_symbol(name, value);
}

/* eql-specializser */
static void clos_table_specializer(addr *ret)
{
	*ret = LispRoot(SPECIALIZER);
}

int clos_find_specializer_nil_(addr name, addr *ret)
{
	addr table;
	clos_table_specializer(&table);
	return findnil_hashtable_(table, name, ret);
}

int clos_find_specializer_(addr name, addr *ret)
{
	Return(clos_find_specializer_nil_(name, ret));
	if (*ret == Nil)
		return fmte_("No method eql-specializer named ~S.", name, NULL);

	return 0;
}

int clos_define_specializer_(addr name, addr value)
{
	addr table;

	clos_table_specializer(&table);
	Return(intern_hashheap_(table, name, &name));
	SetCdr(name, value);

	return 0;
}

void clos_forget_all_specializer_unsafe(void)
{
	addr table;
	clos_table_specializer(&table);
	clear_hashtable_heap(table);
}


/*
 *  build
 */
void init_clos(void)
{
	init_clos_generic();
	init_clos_make();
	init_clos_type();
}

static void build_clos_table(Execute ptr)
{
	addr pos;

	/* eql-specializer */
	hashtable_size_heap(&pos, CLOS_TABLE_SPECIALIZER_SIZE);
	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	SetLispRoot(SPECIALIZER, pos);
}

void build_clos(Execute ptr)
{
	/* Variable */
	Clos_standard_class = 0;
	Clos_standard_generic = 0;
	Clos_standard_method = 0;
	Clos_standard_combination = 0;
	Clos_standard_specializer = 0;
	Clos_standard_ignore = 0;
	/* build */
	build_clos_table(ptr);
	build_clos_class(ptr->local);
	build_clos_combination();
}


/************************************************************
 *  clos_cache.c
 ************************************************************/

/*
 *  cache -> nil | (object1 ...)
 */
int hashindex_cache_(addr right, size_t size, size_t *ret)
{
	addr left;
	size_t value, index;
	fixnum check;

	value = 0;
	for (index = 0; right != Nil; index++) {
		Check(GetType(right) != LISPTYPE_CONS, "type error");
		GetCons(right, &left, &right);
		Return(sxhash_eq_(left, &check));
		value += (size_t)check + index;
	}

	return Result(ret, value % size);
}

int cache_equal_function_(addr right1, addr right2, int *ret)
{
	int check1, check2;
	addr left1, left2;

	check1 = (right1 == Nil);
	check2 = (right2 == Nil);
	if (check1 && check2)
		return Result(ret, 1);
	if (check1)
		return Result(ret, 0);
	if (check2)
		return Result(ret, 0);
	Check(GetType(right1) != LISPTYPE_CONS, "type right1 error");
	Check(GetType(right2) != LISPTYPE_CONS, "type right2 error");
	GetCons(right1, &left1, &right1);
	GetCons(right2, &left2, &right2);
	if (left1 != left2)
		return Result(ret, 0);

	return cache_equal_function_(right1, right2, ret);
}

int cache_equal_debug(addr left, addr right)
{
	int check;
	check = 0;
	Error(cache_equal_function_(left, right, &check));
	return check;
}


/************************************************************
 *  clos_class.c
 ************************************************************/

/*
 *  access
 */
static int stdget_class_constant_(addr pos, addr *ret,
		enum Clos_class_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_class_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_CLASS, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}

static int stdset_class_constant_(addr pos, addr value,
		enum Clos_class_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_class_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_CLASS, &check);
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
#define StdGetClass_(p,r,a,b) \
	stdget_class_constant_((p), (r), Clos_class_##a, CONSTANT_CLOSNAME_##b)
#define StdSetClass_(p,r,a,b) \
	stdset_class_constant_((p), (r), Clos_class_##a, CONSTANT_CLOSNAME_##b)

void stdget_class_name_check(addr pos, addr *ret)
{
	addr key;

	if (GetType(pos) != LISPTYPE_CLOS)
		goto unbound;
	GetConstant(CONSTANT_CLOSNAME_NAME, &key);
	if (! clos_getp(pos, key, &pos))
		goto unbound;
	if (pos == Unbound)
		goto unbound;
	*ret = pos;
	return;

unbound:
	*ret = Unbound;
}

int stdget_class_name_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, name, NAME);
}
int stdset_class_name_(addr pos, addr value)
{
	return StdSetClass_(pos, value, name, NAME);
}

int stdget_class_direct_slots_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, direct_slots, DIRECT_SLOTS);
}
int stdset_class_direct_slots_(addr pos, addr value)
{
	return StdSetClass_(pos, value, direct_slots, DIRECT_SLOTS);
}

int stdget_class_direct_subclasses_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, direct_subclasses, DIRECT_SUBCLASSES);
}
int stdset_class_direct_subclasses_(addr pos, addr value)
{
	return StdSetClass_(pos, value, direct_subclasses, DIRECT_SUBCLASSES);
}

int stdget_class_direct_superclasses_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, direct_superclasses, DIRECT_SUPERCLASSES);
}
int stdset_class_direct_superclasses_(addr pos, addr value)
{
	return StdSetClass_(pos, value, direct_superclasses, DIRECT_SUPERCLASSES);
}

int stdget_class_precedence_list_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, precedence_list, CLASS_PRECEDENCE_LIST);
}
int stdset_class_precedence_list_(addr pos, addr value)
{
	return StdSetClass_(pos, value, precedence_list, CLASS_PRECEDENCE_LIST);
}

int stdget_class_slots_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, slots, EFFECTIVE_SLOTS);
}
int stdset_class_slots_(addr pos, addr value)
{
	return StdSetClass_(pos, value, slots, EFFECTIVE_SLOTS);
}

int stdget_class_finalized_p_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, finalized_p, FINALIZED_P);
}
int stdset_class_finalized_p_(addr pos, addr value)
{
	return StdSetClass_(pos, value, finalized_p, FINALIZED_P);
}

int stdget_class_prototype_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, prototype, PROTOTYPE);
}
int stdset_class_prototype_(addr pos, addr value)
{
	return StdSetClass_(pos, value, prototype, PROTOTYPE);
}

int stdget_class_default_initargs_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, default_initargs, DEFAULT_INITARGS);
}
int stdset_class_default_initargs_(addr pos, addr value)
{
	return StdSetClass_(pos, value, default_initargs, DEFAULT_INITARGS);
}

int stdget_class_direct_default_initargs_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, direct_default_initargs, DIRECT_DEFAULT_INITARGS);
}
int stdset_class_direct_default_initargs_(addr pos, addr value)
{
	return StdSetClass_(pos, value, direct_default_initargs, DIRECT_DEFAULT_INITARGS);
}

int stdget_class_version_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, version, VERSION);
}
int stdset_class_version_(addr pos, addr value)
{
	return StdSetClass_(pos, value, version, VERSION);
}

int stdget_class_document_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, document, DOCUMENTATION);
}
int stdset_class_document_(addr pos, addr value)
{
	return StdSetClass_(pos, value, document, DOCUMENTATION);
}

int stdget_class_redefined_class_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, redefined_class, REDEFINED_CLASS);
}
int stdset_class_redefined_class_(addr pos, addr value)
{
	return StdSetClass_(pos, value, redefined_class, REDEFINED_CLASS);
}


/*
 *  check
 */
int clos_subclass_p_(addr clos, addr super, int *ret)
{
	CheckType(clos, LISPTYPE_CLOS);
	CheckType(super, LISPTYPE_CLOS);
	Return(stdget_class_precedence_list_(clos, &clos));

	return Result(ret, find_list_eq_unsafe(super, clos));
}

int clos_subtype_p_(addr clos, addr super, int *ret)
{
	CheckType(clos, LISPTYPE_CLOS);
	CheckType(super, LISPTYPE_CLOS);
	GetClassOfClos(clos, &clos);
	Check(clos == Unbound, "unbound error");

	return clos_subclass_p_(clos, super, ret);
}

static int clos_constant_p_(addr clos, constindex index, int *ret)
{
	addr super;

	if (! closp(clos))
		return Result(ret, 0);
	GetConstant(index, &super);
	return clos_subtype_p_(clos, super, ret);
}

int clos_class_p_(addr clos, int *ret)
{
	return clos_constant_p_(clos, CONSTANT_CLOS_CLASS, ret);
}

int clos_funcallable_p_(addr clos, int *ret)
{
	addr super;

	if (! closp(clos))
		return Result(ret, 0);
	if (clos_funcall_p(clos))
		return Result(ret, 1);
	GetConst(CLOS_FUNCALLABLE_STANDARD_OBJECT, &super);
	return clos_subtype_p_(clos, super, ret);
}

int clos_generic_p_(addr clos, int *ret)
{
	return clos_constant_p_(clos, CONSTANT_CLOS_GENERIC_FUNCTION, ret);
}

int clos_method_p_(addr clos, int *ret)
{
	return clos_constant_p_(clos, CONSTANT_CLOS_METHOD, ret);
}

int clos_define_combination_p_(addr clos, int *ret)
{
	return clos_constant_p_(clos, CONSTANT_CLOS_DEFINE_METHOD_COMBINATION, ret);
}

int clos_define_long_combination_p_(addr clos, int *ret)
{
	return clos_constant_p_(clos, CONSTANT_CLOS_DEFINE_LONG_METHOD_COMBINATION, ret);
}

int clos_define_short_combination_p_(addr clos, int *ret)
{
	return clos_constant_p_(clos, CONSTANT_CLOS_DEFINE_SHORT_METHOD_COMBINATION, ret);
}

int clos_combination_p_(addr clos, int *ret)
{
	return clos_constant_p_(clos, CONSTANT_CLOS_METHOD_COMBINATION, ret);
}

int clos_long_combination_p_(addr clos, int *ret)
{
	return clos_constant_p_(clos, CONSTANT_CLOS_LONG_METHOD_COMBINATION, ret);
}

int clos_short_combination_p_(addr clos, int *ret)
{
	return clos_constant_p_(clos, CONSTANT_CLOS_SHORT_METHOD_COMBINATION, ret);
}

int clos_specializer_p_(addr clos, int *ret)
{
	return clos_constant_p_(clos, CONSTANT_CLOS_EQL_SPECIALIZER, ret);
}

int clos_referenced_p_(addr clos, int *ret)
{
	addr super;

	if (! closp(clos))
		return Result(ret, 0);
	GetConst(CLOS_FORWARD_REFERENCED_CLASS, &super);
	Return(clos_class_of_(clos, &clos));
	return Result(ret, clos == super);
}

int clos_built_p_(addr clos, int *ret)
{
	addr super;

	if (! closp(clos))
		return Result(ret, 0);
	GetConst(CLOS_BUILT_IN_CLASS, &super);
	return clos_subtype_p_(clos, super, ret);
}

int funcallp_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FUNCTION:
			return Result(ret, 1);

		case LISPTYPE_CLOS:
			if (clos_funcall_p(pos))
				return Result(ret, 1);
			else
				return clos_funcallable_p_(pos, ret);

		default:
			return Result(ret, 0);
	}
}


/*
 *  make-instance
 */
int clos_find_slotname(addr slots, size_t size, addr name)
{
	addr check;
	size_t i;

	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &check);
		GetNameSlot(check, &check);
		if (check == name)
			return 1;
	}

	return 0;
}

static int clos_instance_unsafe_(LocalRoot local, addr clos, addr slots, addr *ret)
{
	addr instance, value, slot, check;
	size_t size, i, loc;

	CheckType(clos, LISPTYPE_CLOS);
	/* allocate */
	slot_vector_copyheap_alloc(local, &slots, slots);
	clos_alloc(local, &instance, slots);

	/* clos-value */
	LenSlotVector(slots, &size);
	GetValueClos(instance, &value);

	/* class-of */
	SetClassOfClos(instance, clos);

	/* value */
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		/* name check */
		GetNameSlot(slot, &check);
		if (! symbolp(check)) {
			*ret = Nil;
			return fmte_("The slot name ~S must be a symbol.", check, NULL);
		}
		/* already exist */
		if (clos_find_slotname(slots, i, check)) {
			*ret = Nil;
			return fmte_("The slot name ~S already exists.", check, NULL);
		}
		/* location */
		GetLocationSlot(slot, &loc);
		if (loc != i) {
			*ret = Nil;
			return fmte_("The slot location ~A is invalid.", intsizeh(i), NULL);
		}
		/* allocation */
		if (slot_class_p(slot)) {
			*ret = Nil;
			return fmte_("The allocation must be an :INSTANCE.", NULL);
		}
		/* value */
		GetFormSlot(slot, &check);
		SetClosValue(value, i, check);
	}

	return Result(ret, instance);
}
int clos_instance_alloc_(LocalRoot local, addr clos, addr *ret)
{
	addr pos;

	/* finalized-p check */
	Return(stdget_class_finalized_p_(clos, &pos));
	if (pos == Nil) {
		*ret = Nil;
		return fmte_("The class ~S is not finalized.", clos, NULL);
	}

	/* make-instance */
	Return(stdget_class_slots_(clos, &pos));
	return clos_instance_unsafe_(local, clos, pos, ret);
}
int clos_instance_local_(LocalRoot local, addr clos, addr *ret)
{
	CheckLocal(local);
	return clos_instance_alloc_(local, clos, ret);
}
int clos_instance_heap_(addr clos, addr *ret)
{
	return clos_instance_alloc_(NULL, clos, ret);
}


/*
 *  class-precedence-list
 */
static int clos_precedence_classes_(LocalRoot local, addr right, addr *ret)
{
	addr left, list;

	conscar_local(local, &list, right);
	Return(stdget_class_direct_superclasses_(right, &right));
	while (right != Nil) {
		GetCons(right, &left, &right);
		cons_local(local, &list, left, list);
	}
	cons_local(local, &list, Unbound, list);
	nreverse(ret, list);

	return 0;
}

static int clos_precedence_pair_(LocalRoot local, addr right, addr *ret)
{
	addr child, result, cons, left, temp;

	Return(clos_precedence_classes_(local, right, &right));
	child = result = Nil;
	while (right != Nil) {
		GetCons(right, &left, &right);
		if (child == Nil) {
			conscdr_local(local, &child, left);
		}
		else {
			GetCdr(child, &temp);
			SetCar(child, temp);
			SetCdr(child, left);
			GetCons(child, &left, &temp);
			cons_local(local, &cons, left, temp);
			cons_local(local, &result, cons, result);
		}
	}

	return Result(ret, result);
}

static int clos_precedence_super_(
		LocalRoot local, addr pos, addr *ret, addr x, addr list)
{
	addr stack, result, temp, supers, super;

	result = Nil;
	conscar_local(local, &stack, pos);

	while (stack != Nil) {
		for (temp = Nil; stack != Nil; ) {
			GetCons(stack, &supers, &stack);
			if (! find_list_eq_unsafe(supers, result)) {
				cons_local(local, &result, supers, result);
				/* redefine */
				if (supers == x) {
					supers = list;
				}
				else {
					Return(stdget_class_direct_superclasses_(supers, &supers));
				}
				/* supers */
				while (supers != Nil) {
					GetCons(supers, &super, &supers);
					if (pos == super) {
						*ret = Nil;
						return fmte_("Loop detection "
								"in the class-precedence-list.", NULL);
					}
					pushnew_local(local, temp, super, &temp);
				}
			}
		}
		nreverse(&stack, temp);
	}
	nreverse(ret, result);

	return 0;
}

static int clos_precedence_find(addr a1, addr cons)
{
	addr b1, a2, b2;

	Check(GetType(a1) != LISPTYPE_CONS, "type key error");
	GetCons(a1, &a1, &b1);
	while (cons != Nil) {
		GetCons(cons, &a2, &cons);
		GetCons(a2, &a2, &b2);
		if ((a1 == a2) && (b1 == b2))
			return 1;
	}

	return 0;
}

static int clos_precedence_chain_(LocalRoot local, addr pos, addr *ret)
{
	addr one, chain, result;

	for (result = Nil; pos != Nil; ) {
		GetCons(pos, &one, &pos);
		Return(clos_precedence_pair_(local, one, &one));
		while (one != Nil) {
			GetCons(one, &chain, &one);
			/* pushnew */
			if (! clos_precedence_find(chain, result))
				cons_local(local, &result, chain, result);
		}
	}

	return Result(ret, result);
}

static int clos_precedence_top_(addr cons, addr *ret)
{
	int check;
	addr left, right, car, cdr;

	for (right = cons; right != Nil; ) {
		GetCons(right, &left, &right);
		GetCar(left, &left);
		for (check = 0, cdr = cons; cdr != Nil; ) {
			GetCons(cdr, &car, &cdr);
			GetCdr(car, &car);
			if (car == left) {
				check = 1;
				break;
			}
		}
		if (check == 0)
			return Result(ret, left);
	}
	*ret = 0;
	return fmte_("Cannot make class precedence list. "
			"Perhaps, class inherit is loop.", NULL);
}

static int clos_precedence_remove_(addr key, addr right2, addr *ret)
{
	int check;
	addr left, right1, right3, result;

	check = 0;
	result = right1 = right3 = Nil;
	while (right2 != Nil) {
		GetCons(right2, &left, &right3);
		GetCar(left, &left);
		if (left == key) {
			check = 1;
			if (right1 != Nil) {
				SetCdr(right1, right3);
			}
			right2 = right3;
			continue;
		}
		if (result == Nil)
			result = right2;
		right1 = right2;
		right2 = right3;
	}
	if (check == 0) {
		*ret = Nil;
		return fmte_("Cannot make class precedence list. "
				"Class key is not found.", NULL);
	}

	return Result(ret, result);
}

static int clos_precedence_result_(
		LocalRoot local, addr pos, addr *ret, addr x, addr list)
{
	addr root, key;

	Return(clos_precedence_super_(local, pos, &pos, x, list));
	Return(clos_precedence_chain_(local, pos, &pos));
	for (root = Nil; pos != Nil; ) {
		Return(clos_precedence_top_(pos, &key));
		cons_heap(&root, key, root);
		Return(clos_precedence_remove_(key, pos, &pos));
	}
	nreverse(ret, root);

	return 0;
}

int clos_precedence_list_redefine_(
		LocalRoot local, addr pos, addr *ret, addr x, addr list)
{
	LocalStack stack;

	CheckLocal(local);
	CheckType(pos, LISPTYPE_CLOS);
	push_local(local, &stack);
	Return(clos_precedence_result_(local, pos, ret, x, list));
	rollback_local(local, stack);

	return 0;
}

int clos_precedence_list_(LocalRoot local, addr pos, addr *ret)
{
	return clos_precedence_list_redefine_(local, pos, ret, Unbound, Nil);
}


/*
 *  compute-slots
 */
static int clos_slots_name(addr *ret, addr name, addr list)
{
	addr pos, check, next;

	for (; list != Nil; list = next) {
		GetCons(list, &pos, &next);
		GetNameSlot(pos, &check);
		if (name == check) {
			*ret = pos;
			return 1;
		}
	}

	return 0;
}

static int clos_slots_push_(addr pos, addr check)
{
	addr list, a;

	GetArgsSlot(pos, &list);
	GetArgsSlot(check, &check);
	while (check != Nil) {
		Return_getcons(check, &a, &check);
		pushnew_heap(list, a, &list);
	}
	SetArgsSlot(pos, list);

	return 0;
}

static int clos_slots_loop_(LocalRoot local, addr list, addr *ret, size_t *rsize)
{
	addr root, slots, a, b;
	size_t count, i, size;

	root = Nil;
	count = 0;
	while (list != Nil) {
		GetCons(list, &slots, &list);
		Return(stdget_class_direct_slots_(slots, &slots));
		LenSlotVector(slots, &size);
		for (i = 0; i < size; i++) {
			GetSlotVector(slots, i, &a);
			GetNameSlot(a, &b);
			if (clos_slots_name(&b, b, root)) {
				Return(clos_slots_push_(b, a));
			}
			else {
				slot_copy_heap(&a, a);
				cons_local(local, &root, a, root);
				count++;
			}
		}
	}
	nreverse(ret, root);
	*rsize = count;

	return 0;
}

static int clos_slots_gather_(LocalRoot local, addr clos, addr *ret)
{
	addr slots, pos;
	size_t i;

	Return(stdget_class_precedence_list_(clos, &clos));
	Return(clos_slots_loop_(local, clos, &clos, &i));
	slot_vector_heap(&slots, i);
	for (i = 0; clos != Nil; i++) {
		GetCons(clos, &pos, &clos);
		SetSlotVector(slots, i, pos);
		SetLocationSlot(pos, i);
	}

	return Result(ret, slots);
}

int clos_compute_slots_(LocalRoot local, addr clos, addr *ret)
{
	LocalStack stack;

	CheckLocal(local);
	CheckType(clos, LISPTYPE_CLOS);
	push_local(local, &stack);
	Return(clos_slots_gather_(local, clos, ret));
	rollback_local(local, stack);

	return 0;
}


/*
 *  build-check
 */
static void build_clos_class_init(void)
{
	addr pos;

	/* check */
#ifdef LISP_DEBUG
	GetConst(COMMON_STANDARD_CLASS, &pos);
	clos_find_class_nil(pos, &pos);
	if (pos != Nil)
		Abort("STANDARD-CLASS is already exist.");
#endif

	/* symbol type */
	type0_heap(LISPDECL_SYMBOL, &pos);
	SetStatusReadOnly(pos);
	SetConst(CLOSDATA_SYMBOL_TYPE, pos);
}


/*
 *  standard-class
 */
static void slot_make_name_symbol(addr pos, constindex n1, constindex n2, size_t n3)
{
	addr slot, value;

	slot_heap(&slot);
	/* name */
	GetConstant(n1, &value);
	SetNameSlot(slot, value);
	/* type */
	GetConst(CLOSDATA_SYMBOL_TYPE, &value);
	CheckType(value, LISPTYPE_TYPE);
	SetTypeSlot(slot, value);
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	SetArgsSlot(slot, value);
	/* result */
	SetSlotVector(pos, n3, slot);
}
#define SlotMakeNameSymbol_number(x,y,z) \
	slot_make_name_symbol((x), CONSTANT_CLOSNAME_##y, CONSTANT_CLOSKEY_##y, (z))
#define SlotMakeNameSymbol(x,y,z) \
	slot_make_name_symbol((x), CONSTANT_CLOSNAME_##y, CONSTANT_CLOSKEY_##y, Clos_##z)

static void slot_make_name(addr pos, constindex n1, constindex n2, size_t n3)
{
	addr slot, value;

	slot_heap(&slot);
	/* name */
	GetConstant(n1, &value);
	SetNameSlot(slot, value);
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	SetArgsSlot(slot, value);
	/* result */
	SetSlotVector(pos, n3, slot);
}
#define SlotMakeName_number(x,y,z) \
	slot_make_name((x), CONSTANT_CLOSNAME_##y, CONSTANT_CLOSKEY_##y, (z))
#define SlotMakeName(x,y,z) \
	slot_make_name((x), CONSTANT_CLOSNAME_##y, CONSTANT_CLOSKEY_##y, Clos_##z)

static void slot_make_form(addr pos, constindex n1, constindex n2, size_t n3)
{
	addr slot, value;

	slot_heap(&slot);
	/* name */
	GetConstant(n1, &value);
	SetNameSlot(slot, value);
	/* initform */
	SetFormSlot(slot, Nil);
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	SetArgsSlot(slot, value);
	/* result */
	SetSlotVector(pos, n3, slot);
}
#define SlotMakeForm_number(x,y,z) \
	slot_make_form((x), CONSTANT_CLOSNAME_##y, CONSTANT_CLOSKEY_##y, (z))
#define SlotMakeForm(x,y,z) \
	slot_make_form((x), CONSTANT_CLOSNAME_##y, CONSTANT_CLOSKEY_##y, Clos_##z)

static void slot_make_version(addr pos, constindex n1, constindex n2, size_t n3)
{
	addr slot, value;

	slot_heap(&slot);
	/* name */
	GetConstant(n1, &value);
	SetNameSlot(slot, value);
	/* initform */
	fixnum_heap(&value, 0);
	SetFormSlot(slot, value);
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	SetArgsSlot(slot, value);
	/* result */
	SetSlotVector(pos, n3, slot);
}
#define SlotMakeVersion(x,y,z) \
	slot_make_version((x), CONSTANT_CLOSNAME_##y, CONSTANT_CLOSKEY_##y, Clos_##z)

void slotvector_set_location(addr slots)
{
	addr pos;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		SetLocationSlot(pos, i);
	}
}

static void clos_stdclass_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_class_size);
	SlotMakeNameSymbol(slots, NAME, class_name);
	SlotMakeForm(slots, DIRECT_SLOTS, class_direct_slots);
	SlotMakeForm(slots, DIRECT_SUBCLASSES, class_direct_subclasses);
	SlotMakeName(slots, DIRECT_SUPERCLASSES, class_direct_superclasses);
	SlotMakeName(slots, CLASS_PRECEDENCE_LIST, class_precedence_list);
	SlotMakeName(slots, EFFECTIVE_SLOTS, class_slots);
	SlotMakeForm(slots, FINALIZED_P, class_finalized_p);
	SlotMakeName(slots, PROTOTYPE, class_prototype);
	SlotMakeForm(slots, DEFAULT_INITARGS, class_default_initargs);
	SlotMakeForm(slots, DIRECT_DEFAULT_INITARGS, class_direct_default_initargs);
	SlotMakeVersion(slots, VERSION, class_version);
	SlotMakeForm(slots, DOCUMENTATION, class_document);
	SlotMakeForm(slots, REDEFINED_CLASS, class_redefined_class);
	slotvector_set_location(slots);
	*ret = slots;
}

int clos_stdclass_direct_slots_(addr instance, addr slots)
{
#ifdef LISP_DEBUG
	addr check;
#endif
	addr slot;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
#ifdef LISP_DEBUG
		GetClassSlot(slot, &check);
		Check(check != Nil, "slot class error");
#endif
		SetClassSlot(slot, instance);
	}
	return stdset_class_direct_slots_(instance, slots);
}

static int clos_stdclass_dummy_(addr *ret, addr slots)
{
	addr instance;

	clos_heap(&instance, slots);
	SetClassOfClos(instance, Nil);
	Return(clos_stdclass_direct_slots_(instance, slots));
	Return(stdset_class_slots_(instance, slots));
	Return(stdset_class_finalized_p_(instance, T));

	return Result(ret, instance);
}

static int clos_stdclass_make_(addr *ret, addr clos, addr name, addr slots)
{
	addr instance;

	Check(! symbolp(name), "type error");
	Return(clos_instance_heap_(clos, &instance));
	Return(stdset_class_name_(instance, name));
	Return(clos_stdclass_direct_slots_(instance, slots));
	Return(stdset_class_prototype_(instance, instance));

	return Result(ret, instance);
}

static int clos_stdclass_empty_(addr *ret, addr clos, addr name)
{
	addr slots;
	slot_vector_heap(&slots, 0);
	return clos_stdclass_make_(ret, clos, name, slots);
}

static void clos_stdclass_class_of(addr instance, addr class_of)
{
	fixnum version;

	SetClassOfClos(instance, class_of);
	GetVersionClos(class_of, &version);
	SetVersionClos(instance, version);
}

static int list_referenced_check_(addr list, int *ret)
{
	int check;
	addr value;

	while (list != Nil) {
		Return_getcons(list, &value, &list);
		Return(clos_referenced_p_(value, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static void slot_vector_prototype(addr clos, addr slots, addr *ret)
{
	addr pos, slot, check;
	size_t size, i;

	LenSlotVector(slots, &size);
	slot_vector_heap(&pos, size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		if (slot_class_p(slot)) {
			GetClassSlot(slot, &check);
			if (clos == check) {
				slot_copy_heap(&slot, slot);
				slot_set_instance(slot);
			}
		}
		SetSlotVector(pos, i, slot);
	}
	*ret = pos;
}

static void clos_stdclass_prototype_initialize(addr pos, addr slots)
{
	addr slot, value;
	size_t size, i;

	LenSlotVector(slots, &size);
	GetValueClos(pos, &pos);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		if (slot_class_p(slot)) {
			GetFormSlot(slot, &value);
			SetClosValue(pos, i, value);
		}
	}
}

int clos_stdclass_prototype_(addr clos)
{
	addr pos, slots, value;

	/* make prototype */
	Return(stdget_class_slots_(clos, &slots));
	slot_vector_prototype(clos, slots, &value);
	clos_heap(&pos, value);
	SetClassOfClos(pos, clos);
	/* initialize shared slots */
	clos_stdclass_prototype_initialize(pos, slots);
	/* result */
	return stdset_class_prototype_(clos, pos);
}

static int clos_stdclass_inherit_(LocalRoot local, addr pos, addr clos, addr supers)
{
	int check;
	addr list, super;

	/* class-of */
	clos_stdclass_class_of(pos, clos);
	/* direct-superclasses */
	Return(stdset_class_direct_superclasses_(pos, supers));
	/* forward-referenced-class check */
	Return(list_referenced_check_(supers, &check));
	if (! check) {
		/* class-precedence-list */
		Return(clos_precedence_list_(local, pos, &list));
		Return(stdset_class_precedence_list_(pos, list));
		/* effective-slots */
		Return(clos_compute_slots_(local, pos, &list));
		Return(stdset_class_slots_(pos, list));
		/* finalized-p */
		Return(clos_stdclass_prototype_(pos));
		Return(stdset_class_finalized_p_(pos, T));
	}
	/* direct-subclasses */
	while (supers != Nil) {
		GetCons(supers, &super, &supers);
		Return(clos_referenced_p_(super, &check));
		if (! check) {
			Return(stdget_class_direct_subclasses_(super, &list));
			cons_heap(&list, pos, list);
			Return(stdset_class_direct_subclasses_(super, list));
		}
	}
	/* setf-find-class */
	Return(stdget_class_name_(pos, &list));
	clos_define_class(list, pos);

	return 0;
}

static int clos_stdclass_single_(LocalRoot local, addr pos, addr clos, addr super)
{
	conscar_heap(&super, super);
	return clos_stdclass_inherit_(local, pos, clos, super);
}

static int clos_stdclass_metaclass_(LocalRoot local, addr *ret)
{
	addr slots, name;
	addr builtin, tc, stdobject, classc, stdclass, metaobject, specializer;

	/* dummy metaclass */
	clos_stdclass_slots(&slots);
	Return(clos_stdclass_dummy_(&stdclass, slots));
	/* class */
	slot_vector_clear(slots);
	GetConst(COMMON_CLASS, &name);
	Return(clos_stdclass_make_(&classc, stdclass, name, slots));
	Return(clos_stdclass_inherit_(local, classc, classc, Nil));

	/* object */
	Return(clos_stdclass_empty_(&tc, classc, T));
	GetConst(COMMON_STANDARD_OBJECT, &name);
	Return(clos_stdclass_empty_(&stdobject, classc, name));
	GetConst(CLOSNAME_METAOBJECT, &name);
	Return(clos_stdclass_empty_(&metaobject, classc, name));
	GetConst(CLOSNAME_SPECIALIZER, &name);
	Return(clos_stdclass_empty_(&specializer, classc, name));
	GetConst(COMMON_STANDARD_CLASS, &name);
	Return(clos_stdclass_empty_(&stdclass, classc, name));
	GetConst(COMMON_BUILT_IN_CLASS, &name);
	Return(clos_stdclass_empty_(&builtin, classc, name));

	/* inheritance */
	Return(clos_stdclass_inherit_(local, tc, classc, Nil));
	Return(clos_stdclass_single_(local, stdobject, classc, tc));
	Return(clos_stdclass_single_(local, metaobject, classc, stdobject));
	Return(clos_stdclass_single_(local, specializer, classc, metaobject));
	Return(clos_stdclass_single_(local, classc, classc, specializer));
	Return(clos_stdclass_single_(local, stdclass, stdclass, classc));
	Return(clos_stdclass_single_(local, builtin, stdclass, classc));
	Clos_standard_class = stdclass;

	/* update class-of */
	clos_stdclass_class_of(tc, builtin);
	clos_stdclass_class_of(stdobject, stdclass);
	clos_stdclass_class_of(metaobject, stdclass);
	clos_stdclass_class_of(specializer, stdclass);
	clos_stdclass_class_of(classc, stdclass);
	clos_stdclass_class_of(stdclass, stdclass);
	clos_stdclass_class_of(builtin, stdclass);
	/* constant */
	SetConst(CLOS_T, tc);
	SetConst(CLOS_STANDARD_OBJECT, stdobject);
	SetConst(CLOS_METAOBJECT, metaobject);
	SetConst(CLOS_SPECIALIZER, specializer);
	SetConst(CLOS_CLASS, classc);
	SetConst(CLOS_STANDARD_CLASS, stdclass);
	SetConst(CLOS_BUILT_IN_CLASS, builtin);
	/* constant */
	SetStatusReadOnly(builtin);
	/* result */
	return Result(ret, stdclass);
}

int clos_stdclass_supers_(LocalRoot local,
		addr *ret, addr metaclass, addr name, addr slots, addr supers)
{
	addr instance;

	Return(clos_stdclass_make_(&instance, metaclass, name, slots));
	Return(clos_stdclass_inherit_(local, instance, metaclass, supers));
	return Result(ret, instance);
}

static int clos_stdclass_type_(LocalRoot local,
		addr *ret, addr metaclass, addr name, addr supers)
{
	addr slots;
	slot_vector_heap(&slots, 0);
	return clos_stdclass_supers_(local, ret, metaclass, name, slots, supers);
}

static void clos_stdclass_va_list(addr *ret, va_list args)
{
	addr list, pos;
	constindex arg;

	list = Nil;
	for (;;) {
		arg = va_arg(args, constindex);
		if (arg == CONSTANT_EMPTY)
			break;
		GetConstant(arg, &pos);
		CheckType(pos, LISPTYPE_CLOS);
		cons_heap(&list, pos, list);
	}
	nreverse(ret, list);
}

static int clos_stdclass_va_(LocalRoot local, addr m, constindex n, constindex c, ...)
{
	va_list args;
	addr list, clos, name;

	/* args */
	va_start(args, c);
	clos_stdclass_va_list(&list, args);
	va_end(args);
	/* make class */
	GetConstant(n, &name);
	Check(! symbolp(name), "type error");
	Return(clos_stdclass_type_(local, &clos, m, name, list));
	clos_define_class(name, clos);
	SetConstant(c, clos);

	return 0;
}

#define ClosMakeClass1_(p,m,a,b,c) { \
	Return(clos_stdclass_va_((p),(m), \
				CONSTANT_##a,CONSTANT_##b,CONSTANT_##c,CONSTANT_EMPTY)); \
}
#define ClosMakeClass2_(p,m,a,b,c,d) { \
	Return(clos_stdclass_va_((p),(m), \
				CONSTANT_##a,CONSTANT_##b,CONSTANT_##c,CONSTANT_##d,CONSTANT_EMPTY)); \
}

static int clos_stdclass_slotsconstant_(LocalRoot local, addr metaclass, addr slots,
		constindex n, constindex c, constindex s)
{
	addr name, supers, clos;

	GetConstant(n, &name);
	Check(! symbolp(name), "type error");
	GetConstant(s, &supers);
	CheckType(supers, LISPTYPE_CLOS);
	conscar_heap(&supers, supers);
	Return(clos_stdclass_supers_(local, &clos, metaclass, name, slots, supers));
	clos_define_class(name, clos);
	SetConstant(c, clos);

	return 0;
}
#define ClosMakeClassSlot_(p,m,s,a,b,c) { \
	Return(clos_stdclass_slotsconstant_((p),(m),(s), \
				CONSTANT_##a,CONSTANT_##b,CONSTANT_##c)); \
}

static int clos_stdclass2_slotsconstant_(LocalRoot local,
		addr metaclass, addr slots,
		constindex n, constindex c,
		constindex x, constindex y)
{
	addr name, supers, c1, c2, clos;

	GetConstant(n, &name);
	Check(! symbolp(name), "type error");

	GetConstant(x, &c1);
	GetConstant(y, &c2);
	CheckType(c1, LISPTYPE_CLOS);
	CheckType(c2, LISPTYPE_CLOS);
	list_heap(&supers, c1, c2, NULL);
	Return(clos_stdclass_supers_(local, &clos, metaclass, name, slots, supers));
	clos_define_class(name, clos);
	SetConstant(c, clos);

	return 0;
}
#define ClosMakeClass2Slot_(p,m,s,a,b,x,y) { \
	Return(clos_stdclass2_slotsconstant_((p),(m),(s), \
				CONSTANT_##a, CONSTANT_##b, \
				CONSTANT_##x, CONSTANT_##y)); \
}

static void clos_structure_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_structure_size);
	SlotMakeNameSymbol(slots, NAME, structure_name);
	SlotMakeForm(slots, DIRECT_SLOTS, structure_direct_slots);
	SlotMakeForm(slots, SLOTS, structure_slots);
	SlotMakeForm(slots, DOCUMENTATION, structure_documentation);
	SlotMakeForm(slots, INCLUDE, structure_include);
	SlotMakeName(slots, CLASS_PRECEDENCE_LIST, structure_precedence_list);
	SlotMakeName(slots, VALUE, structure_value);
	SlotMakeName(slots, PREDICATE, structure_predicate);
	SlotMakeForm(slots, ACCESS, structure_access);
	SlotMakeForm(slots, COPIER, structure_copier);
	SlotMakeForm(slots, CONSTRUCTOR, structure_constructor);
	slotvector_set_location(slots);
	*ret = slots;
}

static int build_clos_class_standard_(LocalRoot local)
{
	addr metaclass, structure, slots;

	/* standard-class, others */
	Return(clos_stdclass_metaclass_(local, &metaclass));
	/* structure-class */
	clos_structure_slots(&slots);
	ClosMakeClassSlot_(local, metaclass, slots,
			COMMON_STRUCTURE_CLASS,
			CLOS_STRUCTURE_CLASS,
			CLOS_CLASS);
	/* structure-object */
	GetConst(CLOS_STRUCTURE_CLASS, &structure);
	ClosMakeClass1_(local, structure,
			COMMON_STRUCTURE_OBJECT,
			CLOS_STRUCTURE_OBJECT,
			CLOS_T);
	/* forward-referenced-class */
	clos_stdclass_slots(&slots);
	ClosMakeClassSlot_(local, metaclass, slots,
			CLOSNAME_FORWARD_REFERENCED_CLASS,
			CLOS_FORWARD_REFERENCED_CLASS,
			CLOS_CLASS);

	return 0;
}


/*
 *  standard-generic-function
 */
static void clos_stdgeneric_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_generic_size);
	SlotMakeNameSymbol(slots, NAME, generic_name);
	SlotMakeForm(slots, METHODS, generic_methods);
	SlotMakeName(slots, LAMBDA_LIST, generic_lambda_list);
	SlotMakeName(slots, ARGUMENT_PRECEDENCE_ORDER, generic_argument_precedence_order);
	SlotMakeForm(slots, DECLARATIONS, generic_declarations);
	SlotMakeName(slots, METHOD_CLASS, generic_method_class);
	SlotMakeForm(slots, METHOD_COMBINATION, generic_method_combination);
	SlotMakeForm(slots, VECTOR, generic_vector);
	SlotMakeForm(slots, REMOVE, generic_remove);
	SlotMakeForm(slots, ARGUMENT, generic_argument);
	SlotMakeForm(slots, DOCUMENTATION, generic_documentation);
	SlotMakeName(slots, EQLCHECK, generic_eqlcheck);
	SlotMakeName(slots, CACHE, generic_cache);
	SlotMakeName(slots, CALL, generic_call);
	SlotMakeName(slots, PRECEDENCE_INDEX, generic_precedence_index);
	slotvector_set_location(slots);
	*ret = slots;
}

static int build_clos_class_generic_(LocalRoot local)
{
	addr metaclass, builtin, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	GetConst(CLOS_BUILT_IN_CLASS, &builtin);
	/* function */
	ClosMakeClass1_(local, builtin,
			COMMON_FUNCTION,
			CLOS_FUNCTION,
			CLOS_T);
	/* funcallable-standard-object */
	ClosMakeClass2_(local, metaclass,
			CLOSNAME_FUNCALLABLE_STANDARD_OBJECT,
			CLOS_FUNCALLABLE_STANDARD_OBJECT,
			CLOS_FUNCTION,
			CLOS_STANDARD_OBJECT);
	/* funcallable-standard-class */
	ClosMakeClass1_(local, metaclass,
			CLOSNAME_FUNCALLABLE_STANDARD_CLASS,
			CLOS_FUNCALLABLE_STANDARD_CLASS,
			CLOS_CLASS);
	/* generic-function */
	ClosMakeClass2_(local, metaclass,
			COMMON_GENERIC_FUNCTION,
			CLOS_GENERIC_FUNCTION,
			CLOS_METAOBJECT,
			CLOS_FUNCALLABLE_STANDARD_OBJECT);
	/* standard-generic-function */
	clos_stdgeneric_slots(&slots);
	ClosMakeClassSlot_(local, metaclass, slots,
			COMMON_STANDARD_GENERIC_FUNCTION,
			CLOS_STANDARD_GENERIC_FUNCTION,
			CLOS_GENERIC_FUNCTION);

	return 0;
}


/*
 *  standard-method
 */
static void clos_stdmethod_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_method_size);
	SlotMakeName(slots, FUNCTION, method_function);
	SlotMakeForm(slots, GENERIC_FUNCTION, method_generic_function);
	SlotMakeName(slots, LAMBDA_LIST, method_lambda_list);
	SlotMakeName(slots, QUALIFIERS, method_qualifiers);
	SlotMakeName(slots, SPECIALIZERS, method_specializers);
	slotvector_set_location(slots);
	*ret = slots;
}

static int build_clos_class_method_(LocalRoot local)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* method */
	ClosMakeClass1_(local, metaclass,
			COMMON_METHOD,
			CLOS_METHOD,
			CLOS_METAOBJECT);
	/* standard-method */
	clos_stdmethod_slots(&slots);
	ClosMakeClassSlot_(local, metaclass, slots,
			COMMON_STANDARD_METHOD,
			CLOS_STANDARD_METHOD,
			CLOS_METHOD);
	/* standard-accessor-method */
	ClosMakeClass1_(local, metaclass,
			CLOSNAME_STANDARD_ACCESSOR_METHOD,
			CLOS_STANDARD_ACCESSOR_METHOD,
			CLOS_STANDARD_METHOD);
	/* standard-reader-method */
	ClosMakeClass1_(local, metaclass,
			CLOSNAME_STANDARD_READER_METHOD,
			CLOS_STANDARD_READER_METHOD,
			CLOS_STANDARD_ACCESSOR_METHOD);
	/* standard-writer-method */
	ClosMakeClass1_(local, metaclass,
			CLOSNAME_STANDARD_WRITER_METHOD,
			CLOS_STANDARD_WRITER_METHOD,
			CLOS_STANDARD_ACCESSOR_METHOD);

	return 0;
}


/*
 *  method-combination
 */
static void clos_stdlongcomb_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_longcomb_size);
	SlotMakeNameSymbol(slots, NAME, longcomb_name);
	SlotMakeName(slots, DOCUMENTATION, longcomb_document);
	SlotMakeName(slots, LAMBDA_LIST, longcomb_lambda_list);
	SlotMakeName(slots, BINDING, longcomb_binding);
	SlotMakeName(slots, QUALIFIERS, longcomb_qualifiers);
	SlotMakeName(slots, ARGUMENTS, longcomb_arguments);
	SlotMakeName(slots, GENERIC, longcomb_generic);
	SlotMakeName(slots, FORM, longcomb_form);
	SlotMakeName(slots, FUNCTION, longcomb_function);
	slotvector_set_location(slots);
	*ret = slots;
}

static void clos_stdshortcomb_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_shortcomb_size);
	SlotMakeNameSymbol(slots, NAME, shortcomb_name);
	SlotMakeName(slots, DOCUMENTATION, shortcomb_document);
	SlotMakeName(slots, IDENTITY, shortcomb_identity);
	SlotMakeName(slots, OPERATOR, shortcomb_operator);
	SlotMakeName(slots, ORDER, shortcomb_order);
	slotvector_set_location(slots);
	*ret = slots;
}

static void clos_stdlongdef_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_longdef_size);
	SlotMakeNameSymbol(slots, NAME, longdef_name);
	SlotMakeName(slots, DOCUMENTATION, longdef_document);
	SlotMakeName(slots, LAMBDA_LIST, longdef_lambda_list);
	SlotMakeName(slots, QUALIFIERS, longdef_qualifiers);
	SlotMakeName(slots, ARGUMENTS, longdef_arguments);
	SlotMakeName(slots, GENERIC, longdef_generic);
	SlotMakeName(slots, FORM, longdef_form);
	slotvector_set_location(slots);
	*ret = slots;
}

static void clos_stdshortdef_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_shortdef_size);
	SlotMakeNameSymbol(slots, NAME, shortdef_name);
	SlotMakeName(slots, DOCUMENTATION, shortdef_document);
	SlotMakeName(slots, IDENTITY, shortdef_identity);
	SlotMakeName(slots, OPERATOR, shortdef_operator);
	slotvector_set_location(slots);
	*ret = slots;
}

static int build_clos_class_combination_(LocalRoot local)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* method-combination */
	ClosMakeClass1_(local, metaclass,
			COMMON_METHOD_COMBINATION,
			CLOS_METHOD_COMBINATION,
			CLOS_METAOBJECT);
	/* long-method-combination */
	clos_stdlongcomb_slots(&slots);
	ClosMakeClassSlot_(local, metaclass, slots,
			CLOSNAME_LONG_METHOD_COMBINATION,
			CLOS_LONG_METHOD_COMBINATION,
			CLOS_METHOD_COMBINATION);
	/* short-method-combination */
	clos_stdshortcomb_slots(&slots);
	ClosMakeClassSlot_(local, metaclass, slots,
			CLOSNAME_SHORT_METHOD_COMBINATION,
			CLOS_SHORT_METHOD_COMBINATION,
			CLOS_METHOD_COMBINATION);
	/* define-method-combination */
	ClosMakeClass1_(local, metaclass,
			COMMON_DEFINE_METHOD_COMBINATION,
			CLOS_DEFINE_METHOD_COMBINATION,
			CLOS_STANDARD_OBJECT);
	/* define-long-method-combination */
	clos_stdlongdef_slots(&slots);
	ClosMakeClassSlot_(local, metaclass, slots,
			CLOSNAME_DEFINE_LONG_METHOD_COMBINATION,
			CLOS_DEFINE_LONG_METHOD_COMBINATION,
			CLOS_DEFINE_METHOD_COMBINATION);
	/* define-short-method-combination */
	clos_stdshortdef_slots(&slots);
	ClosMakeClassSlot_(local, metaclass, slots,
			CLOSNAME_DEFINE_SHORT_METHOD_COMBINATION,
			CLOS_DEFINE_SHORT_METHOD_COMBINATION,
			CLOS_DEFINE_METHOD_COMBINATION);

	return 0;
}


/*
 *  eql-specializer
 */
static void clos_stdspecializer_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_specializer_size);
	SlotMakeName(slots, OBJECT, specializer_object);
	SlotMakeName(slots, TYPE, specializer_type);
	slotvector_set_location(slots);
	*ret = slots;
}

static int build_clos_class_specializer_(LocalRoot local)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* eql-specializer */
	clos_stdspecializer_slots(&slots);
	ClosMakeClassSlot_(local, metaclass, slots,
			CLOSNAME_EQL_SPECIALIZER,
			CLOS_EQL_SPECIALIZER,
			CLOS_SPECIALIZER);

	return 0;
}


/*
 *  condition
 */
static void clos_stdcondition_slot1(addr *ret, constindex n1, constindex n2)
{
	addr slots;

	slot_vector_heap(&slots, 1);
	slot_make_name(slots, n1, n2, 0);
	slotvector_set_location(slots);
	*ret = slots;
}
#define SlotMakeCondition1(p,a) \
	clos_stdcondition_slot1((p), CONSTANT_CLOSNAME_##a, CONSTANT_KEYWORD_##a)

static void clos_stdcondition_slot2(addr *ret,
		constindex a1, constindex a2,
		constindex b1, constindex b2)
{
	addr slots;

	slot_vector_heap(&slots, 2);
	slot_make_name(slots, a1, a2, 0);
	slot_make_name(slots, b1, b2, 1);
	slotvector_set_location(slots);
	*ret = slots;
}
#define SlotMakeCondition2(p,a,b) { \
	clos_stdcondition_slot2((p), \
			CONSTANT_CLOSNAME_##a, CONSTANT_KEYWORD_##a, \
			CONSTANT_CLOSNAME_##b, CONSTANT_KEYWORD_##b); \
}

static void clos_simple_condition_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, 2);
	/* :format-control */
	slot_make_name(slots,
			CONSTANT_CLOSNAME_FORMAT_CONTROL,
			CONSTANT_KEYWORD_FORMAT_CONTROL,
			0);
	/* :format-arguments */
	slot_make_form(slots,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS,
			CONSTANT_KEYWORD_FORMAT_ARGUMENTS,
			1);
	slotvector_set_location(slots);
	*ret = slots;
}

static int build_clos_class_condition_(LocalRoot local)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* condition */
	ClosMakeClass1_(local, metaclass,
			COMMON_CONDITION,
			CLOS_CONDITION,
			CLOS_STANDARD_OBJECT);
	/* serious-condition (condition) */
	ClosMakeClass1_(local, metaclass,
			COMMON_SERIOUS_CONDITION,
			CONDITION_SERIOUS_CONDITION,
			CLOS_CONDITION);
	/* simple-condition (condition) :format-control :format-arguments*/
	clos_simple_condition_slots(&slots);
	ClosMakeClassSlot_(local, metaclass, slots,
			COMMON_SIMPLE_CONDITION,
			CONDITION_SIMPLE_CONDITION,
			CLOS_CONDITION);
	/* warning (condition) */
	ClosMakeClass1_(local, metaclass,
			COMMON_WARNING,
			CONDITION_WARNING,
			CLOS_CONDITION);
	/* error (serious-condition) */
	ClosMakeClass1_(local, metaclass,
			COMMON_ERROR,
			CONDITION_ERROR,
			CONDITION_SERIOUS_CONDITION);
	/* storage-condition (serious-condition) */
	ClosMakeClass1_(local, metaclass,
			COMMON_STORAGE_CONDITION,
			CONDITION_STORAGE_CONDITION,
			CONDITION_SERIOUS_CONDITION);
	/* arithmetic-error (error) :operation :operands */
	SlotMakeCondition2(&slots, OPERATION, OPERANDS);
	ClosMakeClassSlot_(local, metaclass, slots,
			COMMON_ARITHMETIC_ERROR,
			CONDITION_ARITHMETIC_ERROR,
			CONDITION_ERROR);
	/* cell-error (error) :name */
	SlotMakeCondition1(&slots, NAME);
	ClosMakeClassSlot_(local, metaclass, slots,
			COMMON_CELL_ERROR,
			CONDITION_CELL_ERROR,
			CONDITION_ERROR);
	/* control-error (error) */
	ClosMakeClass1_(local, metaclass,
			COMMON_CONTROL_ERROR,
			CONDITION_CONTROL_ERROR,
			CONDITION_ERROR);
	/* file-error (error) :pathname */
	SlotMakeCondition1(&slots, PATHNAME);
	ClosMakeClassSlot_(local, metaclass, slots,
			COMMON_FILE_ERROR,
			CONDITION_FILE_ERROR,
			CONDITION_ERROR);
	/* package-error (error) :package */
	SlotMakeCondition1(&slots, PACKAGE);
	ClosMakeClassSlot_(local, metaclass, slots,
			COMMON_PACKAGE_ERROR,
			CONDITION_PACKAGE_ERROR,
			CONDITION_ERROR);
	/* parse-error (error) */
	ClosMakeClass1_(local, metaclass,
			COMMON_PARSE_ERROR,
			CONDITION_PARSE_ERROR,
			CONDITION_ERROR);
	/* print-not-readable (error) :object */
	SlotMakeCondition1(&slots, OBJECT);
	ClosMakeClassSlot_(local, metaclass, slots,
			COMMON_PRINT_NOT_READABLE,
			CONDITION_PRINT_NOT_READABLE,
			CONDITION_ERROR);
	/* program-error (error) */
	ClosMakeClass1_(local, metaclass,
			COMMON_PROGRAM_ERROR,
			CONDITION_PROGRAM_ERROR,
			CONDITION_ERROR);
	/* stream-error (error) :stream */
	SlotMakeCondition1(&slots, STREAM);
	ClosMakeClassSlot_(local, metaclass, slots,
			COMMON_STREAM_ERROR,
			CONDITION_STREAM_ERROR,
			CONDITION_ERROR);
	/* type-error (error) :datum :expected-type */
	SlotMakeCondition2(&slots, DATUM, EXPECTED_TYPE);
	ClosMakeClassSlot_(local, metaclass, slots,
			COMMON_TYPE_ERROR,
			CONDITION_TYPE_ERROR,
			CONDITION_ERROR);
	/* unbound-slot (cell-error) :instance :name */
	SlotMakeCondition1(&slots, INSTANCE);
	ClosMakeClassSlot_(local, metaclass, slots,
			COMMON_UNBOUND_SLOT,
			CONDITION_UNBOUND_SLOT,
			CONDITION_CELL_ERROR);
	/* unbound-variable (cell-error) :name */
	ClosMakeClass1_(local, metaclass,
			COMMON_UNBOUND_VARIABLE,
			CONDITION_UNBOUND_VARIABLE,
			CONDITION_CELL_ERROR);
	/* undefined-function (cell-error) :name */
	ClosMakeClass1_(local, metaclass,
			COMMON_UNDEFINED_FUNCTION,
			CONDITION_UNDEFINED_FUNCTION,
			CONDITION_CELL_ERROR);
	/* style-warning (warning) */
	ClosMakeClass1_(local, metaclass,
			COMMON_STYLE_WARNING,
			CONDITION_STYLE_WARNING,
			CONDITION_WARNING);
	/* simple-error (simple-condition error) :format-control :format-arguments */
	ClosMakeClass2_(local, metaclass,
			COMMON_SIMPLE_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_SIMPLE_CONDITION,
			CONDITION_ERROR);
	/* simple_type_error (simple-condition type_error)
	 *   :format-control :format-arguments :datum :expected-type */
	ClosMakeClass2_(local, metaclass,
			COMMON_SIMPLE_TYPE_ERROR,
			CONDITION_SIMPLE_TYPE_ERROR,
			CONDITION_SIMPLE_CONDITION,
			CONDITION_TYPE_ERROR);
	/* simple-warning (simple-condition warning) :format-control :format-arguments */
	ClosMakeClass2_(local, metaclass,
			COMMON_SIMPLE_WARNING,
			CONDITION_SIMPLE_WARNING,
			CONDITION_SIMPLE_CONDITION,
			CONDITION_WARNING);
	/* division-by-zero (arithmetic-error) :operation :operands */
	ClosMakeClass1_(local, metaclass,
			COMMON_DIVISION_BY_ZERO,
			CONDITION_DIVISION_BY_ZERO,
			CONDITION_ARITHMETIC_ERROR);
	/* floating-point-inexact (arithmetic-error) :operation :operands */
	ClosMakeClass1_(local, metaclass,
			COMMON_FLOATING_POINT_INEXACT,
			CONDITION_FLOATING_POINT_INEXACT,
			CONDITION_ARITHMETIC_ERROR);
	/* floating-point-invalid-operation (arithmetic-error) :operation :operands */
	ClosMakeClass1_(local, metaclass,
			COMMON_FLOATING_POINT_INVALID_OPERATION,
			CONDITION_FLOATING_POINT_INVALID_OPERATION,
			CONDITION_ARITHMETIC_ERROR);
	/* floating-point-overflow (arithmetic-error) :operation :operands */
	ClosMakeClass1_(local, metaclass,
			COMMON_FLOATING_POINT_OVERFLOW,
			CONDITION_FLOATING_POINT_OVERFLOW,
			CONDITION_ARITHMETIC_ERROR);
	/* floating-point-underflow (arithmetic-error) :operation :operands */
	ClosMakeClass1_(local, metaclass,
			COMMON_FLOATING_POINT_UNDERFLOW,
			CONDITION_FLOATING_POINT_UNDERFLOW,
			CONDITION_ARITHMETIC_ERROR);
	/* end-of-file (stream-error) :stream */
	ClosMakeClass1_(local, metaclass,
			COMMON_END_OF_FILE,
			CONDITION_END_OF_FILE,
			CONDITION_STREAM_ERROR);
	/* reader-error (parse-error stream-error) :stream */
	ClosMakeClass2_(local, metaclass,
			COMMON_READER_ERROR,
			CONDITION_READER_ERROR,
			CONDITION_PARSE_ERROR,
			CONDITION_STREAM_ERROR);
	/* lisp-system::simple-control-error (simple-error control-error) */
	ClosMakeClass2_(local, metaclass,
			SYSTEM_SIMPLE_CONTROL_ERROR,
			CONDITION_SIMPLE_CONTROL_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_CONTROL_ERROR);
	/* lisp-system::simple-file-error (simple-error file-error) */
	ClosMakeClass2_(local, metaclass,
			SYSTEM_SIMPLE_FILE_ERROR,
			CONDITION_SIMPLE_FILE_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_FILE_ERROR);
	/* lisp-system::simple-package-error (simple-error package-error) */
	ClosMakeClass2_(local, metaclass,
			SYSTEM_SIMPLE_PACKAGE_ERROR,
			CONDITION_SIMPLE_PACKAGE_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_PACKAGE_ERROR);
	/* lisp-system::simple-parse-error (simple-error parse-error) */
	ClosMakeClass2_(local, metaclass,
			SYSTEM_SIMPLE_PARSE_ERROR,
			CONDITION_SIMPLE_PARSE_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_PARSE_ERROR);
	/* lisp-system::simple-program-error (simple-error program-error) */
	ClosMakeClass2_(local, metaclass,
			SYSTEM_SIMPLE_PROGRAM_ERROR,
			CONDITION_SIMPLE_PROGRAM_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_PROGRAM_ERROR);
	/* lisp-system::simple-reader-error (simple-error reader-error) */
	ClosMakeClass2_(local, metaclass,
			SYSTEM_SIMPLE_READER_ERROR,
			CONDITION_SIMPLE_READER_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_READER_ERROR);
	/* lisp-system::simple-style-warning (simple-warning style-warning) */
	ClosMakeClass2_(local, metaclass,
			SYSTEM_SIMPLE_STYLE_WARNING,
			CONDITION_SIMPLE_STYLE_WARNING,
			CONDITION_SIMPLE_WARNING,
			CONDITION_STYLE_WARNING);
	/* lisp-system::delay-warning (warning) */
	ClosMakeClass1_(local, metaclass,
			SYSTEM_DELAY_WARNING,
			CONDITION_DELAY_WARNING,
			CONDITION_WARNING);

	return 0;
}

static int build_clos_class_system_(LocalRoot local)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* savecore (serious-condition) */
	ClosMakeClass1_(local, metaclass,
			SYSTEM_SAVECORE,
			CONDITION_SAVECORE,
			CONDITION_SERIOUS_CONDITION);
	/* exit (serious-condition) */
	SlotMakeCondition1(&slots, VALUE);
	ClosMakeClassSlot_(local, metaclass, slots,
			SYSTEM_EXIT,
			CONDITION_EXIT,
			CONDITION_SERIOUS_CONDITION);

	return 0;
}


/*
 *  lisp type
 */
static int clos_stdtype_buildin_(LocalRoot local, constindex n, constindex c, addr list)
{
	addr metaclass, name, clos;

	GetConst(CLOS_BUILT_IN_CLASS, &metaclass);
	GetConstant(n, &name);
	Check(! symbolp(name), "type error");
	Return(clos_stdclass_type_(local, &clos, metaclass, name, list));
	clos_define_class(name, clos);
	SetConstant(c, clos);

	return 0;
}

static int clos_stdtype_buildin0_(LocalRoot local,
		constindex n, constindex c)
{
	addr list;

	GetConst(CLOS_T, &list);
	list_heap(&list, list, NULL);
	return clos_stdtype_buildin_(local, n, c, list);
}

static int clos_stdtype_buildin1_(LocalRoot local,
		constindex n, constindex c, constindex s1)
{
	addr list, clos1, clos2;

	GetConstant(s1, &clos1);
	GetConst(CLOS_T, &clos2);
	CheckType(clos1, LISPTYPE_CLOS);
	CheckType(clos2, LISPTYPE_CLOS);
	list_heap(&list, clos1, clos2, NULL);
	return clos_stdtype_buildin_(local, n, c, list);
}

static int clos_stdtype_buildin2_(LocalRoot local,
		constindex n, constindex c, constindex s1, constindex s2)
{
	addr list, clos1, clos2, clos3;

	GetConstant(s1, &clos1);
	GetConstant(s2, &clos2);
	GetConst(CLOS_T, &clos3);
	CheckType(clos1, LISPTYPE_CLOS);
	CheckType(clos2, LISPTYPE_CLOS);
	CheckType(clos3, LISPTYPE_CLOS);
	list_heap(&list, clos1, clos2, clos3, NULL);
	return clos_stdtype_buildin_(local, n, c, list);
}

#define ClosMakeType0_(p,x) { \
	Return(clos_stdtype_buildin0_(p, CONSTANT_COMMON_##x, CONSTANT_CLOS_##x)); \
}
#define ClosMakeType1_(p,x,y) { \
	Return(clos_stdtype_buildin1_(p, \
				CONSTANT_COMMON_##x, CONSTANT_CLOS_##x, CONSTANT_CLOS_##y)); \
}
#define ClosMakeType2_(p,x,y,z) { \
	Return(clos_stdtype_buildin2_(p, \
				CONSTANT_COMMON_##x, CONSTANT_CLOS_##x, \
				CONSTANT_CLOS_##y, CONSTANT_CLOS_##z)); \
}

static int build_clos_class_type_(LocalRoot local)
{
	ClosMakeType0_(local, ARRAY);
	ClosMakeType0_(local, CHARACTER);
	ClosMakeType0_(local, HASH_TABLE);
	ClosMakeType0_(local, NUMBER);
	ClosMakeType0_(local, PACKAGE);
	ClosMakeType0_(local, PATHNAME);
	ClosMakeType0_(local, RANDOM_STATE);
	ClosMakeType0_(local, READTABLE);
	ClosMakeType0_(local, RESTART);
	ClosMakeType0_(local, SEQUENCE);
	ClosMakeType0_(local, STREAM);
	ClosMakeType0_(local, SYMBOL);
	ClosMakeType1_(local, LOGICAL_PATHNAME, PATHNAME);

	ClosMakeType1_(local, LIST, SEQUENCE);
	ClosMakeType1_(local, CONS, LIST);
	ClosMakeType2_(local, VECTOR, ARRAY, SEQUENCE);
	ClosMakeType1_(local, BIT_VECTOR, VECTOR);
	ClosMakeType2_(local, NULL, SYMBOL, LIST);
	ClosMakeType1_(local, STRING, VECTOR);

	ClosMakeType1_(local, COMPLEX, NUMBER);
	ClosMakeType1_(local, REAL, NUMBER);
	ClosMakeType1_(local, FLOAT, REAL);
	ClosMakeType1_(local, RATIONAL, REAL);
	ClosMakeType1_(local, INTEGER, RATIONAL);
	ClosMakeType1_(local, RATIO, RATIONAL);

	ClosMakeType1_(local, BROADCAST_STREAM, STREAM);
	ClosMakeType1_(local, CONCATENATED_STREAM, STREAM);
	ClosMakeType1_(local, ECHO_STREAM, STREAM);
	ClosMakeType1_(local, FILE_STREAM, STREAM);
	ClosMakeType1_(local, STRING_STREAM, STREAM);
	ClosMakeType1_(local, SYNONYM_STREAM, STREAM);
	ClosMakeType1_(local, TWO_WAY_STREAM, STREAM);

	ClosMakeType1_(local, BASE_CHAR, CHARACTER);
	ClosMakeType1_(local, EXTENDED_CHAR, CHARACTER);
	ClosMakeType1_(local, STANDARD_CHAR, BASE_CHAR);
	ClosMakeType1_(local, SIMPLE_ARRAY, ARRAY);
	ClosMakeType2_(local, SIMPLE_VECTOR, VECTOR, SIMPLE_ARRAY);

	ClosMakeType1_(local, BASE_STRING, STRING);
	ClosMakeType2_(local, SIMPLE_STRING, STRING, SIMPLE_ARRAY);
	ClosMakeType2_(local, SIMPLE_BASE_STRING, BASE_STRING, SIMPLE_STRING);
	ClosMakeType2_(local, SIMPLE_BIT_VECTOR, BIT_VECTOR, SIMPLE_ARRAY);
	ClosMakeType1_(local, BIGNUM, INTEGER);
	ClosMakeType1_(local, FIXNUM, INTEGER);
	ClosMakeType1_(local, SHORT_FLOAT, FLOAT);
	ClosMakeType1_(local, SINGLE_FLOAT, FLOAT);
	ClosMakeType1_(local, DOUBLE_FLOAT, FLOAT);
	ClosMakeType1_(local, LONG_FLOAT, FLOAT);
	ClosMakeType1_(local, SIGNED_BYTE, INTEGER);
	ClosMakeType1_(local, UNSIGNED_BYTE, SIGNED_BYTE);
	ClosMakeType1_(local, BIT, UNSIGNED_BYTE);
	ClosMakeType1_(local, COMPILED_FUNCTION, FUNCTION);
	ClosMakeType1_(local, KEYWORD, SYMBOL);

	Return(clos_stdtype_buildin0_(local, CONSTANT_SYSTEM_PAPER, CONSTANT_CLOS_PAPER));

	return 0;
}


/*
 *  Metaobject Protocol
 */
static void clos_mop_slot_definition_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, 9);
	SlotMakeNameSymbol_number(slots, NAME, 0);
	SlotMakeName_number(slots, TYPE, 1);
	SlotMakeName_number(slots, ALLOCATION, 2);
	SlotMakeName_number(slots, INITARGS, 3);
	SlotMakeName_number(slots, INITFORM, 4);
	SlotMakeForm_number(slots, INITFUNCTION, 5);
	SlotMakeForm_number(slots, DOCUMENTATION, 6);
	SlotMakeForm_number(slots, READERS, 7);
	SlotMakeForm_number(slots, WRITERS, 8);
	slotvector_set_location(slots);
	*ret = slots;
}

static int build_clos_class_mop_(LocalRoot local)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* slot-definition */
	ClosMakeClass1_(local, metaclass,
			CLOSNAME_SLOT_DEFINITION,
			CLOS_SLOT_DEFINITION,
			CLOS_METAOBJECT);
	/* direct-slot-definition */
	ClosMakeClass1_(local, metaclass,
			CLOSNAME_DIRECT_SLOT_DEFINITION,
			CLOS_DIRECT_SLOT_DEFINITION,
			CLOS_SLOT_DEFINITION);
	/* effective-slot-definition */
	ClosMakeClass1_(local, metaclass,
			CLOSNAME_EFFECTIVE_SLOT_DEFINITION,
			CLOS_EFFECTIVE_SLOT_DEFINITION,
			CLOS_SLOT_DEFINITION);
	/* standard-slot-definition */
	clos_mop_slot_definition_slots(&slots);
	ClosMakeClassSlot_(local, metaclass, slots,
			CLOSNAME_STANDARD_SLOT_DEFINITION,
			CLOS_STANDARD_SLOT_DEFINITION,
			CLOS_SLOT_DEFINITION);
	/* standard-direct-slot-definition */
	clos_mop_slot_definition_slots(&slots);
	ClosMakeClass2Slot_(local, metaclass, slots,
			CLOSNAME_STANDARD_DIRECT_SLOT_DEFINITION,
			CLOS_STANDARD_DIRECT_SLOT_DEFINITION,
			CLOS_STANDARD_SLOT_DEFINITION,
			CLOS_DIRECT_SLOT_DEFINITION);
	/* standard-effective-slot-definition */
	clos_mop_slot_definition_slots(&slots);
	ClosMakeClass2Slot_(local, metaclass, slots,
			CLOSNAME_STANDARD_EFFECTIVE_SLOT_DEFINITION,
			CLOS_STANDARD_EFFECTIVE_SLOT_DEFINITION,
			CLOS_STANDARD_SLOT_DEFINITION,
			CLOS_EFFECTIVE_SLOT_DEFINITION);

	return 0;
}


/*
 *  build-clos-class
 */
static void build_clos_class_variable(void)
{
	addr pos;

	/* standard-class */
#ifdef LISP_DEBUG
	GetConst(CLOS_STANDARD_CLASS, &pos);
	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_standard_class != pos, "error.");
#endif

	/* standard-generic-function */
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	CheckType(pos, LISPTYPE_CLOS);
	Clos_standard_generic = pos;

	/* standard-method */
	GetConst(CLOS_STANDARD_METHOD, &pos);
	CheckType(pos, LISPTYPE_CLOS);
	Clos_standard_method = pos;

	/* method-combination */
	GetConst(CLOS_METHOD_COMBINATION, &pos);
	CheckType(pos, LISPTYPE_CLOS);
	Clos_standard_combination = pos;

	/* eql-specializer */
	GetConst(CLOS_EQL_SPECIALIZER, &pos);
	CheckType(pos, LISPTYPE_CLOS);
	Clos_standard_specializer = pos;
}

static int build_clos_class_call_(LocalRoot local)
{
	build_clos_class_init();
	Return(build_clos_class_standard_(local));
	Return(build_clos_class_generic_(local));
	Return(build_clos_class_method_(local));
	Return(build_clos_class_combination_(local));
	Return(build_clos_class_specializer_(local));
	Return(build_clos_class_condition_(local));
	Return(build_clos_class_system_(local));
	Return(build_clos_class_type_(local));
	Return(build_clos_class_mop_(local));
	build_clos_class_variable();

	return 0;
}

void build_clos_class(LocalRoot local)
{
	Error(build_clos_class_call_(local));
}


/*
 *  debug
 */
int clos_subclass_p_debug(addr clos, addr super)
{
	int check;
	check = 0;
	Error(clos_subclass_p_(clos, super, &check));
	return check;
}

int clos_subtype_p_debug(addr clos, addr super)
{
	int check;
	check = 0;
	Error(clos_subtype_p_(clos, super, &check));
	return check;
}

int clos_generic_p_debug(addr clos)
{
	int check;
	check = 0;
	Error(clos_generic_p_(clos, &check));
	return check;
}

int clos_method_p_debug(addr clos)
{
	int check;
	check = 0;
	Error(clos_method_p_(clos, &check));
	return check;
}

int clos_define_combination_p_debug(addr clos)
{
	int check;
	check = 0;
	Error(clos_define_combination_p_(clos, &check));
	return check;
}


/************************************************************
 *  clos_combination.c
 ************************************************************/

/*
 *  long-method-combination
 */
static int stdget_longcomb_constant_(addr pos, addr *ret,
		enum Clos_longcomb_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_longcomb_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_LONG_METHOD_COMBINATION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}

static int stdset_longcomb_constant_(addr pos, addr value,
		enum Clos_longcomb_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_longcomb_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_LONG_METHOD_COMBINATION, &check);
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
#define StdGetLongCombination_(p,r,a,b) \
	stdget_longcomb_constant_((p), (r), Clos_longcomb_##a, CONSTANT_CLOSNAME_##b)
#define StdSetLongCombination_(p,r,a,b) \
	stdset_longcomb_constant_((p), (r), Clos_longcomb_##a, CONSTANT_CLOSNAME_##b)

int stdget_longcomb_name_(addr pos, addr *ret)
{
	return StdGetLongCombination_(pos, ret, name, NAME);
}
int stdset_longcomb_name_(addr pos, addr value)
{
	return StdSetLongCombination_(pos, value, name, NAME);
}

int stdget_longcomb_document_(addr pos, addr *ret)
{
	return StdGetLongCombination_(pos, ret, document, DOCUMENTATION);
}
int stdset_longcomb_document_(addr pos, addr value)
{
	return StdSetLongCombination_(pos, value, document, DOCUMENTATION);
}

int stdget_longcomb_lambda_list_(addr pos, addr *ret)
{
	return StdGetLongCombination_(pos, ret, lambda_list, LAMBDA_LIST);
}
int stdset_longcomb_lambda_list_(addr pos, addr value)
{
	return StdSetLongCombination_(pos, value, lambda_list, LAMBDA_LIST);
}

int stdget_longcomb_binding_(addr pos, addr *ret)
{
	return StdGetLongCombination_(pos, ret, binding, BINDING);
}
int stdset_longcomb_binding_(addr pos, addr value)
{
	return StdSetLongCombination_(pos, value, binding, BINDING);
}

int stdget_longcomb_qualifiers_(addr pos, addr *ret)
{
	return StdGetLongCombination_(pos, ret, qualifiers, QUALIFIERS);
}
int stdset_longcomb_qualifiers_(addr pos, addr value)
{
	return StdSetLongCombination_(pos, value, qualifiers, QUALIFIERS);
}

int stdget_longcomb_arguments_(addr pos, addr *ret)
{
	return StdGetLongCombination_(pos, ret, arguments, ARGUMENTS);
}
int stdset_longcomb_arguments_(addr pos, addr value)
{
	return StdSetLongCombination_(pos, value, arguments, ARGUMENTS);
}

int stdget_longcomb_generic_(addr pos, addr *ret)
{
	return StdGetLongCombination_(pos, ret, generic, GENERIC);
}
int stdset_longcomb_generic_(addr pos, addr value)
{
	return StdSetLongCombination_(pos, value, generic, GENERIC);
}

int stdget_longcomb_form_(addr pos, addr *ret)
{
	return StdGetLongCombination_(pos, ret, form, FORM);
}
int stdset_longcomb_form_(addr pos, addr value)
{
	return StdSetLongCombination_(pos, value, form, FORM);
}

int stdget_longcomb_function_(addr pos, addr *ret)
{
	return StdGetLongCombination_(pos, ret, function, FUNCTION);
}
int stdset_longcomb_function_(addr pos, addr value)
{
	return StdSetLongCombination_(pos, value, function, FUNCTION);
}


/*
 *  short-method-combination
 */
static int stdget_shortcomb_constant_(addr pos, addr *ret,
		enum Clos_shortcomb_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_shortcomb_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_SHORT_METHOD_COMBINATION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}

static int stdset_shortcomb_constant_(addr pos, addr value,
		enum Clos_shortcomb_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_shortcomb_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_SHORT_METHOD_COMBINATION, &check);
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
#define StdGetShortCombination_(p,r,a,b) \
	stdget_shortcomb_constant_((p), (r), Clos_shortcomb_##a, CONSTANT_CLOSNAME_##b)
#define StdSetShortCombination_(p,r,a,b) \
	stdset_shortcomb_constant_((p), (r), Clos_shortcomb_##a, CONSTANT_CLOSNAME_##b)

int stdget_shortcomb_name_(addr pos, addr *ret)
{
	return StdGetShortCombination_(pos, ret, name, NAME);
}
int stdset_shortcomb_name_(addr pos, addr value)
{
	return StdSetShortCombination_(pos, value, name, NAME);
}

int stdget_shortcomb_document_(addr pos, addr *ret)
{
	return StdGetShortCombination_(pos, ret, document, DOCUMENTATION);
}
int stdset_shortcomb_document_(addr pos, addr value)
{
	return StdSetShortCombination_(pos, value, document, DOCUMENTATION);
}

int stdget_shortcomb_identity_(addr pos, addr *ret)
{
	return StdGetShortCombination_(pos, ret, identity, IDENTITY);
}
int stdset_shortcomb_identity_(addr pos, addr value)
{
	return StdSetShortCombination_(pos, value, identity, IDENTITY);
}

int stdget_shortcomb_operator_(addr pos, addr *ret)
{
	return StdGetShortCombination_(pos, ret, operator, OPERATOR);
}
int stdset_shortcomb_operator_(addr pos, addr value)
{
	return StdSetShortCombination_(pos, value, operator, OPERATOR);
}

int stdget_shortcomb_order_(addr pos, addr *ret)
{
	return StdGetShortCombination_(pos, ret, order, ORDER);
}
int stdset_shortcomb_order_(addr pos, addr value)
{
	return StdSetShortCombination_(pos, value, order, ORDER);
}


/*
 *  define-long-method-combination
 */
static int stdget_longdef_constant_(addr pos, addr *ret,
		enum Clos_longdef_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_longdef_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}

static int stdset_longdef_constant_(addr pos, addr value,
		enum Clos_longdef_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_longdef_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &check);
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
#define StdGetLongDefineCombination_(p,r,a,b) \
	stdget_longdef_constant_((p), (r), Clos_longdef_##a, CONSTANT_CLOSNAME_##b)
#define StdSetLongDefineCombination_(p,r,a,b) \
	stdset_longdef_constant_((p), (r), Clos_longdef_##a, CONSTANT_CLOSNAME_##b)

int stdget_longdef_name_(addr pos, addr *ret)
{
	return StdGetLongDefineCombination_(pos, ret, name, NAME);
}
int stdset_longdef_name_(addr pos, addr value)
{
	return StdSetLongDefineCombination_(pos, value, name, NAME);
}

int stdget_longdef_document_(addr pos, addr *ret)
{
	return StdGetLongDefineCombination_(pos, ret, document, DOCUMENTATION);
}
int stdset_longdef_document_(addr pos, addr value)
{
	return StdSetLongDefineCombination_(pos, value, document, DOCUMENTATION);
}

int stdget_longdef_lambda_list_(addr pos, addr *ret)
{
	return StdGetLongDefineCombination_(pos, ret, lambda_list, LAMBDA_LIST);
}
int stdset_longdef_lambda_list_(addr pos, addr value)
{
	return StdSetLongDefineCombination_(pos, value, lambda_list, LAMBDA_LIST);
}

int stdget_longdef_qualifiers_(addr pos, addr *ret)
{
	return StdGetLongDefineCombination_(pos, ret, qualifiers, QUALIFIERS);
}
int stdset_longdef_qualifiers_(addr pos, addr value)
{
	return StdSetLongDefineCombination_(pos, value, qualifiers, QUALIFIERS);
}

int stdget_longdef_arguments_(addr pos, addr *ret)
{
	return StdGetLongDefineCombination_(pos, ret, arguments, ARGUMENTS);
}
int stdset_longdef_arguments_(addr pos, addr value)
{
	return StdSetLongDefineCombination_(pos, value, arguments, ARGUMENTS);
}

int stdget_longdef_generic_(addr pos, addr *ret)
{
	return StdGetLongDefineCombination_(pos, ret, generic, GENERIC);
}
int stdset_longdef_generic_(addr pos, addr value)
{
	return StdSetLongDefineCombination_(pos, value, generic, GENERIC);
}

int stdget_longdef_form_(addr pos, addr *ret)
{
	return StdGetLongDefineCombination_(pos, ret, form, FORM);
}
int stdset_longdef_form_(addr pos, addr value)
{
	return StdSetLongDefineCombination_(pos, value, form, FORM);
}


/*
 *  define-short-method-combination
 */
static int stdget_shortdef_constant_(addr pos, addr *ret,
		enum Clos_shortdef_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_shortdef_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}

static int stdset_shortdef_constant_(addr pos, addr value,
		enum Clos_shortdef_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_shortdef_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &check);
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
#define StdGetShortDefineCombination_(p,r,a,b) \
	stdget_shortdef_constant_((p), (r), Clos_shortdef_##a, CONSTANT_CLOSNAME_##b)
#define StdSetShortDefineCombination_(p,r,a,b) \
	stdset_shortdef_constant_((p), (r), Clos_shortdef_##a, CONSTANT_CLOSNAME_##b)

int stdget_shortdef_name_(addr pos, addr *ret)
{
	return StdGetShortDefineCombination_(pos, ret, name, NAME);
}
int stdset_shortdef_name_(addr pos, addr value)
{
	return StdSetShortDefineCombination_(pos, value, name, NAME);
}

int stdget_shortdef_document_(addr pos, addr *ret)
{
	return StdGetShortDefineCombination_(pos, ret, document, DOCUMENTATION);
}
int stdset_shortdef_document_(addr pos, addr value)
{
	return StdSetShortDefineCombination_(pos, value, document, DOCUMENTATION);
}

int stdget_shortdef_identity_(addr pos, addr *ret)
{
	return StdGetShortDefineCombination_(pos, ret, identity, IDENTITY);
}
int stdset_shortdef_identity_(addr pos, addr value)
{
	return StdSetShortDefineCombination_(pos, value, identity, IDENTITY);
}

int stdget_shortdef_operator_(addr pos, addr *ret)
{
	return StdGetShortDefineCombination_(pos, ret, operator, OPERATOR);
}
int stdset_shortdef_operator_(addr pos, addr value)
{
	return StdSetShortDefineCombination_(pos, value, operator, OPERATOR);
}


/*****************************************************************************
 *  check-qualifiers
 *****************************************************************************/
static int qualifiers_equal_list(addr left, addr right)
{
	addr left2, right2, asterisk;

	if (left == Nil && right == Nil)
		return 1;
	if (! consp(left))
		return 0;
	if (! consp(right))
		return 0;
	GetCons(left, &left, &left2);
	GetCons(right, &right, &right2);
	GetConst(COMMON_ASTERISK, &asterisk);
	if ((right != asterisk) && (left != right))
		return 0;
	if (right2 == asterisk)
		return 1;

	return qualifiers_equal_list(left2, right2);
}

static int qualifiers_equal_symbol_call_(Execute ptr, addr left, addr right, int *ret)
{
	addr call;

	conscar_local(ptr->local, &left, left);
	Return(getfunction_global_(right, &call));
	Return(apply_control_(ptr, call, left));
	getresult_control(ptr, &left);
	*ret = left != Nil;

	return 0;
}

static int qualifiers_equal_symbol_(Execute ptr, addr left, addr right, int *ret)
{
	addr control;
	push_control(ptr, &control);
	(void)qualifiers_equal_symbol_call_(ptr, left, right, ret);
	return pop_control_(ptr, control);
}

static int qualifiers_equal_(Execute ptr, addr left, addr right, int *ret)
{
	addr aster;

	*ret = 0;
	GetConst(COMMON_ASTERISK, &aster);
	if (right == aster)
		return Result(ret, 1);
	if (listp(right))
		return Result(ret, qualifiers_equal_list(left, right));
	if (symbolp(right))
		return qualifiers_equal_symbol_(ptr, left, right, ret);

	/* error */
	return fmte_("Invalid method-combination-eualifiers ~S.", right, NULL);
}

static int check_qualifiers_equal_long_(Execute ptr, addr comb, addr qua, int *ret)
{
	int check, size;
	addr cons, list;

	Return(stdget_longcomb_qualifiers_(comb, &cons));
	size = 0;
	while (cons != Nil) {
		GetCons(cons, &list, &cons);
		/* cadr */
		GetCdr(list, &list);
		GetCar(list, &list);
		/* check */
		Return(qualifiers_equal_(ptr, qua, list, &check));
		if (check)
			size++;
	}

	/* equal */
	if (size == 1)
		return Result(ret, 1);
	/* not equal */
	if (size == 0)
		return Result(ret, 0);

	/* error */
	*ret = 0;
	return fmte_("Qualifiers ~S must match a only specializer, "
			"but match multiple specializers.", qua, NULL);
}

static int check_qualifiers_equal_short_(addr comb, addr qua, int *ret)
{
	addr name, check;

	if (! consp(qua))
		return Result(ret, 0);
	GetCons(qua, &qua, &check);
	if (check != Nil)
		return Result(ret, 0);
	Return(stdget_shortcomb_name_(comb, &name));
	if (qua == name)
		return Result(ret, 1);
	GetConst(KEYWORD_AROUND, &check);
	if (qua == check)
		return Result(ret, 1);

	return Result(ret, 0);
}

static int check_qualifiers_equal_standard(addr qua)
{
	addr check;

	/* primary */
	if (qua == Nil)
		return 1;
	if (! consp(qua))
		return 0;
	GetCons(qua, &qua, &check);
	if (check != Nil)
		return 0;
	/* around, before, after */
	GetConst(KEYWORD_AROUND, &check);
	if (qua == check)
		return 1;
	GetConst(KEYWORD_BEFORE, &check);
	if (qua == check)
		return 1;
	GetConst(KEYWORD_AFTER, &check);
	if (qua == check)
		return 1;

	return 0;
}

int check_qualifiers_equal_(Execute ptr, addr comb, addr qua, int *ret)
{
	int check;

	if (comb == Nil)
		return Result(ret, check_qualifiers_equal_standard(qua));
	Return(clos_long_combination_p_(comb, &check));
	if (check)
		return check_qualifiers_equal_long_(ptr, comb, qua, ret);
	Return(clos_short_combination_p_(comb, &check));
	if (check)
		return check_qualifiers_equal_short_(comb, qua, ret);
	/* error */
	*ret = 0;
	return fmte_("Invalid method-combination instance ~S.", comb, NULL);
}


/*****************************************************************************
 *  qualifiers-position
 *****************************************************************************/
int method_combination_qualifiers_count_(addr comb, size_t *ret)
{
	int check;

	if (comb == Nil)
		return Result(ret, Clos_standard_size);
	Return(clos_long_combination_p_(comb, &check));
	if (check) {
		Return(stdget_longcomb_qualifiers_(comb, &comb));
		return Result(ret, length_list_unsafe(comb));
	}
	Return(clos_short_combination_p_(comb, &check));
	if (check)
		return Result(ret, Clos_short_size);
	/* error */
	*ret = 0;
	return fmte_("Invalid method-combination instance ~S.", comb, NULL);
}

static int qualifiers_position_standard_nil(addr qua, size_t *ret)
{
	addr check;

	/* primary */
	if (qua == Nil) {
		*ret = Clos_standard_primary;
		return 0;
	}
	if (! consp(qua))
		return 1;
	GetCons(qua, &qua, &check);
	if (check != Nil)
		return 1;
	/* around, before, after */
	GetConst(KEYWORD_AROUND, &check);
	if (qua == check) {
		*ret = Clos_standard_around;
		return 0;
	}
	GetConst(KEYWORD_BEFORE, &check);
	if (qua == check) {
		*ret = Clos_standard_before;
		return 0;
	}
	GetConst(KEYWORD_AFTER, &check);
	if (qua == check) {
		*ret = Clos_standard_after;
		return 0;
	}

	return 1;
}

static int qualifiers_position_long_nil_(Execute ptr, addr qua, addr comb,
		size_t *rsize, int *ret)
{
	int check;
	addr cons, list;
	size_t index;

	Return(stdget_longcomb_qualifiers_(comb, &cons));
	for (index = 0; cons != Nil; index++) {
		GetCons(cons, &list, &cons);
		/* cadr */
		GetCdr(list, &list);
		GetCar(list, &list);
		/* check */
		Return(qualifiers_equal_(ptr, qua, list, &check));
		if (check) {
			*rsize = index;
			return Result(ret, 0);
		}
	}

	return Result(ret, 1);
}

static int qualifiers_position_short_nil_(addr qua, addr comb, size_t *value, int *ret)
{
	addr name, check;

	if (! consp(qua))
		return Result(ret, 1);
	GetCons(qua, &qua, &check);
	if (check != Nil)
		return Result(ret, 1);
	Return(stdget_shortcomb_name_(comb, &name));
	if (qua == name) {
		*value = Clos_short_primary;
		return Result(ret, 0);
	}
	GetConst(KEYWORD_AROUND, &check);
	if (qua == check) {
		*value = Clos_short_around;
		return Result(ret, 0);
	}

	return Result(ret, 1);
}

int qualifiers_position_nil_(Execute ptr, addr qua, addr comb,
		size_t *rsize, int *ret)
{
	int check;

	Check(clos_define_combination_p_debug(comb), "type error");
	if (comb == Nil)
		return Result(ret, qualifiers_position_standard_nil(qua, rsize));
	Return(clos_long_combination_p_(comb, &check));
	if (check)
		return qualifiers_position_long_nil_(ptr, qua, comb, rsize, ret);
	Return(clos_short_combination_p_(comb, &check));
	if (check)
		return qualifiers_position_short_nil_(qua, comb, rsize, ret);
	/* error */
	*rsize = 0;
	*ret = 0;
	return fmte_("Invalid method-combination type ~S.", comb, NULL);
}

int qualifiers_position_(Execute ptr, addr qua, addr comb, size_t *rsize)
{
	int check;

	Return(qualifiers_position_nil_(ptr, qua, comb, rsize, &check));
	if (check) {
		*rsize = 0;
		return fmte_("The qualifiers ~S is not found.", qua, NULL);
	}

	return 0;
}


/*****************************************************************************
 *  standard method-combination
 *****************************************************************************/
static int build_clos_method_combination_standard_(void)
{
	addr clos, inst, name;

	GetConst(CLOS_LONG_METHOD_COMBINATION, &clos);
	Return(clos_instance_heap_(clos, &inst));
	GetConst(COMMON_STANDARD, &name);
	Return(stdset_longcomb_name_(inst, name));
	SetConst(CLOS_COMBINATION_STANDARD, inst);

	return 0;
}

static int build_clos_method_combination_short_(constindex n, int ident)
{
	addr clos, inst, name;

	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &clos);
	Return(clos_instance_heap_(clos, &inst));
	GetConstant(n, &name);
	Return(stdset_shortdef_name_(inst, name));
	Return(stdset_shortdef_document_(inst, Nil));
	Return(stdset_shortdef_identity_(inst, ident? T: Nil));
	Return(stdset_shortdef_operator_(inst, name));
	clos_define_combination(name, inst);

	return 0;
}

static int build_clos_combination_call_(void)
{
	Return(build_clos_method_combination_standard_());
	Return(build_clos_method_combination_short_(CONSTANT_COMMON_PLUS, 1));
	Return(build_clos_method_combination_short_(CONSTANT_COMMON_AND, 1));
	Return(build_clos_method_combination_short_(CONSTANT_COMMON_APPEND, 1));
	Return(build_clos_method_combination_short_(CONSTANT_COMMON_LIST, 0));
	Return(build_clos_method_combination_short_(CONSTANT_COMMON_MAX, 1));
	Return(build_clos_method_combination_short_(CONSTANT_COMMON_MIN, 1));
	Return(build_clos_method_combination_short_(CONSTANT_COMMON_NCONC, 1));
	Return(build_clos_method_combination_short_(CONSTANT_COMMON_OR, 1));
	Return(build_clos_method_combination_short_(CONSTANT_COMMON_PROGN, 1));

	return 0;
}

void build_clos_combination(void)
{
	Error(build_clos_combination_call_());
}


/*
 *  generic-function
 */
static int clos_method_combination_standard_p(addr pos)
{
	addr check;
	GetConst(COMMON_STANDARD, &check);
	return pos == check;
}

static int clos_method_combination_standard_(addr comb, addr list, addr *ret)
{
	if (list != Nil) {
		*ret = Nil;
		return fmte_("Invalid STANDARD method-combination arguments ~S.", list, NULL);
	}

	return Result(ret, Nil);
}

static int clos_method_combination_long_(addr comb, addr list, addr *ret)
{
	addr pos, value;

	GetConst(CLOS_LONG_METHOD_COMBINATION, &pos);
	Return(clos_instance_heap_(pos, &pos));
	/* copy */
	Return(stdget_longdef_name_(comb, &value));
	Return(stdset_longcomb_name_(pos, value));
	Return(stdget_longdef_document_(comb, &value));
	Return(stdset_longcomb_document_(pos, value));
	Return(stdget_longdef_lambda_list_(comb, &value));
	Return(stdset_longcomb_lambda_list_(pos, value));
	Return(stdget_longdef_qualifiers_(comb, &value));
	Return(stdset_longcomb_qualifiers_(pos, value));
	Return(stdget_longdef_arguments_(comb, &value));
	Return(stdset_longcomb_arguments_(pos, value));
	Return(stdget_longdef_generic_(comb, &value));
	Return(stdset_longcomb_generic_(pos, value));
	Return(stdget_longdef_form_(comb, &value));
	Return(stdset_longcomb_form_(pos, value));
	/* binding */
	Return(stdset_longcomb_binding_(pos, list));
	/* result */
	return Result(ret, pos);
}

static int clos_method_combination_short_arguments_(addr list, addr *ret)
{
	addr check, first, last;

	GetConst(KEYWORD_MOST_SPECIFIC_FIRST, &first);
	GetConst(KEYWORD_MOST_SPECIFIC_LAST, &last);
	if (list == Nil)
		return Result(ret, first);
	if (singlep(list)) {
		GetCar(list, &check);
		if (check != first && check != last)
			goto error;
		return Result(ret, check);
	}
	/* error */
error:
	*ret = Nil;
	return fmte_("METHOD-COMBINATION ~S argument must be a "
			":most-specific-first or :most-specific-last.", list, NULL);
}

static int clos_method_combination_short_(addr comb, addr list, addr *ret)
{
	addr pos, value, order;

	/* (&optional argument-precedence-order) */
	Return(clos_method_combination_short_arguments_(list, &order));
	GetConst(CLOS_SHORT_METHOD_COMBINATION, &pos);
	Return(clos_instance_heap_(pos, &pos));
	/* copy */
	Return(stdget_shortdef_name_(comb, &value));
	Return(stdset_shortcomb_name_(pos, value));
	Return(stdget_shortdef_document_(comb, &value));
	Return(stdset_shortcomb_document_(pos, value));
	Return(stdget_shortdef_identity_(comb, &value));
	Return(stdset_shortcomb_identity_(pos, value));
	Return(stdget_shortdef_operator_(comb, &value));
	Return(stdset_shortcomb_operator_(pos, value));
	/* argument-precedence-order */
	Return(stdset_shortcomb_order_(pos, order));
	/* result */
	return Result(ret, pos);
}

static int find_method_combination_(addr symbol, addr list, addr *ret)
{
	int check;
	addr pos;

	Check(! symbolp(symbol), "type error");
	Check(! listp(list), "type error");

	/* standard */
	if (clos_method_combination_standard_p(symbol))
		return clos_method_combination_standard_(symbol, list, ret);

	/* long form */
	Return(clos_find_combination_(symbol, &pos));
	Return(clos_define_long_combination_p_(pos, &check));
	if (check)
		return clos_method_combination_long_(pos, list, ret);

	/* short form */
	Return(clos_define_short_combination_p_(pos, &check));
	if (check)
		return clos_method_combination_short_(pos, list, ret);

	/* error */
	*ret = Nil;
	return fmte_("Invalid method-combination instance ~S, ~S.", pos, list, NULL);
}

int mop_find_method_combination_(addr symbol, addr list, addr *ret)
{
	addr pos;

	Return(find_method_combination_(symbol, list, &pos));
	if (pos == Nil) {
		GetConst(CLOS_COMBINATION_STANDARD, &pos);
	}

	return Result(ret, pos);
}

int clos_find_method_combination_(addr list, addr *ret)
{
	addr pos, tail;

	if (! consp_getcons(list, &pos, &tail)) {
		*ret = Nil;
		return fmte_("Invalid method-combination instance ~S.", list, NULL);
	}

	return find_method_combination_(pos, tail, ret);
}


/*
 *  ensure-define-combination
 */
int ensure_define_combination_short_common_(
		addr name, addr doc, addr ident, addr oper)
{
	addr pos;

	/* instance */
	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &pos);
	Return(clos_instance_heap_(pos, &pos));
	Return(stdset_shortdef_name_(pos, name));
	Return(stdset_shortdef_document_(pos, doc));
	Return(stdset_shortdef_identity_(pos, ident));
	Return(stdset_shortdef_operator_(pos, oper));
	/* define-combination */
	clos_define_combination(name, pos);

	return 0;
}

int ensure_define_combination_long_common_(addr name, addr lambda, addr spec,
		addr args, addr gen, addr doc, addr form, addr decl)
{
	addr pos;

	/* instance */
	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &pos);
	Return(clos_instance_heap_(pos, &pos));
	Return(stdset_longdef_name_(pos, name));
	Return(stdset_longdef_lambda_list_(pos, lambda));
	Return(stdset_longdef_qualifiers_(pos, spec));
	Return(stdset_longdef_arguments_(pos, args));
	Return(stdset_longdef_generic_(pos, gen));
	Return(stdset_longdef_document_(pos, doc));
	Return(stdset_longdef_form_(pos, form));
	/* define-combination */
	clos_define_combination(name, pos);

	return 0;
}


/*
 *  long form
 */
static void comb_longmacro_lambda(addr *ret, addr args,
		addr gen, addr inst, addr array, addr decl, addr form)
{
	/* `(lambda (generic inst array)
	 *    (declare (ignorable generic inst array))
	 *    (destructuring-bind ,args (combination-binding inst)
	 *      ,declarations
	 *      ,form))
	 *
	 * `(lambda (generic inst array)
	 *    (declare (ignorable generic inst array))
	 *    ,declarations
	 *    ,form)
	 */
	addr pos;
	addr lambda, declare, ignorable, dbind, call;

	/* form */
	GetConst(COMMON_DECLARE, &declare);
	cons_heap(&pos, declare, decl);
	list_heap(&form, pos, form, NULL);

	/* destructuring-bind */
	if (args != Nil) {
		GetConst(COMMON_DESTRUCTURING_BIND, &dbind);
		GetConst(CLOSNAME_COMBINATION_BINDING, &call);
		list_heap(&call, call, inst, NULL);
		lista_heap(&form, dbind, args, call, form, NULL);
	}

	/* lambda */
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_IGNORABLE, &ignorable);
	list_heap(&ignorable, ignorable, gen, inst, array, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(&gen, gen, inst, array, NULL);
	if (args != Nil)
		list_heap(ret, lambda, gen, declare, form, NULL);
	else
		lista_heap(ret, lambda, gen, declare, form, NULL);
}

static void comb_longmacro_variables(addr args, addr *ret)
{
	addr root, list, pos;

	Check(ArgumentStruct(args)->type != ArgumentType_combination, "type error");
	root = Nil;
	/* var */
	GetArgument(args, ArgumentIndex_var, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		cons_heap(&root, pos, root);
	}
	/* opt */
	GetArgument(args, ArgumentIndex_opt, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCar(pos, &pos);
		cons_heap(&root, pos, root);
	}
	/* rest */
	GetArgument(args, ArgumentIndex_rest, &pos);
	if (pos != Nil)
		cons_heap(&root, pos, root);
	/* key */
	GetArgument(args, ArgumentIndex_key, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCar(pos, &pos);
		cons_heap(&root, pos, root);
	}
	/* aux */
	GetArgument(args, ArgumentIndex_aux, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCar(pos, &pos);
		cons_heap(&root, pos, root);
	}
	/* whole */
	GetArgument(args, ArgumentIndex_whole, &pos);
	if (pos != Nil)
		cons_heap(&root, pos, root);
	/* result */
	nreverse(ret, root);
}

static void comb_longmacro_arguments(addr *ret, addr args, addr form)
{
	/* `(let ((var1 'var1)
	 *        (var2 'var2)
	 *        ...
	 *        (auxN 'auxN)
	 *        (whole 'whole))
	 *    (declare (ignorable ...))
	 *    ,form)
	 */
	addr root, vars, list, pos, value;
	addr quote, declare, ignorable, let;

	/* no :arguments */
	if (args == Nil) {
		*ret = form;
		return;
	}

	/* args */
	GetConst(COMMON_QUOTE, &quote);
	comb_longmacro_variables(args, &vars);
	root = Nil;
	list = vars;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		list_heap(&value, quote, pos, NULL);
		list_heap(&pos, pos, value, NULL);
		cons_heap(&root, pos, root);
	}
	nreverse(&root, root);
	/* declare */
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, vars);
	list_heap(&declare, declare, ignorable, NULL);
	/* let */
	GetConst(COMMON_LET, &let);
	list_heap(ret, let, root, declare, form, NULL);
}

static int comb_longmacro_form_(addr *ret,
		addr spec, addr gens, addr gen, addr array, addr form)
{
	/* `(let* ((,spec0 (qualifiers-elt 'spec0 ,array 0 order0 required0))
	 *         (,spec1 (qualifiers-elt 'spec1 ,array 1 order1 required1))
	 *         (,gens ,gen))
	 *    (declare (ignorable spec0 ... ,gens))
	 *    ,@form)
	 */
	addr args, vars, pos, temp;
	addr name, order, req, nameq;
	addr elt, quote, declare, ignorable, leta;
	fixnum i;

	args = vars = Nil;
	/* specializers */
	GetConst(CLOSNAME_QUALIFIERS_ELT, &elt);
	GetConst(COMMON_QUOTE, &quote);
	for (i = 0; spec != Nil; i++) {
		Return_getcons(spec, &pos, &spec);
		Return(list_bind_(pos, &name, &temp, &order, &req, &temp, NULL));
		fixnum_heap(&pos, i);
		list_heap(&nameq, quote, name, NULL);
		list_heap(&pos, elt, nameq, array, pos, order, req, NULL);
		list_heap(&pos, name, pos, NULL);
		cons_heap(&vars, name, vars);
		cons_heap(&args, pos, args);
	}
	/* :generic-function */
	if (gens != Unbound) {
		list_heap(&pos, gens, gen, NULL);
		cons_heap(&args, pos, args);
		cons_heap(&vars, gens, vars);
	}
	/* ignorable */
	GetConst(COMMON_IGNORABLE, &ignorable);
	nreverse(&vars, vars);
	cons_heap(&vars, ignorable, vars);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, vars, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	nreverse(&args, args);
	lista_heap(ret, leta, args, declare, form, NULL);

	return 0;
}

int comb_longmacro_(addr *ret,
		addr lambda, addr spec, addr args, addr gens, addr decl, addr form)
{
	addr gen, inst, array;

	make_symbolchar(&gen, "GENERIC");
	make_symbolchar(&inst, "COMBINATION");
	make_symbolchar(&array, "QUALIFIERS");

	Return(comb_longmacro_form_(&form, spec, gens, gen, array, form));
	comb_longmacro_arguments(&form, args, form);
	comb_longmacro_lambda(ret, lambda, gen, inst, array, decl, form);

	return 0;
}

static void comb_longform_macrolet(addr *ret, addr args, addr gen, addr form)
{
	/*  `(lambda (&rest ,args)
	 *     (declare (ignorable ,args))
	 *     (macrolet ((make-method (#:expr)
	 *                  (macro-make-method ,gen #:expr))
	 *                (call-method (#:car &optional #:cdr)
	 *                  (macro-call-method #:car #:cdr (quote ,args))))
	 *       ,form))
	 */
	addr lambda, declare, ignorable, macrolet, quote, rest, optional;
	addr make, call, mmake, mcall;
	addr expr, car, cdr;

	/* constant */
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MACROLET, &macrolet);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_MAKE_METHOD, &make);
	GetConst(COMMON_CALL_METHOD, &call);
	GetConst(AMPERSAND_REST, &rest);
	GetConst(AMPERSAND_OPTIONAL, &optional);
	GetConst(CLOSNAME_MACRO_MAKE_METHOD, &mmake);
	GetConst(CLOSNAME_MACRO_CALL_METHOD, &mcall);
	make_symbolchar(&expr, "EXPR");
	make_symbolchar(&car, "CAR");
	make_symbolchar(&cdr, "CDR");
	/* macro */
	list_heap(&quote, quote, args, NULL);
	list_heap(&mcall, mcall, car, cdr, quote, NULL);
	list_heap(&car, car, optional, cdr, NULL);
	list_heap(&call, call, car, mcall, NULL);
	list_heap(&mmake, mmake, gen, expr, NULL);
	list_heap(&expr, expr, NULL);
	list_heap(&make, make, expr, mmake, NULL);
	list_heap(&make, make, call, NULL);
	list_heap(&macrolet, macrolet, make, form, NULL);
	list_heap(&ignorable, ignorable, args, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(&rest, rest, args, NULL);
	list_heap(ret, lambda, rest, declare, macrolet, NULL);
}

static int comb_longmacro_lambda_list_(addr args, addr *ret)
{
	addr root, var, list, a, b, c, d;
	struct argument_struct *str;


	str = ArgumentStruct(args);
	Check(str->type != ArgumentType_combination, "type error");

	root = Nil;
	/* whole */
	if (str->whole) {
		GetConst(AMPERSAND_WHOLE, &var);
		cons_heap(&root, var, root);
		GetArgument(args, ArgumentIndex_whole, &var);
		cons_heap(&root, var, root);
	}

	/* var & opt */
	if (str->var || str->opt) {
		GetConst(AMPERSAND_OPTIONAL, &var);
		cons_heap(&root, var, root);
	}

	/* var */
	GetArgument(args, ArgumentIndex_var, &list);
	while (list != Nil) {
		GetCons(list, &var, &list);
		cons_heap(&root, var, root);
	}

	/* opt */
	GetArgument(args, ArgumentIndex_opt, &list);
	while (list != Nil) {
		GetCons(list, &var, &list);
		Return(list_bind_(var, &a, &b, &c, NULL));
		if (c == Nil)
			list_heap(&var, a, b, NULL);
		cons_heap(&root, var, root);
	}

	/* rest */
	if (str->rest) {
		GetConst(AMPERSAND_REST, &var);
		cons_heap(&root, var, root);
		GetArgument(args, ArgumentIndex_rest, &var);
		cons_heap(&root, var, root);
	}

	/* key */
	if (str->keyp || str->key) {
		GetConst(AMPERSAND_KEY, &var);
		cons_heap(&root, var, root);
	}
	GetArgument(args, ArgumentIndex_key, &list);
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

	/* key */
	if (str->allow) {
		GetConst(AMPERSAND_ALLOW, &var);
		cons_heap(&root, var, root);
	}

	/* result */
	nreverse(ret, root);

	return 0;
}

static int comb_longform_arguments_(addr *ret, addr args, addr comb, addr form)
{
	addr list, lambda;
	addr dbind, declare, ignorable;

	/* no :arguments */
	Return(stdget_longcomb_arguments_(comb, &comb));
	if (comb == Nil)
		return Result(ret, form);

	/* (destructuring-bind ,arguments ,args
	 *   (declare (ignorable ...))
	 *   ,form)
	 */
	comb_longmacro_variables(comb, &list);
	Return(comb_longmacro_lambda_list_(comb, &lambda));
	/* declare */
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, list);
	list_heap(&declare, declare, ignorable, NULL);
	/* result */
	GetConst(COMMON_DESTRUCTURING_BIND, &dbind);
	list_heap(ret, dbind, lambda, args, declare, form, NULL);

	return 0;
}

static int comb_longform_call_(Execute ptr, LocalHold hold,
		addr gen, addr comb, addr data, addr *ret)
{
	addr pos;

	Return(stdget_longcomb_form_(comb, &pos));
	Return(funcall_control_(ptr, pos, gen, comb, data, NULL));
	getresult_control(ptr, &pos);
	localhold_set(hold, 0, pos);

	return Result(ret, pos);
}

static int comb_longform_push_(Execute ptr, addr gen, addr comb, addr data, addr *ret)
{
	addr control, pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	pos = Nil;
	(void)comb_longform_call_(ptr, hold, gen, comb, data, &pos);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return Result(ret, pos);
}

int comb_longform_(Execute ptr, addr *ret, addr gen, addr comb, addr data)
{
	addr pos, args;

	/* execute */
	Return(comb_longform_push_(ptr, gen, comb, data, &pos));

	/* make-form */
	make_symbolchar(&args, "ARGS");
	Return(comb_longform_arguments_(&pos, args, comb, pos));
	comb_longform_macrolet(&pos, args, gen, pos);

	/* eval */
	return eval_result_partial_(ptr, pos, ret);
}

static int comb_shortform_primary_(addr *ret, addr comb, addr list)
{
	addr check, call, root;

	GetConst(COMMON_CALL_METHOD, &call);
	Return(stdget_shortcomb_identity_(comb, &check));
	if (check != Nil && singlep(list)) {
		GetCar(list, &list);
		list_heap(ret, call, list, NULL);
	}
	else {
		Return(stdget_shortcomb_operator_(comb, &check));
		conscar_heap(&root, check);
		while (list != Nil) {
			Return_getcons(list, &check, &list);
			list_heap(&check, call, check, NULL);
			cons_heap(&root, check, root);
		}
		nreverse(ret, root);
	}

	return 0;
}

static int comb_shortform_around_(addr *ret, addr comb, addr list, addr form)
{
	addr pos, root, car, cdr;

	if (list == Nil)
		return Result(ret, form);

	Return_getcons(list, &car, &cdr);
	/* ,@(cdr around) */
	for (root = Nil; cdr != Nil; ) {
		Return_getcons(cdr, &pos, &cdr);
		cons_heap(&root, pos, root);
	}
	/* (make-method ,form) */
	GetConst(COMMON_MAKE_METHOD, &pos);
	list_heap(&pos, pos, form, NULL);
	cons_heap(&root, pos, root);
	nreverse(&root, root);
	/* call-methd */
	GetConst(COMMON_CALL_METHOD, &pos);
	list_heap(ret, pos, car, root, NULL);

	return 0;
}

static int comb_shortform_make_(addr *ret, addr comb, addr data)
{
	addr around, primary, order, check, form;

	Check(lenarrayr(data) != Clos_short_size, "size error");
	/* method */
	getarray(data, Clos_short_around, &around);
	getarray(data, Clos_short_primary, &primary);
	/* required */
	if (primary == Nil) {
		Return(stdget_shortcomb_name_(comb, &primary));
		*ret = Nil;
		return fmte_("The qualifier ~S must be at least one method.", primary, NULL);
	}
	/* order */
	Return(stdget_shortcomb_order_(comb, &order));
	GetConst(KEYWORD_MOST_SPECIFIC_LAST, &check);
	if (order == check) {
		Return(reverse_list_heap_safe_(&primary, primary));
	}
	/* form */
	Return(comb_shortform_primary_(&form, comb, primary));
	return comb_shortform_around_(ret, comb, around, form);
}

int comb_shortform_(Execute ptr, addr *ret, addr gen, addr comb, addr data)
{
	addr pos, args;

	/* make-form */
	make_symbolchar(&args, "ARGS");
	Return(comb_shortform_make_(&pos, comb, data));
	comb_longform_macrolet(&pos, args, gen, pos);

	/* eval */
	return eval_result_partial_(ptr, pos, ret);
}


/************************************************************
 *  clos_defgeneric.c
 ************************************************************/

struct generic_argument {
	unsigned lambda_p : 1;
	unsigned generic_p : 1;
	unsigned method_p : 1;
	unsigned combination_p : 1;
	unsigned order_p : 1;
	unsigned declare_p : 1;
	unsigned doc_p : 1;
	unsigned redefined : 1;
	Execute ptr;
	addr env, instance, callname, name, lambda, args;
	addr generic, method, combination, order, declare, doc;
};


/*
 *  defgeneric
 */
void generic_cache_heap(addr *ret)
{
	hashtable_full_heap(ret,
			HASHTABLE_TEST_CACHE,
			8,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

static int generic_make_methods_(addr *ret, addr comb)
{
	size_t size;

	Return(method_combination_qualifiers_count_(comb, &size));
	vector4_heap(ret, size);

	return 0;
}

static int generic_make_instance_(addr *ret, addr call, addr args, int finalp)
{
	addr name, lambda, pos, vector, method, cache;

	CheckType(call, LISPTYPE_CALLNAME);
	CheckType(args, LISPSYSTEM_ARGUMENT);
	Check(ArgumentStruct(args)->type != ArgumentType_generic, "argument error");

	/* object */
	name_callname_heap(call, &name);
	Return(argument_generic_lambda_heap_(&lambda, args));

	/* methods */
	Return(generic_make_methods_(&vector, Nil));
	GetConst(CLOS_STANDARD_METHOD, &method);
	generic_cache_heap(&cache);

	/* make-instance */
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	Return(clos_instance_heap_(pos, &pos));

	/* setf */
	Return(stdset_generic_name_(pos, name));
	Return(stdset_generic_methods_(pos, Nil));
	Return(stdset_generic_lambda_list_(pos, lambda));
	Return(stdset_generic_argument_precedence_order_(pos, Nil));
	Return(stdset_generic_method_class_(pos, method));
	Return(stdset_generic_method_combination_(pos, Nil));
	Return(stdset_generic_vector_(pos, vector));
	Return(stdset_generic_remove_(pos, Nil));
	Return(stdset_generic_argument_(pos, args));
	Return(stdset_generic_cache_(pos, cache));
	Return(stdset_generic_precedence_index_(pos, Nil));

	/* result */
	if (finalp) {
		Return(generic_finalize_(pos));
	}
	Return(setglobal_parse_callname_(name, pos));
	return Result(ret, pos);
}

int generic_make_(addr *ret, addr call, addr args)
{
#ifdef LISP_DEBUG
	addr pos;

	clos_find_generic_nil(call, &pos);
	Check(pos != Nil, "generic function error");
#endif
	CheckType(call, LISPTYPE_CALLNAME);
	return generic_make_instance_(ret, call, args, 0);
}

int generic_make_empty_(addr call, addr args, addr *ret)
{
	return generic_make_instance_(ret, call, args, 1);
}


/*
 *  generic-new
 */
static int generic_new_order_(addr args, addr order, addr *ret)
{
	addr var, x, list;
	size_t size1, size2, index;

	if (order == Nil)
		return Result(ret, Nil);
	GetArgument(args, ArgumentIndex_var, &var);
	Return(length_list_safe_(var, &size1));
	Return(length_list_safe_(order, &size2));
	if (size1 != size2)
		goto error1;
	for (list = Nil; var != Nil; ) {
		GetCons(var, &x, &var);
		if (! position_list_eq_unsafe(x, order, &index))
			goto error2;
		index_heap(&x, index);
		cons_heap(&list, x, list);
	}
	nreverse(ret, list);
	return 0;

error1:
	*ret = Nil;
	return call_simple_program_error_va_(NULL, "Length of "
			":argument-precedence-order is not equal to lambda-list.", NULL);

error2:
	*ret = Nil;
	return call_simple_program_error_va_(NULL, "The variable ~S is "
			"not exist in :argument-precedence-order", x, NULL);
}

static int generic_find_method_combination_(struct generic_argument *str, addr *ret)
{
	int check;
	addr pos, standard;

	/* standard */
	pos = str->combination;
	if (pos == Nil)
		return Result(ret, Nil);

	/* standard object */
	GetConst(CLOS_COMBINATION_STANDARD, &standard);
	if (pos == standard)
		return Result(ret, Nil);

	/* ensure-generic-function */
	Return(clos_combination_p_(pos, &check));
	if (check)
		return Result(ret, pos);

	/* defgeneric */
	return clos_find_method_combination_(pos, ret);
}

static int generic_new_(struct generic_argument *str, addr *ret)
{
	addr pos, comb, vector, cache, order;

	/* check */
	Return(generic_new_order_(str->args, str->order, &order));

	/* (make-instance generic-function-class) */
	GetConst(COMMON_MAKE_INSTANCE, &pos);
	Return(funcall1_control_(str->ptr, &pos, pos, str->generic, NULL));

	/* value */
	Return(generic_find_method_combination_(str, &comb));
	Return(generic_make_methods_(&vector, comb));
	generic_cache_heap(&cache);

	/* setf */
	Return(stdset_generic_name_(pos, str->name));
	Return(stdset_generic_methods_(pos, Nil));
	Return(stdset_generic_lambda_list_(pos, str->lambda));
	Return(stdset_generic_argument_precedence_order_(pos, str->order));
	Return(stdset_generic_declarations_(pos, str->declare));
	Return(stdset_generic_method_class_(pos, str->method));
	Return(stdset_generic_method_combination_(pos, comb));
	Return(stdset_generic_vector_(pos, vector));
	Return(stdset_generic_argument_(pos, str->args));
	Return(stdset_generic_documentation_(pos, str->doc));
	Return(stdset_generic_cache_(pos, cache));
	Return(stdset_generic_precedence_index_(pos, order));

	/* result */
	Return(generic_finalize_(pos));
	Return(setglobal_parse_callname_(str->name, pos));
	return Result(ret, pos);
}


/*
 *  generic-change
 */
static int generic_change_remove_(struct generic_argument *str)
{
	int ignore;
	addr gen, method, list;
	Execute ptr;

	ptr = str->ptr;
	gen = str->instance;
	if (str->redefined) {
		Return(stdget_generic_remove_(gen, &list));
		while (list != Nil) {
			GetCons(list, &method, &list);
			Return(method_remove_method_unsafe_(ptr, gen, method, &ignore));
		}
	}
	Return(stdset_generic_remove_(gen, Nil));

	return 0;
}

static int generic_change_equal_(struct generic_argument *str, int *ret)
{
	addr gen1, gen2;

	gen1 = str->generic;
	Return(clos_class_of_(str->instance, &gen2));

	return Result(ret, (gen1 == gen2));
}

static int generic_change_lambda_list_(struct generic_argument *str)
{
	addr pos, lambda, args;

	if (str->lambda_p) {
		pos = str->instance;
		lambda = str->lambda;
		Return(argument_generic_heap_(str->ptr->local, &args, lambda));
		Return(stdset_generic_lambda_list_(pos, lambda));
		Return(stdset_generic_argument_(pos, args));
		str->args = args;
	}

	return 0;
}

static int generic_change_order_(struct generic_argument *str)
{
	addr pos, order, index;

	/* &key */
	pos = str->instance;
	if (str->order_p) {
		order = str->order;
	}
	else {
		Return(stdget_generic_argument_precedence_order_(pos, &order));
	}

	/* set */
	Return(generic_new_order_(str->args, order, &index));
	Return(stdset_generic_argument_precedence_order_(pos, order));
	Return(stdset_generic_precedence_index_(pos, index));

	return 0;
}

static int generic_change_change_class_(struct generic_argument *str)
{
	int check;

	Return(generic_change_equal_(str, &check));
	if (! check) {
		Return(clos_change_class_(str->ptr, str->instance, str->generic, Nil));
	}

	return 0;
}

static int generic_change_combination_(struct generic_argument *str)
{
	addr pos, value;

	if (str->combination_p) {
		pos = str->instance;
		Return(generic_find_method_combination_(str, &value));
		Return(stdset_generic_method_combination_(pos, value));
	}

	return 0;
}

static int generic_change_method_(struct generic_argument *str)
{
	addr gen, vector, list, method;
	Execute ptr;

	/* vector */
	ptr = str->ptr;
	gen = str->instance;
	Return(stdget_generic_method_combination_(gen, &vector));
	Return(generic_make_methods_(&vector, vector));
	Return(stdset_generic_vector_(gen, vector));

	/* methods */
	Return(stdget_generic_methods_(gen, &list));
	Return(stdset_generic_methods_(gen, Nil));

	/* add method */
	while (list != Nil) {
		GetCons(list, &method, &list);
		Return(method_add_method_(ptr, gen, method));
	}

	return 0;
}

static int generic_change_execute_(struct generic_argument *str)
{
	addr pos, cache;

	Return(generic_change_lambda_list_(str));
	Return(generic_change_order_(str));
	Return(generic_change_change_class_(str));
	Return(generic_change_combination_(str));
	Return(generic_change_method_(str));
	generic_cache_heap(&cache);

	/* setf */
	pos = str->instance;
	Return(stdset_generic_declarations_(pos, str->declare));
	Return(stdset_generic_method_class_(pos, str->method));
	Return(stdset_generic_documentation_(pos, str->doc));
	Return(stdset_generic_cache_(pos, cache));

	/* result */
	return generic_finalize_(pos);
}

static void generic_change_copy_vector(addr methods, addr *ret)
{
	addr pos, list;
	size_t size, i;

	LenArrayA4(methods, &size);
	vector4_heap(&pos, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(methods, i, &list);
		copy_list_heap_unsafe(&list, list);
		SetArrayA4(pos, i, list);
	}
	*ret = pos;
}

static int generic_change_copy_(addr gen, addr *ret)
{
	addr pos, value;

	clos_allcopy_alloc(NULL, gen, &pos);

	/* methods */
	Return(stdget_generic_methods_(pos, &value));
	copy_list_heap_unsafe(&value, value);
	Return(stdset_generic_methods_(pos, value));

	/* vector */
	Return(stdget_generic_vector_(pos, &value));
	generic_change_copy_vector(value, &value);
	Return(stdset_generic_vector_(pos, value));

	return Result(ret, pos);
}

static int generic_change_call_(struct generic_argument *str)
{
	int check;
	addr pos, save, list;
	LocalHold hold;

	/* save */
	pos = str->instance;
	hold = LocalHold_array(str->ptr, 4);
	localhold_set(hold, 0, str->callname);
	localhold_set(hold, 1, str->args);

	/* instance */
	Return(generic_change_copy_(pos, &save));
	localhold_set(hold, 2, save);

	/* method */
	Return(generic_change_remove_(str));
	Return(stdget_generic_methods_(pos, &list));
	localhold_set(hold, 3, list);

	/* execute */
	check = generic_change_execute_(str);
	if (check)
		clos_swap(pos, save);
	localhold_end(hold);

	return check;
}

static int generic_change_(struct generic_argument *str)
{
	Execute ptr;
	addr control;

	ptr = str->ptr;
	push_control(ptr, &control);
	(void)generic_change_call_(str);
	return pop_control_(ptr, control);
}


/*
 *  ensure-generic-function
 */
int ensure_generic_function_name_(addr name, addr *ret)
{
	addr call, value, expected;

	*ret = Nil;
	Return(parse_callname_error_(&call, name));
	if (! symbolp_callname(call))
		return Result(ret, call);

	/* macro */
	getmacro_symbol(name, &value);
	if (value != Unbound) {
		GetConst(COMMON_GENERIC_FUNCTION, &expected);
		return call_type_error_va_(NULL, name, expected,
				"ENSURE-GENERIC-FUNCTION don't accept "
				"a macro symbol ~S.", name, NULL);
	}

	/* special-operator */
	if (get_special_operator(name)) {
		GetConst(COMMON_GENERIC_FUNCTION, &expected);
		return call_type_error_va_(NULL, name, expected,
				"ENSURE-GENERIC-FUNCTION don't accept "
				"a special symbol ~S.", name, NULL);
	}

	return Result(ret, call);
}

static int ensure_generic_function_call_(Execute ptr,
		addr clos, addr name, addr rest, addr *ret)
{
	addr call;

	/* (apply #'ensure-generic-function-using-class clos name rest) */
	GetConst(CLOSNAME_ENSURE_GENERIC_FUNCTION_USING_CLASS, &call);
	Return(getfunction_global_(call, &call));
	Return(applya_control_(ptr, call, clos, name, rest, NULL));
	getresult_control(ptr, ret);

	return 0;
}

int ensure_generic_function_common_(Execute ptr, addr name, addr rest, addr *ret)
{
	int check;
	addr call, value, expected;

	/* symbol or (setf name) */
	Return(ensure_generic_function_name_(name, &call));

	/* class-of */
	getglobal_parse_callname(call, &value);
	if (value == Unbound)
		return ensure_generic_function_call_(ptr, Nil, name, rest, ret);
	Return(clos_generic_p_(value, &check));
	if (check)
		return ensure_generic_function_call_(ptr, value, name, rest, ret);

	/* error */
	*ret = Nil;
	GetConst(COMMON_GENERIC_FUNCTION, &expected);
	return call_type_error_va_(ptr, value, expected,
			"Invalid generic-function argument ~S.", name, NULL);
}


/*
 *  ensure-generic-function-using-class
 */
static int mop_generic_struct_(
		struct generic_argument *str,
		Execute ptr, addr instance, addr name, addr rest)
{
	int lambda_p, generic_p, method_p, combination_p, order_p, declare_p, doc_p;
	addr call, order, decl, doc, env, gen, args, lambda, method, comb, redefined;

	/* arguments */
	lambda_p = generic_p = method_p = combination_p = 1;
	order_p = declare_p = doc_p = 1;
	if (GetKeyArgs(rest, KEYWORD_ARGUMENT_PRECEDENCE_ORDER, &order)) {
		order_p = 0;
		order = Nil;
	}
	if (GetKeyArgs(rest, KEYWORD_DECLARE, &decl)) {
		declare_p = 0;
		decl = Nil;
	}
	if (GetKeyArgs(rest, KEYWORD_DOCUMENTATION, &doc)) {
		doc_p = 0;
		doc = Nil;
	}
	if (GetKeyArgs(rest, KEYWORD_LAMBDA_LIST, &lambda)) {
		lambda_p = 0;
		lambda = Nil;
	}
	if (GetKeyArgs(rest, KEYWORD_METHOD_COMBINATION, &comb)) {
		combination_p = 0;
		comb = Nil;
	}
	if (GetKeyArgs(rest, KEYWORD_GENERIC_FUNCTION_CLASS, &gen)) {
		generic_p = 0;
		GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &gen);
	}
	if (GetKeyArgs(rest, KEYWORD_METHOD_CLASS, &method)) {
		method_p = 0;
		GetConst(CLOS_STANDARD_METHOD, &method);
	}
	if (GetKeyArgs(rest, KEYWORD_ENVIRONMENT, &env))
		env = Nil;
	if (GetKeyArgs(rest, CLOSNAME_REDEFINED, &redefined))
		redefined = Nil;

	/* name */
	Check(callnamep(name), "type error");
	Return(parse_callname_error_(&call, name));

	/* lambda-list */
	Check(argumentp(lambda), "type error");
	Return(argument_generic_heap_(ptr->local, &args, lambda));

	/* value */
	str->ptr = ptr;
	str->env = env;
	str->instance = instance;
	str->callname = call;
	str->name = name;
	str->args = args;
	str->lambda = lambda;
	str->generic = gen;
	str->method = method;
	str->combination = comb;
	str->order = order;
	str->declare = decl;
	str->doc = doc;
	str->redefined = (redefined != Nil)? 1: 0;

	str->lambda_p = (lambda_p? 1: 0);
	str->generic_p = (generic_p? 1: 0);
	str->method_p = (method_p? 1: 0);
	str->combination_p = (combination_p? 1: 0);
	str->order_p = (order_p? 1: 0);
	str->declare_p = (declare_p? 1: 0);
	str->doc_p = (doc_p? 1: 0);

	return 0;
}

int mop_generic_new_(Execute ptr, addr name, addr rest, addr *ret)
{
	struct generic_argument str;
	Return(mop_generic_struct_(&str, ptr, Nil, name, rest));
	return generic_new_(&str, ret);
}

int mop_generic_change_(Execute ptr, addr clos, addr name, addr rest)
{
	struct generic_argument str;
	Return(mop_generic_struct_(&str, ptr, clos, name, rest));
	return generic_change_(&str);
}


/*
 *  syscall
 */
int system_generic_define_(Execute ptr, addr name, addr args, addr *ret)
{
	addr key;
	LocalRoot local;
	LocalStack stack;

	/* `(redefined t ,@args) */
	local = ptr->local;
	push_local(local, &stack);
	GetConst(CLOSNAME_REDEFINED, &key);
	cons_local(local, &args, T, args);
	cons_local(local, &args, key, args);

	/* call */
	Return(ensure_generic_function_common_(ptr, name, args, ret));

	/* rollback */
	rollback_local(local, stack);
	return 0;
}

int system_generic_method_(addr gen, addr args)
{
	addr list, pos;

	Return(stdget_generic_remove_(gen, &list));
	while (args != Nil) {
		GetCons(args, &pos, &args);
		cons_heap(&list, pos, list);
	}
	Return(stdset_generic_remove_(gen, list));

	return 0;
}


/************************************************************
 *  clos_generic.c
 ************************************************************/

/* clos_generic_call */
enum ClosGenericIndex {
#ifdef LISP_DEGRADE
	gen_debug,
#endif
	gen_comb_standard_call,
	gen_comb_define_call,
	gen_generic_make_lambda_call,
	gen_generic_no_method,
	gen_size
};
typedef int (*clos_generic_call)(Execute, addr, addr, addr);
static clos_generic_call ClosGenericTable[gen_size];
#define CallClosGenericCall(x,y) \
	(*(y) = ClosGenericTable[*(enum ClosGenericIndex *)PtrBodySS(x)])
#define GetClosGenericCall(x,y)  (*(y) = *(enum ClosGenericIndex *)PtrBodySS(x))
#define SetClosGenericCall(x,y)  (*(enum ClosGenericIndex *)PtrBodySS(x) = (y))
#define GetClosGenericCallArray(x,i,y)  GetArraySS(x,i,y)
#define SetClosGenericCallArray(x,i,y)  SetArraySS(x,i,y)

/*
 *  standard-generic-function
 */
static int stdget_generic_constant_(addr pos, addr *ret,
		enum Clos_generic_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_generic_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_getelt(pos, (size_t)index1, &check);
	}
	else {
		GetConstant(index2, &check);
		Return(clos_get_(pos, check, &check));
	}
	/* Unbound check */
	if (check == Unbound) {
		*ret = Nil;
		return fmte_("There is no applicable methods in ~S.", pos, NULL);
	}

	return Result(ret, check);
}

static int stdset_generic_constant_(addr pos, addr value,
		enum Clos_generic_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_generic_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &check);
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
#define StdGetGeneric_(p,r,a,b) \
	stdget_generic_constant_((p), (r), Clos_generic_##a, CONSTANT_CLOSNAME_##b)
#define StdSetGeneric_(p,r,a,b) \
	stdset_generic_constant_((p), (r), Clos_generic_##a, CONSTANT_CLOSNAME_##b)

int stdget_generic_name_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, name, NAME);
}
int stdset_generic_name_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, name, NAME);
}

int stdget_generic_methods_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, methods, METHODS);
}
int stdset_generic_methods_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, methods, METHODS);
}

int stdget_generic_lambda_list_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, lambda_list, LAMBDA_LIST);
}
int stdset_generic_lambda_list_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, lambda_list, LAMBDA_LIST);
}

int stdget_generic_argument_precedence_order_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, argument_precedence_order, ARGUMENT_PRECEDENCE_ORDER);
}
int stdset_generic_argument_precedence_order_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, argument_precedence_order, ARGUMENT_PRECEDENCE_ORDER);
}

int stdget_generic_declarations_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, declarations, DECLARATIONS);
}
int stdset_generic_declarations_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, declarations, DECLARATIONS);
}

int stdget_generic_method_class_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, method_class, METHOD_CLASS);
}
int stdset_generic_method_class_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, method_class, METHOD_CLASS);
}

int stdget_generic_method_combination_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, method_combination, METHOD_COMBINATION);
}
int stdset_generic_method_combination_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, method_combination, METHOD_COMBINATION);
}

int stdget_generic_vector_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, vector, VECTOR);
}
int stdset_generic_vector_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, vector, VECTOR);
}

int stdget_generic_remove_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, remove, REMOVE);
}
int stdset_generic_remove_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, remove, REMOVE);
}

int stdget_generic_argument_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, argument, ARGUMENT);
}
int stdset_generic_argument_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, argument, ARGUMENT);
}

int stdget_generic_documentation_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, documentation, DOCUMENTATION);
}
int stdset_generic_documentation_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, documentation, DOCUMENTATION);
}

int stdget_generic_eqlcheck_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, eqlcheck, EQLCHECK);
}
int stdset_generic_eqlcheck_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, eqlcheck, EQLCHECK);
}

int stdget_generic_cache_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, cache, CACHE);
}
int stdset_generic_cache_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, cache, CACHE);
}

int stdget_generic_call_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, call, CALL);
}
int stdset_generic_call_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, call, CALL);
}

int stdget_generic_precedence_index_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, precedence_index, PRECEDENCE_INDEX);
}
int stdset_generic_precedence_index_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, precedence_index, PRECEDENCE_INDEX);
}

static int stdboundp_generic_constant_(addr pos, int *ret,
		enum Clos_generic_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_generic_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &check);
	if (clos == check) {
		clos_getelt(pos, (size_t)index1, &pos);
		return Result(ret, pos != Unbound);
	}
	else {
		GetConstant(index2, &check);
		return clos_slot_boundp_(pos, check, ret);
	}
}

int stdboundp_generic_argument_precedence_order_(addr pos, int *ret)
{
	return stdboundp_generic_constant_(pos, ret,
			Clos_generic_argument_precedence_order,
			CONSTANT_CLOSKEY_ARGUMENT_PRECEDENCE_ORDER);
}

int stdboundp_generic_eqlcheck_(addr pos, int *ret)
{
	return stdboundp_generic_constant_(pos, ret,
			Clos_generic_eqlcheck,
			CONSTANT_CLOSKEY_EQLCHECK);
}


/*
 *  eql-specializer
 */
static int stdget_specializer_constant_(addr pos, addr *ret,
		enum Clos_specializer_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_specializer_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_EQL_SPECIALIZER, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}

static int stdset_specializer_constant_(addr pos, addr value,
		enum Clos_specializer_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_specializer_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_EQL_SPECIALIZER, &check);
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
#define StdGetSpecializer_(p,r,a,b) \
	stdget_specializer_constant_((p), (r), Clos_specializer_##a, CONSTANT_CLOSNAME_##b)
#define StdSetSpecializer_(p,r,a,b) \
	stdset_specializer_constant_((p), (r), Clos_specializer_##a, CONSTANT_CLOSNAME_##b)

int stdget_specializer_object_(addr pos, addr *ret)
{
	return StdGetSpecializer_(pos, ret, object, OBJECT);
}
int stdset_specializer_object_(addr pos, addr value)
{
	return StdSetSpecializer_(pos, value, object, OBJECT);
}

int stdget_specializer_type_(addr pos, addr *ret)
{
	return StdGetSpecializer_(pos, ret, type, TYPE);
}
int stdset_specializer_type_(addr pos, addr value)
{
	return StdSetSpecializer_(pos, value, type, TYPE);
}


/*****************************************************************************
 *  clos-generic-call
 *****************************************************************************/
static void clos_generic_call_heap(addr *ret, enum ClosGenericIndex call, int size)
{
	addr pos;

	Check(size < 0, "size error");
	Check(255 < size, "size error");
	heap_smallsize(&pos, LISPSYSTEM_GENERIC, size, sizeoft(enum ClosGenericIndex));
	SetClosGenericCall(pos, call);
	*ret = pos;
}


/*****************************************************************************
 *  default method-combination
 *****************************************************************************/
/*
 *  standard
 */
static int comb_standard_method_(Execute ptr, addr car, addr cdr, addr rest)
{
	addr call;
	Return(stdget_method_function_(car, &call));
	return applya_control_(ptr, call, car, cdr, rest, NULL);
}

static int comb_standard_funcall_(Execute ptr, addr rest, addr around, addr primary)
{
	append2_heap_unsafe(around, primary, &around);
	GetCons(around, &around, &primary);
	return comb_standard_method_(ptr, around, primary, rest);
}

static int function_standard_lambda_after_(Execute ptr, addr after, addr args)
{
	addr control, values, one;
	size_t size;

	push_control(ptr, &control);
	save_values_control(ptr, &values, &size);
	while (after != Nil) {
		GetCons(after, &one, &after);
		if (comb_standard_method_(ptr, one, Nil, args))
			goto finish;
	}
	restore_values_control(ptr, values, size);
finish:
	return pop_control_(ptr, control);
}

static int function_standard_lambda(Execute ptr)
{
	addr args, data, before, primary, after, one, car, cdr;

	/*
	 *  (lambda (method next &rest args)
	 *    (declare (ignore method next))
	 *    ...)
	 */
	getargs_list_control_unsafe(ptr, 2, &args);

	/* closure */
	getdata_control(ptr, &data);
	GetCons(data, &before, &data);
	GetCons(data, &primary, &data);
	GetCar(data, &after);

	/* before */
	while (before != Nil) {
		GetCons(before, &one, &before);
		Return(comb_standard_method_(ptr, one, Nil, args));
	}

	/* primary */
	setvalues_nil_control(ptr);
	GetCons(primary, &car, &cdr);
	Return(comb_standard_method_(ptr, car, cdr, args));

	/* after */
	if (after != Nil) {
		Return(function_standard_lambda_after_(ptr, after, args));
	}

	return 0;
}

static int comb_standard_qualifiers_(LocalRoot local,
		addr *ret, addr gen, addr before, addr primary, addr after)
{
	addr clos, call, data, name;

	Return(stdget_generic_method_class_(gen, &clos));
	GetConst(SYSTEM_STANDARD, &name);
	ParseCallName(local, &name, name);
	compiled_local(local, &call, name);
	setcompiled_any(call, p_defun_standard_lambda);
	list_local(local, &data, before, primary, after, NULL);
	SetDataFunction(call, data);
	return method_instance_call_(local, ret, clos, call);
}

static int generic_no_applicable_method_(Execute ptr, addr gen, addr args)
{
	addr call;

	/* call (no-applicable-method generic-function . args) */
	GetConst(COMMON_NO_APPLICABLE_METHOD, &call);
	Return(getfunction_global_(call, &call));
	return applya_control_(ptr, call, gen, args, NULL);
}

static int comb_standard_execute_(Execute ptr, addr inst, addr gen, addr rest)
{
	addr data, around, before, primary, after, temp;
	LocalRoot local;

	GetClosGenericCallArray(inst, 0, &data);
	GetArrayA4(data, Clos_standard_around, &around);
	GetArrayA4(data, Clos_standard_before, &before);
	GetArrayA4(data, Clos_standard_primary, &primary);
	GetArrayA4(data, Clos_standard_after, &after);

	if (primary == Nil)
		return generic_no_applicable_method_(ptr, gen, rest);
	local = ptr->local;
	GetCdr(primary, &temp);
	if (before != Nil || after != Nil || temp != Nil) {
		Return(comb_standard_qualifiers_(local, &temp, gen, before, primary, after));
		list_local(local, &primary, temp, NULL);
	}
	return comb_standard_funcall_(ptr, rest, around, primary);
}

static int comb_standard_call_(Execute ptr, addr inst, addr gen, addr rest)
{
	addr control;

	push_control(ptr, &control);
	(void)comb_standard_execute_(ptr, inst, gen, rest);
	return pop_control_(ptr, control);
}

static int comb_standard_(addr *ret, addr data)
{
	addr pos;

	clos_generic_call_heap(&pos, gen_comb_standard_call, 1);
	SetClosGenericCallArray(pos, 0, data);

	return Result(ret, pos);
}


/*
 *  execute combination
 */
static int comb_define_call_(Execute ptr, addr inst, addr gen, addr rest)
{
	addr control, call;

	push_control(ptr, &control);
	GetClosGenericCallArray(inst, 0, &call);
	(void)apply_control_(ptr, call, rest);
	return pop_control_(ptr, control);
}

static int comb_long_(Execute ptr, addr *ret, addr gen, addr comb, addr data)
{
	addr pos;

	/* make function */
	Return(comb_longform_(ptr, &data, gen, comb, data));
	Check(! functionp(data), "type error");
	/* result */
	clos_generic_call_heap(&pos, gen_comb_define_call, 1);
	SetClosGenericCallArray(pos, 0, data);

	return Result(ret, pos);
}

static int comb_short_(Execute ptr, addr *ret, addr gen, addr comb, addr data)
{
	addr pos;

	/* make function */
	Return(comb_shortform_(ptr, &data, gen, comb, data));
	Check(! functionp(data), "type error");
	/* result */
	clos_generic_call_heap(&pos, gen_comb_define_call, 1);
	SetClosGenericCallArray(pos, 0, data);

	return Result(ret, pos);
}

static int comb_lambda_(Execute ptr, addr *ret, addr gen, addr comb, addr data)
{
	int check;

	/* standard */
	if (comb == Nil)
		return comb_standard_(ret, data);

	/* long-form */
	Return(clos_long_combination_p_(comb, &check));
	if (check)
		return comb_long_(ptr, ret, gen, comb, data);

	/* short-form */
	Return(clos_short_combination_p_(comb, &check));
	if (check)
		return comb_short_(ptr, ret, gen, comb, data);

	/* error */
	*ret = Nil;
	return fmte_("Invalid method-combination ~S.", comb, NULL);
}


/*****************************************************************************
 *  generic-finalize
 *****************************************************************************/
int generic_eql_specializer_(addr left, addr right, int check, int *ret)
{
	int check1, check2;

	Return(clos_specializer_p_(left, &check1));
	Return(clos_specializer_p_(right, &check2));
	if (check1 && check2) {
		return Result(ret, left == right);
	}
	if (check1) {
		Return(stdget_specializer_type_(left, &left));
		return clos_subclass_p_(left, right, ret);
	}
	if (check2) {
		if (! check)
			return Result(ret, 0);
		Return(stdget_specializer_type_(right, &right));
		return clos_subclass_p_(left, right, ret);
	}

	return clos_subclass_p_(left, right, ret);
}

static int generic_make_method_check_(addr argtype, addr method, int *ret)
{
	int check;
	addr left, right;

	Return(stdget_method_specializers_(method, &method));
	while (argtype != Nil || method != Nil) {
		Check(argtype == Nil || method == Nil, "argument error");
		GetCons(argtype, &left, &argtype);
		GetCons(method, &right, &method);
		Return(generic_eql_specializer_(left, right, 0, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int generic_compare_class_(addr left, addr right, int *ret)
{
	int check;

	if (left == right)
		return Result(ret, 0);
	Return(clos_subclass_p_(left, right, &check));

	return Result(ret, check? -1: 1);
}

static int generic_compare_eql_(addr left, addr right, int *ret)
{
	if (left == right)
		return Result(ret, 0);
	Return(stdget_specializer_object_(left, &left));
	Return(stdget_specializer_object_(right, &right));
	*ret = 1;
	return fmte_("The eql-specializers have "
			"a difference value ~S /= ~S.", left, right, NULL);
}

static int generic_compare_eql_type_(addr left, addr right, int *ret)
{
	int check;

	Return(stdget_specializer_type_(left, &left));
	Return(generic_compare_class_(left, right, &check));

	return Result(ret, check == 0? -1: check);
}

static int generic_compare_type_eql_(addr left, addr right, int *ret)
{
	int check;

	Return(stdget_specializer_type_(right, &right));
	Return(generic_compare_class_(left, right, &check));

	return Result(ret, check == 0? 1: check);
}

static int generic_compare_specializer_(addr left, addr right, int *ret)
{
	int check1, check2;

	Return(clos_specializer_p_(left, &check1));
	Return(clos_specializer_p_(right, &check2));
	if (check1 && check2)
		return generic_compare_eql_(left, right, ret);
	if (check1)
		return generic_compare_eql_type_(left, right, ret);
	if (check2)
		return generic_compare_type_eql_(left, right, ret);
	if (left == right)
		return Result(ret, 0);

	return generic_compare_class_(left, right, ret);
}

static int generic_sort_compare_(addr a, addr b, int *ret,
		int (*call_)(addr, addr, int *))
{
	int check;
	addr x, y;

	for (;;) {
		if (a == Nil || b == Nil)
			break;
		GetCons(a, &x, &a);
		GetCons(b, &y, &b);
		Return((*call_)(x, y, &check));
		if (check < 0)
			return Result(ret, 1);
		if (0 < check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int generic_sort_call_(addr left, addr right, int *ret)
{
	Return(stdget_method_specializers_(left, &left));
	Return(stdget_method_specializers_(right, &right));
	return generic_sort_compare_(left, right, ret, generic_compare_specializer_);
}

static int generic_sort_(addr order,
		int (*call_)(addr, addr, int *),
		addr a, addr b, int *ret)
{
	int check;
	addr pos, x, y;
	size_t i;

	while (order != Nil) {
		GetCons(order, &pos, &order);
		GetIndex(pos, &i);
		getnth_unsafe(a, i, &x);
		getnth_unsafe(b, i, &y);
		Return((*call_)(x, y, &check));
		if (check < 0)
			return Result(ret, 1);
		if (0 < check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int generic_sort_order_call_(addr order, addr left, addr right, int *ret)
{
	Return(stdget_method_specializers_(left, &left));
	Return(stdget_method_specializers_(right, &right));
	return generic_sort_(order, generic_compare_specializer_, left, right, ret);
}

static int generic_specializers_sort_(addr *ret, addr gen, addr list)
{
	addr order;

	Return(stdget_generic_precedence_index_(gen, &order));
	if (order != Nil) {
		return simplesort_info_cons_unsafe_(ret,
				list, order, generic_sort_order_call_);
	}
	else {
		return simplesort_cons_unsafe_(ret,
				list, generic_sort_call_);
	}
}

static int generic_make_array_(addr *ret, addr gen, addr type)
{
	int check;
	addr array, data, methods, method, list;
	size_t size, index;

	Return(stdget_generic_vector_(gen, &array));
	LenArrayA4(array, &size);
	vector4_heap(&data, size);
	for (index = 0; index < size; index++) {
		GetArrayA4(array, index, &methods);
		/* remove-if-not */
		for (list = Nil; methods != Nil; ) {
			GetCons(methods, &method, &methods);
			Return(generic_make_method_check_(type, method, &check));
			if (check)
				cons_heap(&list, method, list);
		}
		/* sort */
		Return(generic_specializers_sort_(&list, gen, list));
		SetArrayA4(data, index, list);
	}

	return Result(ret, data);
}

static int generic_make_type_(Execute ptr, addr *ret, addr gen, addr type)
{
	addr comb, data;
	LocalHold hold;

	Return(stdget_generic_method_combination_(gen, &comb));
	Return(generic_make_array_(&data, gen, type));
	hold = LocalHold_local_push(ptr, data);
	Return(comb_lambda_(ptr, ret, gen, comb, data));
	localhold_end(hold);

	return 0;
}

static int generic_make_mapcar_class_of_(LocalRoot local,
		addr *ret, addr list, addr args)
{
	addr result, eqlcheck, arg, check;

	for (result = Nil; list != Nil; ) {
		GetCons(list, &eqlcheck, &list);
		if (args == Nil) {
			*ret = Nil;
			return fmte_("Too few arguments.", NULL);
		}
		GetCons(args, &arg, &args);
		if (eqlcheck == Nil) {
			check = Nil;
		}
		else {
			Return(clos_find_specializer_nil_(arg, &check));
		}
		if (check == Nil) {
			Return(clos_class_of_(arg, &check));
		}
		cons_local(local, &result, check, result);
	}
	nreverse(ret, result);

	return 0;
}

static int generic_make_lambda_call_(Execute ptr, addr inst, addr gen, addr args)
{
	addr eqlcheck, cache, key, value, cons;
	clos_generic_call call;
	LocalRoot local;
	LocalHold hold;

	local = ptr->local;
	GetClosGenericCallArray(inst, 0, &eqlcheck);
	GetClosGenericCallArray(inst, 1, &cache);
	Return(generic_make_mapcar_class_of_(local, &key, eqlcheck, args));
	Return(find_hashtable_(cache, key, &value));
	if (value == Unbound) {
		/* not found, tranlate to heap-list from dynamic list */
		copy_list_heap_unsafe(&key, key);
		hold = LocalHold_local_push(ptr, key);
		Return(generic_make_type_(ptr, &value, gen, key));
		localhold_end(hold);
		Return(intern_hashheap_(cache, key, &cons));
		SetCdr(cons, value);
	}

	/* clos_generic_call */
	CallClosGenericCall(value, &call);
	return (*call)(ptr, value, gen, args);
}

static int generic_make_lambda_(addr gen, addr *ret)
{
	addr eqlcheck, cache, call;

	Return(stdget_generic_eqlcheck_(gen, &eqlcheck));
	Return(stdget_generic_cache_(gen, &cache));
	clos_generic_call_heap(&call, gen_generic_make_lambda_call, 2);
	SetClosGenericCallArray(call, 0, eqlcheck);
	SetClosGenericCallArray(call, 1, cache);

	return Result(ret, call);
}

static int generic_no_method_(Execute ptr, addr inst, addr gen, addr args)
{
	return generic_no_applicable_method_(ptr, gen, args);
}

static int generic_make_generic_call_(addr gen, addr *ret)
{
	int check;
	addr call;

	Return(stdboundp_generic_eqlcheck_(gen, &check));
	if (check)
		return generic_make_lambda_(gen, ret);

	GetConst(CLOSDATA_NO_METHOD, &call);
	if (call == Unbound) {
		clos_generic_call_heap(&call, gen_generic_no_method, 0);
		SetConstant(CONSTANT_CLOSDATA_NO_METHOD, call);
	}

	return Result(ret, call);
}

int generic_finalize_(addr gen)
{
	addr pos;

	Return(generic_make_generic_call_(gen, &pos));
	Return(stdset_generic_call_(gen, pos));

	return 0;
}


/*****************************************************************************
 *  execute clos
 *****************************************************************************/
static int funcallable_p_(addr pos, int *ret)
{
	addr clos;

	if (! closp(pos))
		return 0;
	GetConst(CLOS_FUNCALLABLE_STANDARD_OBJECT, &clos);

	return clos_subtype_p_(pos, clos, ret);
}

int closrun_execute_(Execute ptr, addr clos, addr args)
{
	int check;
	addr pos;
	clos_generic_call call;

	Return(funcallable_p_(clos, &check));
	if (! check)
		return fmte_("Cannot exexute a funcallable instance ~S.", clos, NULL);
	Return(stdget_generic_call_(clos, &pos));
	CheckType(pos, LISPSYSTEM_GENERIC);
	CallClosGenericCall(pos, &call);
	return (*call)(ptr, pos, clos, args);
}

int generic_order_(addr gen, addr order, addr list)
{
#ifdef LISP_DEBUG
	addr var, x, y, root;
	size_t size1, size2, size3, index, check;

	Return(stdget_generic_argument_(gen, &var));
	GetArgument(var, ArgumentIndex_var, &var);

	Return(length_list_safe_(var, &size1));
	Return(length_list_safe_(order, &size2));
	Return(length_list_safe_(list, &size3));
	if (size1 != size2) {
		return fmte_("Length of :argument-precedence-order is "
				"not equal to lambda-list.", NULL);
	}
	if (size1 != size3) {
		return fmte_("Length of :precedence-index is "
				"not equal to lambda-list.", NULL);
	}
	for (root = list; order != Nil; ) {
		GetCons(order, &x, &order);
		Return_getcons(root, &y, &root);
		if (! position_list_eq_unsafe(x, var, &index)) {
			return fmte_("The variable ~S "
					"is not exist in :argument-precedence-order", x, NULL);
		}
		GetIndex(y, &check);
		if (index != check)
			return fmte_("Invalid precedence-index list.", NULL);
	}
#endif
	Return(stdset_generic_argument_precedence_order_(gen, order));
	Return(stdset_generic_precedence_index_(gen, list));

	return 0;
}


/*
 *  common
 */
int generic_compute_applicable_methods_(LocalRoot local,
		addr gen, addr args, addr *ret)
{
	addr data, root, list, pos;
	LocalStack stack;
	size_t size, i;

	push_local(local, &stack);
	Return(stdget_generic_eqlcheck_(gen, &data));
	Return(generic_make_mapcar_class_of_(local, &data, data, args));
	Return(generic_make_array_(&data, gen, data));
	LenArrayA4(data, &size);
	root = Nil;
	for (i = 0; i < size; i++) {
		GetArrayA4(data, i, &list);
		while (list != Nil) {
			GetCons(list, &pos, &list);
			cons_heap(&root, pos, root);
		}
	}
	rollback_local(local, stack);
	nreverse(ret, root);

	return 0;
}

static int generic_find_method_equal_(addr method, addr spec, int *ret)
{
	addr left, right, a, b;
	size_t x, y;

	Return(stdget_method_specializers_(method, &left));
	right = spec;
	/* length check */
	Return(length_list_safe_(left, &x));
	Return(length_list_safe_(right, &y));
	if (x != y) {
		*ret = 0;
		return fmte_("The length of specializers ~S "
				"does not match in the method ~S.", spec, method, NULL);
	}
	/* specializer check */
	while (left != Nil) {
		Return_getcons(left, &a, &left);
		Return_getcons(right, &b, &right);
		if (a != b)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int generic_find_method_(Execute ptr,
		addr gen, addr qua, addr spec, addr errorp, addr *ret)
{
	int check;
	addr comb, list, method;
	size_t index;

	Return(stdget_generic_method_combination_(gen, &comb));
	Return(qualifiers_position_nil_(ptr, qua, comb, &index, &check));
	if (check) {
		if (errorp != Nil) {
			*ret = Nil;
			return fmte_("The qualifiers ~S is not found in generic-function ~S.",
					qua, gen, NULL);
		}
		return Result(ret, Nil);
	}

	Return(stdget_generic_vector_(gen, &list));
	GetArrayA4(list, index, &list);
	while (list != Nil) {
		GetCons(list, &method, &list);
		Return(generic_find_method_equal_(method, spec, &check));
		if (check)
			return Result(ret, method);
	}
	/* not found */
	if (errorp != Nil) {
		*ret = Nil;
		return fmte_("The specializes ~S is not found in generic-function ~S ~S.",
				spec, gen, qua, NULL);
	}

	return Result(ret, Nil);
}


/*
 *  documentation
 */
int get_documentation_function_object_(addr pos, addr *ret)
{
	if (functionp(pos)) {
		getdocumentation_function(pos, ret);
		return 0;
	}

	/* generic-function */
	return stdget_generic_documentation_(pos, ret);
}

int set_documentation_function_object_(addr pos, addr value)
{
	if (functionp(pos)) {
		setdocumentation_function(pos, value);
		return 0;
	}

	/* generic-function */
	return stdset_generic_documentation_(pos, value);
}


/*
 *  initialize
 */
void init_clos_generic(void)
{
	SetPointerCall(defun, any, standard_lambda);
	ClosGenericTable[gen_comb_standard_call] = comb_standard_call_;
	ClosGenericTable[gen_comb_define_call] = comb_define_call_;
	ClosGenericTable[gen_generic_make_lambda_call] = generic_make_lambda_call_;
	ClosGenericTable[gen_generic_no_method] = generic_no_method_;
}


/************************************************************
 *  clos_make.c
 ************************************************************/

/*
 *  ensure-class
 */
int clos_ensure_class_supers_(addr args, addr *ret, int *referp)
{
	int check;
	addr list, pos;

	/* arguments */
	if (GetKeyArgs(args, CLOSKEY_DIRECT_SUPERCLASSES, &list)) {
		/* (list (find-class 'standard-object)) */
		GetConst(CLOS_STANDARD_OBJECT, &args);
		Return(clos_find_class_(args, &args));
		list_heap(ret, args, NULL);
		if (referp)
			*referp = 0;
		return 0;
	}

	/* check forward-referenced-class */
	*ret = list;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(clos_referenced_p_(pos, &check));
		if (check) {
			if (referp) {
				*referp = 1;
				return 0;
			}
			else {
				return fmte_("Cannot have a forward-referenced-class ~S "
						"in the finalized class.", pos, NULL);
			}
		}
	}
	if (referp)
		*referp = 0;

	return 0;
}

static int clos_ensure_class_parse_slots_(addr list, addr *ret)
{
	addr slot, name, readers, writers, alloc, args, form, func, type, doc;

	/* arguments */
	if (GetKeyArgs(list, CLOSKEY_NAME, &name)) {
		*ret = Nil;
		return fmte_("Invalid slot :name ~S.", name, NULL);
	}
	if (GetKeyArgs(list, CLOSKEY_TYPE, &type))
		GetTypeTable(&type, T);
	if (GetKeyArgs(list, CLOSKEY_INITARGS, &args))
		args = Nil;
	if (GetKeyArgs(list, CLOSKEY_INITFORM, &form))
		form = Unbound;
	if (GetKeyArgs(list, CLOSKEY_INITFUNCTION, &func))
		func = Nil;
	if (GetKeyArgs(list, CLOSKEY_READERS, &readers))
		readers = Nil;
	if (GetKeyArgs(list, CLOSKEY_WRITERS, &writers))
		writers = Nil;
	if (GetKeyArgs(list, CLOSKEY_DOCUMENTATION, &doc))
		doc = Nil;
	if (GetKeyArgs(list, CLOSKEY_ALLOCATION, &alloc))
		GetConst(CLOSKEY_INSTANCE, &alloc);

	/* make-slot */
	slot_heap(&slot);
	SetNameSlot(slot, name);
	SetTypeSlot(slot, type);
	SetArgsSlot(slot, args);
	SetFormSlot(slot, form);
	SetFunctionSlot(slot, func);
	SetReadersSlot(slot, readers);
	SetWritersSlot(slot, writers);
	SetDocumentSlot(slot, doc);
	Return(slot_set_allocation_(slot, alloc));

	/* result */
	return Result(ret, slot);
}

static int clos_ensure_class_slots_error_(addr slots, size_t size, addr pos)
{
	addr x, y;
	size_t i;

	GetNameSlot(pos, &x);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &y);
		GetNameSlot(y, &y);
		if (x == y)
			return call_simple_program_error_va_(NULL,
					"The slot name ~S already exists.", x, NULL);
	}

	return 0;
}

int clos_ensure_class_slots_(addr args, addr *ret)
{
	addr slots, pos;
	size_t size, i;

	/* :direct-slot list */
	if (GetKeyArgs(args, CLOSKEY_DIRECT_SLOTS, &args))
		args = Nil;

	/* slot-vector */
	Return(length_list_safe_(args, &size));
	slot_vector_heap(&slots, size);
	for (i = 0; args != Nil; i++) {
		GetCons(args, &pos, &args);
		Return(clos_ensure_class_parse_slots_(pos, &pos));
		Return(clos_ensure_class_slots_error_(slots, i, pos));
		SetSlotVector(slots, i, pos);
	}

	return Result(ret, slots);
}

int clos_ensure_class_direct_default_initargs_(LocalRoot local,
		addr pos, addr args, addr *ret)
{
	addr check, list, key, a, b;
	LocalStack stack;

	if (GetKeyArgs(args, CLOSKEY_DIRECT_DEFAULT_INITARGS, &args))
		return Result(ret, Nil);
	/* check only */
	push_local(local, &stack);
	*ret = args;
	check = Nil;
	while (args != Nil) {
		Return_getcons(args, &list, &args);
		/* (key initform initfunction) form */
		Return(list_bind_(list, &key, &a, &b, NULL));
		/* check duplicate */
		if (find_list_eq_unsafe(key, check)) {
			*ret = Nil;
			return call_simple_program_error_va_(NULL,
					":INITARG ~S is already exist.", key, NULL);
		}
		cons_local(local, &check, key, check);
	}
	rollback_local(local, stack);

	return 0;
}

static int clos_ensure_class_default_initargs_(LocalRoot local, addr pos, addr *ret)
{
	addr root, check, list, args, init, key;
	LocalStack stack;

	Return(stdget_class_precedence_list_(pos, &list));
	root = check = Nil;
	push_local(local, &stack);
	while (list != Nil) {
		Return_getcons(list, &args, &list);
		Return(stdget_class_direct_default_initargs_(args, &args));
		while (args != Nil) {
			Return_getcons(args, &init, &args);
			Return_getcar(init, &key);
			if (! find_list_eq_unsafe(key, check)) {
				cons_local(local, &check, key, check);
				cons_heap(&root, init, root);
			}
		}
	}
	rollback_local(local, stack);
	nreverse(ret, root);

	return 0;
}

/* reader/writer check */
static int clos_ensure_reader_check_(Execute ptr, addr gen)
{
	int check;
	addr pos;
	size_t index;

	/* generic-function */
	if (gen == Unbound)
		return 0;
	if (functionp(gen))
		return fmte_("The function ~S must be a generic-function.", gen, NULL);

	/* qualifiers */
	Return(stdget_generic_method_combination_(gen, &pos));
	Return(qualifiers_position_nil_(ptr, Nil, pos, &index, &check));
	if (check)
		return fmte_("The generic-function ~S don't have a NIL qualifier.", gen, NULL);

	/* specializer */
	Return(stdget_generic_argument_(gen, &pos));
	Check(! argumentp(pos), "type error");
	if (ArgumentStruct(pos)->var != 1)
		return fmte_("The generic-function ~S must be a 1 specializer.", gen, NULL);

	return 0;
}

static int clos_ensure_writer_method_check_(Execute ptr, addr gen)
{
	int check;
	addr pos;
	size_t index;

	/* generic-function */
	if (gen == Unbound)
		return 0;
	if (functionp(gen))
		return fmte_("The function ~S must be a generic-function.", gen, NULL);

	/* qualifiers */
	Return(stdget_generic_method_combination_(gen, &pos));
	Return(qualifiers_position_nil_(ptr, Nil, pos, &index, &check));
	if (check)
		return fmte_("The generic-function ~S don't have a NIL qualifier.", gen, NULL);

	/* specializer */
	Return(stdget_generic_argument_(gen, &pos));
	Check(! argumentp(pos), "type error");
	if (ArgumentStruct(pos)->var != 2)
		return fmte_("The generic-function ~S must be a 2 specializers.", gen, NULL);

	return 0;
}

static int clos_ensure_readers_check_(Execute ptr, addr list)
{
	addr name, gen;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		Return(clos_ensure_reader_check_(ptr, gen));
	}

	return 0;
}

static int clos_ensure_writers_check_(Execute ptr, addr list)
{
	addr name, gen;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		Return(clos_ensure_writer_method_check_(ptr, gen));
	}

	return 0;
}

static int clos_ensure_class_function_check_(Execute ptr, addr pos)
{
	addr slots, slot, list;
	size_t size, i;

	Return(stdget_class_slots_(pos, &slots));
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		/* reader */
		GetReadersSlot(slot, &list);
		Return(clos_ensure_readers_check_(ptr, list));
		/* writer */
		GetWritersSlot(slot, &list);
		Return(clos_ensure_writers_check_(ptr, list));
	}

	return 0;
}

/* reader/writer generic */
static int clos_ensure_reader_generic_(addr name)
{
	addr lambda;
	mop_argument_generic_var1(&lambda);
	return generic_make_empty_(name, lambda, &name);
}

static int clos_ensure_writer_generic_(addr name)
{
	addr lambda;
	mop_argument_generic_var2(&lambda);
	return generic_make_empty_(name, lambda, &name);
}

static int clos_ensure_readers_generic_(addr list)
{
	addr name, gen;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		if (gen != Unbound)
			continue;
		Return(clos_ensure_reader_generic_(name));
	}

	return 0;
}

static int clos_ensure_writers_generic_(addr list)
{
	addr name, gen;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		if (gen != Unbound)
			continue;
		Return(clos_ensure_writer_generic_(name));
	}

	return 0;
}

static int clos_ensure_class_function_generic_(addr pos)
{
	addr slots, slot, list;
	size_t size, i;

	Return(stdget_class_slots_(pos, &slots));
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		/* reader */
		GetReadersSlot(slot, &list);
		Return(clos_ensure_readers_generic_(list));
		/* writer */
		GetWritersSlot(slot, &list);
		Return(clos_ensure_writers_generic_(list));
	}

	return 0;
}

/* make method */
static void method_argument_clos_ensure_reader(addr clos, addr *ret)
{
	addr pos, list;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 1;
	list_heap(&list, Nil, clos, NULL);
	list_heap(&list, list, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int function_clos_ensure_reader(Execute ptr, addr method, addr next, addr inst)
{
	addr call, symbol;

	/* (slot-value inst symbol) */
	GetConst(COMMON_SLOT_VALUE, &call);
	Return(getfunction_global_(call, &call));
	getdata_control(ptr, &symbol);
	return funcall_control_(ptr, call, inst, symbol, NULL);
}

static int clos_ensure_reader_method_(Execute ptr,
		addr clos, addr name, addr gen, addr symbol)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	SetDataFunction(call, symbol);
	setcompiled_var3(call, p_defun_clos_ensure_reader);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	method_argument_clos_ensure_reader(clos, &pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return method_add_method_(ptr, gen, pos);
}

static void method_argument_clos_ensure_writer(addr clos, addr *ret)
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
	list_heap(&type2, Nil, clos, NULL);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int function_clos_ensure_writer_instance(Execute ptr,
		addr method, addr next, addr value, addr inst)
{
	addr call, symbol;

	/* ((setf slot-value) value inst symbol) */
	GetConst(COMMON_SLOT_VALUE, &call);
	Return(getsetf_global_(call, &call));
	getdata_control(ptr, &symbol);
	return funcall_control_(ptr, call, value, inst, symbol, NULL);
}

static int clos_ensure_writer_method_(Execute ptr,
		addr clos, addr name, addr gen, addr symbol)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	SetDataFunction(call, symbol);
	setcompiled_var4(call, p_defun_clos_ensure_writer_instance);
	GetTypeCompiled(&type, Writer_Method);
	settype_function(call, type);
	/* method */
	method_argument_clos_ensure_writer(clos, &pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return method_add_method_(ptr, gen, pos);
}

static int clos_ensure_readers_method_(Execute ptr, addr pos, addr symbol, addr list)
{
	addr name, gen;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		Check(gen == Unbound, "generic-function error");
		Return(clos_ensure_reader_method_(ptr, pos, name, gen, symbol));
	}

	return 0;
}

static int clos_ensure_writers_method_(Execute ptr, addr pos, addr symbol, addr list)
{
	addr name, gen;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		Check(gen == Unbound, "generic-function error");
		Return(clos_ensure_writer_method_(ptr, pos, name, gen, symbol));
	}

	return 0;
}

static int clos_ensure_class_method_(Execute ptr, addr pos)
{
	addr slots, slot, symbol, list;
	size_t size, i;

	Return(stdget_class_slots_(pos, &slots));
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetNameSlot(slot, &symbol);
		/* reader */
		GetReadersSlot(slot, &list);
		Return(clos_ensure_readers_method_(ptr, pos, symbol, list));
		/* writer */
		GetWritersSlot(slot, &list);
		Return(clos_ensure_writers_method_(ptr, pos, symbol, list));
	}

	return 0;
}

static int clos_ensure_class_function_(Execute ptr, addr pos)
{
	/* check */
	Return(clos_ensure_class_function_check_(ptr, pos));
	/* make generic-function */
	Return(clos_ensure_class_function_generic_(pos));
	/* make method */
	return clos_ensure_class_method_(ptr, pos);
}

static int clos_ensure_class_subclasses_(addr pos)
{
	addr supers, super, list;

	Return(stdget_class_direct_superclasses_(pos, &supers));
	while (supers != Nil) {
		Return_getcons(supers, &super, &supers);
		Return(stdget_class_direct_subclasses_(super, &list));
		pushnew_heap(list, pos, &list);
		Return(stdset_class_direct_subclasses_(super, list));
	}

	return 0;
}

static int clos_built_in_class_check_(addr pos)
{
	int check;
	addr list, super, tclass, built;

	Return(stdget_class_direct_superclasses_(pos, &list));
	GetConst(CLOS_T, &tclass);
	GetConst(CLOS_BUILT_IN_CLASS, &built);
	while (list != Nil) {
		GetCons(list, &super, &list);
		if (super == tclass)
			continue;
		Return(clos_subtype_p_(super, built, &check));
		if (check) {
			Return(stdget_class_name_(pos, &pos));
			return call_type_error_va_(NULL, pos, Nil,
					"Invalid superclass ~S in ~S.", super, pos, NULL);
		}
	}

	return 0;
}

int clos_ensure_class_init_(LocalRoot local, addr pos, int pushp)
{
	addr value;

	/* class-precedence-list */
	Return(clos_built_in_class_check_(pos));
	Return(clos_precedence_list_(local, pos, &value));
	Return(stdset_class_precedence_list_(pos, value));
	/* effective-slots */
	Return(clos_compute_slots_(local, pos, &value));
	Return(stdset_class_slots_(pos, value));
	/* default-initargs */
	Return(clos_ensure_class_default_initargs_(local, pos, &value));
	Return(stdset_class_default_initargs_(pos, value));
	/* subclasses */
	if (pushp) {
		Return(clos_ensure_class_subclasses_(pos));
	}

	return 0;
}

static int clos_ensure_class_set_(
		LocalRoot local, addr pos, addr name, addr args, int pushp)
{
	int referp;
	addr supers, slots, value;

	/* arguments */
	Return(clos_ensure_class_supers_(args, &supers, &referp));
	Return(clos_ensure_class_slots_(args, &slots));
	/* set value */
	Return(stdset_class_name_(pos, name));
	Return(clos_stdclass_direct_slots_(pos, slots));
	Return(stdset_class_direct_superclasses_(pos, supers));
	/* direct-default-initargs */
	Return(clos_ensure_class_direct_default_initargs_(local, pos, args, &value));
	Return(stdset_class_direct_default_initargs_(pos, value));
	/* forward-referenced-class */
	if (! referp)
		return clos_ensure_class_init_(local, pos, pushp);

	return 0;
}

int clos_finalize_(Execute ptr, addr pos, int *ret);
static int clos_finalize_forward_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;
	addr name;

	Return(stdget_class_name_(pos, &name));
	Return(clos_find_class_(name, &pos));
	Return(clos_finalize_(ptr, pos, &check));
	if (check)
		return Result(ret, 1);
	*value = pos;
	return Result(ret, 0);
}

static int clos_finalize_forward_p_(Execute ptr, addr clos, int *refp, int *ret)
{
	int value, check;
	addr list, root, pos;
	LocalRoot local;
	LocalStack stack;

	/* update */
	Return(stdget_class_direct_superclasses_(clos, &list));
	value = 0;
	local = ptr->local;
	push_local(local, &stack);
	for (root = Nil; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(clos_referenced_p_(pos, &check));
		if (check) {
			Return(clos_finalize_forward_(ptr, pos, &pos, &check));
			if (check)
				return Result(ret, 1);
			value = 1;
		}
		cons_local(local, &root, pos, root);
	}
	/* replace */
	if (value) {
		reverse_list_heap_unsafe(&root, root);
		Return(stdset_class_direct_superclasses_(clos, root));
	}
	rollback_local(local, stack);
	*refp = value;

	return Result(ret, 0);
}

int clos_finalize_(Execute ptr, addr pos, int *ret)
{
	int refp, check;
	addr list, value;

	/* finalized check */
	Return(stdget_class_finalized_p_(pos, &value));
	if (value != Nil)
		return Result(ret, 0);

	/* referenced class */
	Return(clos_finalize_forward_p_(ptr, pos, &refp, &check));
	if (check)
		return Result(ret, 1);
	if (refp) {
		/* make class */
		Return(clos_ensure_class_init_(ptr->local, pos, 1));
	}

	/* superclasses */
	Return(stdget_class_direct_superclasses_(pos, &list));
	while (list != Nil) {
		GetCons(list, &value, &list);
		Return(clos_finalize_(ptr, value, &check));
		if (check)
			return Result(ret, 1);
	}

	/* prototype */
	Return(clos_stdclass_prototype_(pos));
	Return(clos_ensure_class_function_(ptr, pos));
	Return(stdset_class_finalized_p_(pos, T));

	return Result(ret, 0);
}

static int clos_ensure_class_object_(Execute ptr, addr name, addr args, addr *ret)
{
	int ignore;
	addr metaclass, pos;
	LocalRoot local;

	/* :metaclass ... */
	if (GetKeyArgs(args, CLOSKEY_METACLASS, &metaclass))
		GetConst(CLOS_STANDARD_CLASS, &metaclass);
	local = ptr->local;
	GetConst(CLOSKEY_METACLASS, &pos);
	Return(remplist_local_(local, args, pos, &args, &ignore));

	/* (apply #'make-instance metaclass args) */
	GetConst(COMMON_MAKE_INSTANCE, &pos);
	Return(getfunction_global_(pos, &pos));
	return applya1_control_(ptr, ret, pos, metaclass, args, NULL);
}

int clos_ensure_class_(Execute ptr, addr name, addr args, addr *ret)
{
	addr pos;

	/* make-instance */
	Return(clos_ensure_class_object_(ptr, name, args, &pos));

	/* define class */
	Return(clos_ensure_class_set_(ptr->local, pos, name, args, 1));
	clos_define_class(name, pos);

	return Result(ret, pos);
}


/*
 *  allocate-initialize
 */
int allocate_instance_standard_(Execute ptr, addr clos, addr *ret)
{
	int check;
	addr instance, slots, slot, name;
	size_t size, i, loc;

	CheckType(clos, LISPTYPE_CLOS);
	/* finalized */
	Return(clos_finalize_(ptr, clos, &check));
	if (check) {
		*ret = Nil;
		return fmte_("Cannot finalize class object ~S.", clos, NULL);
	}

	/* allocate */
	Return(stdget_class_slots_(clos, &slots));
	slot_vector_copyheap_alloc(NULL, &slots, slots);
	clos_heap(&instance, slots);

	/* class-of */
	SetClassOfClos(instance, clos);

	/* value */
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		/* name check */
		GetNameSlot(slot, &name);
		if (! symbolp(name)) {
			*ret = Nil;
			return fmte_("The slot name ~S must be a symbol.", name, NULL);
		}
		/* already exist */
		if (clos_find_slotname(slots, i, name)) {
			*ret = Nil;
			return fmte_("The slot name ~S already exists.", name, NULL);
		}
		/* location */
		GetLocationSlot(slot, &loc);
		if (loc != i) {
			*ret = Nil;
			return fmte_("The slot location ~A is invalid.", intsizeh(i), NULL);
		}
	}

	return Result(ret, instance);
}


/*
 *  initialize-instance
 *  reinitialize-instance
 */
static int initialize_instance_(Execute ptr, addr pos, addr type, addr rest, addr *ret)
{
	/* (apply #'shared-initialize instance type initargs) */
	addr call;

	GetConst(COMMON_SHARED_INITIALIZE, &call);
	Return(getfunction_global_(call, &call));
	return applya1_control_(ptr, ret, call, pos, type, rest, NULL);
}

int initialize_instance_stdobject_(Execute ptr, addr pos, addr rest, addr *ret)
{
	/* (apply #'shared-initialize instance T initargs) */
	return initialize_instance_(ptr, pos, T, rest, ret);
}

int reinitialize_instance_stdobject_(Execute ptr, addr pos, addr rest, addr *ret)
{
	/* (apply #'shared-initialize instance () initargs) */
	return initialize_instance_(ptr, pos, Nil, rest, ret);
}


/*
 *  shared-initialize
 */
static int setf_slot_value_call_(Execute ptr, addr pos, addr key, addr value);
static int shared_initialize_arguments_(Execute ptr,
		addr pos, addr slot, addr rest, int *ret)
{
	addr list, key, value;

	GetArgsSlot(slot, &list);
	while (list != Nil) {
		Return_getcons(list, &key, &list);
		if (getplist_safe(rest, key, &value))
			continue;
		GetNameSlot(slot, &key);
		Return(setf_slot_value_call_(ptr, pos, key, value));
		return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int slot_boundp_call_(Execute ptr, addr pos, addr key, int *ret);
static int shared_initialize_initform_(Execute ptr, addr pos, addr key, addr slot)
{
	int check;
	addr value;

	/* boundp */
	Return(slot_boundp_call_(ptr, pos, key, &check));
	if (check)
		return 0;

	/* initform */
	GetFormSlot(slot, &value);
	if (value == Unbound)
		return 0;

	/* initfunction */
	GetFunctionSlot(slot, &value);
	if (value == Nil) {
		/* :initform */
		GetFormSlot(slot, &value);
	}
	else {
		/* funcall */
		Return(funcall1_control_(ptr, &value, value, NULL));
	}

	/* (setf slot-value) */
	return setf_slot_value_call_(ptr, pos, key, value);
}

static int shared_initialize_stdobject_p_(addr key, addr name, int *ret)
{
	int check;

	if (name == T)
		return Result(ret, 0);
	Return(find_list_eq_safe_(key, name, &check));
	return Result(ret, ! check);
}

int shared_initialize_stdobject_(Execute ptr, addr pos, addr name, addr rest)
{
	int check;
	addr slots, slot, key;
	size_t size, i;

	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		/* initialize arguments */
		Return(shared_initialize_arguments_(ptr, pos, slot, rest, &check));
		if (check)
			continue;
		/* initform */
		GetNameSlot(slot, &key);
		Return(shared_initialize_stdobject_p_(key, name, &check));
		if (check)
			continue;
		Return(shared_initialize_initform_(ptr, pos, key, slot));
	}

	return 0;
}


/*
 *  make-instance
 */
static int make_instance_initargs_(Execute ptr, addr clos, addr rest, addr *ret)
{
	addr list, root, keys, key, slots, temp, value, call;
	LocalRoot local;

	local = ptr->local;
	Return(stdget_class_default_initargs_(clos, &list));
	if (list == Nil) {
		cons_local(local, ret, clos, rest);
		return 0;
	}

	/* rest */
	root = keys = Nil;
	while (rest != Nil) {
		Return_getcons(rest, &key, &rest);
		Return_getcons(rest, &value, &rest);
		if (! find_list_eq_unsafe(key, keys)) {
			cons_local(local, &keys, key, keys);
			cons_local(local, &root, key, root);
			cons_local(local, &root, value, root);
		}
	}

	/* default-initargs */
	Return(stdget_class_slots_(clos, &slots));
	for (root = Nil; list != Nil; ) {
		Return_getcons(list, &temp, &list);
		Return(list_bind_(temp, &key, &value, &call, NULL));
		if (! find_list_eq_unsafe(key, keys)) {
			if (call != Nil) {
				Return(funcall1_control_(ptr, &value, call, NULL));
			}
			cons_local(local, &keys, key, keys);
			cons_local(local, &root, key, root);
			cons_local(local, &root, value, root);
		}
	}

	/* result */
	nreverse(&root, root);
	cons_local(local, ret, clos, root);
	return 0;
}

static int make_instance_check_(Execute ptr, addr clos, addr rest)
{
	int loop, check;
	addr slots, slot, key, value;
	size_t size, i;

	Return(stdget_class_slots_(clos, &slots));
	LenSlotVector(slots, &size);
	Return_getcdr(rest, &rest);
	while (rest != Nil) {
		Return_getcons(rest, &key, &rest);
		Return_getcdr(rest, &rest);
		loop = 0;
		for (i = 0; i < size; i++) {
			GetSlotVector(slots, i, &slot);
			GetArgsSlot(slot, &value);
			Return(find_list_eq_safe_(key, value, &check));
			loop |= check;
		}
		if (! loop) {
			return fmte_("The initialize argument ~S don't exist in ~S slots.",
					key, clos, NULL);
		}
	}

	return 0;
}

int make_instance_stdclass_(Execute ptr, addr rest, addr *ret)
{
	int check;
	addr clos, type, call, instance;

	/* built-in-class */
	GetCons(rest, &clos, &rest);
	GetConst(CLOS_BUILT_IN_CLASS, &type);
	if (clos == type) {
		GetConst(CLOS_STANDARD_CLASS, &type);
		return call_type_error_va_(ptr, clos, type,
				"Cannot make an instance of the built-in-class.", NULL);
	}

	/* finalize */
	Return(clos_finalize_(ptr, clos, &check));
	if (check) {
		*ret = Nil;
		return fmte_("Cannot finalize class object ~S.", clos, NULL);
	}

	/* initargs */
	Return(make_instance_initargs_(ptr, clos, rest, &rest));
	Return(make_instance_check_(ptr, clos, rest));

	/* allocation-instance */
	GetConst(COMMON_ALLOCATE_INSTANCE, &call);
	Return(getfunction_global_(call, &call));
	Return(apply1_control_(ptr, &instance, call, rest));

	/* initialize-instance */
	GetCdr(rest, &rest);
	cons_local(ptr->local, &rest, instance, rest);
	GetConst(COMMON_INITIALIZE_INSTANCE, &call);
	Return(getfunction_global_(call, &call));
	Return(apply1_control_(ptr, &call, call, rest));

	/* result */
	return Result(ret, instance);
}


/*
 *  slot-missing
 */
static int clos_slot_missing_(Execute ptr, addr *ret,
		addr clos, addr pos, addr name, addr operation, addr value)
{
	addr call;

	GetConst(COMMON_SLOT_MISSING, &call);
	Return(getfunction_global_(call, &call));
	return funcall1_control_(ptr, ret, call, clos, pos, name, operation, value, NULL);
}

static int clos_slot_unbound_(Execute ptr, addr *ret, addr clos, addr pos, addr name)
{
	addr call;

	GetConst(COMMON_SLOT_UNBOUND, &call);
	Return(getfunction_global_(call, &call));
	return funcall1_control_(ptr, ret, call, clos, pos, name, NULL);
}


/*
 *  slot-boundp
 */
static int slot_boundp_call_(Execute ptr, addr pos, addr key, int *ret)
{
	addr call;

	GetConst(COMMON_SLOT_BOUNDP, &call);
	Return(getfunction_global_(call, &call));
	Return(funcall1_control_(ptr, &pos, call, pos, key, NULL));
	return Result(ret, (pos != Nil));
}

int slot_boundp_using_class_common_(Execute ptr,
		addr clos, addr pos, addr key, int *ret)
{
	addr slots, slot, check;
	size_t size, i;

	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetNameSlot(slot, &check);
		if (key != check)
			continue;
		if (slot_instance_p(slot)) {
			/* :instance */
			GetValueClos(pos, &pos);
			GetClosValue(pos, i, &pos);
			return Result(ret, pos != Unbound);
		}
		else {
			/* :class */
			GetClassSlot(slot, &pos);
			Return(stdget_class_prototype_(pos, &pos));
			return slot_boundp_call_(ptr, pos, key, ret);
		}
	}

	/* slot-missing */
	GetConst(COMMON_SLOT_BOUNDP, &check);
	Return(clos_slot_missing_(ptr, &pos, clos, pos, key, check, Unbound));
	return Result(ret, (pos != Nil));
}


/*
 *  slot-makunbound
 */
static int slot_makunbound_call_(Execute ptr, addr pos, addr key)
{
	addr call;

	GetConst(COMMON_SLOT_MAKUNBOUND, &call);
	Return(getfunction_global_(call, &call));
	return funcall1_control_(ptr, &pos, call, pos, key, NULL);
}

int slot_makunbound_using_class_(Execute ptr, addr clos, addr pos, addr key)
{
	addr slots, slot, check;
	size_t size, i;

	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetNameSlot(slot, &check);
		if (key != check)
			continue;
		if (slot_instance_p(slot)) {
			/* :instance */
			GetValueClos(pos, &pos);
			SetClosValue(pos, i, Unbound);
			return 0;
		}
		else {
			/* :class */
			GetClassSlot(slot, &pos);
			Return(stdget_class_prototype_(pos, &pos));
			return slot_makunbound_call_(ptr, pos, key);
		}
	}

	/* slot-missing */
	GetConst(COMMON_SLOT_MAKUNBOUND, &check);
	return clos_slot_missing_(ptr, &pos, clos, pos, key, check, Unbound);
}


/*
 *  slot-value
 */
static int slot_value_call_(Execute ptr, addr pos, addr key, addr *ret)
{
	addr call;

	GetConst(COMMON_SLOT_VALUE, &call);
	Return(getfunction_global_(call, &call));
	return funcall1_control_(ptr, ret, call, pos, key, NULL);
}

static int slot_value_using_class_getp_(Execute ptr,
		addr clos, addr pos, addr key, addr *ret)
{
	addr slots, slot, check;
	size_t size, i;

	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetNameSlot(slot, &check);
		if (key != check)
			continue;
		if (slot_instance_p(slot)) {
			/* :instance */
			GetValueClos(pos, &pos);
			GetClosValue(pos, i, ret);
			return 0;
		}
		else {
			/* :class */
			GetClassSlot(slot, &pos);
			Return(stdget_class_prototype_(pos, &pos));
			return slot_value_call_(ptr, pos, key, ret);
		}
	}

	/* slot-missing */
	GetConst(COMMON_SLOT_VALUE, &check);
	return clos_slot_missing_(ptr, ret, clos, pos, key, check, Unbound);
}

int slot_value_using_class_common_(Execute ptr,
		addr clos, addr pos, addr key, addr *ret)
{
	Return(slot_value_using_class_getp_(ptr, clos, pos, key, ret));
	if (*ret == Unbound)
		return clos_slot_unbound_(ptr, ret, clos, pos, key);

	return 0;
}


/*
 *  (setf slot-value)
 */
static int setf_slot_value_call_(Execute ptr, addr pos, addr key, addr value)
{
	addr call;

	GetConst(COMMON_SLOT_VALUE, &call);
	Return(getsetf_global_(call, &call));
	return funcall1_control_(ptr, &value, call, value, pos, key, NULL);
}

int setf_slot_value_using_class_common_(Execute ptr,
		addr clos, addr pos, addr key, addr value)
{
	addr slots, slot, check;
	size_t size, i;

	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetNameSlot(slot, &check);
		if (key != check)
			continue;
		if (slot_instance_p(slot)) {
			/* :instance */
			GetValueClos(pos, &pos);
			SetClosValue(pos, i, value);
			return 0;
		}
		else {
			/* :class */
			GetClassSlot(slot, &pos);
			Return(stdget_class_prototype_(pos, &pos));
			return setf_slot_value_call_(ptr, pos, key, value);
		}
	}

	/* slot-missing */
	GetConst(COMMON_SETF, &check);
	return clos_slot_missing_(ptr, &pos, clos, pos, key, check, value);
}


/*
 *  initialize
 */
void init_clos_make(void)
{
	SetPointerCall(defun, var3, clos_ensure_reader);
	SetPointerCall(defun, var4, clos_ensure_writer_instance);
}


/************************************************************
 *  clos_method.c
 ************************************************************/

/*
 *  access
 */
static int stdget_method_constant_(addr pos, addr *ret,
		enum Clos_method_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_method_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_METHOD, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}

static int stdset_method_constant_(addr pos, addr value,
		enum Clos_method_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_method_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_METHOD, &check);
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
#define StdGetMethod_(p,r,a,b) \
	stdget_method_constant_((p), (r), Clos_method_##a, CONSTANT_CLOSNAME_##b)
#define StdSetMethod_(p,r,a,b) \
	stdset_method_constant_((p), (r), Clos_method_##a, CONSTANT_CLOSNAME_##b)

int stdget_method_function_(addr pos, addr *ret)
{
	return StdGetMethod_(pos, ret, function, FUNCTION);
}
int stdset_method_function_(addr pos, addr value)
{
	return StdSetMethod_(pos, value, function, FUNCTION);
}

int stdget_method_generic_function_(addr pos, addr *ret)
{
	return StdGetMethod_(pos, ret, generic_function, GENERIC_FUNCTION);
}
int stdset_method_generic_function_(addr pos, addr value)
{
	return StdSetMethod_(pos, value, generic_function, GENERIC_FUNCTION);
}

int stdget_method_lambda_list_(addr pos, addr *ret)
{
	return StdGetMethod_(pos, ret, lambda_list, LAMBDA_LIST);
}
int stdset_method_lambda_list_(addr pos, addr value)
{
	return StdSetMethod_(pos, value, lambda_list, LAMBDA_LIST);
}

int stdget_method_qualifiers_(addr pos, addr *ret)
{
	return StdGetMethod_(pos, ret, qualifiers, QUALIFIERS);
}
int stdset_method_qualifiers_(addr pos, addr value)
{
	return StdSetMethod_(pos, value, qualifiers, QUALIFIERS);
}

int stdget_method_specializers_(addr pos, addr *ret)
{
	return StdGetMethod_(pos, ret, specializers, SPECIALIZERS);
}
int stdset_method_specializers_(addr pos, addr value)
{
	return StdSetMethod_(pos, value, specializers, SPECIALIZERS);
}


/*
 *  defmethod
 */
static int method_instance_alloc_(LocalRoot local, addr *ret, addr clos,
		addr lambda, addr qua, addr spec, addr call)
{
	addr pos;

	/* make-instance */
	if (clos == Nil)
		GetConst(CLOS_STANDARD_METHOD, &clos);
	Return(clos_instance_alloc_(local, clos, &pos));
	Return(stdset_method_lambda_list_(pos, lambda));
	Return(stdset_method_qualifiers_(pos, qua));
	Return(stdset_method_specializers_(pos, spec));
	Return(stdset_method_function_(pos, call));

	return Result(ret, pos);
}

static int method_instance_heap_(addr *ret, addr clos,
		addr lambda, addr qua, addr spec, addr call)
{
	return method_instance_alloc_(NULL, ret, clos, lambda, qua, spec, call);
}

int method_instance_call_(LocalRoot local, addr *ret, addr clos, addr call)
{
	return method_instance_alloc_(local, ret, clos, Nil, Nil, Nil, call);
}

static int method_specializer_list_(addr *ret, addr list)
{
	addr root, var, spec;

	/* var opt rest key */
	GetArgument(list, ArgumentIndex_var, &list);
	/* ((symbol spec) ...) */
	for (root = Nil; list != Nil; ) {
		/* (symbol spec) */
		GetCons(list, &var, &list);
		GetCons(var, &var, &spec);
		GetCar(spec, &spec);
		/* spec */
		if (consp(spec)) {
			/* (eql spec) */
			GetCdr(spec, &spec); /* eql */
			GetCar(spec, &spec); /* spec */
			Return(clos_intern_specializer_(spec, &spec));
		}
		else {
			/* type */
			if (! closp(spec)) {
				Return(clos_find_class_(spec, &spec));
			}
		}
		cons_heap(&root, spec, root);
	}
	nreverse(ret, root);

	return 0;
}

int method_instance_lambda_(LocalRoot local, addr *ret, addr clos, addr lambda)
{
	addr spec;

	Check(! argumentp(lambda), "type error");
	Return(method_specializer_list_(&spec, lambda));
	return method_instance_heap_(ret, clos, lambda, Nil, spec, Nil);
}


/*
 *  add-method
 */
static int method_check_generic_function_(addr gen, addr method)
{
	addr check;

	Return(stdget_method_generic_function_(method, &check));
	if (check != Nil && check != gen) {
		return fmte_("The method ~S is already exists "
				"in the generic-function ~S.", method, gen, NULL);
	}

	return 0;
}

static int method_check_method_class_(addr gen, addr method)
{
	int check;

	Return(stdget_generic_method_class_(gen, &gen));
	Return(clos_subtype_p_(method, gen, &check));
	if (! check)
		return fmte_("The method don't push in the generic-function.", NULL);

	return 0;
}

static int method_check_method_qualifiers_(Execute ptr, addr gen, addr method)
{
	int check;
	addr qua, comb;

	Return(stdget_method_qualifiers_(method, &qua));
	Return(stdget_generic_method_combination_(gen, &comb));
	Return(check_qualifiers_equal_(ptr, comb, qua, &check));
	if (! check) {
		return fmte_("The qualifiers ~S "
				"is not found in the method-combination.", qua, NULL);
	}

	return 0;
}

static int method_null_set_difference(addr key1, addr key2)
{
	int check;
	addr left, right, loop;

	/* (null (set-difference key1 key2 :key #'cadr :test #'eq)) */
	while (key1 != Nil) {
		GetCons(key1, &left, &key1);
		/* cadr */
		GetCdr(left, &left);
		GetCar(left, &left);
		/* (find left key2) */
		check = 0;
		for (loop = key2; loop != Nil; ) {
			GetCons(loop, &right, &loop);
			/* cadr */
			GetCdr(right, &right);
			GetCar(right, &right);
			if (left == right) {
				check = 1;
				break;
			}
		}
		if (check == 0) {
			/* not found */
			return 0;
		}
	}

	return 1;
}

static int method_arguments_check1_(
		const struct argument_struct *str1,
		const struct argument_struct *str2)
{
	if (str1->var != str2->var) {
		return fmte_("The count of variable is "
				"not equal to the generic function.", NULL);
	}

	return 0;
}

static int method_arguments_check2_(
		const struct argument_struct *str1,
		const struct argument_struct *str2)
{
	if (str1->opt != str2->opt) {
		return fmte_("The count of &optional is "
				"not equal to the generic function.", NULL);
	}

	return 0;
}

static int method_arguments_check3_(
		const struct argument_struct *str1,
		const struct argument_struct *str2)
{
	int check1, check2, checka;

	check1 = str1->rest || str1->keyp;
	check2 = str2->rest || str2->keyp;
	checka = ((! check1) && (! check2));
	if (! (checka || check2))
		return fmte_("The method must have &rest or &key arguments.", NULL);

	return 0;
}

static int method_arguments_check4_(
		addr pos1, const struct argument_struct *str1,
		addr pos2, const struct argument_struct *str2)
{
	if (! str1->key)
		return 0; /* not keyp */
	if (str2->rest)
		return 0; /* &key ..., &rest -> ok */
	if (str2->allow)
		return 0; /* &key ..., &key &allow-other-keys -> ok */
	if (str1->keyp && str2->keyp)
		return 0; /* &key, &key -> ok */
	if (str1->keyp)
		return 0; /* &key, ... -> ok */
	if (str2->keyp)
		goto error; /* ..., &key -> error */
	/* &key ..., &key ... */
	GetArgument(pos1, ArgumentIndex_key, &pos1);
	GetArgument(pos2, ArgumentIndex_key, &pos2);
	if (! method_null_set_difference(pos1, pos2))
		goto error;
	return 0;

error:
	return fmte_("The &key arguments in the method must have "
			"all &key arguments in generic function.", NULL);
}

static int method_check_method_arguments_(addr gen, addr method)
{
	addr pos1, pos2;
	struct argument_struct *str1, *str2;

	/* generic-lambda-list */
	Return(stdget_generic_argument_(gen, &pos1));
	CheckType(pos1, LISPSYSTEM_ARGUMENT);
	str1 = ArgumentStruct(pos1);
	Check(str1->type != ArgumentType_generic, "type error");

	/* method-lambda-list */
	Return(stdget_method_lambda_list_(method, &pos2));
	CheckType(pos2, LISPSYSTEM_ARGUMENT);
	str2 = ArgumentStruct(pos2);
	Check(str2->type != ArgumentType_method, "type error");

	/* check */
	Return(method_arguments_check1_(str1, str2));
	Return(method_arguments_check2_(str1, str2));
	Return(method_arguments_check3_(str1, str2));
	Return(method_arguments_check4_(pos1, str1, pos2, str2));

	return 0;
}

static int method_eqlcheck_(addr method, addr *ret)
{
	int check;
	addr cons, spec;

	Return(stdget_method_specializers_(method, &method));
	for (cons = Nil; method != Nil; ) {
		GetCons(method, &spec, &method);
		Return(clos_specializer_p_(spec, &check));
		spec = check? T: Nil;
		cons_heap(&cons, spec, cons);
	}
	nreverse(ret, cons);

	return 0;
}

static int method_update_eqlcheck_p_(addr value, addr spec, int *ret)
{
	if (value != Nil)
		return Result(ret, 1);
	else
		return clos_specializer_p_(spec, ret);
}

static int method_update_eqlcheck_(addr gen, addr method, int deletep)
{
	int update, check;
	addr eqlcheck, value, specs, spec, next;

	Return(stdget_generic_eqlcheck_(gen, &eqlcheck));
	Return(stdget_method_specializers_(method, &specs));

	/* update eqlcheck */
	for (update = 0; eqlcheck != Nil; eqlcheck = next) {
		GetCons(eqlcheck, &value, &next);
		Check(specs == Nil, "method-specializers error");
		GetCons(specs, &spec, &specs);
		Return(method_update_eqlcheck_p_(value, spec, &check));
		spec = check? T: Nil;
		if (spec != value) {
			SetCar(eqlcheck, spec);
			update = 1;
		}
	}

	/* clear cache */
	if (deletep && update) {
		Return(stdget_generic_cache_(gen, &value));
		clear_hashtable(value);
	}

	return 0;
}

static int method_update_check_(addr gen, addr method, int deletep)
{
	int check;

	Return(stdboundp_generic_eqlcheck_(gen, &check));
	if (check) {
		Return(method_update_eqlcheck_(gen, method, deletep));
	}
	else {
		Return(method_eqlcheck_(method, &method));
		Return(stdset_generic_eqlcheck_(gen, method));
	}

	return 0;
}

static int method_push_generic_(Execute ptr, addr gen, addr method)
{
	addr methods, comb, qua, cons;
	size_t index;

	/* vector */
	Return(stdget_generic_vector_(gen, &methods));
	Return(stdget_generic_method_combination_(gen, &comb));
	Return(stdget_method_qualifiers_(method, &qua));
	Return(qualifiers_position_(ptr, qua, comb, &index));
	GetArrayA4(methods, index, &cons);
	cons_heap(&cons, method, cons);
	SetArrayA4(methods, index, cons);

	/* list */
	Return(stdget_generic_methods_(gen, &methods));
	pushnew_heap(methods, method, &methods);
	Return(stdset_generic_methods_(gen, methods));

	return 0;
}

static int method_cache_check_(addr eqlcheck, addr args, addr keys, int *ret)
{
	int every;
	addr check, arg, key;

	while (eqlcheck != Nil) {
		GetCons(eqlcheck, &check, &eqlcheck);
		Check(args == Nil, "args error");
		Check(keys == Nil, "keys error");
		GetCons(args, &arg, &args);
		GetCons(keys, &key, &keys);
		if (check != Nil) {
			Return(generic_eql_specializer_(key, arg, 1, &every));
		}
		else {
			Return(clos_subclass_p_(key, arg, &every));
		}
		if (! every)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int method_cache_remove_(LocalRoot local, addr gen, addr method)
{
	int check;
	addr eqlcheck, args, cache, key, keys;
	LocalStack stack;

	Return(stdboundp_generic_eqlcheck_(gen, &check));
	if (! check)
		return 0;
	Return(stdget_generic_cache_(gen, &cache));
	Return(stdget_generic_eqlcheck_(gen, &eqlcheck));
	Return(stdget_method_specializers_(method, &args));

	push_local(local, &stack);
	allkeys_hashtable_local(local, cache, &keys);
	while (keys != Nil) {
		GetCons(keys, &key, &keys);
		Return(method_cache_check_(eqlcheck, args, key, &check));
		if (check) {
			Return(delete_hashtable_(cache, key, &check));
		}
	}
	rollback_local(local, stack);

	return 0;
}

int method_find_method_nil_(Execute ptr, addr gen, addr qua, addr spec, addr *ret)
{
	int check;
	addr methods, comb, method, value;
	size_t index;

	Return(stdget_generic_vector_(gen, &methods));
	Return(stdget_generic_method_combination_(gen, &comb));
	Return(qualifiers_position_nil_(ptr, qua, comb, &index, &check));
	if (! check) {
		GetArrayA4(methods, index, &methods);
		while (methods != Nil) {
			GetCons(methods, &method, &methods);
			Return(stdget_method_specializers_(method, &value));
			Return(cache_equal_function_(spec, value, &check));
			if (check)
				return Result(ret, method);
		}
	}

	return Result(ret, Nil);
}

int method_find_method_(Execute ptr, addr gen, addr qua, addr spec, addr *ret)
{
	Return(method_find_method_nil_(ptr, gen, qua, spec, ret));
	if (*ret == Nil)
		return fmte_("No method found.", NULL);

	return 0;
}

int method_remove_method_unsafe_(Execute ptr, addr gen, addr method, int *ret)
{
	int check;
	addr methods, comb, qua, cons;
	size_t index;

	/* list */
	Return(stdget_generic_methods_(gen, &methods));
	(void)delete1_list_eq_unsafe(method, methods, &methods);
	Return(stdset_generic_methods_(gen, methods));

	/* vector */
	Return(stdget_generic_vector_(gen, &methods));
	Return(stdget_generic_method_combination_(gen, &comb));
	Return(stdget_method_qualifiers_(method, &qua));
	Return(qualifiers_position_nil_(ptr, qua, comb, &index, &check));
	if (check)
		return Result(ret, 0);
	GetArrayA4(methods, index, &cons);
	if (! delete1_list_eq_unsafe(method, cons, &cons))
		return Result(ret, 0);
	SetArrayA4(methods, index, cons);
	Return(stdset_method_generic_function_(method, Nil));

	return Result(ret, 1);
}

int method_remove_method_(Execute ptr, addr gen, addr method)
{
	int check;

	Return(method_remove_method_unsafe_(ptr, gen, method, &check));
	if (! check)
		return 0;
	Return(method_cache_remove_(ptr->local, gen, method));
	return generic_finalize_(gen);
}

static int method_replace_check_(Execute ptr, addr gen, addr method, addr *ret)
{
	addr qualifiers, specializers;

	Return(stdget_method_qualifiers_(method, &qualifiers));
	Return(stdget_method_specializers_(method, &specializers));
	return method_find_method_nil_(ptr, gen, qualifiers, specializers, ret);
}

static int method_add_replace_(Execute ptr,
		addr gen, addr method, addr check_method)
{
	int check;
	Return(method_remove_method_unsafe_(ptr, gen, check_method, &check));
	return method_push_generic_(ptr, gen, method);
}

static int method_add_check_(Execute ptr, addr gen, addr method)
{
	Return(method_check_generic_function_(gen, method));
	Return(method_check_method_class_(gen, method));
	Return(method_check_method_qualifiers_(ptr, gen, method));
	Return(method_check_method_arguments_(gen, method));

	return 0;
}

int method_add_method_(Execute ptr, addr gen, addr method)
{
	addr check_method;

	Return(method_add_check_(ptr, gen, method));
	Return(method_replace_check_(ptr, gen, method, &check_method));
	if (check_method != Nil) {
		Return(method_add_replace_(ptr, gen, method, check_method));
	}
	else {
		Return(method_update_check_(gen, method, 1));
		Return(method_push_generic_(ptr, gen, method));
		Return(stdset_method_generic_function_(method, gen));
	}
	Return(method_cache_remove_(ptr->local, gen, method));
	return generic_finalize_(gen);
}


/*
 *  common_objects
 */
#ifdef LISP_DEBUG
int common_method_add_(Execute ptr, addr gen, addr method)
{
	addr check_method;

	Check(! clos_generic_p_debug(gen), "generic error");
	Check(! clos_method_p_debug(method), "method error");
	Return(method_add_check_(ptr, gen, method));
	Return(method_replace_check_(ptr, gen, method, &check_method));
	if (check_method != Nil) {
		return fmte_("The method is already exists.", NULL);
	}
	else {
		Return(method_update_check_(gen, method, 1));
		Return(method_push_generic_(ptr, gen, method));
		Return(stdset_method_generic_function_(method, gen));
	}
	Return(method_cache_remove_(ptr->local, gen, method));
	return generic_finalize_(gen);
}
#else
int common_method_add_(Execute ptr, addr gen, addr method)
{
	Check(! clos_generic_p_debug(gen), "generic error");
	Check(! clos_method_p_debug(method), "method error");
	Return(method_push_generic_(ptr, gen, method));
	return stdset_method_generic_function_(method, gen);
}
#endif

static int defmethod_make_generic_function_(addr name, addr lambda, addr *ret)
{
	Check(! callnamep(name), "type error");
	Check(! argumentp(lambda), "type error");
	argument_method_to_generic(lambda, &lambda);
	return generic_make_empty_(name, lambda, ret);
}

int ensure_method_common_(Execute ptr, addr *ret,
		addr name, addr lambda, addr qua, addr spec, addr call)
{
	int check;
	addr gen, method, clos;
	LocalHold hold;

	Return(ensure_generic_function_name_(name, &name));
	getglobal_callname(name, &gen);
	if (! argumentp(lambda)) {
		Return(argument_method_heap_(ptr->local, &lambda, lambda));
	}
	if (gen == Unbound) {
		Return(defmethod_make_generic_function_(name, lambda, &gen));
	}
	Return(clos_generic_p_(gen, &check));
	if (! check)
		return fmte_("The function ~S is not generic-function.", gen, NULL);
	Return(stdget_generic_method_class_(gen, &clos));

	hold = LocalHold_array(ptr, 1);
	Return(method_instance_heap_(&method, clos, lambda, qua, spec, call));
	localhold_set(hold, 0, method);
	Return(method_add_method_(ptr, gen, method));
	localhold_end(hold);

	return Result(ret, method);
}

static int common_method_set_finalize_(addr gen)
{
	addr pos, list, method;
	size_t size, i;

	Return(stdget_generic_vector_(gen, &pos));
	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &list);
		while (list != Nil) {
			GetCons(list, &method, &list);
			Return(method_update_check_(gen, method, 0));
		}
	}
	return generic_finalize_(gen);
}

int common_method_finalize_(addr gen)
{
#ifdef LISP_DEBUG
	addr pos;

	/* eqlcheck clear */
	Return(stdset_generic_eqlcheck_(gen, Unbound));
	/* cache clear */
	Return(stdget_generic_cache_(gen, &pos));
	clear_hashtable(pos);
#endif
	/* build generic */
	return common_method_set_finalize_(gen);
}


/*
 *  document
 */
int methodget_document_(addr clos, addr *ret)
{
	Return(stdget_method_function_(clos, &clos));
	return get_documentation_function_object_(clos, ret);
}

int methodset_document_(addr clos, addr value)
{
	Return(stdget_method_function_(clos, &clos));
	return set_documentation_function_object_(clos, value);
}


/*
 *  common
 */
void method_make_method_lambda(addr list, addr env, addr *ret)
{
	/* `(lambda (,method ,next &rest ,args)
	 *    (flet ((next-method-p ()
	 *             (clos::flet-method-p ,next))
	 *           (call-next-method (&rest ,rest)
	 *             (clos::flet-next-method ,method ,next ,args ,rest)))
	 *      (declare (ignorable #'next-method-p #'call-next-method))
	 *      (apply (lambda ,lambda-list ,@form) ,args)))
	 */
	addr lambda, apply, next1, next2, call1, call2, a, b, c;
	addr method, next, args, rest, ignorable, declare, arest, flet;

	/* gensym */
	make_symbolchar(&method, "METHOD");
	make_symbolchar(&next, "NEXT");
	make_symbolchar(&args, "ARGS");
	make_symbolchar(&rest, "REST");
	/* constant */
	GetConst(COMMON_NEXT_METHOD_P, &next1);
	GetConst(CLOSNAME_FLET_METHOD_P, &next2);
	GetConst(COMMON_CALL_NEXT_METHOD, &call1);
	GetConst(CLOSNAME_FLET_NEXT_METHOD, &call2);
	/* apply */
	GetConst(COMMON_APPLY, &apply);
	list_heap(&apply, apply, list, args, NULL);
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
	list_heap(ret, lambda, method, flet, NULL);
}


/************************************************************
 *  clos_redefine.c
 ************************************************************/

enum ClosRedefine_Index {
	ClosRedefine_Add,
	ClosRedefine_Discard,
	ClosRedefine_Size
};

#define SetRedefine(x,i,y)		SetArrayA2((x),(i),(y))
#define GetRedefine(x,i,y)		GetArrayA2((x),(i),(y))

static void redefine_heap(addr *ret)
{
	heap_array2(ret, LISPSYSTEM_REDEFINE, ClosRedefine_Size);
}

static void getadd_redefine(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_REDEFINE);
	GetRedefine(pos, ClosRedefine_Add, ret);
}
static void setadd_redefine(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_REDEFINE);
	SetRedefine(pos, ClosRedefine_Add, value);
}

static void getdiscard_redefine(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_REDEFINE);
	GetRedefine(pos, ClosRedefine_Discard, ret);
}
static void setdiscard_redefine(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_REDEFINE);
	SetRedefine(pos, ClosRedefine_Discard, value);
}


/*
 *  redefine check
 */
static int clos_finalized_p_(addr pos, int *ret)
{
	Return(stdget_class_finalized_p_(pos, &pos));
	return Result(ret, pos != Nil);
}

static int clos_superclasses_referenced_p_(addr pos, int *ret)
{
	int check;
	addr list;

	Return(stdget_class_direct_superclasses_(pos, &list));
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(clos_referenced_p_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int clos_redefine_check_subclasses_(
		LocalRoot local, addr pos, addr x, addr list)
{
	addr root;

	/* class-precedence-list check */
	Return(clos_precedence_list_redefine_(local, pos, &root, x, list));
	/* subclasses */
	Return(stdget_class_direct_subclasses_(pos, &root));
	while (root != Nil) {
		Return_getcons(root, &pos, &root);
		Return(clos_redefine_check_subclasses_(local, pos, x, list));
	}

	return 0;
}

static int clos_redefine_slots_(addr pos, addr clos, addr slots1)
{
	/* slots1 -> slots2 */
	addr slots2, slot, name, list;
	size_t size1, size2, i;

	Return(stdget_class_slots_(clos, &slots2));
	LenSlotVector(slots1, &size1);
	LenSlotVector(slots2, &size2);

	/* added-slots */
	list = Nil;
	for (i = 0; i < size2; i++) {
		GetSlotVector(slots2, i, &slot);
		GetNameSlot(slot, &name);
		if (! clos_find_slotname(slots1, size1, name))
			cons_heap(&list, name, list);
	}
	nreverse(&list, list);
	setadd_redefine(pos, list);

	/* discarded-slots */
	list = Nil;
	for (i = 0; i < size1; i++) {
		GetSlotVector(slots1, i, &slot);
		GetNameSlot(slot, &name);
		if (! clos_find_slotname(slots2, size2, name))
			cons_heap(&list, name, list);
	}
	nreverse(&list, list);
	setdiscard_redefine(pos, list);

	return 0;
}

static int clos_redefine_information_(LocalRoot local, addr clos, addr slots)
{
	addr pos, value;

	/* redefined */
	redefine_heap(&pos);
	Return(clos_redefine_slots_(pos, clos, slots));
	Return(stdset_class_redefined_class_(clos, pos));
	/* increment */
	Return(stdget_class_version_(clos, &value));
	Return(oneplus_integer_common_(local, value, &value));
	Return(stdset_class_version_(clos, value));
	/* finalized_p */
	Return(stdset_class_finalized_p_(clos, Nil));

	return 0;
}

static int clos_redefine_delete_reader_(Execute ptr, addr clos, addr gen)
{
	addr spec, method;

	list_heap(&spec, clos, NULL);
	Return(generic_find_method_(ptr, gen, Nil, spec, Nil, &method));
	if (method != Nil) {
		Return(method_remove_method_(ptr, gen, method));
	}

	return 0;
}

static int clos_redefine_delete_readers_(Execute ptr, addr clos, addr list)
{
	addr gen, name;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		if (gen != Unbound) {
			Return(clos_redefine_delete_reader_(ptr, clos, gen));
		}
	}

	return 0;
}

static int clos_redefine_delete_writer_(Execute ptr, addr clos, addr gen)
{
	addr spec, method;

	GetConst(CLOS_T, &spec);
	list_heap(&spec, spec, clos, NULL);
	Return(generic_find_method_(ptr, gen, Nil, spec, Nil, &method));
	if (method != Nil) {
		Return(method_remove_method_(ptr, gen, method));
	}

	return 0;
}

static int clos_redefine_delete_writers_(Execute ptr, addr clos, addr list)
{
	addr gen, name;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		if (gen != Unbound) {
			Return(clos_redefine_delete_writer_(ptr, clos, gen));
		}
	}

	return 0;
}

static int clos_redefine_delete_accessor_(Execute ptr, addr clos, addr slots)
{
	addr list, pos;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		GetReadersSlot(pos, &list);
		Return(clos_redefine_delete_readers_(ptr, clos, list));
		GetWritersSlot(pos, &list);
		Return(clos_redefine_delete_writers_(ptr, clos, list));
	}

	return 0;
}

static int clos_redefine_superclasses_(addr clos, addr supers)
{
	addr list, x, y;

	/* delete subclases */
	Return(stdget_class_direct_superclasses_(clos, &list));
	while (list != Nil) {
		Return_getcons(list, &x, &list);
		Return(stdget_class_direct_subclasses_(x, &y));
		delete_list_eq_unsafe(clos, y, &y);
		Return(stdset_class_direct_subclasses_(x, y));
	}

	/* push subclasses */
	Return(stdset_class_direct_superclasses_(clos, supers));
	while (supers != Nil) {
		Return_getcons(supers, &x, &supers);
		Return(stdget_class_direct_subclasses_(x, &y));
		pushnew_heap(y, clos, &y);
		Return(stdset_class_direct_subclasses_(x, y));
	}

	return 0;
}

static int clos_redefine_update_subclasses_(
		LocalRoot local, addr clos, addr x, addr list)
{
	addr root, slots;

	/* update */
	Return(stdget_class_slots_(clos, &slots));
	Return(clos_ensure_class_init_(local, clos, 0));
	/* subclasses */
	Return(stdget_class_direct_subclasses_(clos, &root));
	while (root != Nil) {
		Return_getcons(root, &clos, &root);
		Return(clos_redefine_update_subclasses_(local, clos, x, list));
	}
	/* version increment */
	return clos_redefine_information_(local, clos, slots);
}

static int clos_redefine_finalized_(Execute ptr, addr clos, addr name, addr rest)
{
	addr supers, prev_slots, slots, value;
	LocalRoot local;

	/* class-precedence-list check */
	local = ptr->local;
	Return(stdget_class_slots_(clos, &prev_slots));
	Return(clos_ensure_class_supers_(rest, &supers, NULL));
	Return(clos_ensure_class_slots_(rest, &slots));
	Return(clos_ensure_class_direct_default_initargs_(local, clos, rest, &value));
	Return(clos_redefine_check_subclasses_(local, clos, clos, supers));
	/* update redefine */
	Return(clos_stdclass_direct_slots_(clos, slots));
	Return(clos_redefine_delete_accessor_(ptr, clos, prev_slots));
	Return(clos_redefine_superclasses_(clos, supers));
	Return(stdset_class_direct_default_initargs_(clos, value));
	Return(clos_redefine_update_subclasses_(local, clos, clos, supers));

	return 0;
}

static int clos_redefine_reference_(Execute ptr, addr clos, addr name, addr rest)
{
	int referp;
	addr supers, slots, value;
	LocalRoot local;

	/* class-precedence-list check */
	local = ptr->local;
	Return(clos_ensure_class_supers_(rest, &supers, &referp));
	Return(clos_ensure_class_slots_(rest, &slots));
	Return(clos_ensure_class_direct_default_initargs_(local, clos, rest, &value));
	/* update redefine */
	Return(clos_stdclass_direct_slots_(clos, slots));
	Return(clos_redefine_superclasses_(clos, supers));
	Return(stdset_class_direct_default_initargs_(clos, value));

	return 0;
}

static int clos_redefine_make_(Execute ptr, addr clos, addr name, addr rest)
{
	int check;

	Return(clos_superclasses_referenced_p_(clos, &check));
	if (check)
		return clos_redefine_reference_(ptr, clos, name, rest);
	else
		return clos_redefine_finalized_(ptr, clos, name, rest);
}

static int clos_redefine_make_instances_obsolete_(Execute ptr, addr clos)
{
	addr call;

	GetConst(COMMON_MAKE_INSTANCES_OBSOLETE, &call);
	Return(getfunction_global_(call, &call));
	return funcall1_control_(ptr, &call, call, clos, NULL);
}

static int clos_redefine_finalize_(Execute ptr, addr clos, addr name, addr rest)
{
	int check;

	/* finalize check */
	Return(clos_finalized_p_(clos, &check));
	Return(clos_redefine_make_(ptr, clos, name, rest));

	/* finalize */
	if (check) {
		Return(clos_finalize_(ptr, clos, &check));
		if (check)
			return fmte_("Cannot finalize class object ~S.", clos, NULL);
		return clos_redefine_make_instances_obsolete_(ptr, clos);
	}

	return 0;
}

int clos_ensure_class_redefine_(Execute ptr, addr clos, addr name, addr rest)
{
	int check;
	addr metaclass, pos;

	/* readonly check */
	if (GetStatusReadOnly(clos)) {
		return call_type_error_va_(ptr, clos, Nil,
				"Cannot redefine the object ~S that is a constant.", clos, NULL);
	}

	/* metaclass check */
	Return(clos_class_of_(clos, &metaclass));
	if (! GetKeyArgs(rest, KEYWORD_METACLASS, &pos)) {
		Return(clos_find_class_(pos, &pos));
		if (metaclass != pos)
			return fmte_("Cannot change the metaclass in class ~S.", clos, NULL);
	}

	/* standard-class only */
	check = clos_standard_class_p(metaclass);
	if (! check) {
		return fmte_("This implementation can only redefine a STANDARD-CLASS. "
				"(~S, ~S)", clos, name, NULL);
	}

	/* make-instance */
	return clos_redefine_finalize_(ptr, clos, name, rest);
}


/*
 *  version
 */
static int getproperty_redefine_(addr pos, addr *ret)
{
	addr slots, root, slot, name, check;
	size_t size, i;

	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	root = Nil;
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetNameSlot(slot, &name);
		Return(clos_get_(pos, name, &check));
		if (check != Unbound) {
			cons_heap(&root, name, root);
			cons_heap(&root, check, root);
		}
	}
	nreverse(ret, root);

	return 0;
}

static void clos_redefined_set_value(addr slots, addr values, addr x, addr v)
{
	addr pos, check;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		GetNameSlot(pos, &check);
		if (check == x) {
			SetClosValue(values, i, v);
			break;
		}
	}
}

static int clos_redefined_instance_(addr pos, addr clos)
{
	addr pslots, pvalues, slots, values, x, v;
	size_t size, i;
	fixnum version;

	GetSlotClos(pos, &pslots);
	GetValueClos(pos, &pvalues);

	/* version */
	Return(stdget_class_version_(clos, &x));
	GetFixnum(x, &version);
	SetVersionClos(pos, version);

	/* update */
	Return(stdget_class_slots_(clos, &slots));
	LenSlotVector(slots, &size);
	clos_value_heap(&values, size);
	SetSlotClos(pos, slots);
	SetValueClos(pos, values);

	/* set values */
	LenSlotVector(pslots, &size);
	for (i = 0; i < size; i++) {
		GetClosValue(pvalues, i, &v);
		if (v != Unbound) {
			GetSlotVector(pslots, i, &x);
			GetNameSlot(x, &x);
			clos_redefined_set_value(slots, values, x, v);
		}
	}

	return 0;
}

static int clos_redefined_class_(Execute ptr, addr pos, addr clos)
{
	/* (update-instance-for-redefined-class
	 *     instance added-slots discarded-slots property-list
	 *     &rest initargs &key &allow-other-keys)
	 */
	addr call, add, del, prop;

	/* update instance */
	Return(getproperty_redefine_(pos, &prop));
	Return(clos_redefined_instance_(pos, clos));

	/* argument */
	Return(stdget_class_redefined_class_(clos, &clos));
	getadd_redefine(clos, &add);
	getdiscard_redefine(clos, &del);

	/* call update-instance-for-redefined-class */
	GetConst(COMMON_UPDATE_INSTANCE_FOR_REDEFINED_CLASS, &call);
	Return(getfunction_global_(call, &call));
	return funcall1_control_(ptr, &call, call, pos, add, del, prop, NULL);
}

int clos_version_diff_p_(addr pos, int *ret)
{
	addr clos, check;
	fixnum a, b;

	Return(clos_class_of_(pos, &clos));
	GetVersionClos(pos, &a);
	Return(stdget_class_version_(clos, &check));
	GetFixnum(check, &b);

	return Result(ret, a != b);
}

int clos_version_check_(Execute ptr, addr pos, addr clos)
{
	addr check;
	fixnum a, b;

	GetVersionClos(pos, &a);
	Return(stdget_class_version_(clos, &check));
	GetFixnum(check, &b);
	if (a == b)
		return 0;
	Return(clos_redefined_class_(ptr, pos, clos));
	SetVersionClos(pos, b);

	return 0;
}


/*
 *  update-instance-for-redefined-class
 */
static int clos_redefine_method_find(addr pos, addr key)
{
	addr list, check;
	size_t size, i;

	GetSlotClos(pos, &pos);
	LenSlotVector(pos, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(pos, i, &list);
		GetArgsSlot(list, &list);
		while (list != Nil) {
			GetCons(list, &check, &list);
			if (check == key)
				return 1;
		}
	}

	return 0;
}

static int clos_redefine_method_initargs_(Execute ptr, addr pos, addr rest)
{
	addr key;

	while (rest != Nil) {
		if (! consp_getcons(rest, &key, &rest))
			goto error;
		if (! consp_getcdr(rest, &rest))
			goto error;
		if (! clos_redefine_method_find(pos, key))
			return fmte_("There is no name ~S in the initargs.", key, NULL);
	}
	return 0;

error:
	return fmte_("Invalid &key arguments, ~S.", rest, NULL);
}

int clos_redefine_method_(Execute ptr,
		addr pos, addr add, addr del, addr prop, addr rest)
{
	addr call;

	/* initargs */
	Return(clos_redefine_method_initargs_(ptr, pos, rest));

	/* (shared-initialize ...) */
	GetConst(COMMON_SHARED_INITIALIZE, &call);
	Return(getfunction_global_(call, &call));
	return applya_control_(ptr, call, pos, add, rest, NULL);
}


/*
 *  change-class
 */
static int clos_change_class_find(addr copy, addr name, addr *ret)
{
	addr slots, array, value;
	size_t size, i;

	GetValueClos(copy, &array);
	GetSlotClos(copy, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &value);
		GetNameSlot(value, &value);
		if (name == value) {
			GetClosValue(array, i, ret);
			return 1;
		}
	}

	return 0;
}

static int clos_change_class_update_(Execute ptr, addr copy, addr pos, addr rest)
{
	addr call, slots, array, value;
	size_t size, i;

	/* new slots */
	GetValueClos(pos, &array);
	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &value);
		GetNameSlot(value, &value);
		if (clos_change_class_find(copy, value, &value)) {
			SetClosValue(array, i, value);
		}
	}

	/* update-instance-for-different-class */
	GetConst(COMMON_UPDATE_INSTANCE_FOR_DIFFERENT_CLASS, &call);
	Return(getfunction_global_(call, &call));
	return applya1_control_(ptr, &call, call, copy, pos, rest, NULL);
}

int clos_change_class_(Execute ptr, addr pos, addr clos, addr rest)
{
	addr copy, type, rollback;
	LocalHold hold;

	/* readonly check */
	GetConst(CLOS_BUILT_IN_CLASS, &type);
	if (clos == type) {
		return call_type_error_va_(ptr, clos, Nil,
				"Cannot change the object ~S to the built-in-class.", pos, NULL);
	}

	/* copy */
	hold = LocalHold_array(ptr, 1);
	clos_copy_alloc(ptr->local, pos, &rollback);
	Return(allocate_instance_standard_(ptr, clos, &copy));
	clos_swap(copy, pos);
	localhold_set(hold, 0, copy);

	/* update */
	if (clos_change_class_update_(ptr, copy, pos, rest)) {
		clos_swap(pos, rollback);
		return 1;
	}

	/* destroy copy instance */
	clos_destroy(copy);
	localhold_end(hold);

	return 0;
}


/*
 *  update-instance-for-different-class
 */
static void clos_change_method_slots(Execute ptr, addr copy, addr pos, addr *ret)
{
	addr slots, check, ignore, list;
	size_t size, i;

	/* new slots */
	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &check);
		GetNameSlot(check, &check);
		if (! clos_change_class_find(copy, check, &ignore))
			cons_heap(&list, check, list);
	}
	nreverse(ret, list);
}

int clos_change_method_(Execute ptr, addr copy, addr pos, addr rest)
{
	addr list, call;
	LocalHold hold;

	/* slots */
	Return(clos_redefine_method_initargs_(ptr, pos, rest));
	clos_change_method_slots(ptr, copy, pos, &list);

	/* (shared-initialize ...) */
	hold = LocalHold_local_push(ptr, list);
	GetConst(COMMON_SHARED_INITIALIZE, &call);
	Return(getfunction_global_(call, &call));
	Return(applya_control_(ptr, call, pos, list, rest, NULL));
	localhold_end(hold);

	return 0;
}


/************************************************************
 *  clos_slot.c
 ************************************************************/

/*
 *  access
 */
struct slot_struct *struct_slot(addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	return SlotStruct_Low(pos);
}

void getname_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetNameSlot_Low(pos, ret);
}

void setname_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetNameSlot_Low(pos, value);
}

void gettype_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetTypeSlot_Low(pos, ret);
}

void settype_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetTypeSlot_Low(pos, value);
}

void getargs_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetArgsSlot_Low(pos, ret);
}

void setargs_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetArgsSlot_Low(pos, value);
}

void getform_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetFormSlot_Low(pos, ret);
}

void setform_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFormSlot_Low(pos, value);
}

void getfunction_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetFunctionSlot_Low(pos, ret);
}

void setfunction_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFunctionSlot_Low(pos, value);
}

void getreaders_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetReadersSlot_Low(pos, ret);
}

void setreaders_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetReadersSlot_Low(pos, value);
}

void getwriters_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetWritersSlot_Low(pos, ret);
}

void setwriters_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetWritersSlot_Low(pos, value);
}

void getdocument_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetDocumentSlot_Low(pos, ret);
}

void setdocument_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDocumentSlot_Low(pos, value);
}

void getclass_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetClassSlot_Low(pos, ret);
}

void setclass_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetClassSlot_Low(pos, value);
}

void getreadonly_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetReadOnlySlot_Low(pos, ret);
}

void setreadonly_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetReadOnlySlot_Low(pos, value);
}

void getallocation_slot(addr pos, int *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetAllocationSlot_Low(pos, ret);
}

void setallocation_slot(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetAllocationSlot_Low(pos, value);
}

void getlocation_slot(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetLocationSlot_Low(pos, ret);
}

void setlocation_slot(addr pos, size_t value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetLocationSlot_Low(pos, value);
}

void getaccess_slot(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetAccessSlot_Low(pos, ret);
}

void setaccess_slot(addr pos, size_t value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetAccessSlot_Low(pos, value);
}


/*
 *  slot
 */
int slotp(addr pos)
{
	return GetType(pos) == LISPSYSTEM_SLOT;
}

static inline void slot_unsafe(LocalRoot local, addr *ret)
{
	alloc_smallsize(local, ret,
			LISPSYSTEM_SLOT, SLOT_INDEX_SIZE, sizeoft(struct slot_struct));
}
void slot_alloc(LocalRoot local, addr *ret)
{
	addr pos;

	slot_unsafe(local, &pos);
	SetAllocationSlot_Low(pos, 0);
	SetLocationSlot_Low(pos, 0);
	SetAccessSlot_Low(pos, 0);
	SetNameSlot_Low(pos, Unbound);
	SetFormSlot_Low(pos, Unbound);
	*ret = pos;
}
void slot_local(LocalRoot local, addr *ret)
{
	CheckLocal(local);
	slot_alloc(local, ret);
}
void slot_heap(addr *ret)
{
	slot_alloc(NULL, ret);
}

void slot_copy_alloc(LocalRoot local, addr *ret, addr slot)
{
	int check;
	addr pos, value;
	struct slot_struct *str1, *str2;
	size_t i;

	CheckType(slot, LISPSYSTEM_SLOT);
	slot_unsafe(local, &pos);

	/* value */
	str1 = SlotStruct_Low(slot);
	str2 = SlotStruct_Low(pos);
	str2->location = str1->location;
	str2->access = str1->access;
	GetAllocationSlot_Low(slot, &check);
	SetAllocationSlot_Low(pos, check);

	/* array */
	for (i = 0; i < SLOT_INDEX_SIZE; i++) {
		GetArraySS(slot, i, &value);
		SetArraySS(pos, i, value);
	}

	/* result */
	*ret = pos;
}
void slot_copy_local(LocalRoot local, addr *ret, addr slot)
{
	CheckLocal(local);
	slot_copy_alloc(local, ret, slot);
}
void slot_copy_heap(addr *ret, addr slot)
{
	slot_copy_alloc(NULL, ret, slot);
}


/************************************************************
 *  clos_type.c
 ************************************************************/

/*
 *  class-of
 */
typedef int (*class_of_calltype)(addr object, addr *ret);
static class_of_calltype class_of_call[LISPTYPE_SIZE];

static int class_of_error_(addr object, addr *ret)
{
	infobit(object);
	return fmte_("TYPE ~S cannot convert class type.", object, NULL);
}

static int class_of_nil_(addr object, addr *ret)
{
	GetConst(CLOS_NULL, ret);
	return 0;
}

static int class_of_cons_(addr object, addr *ret)
{
	GetConst(CLOS_CONS, ret);
	return 0;
}

static int class_of_array_(addr object, addr *ret)
{
	int check;

	/* bit-vector */
	if (bitvectorp(object)) {
		if (array_simple_p(object))
			GetConst(CLOS_SIMPLE_BIT_VECTOR, ret);
		else
			GetConst(CLOS_BIT_VECTOR, ret);
		return 0;
	}

	/* string */
	Return(strarray_base_p_(object, &check));
	if (check) {
		if (array_simple_p(object))
			GetConst(CLOS_SIMPLE_BASE_STRING, ret);
		else
			GetConst(CLOS_BASE_STRING, ret);
		return 0;
	}
	if (strarrayp(object)) {
		if (array_simple_p(object))
			GetConst(CLOS_SIMPLE_STRING, ret);
		else
			GetConst(CLOS_STRING, ret);
		return 0;
	}

	/* vector */
	if (array_vector_p(object)) {
		if (array_simple_p(object))
			GetConst(CLOS_SIMPLE_VECTOR, ret);
		else
			GetConst(CLOS_VECTOR, ret);
		return 0;
	}

	/* array */
	if (array_simple_p(object))
		GetConst(CLOS_SIMPLE_ARRAY, ret);
	else
		GetConst(CLOS_ARRAY, ret);
	return 0;
}

static int class_of_vector_(addr object, addr *ret)
{
	GetConst(CLOS_SIMPLE_VECTOR, ret);
	return 0;
}

static int class_of_character_(addr object, addr *ret)
{
	GetConst(CLOS_CHARACTER, ret);
	return 0;
}

static int class_of_string_(addr object, addr *ret)
{
	int check;

	Return(strvect_base_p_(object, &check));
	if (check)
		GetConst(CLOS_SIMPLE_BASE_STRING, ret);
	else
		GetConst(CLOS_SIMPLE_STRING, ret);

	return 0;
}

static int class_of_hashtable_(addr object, addr *ret)
{
	GetConst(CLOS_HASH_TABLE, ret);
	return 0;
}

static int class_of_readtable_(addr object, addr *ret)
{
	GetConst(CLOS_READTABLE, ret);
	return 0;
}

static int class_of_symbol_(addr object, addr *ret)
{
	if (keywordp(object))
		GetConst(CLOS_KEYWORD, ret);
	else
		GetConst(CLOS_SYMBOL, ret);
	return 0;
}

static int class_of_fixnum_(addr object, addr *ret)
{
	GetConst(CLOS_FIXNUM, ret);
	return 0;
}

static int class_of_bignum_(addr object, addr *ret)
{
	GetConst(CLOS_BIGNUM, ret);
	return 0;
}

static int class_of_ratio_(addr object, addr *ret)
{
	GetConst(CLOS_RATIO, ret);
	return 0;
}

static int class_of_short_float_(addr object, addr *ret)
{
	GetConst(CLOS_SHORT_FLOAT, ret);
	return 0;
}

static int class_of_single_float_(addr object, addr *ret)
{
	GetConst(CLOS_SINGLE_FLOAT, ret);
	return 0;
}

static int class_of_double_float_(addr object, addr *ret)
{
	GetConst(CLOS_DOUBLE_FLOAT, ret);
	return 0;
}

static int class_of_long_float_(addr object, addr *ret)
{
	GetConst(CLOS_LONG_FLOAT, ret);
	return 0;
}

static int class_of_complex_(addr object, addr *ret)
{
	GetConst(CLOS_COMPLEX, ret);
	return 0;
}

static int class_of_function_(addr object, addr *ret)
{
	if (compiled_function_p(object))
		GetConst(CLOS_COMPILED_FUNCTION, ret);
	else
		GetConst(CLOS_FUNCTION, ret);
	return 0;
}

static int class_of_package_(addr object, addr *ret)
{
	GetConst(CLOS_PACKAGE, ret);
	return 0;
}

static int class_of_random_state_(addr object, addr *ret)
{
	GetConst(CLOS_RANDOM_STATE, ret);
	return 0;
}

static int class_of_pathname_(addr object, addr *ret)
{
	if (pathname_logical_p(object))
		GetConst(CLOS_LOGICAL_PATHNAME, ret);
	else
		GetConst(CLOS_PATHNAME, ret);
	return 0;
}

static int class_of_stream_(addr object, addr *ret)
{
	switch (getstreamtype(object)) {
		case StreamType_BinaryInput:
		case StreamType_BinaryOutput:
		case StreamType_BinaryIO:
		case StreamType_CharacterInput:
		case StreamType_CharacterOutput:
		case StreamType_CharacterIO:
		case StreamType_BincharInput:
		case StreamType_BincharOutput:
		case StreamType_BincharIO:
		case StreamType_Probe:
			GetConst(CLOS_FILE_STREAM, ret);
			break;

		case StreamType_StringInput:
		case StreamType_StringOutput:
			GetConst(CLOS_STRING_STREAM, ret);
			break;

		case StreamType_Synonym:
			GetConst(CLOS_SYNONYM_STREAM, ret);
			break;

		case StreamType_BroadCast:
			GetConst(CLOS_BROADCAST_STREAM, ret);
			break;

		case StreamType_Concatenated:
			GetConst(CLOS_CONCATENATED_STREAM, ret);
			break;

		case StreamType_TwoWay:
			GetConst(CLOS_TWO_WAY_STREAM, ret);
			break;

		case StreamType_Echo:
			GetConst(CLOS_ECHO_STREAM, ret);
			break;

		default:
			GetConst(CLOS_STREAM, ret);
			break;
	}

	return 0;
}

static int class_of_restart_(addr object, addr *ret)
{
	GetConst(CLOS_RESTART, ret);
	return 0;
}

static int class_of_bit_vector_(addr object, addr *ret)
{
	GetConst(CLOS_SIMPLE_BIT_VECTOR, ret);
	return 0;
}

static int class_of_paper_(addr object, addr *ret)
{
	GetConst(CLOS_PAPER, ret);
	return 0;
}

void init_clos_type(void)
{
	size_t i;

	/* error */
	for (i = 0; i < (size_t)LISPTYPE_SIZE; i++)
		class_of_call[i] = class_of_error_;

	/* class-of */
	class_of_call[LISPTYPE_NIL] = class_of_nil_;
	class_of_call[LISPTYPE_T] = class_of_symbol_;
	class_of_call[LISPTYPE_CONS] = class_of_cons_;
	class_of_call[LISPTYPE_ARRAY] = class_of_array_;
	class_of_call[LISPTYPE_VECTOR] = class_of_vector_;
	class_of_call[LISPTYPE_CHARACTER] = class_of_character_;
	class_of_call[LISPTYPE_STRING] = class_of_string_;
	class_of_call[LISPTYPE_HASHTABLE] = class_of_hashtable_;
	class_of_call[LISPTYPE_READTABLE] = class_of_readtable_;
	class_of_call[LISPTYPE_SYMBOL] = class_of_symbol_;
	class_of_call[LISPTYPE_FIXNUM] = class_of_fixnum_;
	class_of_call[LISPTYPE_BIGNUM] = class_of_bignum_;
	class_of_call[LISPTYPE_RATIO] = class_of_ratio_;
	class_of_call[LISPTYPE_SHORT_FLOAT] = class_of_short_float_;
	class_of_call[LISPTYPE_SINGLE_FLOAT] = class_of_single_float_;
	class_of_call[LISPTYPE_DOUBLE_FLOAT] = class_of_double_float_;
	class_of_call[LISPTYPE_LONG_FLOAT] = class_of_long_float_;
	class_of_call[LISPTYPE_COMPLEX] = class_of_complex_;
	class_of_call[LISPTYPE_FUNCTION] = class_of_function_;
	class_of_call[LISPTYPE_PACKAGE] = class_of_package_;
	class_of_call[LISPTYPE_RANDOM_STATE] = class_of_random_state_;
	class_of_call[LISPTYPE_PATHNAME] = class_of_pathname_;
	class_of_call[LISPTYPE_STREAM] = class_of_stream_;
	class_of_call[LISPTYPE_RESTART] = class_of_restart_;
	class_of_call[LISPTYPE_BITVECTOR] = class_of_bit_vector_;
	class_of_call[LISPTYPE_PAPER] = class_of_paper_;
}

int clos_class_of_(addr object, addr *ret)
{
	enum LISPTYPE type;

	type = GetType(object);
	if (type == LISPTYPE_CLOS) {
		/* clos or structure */
		GetClassOfClos(object, ret);
		return 0;
	}

	/* built-in-class */
	return (class_of_call[(size_t)type])(object, ret);
}


/*
 *  specializer
 */
int clos_intern_specializer_(addr object, addr *ret)
{
	addr pos, type;

	Return(clos_find_specializer_nil_(object, &pos));
	if (pos != Nil)
		return Result(ret, pos);

	/* make eql-specializer */
	GetConst(CLOS_EQL_SPECIALIZER, &pos);
	Return(clos_instance_heap_(pos, &pos));
	/* define eql-specializer */
	Return(clos_class_of_(object, &type));
	Return(stdset_specializer_object_(pos, object));
	Return(stdset_specializer_type_(pos, type));
	Return(clos_define_specializer_(object, pos));
	/* result */
	return Result(ret, pos);
}


/************************************************************
 *  cmpl.c
 ************************************************************/

int complexp(addr pos)
{
	return GetType(pos) == LISPTYPE_COMPLEX;
}

void setreal_complex(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetRealComplex_Low(pos, value);
}

void getreal_complex(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	GetRealComplex_Low(pos, ret);
}

void setimag_complex(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetImagComplex_Low(pos, value);
}

void getimag_complex(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	GetImagComplex_Low(pos, ret);
}

void settype_complex(addr pos, int value)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetTypeComplex_Low(pos, value);
}

enum ComplexType gettype_complex(addr pos)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	return GetTypeComplex_Low(pos);
}

enum ComplexType getcomplex(addr pos, enum ComplexType *type, addr *real, addr *imag)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	*type = GetTypeComplex_Low(pos);
	GetRealComplex_Low(pos, real);
	GetImagComplex_Low(pos, imag);
	return *type;
}

enum ComplexType getcomplexr(addr pos, addr *real, addr *imag)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	GetRealComplex_Low(pos, real);
	GetImagComplex_Low(pos, imag);
	return GetTypeComplex_Low(pos);
}


/*
 *  rational complex  -> #c(1/2 3)
 *  float complex     -> #c(0.5 3.0)
 */
void make_complex_unsafe(LocalRoot local, addr *ret, enum ComplexType type)
{
	alloc_array2(local, ret, LISPTYPE_COMPLEX, 2);
	SetTypeComplex(*ret, type);
}

static int complex_unsafe_alloc_(LocalRoot local, addr *ret,
		addr real, addr imag, enum ComplexType type)
{
	addr pos;

	Return(real_throw_alloc_(local, real, &real));
	Return(real_throw_alloc_(local, imag, &imag));
	make_complex_unsafe(local, &pos, type);
	SetRealComplex(pos, real);
	SetImagComplex(pos, imag);

	return Result(ret, pos);
}

static int complex_unsafe_heap_(addr *ret,
		addr real, addr imag, enum ComplexType type)
{
	return complex_unsafe_alloc_(NULL, ret, real, imag, type);
}

static int complex_fixnum_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_SINGLE_FLOAT:
			single_float_alloc(local, &real, (single_float)RefFixnum(real));
			single_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_fixnum_alloc(local, &real, real);
			double_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_LONG_FLOAT:
			long_float_fixnum_alloc(local, &real, real);
			long_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_long);

		case LISPTYPE_FIXNUM:
			if (zerop_fixnum(imag)) {
				fixnum_throw_alloc(local, real, ret);
			}
			else {
				fixnum_throw_alloc(local, real, &real);
				fixnum_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		case LISPTYPE_BIGNUM:
			if (zerop_bignum(imag)) {
				fixnum_throw_alloc(local, real, ret);
			}
			else {
				fixnum_throw_alloc(local, real, &real);
				bignum_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		case LISPTYPE_RATIO:
			if (zerop_ratio(imag)) {
				fixnum_throw_alloc(local, real, ret);
			}
			else {
				fixnum_throw_alloc(local, real, &real);
				ratio_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		default:
			*ret = Nil;
			return TypeError_(imag, REAL);
	}
}

static int complex_bignum_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_SINGLE_FLOAT:
			Return(single_float_bignum_alloc_(local, &real, real));
			single_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

		case LISPTYPE_DOUBLE_FLOAT:
			Return(double_float_bignum_alloc_(local, &real, real));
			double_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_LONG_FLOAT:
			Return(long_float_bignum_alloc_(local, &real, real));
			long_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_long);

		case LISPTYPE_FIXNUM:
			if (zerop_fixnum(imag)) {
				bignum_throw_alloc(local, real, ret);
			}
			else {
				bignum_throw_alloc(local, real, &real);
				fixnum_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		case LISPTYPE_BIGNUM:
			if (zerop_bignum(imag)) {
				bignum_throw_alloc(local, real, ret);
			}
			else {
				bignum_throw_alloc(local, real, &real);
				bignum_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		case LISPTYPE_RATIO:
			if (zerop_ratio(imag)) {
				bignum_throw_alloc(local, real, ret);
			}
			else {
				bignum_throw_alloc(local, real, &real);
				ratio_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		default:
			*ret = Nil;
			return TypeError_(imag, REAL);
	}
}

static int complex_ratio_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_SINGLE_FLOAT:
			Return(single_float_ratio_alloc_(local, &real, real));
			single_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

		case LISPTYPE_DOUBLE_FLOAT:
			Return(double_float_ratio_alloc_(local, &real, real));
			double_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_LONG_FLOAT:
			Return(long_float_ratio_alloc_(local, &real, real));
			long_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_long);

		case LISPTYPE_FIXNUM:
			if (zerop_fixnum(imag)) {
				ratio_throw_alloc(local, real, ret);
			}
			else {
				ratio_throw_alloc(local, real, &real);
				fixnum_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		case LISPTYPE_BIGNUM:
			if (zerop_bignum(imag)) {
				ratio_throw_alloc(local, real, ret);
			}
			else {
				ratio_throw_alloc(local, real, &real);
				bignum_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		case LISPTYPE_RATIO:
			if (zerop_ratio(imag)) {
				ratio_throw_alloc(local, real, ret);
			}
			else {
				ratio_throw_alloc(local, real, &real);
				ratio_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		default:
			*ret = Nil;
			return TypeError_(imag, REAL);
	}

	return 0;
}

static int complex_single_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_SINGLE_FLOAT:
			single_float_throw_alloc(local, real, &real);
			single_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_alloc(local, &real, (double_float)RefSingleFloat(real));
			double_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_LONG_FLOAT:
			long_float_alloc(local, &real, (long_float)RefSingleFloat(real));
			long_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_long);

		case LISPTYPE_FIXNUM:
			single_float_throw_alloc(local, real, &real);
			single_float_alloc(local, &imag, (single_float)RefFixnum(imag));
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

		case LISPTYPE_BIGNUM:
			single_float_throw_alloc(local, real, &real);
			Return(single_float_bignum_alloc_(local, &imag, imag));
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

		case LISPTYPE_RATIO:
			single_float_throw_alloc(local, real, &real);
			Return(single_float_ratio_alloc_(local, &imag, imag));
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

		default:
			*ret = Nil;
			return TypeError_(imag, REAL);
	}

	return 0;
}

static int complex_double_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_SINGLE_FLOAT:
			double_float_throw_alloc(local, real, &real);
			double_float_alloc(local, &imag, (double_float)RefSingleFloat(imag));
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_throw_alloc(local, real, &real);
			double_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_LONG_FLOAT:
			long_float_alloc(local, &real, (long_float)RefDoubleFloat(real));
			long_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_long);

		case LISPTYPE_FIXNUM:
			double_float_throw_alloc(local, real, &real);
			double_float_fixnum_alloc(local, &imag, imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_BIGNUM:
			double_float_throw_alloc(local, real, &real);
			Return(double_float_bignum_alloc_(local, &imag, imag));
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_RATIO:
			double_float_throw_alloc(local, real, &real);
			Return(double_float_ratio_alloc_(local, &imag, imag));
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		default:
			*ret = Nil;
			return TypeError_(imag, REAL);
	}

	return 0;
}

static int complex_long_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_SINGLE_FLOAT:
			long_float_throw_alloc(local, real, &real);
			long_float_alloc(local, &imag, (long_float)RefSingleFloat(imag));
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			long_float_throw_alloc(local, real, &real);
			long_float_alloc(local, &imag, (long_float)RefDoubleFloat(imag));
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_throw_alloc(local, real, &real);
			long_float_throw_alloc(local, imag, &imag);
			break;

		case LISPTYPE_FIXNUM:
			long_float_throw_alloc(local, real, &real);
			long_float_fixnum_alloc(local, &imag, imag);
			break;

		case LISPTYPE_BIGNUM:
			long_float_throw_alloc(local, real, &real);
			Return(long_float_bignum_alloc_(local, &imag, imag));
			break;

		case LISPTYPE_RATIO:
			long_float_throw_alloc(local, real, &real);
			Return(long_float_ratio_alloc_(local, &imag, imag));
			break;

		default:
			*ret = Nil;
			return TypeError_(imag, REAL);
	}

	return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_long);
}

static int complex2_alloc_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(real)) {
		case LISPTYPE_SINGLE_FLOAT:
			return complex_single_(local, ret, real, imag);

		case LISPTYPE_DOUBLE_FLOAT:
			return complex_double_(local, ret, real, imag);

		case LISPTYPE_LONG_FLOAT:
			return complex_long_(local, ret, real, imag);

		case LISPTYPE_FIXNUM:
			return complex_fixnum_(local, ret, real, imag);

		case LISPTYPE_BIGNUM:
			return complex_bignum_(local, ret, real, imag);

		case LISPTYPE_RATIO:
			return complex_ratio_(local, ret, real, imag);

		default:
			*ret = Nil;
			return TypeError_(real, REAL);
	}
}

static int complex1_alloc_(LocalRoot local, addr *ret, addr real)
{
	addr imag;

	switch (GetType(real)) {
		case LISPTYPE_SINGLE_FLOAT:
			single_float_throw_alloc(local, real, &real);
			single_float_alloc(local, &imag, 0.0f);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_throw_alloc(local, real, &real);
			double_float_alloc(local, &imag, 0.0);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_LONG_FLOAT:
			long_float_throw_alloc(local, real, &real);
			long_float_alloc(local, &imag, 0.0L);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_long);

		case LISPTYPE_FIXNUM:
			fixnum_throw_alloc(local, real, ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_alloc(local, real, ret);
			break;

		case LISPTYPE_RATIO:
			ratio_throw_alloc(local, real, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(real, REAL);
	}

	return 0;
}

int complex_alloc_(LocalRoot local, addr *ret, addr real, addr imag)
{
	if (imag == Unbound)
		return complex1_alloc_(local, ret, real);
	else
		return complex2_alloc_(local, ret, real, imag);
}
int complex_local_(LocalRoot local, addr *ret, addr real, addr imag)
{
	Check(local == NULL, "local error");
	return complex_alloc_(local, ret, real, imag);
}
int complex_heap_(addr *ret, addr real, addr imag)
{
	return complex_alloc_(NULL, ret, real, imag);
}

int complex_single_alloc_(LocalRoot local,
		addr *ret, single_float real, single_float imag)
{
	addr pos1, pos2;

	Return(single_float_check_alloc_(local, &pos1, real));
	Return(single_float_check_alloc_(local, &pos2, imag));
	return complex_unsafe_alloc_(local, ret, pos1, pos2, ComplexType_single);
}
int complex_single_local_(LocalRoot local,
		addr *ret, single_float real, single_float imag)
{
	Check(local == NULL, "local error");
	return complex_single_alloc_(local, ret, real, imag);
}
int complex_single_heap_(addr *ret,
		single_float real, single_float imag)
{
	return complex_single_alloc_(NULL, ret, real, imag);
}

int complex_double_alloc_(LocalRoot local,
		addr *ret, double_float real, double_float imag)
{
	addr pos1, pos2;

	Return(double_float_check_alloc_(local, &pos1, real));
	Return(double_float_check_alloc_(local, &pos2, imag));
	return complex_unsafe_alloc_(local, ret, pos1, pos2, ComplexType_double);
}
int complex_double_local_(LocalRoot local,
		addr *ret, double_float real, double_float imag)
{
	Check(local == NULL, "local error");
	return complex_double_alloc_(local, ret, real, imag);
}
int complex_double_heap_(addr *ret,
		double_float real, double_float imag)
{
	return complex_double_alloc_(NULL, ret, real, imag);
}

int complex_long_alloc_(LocalRoot local,
		addr *ret, long_float real, long_float imag)
{
	addr pos1, pos2;

	Return(long_float_check_alloc_(local, &pos1, real));
	Return(long_float_check_alloc_(local, &pos2, imag));
	return complex_unsafe_alloc_(local, ret, pos1, pos2, ComplexType_long);
}
int complex_long_local_(LocalRoot local,
		addr *ret, long_float real, long_float imag)
{
	Check(local == NULL, "local error");
	return complex_long_alloc_(local, ret, real, imag);
}
int complex_long_heap_(addr *ret,
		long_float real, long_float imag)
{
	return complex_long_alloc_(NULL, ret, real, imag);
}

int real_complex_single_heap_(addr *ret, single_float real, single_float imag)
{
	if (imag == 0.0f)
		return single_float_check_heap_(ret, real);
	else
		return complex_single_heap_(ret, real, imag);
}
int real_complex_double_heap_(addr *ret, double_float real, double_float imag)
{
	if (imag == 0.0)
		return double_float_check_heap_(ret, real);
	else
		return complex_double_heap_(ret, real, imag);
}
int real_complex_long_heap_(addr *ret, long_float real, long_float imag)
{
	if (imag == 0.0L)
		return long_float_check_heap_(ret, real);
	else
		return complex_long_heap_(ret, real, imag);
}

int complex_copy_alloc_(LocalRoot local, addr pos, addr *ret)
{
	enum ComplexType type;
	addr real, imag;

	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	getcomplex(pos, &type, &real, &imag);
	Return(real_copy_alloc_(local, real, &real));
	Return(real_copy_alloc_(local, imag, &imag));
	return complex_unsafe_alloc_(local, ret, real, imag, type);
}
int complex_copy_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return complex_copy_alloc_(local, pos, ret);
}
int complex_copy_heap_(addr pos, addr *ret)
{
	return complex_copy_alloc_(NULL, pos, ret);
}

int complex_result_local_(LocalRoot local, addr pos, addr *ret)
{
	int check;
	addr real, imag;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_COMPLEX);
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	Return(zerop_real_(imag, &check));
	if (check) {
		return real_result_local_(local, real, ret);
	}
	else {
		Return(real_result_local_(local, real, &real));
		Return(real_result_local_(local, imag, &imag));
		return complex_local_(local, ret, real, imag);
	}
}
int complex_result_heap_(LocalRoot local, addr pos, addr *ret)
{
	int check;
	addr real, imag;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_COMPLEX);
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	Return(zerop_real_(imag, &check));
	if (check) {
		return real_result_heap_(local, real, ret);
	}
	else {
		Return(real_result_heap_(local, real, &real));
		Return(real_result_heap_(local, imag, &imag));
		return complex_heap_(ret, real, imag);
	}
}

int complex_throw_alloc_(LocalRoot local, addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	if (GetStatusDynamic(pos))
		return complex_copy_heap_(pos, ret);
	else
		return Result(ret, pos);
}
int complex_throw_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return complex_throw_alloc_(local, pos, ret);
}

int complex_throw_heap_(addr pos, addr *ret)
{
	return complex_throw_alloc_(NULL, pos, ret);
}

static int complex_force_type_(addr pos, enum ComplexType *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			return Result(ret, ComplexType_rational);

		case LISPTYPE_SINGLE_FLOAT:
			return Result(ret, ComplexType_single);

		case LISPTYPE_DOUBLE_FLOAT:
			return Result(ret, ComplexType_double);

		case LISPTYPE_LONG_FLOAT:
			return Result(ret, ComplexType_long);

		default:
			*ret = ComplexType_error;
			return TypeError_(pos, REAL);
	}
}

int complex_force_heap_(addr *ret, addr real, addr imag, enum ComplexType type)
{
	addr pos;

	Return(real_throw_heap_(real, &real));
	Return(real_throw_heap_(imag, &imag));
	switch (type) {
		case ComplexType_single:
		case ComplexType_double:
		case ComplexType_long:
		case ComplexType_rational:
			break;

		case ComplexType_error:
		default:
			Return(complex_force_type_(real, &type));
			break;
	}

	make_complex_unsafe(NULL, &pos, type);
	SetRealComplex(pos, real);
	SetImagComplex(pos, imag);
	return Result(ret, pos);
}

int single_float_complex_(addr pos, single_float *re, single_float *im)
{
	addr real, imag;

	CheckType(pos, LISPTYPE_COMPLEX);
	switch (getcomplexr(pos, &real, &imag)) {
		case ComplexType_single:
			Return(cast_ss_value_(real, re));
			Return(cast_ss_value_(imag, im));
			return 0;

		case ComplexType_double:
			Return(cast_ds_value_(real, re));
			Return(cast_ds_value_(imag, im));
			return 0;

		case ComplexType_long:
			Return(cast_ls_value_(real, re));
			Return(cast_ls_value_(imag, im));
			return 0;

		case ComplexType_rational:
			Return(single_float_rational_(real, re));
			Return(single_float_rational_(imag, im));
			return 0;

		case ComplexType_error:
		default:
			*re = 0.0f;
			*im = 0.0f;
			return TypeError_(pos, COMPLEX);
	}
}

int double_float_complex_(addr pos, double_float *re, double_float *im)
{
	addr real, imag;

	CheckType(pos, LISPTYPE_COMPLEX);
	switch (getcomplexr(pos, &real, &imag)) {
		case ComplexType_single:
			Return(cast_sd_value_(real, re));
			Return(cast_sd_value_(imag, im));
			return 0;

		case ComplexType_double:
			Return(cast_dd_value_(real, re));
			Return(cast_dd_value_(imag, im));
			return 0;

		case ComplexType_long:
			Return(cast_ld_value_(real, re));
			Return(cast_ld_value_(imag, im));
			return 0;

		case ComplexType_rational:
			Return(double_float_rational_(real, re));
			Return(double_float_rational_(imag, im));
			return 0;

		case ComplexType_error:
		default:
			*re = 0.0;
			*im = 0.0;
			return TypeError_(pos, COMPLEX);
	}
}

int long_float_complex_(addr pos, long_float *re, long_float *im)
{
	addr real, imag;

	CheckType(pos, LISPTYPE_COMPLEX);
	switch (getcomplexr(pos, &real, &imag)) {
		case ComplexType_single:
			Return(cast_sl_value_(real, re));
			Return(cast_sl_value_(imag, im));
			return 0;

		case ComplexType_double:
			Return(cast_dl_value_(real, re));
			Return(cast_dl_value_(imag, im));
			return 0;

		case ComplexType_long:
			Return(cast_ll_value_(real, re));
			Return(cast_ll_value_(imag, im));
			return 0;

		case ComplexType_rational:
			Return(long_float_rational_(real, re));
			Return(long_float_rational_(imag, im));
			return 0;

		case ComplexType_error:
		default:
			*re = 0.0L;
			*im = 0.0L;
			return TypeError_(pos, COMPLEX);
	}
}

static int zerop_call_complex_(addr pos, int *ret, int (*call)(addr))
{
	addr value;

	GetRealComplex(pos, &value);
	if (! (*call)(value))
		return Result(ret, 0);

	GetImagComplex(pos, &value);
	return Result(ret, (*call)(value));
}

static int zerop_rational_complex_(addr pos, int *ret)
{
	addr value;

	GetRealComplex(pos, &value);
	Return(zerop_rational_(value, ret));
	if (*ret == 0)
		return 0;

	GetImagComplex(pos, &value);
	return zerop_rational_(value, ret);
}

int zerop_complex_(addr pos, int *ret)
{
	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	switch (GetTypeComplex(pos)) {
		case ComplexType_single:
			return zerop_call_complex_(pos, ret, zerop_single_float);

		case ComplexType_double:
			return zerop_call_complex_(pos, ret, zerop_double_float);

		case ComplexType_long:
			return zerop_call_complex_(pos, ret, zerop_long_float);

		case ComplexType_rational:
			return zerop_rational_complex_(pos, ret);

		default:
			*ret = 0;
			return TypeError_(pos, COMPLEX);
	}
}

int eql_complex(addr left, addr right)
{
	addr value1, value2;

	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(left, &value1);
	GetRealComplex(right, &value2);
	if (! eql_function(value1, value2))
		return 0;
	GetImagComplex(left, &value1);
	GetImagComplex(right, &value2);

	return eql_function(value1, value2);
}

int equal_complex_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	addr value1, value2;

	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(left, &value1);
	GetRealComplex(right, &value2);
	Return(equal_real_(local, value1, value2, &check));
	if (! check)
		return Result(ret, 0);
	GetImagComplex(left, &value1);
	GetImagComplex(right, &value2);

	return equal_real_(local, value1, value2, ret);
}

int equal_fc_number_(addr left, addr right, int *ret)
{
	int check;
	addr value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &value);
	Return(zerop_real_(value, &check));
	if (! check)
		return Result(ret, 0);
	GetRealComplex(right, &value);

	return equal_fixnum_real_(left, value, ret);
}

int equal_bc_number_(addr left, addr right, int *ret)
{
	int check;
	addr value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &value);
	Return(zerop_real_(value, &check));
	if (! check)
		return Result(ret, 0);
	GetRealComplex(right, &value);

	return equal_bignum_real_(left, value, ret);
}

int equal_rc_number_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	addr value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &value);
	Return(zerop_real_(value, &check));
	if (! check)
		return Result(ret, 0);
	GetRealComplex(right, &value);

	return equal_ratio_real_(local, left, value, ret);
}

int equal_sc_number_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	addr value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &value);
	Return(zerop_real_(value, &check));
	if (! check)
		return Result(ret, 0);
	GetRealComplex(right, &value);

	return equal_single_float_real_(local, left, value, ret);
}

int equal_dc_number_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	addr value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &value);
	Return(zerop_real_(value, &check));
	if (! check)
		return Result(ret, 0);
	GetRealComplex(right, &value);

	return equal_double_float_real_(local, left, value, ret);
}

int equal_lc_number_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	addr value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &value);
	Return(zerop_real_(value, &check));
	if (! check)
		return Result(ret, 0);
	GetRealComplex(right, &value);

	return equal_long_float_real_(local, left, value, ret);
}

int sign_reverse_complex_common_(addr pos, addr *ret)
{
	enum ComplexType type;
	addr real, imag;

	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	switch (getcomplex(pos, &type, &real, &imag)) {
		case ComplexType_single:
			sign_reverse_floats_heap(real, &real);
			sign_reverse_floats_heap(imag, &imag);
			break;

		case ComplexType_double:
			sign_reverse_floatd_heap(real, &real);
			sign_reverse_floatd_heap(imag, &imag);
			break;

		case ComplexType_long:
			sign_reverse_floatl_heap(real, &real);
			sign_reverse_floatl_heap(imag, &imag);
			break;

		case ComplexType_rational:
			Return(sign_reverse_rational_common_(real, &real));
			Return(sign_reverse_rational_common_(imag, &imag));
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}

	return complex_unsafe_heap_(ret, real, imag, type);
}


/*
 *  abs
 */
int abs_complex_common_(addr pos, addr *ret)
{
	addr real, imag;
	single_float vf, v2;
	double_float vd;
	long_float vl;

	CheckType(pos, LISPTYPE_COMPLEX);
	switch (getcomplexr(pos, &real, &imag)) {
		case ComplexType_single:
			cabs_f(RefSingleFloat(real), RefSingleFloat(imag), &vf);
			return single_float_check_heap_(ret, vf);

		case ComplexType_double:
			cabs_d(RefDoubleFloat(real), RefDoubleFloat(imag), &vd);
			return double_float_check_heap_(ret, vd);

		case ComplexType_long:
			cabs_l(RefLongFloat(real), RefLongFloat(imag), &vl);
			return long_float_check_heap_(ret, vl);

		case ComplexType_rational:
			Return(single_float_rational_(real, &vf));
			Return(single_float_rational_(imag, &v2));
			cabs_f(vf, v2, &vf);
			return single_float_check_heap_(ret, vf);

		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}
}


/*
 *  signum
 */
static int signum_complex_single_(addr pos, addr *ret)
{
	single_float real, imag, denom;

	Return(single_float_complex_(pos, &real, &imag));
	cabs_f(real, imag, &denom);
	if (denom == 0.0f)
		return single_float_check_heap_(ret, denom);
	else
		return complex_single_heap_(ret, real/denom, imag/denom);
}

static int signum_complex_double_(addr pos, addr *ret)
{
	double_float real, imag, denom;

	Return(double_float_complex_(pos, &real, &imag));
	cabs_d(real, imag, &denom);
	if (denom == 0.0f)
		return double_float_check_heap_(ret, denom);
	else
		return complex_double_heap_(ret, real/denom, imag/denom);
}

static int signum_complex_long_(addr pos, addr *ret)
{
	long_float real, imag, denom;

	Return(long_float_complex_(pos, &real, &imag));
	cabs_l(real, imag, &denom);
	if (denom == 0.0L)
		return long_float_check_heap_(ret, denom);
	else
		return complex_long_heap_(ret, real/denom, imag/denom);
}

int signum_complex_common_(addr pos, addr *ret)
{
	int check;

	CheckType(pos, LISPTYPE_COMPLEX);
	Return(zerop_complex_(pos, &check));
	if (check)
		return Result(ret, pos);

	switch (GetTypeComplex(pos)) {
		case ComplexType_rational:
		case ComplexType_single:
			return signum_complex_single_(pos, ret);

		case ComplexType_double:
			return signum_complex_double_(pos, ret);

		case ComplexType_long:
			return signum_complex_long_(pos, ret);

		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}
}


/************************************************************
 *  cmpl_arch.c
 ************************************************************/

#if defined(LISP_COMPLEX_WINDOWS)
/*
 *  Complex math library for Windows
 *    WARNING: This library is a very inaccurasy.
 *
 *  Common Lisp the Language, 2nd Edition
 *  https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node128.html
 *  12.5.2. Trigonometric and Related Functions
 *    Implementation note: These formulae are mathematically correct, assuming
 *    completely accurate computation. They may be terrible methods for
 *    floating-point computation. Implementors should consult a good text
 *    on numerical analysis. The formulae given above are not necessarily
 *    the simplest ones for real-valued computations, either; they are chosen
 *    to define the branch cuts in desirable ways for the complex case.
 */

/* exp(z) = exp(x) * (cos(y) + i*sin(y)) */
void cexp_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_float e = expf(real);
	*re = e * cosf(imag);
	*im = e * sinf(imag);
}

void cexp_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_float e = exp(real);
	*re = e * cos(imag);
	*im = e * sin(imag);
}

void cexp_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_float e = expl(real);
	*re = e * cosl(imag);
	*im = e * sinl(imag);
}

/* log(z) = log|z| + i*phase(z) */
void clog_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	*re = logf(sqrtf(real*real + imag*imag));
	*im = atan2f(imag, real);
}

void clog_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	*re = log(sqrt(real*real + imag*imag));
	*im = atan2(imag, real);
}

void clog_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	*re = logl(sqrtl(real*real + imag*imag));
	*im = atan2l(imag, real);
}

/* sin(z) = sin(x)*cosh(y) + i*cos(x)*sinh(y) */
void csin_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	*re = sinf(real) * coshf(imag);
	*im = cosf(real) * sinhf(imag);
}

void csin_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	*re = sin(real) * cosh(imag);
	*im = cos(real) * sinh(imag);
}

void csin_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	*re = sinl(real) * coshl(imag);
	*im = cosl(real) * sinhl(imag);
}

/* cos(z) = cos(x)*cosh(y) - i*sin(x)*sinh(y) */
void ccos_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	*re =  cosf(real) * coshf(imag);
	*im = -sinf(real) * sinhf(imag);
}

void ccos_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	*re =  cos(real) * cosh(imag);
	*im = -sin(real) * sinh(imag);
}

void ccos_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	*re =  cosl(real) * coshl(imag);
	*im = -sinl(real) * sinhl(imag);
}

/* tan(z) = (sin(2*x) + i*sinh(2*y)) / (cos(2*x) + cosh(2*y)) */
void ctan_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_float x2, y2, denom;

	x2 = 2.0f * real;
	y2 = 2.0f * imag;
	denom = cosf(x2) + coshf(y2);
	*re = sinf(x2) / denom;
	*im = sinhf(y2) / denom;
}

void ctan_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_float x2, y2, denom;

	x2 = 2.0 * real;
	y2 = 2.0 * imag;
	denom = cos(x2) + cosh(y2);
	*re = sin(x2) / denom;
	*im = sinh(y2) / denom;
}

void ctan_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_float x2, y2, denom;

	x2 = 2.0L * real;
	y2 = 2.0L * imag;
	denom = cosl(x2) + coshl(y2);
	*re = sinl(x2) / denom;
	*im = sinhl(y2) / denom;
}

/* sinh(z) = sinh(x)*cos(y) + i*cosh(x)*sin(y) */
void csinh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	*re = sinhf(real) * cosf(imag);
	*im = coshf(real) * sinf(imag);
}

void csinh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	*re = sinh(real) * cos(imag);
	*im = cosh(real) * sin(imag);
}

void csinh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	*re = sinhl(real) * cosl(imag);
	*im = coshl(real) * sinl(imag);
}

/* cosh(z) = cosh(x)*cos(y) i*sinh(x)*sin(y) */
void ccosh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	*re = coshf(real) * cosf(imag);
	*im = sinhf(real) * sinf(imag);
}

void ccosh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	*re = cosh(real) * cos(imag);
	*im = sinh(real) * sin(imag);
}

void ccosh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	*re = coshl(real) * cosl(imag);
	*im = sinhl(real) * sinl(imag);
}

/* tan(z) = (sinh(2*x) + i*sin(2*y)) / (cosh(2*x) + cos(2*y)) */
void ctanh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_float x2, y2, denom;

	x2 = 2.0f * real;
	y2 = 2.0f * imag;
	denom = coshf(x2) + cosf(y2);
	*re = sinhf(x2) / denom;
	*im = sinf(y2) / denom;
}

void ctanh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_float x2, y2, denom;

	x2 = 2.0 * real;
	y2 = 2.0 * imag;
	denom = cosh(x2) + cos(y2);
	*re = sinh(x2) / denom;
	*im = sin(y2) / denom;
}

void ctanh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_float x2, y2, denom;

	x2 = 2.0L * real;
	y2 = 2.0L * imag;
	denom = coshl(x2) + cosl(y2);
	*re = sinhl(x2) / denom;
	*im = sinl(y2) / denom;
}

/* asin(z) = -i*log(i*z + sqrt(1-z*z)) */
void casin_f(single_float a, single_float b, single_float *re, single_float *im)
{
	single_float c, d;

	csqrt_f(1.0f-(a*a-b*b), -2.0f*a*b, &c, &d);
	clog_f(-b+c, a+d, &c, &d);
	*re = d;
	*im = -c;
}

void casin_d(double_float a, double_float b, double_float *re, double_float *im)
{
	double_float c, d;

	csqrt_d(1.0-(a*a-b*b), -2.0*a*b, &c, &d);
	clog_d(-b+c, a+d, &c, &d);
	*re = d;
	*im = -c;
}

void casin_l(long_float a, long_float b, long_float *re, long_float *im)
{
	long_float c, d;

	csqrt_l(1.0L-(a*a-b*b), -2.0L*a*b, &c, &d);
	clog_l(-b+c, a+d, &c, &d);
	*re = d;
	*im = -c;
}

/* acos(z) = -i*log(z + i*sqrt(1-z*z)) */
void cacos_f(single_float a, single_float b, single_float *re, single_float *im)
{
	single_float c, d;

	csqrt_f(1.0f-(a*a-b*b), -2.0f*a*b, &c, &d);
	clog_f(a-d, b+c, &c, &d);
	*re = d;
	*im = -c;
}

void cacos_d(double_float a, double_float b, double_float *re, double_float *im)
{
	double_float c, d;

	csqrt_d(1.0-(a*a-b*b), -2.0*a*b, &c, &d);
	clog_d(a-d, b+c, &c, &d);
	*re = d;
	*im = -c;
}

void cacos_l(long_float a, long_float b, long_float *re, long_float *im)
{
	long_float c, d;

	csqrt_l(1.0L-(a*a-b*b), -2.0L*a*b, &c, &d);
	clog_l(a-d, b+c, &c, &d);
	*re = d;
	*im = -c;
}

/* atan(z) = (log(1+i*z) - log(1-i*z))/(2*i) */
void catan_f(single_float a, single_float b, single_float *re, single_float *im)
{
	single_float p1, p2, m1, m2;

	clog_f(1.0f-b,  a, &p1, &p2);
	clog_f(1.0f+b, -a, &m1, &m2);
	*re =  0.5f * (p2 - m2);
	*im = -0.5f * (p1 - m1);
}

void catan_d(double_float a, double_float b, double_float *re, double_float *im)
{
	double_float p1, p2, m1, m2;

	clog_d(1.0-b,  a, &p1, &p2);
	clog_d(1.0+b, -a, &m1, &m2);
	*re =  0.5 * (p2 - m2);
	*im = -0.5 * (p1 - m1);
}

void catan_l(long_float a, long_float b, long_float *re, long_float *im)
{
	long_float p1, p2, m1, m2;

	clog_l(1.0L-b,  a, &p1, &p2);
	clog_l(1.0L+b, -a, &m1, &m2);
	*re =  0.5L * (p2 - m2);
	*im = -0.5L * (p1 - m1);
}

/* asinh(z) = -i*asin(i*z) */
void casinh_f(single_float a, single_float b, single_float *re, single_float *im)
{
	casin_f(-b, a, &a, &b);
	*re = b;
	*im = -a;
}

void casinh_d(double_float a, double_float b, double_float *re, double_float *im)
{
	casin_d(-b, a, &a, &b);
	*re = b;
	*im = -a;
}

void casinh_l(long_float a, long_float b, long_float *re, long_float *im)
{
	casin_l(-b, a, &a, &b);
	*re = b;
	*im = -a;
}

/* acosh(z) = 2*log(sqrt((z+1)/2) + sqrt((z-1)/2)) */
void cacosh_f(single_float a, single_float b, single_float *re, single_float *im)
{
	single_float p1, p2, m1, m2;

	csqrt_f(0.5f*(a+1.0f), 0.5f*b, &p1, &p2);
	csqrt_f(0.5f*(a-1.0f), 0.5f*b, &m1, &m2);
	clog_f(p1+m1, p2+m2, &a, &b);
	*re = 2.0f*a;
	*im = 2.0f*b;
}

void cacosh_d(double_float a, double_float b, double_float *re, double_float *im)
{
	double_float p1, p2, m1, m2;

	csqrt_d(0.5*(a+1.0), 0.5*b, &p1, &p2);
	csqrt_d(0.5*(a-1.0), 0.5*b, &m1, &m2);
	clog_d(p1+m1, p2+m2, &a, &b);
	*re = 2.0*a;
	*im = 2.0*b;
}

void cacosh_l(long_float a, long_float b, long_float *re, long_float *im)
{
	long_float p1, p2, m1, m2;

	csqrt_l(0.5L*(a+1.0L), 0.5L*b, &p1, &p2);
	csqrt_l(0.5L*(a-1.0L), 0.5L*b, &m1, &m2);
	clog_l(p1+m1, p2+m2, &a, &b);
	*re = 2.0L*a;
	*im = 2.0L*b;
}

/* atanh(z) = atan(iz)/i */
void catanh_f(single_float a, single_float b, single_float *re, single_float *im)
{
	catan_f(-b, a, &a, &b);
	*re = b;
	*im = -a;
}

void catanh_d(double_float a, double_float b, double_float *re, double_float *im)
{
	catan_d(-b, a, &a, &b);
	*re = b;
	*im = -a;
}

void catanh_l(long_float a, long_float b, long_float *re, long_float *im)
{
	catan_l(-b, a, &a, &b);
	*re = b;
	*im = -a;
}

void cabs_f(single_float real, single_float imag, single_float *ret)
{
	*ret = sqrtf(real*real + imag*imag);
}

void cabs_d(double_float real, double_float imag, double_float *ret)
{
	*ret = sqrt(real*real + imag*imag);
}

void cabs_l(long_float real, long_float imag, long_float *ret)
{
	*ret = sqrtl(real*real + imag*imag);
}

#elif defined(LISP_COMPLEX_CPLUSPLUS)
#include <cmath>
#include <complex>

typedef std::complex<float> single_complex;
typedef std::complex<double> double_complex;
typedef std::complex<long double> long_complex;

void cexp_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::exp(c);
	*re = c.real();
	*im = c.imag();
}

void cexp_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::exp(c);
	*re = c.real();
	*im = c.imag();
}

void cexp_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::exp(c);
	*re = c.real();
	*im = c.imag();
}

void clog_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::log(c);
	*re = c.real();
	*im = c.imag();
}

void clog_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::log(c);
	*re = c.real();
	*im = c.imag();
}

void clog_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::log(c);
	*re = c.real();
	*im = c.imag();
}

void csin_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::sin(c);
	*re = c.real();
	*im = c.imag();
}

void csin_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::sin(c);
	*re = c.real();
	*im = c.imag();
}

void csin_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::sin(c);
	*re = c.real();
	*im = c.imag();
}

void ccos_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::cos(c);
	*re = c.real();
	*im = c.imag();
}

void ccos_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::cos(c);
	*re = c.real();
	*im = c.imag();
}

void ccos_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::cos(c);
	*re = c.real();
	*im = c.imag();
}

void ctan_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::tan(c);
	*re = c.real();
	*im = c.imag();
}

void ctan_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::tan(c);
	*re = c.real();
	*im = c.imag();
}

void ctan_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::tan(c);
	*re = c.real();
	*im = c.imag();
}

void csinh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::sinh(c);
	*re = c.real();
	*im = c.imag();
}

void csinh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::sinh(c);
	*re = c.real();
	*im = c.imag();
}

void csinh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::sinh(c);
	*re = c.real();
	*im = c.imag();
}

void ccosh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::cosh(c);
	*re = c.real();
	*im = c.imag();
}

void ccosh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::cosh(c);
	*re = c.real();
	*im = c.imag();
}

void ccosh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::cosh(c);
	*re = c.real();
	*im = c.imag();
}

void ctanh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::tanh(c);
	*re = c.real();
	*im = c.imag();
}

void ctanh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::tanh(c);
	*re = c.real();
	*im = c.imag();
}

void ctanh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::tanh(c);
	*re = c.real();
	*im = c.imag();
}

void casin_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::asin(c);
	*re = c.real();
	*im = c.imag();
}

void casin_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::asin(c);
	*re = c.real();
	*im = c.imag();
}

void casin_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::asin(c);
	*re = c.real();
	*im = c.imag();
}

void cacos_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::acos(c);
	*re = c.real();
	*im = c.imag();
}

void cacos_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::acos(c);
	*re = c.real();
	*im = c.imag();
}

void cacos_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::acos(c);
	*re = c.real();
	*im = c.imag();
}

void catan_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::atan(c);
	*re = c.real();
	*im = c.imag();
}

void catan_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::atan(c);
	*re = c.real();
	*im = c.imag();
}

void catan_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::atan(c);
	*re = c.real();
	*im = c.imag();
}

void casinh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::asinh(c);
	*re = c.real();
	*im = c.imag();
}

void casinh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::asinh(c);
	*re = c.real();
	*im = c.imag();
}

void casinh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::asinh(c);
	*re = c.real();
	*im = c.imag();
}

void cacosh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::acosh(c);
	*re = c.real();
	*im = c.imag();
}

void cacosh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::acosh(c);
	*re = c.real();
	*im = c.imag();
}

void cacosh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::acosh(c);
	*re = c.real();
	*im = c.imag();
}

void catanh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::atanh(c);
	*re = c.real();
	*im = c.imag();
}

void catanh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::atanh(c);
	*re = c.real();
	*im = c.imag();
}

void catanh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::atanh(c);
	*re = c.real();
	*im = c.imag();
}

void cabs_f(single_float real, single_float imag, single_float *ret)
{
	single_complex c(real, imag);
	*ret = abs(c);
}

void cabs_d(double_float real, double_float imag, double_float *ret)
{
	double_complex c(real, imag);
	*ret = abs(c);
}

void cabs_l(long_float real, long_float imag, long_float *ret)
{
	long_complex c(real, imag);
	*ret = abs(c);
}

#else

typedef float complex single_complex;
typedef double complex double_complex;
typedef long double complex long_complex;

#ifdef LISP_COMPLEX_LONG
#define clogf_define clogf
#define clogd_define clog
#define clogl_define clogl
#else
#define cexpl cexp
#define csinl csin
#define ccosl ccos
#define ctanl ctan
#define csinhl csinh
#define ccoshl ccosh
#define ctanhl ctanh
#define casinl casin
#define cacosl cacos
#define catanl catan
#define casinhl casinh
#define cacoshl cacosh
#define catanhl catanh

static single_complex clogf_define(single_complex z)
{
	return logf(cabsf(z)) + I * cargf(z);
}
static double_complex clogd_define(double_complex z)
{
	return log(cabs(z)) + I * carg(z);
}
static long_complex clogl_define(long_complex z)
{
	return logl(cabsl(z)) + I * cargl(z);
}
#endif

void cexp_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = cexpf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void cexp_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = cexp(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void cexp_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = cexpl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void clog_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = clogf_define(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void clog_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = clogd_define(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void clog_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = clogl_define(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void csin_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = csinf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void csin_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = csin(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void csin_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = csinl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void ccos_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = ccosf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void ccos_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = ccos(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void ccos_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = ccosl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void ctan_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = ctanf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void ctan_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = ctan(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void ctan_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = ctanl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void csinh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = csinhf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void csinh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = csinh(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void csinh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = csinhl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void ccosh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = ccoshf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void ccosh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = ccosh(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void ccosh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = ccoshl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void ctanh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = ctanhf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void ctanh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = ctanh(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void ctanh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = ctanhl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void casin_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = casinf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void casin_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = casin(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void casin_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = casinl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void cacos_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = cacosf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void cacos_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = cacos(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void cacos_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = cacosl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void catan_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = catanf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void catan_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = catan(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void catan_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = catanl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void casinh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = casinhf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void casinh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = casinh(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void casinh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = casinhl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void cacosh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = cacoshf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void cacosh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = cacosh(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void cacosh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = cacoshl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void catanh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = catanhf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void catanh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = catanh(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void catanh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = catanhl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

void cabs_f(single_float real, single_float imag, single_float *ret)
{
	single_complex c = real + imag * I;
	*ret = cabsf(c);
}

void cabs_d(double_float real, double_float imag, double_float *ret)
{
	double_complex c = real + imag * I;
	*ret = cabs(c);
}

void cabs_l(long_float real, long_float imag, long_float *ret)
{
	long_complex c = real + imag * I;
	*ret = cabsl(c);
}

#endif


/************************************************************
 *  cmpl_math.c
 ************************************************************/

#ifdef LISP_FREEBSD_OLD_VERSION
#define powl_define(x,y) pow((double)(x), (double)(y))
#else
#define powl_define powl
#endif

/*
 *  expt
 */
void expt_f(single_float a, single_float b, single_float c, single_float d,
		single_float *re, single_float *im)
{
	single_float x, y;

	/* a**b = exp{b*log(a)} */
	if (0.0f <= a && b == 0.0f && d == 0.0f) {
		*re = powf(a, c);
		*im = 0.0f;
	}
	else {
		/* log(a) */
		clog_f(a, b, &a, &b);
		/* b*log(a) */
		x = a*c - b*d;
		y = b*c + a*d;
		/* exp{...} */
		cexp_f(x, y, re, im);
	}
}

void expt_d(double_float a, double_float b, double_float c, double_float d,
		double_float *re, double_float *im)
{
	double_float x, y;

	/* a**b = exp{b*log(a)} */
	if (0.0 <= a && b == 0.0 && d == 0.0) {
		*re = pow(a, c);
		*im = 0.0;
	}
	else {
		/* log(a) */
		clog_d(a, b, &a, &b);
		/* b*log(a) */
		x = a*c - b*d;
		y = b*c + a*d;
		/* exp{...} */
		cexp_d(x, y, re, im);
	}
}

void expt_l(long_float a, long_float b, long_float c, long_float d,
		long_float *re, long_float *im)
{
	long_float x, y;

	/* a**b = exp{b*log(a)} */
	if (0.0L <= a && b == 0.0L && d == 0.0L) {
		*re = powl_define(a, c);
		*im = 0.0L;
	}
	else {
		/* log(a) */
		clog_l(a, b, &a, &b);
		/* b*log(a) */
		x = a*c - b*d;
		y = b*c + a*d;
		/* exp{...} */
		cexp_l(x, y, re, im);
	}
}


/*
 *  clogb
 */
int clogb_f_(single_float a, single_float b, single_float c, single_float d,
		single_float *re, single_float *im)
{
	addr real, imag;
	single_float denom;

	/* clogb(number, base) -> log(a+ib)/log(c+id) */
	if (a == 0.0f && b == 0.0f) {
		Return(complex_single_heap_(&real, a, b));
		Return(complex_single_heap_(&imag, c, d));
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_LOG, real, imag);
	}
	if (c == 0.0f && d == 0.0f) {
		*re = *im = 0.0f;
		return 0;
	}
	/* log(a+ib) */
	clog_f(a, b, &a, &b);
	/* log(c+id) */
	clog_f(c, d, &c, &d);
	/* log(a+ib) / log(c+id) */
	denom = c*c + d*d;
	*re = (a*c + b*d) / denom;
	*im = (b*c - a*d) / denom;

	return 0;
}

int clogb_d_(double_float a, double_float b, double_float c, double_float d,
		double_float *re, double_float *im)
{
	addr real, imag;
	double_float denom;

	/* clogb(number, base) -> log(a+ib)/log(c+id) */
	if (a == 0.0 && b == 0.0) {
		Return(complex_double_heap_(&real, a, b));
		Return(complex_double_heap_(&imag, c, d));
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_LOG, real, imag);
	}
	if (c == 0.0 && d == 0.0) {
		*re = *im = 0.0;
		return 0;
	}
	/* log(a+ib) */
	clog_d(a, b, &a, &b);
	/* log(c+id) */
	clog_d(c, d, &c, &d);
	/* log(a+ib) / log(c+id) */
	denom = c*c + d*d;
	*re = (a*c + b*d) / denom;
	*im = (b*c - a*d) / denom;

	return 0;
}

int clogb_l_(long_float a, long_float b, long_float c, long_float d,
		long_float *re, long_float *im)
{
	addr real, imag;
	long_float denom;

	/* clogb(number, base) -> log(a+ib)/log(c+id) */
	if (a == 0.0L && b == 0.0L) {
		Return(complex_long_heap_(&real, a, b));
		Return(complex_long_heap_(&imag, c, d));
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_LOG, real, imag);
	}
	if (c == 0.0L && d == 0.0L) {
		*re = *im = 0.0L;
		return 0;
	}
	/* log(a+ib) */
	clog_l(a, b, &a, &b);
	/* log(c+id) */
	clog_l(c, d, &c, &d);
	/* log(a+ib) / log(c+id) */
	denom = c*c + d*d;
	*re = (a*c + b*d) / denom;
	*im = (b*c - a*d) / denom;

	return 0;
}


/*
 *  csqrt
 */
void csqrt_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	expt_f(real, imag, 0.5f, 0.0f, re, im);
}

void csqrt_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	expt_d(real, imag, 0.5, 0.0, re, im);
}

void csqrt_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	expt_l(real, imag, 0.5L, 0.0L, re, im);
}


/************************************************************
 *  cmpl_multi.c
 ************************************************************/

/*
 *  multiple
 */
static int multi_real_complex_(addr left, addr right, addr *value, int *ret)
{
	enum MathType type;
	single_float vs;
	double_float vd;
	long_float vl;
	struct mathcomplex2_struct str;

	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			vs = str.v.s.a;
			Return(complex_single_heap_(value, vs*str.v.s.c, vs*str.v.s.d));
			return Result(ret, 0);

		case MathType_double:
			vd = str.v.d.a;
			Return(complex_double_heap_(value, vd*str.v.d.c, vd*str.v.d.d));
			return Result(ret, 0);

		case MathType_long:
			vl = str.v.l.a;
			Return(complex_long_heap_(value, vl*str.v.l.c, vl*str.v.l.d));
			return Result(ret, 0);

		case MathType_rational:
			return Result(ret, 1);

		case MathType_complex:
		case MathType_error:
		default:
			*value = NULL;
			*ret = 0;
			return fmte_("type error", NULL);
	}
}

int multi_rational_complex_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int check;
	addr real, imag;

	CheckLocal(local);
	Check(! rationalp(left), "type error");
	CheckType(right, LISPTYPE_COMPLEX);
	Return(multi_real_complex_(left, right, ret, &check));
	if (check) {
		GetRealComplex(right, &real);
		GetImagComplex(right, &imag);
		Return(multi_rational_common_(local, left, real, &real));
		Return(multi_rational_common_(local, left, imag, &imag));
		return complex_heap_(ret, real, imag);
	}

	return 0;
}

int multi_fc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	return multi_rational_complex_common_(local, left, right, ret);
}

int multi_bc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	return multi_rational_complex_common_(local, left, right, ret);
}

int multi_rc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	return multi_rational_complex_common_(local, left, right, ret);
}

int multi_sc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(multi_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

int multi_dc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(multi_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

int multi_lc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(multi_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

static int multi_cc_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b, c, d, ac, bd, ad, bc;

	/* (a+bi)(c+di) = ac-bd + (ad+bc)i */
	Check(GetTypeComplex(left) != ComplexType_rational, "type error");
	Check(GetTypeComplex(right) != ComplexType_rational, "type error");
	push_local(local, &stack);
	GetRealComplex(left, &a);
	GetImagComplex(left, &b);
	GetRealComplex(right, &c);
	GetImagComplex(right, &d);
	Return(multi_rational_local_(local, a, c, &ac));
	Return(multi_rational_local_(local, b, d, &bd));
	Return(multi_rational_local_(local, a, d, &ad));
	Return(multi_rational_local_(local, b, c, &bc));
	Return(minus_rational_local_(local, ac, bd, &a));
	Return(plus_rational_local_(local, ad, bc, &b));
	Return(complex_heap_(ret, a, b));
	rollback_local(local, stack);

	return 0;
}

int multi_cc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	enum MathType type;
	single_float as, bs, cs, ds;
	double_float ad, bd, cd, dd;
	long_float al, bl, cl, dl;
	struct mathcomplex2_struct str;

	/* (a+bi)(c+di) = ac-bd + (ad+bc)i */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			as = str.v.s.a;
			bs = str.v.s.b;
			cs = str.v.s.c;
			ds = str.v.s.d;
			return complex_single_heap_(ret, as*cs - bs*ds, as*ds + bs*cs);

		case MathType_double:
			ad = str.v.d.a;
			bd = str.v.d.b;
			cd = str.v.d.c;
			dd = str.v.d.d;
			return complex_double_heap_(ret, ad*cd - bd*dd, ad*dd + bd*cd);

		case MathType_long:
			al = str.v.l.a;
			bl = str.v.l.b;
			cl = str.v.l.c;
			dl = str.v.l.d;
			return complex_long_heap_(ret, al*cl - bl*dl, al*dl + bl*cl);

		case MathType_rational:
			return multi_cc_rational_common_(local, left, right, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}


/*
 *  inverse
 */
static int inverse_complex_rational_(LocalRoot local, addr pos, addr *ret)
{
	int check;
	LocalStack stack;
	addr a, b, a2, b2, ab;

	/* Re:  a/(a*a + b*b)
	 * Im: -b/(a*a + b*b)
	 */
	Return(zerop_complex_(pos, &check));
	if (check)
		return call_division_by_zero1_(NULL, pos);
	GetRealComplex(pos, &a);
	GetImagComplex(pos, &b);
	push_local(local, &stack);
	Return(multi_rational_local_(local, a, a, &a2));
	Return(multi_rational_local_(local, b, b, &b2));
	Return(plus_rational_local_(local, a2, b2, &ab));
	Return(div_rational_local_(local, a, ab, &a));
	Return(div_rational_local_(local, b, ab, &b));
	Return(sign_reverse_rational_local_(local, b, &b));
	Return(complex_heap_(ret, a, b));
	rollback_local(local, stack);

	return 0;
}

int inverse_complex_common_(LocalRoot local, addr pos, addr *ret)
{
	enum MathType type;
	single_float as, bs, ds;
	double_float ad, bd, dd;
	long_float al, bl, dl;
	struct mathreal2_struct str;

	CheckLocalType(local, pos, LISPTYPE_COMPLEX);
	Return(getmathcomplex1_inverse_(&str, pos, &type));
	switch (type) {
		case MathType_single:
			as = str.v.s.a;
			bs = str.v.s.b;
			ds = as*as + bs+bs;
			return complex_single_heap_(ret, as/ds, -bs/ds);

		case MathType_double:
			ad = str.v.d.a;
			bd = str.v.d.b;
			dd = ad*ad + bd+bd;
			return complex_double_heap_(ret, ad/dd, -bd/dd);

		case MathType_long:
			al = str.v.l.a;
			bl = str.v.l.b;
			dl = al*al + bl+bl;
			return complex_long_heap_(ret, al/dl, -bl/dl);

		case MathType_rational:
			return inverse_complex_rational_(local, pos, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = 0;
			return fmte_("Type error", NULL);
	}
}


/*
 *  division
 */
static int div_real_complex_(addr left, addr right, addr *value, int *ret)
{
	/* Re:  n*a/(a*a + b*b)
	 * Im: -n*b/(a*a + b*b)
	 */
	enum MathType type;
	single_float ns, as, bs, ds;
	double_float nd, ad, bd, dd;
	long_float nl, al, bl, dl;
	struct mathcomplex2_struct str;

	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			ns = str.v.s.a;
			as = str.v.s.c;
			bs = str.v.s.d;
			ds = as*as + bs*bs;
			Return(complex_single_heap_(value, ns*as/ds, -ns*bs/ds));
			return Result(ret, 0);

		case MathType_double:
			nd = str.v.d.a;
			ad = str.v.d.c;
			bd = str.v.d.d;
			dd = ad*ad + bd*bd;
			Return(complex_double_heap_(value, nd*ad/dd, -nd*bd/dd));
			return Result(ret, 0);

		case MathType_long:
			nl = str.v.l.a;
			al = str.v.l.c;
			bl = str.v.l.d;
			dl = al*al + bl*bl;
			Return(complex_long_heap_(value, nl*al/dl, -nl*bl/dl));
			return Result(ret, 0);

		case MathType_rational:
			return Result(ret, 1);

		case MathType_complex:
		case MathType_error:
		default:
			*value = NULL;
			*ret = 0;
			return fmte_("type error", NULL);
	}
}

static int div_complex_real_(addr left, addr right, addr *value, int *ret)
{
	/* Re: a/n
	 * Im: b/n
	 */
	enum MathType type;
	single_float ns, as, bs;
	double_float nd, ad, bd;
	long_float nl, al, bl;
	struct mathcomplex2_struct str;

	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			as = str.v.s.a;
			bs = str.v.s.b;
			ns = str.v.s.c;
			Return(complex_single_heap_(value, as/ns, bs/ns));
			return Result(ret, 0);

		case MathType_double:
			ad = str.v.d.a;
			bd = str.v.d.b;
			nd = str.v.d.c;
			Return(complex_double_heap_(value, ad/nd, bd/nd));
			return Result(ret, 0);

		case MathType_long:
			al = str.v.l.a;
			bl = str.v.l.b;
			nl = str.v.l.c;
			Return(complex_long_heap_(value, al/nl, bl/nl));
			return Result(ret, 0);

		case MathType_rational:
			return Result(ret, 1);

		case MathType_complex:
		case MathType_error:
		default:
			*value = 0;
			*ret = 0;
			return fmte_("type error", NULL);
	}
}

int div_rational_complex_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int check;
	LocalStack stack;
	addr a, b, a2, b2, ab;

	/* Re:  n*a/(a*a + b*b)
	 * Im: -n*b/(a*a + b*b)
	 */
	CheckLocal(local);
	Check(! rationalp(left), "type error");
	CheckType(right, LISPTYPE_COMPLEX);
	Return(zerop_complex_(right, &check));
	if (check)
		return call_division_by_zero2_(NULL, left, right);
	Return(div_real_complex_(left, right, ret, &check));
	if (check) {
		GetRealComplex(right, &a);
		GetImagComplex(right, &b);
		push_local(local, &stack);
		Return(multi_rational_local_(local, a, a, &a2));
		Return(multi_rational_local_(local, b, b, &b2));
		Return(plus_rational_local_(local, a2, b2, &ab));
		Return(multi_rational_local_(local, left, a, &a));
		Return(multi_rational_local_(local, left, b, &b));
		Return(div_rational_local_(local, a, ab, &a));
		Return(div_rational_local_(local, b, ab, &b));
		Return(sign_reverse_rational_local_(local, b, &b));
		Return(complex_heap_(ret, a, b));
		rollback_local(local, stack);
	}

	return 0;
}

int div_complex_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int check;
	LocalStack stack;
	addr a, b;

	/* Re: a/n
	 * Im: b/n
	 */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	Check(! rationalp(right), "type right error");
	Return(zerop_rational_(right, &check));
	if (check) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(div_complex_real_(left, right, ret, &check));
	if (check) {
		GetRealComplex(left, &a);
		GetImagComplex(left, &b);
		push_local(local, &stack);
		Return(div_rational_local_(local, a, right, &a));
		Return(div_rational_local_(local, b, right, &b));
		Return(complex_heap_(ret, a, b));
		rollback_local(local, stack);
	}

	return 0;
}

int div_fc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	return div_rational_complex_common_(local, left, right, ret);
}

int div_cf_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_FIXNUM);
	return div_complex_rational_common_(local, left, right, ret);
}

int div_bc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	return div_rational_complex_common_(local, left, right, ret);
}

int div_cb_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_BIGNUM);
	return div_complex_rational_common_(local, left, right, ret);
}

int div_rc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	return div_rational_complex_common_(local, left, right, ret);
}

int div_cr_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_RATIO);
	return div_complex_rational_common_(local, left, right, ret);
}

int div_sc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(div_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

int div_cs_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(div_complex_real_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

int div_dc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(div_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

int div_cd_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(div_complex_real_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

int div_lc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(div_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

int div_cl_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(div_complex_real_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

static int div_cc_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b, c, d, ac, bd, bc, ad, c2, d2;

	/* (a+bi)(c+di) =
	 *   Re: (ac + bd)/(c*c + d*d)
	 *   Im: (bc - ad)/(c*c + d*d)
	 */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_COMPLEX);
	GetRealComplex(left, &a);
	GetImagComplex(left, &b);
	GetRealComplex(right, &c);
	GetImagComplex(right, &d);
	push_local(local, &stack);
	Return(multi_rational_local_(local, c, c, &c2));
	Return(multi_rational_local_(local, d, d, &d2));
	Return(multi_rational_local_(local, a, c, &ac));
	Return(multi_rational_local_(local, b, d, &bd));
	Return(multi_rational_local_(local, b, c, &bc));
	Return(multi_rational_local_(local, a, d, &ad));
	Return(plus_rational_local_(local, ac, bd, &ac));
	Return(minus_rational_local_(local, bc, ad, &bc));
	Return(plus_rational_local_(local, c2, d2, &c2));
	Return(div_rational_local_(local, ac, c2, &ac));
	Return(div_rational_local_(local, bc, c2, &bc));
	Return(complex_heap_(ret, ac, bc));
	rollback_local(local, stack);

	return 0;
}

int div_cc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	enum MathType type;
	single_float as, bs, cs, ds, xs, ys, zs;
	double_float ad, bd, cd, dd, xd, yd, zd;
	long_float al, bl, cl, dl, xl, yl, zl;
	struct mathcomplex2_struct str;

	/* (a+bi)(c+di) =
	 *   Re: (ac + bd)/(c*c + d*d)
	 *   Im: (bc - ad)/(c*c + d*d)
	 */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			as = str.v.s.a;
			bs = str.v.s.b;
			cs = str.v.s.c;
			ds = str.v.s.d;
			xs = as*cs + bs*ds;
			ys = bs*cs - as*ds;
			zs = cs*cs + ds*ds;
			return complex_single_heap_(ret, xs/zs, ys/zs);

		case MathType_double:
			ad = str.v.d.a;
			bd = str.v.d.b;
			cd = str.v.d.c;
			dd = str.v.d.d;
			xd = ad*cd + bd*dd;
			yd = bd*cd - ad*dd;
			zd = cd*cd + dd*dd;
			return complex_double_heap_(ret, xd/zd, yd/zd);

		case MathType_long:
			al = str.v.l.a;
			bl = str.v.l.b;
			cl = str.v.l.c;
			dl = str.v.l.d;
			xl = al*cl + bl*dl;
			yl = bl*cl - al*dl;
			zl = cl*cl + dl*dl;
			return complex_long_heap_(ret, xl/zl, yl/zl);

		case MathType_rational:
			return div_cc_rational_common_(local, left, right, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}


/************************************************************
 *  cmpl_plus.c
 ************************************************************/

/*
 *  oneplus
 */
int oneplus_complex_heap_(LocalRoot local, addr pos, addr *ret)
{
	enum ComplexType type;
	addr real, imag;

	Check(! complexp(pos), "type error");
	switch (getcomplex(pos, &type, &real, &imag)) {
		case ComplexType_single:
			Return(plus_float_sv_heap_(real, 1.0f, &real));
			break;

		case ComplexType_double:
			Return(plus_float_dv_heap_(real, 1.0, &real));
			break;

		case ComplexType_long:
			Return(plus_float_lv_heap_(real, 1.0L, &real));
			break;

		case ComplexType_rational:
			Return(oneplus_rational_common_(local, real, &real));
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}

	return complex_heap_(ret, real, imag);
}

int oneminus_complex_heap_(LocalRoot local, addr pos, addr *ret)
{
	enum ComplexType type;
	addr real, imag;

	Check(! complexp(pos), "type error");
	switch (getcomplex(pos, &type, &real, &imag)) {
		case ComplexType_single:
			Return(plus_float_sv_heap_(real, -1.0f, &real));
			break;

		case ComplexType_double:
			Return(plus_float_dv_heap_(real, -1.0, &real));
			break;

		case ComplexType_long:
			Return(plus_float_lv_heap_(real, -1.0L, &real));
			break;

		case ComplexType_rational:
			Return(oneminus_rational_common_(local, real, &real));
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}

	return complex_heap_(ret, real, imag);
}


/*
 *  plus
 */
static int plus_real_complex_(addr left, addr right, addr *value, int *ret)
{
	enum MathType type;
	struct mathcomplex2_struct str;

	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			Return(complex_single_heap_(value, str.v.s.a+str.v.s.c, str.v.s.d));
			return Result(ret, 0);

		case MathType_double:
			Return(complex_double_heap_(value, str.v.d.a+str.v.d.c, str.v.d.d));
			return Result(ret, 0);

		case MathType_long:
			Return(complex_long_heap_(value, str.v.l.a+str.v.l.c, str.v.l.d));
			return Result(ret, 0);

		case MathType_rational:
			return Result(ret, 1);

		case MathType_complex:
		case MathType_error:
		default:
			*value = Nil;
			*ret = 0;
			return fmte_("type error", NULL);
	}
}

int plus_rational_complex_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int check;
	addr real, imag;

	CheckLocal(local);
	Check(! rationalp(left), "type error");
	CheckType(right, LISPTYPE_COMPLEX);
	Return(plus_real_complex_(left, right, ret, &check));
	if (check) {
		GetRealComplex(right, &real);
		GetImagComplex(right, &imag);
		Return(plus_rational_common_(local, left, real, &real));
		return complex_heap_(ret, real, imag);
	}

	return 0;
}

int plus_fc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	return plus_rational_complex_common_(local, left, right, ret);
}

int plus_bc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	return plus_rational_complex_common_(local, left, right, ret);
}

int plus_rc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	return plus_rational_complex_common_(local, left, right, ret);
}

int plus_sc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(plus_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

int plus_dc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(plus_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

int plus_lc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(plus_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

static int plus_cc_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b, c, d;

	/* (a+bi) + (c+di) = a+c + (b+d)i */
	Check(GetTypeComplex(left) != ComplexType_rational, "type error");
	Check(GetTypeComplex(right) != ComplexType_rational, "type error");
	push_local(local, &stack);
	GetRealComplex(left, &a);
	GetImagComplex(left, &b);
	GetRealComplex(right, &c);
	GetImagComplex(right, &d);
	Return(plus_rational_local_(local, a, c, &a));
	Return(plus_rational_local_(local, b, d, &b));
	Return(complex_heap_(ret, a, b));
	rollback_local(local, stack);

	return 0;
}

int plus_cc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	enum MathType type;
	struct mathcomplex2_struct str;

	/* (a+bi) + (c+di) = a+c + (b+d)i */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			return complex_single_heap_(ret, str.v.s.a+str.v.s.c, str.v.s.b+str.v.s.d);

		case MathType_double:
			return complex_double_heap_(ret, str.v.d.a+str.v.d.c, str.v.d.b+str.v.d.d);

		case MathType_long:
			return complex_long_heap_(ret, str.v.l.a+str.v.l.c, str.v.l.b+str.v.l.d);

		case MathType_rational:
			return plus_cc_rational_common_(local, left, right, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}


/*
 *  minus
 */
static int minus_real_complex_(addr left, addr right, addr *value, int *ret)
{
	enum MathType type;
	struct mathcomplex2_struct str;

	/* n-(a+bi) = n-a + (-b)i */
	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			Return(complex_single_heap_(value, str.v.s.a-str.v.s.c, -str.v.s.d));
			return Result(ret, 0);

		case MathType_double:
			Return(complex_double_heap_(value, str.v.d.a-str.v.d.c, -str.v.d.d));
			return Result(ret, 0);

		case MathType_long:
			Return(complex_long_heap_(value, str.v.l.a-str.v.l.c, -str.v.l.d));
			return Result(ret, 0);

		case MathType_rational:
			return Result(ret, 1);

		case MathType_complex:
		case MathType_error:
		default:
			*value = NULL;
			*ret = 0;
			return fmte_("type error", NULL);
	}
}

static int minus_complex_real_(addr left, addr right, addr *value, int *ret)
{
	enum MathType type;
	struct mathcomplex2_struct str;

	/* (a+bi)-n = a-n + bi */
	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			Return(complex_single_heap_(value, str.v.s.a-str.v.s.c, str.v.s.b));
			return Result(ret, 0);

		case MathType_double:
			Return(complex_double_heap_(value, str.v.d.a-str.v.d.c, str.v.d.b));
			return Result(ret, 0);

		case MathType_long:
			Return(complex_long_heap_(value, str.v.l.a-str.v.l.c, str.v.l.b));
			return Result(ret, 0);

		case MathType_rational:
			return Result(ret, 1);

		case MathType_complex:
		case MathType_error:
		default:
			*value = NULL;
			*ret = 0;
			return fmte_("type error", NULL);
	}
}

int minus_rational_complex_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int check;
	LocalStack stack;
	addr a, b;

	/* n-(a+bi) = n-a + (-b)i */
	CheckLocal(local);
	Check(! rationalp(left), "type error");
	CheckType(right, LISPTYPE_COMPLEX);
	Return(minus_real_complex_(left, right, ret, &check));
	if (check) {
		GetRealComplex(right, &a);
		GetImagComplex(right, &b);
		push_local(local, &stack);
		Return(minus_rational_local_(local, left, a, &a));
		Return(sign_reverse_rational_local_(local, b, &b));
		Return(complex_heap_(ret, a, b));
		rollback_local(local, stack);
	}

	return 0;
}

int minus_complex_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int check;
	LocalStack stack;
	addr a, b;

	/* (a+bi)-n = a-n + bi */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	Check(! rationalp(right), "type right error");
	Return(minus_complex_real_(left, right, ret, &check));
	if (check) {
		GetRealComplex(left, &a);
		GetImagComplex(left, &b);
		push_local(local, &stack);
		Return(minus_rational_local_(local, a, right, &a));
		Return(complex_heap_(ret, a, b));
		rollback_local(local, stack);
	}

	return 0;
}

int minus_fc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	return minus_rational_complex_common_(local, left, right, ret);
}

int minus_cf_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_FIXNUM);
	return minus_complex_rational_common_(local, left, right, ret);
}

int minus_bc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	return minus_rational_complex_common_(local, left, right, ret);
}

int minus_cb_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_BIGNUM);
	return minus_complex_rational_common_(local, left, right, ret);
}

int minus_rc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	return minus_rational_complex_common_(local, left, right, ret);
}

int minus_cr_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_RATIO);
	return minus_complex_rational_common_(local, left, right, ret);
}

int minus_sc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(minus_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

int minus_cs_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(minus_complex_real_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

int minus_dc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(minus_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

int minus_cd_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(minus_complex_real_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

int minus_lc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(minus_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

int minus_cl_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(minus_complex_real_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

static int minus_cc_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b, c, d;

	/* (a+bi) - (c+di) = a-c + (b-d)i */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_COMPLEX);
	GetRealComplex(left, &a);
	GetImagComplex(left, &b);
	GetRealComplex(right, &c);
	GetImagComplex(right, &d);
	push_local(local, &stack);
	Return(minus_rational_local_(local, a, c, &a));
	Return(minus_rational_local_(local, b, d, &b));
	Return(complex_heap_(ret, a, b));
	rollback_local(local, stack);

	return 0;
}

int minus_cc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	enum MathType type;
	struct mathcomplex2_struct str;

	/* (a+bi) - (c+di) = a-c + (b-d)i */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			return complex_single_heap_(ret, str.v.s.a-str.v.s.c, str.v.s.b-str.v.s.d);

		case MathType_double:
			return complex_double_heap_(ret, str.v.d.a-str.v.d.c, str.v.d.b-str.v.d.d);

		case MathType_long:
			return complex_long_heap_(ret, str.v.l.a-str.v.l.c, str.v.l.b-str.v.l.d);

		case MathType_rational:
			return minus_cc_rational_common_(local, left, right, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}


/************************************************************
 *  code.c
 ************************************************************/

void init_code(void)
{
	init_code_init();
	init_optimize_common();
}

void build_code(void)
{
	build_code_init();
	build_optimize_common();
}


/************************************************************
 *  code_function.c
 ************************************************************/

/*
 *  system
 */
int nop_code(Execute ptr, CodeValue x)
{
	return 0;
}

#ifdef LISP_DEBUG
static void fixnum_null_heap(addr *ret, CodeValue x)
{
	if (x.pos == Nil)
		*ret = Nil;
	else
		fixnum_heap(ret, x.value);
}

int begin_code(Execute ptr, CodeValue x)
{
	addr ignore, pos;

	push_control(ptr, &ignore);
	fixnum_null_heap(&pos, x);
	pushdebug_control(ptr, pos);

	return 0;
}

int begin_call_code(Execute ptr, CodeValue x)
{
	addr ignore, pos;

	push_args_control(ptr, &ignore);
	fixnum_null_heap(&pos, x);
	pushdebug_control(ptr, pos);

	return 0;
}
#else
int begin_code(Execute ptr, CodeValue x)
{
	addr ignore;
	push_control(ptr, &ignore);
	return 0;
}

int begin_call_code(Execute ptr, CodeValue x)
{
	addr ignore;
	push_args_control(ptr, &ignore);
	return 0;
}
#endif

#ifdef LISP_DEBUG
int end_code(Execute ptr, CodeValue x)
{
	addr pos;
	fixnum check;

	if (! getdebug_control(ptr, &pos))
		return fmte_("end-code error, getdebug.", NULL);
	if (pos == Nil) {
		if (x.pos != Nil)
			return fmte_("end-code error, null check ~S.", x.pos, NULL);
	}
	else {
		if (! fixnump(pos))
			return fmte_("end-code error, object ~S.", pos, NULL);
		GetFixnum(pos, &check);
		if (check != x.value)
			return fmte_("end-code error, check ~S.", pos, NULL);
	}

	return pop_control_(ptr, ptr->control);
}
#else
int end_code(Execute ptr, CodeValue x)
{
	return pop_control_(ptr, ptr->control);
}
#endif

int escape_code(Execute ptr, CodeValue x)
{
	if (ptr->throw_value != throw_normal)
		return goto_control_(ptr, x.index);

	return 0;
}

int escape_not_code(Execute ptr, CodeValue x)
{
	if (ptr->throw_value == throw_normal)
		return goto_control_(ptr, x.index);

	return 0;
}

int save_code(Execute ptr, CodeValue x)
{
	save_control(ptr);
	return 0;
}

int restore_code(Execute ptr, CodeValue x)
{
	return restore_control_(ptr);
}

int normal_code(Execute ptr, CodeValue x)
{
	normal_throw_control(ptr);
	return 0;
}

int revert_code(Execute ptr, CodeValue x)
{
	return revert_control_(ptr);
}

int revert_goto_code(Execute ptr, CodeValue x)
{
	return revert_goto_control_(ptr, x.index);
}


/*
 *  object
 */
int set_code(Execute ptr, CodeValue x)
{
	setresult_control(ptr, x.pos);
	return 0;
}

int push_code(Execute ptr, CodeValue x)
{
	pushargs_control(ptr, x.pos);
	return 0;
}

int push_result_code(Execute ptr, CodeValue x)
{
	getresult_control(ptr, &x.pos);
	pushargs_control(ptr, x.pos);
	return 0;
}

int push_values_code(Execute ptr, CodeValue x)
{
	pushargs_allvalues(ptr);
	return 0;
}

int nil_set_code(Execute ptr, CodeValue x)
{
	setresult_control(ptr, Nil);
	return 0;
}

int nil_push_code(Execute ptr, CodeValue x)
{
	pushargs_control(ptr, Nil);
	return 0;
}

int t_set_code(Execute ptr, CodeValue x)
{
	setresult_control(ptr, T);
	return 0;
}

int t_push_code(Execute ptr, CodeValue x)
{
	pushargs_control(ptr, T);
	return 0;
}


/*
 *  symbol
 */
int lexical_code(Execute ptr, CodeValue x)
{
	addr list, pos;
	size_t index;

	/* allocate */
	GetCons(x.pos, &pos, &list);
	GetIndex(pos, &index);
	lexical_control(ptr, index);

	/* closure */
	while (list != Nil) {
		GetCons(list, &pos, &list);
		CheckType(pos, LISPTYPE_INDEX);
		GetIndex(pos, &index);
		reference_lexical_control(ptr, index);
	}

	return 0;
}

int lexical_set_code(Execute ptr, CodeValue x)
{
	addr pos;

	get_lexical_control(ptr, x.index, &pos);
	setresult_control(ptr, pos);

	return 0;
}

int lexical_push_code(Execute ptr, CodeValue x)
{
	addr pos;

	get_lexical_control(ptr, x.index, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

int lexical_rem_code(Execute ptr, CodeValue x)
{
#ifdef LISP_DEBUG
	get_lexical_control(ptr, x.index, &x.pos);
#endif
	return 0;
}

int special_set_code(Execute ptr, CodeValue x)
{
	Return(symbol_special_restart_(ptr, x.pos, &x.pos));
	setresult_control(ptr, x.pos);
	return 0;
}

int special_push_code(Execute ptr, CodeValue x)
{
	Return(symbol_special_restart_(ptr, x.pos, &x.pos));
	pushargs_control(ptr, x.pos);
	return 0;
}

int special_rem_code(Execute ptr, CodeValue x)
{
	return symbol_special_restart_(ptr, x.pos, &x.pos);
}


/*
 *  declaim
 */
int declaim_special_code(Execute ptr, CodeValue x)
{
	return setspecial_symbol_(x.pos);
}

int declaim_type_value_code(Execute ptr, CodeValue x)
{
	addr symbol, type;

	List_bind(x.pos, &symbol, &type, NULL);
	return settype_value_symbol_(symbol, type);
}

int declaim_type_function_code(Execute ptr, CodeValue x)
{
	addr key, symbol, type;

	List_bind(x.pos, &key, &type, NULL);
	GetCallName(key, &symbol);
	if (symbolp_callname(key))
		return settype_function_symbol_(symbol, type);
	else
		return settype_setf_symbol_(symbol, type);
}

int declaim_inline_code(Execute ptr, CodeValue x)
{
	addr symbol;

	GetCallName(x.pos, &symbol);
	if (symbolp_callname(x.pos))
		setinline_function_symbol(symbol);
	else
		setinline_setf_symbol(symbol);

	return 0;
}

int declaim_notinline_code(Execute ptr, CodeValue x)
{
	addr symbol;

	GetCallName(x.pos, &symbol);
	if (symbolp_callname(x.pos))
		setnotinline_function_symbol(symbol);
	else
		setnotinline_setf_symbol(symbol);

	return 0;
}

int declaim_compilation_code(Execute ptr, CodeValue x)
{
	apply_compilation_speed_declaim((OptimizeType)x.value);
	return 0;
}

int declaim_debug_code(Execute ptr, CodeValue x)
{
	apply_debug_declaim((OptimizeType)x.value);
	return 0;
}

int declaim_safety_code(Execute ptr, CodeValue x)
{
	apply_safety_declaim((OptimizeType)x.value);
	return 0;
}

int declaim_space_code(Execute ptr, CodeValue x)
{
	apply_space_declaim((OptimizeType)x.value);
	return 0;
}

int declaim_speed_code(Execute ptr, CodeValue x)
{
	apply_speed_declaim((OptimizeType)x.value);
	return 0;
}

int declaim_declaration_code(Execute ptr, CodeValue x)
{
	push_declaration_declaim(x.pos);
	return 0;
}


/*
 *  let
 */
static int typep_error_code_(Execute ptr, addr value, addr type)
{
	int check;
	addr call;

	Return(typep_clang_(ptr, value, type, &check));
	if (check)
		return 0;

	copyheap(&value, value);
	type_copy_heap(&type, type);
	(void)getcall_control(ptr, &call);
	if (callnamep(call))
		name_callname_heap(call, &call);

	return call_type_error_va_(ptr, value, type,
			"The value ~S must be a ~S type in ~S.",
			value, type, call, NULL);
}

int type_result_code(Execute ptr, CodeValue x)
{
	/* the-set-code */
	addr value;
	getresult_control(ptr, &value);
	return typep_error_code_(ptr, value, x.pos);
}

int type_lexical_code(Execute ptr, CodeValue x)
{
	addr pos, type;
	size_t index;

	List_bind(x.pos, &pos, &type, NULL);
	GetIndex(pos, &index);
	get_lexical_control(ptr, index, &pos);

	return typep_error_code_(ptr, pos, type);
}

int type_special_code(Execute ptr, CodeValue x)
{
	addr pos, type;
	List_bind(x.pos, &pos, &type, NULL);
	getspecial_local(ptr, pos, &pos);
	return call_typep_unbound_error_(ptr, pos, type);
}

int type_global_code(Execute ptr, CodeValue x)
{
	addr pos, type;
	List_bind(x.pos, &pos, &type, NULL);
	GetValueSymbol(pos, &pos);
	return call_typep_unbound_error_(ptr, pos, type);
}

int type_function_code(Execute ptr, CodeValue x)
{
	addr pos, type;
	List_bind(x.pos, &pos, &type, NULL);
	GetFunctionSymbol(pos, &pos);
	return call_typep_unbound_error_(ptr, pos, type);
}

int type_setf_code(Execute ptr, CodeValue x)
{
	addr pos, type;
	List_bind(x.pos, &pos, &type, NULL);
	getsetf_symbol(pos, &pos);
	return call_typep_unbound_error_(ptr, pos, type);
}

int let_lexical_code(Execute ptr, CodeValue x)
{
	addr src, dst, value;
	size_t srci, dsti;

	List_bind(x.pos, &src, &dst, NULL);
	GetIndex(src, &srci);
	GetIndex(dst, &dsti);
	/* src -> dst */
	get_lexical_control(ptr, srci, &value);
	set_lexical_control(ptr, dsti, value);

	return 0;
}

int let_special_code(Execute ptr, CodeValue x)
{
	addr src, dst, value;
	size_t index;

	List_bind(x.pos, &src, &dst, NULL);
	GetIndex(src, &index);
	/* src -> dst */
	get_lexical_control(ptr, index, &value);
	pushspecial_control(ptr, dst, value);

	return 0;
}

int leta_special_code(Execute ptr, CodeValue x)
{
	addr value;

	getresult_control(ptr, &value);
	pushspecial_control(ptr, x.pos, value);

	return 0;
}


/*
 *  setq
 */
#define Return_check_readonly_variable_(x) { \
	if (GetStatusReadOnly(x)) { \
		return fmte_("Cannot set value to the constant variable ~S.", x, NULL); \
	} \
}

int setq_lexical_code(Execute ptr, CodeValue x)
{
	addr value;

	getresult_control(ptr, &value);
	set_lexical_control(ptr, x.index, value);

	return 0;
}

int setq_special_code(Execute ptr, CodeValue x)
{
	addr value;

	getresult_control(ptr, &value);
	Return_check_readonly_variable_(x.pos);
	setspecial_local(ptr, x.pos, value);

	return 0;
}

int setq_global_code(Execute ptr, CodeValue x)
{
	addr value;

	getresult_control(ptr, &value);
	Return_check_readonly_variable_(x.pos);
	SetValueSymbol(x.pos, value);

	return 0;
}


/*
 *  function
 */
int function_set_code(Execute ptr, CodeValue x)
{
	Return(function_global_restart_(ptr, x.pos, &x.pos));
	setresult_control(ptr, x.pos);
	return 0;
}

int function_push_code(Execute ptr, CodeValue x)
{
	Return(function_global_restart_(ptr, x.pos, &x.pos));
	pushargs_control(ptr, x.pos);
	return 0;
}

int setf_set_code(Execute ptr, CodeValue x)
{
	Return(setf_global_restart_(ptr, x.pos, &x.pos));
	setresult_control(ptr, x.pos);
	return 0;
}

int setf_push_code(Execute ptr, CodeValue x)
{
	Return(setf_global_restart_(ptr, x.pos, &x.pos));
	pushargs_control(ptr, x.pos);
	return 0;
}


/*
 *  define
 */
int defmacro_code(Execute ptr, CodeValue x)
{
	addr value;

	getresult_control(ptr, &value);
	Return(setmacro_symbol_(x.pos, value));
	setresult_control(ptr, x.pos);

	return 0;
}

int deftype_code(Execute ptr, CodeValue x)
{
	addr pos, symbol, doc;

	List_bind(x.pos, &symbol, &doc, NULL);
	getresult_control(ptr, &pos);
	Return(setdeftype_(symbol, pos));
	Return(set_documentation_function_object_(pos, doc));
	setresult_control(ptr, symbol);

	return 0;
}

int define_compiler_macro_code(Execute ptr, CodeValue x)
{
	addr pos, name, doc;

	List_bind(x.pos, &name, &doc, NULL);
	getresult_control(ptr, &pos);
	Return(set_documentation_function_object_(pos, doc));
	Return(set_define_compiler_macro_(name, pos));
	name_callname_heap(name, &name);
	setresult_control(ptr, name);

	return 0;
}

int defun_code(Execute ptr, CodeValue x)
{
	addr pos, call, symbol;

	getresult_control(ptr, &pos);
	GetNameFunction(pos, &call);
	GetCallName(call, &symbol);

	if (symbolp_callname(call)) {
		Return(setfunction_symbol_(symbol, pos));
		setresult_control(ptr, symbol);
	}
	else {
		Return(setsetf_symbol_(symbol, pos));
		GetConst(COMMON_SETF, &pos);
		list_heap(&pos, pos, symbol, NULL);
		setresult_control(ptr, pos);
	}

	return 0;
}


/*
 *  call
 */
int call_name_code(Execute ptr, CodeValue x)
{
	SetControl(ptr->control, Control_Call, x.pos);
	return 0;
}

int call_result_code(Execute ptr, CodeValue x)
{
	getresult_control(ptr, &x.pos);
	return execute_control_(ptr, x.pos);
}

int call_type_code(Execute ptr, CodeValue x)
{
	addr value;
	getargs_tail_control(ptr, &value);
	return typep_error_code_(ptr, value, x.pos);
}

int call_key_code(Execute ptr, CodeValue x)
{
	addr list, key, value, pos, type;

	list = x.pos;
	getresult_control(ptr, &value);
	getargs_tail_control(ptr, &key);

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &type);
		if (pos == key) {
			return call_typep_asterisk_error_(ptr, value, type);
		}
	}

	return fmte_("Invalid argument key ~S.", key, NULL);
}

int call_function_code(Execute ptr, CodeValue x)
{
	addr value;

	GetFunctionSymbol(x.pos, &value);
	if (value == Unbound) {
		Return(function_global_restart_(ptr, x.pos, &value));
	}

	return execute_control_(ptr, value);
}

int call_setf_code(Execute ptr, CodeValue x)
{
	addr value;

	getsetf_symbol(x.pos, &value);
	if (value == Unbound) {
		Return(setf_global_restart_(ptr, x.pos, &value));
	}

	return execute_control_(ptr, value);
}

int call_lexical_code(Execute ptr, CodeValue x)
{
	addr value;
	getlow_lexical_control(ptr, x.index, &value);
	return execute_control_(ptr, value);
}


/*
 *  values
 */
int values_nil_code(Execute ptr, CodeValue x)
{
	setvalues_nil_control(ptr);
	return 0;
}

int values_set_code(Execute ptr, CodeValue x)
{
	addr pos;

	getargs_list_control_unsafe(ptr, 0, &pos);
	setvalues_list_control(ptr, pos);

	return 0;
}

int the_set_code(Execute ptr, CodeValue x)
{
	return values_typep_error_(ptr, x.pos);
}

int the_push_code(Execute ptr, CodeValue x)
{
	addr value;

	Return(the_set_code(ptr, x));
	getresult_control(ptr, &value);
	pushargs_control(ptr, value);

	return 0;
}


/*
 *  control
 */
int if_unbound_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	if (pos == Unbound)
		return goto_control_(ptr, x.index);

	return 0;
}

int if_nil_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	if (pos == Nil)
		return goto_control_(ptr, x.index);

	return 0;
}

int if_t_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	if (pos != Nil)
		return goto_control_(ptr, x.index);

	return 0;
}

int goto_code(Execute ptr, CodeValue x)
{
	return goto_control_(ptr, x.index);
}

int go_code(Execute ptr, CodeValue x)
{
	addr pos;
	get_lexical_control(ptr, x.index, &pos);
	return go_control_(ptr, pos);
}

int return_from_code(Execute ptr, CodeValue x)
{
	addr pos;
	get_lexical_control(ptr, x.index, &pos);
	return return_from_control_(ptr, pos);
}

int catch_code(Execute ptr, CodeValue x)
{
	getresult_control(ptr, &x.pos);
	catch_control(ptr, x.pos);
	return 0;
}

int throw_operator_code(Execute ptr, CodeValue x)
{
	getargs_control(ptr, 0, &x.pos);
	return throw_control_(ptr, x.pos);
}

int taginfo_code(Execute ptr, CodeValue x)
{
	set_taginfo_control(ptr, x.pos);
	return 0;
}

int blockinfo_code(Execute ptr, CodeValue x)
{
	set_blockinfo_control(ptr, x.pos);
	return 0;
}


/*
 *  control-switch
 */
static int push_handler_code_(Execute ptr, int escape)
{
	addr args, symbol, lambda;

	getargs_list_control_unsafe(ptr, 0, &args);
	while (args != Nil) {
		GetCons(args, &symbol, &args);
		GetCons(args, &lambda, &args);
		Return(pushhandler_common_(ptr, symbol, lambda, escape));
	}
	reverse_handler_control(ptr);

	return 0;
}

int handler_bind_code(Execute ptr, CodeValue x)
{
	return push_handler_code_(ptr, 0);
}

int handler_case_code(Execute ptr, CodeValue x)
{
	return push_handler_code_(ptr, 1);
}

static void push_restart_code(Execute ptr, int escape)
{
	addr args, list;

	getargs_list_control_unsafe(ptr, 0, &args);
	while (args != Nil) {
		GetCons(args, &list, &args);
		pushbind_restart_control(ptr, list, escape);
	}
	reverse_restart_control(ptr);
}

int restart_bind_code(Execute ptr, CodeValue x)
{
	push_restart_code(ptr, 0);
	return 0;
}

int restart_case_code(Execute ptr, CodeValue x)
{
	push_restart_code(ptr, 1);
	return 0;
}

int restart_progn_code(Execute ptr, CodeValue x)
{
	addr list, pos;

	getresult_control(ptr, &list);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		pushrestart_control(ptr, pos);
	}
	reverse_restart_control(ptr);

	return 0;
}


/*
 *  eval
 */
int funcall_code(Execute ptr, CodeValue x)
{
	int check;
	addr pos, list;

	/* multiple-value-call only used. */
	getargs_list_control_unsafe(ptr, 0, &pos);
	GetCons(pos, &pos, &list);
	Return(funcallp_(pos, &check));
	if (! check)
		return TypeError_(pos, FUNCTION);

	return apply_control_(ptr, pos, list);
}

int nth_value_code(Execute ptr, CodeValue x)
{
	int check;
	addr pos;
	size_t index;

	getargs_control(ptr, 0, &pos);
	if (! integerp(pos))
		return fmte_("NTH-VALUE argument ~S must be integer type.", pos, NULL);
	Return(zerop_or_plusp_integer_(pos, &check));
	if (! check)
		return fmte_("NTH-VALUE argument ~S must be greater than equal to 0.", pos, NULL);
	if (GetIndex_integer(pos, &index)) {
		setresult_control(ptr, Nil);
	}
	else {
		getvalues_control(ptr, index, &pos);
		setresult_control(ptr, (pos == Unbound)? Nil: pos);
	}

	return 0;
}

int progv_code(Execute ptr, CodeValue x)
{
	addr symbols, values, symbol, value;

	getargs_control(ptr, 0, &symbols);
	getargs_control(ptr, 1, &values);
	while (symbols != Nil) {
		if (! consp(symbols))
			return fmte_("PROGV form ~S must be a cons.", symbols, NULL);
		GetCons(symbols, &symbol, &symbols);
		if (! symbolp(symbol))
			return fmte_("PROGV argument ~S must be a symbol.", symbol, NULL);
		if (values == Nil) {
			pushspecial_control(ptr, symbol, Unbound);
		}
		else if (consp(values)) {
			GetCons(values, &value, &values);
			pushspecial_control(ptr, symbol, value);
		}
		else {
			return fmte_("PROGV form ~S must be a cons.", values, NULL);
		}
	}

	return 0;
}


/*
 *  load-time-value
 */
int load_alloc_code(Execute ptr, CodeValue x)
{
	execute_load_alloc(ptr, x.index);
	return 0;
}

int load_gensym_code(Execute ptr, CodeValue x)
{
	addr pos, index;
	size_t value;

	List_bind(x.pos, &pos, &index, NULL);
	GetIndex(index, &value);
	return execute_load_gensym_(ptr, pos, value);
}

int load_set_code(Execute ptr, CodeValue x)
{
	return execute_load_set_(ptr, x.index);
}

int reference_set_code(Execute ptr, CodeValue x)
{
	addr pos;

	Return(result_load_time_value_(ptr, x.pos, &pos));
	setresult_control(ptr, pos);

	return 0;
}

int reference_push_code(Execute ptr, CodeValue x)
{
	addr pos;

	Return(result_load_time_value_(ptr, x.pos, &pos));
	pushargs_control(ptr, pos);

	return 0;
}


/*
 *  step
 */
int step_code(Execute ptr, CodeValue x)
{
	if (! ptr->step_begin)
		return 0;
	ptr->step_depth++;

	/* step over */
	if (! ptr->step_in) {
		if (ptr->step_break <= ptr->step_depth)
			return 0;
	}

	/* step */
	return execute_step_code(ptr, x.pos);
}

int step_off_code(Execute ptr, CodeValue x)
{
	if (! ptr->step_begin)
		return 0;
	Check(ptr->step_depth == 0, "step_depth error.");
	ptr->step_depth--;

	/* step over */
	if (! ptr->step_in) {
		if (ptr->step_depth < ptr->step_break)
			ptr->step_in = 1;
	}

	return 0;
}

int step_begin_code(Execute ptr, CodeValue x)
{
	addr symbol, value;

	if (! ptr->step_begin) {
		value = Nil;
		ptr->step_begin = 1;
		ptr->step_in = 1;
		ptr->step_depth = 1;
		ptr->step_break = 1;
	}
	else {
		value = T;
	}
	GetConst(SYSTEM_STEP_BEGIN, &symbol);
	pushspecial_control(ptr, symbol, value);

	return 0;
}

int step_end_code(Execute ptr, CodeValue x)
{
	addr symbol, value;

	if (! ptr->step_begin)
		return 0;
	GetConst(SYSTEM_STEP_BEGIN, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &value));
	if (value == Nil)
		ptr->step_begin = 0;

	return 0;
}


/************************************************************
 *  code_init.c
 ************************************************************/

#define defcode(x,y) defcode_constant(CONSTANT_CODE_##x, p_##y)
#define initcode(x,y) { \
	SetPointer_code(p_##x, x); \
	CodeValueArray[p_##x] = (byte)CodeValueType_##y; \
}

byte CodeValueArray[p_size_code];

/*
 *  initialize
 */
void defcode_constant(constindex index, pointer p)
{
	addr symbol, pos;

	GetConstant(index, &symbol);
	Check(symbol == Nil || symbol == Unbound, "constant error");
	GetFunctionSymbol(symbol, &pos);
	Check(pos != Unbound, "code-function already exists.");
	compiled_system(&pos, symbol);
	setcompiled_code(pos, p);
	SetFunctionSymbol(symbol, pos);
}

void init_code_init(void)
{
	cleartype(CodeValueArray);

	/* system */
	initcode(nop_code, Null);
#ifdef LISP_DEBUG
	initcode(begin_code, FixnumNull);
	initcode(begin_call_code, FixnumNull);
	initcode(end_code, FixnumNull);
#else
	initcode(begin_code, Null);
	initcode(begin_call_code, Null);
	initcode(end_code, Null);
#endif
	initcode(escape_code, Index);
	initcode(escape_not_code, Index);
	initcode(save_code, Null);
	initcode(restore_code, Null);
	initcode(normal_code, Null);
	initcode(revert_code, Null);
	initcode(revert_goto_code, Index);

	/* object */
	initcode(set_code, Addr);
	initcode(push_code, Addr);
	initcode(push_result_code, Null);
	initcode(push_values_code, Null);
	initcode(nil_set_code, Null);
	initcode(nil_push_code, Null);
	initcode(t_set_code, Null);
	initcode(t_push_code, Null);

	/* symbol */
	initcode(lexical_code, Addr);
	initcode(lexical_set_code, Index);
	initcode(lexical_push_code, Index);
	initcode(lexical_rem_code, Index);
	initcode(special_set_code, Addr);
	initcode(special_push_code, Addr);
	initcode(special_rem_code, Addr);

	/* declaim */
	initcode(declaim_special_code, Addr);
	initcode(declaim_type_value_code, Addr);
	initcode(declaim_type_function_code, Addr);
	initcode(declaim_inline_code, Addr);
	initcode(declaim_notinline_code, Addr);
	initcode(declaim_compilation_code, Fixnum);
	initcode(declaim_debug_code, Fixnum);
	initcode(declaim_safety_code, Fixnum);
	initcode(declaim_space_code, Fixnum);
	initcode(declaim_speed_code, Fixnum);
	initcode(declaim_declaration_code, Addr);

	/* let */
	initcode(type_result_code, Addr);
	initcode(type_lexical_code, Addr); /* delete */
	initcode(type_special_code, Addr);
	initcode(type_global_code, Addr);
	initcode(type_function_code, Addr);
	initcode(type_setf_code, Addr);
	initcode(let_lexical_code, Addr);
	initcode(let_special_code, Addr);
	initcode(leta_special_code, Addr);

	/* setq */
	initcode(setq_lexical_code, Index);
	initcode(setq_special_code, Addr);
	initcode(setq_global_code, Addr);

	/* function */
	initcode(function_set_code, Addr);
	initcode(function_push_code, Addr);
	initcode(setf_set_code, Addr);
	initcode(setf_push_code, Addr);

	/* define */
	initcode(defmacro_code, Addr);
	initcode(deftype_code, Addr);
	initcode(define_compiler_macro_code, Addr);
	initcode(defun_code, Null);

	/* call */
	initcode(call_name_code, Addr);
	initcode(call_result_code, Addr);
	initcode(call_type_code, Addr);
	initcode(call_key_code, Addr);
	initcode(call_function_code, Addr);
	initcode(call_setf_code, Addr);
	initcode(call_lexical_code, Index);

	/* values */
	initcode(values_nil_code, Null);
	initcode(values_set_code, Null);
	initcode(the_set_code, Addr);
	initcode(the_push_code, Addr);

	/* control */
	initcode(if_unbound_code, Index);
	initcode(if_nil_code, Index);
	initcode(if_t_code, Index);
	initcode(goto_code, Index);
	initcode(go_code, Index);
	initcode(return_from_code, Index);
	initcode(catch_code, Addr);
	initcode(throw_operator_code, Addr);
	initcode(taginfo_code, Addr);
	initcode(blockinfo_code, Addr);

	/* control-switch */
	initcode(handler_bind_code, Null);
	initcode(handler_case_code, Null);
	initcode(restart_bind_code, Null);
	initcode(restart_case_code, Null);
	initcode(restart_progn_code, Null);

	/* eval */
	initcode(funcall_code, Addr);
	initcode(nth_value_code, Null);
	initcode(progv_code, Null);

	/* lambda */
	initcode(pop_code, Null);
	initcode(pop_unbound_code, Null);
	initcode(getf_code, Addr);
	initcode(rest_copy_code, Null);
	initcode(rest_bind_code, Null);
	initcode(allow_other_keys_code, Addr);
	initcode(rest_null_code, Addr);
	initcode(whole_code, Null);
	initcode(lambda_code, Addr);
	initcode(lambda_name_code, Addr);
	initcode(lambda_type_code, Addr);
	initcode(lambda_doc_code, Addr);
	initcode(lambda_form_code, Addr);
	initcode(lambda_defun_code, Addr);
	initcode(lambda_closure_code, Addr);
	initcode(lambda_lexical_code, Addr);
	initcode(lambda_cache_code, Addr);
	initcode(lambda_cache_set_code, Addr);
	initcode(macro_code, Addr);
	initcode(macro_special_code, Addr);
	initcode(macro_env_code, Null);
	initcode(macro_whole_code, Null);

	/* labels */
	initcode(labels_make_code, Addr);
	initcode(labels_lambda_code, Addr);

	/* multiple-value-bind */
	initcode(bind1_type_code, Addr);
	initcode(bind1_special_code, Addr);
	initcode(bind1_lexical_code, Addr);
	initcode(bind2_type_code, Addr);
	initcode(bind2_special_code, Addr);
	initcode(bind2_lexical_code, Index);

	/* load-time-value */
	initcode(load_alloc_code, Index);
	initcode(load_gensym_code, Addr);
	initcode(load_set_code, Index);
	initcode(reference_set_code, Addr);
	initcode(reference_push_code, Addr);

	/* step */
	initcode(step_code, Addr);
	initcode(step_off_code, Null);
	initcode(step_begin_code, Null);
	initcode(step_end_code, Null);
}


/*
 *  build
 */
void build_code_init(void)
{
	/* system */
	defcode(NOP, nop_code);
	defcode(BEGIN, begin_code);
	defcode(BEGIN_CALL, begin_call_code);
	defcode(END, end_code);
	defcode(ESCAPE, escape_code);
	defcode(ESCAPE_NOT, escape_not_code);
	defcode(SAVE, save_code);
	defcode(RESTORE, restore_code);
	defcode(NORMAL, normal_code);
	defcode(REVERT, revert_code);
	defcode(REVERT_GOTO, revert_goto_code);

	/* object */
	defcode(SET, set_code);
	defcode(PUSH, push_code);
	defcode(PUSH_RESULT, push_result_code);
	defcode(PUSH_VALUES, push_values_code);
	defcode(NIL_SET, nil_set_code);
	defcode(NIL_PUSH, nil_push_code);
	defcode(T_SET, t_set_code);
	defcode(T_PUSH, t_push_code);

	/* symbol */
	defcode(LEXICAL, lexical_code);
	defcode(LEXICAL_SET, lexical_set_code);
	defcode(LEXICAL_PUSH, lexical_push_code);
	defcode(LEXICAL_REM, lexical_rem_code);
	defcode(SPECIAL_SET, special_set_code);
	defcode(SPECIAL_PUSH, special_push_code);
	defcode(SPECIAL_REM, special_rem_code);

	/* declaim */
	defcode(DECLAIM_SPECIAL, declaim_special_code);
	defcode(DECLAIM_TYPE_VALUE, declaim_type_value_code);
	defcode(DECLAIM_TYPE_FUNCTION, declaim_type_function_code);
	defcode(DECLAIM_INLINE, declaim_inline_code);
	defcode(DECLAIM_NOTINLINE, declaim_notinline_code);
	defcode(DECLAIM_COMPILATION, declaim_compilation_code);
	defcode(DECLAIM_DEBUG, declaim_debug_code);
	defcode(DECLAIM_SAFETY, declaim_safety_code);
	defcode(DECLAIM_SPACE, declaim_space_code);
	defcode(DECLAIM_SPEED, declaim_speed_code);
	defcode(DECLAIM_DECLARATION, declaim_declaration_code);

	/* let */
	defcode(TYPE_RESULT, type_result_code);
	defcode(TYPE_LEXICAL, type_lexical_code); /* delete */
	defcode(TYPE_SPECIAL, type_special_code);
	defcode(TYPE_GLOBAL, type_global_code);
	defcode(TYPE_FUNCTION, type_function_code);
	defcode(TYPE_SETF, type_setf_code);
	defcode(LET_LEXICAL, let_lexical_code);
	defcode(LET_SPECIAL, let_special_code);
	defcode(LETA_SPECIAL, leta_special_code);

	/* setq */
	defcode(SETQ_LEXICAL, setq_lexical_code);
	defcode(SETQ_SPECIAL, setq_special_code);
	defcode(SETQ_GLOBAL, setq_global_code);

	/* function */
	defcode(FUNCTION_SET, function_set_code);
	defcode(FUNCTION_PUSH, function_push_code);
	defcode(SETF_SET, setf_set_code);
	defcode(SETF_PUSH, setf_push_code);

	/* define */
	defcode(DEFMACRO, defmacro_code);
	defcode(DEFTYPE, deftype_code);
	defcode(DEFINE_COMPILER_MACRO, define_compiler_macro_code);
	defcode(DEFUN, defun_code);

	/* call */
	defcode(CALL_NAME, call_name_code);
	defcode(CALL_RESULT, call_result_code);
	defcode(CALL_TYPE, call_type_code);
	defcode(CALL_KEY, call_key_code);
	defcode(CALL_FUNCTION, call_function_code);
	defcode(CALL_SETF, call_setf_code);
	defcode(CALL_LEXICAL, call_lexical_code);

	/* values */
	defcode(VALUES_NIL, values_nil_code);
	defcode(VALUES_SET, values_set_code);
	defcode(THE_SET, the_set_code);
	defcode(THE_PUSH, the_push_code);

	/* control */
	defcode(IF_UNBOUND, if_unbound_code);
	defcode(IF_NIL, if_nil_code);
	defcode(IF_T, if_t_code);
	defcode(GOTO, goto_code);
	defcode(GO, go_code);
	defcode(RETURN_FROM, return_from_code);
	defcode(CATCH, catch_code);
	defcode(THROW, throw_operator_code);
	defcode(TAGINFO, taginfo_code);
	defcode(BLOCKINFO, blockinfo_code);

	/* control-switch */
	defcode(HANDLER_BIND, handler_bind_code);
	defcode(HANDLER_CASE, handler_case_code);
	defcode(RESTART_BIND, restart_bind_code);
	defcode(RESTART_CASE, restart_case_code);
	defcode(RESTART_PROGN, restart_progn_code);

	/* eval */
	defcode(FUNCALL, funcall_code);
	defcode(NTH_VALUE, nth_value_code);
	defcode(PROGV, progv_code);

	/* lambda */
	defcode(POP, pop_code);
	defcode(POP_UNBOUND, pop_unbound_code);
	defcode(GETF, getf_code);
	defcode(REST_COPY, rest_copy_code);
	defcode(REST_BIND, rest_bind_code);
	defcode(ALLOW_OTHER_KEYS, allow_other_keys_code);
	defcode(REST_NULL, rest_null_code);
	defcode(WHOLE, whole_code);
	defcode(LAMBDA, lambda_code);
	defcode(LAMBDA_NAME, lambda_name_code);
	defcode(LAMBDA_TYPE, lambda_type_code);
	defcode(LAMBDA_DOC, lambda_doc_code);
	defcode(LAMBDA_FORM, lambda_form_code);
	defcode(LAMBDA_DEFUN, lambda_defun_code);
	defcode(LAMBDA_CLOSURE, lambda_closure_code);
	defcode(LAMBDA_LEXICAL, lambda_lexical_code);
	defcode(LAMBDA_CACHE, lambda_cache_code);
	defcode(LAMBDA_CACHE_SET, lambda_cache_set_code);
	defcode(MACRO, macro_code);
	defcode(MACRO_SPECIAL, macro_special_code);
	defcode(MACRO_ENV, macro_env_code);
	defcode(MACRO_WHOLE, macro_whole_code);

	/* labels */
	defcode(LABELS_MAKE, labels_make_code);
	defcode(LABELS_LAMBDA, labels_lambda_code);

	/* multiple-value-bind */
	defcode(BIND1_TYPE, bind1_type_code);
	defcode(BIND1_SPECIAL, bind1_special_code);
	defcode(BIND1_LEXICAL, bind1_lexical_code);
	defcode(BIND2_TYPE, bind2_type_code);
	defcode(BIND2_SPECIAL, bind2_special_code);
	defcode(BIND2_LEXICAL, bind2_lexical_code);

	/* load-time-value */
	defcode(LOAD_ALLOC, load_alloc_code);
	defcode(LOAD_GENSYM, load_gensym_code);
	defcode(LOAD_SET, load_set_code);
	defcode(REFERENCE_SET, reference_set_code);
	defcode(REFERENCE_PUSH, reference_push_code);

	/* step */
	defcode(STEP, step_code);
	defcode(STEP_OFF, step_off_code);
	defcode(STEP_BEGIN, step_begin_code);
	defcode(STEP_END, step_end_code);
}

#undef defcode
#undef initcode


/************************************************************
 *  code_lambda.c
 ************************************************************/

/*
 *  lambda object
 */
int pop_code(Execute ptr, CodeValue x)
{
	addr pos;

	Return(popargs_control_(ptr, &pos));
	if (pos == Unbound) {
		(void)getcall_control(ptr, &pos);
		if (callnamep(pos))
			name_callname_heap(pos, &pos);
		return fmte_("Too few argument at ~S arguments.", pos, NULL);
	}
	setresult_control(ptr, pos);

	return 0;
}

int pop_unbound_code(Execute ptr, CodeValue x)
{
	addr pos;

	Return(popargs_control_(ptr, &pos));
	setresult_control(ptr, pos);

	return 0;
}

int getf_code(Execute ptr, CodeValue x)
{
	addr list, key, value;

	GetArgsControl(ptr, &list);
	while (GetType(list) == LISPTYPE_CONS) {
		GetCons(list, &key, &list);
		if (GetType(list) != LISPTYPE_CONS)
			return fmte_("Invalid property list ~S.", list, NULL);
		GetCons(list, &value, &list);
		if (key == x.pos) {
			setresult_control(ptr, value);
			return 0;
		}
	}

	/* not found */
	setresult_control(ptr, Unbound);
	return 0;
}

int rest_copy_code(Execute ptr, CodeValue x)
{
	addr pos;

	getargs_list_control_unsafe(ptr, 0, &pos);
	copy_list_alloc_safe(NULL, &pos, pos);
	setresult_control(ptr, pos);

	return 0;
}

int rest_bind_code(Execute ptr, CodeValue x)
{
	addr pos;

	getargs_list_control_unsafe(ptr, 0, &pos);
	setresult_control(ptr, pos);

	return 0;
}

int allow_other_keys_code(Execute ptr, CodeValue x)
{
	addr list, key1, key2, keys;

	GetArgsControl(ptr, &list);

	/* :allow-other-keys t */
	GetConst(KEYWORD_ALLOW_OTHER_KEYS, &key1);
	if (! getplist_safe(list, key1, &key1)) {
		if (key1 != Nil)
			return 0;
	}

	/* check */
	while (GetType(list) == LISPTYPE_CONS) {
		GetCons(list, &key1, &list);
		keys = x.pos;
		for (;;) {
			if (keys == Nil)
				return fmte_("The key ~S cannot accept the function.", key1, NULL);
			GetCons(keys, &key2, &keys);
			if (key1 == key2)
				break;
		}
		if (GetType(list) != LISPTYPE_CONS)
			break;
		GetCdr(list, &list);
	}

	return 0;
}

static int rest_keys_code(Execute ptr)
{
	addr list, x, pos;
	const char *str;

	GetArgsControl(ptr, &list);
	while (list != Nil) {
		Return_getcons(list, &x, &list);
		if (! symbolp(x)) {
			str = "The key name ~S must be a symbol type at ~S.";
			goto error;
		}
		if (! consp_getcdr(list, &list)) {
			str = "There is no value in the ~S &key argument at ~S.";
			goto error;
		}
		Return_getcons(list, &x, &list);
	}
	return 0;

error:
	(void)getcall_control(ptr, &pos);
	if (callnamep(pos))
		name_callname_heap(pos, &pos);
	return fmte_(str, x, pos, NULL);
}

int rest_null_code(Execute ptr, CodeValue x)
{
	addr list, pos;

	if (x.pos != Nil)
		return rest_keys_code(ptr);

	/* rest-null */
	GetArgsControl(ptr, &list);
	if (list != Nil) {
		(void)getcall_control(ptr, &pos);
		if (callnamep(pos))
			name_callname_heap(pos, &pos);
		return fmte_("Too many argument at ~S arguments.", pos, NULL);
	}

	return 0;
}

int whole_code(Execute ptr, CodeValue x)
{
	addr list;

	/* (args) */
	getresult_control(ptr, &list);
	SetControl(ptr->control, Control_Cons, list);
	SetControl(ptr->control, Control_ConsTail, Nil);

	return 0;
}

int lambda_code(Execute ptr, CodeValue x)
{
	addr pos;
	function_heap(&pos, Nil, x.pos);
	setresult_control(ptr, pos);
	return 0;
}

int lambda_name_code(Execute ptr, CodeValue x)
{
	addr pos;
	getresult_control(ptr, &pos);
	SetNameFunction(pos, x.pos);
	return 0;
}

int lambda_type_code(Execute ptr, CodeValue x)
{
	addr pos;
	getresult_control(ptr, &pos);
	settype_function(pos, x.pos);
	return 0;
}

int lambda_doc_code(Execute ptr, CodeValue x)
{
	addr pos;
	getresult_control(ptr, &pos);
	return set_documentation_function_object_(pos, x.pos);
}

int lambda_form_code(Execute ptr, CodeValue x)
{
	addr pos;
	getresult_control(ptr, &pos);
	setlambda_expression_function(pos, x.pos);
	return 0;
}

int lambda_defun_code(Execute ptr, CodeValue x)
{
	addr pos;
	getresult_control(ptr, &pos);
	setdefunform_function(pos, x.pos);
	return 0;
}

static void getclosure_list_code(Execute ptr, addr pos, addr list, addr *ret)
{
	addr x, y, z;
	fixnum type;
	size_t dst, src;

	List_bind(list, &x, &y, &z, NULL);
	GetFixnum(x, &type);
	GetIndex(y, &src);
	GetIndex(z, &dst);

	switch ((enum EvalTable)type) {
		case EvalTable_Value:
			getlow_lexical_control(ptr, src, &x);
			closure_heap(ret, x, dst);
			break;

		case EvalTable_Function:
			getlow_lexical_control(ptr, src, &x);
			closure_heap(ret, x, dst);
			break;

		case EvalTable_TagBody:
			getlow_lexical_control(ptr, src, &x);
			closure_heap(ret, x, dst);
			break;

		case EvalTable_Block:
			getlow_lexical_control(ptr, src, &x);
			closure_heap(ret, x, dst);
			break;

		case EvalTable_Self:
			closure_heap(ret, pos, dst);
			break;

		default:
			Abort("Invalid eval-table type.");
			break;
	}
}

int lambda_closure_code(Execute ptr, CodeValue x)
{
	addr list, pos, root, value;

	getresult_control(ptr, &pos);
	list = x.pos;
	root = Nil;
	while (list != Nil) {
		GetCons(list, &value, &list);
		getclosure_list_code(ptr, pos, value, &value);
		cons_heap(&root, value, root);
	}
	nreverse(&root, root);
	SetDataFunction(pos, root);

	return 0;
}

int lambda_lexical_code(Execute ptr, CodeValue x)
{
	addr list, pos, data;
	size_t value;

	/* allocate */
	GetCons(x.pos, &pos, &list);
	GetIndex(pos, &value);
	lexical_control(ptr, value);

	/* restore closure */
	getdata_code_control(ptr, &data);
	while (data != Nil) {
		GetCons(data, &pos, &data);
		CheckType(pos, LISPSYSTEM_CLOSURE);
		value = lexical_closure(pos);
		get_closure(pos, &pos);
		setlow_lexical_control(ptr, value, pos);
	}

	/* set closure */
	while (list != Nil) {
		GetCons(list, &pos, &list);
		CheckType(pos, LISPTYPE_INDEX);
		GetIndex(pos, &value);
		reference_lexical_control(ptr, value);
	}

	return 0;
}

int lambda_cache_code(Execute ptr, CodeValue x)
{
	addr jump, pos;
	size_t index;

	List_bind(x.pos, &jump, &pos, NULL);
	GetValueSymbol(pos, &pos);
	if (pos == Unbound)
		return 0;

	/* cache hit */
	setresult_control(ptr, pos);
	GetIndex(jump, &index);
	return goto_control_(ptr, index);
}

int lambda_cache_set_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	CheckType(pos, LISPTYPE_FUNCTION);
	CheckType(x.pos, LISPTYPE_SYMBOL);
	SetValueSymbol(x.pos, pos);

	return 0;
}


/*
 *  macro object
 */
int macro_code(Execute ptr, CodeValue x)
{
	addr pos;
	macro_heap(&pos, Nil, x.pos);
	setresult_control(ptr, pos);
	return 0;
}

int macro_special_code(Execute ptr, CodeValue x)
{
	pushspecial_control(ptr, x.pos, Unbound);
	return 0;
}

int macro_env_code(Execute ptr, CodeValue x)
{
	addr list;

	GetArgsControl(ptr, &list);
	/* ((call . args) env) */
	Return_getcdr(list, &list); /* (env) */
	Return_getcar(list, &list); /* env */
	setresult_control(ptr, list);

	return 0;
}

int macro_whole_code(Execute ptr, CodeValue x)
{
	addr list;

	/* (call . args) */
	getresult_control(ptr, &list);
	Return_getcdr(list, &list); /* (args) */
	SetControl(ptr->control, Control_Cons, list);
	SetControl(ptr->control, Control_ConsTail, Nil);

	return 0;
}

int labels_make_code(Execute ptr, CodeValue x)
{
	addr list, pos;
	size_t index;

	list = x.pos;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetIndex(pos, &index);
		function_empty_heap(&pos, Nil);
		set_lexical_control(ptr, index, pos);
	}

	return 0;
}

int labels_lambda_code(Execute ptr, CodeValue x)
{
	addr pos, code;
	size_t index;

	List_bind(x.pos, &pos, &code, NULL);
	GetIndex(pos, &index);
	get_lexical_control(ptr, index, &pos);
	SetCodeFunction(pos, code);
	setresult_control(ptr, pos);

	return 0;
}


/*
 *  multiple-value-bind
 */
int bind1_type_code(Execute ptr, CodeValue x)
{
	addr value, index, type;
	size_t i;

	GetCons(x.pos, &index, &value);
	GetCar(value, &type);
	GetIndex(index, &i);
	getvalues_control(ptr, i, &value);
	if (value == Unbound)
		value = Nil;

	return call_typep_error_(ptr, value, type);
}

int bind1_special_code(Execute ptr, CodeValue x)
{
	addr value, index, symbol;
	size_t i;

	GetCons(x.pos, &index, &value);
	GetCar(value, &symbol);
	GetIndex(index, &i);
	getvalues_control(ptr, i, &value);
	if (value == Unbound)
		value = Nil;
	pushspecial_control(ptr, symbol, value);

	return 0;
}

int bind1_lexical_code(Execute ptr, CodeValue x)
{
	addr value, index, lexical;
	size_t i;

	GetCons(x.pos, &index, &value);
	GetCar(value, &lexical);
	GetIndex(index, &i);
	getvalues_control(ptr, i, &value);
	if (value == Unbound)
		value = Nil;
	GetIndex(lexical, &i);
	set_lexical_control(ptr, i, value);

	return 0;
}

int bind2_type_code(Execute ptr, CodeValue x)
{
	addr value;

	getvalues_root_control(ptr, &value);
	if (value == Unbound)
		value = Nil;

	return call_typep_error_(ptr, value, x.pos);
}

int bind2_special_code(Execute ptr, CodeValue x)
{
	addr value;

	getvalues_pop_control(ptr, &value);
	if (value == Unbound)
		value = Nil;
	pushspecial_control(ptr, x.pos, value);

	return 0;
}

int bind2_lexical_code(Execute ptr, CodeValue x)
{
	addr value;

	getvalues_pop_control(ptr, &value);
	if (value == Unbound)
		value = Nil;
	set_lexical_control(ptr, x.index, value);

	return 0;
}


/************************************************************
 *  code_object.c
 ************************************************************/

/*
 *  code
 */
static void alloc_code_heap(addr *ret)
{
	heap_smallsize(ret, LISPTYPE_CODE, Code_Size, sizeoft(struct code_struct));
}

static void alloc_code_system_heap(addr *ret, size_t size)
{
	addr pos;
	size_t allsize;
#ifdef LISP_DEBUG
	struct code_value *ptr;
#endif

	allsize = (size + 1) * sizeoft(struct code_value);
	heap_body4(&pos, LISPSYSTEM_CODE, allsize);
#ifdef LISP_DEBUG
	ptr = StructCallCode(pos);
	aamemory(ptr, allsize);
#endif
	*ret = pos;
}

void code_heap(addr *ret, addr codeA4)
{
	addr pos, call;
	struct code_struct *ptr;
	size_t size;

	alloc_code_heap(&pos);
	ptr = StructCode(pos);
	clearpoint(ptr);
	CheckType(codeA4, LISPTYPE_VECTOR);
	LenArrayA4(codeA4, &size);
	alloc_code_system_heap(&call, size);
	SetArrayCode(pos, Code_Array, codeA4);
	SetArrayCode(pos, Code_Call, call);
	ptr->size = size;
	update_code(pos);
	*ret = pos;
}

void getarray_code(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CODE);
	GetArrayCode(pos, Code_Array, ret);
}


/*
 *  update_code
 */
static CodeValue make_code_value(pointer id, addr pos)
{
	CodeValue ret;
	enum CodeValueType type;

	Check(p_size_code < id, "pointer error");
	GetCodeValueArray(id, &type);
	switch (type) {
		case CodeValueType_Addr:
			ret.pos = pos;
			break;

		case CodeValueType_Index:
			GetIndex(pos, &ret.index);
			break;

		case CodeValueType_Fixnum:
			GetFixnum(pos, &ret.value);
			break;

		case CodeValueType_FixnumNull:
			if (pos == Nil) {
				ret.pos = Nil;
			}
			else {
				GetFixnum(pos, &ret.value);
			}
			break;

		case CodeValueType_Character:
			GetCharacter(pos, &ret.character);
			break;

		case CodeValueType_Null:
			ret.pos = Nil;
			break;
	}

	return ret;
}

static void update_struct_code(struct code_value *ptr, addr list)
{
	addr symbol, pos;
	callbind_code bind;
	CodeValue value;
	pointer id;

	/* operator */
	GetCons(list, &symbol, &list);
	GetFunctionSymbol(symbol, &pos);
#ifdef LISP_DEBUG
	if (pos == Unbound)
		infoprint(symbol);
	Check(pos == Unbound, "unbound error");
#endif
	Check(! compiled_funcall_function_p(pos), "type error");
	id = StructFunction(pos)->index;
	GetPointer_code(id, &bind);
	value = make_code_value(id, list);

	/* result */
	ptr->call= bind;
	ptr->id = id;
	ptr->value = value;
}

void update_code(addr code)
{
	addr array, call, list;
	struct code_struct *str;
	struct code_value *ptr;
	size_t size, i;

	CheckType(code, LISPTYPE_CODE);
	GetArrayCode(code, Code_Array, &array);
	GetArrayCode(code, Code_Call, &call);
	str = StructCode(code);
	ptr = StructCallCode(call);
	size = str->size;

	for (i = 0; i < size; i++) {
		GetArgumentCall(array, i, &list);
		update_struct_code(ptr + i, list);
	}
	ptr[i].call = NULL;
	str->sys = ptr;
}


/************************************************************
 *  code_values.c
 ************************************************************/

/*
 *  code
 */
static void get_type_values(addr type, size_t index, addr *ret)
{
	addr list, pos;
	size_t now;

	CheckType(type, LISPTYPE_TYPE);
	Check(RefLispDecl(type) != LISPDECL_VALUES, "decl error");
	now = 0;

	/* var */
	GetArrayType(type, 0, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (index == now) {
			*ret = pos;
			return;
		}
		now++;
	}

	/* opt */
	GetArrayType(type, 1, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (index == now) {
			*ret = pos;
			return;
		}
		now++;
	}

	/* rest */
	GetArrayType(type, 2, ret);
}

static void get_length_values(addr type, size_t *ret)
{
	addr list;

	CheckType(type, LISPTYPE_TYPE);
	Check(RefLispDecl(type) != LISPDECL_VALUES, "decl error");
	/* var */
	GetArrayType(type, 0, &list);
	*ret = length_list_unsafe(list);
}

static int values_typep_values_(Execute ptr, addr type)
{
	addr value, check;
	size_t must, size, i;

	CheckType(type, LISPTYPE_TYPE);
	Check(RefLispDecl(type) != LISPDECL_VALUES, "decl error");

	get_length_values(type, &must);
	size = lengthvalues_control(ptr);
	size = (size < must)? must: size;
	for (i = 0; i < size; i++) {
		getvalues_control(ptr, i, &value);
		if (value == Unbound)
			value = Nil;
		get_type_values(type, i, &check);
		Return(call_typep_error_(ptr, value, check));
	}

	return 0;
}

int values_typep_error_(Execute ptr, addr type)
{
	enum LISPDECL decl;
	addr value;

	CheckType(type, LISPTYPE_TYPE);
	GetLispDecl(type, &decl);
	Check(decl == LISPDECL_SUBTYPEP, "type error, subtypep");
	Check(decl == LISPDECL_OPTIMIZED, "type error, optimized");
	if (decl == LISPDECL_VALUES)
		return values_typep_values_(ptr, type);

	getresult_control(ptr, &value);
	return call_typep_error_(ptr, value, type);
}


/************************************************************
 *  common.c
 ************************************************************/
/*
 *  ANSI COMMON LISP Function
 */

void init_common(void)
{
	/* 3. Evaluation and Compilation */
	init_common_eval();
	/* 4. Types and Classes */
	init_common_types();
	/* 5. Data and Control Flow */
	init_common_data();
	/* 6. Iteration */
	init_common_iteration();
	/* 7. Objects */
	init_common_objects();
	/* 8. Structures */
	init_common_structures();
	/* 9. Conditions */
	init_common_conditions();
	/* 10. Symbols */
	init_common_symbols();
	/* 11. Packages */
	init_common_packages();
	/* 12. Numbers */
	init_common_numbers();
	/* 13. Characters */
	init_common_characters();
	/* 14. Conses */
	init_common_conses();
	/* 15. Arrays */
	init_common_arrays();
	/* 16. Strings */
	init_common_strings();
	/* 17. Sequences */
	init_common_sequences();
	/* 18. Hash Tables */
	init_common_hashtables();
	/* 19. Filenames */
	init_common_filenames();
	/* 20. Files */
	init_common_files();
	/* 21. Streams */
	init_common_streams();
	/* 22. Printer */
	init_common_printer();
	/* 23. Reader */
	init_common_reader();
	/* 24. System Construction */
	init_common_system();
	/* 25. Environment */
	init_common_environment();
}

void build_common(void)
{
	/* 3. Evaluation and Compilation */
	build_common_eval();
	/* 4. Types and Classes */
	build_common_types();
	/* 5. Data and Control Flow */
	build_common_data();
	/* 6. Iteration */
	build_common_iteration();
	/* 7. Objects */
	build_common_objects();
	/* 8. Structures */
	build_common_structures();
	/* 9. Conditions */
	build_common_conditions();
	/* 10. Symbols */
	build_common_symbols();
	/* 11. Packages */
	build_common_packages();
	/* 12. Numbers */
	build_common_numbers();
	/* 13. Characters */
	build_common_characters();
	/* 14. Conses */
	build_common_conses();
	/* 15. Arrays */
	build_common_arrays();
	/* 16. Strings */
	build_common_strings();
	/* 17. Sequences */
	build_common_sequences();
	/* 18. Hash Tables */
	build_common_hashtables();
	/* 19. Filenames */
	build_common_filenames();
	/* 20. Files */
	build_common_files();
	/* 21. Streams */
	build_common_streams();
	/* 22. Printer */
	build_common_printer();
	/* 23. Reader */
	build_common_reader();
	/* 24. System Construction */
	build_common_system();
	/* 25. Environment */
	build_common_environment();
}


/************************************************************
 *  common_arrays.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 15. Arrays
 */

/* (defun make-array (dimensions &key element-type initial-element
 *     initial-contents adjustable fill-pointer
 *     displaced-to displaced-index-offset) ...) -> array
 *   dimensions              (or index list) ;; array-dimensions
 *   element-type            type-specifier  ;; default t
 *   initial-element         t
 *   initial-contents        t
 *   adjustable              t  ;; boolean, default nil
 *   fill-pointer            (or index null (eql t))  ;; default nil
 *   displaced-to            (or array null)  ;; default nil
 *   displaced-index-offset  index  ;; default 0
 */
static int function_make_array(Execute ptr, addr var, addr rest)
{
	Return(make_array_common_(ptr, var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_make_array_key(addr *ret, int adjustable)
{
	addr keyword, type, type1, type2, type3, key, pos;

	/* element-type  type-specifier */
	GetConst(KEYWORD_ELEMENT_TYPE, &keyword);
	GetTypeTable(&type, TypeSpec);
	cons_heap(&pos, keyword, type);
	conscar_heap(&key, pos);
	/* initial-element  t */
	GetConst(KEYWORD_INITIAL_ELEMENT, &keyword);
	GetTypeTable(&type, T);
	cons_heap(&pos, keyword, type);
	cons_heap(&key, pos, key);
	/* initial-contents  t */
	GetConst(KEYWORD_INITIAL_CONTENTS, &keyword);
	GetTypeTable(&type, T);
	cons_heap(&pos, keyword, type);
	cons_heap(&key, pos, key);
	/* adjustable  t */
	if (adjustable) {
		GetConst(KEYWORD_ADJUSTABLE, &keyword);
		GetTypeTable(&type, T);
		cons_heap(&pos, keyword, type);
		cons_heap(&key, pos, key);
	}
	/* fill-pointer  (or index null (eql t)) */
	GetConst(KEYWORD_FILL_POINTER, &keyword);
	GetTypeTable(&type1, Index);
	GetTypeTable(&type2, Null);
	GetTypeTable(&type3, EqlT);
	type3or_heap(type1, type2, type3, &type);
	cons_heap(&pos, keyword, type);
	cons_heap(&key, pos, key);
	/* displaced-to  (or array null) */
	GetConst(KEYWORD_DISPLACED_TO, &keyword);
	GetTypeTable(&type1, Array);
	GetTypeTable(&type2, Null);
	type2or_heap(type1, type2, &type);
	cons_heap(&pos, keyword, type);
	cons_heap(&key, pos, key);
	/* displaced-index-offset  index */
	GetConst(KEYWORD_DISPLACED_INDEX_OFFSET, &keyword);
	GetTypeTable(&type, Index);
	cons_heap(&pos, keyword, type);
	cons_heap(&key, pos, key);
	/* result */
	nreverse(ret, key);
}

static void type_make_array(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Index);
	GetTypeTable(&values, List);
	type2or_heap(args, values, &args);
	type_make_array_key(&values, 1);
	typeargs_var1key(&args, args, values);
	GetTypeValues(&values, Array);
	type_compiled_heap(args, values, ret);
}

static void defun_make_array(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_ARRAY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_make_array);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_array(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun adjust-array (array dimensions &key element-type initial-element
 *     initial-contents fill-pointer
 *     displaced-to displaced-index-offset) ...) -> array
 *   dimensions              (or index list) ;; array-dimensions
 *   element-type            type-specifier  ;; default t
 *   initial-element         t
 *   initial-contents        t
 *   fill-pointer            (or index null (eql t))  ;; default nil
 *   displaced-to            (or array null)  ;; default nil
 *   displaced-index-offset  index  ;; default 0
 */
static int function_adjust_array(Execute ptr, addr pos, addr dim, addr rest)
{
	Return(adjust_array_common_(ptr, pos, dim, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_adjust_array(addr *ret)
{
	addr args, values, key;

	GetTypeTable(&args, Index);
	GetTypeTable(&values, List);
	type2or_heap(args, values, &values);
	type_make_array_key(&key, 0);
	GetTypeTable(&args, Array);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, Array);
	type_compiled_heap(args, values, ret);
}

static void defun_adjust_array(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ADJUST_ARRAY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_adjust_array);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_adjust_array(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun adjustable-array-p (array) ...) -> boolean */
static int function_adjustable_array_p(Execute ptr, addr array)
{
	int check;
	Return(adjustable_array_p_common_(array, &check));
	setbool_control(ptr, check);
	return 0;
}

static void defun_adjustable_array_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ADJUSTABLE_ARRAY_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_adjustable_array_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ArrayBoolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun aref (array &rest args) ...) -> t */
static int function_aref(Execute ptr, addr var, addr rest)
{
	Return(aref_common_(var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_aref(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Array);
	GetTypeTable(&values, Index);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_aref(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_AREF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_aref);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_aref(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf aref) (value array &rest args) ...) -> value */
static int function_setf_aref(Execute ptr, addr value, addr var, addr rest)
{
	Return(setf_aref_common_(value, var, rest));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_aref(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&type, T);
	GetTypeTable(&args, Array);
	GetTypeTable(&values, Index);
	typeargs_var2rest(&args, type, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_aref(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_AREF, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_setf_aref);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_aref(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun array-dimension (array axis) -> dimension
 *   axis       index
 *   dimension  index
 */
static int function_array_dimension(Execute ptr, addr var, addr axis)
{
	Return(array_dimension_common_(var, axis, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_array_dimension(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Array);
	GetTypeTable(&values, Index);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Index);
	type_compiled_heap(args, values, ret);
}

static void defun_array_dimension(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_DIMENSION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_array_dimension);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_array_dimension(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-dimensions (array) ...) -> (integer 0 *) */
static int function_array_dimensions(Execute ptr, addr var)
{
	Return(array_dimensions_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_array_dimensions(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_DIMENSIONS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_array_dimensions);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ArrayIndex);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-element-type (array) ...) -> typespec */
static int function_array_element_type(Execute ptr, addr var)
{
	Return(array_element_type_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_array_element_type(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Array);
	typeargs_var1(&args, args);
	GetTypeTable(&values, TypeSpec);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_array_element_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_ELEMENT_TYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_array_element_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_array_element_type(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-has-fill-pointer-p (array) ...) -> boolean */
static int function_array_has_fill_pointer_p(Execute ptr, addr var)
{
	int check;
	Return(array_has_fill_pointer_p_common_(var, &check));
	setbool_control(ptr, check);
	return 0;
}

static void defun_array_has_fill_pointer_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_HAS_FILL_POINTER_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_array_has_fill_pointer_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ArrayBoolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-displacement (array) ...) -> (or nil array), index */
static int function_array_displacement(Execute ptr, addr pos)
{
	addr value, offset;
	Return(array_displacement_common_(pos, &value, &offset));
	setvalues_control(ptr, value, offset, NULL);
	return 0;
}

static void type_array_displacement(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Array);
	typeargs_var1(&args, args);
	GetTypeTable(&values, Array);
	GetTypeTable(&type, Null);
	type2or_heap(values, type, &values);
	GetTypeTable(&type, Index);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_array_displacement(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_DISPLACEMENT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_array_displacement);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_array_displacement(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-in-bounds-p (array &rest args) ...) -> boolean */
static int function_array_in_bounds_p(Execute ptr, addr array, addr rest)
{
	int check;
	Return(array_in_bounds_p_common_(array, rest, &check));
	setbool_control(ptr, check);
	return 0;
}

static void type_array_in_bounds_p(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Array);
	GetTypeTable(&values, Integer);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_array_in_bounds_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_IN_BOUNDS_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_array_in_bounds_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_array_in_bounds_p(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-rank (array) ...) -> (intege 0 *) */
static int function_array_rank(Execute ptr, addr pos)
{
	Return(array_rank_common_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_array_rank(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_RANK, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_array_rank);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ArrayIndex);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-row-major-index (array &rest args) ...) -> index */
static int function_array_row_major_index(Execute ptr, addr array, addr rest)
{
	Return(array_row_major_index_common_(array, rest, &array));
	setresult_control(ptr, array);
	return 0;
}

static void type_array_row_major_index(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Array);
	GetTypeTable(&values, Index);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, Index);
	type_compiled_heap(args, values, ret);
}

static void defun_array_row_major_index(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_ROW_MAJOR_INDEX, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_array_row_major_index);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_array_row_major_index(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-total-size (array) ...) -> (integer 0 *) */
static int function_array_total_size(Execute ptr, addr array)
{
	Return(array_total_size_common_(array, &array));
	setresult_control(ptr, array);
	return 0;
}

static void defun_array_total_size(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_TOTAL_SIZE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_array_total_size);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ArrayIndex);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun arrayp (object) ...) -> boolean */
static int function_arrayp(Execute ptr, addr var)
{
	setbool_control(ptr, arrayp_common(var));
	return 0;
}

static void defun_arrayp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAYP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_arrayp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun fill-pointer (vector) ...) -> index */
static int function_fill_pointer(Execute ptr, addr array)
{
	Return(fill_pointer_common_(ptr, array, &array));
	setresult_control(ptr, array);
	return 0;
}

static void type_fill_pointer(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Vector);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Index);
	type_compiled_heap(args, values, ret);
}

static void defun_fill_pointer(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILL_POINTER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_fill_pointer);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_fill_pointer(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf fill-pointer) (value array) ...) -> value
 *   value  index
 */
static int function_setf_fill_pointer(Execute ptr, addr value, addr array)
{
	Return(setf_fill_pointer_common_(ptr, value, array));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_fill_pointer(addr *ret)
{
	addr args, values;

	GetTypeTable(&values, Index);
	GetTypeTable(&args, Vector);
	typeargs_var2(&args, values, args);
	GetTypeValues(&values, Index);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_fill_pointer(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILL_POINTER, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_setf_fill_pointer);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_fill_pointer(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun row-major-aref (array index) ...) -> t */
static int function_row_major_aref(Execute ptr, addr array, addr index)
{
	Return(row_major_aref_common_(array, index, &array));
	setresult_control(ptr, array);
	return 0;
}

static void type_row_major_aref(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Array);
	GetTypeTable(&values, Index);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_row_major_aref(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ROW_MAJOR_AREF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_row_major_aref);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_row_major_aref(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf row-major-aref) (value array index) ...) -> value */
static int function_setf_row_major_aref(Execute ptr,
		addr value, addr array, addr index)
{
	Return(setf_row_major_aref_common_(value, array, index));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_row_major_aref(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&type, T);
	GetTypeTable(&args, Array);
	GetTypeTable(&values, Index);
	typeargs_var3(&args, type, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_row_major_aref(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ROW_MAJOR_AREF, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_setf_row_major_aref);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_row_major_aref(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun upgraded-array-element-type (typespec &optional environment) ...)
 *     -> typespec
 */
static int function_upgraded_array_element_type(Execute ptr, addr pos, addr env)
{
	Return(upgraded_array_common_(ptr, env, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_upgraded_array_element_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UPGRADED_ARRAY_ELEMENT_TYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_upgraded_array_element_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, UpgradedType);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defconstnat array-dimension-limit FIXNUM_MAX) */
static void defconstant_array_dimension_limit(void)
{
	addr symbol, value;

	GetConst(COMMON_ARRAY_DIMENSION_LIMIT, &symbol);
	GetConst(FIXNUM_MAX, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstnat array-rank-limit FIXNUM_MAX) */
static void defconstant_array_rank_limit(void)
{
	addr symbol, value;

	GetConst(COMMON_ARRAY_RANK_LIMIT, &symbol);
	GetConst(FIXNUM_MAX, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstnat array-total-size-limit FIXNUM_MAX) */
static void defconstant_array_total_size_limit(void)
{
	addr symbol, value;

	GetConst(COMMON_ARRAY_TOTAL_SIZE_LIMIT, &symbol);
	GetConst(FIXNUM_MAX, &value);
	defconstant_symbol(symbol, value);
}


/* (defun simple-vector-p (object) ...) -> boolean */
static int function_simple_vector_p(Execute ptr, addr var)
{
	setbool_control(ptr, simple_vector_p_common(var));
	return 0;
}

static void defun_simple_vector_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIMPLE_VECTOR_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_simple_vector_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun svref (simple-vector index) ...) -> t */
static int function_svref(Execute ptr, addr pos, addr index)
{
	Return(svref_common_(pos, index, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_svref(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, SimpleVector);
	GetTypeTable(&values, Index);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_svref(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SVREF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_svref);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_svref(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf svref) (value simple-vector index) ...) -> t */
static int function_setf_svref(Execute ptr, addr value, addr pos, addr index)
{
	Return(setf_svref_common_(value, pos, index));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_svref(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&type, T);
	GetTypeTable(&args, SimpleVector);
	GetTypeTable(&values, Index);
	typeargs_var3(&args, type, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_svref(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SVREF, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_setf_svref);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_svref(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun vector (&rest args) ...) -> simple-vector */
static int function_vector(Execute ptr, addr rest)
{
	Return(make_vector_from_list_(&rest, rest));
	setresult_control(ptr, rest);
	return 0;
}

static void type_common_vector(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetTypeValues(&values, SimpleVector);
	type_compiled_heap(args, values, ret);
}

static void defun_vector(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VECTOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_vector);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_common_vector(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun vector-pop (vector) ...) -> t */
static int function_vector_pop(Execute ptr, addr pos)
{
	Return(vector_pop_common_(ptr, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_vector_pop(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Vector);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_vector_pop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VECTOR_POP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_vector_pop);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_vector_pop(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun vector-push (value vector) ...) -> index-null */
static int function_vector_push(Execute ptr, addr value, addr pos)
{
	Return(vector_push_common_(ptr, value, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_vector_push(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Vector);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, IndexNull);
	type_compiled_heap(args, values, ret);
}

static void defun_vector_push(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VECTOR_PUSH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_vector_push);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_vector_push(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun vector-push-extend (value vector &optional extension) ...)
 *    -> index
 *   extension  (integer 1 *)
 */
static int function_vector_push_extend(Execute ptr,
		addr value, addr pos, addr extension)
{
	Return(vector_push_extend_common_(ptr, value, pos, extension, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_vector_push_extend(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Vector);
	type2integer_ab_heap(Nil, 1, &type);
	typeargs_var2opt1(&args, args, values, type);
	GetTypeValues(&values, Index);
	type_compiled_heap(args, values, ret);
}

static void defun_vector_push_extend(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VECTOR_PUSH_EXTEND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_vector_push_extend);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_vector_push_extend(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun vectorp (object) ...) -> boolean */
static int function_vectorp(Execute ptr, addr var)
{
	setbool_control(ptr, vectorp_common(var));
	return 0;
}

static void defun_vectorp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VECTORP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_vectorp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit (bit-array &rest args) ...) -> bit */
static int function_bit(Execute ptr, addr pos, addr rest)
{
	Return(bit_common_(pos, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_bit_common(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, BitArray);
	GetTypeTable(&values, Index);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, Bit);
	type_compiled_heap(args, values, ret);
}

static void defun_bit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_bit);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_bit_common(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sbit (simple-bit-array &rest args) ...) -> bit */
static void type_sbit_common(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, SimpleBitArray);
	GetTypeTable(&values, Index);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, Bit);
	type_compiled_heap(args, values, ret);
}

static void defun_sbit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SBIT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_bit);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_sbit_common(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf bit) (bit bit-array &rest args) ...) -> bit */
static int function_setf_bit(Execute ptr, addr value, addr pos, addr rest)
{
	Return(setf_bit_common_(value, pos, rest));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_bit(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&type, Bit);
	GetTypeTable(&args, BitArray);
	GetTypeTable(&values, Index);
	typeargs_var2rest(&args, type, args, values);
	GetTypeValues(&values, Bit);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_bit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_setf_bit);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_bit(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun (setf sbit) (bit simple-bit-array &rest args) ...) -> bit */
static void type_setf_sbit(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&type, Bit);
	GetTypeTable(&args, SimpleBitArray);
	GetTypeTable(&values, Index);
	typeargs_var2rest(&args, type, args, values);
	GetTypeValues(&values, Bit);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_sbit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SBIT, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_setf_bit);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_sbit(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun bit-vector-p (t) ...) -> boolean */
static int function_bit_vector_p(Execute ptr, addr var)
{
	setbool_control(ptr, bit_vector_p_common(var));
	return 0;
}

static void defun_bit_vector_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_VECTOR_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_bit_vector_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun simple-bit-vector-p (t) ...) -> boolean */
static int function_simple_bit_vector_p(Execute ptr, addr var)
{
	setbool_control(ptr, simple_bit_vector_p_common(var));
	return 0;
}

static void defun_simple_bit_vector_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIMPLE_BIT_VECTOR_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_simple_bit_vector_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-and (bit-array1 bit-array2 &optional opt-args) ...)
 *     -> bit-array
 *   opt-args  (or boolean bit-array)
 */
static int function_bit_and(Execute ptr, addr x, addr y, addr opt)
{
	Return(bit_and_common_(x, y, opt, &x));
	setresult_control(ptr, x);
	return 0;
}

static void defun_bit_and(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_AND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_and);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-andc1 (bit-array1 bit-array2 &optional opt-args) ...) */
static int function_bit_andc1(Execute ptr, addr x, addr y, addr opt)
{
	Return(bit_andc1_common_(x, y, opt, &x));
	setresult_control(ptr, x);
	return 0;
}

static void defun_bit_andc1(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_ANDC1, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_andc1);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-andc2 (bit-array1 bit-array2 &optional opt-args) ...) */
static int function_bit_andc2(Execute ptr, addr x, addr y, addr opt)
{
	Return(bit_andc2_common_(x, y, opt, &x));
	setresult_control(ptr, x);
	return 0;
}

static void defun_bit_andc2(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_ANDC2, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_andc2);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-eqv (bit-array1 bit-array2 &optional opt-args) ...) */
static int function_bit_eqv(Execute ptr, addr x, addr y, addr opt)
{
	Return(bit_eqv_common_(x, y, opt, &x));
	setresult_control(ptr, x);
	return 0;
}

static void defun_bit_eqv(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_EQV, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_eqv);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-ior (bit-array1 bit-array2 &optional opt-args) ...) */
static int function_bit_ior(Execute ptr, addr x, addr y, addr opt)
{
	Return(bit_ior_common_(x, y, opt, &x));
	setresult_control(ptr, x);
	return 0;
}

static void defun_bit_ior(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_IOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_ior);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-nand (bit-array1 bit-array2 &optional opt-args) ...) */
static int function_bit_nand(Execute ptr, addr x, addr y, addr opt)
{
	Return(bit_nand_common_(x, y, opt, &x));
	setresult_control(ptr, x);
	return 0;
}

static void defun_bit_nand(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_NAND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_nand);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-nor (bit-array1 bit-array2 &optional opt-args) ...) */
static int function_bit_nor(Execute ptr, addr x, addr y, addr opt)
{
	Return(bit_nor_common_(x, y, opt, &x));
	setresult_control(ptr, x);
	return 0;
}

static void defun_bit_nor(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_NOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_nor);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-orc1 (bit-array1 bit-array2 &optional opt-args) ...) */
static int function_bit_orc1(Execute ptr, addr x, addr y, addr opt)
{
	Return(bit_orc1_common_(x, y, opt, &x));
	setresult_control(ptr, x);
	return 0;
}

static void defun_bit_orc1(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_ORC1, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_orc1);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-orc2 (bit-array1 bit-array2 &optional opt-args) ...) */
static int function_bit_orc2(Execute ptr, addr x, addr y, addr opt)
{
	Return(bit_orc2_common_(x, y, opt, &x));
	setresult_control(ptr, x);
	return 0;
}

static void defun_bit_orc2(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_ORC2, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_orc2);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-xor (bit-array1 bit-array2 &optional opt-args) ...) */
static int function_bit_xor(Execute ptr, addr x, addr y, addr opt)
{
	Return(bit_xor_common_(x, y, opt, &x));
	setresult_control(ptr, x);
	return 0;
}

static void defun_bit_xor(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_XOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_xor);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-not (bit-array1 bit-array2 &optional opt-args) ...) */
static int function_bit_not(Execute ptr, addr x, addr opt)
{
	Return(bit_not_common_(x, opt, &x));
	setresult_control(ptr, x);
	return 0;
}

static void type_bit_not(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, BitArray);
	GetTypeTable(&values, Boolean);
	type2or_heap(args, values, &values);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, BitArray);
	type_compiled_heap(args, values, ret);
}

static void defun_bit_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_bit_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_bit_not(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_arrays(void)
{
	SetPointerCall(defun, var1dynamic, make_array);
	SetPointerCall(defun, var2dynamic, adjust_array);
	SetPointerCall(defun, var1, adjustable_array_p);
	SetPointerCall(defun, var1dynamic, aref);
	SetPointerCall(defun, var2dynamic, setf_aref);
	SetPointerCall(defun, var2, array_dimension);
	SetPointerCall(defun, var1, array_dimensions);
	SetPointerCall(defun, var1, array_element_type);
	SetPointerCall(defun, var1, array_has_fill_pointer_p);
	SetPointerCall(defun, var1, array_displacement);
	SetPointerCall(defun, var1dynamic, array_in_bounds_p);
	SetPointerCall(defun, var1, array_rank);
	SetPointerCall(defun, var1dynamic, array_row_major_index);
	SetPointerCall(defun, var1, array_total_size);
	SetPointerCall(defun, var1, arrayp);
	SetPointerCall(defun, var1, fill_pointer);
	SetPointerCall(defun, var2, setf_fill_pointer);
	SetPointerCall(defun, var2, row_major_aref);
	SetPointerCall(defun, var3, setf_row_major_aref);
	SetPointerCall(defun, var1opt1, upgraded_array_element_type);
	SetPointerCall(defun, var1, simple_vector_p);
	SetPointerCall(defun, var2, svref);
	SetPointerCall(defun, var3, setf_svref);
	SetPointerCall(defun, dynamic, vector);
	SetPointerCall(defun, var1, vector_pop);
	SetPointerCall(defun, var2, vector_push);
	SetPointerCall(defun, var2opt1, vector_push_extend);
	SetPointerCall(defun, var1, vectorp);
	SetPointerCall(defun, var1dynamic, bit);
	SetPointerCall(defun, var1dynamic, bit);
	SetPointerCall(defun, var2dynamic, setf_bit);
	SetPointerCall(defun, var2dynamic, setf_bit);
	SetPointerCall(defun, var1, bit_vector_p);
	SetPointerCall(defun, var1, simple_bit_vector_p);
	SetPointerCall(defun, var2opt1, bit_and);
	SetPointerCall(defun, var2opt1, bit_andc1);
	SetPointerCall(defun, var2opt1, bit_andc2);
	SetPointerCall(defun, var2opt1, bit_eqv);
	SetPointerCall(defun, var2opt1, bit_ior);
	SetPointerCall(defun, var2opt1, bit_nand);
	SetPointerCall(defun, var2opt1, bit_nor);
	SetPointerCall(defun, var2opt1, bit_orc1);
	SetPointerCall(defun, var2opt1, bit_orc2);
	SetPointerCall(defun, var2opt1, bit_xor);
	SetPointerCall(defun, var1opt1, bit_not);
}

void build_common_arrays(void)
{
	defun_make_array();
	defun_adjust_array();
	defun_adjustable_array_p();
	defun_aref();
	defun_setf_aref();
	defun_array_dimension();
	defun_array_dimensions();
	defun_array_element_type();
	defun_array_has_fill_pointer_p();
	defun_array_displacement();
	defun_array_in_bounds_p();
	defun_array_rank();
	defun_array_row_major_index();
	defun_array_total_size();
	defun_arrayp();
	defun_fill_pointer();
	defun_setf_fill_pointer();
	defun_row_major_aref();
	defun_setf_row_major_aref();
	defun_upgraded_array_element_type();
	defconstant_array_dimension_limit();
	defconstant_array_rank_limit();
	defconstant_array_total_size_limit();
	defun_simple_vector_p();
	defun_svref();
	defun_setf_svref();
	defun_vector();
	defun_vector_pop();
	defun_vector_push();
	defun_vector_push_extend();
	defun_vectorp();
	defun_bit();
	defun_sbit();
	defun_setf_bit();
	defun_setf_sbit();
	defun_bit_vector_p();
	defun_simple_bit_vector_p();
	defun_bit_and();
	defun_bit_andc1();
	defun_bit_andc2();
	defun_bit_eqv();
	defun_bit_ior();
	defun_bit_nand();
	defun_bit_nor();
	defun_bit_orc1();
	defun_bit_orc2();
	defun_bit_xor();
	defun_bit_not();
}


/************************************************************
 *  common_characters.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 13. Characters
 */

/* (defconstnat char-code-limit UnicodeCount) */
static void defconstant_char_code_limit(void)
{
	addr symbol, value;

	GetConst(COMMON_CHAR_CODE_LIMIT, &symbol);
	fixnum_heap(&value, UnicodeCount);
	defconstant_symbol(symbol, value);
}


/* (defun char= (character &rest character) ...) -> boolean */
static void defun_char_check(constindex index, pointer p)
{
	addr symbol, pos, type;

	/* function */
	GetConstant(index, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, CharEql);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static int function_char_eql(Execute ptr, addr list)
{
	Return(char_eql_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_eql(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_EQL, p_defun_char_eql);
}


/* (defun char/= (character &rest character) ...) -> boolean */
static int function_char_not_eql(Execute ptr, addr list)
{
	Return(char_not_eql_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_not_eql(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_NOT_EQL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_char_not_eql);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, CharEql);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char< (character &rest character) ...) -> boolean */
static int function_char_less(Execute ptr, addr list)
{
	Return(char_less_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_less(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_LESS, p_defun_char_less);
}


/* (defun char> (character &rest character) ...) -> boolean */
static int function_char_greater(Execute ptr, addr list)
{
	Return(char_greater_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_greater(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_GREATER, p_defun_char_greater);
}


/* (defun char<= (character &rest character) ...) -> boolean */
static int function_char_less_equal(Execute ptr, addr list)
{
	Return(char_less_equal_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_less_equal(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_LESS_EQUAL, p_defun_char_less_equal);
}


/* (defun char>= (character &rest character) ...) -> boolean */
static int function_char_greater_equal(Execute ptr, addr list)
{
	Return(char_greater_equal_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_greater_equal(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_GREATER_EQUAL, p_defun_char_greater_equal);
}


/* (defun char-equal (character &rest character) ...) -> boolean */
static int function_char_equal(Execute ptr, addr list)
{
	Return(char_equal_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_equal(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_EQUAL, p_defun_char_equal);
}


/* (defun char-not-equal (character &rest character) ...) -> boolean */
static int function_char_not_equal(Execute ptr, addr list)
{
	Return(char_not_equal_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_not_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_NOT_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_char_not_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, CharEql);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char-lessp (character &rest character) ...) -> boolean */
static int function_char_lessp(Execute ptr, addr list)
{
	Return(char_lessp_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_lessp(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_LESSP, p_defun_char_lessp);
}


/* (defun char-greaterp (character &rest character) ...) -> boolean */
static int function_char_greaterp(Execute ptr, addr list)
{
	Return(char_greaterp_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_greaterp(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_GREATERP, p_defun_char_greaterp);
}


/* (defun char-not-lessp (character &rest character) ...) -> boolean */
static int function_char_not_lessp(Execute ptr, addr list)
{
	Return(char_not_lessp_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_not_lessp(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_NOT_LESSP, p_defun_char_not_lessp);
}


/* (defun char-not-greaterp (character &rest character) ...) -> boolean */
static int function_char_not_greaterp(Execute ptr, addr list)
{
	Return(char_not_greaterp_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_not_greaterp(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_NOT_GREATERP, p_defun_char_not_greaterp);
}


/* (defun character (character) ...) -> character */
static int function_character(Execute ptr, addr var)
{
	Return(character_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_character_stringone(addr *ret)
{
	addr pos;
	fixnum_heap(&pos, 1);
	type1_heap(LISPDECL_SIMPLE_BASE_STRING, pos, ret);
}

static void type_character_arguments(addr *ret)
{
	addr type1, type2, type3, pos;

	/* (or character
	 *     (simple-base-string 1)
	 *     symbol)  ;; (= (length (symbol-name x)) 1)
	 */
	GetTypeTable(&type1, Character);
	type_character_stringone(&type2);
	GetTypeTable(&type3, Symbol);
	type3or_heap(type1, type2, type3, &pos);
	typeargs_var1(ret, pos);
}

static void type_character(addr *ret)
{
	addr args, values;

	type_character_arguments(&args);
	GetTypeValues(&values, Character);
	type_compiled_heap(args, values, ret);
}

static void defun_character(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHARACTER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_character);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun characterp (object) ...) -> boolean */
static int function_characterp(Execute ptr, addr var)
{
	setbool_control(ptr, characterp(var));
	return 0;
}

static void defun_characterp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHARACTERP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_characterp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun alpha-char-p (character) ...) -> boolean */
static int function_alpha_char_p(Execute ptr, addr var)
{
	alpha_char_p_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_character_boolean(addr *ret)
{
	addr args, values;

	/* (function (character) boolean) */
	GetTypeTable(&args, Character);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_alpha_char_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ALPHA_CHAR_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_alpha_char_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun alphanumericp (character) ...) -> boolean */
static int function_alphanumericp(Execute ptr, addr var)
{
	alphanumericp_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_alphanumericp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ALPHANUMERICP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_alphanumericp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun digit-char (weight &optional radix) ...) -> result
 *   weight  (integer 0 *)
 *   radix   (integer 2 36), default 10
 *   result  (or null character)
 */
static int function_digit_char(Execute ptr, addr var, addr opt)
{
	digit_char_common(var, opt, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_digit_char(addr *ret)
{
	addr weight, radix, args, values;

	/* ((integer 0 *) &optional (integer 2 36)) */
	GetTypeTable(&weight, Intplus);
	GetTypeTable(&radix, RadixInteger);
	typeargs_var1opt1(&args, weight, radix);
	/* (or character null) */
	GetTypeValues(&values, CharacterNull);
	/* function */
	type_compiled_heap(args, values, ret);
}

static void defun_digit_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DIGIT_CHAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_digit_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_digit_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun digit-char-p (character &optional radix) ...) -> weight
 *   character  character
 *   radix      (integer 2 36), default 10
 *   weight     (or (integer 0 *) null)
 */
static int function_digit_char_p(Execute ptr, addr var, addr opt)
{
	digit_char_p_common(var, opt, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_digit_char_p(addr *ret)
{
	addr character, radix, args, values;

	/* (character &optional (integer 2 36)) */
	GetTypeTable(&character, Character);
	GetTypeTable(&radix, RadixInteger);
	typeargs_var1opt1(&args, character, radix);
	/* (or (integer 0 *) null) */
	GetTypeValues(&values, IntplusNull);
	/* function */
	type_compiled_heap(args, values, ret);
}

static void defun_digit_char_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DIGIT_CHAR_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_digit_char_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_digit_char_p(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun graphic-char-p (character) ...) -> boolean */
static int function_graphic_char_p(Execute ptr, addr var)
{
	graphic_char_p_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_graphic_char_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GRAPHIC_CHAR_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_graphic_char_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun standard-char-p (character) ...) -> boolean */
static int function_standard_char_p(Execute ptr, addr var)
{
	standard_char_p_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_standard_char_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STANDARD_CHAR_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_standard_char_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char-upcase (character) ...) -> character */
static int function_char_upcase(Execute ptr, addr var)
{
	char_upcase_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_character_character(addr *ret)
{
	addr args, values;

	/* (function (character) character) */
	GetTypeTable(&args, Character);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Character);
	type_compiled_heap(args, values, ret);
}

static void defun_char_upcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_UPCASE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_char_upcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char-downcase (character) ...) -> character */
static int function_char_downcase(Execute ptr, addr var)
{
	char_downcase_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_char_downcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_DOWNCASE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_char_downcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun upper-case-p (character) ...) -> boolean */
static int function_upper_case_p(Execute ptr, addr var)
{
	upper_case_p_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_upper_case_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UPPER_CASE_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_upper_case_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun lower-case-p (character) ...) -> boolean */
static int function_lower_case_p(Execute ptr, addr var)
{
	lower_case_p_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_lower_case_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOWER_CASE_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_lower_case_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun both-case-p (character) ...) -> boolean */
static int function_both_case_p(Execute ptr, addr var)
{
	both_case_p_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_both_case_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BOTH_CASE_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_both_case_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char-code (character) ...) -> code
 *   code  (integer 0 UnicodeCount)
 */
static int function_char_code(Execute ptr, addr var)
{
	char_code_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_char_code(addr *ret)
{
	addr args, values;

	/* (function (character) (values (integer 0 UnicodeCount))) */
	GetTypeTable(&args, Character);
	typeargs_var1(&args, args);
	type4integer_heap(Nil, 0, Nil, (fixnum)UnicodeCount, &values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_char_code(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_CODE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_char_code);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_char_code(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static void defun_char_int(void)
{
	/* char-code == char-int */
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_INT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_char_code);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_char_code(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun code-char (code) ...) -> char-p
 *   code    (integer 0 (UnicodeCount))
 *   char-p  (or character boolean)
 */
static int function_code_char(Execute ptr, addr var)
{
	code_char_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_code_char(addr *ret)
{
	addr args, values, type;

	/* (function
	 *   ((integer 0 UnicodeCount))
	 *   (values (or character boolean))) */
	type4integer_heap(Nil, 0, T, (fixnum)UnicodeCount, &args);
	typeargs_var1(&args, args);
	GetTypeTable(&values, Character);
	GetTypeTable(&type, Boolean);
	type2or_heap(values, type, &values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_code_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CODE_CHAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_code_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_code_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char-name (character) ...) -> name
 *   name  (or string null)
 */
static int function_char_name(Execute ptr, addr var)
{
	Return(char_name_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_char_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Character);
	typeargs_var1(&args, args);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, ret);
}

static void defun_char_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_NAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_char_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_char_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun name-char (name) ...) -> char-p
 *   name    (or string symbol character)  ;; string-designator
 *   char-p  (or character null)
 */
static int function_name_char(Execute ptr, addr var)
{
	Return(name_char_common_(ptr->local, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_name_char(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringDesignator);
	typeargs_var1(&args, args);
	GetTypeValues(&values, CharacterNull);
	type_compiled_heap(args, values, ret);
}

static void defun_name_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NAME_CHAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_name_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_name_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_characters(void)
{
	SetPointerCall(defun, dynamic, char_eql);
	SetPointerCall(defun, dynamic, char_less);
	SetPointerCall(defun, dynamic, char_greater);
	SetPointerCall(defun, dynamic, char_less_equal);
	SetPointerCall(defun, dynamic, char_greater_equal);
	SetPointerCall(defun, dynamic, char_equal);
	SetPointerCall(defun, dynamic, char_lessp);
	SetPointerCall(defun, dynamic, char_greaterp);
	SetPointerCall(defun, dynamic, char_not_lessp);
	SetPointerCall(defun, dynamic, char_not_greaterp);
	SetPointerCall(defun, dynamic, char_not_eql);
	SetPointerCall(defun, dynamic, char_not_equal);
	SetPointerCall(defun, var1, character);
	SetPointerCall(defun, var1, characterp);
	SetPointerCall(defun, var1, alpha_char_p);
	SetPointerCall(defun, var1, alphanumericp);
	SetPointerCall(defun, var1opt1, digit_char);
	SetPointerCall(defun, var1opt1, digit_char_p);
	SetPointerCall(defun, var1, graphic_char_p);
	SetPointerCall(defun, var1, standard_char_p);
	SetPointerCall(defun, var1, char_upcase);
	SetPointerCall(defun, var1, char_downcase);
	SetPointerCall(defun, var1, upper_case_p);
	SetPointerCall(defun, var1, lower_case_p);
	SetPointerCall(defun, var1, both_case_p);
	SetPointerCall(defun, var1, char_code);
	SetPointerCall(defun, var1, char_code);
	SetPointerCall(defun, var1, code_char);
	SetPointerCall(defun, var1, char_name);
	SetPointerCall(defun, var1, name_char);
}

void build_common_characters(void)
{
	defconstant_char_code_limit();
	defun_char_eql();
	defun_char_not_eql();
	defun_char_less();
	defun_char_greater();
	defun_char_less_equal();
	defun_char_greater_equal();
	defun_char_equal();
	defun_char_not_equal();
	defun_char_lessp();
	defun_char_greaterp();
	defun_char_not_lessp();
	defun_char_not_greaterp();
	defun_character();
	defun_characterp();
	defun_alpha_char_p();
	defun_alphanumericp();
	defun_digit_char();
	defun_digit_char_p();
	defun_graphic_char_p();
	defun_standard_char_p();
	defun_char_upcase();
	defun_char_downcase();
	defun_upper_case_p();
	defun_lower_case_p();
	defun_both_case_p();
	defun_char_code();
	defun_char_int();
	defun_code_char();
	defun_char_name();
	defun_name_char();
}


/************************************************************
 *  common_conditions.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 9. Conditions
 */

/* (defun cell-error-name (condition) ...) -> t */
static int function_cell_error_name(Execute ptr, addr var)
{
	Return(cell_error_name_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_cell_error_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, CellError);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_cell_error_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CELL_ERROR_NAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_cell_error_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_cell_error_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro assert (test &optional (place*) format &rest args) ...) -> nil */
static int function_assert(Execute ptr, addr form, addr env)
{
	Return(assert_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_assert(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_ASSERT, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_assert);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun error (datum &rest args) ...) -> nil
 *   datum  (or string symbol condition)
 *   args   (&rest t)
 *   nil    nil  ;; not null
 */
static int function_error(Execute ptr, addr datum, addr rest)
{
	Return(error_common_(ptr, datum, rest));
	setvalues_nil_control(ptr);
	return 0;
}

static void type_error_function(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, Error);
	GetTypeValues(&values, Nil);
	type_compiled_heap(args, values, ret);
}

static void defun_error(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ERROR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_error);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_error_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun cerror (continue-format datum &args) ...) -> null */
static int function_cerror(Execute ptr, addr restart, addr datum, addr rest)
{
	Return(cerror_common_(ptr, restart, datum, rest));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_cerror(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, String);
	GetTypeTable(&values, ConditionDesignator);
	GetTypeTable(&type, T);
	typeargs_var2rest(&args, args, values, type);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_cerror(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CERROR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2rest(pos, p_defun_cerror);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_cerror(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro check-type (place type &optional string) ...) -> null */
static int function_check_type(Execute ptr, addr form, addr env)
{
	Return(check_type_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_check_type(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_CHECK_TYPE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_check_type);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun invalid-method-error (method format &rest args) ...) -> null */
static int function_invalid_method_error(Execute ptr,
		addr method, addr format, addr args)
{
	Return(invalid_method_error_common_(ptr, method, format, args));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_invalid_method_error(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Method);
	GetTypeTable(&values, String);
	GetTypeTable(&type, T);
	typeargs_var2rest(&args, args, values, type);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_invalid_method_error(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INVALID_METHOD_ERROR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2rest(pos, p_defun_invalid_method_error);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_invalid_method_error(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun method-combination-error (format &rest args) ...) -> null */
static int function_method_combination_error(Execute ptr, addr format, addr args)
{
	Return(method_combination_error_common_(ptr, format, args));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_method_combination_error(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, String);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_method_combination_error(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_METHOD_COMBINATION_ERROR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_method_combination_error);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_method_combination_error(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun signal (datum &rest args) ...) -> nil */
static int function_signal(Execute ptr, addr datum, addr rest)
{
	Return(signal_common_(ptr, datum, rest));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_signal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIGNAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_signal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Signal);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun defun-simple-condition-format-control (condition) ...) -> t */
static int function_simple_condition_format_control(Execute ptr, addr var)
{
	Return(simple_condition_format_control_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_simple_condition_format_control(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, SimpleCondition);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_simple_condition_format_control(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIMPLE_CONDITION_FORMAT_CONTROL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_simple_condition_format_control);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_simple_condition_format_control(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun simple-condition-format-arguments (conditino) ...) -> list */
static int function_simple_condition_format_arguments(Execute ptr, addr var)
{
	Return(simple_condition_format_arguments_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_simple_condition_format_arguments(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, SimpleCondition);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_simple_condition_format_arguments(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIMPLE_CONDITION_FORMAT_ARGUMENTS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_simple_condition_format_arguments);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_simple_condition_format_arguments(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun warn (datum &rest args) ...) -> nil */
static int function_warn(Execute ptr, addr datum, addr rest)
{
	Return(warn_common_(ptr, datum, rest));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_warn(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WARN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_warn);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Signal);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun invoke-debugger (condition) ...) -> nil */
static int function_invoke_debugger(Execute ptr, addr var)
{
	Return(invoke_debugger_(ptr, var));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_invoke_debugger(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Condition);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Nil);
	type_compiled_heap(args, values, ret);
}

static void defun_invoke_debugger(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INVOKE_DEBUGGER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_invoke_debugger);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_invoke_debugger(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun break (&optional format &rest args) ...) -> null */
static int function_break(Execute ptr, addr format, addr args)
{
	Return(break_common_(ptr, format, args));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_break(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, String);
	GetTypeTable(&values, T);
	typeargs_opt1rest(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_break(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BREAK, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1rest(pos, p_defun_break);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_break(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defvar debugger-hook nil) */
static void type_debugger_hook(addr *ret)
{
	addr type1, type2;

	GetTypeTable(&type1, Function);
	GetTypeTable(&type2, Symbol);
	type2or_heap(type1, type2, ret);
}

static void defvar_debugger_hook(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_DEBUGGER_HOOK, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	type_debugger_hook(&type);
	settype_value_symbol(symbol, type);
}


/* (defvar break-on-signals nil) */
static void defvar_break_on_signals(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_BREAK_ON_SIGNALS, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, TypeSpec);
	settype_value_symbol(symbol, type);
}


/* (defmacro handler-bind (binding &body form) ...)
 * -> (lisp-system::handler
 *      (lisp-system::handler-bind name1 lambda1 ...)
 *      body...)
 */
static int function_handler_bind(Execute ptr, addr form, addr env)
{
	Return(handler_bind_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_handler_bind(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_HANDLER_BIND, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_handler_bind);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro handler-case (expr &rest clause*) ...)
 * -> (lisp-system::handler
 *      (lisp-system::handler-case name1 lambda1 ...)
 *      expr)
 * -> (lisp-system::handler
 *      (lisp-system::handler-case name1 lambda1 ...)
 *      (multiple-value-call
 *        (lambda (a b c d) ...)
 *        expr))
 */
static int function_handler_case(Execute ptr, addr right, addr env)
{
	Return(handler_case_common_(ptr, right, env, &right));
	setresult_control(ptr, right);
	return 0;
}

static void defmacro_handler_case(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_HANDLER_CASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_handler_case);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro ignore-errors (&body form) ...) -> t */
static int function_ignore_errors(Execute ptr, addr form, addr env)
{
	Return(ignore_errors_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_ignore_errors(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_IGNORE_ERRORS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_ignore_errors);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro define-condition (name (supers) (slots) &rest option) ...) -> name */
static int function_define_condition(Execute ptr, addr form, addr env)
{
	Return(define_condition_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_define_condition(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_CONDITION, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_define_condition);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/*  (defun make-condition (type) &rest args) ...) -> condition */
static int function_make_condition(Execute ptr, addr args)
{
	Return(make_condition_common_(ptr, args, &args));
	setresult_control(ptr, args);
	return 0;
}

static void type_make_condition(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, ConditionDesignator);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	GetTypeTable(&values, Condition);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_condition(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_CONDITION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_make_condition);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_condition(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defun compute-restarts (&optional condition) -> restarts
 *    condition  (or condition null)
 *    restarts   list
 */
static int function_compute_restarts(Execute ptr, addr pos)
{
	Return(compute_restarts_common_(ptr, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_compute_restarts(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, OptConditionNull);
	GetTypeTable(&values, Restart);
	typevalues_rest(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_compute_restarts(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPUTE_RESTARTS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_compute_restarts);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_compute_restarts(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defun find-restart (identifier &optional condition) -> restart
 *     ideitifier  (or restart (and symbol (not null)))  ;; restart-designator
 *     condition   (or condition null)
 *     restart     (or restart null)
 */
static int function_find_restart(Execute ptr, addr var, addr opt)
{
	Return(find_restart_common_(ptr, var, opt, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_find_restart(addr *ret)
{
	addr condition, args, values;

	GetTypeTable(&args, RestartDesignator);
	GetTypeTable(&condition, ConditionNull);
	typeargs_var1opt1(&args, args, condition);
	/* restart */
	GetTypeTable(&values, RestartNull);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_find_restart(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_RESTART, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_find_restart);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_find_restart(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun invoke-restart (restart &rest arguments) ...) -> result*
 *   restart    (or restart (and symbol (not null)))  ;; restart-designator
 *   arguments  &rest t
 *   result*    *
 */
static int function_invoke_restart(Execute ptr, addr var, addr rest)
{
	return invoke_restart_control_(ptr, var, rest);
}

static void type_invoke_restart(addr *ret)
{
	addr args, values, restart;

	GetTypeTable(&restart, RestartDesignator);
	GetTypeTable(&args, T);
	typeargs_var1rest(&args, restart, args);
	GetTypeTable(&values, Asterisk);
	type_compiled_heap(args, values, ret);
}

static void defun_invoke_restart(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INVOKE_RESTART, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_invoke_restart);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_invoke_restart(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun invoke-restart-interactively (restart) ...) -> result*
 *   restart  (or restart (and symbol (not null)))  ;; restart-designator
 *   result*  *
 */
static int function_invoke_restart_interactively(Execute ptr, addr var)
{
	return invoke_restart_interactively_control_(ptr, var);
}

static void type_invoke_restart_interactively(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, RestartDesignator);
	typeargs_var1(&args, args);
	GetTypeTable(&values, Asterisk);
	type_compiled_heap(args, values, ret);
}

static void defun_invoke_restart_interactively(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INVOKE_RESTART_INTERACTIVELY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_invoke_restart_interactively);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_invoke_restart_interactively(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro restart-bind (binding &body form) ...)
 * -> (lisp-system::restart
 *      (lisp-system::restart-bind (list ...) (list ...) ...)
 *      body...)
 */
static int function_restart_bind(Execute ptr, addr right, addr env)
{
	Return(restart_bind_common_(right, env, &right));
	setresult_control(ptr, right);
	return 0;
}

static void defmacro_restart_bind(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_RESTART_BIND, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_restart_bind);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro restart-case (expr &rest clause*) ...)
 * -> (lisp-system::restart
 *      (lisp-system::restart-case (list ...) (list ...) ...)
 *      body...)
 */
static int function_restart_case(Execute ptr, addr right, addr env)
{
	Return(restart_case_common_(ptr, right, env, &right));
	setresult_control(ptr, right);
	return 0;
}

static void defmacro_restart_case(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_RESTART_CASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_restart_case);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun restart-name (restart) ...) -> symbol */
static int function_restart_name(Execute ptr, addr var)
{
	getname_restart(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_restart_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Restart);
	typeargs_var1(&args, args);
	GetTypeTable(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_restart_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RESTART_NAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_restart_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_restart_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defmacro with-condition-restarts
 *    (condition-form restarts-form &rest form) ...) -> result*
 *    condition-form  list
 *    restarts-form   list
 *    form            &rest t
 *    result*         *
 */
static int function_with_condition_restarts(Execute ptr, addr right, addr env)
{
	Return(with_condition_restarts_common_(right, &right));
	setresult_control(ptr, right);
	return 0;
}

static void defmacro_with_condition_restarts(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_CONDITION_RESTARTS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_condition_restarts);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro with-simple-restart ((name format &rest args) &body body) ...)
 *     -> result
 *   name    symbol  ;; restart-name
 *   format  format-control
 *   args    format-arguments
 *   body    progn
 *   result  (values t t)
 */
static int function_with_simple_restart(Execute ptr, addr form, addr env)
{
	Return(with_simple_restart_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_simple_restart(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_SIMPLE_RESTART, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_simple_restart);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/*
 *  (defun abort (&optional condition) ...) -> |
 */
static int function_abort(Execute ptr, addr opt)
{
	return abort_common_(ptr, opt);
}

static void defun_abort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ABORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_abort);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Abort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defun continue (&optional condition) ...) -> nil
 */
static int function_continue(Execute ptr, addr opt)
{
	Return(continue_common_(ptr, opt));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_continue(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONTINUE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_continue);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Continue);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defun muffle-warning (&optional condition) ...) -> |
 */
static int function_muffle_warning(Execute ptr, addr opt)
{
	return muffle_warning_common_(ptr, opt);
}

static void defun_muffle_warning(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MUFFLE_WARNING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_muffle_warning);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Abort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defun store-value (object &optional condition) ...) -> nil
 */
static int function_store_value(Execute ptr, addr var, addr opt)
{
	Return(store_value_common_(ptr, var, opt));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_store_value(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STORE_VALUE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_store_value);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StoreValue);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defun use-value (&optional condition) ...) -> nil
 */
static int function_use_value(Execute ptr, addr var, addr opt)
{
	Return(use_value_common_(ptr, var, opt));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_use_value(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_USE_VALUE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_use_value);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StoreValue);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_conditions(void)
{
	SetPointerCall(defun, var1, cell_error_name);
	SetPointerCall(defmacro, macro, assert);
	SetPointerCall(defun, var1rest, error);
	SetPointerCall(defun, var2rest, cerror);
	SetPointerCall(defmacro, macro, check_type);
	SetPointerCall(defun, var2rest, invalid_method_error);
	SetPointerCall(defun, var1rest, method_combination_error);
	SetPointerCall(defun, var1rest, signal);
	SetPointerCall(defun, var1, simple_condition_format_control);
	SetPointerCall(defun, var1, simple_condition_format_arguments);
	SetPointerCall(defun, var1rest, warn);
	SetPointerCall(defun, var1, invoke_debugger);
	SetPointerCall(defun, opt1rest, break);
	SetPointerCall(defmacro, macro, handler_bind);
	SetPointerCall(defmacro, macro, handler_case);
	SetPointerCall(defmacro, macro, ignore_errors);
	SetPointerCall(defmacro, macro, define_condition);
	SetPointerCall(defun, dynamic, make_condition);
	SetPointerCall(defun, opt1, compute_restarts);
	SetPointerCall(defun, var1opt1, find_restart);
	SetPointerCall(defun, var1dynamic, invoke_restart);
	SetPointerCall(defun, var1, invoke_restart_interactively);
	SetPointerCall(defmacro, macro, restart_bind);
	SetPointerCall(defmacro, macro, restart_case);
	SetPointerCall(defun, var1, restart_name);
	SetPointerCall(defmacro, macro, with_condition_restarts);
	SetPointerCall(defmacro, macro, with_simple_restart);
	SetPointerCall(defun, opt1, abort);
	SetPointerCall(defun, opt1, continue);
	SetPointerCall(defun, opt1, muffle_warning);
	SetPointerCall(defun, var1opt1, store_value);
	SetPointerCall(defun, var1opt1, use_value);
}

void build_common_conditions(void)
{
	defun_cell_error_name();
	defmacro_assert();
	defun_error();
	defun_cerror();
	defmacro_check_type();
	defun_invalid_method_error();
	defun_method_combination_error();
	defun_signal();
	defun_simple_condition_format_control();
	defun_simple_condition_format_arguments();
	defun_warn();
	defun_invoke_debugger();
	defun_break();
	defvar_debugger_hook();
	defvar_break_on_signals();
	defmacro_handler_bind();
	defmacro_handler_case();
	defmacro_ignore_errors();
	defmacro_define_condition();
	defun_make_condition();
	defun_compute_restarts();
	defun_find_restart();
	defun_invoke_restart();
	defun_invoke_restart_interactively();
	defmacro_restart_bind();
	defmacro_restart_case();
	defun_restart_name();
	defmacro_with_condition_restarts();
	defmacro_with_simple_restart();
	defun_abort();
	defun_continue();
	defun_muffle_warning();
	defun_store_value();
	defun_use_value();
}


/************************************************************
 *  common_conses.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 14. Conses
 */

/* (defun cons (object1 object2) ...) -> cons */
static int function_cons(Execute ptr, addr var1, addr var2)
{
	cons_heap(&var1, var1, var2);
	setresult_control(ptr, var1);
	return 0;
}

static void type_cons_common(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Asterisk);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Cons);
	type_compiled_heap(args, values, ret);
}

static void defun_cons(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_cons);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_cons_common(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun consp (object) ...) -> boolean */
static int function_consp(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_CONS);
	return 0;
}

static void defun_consp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONSP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_consp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun atom (object) ...) -> boolean */
static int function_atom(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) != LISPTYPE_CONS);
	return 0;
}

static void defun_atom(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ATOM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_atom);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rplaca (cons object) ...) -> cons */
static int function_rplaca(Execute ptr, addr cons, addr object)
{
	SetCar(cons, object);
	setresult_control(ptr, cons);
	return 0;
}

static void defun_rplaca(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RPLACA, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_rplaca);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Rplaca);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rplacd (cons object) ...) -> cons */
static int function_rplacd(Execute ptr, addr cons, addr object)
{
	SetCdr(cons, object);
	setresult_control(ptr, cons);
	return 0;
}

static void defun_rplacd(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RPLACD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_rplacd);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Rplaca);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun car (list) ...) -> object
 * (defun cddddr (list-cdddr) ...) -> object
 */
static void type_cxr(addr *ret, enum TypeTable cxr)
{
	addr args, values;

	gettypetable(cxr, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_cxr(constindex index, pointer p, enum TypeTable cxr)
{
	addr symbol, pos, type;

	/* function */
	GetConstant(index, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_cxr(&type, cxr);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static int function_car(Execute ptr, addr list)
{
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cadar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cddar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caaaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caaadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caadar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caaddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cadaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cadadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caddar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cadddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdaaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdaadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdadar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdaddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cddaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cddadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdddar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cddddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

#define DefunCxr(x,y,z) defun_cxr(CONSTANT_COMMON_##x, p_defun_##y, TypeTable_##z)
static void defun_car(void)
{
	DefunCxr(CAR, car, Cxr);
	DefunCxr(CDR, cdr, Cxr);
	DefunCxr(CAAR, caar, Cxar);
	DefunCxr(CADR, cadr, Cxdr);
	DefunCxr(CDAR, cdar, Cxar);
	DefunCxr(CDDR, cddr, Cxdr);
	DefunCxr(CAAAR, caaar, Cxaar);
	DefunCxr(CAADR, caadr, Cxadr);
	DefunCxr(CADAR, cadar, Cxdar);
	DefunCxr(CADDR, caddr, Cxddr);
	DefunCxr(CDAAR, cdaar, Cxaar);
	DefunCxr(CDADR, cdadr, Cxadr);
	DefunCxr(CDDAR, cddar, Cxdar);
	DefunCxr(CDDDR, cdddr, Cxddr);
	DefunCxr(CAAAAR, caaaar, Cxaaar);
	DefunCxr(CAAADR, caaadr, Cxaadr);
	DefunCxr(CAADAR, caadar, Cxadar);
	DefunCxr(CAADDR, caaddr, Cxaddr);
	DefunCxr(CADAAR, cadaar, Cxdaar);
	DefunCxr(CADADR, cadadr, Cxdadr);
	DefunCxr(CADDAR, caddar, Cxddar);
	DefunCxr(CADDDR, cadddr, Cxdddr);
	DefunCxr(CDAAAR, cdaaar, Cxaaar);
	DefunCxr(CDAADR, cdaadr, Cxaadr);
	DefunCxr(CDADAR, cdadar, Cxadar);
	DefunCxr(CDADDR, cdaddr, Cxaddr);
	DefunCxr(CDDAAR, cddaar, Cxdaar);
	DefunCxr(CDDADR, cddadr, Cxdadr);
	DefunCxr(CDDDAR, cdddar, Cxddar);
	DefunCxr(CDDDDR, cddddr, Cxdddr);
}


/* (defun (setf car) (object list) ...) -> object
 * (defun (setf cddddr) (object list-cdddr) ...) -> object
 */
static void type_setf_cxr(addr *ret, enum TypeTable cxr)
{
	addr args, values, type;

	gettypetable(cxr, &args);
	GetTypeTable(&type, T);
	typeargs_var2(&args, type, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_cxr(constindex index, pointer p, enum TypeTable cxr)
{
	addr symbol, pos, type;

	/* function */
	GetConstant(index, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2(pos, p);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_cxr(&type, cxr);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}

static int function_setf_car(Execute ptr, addr value, addr cons)
{
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdr(Execute ptr, addr value, addr cons)
{
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cadar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cddar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caaaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caaadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caadar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caaddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cadaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cadadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caddar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cadddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdaaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdaadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdadar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdaddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cddaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cddadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdddar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cddddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

#define DefunSetfCxr(x,y,z) { \
	defun_setf_cxr(CONSTANT_COMMON_##x, p_defun_setf_##y, TypeTable_##z); \
}
static void defun_setf_car(void)
{
	DefunSetfCxr(CAR, car, Cons);
	DefunSetfCxr(CDR, cdr, Cons);
	DefunSetfCxr(CAAR, caar, SetfCxar);
	DefunSetfCxr(CADR, cadr, SetfCxdr);
	DefunSetfCxr(CDAR, cdar, SetfCxar);
	DefunSetfCxr(CDDR, cddr, SetfCxdr);
	DefunSetfCxr(CAAAR, caaar, SetfCxaar);
	DefunSetfCxr(CAADR, caadr, SetfCxadr);
	DefunSetfCxr(CADAR, cadar, SetfCxdar);
	DefunSetfCxr(CADDR, caddr, SetfCxddr);
	DefunSetfCxr(CDAAR, cdaar, SetfCxaar);
	DefunSetfCxr(CDADR, cdadr, SetfCxadr);
	DefunSetfCxr(CDDAR, cddar, SetfCxdar);
	DefunSetfCxr(CDDDR, cdddr, SetfCxddr);
	DefunSetfCxr(CAAAAR, caaaar, SetfCxaaar);
	DefunSetfCxr(CAAADR, caaadr, SetfCxaadr);
	DefunSetfCxr(CAADAR, caadar, SetfCxadar);
	DefunSetfCxr(CAADDR, caaddr, SetfCxaddr);
	DefunSetfCxr(CADAAR, cadaar, SetfCxdaar);
	DefunSetfCxr(CADADR, cadadr, SetfCxdadr);
	DefunSetfCxr(CADDAR, caddar, SetfCxddar);
	DefunSetfCxr(CADDDR, cadddr, SetfCxdddr);
	DefunSetfCxr(CDAAAR, cdaaar, SetfCxaaar);
	DefunSetfCxr(CDAADR, cdaadr, SetfCxaadr);
	DefunSetfCxr(CDADAR, cdadar, SetfCxadar);
	DefunSetfCxr(CDADDR, cdaddr, SetfCxaddr);
	DefunSetfCxr(CDDAAR, cddaar, SetfCxdaar);
	DefunSetfCxr(CDDADR, cddadr, SetfCxdadr);
	DefunSetfCxr(CDDDAR, cdddar, SetfCxddar);
	DefunSetfCxr(CDDDDR, cddddr, SetfCxdddr);
}


/* (defun first (list) ...) -> object
 * (defun tenth (list-tenth) ...) -> object
 */
static int function_fifth(Execute ptr, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCar(list, &list); /*5*/
	setresult_control(ptr, list);
	return 0;
}

static int function_sixth(Execute ptr, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCar(list, &list); /*6*/
	setresult_control(ptr, list);
	return 0;
}

static int function_seventh(Execute ptr, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	GetCar(list, &list); /*7*/
	setresult_control(ptr, list);
	return 0;
}

static int function_eighth(Execute ptr, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	GetCdr(list, &list); /*7*/
	GetCar(list, &list); /*8*/
	setresult_control(ptr, list);
	return 0;
}

static int function_ninth(Execute ptr, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	GetCdr(list, &list); /*7*/
	GetCdr(list, &list); /*8*/
	GetCar(list, &list); /*9*/
	setresult_control(ptr, list);
	return 0;
}

static int function_tenth(Execute ptr, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	GetCdr(list, &list); /*7*/
	GetCdr(list, &list); /*8*/
	GetCdr(list, &list); /*9*/
	GetCar(list, &list); /*10*/
	setresult_control(ptr, list);
	return 0;
}

static void defun_first(void)
{
	DefunCxr(REST, cdr, Cxr);
	DefunCxr(FIRST, car, Cxr);
	DefunCxr(SECOND, cadr, Cxdr);
	DefunCxr(THIRD, caddr, Cxddr);
	DefunCxr(FOURTH, cadddr, Cxdddr);
	DefunCxr(FIFTH, fifth, Fifth);
	DefunCxr(SIXTH, sixth, Sixth);
	DefunCxr(SEVENTH, seventh, Seventh);
	DefunCxr(EIGHTH, eighth, Eighth);
	DefunCxr(NINTH, ninth, Ninth);
	DefunCxr(TENTH, tenth, Tenth);
}


/* (defun (setf car) (object list) ...) -> object
 * (defun (setf cddddr) (object list-cdddr) ...) -> object
 */
static int function_setf_fifth(Execute ptr, addr value, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	SetCar(list, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_sixth(Execute ptr, addr value, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	SetCar(list, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_seventh(Execute ptr, addr value, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	SetCar(list, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_eighth(Execute ptr, addr value, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	GetCdr(list, &list); /*7*/
	SetCar(list, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_ninth(Execute ptr, addr value, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	GetCdr(list, &list); /*7*/
	GetCdr(list, &list); /*8*/
	SetCar(list, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_tenth(Execute ptr, addr value, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	GetCdr(list, &list); /*7*/
	GetCdr(list, &list); /*8*/
	GetCdr(list, &list); /*9*/
	SetCar(list, value);
	setresult_control(ptr, value);
	return 0;
}

static void defun_setf_first(void)
{
	DefunSetfCxr(REST, cdr, Cons);
	DefunSetfCxr(FIRST, car, Cons);
	DefunSetfCxr(SECOND, cadr, SetfCxdr);
	DefunSetfCxr(THIRD, caddr, SetfCxddr);
	DefunSetfCxr(FOURTH, cadddr, SetfCxdddr);
	DefunSetfCxr(FIFTH, fifth, SetfFifth);
	DefunSetfCxr(SIXTH, sixth, SetfSixth);
	DefunSetfCxr(SEVENTH, seventh, SetfSeventh);
	DefunSetfCxr(EIGHTH, eighth, SetfEighth);
	DefunSetfCxr(NINTH, ninth, SetfNinth);
	DefunSetfCxr(TENTH, tenth, SetfTenth);
}


/* (defun copy-list (list) ...) -> list */
static int function_copy_list(Execute ptr, addr list)
{
	copy_list_heap_safe(&list, list);
	setresult_control(ptr, list);
	return 0;
}

static void defun_copy_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_LIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_copy_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, List_List);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun copy-tree (t) ...) -> t */
static int function_copy_tree(Execute ptr, addr list)
{
	copy_tree_heap(&list, list);
	setresult_control(ptr, list);
	return 0;
}

static void type_copy_tree(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_copy_tree(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_TREE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_copy_tree);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_copy_tree(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sublis (alist tree &key key test test-not) ...) -> tree
 *   alist     list
 *   tree      list
 *   key       (or (function (t &rest t) *) symbol) ;; or null
 *   test      (or (function (t t &rest t) *) symbol)
 *   test-not  (or (function (t t &rest t) *) symbol)
 */
static int function_sublis(Execute ptr, addr alist, addr tree, addr rest)
{
	Return(sublis_common_(ptr, alist, tree, rest, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_sublis(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBLIS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_sublis);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sublis);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nsublis (alist tree &key key test test-not) ...) -> tree
 *   alist     list
 *   tree      list
 *   key       (or (function (t &rest t) *) symbol) ;; or null
 *   test      (or (function (t t &rest t) *) symbol)
 *   test-not  (or (function (t t &rest t) *) symbol)
 */
static int function_nsublis(Execute ptr, addr alist, addr tree, addr rest)
{
	Return(nsublis_common_(ptr, alist, tree, rest, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_nsublis(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSUBLIS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_nsublis);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sublis);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subst (new old tree &key key test test-not) ...) -> tree
 *   new       object
 *   old       object
 *   tree      list
 *   key       (or (function (t &rest t) *) symbol) ;; or null
 *   test      (or (function (t t &rest t) *) symbol)
 *   test-not  (or (function (t t &rest t) *) symbol)
 */
static int function_subst(Execute ptr, addr one, addr old, addr tree, addr key)
{
	Return(subst_common_(ptr, one, old, tree, key, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_subst(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_subst);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Subst);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nsubst (new old tree &key key test test-not) ...) -> tree
 *   new       object
 *   old       object
 *   tree      list
 *   key       (or (function (t &rest t) *) symbol) ;; or null
 *   test      (or (function (t t &rest t) *) symbol)
 *   test-not  (or (function (t t &rest t) *) symbol)
 */
static int function_nsubst(Execute ptr, addr one, addr old, addr tree, addr key)
{
	Return(nsubst_common_(ptr, one, old, tree, key, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_nsubst(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSUBST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_nsubst);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Subst);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subst-if (new predicate tree &key key) ...) -> tree
 *   new        object
 *   predicate  (or (function (t &rest t) *) symbol)
 *   tree       list
 *   key        (or (function (t &rest t) *) symbol) ;; or null
 */
static int function_subst_if(Execute ptr,
		addr one, addr predicate, addr tree, addr key)
{
	Return(subst_if_common_(ptr, one, predicate, tree, key, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_subst_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBST_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_subst_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SubstIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nsubst-if (new predicate tree &key key) ...) -> tree
 *   new        object
 *   predicate  (or (function (t &rest t) *) symbol)
 *   tree       list
 *   key        (or (function (t &rest t) *) symbol) ;; or null
 */
static int function_nsubst_if(Execute ptr,
		addr one, addr predicate, addr tree, addr key)
{
	Return(nsubst_if_common_(ptr, one, predicate, tree, key, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_nsubst_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSUBST_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_nsubst_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SubstIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subst-if-not (new predicate tree &key key) ...) -> tree
 *   new        object
 *   predicate  (or (function (t &rest t) *) symbol)
 *   tree       list
 *   key        (or (function (t &rest t) *) symbol) ;; or null
 */
static int function_subst_if_not(Execute ptr,
		addr one, addr predicate, addr tree, addr key)
{
	Return(subst_if_not_common_(ptr, one, predicate, tree, key, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_subst_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBST_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_subst_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SubstIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subst-if-not (new predicate tree &key key) ...) -> tree
 *   new        object
 *   predicate  (or (function (t &rest t) *) symbol)
 *   tree       list
 *   key        (or (function (t &rest t) *) symbol) ;; or null
 */
static int function_nsubst_if_not(Execute ptr,
		addr one, addr predicate, addr tree, addr key)
{
	Return(nsubst_if_not_common_(ptr, one, predicate, tree, key, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_nsubst_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSUBST_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_nsubst_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SubstIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun tree-equal (tree1 tree2 &key test test-not) ...) -> boolean
 *   tree1     t
 *   tree2     t
 *   test      (or (function (t t &rest t) *) symbol)
 *   test-not  (or (function (t t &rest t) *) symbol)
 */
static int function_tree_equal(Execute ptr, addr tree1, addr tree2, addr key)
{
	int result;

	Return(tree_equal_common_(ptr, tree1, tree2, key, &result));
	setbool_control(ptr, result);

	return 0;
}

static void type_tree_equal(addr *ret)
{
	addr args, values, key, test, test_not, call;

	GetConst(KEYWORD_TEST, &test);
	GetConst(KEYWORD_TEST_NOT, &test_not);
	GetTypeTable(&call, FunctionDesignator);
	cons_heap(&test, test, call);
	cons_heap(&test_not, test_not, call);
	list_heap(&key, test, test_not, NULL);
	GetTypeTable(&args, T);
	typeargs_var2key(&args, args, args, key);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_tree_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TREE_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_tree_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_tree_equal(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun list (&rest objests) ...) -> list */
static int function_list(Execute ptr, addr rest)
{
	setresult_control(ptr, rest);
	return 0;
}

static void type_list_common(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_rest(pos, p_defun_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_list_common(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun list* (object &rest objects) ...) -> object */
static int function_lista(Execute ptr, addr var, addr rest)
{
	Return(lista_safe_heap_(&var, var, rest));
	setresult_control(ptr, var);
	return 0;
}

static void type_lista_common(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1rest(&args, args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_lista(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LISTA, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_lista);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_lista_common(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun list-length (list) ...) -> (or index null) */
static int function_list_length(Execute ptr, addr list)
{
	Return(list_length_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void type_list_length(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1rest(&args, args, args);
	GetTypeTable(&values, IndexNull);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_list_length(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LIST_LENGTH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_list_length);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_list_length(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun listp (object) ...) -> boolean */
static int function_listp(Execute ptr, addr var)
{
	setbool_control(ptr, IsList(var));
	return 0;
}

static void defun_listp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LISTP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_listp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-list (size &key initial-element) ...) -> list
 *   size             index
 *   initial-element  t  ;; default nil
 */
static int function_make_list(Execute ptr, addr var, addr rest)
{
	Return(make_list_common_(var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_make_list(addr *ret)
{
	addr args, values, key, symbol, type;

	GetTypeTable(&args, Index);
	GetConst(KEYWORD_INITIAL_ELEMENT, &symbol);
	GetTypeTable(&type, T);
	cons_heap(&key, symbol, type);
	conscar_heap(&key, key);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_make_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_LIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_make_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro push (item place) ...) -> value
 *   item   t
 *   place  setf-place
 *   value  t
 */
static int function_push(Execute ptr, addr form, addr env)
{
	Return(push_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_push(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PUSH, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_push);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro pop (place) ...) -> t */
static int function_pop(Execute ptr, addr form, addr env)
{
	Return(pop_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_pop(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_POP, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_pop);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun nth (index list) ...) -> object
 *   index  (integer 0 *)  ;; Don't use index (SizeMax)
 *   list   list
 */
static int function_nth(Execute ptr, addr index, addr list)
{
	Return(nth_common_(index, list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_nth(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NTH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_nth);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Nth);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf nth) (value index list) ...) -> value
 *   index  index
 *   list   list
 */
static int function_setf_nth(Execute ptr, addr value, addr index, addr list)
{
	Return(setf_nth_common_(value, index, list));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_nth(addr *ret)
{
	addr args, values, type, value;

	GetTypeTable(&value, T);
	GetTypeTable(&args, Index);
	GetTypeTable(&type, List);
	typeargs_var3(&args, value, args, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_nth(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NTH, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_setf_nth);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_nth(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun nthcdr (index list) ...) -> object */
static int function_nthcdr(Execute ptr, addr index, addr list)
{
	Return(nthcdr_common_(index, list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_nthcdr(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NTHCDR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_nthcdr);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Nth);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun member (item list &key test test-not) ...) -> tail
 *   item      t
 *   list      list
 *   key       (or function-designator null))
 *   test      function-designator
 *   test-not  function-designator
 *   tail      list
 */
static int function_member(Execute ptr, addr item, addr list, addr rest)
{
	Return(member_common_(ptr, item, list, rest, &item));
	setresult_control(ptr, item);
	return 0;
}

static void defun_member(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MEMBER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_member);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Member);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun member-if (call list &key key) ...) -> tail
 *   call   function-designator
 *   list   list
 *   key    (or function-designator null)
 *   tail   list
 */
static int function_member_if(Execute ptr, addr call, addr list, addr rest)
{
	Return(member_if_common_(ptr, call, list, rest, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_member_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MEMBER_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_member_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun member-if-not (call list &key key) ...) -> tail
 *   call   function-designator
 *   list   list
 *   key    (or function-designator null)
 *   tail   list
 */
static int function_member_if_not(Execute ptr, addr call, addr list, addr rest)
{
	Return(member_if_not_common_(ptr, call, list, rest, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_member_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MEMBER_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_member_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mapc (call list &rest list) ...) -> list */
static int function_mapc(Execute ptr, addr call, addr rest)
{
	Return(mapc_common_(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_mapc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPC, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_mapc);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mapcar (call list &rest list) ...) -> list */
static int function_mapcar(Execute ptr, addr call, addr rest)
{
	Return(mapcar_common_(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_mapcar(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPCAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_mapcar);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mapcan (call list &rest list) ...) -> list */
static int function_mapcan(Execute ptr, addr call, addr rest)
{
	Return(mapcan_common_(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_mapcan(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPCAN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_mapcan);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mapl (call list &rest list) ...) -> list */
static int function_mapl(Execute ptr, addr call, addr rest)
{
	Return(mapl_common_(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_mapl(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_mapl);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun maplist (call list &rest list) ...) -> list */
static int function_maplist(Execute ptr, addr call, addr rest)
{
	Return(maplist_common_(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_maplist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPLIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_maplist);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mapcon (call list &rest list) ...) -> list */
static int function_mapcon(Execute ptr, addr call, addr rest)
{
	Return(mapcon_common_(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_mapcon(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPCON, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_mapcon);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun endp (list) ...) -> boolean */
static int function_endp(Execute ptr, addr list)
{
	setbool_control(ptr, list == Nil);
	return 0;
}

static void type_endp(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_endp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ENDP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_endp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_endp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun null (object) ...) -> boolean */
static int function_null(Execute ptr, addr list)
{
	setbool_control(ptr, list == Nil);
	return 0;
}

static void defun_null(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NULL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_null);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nconc (&rest object) ...) -> result
 *   object  t  ;; (list list ... . t)
 *   result  t  ;; (or list t)
 */
static int function_nconc(Execute ptr, addr list)
{
	Return(nconc_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_nconc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NCONC, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_nconc);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Nconc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun append (&rest object) ...) -> result
 *   object  t  ;; (list list ... . t)
 *   result  t  ;; (or list t)
 */
static int function_append(Execute ptr, addr list)
{
	Return(append_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_append(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_APPEND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_append);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Nconc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun revappend (list tail) ...) -> object
 *   list    list
 *   tail    t
 *   object  t
 */
static int function_revappend(Execute ptr, addr list, addr tail)
{
	Return(revappend_common_(list, tail, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_revappend(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REVAPPEND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_revappend);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Nreconc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nreconc (list tail) ...) -> object
 *   list    list
 *   tail    t
 *   object  t
 */
static int function_nreconc(Execute ptr, addr list, addr tail)
{
	Return(nreconc_common_(list, tail, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_nreconc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NRECONC, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_nreconc);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Nreconc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun butlast (list &optional intplus) ...) -> list
 *    index  (integer 0 *)
 */
static int function_butlast(Execute ptr, addr list, addr index)
{
	Return(butlast_common_(list, index, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_butlast(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BUTLAST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_butlast);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ButLast);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nbutlast (list &optional intplus) ...) -> list
 *    index  (integer 0 *)
 */
static int function_nbutlast(Execute ptr, addr list, addr index)
{
	Return(nbutlast_common_(list, index, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_nbutlast(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NBUTLAST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_nbutlast);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ButLast);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun last (list &optional intplus) ...) -> object
 *   index   (integer 0 *)
 *   object  t
 */
static int function_last(Execute ptr, addr list, addr index)
{
	Return(last_common_(list, index, &list));
	setresult_control(ptr, list);
	return 0;
}

static void type_last(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, List);
	GetTypeTable(&type, Intplus);
	typeargs_var1opt1(&args, args, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_last(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LAST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_last);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_last(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ldiff (list object) ...) -> list */
static int function_ldiff(Execute ptr, addr list, addr object)
{
	ldiff_common(list, object, &list);
	setresult_control(ptr, list);
	return 0;
}

static void type_ldiff(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, List);
	GetTypeTable(&type, T);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_ldiff(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LDIFF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_ldiff);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_ldiff(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun tailp (object list) ...) -> boolean */
static int function_tailp(Execute ptr, addr object, addr list)
{
	int check;

	tailp_common(object, list, &check);
	setbool_control(ptr, check);

	return 0;
}

static void type_tailp(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&type, List);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_tailp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TAILP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_tailp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_tailp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun acons (key datum alist) ...) -> list
 *   key    t
 *   datum  t
 *   alist  list
 */
static int function_acons(Execute ptr, addr key, addr datum, addr list)
{
	cons_heap(&key, key, datum);
	cons_heap(&list, key, list);
	setresult_control(ptr, list);

	return 0;
}

static void type_acons(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&type, List);
	typeargs_var3(&args, args, args, type);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_acons(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ACONS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_acons);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_acons(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun assoc (item list &key key test test-not) ...) -> entry
 *   item   t
 *   &key   [key, test, test-not type]
 *   entry  list
 */
static int function_assoc(Execute ptr, addr item, addr list, addr rest)
{
	Return(assoc_common_(ptr, item, list, rest, &item));
	setresult_control(ptr, item);
	return 0;
}

static void defun_assoc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASSOC, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_assoc);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Member);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun assoc-if (call list &key key) ...) -> list */
static int function_assoc_if(Execute ptr, addr call, addr list, addr rest)
{
	Return(assoc_if_common_(ptr, call, list, rest, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_assoc_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASSOC_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_assoc_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun assoc-if-not (call list &key key) ...) -> list */
static int function_assoc_if_not(Execute ptr, addr call, addr list, addr rest)
{
	Return(assoc_if_not_common_(ptr, call, list, rest, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_assoc_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASSOC_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_assoc_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun copy-alist (list) ...) -> list */
static int function_copy_alist(Execute ptr, addr list)
{
	Return(copy_alist_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_copy_alist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_ALIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_copy_alist);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, List_List);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pairlis (keys data &optional list) ...) -> list
 *   keys    list
 *   data    list
 */
static int function_pairlis(Execute ptr, addr keys, addr data, addr list)
{
	Return(pairlis_common_(keys, data, list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void type_pairlis(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var2opt1(&args, args, args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_pairlis(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PAIRLIS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_pairlis);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pairlis(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rassoc (item list &key key test test-not) ...) -> entry
 *   item   t
 *   &key   [key, test, test-not type]
 *   entry  list
 */
static int function_rassoc(Execute ptr, addr item, addr list, addr rest)
{
	Return(rassoc_common_(ptr, item, list, rest, &item));
	setresult_control(ptr, item);
	return 0;
}

static void defun_rassoc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RASSOC, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_rassoc);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Member);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rassoc-if (call list &key key) ...) -> list */
static int function_rassoc_if(Execute ptr, addr call, addr list, addr rest)
{
	Return(rassoc_if_common_(ptr, call, list, rest, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_rassoc_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RASSOC_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_rassoc_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rassoc-if (call list &key key) ...) -> list */
static int function_rassoc_if_not(Execute ptr, addr call, addr list, addr rest)
{
	Return(rassoc_if_not_common_(ptr, call, list, rest, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_rassoc_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RASSOC_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_rassoc_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get-properties (plist indicator-list) ...) -> indicator, value, tail
 *   plist           list
 *   indicator-list  list
 *   indicator       t
 *   value           t
 *   tail            list
 */
static int function_get_properties(Execute ptr, addr plist, addr indicator)
{
	addr key, value, list;

	Return(get_properties_common_(plist, indicator, &key, &value, &list));
	setvalues_control(ptr, key, value, list, NULL);

	return 0;
}

static void type_get_properties(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, List);
	typeargs_var2(&args, args, args);
	GetTypeTable(&type, T);
	GetTypeTable(&values, List);
	typevalues_values3(&values, type, type, values);
	type_compiled_heap(args, values, ret);
}

static void defun_get_properties(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_PROPERTIES, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_get_properties);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get_properties(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun getf (list indicator &optional default) ...) -> value
 *   indicator  t
 *   default    t   ;; default nil
 *   value      t
 */
static int function_getf(Execute ptr, addr list, addr key, addr value)
{
	Return(getf_common_(list, key, value, &value));
	setresult_control(ptr, value);
	return 0;
}

static void type_getf(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var2opt1(&args, values, args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_getf(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GETF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_getf);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_getf(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (define-setf-expander getf (place indicator &optional default) ...)
 *   place      t
 *   default    t   ;; default nil
 *   value      t
 */
static void define_setf_expander_getf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_GETF, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_setf_getf);
	SetSetfMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro remf (place indicator) ...) -> boolean */
static int function_remf(Execute ptr, addr form, addr env)
{
	Return(remf_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_remf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_REMF, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_remf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun intersection (list1 list2 &key test test-not) ...) -> result
 *   list1   list
 *   list2   list
 *   result  list
 */
static int function_intersection(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(intersection_common_(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_intersection(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INTERSECTION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_intersection);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nintersection (list1 list2 &key test test-not) ...) -> result
 *   list1   list
 *   list2   list
 *   result  list
 */
static int function_nintersection(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(nintersection_common_(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_nintersection(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NINTERSECTION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_nintersection);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun adjoin (item list &key key test test-not) ...) -> list
 *   item  t
 */
static int function_adjoin(Execute ptr, addr item, addr list, addr rest)
{
	Return(adjoin_common_(ptr, item, list, rest, &item));
	setresult_control(ptr, item);
	return 0;
}

static void type_adjoin(addr *ret)
{
	addr args, values, key;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	GetTypeTable(&key, KeyTestList);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_adjoin(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ADJOIN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_adjoin);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_adjoin(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro pushnew (item place &rest args) ...) -> value
 *   item   t
 *   place  setf-place
 *   value  t
 */
static int function_pushnew(Execute ptr, addr form, addr env)
{
	Return(pushnew_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_pushnew(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PUSHNEW, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_pushnew);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun set-difference (list1 list2) &key key test test-not) ...) -> list */
static int function_set_difference(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(set_difference_common_(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_set_difference(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET_DIFFERENCE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_set_difference);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nset-difference (list1 list2 &key test test-not) ...) -> result
 *   list1   list
 *   list2   list
 *   result  list
 */
static int function_nset_difference(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(nset_difference_common_(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_nset_difference(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSET_DIFFERENCE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_nset_difference);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun set-exclusive-or (list1 list2 &key test test-not) ...) -> result
 *   list1   list
 *   list2   list
 *   result  list
 */
static int function_set_exclusive_or(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(set_exclusive_or_common_(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_set_exclusive_or(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET_EXCLUSIVE_OR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_set_exclusive_or);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nset-exclusive-or (list1 list2 &key test test-not) ...) -> result
 *   list1   list
 *   list2   list
 *   result  list
 */
static int function_nset_exclusive_or(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(nset_exclusive_or_common_(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_nset_exclusive_or(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSET_EXCLUSIVE_OR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_nset_exclusive_or);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subsetp (list1 list2 &key test test-not) ...) -> boolean */
static int function_subsetp(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(subsetp_common_(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void type_subsetp(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	GetTypeTable(&values, KeyTestList);
	typeargs_var2key(&args, args, args, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_subsetp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBSETP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_subsetp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_subsetp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun union (list1 list2 &key test test-not) ...) -> result
 *   list1   list
 *   list2   list
 *   result  list
 */
static int function_union(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(union_common_(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_union(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_union);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nunion (list1 list2 &key test test-not) ...) -> result
 *   list1   list
 *   list2   list
 *   result  list
 */
static int function_nunion(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(nunion_common_(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_nunion(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUNION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_nunion);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_conses(void)
{
	SetPointerCall(defun, var1, car);
	SetPointerCall(defun, var1, cdr);
	SetPointerCall(defun, var1, caar);
	SetPointerCall(defun, var1, cadr);
	SetPointerCall(defun, var1, cdar);
	SetPointerCall(defun, var1, cddr);
	SetPointerCall(defun, var1, caaar);
	SetPointerCall(defun, var1, caadr);
	SetPointerCall(defun, var1, cadar);
	SetPointerCall(defun, var1, caddr);
	SetPointerCall(defun, var1, cdaar);
	SetPointerCall(defun, var1, cdadr);
	SetPointerCall(defun, var1, cddar);
	SetPointerCall(defun, var1, cdddr);
	SetPointerCall(defun, var1, caaaar);
	SetPointerCall(defun, var1, caaadr);
	SetPointerCall(defun, var1, caadar);
	SetPointerCall(defun, var1, caaddr);
	SetPointerCall(defun, var1, cadaar);
	SetPointerCall(defun, var1, cadadr);
	SetPointerCall(defun, var1, caddar);
	SetPointerCall(defun, var1, cadddr);
	SetPointerCall(defun, var1, cdaaar);
	SetPointerCall(defun, var1, cdaadr);
	SetPointerCall(defun, var1, cdadar);
	SetPointerCall(defun, var1, cdaddr);
	SetPointerCall(defun, var1, cddaar);
	SetPointerCall(defun, var1, cddadr);
	SetPointerCall(defun, var1, cdddar);
	SetPointerCall(defun, var1, cddddr);
	SetPointerCall(defun, var1, fifth);
	SetPointerCall(defun, var1, sixth);
	SetPointerCall(defun, var1, seventh);
	SetPointerCall(defun, var1, eighth);
	SetPointerCall(defun, var1, ninth);
	SetPointerCall(defun, var1, tenth);

	SetPointerCall(defun, var2, setf_car);
	SetPointerCall(defun, var2, setf_cdr);
	SetPointerCall(defun, var2, setf_caar);
	SetPointerCall(defun, var2, setf_cadr);
	SetPointerCall(defun, var2, setf_cdar);
	SetPointerCall(defun, var2, setf_cddr);
	SetPointerCall(defun, var2, setf_caaar);
	SetPointerCall(defun, var2, setf_caadr);
	SetPointerCall(defun, var2, setf_cadar);
	SetPointerCall(defun, var2, setf_caddr);
	SetPointerCall(defun, var2, setf_cdaar);
	SetPointerCall(defun, var2, setf_cdadr);
	SetPointerCall(defun, var2, setf_cddar);
	SetPointerCall(defun, var2, setf_cdddr);
	SetPointerCall(defun, var2, setf_caaaar);
	SetPointerCall(defun, var2, setf_caaadr);
	SetPointerCall(defun, var2, setf_caadar);
	SetPointerCall(defun, var2, setf_caaddr);
	SetPointerCall(defun, var2, setf_cadaar);
	SetPointerCall(defun, var2, setf_cadadr);
	SetPointerCall(defun, var2, setf_caddar);
	SetPointerCall(defun, var2, setf_cadddr);
	SetPointerCall(defun, var2, setf_cdaaar);
	SetPointerCall(defun, var2, setf_cdaadr);
	SetPointerCall(defun, var2, setf_cdadar);
	SetPointerCall(defun, var2, setf_cdaddr);
	SetPointerCall(defun, var2, setf_cddaar);
	SetPointerCall(defun, var2, setf_cddadr);
	SetPointerCall(defun, var2, setf_cdddar);
	SetPointerCall(defun, var2, setf_cddddr);
	SetPointerCall(defun, var2, setf_fifth);
	SetPointerCall(defun, var2, setf_sixth);
	SetPointerCall(defun, var2, setf_seventh);
	SetPointerCall(defun, var2, setf_eighth);
	SetPointerCall(defun, var2, setf_ninth);
	SetPointerCall(defun, var2, setf_tenth);

	SetPointerCall(defun, var2, cons);
	SetPointerCall(defun, var1, consp);
	SetPointerCall(defun, var1, atom);
	SetPointerCall(defun, var2, rplaca);
	SetPointerCall(defun, var2, rplacd);
	SetPointerCall(defun, var1, copy_list);
	SetPointerCall(defun, var1, copy_tree);
	SetPointerCall(defun, var2dynamic, sublis);
	SetPointerCall(defun, var2dynamic, nsublis);
	SetPointerCall(defun, var3dynamic, subst);
	SetPointerCall(defun, var3dynamic, nsubst);
	SetPointerCall(defun, var3dynamic, subst_if);
	SetPointerCall(defun, var3dynamic, nsubst_if);
	SetPointerCall(defun, var3dynamic, subst_if_not);
	SetPointerCall(defun, var3dynamic, nsubst_if_not);
	SetPointerCall(defun, var2dynamic, tree_equal);
	SetPointerCall(defun, rest, list);
	SetPointerCall(defun, var1dynamic, lista);
	SetPointerCall(defun, var1, list_length);
	SetPointerCall(defun, var1, listp);
	SetPointerCall(defun, var1dynamic, make_list);
	SetPointerCall(defmacro, macro, push);
	SetPointerCall(defmacro, macro, pop);
	SetPointerCall(defun, var2, nth);
	SetPointerCall(defun, var3, setf_nth);
	SetPointerCall(defun, var2, nthcdr);
	SetPointerCall(defun, var2dynamic, member);
	SetPointerCall(defun, var2dynamic, member_if);
	SetPointerCall(defun, var2dynamic, member_if_not);
	SetPointerCall(defun, var1dynamic, mapc);
	SetPointerCall(defun, var1dynamic, mapcar);
	SetPointerCall(defun, var1dynamic, mapcan);
	SetPointerCall(defun, var1dynamic, mapl);
	SetPointerCall(defun, var1dynamic, maplist);
	SetPointerCall(defun, var1dynamic, mapcon);
	SetPointerCall(defun, var1, endp);
	SetPointerCall(defun, var1, null);
	SetPointerCall(defun, dynamic, nconc);
	SetPointerCall(defun, dynamic, append);
	SetPointerCall(defun, var2, revappend);
	SetPointerCall(defun, var2, nreconc);
	SetPointerCall(defun, var1opt1, butlast);
	SetPointerCall(defun, var1opt1, nbutlast);
	SetPointerCall(defun, var1opt1, last);
	SetPointerCall(defun, var2, ldiff);
	SetPointerCall(defun, var2, tailp);
	SetPointerCall(defun, var3, acons);
	SetPointerCall(defun, var2dynamic, assoc);
	SetPointerCall(defun, var2dynamic, assoc_if);
	SetPointerCall(defun, var2dynamic, assoc_if_not);
	SetPointerCall(defun, var1, copy_alist);
	SetPointerCall(defun, var2opt1, pairlis);
	SetPointerCall(defun, var2dynamic, rassoc);
	SetPointerCall(defun, var2dynamic, rassoc_if);
	SetPointerCall(defun, var2dynamic, rassoc_if_not);
	SetPointerCall(defun, var2, get_properties);
	SetPointerCall(defun, var2opt1, getf);
	SetPointerCall(defmacro, macro, setf_getf);
	SetPointerCall(defmacro, macro, remf);
	SetPointerCall(defun, var2dynamic, intersection);
	SetPointerCall(defun, var2dynamic, nintersection);
	SetPointerCall(defun, var2dynamic, adjoin);
	SetPointerCall(defmacro, macro, pushnew);
	SetPointerCall(defun, var2dynamic, set_difference);
	SetPointerCall(defun, var2dynamic, nset_difference);
	SetPointerCall(defun, var2dynamic, set_exclusive_or);
	SetPointerCall(defun, var2dynamic, nset_exclusive_or);
	SetPointerCall(defun, var2dynamic, subsetp);
	SetPointerCall(defun, var2dynamic, union);
	SetPointerCall(defun, var2dynamic, nunion);
}

void build_common_conses(void)
{
	defun_cons();
	defun_consp();
	defun_atom();
	defun_rplaca();
	defun_rplacd();
	defun_car();
	defun_setf_car();
	defun_first();
	defun_setf_first();
	defun_copy_list();
	defun_copy_tree();
	defun_sublis();
	defun_nsublis();
	defun_subst();
	defun_nsubst();
	defun_subst_if();
	defun_nsubst_if();
	defun_subst_if_not();
	defun_nsubst_if_not();
	defun_tree_equal();
	defun_list();
	defun_lista();
	defun_list_length();
	defun_listp();
	defun_make_list();
	defmacro_push();
	defmacro_pop();
	defun_nth();
	defun_setf_nth();
	defun_endp();
	defun_null();
	defun_nconc();
	defun_append();
	defun_revappend();
	defun_nreconc();
	defun_butlast();
	defun_nbutlast();
	defun_last();
	defun_ldiff();
	defun_tailp();
	defun_nthcdr();
	defun_member();
	defun_member_if();
	defun_member_if_not();
	defun_mapc();
	defun_mapcar();
	defun_mapcan();
	defun_mapl();
	defun_maplist();
	defun_mapcon();
	defun_acons();
	defun_assoc();
	defun_assoc_if();
	defun_assoc_if_not();
	defun_copy_alist();
	defun_pairlis();
	defun_rassoc();
	defun_rassoc_if();
	defun_rassoc_if_not();
	defun_get_properties();
	defun_getf();
	define_setf_expander_getf();
	defmacro_remf();
	defun_intersection();
	defun_nintersection();
	defun_adjoin();
	defmacro_pushnew();
	defun_set_difference();
	defun_nset_difference();
	defun_set_exclusive_or();
	defun_nset_exclusive_or();
	defun_subsetp();
	defun_union();
	defun_nunion();
}


/************************************************************
 *  common_data.c
 ************************************************************/
/*
 *  ANSI COMMON LISP: 5. Data and Control Flow
 */

/* (defun apply (call pos &rest args) ...) -> value
 *   call   (or function symbol)  ;; function-designator
 *   pos    t
 *   args   t
 *   value  (values &rest t)
 */
static int function_apply(Execute ptr, addr call, addr pos, addr args)
{
	return apply_common_(ptr, call, pos, args);
}

static void type_apply(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, FunctionDesignator);
	GetTypeTable(&type, T);
	typeargs_var2rest(&args, args, type, type);
	typevalues_rest(&values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_apply(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_APPLY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_apply);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_apply(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (define-setf-expander apply (place indicator &optional default) ...)
 *   place      t
 *   default    t   ;; default nil
 *   value      t
 */
static void define_setf_expander_apply(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_APPLY, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_setf_apply);
	SetSetfMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defun (name lambda-list &rest body) ...) */
static int function_defun(Execute ptr, addr right, addr env)
{
	Return(defun_common_(ptr, right, env, &right));
	setresult_control(ptr, right);
	return 0;
}

static void defmacro_defun(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFUN, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defun);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun fdefinition (name) ...) -> function
 *   name  (or symbol (cons (eql setf) (cons symbol null))) -> function-name
 */
static int function_fdefinition(Execute ptr, addr name)
{
	Return(fdefinition_common_(ptr, name, &name));
	setresult_control(ptr, name);
	return 0;
}

static void type_fdefinition(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, FunctionName);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_fdefinition(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FDEFINITION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_fdefinition);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_fdefinition(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf fdefinition) (function name) ...) -> function
 *   name   function-name
 */
static int function_setf_fdefinition(Execute ptr, addr value, addr name)
{
	Return(setf_fdefinition_common_(value, name));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_fdefinition(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Function);
	GetTypeTable(&values, FunctionName);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_fdefinition(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FDEFINITION, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_setf_fdefinition);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_fdefinition(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun fbound (name) ...) -> boolean
 *   name  function-name
 */
static int function_fboundp(Execute ptr, addr name)
{
	int check;

	Return(fboundp_common_(name, &check));
	setbool_control(ptr, check);

	return 0;
}

static void type_fboundp(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, FunctionName);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_fboundp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FBOUNDP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_fboundp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_fboundp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun fmakunbound (name) ...) -> name
 *   name  function-name
 */
static int function_fmakunbound(Execute ptr, addr name)
{
	Return(fmakunbound_common_(name));
	setresult_control(ptr, name);
	return 0;
}

static void type_fmakunbound(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, FunctionName);
	typeargs_var1(&args, args);
	GetTypeTable(&values, FunctionName);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_fmakunbound(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FMAKUNBOUND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_fmakunbound);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_fmakunbound(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* special-operator flet */
static void defspecial_flet(void)
{
	DefineSpecialOperator(COMMON_FLET);
}


/* special-operator labels */
static void defspecial_labels(void)
{
	DefineSpecialOperator(COMMON_LABELS);
}


/* special-operator macrolet */
static void defspecial_macrolet(void)
{
	DefineSpecialOperator(COMMON_MACROLET);
}


/* (defun funcall (function &rest args) ...) -> value
 *   function  (or function symbol)  ;; function-designator
 *   args      t
 *   value     (values &rest t)
 */
static int function_funcall(Execute ptr, addr call, addr args)
{
	return funcall_common_(ptr, call, args);
}

static void type_funcall(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, FunctionDesignator);
	GetTypeTable(&type, T);
	typeargs_var1rest(&args, args, type);
	typevalues_rest(&values, type);
	type_compiled_heap(args, values, ret);
}

static void defvar_macroexpand_hook(addr value)
{
	addr symbol;
	GetConst(SPECIAL_MACROEXPAND_HOOK, &symbol);
	SetValueSymbol(symbol, value);
}

static void defun_funcall(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FUNCALL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_funcall);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_funcall(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);

	/* (defvar *macroexpand-hook* #'funcall) */
	defvar_macroexpand_hook(pos);
}


/* special-operator function */
static void defspecial_function(void)
{
	DefineSpecialOperator(COMMON_FUNCTION);
}


/* (defun function-lambda-expression (function) ...)
 *   -> lambda-expression, closure-p, name
 *   lambda-expression  t
 *   closure-p          boolean
 *   name               function-name
 */
static int function_function_lambda_expression(Execute ptr, addr var)
{
	addr pos1, pos2, pos3;

	function_lambda_expression_common(var, &pos1, &pos2, &pos3);
	setvalues_control(ptr, pos1, pos2, pos3, NULL);
	return 0;
}

static void type_function_lambda_expression(addr *ret)
{
	addr args, values, type1, type2, type3, null;

	GetTypeTable(&args, Function);
	typeargs_var1(&args, args);
	GetTypeTable(&type1, T);
	GetTypeTable(&type2, Boolean);
	GetTypeTable(&type3, FunctionName);
	GetTypeTable(&null, Null);
	type2or_heap(type3, null, &type3);
	typevalues_values3(&values, type1, type2, type3);
	type_compiled_heap(args, values, ret);
}

static void defun_function_lambda_expression(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FUNCTION_LAMBDA_EXPRESSION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_function_lambda_expression);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_function_lambda_expression(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun functionp (object) ...) -> boolean */
static int function_functionp(Execute ptr, addr var)
{
	int check;
	Return(funcallp_(var, &check));
	setbool_control(ptr, check);
	return 0;
}

static void defun_functionp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FUNCTIONP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_functionp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun compiled-function-p (object) ...) -> boolean */
static int function_compiled_function_p(Execute ptr, addr var)
{
	setbool_control(ptr, compiled_function_p(var));
	return 0;
}

static void defun_compiled_function_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPILED_FUNCTION_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_compiled_function_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defconstant call-arguments-limit FIXNUM-MAX) */
static void defconstant_call_arguments_limit(void)
{
	addr symbol, value;

	GetConst(COMMON_CALL_ARGUMENTS_LIMIT, &symbol);
	GetConst(FIXNUM_MAX, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant lambda-list-keywords
 *   '(&optional &rest &body &key &allow-other-keys &aux
 *     &whole &environment))
 */
static void defconstant_lambda_list_keywords(void)
{
	addr list, pos;

	lambda_list_keywords_common(&list);
	GetConst(COMMON_LAMBDA_LIST_KEYWORDS, &pos);
	defconstant_symbol(pos, list);
}


/* (defconstant lambda-parameters-limit FIXNUM-MAX) */
static void defconstant_lambda_parameters_limit(void)
{
	addr symbol, value;

	GetConst(COMMON_LAMBDA_PARAMETERS_LIMIT, &symbol);
	GetConst(FIXNUM_MAX, &value);
	defconstant_symbol(symbol, value);
}


/* (defmacro defconstant (name value &optional documentation) ...) -> name
 *    name           symbol
 *    value          t
 *    documentation  string
 */
static int function_defconstant(Execute ptr, addr form, addr env)
{
	Return(defconstant_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defconstant(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFCONSTANT, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defconstant);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defparameter (name value &optional documentation) ...) -> name
 *    name           symbol
 *    value          t
 *    documentation  string
 */
static int function_defparameter(Execute ptr, addr form, addr env)
{
	Return(defparameter_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defparameter(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFPARAMETER, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defparameter);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defvar (symbol &optional value document) ...) -> symbol */
static int function_defvar(Execute ptr, addr form, addr env)
{
	Return(defvar_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defvar(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFVAR, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defvar);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro destructuring-bind (lambda expr &body body) ...) -> t */
static int function_destructuring_bind(Execute ptr, addr form, addr env)
{
	Return(destructuring_bind_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_destructuring_bind(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DESTRUCTURING_BIND, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_destructuring_bind);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator let */
static void defspecial_let(void)
{
	DefineSpecialOperator(COMMON_LET);
}


/* special-operator let* */
static void defspecial_leta(void)
{
	DefineSpecialOperator(COMMON_LETA);
}


/* special-operator progv */
static void defspecial_progv(void)
{
	DefineSpecialOperator(COMMON_PROGV);
}


/* special-operator setq */
static void defspecial_setq(void)
{
	DefineSpecialOperator(COMMON_SETQ);
}


/* (defmacro psetq (&rest args) ...) -> value */
static int function_psetq(Execute ptr, addr form, addr env)
{
	Return(psetq_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_psetq(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PSETQ, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_psetq);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator block */
static void defspecial_block(void)
{
	DefineSpecialOperator(COMMON_BLOCK);
}


/* special-operator catch */
static void defspecial_catch(void)
{
	DefineSpecialOperator(COMMON_CATCH);
}


/* special-operator go */
static void defspecial_go(void)
{
	DefineSpecialOperator(COMMON_GO);
}


/* special-operator return-from */
static void defspecial_return_from(void)
{
	DefineSpecialOperator(COMMON_RETURN_FROM);
}


/* (defmacro return (&optional value) ...) */
static int function_return(Execute ptr, addr form, addr env)
{
	Return(return_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_return(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_RETURN, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_return);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator tagbody */
static void defspecial_tagbody(void)
{
	DefineSpecialOperator(COMMON_TAGBODY);
}


/* special-operator throw */
static void defspecial_throw(void)
{
	DefineSpecialOperator(COMMON_THROW);
}


/* special-operator unwind-protect */
static void defspecial_unwind_protect(void)
{
	DefineSpecialOperator(COMMON_UNWIND_PROTECT);
}


/* (defun not (object) ...) -> boolean */
static int function_not(Execute ptr, addr list)
{
	setbool_control(ptr, list == Nil);
	return 0;
}

static void defun_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eq (x y) ...) -> boolean */
static int function_eq(Execute ptr, addr x, addr y)
{
	setbool_control(ptr, eq_function(x, y));
	return 0;
}

static void defun_eq(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EQ, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_eq);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Eq);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eql (x y) ...) -> boolean */
static int function_eql(Execute ptr, addr x, addr y)
{
	setbool_control(ptr, eql_function(x, y));
	return 0;
}

static void defun_eql(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EQL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_eql);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Eq);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun equal (x y) ...) -> boolean */
static int function_equal(Execute ptr, addr x, addr y)
{
	int check;

	Return(equal_function_(x, y, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Eq);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun equalp (x y) ...) -> boolean */
static int function_equalp(Execute ptr, addr x, addr y)
{
	int check;

	Return(equalp_function_(x, y, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_equalp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EQUALP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_equalp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Eq);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun identity (object) ...) -> object */
static int function_identity(Execute ptr, addr var)
{
	setresult_control(ptr, var);
	return 0;
}

static void type_identity(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_identity(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_IDENTITY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_identity);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_identity(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun complement (function) ...) -> function */
static int function_lambda_complement(Execute ptr, addr rest)
{
	addr pos;

	getdata_control(ptr, &pos);
	Return(apply1_control_(ptr, &pos, pos, rest));
	setbool_control(ptr, pos == Nil);

	return 0;
}

static int function_complement(Execute ptr, addr var)
{
	complement_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_complement(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Function);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_complement(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPLEMENT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_complement);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_complement(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun constantly (value) ...) -> function */
static int function_lambda_constantly(Execute ptr)
{
	addr pos;

	getdata_control(ptr, &pos);
	setresult_control(ptr, pos);
	return 0;
}

static int function_constantly(Execute ptr, addr var)
{
	constantly_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_constantly(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_constantly(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONSTANTLY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_constantly);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_constantly(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun every (call &rest sequence+) ...) -> boolean */
static int function_every(Execute ptr, addr call, addr rest)
{
	Return(every_common_(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_every(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EVERY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_every);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Every);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun some (call &rest sequence+) ...) -> result */
static int function_some(Execute ptr, addr call, addr rest)
{
	Return(some_common_(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void type_some(addr *ret)
{
	addr args, values, call, sequence;

	GetTypeTable(&call, FunctionDesignator);
	GetTypeTable(&sequence, Sequence);
	typeargs_var2rest(&args, call, sequence, sequence);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_some(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SOME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_some);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_some(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun notany (call &rest sequence+) ...) -> boolean */
static int function_notany(Execute ptr, addr call, addr rest)
{
	/* (notany predicate sequence*) ==  (not (some predicate sequence*)) */
	Return(notany_common_(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_notany(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NOTANY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_notany);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Every);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun notevery (call &rest sequence+) ...) -> boolean */
static int function_notevery(Execute ptr, addr call, addr rest)
{
	Return(notevery_common_(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_notevery(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NOTEVERY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_notevery);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Every);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro and (&rest form) ...) */
static int function_and(Execute ptr, addr form, addr env)
{
	Return(and_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_and(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_AND, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_and);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro cond (&rest clause) ...) */
static int function_cond(Execute ptr, addr form, addr env)
{
	Return(cond_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_cond(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_COND, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_cond);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator if */
static void defspecial_if(void)
{
	DefineSpecialOperator(COMMON_IF);
}


/* (defmacro or (&rest form) ...) */
static int function_or(Execute ptr, addr form, addr env)
{
	Return(or_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_or(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_OR, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_or);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro when (expr &body body) ...) -> object */
static int function_when(Execute ptr, addr form, addr env)
{
	Return(when_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_when(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WHEN, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_when);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro unless (expr &body body) ...) -> object */
static int function_unless(Execute ptr, addr form, addr env)
{
	Return(unless_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_unless(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_UNLESS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_unless);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro case key &rest args) -> result */
static int function_case(Execute ptr, addr form, addr env)
{
	Return(case_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_case(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_CASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_case);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro ecase (key &rest args) -> result */
static int function_ecase(Execute ptr, addr form, addr env)
{
	Return(ecase_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_ecase(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_ECASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_ecase);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro ccase (keyplace &rest args) -> result */
static int function_ccase(Execute ptr, addr form, addr env)
{
	Return(ccase_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_ccase(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_CCASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_ccase);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro typecase (key &rest clauses) ...) -> result */
static int function_typecase(Execute ptr, addr form, addr env)
{
	Return(typecase_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_typecase(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_TYPECASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_typecase);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro etypecase (key &rest clauses) ...) -> result */
static int function_etypecase(Execute ptr, addr form, addr env)
{
	Return(etypecase_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_etypecase(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_ETYPECASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_etypecase);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro ctypecase (key &rest clauses) ...) -> result */
static int function_ctypecase(Execute ptr, addr form, addr env)
{
	Return(ctypecase_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_ctypecase(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_CTYPECASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_ctypecase);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro multiple-value-bind (vars expr &body body) ...) -> t */
static int function_multiple_value_bind(Execute ptr, addr form, addr env)
{
	Return(multiple_value_bind_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_multiple_value_bind(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_MULTIPLE_VALUE_BIND, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_multiple_value_bind);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator multiple-value-call */
static void defspecial_multiple_value_call(void)
{
	DefineSpecialOperator(COMMON_MULTIPLE_VALUE_CALL);
}


/* (defmacro multiple-value-list (form) ...) */
static int function_multiple_value_list(Execute ptr, addr form, addr env)
{
	Return(multiple_value_list_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_multiple_value_list(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_MULTIPLE_VALUE_LIST, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_multiple_value_list);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator multiple-value-prog1 */
static void defspecial_multiple_value_prog1(void)
{
	DefineSpecialOperator(COMMON_MULTIPLE_VALUE_PROG1);
}


/* (defmacro multiple-value-setq (vars form) ...) */
static int function_multiple_value_setq(Execute ptr, addr form, addr env)
{
	Return(multiple_value_setq_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_multiple_value_setq(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_MULTIPLE_VALUE_SETQ, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_multiple_value_setq);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun values (&rest object) ...) -> * */
static int function_values(Execute ptr, addr rest)
{
	setvalues_list_control(ptr, rest);
	return 0;
}

static void defun_values(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VALUES, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_values);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeTable(&type, CompiledFunction);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (define-setf-expander values (&rest args &environment env) ...) */
static void define_setf_expander_values(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_VALUES, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_setf_values);
	SetSetfMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun values-list (list) ...) -> * */
static int function_values_list(Execute ptr, addr list)
{
	setvalues_list_control(ptr, list);
	return 0;
}

static void type_values_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	GetTypeTable(&values, Asterisk);
	typeargs_var1(&args, args);
	type_compiled_heap(args, values, ret);
}

static void defun_values_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VALUES_LIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_values_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_values_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defconstant multiple-value-limit FIXNUM_MAX) */
static void defconstant_multiple_values_limit(void)
{
	addr symbol, value;

	GetConst(COMMON_MULTIPLE_VALUES_LIMIT, &symbol);
	GetConst(FIXNUM_MAX, &value);
	defconstant_symbol(symbol, value);
}


/* (defmacro nth-value (index form) ...) -> object
 *   index  (integer 0 *)
 */
static int function_nth_value(Execute ptr, addr form, addr env)
{
	Return(nth_value_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_nth_value(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_NTH_VALUE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_nth_value);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro prog ([var] declaration* tagbody*) ...) -> result */
static int function_prog(Execute ptr, addr form, addr env)
{
	Return(prog_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_prog(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PROG, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_prog);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro prog* ([var] declaration* tagbody*) ...) -> result */
static int function_proga(Execute ptr, addr form, addr env)
{
	Return(proga_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_proga(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PROGA, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_proga);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro prog1 (form1 &body form) ...) -> form1 */
static int function_prog1(Execute ptr, addr form, addr env)
{
	Return(prog1_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_prog1(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PROG1, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_prog1);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro prog2 (form1 form2 &body form) ...) -> form2 */
static int function_prog2(Execute ptr, addr form, addr env)
{
	Return(prog2_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_prog2(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PROG2, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_prog2);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator progn */
static void defspecial_progn(void)
{
	DefineSpecialOperator(COMMON_PROGN);
}


/* (defmacro define-modify-macro (name args call &optional doc) ...) -> name */
static int function_define_modify_macro(Execute ptr, addr form, addr env)
{
	Return(define_modify_macro_common_(ptr->local, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_define_modify_macro(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_MODIFY_MACRO, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_define_modify_macro);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defsetf (access &rest args) -> access */
static int function_defsetf(Execute ptr, addr form, addr env)
{
	Return(defsetf_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defsetf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFSETF, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defsetf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro define-setf-expander (access &rest args) ...) -> access */
static int function_define_setf_expander(Execute ptr, addr form, addr env)
{
	Return(define_setf_expander_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_define_setf_expander(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_SETF_EXPANDER, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_define_setf_expander);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun get-setf-expansion (place &optional env) ...) -> (list list list t t) */
static int function_get_setf_expansion(Execute ptr, addr place, addr env)
{
	addr a, b, g, w, r;

	if (env == Unbound)
		env = Nil;
	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	setvalues_control(ptr, a, b, g, w, r, NULL);

	return 0;
}

static void type_get_setf_expansion(addr *ret)
{
	addr args, values, type, list, env;

	GetTypeTable(&type, T);
	GetTypeTable(&env, EnvironmentNull);
	GetTypeTable(&list, T);
	typeargs_var1opt1(&args, type, env);
	typevalues_values5(&values, list, list, list, type, type);
	type_compiled_heap(args, values, ret);
}

static void defun_get_setf_expansion(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_SETF_EXPANSION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_get_setf_expansion);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get_setf_expansion(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro setf (&rest pair) ...) -> t */
static int function_setf(Execute ptr, addr form, addr env)
{
	Return(setf_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_setf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_SETF, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_setf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro psetf (&rest args) ...) -> value */
static int function_psetf(Execute ptr, addr form, addr env)
{
	Return(psetf_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_psetf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PSETF, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_psetf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro shiftf (place+ newvalue) ...) -> oldvalue */
static int function_shiftf(Execute ptr, addr form, addr env)
{
	Return(shiftf_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_shiftf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_SHIFTF, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_shiftf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro rotatef (&rest place) ...) -> nil) */
static int function_rotatef(Execute ptr, addr form, addr env)
{
	Return(rotatef_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_rotatef(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_ROTATEF, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_rotatef);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/*
 *  function
 */
void init_common_data(void)
{
	SetPointerCall(defun,     var2dynamic,  apply);
	SetPointerCall(defmacro,  macro,        setf_apply);
	SetPointerCall(defmacro,  macro,        defun);
	SetPointerCall(defun,     var1,         fdefinition);
	SetPointerCall(defun,     var2,         setf_fdefinition);
	SetPointerCall(defun,     var1,         fboundp);
	SetPointerCall(defun,     var1,         fmakunbound);
	SetPointerCall(defun,     var1dynamic,  funcall);
	SetPointerCall(defun,     var1,         function_lambda_expression);
	SetPointerCall(defun,     var1,         functionp);
	SetPointerCall(defun,     var1,         compiled_function_p);
	SetPointerCall(defmacro,  macro,        defconstant);
	SetPointerCall(defmacro,  macro,        defparameter);
	SetPointerCall(defmacro,  macro,        defvar);
	SetPointerCall(defmacro,  macro,        destructuring_bind);
	SetPointerCall(defmacro,  macro,        psetq);
	SetPointerCall(defmacro,  macro,        return);
	SetPointerCall(defun,     var1,         not);
	SetPointerCall(defun,     var2,         eq);
	SetPointerCall(defun,     var2,         eql);
	SetPointerCall(defun,     var2,         equal);
	SetPointerCall(defun,     var2,         equalp);
	SetPointerCall(defun,     var1,         identity);
	SetPointerCall(defun,     dynamic,      lambda_complement);
	SetPointerCall(defun,     var1,         complement);
	SetPointerCall(defun,     any,          lambda_constantly);
	SetPointerCall(defun,     var1,         constantly);
	SetPointerCall(defun,     var1rest,     every);
	SetPointerCall(defun,     var1rest,     some);
	SetPointerCall(defun,     var1rest,     notany);
	SetPointerCall(defun,     var1rest,     notevery);
	SetPointerCall(defmacro,  macro,        and);
	SetPointerCall(defmacro,  macro,        cond);
	SetPointerCall(defmacro,  macro,        or);
	SetPointerCall(defmacro,  macro,        when);
	SetPointerCall(defmacro,  macro,        unless);
	SetPointerCall(defmacro,  macro,        case);
	SetPointerCall(defmacro,  macro,        ecase);
	SetPointerCall(defmacro,  macro,        ccase);
	SetPointerCall(defmacro,  macro,        typecase);
	SetPointerCall(defmacro,  macro,        etypecase);
	SetPointerCall(defmacro,  macro,        ctypecase);
	SetPointerCall(defmacro,  macro,        multiple_value_bind);
	SetPointerCall(defmacro,  macro,        multiple_value_list);
	SetPointerCall(defmacro,  macro,        multiple_value_setq);
	SetPointerCall(defun,     dynamic,      values);
	SetPointerCall(defmacro,  macro,        setf_values);
	SetPointerCall(defun,     var1,         values_list);
	SetPointerCall(defmacro,  macro,        nth_value);
	SetPointerCall(defmacro,  macro,        prog);
	SetPointerCall(defmacro,  macro,        proga);
	SetPointerCall(defmacro,  macro,        prog1);
	SetPointerCall(defmacro,  macro,        prog2);
	SetPointerCall(defmacro,  macro,        define_modify_macro);
	SetPointerCall(defmacro,  macro,        defsetf);
	SetPointerCall(defmacro,  macro,        define_setf_expander);
	SetPointerCall(defun,     var1opt1,     get_setf_expansion);
	SetPointerCall(defmacro,  macro,        setf);
	SetPointerCall(defmacro,  macro,        psetf);
	SetPointerCall(defmacro,  macro,        shiftf);
	SetPointerCall(defmacro,  macro,        rotatef);
}

void build_common_data(void)
{
	defun_apply();
	define_setf_expander_apply();
	defmacro_defun();
	defun_fdefinition();
	defun_setf_fdefinition();
	defun_fboundp();
	defun_fmakunbound();
	defspecial_flet();
	defspecial_labels();
	defspecial_macrolet();
	defun_funcall();
	defspecial_function();
	defun_function_lambda_expression();
	defun_functionp();
	defun_compiled_function_p();
	defconstant_call_arguments_limit();
	defconstant_lambda_list_keywords();
	defconstant_lambda_parameters_limit();
	defmacro_defconstant();
	defmacro_defparameter();
	defmacro_defvar();
	defmacro_destructuring_bind();
	defspecial_let();
	defspecial_leta();
	defspecial_progv();
	defspecial_setq();
	defmacro_psetq();
	defspecial_block();
	defspecial_catch();
	defspecial_go();
	defspecial_return_from();
	defmacro_return();
	defspecial_tagbody();
	defspecial_throw();
	defspecial_unwind_protect();
	defun_not();
	defun_eq();
	defun_eql();
	defun_equal();
	defun_equalp();
	defun_identity();
	defun_complement();
	defun_constantly();
	defun_every();
	defun_some();
	defun_notany();
	defun_notevery();
	defmacro_and();
	defmacro_cond();
	defspecial_if();
	defmacro_or();
	defmacro_when();
	defmacro_unless();
	defmacro_case();
	defmacro_ecase();
	defmacro_ccase();
	defmacro_typecase();
	defmacro_etypecase();
	defmacro_ctypecase();
	defmacro_multiple_value_bind();
	defspecial_multiple_value_call();
	defmacro_multiple_value_list();
	defspecial_multiple_value_prog1();
	defmacro_multiple_value_setq();
	defun_values();
	define_setf_expander_values();
	defun_values_list();
	defconstant_multiple_values_limit();
	defmacro_nth_value();
	defmacro_prog();
	defmacro_proga();
	defmacro_prog1();
	defmacro_prog2();
	defspecial_progn();
	defmacro_define_modify_macro();
	defmacro_defsetf();
	defmacro_define_setf_expander();
	defun_get_setf_expansion();
	defmacro_setf();
	defmacro_psetf();
	defmacro_shiftf();
	defmacro_rotatef();
}

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#ifdef __clang__
#pragma clang diagnostic pop
#endif
