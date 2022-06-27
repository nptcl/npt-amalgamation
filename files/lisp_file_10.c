/*
 *  npt -- ANSI Common Lisp Programming Language.
 *    https://github.com/nptcl/npt
 *    https://github.com/nptcl/npt-amalgamation
 *
 *  File: lisp_file_10.c
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


#include "lisp_file.h"


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

static void typecopy_delay(LocalRoot local, addr *ret, addr type)
{
	addr expr;

	GetArrayType(type, 0, &expr);
	GetArrayType(type, 1, &type);
	if (type != Nil)
		type_copy_alloc(local, &type, type);
	copylocal_object(local, &expr, expr);
	type2_alloc(local, LISPDECL_DELAY, expr, type, &type);
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

	TypeCopyTable[LISPDECL_DELAY] = typecopy_delay;
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
	TypeCopyTable[LISPDECL_PIPE_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_QUOTE] = typecopy_empty;
	TypeCopyTable[LISPDECL_BYTESPEC] = typecopy_empty;
	TypeCopyTable[LISPDECL_PRINT_DISPATCH] = typecopy_empty;
	TypeCopyTable[LISPDECL_PAPER] = typecopy_empty;
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

int execute_list_deftype_(Execute ptr, addr *ret, addr list, addr env)
{
	addr call, symbol;

	CheckType(list, LISPTYPE_CONS);
	GetCar(list, &symbol);
	CheckSymbol(symbol);
	getdeftype(symbol, &call);
	if (call == Nil)
		return Result(ret, NULL);

	return funcall1_control_(ptr, ret, call, list, env, NULL);
}

int execute_symbol_deftype_(Execute ptr, addr *ret, addr symbol, addr env)
{
	addr call;

	CheckSymbol(symbol);
	getdeftype(symbol, &call);
	if (call == Nil)
		return Result(ret, NULL);

	/* name -> (call `(name ,@args) env) */
	cons_heap(&symbol, symbol, Nil);
	return funcall1_control_(ptr, ret, call, symbol, env, NULL);
}


/*
 *  deftype
 */
int deftype_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr right, eval, name, args, decl, doc;

	/* (deftype . form) */
	Return_getcdr(form, &right);
	if (right == Nil)
		return fmte_("deftype form must have at least a name and body.", NULL);

	/* name */
	if (! consp_getcons(right, &name, &right))
		return fmte_("Invalid deftype form.", NULL);
	if (! symbolp(name))
		return fmte_("deftype name ~S must be a symbol.", name, NULL);
	if (right == Nil)
		return fmte_("deftype form must have at least a name and body.", NULL);

	/* args */
	if (! consp_getcons(right, &args, &right))
		return fmte_("Invalid deftype form.", NULL);
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
 *  type_delay.c
 ************************************************************/

static int get_delay_type_no_error(Execute ptr, addr pos, addr *ret)
{
	int notp;
	addr expr, type;

	CheckType(pos, LISPTYPE_TYPE);
	Check(RefLispDecl(pos) != LISPDECL_DELAY, "decl error");
	GetArrayType(pos, 1, &type);
	if (type != Nil)
		return Result(ret, type);

	/* parse-type */
	GetNotDecl(pos, &notp);
	GetArrayType(pos, 0, &expr);
	if (notp) {
		Return(parse_type_not_(ptr, &type, expr, Nil));
	}
	else {
		Return(parse_type_(ptr, &type, expr, Nil));
	}

	/* error check */
	if (type_delay_p(type))
		return Result(ret, Nil);

	/* Result */
	if (notp) {
		SetNotDecl(pos, 0);
	}
	SetArrayType(pos, 1, type);

	return Result(ret, type);
}

int get_delay_type_(Execute ptr, addr pos, addr *ret)
{
	addr check;

	Return(get_delay_type_no_error(ptr, pos, &check));
	if (check != Nil)
		return Result(ret, check);

	/* error */
	*ret = Nil;
	GetArrayType(pos, 0, &check);
	return call_type_error_va_(ptr, check, Nil, "Invalid type-spec ~S.", check, NULL);
}


/*
 *  check-delay-type
 */
typedef int (*type_delay_calltype)(Execute, addr, int, int *);
static type_delay_calltype TypeDelayCall[LISPDECL_SIZE];

static int check_delay_call_(Execute ptr, addr pos, int errorp, int *ret)
{
	LispDecl decl;
	type_delay_calltype call_;

	CheckType(pos, LISPTYPE_TYPE);
	GetLispDecl(pos, &decl);
	call_ = TypeDelayCall[decl];
	if (call_ == NULL)
		return Result(ret, 1);

	return (*call_)(ptr, pos, errorp ,ret);
}

static int check_delay_type_delay_(Execute ptr, addr pos, int errorp, int *ret)
{
	addr check;

	Return(get_delay_type_no_error(ptr, pos, &check));
	if (check != Nil)
		return Result(ret, 1);
	if (! errorp)
		return Result(ret, 0);

	/* type-error */
	*ret = 0;
	GetArrayType(pos, 0, &check);
	return call_type_error_va_(ptr, check, Nil, "Invalid type-spec ~S.", check, NULL);
}

static int check_delay_type_get1_(Execute ptr, addr pos, int errorp, int *ret)
{
	GetArrayType(pos, 0, &pos);
	return check_delay_call_(ptr, pos, errorp, ret);
}

static int check_delay_type_loop1_(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	addr vector;
	size_t size, i;

	GetArrayType(pos, 0, &vector);
	LenArrayA4(vector, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(vector, i, &pos);
		Return(check_delay_call_(ptr, pos, errorp, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int check_delay_type_cons_(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	addr x;

	/* car */
	GetArrayType(pos, 0, &x);
	Return(check_delay_call_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* cdr */
	GetArrayType(pos, 1, &x);
	Return(check_delay_call_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	return Result(ret, 1);
}

static int check_delay_type_loop_(Execute ptr, addr list, int errorp, int *ret)
{
	int check;
	addr x;

	while (list != Nil) {
		GetCons(list, &x, &list);
		Return(check_delay_call_(ptr, x, errorp, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int check_delay_type_values_(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	addr x;

	/* var */
	GetArrayType(pos, 0, &x);
	Return(check_delay_type_loop_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* opt */
	GetArrayType(pos, 1, &x);
	Return(check_delay_type_loop_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* rest */
	GetArrayType(pos, 2, &x);
	Return(check_delay_call_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	return Result(ret, 1);
}

static int check_delay_type_function_key_(Execute ptr, addr list, int errorp, int *ret)
{
	int check;
	addr x;

	if (list == T)
		return Result(ret, 1);
	while (list != Nil) {
		GetCons(list, &x, &list);
		GetCdr(x, &x);
		Return(check_delay_call_(ptr, x, errorp, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int check_delay_type_function_arguments_(Execute ptr,
		addr pos, int errorp, int *ret)
{
	int check;
	addr x;

	if (type_astert_p(pos))
		return Result(ret, 1);

	/* var */
	GetArrayA2(pos, 0, &x);
	Return(check_delay_type_loop_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* opt */
	GetArrayA2(pos, 1, &x);
	Return(check_delay_type_loop_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* rest */
	GetArrayA2(pos, 2, &x);
	if (x != Nil) {
		Return(check_delay_call_(ptr, x, errorp, &check));
		if (! check)
			return Result(ret, 0);
	}

	/* key */
	GetArrayA2(pos, 3, &x);
	Return(check_delay_type_function_key_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	return 0;
}

static int check_delay_type_function_(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	addr x;

	/* arguments */
	GetArrayType(pos, 0, &x);
	Return(check_delay_type_function_arguments_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* values */
	GetArrayType(pos, 1, &x);
	Return(check_delay_call_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	return Result(ret, 1);
}

int check_delay_type_(Execute ptr, addr pos, int *ret)
{
	return check_delay_call_(ptr, pos, 0, ret);
}

int execute_delay_type_(Execute ptr, addr pos)
{
	int ignore;
	return check_delay_call_(ptr, pos, 1, &ignore);
}

void init_type_delay(void)
{
	cleartype(TypeDelayCall);
	TypeDelayCall[LISPDECL_DELAY] = check_delay_type_delay_;
	TypeDelayCall[LISPDECL_OPTIMIZED] = check_delay_type_get1_;
	TypeDelayCall[LISPDECL_SUBTYPEP] = check_delay_type_get1_;
	TypeDelayCall[LISPDECL_AND] = check_delay_type_loop1_;
	TypeDelayCall[LISPDECL_OR] = check_delay_type_loop1_;
	TypeDelayCall[LISPDECL_NOT] = check_delay_type_get1_;
	TypeDelayCall[LISPDECL_CONS] = check_delay_type_cons_;
	TypeDelayCall[LISPDECL_VECTOR] = check_delay_type_get1_;
	TypeDelayCall[LISPDECL_ARRAY] = check_delay_type_get1_;
	TypeDelayCall[LISPDECL_SIMPLE_ARRAY] = check_delay_type_get1_;
	TypeDelayCall[LISPDECL_VALUES] = check_delay_type_values_;
	TypeDelayCall[LISPDECL_FUNCTION] = check_delay_type_function_;
	TypeDelayCall[LISPDECL_COMPILED_FUNCTION] = check_delay_type_function_;
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

void type_delay_heap(addr pos, addr *ret)
{
	type2_heap(LISPDECL_DELAY, pos, Nil, ret);
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

		case StreamType_Pipe:
			GetConst(SYSTEM_PIPE_STREAM, value);
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
	DefTypeName(LISPTYPE_PAPER, SYSTEM_PAPER);
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

static int type_object_dalay(addr *ret, addr pos)
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
	TypeObjectTable[LISPDECL_DELAY] = type_object_dalay;
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
	TypeObjectTable[LISPDECL_PIPE_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_QUOTE] = type_object_error;
	TypeObjectTable[LISPDECL_BYTESPEC] = type_object_name;
	TypeObjectTable[LISPDECL_PRINT_DISPATCH] = type_object_name;
	TypeObjectTable[LISPDECL_PAPER] = type_object_name;
	TypeObjectTable[LISPDECL_EVAL] = type_object_name;
}


/************************************************************
 *  type_parse.c
 ************************************************************/

static int localhold_parse_type(LocalHold hold,
		Execute ptr, addr *ret, addr pos, addr env)
{
	Return(parse_type_(ptr, ret, pos, env));
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
		Return(parse_type_(ptr, &pos, pos, env));
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
	Return(parse_type_(ptr, &pos, pos, env));
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
	return parse_type_(ptr, ret, left, env);
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
	Return(parse_type_(ptr, &one, one, env));
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
	Return(parse_type_(ptr, &one, one, env));
	cons_heap(&opt, one, opt);
	localhold_set(hold, 1, opt);
	goto opt_label;

rest_label:
	if (list == Nil)
		return fmte_("After &rest parameter must be have a typespec.", NULL);
	Return_getcons(list, &one, &list);
	if (one == const_opt || one == const_rest || one == const_key)
		return fmte_("After &rest parameter don't allow to be a &-symbol.", NULL);
	Return(parse_type_(ptr, &rest, one, env));
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
	Return(parse_type_(ptr, &type, type, env));
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
	Return(parse_type_(ptr, &var, var, env));
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
	Return(parse_type_(ptr, &var, var, env));
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
	Return(parse_type_(ptr, &rest, var, env));
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
		return parse_type_(ptr, ret, type, env);
	Return_getcons(type, &pos, &list);
	GetConst(COMMON_VALUES, &check);
	if (check != pos)
		return parse_type_(ptr, ret, type, env);
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
		Return(parse_type_(ptr, &first, first, env));
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
			Return(parse_type_(ptr, &first, first, env));
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
	return parse_type_(ptr, ret, left, env);
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
		Return(parse_type_(ptr, &first, first, env));
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
			Return(parse_type_(ptr, &first, first, env));
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
	return parse_type_(ptr, ret, left, env);
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
	return parse_type_(ptr, ret, left, env);
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
	return parse_type_(ptr, ret, left, env);
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
	if (! short_float_p(pos))
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
	return parse_type_(ptr, ret, left, env);
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
	Return(parse_type_(ptr, &first, first, env));
	Return(upgraded_complex_type_(ptr, env, first, &first));
	type1_heap(type, first, ret);
	return 0;

asterisk:
	return parse_type_(ptr, ret, left, env);
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
	Return(execute_list_deftype_(ptr, &check, pos, env));
	if (check)
		return parse_type_(ptr, ret, check, env);

	/* error */
	type_delay_heap(pos, ret);
	return 0;
}

static int parse_type_symbol(Execute ptr, addr *ret, addr pos, addr env)
{
	addr check;

	Return(find_symbol_type(ptr, &check, pos, env));
	if (check)
		return parse_type_(ptr, ret, check, env);

	/* error */
	type_delay_heap(pos, ret);
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
	return parse_type_(ptr, ret, x, Nil);  /* don't use env */
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
	return parse_type_(ptr, ret, x, Nil);  /* don't use env */
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

int parse_type_(Execute ptr, addr *ret, addr pos, addr env)
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

int parse_type_not_(Execute ptr, addr *ret, addr pos, addr env)
{
	Return(parse_type_(ptr, &pos, pos, env));
	type_copy_unsafe_heap(&pos, pos);
	type_revnotdecl(pos);
	return Result(ret, pos);
}

int parse_type_noaster_(Execute ptr, addr *ret, addr pos, addr env)
{
	addr aster;

	GetConst(COMMON_ASTERISK, &aster);
	if (pos == aster)
		return fmte_("Don't allow to use asterisk type.", NULL);

	return parse_type_(ptr, ret, pos, env);
}

void parse_type_unsafe(addr *ret, addr pos)
{
	if (parse_type_(Execute_Thread, ret, pos, Nil)) {
		Abort("parse-type error.");
	}
}


/* debug */
int parse_type_values_(Execute ptr, addr *ret, addr type, addr env)
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
	DefSymbolType(SYSTEM_PIPE_STREAM,          PipeStream          );
	DefSymbolType(SYSTEM_QUOTE,                Quote               );
	DefSymbolType(SYSTEM_BYTESPEC,             ByteSpec            );
	DefSymbolType(SYSTEM_PRINT_DISPATCH,       PrintDispatch       );
	DefSymbolType(SYSTEM_PAPER,                Paper               );
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
	DefTypeSymbol(PIPE_STREAM,          SYSTEM_PIPE_STREAM          );
	DefTypeSymbol(QUOTE,                SYSTEM_QUOTE                );
	DefTypeSymbol(BYTESPEC,             SYSTEM_BYTESPEC             );
	DefTypeSymbol(PRINT_DISPATCH,       SYSTEM_PRINT_DISPATCH       );
	DefTypeSymbol(PAPER,                SYSTEM_PAPER                );
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
	Return(execute_symbol_deftype_(ptr, &check, symbol, env));
	if (check)
		return parse_type_(ptr, ret, check, env);

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

static int typep_delay_(Execute ptr, addr value, addr type, int *ret)
{
	Return(get_delay_type_(ptr, type, &type));
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
	Return(funcall1_control_(ptr, &type, type, value, NULL));
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

static int typep_pipe_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = pipe_stream_p(value);
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

static int typep_paper_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_PAPER);
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

	TypeTypep[LISPDECL_DELAY] = typep_delay_;
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
	TypeTypep[LISPDECL_PIPE_STREAM] = typep_pipe_stream_;
	TypeTypep[LISPDECL_BYTESPEC] = typep_byte_;
	TypeTypep[LISPDECL_PRINT_DISPATCH] = typep_print_dispatch_;
	TypeTypep[LISPDECL_PAPER] = typep_paper_;
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

static int upgraded_array_unsigned(fixed value)
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

static int upgraded_array_signed(int sign, fixed value)
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
		if (value <= ((fixed)INT8_MAX) + 1UL) return 8;
		if (value <= ((fixed)INT16_MAX) + 1UL) return 16;
		if (value <= ((fixed)INT32_MAX) + 1UL) return 32;
#ifdef LISP_64BIT
		if (value <= ((fixed)INT64_MAX) + 1ULL) return 64;
#endif
	}
	return 0;
}

static enum ARRAY_TYPE upgraded_array_integer(addr type, int *size)
{
	int sign1, sign2, size1, size2;
	addr left1, left2, right1, right2;
	fixed value1, value2;

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

int upgraded_array_common_(Execute ptr, addr env, addr pos, addr *ret)
{
	int size;
	enum ARRAY_TYPE type;

	if (env == Unbound)
		env = Nil;
	Return(parse_type_(ptr, &pos, pos, env));
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

int upgraded_complex_common_(Execute ptr, addr env, addr pos, addr *ret)
{
	Return(parse_type_(ptr, &pos, pos, env));
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

		case StreamType_Pipe:
			GetTypeTable(ret, PipeStream);
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

/* bytespec */
static int type_value_bytespec_(addr *ret, addr value)
{
	GetTypeTable(ret, ByteSpec);
	return 0;
}

/* print-dispatch */
static int type_value_print_dispatch_(addr *ret, addr value)
{
	GetTypeTable(ret, PrintDispatch);
	return 0;
}

/* paper */
void type_value_paper(addr *ret, addr value)
{
	GetTypeTable(ret, Paper);
}

static int type_value_paper_(addr *ret, addr value)
{
	type_value_paper(ret, value);
	return 0;
}


/* value */
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
	TypeValueTable[LISPTYPE_PAPER] = type_value_paper_;
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
