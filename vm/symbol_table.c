#include "../include/common.h"

struct symbol_table *
st_create()
{
	struct symbol_table *rc = malloc(sizeof(struct symbol_table));
	if(!rc)
		return NULL;
	rc->next = NULL;
	rc->table = ht_create();
	return rc;
}

struct symbol_table *
st_destory(struct symbol_table *stable)
{
	ht_destory(stable->table);
	free(stable);
}

static void
_st_dump(const struct symbol_table *stable, int depth)
{
	if(!stable)
		return;

	printf("**symbol table scope %d**\n",depth);
	ht_dump(stable->table);
	_st_dump(stable->next, depth+1);
}

struct s_exp *
st_dump(const struct symbol_table *stable)
{
	_st_dump(stable,0);
	return sexp_undef;
}

struct s_exp *
st_insert(struct symbol_table *stable, const char *key, struct s_exp *data)
{
	return ht_insert(stable->table, key, data);
}

/*
 * find data from symbol table.
 * return data if success, sexp_undef otherwise.
 */
struct s_exp *
st_find(const struct symbol_table *stable, const char *key)
{
	struct s_exp *e;

	if(!stable)
		return sexp_undef;

	e = ht_find(stable->table, key);
	if(e == sexp_undef)
		return st_find(stable->next, key);
	else
		return e;
}

void
st_init(struct symbol_table *stable)
{
	st_insert(stable, "quote", special2sexp("quote"));
	st_insert(stable, "if", special2sexp("if"));
	st_insert(stable, "define", special2sexp("define"));
	st_insert(stable, "symbols", special2sexp("symbols"));
	st_insert(stable, "lambda", special2sexp("lambda"));
	st_insert(stable, "begin", special2sexp("begin"));

	st_insert(stable, "+", builtin2sexp("+"));
	st_insert(stable, "-", builtin2sexp("-"));
	st_insert(stable, "*", builtin2sexp("*"));
	st_insert(stable, "/", builtin2sexp("/"));
	st_insert(stable, "modulo", builtin2sexp("modulo"));

	st_insert(stable, "cons", builtin2sexp("cons"));
	st_insert(stable, "append", builtin2sexp("append"));
	st_insert(stable, "car", builtin2sexp("car"));
	st_insert(stable, "cdr", builtin2sexp("cdr"));
	st_insert(stable, "list", builtin2sexp("list"));
	st_insert(stable, "eval", builtin2sexp("eval"));
	st_insert(stable, "display", builtin2sexp("display"));
	st_insert(stable, "newline", builtin2sexp("newline"));

	st_insert(stable, "eq?", builtin2sexp("eq?"));
	st_insert(stable, "equal?", builtin2sexp("equal?"));
	st_insert(stable, "atom?", builtin2sexp("atom?"));
	st_insert(stable, "nil?", builtin2sexp("nil?"));
	st_insert(stable, "null?", builtin2sexp("null?"));

	st_insert(stable, "or", builtin2sexp("or"));
	st_insert(stable, "and", builtin2sexp("and"));
	st_insert(stable, "not", builtin2sexp("not"));
	st_insert(stable, "=", builtin2sexp("="));
	st_insert(stable, ">", builtin2sexp(">"));
	st_insert(stable, "<", builtin2sexp("<"));
	st_insert(stable, "<=", builtin2sexp("<="));
	st_insert(stable, ">=", builtin2sexp(">="));

	st_insert(stable, "list->string",builtin2sexp("list->string"));
}
