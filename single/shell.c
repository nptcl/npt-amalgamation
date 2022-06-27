/*
 *  npt -- ANSI Common Lisp Programming Language.
 *    https://github.com/nptcl/npt
 *    https://github.com/nptcl/npt-amalgamation
 *
 *  File: shell.c
 */
#include "lisp.h"


/************************************************************
 *  main.c
 ************************************************************/

#ifdef LISP_TERME_WINDOWS
#include "windows_main.h"

static int main_execute(struct lispargv *ptr)
{
	return windows_main(ptr);
}
#else
static int main_execute(struct lispargv *ptr)
{
	if (ptr->mode_help)
		return lisp_main_help(stdout);
	if (ptr->mode_version)
		return lisp_main_version(ptr, stdout);
	if (ptr->mode_degrade)
		return lisp_main_degrade(ptr);
	lisp_argv_init(ptr);
	lisp_argv_run(ptr);

	return lisp_code? 1: lisp_result;
}
#endif

#ifdef LISP_WINMAIN
#include <Windows.h>

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrev, LPSTR lpCmd, int nShow)
{
	int result;
	struct lispargv *ptr;

	lisp_init();
	ptr = lispargv_windows();
	if (ptr == NULL)
		return 1;
	result = main_execute(ptr);
	lispargv_free(ptr);
	lisp_free();

	return result;
}
#else
int main(int argc, char *argv[], char *env[])
{
	int result;
	struct lispargv *ptr;

	lisp_init();
	ptr = lispargv_main(argc, argv, env);
	if (ptr == NULL)
		return 1;
	result = main_execute(ptr);
	lispargv_free(ptr);
	lisp_free();

	return result;
}
#endif

