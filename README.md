# Npt-amalgamation

Npt-amalgamation is combining many source files of Npt-repository into a single code "lisp.c".  
Npt is a small Lisp Programming Language.
https://github.com/nptcl/npt


## Compile

ANSI-C
```
$ cc -lm lisp.c shell.c
```


FreeBSD
```
$ cc -lm -DLISP_FREEBSD lisp.c shell.c
```

Linux
```
$ cc -lm -DLISP_LINUX lisp.c shell.c
```

FreeBSD (full)
```
$ cc -O3 -lm -lpthread -ledit \
-DLISP_FREEBSD -DLISP_THREAD -DLISP_PROMPT_EDITLINE \
lisp.c shell.c
```

Linux (full)
```
$ cc -O3 -lm -lpthread -lreadline \
-DLISP_LINUX -DLISP_THREAD -DLISP_PROMPT_READLINE \
lisp.c shell.c
```


## License

[The Unlicense](LICENSE)


## Distribution

https://github.com/nptcl/npt-amalgamation

