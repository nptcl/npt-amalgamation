# Npt-amalgamation

Npt-amalgamation is combining many source files of Npt-repository into a single code "lisp.c".  
Npt is an ANSI Common Lisp Programming Language.
https://github.com/nptcl/npt


## Compile

FreeBSD
```
$ cc -DLISP_FREEBSD lisp.c shell.c -lm
```

Linux
```
$ cc -DLISP_LINUX lisp.c shell.c -lm
```

FreeBSD (full)
```
$ cc -O3 -DLISP_FREEBSD -DLISP_EDITLINE lisp.c shell.c -lm -ledit
```

Linux (full)
```
$ cc -O3 -DLISP_LINUX -DLISP_READLINE lisp.c shell.c -lm -lreadline
```


## License

[The Unlicense](LICENSE)


## Distribution

https://github.com/nptcl/npt-amalgamation

