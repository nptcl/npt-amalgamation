# Npt-amalgamation

The npt-amalgamation is to combine the many source files of the npt-repository into one or a few files.  
Npt is an ANSI Common Lisp Programming Language.  
https://github.com/nptcl/npt


## 1. Compile [single]

FreeBSD
```
$ cd single
$ cc -DLISP_FREEBSD lisp.c shell.c -lm
```

Linux
```
$ cd single
$ cc -DLISP_LINUX lisp.c shell.c -lm
```

FreeBSD (full)
```
$ cd single
$ cc -O3 -DLISP_FREEBSD -DLISP_EDITLINE lisp.c shell.c -lm -ledit
```

Linux (full)
```
$ cd single
$ cc -O3 -DLISP_LINUX -DLISP_READLINE lisp.c shell.c -lm -lreadline
```


## 2. Compile [files]

FreeBSD
```
$ cd files
$ cc -DLISP_FREEBSD lisp_file_*.c shell.c -lm
```

Linux
```
$ cd files
$ cc -DLISP_LINUX lisp_file_*.c shell.c -lm
```

FreeBSD (full)
```
$ cd files
$ cc -O3 -DLISP_FREEBSD -DLISP_EDITLINE lisp_file_*.c shell.c -lm -ledit
```

Linux (full)
```
$ cd files
$ cc -O3 -DLISP_LINUX -DLISP_READLINE lisp_file_*.c shell.c -lm -lreadline
```


## 3. License

[The Unlicense](LICENSE)


## 4. Distribution

https://github.com/nptcl/npt-amalgamation

