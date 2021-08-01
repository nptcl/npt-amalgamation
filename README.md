# Npt-amalgamation

The npt-amalgamation is to combine the many source files of the npt-repository into one or a few files.  
Npt is an ANSI Common Lisp Programming Language.  
https://github.com/nptcl/npt


# 1. Compile [single]

FreeBSD
```
$ cd single
$ cc -O3 -o npt -DLISP_FREEBSD lisp.c shell.c -lm
```

Linux
```
$ cd single
$ cc -O3 -o npt -DLISP_LINUX lisp.c shell.c -lm
```


# 2. Compile [files]

FreeBSD
```
$ cd files
$ cc -O3 -o npt -DLISP_FREEBSD lisp_file_*.c shell.c -lm
```

Linux
```
$ cd files
$ cc -O3 -o npt -DLISP_LINUX lisp_file_*.c shell.c -lm
```


# 3. Documentation

[https://nptcl.github.io/npt/docs/md/index.html](https://nptcl.github.io/npt/docs/md/index.html)  
[https://nptcl.github.io/npt/docs/index.html](https://nptcl.github.io/npt/docs/index.html)


# 4. License

[The Unlicense](LICENSE)


# 5. Distribution

https://github.com/nptcl/npt-amalgamation
