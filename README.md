# ley/yacc parser generator

Rewrite of the *Lemon* parser from theh *SQLite* project in Ada

The Lemon combined flex/yacc parser generator is rewritten in Ada 2005.

## Status
1. Partly rewritten from lemon.c
1. Compiling
1. Not working

## Roadmap
1. Generate C parser code from parse.y
1. Replace lemon.c with Ada program
1. Generate C89 parser code with better structure (include yy.h)
1. Generate Ada parser code from parse.y
1. Split into library and application

## Configuring
```sh
ed var/PROGRAM_NAME
ed var/PROGRAM_VERSION
make setup
```

## Building
```sh
make
```

or

```sh
gprbuild
```

