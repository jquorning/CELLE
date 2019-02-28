# Rewrite of the Lemon parser in Ada

The Lemon combined flex/yacc parser from the SQLite3 project is rewritten in Ada 2005.

Cherry itself is written in C and Ada. C will be more and more replaced by Ada 2005.

## Status
1. Partly rewritten from lemon.c
1. Compiling
1. Not working

## Roadmap
1. Generate C parser code from parse.y
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

