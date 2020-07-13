# Cherry parser generator

The *Lemon* parser generator from the *SQLite* project translated to Ada.

## Status
![Build](https://github.com/jquorning/Cherry/workflows/Build/badge.svg)
* Little C left
* Do not work

## Roadmap
1. Generate C parser code from parse.y
2. Generate Ada 83 parser code from translated parse.y
3. Generate Ada 95 parser code from translated parse.y

## Configuring
```sh
$ ed var/PROGRAM_NAME
$ ed var/PROGRAM_VERSION
$ make setup
```

## Building
```sh
$ gprbuild
```

or

```sh
$ make
```
