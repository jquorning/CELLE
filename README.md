Rewrite of the Lemon parser in Ada
==================================

The Lemon combined flex/yacc parser from the SQLite3 project is rewritten in Ada.

Now generates C parser code from parse.y.

Ada parser will follow soon to generate Ada parser from parse.y.

Cherry itself is written in Ada and C.

Building
--------
$ make

or

$ gprbuild cherry.gpr


