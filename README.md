# Rewrite of the Lemon parser in Ada

The Lemon combined flex/yacc parser from the SQLite3 project is rewritten in Ada 2005.

Now generates C parser code from parse.y.

Ada parser will follow soon to generate Ada parser from parse.y.

Cherry itself is written in C and Ada. C will be more and more replaced by Ada 2005.

## Configuring
**$** cd source-var

**$** edit PROGRAM_VERSION

**$** ./create-setup-ads.sh

## Building
**$** make

or

**$** gprbuild cherry.gpr


