##
##
##

parse: parse.y lempar.c
	cherry parse.y

compile: parse
	gcc parse.c

clean:
	rm -f parse.h parse.c parse.out
