##
##
##

parse: parse.y lempar.adb
	cherry --language=Ada parse.y

clean:
	rm -f parse.ads parse.adb parse.out

compile:
	gnatmake parse.adb
