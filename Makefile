##
##  The author disclaims copyright to this source code.  In place of
##  a legal notice, here is a blessing:
##
##    May you do good and not evil.
##    May you find forgiveness for yourself and forgive others.
##    May you share freely, not taking more than you give.
##

all: setup build

setup:
	tools/create-setup-adb.sh

build:
	gprbuild -k cherry.gpr

clean:
	gprclean -q cherry.gpr


