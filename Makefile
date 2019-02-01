##
##  The author disclaims copyright to this source code.  In place of
##  a legal notice, here is a blessing:
##
##    May you do good and not evil.
##    May you find forgiveness for yourself and forgive others.
##    May you share freely, not taking more than you give.
##

build:
	gprbuild -p cherry.gpr

clean:
	gprclean -q cherry.gpr

elim:
	gnatelim -v -Pcherry.gpr -main=cherry.adb >gnat.adc
	echo "Hand merge gnat.adc with src/gnat.adc"
