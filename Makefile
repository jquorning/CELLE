##
##
##

build:
	gprbuild -p cherry.gpr

clean:
	gprclean -q cherry.gpr

elim:
	gnatelim -v -Pcherry.gpr -main=cherry.adb >gnat.adc
	echo "Hand merge gnat.adc with src/gnat.adc"
