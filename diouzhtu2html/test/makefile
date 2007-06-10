GNATMAKE = gnatmake
GNATCLEAN = gnatclean
DIFF = diff
EXEEXT =

all: check check_mem

force:

check:
	-$(RM) example_gen.html
	../bin/diouzhtu2html example.txt example_gen.html
	diff example_gen.html example.html
	../bin/diouzhtu2html example.txt example_gen_all.html -create-all-page example_header.html example_footer.html
	cat example_gen_all.html


check_mem: force
	$(GNATMAKE) -Pcheck_mem
	echo -n "Running " check_mem
	./check_mem example.txt 2 > check_mem.res
	gnatmem 3 -i gmem.out ./check_mem$(EXEEXT) > check_mem.run1
	grep "   Final" check_mem.run1 > run1
	echo ",  run it again"
	./check_mem example.txt 30 >> check_mem.res
	gnatmem 3 -i gmem.out ./check_mem$(EXEEXT) > check_mem.run2
	grep "   Final" check_mem.run2 > run2
	$(GNATCLEAN) -q -Pcheck_mem
	-$(DIFF) run1 run2 1>> alldifs 2>> alldifs
