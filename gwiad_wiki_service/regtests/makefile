##############################################################################
##                                Diouzhtu                                  ##
##                                                                          ##
##                           Copyright (C) 2007                             ##
##                            Olivier Ramonat                               ##
##                                                                          ##
##  This library is free software; you can redistribute it and/or modify    ##
##  it under the terms of the GNU General Public License as published by    ##
##  the Free Software Foundation; either version 2 of the License, or (at   ##
##  your option) any later version.                                         ##
##                                                                          ##
##  This library is distributed in the hope that it will be useful, but     ##
##  WITHOUT ANY WARRANTY; without even the implied warranty of              ##
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       ##
##  General Public License for more details.                                ##
##                                                                          ##
##  You should have received a copy of the GNU General Public License       ##
##  along with this library; if not, write to the Free Software Foundation, ##
##  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       ##
##############################################################################

GENERAL_OPTIONS = CP="$(CP)" MKDIR="$(MKDIR)" RM="$(RM)" \
	GNATMAKE="$(GNATMAKE)" GNATCLEAN="$(GNATCLEAN)" EXEEXT="$(EXEEXT)" \
	DIFF="$(DIFF)"

OPTIONS = MODE="$(MODE)" $(GENERAL_OPTIONS)


all: check_mem

force:

clean:
	$(GNATCLEAN) -q -Pregtests
	rm -f alldifs run1 run2 check_mem.run1 check_mem.run2

check_mem: clean
	mkdir -p obj/
	make -C ../website $(OPTIONS)
	make -C ../service $(OPTIONS)
	cp ../lib/* .
	$(GNATMAKE) -Pregtests -XPRJ_BUILD="$(MODE)"
	echo -n "Running " check_mem
	./check_mem 2 > check_mem.res
	gnatmem 3 -i gmem.out ./check_mem$(EXEEXT) > check_mem.run1
	grep "   Final" check_mem.run1 > run1
	echo ",  run it again"
	./check_mem 30 >> check_mem.res
	gnatmem 3 -i gmem.out ./check_mem$(EXEEXT) > check_mem.run2
	grep "   Final" check_mem.run2 > run2
	-$(DIFF) run1 run2 1>> alldifs 2>> alldifs
	cat alldifs
