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

all:
	gnat make -XPRJ_BUILD=$(MODE) -Pdiouzhtu2html
	gnat make -XPRJ_BUILD=$(MODE) -Pdiouzhtu2html

clean:
	-$(RM) -r gen src/v2p-template_defs*
	-gnat clean -r -Pdiouzhtu2html

check:
	-$(RM) tmp/example_gen.html
	./bin/diouzhtu2html test/example.txt test/example_gen.html
	cat test/example_gen.html
	diff test/example_gen.html test/example.html
	./bin/diouzhtu2html test/example.txt test/example_gen_all.html -create-all-page test/example_header.html test/example_footer.html
	cat test/example_gen_all.html
