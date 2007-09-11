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
	mkdir -p obj bin
	gnat make -XPRJ_BUILD=$(MODE) -XPRJ_LIB_KIND=$(LIB_KIND) -Pdiouzhtu2html
	gnat make -XPRJ_BUILD=$(MODE) -XPRJ_LIB_KIND=$(LIB_KIND) -Pdiouzhtu2html

clean:
	-$(RM) -r gen src/v2p-template_defs*
	-gnat clean -r -Pdiouzhtu2html

check:
	gnat check -dd -Pdiouzhtu2html -rules -from=../diouzhtu.check

regtests:
	make -C test

setup: