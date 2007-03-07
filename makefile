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

# Options
MODE=Debug
CP=cp -p
MKDIR=mkdir -p
RM=rm -f

# Modules support

MODULES = diouzhtu2html diouzhtu

MODULES_BUILD = ${MODULES:%=%_build}

MODULES_CLEAN = ${MODULES:%=%_clean}

# Targets

all: $(MODULES_BUILD)

clean: $(MODULES_CLEAN)

${MODULES_BUILD}:
	${MAKE} -C ${@:%_build=%} $(OPTIONS)

${MODULES_CLEAN}:
	${MAKE} -C ${@:%_clean=%} clean $(OPTIONS)
