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
include mk.config

OPTIONS = MODE="$(MODE)" CP="$(CP)" MKDIR="$(MKDIR)" RM="$(RM)" LIB_KIND="$(LIB_KIND)"

# Modules support

MODULES = diouzhtu diouzhtu2html gwiad_wiki_service

MODULES_BUILD = ${MODULES:%=%_build}

MODULES_SETUP = ${MODULES:%=%_setup}

MODULES_CLEAN = ${MODULES:%=%_clean}

MODULES_CHECK = ${MODULES:%=%_check}

# Targets

all: $(MODULES_BUILD)

setup: $(MODULES_SETUP)

clean: $(MODULES_CLEAN)

check :$(MODULES_CHECK)

# Install directories

I_BIN	= $(INSTALL)/bin
I_INC	= $(INSTALL)/include/diouzhtu
I_LIB	= $(INSTALL)/lib/diouzhtu
I_GPR	= $(INSTALL)/lib/gnat

${MODULES_BUILD}:
	${MAKE} -C ${@:%_build=%} $(OPTIONS)

${MODULES_SETUP}:
	${MAKE} -C ${@:%_setup=%} setup $(OPTIONS)

${MODULES_CLEAN}:
	${MAKE} -C ${@:%_clean=%} clean $(OPTIONS)

${MODULES_CHECK}:
	${MAKE} -C ${@:%_check=%} check $(OPTIONS)

install_clean:
	$(RM) -fr $(I_INC)
	$(RM) -fr $(I_LIB)
	$(RM) -f $(I_GPR)/diouzhtu.gpr

install_dirs: install_clean
	$(MKDIR) $(I_BIN)
	$(MKDIR) $(I_INC)
	$(MKDIR) $(I_LIB)
	$(MKDIR) $(I_GPR)

install: install_dirs
	$(CP) diouzhtu/src/*.ad[sb] $(I_INC)
	$(CP) diouzhtu/lib/* $(I_LIB)
	$(CP) config/projects/diouzhtu.gpr $(I_GPR)

install_gwiad_service:
	$(RM) -f /opt/gwiad/lib/libwiki_service.so
	$(CP) gwiad_wiki_service/lib/libwiki_service.so /opt/gwiad/lib/

install_gwiad_website:
	$(RM) -f /opt/gwiad/lib/libwiki_website.so
	$(CP) gwiad_wiki_service/lib/libwiki_website.so /opt/gwiad/lib/
	$(MKDIR) /opt/gwiad/templates/wiki_website
	$(CP) gwiad_wiki_service/website/templates/wiki_website/*.thtml \
		/opt/gwiad/templates/wiki_website/