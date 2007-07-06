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

OPTIONS = MODE="$(MODE)" CP="$(CP)" MKDIR="$(MKDIR)" RM="$(RM)" \
	LIB_KIND="$(LIB_KIND)"

ifeq ($(OS),Windows_NT)
SOEXT=.dll
EXEEXT=.exe
else
SOEXT=.so
EXEEXT=
endif

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
I_INC_W = $(INSTALL)/include/wiki_interface
I_LIB	= $(INSTALL)/lib/diouzhtu
I_LIB_W	= $(INSTALL)/lib/wiki_interface
I_GPR	= $(INSTALL)/lib/gnat

PLUGIN_DISTRIB = gwiad_wiki_plugin
GWIAD_SERVICES = $(GWIAD_DIR)/lib/services
GWIAD_WEBSITES = $(GWIAD_DIR)/lib/websites

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
	$(MKDIR) $(I_INC_W)
	$(MKDIR) $(I_LIB)
	$(MKDIR) $(I_LIB_W)
	$(MKDIR) $(I_GPR)

install: install_dirs
	$(CP) diouzhtu/src/*.ad[sb] $(I_INC)
	$(CP) gwiad_wiki_service/interface/src/*.ads $(I_INC_W)
	$(CP) diouzhtu/lib/* $(I_LIB)
	$(CP) gwiad_wiki_service/interface/lib/* $(I_LIB_W)
	$(CP) config/projects/diouzhtu.gpr $(I_GPR)
	$(CP) config/projects/wiki_interface.gpr $(I_GPR)

install_gwiad_interface:
	$(CP) gwiad_wiki_service/interface/lib/*wiki_interface$(SOEXT) \
		$(GWIAD_DIR)/bin/


install_gwiad_service:
	-$(GWIAD_UNREGISTER_SCRIPT) 127.0.0.1:8080 service wiki_service
	$(RM) -f $(GWIAD_DIR)/lib/libwiki_service.so
	$(CP) $(INSTALL)/lib/diouzhtu/*$(SOEXT) $(GWIAD_DIR)/bin/
	$(CP) gwiad_wiki_service/lib/*wiki_service$(SOEXT) $(GWIAD_SERVICES)

install_gwiad_website:
	-$(GWIAD_UNREGISTER_SCRIPT) 127.0.0.1:8080 website \
		$(GWIAD_DIR)/lib/*wiki_website$(SOEXT)
	$(RM) -f $(GWIAD_DIR)/lib/*wiki_website$(SOEXT)
	$(MKDIR) $(GWIAD_DIR)/plugins/wiki_website/example/templates/
	$(MKDIR) $(GWIAD_DIR)/plugins/wiki_website/example/css
	$(MKDIR) $(GWIAD_DIR)/plugins/wiki_website/example/js
	$(CP) gwiad_wiki_service/website/config/config.ini \
		$(GWIAD_DIR)/plugins/wiki_website/example/
	$(CP) gwiad_wiki_service/website/templates/*.thtml \
		$(GWIAD_DIR)/plugins/wiki_website/example/templates/
	$(CP) gwiad_wiki_service/website/templates/wiki_website/css/*.css \
		$(GWIAD_DIR)/plugins/wiki_website/example/css/
	$(CP) external_libraries/highlight/*.js	\
		$(GWIAD_DIR)/plugins/wiki_website/example/js/
	$(CP) gwiad_wiki_service/lib/*wiki_website$(SOEXT) $(GWIAD_WEBSITES)

install_gwiad_all: install_gwiad_interface install_gwiad_service \
	install_gwiad_website

gwiad_plugin_distrib:
	$(MKDIR) -p $(PLUGIN_DISTRIB)
	$(CP) gwiad_wiki_service/interface/lib/*wiki_interface$(SOEXT) \
		$(PLUGIN_DISTRIB)/
	$(CP) gwiad_wiki_service/lib/*wiki_website$(SOEXT) $(PLUGIN_DISTRIB)/
	$(CP) gwiad_wiki_service/lib/*wiki_service$(SOEXT) $(PLUGIN_DISTRIB)/
	$(CP) gwiad_wiki_service/plugin/install.sh $(PLUGIN_DISTRIB)/
	$(CP) $(INSTALL)/lib/diouzhtu/*$(SOEXT) $(PLUGIN_DISTRIB)/
	$(TAR_DIR) $(PLUGIN_DISTRIB).tgz $(PLUGIN_DISTRIB)
	$(RM) -r $(PLUGIN_DISTRIB)

gcov_analyse:
	(cd diouzhtu/obj/; gcov -abfu ../src/*)
	(cd diouzhtu2html/obj/;gcov -abfu ../src/*)