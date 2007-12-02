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

GENERAL_OPTIONS = CP="$(CP)" MKDIR="$(MKDIR)" RM="$(RM)" \
	GNATMAKE="$(GNATMAKE)" GNATCLEAN="$(GNATCLEAN)" \
	GNATCHECK="$(GNATCHECK)" EXEEXT="$(EXEEXT)" LIB_KIND="$(LIB_KIND)" \
	DIFF="$(DIFF)"

OPTIONS = MODE="$(MODE)" $(GENERAL_OPTIONS)

ifeq ($(OS),Windows_NT)
SOEXT=.dll
EXEEXT=.exe
LN=cp -f
else
SOEXT=.so
EXEEXT=
LN=ln -sf
endif

# Modules support

MODULES = diouzhtu diouzhtu2html gwiad_wiki_service

MODULES_BUILD = ${MODULES:%=%_build}

MODULES_SETUP = ${MODULES:%=%_setup}

MODULES_CLEAN = ${MODULES:%=%_clean}

MODULES_CHECK = ${MODULES:%=%_check}

ifeq ("$(INSTALL)", "..")
$(error "Wrong install path : INSTALL='$(INSTALL)'")
else
ifeq ("$(INSTALL)", "")
$(error "Wrong install path : empty INSTALL var")
endif
endif

BUILD_DIR=".build/$(shell echo $(MODE) | tr [[:upper:]] [[:lower:]])"

# Targets

all: $(MODULES_BUILD)

setup: $(MODULES_SETUP)

clean: $(MODULES_CLEAN)

check :$(MODULES_CHECK)

# Install directories

I_BIN	= $(INSTALL)/bin
I_INC	= $(INSTALL)/include/diouzhtu
I_INC_W = $(INSTALL)/include/wiki_interface
I_LIB	= $(INSTALL)/lib/
I_LIB_D	= $(INSTALL)/lib/diouzhtu
I_LIB_W	= $(INSTALL)/lib/wiki_interface
I_GPR	= $(INSTALL)/lib/gnat
I_DLIB  = $(INSTALL)/share/diouzhtu/dlib

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
	$(RM) -fr $(I_LIB_D)
	$(RM) -fr $(I_LIB_W)
	$(RM) -fr $(I_INC_W)
	$(RM) -fr $(I_DLIB)
	$(RM) -f $(I_GPR)/diouzhtu.gpr

install_dirs: install_clean
	$(MKDIR) $(I_BIN)
	$(MKDIR) $(I_INC)
	$(MKDIR) $(I_INC_W)
	$(MKDIR) $(I_LIB_D)
	$(MKDIR) $(I_LIB_W)
	$(MKDIR) $(I_GPR)
	$(MKDIR) $(I_DLIB)

install: install_dirs
	$(CP) $(BUILD_DIR)/d/lib/* $(I_LIB_D)
	for library in `ls $(I_LIB_D)/*$(SOEXT)`; do \
		$(LN) $$library $(I_LIB); \
	done
	$(CP) $(BUILD_DIR)/wi/lib/* $(I_LIB_W)
	for library in `ls $(I_LIB_W)/*$(SOEXT)`; do \
		$(LN) $$library $(I_LIB); \
	done
	$(CP) diouzhtu/src/*.ad[sb] $(I_INC)
	$(CP) gwiad_wiki_service/interface/src/*.ads $(I_INC_W)
	$(CP) $(BUILD_DIR)/slib/services/*$(SOEXT) $(I_DLIB)
	$(CP) $(BUILD_DIR)/slib/websites/*$(SOEXT) $(I_DLIB)
	$(CP) config/projects/diouzhtu.gpr $(I_GPR)
	$(CP) config/projects/wiki_interface.gpr $(I_GPR)

install_gwiad_interface:
	$(CP) gwiad_wiki_service/interface/lib/*wiki_interface$(SOEXT) \
		$(GWIAD_DIR)/bin/

install_gwiad_service:
	$(RM) $(GWIAD_SERVICES)/libwiki_service.so
	$(CP) $(INSTALL)/lib/diouzhtu/*$(SOEXT) $(GWIAD_DIR)/bin/
	$(CP) gwiad_wiki_service/lib/*wiki_service$(SOEXT) $(GWIAD_SERVICES)

install_gwiad_website:
	$(RM) $(GWIAD_WEBSITES)/*wiki_website$(SOEXT)
	$(MKDIR) $(GWIAD_DIR)/plugins/wiki_website/example/templates/
	$(MKDIR) $(GWIAD_DIR)/plugins/wiki_website/example/css
	$(MKDIR) $(GWIAD_DIR)/plugins/wiki_website/example/js
	$(CP) gwiad_wiki_service/website/config/config.ini \
		$(GWIAD_DIR)/plugins/wiki_website/example/
	$(CP) gwiad_wiki_service/website/templates/*.thtml \
		$(GWIAD_DIR)/plugins/wiki_website/example/templates/
	$(CP) gwiad_wiki_service/website/css/*.css \
		$(GWIAD_DIR)/plugins/wiki_website/example/css/
	$(CP) external_libraries/highlight/*.js	\
		$(GWIAD_DIR)/plugins/wiki_website/example/js/
	$(CP) -r external_libraries/highlight/languages	\
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
	$(MKDIR) $(PLUGIN_DISTRIB)/example/templates/
	$(MKDIR) $(PLUGIN_DISTRIB)/example/css
	$(MKDIR) $(PLUGIN_DISTRIB)/example/js
	$(CP) gwiad_wiki_service/website/config/config.ini \
		$(PLUGIN_DISTRIB)/example/
	$(CP) gwiad_wiki_service/website/templates/*.thtml \
		$(PLUGIN_DISTRIB)/example/templates/
	$(CP) gwiad_wiki_service/website/css/*.css \
		$(PLUGIN_DISTRIB)/example/css/
	$(CP) external_libraries/highlight/*.js	\
		$(PLUGIN_DISTRIB)/example/js/
	$(CP) -r external_libraries/highlight/languages	\
		$(PLUGIN_DISTRIB)/example/js/
	$(TAR_DIR) $(PLUGIN_DISTRIB).tgz $(PLUGIN_DISTRIB)
	$(RM) -r $(PLUGIN_DISTRIB)

gcov_analyse:
	(cd diouzhtu/obj/; gcov -abfu ../src/*)
	(cd diouzhtu2html/obj/;gcov -abfu ../src/*)

regtests: force
	make -C diouzhtu2html regtests $(GENERAL_OPTIONS)
# 	make -C diouzhtu2html/test MODE="Profile" $(GENERAL_OPTIONS)
# 	rm -f diouzhtu2html/test/obj/*	# To avoid error in lcov_analyse ???
# 	rm -fr diouzhtu2html/obj/*
	make -C gwiad_wiki_service/regtests MODE="Profile" $(GENERAL_OPTIONS)
	make lcov_analyse

lcov_analyse: force
	sh analyse.sh

force:
