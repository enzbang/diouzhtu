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

VERSION     = $(shell git describe --abbrev=0 2>/dev/null)
VERSION_ALL = $(shell git describe 2>/dev/null)

ifeq ($(OS),Windows_NT)
DISTRIB_OS = win32-$(shell uname -m)
else
DISTRIB_OS = $(shell uname -s | tr [[:upper:]] [[:lower:]])-$(shell uname -m)
endif

DISTRIB = $(shell pwd)/diouzhtu-$(DISTRIB_OS)-$(VERSION_ALL)

export LIBRARY_TYPE=relocatable

# Modules support

MODULES = diouzhtu diouzhtu2html gwiad_wiki_service

all: build

include mk.modules

mkinstall: force
ifneq ($(INSTALL), "")
# Write INSTALL target into mk.install (see install target)
	$(shell echo INSTALL = $(INSTALL) > mk.install)
endif

BUILD_DIR=".build/$(shell echo $(MODE) | tr [[:upper:]] [[:lower:]])"

# Targets

build: mkinstall build-default

setup: global_setup setup-default

clean: global_clean clean-default

check: check-default

global_setup:
	$(MKDIR) -p $(BUILD_DIR)/d
	$(MKDIR) -p $(BUILD_DIR)/d2h
	$(MKDIR) -p $(BUILD_DIR)/wi
	$(MKDIR) -p $(BUILD_DIR)/ws
	$(MKDIR) -p $(BUILD_DIR)/slib
	$(MKDIR) -p $(BUILD_DIR)/wweb

global_clean:
	$(RM) -fr $(BUILD_DIR)

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
GWIAD_SERVICES = $(GWIAD_ROOT)/lib/services
GWIAD_WEBSITES = $(GWIAD_ROOT)/lib/websites

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

-include mk.install

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
	$(MKDIR) $(GWIAD_ROOT)/bin/
	$(CP) $(BUILD_DIR)/wi/lib/*wiki_interface$(SOEXT) $(GWIAD_ROOT)/bin/

install_gwiad_service:
	$(RM) $(GWIAD_SERVICES)/libwiki_service.so
	$(MKDIR) $(GWIAD_SERVICES)
	$(CP) $(INSTALL)/lib/diouzhtu/*$(SOEXT) $(GWIAD_ROOT)/bin/
	$(CP) $(BUILD_DIR)/slib/services/*wiki_service$(SOEXT) $(GWIAD_SERVICES)

install_gwiad_website:
	$(RM) $(GWIAD_WEBSITES)/*wiki_website$(SOEXT)
	$(MKDIR) $(GWIAD_WEBSITES)
	$(MKDIR) $(GWIAD_ROOT)/plugins/wiki_website/example/templates/
	$(MKDIR) $(GWIAD_ROOT)/plugins/wiki_website/example/css
	$(MKDIR) $(GWIAD_ROOT)/plugins/wiki_website/example/js
	$(CP) gwiad_wiki_service/website/config/config.ini \
		$(GWIAD_ROOT)/plugins/wiki_website/example/
	$(CP) gwiad_wiki_service/website/templates/*.thtml \
		$(GWIAD_ROOT)/plugins/wiki_website/example/templates/
	$(CP) gwiad_wiki_service/website/css/*.css \
		$(GWIAD_ROOT)/plugins/wiki_website/example/css/
	$(CP) external_libraries/highlight/*.js	\
		$(GWIAD_ROOT)/plugins/wiki_website/example/js/
	$(CP) -r external_libraries/highlight/languages	\
		$(GWIAD_ROOT)/plugins/wiki_website/example/js/
	$(CP) $(BUILD_DIR)/slib/websites/*wiki_website$(SOEXT) $(GWIAD_WEBSITES)

install_gwiad_all: install_gwiad_interface install_gwiad_service \
	install_gwiad_website

install-gwiad-distrib: clean-distrib install_gwiad_all
	(cd $(DISTRIB)/dist; $(TAR_DIR) ../dist.tgz .)
	$(RM) -r $(DISTRIB)/dist
	$(CP) gwiad_wiki_service/scripts/do-install.sh $(DISTRIB)
	$(TAR_DIR) $(shell basename $(DISTRIB)).tgz $(shell basename $(DISTRIB))
	$(RM) -r $(DISTRIB)

install-distrib: GWIAD_ROOT=$(DISTRIB)/dist
install-distrib: install-gwiad-distrib

install-distrib-show-name:
	@echo $(DISTRIB)

clean-distrib:
ifeq ("$(DISTRIB)", "")
	$(error Empty DISTRIB var)
endif
	$(RM) -r $(DISTRIB)


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
