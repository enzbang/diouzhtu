##############################################################################
##                                Diouzhtu                                  ##
##                                                                          ##
##                        Copyright (C) 2007-2008                           ##
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

all::

# Root dir
MODE              = Release
JOBS              = 1
BUILD_DIR         = .build/$(shell echo $(MODE) | tr [[:upper:]] [[:lower:]])
GNATMAKE_OPTIONS  = -XPRJ_BUILD=$(MODE)
VALGRIND          =
TAR_DIR           = tar czf

# Install prefix
GNAT_ROOT         = $(dir $(shell which gnatls))..
ARGWIAD_ROOT      =
prefix	          =

ifeq (,$(prefix))
	prefix = $(shell cat $(BUILD_DIR)/gnat.root 2>/dev/null)
endif

ifeq (,$(ARGWIAD_ROOT))
	ARGWIAD_ROOT = $(shell echo $$ARGWIAD_ROOT)
endif

# Add GPR Library_Kind for AWS (force to shared)
GNATMAKE_OPTIONS += -XLIBRARY_TYPE=relocatable

GNAT              = gnat
GNATMAKE          = $(GNAT) make -p -j$(JOBS) $(GNATMAKE_OPTIONS)
GNATCLEAN         = $(GNAT) clean $(GNATMAKE_OPTIONS)
GNATCHECK         = $(GNAT) check $(GNATMAKE_OPTIONS) -rules -from=diouzhtu.check
RM                = rm -f
LN                = ln
CP                = cp -p
MKDIR             = mkdir -p

PROGRAMS =
PROGRAMS += bin/diouzhtu2html

LIBRARIES =
LIBRARIES += wiki_service
LIBRARIES += wiki_website

GPR =
GPR += diouzhtu2html/diouzhtu2html
GPR += gwiad_wiki_service/service/wiki_service
GPR += gwiad_wiki_service/website/wiki_website

BLD_GPR := $(addprefix bld-, $(GPR))
CLN_GPR := $(addprefix cln-, $(GPR))
CHK_GPR := $(addprefix chk-, $(GPR))

uname_M := $(shell sh -c 'uname -m 2>/dev/null || echo not')

VERSION_ALL = $(shell git describe 2>/dev/null)

ifeq (${OS},Windows_NT)
	LIBEXT = .dll
	EXEEXT = .exe
	DISTRIB_OS = win32-$(uname_M)
else
	LIBEXT = .so
	uname_S       := $(shell sh -c 'uname -s 2>/dev/null || echo not')
	uname_S_lower := $(shell sh -c \
		'uname -s 2>/dev/null \
			| tr [[:upper:]] [[:lower:]] || echo not')

	DISTRIB_OS  = $(uname_S_lower)-$(uname_M)
endif
DISTRIB = $(shell pwd)/diouzhtu-$(DISTRIB_OS)-$(VERSION_ALL)

bin/diouzhtu2html : bld-diouzhtu2html/diouzhtu2html
	@$(RM) $@
	-@$(LN) $(BUILD_DIR)/d2h/obj/diouzhtu2html $@

wiki_service : bld-gwiad_wiki_service/service/wiki_service
wiki_website : $(BUILD_DIR)/wweb/tsrc/wiki_website-template_defs.ads \
	bld-gwiad_wiki_service/website/wiki_website

all:: mkdirs $(PROGRAMS) $(LIBRARIES) prepare_install

$(BLD_GPR): bld-% :
	$(GNATMAKE) -P$*

$(CLN_GPR): cln-% :
	$(GNATCLEAN) -P$*

$(CHK_GPR): chk-% :
	$(GNATCHECK) -P$*

mkdirs:
	$(MKDIR) bin
	$(MKDIR) $(BUILD_DIR)/d
	$(MKDIR) $(BUILD_DIR)/d2h
	$(MKDIR) $(BUILD_DIR)/wi
	$(MKDIR) $(BUILD_DIR)/ws
	$(MKDIR) $(BUILD_DIR)/wweb/gen
	$(MKDIR) $(BUILD_DIR)/wweb/tsrc/
	$(MKDIR) $(BUILD_DIR)/slib

prepare_install:
	$(shell echo $(GNAT_ROOT) > $(BUILD_DIR)/gnat.root)

regtests:
	$(RM) -r $(BUILD_DIR)/test
	$(MKDIR) $(BUILD_DIR)/test
	@$(VALGRIND) bin/diouzhtu2html diouzhtu2html/test/example.txt \
		 $(BUILD_DIR)/test/example.html
	diff $(BUILD_DIR)/test/example.html \
		diouzhtu2html/test/example.html
	@echo "diouzhtu2html regression test OK !"

regtests_valgrind:
	$(MAKE) regtests VALGRIND=valgrind

gnatcheck: $(CHK_GPR)

$(BUILD_DIR)/wweb/tsrc/wiki_website-template_defs.ads :
	templates2ada -r -d gwiad_wiki_service/website/templates \
		-o $(BUILD_DIR)/wweb/gen/templates.cds \
		-t gwiad_wiki_service/website/templates/templates.tads
	gnat chop -wpq $(BUILD_DIR)/wweb/gen/templates.cds \
		$(BUILD_DIR)/wweb/tsrc/

clean: $(CLN_GPR)

clean_all: $(RM) -rf $(BUILD_DIR)

$(CLEAN_PROJECTS): clean-% :
	$(GNATCLEAN) $*

# Install directories

I_BIN	= $(prefix)/bin
I_INC	= $(prefix)/include/diouzhtu
I_INC_W = $(prefix)/include/wiki_interface
I_LIB	= $(prefix)/lib/
I_LIB_D	= $(prefix)/lib/diouzhtu
I_LIB_W	= $(prefix)/lib/wiki_interface
I_GPR	= $(prefix)/lib/gnat
I_DLIB  = $(prefix)/share/diouzhtu/dlib

PLUGIN_DISTRIB = gwiad_wiki_plugin

install_clean:
	$(RM) -fr $(I_INC)
	$(RM) -fr $(I_LIB_D)
	$(RM) -fr $(I_LIB_W)
	$(RM) -fr $(I_INC_W)
	$(RM) -fr $(I_DLIB)
	$(RM) -f $(I_LIB)/*diouzhtu$(LIBEXT)
	$(RM) -f $(I_LIB)/*wiki_interface$(LIBEXT)
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
	for library in `ls $(I_LIB_D)/*$(LIBEXT)`; do \
		$(LN) $$library $(I_BIN); \
	done
	$(CP) $(BUILD_DIR)/wi/lib/* $(I_LIB_W)
	for library in `ls $(I_LIB_W)/*$(LIBEXT)`; do \
		$(LN) $$library $(I_BIN); \
	done
	$(CP) diouzhtu/src/*.ad[sb] $(I_INC)
	$(CP) gwiad_wiki_service/interface/src/*.ads $(I_INC_W)
	$(CP) $(BUILD_DIR)/slib/services/*$(LIBEXT) $(I_DLIB)
	$(CP) $(BUILD_DIR)/slib/websites/*$(LIBEXT) $(I_DLIB)
	$(CP) config/projects/diouzhtu.gpr $(I_GPR)
	$(CP) config/projects/wiki_interface.gpr $(I_GPR)

install_gwiad_plugins:
	@if test ! "$(ARGWIAD_ROOT)"; then \
		echo "NO ARGWIAD_ROOT ?"; \
		exit 1; \
	fi
	$(MKDIR) $(ARGWIAD_ROOT)/bin/
	$(MKDIR) $(ARGWIAD_ROOT)/lib/services
	$(MKDIR) $(ARGWIAD_ROOT)/lib/websites
	$(MKDIR) $(ARGWIAD_ROOT)/plugins/wiki_website/example/templates/
	$(MKDIR) $(ARGWIAD_ROOT)/plugins/wiki_website/example/css
	$(MKDIR) $(ARGWIAD_ROOT)/plugins/wiki_website/example/js
	$(CP) $(BUILD_DIR)/d/lib/*$(LIBEXT) $(ARGWIAD_ROOT)/bin/
	$(CP) $(BUILD_DIR)/wi/lib/*wiki_interface$(LIBEXT) $(ARGWIAD_ROOT)/bin/
	$(CP) $(BUILD_DIR)/slib/services/*wiki_service$(LIBEXT) \
		$(ARGWIAD_ROOT)/lib/services
	$(CP) gwiad_wiki_service/website/config/config.ini \
		$(ARGWIAD_ROOT)/plugins/wiki_website/example/
	$(CP) gwiad_wiki_service/website/templates/*.thtml \
		$(ARGWIAD_ROOT)/plugins/wiki_website/example/templates/
	$(CP) gwiad_wiki_service/website/css/*.css \
		$(ARGWIAD_ROOT)/plugins/wiki_website/example/css/
	$(CP) external_libraries/highlight/*.js	\
		$(ARGWIAD_ROOT)/plugins/wiki_website/example/js/
	$(CP) -r external_libraries/highlight/languages	\
		$(ARGWIAD_ROOT)/plugins/wiki_website/example/js/
	$(CP) $(BUILD_DIR)/slib/websites/*wiki_website$(LIBEXT) \
		$(ARGWIAD_ROOT)/lib/websites

distrib-bin:
	$(MAKE) install_gwiad_plugins ARGWIAD_ROOT=$(DISTRIB)/dist
	(cd $(DISTRIB)/dist; $(TAR_DIR) ../dist.tgz .)
	$(RM) -r $(DISTRIB)/dist
	$(CP) gwiad_wiki_service/scripts/do-install.sh $(DISTRIB)
	$(TAR_DIR) $(shell basename $(DISTRIB)).tgz $(shell basename $(DISTRIB))
	$(RM) -r $(DISTRIB)

dist:
	git archive --prefix="$(shell basename $(PWD))/" --format=tar HEAD \
		 | gzip > "$(shell basename $(PWD))-$(VERSION_ALL)-src.tgz"

.PHONY: all install clean strip regtests gnatcheck
.PHONY: mkdirs prepare_install
.PHONY: install_dirs install_clean install
,PHONY: diouzhtu2html

.SILENT: mkdirs
