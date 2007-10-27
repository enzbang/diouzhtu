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

#  Installer for Gwiad plugin

if [ -z $ARGWIAD_ROOT ]; then
    echo usage: ARGWIAD_ROOT=/opt/gwiad install.sh all service website
    echo ARGWIAD_ROOT=$ARGWIAD_ROOT
    exit 1
fi

function interface_copy
{
    echo "Installing interface"
    cp libwiki_interface.so $ARGWIAD_ROOT/bin
}

function service_copy
{
    echo "Installing service"
    echo cp -f libdiouzhtu.so $ARGWIAD_ROOT/bin
    cp libdiouzhtu.so $ARGWIAD_ROOT/bin
    echo cp -f libwiki_service.so $ARGWIAD_ROOT/lib
    cp -f libwiki_service.so $ARGWIAD_ROOT/lib
}

function website_copy
{
    echo "Installing website"
    cp -f libwiki_website.so $ARGWIAD_ROOT/lib
}

function example_copy
{
    mkdir -p $ARGWIAD_ROOT/plugins/wiki_website/example/templates/
    mkdir -p $ARGWIAD_ROOT/plugins/wiki_website/example/css
    mkdir -p $ARGWIAD_ROOT/plugins/wiki_website/example/js
    cp example/config.ini \
	$ARGWIAD_ROOT/plugins/wiki_website/example/
    cp -r example/templates/ \
	$ARGWIAD_ROOT/plugins/wiki_website/example/
    cp -r example/css/ \
	$ARGWIAD_ROOT/plugins/wiki_website/example/
    cp -r example/js	\
	$ARGWIAD_ROOT/plugins/wiki_website/example/
}

if [ "$1" = "install_all" ]; then
    interface_copy;
    service_copy;
    website_copy;
elif [ "$1" = "install_service" ]; then
    service_copy;
elif [ "$1" = "install_website" ]; then
    website_copy;
elif [ "$1" = "install_example" ]; then
    example_copy;
else
    echo "******************************************"
    echo Welcome to the GWIAD Wiki plugin installer
    echo "******************************************"
    echo
    echo usage: install.sh install_all
    echo
    echo If you only want to install or reinstall service
    echo install.sh install_service
    echo
    echo or install.sh install_website to install... the website !
    echo
    echo You can also install the example website with
    echo install.sh install_example
    echo Or you can modify it and put the directory
    echo into $ARGWIAD_ROOT/plugins/wiki_website/
    echo
    echo ----------------------------------------------------
    echo Note that you will need to reload or restart argwiad
    echo You can use :
    echo   argwiadctl restart \(or argwiadctl reload\)
    echo ----------------------------------------------------
    echo
fi
