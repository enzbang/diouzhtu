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

# Installer for Gwiad plugin
# Use: install.sh

if [ -z $GWIAD_DIR ]; then
    echo usage: GWIAD_DIR=/opt/gwiad install.sh all service website
    echo $GWIAD_DIR
    exit 1
fi

if [ -z $GWIAD_HOST ]; then
    echo Set \$GWIAD_HOST to localhost:8080
    GWIAD_HOST=http://localhost:8080
fi

if [ -z $GWIAD_UNREGISTER_SCRIPT ]; then
    GWIAD_UNREGISTER_SCRIPT=$GWIAD_DIR/scripts/unregister
    echo Set \$GWIAD_UNREGISTER_SCRIPT to $GWIAD_UNREGISTER_SCRIPT
    if [ ! -x $GWIAD_UNREGISTER_SCRIPT ]; then
	echo Aborting... Gwiad unregister script not found !
	echo Please set \$GWIAD_UNREGISTER_SCRIPT
	exit 1
    fi
fi

function service_unregister
{
    echo Unregister service
    $GWIAD_UNREGISTER_SCRIPT $GWIAD_HOST service wiki_service
}

function website_unregister
{
    echo Unregister website
    $GWIAD_UNREGISTER_SCRIPT $GWIAD_HOST website $GWIAD_DIR/lib/libwiki_website.so
}

function interface_copy
{
    echo "Installing interface"
    cp libwiki_interface.so $GWIAD_DIR/bin
}

function service_copy
{
    echo "Installing service"
    echo cp libdiouzhtu.so $GWIAD_DIR/bin
    cp libdiouzhtu.so $GWIAD_DIR/bin
    echo cp libwiki_service.so $GWIAD_DIR/lib
    cp libwiki_service.so $GWIAD_DIR/lib
}

function website_copy
{
    echo "Installing website"
    cp libwiki_website.so $GWIAD_DIR/lib

}

if [ "$1" = "install_all" ]; then
    service_unregister;
    website_unregister;
    interface_copy;
    service_copy;
    website_copy;
elif [ "$1" = "install_service" ]; then
    service_unregister;
    service_copy;
elif [ "$1" = "install_website" ]; then
    website_unregister;
    website_copy;
else
    echo Welcome to the GWIAD Wiki plugin installer
    echo 
    echo usage: install.sh install_all
    echo 
    echo If you only want to install or reinstall service
    echo install.sh install_service
    echo 
    echo or install.sh install_website to install... the website !
    echo 
fi


