------------------------------------------------------------------------------
--                               Diouzhtu                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
--                            Olivier Ramonat                               --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       --
------------------------------------------------------------------------------

with Ada.Directories;
with AWS.Status;

with Gwiad.Registry.Services.Register;

private package Wiki_Website.Config is

   type Wiki_Name is new String;

   Wiki_Config_Exception : exception;

   Wiki_Service_Name   : constant Gwiad.Registry.Services.Register.Service_Name
     := "wiki_service";
   --  Gwiad wiki Service name

   Plugin_Root : constant String :=
                   Ada.Directories.Compose ("plugins", "wiki_website");

   Wiki_Web_Image   : constant String := "img";
   Wiki_Web_Edit    : constant String := "edit";
   Wiki_Web_Preview : constant String := "preview";
   Wiki_Web_CSS     : constant String := "css";
   Wiki_Web_JS      : constant String := "js";

   procedure Add_Config (Name : Wiki_Name; Hostname : String);
   --  Adds a new config (a new wiki website)

   function Get_Wiki_Name (Request : AWS.Status.Data) return Wiki_Name;
   --  Returns wiki name from URI

   function Get_Directory (URI : String) return String;
   --  Get directory from URI

   function Get_Filename (URI : String) return String;
   --  Get filename from URI

   function Wiki_Host (Name : Wiki_Name) return String;
   --  Returns wiki host

   function Wiki_Root (Name : Wiki_Name) return String;
   --  Returns wiki root

   function Wiki_Text_Dir (Name : Wiki_Name) return String;
   --  Returns wiki text dir

   function Wiki_Image_Dir (Name : Wiki_Name) return String;
   --  Returns wiki image dir

   function Wiki_HTML_Dir (Name : Wiki_Name) return String;
   --  Returns wiki HTML Dir

   function Wiki_Data_Root (Name : Wiki_Name) return String;
   --  Returns wiki data root

   function Wiki_CSS_Root (Name : Wiki_Name) return String;
   --  Returns wiki css root

   function Wiki_JS_Root (Name : Wiki_Name) return String;
   --  Returns wiki JS root
end Wiki_Website.Config;
