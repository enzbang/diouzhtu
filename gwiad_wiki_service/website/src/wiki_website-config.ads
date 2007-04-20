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

private package Wiki_Website.Config is

   Wiki_Service_Name   : constant String := "wiki_service";
   --  Gwiad wiki Service name

   Wiki_Web_Edit    : constant String := "edit";
   Wiki_Web_Preview : constant String := "preview";
   Text_Dir         : constant String := "text";
   HTML_Dir         : constant String := "html";

   Wiki_Root        : constant String := "data/wiki_website";
   Wiki_Web_Root    : constant String := "/wiki";

   function Get_Filename (URI : in String) return String;
   --  Get filename from URI

   function Wiki_Text_Dir return String;
   --  Returns wiki text dir

   function Wiki_HTML_Dir return String;
   --  Returns wiki HTML Dir

end Wiki_Website.Config;
