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

package body Wiki_Website.Config is

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (URI : in String) return String is
      Name : constant String :=
               URI (URI'First - 1 + Wiki_Web_Root'Length + 2 .. URI'Last);
   begin

      if Name = Wiki_Web_Edit or else Name = Wiki_Web_Preview then
         return "";
      end if;

      if Name'Length > Wiki_Web_Edit'Length and then
        Name (Name'First ..
                Name'First + Wiki_Web_Edit'Length - 1) = Wiki_Web_Edit
      then
         return Name (Name'First + Wiki_Web_Edit'Length + 1 .. Name'Last);
      end if;

      if Name'Length > Wiki_Web_Preview'Length and then
        Name (Name'First ..
                Name'First + Wiki_Web_Preview'Length - 1) = Wiki_Web_Preview
      then
         return Name (Name'First + Wiki_Web_Preview'Length + 1 .. Name'Last);
      end if;

      return Name;
   end Get_Filename;

   -------------------
   -- Wiki_HTML_Dir --
   -------------------

   function Wiki_HTML_Dir return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Wiki_Root,
         Name                 => HTML_Dir);
   end Wiki_HTML_Dir;

   -------------------
   -- Wiki_Text_Dir --
   -------------------

   function Wiki_Text_Dir return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Wiki_Root,
         Name                 => Text_Dir);
   end Wiki_Text_Dir;
end Wiki_Website.Config;
