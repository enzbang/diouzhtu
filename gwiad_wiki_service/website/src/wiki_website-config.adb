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
      Local_URI : constant String :=
                    URI (URI'First - 1 + Wiki_Web_Root'Length + 1 .. URI'Last);
      Counter   : Natural         := 0;
   begin
      for K in Local_URI'Range loop
         if Local_URI (K) = '/' then
            Counter := Counter + 1;
         end if;
         if Counter = 2 and then K < Local_URI'Last then
            return Local_URI (K + 1 .. Local_URI'Last);
         end if;
      end loop;
      return "";
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
