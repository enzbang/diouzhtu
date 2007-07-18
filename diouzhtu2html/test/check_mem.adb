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

with Diouzhtu.To_HTML;
with Ada.Text_IO;
with Ada.Command_Line;

procedure Check_Mem is
   use Ada.Command_Line;
   use Ada.Text_IO;

   Wiki : constant Diouzhtu.Wiki_Information :=
            Diouzhtu.Initialize ("", "", ".");

begin

   Run_To_HTML :
   declare
      use Diouzhtu.To_HTML;

      Filename  : constant String   := Argument (1);
      Iteration : constant Positive := Positive'Value (Argument (2));

   begin

      for K in 1 .. Iteration loop
         Put_Line (To_HTML (Wiki, Filename));
      end loop;
   end Run_To_HTML;

exception
   when others => Put_Line ("Usage : " & Command_Name &
                              " [FILENAME] [Number of iteration]");
end Check_Mem;
