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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Diouzhtu.Block_Level;

package body Diouzhtu.To_HTML is

   use Ada;
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   -------------
   -- To_HTML --
   -------------

   function To_HTML (Filename : String) return String is
      Diouzhtu_File : File_Type;
      Content       : Unbounded_String := Null_Unbounded_String;
      Result        : Unbounded_String := Null_Unbounded_String;
   begin

      Open (File => Diouzhtu_File,
            Mode => In_File,
            Name => Filename);

      while not End_Of_File (Diouzhtu_File) loop
         declare
            Line : constant String := Get_Line (Diouzhtu_File);
         begin

            if Line /= "" then
               if Content /= Null_Unbounded_String then
                  Append (Content, ASCII.Lf);
               end if;
               Append (Content, Line);
            else
               if Content /= Null_Unbounded_String then
                  Append (Result,
                          Block_Processing (To_String (Content)));
                  Content := Null_Unbounded_String;
               end if;
            end if;
         end;
      end loop;

      if Content /= Null_Unbounded_String then
         Append (Result, Block_Processing (To_String (Content)));
      end if;

      return To_String (Result);

   end To_HTML;

begin
   Diouzhtu.Block_Level.Register;
end Diouzhtu.To_HTML;
