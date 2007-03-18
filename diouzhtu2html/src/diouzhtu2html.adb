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

procedure Diouzhtu2Html is
   use Diouzhtu.To_HTML;
   use Ada.Command_Line;
   use Ada.Text_IO;
begin

   if Argument_Count = 1 then
      Put_Line (To_HTML (Argument (1)));
   elsif Argument_Count >= 2 then
      declare
         File : File_Type;
      begin
         Create (File, Out_File, Argument (2));
         if Argument_Count = 5 and then Argument (3) = "-create-all-page" then
            declare
               Header_File : File_Type;
            begin
               Open (Header_File, In_File, Argument (4));
               while not End_Of_File (Header_File) loop
                  Put (File, Get_Line (Header_File));
               end loop;
            end;
         end if;
         Put (File, To_HTML (Argument (1)));
         if Argument_Count = 5 then
            declare
               Footer_File : File_Type;
            begin
               Open (Footer_File, In_File, Argument (5));
               while not End_Of_File (Footer_File) loop
                  Put (File, Get_Line (Footer_File));
               end loop;
            end;
            Close (File);
         end if;
      end;
   else
      Put_Line ("Usage : " & Command_Name & " [FILENAME]");
      Set_Exit_Status (Failure);
   end if;

--  exception
--     when others => Put_Line ("Usage : " & Command_Name & " [FILENAME]");
end Diouzhtu2Html;
