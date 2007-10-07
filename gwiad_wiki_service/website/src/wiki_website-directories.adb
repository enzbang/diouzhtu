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
with Ada.Strings.Fixed;

package body Wiki_Website.Directories is

   use Ada;

   ------------------------
   -- Get_First_Filename --
   ------------------------

   function Get_First_Filename (Dir : in String) return String is
      use Ada.Directories;
      S : Search_Type;
      D : Directory_Entry_Type;
   begin

      Start_Search (Search    => S,
                    Directory => Dir,
                    Pattern   => "*",
                    Filter    => Filter_Type'(Ordinary_File => True,
                                              Directory     => False,
                                              Special_File  => False));

      if More_Entries (S) then
         Get_Next_Entry (S, D);
         In_Current_Directory :
         declare
            Full_Name : constant String := Ada.Directories.Full_Name (D);
         begin
            return Strings.Fixed.Delete (Source  => Full_Name,
                                         From    => Full_Name'First,
                                         Through => Full_Name'First +
                                           Current_Directory'Length);
         end In_Current_Directory;
      end if;

      --  Not file in directory. Search in subdirectories

      Start_Search (Search    => S,
                    Directory => Dir,
                    Pattern   => "*",
                    Filter    => Filter_Type'(Ordinary_File => False,
                                              Directory     => True,
                                              Special_File  => False));
      while More_Entries (S) loop
         Get_Next_Entry (S, D);
         Search_In_Subdirs :
         declare
            SN : constant String := Simple_Name (D);
         begin
            if SN (SN'First) /= '.' then
               Check_If_Non_Empty :
               declare
                  First_Filename : constant String
                    := Get_First_Filename (Ada.Directories.Full_Name (D));
               begin
                  if First_Filename /= "" then
                     return First_Filename;
                  end if;
               end Check_If_Non_Empty;
            end if;
         end Search_In_Subdirs;
      end loop;

      --  No files found.

      return "";
   exception
      when others => return "";
   end Get_First_Filename;

end Wiki_Website.Directories;
