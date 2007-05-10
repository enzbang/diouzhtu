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
with Diouzhtu.Attribute;
with GNAT.Regpat;
with Ada.Strings.Unbounded;

package body Diouzhtu.Code is

   use GNAT.Regpat;
   use Ada.Strings.Unbounded;
   use Diouzhtu;

   ----------------
   -- Begin_Code --
   ----------------

   function Begin_Code (Block : String) return String
   is
      Extract  : constant Pattern_Matcher :=
        Compile ("^code(_[a-zA-Z]+?)??" &
                   Attribute.Get_Pattern & "\.\s(.*?)$",
                 Case_Insensitive + Single_Line);
      Count   : constant Match_Count := Paren_Count (Extract);
      Matches : Match_Array (0 .. Count);
      Result  : Unbounded_String := Null_Unbounded_String;
   begin
      Match (Extract, Block, Matches);
      if Matches (0) = No_Match then
         return "";
      end if;

      Result := To_Unbounded_String ("<p><pre><code");

      if Matches (2) /= No_Match then
         Append (Result, Attribute.Extract
                   (Block (Matches (2).First .. Matches (2).Last),
                    Block (Matches (1).First + 1 .. Matches (1).Last)));
      elsif Matches (1) /= No_Match then
         Append (Result, " class='" &
                   Block (Matches (1).First + 1 .. Matches (1).Last) &
                   "'");
      end if;

      Append (Result, ">");
      return To_String (Result) & Block (Matches (Count).First .. Block'Last);
   end Begin_Code;

   --------------
   -- End_Code --
   --------------

   function End_Code return String is
   begin
      return "</code></pre></p>" & ASCII.Lf;
   end End_Code;

end Diouzhtu.Code;
