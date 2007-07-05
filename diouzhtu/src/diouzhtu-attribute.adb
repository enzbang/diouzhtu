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
with GNAT.Regpat;

package body Diouzhtu.Attribute is

   use GNAT.Regpat;
   use Ada.Strings.Unbounded;

   Pattern : constant String := "\(([\w-_]+?)??((#[\w-_]+?)??)\)";

   -------------
   -- Extract --
   -------------

   function Extract
     (Content : in String; Add_Class : in String := "") return String is
      PM : constant Pattern_Matcher :=
        Compile (Pattern, Case_Insensitive);
      Matches : Match_Array (0 .. 3);
      Result  : Unbounded_String := Null_Unbounded_String;

      Class_Attr   : constant String := " class='";
      Id_Attr      : constant String := " id='";

   begin
      Match (PM, Content, Matches);
      if Matches (0) = No_Match then
         if Add_Class /= "" then
            return Class_Attr & "'" & Add_Class & "'";
         else
            return "";
         end if;
      end if;

      declare
         Class_Length : constant Integer :=
           Matches (1).Last - Matches (1).First;
         Id_Length    : constant Integer :=
           Matches (2).Last - Matches (2).First;
      begin
         if Matches (1) /= No_Match and then Class_Length > 0 then
            Append (Result, Class_Attr &
                      Content (Matches (1).First .. Matches (1).Last));
            if Add_Class /= "" then
               Append (Result, " " & Add_Class);
            end if;
            Append (Result, "'");
         elsif Add_Class /= "" then
            Append (Result, Class_Attr & Add_Class & "'");
         end if;
         if Matches (2) /= No_Match and then Id_Length > 0 then
            Append (Result, Id_Attr &
                      Content (Matches (2).First + 1 .. Matches (2).Last) &
                      "'");
         end if;
         return To_String (Result);
      end;
   end Extract;

   -----------------
   -- Get_Pattern --
   -----------------

   function Get_Pattern return String is
   begin
      return '(' & Pattern & ")??";
   end Get_Pattern;

end Diouzhtu.Attribute;
