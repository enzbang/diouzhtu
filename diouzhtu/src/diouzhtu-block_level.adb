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
with Ada.Text_IO;

package body Diouzhtu.Block_Level is

   use GNAT.Regpat;
   use Ada.Strings.Unbounded;
   use Diouzhtu;

   function Blockquote (Block : in String) return String;
   --  <blockquote> element

   function Header (Block : in String) return String;
   --  <h1>, <h2>, ... element

   function Paragraph (Block : in String) return String;
   --  <p> element

   ----------------
   -- Blockquote --
   ----------------

   function Blockquote (Block : in String) return String is
      Extract : constant Pattern_Matcher :=
                  Compile ("^bq" & Attribute.Get_Pattern & "\.\s(.*)$",
                           Case_Insensitive + Single_Line);
      Count   : constant Match_Count := Paren_Count (Extract);
      Matches : Match_Array (0 .. Paren_Count (Extract));
      Result  : Unbounded_String := Null_Unbounded_String;
   begin
      Match (Extract, Block, Matches);
      if Matches (0) = No_Match then
         return "";
      end if;

      Result := To_Unbounded_String ("<blockquote");

      if Matches (1) /= No_Match then
         Append (Result, Attribute.Extract
                 (Block (Matches (1).First .. Matches (1).Last)));
      end if;

      if Matches (Count) /= No_Match then
         Append (Result, ">" &
                 Block (Matches (Count).First .. Matches (Count).Last) &
                 "<\blockquote>" & ASCII.Lf);
      end if;
      return To_String (Result);

   end Blockquote;

   ------------
   -- Header --
   ------------

   function Header (Block : in String) return String is
      Extract : constant Pattern_Matcher :=
                  Compile ("^h(\d)" & Attribute.Get_Pattern & "\.\s(.*)$",
                           Case_Insensitive + Single_Line);

      --  Get all hn.
      Count   : constant Match_Count := Paren_Count (Extract);
      Matches : Match_Array (0 .. Paren_Count (Extract));
      Result  : Unbounded_String := Null_Unbounded_String;
   begin
      --   Search for block level
      Match (Extract, Block, Matches);
      if Matches (0) = No_Match then
         Ada.Text_IO.Put_Line (Block);
         return "";
      end if;

      Result := To_Unbounded_String
        ("<h" & Block (Matches (1).First .. Matches (1).Last));

      if Matches (2) /= No_Match then
         Append (Result, Attribute.Extract
                 (Block (Matches (2).First .. Matches (2).Last)));
      end if;

      if Matches (Count) /= No_Match then
         Append (Result, ">" &
                 Block (Matches (Count).First .. Matches (Count).Last) &
                 "</h1>" & ASCII.Lf);
      end if;

      return To_String (Result);
   end Header;

   ---------------
   -- Paragraph --
   ---------------

   function Paragraph (Block : in String) return String is
   begin
      return "<p>" & Block & "</p>" & ASCII.Lf;
   end Paragraph;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Register (Header'Access);
      Register (Blockquote'Access);
      Register (Paragraph'Access);
   end Register;

end Diouzhtu.Block_Level;
