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

package body Diouzhtu.Block is

   use GNAT.Regpat;
   use Ada.Strings.Unbounded;
   use Diouzhtu;

   function Blockquote
     (Wiki : in Wiki_Information; Index : in Positive; Block : in String)
      return String;
   --  bq. Blockquote element

   function Header
     (Wiki : in Wiki_Information; Index : in Positive; Block : in String)
      return String;
   --  <h1>, <h2>, ... element

   function List
     (Wiki : in Wiki_Information; Index : in Positive; Block : in String)
      return String;
   --  list element <ul>, <ol>

   function Paragraph
     (Wiki : in Wiki_Information; Index : in Positive; Block : in String)
      return String;
   --  <p> element

   function Table
     (Wiki : in Wiki_Information; Index : in Positive; Block : in String)
      return String;
   --  table element

   ----------------
   -- Blockquote --
   ----------------

   function Blockquote
     (Wiki : in Wiki_Information; Index : in Positive; Block : in String)
      return String is
      Extract : constant Pattern_Matcher :=
        Compile (Expression => "^bq" & Attribute.Get_Pattern & "\.\s(.*)$",
                 Flags      => Case_Insensitive + Single_Line);
      Count   : constant Match_Count := Paren_Count (Extract);
      Matches : Match_Array (0 .. Paren_Count (Extract));
      Result  : Unbounded_String := Null_Unbounded_String;
   begin
      Match (Extract, Block, Matches);
      if Matches (0) = No_Match then
         return Parse (Wiki    => Wiki,
                       Level   => Block_Level,
                       Content => Block,
                       Index   => Index);
      end if;

      Result := To_Unbounded_String ("<blockquote");

      if Matches (1) /= No_Match then
         Append (Result, Attribute.Extract
                   (Block (Matches (1).First .. Matches (1).Last)));
      end if;

      if Matches (Count) /= No_Match then
         Append
           (Result, "><p>" &
            Parse (Wiki, Inline_Level,
              Block (Matches (Count).First .. Matches (Count).Last)) &
            "</p></blockquote>" & ASCII.Lf);
      end if;
      return To_String (Result);

   end Blockquote;

   ------------
   -- Header --
   ------------

   function Header
     (Wiki : in Wiki_Information; Index : in Positive; Block : in String)
      return String
   is
      Extract : constant Pattern_Matcher :=
        Compile (Expression => "^h(\d)" & Attribute.Get_Pattern & "\.\s(.*)$",
                 Flags => Case_Insensitive + Single_Line);

      --  Get all hn.
      Count   : constant Match_Count := Paren_Count (Extract);
      Matches : Match_Array (0 .. Paren_Count (Extract));
      Result  : Unbounded_String := Null_Unbounded_String;
      Header  : Character;
   begin
      --   Search for block level
      Match (Extract, Block, Matches);
      if Matches (0) = No_Match then
         return Parse (Wiki    => Wiki,
                       Level   => Block_Level,
                       Content => Block,
                       Index   => Index);
      end if;

      Header := Block (Matches (1).First);

      Result := To_Unbounded_String
        ("<h" & Header);

      if Matches (2) /= No_Match then
         Append (Result, Attribute.Extract
                   (Block (Matches (2).First .. Matches (2).Last)));
      end if;

      if Matches (Count) /= No_Match then
         Append (Result, ">" &
                   Parse (Wiki, Inline_Level,
                          Block (Matches
                                   (Count).First .. Matches (Count).Last)) &
                   "</h" & Header & ">" & ASCII.Lf);
      end if;

      return To_String (Result);
   end Header;

   function List
     (Wiki : in Wiki_Information; Index : in Positive; Block : in String)
      return String
   is
      type Elements is array (1 .. 2) of String (1 .. 2);
      type Tags is array (1 .. 2) of Character;

      Element     : constant Elements  := Elements'(1 => "ol", 2 => "ul");
      Tag         : constant Tags := Tags'(1 => '#', 2 => '*');
      Indentation : constant String := "  ";

      Get_Element : String (1 .. 2);
      Get_Tag     : Character;
      Result      : Unbounded_String := Null_Unbounded_String;

      type List_Level is new Natural;

      procedure Parse_Line
        (Wiki       : in Wiki_Information;
         Line       : in String;
         Level      : in out List_Level;
         Line_Level : in List_Level);

      function Get_Current_Level (Line : in String) return List_Level;

      function Indent (Level : in List_Level) return String;

      -----------------------
      -- Get_Current_Level --
      -----------------------

      function Get_Current_Level (Line : in String) return List_Level is
         Level : List_Level := 0;
      begin
         Get_Level :
         for K in Line'Range loop
            exit Get_Level when Block (K) /= Get_Tag;
            Level := Level + 1;
         end loop Get_Level;
         return Level;
      end Get_Current_Level;

      ------------
      -- Indent --
      ------------

      function Indent (Level : in List_Level) return String is
         pragma Warnings (Off);
         N : Natural;
      begin
         if Level > 0 then
            N := 2 * (Positive (Level) - 1);
            return To_String (N * Indentation);
         end if;
         return "";
      end Indent;

      ----------------
      -- Parse_Line --
      ----------------

      procedure Parse_Line
        (Wiki       : in Wiki_Information;
         Line       : in String;
         Level      : in out List_Level;
         Line_Level : in List_Level) is
         pragma Warnings (Off);
      begin
         if Line_Level > Level then
            if Line_Level > 1 then
               Append (Result, Indent (Level) & Indentation);
               Append (Result, "<li>" & ASCII.Lf);
            end if;
            Level := Level + 1;
            Append (Result, Indent (Level)
                      & '<' & Get_Element & '>' & ASCII.Lf);
            Parse_Line (Wiki, Line, Level, Line_Level);
         elsif Line_Level < Level then
            Append (Result, Indent (Level)
                      & "</" & Get_Element & '>' & ASCII.Lf);
            if Level > 1 then
               Level := Level - 1;
               Append (Result, Indent (Level) & Indentation
                         & "</li>" & ASCII.Lf);
               Parse_Line (Wiki, Line, Level, Line_Level);
            end if;
         else
            if Line /= "" then
               Append (Result, Indent (Level) & Indentation);
               Append_Last_Content :
               declare
                  Content_First : constant Positive :=
                                    Line'First + Natural (Level) + 1;
                  Content_Last  : Positive := Line'Last;
               begin
                  if Line (Content_Last) = ASCII.Lf then
                     Content_Last := Content_Last - 1;
                  end if;
                  Append
                    (Result, "<li>" &
                     Parse
                       (Wiki, Inline_Level,
                        Line (Content_First .. Content_Last))
                     &  "</li>" & ASCII.Lf);
               end Append_Last_Content;
            end if;
         end if;
      end Parse_Line;

   begin
      if Block'Length < 3 then
         return Parse (Wiki    => Wiki,
                       Level   => Block_Level,
                       Content => Block,
                       Index   => Index);
      end if;

      Get_Tag := Block (Block'First);

      if Block (Block'First .. Block'First + 1) = Tag (1) & ' ' then
         Get_Element := Element (1);
      elsif Block (Block'First .. Block'First + 1) = Tag (2) & ' ' then
         Get_Element := Element (2);
      else
         return Parse (Wiki    => Wiki,
                       Level   => Block_Level,
                       Content => Block,
                       Index   => Index);
      end if;

      Parse_Lines :
      declare
         Last       : Positive   := Block'First;
         Last_Level : List_Level := 0; -- List_Level'First
      begin
         for K in Block'Range loop
            if K = Block'Last
              or else (Block (K) = ASCII.Lf and then Block (K + 1) = Get_Tag)
            then
               Parse_Current_Line :
               declare
                  Line       : constant String := Block (Last .. K);
                  Line_Level : constant List_Level := Get_Current_Level (Line);
               begin
                  Parse_Line (Wiki       => Wiki,
                              Line       => Line,
                              Level      => Last_Level,
                              Line_Level => Line_Level);
                  Last := K + 1;
               end Parse_Current_Line;
            end if;
         end loop;
         Parse_Line (Wiki       => Wiki,
                     Line       => "",
                     Level      => Last_Level,
                     Line_Level => 0);
      end Parse_Lines;
      return To_String (Result);
   end List;

   ---------------
   -- Paragraph --
   ---------------

   function Paragraph
     (Wiki : in Wiki_Information; Index : in Positive; Block : in String)
      return String
   is
      pragma Unreferenced (Index);
      Extract : constant Pattern_Matcher :=
        Compile (Expression => "^p" & Attribute.Get_Pattern & "\.\s(.*)$",
                 Flags      => Case_Insensitive + Single_Line);
      Count   : constant Match_Count := Paren_Count (Extract);
      Matches : Match_Array (0 .. Paren_Count (Extract));
      Result  : Unbounded_String := Null_Unbounded_String;
   begin
      Match (Extract, Block, Matches);
      if Matches (0) = No_Match then
         return "<p>" & Parse (Wiki, Inline_Level, Block) & "</p>" & ASCII.Lf;
      end if;

      Result := To_Unbounded_String ("<p");

      if Matches (1) /= No_Match then
         Append (Result, Attribute.Extract
                   (Block (Matches (1).First .. Matches (1).Last)));
      end if;

      if Matches (Count) /= No_Match then

         Append
           (Result, ">" &
              Parse (Wiki, Inline_Level,
                     Block (Matches (Count).First .. Matches (Count).Last))
              & "</p>" & ASCII.Lf);
      end if;
      return To_String (Result);
   end Paragraph;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Internal_Register (Block_Level, Header'Access);
      Internal_Register (Block_Level, List'Access);
      Internal_Register (Block_Level, Blockquote'Access);
      Internal_Register (Block_Level, Table'Access);
      Internal_Register (Block_Level, Paragraph'Access);

   end Register;

   -----------
   -- Table --
   -----------

   function Table
     (Wiki : in Wiki_Information; Index : in Positive; Block : in String)
      return String
   is

      Nb_Cols     : Natural          := 0;
      Nb_Rows     : Natural          := 0;
      Table_Block : String           := Block;
      Result      : Unbounded_String := Null_Unbounded_String;

      procedure Get_Dimension;
      --  Get maximum of cols and number of rows

      procedure Get_Dimension is
         Line_Nb_Cols  : Natural := 0;
      begin
         Main_Loop :
         for I in Table_Block'Range loop
            if Table_Block (I) = ASCII.Lf then
               if I > Table_Block'First + 1
                 and then Table_Block (I - 2 .. I - 1) /= " |" then
                  --  A table line MUST end with |
                  exit Main_Loop;
               end if;
               if I < Table_Block'Last - 1
                 and then Table_Block (I + 1 .. I + 2) /= "| " then
                  --  A table line MUST begin with |
                  exit Main_Loop;
               end if;
               if Line_Nb_Cols > Nb_Cols then
                  Nb_Cols := Line_Nb_Cols;
               end if;
               Nb_Rows := Nb_Rows + 1;
               Line_Nb_Cols := 0;
            end if;

            if Table_Block (I) = '|' then
               if (I = Table_Block'First or else
                     Table_Block (I - 1) = ' ' or else
                       Table_Block (I - 1) = ASCII.Lf) and then
                 (I = Table_Block'Last or else
                    Table_Block (I + 1) = ' ' or else
                      Table_Block (I + 1) = ASCII.Lf)
               then
                  Line_Nb_Cols := Line_Nb_Cols + 1;
               else
                  Table_Block (I) := ' ';
               end if;
            end if;
         end loop Main_Loop;
      end Get_Dimension;

   begin
      Get_Dimension;

      if Nb_Cols = 0 or else Nb_Rows = 0 then
         return Parse (Wiki    => Wiki,
                       Level   => Block_Level,
                       Content => Table_Block,
                       Index   => Index);
      end if;

      Parse_Lines :
      declare
         Line_Cols     : Natural := 0;
         Last_Position : Natural := 0;
      begin
         for K in Table_Block'Range loop
            if Table_Block (K) = '|' then
               if Line_Cols > 0 and then Last_Position + 2 < K - 2 then
                  Append
                    (Result, Parse (Wiki, Inline_Level,
                     Table_Block (Last_Position + 2 .. K - 2)));
                  Append (Result, "</td>" & ASCII.Lf);
               end if;
               if Line_Cols < Nb_Cols - 1 then
                  Append (Result, "<td>");
               end if;
               Line_Cols := Line_Cols + 1;
               Last_Position := K;
            end if;
            if Table_Block (K) = ASCII.Lf or else K = Table_Block'Last then
               if Line_Cols < Nb_Cols then
                  Adds_Empty_Cols :
                  declare
                     Nb_Empty_Cols : constant Natural := Nb_Cols - Line_Cols;
                     Empty_Col     : constant String := "<td></td>" & ASCII.Lf;
                  begin
                     Append (Result, "</td>" & ASCII.Lf &
                             (Nb_Empty_Cols - 1) * Empty_Col);
                  end Adds_Empty_Cols;
               end if;
               if K /= Table_Block'Last then
                  Append (Result, "</tr>" & ASCII.Lf & "<tr>" & ASCII.Lf);
               else
                  Append (Result, "</tr>" & ASCII.Lf);
               end if;
               Line_Cols := 0;
            end if;
         end loop;
      end Parse_Lines;
      return "<table>" & ASCII.Lf & "<tr>" & ASCII.Lf
        & To_String (Result) & "</table>" & ASCII.Lf;
   end Table;

end Diouzhtu.Block;
