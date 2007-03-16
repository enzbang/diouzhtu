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

   function Blockquote (Index : Positive; Block : String) return String;
   --  bq. Blockquote element

   function Header (Index : Positive; Block : String) return String;
   --  <h1>, <h2>, ... element

   function List (Index : Positive; Block : String) return String;
   --  list element <ul>, <ol>

   function Paragraph (Index : Positive; Block : String) return String;
   --  <p> element

   function Table (Index : Positive; Block : String) return String;
   --  table element

   ----------------
   -- Blockquote --
   ----------------

   function Blockquote (Index : Positive; Block : String) return String is
      Extract : constant Pattern_Matcher :=
        Compile ("^bq" & Attribute.Get_Pattern & "\.\s(.*)$",
                 Case_Insensitive + Single_Line);
      Count   : constant Match_Count := Paren_Count (Extract);
      Matches : Match_Array (0 .. Paren_Count (Extract));
      Result  : Unbounded_String := Null_Unbounded_String;
   begin
      Match (Extract, Block, Matches);
      if Matches (0) = No_Match then
         return Parse (Block_Level, Block, Index);
      end if;

      Result := To_Unbounded_String ("<blockquote");

      if Matches (1) /= No_Match then
         Append (Result, Attribute.Extract
                   (Block (Matches (1).First .. Matches (1).Last)));
      end if;

      if Matches (Count) /= No_Match then
         Append (Result, ">" &
                   Parse (Inline_Level,
                          Block
                            (Matches (Count).First .. Matches (Count).Last)) &
                   "</blockquote>" & ASCII.Lf);
      end if;
      return To_String (Result);

   end Blockquote;

   ------------
   -- Header --
   ------------

   function Header (Index : Positive; Block : String) return String is
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
         return Parse (Block_Level, Block, Index);
      end if;

      Result := To_Unbounded_String
        ("<h" & Block (Matches (1).First .. Matches (1).Last));

      if Matches (2) /= No_Match then
         Append (Result, Attribute.Extract
                   (Block (Matches (2).First .. Matches (2).Last)));
      end if;

      if Matches (Count) /= No_Match then
         Append (Result, ">" &
                   Parse (Inline_Level,
                          Block (Matches
                                   (Count).First .. Matches (Count).Last)) &
                   "</h1>" & ASCII.Lf);
      end if;

      return To_String (Result);
   end Header;

   function List (Index : Positive; Block : String) return String is
      Element  : constant array (1 .. 2) of String (1 .. 2) := ("ol", "ul");
      Tag      : constant array (1 .. 2) of String (1 .. 2) := ("# ", "* ");
      Get_Tag  : String (1 .. 2);
      Position : Natural := 0;
      Result   : Unbounded_String := Null_Unbounded_String;
   begin
      if Block'Length < 3 then
         return Parse (Block_Level, Block, Index);
      end if;

      Get_Tag := Block (Block'First .. Block'First + 1);

      if Get_Tag = Tag (1) then
         Position := 1;
      elsif Get_Tag = Tag (2) then
         Position := 2;
      else
         return Parse (Block_Level, Block, Index);
      end if;

      declare
         Last_Level    : Positive := 1;
         Current_Level : Natural  := 0;
         Start_Line    : Positive := 1;
         Last          : Positive;
         Begin_List    : constant String :=
           '<' & Element (Position) & '>' & ASCII.Lf;
         End_List      : constant String :=
           "</" & Element (Position) & '>' & ASCII.Lf;
         Begin_Item    : constant String := "<li>";
         End_Item      : constant String := "</li>" & ASCII.Lf;
      begin
         Append (Result, Begin_List);
         for I in Block'First + 2 .. Block'Last loop
            if Block (I - 1) = ASCII.Lf or else I = Block'Last then
               --  Count number of # or * at the beginning of line
               Current_Level := 0;
               for K in Start_Line .. I - 2 loop
                  if Block (K) = Get_Tag (1) then
                     Current_Level := Current_Level + 1;
                  end if;
               end loop;

               --  If current_level = 0
               --  consider this is the same line
               --
               --  ex:
               --  # begin of list with a line too
               --  long
               --  # second element

               if Current_Level /= 0 then
                  if Current_Level > Last_Level then
                     Append (Result,
                             (Current_Level - Last_Level) * Begin_List);
                  elsif Current_Level < Last_Level then
                     Append (Result, (Last_Level - Current_Level) * End_List);
                  end if;
                  if I = Block'Last then
                     Last := I;
                  else
                     --  Does not add the tag
                     Last := I - 2;
                  end if;
                  Append (Result, Begin_Item & Parse
                          (Inline_Level,
                           Block (Start_Line + Current_Level + 1 .. Last)) &
                           End_Item);
                  Last_Level := Current_Level;
                  Start_Line := I;
               end if;
            end if;
         end loop;

         Append (Result, End_List);
      end;
      return To_String (Result);
   end List;

   ---------------
   -- Paragraph --
   ---------------

   function Paragraph (Index : Positive; Block : String) return String is
      pragma Unreferenced (Index);
      Extract : constant Pattern_Matcher :=
        Compile ("^p" & Attribute.Get_Pattern & "\.\s(.*)$",
                 Case_Insensitive + Single_Line);
      Count   : constant Match_Count := Paren_Count (Extract);
      Matches : Match_Array (0 .. Paren_Count (Extract));
      Result  : Unbounded_String := Null_Unbounded_String;
   begin
      Match (Extract, Block, Matches);
      if Matches (0) = No_Match then
         return "<p>" & Parse (Inline_Level, Block) & "</p>" & ASCII.Lf;
      end if;

      Result := To_Unbounded_String ("<p");

      if Matches (1) /= No_Match then
         Append (Result, Attribute.Extract
                   (Block (Matches (1).First .. Matches (1).Last)));
      end if;

      if Matches (Count) /= No_Match then

         Append
           (Result, ">" &
              Parse (Inline_Level,
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

   function Table (Index : Positive; Block : String) return String is

      Nb_Cols     : Natural          := 0;
      Nb_Rows     : Natural          := 0;
      Table_Block : String           := Block;
      Result      : Unbounded_String := Null_Unbounded_String;

      procedure Get_Dimension;
      --  Get maximum of cols and number of rows

      procedure Get_Dimension is
         Line_Nb_Cols  : Natural := 0;
      begin
         for I in Table_Block'Range loop
            if Table_Block (I) = ASCII.Lf then
               if I > Table_Block'First + 1
                 and then Table_Block (I - 2 .. I - 1) /= " |" then
                  --  A table line MUST end with |
                  return;
               end if;
               if I < Table_Block'Last - 1
                 and then Table_Block (I + 1 .. I + 2) /= "| " then
                  --  A table line MUST begin with |
                  return;
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
         end loop;
      end Get_Dimension;

   begin
      Get_Dimension;

      if Nb_Cols = 0 or else Nb_Rows = 0 then
         return Parse (Block_Level, Table_Block, Index);
      end if;

      declare
         Line_Cols     : Natural := 0;
         Last_Position : Natural := 0;
      begin
         for K in Table_Block'Range loop
            if Table_Block (K) = '|' then
               if Line_Cols > 0 and then Last_Position + 2 < K - 2 then
                  Append
                    (Result, Parse (Inline_Level,
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
                  declare
                     Nb_Empty_Cols : constant Natural := Nb_Cols - Line_Cols;
                     Empty_Col     : constant String := "<td></td>" & ASCII.Lf;
                  begin
                     Append (Result, "</td>" & ASCII.Lf &
                             (Nb_Empty_Cols - 1) * Empty_Col);
                  end;
               end if;
               if K /= Table_Block'Last then
                  Append (Result, "</tr>" & ASCII.Lf & "<tr>" & ASCII.Lf);
               else
                  Append (Result, "</tr>" & ASCII.Lf);
               end if;
               Line_Cols := 0;
            end if;
         end loop;
      end;
      return "<table>" & ASCII.Lf & "<tr>" & ASCII.Lf
        & To_String (Result) & "</table>" & ASCII.Lf;
   end Table;

end Diouzhtu.Block;
