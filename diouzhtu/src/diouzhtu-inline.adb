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

with GNAT.Regpat;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Text_IO;
with Diouzhtu;

package body Diouzhtu.Inline is

   use GNAT.Regpat;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   function Code (Index : Positive; S : String) return String;
   --  Code phrases can be surrounded by @.
   --  @code@

   function Default (Index : Positive; S : String) return String;
   --  Default callback

   function Emphasis (Index : Positive; S : String) return String;
   --  Emphasis to text is added by surrounding a phrase with underscores.
   --  _emphasized_ (e.g., italics)

   function Image (Index : Positive; S : String) return String;
   --  !(class)image.url(tooltip)!
   --  The tooltip act as alt text

   function Link (Index : Positive; S : String) return String;
   --  "(class)link name(tooltip)":http ://u.r.l
   --  "(class)link name(tooltip)":relative/url

   function Strong (Index : Positive; S : String) return String;
   --  Strength can be give to text by surrounding with asterisks.
   --  *strongly emphasized* (e.g., boldface)

   function Code (Index : Positive; S : String) return String is
      Extract  : constant Pattern_Matcher :=
        Compile ("@(.*?)@", Case_Insensitive);
      Matches  : Match_Array (0 .. 1);
      Current  : Natural := S'First;
      Result   : Unbounded_String := Null_Unbounded_String;
   begin
      loop
         Match (Extract, S, Matches, Current);
         exit when Matches (0) = No_Match;

         if Matches (1).First > Current + 1 then
            Append
              (Result,
               Parse (Inline_Level,
                      S (Current .. Matches (1).First - 2), Index));
         end if;

         if Matches (1).First = Matches (1).Last + 1 then
            Append (Result, "@");
         else
            --  Do not parse content between @

            Append (Result, "<code>" &
                      S (Matches (1).First .. Matches (1).Last) &
                      "</code>");
         end if;
         Current := Matches (1).Last + 2;
      end loop;

      if Current = S'First then
         --  No match, try next inline callback
         return Parse (Inline_Level, S, Index);
      end if;
      Append (Result, Parse (Inline_Level, S (Current .. S'Last), Index));
      return To_String (Result);
   end Code;

   -------------
   -- Default --
   -------------

   function Default (Index : Positive; S : String) return String is
      pragma Unreferenced (Index);

      Result : Unbounded_String;
      Last   : Integer := S'First;

      procedure Append_To_Result
        (Str : in String; From : in Integer; To : in Integer);
      --  Append S (From .. To) to Result if not empty concatenated with Str
      --  and update Last.

      ----------------------
      -- Append_To_Result --
      ----------------------

      procedure Append_To_Result
        (Str : in String; From : in Integer; To : in Integer) is
      begin
         if From <= To then
            Append (Result, S (From .. To) & Str);
         else
            Append (Result, Str);
         end if;

         Last := To + 2;
      end Append_To_Result;

      To_Skip : Natural := 0;

   begin
      for I in S'Range loop
         if To_Skip > 0 then
            To_Skip := To_Skip - 1;
         else
            case S (I) is
               when '-' =>
                  if S'First < I + 1 and then I < S'Last - 2 then
                     if S (I - 1 .. I + 1) = " - " then
                        Append_To_Result ("&ndash;", Last, I - 1);
                        To_Skip := 1;
                     elsif S (I - 1 .. I + 2) = " -- " then
                        Append_To_Result ("&mdash;", Last, I - 1);
                        To_Skip := 2;
                        Last := Last + 1;
                     end if;
                  end if;
               when '.' =>
                  if I < S'Last - 1 then
                     if S (I .. I + 2) = "..." then
                        Append_To_Result ("&hellip;", Last, I - 1);
                        To_Skip := 2;
                        Last := Last + To_Skip;
                     end if;
                  end if;
               when 'x' =>
                  if S'First + 1 < I and then I < S'Last - 2
                    and then S (I - 1 .. I + 1) = " x " then
                     Append_To_Result ("&times;", Last, I - 1);
                  end if;
               when others => null;
            end case;
         end if;
      end loop;

      if Last <= S'Last then
         Append (Result, S (Last .. S'Last));
      end if;

      return To_String (Result);
   end Default;

   --------------
   -- Emphasis --
   --------------

   function Emphasis (Index : Positive; S : String) return String is
      Extract  : constant Pattern_Matcher :=
        Compile ("_(.*?)_", Case_Insensitive);
      Matches  : Match_Array (0 .. 1);
      Current  : Natural := S'First;
      Result   : Unbounded_String := Null_Unbounded_String;
   begin
      loop
         Match (Extract, S, Matches, Current);
         exit when Matches (0) = No_Match;

         if Matches (1).First > Current + 1 then
            Append
              (Result,
               Parse (Inline_Level,
                      S (Current .. Matches (1).First - 2), Index));
         end if;

         declare
            In_Content : constant String :=
              Parse (Inline_Level,
                     S (Matches (1).First .. Matches (1).Last),
                     Index);
         begin
            Append (Result, "<em>" & In_Content & "</em>");
         end;
         Current := Matches (1).Last + 2;
      end loop;

      if Current = S'First then
         --  No match, try next inline callback
         return Parse (Inline_Level, S, Index);
      end if;
      Append (Result, Parse (Inline_Level, S (Current .. S'Last), Index));
      return To_String (Result);
   end Emphasis;

   -----------
   -- Image --
   -----------

   function Image (Index : Positive; S : String) return String is
      Extract  : constant Pattern_Matcher
        := Compile ("!(\([\w-_]+?\))??((http://)??[\w._-]+?)(\([\w-_]+?\))??!",
                    Case_Insensitive);
      Matches  : Match_Array (0 .. 5);
      Current  : Natural := S'First;
      Result   : Unbounded_String := Null_Unbounded_String;
   begin
      loop
         Match (Extract, S, Matches, Current);
         exit when Matches (0) = No_Match;

         if Matches (0).First > Current + 1 then
            Append
              (Result,
               Parse (Inline_Level,
                      S (Current .. Matches (0).First - 1), Index));
         end if;

         declare
            URL         : constant String :=
              Parse (Inline_Level,
                     S (Matches (2).First .. Matches (2).Last));
            Http_Prefix : constant String := "http://";
         begin
            if URL'Length >= Http_Prefix'Length and then
              URL (URL'First ..
                     URL'First + Http_Prefix'Length - 1) = Http_Prefix then
               Append (Result, "<img src='" & URL & "'");
            else
               Append (Result, "<img src='" & Diouzhtu_Base_URL & URL & "'");
            end if;
         end;

         if Matches (1) /= No_Match then
            Append
              (Result, " class='"
                 & S (Matches (1).First + 1 .. Matches (1).Last - 1) & "'");
         end if;

         if Matches (5) /= No_Match then
            Append
              (Result, " title='"
                 & S (Matches (5).First + 1 .. Matches (5).Last - 1) & "'");
            Append
              (Result, " alt='"
                 & S (Matches (5).First + 1 .. Matches (5).Last - 1) & "'");
         end if;

         Append
           (Result, '>');
         Current := Matches (0).Last + 1;
      end loop;

      if Current = S'First then
         --  No match, try next inline callback
         return Parse (Inline_Level, S, Index);
      end if;
      Append (Result, Parse (Inline_Level, S (Current .. S'Last), Index));
      return To_String (Result);
   end Image;

   ----------
   -- Link --
   ----------

   function Link (Index : Positive; S : String) return String is
      Extract  : constant Pattern_Matcher :=
        Compile ("""(\([\w-_]+?\))??([^\(\)]+?)(\(.*\))??"":" &
                 "((http://)??[^ \s\[\]]+)(\s|$)",
                 Case_Insensitive + Single_Line);
      --          :=
      --  Compile ('(' & '"' & "(\([\w-_]+?\))??([^()/]*)?(\([\w-_]+\))??"
      --                        & """:([^ \s\[\]]+))",
      --                      Case_Insensitive);
      --  & '"' & ":""((http://)??[\w._-]+?)\G)",
      Matches  : Match_Array (0 .. 6);
      Current  : Natural := S'First;
      Result   : Unbounded_String := Null_Unbounded_String;
   begin
      loop
         Match (Extract, S, Matches, Current);
         exit when Matches (0) = No_Match;

         if Matches (0).First > Current + 1 then
            Append
              (Result,
               Parse (Inline_Level,
                      S (Current .. Matches (0).First - 1), Index));
         end if;

         declare
            URL         : constant String :=
              Parse (Inline_Level,
                     S (Matches (4).First .. Matches (4).Last));
            Http_Prefix : constant String := "http://";
         begin
            if URL'Length >= Http_Prefix'Length and then
              URL (URL'First ..
                     URL'First + Http_Prefix'Length - 1) = Http_Prefix then
               Append (Result, "<a href='" & URL & "'");
            else
               Append (Result, "<a href='" & Diouzhtu_Base_URL & URL & "'");
            end if;
         end;

         if Matches (1) /= No_Match then
            Append
              (Result, " class='"
                 & S (Matches (1).First + 1 .. Matches (1).Last - 1) & "'");
         end if;

         if Matches (3) /= No_Match then
            Append
              (Result, " title='"
                 & S (Matches (3).First + 1 .. Matches (3).Last - 1) & "'");
         end if;

         if Matches (2) /= No_Match then
            Append
              (Result, '>' & Parse
                 (Inline_Level,
                  S (Matches (2).First .. Matches (2).Last)) & "</a> ");
         end if;
         Current := Matches (0).Last + 1;
      end loop;

      if Current = S'First then
         --  No match, try next inline callback
         return Parse (Inline_Level, S, Index);
      end if;
      Append (Result, Parse (Inline_Level, S (Current .. S'Last), Index));
      return To_String (Result);
   exception
      when E : others => Ada.Text_IO.Put_Line (Exception_Information (E));
         return "";
   end Link;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Diouzhtu.Internal_Register (Inline_Level, Code'Access);
      Diouzhtu.Internal_Register (Inline_Level, Emphasis'Access);
      Diouzhtu.Internal_Register (Inline_Level, Strong'Access);
      Diouzhtu.Internal_Register (Inline_Level, Link'Access);
      Diouzhtu.Internal_Register (Inline_Level, Image'Access);
      Diouzhtu.Internal_Register (Inline_Level, Default'Access);
   end Register;

   ------------
   -- Strong --
   ------------

   function Strong (Index : Positive; S : String) return String is
      Extract  : constant Pattern_Matcher :=
        Compile ("\*(.*?)\*", Case_Insensitive);
      Matches  : Match_Array (0 .. 1);
      Current  : Natural := S'First;
      Result   : Unbounded_String := Null_Unbounded_String;
   begin
      loop
         Match (Extract, S, Matches, Current);
         exit when Matches (0) = No_Match;

         if Matches (1).First > Current + 1 then
            Append
              (Result, Parse
                 (Inline_Level, S (Current .. Matches (1).First - 2), Index));
         end if;

         declare
            In_Content : constant String :=
              Parse (Inline_Level,
                     S (Matches (1).First .. Matches (1).Last),
                     Index);
         begin
            Append (Result, "<strong>" & In_Content & "</strong>");
         end;
         Current := Matches (1).Last + 2;
      end loop;

      Append (Result, Parse (Inline_Level, S (Current .. S'Last), Index));
      return To_String (Result);
   end Strong;

end Diouzhtu.Inline;

