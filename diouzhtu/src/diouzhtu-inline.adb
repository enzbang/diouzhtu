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
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Text_IO;

with Diouzhtu;

package body Diouzhtu.Inline is

   use GNAT.Regpat;
   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   function Code
     (Wiki : in Wiki_Information; Index : in Positive; S : in String)
      return String;
   --  Code phrases can be surrounded by @.
   --  @code@

   function Default
     (Wiki : in Wiki_Information; Index : in Positive; S : in String)
      return String;
   --  Default callback

   function Emphasis
     (Wiki : in Wiki_Information; Index : in Positive; S : in String)
      return String;
   --  Emphasis to text is added by surrounding a phrase with underscores.
   --  _emphasized_ (e.g., italics)

   function Image
     (Wiki : in Wiki_Information; Index : in Positive; S : in String)
      return String;
   --  !(class)image.url(tooltip)!
   --  The tooltip act as alt text

   function Link
     (Wiki : in Wiki_Information; Index : in Positive; S : in String)
      return String;
   --  "(class)link name(tooltip)":http ://u.r.l
   --  "(class)link name(tooltip)":relative/url

   function Single_Link
     (Wiki : in Wiki_Information; Index : in Positive; S : in String)
      return String;
   --  http://u.r.l

   function Strong
     (Wiki : in Wiki_Information; Index : in Positive; S : in String)
      return String;
   --  Strength can be give to text by surrounding with asterisks.
   --  *strongly emphasized* (e.g., boldface)

   ----------
   -- Code --
   ----------

   function Code
     (Wiki : in Wiki_Information; Index : in Positive; S : in String)
      return String
   is
      Extract  : constant Pattern_Matcher :=
        Compile (Expression => "@(.*?)@", Flags => Case_Insensitive);
      Matches  : Match_Array (0 .. 1);
      Current  : Natural := S'First;
      Result   : Unbounded_String := Null_Unbounded_String;
   begin
      Extract_All_Code_Blocks :
      loop
         Match (Self       => Extract,
                Data       => S,
                Matches    => Matches,
                Data_First => Current);
         exit Extract_All_Code_Blocks when Matches (0) = No_Match;

         if Matches (1).First > Current + 1 then
            Append
              (Result,
               Parse (Wiki    => Wiki,
                      Level   => Inline_Level,
                      Content => S (Current .. Matches (1).First - 2),
                      Index   => Index));
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
      end loop Extract_All_Code_Blocks;

      if Current = S'First then
         --  No match, try next inline callback
         return Parse (Wiki    => Wiki,
                       Level   => Inline_Level,
                       Content => S,
                       Index   => Index);
      end if;
      Append (Result,
        Parse (Wiki    => Wiki,
               Level   => Inline_Level,
               Content => S (Current .. S'Last),
               Index   => Index));
      return To_String (Result);
   end Code;

   -------------
   -- Default --
   -------------

   function Default
     (Wiki : in Wiki_Information; Index : in Positive; S : in String)
      return String
   is
      pragma Unreferenced (Wiki, Index);

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

   function Emphasis
     (Wiki : in Wiki_Information; Index : in Positive; S : in String)
      return String
   is
      Extract  : constant Pattern_Matcher :=
        Compile (Expression => "_(.*?)_", Flags => Case_Insensitive);
      Matches  : Match_Array (0 .. 1);
      Current  : Natural := S'First;
      Result   : Unbounded_String := Null_Unbounded_String;
   begin
      Extract_All :
      loop
         Match (Self       => Extract,
                Data       => S,
                Matches    => Matches,
                Data_First => Current);

         exit Extract_All when Matches (0) = No_Match;

         if Matches (1).First > Current + 1 then
            Append
              (Result,
               Parse (Wiki    => Wiki,
                      Level   => Inline_Level,
                      Content => S (Current .. Matches (1).First - 2),
                      Index   => Index));
         end if;

         Emphasis_Content :
         declare
            In_Content : constant String :=
                           Parse (Wiki    => Wiki,
                                  Level   => Inline_Level,
                                  Content =>
                                    S (Matches (1).First .. Matches (1).Last),
                                  Index   => Index);
         begin
            Append (Result, "<em>" & In_Content & "</em>");
         end Emphasis_Content;
         Current := Matches (1).Last + 2;
      end loop Extract_All;

      if Current = S'First then
         --  No match, try next inline callback
         return Parse (Wiki    => Wiki,
                       Level   => Inline_Level,
                       Content => S,
                       Index   => Index);
      end if;
      Append (Result,
        Parse (Wiki    => Wiki,
               Level   => Inline_Level,
               Content => S (Current .. S'Last),
               Index   => Index));
      return To_String (Result);
   end Emphasis;

   -----------
   -- Image --
   -----------

   function Image
     (Wiki : in Wiki_Information; Index : in Positive; S : in String)
      return String
   is
      Extract  : constant Pattern_Matcher
        := Compile
          (Expression =>
               "!(\([\w-_]+?\))??((http://)??[\w._-]+?)(\([\w-_]+?\))??!",
           Flags => Case_Insensitive);

      Matches  : Match_Array (0 .. 5);
      Current  : Natural := S'First;
      Result   : Unbounded_String := Null_Unbounded_String;
   begin
      Extract_All :
      loop
         Match (Self       => Extract,
                Data       => S,
                Matches    => Matches,
                Data_First => Current);

         exit Extract_All when Matches (0) = No_Match;

         if Matches (0).First > Current + 1 then
            Append
              (Result,
               Parse (Wiki    => Wiki,
                      Level   => Inline_Level,
                      Content => S (Current .. Matches (0).First - 1),
                      Index   => Index));
         end if;

         Extract_Image :
         declare
            URL         : constant String
              := Parse (Wiki    => Wiki,
                        Level   => Inline_Level,
                        Content => S (Matches (2).First .. Matches (2).Last));
            Http_Prefix : constant String := "http://";
         begin
            if URL'Length >= Http_Prefix'Length and then
              URL (URL'First ..
                     URL'First + Http_Prefix'Length - 1) = Http_Prefix then
               Append (Result, "<img src='" & URL & "'");
            else
               Append (Result, "<img src='"
                       & Wiki.Img_Base_URL & "/" & URL & "'");
            end if;
         end Extract_Image;

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
      end loop Extract_All;

      if Current = S'First then
         --  No match, try next inline callback
         return Parse (Wiki    => Wiki,
                       Level   => Inline_Level,
                       Content => S,
                       Index   => Index);
      end if;
      Append (Result,
        Parse (Wiki    => Wiki,
               Level   => Inline_Level,
               Content => S (Current .. S'Last),
               Index   => Index));
      return To_String (Result);
   end Image;

   ----------
   -- Link --
   ----------

   function Link
     (Wiki : in Wiki_Information; Index : in Positive; S : in String)
      return String
   is
      Extract  : constant Pattern_Matcher :=
        Compile (Expression => """(\([\w-_]+?\))??([^\(\)]+?)(\(.*\))??"":" &
                 "((http://)??[^ \s\[\]]+)(\s|$)",
                 Flags      => Case_Insensitive + Single_Line);

      Matches  : Match_Array (0 .. 6);
      Current  : Natural := S'First;
      Result   : Unbounded_String := Null_Unbounded_String;
   begin
      Extract_All :
      loop
         Match (Self       => Extract,
                Data       => S,
                Matches    => Matches,
                Data_First => Current);

         exit Extract_All when Matches (0) = No_Match;

         if Matches (0).First > Current + 1 then
            Append
              (Result,
               Parse (Wiki    => Wiki,
                      Level   => Inline_Level,
                      Content => S (Current .. Matches (0).First - 1),
                      Index   => Index));
         end if;

         Extract_Link :
         declare
            URL         : constant String :=
                             S (Matches (4).First .. Matches (4).Last);
            Http_Prefix : constant String := "http://";
         begin
            if URL'Length >= Http_Prefix'Length and then
              URL (URL'First ..
                     URL'First + Http_Prefix'Length - 1) = Http_Prefix then
               Append (Result, "<a href='" & URL & "'");
            elsif URL (URL'First) = '#' then
               Append (Result, "<a href='" & URL & "'");
            else
               Append (Result, "<a href='"
                       & Wiki.Base_URL & "/" & URL & "'");
            end if;
         end Extract_Link;

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
              (Result, '>'
               & Parse (Wiki    => Wiki,
                        Level   => Inline_Level,
                        Content => S (Matches (2).First .. Matches (2).Last),
                        Index   => Index + 1)
               & "</a> ");
         end if;
         Current := Matches (0).Last + 1;
      end loop Extract_All;

      if Current = S'First then
         --  No match, try next inline callback
         return Parse (Wiki    => Wiki,
                       Level   => Inline_Level,
                       Content => S,
                       Index   => Index);
      end if;
      Append (Result,
        Parse (Wiki    => Wiki,
               Level   => Inline_Level,
               Content => S (Current .. S'Last),
               Index   => Index));
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
      Diouzhtu.Internal_Register (Inline_Level, Link'Access);

      --  Single Link should always comes just after Link to be skipped when
      --  parsing link description

      Diouzhtu.Internal_Register (Inline_Level, Single_Link'Access);
      Diouzhtu.Internal_Register (Inline_Level, Image'Access);
      Diouzhtu.Internal_Register (Inline_Level, Emphasis'Access);
      Diouzhtu.Internal_Register (Inline_Level, Strong'Access);
      Diouzhtu.Internal_Register (Inline_Level, Default'Access);
   end Register;

   -----------------
   -- Single_Link --
   -----------------

   function Single_Link
     (Wiki : in Wiki_Information; Index : in Positive; S : in String)
      return String
   is
      Extract : constant Pattern_Matcher :=
        Compile (Expression => "(http://[^ \s\[\]]+)(\s|$)",
                 Flags      => Case_Insensitive + Single_Line);

      Matches : Match_Array (0 .. 2);
      Current : Natural := S'First;
      Result  : Unbounded_String := Null_Unbounded_String;
   begin
      Extract_All :
      loop
         Match (Self       => Extract,
                Data       => S,
                Matches    => Matches,
                Data_First => Current);

         exit Extract_All when Matches (0) = No_Match;

         if Matches (1).First > Current + 1 then
            Append
              (Result,
               Parse (Wiki    => Wiki,
                      Level   => Inline_Level,
                      Content => S (Current .. Matches (1).First - 1),
                      Index   => Index));
         end if;

         Extract_Single_Link :
         declare
            End_Content : constant Character := S (Matches (1).Last);
            In_Content : constant String :=
              S (Matches (1).First .. Matches (1).Last);
         begin
            if not Characters.Handling.Is_Alphanumeric (End_Content) then
               --  Try to fix url
               Append (Result, "<a href="""
                       & In_Content (In_Content'First .. In_Content'Last - 1)
                       & """>"
                       & In_Content (In_Content'First .. In_Content'Last - 1)
                       & "</a>" & End_Content);
            else
               Append (Result, "<a href="""
                       & In_Content & """>" & In_Content & "</a> ");
            end if;
         end Extract_Single_Link;
         Current := Matches (1).Last + 2;
      end loop Extract_All;

      Append (Result,
        Parse (Wiki    => Wiki,
               Level   => Inline_Level,
               Content => S (Current .. S'Last),
               Index   => Index));
      return To_String (Result);
   exception
      when others => return "";
   end Single_Link;

   ------------
   -- Strong --
   ------------

   function Strong
        (Wiki : in Wiki_Information; Index : in Positive; S : in String)
      return String
   is
      Extract  : constant Pattern_Matcher :=
        Compile (Expression => "\*(.*?)\*", Flags => Case_Insensitive);
      Matches  : Match_Array (0 .. 1);
      Current  : Natural := S'First;
      Result   : Unbounded_String := Null_Unbounded_String;
   begin
      Extract_All :
      loop
         Match (Self       => Extract,
                Data       => S,
                Matches    => Matches,
                Data_First => Current);

         exit Extract_All when Matches (0) = No_Match;

         if Matches (1).First > Current + 1 then
            Append
              (Result,
               Parse (Wiki    => Wiki,
                      Level   => Inline_Level,
                      Content => S (Current .. Matches (1).First - 2),
                      Index   => Index));
         end if;

         Extract_Strong :
         declare
            In_Content : constant String :=
                           Parse (Wiki    => Wiki,
                                  Level   => Inline_Level,
                                  Content =>
                                    S (Matches (1).First .. Matches (1).Last),
                                  Index   => Index);
         begin
            Append (Result, "<strong>" & In_Content & "</strong>");
         end Extract_Strong;
         Current := Matches (1).Last + 2;
      end loop Extract_All;

      Append (Result,
        Parse (Wiki    => Wiki,
               Level   => Inline_Level,
               Content => S (Current .. S'Last),
               Index   => Index));
      return To_String (Result);
   end Strong;

end Diouzhtu.Inline;

