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
with Ada.Characters.Handling;
with Ada.Text_IO;

with Diouzhtu.Block;
with Diouzhtu.Code;
with Diouzhtu.Inline;

package body Diouzhtu.To_HTML is

   use Ada;
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   function CR_Delete (S : in String) return String;
   --  Delete all CR characters

   function Web_Escape (S : in String) return String;
   --  Escape web characters

   ---------------
   -- CR_Delete --
   ---------------

   function CR_Delete (S : in String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for K in S'Range loop
         if S (K) /= ASCII.CR then
            Append (Result, S (K));
         end if;
      end loop;
      return To_String (Result);
   end CR_Delete;

   ------------------
   -- Text_To_HTML --
   ------------------

   function Text_To_HTML
     (Wiki : in Wiki_Information; Source : in String) return String is
      Text : constant String := CR_Delete (Source);

      Content       : Unbounded_String := Null_Unbounded_String;
      Parse_Result  : Unbounded_String := Null_Unbounded_String;
      Result        : Unbounded_String := Null_Unbounded_String;
      Last          : Positive := Text'First;

      In_Code_Block  : Boolean := False;

   begin

      for K in Text'Range loop
         if Text (K) = ASCII.Lf then
            if Last < K - 1 then
               if Content /= Null_Unbounded_String then
                  Append (Content, ASCII.Lf);
               end if;
               Append (Content, Web_Escape (Text (Last .. K - 1)));
            else
               if Content /= Null_Unbounded_String then
                  Code.Parse (Wiki, To_String (Content),
                              In_Code_Block, Parse_Result);
                  Append (Result, Parse_Result);
                  Parse_Result := Null_Unbounded_String;
                  Content := Null_Unbounded_String;
               end if;
            end if;
            Last := K + 1;
         end if;
      end loop;

      if Last < Text'Last then
         Append (Content, Web_Escape (Text (Last .. Text'Last)));
      end if;

      if Content /= Null_Unbounded_String then
         Code.Parse (Wiki, To_String (Content),
                     In_Code_Block, Parse_Result);
         Append (Result, Parse_Result);
      end if;

      return To_String (Result);
   end Text_To_HTML;

   -------------
   -- To_HTML --
   -------------

   function To_HTML
     (Wiki : in Wiki_Information; Filename : in String) return String
   is
      Diouzhtu_File : File_Type;
      Result        : Unbounded_String := Null_Unbounded_String;
   begin

      Open (File => Diouzhtu_File,
            Mode => In_File,
            Name => To_String (Wiki.Text_Directory) & "/" & Filename);

      while not End_Of_File (Diouzhtu_File) loop
         Append (Result, Get_Line (Diouzhtu_File));
         Append (Result, ASCII.Lf);
      end loop;

      Close (Diouzhtu_File);

      return Text_To_HTML (Wiki, To_String (Result));
   exception
      when others =>
         if Is_Open (Diouzhtu_File) then
            Close (Diouzhtu_File);
         end if;
         raise;
   end To_HTML;

   ----------------
   -- Web_Escape --
   ----------------

   function Web_Escape (S : in String) return String is

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
      Escape :
      for I in S'Range loop
         if To_Skip /= 0 then
            To_Skip := To_Skip - 1;
         else
            case S (I) is
               when '&'    =>
                  XML_Entities :
                  for K in I + 1 .. S'Last loop
                     if not Ada.Characters.Handling.Is_Letter (S (K)) then
                        if S (K) /= ';' then
                           Append_To_Result ("&amp;", Last, I - 1);
                        else
                           --  Allow xml entities &copy; &reg; &quot; ...
                           To_Skip := K - I;
                        end if;
                        exit XML_Entities;
                     end if;
                  end loop XML_Entities;
               when '>'    => Append_To_Result ("&gt;", Last, I - 1);
               when '<'    => Append_To_Result ("&lt;", Last, I - 1);
               --  when '"'    => Append_To_Result ("&quot;", Last, I - 1);
               when others => null;
            end case;
         end if;
      end loop Escape;

      if Last <= S'Last then
         Append (Result, S (Last .. S'Last));
      end if;

      return To_String (Result);
   end Web_Escape;

begin  -- Diouzhtu.TO_HTML Register block and inline transformation
   Diouzhtu.Block.Register;
   Diouzhtu.Inline.Register;
end Diouzhtu.To_HTML;
