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
with Ada.Exceptions;

with AWS.Services.Directory;

with Wiki_Website.Config;
with Wiki_Website.Template_Defs.Block_View;

with Ada.Directories;
with Ada.Text_IO;

with Gwiad.Services.Register;

with Wiki_Interface;

package body Wiki_Website.ECWF_Callbacks is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   use Wiki_Interface;

   use Wiki_Website;
   use Wiki_Website.Config;

   ----------
   -- View --
   ----------

   procedure View
     (Request      : in     Status.Data;
      Context      : access AWS.Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      use AWS.Status;
      use Ada.Directories;

      Get_URI       : constant String := URI (Request);
      Filename      : constant String := Get_Filename (Get_URI);
      HTML_Filename : constant String :=  Wiki_HTML_Dir & "/" & Filename;

      HTML_Text : Unbounded_String := Null_Unbounded_String;
      HTML_File : File_Type;
   begin

      Templates.Insert
        (Translations,
         Templates.Assoc (Template_Defs.Block_View.FILENAME, Filename));

      if Exists (HTML_Filename) then
         if Kind (HTML_Filename) /= Ordinary_File then
            if Kind (HTML_Filename) = Directory then
               Templates.Insert
                 (Translations,
                  Templates.To_Set (AWS.Services.Directory.Browse
                    (Directory_Name => HTML_Filename,
                     Request        => Request)));
            end if;
            return;
         end if;

         Open (File => HTML_File,
               Mode => In_File,
               Name => HTML_Filename);

         while not End_Of_File (HTML_File) loop
            Append (HTML_Text, Get_Line (HTML_File));
            Append (HTML_Text, ASCII.LF);
         end loop;

         Close (HTML_File);

         Templates.Insert
           (Translations, Templates.Assoc
              (Template_Defs.Block_View.VIEW, HTML_Text));

      elsif Exists (Wiki_Text_Dir & "/" & Filename)
        and then Kind (Wiki_Text_Dir & "/" & Filename) = Ordinary_File
      then
         if not Gwiad.Services.Register.Exists (Wiki_Service_Name) then
            Templates.Insert
              (Translations,
               Templates.Assoc ("ERROR", "<p>Service down</p>"));
         end if;

         declare
            Get_Service : GW_Service'Class := Get_Wiki_Service;
            New_HTML    : constant String  := HTML (Get_Service, Filename);
         begin
            Templates.Insert
              (Translations,
               Templates.Assoc (Template_Defs.Block_View.VIEW, New_HTML));

            Create_Path (Containing_Directory (HTML_Filename));

            Create (File => HTML_File,
                    Mode => Out_File,
                    Name => HTML_Filename);

            Put (HTML_File, New_HTML);

            Close (HTML_File);
         end;
      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
            Templates.Insert
              (Translations, Templates.Assoc ("ERROR", "<p>Service down</p>"));
   end View;

end Wiki_Website.ECWF_Callbacks;
