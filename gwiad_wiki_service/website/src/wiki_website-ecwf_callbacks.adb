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

with GNAT.Calendar.Time_IO;

with Ada.Strings.Unbounded;

with AWS.Services.Directory;

with Wiki_Website.Config;
with Wiki_Website.Service;
with Wiki_Website.Template_Defs.Bottom;
with Wiki_Website.Template_Defs.Block_View;

with Ada.Directories;
with Ada.Text_IO;

with Gwiad.OS;
with Gwiad.Services.Register;

with Wiki_Interface;

package body Wiki_Website.ECWF_Callbacks is

   use Ada;
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
      Name          : constant Wiki_Name :=
                        Get_Wiki_Name (Hostname => Host (Request),
                                       URI      => Get_URI);
      Web_Root      : constant String := Get_Wiki_Web_Root (Name);
      Filename      : constant String := Get_Filename (Web_Root, Get_URI);
      HTML_Filename : constant String :=
                        Wiki_HTML_Dir (Name)
                        & Gwiad.OS.Directory_Separator & Filename;
      HTML_Text     : Unbounded_String := Null_Unbounded_String;
      HTML_File     : File_Type;

   begin

      Ada.Text_IO.Put_Line ("View");
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

         Ada.Text_IO.Put_Line ("ViewHMTL " & HTML_Filename);

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

      elsif Exists (Wiki_Text_Dir (Name) & "/" & Filename)
        and then Kind (Wiki_Text_Dir (Name)
                       & "/" & Filename) = Ordinary_File
      then
         Ada.Text_IO.Put_Line ("VIEW FILE "& Filename);

         if not Gwiad.Services.Register.Exists (Wiki_Service_Name) then
            Templates.Insert
              (Translations,
               Templates.Assoc ("ERROR", "<p>Service down</p>"));
         end if;

         declare
            Get_Service : GW_Service'Class := Service.Get (Name, Web_Root);
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

      if Exists (HTML_Filename) then
         Templates.Insert
           (Translations,
            Templates.Assoc (Template_Defs.Bottom.MODIFICATION_DATE,
              GNAT.Calendar.Time_IO.Image
                (Directories.Modification_Time
                   (Name => HTML_Filename), "%Y-%m-%d %T")));
      end if;
   end View;

   -------------------
   -- View_Template --
   -------------------

   function View_Template (Request : in Status.Data) return String is
      Get_URI  : constant String    := Status.URI (Request);
      Name     : constant Wiki_Name :=
                   Get_Wiki_Name (Hostname => Status.Host (Request),
                                  URI      => Get_URI);
   begin
      return Wiki_Root (Name) & Gwiad.OS.Directory_Separator
        & Template_Defs.Block_View.Template;
   end View_Template;

end Wiki_Website.ECWF_Callbacks;
