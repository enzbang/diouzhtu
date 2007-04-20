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
with Ada.Text_IO;
with Ada.Directories;

with AWS.MIME;
with AWS.Messages;
with AWS.Parameters;
with AWS.Services.ECWF.Registry;

with Gwiad.Services.Register;

with Wiki_Interface;

with Wiki_Website.Config;
with Wiki_Website.Template_Defs.Edit;
with Wiki_Website.Template_Defs.View;
with Wiki_Website.Template_Defs.Preview;

package body Wiki_Website.Callbacks is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   use Wiki_Website;
   use Wiki_Website.Config;
   use Wiki_Interface;

   ----------------------
   -- Default_Callback --
   ----------------------

   function Default_Callback (Request : in Status.Data) return Response.Data is
      use type Messages.Status_Code;
      URI          : constant String := Status.URI (Request);
      Translations : Templates.Translate_Set;
      Web_Page     : Response.Data;
   begin

      Web_Page := AWS.Services.ECWF.Registry.Build
        (URI, Request, Translations, Cache_Control => Messages.Prevent_Cache);

      if Response.Status_Code (Web_Page) = Messages.S404 then
         --  Page not found
         return Response.Build
           (Content_Type  => MIME.Text_HTML,
            Message_Body  => "<p>Error</p>");
      else
         return Web_Page;
      end if;
   end Default_Callback;

   ---------------
   -- Edit_Page --
   ---------------

   procedure Edit_Page
     (Request      : in     Status.Data;
      Context      : access AWS.Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      use AWS.Status;
      pragma Unreferenced (Context);

      Get_URI  : constant String := URI (Request);
      Name     : constant String := Get_Filename (Get_URI);
      Filename : constant String := Wiki_Text_Dir & "/" & Name;

      Text_Plain : Unbounded_String;
      Text_File  : File_Type;

   begin

      if not Gwiad.Services.Register.Exists (Wiki_Service_Name) then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Edit.ERROR, "<p>Service down</p>"));
         return;
      end if;

      if Ada.Directories.Exists (Filename) then
         Open (File => Text_File,
               Mode => In_File,
               Name => Filename);

         while not End_Of_File (File => Text_File) loop
            Append (Text_Plain, Get_Line (Text_File));
            Append (Text_Plain, ASCII.LF);
         end loop;

         Close (File => Text_File);
      end if;

      Templates.Insert
        (Translations,
         Templates.Assoc (Template_Defs.Edit.TEXT_PLAIN, Text_Plain));
      Templates.Insert
        (Translations,
         Templates.Assoc (Template_Defs.Edit.FILENAME, Name));
   end Edit_Page;

   -------------------
   -- Edit_Template --
   -------------------

   function Edit_Template (Request : in Status.Data) return String is
      pragma Unreferenced (Request);
   begin
      return Template_Defs.Edit.Template;
   end Edit_Template;

   ------------------
   -- Preview_Page --
   ------------------

   procedure Preview_Page
     (Request      : in     Status.Data;
      Context      : access AWS.Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      P          : constant Parameters.List := Status.Parameters (Request);
      Save       : constant String          :=
                     Parameters.Get (P, Template_Defs.Preview.HTTP.save);
      Text_Plain : constant String          :=
                     Parameters.Get (P, Template_Defs.Preview.HTTP.text_plain);

      Get_URI    : constant String := Status.URI (Request);
      Name       : constant String := Get_Filename (Get_URI);

   begin
      Ada.Text_IO.Put_Line (Text_Plain);

      if not Gwiad.Services.Register.Exists (Wiki_Service_Name) then
         Templates.Insert
           (Translations,
            Templates.Assoc
              (Template_Defs.Preview.ERROR, "<p>Service down</p>"));
         return;
      end if;

      declare
         Get_Service : GW_Service'Class := Get_Wiki_Service;
      begin
         if Save /= "" then
            declare
               Text_File : File_Type;
               Filename  : constant String := Wiki_Text_Dir & "/" & Name;
            begin

               Ada.Text_IO.Put_Line (Filename);

               if Ada.Directories.Exists (Filename) then
                  --  Here we should add RCS

                  Ada.Directories.Delete_File (Filename);
               end if;

               Ada.Text_IO.Put_Line (Filename);

               Create (File => Text_File,
                       Mode => Out_File,
                       Name => Filename);

               Put (File => Text_File,
                    Item => Text_Plain);

               Close (File => Text_File);
            end;
            Templates.Insert
              (Translations,
               Templates.Assoc (Template_Defs.Preview.HAS_BEEN_SAVED, Name));
         else
            Templates.Insert
              (Translations,
               Templates.Assoc
                 (Template_Defs.Preview.PREVIEW,
                  HTML_Preview (Get_Service, Text_Plain)));
            Templates.Insert
              (Translations,
               Templates.Assoc (Template_Defs.Preview.TEXT_PLAIN, Text_Plain));
            Templates.Insert
              (Translations,
               Templates.Assoc (Template_Defs.Preview.FILENAME, Name));
         end if;
      end;
   end Preview_Page;

   ----------------------
   -- Preview_Template --
   ----------------------

   function Preview_Template (Request : in Status.Data) return String is
      pragma Unreferenced (Request);
   begin
      return Template_Defs.Preview.Template;
   end Preview_Template;

   -------------------
   -- View_Template --
   -------------------

   function View_Template (Request : in Status.Data) return String is
      pragma Unreferenced (Request);
   begin
      return Template_Defs.View.Template;
   end View_Template;

end Wiki_Website.Callbacks;
