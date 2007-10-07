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

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Gwiad.Web.Virtual_Host;
with Gwiad.Plugins.Websites.Registry;
with Morzhol.Strings;
with Gwiad.Plugins.Websites;

package body Wiki_Website.Config is

   use Ada;
   use Ada.Strings.Unbounded;
   use Morzhol.Strings;

   type Wiki_Data is record
      Host_Name : Unbounded_String;
   end record;

   package Config_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Wiki_Data,
      Hash            => Strings.Hash,
      Equivalent_Keys => "=",
      "="             => "=");
   use Config_Maps;

   Configs : Map;

   ----------------
   -- Add_Config --
   ----------------

   procedure Add_Config (Name : in Wiki_Name; Hostname : in String)
   is
   begin
      Configs.Insert (Key       => String (Name),
                      New_Item  => Wiki_Data'(Host_Name => +Hostname));
   end Add_Config;

   -----------------------
   -- Delete_All_Config --
   -----------------------

   procedure Delete_All_Config is
      Position : Cursor := Configs.First;
   begin
      while Has_Element (Position) loop
         Gwiad.Plugins.Websites.Registry.Unregister
           (Gwiad.Plugins.Websites.Website_Name (Key (Position)));
            Configs.Delete (Key (Position));
         Position := Configs.First;
      end loop;
   end Delete_All_Config;

   -------------------
   -- Get_Directory --
   -------------------

   function Get_Directory (URI : in String) return String
   is
      Filename : constant String := Get_Filename (URI);
   begin
      if Filename = "" or else Filename (Filename'Last) = '/' then
         return Filename;
      else
         return Directories.Containing_Directory (Filename) & '/';
      end if;
   end Get_Directory;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (URI : in String) return String is
      Name : constant String := URI (URI'First + 1 .. URI'Last);
   begin
      if Name = Wiki_Web_Edit or else Name = Wiki_Web_Preview then
         return "";
      end if;

      if Name'Length > Wiki_Web_Edit'Length and then
        Name (Name'First ..
                Name'First + Wiki_Web_Edit'Length - 1) = Wiki_Web_Edit
      then
         return Name (Name'First + Wiki_Web_Edit'Length + 1 .. Name'Last);
      end if;

      if Name'Length > Wiki_Web_Preview'Length and then
        Name (Name'First ..
                Name'First + Wiki_Web_Preview'Length - 1) = Wiki_Web_Preview
      then
         return Name (Name'First +
                        Wiki_Web_Preview'Length + 1 .. Name'Last);
      end if;

      return Name;
   end Get_Filename;

   -------------------
   -- Get_Wiki_Name --
   -------------------

   function Get_Wiki_Name (Request : in AWS.Status.Data) return Wiki_Name is
      URI      : constant String := AWS.Status.URI (Request);
      Hostname : constant String :=
                   Gwiad.Web.Virtual_Host.Get_Hostname
                     (AWS.Status.Host (Request));
      Position : Cursor := No_Element;
   begin

      Position := Configs.First;
      while Position /= No_Element loop
         Check_Name :
         declare
            Wiki : constant Wiki_Data := Wiki_Data (Element (Position));
         begin
            if Wiki.Host_Name = Hostname then
               return Wiki_Name (Key (Position));
            end if;
         end Check_Name;
         Next (Position);
      end loop;

      Ada.Text_IO.Put_Line ("err "   & Hostname & ", " & URI);

      raise Wiki_Config_Exception with Hostname & URI;
   end Get_Wiki_Name;

   -------------------
   -- Wiki_CSS_Root --
   -------------------

   function Wiki_CSS_Root (Name : in Wiki_Name) return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Wiki_Root (Name),
         Name                 => "css");
   end Wiki_CSS_Root;

   --------------------
   -- Wiki_Data_Root --
   --------------------

   function Wiki_Data_Root (Name : in Wiki_Name) return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Wiki_Root (Name),
         Name                 => "data");
   end Wiki_Data_Root;

   ---------------
   -- Wiki_Host --
   ---------------

   function Wiki_Host (Name : in Wiki_Name) return String is
      Element : Wiki_Data := Wiki_Data (Configs.Element (String (Name)));
   begin
      return To_String (Element.Host_Name);
   end Wiki_Host;

   -------------------
   -- Wiki_HTML_Dir --
   -------------------

   function Wiki_HTML_Dir (Name : in Wiki_Name) return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Wiki_Data_Root (Name),
         Name                 => "html");
   end Wiki_HTML_Dir;

   --------------------
   -- Wiki_Image_Dir --
   --------------------

   function Wiki_Image_Dir (Name : in Wiki_Name) return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Wiki_Data_Root (Name),
         Name                 => "image");
   end Wiki_Image_Dir;

   ------------------
   -- Wiki_JS_Root --
   ------------------

   function Wiki_JS_Root (Name : in Wiki_Name) return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Wiki_Root (Name),
         Name                 => "js");
   end Wiki_JS_Root;

   ---------------
   -- Wiki_Root --
   ---------------

   function Wiki_Root (Name : in Wiki_Name) return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Plugin_Root,
         Name                 => String (Name));
   end Wiki_Root;

   -------------------
   -- Wiki_Text_Dir --
   -------------------

   function Wiki_Text_Dir (Name : in Wiki_Name) return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Wiki_Data_Root (Name),
         Name                 => "text");
   end Wiki_Text_Dir;

end Wiki_Website.Config;
