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
with Ada.Exceptions;

with Ada.Directories;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package body Wiki_Website.Config is

   use Ada.Exceptions;
   use Ada;

   Plugin_Root : constant String :=
                   Directories.Compose ("plugin", "wiki_website");

   package Config_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Wiki_Name, Ada.Strings.Hash, "=", "=");
   use Config_Maps;

   Configs : Map;

   ----------------
   -- Add_Config --
   ----------------

   procedure Add_Config (Name : Wiki_Name; Web_Root : String) is
   begin
      Configs.Insert (Key       => Web_Root,
                      New_Item  => Name);
   end Add_Config;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename
     (Wiki_Web_Root : String; URI : String) return String is
   begin
      declare
         Name          : constant String
           := URI (URI'First - 1 + Wiki_Web_Root'Length + 2 .. URI'Last);
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
      end;
   exception
      when E : others => Ada.Text_IO.Put_Line
           ("(Get_Filename) : Error " & Exception_Information (E));
         raise Wiki_Config_Exception;
   end Get_Filename;

   -------------------
   -- Get_Wiki_Name --
   -------------------

   function Get_Wiki_Name (Wiki_Web_Root : String) return Wiki_Name is
   begin
      if Configs.Contains (Wiki_Web_Root) then
         return Configs.Element (Wiki_Web_Root);
      end if;
      raise Wiki_Config_Exception;
   end Get_Wiki_Name;

   -----------------------
   -- Get_Wiki_Web_Root --
   -----------------------

   function Get_Wiki_Web_Root (URI : String) return String is
      Position : Cursor := Configs.First;

      function Match_URI return Boolean;
      --  Test if the web root element match the given URI

      ---------------
      -- Match_URI --
      ---------------

      function Match_URI return Boolean is
         Web_Root : constant String := Key (Position);
      begin
         if URI'Length >= Web_Root'Length and then
           URI (URI'First .. URI'First + Web_Root'Length - 1) = Web_Root
         then
            return True;
         else
            return False;
         end if;
      end Match_URI;

   begin
      while Position /= No_Element and then not Match_URI loop
         Next (Position);
      end loop;

      if Position = No_Element then
         raise Wiki_Config_Exception;
      end if;

      return Key (Position);
   end Get_Wiki_Web_Root;

   -----------------------
   -- Get_Wiki_Web_Root --
   -----------------------

   function Get_Wiki_Web_Root (Name : Wiki_Name) return String is
      Position : Cursor := Configs.First;
   begin
      while Position /= No_Element loop
         if Element (Position) = Name then
            return Key (Position);
         end if;
         Next (Position);
      end loop;
      raise Wiki_Config_Exception;
   end Get_Wiki_Web_Root;

   -------------------
   -- Wiki_CSS_Root --
   -------------------

   function Wiki_CSS_Root (Name : Wiki_Name) return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Wiki_Root (Name),
         Name                 => "css");
   end Wiki_CSS_Root;

   --------------------
   -- Wiki_Data_Root --
   --------------------

   function Wiki_Data_Root (Name : Wiki_Name) return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Wiki_Root (Name),
         Name                 => "data");
   end Wiki_Data_Root;

   -------------------
   -- Wiki_HTML_Dir --
   -------------------

   function Wiki_HTML_Dir (Name : Wiki_Name) return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Wiki_Data_Root (Name),
         Name                 => "html");
   end Wiki_HTML_Dir;

   --------------------
   -- Wiki_Image_Dir --
   --------------------

   function Wiki_Image_Dir (Name : Wiki_Name) return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Wiki_Data_Root (Name),
         Name                 => "image");
   end Wiki_Image_Dir;

   ------------------
   -- Wiki_JS_Root --
   ------------------

   function Wiki_JS_Root (Name : Wiki_Name) return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Wiki_Root (Name),
         Name                 => "js");
   end Wiki_JS_Root;

   ---------------
   -- Wiki_Root --
   ---------------

   function Wiki_Root (Name : Wiki_Name) return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Plugin_Root,
         Name                 => String (Name));
   end Wiki_Root;

   -------------------
   -- Wiki_Text_Dir --
   -------------------

   function Wiki_Text_Dir (Name : Wiki_Name) return String is
   begin
      return Ada.Directories.Compose
        (Containing_Directory => Wiki_Data_Root (Name),
         Name                 => "text");
   end Wiki_Text_Dir;

end Wiki_Website.Config;
