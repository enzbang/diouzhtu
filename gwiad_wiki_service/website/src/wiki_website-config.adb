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
with Ada.Strings.Unbounded;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Fixed;

package body Wiki_Website.Config is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   type Wiki_Data is record
      Host_Name : Unbounded_String;
      Web_Root  : Unbounded_String;
   end record;

   package Config_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Wiki_Data, Strings.Hash, "=", "=");
   use Config_Maps;

   Configs : Map;

   ----------------
   -- Add_Config --
   ----------------

   procedure Add_Config
     (Name : Wiki_Name; Hostname : String; Web_Root : String)
   is
   begin
      Configs.Insert (Key       => String (Name),
                      New_Item  =>
                        (Host_Name => To_Unbounded_String (Hostname),
                         Web_Root  => To_Unbounded_String (Web_Root)));
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

   function Get_Wiki_Name (Request : AWS.Status.Data) return Wiki_Name is
      function Get_Hostname (Hostname : String) return String;
      --  Get hostname

      function Match_URI (URI, Web_Root : String) return Boolean;
      --  Test if the web root element match the given URI

      function Get_Hostname (Hostname : String) return String is
         K : Natural;
      begin
         K := Strings.Fixed.Index (Hostname, ":");

         if K = 0 then
            K := Hostname'Last;
         else
            K := K - 1;
         end if;
         return Hostname (Hostname'First .. K);
      end Get_Hostname;


      ---------------
      -- Match_URI --
      ---------------

      function Match_URI (URI, Web_Root : String) return Boolean is
      begin
         if URI'Length >= Web_Root'Length and then
           URI (URI'First .. URI'First + Web_Root'Length - 1) = Web_Root
         then
            return True;
         else
            return False;
         end if;
      end Match_URI;

      URI      : constant String := AWS.Status.URI (Request);
      Hostname : constant String := Get_Hostname (AWS.Status.Host (Request));
      Position : Cursor := No_Element;
   begin

      Position := Configs.First;
      while Position /= No_Element loop
         declare
            Wiki : constant Wiki_Data := Element (Position);
         begin
            if Wiki.Host_Name = Hostname
              and then Match_URI (URI, To_String (Wiki.Web_Root)) then
               Ada.Text_IO.Put_Line ("found wiki name " & Key (Position)
                                     & " for "
                                     & Hostname & ", " & URI);

               return Wiki_Name (Key (Position));
            end if;
            if Element (Position).Host_Name /= Hostname then
               Ada.Text_IO.Put_Line ("Hostname : "
                                     & To_String (Element (Position).Host_Name)
                                     & "!=" & Hostname);
            end if;
         end;
         Next (Position);
      end loop;

      Ada.Text_IO.Put_Line ("err "   & Hostname & ", " & URI);

      raise Wiki_Config_Exception with Hostname & URI;
   end Get_Wiki_Name;

   -----------------------
   -- Get_Wiki_Web_Root --
   -----------------------

   function Get_Wiki_Web_Root (Name : Wiki_Name) return String is
   begin
      return To_String (Configs.Element (String (Name)).Web_Root);
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

   ---------------
   -- Wiki_Host --
   ---------------

   function Wiki_Host (Name : Wiki_Name) return String is
   begin
      return To_String (Configs.Element (String (Name)).Host_Name);
   end Wiki_Host;

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
