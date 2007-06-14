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
with AWS.Dispatchers.Callback;
with AWS.Services.ECWF.Registry;
with AWS.Services.Dispatchers.URI;
with AWS.MIME;

with Morzhol.OS;
with Morzhol.Iniparser;

with Gwiad.Registry.Websites.Register;
with Gwiad.Registry.Services.Register;
with Gwiad.Web.Register.Virtual_Host;

with Wiki_Website.Callbacks;
with Wiki_Website.ECWF_Callbacks;

with Wiki_Website.Template_Defs.Edit;
with Wiki_Website.Template_Defs.View;
with Wiki_Website.Template_Defs.Preview;


package body Wiki_Website.Service is

   use AWS;
   use Ada;
   use Ada.Exceptions;
   use Morzhol.OS;

   use Wiki_Website.Callbacks;
   use Wiki_Website.ECWF_Callbacks;
   use Gwiad.Registry.Services;
   use Gwiad.Registry.Services.Register;

   package Service_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Service_Id, Ada.Strings.Hash, "=", "=");

   Services : Service_Maps.Map;

   type Attribute is (Description, Virtual_Host);

   package Conf is new Morzhol.Iniparser (Attribute);

   procedure Discover_Wiki_Websites;
   --  Search wiki website on plugin root path

   ----------------------------
   -- Discover_Wiki_Websites --
   ----------------------------

   procedure Discover_Wiki_Websites is
      use Ada.Directories;
      S : Search_Type;
      D : Directory_Entry_Type;
   begin
      Start_Search (Search    => S,
                    Directory => Wiki_Website.Config.Plugin_Root,
                    Pattern   => "*",
                    Filter    => (Directory => True, others => False));

      while More_Entries (S) loop
         Get_Next_Entry (S, D);
         declare
            Path : constant String    := Full_Name (D);
            Name : constant Wiki_Name := Wiki_Name (Simple_Name (D));
         begin
            if Name /= "." and Name /= ".." then
               --  Now read the config file if any

               Conf.IO.Open
                 (Path & Directory_Separator & "config.ini");
               Conf.IO.Close;

               Wiki_Website.Service.Register
                 (Name          => Name,
                  Virtual_Host  => Conf.Get_Value (Virtual_Host),
                  Description   => Conf.Get_Value (Description));
            end if;
         exception
            when Conf.IO.Uncomplete_Config =>
               Ada.Text_IO.Put_Line ("uncomplete");
               Conf.IO.Close;
            when UP : Conf.IO.Unknown_Parameter =>
               Text_IO.Put_Line ("Unknown Parameter : "
                                 & Exceptions.Exception_Message (UP));
               Conf.IO.Close;
            when Text_IO.Name_Error =>
               Ada.Text_IO.Put_Line ("Does not exit");
               null;
         end;
      end loop;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Exception_Information (E));
   end Discover_Wiki_Websites;

   ---------
   -- Get --
   ---------

   function Get (Name : Wiki_Name) return Wiki_Interface.GW_Service'Class
   is
      use Wiki_Interface;
   begin

      if not Services.Contains (String (Name)) then
         declare
            Wiki_Service_Id : Service_Id;
            Wiki_World_Service_Access : constant GW_Service_Access
              := GW_Service_Access (Gwiad.Registry.Services.Register.Get
                                    (Wiki_Service_Name));
            Get_Service               : GW_Service'Class :=
                                          Wiki_World_Service_Access.all;
         begin
            Initialize
              (S              =>
                 GW_Service'Class (Wiki_World_Service_Access.all),
               Base_URL       => "/",
               Img_Base_URL   => "/" & Wiki_Web_Image,
               Text_Directory => Wiki_Text_Dir (Name));

            Wiki_Service_Id := Gwiad.Registry.Services.Register.Set
              (Wiki_Service_Name, Service_Access (Wiki_World_Service_Access));
            Services.Insert (Key       => String (Name),
                             New_Item  => Wiki_Service_Id);

            return Get_Service;
         end;
      else
         declare
            Wiki_Service_Id : constant Service_Id :=
                                Services.Element (String (Name));
            Wiki_World_Service_Access : constant GW_Service_Access
              := GW_Service_Access
              (Gwiad.Registry.Services.Register.Get
                 (Wiki_Service_Name,
                  Wiki_Service_Id));
            Get_Service               : GW_Service'Class :=
                                          Wiki_World_Service_Access.all;
         begin
            return Get_Service;
         end;
      end if;
   exception
      when E : others => Ada.Text_IO.Put_Line (Exception_Information (E));
         raise;
   end Get;

   procedure Register
     (Virtual_Host  : String; Name : Wiki_Name; Description : String)
   is
      Template_Dir : constant String    := Wiki_Root (Name);
      Sep          : constant Character := Directory_Separator;

      Main_Dispatcher : AWS.Services.Dispatchers.URI.Handler;
   begin

      Ada.Text_IO.Put_Line (Template_Dir);

      AWS.Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         "/" & Wiki_Web_Image,
         Action => Dispatchers.Callback.Create (Image_Callback'Access),
         Prefix => True);

      AWS.Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         "/" & Wiki_Web_CSS,
         Action => Dispatchers.Callback.Create (CSS_Callback'Access),
         Prefix => True);

      AWS.Services.Dispatchers.URI.Register
        (Main_Dispatcher,
         "/" & Wiki_Web_JS,
         Action => Dispatchers.Callback.Create (JS_Callback'Access),
         Prefix => True);

      AWS.Services.Dispatchers.URI.Register_Default_Callback
        (Main_Dispatcher,
         Dispatchers.Callback.Create (Default_Callback'Access));
      --  This default callback will handle all ECWF callbacks

      --  Register ECWF pages

      AWS.Services.ECWF.Registry.Register
        (Key          => "/" & Wiki_Web_Edit,
         Template     => Template_Dir & Sep & Template_Defs.Edit.Template,
         Data_CB      => Edit_Page'Access,
         Content_Type => MIME.Text_HTML,
         Prefix       => True);

      AWS.Services.ECWF.Registry.Register
        (Key          => "/" & Wiki_Web_Preview,
         Template     => Template_Dir & Sep & Template_Defs.Preview.Template,
         Data_CB      => Preview_Page'Access,
         Content_Type => MIME.Text_HTML,
         Prefix       => True);

      AWS.Services.ECWF.Registry.Register
        (Key          => "/",
         Template     => Template_Dir & Sep & Template_Defs.View.Template,
         Data_CB      => View'Access,
         Content_Type => MIME.Text_HTML,
         Prefix       => True);

      Config.Add_Config (Name => Name, Hostname => Virtual_Host);

      Gwiad.Web.Register.Virtual_Host.Register
        (Hostname => Virtual_Host,
         Action   => Main_Dispatcher);

      Gwiad.Registry.Websites.Register.Register
        (Name        => String (Name),
         Description => Description,
         Unregister  => Unregister'Access);
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("registering fails : " &
                               Exception_Information (E));
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Website_Name : in String) is
      Host     : constant String := Wiki_Host (Wiki_Name (Website_Name));
   begin
      Gwiad.Web.Register.Virtual_Host.Unregister (Hostname => Host);
   end Unregister;

begin
   --  Load all wiki websites

   Discover_Wiki_Websites;

end Wiki_Website.Service;
