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
with Ada.Directories;

with AWS.Status;
with AWS.Response;
with AWS.Services.Dispatchers.URI;
with AWS.Dispatchers.Callback;
with AWS.Services.ECWF.Registry;
with AWS.MIME;

with Gwiad.Services.Register;
with Gwiad.Websites.Register;
with Gwiad.Web;

with Wiki_Website.Config;
with Wiki_Website.Callbacks;
with Wiki_Website.ECWF_Callbacks;

package body Wiki_Website is

   use AWS;

   use Gwiad;
   use Gwiad.Services;
   use Gwiad.Services.Register;

   use Wiki_Website.Config;
   use Wiki_Website.Callbacks;
   use Wiki_Website.ECWF_Callbacks;

   Wiki_Service_Id : Service_Id := Null_Service_Id;

   Main_Dispatcher : AWS.Services.Dispatchers.URI.Handler;

   function Image_Callback (Request : in Status.Data) return Response.Data;
   --  Image callback

   procedure Unregister;
   --  Unregister website

   ----------------------
   -- Get_Wiki_Service --
   ----------------------

   function Get_Wiki_Service return Wiki_Interface.GW_Service'Class is
      use Wiki_Interface;
   begin

      if Wiki_Service_Id = Null_Service_Id then
         declare
            Wiki_World_Service_Access : constant GW_Service_Access
              := GW_Service_Access (Gwiad.Services.Register.Get
                                    (Wiki_Service_Name));
         begin
            Initialize
              (S              =>
                 GW_Service'Class (Wiki_World_Service_Access.all),
               Base_URL       => Wiki_Web_Root,
               Img_Base_URL   => Wiki_Web_Root & "/" & Wiki_Web_Image,
               Text_Directory => Wiki_Text_Dir);

            Wiki_Service_Id := Gwiad.Services.Register.Set
              (Wiki_Service_Name, Service_Access (Wiki_World_Service_Access));

            Ada.Text_IO.Put_Line (String (Wiki_Service_Id));
         end;
      end if;

      declare
         Wiki_World_Service_Access : constant GW_Service_Access :=
                                       GW_Service_Access
                                         (Gwiad.Services.Register.Get
                                            (Wiki_Service_Name,
                                             Wiki_Service_Id));
         Get_Service               : GW_Service'Class :=
                                       Wiki_World_Service_Access.all;
      begin
         return Get_Service;
      end;
   end Get_Wiki_Service;

   --------------------
   -- Image_Callback --
   --------------------

   function Image_Callback (Request : in Status.Data) return Response.Data is
      URI  : constant String := Status.URI (Request);
      File : constant String :=
               Wiki_Root & "/" & Image_Dir & "/"
                 & URI (URI'First + Wiki_Web_Root'Length
                        + Wiki_Web_Image'Length + 2 .. URI'Last);
   begin
      if Ada.Directories.Exists (File) then
         return Response.File (MIME.Content_Type (File), File);
      else
         return Response.Build
           (Content_Type  => MIME.Text_HTML,
            Message_Body  => "<p>Error</p>");
      end if;
   end Image_Callback;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister is
   begin
      Gwiad.Web.Unregister_Web_Directory
        (Web_Dir => Wiki_Web_Root);
   end Unregister;

begin

   AWS.Services.Dispatchers.URI.Register
     (Main_Dispatcher,
      Wiki_Web_Root & "/" & Wiki_Web_Image,
      Action => Dispatchers.Callback.Create (Image_Callback'Access),
      Prefix => True);

   AWS.Services.Dispatchers.URI.Register_Default_Callback
     (Main_Dispatcher,
      Dispatchers.Callback.Create (Default_Callback'Access));
   --  This default callback will handle all ECWF callbacks

   --  Register ECWF pages

   AWS.Services.ECWF.Registry.Register
     (Wiki_Web_Root & "/" & Wiki_Web_Edit,
      Edit_Template'Access,
      Edit_Page'Access,
      MIME.Text_HTML);

   AWS.Services.ECWF.Registry.Register
     (Wiki_Web_Root & "/" & Wiki_Web_Preview,
      Preview_Template'Access,
      Preview_Page'Access,
      MIME.Text_HTML);

   AWS.Services.ECWF.Registry.Register
     ("/",
      View_Template'Access,
      View'Access,
      MIME.Text_HTML);

   Gwiad.Web.Register_Web_Directory (Web_Dir => Wiki_Web_Root,
                                     Action  => Main_Dispatcher);

   Gwiad.Websites.Register.Register
     (Name        => "wiki",
      Description => "A test for diouzhtu integration in gwiad",
      Unregister  => Unregister'Access);

end Wiki_Website;
