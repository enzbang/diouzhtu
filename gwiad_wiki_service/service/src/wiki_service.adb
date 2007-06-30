------------------------------------------------------------------------------
--                                  Gwiad                                   --
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
with Diouzhtu.To_HTML;
with Gwiad.Plugins.Services.Registry;

package body Wiki_Service is

   use Diouzhtu.To_HTML;
   use Gwiad.Plugins.Services;

   function Builder return access Service'Class;
   --  Build a new test plugin

   -------------
   -- Builder --
   -------------

   function Builder return access Service'Class is
      Test : constant Wiki_Service_Access := new Wiki_Service;
   begin
      return Test;
   end Builder;

   ----------
   -- HTML --
   ----------

   overriding
   function HTML
     (S : Wiki_Service; Filename : String) return String is
   begin
      return To_HTML (S.Information, Filename);
   end HTML;

   ------------------
   -- HTML_Preview --
   ------------------

   overriding
   function HTML_Preview
     (S : Wiki_Service; Text : String) return String is
   begin
      return Text_To_HTML (S.Information, Text);
   end HTML_Preview;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize
     (S              : in out Wiki_Service;
      Text_Directory : in     String;
      Base_URL       : in     String;
      Img_Base_URL   : in     String)
   is
      Wiki : Diouzhtu.Wiki_Information :=
               Diouzhtu.Initialize
                 (Base_URL       => Base_URL,
                  Img_Base_URL   => Img_Base_URL,
                  Text_Directory => Text_Directory);
   begin
      S.Information := Wiki;
   end Initialize;

begin
   Gwiad.Plugins.Services.Registry.Register
     (Name        => "wiki_service",
      Description => "A wiki service for gwiad based on diouzhtu",
      Builder     => Builder'Access);

exception
   when others =>
      Ada.Text_IO.Put_Line ("wiki_service registration failed");
end Wiki_Service;
