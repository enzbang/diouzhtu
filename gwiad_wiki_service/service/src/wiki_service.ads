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

with Wiki_Interface;

private with Diouzhtu;

package Wiki_Service is

   use Wiki_Interface;

   type Wiki_Service is new GW_Service with private;

   type Wiki_Service_Access is access all Wiki_Service;

   overriding
   procedure Initialize
     (S              : in out Wiki_Service;
      Text_Directory : in     String;
      Base_URL       : in     String;
      Img_Base_URL   : in     String);

   overriding
   function HTML_Preview
     (S : in Wiki_Service; Text : in String) return String;

   overriding
   function HTML
     (S : in Wiki_Service; Filename : in String) return String;

private

   type Wiki_Service is new GW_Service with record
      Information : Diouzhtu.Wiki_Information;
   end record;

end Wiki_Service;
