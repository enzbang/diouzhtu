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

with Gwiad.Registry.Services;

package Wiki_Interface is

   use Gwiad.Registry.Services;

   type Wiki2HTML_Interface is limited interface;

   procedure Initialize
     (S              : in out Wiki2HTML_Interface;
      Text_Directory : in     String;
      Base_URL       : in     String;
      Img_Base_URL   : in     String) is abstract;

   function HTML_Preview
     (S : Wiki2HTML_Interface; Text : String) return String is abstract;

   function HTML
     (S : Wiki2HTML_Interface; Filename : String) return String is abstract;

   type GW_Service is abstract new Service and Wiki2HTML_Interface
   with null record;

   type GW_Service_Access is access all GW_Service;

end Wiki_Interface;
