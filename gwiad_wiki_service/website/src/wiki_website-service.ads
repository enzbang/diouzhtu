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

with Wiki_Interface;
with Wiki_Website.Config;
with Gwiad.Registry.Websites;

private package Wiki_Website.Service is

   use Wiki_Website.Config;
   use Gwiad.Registry.Websites;

   function Get (Name : Wiki_Name) return Wiki_Interface.GW_Service'Class;
   --  Returns the service

   procedure Register
     (Virtual_Host : String; Name : Wiki_Name; Description : String);
   --  Register a website

   procedure Unregister (Name : Website_Name);
   --  Unregister a website


end Wiki_Website.Service;
