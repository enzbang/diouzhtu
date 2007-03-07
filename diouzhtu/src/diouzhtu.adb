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

with Ada.Containers.Vectors;

package body Diouzhtu is

   use Ada;

   type Block_Callback is record
      To_HTML : access function (Block : String) return String;
   end record;

   package Block_Callbacks is new Containers.Vectors
     (Positive, Block_Callback, "=");
   use Block_Callbacks;

   Blocks : Vector;

   ----------------------
   -- Block_Processing --
   ----------------------

   function Block_Processing (Block : in String) return String is
      BO_Cursor : Cursor := Blocks.First;
   begin
      while Has_Element (BO_Cursor) loop
         declare
            Result : constant String := Element (BO_Cursor).To_HTML (Block);
         begin
            if Result /= "" then
               return Result;
            end if;
            Next (BO_Cursor);
         end;
      end loop;
      return "";
   end Block_Processing;

   --------------
   -- Register --
   --------------

   procedure Register
     (To_HTML : access function (Block : String) return String)
   is
      BC : Block_Callback;
   begin
      BC.To_HTML := To_HTML;
      Blocks.Append (BC);
   end Register;

end Diouzhtu;
