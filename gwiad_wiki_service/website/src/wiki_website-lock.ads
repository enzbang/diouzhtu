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

with Ada.Calendar;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;

package Wiki_Website.Lock is

   use Ada;
   use Ada.Calendar;
   use Ada.Strings.Unbounded;

   type Object is record
      Timestamp : Time;
      User      : Unbounded_String;
   end record;

   package Lock_Map is new Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Object,
      Hash            => Strings.Hash,
      Equivalent_Keys => "=");

   protected Manager is

      procedure Claim (Filename     : in String;
                       User         : in String;
                       Time_To_Wait : out Duration);
      --  If Time_To_Wait is not null the file is locked for that period

      procedure Release (Filename : in String;
                         User     : in String);
      --  Release the lock

      function Check (Filename : in String; User : in String) return Boolean;
      --  Check is the lock is still valid
   private
      Map : Lock_Map.Map;
   end Manager;

end Wiki_Website.Lock;
