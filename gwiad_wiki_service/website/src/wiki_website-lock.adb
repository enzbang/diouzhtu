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

with Morzhol.Strings;

package body Wiki_Website.Lock is

   use Morzhol.Strings;

   Validity : constant Duration := 10 * 60.0; --  10 minutes

   protected body Manager is

      function Check (Filename : in String; User : in String) return Boolean is
      begin
         if Map.Contains (Filename)
           and then Map.Element (Filename).User = +User
         then
            return True;
         else
            return False;
         end if;
      end Check;

      procedure Claim
        (Filename     : in String;
         User         : in String;
         Time_To_Wait : out Duration) is
      begin

         Time_To_Wait := 0.0;

         if Map.Contains (Filename) then
            declare
               O        : Object := Map.Element (Filename);
               Lifetime : constant Duration := Clock - O.Timestamp;
            begin
               if Lifetime > Validity then
                  --  The last lock is no more valid. Steal it.

                  Map.Replace (Filename,
                               Object'(Timestamp => Clock, User => +User));
               else
                  Time_To_Wait := Validity - Lifetime;
               end if;
            end;
         else
            Map.Insert (Filename, Object'(Timestamp => Clock, User => +User));
         end if;
      end Claim;

      procedure Release (Filename : in String; User : in String) is
      begin
         if Check (Filename, User) then
            Map.Delete (Filename);
         end if;
      end Release;
   end Manager;

end Wiki_Website.Lock;
