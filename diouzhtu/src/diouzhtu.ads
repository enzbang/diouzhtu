------------------------------------------------------------------------------
--                               Diouzhtu                                   --
--                                                                          --
--                         Copyright (C) 2007-2008                          --
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

private with Ada.Strings.Unbounded;

package Diouzhtu is

   type Wiki_Information is private;

   type Converter is access
        function (Wiki  : in Wiki_Information;
                  Index : in Positive;
                  Block : in String) return String;

   function Initialize
     (Base_URL     : in String;
      Img_Base_URL   : in String;
      Text_Directory : in String)
      return Wiki_Information;
   --  Creates a new wiki

   type Register_Level is (Block_Level, Inline_Level);

   procedure Register
     (Level   : in Register_Level;
      To_HTML : Converter);
   --  Register a new recursive callback

private

   type Wiki_Information is record
      Base_URL       : Ada.Strings.Unbounded.Unbounded_String;
      Img_Base_URL   : Ada.Strings.Unbounded.Unbounded_String;
      Text_Directory : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   procedure Internal_Register
     (Level   : in Register_Level;
      To_HTML : in Converter);
   --  Register a new recursive callback

   function Parse
     (Wiki    : in Wiki_Information;
      Level   : in Register_Level;
      Content : in String;
      Index   : in Natural := 0)
     return String;
   --  Parse using all registered callbacks

end Diouzhtu;
