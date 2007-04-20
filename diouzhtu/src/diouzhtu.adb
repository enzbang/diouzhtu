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
   use Ada.Strings.Unbounded;

   type Callback is record
      To_HTML : access
        function (Wiki  : Wiki_Information;
                  Index   : Positive;
                  Block   : String) return String;
   end record;

   package Callbacks is new Containers.Vectors
     (Positive, Callback, "=");
   use Callbacks;

   Blocks  : Vector;
   Inlines : Vector;

   function Initialize
     (Base_URL : String; Img_Base_URL : String; Text_Directory : String)
      return Wiki_Information is
   begin
      return (Base_URL       => To_Unbounded_String (Base_URL),
              Img_Base_URL   => To_Unbounded_String (Img_Base_URL),
              Text_Directory => To_Unbounded_String (Text_Directory));
   end Initialize;

   -----------------------
   -- Internal_Register --
   -----------------------

   procedure Internal_Register
     (Level   : Register_Level;
      To_HTML : access
        function
        (Wiki  : Wiki_Information;
        Index : Positive;
        Content : String) return String)
   is
      Register_Callback : Callback;
   begin
      Register_Callback.To_HTML := To_HTML;
      if Level = Block_Level then
         Blocks.Append (Register_Callback);
      else
         Inlines.Append (Register_Callback);
      end if;
   end Internal_Register;

   -----------
   -- Parse --
   -----------

   function Parse
     (Wiki    : Wiki_Information;
      Level   : Register_Level;
      Content : String;
      Index   : Natural := 0)
     return String
   is
      Text      : Unbounded_String := To_Unbounded_String (Content);
      Current   : Positive;
      Container : Vector;
   begin
      if Level = Block_Level then
         Container := Blocks;
      else
         Container := Inlines;
      end if;

      if Index = 0  then
         Current := Container.First_Index;
      else
         Current := Index + 1;
      end if;

      if Last_Index (Container) >= Current then
         return Element (Container, Current).To_HTML
           (Wiki, Current, To_String (Text));
      end if;

      return To_String (Text);
   end Parse;

   --------------
   -- Register --
   --------------

   procedure Register
     (Level   : Register_Level;
      To_HTML : access
        function
        (Wiki  : Wiki_Information;
        Index : Positive;
        Content : String) return String)
   is
      Register_Callback : Callback;
   begin
      --  ???
      --  First Blocks and Inlines callback is code and user callbacks should
      --  not erase this.
      --  Insert them after code callback

      Register_Callback.To_HTML := To_HTML;
      if Level = Block_Level then
         Blocks.Insert (Before   => Next (Blocks.First),
                        New_Item => Register_Callback);
      else
         Inlines.Insert (Before   => Next (Inlines.First),
                         New_Item => Register_Callback);
      end if;
   end Register;
end Diouzhtu;
