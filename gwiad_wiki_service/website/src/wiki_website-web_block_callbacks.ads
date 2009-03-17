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

with AWS.Status;
with AWS.Templates;
with AWS.Services.Web_Block.Context;

private package Wiki_Website.Web_Block_Callbacks is

   use AWS;

   procedure Menu
     (Request      : in Status.Data;
      Context      : not null access AWS.Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Display the menu

   function Menu_Template (Request : in Status.Data) return String;
   --  Returns the menu template corresponding to the wiki website

   procedure View
     (Request      : in Status.Data;
      Context      : not null access AWS.Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  View a wiki page

   function View_Template (Request : in Status.Data) return String;
   --  Returns the view template corresponding to the wiki website

end Wiki_Website.Web_Block_Callbacks;
