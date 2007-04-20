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
with AWS.Response;
with AWS.Templates;
with AWS.Services.ECWF.Context;

private package Wiki_Website.Callbacks is

   use AWS;

   function Default_Callback
     (Request : in Status.Data) return Response.Data;
   --  Default callback

   function Edit_Template (Request : in Status.Data) return String;
   --  Edit template

   procedure Edit_Page
     (Request      : in     Status.Data;
      Context      : access AWS.Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);

   function View_Template (Request : in Status.Data) return String;
   --  View template

   function Preview_Template (Request : in Status.Data) return String;
   --  Preview template

   procedure Preview_Page
     (Request      : in     Status.Data;
      Context      : access AWS.Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Preview a diouzhtu page

end Wiki_Website.Callbacks;
