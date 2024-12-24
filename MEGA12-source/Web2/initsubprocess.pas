{
	Copyright 1992-2024 Sudhir Kumar and Koichiro Tamura

	This file is part of the MEGA (Molecular Evolutionary Genetics Analyis) software.

	MEGA (Molecular Evolutionary Genetics Analysis) is free software:
	you can redistribute it and/or modify it under the terms of the
	GNU General Public License as published by the Free Software
	Foundation, either version 3 of the License, or (at your option)
	any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <https://www.gnu.org/licenses/>.

   Contributor(s):   The MEGA source code and software is made available in the hopes that it will be useful. 
   In keeping with the spirit and intent that the MEGA project is developed under, the authors of MEGA request that before
   distributing any significant changes to the MEGA source code (or derivatives thereof), you share
   those changes with the MEGA authors so that they may have the opportunity to test that
   the changes do not introduce errors into the code or otherwise negatively impact the correctness
   or performance of the MEGA software.
   
	Please email inqiries to s.kumar@temple.edu
}

// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2021 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit InitSubProcess;


{$mode ObjFPC}{$H+}
{$IFDEF DARWIN}{$modeswitch objectivec1}{$ENDIF}

interface

uses
  {$IFDEF DARWIN}CocoaAll, CocoaUtils, uCEFLazarusCocoa,{$ENDIF}
  uCEFApplication, uCEFWorkScheduler, uCEFTypes, Forms, sysutils, umegarenderer;

implementation

function InitializeChromium: Boolean;
  var
    aPath: String = '';
    aStr: String = '';
  begin
    if not (Assigned(GlobalCEFApp) and (GlobalCEFApp.Status <> asInitialized)) then
    begin
      {$IFDEF DARWIN}
      aPath := String(NSStringToString(NSBundle.mainBundle.bundlePath) + PathDelim + 'Contents' + PathDelim +  'Frameworks' + PathDelim + 'Chromium Embedded Framework.framework') + PathDelim;
      {$ELSE}
      aPath := String(ExtractFilePath(Application.ExeName));
      {$ENDIF}
      aStr := String(aPath);
      SetCurrentDir(aStr);

      {$IFDEF DARWIN}
         CreateGlobalCEFApp();
      {$ELSE}
         CreateGlobalCEFApp(aPath);
      {$ENDIF}
      Result := GlobalCEFApp.StartMainProcess;
    end
    else
        Result := false;
  end;

initialization
{$IFDEF DARWIN}
AddCrDelegate;
{$ENDIF}

if not InitializeChromium then
begin
     {$IFDEF DARWIN}
     if GlobalCEFWorkScheduler <> nil then
        GlobalCEFWorkScheduler.StopScheduler;
     {$ENDIF}
     DestroyGlobalCEFApp;
     {$IFDEF DARWIN}
     DestroyGlobalCEFWorkScheduler;
     {$ENDIF}
     halt(0); // exit the subprocess
end;


end.

