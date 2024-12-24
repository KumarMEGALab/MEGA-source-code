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

unit mceflogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cef3types, Dialogs;

procedure WriteToLog(aMsg: String; sourceProcess: TCefProcessId = PID_RENDERER);

implementation

procedure WriteToLog(aMsg: String; sourceProcess: TCefProcessId = PID_RENDERER);
var
  aFile: TextFile;
  aTxt: String;
  DEBUG_FILE: String;
begin
  {$IFDEF DARWIN}
  Exit;
  {$ENDIF}
  {$IFDEF DEBUG}
  try
    try
      aTxt := DateTimeToStr(Now);
      if SourceProcess = PID_RENDERER then
      begin
        DEBUG_FILE := GetAppConfigDir(False) + 'render_process.log';
        aTxt := Format('%s   MEGA RenderProcess: %s', [aTxt, aMsg])
      end
      else if sourceProcess = PID_BROWSER then
      begin
        DEBUG_FILE := GetAppConfigDir(False) + 'browser_process.log';
        aTxt := Format('%s   MEGA BrowserProcess: %s', [aTxt, aMsg])
      end
      else
      begin
        DEBUG_FILE := GetAppConfigDir(False) + 'cef_main_process.log';
        aTxt := Format('%s   MEGA Process: %s', [aTxt, aMsg]);
      end;
      AssignFile(aFile, DEBUG_FILE);
      if FileExists(DEBUG_FILE) then
        Append(aFile)
      else
        Rewrite(aFile);
      WriteLn(aFile, aTxt);
      //CefLog(CefLogFile, 1, Ord(LOGSEVERITY_INFO), aTxt);
    except
      on E:Exception do
        ShowMessage('CEF Application Error: ' + E.Message + ' ' + DEBUG_FILE);
    end;
  finally
    CloseFile(aFile);
  end;
  {$ENDIF}
end;

end.

