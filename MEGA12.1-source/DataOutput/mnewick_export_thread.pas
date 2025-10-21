{
	Copyright 1992-2025 Sudhir Kumar and Koichiro Tamura

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

unit mnewick_export_thread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, syncobjs;

var
  SaveNewickCS: TCriticalSection;

type

  { TNewickExportThread }

  TNewickExportThread = class(TThread)

    protected
      FFilename: String;
      FNewick: String;
      procedure Execute; override;
    public
      constructor Create(aNewick: String; filename: String);
  end;

  procedure SaveNewickRecoveryFile(aNewick: String; filename: String);
  procedure DeleteRecoveryFile(filename: String);

implementation

procedure SaveNewickRecoveryFile(aNewick: String; filename: String);
var
  t: TNewickExportThread = nil;
begin
  t := TNewickExportThread.Create(aNewick, filename);
  t.Start;
end;

procedure DeleteRecoveryFile(filename: String);
begin
  try
    try
      SaveNewickCS.Acquire;
      if FileExists(filename) then
        DeleteFile(filename);
    except
      on E:Exception do
      begin
      {$IFDEF DEBUG}
      raise E;
      {$ENDIF}
      end;
    end;
  finally
    SaveNewickCS.Release;
  end;
end;

{ TNewickExportThread }

procedure TNewickExportThread.Execute;
var
  aFile: TextFile;
begin
  try
    SaveNewickCS.Acquire;
    AssignFile(aFile, FFilename);
    Rewrite(aFile);
    WriteLn(aFile, FNewick);
  finally
    CloseFile(aFile);
    SaveNewickCS.Release;
  end;
end;

constructor TNewickExportThread.Create(aNewick: String; filename: String);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FFilename := filename;
  FNewick := aNewick;
end;

end.

