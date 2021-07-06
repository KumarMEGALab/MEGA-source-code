{
	Copyright 1992-2021 Sudhir Kumar and Koichiro Tamura

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

unit mwebdialogfiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zipper, LResources;

type

  { TWebDialogFilesManager }

  TWebDialogFilesManager = class(TObject)
    private
      FUnzipper: TUnZipper;
    public
      constructor Create;
      destructor Destroy; override;

      function CheckWebDialogFiles: Boolean;
  end;

  { TWebDialogFilesThread }

  TWebDialogFilesThread = class(TThread)
    private
      FLog: String;
      FIsSuccess: Boolean;
      FManager: TWebDialogFilesManager;
    public
      constructor Create(CreateSuspended: Boolean);
      destructor Destroy; override;
      procedure Execute; override;

      property Log: String read FLog;
      property IsSuccess: Boolean read FIsSuccess;

  end;

implementation

uses
  MegaPrivateFiles, MegaUtils;

{ TWebDialogFilesThread }

constructor TWebDialogFilesThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FManager := TWebDialogFilesManager.Create;
  FIsSuccess := False;
  FLog := EmptyStr;
  FreeOnTerminate := True;
end;

destructor TWebDialogFilesThread.Destroy;
begin
  if Assigned(FManager) then
    FManager.Free;
  inherited Destroy;
end;

procedure TWebDialogFilesThread.Execute;
begin
  try
    FIsSuccess := False;
    if FManager.CheckWebDialogFiles then
      FIsSuccess := True
    else
      FLog := 'An unknown error occurred when extracting the MEGA web dialog files';
  except
    on E:Exception do
      FLog := E.Message;
  end;
end;

{ TWebDialogFilesManager }

constructor TWebDialogFilesManager.Create;
begin
  FUnzipper := TUnZipper.Create;
end;

destructor TWebDialogFilesManager.Destroy;
begin
  if Assigned(FUnzipper) then
    FUnzipper.Free;
  inherited Destroy;
end;

function TWebDialogFilesManager.CheckWebDialogFiles: Boolean;
var
  archive: String;
  aFile: String;

begin
  Result := False;
  aFile := GetPrivateFile(wofWebSeqDataExportFile, False);
  if FileExists(aFile) then
  begin
    Result := True;
    Exit;
  end;
  archive := GetPrivateFile(wofWebOptionsDialogsArchive, True);
  if FileExists(archive) then
  begin
    FUnzipper.Filename := archive;
    FUnzipper.OutputPath := ExtractFileDir(aFile);
    FUnzipper.Examine;
    FUnzipper.UnZipAllFiles;
    Result := FileExists(aFile);
    DeleteFile(archive);
  end;
end;


end.

