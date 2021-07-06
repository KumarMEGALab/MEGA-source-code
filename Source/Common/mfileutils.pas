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

unit MFileUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils{$IFDEF FPC}, FileUtil{$ENDIF};

  function RelativeToAbsolutePath(const RelativePath: String; var AbsolutePath): Boolean;
  function FindAllDirs(RootDir: String; var DirList: TStringList; DoRecursive: Boolean=True): Boolean;
  function FindAllFilesInDir(RootDir: String; var FileList: TStringList; const FileExtsToInclude: TStringList): Boolean;
  function SwitchDirectory(Filename: String): Boolean;
  function ReadDataFromFile(filename: String): String;

implementation

function RelativeToAbsolutePath(const RelativePath: String; var AbsolutePath): Boolean;
begin
  Result := False;

end;

function FindAllDirs(RootDir: String; var DirList: TStringList; DoRecursive: Boolean=True): Boolean;
var
  Info: TSearchRec;
  TempDirs: TStringList;
  i: Integer;
begin
  Result := False;
  TempDirs := nil;

  if DirectoryExists(RootDir) then
    Exit;

  if not Assigned(DirList) then
    DirList := TStringList.Create;

  try
    try
      TempDirs := TStringList.Create;

      if FindFirst(RootDir + PathDelim + '*',faDirectory,Info) = 0 then
      Repeat

        if (Info.Name = '.') or (Info.Name = '..') then
          Continue;
        DirList.Add(RootDir + PathDelim + Info.Name);
        if DoRecursive then
          TempDirs.Add(RootDir + PathDelim + Info.Name);

      Until FindNext(Info) <> 0 ;
      FindClose(Info);

      if DoRecursive then
        for i := 0 to TempDirs.Count - 1 do
          Result := Result and FindAllDirs(TempDirs[i], DirList, True);

      Result := True;
    Except
      on E:Exception do
      begin
        Result := False;
      end;
    end;
  finally
    if Assigned(DirList) then
      DirList.Free;
  end;
end;

function FindAllFilesInDir(RootDir: String; var FileList: TStringList; const FileExtsToInclude: TStringList): Boolean;
var
  i: Integer;
  Info: TSearchRec;
  TempExt: AnsiString;
begin
  Result := False;

  if not DirectoryExists(RootDir) then
    Exit;

  if FileExtsToInclude.Count = 0 then
    Exit;

  if not Assigned(FileList) then
    FileList := TStringList.Create
  else
    FileList.Clear;

  try
    try
      if FindFirst(RootDir + PathDelim + '*',faAnyFile,Info) = 0 then
        Repeat
          if Info.Attr and faDirectory = faDirectory then
            Continue;
          TempExt := ExtractFileExt(Info.Name);
          for i := 0 to FileExtsToInclude.Count - 1 do
          begin
            if AnsiSameText(TempExt, ExtractFileExt(FileExtsToInclude[i])) then
              FileList.Add(RootDir + PathDelim + Info.Name);
          end;

        Until FindNext(Info)  <> 0 ;
        Result := True;
    Except
      on E:Exception do
      begin
        Result := False;
      end;
    end;
  finally
    FindClose(Info);
  end;
end;

function SwitchDirectory(Filename: String): Boolean;
var
  TempDir: String;
begin
  Result := False;
  TempDir := ExtractFilePath(Trim(Filename));
  if DirectoryExists(TempDir)  then
    Result := SetCurrentDir(TempDir);
end;

function ReadDataFromFile(filename: String): String;
var
  aList: TStringList = nil;
begin
  Result := EmptyStr;
  if not FileExists(filename) then
    Exit;
  aList := TStringList.Create;
  try
    aList.LoadFromFile(filename);
    Result := aList.Text;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

end.
