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

unit gutilities;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils{$IFDEF MSWINDOWS}, Windows{$ENDIF};

const
  FO_MOVE = $0001;
  FO_COPY = $0002;
  FO_DELETE = $0003;
  FO_RENAME = $0004;

  FOF_MULTIDESTFILES = $0001;
  FOF_CONFIRMMOUSE = $0002;
  FOF_SILENT = $0004;
  FOF_RENAMEONCOLLISION = $0008;
  FOF_NOCONFIRMATION = $0010;
  FOF_WANTMAPPINGHANDLE = $0020;
  FOF_ALLOWUNDO = $0040;
  FOF_FILESONLY = $0080;
  FOF_SIMPLEPROGRESS = $0100;
  FOF_NOCONFIRMMKDIR = $0200;
  FOF_NOERRORUI = $0400;
  FOF_NOCOPYSECURITYATTRIBS = $0800;
  FOF_NORECURSION = $1000;
  FOF_NO_CONNECTED_ELEMENTS = $2000;
  FOF_WANTNUKEWARNING = $4000;
  FOF_NORECURSEREPARSE = $8000;

function GetFileAttributes(FileName: String;
                           var CanRead: Boolean;
                           var CanWrite: Boolean;
                           var CanExecute: Boolean): Boolean;
function GetHomeDir(var HomeDir: String): Boolean;
function GetAppDataDir(var AppDataDir: String): Boolean;
function GetAvailableAppDirFileName(var FileName: String): Boolean;
function GetAvailableFileName(const ADir:String; const FileName: String): String;
procedure StripNumbers(var FileName: String);
function EndsWithCopyNumber(const FileName: String): Boolean;
{$IFDEF MSWINDOWS}
function CanRecycleFiles: Boolean;
function SendToRecycleBin(FileName: String): Boolean;
{$ENDIF}
function SendToTrash(FileName: String): Boolean;
function GetDirModifiedTime(DirName: String; var ModTime: TDateTime): Boolean;
function GetUniqueFileName(var ResultString: String; RootDir: String; FileName: String; MaxTries: Integer = 10000): Boolean;
function CountFilesAndDirs(RootDir: String): Int64;
function FindAllDirs(RootDir: String): TStringList;
function PathsAreSame(First: String; Second: String): Boolean;
function GetDirInfo(DirName: String;
                    var ModTime: TDateTime;
                    var NumFiles: Integer;
                    var NumDirs: Integer;
                    Recursive: Boolean;
                    IncludeHidden: Boolean = True): Boolean;
function GetFileList(const RootDir: String; var AList: TStringList; const DoRecursive: Boolean=False): Boolean;

implementation

function GetFileAttributes(FileName: String;
                           var CanRead: Boolean;
                           var CanWrite: Boolean;
                           var CanExecute: Boolean): Boolean;
begin
  Result := False;
  Assert(False, 'not implemented');
    { TODO 2 -oglen : implement getFileAttributes }
end;

function GetHomeDir(var HomeDir: String): Boolean;
{$IFDEF MSWINDOWS}
var
  PIDL : PItemIDList = nil;
  Folder : array[0..MAX_PATH] of Char;
const
  CSIDL_PERSONAL = $0005;
{$ENDIF}
begin
  Result := False;
  HomeDir := EmptyStr;
  try
    {$IFDEF MSWINDOWS}
    SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, PIDL);
    SHGetPathFromIDList(PIDL, Folder);
    HomeDir := Folder;
    HomeDir := Trim(HomeDir);
    if not DirectoryExists(HomeDir) then
    begin
      if not ForceDirectories(HomeDir) then
        raise Exception.Create('cannot find the users home directory');
    end;
    {$ELSE}
    HomeDir := GetEnvironmentVariable('HOME');
    if not DirectoryExists(HomeDir) then
    begin
      if not ForceDirectories(HomeDir) then
        raise Exception.Create('cannot find the users home directory');
    end;
    {$ENDIF}
    Result := True;
  Except
    Result := False;
  end;
end;

function GetAppDataDir(var AppDataDir: String): Boolean;
{$IFDEF MSWINDOWS}
var
  PIDL : PItemIDList = nil;
  Folder : array[0..MAX_PATH] of Char;
const
  CSIDL_LOCAL_APPDATA = $0028;
{$ENDIF}
begin
  Result := False;
  AppDataDir := EmptyStr;
  try
    {$IFDEF MSWINDOWS}
    SHGetSpecialFolderLocation(0, CSIDL_LOCAL_APPDATA, PIDL);
    SHGetPathFromIDList(PIDL, Folder);
    AppDataDir := Folder;
    AppDataDir := Trim(AppDataDir);
    if not DirectoryExists(AppDataDir) then
    begin
      if not ForceDirectories(AppDataDir) then
        raise Exception.Create('cannot find the users home directory');
    end;
    {$ELSE}
    AppDataDir := GetEnvironmentVariable('HOME');
    if not DirectoryExists(AppDataDir) then
    begin
      if not ForceDirectories(AppDataDir) then
        raise Exception.Create('cannot find the users home directory');
    end;
    {$ENDIF}
    Result := True;
  Except
    Result := False;
  end;
end;

function GetAvailableAppDirFileName(var FileName: String): Boolean;
var
  AppDataDir: String;
  Index: Integer;
  FileExt: String;
begin
  Result := False;
  AppDataDir := EmptyStr;

  try
    {$IFDEF MSWINDOWS}
    AppDataDir := GetAppConfigDir(False);
    if not DirectoryExists(AppDataDir) then
      if not ForceDirectories(AppDataDir) then
        raise Exception.Create('cannot find the users home directory');
    {$ELSE}
    AppDataDir := GetEnvironmentVariable('HOME');
    if not DirectoryExists(AppDataDir) then
    begin
      if not ForceDirectories(AppDataDir) then
        raise Exception.Create('cannot find the users home directory');
    end;
    {$ENDIF}
    FileExt := ExtractFileExt(FileName);
    FileName := AppDataDir + FileName;
    Index := 0;
    while FileExists(FileName) do
    begin
      if EndsWithCopyNumber(FileName) then
        StripNumbers(FileName);
      FileName := FileName + '(' + IntToStr(Index) + ')' + FileExt;
      //FileName := AppDataDir + ExtractFileName(FileName) + '(' + IntToStr(Index) + ')';
      inc(Index);
    end;
    Result := True;
  Except
    Result := False;
  end;
end;

function GetAvailableFileName(const ADir: String; const FileName: String): String;
var
  Index: Integer;
  MyDir: String;
  FileExt: String;
begin
  Result := EmptyStr;
  MyDir := Trim(ADir);
  if MyDir[Length(MyDir)] = PathDelim then
    Result := ADir + FileName
  else
    Result := ADir + PathDelim + FileName;
  FileExt := ExtractFileExt(FileName);
  Index := 0;
  while FileExists(Result) do
  begin
    if EndsWithCopyNumber(Result) then
      StripNumbers(Result);
    Result := Result + '(' + IntToStr(Index) + ')' + FileExt;
    inc(Index);
  end;
end;

procedure StripNumbers(var FileName: String);
var
  First: Integer;
  Last: Integer;
  Index: Integer;
begin
  if FileName[Length(FileName)] = ')' then
  begin
    Last := Length(FileName);
    Index := Last;
    while true do
    begin
      if FileName[Index] <> '(' then
        dec(Index)
      else
      begin
        First := Index;
        break;
      end;
      if Index <= 1 then
        Exit;
    end;
    Delete(FileName, First, Last - First + 1);
  end;
end;

function EndsWithCopyNumber(const FileName: String): Boolean;
var
  First: Integer;
  Last: Integer;
  Index: Integer;
  TempStr: String;
  TempInt: Integer;
begin
  Result := False;
  if FileName[Length(FileName)] = ')' then
  begin
    Last := Length(FileName);
    Index := Last;
    while true do
    begin
      if FileName[Index] <> '(' then
        dec(Index)
      else
      begin
        First := Index;
        break;
      end;
      if Index <= 1 then
        Exit;
    end;
    TempStr := Copy(FileName, First + 1, Last - First - 1);
    Result := TryStrToInt(TempStr, TempInt);
  end;
end;

{$IFDEF MSWINDOWS}
function CanRecycleFiles: Boolean;
var
  TempList: TStringList = nil;
  HomeDir: String = '';
  UniqueName: String = '';
begin
  Result := False;

  try
    try
      TempList := TStringList.Create;
      TempList.Add('This is a temporary file which can be safely deleted.');
      if GetHomeDir(HomeDir) then
      begin
        if GetUniqueFileName(UniqueName, HomeDir, 'PleaseDeleteMe.txt') then
        begin
          TempList.SaveToFile(UniqueName);
          if SendToRecycleBin(UniqueName) then
            Result := True
          else
            try
              if FileExists(UniqueName) then
                DeleteFile(PChar(UniqueName));
            except
              // no sweat
            end;
        end;
      end;
    except
      Result := False;
    end;
  finally
    if Assigned(TempList) then
      TempList.Free;
  end;
end;

{ TODO 1 -oglen : fix SendToRecycleBin as it is not working. Will need to refer to the msdn documentation }
function SendToRecycleBin(FileName: String): Boolean;
var
  fileOpStruct : TSHFileOpStruct;
begin
  Result := False;
  if not FileExists(FileName) then
    Exit;

  fileOpStruct.hwnd := 0;
  fileOpStruct.wFunc := FO_DELETE;
  fileOpStruct.pFrom := PChar(FileName);
  fileOpStruct.fFlags := FOF_ALLOWUNDO or FOF_SILENT or FOF_NOCONFIRMATION;
  try
    SHFileOperation(fileOpStruct);
    if not FileExists(FileName) then
      Result := True;
  except
    Result := False;
  end;
end;
{$ENDIF}

{ TODO 1 -oglen : implement send to trash }
function SendToTrash(FileName: String): Boolean;
begin
  Result := False;
  Assert(False, 'not implemented');
end;

function GetDirModifiedTime(DirName: String; var ModTime: TDateTime): Boolean;
var
  ParentDir: String;
  Info: TSearchRec;
begin
  ModTime := MinDateTime;
  Result := False;

  if not DirectoryExists(DirName) then
    Exit;

  ParentDir := ExtractFilePath(DirName);
  if not DirectoryExists(ParentDir) then
    Exit;

  try
    if (FindFirst(ParentDir + PathDelim + '*', faAnyFile and faDirectory, Info) = 0) then
    begin
      Repeat
        if (Info.Attr and faDirectory) = faDirectory then
        begin
          if (Info.Name = '.') or (Info.Name = '..') then
            Continue;
          if PathsAreSame(ParentDir + PathDelim + Info.Name, DirName) then
          begin
            ModTime := FileDateToDateTime(Info.Time);
            Break;
          end;
        end;
      until FindNext(Info) <> 0;
      SysUtils.FindClose(Info);
      Result := True;
    end;
  Except
    ModTime := MinDateTime;
    Result := False;
  end;
end;

function GetUniqueFileName(var ResultString: String; RootDir: String; FileName: String; MaxTries: Integer = 10000): Boolean;
var
  i: Integer;
  Prospect: String;
begin
  Result := False;
  ResultString := EmptyStr;

  if not DirectoryExists(RootDir) then
    if not ForceDirectories(RootDir) then
      Exit;

  Prospect := RootDir + PathDelim + FileName;

  i := 1;
  while FileExists(Prospect) and (i <= MaxTries) do
  begin
    inc(i);
    Prospect := RootDir + PathDelim + '(' + IntToStr(i) + ')' + FileName;
  end;

  if FileExists(Prospect) then
    Exit;

  Result := True;
  ResultString := Prospect;
end;

function CountFilesAndDirs(RootDir: String): Int64;
var
  Info: TSearchRec;
  DirPathList: TStringList;
  i: Integer;
begin
  Result := 0;
  DirPathList := nil;

  try
    try
      DirPathList := TStringList.Create;

      if (SysUtils.FindFirst(RootDir + PathDelim + '*', faAnyFile and faDirectory, Info) = 0) then
      begin
        Repeat
          if ((Info.Attr and faDirectory) = faDirectory) then
          begin
            if (Info.Name = '.') or (Info.Name = '..') then
              Continue;

          DirPathList.Add(RootDir + PathDelim + Info.Name);
          end;
          inc(Result);
        until SysUtils.FindNext(Info) <> 0;
      end;
      SysUtils.FindClose(Info);

      for i := 0 to DirPathList.Count - 1 do
      begin
        Result := Result + CountFilesAndDirs(DirPathList[i]);
      end;
    Except
      Result := 0;
    end;
  finally
    DirPathList.Free;
  end;
end;

function FindAllDirs(RootDir: String): TStringList;
var
  Info: TSearchRec;
  DirPathList: TStringList;
  TempList: TStringList;
  i: Integer;
  j: Integer;
begin
  Result := nil;
  DirPathList := nil;
  TempList := nil;

  try
    try
      Result := TStringList.Create;
      DirPathList := TStringList.Create;

      if (SysUtils.FindFirst(RootDir + PathDelim + '*', faAnyFile and faDirectory, Info) = 0) then
      begin
        Repeat
          if ((Info.Attr and faDirectory) = faDirectory) then
          begin
            if (Info.Name = '.') or (Info.Name = '..') then
              Continue;

            Result.Add(RootDir + PathDelim + Info.Name);
            DirPathList.Add(RootDir + PathDelim + Info.Name);
          end;
        until SysUtils.FindNext(Info) <> 0;
      end;
      SysUtils.FindClose(Info);

      for i := 0 to DirPathList.Count - 1 do
      begin
        TempList := FindAllDirs(DirPathList[i]);
        for j := 0 to TempList.Count - 1 do
        begin
          Result.Add(TempList[j]);
        end;
      end;
    Except
      //Result := nil;
    end;
  finally
    DirPathList.Free;
    if Assigned(TempList) then
      TempList.Free;
  end;
end;

function PathsAreSame(First: String; Second: String): Boolean;
var
  AreSame: Integer;
begin
  { TODO 1 -oglen : make sure that the comparison for windows is case insensitive and for other platforms it is case sensitive }
  {$IFDEF MSWINDOWS}
  AreSame := CompareText(First, Second);
  {$ELSE}
  AreSame := CompareStr(First, Second);
  {$ENDIF}
  if AreSame = 0 then
    Result := True
  else
    Result := False;
end;

function GetDirInfo(DirName: String;
                    var ModTime: TDateTime;
                    var NumFiles: Integer;
                    var NumDirs: Integer;
                    Recursive: Boolean;
                    IncludeHidden: Boolean = True): Boolean;
var
  Info: TSearchRec;
  DirList: TStringList;
  i: Integer;
  TempModTime: TDateTime = 0.0;
  TempNumFiles: Integer = -1;
  TempNumDirs: Integer = -1;
begin
  NumFiles := 0;
  NumDirs := 0;
  ModTime := MinDateTime;
  Result := False;
  DirList := nil;

  try
    try
      if Recursive then
        DirList := TStringList.Create;

      if (FindFirst(DirName + PathDelim + '*', faAnyFile and faDirectory, Info) = 0) then
      begin
        Repeat
          if (Info.Attr and faDirectory) = faDirectory then
          begin
            if (Info.Name = '.') or (Info.Name = '..') or (not Recursive) then
              Continue;
            if Recursive then
               DirList.Add(DirName + PathDelim + Info.Name);
            inc(NumDirs);
          end
          else
          begin
            if not IncludeHidden then
            begin
              if not ((Info.Attr and faHidden) = faHidden) then
                 inc(NumFiles);
            end
            else
              inc(NumFiles);

            if Info.Time > ModTime then
              ModTime := Info.Time;
          end;
        until FindNext(Info) <> 0;
        SysUtils.FindClose(Info);
      end;

      if Recursive then
      begin
        for i := 0 to DirList.Count - 1 do
        begin
          if GetDirInfo(DirList[i], TempModTime, TempNumFiles, TempNumDirs, Recursive, IncludeHidden) then
          begin
            inc(NumFiles, TempNumFiles);
            inc(NumDirs, TempNumDirs);
            if TempModTime > ModTime then
              ModTime := TempModTime;
          end
          else
            raise Exception.Create('');
        end;
      end;
      Result := True;
    Except
      ModTime := MinDateTime;
      NumFiles := 0;
      NumDirs := 0;
      Result := False;
    end;
  finally
    if Assigned(DirList) then
      DirList.Free;
  end;
end;

function GetFileList(const RootDir: String; var AList: TStringList; const DoRecursive: Boolean): Boolean;
var
  Info: TSearchRec;
  DirPathList: TStringList;
  i: Integer;
begin
  Result := False;
  DirPathList := nil;

  try
    try
      DirPathList := TStringList.Create;

      if (SysUtils.FindFirst(RootDir + PathDelim + '*', faAnyFile and faDirectory, Info) = 0) then
      begin
        Repeat
          if ((Info.Attr and faDirectory) = faDirectory) then
          begin
            if (Info.Name = '.') or (Info.Name = '..') then
              Continue;

          DirPathList.Add(RootDir + PathDelim + Info.Name);
          end
          else
            AList.Add(RootDir + PathDelim + Info.Name);
        until SysUtils.FindNext(Info) <> 0;
      end;
      SysUtils.FindClose(Info);

      if DoRecursive then
      begin
        for i := 0 to DirPathList.Count - 1 do
          GetFileList(DirPathList[i], AList, DoRecursive);
      end;
      Result := True;
    Except
      Result := False;
    end;
  finally
    DirPathList.Free;
  end;
end;

end.
