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

unit recentlyusedfiles;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Menus;

type

  { TRecentlyUsedItem }

  TRecentlyUsedItem = class(TObject)
    private
      FBaseFilename: String;
      FFilePath: String;
      FMenuItem: TMenuItem;

    public
      constructor Create(aFileName: String; aOwner: TComponent; clickHandler: TNotifyEvent);
      destructor Destroy; override;
      property BaseFilename: String read FBaseFilename;
      property FilePath: String read FFilePath;
      property MenuItem: TMenuItem read FMenuItem;
  end;

  { TRecentlyUsedFilesList }

  TRecentlyUsedFilesList = class(TObject)
    private
      FMaxItems: Integer;
      FRecentlyUsedFiles: TList;
      FStorageFilePath: String;
      FOwner: TComponent;
      FClickHandler: TNotifyEvent;
      procedure Clear;
      function GetCount: Integer;
      function GetItem(Index: Integer): TRecentlyUsedItem;
      function Load: Boolean; virtual;
      function Save: Boolean; virtual;
      procedure SetMaxItems(AValue: Integer);
      procedure SetStorageFilePath(AValue: String);
      function IndexOf(aFilePath: String): Integer;
    public
      constructor Create(aOwner: TComponent; clickHandler: TNotifyEvent; Storage: String);
      destructor Destroy; override;
      function HasFile(aFilePath: String): Boolean;
      function Add(aFilePath: String): TRecentlyUsedItem;
      function Remove(aMenuItem: TMenuItem): Integer;
      property Count: Integer read GetCount;
      property StorageFilePath: String read FStorageFilePath write SetStorageFilePath;
      property Items[Index: Integer]: TRecentlyUsedItem read GetItem; default;
      property MaxItems: Integer read FMaxItems write SetMaxItems;
  end;

    { TJobQueueList }

  TJobQueueList = class(TRecentlyUsedFilesList)
    private
      function Load: Boolean; override;
      function Save: Boolean; override;
    public
  end;


implementation

{ TRecentlyUsedItem }

constructor TRecentlyUsedItem.Create(aFileName: String; aOwner: TComponent; clickHandler: TNotifyEvent);
begin
  FBaseFilename := ExtractFilename(aFileName);
  FFilePath := aFileName;
  FMenuItem := TMenuItem.Create(aOwner);
  FMenuItem.OnClick := clickHandler;
  FMenuItem.Caption := ExtractFileName(aFilename);
  FMenuItem.Hint := FFilePath;
end;

destructor TRecentlyUsedItem.Destroy;
begin
  FMenuItem := nil; { not owned by us}
  inherited Destroy;
end;

{ TRecentlyUsedFilesList }

procedure TRecentlyUsedFilesList.Clear;
var
  i: Integer;
begin
  if FRecentlyUsedFiles.Count > 0 then
    for i := 0 to FRecentlyUsedFiles.Count - 1 do
      TRecentlyUsedItem(FRecentlyUsedFiles[i]).Free;
  FRecentlyUsedFiles.Clear;
end;

function TRecentlyUsedFilesList.GetCount: Integer;
begin
  Result := FRecentlyUsedFiles.Count;
end;

function TRecentlyUsedFilesList.GetItem(Index: Integer): TRecentlyUsedItem;
begin
  Result := nil;
  if (Index < 0) or (Index >= FRecentlyUsedFiles.Count) then
    Exit;
  Result := TRecentlyUsedItem(FRecentlyUsedFiles[Index]);
end;

function TRecentlyUsedFilesList.Load: Boolean;
var
  aList: TStringList = nil;
  i: Integer;
begin
  Clear;
  Result := False;
  if not FileExists(FStorageFilePath) then
    Exit;
  try
    aList := TStringList.Create;
    aList.LoadFromFile(FStorageFilePath);
    if aList.Count > 0 then
      for i := 0 to aList.Count - 1 do
      begin
        if not FileExists(aList[i]) then
          continue;
        Add(aList[i]);
      end;
    Result := (FRecentlyUsedFiles.Count > 0);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TRecentlyUsedFilesList.Save: Boolean;
var
  aList: TStringList = nil;
  aFile: TRecentlyUsedItem = nil;
  i: Integer;
begin
  Result := False;
  if FRecentlyUsedFiles.Count > 0 then
  begin
    try
      aList := TStringList.Create;
      for i := 0 to FRecentlyUsedFiles.Count - 1 do
      begin
        aFile := TRecentlyUsedItem(FRecentlyUsedFiles[i]);
        aList.Add(aFile.FilePath);
      end;
      aList.SaveToFile(FStorageFilePath);
      Result := FileExists(FStorageFilePath);
    finally
      if Assigned(aList) then
        aList.Free;
    end;
  end;
end;

procedure TRecentlyUsedFilesList.SetMaxItems(AValue: Integer);
var
  aFile: TRecentlyUsedItem;
begin
  FMaxItems:=AValue;
  while Count > FMaxItems do
  begin
    aFile := TRecentlyUsedItem(FRecentlyUsedFiles[FRecentlyUsedFiles.Count - 1]);
    FRecentlyUsedFiles.Delete(FRecentlyUsedFiles.Count - 1);
    aFile.Free;
  end;
end;

procedure TRecentlyUsedFilesList.SetStorageFilePath(AValue: String);
begin
  if FStorageFilePath=AValue then Exit;
  FStorageFilePath:=AValue;
end;

function TRecentlyUsedFilesList.IndexOf(aFilePath: String): Integer;
var
  i: Integer;
  aFile: TRecentlyUsedItem;
begin
  Result := -1;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      aFile := Items[i];
      {$IFDEF MSWINDOWS}
      if SameText(aFilePath, aFile.FilePath) then
      {$ELSE}
      if aFilePath = aFile.FilePath then
      {$ENDIF}
      begin
        Result := i;
        Exit;
      end;
    end;
end;

constructor TRecentlyUsedFilesList.Create(aOwner: TComponent; clickHandler: TNotifyEvent; Storage: String);
begin
  FOwner := aOwner;
  FClickHandler := clickHandler;
  FRecentlyUsedFiles := TList.Create;
  FStorageFilePath := Storage;
  FMaxItems := 20;
  Load;
end;

destructor TRecentlyUsedFilesList.Destroy;
begin
  Save;
  Clear;
  FRecentlyUsedFiles.Free;
  inherited Destroy;
end;

function TRecentlyUsedFilesList.HasFile(aFilePath: String): Boolean;
begin
  Result := (IndexOf(aFilePath) >= 0);
end;

function TRecentlyUsedFilesList.Add(aFilePath: String): TRecentlyUsedItem;
begin
  if IndexOf(aFilePath) >= 0 then
  begin
    Result := Items[IndexOf(aFilePath)];
    Exit;
  end;
  Result := TRecentlyUsedItem.Create(aFilePath, FOwner, FClickHandler);
  FRecentlyUsedFiles.Add(Result);
  MaxItems := FMaxItems; { triggers removal of extra items if needed}
end;

function TRecentlyUsedFilesList.Remove(aMenuItem: TMenuItem): Integer;
var
  i: Integer;
  aFile: TRecentlyUsedItem;
begin
  Result := -1;
  if FRecentlyUsedFiles.Count > 0 then
    for i := 0 to FRecentlyUsedFiles.Count - 1 do
    begin
      aFile := TRecentlyUsedItem(FRecentlyUsedFiles[i]);
      if aFile.MenuItem = aMenuItem then
      begin
        Result := FRecentlyUsedFiles.Remove(aFile);
        aFile.Free;
        Exit;
      end;
    end;
end;

 { TJobQueueList }

function TJobQueueList.Save: Boolean;
var
  aList: TStringList = nil;
  aFile: TRecentlyUsedItem = nil;
  i: Integer;
begin
  Result := False;
  if FRecentlyUsedFiles.Count > 0 then
  begin
    try
      aList := TStringList.Create;
      for i := 0 to FRecentlyUsedFiles.Count - 1 do
      begin
        aFile := TRecentlyUsedItem(FRecentlyUsedFiles[i]);
        aList.Add(aFile.FilePath);
      end;
      aList.SaveToFile(FStorageFilePath);
      Result := FileExists(FStorageFilePath);
    finally
      if Assigned(aList) then
        aList.Free;
    end;
  end;
end;

function TJobQueueList.Load: Boolean;
var
  aList: TStringList = nil;
  i: Integer;
begin
  Clear;
  Result := False;
  if not FileExists(FStorageFilePath) then
    Exit;
  try
    aList := TStringList.Create;
    aList.LoadFromFile(FStorageFilePath);
    if aList.Count > 0 then
      for i := 0 to aList.Count - 1 do
      begin
        Add(aList[i]);
      end;
    Result := (FRecentlyUsedFiles.Count > 0);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

end.

