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

unit miniformstream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, LResources,
  StdCtrls, Dialogs, ExtCtrls;

type

  { TIniFormStream }
  TIniFormStream = class(TObject)
    private
      FComponent: TComponent;
      FComponentPropertiesAsString: String;
      procedure ReadComponentSessionToStream;
      procedure WriteComponentStreamToPropertiesString;
    public
      constructor Create;
      procedure ConvertBinaryDataToText;
      procedure SaveStreamAsString(AStream: TStream);
      procedure ReadStreamFromString(AStream: TStream);
      function ReadStringFromStream(AStream: TStream): string;
      procedure OnFindClass(Reader: TReader; const AClassName: string; var ComponentClass: TComponentClass);
      function DebugRestoreProperties(aComponent: TComponent; filename: String): Boolean;
      function DebugSaveProperties(aComponent: TComponent; filename: String): Boolean;

      { the two static procedures below should be used for production releases as they write smaller binary files that load/save faster}
      class procedure IniPropStorageRestoreProperties(aForm: TForm; aSessionFile: String); static;
      class procedure IniPropStorageSaveProperties(aForm: TForm; aSessionFile: String); static;
  end;

  { these two methods below can be used for debugging as they load/save human readable text files}
  function SaveCustomComponentSessionProperties(var aComponent: TComponent; filename: String): Boolean;
  function LoadCustomComponentSessionProperties(var aComponent: TComponent; filename: String): Boolean;

implementation

uses
  MegaConsts, MAlignGrid, MTreeBox, MTraceEdit, MClustalW;

function SaveCustomComponentSessionProperties(var aComponent: TComponent; filename: String): Boolean;
var
  SessionProps: TIniFormStream = nil;
begin
  try
    SessionProps := TIniFormStream.Create;
    Result := SessionProps.DebugSaveProperties(aComponent, filename);
  finally
    if Assigned(SessionProps) then
      SessionProps.Free;
  end;
end;

function LoadCustomComponentSessionProperties(var aComponent: TComponent; filename: String): Boolean;
var
  SessionProps: TIniFormStream = nil;
begin
  Result := False;
  try
    if FileExists(filename) then
    begin
      SessionProps := TIniFormStream.Create;
      Result := SessionProps.DebugRestoreProperties(aComponent, filename);
    end;
  finally
    if Assigned(SessionProps) then
      SessionProps.Free;
  end;
end;

{ TIniFormStream }

procedure TIniFormStream.ReadComponentSessionToStream;
var
  AStream: TMemoryStream = nil;
begin
  AStream := TMemoryStream.Create;
  try
    ReadStreamFromString(AStream);
    ReadComponentFromTextStream(AStream, FComponent, @OnFindClass);
  finally
    AStream.Free;
  end;
end;

procedure TIniFormStream.WriteComponentStreamToPropertiesString;
var
  AStream: TMemoryStream = nil;
begin
  AStream := TMemoryStream.Create;
  try
    WriteComponentAsBinaryToStream(AStream, FComponent);
    SaveStreamAsString(AStream);
  finally
    AStream.Free;
  end;
end;

constructor TIniFormStream.Create;
begin
  FComponent := nil;
end;

procedure TIniFormStream.ConvertBinaryDataToText;
var
  LRSStream: TMemoryStream = nil;
  LFMStream: TMemoryStream = nil;
begin
  LRSStream:=TMemoryStream.Create;
  LFMStream:=TMemoryStream.Create;
  try
    ReadStreamFromString(LRSStream);
    LRSObjectBinaryToText(LRSStream, LFMStream);
    FComponentPropertiesAsString := ReadStringFromStream(LFMStream);
  finally
    LRSStream.Free;
    LFMStream.Free;
  end;
end;

procedure TIniFormStream.SaveStreamAsString(AStream: TStream);
begin
  FComponentPropertiesAsString := ReadStringFromStream(AStream);
end;

procedure TIniFormStream.ReadStreamFromString(AStream: TStream);
begin
  AStream.Size := 0;
  if FComponentPropertiesAsString <> EmptyStr then
    AStream.Write(FComponentPropertiesAsString[1], length(FComponentPropertiesAsString));
  AStream.Position := 0;
end;

function TIniFormStream.ReadStringFromStream(AStream: TStream): string;
begin
  AStream.Position := 0;
  SetLength(Result, AStream.Size);
  if Result <> EmptyStr then
    AStream.Read(Result[1], length(Result));
end;

procedure TIniFormStream.OnFindClass(Reader: TReader; const AClassName: string; var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName, 'TAlignGrid') = 0 then
    ComponentClass := TAlignGrid
  else if CompareText(AClassName,'TTreeBox') = 0 then
    ComponentClass := TTreeBox
  else if CompareText(AClassName, 'TTraceEdit') = 0 then
    ComponentClass := TTraceEdit
  else if CompareText(AClassName, 'TClustalW') = 0 then
    ComponentClass := TClustalW;
end;

function TIniFormStream.DebugRestoreProperties(aComponent: TComponent; filename: String): Boolean;
var
  aList: TStringList = nil;
begin
  Result := False;
  try
    if FileExists(filename) then
    begin
      FComponent := aComponent;
      aList := TStringList.Create;
      aList.LoadFromFile(filename);
      if aList.Count > 0 then
      begin
        FComponentPropertiesAsString := aList.Text;
        ReadComponentSessionToStream;
        Result := True;
      end;
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TIniFormStream.DebugSaveProperties(aComponent: TComponent; filename: String): Boolean;
var
  aList: TStringList = nil;
begin
  Result := False;
  try
    FComponent := aComponent;
    WriteComponentStreamToPropertiesString;
    ConvertBinaryDataToText;
    aList := TStringList.Create;
    aList.Text := FComponentPropertiesAsString;
    aList.SaveToFile(filename);
    Result := FileExists(filename);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

class procedure TIniFormStream.IniPropStorageRestoreProperties(aForm: TForm; aSessionFile: String);
var
  i: Integer;
  aStream: TMemoryStream = nil;
begin
  aStream := TMemoryStream.Create;
  try
    try
      if FileExists(aSessionFile) then
      begin
        aStream.LoadFromFile(aSessionFile);
        for i := 0 to aForm.ComponentCount - 1 do
        begin
          if (aForm.Components[i].ClassName='TAlignGrid') or (aForm.Components[i].ClassName='TClustalW')
          or (aForm.Components[i].ClassName='TTraceEdit') or (aForm.Components[i].ClassName='TTreeBox') then
            begin
              aStream.ReadComponent(aForm.Components[i]);
              break;
            end;
        end;
      end
      else
      begin
        Assert(False, 'missing ini session properties file: ' + aSessionFile);
      end;
    except
      on  E:Exception do
      begin
        {$IFDEF DEBUG}
        ShowMessage('Application Error: Failed to read components from stream: ' + E.Message);
        {$ENDIF}
      end;
    end;
  finally
    if Assigned(aStream) then
      aStream.Free;
  end;
end;

class procedure TIniFormStream.IniPropStorageSaveProperties(aForm: TForm; aSessionFile: String);
var
  i: Integer;
  aStream: TMemoryStream = nil;
begin
  aStream := TMemoryStream.Create;
  try
    try
      for i := 0 to aForm.ComponentCount - 1 do
      begin
        if (aForm.Components[i].ClassName='TAlignGrid') or (aForm.Components[i].ClassName='TClustalW')
        or (aForm.Components[i].ClassName='TTraceEdit') or (aForm.Components[i].ClassName='TTreeBox') then
          begin
            aStream.WriteComponent(aForm.Components[i]);
            aStream.SaveToFile(aSessionFile);
            break;
          end;
      end;
    except
      on  E:Exception do
        if not IsSessionTest then
          ShowMessage('Application Error: failed to write components from stream: ' + E.Message);
    end;
  finally
    if Assigned(aStream) then
      aStream.Free;
  end;
end;

end.

