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

unit mmodel_info_list;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, MLTree;

type

  { TModelInfoList }

  TModelInfoList = class(TList)
  private
    function GetItems(index: integer): TModelInfo;
    procedure SetItems(index: integer; newitem: TModelInfo);
    function MaxModels(isAminoAcid: Boolean): Integer;
  public
    procedure DeleteAll;
    property Items[Index: integer]: TModelInfo read GetItems write SetItems; default;
    procedure Assign(Source: TModelInfoList);
    function AddByDataType(Item: Pointer; isAminoAcid: Boolean): Integer;
    function ContainsSameModelInfo(aModelInfo: TModelInfo): Boolean;
    function GetBaseModel(baseModelName: String): TModelInfo;
    function ModelNames: String;
  end;

TModelInfoListArray = array of TModelInfoList;

implementation

uses
  mstringbuilder;

procedure TModelInfoList.Assign(Source: TModelInfoList);
var
  i: Integer = -1;
  AInfo: TModelInfo = nil;
begin
  Assert(Source <> Self, 'assigning a TModelInfoList to itself');
  if Count > 0 then
    DeleteAll;
  if Source.Count > 0 then
  begin
    for i := 0 to Source.Count - 1 do
    begin
      AInfo := TModelInfo.Create;
      AInfo.Assign(Source[i]);
      Add(AInfo);
    end;
  end;
end;

function TModelInfoList.AddByDataType(Item: Pointer; isAminoAcid: Boolean): Integer;
begin
  Result := inherited Add(Item);
  if Result >= MaxModels(isAminoAcid) then
    raise Exception.Create(Format('TModelInfoList has more models (%d) than allowed when using amino acid data is %s ', [Result, MaxModels(isAminoAcid), BoolToStr(isAminoAcid, True)]));
end;

function TModelInfoList.ContainsSameModelInfo(aModelInfo: TModelInfo): Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      if aModelInfo.FullName = Items[i].FullName then
        Exit(True);
    end;
end;

function TModelInfoList.GetBaseModel(baseModelName: String): TModelInfo;
var
  i: Integer = -1;
begin
  Result := nil;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      if Items[i].BaseModelName = baseModelName then
      begin
        Result := Items[i];
        Exit;
      end;
    end;
end;

function TModelInfoList.ModelNames: String;
var
  builder: TMegaStringBuilder = nil;
  i: Integer = -1;
  m: TModelInfo = nil;
begin
  Result := EmptyStr;
  try
    builder := TMegaStringBuilder.Create;
    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        m := Items[i];
        builder.Add(m.ModelName);
        if i < Count - 1 then
          builder.Add(', ');
      end;
    Result := builder.GenerateString;
  finally
    if Assigned(builder) then
      builder.Free;
  end;
end;

function TModelInfoList.GetItems(index: integer): TModelInfo;
begin
  result := inherited Items[index];
end;

procedure TModelInfoList.SetItems(index: integer; newitem: TModelInfo);
begin
  inherited Items[index] := newitem;
end;

function TModelInfoList.MaxModels(isAminoAcid: Boolean): Integer;
begin
  if isAminoAcid then
    Result := 64
  else
    Result := 24;
end;

procedure TModelInfoList.DeleteAll;
var
  i: Integer;
  aModel: TModelInfo = nil;
begin
  if Count > 0 then
    for i := Count - 1 downto 0 do
    begin
      aModel := GetItems(i);
      if Assigned(aModel) then
        aModel.Free;
    end;
  inherited Clear;
end;

end.

