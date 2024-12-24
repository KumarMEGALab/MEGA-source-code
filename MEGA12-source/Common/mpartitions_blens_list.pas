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

unit mpartitions_blens_list;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, MegaConsts, mextendedlist, StrHashMap;

type

  { TAvgBLensList }
  { Stores branch lengths for a given partition of tree data across bootstrap replicates}
  TAvgBLensList = class(TExtendedList)
    private
      FBranchIndex: Integer;
      FFrequency: Double;
      FIsOTU: Boolean;
      FPartitionString: String;
    public
      constructor Create;
      function GetPartitionInfo(var avg: Extended; var sd: Extended; var pString: String; var bIndex: Integer): Boolean;
      function CsvString: String;
      function GetMean: Extended;
      procedure Add(aValue: Extended; aPartitionString: String); overload;
      property BranchIndex: Integer read FBranchIndex write FBranchIndex;
      property PartitionString: String read FPartitionString write FPartitionString;
      property Frequency: Double read FFrequency write FFrequency;
      property IsOTU: Boolean read FIsOTU write FIsOTU;

  end;

  { TPartitionsBLensList }
  { A data structure for storing branch lengths for all partitions when doing bootstrap and then summarizing the information}
  TPartitionsBLensList = class(TList)
    private
      FNumTaxa: Integer;
      function GetItem(Index: Int64): TAvgBLensList;
      function GetOtuPartitionString(index: Integer): String;
      procedure SetItem(Index: Int64; AValue: TAvgBLensList);
      function CsvHeaderString: String;
      procedure InitializeOTUBlens;
    public
      constructor Create(numTaxa: Integer);
      destructor Destroy; override;
      procedure AddNewPartition(aVal: Extended; pString: String);
      procedure Clear; override;
      procedure SortAscendingByBranchIndex;
      procedure RemoveExtraneousPartitions;
      function GetHashList: TStringHashMap;
      {$IFNDEF VISUAL_BUILD}
      procedure DumpToCsvFile(filename: String);
      {$ENDIF}
      property Items[Index: Int64]: TAvgBLensList read GetItem write SetItem; default;
  end;

  //TArrayOfPartitionsBLensList = array of TPartitionsBLensList;

  function CompareAvgLensList(Item1: Pointer; Item2: Pointer): Integer; { sort by BranchIndex}

implementation

uses
  {$IFNDEF VISUAL_BUILD}MegaUtils_NV,{$ENDIF}
  math;

function CompareAvgLensList(Item1: Pointer; Item2: Pointer): Integer;
var
  val1, val2: Integer;
begin
  val1 := TAvgBLensList(Item1).BranchIndex;
  val2 := TAvgBLensList(Item2).BranchIndex;
  Result := CompareValue(val1, val2);
end;

{ TPartitionsBLensList }

function TPartitionsBLensList.GetItem(Index: Int64): TAvgBLensList;
var
  p: Pointer;
begin
  Assert(Index < Count, Format('index (%d) out of bounds. Count is %d', [Index, Count]));
  Result := nil;
  p := inherited Items[Index];
  if Assigned(p) then
    Result := TAvgBLensList(p);
end;

function TPartitionsBLensList.GetOtuPartitionString(index: Integer): String;
var
  i: Integer;
begin
  SetLength(Result, FNumTaxa);
  for i := 1 to FNumTaxa do
    if i = index then
      Result[i] := '1'
    else
      Result[i] := '0';
end;

procedure TPartitionsBLensList.SetItem(Index: Int64; AValue: TAvgBLensList);
begin
  inherited Items[Index] := Pointer(AValue);
end;

function TPartitionsBLensList.CsvHeaderString: String;
begin
  {$IFDEF DEBUG}
  Result := 'branch_index,mean,std_dev,frequency,frequency2,partition';
  {$ELSE}
  Result := 'mean,std_dev,frequency,partition';
  {$ENDIF}

end;

procedure TPartitionsBLensList.InitializeOTUBlens;
var
  i: Integer;
  aItem: TAvgBLensList = nil;
begin
  for i := 1 to FNumTaxa do
  begin
    aItem := TAvgBLensList.Create;
    aItem.BranchIndex := i - 1;
    aItem.PartitionString := GetOtuPartitionString(i);
    aItem.Frequency := 0;
    aItem.IsOTU := True;
    inherited Add(Pointer(aItem));
  end;
end;

constructor TPartitionsBLensList.Create(numTaxa: Integer);
begin
  inherited Create;
  FNumTaxa := numTaxa;
  InitializeOTUBlens;
end;

destructor TPartitionsBLensList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TPartitionsBLensList.AddNewPartition(aVal: Extended; pString: String);
var
  aItem: TAvgBLensList = nil;
begin
  aItem := TAvgBLensList.Create;
  aItem.Add(aVal);
  aItem.PartitionString := pString;
  inherited Add(Pointer(aItem));
end;

procedure TPartitionsBLensList.Clear;
var
  i: Integer;
  aItem: TAvgBLensList = nil;
begin
  if Count > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      aItem := GetItem(i);
      aItem.Free;
    end;
    inherited Clear;
  end;
end;

procedure TPartitionsBLensList.SortAscendingByBranchIndex;
begin
  Sort(CompareAvgLensList)
end;

procedure TPartitionsBLensList.RemoveExtraneousPartitions;
var
  i: Integer;
  aItem: TAvgBLensList = nil;
begin
  if Count > 0 then
    for i := Count - 1 downto 0 do
    begin
      aItem := GetItem(i);
      if aItem.BranchIndex = MaxInt then
      begin
        aItem.Free;
        Inherited Delete(i);
      end;
    end;
  Pack;
end;

function TPartitionsBLensList.GetHashList: TStringHashMap;
var
  i: Integer;
  data: TAvgBLensList = nil;
  p: Pointer = nil;
begin
  Result := TStringHashMap.Create;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      data := GetItem(i);
      p := Pointer(data);
      Result.Add(data.PartitionString, p);
    end;
end;

{$IFNDEF VISUAL_BUILD}
procedure TPartitionsBLensList.DumpToCsvFile(filename: String);
var
  aFile: TextFile;
  i: Integer;
begin
  try
    try
      AssignFile(aFile, filename);
      Rewrite(aFile);
      WriteLn(aFile, CsvHeaderString);
      if Count > 0 then
        for i := 0 to Count - 1 do
        begin
          if GetItem(i).BranchIndex = MaxInt then
            break;
          WriteLn(aFile, GetItem(i).CsvString);
        end;
    except
      on E:Exception do
        warn_nv('error when saving partition branch lengths to file: ' + E.Message);
    end;
  finally
    CloseFile(aFile);
  end;
end;
{$ENDIF}

{ TAvgBLensList }

constructor TAvgBLensList.Create;
begin
  inherited Create;
  FBranchIndex := MaxInt;
  FPartitionString := EmptyStr;
  FFrequency := -1.0;
  FIsOTU := False;
end;

function TAvgBLensList.GetPartitionInfo(var avg: Extended; var sd: Extended; var pString: String; var bIndex: Integer): Boolean;
var
  i: Integer;
  vals: array of Extended;
begin
  Result := False;
  if Count = 0 then
  begin
    avg := 0.0;
    sd := 0.0;
    pString := '-';
    bIndex := FBranchIndex;
    Exit;
  end
  else if Count = 1 then
  begin
    avg := Items[0];
    sd := 0.0;
    pString := '" ' + PartitionString + ' "';
    bIndex := FBranchIndex;
    Exit;
  end;

  SetLength(vals, Count);
  for i := 0 to Count - 1 do
    vals[i] := Items[i];
  try
    meanandstddev(vals, avg, sd);
    pString := '" ' + PartitionString + ' "';
    bIndex := FBranchIndex;
  except
    Result := False;
  end;
  Result := True;
end;

function TAvgBLensList.CsvString: String;
var
  m: Extended = 0;
  sd: Extended = 0;
  p: String = '';
  b: Integer = -1;
  vals: String = '';
  i: Integer;
begin
  GetPartitionInfo(m, sd, p, b);
  if FBranchIndex = MaxInt then
    b := -1;
  {$IFDEF DEBUG}
  if Count > 0 then
    for i := 0 to Count - 1 do
      vals := vals + Format('%3e,', [GetItem(i)]);
  Result := Format('%d,%.3e,%.3e,%d,%.3f,%s,%s', [b, m, sd, Count, FFrequency, p, vals]);
  {$ELSE}
  Result := Format('%.3e,%.3e,%d,%s', [m, sd, Count, p]);
  {$ENDIF}
end;

function TAvgBLensList.GetMean: Extended;
var
  i: Integer;
begin
  if Count = 0 then
    Exit(0)
  else if Count = 1 then
    Exit(Items[0]);

  Result := 0;
  for i := 0 to Count - 1 do
    Result += Items[i];
  Result := Result/Count;
end;

procedure TAvgBLensList.Add(aValue: Extended; aPartitionString: String);
begin
  if aPartitionString <> FPartitionString then
    raise Exception.Create(Format('mismatched partition identifiers %s vs %s', [FPartitionString, aPartitionString]));
  inherited Add(aValue);
end;



end.

