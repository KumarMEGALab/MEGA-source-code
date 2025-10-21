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

unit mtimetree_map;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, MegaConsts;

type

  { TTimeTreeTaxonData }

  TTimeTreeTaxonData = class(TObject)
    private
      FIsSelected: Boolean;
      FSynonym: String;
      FIsDescendantOneLineage: Boolean;
      FNcbiId: Integer;
      FOtuIndex: Integer;
      FOtuName: String;
      FSpeciesName: String;
      FTaxonomicRank: String;
      FTimetreeName: String;
      FTimetreeId: Integer;
      FUserQuery: String;
      function GetDescription: String;
      function GetLongDescription: String;
      function GetNcbiIdStr: String;
      function GetSearchString: String;
      function GetTimetreeDisplayName: String;
      procedure SetOtuName(AValue: String);
    public
      constructor Create;
      destructor Destroy; override;

      procedure Assign(Source: TTimeTreeTaxonData);
      function Clone: TTimeTreeTaxonData;
      function DebugString: String;
      class function DebugStringHeader: String; static;
      property TimetreeName: String read FTimetreeName write FTimetreeName;
      property TimetreeDisplayName: String read GetTimetreeDisplayName;
      property TimetreeId: Integer read FTimetreeId write FTimetreeId;
      property SpeciesName: String read FSpeciesName write FSpeciesName;
      property OtuName: String read FOtuName write SetOtuName;
      property OtuIndex: Integer read FOtuIndex write FOtuIndex;
      property NcbiId: Integer read FNcbiId write FNcbiId;
      property NcbiIdStr: String read GetNcbiIdStr;
      property IsDescendantOneLineage: Boolean read FIsDescendantOneLineage write FIsDescendantOneLineage;
      property TaxonomicRank: String read FTaxonomicRank write FTaxonomicRank;
      property Description: String read GetDescription;
      property LongDescription: String read GetLongDescription;
      property Synonym: String read FSynonym write FSynonym;
      property IsSelected: Boolean read FIsSelected write FIsSelected;
      property UserQuery: String read FUserQuery write FUserQuery;
      property SearchString: String read GetSearchString;
  end;

  PTimeTreeTaxonData = ^TTimeTreeTaxonData;
  TTimeTreeTaxonDataList = specialize TFPGList<TTimeTreeTaxonData>;
  TUpdateTimetreeDataProc = procedure(ttData: TTimetreeTaxonData) of object;

  { TTimeTreeStudyData }

  TTimeTreeStudyData = class(TObject)
    private
      FAgeEstimate: Double;
      FFirstAuthor: String;
      FNumTrees: Integer;
      FTitle: String;
      FYear: Integer;
    public
      constructor Create;
      procedure Assign(Source: TTimeTreeStudyData);
      property FirstAuthor: String read FFirstAuthor write FFirstAuthor;
      property Title: String read FTitle write FTitle;
      property Year: Integer read FYear write FYear;
      property NumTrees: Integer read FNumTrees write FNumTrees;
      property AgeEstimate: Double read FAgeEstimate write FAgeEstimate;
  end;

  TTimeTreeStudyList = specialize TFPGList<TTimeTreeStudyData>;

  { TTimeTreeMap }

  TTimeTreeMap = class(TObject)
    private
      FCalibrationType: TCalibrationCategory;
      FMaxTime: Double;
      FMedianTime: Double;
      FMinTime: Double;
      FTreeBoxNodeId: Integer;
      FTaxaData: TTimeTreeTaxonDataList;
      FTimetreeMrcaId: Integer;
      FTimeTreeStudyList: TTimeTreeStudyList;

      procedure Clear;
      function GetCount: Integer;
      function GetHasDataForBothDescendantLineages: Boolean;
      function GetItems(Index: integer): TTimeTreeTaxonData;
      function GetNumStudies: Integer;
      function GetTimeTreeStudyData(Index: Integer): TTimeTreeStudyData;
    public
      constructor Create(aTreeBoxNodeId: Integer);
      destructor Destroy; override;

      procedure Assign(Source: TTimeTreeMap);
      function Clone: TTimeTreeMap;
      procedure Add(aData: TTimeTreeTaxonData);
      procedure Remove(aData: TTimeTreeTaxonData);
      procedure AddStudyData(aStudyData: TTimeTreeStudyData);
      function GetCalibrationString: String;
      function NumDistinctTaxa: Integer;
      function DebugString: String;
      function DebugStrings: TStringList;
      class function DebugStringHeader: String; static;
      property TreeBoxNodeId: Integer read FTreeBoxNodeId write FTreeBoxNodeId;
      property MinTime: Double read FMinTime write FMinTime;
      property MaxTime: Double read FMaxTime write FMaxTime;
      property MedianTime: Double read FMedianTime write FMedianTime;
      property TimetreeMrcaId: Integer read FTimetreeMrcaId write FTimetreeMrcaId;
      property TaxaData: TTimeTreeTaxonDataList read FTaxaData;
      property Items[Index: integer]: TTimeTreeTaxonData read GetItems; default;
      property Count: Integer read GetCount;
      property HasDataForBothDescendantLineages: Boolean read GetHasDataForBothDescendantLineages;
      property CalibrationType: TCalibrationCategory read FCalibrationType write FCalibrationType;
      property Studies[Index: Integer]: TTimeTreeStudyData read GetTimeTreeStudyData;
      property NumStudies: Integer read GetNumStudies;
  end;

  TTimeTreeMapList = specialize TFPGList<TTimeTreeMap>;
  TUpdateTimetreeMapProc = procedure(ttMap: TTimeTreeMap) of object;

implementation

uses
  mstringbuilder;

{ TTimeTreeStudyData }

constructor TTimeTreeStudyData.Create;
begin
  FFirstAuthor := EmptyStr;
  FTitle := EmptyStr;
  FYear := -1;
  FNumTrees := 0;
  FAgeEstimate := 0.0;
end;

procedure TTimeTreeStudyData.Assign(Source: TTimeTreeStudyData);
begin
  FAgeEstimate := Source.FAgeEstimate;
  FFirstAuthor := Source.FFirstAuthor;
  FNumTrees := Source.FNumTrees;
  FTitle := Source.FTitle;
  FYear := Source.FYear;
end;

{ TTimeTreeTaxonData }

function TTimeTreeTaxonData.GetDescription: String;
begin
  Result := Format('%s  (%s) - %d', [FTimetreeName, FTaxonomicRank, FNcbiId]);
end;

function TTimeTreeTaxonData.GetLongDescription: String;
begin
  Result := Format('otuName: %s speciesName: %s timetreeName: %s ncbiId: %d rank: %s otuIndex: %d ttId: %d', [FOtuName, FSpeciesName, FTimetreeName, FNcbiId, FTaxonomicRank, FOtuIndex, FTimetreeId]);
end;

function TTimeTreeTaxonData.GetNcbiIdStr: String;
begin
  Result := IntToStr(FNcbiId);
end;

function TTimeTreeTaxonData.GetSearchString: String;
begin
  if FUserQuery <> EmptyStr then
    Result := FUserQuery
  else if FSynonym <> EmptyStr then
    Result := FSynonym
  else
    Result := FOtuName
end;

function TTimeTreeTaxonData.GetTimetreeDisplayName: String;
begin
  Result := Format('%s%s%s', ['<', FTimetreeName, '>']);
end;

procedure TTimeTreeTaxonData.SetOtuName(AValue: String);
begin
  if FOtuName = AValue then Exit;
  FOtuName := AValue;
end;

constructor TTimeTreeTaxonData.Create;
begin
  FTaxonomicRank := 'unknown';
  FIsSelected := False;
  FUserQuery := EmptyStr;
end;

destructor TTimeTreeTaxonData.Destroy;
begin
  inherited Destroy;
end;

procedure TTimeTreeTaxonData.Assign(Source: TTimeTreeTaxonData);
begin
  FIsDescendantOneLineage := Source.IsDescendantOneLineage;
  FNcbiId := Source.NcbiId;
  FOtuIndex := Source.OtuIndex;
  FOtuName := Source.OtuName;
  FTimetreeName := Source.TimetreeName;
  FTimetreeId := Source.TimetreeId;
  FSpeciesName := Source.SpeciesName;
  FTaxonomicRank := Source.TaxonomicRank;
  FSynonym := Source.Synonym;
  FIsSelected := Source.FIsSelected;
  FUserQuery := Source.FUserQuery;
end;

function TTimeTreeTaxonData.Clone: TTimeTreeTaxonData;
begin
  Result := TTimeTreeTaxonData.Create;
  Result.Assign(Self);
end;

function TTimeTreeTaxonData.DebugString: String;
begin
  Result := Format('%10d %-20s %-10s %-10s %-30s %-20s %10d %8d %-20s %-20s %s', [FOtuIndex, FOtuName, FTaxonomicRank, BoolToStr(FIsSelected, True), FTimetreeName, FSynonym, FNcbiId, FTimetreeId, FUserQuery, FSpeciesName, BoolToStr(FIsDescendantOneLineage, True)]);
end;

class function TTimeTreeTaxonData.DebugStringHeader: String;
begin
  Result := Format('%10s %-20s %-10s %-10s %-30s %-20s %10s %8s %-20s %-20s %s', ['OtuIndex', 'OtuName', 'Rank', 'Selected', 'TimetreeName', 'Synonym', 'NcbiId', 'ttId', 'UserQuery', 'SpeciesName', 'IsDescendantOne']);
end;

{ TTimeTreeMap }

procedure TTimeTreeMap.Clear;
var
  i: Integer = -1;
begin
  if Assigned(FTaxaData) and (FTaxaData.Count > 0) then
    FTaxaData.Clear;
  if Assigned(FTimeTreeStudyList) and (FTimeTreeStudyList.Count > 0) then
  begin
    for i := FTimeTreeStudyList.Count - 1 downto 0 do
      FTimeTreeStudyList[i].Free;
    FTimeTreeStudyList.Clear;
  end;
end;

function TTimeTreeMap.GetCount: Integer;
begin
  if Assigned(FTaxaData) then
    Result := FTaxaData.Count
  else
    Result := 0;
end;

function TTimeTreeMap.GetHasDataForBothDescendantLineages: Boolean;
var
  des1Found: Boolean = False;
  des2Found: Boolean = False;
  i: Integer = -1;
begin
  if FTaxaData.Count > 0 then
    for i := 0 to FTaxaData.Count - 1 do
      if FTaxaData[i].FIsDescendantOneLineage then
        des1Found := True
      else
        des2Found := True;
  Result := (des1Found and des2Found);
end;

function TTimeTreeMap.GetItems(Index: integer): TTimeTreeTaxonData;
begin
  if Index < FTaxaData.Count then
    Result := FTaxaData[Index];
end;

function TTimeTreeMap.GetNumStudies: Integer;
begin
  Result := 0;
  if Assigned(FTimeTreeStudyList) then
    Result := FTimeTreeStudyList.Count;
end;

function TTimeTreeMap.GetTimeTreeStudyData(Index: Integer): TTimeTreeStudyData;
begin
  Result := nil;
  if Assigned(FTimeTreeStudyList) and (FTimeTreeStudyList.Count > 0) and (Index < FTimeTreeStudyList.Count) then
    Result := FTimeTreeStudyList[Index];
end;

constructor TTimeTreeMap.Create(aTreeBoxNodeId: Integer);
begin
  FMaxTime := -1;
  FMinTime := -1;
  FTreeBoxNodeId := aTreeBoxNodeId;
  FTaxaData := TTimeTreeTaxonDataList.Create;
  FTimetreeMrcaId := -1;
  FCalibrationType := ccNone;
  FTimeTreeStudyList := TTimeTreeStudyList.Create;
end;

destructor TTimeTreeMap.Destroy;
begin
  if Assigned(FTaxaData) then
  begin
    Clear;
    FTaxaData.Free;
  end;
  if Assigned(FTimeTreeStudyList) then
    FTimeTreeStudyList.Free;
  inherited Destroy;
end;

procedure TTimeTreeMap.Assign(Source: TTimeTreeMap);
var
  aData: TTimeTreeTaxonData = nil;
  aStudy: TTimeTreeStudyData = nil;
  i: Integer;
begin
  if Source = Self then
    raise Exception.Create('assigning timetree map to itself');
  Assert(Source is TTimeTreeMap);
  FCalibrationType := Source.CalibrationType;
  FMaxTime := Source.MaxTime;
  FMinTime := Source.MinTime;
  FMedianTime := Source.MedianTime;
  FTreeBoxNodeId := Source.TreeBoxNodeId;
  FTimetreeMrcaId := Source.TimetreeMrcaId;
  Clear;
  if Source.FTaxaData.Count > 0 then
    for i := 0 to Source.FTaxaData.Count - 1 do
    begin
      aData := TTimeTreeTaxonData.Create;
      aData.Assign(Source.FTaxaData[i]);
      FTaxaData.Add(aData);
    end;
  if Source.NumStudies > 0 then
    for i := 0 to Source.NumStudies - 1 do
    begin
      aStudy := TTimeTreeStudyData.Create;
      aStudy.Assign(Source.Studies[i]);
      FTimeTreeStudyList.Add(aStudy);
    end;
end;

function TTimeTreeMap.Clone: TTimeTreeMap;
begin
  Result := TTimeTreeMap.Create(FTreeBoxNodeId);
  Result.Assign(Self);
end;

procedure TTimeTreeMap.Add(aData: TTimeTreeTaxonData);
begin
  FTaxaData.Add(aData);
end;

procedure TTimeTreeMap.Remove(aData: TTimeTreeTaxonData);
var
  i: Integer = -1;
begin
  if FTaxaData.Count > 0 then
    for i := 0 to FTaxaData.Count - 1 do
      if aData.FOtuIndex = FTaxaData[i].OtuIndex then
      begin
        FTaxaData.Delete(i);
        Exit;
      end;
end;

procedure TTimeTreeMap.AddStudyData(aStudyData: TTimeTreeStudyData);
begin
  FTimeTreeStudyList.Add(aStudyData);
end;

function TTimeTreeMap.GetCalibrationString: String;
var
  taxonA: String = '';
  taxonB: String = '';
  mrca: String = '';
begin
  taxonA := StringReplace(FTaxaData[0].OtuName, ' ', '_', [rfReplaceAll]);
  taxonB := StringReplace(FTaxaData[FTaxaData.Count - 1].OtuName, ' ', '_', [rfReplaceAll]);
  mrca := taxonA + '-' + taxonB + '-split';
  case FCalibrationType of
    ccDensityUniform:
      begin
        Result := Format('!MRCA=%s%s%s TaxonA=%s TaxonB=%s Distribution=uniform minTime=%.8f maxTime=%.8f', [#39, mrca, #39, taxonA, taxonB, FMinTime, FMaxTime]);
      end;
    ccMinTimeOnly:
      begin
        Result := Format('!MRCA=%s%s%s TaxonA=%s TaxonB=%s minTime=%.8f', [#39, mrca, #39, taxonA, taxonB, FMedianTime]);
      end;
    ccMaxTimeOnly:
      begin
        Result := Format('!MRCA=%s%s%s TaxonA=%s TaxonB=%s maxTime=%.8f', [#39, mrca, #39, taxonA, taxonB, FMedianTime]);
      end;
    ccFixedTime:
      begin
        Result := Format('!MRCA=%s%s%s TaxonA=%s TaxonB=%s Time=%.8f', [#39, mrca, #39, taxonA, taxonB, FMedianTime]);
      end;
  end;
end;

function TTimeTreeMap.NumDistinctTaxa: Integer;
var
  counter: TStringList = nil;
  i: Integer = -1;
  ttData: TTimeTreeTaxonData = nil;
begin
  Result := 0;
  try
     counter := TStringList.Create;
     counter.Sorted := True;
     counter.Duplicates := dupIgnore;
     if FTaxaData.Count > 0 then
       for i := 0 to FTaxaData.Count - 1 do
       begin
         ttData := FTaxaData[i];
         counter.Add(ttData.NcbiIdStr);
       end;
     Result := counter.Count;
  finally
    if Assigned(counter) then
      counter.Free;
  end;
end;

function TTimeTreeMap.DebugString: String;
var
  b: TMegaStringBuilder = nil;
  i: Integer = -1;
begin
  try
    b := TMegaStringBuilder.Create;
    b.Add(Format('%6d %10.2f %10.2f %10.2f %d', [FTreeBoxNodeId, FMedianTime, FMinTime, FMaxTime, FTimetreeMrcaId]));
    b.Add(NewLine);
    b.Add(TTimeTreeTaxonData.DebugStringHeader);
    b.Add(NewLine);
    if FTaxaData.Count > 0 then
      for i := 0 to FTaxaData.Count - 1 do
      begin
        b.Add(FTaxaData[i].DebugString);
        b.Add(NewLine);
      end;
    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

function TTimeTreeMap.DebugStrings: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := DebugString;
end;

class function TTimeTreeMap.DebugStringHeader: String;
begin
  Result := Format('%6s %10s %10s %10s %s', ['TBoxId', 'Median', 'Min', 'Max', 'ttId']);
end;

end.

