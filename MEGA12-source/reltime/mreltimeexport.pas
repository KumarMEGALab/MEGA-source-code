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

unit mreltimeexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExcelWrite, MegaConsts, mreltimetreenode;

const
  NUM_DECIMALS = 6;
type

  TReltimeExportType = (retMinMaxTimes, retReltimes, retSampleTimes, retStrictClock, retElapsedTimes);
  { TReltimeExportBuilder }

  TReltimeExportBuilder = class(TObject)
    private
      FComputer: TObject;
      FNodes: TReltimeTreeNodeArray;
      FIsBLensOnly: Boolean;
      FNames: TStringList;
      FLabels: TStringList;
      FCaption: TStringList;
      FMinTimes: ArrayOfExtended;
      FMaxTimes: ArrayOfExtended;
      FSampleTimes: ArrayOfExtended;
      FReltimeExportType: TReltimeExportType;
      FExport: TExcelWrite;
      FInitialized: Boolean;
      procedure SetReltimeExportType(AValue: TReltimeExportType);
    protected
      procedure SetNames(aNames: TStringList);
      procedure SetLabels(aLabels: TStringList);
      procedure AddHeader; virtual;
      procedure AddLeafNodes; virtual;
      procedure AddInternalNodes; virtual;
      function GetInternalNodeLabel(TreeDataIndex: Integer; n: TReltimeTreeNode): String;
      function NumColumns: Integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Initialize(computer: TObject; n: TReltimeTreeNodeArray; names: TStringList; labels: TStringList; isBlensOnly: Boolean; MinTimes: array of Extended; MaxTimes: array of Extended); overload;
      procedure Initialize(computer: TObject; n: TReltimeTreeNodeArray; names: TStringList; labels: TStringList; isBlensOnly: Boolean; sampleTimes: array of Extended); overload;
      procedure Initialize(computer: TObject; n: TReltimeTreeNodeArray; names: TStringList; labels: TStringList; isBlensOnly: Boolean); overload;
      procedure AddCaptionAsWorksheet(aCaption: TStringList);
      function GenerateExport: TExcelWrite; overload;
      function GenerateExport(n: TRelTimeTreeNodeArray; isBlensOnly: Boolean; MinTimes: array of Extended; MaxTimes: array of Extended): TExcelWrite; overload;
      function GenerateExport(n: TRelTimeTreeNodeArray; isBlensOnly: Boolean; sampleTimes: array of Extended): TExcelWrite; overload;
      function GenerateExport(n: TRelTimeTreeNodeArray; isBlensOnly: Boolean): TExcelWrite; overload;
      property IsInitialized: Boolean read FInitialized;
      property ReltimeExportType: TReltimeExportType read FReltimeExportType write SetReltimeExportType;
  end;

  { TElapsedTimesExportBuilder }

  TElapsedTimesExportBuilder = class(TReltimeExportBuilder)
    protected
      procedure AddHeader; override;
      procedure AddLeafNodes; override;
      procedure AddInternalNodes; override;
  end;

implementation

uses
  mreltimecomputer;

{ TElapsedTimesExportBuilder }

procedure TElapsedTimesExportBuilder.AddHeader;
var
  aRect: TRect;
begin
  FExport.Add('NodeLabel');
  FExport.Add('NodeId');
  FExport.Add('Outgroup');
  FExport.Add('Des1');
  FExport.Add('Des2');
  FExport.Add('RelTime');
  FExport.Add('StdErr(RelTime)');
  FExport.Add('Des1 BLen');
  FExport.Add('Des2 BLen');
  FExport.Add('Des1 Rate');
  FExport.Add('Des2 Rate');
  if (FReltimeExportType = retMinMaxTimes) or (FReltimeExportType = retSampleTimes) or (FReltimeExportType = retStrictClock) then
    FExport.Add('DivTime');
  if (FReltimeExportType = retSampleTimes) then
    FExport.Add('Elapsed (days)');
  FExport.Add('CI_Lower');
  FExport.Add('CI_Upper');
  if FReltimeExportType = retMinMaxTimes then
  begin
    FExport.Add('CI_Lower_No_Constraint');
    FExport.Add('CI_Upper_No_Constraint');
  end;

  if (FReltimeExportType = retMinMaxTimes) or (FReltimeExportType = retSampleTimes) then
  begin
    if FReltimeExportType = retMinMaxTimes then
    begin
      FExport.Add('MinTime');
      FExport.Add('MaxTime');
    end
    else
      FExport.Add('SampleTime');
  end;
  FExport.Add('Des1 Elapsed Time');
  FExport.Add('Des2 Elapsed Time');
  if not FIsBlensOnly then
    FExport.Add('Data Coverage');
  FExport.WriteLine;
  aRect := Rect(0, 0, NumColumns - 1, 0);
  FExport.BoldCells(aRect);
  FExport.AlignCells(aRect, aCenter, aCenter);
end;

procedure TElapsedTimesExportBuilder.AddLeafNodes;
var
  i: Integer;
  cpu: TReltimeComputer;
begin
  cpu := TReltimeComputer(FComputer);
  for i := 0 to FNames.Count - 1 do
  begin
    FExport.Add(FNames[i]);
    FExport.Add(FNodes[i].Index + 1); { NodeId}
    if FNodes[i].IsOutgroupMember or FNodes[i].flag then
      FExport.Add('yes')
    else
      FExport.Add('no');
    FExport.AddBlankCell;   { Des1}
    FExport.AddBlankCell;   { Des2}
    FExport.AddBlankCell;   { RelTime}
    FExport.AddBlankCell;   { StdErr (RelTime)}
    FExport.AddBlankCell;   { Des1 blen}
    FExport.AddBlankCell;   { Des2 blen}
    FExport.AddBlankCell;   { Des1 rate}
    FExport.AddBlankCell;   { Des2 rate}
    if (FReltimeExportType = retSampleTimes) then
    begin
      FExport.AddBlankCell; { DivTime}
      FExport.Add(cpu.ElapsedDaysForRtdtTime(i));
      FExport.AddBlankCell; { CI_Lower}
      FExport.AddBlankCell; { CI_Higher}
      if FNodes[i].Time > FP_CUTOFF then
        FExport.Add(cpu.RtdtFloatTimeToDateString(FNodes[i].Time))
      else
        FExport.AddBlankCell;
    end;
    FExport.AddBlankCell; { Des1 elapsed time}
    FExport.AddBlankCell; { Des2 elapsed time}
    FExport.WriteLine;
  end;
end;

procedure TElapsedTimesExportBuilder.AddInternalNodes;
const
  PercentString = '%13.2f%%';
var
  i: Integer;
  NodeLabel: String;
  NoOfNames: Integer;
  cpu: TReltimeComputer;
begin
  cpu := TReltimeComputer(FComputer);
  NoOfNames := FNames.Count;
  for i := NoOfNames to 2*NoOfNames-2 do
  begin
    NodeLabel := GetInternalNodeLabel(cpu.MapNodeIndexToTreeDataIndex(FNodes[i].Index), FNodes[i]);
    if Trim(NodeLabel) <> EmptyStr then
      FExport.Add(NodeLabel)
    else
      FExport.AddBlankCell;

    FExport.Add(FNodes[i].Index + 1);
    if FNodes[i].flag then { in this context, flag indicates outgroup member}
      FExport.Add('yes')
    else
      FExport.Add('no');
    FExport.Add(FNodes[i].des1.Index + 1);
    FExport.Add(FNodes[i].des2.Index + 1);
    if FNodes[i].flag then
    begin
      FExport.AddBlankCell; { Height}
      FExport.AddBlankCell; { vh}
      FExport.AddP(FNodes[i].des1.blen, NUM_DECIMALS); { Des1 Blen}
      FExport.AddP(FNodes[i].des2.blen, NUM_DECIMALS); { Des2 Blen}
      FExport.AddP(FNodes[i].des1.rate, NUM_DECIMALS); { Des1 Rate}
      FExport.AddP(FNodes[i].des2.rate, NUM_DECIMALS); { Des2 Rate}
    end
    else
    begin
      FExport.AddP(FNodes[i].Height, NUM_DECIMALS);
      FExport.AddP(sqrt(FNodes[i].vh), NUM_DECIMALS);
      FExport.AddP(FNodes[i].des1.blen, NUM_DECIMALS);
      FExport.AddP(FNodes[i].des2.blen, NUM_DECIMALS);
      FExport.AddP(FNodes[i].des1.rate, NUM_DECIMALS);
      FExport.AddP(FNodes[i].des2.rate, NUM_DECIMALS);
    end;

    if (FReltimeExportType = retMinMaxTimes) then
    begin
      if FNodes[i].flag then
      begin
        FExport.AddBlankCell;  { DivTime}
        FExport.AddBlankCell;  { CI_Low}
        FExport.AddBlankCell;  { CI_High}
        FExport.AddBlankCell;  { CI_Low_Raw}
        FExport.AddBlankCell;  {CI_High_Raw}
        FExport.AddBlankCell;  { MinTime}
        FExport.AddBlankCell;  { MaxTime}
        FExport.AddBlankCell;  { des1 elapsed time}
        FExport.AddBlankCell;  { des2 elapsed time}
      end
      else
      begin
        FExport.AddP(cpu.DivTime(i), NUM_DECIMALS);
        FExport.AddP(cpu.MinDivTime(i), NUM_DECIMALS);
        FExport.AddP(cpu.MaxDivTime(i), NUM_DECIMALS);
        if FMinTimes[i] > FP_CUTOFF then
          FExport.AddP(cpu.MinDivTimeRaw(i), NUM_DECIMALS)
        else
          FExport.AddBlankCell;
        if FMaxTimes[i] > FP_CUTOFF then
          FExport.AddP(cpu.MaxDivTimeRaw(i), NUM_DECIMALS)
        else
          FExport.AddBlankCell;

        if FMinTimes[i] > FP_CUTOFF then
          FExport.AddP(FMinTimes[i], NUM_DECIMALS)
        else
          FExport.AddBlankCell;
        if FMaxTimes[i] > FP_CUTOFF then
          FExport.AddP(FMaxTimes[i], NUM_DECIMALS)
        else
          FExport.AddBlankCell;

        if FNodes[i].des1.OTU then
          FExport.AddP(cpu.DivTime(i), NUM_DECIMALS)
        else
         FExport.AddP(cpu.DivTime(i) - cpu.DivTime(FNodes[i].des1.index), NUM_DECIMALS);

        if FNodes[i].des2.OTU then
          FExport.AddP(cpu.DivTime(i), NUM_DECIMALS)
        else
         FExport.AddP(cpu.DivTime(i) - cpu.DivTime(FNodes[i].des2.index), NUM_DECIMALS);
      end;
    end
    else if FReltimeExportType = retSampleTimes then
    begin
      if FNodes[i].flag then
      begin
        FExport.AddBlankCell;  { DivTime}
        FExport.AddBlankCell;  { Elapsed (day)}
        FExport.AddBlankCell;  { CI_Low}
        FExport.AddBlankCell;  { CI_High}
        FExport.AddBlankCell;  { SampleTime}
        FExport.AddBlankCell;  { des1 elapsed time}
        FExport.AddBlankCell;  { des2 elapsed time}
      end
      else
      begin
        FExport.Add(cpu.RtdtFloatTimeToDateString(cpu.DivTime(i)));
        FExport.Add(cpu.ElapsedDaysForRtdtTime(i));
        FExport.Add(cpu.RtdtFloatTimeToDateString(cpu.MinDivTime(i)));
        FExport.Add(cpu.RtdtFloatTimeToDateString(cpu.MaxDivTime(i)));
        if FNodes[i].Time > FP_CUTOFF then
          FExport.Add(cpu.RtdtFloatTimeToDateString(FNodes[i].Time))
        else
          FExport.AddBlankCell;

        if FNodes[i].des1.OTU then
          FExport.AddP(cpu.DivTime(i), NUM_DECIMALS)
        else
         FExport.AddP(abs(cpu.DivTime(i) - cpu.DivTime(FNodes[i].des1.index)), NUM_DECIMALS);

        if FNodes[i].des2.OTU then
          FExport.AddP(cpu.DivTime(i), NUM_DECIMALS)
        else
         FExport.AddP(abs(cpu.DivTime(i) - cpu.DivTime(FNodes[i].des2.index)), NUM_DECIMALS);
      end;
    end
    else if FReltimeExportType = retStrictClock then
    begin
      if FNodes[i].flag then
      begin
        FExport.AddBlankCell;  { DivTime}
        FExport.AddBlankCell;  { CI_Low}
        FExport.AddBlankCell;  { CI_High}
      end
      else
      begin
        FExport.AddP(cpu.DivTime(i), NUM_DECIMALS);
        FExport.AddP(cpu.MinDivTime(i), NUM_DECIMALS);
        FExport.AddP(cpu.MaxDivTime(i), NUM_DECIMALS);
      end;
    end
    else if FReltimeExportType = retReltimes then
    begin
      if FNodes[i].flag then
      begin
        FExport.AddBlankCell;
        FExport.AddBlankCell;
        FExport.AddBlankCell;
        FExport.AddBlankCell;
      end
      else
      begin
        FExport.AddP(cpu.MinDivTime(i), NUM_DECIMALS);
        FExport.AddP(cpu.MaxDivTime(i), NUM_DECIMALS);
        if FNodes[i].des1.OTU then
          FExport.AddP(cpu.DivTime(i), NUM_DECIMALS)
        else
         FExport.AddP(abs(cpu.DivTime(i) - cpu.DivTime(FNodes[i].des1.index)), NUM_DECIMALS);

        if FNodes[i].des2.OTU then
          FExport.AddP(cpu.DivTime(i), NUM_DECIMALS)
        else
         FExport.AddP(abs(cpu.DivTime(i) - cpu.DivTime(FNodes[i].des2.index)), NUM_DECIMALS);
      end;
    end
    else
      raise Exception.Create('invalid Reltime export type');
    if not FIsBlensOnly then
      FExport.Add(Trim(Format(PercentString, [FNodes[i].DataCoverage * 100])));
    FExport.WriteLine;
  end;
end;

{ TReltimeExportBuilder }

procedure TReltimeExportBuilder.SetReltimeExportType(AValue: TReltimeExportType);
begin
  if FReltimeExportType=AValue then Exit;
  FReltimeExportType:=AValue;
end;

procedure TReltimeExportBuilder.SetNames(aNames: TStringList);
begin
  FNames.Assign(aNames)
end;

procedure TReltimeExportBuilder.SetLabels(aLabels: TStringList);
begin
  if Assigned(aLabels) then { labels are optional}
    FLabels.Assign(aLabels);
end;

procedure TReltimeExportBuilder.AddHeader;
var
  aRect: TRect;
  cpu: TReltimeComputer;
begin
  cpu := TReltimeComputer(FComputer);
  FExport.Add('NodeLabel');
  FExport.Add('NodeId');
  FExport.Add('Outgroup');
  FExport.Add('Des1');
  FExport.Add('Des2');
  if IsDeveloper  and (not cpu.IsSamplingTime) and (Length(cpu.OriginalReltimes) > 0) then
    FExport.Add('Orig_RelTime');
  FExport.Add('RelTime');
  FExport.Add('StdErr(RelTime)');
  if IsDeveloper then
    FExport.Add('BLen');
  FExport.Add('Rate');
  if (FReltimeExportType = retMinMaxTimes) or (FReltimeExportType = retSampleTimes) or (FReltimeExportType = retStrictClock) then
    FExport.Add('DivTime');
  if (FReltimeExportType = retSampleTimes) then
    FExport.Add('Elapsed (days)');
  FExport.Add('CI_Lower');
  FExport.Add('CI_Upper');
  if FReltimeExportType = retMinMaxTimes then
  begin
    FExport.Add('CI_Lower_No_Constraint');
    FExport.Add('CI_Upper_No_Constraint');
  end;

  if (FReltimeExportType = retMinMaxTimes) or (FReltimeExportType = retSampleTimes) then
  begin
    if FReltimeExportType = retMinMaxTimes then
    begin
      FExport.Add('MinTime');
      FExport.Add('MaxTime');
    end
    else
      FExport.Add('SampleTime');
  end;
  if not FIsBlensOnly then
    FExport.Add('Data Coverage');
  FExport.WriteLine;
  aRect := Rect(0, 0, NumColumns - 1, 0);
  FExport.BoldCells(aRect);
  FExport.AlignCells(aRect, aCenter, aCenter);
end;

procedure TReltimeExportBuilder.AddLeafNodes;
var
  i: Integer;
  cpu: TReltimeComputer;
begin
  cpu := TReltimeComputer(FComputer);
  for i := 0 to FNames.Count - 1 do
  begin
    FExport.Add(FNames[i]);
    FExport.Add(FNodes[i].Index + 1); { NodeId}
    if FNodes[i].IsOutgroupMember or FNodes[i].flag then
      FExport.Add('yes')
    else
      FExport.Add('no');
    FExport.AddBlankCell;   { Des1}
    FExport.AddBlankCell;   { Des2}
    if IsDeveloper and (not cpu.IsSamplingTime) and (Length(cpu.OriginalReltimes) > 0) then
      FExport.AddBlankCell; { Orig_RelTime}
    FExport.AddBlankCell;   { RelTime}
    FExport.AddBlankCell;   { StdErr (RelTime)}
    if IsDeveloper then
      FExport.AddP(FNodes[i].BLen, NUM_DECIMALS);
    FExport.AddP(FNodes[i].Rate, NUM_DECIMALS);
    if (FReltimeExportType = retSampleTimes) then
    begin
      FExport.AddBlankCell; { DivTime}
      FExport.Add(cpu.ElapsedDaysForRtdtTime(i));
      FExport.AddBlankCell; { CI_Lower}
      FExport.AddBlankCell; { CI_Higher}
      if FNodes[i].Time > FP_CUTOFF then
        FExport.Add(cpu.RtdtFloatTimeToDateString(FNodes[i].Time))
      else
        FExport.AddBlankCell;
    end;
    FExport.WriteLine;
  end;
end;

procedure TReltimeExportBuilder.AddInternalNodes;
const
  PercentString = '%13.2f%%';
var
  i: Integer;
  NodeLabel: String;
  NoOfNames: Integer;
  cpu: TReltimeComputer;
begin
  cpu := TReltimeComputer(FComputer);
  NoOfNames := FNames.Count;
  for i := NoOfNames to 2*NoOfNames-2 do
  begin
    NodeLabel := GetInternalNodeLabel(cpu.MapNodeIndexToTreeDataIndex(FNodes[i].Index), FNodes[i]);
    if Trim(NodeLabel) <> EmptyStr then
      FExport.Add(NodeLabel)
    else
      FExport.AddBlankCell;

    FExport.Add(FNodes[i].Index + 1);
    if FNodes[i].flag then { in this context, flag indicates outgroup member}
      FExport.Add('yes')
    else
      FExport.Add('no');
    FExport.Add(FNodes[i].des1.Index+1);
    FExport.Add(FNodes[i].des2.Index+1);
    if FNodes[i].flag then
    begin
      if IsDeveloper and (not cpu.IsSamplingTime) and (i < Length(cpu.OriginalReltimes)) then
        FExport.AddBlankCell; { Original Reltimes}
      FExport.AddBlankCell; { Height}
      FExport.AddBlankCell; { vh}
      if IsDeveloper then
        FExport.AddBlankCell; { Blen}
      FExport.AddBlankCell; { Rate}
    end
    else
    begin
      if IsDeveloper and (not cpu.IsSamplingTime) and (i < Length(cpu.OriginalReltimes)) then
        FExport.AddP(cpu.OriginalReltimes[i], NUM_DECIMALS);
      FExport.AddP(FNodes[i].Height, NUM_DECIMALS);
      FExport.AddP(sqrt(FNodes[i].vh), NUM_DECIMALS);
      if IsDeveloper then
        FExport.AddP(FNodes[i].BLen, NUM_DECIMALS);
      FExport.AddP(FNodes[i].Rate, NUM_DECIMALS);
    end;

    if (FReltimeExportType = retMinMaxTimes) then
    begin
      if FNodes[i].flag then
      begin
        FExport.AddBlankCell;  { DivTime}
        FExport.AddBlankCell;  { CI_Low}
        FExport.AddBlankCell;  { CI_High}
        FExport.AddBlankCell;  { CI_Low_Raw}
        FExport.AddBlankCell;  {CI_High_Raw}
        FExport.AddBlankCell;  { MinTime}
        FExport.AddBlankCell;  { MaxTime}
      end
      else
      begin
        FExport.AddP(cpu.DivTime(i), NUM_DECIMALS);
        FExport.AddP(cpu.MinDivTime(i), NUM_DECIMALS);
        FExport.AddP(cpu.MaxDivTime(i), NUM_DECIMALS);
        if FMinTimes[i] > FP_CUTOFF then
          FExport.AddP(cpu.MinDivTimeRaw(i), NUM_DECIMALS)
        else
          FExport.AddBlankCell;
        if FMaxTimes[i] > FP_CUTOFF then
          FExport.AddP(cpu.MaxDivTimeRaw(i), NUM_DECIMALS)
        else
          FExport.AddBlankCell;

        if FMinTimes[i] > FP_CUTOFF then
          FExport.AddP(FMinTimes[i], NUM_DECIMALS)
        else
          FExport.AddBlankCell;
        if FMaxTimes[i] > FP_CUTOFF then
          FExport.AddP(FMaxTimes[i], NUM_DECIMALS)
        else
          FExport.AddBlankCell;
      end;
    end
    else if FReltimeExportType = retSampleTimes then
    begin
      if FNodes[i].flag then
      begin
        FExport.AddBlankCell;  { DivTime}
        FExport.AddBlankCell;  { Elapsed (day)}
        FExport.AddBlankCell;  { CI_Low}
        FExport.AddBlankCell;  { CI_High}
        FExport.AddBlankCell;  { SampleTime}
      end
      else
      begin
        FExport.Add(cpu.RtdtFloatTimeToDateString(cpu.DivTime(i)));
        FExport.Add(cpu.ElapsedDaysForRtdtTime(i));
        FExport.Add(cpu.RtdtFloatTimeToDateString(cpu.MinDivTime(i)));
        FExport.Add(cpu.RtdtFloatTimeToDateString(cpu.MaxDivTime(i)));
        if FNodes[i].Time > FP_CUTOFF then
          FExport.Add(cpu.RtdtFloatTimeToDateString(FNodes[i].Time))
        else
          FExport.AddBlankCell;
      end;
    end
    else if FReltimeExportType = retStrictClock then
    begin
      if FNodes[i].flag then
      begin
        FExport.AddBlankCell;  { DivTime}
        FExport.AddBlankCell;  { CI_Low}
        FExport.AddBlankCell;  { CI_High}
      end
      else
      begin
        FExport.AddP(cpu.DivTime(i), NUM_DECIMALS);
        FExport.AddP(cpu.MinDivTime(i), NUM_DECIMALS);
        FExport.AddP(cpu.MaxDivTime(i), NUM_DECIMALS);
      end;
    end
    else if FReltimeExportType = retReltimes then
    begin
      if FNodes[i].flag then
      begin
        FExport.AddBlankCell;
        FExport.AddBlankCell;
      end
      else
      begin
        FExport.AddP(cpu.MinDivTime(i), NUM_DECIMALS);
        FExport.AddP(cpu.MaxDivTime(i), NUM_DECIMALS);
      end;
    end
    else
      raise Exception.Create('invalid Reltime export type');
    if not FIsBlensOnly then
      FExport.Add(Trim(Format(PercentString, [FNodes[i].DataCoverage * 100])));
    FExport.WriteLine;
  end;
end;

function TReltimeExportBuilder.GenerateExport: TExcelWrite;
begin
  Result := nil;
  if not FInitialized then
    raise Exception.Create('TReltimeExport.Intialize must be called before generating a spreadsheet');
  if Assigned(FExport) then
    FreeAndNil(FExport);
  FExport := TExcelWrite.Create(nil, 'Timetree');
  FExport.IsXLS := True;
  AddHeader;
  AddLeafNodes;
  AddInternalNodes;
  if Trim(FCaption.Text) <> EmptyStr then
    FExport.AddCaptionAsWorksheet(FCaption);
  FExport.AutoSizeColumns;
  Result := FExport;
  FExport := nil;
end;

function TReltimeExportBuilder.GenerateExport(n: TRelTimeTreeNodeArray; isBlensOnly: Boolean; MinTimes: array of Extended; MaxTimes: array of Extended): TExcelWrite;
var
  i: Integer;
begin
  Result := nil;
  FReltimeExportType := retMinMaxTimes;
  FNodes := n;
  SetLength(FMinTimes, Length(minTimes));
  if Length(minTimes) > 0 then
    for i := 0 to Length(minTimes) - 1 do
      FMinTimes[i] := minTimes[i];
  SetLength(FMaxTimes, Length(maxTimes));
  if Length(maxTimes) > 0 then
    for i := 0 to Length(maxTimes) - 1 do
      FMaxTimes[i] := maxTimes[i];
  FIsBLensOnly := isBlensOnly;
  Result := GenerateExport;
end;

function TReltimeExportBuilder.GenerateExport(n: TRelTimeTreeNodeArray; isBlensOnly: Boolean; sampleTimes: array of Extended): TExcelWrite;
var
  i: Integer;
begin
  Result := nil;
  FReltimeExportType := retSampleTimes;
  FNodes := n;
  SetLength(FSampleTimes, Length(sampleTimes));
  if Length(sampleTimes) > 0 then
    for i := 0 to Length(sampleTimes) - 1 do
      FSampleTimes[i] := sampleTimes[i];
  FIsBLensOnly := isBlensOnly;
  Result := GenerateExport;
end;

function TReltimeExportBuilder.GenerateExport(n: TRelTimeTreeNodeArray; isBlensOnly: Boolean): TExcelWrite;
begin
  Result := nil;
  FReltimeExportType := retReltimes;
  FNodes := n;
  FIsBLensOnly := isBlensOnly;
  Result := GenerateExport;
end;

function TReltimeExportBuilder.GetInternalNodeLabel(TreeDataIndex: Integer; n: TRelTimeTreeNode): String;
begin
  if (FLabels <> nil) and (TreeDataIndex < FLabels.Count) and (FLabels.Strings[TreeDataIndex] <> EmptyStr) then
  begin
    if n.IsOutgroupMember or n.flag then
      Result := 'outgroup-' + FLabels[TreeDataIndex]
    else
      Result := FLabels[TreeDataIndex]
  end
  else
  begin
    if n.IsOutgroupMember or n.flag then
      Result := 'outgroup'
    else
      Result := '-';
  end;
end;

function TReltimeExportBuilder.NumColumns: Integer;
begin
  Result := 0;
  case FReltimeExportType of
    retMinMaxTimes: Result := 15;
    retReltimes: Result := 10;
    retSampleTimes: Result := 12;
    retStrictClock: Result := 11;
  end;
  if IsDeveloper then
    Result := Result + 2;
  if not FIsBLensOnly then
    Result := Result + 1;
end;

constructor TReltimeExportBuilder.Create;
begin
  FInitialized := False;
  FNames := TStringList.Create;
  FLabels := TStringList.Create;
  FCaption := TStringList.Create;
  SetLength(FMinTimes, 0);
  SetLength(FMaxTimes, 0);
  SetLength(FSampleTimes, 0);
  FExport := TExcelWrite.Create(nil, 'Timetree');
end;

destructor TReltimeExportBuilder.Destroy;
begin
  if Assigned(FNames) then
    FNames.Free;
  if Assigned(FLabels) then
    FLabels.Free;
  if Assigned(FCaption) then
    FCaption.Free;
  if Assigned(FExport) then
    FExport.Free;
  SetLength(FMinTimes, 0);
  SetLength(FMaxTimes, 0);
  SetLength(FSampleTimes, 0);
  inherited Destroy;
end;

procedure TReltimeExportBuilder.Initialize(computer: TObject; n: TReltimeTreeNodeArray; names: TStringList; labels: TStringList; isBlensOnly: Boolean; MinTimes: array of Extended; MaxTimes: array of Extended);
var
  i: Integer;
begin
  FReltimeExportType := retMinMaxTimes;
  FComputer := computer;
  FNodes := n;
  SetNames(names);
  SetLabels(labels);
  SetLength(FMinTimes, Length(minTimes));
  if Length(minTimes) > 0 then
    for i := 0 to Length(minTimes) - 1 do
      FMinTimes[i] := minTimes[i];
  SetLength(FMaxTimes, Length(maxTimes));
  if Length(maxTimes) > 0 then
    for i := 0 to Length(maxTimes) - 1 do
      FMaxTimes[i] := maxTimes[i];
  FIsBLensOnly := isBlensOnly;
  FInitialized := True;
end;

procedure TReltimeExportBuilder.Initialize(computer: TObject; n: TReltimeTreeNodeArray; names: TStringList; labels: TStringList; isBlensOnly: Boolean; sampleTimes: array of Extended);
var
  i: Integer;
begin
  FReltimeExportType := retSampleTimes;
  FComputer := computer;
  FNodes := n;
  SetNames(names);
  SetLabels(labels);
  SetLength(FSampleTimes, Length(sampleTimes));
  if Length(sampleTimes) > 0 then
    for i := 0 to Length(sampleTimes) - 1 do
      FSampleTimes[i] := sampleTimes[i];
  FIsBLensOnly := isBlensOnly;
  FInitialized := True;
end;

procedure TReltimeExportBuilder.Initialize(computer: TObject; n: TReltimeTreeNodeArray; names: TStringList; labels: TStringList; isBlensOnly: Boolean);
begin
  FReltimeExportType := retReltimes;
  FComputer := computer;
  FNodes := n;
  SetNames(names);
  SetLabels(Labels);
  FIsBLensOnly := isBlensOnly;
  FInitialized := True;
end;

procedure TReltimeExportBuilder.AddCaptionAsWorksheet(aCaption: TStringList);
begin
  FCaption.Assign(aCaption);
end;

end.

