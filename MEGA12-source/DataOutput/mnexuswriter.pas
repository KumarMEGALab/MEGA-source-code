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

unit mnexuswriter;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, MLTree, mreltimetreenode, MLongintList;

type
    TDivTimeFunc = function(Index: Integer): Extended of object;

  { TNexusWriter }

  TNexusWriter = class(TObject)
    private
      FConfidenceIntervalPrefix: String;
      FHasDataCoverage: Boolean;
      FHasRates: Boolean;
      FHasReltimeStdErr: Boolean;
      FIsSamplingTime: Boolean;
      FTreeIsCalibrated: Boolean;
      FDivTimeFunc: TDivTimeFunc;
      FMaxDivTimeFunc: TDivTimeFunc;
      FMinDivTimeFunc: TDivTimeFunc;
      FRoot: TRelTimeTreeNode;
      FNodes: TRelTimeTreeNodeArray;
      FNexusStrings: TStringList;
      FOtuNames: TStringList;
      FIngroupOtuNames: TStringList;
      FIngroupIds: TLongintList;
      FNumTaxa: Integer;
      FTimeFactor: Extended;
      FIgnoreOutgroup: Boolean;
      procedure GetTaxaBlock(var aList: TStringList);
      procedure GetTreesBlock(var aList: TStringList);
      procedure GetTranslationBlock(var aList: TStringList);
      function GetNewickString: String;
      function BuildNewickRecursive(aNode: TRelTimeTreeNode): String;
      function GetNewickCommentForNode(aNode: TRelTimeTreeNode): String;
      function GetNewickCommentForNodeWithCalibrations(aNode: TRelTimeTreeNode): String;
      function GetNewickCommentForNodeNoCalibrations(aNode: TRelTimeTreeNode): String;
      procedure SetConfidenceIntervalPrefix(AValue: String);
      procedure SetHasDataCoverage(AValue: Boolean);
      procedure SetHasRates(AValue: Boolean);
      procedure SetHasReltimeStdErr(AValue: Boolean);
      procedure UpdateOtuNames;
    public
      constructor Create;
      destructor Destroy; override;

      procedure SetData(Root: TRelTimeTreeNode; NodesArray: TReltimeTreeNodeArray; TimeFactor: Extended; OtuNames: TStringList; NumTaxa: Integer); overload;
      procedure SetData(Root: TRelTimeTreeNode; NodesArray: TReltimeTreeNodeArray; OtuNames: TStringList; NumTaxa: Integer); overload;
      function BuildNexusStrings: TStringList;
      property MinDivTimeFunc: TDivTimeFunc read FMinDivTimeFunc write FMinDivTimeFunc;
      property MaxDivTimeFunc: TDivTimeFunc read FMaxDivTimeFunc write FMaxDivTimeFunc;
      property DivTimeFunc: TDivTimeFunc read FDivTimeFunc write FDivTimeFunc;
      property HasReltimeStdErr: Boolean read FHasReltimeStdErr write SetHasReltimeStdErr;
      property HasDataCoverage: Boolean read FHasDataCoverage write SetHasDataCoverage;
      property HasRates: Boolean read FHasRates write SetHasRates;
      property IsSamplingTime: Boolean read FIsSamplingTime write FIsSamplingTime;
      property ConfidenceIntervalPrefix: String read FConfidenceIntervalPrefix write SetConfidenceIntervalPrefix;
  end;



implementation

uses
  math;

{ TNexusWriter }

procedure TNexusWriter.GetTaxaBlock(var aList: TStringList);
var
  i: Integer;
  tempNames: TStringList = nil;
begin
  if FIgnoreOutgroup then
    tempNames := FIngroupOtuNames
  else
    tempNames := FOtuNames;
  aList.Clear;
  aList.Add('Begin Taxa;');
  aList.Add(#9 + 'Dimensions ntax=' + IntToStr(tempNames.Count) + ';');
  aList.Add(#9 + 'TaxLabels');
  if tempNames.Count > 0 then
    for i := 0 to tempNames.Count - 1 do
      aList.Add(#9 + #9 + tempNames[i]);
  aList.Add(#9 + #9 + ';');
  aList.Add('End;');
end;

procedure TNexusWriter.GetTreesBlock(var aList: TStringList);
var
  Newick: String;
  TranslateBlock: TStringList;
begin
  TranslateBlock := nil;
  aList.Clear;
  try
    aList.Add('Begin Trees;');
    TranslateBlock := TStringList.Create;
    GetTranslationBlock(TranslateBlock);
    aList.AddStrings(TranslateBlock);
    Newick := GetNewickString;
    Newick := 'tree TREE1 = [&R] ' + Newick;
    aList.Add(Newick);
    aList.Add('End;');
  finally
    if Assigned(TranslateBlock) then
      TranslateBlock.Free;
  end;
end;

procedure TNexusWriter.GetTranslationBlock(var aList: TStringList);
var
  i: Integer;
  aStr: String;
  tempNames: TStringList = nil;
begin
  if FIgnoreOutgroup then
    tempNames := FIngroupOtuNames
  else
    tempNames := FOtuNames;
  aList.Clear;
  aList.Add(#9 + 'Translate');
  if tempNames.Count > 0 then
  begin
    for i := 0 to tempNames.Count - 1 do
    begin
      if FIgnoreOutgroup then
        aStr := (#9 + #9 + IntToStr(FIngroupIds[i]) + ' ' + tempNames[i])
      else
        aStr := (#9 + #9 + IntToStr(i + 1) + ' ' + tempNames[i]);
      if i < (tempNames.Count - 1) then
        aStr := aStr + ',';
      aList.Add(aStr);
    end;
  end;
  aList.Add(#9 + #9 + ';');
end;

function TNexusWriter.GetNewickString: String;
begin
  Assert(Assigned(FRoot) and Assigned(FRoot.des1));
  if FIgnoreOutgroup then
    Result := BuildNewickRecursive(FRoot.des1) + GetNewickCommentForNode(FRoot.Des1) + ';'
  else
    Result := BuildNewickRecursive(FRoot) + GetNewickCommentForNode(FRoot) + ';';
end;

function TNexusWriter.BuildNewickRecursive(aNode: TRelTimeTreeNode): String;
var
  TF: Extended;
  blen, blen2: Extended;

  function BranchLengthForNode(n: TRelTimeTreeNode): Double;
  begin
    Result := 0;
    if FIsSamplingTime then
    begin
      if Assigned(n.anc) then
        Result := DivTimeFunc(n.index) - DivTimeFunc(n.anc.index);
    end
    else
    begin
      Result := (n.anc.height - n.height)*TF;
    end;
  end;

begin
  TF := abs(FTimeFactor);
  if aNode.OTU then
  begin
    Result := IntToStr(aNode.index + 1);
    exit;
  end;

  if ANode.des1.OTU and ANode.des2.Otu then
  begin
    Result := '(' + BuildNewickRecursive(ANode.des1) + GetNewickCommentForNode(aNode.des1) + ':' + Format('%e', [BranchLengthForNode(ANode.des1)]) + ',' + BuildNewickRecursive(ANode.des2) + GetNewickCommentForNode(aNode.des2) + ':' + Format('%e', [BranchLengthForNode(ANode.des2)]) + ')'
  end
  else if ANode.des1.OTU then
  begin
    if IsZero(ANode.height - ANode.des2.height, FP_CUTOFF) then
      blen := 0
    else
      blen := ANode.height - ANode.des2.height;
    Result := '(' + BuildNewickRecursive(ANode.des1) + GetNewickCommentForNode(aNode.des1) + ':' + Format('%e', [BranchLengthForNode(ANode.des1)]) + ',' + BuildNewickRecursive(ANode.des2) + GetNewickCommentForNode(aNode.des2) + ':' + Format('%e', [blen*TF]) + ')'
  end
  else if ANode.des2.OTU then
  begin
    if IsZero(ANode.height - ANode.des1.height, FP_CUTOFF) then
      blen := 0
    else
      blen := ANode.height - ANode.des1.height;
    Result := '(' + BuildNewickRecursive(ANode.des1) + GetNewickCommentForNode(aNode.des1) + ':' + Format('%e', [blen*TF]) + ',' + BuildNewickRecursive(ANode.des2) + GetNewickCommentForNode(aNode.des2) + ':' + Format('%e', [BranchLengthForNode(ANode.des2)]) + ')'
  end
  else
  begin
    if IsZero(ANode.height - ANode.des1.height, FP_CUTOFF) then
      blen := 0
    else
      blen := ANode.height - ANode.des1.height;
    if IsZero(ANode.height - ANode.des2.height, FP_CUTOFF) then
      blen2 := 0
    else
      blen2 := ANode.height - ANode.des2.height;
    Result := '(' + BuildNewickRecursive(ANode.des1) + GetNewickCommentForNode(aNode.des1) + ':' + Format('%e', [blen*TF]) + ',' + BuildNewickRecursive(ANode.des2) + GetNewickCommentForNode(aNode.des2) + ':' + Format('%e', [blen2*TF]) + ')';
  end;
end;

function TNexusWriter.GetNewickCommentForNode(aNode: TRelTimeTreeNode): String;
begin
  if FTreeIsCalibrated then
    Result := GetNewickCommentForNodeWithCalibrations(aNode)
  else
    Result := GetNewickCommentForNodeNoCalibrations(aNode);
end;

function TNexusWriter.GetNewickCommentForNodeWithCalibrations(aNode: TRelTimeTreeNode): String;
var
  MinTime, MaxTime, DivTime: Extended;
begin
  DivTime := FDivTimeFunc(aNode.Index);
  if FHasRates then
    Result := Format('[&rate=%e', [aNode.rate])
  else
    Result := Format('[&rate=%s', ['NA']);
  Result := Result + Format(',branch_length=%e', [aNode.blen]);
  if not aNode.OTU then
  begin
    Result := Result + Format(',reltime=%e', [aNode.height]);
    if FHasReltimeStdErr then
      Result := Result + Format(',reltime_stderr=%e', [sqrt(aNode.vh)]);

    Result := Result + Format(',divtime=%e', [DivTime]);
    if Assigned(FMinDivTimeFunc) and Assigned(FMaxDivTimeFunc) then
    begin
      MinTime := FMinDivTimeFunc(aNode.index);
      MaxTime := FMaxDivTimeFunc(aNode.index);
      Result := Result + Format(',divtime_95%%_CI={%e,%e}', [MinTime, MaxTime]);
    end;
    if FHasDataCoverage then
      Result := Result + Format(',data_coverage=%d%%', [Round(aNode.DataCoverage * 100)]);
  end;
  Result := Result + ']';
end;

function TNexusWriter.GetNewickCommentForNodeNoCalibrations(aNode: TRelTimeTreeNode): String;
var
  MinTime, MaxTime: Extended;
begin
  if FHasRates then
    Result := Format('[&rate=%e', [aNode.rate])
  else
    Result := Format('[&rate=%s', ['NA']);
  Result := Result + Format(',branch_length=%e', [aNode.blen]);
  if not aNode.OTU then
  begin
    Result := Result + Format(',reltime=%e', [aNode.height]);
    if FHasReltimeStdErr then
      Result := Result + Format(',reltime_stderr=%e', [sqrt(aNode.vh)]);
    if Assigned(FMinDivTimeFunc) and Assigned(FMaxDivTimeFunc) then
    begin
      MinTime := FMinDivTimeFunc(aNode.index);
      MaxTime := FMaxDivTimeFunc(aNode.index);
      Result := Result + Format(',%s_95%%_CI={%e,%e}', [FConfidenceIntervalPrefix, MinTime, MaxTime]);
    end;
    if FHasDataCoverage then
      Result := Result + Format(',data_coverage=%d%%', [Round(aNode.DataCoverage * 100)]);
  end;
  Result := Result + ']';
end;

procedure TNexusWriter.SetConfidenceIntervalPrefix(AValue: String);
begin
  if FConfidenceIntervalPrefix = AValue then Exit;
  FConfidenceIntervalPrefix := AValue;
end;

procedure TNexusWriter.SetHasDataCoverage(AValue: Boolean);
begin
  if FHasDataCoverage=AValue then Exit;
  FHasDataCoverage:=AValue;
end;

procedure TNexusWriter.SetHasRates(AValue: Boolean);
begin
  if FHasRates = AValue then Exit;
  FHasRates := AValue;
end;

procedure TNexusWriter.SetHasReltimeStdErr(AValue: Boolean);
begin
  if FHasReltimeStdErr=AValue then Exit;
  FHasReltimeStdErr:=AValue;
end;

procedure TNexusWriter.UpdateOtuNames;
var
  i: Integer;
  aName: String;
begin
  FIngroupOtuNames := TStringList.Create;
  FIngroupIds := TLongIntList.Create;
  if FOtuNames.Count > 0 then
    for i := 0 to FOtuNames.Count - 1 do
    begin
      aName := FOtuNames[i];
      aName := StringReplace(aName, ' ', '_', [rfReplaceAll]);
      FOtuNames[i] := aName;
      if not FNodes[i].IsOutgroupMember then
      begin
        FIngroupOtuNames.Add(FOtuNames[i]);
        FIngroupIds.Add(i + 1);
      end;
    end;

end;

constructor TNexusWriter.Create;
begin
  FTreeIsCalibrated := False;
  FHasDataCoverage := False;
  FHasReltimeStdErr := False;
  FNodes := nil;
  FRoot := nil;
  FNexusStrings := TStringList.Create;
  FOtuNames := TStringList.Create;
  FTimeFactor := 1;
  FConfidenceIntervalPrefix := 'reltime';
  FIgnoreOutgroup := False;
  FIngroupOtuNames := nil;
  FIngroupIds := nil;
end;

destructor TNexusWriter.Destroy;
begin
  if Assigned(FNexusStrings) then
    FNexusStrings.Free;
  if Assigned(FOtuNames) then
    FOtuNames.Free;
  if Assigned(FIngroupOtuNames) then
    FIngroupOtuNames.Free;
  if Assigned(FIngroupIds) then
    FIngroupIds.Free;
  inherited Destroy;
end;

procedure TNexusWriter.SetData(Root: TRelTimeTreeNode;
  NodesArray: TReltimeTreeNodeArray; TimeFactor: Extended;
  OtuNames: TStringList; NumTaxa: Integer);
begin
  FNumTaxa := NumTaxa;
  FTimeFactor := TimeFactor;
  FTreeIsCalibrated := True;
  FOtuNames.Assign(OtuNames);
  FNodes := NodesArray;
  FRoot := Root;
  FIgnoreOutgroup := True;
end;

procedure TNexusWriter.SetData(Root: TRelTimeTreeNode;
  NodesArray: TReltimeTreeNodeArray; OtuNames: TStringList; NumTaxa: Integer);
begin
  FNumTaxa := NumTaxa;
  FTimeFactor := 1.0;
  FTreeIsCalibrated := False;
  FOtuNames.Assign(OtuNames);
  FNodes := NodesArray;
  FRoot := Root;
  FIgnoreOutgroup := True;
end;

function TNexusWriter.BuildNexusStrings: TStringList;
var
  aList: TStringList;
begin
  Assert(Assigned(FDivTimeFunc));

  aList := nil;
  try
    aList := TStringList.Create;
    FNexusStrings.Clear;
    UpdateOtuNames;
    FNexusStrings.Add('#NEXUS');
    FNexusStrings.Add(EmptyStr);
    GetTaxaBlock(aList);
    FNexusStrings.AddStrings(aList);
    FNexusStrings.Add(EmptyStr);
    GetTreesBlock(aList);
    FNexusStrings.AddStrings(aList);
    Result := TStringList.Create;
    Result.Assign(FNexusStrings);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

end.

