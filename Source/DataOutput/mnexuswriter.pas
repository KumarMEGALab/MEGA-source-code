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

unit mnexuswriter;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, MLTree, mreltimetreenode;

type
    TDivTimeFunc = function(Index: Integer): Extended of object;

  { TNexusWriter }

  TNexusWriter = class(TObject)
    private
      FHasDataCoverage: Boolean;
      FHasReltimeStdErr: Boolean;
      FTreeIsCalibrated: Boolean;
      FDivTimeFunc: TDivTimeFunc;
      FMaxDivTimeFunc: TDivTimeFunc;
      FMinDivTimeFunc: TDivTimeFunc;
      FRoot: TRelTimeTreeNode;
      FNodes: TRelTimeTreeNodeArray;
      FNexusStrings: TStringList;
      FOtuNames: TStringList;
      FNumTaxa: Integer;
      FTimeFactor: Extended;

      procedure GetTaxaBlock(var aList: TStringList);
      procedure GetTreesBlock(var aList: TStringList);
      procedure GetTranslationBlock(var aList: TStringList);
      function GetNewickString: String;
      function BuildNewickRecursive(aNode: TRelTimeTreeNode): String;
      function GetNewickCommentForNode(aNode: TRelTimeTreeNode): String;
      function GetNewickCommentForNodeWithCalibrations(aNode: TRelTimeTreeNode): String;
      function GetNewickCommentForNodeNoCalibrations(aNode: TRelTimeTreeNode): String;
      procedure SetHasDataCoverage(AValue: Boolean);
      procedure SetHasReltimeStdErr(AValue: Boolean);
      procedure UpdateOtuNames;
    public
      constructor Create;
      destructor Destroy; override;

      procedure SetData(Root: TRelTimeTreeNode; NodesArray: TReltimeTreeNodeArray; TimeFactor: Extended; OtuNames: TStringList; NumTaxa: Integer); overload;
      procedure SetData(Root: TRelTimeTreeNode; NodesArray: TReltimeTreeNodeArray; OtuNames: TStringList; NumTaxa: Integer); overload;
//      procedure SetData(Root: TMLTreeNode; NodesArray: TMLTreeNodeArray; TimeFactor: Extended; OtuNames: TStringList; NumTaxa: Integer); overload;
//      procedure SetData(Root: TMLTreeNode; NodesArray: TMLTreeNodeArray; OtuNames: TStringList; NumTaxa: Integer); overload;

      function BuildNexusStrings: TStringList;
      property MinDivTimeFunc: TDivTimeFunc read FMinDivTimeFunc write FMinDivTimeFunc;
      property MaxDivTimeFunc: TDivTimeFunc read FMaxDivTimeFunc write FMaxDivTimeFunc;
      property DivTimeFunc: TDivTimeFunc read FDivTimeFunc write FDivTimeFunc;
      property HasReltimeStdErr: Boolean read FHasReltimeStdErr write SetHasReltimeStdErr;
      property HasDataCoverage: Boolean read FHasDataCoverage write SetHasDataCoverage;
  end;



implementation

uses
  math;

{ TNexusWriter }

procedure TNexusWriter.GetTaxaBlock(var aList: TStringList);
var
  i: Integer;
begin
  aList.Clear;
  aList.Add('Begin Taxa;');
  aList.Add(#9 + 'Dimensions ntax=' + IntToStr(FNumTaxa) + ';');
  aList.Add(#9 + 'TaxLabels');
  if FOtuNames.Count > 0 then
    for i := 0 to FOtuNames.Count - 1 do
      aList.Add(#9 + #9 + FOtuNames[i]);
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
begin
  aList.Clear;
  aList.Add(#9 + 'Translate');
  if FOtuNames.Count > 0 then
  begin
    for i := 0 to FOtuNames.Count - 1 do
    begin
      aStr := (#9 + #9 + IntToStr(i + 1) + ' ' + FOtuNames[i]);
      if i < (FOtuNames.Count - 1) then
        aStr := aStr + ',';
      aList.Add(aStr);
    end;
  end;
  aList.Add(#9 + #9 + ';');
end;

function TNexusWriter.GetNewickString: String;
begin
  Assert(Assigned(FRoot));
  Result := BuildNewickRecursive(FRoot) + ';';
end;

function TNexusWriter.BuildNewickRecursive(aNode: TRelTimeTreeNode): String;
var
  TF: Extended;
  blen, blen2: Extended;
begin
  TF := abs(FTimeFactor);

  if aNode.OTU then
  begin
    Result := IntToStr(aNode.index + 1);
    exit;
  end;

  if ANode.des1.OTU and ANode.des2.Otu then
  begin
    Result := '(' + BuildNewickRecursive(ANode.des1) + GetNewickCommentForNode(aNode.des1) + ':' + Format('%e', [ANode.height*TF]) + ',' + BuildNewickRecursive(ANode.des2) + GetNewickCommentForNode(aNode.des2) + ':' + Format('%e', [ANode.height*TF]) + ')'
  end
  else if ANode.des1.OTU then
  begin
    if IsZero(ANode.height - ANode.des2.height, FP_CUTOFF) then
      blen := 0
    else
      blen := ANode.height - ANode.des2.height;
    Result := '(' + BuildNewickRecursive(ANode.des1) + GetNewickCommentForNode(aNode.des1) + ':' + Format('%e', [ANode.height*TF]) + ',' + BuildNewickRecursive(ANode.des2) + GetNewickCommentForNode(aNode.des2) + ':' + Format('%e', [blen*TF]) + ')'
  end
  else if ANode.des2.OTU then
  begin
    if IsZero(ANode.height - ANode.des1.height, FP_CUTOFF) then
      blen := 0
    else
      blen := ANode.height - ANode.des1.height;
    Result := '(' + BuildNewickRecursive(ANode.des1) + GetNewickCommentForNode(aNode.des1) + ':' + Format('%e', [blen*TF]) + ',' + BuildNewickRecursive(ANode.des2) + GetNewickCommentForNode(aNode.des2) + ':' + Format('%e', [ANode.height*TF]) + ')'
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
  Result := Format('[&rate=%e', [aNode.rate]);
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
  Result := Format('[&rate=%e', [aNode.rate]);
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
      Result := Result + Format(',reltime_95%%_CI={%e,%e}', [MinTime, MaxTime]);
    end;
    if FHasDataCoverage then
      Result := Result + Format(',data_coverage=%d%%', [Round(aNode.DataCoverage * 100)]);
  end;
  Result := Result + ']';
end;

procedure TNexusWriter.SetHasDataCoverage(AValue: Boolean);
begin
  if FHasDataCoverage=AValue then Exit;
  FHasDataCoverage:=AValue;
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
  if FOtuNames.Count > 0 then
    for i := 0 to FOtuNames.Count - 1 do
    begin
      aName := FOtuNames[i];
      aName := StringReplace(aName, ' ', '_', [rfReplaceAll]);
      FOtuNames[i] := aName;
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
end;

destructor TNexusWriter.Destroy;
begin
  if Assigned(FNexusStrings) then
    FNexusStrings.Free;
  if Assigned(FOtuNames) then
    FOtuNames.Free;
  inherited Destroy;
end;

procedure TNexusWriter.SetData(Root: TRelTimeTreeNode; NodesArray: TRelTimeTreeNodeArray; TimeFactor: Extended; OtuNames: TStringList; NumTaxa: Integer);
begin
  FNumTaxa := NumTaxa;
  FTimeFactor := TimeFactor;
  FTreeIsCalibrated := True;
  FOtuNames.Assign(OtuNames);
  FNodes := NodesArray;
  FRoot := Root;
end;

procedure TNexusWriter.SetData(Root: TRelTimeTreeNode; NodesArray: TRelTimeTreeNodeArray; OtuNames: TStringList; NumTaxa: Integer);
begin
  FNumTaxa := NumTaxa;
  FTimeFactor := 1.0;
  FTreeIsCalibrated := False;
  FOtuNames.Assign(OtuNames);
  FNodes := NodesArray;
  FRoot := Root;
end;
{
procedure TNexusWriter.SetData(Root: TMLTreeNode; NodesArray: TMLTreeNodeArray; TimeFactor: Extended; OtuNames: TStringList; NumTaxa: Integer);
begin
  SetData(Root, TRelTimeTreeNodeArray(NodesArray), TimeFactor, OtuNames, NumTaxa);
end;

procedure TNexusWriter.SetData(Root: TMLTreeNode; NodesArray: TMLTreeNodeArray; OtuNames: TStringList; NumTaxa: Integer);
begin
  SetData(Root, TRelTimeTreeNodeArray(NodesArray), OtuNames, NumTaxa);
end;
}
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

