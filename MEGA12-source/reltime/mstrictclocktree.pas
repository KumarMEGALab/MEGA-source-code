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

unit mstrictclocktree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MTreeDataAdapter, MSimpleTreeNode, MTreeData, MegaConsts;

type

  { TStrictClockTree }

  TStrictClockTree = class(TSimpleTreeDataAdapter)
    private
      FCalibrateClockMode: TCalibrateClockMode;
      FNames: TStringList;
      FClockRate: Double;
      FIsRooted: Boolean;
      FDivTime: Extended;
      FNodeId: Integer;
      FUnitStr: String;
      procedure MakeUltrametric(aNode: TSimpleTreeNode);
      procedure SetCalibrateClockMode(AValue: TCalibrateClockMode);
      procedure UpdateBranchLengths(aNode: TSimpleTreeNode);
    protected

    public
      constructor Create(aData: TTreeData; aNamesList: TStringList; aClockRate: Double; isRooted: Boolean); deprecated; { strict clocks are now ony used for Reltime trees which are already ultra-metric so this class is no longer needed}
      destructor Destroy; override;
      procedure GetOtuNames(var aList: TStringList);
      procedure SetToUseDivTime(aTime: Extended; aNodeId: Integer; aUnitStr: String);
      property ClockRate: Double read FClockRate;
      property IsRooted: Boolean read FIsRooted;
      property NodeId: Integer read FNodeId;
      property DivTime: Extended read FDivTime;
      property TimeUnitStr: String read FUnitStr;
      property CalibrateClockMode: TCalibrateClockMode read FCalibrateClockMode write SetCalibrateClockMode;
  end;

  { TStrictClockTreeThread }

  TStrictClockTreeThread = class(TThread)
    private
      FTimeUnitStr: String;
      FIsSuccess: Boolean;
      FMsgLog: String;
      FClockTree: TStrictClockTree;
      FData: TTreeData;
      FClockRate: Extended;
      FNames: TStringList;
      FIsRooted: Boolean;
      FDivTime: Extended;
      FNodeId: Integer;
      FCalibrateClockMode: TCalibrateClockMode;
      procedure MakeUltrametricTree;
    protected
      procedure Execute; override;
    public
      constructor Create(aData: TTreeData; aNamesList: TStringList; aClockRate: Extended; IsRooted: Boolean); deprecated; { strict clocks are now only used for Reltime trees so they are already ultrametric}
      destructor Destroy; override;
      procedure SetDivergenceTime(aTime: Extended; aNode: Integer; aTimeUnitStr: String);
      function GetStrictClockTree: TStrictClockTree;
      property IsSuccess: Boolean read FIsSuccess;
      property MsgLog: String read FMsgLog;
      property CalibrateClockMode: TCalibrateClockMode read FCalibrateClockMode;
  end;

implementation

{ TStrictClockTreeThread }

procedure TStrictClockTreeThread.MakeUltrametricTree;
begin
  try
    FClockTree := TStrictClockTree.Create(FData, FNames, FClockRate, FIsRooted);
    if FCalibrateClockMode = ccmDivTime then
      FClockTree.SetToUseDivTime(FDivTime, FNodeId, FTimeUnitStr);
    FIsSuccess := True;
  except
    on E:Exception do
    begin
      FIsSuccess := False;
      FMsgLog := E.Message;
    end;
  end;
end;

procedure TStrictClockTreeThread.Execute;
begin
  MakeUltrametricTree;
end;

constructor TStrictClockTreeThread.Create(aData: TTreeData; aNamesList: TStringList; aClockRate: Extended; IsRooted: Boolean);
begin
  raise Exception.Create('TStrictClockTreeThread is deprecated');
  inherited Create(True);
  FCalibrateClockMode := ccmEvoRate; { by default}
  FDivTime := -1.0;
  FNodeId := -1;
  FIsSuccess := False;
  FClockTree := nil;
  FData := TTreeData.Create(aData.NoOfOTUs, aData.isBLen, aData.isSE, aData.isStats);
  FData.Assign(aData);
  FClockRate := aClockRate;
  FNames := TStringList.Create;
  FNames.AddStrings(aNamesList);
  FIsRooted := IsRooted;
  FreeOnTerminate := True;
end;

destructor TStrictClockTreeThread.Destroy;
begin
  if Assigned(FData) then
    FData.Free;
  if Assigned(FNames) then
    FNames.Free;
  if Assigned(FClockTree) then
    FClockTree.Free;
  inherited Destroy;
end;

procedure TStrictClockTreeThread.SetDivergenceTime(aTime: Extended;
  aNode: Integer; aTimeUnitStr: String);
begin
  FTimeUnitStr := aTimeUnitStr;
  FDivTime := aTime;
  FNodeId := aNode;
  FCalibrateClockMode := ccmDivTime;
end;

function TStrictClockTreeThread.GetStrictClockTree: TStrictClockTree;
begin
  Result := FClockTree;
  FClockTree := nil;
end;

{ TStrictClockTree }

procedure TStrictClockTree.MakeUltrametric(aNode: TSimpleTreeNode);
begin
  if aNode.IsOTU then
  begin
    aNode.Value := 0.0;
    aNode.Value2 := 0;
  end
  else
  begin
    MakeUltrametric(aNode.Des1);
    MakeUltrametric(aNode.Des2);
    aNode.Value2 := ((aNode.Des1.Value2 + aNode.Des1.BLen)*aNode.Des1.Size + (aNode.Des2.Value2 + aNode.Des2.Blen)*aNode.Des2.Size)/aNode.Size;
    aNode.Value := (aNode.Des1.Value2 + aNode.Des1.BLen + aNode.Des2.Value2 + aNode.Des2.BLen)/2;
  end;
end;

procedure TStrictClockTree.SetCalibrateClockMode(AValue: TCalibrateClockMode);
begin
  if FCalibrateClockMode=AValue then Exit;
  FCalibrateClockMode:=AValue;
end;

procedure TStrictClockTree.UpdateBranchLengths(aNode: TSimpleTreeNode);
begin
  if not aNode.IsOtu then
  begin
    UpdateBranchLengths(aNode.Des1);
    UpdateBranchLengths(aNode.Des2);
    aNode.Des1.BLen := aNode.Value - aNode.Des1.Value;
    aNode.Des2.BLen := aNode.Value - aNode.Des2.Value;
  end;
end;

constructor TStrictClockTree.Create(aData: TTreeData; aNamesList: TStringList; aClockRate: Double; isRooted: Boolean);
begin
  raise Exception.Create('TStrictClockTree is deprecated');
  FCalibrateClockMode := ccmEvoRate;
  FIsRooted := isRooted;
  FNames := TStringList.Create;
  FNames.AddStrings(aNamesList);
  FClockRate := aClockRate;
  SetTreeData(aData, isRooted, aNamesList);
  MakeUltrametric(FRoot);
  UpdateBranchLengths(FRoot);
end;

destructor TStrictClockTree.Destroy;
begin
  if Assigned(FNames) then
    FNames.Free;
  inherited Destroy;
end;

procedure TStrictClockTree.GetOtuNames(var aList: TStringList);
begin
  aList.Clear;
  aList.AddStrings(FNames);
end;

procedure TStrictClockTree.SetToUseDivTime(aTime: Extended; aNodeId: Integer; aUnitStr: String);
begin
  FDivTime := aTime;
  FNodeId := aNodeId;
  FUnitStr := aUnitStr;
  FCalibrateClockMode := ccmDivTime;
end;

end.

