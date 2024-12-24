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

unit mrobinson_foulds_metric;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MTreeData, MPartitionList;

type

  { TRobinsonFouldsMetric }

  TRobinsonFouldsMetric = class(TObject)
    private
      FTree1Partitions: TList;
      FTree2Partitions: TList;
      FDivideByTwo: Boolean;
      procedure Clear;
      function GetPartitionsForTree(aTree: TTreeData): TList;
      function CountPartitionsNotInListA(listA: TList; listB: TList): Integer;
      procedure WriteToStdOut(aMsg: String);
    public
      constructor Create(aDivByTwo: Boolean);
      destructor Destroy; override;

      function Compute_RF_distance(tree1: TTreeData; tree2: TTreeData): Double;
      property DivideByTwo: Boolean read FDivideByTwo;
  end;


implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MegaUtils_NV,
  {$ENDIF}
  MPartition, StrHashMap;

{ TRobinsonFouldsMetric }

procedure TRobinsonFouldsMetric.Clear;
var
  i: Integer = -1;
  p: TPartition = nil;
begin
  if Assigned(FTree1Partitions) then
    if FTree1Partitions.Count > 0 then
    begin
      for i := 0 to FTree1Partitions.Count - 1 do
      begin
        if Assigned(FTree1Partitions[i]) then
        begin
          p := TPartition(FTree1Partitions[i]);
          p.Free;
        end;
      end;
      FTree1Partitions.Clear;
    end;

  if Assigned(FTree2Partitions) then
    if FTree2Partitions.Count > 0 then
    begin
      for i := 0 to FTree2Partitions.Count - 1 do
      begin
        if Assigned(FTree2Partitions[i]) then
        begin
          p := TPartition(FTree2Partitions[i]);
          p.Free;
        end;
      end;
      FTree2Partitions.Clear;
    end;
end;

function TRobinsonFouldsMetric.GetPartitionsForTree(aTree: TTreeData): TList;
var
  plist: TPartitionList = nil;
begin
  try
    plist := TPartitionList.Create(aTree.NoOfOTUs, 0, False);
    plist.Add(aTree.NodeArray, 0);
    Result := plist.ClonePartitions;
    WriteToStdOut(Format('found %d partitions in tree', [Result.Count]));
  finally
    if Assigned(plist) then
      plist.Free;
  end;
end;

function TRobinsonFouldsMetric.CountPartitionsNotInListA(listA: TList; listB: TList): Integer;
var
  i: Integer = -1;
  a: TPartition = nil;
  b: TPartition = nil;
  hashLookup: TStringHashMap = nil;
begin
  Result := 0;
  if listA.Count = 0 then
    Exit(listB.Count);
  if listB.Count = 0 then
    Exit(0);

  try
    hashLookup := TStringHashMap.Create;
    for i := 0 to listA.Count - 1 do
    begin
      a := TPartition(listA[i]);
      hashLookup.Add(a.GetPartitionString, a);
    end;

    for i := 0 to listB.Count - 1 do
    begin
      b := TPartition(listB[i]);
      if not hashLookup.Contains(b.GetPartitionString) then
        inc(Result);
    end;
    WriteToStdOut(Format('Found %d unmatched partitions', [Result]));
  finally
    if Assigned(hashLookup) then
      hashLookup.Free;
  end;
end;

procedure TRobinsonFouldsMetric.WriteToStdOut(aMsg: String);
begin
  {$IFNDEF VISUAL_BUILD}
  //WriteLn(aMsg); {only used for dev/debugging}
  {$ENDIF}
end;

constructor TRobinsonFouldsMetric.Create(aDivByTwo: Boolean);
begin
  FDivideByTwo := aDivByTwo;
  FTree1Partitions := nil;
  FTree2Partitions := nil;
end;

destructor TRobinsonFouldsMetric.Destroy;
begin
  Clear;
  if Assigned(FTree1Partitions) then
    FTree1Partitions.Free;
  if Assigned(FTree2Partitions) then
    FTree2Partitions.Free;
  inherited Destroy;
end;

function TRobinsonFouldsMetric.Compute_RF_distance(tree1: TTreeData; tree2: TTreeData): Double;
var
  a: Integer = 0;
  b: Integer = 0;
begin
  try
    WriteToStdOut('Preparing to compute Robinson-Foulds distance');
    Clear;
    if Assigned(FTree1Partitions) then
      FreeAndNil(FTree1Partitions);
    if Assigned(FTree2Partitions) then
      FreeAndNil(FTree2Partitions);

    FTree1Partitions := GetPartitionsForTree(tree1);
    FTree2Partitions := GetPartitionsForTree(tree2);
    a := CountPartitionsNotInListA(FTree1Partitions, FTree2Partitions);
    b := CountPartitionsNotInListA(FTree2Partitions, FTree1Partitions);
    Result := a + b;
    if FDivideByTwo then
      Result := Result/2;
  except
    on E:Exception do
    begin
      WriteToStdOut('Error when computing the RF distance: ' + E.Message);
      {$IFNDEF VISUAL_BUILD}
      error_nv('Error when computing the RF distance', E);
      {$ELSE}
        raise E;
      {$ENDIF}
    end;
  end;
end;

end.

