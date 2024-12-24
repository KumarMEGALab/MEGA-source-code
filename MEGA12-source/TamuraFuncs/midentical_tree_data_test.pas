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

unit midentical_tree_data_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MPartitionList, MTreeData;

type

  { TIdenticalTreeDataTest }

  TIdenticalTreeDataTest = class(TObject)
    private
      FPartitionList: TPartitionList;
      FTree1: TTreeData;
      FTree2: TTreeData;
      FNumOtus: Integer;
      function TotalFrequency(aTree: TTreeData): Integer;
    public
      constructor Create(numOtus: Integer);
      destructor Destroy; override;
      function TreesAreIdentical(tree1: TTreeData; tree2: TTreeData): Boolean;
  end;

implementation

uses
  MegaConsts, math;

{ TIdenticalTreeDataTest }

function TIdenticalTreeDataTest.TotalFrequency(aTree: TTreeData): Integer;
var
  i: Integer;
  temp: Double = 0;
begin
  Result := 0;
  for i := FNumOtus to (2*FNumOtus - 3) do
  begin
    temp += aTree.Stats[i];
    Assert((CompareValue(aTree.Stats[i], 1, FP_CUTOFF) = 0) or (CompareValue(aTree.Stats[i], 0, FP_CUTOFF) = 0));
  end;
  Result := Round(temp);
end;

constructor TIdenticalTreeDataTest.Create(numOtus: Integer);
begin
  FNumOtus := numOtus;
  FPartitionList := TPartitionList.Create(numOtus, 0, False, False);
  FTree1 := TTreeData.Create(numOtus, True, True, True);
  FTree2 := TTreeData.Create(numOtus, True, True, True);
end;

destructor TIdenticalTreeDataTest.Destroy;
begin
  if Assigned(FPartitionList) then
  begin
    FPartitionList.ClearData;
    FPartitionList.Free;
  end;
  inherited Destroy;
end;

function TIdenticalTreeDataTest.TreesAreIdentical(tree1: TTreeData; tree2: TTreeData): Boolean;
begin
  FPartitionList.ClearData;
  FTree1.Assign(tree1);
  FTree1.isStats := True;
  FTree2.Assign(tree2);
  Tree2.isStats := True;
  FPartitionList.Add(FTree1.NodeArray, 0);
  FPartitionList.Compare(FTree2.NodeArray, PArrayOfDouble(@FTree2.StatsArray^[FTree1.NoOfOTUs]));
  Result := (TotalFrequency(FTree2) = (FNumOtus - 3));
end;

end.

