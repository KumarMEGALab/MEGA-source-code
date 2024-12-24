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

unit DistanceUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Forms, SysUtils, MegaConsts, Classes, StdCtrls, controls,
  MAnalysisInfo, MD_Sequences;



function ComputePairwiseDistances(AlignedPairs: TSequenceList; MAI: TAnalysisInfo):TPairwiseDistances;
function AugmentDistanceMatrix(InitialMatrix: PDistanceMatrix; PairwiseDistances: TPairwiseDistances): PDistanceMatrix;
function DistanceMatrixToStringList(Matrix: PDistanceMatrix; NumTaxa: Integer): TStringList;
function PrepareRNADataForDistAnalysis(Sequence_1, Sequence_2: TSequence; AOptions: TDataSubsetOptions; var NumSites: Integer): TList;

implementation

uses
  MegaUtils, MAminoDist,
  MNucDist, MSeqDistBase, MDistPack;

/// <summary>Adds a new column to an existing distance matrix. A square matrix will be returned
/// where the first new pairwise distances are added as the first row and first column</summary>
function AugmentDistanceMatrix(InitialMatrix: PDistanceMatrix; PairwiseDistances: TPairwiseDistances): PDistanceMatrix;
var
  NumTaxa: Integer;
  MyNewDistMatrix: PDistanceMatrix;
  i, j: Integer;
begin

  NumTaxa := Length(PairWiseDistances) + 1;
  MyNewDistMatrix := NewDistMatrix(NumTaxa, true);

  MyNewDistMatrix[0][0] := 0.0;

  for i := 0 to NumTaxa - 2 do
  begin
    MyNewDistMatrix[i + 1][0] := PairwiseDistances[i];
    MyNewDistMatrix[0][i + 1] := PairwiseDistances[i];
  end;

  for i := 0 to NumTaxa - 2 do
    for j := 0 to NumTaxa - 2 do
      MyNewDistMatrix[i + 1][j + 1] := InitialMatrix[i][j];
  Result := MyNewDistMatrix;
end;

function PrepareRNADataForDistAnalysis(Sequence_1, Sequence_2: TSequence; AOptions: TDataSubsetOptions; var NumSites: Integer): TList;
var
  i: Integer;
  TempSeq1: AnsiString;
  TempSeq2: AnsiString;
  ASeq1: PAnsiChar;
  ASeq2: PAnsiChar;
  MyNumSites: LongInt;
  MappedSequences: TList;

  function CharIsValidBase(MyChar: AnsiChar): Boolean;
  begin
    Result := False;
    if (MyChar = 'A') or
       (MyChar = 'G') or
       (MyChar = 'C') or
       (MyChar = 'T') or
       (MyChar = 'U') or
       (MyChar = 'a') or
       (MyChar = 'g') or
       (MyChar = 'c') or
       (MyChar = 't') or
       (MyChar = 'u')then Result := True;
  end;

  function SiteHasMissingData(SiteNum: Longint): Boolean;
  begin
    Result := True;
    if (CharIsValidBase(Sequence_1.SeqData[SiteNum])) and (CharIsValidBase(Sequence_2.SeqData[SiteNum])) then
      Result := False;
  end;

begin
  NumSites := 0;
  MyNumSites := 0;
  MappedSequences := TList.Create;
  TempSeq1 := '';
  TempSeq2 := '';
  if (dsoCompleteDeletion in AOptions) or (dsoPairwiseDeletion in AOptions) then
  begin
    for i := 1 to Length(Sequence_1.SeqData) do
      if not SiteHasMissingData(i) then
      begin
        Inc(MyNumSites);
        TempSeq1 := TempSeq1 + Sequence_1.SeqData[i];
        TempSeq2 := TempSeq2 + Sequence_2.SeqData[i];
      end;
  end
  else
  begin
    MyNumSites := Length(Sequence_1.SeqData);
    TempSeq1 := Sequence_1.SeqData;
    TempSeq2 := Sequence_2.SeqData;
  end;

  GetMem(ASeq1, sizeOf(AnsiChar)*(MyNumSites+1));
  GetMem(ASeq2, sizeOf(AnsiChar)*(MyNumSites+1));



  for i := 0 to MyNumsites - 1 do
  begin
    ASeq1[i] := NucToDistMap(TempSeq1[i + 1]);
    ASeq2[i] := NucToDistMap(TempSeq2[i + 1]);
  end;
  MappedSequences.Add(ASeq1);
  MappedSequences.Add(ASeq2);
  NumSites := MyNumSites;
  Result := MappedSequences;
end;

/// <summary>Computes pairwise distances for aligned pairs of sequences in the given TSequenceList.</summary>
/// <param>AlignedPairs: TSequenceList where pairs 0-1, 2-3, 4-5 ... are aligned pairwise.</param>
/// <param>MAI: TAnalysisInfo</param>
/// <return>TPairwiseDistances: array of extenced</return>
function ComputePairwiseDistances(AlignedPairs: TSequenceList; MAI: TAnalysisInfo):TPairwiseDistances;
var
  DistComputer: TSeqDistBase = nil;
  MyDistanceMatrix: PDistanceMatrix = nil;
  NumAlignedSequences: Integer;
  Index: Integer;
  PairwiseDistances: TPairwiseDistances;
  MyNumSites: Integer;
  MappedData: TList;
begin
  MappedData := TList.Create;
  SetLength(PairwiseDistances, AlignedPairs.Count div 2);

  NumAlignedSequences := AlignedPairs.Count;
  MyDistanceMatrix := NewDistMatrix(2, True);

  if MAI.MyDistPack.DoesContain(gdOneNuc) then
    DistComputer := TNucDist.Create
  else if MAI.MyDistPack.DoesContain(gdAmino)
    then  DistComputer := TAminoDist.Create;

  DistComputer.DistPack := MAI.MyDistPack;
  DistComputer.NoOfSeqs := 2;
  DistComputer.QuickExit := True;
  DistComputer.D := MyDistanceMatrix;

  Index := 0;
  while Index < NumAlignedSequences do
  begin
    while MappedData.Count > 0 do
    begin
      MappedData.Delete(0);
    end;

    MyNumSites := 0;
    MappedData := PrepareRNADataForDistAnalysis(AlignedPairs[Index], AlignedPairs[Index + 1], MAI.MySubsetOpt, MyNumSites);
    inc(Index);
    inc(Index);

    DistComputer.Sequences := MappedData;
    DistComputer.NoOfSites := MyNumSites;

    if DistComputer is TNucDist then
    begin
      if MAI.MyDistPack.DoesContain(gdMCL) then
        TNucDist(DistComputer).ComputeDistancesSE
      else
        TNucDist(DistComputer).ComputeDistances;
    end
    else if DistComputer is TAminoDist then
      TAminoDist(DistComputer).ComputeDistances;

    PairwiseDistances[((Index) div 2) - 1] := (MyDistanceMatrix[1][0]);
  end;

  Result := PairwiseDistances;
  FreeAndNil(DistComputer);
  FreeDistMatrix(MyDistanceMatrix, 2);
  FreeAndNil(MappedData);
end;

/// <summary>Return a TStringList where each string is a row in the given
/// distance matrix. It is assumed that a square distance matrix is given</summary>
function DistanceMatrixToStringList(Matrix: PDistanceMatrix; NumTaxa:Integer): TStringList;
var
  i: Integer;
  j: Integer;
  MyString: String;
  MyStringList: TStringList;
begin
  MyStringList := TStringList.Create;
  for i := 0 to NumTaxa - 1 do
  begin
    MyString := EmptyStr;
    for j := 0 to NumTaxa - 1 do
    begin
      MyString := MyString + '[' + FloatToStrF(Matrix[i][j], ffFixed, 3, 3) + ']';
    end;
    MyString := MyString + '=';
    MyStringList.Add(MyString);
  end;
  Result := MyStringList;
end;


end.
