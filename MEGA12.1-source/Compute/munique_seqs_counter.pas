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

unit munique_seqs_counter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MegaConsts, fgl;

type

  { TSequenceData }

  TSequenceData = class(TObject)
    private
      FSequence: PAnsiChar;
      FNumSites: Integer;
    public
      constructor Create(data: PAnsiChar; numSites: Integer);
      destructor Destroy; override;
      function IsEqual(other: PAnsiChar): Boolean;
      property Sequence: PAnsiChar read FSequence;
  end;

  TSeqDataList = specialize TFPGObjectList<TSequenceData>;
  TSeqDataMap = specialize TFPGMap<QWord, TSeqDataList>;
  TGroupMap = specialize TFPGMap<Integer, TSeqDataMap>;

  { TUniqueSequenceCounter }

  TUniqueSequenceCounter = class(TObject)
    private
      FInitialized: Boolean;
      FSeqs: TList;
      FNumSites: Integer;
      FNumSeqs: Integer;
      FGroups: TIntArray;
      FSeqFrequenciesByGroup: TGroupMap; { for counting unique sequences in each group/subpopulation}
      FNumGroups: Integer;
      procedure ComputeFrequenciesByGroup;
      function GetHashForSequence(seqData: PAnsiChar): QWord;
    public
      constructor Create(aSeqData: TList; aNumSites: Integer);
      destructor Destroy; override;
      procedure Initialize(aGroups: TIntArray);
      function NumUniqueSeqsInGroup(groupIndex: Integer): Integer;
  end;

implementation

{ TSequenceData }

constructor TSequenceData.Create(data: PAnsiChar; numSites: Integer);
begin
  FSequence := data;
  FNumSites := numSites;
end;

destructor TSequenceData.Destroy;
begin
  FSequence := nil; { no ownership here}
  inherited Destroy;
end;

function TSequenceData.IsEqual(other: PAnsiChar): Boolean;
begin
  Result := CompareMem(FSequence, other, FNumSites);
end;

{ TUniqueSequenceCounter }

procedure TUniqueSequenceCounter.ComputeFrequenciesByGroup;
var
  i: Integer = -1;
  j: Integer = -1;
  MapIndex: Integer = -1;
  Seq: PAnsiChar = nil;
  Hash: QWord = 0;
  GroupSeqMap: TSeqDataMap = nil;
  seqDataList: TSeqDataList = nil;
  sameSequenceExists: Boolean = False;
begin
  FSeqFrequenciesByGroup := TGroupMap.Create;

  for i := 0 to FSeqs.Count - 1 do
  begin
    Seq := PAnsiChar(FSeqs[i]);
    Hash := GetHashForSequence(Seq);

    // Get or create the TSeqMap for this group
    MapIndex := FSeqFrequenciesByGroup.IndexOf(FGroups[i]);
    if MapIndex < 0 then
    begin
      GroupSeqMap := TSeqDataMap.Create;
      FSeqFrequenciesByGroup.Add(FGroups[i], GroupSeqMap);
    end
    else
      GroupSeqMap := FSeqFrequenciesByGroup.Data[MapIndex];

    // Get or create the list of entries for this hash
    MapIndex := GroupSeqMap.IndexOf(Hash);
    if MapIndex < 0 then
    begin
      seqDataList := TSeqDataList.Create(True);
      seqDataList.Add(TSequenceData.Create(Seq, FNumSites));
      GroupSeqMap.Add(Hash, seqDataList);
    end
    else
    begin
      seqDataList := GroupSeqMap.Data[MapIndex];
      sameSequenceExists := False;
      for j := 0 to seqDataList.Count - 1 do
      begin
        if seqDataList[j].IsEqual(Seq) then
        begin
          sameSequenceExists := True;
          Break;
        end;
      end;
      if not sameSequenceExists then
        seqDataList.Add(TSequenceData.Create(Seq, FNumSites));
    end;
  end;
end;

function TUniqueSequenceCounter.GetHashForSequence(seqData: PAnsiChar): QWord; { use the FNV-1a hashing algorithm}
const
  FNV_OFFSET_BASIS = QWord(14695981039346656037);
  FNV_PRIME = QWord(1099511628211);
var
  i: Integer = 0;
begin
  {$Q-}{$R-}  { Disable range checks and overflow checks intentionally. The FNV-1a algorithm intentionally relies on wraparound behavior of fixed width integers, i.e. overflow is by design}
  Result := FNV_OFFSET_BASIS;
  for i := 0 to FNumSites - 1 do
    Result := (Result xor Byte(seqData[i]))*FNV_PRIME;
  {$R+}{$Q+}
end;

constructor TUniqueSequenceCounter.Create(aSeqData: TList; aNumSites: Integer);
begin
  FInitialized := False;
  FSeqs := aSeqData;
  FNumSites := aNumSites;
  FNumSeqs := FSeqs.Count;
end;

destructor TUniqueSequenceCounter.Destroy;
begin
  if Assigned(FSeqFrequenciesByGroup) then
    FSeqFrequenciesByGroup.Free;
  SetLength(FGroups, 0);
  FSeqs := nil; { no ownership here}
  inherited Destroy;
end;

procedure TUniqueSequenceCounter.Initialize(aGroups: TIntArray);
var
  i: Integer = -1;
begin
  SetLength(FGroups, Length(aGroups));
  for i := Low(aGroups) to High(aGroups) do
  begin
    FNumGroups := 0;
    FGroups[i] := aGroups[i];
    if FGroups[i] > FNumGroups then
      FNumGroups := FGroups[i]
  end;
  inc(FNumGroups);
  ComputeFrequenciesByGroup;
  FInitialized := True;
end;

function TUniqueSequenceCounter.NumUniqueSeqsInGroup(groupIndex: Integer): Integer;
var
  i: Integer = -1;
begin
  if not FInitialized then
    raise Exception.Create('Developer error: TUniqueSequenceCounter was not properly initialized');
  Result := 0;
  for i := 0 to FSeqFrequenciesByGroup.Data[groupIndex].Count - 1 do
    inc(Result, FSeqFrequenciesByGroup.Data[groupIndex].Data[i].Count);
end;

end.

