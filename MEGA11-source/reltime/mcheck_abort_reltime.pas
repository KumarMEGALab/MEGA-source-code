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

unit mcheck_abort_reltime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mruntimeprogressdlg, mentropy;

type

  { TFindIdenticalSeqs }

  TFindIdenticalSeqs = class(TObject)
    private
      FEntropyList: TShannonEntropyList;
      FArp: TRuntimeProgress;
      FIsAminoData: Boolean;
      FNumSites: Integer;
      function ConvertOlsToMLSeqs(olsSeqs: TList; numSites: Integer): TStringList;
      procedure InitEntropyList(seqData: TStringList); overload;
      procedure InitEntropyList(seqData: TList; numSites: Integer); overload;
    public
      constructor Create(isAmino: Boolean);
      destructor Destroy; override;

      function FindIdenticalSequences(mlSeqs: TStringList; otuNames: TStringList; arp: TRuntimeProgress): TStringList; overload;
      function FindIdenticalSequences(olsSeqs: TList; otuNames: TStringList; numSites: Integer; arp: TRuntimeProgress): TStringList; overload;
      property IsAminoData: Boolean read FIsAminoData;
  end;

  { TCheckAbortReltime }

  TCheckAbortReltime = class(TObject)
    private
      FIsAminoAcidData: Boolean;
      FSeqsFinder: TFindIdenticalSeqs;
      function CheckAbort(identicalSeqs: TStringList): Boolean;
    public
      constructor Create(isAmino: Boolean);
      destructor Destroy; override;


      function CheckAbortML(seqs: TStringList; otuNames: TStringList; arp: TRuntimeProgress): Boolean;
      function CheckAbortOLS(seqs: TList; otuNames: TStringList; numSites: Integer; arp: TRuntimeProgress): Boolean;
      property IsAminoAcidData: Boolean read FIsAminoAcidData;
  end;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  Dialogs, Controls, Forms,
  {$ELSE}
  MegaUtils_NV,
  {$ENDIF}
  MegaConsts, math, dateutils;

{ TFindIdenticalSeqs }

function TFindIdenticalSeqs.ConvertOlsToMLSeqs(olsSeqs: TList; numSites: Integer): TStringList;
var
  i: Integer;
  aSeq: PAnsiChar;
  aSeqString: AnsiString;
begin
  Result := TStringList.Create;
  for i := 0 to olsSeqs.Count - 1 do
  begin
    aSeq := PAnsiChar(olsSeqs[i]);
    SetString(aSeqString, @aSeq[0], numSites);
    Result.Add(aSeqString);
  end;
end;

procedure TFindIdenticalSeqs.InitEntropyList(seqData: TStringList);
var
  p: PAnsiChar = nil;
  i: Integer;
  e: TShannonEntropy = nil;
  sitesUsed: TIntArray;
  numSites: Integer;
begin
  if seqData.Count > 0 then
  begin
    numSites := Length(seqData[0]);
    SetLength(sitesUsed, numSites);
    for i := 0 to numSites - 1 do
      sitesUsed[i] := i;
    for i := 0 to seqData.Count - 1 do
    begin
      p := PAnsiChar(seqData[i]);
      e := TShannonEntropy.Create(p, sitesUsed, FIsAminoData);
      FEntropyList.Add(e);
    end;
  end;
end;

procedure TFindIdenticalSeqs.InitEntropyList(seqData: TList; numSites: Integer);
var
  p: PAnsiChar = nil;
  i: Integer;
  e: TShannonEntropy = nil;
  sitesUsed: TIntArray;
begin
  if seqData.Count > 0 then
  begin
    SetLength(sitesUsed, numSites);
    for i := 0 to numSites - 1 do
      sitesUsed[i] := i;
    for i := 0 to seqData.Count - 1 do
    begin
      p := PAnsiChar(seqData[i]);
      e := TShannonEntropy.Create(p, sitesUsed, FIsAminoData);
      FEntropyList.Add(e);
    end;
  end;
end;

constructor TFindIdenticalSeqs.Create(isAmino: Boolean);
begin
  FArp := nil;
  FEntropyList := TShannonEntropyList.Create;
  FIsAminoData := isAmino;
end;

destructor TFindIdenticalSeqs.Destroy;
var
  i: Integer;
  e: TShannonEntropy = nil;
begin
  FArp := nil;
  if Assigned(FEntropyList) then
  begin
    if FEntropyList.Count > 0 then
      for i := 0 to FEntropyList.Count - 1 do
      begin
        e := FEntropyList[i];
        e.Free;
      end;
    FEntropyList.Free;
  end;
  inherited Destroy;
end;

function TFindIdenticalSeqs.FindIdenticalSequences(mlSeqs: TStringList; otuNames: TStringList; arp: TRuntimeProgress): TStringList;
var
  i, j, site: Integer;
  areIdentical: Boolean;
  updateTime: TDateTime;
begin
  FArp := arp;
  InitEntropyList(mlSeqs);
  Result := TStringList.Create;
  if mlSeqs.Count < 2 then
    Exit;
  FNumSites := Length(mlSeqs[0]);
  updateTime := Now;
  for i := 0 to mlSeqs.Count - 2 do
    for j := i + 1 to mlSeqs.Count - 1 do
    begin
      if CompareValue(FEntropyList[i].Entropy, FEntropyList[j].Entropy) = 0 then
      begin
        site := 1;
        areIdentical := True;
        while site <= FNumSites do
        begin
          if mlSeqs[i][site] <> mlSeqs[j][site] then
          begin
            areIdentical := False;
            break;
          end;
          inc(site);
        end;
        if areIdentical then
          Result.Add(otuNames[j] + '=' + otuNames[i]);
      end;
      if Assigned(FArp) and (MillisecondsBetween(Now, updateTime) > 200) then
      begin
        FArp.UpdatePercentProgress(Round(i/mlSeqs.Count*100));
        updateTime := Now;
        {$IFDEF VISUAL_BUILD}
        Application.ProcessMessages;
        {$ENDIF}
      end;
    end;
end;

function TFindIdenticalSeqs.FindIdenticalSequences(olsSeqs: TList; otuNames: TStringList; numSites: Integer; arp: TRuntimeProgress): TStringList;
var
  i, j: Integer;
  site: Integer;
  areIdentical: Boolean = True;
  s1, s2: PAnsiChar;
begin
  FArp := arp;
  FNumSites := numSites;
  InitEntropyList(olsSeqs, numSites);
  Result := TStringList.Create;
  if olsSeqs.Count < 2 then
    Exit;
  for i := 0 to olsSeqs.Count - 2 do
    for j := i + 1 to olsSeqs.Count - 1 do
    begin
      if CompareValue(FEntropyList[i].Entropy, FEntropyList[j].Entropy ,FP_CUTOFF) = 0 then
      begin
        areIdentical := True;
        site := 0;
        s1 := PAnsiChar(olsSeqs[i]);
        s2 := PAnsiChar(olsSeqs[j]);
        while site < FNumSites do
        begin
          if s1[site] <> s2[site] then
          begin
            areIdentical := False;
            break;
          end;
          inc(site);
        end;
        if areIdentical then
          Result.Add(otuNames[j] + '=' + otuNames[i]);
      end;
    end;
end;

{ TCheckAbortReltime }

function TCheckAbortReltime.CheckAbort(identicalSeqs: TStringList): Boolean;
var
  aMsg: String;
  {$IFNDEF VISUAL_BUILD}i: Integer;{$ENDIF}
  response: Integer;
begin
  Result := False;
  if identicalSeqs.Count > 0 then
  begin
    {$IFDEF VISUAL_BUILD}
    aMsg := Format('At least one sequence pair (%s and %s) with identical data (after subsetting) was found. This may negatively affect the Reltime calculation. Do you want to continue anyway?', [identicalSeqs.Names[0], identicalSeqs.ValueFromIndex[0]]);
    response := MessageDlg('Identical Sequence Data Found', aMsg, mtWarning, mbYesNo, 0);
    if response <> mrYes then
      Result := True;
    {$ELSE}
    aMsg := 'The following sequences are identical which can negatively affect the Reltime calculation:' + LineEnding;
    for i := 0 to identicalSeqs.Count - 1 do
      aMsg := aMsg + #9 + #9 + identicalSeqs.Names[i] + ' and ' + identicalSeqs.ValueFromIndex[i] + LineEnding;
    aMsg := aMsg + 'Please check the results carefully';
    warn_nv(aMsg);
    {$ENDIF}
  end;
end;

constructor TCheckAbortReltime.Create(isAmino: Boolean);
begin
  FIsAminoAcidData := isAmino;
  FSeqsFinder := TFindIdenticalSeqs.Create(FIsAminoAcidData);
end;

destructor TCheckAbortReltime.Destroy;
begin
  if Assigned(FSeqsFinder) then
    FSeqsFinder.Free;
  inherited Destroy;
end;

function TCheckAbortReltime.CheckAbortML(seqs: TStringList; otuNames: TStringList; arp: TRuntimeProgress): Boolean;
var
  identicalSeqs: TStringList = nil;
begin
  try
    identicalSeqs := FSeqsFinder.FindIdenticalSequences(seqs, otuNames, arp);
    Result := CheckAbort(identicalSeqs);
  finally
    if Assigned(identicalSeqs) then
      identicalSeqs.Free;
  end;
end;

function TCheckAbortReltime.CheckAbortOLS(seqs: TList; otuNames: TStringList; numSites: Integer; arp: TRuntimeProgress): Boolean;
var
  identicalSeqs: TStringList = nil;
begin
  try
    identicalSeqs := FSeqsFinder.FindIdenticalSequences(seqs, otuNames, numSites, arp);
    Result := CheckAbort(identicalSeqs);
  finally
    if Assigned(identicalSeqs) then
      identicalSeqs.Free;
  end;
end;

end.

