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

unit mfastaparser;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, MD_Sequences, MDataFileInfo, MegaUtils;

type

  { TFastaParser }

  TFastaParser = class(TObject)
    private
      FNoOfTaxa: Integer;
      FDataFileInfo: TDataFileInfo;
      FNoOfSites: Integer;
      FFilename: String;
      FUnknownNoOfSites: Boolean;
      FFastaFile: TextFile;
      FLines: Integer;
      function GetSequence(Index: Integer): TSequence;
      procedure SetDataFileInfo(AValue: TDataFileInfo);
      procedure SetFilename(AValue: String);
      function IsValidNucBase(c: Char): Boolean;
      function IsValidAmino(c: Char): Boolean;
      procedure SetUnknownNoOfSites(AValue: Boolean);
    protected
      FSeqs: TSequenceList;
      procedure Clear;
      procedure ReadName;
      procedure ReadSequence;
      procedure ReadGreaterThan;
    public
      constructor Create(fileInfo: TDataFileInfo; filename: String);
      destructor Destroy; override;
      function NumSeqs: Integer;
      function NumSites: Integer;
      function Parse: Boolean;
      function GetSeqs: TSequenceList;
      function GetLine: Integer;
      property Filename: String read FFilename write SetFilename;
      property UnknownNoOfSites: Boolean read FUnknownNoOfSites write SetUnknownNoOfSites;
      property DataFileInfo: TDataFileInfo read FDataFileInfo write SetDataFileInfo;
      property Items[Index: Integer]: TSequence read GetSequence; Default;
  end;


implementation

uses
  KeywordConsts;

{ TFastaParser }

procedure TFastaParser.SetFilename(AValue: String);
begin
  if FFilename=AValue then Exit;
  FFilename:=AValue;
end;

procedure TFastaParser.SetDataFileInfo(AValue: TDataFileInfo);
begin
  if FDataFileInfo=AValue then Exit;
  FDataFileInfo:=AValue;
end;

function TFastaParser.GetSequence(Index: Integer): TSequence;
begin
  if (FSeqs.Count = 0) or (Index < 0) or (Index >= FSeqs.Count) then
    raise Exception.Create('index out of bounds for TSequenceList');
  Result := FSeqs[Index];
end;

function TFastaParser.IsValidNucBase(c: Char): Boolean;
begin
  Result := False;
  case UpCase(c) of
    'A'..'D','G','H','K','M','R','S','T','U','V','W','Y','N': Result := True;
  end;
  if (c = FDataFileInfo.GetMissingBaseSymbol) or (c = FDataFileInfo.GetIdenticalBaseChar) or (c = FDataFileInfo.GetGapSymbol) then
    Result := True;
end;

function TFastaParser.IsValidAmino(c: Char): Boolean;
begin
  Result := False;
  case UpCase(c) of
  'A'..'I','K'..'N', 'P'..'T', 'V'..'Z', '*': Result := True;
  end;
  if (c = FDataFileInfo.GetMissingBaseSymbol) or (c = FDataFileInfo.GetIdenticalBaseChar) or (c = FDataFileInfo.GetGapSymbol) then
    Result := True;
end;

procedure TFastaParser.SetUnknownNoOfSites(AValue: Boolean);
begin
  if FUnknownNoOfSites=AValue then Exit;
  FUnknownNoOfSites:=AValue;
end;

procedure TFastaParser.Clear;
begin
  FSeqs.Clear;
end;

procedure TFastaParser.ReadName;
var
  charIn: Char;
  Name: String = '';
  fastaSeq: TSequence;
begin
  while not Eof(FFastaFile) do
  begin
    Read(FFastaFile, charIn);
    if (charIn = #10) or (charIn = #13) then
      break;
    Name := Name + charIn;
  end;
  {$IFDEF MSWINDOWS}
  if charIn = #13 then // If we read in a CR then there's still a LF which has to be read off before we have the next line.
    Read(FFastaFile, charIn);
  {$ENDIF}
  inc(FLines);
  TrimTaxaName2(Name);
  if (Name = EmptyStr) then
    Raise Exception.Create('Fasta parse error.  Expected a name to follow ''>'', but none found');
  fastaSeq := TSequence.Create;
  fastaSeq.SeqName := Name;
  FSeqs.Add(fastaSeq);
end;

procedure TFastaParser.ReadSequence;
var
  Seq: String = '';
  charIn: Char;
  FoundGreaterThan: Boolean;
  SiteNo: Integer;
begin
  SiteNo := 0;
  FoundGreaterThan := False;
  while (not Eof(FFastaFile)) and (not FoundGreaterThan) do
  begin
    Read(FFastaFile, charIn);
    if (charIn = #13) or (charIn = #10) or (charIn = ' ') then
    begin
      if charIn = #10 then
        inc(FLines);
      Continue;
    end;
    if not FUnknownNoOfSites then
    begin
      if Length(Seq) = FNoOfSites then // This char MUST be a >, the start of a new seq or we have different length seqs.
      begin
        if charIn <> '>' then
          Raise Exception.Create('Fasta parse error.  Expected ''>'', but found ' + charIn + '.  Sequences are different lengths, check that they have been aligned BEFORE analysis.');
      end
      else
      begin
        if charIn = '>' then // We have encountered the name of another seq before this one was finished, different seq lengths.
          Raise Exception.Create('Fasta parse error.  Expected sequence character, but found ''>''.  Sequences are different lengths.');
      end;
    end;
    inc(SiteNo);
    if charIn = '>' then
      FoundGreaterThan := True
    else
    begin
      if charIn = FDataFileInfo.GetIdenticalBaseChar then // Replace the identical symbol with the actual symbol it is identical to.
      begin
        if FNoOfTaxa = 1 then
          Raise Exception.Create('Fasta parse error.  An identical symbol was found in the first sequence.');
        charIn := FSeqs[0][Length(Seq)];
      end;
      if FDataFileInfo.GetDataType = snNucleotide then
      begin
        if not IsValidNucBase(charIn) then
          Raise Exception.Create('Invalid base found.  Expecting a nucleotide base, but found ''' + charIn + ''' ');
      end
      else
      begin
        if not IsValidAmino(charIn) then
          Raise Exception.Create('Invalid Amino found.  Expecting an amino acid, but found ''' + charIn + ''' ');
      end;
      Seq := Seq + charIn;
    end;
  end;
  if (Trim(Seq) = EmptyStr) then
    Raise Exception.Create('FastA data parsing error.  Expected a sequence, but none found.');

  if UnknownNoOfSites then
  begin
    UnknownNoOfSites := False;
    FNoOfSites := length(Seq);
  end;
  FSeqs[FSeqs.Count - 1].SeqData := Seq;
end;

procedure TFastaParser.ReadGreaterThan;
var
  letter: Char;
begin
  Read(FFastaFile, letter);
  if letter <> '>' then
    Raise Exception.Create('Fasta parse error.  Expected ''>'', but found ' + letter + '.');
end;

constructor TFastaParser.Create(fileInfo: TDataFileInfo; filename: String);
begin
  FSeqs := TSequenceList.Create;
  FUnknownNoOfSites := True;
  FNoOfSites := 0;
  FLines := 0;
  FDataFileInfo := fileInfo;
  FFilename := filename;
end;

destructor TFastaParser.Destroy;
begin
  Clear;
  FDataFileInfo := nil; { not owned by us}
  FSeqs.Free;
  inherited Destroy;
end;

function TFastaParser.NumSeqs: Integer;
begin
  Result := FSeqs.Count;
end;

function TFastaParser.NumSites: Integer;
begin
  Result := FNoOfSites;
end;

function TFastaParser.Parse: Boolean;
begin
  Result := False;
  FLines := 1;
  FUnknownNoOfSites := True;
  if not FileExists(FFilename) then
    raise Exception.Create('Input data file not found: ' + FFilename);
  try
    try
      AssignFile(FFastaFile, FFilename);
      Reset(FFastaFile);
      ReadGreaterThan;
      while not Eof(FFastaFile) do
      begin
        ReadName;
        ReadSequence;
      end;
      Result := (FSeqs.Count > 0);
    Except
      on E: Exception do
        Raise Exception.Create(E.Message + ' On line: ' + IntToStr(FLines - 1));
    end;
  finally
    CloseFile(FFastaFile);
  end;
end;

function TFastaParser.GetSeqs: TSequenceList;
begin
  Result := TSequenceList.Create;
  Result.Assign(FSeqs);
end;

function TFastaParser.GetLine: Integer;
begin
  Result := FLines;
end;


end.

