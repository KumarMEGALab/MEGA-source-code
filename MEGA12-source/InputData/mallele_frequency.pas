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

unit mallele_frequency;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MegaConsts;

type

  { TAlleleFrequency }

  TAlleleFrequency = class(TObject)
    private
      FMajorAllele: AnsiChar;
      FMajorAlleleSetFromReferenceSequence: Boolean;
      FMinorAllele: AnsiChar;
      FAlleleFreqs: TArrayOfInteger;
      FIsComputed: Boolean;
      FNumSeqs: Integer;
      FNumSites: Integer;
      FIsAminoAcid: Boolean;
      FReferenceMajorAllele: AnsiChar;
      function GetMajorAllele: AnsiChar;
      function GetMinorAllele: AnsiChar;
      procedure ComputeValues;
      function NumSitesFormatString: String;
    public

      constructor Create(isAmino: Boolean; numSeqs, numSites: Integer);
      destructor Destroy; override;
      function DeveloperString(site: Integer): String;
      function Frequency(allele: AnsiChar): Integer;
      function PercentFrequency(allele: AnsiChar): Extended;
      function MinorAllelePercentFrequency: Extended;
      function MajorAllelePercentFrequency: Extended;
      procedure SetMajorAlleleFromReferenceSequence(allele: AnsiChar);
      procedure IncrementFrequency(allele: AnsiChar);
      property IsAminoAcid: Boolean read FIsAminoAcid;
      property MajorAllele: AnsiChar read GetMajorAllele;
      property MinorAllele: AnsiChar read GetMinorAllele;
      property AlleleFreqs: TArrayOfInteger read FAlleleFreqs write FAlleleFreqs;
      property MajorAlleleSetFromReferenceSequence: Boolean read FMajorAlleleSetFromReferenceSequence;
      property ReferenceMajorAllele: AnsiChar read FReferenceMajorAllele;
  end;

  TAlleleFrequencyArray = array of TAlleleFrequency;

implementation

uses
  MegaUtils;

{ TAlleleFrequency }

function TAlleleFrequency.GetMajorAllele: AnsiChar;
begin
  if not FIsComputed then
    ComputeValues;
  if FMajorAlleleSetFromReferenceSequence then
    Result := FReferenceMajorAllele
  else
    Result := FMajorAllele;
end;

function TAlleleFrequency.GetMinorAllele: AnsiChar;
begin
  if not FIsComputed then
    ComputeValues;

  if FMajorAlleleSetFromReferenceSequence then
  begin
    if FMajorAllele <> FReferenceMajorAllele then
      Result := FMajorAllele
    else
      Result := FMinorAllele;
    Exit;
  end;

  Result := FMinorAllele;
end;

procedure TAlleleFrequency.ComputeValues;
var
  i: Integer;
  minorFreq: Integer = 0;
begin
  FMajorAllele := #0;
  if FIsAminoAcid then
    FMinorAllele := #20
  else
    FMinorAllele := #4;
  for i := Low(FAlleleFreqs) to High(FAlleleFreqs) do
  begin
    if FAlleleFreqs[i] > FAlleleFreqs[ord(FMajorAllele)] then
      FMajorAllele := chr(i);
  end;

  for i := Low(FAlleleFreqs) to High(FAlleleFreqs) do
  begin
    if (FAlleleFreqs[i] > minorFreq) and (FAlleleFreqs[i] < FAlleleFreqs[ord(FMajorAllele)]) then
    begin
      minorFreq := FAlleleFreqs[i];
      FMinorAllele := chr(i);
    end;
  end;
end;

function TAlleleFrequency.NumSitesFormatString: String;
begin
  if FNumSites >= 1.0E+9 then       { site=1,000,000,000}
    Result := 'site=%15.0n'
  else if FNumSites >= 1.0E+8  then {   site=100,000,000}
    Result := 'site=%13.0n'
  else if FNumSites >= 1.0E+8 then  {    site=10,000,000}
    Result := 'site=%12.0n'
  else if FNumSites >= 1.0E+7 then  {     site=1,000,000}
    Result := 'site=%11.0n'
  else if FNumSites >= 1.0E+6 then  {       site=100,000}
    Result := 'site=%9.0n'
  else if FNumSites >= 1.0E+5 then  {        site=10,000}
    Result := 'site=%8.0n'
  else if FNumSites >= 1.0E+4 then  {         site=1,000}
    Result := 'site=%7.0n'
  else if FNumSites >= 1.0E+3 then  {           site=100}
    Result := 'site=%6.0n'
  else
    Result := 'site=%5.0n';
end;

constructor TAlleleFrequency.Create(isAmino: Boolean; numSeqs, numSites: Integer);
var
  i: Integer;
begin
  FIsComputed := False;
  FIsAminoAcid := isAmino;
  FNumSeqs := numSeqs;
  FNumSites := numSites;
  if FIsAminoAcid then
    SetLength(FAlleleFreqs, 21)
  else
    SetLength(FAlleleFreqs, 6);
  for i := Low(FAlleleFreqs) to High(FAlleleFreqs) do
    FAlleleFreqs[i] := 0;
end;

destructor TAlleleFrequency.Destroy;
begin
  SetLength(FAlleleFreqs, 0);
  inherited Destroy;
end;

function TAlleleFrequency.DeveloperString(site: Integer): String;
var
  fStr: String = '';
begin
  fStr := NumSitesFormatString;
  if FIsAminoAcid then
    Result := Format(fStr + '  Major=%s(%5.2f%%)  Minor=%s(%5.2f%%)     A=%-10d C=%-10d D=%-10d E=%-10d F=%-10d G=%-10d H=%-10d I=%-10d K=%-10d L=%-10d M=%-10d N=%-10d P=%-10d Q=%-10d R=%-10d S=%-10d T=%-10d V=%-10d W=%-10d Y=%-10d X=%-10d', [site*1.0, DistMapToAmino(MajorAllele), MajorAllelePercentFrequency*100, DistMapToAmino(MinorAllele), MajorAllelePercentFrequency*100, FAlleleFreqs[0], FAlleleFreqs[1], FAlleleFreqs[2], FAlleleFreqs[3], FAlleleFreqs[4], FAlleleFreqs[5], FAlleleFreqs[6], FAlleleFreqs[7], FAlleleFreqs[8], FAlleleFreqs[9], FAlleleFreqs[10], FAlleleFreqs[11], FAlleleFreqs[12], FAlleleFreqs[13], FAlleleFreqs[14], FAlleleFreqs[15], FAlleleFreqs[16], FAlleleFreqs[17], FAlleleFreqs[18], FAlleleFreqs[19], FAlleleFreqs[20]])
  else
    Result := Format(fStr + '  Major=%s(%5.2f%%)  Minor=%s(%5.2f%%)     A=%-10d T=%-10d C=%-10d G=%-10d N=%-10d', [site*1.0, DistMapToNuc(MajorAllele), MajorAllelePercentFrequency*100, DistMapToNuc(MinorAllele), MinorAllelePercentFrequency*100, FAlleleFreqs[0], FAlleleFreqs[1], FAlleleFreqs[2], FAlleleFreqs[3], FAlleleFreqs[4], FAlleleFreqs[5]]);
end;

function TAlleleFrequency.Frequency(allele: AnsiChar): Integer;
begin
  if not FIsComputed then
    ComputeValues;
  Result := FAlleleFreqs[ord(allele)];
end;

function TAlleleFrequency.PercentFrequency(allele: AnsiChar): Extended;
begin
  if not FIsComputed then
    ComputeValues;
  Result := FAlleleFreqs[ord(allele)]/FNumSeqs;
end;

function TAlleleFrequency.MinorAllelePercentFrequency: Extended;
begin
  if not FIsComputed then
    ComputeValues;
  if IsAminoAcid then
  begin
    if(MinorAllele <> gdResiX) then
      Result := FAlleleFreqs[ord(MinorAllele)]/FNumSeqs
    else
      Result := 0.0;
  end
  else
  begin
    if MinorAllele <> gdBaseN then
      Result := FAlleleFreqs[ord(MinorAllele)]/FNumSeqs
    else
      Result := 0.0;
  end;
end;

function TAlleleFrequency.MajorAllelePercentFrequency: Extended;
begin
  if not FIsComputed then
    ComputeValues;
  if IsAminoAcid then
  begin
    if(MajorAllele <> gdResiX) then
      Result := FAlleleFreqs[ord(MajorAllele)]/FNumSeqs
    else
      Result := 0.0;
  end
  else
  begin
    if MajorAllele <> gdBaseN then
      Result := FAlleleFreqs[ord(MajorAllele)]/FNumSeqs
    else
      Result := 0.0;
  end;
end;

procedure TAlleleFrequency.SetMajorAlleleFromReferenceSequence(allele: AnsiChar);
begin
  FMajorAlleleSetFromReferenceSequence := True;
  if IsAminoAcid then
    FReferenceMajorAllele := AminoToDistMap(allele)
  else
    FReferenceMajorAllele := NucToDistMap(allele);
end;

procedure TAlleleFrequency.IncrementFrequency(allele: AnsiChar);
begin
  inc(FAlleleFreqs[ord(NucToDistMap(allele))]);
  if FIsComputed then
    FIsComputed := False;
end;

end.

