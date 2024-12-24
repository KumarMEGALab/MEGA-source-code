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

unit mfastaalignmentfileloader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, malignmentfileloader, MDataFileInfo, mfastaparser;

type

  { TFastaAlignmentLoader }

  TFastaAlignmentLoader = class(TAlignmentFileLoader)
    private
      FParser: TFastaParser;
      FFileInfo: TDataFileInfo;
    protected
      procedure InitInputSeqData; override;
      procedure InitOtuInfos; override;
    public
      constructor Create(aFilename: String; fileInfo: TDataFileInfo; aShowProgress: Boolean=True);
      destructor Destroy; override;
      function LoadFile: Boolean; override;
      function TryGetSpecialChars(var gap: Char; var ident: Char; var miss: Char): Boolean; override;
  end;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  mega_main,
  {$ELSE}
  MegaUtils_NV,
  {$ENDIF}
  MD_Sequences, MOtuInfo, KeywordConsts, MEditorForm, MegaErrUtils;

{ TFastaAlignmentLoader }

procedure TFastaAlignmentLoader.InitInputSeqData;
begin
  FInputSeqData.SourceFileName := FFileName;
  InitOtuInfos;
  FInputSeqData.IsNuc := (FFileInfo.GetDataType = snNucleotide);
  FInputSeqData.IsAmino := (FFileInfo.GetDataType = snProtein);
  FInputSeqData.GapSym    := FFileInfo.GetGapSymbol;
  FInputSeqData.MissSym   := FFileInfo.GetMissingBaseSymbol;
  FInputSeqData.IdenSym   := FFileInfo.GetIdenticalBaseChar;
  FInputSeqData.NoOfTaxa  := FParser.NumSeqs;
  FInputSeqData.NoOfSites := FParser.NumSites;
  FInputSeqData.OtuInfos  := FAllOtuInfo;
  {$IFDEF VISUAL_BUILD}
  MegaForm.UpdateMainWindow(FParser.FileName, EmptyStr, EmptyStr, FFileInfo.GetDataType);
  {$ENDIF}
end;

procedure TFastaAlignmentLoader.InitOtuInfos;
var
  i, j: Integer;
  AInfo: TOtuInfo;
  CurSeq: PAnsiChar;
  aSeq: TSequence;
begin
  FAllOtuInfo := TAllOtuInfo.Create;
  FAllOtuInfo.NoOfOtus := FParser.NumSeqs;
  for i:=0 to FParser.NumSeqs - 1 do
  begin
    aSeq := FParser[i];
    AInfo := TOtuInfo.Create;
    AInfo.Id := i;
    AInfo.Name := aSeq.SeqName;
    AInfo.IsUsed := True;
    GetMem(CurSeq, SizeOf(PAnsiChar)*(Length(aSeq.SeqData) + 1));
    for j := 1 to Length(aSeq.SeqData) do
      CurSeq[j - 1] := aSeq.SeqData[j];
    CurSeq[Length(aSeq.SeqData)] := #0;
    AInfo.Data := CurSeq;
    FAllOtuInfo[i] := AInfo;
  end;
end;

constructor TFastaAlignmentLoader.Create(aFilename: String; fileInfo: TDataFileInfo; aShowProgress: Boolean);
begin
  inherited Create(aFilename, aShowProgress);
  FParser := TFastaParser.Create(fileInfo, FFilename);
  FFileInfo := fileInfo;
end;

destructor TFastaAlignmentLoader.Destroy;
begin
  if Assigned(FParser) then
    FParser.Free;
  inherited Destroy;
end;

function TFastaAlignmentLoader.LoadFile: Boolean;
var
  aRow, aCol: Integer;
begin
  Result := False;
  try
    if not FParser.Parse then
      raise Exception.Create('failed to parse fasta file: ' + FFilename);
    InitInputSeqData;
    FSequenceList := FInputSeqData.GenerateTSequenceList;
    Result := True;
  except
    On E: Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      if not FIsConcatenatingFiles then
      begin
        aRow := FParser.GetLine;
        aCol := 1;
        FreeAndNil(FParser);
        OpenFileAndFocus(FFilename, aRow, aCol);
        ShowErrorMessage(E);
      end
      else
        raise Exception.Create(E.Message);
      {$ELSE}
      error_nv('Unable to open your file: ' + E.Message, E);
      {$ENDIF}
    end;
  end
end;

function TFastaAlignmentLoader.TryGetSpecialChars(var gap: Char; var ident: Char; var miss: Char): Boolean;
begin
  gap := FFileInfo.GetGapSymbol;
  ident := FFileInfo.GetIdenticalBaseChar;
  miss := FFileInfo.GetMissingBaseSymbol;
  Result := True;
end;

end.

