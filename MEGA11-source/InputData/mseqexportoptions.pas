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

unit mseqexportoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MegaConsts;

{ TSeqExportOptions }
type
  TSeqExportOptions = class(TObject)
    private
      FExportFormat: TMegaExportFormat;
      procedure SetExportFormat(AValue: TMegaExportFormat);
    public
      isNucData : Boolean;              //If the data to export is Nucleotide data
      isChooseBases : Boolean;          //Used for setup
      IsRemove1stBase : Boolean;      //If the 1st is checked in the list of bases
      IsRemove2ndBase : Boolean;      //If the 2nd is checked in the list of bases
      IsRemove3rdBase : Boolean;      //If the 3rd is checked in the list of bases
      IsRemove0thBase : Boolean;      //If the 0th is checked in the list of bases
      IsRemoveMissData : Boolean;   //if the user has selected to remove the MissSumData
      IsRemoveGapData : Boolean;       //If the user has selected to remove the GapData
      IsWriteSiteNumAtEnd : Boolean;
      IsWriteSiteNumAtTop : Boolean;
      IsInterleaved : Boolean;
      IsExportHighlightedSites : Boolean;
      IsExportUnHighlightedSites : Boolean;
      IsPAUP4Format : Boolean;
      IsPAUP3Format : Boolean;
      IsMEGAFormat : Boolean;
      IsPhylipFormat : Boolean;
      IsEXCELFormat : Boolean;
      IsExcelXmlFormat: Boolean;
      IsOdsFormat: Boolean;
      IsCSVFormat : Boolean;
      IsFastaFormat: Boolean;
      IsCodonByCodonWriting : Boolean;
      IsStandardNames: Boolean;
      SitesPerLine : Integer;
      FileTitle : String;
      FileDescription : String;
      ExportFullCodons: Boolean;
      IsInMainThread: Boolean;
      constructor Create;
      function IsSpreadsheetFormat: Boolean;
      function DebugDump: TStringList;
      property ExportFormat: TMegaExportFormat read FExportFormat write SetExportFormat;
  end;

  function StringToMegaExportFormat(aString: String): TMegaExportFormat;
  function MegaExportFormatToString(aFormat: TMegaExportFormat): String;

implementation

uses
  mbrowserutils;

function StringToMegaExportFormat(aString: String): TMegaExportFormat;
begin
  if SameText(aString, MEF_MEGA) then
    Result := mefMega
  else if SameText(aString, MEF_FASTA) then
    Result := mefFasta
  else if SameText(aString, MEF_PAUP3) then
    Result := mefPaup3
  else if SameText(aString, MEF_PAUP4) then
    Result := mefPaup4
  else if SameText(aString, MEF_CSV) then
    Result := mefCsv
  else if SameText(aString, MEF_EXCEL) then
    Result := mefExcel
  else if SameText(aString, MEF_EXCEL_XML) then
    Result := mefExcelXml
  else if SameText(aString, MEF_ODS) then
    Result := mefOds
  else if SameText(aString, MEF_PHYLIP) then
    Result := mefPhylip
  else if SameText(aString, MEF_TEXT) then
    Result := mefTxt
  else
    raise Exception.Create('Invalid export format: ' + aString);
end;

function MegaExportFormatToString(aFormat: TMegaExportFormat): String;
begin
  case aFormat of
    mefPaup4, mefNexus: Result := MEF_PAUP4;
    mefPaup3: Result := MEF_PAUP3;
    mefMega: Result := MEF_MEGA;
    mefFasta: Result := MEF_FASTA;
    mefPhylip: Result := MEF_PHYLIP;
    mefExcel: Result := MEF_EXCEL;
    mefExcelXml: Result := MEF_EXCEL_XML;
    mefOds: Result := MEF_ODS;
    mefCsv: Result := MEF_CSV;
    mefTxt: Result := MEF_TEXT;
  end;
end;

{ TSeqExportOptions }

procedure TSeqExportOptions.SetExportFormat(AValue: TMegaExportFormat);
begin
  if FExportFormat=AValue then Exit;
  FExportFormat:=AValue;
  IsPAUP4Format := False;
  IsPAUP3Format := False;
  IsMEGAFormat := False;
  IsPhylipFormat := False;
  IsEXCELFormat := False;
  IsExcelXmlFormat:= False;
  IsCSVFormat := False;
  IsFastaFormat := False;
  case FExportFormat of
    mefPaup4, mefNexus: IsPAUP4Format := True;
    mefPaup3: IsPAUP3Format := True;
    mefMega: IsMEGAFormat := True;
    mefFasta: IsFastaFormat := True;
    mefPhylip: IsPhylipFormat := True;
    mefExcel: IsExcelFormat := True;
    mefExcelXml: IsExcelXmlFormat := True;
    mefOds: IsOdsFormat := True;
    mefCsv: IsCSVFormat := True;
    mefTxt: Assert(False, 'not implemented');
  end;
end;

constructor TSeqExportOptions.Create;
begin
  IsInMainThread := True;
  isNucData := True;
  isChooseBases := False;
  IsRemove1stBase := False;
  IsRemove2ndBase := False;
  IsRemove3rdBase := False;
  IsRemove0thBase := False;
  IsRemoveMissData := False;
  IsRemoveGapData := False;
  IsWriteSiteNumAtEnd := False;
  IsWriteSiteNumAtTop := False;
  IsInterleaved := True;
  IsExportHighlightedSites := True;
  IsExportUnHighlightedSites := True;
  IsPAUP4Format := False;
  IsPAUP3Format := False;
  IsMEGAFormat := True;
  IsPhylipFormat := False;
  IsEXCELFormat := False;
  IsExcelXmlFormat:= False;
  IsOdsFormat := False;
  IsCSVFormat := False;
  IsFastaFormat := False;
  IsCodonByCodonWriting := False;
  IsStandardNames := False;
  SitesPerLine := 80;
  FileTitle := EmptyStr;
  FileDescription := EmptyStr;
  ExportFullCodons := False;
  ExportFormat := mefMega;
end;

function TSeqExportOptions.IsSpreadsheetFormat: Boolean;
begin
  case FExportFormat of
    mefExcel, mefExcelXml, mefOds, mefCsv: Result := True;
    else
      Result := False;
  end;
end;

function TSeqExportOptions.DebugDump: TStringList;
var
  aStr: String;
begin
  Result := TStringList.Create;
  aStr := Format('isNucData                 : %s', [BoolToStr(isNucData, True)]);
  Result.Add(aStr);
  aStr := Format('isChooseBases             : %s', [BoolToStr(isChooseBases , True)]);
  Result.Add(aStr);
  aStr := Format('IsRemove1stBase           : %s', [BoolToStr(IsRemove1stBase , True)]);
  Result.Add(aStr);
  aStr := Format('IsRemove2ndBase           : %s', [BoolToStr(IsRemove2ndBase , True)]);
  Result.Add(aStr);
  aStr := Format('IsRemove3rdBase           : %s', [BoolToStr(IsRemove3rdBase , True)]);
  Result.Add(aStr);
  aStr := Format('IsRemove0thBase           : %s', [BoolToStr(IsRemove0thBase , True)]);
  Result.Add(aStr);
  aStr := Format('IsRemoveMissData          : %s', [BoolToStr(IsRemoveMissData , True)]);
  Result.Add(aStr);
  aStr := Format('IsRemoveGapData           : %s', [BoolToStr(IsRemoveGapData , True)]);
  Result.Add(aStr);
  aStr := Format('IsWriteSiteNumAtEnd       : %s', [BoolToStr(IsWriteSiteNumAtEnd , True)]);
  Result.Add(aStr);
  aStr := Format('IsWriteSiteNumAtTop       : %s', [BoolToStr(IsWriteSiteNumAtTop , True)]);
  Result.Add(aStr);
  aStr := Format('IsInterleaved             : %s', [BoolToStr(IsInterleaved , True)]);
  Result.Add(aStr);
  aStr := Format('IsExportHighlightedSites  : %s', [BoolToStr(IsExportHighlightedSites , True)]);
  Result.Add(aStr);
  aStr := Format('IsExportUnHighlightedSites: %s', [BoolToStr(IsExportUnHighlightedSites , True)]);
  Result.Add(aStr);
  aStr := Format('IsPAUP4Format             : %s', [BoolToStr(IsPAUP4Format , True)]);
  Result.Add(aStr);
  aStr := Format('IsPAUP3Format             : %s', [BoolToStr(IsPAUP3Format , True)]);
  Result.Add(aStr);
  aStr := Format('IsMEGAFormat              : %s', [BoolToStr(IsMEGAFormat , True)]);
  Result.Add(aStr);
  aStr := Format('IsPhylipFormat            : %s', [BoolToStr(IsPhylipFormat , True)]);
  Result.Add(aStr);
  aStr := Format('IsEXCELFormat             : %s', [BoolToStr(IsEXCELFormat , True)]);
  Result.Add(aStr);
  aStr := Format('IsExcelXmlFormat          : %s', [BoolToStr(IsExcelXmlFormat , True)]);
  Result.Add(aStr);
  aStr := Format('IsCSVFormat               : %s', [BoolToStr(IsCSVFormat , True)]);
  Result.Add(aStr);
  aStr := Format('IsFastaFormat             : %s', [BoolToStr(IsFastaFormat , True)]);
  Result.Add(aStr);
  aStr := Format('IsCodonByCodonWriting     : %s', [BoolToStr(IsCodonByCodonWriting , True)]);
  Result.Add(aStr);
  aStr := Format('IsStandardNames           : %s', [BoolToStr(IsStandardNames , True)]);
  Result.Add(aStr);
  aStr := Format('SitesPerLine              : %d', [SitesPerLine]);
  Result.Add(aStr);
  aStr := Format('FileTitle                 : %s', [FileTitle]);
  Result.Add(aStr);
  aStr := Format('FileDescription           : %s', [FileDescription]);
  Result.Add(aStr);
  aStr := Format('ExportFormat              : %s', [MegaExportFormatToString(ExportFormat)]);
  Result.Add(aStr);
end;

end.

