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

unit MDataFileInfo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF VISUAL_BUILD}
  htmloptionsdlg, uCEFInterfaces,
  {$ENDIF}
  KeywordConsts, MegaConsts, SysUtils, mbrowserutils, Classes;

type

{ TDataFileInfo }

 TDataFileInfo = class
  private
    FDataType: TSnTokenCode;
    FContainsCodingNuc: Boolean;
    FDataFormat: TSnTokenCode;
    FDataFileFormat: TDataFileFormat;
    FMissingBaseSymbol: AnsiChar;
    FIdenticalBaseSymbol: AnsiChar;
    FGapSymbol: AnsiChar;
    FFileName: String;
    FOpenInSDE: Boolean;
    FUserSpecifiedSymbols: Boolean;
    procedure InferDataFileFormat;
    procedure SetOpenInSDE(AValue: Boolean);

  public
    NotifyEvent: TNotifyEvent;
    constructor Create(MyFileName: String);
    procedure Assign(Source: TDataFileInfo);
    function GetDataType:TSnTokenCode;
    function GetContainsCodingNuc: Boolean;
    function GetDataFormat: TsnTokenCode;
    function GetDataFileFormat: TDataFileFormat;
    function GetMissingBaseSymbol: AnsiChar;
    function GetIdenticalBaseChar: AnsiChar;
    function GetGapSymbol: AnsiChar;
    function GetFileName: String;
    function IsUserSpecifiedSymbols: Boolean;
    procedure SetDataType(MyDataType: TSnTokenCode);
    procedure SetContainsCodingNuc(isCoding: Boolean);
    procedure SetDataFormat(MyDataFormat: TSnTokenCode);
    procedure SetDataFileFormat(MyDataFileFormat: TDataFileFormat);
    procedure SetMissingBaseSymbol(MyMissingBaseSymbol: AnsiChar);
    procedure SetIdenticalBaseSymbol(MyIdenticalBaseSymbol: AnsiChar);
    procedure SetGapSymbol(MyGapSymbol: AnsiChar);
    procedure SetFileName(MyFileName: String);
    procedure SetUserSpecifiedSymbols(MyValue: Boolean);
    {$IFDEF VISUAL_BUILD}
    procedure ProcessInputOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    {$ENDIF}
    property OpenInSDE: Boolean read FOpenInSDE write SetOpenInSDE;
end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}MD_MegaMain,{$ENDIF}
  Dialogs, ProcessInputData;

constructor TDataFileInfo.Create(MyFileName: String);
begin
  NotifyEvent := nil;
  FFileName := MyFileName;
  FDataType := snNoToken;
  FDataFormat := snNoToken;
  FDataFileFormat := dfUnknown;
  FMissingBaseSymbol := #0;
  FIdenticalBaseSymbol := #0;
  FGapSymbol := #0;
end;

procedure TDataFileInfo.Assign(Source: TDataFileInfo);
begin
  FDataType := Source.FDataType;
  FContainsCodingNuc := Source.FContainsCodingNuc;
  FDataFormat := Source.FDataFormat;
  FDataFileFormat := Source.FDataFileFormat;
  FMissingBaseSymbol := Source.FMissingBaseSymbol;
  FIdenticalBaseSymbol := Source.FIdenticalBaseSymbol;
  FGapSymbol := Source.FGapSymbol;
  FFileName := Source.FFileName;
  FOpenInSDE := Source.FOpenInSDE;
  FUserSpecifiedSymbols := Source.FUserSpecifiedSymbols;
end;

function TDataFileInfo.GetDataType:TSnTokenCode;
begin
  Result := FDataType;
end;

function TDataFileInfo.GetContainsCodingNuc: Boolean;
begin
  Result := FContainsCodingNuc;
end;

function TDataFileInfo.GetDataFormat: TsnTokenCode;
begin
  Result := FDataFormat;
end;

function TDataFileInfo.GetDataFileFormat: TDataFileFormat;
begin
  InferDataFileFormat;
  Result := FDataFileFormat;
end;

function TDataFileInfo.GetMissingBaseSymbol: AnsiChar;
begin
  Result := FMissingBaseSymbol;
end;

function TDataFileInfo.GetIdenticalBaseChar: AnsiChar;
begin
  Result := FIdenticalBaseSymbol;
end;

function TDataFileInfo.GetGapSymbol: AnsiChar;
begin
  Result := FGapSymbol;
end;

function TDataFileInfo.GetFileName: String;
begin
  Result := FFileName;
end;

function TDataFileInfo.IsUserSpecifiedSymbols: Boolean;
begin
  Result := FUserSpecifiedSymbols;
end;

procedure TDataFileInfo.SetDataType(MyDataType: TSnTokenCode);
begin
  FDataType := MyDataType;
end;

procedure TDataFileInfo.SetContainsCodingNuc(isCoding: Boolean);
begin
  FContainsCodingNuc := isCoding;
end;

procedure TDataFileInfo.SetDataFormat(MyDataFormat: TSnTokenCode);
begin
  FDataFormat := MyDataFormat;
end;

procedure TDataFileInfo.SetDataFileFormat(MyDataFileFormat: TDataFileFormat);
begin
  FDataFileFormat := MyDataFileFormat;
end;

procedure TDataFileInfo.SetMissingBaseSymbol(MyMissingBaseSymbol: AnsiChar);
begin
  FMissingBaseSymbol := MyMissingBaseSymbol;
  {$IFNDEF VISUAL_BUILD} {in MEGA-CC the user can override it}
  if D_MegaMain.DataInfoGridHasInfo(MissingBaseSymbolStr) then
  begin
    FMissingBaseSymbol := D_MegaMain.DataInfoGridGetInfo(MissingBaseSymbolStr)[1];
    FUserSpecifiedSymbols := True;
  end;
  {$ENDIF}
end;

procedure TDataFileInfo.SetIdenticalBaseSymbol(MyIdenticalBaseSymbol: AnsiChar);
begin
  FIdenticalBaseSymbol := MyIdenticalBaseSymbol;
  {$IFNDEF VISUAL_BUILD} {in MEGA-CC the user can override it}
  if D_MegaMain.DataInfoGridHasInfo(IdenticalBaseSymbolStr) then
  begin
    FIdenticalBaseSymbol := D_MegaMain.DataInfoGridGetInfo(IdenticalBaseSymbolStr)[1];
    FUserSpecifiedSymbols := True;
  end;
  {$ENDIF}
end;

procedure TDataFileInfo.SetGapSymbol(MyGapSymbol: AnsiChar);
begin
  FGapSymbol := MyGapSymbol;
  {$IFNDEF VISUAL_BUILD} {in MEGA-CC the user can override it}
  if D_MegaMain.DataInfoGridHasInfo(GapSymbolStr) then
  begin
    FGapSymbol := D_MegaMain.DataInfoGridGetInfo(GapSymbolStr)[1];
    FUserSpecifiedSymbols := True;
  end;
  {$ENDIF}
end;

procedure TDataFileInfo.SetFileName(MyFileName: String);
begin
  FFileName := MyFileName;
end;

procedure TDataFileInfo.SetUserSpecifiedSymbols(MyValue: Boolean);
begin
  FUserSpecifiedSymbols := MyValue;
end;

procedure TDataFileInfo.InferDataFileFormat;
var
  MyFileExtension: String;
begin
  MyFileExtension := lowercase(ExtractFileExt(FFileName));

  if (MyFileExtension = MasExt1) or (MyFileExtension = MasExt2) then
    FDataFileFormat := dfMas
  else if (MyFileExtension = MtsExt1) or (MyFileExtension = MtsExt2) then
    FDataFileFormat := dfMts
  else if MyFileExtension = MegaExt2 then
    FDataFileFormat := dfMdsx
  else if pos(MyFileExtension, NewickExts) > 0 then
    FDataFileFormat := dfNewick
  else if pos(MyFileExtension, FastaExts) > 0 then
    FDataFileFormat := dfFasta
  else if (MyFileExtension = MegaExt1) or (MyFileExtension = MegaExt3) then
    FDataFileFormat := dfMeg
  else if pos(MyFileExtension, PhylipExts) > 0 then
    FDataFileFormat := dfPhylip
  else if pos(MyFileExtension, PaupExts) > 0 then
    FDataFileFormat := dfNexus
  else if MyFileExtension = BlastExt1 then
    FDataFileFormat := dfBlast
  else if pos(MyFileExtension, ClustalExts) > 0 then
    FDataFileFormat := dfClustal
  else if pos(MyFileExtension, GCGExts) > 0 then
    FDataFileFormat := dfGcg
  else if pos(MyFileExtension, PIRExts) > 0 then
    FDataFileFormat := dfPir
  else if pos(MyFileExtension, NbrfExts) > 0 then
    FDataFileFormat:= dfNbrf
  else if MyFileExtension = AnalysisOptExt then
    FDataFileFormat := dfMao
  else if pos(MyFileExtension, MsfExts) > 0 then
    FDataFileFormat := dfMsf
  else if pos(MyFileExtension, IgExts) > 0 then
    FDataFileFormat := dfIg
  else if pos(MyFileExtension, ABIExts) > 0 then
    FDataFileFormat := dfAbi
  else if pos(MyFileExtension, StadenExts) > 0 then
    FDataFileFormat := dfStaden
  else if pos(MyFileExtension, XMLExts) > 0 then
    FDataFileFormat := dfXml
  else if pos(MyFileExtension, TextExts) > 0 then
    FDataFileFormat := dfText
  else if pos(MyFileExtension, HTMLExts) > 0 then
    FDataFileFormat := dfHtml
  else
    FDataFileFormat := dfUnknown;


end;

procedure TDataFileInfo.SetOpenInSDE(AValue: Boolean);
begin
  if FOpenInSDE=AValue then Exit;
  FOpenInSDE:=AValue;
end;

{$IFDEF VISUAL_BUILD}
procedure TDataFileInfo.ProcessInputOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
var
  temp: String;
begin
  try
    HtmlOptionsDialog.Hide;
    temp := message.ArgumentList.GetString(0);
    if Pos(RENDER_PROCESS_ERROR, temp) > 0 then
      raise Exception.Create(temp);
    temp := message.ArgumentList.GetString(NUCLEOTIDE_SEQUENCE_INDEX);
    if SameText(temp, 'true') then
    begin
      SetDataType(snNucleotide);
    end;
    temp := message.ArgumentList.GetString(PROTEIN_SEQUENCE_INDEX);
    if SameText(temp, 'true') then
    begin
      SetDataType(snProtein);
    end;
    temp := message.ArgumentList.GetString(PAIRWISE_DISTANCE_INDEX);
    if SameText(temp, 'true') then
    begin
      SetDataType(snDistance);
    end;
    case GetDataType of
      snNucleotide:
        begin
          temp := message.ArgumentList.GetString(DNA_MISSING_DATA_INDEX);
          SetMissingBaseSymbol(temp[1]);
          temp := message.ArgumentList.GetString(DNA_ALIGNMENT_GAP_INDEX);
          SetGapSymbol(temp[1]);
          temp := message.ArgumentList.GetString(DNA_IDENTICAL_SYMBOL_INDEX);
          SetIdenticalBaseSymbol(temp[1]);
          if GetDataFileFormat = dfFasta then
          begin
            if not ReadFastaFile(Self, True) then
              raise Exception.Create('Failed to read fasta alignment file');
          end;
        end;
      snProtein:
        begin
          temp := message.ArgumentList.GetString(PROTEIN_MISSING_DATA_INDEX);
          SetMissingBaseSymbol(temp[1]);
          temp := message.ArgumentList.GetString(PROTEIN_ALIGNMENT_GAP_INDEX);
          SetGapSymbol(temp[1]);
          temp := message.ArgumentList.GetString(PROTEIN_IDENTICAL_SYMBOL_INDEX);
          SetIdenticalBaseSymbol(temp[1]);
          if GetDataFileFormat = dfFasta then
          begin
            if not ReadFastaFile(Self, True) then
              raise Exception.Create('Failed to read fasta alignment file');
          end
          else if not ReadSequenceFile(FFileName, True, GetDataType, GetDataFormat, FMissingBaseSymbol, FIdenticalBaseSymbol, GetGapSymbol, OpenInSDE) then
              raise Exception.Create('Failed to read alignment file');
        end;
      snDistance:
        begin
          temp := message.ArgumentList.GetString(PAIRWISE_MISSING_DATA_INDEX);
          SetMissingBaseSymbol(temp[1]);
          temp := message.ArgumentList.GetString(LOWER_LEFT_MATRIX_INDEX);
          temp := message.ArgumentList.GetString(UPPER_RIGHT_MATRIX_INDEX);
          if not ReadDistFile(FFileName, True, GetDataType, GetDataFormat) then
            raise Exception.Create('Failed to parse distance data file');
        end
        else
          Raise Exception.Create('MEGA failed to parse the input data file because it did not determine the data type correctly');
    end;
    Result := True;
    SetUserSpecifiedSymbols(true);
    if Assigned(NotifyEvent) then
      NotifyEvent(nil);
  except
    on E: Exception do
      ShowMessage('Oh no! MEGA failed to parse the input file: ' + E.Message);
  end;
end;
{$ENDIF}
end.
