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

unit mmegaalignmentfileloader;
{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
interface

uses
  {$IFDEF VISUAL_BUILD}
  cef3types, cef3intf,
  {$ENDIF}
  Classes, SysUtils, malignmentfileloader, MLexSeq, MDataFileInfo;

type

  { TMegaAlignmentFileLoader }

  TMegaAlignmentFileLoader = class(TAlignmentFileLoader)
    private
      FExpectedFileInfo: TDataFileInfo;
      FParser: TLexSeq;
      function ReadHeader: Boolean;
      function ReadData: Boolean;
      function GetDataFileInfo: Boolean;
      function DoGetDataFileInfo: Boolean;
      procedure PromptUserForInfo;
      {$IFDEF VISUAL_BUILD}
      procedure ProcessInputOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
      {$ENDIF}
      procedure InitParser;
      procedure TransferDataFileInfoToParser;
      procedure UpdateDataTitle;
      function DoLoadFile: Boolean;
    protected
      procedure InitInputSeqData; override;
      procedure InitOtuInfos; override;
    public
      OnLoadFileDone: TNotifyEvent;
      constructor Create(aFilename: String; aShowProgress: Boolean=True);
      destructor Destroy; override;
      procedure UpdateExpectedFileInfo(aInfo: TDataFileInfo);
      function LoadFile: Boolean; override;
      function TryGetSpecialChars(var gap: Char; var ident: Char; var miss: Char): Boolean; override;
  end;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  Mega_Main, htmloptionsdlg, mbrowserutils,
  {$ELSE}
  MD_MegaMain, MProcessPack, MegaUtils_NV, StringUtils,
  {$ENDIF}
  ProcessInputData, MegaErrUtils, MegaPrivateFiles, MOtuInfo,
  KeywordConsts, MegaUtils, MEditorForm, MegaConsts, math,
  ContextHelp_HC, Dialogs;

{ TMegaAlignmentFileLoader }

constructor TMegaAlignmentFileLoader.Create(aFilename: String; aShowProgress: Boolean=True);
begin
  inherited Create(aFilename, aShowProgress);
  FParser := nil;
  OnLoadFileDone := nil;
  FExpectedFileInfo := nil;
end;

destructor TMegaAlignmentFileLoader.Destroy;
begin
  if Assigned(FParser) then
    FParser.Free;
  if Assigned(FExpectedFileInfo) then
    FExpectedFileInfo.Free;
  inherited Destroy;
end;

procedure TMegaAlignmentFileLoader.UpdateExpectedFileInfo(aInfo: TDataFileInfo);
begin
  if not Assigned(FExpectedFileInfo) then
    FExpectedFileInfo := TDataFileInfo.Create(aInfo.GetFileName );
  FExpectedFileInfo.Assign(aInfo);
end;

function TMegaAlignmentFileLoader.GetDataFileInfo: Boolean;
begin
  Result := False;
  try
    if not DoGetDataFileInfo then
      Exit;
    Result := True;
  Except
    on E:EAbort do
      Exit;
    on E:EParserError do
      Exit; // in this case, we already informed the user and opened the input data file in the text editor
    on E:Exception do
    begin
      ShowErrorMessage(E);
    end;
  end;
end;

function TMegaAlignmentFileLoader.DoGetDataFileInfo: Boolean;
var
  {$IFNDEF VISUAL_BUILD}
  ProcessPack: TProcessPack = nil;
  {$ENDIF}
  MyDataFileFormat: TDataFileFormat;
  ADataType: String;
  AGapSym: String;
  AMissingSym: String;
  AIdentSym: String;
  AFoundFormat: Boolean = False;
  ContainsCoding: String;
  aList: TStringList;
  SymbolStr: String;
begin
  {$IFDEF VISUAL_BUILD}
  try
    Result := False;
    MyDataFileFormat := FDataFileInfo.GetDataFileFormat;

    if MyDataFileFormat = dfMeg then
    begin
      ReadFormatValsFromMegaSeqFile(FFileName, ADataType, AIdentSym, AMissingSym, AGapSym, AFoundFormat);
      if AFoundFormat then
      begin
        FDataFileInfo.SetDataType(MapDataTypeStringToTokenCode(ADataType));
        FDataFileInfo.SetMissingBaseSymbol(AnsiChar(AMissingSym[1]));
        FDataFileInfo.SetIdenticalBaseSymbol(AnsiChar(AIdentSym[1]));
        FDataFileInfo.SetGapSymbol(AnsiChar(AGapSym[1]));
        FDataFileInfo.SetUserSpecifiedSymbols(false);
        Result := True;
        Exit;
      end;
    end
    else if (MyDataFileFormat = dfFasta) or (MyDataFileFormat = dfNexus) then
    begin
      Result := False;
    end
    else
      raise EInvalidArgument.Create('Invalid data file format specified');
    if not FIsConcatenatingFiles then
      PromptUserForInfo;
  except
    on E:EAbort do
    begin
      Result := False;
      raise EAbort.Create(E.Message);
    end;

    on E:EInvalidArgument do
    begin
      Result := False;
      raise Exception.Create(E.Message);
    end;

    on E:Exception do
    begin
      Result := False;
      raise Exception.Create(E.Message);
    end;
  end;
  {$ELSE}
  try
  Result := False;
  MyDataFileFormat := FDataFileInfo.GetDataFileFormat;

  if MyDataFileFormat = dfMeg then
  begin
    ReadFormatValsFromMegaSeqFile(FFileName, ADataType, AIdentSym, AMissingSym, AGapSym, AFoundFormat);
    if not AFoundFormat then
    begin
      if not PromptUserForDataFileInfo(FDataFileInfo) then
        raise EAbort.Create('User aborted processing of input data');
      ProcessPack := D_MegaMain.ProcessPack;
      FDataFileInfo.SetDataType(MapDataTypeStringToTokenCode(ProcessPack.TextualSettingsList.Values[DataTypeStr]));
      FDataFileInfo.SetMissingBaseSymbol(AnsiChar(ProcessPack.TextualSettingsList.Values[MissingBaseSymbolStr][1]));
      FDataFileInfo.SetIdenticalBaseSymbol(AnsiChar(ProcessPack.TextualSettingsList.Values[IdenticalBaseSymbolStr][1]));
      FDataFileInfo.SetGapSymbol(AnsiChar(ProcessPack.TextualSettingsList.Values[GapSymbolStr][1]));
      ContainsCoding := ProcessPack.TextualSettingsList.Values[ContainsCodingNucStr];
      FDataFileInfo.SetContainsCodingNuc(SameText(containsCoding, 'True'));
      FDataFileInfo.SetUserSpecifiedSymbols(true);
      D_MegaMain.FDataType := FDataFileInfo.GetDataType;
    end
    else
    begin
      FDataFileInfo.SetDataType(MapDataTypeStringToTokenCode(ADataType));
      FDataFileInfo.SetMissingBaseSymbol(AnsiChar(AMissingSym[1]));
      FDataFileInfo.SetIdenticalBaseSymbol(AnsiChar(AIdentSym[1]));
      FDataFileInfo.SetGapSymbol(AnsiChar(AGapSym[1]));
      FDataFileInfo.SetUserSpecifiedSymbols(false);
      {$IFNDEF VISUAL_BUILD}
      if not ContainsText(ADataType, 'distance') then
      begin
        // In this case, we need to make sure that the information in the MEGA file is consistent
        // with the information in the analysis options file. If not, we default to
        // the information in the MEGA file and let the user know about it
        ProcessPack := D_MegaMain.ProcessPack;
        if (ProcessPack.TextualSettingsList.Values[MissingBaseSymbolStr][1] <> String(FDataFileInfo.GetMissingBaseSymbol)) then
          Warn_NV('Missing base symbol specified in analysis options file does not match the missing base symbol indicated in the MEGA file. Using the symbol indicated in the MEGA file: ' + FDataFileInfo.GetMissingBaseSymbol);

        if (ProcessPack.TextualSettingsList.Values[IdenticalBaseSymbolStr][1] <> String(FDataFileInfo.GetIdenticalBaseChar)) then
          Warn_NV('Identical base symbol specified in analysis options file does not match the identical base symbol indicated in the MEGA file. Using the symbol indicated in the MEGA file: ' + FDataFileInfo.GetIdenticalBaseChar);

        if (ProcessPack.TextualSettingsList.Values[GapSymbolStr][1] <> String(FDataFileInfo.GetGapSymbol)) then
          Warn_NV('Gap symbol specified in analysis options file does not match the Gap base symbol indicated in the MEGA file. Using the symbol indicated in the MEGA file: ' + FDataFileInfo.GetGapSymbol);
      end;
      {$ENDIF}
    end;
  end
  else if (MyDataFileFormat = dfFasta) or (MyDataFileFormat = dfNexus) then
  begin
    {$IFDEF VISUAL_BUILD}
    if MyDataFileFormat = dfFasta then
      if not PromptUserForDataFileInfo(Result) then
        raise EAbort.Create('User aborted processing of input data');
    {$ELSE}
      ProcessPack := D_MegaMain.ProcessPack;
      FDataFileInfo.SetDataType(MapDataTypeStringToTokenCode(ProcessPack.TextualSettingsList.Values[DataTypeStr]));
      AList := ProcessPack.TextualSettingsList;
      if Alist.IndexOfName(MissingBaseSymbolStr2) >= 0 then
        SymbolStr := AList.Values[MissingBaseSymbolStr2]
      else
        SymbolStr := AList.Values[MissingBaseSymbolStr];
      FDataFileInfo.SetMissingBaseSymbol(AnsiChar(SymbolStr[1]));

      if Alist.IndexOfName(IdenticalBaseSymbolStr2) >= 0 then
        SymbolStr := AList.Values[IdenticalBaseSymbolStr2]
      else
        SymbolStr := AList.Values[IdenticalBaseSymbolStr];
      FDataFileInfo.SetIdenticalBaseSymbol(AnsiChar(SymbolStr[1]));
      if Alist.IndexOfName(GapSymbolStr2) >= 0 then
        SymbolStr := AList.Values[GapSymbolStr2]
      else
        SymbolStr := AList.Values[GapSymbolStr];
      FDataFileInfo.SetGapSymbol(AnsiChar(SymbolStr[1]));
      ContainsCoding := ProcessPack.TextualSettingsList.Values[ContainsCodingNucStr];
      FDataFileInfo.SetContainsCodingNuc(SameText(containsCoding, 'True'));
      FDataFileInfo.SetUserSpecifiedSymbols(true);
      D_MegaMain.FDataType := FDataFileInfo.GetDataType;
    {$ENDIF}
  end
  else
    raise EInvalidArgument.Create('Invalid data file format specified');
  Result := True;
  {$IFNDEF VISUAL_BUILD}
   D_MegaMain.FDataType := FDataFileInfo.GetDataType;
  {$ENDIF}
  except
    on E:EAbort do
    begin
      FreeAndNil(FDataFileInfo);
      raise EAbort.Create(E.Message);
    end;

    on E:EInvalidArgument do
    begin
      FreeAndNil(FDataFileInfo);
      raise Exception.Create(E.Message);
    end;

    on E:Exception do
    begin
      FreeAndNil(FDataFileInfo);
      raise Exception.Create(E.Message);
    end;
  end;
  {$ENDIF}
end;

procedure TMegaAlignmentFileLoader.PromptUserForInfo;
var
  {$IFDEF VISUAL_BUILD}
  js: String;
  {$ELSE}
  ProcessPack: TProcessPack;
  ContainsCoding: String;
  {$ENDIF}
begin
  {$IFDEF VISUAL_BUILD}
  try
    js := EmptyStr;
    HtmlOptionsDialog.Show;
    HtmlOptionsDialog.ProcessMessageReceivedProc := ProcessInputOptionsMessage;
    HtmlOptionsDialog.DomProcedureName := VISITDOMPROC_INPUT_DATA_OPTIONS;
    HtmlOptionsDialog.LoadOptionsFile(wofInputDataDlg, js, 'Input Data Options', 385, 220, HC_Input_Data_Format);
  except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when setting up the input data options');
  end;
  {$ELSE}
  ProcessPack := D_MegaMain.ProcessPack;
  if not Assigned(FDataFileInfo) then
    FDataFileInfo := TDataFileInfo.Create(FFilename);
  FDataFileInfo.SetDataType(MapDataTypeStringToTokenCode(ProcessPack.TextualSettingsList.Values[DataTypeStr]));
  FDataFileInfo.SetMissingBaseSymbol(AnsiChar(ProcessPack.TextualSettingsList.Values[MissingBaseSymbolStr][1]));
  FDataFileInfo.SetIdenticalBaseSymbol(AnsiChar(ProcessPack.TextualSettingsList.Values[IdenticalBaseSymbolStr][1]));
  FDataFileInfo.SetGapSymbol(AnsiChar(ProcessPack.TextualSettingsList.Values[GapSymbolStr][1]));
  ContainsCoding := ProcessPack.TextualSettingsList.Values[ContainsCodingNucStr];
  FDataFileInfo.SetContainsCodingNuc(SameText(containsCoding, 'True'));
  FDataFileInfo.SetUserSpecifiedSymbols(true);
  D_MegaMain.FDataType := FDataFileInfo.GetDataType;
  {$ENDIF}
end;

{$IFDEF VISUAL_BUILD}
procedure TMegaAlignmentFileLoader.ProcessInputOptionsMessage( const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
var
  temp: String;
  fileExt: String;
begin
  try
    HtmlOptionsDialog.Hide;
    temp := message.ArgumentList.GetString(0);
    if Pos(RENDER_PROCESS_ERROR, temp) > 0 then
      raise Exception.Create(temp);
  except
    on E: Exception do
      ShowMessage('Oh no! MEGA has encountered an error: ' + E.Message);
  end;

  temp := message.ArgumentList.GetString(NUCLEOTIDE_SEQUENCE_INDEX);
  if SameText(temp, 'true') then
  begin
    FDataFileInfo.SetDataType(snNucleotide);
    InputDataOptionsFiletype := '#nucleotide_sequence_li a';
  end;
  temp := message.ArgumentList.GetString(PROTEIN_SEQUENCE_INDEX);
  if SameText(temp, 'true') then
  begin
    FDataFileInfo.SetDataType(snProtein);
    InputDataOptionsFiletype := '#protein_sequence_li a';
  end;
  temp := message.ArgumentList.GetString(PAIRWISE_DISTANCE_INDEX);
  if SameText(temp, 'true') then
  begin
    FDataFileInfo.SetDataType(snDistance);
    InputDataOptionsFiletype := '#pairwise_distance_li a';
  end;
  case FDataFileInfo.GetDataType of
    snNucleotide:
      begin
        temp := message.ArgumentList.GetString(DNA_MISSING_DATA_INDEX);
        FDataFileInfo.SetMissingBaseSymbol(temp[1]);
        temp := message.ArgumentList.GetString(DNA_ALIGNMENT_GAP_INDEX);
        FDataFileInfo.SetGapSymbol(temp[1]);
        temp := message.ArgumentList.GetString(DNA_IDENTICAL_SYMBOL_INDEX);
        FDataFileInfo.SetIdenticalBaseSymbol(temp[1]);
      end;
    snProtein:
      begin
        temp := message.ArgumentList.GetString(PROTEIN_MISSING_DATA_INDEX);
        FDataFileInfo.SetMissingBaseSymbol(temp[1]);
        temp := message.ArgumentList.GetString(PROTEIN_ALIGNMENT_GAP_INDEX);
        FDataFileInfo.SetGapSymbol(temp[1]);
        temp := message.ArgumentList.GetString(PROTEIN_IDENTICAL_SYMBOL_INDEX);
        FDataFileInfo.SetIdenticalBaseSymbol(temp[1]);
      end;
      else
      begin
      {$IFDEF VISUAL_BUILD}
      Raise Exception.Create('MEGA failed to parse the input data file because it did not determine the data type correctly');
      {$ELSE}
      Error_NV('MEGA failed to parse the input data file because it did not determine the data type correctly');
      {$ENDIF}
      end;
  end;
  FDataFileInfo.SetUserSpecifiedSymbols(true);
  DoLoadFile;
  if Assigned(OnLoadFileDone) then
    OnLoadFileDone(Self);
end;
{$ENDIF}
procedure TMegaAlignmentFileLoader.InitInputSeqData;
begin
  try
    FInputSeqData.SourceFileName := FFileName;
    if FShowProgress then
    begin
      FProgress.UpdateRunStatusInfo('Status', 'Setting up Sequence/group names');
      FProgress.UpdatePercentProgress(0);
    end;

    InitOtuInfos;

    if FShowProgress then
    begin
      FProgress.UpdateRunStatusInfo('Status', 'Organizing sequence information');
      ProcessMessages;
      {$IFNDEF VISUAL_BUILD}
      WriteLn();
      {$ENDIF}
    end;
    FParser.ConstructAllSiteDomainMark;
    FInputSeqData.IsNuc := (FParser.DataType = snNucleotide);
    FInputSeqData.IsAmino := (FParser.DataType = snProtein);
    FInputSeqData.GapSym    := FParser.GapSym;
    FInputSeqData.MissSym   := FParser.MissingSym;
    FInputSeqData.IdenSym   := FParser.IdenticalSym;
    FInputSeqData.NoOfTaxa  := FParser.NoOfOtus;
    FInputSeqData.NoOfSites := FParser.NoOfSites;
    FInputSeqData.IsCoding := FParser.AllSiteDomainMark.IsCoding;
    FInputSeqData.OtuInfos  := FAllOtuInfo;
    {$IFDEF VISUAL_BUILD}
    MegaForm.UpdateMainWindow(FParser.FileName, FParser.Title, FParser.Description, FParser.FDataType);
    {$ENDIF}
  except
    On E: Exception do
    begin
      ShowErrorMessage(E);
      {$IFDEF VISUAL_BUILD}
      MegaForm.Update;
      {$ENDIF}
    end;
  end;
end;

procedure TMegaAlignmentFileLoader.InitOtuInfos;
var
  i: Integer;
  AInfo: TOtuInfo;
  CurSeq: PAnsiChar;
begin
  FAllOtuInfo := TAllOtuInfo.Create;
  FAllOtuInfo.NoOfOtus := FParser.NoOfOtus;
  for i:=0 to FParser.NoOfOtus - 1 do
  begin
    AInfo := TOtuInfo.Create;
    AInfo.Id := i;
    AInfo.Name := FParser.Taxon[i];
    AInfo.GpName := FParser.GpName[i];
    AInfo.SpName := FParser.SpName[i];
    AInfo.PopName := FParser.PopName[i];
    AInfo.OutgroupMember := FParser.IsOutgroupMember[i];
    AInfo.IsUsed := True;
    AInfo.AssignGeographicalInfo(FParser.GeographicalInfo[i]);
    CurSeq := FParser.Sequence[i];
    if FShowProgress then
    begin
      FProgress.UpdatePercentProgress((i+1)*100 div FParser.NoOfOtus);
      ProcessMessages;
    end;
    AInfo.Data := CurSeq;
    FAllOtuInfo[i] := AInfo;
  end;
end;

procedure TMegaAlignmentFileLoader.InitParser;
begin
  FParser := TLexSeq.Create;
  FParser.SetKeywords(GetPrivateFile(mfKeywordsFile));
  FParser.SetDataFile(FFilename);
  if ShowProgress then
  begin
    FParser.SetRuntimeProgress(FProgress);
    {$IFDEF VISUAL_BUILD}
    FParser.SetStopButton(FProgress.StopBtn);
    {$ENDIF}
  end;
end;

function TMegaAlignmentFileLoader.LoadFile: Boolean;
var
  aCol, aRow: Integer;
begin
  Result := False;
  FDataFileInfo := TDataFileInfo.Create(FFilename);
  if not GetDataFileInfo then
  begin
    if FIsConcatenatingFiles then
      FDataFileInfo.Assign(FExpectedFileInfo) { fallback to info for first found file}
    else
      Exit;
  end;

  try
    Result := DoLoadFile;
  except
    On E: Exception do
    begin
      FLog.Add(E.Message);
      aRow := FParser.Row;
      aCol := FParser.Col;
      FreeAndNil(FParser);
      {$IFDEF VISUAL_BUILD}
      if not FIsConcatenatingFiles then
      begin
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

function TMegaAlignmentFileLoader.TryGetSpecialChars(var gap: Char; var ident: Char; var miss: Char): Boolean;
begin
  if Assigned(FInputSeqData) then
  begin
    Result := True;
    gap := FInputSeqData.GapSym;
    miss := FInputSeqData.MissSym;
    ident := FInputSeqData.IdenSym;
  end;
end;

function TMegaAlignmentFileLoader.ReadData: Boolean;
begin
  Result := FParser.ReadData;
  if not Result then
    FLog.Add('failed to read data');
  if FShowProgress then
  begin
    FProgress.UpdateRunStatusInfo('Status', 'Storing information');
    ProcessMessages;
  end;
end;

function TMegaAlignmentFileLoader.ReadHeader: Boolean;
begin
  Result := FParser.ReadHeader;
  if Result then
  begin
    UpdateDataTitle;
    TransferDataFileInfoToParser;
  end
  else
    FLog.Add('failed to read header');
  if FShowProgress then
  begin
    FProgress.UpdateRunStatusInfo('Status', 'Reading sequence data');
    ProcessMessages;
  end;
end;

procedure TMegaAlignmentFileLoader.TransferDataFileInfoToParser;
begin
  {$IFDEF VISUAL_BUILD}
  if (not FParser.FoundFormat) then
  begin
    FParser.DataType := FDataFileInfo.GetDataType;
    FParser.DataFormat := FDataFileInfo.GetDataFormat;
    FParser.IdenticalSym := FDataFileInfo.GetIdenticalBaseChar;
    FParser.GapSym := FDataFileInfo.GetGapSymbol;
    FParser.MissingSym := FDataFileInfo.GetMissingBaseSymbol;
  end;
  {$ELSE}
  if FParser.FoundFormat then
  begin
    // If the file doesn't specify a format, then take the one from the MAO file.
    if FParser.DataType = snNoToken then
      FParser.DataType := FDataFileInfo.GetDataType;
    if FParser.DataFormat = snNoToken then
      FParser.DataFormat := FDataFileInfo.GetDataFormat;

    if FParser.IdenticalSym <> FDataFileInfo.GetIdenticalBaseChar then
      Warn_nv('Warning: identical base symbol mismatch. MEGA Analysis Options file lists identical base symbol as ''' + FDataFileInfo.GetIdenticalBaseChar + ''', while the input data file lists it as''' + FParser.IdenticalSym + '''. Using ''' + FParser.IdenticalSym + ''' to identify identical bases.');
    if FParser.GapSym <> FDataFileInfo.GetGapSymbol then
      Warn_nv('Warning: gap symbol mismatch. MEGA Analysis Options file lists gap symbol as ''' + FDataFileInfo.GetGapSymbol + ''', while the input data file lists it as''' + FParser.GapSym + '''. Using ''' + FParser.GapSym + ''' to identify gaps.');
    if FParser.MissingSym <> FDataFileInfo.GetMissingBaseSymbol then
      Warn_nv('Warning: missing symbol mismatch. MEGA Analysis Options file lists missing base symbol as ''' + FDataFileInfo.GetMissingBaseSymbol + ''', while the input data file lists it as''' + FParser.MissingSym + '''. Using ''' + FParser.MissingSym + ''' to identify missing symbols.');
  end
  else
  begin
    FParser.DataType := FDataFileInfo.GetDataType;
    FParser.DataFormat := FDataFileInfo.GetDataFormat;
    FParser.IdenticalSym := FDataFileInfo.GetIdenticalBaseChar;
    FParser.GapSym := FDataFileInfo.GetGapSymbol;
    FParser.MissingSym := FDataFileInfo.GetMissingBaseSymbol;
  end;
  {$ENDIF}
end;

procedure TMegaAlignmentFileLoader.UpdateDataTitle;
begin
  {$IFNDEF VISUAL_BUILD}
  D_MegaMain.DataTitle := FParser.Title;
  {$ENDIF}
end;

function TMegaAlignmentFileLoader.DoLoadFile: Boolean;
begin
  Result := False;
  if FShowProgress then
    InitProgress;
  InitParser;
  Result := ReadHeader;
  if Result then
  begin
    Result := ReadData;
    if Result then
    begin
      InitInputSeqData;
      FSequenceList := FInputSeqData.GenerateTSequenceList;
    end;
  end;
  if FShowProgress then
    FProgress.Hide;
end;

end.
