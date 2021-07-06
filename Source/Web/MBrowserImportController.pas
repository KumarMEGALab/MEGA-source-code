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

unit MBrowserImportController;
  {$IFDEF FPC}
  {$MODE Delphi}
  {$ENDIF}
interface

uses
  LCLType, LCLIntF, Classes, Controls, StdCtrls, SysUtils, MAlignGrid, KeywordConsts, MAlignEditMainForm, MD_Sequences,
  Dialogs, FileUtil, Forms, Graphics, MegaPrivateFiles, cef3types, MegaConsts,
  cef3intf, htmloptionsdlg, mbrowserutils, AlnBuilder_HC, mrenderprocesshandler;


type
  {
    Handles the parsing of sequence data retrieved from the browser as well as create of TSequence
    objects and exporting the TSequence objects to MAlignEditMainForm.
  }

  { TBrowserImportController }

  TBrowserImportController = class
  private
    FImportOnlyCDSs: Boolean;
    FRawSequenceData: TStringList;
    FSequenceList: TSequenceList;
    FSequenceFactory: TSequenceFactory;
    FastaValidationWasAttempted: boolean;
    GenBankValidationWasAttempted: boolean;
    UseInitial: String;
    FDataType: TsnTokenCode;
    FWebDataFormat: TWebSeqDataFormat;
    FGenbankValidRegions: TList;
    FCDSs: TCDSArrayArray;
    tempCDSs: TCDSArrayArray;
    function FindGenbankValidRegions: Boolean;
    procedure ClearGenbankValidRegions;
    procedure DetermineDataType(ValidData: TStringList; IsFasta: Boolean);
    function BuildSequenceListFromFasta(ValidData: TStringList): TSequenceList;
    function GetSequenceList: TSequenceList;
    function GetWebDataFormat: TWebSeqDataFormat;
    procedure SetRawSequenceData(AValue: TStringList);
    procedure SetSequenceList(AValue: TSequenceList);
    function ValidateFastaData(RawData: TStringList): boolean; overload;
    function ValidateFastaData(RawSeqId: String; RawSeqData: String): boolean; overload;
    function ValidateGenBankData(RawData: TStringList): boolean;
    function MapNameFieldToValue(ASequence: TSequence; AField: String): String;
    procedure FindCDSs;
    procedure ProcessJSMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure DisplayJsMessageDialog(jsMessage: String);
  public
    constructor Create;
    destructor Destroy; override;
    function ValidateData(RawData: TStringlist): boolean;
    function ValidateMessyFastaData(RawData: TStringList): boolean; // Detects attempted FASTA import, with not clean FASTA. eg. NCBI "FASTA" instead of "FASTA (text)"
    procedure ExportFastaToAlignEditor(ValidData: TStringList);
    procedure ExportToAlignEditor;
    procedure ProcessSeqNameOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    function GetFirstGenbankSequence: TSequence;
    function NumValidGenbankResults: Integer;
    function NumCDSs(region: Integer): Integer;
    function HasCDSs: Boolean;
    function GetCDSs: TCDSArrayArray;
    function BuildSequenceListFromGenBank: TSequenceList; overload;
    procedure UpdateIncludedCDSs(CDSsToInclude: TBoolArrayArray);
    property SequenceList: TSequenceList read GetSequenceList write SetSequenceList;
    property RawSequenceData: TStringList read FRawSequenceData write SetRawSequenceData;
    property WebDataFormat: TWebSeqDataFormat read GetWebDataFormat;
    property DataType: TSnTokenCode read FDataType;
    property ImportOnlyCDSs: Boolean read FImportOnlyCDSs write FImportOnlyCDSs;
end;

implementation

uses
  StringUtils;

constructor TBrowserImportController.Create;
begin
  FSequenceList := nil;
  FSequenceFactory := TSequenceFactory.Create;
  FastaValidationWasAttempted := false;
  GenBankValidationWasAttempted := false;
  FDataType := snNoToken;
  FWebDataFormat := wsdfUnknown;
  FGenbankValidRegions := nil;
  SetLength(FCDSs, 0);
  FImportOnlyCDSs := False;
end;

destructor TBrowserImportController.Destroy;
begin
  if FSequenceFactory <> nil then
    FreeAndNil(FSequenceFactory);
  if Assigned(FGenbankValidRegions) then
  begin
    ClearGenbankValidRegions;
    FGenbankValidRegions.Free;
  end;
  if Assigned(FSequenceList) then
    FSequenceList.Free;
  inherited;
end;

procedure TBrowserImportController.FindCDSs;
var
  i: Integer;
  validRegion: TStringList = nil;
begin
  if not Assigned(FRawSequenceData) then
    Exit;
  if (not Assigned(FGenbankValidRegions)) or (FGenbankValidRegions.Count = 0) then
    if not FindGenbankValidRegions then
      Exit;
  SetLength(FCDSs, FGenbankValidRegions.Count);
  for i := 0 to FGenbankValidRegions.Count - 1 do
  begin
    validRegion := TStringList(FGenbankValidRegions[i]);
    FCDSs[i] := FSequenceFactory.GetCDSsForData(validRegion);
    if Length(FCDSs[i]) > 0 then
      FCDSs[i][0].accession := FSequenceFactory.FindGenbankAccessionNum(validRegion);
  end;
end;

procedure TBrowserImportController.ProcessJSMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
begin
  if HtmlOptionsDialog.Visible then
    HtmlOptionsDialog.Hide;
end;

procedure TBrowserImportController.DisplayJsMessageDialog(jsMessage: String);
var
  js: String;
begin
  js := EmptyStr;
  HtmlOptionsDialog.HelpButtonImg.Visible:=False;
  HtmlOptionsDialog.Show;
  HtmlOptionsDialog.ProcessMessageReceivedProc := ProcessJSMessage;
  HtmlOptionsDialog.DomProcedureName := VISITDOMPROC_JS_MESSAGE_DIALOG;
  js := js + Format('$("#%s").append("%s");', [JS_MESSAGE, jsMessage]);
  HtmlOptionsDialog.LoadOptionsFile(wofJSMessageDialog, js, 'Message', 390, 200, HC_Edit_Menu_in_Alignment_Explorer);
end;

procedure TBrowserImportController.UpdateIncludedCDSs(CDSsToInclude: TBoolArrayArray);
var
  i, j: Integer;
  index: Integer = 0;
begin
  SetLength(tempCDSs, Length(CDSsToInclude));
  for i := Low(CDSsToInclude) to High(CDSsToInclude) do
  begin
    index := 0;
    if Length(CDSsToInclude[i]) > 0 then
      for j := Low(CDSsToInclude[i]) to High(CDSsToInclude[i]) do
      begin
        if CDSsToInclude[i][j] then
        begin
          if Length(tempCDSs[i]) <= index then
            SetLength(tempCDSs[i], index + 1);
          CopyCds(FCDSs[i, j], tempCDSs[i][index]);
          inc(index);
        end;
      end;
  end;
  for i := Low(FCDSs) to High(FCDSs) do
    SetLength(FCDSs[i], 0);
  SetLength(FCDSs, 0);
  FCDSs := tempCDSs;
end;

procedure TBrowserImportController.ProcessSeqNameOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
var
  temp: ustring;
   i : Integer;
  SeqNameFirst : string;
  SeqNameSecond : string;
  SeqNameThird : string;
  SeqNameFourth : string;
  SeqInfoString : string;
  SeqNameLabel : string;
  FSequence: TSequence = nil;
begin
  HtmlOptionsDialog.Hide;
  temp := message.ArgumentList.GetString(0);
  if Pos(RENDER_PROCESS_ERROR, temp) > 0 then
    raise Exception.Create(temp);

  UseInitial := message.ArgumentList.GetString(SEQNAME_USE_INITIAL_INDEX);
  SeqNameFirst := message.ArgumentList.GetString(SEQNAME_FIRST_INDEX);
  SeqNameSecond := message.ArgumentList.GetString(SEQNAME_SECOND_INDEX);
  SeqNameThird := message.ArgumentList.GetString(SEQNAME_THIRD_INDEX);
  SeqNameFourth := message.ArgumentList.GetString(SEQNAME_FOURTH_INDEX);
  SeqNameLabel := message.ArgumentList.GetString(SEQNAME_SEQ_LABEL_INDEX);

  Assert((FSequenceList <> nil) and (FSequenceList.Count > 0));
  for i := 0 to FSequenceList.Count - 1 do
  begin
    FSequence := FSequenceList[i];
    if (UseInitial = 'true') and (Trim(FSequence.Species) <> EmptyStr) then
      FSequenceList[i].Species := FSequence.Species[1] + '.' + Copy(FSequence.Species, Pos(' ', FSequence.Species), length(FSequence.Species));
    SeqInfoString := EmptyStr;
    SeqInfoString := MapNameFieldToValue(FSequence, SeqNameFirst);
    SeqInfoString := SeqInfoString + ' ' + MapNameFieldToValue(FSequence, SeqNameSecond);
    SeqInfoString := SeqInfoString + ' ' + MapNameFieldToValue(FSequence, SeqNameThird);
    SeqInfoString := SeqInfoString + ' ' + MapNameFieldToValue(FSequence, SeqNameFourth);
    SeqInfoString := Trim(SeqInfoString);
    if SeqInfoString.isEmpty then
      SeqInfoString := FSequence.SeqName;
    if (i = 0) and (SeqNameLabel <> '') and (SeqNameLabel <> SeqInfoString) then
      FSequence.SeqName := SeqNameLabel
    else
      FSequence.SeqName := SeqInfoString;
  end;
  ExportToAlignEditor;
end;

function TBrowserImportController.GetFirstGenbankSequence: TSequence;
var
  temp: TStringList = nil;
begin
  Assert(FGenbankValidRegions.Count > 0);
  temp := TStringList(FGenbankValidRegions[0]);
  Result := FSequenceFactory.GenerateGenbankSequenceObject(temp);
end;

function TBrowserImportController.NumValidGenbankResults: Integer;
begin
  Result := Length(FCDSs);
end;

function TBrowserImportController.NumCDSs(region: Integer): Integer;
begin
  Result := 0;
  if (region >= Low(FCDSs)) and (region <= High(FCDSs)) then
    Result := Length(FCDSs[region]);
end;

function TBrowserImportController.HasCDSs: Boolean;
var
  i: Integer;
begin
  Result := False;
  if Length(FCDSs) > 0 then
    for i := Low(FCDSs) to High(FCDSs) do
      if (Length(FCDSs[i]) > 0) and (FCDSs[i][0].gene <> FULL_SEQUENCE_STR) then
      begin
        Result := True;
        Exit;
      end;
end;

function TBrowserImportController.GetCDSs: TCDSArrayArray;
begin
  if Length(FCDSs) = 0 then
    FindCDSs;
  Result := FCDSs;
end;


{ Given a TStringList of fasta formatted sequence data that has already
  been validated, creates a TSequenceList. It is assumed that the data has
  been validated using the 'ValidateData' function(s) defined for this class.}
function TBrowserImportController.BuildSequenceListFromFasta(ValidData: TStringList): TSequenceList;
var
  i: Integer;
  SequenceId: String;
  SequenceData: String;
  FoundNewId: boolean;
  CurrentString: String;
  SequenceList: TSequenceList;
  SequenceObject: TSequence;
begin
  CurrentString := Trim(ValidData[0]);
  Assert(BeginsWith(CurrentString, '>')); // if the data is valid, this will always be true
  SequenceList := TSequenceList.Create;
  SequenceId := CurrentString;
  SequenceData := EmptyStr;
  for i := 1 to ValidData.Count - 1 do
  begin
    CurrentString := Trim(ValidData[i]);
    FoundNewId := BeginsWith(CurrentString, '>');
    if FoundNewId then
    begin
      SequenceObject := FSequenceFactory.GenerateFastaSequenceObject(SequenceId, SequenceData);
      SequenceList.Add(SequenceObject);
      SequenceId := CurrentString;
      SequenceData := EmptyStr;
    end
    else
      SequenceData := SequenceData + CurrentString;
  end;
  SequenceObject := FSequenceFactory.GenerateFastaSequenceObject(SequenceId, SequenceData); // create the last one
  SequenceList.Add(SequenceObject);
  Result := SequenceList;
  SequenceObject := nil;
end;

function TBrowserImportController.GetSequenceList: TSequenceList;
begin
  if Assigned(FSequenceList) then
  begin
    Result := TSequenceList.Create;
    Result.Assign(FSequenceList);
  end
  else
    Result := nil;
end;

function TBrowserImportController.GetWebDataFormat: TWebSeqDataFormat;
begin
  Result := FWebDataFormat;
end;

procedure TBrowserImportController.SetRawSequenceData(AValue: TStringList);
begin
  if not Assigned(FRawSequenceData) then
    FRawSequenceData := TStringList.Create;
  FRawSequenceData.Assign(AValue);
  if ValidateFastaData(FRawSequenceData) then
    FWebDataFormat := wsdfFasta
  else if ValidateGenBankData(FRawSequenceData) then
  begin
    FWebDataFormat := wsdfGenbank;
    FindCDSs;
  end
  else
    FWebDataFormat := wsdfUnknown;
end;

procedure TBrowserImportController.SetSequenceList(AValue: TSequenceList);
begin
   FSequenceList := AValue;
end;

function TBrowserImportController.BuildSequenceListFromGenBank: TSequenceList;
var
  TempStringList: TStringList = nil;
  i: Integer;
  aSeq: TSequence = nil;
begin
  Assert(FGenbankValidRegions.Count > 0);
  if Assigned(FSequenceList) then
    FreeAndNil(FSequenceList);
  FSequenceList := TSequenceList.Create;
  for i := 0 to FGenbankValidRegions.Count - 1 do
  begin
    if (Length(FCDSs[i]) = 0) and ImportOnlyCDSs then
      continue;
    TempStringList := TStringList(FGenbankValidRegions[i]);
    aSeq := TSequence.Create;
    if ImportOnlyCDSs and (FCDSs[i][0].gene <> FULL_SEQUENCE_STR) then
      FSequenceFactory.ExtractAllGenbankData(TempStringList, FCDSs[i], aSeq)
    else
      FSequenceFactory.ExtractAllGenbankData(TempStringList, aSeq);
    FSequenceList.Add(aSeq);
  end;
  Result := GetSequenceList;
end;

function TBrowserImportController.ValidateGenBankData(RawData: TStringList): boolean;
begin
  Result := FSequenceFactory.ValidateGenbank(RawData);
  GenBankValidationWasAttempted := true;
end;

function TBrowserImportController.MapNameFieldToValue(ASequence: TSequence;
  AField: String): String;
begin
  if AField = 'empty' then
     Result := EmptyStr
  else if AField = 'species' then
     Result := ASequence.Species
  else if AField = 'subspecies' then
       Result := ASequence.Subspecies
  else if AField = 'strain' then
       Result := ASequence.Strain
  else if AField = 'host' then
       Result := ASequence.Host
  else if AField = 'senotype' then
       Result := ASequence.Senotype
  else if AField = 'gene' then
       Result := ASequence.Gene
  else if AField = 'allele' then
       Result := ASequence.Allele
  else if AField = 'uids' then
       Result := ASequence.AccessionNum
  else
      Result := AField;
end;

{ Determines if the given RawData is in a format that we know how to
  handle. If the data is valid, we can create the required sequence objects
  and export them to AlignEditMainForm. Otherwise, we cannot handle the data
  reliably.}
function TBrowserImportController.ValidateData(RawData: TStringlist): boolean;
var
  IsFasta: boolean;
begin
  IsFasta := false;
  Result := ValidateFastaData(RawData);
  if Result = true then
  begin
    FWebDataFormat := wsdfFasta;
    IsFasta := true;
  end
  else
  begin
    Result := ValidateGenBankData(RawData);
    if Result then
      FWebDataFormat := wsdfGenbank;
  end;

  if Result = true then
    DetermineDataType(RawData, IsFasta);
end;

function TBrowserImportController.ValidateFastaData(RawData: TStringList): boolean;
begin
  FastaValidationWasAttempted := true; // This condition will be asserted whenever ExportToAlignEditor is called because that procedure assumes valid data
  Result := FSequenceFactory.ValidateFasta(RawData); // delegate to FSequenceFactory, which knows about sequence data
end;

function TBrowserImportController.ValidateFastaData(RawSeqId: String; RawSeqData: String): boolean;
begin
  FastaValidationWasAttempted := true; // This condition will be asserted whenever ExportToAlignEditor is called because that procedure assumes valid data
  Result := FSequenceFactory.ValidateFasta(RawSeqId, RawSeqData);
end;

function TBrowserImportController.ValidateMessyFastaData(RawData: TStringList): boolean;
begin
  Result := FSequenceFactory.ValidateMessyFasta(RawData); // delegate to FSequenceFactory, which knows about sequence data
end;

function TBrowserImportController.FindGenbankValidRegions: Boolean;
var
  InsideValidRegion: boolean;
  FoundOriginLabel: boolean;
  FoundNewId: boolean;
  CurrentString: String;
  TempStringList: TStringList = nil;
  validRegion: TStringList = nil;
  CurrentIndex: Integer;
begin
  if not Assigned(FGenbankValidRegions) then
    FGenbankValidRegions := TList.Create
  else
    ClearGenbankValidRegions;
  TempStringList := TStringList.Create;
  CurrentIndex := 0;
  InsideValidRegion := false;
  try
    try
    while CurrentIndex < FRawSequenceData.Count - 1 do
    begin
      TempStringList.Clear;
      FoundNewId := False;
      FoundOriginLabel := False;
      while (FoundNewId <> True) and (CurrentIndex < FRawSequenceData.Count - 1) do
      begin
        CurrentString := Trim(FRawSequenceData[CurrentIndex]);
        if (BeginsWith(CurrentString, 'LOCUS')) or (BeginsWith(CurrentString, 'DEFINITION')) then
          InsideValidRegion := true;
        if BeginsWith(CurrentString, 'ORIGIN') then
          FoundOriginLabel := True;
        if (CurrentString <> EmptyStr) and (InsideValidRegion) then
          TempStringList.Add(CurrentString);
        inc(CurrentIndex);
        if CurrentString = '//' then
        begin
          InsideValidRegion := false;
          FoundNewId := true;
          if FoundOriginLabel then
          begin
            validRegion := TStringList.Create;
            validRegion.AddStrings(TempStringList);
            FGenbankValidRegions.Add(validRegion);
          end;
        end;
      end;
    end;
    Result := FGenbankValidRegions.Count > 0;
    except
      on E:Exception do
        DisplayJsMessageDialog('An error occurred when parsing Genbank data: ' + E.Message);
    end;
  finally
    if Assigned(TempStringList) then
      TempStringList.Free;
  end;
end;

procedure TBrowserImportController.ClearGenbankValidRegions;
var
  i: Integer;
begin
  if Assigned(FGenbankValidRegions) and (FGenbankValidRegions.Count > 0) then
    for i := 0 to FGenbankValidRegions.Count - 1 do
      if Assigned(FGenbankValidRegions[i]) then
        TStringList(FGenbankValidRegions[i]).Free;
end;

procedure TBrowserImportController.DetermineDataType(ValidData: TStringList;
  IsFasta: Boolean);
var
  MySampleOfData: String;
  ListIndex: Integer;
  FoundSequenceData: Boolean;
  i: Integer;
const
  MaxSampleSize = 500;
begin
  ListIndex := 0;
  MySampleOfData := EmptyStr;

  if IsFasta then
  begin
    while (Length(MySampleOfData) < MaxSampleSize) and (ListIndex < ValidData.Count - 1) do
    begin
      if Pos('>', ValidData[ListIndex]) = 0 then
        MySampleOfData := MySampleOfData + Trim(ValidData[ListIndex]);
      inc(ListIndex);
    end;
  end
  else
  begin
    FoundSequenceData := false;
    while (Length(MySampleOfData) < MaxSampleSize) and (ListIndex < ValidData.Count - 1) do
    begin
      if Pos('ORIGIN', ValidData[ListIndex]) > 0 then // these two markers identify the boundaries of the sequence data area
        FoundSequenceData := true
      else if Pos('//', ValidData[ListIndex]) > 0 then
        FoundSequenceData := false;
      if FoundSequenceData = true then
      begin
        for i := 1 to Length(ValidData[ListIndex]) do
         if not (ValidData[ListIndex][i] in ['0'..'9', ' ']) then
           MySampleOfData := MySampleOfData + ValidData[ListIndex][i];
      end;
      inc (ListIndex);
    end;
  end;
  FDataType := FSequenceFactory.DetermineDataType(MySampleOfData);
end;

procedure TBrowserImportController.ExportFastaToAlignEditor(ValidData: TStringList);
var
  IsFasta: Boolean;
begin
  HtmlOptionsDialog.AddToAlign := True;
  Assert(FastaValidationWasAttempted or GenBankValidationWasAttempted);
  Assert((FDataType = snNucleotide) or (FDataType = snProtein));
  IsFasta := ValidateFastaData(ValidData);
  Assert(IsFasta);
  FSequenceList := BuildSequenceListFromFasta(ValidData);
  ExportToAlignEditor;
end;

procedure TBrowserImportController.ExportToAlignEditor;
var
  i: Integer;
begin
  Assert((FDataType = snNucleotide) or (FDataType = snProtein));
  FindAlignmentEditorWindow(false);
  if AlignEditMainForm.AlignmentIsInProgress then
  begin
    DisplayJsMessageDialog('Cannot import sequence data to Alignment Explorer while a sequence alignment is still in progress');
    Exit;
  end;
  if AlignEditMainForm.PageControl1.ActivePageIndex = 1 then
    AlignEditMainForm.ActionTranslateExecute(nil);

  if AlignEditMainForm.AlignGrid1.Empty then // tell AlignEditMainForm what type of data to expect
  begin
    if FDataType = snNucleotide then
      AlignEditMainForm.DNAMenuItemClick(nil)
    else
      AlignEditMainForm.ProteinMenuItemClick(nil);
  end
  else // make sure that we are not mixing nucleotide and amino acid data in the same alignment grid
  begin
    {$IFNDEF DARWIN}
    if (FDataType = snProtein) and AlignEditMainForm.AlignGrid1.IsDNA then
    begin
      if MessageDlg('Warning: about to add protein sequence(s) to DNA alignment. Please confirm.', mtConfirmation, [mbOK, mbCancel], 0) = mrCancel then
        exit;
    end
    else if (FDataType = snNucleotide) and not AlignEditMainForm.AlignGrid1.IsDNA then
      if MessageDlg('Warning: about to add DNA sequence(s) to protein alignment. Please confirm', mtConfirmation, [mbOK, mbCancel], 0) = mrCancel then
        exit;
    {$ELSE} { on macOS we cannot show a modal dialog in the context of the browser process so just inform the user}
    if (FDataType = snProtein) and AlignEditMainForm.AlignGrid1.IsDNA then
      DisplayJsMessageDialog('Warning: added protein sequence(s) to DNA alignment. Please check the data carefully.')
    else if (FDataType = snNucleotide) and not AlignEditMainForm.AlignGrid1.IsDNA then
      DisplayJsMessageDialog('Warning: added DNA sequence(s) to protein alignment. Please check the data carefully.');
    {$ENDIF}
  end;
  Assert((FSequenceList <> nil) and (FSequenceList.Count > 0));
  try
    AlignEditMainForm.AlignGrid1.SkipHeaderUpdates := True;
    for i := 0 to FSequenceList.Count - 1 do
      AlignEditMainForm.AlignGrid1.AppendSequence(FSequenceList[i]);
  finally
    AlignEditMainForm.AlignGrid1.SkipHeaderUpdates := False;
  end;
  FSequenceList.DeleteAll;
  FreeAndNil(FSequenceList);
  AlignEditMainForm.Visible := True;
  AlignEditMainForm.InitForm;
end;


end.
 
