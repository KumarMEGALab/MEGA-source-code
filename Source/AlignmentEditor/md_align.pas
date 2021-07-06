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

unit MD_Align;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses

  LCLIntf, LCLType, FileUtil,
  MClustalW, Classes, SysUtils, MProcessPack, MD_InputSeqData,
  MegaConsts, KeywordConsts, TypInfo, MD_Sequences, MegaUtils, AppLinker,
  RegExpr, MAnalysisInfo,
{$IFDEF VISUAL_BUILD}AppOptionsDlg,{$ENDIF} Dialogs, Forms, Controls;


/// <summary>
/// Coordinates multiple sequence alignment for running MEGA in batch mode (also provides
/// services for running MUSCLE alignments when in visual mode).
/// </summary>
/// <remarks>
/// For MUSCLE alignments, a TMuscleLink (extends TAppLink) object is used to setup and run the
/// MUSCLE.exe executable. For ClustalW alignments, MEGA's TClustalW class is used and so does
/// not require the use of TAppLink.
/// </remarks>
type

  { TD_Align }

  TD_Align = class
  private
    FProcessPack: TProcessPack;
    ClustalW: TClustalW;
    FWriteAlignmentsToFile: Boolean;
    procedure ClustalWriteResults(Sender : TObject);
    function LoadThirdPartyFileFormat(InFile : String; Overwrite : boolean; FDataType: TSnTokenCode): boolean;
    function OpenAFastaFile(AFileName: String; Overwrite: boolean): boolean;
    function OpenAMegaFile(AFileName: String; Overwrite: boolean): boolean;
    function OpenFromActiveSeqs(SeqList : TSequenceList; Overwrite: boolean): boolean;
//    function ReadMEGA2SequenceFile(FileName: AnsiString; Data: TSequenceList; HasPara: Boolean;
//                  ADataType, ADataFormat: TSnTokenCode; AMissSym, AIdenSym, AGapSym: AnsiChar): Boolean;
    procedure WriteMEGAFile(SeqList: TSequenceList; FileName: String);
    procedure WriteFastaFile(SeqList: TSequenceList; FileName: String);
    procedure ClustalUnTranslateAndWriteResults(Sender: TObject);
    procedure SetUpClustalParameters(ProcessPack : TProcessPack);
    procedure AddMuscleUserOptions(var muscleOptions: TStringList; const maoStrings: TStringList);
    procedure UpdateUsageStatistics(methodName: String; isDna: Boolean; isCoding: Boolean);
    procedure WorkflowMuscleThreadFinished(aThread: TObject);
  public
    ASeqList : TSequenceList;
    destructor Destroy; override;
    Constructor Create;
    function WriteAlignmentToMegaFile(filename: String; isCoding: Boolean): Boolean;
    procedure MuscleAlignExecute(Caller : TObject; MuscleAlignCodons : boolean = FALSE; MuscleIsDNA : boolean = FALSE; PresetArgs : AnsiString = ''; PhyloQMAI: TPhyloQAnalysisInfo = nil);
    procedure ClustalAlignExecute(ProcessPack : TProcessPack);
  published
    property WriteAlignmentsToFile: Boolean read FWriteAlignmentsToFile write FWriteAlignmentsToFile;
  end;

  Const
    AllExt1    = '.*';

  // use in import from file
  AlignmentSessionFilter= 'Aln Session ('+MasExts    +')|'+MasExts;
  ImportFromFileFilters = 'FASTA ('        +FastaExts  +')|'+FastaExts  +'|'+
                          'PAUP/MacClade ('+PaupExts   +')|'+PaupExts   +'|'+
                          'MEGA ('         +MegaExts   +')|'+MegaExts   +'|'+
                          'CLUSTAL ('      +ClustalExts+')|'+ClustalExts+'|'+
                          'Phylip ('       +PhylipExts +')|'+PhylipExts +'|'+
                          'GCG ('          +GCGExts    +')|'+GCGExts    +'|'+
                          'PIR ('          +PIRExts    +')|'+PIRExts    +'|'+
                          'NBRF ('         +NBRFExts   +')|'+NBRFExts   +'|'+
                          'MSF ('          +MSFExts    +')|'+MSFExts    +'|'+
                          'IG ('           +IGExts     +')|'+IGExts     +'|'+
                          'All (*.*)'                  +'|'+'*.*';

  InsertFromFileFilters = 'ABI ('          +ABIExts    +')|'+ABIExts    +'|'+
                          'Staden ('       +StadenExts +')|'+StadenExts +'|'+
                          'Text ('         +TextExts   +')|'+TextExts   +'|'+
                          'XML ('          +XMLExts    +')|'+XMLExts    +'|'+
                          ImportFromFileFilters;

procedure AlignSequences(ProcessPack : TProcessPack);
function MuscleAlignProfileExecute(Caller:TObject; MuscleAlignCodons: boolean = FALSE; MuscleIsDNA : boolean = FALSE; PresetArgs : AnsiString = ''; PhyloQMAI: TPhyloQAnalysisInfo = nil):TSequenceList;
function MuscleAlignPairwiseExecute(Caller:TObject; MuscleAlignCodons: boolean = FALSE; MuscleIsDNA : boolean = FALSE; PresetArgs : AnsiString = ''; PhyloQMAI: TPhyloQAnalysisInfo = nil):TSequenceList;
function ClustalWAlignPairwiseExecute(PhyloQMAI: TPhyloQAnalysisInfo):TSequenceList;
procedure SetGeneticCode(GeneticCodeTable : AnsiString);
procedure LoadAlignmentDataFromFile(FileName: String; FDataType: TSnTokenCode);
procedure LoadAlignmentDataFromActiveSeqs(Seqs: TSequenceList);
procedure MuscleFinished(AppLink: TAppLink);

var
  D_Align : TD_Align;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  Mega_Main,
  {$ELSE}
  MD_MegaMain, MegaErrUtils, MegaUtils_NV,
  {$ENDIF}
  MAlnThread, MDistPack, ProcessInputData, MUsageStatistics, MFormatConvertToMega,
  MRuntimeProgressDlg, MegaAnalysisPrefStrings, mworkflow_alignment_element;

procedure SetGeneticCode(GeneticCodeTable : AnsiString);
begin
  D_Align.ASeqList.CodeTable := GeneticCodeTable;
end;

/// <summary>Frees the ClustalW object and optionally the TAlignEditMainForm object in the case of a VISUAL_BUILD</summary>
destructor TD_Align.Destroy;
begin
  FreeAndNil(ClustalW);
end;

/// <summary>Instantiates a TD_Align object with an initialized TClustalW object</summary>
constructor TD_Align.Create;
begin
  inherited;
  ClustalW := TClustalW.Create(nil);
  WriteAlignmentsToFile := True;
end;

function TD_Align.WriteAlignmentToMegaFile(filename: String; isCoding: Boolean): Boolean;
var
  tempList: TSequenceList = nil;
  i: Integer;
  s: TSequence = nil;
begin
  Result := False;
  try
    tempList := TSequenceList.Create;
    tempList.IsDNA := ClustalW.IsDNA;
    tempList.IsProteinCoding := isCoding;
    if ClustalW.Sequences.Count > 0 then
    begin
      for i := 0 to ClustalW.Sequences.Count - 1 do
      begin
        s := TSequence.Create;
        s.SeqName := ClustalW.SeqNames[i];
        s.SeqData := ClustalW.Sequences[i];
        tempList.Add(s);
      end;
      WriteMEGAFile(tempList, filename);
      Result := FileExists(filename);
    end
    else if ASeqList.Count > 0 then
    begin
      WriteMEGAFile(ASeqList, filename);
      Result := FileExists(filename);
    end;
  finally
    if Assigned(tempList) then
      tempList.Free;
  end;
end;


procedure AlignSequences(ProcessPack : TProcessPack);
var
  i: Integer;
  MyKey: AnsiString;
  MyValue: AnsiString;
begin
  if D_Align = nil then
    Raise Exception.Create('You must first load data for alignment');
  ShowRunStatusInfoStatic('status', 'Alignment settings:');
  for i := 0 to ProcessPack.TextualSettingsList.Count - 1 do
  begin
    MyKey := ProcessPack.TextualSettingsList.Names[i];
    MyValue := ProcessPack.TextualSettingsList.Values[MyKey];
    ShowRunStatusInfoStatic(' ', MakePrettyString(MyKey, 35) + ' : ' + MyValue);
  end;
  ShowRunStatusInfoStatic('status', ' ');

  if SameText(ProcessPack.TextualSettingsList.Values[containsCodingNucStr], 'True') then
    processpack.containsCodingNuc := true;
  if ProcessPack.IsWorkflowPack then
    TWorkflowAlignmentElement(ProcessPack.WorkflowElement).Aligner := D_Align;
  if ProcessPack.ProcessTypes[1] = ppMuscle then
    D_Align.MuscleAlignExecute(ProcessPack)
  else if ProcessPack.ProcessTypes[1] = ppClustalW then
    D_Align.ClustalAlignExecute(ProcessPack);
  {$IFDEF VISUAL_BUILD}
  if not ProcessPack.IsWorkflowPack then
    FreeAndNil(D_Align);
  {$ENDIF}
end;

/// <summary>Performs a profile-profile alignment using MUSCLE</summary>
/// <remarks>This function is used only for the PhyloQ analysis as it makes certain assumptions about
/// available data objects</remarks>
/// <returns>A TSequenceList where the sequence at every even index (0,2,4,...) is the PhyloQ target
/// sequence and the sequence at every odd index(1,3,5,...) is one of the reference sequences.</returns>
function MuscleAlignProfileExecute(Caller:TObject; MuscleAlignCodons: boolean = FALSE; MuscleIsDNA : boolean = FALSE; PresetArgs : AnsiString = ''; PhyloQMAI: TPhyloQAnalysisInfo = nil):TSequenceList;
var
  AlignedSequences: TSequenceList;
  i : Integer;
  TargetSequence: TSequence;
  TempSequence1: TSequence;
  TempSequence2: TSequence;
  NumRefSeqs: Integer;
begin
  ShowRunStatusInfoStatic('status', 'Waiting for MUSCLE to perform profile alignment');
  NumRefSeqs := D_Align.ASeqList.Count;
  D_Align.MuscleAlignExecute(Caller, MuscleAlignCodons, MuscleIsDNA, PresetArgs, PhyloQMAI);

  ShowRunStatusInfoStatic('status', 'Reading profile alignment');
  TargetSequence := TSequence.Create;
  TargetSequence.SeqData := D_Align.ASeqList[D_Align.ASeqList.Count - 1].SeqData;
  TargetSequence.SeqName := D_Align.ASeqList[D_Align.ASeqList.Count - 1].SeqName;

  // set up the aligned sequences list
  AlignedSequences := TSequenceList.Create;
  for i := 0 to NumRefSeqs - 1 do
  begin
    TempSequence1 := TSequence.Create;
    TempSequence2 := TSequence.Create;
    AlignedSequences.Add(TempSequence1);
    AlignedSequences.Add(TempSequence2);
  end;

  // build the sequence list interleaving the target sequence with the reference sequences
  for i := 0 to D_Align.ASeqList.Count - 2 do
  begin
    AlignedSequences.Items[2*i].SeqData := TargetSequence.SeqData;
    AlignedSequences.Items[2*i].SeqName := TargetSequence.SeqName;
    AlignedSequences.Items[(2*i) + 1].SeqData := D_Align.ASeqList.Items[i].SeqData;
    AlignedSequences.Items[(2*i) + 1].SeqName := D_Align.ASeqList.Items[i].SeqName;

  end;

  Result := TSequenceList.Create;
  Result.Assign(AlignedSequences);

  TempSequence1 := nil;
  TempSequence2 := nil;
  TargetSequence := nil;
end;

/// <summary>Create pairwise alignments between the PhyloQTargetSequence and all sequences in the reference alignment using MUSCLE</summary>
function MuscleAlignPairwiseExecute(Caller:TObject; MuscleAlignCodons: boolean = FALSE; MuscleIsDNA : boolean = FALSE; PresetArgs : AnsiString = ''; PhyloQMAI: TPhyloQAnalysisInfo = nil):TSequenceList;
var
  TempSequences: TSequenceList;
  AlignedSequences: TSequenceList;
  i : Integer;
  TargetSequence: TSequence;
  TempSequence1: TSequence;
  TempSequence2: TSequence;
begin
  TempSequences := TSequenceList.Create;
  TempSequences.Assign(D_Align.ASeqList);
  AlignedSequences := TSequenceList.Create;
  TargetSequence := TSequence.Create;
  TargetSequence.SeqData := PhyloQMAI.PhyloQTargetSequence;
  TargetSequence.SeqName := PhyloQMAI.PhyloQTargetName;

  for i := 0 to TempSequences.Count - 1 do
  begin
    TempSequence1 := TSequence.Create;
    TempSequence2 := TSequence.Create;
    AlignedSequences.Add(TempSequence1);
    AlignedSequences.Add(TempSequence2);
  end;


  D_Align.ASeqList.Clear;
  D_Align.ASeqList.add(TargetSequence);
  D_Align.ASeqList.Add(TempSequences.Items[0]);
  ShowRunStatusInfoStatic('status', 'Performing pairwise alignments using MUSCLE');
  for i := 0 to TempSequences.Count - 1 do
  begin
    D_Align.ASeqList.Items[0] := TargetSequence;
    D_Align.ASeqList.Items[1] := TempSequences.Items[i];
    ShowProgressIncrementStatic;
    D_Align.MuscleAlignExecute(Caller, MuscleAlignCodons, MuscleIsDNA, PresetArgs{$IFNDEF VISUAL_BUILD},PhyloQMAI{$ENDIF});
    AlignedSequences.Items[2*i].SeqData := D_Align.ASeqList.Items[0].SeqData;
    AlignedSequences.Items[2*i].SeqName := D_Align.ASeqList.Items[0].SeqName;
    AlignedSequences.Items[(2*i) + 1].SeqData := D_Align.ASeqList.Items[1].SeqData;
    AlignedSequences.Items[(2*i) + 1].SeqName := D_Align.ASeqList.Items[1].SeqName;
  end;
  ShowRunStatusInfoStatic('status', ' ');
  Result := TSequenceList.Create;
  Result.Assign(AlignedSequences);
end;

/// <summary>Create pairwise alignments between the PhyloQTargetSequence and all sequences in the reference alignment using ClustalW</summary>
function ClustalWAlignPairwiseExecute(PhyloQMAI: TPhyloQAnalysisInfo):TSequenceList;
var
  TempSequences: TSequenceList;
  AlignedSequences: TSequenceList;
  i : Integer;
  TargetSequence: TSequence;
  TempSequence1: TSequence;
  TempSequence2: TSequence;
begin
  D_Align.WriteAlignmentsToFile := False;
  TempSequences := TSequenceList.Create;
  TempSequences.Assign(D_Align.ASeqList);
  AlignedSequences := TSequenceList.Create;
  TargetSequence := TSequence.Create;
  TargetSequence.SeqData := PhyloQMAI.PhyloQTargetSequence;
  TargetSequence.SeqName := PhyloQMAI.PhyloQTargetName;

  for i := 0 to TempSequences.Count - 1 do
  begin
    TempSequence1 := TSequence.Create;
    TempSequence2 := TSequence.Create;
    AlignedSequences.Add(TempSequence1);
    AlignedSequences.Add(TempSequence2);
  end;


  D_Align.ASeqList.Clear;
  D_Align.ASeqList.add(TargetSequence);
  D_Align.ASeqList.Add(TempSequences.Items[0]);

  for i := 0 to TempSequences.Count - 1 do
  begin
    D_Align.ASeqList.Items[0] := TargetSequence;
    D_Align.ASeqList.Items[1] := TempSequences.Items[i];
    ShowRunStatusInfoStatic('status', 'ClustalW aligning pair: ' + IntToStr(i + 1));
    D_Align.ClustalAlignExecute(PhyloQMAI.MyProcessPack);
    AlignedSequences.Items[2*i].SeqData := D_Align.ASeqList.Items[0].SeqData;
    AlignedSequences.Items[2*i].SeqName := D_Align.ASeqList.Items[0].SeqName;
    AlignedSequences.Items[(2*i) + 1].SeqData := D_Align.ASeqList.Items[1].SeqData;
    AlignedSequences.Items[(2*i) + 1].SeqName := D_Align.ASeqList.Items[1].SeqName;
  end;

  Result := TSequenceList.Create;
  Result.Assign(AlignedSequences);
  TempSequences := nil;
  TempSequence1 := nil;
  TempSequence2 := nil;
  TargetSequence := nil;

end;

procedure TD_Align.MuscleAlignExecute(Caller : TObject ; MuscleAlignCodons : boolean = FALSE; MuscleIsDNA : boolean = FALSE; PresetArgs : AnsiString = ''; PhyloQMAI: TPhyloQAnalysisInfo = nil);
var
  {$IFNDEF VISUAL_BUILD}
  SeqFlag: String = '';
  i: Integer;
  {$ENDIF}
  ProcessPack : TProcessPack = nil;
  TempDir: String;
  MuscleExeFile: String;
  MuscleLink: TMuscleLink = nil;
  MAI: TAnalysisInfo = nil;
  ARP: TRuntimeProgress = nil;
  Options: TStringList = nil;
  isSuccess: Boolean = fALSE;
  aMsg: String = '';
begin
  try try
    if Caller.ClassNameIs('TProcessPack') then
    begin
      ProcessPack := TProcessPack(Caller);
      FProcessPack := ProcessPack;
    end
    else
      raise Exception.Create('invalid caller to MuscleAlignExecute');

    MuscleExeFile := GetMuscleExe;
    if not FileExists(MuscleExeFile) then
      Raise Exception.Create('MEGA installation appears to be corrupted, as critical MUSCLE files are missing.  Please reinstall MEGA.');

    if ASeqList.Count < 2 then
      Raise Exception.Create('At least two sequences need to be provided for alignment.');

    ARP := TRuntimeProgress.Create(nil);
    if not ProcessPack.ContainsProcessType(ppPhyloQ) then
    	MAI := TAnalysisInfo.Create
    else
      if PhyloQMAI <> nil then
        MAI := PhyloQMAI
      else
      	Raise Exception.Create('For PhyloQ analysis, PhyloQMAI cannot be nil');

    MAI.ARP := ARP;
    if not ProcessPack.ContainsProcessType(ppPhyloQ) then
      MAI.MyProcessPack := ProcessPack;
    MuscleLink := TMuscleLink.Create(nil);
    Options := MuscleLink.MuscleOptions;
    MuscleLink.OnTerm := @MuscleFinished;

    TempDir := GetAppConfigDir(False) + 'temp' + PathDelim;
    if not DirectoryExists(TempDir) then
      if not ForceDirectories(TempDir) then
        raise Exception.Create('unable to create temp directory needed for MUSCLE Alignment: ' + TempDir);
    {$IFNDEF VISUAL_BUILD}
    if (D_MegaMain.FDataType = snNucleotide) and (not ASeqList.IsDNA) then
      Error_nv('Tried to align protein sequence with nucleotide options.');
    if (D_MegaMain.FDataType = snProtein) and ASeqList.IsDNA and (not ASeqList.IsProteinCoding) then
      Error_nv('Tried to align non-coding nucleotide sequnces with protein sequence options.');
    {$ENDIF}
    MuscleLink.IsDNA := (ProcessPack.TextualSettingsList.Values['datatype'] = 'snNucleotide') and (not ProcessPack.ContainsCodingNuc);
    MuscleLink.IscDNA := ProcessPack.containsCodingNuc;
    MuscleLink.IsProtein := (ProcessPack.TextualSettingsList.Values['datatype'] = 'snProtein');
    {$IFNDEF VISUAL_BUILD}
    if MuscleLink.IsDNA then
      D_MegaMain.AnalysisSummary.DataType := snNucleotide
    else if MuscleLink.IscDNA then
      D_MegaMain.AnalysisSummary.DataType := snCoding
    else if MuscleLink.IsProtein then
      D_MegaMain.AnalysisSummary.DataType := snProtein;
    D_MegaMain.AnalysisSummary.NumTaxa := ASeqList.Count;
    for i := 0 to ProcessPack.TextualSettingsList.Count - 1 do
      D_MegaMain.AnalysisSummary.AnalysisOptions.Add(ProcessPack.TextualSettingsList[i]);
    {$ENDIF}
    MuscleLink.OutputFilename := GetMuscleOutputFile;
    MuscleLink.InputFilename := GetMuscleInputFile;


    if (ProcessPack.ContainsProcessType(ppProfileAlignment)) and (ProcessPack.ContainsProcessType(ppPhyloQ)) then
    begin
      {$IFNDEF VISUAL_BUILD}
      Options.Add(Format('-profile=%s', [SeqFlag]));
      Options.Add(Format('-in1=%s', [MuscleLink.InputFileName]));
      Options.Add(Format('-in2=%s', [D_MegaMain.PhyloQFileName]));
      Options.Add(Format('-out=%s', [MuscleLink.OutputFilename]));
      {$ENDIF}
    end
    else
    begin
      AddMuscleUserOptions(Options, ProcessPack.TextualSettingsList);
    end;

    MuscleLink.SetSequenceData(ASeqList);
    UpdateUsageStatistics('MuscleAlign', not MuscleLink.IsProtein, MuscleLink.IscDNA);

    LaunchMuscleLinkThread(MuscleLink, MAI, WorkflowMuscleThreadFinished, isSuccess, aMsg);
    if not isSuccess then
      raise Exception.Create(aMsg);
    MuscleLink := nil;
    Except
      on E: Exception do
      begin
        {$IFDEF VISUAL_BUILD}
        raise E;
        {$ELSE}
        warn_NV('An unexpected error occurred while performing sequence alignment with MUSCLE. ' + E.Message);
        Exit;
        {$ENDIF}
      end;
  end;
  finally
    if Assigned(MuscleLink) then
      MuscleLink.Free;
  end;
end;

procedure TD_Align.ClustalAlignExecute(ProcessPack : TProcessPack);
var
  {$IFNDEF VISUAL_BUILD}
  Row: Integer = -1;
  Col: Integer = -1;
  {$ENDIF}
  i: integer;
  SequenceList : TStringList;
  SequenceNames : TStringList;
begin
  if ASeqList.Count < 2 then
  begin
    Raise Exception.Create('At least two sequences need to be provided for alignment.');
    Exit;
  end;
  SequenceList := TStringList.Create;
  SequenceNames := TStringList.Create;
  D_Align.SetUpClustalParameters(ProcessPack);

  UpdateUsageStatistics('clustalWAlign', aSeqList.IsDNA, aSeqList.IsProteinCoding);

  try
    for i := 0 to ASeqList.Count-1 do
    begin
      SequenceList.Add(ASeqList[i].SeqData);
      SequenceNames.Add(ASeqList[i].SeqName);
    end;
    ClustalW.Sequences := SequenceList;
    ClustalW.SeqNames := SequenceNames;
    {$IFNDEF VISUAL_BUILD}
    if MegaUtils.CheckStopCodons(ASeqList, Row, Col) then
      warn_nv('Stop codon(s) are found in the translated sequences. Is the correct genetic code selected?');
    {$ENDIF}
    if ProcessPack.IsWorkflowPack then
      ClustalW.OnTerminate := ProcessPack.WorkflowElement.ElementFinishedCallback;
    ClustalW.Align;
    {$IFNDEF VISUAL_BUILD}
    D_MegaMain.AnalysisSummary.WriteToFile(ChangeFileExt(NextAvailableFileNameNV('.txt'), '_summary.txt'));
    {$ENDIF}
    SequenceList.Free;
    SequenceNames.Free;
  Except on E: Exception do
    begin
        if ProcessPack.IsWorkflowPack then
        begin
          ProcessPack.WorkflowElement.IsSuccess := False;
          ProcessPack.WorkflowElement.ErrorMsg := E.Message;
        end;
      {$IFDEF VISUAL_BUILD}
      MessageDlg('An unexpected error occurred while performing sequence alignment with ClustalW. ' + E.Message, mtWarning, [mbOK], 0);
      {$ELSE}
      warn_NV('An unexpected error occurred while performing sequence alignment with ClustalW. ' + E.Message);
      {$ENDIF}
      Exit;
    end;
  end;
end;

/// <summary>Sets up parameter settings such as gap penalties and DNA matrix for the TClustalW object.</summary>
/// <remarks>
/// For a visual build we just call <see cref= "MAlignEditMainFrom|TAlignEditMainForm.ActionSetUpClustalExecute(self)" /> which
/// gets the settings from the user with the ClustalSetupDlg. For Batch mode, we need a TProcessPack
/// with the TextualSettingsList initiated from the user's analysis settings file.
/// </remarks>
procedure TD_Align.SetUpClustalParameters(ProcessPack : TProcessPack);
var
  MyDataType: String;
  containsCoding: Boolean;
begin
  {$IFNDEF VISUAL_BUILD}
  D_MegaMain.AnalysisSummary.NumTaxa := ASeqList.Count;
  {$ENDIF}
  MyDataType := ProcessPack.TextualSettingsList.Values['DataType'];
  containsCoding := SameText(ProcessPack.TextualSettingsList.Values[containsCodingNucStr], 'True');
  ClustalW.OnTerminate := ClustalWriteResults;
  if ((MyDataType = 'DNA') or (MyDataType = 'snNucleotide')) and (not containsCoding) then
  begin
    ClustalW.IsDNA := True;
    ClustalW.DNAPWGapOpenPenalty := StrToFloat(ProcessPack.TextualSettingsList.Values['DNAPWGapOpeningPenalty']);
    ClustalW.DNAPWGapExtendPenalty := StrToFloat(ProcessPack.TextualSettingsList.Values['DNAPWGapExtensionPenalty']);
    ClustalW.DNAGapOpenPenalty := StrToFloat(ProcessPack.TextualSettingsList.Values['DNAMAGapOpeningPenalty']);
    ClustalW.DNAGapExtendPenalty := StrToFloat(ProcessPack.TextualSettingsList.Values['DNAMAGapExtensionPenalty']);
    ClustalW.DNAMatrix := TDNAMatrix(GetEnumValue(TypeInfo(TDNAMatrix),ProcessPack.TextualSettingsList.Values['DNAWeightMatrix']));//ProcessPack.TextualSettingsList.Values['DNAWeightMatrix']);
    ClustalW.TransitionWeight := StrToFloat(ProcessPack.TextualSettingsList.Values['TransitionWeightNEdit']);
    {$IFNDEF VISUAL_BUILD}
    D_MegaMain.AnalysisSummary.DataType :=snNucleotide;
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('DNAPWGapOpenPenalty=' + ProcessPack.TextualSettingsList.Values['DNAPWGapOpeningPenalty']);
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('DNAPWGapExtendPenalty=' + ProcessPack.TextualSettingsList.Values['DNAPWGapExtensionPenalty']);
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('DNAGapOpenPenalty=' + ProcessPack.TextualSettingsList.Values['DNAMAGapOpeningPenalty']);
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('DNAGapExtendPenalty=' + ProcessPack.TextualSettingsList.Values['DNAMAGapExtensionPenalty']);
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('DNAMatrix=' + ProcessPack.TextualSettingsList.Values['DNAWeightMatrix']);
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('TransitionWeight=' + ProcessPack.TextualSettingsList.Values['TransitionWeightNEdit']);
    {$ENDIF}
  end
  else
  begin
    if ((MyDataType = 'cDNA') or (MyDataType = 'snNucleotide')) and containsCoding then
    begin
      ASeqList.CodeTable := ProcessPack.TextualSettingsList.Values['GeneticCodeTable'];
      ASeqList.Translate;
      ClustalW.OnTerminate := ClustalUntranslateAndWriteResults;
    end;
    {$IFNDEF VISUAL_BUILD}
    if ((MyDataType = 'cDNA') or (MyDataType = 'snNucleotide')) and containsCoding then
      D_MegaMain.AnalysisSummary.DataType := snCoding
    else
      D_MegaMain.AnalysisSummary.DataType := snProtein;
    {$ENDIF
    ClustalW.IsDNA := False; { TODO -oDan : Make sure this is correct, I think clustal will see it as protein since it should already be translated by the time clustal gets it }
    ClustalW.ProteinPWGapOpenPenalty := StrToFloat(ProcessPack.TextualSettingsList.Values['ProteinPWGapOpeningPenalty']);
    ClustalW.ProteinPWGapExtendPenalty := StrToFloat(ProcessPack.TextualSettingsList.Values['ProteinPWGapExtensionPenalty']);
    ClustalW.ProteinGapOpenPenalty := StrToFloat(ProcessPack.TextualSettingsList.Values['ProteinMAGapOpeningPenalty']);
    ClustalW.ProteinGapExtendPenalty := StrToFloat(ProcessPack.TextualSettingsList.Values['ProteinMAGapExtensionPenalty']);
    ClustalW.ProteinMatrix := TProteinMatrix(GetEnumValue(TypeInfo(TProteinMatrix),ProcessPack.TextualSettingsList.Values['ProteinWeightMatrix']));
    ClustalW.ResidueSpecificPenalty :=  ProcessPack.TextualSettingsList.Values['Residue-specificPenalties'] = 'ON';
    ClustalW.HydrophilicPenalty := ProcessPack.TextualSettingsList.Values['HydrophillicPenalties'] = 'ON';
    ClustalW.GapSeparationDistance := StrToInt(ProcessPack.TextualSettingsList.Values['GapSeperationDistance']);
    ClustalW.EndGapSeparation := ProcessPack.TextualSettingsList.Values['EndGapSeperation'] = 'ON';
    {$IFNDEF VISUAL_BUILD}
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('ProteinPWGapOpenPenalty=' + ProcessPack.TextualSettingsList.Values['ProteinPWGapOpeningPenalty']);
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('ProteinPWGapExtendPenalty=' + ProcessPack.TextualSettingsList.Values['ProteinPWGapExtensionPenalty']);
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('ProteinGapOpenPenalty=' + ProcessPack.TextualSettingsList.Values['ProteinMAGapOpeningPenalty']);
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('ProteinGapExtendPenalty=' + ProcessPack.TextualSettingsList.Values['ProteinMAGapExtensionPenalty']);
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('ProteinMatrix=' + ProcessPack.TextualSettingsList.Values['ProteinWeightMatrix']);
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('ResidueSpecificPenalty=' +  BoolToStr(ProcessPack.TextualSettingsList.Values['Residue-specificPenalties'] = 'ON', True));
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('HydrophilicPenalty=' + BoolToStr(ProcessPack.TextualSettingsList.Values['HydrophillicPenalties'] = 'ON', True));
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('GapSeparationDistance=' + ProcessPack.TextualSettingsList.Values['GapSeperationDistance']);
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('EndGapSeparation=' + BoolToStr(ProcessPack.TextualSettingsList.Values['EndGapSeperation'] = 'ON', True));
    {$ENDIF}
  end;
  ClustalW.UseNegativeMatrix := ProcessPack.TextualSettingsList.Values['UseNegativeMatrix'] = 'ON';
  ClustalW.DivergentCutoff := StrToInt(ProcessPack.TextualSettingsList.Values['DelayDivergentCutoff']);
  ClustalW.ResetGaps := ProcessPack.TextualSettingsList.Values['KeepPredefinedGaps'] = '0';
  ClustalW.GuideTreeFile := ProcessPack.TextualSettingsList.Values['GuideTree'];
  {$IFNDEF VISUAL_BUILD}
  D_MegaMain.AnalysisSummary.AnalysisOptions.Add('UseNegativeMatrix=' + BoolToStr(ProcessPack.TextualSettingsList.Values['UseNegativeMatrix'] = 'ON', True));
  D_MegaMain.AnalysisSummary.AnalysisOptions.Add('DivergentCutoff=' + ProcessPack.TextualSettingsList.Values['DelayDivergentCutoff']);
  D_MegaMain.AnalysisSummary.AnalysisOptions.Add('ResetGaps=' + BoolToStr(ProcessPack.TextualSettingsList.Values['KeepPredefinedGaps'] = '0', True));
  if Trim(ProcessPack.TextualSettingsList.Values['GuideTree']) <> EmptyStr then
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add('GuideTreeFile=' + ProcessPack.TextualSettingsList.Values['GuideTree']);
  {$ENDIF}
end;

procedure TD_Align.AddMuscleUserOptions(var muscleOptions: TStringList; const maoStrings: TStringList);
begin
  if maoStrings.IndexOfName(opsGapOpen) >= 0 then
    muscleOptions.Add(Format('-gapopen=%s', [maoStrings.Values[opsGapOpen]]));
  if maoStrings.IndexOfName(opsGapExtend) >= 0 then
    muscleOptions.Add(Format('-gapextend=%s', [maoStrings.Values[opsGapExtend]]));
  if maoStrings.IndexOfName(opsMaxMem) >= 0 then
    muscleOptions.Add(Format('-maxmb=%s', [maoStrings.Values[opsMaxMem]]));
  if maoStrings.IndexOfName(opsMaxIters) >= 0 then
    muscleOptions.Add(Format('-maxiters=%s', [maoStrings.Values[opsMaxIters]]));
  if maoStrings.IndexOfName(opsCluster1) >= 0 then
    muscleOptions.Add(Format('-cluster1=%s', [maoStrings.Values[opsCluster1]]));
  if maoStrings.IndexOfName(opsCluster2) >= 0 then
    muscleOptions.Add(Format('-cluster2=%s', [maoStrings.Values[opsCluster2]]));
  if maoStrings.IndexOfName(opsLambda) >= 0 then
    muscleOptions.Add(Format('-diaglength=%s', [maoStrings.Values[opsLambda]]));
  if maoStrings.IndexOfName(opsHydrophobicity) >= 0 then
    muscleOptions.Add(Format('-hydrofactor=%s', [maoStrings.Values[opsHydrophobicity]]));
end;

procedure TD_Align.UpdateUsageStatistics(methodName: String; isDna: Boolean; isCoding: Boolean);
var
  StatValue: String;
  usageStats: TUsageStatistics = nil;
begin
  try
    try
      if isDNA then
        StatValue := 'dna'
      else if isCoding then
        StatValue := 'codingDna'
      else
        StatValue := 'aminoAcid';
      UsageStats := TUsageStatistics.Create;
      UsageStats.UpdateUsageStatistics(methodName, StatValue);
    Except
      on E:Exception do
      begin
        Assert(False, 'Error saving usage statistics: ' + E.Message);
        // do nothing
      end;
    end;
  finally
    if UsageStats <> nil then
      UsageStats.Free;
  end;
end;

procedure TD_Align.WorkflowMuscleThreadFinished(aThread: TObject);
var
  t: TMuscleLinkThread = nil;
begin
  try
    if not (aThread is TMuscleLinkThread) then
      raise Exception.Create('expected TMuscleLinkThread but got ' + aThread.ClassName);
    t := TMuscleLinkThread(aThread);
    if not t.IsSuccess then
    begin
      FProcessPack.WorkflowElement.IsSuccess := False;
      FProcessPack.WorkflowElement.ErrorMsg := t.MessagesLog.Text;
      raise Exception.Create(t.MessagesLog.Text);
    end;
    {$IFDEF VISUAL_BUILD}
    FProcessPack.WorkflowElement.ElementFinishedCallback(Self);
    {$ENDIF}
  except
    on E:Exception do
    begin
      if Assigned(t) then
        t.SynchronizeErrorMessage(E)
      else
        raise E;
    end;
  end;
end;

procedure TD_Align.WriteMEGAFile(SeqList : TSequenceList; FileName : String);
var
  j: integer;
  i: integer;
  str : AnsiString;
  data : TextFile;
begin
  AssignFile(data, ExpandFileName(FileName));
  Rewrite(data);
  writeln(data, '#mega');
  writeln(data, '!Title ' + SeqList.Title + ';');
  if SeqList.IsDNA then
    if SeqList.IsProteinCoding then
      {$IFDEF VISUAL_BUILD}
      if MegaForm.CodeTableName <> EmptyStr then
      {$ELSE}
      if D_MegaMain.CodeTableName <> EmptyStr then
      {$ENDIF}
      begin
        {$IFDEF VISUAL_BUILD}
        str := MegaForm.CodeTableName;
        {$ELSE}
        str := D_MegaMain.CodeTableName;
        {$ENDIF}
        while Pos(' ', str) > 0 do
          str[Pos(' ', str)] := '_';
        writeln(data, '!Format DataType=DNA indel=- CodeTable='+str+';')
      end
      else
        writeln(data, '!Format DataType=DNA indel=-;')
    else
      writeln(data, '!Format DataType=DNA indel=-;')
  else
    writeln(data, '!Format DataType=Protein indel=-;');
  writeln(data);

  if SeqList.IsDNA and SeqList.IsProteinCoding then
    writeln(data, '!Domain=Data property=Coding CodonStart=1;');

  for i := 0 to SeqList.Count-1 do
  begin
    str := SeqList[i].SeqName;
    while Pos(' ', str) > 0 do
      str[Pos(' ', str)] := '_';
    if SeqList[i].SeqInfo <> '' then
      str := str+' "'+SeqList[i].SeqInfo+'"';
    writeln(data, '#'+str);
    str := SeqList[i].SeqData;
    if SeqList.IsDNA then
    begin
      for j := 1 to length(str) do
        if upcase(str[j]) = 'N' then str[j] := '?';
    end
    else
      for j := 1 to length(str) do
        if upcase(str[j]) = 'X' then str[j] := '?';
    for j := 0 to length(str) div 60 do
      writeln(data, System.Copy(str, j*60+1, 60));
      writeln(data, StringOfChar('-', SeqList.MaxNoOfSites-SeqList[i].NoOfSites))
  end;
  CloseFile(data);
end;

/// <summary>Given a sequence list, write to a Fasta file</summary>
procedure TD_Align.WriteFastaFile(SeqList: TSequenceList; FileName: String);
var
  j: integer;
  i: integer;
  str : AnsiString;
  data : TextFile;
begin
  AssignFile(data, FileName);
  Rewrite(data);

  for i := 0 to SeqList.Count-1 do
  begin
    str := SeqList[i].SeqName;
    writeln(data, '>'+str);
    str := SeqList[i].SeqData;
    for j := 0 to length(str) div 60 do
      writeln(data, System.Copy(str, j*60+1, 60));
      writeln(data, StringOfChar('-', SeqList.MaxNoOfSites-SeqList[i].NoOfSites))
  end;
  CloseFile(data);
end;

/// <summary>Writes the aligned sequences to a MEGA or Fasta file as specified by the user</summary>
procedure TD_Align.ClustalWriteResults(Sender : TObject);
{$IFNDEF VISUAL_BUILD}
var
  i: integer;
{$ENDIF}
begin
 {$IFNDEF VISUAL_BUILD}
 //NOTE: WHEN CHANGING THIS FUNCTION, ALSO CHANGE "ClustalUnTranslateAndWriteResults"
 if Not WriteAlignmentsToFile then
 begin
   Exit;
 end;

 for i := 0 to ClustalW.Sequences.Count-1 do    //Place the aligned sequences into the seqlist overwriting unaligned seqs
   ASeqList[i].SeqData := ClustalW.Sequences[i];
  if (D_MegaMain.OutputFormat = ExportFasta) then
    WriteFastaFile(ASeqList, NextAvailableFilenameNV('.fasta'))
  else
   WriteMEGAFile(ASeqList, NextAvailableFilenameNV('.meg'));    //Write the aligned sequences into a MEGA file
  try
    NumActiveThreadsCS.Acquire;
    RunningThreadCount := RunningThreadCount - 1;
  finally
    NumActiveThreadsCS.Release;
  end;

 {$ENDIF}
end;

/// <summary> Untranslates the aligned sequences and writes the sequences
/// to a .meg or .fasta file as specified by the user</summary>
procedure TD_Align.ClustalUnTranslateAndWriteResults(Sender : TObject);
{$IFNDEF VISUAL_BUILD}
var
  i: integer;
{$ENDIF}
begin
{$IFNDEF VISUAL_BUILD}
   //NOTE: WHEN CHANGING THIS FUNCTION, ALSO CHANGE "ClustalWriteResults"
   if Not WriteAlignmentsToFile then
   begin
     Exit;
   end;

   for i := 0 to ClustalW.Sequences.Count-1 do //Place the aligned sequences back into the seqlist overwriting unaligned seqs
     ASeqList[i].SeqData := ClustalW.Sequences[i];
   ASeqList.UnTranslate; //Untranslate the cDNA sequences
   if (D_MegaMain.OutputFormat = ExportFasta) then
     WriteFastaFile(ASeqList, NextAvailableFilenameNV('.fasta'))
   else
     WriteMEGAFile(ASeqList, NextAvailableFilenameNV('.meg')); //Write the untranslated aligned sequences into a MEGA file
   try
     NumActiveThreadsCS.Acquire;
     RunningThreadCount := RunningThreadCount - 1;
   finally
     NumActiveThreadsCS.Release;
   end;
  {$ENDIF}
end;

/// <summary> Factory method that calls the proper procedure for loading
/// an alignment file based on the file type</summary>
procedure LoadAlignmentDataFromFile(FileName : String; FDataType: TSnTokenCode);
begin
  if D_Align <> nil then
    Exit;
  D_Align := TD_Align.Create;
  if (CompareText(ExtractFileExt(filename), MasExt1) = 0) or (CompareText(ExtractFileExt(filename), MasExt2) = 0) then
    Raise Exception.Create('PROGRAMING ERROR: This function should not be used to re-load an Alignment Session file')
  else if CompareText(ExtractFileExt(fileName), '.meg') = 0 then
  begin
    if not D_Align.OpenAMEGAFile(fileName, true) then
      Exit;
  end
  else
  begin
    if not D_Align.LoadThirdPartyFileFormat(FileName, True, FDataType) then
      Exit;
  end;
  {$IFNDEF VISUAL_BUILD}
  D_MegaMain.DataFileName := FileName;
  {$ENDIF}
end;

procedure LoadAlignmentDataFromActiveSeqs(Seqs: TSequenceList);
begin
  if D_Align <> nil then
    exit;
  D_Align := TD_Align.Create;
  if D_Align.OpenFromActiveSeqs(Seqs, true) then

  else
    exit;
  {$IFNDEF VISUAL_BUILD}
  //D_MegaMain.DataFileName := filename;
  {$ENDIF}
end;


function TD_Align.OpenAFastaFile(AFileName: String; Overwrite: boolean): boolean;
begin
  ASeqList := nil;

  try
    Result := ProcessInputFastaFile(AFileName, False);
    if not Result then
    begin
      {$IFDEF VISUAL_BUILD}
      Raise Exception.Create('Failed to parse input Fasta file');
      {$ELSE}
      Error_NV('Failed to parse input Fasta file');
      {$ENDIF}
    end;
    ASeqList := D_InputSeqData.GenerateTSequenceList;

  except
    On E:Exception do
    begin
      Assert(False, 'Error opening a fasta file: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TD_Align.OpenAMegaFile(AFileName : String; Overwrite : boolean): boolean;
begin
  ASeqList := nil;

  try
    Result := ProcessInputMegaFile(AFileName, False);
    if not Result then
    begin
      {$IFDEF VISUAL_BUILD}
      Raise Exception.Create('Failed to parse input MEGA file');
      {$ELSE}
      Error_NV('Failed to parse input MEGA file');
      {$ENDIF}
    end;
    ASeqList := D_InputSeqData.GenerateTSequenceList;
  except
    On E:Exception do
    begin
      Result := False;
      Assert(False, 'Error opening a mega file: ' + E.Message);
    end;
  end;

end;

function TD_Align.OpenFromActiveSeqs(SeqList : TSequenceList; Overwrite : boolean): boolean;
begin
  Result := False;
  ASeqList := nil;
  try
    ASeqList := SeqList;
  except
    On E:Exception do
    begin
      Result := False;
      Assert(False, 'Error in OpenFromActiveSeqs: ' + E.Message);
    end;
  end;
end;

function TD_Align.LoadThirdPartyFileFormat(InFile : String; Overwrite : boolean; FDataType: TSnTokenCode): boolean;
var
  Converter : TFormatConvertToMega = nil;
  Seq : TSequence = nil;
  i,j,na,nn : Integer;
  isDNA: boolean;
  aName: AnsiString;
begin
  result := false;
  ASeqList   := nil;

  try
    Converter := TFormatConvertToMega.Create(InFile);
    Converter.UpdateFileType;
    Converter.Convert(Converter.InFileType);
    ASeqList := TSequenceList.Create;
    if Converter.SeqDataHash.Count <> 0 then
    begin
      with Converter do
        for i := 0 to SeqDataHash.Count - 1 do
        begin
          Seq := TSequence.Create;
          aName := SeqDataHash.Names[i];
          TrimTaxaName(aName);
          Seq.SeqName := aName;
          Seq.SeqData := SeqDataHash.Values[SeqDataHash.Names[i]];
          ASeqList.Add(Seq);
          Seq := nil;
        end;

      if ASeqList.Count = 0 then
        exit;

      nn := 0;
      na := 0;
      if ASeqList.Count > 0 then
        for i := 0 to ASeqList.Count-1 do
          for j := 1 to ASeqList[i].NoOfSites do
            if Upcase(ASeqList[i].Base[j]) in DNAbases then
              inc(nn)
            else if Upcase(ASeqList[i].Base[j]) in AABases then
              inc(na);
      if FDataType <> snNoToken then // If the user forces the data type by command line use that!
      begin
        isDNA := (FDataType = snNucleotide) or (FDataType = snCoding);
//        ASeqList.IsProteinCoding := (FDataType = snCoding);
        {$IFNDEF VISUAL_BUILD}
        ASeqList.IsProteinCoding := D_MegaMain.FFileShouldContainCoding;
        {$ENDIF}
      end
      else
      begin
        isDNA := nn > 9*na;
        {$IFNDEF VISUAL_BUILD}
        ASeqList.IsProteinCoding := D_MegaMain.FFileShouldContainCoding;
        {$ELSE}
        ASeqList.isProteinCoding := isDNA;
        {$ENDIF}
      end;
      ASeqList.isDNA := isDNA;  // ASeqList wasn't having it's isDNA properly set, it always defaulted to true.

      

      result := true;
    end;


  finally
    if Assigned(Converter) then
      Converter.Free;
    {if Assigned(ASeqList) then  //we will use this
      ASeqList.Free; }
    if Assigned(Seq) then
      Seq.Free;
  end;
end;

procedure MuscleFinished(AppLink : TAppLink);
const
  OFFSET = 2;
var
  PhyloQSequence: TSequence;
  MuscleLink : TMuscleLink;
  TempFastaList, NameOrder, ResultList: TStringList;
  i,j,SeqNum, LocOfSeqI : Integer;
  ResultStr, InsertName, ToAdd, XSites: AnsiString;
  seq: TSequenceList;
  NumSeqsFound: Integer;
  str: AnsiString;
begin
  try
    MuscleLink := TMuscleLink(AppLink);
    if MuscleLink.AppExitCode = USER_ABORTED then
    begin
      //ShowMessage('MUSCLE sequence alignment aborted');
    end
    else
    begin
      MuscleLink.CheckUnhandledError;  // check that output exists, log file has no errors, etc.
      case MuscleLink.ErrorCode of
         noError:
           begin
             TempFastaList := TStringList.Create;
             TempFastaList.LoadFromFile(MuscleLink.OutputFilename);
             ResultList := TStringList.Create;
             NameOrder := TStringList.Create;
             ResultStr := TempFastaList.Text;
             seq := D_Align.ASeqList; // Might have to create the seq object first before assigning.
             Delete(ResultStr, 1, pos('>', ResultStr)-1); // Delete any junk before the first >
             NumSeqsFound := 0; // for the case of profile alignment, the last sequence in the alignment will be the extra one
             While ResultStr <> EmptyStr do
             begin
               InsertName := Copy(ResultStr, pos('>', ResultStr)+1, pos(LineEnding, ResultStr) - OFFSET);  // Grabs from '>' to EOL { TODO 1 -oglen -clazarus : test this for windows (changed #10 to LineEnding) }
               if NumSeqsFound < Seq.Count then
                 SeqNum := StrToInt(Copy(InsertName, 3, Length(InsertName)))  // Turns TX1 into 1, etc.
               else
                 SeqNum := NumSeqsFound;

               if (SeqNum < seq.count) and (SeqNum >= 0)  then
                 InsertName := seq[SeqNum].SeqName; // Get the real taxa name, number corelates to the index in seq.
               NameOrder.Add(InsertName);  // Add the real taxa name to NameOrder in the order it's listed in the TempFastaList file.
               Delete(ResultStr, pos('>', ResultStr), pos(LineEnding, ResultStr)); // Delete the name we just processed
               if pos('>', ResultStr) = 0  then // if we've reached the last taxa, as there are no more '>' to denote the starting of a new name left
               begin
                 ToAdd := Copy(ResultStr, 1, Length(ResultStr));
                 {$IFDEF MSWINDOWS}
                 ToAdd := Trim(ToAdd);
                 {$ENDIF}
                 Delete(ResultStr, 1, Length(ResultStr));
               end
               else
               begin
                 ToAdd := Copy(ResultStr, 1, pos('>', ResultStr)-1); // Copy off the sequence (everything until the next '>')
                 {$IFDEF MSWINDOWS}
                 ToAdd := Trim(ToAdd);
                 {$ENDIF}
                 Delete(ResultStr, 1, pos('>', ResultStr)-1); // Delete the sequence (everything we just copied to ToAdd)
               end;
               ResultList.Add(ReplaceRegExpr(LineEnding, ToAdd, EmptyStr, False)); // Add the sequence to the List
               inc(NumSeqsFound);
             end;

             // ADDDDDDDDDDDDDDDDDDDDDDD

            if NameOrder.Count <> Seq.Count then
            begin
              if not Applink.MAI.MyProcessPack.ContainsProcessType(ppPhyloQ) then
                MuscleLink.AddWarning('Warning: The number of taxa which MUSCLE returned differs from the number of selected taxa in MEGA.' + #10#13 + 'This can be happen if for example one sequence was completely empty, or MUSCLE eliminated it for some reason.');
              // Compare out Names to the selected taxa and see WHERE we are missing a taxa.

              // The previous strategy assumes that the sequences will be returned in order.  This was fine until we were forced to remove the -stable flag due to a known error in muscle when it is used.
              for i := 0 to Seq.Count-1 do
              begin
                if NameOrder.IndexOf(Seq.Items[i].SeqName)= -1 then
                begin
                  // In this case we have a name in the origional unaligned sequence list which isn't in the aligned version.
                  MuscleLink.AddWarning('The selection for sequence named ''' + Seq.Items[i].Seqname + ''' was removed from the final MUSCLE alignment, because it was empty or contained only gaps.  MEGA added the sequence back in for you, but you may want to remove it if it''s not necessary.');
                  ResultList.Insert(i, StringOfChar('-', length(Seq.Items[i].SeqData) ));
                  NameOrder.Insert(i, Seq.Items[i].SeqName);
                end;
              end;
            end;

            // Since the -stable flag was depreciated we have to shuffle the sequences back into their original order.  This is necessary for the XSites as well as not confusing the user.
            for i := 0 to Seq.Count-1 do
            begin
              LocOfSeqI := NameOrder.IndexOf(Seq[i].SeqName);
              if LocOfSeqI > -1 then
              begin
                NameOrder.Move(LocOfSeqI, i);
                ResultList.Move(LocOfSeqI, i);
              end;
            end;
            XSites := MuscleLink.GetXSites;
            // Before we can reaplace the aligned sequences we need to replace 'X' with their specific char we saved before alignment
            if XSites <> EmptyStr then
              for i := 0 to ResultList.Count-1 do
              begin
                str := ResultList.Strings[i];
                for j := 1 to Length(str) do
                  if (MuscleLink.IsDNA and (str[j] = 'N')) or ((not MuscleLink.IsDNA) and (str[j] = 'X')) then
                  begin
                    str[j] := XSites[1];
                    Delete(XSites, 1, 1);
                  end;
                ResultList.Strings[i] := str;
              end;
             // END OF UPDATES

             if (Applink.MAI.MyProcessPack.ContainsProcessType(ppPhyloQ)) and (ResultList.Count = Seq.Count + 1) then
             begin
               for i := 0 to ResultList.Count-2 do
               begin
                 D_Align.ASeqList[i].SeqData := ResultList.Strings[i];
                 D_Align.ASeqList[i].SeqName := NameOrder.Strings[i]; // GS - added for profile alignments so that the extra sequence name gets included
               end;
               PhyloQSequence := TSequence.Create;
               PhyloQSequence.SeqData := ResultList.Strings[ResultList.Count - 1];
               PhyloQSequence.SeqName := NameOrder.Strings[NameOrder.Count - 1];
               D_Align.ASeqList.Add(PhyloQSequence);
             end
             else
             begin
               for I := 0 to ResultList.Count-1 do
               begin
                 D_Align.ASeqList[i].SeqData := ResultList.Strings[i];
                 D_Align.ASeqList[i].SeqName := NameOrder.Strings[i]; // GS - added for profile alignments so that the extra sequence name gets included
               end;
             end;
             if not AppLink.MAI.MyProcessPack.ContainsProcessType(ppPhyloQ) then
             begin
               AppLink.MAI.ARP.SetProgress(100);
             end;

             if (not AppLink.MAI.MyProcessPack.ContainsProcessType(ppPhyloQ)) and (MuscleLink.IscDNA) then
             begin
               AppLink.MAI.ARP.UpdateRunStatusInfo('Status', 'Untranslating MUSCLE Results...');
               AppLink.MAI.ARP.SetProgress(10); // if set to zero, nothing will be reported
             end;

             if MuscleLink.IscDNA then //Un-translate cDNA here
               D_Align.ASeqList.UnTranslate;

             if not AppLink.MAI.MyProcessPack.ContainsProcessType(ppPhyloQ) then
               AppLink.MAI.ARP.SetProgress(100);

             if AppLink.MAI.MyUsrOperation = dtdoPhyloQ then
             begin
                // in this case we don't want to write to file but rather we need the alignment for our analysis
             end
             else
             begin
               AppLink.MAI.ARP.UpdateRunStatusInfo('Status', 'Writing alignment file...');
               AppLink.MAI.ARP.SetProgress(10); // if set to zero, nothing will be reported
               {$IFNDEF VISUAL_BUILD}
               if (D_MegaMain.OutputFormat = ExportFasta) then
                 D_Align.WriteFastaFile(D_Align.ASeqList, NextAvailableFilenameNV('.fasta'))
               else
                 D_Align.WriteMEGAFile(D_Align.ASeqList, NextAvailableFilenameNV('.meg'));
               {$ENDIF}
               AppLink.MAI.ARP.SetProgress(100);
               AppLink.MAI.ARP.UpdateRunStatusInfo('Status', 'MUSCLE alignment completed successfully!');
               AppLink.MAI.ARP.SetProgress(100);  { because ARP will set progress to zero when the status string changes}
               {$IFNDEF VISUAL_BUILD}
               WriteLn();
               {$ENDIF}
           end;
         end
         else
         begin
           {$IFNDEF VISUAL_BUILD}
            warn_NV('Error: ' + MuscleLink.ErrorCodeMessage);
           {$ENDIF}
         end;
      end;
    end;
  finally
    if Assigned(MuscleLink.MAI) and Assigned(MuscleLink.MAI.ARP) then
      MuscleLink.MAI.ARP.Hide;
    {$IFNDEF VISUAL_BUILD} { This file is needed for the workflow system still}
    if FileExists(MuscleLink.OutputFilename) then
      DeleteFile(MuscleLink.OutputFilename);
    {$ENDIF}
    if FileExists(MuscleLink.InputFilename) then
      DeleteFile(MuscleLink.InputFilename);
    if FileExists(MuscleLink.LogFilename) then
      DeleteFile(MuscleLink.LogFilename);
    {$IFNDEF VISUAL_BUILD}
    D_MegaMain.AnalysisSummary.WriteToFile(ChangeFileExt(NextAvailableFileNameNV('.txt'), '_summary.txt'));
    try
      NumActiveThreadsCS.Acquire;
      RunningThreadCount := RunningThreadCount - 1;
    finally
      NumActiveThreadsCS.Release;
    end;
    {$ENDIF}
  end;
end;


end.

