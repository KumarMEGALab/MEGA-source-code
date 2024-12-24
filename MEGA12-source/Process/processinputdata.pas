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

unit ProcessInputData;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFNDEF VISUAL_BUILD}MegaUtils_NV, mlexbase,{$ENDIF}
  FileUtil, KeywordConsts, Forms, Classes, MLongintList, MegaConsts,
  MD_Sequences, MDataFileInfo, nexusfile, nexusblock,
  MLexSeq, MRuntimeProgressDlg, MLexDist, MDomainInfo;

procedure LaunchConcatenateAlignmentsThread(SourceDir: String; aCheckCancel: TCheckCancelFunc; ARP: TRuntimeProgress); overload;
procedure LaunchConcatenateAlignmentsThread(aFileList: TStringList; aCheckCancel: TCheckCancelFunc; ARP: TRuntimeProgress); overload;
function ProcessDataSessionFile(FileName: String; var Seqs: TSequenceList): Boolean; // specify Seqs only if you want to have the TSequenceList filled and the data file NOT activated by MEGA for analysis.
function DoOpenDataFile(FileName: String; OpenInSDE: Boolean=True; aNotify: TNotifyEvent=nil):Boolean;
function readStringFromFile(data : TStream):AnsiString;
function readBooleanFromFile(data : TStream):boolean;
function ProcessInputFastaFile(FileName: String; OpenInSDE: Boolean=True; aNotify: TNotifyEvent=nil): Boolean;
function ProcessInputNexusFile(Filename: String; OpenInSDE: Boolean=True; aNotify: TNotifyEvent=nil): Boolean;
function ProcessInputMegaFile(FileName: String; OpenInSDE: Boolean=True; aNotify: TNotifyEvent=nil): Boolean; overload;
function ProcessInputMegaFile(FileStrings: TStringList; OpenInSDE: Boolean=False; Filename: String = ''): Boolean; overload;
function DeriveDataDirectoryInfo(SourceDir: String): TDataFileInfo;
function DeriveDataFileInfo(MyDataFileName: String; OpenInSDE: Boolean=True; aNotify: TNotifyEvent=nil):TDataFileInfo; overload;
function DeriveDataFileInfo(FileStrings: TStringList; Filename: String = ''):TDataFileInfo; overload;
function PromptUserForDataFileInfo(aInfo: TDataFileInfo; OpenInSDE: Boolean=True): Boolean;
procedure ReadFormatValsFromMegaSeqFile(FileName: String;
                                        var DataType: String;
                                        var IdentSym: String;
                                        var MissingSym: String;
                                        var GapSym: String;
                                        var FoundFormat: Boolean); overload;
procedure ReadFormatValsFromMegaSeqFile(aList: TStringList;
                                        var DataType: String;
                                        var IdentSym: String;
                                        var MissingSym: String;
                                        var GapSym: String;
                                        var FoundFormat: Boolean); overload;
//function ReadAlleleFreqFile(FileName: String; HasPara: Boolean; ADataType: TSnTokenCode): Boolean;
function ReadFastaFile(DataFileInfo: TDataFileInfo; OpenInSDE: Boolean=True): Boolean;
function ReadSequenceFile(FileName: String; HasPara: Boolean; ADataType, ADataFormat: TSnTokenCode; AMissSym, AIdenSym, AGapSym: AnsiChar; OpenInSDE: Boolean=True): Boolean; overload;
function ReadSequenceFile(FileStrings: TStringList; HasPara: Boolean; ADataType, ADataFormat: TSnTokenCode; AMissSym, AIdenSym, AGapSym: AnsiChar; OpenInSDE: Boolean=True): Boolean; overload;
function DoReadSequenceFile(MyParser: TLexSeq; ARP: TRuntimeProgress; HasPara: Boolean; aDataType, aDataFormat: TSnTokenCode;  AMissSym, AIdenSym, AGapSym: AnsiChar; OpenInSDE: Boolean=True): Boolean;
function ReadDistFile(FileName: String; HasPara: Boolean; ADataType, ADataFormat: TSnTokenCode): Boolean; overload;
function ReadDistFile(FileStrings: TStringList; HasPara: Boolean; ADataType, ADataFormat: TSnTokenCode): Boolean; overload;
function DoReadDistFile(MyParser: TLexDist; ARP: TRuntimeProgress; HasPara: Boolean; ADataType, ADataFormat: TSnTokenCode): Boolean;
function InitSeqDataParser(FileName: String; var ARP: TRuntimeProgress): TLexSeq; overload;
function InitSeqDataParser(var FileStrings: TStringList; var ARP: TRuntimeProgress): TLexSeq; overload;
function InitDistDataParser(var FileStrings: TStringList; var ARP: TRuntimeProgress): TLexDist;
procedure SetupConcatenatedSequencesInformation(seqs: TSequenceList; dInfo: TAllDomainInfo; ARP: TRuntimeProgress; OpenInSDE: Boolean=True);



var
  CurrentDataFileInfo: TDataFileInfo;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  MInputDataOptionsDlg, Mega_Main, MSelectGeneticCodeDlg, MV_SeqDataExplorer, MTaxaGpsDlg,
  MGeneDomainDlg, MEditorForm, MV_DistDataExplorer, htmloptionsdlg, ContextHelp_HC,
  MAlignEditMainForm, mega_info_form,
  {$ELSE}
  MD_MegaMain, MProcessPack,
  {$ENDIF}
  LCLIntf, LCLType,  SysUtils, Graphics, Controls, Dialogs,
  Buttons, StdCtrls, MegaUtils, MegaErrUtils, ErrorMessages_HC,
  MegaPrivateFiles,  MOtuInfo,
  MVS_SeqDataExplorer, MD_InputSeqData, MD_InputDistData, MVS_DistDataExplorer,
  MegaVerConsts, Math, mcrossprojectutils, StrUtils, ShowTrees,
  StringUtils, MFileUtils, alignmentconcatenator, mbrowserutils, MPleaseWait,
  dateutils, mchar_scanner;

function ProcessAlignmentSessionFile(FileName: String): Boolean; forward;
function ReadNexusFile(DataFileInfo: TDataFileInfo; OpenInSDE: Boolean=True): Boolean; forward;
function ParseFastaData(DataFileInfo: TDataFileInfo; var ErrorRow: Integer; ARP: TRuntimeProgress; OpenInSDE: Boolean=True): Boolean; forward;
function ParseNexusData(DataFileInfo: TDataFileInfo; var ErrorRow: Integer; var ErrorCol: Integer; var Msg: String; ARP: TRuntimeProgress; OpenInSDE: Boolean=True): Boolean; forward;
function RetrieveMegaAlignmentSession(FileName: String; var ErrorRow: Integer; ARP: TRuntimeProgress): Boolean; forward;
procedure SetupSequenceInformation(Parser: TLexSeq; ARP: TRuntimeProgress; OpenInSDE: Boolean=True); forward;
procedure SetupNexusSequenceInformation(NexusFile: TNexusFile; ARP: TRuntimeProgress; OpenInSDE: Boolean = True); forward;
procedure SetupFastaSequenceInformation(DataFileInfo: TDataFileInfo; NoOfOtus, NoOfSites: Integer; Taxon, Sequence: Array of PAnsiChar; ARP: TRuntimeProgress; OpenInSDE: Boolean=True); forward;
procedure SetupMASSequenceInformation(FileName: String; NoOfOtus, NoOfSites: Integer; DataType: TSnTokenCode; SeqList: TSequenceList; CodeTableName: AnsiString; CodeTable: AnsiString; GapSym, MissingSym, IdenticalSym: AnsiChar; ARP: TRuntimeProgress); forward; deprecated; // not used in MEGA
function OpenSequenceDataSession(data: TStream; Version : integer; ARP : TRuntimeProgress; filename: String): boolean; forward;
function RetriveSequenceFromDataSession(data: TStream; Version : integer; ARP : TRuntimeProgress; Seqs: TSequenceList): boolean; forward;
procedure SetupDistanceInformation(Parser: TLexDist; ARP: TRuntimeProgress); forward;
function OpenDistanceDataSession(data: TStream; Version : integer; ARP : TRuntimeProgress): boolean; forward;
function MessageStringForDisabledSeqDataExplorer(dTypeStr: String; numSeqs: QWord; numSites: QWord): String; forward;


function DoOpenDataFile(FileName: String; OpenInSDE: Boolean=True; aNotify: TNotifyEvent=nil): Boolean;
var
  UnusedSeqList: TSequenceList;
begin
  Result := False;
  if CompareText(ExtractFileExt(FileName), '.mdsx') = 0 then  //see if it is a data session file
  begin
    UnusedSeqList := nil;
    if not ProcessDataSessionFile(FileName, UnusedSeqList) then //Process the data session file seperately from Meg files
      Exit;
  end
  else if (CompareText(ExtractFileExt(FileName), MtsExt1) = 0) or (CompareText(ExtractFileExt(FileName), MtsExt2) = 0) then
  begin
    if ShowTreeFromFile(Filename) = nil then
      Exit
  end
  else if (CompareText(ExtractFileExt(FileName), MasExt1) = 0) or (CompareText(ExtractFileExt(FileName), MasExt2) = 0) then
  begin
    if not ProcessAlignmentSessionFile(FileName) then
      Exit;
  end
  else if Pos(ExtractFileExt(FileName), FastaExts) > 0 then
  begin
    if not ProcessInputFastaFile(FileName, OpenInSDE, aNotify) then
      Exit;
  end
  else if Pos(ExtractFileExt(FileName), PaupExts) > 0 then
  begin
    if not ProcessInputNexusFile(Filename, OpenInSDE, aNotify) then
      Exit;
  end
  else
  begin
    if not ProcessInputMegaFile(FileName, OpenInSDE, aNotify) then
      Exit;
  end;

  {$IFDEF VISUAL_BUILD}
  if not isPrototyper then { otherwise we will be switching into the app data directories}
    SwitchDirectory(FileName);
  {$ENDIF}
  SetActiveDataFileName(FileName);
  Result := True;
end;

procedure LaunchConcatenateAlignmentsThread(SourceDir: String; aCheckCancel: TCheckCancelFunc; ARP: TRuntimeProgress);
var
  aThread: TAlignConcatThread = nil;
  fileInfo: TDataFileInfo = nil;
begin
  try
    fileInfo := DeriveDataDirectoryInfo(SourceDir);
    if Assigned(fileInfo) then
    begin
      if (not (FileInfo.GetDataType = snNucleotide)) and (not (FileInfo.GetDataType = snProtein)) then
        raise Exception.Create('alignment concatenation utility is only available for sequence alignment data');
      aThread := TAlignConcatThread.Create(SourceDir, fileInfo);
      aThread.CheckCancel := aCheckCancel;
      aThread.UpdateRunStatusInfoProc := ARP.UpdateRunStatusInfo;
      {$IFDEF VISUAL_BUILD}
      aThread.OnTerminate := MegaForm.ConcatenateFilesDone;
      ARP.Show;
      {$ELSE}
      aThread.OnTerminate := D_MegaMain.ConcatenateAlignmentsDone;
      {$ENDIF}
      aThread.Start;
    end;
  except
    on E:EAbort do
    begin
      {$IFDEF VISUAL_BUILD}
      ShowMessage(E.Message);
      {$ELSE}
      error_nv('Failed to concatenate alignments', E);
      {$ENDIF}
    end;
    on E:Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Failed to concatenate alignments: ' + E.Message);
      {$ELSE}
      error_nv('Failed to concatenate alignments', E);
      {$ENDIF}
    end;
  end;
end;

procedure LaunchConcatenateAlignmentsThread(aFileList: TStringList; aCheckCancel: TCheckCancelFunc; ARP: TRuntimeProgress);
var
  aThread: TAlignConcatThread = nil;
  fileInfo: TDataFileInfo = nil;
begin
  try
    if (not (aFileList.Count > 0)) or (not FileExists(aFileList[0])) then
      raise Exception.Create('no files in supplied list were found');
    fileInfo := DeriveDataFileInfo(aFileList[0]{$IFNDEF VISUAL_BUILD}, False{$ENDIF});
    if Assigned(fileInfo) then
    begin
      if (FileInfo.GetDataFileFormat = dfFasta) and (FileInfo.GetDataType = snNoToken) then
        raise Exception.Create('when concatenating alignment files, you must specify a data type(e.g. --nuc, --amino, or --coding)');
      if (not (FileInfo.GetDataType = snNucleotide)) and (not (FileInfo.GetDataType = snProtein)) then
        raise Exception.Create('alignment concatenation utility is only available for sequence alignment data');
      aThread := TAlignConcatThread.CreateWithList(aFileList, fileInfo);
      aThread.CheckCancel := aCheckCancel;
      aThread.UpdateRunStatusInfoProc := ARP.UpdateRunStatusInfo;
      {$IFDEF VISUAL_BUILD}
      aThread.OnTerminate := MegaForm.ConcatenateFilesDone;
      ARP.Show;
      {$ELSE}
      aThread.OnTerminate := D_MegaMain.ConcatenateAlignmentsDone;
      {$ENDIF}
      aThread.Start;
    end;
  except
    on E:EAbort do
    begin
      {$IFDEF VISUAL_BUILD}
      ShowMessage(E.Message);
      {$ELSE}
      error_nv('Failed to concatenate alignments', E);
      {$ENDIF}
    end;
    on E:Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Failed to concatenate alignments: ' + E.Message);
      {$ELSE}
      error_nv('Failed to concatenate alignments', E);
      {$ENDIF}
    end;
  end;
end;

function ProcessDataSessionFile(FileName: String; var Seqs: TSequenceList): Boolean;
var
  data2 : TFileStream;
  data : TMemoryStream;
  i: Integer = -1;
  size: Integer = -1;
  version: Integer = -1;
  ViewerType: integer = -1;
  ARP : TRuntimeProgress;
  Title, Description, BufferString : AnsiString;
  msg: String = '';
begin
  Result := false;
  data2 := nil;
  ARP := nil;
  try try
      try
        data2 := TFileStream.Create(filename, fmOpenRead);
      except
        on E : Exception do
        begin
          Raise(Exception.Create('Unable to open Mega Session file for reading : ' + E.Message));
          exit;
        end;
      end;

      ARP := TRuntimeProgress.Create(Application);
      ARP.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Reading Saved Session File';
      ARP.AddRunStatusInfo('Session File', FileName);
      ARP.AddRunStatusInfo('Status', 'Reading header');
      ARP.HideAnalysisOptions;
      ARP.Show;
      Application.processmessages;
      data2.Read(i, 4);
      //if i = 67324752 then //Compressed mdsx file
     { if i = 134777631 then
      begin
        size := data2.Size;
        data := TMemoryStream.Create;
        data.SetSize(size);
        FreeAndNil(data2);
        DeCompress := TAbZipKit.Create(nil);
        DeCompress.ArchiveType := atGzip;
        DeCompress.ForceType := True;
        DeCompress.LogFile := 'c:\unziplog.txt';
        DeCompress.Logging := True;
        DeCompress.OpenArchive(FileName);
        DeCompress.ExtractToStream('Data.dat', data);
      end
      else
      begin}
        size := data2.Size;
        data := TMemoryStream.Create;
        data.SetSize(size);
        data.LoadFromStream(data2);
      {end;}

      SetLength(BufferString, 4);
      data.Read(BufferString[1], 4);
      if BufferString = '#BAD' then
        Raise(Exception.Create('The file selected is a MEGA session file, but appears to have been not fully saved when it was created'))
      else if BufferString <> '#MDS' then  //Check if it says #MDS at beginning of file
      begin
        Raise(Exception.Create('The file selected does not appear to be a Mega Session File, it may be corrupt'));
      end;

      data.Read(version, 4);

      if version < MSDX_SESSION_VERSION then
      begin
        ARP.Hide;
        if not IsSessionTest then
          ShowWarningMessage('This Mega Session file was created with an older version of Mega; only settings that are compatable with this version of Mega will be restored');
      end;

     if version >= 1001 then
     begin
       data.Read(i, SizeOf(i));
       if not SessionIsSameTargetPlatform(i, msg) then
         raise Exception.Create(msg);
     end;

     data.Read(ViewerType, 4);

     i := 0;
     data.Read(i, 4);
     SetLength(Title, i);  // Fixed, was previously reading in an extra #0 which caused bugs to crop up.
     if i > 0 then
       data.Read(Title[1], i);

     data.Read(i, 4);
     setLength(Description, i);
     if i > 0 then
       data.Read(Description[1], i);
     Application.processmessages;
     if Seqs = nil then
     begin
       if ViewerType = 1 then //Sequence Data Viewer
         OpenSequenceDataSession(data, version, ARP, FileName)
       else if ViewerType = 2 then //Distance Data Viewer
         OpenDistanceDataSession(data, version, ARP)
       else
         raise(Exception.Create('Can not identify Type of Mega Session, please upgrade to the newest version of Mega to open this file'));
     end
     else
     begin
       if ViewerType = 1 then // Sequence file which we can read
       begin
         //read everything in the file until #MDS at the end of file, so that integrity checking a few lines down works, put sequences into seqs object.
         RetriveSequenceFromDataSession(data, version, ARP, Seqs); // MODIFY THIS
       end
       else
         ShowMessage('This is a distance file, it contains no sequences and can not be open for alignment.');
     end;
       ARP.Hide;
     Application.processmessages;
       //CHECK IF THERE IS THE ENDING #MDS
       BufferString := EmptyStr; // We need to empty this out otherwise it may retain the value from the previous integrity check and give a false negative.
       SetLength(BufferString, 4);
       data.Read(BufferString[1], 4);  // Needed to change data2 to data.  It was reading from the WRONG source.
       if (BufferString <> '#MDS') and (Seqs = nil) then
         Raise(Exception.Create('Data integrity marker at the end of the file was not found'));
       {$IFDEF VISUAL_BUILD}
       if (D_InputSeqData <> nil) and (D_InputSeqData.IsNuc) then
         MegaForm.UpdateMainWindow(FileName, Title, Description, snNucleotide)
       else if (D_InputSeqData <> nil) then
         MegaForm.UpdateMainWindow(FileName, Title, Description, snProtein)
       else
         MegaForm.UpdateMainWindow(FileName, Title, Description, snDistance);
       {$ENDIF}
      Result := True; //We setup everything without an exception
    except
      on E : Exception do
      begin
      {$IFDEF VISUAL_BUILD}
        if Assigned(V_SeqDataExplorer) then
        begin
          V_SeqDataExplorer.Hide;
          FreeAndNil(V_SeqDataExplorer);
        end
        else if Assigned(V_DistDataExplorer) then
        begin
          V_DistDataExplorer.Hide;
          FreeAndNil(V_DistDataExplorer);
        end;
        MegaForm.UpdateMainWindow;
        {$ENDIF}
        ShowErrorMessage(Exception.Create('Unable to open Mega Session file : ' + E.message));
      end;
    end;
  Finally
  FreeAndNil(data);
  FreeAndNil(data2);
  ARP.Hide;
  FreeAndNil(ARP);
  {$IFDEF VISUAL_BUILD}
  if IsSessionTest then
    MegaForm.SessionTestFinished(nil);
  {$ENDIF}
  end;
end;

function readStringFromFile(data : TStream):AnsiString;
var
  readLength : integer = -1;
  buffer : array of AnsiChar;
begin
Result := '';
  try
    data.Read(readLength, 4); //length of incoming string
    if readLength < 1 then
      exit;
    setlength(buffer, readLength+1);
    data.Read(buffer[0], readLength);
    Result := PAnsiChar(Buffer);

    //buffer[readLength] := #0;
    //result := buffer;
  except
    on E: Exception do
      Raise(Exception.create('Unable to read string from file Error: ' + E.Message));
  end;
end;

function readBooleanFromFile(data : TStream):boolean;
var
  i : integer;
begin
  i := 0;
  data.Read(i, 1);
  Result := (i = 1);
end;


procedure UpdatePercentProgress(data : TStream; ARP : TRuntimeProgress);
begin
  {$IFDEF VISUAL_BUILD}
  ARP.PercentGauge.Min := 0;
  ARP.PercentGauge.Max := data.Size;
  ARP.PercentGauge.Position := data.Position;
  Application.ProcessMessages;
  {$ENDIF}
end;

function OpenSequenceDataSession(data: TStream; Version : integer; ARP : TRuntimeProgress; filename: String): boolean;
var
  i: Integer = -1;
  j: Integer = -1;
  k: Integer = -1;
  l: Integer = -1;
  readLength: Integer = -1;
  nextDomain: Integer = -1;
  t : TSiteAttr = []; //A temporary variable to store the site attribute one at a time while being read in
  c : AnsiChar; //To read in chars such as the missing character, gap character, etc.
  Taxa, Temp, GroupsCSV, BufferString : AnsiString;

  TaxaList, SequenceList, GroupsList: TStringList;
  AOtuInfo         : TAllOtuInfo;
  TempOtu : TOtuInfo;
  te : TSiteAttrType;
  buffer : array of AnsiChar;
  TempChar, TempRsv, TempMarks : PAnsiChar;
  TempDomainInfo : TDomainInfo;
  TempAllDomainInfo : TAllDomainInfo;
  TempSRes: TSearchResult;
begin
  Result := False;

  AOtuInfo       := nil;


  ARP.UpdateRunStatusInfo('Status', 'Reading data from saved session');
  Application.ProcessMessages;
  VS_SeqDataExplorer := TVS_SeqDataExplorer.Create;
  D_InputSeqData := TD_InputSeqData.Create;
  D_InputSeqData.FDomainMarks := TAllSiteDomainMark.Create;

  {$IFDEF VISUAL_BUILD}
  V_SeqDataExplorer := TV_SeqDataExplorer.Create(Application);
  V_SeqDataExplorer.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Sequence Data Explorer (' + ExtractFileName(filename) + ')';
  {$ENDIF}

  data.Read(i,4); //How many records to skip (this can be adjusted so we can add more content at the beginning of the file in the future)
  data.Read(j,i); //Skip the indicated # of records
  UpdatePercentProgress(data, ARP);
  D_InputSeqData.IsNuc := readBooleanFromFile(data);
  data.Read(i, 1);
  D_InputSeqData.IsCoding := (i = 1);
  D_InputSeqData.IsAmino  := readBooleanFromFile(data);
  VS_SeqDataExplorer.isTranslated  := readBooleanFromFile(data);
  ARP.UpdateRunStatusInfo('Status', 'Reading in Taxonomy information');
  Application.ProcessMessages;
  //READ IN TAXA
  data.Read(readLength, 4);
  setlength(buffer, (readLength +1));
  data.Read(buffer[0], readLength);
  buffer[readLength] := #0;  //Add that stopping character so it can be turned into a string
  taxa := PAnsiChar(buffer);   //from array of char to a string
  TaxaList := TStringList.create;
  splitstr(taxa, ',', TaxaList);
  UpdatePercentProgress(data, ARP);

  //READ IN GROUPS
  GroupsCSV := readStringFromFile(data);
  GroupsList := TStringList.Create;
  splitstr(GroupsCSV, ',', GroupsList);
  UpdatePercentProgress(data, ARP);

  //SETUP OTUINFOS
  AOtuInfo := TAllOtuInfo.Create;
  AOtuInfo.NoOfOtus := TaxaList.Count;
  D_InputSeqData.NoOfTaxa := TaxaList.Count;
  UpdatePercentProgress(data, ARP);

  data.Read(i, 4);
  D_InputSeqData.NoOfSites := i;

  data.Read(i, 4);
  D_InputSeqData.NoOfSitesUsed := i;

  data.Read(i, 4);
  D_InputSeqData.FDomainMarks.NoOfSites := i;

  ARP.UpdateRunStatusInfo('Status', 'Reading in site attribute information');
  Application.ProcessMessages;
  //NUMBER OF SITE ATTRIBUTES IN SAVED FILE
  data.Read(i, 4);

  j :=0;
  //READ IN NUMBER OF SITES EACH SITE ATTRIBUTE HAS
  for te:=megNone to megLast do
  begin
    j := j +1;  //increment the number of sites we have read
    if j <= i then //Read in the next number of attribute sites if it is stored in the data file
      data.Read(D_InputSeqData.FNoOfAttrSites[te], 4)
    else
      D_InputSeqData.FNoOfAttrSites[te] := 0; //Otherwise we have to say there are none of that type as it does not exist in this version of mega
  end;

  UpdatePercentProgress(data, ARP);

  while(j < i) do   //In case we are opening a newer version of a data session that has nore attrSiteTyps
  begin
    j := j +1;
    data.Read(k, 4);   //read until we've read all the site types and are back in the right position
  end;
  UpdatePercentProgress(data, ARP);

  ARP.UpdateRunStatusInfo('Status', 'Reading in sequences');
  Application.ProcessMessages;
  //READ IN SEQUENCES
  SequenceList := TStringList.Create;
 // setlength(buffer, D_InputSeqData.NoOfSites+1);
  for j:=0 to TaxaList.Count -1 do
  begin
    SetLength(BufferString, D_InputSeqData.FDomainMarks.NoOfSites+1);
    data.Read(BufferString[1], D_InputSeqData.FDomainMarks.NoOfSites);
    SequenceList.Add(BufferString);
    UpdatePercentProgress(data, ARP);
  end;


  //MAKE AND FILL IN OTU OBJECTS
  TempChar := nil;
  for j := 0 to AOtuInfo.NoOfOtus -1 do
  begin
    TempOtu := TOtuInfo.Create;
      with TempOtu do
      begin
        Id     := j;
        Name   := TaxaList.Strings[j];
        GpName := GroupsList.Strings[j];
        OutgroupMember := SameText(GpName, 'outgroup');

        SpName := EmptyStr;
        PopName := EmptyStr;
        IsUsed := True;
        GetMem(TempChar, (D_InputSeqData.FDomainMarks.NoOfSites+1)*sizeOf(AnsiChar));

        TempChar := nil;
        // Allocate Allocate a PAnsiChar, copy char->AnsiChar.
        // +1 is the #0 to end the string.
        GetMem(TempChar, ( length(SequenceList.Strings[j])*sizeOf(AnsiChar) )+1);
        for l := 1 to length(SequenceList.Strings[j]) do
          TempChar[l-1] := AnsiChar(SequenceList.Strings[j][l]);
        Data := TempChar;
        TempChar := nil;
        GetMem(TempRsv, ((D_InputSeqData.FDomainMarks.NoOfSites div 3)+1)*sizeOf(AnsiChar));
        RsvData := TempRsv;
      end;
    AOtuInfo.X[j] := TempOtu;
  end;

  //REFERENCE SEQUENCE
  D_InputSeqData.ReferenceSeq := PAnsiChar(AOtuInfo[0].Data);

  D_InputSeqData.OtuInfos := AOtuInfo;  //Assign created OTU to the data part of sequence viewer

  c := #0;
  data.Read(c, 1);
  D_InputSeqData.GapSym  := c;

  c:=#0;
  data.Read(c, 1);
  D_InputSeqData.MissSym  := c;

  c:=#0;
  data.Read(c, 1);
  D_InputSeqData.IdenSym  := c;

  if (D_InputSeqData.IsNuc AND D_InputSeqData.IsCoding) then
  begin
    ARP.UpdateRunStatusInfo('Status', 'Reading in Codon Positions');
    Application.ProcessMessages;
    D_InputSeqData.AACodonStarts := TLongintList.Create;
    D_InputSeqData.AACodonPos2 := TLongintList.Create;
    D_InputSeqData.AACodonPos3 := TLongintList.Create;

    //READ IN CODON POSITIONS
    data.Read(readLength, 4);
    for j:=0 to readLength-1 do
    begin
      data.Read(i, 4);
      D_InputSeqData.AACodonStarts.Add(i);
    end;
    UpdatePercentProgress(data, ARP);

    data.Read(readLength, 4);
    for j:=0 to readLength-1 do
    begin
      data.Read(i, 4);
      D_InputSeqData.AACodonPos2.Add(i);
    end;
    UpdatePercentProgress(data, ARP);

    data.Read(readLength, 4);
    for j:=0 to readLength-1 do
    begin
      data.Read(i, 4);
      D_InputSeqData.AACodonPos3.Add(i);
    end;
  end;
  UpdatePercentProgress(data, ARP);

  ARP.UpdateRunStatusInfo('Status', 'Re-creating objects from data');
  //WHAT ORDER TO DISPLAY TAXA IN
  D_InputSeqData.DispTaxaList := TLongIntList.Create;
  data.Read(i, 4);//How many taxa

  for j :=0 to i -1 do
  begin
    data.Read(k, 4);
    D_InputSeqData.DispTaxaList.Add(k);
  end;

  UpdatePercentProgress(data, ARP);


  //WEITHER EACH TAXA IS USED OR NOT
  for j:=0 to D_InputSeqData.OtuInfos.NoOfOtus-1 do
    D_InputSeqData.FOtuInfos.Otu[j].IsUsed  := readBooleanFromFile(data);

  for j:=0 to D_InputSeqData.OtuInfos.NoOfOtus-1 do
    D_InputSeqData.FOtuInfos.Otu[j].IsHidden  := readBooleanFromFile(data);

  if Version >= 1012 then
  begin
    for j := 0 to D_InputSeqData.OtuInfos.NoOfOtus - 1 do
    begin
      Temp := readStringFromFile(data);
      if Temp <> NO_GEOGRAPHICAL_INFO then
        D_InputSeqData.FOtuInfos.Otu[j].GeographicalInfo.InitFromMegaFileCommandString(Temp);
    end;
  end;

  UpdatePercentProgress(data, ARP);

  //WHAT ATTRIBUTES EACH SITE HAS
  ARP.UpdateRunStatusInfo('Status', 'Reading in attributes of each site');
  GetMem(D_InputSeqData.SiteAttrArray, D_InputSeqData.FDomainMarks.NoOfSites*SizeOf(TSiteAttr));
  for j:=0 to D_InputSeqData.FDomainMarks.NoOfSites-1 do
  begin
    data.Read(t, 4);
    D_InputSeqData.SiteAttrArray[j] := t;
     if ((j mod 200) = 0) then
      UpdatePercentProgress(data, ARP);
  end;

  UpdatePercentProgress(data, ARP);

  data.Read(i, 4);
  D_InputSeqData.NoOfCodingSitesUsed := i;

  ARP.UpdateRunStatusInfo('Status', 'Reading in Domains');
  Application.ProcessMessages;
  //DOMAIN INFOS
  data.Read(k, 4);  //Number of saved DomainInfos
  for j:=0 to k-1 do
  begin
    TempDomainInfo := TDomainInfo.Create;
    TempDomainInfo.Name := readStringFromFile(data);
    TempDomainInfo.GeneName := readStringFromFile(data);
    TempDomainInfo.IsGene  := readBooleanFromFile(data);
    TempDomainInfo.IsCoding  := readBooleanFromFile(data);
    data.Read(i, 4);
    TempDomainInfo.CodonStart :=  i;
    data.Read(i, 4);
    TempDomainInfo.FromSite :=  i;
    data.Read(i, 4);
    TempDomainInfo.ToSite :=  i;
    i := 0;
    data.Read(i, 1);
    TempDomainInfo.IsUsed := (i = 1);

    for i := TempDomainInfo.FromSite to  TempDomainInfo.ToSite do
      D_InputSeqData.FDomainMarks.X[i] := TempDomainInfo;
    TempDomainInfo := nil;
  end;

  UpdatePercentProgress(data, ARP);
  //FDOMAIN MARKS

  data.Read(i, 4);  //read in length of site Marks for FDomainMarks
  GetMem(TempMarks, i+1);
  data.Read(TempMarks[0], i); //Read in Site Marks
  D_InputSeqData.FDomainMarks.SiteMarks := TempMarks;
  D_InputSeqData.FDomainMarks.IsDirty  := readBooleanFromFile(data);
  D_InputSeqData.FDomainMarks.IsUsedIndependent  := readBooleanFromFile(data);
  data.read(i, 4);
  D_InputSeqData.FDomainMarks.NoOfLabelledSites := i;
  D_InputSeqData.FDomainMarks.SiteMarkTypes.CommaText := readStringFromFile(data);
  UpdatePercentProgress(data, ARP);
  ARP.UpdateRunStatusInfo('Status', 'Reading in CodeTable');
  Application.ProcessMessages;
  D_InputSeqData.CodeName := readStringFromFile(data);
  Temp := readStringFromFile(data);
  if Temp <> '' then
  begin
    setLength(Temp, 64);
    D_InputSeqData.CodeTable := Temp;
  end;

  UpdatePercentProgress(data, ARP);

  data.Read(i, 4);
  D_InputSeqData.CurDomainInfo := D_InputSeqData.DomainMarks.X[i];
  VS_SeqDataExplorer.CurStatusBarStatistics := nil;
  VS_SeqDataExplorer.CurStatusBarStatistics := TStringList.Create;

  //Placeholder for now
  VS_SeqDataExplorer.CurStatusBarStatistics.Add(' ');
  VS_SeqDataExplorer.CurStatusBarStatistics.Add(' ');
  VS_SeqDataExplorer.CurStatusBarStatistics.Add(' ');
  VS_SeqDataExplorer.CurStatusBarStatistics.Add(' ');
  {$IFDEF VISUAL_BUILD}
  VS_SeqDataExplorer.CurStatusBarStatistics.OnChange := V_SeqDataExplorer.UpdateStatusBar;  //make sure we don't call the update status bar before we initialize it
  {$ENDIF}

  if D_InputSeqData.isNuc then
    D_InputSeqData.NoOfNucSites := D_InputSeqData.FDomainMarks.NoOfSites; // changed Feb, 20 to fixes number of sites if the session was saved as translated. (Implemented by DP, fixed by NP)

  ARP.UpdateRunStatusInfo('Status', 'Preparing visual');
  Application.ProcessMessages;
  {$IFDEF VISUAL_BUILD}
  TaxaGpsDlg := TTaxaGpsDlg.Create(Application);
  GeneDomainDlg := TGeneDomainDlg.Create(Application);
  {$ENDIF}
  TempAllDomainInfo := nil;
  TempAllDomainInfo := TAllDomainInfo.Create;
  nextDomain := 0;

  for i:=0 to D_InputSeqdata.FDomainMarks.NoOfSites-1 do
  begin
    if (D_InputSeqData.FDomainMarks.X[i] <> nil) AND (TDomainInfo(D_InputSeqData.FDomainMarks.X[i]).ToSite <> nextDomain) then
    begin
      nextDomain := TDomainInfo(D_InputSeqData.FDomainMarks.X[i]).ToSite;
      TempAllDomainInfo.Add(D_InputSeqData.FDomainMarks.X[i]);
    end;
  end;

  D_InputSeqData.AllDomainInfo := TempAllDomainInfo;
  {$IFDEF VISUAL_BUILD}
  TaxaGpsDlg.Initialize;
  GeneDomainDlg.Initialize;
  {$ENDIF}
  D_InputSeqData.FDomainMarks.IsDirty := False;
  D_InputSeqData.OtuInfos.IsDirty   := False;

  ARP.UpdateRunStatusInfo('Status', 'Reading in visual preferences');
  Application.ProcessMessages;
  with VS_SeqDataExplorer do
  begin
    UpdatePercentProgress(data, ARP);
    //READ IN VISUAL STATUS
    data.Read(i, 4);
    HighlightColor := TColor(i);
    DispTaxaNamesItem  := readBooleanFromFile(data);
    DispSelTaxaItem  := readBooleanFromFile(data);
    D_InputSeqData.UpdateDispTaxaList;
    DispUseIdenSymbolItem  := readBooleanFromFile(data);
    DispGpNamesItem  := readBooleanFromFile(data);
    DispColorItem  := readBooleanFromFile(data);
    ConstantItem  := readBooleanFromFile(data);
    VariableItem  := readBooleanFromFile(data);
    ParsimInfoItem  := readBooleanFromFile(data);
    SingletonItem  := readBooleanFromFile(data);
    if Version >= 1013 then
      LabelledItem := readBooleanFromFile(data);
    ZeroFoldItem  := readBooleanFromFile(data);
    TwoFoldItem  := readBooleanFromFile(data);
    FourFoldItem  := readBooleanFromFile(data);
    StatUseOnlyMarkedSitesItem  := readBooleanFromFile(data);
    StatAllSelSitesItem  := readBooleanFromFile(data);
    StatDispInXLItem  := readBooleanFromFile(data);
    StatDispInCSVItem  := readBooleanFromFile(data);
    StatDispInTextItem  := readBooleanFromFile(data);

    DispResultsOnlyItem := readBooleanFromFile(data);
    HideNameItem := readBooleanFromFile(data);
    HideSeqItem := readBooleanFromFile(data);
    HideMotifItem := readBooleanFromFile(data);

    UpdatePercentProgress(data, ARP);

    //READ IN FONT
    FreeAndNil(CurFont);
    CurFont := TFont.Create;
    CurFont.Name := readStringFromFile(data);
    data.Read(i, 4);
    CurFont.Charset := TFontCharSet(i);
    data.Read(i, 4);
    CurFont.Color := TColor(i);
    data.Read(i, 4);
    CurFont.Size := i;
    CurFont.Bold := readBooleanFromFile(data);
    CurFont.Italic := readBooleanFromFile(data);
    CurFont.Underline := readBooleanFromFile(data);
    CurFont.StrikeThrough := readBooleanFromFile(data);
  end;

  data.Read(j, 4); // Nunber of search results
  for i:=0 to j-1 do
  begin
   TempSRes := TSearchResult.Create;
   data.Read(k, 4);
   TempSRes.Left  := k;
   data.Read(k, 4);
   TempSRes.Top   := k;
   data.Read(k, 4);
   TempSRes.Right := k;
   TempSRes.UserInput := readStringFromFile(data);
   TempSRes.SearchStr := readStringFromFile(data);
   TempSRes.MatchStr  := readStringFromFile(data);
   D_InputSeqData.AddSearchResult(TempSRes);
  end;

  VS_SeqDataExplorer.CurMotifStr := readStringFromFile(data);// read in motif string
  VS_SeqDataExplorer.CurSearchStr := readStringFromFile(data);// read in seq search str

  data.Read(i, 4);
  VS_SeqDataExplorer.CurRow := i;
  data.Read(i, 4);
  VS_SeqDataExplorer.CurCol := i;

  data.Read(i, 4);
  data.Read(D_InputSeqData.JobsPending, i);

  UpdatePercentProgress(data, ARP);

  ARP.UpdateRunStatusInfo('Status', 'Applying visual preferences');
  Application.ProcessMessages;
  //SIZE THE GRID TO HOLD DATA CORRECTLY
  {$IFDEF VISUAL_BUILD}
  with V_SeqDataExplorer.DataGrid do
  begin
    ColCount := D_InputSeqData.FNoOfSites + 1;
    RowCount := D_InputSeqData.DispTaxaList.Count+1;
  end;
  V_SeqDataExplorer.Show;
  VS_SeqDataExplorer.DispTaxaNamesItem:=True;
  V_SeqDataExplorer.ActionDispTaxaNames.Checked:=True;
  VS_SeqDataExplorer.DispUseIdenSymbolItem:=True;
  V_SeqDataExplorer.ActionUseIdenticalSymbol.Checked:=True;
  VS_SeqDataExplorer.ApplyingToVisual := True;
  V_SeqDataExplorer.AdjustEnabledStatus;
  {$ENDIF}
  VS_SeqDataExplorer.ApplyingToVisual := False;
  {$IFDEF VISUAL_BUILD}
  VS_SeqDataExplorer.ApplyToVisual(V_SeqDataExplorer);
  {$ENDIF}
  ARP.UpdateRunStatusInfo('Status', 'Done restoring saved session');
  Application.ProcessMessages;
  ARP.Hide;
end;

function RetriveSequenceFromDataSession(data: TStream; Version : integer; ARP : TRuntimeProgress; Seqs: TSequenceList): boolean;
var
  i: Integer = -1;
  j: Integer = -1;
  k: Integer = -1;
  z: Integer = -1;
  readLength:   Integer = -1;
  Taxa, GroupsCSV, SpeciesCSV, PopulationCSV, BufferString : AnsiString;
  TaxaList, GroupsList, SpeciesList, PopulationList : TStringList;
  te : TSiteAttrType;
  buffer : array of AnsiChar;
  NumOfSites: Integer;
  newSeq: TSequence;
begin
  Result := False;
  ARP.UpdateRunStatusInfo('Status', 'Reading data from saved session');
  Application.ProcessMessages;

  data.Read(i,4); //How many records to skip (this can be adjusted so we can add more content at the beginning of the file in the future)
  data.Read(j,i); //Skip the indicated # of records
  UpdatePercentProgress(data, ARP);
  Seqs.IsDNA := readBooleanFromFile(data);
  data.Read(i, 1);
  Seqs.IsProteinCoding := (i = 1);
  readBooleanFromFile(data); // DO NOT REMOVE THIS.  Even if it's not setting a variable we need to read the boolean or our reading frame will be off.
  readBooleanFromFile(data); // DO NOT REMOVE THIS.

  ARP.UpdateRunStatusInfo('Status', 'Reading in Taxonomy information');
  Application.ProcessMessages;

  //READ IN TAXA
  data.Read(readLength, 4);
  setlength(buffer, (readLength +1));
  data.Read(buffer[0], readLength);
  buffer[readLength] := #0;  //Add that stopping character so it can be turned into a string
  taxa := PAnsiChar(buffer);   //from array of char to a string
  TaxaList := TStringList.create;
  splitstr(taxa, ',', TaxaList);
  UpdatePercentProgress(data, ARP);

  //READ IN GROUPS  - NOT USED FOR SEQUENCES
  GroupsCSV := readStringFromFile(data);
  GroupsList := TStringList.Create;
  splitstr(GroupsCSV, ',', GroupsList);
  SpeciesCSV := readStringFromFile(data);
  SpeciesList := TStringList.Create;
  splitstr(SpeciesCSV, ',', SpeciesList);
  PopulationCSV := readStringFromFile(data);
  PopulationList := TStringList.Create;
  splitstr(PopulationCSV, ',', PopulationList);
  UpdatePercentProgress(data, ARP);

  data.Read(i, 4);
  NumOfSites := i;

  data.Read(i, 4);
  //D_InputSeqData.NoOfSitesUsed := i;

  data.Read(i, 4);
  //D_InputSeqData.FDomainMarks.NoOfSites := i;

  ARP.UpdateRunStatusInfo('Status', 'Reading in site attribute information');
  Application.ProcessMessages;
  //NUMBER OF SITE ATTRIBUTES IN SAVED FILE
  data.Read(i, 4);

  j :=0;
  //READ IN NUMBER OF SITES EACH SITE ATTRIBUTE HAS
  for te:=megNone to megLast do
  begin
    j := j +1;  //increment the number of sites we have read
    if j <= i then //Read in the next number of attribute sites if it is stored in the data file
      data.Read(z, 4)
  end;

  UpdatePercentProgress(data, ARP);

  while(j < i) do   //In case we are opening a newer version of a data session that has nore attrSiteTyps
  begin
    j := j +1;
    data.Read(k, 4);   //read until we've read all the site types and are back in the right position
  end;
  UpdatePercentProgress(data, ARP);

  ARP.UpdateRunStatusInfo('Status', 'Reading in sequences');
  Application.ProcessMessages;
  //READ IN SEQUENCES
  for j:=0 to TaxaList.Count -1 do
  begin
    SetLength(BufferString, NumOfSites+1);
    data.Read(BufferString[1], NumOfSites);
    newSeq := TSequence.Create;
    with newSeq do
    begin
      SeqName := TaxaList.Strings[j];
      SeqData := BufferString;
      GroupName := GroupsList.Strings[j];
      SpeciesName := SpeciesList.Strings[j];
      PopulationName := PopulationList.Strings[j]
    end;
    Seqs.Add(newSeq);
    UpdatePercentProgress(data, ARP);
  end;

  result := true;
  ARP.Hide;
end;


function OpenDistanceDataSession( data: TStream; Version : integer; ARP : TRuntimeProgress): boolean;
var
  i: Integer = -1;
  j: Integer = -1;
  readLength : Integer = -1;
  AOtuInfo : TAllOtuInfo;
begin
  Result := False;

  ARP.UpdateRunStatusInfo('Status', 'Opening Distance Data File');
  UpdatePercentProgress(data, arp);

  D_InputDistData := TD_InputDistData.create;
  VS_DistDataExplorer := TVS_DistDataExplorer.create;
  {$IFDEF VISUAL_BUILD}
  V_DistDataExplorer := TV_DistDataExplorer.Create(nil);
  {$ENDIF}

  with D_InputDistData do
  begin

    //SETUP AOTUINFO
    AOtuInfo := TAllOtuInfo.Create;
    ARP.UpdateRunStatusInfo('Status', 'Restoring Organizational Taxa Unit info and Distances');
    UpdatePercentProgress(data, arp);

    AOtuInfo.RestoreFromFile(data, DataDist, version);

    FOtuInfos := AOtuInfo;

    D_InputDistData.DispTaxaList := TLongIntList.Create;
    data.read(readLength, 4);
    for j :=0 to readLength-1 do
    begin
      data.Read(i, 4);
      DispTaxaList.Add(i);
    end;

    ARP.UpdateRunStatusInfo('Status', 'Preparing visual');
    UpdatePercentProgress(data, arp);
    //SETUP TAXA GROUPS DIALOG
    {$IFDEF VISUAL_BUILD}
    TaxaGpsDlg := TTaxaGpsDlg.Create(Application);
    {$ENDIF}

    D_InputDistData.NoOfTaxa := FOtuInfos.NoOfOtus;

    // **** Visual Status **** //
    ARP.UpdateRunStatusInfo('Status', 'Applying saved visual status');
    UpdatePercentProgress(data, arp);
    //DISPLAY TAXA NAMES?
    i := 0;
    data.Read(i, 1);
    VS_DistDataExplorer.DispSelTaxaItem := (i = 1);

    //DISPLAY GROUP NAMES?
    i := 0;
    data.Read(i, 1);
    VS_DistDataExplorer.DispShowGpNames := (i = 1);

    //WHAT PRECISION SHOULD WE USE WHEN SHOWING DISTANCES
    data.Read(VS_DistDataExplorer.FPrecision, 4);

    //HOW MANY CHARACTERS WILL IT TAKE TO LEFT OF THE DECIMAL POINT TO SHOW ALL DISTANCES
    data.Read(VS_DistDataExplorer.LeftOfDecimalLen, 4);

    with VS_DistDataExplorer do
    begin
      FreeAndNil(CurFont);
      CurFont := TFont.Create;
      CurFont.Name := readStringFromFile(data);
      data.Read(i, 4);
      CurFont.Charset := TFontCharSet(i);
      data.Read(i, 4);
      CurFont.Size := i;
      data.Read(i, 4);
      {$IFNDEF FPC}
      CurFont.Style := TFontStyles(byte(i));
      {$ENDIF}
    end;
    //RESTORE THE MAX DISTANCE
    D_InputDistData.ComputeMaximumDistance;
    {$IFDEF VISUAL_BUILD}
    TaxaGpsDlg.Initialize;  // this sets up display and other things
    V_DistDataExplorer.Show;
    if VS_DistDataExplorer.DispSelTaxaItem then  //SET SIZE OF DATA GRID DEPENDING ON DATA
      V_DistDataExplorer.DataGrid.RowCount := FOtuInfos.NoOfSelOtus + 1
    else
      V_DistDataExplorer.DataGrid.RowCount := FOtuInfos.NoOfOtus + 1;

    V_DistDataExplorer.DataGrid.ColCount := V_DistDataExplorer.DataGrid.RowCount;

    VS_DistDataExplorer.ApplyToVisual(V_DistDataExplorer);
    {$ENDIF}
  end;
end;

function MessageStringForDisabledSeqDataExplorer(dTypeStr: String; numSeqs: QWord; numSites: QWord): String;
begin
  Result := Format('    The Sequence Data Explorer is disabled because the size of the active dataset exceeds it%ss capacity.', [#39]) + LineEnding + LineEnding;
  Result += Format('    No. of Taxa:  %.0n', [numSeqs*1.0]) + LineEnding;
  Result += Format('    No. of Sites: %.0n', [numSites*1.0]) + LineEnding;
  Result += Format('    Data Type:    %s', [dTypeStr]) + LineEnding + LineEnding;
  Result += Format('    The active data is currently stored in memory and analyses can still be run using the active %s data.', [dTypeStr]);
end;

function DeriveDataFileInfo(FileStrings: TStringList; Filename: String = ''): TDataFileInfo;
var
  {$IFNDEF VISUAL_BUILD}
  ProcessPack: TProcessPack;
  aList: TStringList;
  ContainsCoding: String;
  SymbolStr: String;
  {$ENDIF}
  MyDataFileFormat: TDataFileFormat;
  ADataType: String = '';
  AGapSym: String = '';
  AMissingSym: String = '';
  AIdentSym: String = '';
  AFoundFormat: Boolean = False;
begin
  try
    {$IFNDEF VISUAL_BUILD}
    ProcessPack := nil;
    {$ENDIF}
    if Filename <> EmptyStr then
      Result := TDataFileInfo.Create(Filename)
    else
      Result := TDataFileInfo.Create(GetActiveDataFileName);
    MyDataFileFormat := Result.GetDataFileFormat;

    if MyDataFileFormat = dfMeg then
    begin
      ReadFormatValsFromMegaSeqFile(FileStrings, ADataType, AIdentSym, AMissingSym, AGapSym, AFoundFormat);
      if not AFoundFormat then
      begin
        if not PromptUserForDataFileInfo(Result) then
          raise EAbort.Create('User aborted processing of input data');
        {$IFNDEF VISUAL_BUILD}
        ProcessPack := D_MegaMain.ProcessPack;
        Result.SetDataType(MapDataTypeStringToTokenCode(ProcessPack.TextualSettingsList.Values[DataTypeStr]));
        Result.SetMissingBaseSymbol(AnsiChar(ProcessPack.TextualSettingsList.Values[MissingBaseSymbolStr][1]));
        Result.SetIdenticalBaseSymbol(AnsiChar(ProcessPack.TextualSettingsList.Values[IdenticalBaseSymbolStr][1]));
        Result.SetGapSymbol(AnsiChar(ProcessPack.TextualSettingsList.Values[GapSymbolStr][1]));
        ContainsCoding := ProcessPack.TextualSettingsList.Values[ContainsCodingNucStr];
        Result.SetContainsCodingNuc(SameText(containsCoding, 'True'));
        Result.SetUserSpecifiedSymbols(true);
        D_MegaMain.FDataType := Result.GetDataType;
        {$ENDIF}
      end
      else
      begin
        Result.SetDataType(MapDataTypeStringToTokenCode(ADataType));
        Result.SetMissingBaseSymbol(AnsiChar(AMissingSym[1]));
        Result.SetIdenticalBaseSymbol(AnsiChar(AIdentSym[1]));
        Result.SetGapSymbol(AnsiChar(AGapSym[1]));
        Result.SetUserSpecifiedSymbols(false);
        {$IFNDEF VISUAL_BUILD}
        if not AnsiContainsText(ADataType, 'distance') then
        begin
          // In this case, we need to make sure that the information in the MEGA file is consistent
          // with the information in the analysis options file. If not, we default to
          // the information in the MEGA file and let the user know about it
          ProcessPack := D_MegaMain.ProcessPack;
          if (ProcessPack.TextualSettingsList.Values[MissingBaseSymbolStr][1] <> String(Result.GetMissingBaseSymbol)) then
            Warn_NV('Missing base symbol specified in analysis options file does not match the missing base symbol indicated in the MEGA file. Using the symbol indicated in the MEGA file: ' + Result.GetMissingBaseSymbol);

          if (ProcessPack.TextualSettingsList.Values[IdenticalBaseSymbolStr][1] <> String(Result.GetIdenticalBaseChar)) then
            Warn_NV('Identical base symbol specified in analysis options file does not match the identical base symbol indicated in the MEGA file. Using the symbol indicated in the MEGA file: ' + Result.GetIdenticalBaseChar);

          if (ProcessPack.TextualSettingsList.Values[GapSymbolStr][1] <> String(Result.GetGapSymbol)) then
            Warn_NV('Gap symbol specified in analysis options file does not match the Gap base symbol indicated in the MEGA file. Using the symbol indicated in the MEGA file: ' + Result.GetGapSymbol);
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
        Result.SetDataType(MapDataTypeStringToTokenCode(ProcessPack.TextualSettingsList.Values[DataTypeStr]));
        AList := ProcessPack.TextualSettingsList;
        if Alist.IndexOfName(MissingBaseSymbolStr2) >= 0 then
          SymbolStr := AList.Values[MissingBaseSymbolStr2]
        else
          SymbolStr := AList.Values[MissingBaseSymbolStr];
        Result.SetMissingBaseSymbol(AnsiChar(SymbolStr[1]));

        if Alist.IndexOfName(IdenticalBaseSymbolStr2) >= 0 then
          SymbolStr := AList.Values[IdenticalBaseSymbolStr2]
        else
          SymbolStr := AList.Values[IdenticalBaseSymbolStr];
        Result.SetMissingBaseSymbol(AnsiChar(SymbolStr[1]));
        Result.SetIdenticalBaseSymbol(AnsiChar(SymbolStr[1]));
        if Alist.IndexOfName(GapSymbolStr2) >= 0 then
          SymbolStr := AList.Values[GapSymbolStr2]
        else
          SymbolStr := AList.Values[GapSymbolStr];
        Result.SetMissingBaseSymbol(AnsiChar(SymbolStr[1]));
        Result.SetGapSymbol(AnsiChar(SymbolStr[1]));
        ContainsCoding := ProcessPack.TextualSettingsList.Values[ContainsCodingNucStr];
        Result.SetContainsCodingNuc(SameText(containsCoding, 'True'));
        Result.SetUserSpecifiedSymbols(true);
        D_MegaMain.FDataType := Result.GetDataType;
      {$ENDIF}
    end
    else
      raise EInvalidArgument.Create('Invalid data file format specified');

    {$IFNDEF VISUAL_BUILD}
     D_MegaMain.FDataType := Result.GetDataType;
    {$ENDIF}
  Except
    on E:EAbort do
    begin
      Result := nil;
      raise EAbort.Create(E.Message);
    end;

    on E:EInvalidArgument do
    begin
      Result := nil;
      raise Exception.Create(E.Message);
    end;

    on E:Exception do
    begin
      Result := nil;
      raise Exception.Create(E.Message);
    end;
  end;
end;

function PromptUserForDataFileInfo(aInfo: TDataFileInfo; OpenInSDE: Boolean=True): Boolean;
var
  {$IFDEF VISUAL_BUILD}
  MyInputDataOptionsDlg: TInputDataOptionsDlg = nil;
  {$ELSE}
  ProcessPack: TProcessPack = nil;
  ContainsCoding: String = '';
  {$ENDIF}
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
    try
      MyInputDataOptionsDlg := TInputDataOptionsDlg.Create(Application);
      MyInputDataOptionsDlg.HideDistDataOption;
      if MyInputDataOptionsDlg.ShowModal = mrOK then
      begin
        aInfo.SetDataType(MyInputDataOptionsDlg.DataType);
        aInfo.SetMissingBaseSymbol(MyInputDataOptionsDlg.MissSym);
        aInfo.SetIdenticalBaseSymbol(MyInputDataOptionsDlg.IdenSym);
        aInfo.SetGapSymbol(MyInputDataOptionsDlg.GapSym);
        aInfo.SetUserSpecifiedSymbols(true);
        Result := True;
      end;
    finally
      if Assigned(MyInputDataOptionsDlg) then
        MyInputDataOptionsDlg.Free;
    end;
  {$ELSE}
  ProcessPack := D_MegaMain.ProcessPack;
  aInfo.SetDataType(MapDataTypeStringToTokenCode(ProcessPack.TextualSettingsList.Values[DataTypeStr]));
  aInfo.SetMissingBaseSymbol(AnsiChar(ProcessPack.TextualSettingsList.Values[MissingBaseSymbolStr][1]));
  aInfo.SetIdenticalBaseSymbol(AnsiChar(ProcessPack.TextualSettingsList.Values[IdenticalBaseSymbolStr][1]));
  aInfo.SetGapSymbol(AnsiChar(ProcessPack.TextualSettingsList.Values[GapSymbolStr][1]));
  ContainsCoding := ProcessPack.TextualSettingsList.Values[ContainsCodingNucStr];
  aInfo.SetContainsCodingNuc(SameText(containsCoding, 'True'));
  aInfo.SetUserSpecifiedSymbols(true);
  D_MegaMain.FDataType := aInfo.GetDataType;
  Result := True;
  {$ENDIF}
end;

procedure ReadFormatValsFromMegaSeqFile(FileName: String;
                                        var DataType: String;
                                        var IdentSym: String;
                                        var MissingSym: String;
                                        var GapSym: String;
                                        var FoundFormat: Boolean);
var
  AFile: TextFile;
  AStr: String;
  TokenList: TStringList;
  j: Integer;
  Key: String;
  Value: String;
  InsideFormatStatement: Boolean;
begin
  TokenList := nil;
  InsideFormatStatement := False;
  FoundFormat := False;
  DataType := 'nil';
  IdentSym := '.';
  MissingSym := '?';
  GapSym := '-';

  if not FileExists(FileName) then
    Exit;

  try
    AssignFile(AFile, Filename);
    Reset(AFile);
    TokenList := TStringList.Create;
    while (not Eof(AFile)) and (not FoundFormat) do
    begin
      ReadLn(AFile, AStr);
      if BeginsWith(LowerCase(trim(AStr)), '!format') then
      begin
        InsideFormatStatement := True;
        while ((not Eof(AFile)) and InsideFormatStatement) do
        begin
          TokenList.Clear;
          SplitOnWhiteSpace2(AStr, TokenList);
          for j := 0 to TokenList.Count - 1 do
          begin
            if AnsiContainsText(TokenList[j], '=') then
            begin
              Key := TokenList.Names[j];
              Value := TokenList.Values[Key];
              if Pos(';', Value) <> 0 then
                Value := Copy(Value, 1, Pos(';', Value) - 1);
              if LowerCase(Key) = 'indel' then
                GapSym := Value[1]
              else if LowerCase(Key) = 'missing' then
                MissingSym := Value[1]
              else if (LowerCase(Key) = 'identical') or (LowerCase(Key) = 'matchchar') then
                IdentSym := Value[1]
              else if LowerCase(Key) = 'datatype' then
                DataType := Value;
            end;
          end;
          if EndsWith(AStr, ';') then
            InsideFormatStatement := False
          else
            ReadLn(AFile, AStr);
        end;

        if (DataType <> 'nil') and (not InsideFormatStatement) then
          FoundFormat := True;
      end;
    end;
  finally
    CloseFile(AFile);
    if Assigned(TokenList) then
      FreeAndNil(TokenList);
  end;
end;

procedure ReadFormatValsFromMegaSeqFile(aList: TStringList; var DataType: String; var IdentSym: String; var MissingSym: String; var GapSym: String; var FoundFormat: Boolean);
var
  AStr: String;
  TokenList: TStringList;
  Index, j: Integer;
  Key: String;
  Value: String;
  InsideFormatStatement: Boolean;
begin
  TokenList := nil;
  InsideFormatStatement := False;
  FoundFormat := False;
  DataType := 'nil';
  IdentSym := '.';
  MissingSym := '?';
  GapSym := '-';

  try
    TokenList := TStringList.Create;
    Index := 0;
    while (Index < aList.Count) and (not FoundFormat) do
    begin
      AStr := aList[Index];
      inc(Index);
      if BeginsWith(LowerCase(trim(AStr)), '!format') then
      begin
        InsideFormatStatement := True;
        while (Index < aList.Count) and InsideFormatStatement do
        begin
          TokenList.Clear;
          SplitOnWhiteSpace2(AStr, TokenList);
          for j := 0 to TokenList.Count - 1 do
          begin
            if AnsiContainsText(TokenList[j], '=') then
            begin
              Key := TokenList.Names[j];
              Value := TokenList.Values[Key];
              if Pos(';', Value) <> 0 then
                Value := Copy(Value, 1, Pos(';', Value) - 1);
              if LowerCase(Key) = 'indel' then
                GapSym := Value[1]
              else if LowerCase(Key) = 'missing' then
                MissingSym := Value[1]
              else if (LowerCase(Key) = 'identical') or (LowerCase(Key) = 'matchchar') then
                IdentSym := Value[1]
              else if LowerCase(Key) = 'datatype' then
                DataType := Value;
            end;
          end;
          if EndsWith(AStr, ';') then
            InsideFormatStatement := False
          else
          begin
            aStr := aList[Index];
            inc(Index);
          end;
        end;

        if (DataType <> 'nil') and (not InsideFormatStatement) then
          FoundFormat := True;
      end;
    end;
  finally
    if Assigned(TokenList) then
      FreeAndNil(TokenList);
  end;
end;

function ProcessInputMegaFile(FileStrings: TStringList; OpenInSDE: Boolean; Filename: String = ''): Boolean;
var
  MyDataType: TSnTokenCode;
  MyDataFormat: TSnTokenCode;
  MyMissingSymbol: AnsiChar;
  MyIdenticalSymbol: AnsiChar;
  MyGapSymbol: AnsiChar;
  DataFileInfo: TDataFileInfo;
begin
  Result := False;

  try
    DataFileInfo := DeriveDataFileInfo(FileStrings, Filename);
  Except
    on E:EAbort do
    begin
      Exit;
    end;

    on E:EParserError do
    begin
      Exit; // in this case, we already informed the user and opened the input data file in the text editor
    end;

    on E:Exception do
    begin
      Result := False;
      ShowErrorMessage(E);
      Exit;
    end;
  end;

  MyDataType := DataFileInfo.GetDataType;
  MyDataFormat := DataFileInfo.GetDataFormat;
  MyMissingSymbol := DataFileInfo.GetMissingBaseSymbol;
  MyIdenticalSymbol := DataFileInfo.GetIdenticalBaseChar;
  MyGapSymbol := DataFileInfo.GetGapSymbol;
  try try
    case MyDataType of
      snNucleotide,
      snProtein        : Result := ReadSequenceFile(FileStrings, True, MyDataType, MyDataFormat, MyMissingSymbol, MyIdenticalSymbol, MyGapSymbol, OpenInSDE);
      snDistance       : RaiseErrorMessage(HC_Not_Yet_Implemented,  'Selected option is not yet activated.');
      snAlleleFreq     : RaiseErrorMessage(HC_Not_Yet_Implemented,  'Selected option is not yet activated.');
      snGenotypeFreq   : RaiseErrorMessage(HC_Not_Yet_Implemented,  'Selected option is not yet activated.');
      snMicrosatellites: RaiseErrorMessage(HC_Not_Yet_Implemented,  'Selected option is not yet activated.');
      snRestrictionSite: RaiseErrorMessage(HC_Not_Yet_Implemented,  'Selected option is not yet activated.');
      snRFLP           : RaiseErrorMessage(HC_Not_Yet_Implemented,  'Selected option is not yet activated.');
      snRAPD           : RaiseErrorMessage(HC_Not_Yet_Implemented,  'Selected option is not yet activated.');
      else
        begin
        {$IFDEF VISUAL_BUILD}
        Raise Exception.Create('MEGA failed to parse the input data file because it did not determine the data type correctly');
        {$ELSE}
        Error_NV('MEGA failed to parse the input data file because it did not determine the data type correctly');
        {$ENDIF}
        end;
    end;
  except
    On E:Exception do
    begin
      Result := False;
      ShowErrorMessage(E);
    end;
  end;
  finally
    if Assigned(DataFileInfo) then
      DataFileInfo.Free;
  end;
end;

function DeriveDataDirectoryInfo(SourceDir: String): TDataFileInfo;
var
  files: TStringList;
  {$IFNDEF VISUAL_BUILD}
  ProcessPack: TProcessPack = nil;
  {$ENDIF}
  MyDataFileFormat: TDataFileFormat;
  ADataType: String = '';
  AGapSym: String = '';
  AMissingSym: String = '';
  AIdentSym: String = '';
  AFoundFormat: Boolean = False;
begin
  Result := nil;
  try
    files := FindAllFiles(SourceDir, '*.meg;*.fas;*.fasta;*.fst;*.fsta', False);
    if files.Count > 0 then
    begin
      Result := TDataFileInfo.Create(files[0]);
      MyDataFileFormat := Result.GetDataFileFormat;

      if MyDataFileFormat = dfMeg then
      begin
        ReadFormatValsFromMegaSeqFile(files[0], ADataType, AIdentSym, AMissingSym, AGapSym, AFoundFormat);
        if not AFoundFormat then
        begin
          if not PromptUserForDataFileInfo(Result, False) then
            raise EAbort.Create('user cancelled operation');
        end
        else
        begin
          Result.SetDataType(MapDataTypeStringToTokenCode(ADataType));
          Result.SetMissingBaseSymbol(AnsiChar(AMissingSym[1]));
          Result.SetIdenticalBaseSymbol(AnsiChar(AIdentSym[1]));
          Result.SetGapSymbol(AnsiChar(AGapSym[1]));
          Result.SetUserSpecifiedSymbols(false);
        end;
      end
      else if (MyDataFileFormat = dfFasta) or (MyDataFileFormat = dfNexus) then
      begin
        if MyDataFileFormat = dfFasta then
        begin
          if not PromptUserForDataFileInfo(Result, False) then
            raise EAbort.Create('user cancelled operation');
        end;
      end
      else
        raise EInvalidArgument.Create('Invalid data file format specified');
    end
    else
      raise Exception.Create('no sequence alignment files found. Valid file extensions are .meg, .fas, .fasta, .fst, or .fsta');
  finally
    if Assigned(files) then
      files.Free;
  end;
end;

function DeriveDataFileInfo(MyDataFileName: String; OpenInSDE: Boolean=True; aNotify: TNotifyEvent=nil):TDataFileInfo;
var
  {$IFNDEF VISUAL_BUILD}
  ProcessPack: TProcessPack = nil;
  ContainsCoding: String;
  SymbolStr: String;
  aList: TStringList;
  {$ENDIF}
  MyDataFileFormat: TDataFileFormat;
  ADataType: String = '';
  AGapSym: String = '';
  AMissingSym: String = '';
  AIdentSym: String = '';
  AFoundFormat: Boolean = False;
begin
  {$IFDEF VISUAL_BUILD}
  try
    Result := TDataFileInfo.Create(MyDataFileName);
    if Assigned(aNotify) then
      Result.NotifyEvent := aNotify;
    MyDataFileFormat := Result.GetDataFileFormat;

    if MyDataFileFormat = dfMeg then
    begin
      ReadFormatValsFromMegaSeqFile(MyDataFileName, ADataType, AIdentSym, AMissingSym, AGapSym, AFoundFormat);
      if not AFoundFormat then
      begin
        {$IFDEF DARWIN}
        if not PromptUserForDataFileInfo(Result, OpenInSDE) then
          raise EAbort.Create('user aborted');
        {$ELSE}
        if not PromptUserForDataFileInfo(Result, OpenInSDE) then
          raise EAbort.Create('Aborting normal process execution in order to prompt user for needed information');
        {$ENDIF}
      end
      else
      begin
        Result.SetDataType(MapDataTypeStringToTokenCode(ADataType));
        Result.SetMissingBaseSymbol(AnsiChar(AMissingSym[1]));
        Result.SetIdenticalBaseSymbol(AnsiChar(AIdentSym[1]));
        Result.SetGapSymbol(AnsiChar(AGapSym[1]));
        Result.SetUserSpecifiedSymbols(false);
      end;
    end
    else if (MyDataFileFormat = dfFasta) or (MyDataFileFormat = dfNexus) then
    begin
      if MyDataFileFormat = dfFasta then
      begin
        {$IFDEF DARWIN}
        if not PromptUserForDataFileInfo(Result) then
          raise EAbort.Create('user aborted');
        {$ELSE}
        if not PromptUserForDataFileInfo(Result) then
          raise EAbort.Create('Aborting normal process execution in order to prompt user for needed information');
        {$ENDIF}
      end;
    end
    else
      raise EInvalidArgument.Create('Invalid data file format specified');
  except
    on E:EAbort do
    begin
      Result := nil;
      raise EAbort.Create(E.Message);
    end;

    on E:EInvalidArgument do
    begin
      Result := nil;
      raise Exception.Create(E.Message);
    end;

    on E:Exception do
    begin
      Result := nil;
      raise Exception.Create(E.Message);
    end;
  end;
  {$ELSE}
  try
  Result := TDataFileInfo.Create(MyDataFileName);
  MyDataFileFormat := Result.GetDataFileFormat;

  if MyDataFileFormat = dfMeg then
  begin
    ReadFormatValsFromMegaSeqFile(MyDataFileName, ADataType, AIdentSym, AMissingSym, AGapSym, AFoundFormat);
    if not AFoundFormat then
    begin
      if not PromptUserForDataFileInfo(Result) then
        raise EAbort.Create('User aborted processing of input data');
      {$IFNDEF VISUAL_BUILD}
      ProcessPack := D_MegaMain.ProcessPack;
      Result.SetDataType(MapDataTypeStringToTokenCode(ProcessPack.TextualSettingsList.Values[DataTypeStr]));
      Result.SetMissingBaseSymbol(AnsiChar(ProcessPack.TextualSettingsList.Values[MissingBaseSymbolStr][1]));
      Result.SetIdenticalBaseSymbol(AnsiChar(ProcessPack.TextualSettingsList.Values[IdenticalBaseSymbolStr][1]));
      Result.SetGapSymbol(AnsiChar(ProcessPack.TextualSettingsList.Values[GapSymbolStr][1]));
      ContainsCoding := ProcessPack.TextualSettingsList.Values[ContainsCodingNucStr];
      Result.SetContainsCodingNuc(SameText(containsCoding, 'True'));
      Result.SetUserSpecifiedSymbols(true);
      D_MegaMain.FDataType := Result.GetDataType;
      {$ENDIF}
    end
    else
    begin
      Result.SetDataType(MapDataTypeStringToTokenCode(ADataType));
      Result.SetMissingBaseSymbol(AnsiChar(AMissingSym[1]));
      Result.SetIdenticalBaseSymbol(AnsiChar(AIdentSym[1]));
      Result.SetGapSymbol(AnsiChar(AGapSym[1]));
      Result.SetUserSpecifiedSymbols(false);
      {$IFNDEF VISUAL_BUILD}
      if (not AnsiContainsText(ADataType, 'distance')) and (not (D_MegaMain.MegaAction = maFindTipDates)) and (not (D_MegaMain.MegaAction = maFindSitePatterns))  then
      begin
        //need to update here to detect when symbols specified on command line
        // In this case, we need to make sure that the information in the MEGA file is consistent
        // with the information in the analysis options file. If not, we default to
        // the information in the MEGA file and let the user know about it
        ProcessPack := D_MegaMain.ProcessPack;
        if (ProcessPack.TextualSettingsList.Values[MissingBaseSymbolStr][1] <> String(Result.GetMissingBaseSymbol)) then
          Warn_NV('Missing base symbol specified in analysis options file does not match the missing base symbol indicated in the MEGA file. Using the symbol indicated in the MEGA file: ' + Result.GetMissingBaseSymbol);

        if (ProcessPack.TextualSettingsList.Values[IdenticalBaseSymbolStr][1] <> String(Result.GetIdenticalBaseChar)) then
          Warn_NV('Identical base symbol specified in analysis options file does not match the identical base symbol indicated in the MEGA file. Using the symbol indicated in the MEGA file: ' + Result.GetIdenticalBaseChar);

        if (ProcessPack.TextualSettingsList.Values[GapSymbolStr][1] <> String(Result.GetGapSymbol)) then
          Warn_NV('Gap symbol specified in analysis options file does not match the Gap base symbol indicated in the MEGA file. Using the symbol indicated in the MEGA file: ' + Result.GetGapSymbol);
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
    if not (D_MegaMain.MegaAction = maFindTipDates) then
    begin
      ProcessPack := D_MegaMain.ProcessPack;
      Result.SetDataType(MapDataTypeStringToTokenCode(ProcessPack.TextualSettingsList.Values[DataTypeStr]));
      AList := ProcessPack.TextualSettingsList;
      if Alist.IndexOfName(MissingBaseSymbolStr2) >= 0 then
        SymbolStr := AList.Values[MissingBaseSymbolStr2]
      else
        SymbolStr := AList.Values[MissingBaseSymbolStr];
      Result.SetMissingBaseSymbol(AnsiChar(SymbolStr[1]));

      if Alist.IndexOfName(IdenticalBaseSymbolStr2) >= 0 then
        SymbolStr := AList.Values[IdenticalBaseSymbolStr2]
      else
        SymbolStr := AList.Values[IdenticalBaseSymbolStr];
      Result.SetIdenticalBaseSymbol(AnsiChar(SymbolStr[1]));
      if Alist.IndexOfName(GapSymbolStr2) >= 0 then
        SymbolStr := AList.Values[GapSymbolStr2]
      else
        SymbolStr := AList.Values[GapSymbolStr];
      Result.SetGapSymbol(AnsiChar(SymbolStr[1]));
      ContainsCoding := ProcessPack.TextualSettingsList.Values[ContainsCodingNucStr];
      Result.SetContainsCodingNuc(SameText(containsCoding, 'True'));
      Result.SetUserSpecifiedSymbols(true);
      D_MegaMain.FDataType := Result.GetDataType;
    end;
    {$ENDIF}
  end
  else
    raise EInvalidArgument.Create('Invalid data file format specified');

  {$IFNDEF VISUAL_BUILD}
  if not (D_MegaMain.MegaAction = maFindTipDates) then
    D_MegaMain.FDataType := Result.GetDataType;
  {$ENDIF}
  except
    on E:EAbort do
    begin
      Result := nil;
      raise EAbort.Create(E.Message);
    end;

    on E:EInvalidArgument do
    begin
      Result := nil;
      raise Exception.Create(E.Message);
    end;

    on E:Exception do
    begin
      Result := nil;
      raise Exception.Create(E.Message);
    end;
  end;
  {$ENDIF}
end;

function ProcessInputNexusFile(Filename: String; OpenInSDE: Boolean; aNotify: TNotifyEvent=nil): Boolean;
var
  ADataFileInfo: TDataFileInfo;
begin
  Result := False;
  ADataFileInfo := nil;

  try
    ADataFileInfo := DeriveDataFileInfo(FileName, True, aNotify);
  Except
    on E:EAbort do
    begin
      Exit;
    end;

    on E:EParserError do
    begin
      Exit; // in this case, we already informed the user and opened the input data file in the text editor
    end;

    on E:Exception do
    begin
      Result := False;
      ShowErrorMessage(E);
      Exit;
    end;
  end;

  try try
    Result := ReadNexusFile(ADataFileInfo, OpenInSDE);
  except
    On E:Exception do
    begin  // Note: Choosing to do the OpenAndFocus on error in ReadFastaFile instead of here.
      ShowErrorMessage(E);
      Result := False;
    end;
  end;
  finally
    if Assigned(ADataFileInfo) then
      ADataFileInfo.Free;
  end;
end;

function ProcessInputMegaFile(FileName: String; OpenInSDE: Boolean=True; aNotify: TNotifyEvent=nil): Boolean;
var
  MyDataType: TSnTokenCode;
  MyDataFormat: TSnTokenCode;
  MyMissingSymbol: AnsiChar;
  MyIdenticalSymbol: AnsiChar;
  MyGapSymbol: AnsiChar;
  DataFileInfo: TDataFileInfo;
begin
  Result := False;

  try
    DataFileInfo := DeriveDataFileInfo(FileName, True, aNotify);
  Except
    on E:EAbort do
    begin
      Exit;
    end;

    on E:EParserError do
    begin
      Exit; // in this case, we already informed the user and opened the input data file in the text editor
    end;

    on E:Exception do
    begin
      Result := False;
      ShowErrorMessage(E);
      Exit;
    end;
  end;

  MyDataType := DataFileInfo.GetDataType;
  MyDataFormat := DataFileInfo.GetDataFormat;
  MyMissingSymbol := DataFileInfo.GetMissingBaseSymbol;
  MyIdenticalSymbol := DataFileInfo.GetIdenticalBaseChar;
  MyGapSymbol := DataFileInfo.GetGapSymbol;
  try try
    case MyDataType of
      snNucleotide,
      snProtein        : Result := ReadSequenceFile(FileName, True, MyDataType, MyDataFormat, MyMissingSymbol, MyIdenticalSymbol, MyGapSymbol, OpenInSDE);
      snDistance       : Result := ReadDistFile(FileName, True, MyDataType, MyDataFormat);
      snAlleleFreq     : RaiseErrorMessage(HC_Not_Yet_Implemented,  'Selected option is not yet activated.');
      snGenotypeFreq   : RaiseErrorMessage(HC_Not_Yet_Implemented,  'Selected option is not yet activated.');
      snMicrosatellites: RaiseErrorMessage(HC_Not_Yet_Implemented,  'Selected option is not yet activated.');
      snRestrictionSite: RaiseErrorMessage(HC_Not_Yet_Implemented,  'Selected option is not yet activated.');
      snRFLP           : RaiseErrorMessage(HC_Not_Yet_Implemented,  'Selected option is not yet activated.');
      snRAPD           : RaiseErrorMessage(HC_Not_Yet_Implemented,  'Selected option is not yet activated.');
      else
        begin
        {$IFDEF VISUAL_BUILD}
        Raise Exception.Create('MEGA failed to parse the input data file because it did not determine the data type correctly');
        {$ELSE}
        Error_NV('MEGA failed to parse the input data file because it did not determine the data type correctly');
        {$ENDIF}
        end;
    end;
  except
    On E:Exception do
    begin
      Result := False;
      ShowErrorMessage(E);
    end;
  end;
  finally
    if Assigned(DataFileInfo) then
      DataFileInfo.Free;
  end;
end;

function ProcessInputFastaFile(FileName: String; OpenInSDE: Boolean=True; aNotify: TNotifyEvent=nil): Boolean;
var
  ADataFileInfo: TDataFileInfo;
begin
  Result := False;
  ADataFileInfo := nil;

  try
    ADataFileInfo := DeriveDataFileInfo(FileName, True, aNotify);
  Except
    on E:EAbort do
    begin
      Exit;
    end;

    on E:EParserError do
    begin
      Exit; // in this case, we already informed the user and opened the input data file in the text editor
    end;

    on E:Exception do
    begin
      Result := False;
      ShowErrorMessage(E);
      Exit;
    end;
  end;

  try try
    Result := ReadFastaFile(ADataFileInfo, OpenInSDE);
  except
    On E:Exception do
    begin  // Note: Choosing to do the OpenAndFocus on error in ReadFastaFile instead of here.
      ShowErrorMessage(E);
      Result := False;
    end;
  end;
  finally
    if Assigned(ADataFileInfo) then
      ADataFileInfo.Free;
  end;
end;

function ProcessAlignmentSessionFile(FileName: String): Boolean;
var
  ErrorRow: Integer = -1;
  ARP: TRuntimeProgress;
begin
  ARP := nil;
  Result := False;
  try try
    ARP := TRuntimeProgress.Create(Application);
    ARP.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Reading Input Data File';
    ARP.AddRunStatusInfo('Data file', ExtractFileName(Filename));
    ARP.UpdateRunStatusInfo('Status', 'Reading session data');
    ARP.HideAnalysisOptions;
    ARP.Show;
    Application.ProcessMessages;
    {$IFNDEF VISUAL_BUILD}
    raise Exception.Create('Alignment session files are not supported by megacc. You must use a MEGA or FASTA formatted sequence alignment.');
    {$ENDIF}
    Result := RetrieveMegaAlignmentSession(FileName, ErrorRow, ARP);
    Application.ProcessMessages;
    ARP.Hide;
  except
    On E: Exception do
    begin
      if ARP <> nil then ARP.Visible := False;
      {$IFDEF VISUAL_BUILD}
      OpenFileAndFocus(Filename, ErrorRow, 0);
      {$ENDIF}
      ShowErrorMessage(E);
    end;
  end
  finally
    if ARP <> nil then
      ARP.Free;
  end;
end;

function  ProcessAlignDataFile(FileName: String): Boolean;
begin
  ShowMessage('Not yet implemented');
  Result := False;
end;

function ReadSequenceFile(FileName: String; HasPara: Boolean; ADataType, ADataFormat: TSnTokenCode; AMissSym, AIdenSym, AGapSym: AnsiChar; OpenInSDE: Boolean=True): Boolean;
var
  MyParser: TLexSeq = nil;
  ARP: TRuntimeProgress = nil;
  aRow, aCol: Integer;
begin
  Result := False;
  try
    try
      ARP := TRuntimeProgress.Create(Application);
      ARP.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Reading Input Data File';
      MyParser := InitSeqDataParser(FileName, ARP);
      if (Trim(MyParser.Title) <> EmptyStr) and (not SameText(MyParser.Title, 'untitled')) then
        ARP.AddRunStatusInfo('Data file', MyParser.Title)
      else
        ARP.AddRunStatusInfo('Data file', ExtractFileName(FileName));
      ARP.AddRunStatusInfo('Status', 'Reading header');
      ARP.HideAnalysisOptions;
      if not isPrototyper then
        ARP.Show;
      Result := DoReadSequenceFile(MyParser, ARP, HasPara, ADataType, aDataFormat, AMissSym, AIdenSym, AGapSym, OpenInSDE);
    except
      On E: Exception do
      begin
        FreeAndNil(MyParser);
        if ARP <> nil then
          ARP.Hide;
        {$IFDEF VISUAL_BUILD}
        aRow := MyParser.Row + 1;
        aCol := MyParser.Col;
        OpenFileAndFocus(Filename, aRow, aCol);
        raise Exception.Create(E.Message);
        {$ELSE}
        error_nv('Unable to open your file: ' + E.Message);
        {$ENDIF}
      end;
    end
  finally
    if Assigned(MyParser) then
      MyParser.Free;
    if Assigned(ARP) then
      ARP.Free;
  end;
end;

function ReadSequenceFile(FileStrings: TStringList; HasPara: Boolean; ADataType, ADataFormat: TSnTokenCode; AMissSym, AIdenSym, AGapSym: AnsiChar; OpenInSDE: Boolean): Boolean;
var
  MyParser: TLexSeq = nil;
  ARP: TRuntimeProgress = nil;
begin
  Result := False;

  try
    try
      ARP := TRuntimeProgress.Create(Application);
      ARP.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Reading Input Data File';
      MyParser := InitSeqDataParser(FileStrings, ARP);
      if (Trim(MyParser.Title) <> EmptyStr) and (not SameText(Trim(MyParser.Title), 'untitled')) then
        ARP.AddRunStatusInfo('Data file', MyParser.Title);
      ARP.AddRunStatusInfo('Status', 'Reading header');
      ARP.HideAnalysisOptions;
      if not isPrototyper then
        ARP.Show;
      Result := DoReadSequenceFile(MyParser, ARP, HasPara, ADataType, aDataFormat, AMissSym, AIdenSym, AGapSym, OpenInSDE);
  except
    On E: Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      OpenStringList(FileStrings, 'Sequence Data', False);
      raise Exception.Create(E.Message);
      {$ELSE}
      error_nv('Unable to open your file: ' + E.Message);
      {$ENDIF}
    end;
  end
  finally
    FreeAndNil(MyParser);
    FreeAndNil(ARP);
  end;
end;

function DoReadSequenceFile(MyParser: TLexSeq; ARP: TRuntimeProgress; HasPara: Boolean; aDataType, aDataFormat: TSnTokenCode; AMissSym, AIdenSym, AGapSym: AnsiChar; OpenInSDE: Boolean): Boolean;
begin
  Result := False;
  MyParser.ReadHeader;
  {$IFDEF VISUAL_BUILD}
  if (not MyParser.FoundFormat) and HasPara then
  begin
    MyParser.DataType     := ADataType;
    MyParser.DataFormat   := ADataFormat;
    MyParser.IdenticalSym := AIdenSym;
    MyParser.GapSym       := AGapSym;
    MyParser.MissingSym   := AMissSym;
  end;
  {$ELSE}
  D_MegaMain.DataTitle := MyParser.Title;
  if (myParser.FoundFormat) and HasPara then
  begin
    // If the file doesn't specify a format, then take the one from the MAO file.
    if MyParser.DataType = snNoToken then
      MyParser.DataType := ADataType;
    if MyParser.DataFormat = snNoToken then
      MyParser.DataFormat := ADataFormat;

    // Warn if the identical symbol, gap symbol, or missing symbol differ from the one specified in the mao file.
    if MyParser.IdenticalSym <> AIdenSym then
      Warn_nv('Warning: identical base symbol mismatch. MEGA Analysis Options file lists identical base symbol as ''' + AIdenSym + ''', while the input data file lists it as''' + MyParser.IdenticalSym + '''. Using ''' + MyParser.IdenticalSym + ''' to identify identical bases.');
    if MyParser.GapSym <> AGapSym then
      Warn_nv('Warning: gap symbol mismatch. MEGA Analysis Options file lists gap symbol as ''' + AGapSym + ''', while the input data file lists it as''' + MyParser.GapSym + '''. Using ''' + MyParser.GapSym + ''' to identify gaps.');
    if MyParser.MissingSym <> AMissSym then
      Warn_nv('Warning: missing symbol mismatch. MEGA Analysis Options file lists missing base symbol as ''' + AMissSym + ''', while the input data file lists it as''' + MyParser.MissingSym + '''. Using ''' + MyParser.MissingSym + ''' to identify missing symbols.');
  end
  else
  begin
    MyParser.DataType   := ADataType;
    MyParser.DataFormat   := ADataFormat;
    MyParser.IdenticalSym := AIdenSym;
    MyParser.GapSym       := AGapSym;
    MyParser.MissingSym   := AMissSym;
  end;
  {$ENDIF}

  ARP.UpdateRunStatusInfo('Status', 'Reading sequence data');
  Application.ProcessMessages;
  MyParser.ReadData;
  ARP.UpdateRunStatusInfo('Status', 'Storing information');
  Application.ProcessMessages;
  SetupSequenceInformation(MyParser, ARP, OpenInSDE);
  ARP.Hide;
  Result := True;
end;

function ReadDistFile(FileStrings: TStringList; HasPara: Boolean; ADataType, ADataFormat: TSnTokenCode): Boolean;
var
  MyParser: TLexDist = nil;
  ARP: TRuntimeProgress = nil;
begin
  Result := False;

  try
    try
      ARP := TRuntimeProgress.Create(Application);
      ARP.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Reading Input Data File';
      MyParser := InitDistDataParser(FileStrings, ARP);
      if (Trim(MyParser.Title) <> EmptyStr) and (not SameText(Trim(MyParser.Title), 'untitled')) then
        ARP.AddRunStatusInfo('Data file', MyParser.Title);
      ARP.AddRunStatusInfo('Status', 'Reading header');
      ARP.HideAnalysisOptions;
      if not isPrototyper then
        ARP.Show;
      Result := DoReadDistFile(MyParser, ARP, HasPara, ADataType, aDataFormat);
  except
    On E: Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      OpenStringList(FileStrings, 'Sequence Data', False);
      raise Exception.Create(E.Message);
      {$ELSE}
      error_nv('Unable to open your file: ' + E.Message);
      {$ENDIF}
    end;
  end
  finally
    FreeAndNil(MyParser);
    FreeAndNil(ARP);
  end;
end;

function DoReadDistFile(MyParser: TLexDist; ARP: TRuntimeProgress; HasPara: Boolean; ADataType, ADataFormat: TSnTokenCode): Boolean;
begin
  MyParser.SetKeywords(GetPrivateFile(mfKeywordsFile));
  MyParser.SetRuntimeProgress(ARP);
  {$IFDEF VISUAL_BUILD}
  MyParser.SetStopButton(ARP.StopBtn);
  {$ENDIF}
  MyParser.ReadHeader;

  if (not MyParser.FoundFormat) and HasPara then
  begin
    MyParser.DataType     := ADataType;
    MyParser.DataFormat   := ADataFormat;
  end;
  ARP.UpdateRunStatusInfo('Status', 'Reading distance matrix');
  MyParser.ReadData;
  ARP.UpdateRunStatusInfo('Status', 'Data read');
  ARP.UpdateRunStatusInfo('Status', 'Storing information');
  SetupDistanceInformation(MyParser, ARP);
  ARP.Hide;
  Result := True;
end;

function InitSeqDataParser(FileName: String; var ARP: TRuntimeProgress): TLexSeq;
begin
  Result := TLexSeq.Create;
  Result.SetKeywords(GetPrivateFile(mfKeywordsFile));
  Result.SetDataFile(FileName);
  Result.SetRuntimeProgress(ARP);
  {$IFDEF VISUAL_BUILD}
  Result.SetStopButton(ARP.StopBtn);
  {$ENDIF}
end;

function InitSeqDataParser(var FileStrings: TStringList; var ARP: TRuntimeProgress): TLexSeq;
begin
  Result := TLexSeq.Create;
  Result.SetKeywords(GetPrivateFile(mfKeywordsFile));
  Result.SetDataFromStringList(FileStrings);
  Result.SetRuntimeProgress(ARP);
  {$IFDEF VISUAL_BUILD}
  Result.SetStopButton(ARP.StopBtn);
  {$ENDIF}
end;

function InitDistDataParser(var FileStrings: TStringList; var ARP: TRuntimeProgress): TLexDist;
begin
  Result := TLexDist.Create;
  Result.SetKeywords(GetPrivateFile(mfKeywordsFile));
  Result.SetDataFromStringList(FileStrings);
  Result.SetRuntimeProgress(ARP);
  {$IFDEF VISUAL_BUILD}
  Result.SetStopButton(ARP.StopBtn);
  {$ENDIF}
end;

procedure SetupConcatenatedSequencesInformation(seqs: TSequenceList; dInfo: TAllDomainInfo; ARP: TRuntimeProgress; OpenInSDE: Boolean);
var
  p: PAnsiChar = nil;
  i: Integer = -1;
  j: Integer = -1;
  {$IFDEF VISUAL_BUILD}
  CodeTableDlg1: TSelectGeneticCodeDlg = nil;
  {$ELSE}
  CodeTableName: String;
  NonCodingDomainFound: Boolean;
  {$ENDIF}
  CodeTableList : TStringList = nil;
  ADataType    : TSnTokenCode;
  ANoOfOtus    : LongInt;
  ANoOfSites   : LongInt;
  ANoOfDomains : LongInt;
  AOtuInfo         : TAllOtuInfo = nil;
  AllSiteDomainMark : TAllSiteDomainMark = nil;

  function GetDataTypeString: String;
  begin
    if AllSiteDomainMark[0].IsCoding then
      Result := MapTokenCodeToDataTypeString(snCoding)
    else
      Result := MapTokenCodeToDataTypeString(ADataType);
  end;

begin
  {$IFDEF VISUAL_BUILD}
  TaxaGpsDlg     := nil;
  GeneDomainDlg  := nil;
  {$ENDIF}
  try
    try
      D_InputSeqData := TD_InputSeqData.Create;
      D_InputSeqData.SourceFileName := 'Concatenated Sequences';
      D_InputSeqData.ARP := ARP;
      ANoOfOtus := seqs.Count;
      ANoOfSites := seqs[0].NoOfSites;
      if seqs.IsDNA then
        ADataType := snNucleotide
      else
        ADataType := snProtein;
      ARP.UpdateRunStatusInfo('Status', 'Setting up Sequence names');
      AOtuInfo := TAllOtuInfo.Create;
      AOtuInfo.NoOfOtus := ANoOfOtus;

      ARP.UpdatePercentProgress(0);
      for i := 0 to ANoOfOtus - 1 do
      begin
        AOtuInfo[i] := TOtuInfo.Create;
        with AOtuInfo[i] do
        begin
          Id := i;
          Name := seqs[i].SeqName;
          OutgroupMember := False;
          IsUsed := True;

          GetMem(p, (ANoOfSites + 1)*SizeOf(PAnsiChar));
          for j := 0 to ANoOfSites - 1 do
            p[j] := seqs[i].SeqData[j + 1];
          p[ANoOfSites] := #0;
          AOtuInfo[i].Data := p;
          ARP.UpdatePercentProgress((i + 1)*100 div ANoOfOtus);
          Application.ProcessMessages;
        end;
      end;

      {$IFDEF VISUAL_BUILD}
      if OpenInSDE then
        TaxaGpsDlg := TTaxaGpsDlg.Create(Application);
      {$ENDIF}

      ARP.UpdateRunStatusInfo('Status', 'Setting up genes/domains information');
      Application.ProcessMessages;
      ANoOfDomains := dInfo.NoOfDomains;
      {$IFDEF VISUAL_BUILD}
      if OpenInSDE then
        GeneDomainDlg := TGeneDomainDlg.Create(Application);
      {$ENDIF}

      AllSiteDomainMark := TAllSiteDomainMark.Create;
      AllSiteDomainMark.NoOfSites := ANoofSites;

      for i := 0 to ANoOfDomains - 1 do
        with dInfo[i] do
          if IsDomain and (FromSite >= 0) then
            for j:= FromSite to ToSite do
               AllSiteDomainMark[j] := dInfo[i];

      D_InputSeqData.DomainMarks  := Pointer(AllSiteDomainMark);
      D_InputSeqData.AllDomainInfo := Pointer(dInfo);

      {$IFDEF VISUAL_BUILD}
      if ADataType = snNucleotide then
      begin
        if ARP.Visible then
          ARP.Hide;

        if MessageDlg('Protein-coding nucleotide sequence data?', mtConfirmation, [mbYes,mbNo],0) = mrYes then
        begin
          for i := 0 to dInfo.NoOfDomains - 1 do
          begin
            dInfo[i].IsCoding := True;
            dInfo[i].CodonStart := 0;
          end;
        end;
      end;
      {$ELSE}
        if seqs.IsProteinCoding then
        begin
          D_InputSeqData.IsCoding := True;
          if AllSiteDomainMark.NoOfSites > 0 then
            for i := 0 to AllSiteDomainMark.NoOfSites - 1 do
            begin
              if Assigned(AllSiteDomainMark[i]) then
              begin
                if not AllSiteDomainMark[i].IsCoding then
                begin
                  AllSiteDomainMark[i].IsCoding   := True;
                  AllSiteDomainMark[i].CodonStart := 0;
                end;
              end;
            end;
        end;
      {$ENDIF}

      ARP.UpdateRunStatusInfo('Status', 'Organizing sequence information');
      Application.ProcessMessages;
      VS_SeqDataExplorer := TVS_SeqDataExplorer.Create;

      {$IFDEF VISUAL_BUILD}
      if OpenInSde then
      begin
        V_SeqDataExplorer := TV_SeqDataExplorer.Create(Application);
        V_SeqDataExplorer.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Sequence Data Explorer (Concatenated Alignments)';
      end;

      if (not IsPrototyper) and (seqs.MaxNoOfSites*seqs.Count > SDE_MAX_TOTAL_SITES) then
      begin
        if not Assigned(MegaInfoForm) then
          MegaInfoForm := TMegaInfoForm.Create(MegaForm);
        MegaInfoForm.SetInfoText(MessageStringForDisabledSeqDataExplorer(GetDataTypeString, ANoOfOtus, ANoOfSites));
        MegaForm.ExplorersDisabledForLargeData := True;
      end
      else
        MegaForm.ExplorersDisabledForLargeData := False;
      {$ENDIF}

      D_InputSeqData.IsNuc := (ADataType = snNucleotide);
      D_InputSeqData.IsAmino := (ADataType = snProtein);

      // code table selection
      if ((ADataType = snNucleotide) and AllSiteDomainMark.IsCoding)  then
      begin
        D_InputSeqData.IsCoding  := True;
        {$IFDEF VISUAL_BUILD}
        CodeTableDlg1 := TSelectGeneticCodeDlg.Create(Application);
        if CodeTableDlg1.ShowModal <> mrOK then
        begin
          ALLSiteDomainMark[0].IsCoding   := False;
          AllSiteDomainMark[0].CodonStart := -1;
          D_InputSeqData.IsCoding    := False;
        end;
        {$ELSE}
        CodeTableList := TStringList.Create;
        CodeTableList.LoadFromFile(GetPrivateFile(mfCodeTables));
        {$ENDIF}
      end
      else
        D_InputSeqData.IsCoding  := False;

      if AllSiteDomainMark.IsCoding then
      begin
      {$IFDEF VISUAL_BUILD}
        D_InputSeqData.CodeName  := CodeTableDlg1.CodeTableName;
        D_InputSeqData.CodeTable := CodeTableDlg1.CodeTable;
      {$ELSE}
        if CodeTableName <> EmptyStr then
        begin
          D_InputSeqData.CodeName := CodeTableName;
          try
            D_InputSeqData.CodeTable := CodeTableList.Values[D_InputSeqData.CodeName];
          except
            on E : Exception do
              Warn_nv('Unable to load code table ' + D_InputSeqData.CodeName + ' if this file requires a custom code table please specify it as seen in the help file');
          end;
        end;
      {$ENDIF}
      end;

      //D_InputSeqData.GapSym    := Parser.GapSym;
      //D_InputSeqData.MissSym   := Parser.MissingSym;
      //D_InputSeqData.IdenSym   := Parser.IdenticalSym;
      D_InputSeqData.NoOfTaxa  := ANoOfOtus;
      D_InputSeqData.NoOfSites := ANoOfSites;
      D_InputSeqData.OtuInfos  := Pointer(AOtuInfo);
      D_InputSeqData.DomainMarks := Pointer(AllSiteDomainMark);

      D_InputSeqData.Initialize;
      ARP.UpdatePercentProgress(80);
      {$IFDEF VISUAL_BUILD}
      if OpenInSDE then
      begin
        V_SeqDataExplorer.Initialize;
        ARP.UpdatePercentProgress(90);
        Application.ProcessMessages;
      end;
      {$ENDIF}
      if OpenInSDE then
        VS_SeqDataExplorer.Initialize;
      AllSiteDomainMark.IsDirty := False;
      AOtuInfo.IsDirty    := False;

      {$IFDEF VISUAL_BUILD}
      if seqs.IsDNA then
        MegaForm.UpdateMainWindow('Concatenated alignments', EmptyStr, 'Concatenated DNA alignments', snNucleotide)
      else
        MegaForm.UpdateMainWindow('Concatenated alignments', EmptyStr, 'Concatenated protein alignments', snProtein);
      {$ENDIF}
      ARP.UpdatePercentProgress(100);
      D_InputSeqData.ARP := nil;
    except
      On E: Exception do
      begin
        ShowErrorMessage(E);
        FreeAndNil(D_InputSeqData);
        {$IFDEF VISUAL_BUILD}
        if V_SeqDataExplorer <> nil then
          FreeAndNil(V_SeqDataExplorer);
        {$ENDIF}
        if VS_SeqDataExplorer <> nil then
          FreeAndNil(VS_SeqDataExplorer);
        {$IFDEF VISUAL_BUILD}
        if GeneDomainDlg <> nil then
          FreeAndNil(GeneDomainDlg);
        if TaxaGpsDlg <> nil then
          FreeAndNil(TaxaGpsDlg);
        MegaForm.Update;
        {$ENDIF}
      end;
    end;
  finally
    if Assigned(CodeTableList) then
      CodeTableList.Free;
  end
end;

function ReadNexusFile(DataFileInfo: TDataFileInfo; OpenInSDE: Boolean): Boolean;
var
  ErrorRow: Integer = -1;
  ErrorCol: Integer = -1;
  ErrorMsg: String = '';
  ARP: TRuntimeProgress;
begin
  ARP      := nil;
  Result := False;
  try try
    // ARP Setup
    ARP := TRuntimeProgress.Create(Application);
    ARP.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Reading Input Nexus File';
    ARP.AddRunStatusInfo('Data file', DataFileInfo.GetFilename);
    ARP.UpdateRunStatusInfo('Status', 'Reading sequence data');
    ARP.HideAnalysisOptions;
    ARP.Show;
    Application.ProcessMessages;

    // Read Data & Setup Sequences
    Result := ParseNexusData(DataFileInfo, ErrorRow, ErrorCol, ErrorMsg, ARP, OpenInSDE);
    Application.ProcessMessages;
    ARP.Hide;
    {$IFDEF VISUAL_BUILD}
    if not Result then
    begin
      OpenFileAndFocus(DataFileInfo.GetFilename, ErrorRow, ErrorCol);
      ShowMessage(ErrorMsg);
    end;
    {$ENDIF}
  except
    On E: Exception do
    begin
      if ARP <> nil then ARP.hide;
      {$IFDEF VISUAL_BUILD}
      OpenFileAndFocus(DataFileInfo.GetFilename, ErrorRow, 0);
      ShowErrorMessage(E);
      {$ELSE}
      error_nv('An error occurred when parsing the NEXUS file at line ' + IntToStr(ErrorRow) + ': ' + E.Message);
      {$ENDIF}
    end;
  end
  finally
    if ARP <> nil then
      ARP.Free;
  end;
end;

function ParseFastaData(DataFileInfo: TDataFileInfo; var ErrorRow: Integer; ARP: TRuntimeProgress; OpenInSDE: Boolean=True): Boolean;
const
  MEM_INCREMENT = 4096;
var
  Lines: Integer;
  FTaxa: Array of PAnsiChar;
  FSequence: Array of PAnsiChar;
  FNoOfTaxa: LongInt;
  FNoOfSites: LongInt;
  UnknownNoOfSites: Boolean;
  updateTime: TDateTime;
  scanner: TCharScanner = nil;

  procedure ReadGreaterThan;
  var
    letter: AnsiChar = #0;
  begin
    scanner.Read(letter);
    if letter <> '>' then
      Raise Exception.Create('Fasta parse error.  Expected ''>'', but found ' + letter + '.  Make sure your file is aligned before analysis, all sequences must be the same length.');
  end;

  procedure ReadName;
  var
    charIn: AnsiChar = #0;
    Name: AnsiString = '';
    PName: PAnsiChar;
  begin
    while scanner.HasNext do
    begin
      scanner.Read(charIn);
      if (charIn = #10) or (charIn = #13) then // had to implement an EOL checker myself.
        break;
      Name := Name + charIn;
    end;
    if charIn = #13 then // If we read in a CR then there's still a LF which has to be read off before we have the next line.
      scanner.Read(charIn);
    inc(Lines);
    TrimTaxaName(Name);
    if (Name = EmptyStr) then
      Raise Exception.Create('Fasta parse error.  Expected a name to follow ''>'', but none found');

    inc(FNoOfTaxa);
    if Length(FTaxa) = FNoOfTaxa then
    begin
      SetLength(FTaxa, FNoOfTaxa + MEM_INCREMENT);
      SetLength(FSequence, FNoOfTaxa + MEM_INCREMENT);
    end;
    GetMem(PName, (sizeof(AnsiChar)*length(Name))+1);
    StrCopy(PName, PAnsiChar(Name));
    FTaxa[FNoOfTaxa-1] := PName;
  end;

  procedure ReadSequence;
    function IsValidNucBase(c: AnsiChar): Boolean;
    begin
      if (c = DataFileInfo.GetMissingBaseSymbol) or (c = DataFileInfo.GetIdenticalBaseChar) or (c = DataFileInfo.GetGapSymbol) then
      begin
        Result := True;
        Exit;
      end;
      Result := False;
      case UpCase(c) of
      'A'..'D','G','H','K','M','R','S','T','U','V','W','Y','N': Result := True;
      end;
    end;

    function IsValidAmino(c: AnsiChar): Boolean;
    begin
      if (c = DataFileInfo.GetMissingBaseSymbol) or (c = DataFileInfo.GetIdenticalBaseChar) or (c = DataFileInfo.GetGapSymbol) then
      begin
        Result := True;
        Exit;
      end;
      Result := False;
      case UpCase(c) of
      'A'..'I','K'..'N', 'P'..'T', 'V'..'Z', '*': Result := True;
      end;
    end;

  var
    Seq: AnsiString = '';
    PSeq: PAnsiChar;
    charIn: AnsiChar = #0;
    FoundGreaterThan: Boolean = False;
    SiteNo: Integer = 0;
  begin
    while (not FoundGreaterThan) and scanner.HasNext do
    begin
      if not scanner.Read(charIn) then
        break;
      if (charIn = #13) or (charIn = #10) or (charIn = ' ') then
      begin
        if charIn = #10 then
          inc(Lines);
      end
      else
      begin
        if not UnknownNoOfSites then
        begin
          if SiteNo = FNoOfSites then // This char MUST be a >, the start of a new seq or we have different length seqs.
          begin
            if (charIn <> '>') and scanner.HasNext then
              Raise Exception.Create('Fasta parse error.  Expected ''>'', but found ' + charIn + '.  Sequences are different lengths, check that they have been aligned BEFORE analysis.');
          end
          else
          begin
            if (charIn = '>') and (SiteNo > 0) then // We have encountered the name of another seq before this one was finished, different seq lengths.
              Raise Exception.Create('Fasta parse error.  Expected sequence character, but found ''>''.  Sequences are different lengths.');
          end;
        end;
        inc(SiteNo);
        if charIn = '>' then
          FoundGreaterThan := True
        else
        begin
          if charIn = DataFileInfo.GetIdenticalBaseChar then // Replace the identical symbol with the actual symbol it is identical to.
          begin
            if FNoOfTaxa = 1 then
              Raise Exception.Create('Fasta parse error.  An identical symbol was found in the first sequence.');
            charIn := FSequence[0][SiteNo];
          end;
          if DataFileInfo.GetDataType = snNucleotide then
          begin
            if not IsValidNucBase(charIn) then // Make sure it is a valid nucleotide, allow gaps
              Raise Exception.Create('Invalid base found.  Expecting a nucleotide base, but found ''' + charIn + ''' ');
          end
          else
          begin
            if not IsValidAmino(charIn) then // allow for gaps, added * for amino acid.
              Raise Exception.Create('Invalid Amino found.  Expecting an amino acid, but found ''' + charIn + ''' ');
          end;
          if UnknownNoOfSites then
            Seq := Seq + charIn
          else
          begin
            if Length(Seq) < FNoOfSites then
              SetLength(Seq, FNoOfSites);
            Seq[SiteNo] := charIn;
          end;
        end;
      end;
    end;
    if (Trim(Seq) = EmptyStr) then
      Raise Exception.Create('FastA data parsing error.  Expected a sequence, but none found.');

    if UnknownNoOfSites then
    begin
      UnknownNoOfSites := False;
      FNoOfSites := length(Seq);
      GetMem(PSeq, (sizeof(AnsiChar)*length(Seq))+1);
    end
    else
      GetMem(PSeq, (sizeof(AnsiChar)*FNoOfSites)+1);
    StrCopy(PSeq, PAnsiChar(Seq));
    FSequence[FNoOfTaxa-1] := PSeq;
  end;
begin
  updateTime := Now;
  Result := False;
  FNoOfTaxa := 0;
  Lines := 1;
  UnknownNoOfSites := True;
  SetLength(FTaxa, MEM_INCREMENT);
  SetLength(FSequence, MEM_INCREMENT);
  try
    try
      ARP.IsMarqueeMode := True;
      scanner := TCharScanner.Create(DataFileInfo.GetFileName);
      ReadGreaterThan;
      while scanner.HasNext do
      begin
        if MilliSecondsBetween(Now, updateTime) > 200 then
        begin
          ARP.UpdateRunStatusInfo('Status', Format('Found %.0n sequences', [FNoOfTaxa*1.0]));
          Application.ProcessMessages;
          updateTime := Now;
        end;
        ReadName;
        ReadSequence;
      end;
      SetLength(FTaxa, FNoOfTaxa);
      SetLength(FSequence, FNoOfTaxa);
    Except
      on E: Exception do
      begin
        ErrorRow := Lines-1;
        Raise Exception.Create(E.Message + ' On line: ' + IntToStr(Lines));
      end;
    end;
  finally
    ARP.IsMarqueeMode := False;
    if Assigned(scanner) then
      scanner.Free;
  end;
  SetupFastaSequenceInformation(DataFileInfo, FNoOfTaxa, FNoOfSites, FTaxa, FSequence, ARP, OpenInSDE);
end;

function ParseNexusData(DataFileInfo: TDataFileInfo; var ErrorRow: Integer; var ErrorCol: Integer; var Msg: String; ARP: TRuntimeProgress; OpenInSDE: Boolean): Boolean;
var
  ANexusFile: TNexusFile;
begin
  ANexusFile := nil;
  Result := False;

  try
    ANexusFile := TNexusFile.Create;
    ANexusFile.Filename := DataFileInfo.GetFileName;
    if not ANexusFile.Parse then
    begin
      ErrorRow := ANexusFile.ErrorLine;
      ErrorCol := ANexusFile.ErrorColumn;
      Msg := ANexusFile.ErrorMsg;
    end
    else
    begin
      if ANexusFile.NumCharacterBlocks > 0 then
      begin
        SetupNexusSequenceInformation(ANexusFile, ARP, OpenInSDE);
        Result := True;
      end
      else
      begin
        Msg := 'No CHARACTERS block was found in the NEXUS file';
        Result := False;
      end;
    end;
  except
    on E:Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Oh no! An error occurred while parsing the NEXUS file: ' + E.Message);
      {$ELSE}
      error_nv('Error parsing Nexus file: ' + E.Message);
      {$ENDIF}
    end;
  end;
end;

function RetrieveMegaAlignmentSession(FileName: String; var ErrorRow: Integer; ARP: TRuntimeProgress): Boolean;
var
  pleaseWait: TPleaseWait = nil;
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
  try
    if FindAlignmentEditorWindow(False) <> nil then
    begin
      pleaseWait := TPleaseWait.Create(nil);
      Result := AlignEditMainForm.RetrieveSession(Filename, pleaseWait);
      AlignEditMainForm.ActionAnalyzeExecute(nil);
      AlignEditMainForm.Hide;
    end
    else
      raise Exception.Create('failed to initialize the Alignment Editor form');
  finally
    if Assigned(pleaseWait) then
      pleaseWait.Free;
  end;
  {$ENDIF}
end;

function ReadFastaFile(DataFileInfo: TDataFileInfo; OpenInSDE: Boolean=True): Boolean;
var
  ErrorRow: Integer = -1;
  ARP: TRuntimeProgress;
begin
  ARP      := nil;
  Result := False;
  try try
    ARP := TRuntimeProgress.Create(Application);
    ARP.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Reading Input Data File';
    ARP.AddRunStatusInfo('Data file', DataFileInfo.GetFilename);
    ARP.UpdateRunStatusInfo('Status', 'Reading sequence data');
    ARP.HideAnalysisOptions;
    ARP.Show;
    Application.ProcessMessages;
    ParseFastaData(DataFileInfo, ErrorRow, ARP, OpenInSDE);
    Application.ProcessMessages;
    ARP.Hide;
    Result := True;
  except
    On E: Exception do
    begin
      if ARP <> nil then ARP.hide;
      {$IFDEF VISUAL_BUILD}
      OpenFileAndFocus(DataFileInfo.GetFilename, ErrorRow, 0);
      raise Exception.Create(E.Message);
      {$ELSE}
      error_nv('Unable to read your file, error at line ' + IntToStr(ErrorRow) + ' details: ' + E.Message);
      {$ENDIF}
    end;
  end
  finally
    if ARP <> nil then
      ARP.Free;
  end;
end;

procedure SetupSequenceInformation(Parser: TLexSeq; ARP: TRuntimeProgress; OpenInSDE: Boolean=True);
var
  i:   Integer;
  {$IFDEF VISUAL_BUILD}
  CodeTableDlg1: TSelectGeneticCodeDlg = nil;
  {$ELSE}
  NonCodingDomainFound: Boolean;
  {$ENDIF}
  CodeTableName: String = '';
  NeedToShowCodeTableDlg : Boolean;
  CodeTableList : TStringList = nil;
  ADataType    : TSnTokenCode;
  ANoOfOtus    : LongInt;
  ANoOfSites   : LongInt;
  ANoOfDomains : LongInt;
  AOtuInfo         : TAllOtuInfo = nil;
  ADomainInfoList  : TAllDomainInfo = nil;
  AllSiteDomainMark : TAllSiteDomainMark = nil;

  function GetDataTypeString: String;
  begin
    if AllSiteDomainMark[0].IsCoding then
      Result := MapTokenCodeToDataTypeString(snCoding)
    else
      Result := MapTokenCodeToDataTypeString(ADataType);
  end;

begin
  {$IFDEF VISUAL_BUILD}
  TaxaGpsDlg     := nil;
  GeneDomainDlg  := nil;
  {$ENDIF}
  D_InputSeqData := nil;
  try
    try
      D_InputSeqData := TD_InputSeqData.Create;
      D_InputSeqData.SourceFileName := Parser.FileName;
      D_InputSeqData.ARP := ARP;
      ANoOfOtus    := Parser.NoOfOtus;
      ANoOfSites   := Parser.NoOfSites;
      ADataType    := Parser.DataType;
      ARP.UpdateRunStatusInfo('Status', 'Setting up Sequence/group names');
      AOtuInfo := TAllOtuInfo.Create;
      AOtuInfo.NoOfOtus := ANoOfOtus;

      ARP.UpdatePercentProgress(0);
      for i:=0 to ANoOfOtus-1 do
      begin
        AOtuInfo[i] := TOtuInfo.Create;
        with AOtuInfo[i] do
        begin
          Id     := i;
          Name   := Parser.Taxon[i];
          GpName := Parser.GpName[i];
          SpName := Parser.SpName[i];
          PopName := Parser.PopName[i];
          OutgroupMember := Parser.IsOutgroupMember[i];
          IsUsed := True;
          Data := Parser.Sequence[i];  // memory is transferred
          AssignGeographicalInfo(Parser.GeographicalInfo[i]);
          ARP.UpdatePercentProgress((i+1)*100 div ANoOfOtus);
          Application.ProcessMessages;
        end;
      end;

      {$IFDEF VISUAL_BUILD}
      if OpenInSDE then
        TaxaGpsDlg := TTaxaGpsDlg.Create(Application);
      {$ENDIF}

      ARP.UpdateRunStatusInfo('Status', 'Setting up genes/domains information');
      Application.ProcessMessages;
      ANoOfDomains := Parser.NoOfDomains;
      {$IFDEF VISUAL_BUILD}
      if OpenInSDE then
        GeneDomainDlg := TGeneDomainDlg.Create(Application);
      {$ENDIF}
      Parser.ConstructAllSiteDomainMark;
      AllSiteDomainMark := Parser.AllSiteDomainMark;
      D_InputSeqData.DomainMarks  := Pointer(AllSiteDomainMark);
      ADomainInfoList  := Pointer(Parser.AllDomainInfo);
      D_InputSeqData.AllDomainInfo := Pointer(ADomainInfoList);

      {$IFDEF VISUAL_BUILD}
      if (ADataType = snNucleotide) and (not Parser.FoundFormat) and (ANoOfDomains < 2) then
      begin
        if ARP.Visible then
          ARP.Hide;
        // only one domain exists; created artificially
        if MessageDlg('Protein-coding nucleotide sequence data?', mtConfirmation, [mbYes,mbNo],0) = mrYes then
        begin
          AllSiteDomainMark[0].IsCoding   := True;
          AllSiteDomainMark[0].CodonStart := 0;
        end;
      end;
      {$ELSE}
        if D_MegaMain.FFileShouldContainCoding then
        begin
          NonCodingDomainFound := False;
          D_InputSeqData.IsCoding := True;
          if AllSiteDomainMark.NoOfSites > 0 then
            for i := 0 to AllSiteDomainMark.NoOfSites - 1 do
            begin
              if Assigned(AllSiteDomainMark[i]) then
              begin
                if not AllSiteDomainMark[i].IsCoding then
                begin
                  AllSiteDomainMark[i].IsCoding   := True;
                  AllSiteDomainMark[i].CodonStart := 0;  {todo -oSudhir : eventually there should be a flag to specify the codon start (if not 0)}
                  if NonCodingDomainFound = False then
                    Warn_nv('One or more domains not specified as having coding data were found but the mao file calls for coding data. All domains will be treated as having coding data');
                  NonCodingDomainFound := True;
                end;
              end;
            end;
        end;
      {$ENDIF}

      ARP.UpdateRunStatusInfo('Status', 'Organizing sequence information');
      Application.ProcessMessages;
      VS_SeqDataExplorer := TVS_SeqDataExplorer.Create;

      {$IFDEF VISUAL_BUILD}
      if OpenInSDE then
      begin
        V_SeqDataExplorer := TV_SeqDataExplorer.Create(Application);
        V_SeqDataExplorer.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Sequence Data Explorer (' + ExtractFileName(Parser.FileName) + ')';
      end;

      if (not IsPrototyper) and (Parser.NoOfOtus*Parser.NoOfSites > SDE_MAX_TOTAL_SITES) then
      begin
        if not Assigned(MegaInfoForm) then
          MegaInfoForm := TMegaInfoForm.Create(MegaForm);
        MegaInfoForm.SetInfoText(MessageStringForDisabledSeqDataExplorer(GetDataTypeString, ANoOfOtus, ANoOfSites));
        MegaForm.ExplorersDisabledForLargeData := True;
      end
      else
        MegaForm.ExplorersDisabledForLargeData := False;
      {$ENDIF}


      D_InputSeqData.IsNuc := (ADataType = snNucleotide);
      D_InputSeqData.IsAmino := (ADataType = snProtein);

      // code table selection
      if ((ADataType = snNucleotide) and AllSiteDomainMark.IsCoding)  then
      begin
        D_InputSeqData.IsCoding  := True;
        {$IFDEF VISUAL_BUILD}
        CodeTableDlg1 := TSelectGeneticCodeDlg.Create(Application);
        {$ELSE}
        CodeTableList := TStringList.Create;
        CodeTableList.LoadFromFile(GetPrivateFile(mfCodeTables));
        {$ENDIF}
        NeedToShowCodeTableDlg := True;
        if Length(Parser.CodeTableName) > 0 then
        begin
          try
            CodeTableName := Parser.CodeTableName;
            {$IFDEF VISUAL_BUILD}
            CodeTableDlg1.CodeTableName := Parser.CodeTableName;
            {$ENDIF}
            NeedToShowCodeTableDlg := False;
          except
            On E: Exception do ShowMessage(E.Message);
          end;
        end;

        if NeedToShowCodeTableDlg then
        {$IFDEF VISUAL_BUILD}
          if (CodeTableDlg1.ShowModal <> mrOK) and (not Parser.FoundFormat) then
          begin
            ALLSiteDomainMark[0].IsCoding   := False;
            AllSiteDomainMark[0].CodonStart := -1;
            D_InputSeqData.IsCoding    := False;
          end;
          {$ENDIF}
      end
      else
        D_InputSeqData.IsCoding  := False;

      if AllSiteDomainMark.IsCoding then
      begin
      {$IFDEF VISUAL_BUILD}
        D_InputSeqData.CodeName  := CodeTableDlg1.CodeTableName;
        D_InputSeqData.CodeTable := CodeTableDlg1.CodeTable;
      {$ELSE}
        if CodeTableName <> EmptyStr then
        begin
          D_InputSeqData.CodeName := CodeTableName;
          try
            D_InputSeqData.CodeTable := CodeTableList.Values[D_InputSeqData.CodeName];
          except
            on E : Exception do
              Warn_nv('Unable to load code table ' + D_InputSeqData.CodeName + ' if this file requires a custom code table please specify it as seen in the help file');
          end;
        end
        else
        begin
          if (Length(D_MegaMain.ProcessPack.TextualSettingsList.Values['Genetic Code']) <> 0) and (not SameText(D_MegaMain.ProcessPack.TextualSettingsList.Values['Genetic Code'], 'Not Applicable')) then
            D_InputSeqData.CodeTable := D_MegaMain.ProcessPack.TextualSettingsList.Values['Genetic Code'];
        end;
      {$ENDIF}
      end;

      D_InputSeqData.GapSym    := Parser.GapSym;
      D_InputSeqData.MissSym   := Parser.MissingSym;
      D_InputSeqData.IdenSym   := Parser.IdenticalSym;
      D_InputSeqData.NoOfTaxa  := ANoOfOtus;
      D_InputSeqData.NoOfSites := ANoOfSites;
      D_InputSeqData.OtuInfos  := Pointer(AOtuInfo);
      D_InputSeqData.DomainMarks := Pointer(AllSiteDomainMark);

      D_InputSeqData.Initialize;
      ARP.UpdatePercentProgress(80);
      {$IFDEF VISUAL_BUILD}
      if OpenInSDE then
      begin
        V_SeqDataExplorer.Initialize;
        ARP.UpdatePercentProgress(90);
        Application.ProcessMessages;
      end;
      {$ENDIF}
      if OpenInSDE then
        VS_SeqDataExplorer.Initialize;
      AllSiteDomainMark.IsDirty := False;
      AOtuInfo.IsDirty    := False;

      {$IFDEF VISUAL_BUILD}
      MegaForm.UpdateMainWindow(Parser.FileName, Parser.Title, Parser.Description, Parser.FDataType);
      {$ENDIF}
      ARP.UpdatePercentProgress(100);
      D_InputSeqData.ARP := nil;
    except
      On E: Exception do
      begin
        ShowErrorMessage(E);
        FreeAndNil(D_InputSeqData);
        {$IFDEF VISUAL_BUILD}
        if V_SeqDataExplorer <> nil then
          FreeAndNil(V_SeqDataExplorer);
        {$ENDIF}
        if VS_SeqDataExplorer <> nil then
          FreeAndNil(VS_SeqDataExplorer);
        {$IFDEF VISUAL_BUILD}
        if GeneDomainDlg <> nil then
          FreeAndNil(GeneDomainDlg);
        if TaxaGpsDlg <> nil then
          FreeAndNil(TaxaGpsDlg);
        MegaForm.Update;
        {$ENDIF}
      end;
    end;
  finally
    if Assigned(CodeTableList) then
      CodeTableList.Free;
  end
end;

procedure SetupNexusSequenceInformation(NexusFile: TNexusFile; ARP: TRuntimeProgress; OpenInSDE: Boolean);
var
  i:   Integer;
  CurSeq: PAnsiChar;
  SeqDataBlock: TCharactersBlock;
  TempName: AnsiString;
  {$IFDEF VISUAL_BUILD}
  CodeTableDlg1: TSelectGeneticCodeDlg;
  {$ENDIF}
  ADataType    : TSnTokenCode;
  ANoOfOtus    : LongInt;
  ANoOfSites   : LongInt;
  ANoOfDomains : LongInt;
  AOtuInfo         : TAllOtuInfo;
  ADomainInfoList  : TAllDomainInfo;
  ADomainMark      : TAllSiteDomainMark;

  function GetDataTypeString: String;
  begin
    if ADomainMark[0].IsCoding then
      Result := MapTokenCodeToDataTypeString(snCoding)
    else
      Result := MapTokenCodeToDataTypeString(ADataType);
  end;

  procedure ConstructAllSiteDomainMark;
  var
    j: Integer;
    ADomainInfo: TDomainInfo;
  begin
    if ADomainMark <> nil then
      Exit;

    ADomainMark := TAllSiteDomainMark.Create;
    ADomainMark.NoOfSites := SeqDataBlock.NumSites;
    ADomainInfo := TDomainInfo.Create;
    ADomainInfo.Name := 'Data';
    ADomainInfo.FromSite := 0;
    ADomainInfo.ToSite   := SeqDataBlock.NumSites - 1;
    ADomainInfoList.Add(ADomainInfo);

    with ADomainInfoList[0] do // No domains besides Data
      if IsDomain and (FromSite >= 0) then
        for j:= FromSite to ToSite do
           ADomainMark[j] := ADomainInfoList[0];
  end;

  function GetPointerString(AString: String): PAnsiChar;
  begin
    GetMem(Result, (sizeof(AnsiChar)*length(AString))+1);
    StrCopy(Result, PAnsiChar(AString));
  end;

begin
{$IFDEF VISUAL_BUILD}
  TaxaGpsDlg      := nil;
  GeneDomainDlg   := nil;
  CodeTableDlg1   := nil;
{$ENDIF}
  if Assigned(D_InputSeqData) then
    FreeAndNil(D_InputSeqData)
  else
    D_InputSeqData  := nil;
  ADomainMark     := nil;
  ADomainInfoList := TAllDomainInfo.Create;
  AOtuInfo        := nil;

  try
    D_InputSeqData := TD_InputSeqData.Create;
    D_InputSeqData.SourceFileName := NexusFile.FileName;
    D_InputSeqData.NexusFile := NexusFile;
    SeqDataBlock := NexusFile.SelectedCharsBlock;
    ANoOfOtus    := SeqDataBlock.NumSequences;
    ANoOfSites   := SeqDataBlock.NumSites;
    ADataType    := SeqDataBlock.DataType;

    { setup OtuInfos}
    ARP.UpdateRunStatusInfo('Status', 'Setting up sequence information');
    AOtuInfo := TAllOtuInfo.Create;
    AOtuInfo.NoOfOtus := ANoOfOtus;

    ARP.UpdatePercentProgress(0);
    for i:=0 to ANoOfOtus-1 do
    begin
      AOtuInfo[i] := TOtuInfo.Create;
      with AOtuInfo[i] do
      begin
        Id     := i;
        TempName := SeqDataBlock.Labels[i];
        TrimTaxaName(TempName);
        Name   := GetPointerString(TempName);
        GpName := EmptyStr;
        IsUsed := True;
        CurSeq := GetPointerString(SeqDataBlock.SeqData[i]);
        ARP.UpdatePercentProgress((i+1)*100 div ANoOfOtus);
        Application.ProcessMessages;
        Data   := CurSeq;
      end;
    end;

    { initialize the TaxaGps Dlg}
    {$IFDEF VISUAL_BUILD}
    if OpenInSDE then
      TaxaGpsDlg := TTaxaGpsDlg.Create(Application);
    {$ENDIF}

    { Domains}
    ANoOfDomains := 0; // No way to specify domains in the formal Fasta format

    { initialize the GeneDomainDlg}
    {$IFDEF VISUAL_BUILD}
    if OpenInSDE then
      GeneDomainDlg := TGeneDomainDlg.Create(Application);
    {$ENDIF}

    ConstructAllSiteDomainMark;
    {ADomainMark := Pointer(Parser.AllSiteDomainMark); }
    D_InputSeqData.DomainMarks  := Pointer(ADomainMark);

    {ADomainInfoList  := Pointer(Parser.AllDomainInfo); }
    D_InputSeqData.AllDomainInfo := Pointer(ADomainInfoList);

    ADomainMark[0].IsCoding   := SeqDataBlock.ContainsCodingNuc;
    ADomainMark[0].CodonStart := 0;

    {$IFDEF VISUAL_BUILD}
    if (ADataType = snNucleotide) and (ANoOfDomains < 2) then
    begin
      ADomainMark[0].IsCoding := SeqDataBlock.ContainsCodingNuc;
      D_InputSeqData.IsCoding := SeqDataBlock.ContainsCodingNuc;
      ADomainMark[0].CodonStart := 0;
    end;
    {$ELSE}
      if SeqDataBlock.ContainsCodingNuc then
      begin
        D_InputSeqData.IsCoding := True;
        ADomainMark[0].IsCoding   := True;
        ADomainMark[0].CodonStart := 0;
      end;
    {$ENDIF}

    ARP.UpdateRunStatusInfo('Status', 'Organizing sequence information');
    Application.ProcessMessages;
     { setup Sequence viewer}
    VS_SeqDataExplorer := TVS_SeqDataExplorer.Create;
    {$IFDEF VISUAL_BUILD}
    if OpenInSDE then
    begin
      V_SeqDataExplorer := TV_SeqDataExplorer.Create(Application);
      V_SeqDataExplorer.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Sequence Data Explorer (' + ExtractFileName(NexusFile.Filename) + ')';
    end;

    if (not IsPrototyper) and (ANoOfOtus*ANoOfSites > SDE_MAX_TOTAL_SITES) then
    begin
      if not Assigned(MegaInfoForm) then
        MegaInfoForm := TMegaInfoForm.Create(MegaForm);
      MegaInfoForm.SetInfoText(MessageStringForDisabledSeqDataExplorer(GetDataTypeString, ANoOfOtus, ANoOfSites));
      MegaForm.ExplorersDisabledForLargeData := True;
    end
    else
      MegaForm.ExplorersDisabledForLargeData := False;
    {$ENDIF}

    D_InputSeqData.IsNuc := ((ADataType = snNucleotide) or (ADataType = snCoding));
    D_InputSeqData.IsAmino := (ADataType = snProtein);

    { code table selection}
    if D_InputSeqData.IsCoding then
    begin
      {$IFDEF VISUAL_BUILD}
      CodeTableDlg1 := TSelectGeneticCodeDlg.Create(Application);
      if (CodeTableDlg1.ShowModal <> mrOK) then
      begin
        ADomainMark[0].IsCoding   := False;
        ADomainMark[0].CodonStart := -1;
        D_InputSeqData.IsCoding    := False;
      end;
      {$ENDIF}
    end;


    {$IFDEF VISUAL_BUILD}
    if ADomainMark.IsCoding then
    begin
      D_InputSeqData.CodeName  := CodeTableDlg1.CodeTableName;
      D_InputSeqData.CodeTable := CodeTableDlg1.CodeTable;
    end;
    {$ENDIF}

    D_InputSeqData.GapSym    := SeqDataBlock.GapSymbol;
    D_InputSeqData.MissSym   := SeqDataBlock.MissingSymbol;
    D_InputSeqData.IdenSym   := SeqDataBlock.IdenticalSymbol;
    D_InputSeqData.NoOfTaxa  := ANoOfOtus;
    D_InputSeqData.NoOfSites := ANoOfSites;
    D_InputSeqData.OtuInfos  := Pointer(AOtuInfo);
    D_InputSeqData.DomainMarks := Pointer(ADomainMark);

    D_InputSeqData.Initialize;
    {$IFDEF VISUAL_BUILD}
    if OpenInSDE then
    begin
      V_SeqDataExplorer.Initialize;
      ARP.UpdatePercentProgress(90);
      Application.ProcessMessages;
    end;
    {$ENDIF}
    VS_SeqDataExplorer.Initialize;
    ADomainMark.IsDirty := False;
    AOtuInfo.IsDirty    := False;

    {$IFDEF VISUAL_BUILD}
    MegaForm.UpdateMainWindow(NexusFile.FileName, 'NEXUS file', SeqDataBlock.Title, SeqDataBlock.DataType);
    {$ENDIF}
  except
    On E: Exception do
    begin
      ShowErrorMessage(E);
      FreeAndNil(D_InputSeqData);
      {$IFDEF VISUAL_BUILD}
      if V_SeqDataExplorer <> nil then
	FreeAndNil(V_SeqDataExplorer);
      {$ENDIF}
      if VS_SeqDataExplorer <> nil then
        FreeAndNil(VS_SeqDataExplorer);
      {$IFDEF VISUAL_BUILD}
      if GeneDomainDlg <> nil then
        FreeAndNil(GeneDomainDlg);
      if TaxaGpsDlg <> nil then
        FreeAndNil(TaxaGpsDlg);
      MegaForm.Update;
      {$ENDIF}
    end;
  end;
end;

procedure SetupFastaSequenceInformation(DataFileInfo: TDataFileInfo; NoOfOtus, NoOfSites: Integer; Taxon, Sequence: Array of PAnsiChar; ARP: TRuntimeProgress; OpenInSDE: Boolean=True);
var
  updateTime: TDateTime;
  i:   Integer;
  CurSeq: PAnsiChar;
  {$IFDEF VISUAL_BUILD}
  CodeTableDlg1: TSelectGeneticCodeDlg;
  {$ENDIF}
  ADataType    : TSnTokenCode;
  ANoOfOtus    : LongInt;
  ANoOfSites   : LongInt;
  ANoOfDomains : LongInt;
  AOtuInfo         : TAllOtuInfo;
  ADomainInfoList  : TAllDomainInfo;
  ADomainMark      : TAllSiteDomainMark;

  function GetDataTypeString: String;
  begin
    if ADomainMark[0].IsCoding then
      Result := MapTokenCodeToDataTypeString(snCoding)
    else
      Result := MapTokenCodeToDataTypeString(ADataType);
  end;

  procedure ConstructAllSiteDomainMark;
  var
    j: Integer;
    ADomainInfo: TDomainInfo;
  begin
    if ADomainMark <> nil then Exit;

    ADomainMark := TAllSiteDomainMark.Create;
    ADomainMark.NoOfSites := NoofSites;

    ADomainInfo := nil;

    ADomainInfo := TDomainInfo.Create;
    ADomainInfo.Name := 'Data';
    ADomainInfo.FromSite := 0;
    ADomainInfo.ToSite   := NoOfSites-1;
    ADomainInfoList.Add(ADomainInfo);
    ADomainInfo := nil;

    with ADomainInfoList[0] do // No domains besides Data
      if IsDomain and (FromSite >= 0) then
        for j:= FromSite to ToSite do
           ADomainMark[j] := ADomainInfoList[0];
  end;
begin
  updateTime := Now;
{$IFDEF VISUAL_BUILD}
  TaxaGpsDlg      := nil;
  GeneDomainDlg   := nil;
  CodeTableDlg1   := nil;
{$ENDIF}

  D_InputSeqData  := nil;
  ADomainMark     := nil;
  ADomainInfoList := TAllDomainInfo.Create;
  AOtuInfo        := nil;

  try
    D_InputSeqData := TD_InputSeqData.Create;
    D_InputSeqData.SourceFileName := DataFileInfo.GetFileName;
    ANoOfOtus    := NoOfOtus;
    ANoOfSites   := NoOfSites;
    ADataType    := DataFileInfo.GetDataType;
    ARP.UpdateRunStatusInfo('Status', 'Setting up Sequence/group names');
    AOtuInfo := TAllOtuInfo.Create;
    AOtuInfo.NoOfOtus := ANoOfOtus;

    ARP.UpdatePercentProgress(0);
    for i:=0 to ANoOfOtus-1 do
    begin
      AOtuInfo[i] := TOtuInfo.Create;
      with AOtuInfo[i] do
      begin
        Id     := i;
        Name   := Taxon[i];
        GpName := EmptyStr; // Group names can't be specified in the formal Fasta format.
        IsUsed := True;
        CurSeq := Sequence[i];  // memory is transferred
        if MilliSecondsBetween(Now, updateTime) > 500 then
        begin
          updateTime := Now;
          ARP.UpdatePercentProgress(Round(i/ANoOfOtus*100));
          Application.ProcessMessages;
        end;
        Data   := CurSeq;
      end;
    end;

    {$IFDEF VISUAL_BUILD}
    if OpenInSDE then
      TaxaGpsDlg := TTaxaGpsDlg.Create(Application);
    {$ENDIF}
    ARP.UpdateRunStatusInfo('Status', 'Setting up genes/domains information');
    Application.ProcessMessages;
    ANoOfDomains := 0; // No way to specify domains in the formal Fasta format
  {$IFDEF VISUAL_BUILD}
    if OpenInSDE then
      GeneDomainDlg := TGeneDomainDlg.Create(Application);
  {$ENDIF}

    ConstructAllSiteDomainMark;
    {ADomainMark := Pointer(Parser.AllSiteDomainMark); }
    D_InputSeqData.DomainMarks  := Pointer(ADomainMark);

    {ADomainInfoList  := Pointer(Parser.AllDomainInfo); }
    D_InputSeqData.AllDomainInfo := Pointer(ADomainInfoList);

    // With no way of specifying coding or not we have to ask the user now.
    {$IFDEF VISUAL_BUILD}
    if (ADataType = snNucleotide) and (ANoOfDomains < 2) then
    begin
      if ARP.Visible then
        ARP.Hide;
      // only one domain exists; created artificially
      if MessageDlg('Protein-coding nucleotide sequence data?', mtConfirmation, [mbYes,mbNo],0) = mrYes then
      begin
        ADomainMark[0].IsCoding   := True;
        ADomainMark[0].CodonStart := 0;
      end;
    end;
    {$ELSE}
      if DataFileInfo.GetContainsCodingNuc then
      begin
        D_InputSeqData.IsCoding := True;
        ADomainMark[0].IsCoding   := True;
        ADomainMark[0].CodonStart := 0;  {todo -oSudhir : eventually there should be a flag to specify the codon start (if not 0)}
      end;
    {$ENDIF}

    ARP.UpdateRunStatusInfo('Status', 'Organizing sequence information');
    Application.ProcessMessages;
    VS_SeqDataExplorer := TVS_SeqDataExplorer.Create;
    {$IFDEF VISUAL_BUILD}
    if (not IsPrototyper) and (NoOfOtus*NoOfSites > SDE_MAX_TOTAL_SITES) then
    begin
      if not Assigned(MegaInfoForm) then
        MegaInfoForm := TMegaInfoForm.Create(MegaForm);
      MegaInfoForm.SetInfoText(MessageStringForDisabledSeqDataExplorer(GetDataTypeString, ANoOfOtus, ANoOfSites));
      MegaForm.ExplorersDisabledForLargeData := True;
    end
    else
      MegaForm.ExplorersDisabledForLargeData := False;

    if OpenInSDE then
    begin
      V_SeqDataExplorer := TV_SeqDataExplorer.Create(Application);
      V_SeqDataExplorer.Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Sequence Data Explorer (' + ExtractFileName(DataFileInfo.GetFileName) + ')';
    end;

    {$ENDIF}
    D_InputSeqData.IsNuc := ((ADataType = snNucleotide) or (ADataType = snCoding));
    D_InputSeqData.IsAmino := (ADataType = snProtein);

    if ((ADataType = snNucleotide) or (ADataType = snCoding)) and ADomainMark.IsCoding then
    begin
      D_InputSeqData.IsCoding  := True;
      {$IFDEF VISUAL_BUILD}
      CodeTableDlg1 := TSelectGeneticCodeDlg.Create(Application);
      if (CodeTableDlg1.ShowModal <> mrOK) then
      begin
        ADomainMark[0].IsCoding   := False;
        ADomainMark[0].CodonStart := -1;
        D_InputSeqData.IsCoding    := False;
      end;
      {$ENDIF}
    end
    else
      D_InputSeqData.IsCoding  := False;

    {$IFDEF VISUAL_BUILD}
    if ADomainMark.IsCoding then
    begin
      D_InputSeqData.CodeName  := CodeTableDlg1.CodeTableName;
      D_InputSeqData.CodeTable := CodeTableDlg1.CodeTable;
    end;
    {$ENDIF}

    D_InputSeqData.GapSym    := DataFileInfo.GetGapSymbol;
    D_InputSeqData.MissSym   := DataFileInfo.GetMissingBaseSymbol;
    D_InputSeqData.IdenSym   := DataFileInfo.GetIdenticalBaseChar;
    D_InputSeqData.NoOfTaxa  := ANoOfOtus;
    D_InputSeqData.NoOfSites := ANoOfSites;
    D_InputSeqData.OtuInfos  := Pointer(AOtuInfo);
    D_InputSeqData.DomainMarks := Pointer(ADomainMark);

    D_InputSeqData.Initialize;
    {$IFDEF VISUAL_BUILD}
    if OpenInSDE and (NoOfOtus*NoOfSites < SDE_MAX_TOTAL_SITES) then
    begin
      V_SeqDataExplorer.Initialize;
      ARP.UpdatePercentProgress(90);
      Application.ProcessMessages;
    end;
    {$ENDIF}
    if OpenInSDE then
      VS_SeqDataExplorer.Initialize;

    ADomainMark.IsDirty := False;
    AOtuInfo.IsDirty    := False;
    {$IFDEF VISUAL_BUILD}
    MegaForm.UpdateMainWindow(DataFileInfo.GetFileName, 'fasta file', EmptyStr, DataFileInfo.GetDataType);
    {$ENDIF}
  except
    On E: Exception do
    begin
      ShowErrorMessage(E);
      FreeAndNil(D_InputSeqData);
      {$IFDEF VISUAL_BUILD}
      if V_SeqDataExplorer <> nil then
	      FreeAndNil(V_SeqDataExplorer);
	    {$ENDIF}
      if VS_SeqDataExplorer <> nil then
        FreeAndNil(VS_SeqDataExplorer);
	    {$IFDEF VISUAL_BUILD}
      if GeneDomainDlg <> nil then
        FreeAndNil(GeneDomainDlg);
      if TaxaGpsDlg <> nil then
        FreeAndNil(TaxaGpsDlg);
      MegaForm.Update;
      {$ENDIF}
    end;
  end;
end;

procedure SetupMASSequenceInformation(FileName: String; NoOfOtus, NoOfSites: Integer; DataType: TSnTokenCode; SeqList: TSequenceList; CodeTableName: AnsiString; CodeTable: AnsiString; GapSym, MissingSym, IdenticalSym: AnsiChar; ARP: TRuntimeProgress);
var
  i:   Integer;
  CurSeq: PAnsiChar;
  ADataType    : TSnTokenCode;
  ANoOfOtus    : LongInt;
  ANoOfSites   : LongInt;
  ANoOfDomains : LongInt;
  AOtuInfo         : TAllOtuInfo;
  ADomainInfoList  : TAllDomainInfo;
  ADomainMark      : TAllSiteDomainMark;

  procedure ConstructAllSiteDomainMark;
  var
    j: Integer;
    ADomainInfo: TDomainInfo;
  begin
    if ADomainMark <> nil then
      Exit;
    ADomainMark := TAllSiteDomainMark.Create;
    ADomainMark.NoOfSites := NoofSites;
    ADomainInfo := nil;
    ADomainInfo := TDomainInfo.Create;
    ADomainInfo.Name := 'Data';
    ADomainInfo.FromSite := 0;
    ADomainInfo.ToSite   := NoOfSites-1;
    ADomainInfoList.Add(ADomainInfo);
    ADomainInfo := nil;

    with ADomainInfoList[0] do // No domains besides Data
      if IsDomain and (FromSite >= 0) then
        for j:= FromSite to ToSite do
           ADomainMark[j] := ADomainInfoList[0];
  end;
begin
  {$IFDEF VISUAL_BUILD}
  TaxaGpsDlg      := nil;
  GeneDomainDlg   := nil;
  {$ENDIF}
  D_InputSeqData  := nil;
  ADomainMark     := nil;
  AOtuInfo        := nil;
  ADomainInfoList := TAllDomainInfo.Create;

  try
    D_InputSeqData := TD_InputSeqData.Create;
    ANoOfOtus    := NoOfOtus;
    ANoOfSites   := NoOfSites;
    ADataType    := DataType;
    ARP.UpdateRunStatusInfo('Status', 'Setting up Sequence/group names');
    AOtuInfo := TAllOtuInfo.Create;
    AOtuInfo.NoOfOtus := ANoOfOtus;

    ARP.UpdatePercentProgress(0);
    for i:=0 to ANoOfOtus-1 do
    begin
      AOtuInfo[i] := TOtuInfo.Create;
      with AOtuInfo[i] do
      begin
        Id     := i;
        Name   := SeqList.Items[i].SeqName;
        GpName := EmptyStr; // Group names can't be specified in the formal Fasta format.
        IsUsed := True;
        CurSeq := PAnsiChar(SeqList.Items[i].SeqData);
        ARP.UpdatePercentProgress((i+1)*100 div ANoOfOtus);
        Application.ProcessMessages;
        Data   := CurSeq;
      end;
    end;

    {$IFDEF VISUAL_BUILD}
    TaxaGpsDlg := TTaxaGpsDlg.Create(Application);
	  {$ENDIF}

    ARP.UpdateRunStatusInfo('Status', 'Setting up genes/domains information');
    Application.ProcessMessages;
    ANoOfDomains := 0; // No way to specify domains in the formal Fasta format

    {$IFDEF VISUAL_BUILD}
    GeneDomainDlg := TGeneDomainDlg.Create(Application);
    {$ENDIF}

    ConstructAllSiteDomainMark;
    {ADomainMark := Pointer(Parser.AllSiteDomainMark); }
    D_InputSeqData.DomainMarks  := Pointer(ADomainMark);

    {ADomainInfoList  := Pointer(Parser.AllDomainInfo); }
    D_InputSeqData.AllDomainInfo := Pointer(ADomainInfoList);

    // With no way of specifying coding or not we have to ask the user now.
    if (ADataType = snNucleotide) and (ANoOfDomains < 2) then
    begin
      // only one domain exists; created artificially
      if SeqList.IsProteinCoding then
      begin
        ADomainMark[0].IsCoding   := True;
        ADomainMark[0].CodonStart := 0;
      end;
    end;

    ARP.UpdateRunStatusInfo('Status', 'Organizing sequence information');
    Application.ProcessMessages;

    VS_SeqDataExplorer := TVS_SeqDataExplorer.Create;
    {$IFDEF VISUAL_BUILD}
    V_SeqDataExplorer := TV_SeqDataExplorer.Create(Application);
    V_SeqDataExplorer.Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Sequence Data Explorer (' + ExtractFileName(FileName) + ')';
    {$ENDIF}
    D_InputSeqData.IsNuc := (ADataType = snNucleotide);
    D_InputSeqData.IsAmino := (ADataType = snProtein);

    if (ADataType = snNucleotide) and ADomainMark.IsCoding then
    begin
      D_InputSeqData.IsCoding  := True;
    end
    else
      D_InputSeqData.IsCoding  := False;

    if ADomainMark.IsCoding then
    begin
      D_InputSeqData.CodeName  := CodeTableName;
      D_InputSeqData.CodeTable := CodeTable;
    end;

    D_InputSeqData.GapSym    := GapSym;
    D_InputSeqData.MissSym   := MissingSym;
    D_InputSeqData.IdenSym   := IdenticalSym;
    D_InputSeqData.NoOfTaxa  := ANoOfOtus;
    D_InputSeqData.NoOfSites := ANoOfSites;
    D_InputSeqData.OtuInfos  := Pointer(AOtuInfo);
    D_InputSeqData.DomainMarks := Pointer(ADomainMark);
    D_InputSeqData.Initialize;
    {$IFDEF VISUAL_BUILD}
    V_SeqDataExplorer.Initialize;
    ARP.UpdatePercentProgress(90);
    Application.ProcessMessages;
    {$ENDIF}
    VS_SeqDataExplorer.Initialize;

    ADomainMark.IsDirty := False;
    AOtuInfo.IsDirty    := False;

    {$IFDEF VISUAL_BUILD}
    MegaForm.UpdateMainWindow(FileName, 'fasta file', EmptyStr, DataType);
    {$ENDIF}
  except
    On E: Exception do
    begin
      ShowErrorMessage(E);
      FreeAndNil(D_InputSeqData);
      {$IFDEF VISUAL_BUILD}
      FreeAndNil(V_SeqDataExplorer);
      {$ENDIF}
      FreeAndNil(VS_SeqDataExplorer);
      {$IFDEF VISUAL_BUILD}
      FreeAndNil(GeneDomainDlg);
      FreeAndNil(TaxaGpsDlg);
      MegaForm.Update;
      {$ENDIF}
    end;
  end;
end;

function ReadDistFile(FileName: String; HasPara: Boolean; ADataType, ADataFormat: TSnTokenCode): Boolean;
var
  MyParser: TLexDist = nil;
  ARow, ACol: Integer;
  ARP: TRuntimeProgress = nil;
begin
  Result := False;
  try try
    MyParser := TLexDist.Create;
    ARP := TRuntimeProgress.Create(Application);
    ARP.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Reading Input Data File';
    ARP.AddRunStatusInfo('Data file', Filename);
    ARP.AddRunStatusInfo('Status', 'Reading header');
    if not isPrototyper then
      ARP.Show;
    MyParser.SetDataFile(Filename);
    Result := DoReadDistFile(MyParser, ARP, HasPara, ADataType, aDataFormat);
  except
    On E: Exception do
    begin
      ARow := MyParser.Row;
      ACol := MyParser.Col;
      MyParser.Free;
      if ARP <> nil then ARP.Visible := False;
      MyParser := nil;
      {$IFDEF VISUAL_BUILD}
      OpenFileAndFocus(Filename, ARow, ACol);
      {$ENDIF}
      raise Exception.Create(E.Message);
    end;
  end
  finally
    if MyParser <> nil then MyParser.Free;
    if ARP <> nil      then  ARP.Free;
  end;
end;

procedure SetupDistanceInformation(Parser: TLexDist; ARP: TRuntimeProgress);
var
  i,j:   Integer;
  CurDistRow: PArrayOfDouble = nil;
  ANoOfOtus: LongInt;
  AOtuInfo: TAllOtuInfo = nil;
begin
  {$IFDEF VISUAL_BUILD}
  TaxaGpsDlg     := nil;
  V_DistDataExplorer := nil;
  {$ENDIF}

  try
    ANoOfOtus    := Parser.NoOfOtus;
    ARP.UpdateRunStatusInfo('Status', 'Setting up taxa/group names');
    AOtuInfo := TAllOtuInfo.Create;
    AOtuInfo.NoOfOtus := ANoOfOtus;
    for i:=0 to ANoOfOtus-1 do
    begin
      AOtuInfo[i] := TOtuInfo.Create;
      CurDistRow := NewDistArray(ANoOfOtus);
      with AOtuInfo[i] do
      begin
        Id     := i;
        Name   := Parser.Taxon[i];
        GpName := Parser.GpName[i];
        AssignGeographicalInfo(Parser.GeographicalInfo[i]);
        IsUsed := True;
        for j:=0 to ANOOfOtus-1 do
          CurDistRow[j] := Parser.Dist[i,j];
        Data   := CurDistRow;
        CurDistRow := nil;
      end;
    end;

    {$IFDEF VISUAL_BUILD}
    TaxaGpsDlg := TTaxaGpsDlg.Create(Application);
    {$ENDIF}

    ARP.UpdateRunStatusInfo('Current', 'Organizing distance information');
    VS_DistDataExplorer := TVS_DistDataExplorer.create;
    D_InputDistData := TD_InputDistData.create;

    {$IFDEF VISUAL_BUILD}
    V_DistDataExplorer := TV_DistDataExplorer.Create(Application);
    V_DistDataExplorer.Caption := VER_MEGA_WIN_CAPTION_PREFIX+': Distance Data Explorer (' + ExtractFileName(Parser.FileName) + ')';
    {$ENDIF}

    D_InputDistData.NoOfTaxa  := ANoOfOtus;
    D_InputDistData.OtuInfos  := Pointer(AOtuInfo);

    VS_DistDataExplorer.Initialize;
    D_InputDistData.Initialize;
    {$IFDEF VISUAL_BUILD}
    TaxaGpsDlg.Initialize;
    V_DistDataExplorer.Initialize;
    {$ENDIF}

    AOtuInfo.IsDirty    := False;
    {$IFDEF VISUAL_BUILD}
    MegaForm.UpdateMainWindow(Parser.FileName, Parser.Title, Parser.Description, Parser.FDataType);
    {$ENDIF}
  except
    On E: Exception do
    begin
      if Assigned(ARP) then
        ARP.Hide;
      {$IFDEF VISUAL_BUILD}
      FreeAndNil(V_DistDataExplorer);
      FreeAndNil(TaxaGpsDlg);
      MegaForm.UpdateMainWindow;
      {$ENDIF}
      raise E;
    end;
  end;
end;


end.

