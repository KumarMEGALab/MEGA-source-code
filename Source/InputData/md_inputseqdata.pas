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

unit MD_InputSeqData;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Dialogs, Types, MegaConsts, MCodons, MDomainInfo,
  MOtuInfo, MLongintList, MVS_SeqDataExplorer, MLocusInfo, ExcelWrite, StrUtils,
  MGlobalSettings, MPleaseWait, MRuntimeProgressDlg, MD_Sequences, MPartitionData,
  nexusfile, mseqexportoptions, mgeographical_info, mallele_frequency, mentropy,
  contnrs, mstringbuilder;

  type
  TToDoSvType = (
                 svUpdateCodonSiteType,   // set to mark 0th, 1st, 2nd and 3rd base
                 svUpdateNucVar,          // var/info/singleton
                 svUpdateAminoVar,        // needed only for coding nuc data
                 svUpdateBasicCounts,     // conts of var/info/singleton etc are updated
                 svUpdateDegeneracy,      // 0,2,4 fold degeneracy
                 svUpdateCodonStarts,     // Recomputes the codon starts
                 svUpdateAASeq,           // Translates using codon starts
                 svUpdateSpecial        // Sites which meet the partial deletion percentage
                 );
  TToJobsPending  = set of TToDoSvType;
  CharIndexDoubleArray = array ['A'..'Z'] of Double;
  TIncludedSitesArray = array of Integer;

type

  { TGroupCount }

  TGroupCount = class(TObject)
    private
      FGroupName: String;
      FCount: Integer;
    public
      constructor Create(gpName: String);
      procedure Increment;
      property GroupName: String read FGroupName;
      property Count: Integer read FCount write FCount;
  end;

  { TD_InputSeqData }

  TD_InputSeqData = class(TObject)
  private
    FIsRunningInThread: Boolean;
    FEntropyList: TShannonEntropyList;
    FForcingAttributeUpdates: Boolean;
    FComputedStats: TSiteAttr;
    PleaseWait: TPleaseWait;
    FSourceFileName: String;
    threadProgressArray: Array of Integer;
    procedure writeStringToFile(data: TStream; toWrite: AnsiString);
    function NeedsComputed(aAttr: TSiteAttrType): Boolean;
    procedure writeIntToFile(data: TStream; toWrite:integer; Size: integer=4);
    procedure writeLongIntListToFile(data: TFileStream;
      intlist: TLongIntList);
    function GetSearchCanceled: Boolean;
    function GetIsSiteIncluded(AOptions: TDataSubsetOptions; LabelsToInclude: TStringList; SiteNum: LongInt; Pos2: LongInt = -1; Pos3: LongInt = -1): Boolean;
    function GetMaxAmbiguousAllowed(AOptions: TDataSubsetOptions; NumTaxa: Integer; SiteCoverage: Integer): Integer;
    function GetMaxSitesIncluded(AOptions: TDataSubsetOptions; LabelstoInclude: TStringlist): Integer;
    procedure InitSearchResults;

  public
    function GetGroupCounts: TFPHashList;
    function GetLargestGroups(numGroups: Integer): TFPHashList;
    function GetGroupsSortedByTime: TFPHashList;
    procedure Initialize;
    procedure ClearComputedStatsCache;
    function GenerateTSequenceList: TSequenceList;
    function  FindSequence(CurSearchStr: AnsiString; RowToStartAt: Integer; ARP: TRuntimeProgress): Integer;
    function  FindGroup(CurSearchStr: AnsiString; RowToStartAt: Integer; ARP: TRuntimeProgress): Integer;
    function  FindPrevGroup(CurSearchStr: AnsiString; RowToStartAt: Integer; ARP: TRuntimeProgress): Integer;
    function  FindPrevSequence(CurSearchStr: AnsiString; RowToStartAt: Integer; ARP: TRuntimeProgress): Integer;
    function  SearchForMotifs(Motifs: TStringList; RowToStart, ColToStart: Integer; StopAfterFirst: Boolean; ARP: TRuntimeProgress; SearchingForNext: Boolean): Boolean;
    procedure  MotifProgressUpdate(ProgressIndex: Integer; NewProgress: Integer);
    procedure  CheckRemainingSearchThreads(searchingForNext: Boolean); // if all threads are done then invalidate the datagird, re-enable everything, close the progress dialog, and show results.
    function  SearchForKmer(AminoAcidStr: AnsiString; Ambiguities: Integer; AddToSearchResults: Boolean; ARP: TRuntimeProgress; EpitopeNum, OverallNumEpitopes: Integer; IsSearchingForBest: Boolean; var Abort: Boolean): Integer;
    function  CheckKMer(searchAA: AnsiString; Seq, Site, Ambiguities: Integer; AddToSearchResults: Boolean): Boolean;
    procedure ConstructKMer(AAStr: AnsiString; currWindow: Integer; windowLimit: Integer; resultsStringList: TStringList);
    function  WriteExportDataToFile(Options: TSeqExportOptions; SaveTo: String; ResultOutput: TStringList; xlFile: TExcelWrite=nil): Boolean;
    function SortSearchResultsBySeqName: TList;
    procedure WriteSearchResultsOnALine(OutList: TStringList; SaveLocation: String; Format: TExportType; APleaseWait : TPleaseWait);
    function  SRIndexInVisual(Index: Integer): Integer;
    procedure StatNucComposition(var ResultOutput: TStringList; var nucXls: TExcelWrite; const SaveTo : String);
    procedure StatPairFreq(var ResultOutput: TStringList; var FreqXls: TExcelWrite; const SaveTo: String; const IsDirectional: Boolean);
    procedure StatCodonUsage(var ResultOutput: TStringList; var CodonXls: TExcelWrite; const SaveTo: String);
    procedure StatAminoAcidComposition(var ResultOutput: TStringList; var AmiXls: TExcelWrite; const SaveTo: String);
    function SeqDataIsValidForLikelihoodAnalysis(aSeqData: TStringList; var aMsg: String): Boolean;
    function TaxaWithNoDataMsg(taxa: TArrayOfInteger; otus: TList): String;
    function DoProgressCheckCancel(progress: Integer; status: AnsiString): Boolean;
  public
    ProgressCheckCancelFunc: TCheckCancelFunc;
    SearchIsCompleted: Boolean;
    ASearchRP: TRuntimeProgress;
    IncludedSites: TIncludedSitesArray;
    procedure HideProgressDlg;
    function GetDomainMark(Index: Integer): TDomainInfo;
    function GetDomainCount: Integer;
    function GetNoOfSiteLabelTypes: Integer;
    function HasSiteLabels: Boolean;
    function GetSiteLabelType(Index: LongInt): AnsiChar;

    function GetDomainName(Index: Integer): AnsiString;
    function GetGeneName(Index: Integer): AnsiString;
    function GetGpName(Index: Integer): AnsiString;
    function GetSpName(Index: Integer): AnsiString;
    function GetPopName(Index: Integer): AnsiString;
    function GetSequence(Index: Integer): PAnsiChar;
    function GetSeqUsed(Index: Integer): Boolean;
    function GetSiteLabel(Index: Integer): AnsiChar;
    function GetSiteUsed(Index: Integer): Boolean;
    function GetTaxaName(Index: Integer): AnsiString;
    function GetGeographicalInfo(index: Integer): TGeographicalInfo;
    function GetAA(Codon: array of AnsiChar): AnsiChar;
    function HasAttribute(Site: Integer; Attr: TSiteAttrType): Boolean;
   // function IsGoodAA(Base: Char): Boolean;
    function IsGoodNuc(Base: AnsiChar): Boolean;
    function GetSearchResult(Index: Integer): TSearchResult;
    procedure ClearSearchResults;
    function GetNumOfSearchRes: Integer;
    //procedure SaveSearchResults(FileName: String; Format: TExportType);
    function CheckFilterEmptySequences(aOptions: TDataSubsetOptions; aSeqList: TList; usedInfos: TList; var numTaxa, numSites: Integer; otuNames: TStringList): Boolean; overload;
    function CheckFilterEmptySequences(MAI: TObject): Boolean; overload;
    procedure PreprocessDataFromReltimeAnalysis(MyUsedOtuInfos: TList);
    procedure PrepareDataForDistAnalysis(AOptions: TDataSubsetOptions;
      const ASeqList, MyUsedOtuInfos: TList;
      var TotalTaxa, TotalSites: Integer;
      LabelsToInclude: TStringList;  // EmptyStr means all, 'space string' = non-label sites
      ARecodeScheme: AnsiString;
      SiteCoverage: Integer);
    procedure PrepareDataForParsimAnalysis(AOptions: TDataSubsetOptions;
      const ASeqList, MyUsedOtuInfos: TList; var TotalTaxa, TotalSites,
      InfoSites, ConstContribute: Integer;
      LabelsToInclude: TStringList;  // EmptyStr means all, 'space string' = non-label sites
      ARecodeScheme: AnsiString;
      SiteCoverage: Integer);
    procedure PrepareDataForMLAnalysis(MAI: TObject);
    procedure PrepareDataForPartitionedMLAnalysis(AOptions: TDataSubsetOptions;
      var PartitionList: TListOfSeqPartitions; MyUsedOtuInfos: TList; var TotalTaxa,
      TotalSites: Integer;
      LabelsToInclude: TStringList;  // EmptyStr means all, 'space string' = non-label sites
      ARecodeScheme: AnsiString;
      SiteCoverage: Integer);
    function PrepareDataForCodonOmegaAnalysis(
      AOptions: TDataSubsetOptions;
      const ASeqList: TList;
      const MyUsedOtuInfos: TList;
      var TotalTaxa: Integer;
      var TotalSites: Integer;
      LabelsToInclude: TStringList;
      SiteCoverage: Integer): TIntArray; // returns an array of Integer which indicates the start positions for all codons which were included in the analysis

    function GetCodonStringAtNucSite(aSite: Int64; aSeq: Int64): AnsiString;
    procedure CalculateMaxTaxaNameLen;
    procedure SortBy(MethodToSortBy: AnsiString);
    procedure UpdateAACodonStarts;
    procedure UpdateAminoVar;
    procedure UpdateCodonSitesType;
    procedure UpdateDegeneracy;
    procedure UpdateNucVar;
    procedure UpdateLabelledSites;
    procedure UpdateSpecial;
    procedure UpdateSiteAttrArray(forceUpdates: Boolean = False);
    function  GetCodeTable: AnsiString;
    procedure SetCodeTable(Value: AnsiString);
    procedure UpdateDispTaxaList;

    procedure UpdateSearchResults(FromIndex, ToIndex: Integer);
    procedure SaveSession(filename : String);

  public

    ARP: TRuntimeProgress;
    FIsNuc:    Boolean;  // Input data type
    FIsAmino:  Boolean;
    FGapSym:       AnsiChar;    // Alignment gap symbol
    FMissSym:      AnsiChar;    // Missing data symbol
    FIdenSym:      AnsiChar;    // Identical site symbol

    FIsCoding:     Boolean;  // True if Contains coding regins
    FCodeTable:    array[0..63] of AnsiChar;   // Code table
    FCodeName:     AnsiString;  // Code table Name;

    PDelValue: Integer;

      // Taxa Information
    FNoOfTaxa:     LongInt;      // No. of seqs
    FOtuInfos:     TAllOtuInfo;  // "Changes are global"

    // Group Information
    FNoOfGps:      LongInt;      // number of groups
    FGroupNames:   TStringList;  // list of names

      // Site Information
    FNoOfSites:    LongInt;            // No. of total sites
    NoOfNucSites:  LongInt;            // Used to save #sites if coding
    FDomainMarks:  TAllSiteDomainMark; // Domain information about all sites

      // Useful information is kept here
    AACodonStarts: TLongIntList; // list of full codon start indices
    AACodonPos2:   TLongIntList; // list of 2nd pos for full codons
    AACodonPos3:   TLongIntList; // list of 3rd pos for full codons
    ReferenceSeq:  PAnsiChar;        // points to the reference sequence
    CodonInfo:     TCodonInfo;   // allocated only for coding information
    SiteAttrArray: PSiteAttrArray; // site attributes array pointer
    FNoOfAttrSites: array [megNone..megLast] of LongInt;

    NoOfSitesUsed: LongInt;
    NoOfCodingSitesUsed:LongInt;

    MaxTaxaNameLen:    LongInt;      // Characters needed to show taxa names
    MaxGpNameLen:      LongInt;      // Characters needed to show gps
    MaxSpNameLen:      LongInt;      // Characters neede to show species
    MaxPopNameLen:     LongInt;      // Character needed to show populations

     // System state
    JobsPending:   TToJobsPending; // Jobs to do for complete updating
    DispTaxaList:  TLongintList;   // this is for display

    CurOtuInfo:    TOtuInfo; //Doesn't seem to be used
    CurDomainInfo: TDomainInfo;

    NextMotifAt: TSearchResult;
  private
    FStringBuilder: TStringBuilder;
    FAlleleComparisonSites: TIntArray;
    FAlleleFrequencies: TAlleleFrequencyArray;
    FAllDomainInfo: TAllDomainInfo;
    FNexusFile: TNexusFile;
    FSearchResults: TList;
    LastEpitopeAmbiguity: Integer; // number of ambiguous characters allowed in the last epitope searched for.
    procedure ClearAlleleFrequencies;
    procedure ClearEntropyList;
    function GetTotalNumBases: LongInt;
    procedure SetAllDomainInfo(DomainInfo: TAllDomainInfo);
    procedure NotifySearchResults(Sender: TObject; const Item: TSearchResult; Action: TCollectionNotification);
    procedure SetNexusFile(AValue: TNexusFile);
    function GetUseUnlabeledSites: Boolean;
    procedure SetUseUnlabeledSites(aValue: Boolean);
  Public
    constructor Create;
    destructor  Destroy; override;
    function GetTaxaNamesList: TStringList;
    function HasGroups: Boolean;
    function HasPopulations: Boolean;
    function HasSpecies: Boolean;
    function CountNoOfGps: Integer;
    function CountNoOfPopulations: Integer;
    procedure AddSearchResult(Res: TSearchResult); // moved from private to public for thread accessablity.
    procedure ClearSearchResult;
    procedure ReportNextLoc(Res: TSearchResult);
    procedure LabelSitesUsedForMinorAlleleIdentityComparisons(aLabel: AnsiChar);
    {$IFDEF VISUAL_BUILD}
    procedure AddDomainsToSitesUsedFromMinorAlleleIdentityComparisons;
    {$ENDIF}
    procedure AutoLabelSites(a: TSiteAttrType);
    function  CountAttrSites(Attr: TSiteAttrType): LongInt;
    function  DoTranslation: Boolean;  // False if 0 AA sites
    procedure DoUnTranslation;
    procedure WriteCodonMapsToNexus(NexusOut: TStringList; MyMappedData: TList; NChar: Integer; DataTypeStr: String);
    function BestEpitope(Epitope: AnsiString; ARP: TRuntimeProgress; EpitopeNum, OverallNumEpitopes: Integer; var Abort: Boolean): AnsiString;
    function BestEpitopes(Epitopes: TStringList; ARP: TRuntimeProgress): AnsiString;
    function FindEpitope(Epitope: AnsiString; ARP: TRuntimeProgress; EpitopeNum, OverallNumEpitopes: Integer; var Abort: Boolean; NumAmbig: Integer; var CurrAmbigStr: AnsiString): AnsiString;
    function FindEpitopes(Epitopes: TStringList; FixedNumAmbig: Integer;ARP: TRuntimeProgress): AnsiString;
    function SiteAttributesToString(siteIndex: Int64): String;
    function ComputeAlleleFrequencies(minCutoff, maxCutoff: Double; useReferenceSeq: Boolean; includeReferenceSeq: Boolean): Integer;
    function GetAlleleFrequenciesAsStringList(minCutoff, maxCutoff: Double; useReferenceSeq: Boolean; includeRefSeq: Boolean): TStringList;
    function NumAlleleComparisonSitesUsed: Integer;
    function AreIdentical(seq1: Integer; seq2: Integer): Boolean;
    function GroupIdenticalSeqs(minCutoff, maxCutoff: Double): Integer;
    procedure ComputeSequenceEntropy;
  Public
    property  UseUnlabeledSites: Boolean read GetUseUnlabeledSites write SetUseUnlabeledSites;
    property  SourceFileName: String read FSourceFilename write FSourceFileName;
    property  PartialDelValue: Integer read PDelValue write PDelValue;
    property  AllDomainInfo: TAllDomainInfo read FAllDomainInfo write SetAllDomainInfo;
    property  DomainMark[Index:LongInt]: TDomainInfo  read GetDomainMark;
    property  SiteLabel[Index:LongInt]: AnsiChar read GetSiteLabel;

    property  NoOfSiteLabeltypes: Integer read GetNoOfSiteLabelTypes;
    property  SiteLabelType[Index: LongInt]: AnsiChar read GetSiteLabelType;

    property IsNuc: Boolean    read FIsNuc write FIsNuc;
    property IsAmino: Boolean  read FIsAmino write FIsAmino;
    property IsCoding: Boolean     read FIsCoding  write FIsCoding;

    property NoOfTaxa:  LongInt read FNoOfTaxa write FNoOfTaxa;
    property OtuInfos:  TAllOtuInfo read FOtuInfos write FOtuInfos;   // "Changes are global"
    property NoOfGps:   LongInt     read FNoOfGps write FNoOfGps;

    property NoOfSites: LongInt  read FNoOfSites  write FNoOfSites;
    property TotalNumBases: LongInt read GetTotalNumBases;
    property CodeTable: AnsiString        read GetCodeTable write SetCodeTable;
    property CodeName: AnsiString         read FCodeName    write FCodeName;
    property GapSym:  AnsiChar            read FGapSym      write FGapSym;
    property MissSym: AnsiChar            read FMissSym     write FMissSym;
    property IdenSym: AnsiChar            read FIdenSym     write FIdenSym;

    property DomainMarks: TAllSiteDomainMark  read FDomainMarks write FDomainMarks;

    property Sequence[Index: LongInt]: PAnsiChar   read GetSequence;  // only for display
    property TaxaName[Index: LongInt]: AnsiString  read GetTaxaName; // only for display
    property GpName[Index: LongInt]: AnsiString    read GetGpName;    // only for display
    property SpName[Index: LongInt]: AnsiString    read GetSpName;    // only for display
    property PopName[Index: LongInt]: AnsiString    read GetPopName;    // only for display
    property SiteUsed[Index: LongInt]: Boolean read GetSiteUsed;  // only for display
    property SeqUsed[Index: LongInt]: Boolean  read GetSeqUsed;   // only for display
      // info about individual sites; amino or nuc, it doesn't matter
    property GeneName[Index: LongInt]: AnsiString read GetGeneName;  // gene name for a site
    property DomainName[Index: LongInt]: AnsiString read GetDomainName;
    property SearchResults[Index: LongInt]: TSearchResult read GetSearchResult;
    property NoOfSearchResults: Integer read GetNumOfSearchRes;
    property SearchTreadCanceled: Boolean read GetSearchCanceled;
    property LastEpitopeAmb: Integer read LastEpitopeAmbiguity write LastEpitopeAmbiguity;
    property NexusFile: TNexusFile read FNexusFile write SetNexusFile;
    property IsRunningInThread: Boolean read FIsRunningInThread write FIsRunningInThread;
  end;

  function CompareGroupCounts(Item1: Pointer; Item2: Pointer): Integer;

var
  D_InputSeqData: TD_InputSeqData;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  Mega_Main, Forms, MGeneDomainDlg,
  {$ELSE}
  MD_MegaMain, MegaUtils_NV,
  {$ENDIF}
  LCLIntf, LCLType, MegaUtils, Math, ErrorMessages_HC, MegaErrUtils,
  RegExpr, MSeqDataSearchThreads, Controls, MAnalysisInfo, MTreeList, typinfo,
  dateutils;

function CompareGroupCounts(Item1: Pointer; Item2: Pointer): Integer;
var
  gc1: TGroupCount = nil;
  gc2: TGroupCount = nil;
begin
  gc1 := TGroupCount(Item1);
  gc2 := TGroupCount(Item2);
  Result := CompareValue(gc2.Count, gc1.Count);
  if Result = 0 then
    Result := CompareText(gc2.GroupName, gc1.GroupName);
end;

{ TGroupCount }

constructor TGroupCount.Create(gpName: String);
begin
  FGroupName := gpName;
  FCount := 0;
end;

procedure TGroupCount.Increment;
begin
  inc(FCount);
end;

constructor TD_InputSeqData.Create;
begin
  inherited;
  FStringBuilder := TStringBuilder.Create;
  FIsRunningInThread := False;
  FComputedStats := [];
  ProgressCheckCancelFunc := nil;
  FNexusFile := nil;
  SetLength(IncludedSites, 0);
  FIsNuc    := False;
  FIsAmino  := False;
  FGapSym   := '-';
  FMissSym  := '?';
  FIdenSym  := '.';

  FIsCoding := False;
  FCodeTable[0] := #0;
  FCodeName := EmptyStr;

  FNoOfTaxa := -1;
  FOtuInfos := nil;
  FNoOfGps:= -1;

  FNoOfSites:= -1;
  FDomainMarks := nil;

  AACodonStarts := nil;
  AACodonPos2   := nil;
  AACodonPos3   := nil;
  ReferenceSeq:= nil;
  CodonInfo    := nil;
  SiteAttrArray:= nil;

  JobsPending := [];
  DispTaxaList := nil;

  CurOtuInfo    := nil;
  CurDomainInfo := nil;
  ARP := nil;
  PleaseWait := TPleaseWait.Create(nil);
  InitSearchResults;
end;

destructor TD_InputSeqData.Destroy;
var
  i : integer;
begin
  try
   {$IFDEF VISUAL_BUILD}
   MegaForm.ExplorersDisabledForLargeData := False;
   {$ENDIF}
    if Assigned(FStringBuilder) then
      FStringBuilder.Free;
    ClearAlleleFrequencies;
    ClearEntropyList;
    if Assigned(SiteAttrArray) then
      FreeMemAndNil(SiteAttrArray);
    SiteAttrArray := nil;
    if Assigned(FNexusFile) then
      FreeAndNil(FNexusFile);
    if CodonInfo <> nil     then CodonInfo.Free;
    if AACodonStarts <> nil then AACodonStarts.Free;
    if AACodonPos2   <> nil then AACodonPos2.Free;
    if AACodonPos3   <> nil then AACodonPos3.Free;

    if DispTaxaList <> nil  then
    begin
      DispTaxaList.Clear;
      DispTaxaList.Free;
    end;

    if FOtuInfos <> nil then
    begin
      // clean up sequence specific RsvData stuff
      for i:=0 to FNoOfTaxa-1 do
      begin
        if FOtuInfos[i].RsvData <> nil then
        begin
          FreeMem(FOtuInfos[i].RsvData);
          FOtuInfos[i].RsvData := nil;
        end;
        {if FOtuInfos[i].Data <> nil then // Shouldn't call FreeMem because we never explicitly allocated memory for Data, it was created by Delphi by automaticly converting a String to a PChar.
        begin
          FreeMem(FOtuInfos[i].Data);
          FOtuInfos[i].Data := nil;
        end; }
      end;
      if Assigned(FOtuInfos) then
        FreeAndNil(FOtuInfos);
    end;
    if Assigned(FDomainMarks) then
      FreeAndNil(FDomainMarks);
    if Assigned(FAllDomainInfo) then
      FreeAndNil(FAllDomainInfo);

    if FGroupNames <> nil then
    begin
      FGroupNames.Free;
      FGroupNames := nil;
    end;
    if Assigned(FSearchResults) then
    begin
      if FSearchResults.Count > 0 then
        for i := 0 to FSearchResults.Count - 1 do
          TSearchResult(FSearchResults[i]).Free;
      FSearchResults.Free;
    end;
    if Assigned(PleaseWait) then
      PleaseWait.Free;
  Except on E: Exception do
  {$IFDEF VISUAL_BUILD}
    MessageDlg('MEGA Encountered an error while closing the file.  Please submit a bug report with details about how this happened, the data file, and this information: ' + #10#13 + E.Message, mtWarning, [mbOK], 0);
  {$ELSE}
    Warn_NV('MEGA Encountered an error while closing the file.  Please submit a bug report with details about how this happened, the data file, and this information: ' + #10#13 + E.Message);
  {$ENDIF}
  end;
  SetLength(IncludedSites, 0);
  inherited destroy;
end;

function TD_InputSeqData.GetTaxaNamesList: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  if Assigned(FOtuInfos) and (FOtuInfos.NoOfOtus > 0) then
    for i := 0 to FOtuInfos.NoOfOtus - 1 do
      Result.Add(FOtuInfos[i].Name);
end;

procedure TD_InputSeqData.ClearComputedStatsCache;
begin
  FComputedStats := [];
end;

function TD_InputSeqData.GetGroupCounts: TFPHashList;
var
  i: Integer;
  otu: TOtuInfo = nil;
  gc: TGroupCount = nil;
  index: Integer;
begin
  Result := TFPHashList.Create;
  if FNoOfTaxa > 0 then
    for i := 0 to FNoOfTaxa - 1 do
    begin
      otu := FOtuInfos[i];
      if otu.GpName <> EmptyStr then
      begin
        index := Result.FindIndexOf(otu.GpName);
        if index < 0 then
        begin
          gc := TGroupCount.Create(otu.GpName);
          gc.Increment;
          Result.Add(otu.GpName, gc);
        end
        else
        begin
          gc := TGroupCount(Result[index]);
          gc.Increment;
        end;
      end;
    end;
end;

function TD_InputSeqData.GetLargestGroups(numGroups: Integer): TFPHashList;
var
  aList: TList = nil;
  temp: TFPHashList = nil;
  i: Integer;
  gc: TGroupCount = nil;
begin
  try
    Result := TFPHashList.Create;
    aList := TList.Create;
    temp := GetGroupCounts;
    if temp.Count > 0 then
    begin
     for i := 0 to temp.Count - 1 do
       aList.Add(temp[i]);
      aList.Sort(CompareGroupCounts);
      i := 0;
      while (i < numGroups) and (i < aList.Count) do
      begin
        gc := TGroupCount(aList[i]);
        Result.Add(gc.GroupName, gc);
        inc(i);
      end;
    end;
  finally
    if Assigned(aList) then
      aList.Free;
    if Assigned(temp) then
      temp.Free;
  end;
end;

function TD_InputSeqData.GetGroupsSortedByTime: TFPHashList;
var
  i: Integer;
  otu: TOtuInfo = nil;
  aList: TList = nil;
  index: Integer;
begin
  Result := TFPHashList.Create;
  if FNoOfTaxa > 0 then
  begin
    for i := 0 to FNoOfTaxa - 1 do
    begin
      otu := FOtuInfos[i];
      if otu.GpName <> EmptyStr then
      begin
        index := Result.FindIndexOf(otu.GpName);
        if index < 0 then
        begin
          aList := TList.Create;
          aList.Add(otu);
          Result.Add(otu.GpName, aList);
        end
        else
        begin
          aList := TList(Result.Items[index]);
          aList.Add(otu);
        end;
      end;
    end;
    if Result.Count > 0 then
      for i := 0 to Result.Count - 1 do
      begin
        aList := TList(Result.Items[i]);
        aList.Sort(CompareOtuByTimestamp)
      end;
  end;
end;

procedure TD_InputSeqData.Initialize;
var
  i : integer;
begin
    {$IFDEF VISUAL_BUILD}
    if FNoOfSites > SDE_MAX_SITES then
      MegaForm.ExplorersDisabledForLargeData := True
    else
      MegaForm.ExplorersDisabledForLargeData := False;
    {$ENDIF}
    FForcingAttributeUpdates := False;
    GetMem(SiteAttrArray, FNoOfSites*SizeOf(TSiteAttr));
    For i:=0 to FNoOfSites-1 do
       SiteAttrArray[i] := [];

    if FIsNuc then
    begin
      NoOfNucSites := FNoOfSites; // important if coding
      FIsAmino := False;
    end;

    // Setup the displayed taxa; default: keep all taxa
    DispTaxaList := TLongIntList.Create;

    UpdateDispTaxaList;

    // reference sequence is here
    ReferenceSeq := Sequence[0];

    // by DomainInfo generate meg0Site, meg1stSite, meg2ndSite, and meg3rdSite
    JobsPending := [svUpdateCodonSiteType];
    UpdateCodonSitesType;

    // Marking site variability etc
    JobsPending := JobsPending + [svUpdateBasicCounts];
    if FIsNuc then
    begin
      JobsPending := JobsPending + [svUpdateNucVar];
      JobsPending := JobsPending + [svUpdateSpecial]; // update special properties which only have to be calculated once.
    end
    else
      JobsPending := JobsPending + [svUpdateAminoVar];

    if FIsNuc and FIsCoding then
    begin
      if CodonInfo = nil then
        CodonInfo := TCodonInfo.Create; // fixed in mega2b2
      if FCodeTable = EmptyStr then  // if code table is empty default to Standard. (Sudhir, is this a good idea?)
      begin
        SetCodeTable(GetStandardGeneticCode);
        {$IFNDEF VISUAL_BUILD}  
        warn_nv('No Code table was specified, assuming the Genetic Code Table is ''Standard''');
        {$ENDIF}
      end;
      CodonInfo.CodeTable := FCodeTable;
      AACodonStarts := TLongintList.Create;
      AACodonPos2   := TLongintList.Create;
      AACodonPos3   := TLongintList.Create;

      JobsPending := JobsPending + [svUpdateCodonStarts, svUpdateAASeq,
                                    svUpdateAminoVar, svUpdateDegeneracy];
    end;
end;

function TD_InputSeqData.GenerateTSequenceList: TSequenceList;
var
  Sequence: TSequence;
  i: Integer;
begin
  Result := TSequenceList.Create;
  {$IFNDEF VISUAL_BUILD}
  Result.Title := D_MegaMain.DataTitle;
  {$ELSE}
  Result.Title := MegaForm.DataTitle;
  {$ENDIF}
  Result.IsDNA := FIsNuc;
  Result.IsProteinCoding := FIsCoding;

  for i := 0 to FNoOfTaxa - 1 do
  begin
    Sequence := TSequence.Create;
    Sequence.SeqName := FOtuInfos.Otu[i].Name;
    Sequence.GroupName := FOtuInfos.Otu[i].GpName;
    Sequence.SeqData := PAnsiChar(FOtuInfos.Otu[i].Data);
    Result.Add(Sequence);
    Sequence := nil;
  end;

end;

//------- Count the number of sites of a given kind
function TD_InputSeqData.CountAttrSites(Attr: TSiteAttrType): LongInt;
var
  s: LongInt;
begin
  Result := 0;
  for s:=0 to FNoOfSites-1 do
    if Attr in SiteAttrArray[s] then Inc(Result);
end;

function TD_InputSeqData.HasGroups: Boolean;
var
  i: Integer;
begin
  Result := False;
  if (FOtuInfos = nil) or (FOtuInfos.NoOfOtus = 0) then
    Exit;

  for i := 0 to FOtuInfos.NoOfOtus - 1 do
    if Length(FOtuInfos[i].GpName) > 0 then
    begin
      Result := True;
      Exit;
    end;
end;

function TD_InputSeqData.HasPopulations: Boolean;
var
  i: Integer;
begin
  Result := False;
  if (FOtuInfos = nil) or (FOtuInfos.NoOfOtus = 0) then
    Exit;

  for i := 0 to FOtuInfos.NoOfOtus - 1 do
    if Length(FOtuInfos[i].PopName) > 0 then
    begin
      Result := True;
      Exit;
    end;
end;

function TD_InputSeqData.HasSpecies: Boolean;
var
  i: Integer;
begin
  Result := False;
  if (FOtuInfos = nil) or (FOtuInfos.NoOfOtus = 0) then
    Exit;

  for i := 0 to FOtuInfos.NoOfOtus - 1 do
    if Length(FOtuInfos[i].SpName) > 0 then
    begin
      Result := True;
      Exit;
    end;
end;

function TD_InputSeqData.CountNoOfGps: Integer;
var
  i,j: Integer;
  IsStrFound: Boolean;
begin
  Result := 0;
  if FOtuInfos = nil then
    Exit;

  for i:=0 to FOtuInfos.NoOfOtus-1 do
  begin
    if Length(FOtuInfos[i].GpName) = 0 then
      Continue; // empty str is not worth it
    IsStrFound := False;
    for j:=i+1 to FOtuInfos.NoOfOtus-1 do
      if CompareStr(FOtuInfos[i].GpName, FOtuInfos[j].GpName) = 0 then // same string found then break
      begin
        IsStrFound := True;
        break;
      end;
    if not IsStrFound then
      Inc(Result);
  end;
end;

function TD_InputSeqData.CountNoOfPopulations: Integer;
var
  i,j: Integer;
  IsStrFound: Boolean;
begin
  Result := 0;
  if FOtuInfos = nil then
    Exit;

  for i:=0 to FOtuInfos.NoOfOtus-1 do
  begin
    if Length(FOtuInfos[i].SpName) = 0 then
      Continue; // empty str is not worth it
    IsStrFound := False;
    for j:=i+1 to FOtuInfos.NoOfOtus-1 do
      if CompareStr(FOtuInfos[i].SpName, FOtuInfos[j].SpName) = 0 then // same string found then break
      begin
        IsStrFound := True;
        break;
      end;
    if not IsStrFound then
      Inc(Result);
  end;
end;

function TD_InputSeqData.FindSequence(CurSearchStr : AnsiString; RowToStartAt : Integer; ARP: TRuntimeProgress): Integer; //this function given a string searches through the data and returns the row it is on
  var
  i: Integer;
begin
  ARP.UpdatePercentProgress(0);
  ARP.UpdateRunStatusInfo('Searching', 'For an exact match ...');
  for i:= RowToStartAt to DispTaxaList.Count do   //Look for an exact match  // DP Removed -1 from DispTaxaList.Count-1 because it was missing the last taxa
  begin
      if AnsiCompareText(CurSearchStr, D_InputSeqData.TaxaName[i-1]) = 0 then
      begin
         Result := i;
         Exit;
      end;
      ARP.UpdatePercentProgress(((i+1)*100) div DispTaxaList.Count);
  end;

  //If not found, Look for start of the name
  ARP.UpdatePercentProgress(0);
  ARP.UpdateRunStatusInfo('Searching', 'For "begins with" ...');
  for i:=RowToStartAt to DispTaxaList.Count do // DP Removed -1 from DispTaxaList.Count-1 because it was missing the last taxa
  begin
    if Pos(UpperCase(CurSearchStr), UpperCase(D_InputSeqData.TaxaName[i-1])) = 1 then //start with it not case sensitive now after refactor
    begin
      Result := i;
      Exit;
    end;
    ARP.UpdatePercentProgress(((i+1)*100) div DispTaxaList.Count);
  end;

  //If still not found, see if their search string is anywhere in any of the taxa
  ARP.UpdatePercentProgress(0);
  ARP.UpdateRunStatusInfo('Searching', 'For query anywhere in Taxa');
  for i:=RowToStartAt to DispTaxaList.Count do // DP Removed -1 from DispTaxaList.Count-1 because it was missing the last taxa
  begin
    if AnsiContainsText(D_InputSeqData.TaxaName[i-1], CurSearchStr) then
    begin
      Result := i;
      Exit;
    end;
    ARP.UpdatePercentProgress(((i+1)*100) div DispTaxaList.Count);
  end;
  Result := -1; //we haven't found it
end;

function TD_InputSeqData.FindGroup(CurSearchStr : AnsiString; RowToStartAt : Integer; ARP: TRuntimeProgress): Integer; //this function given a string searches through the data and returns the row it is on
  var
  i: Integer;
begin
  ARP.UpdatePercentProgress(0);
  ARP.UpdateRunStatusInfo('Searching', 'For an exact match ...');

  for i:= RowToStartAt to DispTaxaList.Count do   //Look for an exact match  // DP Removed -1 from DispTaxaList.Count-1 because it was missing the last taxa
  begin

      if AnsiCompareText(CurSearchStr, GpName[i-1]) = 0 then
      begin
         Result := i;
         Exit;
      end;
      ARP.UpdatePercentProgress(((i+1)*100) div DispTaxaList.Count);
  end;

  //If not found, Look for start of the name
  ARP.UpdatePercentProgress(0);
  ARP.UpdateRunStatusInfo('Searching', 'For "begins with" ...');
  for i:=RowToStartAt to DispTaxaList.Count do // DP Removed -1 from DispTaxaList.Count-1 because it was missing the last taxa
  begin
    if Pos(UpperCase(CurSearchStr), UpperCase(GpName[i-1])) = 1 then //start with it not case sensitive now after refactor
    begin
      Result := i;
      Exit;
    end;
    ARP.UpdatePercentProgress(((i+1)*100) div DispTaxaList.Count);
  end;

  //If still not found, see if their search string is anywhere in any of the group
  ARP.UpdatePercentProgress(0);
  ARP.UpdateRunStatusInfo('Searching', 'For query anywhere in Group');
  for i:=RowToStartAt to DispTaxaList.Count do // DP Removed -1 from DispTaxaList.Count-1 because it was missing the last taxa
  begin
    if AnsiContainsText(GpName[i-1], CurSearchStr) then
    begin
      Result := i;
      Exit;
    end;
    ARP.UpdatePercentProgress(((i+1)*100) div DispTaxaList.Count);
  end;
  Result := -1; //we haven't found it
end;

function TD_InputSeqData.FindPrevGroup(CurSearchStr : AnsiString; RowToStartAt : Integer; ARP: TRuntimeProgress): Integer; //this function given a string searches through the data and returns the row it is on
  var
  i: Integer;
begin
  ARP.UpdatePercentProgress(0);
  ARP.UpdateRunStatusInfo('Searching', 'For an exact match ...');
  for i:= RowToStartAt downto 1 do   //Look for an exact match  // DP Removed -1 from DispTaxaList.Count-1 because it was missing the last taxa
  begin
      if AnsiCompareText(CurSearchStr, GpName[i-1]) = 0 then
      begin
         Result := i;
         Exit;
      end;
      ARP.UpdatePercentProgress(((i+1)*100) div DispTaxaList.Count);
  end;

  //If not found, Look for start of the name
  ARP.UpdatePercentProgress(0);
  ARP.UpdateRunStatusInfo('Searching', 'For "begins with" ...');
  for i:=RowToStartAt downto 1 do // DP Removed -1 from DispTaxaList.Count-1 because it was missing the last taxa
  begin
    if Pos(UpperCase(CurSearchStr), UpperCase(GpName[i-1])) = 1 then //start with it not case sensitive now after refactor
    begin
      Result := i;
      Exit;
    end;
    ARP.UpdatePercentProgress(((i+1)*100) div DispTaxaList.Count);
  end;

  //If still not found, see if their search string is anywhere in any of the taxa
  ARP.UpdatePercentProgress(0);
  ARP.UpdateRunStatusInfo('Searching', 'For query anywhere in Taxa');
  for i:=RowToStartAt downto 1 do // DP Removed -1 from DispTaxaList.Count-1 because it was missing the last taxa
  begin
    if AnsiContainsText(GpName[i-1], CurSearchStr) then
    begin
      Result := i;
      Exit;
    end;
    ARP.UpdatePercentProgress(((i+1)*100) div DispTaxaList.Count);
  end;
  Result := -1; //we haven't found it
end;


function TD_InputSeqData.FindPrevSequence(CurSearchStr : AnsiString; RowToStartAt : Integer; ARP: TRuntimeProgress): Integer; //this function given a string searches through the data and returns the row it is on
  var
  i: Integer;
begin
  ARP.UpdatePercentProgress(0);
  ARP.UpdateRunStatusInfo('Searching', 'For an exact match ...');
  for i:= RowToStartAt downto 1 do   //Look for an exact match  // DP Removed -1 from DispTaxaList.Count-1 because it was missing the last taxa
  begin
      if AnsiCompareText(CurSearchStr, D_InputSeqData.TaxaName[i-1]) = 0 then
      begin
         Result := i;
         Exit;
      end;
      ARP.UpdatePercentProgress(((i+1)*100) div DispTaxaList.Count);
  end;

  //If not found, Look for start of the name
  ARP.UpdatePercentProgress(0);
  ARP.UpdateRunStatusInfo('Searching', 'For "begins with" ...');
  for i:=RowToStartAt downto 1 do // DP Removed -1 from DispTaxaList.Count-1 because it was missing the last taxa
  begin
    if Pos(UpperCase(CurSearchStr), UpperCase(D_InputSeqData.TaxaName[i-1])) = 1 then //start with it not case sensitive now after refactor
    begin
      Result := i;
      Exit;
    end;
    ARP.UpdatePercentProgress(((i+1)*100) div DispTaxaList.Count);
  end;

  //If still not found, see if their search string is anywhere in any of the taxa
  ARP.UpdatePercentProgress(0);
  ARP.UpdateRunStatusInfo('Searching', 'For query anywhere in Taxa');
  for i:=RowToStartAt downto 1 do // DP Removed -1 from DispTaxaList.Count-1 because it was missing the last taxa
  begin
    if AnsiContainsText(D_InputSeqData.TaxaName[i-1], CurSearchStr) then
    begin
      Result := i;
      Exit;
    end;
    ARP.UpdatePercentProgress(((i+1)*100) div DispTaxaList.Count);
  end;
  Result := -1; //we haven't found it  
end;

function TD_InputSeqData.DoTranslation: Boolean;  // returns false on error
var
  i, j: LongInt;
  OneSeq, NucSeq: PAnsiChar;
  Codon: array[0..2] of AnsiChar;
  SitesPerPercent, SitesDone: longInt;
begin
  try
    Result := False;
    if svUpdateAASeq in JobsPending then
    begin
      if svUpdateCodonStarts in JobsPending then
        UpdateAACodonStarts;

      if AACodonStarts.Count = 0 then
      begin
        RaiseErrorMessage(HC_Unexpected_Error, 'There are 0 full codons in the data, so no translation is possible.');
        FNoOfSites   := NoOfNucSites;
        VS_SeqDataExplorer.IsTranslated := False;
        ReferenceSeq := Sequence[0];
        JobsPending  := JobsPending + [svUpdateBasicCounts];
//        UpdateSiteAttrArray;
        Exit;
      end;

      // Now we do the real translation
      if DoProgressCheckCancel(0, 'Translating...') then
        raise EAbort.Create('user cancelled');
      SitesPerPercent := FNoOfTaxa * AACodonStarts.Count div 100;
      SitesDone := 0;

      for i:= 0 to FNoOfTaxa-1 do
      begin
        OneSeq := FOtuInfos[i].RsvData;
        NucSeq := PAnsiChar(FOtuInfos[i].Data);
        for j:= 0 to AACodonStarts.Count-1 do
        begin
          Inc(SitesDone);
          if (SitesPerPercent > 0) and ((SitesDone mod SitesPerPercent) = 0) then
            if DoProgressCheckCancel(SitesDone div SitesPerPercent, 'Translating...') then
              raise EAbort.Create('user cancelled');
          Codon[0]  := UpCase(NucSeq[AACodonStarts[j]]);
          Codon[1]  := UpCase(NucSeq[AACodonPos2[j]]);
          Codon[2]  := UpCase(NucSeq[AACodonPos3[j]]);
          OneSeq[j] := GetAA(Codon);
        end;
      end;
      JobsPending := JobsPending - [svUpdateAASeq];
    end;

    if AACodonStarts.Count = 0 then
    begin
      RaiseErrorMessage(HC_Unexpected_Error, 'There are 0 full codons in the data, so no translation possible.');
      FNoOfSites   := NoOfNucSites;
      VS_SeqDataExplorer.IsTranslated := False;
      ReferenceSeq := Sequence[0];
      JobsPending  := JobsPending + [svUpdateBasicCounts];
//      UpdateSiteAttrArray;
      Exit;
    end;

    FNoOfSites      := AACodonStarts.Count;
    VS_SeqDataExplorer.IsTranslated    := True;
    ReferenceSeq    := Sequence[0];
    JobsPending := JobsPending + [svUpdateBasicCounts];
//    UpdateSiteAttrArray;
    Result := True;
  finally
    if Assigned(PleaseWait) and PleaseWait.Visible then
      PleaseWait.Hide
  end;
end;

procedure TD_InputSeqData.DoUnTranslation;
begin
  FNoOfSites   := NoOfNucSites;
  VS_SeqDataExplorer.IsTranslated := False;
  ReferenceSeq := Sequence[0];
  JobsPending  := JobsPending + [svUpdateBasicCounts];
//  UpdateSiteAttrArray;
end;

function TD_InputSeqData.GetDomainName(Index: Integer): AnsiString;
begin
  Result := EmptyStr;
  if FDomainMarks = nil then
    Exit;
  Result := FDomainMarks[Index].Name;
end;

//--------- Get Translated sequence ------
function TD_InputSeqData.GetAA(Codon: array of AnsiChar):AnsiChar;
var
  x, y, z: LongInt;
begin
  x := -1; y := -1; z := -1;
  case Codon[0] of  'U', 'T': x:=0; 'C': x:=1; 'A': x:=2; 'G': x:=3; end;
  case Codon[1] of  'U', 'T': y:=0; 'C': y:=1; 'A': y:=2; 'G': y:=3; end;
  case Codon[2] of  'U', 'T': z:=0; 'C': z:=1; 'A': z:=2; 'G': z:=3; end;

  if (x < 0) or (y < 0) or (z < 0) then
  begin
    if (Codon[0] = FGapSym) or (Codon[1] = FGapSym) or (Codon[2] = FGapSym) then
      Result := '-'
    else
      Result := '?'
  end
  else
    Result := FCodeTable[x*16+y*4+z]; // simple index
end;

function TD_InputSeqData.GetCodeTable: AnsiString;
begin
  Result := AnsiString(FCodeTable);
end;

function TD_InputSeqData.GetDomainCount: Integer;
begin
  Result := FDomainMarks.NoOfDomains;
end;

function TD_InputSeqData.GetDomainMark(Index: Integer): TDomainInfo;
begin
  if FIsNuc and VS_SeqDataExplorer.IsTranslated then
    Result := FDomainMarks[AACodonStarts[Index]] // domain mark is from
  else
    Result := FDomainMarks[Index];
end;

function TD_InputSeqData.GetGeneName(Index: Integer): AnsiString;
begin
  Result := EmptyStr;
  if FDomainMarks = nil then
    Exit;
  Result := FDomainMarks[Index].GeneName;
end;

function TD_InputSeqData.GetGpName(Index: Integer): AnsiString;
begin
  Index := DispTaxaList[Index];
  if Index < 0 then
    Result := EmptyStr
  else
    Result := FOtuInfos[Index].GpName;
end;

function TD_InputSeqData.GetMaxAmbiguousAllowed(AOptions: TDataSubsetOptions; NumTaxa: Integer; SiteCoverage: Integer): Integer;
var
  MinSiteCoverage: Integer;
begin
    MinSiteCoverage := -1;
    if dsoCompleteDeletion in AOptions then
      MinSiteCoverage := NumTaxa // 100% coverage needed
    else if dsoPartialDeletion in AOptions then
    begin
      MinSiteCoverage := Ceil(SiteCoverage*1.0/100 * NumTaxa);
      if MinSiteCoverage < 1 then
        MinSiteCoverage := 1;
    end
    else // Pairwise
      MinSiteCoverage := 0;
    Result := NumTaxa - MinSiteCoverage;
end;

function TD_InputSeqData.GetMaxSitesIncluded(AOptions: TDataSubsetOptions;
  LabelstoInclude: TStringlist): Integer;
var
  i: Integer;
  MaxSites: Integer;
begin
  Result := 0;
  if (FIsNuc and FIsCoding) and (dsoUseCodon in AOptions) then // for codon or aa analysis
  begin
    for i := 0 to AACodonStarts.Count-1 do
      if GetIsSiteIncluded(AOptions, LabelsToInclude, AACodonStarts[i],AACodonPos2[i],AACodonPos3[i]) then
        Inc(Result);
  end
  else
  begin
    if FIsNuc then
      MaxSites := NoOfNucSites
    else
      MaxSites := FNoOfSites;

    for i:= 0 to MaxSites-1 do
    begin
      if GetIsSiteIncluded(AOptions, LabelsToInclude, i) then
        Inc(Result);
    end;
  end;
end;

function TD_InputSeqData.GetSpName(Index: Integer): AnsiString;
begin
  Index := DispTaxaList[Index];
  if Index < 0 then  Result := EmptyStr
  else               Result := FOtuInfos[Index].SpName;
end;

function TD_InputSeqData.GetPopName(Index: Integer): AnsiString;
begin
  Index := DispTaxaList[Index];
  if Index < 0 then  Result := EmptyStr
  else               Result := FOtuInfos[Index].PopName;
end;

//-------- some get functions
function TD_InputSeqData.GetSequence(Index: Integer): PAnsiChar;
begin
  try
    Index := DispTaxaList[Index];
    if FIsNuc and VS_SeqDataExplorer.IsTranslated then
      Result := FOtuInfos[Index].RsvData
    else
      Result := FOtuInfos[Index].Data;
  except
    on E: Exception do
    begin
      raise Exception.Create('Error in GetSequence: ' + E.Message);
    end;
  end;
end;

function TD_InputSeqData.GetSeqUsed(Index: Integer): Boolean;
begin
  Index := DispTaxaList[Index];
  if Index < 0 then
    Result := False
  else
    Result := FOtuInfos[Index].IsUsed;
end;

function TD_InputSeqData.GetSiteLabelType(Index: LongInt): AnsiChar;
begin
  Result := FDomainMarks.SiteLabelType[Index];
end;

function TD_InputSeqData.GetNoOfSiteLabelTypes: Integer;
begin
  Result := FDomainMarks.NoOfSiteLabelTypes;
end;

function TD_InputSeqData.HasSiteLabels: Boolean;
begin
  Result := FDomainMarks.HasLabelledSites;
end;

function TD_InputSeqData.GetSiteLabel(Index: Integer): AnsiChar;
var
  a,b,c: AnsiChar;
begin
  if FIsNuc and VS_SeqDataExplorer.IsTranslated then
  begin
    a := FDomainMarks.SiteLabel[AACodonStarts[Index]]; // domain mark is from
    b := FDomainMarks.SiteLabel[AACodonPos2[Index]];
    c := FDomainMarks.SiteLabel[AACodonPos3[Index]];
    if (a = b) and (b = c) then
      Result := a
    else
      Result := '_';  // no marks
  end
  else
    Result := FDomainMarks.SiteLabel[Index];
end;

function TD_InputSeqData.GetTaxaName(Index: Integer): AnsiString;
begin
  Index := DispTaxaList[Index];
  if Index < 0 then  Result := EmptyStr
  else               Result := FOtuInfos[Index].Name;
end;

function TD_InputSeqData.GetGeographicalInfo(index: Integer): TGeographicalInfo;
begin
  Index := DispTaxaList[Index];
  if Index < 0 then
    Result := nil
  else
    Result := FOtuInfos[Index].GeographicalInfo;
end;

// Test for any given attribute
function TD_InputSeqData.HasAttribute(Site: Integer; Attr: TSiteAttrType
  ): Boolean;
begin
  Result := (Attr in SiteAttrArray[Site]);
end;

//--------- Functions to test if Unambiguous base/residue ---
function TD_InputSeqData.IsGoodNuc(Base: AnsiChar):Boolean;
begin
  case UpCase(Base) of
    'A', 'T', 'U', 'C', 'G': Result := True;
  else                       Result := False; end;
end;

function TD_InputSeqData.GetIsSiteIncluded(AOptions: TDataSubsetOptions; LabelsToInclude: TStringList; SiteNum: LongInt; Pos2: LongInt = -1; Pos3: LongInt = -1): Boolean;
begin
    Result := False;
    // check if the nuc/codon/amino acid is included
    With FDomainMarks do
    begin
      if dsoUseOnlyLabelledSites in AOptions then
      begin
        if not FDomainMarks.IsSiteUsedWithLabel(SiteNum, (LabelsToInclude <> nil)) then // for Labelled sites
          Exit;
      end
      else if dsoUseOnlyUnlabelledSites in AOptions then
      begin
        if not FDomainMarks.IsSiteUsedWithNoLabel(SiteNum) then // for unlabelled sites
          Exit;
      end
      else
        if not FDomainMarks.IsSiteUsed[SiteNum] then
          Exit;
    end;
    if (not FIsNuc) or (not FIsCoding) then // no more complications
    begin
      Result := True;
      Exit;
    end;

    // for coding nucleotide data
    if dsoUseNuc in AOptions then // use nucsite-by-nucsite
    begin
      if (meg1stBase in SiteAttrArray[SiteNum]) and not (dsoUse1stPos in AOptions) then Exit;
      if (meg2ndBase in SiteAttrArray[SiteNum]) and not (dsoUse2ndPos in AOptions) then Exit;
      if (meg3rdBase in SiteAttrArray[SiteNum]) and not (dsoUse3rdPos in AOptions) then Exit;
      if (meg0thBase in SiteAttrArray[SiteNum]) and not (dsoUseNonCod in AOptions) then Exit;
    end
    else if (dsoUseCodon in AOptions) then // for Nuc codon or aa analysis
      if not( FDomainMarks.IsSiteUsed[SiteNum ] and
              FDomainMarks.IsSiteUsed[Pos2] and
              FDomainMarks.IsSiteUsed[Pos3]) then Exit;
    Result := True;
end;

// Main function to be called for preparing data for the CodonOmegaAnalysis(hyphy codon by codon test of selection)
function TD_InputSeqData.PrepareDataForCodonOmegaAnalysis(
  AOptions: TDataSubsetOptions;
  const ASeqList: TList;
  const MyUsedOtuInfos: TList;
  var TotalTaxa: Integer;
  var TotalSites: Integer;
  LabelsToInclude: TStringList;
  SiteCoverage: Integer): TIntArray;
var
  ASeq: PAnsiChar;
  CodonColStart: PAnsiChar; // the column in the seq data for the first position of the currrent codon
  CodonCol2nd: PAnsiChar;   // the column in the seq data for the second position of the currrent codon
  CodonCol3rd: PAnsiChar;   // the column in the seq data for the third position of the currrent codon
  CodonMaps: PAnsiChar;     // the columns in the seq data for the current codon, mapped to an integer (so we can call the function for identifying stop codons)
  MyNTaxa: LongInt;
  MyNSites: LongInt;
  i: LongInt;
  j: LongInt;
  Index: LongInt;
  WriteAtIndex : Longint;
  Codon: array[0..2] of AnsiChar;
  MinSiteCoverage: Integer;
  MaxAmbiguousAllowed : Integer;
  ProgressIncrement: Integer;

  // filter out codons based on selected domains/genes and site labels
  function IsSiteIncluded(StartPos: Longint; Pos2: LongInt; Pos3:LongInt): Boolean;
  begin
    Result := False;

    With FDomainMarks do
    begin
      if dsoUseOnlyLabelledSites in AOptions then
      begin
        if (not FDomainMarks.IsSiteUsedWithLabel(StartPos, (LabelsToInclude <> nil))) or
           (not FDomainMarks.IsSiteUsedWithLabel(Pos2, (LabelsToInclude <> nil))) or
           (not FDomainMarks.IsSiteUsedWithLabel(Pos3, (LabelsToInclude <> nil))) then
          Exit;
      end
      else if dsoUseOnlyUnlabelledSites in AOptions then
      begin
        if (not FDomainMarks.IsSiteUsedWithNoLabel(StartPos)) or
           (not FDomainMarks.IsSiteUsedWithNoLabel(Pos2)) or
           (not FDomainMarks.IsSiteUsedWithNoLabel(Pos3)) then
          Exit;
      end
      else
        if not(FDomainMarks.IsSiteUsed[StartPos] and
               FDomainMarks.IsSiteUsed[Pos2] and
               FDomainMarks.IsSiteUsed[Pos3]) then
          Exit;
    end;
    Result := True;
  end;

  function IsGapOnlySite: Boolean;
  var
    k: Integer;
  begin
    k := 0;
    Result := False;
    for k := 0 to MyNTaxa - 1 do
    begin
      if (CodonColStart[k] <> FGapSym) or
         (CodonCol2nd[k] <> FGapSym) or
         (CodonCol3rd[k] <> FGapSym) then
         Exit;
    end;
    Result := True;
  end;

begin
  TotalSites := 0;
  TotalTaxa  := 0;
  ASeqList.Clear;
  CodonColStart := nil;
  CodonCol2nd := nil;
  CodonCol3rd := nil;
  MyNTaxa := MyUsedOtuInfos.Count;
  UpdateAACodonStarts;
  SetLength(Result, 0);

  try try
    // allocates memory for the column
    GetMem(CodonColStart, MyNTaxa * sizeOf(AnsiChar));
    GetMem(CodonCol2nd, MyNTaxa * sizeOf(AnsiChar));
    GetMem(CodonCol3rd, MyNTaxa * sizeOf(AnsiChar));
    GetMem(CodonMaps, MyNTaxa * sizeOf(AnsiChar));

    // tell DomainMarks about the site labels to use
    FDomainmarks.SetSiteLabelsToInclude(LabelsToInclude);

    // Get the upperbound on the number of sites/codons
    MyNSites := 0;
    for i := 0 to AACodonStarts.Count-1 do
      if IsSiteIncluded(AACodonStarts[i],AACodonPos2[i],AACodonPos3[i]) then
        Inc(MyNSites, 3);
    if MyNSites = 0 then
      Exit;

    //== Determination of site coverage
    MinSiteCoverage := -1;
    if dsoCompleteDeletion in AOptions then
      MinSiteCoverage := MyNTaxa // 100% coverage needed
    else if dsoPartialDeletion in AOptions then
    begin
      MinSiteCoverage := Ceil(SiteCoverage*1.0/100*MyNTaxa);
      if MinSiteCoverage < 1 then
       MinSiteCoverage := 1;
    end;
    MaxAmbiguousAllowed := MyNTaxa - MinSiteCoverage;

    // Allocate memory for MyNSites; comp-del sites is fewer or equal
    for i:=0 to MyNTaxa-1 do
    begin
      GetMem(ASeq, sizeOf(AnsiChar)*(MyNSites+1));
      ASeqList.Add(ASeq);
    end;

    WriteAtIndex := 0;  //== keep track of the sites included
    begin
      i := 0;
      ProgressIncrement := Max(1, Round(AACodonStarts.Count / 100));
      while i < AACodonStarts.Count do
      begin
        if not IsSiteIncluded(AACodonStarts[i],AACodonPos2[i],AACodonPos3[i]) then
        begin
          inc(i);
          Continue;
        end;
        if Assigned(ARP) then
          if (i mod ProgressIncrement) = 0 then
          begin
            ARP.Progress := Round(i / (AACodonStarts.Count - 1) * 100);
            {$IFDEF VISUAL_BUILD}
            Application.ProcessMessages;
            {$ENDIF}
          end;
        Index := 0;
        for j:=0 to MyNTaxa-1 do
        begin
          ASeq := TOtuInfo(MyUsedOtuInfos[j]).Data;
          Codon[0] := UpCase(ASeq[AACodonStarts[i]]);
          Codon[1] := UpCase(ASeq[AACodonPos2[i]]);
          Codon[2] := UpCase(ASeq[AACodonPos3[i]]);
          CodonMaps[Index] := CodonInfo.MakeCodonBitMap(Codon); // so we can call HasStopCodonInCodonMapCol
          CodonColStart[Index] := UpCase(ASeq[AACodonStarts[i]]);
          CodonCol2nd[Index] := UpCase(ASeq[AACodonPos2[i]]);
          CodonCol3rd[Index] := UpCase(ASeq[AACodonPos3[i]]);
          Inc(Index);
        end;
        inc(i);

       // check for too many ambiguous bases as well as stop codons and gap-only sites
        if CountAmbiguousInNucCol(CodonColStart, MyNTaxa) > MaxAmbiguousAllowed then
            Continue;
        if CountAmbiguousInNucCol(CodonCol2nd, MyNTaxa) > MaxAmbiguousAllowed then
            Continue;
        if CountAmbiguousInNucCol(CodonCol3rd, MyNTaxa) > MaxAmbiguousAllowed then
            Continue;
        if HasStopCodonInCodonMapCol(CodonMaps, MyNTaxa, FCodeTable) then // stop codon sites are removed
          Continue;
        if IsGapOnlySite then
          Continue;

        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := (i - 1) * 3 + 1;
        j := 0; // keeps track of which sequence we are adding a codon to
        while j < MyNTaxa do
        begin
          PAnsiChar(ASeqList[j])[WriteAtIndex] := CodonColStart[j];
          PAnsiChar(ASeqList[j])[WriteAtIndex + 1] := CodonCol2nd[j];
          PAnsiChar(ASeqList[j])[WriteAtIndex + 2] := CodonCol3rd[j];
          inc(j);
        end;
        Inc(WriteAtIndex, 3);
      end; // end while i < aacodonstarts.count
    end;
    TotalSites := WriteAtIndex;
    TotalTaxa  := MyNTaxa;
  except
    On E: Exception do
    begin
      SetLength(Result, 0);
      if ASeqList <> nil then
      begin
        for i:=0 to ASeqList.Count-1 do
        begin
          if ASeqList[i] <> nil then
            FreeMem(ASeqList[i]);
          ASeqList[i] := nil;
        end;
        ASeqList.Clear;
      end;
      RaiseErrorMessage(HC_Unexpected_Error, 'Failed to prepare data for codon based test of selection.');
    end;
  end;
  finally
  if Assigned(CodonColStart) then
    FreeMemAndNil(CodonColStart);
  if Assigned(CodonCol2nd) then
    FreeMemAndNil(CodonCol2nd);
  if Assigned(CodonCol3rd) then
    FreeMemAndNil(CodonCol3rd);
  if Assigned(CodonMaps) then
    FreeMemAndNil(CodonMaps);
  FDomainmarks.SetSiteLabelsToInclude(nil); // clearout
  end;
end;  { end PrepareDataForCodonOmegaAnalysis }

function TD_InputSeqData.GetCodonStringAtNucSite(aSite: Int64; aSeq: Int64): AnsiString;
var
  codon: array[0..2] of AnsiChar;
  i: Integer;
  base: AnsiChar;
begin
  for i := Low(codon) to High(codon) do
    codon[i] := #0;
  Result := EmptyStr;
  if meg1stBase in SiteAttrArray[aSite] then
  begin
    if (aSite + 2) < FNoOfSites then
    begin
      codon[0] := D_InputSeqData.Sequence[aSeq][aSite];
      codon[1] := D_InputSeqData.Sequence[aSeq][aSite + 1];
      codon[2] := D_InputSeqData.Sequence[aSeq][aSite + 2];
    end;
  end
  else if meg2ndBase in SiteAttrArray[aSite] then
  begin
    if (aSite + 1) < FNoOfSites then
    begin
      codon[0] := D_InputSeqData.Sequence[aSeq][aSite - 1];
      codon[1] := D_InputSeqData.Sequence[aSeq][aSite];
      codon[2] := D_InputSeqData.Sequence[aSeq][aSite + 1];
    end;
  end
  else if meg3rdBase in SiteAttrArray[aSite] then
  begin
    if aSite < FNoOfSites then
    begin
      codon[0] := D_InputSeqData.Sequence[aSeq][aSite - 2];
      codon[1] := D_InputSeqData.Sequence[aSeq][aSite - 1];
      codon[2] := D_InputSeqData.Sequence[aSeq][aSite];
    end;
  end;
  base := GetAA(codon);
  Result := Format('%s (%s)', [UpperCase(base), UpperCase(GetThreeLetterCode(base))]);
end;

procedure TD_InputSeqData.PrepareDataForDistAnalysis(
  AOptions: TDataSubsetOptions; const ASeqList, MyUsedOtuInfos: TList;
  var TotalTaxa, TotalSites: Integer; LabelsToInclude: TStringList;
  ARecodeScheme: AnsiString; SiteCoverage: Integer);
var
  ASeq: PAnsiChar;
  ColConfig: PAnsiChar;    // filled over and over again
  MaxSites, MyNTaxa, MyNSites, i, j: LongInt;
  Index, WriteAtIndex : Longint;
  Codon: array[0..2] of AnsiChar;
  RecodeScheme: TArrayAtoZOfChar;
  MaxAmbiguousAllowed : Integer;
  ProgressIncrement: Integer;

begin
  TotalSites := 0;
  TotalTaxa  := 0;
  ASeqList.Clear;
  ColConfig := nil;
  SetLength(IncludedSites, 0);

  if (FIsNuc and FIsCoding) and (dsoUseCodon in AOptions) then // for codon or aa analysis
    UpdateAACodonStarts;

  try try
    MyNTaxa := MyUsedOtuInfos.Count; //FOtuInfos.NoOfSelOtus;

    // allocates memory for the column
    GetMem(ColConfig, MyNTaxa*sizeOf(AnsiChar));

    // tell DomainMarks about the site labels to use
    FDomainmarks.SetSiteLabelsToInclude(LabelsToInclude);

    // Get the upperbound on the number of sites/codons
    MyNSites := GetMaxSitesIncluded(AOptions, LabelsToInclude);
    if MyNSites = 0 then
    begin
      ARP := nil;
      Exit;
    end;

    //== Determination of site coverage
    MaxAmbiguousAllowed := GetMaxAmbiguousAllowed(AOptions, MyNTaxa, SiteCoverage);

    // Allocate memory for MyNSites; comp-del sites is fewer or equal
    for i:=0 to MyNTaxa-1 do
    begin
      GetMem(ASeq, sizeOf(AnsiChar)*(MyNSites+1));
      ASeqList.Add(ASeq);
    end;

    //== keep track of the sites included
    WriteAtIndex := 0;  //

    // Recoding scheme
    // Sequence Data set does not care what it is, it just assumes that MegaUtils used is OK
    if (dsoRecodeBases in AOptions) and (Length(ARecodeScheme)>0) then
    begin
       if dsoUseAmino in AOptions then
        ConvertStringToRecodeScheme(ARecodeScheme, RecodeScheme, 'X')
       else
        ConvertStringToRecodeScheme(ARecodeScheme, RecodeScheme, 'N');
    end;

    if dsoUseCodon in AOptions then  // if the data is treated codon-by-codon
    begin
      for i:=0 to AACodonStarts.Count-1 do
      begin
        if not GetIsSiteIncluded(AOptions, LabelsToInclude, AACodonStarts[i],AACodonPos2[i],AACodonPos3[i]) then
          Continue;
        Index := 0;
        for j:=0 to MyNTaxa-1 do
        begin
          ASeq := TOtuInfo(MyUsedOtuInfos[j]).Data;
          Codon[0]  := UpCase(ASeq[AACodonStarts[i]]);
          Codon[1]  := UpCase(ASeq[AACodonPos2[i]]);
          Codon[2]  := UpCase(ASeq[AACodonPos3[i]]);
          if dsoUseAmino in AOptions then
          begin
            ColConfig[Index] := UpCase(GetAA(Codon));
            if (dsoRecodeBases in AOptions) then     //recode here
              ColConfig[Index] := RecodeNucOrAmino(ColConfig[Index], RecodeScheme);
          end
          else
            ColConfig[Index] := CodonInfo.MakeCodonBitMap(Codon);
          Inc(Index);
        end;

        if (dsoUseAmino in AOptions) then
        begin
          if CountAmbiguousInAminoCol(ColConfig, MyNTaxa) > MaxAmbiguousAllowed then
            Continue;

          if HasStopAminoInAminoCol(ColConfig, MyNTaxa, i=(AACodonStarts.Count-1)) then // stop codon sites are removed
            Continue;
          // note that bases are recoded after sites are included or excluded

          for j:=0 to MyNTaxa-1 do
          begin
            if dsoDistMap in AOptions then
              PAnsiChar(ASeqList[j])[WriteAtIndex] := AminoToDistMap(ColConfig[j])
            else
              PAnsiChar(ASeqList[j])[WriteAtIndex] := ColConfig[j];
          end;
        end
        else  // the codon is being used as a triplet
        begin
          if CountAmbiguousInCodonMapCol(ColConfig, MyNTaxa) > MaxAmbiguousAllowed then
              Continue;

          if HasStopCodonInCodonMapCol(ColConfig, MyNTaxa, FCodeTable) then // stop codon sites are removed
            Continue;
          for j:=0 to MyNTaxa-1 do
            PAnsiChar(ASeqList[j])[WriteAtIndex] := ColConfig[j]; // just the codon's map is used as is
        end;
        Inc(WriteAtIndex);
        SetLength(IncludedSites, Length(IncludedSites) + 3);
        IncludedSites[Length(IncludedSites) - 3] := AACodonStarts[i] + 1;
        IncludedSites[Length(IncludedSites) - 2] := AACodonPos2[i] + 1;
        IncludedSites[Length(IncludedSites) - 1] := AACodonPos3[i] + 1;
      end; // end over i for aa codon starts
    end  // end of if dsoUseCodon
    else // we need to use only nuc or only amino
    begin
      if FIsNuc then
        MaxSites := NoOfNucSites
      else
        MaxSites := FNoOfSites;
      ProgressIncrement := Max(1, Round(MaxSites / 100));
      for i:=0 to MaxSites-1 do
      begin
        if not GetIsSiteIncluded(AOptions, LabelsToInclude, i) then
          Continue;
        if Assigned(ARP) then
          if (MaxSites > 1) and ((i mod ProgressIncrement) = 0) then
          begin
            ARP.Progress := Round(i / (MaxSites - 1) * 100);
            {$IFDEF VISUAL_BUILD}
            Application.ProcessMessages;
            {$ENDIF}
          end;
        // fill up the Col with the appropriate data
        Index := 0;
        for j:=0 to MyNTaxa-1 do
        begin
          ASeq := TOtuInfo(MyUsedOtuInfos[j]).Data;
          ColConfig[Index] := Upcase(ASeq[i]);
          if (dsoRecodeBases in AOptions) then     //recode here
              ColConfig[Index] := RecodeNucOrAmino(ColConfig[Index], RecodeScheme);
          Inc(Index);
        end;

        if dsoUseNuc in AOptions then
        begin
          if CountAmbiguousInNucCol(ColConfig, MyNTaxa) > MaxAmbiguousAllowed then
            Continue;

          for j:=0 to MyNTaxa-1 do
          begin
            if dsoDistMap in AOptions then
              PAnsiChar(ASeqList[j])[WriteAtIndex] := NucToDistMap(ColConfig[j])
            else
              PAnsiChar(ASeqList[j])[WriteAtIndex] := ColConfig[j];
          end;
        end
        else if dsoUseAmino in AOptions then
        begin
          if CountAmbiguousInAminoCol(ColConfig, MyNTaxa) > MaxAmbiguousAllowed then
            Continue;

          if HasStopAminoInAminoCol(ColConfig, MyNTaxa, i=MaxSites-1) then // stop codon sites are removed
            Continue;
          for j:=0 to MyNTaxa-1 do
          begin
            if dsoDistMap in AOptions then
              PAnsiChar(ASeqList[j])[WriteAtIndex] := AminoToDistMap(ColConfig[j])
            else
              PAnsiChar(ASeqList[j])[WriteAtIndex] := ColConfig[j];
          end;
        end;
        Inc(WriteAtIndex);
        SetLength(IncludedSites, Length(IncludedSites) + 1);
        IncludedSites[Length(IncludedSites) - 1] := i + 1;
      end;
    end;
    TotalSites := WriteAtIndex;
    TotalTaxa  := MyNTaxa;
  except
    On E: Exception do
    begin
      if ASeqList <> nil then
      begin
        for i:=0 to ASeqList.Count-1 do
        begin
          if ASeqList[i] <> nil then
            FreeMem(ASeqList[i]);
          ASeqList[i] := nil;
        end;
        ASeqList.Clear;
      end;
      RaiseErrorMessage(HC_Unexpected_Error, E.Message);
    end;
  end;
  finally
    if ColConfig <> nil then FreeMemAndNil(ColConfig);
    FDomainmarks.SetSiteLabelsToInclude(nil); // clearout
    ARP := nil;
  end;
end;

//=== Functions to prepare data for parsimony analysis
// Main function to be called for preparing data for computing distances
procedure TD_InputSeqData.PrepareDataForParsimAnalysis(
  AOptions: TDataSubsetOptions; const ASeqList, MyUsedOtuInfos: TList;
  var TotalTaxa, TotalSites, InfoSites, ConstContribute: Integer;
  LabelsToInclude: TStringList; ARecodeScheme: AnsiString; SiteCoverage: Integer
  );
var
  i, j: LongInt;
  AIntSeq: PArrayOfInt;
  ACharSeq, ASeq: PAnsiChar;
  ColConfig: PAnsiChar;
  TempSeqList: TList;
  MinTLen: LongInt;
  ProgressIncrement: Integer;
begin
  ColConfig := nil;
  TempSeqList := nil;
  ConstContribute := 0;

  try
    TempSeqList := TList.Create;

    // now we use the DistAnalysis system to get the data
    PrepareDataForDistAnalysis(AOptions, TempSeqList, MyUsedOtuInfos, TotalTaxa, TotalSites, LabelsToInclude, ARecodeScheme, SiteCoverage);

    // since dist analysis already recodes everything needed, when needed, parsimony does not need to deal wth it any more
    // we do parsimony related stuff

    GetMem(ColConfig, TotalTaxa*sizeOf(AnsiChar));

    // Allocate memory for MyNSites; comp-del sites is fewer or equal
    for i:=0 to TotalTaxa-1 do  // only minimum required number is allocated
    begin
      if (dsoUseAmino in AOptions) and (dsoParsimMap in AOptions) then
      begin
        GetMem(AIntSeq, sizeOf(LongInt)*(TotalSites+1));
        ASeqList.Add(AIntSeq);
      end
      else
      begin
        GetMem(ACharSeq, sizeOf(AnsiChar)*(TotalSites+1));
        ASeqList.Add(ACharSeq);
      end
    end;

    //== now we extract
    InfoSites := 0;
    ProgressIncrement := Max(1, Round(TotalSites / 100));
    //== keep track of the sites included
    // at present assume that the AIndexMap = nil; so re-arrange freely.
    for i:=0 to TotalSites-1 do
    begin
      if Assigned(ARP) then
        if (i mod ProgressIncrement) = 0 then
        begin
          ARP.Progress := Round(i / (TotalSites - 1) * 100);
          {$IFDEF VISUAL_BUILD}
          Application.ProcessMessages;
          {$ENDIF}
        end;
      for j:=0 to TotalTaxa-1 do
      begin
        ASeq := TempSeqList[j];
        ColConfig[j] := Upcase(ASeq[i]);
      end;

      MinTLen := 0;  // added in Mega2 beta build 2; absence caused integer overflow
      // for rearranging
      if  ((dsoUseNuc in AOptions)   and  IsNucColParsimInfo(ColConfig, TotalTaxa, MinTLen))   or
          ((dsoUseAmino in AOptions) and  IsAminoColParsimInfo(ColConfig, TotalTaxa, MinTLen)) then
        Inc(InfoSites);

      // else  else was removed because it led to wrong code.  fixed in mega2 beta build 2

      ConstContribute := ConstContribute + MinTLen;

      // for writing
      for j:=0 to TotalTaxa-1 do
        if not(dsoParsimMap in AOptions) then
        begin
          ACharSeq := ASeqList[j];
          ACharSeq[i] := ColConfig[j];
        end
        else
        begin
          if (dsoUseNuc in AOptions) then
          begin
            ACharSeq := ASeqList[j];
            ACharSeq[i] := NucToParsimMap(ColConfig[j]);
          end
          else if (dsoUseAmino in AOptions) then
          begin
            AIntSeq := ASeqList[j];
            AIntSeq[i] := AminoToParsimMap(ColConfig[j]);
          end;
        end;
    end;
  finally
    if ColConfig <> nil then FreeMemAndNil(ColConfig);
    if TempSeqList <> nil then
    begin
      for i:=0 to TempSeqList.Count-1 do
	  begin
        if TempSeqList[i] <> nil then
          FreeMem(TempSeqList[i]);
		TempSeqList[i] := nil;
	  end;
      FreeAndNil(TempSeqList);
    end;
  end;
end;

procedure TD_InputSeqData.PrepareDataForPartitionedMLAnalysis(
  AOptions: TDataSubsetOptions; var PartitionList: TListOfSeqPartitions;
  MyUsedOtuInfos: TList; var TotalTaxa, TotalSites: Integer;
  LabelsToInclude: TStringList; ARecodeScheme: AnsiString;
  SiteCoverage: Integer);
var
  ASeq: PAnsiChar;
  ColConfig: PAnsiChar;    { a single column in the alignment}
  MyNTaxa, MyNSites, i, j: LongInt;
  Index: LongInt;
  WriteAtIndex : Longint; { keeps track of the sites included}
  Codon: array[0..2] of AnsiChar;
  RecodeScheme: TArrayAtoZOfChar;
  MaxAmbiguousAllowed : Integer; { for enforcing site coverage constraints}
  IncludedDomains: TDomainInfoArray; { those domains which are selected for the analysis}
  DomainInfo: TDomainInfo;
  DIndex: Integer; { index of the current domain}
  FirstSite, LastSite: Integer; { start and end coordinates for the current domain}
  TempSeqs: TStringList;
  TempSeq: AnsiString;
  MySeqs: array of array of AnsiChar;
begin
  Assert(False, 'PrepareDataForPartitionedMLAnalysis not complete. Need to add progress reporting');
  TotalSites := 0;
  TotalTaxa  := 0;
  WriteAtIndex := 0;
  ColConfig := nil;
  SetLength(IncludedSites, 0);

  if (FIsNuc and FIsCoding) and (dsoUseCodon in AOptions) then // for codon or aa analysis
    UpdateAACodonStarts;

  try try
    MyNTaxa := MyUsedOtuInfos.Count;
    GetMem(ColConfig, MyNTaxa * sizeOf(AnsiChar));
    FDomainmarks.SetSiteLabelsToInclude(LabelsToInclude);
    MyNSites := GetMaxSitesIncluded(AOptions, LabelsToInclude);
    if MyNSites = 0 then
      Exit;

    SetLength(MySeqs, MyNTaxa);
    for i := 0 to MyNTaxa - 1 do
      SetLength(MySeqs[i], MyNSites);

    MaxAmbiguousAllowed := GetMaxAmbiguousAllowed(AOptions, MyNTaxa, SiteCoverage);

    if (dsoRecodeBases in AOptions) and (Length(ARecodeScheme) > 0) then
    begin
      if dsoUseAmino in AOptions then
        ConvertStringToRecodeScheme(ARecodeScheme, RecodeScheme, 'X')
      else
        ConvertStringToRecodeScheme(ARecodeScheme, RecodeScheme, 'N');
    end;

    FDomainMarks.GetIncludedDomains(IncludedDomains);

    if dsoUseCodon in AOptions then
    begin
      for DIndex := Low(IncludedDomains) to High(IncludedDomains) do
      begin
        WriteAtIndex := 0;
        for i := 0 to MyNTaxa - 1 do
          for j := 0 to MyNSites - 1 do
            MySeqs[i][j] := #0;
        FirstSite := IncludedDomains[DIndex].FromSite;
        LastSite := IncludedDomains[DIndex].ToSite;
        for i := 0 to AACodonStarts.Count - 1 do
        begin
          { make sure to only use codons which are in the current domain}
          if (AACodonStarts[i] < FirstSite) or (AACodonPos3[i] > LastSite) then
            continue;

          if not GetIsSiteIncluded(AOptions, LabelsToInclude, AACodonStarts[i],AACodonPos2[i],AACodonPos3[i]) then
            Continue;

          Index := 0;
          for j := 0 to MyNTaxa - 1 do
          begin
            ASeq := TOtuInfo(MyUsedOtuInfos[j]).Data;
            Codon[0]  := UpCase(ASeq[AACodonStarts[i]]);
            Codon[1]  := UpCase(ASeq[AACodonPos2[i]]);
            Codon[2]  := UpCase(ASeq[AACodonPos3[i]]);
            if dsoUseAmino in AOptions then
            begin
              ColConfig[Index] := UpCase(GetAA(Codon));
              if (dsoRecodeBases in AOptions) then     //recode here
                ColConfig[Index] := RecodeNucOrAmino(ColConfig[Index], RecodeScheme);
            end
            else
              ColConfig[Index] := CodonInfo.MakeCodonBitMap(Codon);
            Inc(Index);
          end;

          if (dsoUseAmino in AOptions) then
          begin
            if CountAmbiguousInAminoCol(ColConfig, MyNTaxa) > MaxAmbiguousAllowed then
              Continue;

            if HasStopAminoInAminoCol(ColConfig, MyNTaxa, i=(AACodonStarts.Count-1)) then // stop codon sites are removed
              Continue;
            // note that bases are recoded after sites are included or excluded

            for j:=0 to MyNTaxa-1 do
            begin
              if dsoDistMap in AOptions then
                MySeqs[j][WriteAtIndex] := AminoToDistMap(ColConfig[j])
              else
                MySeqs[j][WriteAtIndex] := ColConfig[j];
            end;
          end
          else  // the codon is being used as a triplet
          begin
            if CountAmbiguousInCodonMapCol(ColConfig, MyNTaxa) > MaxAmbiguousAllowed then
                Continue;

            if HasStopCodonInCodonMapCol(ColConfig, MyNTaxa, FCodeTable) then // stop codon sites are removed
              Continue;

            for j:=0 to MyNTaxa-1 do
              MySeqs[j][WriteAtIndex] := ColConfig[j]; // just the codon's map is used as is
          end;
          Inc(WriteAtIndex);
          SetLength(IncludedSites, Length(IncludedSites) + 3);
          IncludedSites[Length(IncludedSites) - 3] := AACodonStarts[i] + 1;
          IncludedSites[Length(IncludedSites) - 2] := AACodonPos2[i] + 1;
          IncludedSites[Length(IncludedSites) - 1] := AACodonPos3[i] + 1;
        end; // end over i for aa codon starts
        TempSeqs := TStringList.Create;
        for j := 0 to MyNTaxa - 1 do
        begin
          SetString(TempSeq, PAnsiChar(MySeqs[j]),WriteAtIndex);
          TempSeqs.Add(TempSeq);
        end;
        PartitionList.Add(TempSeqs);
      end;
    end
    else { then we are just using either nucleotide or amino-acid bases}
    begin
      for DIndex := Low(IncludedDomains) to High(IncludedDomains) do
      begin
        WriteAtIndex := 0;
        for i := 0 to MyNTaxa - 1 do
          for j := 0 to MyNSites - 1 do
            MySeqs[i][j] := #0;
        DomainInfo := IncludedDomains[DIndex];
        FirstSite := DomainInfo.FromSite;
        LastSite := DomainInfo.ToSite;
        for i := FirstSite to LastSite do
        begin
          if not GetIsSiteIncluded(AOptions, LabelsToInclude, i) then
            continue;

          Index := 0;
          for j:=0 to MyNTaxa-1 do
          begin
            ASeq := TOtuInfo(MyUsedOtuInfos[j]).Data;
            ColConfig[Index] := Upcase(ASeq[i]);
            if (dsoRecodeBases in AOptions) then     //recode here
                ColConfig[Index] := RecodeNucOrAmino(ColConfig[Index], RecodeScheme);
            Inc(Index);
          end;

          if dsoUseNuc in AOptions then
          begin
            if CountAmbiguousInNucCol(ColConfig, MyNTaxa) > MaxAmbiguousAllowed then
              Continue;

            for j:=0 to MyNTaxa-1 do
            begin
              if dsoDistMap in AOptions then
                MySeqs[j][WriteAtIndex] := NucToDistMap(ColConfig[j])
              else
                MySeqs[j][WriteAtIndex] := ColConfig[j];
            end;
          end
          else if dsoUseAmino in AOptions then
          begin
            if CountAmbiguousInAminoCol(ColConfig, MyNTaxa) > MaxAmbiguousAllowed then
              Continue;

            if HasStopAminoInAminoCol(ColConfig, MyNTaxa, i=lastSite) then // stop codon sites are removed
              Continue;

            for j:=0 to MyNTaxa-1 do
            begin
              if dsoDistMap in AOptions then
                MySeqs[j][WriteAtIndex] := AminoToDistMap(ColConfig[j])
              else
                MySeqs[j][WriteAtIndex] := ColConfig[j];
            end;
          end;
          Inc(WriteAtIndex);
          SetLength(IncludedSites, Length(IncludedSites) + 1);
          IncludedSites[Length(IncludedSites) - 1] := i + 1;
        end;
        TempSeqs := TStringList.Create;
        for i := 0 to MyNTaxa - 1 do
        begin
          SetString(TempSeq, PAnsiChar(MySeqs[i]), WriteAtIndex);
          TempSeqs.Add(TempSeq);
        end;
        PartitionList.Add(TempSeqs);
      end;
    end;
    TotalSites := WriteAtIndex;
    TotalTaxa  := MyNTaxa;
  except
    On E: Exception do
    begin
      RaiseErrorMessage(HC_Unexpected_Error, 'An error occurred when preparing the sequence data: ' + E.Message);
    end;
  end;
  finally
    if Assigned(ColConfig) then
      FreeMemAndNil(ColConfig);
    FDomainmarks.SetSiteLabelsToInclude(nil); // clearout
  end;
end;

procedure TD_InputSeqData.PreprocessDataFromReltimeAnalysis(MyUsedOtuInfos: TList);
begin
  if not Assigned(MyUsedOtuInfos) then
    MyUsedOtuInfos := TList.Create;
  MyUsedOtuInfos.Clear;
  CurrentAllOtuInfos.UsedOtuInfos(MyUsedOtuInfos, False);
end;

//  new for ML
procedure TD_InputSeqData.PrepareDataForMLAnalysis(MAI: TObject);
var
  i, j: LongInt;
  AString: AnsiString;
  ACharSeq: PAnsiChar;
  TempSeqList: TList = nil;
  aMsg: String = '';
  aInfo: TAnalysisInfo = nil;
  AOptions: TDataSubsetOptions;
  MyUsedOtuInfos: TList = nil;
  LabelsToInclude: TStringList = nil;
  ASeqList: TStringList = nil;
  ARecodeScheme: AnsiString;
  SiteCoverage: Integer;
  newSeqs: TStringList = nil;
  tempSeq: String;
begin
  if not (MAI is TAnalysisInfo) then
    raise Exception.Create('Application error: Invalid call to PrepareDataForMLAnalysis. Requires TAnalysisInfo');
  aInfo := TAnalysisInfo(MAI);
  ASeqList := aInfo.MySeqStrings;
  ASeqList.Clear;
  AOptions := aInfo.MySubsetOpt;
  MyUsedOtuInfos := aInfo.MyUsedOtuInfos;
  LabelsToInclude := aInfo.MyLabelsUsed;
  ARecodeScheme := aInfo.RecodeScheme;
  SiteCoverage := aInfo.SiteCoverage;

  try
    TempSeqList := TList.Create;

    // now we use the DistAnalysis system to get the data
    PrepareDataForDistAnalysis(AOptions, TempSeqList, MyUsedOtuInfos, aInfo.MyNoOfSeqs, aInfo.MyNoOfSites, LabelsToInclude, ARecodeScheme, SiteCoverage);
    SetLength(AString, aInfo.MyNoOfSites);   // only minimum required number is allocated
    for i := 0 to aInfo.MyNoOfSeqs-1 do
    begin
      ACharSeq := TempSeqList[i];
      for j := 1 to aInfo.MyNoOfSites do
        AString[j] := ACharSeq[j-1];
      ASeqList.Add(AString);
    end;
    if not CheckFilterEmptySequences(aInfo) then
      raise EAbort.Create('User aborted the analysis');
    if not SeqDataIsValidForLikelihoodAnalysis(ASeqList, aMsg) then
      raise Exception.Create(aMsg);

    {todo -oDan: We need to do a better job at the conversion above, and implement that for all places}
    if AInfo.TransposeData then
    begin
      SetLength(tempSeq, aInfo.MyNoOfSeqs);
      newSeqs := TStringList.Create;
      for i := 1 to aInfo.MyNoOfSites do
      begin
        for j := 0 to aInfo.MyNoOfSeqs - 1 do
        begin
          tempSeq[j + 1] := ASeqList[j][i];
        end;
        newSeqs.Add(tempSeq);
      end;
      AInfo.MyNoOfSites := AInfo.MyNoOfSeqs;
      AInfo.MyNoOfSeqs := newSeqs.Count;
      AInfo.MyOtuNames.Clear;
      for i := 0 to newSeqs.Count - 1 do
        AInfo.MyOtuNames.Add(Format('site-%d', [i + 1]));
      AInfo.MySeqStrings.Clear;
      AInfo.MySeqStrings.AddStrings(newSeqs);
    end;
  finally
    if Assigned(newSeqs) then
      newSeqs.Free;
    if TempSeqList <> nil then
    begin
      for i:=0 to TempSeqList.Count-1 do
      begin
        if TempSeqList[i] <> nil then
            FreeMem(TempSeqList[i]);
        TempSeqList[i] := nil;
      end;
      FreeAndNil(TempSeqList);
    end;
  end;
end;

//---- Important functions
procedure TD_InputSeqData.SetCodeTable(Value: AnsiString);
var
  i: LongInt;
begin
  Assert(Length(Value) = 64, Format('invalid code table length: %d', [Length(Value)]));
  for i:=1 to 64 do
    FCodeTable[i-1] := Value[i];
  if CodonInfo = nil then CodonInfo := TCodonInfo.Create;
  CodonInfo.CodeTable := FCodeTable;
end;

procedure TD_InputSeqData.SortBy(MethodToSortBy : AnsiString);
begin
    //Need to add code here to take care of sorting, must replace other sorting algorithms
end;

// update codon starts
// not affected by the marking of sites like aaa etc.
procedure TD_InputSeqData.UpdateAACodonStarts;
var
  i: LongInt;
  s1,s2,s3: LongInt;
  APChar: PAnsiChar;
  SitesPerPercent: LongInt;

  //we get base-by-base information
  function GetNext1stBase(From: Integer): Integer; // find next codon
  var
    x: Integer;
  begin
    Result := -1;
    x := From;
    while x < NoOfNucSites do
    begin
      if meg1stBase in SiteAttrArray[x] then
      begin
        Result := x;
        Exit;
      end;
      Inc(x);
      if (SitesPerPercent > 0) and ((x mod SitesPerPercent) = 0) then
        if DoProgressCheckCancel(x div SitesPerPercent, 'Updating codon starts') then
          raise EAbort.Create('user cancelled');
    end;
  end;
  //
  function GetNext2ndBase(Base1At: Integer): Integer; // find next codon
  var
    x: Integer;
  begin
    Result := -1;
    x := Base1At+1;
    while x < NoOfNucSites do
    begin
      if meg0thBase in SiteAttrArray[x] then
      begin
        Inc(x);
        if (SitesPerPercent > 0) and ((x mod SitesPerPercent) = 0) then
          if DoProgressCheckCancel(x div SitesPerPercent, 'Updating codon starts') then
            raise EAbort.Create('user cancelled');
        Continue;
      end;
      if not(meg2ndBase in SiteAttrArray[x]) then Exit;
      Result := x;
      if (FDomainMarks[Base1At] = FDomainMarks[x]) then Exit;
      if (FDomainMarks[Base1At] = nil) or (FDomainMarks[x] = nil) then
      begin
        Result := -1;
        Exit;
      end;
      if (FDomainMarks[Base1At].GeneName = FDomainMarks[x].GeneName) then
        Exit
      else
      begin
        Result := -1;
        Exit;
      end;
      if (SitesPerPercent > 0) and ((x mod SitesPerPercent) = 0) then
        if DoProgressCheckCancel(x div SitesPerPercent, 'Updating codon starts') then
          raise EAbort.Create('user cancelled');
    end;
  end;

  function GetNext3rdBase(Base2At: Integer): Integer; // find next codon
  var
    x: Integer;
  begin
    Result := -1;
    x := Base2At+1;
    while x < NoOfNucSites do
    begin
      if meg0thBase in SiteAttrArray[x] then // skip all non-coding area
      begin
        Inc(x);
        if (SitesPerPercent > 0) and ((x mod SitesPerPercent) = 0) then
          if DoProgressCheckCancel(x div SitesPerPercent, 'Updating codon starts') then
            raise EAbort.Create('user cancelled');
        Continue;
      end;
      if not(meg3rdBase in SiteAttrArray[x]) then  // if 3rd base is not found immediately after then exit
        Exit;
      Result := x;
      if (FDomainMarks[Base2At] = FDomainMarks[x]) then
        Exit; // if the domain is not the same
      if (FDomainMarks[Base2At] = nil) or (FDomainMarks[x] = nil) then // if either site has no domain
      begin
        Result := -1;
        Exit;
      end;
      if (FDomainMarks[Base2At].GeneName = FDomainMarks[x].GeneName) then // if gene names are not the same
        Exit
      else
      begin
        Result := -1;
        Exit;
      end;
      if (SitesPerPercent > 0) and ((x mod SitesPerPercent) = 0) then
        if DoProgressCheckCancel(x div SitesPerPercent, 'Updating codon starts') then
          raise EAbort.Create('user cancelled');
    end;
  end;

begin
  if not (svUpdatecodonStarts in JobsPending) then
    Exit;

  try
    AACodonStarts.Clear;
    AACodonPos2.Clear;
    AACodonPos3.Clear;

    SitesPerPercent := NoOfNucSites div 100;
    if SitesPerPercent > 0 then
      if DoProgressCheckCancel(0, 'Identifying codons') then
        raise EAbort.Create('user cancelled');

    s1 := -1;
    while True do
    begin
      s1 := GetNext1stBase(s1+1); // we go just one ahead to make sure we don't miss anything
      if s1 < 0 then Break;

      s2 := GetNext2ndBase(s1);
      if s2 < 0 then Continue;

      s3 := GetNext3rdBase(s2);
      if s3 < 0 then Continue;

      // it will fall here only if a full codon is found.
      AACodonStarts.Add(s1);
      AACodonPos2.Add(s2);
      AACodonPos3.Add(s3);
      // CurCodonIndex := AACodonStart.Count-1;

      // if it is the last codon of a domain (belonging to the same gene),
      // then it can be a stop codon
      // basically if the FDomainMarks[a given s1] <> FDomainMarks[the previous s1]
      // then we remove the s1 based codon if it is a start codon and we
      // remove the s1-1 based codon if it is a stop codon
      //if FDomainMarks[AACodonStarts[CurCodonIndex]] <> FDomainMarks[AACodonStarts[CurCodonIndex-1] then
      //      begin
      //      end;
      //
    end;

    // also prepares memory
    for i:=0 to FNoOfTaxa-1 do
    begin
      if (FOtuInfos <> nil) and (FOtuInfos[i].RsvData <> nil) then
      begin
        FreeMem(FOtuInfos[i].RsvData);
        FOtuInfos[i].RsvData := nil;
      end;
      APChar := AllocMem((AACodonStarts.Count+1)*sizeOf(AnsiChar)); // +1 so for 0 case we have some memory
      FOtuInfos[i].RsvData := APChar;
    end;
    JobsPending := JobsPending - [svUpdateCodonStarts];
  finally
    if Assigned(PleaseWait) and PleaseWait.Visible then
      PleaseWait.Hide;
  end;
end;


// Amino variability done for all sites
procedure TD_InputSeqData.UpdateAminoVar;
var
  MaxSite, i, j, Sum, Kinds: LongInt;
  Freq: array['A'..'Z'] of LongInt;
  x: AnsiChar;
  ASeq: PAnsiChar;
  SitesPerPercent, SitesDone: LongInt;
begin
  if not (svUpdateAminoVar in JobsPending) then
    Exit;

  if (not FIsNuc) then
    MaxSite := FNoOfSites
  else
    MaxSite := AACodonStarts.Count;

  try
    JobsPending := JobsPending - [svUpdateAminoVar];
    SitesPerPercent := MaxSite*FNoOfTaxa div 100;
    SitesDone := 0;
    if SitesPerPercent > 0 then
      if DoProgressCheckCancel(0, 'Amino acid variability') then
        raise EAbort.Create('user cancelled');

    for i:= 0 to MaxSite-1 do
    begin
      SiteAttrArray[i] := SiteAttrArray[i] - [megAminoConst, megAminoVar, megAminoParsimInfo, megAminoSingleton,
                                              megAminoGap, megAminoMissing];

      for x:='A' to 'Z' do
        Freq[x] := 0;

      for j:=0 to FNoOfTaxa-1 do
      begin
        Inc(SitesDone);
        if (SitesPerPercent > 0) and ((SitesDone mod SitesPerPercent) = 0) then
          if DoProgressCheckCancel(SitesDone div SitesPerPercent, 'Amino acid variability') then
            raise EAbort.Create('user cancelled');

        if not FOtuInfos[j].IsUsed then
          Continue;

        if not FIsNuc then ASeq := FOtuInfos[j].Data
        else               ASeq := FOtuInfos[j].RsvData;
        x := UpCase(ASeq[i]);
        if not IsUnambiguousAminoAcid(x) then
        begin
          if x = FGapSym then SiteAttrArray[i] := SiteAttrArray[i] + [megAminoGap]
                         else SiteAttrArray[i] := SiteAttrArray[i] + [megAminoMissing];
          continue;
        end;
        Inc(Freq[x]); // increments the frequency of the given aa
      end;

      Sum := 0;
      for x:='A' to 'Z' do
        Sum := Sum + Freq[x];

      if Sum <= 1 then
        Continue;

      Kinds := 0;
      for x:='A' to 'Z' do
        if Freq[x] <> 0 then  Inc(Kinds);

      // if we have atleast two kinds of states
      if Kinds > 1 then
      begin
        SiteAttrArray[i] := SiteAttrArray[i] + [megAminoVar];
        Kinds := 0;
        for x:='A' to 'Z' do
          if Freq[x] > 1 then  Inc(Kinds);
        if Kinds > 1 then // i.e., >= 2 bases with freq 2 or more
           SiteAttrArray[i] := SiteAttrArray[i] + [megAminoParsimInfo]
        else if (Sum > 2) then
              SiteAttrArray[i] := SiteAttrArray[i] + [megAminoSingleton];
      end
      else
          SiteAttrArray[i] := SiteAttrArray[i] + [megAminoConst];
    end;
  finally
    if Assigned(PleaseWait) and PleaseWait.Visible then
      PleaseWait.Hide;
  end;
end;

//----- At the time of form activation, we need this information
procedure TD_InputSeqData.UpdateCodonSitesType;
var
  i, SitesPerPercent: LongInt;
  updateTime: TDateTime;
begin
  if not(svUpdateCodonSiteType in JobsPending) then
    Exit;
  updateTime := Now;
  try
    SitesPerPercent := FNoOfSites div 100;
    if (SitesPerPercent > 0) and (not Assigned(ARP)) then
      if DoProgressCheckCancel(0, 'Site/codon position types') then
        raise EAbort.Create('user cancelled');

    if not FIsNuc then // it is aa data then just 0th base
    begin
      for i:=0 to FNoOfSites-1 do
      begin
        SiteAttrArray[i] := SiteAttrArray[i] + [meg0thBase];
        if (SitesPerPercent > 0) and ((i mod SitesPerPercent) = 0) then
          if DoProgressCheckCancel(i div SitesPerPercent, 'Site/codon position types') then
            raise EAbort.Create('user cancelled');
      end;
    end
    else
      for i:=0 to NoOfNucSites-1 do
      begin
        SiteAttrArray[i] := SiteAttrArray[i] - [meg0thBase, meg1stBase, meg2ndBase, meg3rdBase];
        if FDomainMarks[i] <> nil then
          SiteAttrArray[i] := SiteAttrArray[i] + FDomainMarks[i].CodonSiteAttr[i]
        else
          SiteAttrArray[i] := SiteAttrArray[i] + [meg0thBase];
        if SecondsBetween(Now, updateTime) > 1 then
        begin
          UpdateTime := Now;
          if DoProgressCheckCancel(i div SitesPerPercent, 'Site/codon position types') then
            raise EAbort.Create('user cancelled');
          {$IFDEF VISUAL_BUILD}Application.ProcessMessages;{$ENDIF}
        end;
      end;
    JobsPending := JobsPending - [svUpdateCodonSiteType];
  finally
    if Assigned(PleaseWait) and PleaseWait.Visible then
      PleaseWait.Hide;
  end;
end;

//-------------------------------------------------------------------
// for all coding sites
procedure TD_InputSeqData.UpdateDegeneracy;
var
  CodonMap: TCodonMap;
  DoPos: array [0..2] of Boolean;
  Codon: array [0..2] of AnsiChar;
  FoldStates, m, i, k, z: LongInt;
  s: array[0..2] of LongInt;
  ASeq: PAnsiChar;
  SitesPerPercent: LongInt;
  SitesDone : LongInt;
begin
  if not (svUpdateDegeneracy in JobsPending) then
    Exit;

  try
    if svUpdateCodonStarts in JobsPending then
      UpdateAACodonStarts;

    JobsPending := JobsPending - [svUpdateDegeneracy];
    JobsPending := JobsPending + [svUpdateBasicCounts];
    for i:=0 to NoOfNucSites-1 do
      SiteAttrArray[i] := SiteAttrArray[i] - [meg0Fold, meg2Fold, meg4Fold,
                                              megCod0Fold, megCod2Fold, megCod4Fold];

    SitesPerPercent := (AACodonStarts.Count*FNoOfTaxa) div 100;
    if SitesPerPercent > 133 then  // Don't show PleaseWait for small amounts of data (acually slows down UI response).
      if DoProgressCheckCancel(0, 'Codon position degeneracy') then
        raise EAbort.Create('user cancelled');
    SitesDone := 0;

    for m:=0 to AACodonStarts.Count-1 do
    begin
      s[0] := AACodonStarts[m];
      s[1] := AACodonPos2[m];
      s[2] := AACodonPos3[m];

      DoPos[0] := True;
      DoPos[1] := True;
      DoPos[2] := True;

      for k:=0 to FNoOfTaxa-1 do
      begin
        Inc(SitesDone);
        if (SitesPerPercent > 0) and ((SitesDone mod SitesPerPercent) = 0) then
          if DoProgressCheckCancel(SitesDone div SitesPerPercent, 'Codon position degeneracy') then
            raise EAbort.Create('user cancelled');

        if not FOtuInfos[k].IsUsed then
          Continue;
        ASeq := FOtuInfos[k].Data;  // independent of the display mode

        Codon[0] := Upcase(ASeq[s[0]]);
        Codon[1] := Upcase(ASeq[s[1]]);
        Codon[2] := Upcase(ASeq[s[2]]);
        CodonMap := CodonInfo.MakeCodonBitMap(Codon);
        if CodonMap > #63 then // neither exit yet nor go on
          continue;

        for z:=0 to 2 do  // three positions
          if DoPos[z] then
          begin
            case CodonInfo.Redundancy[CodonMap, z] of
              0: SiteAttrArray[s[z]] := SiteAttrArray[s[z]] + [megCod0Fold];
              2: SiteAttrArray[s[z]] := SiteAttrArray[s[z]] + [megCod2Fold];
              4: SiteAttrArray[s[z]] := SiteAttrArray[s[z]] + [megCod4Fold];
            end;
          end;

        for z:=0 to 2 do
        begin
          if not DoPos[z] then Continue;
          FoldStates := 0;
          // if multiple states found then no point
          if megCod0Fold in SiteAttrArray[s[z]] then  Inc(FoldStates);
          if megCod2Fold in SiteAttrArray[s[z]] then  Inc(FoldStates);
          if megCod4Fold in SiteAttrArray[s[z]] then  Inc(FoldStates);
          DoPos[z] := FoldStates < 2;  // if States are 0 or 1 then we still have possibility
          if not DoPos[z] then
            SiteAttrArray[s[z]] := SiteAttrArray[s[z]] - [megCod0Fold, megCod2Fold, megCod4Fold];
        end;
        if not (DoPos[0] or DoPos[1] or DoPos[2]) then // already no point doing any further
          Break;
      end; // end of for k over all sequences

    end;  // end of for sites
  finally
    if Assigned(PleaseWait) and PleaseWait.Visible then
      PleaseWait.Hide;
  end;
end;


procedure TD_InputSeqData.UpdateNucVar;
var
  FreqA, FreqT, FreqC, FreqG: LongInt;
  Sum, Kinds, i,j: LongInt;
  x: AnsiChar;
  ASeq: PAnsiChar;
  SitesPerPercent, SitesDone: LongInt;
begin
  if not (svUpdateNucVar in JobsPending) then
    Exit;

  try
    JobsPending := JobsPending - [svUpdateNucVar];
    SitesPerPercent := FNoOfSites*FNoOfTaxa div 100;
    if SitesPerPercent > 400 then // Don't show PleaseWait for small amounts of data (acually slows down UI response).
      if DoProgressCheckCancel(0, 'Updating site attributes') then
        raise EAbort.Create('user cancelled');
    SitesDone := 0;

    // make all site status as invariable and uninformative
    for i:= 0 to FNoOfSites-1 do
    begin
      SiteAttrArray[i] := SiteAttrArray[i] - [megNucConst, megNucVar, megNucParsimInfo, megNucSingleton,
                                              megNucGap, megNucMissing];

      FreqA := 0;
      FreqT := 0;
      FreqC := 0;
      FreqG := 0;
      for j:=0 to FNoOfTaxa-1 do
      begin
        Inc(SitesDone);
        if (SitesPerPercent > 0) and ((SitesDone mod SitesPerPercent) = 0) then
          if DoProgressCheckCancel(SitesDone div SitesPerPercent, 'Updating site attributes') then
            raise EAbort.Create('user cancelled');
        if not FOtuInfos[j].IsUsed then
          Continue;
        ASeq := FOtuInfos[j].Data;  // independent of the display mode
        x := Upcase(ASeq[i]);
        if not IsUnambiguousNucleotide(x) then
        begin
          if x = FGapSym then SiteAttrArray[i] := SiteAttrArray[i] + [megNucGap]
                         else SiteAttrArray[i] := SiteAttrArray[i] + [megNucMissing];
          continue;
        end;

        case x of
          'A'     : Inc(FreqA);
          'T', 'U': Inc(FreqT);
          'C'     : Inc(FreqC);
          'G'     : Inc(FreqG);
        end;
      end;
      sum := (FreqA+FreqT+FreqC+FreqG);

      if sum < 2 then
        continue;

      Kinds := 0;
      if FreqA <>  0 then  Inc(Kinds);
      if FreqT <>  0 then  Inc(Kinds);
      if FreqC <>  0 then  Inc(Kinds);
      if FreqG <>  0 then  Inc(Kinds);

      // if we have atleast two kinds of states
      if Kinds > 1 then // so at least two types of states are found
      begin
        SiteAttrArray[i] := SiteAttrArray[i] + [megNucVar];
        // find if it is a parsimony informative site
        Kinds := 0;
        if FreqA >=2 then  Inc(Kinds);
        if FreqT >=2 then  Inc(Kinds);
        if FreqC >=2 then  Inc(Kinds);
        if FreqG >=2 then  Inc(Kinds);
        if Kinds >=2 then // i.e., >= 2 bases with a min. freq of 2
           SiteAttrArray[i] := SiteAttrArray[i] + [megNucParsimInfo]
        else if sum >= 3 then // at least three valid states
           SiteAttrArray[i] := SiteAttrArray[i] + [megNucSingleton]
      end
      else
        SiteAttrArray[i] := SiteAttrArray[i] + [megNucConst];
    end;
  finally
    if Assigned(PleaseWait) and PleaseWait.Visible then
      PleaseWait.Hide;
  end;
end;

procedure TD_InputSeqData.UpdateLabelledSites;
var
  i: LongInt;
  SitesPerPercent: LongInt;
begin
  SitesPerPercent := FNoOfSites div 100;
  if SitesPerPercent > 400 then
    if DoProgressCheckCancel(0, 'Updating site labels') then
      raise EAbort.Create('user cancelled');

  for i:= 0 to FNoOfSites - 1 do
  begin
    if (FDomainMarks.HasLabelOverride(i)) or (FDomainMarks.SiteMarks[i] <> ' ') then
    begin
      if FIsAmino or (FIsCoding and VS_SeqDataExplorer.IsTranslated) then
        SiteAttrArray[i] := SiteAttrArray[i] + [megAminoLabelled, megLabelled]
      else
        SiteAttrArray[i] := SiteAttrArray[i] + [megNucLabelled, megLabelled];
    end
    else
    begin
      if FIsAmino or (FIsCoding and VS_SeqDataExplorer.IsTranslated) then
        SiteAttrArray[i] := SiteAttrArray[i] - [megAminoLabelled, megLabelled]
      else
        SiteAttrArray[i] := SiteAttrArray[i] - [megNucLabelled, megLabelled];
    end;
    if (SitesPerPercent > 0) and ((i mod SitesPerPercent) = 0) then
      if DoProgressCheckCancel(i div SitesPerPercent, 'Updating site labels') then
        raise EAbort.Create('user cancelled');
  end;
end;

procedure TD_InputSeqData.UpdateSpecial;
var
  numAmbiguous: LongInt;
  i,j: LongInt;
  x: AnsiChar;
  ASeq: PAnsiChar;
  SitesPerPercent, SitesDone: LongInt;
  NextSiteChar: AnsiChar;
  CpGNum, TpGNum, CpANum: Integer;
  NumNeeded: Double;
  HighlightNextSite: Boolean = True;
begin
  if not (svUpdateSpecial in JobsPending) then
    Exit;
  try
    JobsPending := JobsPending - [svUpdateSpecial];
    SitesPerPercent := FNoOfSites*FNoOfTaxa div 100;
    if SitesPerPercent > 0 then
      if DoProgressCheckCancel(0, 'Partial deletion') then
        raise EAbort.Create('user cancelled');
    SitesDone := 0;
    if VS_SeqDataExplorer.CoveragePerc = 0 then VS_SeqDataExplorer.CoveragePerc := 100;
    // make all site status as invariable and uninformative
    for i:= 0 to FNoOfSites-1 do
    begin
      SiteAttrArray[i] := SiteAttrArray[i] - [megCoverage, megCpG]; // list all special here
      if HighlightNextSite then
        SiteAttrArray[i] := SiteAttrArray[i] + [megCpG];
      HighlightNextSite := false;
      numAmbiguous := 0;
      if VS_SeqDataExplorer.CurAttrDisp = megCpG then
        if megCpG in SiteAttrArray[i] then Continue;
      CpGNum := 0;
      TpGNum := 0;
      CpANum := 0;
      if (VS_SeqDataExplorer.CurAttrDisp = megCoverage) or (VS_SeqDataExplorer.CurAttrDisp = megCpG) then
      begin
        for j:=0 to FNoOfTaxa-1 do
        begin
          Inc(SitesDone);
          if (SitesPerPercent > 0) and ((SitesDone mod SitesPerPercent) = 0) then
            if DoProgressCheckCancel(SitesDone div SitesPerPercent, 'Partial deletion') then
              raise EAbort.Create('user cancelled');
          if not FOtuInfos[j].IsUsed then
            Continue;
          ASeq := FOtuInfos[j].Data;  // independent of the display mode
          x := Upcase(ASeq[i]);

          Case VS_SeqDataExplorer.CurAttrDisp of
            megCoverage:
              begin
                if (IsNuc and (not IsUnambiguousNucleotide(x))) or (IsAmino and (not IsUnambiguousAminoAcid(x))) then
                  inc(numAmbiguous);
              end;
            megCpG:
              begin
                NextSiteChar := Upcase(ASeq[i+1]);
                if X = 'C' then
                  if NextSiteChar = 'G' then
                    Inc(CpGNum);
                if (X = 'T') OR (X = 'U') then
                  if NextSiteChar = 'G' then
                    Inc(TpGNum);
                if X = 'C' then
                  if NextSiteChar = 'A' then
                    Inc(CpANum);
              end;
            else
              begin
                //ShowMessage('Special update called w/o any relevant CurAttrDisp eg. megCoverage');
              end;
          end;
        end;

        Case VS_SeqDataExplorer.CurAttrDisp of
          megCpG:
          begin
            // Remember to ask for and set CoveragePerc for what percentage of the chars in a site must be CpG/TpG/CpA
            NumNeeded := ((FNoOfTaxa * VS_SeqDataExplorer.CoveragePerc) / 100);

            if CpGNum >= NumNeeded then
            begin
              SiteAttrArray[i] := SiteAttrArray[i] + [megCpG];
              HighlightNextSite := true;
            end
            else if TpGNum >= NumNeeded then
            begin
              SiteAttrArray[i] := SiteAttrArray[i] + [megCpG];
              HighlightNextSite := true;
            end
            else if CpANum >= NumNeeded then
            begin
              SiteAttrArray[i] := SiteAttrArray[i] + [megCpG];
              HighlightNextSite := true;
            end;
          end;
          megCoverage:
          begin
            if numAmbiguous <= ((FNoOfTaxa * (100 - VS_SeqDataExplorer.CoveragePerc))/100) then
              SiteAttrArray[i] := SiteAttrArray[i] + [megCoverage];
          end;
        end;
      end
      else
      begin
        inc(SitesDone, FNoOfTaxa);
      end;
    end;
  finally
    if Assigned(PleaseWait) and PleaseWait.Visible then
      PleaseWait.Hide;
  end;
end;

procedure TD_InputSeqData.UpdateSiteAttrArray(forceUpdates: Boolean = False);
var
  i,MaxSite: LongInt;
  skip: Boolean = False;
begin
  try
    FForcingAttributeUpdates := forceUpdates;
    if VS_SeqDataExplorer.CurAttrDisp = megLabelled then
      UpdateLabelledSites;
    if NeedsComputed(megLabelled) then
    begin
      if FIsAmino or (FIsCoding and VS_SeqDataExplorer.IsTranslated) then
        FNoOfAttrSites[megLabelled] := CountAttrSites(megAminoLabelled)
      else
        FNoOfAttrSites[megLabelled] := CountAttrSites(megNucLabelled);
    end;
    if VS_SeqDataExplorer.CurAttrDisp <> megLabelled then
    begin
      // nuc markings
      if (svUpdateNucVar in JobsPending) and FIsNuc and
         (not (FIsCoding and VS_SeqDataExplorer.IsTranslated)) then
      begin
        if FComputedStats = [] then
          UpdateNucVar;
        if NeedsComputed(megConst) then
          FNoOfAttrSites[megNucConst] := CountAttrSites(megNucConst);
        if NeedsComputed(megVar) then
          FNoOfAttrSites[megNucVar]  := CountAttrSites(megNucVar);
        if NeedsComputed(megParsimInfo) then
          FNoOfAttrSites[megNucParsimInfo] := CountAttrSites(megNucParsimInfo);
        if NeedsComputed(megSingleton) then
          FNoOfAttrSites[megNucSingleton]  := CountAttrSites(megNucSingleton);
        JobsPending := JobsPending - [svUpdateNucVar];
      end;

      if (svUpdateSpecial in JobsPending) then
      begin
        UpdateSpecial;
        FNoOfAttrSites[VS_SeqDataExplorer.CurAttrDisp] := CountAttrSites(VS_SeqDataExplorer.CurAttrDisp);
        FNoOfAttrSites[megCpG] := CountAttrSites(megCpG);
      end;

      // amino acid markings
      if (svUpdateAminoVar in JobsPending) and
         ((not FIsNuc) or (FIsNuc and FIsCoding and VS_SeqDataExplorer.IsTranslated)) then
      begin
        if FComputedStats = [] then
          UpdateAminoVar;
        if NeedsComputed(megConst) then
          FNoOfAttrSites[megAminoConst] := CountAttrSites(megAminoConst);
        if NeedsComputed(megVar) then
          FNoOfAttrSites[megAminoVar] := CountAttrSites(megAminoVar);
        if NeedsComputed(megParsimInfo) then
          FNoOfAttrSites[megAminoParsimInfo] := CountAttrSites(megAminoParsimInfo);
        if NeedsComputed(megSingleton) then
          FNoOfAttrSites[megAminoSingleton] := CountAttrSites(megAminoSingleton);
        JobsPending := JobsPending - [svUpdateAminoVar];
      end;

      // degeneracy
      if (svUpdateDegeneracy in JobsPending) and (FIsNuc and FIsCoding and (not VS_SeqDataExplorer.IsTranslated)) then
      begin
        if (NeedsComputed(meg0Fold)) or (NeedsComputed(meg2Fold)) or (NeedsComputed(meg4Fold)) then
        begin
          UpdateDegeneracy;
          JobsPending := JobsPending - [svUpdateDegeneracy];
        end;
      end;

      if svUpdateBasicCounts in JobsPending then
      begin
        if FIsNuc and (not (FIsCoding and VS_SeqDataExplorer.IsTranslated)) then
        begin
          NoOfSitesUsed := 0;
          for i:= 0 to FNoOfSites-1 do
          begin
             SiteAttrArray[i] := SiteAttrArray[i] - [megConst, megVar, megParsimInfo, megSingleton, megGap, megMissing];
             if SiteUsed[i] then
             begin
               Inc(NoOfSitesUsed);
               if megNucConst      in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [megConst];
               if megNucVar        in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [megVar];
               if megNucParsimInfo in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [megParsimInfo];
               if megNucSingleton  in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [megSingleton];
               if megNucGap        in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [megGap];
               if megNucMissing    in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [megMissing];
             end;
          end;
          if NeedsComputed(megConst) then
            FNoOfAttrSites[megConst] := CountAttrSites(megConst);
          if NeedsComputed(megVar) then
            FNoOfAttrSites[megVar] := CountAttrSites(megVar);
          if NeedsComputed(megParsimInfo) then
            FNoOfAttrSites[megParsimInfo] := CountAttrSites(megParsimInfo);
          if NeedsComputed(megSingleton) then
            FNoOfAttrSites[megSingleton] := CountAttrSites(megSingleton);
        end;

        if FIsNuc and FIsCoding then
        begin
          NoOfCodingSitesUsed:=0;
          for i:= 0 to FNoOfSites-1 do
          begin
            skip := False;
             SiteAttrArray[i] := SiteAttrArray[i] - [meg0Fold, meg2Fold, meg4Fold];
             if meg0thBase in SiteAttrArray[i] then
               skip := True;
             if not SiteUsed[i] then
               skip := True;
             if not skip then
             begin
               Inc(NoOfCodingSitesUsed);
               if megCod0Fold  in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [meg0Fold];
               if megCod2Fold  in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [meg2Fold];
               if megCod4Fold  in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [meg4Fold];
               if megNucGap    in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [megGap];
               if megNucMissing in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [megMissing];
             end;
          end;
          if NeedsComputed(meg0Fold) then
            FNoOfAttrSites[meg0Fold] := CountAttrSites(meg0Fold);
          if NeedsComputed(meg2Fold) then
            FNoOfAttrSites[meg2Fold] := CountAttrSites(meg2Fold);
          if NeedsComputed(meg4Fold) then
            FNoOfAttrSites[meg4Fold] := CountAttrSites(meg4Fold);
        end;

        if (not FIsNuc) or (FIsCoding and VS_SeqDataExplorer.IsTranslated) then
        begin
          if not FIsNuc then
            MaxSite := FNoOFSites
          else
            MaxSite := AACodonStarts.Count;
          NoOfSitesUsed := 0;
          for i:= 0 to MaxSite-1 do
          begin
            SiteAttrArray[i] := SiteAttrArray[i] - [megConst, megVar, megSingleton, megParsimInfo];
            if SiteUsed[i] then
            begin
              Inc(NoOfSitesUsed);
              if megAminoConst      in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [megConst];
              if megAminoVar        in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [megVar];
              if megAminoParsimInfo in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [megParsimInfo];
              if megAminoSingleton  in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [megSingleton];
              if megAminoGap        in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [megGap];
              if megAminoMissing    in SiteAttrArray[i] then  SiteAttrArray[i] := SiteAttrArray[i] + [megMissing];
            end;
          end;
          if NeedsComputed(megConst) then
            FNoOfAttrSites[megConst] := CountAttrSites(megConst);
          if NeedsComputed(megVar) then
            FNoOfAttrSites[megVar] := CountAttrSites(megVar);
          if NeedsComputed(megParsimInfo) then
            FNoOfAttrSites[megParsimInfo] := CountAttrSites(megParsimInfo);
          if NeedsComputed(megSingleton) then
            FNoOfAttrSites[megSingleton] := CountAttrSites(megSingleton);
        end;
        JobsPending := JobsPending - [svUpdateBasicCounts];
      end;
      if FForcingAttributeUpdates then
      begin
        Include(FComputedStats, megConst);
        Include(FComputedStats, megVar);
        Include(FComputedStats, megParsimInfo);
        Include(FComputedStats, megSingleton);
        Include(FComputedStats, meg0Fold);
        Include(FComputedStats, meg2Fold);
        Include(FComputedStats, meg4Fold);
      end
      else
        Include(FComputedStats, VS_SeqDataExplorer.CurAttrDisp);

    end;
  finally
    FForcingAttributeUpdates := False;
    HideProgressDlg;
  end;
end;

function TD_InputSeqData.GetSiteUsed(Index: Integer): Boolean;
begin
  if FIsNuc and FIsCoding and VS_SeqDataExplorer.IsTranslated then
    Result := FDomainMarks.IsSiteUsed[AACodonStarts[Index]] and
              FDomainMarks.IsSiteUsed[AACodonPos2[Index]  ] and
              FDomainMarks.IsSiteUsed[AACodonPos3[Index]  ]
  else
    Result := FDomainMarks.IsSiteUsed[Index];
end;

procedure TD_InputSeqData.UpdateDispTaxaList;
var
  i: LongInt;
begin
  if (VS_SeqDataExplorer.DispSelTaxaItem) or (VS_SeqDataExplorer.DispResultsOnlyItem) then
    VS_SeqDataExplorer.NoOfSelTaxa := FOtuInfos.ConstructSelTaxaList(DispTaxaList)
  else
  begin
    DispTaxaList.Clear;
    for i:=0 to FNoOfTaxa-1 do
        DispTaxaList.Add(i);
  end;
end;

//
procedure TD_InputSeqData.StatAminoAcidComposition(var ResultOutput: TStringList; var AmiXls: TExcelWrite; const SaveTo: String);
var
  FreqAA:  CharIndexDoubleArray;
  AllSeq:  CharIndexDoubleArray;
  AllDoms: CharIndexDoubleArray;

  TotalSeqs, i, SeqI, SiteI, DomainI: LongInt;
  x: AnsiChar;
  CurDomainIndex, CurSeqIndex: LongInt;

  CurDomain: TDomainInfo = nil;
  AFreq    : TLocusPopFreqInfo = nil;
  TempArray: PAnsiChar = nil;
  CurStr   : AnsiString;      //

  MyDomains: TList;     // List of domain pointers
  FreqInfo:  TList;     // List of Lists of pops Allelefreq arrays
  DoubleArr: PArrayOfDouble;
  Z, MyMaxTaxaNameLen: LongInt;
  SitesDone, SitesPerPercent: Longint;

  // Make Percent outof the Input Array
  procedure WritePercent(Input: CharIndexDoubleArray);
  var
    x: AnsiChar;
    Percent: Double;
  begin
    Input['Z'] := 0;
    for x := 'A' to 'Y' do
      if IsUnambiguousAminoAcid(x) then
         Input['Z'] := Input['Z']+ Input[x];
    for x := 'A' to 'Y' do
    begin
      if not IsUnambiguousAminoAcid(x) then
        Continue;
      Percent := 0;
      if Input['Z'] > 0 then
        Percent := Input[x]/Input['Z']*100;
      CurStr := CurStr + FloatToStrWidth(Percent, 5, 2)+' ';
    end;
    if Ceil(Input['Z']) <> Floor(Input['Z']) then
      CurStr := CurStr + FloatToStrWidth(Input['Z'],Z+2,1)+' '
    else
      CurStr := CurStr + IntToStrWidth(Floor(Input['Z']), Z)+' ';
  end;
   procedure WriteExcelPercent(Input: CharIndexDoubleArray);
  var
    x: AnsiChar;
    Percent: Double;
  begin
    Input['Z'] := 0;
    for x := 'A' to 'Y' do
      if IsUnambiguousAminoAcid(x) then
         Input['Z'] := Input['Z']+ Input[x];
    for x := 'A' to 'Y' do
    begin
      if not IsUnambiguousAminoAcid(x) then
        Continue;
      Percent := 0;
      if Input['Z'] > 0 then
        Percent := Input[x]/Input['Z']*100;
      AmiXls.Add(Percent);

    end;
    if Ceil(Input['Z']) <> Floor(Input['Z']) then
      AmiXls.Add(Input['Z'])
    else
      AmiXls.Add(Floor(Input['Z']));
  end;

begin
  CalculateMaxTaxaNameLen;
  MyMaxTaxaNameLen := MaxTaxaNameLen;
  if MyMaxTaxaNameLen < 7 then
    MyMaxTaxaNameLen := 7;
  Z := Length(IntToStr(FNoOfSites)) + 1;
  if Z < 5 then Z := 5;
  UpdateSiteAttrArray;
  try try
    // first get total seqs
    TotalSeqs := 0;
    for SeqI:=0 to FNoOfTaxa-1 do
      if FOtuInfos[SeqI].IsUsed then Inc(TotalSeqs);

    SitesDone := 0;
    SitesPerPercent := FNoOfSites*FNoOfTaxa div 100;
    if SitesPerPercent > 0 then
      if DoProgressCheckCancel(0, 'Computing amino acid frequencies...') then
        raise EAbort.Create('user cancelled');

    // create my domains list and the list to hold freqinfo
    MyDomains := TList.Create;
    FreqInfo  := TList.Create;

    // now we have either direct aa or translated aa
    for SiteI:=0 to FNoOfSites-1 do  // this is the
    begin
      // so we first find the index in the Myfreqs
      CurDomain := DomainMark[SiteI];
      if (CurDomain <> nil) and (not CurDomain.IsUsed) then
      begin
         SitesDone := SitesDone + FNoOfTaxa;
         Continue;
      end;

      // check if the user wants only highlighted sites.
      if (VS_SeqDataExplorer.CurAttrDisp <> megNone) and (VS_SeqDataExplorer.StatUseOnlyMarkedSitesItem) then
        if not (VS_SeqDataExplorer.CurAttrDisp in D_InputSeqData.SiteAttrArray[SiteI]) then
      begin
         SitesDone := SitesDone + FNoOfTaxa;
         Continue;
      end;

      //Now we start
      CurDomainIndex := MyDomains.IndexOf(CurDomain);
      if CurDomainIndex < 0 then
      begin
        MyDomains.Add(CurDomain);
        CurDomainIndex := MyDomains.Count-1;
        for SeqI:=0 to TotalSeqs-1 do
        begin
          AFreq := TLocusPopFreqInfo.Create;
          FreqInfo.Add(AFreq); // add first to make sure memory will be freed in case of error
          AFreq.NoOfAlleles :=  26; // just for simplicity
          GetMem(DoubleArr, AFreq.NoOfAlleles*Sizeof(Double));
          for i:=0 to AFreq.NoOfAlleles-1 do
            DoubleArr[i] := 0;
          AFreq.AlleleFreq := DoubleArr;
          AFreq.NoOfIndv := 0;
        end
      end;

      CurSeqIndex := -1;
      for SeqI:=0 to FNoOfTaxa-1 do
      begin
        Inc(SitesDone);
        if (SitesPerPercent > 0) and ((SitesDone mod SitesPerPercent) = 0) then
          if DoProgressCheckCancel(SitesDone div SitesPerPercent, 'Computing amino acid frequencies...') then
            raise EAbort.Create('user cancelled');

        if not FOtuInfos[SeqI].IsUsed then
          Continue;
        Inc(CurSeqIndex);
        AFreq     := FreqInfo[TotalSeqs*CurDomainIndex + CurSeqIndex];
        DoubleArr := AFreq.AlleleFreq;
        if FIsNuc and VS_SeqDataExplorer.IsTranslated then
          TempArray := FOtuInfos[SeqI].RsvData
        else
          //TempArray := FOtuInfos[CurSeqIndex].Data;
          TempArray := FOtuInfos[SeqI].Data;
        x := Upcase(TempArray[SiteI]);
        if IsUnambiguousAminoAcid(x) then
          DoubleArr[Ord(x)-Ord('A')] := DoubleArr[Ord(x)-Ord('A')]+1;
      end;
    end; // for all sites

    // reset Freq for overage over all domains
    for x:='A' to 'Z' do  AllDoms[x] := 0;

    SitesPerPercent := MyDomains.Count*FNoOfTaxa div 100;
    if SitesPerPercent > 0 then
      if DoProgressCheckCancel(0, 'Computing amino acid frequencies...') then
        raise EAbort.Create('user cancelled');
    SitesDone := 0;

    ResultOutput.Add(rtfHeaderStr);
	{$IFDEF VISUAL_BUILD}
    ResultOutput.Add(rtfStartStr+rtfPlainStr+'Data Filename: '+ExtractFileName(MegaForm.DataFileName));
    AmiXls.WriteKeypair('Data Filename:', ExtractFileName(MegaForm.DataFileName), 1);
    ResultOutput.Add(rtfParaStr +'Data Title: '+MegaForm.DataTitle);
    AmiXls.WriteKeypair('Data Title:', MegaForm.DataTitle, 1);
	{$ELSE}
	ResultOutput.Add(rtfStartStr+rtfPlainStr+'Data Filename: '+ExtractFileName(D_MegaMain.DataFileName));
    AmiXls.WriteKeypair('Data Filename:', ExtractFileName(D_MegaMain.DataFileName), 1);
    ResultOutput.Add(rtfParaStr +'Data Title: '+D_MegaMain.DataTitle);
    AmiXls.WriteKeypair('Data Title:', D_MegaMain.DataTitle, 1);
	{$ENDIF}
    ResultOutput.Add(rtfParaStr+rtfBoldStr+'Amino acid Frequencies'+rtfPlainStr);
    AmiXls.WriteLine('Amino acid Frequencies', 1);
    if (VS_SeqDataExplorer.CurAttrDisp <> megNone) and (VS_SeqDataExplorer.StatUseOnlyMarkedSitesItem) then
    begin
      ResultOutput.Add(rtfParaStr+'Sites Used: '+VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites]);
      AmiXls.WriteKeypair('Sites Used:', VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites], 1);
    end
    else
    begin
      ResultOutput.Add(rtfParaStr+'Sites Used: '+'All selected');
      AmiXls.WriteKeypair('Sites Used:', 'All selected', 1);
    end;
    ResultOutput.Add(rtfParaStr+'All frequencies are given in percent.');
    AmiXls.WriteLine('All frequencies are given in percent.');
    //===
    for DomainI :=0 to MyDomains.Count-1 do
    begin
      // write the domain name
      CurDomain := MyDomains[DomainI];
      if CurDomain = nil then
      begin
        CurStr := 'Independent Sites';
        AmiXls.Add('Independent Sites');
      end
      else
      begin
        CurStr := 'Domain: '+CurDomain.Name;
        AmiXls.Add('Domain: '+CurDomain.Name);
        if length(CurDomain.GeneName)  > 0 then
        begin
          CurStr := CurStr + ' (Gene: '+CurDomain.GeneName+')';
          AmiXls.Add(' (Gene: '+CurDomain.GeneName+')');
        end;
      end;
      ResultOutput.Add(rtfParaStr+rtfBoldStr+CurStr+rtfPlainStr);
      AmiXls.WriteLine();

      // now write the column headers for each domain separately
      CurStr  := StrToStrWidth('', MyMaxTaxaNameLen)+'  ';
      for x:='A' to 'Z' do
        if IsUnambiguousAminoAcid(x) then
        begin
          CurStr := CurStr + StrToStrWidth(GetThreeLetterCode(x), 5)+' ';
          AmiXls.Add(GetThreeLetterCode(x));
        end;
      CurStr := CurStr+StrToStrWidth('Total',Z);
      AmiXls.Add('Total');
      ResultOutput.Add(rtfParaStr+rtfBoldStr+CurStr+rtfPlainStr);
      AmiXls.WriteLine(0, 'B');
      // reset Freq for all Seqs
      for x:='A' to 'Z' do AllSeq[x] := 0;

      CurSeqIndex := -1;
      for SeqI := 0 to FNoOfTaxa-1 do
      begin
        Inc(SitesDone);
        if (SitesPerPercent > 0) and ((SitesDone mod SitesPerPercent) = 0) then
          if DoProgressCheckCancel(SitesDone div SitesPerPercent, 'Computing amino acid frequencies...') then
            raise EAbort.Create('user cancelled');

        if not FOtuInfos[SeqI].IsUsed then
          Continue;
        Inc(CurSeqIndex);
        AFreq       := FreqInfo[TotalSeqs*DomainI + CurSeqIndex];
        DoubleArr   := AFreq.AlleleFreq;
        CurStr      := StrToStrWidth(FOtuInfos[SeqI].Name, MyMaxTaxaNameLen)+'  ';
        AmiXls.Add(FOtuInfos[SeqI].Name);

        // write total base freq percent first
        for x:='A' to 'Z' do
          FreqAA[x] := DoubleArr[Ord(x)-Ord('A')];
        WritePercent(FreqAA);
        WriteExcelPercent(FreqAA);
        ResultOutput.Add(rtfParaStr+CurStr);
        CurStr := EmptyStr;
        AmiXls.WriteLine();

        //=== update overall totals for all sequences
        for x:='A' to 'Z' do
          AllSeq[x] := AllSeq[x] + FreqAA[x];
      end; // end for each taxon

      // write averages over all sequences for this domain
      if TotalSeqs > 1 then
      begin
        CurStr := StrToStrWidth('Avg', MyMaxTaxaNameLen)+'  ';
        AmiXls.Add('Avg.');
        for x:= 'A' to 'Z' do
          AllSeq[x] := AllSeq[x]/TotalSeqs;
        WritePercent(AllSeq);
        WriteExcelPercent(AllSeq);
        ResultOutput.Add(rtfParaStr+rtfItalStr+CurStr+rtfPlainStr);
        CurStr := EmptyStr;
        AmiXls.WriteLine();
      end;
      // update all doms
      for x:='A' to 'Z' do
        AllDoms[x] := AllDoms[x] + AllSeq[x];
    end;  // end for all domains

    if MyDomains.Count > 1 then
    begin
      // now write the column headers for each domain separately
      CurStr  := StrToStrWidth('', MyMaxTaxaNameLen)+'  ';
      for x:='A' to 'Z' do
        if IsUnambiguousAminoAcid(x) then
        begin
          CurStr := CurStr + StrToStrWidth(GetThreeLetterCode(x), 5)+' ';
          AmiXls.Add(GetThreeLetterCode(x));
        end;
      ResultOutput.Add(rtfParaStr+rtfBoldStr+CurStr+rtfPlainStr);
      AmiXls.WriteLine(0, 'B');

      CurStr := StrToStrWidth('All', MyMaxTaxaNameLen)+'  ';
      AmiXls.Add('All');
      WritePercent(AllDoms);
      WriteExcelPercent(AllDoms);
      ResultOutput.Add(rtfParaStr+rtfBoldStr+CurStr+rtfPlainStr);
      CurStr := EmptyStr;
      AmiXls.WriteLine();
    end;
    ResultOutput.Add(rtfParaStr+CurStr);
    ResultOutput.Add(rtfEndStr);
 except
   On E: Exception do
   begin
     AmiXls.HadError;
     raise Exception.Create(E.Message);
   end;
 end;
 finally
   if FreqInfo <> nil then
   begin
     if FreqInfo.Count > 0 then
      for i:=0 to FreqInfo.Count-1 do
        if FreqInfo[i] <> nil then
          TLocusPopFreqInfo(FreqInfo[i]).Free;
    FreqInfo.Free;
   end;
   if Assigned(MyDomains) then
     MyDomains.Free;
 end;
end;

function TD_InputSeqData.TaxaWithNoDataMsg(taxa: TArrayOfInteger; otus: TList): String;
var
  index: Integer;
begin
  {$IFDEF VISUAL_BUILD}
  Result := 'The following taxa have no data available for the selected analysis:' + LineEnding;
  {$ELSE}
  Result := 'The following taxa were removed for the analysis because their data subset is empty:' + LineEnding;
  {$ENDIF}
  index := 0;
  while (index < Length(taxa)) and (index < 10) do
  begin
    {$IFDEF VISUAL_BUILD}
    if index = 9 then
      Result := Result + Format('+%d more', [Length(taxa) - 9]) + LineEnding
    else
    {$ENDIF}
      Result := Result + TOtuInfo(otus[taxa[index]]).Name + LineEnding;
    inc(index);
  end;
  {$IFDEF VISUAL_BUILD}
  Result := Result + 'Remove these taxa to allow the analysis to continue?';
  {$ENDIF}
end;

function TD_InputSeqData.DoProgressCheckCancel(progress: Integer; status: AnsiString): Boolean;
begin
 Result := False;
  if Assigned(ProgressCheckCancelFunc) then
    Result := ProgressCheckCancelFunc(progress, status)
  else if Assigned(ARP) then
    Result := ARP.ProgressAndStatusCheckCancel(progress, 'Status', 'status')
  else if Assigned(PleaseWait) then
  begin
    PleaseWait.Action := status;
    PleaseWait.PercentDone := progress;
    if not PleaseWait.Visible then
      PleaseWait.Show;
  end;
end;

procedure TD_InputSeqData.HideProgressDlg;
begin
  {$IFDEF VISUAL_BUILD}
   if Assigned(PleaseWait) and (not FIsRunningInThread) then
     PleaseWait.Hide;
  {$ENDIF}
end;

function TD_InputSeqData.SeqDataIsValidForLikelihoodAnalysis(aSeqData: TStringList; var aMsg: String): Boolean;
var
  aSeq, aSite: LongInt;
  aSequence: String;
  numValidBases: Integer;
begin
  Result := False;
  if aSeqData.Count < 3 then
  begin
    aMsg := 'at least 3 sequences are needed for ML analysis';
    Exit;
  end;
  for aSite := 1 to FNoOfSites do
  begin
    numValidBases := 0;
    for aSeq := 0 to aSeqData.Count -1 do
    begin
      aSequence := Trim(aSeqData[aSeq]);
      if Length(aSequence) > 0 then
      begin
        if (aSequence[aSite] <> '-') and (aSequence[aSite] <> '?') then
          inc(numValidBases);
      end
      else
      begin
        aMsg := 'insufficient data available for ML analysis. Please validate your sequence data';
        Exit;
      end;
      if numValidBases >= 3 then { then we at least one site where there are at least 3 sequences that have usable data}
      begin
        Result := True;
        Exit;
      end;
    end;
    aMsg := 'insufficient data available for ML analysis. Please validate your sequence data';
  end;
end;

function TD_InputSeqData.CheckFilterEmptySequences(aOptions: TDataSubsetOptions; aSeqList: TList; usedInfos: TList; var numTaxa, numSites: Integer; otuNames: TStringList): Boolean;
//var
//  seq: Integer;
begin
 Result := False;
 Assert(False, 'not implemented');
  //Result := True;
  //if not (usedInfos.Count > 0) then
  //  exit;
  //for seq := 0 to aSeqList.Count - 1 do
  //begin
  //
  //end;
end;

function TD_InputSeqData.CheckFilterEmptySequences(MAI: TObject): Boolean; overload;
var
  aSeqList: TStringList = nil;
  aTreeList: TTreeList = nil;
  aSeqData: String;
  msg: String;
  seq, site: Integer;
  isValid: Boolean;
  toRemove: TArrayOfInteger;
  response: Integer;
  aInfo: TAnalysisInfo = nil;
begin
  if not (MAI is TAnalysisInfo) then
    raise Exception.Create('Application error: CheckFilterEmptySequences requires TAnalysisInfo');
  aInfo := TAnalysisInfo(MAI);
  aSeqList := aInfo.MySeqStrings;
  Result := True;
  if not (aInfo.MyUsedOtuInfos.Count > 0) then
    exit;
  try
    SetLength(toRemove, 0);
    for seq := 0 to aSeqList.Count - 1 do
    begin
      aSeqData := aSeqList[seq];
      isValid := False;
      for site := 1 to Length(aSeqData) do
        if aSeqData[site] <> '-' then
        begin
          isValid := True;
          break;
        end;
      if not IsValid then
      begin
        SetLength(toRemove, Length(toRemove) + 1);
        toRemove[Length(toRemove) - 1] := seq;
      end;
    end;
    if Length(toRemove) > 0 then
    begin
      msg := TaxaWithNoDataMsg(toRemove, aInfo.MyUsedOtuInfos);
      {$IFDEF VISUAL_BUILD}
      response := MessageDlg('Disable unusable sequences?', msg, mtConfirmation, mbYesNo, 0);
      {$ELSE}
      Warn_NV(msg);
      response := mrYes;
      {$ENDIF}
      if response <> mrYes then
        Result := False
      else
      begin
        if Assigned(aInfo.MyOriTreeList) then
        begin
          for seq := Length(toRemove) - 1 downto 0 do
            aInfo.MyOriTreeList.RemoveOtu(aInfo.MyOtuNames[toRemove[seq]]);
        end
        else if aInfo.MyUserNewickTree <> EmptyStr then
        begin
          aTreeList := TTreeList.Create;
          aTreeList.ImportFromNewick(aInfo.MyUserNewickTree, aInfo.MyOtuNames);
          for seq := Length(toRemove) - 1 downto 0 do
            aTreeList.RemoveOtu(aInfo.MyOtuNames[toRemove[seq]]);
          aInfo.MyUserNewickTree := aTreeList.OutputNewickTree(0, False, False, 0.0);
        end;
        for seq := Length(toRemove) - 1 downto 0 do
        begin
          aSeqList.Delete(toRemove[seq]);
          TOtuInfo(aInfo.MyUsedOtuInfos[toRemove[seq]]).IsUsed := False;
          aInfo.MyUsedOtuInfos.Delete(toRemove[seq]);
          aInfo.MyOtuNames.Delete(toRemove[seq]);
        end;
        aInfo.MyNoOfSeqs := aSeqList.Count;
      end;
    end;
  finally
    SetLength(toRemove, 0);
    if Assigned(aTreeList) then
      aTreeList.Free;
  end;
end;

//===== Codon usage bias
procedure TD_InputSeqData.StatCodonUsage(var ResultOutput : TStringList; var CodonXls: TExcelWrite; const SaveTo : String);
var
  AllSeq : array [0..64] of Double;  // frequency of different nucs (at pos if applicable) for all seqs
  AllDoms: array [0..64] of Double;
  Usage:   array [0..64] of Double;
  TotalSeqs, i, j, Z, SeqI, SiteI, DomainI: LongInt;
  m1,m2,m3: LongInt;
  CurDomainIndex: LongInt;
  Codon: array[0..2] of AnsiChar;
  CodonMap: TCodonMap;
  CurDomain: TDomainInfo = nil;
  AFreq    : TLocusPopFreqInfo = nil;
  TempArray: PAnsiChar;
  CurStr   : AnsiString;
  MyDomains: TList = nil;     // List of domain pointers
  FreqInfo:  TList = nil;     // List of Lists of pops Allelefreq arrays
  DoubleArr: PArrayOfDouble;
  SitesDone, SitesPerPercent: Longint;

  procedure ComputeCodonUsage(Input: array of Double);
  var
    a, b: Longint;
    TotalCodons: Double;
    SynCodons: LongInt;
  begin
    for a:=0 to 63 do
      Usage[a] := -1;

    for a:=0 to 63 do
    begin
      if Usage[a] >= 0 then
        Continue;
      TotalCodons := 0;
      SynCodons   := 0;
      for b:=a to 63 do
        if FCodeTable[a] = FCodeTable[b] then
        begin
          TotalCodons := TotalCodons + Input[b];
          Inc(SynCodons);
        end;
      for b:=a to 63 do
        if FCodeTable[a] = FCodeTable[b] then
          if TotalCodons > 0 then
            Usage[b] := Input[b]/TotalCodons*SynCodons
          else
            Usage[b] := 0;
    end;
  end;

  // Make Percent out of the Input Array
  procedure WriteCodonUsage(Input: array of Double);
  var
    a,b,c: LongInt;
  begin
    Input[64] := 0;
    for a:= 0 to 63 do
      Input[64] := Input[64] + Input[a];

    ComputeCodonUsage(Input);
      CodonXls.Add('Codon');
      CodonXls.Add('Count');
      CodonXls.Add('RSCU');
      CodonXls.Add('Codon');
      CodonXls.Add('Count');
      CodonXls.Add('RSCU');
      CodonXls.Add('Codon');
      CodonXls.Add('Count');
      CodonXls.Add('RSCU');
      CodonXls.Add('Codon');
      CodonXls.Add('Count');
      CodonXls.Add('RSCU');
      CodonXls.WriteLine;
    for a:=0 to 3  do
    begin
      for b := 0 to 3 do
      begin
        for c := 0 to 3 do
        begin
          CodonMap := TCodonMap(a*16+ c*4 + b); // due to writing purposes
          CurStr := CurStr + CodonInfo.MakeCodonString(CodonMap) +
                            '('+FCodetable[Integer(CodonMap)]+')'+
                             FloatToStrWidth(Input[Integer(CodonMap)],Z+2,1);
//                            IntToStrWidth(Round(Input[Integer(CodonMap)]),Z);
          CurStr := CurStr+'('+FloatToStrWidth(Usage[Integer(CodonMap)],4,2)+')';
          CodonXls.Add(CodonInfo.MakeCodonString(CodonMap) + '('+FCodetable[Integer(CodonMap)]+')');
          CodonXls.Add(StrToFloat(Trim(FloatToStrWidth(Input[Integer(CodonMap)],Z+2,1))));
          CodonXls.Add(StrToFloat(Trim(FloatToStrWidth(Usage[Integer(CodonMap)],4,2))));
          CurStr := CurStr+' | ';
        end;
        ResultOutput.Add(rtfParaStr+CurStr);
        CodonXls.WriteLine();
        CurStr := EmptyStr;
      end;
      ResultOutput.Add(rtfParaStr+CurStr);
    end;
    ResultOutput.Add('Average# codons=' + IntToStrWidth(Round(Input[64]), 6)+' ');
    CodonXls.add('Average# codons=' + IntToStr(Round(Input[64])));
    CodonXls.WriteLine(0, 'A', '', True);
    CodonXls.NewLine();
  end;
begin
  CalculateMaxTaxaNameLen;
  UpdateSiteAttrArray;
  try try
    UpdateAACodonStarts;
    Z := Length(IntToStr(AACodonStarts.Count))+1;

    // first get total seqs
    TotalSeqs := 0;
    for SeqI:=0 to FNoOfTaxa-1 do
    begin
    //if (StatUseOnlyMarkedSitesItem.Checked) and (not IsHighlighted(SeqI)) then Continue;   // if we want only highlighted, and the site isn't highlighted then skip it.
      if FOtuInfos[SeqI].IsUsed then Inc(TotalSeqs);
    end;
    // create my domains list and the list to hold freqinfo
    MyDomains := TList.Create;
    FreqInfo  := TList.Create;

    SitesDone := 0;
    SitesPerPercent := AACodonStarts.Count*FNoOfTaxa div 100;
    if SitesPerPercent > 0 then
      if DoProgressCheckCancel(0, 'Calculating codon usage...') then
        raise EAbort.Create('user cancelled');

    for SiteI:=0 to AACodonStarts.Count-1 do
    begin
      // so we first find the index in the Myfreqs
      m1 := AACodonStarts[SiteI];  // list of full codon start indices
      m2 := AACodonPos2[SiteI]; // list of 2nd pos for full codons
      m3 := AACodonPos3[SiteI]; // list of 3rd pos for full codons
      CurDomain := DomainMark[m1];
      if (CurDomain <> nil) and (not CurDomain.IsUsed) then
      begin
        SitesDone := SitesDone + FNoOfTaxa;
        Continue;
      end;

      // check if the user wants only highlighted codons
      if (VS_SeqDataExplorer.CurAttrDisp <> megNone) and (VS_SeqDataExplorer.StatUseOnlyMarkedSitesItem) then
      begin
        if not ( (VS_SeqDataExplorer.CurAttrDisp in SiteAttrArray[m1]) or  // mega2b4
                 (VS_SeqDataExplorer.CurAttrDisp in SiteAttrArray[m2]) or
                 (VS_SeqDataExplorer.CurAttrDisp in SiteAttrArray[m3])    ) then
        begin
          SitesDone := SitesDone + FNoOfTaxa;
          Continue;
        end;
      end;

      CurDomainIndex := MyDomains.IndexOf(CurDomain);
      if CurDomainIndex < 0 then
      begin
        MyDomains.Add(CurDomain);
        CurDomainIndex := MyDomains.Count-1;
        AFreq := TLocusPopFreqInfo.Create;
        FreqInfo.Add(AFreq); // add first to make sure memory free
        AFreq.NoOfAlleles := 64;
        GetMem(DoubleArr, AFreq.NoOfAlleles*Sizeof(Double));
        for i:=0 to AFreq.NoOfAlleles-1 do
          DoubleArr[i] := 0;
        AFreq.AlleleFreq := DoubleArr;
        AFreq.NoOfIndv := 0;
      end;

      // now we count codon freq
      // so we first find the index in the Myfreqs
      for SeqI:=0 to FNoOfTaxa-1 do
      begin
        Inc(SitesDone);
        if (SitesPerPercent > 0) and ((SitesDone mod SitesPerPercent) = 0) then
          if DoProgressCheckCancel(SitesDone div SitesPerPercent, 'Calculating codon usage...') then
            raise EAbort.Create('user cancelled');

        if not FOtuInfos[SeqI].IsUsed then
          Continue;
        AFreq     := FreqInfo[CurDomainIndex];
        DoubleArr := AFreq.AlleleFreq;

        TempArray := PAnsiChar(FOtuInfos[SeqI].Data);

        Codon[0]  := UpCase(TempArray[m1]);
        Codon[1]  := UpCase(TempArray[m2]);
        Codon[2]  := UpCase(TempArray[m3]);
        CodonMap := CodonInfo.MakeCodonBitMap(Codon);
        if CodonMap <= #63 then
          DoubleArr[Integer(CodonMap)] := DoubleArr[Integer(CodonMap)]+1;
      end;
    end; // for all sites

    // reset Freq for overage over all domains
    for i:=0 to 63 do AllDoms[i] := 0;

    SitesPerPercent := MyDomains.Count div 100;
    if SitesPerPercent > 0 then
      if DoProgressCheckCancel(0, 'Calculating codon usage...') then
        raise EAbort.Create('user cancelled');
    SitesDone := 0;

    CodonXls := TExcelWrite.Create(nil, 'Codon Usage');
    CodonXls.IsXLS := VS_SeqDataExplorer.IsXls;
    CodonXls.AddWorksheet('Info');

    ResultOutput.Clear;
    ResultOutput.Add(rtfHeaderStr);
    ResultOutput.Add(rtfBoldStr+'Codon Usage'+rtfPlainStr);
    CodonXls.WriteLine('Codon Usage', 1);
    ResultOutput.Add(rtfParaStr+'Code Table: '+FCodeName);
    CodonXls.WriteLine('Codon Table', 1);
    ResultOutput.Add(rtfParaStr+'Codons Used: '+'All selected');
    CodonXls.WriteKeypair('Codons Used', 'All Selected', 1);
    ResultOutput.Add(rtfParaStr+'No. of sequences used: '+IntToStr(TotalSeqs));
    CodonXls.WriteKeypair('No. of sequences used', 'TotalSeqs', 1);
    ResultOutput.Add(rtfParaStr+'Sequences used');
    CodonXls.WriteLine('Sequences used', 1);
    for SeqI:=0 to FNoOfTaxa-1 do
    begin
    // if (StatUseOnlyMarkedSitesItem.Checked) and (not IsHighlighted(SeqI)) then Continue;
      if FOtuInfos[SeqI].IsUsed then
      begin
        ResultOutput.Add(rtfParaStr+'  '+FOtuInfos[SeqI].Name);
        CodonXls.WriteLine(FOtuInfos[SeqI].Name, 1);
      end;
    end;
    if TotalSeqs > 1 then
    begin
      ResultOutput.Add(rtfParaStr+'All frequencies are averages over all taxa.');
      CodonXls.Add('All frequencies are averages over all taxa.');
      CodonXls.WriteLine(0, 'A', '', True);
    end;
    ResultOutput.Add(rtfParaStr+'Relative synonymous codon usage is given in parentheses');
    CodonXls.Add('Relative synonymous codon usage is given in parentheses following the codon frequency');
    CodonXls.WriteLine(0, 'A', '', True);
    CodonXls.NewLine();
    ResultOutput.Add(rtfParaStr+'following the codon frequency');


    for DomainI :=0 to MyDomains.Count-1 do
    begin
      Inc(SitesDone);
      if (SitesPerPercent > 0) and ((SitesDone mod SitesPerPercent) = 0) then
        if DoProgressCheckCancel(SitesDone div SitesPerPercent, 'Calculating codon usage...') then
          raise EAbort.Create('user cancelled');

      ResultOutput.Add(rtfParaStr+EmptyStr);
      // write the domain name
      CurDomain := MyDomains[DomainI];
      if CurDomain = nil then
        CurStr := 'Sites with no domain definition'
      else
      begin
        CurStr := 'Domain: '+CurDomain.Name;
        if length(CurDomain.GeneName)  > 0 then
          CurStr := CurStr + ' (Gene: '+CurDomain.GeneName+')';
      end;
      ResultOutput.Add(rtfParaStr+rtfBoldStr+CurStr+rtfPlainStr);
      CodonXls.Add(CurStr);
      CodonXls.WriteLine(0, 'A', '', True);
      CurStr := EmptyStr;

      AFreq       := FreqInfo[DomainI];
      DoubleArr   := AFreq.AlleleFreq;

      // write total base freq percent first
      for j:=0 to 63 do
      begin
        AllSeq[j]    := DoubleArr[j]/TotalSeqs;
        AllDoms[j]   := AllDoms[j] + DoubleArr[j];
      end;
      WriteCodonUsage(AllSeq);
//      ResultOutput.Add(rtfParaStr+CurStr);
      CurStr := EmptyStr;
    end;  // end for all domains

    if MyDomains.Count > 1 then
    begin
      ResultOutput.Add(rtfParaStr+EmptyStr);
      CodonXls.NewLine();
      ResultOutput.Add(rtfParaStr+rtfBoldStr+'Codon Usage over all Domains'+rtfPlainStr);
      CodonXls.Add('Codon Usage over all domains');
      CodonXls.WriteLine(0, 'A', '', True);
{      for j:=0 to 63 do
        AllDoms[j]   := AllDoms[j]/TotalSeqs;}
      CurStr := EmptyStr;
      WriteCodonUsage(AllDoms);
      CurStr := EmptyStr;
    end;
    ResultOutput.Add(rtfParaStr+CurStr);
    CodonXls.WriteLine(CurStr);
    ResultOutput.Add(rtfEndStr);
 except
   on E:EAbort do
     raise E;
   On E: Exception do
     raise E;
 end;
 finally
   if FreqInfo <> nil then
   begin
     if FreqInfo.Count > 0 then
      for i := 0 to FreqInfo.Count - 1 do
        if Assigned(FreqInfo[i]) then
          TLocusPopFreqInfo(FreqInfo[i]).Free;
     FreqInfo.Free;
   end;
   if Assigned(MyDomains) then
     MyDomains.Free;
 end;
end;

// nucl composition
procedure TD_InputSeqData.StatNucComposition(var ResultOutput : TStringList; var nucXls: TExcelWrite; const SaveTo : String);
const
  PROGRESS_MESSAGE = 'Computing Base Frequencies...';
  PROGRESS_MESSAGE2 = 'Preparing Output...';
var
  OnePos: array [0..4]       of Double;  // At given position 1st, 2nd, 3rd and Over all sites; Fourbases + 1 for total
  SumPos : array [0..4]      of Double;  // Sum over all frequencies in a given sequence
  AllSeq : array [0..3,0..4] of Double;  // frequency of different nucs (at pos if applicable) for all seqs
  AllDoms: array [0..3,0..4] of Double;
  Z, TotalSeqs, i, j, SeqI, SiteI, DomainI: LongInt;
  CurDomainIndex, CurSeqIndex, Basemult: LongInt;
  MyMaxTaxaNameLen: LongInt;
  CurDomain: TDomainInfo = nil;
  AFreq    : TLocusPopFreqInfo = nil;
  TempArray: PAnsiChar;
  CurStr   : AnsiString;      //
  MyDomains: TList = nil;     // List of domain pointers
  FreqInfo:  TList = nil;     // List of Lists of pops Allelefreq arrays
  DoubleArr: PArrayOfDouble;
  SitesDone, SitesPerPercent: Longint;

  // Make Percent out of the Input Array
  procedure WritePercent(Input: array of Double);
  var
    x: LongInt;
    Percent: Double;
  begin
    Input[4] := 0;
    for x:= 0 to 3 do
      Input[4] := Input[4] + Input[x];

    for x:= 0 to 3 do
    begin
      Percent := 0;
      if Input[4] > 0 then
        Percent := Input[x]/Input[4]*100;
      CurStr := CurStr + FloatToStrWidth(Percent, 5, 1)+' ';
    end;

    if Ceil(Input[4]) <> Floor(Input[4]) then
      CurStr := CurStr + FloatToStrWidth(Input[4], Z+2, 1)
    else
      CurStr := CurStr + IntToStrWidth(Round(Input[4]), Z)+' ';
  end;
  procedure WriteExcelPercent(Input: array of Double);
  var
    x: LongInt;
    Percent: Double;
  begin
    Input[4] := 0;
    for x:= 0 to 3 do
      Input[4] := Input[4] + Input[x];

    for x:= 0 to 3 do
    begin
      Percent := 0;
      if Input[4] > 0 then
        Percent := Input[x]/Input[4]*100;
      NucXls.Add(Percent);
    end;

    if Ceil(Input[4]) <> Floor(Input[4]) then
    begin
      NucXls.Add(Input[4]);
    end
    else
    begin
      NucXls.Add(Round(Input[4]));
    end;  
  end;
begin
  CalculateMaxTaxaNameLen;
  MyMaxTaxaNameLen := MaxTaxaNameLen;
  if MyMaxTaxaNameLen < 7 then
    MyMaxTaxaNameLen := 7;

  Z := Length(IntToStr(FNoOfSites))+1;
  if Z < 5 then  Z := 5;

  AFreq := nil;
  UpdateSiteAttrArray;
  try try
    DoProgressCheckCancel(0, PROGRESS_MESSAGE);

    // first get total seqs
    TotalSeqs := 0;
    for SeqI:=0 to FNoOfTaxa-1 do
      if FOtuInfos[SeqI].IsUsed then Inc(TotalSeqs);

    // create my domains list and the list to hold freqinfo
    MyDomains := TList.Create;
    FreqInfo  := TList.Create;

    SitesDone := 0;
    SitesPerPercent := FNoOfSites*FNoOfTaxa div 100;
    if SitesPerPercent > 0 then
      if DoProgressCheckCancel(0, PROGRESS_MESSAGE) then
        raise EAbort.Create('user cancelled');

    for SiteI:=0 to FNoOfSites-1 do
    begin
      // so we first find the index in the Myfreqs
      CurDomain := DomainMark[SiteI];
      if (CurDomain <> nil) and (not CurDomain.IsUsed) then
      begin
         SitesDone := SitesDone + FNoOfTaxa;
         Continue;
      end;

      // check if the user wants only highlighted sites.
      if (VS_SeqDataExplorer.CurAttrDisp <> megNone) and (VS_SeqDataExplorer.StatUseOnlyMarkedSitesItem) then
        if not (VS_SeqDataExplorer.CurAttrDisp in SiteAttrArray[SiteI]) then
        begin
          SitesDone := SitesDone + FNoOfTaxa;
          Continue;
        end;

      //Go do computation
      CurDomainIndex := MyDomains.IndexOf(CurDomain);
      if CurDomainIndex < 0 then
      begin
        MyDomains.Add(CurDomain);
        CurDomainIndex := MyDomains.Count-1;
        for SeqI:=0 to FNoOfTaxa-1 do
        begin
          if not FOtuInfos[SeqI].IsUsed then
           Continue;
          AFreq := TLocusPopFreqInfo.Create;
          FreqInfo.Add(AFreq); // add first to make sure memory free
          if (CurDomain <> nil) and CurDomain.IsCoding then
            AFreq.NoOfAlleles := 12
          else
            AFreq.NoOfAlleles :=  4;
          GetMem(DoubleArr, AFreq.NoOfAlleles*Sizeof(Double));
          for i:=0 to AFreq.NoOfAlleles-1 do
            DoubleArr[i] := 0;
          AFreq.AlleleFreq := DoubleArr;
          AFreq.NoOfIndv := 0;
        end
      end;

      // now we count base freq
      // so we first find the index in the Myfreqs
      BaseMult := -1;
      if AFreq.NoOfAlleles = 4 then BaseMult :=0
      else if meg1stBase in SiteAttrArray[SiteI] then BaseMult :=0
      else if meg2ndBase in SiteAttrArray[SiteI] then BaseMult :=1
      else if meg3rdBase in SiteAttrArray[SiteI] then BaseMult :=2;

      if BaseMult < 0 then // unclassifiable site in some sense
       Continue;

      CurSeqIndex := -1;
      for SeqI:=0 to FNoOfTaxa-1 do
      begin
        Inc(SitesDone);
        if (SitesPerPercent > 0) and ((SitesDone mod SitesPerPercent) = 0) then
          if DoProgressCheckCancel(SitesDone div SitesPerPercent, PROGRESS_MESSAGE) then
            raise EAbort.Create('user cancelled');

        if not FOtuInfos[SeqI].IsUsed then
          Continue;
        Inc(CurSeqIndex);
        AFreq     := FreqInfo[TotalSeqs*CurDomainIndex + CurSeqIndex];
        DoubleArr := AFreq.AlleleFreq;

        TempArray := PAnsiChar(FOtuInfos[SeqI].Data);
        case UpCase(TempArray[SiteI]) of
          'T',
          'U': DoubleArr[BaseMult*4+0] := DoubleArr[BaseMult*4+0]+1;
          'C': DoubleArr[BaseMult*4+1] := DoubleArr[BaseMult*4+1]+1;
          'A': DoubleArr[BaseMult*4+2] := DoubleArr[BaseMult*4+2]+1;
          'G': DoubleArr[BaseMult*4+3] := DoubleArr[BaseMult*4+3]+1;
        end;
      end;
    end; // for all sites

    // reset Freq for average over all domains
    for i:=0 to 3 do  // for overall + 3 pos
     for j:= 0 to 4 do AllDoms[i][j] := 0;

    SitesPerPercent := MyDomains.Count*FNoOfTaxa div 100;
    if SitesPerPercent > 0 then
      if DoProgressCheckCancel(0, PROGRESS_MESSAGE2) then
        raise EAbort.Create('user cancelled');
    SitesDone := 0;

    ResultOutput.Add(rtfHeaderStr);
    {$IFDEF VISUAL_BUILD}
    ResultOutput.Add(rtfStartStr+rtfPlainStr+'Data Filename: '+ExtractFileName(MegaForm.DataFileName));
    NucXls.WriteKeypair('Data Filename:', ExtractFileName(MegaForm.DataFileName), 1);
    ResultOutput.Add(rtfParaStr +'Data Title: '+MegaForm.DataTitle);
    NucXls.WriteKeypair('Data Title:', MegaForm.DataTitle, 1);
    {$ELSE}
    ResultOutput.Add(rtfStartStr+rtfPlainStr+'Data Filename: '+ExtractFileName(D_MegaMain.DataFileName));
    NucXls.WriteKeypair('Data Filename:', ExtractFileName(D_MegaMain.DataFileName), 1);
    ResultOutput.Add(rtfParaStr +'Data Title: '+D_MegaMain.DataTitle);
    NucXls.WriteKeypair('Data Title:', D_MegaMain.DataTitle, 1);
    {$ENDIF}
    ResultOutput.Add(rtfParaStr+rtfBoldStr+'Nucleotide Frequencies'+rtfPlainStr);
    NucXls.WriteKeypair('Nucleotide Frequencies', rtfPlainStr, 1);
    if (VS_SeqDataExplorer.CurAttrDisp <> megNone) and (VS_SeqDataExplorer.StatUseOnlyMarkedSitesItem) then
    begin
      ResultOutput.Add(rtfParaStr+'Sites Used: '+VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites]);
      NucXLS.WriteKeypair('Sites Used:', VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites], 1);
    end
    else
    begin
      ResultOutput.Add(rtfParaStr+'Sites Used: '+'All selected');
      NucXls.WriteKeypair('Sites Used:', 'All selected', 1);
    end;
    ResultOutput.Add(rtfParaStr+'All frequencies are given in percent.');
    NucXls.WriteLine('All frequencies are given in percent.', 1);
    // Now we need to write base frequencies //
    for DomainI :=0 to MyDomains.Count-1 do
    begin
      // write the domain name
      CurDomain := MyDomains[DomainI];
      if CurDomain = nil then
      begin
        CurStr := 'Independent Sites';
        NucXls.Add('Independent Sites');        
      end
      else
      begin
        CurStr := 'Domain: '+CurDomain.Name;
        NucXls.Add('Domain: ' + CurDomain.Name);
        if length(CurDomain.GeneName)  > 0 then
        begin
          CurStr := CurStr + ' (Gene: '+CurDomain.GeneName+')';
          NucXls.Add('Gene: ' + CurDomain.GeneName);
        end;

      end;
      ResultOutput.Add(rtfParaStr+rtfBoldStr+CurStr+rtfPlainStr);

      NucXls.WriteLine();
      // now write the column headers for each domain separately
      CurStr := BlankString(MyMaxTaxaNameLen)+'  '+
                ' T(U)   C     A     G    Total ';
      NucXls.Add('T(U)');
      NucXls.Add('C');
      NucXls.Add('A');
      NucXls.Add('G');
      NucXls.Add('Total');


      if (CurDomain <> nil) and CurDomain.IsCoding then
        begin
          CurStr := CurStr +
                  ' T-1   C-1   A-1   G-1  Pos #1 '+
                  ' T-2   C-2   A-2   G-2  Pos #2 '+
                  ' T-3   C-3   A-3   G-3  Pos #3 ';

          NucXls.Add('T-1');
          NucXls.Add('C-1');
          NucXls.Add('A-1');
          NucXls.Add('G-1');
          NucXls.Add('Pos #1');
          NucXls.Add('T-2');
          NucXls.Add('C-2');
          NucXls.Add('A-2');
          NucXls.Add('G-2');
          NucXls.Add('Pos #2');
          NucXls.Add('T-3');
          NucXls.Add('C-3');
          NucXls.Add('A-3');
          NucXls.Add('G-3');
          NucXls.Add('Pos #3');
        end;

      ResultOutput.Add(rtfParaStr+CurStr+rtfPlainStr);

      NucXls.WriteLine(0, 'B');
      // reset Freq for all Seqs
      for i:=0 to 3 do  // for three positions and one over all positions
        for j:= 0 to 4 do AllSeq[i][j] := 0;

      CurSeqIndex := -1;
      for SeqI := 0 to FNoOfTaxa-1 do
      begin
        Inc(SitesDone);
        if (SitesPerPercent > 0) and ((SitesDone mod SitesPerPercent) = 0) then
          if DoProgressCheckCancel(SitesDone div SitesPerPercent, PROGRESS_MESSAGE2) then
             raise EAbort.Create('user cancelled');

        if not FOtuInfos[SeqI].IsUsed then
          Continue;
        Inc(CurSeqIndex);
        AFreq       := FreqInfo[TotalSeqs*DomainI + CurSeqIndex];
        DoubleArr   := AFreq.AlleleFreq;
        CurStr      := StrToStrWidth(FOtuInfos[SeqI].Name, MyMaxTaxaNameLen)+'  ';
        for j:=0 to 4 do
          SumPos[j] := 0;

        // write total base freq percent first
        for j:=0 to 3 do
          if (CurDomain <> nil) and CurDomain.IsCoding then
            SumPos[j] := DoubleArr[j] + DoubleArr[4+j] + DoubleArr[8+j]
          else
            SumPos[j] := DoubleArr[j];
        NucXls.Add(FOtuInfos[SeqI].Name);
        WritePercent(SumPos);
        WriteExcelPercent(SumPos);

        //  now write the three positions if coding data
        if (CurDomain <> nil) and CurDomain.IsCoding then
        begin
          for j:=0 to 3 do OnePos[j] := DoubleArr[j];   // 1st base
          begin
          WritePercent(OnePos);
          WriteExcelPercent(OnePos);
          end;
          for j:=0 to 3 do OnePos[j] := DoubleArr[4+j]; // 2nd base
          begin
          WritePercent(OnePos);
          WriteExcelPercent(OnePos);
          end;
          for j:=0 to 3 do OnePos[j] := DoubleArr[8+j]; // 3rd base
          begin
          WritePercent(OnePos);
          WriteExcelPercent(OnePos);
          end;
        end;
        ResultOutput.Add(rtfParaStr+CurStr);

       NucXls.WriteLine();

        //=== update overall totals for all sequences
        for j:=0 to 3 do  AllSeq[0][j] := AllSeq[0][j] + SumPos[j];
        //== Update three positions overall totals
        if (CurDomain <> nil) and CurDomain.IsCoding then
          for j:=0 to 3 do
          begin
            AllSeq[1][j] := AllSeq[1][j] + DoubleArr[j];
            AllSeq[2][j] := AllSeq[2][j] + DoubleArr[4+j];
            AllSeq[3][j] := AllSeq[3][j] + DoubleArr[8+j];
          end;
      end; // end for each taxon

      // write averages over all sequences for this domain
      if TotalSeqs > 1 then
      begin
        CurStr := StrToStrWidth('Avg.', MyMaxTaxaNameLen)+'  ';
        NucXls.Add('Avg.');
        for j:= 0 to 4 do
          AllSeq[0][j] := AllSeq[0][j]/TotalSeqs;
        WritePercent(AllSeq[0]);
        WriteExcelPercent(AllSeq[0]);
        //  now write the three positions if coding data
        if (CurDomain <> nil) and CurDomain.IsCoding then
          for i:= 1 to 3 do
          begin
            for j:= 0 to 4 do
              AllSeq[i][j] := AllSeq[i][j]/TotalSeqs;
            WritePercent(AllSeq[i]);
            WriteExcelPercent(AllSeq[i]);
          end;
        ResultOutput.Add(rtfParaStr+rtfItalStr+CurStr+rtfPlainStr);
        CurStr := EmptyStr;
        NucXls.WriteLine();
      end;
      // update all doms
      for i:=0 to 3 do  // for overall + 3 pos
        for j:= 0 to 4 do AllDoms[i][j] := AllDoms[i][j] + AllSeq[i][j];
    end;  // end for all domains

    if MyDomains.Count > 1 then
    begin
      // now write the column headers for each domain separately
      CurStr := BlankString(MyMaxTaxaNameLen)+'  '+
                ' T(U)   C     A     G    Total ';
      if FDomainMarks.IsCoding then
        CurStr := CurStr +
                ' T-1   C-1   A-1   G-1  Pos #1 '+
                ' T-2   C-2   A-2   G-2  Pos #2 '+
                ' T-3   C-3   A-3   G-3  Pos #3 ';
      ResultOutput.Add(rtfParaStr+rtfBoldStr+CurStr+rtfPlainStr);

      CurStr := StrToStrWidth('All', MyMaxTaxaNameLen)+'  ';
      WritePercent(AllDoms[0]);
      //  now write the three positions if coding data
      if FDomainMarks.IsCoding then
        for i:=1 to 3 do
          WritePercent(AllDoms[i]);
      ResultOutput.Add(rtfParaStr+rtfBoldStr+CurStr+rtfPlainStr);
      CurStr := EmptyStr;
    end;
    ResultOutput.Add(rtfParaStr+CurStr);
    ResultOutput.Add(rtfEndStr);

    NucXls.ApplyFormat(1, 2, 24, NucXls.OutputLineLvl[0], '?.0'); // B2:Z+OutputLineLvl[0]
    NucXls.ApplyFormat(6, 2, 6, NucXls.OutputLineLvl[0], '?'); // F2:F+OutputLineLvl[0]
    if FDomainMarks.IsCoding then
    begin
      NucXls.ApplyFormat(11, 2, 11, NucXls.OutputLineLvl[0], '?');  // K2:K+OutputLineLvl[0]
      NucXls.ApplyFormat(16, 2, 16, NucXls.OutputLineLvl[0], '?');  // P2:P+OutputLineLvl[0]
      NucXls.ApplyFormat(21, 2, 32, NucXls.OutputLineLvl[0], '?');  // U2:U+OutputLineLvl[0]
    end;
 except
   On E:EAbort do
   begin
     NucXls.HadError;
   end;
   On E: Exception do
   begin
     NucXls.HadError;
     raise Exception.Create(E.Message);
   end;
 end;
 finally
   if Assigned(FreqInfo) and (FreqInfo.Count > 0) then
   begin
    for i := FreqInfo.Count-1 downto 0 do
      if FreqInfo[i] <> nil then
        TLocusPopFreqInfo(FreqInfo[i]).Free;
    FreqInfo.Free;
   end;
   if Assigned(MyDomains) then
     MyDomains.Free;
 end;
end;

//------- Statistics related material ------------
procedure TD_InputSeqData.StatPairFreq(var ResultOutput : TStringList; var FreqXls: TExcelWrite; const SaveTo : String; const IsDirectional : Boolean);
const
  T: Integer = 0;
  C: Integer = 1;
  A: Integer = 2;
  G: Integer = 3;
const
  cBase='TCAG';
var
  Map1, Map2: LongInt;
  i,j,Z: LongInt;  // Z is the length of the FNoOfSites
  SiteCount, iden, ts, tv: Double;
  Seq1, Seq2: PAnsiChar;
  Matrix: array [0..3,0..3] of Double;
  TotalSeqs, SeqI, SiteI, DomainI: LongInt;
  CurDomainIndex, Basemult: LongInt;
  CurDomain: TDomainInfo = nil;
  AFreq    : TLocusPopFreqInfo = nil;
  FixedLenBlankStr, DomainNameStr, CurStr   : AnsiString;      //
  MyDomains: TList = nil;     // List of domain pointers
  FreqInfo:  TList = nil;     // List of Lists of pops Allelefreq arrays
  DoubleArr: PArrayOfDouble = nil;
  SitesDone, SitesPerPercent: Longint;

  // maps the nuc to an array index
  function MapNuc(Base: AnsiChar): Integer;  // maps
  begin
    Result := -1;
    case Upcase(Base) of
      'T', 'U': Result := T;     'C': Result := C;
      'A'     : Result := A;     'G': Result := G;
    end;
  end;

  procedure WriteValues;
  var
    ii,jj: Integer;
  begin
    // write R first
    Iden := Matrix[T,T] + Matrix[C,C] + Matrix[A,A] + Matrix[G,G];
    ts   := Matrix[T,C] + Matrix[C,T] + Matrix[A,G] + Matrix[G,A];
    tv   := Matrix[T,A] + Matrix[A,T] + Matrix[T,G] + Matrix[G,T]+
            Matrix[C,A] + Matrix[A,C] + Matrix[C,G] + Matrix[G,C];
    SiteCount:= ts+tv+Iden;

    // write Identical, Ts, Tv
    CurStr := CurStr +
              IntToStrWidth(Round(Iden), Z) +
              IntToStrWidth(Round(ts),   Z) +
              IntToStrWidth(Round(tv),   Z);
    FreqXls.Add(Round(Iden));
    FreqXls.Add(Round(ts));
    FreqXls.Add(Round(tv));
    // write R
    if tv > 0 then
    begin
      CurStr := CurStr + FloatToStrWidth(ts/tv, Z,1);
      FreqXls.Add(ts/tv);
    end
    else
    begin
      CurStr := CurStr + StrToStrWidth(FixedLenBlankStr+'nc', Z);
      FreqXls.Add('nc');
    end;

    // write the identical Pairs
    for ii:=0 to 3 do
      for jj:=0 to 3 do
        if (IsDirectional) or (ii=jj) then
        begin
          CurStr := CurStr +IntToStrWidth(Round(Matrix[ii,jj]), Z);
          FreqXls.Add(Round(Matrix[ii,jj]));
        end
        else if (jj > ii) then
        begin
          CurStr := CurStr +IntToStrWidth(Round(Matrix[ii,jj]+Matrix[jj,ii]), Z);
          FreqXls.Add(Round(Matrix[ii,jj]+Matrix[jj,ii]));
        end;
    CurStr := CurStr + ' '+FloatToStrWidth(SiteCount, Z+1,1);
    FreqXls.Add(SiteCount);
  end; // end of writevalue procedure

begin
  UpdateSiteAttrArray;
  try try
    // first get total seqs
    TotalSeqs := 0;
    for SeqI:=0 to FNoOfTaxa-1 do
      if FOtuInfos[SeqI].IsUsed then Inc(TotalSeqs);

    if TotalSeqs < 2 then
    begin
      RaiseErrorMessage(HC_Unexpected_Error, 'You need to select at least two sequences.');
      Exit;
    end;

    if DoProgressCheckCancel(0, 'Computing nucleotide pair frequencies...') then
      raise EAbort.Create('user cancelled');
    SitesDone := 0;
    SitesPerPercent := FNoOfSites*FNoOfTaxa div 100;

    // create my domains list and the list to hold freqinfo
    MyDomains := TList.Create;
    FreqInfo  := TList.Create;

    for SiteI:=0 to FNoOfSites-1 do
    begin
      // so we first find the index in the Myfreqs
      CurDomain := DomainMark[SiteI];
      if (CurDomain <> nil) and (not CurDomain.IsUsed) then
      begin
        SitesDone := SitesDone + FNoOfTaxa;
        Continue;
      end;

      // check if the user wants only highlighted sites.
      if (VS_SeqDataExplorer.CurAttrDisp <> megNone) and (VS_SeqDataExplorer.StatUseOnlyMarkedSitesItem) then
        if not (VS_SeqDataExplorer.CurAttrDisp in SiteAttrArray[SiteI]) then
        begin
          SitesDone := SitesDone + FNoOfTaxa;
          Continue;
        end;

      // go in and do the analysis
      CurDomainIndex := MyDomains.IndexOf(CurDomain);
      if CurDomainIndex < 0 then
      begin
        MyDomains.Add(CurDomain);
        CurDomainIndex := MyDomains.Count-1;
        AFreq := TLocusPopFreqInfo.Create;
        FreqInfo.Add(AFreq); // add first to make sure memory free
        if (CurDomain <> nil) and CurDomain.IsCoding then
          AFreq.NoOfAlleles := 16*3  // for three pos
        else
          AFreq.NoOfAlleles := 16;
        GetMem(DoubleArr, AFreq.NoOfAlleles*Sizeof(Double));
        for i:=0 to AFreq.NoOfAlleles-1 do
          DoubleArr[i] := 0;
        AFreq.AlleleFreq := DoubleArr;
        AFreq.NoOfIndv := 0;
      end;

      // now we count freqs
      // so we first find the index in the Myfreqs
      BaseMult := -1;
      if AFreq.NoOfAlleles = 16                  then BaseMult :=0
      else if meg1stBase in SiteAttrArray[SiteI] then BaseMult :=0
      else if meg2ndBase in SiteAttrArray[SiteI] then BaseMult :=1
      else if meg3rdBase in SiteAttrArray[SiteI] then BaseMult :=2;

      if BaseMult < 0 then // unclassifiable site in some sense
       Continue;

      // now we update the counts
      AFreq     := FreqInfo[CurDomainIndex];
      DoubleArr := AFreq.AlleleFreq;

      for i:=1 to FNoOfTaxa - 1 do
      begin
        Inc(SitesDone);
        if (SitesPerPercent > 0) and ((SitesDone mod SitesPerPercent) = 0) then
          if DoProgressCheckCancel(SitesDone div SitesPerPercent, 'Computing nucleotide pair frequencies...') then
            raise EAbort.Create('user cancelled');

        if not FOtuInfos[i].IsUsed then
          Continue;  // skip
        Seq1 := PAnsiChar(FOtuInfos[i].Data);
        Map1 := MapNuc(Seq1[SiteI]);
        if Map1 < 0 then
          Continue;

        for j:=0 to i-1 do
        begin
          if not FOtuInfos[j].IsUsed then Continue;  // skip
          Seq2 := PAnsiChar(FOtuInfos[j].Data);
          Map2 := MapNuc(Seq2[SiteI]);
          if Map2 < 0 then Continue;
          DoubleArr[BaseMult*16+(Map1*4+Map2)] := DoubleArr[BaseMult*16+(Map1*4+Map2)]+1;
        end;
      end;
    end; // for all sites for a given domain

    // reset Freq for overage over all domains
    Z := Length(IntToStr(FNoOfSites))+1;
    if Z < 5 then  Z := 5;
    FixedLenBlankStr:= BlankString(Z-2);

    if DoProgressCheckCancel(0, 'Preparing to display results') then
      raise EAbort.Create('user cancelled');
    SitesPerPercent := MyDomains.Count div 100;
    SitesDone := 0;

    ResultOutput.Add(rtfHeaderStr);
	{$IFDEF VISUAL_BUILD}
     ResultOutput.Add(rtfStartStr+rtfPlainStr+'Data Filename: '+ExtractFileName(MegaForm.DataFileName));
     FreqXls.WriteKeypair('Data Filename:', ExtractFileName(MegaForm.DataFileName), 1);
     ResultOutput.Add(rtfParaStr +'Data Title: '+MegaForm.DataTitle);
     FreqXls.WriteKeypair('Data Title:', MegaForm.DataTitle, 1);
	 {$ELSE}
	  ResultOutput.Add(rtfStartStr+rtfPlainStr+'Data Filename: '+ExtractFileName(D_MegaMain.DataFileName));
     FreqXls.WriteKeypair('Data Filename:', ExtractFileName(D_MegaMain.DataFileName), 1);
     ResultOutput.Add(rtfParaStr +'Data Title: '+D_MegaMain.DataTitle);
     FreqXls.WriteKeypair('Data Title:', D_MegaMain.DataTitle, 1);
	 {$ENDIF}
    ResultOutput.Add(rtfParaStr+rtfBoldStr+'Nucleotide Pair Frequencies'+rtfPlainStr);
    FreqXls.WriteLine('Nucleotide Pair Frequencies', 1);
    if (VS_SeqDataExplorer.CurAttrDisp <> megNone) and (VS_SeqDataExplorer.StatUseOnlyMarkedSitesItem) then
    begin
      ResultOutput.Add(rtfParaStr+'Sites Used: '+ VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites]);
      FreqXls.WriteKeypair('Sites Used:', VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites], 1);
    end
    else
    begin
      ResultOutput.Add(rtfParaStr+'Sites Used: '+'All selected');
      FreqXls.WriteKeypair('Sites Used', 'All selected', 1);
    end;

    ResultOutput.Add(rtfParaStr+'No. of sequences used: '+IntToStr(TotalSeqs));
    FreqXls.WriteKeypair('No. of sequences used:', TotalSeqs, 1);
    ResultOutput.Add(rtfParaStr+'Sequences used');
    FreqXls.WriteLine('Sequences used', 1);
    for SeqI:=0 to FNoOfTaxa-1 do
      if FOtuInfos[SeqI].IsUsed then
      begin
        ResultOutput.Add(rtfParaStr+'  '+FOtuInfos[SeqI].Name);
        FreqXls.WriteLine('  ' + FOtuInfos[SeqI].Name, 1);
      end;
    if TotalSeqs > 1 then
    begin
      ResultOutput.Add(rtfParaStr+'All frequencies are averages (rounded) over all taxa.');
      FreqXls.Add('All frequencies are averages (rounded) over all taxa.');
      FreqXls.WriteLine(0, 'A', '', True);
    end;
    ResultOutput.Add(rtfParaStr+'ii = Identical Pairs');
      FreqXls.Add('ii = Identical Pairs');
      FreqXls.WriteLine(0, 'A', '', True);
    ResultOutput.Add(rtfParaStr+'si = Transitionsal Pairs');
      FreqXls.Add('si = Transitionsal Pairs');
      FreqXls.WriteLine(0, 'A', '', True);
    ResultOutput.Add(rtfParaStr+'sv = Transversional Pairs');
      FreqXls.Add('sv = Transversional Pairs');
      FreqXls.WriteLine(0, 'A', '', True);
    ResultOutput.Add(rtfParaStr+'R  = si/sv');
      FreqXls.Add('R  = si/sv');
      FreqXls.WriteLine(0, 'A', '', True);
    CurStr := 'Domain   '+
              FixedLenBlankStr+'ii'+
              FixedLenBlankStr+'si'+
              FixedLenBlankStr+'sv'+
              FixedLenBlankStr+'R ';
       FreqXls.Add('Domain');
       FreqXls.AddBlankCell;
       FreqXls.Add('ii');
       FreqXls.Add('si');
       FreqXls.Add('sv');
       FreqXls.Add('R');

    for i:=1 to 4 do
      for j:=1 to 4 do
        if (IsDirectional) or (j >= i) then
        begin
          CurStr := CurStr +FixedLenBlankStr+cBase[i]+cBase[j];
          FreqXls.Add(cBase[i]+cBase[j]);
        end;

    CurStr := CurStr + ' Total  Domain Info';
    ResultOutput.Add(rtfParaStr+rtfBoldStr+CurStr+rtfPlainStr);
    CurStr := EmptyStr;
    FreqXls.Add('Total');
    FreqXls.Add('Domain Info');
    FreqXls.WriteLine();

    //===
    for DomainI :=0 to MyDomains.Count-1 do
    begin
      Inc(SitesDone);
      if (SitesPerPercent > 0) and ((SitesDone mod SitesPerPercent) = 0) then
        if DoProgressCheckCancel(SitesDone div SitesPerPercent, 'Preparing to display results') then
          raise EAbort.Create('user cancelled');

      // write the domain name
      CurDomain := MyDomains[DomainI];
      if CurDomain = nil then
         DomainNameStr :='Sites with no domain definition'
      else
      begin
        DomainNameStr := CurDomain.Name;
        if length(CurDomain.GeneName)  > 0 then
          DomainNameStr := DomainNameStr + ' ('+CurDomain.GeneName+')';
      end;

      AFreq       := FreqInfo[DomainI];
      DoubleArr   := AFreq.AlleleFreq;

      // write the average first over all 3 pos if applicable
      CurStr := IntToStrWidth(DomainI+1, 4)+'. Avg';

      FreqXls.Add(IntToStr(DomainI+1));
      FreqXls.Add('Avg');
      for i:=0 to 3 do
        for j:=0 to 3 do
        if (CurDomain <> nil) and CurDomain.IsCoding then
           Matrix[i][j] := (DoubleArr[i*4+j]+DoubleArr[16+i*4+j]+DoubleArr[32+i*4+j])*2/TotalSeqs/(TotalSeqs-1)
        else
           Matrix[i][j] := DoubleArr[i*4+j]*2/TotalSeqs/(TotalSeqs-1);
      WriteValues;
      ResultOutput.Add(rtfParaStr+CurStr+'  '+DomainNameStr);
      CurStr := EmptyStr;

      FreqXls.Add(DomainNameStr);
      FreqXls.WriteLine();
      if (CurDomain <> nil) and CurDomain.IsCoding then
      begin
        // write three positions separately
        CurStr := '    . 1st';
        FreqXls.AddBlankCell;
        FreqXls.Add('1st');
        for i:=0 to 3 do
          for j:=0 to 3 do
            Matrix[i][j] := DoubleArr[i*4+j]*2/TotalSeqs/(TotalSeqs-1);
        WriteValues;
        ResultOutput.Add(rtfParaStr+CurStr+'  1st Pos '+DomainNameStr);
        FreqXls.Add('1st Pos '+DomainNameStr);
        FreqXls.WriteLine();
        CurStr := '    . 2nd';
        FreqXls.AddBlankCell;
        FreqXls.Add('2nd');
        for i:=0 to 3 do
          for j:=0 to 3 do
            Matrix[i][j] := DoubleArr[16+i*4+j]*2/TotalSeqs/(TotalSeqs-1);
        WriteValues;
        ResultOutput.Add(rtfParaStr+CurStr+'  2nd Pos '+DomainNameStr);
        FreqXls.Add('2nd Pos ' + DomainNameStr);
        FreqXls.WriteLine();
        CurStr := '    . 3rd';
        FreqXls.AddBlankCell;
        FreqXls.Add('3rd');
        for i:=0 to 3 do
          for j:=0 to 3 do
            Matrix[i][j] := DoubleArr[32+i*4+j]*2/TotalSeqs/(TotalSeqs-1);
        WriteValues;
        ResultOutput.Add(rtfParaStr+CurStr+'  3rd Pos '+DomainNameStr);
        FreqXls.Add('3rd Pos '+DomainNameStr);
        FreqXls.WriteLine();
        CurStr := EmptyStr;
      end;
    end;  // end for all domains

    ResultOutput.Add(rtfParaStr+CurStr);
    ResultOutput.Add(rtfEndStr);
    FreqXls.ApplyFormat(5, 1, 5, FreqXls.OutputLineLvl[0], '0.0'); // E1:E1 + OutputLineLvl[0]
 except
   On E: Exception do
   begin
     FreqXls.HadError;
     raise Exception.Create(E.Message);
   end;
 end;
 finally
   if FreqInfo <> nil then
   begin
     if FreqInfo.Count > 0 then
      for i:=0 to FreqInfo.Count-1 do
        if FreqInfo[i] <> nil then
         TLocusPopFreqInfo(FreqInfo[i]).Free;
     FreqInfo.Free;
   end;
   MyDomains.Free;
 end;
end;


function TD_InputSeqData.WriteExportDataToFile(Options: TSeqExportOptions;
  SaveTo: String; ResultOutput: TStringList; xlFile: TExcelWrite=nil): Boolean; //False if we have an error
var
  ColsHighlighted: Boolean;
  doBreak: Boolean;
  NextStartSite, i, j: Integer;
  TheFirstSeq: Integer; // the first sequence written
  MaxLabelLen: Integer; // counts full length, including gp if needed to be written
  MaxSitesCount: Integer; // holds the actual number of sites to write
  MaxTaxaCount: Integer; // actual number of taxa to write
  DigitNum: Integer;
  LastValidSiteNum: Integer = -1;
  OutFile: TextFile;
  CurrentStr, TempStr: String;
  PrevDomainInfo: TDomainInfo = nil;
  XDomain: Pointer = nil;
  ExpD : TExcelWrite = nil;
  PDone: Integer;

  FirstIncludedSite: Integer;
  Accumulate : Integer;
  AClusterSize: Integer;
  ASitesPerLine: Integer;
  StartSiteForLabels, AMaxSiteNumStrLen: Integer;

  CurDomainStartSite, CurStartSite: Integer;

  ACodonOptionsValid    : Boolean;
  AInclude1stPos        : Boolean;
  AInclude2ndPos        : Boolean;
  AInclude3rdPos        : Boolean;
  AInclude0thPos        : Boolean;
  AIncludeMissData      : Boolean;
  AIncludeGapData       : Boolean;
  AIncludeGps           : Boolean;
  AIncludeSps           : Boolean;
  AIncludePops          : Boolean;
  AIncludeHighlighted   : Boolean;
  AIncludeUnHighlighted : Boolean;
  AUseIdenSymbol        : Boolean;
  AWriteCodonByCodon    : Boolean;
  AWriteNumAtEnd        : Boolean;
  AWriteNumAtTop        : Boolean;
  AInterleaved          : Boolean;
  AIsPAUP4Format        : Boolean;
  AIsPAUP3Format        : Boolean;
  AIsMEGAFormat         : Boolean;
  AIsPhylipFormat       : Boolean;
  AIsCSVFormat          : Boolean;
  AIsFastaFormat        : Boolean;
  doXl: Boolean;
  tempSeq: PChar = nil;
  tsi: Int64; { tempSeq index}

  //=== for writing labels
  function ComputeMaxLabelLen: Integer;
  var
    i:      Integer;
    ALabel: String;
    aGeoInfo: TGeographicalInfo = nil;
  begin
    MaxLabelLen := 0;
    for i:=0 to DispTaxaList.Count-1 do
    begin
      if not SeqUsed[i] then
        continue;
      ALabel := TaxaName[i];
      aGeoInfo := GetGeographicalInfo(i);
      if Assigned(aGeoInfo) then
        if aGeoInfo.LabelNeedsCommandString then
          ALabel := aLabel + aGeoInfo.MegFileCommandString;
      ALabel := OtuNameToMegaStr(ALabel);
      if Length(ALabel) > MaxLabelLen then
        MaxLabelLen := Length(ALabel);
    end;
    Result := MaxLabelLen;
  end;

  function StripIllegalNexusSymbols(TaxaLabel : String) : String;
  const
    illegalNexusChars: array [0..18] of Char = ('(',')','{','}','[',']','/','\',',',';',':','=','*','"','''','`','<','>','^');
  var
    i : Integer;
  begin
    Result := '';
    if TaxaLabel = EmptyStr then
      Exit;
    for i := Low(illegalNexusChars) to High(illegalNexusChars) do
    begin
      While Pos(illegalNexusChars[i],TaxaLabel) > 0 do
        TaxaLabel[Pos(illegalNexusChars[i],TaxaLabel)] := '_';
    end;
    Result := TaxaLabel;
  end;

  procedure WriteLabel(SeqNum: Integer; ExportOptions: TSeqExportOptions);
  var
    aGeoInfo: TGeographicalInfo = nil;
    ALabel: String;
  begin
    if ExportOptions.IsStandardNames then
      ALabel := 'TX' + IntToStr(SeqNum)
    else
      ALabel := TaxaName[SeqNum];
    if doXl then
      ExpD.Add(ALabel);
    if AIncludeGps or AIncludeSps or AIncludePops then
    begin
      aGeoInfo := GetGeographicalInfo(SeqNum);
      if Assigned(aGeoInfo) then
      begin
        if aGeoInfo.LabelNeedsCommandString then
          ALabel := ALabel + aGeoInfo.MegFileCommandString;
      end;

      if doXl then
      begin
        if Assigned(aGeoInfo) then
        begin
          ExpD.Add(aGeoInfo.Group);
          ExpD.Add(aGeoInfo.Species);
          ExpD.Add(aGeoInfo.Population);
          ExpD.Add(aGeoInfo.Continent);
          ExpD.Add(aGeoInfo.Country);
          ExpD.Add(aGeoInfo.City);
          ExpD.Add(aGeoInfo.Year);
          ExpD.Add(aGeoInfo.Month);
          ExpD.Add(aGeoInfo.Day);
          ExpD.Add(aGeoInfo.ReferenceTime);
        end
        else
          ExpD.AddBlankCells(10);
      end;
    end;

    ALabel := OtuNameToMegaStr(ALabel);
    if ExportOptions.IsPAUP4Format then
      ALabel := StripIllegalNexusSymbols(ALabel); 
    if Length(ALabel) > MaxLabelLen then
      CurrentStr := CurrentStr + Copy(ALabel, 1, MaxLabelLen) + ' '
    else if MaxLabelLen > Length(ALabel) then
      CurrentStr := CurrentStr + ALabel+BlankString(MaxLabelLen-1-Length(ALabel)) + ' '
    else
      CurrentStr := CurrentStr + ALabel;
  end;  // end of writing labels

  // Tests the meg0xxBase attribute using ED_IncludeCodonPos
  function  IsUsedCodPos(ASite:Integer):Boolean;
  begin
    Result := (AInclude1stPos and (meg1stBase in SiteAttrArray[ASite])) or
              (AInclude2ndPos and (meg2ndBase in SiteAttrArray[ASite])) or
              (AInclude3rdPos and (meg3rdBase in SiteAttrArray[ASite]));
  end;

  //---------- function to find if a site needs to be used
  function IsUsed(ASite: Integer): Boolean;
  begin
    Result := SiteUsed[ASite]; // after integration with main
    if not Result then
      Exit;
    if Options.ExportFullCodons then
    begin
      Assert(FIsCoding, 'full codons option should only be enabled for coding data');
      if AIncludeHighlighted and AIncludeUnHighlighted then
        Exit;
      if AIncludeHighlighted then
      begin
        if meg1stBase in SiteAttrArray[aSite] then
          Result := HasAttribute(ASite, VS_SeqDataExplorer.CurAttrDisp) or HasAttribute(ASite + 1, VS_SeqDataExplorer.CurAttrDisp) or HasAttribute(ASite + 2, VS_SeqDataExplorer.CurAttrDisp)
        else if meg2ndBase in SiteAttrArray[aSite] then
          Result := HasAttribute(ASite - 1, VS_SeqDataExplorer.CurAttrDisp) or HasAttribute(ASite, VS_SeqDataExplorer.CurAttrDisp) or HasAttribute(ASite + 1, VS_SeqDataExplorer.CurAttrDisp)
        else if meg3rdBase in SiteAttrArray[aSite] then
          Result := HasAttribute(ASite - 2, VS_SeqDataExplorer.CurAttrDisp) or HasAttribute(ASite - 1, VS_SeqDataExplorer.CurAttrDisp) or HasAttribute(ASite, VS_SeqDataExplorer.CurAttrDisp)
        else
          Result := False;
      end;
      if AIncludeUnHighlighted then
      begin
        if meg1stBase in SiteAttrArray[aSite] then
          Result := (not HasAttribute(ASite, VS_SeqDataExplorer.CurAttrDisp)) or (not HasAttribute(ASite + 1, VS_SeqDataExplorer.CurAttrDisp)) or (not HasAttribute(ASite + 2, VS_SeqDataExplorer.CurAttrDisp))
        else if meg2ndBase in SiteAttrArray[aSite] then
          Result := (not HasAttribute(ASite - 1, VS_SeqDataExplorer.CurAttrDisp)) or (not HasAttribute(ASite, VS_SeqDataExplorer.CurAttrDisp)) or (not HasAttribute(ASite + 1, VS_SeqDataExplorer.CurAttrDisp))
        else if meg3rdBase in SiteAttrArray[aSite] then
          Result := (not HasAttribute(ASite - 2, VS_SeqDataExplorer.CurAttrDisp)) or (not HasAttribute(ASite - 1, VS_SeqDataExplorer.CurAttrDisp)) or (not HasAttribute(ASite, VS_SeqDataExplorer.CurAttrDisp))
        else
          Result := False;
      end;
      Exit;
    end;

    if ACodonOptionsValid then
    begin
      if (not AInclude1stPos) and (meg1stBase in SiteAttrArray[ASite]) then
        Result := False
      else if (not AInclude2ndPos) and (meg2ndBase in SiteAttrArray[ASite]) then
        Result := False
      else if (not AInclude3rdPos) and (meg3rdBase in SiteAttrArray[ASite]) then
        Result := False
      else if (not AInclude0thPos) and (meg0thBase in SiteAttrArray[ASite]) then
        Result := False
    end;
    if not Result then
      Exit;

    if (not AIncludeGapData) and (megGap in SiteAttrArray[ASite]) then
      Result := False
    else if (not AIncludeMissData) and (megMissing in SiteAttrArray[ASite]) then
      Result := False;
    if not Result then
      Exit;

    if AIncludeHighlighted and AIncludeUnhighlighted then
    begin
    end
    else
    begin
      if AIncludeHighlighted then
      begin
        if not HasAttribute(ASite, VS_SeqDataExplorer.CurAttrDisp) then
        begin
          Result := False;
          Exit;
        end;
      end
      else if AincludeUnhighlighted then
      begin
         if HasAttribute(ASite, VS_SeqDataExplorer.CurAttrDisp) then
         begin
           Result := False;
           Exit;
         end;
      end;
    end;
    Result := True;
  end;

  //======= write the sites numbers on top
  procedure WriteSiteNum(StartSite: Integer);
  var
    si: Integer;
  begin
    for si := StartSite to FNoOfSites-1 do  // PChar should hold
    begin
      if not IsUsed(si) then  // no point going on
        Continue;
      // still to finish
    end;
  end; // end of writing sequence

  //======= write the sequence or site: digits=0 means sequence; else digit is from top to bottom
  function WriteSequence(Digit: Integer; SeqNum, StartSite, TFS: Integer; var EndValidSite: Integer): Integer; // returns the last site written (or the end of sites)
  var
    TempArray: PChar;
    FSA: PChar;
    ADigitStr: String;
    MaxDigits, si, newSW: Integer;
    Highlight: TColor;
  begin
    FSA := nil;
    TempArray := nil;
    if SeqNum >= 0 then
    begin
      TempArray := Sequence[SeqNum];
      FSA := Sequence[TFS];
    end;
    NewSW := 0;
    MaxDigits := Length(IntToStr(FNOOfSites));

    tsi := 0;
    for si := StartSite to FNoOfSites-1 do
    begin
      if VS_SeqDataExplorer.IsHighlighted(si) and ColsHighlighted then
        Highlight := VS_SeqDataExplorer.HighlightColor
      else
        Highlight := clWhite;

      if not IsUsed(si) then  // no point going on
        Continue;
      if (DomainMark[StartSite] <> DomainMark[si]) and (not AIsFastaFormat) then
      begin
        Result := si-1;  // si-th is
        if Assigned(tempSeq) and (tsi > 0) then
        begin
          tempSeq[tsi] := #0;
          if CurrentStr <> EmptyStr then
            CurrentStr := CurrentStr + StrPas(tempSeq)
          else
            CurrentStr := StrPas(tempSeq);
        end;
        Exit;
      end;

      //=== write the character needed
      if Digit < 0 then // site labels
      begin
        tempSeq[tsi] := SiteLabel[si];
        inc(tsi);
        if (VS_SeqDataExplorer.DispColorItem) and (not ColsHighlighted) then
          Highlight := GetCellColor(SiteLabel[si]);
        if doXl then
          ExpD.Add(SiteLabel[si], Highlight);
      end
      else if Digit = 0 then // sequence
      begin
        if (SeqNum = TFS) or (not AUseIdenSymbol) then
        begin
          tempSeq[tsi] := TempArray[si];
          inc(tsi);
          if (VS_SeqDataExplorer.DispColorItem) and (not ColsHighlighted) then
            Highlight := GetCellColor(TempArray[si]);
          if doXl then
            ExpD.Add(TempArray[si], Highlight);
        end
        else
          if (UpCase(FSA[si]) = Upcase(FGapSym)) or
             (UpCase(FSA[si]) = Upcase(FMissSym)) then
             begin
                 tempSeq[tsi] := TempArray[si];
                 inc(tsi);
                 if (VS_SeqDataExplorer.DispColorItem) and (not ColsHighlighted) then
                   Highlight := GetCellColor(TempArray[si]);
                 if doXl then
                   ExpD.Add(TempArray[si], Highlight);
             end
          else
              if TempArray[si] = FSA[si] then
              begin
                tempSeq[tsi] := FIdenSym;
                inc(tsi);
                if (VS_SeqDataExplorer.DispColorItem) and (not ColsHighlighted) then
                  Highlight := GetCellColor(TempArray[si]);
                if doXl then
                  ExpD.Add(FIdenSym, Highlight);
              end
              else
              begin
                tempSeq[tsi] := TempArray[si];
                inc(tsi);
                if (VS_SeqDataExplorer.DispColorItem) and (not ColsHighlighted) then
                  Highlight := GetCellColor(TempArray[si]);
                if doXl then
                  ExpD.Add(TempArray[si], Highlight);
              end;
        EndValidSite := si;
      end
      else  // writing site number on top
      begin
        ADigitStr := IntToStr(si+1);
        ADigitStr := BlankString(MaxDigits-Length(ADigitStr))+ADigitStr;
        tempSeq[tsi] := aDigitStr[Digit];
        inc(tsi);
        if doXl then
        begin
          if AIncludeGps and (si = StartSite) then
            ExpD.AddBlankCell();
    	  ExpD.Add(ADigitStr[Digit]);
        end;
      end;

      Inc(NewSW);
      Inc(Accumulate);

      // if it is about full codons then insert blank after 3rd pos
      if AIsPhylipFormat then
      begin
        if Accumulate = 10 then
        begin
          tempSeq[tsi] := ' ';
          inc(tsi);
          Accumulate := 0;
        end;
      end
      else if AWriteCodonByCodon and (meg3rdBase in SiteAttrArray[si]) then
      begin
         if not (AIsPAUP4Format or AIsPAUP3Format) then
         begin
           tempSeq[tsi] := ' ';
           inc(tsi);
         end;
         Accumulate := 0;
      end
      else if Accumulate = AClusterSize then  // Accumulate is automatically set to 0 in between domains
      begin
        if not (AIsPAUP4Format or AIsPAUP3Format or AIsFastaFormat) then
        begin
          tempSeq[tsi] := ' ';
          inc(tsi);
        end;

        Accumulate := 0;
      end;

      if NewSW = ASitesPerLine then
      begin
        Result := si;
        if tempSeq <> nil then
        begin
          tempSeq[tsi] := #0;
          if CurrentStr <> EmptyStr then
            CurrentStr := CurrentStr + StrPas(tempSeq)
          else
            CurrentStr := StrPas(tempSeq);
        end;
        tsi := si;
        Exit;
      end;
    end;
    PDone := ((j+1) * 100) div MaxTaxaCount+1;
    Result := FNoOfSites-1;  // if flow falls here than all sites written
    if tempSeq <> nil then
    begin
      tempSeq[tsi] := #0;
      if CurrentStr <> EmptyStr then
        CurrentStr := CurrentStr + StrPas(tempSeq)
      else
        CurrentStr := StrPas(tempSeq);
    end;
  end; // end of writing sequence

  procedure WriteToOutFile(aLine: String);
  begin
    if not doXl then
      WriteLn(OutFile, aLine);
  end;

begin
  doXl := Options.IsSpreadsheetFormat;
  doBreak := False;
  Result := False;
  ResultOutput.Clear;
  if VS_SeqDataExplorer.LabelledItem or VS_SeqDataExplorer.ConstantItem or VS_SeqDataExplorer.VariableItem or VS_SeqDataExplorer.ParsimInfoItem or VS_SeqDataExplorer.SingletonItem or VS_SeqDataExplorer.ZeroFoldItem or VS_SeqDataExplorer.TwoFoldItem or VS_SeqDataExplorer.FourFoldItem then
    ColsHighlighted := true;
 try
   try
    if not doXl then
    begin
      AssignFile(OutFile, SaveTo);
      Rewrite(OutFile);
    end;
    GetMem(tempSeq, SizeOf(Char)*FNoOfSites + 1);
    // get the codon/gaps/miss options first
    ACodonOptionsValid    := (Options.IsChooseBases and (not Options.ExportFullCodons));
    AInclude1stPos        := not Options.IsRemove1stBase;
    AInclude2ndPos        := not Options.IsRemove2ndBase;
    AInclude3rdPos        := not Options.IsRemove3rdBase;
    AInclude0thPos        := not Options.IsRemove0thBase;
    AIncludeMissData      := not Options.IsRemoveMissData;
    AIncludeGapData       := not Options.IsRemoveGapData;
    AWriteNumAtEnd        := Options.IsWriteSiteNumAtEnd;
    AWriteNumAtTop        := Options.IsWriteSiteNumAtTop;
    AInterleaved          := Options.IsInterleaved;
    AIncludeHighlighted   := Options.IsExportHighlightedSites;
    AIncludeUnhighlighted := Options.IsExportUnHighlightedSites;
    AUseIdenSymbol        := VS_SeqDataExplorer.DispUseIdenSymbolItem and (not Options.IsFastaFormat);

    AIsPAUP4Format        := Options.IsPAUP4Format;
    AIsPAUP3Format        := Options.IsPAUP3Format;
    AIsMEGAFormat         := Options.IsMEGAFormat;
    AIsPhylipFormat       := Options.IsPhylipFormat;
    AIsCSVFormat          := Options.IsCSVFormat;
    AIsFastaFormat        := Options.IsFastaFormat;

    AWriteCodonByCodon := Options.IsCodonByCodonWriting;
    if AWriteCodonByCodon then
      AClusterSize := 3
    else
      AClusterSize := 10;

    ASitesPerLine := Options.SitesPerLine;
    if AWriteCodonByCodon then
      if (ASitesPerLine mod 3) <> 0 then
        ASitesPerLine := (ASitesPerLine div 3)*3;

    // now we are ready to write
    // find the number of sites to be written
    MaxSitesCount := 0;
    for i := 0 to FNoOfSites-1 do
      if IsUsed(i) then Inc(MaxSitesCount);

    if MaxSitesCount = 0 then
    begin
      RaiseErrorMessage(HC_Unexpected_Error, 'Error: There are 0 selected sites in the currently chosen data subset.' +LineEnding+ 'If there is data then check your export settings for excluding gaps and ambiguous data.  This may cause there to be no valid sites.');
      Exit;
    end;
    if doXl then
    begin
      if Assigned(xlFile) then
        ExpD := xlFile
      else
        ExpD := TExcelWrite.Create(nil, 'Sequence Data');
    end;
    if doXl then
    begin
      ExpD.IsXLS := True;
      ExpD.AddWorksheet('Info');
    end;

    AMaxSiteNumStrLen := Length(IntToStr(FNoOfSites));

    MaxTaxaCount := 0;
    for i:= 0 to DispTaxaList.Count-1 do
       if SeqUsed[i] then
         Inc(MaxTaxaCount);

    if (ASitesPerLine > MaxSitesCount) and (not AIsMEGAFormat) and (not Options.IsSpreadsheetFormat) then
      AInterleaved := False;

    XDomain := nil;
    if AIsMEGAFormat then
       for i := 0 to FNoOfSites-1 do
        if IsUsed(i) and (DomainMark[i] <> nil) then
        begin
          if (XDomain <> nil) and (DomainMark[i] <> XDomain) then
          begin
             AInterleaved := True; // force it
             break;
          end
          else
            XDomain := DomainMark[i];
        end;
    if not ((AisMEGAFormat) or (Options.IsSpreadsheetFormat)) then
    begin
      AIncludeGps := False;
      AIncludeSps := False;
      AIncludePops := False;
    end;

    //===write header for every output type
    if AIsMEGAFormat then
    begin
      WriteToOutFile('#MEGA');
      WriteToOutFile('!Title ' + Options.FileTitle+';');
      if Length(Options.FileDescription) > 0 then
        WriteToOutFile('!Description ' + Options.FileDescription+';');
      AIncludeGps := True;  // if there is any groupname
      AIncludeSps := True;
      AIncludePops := True;
      ComputeMaxLabelLen;
      //=== write format statement
      WriteToOutFile('!Format');

      //=== write data type and code table
      CurrentStr := SeqOffSet;
      if Options.IsNucData then
        CurrentStr := CurrentStr + 'DataType=Nucleotide'
      else
        CurrentStr := CurrentStr + 'DataType=Protein';

      if AWriteCodonByCodon then
        CurrentStr := CurrentStr + ' CodeTable='+OtuNameToMegaStr(FCodeName);
      WriteToOutFile(CurrentStr);
      // no of taxa;
      CurrentStr := SeqOffset+'NSeqs='+IntToStr(MaxTaxaCount)+' NSites='+IntToStr(MaxSitesCount);
      WriteToOutFile(CurrentStr);
      // add no of seqs and sites; identical etc. symbols
      CurrentStr := SeqOffset+'Identical='+FIdenSym+ ' Missing='+FMissSym+' Indel='+FGapSym+';';
      WriteToOutFile(CurrentStr);
      WriteToOutFile(EmptyStr);
      // write here about the datasubset selected
    end;

    if AIsFastaFormat then
    begin
      ComputeMaxLabelLen;
      Inc(MaxLabelLen); // Adding one exta to force a space between name and data.
    end;

    if AIsPAUP3Format or AIsPAUP4Format then
    begin
      WriteToOutFile('#NEXUS');
      WriteToOutFile('[ Title '+ Options.FileTitle+']');
      if Length(Options.FileDescription) > 0 then
        WriteToOutFile('[Description ' + Options.FileDescription+']');
      ComputeMaxLabelLen;
      if MaxLabelLen > 32 then
        MaxLabelLen := 32; // force it to be less than 32

      if AIsPAUP3Format then
      begin
        WriteToOutFile('begin data;');
        WriteToOutFile(SeqOffset+'dimensions ntax='+IntToStr(MaxTaxaCount)+
                                     ' nchar='+ IntToStr(MaxSitesCount)+';');
        CurrentStr := SeqOffset+'format'+' missing='+FMissSym+' gap='+FGapSym+' matchchar='+FIdenSym;
        if Options.IsNucData then CurrentStr := CurrentStr + ' datatype=nucleotide'
                             else CurrentStr := CurrentStr + ' datatype=protein';
        if AInterleaved then
          CurrentStr := CurrentStr+' interleave=yes';
         CurrentStr := CurrentStr + ';';
        WriteToOutFile(CurrentStr);
        WriteToOutFile(SeqOffset+'matrix');
      end
      else
      begin
        // write taxa info
        WriteToOutFile('begin taxa;');
        WriteToOutFile(SeqOffset+'dimensions ntax= '+IntToStr(MaxTaxaCount)+';');
        WriteToOutFile(SeqOffset+'taxlabels');
        for j := 0 to DispTaxaList.Count-1 do
        begin
           if not SeqUsed[j] then continue;
           //======= write the label
           // Joel: We need to clean up the label to make sure it does not contain any of the symbols disallowed by the NEXUS spec
           CurrentStr := EmptyStr;
           WriteLabel(j, Options);
           WriteToOutFile(Seqoffset+SeqOffset+StripIllegalNexusSymbols(CurrentStr));
        end;
        WriteToOutFile(';');
        WriteToOutFile('end;');
        // write characters
        WriteToOutFile('begin characters;');
        WriteToOutFile(SeqOffset+'dimensions nchar= '+IntToStr(MaxSitesCount)+';');
        CurrentStr := 'format'+' missing='+FMissSym+' gap='+FGapSym+' matchchar='+FIdenSym;
        if Options.IsNucData then CurrentStr := CurrentStr + ' datatype=nucleotide'
                             else CurrentStr := CurrentStr + ' datatype=protein';
        if AInterleaved then
           CurrentStr := CurrentStr + ' interleave=yes';
        CurrentStr := CurrentStr + ';';
        WriteToOutFile(SeqOffset+CurrentStr);
        WriteToOutFile(SeqOffset+'matrix');
      end;
    end;

    if AIsPHYLIPFormat then
    begin
      MaxLabelLen := 10;
      WriteToOutFile(IntToStr(MaxTaxaCount)+' '+IntToStr(MaxSitesCount));
    end;

    if doXl then
    begin
      AIncludeGps := VS_SeqDataExplorer.DispGpNamesItem;  // if there is any groupname
      ExpD.WriteKeypair('Title', Options.FileTitle, 1);
      if Length(Options.FileDescription) > 0 then
      begin
        ExpD.WriteKeypair('Description', Options.FileDescription, 1);
      end;
      ExpD.WriteLine('Format', 1);
      if Options.IsNucData then
      begin
        ExpD.Add('DataType');
        ExpD.Add('Nucleotide');
      end
      else
      begin
        ExpD.Add('DataType');
        ExpD.Add('Protein');
      end;
      ExpD.WriteLine(1);
      if AWriteCodonByCodon then
      begin
        ExpD.WriteKeypair('CodeTable', OtuNameToMegaStr(FCodeName), 1);
      end;
      ExpD.WriteLine(IntToStr(MaxTaxaCount), 1);

      ExpD.WriteKeypair('Number Sites', MaxSitesCount, 1);
      // add no of seqs and sites; identical etc. symbols
      ExpD.WriteKeypair('Identical', FIdenSym, 1);
      ExpD.WriteKeypair('Missing', FMissSym, 1);
      ExpD.WriteKeypair('Indel', FGapSym, 1);
    end;

    if AIsCSVFormat then
      AIncludeGps := VS_SeqDataExplorer.DispGpNamesItem;

    // Done adding headers dependant on Export Option
    // At this point, we have to identify the first included site
    FirstIncludedSite := -1;
    for i := 0 to FNoOfSites-1 do
      if IsUsed(i) then
      begin
        FirstIncludedSite := i;
        break;
      end;
    // Now Write the data;
    // To make it sensitive to the domain name
    CurrentStr := EmptyStr;

    // interleaved option should work in the context of a given domain
    // if non-interleaved then a domain is written in its entirety in one place
    CurDomainStartSite := FirstIncludedSite;
    NextStartSite      := CurDomainStartSite;
    PrevDomainInfo := nil;
    while CurDomainStartSite <= (FNoOfSites-1) do
    begin
      if DomainMark[CurDomainStartSite] = nil then
      begin
        if PrevDomainInfo <> nil then  // if we just wrote a domain then delimit it
        begin
          WriteToOutFile('!domain='+OtuNameToMegaStr(PrevDomainInfo.Name)+' property=domainend;');
          if doXl then
          begin
            ExpD.Add('domain');
            ExpD.Add(OtuNameToMegaStr(PrevDomainInfo.Name));
            ExpD.Add('Property');
            ExpD.Add('domainend');
            ExpD.WriteLine();
          end;
        end;
        PrevDomainInfo := nil;
      end;
      if not AIsFastaFormat then
        WriteToOutFile(EmptyStr);

      if DomainMark[CurDomainStartSite] <> nil then
      begin
        if PrevDomainInfo <> DomainMark[CurDomainStartSite] then
        begin
          PrevDomainInfo := DomainMark[CurDomainStartSite];
          with DomainMark[CurDomainStartSite] do
          begin
            CurrentStr := '!Domain='+OtuNameToMegaStr(Name);
            TempStr := 'Domain: ' + Name + '  ';
            if Length(GeneName)> 0 then
            begin
              CurrentStr := CurrentStr + ' Gene='+OtuNameToMegaStr(GeneName);
              TempStr := TempStr + 'Gene: ' + GeneName + '  ';
            end;
            if IsCoding and AWriteCodonByCodon then
            begin
              CurrentStr := CurrentStr+ ' property=Coding CodonStart='+IntToStr(CodonStart+1);
              TempStr := TempStr + 'Coding Codon Start: ' + IntToStr(CodonStart+1);
              AClusterSize := 3;
            end
            else AClusterSize := 10;
            CurrentStr := CurrentStr + ';';
            if doXl then
            begin
              ExpD.Add(TempStr);
              ExpD.WriteLine(0, 'A', '', True);
              ExpD.ColumnSize[1] := 17;
            end;
            if AIsMEGAFormat then
              WriteToOutFile(CurrentStr)
            else if AIsPAUP3Format or AIsPAUP4Format then
              WriteToOutFile('['+CurrentStr+']'); // no writing for PHYLIP
          end;
        end;
      end
      else
        AClusterSize := 10;

      // where do we write the labels
      TheFirstSeq:=-1;
      StartSiteForLabels := CurDomainStartSite;
      for j := 0 to DispTaxaList.Count-1 do
      begin
        if not SeqUsed[j] then
          continue;
        if TheFirstSeq < 0 then
        begin
           TheFirstSeq := j;
           //at this point, we can write the site numbers
           if AWriteNumAtTop and AInterleaved then
           begin
             NextStartSite := CurDomainStartSite;
               for DigitNum:= 1 to length(IntToStr(FNOOfSites+1)) do  // This for loop writes the site numbers above the sites in multiple rows
               begin
                 Accumulate := 0;
                 if AIsPAUP3Format or AIsPAUP4Format then
                   CurrentStr := BlankString(MaxLabelLen)
                 else
                   CurrentStr := BlankString(MaxLabelLen+1);// for # sign
                 if doXl then
                   ExpD.AddBlankCell; // Moves the site numbers forward one cell because the taxa names previously threw it off.
                 WriteSequence(DigitNum, -1, NextStartSite, TheFirstSeq, LastValidSiteNum);
                 if Length(Trim(CurrentStr)) > 0 then
                 begin
                   WriteToOutFile('['+CurrentStr+']');
                   if doXl then
                     ExpD.WriteLine();
                 end
                 else
                   if doXl then
                     ExpD.Empty;
             end;
             CurrentStr := EmptyStr;
           end;
        end;

        //======= now write the sequence
        NextStartSite := CurDomainStartSite;
        StartSiteForLabels := NextStartSite;

        //======= write the label
        if AIsMEGAFormat then
          CurrentStr := '#'
        else
          CurrentStr := EmptyStr;
        if AIsFastaFormat then
          CurrentStr := '>';

        WriteLabel(j, Options);
        if not AInterleaved then
        begin
          if doXl then
            ExpD.WriteLine();
          WriteToOutFile(CurrentStr);
          CurrentStr := EmptyStr;
        end
        else if AIsPhylipFormat then
        begin
          if NextStartSite > FirstIncludedSite then
            CurrentStr := '          ';  // 10 characters are included
        end
        else
          if AIsMEGAFormat or AIsPAUP3Format or AIsPAUP4Format  then
            CurrentStr := CurrentStr + ' '; // just add a blank

        // sequence follows
        repeat
          Accumulate := 0;
          CurStartSite := NextStartSite;
          NextStartSite := WriteSequence(0, j, NextStartSite, TheFirstSeq, LastValidSiteNum)+1;
          if AWriteNumAtEnd and (not AIsPhylipFormat) and AInterleaved then
          begin
            CurrentStr := CurrentStr + ' ['+ IntToStrWidth(LastValidSiteNum+1,AMaxSiteNumStrLen)+']';
            if doXl then
              ExpD.Add(' ['+ IntToStrWidth(LastValidSiteNum+1,AMaxSiteNumStrLen)+']');
          end;
          WriteToOutFile(CurrentStr);
          CurrentStr := EmptyStr;
          if doXl then
            ExpD.WriteLine();
          if NextStartSite >= FNoOfSites then
            break;
          if AInterleaved then
            break;
          doBreak := ((not AIsFastaFormat) and (DomainMark[CurStartSite] <> DomainMark[NextStartSite]));
        until doBreak;

        if Assigned(ProgressCheckCancelFunc) then
        begin
          if not AInterleaved then
            PDone := Round(j*100/DispTaxaList.Count)
          else
            PDone := Round(CurStartSite*100/MaxSitesCount);
          if ProgressCheckCancelFunc(PDone, 'Exporting...') then
            raise EAbort.Create('Aborting sequence data export');
        end;
      end; // for all the sequences; writing noninterleaved

      // now write domain labels
      if (AIsMegaFormat or Options.IsSpreadsheetFormat) and (FDomainMarks.NoOfLabelledSites > 0) then
      begin
        Accumulate := 0;
        CurrentStr := '!Label'+BlankString(MaxLabelLen+1-5);// for # sign
        WriteSequence(-1, -1, StartSiteForLabels, TheFirstSeq, LastValidSiteNum);
        WriteToOutFile(CurrentStr+';');  //added a semicolumn as it is a label statement
        if doXl then
          ExpD.WriteLine(0, 'C'); // skip the first two cells, since that's the taxa names and groups.
      end;
      CurDomainStartSite := NextStartSite;
      if doXl then
        ExpD.WriteLine();
    end;

    if AIsPAUP3Format or AIsPAUP4Format then
    begin
      WriteToOutFile(';');
      WriteToOutFile('end;');
    end;

    if doXl then
      ExpD.Visible := true;

    if Options.IsSpreadsheetFormat and Options.IsInMainThread then
    begin
      if Options.IsEXCELFormat then
        Result := ExpD.SaveFile(SaveTo, ExportExcel, Options.IsInMainThread)
      else if Options.IsExcelXmlFormat then
        Result := ExpD.SaveFile(SaveTo, ExportExelXML, Options.IsInMainThread)
      else if Options.IsOdsFormat then
        Result := ExpD.SaveFile(SaveTo, ExportODS, Options.IsInMainThread)
      else
        Result := ExpD.SaveFile(SaveTo, ExportCSV, Options.IsInMainThread);
    end
    else if Options.IsSpreadsheetFormat and (not Options.IsInMainThread) then
      Result := True
    else
      Result := FileExists(SaveTo);
   except
     on E: EAbort do
     begin
       { it's ok, the user cancelled}
     end;
     On E: Exception do
     begin
       RaiseErrorMessage(HC_Unexpected_Error, E.Message);
       Result := False;
     end;
   end;
 finally
   if not doXl then
     CloseFile(OutFile);
   if Assigned(ExpD) and (xlFile = nil) then
     ExpD.Free;
   if Assigned(tempSeq) then
     FreeMem(tempSeq);
 end;
end;

procedure TD_InputSeqData.CalculateMaxTaxaNameLen;
var
 i: integer;
begin
  for i := 0 to D_InputSeqData.NoOfTaxa -1 do      //Find the name thats longest in number of characters
  begin
    if Length(D_InputSeqData.FOtuInfos[i].name) > D_InputSeqData.MaxTaxaNameLen then
    D_InputSeqData.MaxTaxaNameLen := Length(D_InputSeqData.FOtuInfos[i].name);
  end;
end;


procedure TD_InputSeqData.writeLongIntListToFile(data : TFileStream; intlist :TLongIntList);
var
  i, j : integer;
begin
    i := intlist.Count;
        data.write(i, 4);
        for i:=0 to intlist.Count-1 do
        begin
          j := intlist.Items[i];
          data.Write(j, 8);
        end;
end;

procedure TD_InputSeqData.SaveSession(filename : String);
var
  data : TMemoryStream;
  GroupComma: AnsiString = '';
  TaxaComma, BufferString: AnsiString;
  i, j: longint;
   done: Boolean;
   te : TSiteAttrType;
begin
 TaxaComma := '';
 try try
    try
      data := TMemoryStream.Create;
      i := Round((D_InputSeqData.NoOfSites * D_InputSeqData.NoOfTaxa) * 1.2);  //How many bytes we think we'll need to hold data
      data.SetSize(i);
    except
      on E: Exception do     //If we couldn't create the file let the user know
      begin
        E.Message := 'Unable to create file "' + fileName + '" : ' + E.Message;
        ShowErrorMessage(E);
        Exit;
      end;
    end;

    with VS_SeqDataExplorer do
    begin
      with D_InputSeqData do
      begin
      BufferString := '#BAD';  //replaced by #MDS when file finishes writing properly, able to detect unfinished file
      //BlockWrite(data, BufferString[1], 4);
      data.Write(BufferString[1], 4);

      i := MSDX_SESSION_VERSION; //Version
      data.write(i, 4);
      //BlockWrite(data, i, 4);
      i := TARGET_PLATFORM;
      data.Write(i, SizeOf(i));

      i := 1; //We are saving a Sequence Viewer Data Session as opposed to other viewer saved sessions
      data.write(i, 4);
      //BlockWrite(data, i, 4);

      //SAVE DATA TITLE
	  {$IFDEF VISUAL_BUILD}
      writeStringToFile(data, MegaForm.DataTitle);
	  {$ELSE}
	  writeStringToFile(data, D_MegaMain.DataTitle);
	  {$ENDIF}

    //SAVE DATA DESCRIPTION
    {$IFDEF VISUAL_BUILD}
	  writeStringToFile(data, MegaForm.GetDataDescr);
	  {$ELSE}
	  writeStringToFile(data, D_MegaMain.DataDescription);
	  {$ENDIF}

      i := 0;
      data.write(i, 4);  //How many blocks to skip for future use

      data.write(isNuc, 1); //Record if the data is Nucleotide

      data.write(isCoding, 1); //If the data is coding = 1 otherwise 0

      data.write(isAmino, 1); //If the data is an amino acid =1 otherwise 0

      data.write(isTranslated, 1);  //If the user currently has the sequence viewer viewing the data as codons, this  is just a state.  Simply knowing it should be translated is good enough as we will translate it once the data is loaded.  This is so that the user can un-translate the data correctly

      //SAVE TAXA AS Comma Seperated Values STRING
      for j:=0 to NoOfTaxa-1 do     //Put all taxa into comma delimited string
        TaxaComma := TaxaComma + FOtuInfos.Otu[j].Name + ',';
      setlength(taxacomma, length(taxacomma)-1); //remove the last unnecessary comma in taxacomma
      writeStringToFile(data, TaxaComma);
        {BytesTaxa := length(TaxaComma);  //The amount of bytes that makes up the CSV list of taxa
         BlockWrite(data, BytesTaxa, 4);  //Save the amount of bytes for the CSV taxa
         setlength(buffer, BytesTaxa + 1);  //Make sure the buffer can handle the CSV list
         StrPCopy(Pchar(buffer), pchar(TaxaComma)); //Copy the taxa CSV into the buffer so its a VAR
         BlockWrite(data, Buffer[0], BytesTaxa);   //Write the CSV list  }

      // GROUP: Write out the group for every taxa.
      for j:=0 to NoOfTaxa-1 do
        GroupComma := GroupComma + FOtuInfos.Otu[j].GpName + ',';
      setlength(GroupComma, length(GroupComma)-1); //remove the last unnecessary comma in GroupComma
      writeStringToFile(data, GroupComma);

      {// SPECIES: Write out the species for every taxa.
      for j:=0 to NoOfTaxa-1 do
        SpeciesComma := SpeciesComma + FOtuInfos.Otu[j].SpName + ',';
      setlength(SpeciesComma, length(SpeciesComma)-1); //remove the last unnecessary comma in GroupComma
      writeStringToFile(data, SpeciesComma);

      // POPULATION: Write out the population for every taxa.
      for j:=0 to NoOfTaxa-1 do
        PopComma := PopComma + FOtuInfos.Otu[j].PopName + ',';
      setlength(PopComma, length(PopComma)-1); //remove the last unnecessary comma in GroupComma
      writeStringToFile(data, PopComma);}

      //SAVE SEQUENCES
      data.write(FNoOfSites, 4);  //The number of sites we should display
      data.write(NoOfSitesUsed, 4);
      data.write(FDomainMarks.NoOfSites, 4); //Number of sequences we have data for (will be more if data is to be translated)

      //WRITE THE NUMBER OF TYPES OF SITE ATTRIBUTES i.e. [meg0base], [meg1base], [megGap], [megMissing], [megParsimInfo]
      
      i := (ord(high(TSiteAttrType))+1);
      data.write(i, 4);

      //WRITE OUT THE NUMBER OF ATTRIBUTE SITES this way we know how many sites there are of each attribute type
      for te:=megNone to megLast do
      begin
        data.write(FNoOfAttrSites[te], 4);
      end;

      for j:=0 to NoOfTaxa-1 do
      begin
        BufferString := PAnsiChar(FOtuInfos.Otu[j].Data);
        data.write(BufferString[1], FDomainMarks.NoOfSites);
      end;

      data.write(GapSym, 1);
      data.write(MissSym,  1);
      data.write(IdenSym, 1);


      if (isNuc and isCoding) then
      begin
        i := AACodonStarts.Count;
        data.write(i, 4);
        for i:=0 to AACodonStarts.Count-1 do
        begin
          j := AACodonStarts.Items[i];
          data.write(j, 4);
        end;

        i := AACodonPos2.Count;
        data.write(i, 4);
        for i:=0 to AACodonPos2.Count-1 do
        begin
          j := AACodonPos2.Items[i];
          data.write(j, 4);
        end;

        i := AACodonPos3.Count;
        data.write(i, 4);
        for i:=0 to AACodonPos3.Count-1 do
        begin
          j := AACodonPos3.Items[i];
          data.write(j, 4);
        end;
      end;

      i := DispTaxaList.Count; //the amount of taxa displayed
      data.write(i, 4);

      for j:=0 to DispTaxaList.Count-1 do  //save the DispTaxaList
      begin
        i := DispTaxaList.items[j];
       // StrPCopy(Pchar(buffer), PChar(DispTaxaList.items[j]));
        data.write(i, 4);
      end;

     //KEEP TRACK OF WHICH OTUS ARE CURRENTLY USED
      for j:=0 to FOtuInfos.noOfOtus -1 do
      begin
        if FOtuInfos.Otu[j].IsUsed then
          i := 1
        else
          i :=0;
        data.write(i, 1);
      end;

      //KEEP TRACK OF WHICH OTUS ARE CURRENTLY HIDDEN
      for j:=0 to FOtuInfos.noOfOtus -1 do
      begin
        if FOtuInfos.Otu[j].IsHidden then
          i := 1
        else
          i :=0;
        data.write(i, 1);
      end;

      for j := 0 to FOtuInfos.NoOfOtus - 1 do
      begin
        BufferString := FOtuInfos[j].GeographicalInfo.MegFileCommandString;
        if Trim(BufferString) <> EmptyStr then
          writeStringToFile(data, BufferString)
        else
          writeStringToFile(data, NO_GEOGRAPHICAL_INFO);
      end;

      for j:=0 to FDomainMarks.NoOfSites-1 do
        data.write(SiteAttrArray[j], 4);

      data.write(NoOfCodingSitesUsed, 4);

      FDomainMarks.WriteToFile(data);

      writeStringToFile(data, codeName);
      writeStringToFile(data, codeTable);


      done := False;

      for i:=0 to FDomainMarks.NoOfSites-1 do
      begin
        if FDomainMarks[i] = CurDomainInfo then
        begin
          done := true;
          data.write(i, 4);
          break;
        end;
      end;

      if NOT done then  //If the user has not opened the visual yet then there is no current domain info
      begin
        i := 0;     //Mega makes a default domain if none is specified so we are safe assuming there is at least one domain
        data.write(i, 4);
      end;
      writeIntToFile(data, HighlightColor);
      data.write(DispTaxaNamesItem, 1);
      data.write(DispSelTaxaItem, 1);
      data.write(DispUseIdenSymbolItem, 1);
      data.write(DispGpNamesItem, 1);
      data.write(DispColorItem, 1);
      data.write(ConstantItem, 1);
      data.write(VariableItem, 1);
      data.write(ParsimInfoItem, 1);
      data.write(SingletonItem, 1);
      data.write(LabelledItem, 1);
      data.write(ZeroFoldItem, 1);
      data.write(TwoFoldItem, 1);
      data.write(FourFoldItem, 1);
      data.write(StatUseOnlyMarkedSitesItem, 1);
      data.write(StatAllSelSitesItem, 1);

      { TODO 1 -oglen -cfpspreadsheet : update savesession to handle StatDispInOds and StatDispInXls }
      data.write(StatDispInXLItem, 1);
      data.write(StatDispInCSVItem, 1);
      data.write(StatDispInTextItem, 1);

      data.write(DispResultsOnlyItem, 1);
      data.write(HideNameItem, 1);
      data.write(HideSeqItem, 1);
      data.write(HideMotifItem, 1);

      writeStringToFile(data, CurFont.Name);

      writeIntToFile(data, CurFont.CharSet);
      writeIntToFile(data, CurFont.Color);
      writeIntToFile(data, CurFont.Size);
      {$IFNDEF FPC}
      writeIntToFile(data, Byte(CurFont.Style));
      {$ELSE}
      data.write(CurFont.Bold, 1);
      data.write(CurFont.Italic, 1);
      data.write(CurFont.Underline, 1);
      data.write(CurFont.StrikeThrough, 1);
      {$ENDIF}

      // -- Search Result BEGIN
      writeIntToFile(data, NoOfSearchResults); // So we know how many search results to read out
      for i:=0 to NoOfSearchResults-1 do
      begin
        writeIntToFile(data, SearchResults[i].Left);
        writeIntToFile(data, SearchResults[i].Top);
        writeIntToFile(data, SearchResults[i].Right);
        writeStringToFile(data, SearchResults[i].UserInput);
        writeStringToFile(data, SearchResults[i].SearchStr);
        writeStringToFile(data, SearchResults[i].MatchStr);
      end;

      writeStringToFile(data, VS_SeqDataExplorer.CurMotifStr); // Write out Single Motif Search

      writeStringToFile(data, VS_SeqDataExplorer.CurSearchStr);  // Write out Name Search
      // -- Search Result END

      if VS_SeqDataExplorer.CurRow < 1 then
        VS_SeqDataExplorer.CurRow := 1;
      if VS_SeqDataExplorer.CurCol < 1 then
        VS_SeqDataExplorer.CurCol := 1;
      writeIntToFile(data, VS_SeqDataExplorer.CurRow);  // Currently Selected row
      writeIntToFile(data, VS_SeqDataExplorer.CurCol);  // Currently Selected column

      i := sizeof(JobsPending);
      data.write(i, 4);
      data.write(JobsPending, i);

      end;

    end;



    i := 1396985123;
    data.write(i, 4);
    data.SetSize(data.Position);

    data.Seek(soFromBeginning, 0); //Change #BAD to #MDS at beginning of file
    i := 1396985123;
    data.write(i, 4);
    data.SaveToFile(filename);
  except
     on E: EInOutError do //Likely user is out of disk space as writeblock returned error
     begin
       E.Message := 'Unable to fully write file, please make sure hard disk is not full : ' + E.Message;
       ShowErrorMessage(E)
     end;
     on E: Exception do  //Catch- all
     begin
       E.Message := 'Unable to write file, please try again : ' + E.Message;
       ShowErrorMessage(E);
     end;
   end;
  finally
    //FreeAndNil(Compress);
    FreeAndNil(data);
  end;
end;


procedure TD_InputSeqData.writeStringToFile(data : TStream; toWrite : AnsiString);
var
  i : integer;
  buffer : array of AnsiChar;
  TempString : AnsiString;
begin
  try
      i := length(toWrite);
      data.write(i, 4);
      if i < 1 then
        exit;
      setlength(buffer, i+1);
      TempString := toWrite;
      strPcopy(PAnsiChar(buffer), PAnsiChar(TempString));
      data.write(buffer[0], i);
  except
    on E: Exception do
      Raise(Exception.create('Unable to write string "' + toWrite + '" to file ' + E.Message));
  end;
end;

function TD_InputSeqData.NeedsComputed(aAttr: TSiteAttrType): Boolean;
begin
  Result := FForcingAttributeUpdates or ((VS_SeqDataExplorer.CurAttrDisp = aAttr) and (not (aAttr in FComputedStats)));
end;

procedure TD_InputSeqData.writeIntToFile(data : TStream; toWrite : integer; Size : integer = 4);
var
 i : integer;
begin
  try
    i := toWrite;
    data.write(i, Size);
  except
    on E: Exception do
      Raise(Exception.Create('Unable to write integer "' + IntToStr(toWrite) + ' of size ' + IntToStr(Size) + ' to file ' + E.Message));
   end;

end;

procedure TD_InputSeqData.ClearAlleleFrequencies;
var
  i: Integer;
begin
  if Length(FAlleleFrequencies) > 0 then
  begin
    for i := Low(FAlleleFrequencies) to High(FAlleleFrequencies) do
      if Assigned(FAlleleFrequencies[i]) then
        FreeAndNil(FAlleleFrequencies[i]);
    SetLength(FAlleleFrequencies, 0);
  end;
end;

procedure TD_InputSeqData.ClearEntropyList;
var
  i: Integer;
begin
  if Assigned(FEntropyList) then
  begin
    if FEntropyList.Count > 0 then
      for i := 0 to FEntropyList.Count - 1 do
        FEntropyList[i].Free;
    FreeAndNil(FEntropyList);
  end;
end;

function TD_InputSeqData.GetTotalNumBases: LongInt;
begin
  Result := NoOfSites*NoOfTaxa;
end;

procedure TD_InputSeqData.SetAllDomainInfo(DomainInfo: TAllDomainInfo);
begin
  FAllDomainInfo := DomainInfo;
end;

procedure TD_InputSeqData.WriteCodonMapsToNexus(NexusOut: TStringList;
                                                MyMappedData: TList;
                                                NChar: Integer;
                                                DataTypeStr: String);
var
  i         : Integer;
  ConcatStr    : AnsiString;
  TaxaLabelsStr: AnsiString = '';
begin
  NexusOut.Add('#NEXUS');
  NexusOut.Add('');
  NexusOut.Add('BEGIN TAXA;');
  NexusOut.Add('        DIMENSIONS NTAX=' + IntToStr(MyMappedData.Count) + ';');
  NexusOut.Add('        TAXLABELS');
  for i:=0 to MymappedData.Count-1 do
  begin
    // do tx+i add to taxa labels str, output
    TaxaLabelsStr := TaxaLabelsStr + '''' + TaxaName[i] + ''' ';
  end;
  NexusOut.Add('                ' + TaxaLabelsStr + ';');
  NexusOut.Add('END;');
  NexusOut.Add('');
  NexusOut.Add('BEGIN CHARACTERS;');
  NexusOut.Add('        DIMENSIONS NCHAR=' + IntToStr(NChar) + ';');
  NexusOut.Add('        FORMAT');
  NexusOut.Add('                DATATYPE=' + DataTypeStr);
  NexusOut.Add('                GAP=' + FGapSym);
  NexusOut.Add('                MISSING=' + FMissSym);
  NexusOut.Add('        ;');
  NexusOut.Add('');
  NexusOut.Add('MATRIX');

  for i:=0 to MyMappedData.Count-1 do
  begin
    SetString(ConcatStr,PAnsiChar(MyMappedData[i]), NChar);
    NexusOut.Add('      ''' + TaxaName[i] + ''' ' + ConcatStr);
  end;

  NexusOut.Add('        ;');
  NexusOut.Add('END;');
end;


procedure TD_InputSeqData.MotifProgressUpdate(ProgressIndex: Integer; NewProgress: Integer);
var
  i, total, newPerc: Integer;
begin
  // later on the Runtime progress dlg will have to be refactored into data and a visual.
  ThreadProgressArray[ProgressIndex] := newProgress;
  if ASearchRP = nil then
  begin
    ASearchRP := TRuntimeProgress.Create(nil);
    ASearchRP.Show;
  end;
  //Update with average from the ThreadProgressArray.
  total := 0;
  for i:=0 to high(threadProgressArray) do
  begin
    total := total + threadProgressArray[i];
  end;
  ASearchRP.UpdateRunStatusInfo('Matches Found', IntToStr(NoOfSearchResults));
  newPerc := total div Length(threadProgressArray);
  if ASearchRP.Progress <> newPerc then
  begin
    ASearchRP.UpdatePercentProgress(newPerc);
    {$IFDEF VISUAL_BUILD}
    ASearchRP.ProcessMessages;
    {$ENDIF}
  end;
end;

procedure TD_InputSeqData.NotifySearchResults(Sender: TObject; const Item: TSearchResult; Action: TCollectionNotification);
begin
  if Action = cnAdded then
    Exit;
  if Assigned(Item) then
    Item.Free;
end;

procedure TD_InputSeqData.SetNexusFile(AValue: TNexusFile);
begin
  if FNexusFile=AValue then Exit;
  FNexusFile:=AValue;
end;

function TD_InputSeqData.GetUseUnlabeledSites: Boolean;
begin
  if Assigned(FDomainMarks) then
    Result := FDomainMarks.UseUnlabeledSites
  else
    Result := True;
end;

procedure TD_InputSeqData.SetUseUnlabeledSites(aValue: Boolean);
begin
  if Assigned(FDomainMarks) then
    FDomainMarks.UseUnlabeledSites := aValue;
end;

function TD_InputSeqData.SearchForMotifs(Motifs: TStringList; RowToStart, ColToStart: Integer; StopAfterFirst: Boolean; ARP: TRuntimeProgress; SearchingForNext: Boolean): Boolean;
var
  i: Integer;
  ASearchThread: TMotifSearchThread = nil;
begin
  SearchIsCompleted := false;
  if ARP <> nil then
    ARP.UpdateRunStatusInfo('Status', 'Searching all motifs simultaneously');
  for i:=0 to Motifs.Count-1 do
  begin
    if Length(Motifs.Strings[i]) < 3 then
      Continue;  // Skip motifs less than 3 chars long.
    SetLength(ThreadProgressArray, Length(ThreadProgressArray)+1);
    ThreadProgressArray[high(ThreadProgressArray)] := 0;
    ASearchThread := TMotifSearchThread.Create(Motifs.Strings[i], RowToStart, ColToStart, StopAfterFirst, Length(ThreadProgressArray)-1, SearchingForNext);
    ASearchThread.Start;
  end;
  Result := True;
end;


function TD_InputSeqData.GetSearchResult(Index: Integer): TSearchResult;
begin
  if Index < FSearchResults.Count then
  begin
    result := TSearchResult(FSearchResults.Items[Index]);
  end
  else
    Raise Exception.Create('Out of Bounds error, accessing invalid index of FSearchResults');
end;

procedure TD_InputSeqData.AddSearchResult(Res: TSearchResult);
begin
  FSearchResults.Add(Res);
end;

procedure TD_InputSeqData.ReportNextLoc(Res: TSearchResult);
begin
  NextMotifAt := Res;
end;

procedure TD_InputSeqData.LabelSitesUsedForMinorAlleleIdentityComparisons(aLabel: AnsiChar);
var
  i: Integer;
begin
  if Length(FAlleleComparisonSites) > 0 then
    for i := Low(FAlleleComparisonSites) to High(FAlleleComparisonSites) do
    begin
      if VS_SeqDataExplorer.IsTranslated then
      begin
        FDomainMarks.SiteLabel[AACodonStarts[FAlleleComparisonSites[i]]] := aLabel;
        FDomainMarks.SiteLabel[AACodonPos2[FAlleleComparisonSites[i]]] := aLabel;
        FDomainMarks.SiteLabel[AACodonPos3[FAlleleComparisonSites[i]]] := aLabel;
      end
      else
      begin
        FDomainMarks.SiteLabel[FAlleleComparisonSites[i]] := aLabel;
      end;
    end;
end;

{$IFDEF VISUAL_BUILD}
procedure TD_InputSeqData.AddDomainsToSitesUsedFromMinorAlleleIdentityComparisons;
var
  aInfo: TDomainInfo = nil;
  i: Integer;
begin
  if Length(FAlleleComparisonSites) > 0 then
    for i := Low(FAlleleComparisonSites) to High(FAlleleComparisonSites) do
    begin
      aInfo := TDomainInfo.Create;
      aInfo.FromSite := FAlleleComparisonSites[i];
      aInfo.ToSite := FAlleleComparisonSites[i];
      aInfo.Name := FAllDomainInfo.NextAvailableDomainName;
      aInfo.IsUsed := True;
      aInfo.IsDomain := True;
      aInfo.IsCoding := IsCoding;
      FAllDomainInfo.Add(aInfo);
      FDomainMarks[aInfo.FromSite] := aInfo;
      FDomainMarks.IsDirty := True;
      GeneDomainDlg.AddInfo(aInfo);
    end;
end;

{$ENDIF}

procedure TD_InputSeqData.AutoLabelSites(a: TSiteAttrType);
var
  s: LongInt;
begin
  if not (a in [megConst, megVar, megParsimInfo, megSingleton, meg0Fold, meg2Fold, meg4Fold]) then
    raise Exception.Create('unexpected site attribute');

  if VS_SeqDataExplorer.IsTranslated then
  begin
    for s := 0 to AACodonStarts.Count - 1 do
    begin
      if (a = megConst) and (megAminoConst in SiteAttrArray[s]) then
      begin
        FDomainMarks.SiteLabel[AACodonStarts[s]] := AnsiChar('c');
        FDomainMarks.SiteLabel[AACodonPos2[s]] := AnsiChar('c');
        FDomainMarks.SiteLabel[AACodonPos3[s]] := AnsiChar('c');
      end
      else if (a = megVar) and (megAminoVar in SiteAttrArray[s]) then
      begin
        FDomainMarks.SiteLabel[AACodonStarts[s]] := AnsiChar('v');
        FDomainMarks.SiteLabel[AACodonPos2[s]] := AnsiChar('v');
        FDomainMarks.SiteLabel[AACodonPos3[s]] := AnsiChar('v');
      end
      else if (a = megParsimInfo) and (megAminoParsimInfo in SiteAttrArray[s]) then
      begin
        FDomainMarks.SiteLabel[AACodonStarts[s]] := AnsiChar('p');
        FDomainMarks.SiteLabel[AACodonPos2[s]] := AnsiChar('p');
        FDomainMarks.SiteLabel[AACodonPos3[s]] := AnsiChar('p');
      end
      else if (a = megSingleton) and (megAminoSingleton in SiteAttrArray[s]) then
      begin
        FDomainMarks.SiteLabel[AACodonStarts[s]] := AnsiChar('s');
        FDomainMarks.SiteLabel[AACodonPos2[s]] := AnsiChar('s');
        FDomainMarks.SiteLabel[AACodonPos3[s]] := AnsiChar('s');
      end;
    end;
  end
  else
  begin
    for s := 0 to FNoOfSites - 1 do
    begin
      if (a = megConst) and (megConst in SiteAttrArray[s]) then
        FDomainMarks.SiteLabel[s] := AnsiChar('c')
      else if (a = megVar) and (megVar in SiteAttrArray[s]) then
        FDomainMarks.SiteLabel[s] := AnsiChar('v')
      else if (a = megParsimInfo) and (megParsimInfo in SiteAttrArray[s]) then
        FDomainMarks.SiteLabel[s] := AnsiChar('p')
      else if (a = megSingleton) and (megSingleton in SiteAttrArray[s]) then
        FDomainMarks.SiteLabel[s] := AnsiChar('s')
      else if (a = meg0Fold) and (meg0Fold in SiteAttrArray[s]) then
        FDomainMarks.SiteLabel[s] := AnsiChar('0')
      else if (a = meg2Fold) and (meg2Fold in SiteAttrArray[s]) then
        FDomainMarks.SiteLabel[s] := AnsiChar('2')
      else if (a = meg4Fold) and (meg4Fold in SiteAttrArray[s]) then
        FDomainMarks.SiteLabel[s] := AnsiChar('4');
    end;
  end;
end;

function TD_InputSeqData.GetNumOfSearchRes: Integer;
begin
  result := FSearchResults.Count;
end;

procedure TD_InputSeqData.ClearSearchResult;
begin
  FSearchResults.Clear;
end;

procedure TD_InputSeqData.ClearSearchResults;
begin
  //SetLength(FSearchResults, 0);
  FSearchResults.Clear;
end;

procedure TD_InputSeqData.UpdateSearchResults(FromIndex, ToIndex: Integer);
var
  i, j, FromTopIndex, ToTopIndex: Integer;
begin
  FromTopIndex := D_InputSeqData.DispTaxaList[FromIndex-1];
  ToTopIndex   := D_InputSeqData.DispTaxaList[ToIndex-1];

   for i:=0 to NoOfSearchResults-1 do // Temporarily change the value of those which are being moved to -1.
     if SearchResults[i].Top = FromTopIndex then
       TSearchResult(FSearchResults.Items[i]).Top := -1;

  if ToIndex > FromIndex then  // moving DOWN
  begin
    for i:=FromIndex to ToIndex do
    begin
      for j:=0 to NoOfSearchResults-1 do
      begin
        if (SearchResults[j].Top = D_InputSeqData.DispTaxaList[i-1]) then
          TSearchResult(FSearchResults.Items[j]).Top := SearchResults[j].Top-1;
      end;
    end;
  end
  else // else FromIndex > ToIndex  moving UP
  begin

    for i:=FromIndex downto ToIndex do
    begin
      for j:=0 to NoOfSearchResults-1 do
      begin
        if (SearchResults[j].Top = D_InputSeqData.DispTaxaList[i-1]) then
          TSearchResult(FSearchResults.Items[j]).Top := SearchResults[j].Top+1;
      end;
    end;

  end;

   for i:=0 to NoOfSearchResults-1 do // Make the actual move.
   begin
     if SearchResults[i].Top = -1 then
       TSearchResult(FSearchResults.Items[i]).Top := ToTopIndex;
   end;

end;

function TD_InputSeqData.SearchForKmer(AminoAcidStr: AnsiString; Ambiguities: Integer; AddToSearchResults: Boolean; ARP: TRuntimeProgress; EpitopeNum, OverallNumEpitopes: Integer; IsSearchingForBest: Boolean; var Abort: Boolean): Integer;
var
  i, j, NoOfBeginResults: Integer;
  SeqPercStr: AnsiString;
  MaxAmbigAllowed: Integer;
begin
  NoOfBeginResults := NoOfSearchResults;
  for i:=0 to NoOfTaxa-1 do
  begin
  {$IFDEF VISUAL_BUILD}
    if ARP.StopBtn.Down then
    begin
      Abort := True;
      Exit;
    end;
    {$ENDIF}
    if IsSearchingForBest then
    begin
      MaxAmbigAllowed := Length(AminoAcidStr);
      SeqPercStr := IntToStr( ((i*100) div NoOfTaxa) div (MaxAmbigAllowed - Ambiguities + 1) ) + '%';
    end
    else
      SeqPercStr :=  IntToStr((i*100) div NoOfTaxa) + '%';
    {$IFDEF VISUAL_BUILD}
    ARP.ProcessMessages;
    {$ENDIF}
    ARP.UpdateRunStatusInfo('%Variants Done', SeqPercStr); // which sequence we are currently searching.
    {$IFDEF VISUAL_BUILD}
    ARP.Repaint;
    {$ENDIF}
    for j:=0 to NoOfSites-1-Length(AminoAcidStr) do
    begin
      // check for a len(AminoAcidStr)-mer match
      if Sequence[i][j] = '-' then
        continue;
      if CheckKMer(AminoAcidStr, i, j, Ambiguities, AddToSearchResults) then
      begin
        ARP.UpdateRunStatusInfo('Matches Found', IntToStr(NoOfSearchResults)); // update number of results if we found one.
        {$IFDEF VISUAL_BUILD}
        ARP.Repaint;
        {$ENDIF}
      end;
    end;
    ARP.UpdatePercentProgress( ((((i+1)*100) div NoOfTaxa) div OverallNumEpitopes) + ((EpitopeNum * 100) div OverallNumEpitopes) );
  end;
  Result := NoOfSearchResults - NoOfBeginResults;
end;

function TD_InputSeqData.CheckKMer(searchAA: AnsiString; Seq, Site, Ambiguities: Integer; AddToSearchResults: Boolean): Boolean;
var
  bad: Integer = 0;
  i: Integer;
  gapOffset: Integer = 0;
  sRes: TSearchResult = nil;
  exitByNotUsedSite: Boolean = False;
  searchResultStr: String = '';
  matchStr: AnsiString = ''; // indicates where the ambiguities were in a match.  Used for debugging, turn off when done for speedup.

  function Matches(Target, Input: AnsiChar): Boolean;
  begin
    Input := UpCase(Input);
    Target := UpCase(Target);
    if IsNuc and (not VS_SeqDataExplorer.IsTranslated) then
    begin
      case Input of
        'A': result := (Target = 'A');
        'T': result := (Target = 'T') or (Target = 'U');
        'U': result := (Target = 'U') or (Target = 'T');
        'C': result := (Target = 'C');
        'G': result := (Target = 'G');
        'R': result := (Target = 'R') or (Target = 'A') or (Target = 'G');
        'Y': result := (Target = 'Y') or (Target = 'T') or (Target = 'C') or (Target = 'U');
        'M': result := (Target = 'M') or (Target = 'A') or (Target = 'C');
        'K': result := (Target = 'K') or (Target = 'T') or (Target = 'G') or (Target = 'U');
        'S': result := (Target = 'S') or (Target = 'C') or (Target = 'G');
        'W': result := (Target = 'W') or (Target = 'A') or (Target = 'T') or (Target = 'U');
        'B': result := (Target = 'B') or (Target = 'T') or (Target = 'U') or (Target = 'C') or (Target = 'G') or (Target = 'Y') or (Target = 'K') or (Target = 'S');
        'V': result := (Target = 'V') or (Target = 'A') or (Target = 'C') or (Target = 'G')                   or (Target = 'R') or (Target = 'M') or (Target = 'S');
        'D': result := (Target = 'D') or (Target = 'A') or (Target = 'T') or (Target = 'U') or (Target = 'G') or (Target = 'R') or (Target = 'K') or (Target = 'W');
        'H': result := (Target = 'H') or (Target = 'A') or (Target = 'T') or (Target = 'U') or (Target = 'C') or (Target = 'Y') or (Target = 'M') or (Target = 'W');
        'N': result := true;
        '-': result := Target = '-'
      else
        result := false;
      end;
    end
    else
      result := (Input = Target);
  end;  

  function LastColumn: Integer;
  begin
    Result := Site + i + gapOffset;
  end;

begin
  Result := False;
  FStringBuilder.Clean;
  if not SeqUsed[Seq] then
    Exit;
  i := 0;
  while (i < length(searchAA)) and (LastColumn < NoOfSites) and (not exitByNotUsedSite) do  // while i < 9 do  for a 9-mer
  begin
    Assert(LastColumn = (Site +i + gapOffset));
    if not FDomainMarks.IsSiteUsed[LastColumn] then
    begin
      exitByNotUsedSite := true;
    end
    else
    begin
      matchStr := matchStr + Sequence[Seq][LastColumn];
      if Sequence[Seq][LastColumn] = '-' then
      begin
        inc(gapOffset);
        continue;
      end;
      if not Matches(Sequence[Seq][LastColumn], searchAA[i+1]) then
      begin
        inc(bad);
        if bad > Ambiguities then
          Break; // No reason to continue searching after we know there are too many bad.
        FStringBuilder.Add('?');
      end
      else
        FStringBuilder.Add(searchAA[i + 1]);
      inc(i);
    end;
  end;

  if (bad <= Ambiguities) and (not exitByNotUsedSite) and (LastColumn <= NoOfSites) then  // check that this doesn't contain a site which is in a domain that is not used, or we ended with -'s.
  begin
    searchResultStr := FStringBuilder.GenerateString;
    Result := (Length(searchResultStr) >= Length(searchAA));
    if Result and AddToSearchResults then
    begin
      sRes := TSearchResult.Create;
      sRes.Top := Seq;
      sRes.Left := Site;
      sRes.Right := LastColumn;
      sRes.SearchStr := searchResultStr;
      sRes.UserInput := searchAA;
      sRes.MatchStr  := matchStr;
      AddSearchResult(sRes);
    end;
  end;
end;

procedure TD_InputSeqData.ConstructKMer(AAStr: AnsiString; currWindow: Integer;
  windowLimit: Integer; resultsStringList: TStringList);
var
  i: integer;
  tempAA: AnsiString;
begin
  if currWindow = 9 then // first time this function is called
  begin
    resultsStringList.sorted := true;
    resultsStringList.Duplicates := dupIgnore;
  end;
  if currWindow = windowLimit then
    Exit;
  for i:=1 to length(AAStr) do
  begin
    if AAstr[i] = '.' then
      continue;
    tempAA := AAstr;
    tempAA[i] := '.';
    resultsStringList.add(tempAA);
    constructKMer(tempAA, currWindow-1, windowLimit, resultsStringList);
  end;
end;

function TD_InputSeqData.BestEpitope(Epitope: AnsiString; ARP: TRuntimeProgress; EpitopeNum, OverallNumEpitopes: Integer; var Abort: Boolean): AnsiString;
var
  i, numResults: Integer;
begin
  // run with 1 amb, if no result then 2 amb, if no result 3 amb ... until you find a result or reach length of the epitope (then display "sorry").
  ARP.UpdateRunStatusInfo('Status', 'Searching for Motif(s)');
  i:=0;
  Repeat
    //ARP.UpdateRunStatusInfo('No. of Ambiguities', IntToStr(i));
    {$IFDEF VISUAL_BUILD}
    ARP.Refresh;
    {$ENDIF}
    numResults := SearchForKmer(Epitope, i, True, ARP, EpitopeNum, OverallNumEpitopes, True, Abort);
    inc(i);
  until (numResults > 0) or ((i > (Length(Epitope) - 3)));

  if D_InputSeqData.NoOfSearchResults > 0 then
    Result := 'The best match for epitope: ' + Epitope + ' had ' + IntToStr(i-1) + ' ambiguities.  Highlighted are matches with ' + IntToStr(i-1) + ' ambiguities.'
  else
    Result := 'No matches were found for the current Epitope.';
end;

function TD_InputSeqData.FindEpitope(Epitope: AnsiString; ARP: TRuntimeProgress; EpitopeNum, OverallNumEpitopes: Integer; var Abort: Boolean; NumAmbig: Integer; var CurrAmbigStr: AnsiString): AnsiString;
var
  CurrAmbigInt: Integer= -1;
begin
  Result := EmptyStr;
  if Length(Epitope) < 3 then  // Skip epitopes which are less than 3 chars long.
    Exit;
  SearchForKmer(Epitope, NumAmbig, True, ARP, EpitopeNum, OverallNumEpitopes, False, Abort);
  if NoOfSearchResults > 0 then
    Result := 'There were ' + IntToStr(NoOfSearchResults) + ' matches to ' + Epitope + ' with <= ' + IntToStr(CurrAmbigInt) + ' ambiguities.'
  else
    Result := 'No matches were found for the current Epitope.';
end;

function TD_InputSeqData.BestEpitopes(Epitopes: TStringList; ARP: TRuntimeProgress): AnsiString;
var
  i: Integer;
  Abort: Boolean = False;
begin
  for i:=0 to Epitopes.Count-1 do
  begin
    ARP.UpdateRunStatusInfo('Current Motif', Epitopes.Strings[i]);
    {$IFDEF VISUAL_BUILD}
    ARP.Refresh;
    {$ENDIF}
    BestEpitope(Epitopes.Strings[i], ARP, i, Epitopes.Count, Abort);
    {$IFDEF VISUAL_BUILD}
    if Abort then
    begin
      result := 'User aborted search.';
      if (MessageDlg('Would you like to keep the search results found so far?', mtConfirmation, [mbYes, mbNo], 0) = mrNo) then
      begin
        D_InputSeqData.ClearSearchResults;
        Exit;
      end;
    end;
    {$ELSE}
    if Abort then
      D_InputSeqData.ClearSearchResults;
    {$ENDIF}
  end;
  if NoOfSearchResults > 0 then
    result := IntToStr(NoOfSearchResults) + ' matches were found.'
  else
    result := 'No results were found.';
end;

function TD_InputSeqData.FindEpitopes(Epitopes: TStringList; FixedNumAmbig: Integer; ARP: TRuntimeProgress): AnsiString;
var
  i: Integer;
  Abort: Boolean = False;
  CurrAmbigStr: AnsiString = '';
begin
  ARP.UpdatePercentProgress(0);
  for i:=0 to Epitopes.Count-1 do   
  begin
    ARP.UpdateRunStatusInfo('Epitope Searching', Epitopes.Strings[i]);
    {$IFDEF VISUAL_BUILD}
    ARP.Refresh;
    {$ENDIF}
    FindEpitope(Epitopes.Strings[i], ARP, i, Epitopes.Count, Abort, FixedNumAmbig, CurrAmbigStr);
    {$IFDEF VISUAL_BUILD}
    if Abort then
    begin
      result := 'User aborted search.';
      if (MessageDlg('Would you like to keep the search results found so far?', mtConfirmation, [mbYes, mbNo], 0) in [mrNo, mrNone]) then
        D_InputSeqData.ClearSearchResults;
      Exit;
    end;
    {$ELSE}
    if Abort then
      D_InputSeqData.ClearSearchResults;
    {$ENDIF}
    ARP.UpdatePercentProgress( ((i+1)*100) div Epitopes.Count );
  end;
  ARP.UpdatePercentProgress(100);
  if NoOfSearchResults > 0 then
    result := IntToStr(NoOfSearchResults) + ' matches were found.'
  else
    result := 'No results were found.';
end;

function TD_InputSeqData.SiteAttributesToString(siteIndex: Int64): String;
var
  t: TSiteAttr;
  attr: TSiteAttrType;
begin
  Result := EmptyStr;
  t := SiteAttrArray[siteIndex];
  for attr in t do
  begin
    Result := Result + GetEnumName(TypeInfo(TSiteAttrType), Ord(attr)) + ' ';
  end;
end;

function TD_InputSeqData.ComputeAlleleFrequencies(minCutoff, maxCutoff: Double; useReferenceSeq: Boolean; includeReferenceSeq: Boolean): Integer;
var
  i: Integer;
  j: Integer;
  aSeq: PAnsiChar;
  index: Integer = 0;
  siteUsed: Boolean;
  updateTime: TDateTime;
  numUsed: Integer;
  refIndex: Integer = 0;
  seqIndex: Integer = 0;
begin
  Result := 0;
  updateTime := Now;
  if Length(FAlleleFrequencies) > 0 then
    ClearAlleleFrequencies;
  SetLength(FAlleleFrequencies, NoOfSites);
  numUsed := FOtuInfos.NoOfSelOtus;
  for i := Low(FAlleleFrequencies) to High(FAlleleFrequencies) do
    FAlleleFrequencies[i] := TAlleleFrequency.Create(FIsAmino, numUsed);
  while (not FOtuInfos[refIndex].IsUsed) and (refIndex < FOtuInfos.NoOfOtus) do
    inc(refIndex);
  if not FOtuInfos[refIndex].IsUsed then
    raise Exception.Create('cannot compute allele frequencies because no sequences are used');
  if useReferenceSeq then
  begin
    aSeq := PAnsiChar(FOtuInfos[refIndex].Data);
    for i := Low(FAlleleFrequencies) to High(FAlleleFrequencies) do
      FAlleleFrequencies[i].SetMajorAlleleFromReferenceSequence(aSeq[i]);
  end;

  if includeReferenceSeq then
    seqIndex := refIndex
  else
    seqIndex := refIndex + 1;
  for i := seqIndex to NoOfTaxa - 1 do
  begin
    if FOtuInfos[i].IsUsed then
    begin
      aSeq := PAnsiChar(FOtuInfos[i].Data);
      for j := 0 to NoOfSites - 1 do
        FAlleleFrequencies[j].IncrementFrequency(aSeq[j]);
      if SecondsBetween(Now, updateTime) > 1 then
      begin
        if DoProgressCheckCancel(Round(i/FNoOfSites*100), 'Computing allele frequencies...') then
          Exit;
        updateTime := Now;
      end;
    end;
  end;

  SetLength(FAlleleComparisonSites, FNoOfSites);
  for i := 0 to FNoOfSites - 1 do
  begin
    if FIsAmino then
    begin
     if FAlleleFrequencies[i].MajorAllele <> gdResiX then
     begin
       siteUsed := IsInRangeInclusive(FAlleleFrequencies[i].MinorAllelePercentFrequency, minCutoff, maxCutoff);
       if siteUsed then
       begin
         FAlleleComparisonSites[index] := i;
         inc(index);
       end;
     end;
    end
    else
    begin
     if FAlleleFrequencies[i].MajorAllele <> gdBaseN then
     begin
       siteUsed := IsInRangeInclusive(FAlleleFrequencies[i].MinorAllelePercentFrequency, minCutoff, maxCutoff);
       if siteUsed then
       begin
         FAlleleComparisonSites[index] := i;
         inc(index);
       end;
     end;
    end;
    if SecondsBetween(Now, updateTime) > 1 then
    begin
      if DoProgressCheckCancel(Round(i/FNoOfSites*100), 'Finding used sites...') then
        Exit;
      updateTime := Now;
    end;
  end;
  SetLength(FAlleleComparisonSites, index);
  Result := Index;
end;

function TD_InputSeqData.GetAlleleFrequenciesAsStringList(minCutoff, maxCutoff: Double; useReferenceSeq: Boolean; includeRefSeq: Boolean): TStringList;
var
  i: Integer;
  updateTime: TDateTime;
begin
  updateTime := Now;
  ComputeAlleleFrequencies(minCutoff, maxCutoff, useReferenceSeq, includeRefSeq);
  Result := TStringList.Create;
  Result.Add(Format('Shown below are major and minor allele frequencies for all %d sites whose minor allele is', [Length(FAlleleComparisonSites)]));
  Result.Add(Format('in the specified range (%.4f%% to %.4f%%)', [minCutoff*100, maxCutoff*100]));
  Result.Add('These are the sites that were used to compare sequence identity. All other sites were ignored.');
  if useReferenceSeq then
    Result.Add(Format('Major alleles are assumed to be the reference sequence (%s) alleles', [FOtuInfos.GetFirstUsedSequenceName]));
  Result.Add(LineEnding);

  if Length(FAlleleComparisonSites) > 0 then
    for i := Low(FAlleleComparisonSites) to High(FAlleleComparisonSites) do
    begin
      Result.Add(FAlleleFrequencies[FAlleleComparisonSites[i]].DeveloperString(FAlleleComparisonSites[i] + 1));
      if SecondsBetween(Now, updateTime) > 1 then
      begin
        if DoProgressCheckCancel(Round(i/High(FAlleleComparisonSites)*100), 'Calculating allele frequencies...') then
          Exit;
        updateTime := Now;
      end;
    end;
end;

function TD_InputSeqData.NumAlleleComparisonSitesUsed: Integer;
begin
  Result := Length(FAlleleComparisonSites);
end;

function TD_InputSeqData.AreIdentical(seq1: Integer; seq2: Integer): Boolean;
var
  i, site: Integer;
  s1, s2: PAnsiChar;
begin
  Result := False;
  if CompareValue(FEntropyList[seq1].Entropy, FEntropyList[seq2].Entropy, FP_CUTOFF) <> 0 then
    Exit;

  s1 := PAnsiChar(FOtuInfos[seq1].Data);
  s2 := PAnsiChar(FOtuInfos[seq2].Data);
  for i := 0 to Length(FAlleleComparisonSites) - 1 do
  begin
    site := FAlleleComparisonSites[i];
    if s1[site] <> s2[site] then
      Exit;
  end;
  Result := True;
end;

function TD_InputSeqData.GroupIdenticalSeqs(minCutoff, maxCutoff: Double): Integer;
var
  i, j: Integer;
  isGrouped: array of Boolean;
  numComparisons: Integer;
  numDone: Integer = 0;
  msg: String;
  updateTime: TDateTime;
  groupCounts: TFPHashList = nil;

  function NextAvailableGroupName: String;
  var
    index: Integer = 1;
  begin
    Result := Format('Group-%d', [index]);
    while groupCounts.FindIndexOf(Result) >= 0 do
    begin
      inc(index);
      Result := Format('Group-%d', [index]);
    end;
    groupCounts.Add(Result, TGroupCount.Create(Result))
  end;

begin
  try
   Result := 0;
   updateTime := Now;
   msg := Format('Searching for identical seqs (%.3f%% - %.3f%%, using %d sites)', [minCutoff*100, maxCutoff*100, NumAlleleComparisonSitesUsed]);
   numComparisons := FNoOfTaxa*(FNoOfTaxa - 1) div 2;
   SetLength(isGrouped, FNoOfTaxa);
   for i := 0 to FNoOfTaxa - 1 do
   begin
     FOtuInfos[i].GpName := EmptyStr;
     isGrouped[i] := False;
   end;
   ComputeSequenceEntropy;
   groupCounts := GetGroupCounts;
   for i := 0 to FNoOfTaxa - 2 do
   begin
     if FOtuInfos[i].IsUsed then
     begin
       for j := i + 1 to FNoOfTaxa - 1 do
       begin
         inc(numDone);
         if FOtuInfos[j].IsUsed then
         begin
           if (not isGrouped[j]) and AreIdentical(i, j) then
           begin
             if not isGrouped[i] then
             begin
               inc(Result);
               isGrouped[i] := True;
               if FOtuInfos[i].GpName = EmptyStr then
                 FOtuInfos[i].GpName := NextAvailableGroupName; // FOtuInfos[DispTaxaList[i]].Name;
             end;
             FOtuInfos[j].GpName := FOtuInfos[DispTaxaList[i]].GpName;
             isGrouped[j] := True;
           end;
         end;
         if SecondsBetween(Now, updateTime) > 1 then
         begin
           if Result > 1 then
             msg := Format('Completed %.0n pairwise comparisons, created %.0n groups', [numDone*1.0, Result*1.0]);
           if DoProgressCheckCancel(Round(numDone/numComparisons*100), msg) then
             Exit;
           updateTime := Now;
         end;
       end;
     end
     else
       inc(numDone, FNoOfTaxa - i);
   end;
  finally
    if Assigned(groupCounts) then
    begin
      if groupCounts.Count > 0 then
        for i := 0 to groupCounts.Count - 1 do
          TGroupCount(groupCounts.Items[i]).Free;
      groupCounts.Free;
    end;
  end;
end;

procedure TD_InputSeqData.ComputeSequenceEntropy;
var
  i: Integer;
  entropy: TShannonEntropy = nil;
begin
  FEntropyList := TShannonEntropyList.Create;
  if FNoOfTaxa > 0 then
    for i := 0 to FNoOfTaxa - 1 do
    begin
      entropy := TShannonEntropy.Create(PAnsiChar(FOtuInfos[i].Data), FAlleleComparisonSites, FIsAmino);
      FEntropyList.Add(entropy);
      DoProgressCheckCancel(Round(i/FNoOfTaxa*100), 'Computing Sequence Entropy...');
    end;
end;

procedure TD_InputSeqData.CheckRemainingSearchThreads(searchingForNext: Boolean);
var
  i: Integer;
  notDone: Boolean;
begin
  notDone := false;
  for i:=0 to high(threadProgressArray) do
    if threadProgressArray[i] <> 100 then
    begin
      notDone := true;
      break;
    end;

  if not notDone then  // if done
  begin
    FreeAndNil(ASearchRP);
    VS_SeqDataExplorer.RefreshDataGrid;
    if searchingForNext then
      VS_SeqDataExplorer.JumpTo(NextMotifAt.Top, NextMotifAt.Left)
    else
      VS_SeqDataExplorer.NotifySearchCompletion(NoOfSearchResults);
    SetLength(threadProgressArray, 0); // clear the progresses
    SearchIsCompleted := True;;
  end;
end;

function TD_InputSeqData.GetSearchCanceled: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  result := (ASearchRP <> nil) and (ASearchRP.StopBtn.Down);
  {$ELSE}
  result := false;
  {$ENDIF}
end;

function TD_InputSeqData.SRIndexInVisual(Index: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  i := -1;
  for i:=0 to NoOfTaxa do
  begin
    if not SeqUsed[i] then
      continue;
    if DispTaxaList[i] = Index then
    begin
      result := i;
      Exit;
    end;
  end;
end;


procedure TD_InputSeqData.WriteSearchResultsOnALine(OutList: TStringList; SaveLocation: String; Format: TExportType; APleaseWait: TPleaseWait);
var
  i, LongestUserInput, LongestKAmbig, LongestSearchStr, LongestMatchStr: Integer;
  XlsOut: TExcelWrite = nil;
  ResultsSorted: TList = nil;

  function NoOfMismatch(MismatchStr: AnsiString): Integer;
  var
    i: Integer;
  begin
    result := 0;
    for i:=1 to Length(MismatchStr) do
      if MismatchStr[i] = '?' then
        inc(result);
  end;

  function SortedSearchResult(Index: Integer): TSearchResult;
  begin
    result := TSearchResult(ResultsSorted.Items[Index]);
  end;
begin
  ResultsSorted := SortSearchResultsBySeqName;

  if Format = EXtext then
  begin
    OutList.Add('Search Results:');
    CalculateMaxTaxaNameLen;
    LongestUserInput := 10;
    LongestKAmbig    := 9;
    LongestSearchStr := 16;
    LongestMatchStr  := 8;
    for i:=0 to NoOfSearchResults-1 do
    begin
      if DoProgressCheckCancel((i*100) div (NoOfSearchResults * 2), 'Exporting search results...') then
        exit;
      if Length(SearchResults[i].UserInput) > LongestUserInput then
        LongestUserInput := Length(SearchResults[i].UserInput);
      if Length(SearchResults[i].SearchStr) > LongestSearchStr then
        LongestSearchStr := Length(SearchResults[i].SearchStr);
      if Length(SearchResults[i].MatchStr) > LongestMatchStr then
        LongestMatchStr := Length(SearchResults[i].MatchStr);
      if Length(IntToStr(NoOfMisMatch(SearchResults[i].SearchStr))) > LongestKAmbig then
        LongestKAmbig := Length(IntToStr(NoOfMisMatch(SearchResults[i].SearchStr)));
    end;
    OutList.Add(PadStrToWidth('Sequence Name', D_InputSeqData.MaxTaxaNameLen) + '  ' + PadStrToWidth('User Motif', LongestUserInput) +  '  ' + PadStrToWidth('K (ambig)', LongestKAmbig) + '  ' + PadStrToWidth('Searched Motif', LongestSearchStr) + PadStrToWidth('Curr Seq', LongestMatchStr) + ' Start Location');
    for i:=0 to NoOfSearchResults-1 do
    begin
      if DoProgressCheckCancel(i*100 div Ceil(NoOfSearchResults * 2) + 49, 'Exporting search results...') then
        exit;
      OutList.Add(PadStrToWidth(FOtuInfos[SortedSearchResult(i).Top].Name, D_InputSeqData.MaxTaxaNameLen) + ', ' + PadStrToWidth(SortedSearchResult(i).UserInput, LongestUserInput) + ', ' + PadStrToWidth(IntToStr(NoOfMismatch(SortedSearchResult(i).SearchStr)), LongestKAmbig) + ', ' + PadStrToWidth(SortedSearchResult(i).SearchStr, LongestSearchStr) + ', ' + PadStrToWidth(SortedSearchResult(i).MatchStr, LongestMatchStr) + ', ' + IntToStr(SortedSearchResult(i).Left+1));
    end;

  end
  else
  begin
    XlsOut := TExcelWrite.Create(nil, 'Search Results');
    XlsOut.IsXLS := true;
    XlsOut.Add('Sequence Name');
    XlsOut.Add('User Motif');
    XlsOut.Add('K (ambig)');
    XlsOut.Add('Searched Motif');
    XlsOut.Add('Curr Seq');
    XlsOut.Add('Start Location');
    XlsOut.WriteLine();

    for i:=0 to ResultsSorted.Count-1 do
    begin
      if DoProgressCheckCancel((i * 100) div ResultsSorted.Count, 'Exporting search results...') then
      begin
        XlsOut.HadError;
        Exit;
      end;
      XlsOut.Add(FOtuInfos[SortedSearchResult(i).Top].Name);
      XlsOut.Add(SortedSearchResult(i).UserInput);
      XlsOut.Add(NoOfMismatch(SortedSearchResult(i).SearchStr));
      XlsOut.Add(SortedSearchResult(i).SearchStr);
      XlsOut.Add(SortedSearchResult(i).MatchStr);
      XlsOut.Add(SortedSearchResult(i).Left+1);
      XlsOut.WriteLine();
    end;
  end;

  If (Format = EXexcelDisp) or (Format = EXexcelSave) or (Format = EXexcelXmlDisp) or (Format = EXexcelXmlSave) then
  begin
    //XlsOut.AutoFitCols := false;  // If there is an error durring save uncomment this line.
    XlsOut.SaveFile(SaveLocation, ExcelExportToFileType(Format));
  end
  else if (Format = EXcsvDisp) or (Format = EXcsvSave) then
    OutList.Text := XlsOut.GetCsvText;
         
end;


function TD_InputSeqData.SortSearchResultsBySeqName: TList;
var
  ResultList: TList;
  i: Integer;
  
  function CompareResults(Item1: Pointer; Item2: Pointer): Integer; // ALPHA SORT
  var
    Res1, Res2: TSearchResult;
  begin
    Res1 := TSearchResult(Item1);
    Res2 := TSearchResult(Item2);

    if Res1.Top > Res2.Top then
      Result := 1
    else
    if Res1.Top < Res2.Top then
      Result := -1
    else // they are on the same line
    begin
      if Res1.Left > Res2.Left then
        Result := 1
      else
      if Res1.Left < Res2.Left then
        Result := -1
      else
        Result := 0;
    end;
  end;

begin
  ResultList := TList.Create;
  for i:=0 to NoOfSearchResults-1 do
    ResultList.Add(FSearchResults.Items[i]);

  ResultList.Sort(@CompareResults); // sort the search results by sequence (as they are displayed).
  result := ResultList;
end;

procedure TD_InputSeqData.InitSearchResults;
begin
  FSearchResults := TList.Create;
end;

end.




