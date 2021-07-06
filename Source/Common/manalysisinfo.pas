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

unit MAnalysisInfo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$interfaces corba}

interface

{
  TAnalysisInfo Object is passed around and has auto destruction capabilities
  It provides abstraction of all major functions into smaller and more universal
    procedures
  Destroy MAnalysisInfo Last after all the displays have been completely shown and
  become independent
}

uses
  {$IFDEF VISUAL_BUILD}
  MAnalysisPrefDlg,
  {$ENDIF}
  FileUtil, Classes, MOtuInfo, MDistPack, MTreePack, MegaConsts, MegaErrUtils,
  MSeqDistBase, MTreeData, MTreeList, MPartitionList, MLTreeAnalyzer,
  MRuntimeProgressDlg, MD_InputSeqData, Forms, Dialogs,
  MProcessPack, MAnalysisSummary, MD_Sequences,  MReltimeComputer,
  MCalibrationData, MPartitionData, mgroupinfo, ExcelWrite;

{$M+}
type

  { analysis settings and results for gene duplication inference}
  TGeneDupsInfo = class(TObject)
    public
      GeneTreeRooted: Boolean;
      SpeciesTreeRooted: Boolean;
      UsedSpeciesTree: Boolean;
      NumTreeConfigs: Integer;
      NumGeneDups: Integer;
      NumSpeciations: Integer;
      NumSpecies: Integer;
      RootIndex: Integer;
      constructor Create;
      destructor Destroy; override;
      function GeneDupsFound: Boolean;
      procedure Assign(Other: TGeneDupsInfo);
      function WriteToFile(var SessionFile: File; SessionVersion: Integer): Boolean;
      function ReadFromFile(var SessionFile: File; SessionVersion: Integer): Boolean;
  end;
  TAnalysisInfo = class;

  TAnalysisInfo = class(TObject)
  private
    FDataFilename: String;
    FFixedEvolutionaryRate: Double;
    // These are only for the molecular clock test
    FLoglWClock  : AnsiString;
    FLoglWoClock : AnsiString;
    FMolClockTest : AnsiString;
    FInvarWClock  : AnsiString;
    FGammaWClock  : AnsiString;
    FInvarWoClock : AnsiString;
    FGammaWoClock : AnsiString;
    FFullMdlName  : AnsiString;
    FCodonsUsed   : AnsiString;
    FClockType: TClockType;
    FClockLevel: TClockLevel;
    FMergeRates: Boolean;
    FDegreesOfFreedom: AnsiString;
    FIsMyPegAnalysis: Boolean;
    FMyProcessPack: TProcessPack;
//    FOptimizeBLens: Boolean;
    FMyUsedOtuInfos: TList;
    FPropagateConstraints: Boolean;
    FTransposeData: Boolean;
    FUseFixedEvolutionaryRate: Boolean;
    FUserInputTreeIsRooted: Boolean;
    FWorkflowElementFinishedCallBack: TWorkflowElementFinishedCallBack;
    TextAnalysisStrings : TStringList;
    MySiteLabelCaption: AnsiString;
    FMaxRateRatio: Extended;
    FGroupInfo: TGroupInfo;
    FMyBootReps: Integer;
    FNeedApplinkerOptions: Boolean;
    FNumSitesPerSample: Integer;
    FNumRepsPerSubSample: Integer;
    FNumBootstrapSubSamples: Integer;
    {$IFNDEF VISUAL_BUILD}
    FBootstrapSampleSizeFactor: Double;
    {$ENDIF}
    procedure ProcessPackFreedNotify(Sender: TObject);
    function GetIngroupDefined: Boolean;
    function GetIsRtdt: Boolean;
    function GetMyBootReps: Integer;
    function GetNumTaxa: LongInt;
    function GetOperatingSystem: String;
    function GetUserTreeFileNameOnly: String;
    function GetUsesCalibrationDensities: Boolean;
    procedure PrivateAddDataSubsetInfoToRuntimeProgress;
    procedure SetFixedEvolutionaryRate(AValue: Double);
    procedure SetMyBootReps(AValue: Integer);
    procedure SetMyProcessPack(AValue: TProcessPack);
    procedure SetNumBootstrapSubSamples(AValue: Integer);
    procedure SetNumRepsPerSubSample(AValue: Integer);
    procedure SetNumSitesPerSubSample(AValue: Int64);
    procedure SetPropagateConstraints(AValue: Boolean);
    procedure SetupSiteLabelTypeOptions;
    procedure FetchSiteLabelTypeOptions;
    //From AnalysisPrefsDlg
    function  IsIncludeOnlyUnlabelledSites:Boolean;
    function  IsIncludeOnlyLabelledSites:Boolean;
    function  HasSiteLabelTypes:Boolean;
    function GetIsCompleteDeletion: Boolean;
    function GetIsPairwiseDeletion: Boolean;
    function GetIsPartialDeletion: Boolean;
    function GetSiteCoverage: Integer;
    function GetSiteCoverageReverse: Integer;
    function GetIsPairwiseStyleAnalysis: Boolean;
    function GetCodonPositions: AnsiString;
    function GetSiteLabelsCaption: AnsiString;
    function GetIsCoding: Boolean;
    function GetHasDistPack: Boolean;
    function GetHasCalibrations: Boolean;
    function GetDistComputationTypeTitleCaption: AnsiString;
    function GetDistComputationTypeShortCaption: AnsiString;
    function GetDistUnitCaption: AnsiString;
    function GetDistGammaParaCaption: AnsiString;
    function GetDistHeteroPatternCaption: AnsiString;
    function GetDistModelName: AnsiString;
    function GetKumarGadagkar2001InfoCaption: AnsiString;
    function GetIsAminoAcid: boolean;
    function GetAnalysisPrefsChosen: TStrings;
    function GetGlobalClockType: Integer;
    function GetMLBlensFilter: Extended;
    function GetGeneTreeRooted: Boolean;
    function GetIsGeneDupsTree: Boolean;
    function GetNumConfigs: Integer;
    function GetNumGeneDups: Integer;
    function GetNumSpeciations: Integer;
    function GetNumSpecies: Integer;
    function GetSpeciesTreeRooted: Boolean;
    function GetUsedSpeciesTree: Boolean;
    procedure SetGeneTreeRooted(const Value: Boolean);
    procedure SetIsGeneDupsTree(const Value: Boolean);
    procedure SetNumConfigs(const Value: Integer);
    procedure SetNumGeneDups(const Value: Integer);
    procedure SetNumSpeciations(const Value: Integer);
    procedure SetNumSpecies(const Value: Integer);
    procedure SetUsedSpeciesTree(const Value: Boolean);
    function GetGeneDupsFound: Boolean;
    procedure SetSpeciesTreeRooted(const Value: Boolean);
    function GetGeneDupsRootIndex: Integer;
    procedure SetGeneDupsRootIndex(const Value: Integer);
    function GetOutgroupDefined: Boolean;
    function GetNumCalibrations: Integer;
    procedure SetMergeRates(const Value: Boolean);
    function GetReltimeVarianceMethod: TReltimeVarianceMethod;
    procedure SetMyOriTreeList(const Value: TTreeList);
    function GetMaxRateRatio: Extended;
    function GetMyGpNames: TStringList;
    procedure SetMyGpNames(const Value: TStringList);
    function GetGroupInfo: TGroupInfo;
    function GetMyOutgroupMembers(Index: Integer): Boolean;
    procedure SetMyOutgroupMembers(Index: Integer; const Value: Boolean);
    procedure SetMyOtuInfos(const Value: TList);
    procedure SetUseFixedEvolutionaryRate(AValue: Boolean);
    procedure AddCodonPositionSubsets;
    function NumThreadsToUseCC: Integer;
    procedure SetupDeletionOption; // Complete/Pairwise/Partial Deletion
    function IsRecodeBases:boolean;
    procedure CheckCodonBasedTestCorrectData(CurOperation: TDistTreeDlgOption);
    {$IFDEF VISUAL_BUILD}
    procedure InitAnalysisPrefsDlg(CurOperation: TDistTreeDlgOption);
    {$ENDIF}
    procedure SaveUsageStats(stats: TStrings);
    procedure AddChosenOptionsToRuntimeProgress(options: TStrings);
    procedure UpdateClockSettings(CurOperation: TDistTreeDlgOption);
    procedure FinalizeCodonOmegaOptions;
    procedure FinalizeMLSubsetOptions;
    procedure FinalizeMLThreadAndInitialTreeOptions(CurOperation: TDistTreeDlgOption);
    procedure FinalizeMPSubsetOptions(CurOperation: TDistTreeDlgOption);
    procedure FinalizeDistSubsetOptions(CurOperation: TDistTreeDlgOption);
    procedure ConstructTreePack(CurOperation: TDistTreeDlgOption; options: TStringList);
    procedure ConstructDistPack(CurOperation: TDistTreeDlgOption; options: TStringList);
  public
    DistCommandResultsWrapper: TObject;
    ReltimeNexusExport: TStringList;
    AnalysisSummary: TAnalysisSummary;
    MyGeneDupsInfo: TGeneDupsInfo;
    MyNoOfSeqs:       LongInt;
    MyNoOfSites:      LongInt;
    MySeqStrings:     TStringList;     // for ML analysis
    MyOrigSeqStrings: TStringList;
    MySeqPartitions:  TListOfSeqPartitions;
    MyMappedData:     TList;
    MyInfoSites:      TList;
    MyNoOfStates:     Integer;
    MyNoOfInfoSites:  Integer;
    MyConstContribute:Integer;
    MyTvWeight:       Integer;

    MyPegSite: Integer;                // for myPEG

    MyLabelsUsed:        TStringList; // for holding labels used in the analysis
    MyRecodeScheme:      AnsiString; // for recoding data
    MyRecodeSchemeName:  AnsiString; // for recoding data

    MyNoOfGammaCat:  Integer;        // for ML
    MySiteCoverage:  Integer;        // for Partial Deletion

      // for parsimony analysis
    MySearchFactor: Integer;
    MySearchLevel:  Integer;
    MyMaxNoOfTrees: Integer;
      // for non-overlapping groups
    MyNoOfGps:      LongInt;
    MyGpIds:        ArrayOfInteger;
      // for clusters
    MyNoOfClusters: LongInt;
    MyClusterIds:   ArrayOfInteger;
    MySubsetOpt:    TDataSubsetOptions;
       // for instantaneous info on OTU Infos

      // For getting OTU names, you must use this to keep consistant
    MyOtuNames    : TStringList;

      // Option for tree-making
    MyDistPack:     TDistPack;
    MyTreePack:     TTreePack;
    MyMLAnalysisPack: TMLTreeAnalyzer;               // for ML
    MyRelTimeComputer: TRelTimeComputer;
    MyNumThreadsToUse: Integer; // user-defined number of threads for multi-threaded analyses
    MyInitialTreeMethod: Integer; // user-defined method for auto-generating an initial tree when performing ML inference
      //
    MySiteFreqs:    ArrayOfInteger;
    MyNoOfReps :    Integer;
    MyValidReps:    Integer;

    MyUserTreeFile: AnsiString;
    MyUserNewickTree: AnsiString;

    GlobalClockType:    Integer;
    ClockTypeSet:       Boolean;
    ClockTreeExport:    TStringList;
    DeveloperLog: TStringList;
    CalibrationFile: String;
    MyMLBLensFilter: Extended;
    MyUsrOperation : TDistTreeDlgOption;

    DistComputer:   TSeqDistBase;  // use for Computing distances

    ARP:            TRuntimeProgress;
    MyShowD   : PDistanceMatrix;
    MyOriD    : PDistanceMatrix;
    MyBootD   : PDistanceMatrix;
    MyBootTree         : TTreeData;
    FMyOriTreeList      : TTreeList;
    MyBootPartitionList: TPartitionList;
   CalibrationTimes: TCalibrations;
   NeedToFreeOtuInfos: Boolean;
   LastUpdateTime: TDateTime;
   UsingAlignmentAndTreeThatDontResolve: Boolean;
   procedure AddSubsampleSeqs(subsampleSeqs: TStringList);
   constructor Create;
   destructor  Destroy; override;
   function GetPickListSettingValue(pickListName: String): String;
   function TransferTreeOutgroupInfoToOtuInfos(TreeIndex: Integer): Boolean; overload;
   function TransferTreeOutgroupInfoToOtuInfos(OutgroupInfo: TOutgroupNameValuePairArray): Boolean; overload;
   function SeqStringsToMegaFile(filename: String; title: String=''; dtype: String=''): Boolean;
   function GetOtuNamesList: TStringList;
   function UpdateProgress(Progress: integer; Status: AnsiString): boolean; { a standin for the CheckCancel function so that we can update progress in the Process...Command procedures}
   function LoadGroupAndOutgroupInfo(Filename: String; NamesList: TStringList): Boolean; { this is for the case where the user is doing reltime with only blens because UsedOtuInfos does not get setup}
   procedure Assign(Source: TAnalysisInfo);
   procedure SetIsGeneDups(Value: Boolean);
   procedure SetupUsedOtuInfos(CurOperation: TDistTreeDlgOption);
   procedure SetupOtuNamesFromTreeList;
   procedure SetupOutgroupMembers;
   procedure SetupGroupIds; // assumes that SetupUsedOtuInfo is already called
   function GetAnalysisOptions(CurOperation: TDistTreeDlgOption): Boolean; // called by routines here
   {$IFDEF VISUAL_BUILD}
   function GetMegaAndApplinkerOptions(CurOperation: TDistTreeDlgOption): Boolean;
   {$ENDIF}
   function GetUserTreeInput(CurOperation: TDistTreeDlgOption; AFileName: String; ARP: TRuntimeProgress; TaxaNames: TStringList=nil; DefaultTree: TTreeType = ttNone; UserTreeRequired: Boolean = False): Boolean; // Continue vs. Abort process
   function GetRelTimeUserTreeInput(FileName: String; ARP: TRuntimeProgress; OtuInfos: TList): Boolean;
   function GetUCSC_46_SpeciesTree: Boolean;
   procedure FillThreeOutInfos(AList: TList);  // used for Tajima's test
   function GetOptionValue(OptionName: AnsiString):AnsiString;
   function IsAncestralSeqsOperation: Boolean;
   function DataTitle: String;
   function ReadFromFile(var SessionFile: File; SessionVersion: LongInt):boolean;
   procedure WriteToFile(var SessionFile: File; SessionVersion: LongInt);
   function StateToStringList(Comment: String=''): TStringList;
   function SubSetOptionsToStringList: TStringList;
   function SequencesToStringList: TStringList;
   function UpdateSpNamesInTreeList: Boolean;
   function UniqueSpeciesNamesToStringList: TStringList;
   function AllSpeciesNamesToStringList: TStringList;
   function AllOtusHaveSpName: Boolean;
   function AtleastOneOtuHasSpName: Boolean;
   function MyOutgroupMembersArray: TBoolArray;
   function GetUsedOtuNames: TStringList;
   property MyOutgroupMembers[Index: Integer]: Boolean read GetMyOutgroupMembers write SetMyOutgroupMembers;
   property PropagateConstraints: Boolean read FPropagateConstraints write SetPropagateConstraints;
   property MyProcessPack:  TProcessPack read FMyProcessPack write SetMyProcessPack;
   property UserInputTreeIsRooted: Boolean read FUserInputTreeIsRooted;
  published
    property TransposeData: Boolean read FTransposeData write FTransposeData;
    property OperatingSystem: String read GetOperatingSystem;
    property MyBootReps: Integer read GetMyBootReps write SetMyBootReps;
    property UseFixedEvolutionaryRate: Boolean read FUseFixedEvolutionaryRate write SetUseFixedEvolutionaryRate;
    property FixedEvolutionaryRate: Double read FFixedEvolutionaryRate write SetFixedEvolutionaryRate;
    property UserTreeFileNameOnly: String read GetUserTreeFileNameOnly;
    property MyUsedOtuInfos: TList read FMyUsedOtuInfos write SetMyOtuInfos;
    property MyOriTreeList: TTreeList read FMyOriTreeList write SetMyOriTreeList;
    property NumCalibrations: Integer read GetNumCalibrations;
    property DataFilename: String read FDataFilename write FDataFilename;
    property OutgroupDefined: Boolean read GetOutgroupDefined;
    property IngroupDefined: Boolean read GetIngroupDefined;
    property IsGeneDupsTree: Boolean read GetIsGeneDupsTree write SetIsGeneDupsTree;
    property NumSpeciations: Integer read GetNumSpeciations write SetNumSpeciations;
    property NumGeneDups: Integer read GetNumGeneDups write SetNumGeneDups;
    property GeneDupsRootIndex: Integer read GetGeneDupsRootIndex write SetGeneDupsRootIndex;
    property UsedSpeciesTree: Boolean read GetUsedSpeciesTree write SetUsedSpeciesTree;
    property GeneTreeRooted: Boolean read GetGeneTreeRooted write SetGeneTreeRooted;
    property SpeciesTreeRooted: Boolean read GetSpeciesTreeRooted write SetSpeciesTreeRooted;
    property NumSpecies: Integer read GetNumSpecies write SetNumSpecies;
    property NumConfigs: Integer read GetNumConfigs write SetNumConfigs;
    property GeneDupsFound: Boolean read GetGeneDupsFound;
    property IsMyPegAnalysis: Boolean read FIsMyPegAnalysis write FIsMyPegAnalysis;
    property InitialUsrOperation   : TDistTreeDlgOption read MyUsrOperation write MyUsrOperation;
    property NoOfSeqs       : LongInt read MyNoOfSeqs;
    property NoOfTaxa       : LongInt read GetNumTaxa; { handles the case where only a tree is used}
    property NoOfSites      : LongInt read MyNoOfSites;
    property NoOfStates     : Integer read MyNoOfStates;
    property NoOfInfoSites  : Integer read MyNoOfInfoSites;
    property NoOfGammaCat   : Integer read MyNoOfGammaCat;
    property ConstContribute: Integer read MyConstContribute;
    property TvWeight       : Integer read MyTvWeight;
    property AnalysisPrefsChosen : TStrings read GetAnalysisPrefsChosen;
       // recode scheme
    property RecodeScheme    :   AnsiString read MyRecodeScheme;     // for recoding data
    property RecodeSchemeName:   AnsiString read MyRecodeSchemeName; // for recoding data
      // for parsimony analysis
    property SearchFactor   : Integer read MySearchFactor;
    property SearchLevel    : Integer read MySearchLevel;
    property MaxNoOfTrees    : Integer read MyMaxNoOfTrees;
      // for non-overlapping groups
    property NoOfGps        : LongInt read MyNoOfGps;

    property BootReps       : Integer read GetMyBootReps write SetMyBootReps;
    property NoOfReps       : Integer read MyNoOfReps;
    property ValidReps      : Integer read MyValidReps;
    property IsBootstrapSubSampling: Boolean read GetIsBootstrapSubSampling;
    property NumBootstrapSubSamples: Integer read GetNumBootstrapSubSamples write SetNumBootstrapSubSamples;
    property NumRepsPerSubSample: Integer read GetNumRepsPerSubSample write SetNumRepsPerSubSample;
    property NumSitesPerSubSample: Int64 read GetNumSitesPerSubSample write SetNumSitesPerSubSample;
    property IsPairwiseStyleAnalysis: Boolean read GetIsPairwiseStyleAnalysis;
    property IsCompleteDeletion : Boolean read GetIsCompleteDeletion;
    property IsPairwiseDeletion : Boolean read GetIsPairwiseDeletion;
    property IsPartialDeletion  : Boolean read GetIsPartialDeletion;
    property SiteCoverage       : Integer read GetSiteCoverage;
    property SiteCoverageReverse: Integer read GetSiteCoverageReverse;

    property CodonPositions     : AnsiString read GetCodonPositions;
    property SiteLabelsCaption  : AnsiString read GetSiteLabelsCaption;

    property IsCoding : Boolean read GetIsCoding; // Why is this locked away in SeqDataViewer, breaks MVC
    property HasDistPack : Boolean read GetHasDistPack;
    property HasCalibrations: Boolean read GetHasCalibrations;

    // Properties for Captions

    property DistComputationTypeTitleCaption: AnsiString read GetDistComputationTypeTitleCaption;
    property DistComputationTypeShortCaption: AnsiString read GetDistComputationTypeShortCaption;
    property DistUnitCaption                : AnsiString read GetDistUnitCaption;
    property DistGammaParaCaption           : AnsiString read GetDistGammaParaCaption;
    property DistHeteroPatternCaption       : AnsiString read GetDistHeteroPatternCaption;
    property KumarGadagkar2001InfoCaption   : AnsiString read GetKumarGadagkar2001InfoCaption;
    property DistModelName                  : AnsiString read GetDistModelName;
    property UserTreeFile                   : AnsiString read MyUserTreeFile;
    property isAminoAcid                    : boolean read GetIsAminoAcid;

    // These properties only applicable to the clock analyses
    property IsRtdt: Boolean            read GetIsRtdt;
    property UsesCalibrationDensities   : Boolean read GetUsesCalibrationDensities;
    property LogLikelihoodWithClock     : AnsiString read FLoglWClock write FLogLWClock;
    property LogLikelihoodWithoutClock  : AnsiString read FLoglWoClock write FLogLWoClock;
    property MolecularClockTest         : AnsiString read FMolClockTest write FMolClockTest;
    property InvarWithClock             : AnsiString read FInvarWClock write FInvarWClock;
    property GammaWithClock             : AnsiString read FGammaWClock write FGammaWClock;
    property InvarWithoutClock          : AnsiString read FInvarWoClock write FInvarWoClock;
    property GammaWithoutClock          : AnsiString read FGammaWoClock write FGammaWoClock;
    property DegreesOfFreedom           : AnsiString read FDegreesOfFreedom write FDegreesOfFreedom;
    property FullModelName              : AnsiString read FFullMdlName write FFullMdlName;
    property CodonPositionsUsed         : AnsiString read FCodonsUsed write FCodonsUsed;
    property MLBLensFilter              : Extended read GetMLBlensFilter;
    property ClockType: TClockType read FClockType write FClockType;
    property ClockLevel: TClockLevel read FClockLevel write FClockLevel;
    property MergeRates: Boolean read FMergeRates write SetMergeRates;
    property ReltimeVarianceMethod: TReltimeVarianceMethod read GetReltimeVarianceMethod;
    property MaxRateRatio: Extended read GetMaxRateRatio write FMaxRateRatio;
    property MyGpNames: TStringList read GetMyGpNames write SetMyGpNames;
    property GroupInfo: TGroupInfo read GetGroupInfo;

    // for the BEAM analysis
    property MaxPercentSnvDiffForIdentSeqs: Double read GetMaxPercentSnvDiffForIdentSeqs write SetMaxPercentSnvDiffForIdentSeqs;

  end;

  type
    TPairwiseDistances = array of Double;


implementation

uses
  dateutils, math,
  {$IFDEF VISUAL_BUILD}
  MTreeInputForm, Mega_Main, MPleaseWait,
  {$ELSE}
  MD_MegaMain, MegaUtils_NV, strutils,
  {$ENDIF}
  Sysutils, MegaUtils, Controls,
  ErrorMessages_HC, MAminoDist, MNucDist, MSynNonsynDist, MegaAnalysisPrefStrings,
  DistanceUtils, TypInfo, MUsageStatistics,
  MLTree;

function TAnalysisInfo.AllOtusHaveSpName: Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Assigned(FMyUsedOtuInfos) then { is nil for tree session files}
    Exit;
  for i := 0 to FMyUsedOtuInfos.Count - 1 do
  begin
    if Trim(TOtuInfo(FMyUsedOtuInfos[i]).SpName) = EmptyStr then
      Exit;
  end;
  Result := True;
end;

function TAnalysisInfo.AllSpeciesNamesToStringList: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to FMyUsedOtuInfos.Count - 1 do
    Result.Add(TOtuInfo(FMyUsedOtuInfos[i]).SpName);
end;

procedure TAnalysisInfo.Assign(Source: TAnalysisInfo);
var
  i: Integer;
  AOption: dsoDataSubsetOption;
  TempInfo: TOtuInfo;
  AInfo: TOtuInfo;
  ATree: TTreeData;
  AModel: TGammaRateVariationModel;
  AIntSeq: PArrayOfInt;
  ACharSeq: PAnsiChar;
begin
  FUseFixedEvolutionaryRate := Source.UseFixedEvolutionaryRate;
  FFixedEvolutionaryRate := Source.FixedEvolutionaryRate;
  FDataFilename := Source.DataFilename;
  LogLikelihoodWithClock  := Source.FLoglWClock;
  LogLikelihoodWithoutClock := Source.FLoglWoClock;
  MolecularClockTest := Source.FMolClockTest;
  InvarWithClock := Source.FInvarWClock;
  InvarWithoutClock := Source.FInvarWoClock;
  GammaWithClock := Source.FGammaWClock;
  GammaWithoutClock := Source.FGammaWoClock;
  FullModelName := Source.FFullMdlName;
  CodonPositionsUsed := Source.FCodonsUsed;
  ClockType := Source.FClockType;
  ClockLevel := Source.FClockLevel;
  MergeRates := Source.FMergeRates;
  DegreesOfFreedom := Source.FDegreesOfFreedom;
  IsMyPegAnalysis := Source.FIsMyPegAnalysis;

  if Assigned(TextAnalysisStrings) then
  begin
    TextAnalysisStrings := TStringlist.Create;
    TextAnalysisStrings.AddStrings(Source.TextAnalysisStrings);
  end;
  MySiteLabelCaption := Source.MySiteLabelCaption;
  SetIsGeneDups(Source.IsGeneDupsTree);
  if Source.IsGeneDupsTree then
    MyGeneDupsInfo.Assign(Source.MyGeneDupsInfo);
  MyNoOfSeqs := Source.MyNoOfSeqs;
  MyNoOfSites := Source.MyNoOfSites;
  if Assigned(Source.MySeqStrings) then
  begin
    MySeqStrings := TStringlist.Create;
    MySeqStrings.AddStrings(Source.MySeqStrings);
  end;
  if Assigned(Source.MySeqPartitions) then
  begin
    MySeqPartitions := TListOfSeqPartitions.Create;
    for i := 0 to Source.MySeqPartitions.Count - 1 do
    begin
      MySeqPartitions.Add(TStringList.Create);
      MySeqPartitions[i].AddStrings(Source.MySeqPartitions[i]);
    end;
  end;
  if Assigned(Source.MyMappedData) then
  begin
    MyMappedData := TList.Create;
    if Source.MyMappedData.Count > 0 then
    begin
      for i := 0 to MyMappedData.Count - 1 do
      begin
        AInfo := TOtuInfo.Create;
        AInfo.Assign(TOtuInfo(Source.MyMappedData[i]));
        MyMappedData.Add(AInfo);
      end;
    end;
  end;
  if Assigned(Source.MyInfoSites) then
  begin
    if not Assigned(MyInfoSites) then
      MyInfoSites := TList.Create;
    if Source.MyInfoSites.Count > 0 then
    begin
      for i := 0 to Source.MyInfoSites.Count - 1 do
      begin
        if (dsoUseAmino in MySubsetOpt) and (dsoParsimMap in MySubsetOpt) then
        begin
          GetMem(AIntSeq, sizeOf(LongInt)*(NoOfSites+1));
          Move(Source.MyInfoSites[i], AIntSeq, sizeOf(LongInt)*(NoOfSites+1));
          MyInfoSites.Add(AIntSeq);
        end
        else
        begin
          GetMem(ACharSeq, sizeOf(AnsiChar)*(NoOfSites+1));
          Move(Source.MyInfoSites[i], ACharSeq, sizeOf(AnsiChar)*(NoOfSites+1));
          MyInfoSites.Add(ACharSeq);
        end;
      end;
    end;
  end;

  MyNoOfStates := Source.MyNoOfStates;
  MyNoOfInfoSites := Source.MyNoOfInfoSites;
  MyConstContribute := Source.MyConstContribute;
  MyTvWeight := Source.MyTvWeight;
  MyPegSite := Source.MyPegSite;
  if Assigned(Source.MyLabelsUsed) then
  begin
    MyLabelsUsed := TStringList.Create;
    if Source.MyLabelsUsed.Count > 0 then
    begin
      for i := 0 to Source.MyLabelsUsed.Count - 1 do
        MyLabelsUsed.Add(Source.MyLabelsUsed[i]);
    end;
  end;
  MyRecodeScheme := Source.MyRecodeScheme;
  MyRecodeSchemeName := Source.MyRecodeSchemeName;
  MyNoOfGammaCat := Source.MyNoOfGammaCat;
  MySiteCoverage := Source.MySiteCoverage;
  MySearchFactor := Source.MySearchFactor;
  MySearchLevel := Source.MySearchLevel;
  MyMaxNoOfTrees := Source.MyMaxNoOfTrees;
  MyNoOfGps := Source.MyNoOfGps;
  MyGpIds := Source.MyGpIds;
  MyNoOfClusters := Source.MyNoOfClusters;
  if Length(Source.MyClusterIds) > 0 then
  begin
    SetLength(MyClusterIds, Length(Source.MyClusterIds));
    for i := Low(MyClusterIds) to High(MyClusterIds) do
      MyClusterIds[i] := Source.MyClusterIds[i];
  end;
  for AOption in Source.MySubsetOpt do
  begin
    MySubsetOpt := MySubsetOpt + [AOption];
  end;

  if Assigned(Source.FMyUsedOtuInfos) then
  begin
	  FMyUsedOtuInfos := TList.Create;
    if Source.FMyUsedOtuInfos.Count > 0 then
      for i := 0 to Source.FMyUsedOtuInfos.Count - 1 do
      begin
        TempInfo := TOtuInfo.Create;
        TempInfo.Assign(TOtuInfo(Source.FMyUsedOtuInfos[i]));
    		FMyUsedOtuInfos.Add(TempInfo);
      end;
  end;
  if Assigned(Source.MyOtuNames) then
  begin
    MyOtuNames := TStringList.Create;
    if Source.MyOtuNames.Count > 0 then
      MyOtuNames.Assign(Source.MyOtuNames);
  end;

  if Assigned(Source.FGroupInfo) then
    FGroupInfo.Assign(Source.FGroupInfo);

  if Assigned(Source.MyDistPack) then
  begin
    MyDistPack := TDistPack.Create;
    MyDistPack.Assign(Source.MyDistPack);
  end;
  if Assigned(Source.MyTreePack) then
  begin
    MyTreePack := TTreePack.Create;
    MyTreePack.Assign(Source.MyTreePack);
  end;
  if Assigned(Source.MyMLAnalysisPack) then
  begin
    ATree := Source.MyMLAnalysisPack.InitTree.Clone;
    AModel := TGammaRateVariationModel.Create(Source.MyMLAnalysisPack.Model.Gamma, Source.MyMLAnalysisPack.Model.UseInvar, Source.MyMLAnalysisPack.Model.NoOfRates, Source.MyMLAnalysisPack.Model.NoOfThreads);
    MyMLAnalysisPack := TMLTreeAnalyzer.Create(MySeqStrings, ATree, AModel);
    MyMLAnalysisPack.Assign(Source.MyMLAnalysisPack);
  end;
  if Assigned(Source.MyRelTimeComputer) then
  begin
    MyRelTimeComputer := TRelTimeComputer.Create;
    MyRelTimeComputer.Assign(Source.MyRelTimeComputer);
  end;
	MyNumThreadsToUse := Source.MyNumThreadsToUse;
	MyInitialTreeMethod := Source.MyInitialTreeMethod;
	MySiteFreqs := Source.MySiteFreqs;
	MyBootReps := Source.MyBootReps;
	MyNoOfReps := Source.MyNoOfReps;
	MyValidReps := Source.MyValidReps;
	MyUserTreeFile := Source.MyUserTreeFile;
	MyUserNewickTree := Source.MyUserNewickTree;

	GlobalClockType := Source.GlobalClockType;
	ClockTypeSet := Source.ClockTypeSet;
  if Assigned(Source.ClockTreeExport) then
  begin
    ClockTreeExport := TStringList.Create;
    ClockTreeExport.AddStrings(Source.ClockTreeExport);
  end;
	CalibrationFile := Source.CalibrationFile;
	MyMLBLensFilter := Source.MyMLBLensFilter;
	MyUsrOperation := Source.MyUsrOperation;
//  DistComputer:   TSeqDistBase;  { skipping dist computer because it would be a lot of work but there is no scenario where it will be used from a copy of TAnalysisInfo}
  DistComputer := nil;
	ARP := TRuntimeProgress.Create(nil);

  if Assigned(Source.MyShowD) then
  begin
    MyShowD := CreateDistanceMatrix(MyNoOfSeqs);
    CopyDistanceMatrix(Source.MyShowD, MyShowD, MyNoOfSeqs);
  end;
  if Assigned(Source.MyOriD) then
  begin
    MyOriD := CreateDistanceMatrix(MyNoOfSeqs);
    CopyDistanceMatrix(Source.MyOriD, MyOriD, MyNoOfSeqs);
  end;
  if Assigned(Source.MyBootD) then
  begin
    MyBootD := CreateDistanceMatrix(MyNoOfSeqs);
    CopyDistanceMatrix(Source.MyBootD, MyBootD, MyNoOfSeqs);
  end;
  if Assigned(Source.MyBootTree) then
    MyBootTree := Source.MyBootTree.Clone;
  if Assigned(Source.FMyOriTreeList) then { keep in mind the TTreeViewForm sets it to nil so in that case it will have to be copied after this Assign function}
  begin
    FMyOriTreeList := TTreeList.Create;
    FMyOriTreeList.Assign(Source.FMyOriTreeList);
  end;
  if Assigned(Source.MyBootPartitionList) then { TTreeViewForm also sets this to nil so in that case it will have to be copied after this Assign function}
  begin
    MyBootPartitionList := TPartitionList.Create(MyNoOfSeqs, Source.MyBootPartitionList.MaxNoOfTrees, Source.MyBootPartitionList.isRooted);
    MyBootPartitionList.Assign(Source.MyBootPartitionList);
  end;
  if Assigned(Source.CalibrationTimes) and (Source.CalibrationTimes.Count > 0) then
  begin
    CalibrationTimes := TCalibrations.Create;
    CalibrationTimes.Assign(Source.CalibrationTimes);
  end;
  NeedToFreeOtuInfos := True;
end;

function TAnalysisInfo.AtleastOneOtuHasSpName: Boolean;
var
  i: Integer;
begin
  Result := False;
  try
    if (not Assigned(FMyUsedOtuInfos)) or (FMyUsedOtuInfos.Count = 0) then
      Exit;
    for i := 0 to FMyUsedOtuInfos.Count - 1 do
    begin
      if Trim(TOtuInfo(FMyUsedOtuInfos[i]).SpName) <> EmptyStr then
      begin
        Result := True;
        Exit;
      end;
    end;
  except
    // just return false
  end;
end;

constructor TAnalysisInfo.Create;
begin
  inherited create;
  FNeedApplinkerOptions := False;
  FTransposeData := False;
  FUserInputTreeIsRooted := False;
  FWorkflowElementFinishedCallBack := nil;
  FUseFixedEvolutionaryRate := False;
  FFixedEvolutionaryRate := 1;
  FPropagateConstraints := True;
  FMaxRateRatio := DEFAULT_MAX_RATE_RATIO;
  UsingAlignmentAndTreeThatDontResolve := False;
  ReltimeNexusExport := nil;
  LastUpdateTime := Time;
  FDataFilename := EmptyStr;
  AnalysisSummary := TAnalysisSummary.Create;
  NeedToFreeOtuInfos := False;
  MyGeneDupsInfo := nil;
  MyPegSite := 1;
  FIsMyPegAnalysis := False;
  FClockLevel := clUnknown;
  FClockType := ctUnknown;
  FMergeRates := False;

  MyNoOfSeqs := 0;
  MyNoOfSites:= 0;
  MyMappedData:=nil;

  MySeqStrings := nil;
  MySeqPartitions := nil;
  MyInfoSites := nil;
  MyNoOfStates:= 0;

  MyLabelsUsed := nil;
  MyNoOfGps:=0;
  MyGpIds:= nil;
  MySubsetOpt:= [];

  FMyUsedOtuInfos  := nil;
  MyDistPack      := nil;
  MyTreePack      := nil;
  MyMLAnalysisPack:= nil;
  MyRelTimeComputer := nil;

  MySiteLabelCaption := '';

  DistComputer := nil;
  ARP := nil;

  MyNumThreadsToUse := 1;
  ClockTreeExport := nil;
  CalibrationTimes := nil;
  FGroupInfo := TGroupInfo.Create(nil);
  FMyProcessPack := nil;
end;

destructor  TAnalysisInfo.Destroy;
var
  i: Integer;
begin
  if Assigned(DeveloperLog) then
    DeveloperLog.Free;
  if Assigned(ReltimeNexusExport) then
    ReltimeNexusExport.Free;
  if Assigned(AnalysisSummary) then
    AnalysisSummary.Free;
  if Assigned(MyGeneDupsInfo) then
    MyGeneDupsInfo.Free;

  if Assigned(MySeqPartitions) then
    MySeqPartitions.Free;

//  if Assigned(MySeqStrings) then  { this will always be free by TMLTreeAnalyzer if it exists}
//    FreeAndNil(MySeqStrings);

  if MyMLAnalysisPack <> nil then
    FreeAndNil(MyMLAnalysisPack);

  if Assigned(MyRelTimeComputer) then
    FreeAndNil(MyRelTimeComputer);

  if MyMappedData <> nil then
  begin
    for i:= 0 to MyMappedData.Count-1 do
    begin
      if MyMappedData[i] <> nil then
        FreeMem(MyMappedData[i]);
      MyMappedData[i] := nil;
    end;
    MyMappedData.Free;
  end;

  if DistComputer <> nil then
  begin
     if     DistComputer is TNucDist       then  TNucDist(DistComputer).Free
    else if DistComputer is TSynNonsynDist then  TSynNonsynDist(DistComputer).Free
    else if DistComputer is TAminoDist     then  TAminoDist(DistComputer).Free;
  end;

  if MyShowD <> nil then FreeDistMatrix(MyShowD, MyNoOfSeqs);
  if MyOriD  <> nil then FreeDistMatrix(MyOriD, MyNoOfSeqs);
  if MyBootD <> nil then FreeDistMatrix(MyBootD, MyNoOfSeqs);
  if MyBootTree <> nil then MyBootTree.Free;
  if FMyOriTreeList <> nil then
  begin
    FMyOriTreeList.Free;
    FMyOriTreeList := nil;
  end;
  if MyBootPartitionList <> nil then MyBootPartitionList.Free;

  if MyInfoSites <> nil then
  begin
    for i := 0 to MyInfoSites.Count-1 do
	begin
      if MyInfoSites[i] <> nil then
        FreeMem(MyInfoSites[i]);
      MyInfoSItes[i] := nil;
    end;
    MyInfoSites.Free;
  end;       //@KT at MEGA 2.1

  MyNoOfSeqs := 0;
  MyNoOfSites:= 0;
  MyMappedData:=nil;

  MyInfoSites:=nil;
  MyNoOfStates:=0;

  MyNoOfGps:=0;
  MyGpIds:= nil;
  MySubsetOpt:= [];

  if MyDistPack <> nil then
    MyDistPack.Free;
  if MyTreePack <> nil then
    MyTreePack.Free;

  if FMyUsedOtuInfos <> nil then
  begin
    if NeedToFreeOtuInfos and (FMyUsedOtuInfos.Count > 0) then
      for i := 0 to FMyUsedOtuInfos.Count - 1 do
      begin
        TOtuInfo(FMyUsedOtuInfos[i]).Free;
        FMyUsedOtuInfos[i] := nil;
      end;
    FMyUsedOtuInfos.Free;
  end;

  if MyOtuNames <> nil then MyOtuNames.Free;

  if Assigned(ClockTreeExport) then
    ClockTreeExport.Free;
  if Assigned(TextAnalysisStrings) then
    FreeAndNil(TextAnalysisStrings);
  if Assigned(FGroupInfo) then
    FGroupInfo.Free;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FMyProcessPack) then
   begin
     FMyProcessPack.OnFreeNotify := nil;
     FMyProcessPack.Free;
   end;
  {$ENDIF}
end;

procedure TAnalysisInfo.AddSubsampleSeqs(subsampleSeqs: TStringList);
begin
  MyOrigSeqStrings := MySeqStrings;
  MySeqStrings := TStringList.Create;
  MySeqStrings.AddStrings(subsampleSeqs);
end;

function TAnalysisInfo.GetPickListSettingValue(pickListName: String): String;
begin
  Result := EmptyStr;
  {$IFDEF VISUAL_BUILD}
  if Assigned(AnalysisPrefDlg) then
    Result := AnalysisPrefDlg.GetPickListSettingValue(pickListName);
  {$ELSE}
  raise Exception.Create('not implemented');
  {$ENDIF}
end;

function TAnalysisInfo.TransferTreeOutgroupInfoToOtuInfos(TreeIndex: Integer): Boolean;
var
  OutgroupInfo: TOutgroupNameValuePairArray;
begin
  Result := False;
  if Assigned(MyOriTreeList) and (TreeIndex < MyOriTreeList.NoOfTrees) then
  begin
    OutgroupInfo := MyOriTreeList.GetOutgroupNameValuePairs(TreeIndex);
    Result := TransferTreeOutgroupInfoToOtuInfos(OutgroupInfo);
  end;
  SetLength(OutgroupInfo, 0);
end;

function TAnalysisInfo.TransferTreeOutgroupInfoToOtuInfos(OutgroupInfo: TOutgroupNameValuePairArray): Boolean;
var
  i, j: Integer;
  AInfo: TOtuInfo;
begin
  for i := 0 to MyUsedOtuInfos.Count - 1 do
  begin
    AInfo := TOtuInfo(MyUsedOtuInfos[i]);
    for j := 0 to Length(OutgroupInfo) - 1 do
    begin
      if AInfo.Name = OutgroupInfo[j].TaxonName then
      begin
        AInfo.OutgroupMember := OutgroupInfo[j].IsOutgroupMember;
        break;
      end;
    end;
  end;
  Result := True;
end;

function TAnalysisInfo.SeqStringsToMegaFile(filename: String; title: String=''; dtype: String=''): Boolean;
var
  target: TextFile;
  temp: String;
  i: Integer;
begin
  Result := False;
  if not Assigned(MySeqStrings) then
    raise Exception.Create('SeqStringsToMegaFile only avaiable for ML analyses');
  try
    AssignFile(target, filename);
    Rewrite(target);
    temp := '#MEGA';
    WriteLn(target, temp);
    if Trim(title) <> EmptyStr then
    begin
     temp := '!Title ' + title + ';';
     WriteLn(target, temp);
    end;
    if Trim(dtype) <> EmptyStr then
    begin
      temp := '!Format datatype=' + dtype + ';';
      WriteLn(target, temp);
    end;
    WriteLn(target, EmptyStr);
    if MySeqStrings.Count > 0 then
      for i := 0 to MySeqStrings.Count - 1 do
      begin
        temp := '#' + MyOtuNames[i];
        WriteLn(target, temp);
        temp := MySeqStrings[i];
        WriteLn(target, temp);
      end;
    Result := FileExists(filename);
  finally
    CloseFile(target);
  end;
end;

function TAnalysisInfo.LoadGroupAndOutgroupInfo(Filename: String; NamesList: TStringList): Boolean;
var
  i: Integer;
  j: Integer;
  Temp: String;
  aGroupNames: TStringList;
begin
  Assert(Assigned(NamesList) and (NamesList.Count >  0));
  Result := False;
  if not FileExists(Filename) then
  begin
    {$IFDEF VISUAL_BUILD}
    raise Exception.Create('The specified groups file does not exist (' + Filename + ')');
    {$ELSE}
    error_nv('The specified groups file does not exist (' + Filename + ')');
    {$ENDIF}
  end;


  try
    FGroupInfo.SetTreeOutgroupFromGroupsFile(Filename);
    aGroupNames := FGroupInfo.GroupNames;
    if not aGroupNames.Count > 0 then
      raise Exception.Create('no group information found');

    { validate that each line has the form taxonName=groupName}
    for i := 0 to aGroupNames.Count - 1 do
      if (Trim(aGroupNames.Names[i]) = EmptyStr) or (Trim(aGroupNames.ValueFromIndex[i]) = EmptyStr) then
        raise Exception.Create('Invalid group information - ' + aGroupNames[i]);

    { for each taxon, search MyGpNames for an entry and update MyOutGroupMembers if needed}
    for i := 0 to NamesList.Count - 1 do
    begin
      for j := 0 to aGroupNames.Count - 1 do
      begin
        Temp := aGroupNames.Names[j];
        TrimTaxaName2(Temp);
        if SameText(NamesList[i], Temp) then
        begin
          if SameText(Trim(aGroupNames.ValueFromIndex[j]), 'outgroup') then
          begin
            MyOutgroupMembers[i] := True;
            Result := True;
          end;
          break;
        end;
      end;
    end;
  except
    on E:Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      raise Exception.Create('An error occurred when loading the groups information: ' + E.Message);
      {$ELSE}
      error_nv('An error occurred when loading the groups information: ' + E.Message, E);
      {$ENDIF}
    end;
  end;
end;

function TAnalysisInfo.MyOutgroupMembersArray: TBoolArray;
begin
  Result := FGroupInfo.GetOutgroupMembers;
end;

function TAnalysisInfo.DataTitle: String;
begin
  Result := EmptyStr;
  if ARP <> nil then Result := ARP.DataTitle;
end;

procedure TAnalysisInfo.SetupSiteLabelTypeOptions;
var
  i: Integer;
  x: AnsiChar;
begin
  MySiteLabelCaption:= '';
  {$IFDEF VISUAL_BUILD}
  with AnalysisPrefDlg do  // setting up the list of labels
  begin
    if not HasSiteLabelTypes then
      Exit;
    for i:= 0 to D_InputSeqData.NoOfSiteLabelTypes-1 do
    begin
      x := D_InputSeqData.SiteLabelType[i];
      AddSiteLabelType(x);
    end;
  end;
  {$ENDIF}
end;

function TAnalysisInfo.IsIncludeOnlyUnlabelledSites:Boolean;
begin
  {$IFDEF VISUAL_BUILD}
    result := AnalysisPrefDlg.IsIncludeOnlyUnlabelledSites;
  {$ELSE}
    result := MyProcessPack.TextualSettingsList.Values[opsLabelledSites2] = OnlyUnlabelledSitesStr;
  {$ENDIF}
end;

function TAnalysisInfo.GetNumCalibrations: Integer;
begin
  if Assigned(CalibrationTimes) then
    Result := CalibrationTimes.Count
  else
    Result := 0;
end;

function TAnalysisInfo.GetNumConfigs: Integer;
begin
  Result := 0;
  if Assigned(MyGeneDupsInfo) then
    Result := MyGeneDupsInfo.NumTreeConfigs;
end;

function TAnalysisInfo.GetNumGeneDups: Integer;
begin
  Result := 0;
  if Assigned(MyGeneDupsInfo) then
    Result := MyGeneDupsInfo.NumGeneDups;
end;

function TAnalysisInfo.GetNumSpeciations: Integer;
begin
  Result := 0;
  if Assigned(MyGeneDupsInfo) then
    Result := MyGeneDupsInfo.NumSpeciations;
end;

function TAnalysisInfo.GetNumSpecies: Integer;
begin
  Result := 0;
  if Assigned(MyGeneDupsInfo) then
    Result := MyGeneDupsInfo.NumSpecies;
end;

function TAnalysisInfo.GetIsGeneDupsTree: Boolean;
begin
  Result := Assigned(MyGeneDupsInfo);
end;

function TAnalysisInfo.IsIncludeOnlyLabelledSites:Boolean;
begin
  {$IFDEF VISUAL_BUILD}
    result := AnalysisPrefDlg.IsIncludeOnlyLabelledSites;
  {$ELSE}
    result := MyProcessPack.TextualSettingsList.Values[opsLabelledSites2] = opsLabelledSites2;
  {$ENDIF}
end;

function TAnalysisInfo.HasSiteLabelTypes:Boolean;
begin
  result := D_InputSeqData.NoOfSiteLabelTypes > 0;
end;  

procedure TAnalysisInfo.FetchSiteLabelTypeOptions;
var
  {$IFNDEF VISUAL_BUILD}
  j: Integer;
  usedLabels: AnsiString;
  {$ENDIF}
  i: Integer;
  x: AnsiChar;
begin
  MySiteLabelCaption:= EmptyStr;
  if Assigned(MyLabelsUsed) then
   FreeAndNil(MyLabelsUsed);

  if not HasSiteLabelTypes then
   Exit;

   if IsIncludeOnlyLabelledSites or IsIncludeOnlyUnlabelledSites then
   begin
     if IsIncludeOnlyUnlabelledSites then
     begin
       MySubsetOpt := MySubsetOpt + [dsoUseOnlyUnlabelledSites];
       MySiteLabelCaption := 'Unlabelled sites';
     end
     else if IsIncludeOnlyLabelledSites then
     begin
       MyLabelsUsed := TStringList.Create;
       MySubsetOpt := MySubsetOpt + [dsoUseOnlyLabelledSites];
       for i:= 0 to D_InputSeqData.NoOfSiteLabelTypes-1 do
       begin
         x := D_InputSeqData.SiteLabelType[i];
         {$IFDEF VISUAL_BUILD}
         if AnalysisPrefDlg.IsSiteLabelTypeUsed[x] then
         begin
           MyLabelsUsed.Add(x);
           MySiteLabelCaption := MySiteLabelCaption+x;
         end;
         {$ELSE}
         usedLabels := MyProcessPack.TextualSettingsList.Values[opsSelectLabelsPanel2];
         if Length(usedLabels) > 0 then
           for j := 1 to Length(usedLabels) do
             if usedLabels[j] = x then
             begin
              MyLabelsUsed.Add(x);
              MySiteLabelCaption := MySiteLabelCaption+x;
             end;
         {$ENDIF}
       end;
     end;
  end;
end;

function TAnalysisInfo.GetOptionValue(OptionName: AnsiString):AnsiString;
begin
  Result := TextAnalysisStrings.Values[OptionName];
end;

function TAnalysisInfo.IsAncestralSeqsOperation: Boolean;
begin
  Result := False;
  if (MyUsrOperation = dtdoMLInferAncSeq) or
     (MyUsrOperation = dtdoMLInferAncSeqMyPeg) or
     (MyUsrOperation = dtdoMPInferAncSeq) or
     (MyUsrOperation = dtdoMPInferAncSeqMyPeg) then
  begin
    Result := True;
  end;
end;

function TAnalysisInfo.GetOtuNamesList: TStringList;
begin
  Result := TStringList.Create;
  Result.Assign(MyOtuNames);
end;

function TAnalysisInfo.GetOutgroupDefined: Boolean;
var
  i: Integer;
begin
  Result := False;
  if (not Assigned(FMyUsedOtuInfos)) or (FMyUsedOtuInfos.Count = 0) then
    Exit;
  for i := 0 to FMyUsedOtuInfos.Count - 1 do
    if TOtuInfo(FMyUsedOtuInfos[i]).OutgroupMember then
    begin
      Result := True;
      Exit;
    end;
end;

{ For reltime analyses, the user may have a newick tree with internal node labels. If there
  is a problem with the tree, we won't provide a way to edit the tree because our tree
  editor (TTreeInputForm) would remove the internal node labels. }
function TAnalysisInfo.GetRelTimeUserTreeInput(FileName: String; ARP: TRuntimeProgress; OtuInfos: TList): Boolean;
var
  {$IFDEF VISUAL_BUILD}
  TreeDlg: TTreeInputForm = nil; // provides name checking capability as well as updates progress for very large trees
  {$ELSE}
  tempList: TStringList = nil;
  InputTreeList: TTreeList = nil; // for reading the Newick file, in the case of MEGA-CC
  FoundNames: TStringList = nil;  // the taxa names found in the Newick file
  {$ENDIF}
  ExpectedNames:TStringList = nil; // the expected taxa names from the alignment file
  NewickList: TStringList = nil; // provides an easy way to load the un-altered users newick string
  i: Integer;
begin
  Result := False;

  {$IFDEF VISUAL_BUILD}
  try
    try
      ExpectedNames := TStringList.Create;
      for i:=0 to OtuInfos.Count-1 do
        ExpectedNames.Add(TOtuInfo(OtuInfos[i]).Name);
      TreeDlg := TTreeInputForm.CreateParented(Application.MainForm.Handle);
      TreeDlg.Hide;
      TreeDlg.SetActiveDataNames(ExpectedNames);

      if FileExists(FileName) then
      begin
        if not IsPerfectTreeFileWithInternalLabels(FileName) then // check if the file is a valid newick file.
        begin
          raise Exception.Create('The contents of the provided tree file do not conform to the Newick Format standard. Please provide a valid Newick tree.');
        end
        else // To check if the number of taxa are equal, and the names all match exactly we need a list of names from the tree, to do this we essentially have to parse the tree file, so we may as well read the tree in now.
        begin
          TreeDlg.SetTreeFromFile(FileName);
          TreeDlg.InitializeActiveDataAndTreeNameMatches;
          if not TreeDlg.DoesTreeHaveAllActiveNames then
          begin
            raise Exception.Create('The taxa names in the provided Newick file do not match the expected taxa names. Please resolve the mismatches before executing this analysis.');
          end;
        end;
      end
      else
        raise Exception.Create('The Newick tree file could not be found. Please provide a valid Newick tree file.');

//      MyTreePack.TreeFile := FileName;
      MyUserTreeFile      := FileName;
      NewickList := TStringList.Create;
      NewickList.LoadFromFile(FileName);
      MyUserNewickTree    := NewickList.Text; // can't use the newick string from TreeDlg because it removes internal node labels
      Result := True;
    except
      on E: Exception do
        ShowErrorMessage(E);
    end;
  finally
    if Assigned(TreeDlg) then
      TreeDlg.Free;
    if Assigned(ExpectedNames) then
      ExpectedNames.Free;
    if Assigned(NewickList) then
      NewickList.Free;
  end;

  {$ELSE}
    if not IsPerfectTreeFileWithInternalLabels(MyTreePack.TreeFile) then
      Error_nv('The contents of the provided tree file do not conform to the Newick format standard.');

    InputTreeList := nil;
    FoundNames := nil;
    ExpectedNames := nil;
    NewickList := nil;

    try
      InputTreeList := TTreeList.Create;
      ExpectedNames := TStringList.Create;
      FoundNames := TStringList.Create;
      InputTreeList.ImportFromNewickFile(FileName, nil);
      for i:= 0 to InputTreeList.NoOfOTUs-1 do
        FoundNames.Add(InputTreeList.OTUName[i]);
      tempList :=  RegexReplaceAStringList(FoundNames, '[^A-Za-z0-9]', '');
      FoundNames.Free;
      FoundNames := tempList;
      for i:=0 to OtuInfos.Count-1 do
        ExpectedNames.Add(TOtuInfo(OtuInfos[i]).Name);
      tempList := RegexReplaceAStringList(ExpectedNames, '[^A-Za-z0-9]', '');
      ExpectedNames.Free;
      ExpectedNames := tempList;

      for i := 0 to ExpectedNames.Count - 1 do
      begin
        if FoundNames.IndexOf(ExpectedNames[i]) < 0 then
          Error_NV('The list of taxa in the tree file is ' +
            'different from the list of taxa in the sequence file, this must ' +
            'be corrected to complete this analysis.  Tree missing:' + ExpectedNames[i]);
      end;
      for i := 0 to FoundNames.Count - 1 do
        if ExpectedNames.IndexOf(FoundNames[i]) < 0 then
          Error_NV('The list of taxa in the tree file is ' +
            'different from the list of taxa in the sequence file, this must ' +
            'be corrected to complete this analysis.  Tree missing:' + ExpectedNames[i]);
      Result := True;
//      MyTreePack.TreeFile := FileName;
      MyUserTreeFile := FileName;
      NewickList := TStringlist.Create;
      NewickList.LoadFromFile(FileName);
      MyUserNewickTree := NewickList.Text; { TODO 1 -oglen -creltime : this is broken for names with underscores, ttreeinputform returns a newick string with names in quotes but we currently don't here. As a result, we get al list index out of bounds exception downstream }
    finally
      if Assigned(FoundNames) then
        FoundNames.Free;
      if Assigned(InputTreeList) then
        InputTreeList.Free;
      if Assigned(ExpectedNames) then
        ExpectedNames.Free;
      if Assigned(NewickList) then
        NewickList.Free
    end;
  {$ENDIF}
end;

function TAnalysisInfo.GetReltimeVarianceMethod: TReltimeVarianceMethod;
begin
  {$IFDEF VISUAL_BUILD}
  Result := AnalysisPrefDlg.ReltimeVarianceMethod;
  {$ELSE}
  Assert(False, 'TReltimeVarianceMethod not supported');
  Result := rvmNone;
  {$ENDIF}
end;

procedure TAnalysisInfo.FillThreeOutInfos(AList: TList);  // used for Tajima's test
var
  A, B, C: Integer;
begin
  A := -1;
  B := -1;
  C := -1;

  {$IFDEF VISUAL_BUILD}
  with AnalysisPrefDlg do
    GetThreeTaxa(A, B, C);
  {$ELSE}
  if FMyUsedOtuInfos.Count < 3 then
    error_nv('three taxa are required for Tajima' + #39 + 's neutrality test')
  else if FMyUsedOtuInfos.Count > 3 then
    warn_nv('The alignment file had more then 3 sequences, only the first 3 were ' +
    'used. The 2 sequences used for the ingroup taxa are: ' + OtuNameToMegaStr(TOtuInfo(FMyUsedOtuInfos[0]).Name) +
    ' and ' + OtuNameToMegaStr(TOtuInfo(FMyUsedOtuInfos[1]).Name) + '. The sequence ' +
    'used for the outgroup taxon is: ' + OtuNameToMegaStr(TOtuInfo(FMyUsedOtuInfos[2]).Name));
   { use the first 2 taxa in the data file as the ingroup and the third one as the outgroup}
   A := 0;
   B := 1;
   C := 2;
  {$ENDIF}
  AList.Add(FMyUsedOtuInfos[A]);
  AList.Add(FMyUsedOtuInfos[B]);
  AList.Add(FMyUsedOtuInfos[C]);
end;

function TAnalysisInfo.GetAnalysisPrefsChosen: TStrings;
begin
  Result := nil;
  {$IFDEF VISUAL_BUILD}
  result := AnalysisPrefDlg.GetChosenOptionStrings;
  {$ENDIF}
end;

function TAnalysisInfo.GetGeneDupsFound: Boolean;
begin
  Result := False;
  if Assigned(MyGeneDupsInfo) then
    Result := MyGeneDupsInfo.GeneDupsFound;
end;

function TAnalysisInfo.GetGeneDupsRootIndex: Integer;
begin
  Result := -1;
  if Assigned(MyGeneDupsInfo) then
    Result := MyGeneDupsInfo.RootIndex;
end;

function TAnalysisInfo.GetGeneTreeRooted: Boolean;
begin
  Result := False;
  if Assigned(MyGeneDupsInfo) then
    Result := MyGeneDupsInfo.GeneTreeRooted;
end;

function TAnalysisInfo.GetAnalysisOptions(CurOperation: TDistTreeDlgOption): Boolean; // called by routines here
var
  TempPrefsChosen: TStrings;
  customRes: Integer;
begin
  Result := False;
  CheckCodonBasedTestCorrectData(CurOperation);
  if not isPrototyper then
    SetupUsedOtuInfos(CurOperation);  // this is needed for AnalysisPrefDlg potentially
  MyDistPack := TDistPack.Create;
  MyTreePack := TTreePack.Create;
{$IFDEF VISUAL_BUILD}
  InitAnalysisPrefsDlg(CurOperation);
{ TODO 3 -oglen -crefactor : we shouldn't be using a 'with' statement here. The 'with' block is so large that it becomes difficult to tell which variables belong to AnalysisPrefDlg and which don't }
  with AnalysisPrefDlg do
  begin
    {$IFDEF MYPEG_ONLY}
    if IsMyPegAnalysis and (CurOperation = dtdoMPInferAncSeqMyPeg) then
    begin
      AnalysisPrefDlg.FormActivate(nil); // it is critical to call FormActive, otherwise the values are not updated
      AnalysisPrefDlg.OKBtnClick(nil); // we are not giving the user any options for this
    end
    else
    begin
      if ShowModal <> mrOK then
        Exit;
    end;
    {$ELSE}
    {$IFDEF DEBUG}
    if IsGlenDevMode then
    begin
     Show;
     MyTreePack.TreeFile := GlenTreeFile;
     TreePack.TreeFile := GlenTreeFile;
     UserTreeFile := GlenTreeFile;
     OkBtnClick(nil);
     customRes := mrOk;

     Result := True;
     Close;
    end;
    {$ENDIF}
    if IsSessionTest then
    begin
      Show;
      customRes := mrOk;
      OkBtnClick(nil);
      Result := True;
      Close;
    end
    else if MyProcessPack.IsWorkflowPack and (not IsPrototyper) then
    begin
      ConstructDistPack(CurOperation, MyProcessPack.TextualSettingsList);
      ConstructTreePack(CurOperation, MyProcessPack.TextualSettingsList);
    end
    else if MyProcessPack.IsWorkflowPack and IsPrototyper and (MyProcessPack.TextualSettingsList.Count = 0) then
    begin
      Show;
      customRes := mrOk;
      SaveSettingsBtnClick(nil);
      FMyProcessPack := nil;
      Result := False;
      Close;
    end
    else
    begin
     customRes := ShowModal;
     while customRes = mrRetry do
       customRes := ShowModal;
     if customRes <> mrOK then
     begin
       if IsPrototyper then
         FMyProcessPack := nil; { gets freed in mega_main.pas}
       if AnalysisPrefDlg.NeedsAppOptions and (customRes = mrIgnore) then
         Result := True;
       if IsPrototyper or (customRes = mrCancel) then
         Exit;
     end;
    end;
    {$IFDEF CALTEST}
    Result := (CustomRes = mrOk);
    {$ENDIF}
    if isPrototyper then
      Exit;
    {$ENDIF}
    if MyProcessPack.IsWorkflowPack then
      TempPrefsChosen := MyProcessPack.TextualSettingsList
    else
      TempPrefsChosen := AnalysisPrefsChosen;
    SaveUsageStats(TempPrefsChosen);
    AddChosenOptionsToRuntimeProgress(TempPrefsChosen);
    if not MyProcessPack.IsWorkflowPack then
    begin
      MyDistPack.Assign(DistPack);
      MyTreePack.Assign(TreePack);
      TempPrefsChosen.Free;
    end;
{$ELSE}
    TextAnalysisStrings := TStringList.Create;
    TextAnalysisStrings.CaseSensitive := False;
    TextAnalysisStrings.Assign(MyProcessPack.TextualSettingsList);
    SaveUsageStats(TextAnalysisStrings);
    ConstructDistPack(CurOperation, TextAnalysisStrings);
    ConstructTreePack(CurOperation, TextAnalysisStrings);
    AddChosenOptionsToRuntimeProgress(TextAnalysisStrings);
{$ENDIF}

    // redo selections based on whether the user has selected groups or if groups are needed
    if (CurOperation = dtdoSelectionZTest) and MyDistPack.DoesContain(gdWithinGroupMean) then
      SetupUsedOtuInfos(dtdoSelectionZGpTest);
    UpdateClockSettings(CurOperation);
    MyMLBLensFilter := {$IFDEF VISUAL_BUILD}BLenFilter;{$ELSE}MyProcessPack.MLBLensFilter;{$ENDIF}

    if D_InputSeqData <> nil then
    begin
      MySubsetOpt := [];
      SetupDeletionOption;
      if HasSiteLabelTypes then
        FetchSiteLabelTypeOptions;
      case CurOperation of
        dtdoMLCodonOmega: FinalizeCodonOmegaOptions; // special case for HyPhy

        dtdoPairwiseDist, dtdoOverallMean, dtdoWithinGroupMean,  // all distance type analyses, including trees
        dtdoBetweenGroupMean, dtdoNetGroupMean,
        dtdoAvgDiversityWithinSubPops, dtdoAvgDiversityForEntirePop,
        dtdoInterPopDiversity, dtdoPropOfInterPopDiversity,
        dtdoSelectionExactTest, dtdoSelectionZGpTest, dtdoSelectionZTest,
        dtdoTajimaNeutralityTest,
        dtdoCompositionDistance, dtdoDisparityIndex,
        dtdoDisparityIndexTest,
        dtdoTajimaClockTest,
        dtdoUPGMATree, dtdoNJTree, dtdoMETree,
        dtdoOLSComputeUserTreeBLens, dtdoPhyloQ, dtdoRelTimeLS, dtdoRtdtLS,
        dtdoMCLComputePattern,  // moved dtdoMCLComputePattern up here since it needs to be formatted dsoDistMap else we get an error.
        dtdoMCLTsTvBias,  // moved dtdoMCLTsTvBias for same reason as above (dtdoMCLComputePattern).
        dtdoSiteCoverage,
        dtdoOLSInteriorBranchTest:
          begin
            FinalizeDistSubsetOptions(CurOperation);
          end;
        dtdoMPTree,
        dtdoMPInferAncSeq,
        dtdoMPInferAncSeqMyPeg,
        dtdoMPComputeUserTreeBLens:
          begin
            FinalizeMPSubsetOptions(CurOperation);
          end;
        dtdoMLTree,                   // For all the ML analyses
        dtdoMLClockTest,
        dtdoMLClockTestLocal,
        dtdoRelTimeML,
        dtdoRtdtML,
        dtdoCorrTestML,
        dtdoCalTest,
        dtdoMLModelTest, dtdoMLModelTestIQTree,
        dtdoMLInferAncSeq,
        dtdoMLInferAncSeqMyPeg,
        dtdoMLPredictLivingSeq, dtdoEpML,
        dtdoMLComputePattern,
        dtdoMLTsTvBias,
        dtdoMLGammRates,
        dtdoBEAM,
        dtdoMLInferSiteBySiteRates,
        dtdoMLComputeUserTreeBLens,
        dtdoMLIQtree:
          begin
            {$IFDEF VISUAL_BUILD}
            FTransposeData := IsDeveloper and AnalysisPrefDlg.TransposeDataCheckbx.Checked;
            {$ENDIF}
            begin
             FinalizeMLSubsetOptions;
             FinalizeMLThreadAndInitialTreeOptions(CurOperation);
            end;
          end;
      end;
    end;
  {$IFDEF VISUAL_BUILD}
 end;
  {$ENDIF}
  if MyTreePack.DoesContain(ttUserTree) then
    MyUserTreeFile := MyTreePack.TreeFile;

  PrivateAddDataSubsetInfoToRuntimeProgress;

  {$IFDEF VISUAL_BUILD}if not AnalysisPrefDlg.NeedsAppOptions then{$ENDIF}
    Result := True;
 end;

{$IFDEF VISUAL_BUILD}
function TAnalysisInfo.GetMegaAndApplinkerOptions(CurOperation: TDistTreeDlgOption): Boolean;
begin
  FNeedApplinkerOptions := True;
  AnalysisPrefDlg.NeedsAppOptions := True;
  Result := GetAnalysisOptions(CurOperation);
end;
{$ENDIF}

function TAnalysisInfo.GetUCSC_46_SpeciesTree: Boolean;
begin
  MyUserNewickTree := '(((((((((((((((((Human:6.39,Chimp:6.39):2.32,Gorilla:8.71):7.53,Orangutan:16.24):13.36,' +
                      '(Rhesus:13.2,Baboon:13.2):16.4):14.6,Marmoset:44.2):26.9,Tarsier:71.1):6.4,(Mouse_Lemur:69.22,' +
                      'Bushbaby:69.22):8.28)Primates:11.6,Tree_Shrew:89.1):1.9,(((((Mouse:36.95,Rat:36.95):41.95,' +
                      'Kangaroo_Rat:78.9):3.9,Guinea_Pig:82.8):2.34,Squirrel:85.14):1.26,(Rabbit:39.6,Pika:39.6):46.8)' +
                      ':4.6):6.4,(((Alpaca:67.3,(Dolphin:59.1,Cow:59.1):8.2):17.3,((Horse:82.5,(Cat:52.9,Dog:52.9)' +
                      ':29.6):1.7,(Microbat:62.0,Megabat:62.0):22.2):0.4):2.6,(Hedgehog:66.2,Shrew:66.2):21.0):10.2)' +
                      ':7.3,(((Elephant:61.1,Rock_Hyrax:61.1):16.7,Tenrec:77.8):25.5,(Armadillo:70.5,Sloth:70.5):32.8)' +
                      ':1.4):71.4,(Opossum:70.6,Wallaby:70.6):105.5):44.1,Platypus:220.2)Mammals:104.3,((Chicken:97.3,' +
                      'Zebra_Finch:97.3):177.6,Lizard:274.9):49.6):36.5,X_tropicalis:361):94,(((Tetraodon:44.1,' +
                      'Fugu:44.1):79.8,(Stickleback:62,Medaka:62):61.9):183.1,Zebrafish:307.0):148):153,Lamprey:608);';
  Result := True;
end;


function TAnalysisInfo.GetUsedOtuNames: TStringList;
var
  i: Integer;
  AName: String;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
  if FMyUsedOtuInfos.Count > 0 then
    for i := 0 to FMyUsedOtuInfos.Count - 1 do
    begin
      AName := TOtuInfo(FMyUsedOtuInfos[i]).Name;
      TrimTaxaName2(AName);
      Result.Add(AName);
    end;
end;

function TAnalysisInfo.GetUsedSpeciesTree: Boolean;
begin
  Result := False;
  if Assigned(MyGeneDupsInfo) then
    Result := MyGeneDupsInfo.UsedSpeciesTree;
end;

function TAnalysisInfo.GetUserTreeInput(CurOperation: TDistTreeDlgOption;
                                        AFileName: String;
                                        ARP: TRuntimeProgress;
                                        TaxaNames: TStringList=nil; // if taxa names needed
                                        DefaultTree: TTreeType = ttNone;
                                        UserTreeRequired: Boolean = False): Boolean; // UserTreeReq doesn't let you say 'NO' to gen a tree. ;result is Continue vs. Abort process
const
  MR_AUTOMATIC  = 1000;
  MR_SPECIFY_MANAGER = 1001;
  MR_CANCEL = 1002;
var
  {$IFDEF VISUAL_BUILD}
  ATreeDlg: TTreeInputForm = nil;
  {$ELSE}
  tempList: TStringList = nil;
  ANamesList: TStringList = nil;
  AInputTree: TTreeList = nil;
  {$ENDIF}
  AExpNameStrList:TStringList = nil;
  i, j: Integer;
  MyResult : Integer = -1;
  PerfectMatch: Boolean = False;
  aName: String;
begin
  Result := False;

  try try
    //== expected names are put together here
    AExpNameStrList := TStringList.Create;
    if TaxaNames <> nil then TaxaNames.Clear;
    for i:=0 to FMyUsedOtuInfos.Count-1 do
      with TOtuInfo(FMyUsedOtuInfos[i]) do
      begin
        AExpNameStrList.Add(Name);
        if TaxaNames <> nil then
          TaxaNames.Add(Name);
      end;

    //== Allocate tree dialog
    {$IFDEF VISUAL_BUILD}
    ATreeDlg := TTreeInputForm.Create(Application);
    ATreeDlg.SetActiveDataNames(AExpNameStrList);
    {$ENDIF}
    if FileExists(AFileName) then // the user has asked to select a file name.
    begin
  {$IFDEF VISUAL_BUILD}
      if not IsPerfectTreeFile(AFileName) then // check if the file is a valid newick file.
      begin
        {$IFDEF CALTEST}
         raise Exception.Create('The provided tree file does not conform to the Newick format');
        {$ELSE}
        if UserTreeRequired then
          MyResult := QuestionDlg('Non standard newick file', 'The contents of this tree file don''t conform to the Newick Format standard.' + LineEnding + 'How would you like to resolve this?', mtCustom, [MR_SPECIFY_MANAGER, UserSpecifyManager, MR_CANCEL, 'Cancel Analysis'], 0)
        else
          MyResult := QuestionDlg('Non standard newick file', 'The contents of this tree file don''t conform to the Newick Format standard.' + LineEnding + 'How would you like to resolve this?', mtCustom, [MR_AUTOMATIC, 'Automatic Tree', MR_SPECIFY_MANAGER, UserSpecifyManager, MR_CANCEL, 'Cancel Analysis'], 0);
        {$ENDIF CALTEST}
      end
      // By this point we know the file is valid.
      // To check if the number of taxa are equal, and the names all match exactly we need a list of names from the tree, to do this we essentially have to parse the tree file, so we may as well read the tree in now.
      else
      begin
        ATreeDlg.SetTreeFromFile(AFileName);
        ATreeDlg.InitializeActiveDataAndTreeNameMatches;
        if not ATreeDlg.DoesTreeHaveAllActiveNames then
        begin
          {$IFDEF CALTEST}
           raise Exception.Create('The names in the provided tree file do not match the active data');
          {$ELSE}
          ATreeDlg.TreeToOpenTreeOnShow(AFileName);
          if UserTreeRequired then
            MyResult := QuestionDlg('Taxa Name Mismatch', 'The contents of this tree file need to be edited.'+#10#13+'How would you like to resolve this?', mtCustom, [MR_SPECIFY_MANAGER, UserSpecifyManager, MR_CANCEL, 'Cancel Analysis', 'IsCancel'], 0)
          else
            MyResult := QuestionDlg('Taxa Name Mismatch', 'The contents of this tree file need to be edited.'+#10#13+'How would you like to resolve this?', mtCustom, [MR_AUTOMATIC, 'Automatic Tree', MR_SPECIFY_MANAGER, UserSpecifyManager, MR_CANCEL, 'Cancel Analysis', 'IsCancel'], 0);
          {$ENDIF CALTEST}
        end
        else
        begin
          PerfectMatch := true;
          ATreeDlg.RebuildTreeNames;
          ATreeDlg.PruneUnwantedTreeNodesAndNames;
          FUserInputTreeIsRooted := ATreeDlg.IsRootedTree;
        end;
      end;
    end
    else
    begin // The user has asked to use the tree manager.
      // you don't need to do anything.  The tree input dlg will appear.
    end;

    // now handle the user selection
    case MyResult of
      MR_AUTOMATIC:
        begin
          MyTreePack.ReplaceType(ttUserTree, DefaultTree);
          MyUserTreeFile := EmptyStr;
          MyUserNewickTree := EmptyStr;
          MyTreePack.TreeFile := EmptyStr;
          Result := True;
          Exit;
        end;
      MR_CANCEL:
        begin
          Result := False;
          MyUserTreeFile := EmptyStr;
          MyUserNewickTree := EmptyStr;
          MyTreePack.TreeFile := EmptyStr;
          Exit;
        end
    end;
    if not PerfectMatch then
    begin
      ARP.Hide;
      if ATreeDlg.ShowModal <> mrOK then
      begin
        Result := False;
        Exit;
      end;
      ARP.Show;
    end;
    ATreeDlg.RebuildTreeNames;
    MyTreePack.TreeFile := ATreeDlg.FileName;

    MyUserTreeFile      := ATreeDlg.FileName;
    MyUserNewickTree    := ATreeDlg.NewickTree;
    FUserInputTreeIsRooted := ATreeDlg.IsRootedTree;
  {$ELSE}
      //end;
      result := false; // wasn't specified earlier, needed in case of exiting on the IsPerfectTreeFile call.

      { TODO 1 -oglen -creltime : we need to fix IsPerfectTreeFile so that it doesn't fail for a newick string with a quoted label at the root }
      {$IFNDEF RELTIME_ONLY}
      if not IsPerfectTreeFile(MyTreePack.TreeFile) then
        warn_nv('The contents of this tree file don''t conform to the Newick format standard.');
      {$ENDIF}
      AInputTree := nil;
      ANamesList := nil;
      try
        AInputTree := TTreeList.Create;
        ANamesList := TStringList.Create;
        AInputTree.ImportFromNewickFile(AFileName, nil);
        for i:= 0 to AInputTree.NoOfOTUs-1 do
          ANamesList.Add(AInputTree.OTUName[i]);
        tempList := RegexReplaceAStringList(ANamesList, '[^A-Za-z0-9]', '');
        ANamesList.Free;
        ANamesList := tempList;
        tempList := RegexReplaceAStringList(AExpNameStrList, '[^A-Za-z0-9]', '');
        AExpNameStrList.Free;
        AExpNameStrList := tempList;
        PerfectMatch := true;
        for i := 0 to AExpNameStrList.Count-1 do
        begin
          if ANamesList.IndexOf(AExpNameStrList.Strings[i]) < 0 then
            begin
            PerfectMatch := False;
            Result := False;
            Error_NV('The list of taxa in the tree file is ' +
              'different from the list of taxa in the sequence file, this must ' +
              'be corrected to complete this analysis.  Tree missing:' + AExpNameStrList[i]);
            end;
        end;
        for i := 0 to ANamesList.Count - 1 do
        begin
            if AExpNameStrList.IndexOf(ANamesList[i]) < 0 then
            begin
              warn_nv(Format('pruning unused taxon from input tree: %s', [ANamesList[i]]));
              AInputTree.RemoveOtu(ANamesList[i]);
            end;
        end;
        AInputTree.OutputNodeLabels := True;
        MyUserNewickTree := AInputTree.OutputNewickTree(0, AInputTree.isBLen, AInputTree.isStats, 0.0);
        FUserInputTreeIsRooted := AInputTree.isRooted;
      finally
        FreeAndNil(ANamesList);
        FreeAndNil(AInputTree);
      end;
    end;
     MyTreePack.TreeFile := AFileName;
     MyUserTreeFile     := AFileName;
  {$ENDIF}
    Result := True;
    {$IFDEF VISUAL_BUILD}
    if ATreeDlg.HasUnusedSequenceData then
    begin
      for i := 0 to ATreeDlg.UnusedSequences.Count - 1 do
      begin
        aName := ATreeDlg.UnusedSequences[i];
        for j := FMyUsedOtuInfos.Count - 1 downto 0 do
          if TOtuInfo(FMyUsedOtuInfos[j]).Name = aName then
          begin
            FMyUsedOtuInfos.Delete(j);
            MyOtuNames.Delete(j);
            break;
          end;
      end;
    end;
    UsingAlignmentAndTreeThatDontResolve := (ATreeDlg.HasUnusedSequenceData or ATreeDlg.HasUnusedTreeTaxa);
    {$ENDIF}
  except
    on E: Exception do
      ShowErrorMessage(E);
  end
  finally
  {$IFDEF VISUAL_BUILD}
    if Assigned(ATreeDlg) then
      ATreeDlg.Free;
  {$ENDIF}
    if Assigned(AExpNameStrList) then
      AExpNameStrList.Free;
  end;
end;

{
function TAnalysisInfo.GetClusters(ANoOfClusters: Integer): Boolean;
begin
  TaxaGpsDlg.NoofClusters := ANoOfClusters;
  TaxaGpsDlg.ClusterView := True;
  if TaxaGpsDlg.ShowModal <> mrOK then
  begin
    TaxaGpsDlg.ClusterView := False;
    Result := False;
    Exit;
  end;
  TaxaGpsDlg.ClusterView := False;
  Result := True;
end;
}

//=== Sets-up the used OtuInfos with gp/cluster constraints if necessary
procedure TAnalysisInfo.SetupUsedOtuInfos(CurOperation: TDistTreeDlgOption);
var
  i, ACount: Integer;
  {$IFDEF VISUAL_BUILD}
  pleaseWait: TPleaseWait = nil;
  {$ENDIF}
begin
  MyNoOfSeqs  := 0;
  MyNoOfGps   := 0;
  MyNoOfSites := 0;

  try
    {$IFDEF VISUAL_BUILD}
    if CurrentAllOtuInfos.NoOfSelOtus > 1000 then
    begin
      pleaseWait := TPleaseWait.Create(Application);
      pleaseWait.SetToMarqueeMode;
      pleaseWait.Caption := 'Please Wait...';
      pleaseWait.Action := 'Setting up OTU information...';
      pleaseWait.SetShowCancel(False);
      pleaseWait.Show;
    end;
    {$ENDIF}
    if FMyUsedOtuInfos <> nil then
      FMyUsedOtuInfos.Clear
    else
      FMyUsedOtuInfos := TList.Create;
    MyGpIds := nil;
    CurrentAllOtuInfos.UsedOtuInfos(FMyUsedOtuInfos, DoesOperationRequireGroups(CurOperation));

    if MyOtuNames <> nil then
      MyOtuNames.Clear
    else
      MyOtuNames := TStringList.Create;

    for i:=0 to FMyUsedOtuInfos.Count-1 do
    begin
      with TOtuInfo(FMyUsedOtuInfos[i]) do
        MyOtuNames.Add(Name);
    end;

    if MyGpNames <> nil then
      MyGpNames.Clear
    else
      MyGpNames := TStringList.Create;
    SetupOutgroupMembers;

    MyNoOfSeqs := FMyUsedOtuInfos.Count;
    MyNoOfGps  := CountNoOfGpsInOtuInfos(FMyUsedOtuInfos);

    // Raise a flag if not enough groups
    case CurOperation of
      dtdoAvgDiversityForEntirePop: if MyNoOfGps < 1 then RaiseErrorMessage(HC_Not_Enough_Groups_Selected,'At least one group must be defined to use this option.  If you wish to calculate mean distance, please use the "Overall mean..." option in the Distances menu.', mtWarning);
      dtdoWithinGroupMean,
      dtdoSelectionZGpTest:         if MyNoOfGps < 1 then RaiseErrorMessage(HC_Not_Enough_Groups_Selected,'At least one group must be defined.', mtWarning);
      dtdoBetweenGroupMean,
      dtdoNetGroupMean:             if MyNoOfGps < 2 then RaiseErrorMessage(HC_Not_Enough_Groups_Selected,'At least two groups must be defined.', mtWarning);

      dtdoAvgDiversityWithinSubPops,
      dtdoInterPopDiversity,
      dtdoPropOfInterPopDiversity: if MyNoOfGps < 2 then RaiseErrorMessage(HC_Not_Enough_Groups_Selected,'At least two groups (subpopulations) must be defined.', mtWarning);
    end;

    ACount := MinNoOfTaxaNeededForAnalysis(CurOperation);
    if MyNoOfSeqs < ACount then
    begin
      if CurOperation = dtdoWithinGroupMean then
        RaiseLimitationMessage('At least '+IntToStr(ACount)+' taxa are required to be in the groups analyzed')
      else if CurOperation = dtdoMLCodonOmega then
        raise Exception.Create('At least '+IntToStr(ACount)+' taxa are required for the codon selection analysis')
      else
        RaiseLimitationMessage('At least '+IntToStr(ACount)+' taxa are required');
    end;

    if DoesOperationRequireGroups(CurOperation) then
     SetupGroupIds;

    if ARP <> nil then
    begin
      ARP.AddAnalysisOptions('No. of Taxa', IntToStr(MyNoOfSeqs));
      if MyNoOfGps > 0 then
        ARP.AddAnalysisOptions('No. of Groups', IntToStr(MyNoOfGps));
    end;
  finally
    {$IFDEF VISUAL_BUILD}
    if Assigned(pleaseWait) then
      pleaseWait.Free;
    {$ENDIF}
  end;
end;

procedure TAnalysisInfo.SetUsedSpeciesTree(const Value: Boolean);
begin
  if not Assigned(MyGeneDupsInfo) then
    MyGeneDupsInfo := TGeneDupsInfo.Create;
  MyGeneDupsInfo.UsedSpeciesTree := Value;
end;

function TAnalysisInfo.GetSpeciesTreeRooted: Boolean;
begin
  Result := False;
  if Assigned(MyGeneDupsInfo) then
    Result := MyGeneDupsInfo.SpeciesTreeRooted;
end;

function TAnalysisInfo.UniqueSpeciesNamesToStringList: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
  for i := 0 to FMyUsedOtuInfos.Count - 1 do
    Result.Add(TOtuInfo(FMyUsedOtuInfos[i]).SpName);
end;

function TAnalysisInfo.UpdateSpNamesInTreeList: Boolean;
var
  i: Integer;
  SpeciesName, OtuName: String;
begin
  Result := False;
  try
    if (not Assigned(FMyOriTreeList)) or (not Assigned(FMyUsedOtuInfos)) or FTransposeData then
      Exit;
    if FMyUsedOtuInfos.Count > 0 then
    begin
      for i := 0 to FMyUsedOtuInfos.Count - 1 do
      begin
        SpeciesName := TOtuInfo(FMyUsedOtuInfos[i]).SpName;
        OtuName := TOtuInfo(FMyUsedOtuInfos[i]).Name;
        FMyOriTreeList.SetSpeciesNameForOtu(SpeciesName, OtuName);
      end;
      Result := True;
    end;
  except
    on E:Exception do
      Assert(False, 'Error in UpdateSpNamesListInTreeList: ' + E.Message);
    // just return false
  end;
end;

function TAnalysisInfo.UpdateProgress(Progress: integer; Status: AnsiString): boolean;
begin
  Result := False; { callers of this function might use the result to see of the user has cancelled, but we have no idea}
  if (MillisecondsBetween(Time, LastUpdateTime) < PROG_UPDATE_INTERVAL) or (not Assigned(ARP)) then
    Exit;
  ARP.UpdateRunStatusInfo(Status, Format('%d%%', [Progress]));
  ARP.Refresh;
  LastUpdateTime := Time;
end;

function TAnalysisInfo.SequencesToStringList: TStringList;
var
  i: Integer;
  TempStr: AnsiString;
  TheData: PAnsiChar;
  NumSites: Int64;
begin
  TempStr := PAnsiChar(TOtuInfo(FMyUsedOtuInfos[0]).Data);
  NumSites := Length(TempStr) - 1;
  Result := TStringList.Create;

  for i := 0 to FMyUsedOtuInfos.Count - 1 do
  begin
    TheData := PAnsiChar(TOtuInfo(FMyUsedOtuInfos[i]).Data);
    SetString(TempStr, TheData, NumSites);
    Result.Add(TempStr);
  end;
end;

procedure TAnalysisInfo.SetGeneDupsRootIndex(const Value: Integer);
begin
  if not Assigned(MyGeneDupsInfo) then
    MyGeneDupsInfo := TGeneDupsInfo.Create;
  MyGeneDupsInfo.RootIndex := Value;
end;

procedure TAnalysisInfo.SetGeneTreeRooted(const Value: Boolean);
begin
  if not Assigned(MyGeneDupsInfo) then
    MyGeneDupsInfo := TGeneDupsInfo.Create;
  MyGeneDupsInfo.GeneTreeRooted := Value;
end;

procedure TAnalysisInfo.SetIsGeneDups(Value: Boolean);
begin
  if Value then
    if not Assigned(MyGeneDupsInfo) then
      MyGeneDupsInfo := TGeneDupsInfo.Create;
end;

procedure TAnalysisInfo.SetIsGeneDupsTree(const Value: Boolean);
begin
  if not Value then
    Exit;
  if not Assigned(MyGeneDupsInfo) then
    MyGeneDupsInfo := TGeneDupsInfo.Create;
end;

procedure TAnalysisInfo.SetMergeRates(const Value: Boolean);
begin
  FMergeRates := Value;
end;

procedure TAnalysisInfo.SetMyGpNames(const Value: TStringList);
begin
  FGroupInfo.GroupNames := Value;
end;

procedure TAnalysisInfo.SetMyOriTreeList(const Value: TTreeList);
begin
  FMyOriTreeList := Value;
  if not Assigned(FGroupInfo) then
    FGroupInfo := TGroupInfo.Create(FMyOriTreeList)
  else
    FGroupInfo.TreeList := FMyOriTreeList;
  FGroupInfo.UsedOtuInfos := FMyUsedOtuInfos;
  if Assigned(Value) and Assigned(FMyUsedOtuInfos) and (FMyUsedOtuInfos.Count > 0) and (Value.NoOfOtus > 0) then
    UpdateSpNamesInTreeList;  
end;

procedure TAnalysisInfo.SetMyOtuInfos(const Value: TList);
begin
  FMyUsedOtuInfos := Value;
  FGroupInfo.UsedOtuInfos := FMyUsedOtuInfos;
end;

procedure TAnalysisInfo.SetUseFixedEvolutionaryRate(AValue: Boolean);
begin
  if FUseFixedEvolutionaryRate=AValue then Exit;
  FUseFixedEvolutionaryRate:=AValue;
end;

procedure TAnalysisInfo.AddCodonPositionSubsets;
{$IFNDEF VISUAL_BUILD}
var
  i: Integer;
{$ENDIF}
begin
  {$IFDEF VISUAL_BUILD}
  with AnalysisPrefDlg do
  begin
    if (meg1stSite   in IncludeCodonPos) then  MySubsetOpt := MySubsetOpt + [dsoUse1stPos];
    if (meg2ndSite   in IncludeCodonPos) then  MySubsetOpt := MySubsetOpt + [dsoUse2ndPos];
    if (meg3rdSite   in IncludeCodonPos) then  MySubsetOpt := MySubsetOpt + [dsoUse3rdPos];
    if (megNoncoding in IncludeCodonPos) then  MySubsetOpt := MySubsetOpt + [dsoUseNonCod];
  end;
  {$ELSE}
   for i := 0 to TextAnalysisStrings.Count - 1 do
   begin
     if Copy(TextAnalysisStrings[i], 0, 22) = 'Select Codon Positions' then
     begin
       if (AnsiContainsStr(TextAnalysisStrings[i], '1st')) then  MySubsetOpt := MySubsetOpt + [dsoUse1stPos];
       if (AnsiContainsStr(TextAnalysisStrings[i], '2nd')) then  MySubsetOpt := MySubsetOpt + [dsoUse2ndPos];
       if (AnsiContainsStr(TextAnalysisStrings[i], '3rd')) then  MySubsetOpt := MySubsetOpt + [dsoUse3rdPos];
       if (AnsiContainsStr(TextAnalysisStrings[i], 'Non-Coding')) then  MySubsetOpt := MySubsetOpt + [dsoUseNonCod];
     end;
   end;
  {$ENDIF}
  MySubsetOpt := MySubsetOpt + [dsoUseNuc];
end;

function TAnalysisInfo.NumThreadsToUseCC: Integer;
begin
  if (TextAnalysisStrings.IndexOfName(opsMLNumThreads) <> -1) and (TextAnalysisStrings.Values[opsMLNumThreads] <> NotApplicableStr) then
    Result := StrToInt(TextAnalysisStrings.Values[opsMLNumThreads])
  else
    Result := -1; // if the user didn't specify (in the case of older mao files), then set it directly below
  if Result < 1 then
  begin
    if GetNoOfProcessors > 1 then  // then use the default
      Result := GetNoOfProcessors - 1
    else
      Result := 1;
  end;
end;

procedure TAnalysisInfo.SetupDeletionOption;
begin
  {$IFDEF VISUAL_BUILD}
  if MyProcessPack.IsWorkflowPack then
  begin
      if MyProcessPack.TextualSettingsList.Values[opsGapMissingHandle2] = CompleteDelStr then
        MySubsetOpt := MySubsetOpt + [dsoCompleteDeletion];
      if MyProcessPack.TextualSettingsList.Values[opsGapMissingHandle2] = PartialDelStr then
      begin
        MySubsetOpt := MySubsetOpt + [dsoPartialDeletion];
        MySiteCoverage := StrToInt(TextAnalysisStrings.Values[opsSiteCoverage2]);
      end;
      if MyProcessPack.TextualSettingsList.Values[opsGapMissingHandle2] = PairwiseDelStr then
        MySubsetOpt := MySubsetOpt + [dsoPairwiseDeletion];
    exit;
  end;

  with AnalysisPrefDlg do
  begin
  {$ENDIF}
    if IsCompleteDeletion then
      MySubsetOpt := MySubsetOpt + [dsoCompleteDeletion]
    else if IsPartialDeletion then
    begin
      MySubsetOpt := MysubsetOpt + [dsoPartialDeletion];
      {$IFDEF VISUAL_BUILD}
      MySiteCoverage := SiteCoverage;
      {$ELSE}
      MySiteCoverage := StrToInt(TextAnalysisStrings.Values[opsSiteCoverage2]);
      {$ENDIF}
    end
    else if isPairwiseDeletion then
      MySubsetOpt := MySubsetOpt + [dsoPairwiseDeletion];
  {$IFDEF VISUAL_BUILD}
  end;
  {$ENDIF}
end;

function TAnalysisInfo.IsRecodeBases: boolean;
{$IFNDEF VISUAL_BUILD}
var
  TextAnalysisStrings: TStringList = nil;
{$ENDIF}
begin
  {$IFDEF VISUAL_BUILD}
  Result := AnalysisPrefDlg.IsRecodeBases;
  {$ELSE}
  TextAnalysisStrings := MyProcessPack.TextualSettingsList;
  Result := not (
  (TextAnalysisStrings.Values[opsRecodeBases2] = AtomicNoRecodingPickStr) or
  (TextAnalysisStrings.Values[opsRecodeBases2] = NotApplicableStr) or
  (TextAnalysisStrings.Values[opsRecodeBases2] = EmptyStr)
  );
  {$ENDIF}
end;

procedure TAnalysisInfo.CheckCodonBasedTestCorrectData(CurOperation: TDistTreeDlgOption);
begin
  if (CurOperation = dtdoSelectionZTest) or (CurOperation = dtdoSelectionExactTest) then
  begin
    if not (D_InputSeqData.IsNuc and D_InputSeqData.IsCoding) then
       RaiseErrorMessage(HC_Inapplicable_Computation_Requested, 'Codon-based tests Of selection require coding sequences.');
  end;
end;

{$IFDEF VISUAL_BUILD}
procedure TAnalysisInfo.InitAnalysisPrefsDlg(CurOperation: TDistTreeDlgOption);
begin
  with AnalysisPrefDlg do
  begin
    if IsPrototyper then
      NumSitesBeforeSubsetting := DEFAULT_MAX_SAMPLE_SITES
    else if Assigned(D_InputSeqData) then
      NumSitesBeforeSubsetting := D_InputSeqData.NoOfSites;
    if not IsPrototyper then
      NumTaxa := FMyUsedOtuInfos.Count;
    Operation := CurOperation;
    ProcessPackTemp := MyProcessPack;
    if D_InputSeqData <> nil then
    begin
      IsNuc     := D_InputSeqData.IsNuc;
      IsCoding  := D_InputSeqData.IsNuc and D_InputSeqData.IsCoding;
      IsAmino   := not D_InputSeqData.IsNuc;

      // make this HasGps more advanced when group computing is available everywhere
      // should be > 0 for MP; >2 for UPGMA; > 3 for other distance methods
      HasGps := (MyNoOfGps > 0) and
                ((CurOperation = dtdoSelectionZTest) or
                (CurOperation = dtdoSelectionExactTest));

      HasSiteLabelTypes := D_InputSeqData.NoOfSiteLabelTypes > 0;
      if HasSiteLabelTypes then
        SetupSiteLabelTypeOptions;
      IsDist := False;
    end
    else
      IsDist := True;
    if CurOperation = dtdoTajimaClockTest then
      SetTaxaList(MyOtuNames);
  end;
end;
{$ENDIF}

procedure TAnalysisInfo.SaveUsageStats(stats: TStrings);
var
  usageStatistics: TUsageStatistics = nil;
begin
  try
    try
      UsageStatistics := TUsageStatistics.Create;
      UsageStatistics.UpdateUsageStatistics(stats);
    except
      on E:Exception do
      begin
       Assert(False, 'Error saving usage statistics: ' + E.Message);
      // do nothing, if we lose some users stats it is no big deal, we only want a sample anyway
      end;
    end;
  finally
    if Assigned(usageStatistics) then
      usageStatistics.Free;
  end;
end;

procedure TAnalysisInfo.AddChosenOptionsToRuntimeProgress(options: TStrings);
var
  i: Integer;
begin
  {$IFDEF VISUAL_BUILD}
  ARP.AnalysisOptStrList.Assign(options);
  if options.Count > 0 then
    for i := 0 to options.Count - 1 do
      AnalysisSummary.AddOptionString(options[i]);
  AnalysisSummary.StartTime := Now;
  {$ELSE}
  for i := 0 to options.Count - 1 do
      ARP.AddAnalysisOptions(options.Names[i], options.Values[options.Names[i]]);
  ShowRunStatusInfoStatic('status', ' ');
  {$ENDIF}
end;

procedure TAnalysisInfo.UpdateClockSettings(CurOperation: TDistTreeDlgOption);
begin
  if CurOperation = dtdoMLClockTest then
  begin
    FClockType := ctGlobal;
    BootReps := 0;
  end
  else if (CurOperation = dtdoMLClockTestLocal) or
          IsReltimeAnalysis(CurOperation) or
          IsCorrTestAnalysis(CurOperation) then
  begin
    FClockType := ctLocal;
    {$IFDEF VISUAL_BUILD}
    FClockLevel := AnalysisPrefDlg.GetClockLevel;
    if IsDeveloper then
      FMaxRateRatio := AnalysisPrefDlg.MaxRateRatio
    else
      FMaxRateRatio := DEFAULT_MAX_RATE_RATIO;
    {$ELSE}
    FClockLevel := MyProcessPack.GetClockLevel;
    CalibrationFile := MyProcessPack.GetCalibrationFile;
    if IsDeveloper then
      FMaxRateRatio := MyProcessPack.MaxRateRatio
    else
      FMaxRateRatio := DEFAULT_MAX_RATE_RATIO;
    {$ENDIF}
  end;
end;

procedure TAnalysisInfo.FinalizeCodonOmegaOptions;
begin
  MyDistPack.AddType(gdPairwise);
  MySubsetOpt := MySubsetOpt + [dsoNoMap];
  MySubsetOpt := MySubsetOpt + [dsoUseNuc];
  MySubsetOpt := MySubsetOpt + [dsoUse1stPos];
  MySubsetOpt := MySubsetOpt + [dsoUse2ndPos];
  MySubsetOpt := MySubsetOpt + [dsoUse3rdPos];
  MySubsetOpt := MySubsetOpt + [dsoUseNonCod];
end;

procedure TAnalysisInfo.FinalizeMLSubsetOptions;
begin
  {$IFDEF VISUAL_BUILD}
  if AnalysisPrefDlg.GetPickListSettingValue(opsTestPhylo2) = LittleBootTestStr then
  begin
   FNumBootstrapSubSamples := AnalysisPrefDlg.GetIntegerSettingValue(opsBootstrapSubSamples);
   FNumRepsPerSubSample := AnalysisPrefDlg.GetIntegerSettingValue(opsBootstrapRepsPerSample);
   FNumSitesPerSample := AnalysisPrefDlg.GetIntegerSettingValue(opsBootstrapSitesPerSample);
  end;
  {$ELSE}
  if MyProcessPack.HasSetting(opsTestPhylo2) and MyProcessPack.SettingValueIs(opsTestPhylo2, LittleBootTestStr) then
  begin
    if MyProcessPack.TextualSettingsList.IndexOfName(opsBootstrapSubSamples) >= 0 then
      FNumBootstrapSubSamples := StrToInt(MyProcessPack.TextualSettingsList.Values[opsBootstrapSubSamples]);
    if MyProcessPack.TextualSettingsList.IndexOfName(opsBootstrapRepsPerSample) >= 0 then
      FNumRepsPerSubSample := StrToInt(MyProcessPack.TextualSettingsList.Values[opsBootstrapRepsPerSample]);
    if MyProcessPack.SettingIsApplicable(opsBootstrapSitesPerSample) and (MyProcessPack.TextualSettingsList.IndexOfName(opsBootstrapSitesPerSample) >= 0) then
      FNumSitesPerSample := StrToInt(MyProcessPack.TextualSettingsList.Values[opsBootstrapSitesPerSample]);
    if MyProcessPack.TextualSettingsList.IndexOfName(opsBootstrapSampleSizeFactor) >= 0 then
      FBootstrapSampleSizeFactor := StrToFloat(MyProcessPack.TextualSettingsList.Values[opsBootstrapSampleSizeFactor]);
  end;
  {$ENDIF}
  MySubsetOpt := MySubsetOpt + [dsoNoMap];  // NO MAP is needed
  if MyDistPack.DoesContain(gdOneNuc) then
  begin
    MySubsetOpt := MySubsetOpt + [dsoUseNuc];
    if D_InputSeqData.IsCoding then
      AddCodonPositionSubsets
    else
     MySubsetOpt := MySubsetOpt + [dsoUseNonCod];
  end
  else if MyDistPack.DoesContain(gdThreeNuc) then
  begin
    MySubsetOpt := MySubsetOpt + [dsoUseCodon];
    MySubsetOpt := MySubsetOpt + [dsoUseAmino];
  end
  else
    MySubsetOpt := MySubsetOpt + [dsoUseAmino];
end;


procedure TAnalysisInfo.FinalizeMLThreadAndInitialTreeOptions(CurOperation: TDistTreeDlgOption);
{$IFNDEF VISUAL_BUILD}
var
  TmpTreeToUse: String;
{$ENDIF}
begin
  {$IFDEF VISUAL_BUILD}
  with AnalysisPrefDlg do
  begin
    if CurOperation = dtdoMLTree then
    begin
      MyNumThreadsToUse := GetNumThreadsToUse;
      MyInitialTreeMethod := GetInitialTreeMethod;
    end
    else if CurOperation = dtdoBEAM then
    begin
      MyInitialTreeMethod := DefaultInitTreeMethod;
      MyNumThreadsToUse := GetNumThreadsToUse;
    end;
    if ((CurOperation = dtdoMLComputeUserTreeBLens) or
       (CurOperation = dtdoMLModelTest) or
       (CurOperation = dtdoMLClockTestLocal) or
       (CurOperation = dtdoMLClockTest) or
       (CurOperation = dtdoCalTest) or
       (CurOperation = dtdoCorrTestML) or
       (CurOperation = dtdoRtdtML) or
       (CurOperation = dtdoRelTimeML))then
      MyNumThreadsToUse := GetNumThreadsToUse;
  end;
  {$ELSE}
  if (CurOperation = dtdoMLTree) or (CurOperation = dtdoBEAM) or (CurOperation = dtdoMLModelTest) then
  begin
    MyNumThreadsToUse := NumThreadsToUseCC;

    TmpTreeToUse := TextAnalysisStrings.Values[opsMLInitialTrees2];
    if (TmpTreeToUse = InitialTreeByNJStr) then
     MyInitialTreeMethod := NJInitTreeMethod
    else if (TmpTreeToUse = InitialTreeByBioNJStr) then
     MyInitialTreeMethod := BioNJInitTreeMethod
    else
     MyInitialTreeMethod := DefaultInitTreeMethod;
  end
  else if ((CurOperation = dtdoMLComputeUserTreeBLens) or
           (CurOperation = dtdoMLClockTestLocal) or
           (CurOperation = dtdoMLClockTest) or
           (CurOperation = dtdoRelTimeML) or
           (CurOperation = dtdoRtdtML) or
           (CurOperation = dtdoCorrTestML))then
  begin
    MyNumThreadsToUse := NumThreadsToUseCC;
  end;
{$ENDIF}
end;

procedure TAnalysisInfo.FinalizeMPSubsetOptions(CurOperation: TDistTreeDlgOption);
begin
  if CurOperation = dtdoMPTree then
    MyNumThreadsToUse := {$IFDEF VISUAL_BUILD}AnalysisPrefDlg.GetNumThreadsToUse{$ELSE}NumThreadsToUseCC{$ENDIF};
  MySubsetOpt := MySubsetOpt + [dsoParsimMap];
  if D_InputSeqData.IsNuc and D_InputSeqData.IsCoding then
  begin
    if MyTreePack.DoesContain(ttNucParsim) then
    begin
      if D_InputSeqData.IsCoding then AddCodonPositionSubsets;
    end
    else
    begin
      MySubsetOpt := MySubsetOpt + [dsoUseCodon];
      MySubsetOpt := MySubsetOpt + [dsoUseAmino];
    end;
  end
  else if D_InputSeqData.IsNuc then
    MySubsetOpt := MySubsetOpt + [dsoUseNuc]
  else
    MySubsetOpt := MySubsetOpt + [dsoUseAmino];

  if (dsoUseNuc in MySubsetOpt) and MyTreePack.DoesContain(ttTvParsim) then
    MySubsetOpt := MySubsetOpt + [dsoNucToTvParsim];
end;

procedure TAnalysisInfo.FinalizeDistSubsetOptions(CurOperation: TDistTreeDlgOption);
begin
  if ((CurOperation = dtdoNJTree) or (CurOperation = dtdoMETree) or (CurOperation = dtdoUPGMATree)) then
    MyNumThreadsToUse := {$IFDEF VISUAL_BUILD}AnalysisPrefDlg.GetNumThreadsToUse{$ELSE}NumThreadsToUseCC{$ENDIF};
  if (CurOperation = dtdoUPGMATree) or (CurOperation = dtdoNJTree) or (CurOperation = dtdoMETree) or (CurOperation = dtdoOLSComputeUserTreeBLens) or (CurOperation = dtdoPhyloQ) then
    MyDistPack.AddType(gdPairwise); // always (ONLY FOR OPERATIONS WHICH WERE SETUP USING GetTreeOptions, otherwise there WILL be errors!) Pairwise distances/Allow to do UPGMA, NJ, and ME-Trees for average between group distances
  MySubsetOpt := MySubsetOpt + [dsoDistMap];
  if MyDistPack.DoesContain(gdOneNuc) then
  begin
    MySubsetOpt := MySubsetOpt + [dsoUseNuc];
    if D_InputSeqData.IsCoding then
      AddCodonPositionSubsets
    else
      MySubsetOpt := MySubsetOpt + [dsoUseNonCod];
  end
  else if MyDistPack.DoesContain(gdThreeNuc) then
    MySubsetOpt := MySubsetOpt + [dsoUseCodon];

  if MyDistPack.DoesContain(gdAmino) then // it adds dsoUseAmino in two cases, if amino data or if threeNuc
    MySubsetOpt := MySubsetOpt + [dsoUseAmino];

  if IsRecodeBases then
  begin
   MySubsetOpt := MySubsetOpt + [dsoRecodeBases];
   MyRecodeScheme := RecodeScheme;
  end;
end;

procedure TAnalysisInfo.ConstructTreePack(CurOperation: TDistTreeDlgOption; options: TStringList);
begin
  MyTreePack.ConstructPack(CurOperation, options, False);
  {$IFNDEF VISUAL_BUILD}
  if MyTreePack.DoesContain(ttUserTree) then
  begin
    MyTreePack.TreeFile := MyProcessPack.TreeFile;
    if (Length(Trim(MyTreePack.TreeFile)) = 0) then
    begin
      Error_nv('Please provide a tree file, using the -t flag.');
      D_MegaMain.PrintHelp();
    end
    else if not FileExists(MyTreePack.TreeFile) then
    begin
      Error_nv('The tree file you specified does not exist.');
    end;
  end;
  {$ENDIF}
end;

procedure TAnalysisInfo.ConstructDistPack(CurOperation: TDistTreeDlgOption; options: TStringList);
begin
  MyDistPack.ConstructPack(CurOperation, options, Assigned(D_InputSeqData) and D_InputSeqData.IsCoding);
end;

procedure TAnalysisInfo.SetMyOutgroupMembers(Index: Integer; const Value: Boolean);
begin
  FGroupInfo.IsOutgroupMember[Index] := Value;
end;

procedure TAnalysisInfo.SetNumConfigs(const Value: Integer);
begin
  if not Assigned(MyGeneDupsInfo) then
    MyGeneDupsInfo := TGeneDupsInfo.Create;
  MyGeneDupsInfo.NumTreeConfigs := Value;
end;

procedure TAnalysisInfo.SetNumGeneDups(const Value: Integer);
begin
  if not Assigned(MyGeneDupsInfo) then
    MyGeneDupsInfo := TGeneDupsInfo.Create;
  MyGeneDupsInfo.NumGeneDups := Value;
end;

procedure TAnalysisInfo.SetNumSpeciations(const Value: Integer);
begin
  if not Assigned(MyGeneDupsInfo) then
    MyGeneDupsInfo := TGeneDupsInfo.Create;
  MyGeneDupsInfo.NumSpeciations := Value;
end;

procedure TAnalysisInfo.SetNumSpecies(const Value: Integer);
begin
  if not Assigned(MyGeneDupsInfo) then
    MyGeneDupsInfo := TGeneDupsInfo.Create;
  MyGeneDupsInfo.NumSpecies := Value;
end;

procedure TAnalysisInfo.SetSpeciesTreeRooted(const Value: Boolean);
begin
  if not Assigned(MyGeneDupsInfo) then
    MyGeneDupsInfo := TGeneDupsInfo.Create;
  MyGeneDupsInfo.SpeciesTreeRooted := Value;
end;

procedure TAnalysisInfo.SetupGroupIds;
begin
  MyGpIds    := nil;
  SetLength(MyGpIds,MyNoOfSeqs);
  GetGpIdForOtuInfos(FMyUsedOtuInfos, MyGpIds);
end;

procedure TAnalysisInfo.SetupOtuNamesFromTreeList;
var
  i: Integer;
begin
  if MyOtuNames <> nil then
    MyOtuNames.Clear
  else
    MyOtuNames := TStringList.Create;
  if FMyOriTreeList.OTUNameList.Count > 0 then
    for i := 0 to FMyOriTreeList.OTUNameList.Count - 1 do
      MyOtuNames.Add(Trim(FMyOriTreeList.OTUNameList[i]));
end;

procedure TAnalysisInfo.SetupOutgroupMembers;
var
  i: Integer;
  temp: String;
  updateTime: TDateTime;
begin
  updateTime := Now;
  MyGpNames.Clear;
  FGroupInfo.GroupNames.Clear;
  for i := 0 to FMyUsedOtuInfos.Count - 1 do
  begin
    with TOtuInfo(FMyUsedOtuInfos[i]) do
    begin
      if Length(GpName) > 0 then
      begin
        temp := Format('%s=%s', [Name, GpName]);
        MyGpNames.Add(temp);
        FGroupInfo.GroupNames.Add(temp);
      end;
      MyOutgroupMembers[i] := OutgroupMember;
    end;
    {$IFDEF VISUAL_BUILD}
    if MilliSecondsBetween(Now, updateTime) > 300 then
    begin
      Application.ProcessMessages;
      updateTime := Now;
    end;
    {$ENDIF}
  end;
end;

//============= PRIVATE FUNCTIONS =============================================
//Add Datasubset information to Runtime Progress
procedure TAnalysisInfo.PrivateAddDataSubsetInfoToRuntimeProgress;
//var
//  AStr: AnsiString;
//  i: Integer;
  //AStrList: TStringList;
begin
{  if ARP = nil then Exit;

  AStrList := nil;
  try
    AStrList := TStringList.Create;
    AStrList.Add('Data File=' + DataFileName);
    AStrList.Add('Data Title=' + DataTitle);

    AnalysisPrefDlg.FillupOptionsInfo(AStrList);

    with AStrList do
      for i:=0 to Count-1 do
        ARP.AddAnalysisOptions(Names[i], Values[Names[i]]);
  finally
    AStrList.Free;
  end;
}
end;

procedure TAnalysisInfo.SetFixedEvolutionaryRate(AValue: Double);
begin
  UseFixedEvolutionaryRate := True;
  if FFixedEvolutionaryRate=AValue then Exit;
  FFixedEvolutionaryRate:=AValue;
end;


procedure TAnalysisInfo.SetMyBootReps(AValue: Integer);
begin
  FMyBootReps := AValue;
end;

procedure TAnalysisInfo.SetMyProcessPack(AValue: TProcessPack);
begin
  if Assigned(FMyProcessPack) then
    FMyProcessPack.OnFreeNotify := nil;
  FMyProcessPack := AValue;
  if not Assigned(FMyProcessPack) then
    Exit;
  FMyProcessPack.OnFreeNotify := ProcessPackFreedNotify;
  if Assigned(FMyProcessPack.WorkflowElement) then
    WorkflowElementFinishedCallBack := FMyProcessPack.WorkflowElement.ElementFinishedCallback;
end;

procedure TAnalysisInfo.SetNumBootstrapSubSamples(AValue: Integer);
begin
  {$IFDEF VISUAL_BUILD}
  FNumBootstrapSubSamples := AValue;
  {$ELSE}
  MyProcessPack.TextualSettingsList.Values[opsBootstrapSubSamples] := IntToStr(AValue);
  {$ENDIF}
end;

procedure TAnalysisInfo.SetNumRepsPerSubSample(AValue: Integer);
begin
  {$IFDEF VISUAL_BUILD}
  FNumRepsPerSubSample := AValue;
  {$ELSE}
  MyProcessPack.TextualSettingsList.Values[opsBootstrapRepsPerSample] := IntToStr(AValue);
  {$ENDIF}
end;

procedure TAnalysisInfo.SetNumSitesPerSubSample(AValue: Int64);
var
  index: Integer;
begin
  {$IFDEF VISUAL_BUILD}
  FNumSitesPerSample := AValue;
  {$ELSE}
  if MyProcessPack.TextualSettingsList.IndexOfName(opsBootstrapSitesPerSample) >= 0 then
    MyProcessPack.TextualSettingsList.Values[opsBootstrapSitesPerSample] := IntToStr(AValue)
  else
    MyProcessPack.TextualSettingsList.Add(Format('%s=%d', [opsBootstrapSitesPerSample, AValue]));

  index := MyProcessPack.TextualSettingsList.IndexOfName(opsBootstrapSampleSizeFactor);
  if index >= 0 then
    MyProcessPack.TextualSettingsList.Delete(index);
  {$ENDIF}
end;

procedure TAnalysisInfo.SetPropagateConstraints(AValue: Boolean);
begin
  if FPropagateConstraints=AValue then Exit;
  FPropagateConstraints:=AValue;
end;

function TAnalysisInfo.GetIngroupDefined: Boolean;
var
  i: Integer;
begin
  Result := False;
  if (not Assigned(FMyUsedOtuInfos)) or (FMyUsedOtuInfos.Count = 0) then
    Exit;
  for i := 0 to FMyUsedOtuInfos.Count - 1 do
    if not TOtuInfo(FMyUsedOtuInfos[i]).OutgroupMember then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TAnalysisInfo.ProcessPackFreedNotify(Sender: TObject);
begin
  if Assigned(FMyProcessPack) and (Sender <> FMyProcessPack) then
    raise Exception.Create('Unexpected sender in ProcessPackFreedNotify');
  FMyProcessPack := nil;
end;

function TAnalysisInfo.GetIsBootstrapSubSampling: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := (FNumBootstrapSubSamples > 0);
  {$ELSE}
  if Trim(MyProcessPack.TextualSettingsList.Values[opsTestPhylo2]) = EmptyStr then
    Result := False
  else
    Result := SameText(MyProcessPack.TextualSettingsList.Values[opsTestPhylo2], LittleBootTestStr);
  {$ENDIF}
end;

function TAnalysisInfo.GetIsRtdt: Boolean;
begin
  Result := False;
  if Assigned(CalibrationTimes) then
    Result := CalibrationTimes.IsSampleTimes;
end;


function TAnalysisInfo.GetMyBootReps: Integer;
begin
  if IsBootstrapSubSampling then
    Result := NumBootstrapSubSamples*NumRepsPerSubSample
  else
    Result := FMyBootReps;
end;


function TAnalysisInfo.GetNumTaxa: LongInt;
begin
  if Assigned(FMyUsedOtuInfos) and (FMyUsedOtuInfos.Count > 0) then
    Result := FMyUsedOtuInfos.Count
  else if Assigned(MyOtuNames) and (MyOtuNames.Count > 0) then { this covers the case where a tree session is loaded}
    Result := MyOtuNames.Count
  else if Assigned(FMyOriTreeList) and (FMyOriTreeList.NoOfOTUs > 0) then { this covers the case where the only data used is a tree}
    Result := FMyOriTreeList.NoOfOTUs
  else
  begin
    Assert(False, 'No data available in TAnalysisInfo.GetNumTaxa - this is a bug!');
    Result := 0;
  end;
end;

function TAnalysisInfo.GetOperatingSystem: String;
begin
  {$IFDEF MSWINDOWS}
  Result := 'Windows';
  {$ENDIF}
  {$IFDEF UNIX}
     {$IFDEF DARWIN}
     Result := 'macOS';
     {$ELSE}
     Result := 'Linux';
     {$ENDIF}
  {$ENDIF}
end;

function TAnalysisInfo.GetUserTreeFileNameOnly: String;
begin
  Result := ExtractFileName(MyUserTreeFile);
end;

function TAnalysisInfo.GetUsesCalibrationDensities: Boolean;
begin
  Result := False;
  if not Assigned(CalibrationTimes) then
    Exit;
  Result := CalibrationTimes.UsesCalibrationDensities;
end;


function TAnalysisInfo.GetIsCompleteDeletion: Boolean;
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
  if dsoCompleteDeletion in MySubsetOpt then
    Result := True;
  {$ELSE}
  if MyProcessPack.TextualSettingsList.Values[opsGapMissingHandle2] = CompleteDelStr then
    Result := True;
  {$ENDIF}
end;


function TAnalysisInfo.GetIsPairwiseDeletion: Boolean;
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
  if dsoPairwiseDeletion in MySubsetOpt then
    Result := True;
  {$ELSE}
  if MyProcessPack.TextualSettingsList.Values[opsGapMissingHandle2] = PairwiseDelStr then   // Was Partial deletion string, fixed
    Result := True
  {$ENDIF}
end;

function TAnalysisInfo.GetIsPartialDeletion: Boolean;
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
  if dsoPartialDeletion in MySubsetOpt then
    Result := True;
  {$ELSE}
  if MyProcessPack.TextualSettingsList.Values[opsGapMissingHandle2] = PartialDelStr then  // was pairwise deletion string, fixed
    Result := True
  {$ENDIF}
end;

function TAnalysisInfo.GetSiteCoverage: Integer;
begin
  Result := -1;
  if IsPartialDeletion then
    Result := MySiteCoverage;
end;

(* added this because caption expert asks for this value in several templates - G.S 11-17-2010 (DP Modified for simplicity)*)
function TAnalysisInfo.GetSiteCoverageReverse: Integer;
begin
  Result := 100 - GetSiteCoverage;
end;

function TAnalysisInfo.GetCodonPositions: AnsiString;
begin
  Result := '';
  if not IsCoding then Exit;

  if dsoUse1stPos in MySubsetOpt then
    Result := Result + '1st';
  if dsoUse2ndPos in MySubsetOpt then
    if Length(Result) > 0 then
      Result := Result + '+2nd'
    else
      Result := Result + '2nd';
  if dsoUse3rdPos in MySubsetOpt then
    if Length(Result) > 0 then
      Result := Result + '+3rd'
    else
      Result := Result + '3rd';
  if dsoUseNonCod in MySubsetOpt then
    if Length(Result) > 0 then
      Result := Result + '+Noncoding'
    else
      Result := Result + 'Noncoding';
end;

function TAnalysisInfo.GetSiteLabelsCaption: AnsiString;
begin
  Result := MySiteLabelCaption;
end;

function TAnalysisInfo.GetIsCoding: Boolean;
begin
  if D_InputSeqData = nil then
    Result := false
  else
    Result := D_InputSeqData.IsCoding;
end;

function TAnalysisInfo.GetHasCalibrations: Boolean;
begin
  if not Assigned(CalibrationTimes) then
    Result := False
  else
    Result := (CalibrationTimes.Count > 0);
end;

function TAnalysisInfo.GetHasDistPack: Boolean;
begin
  Result := MyDistPack <> nil;
end;

function TAnalysisInfo.GetDistComputationTypeTitleCaption: AnsiString;
begin
  Result := '';
  if MyDistPack = nil then Exit;
  case MyDistPack.DistComponent of
    gdCommonSites:      Result := 'Numbers of Common Sites used';
    gdNoOfSynSites:     Result := 'Estimates of the numbers of Synonymous Sites';
    gdNoOfNonsynSites:  Result := 'Estimates of the Numbers of Nonsynonymous Sites';
    gdNoOf0FoldSites:   Result := 'Estimates of the Numbers of Zerofold Degenerate Sites';
    gdNoOf4FoldSites:   Result := 'Estimates of the Numbers of Fourfold Degenerate Sites';
  else
  begin
    if MyDistPack.DoesContain(gdThreeNuc) and (not MyDistPack.DoesContain(gdAmino)) then
    begin
      case MyDistPack.ComputationType of
        gdPairwise:         Result := 'Estimates of Codon-based Evolutionary Divergence between Sequences';
        gdOverallMean:      Result := 'Estimates of Average Codon-based Evolutionary Divergence over all Sequence Pairs';
        gdWithinGroupMean:  Result := 'Estimates of Average Codon-based Evolutionary Divergence over Sequence Pairs within Groups';
        gdBetweenGroupMean: Result := 'Estimates of Codon-based Evolutionary Divergence over Sequence Pairs between Groups';
        gdNetGroupMean:     Result := 'Estimates of Codon-based Net Evolutionary Divergence between Groups of Sequences';
        gdAvgDiversityWithinSubPops: Result := 'Estimates of Mean Codon-based Evolutionary Diversity within Subpopulations';
        gdAvgDiversityForEntirePop: Result := 'Estimate of the Mean Codon-based Evolutionary Diversity for the Entire Population';
        gdInterPopDiversity: Result := 'Estimates of Mean Codon-based Interpopulational Evolutionary Diversity';
        gdPropOfInterPopDiversity: Result := 'Estimate of Codon-based Coefficient of Evolutionary Differentiation';
      end;
    end
    else
      case MyDistPack.ComputationType of
        gdPairwise:         Result := 'Estimates of Evolutionary Divergence between Sequences';
        gdOverallMean:      Result := 'Estimates of Average Evolutionary Divergence over all Sequence Pairs';
        gdWithinGroupMean:  Result := 'Estimates of Average Evolutionary Divergence over Sequence Pairs within Groups';
        gdBetweenGroupMean: Result := 'Estimates of Evolutionary Divergence over Sequence Pairs between Groups';
        gdNetGroupMean:     Result := 'Estimates of Net Evolutionary Divergence between Groups of Sequences';
        gdAvgDiversityWithinSubPops: Result := 'Estimates of the Mean Evolutionary Diversity within Subpopulations';
        gdAvgDiversityForEntirePop: Result := 'Estimate of the Mean Evolutionary Diversity for the Entire Population';
        gdInterPopDiversity: Result := 'Estimates of the Mean Interpopulational Evolutionary Diversity';
        gdPropOfInterPopDiversity: Result := 'Estimate of the Coefficient of Evolutionary Differentiation';
      end;
    end;
  end;
end;

function TAnalysisInfo.GetDistComputationTypeShortCaption: AnsiString;
begin
  Result := '';
  if MyDistPack = nil then Exit;
  case MyDistPack.ComputationType of
    gdPairwise:         Result := 'between sequences'; // analysis is already written in the caption (typo).
    gdOverallMean:      Result := 'averaging over all sequence pairs';
    gdWithinGroupMean:  Result := 'averaging over all sequence pairs within each group';
    gdBetweenGroupMean: Result := 'averaging over all sequence pairs between groups';
    gdNetGroupMean:     Result := 'estimation of net average between groups of sequences';
    gdAvgDiversityWithinSubPops: Result := 'mean diversity calculations within subpopulations';
    gdAvgDiversityForEntirePop: Result := 'mean diversity calculations for the entire population';
    gdInterPopDiversity: Result := 'mean interpopulational diversity calculations';
    gdPropOfInterPopDiversity: Result := 'estimation of coefficient of evolutionary differentiation';
  end;
end;

function TAnalysisInfo.GetDistUnitCaption: AnsiString;
begin
  Result := '';
  if MyDistPack = nil then Exit;
  with MyDistPack do
    case BasicAnalysisType of
      gdOneNuc:
        if DistModel = gdNoOfDiff then
        begin
          case DistComponent of
            gdNucTsOnly   :  Result := 'number of transitional differences per sequence';
            gdNucTvOnly   :  Result := 'number of transversional differences per sequence';
            gdNucRatioTsTv:  Result := 'ratio of transitional to transversional differences';
            gdCommonSites :  Result := 'number of comparable sites between sequences'
          else
            Result := 'number of base differences per sequence';
          end;
        end
        else if DistModel = gdPropDist then
        begin
          case DistComponent of
            gdNucTsOnly   :  Result := 'number of transitional differences per site';
        	  gdNucTvOnly   :  Result := 'number of transversional differences per site';
            gdNucRatioTsTv:  Result := 'ratio of transitional to transversional differences';
            gdCommonSites :  Result := 'number of comparable sites between sequences'
          else
            Result := 'number of base differences per site';
          end;
        end
        else
        begin
          case DistComponent of
            gdNucTsOnly   :  Result := 'number of transitional substitutions per site';
            gdNucTvOnly   :  Result := 'number of transversional substitutions per site';
            gdNucRatioTsTv:  Result := 'ratio of transitional to transversional distances per site (R)';
            gdCommonSites :  Result := 'number of comparable sites between sequences'
				  else
            Result := 'number of base substitutions per site';
          end;
        end;
    gdThreeNuc:   // note that DistCorrection is the same as distmodel if it is
       if MyDistPack.DoesContain(gdAmino) then
       begin
        case DistCorrection of
          gdNoOfDiff        : Result := 'number of amino acid differences per sequence';
          gdPropDist        : Result := 'number of amino acid differences per site';
          gdCommonSites     :  Result := 'number of comparable sites between sequences'
        else
          Result := 'number of amino acid substitutions per site';
        end;
       end
       else
       begin
         case DistCorrection of
            gdNoOfDiff:
    	  	  	case DistComponent of
                gdSynOnly     : Result := 'number of synonymous differences per sequence';
                gdNonsynOnly  : Result := 'number of nonsynonymous differences per sequence';
                gdNonsyn0Fold : Result := 'number of differences at the zerofold-degenerate codon positions';
                gdSyn4Fold    : Result := 'number of differences at the fourfold-degenerate codon positions';
                gdNoOfSynSites:     Result := 'number of synonymous sites';
                gdNoOfNonsynSites:  Result := 'number of nonsynonymous sites';
                gdNoOf0FoldSites:   Result := 'number of zerofold-degenerate sites';
                gdNoOf4FoldSites:   Result := 'number of fourfold-degenerate sites';
              end;
            gdPropDist:
    	    		case DistComponent of
                gdSynOnly     : Result := 'number of synonymous differences per synonymous site';
                gdNonsynOnly  : Result := 'number of nonsynonymous differences per nonsynonymous site';
                gdNonsyn0Fold : Result := 'number of differences per site at the zerofold-degenerate codon positions';
                gdSyn4Fold    : Result := 'number of differences per site at the fourfold-denegrate codon positions';
                gdDiffSynNonsyn:  Result := 'difference between the synonymous and nonsynonymous differences per site';
                gdDiffNonsynSyn:  Result := 'difference between the nonsynonymous and synonymous differences per site';
                gdNoOfSynSites:     Result := 'number of synonymous sites';
                gdNoOfNonsynSites:  Result := 'number of nonsynonymous sites';
                gdNoOf0FoldSites:   Result := 'number of zerofold-degenerate sites';
                gdNoOf4FoldSites:   Result := 'number of fourfold-degenerate sites';
              end;
            else
    	  		  case DistComponent of
                gdSynOnly     : Result := 'number of synonymous substitutions per synonymous site';
                gdNonsynOnly  : Result := 'number of nonsynonymous substitutions per nonsynonymous site';
                gdNonsyn0Fold : Result := 'number of substitutions per site at the zerofold-degenerate codon positions';
                gdSyn4Fold    : Result := 'number of substitutions per site at the fourfold-degenerate codon positons';
                gdDiffSynNonsyn:  Result := 'difference between the synonymous and nonsynonymous distances per site';
                gdDiffNonsynSyn:  Result := 'difference between the nonsynonymous and synonymous distances per site';
                gdNoOfSynSites:     Result := 'number of synonymous sites';
                gdNoOfNonsynSites:  Result := 'number of nonsynonymous sites';
                gdNoOf0FoldSites:   Result := 'number of zerofold-degenerate sites';
                gdNoOf4FoldSites:   Result := 'number of fourfold-degenerate sites';
              end;
         end;
       end;
    gdAmino:
      case DistCorrection of
        gdNoOfDiff        : Result := 'number of amino acid differences per sequence';
        gdPropDist        : Result := 'number of amino acid differences per site';
        gdCommonSites :  Result := 'number of comparable sites between sequences'
      else
        Result := 'number of amino acid substitutions per site';
      end;
    end;
end;

function TAnalysisInfo.GetDistGammaParaCaption: AnsiString;
begin
  Result := '';
  if MyDistPack = nil then Exit;
  if MyDistPack.GammaParameter > 0 then
	  Result := 'The rate variation among sites was modeled with a gamma distribution';
end;

function TAnalysisInfo.GetKumarGadagkar2001InfoCaption: AnsiString;
begin
  Result := '';
  if MyDistPack = nil then Exit;
  if MyDistPack.DoesContain(gdDisparityIndexTest) then
        Result := 'Disparity Index Test'
  else if MyDistPack.DoesContain(gdDisparityIndex) then
        Result := 'Disparity Index'
  else if MyDistPack.DoesContain(gdCompositionDistance) then
        Result := 'Composition Distance';
end;

function TAnalysisInfo.GetDistHeteroPatternCaption: AnsiString;
begin
  Result := '';
  if MyDistPack = nil then Exit;
  if MyDistPack.DoesContain(gdHetero) then
	  Result := 'The differences in the composition bias among sequences were considered in evolutionary comparisons';
end;

function TAnalysisInfo.GetDistModelName: AnsiString;
begin
  Result := '';
  if MyDistPack = nil then Exit;
  if (MyDistPack.DistModel = gdNoOfDiff) or (MyDistPack.DistModel = gdPropDist) then
    Exit;
  Result := MyDistPack.SimpleName;
  if MyDistPack.DoesContain(gdIna) then //'modified Nei-Gojobori';
    Result := Result + ' (assumed transition/transversion bias = '+FloatToStr(MyDistPack.InaRValue)+')';
end;

function TAnalysisInfo.ReadFromFile(var SessionFile: File; SessionVersion: LongInt):boolean;
var
  i: Integer = -1;
  j: Integer = -1;
  k: Integer = -1;
  n: Integer = -1;
  version: integer = -1;
  c : AnsiChar = #0;
  str: AnsiString = '';
  b: Boolean = False;
begin
  result := True;
  try
    if SessionVersion >= 602 then // MEGA6
    begin
      BlockRead(SessionFile, FMergeRates, SizeOf(Boolean));
      BlockRead(SessionFile, FClockType, SizeOf(FClockType));
      BlockRead(SessionFile, FClockLevel, SizeOf(FClockLevel));
      BlockRead(SessionFile, MyUsrOperation, SizeOf(MyUsrOperation));
      BlockRead(SessionFile, MySiteCoverage, SizeOf(MySiteCoverage));
      BlockRead(SessionFile, i, SizeOf(Integer));
      if i > 0 then
      begin
       CalibrationTimes := TCalibrations.Create;
       CalibrationTimes.ReadFromSessionFile(SessionFile, SessionVersion, i);
      end;
    end;

    if SessionVersion >= 606 then
    begin
      if not Assigned(ClockTreeExport) then
        ClockTreeExport := TStringList.Create
      else
        ClockTreeExport.Clear;
      BlockRead(SessionFile, b, SizeOf(Boolean));
      if b = True then
      begin
        BlockRead(SessionFile, n, SizeOf(Integer));
        if n > 0 then
        begin
          for i := 0 to n-1 do
          begin
            BlockRead(SessionFile, j, SizeOf(j));
            if j > 0 then
            begin
              SetLength(str, j);
              for k := 1 to j do
              begin
                BlockRead(SessionFile, c, SizeOf(c));
                str[k] := c;
              end;
              ClockTreeExport.Add(str);
            end;
          end;
        end;
      end;
    end;
    BlockRead(SessionFile, version, 4);

    BlockRead(SessionFile, MyNoOfSeqs, SizeOf(MyNoOfSeqs));
    BlockRead(SessionFile, MyNoOfSites, SizeOf(MyNoOfSites));
    BlockRead(SessionFile, i, 4);
    BlockRead(SessionFile, i, 4);
    BlockRead(SessionFile, MyNoOfStates, SizeOf(MyNoOfStates));
    BlockRead(SessionFile, MyNoOfInfoSites, SizeOf(MyNoOfInfoSites));
    BlockRead(SessionFile, MyConstContribute, SizeOf(MyConstContribute));
    BlockRead(SessionFile, MyTvWeight, SizeOf(MyTvWeight));
    BlockRead(SessionFile, MySearchFactor, SizeOf(MySearchFactor));
    BlockRead(SessionFile, MySearchLevel, SizeOf(MySearchLevel));
    BlockRead(SessionFile, MyNoOfGps, SizeOf(MyNoOfGps));

    BlockRead(SessionFile, n, SizeOf(n));
    if n > 0 then
    begin
      SetLength(MyGpIds, n);
      for i := 0 to n-1 do
      begin
       BlockRead(SessionFile, j, SizeOf(j));
       MyGpIds[i] := j;
      end;
    end;
    BlockRead(SessionFile, MyNoOfClusters, SizeOf(MyNoOfClusters));

    BlockRead(SessionFile, n, SizeOf(n));
    if n > 0 then
    begin
      SetLength(MyClusterIds, n);
      for i := 0 to n-1 do
      begin
       BlockRead(SessionFile, j, SizeOf(j));
       MyClusterIds[i] := j;
      end;
    end;
    if Version = 1 then  // MEGA5 added additional entries to MySubsetOpt making it's size increase and messing up the offset of the session.
      BlockRead(SessionFile, MySubsetOpt, 2)
    else
      BlockRead(SessionFile, MySubsetOpt, 4);  // bugfix, Third argument was SizeOf(TDataSubsetOptions) but this changes when you add or remove entries from dsoDataSubsetOption

    if MyOtuNames = nil then
      MyOtuNames := TStringList.Create
    else
      MyOtuNames.Clear;
    BlockRead(SessionFile, n, SizeOf(n));
    if n > 0 then
      for i := 0 to n-1 do
      begin
        BlockRead(SessionFile, j, SizeOf(j));
        if j > 0 then
        begin
          SetLength(str, j);
          for k := 1 to j do
          begin
            BlockRead(SessionFile, c, SizeOf(c));
            str[k] := c;
          end;
          MyOtuNames.Add(str);
        end;
      end;

    if MyGpNames = nil then
      MyGpNames := TStringList.Create
    else
      MyGpNames.Clear;
    BlockRead(SessionFile, n, SizeOf(n));
    if n > 0 then
      for i := 0 to n-1 do
      begin
        BlockRead(SessionFile, j, SizeOf(j));
        if j > 0 then
        begin
          SetLength(str, j);
          for k := 1 to j do
          begin
            BlockRead(SessionFile, c, SizeOf(c));
            str[k] := c;
          end;
          MyGpNames.Add(str);
        end;
      end;

    BlockRead(SessionFile, n, SizeOf(n));
    if n > 0 then
    begin
      SetLength(MySiteFreqs, n);
      for i := 0 to n-1 do
        BlockRead(SessionFile, MySiteFreqs[i], SizeOf(Integer));
    end;
    BlockRead(SessionFile, i, SizeOf(i));
    MyBootReps := i;
    BlockRead(SessionFile, MyNoOfReps, SizeOf(MyNoOfReps));
    BlockRead(SessionFile, MyValidReps, SizeOf(MyValidReps));
    if SessionVersion >= 604 then
    begin
      BlockRead(SessionFile, b, SizeOf(Boolean));
      if b then
      begin
        MyGeneDupsInfo := TGeneDupsInfo.Create;
        Result := Result and MyGeneDupsInfo.ReadFromFile(SessionFile, SessionVersion);
      end;
    end;

    if SessionVersion >= 700 then
    begin
      BlockRead(SessionFile, b, SizeOf(Boolean));
      if b then
      begin
        BlockRead(SessionFile, n, SizeOf(integer));
        for i := 0 to n-1 do
        begin
          BlockRead(SessionFile, b, SizeOf(Boolean));
          MyOutgroupMembers[i] := b;
        end;

      end;
      FMyUsedOtuInfos := TList.Create;
    end;

    if SessionVersion >= 1004 then
    begin
      BlockRead(SessionFile, FUseFixedEvolutionaryRate, SizeOf(FUseFixedEvolutionaryRate));
      BlockRead(SessionFile, FFixedEvolutionaryRate, SizeOf(FFixedEvolutionaryRate));
    end;
  except
    on E:Exception do
    begin
      result := False;
      Assert(False, 'Error in TAnalysisInfo.ReadFromFile: ' + E.Message);
    end;
  end;
end;

procedure TAnalysisInfo.WriteToFile(var SessionFile: File; SessionVersion: LongInt);
var
  i,j,k,n: integer;
  c: AnsiChar = #0;
  b: Boolean = False;
  updateTime: TDateTime;
begin
  updateTime := Now;
  if SessionVersion >= 602 then // MEGA6
  begin
    BlockWrite(SessionFile, FMergeRates, SizeOf(Boolean));
    BlockWrite(SessionFile, FClockType, SizeOf(FClockType));
    BlockWrite(SessionFile, FClockLevel, SizeOf(FClockLevel));
    BlockWrite(SessionFile, MyUsrOperation, SizeOf(MyUsrOperation));
    BlockWrite(SessionFile, MySiteCoverage, SizeOf(MySiteCoverage));
    if not Assigned(CalibrationTimes) then
      i := 0
    else
      i := CalibrationTimes.Count; { awkward but done here for backward-compatibility}
    BlockWrite(SessionFile, i, SizeOf(Integer));
    if i > 0 then
      CalibrationTimes.WriteToSessionFile(SessionFile);
  end;

  if Assigned(ClockTreeExport) and (ClockTreeExport.Count > 0) then
  begin
    b := True;
    BlockWrite(SessionFile, b, SizeOf(b));
    n := ClockTreeExport.Count;
    BlockWrite(SessionFile, n, SizeOf(n));
    for i := 0 to ClockTreeExport.Count - 1 do
    begin
      j := Length(ClockTreeExport[i]);
      BlockWrite(SessionFile, j, SizeOf(j));
      if j > 0 then
        for k := 1 to j do
        begin
          c := AnsiString(ClockTreeExport[i])[k];
          BlockWrite(SessionFile, c, SizeOf(c));
        end;
    end;
  end
  else
  begin
    b := False;
    BlockWrite(SessionFile, b, SizeOf(b));
  end;

  i := 2;  //version
  BlockWrite(SessionFile, i, 4);

  BlockWrite(SessionFile, MyNoOfSeqs, SizeOf(MyNoOfSeqs));
  BlockWrite(SessionFile, MyNoOfSites, SizeOf(MyNoOfSites));
  i := 0;
  BlockWrite(SessionFile, i, 4);
  BlockWrite(SessionFile, i, 4);
  BlockWrite(SessionFile, MyNoOfStates, SizeOf(MyNoOfStates));
  BlockWrite(SessionFile, MyNoOfInfoSites, SizeOf(MyNoOfInfoSites));
  BlockWrite(SessionFile, MyConstContribute, SizeOf(MyConstContribute));
  BlockWrite(SessionFile, MyTvWeight, SizeOf(MyTvWeight));
  BlockWrite(SessionFile, MySearchFactor, SizeOf(MySearchFactor));
  BlockWrite(SessionFile, MySearchLevel, SizeOf(MySearchLevel));

  BlockWrite(SessionFile, MyNoOfGps, SizeOf(MyNoOfGps));

  n := Length(MyGpIds);
  BlockWrite(SessionFile, n, SizeOf(n));
  if n > 0 then
    for i := 0 to n-1 do
      BlockWrite(SessionFile, MyGpIds[i], SizeOf(Integer));

  BlockWrite(SessionFile, MyNoOfClusters, SizeOf(MyNoOfClusters));

  n := Length(MyClusterIds);
  BlockWrite(SessionFile, n, SizeOf(n));
  if n > 0 then
    for i := 0 to n-1 do
      BlockWrite(SessionFile, MyClusterIds[i], SizeOf(Integer));

  BlockWrite(SessionFile, MySubsetOpt, 4); // bugfix, Third argument was SizeOf(TDataSubsetOptions) but this changes when you add or remove entries from dsoDataSubsetOption

  n := MyOtuNames.Count;
  BlockWrite(SessionFile, n, SizeOf(n));
  if n > 0 then
    for i := 0 to n-1 do
    begin
      j := Length(MyOtuNames[i]);
      BlockWrite(SessionFile, j, SizeOf(j));
      if j > 0 then
        for k := 1 to j do
        begin
          c := AnsiString(MyOtuNames[i])[k];
          BlockWrite(SessionFile, c, SizeOf(c));
        end;
      if MilliSecondsBetween(Now, updateTime) > 200 then
      begin
        {$IFDEF VISUAL_BUILD}Application.ProcessMessages;{$ENDIF}
        updateTime := Now;
      end;
    end;
  if Assigned(MyGpNames) then
    n := MyGpNames.Count
  else
    n := 0;
  BlockWrite(SessionFile, n, SizeOf(n));
  if n > 0 then
    for i := 0 to n-1 do
    begin
      j := Length(MyGpNames[i]);
      BlockWrite(SessionFile, j, SizeOf(j));
      if j > 0 then
        for k := 1 to j do
        begin
          c := AnsiString(MyGpNames[i])[k];
          BlockWrite(SessionFile, c, SizeOf(c));
        end;
      if MilliSecondsBetween(Now, updateTime) > 200 then
      begin
        {$IFDEF VISUAL_BUILD}Application.ProcessMessages;{$ENDIF}
        updateTime := Now;
      end;
    end;

  n := Length(MySiteFreqs);
  BlockWrite(SessionFile, n, SizeOf(n));
  if n > 0 then
    for i := 0 to n-1 do
    begin
      BlockWrite(SessionFile, MySiteFreqs[i], SizeOf(Integer));
      if MilliSecondsBetween(Now, updateTime) > 200 then
      begin
        {$IFDEF VISUAL_BUILD}Application.ProcessMessages;{$ENDIF}
        updateTime := Now;
      end;
    end;

  BlockWrite(SessionFile, MyBootReps, SizeOf(MyBootReps));
  BlockWrite(SessionFile, MyNoOfReps, SizeOf(MyNoOfReps));
  BlockWrite(SessionFile, MyValidReps, SizeOf(MyValidReps));
  if Assigned(MyGeneDupsInfo) then
    b := True
  else
    b := False;
  blockWrite(SessionFile, b, SizeOf(Boolean));
  if b = True then
    MyGeneDupsInfo.WriteToFile(SessionFile, SessionVersion);

  b := True;
  blockWrite(SessionFile, b, SizeOf(Boolean));
  if b = True then
  begin
    n := FGroupInfo.NumTaxa;
    BlockWrite(SessionFile, n, SizeOf(n));
    for i := 0 to n-1 do
    begin
      b := MyOutgroupMembers[i];
      BlockWrite(SessionFile, b, SizeOf(boolean));
    end;
  end;
  BlockWrite(SessionFile, FUseFixedEvolutionaryRate, SizeOf(Boolean));
  BlockWrite(SessionFile, FFixedEvolutionaryRate, SizeOf(FFixedEvolutionaryRate));
end;

function TAnalysisInfo.GetIsAminoAcid: boolean;
begin
  Result := False;
  if MyDistPack <> nil then
    Result := MyDistPack.DoesContain(gdAmino)
  else if MyProcessPack <> nil then
    Result := MyProcessPack.HasAminoAcidData
  else if (not IsGeneDupsTree) and (not IsReltimeBLensAnalysis(MyUsrOperation)) then
    Raise Exception.Create('AnalysisInfo is missing packs');
end;

function TAnalysisInfo.GetIsPairwiseStyleAnalysis: Boolean;
begin
  Result := False;
  case MyUsrOperation of
    dtdoPairwiseDist, dtdoOverallMean, dtdoWithinGroupMean,
    dtdoBetweenGroupMean, dtdoNetGroupMean,
    dtdoAvgDiversityWithinSubPops, dtdoAvgDiversityForEntirePop,
    dtdoInterPopDiversity, dtdoPropOfInterPopDiversity,
    dtdoSelectionExactTest, dtdoSelectionZGpTest, dtdoSelectionZTest,
    dtdoTajimaNeutralityTest,
    dtdoCompositionDistance, dtdoDisparityIndex,
    dtdoDisparityIndexTest,
    dtdoTajimaClockTest,
    dtdoUPGMATree, dtdoNJTree, dtdoMETree,
    dtdoOLSComputeUserTreeBLens,
    dtdoPhyloQ,
    dtdoMCLComputePattern,
    dtdoMCLTsTvBias,
    dtdoOLSInteriorBranchTest:
      Result := True;
  end;
end;

function TAnalysisInfo.GetGlobalClockType: Integer;
begin
  result := GlobalClockType;
end;

function TAnalysisInfo.GetGroupInfo: TGroupInfo;
begin
  FGroupInfo.UsedOtuInfos := FMyUsedOtuInfos;
  FGroupInfo.TreeList := FMyOriTreeList;
  Result := FGroupInfo;
end;

function TAnalysisInfo.GetMaxRateRatio: Extended;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FMaxRateRatio;
  {$ELSE}
  Result := MyProcessPack.MaxRateRatio;
  {$ENDIF}
end;

function TAnalysisInfo.GetMLBlensFilter: Extended;
begin
  result := MyMLBLensFilter;
end;

function TAnalysisInfo.GetMyGpNames: TStringList;
begin
  Result := FGroupInfo.GroupNames;
end;

function TAnalysisInfo.GetMyOutgroupMembers(Index: Integer): Boolean;
begin
  Result := FGroupInfo.IsOutgroupMember[Index];
end;

function TAnalysisInfo.StateToStringList(Comment: String=''): TStringList;
var
  MyStringList: TStringList;
  NewLine: AnsiString;
  TempStringList: TStringList;
  i: Integer;
begin
  NewLine := LineEnding;
  MyStringList := TStringList.Create();
  TempStringList := TStringList.Create();
  MyStringList.Add('TAnalysisInfo=MyName');

  if Comment <> EmptyStr then
	MyStringList.Add('Comment=' + Comment);

  MyStringList.Add('TAnalysisInfo.FLogLWClock=' + FLoglWClock);
  MyStringList.Add('TAnalysisInfo.FLoglWoClock=' + FLoglWoClock);

  MyStringList.Add('TAnalysisInfo.MolClockTest=' + FMolClockTest);
  MyStringList.Add('TAnalysisInfo.FLoglWoClock=' + FLoglWoClock);

  MyStringList.Add('TAnalysisInfo.InvarWClock=' + FInvarWClock);
  MyStringList.Add('TAnalysisInfo.InvarWoClock=' + FInvarWoClock);

  MyStringList.Add('TAnalysisInfo.GammaWClock=' + FGammaWClock);
  MyStringList.Add('TAnalysisInfo.GammaWoClock=' + FGammaWoClock);
  MyStringList.Add('TAnalysisInfo.FullMdlName=' + FFullMdlName);
  MyStringList.Add('TAnalysisInfo.codonsUsed=' + FCodonsUsed);

  MyStringList.Add('TAnalysisInfo.MyNoOfSeqs=' + IntToStr(MyNoOfSeqs));
  MyStringList.Add('TAnalysisInfo.MyNoOfSites]=' + IntToStr(MyNoOfSites));
  if MySeqStrings <> nil then
  begin
    for i := 0 to MySeqStrings.Count - 1 do
    begin
      MyStringList.Add('TAnalysisInfo.SeqString_[' + IntToStr(i) + ']=' + IntToStr(MyNoOfSites));
    end;
  end;

  MyStringList.Add('TAnalysisInfo.MyNoOfStates=' + IntToStr(MyNoOfStates));
  MyStringList.Add('TAnalysisInfo.MyNoOfInfoSites=' + IntToStr(MyNoOfInfoSites));
  MyStringList.Add('TAnalysisInfo.MyConstContribute=' + IntToStr(MyConstContribute));
  MyStringList.Add('TAnalysisInfo.MyTvWeight=' + IntToStr(MyTvWeight));

  if MyLabelsUsed <> nil then
  begin
    for i := 0 to MyLabelsUsed.Count - 1 do
    begin
      MyStringList.Add('TAnalysisInfo.MyLabelsUsed[' + IntToStr(i) + ']=' + MyLabelsUsed[i]);
    end;
  end;


  MyStringList.Add('TAnalysisInfo.MyRecodeScheme=' + MyRecodeScheme);
  MyStringList.Add('TAnalysisInfo.MyRecodeSchemeName=' + MyRecodeSchemeName);

  MyStringList.Add('TAnalysisInfo.MyNoOfGammaCat=' + IntToStr(MyNoOfGammaCat));
  MyStringList.Add('TAnalysisInfo.MySiteCoverage=' + IntToStr(MySiteCoverage));        // for Partial Deletion
  MyStringList.Add('TAnalysisInfo.MySearchFactor=' + IntToStr(MySearchFactor));
  MyStringList.Add('TAnalysisInfo.MySearchLevel=' + IntToStr(MySearchLevel));
  MyStringList.Add('TAnalysisInfo.MyNoOfGps=' + IntToStr(MyNoOfGps));

  if MyGpIds <> nil then
  begin
    if Length(MyGpIds) > 0 then
    for i := 0 to Length(MyGpIds) - 1 do
    begin
      MyStringList.Add('TAnalysisInfo.MyGpIds[' + IntToStr(i) + ']=' + IntToStr(MyGpIds[i]));
    end;
  end;


  MyStringList.Add('TAnalysisInfo.MyNoOfClusters=' + IntToStr(MyNoOfClusters));

  if MyClusterIds <> nil then
  begin
    if Length(MyClusterIds) > 0 then
    for i := 0 to Length(MyClusterIds) - 1 do
    begin
      MyStringList.Add('TAnalysisInfo.MyClusterIds[' + IntToStr(i) + ']=' + IntToStr(MyClusterIds[i]));
    end;
  end;

  for i := 0 to MyOtuNames.Count - 1 do
    MyStringList.Add('TAnalysisInfo.MyOtuNames[' + IntToStr(i) + ']=' + MyOtuNames[i]);
  if Assigned(MyGpNames) then
    for i := 0 to MyGpNames.Count - 1 do
      MyStringList.Add('TAnalysisInfo.MyGpNames[' + IntToStr(i) + ']=' + MyGpNames[i]);

  MyStringList.Add('TAnalysisInfo.MyNumThreadsToUse=' + IntToStr(MyNumThreadsToUse));
  MyStringList.Add('TAnalysisInfo.MyInitialTreeMethod=' + IntToStr(MyInitialTreeMethod));
  MyStringList.Add('TAnalysisInfo.MyBootReps=' + IntToStr(MyBootReps));
  MyStringList.Add('TAnalysisInfo.MyNoOfReps=' + IntToStr(MyNoOfReps));
  MyStringList.Add('TAnalysisInfo.MyValidReps=' + IntToStr(MyValidReps));

  if MySiteFreqs <> nil then
  begin
    for i := 0 to Length(MySiteFreqs) - 1 do
      MyStringList.Add('TAnalysisInfo.MySiteFreqs[' + IntToStr(i) + ']=' + IntToStr(MySiteFreqs[i]));
  end;

  MyStringList.Add('TAnalysisInfo.MyUserTreeFile=' + MyUserTreeFile);
  MyStringList.Add('TAnalysisInfo.MyUserNewickTree=' + MyUserNewickTree);

  MyStringList.Add('TAnalysisInfo.GlobalClockType=' + IntToStr(GlobalClockType));
  MyStringList.Add('TAnalysisInfo.ClockTypeSet=' + BoolToStr(ClockTypeSet));

  MyStringList.Add('TAnalysisInfo.MyUsrOperation=' + GetEnumName(TypeInfo(TDistTreeDlgOption), integer(MyUsrOperation)));

  if MyProcessPack <> nil then
  begin
    MyStringList.Add(NewLine);
    TempStringList := MyProcessPack.StateToStringList(EmptyStr);
    for i := 0 to TempStringList.Count - 1 do
      MyStringList.Add(TempStringList[i]);
  end
  else
    MyStringList.Add('TAnalysisInfo.MyProcessPack=nil');

  if MyDistPack <> nil then
  begin
    MyStringList.Add(NewLine);
    TempStringList := MyDistPack.StateToStringList(EmptyStr);
    for i := 0 to TempStringList.Count - 1 do
      MyStringList.Add(TempStringList[i]);
  end
  else
    MyStringList.Add('TAnalysisInfo.MyDistPack=nil');

  if MyTreePack <> nil then
  begin
    MyStringList.Add(NewLine);
    TempStringList := MyTreePack.StateToStringList();
    for i := 0 to TempStringList.Count - 1 do
      MyStringList.Add(TempStringList[i]);
  end
  else
    MyStringList.Add('TAnalysisInfo.MyTreePack=nil');

  MyStringList.Add(NewLine);
  TempStringList := SubSetOptionsToStringList();
  for i := 0 to TempStringList.Count - 1 do
    MyStringList.Add(TempStringList[i]);

  if MyOriD <> nil then
  begin
    MyStringList.Add(NewLine);
    MyStringList.Add('TAnalysisInfo.MyOriD=');
    MyStringList.Add(NewLine);
    TempStringList := DistanceMatrixToStringList(MyOriD, MyNoOfSeqs);
    for i := 0 to TempStringList.Count - 1 do
      MyStringList.Add(TempStringList[i]);
  end
  else
  begin
    MyStringList.Add(NewLine);
    MyStringList.Add('TAnalysisInfo.MyOriD=nil');
  end;

  if MyShowD <> nil then
  begin
    MyStringList.Add(NewLine);
    MyStringList.Add('TAnalysisInfo.MyShowD=');
    MyStringList.Add(NewLine);
    TempStringList := DistanceMatrixToStringList(MyShowD, MyNoOfSeqs);
    for i := 0 to TempStringList.Count - 1 do
      MyStringList.Add(TempStringList[i]);
  end
  else
  begin
    MyStringList.Add(NewLine);
    MyStringList.Add('TAnalysisInfo.MyShowD=nil');
  end;

if MyBootD <> nil then
  begin
    MyStringList.Add(NewLine);
    MyStringList.Add('TAnalysisInfo.MyBootD=');
    MyStringList.Add(NewLine);
    TempStringList := DistanceMatrixToStringList(MyBootD, MyNoOfSeqs);
    for i := 0 to TempStringList.Count - 1 do
      MyStringList.Add(TempStringList[i]);
  end
  else
  begin
    MyStringList.Add(NewLine);
    MyStringList.Add('TAnalysisInfo.MyBootD=nil');
  end;

  Result := MyStringList;
  // come back to these when we implement tostring functions for these classes
  // MyMappedData:     TList;
  // MyInfoSites:      TList;
  // FMyUsedOtuInfos: TList;
  // MyMLAnalysisPack: TMLTreeAnalyzer;

end;

function TAnalysisInfo.SubSetOptionsToStringList: TStringList;
var
  MyStringList: TStringList;
  NewLine: String;
begin
  NewLine := LineEnding;
  MyStringList := TStringList.Create;
  MyStringList.Add(NewLine);
  MyStringList.Add('MyType=SubsetOptions');

  if dsoUseNuc in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoUseNuc=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoUseNuc=False');

  if dsoUseAmino in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoUseAmino=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoUseAmino=False');

  if dsoUseCodon in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoUseCodon=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoUseCodon=False');

  if dsoCompleteDeletion in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoCompleteDeletion=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoCompleteDeletion=False');

  if dsoPairwiseDeletion in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoPairwiseDeletion=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoPairwiseDeletion=False');

  if dsoPartialDeletion in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoPartialDeletion=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoPartialDeletion=False');

  if dsoUseOnlyLabelledSites in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoUseOnlyLabelledSites=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoUseOnlyLabelledSites=False');

  if dsoUseNonCod in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoUseNonCod=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoUseNonCod=False');

  if dsoUse1stPos in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoUse1stPos=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoUse1stPos=False');

  if dsoUse2ndPos in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoUse2ndPos=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoUse2ndPos=False');

  if dsoUse3rdPos in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoUse3rdPos=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoUse3rdPos=False');

  if dsoParsimMap in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoParsimMap=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoParsimMap=False');

  if dsoNucToTvParsim in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoNucToTvParsim=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoNucToTvParsim=False');

  if dsoDistMap in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoDistMap=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoDistMap=False');

  if dsoNoMap in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoNoMap=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoNoMap=False');

  if dsoRemoveInvar in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoRemoveInvar=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoRemoveInvar=False');

  if dsoRemoveUninfoVar in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoRemoveUninfoVar=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoRemoveUninfoVar=False');

  if dsoRecodeBases in MySubSetOpt then
    MyStringList.Add('TAnalysisInfo.dsoRecodeBases=True')
  else
    MyStringList.Add('TAnalysisInfo.dsoRecodeBases=False');

  Result := MyStringList;
end;


/// TPhyloQAnalysisInfo methods


/// <summary>Create an instance of TPhyloQAnalysisInfo</summary>
constructor TPhyloQAnalysisInfo.Create;
begin
  inherited Create;
end;

procedure TPhyloQAnalysisInfo.ReadTargetData;
var
  PhyloQTargetFileName: AnsiString;
  MyFile: TextFile;
  MyText: AnsiString;
begin
  {$IFDEF VISUAL_BUILD}
  PhyloQTargetFileName := MegaForm.DataFileName;
  {$ELSE}
  PhyloQTargetFileName := D_MegaMain.PhyloQFileName;
  {$ENDIF}

  AssignFile(MyFile, PhyloQTargetFileName);
  FileMode := fmOpenRead;
  Reset(MyFile);
  while not Eof(MyFile) do
  begin
    ReadLn(MyFile, MyText);
    if (Pos('>', MyText)) > 0 then
      PhyloQTargetName := Copy(MyText, 2, Length(MyText) - 1)
    else
      PhyloQTargetSequence := MyText;
  end;
  CloseFile(MyFile);
end;

/// <summary>Compute the pairwise distances between the target sequence and each
/// other sequence in the user's alignment</summary>
/// <note>Distances are not computed between all pairs in the alignment, only between
/// the target sequence (first in alignment) and the rest.</note>
procedure TPhyloQAnalysisInfo.ComputePwDistances;
begin
  FPhyloQPairwiseDistances := ComputePairwiseDistances(FPairwiseAlignments, Self);
end;

/// <summary>Add FPhyloQPairwiseDistances to the MyOriD distance matrix so that MyOriD
/// becomes an (n+1) x (n+1) distance matrix instead of n x n and the added pairwise
/// distances are calculated from pairwise alignments between the PhyloQ target sequence</summary>
/// <note>FPhyloQPairwseDistances are assumed to already exist</note>
procedure TPhyloQAnalysisinfo.UpdateAugmentedMatrix;
begin
  MyOriD := AugmentDistanceMatrix(MyOriD,FPhyloQPairwiseDistances);

  SwapRectDistEntries(MyOriD, MyNoOfSeqs + 1, 0, MyNoOfSeqs); // this is necessary for the PhyloQ analysis because the
  // TOLSSearchLastOtuPositionThread targets the last taxon but our input sets the first sequence as the target
  MyOtuNames.Insert(0, PhyloQTargetName);
  MyOtuNames.Exchange(0, MyOtuNames.Count - 1);

end;


{ TGeneDupsAnalysisInfo }

procedure TGeneDupsInfo.Assign(Other: TGeneDupsInfo);
begin
  GeneTreeRooted := Other.GeneTreeRooted;
  SpeciesTreeRooted := Other.SpeciesTreeRooted;
  UsedSpeciesTree := Other.UsedSpeciesTree;
  NumTreeConfigs := Other.NumTreeConfigs;
  NumGeneDups := Other.NumGeneDups;
  NumSpeciations := Other.NumSpeciations;
  NumSpecies := Other.NumSpecies;
end;

constructor TGeneDupsInfo.Create;
begin
  inherited Create;
  GeneTreeRooted := False;
  SpeciesTreeRooted := False;
  UsedSpeciesTree := False;
  NumTreeConfigs := 0;
  NumGeneDups := 0;
  NumSpeciations := 0;
  NumSpecies := 0;
  RootIndex := -1;
end;

destructor TGeneDupsInfo.Destroy;
begin

  inherited Destroy;
end;

function TGeneDupsInfo.GeneDupsFound: Boolean;
begin
  Result := (NumGeneDups > 0);
end;

function TGeneDupsInfo.ReadFromFile(var SessionFile: File; SessionVersion: Integer): Boolean;
begin
  Result := False;
  try
    BlockRead(SessionFile,GeneTreeRooted, SizeOf(Boolean));
    BlockRead(SessionFile,SpeciesTreeRooted, SizeOf(Boolean));
    BlockRead(SessionFile,UsedSpeciesTree, SizeOf(Boolean));
    BlockRead(SessionFile,NumTreeConfigs, SizeOf(Integer));
    BlockRead(SessionFile,NumGeneDups, SizeOf(Integer));
    BlockRead(SessionFile,NumSpeciations, SizeOf(Integer));
    BlockRead(SessionFile,NumSpecies, SizeOf(Integer));
    BlockRead(SessionFile,RootIndex, SizeOf(Integer));
    Result := True;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

function TGeneDupsInfo.WriteToFile(var SessionFile: File; SessionVersion: Integer): Boolean;
begin
  Result := False;
  try
    BlockWrite(SessionFile,GeneTreeRooted, SizeOf(Boolean));
    BlockWrite(SessionFile,SpeciesTreeRooted, SizeOf(Boolean));
    BlockWrite(SessionFile,UsedSpeciesTree, SizeOf(Boolean));
    BlockWrite(SessionFile,NumTreeConfigs, SizeOf(Integer));
    BlockWrite(SessionFile,NumGeneDups, SizeOf(Integer));
    BlockWrite(SessionFile,NumSpeciations, SizeOf(Integer));
    BlockWrite(SessionFile,NumSpecies, SizeOf(Integer));
    BlockWrite(SessionFile,RootIndex, SizeOf(Integer));
  except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

end.

