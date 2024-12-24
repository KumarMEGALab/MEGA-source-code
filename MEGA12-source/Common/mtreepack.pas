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

unit mtreepack;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

Uses
  MegaConsts, Classes,
  MDistPack;

{$M+}

const
  MaxTreePackIndex = 20;

type
  TTreeType = (ttNone,
    ttNJ, ttUPGMA, ttME, ttBioNJ,
    ttMP, ttML, ttMCL,
    ttInferTree,        // combined with ttNJ, UPGMA, ME, MP, and ML to build trees
    ttUseInitialTree,   // directive that an initial tree should be used, it could be NJ, ME, or UserTree
    ttUserTree,         // combine this with UseInitialTreee
    //ttUseTreeFromFile,  // The User tree specified (if it is) comes from a file.
    ttBranchBound, ttMPMinMini, ttRandomAddition,  // search types for MP
    ttRzhetskyNei, ttPurdomKumar, ttLeadingTaxon,  // search types for ME
    ttFixedSearchFactor, ttPropSearchFactor,       // search parameters for MP and ME
    ttDtSearch,  ttCNI, ttNNI, ttSPR, ttTBR, ttSPRFast, ttSPRExtensive,             // search parameters for branch swapping
    ttOptimizePreBLens,
    ttUseOLS,                                      // directs the use of OLS for calculations
    ttWtParsim,
    ttTvParsim, ttNucParsim, ttAminoParsim,        // needed to decide the type of parsimony
    ttBootstrap, ttCPTest, ttFourCluster,          // type of test
    ttClock,                                       // assume clock
    // instruction to linearize a tree using ML or OLS
    ttClockTest,                                   // Instruction to test Clock using ML or OLS
    ttModelTest,                                   // Use in conjuction with ML to test model
    ttBLens,                                       // indicates need for computation of BLens, works with OLS or ML
    ttAncState, ttPattern, ttTsTvBias,             //
    ttCodonOmega, ttSiteRates, ttGamma,
    ttEstimateVar,                                  // Directive that we need variance, use with ttBootstrap as needed
    ttPredictLivingSeq,
    ttKeepUserTreeBLens,                            // Indicates that we should keep the user tree branch lengths rather than computing an inital length.
    tt46SpeciesTree,                                 // obtained from the UCSC Genome Browser database update 2011
    ttTumorTree,
    ttEpML,
    ttModelTamer
  );

  { TTreePack }

  TTreePack = class
  private
    FTreeType: array[0..MaxTreePackIndex] of TTreeType;
    FBootReps: Integer;
    FRandSeed: Integer;
    FRandomAddReps: Integer;
    IsLocked : Boolean;
    FMaxTrees: Integer;
    FSearchFactor: Double;
    FSearchLevel: integer;
    FTvWeight  : Integer;
    FTreeFile  : AnsiString;
    Where : Integer;
    FGlobalClock: Boolean;
    FGlobalClockLevel: Integer;
    FMLInitialTreesMethod: Integer;

    function GetMLInitialTreeMethodStr: String;
    function GetNameAcronym: AnsiString;
    function GetRandSeed: Integer;
    function GetTreeMethodName: AnsiString;
    function GetTreeMethodCitation: AnsiString;
    function GetMyPegTreeMethodCitation: AnsiString;
    function GetSearchName: AnsiString;
    function GetHeuristicMethod: AnsiString;
    function GetFullName: AnsiString;
    function GetTestAttributes: AnsiString;
    function GetMPInitialTreesMethod : AnsiString;
    function GetMPSearchName : AnsiString;
    function GetMLInitialTreesMethod: Integer;

    function GetTestType : TTreeType;
    function GetIsValid: Boolean;
    function GetIsInfered: Boolean;
    function GetIsAncesteral : Boolean;
    function GetIsDoubleOptimized : Boolean;
    function GetGlobalClock: Boolean;
    function GetGlobalClockLevel: Integer;
    function GetKeepUserTreeBLens: Boolean;
    procedure SetMLInitialTreesMethod(initialMethod: Integer);

  public
    constructor Create;

    procedure Assign(Source: TTreePack);
    procedure CopyFrom(Value: TTreePack);
    procedure Clear;
    procedure Unwind; // go back up to index 0;
    function  AddType(Value: TTreeType): Boolean;
    procedure Lock;                    // it closes DistPack Input channel
    procedure UnLock;                    // it reopens DistPack Input channel

    function  GetNext: TTreeType;  // returns next type of info
    function  DoesContain(Value: TTreeType): Boolean; // if contained

    function  ReadFromFile(var data: File):boolean;
    procedure WriteToFile(var data: File);

    procedure ConstructPack(FOperation: TDistTreeDlgOption; const AStringList: TStrings;IsCoding: Boolean);
    procedure ReplaceType(FromType, ToType: TTreeType);
    function StateToStringList(Comment: String=''): TStringList;
    function DebugTreeTypes: String;
  published
    property IsValid: Boolean read GetIsValid;
    property IsInfered: Boolean read GetIsInfered;
    property IsAncesteral: Boolean read GetIsAncesteral;
    property IsDoubleOptimized: Boolean read GetIsDoubleOptimized;

    property MaxTrees: Integer    read FMaxTrees write FMaxTrees;
    property SearchFactor: Double read FSearchFactor write FSearchFactor;
    property SearchLevel: integer read FSearchLevel write FSearchLevel;
    property RandomAddReps: Integer read FRandomAddReps write FRandomAddReps;
    property BootReps: Integer read FBootReps write FBootReps;
    property RandSeed: Integer read GetRandSeed write FRandSeed;

    property TvWeight:  Integer read FTvWeight  write FTvWeight;
    property TreeFile: AnsiString read FTreeFile write FTreeFile;

    property NameAcronym: AnsiString    read GetNameAcronym;
    property TreeMethodName: AnsiString read GetTreeMethodName;
    property TreeMethodCitation: AnsiString read GetTreeMethodCitation;
    property MyPegTreeMethodCitation: AnsiString read GetMyPegTreeMethodCitation;
    property SearchName: AnsiString     read GetSearchName;
    property HeuristicMethod: AnsiString read GetHeuristicMethod;
    property FullName: AnsiString       read GetFullName;
    property TestAttributes: AnsiString read GetTestAttributes;
    property TestType : TTreeType read GetTestType;
    property MPInitialTreesMethod : AnsiString read GetMPInitialTreesMethod;
    property MPSearchName : AnsiString read GetMPSearchName;
    property MLInitialTreesMethod : Integer read GetMLInitialTreesMethod write SetMLInitialTreesMethod;

    property MLInitialTreeMethodStr: String read GetMLInitialTreeMethodStr;
    property GlobalClock: Boolean read GetGlobalClock;
    property GlobalClockLevel: Integer read GetGlobalClockLevel;

    property KeepUserTreeBLens: Boolean read GetKeepUserTreeBLens;
end;

implementation

uses
  Sysutils, MegaUtils, MegaAnalysisPrefStrings, TypInfo, mstringbuilder;

constructor TTreePack.Create;
begin
  Clear;
end;

function TTreePack.GetIsValid: Boolean;
begin
  Result := (FTreeType[0] <> ttNone);
end;

procedure TTreePack.Clear;
var
 i: Integer;
begin
  IsLocked := False;
  where := 0;
  for i:= 0 to MaxTreePackIndex do
    FTreeType[i] := ttNone;
  FBootReps := -1;
  FRandSeed := -1;
  FRandomAddReps := -1;
  FMaxTrees := 1;
  FSearchFactor := -1;
  FSearchLevel := -1;
  FTvWeight  := -1;
end;

procedure TTreePack.CopyFrom(Value: TTreePack);
begin
  // do something
end;

procedure TTreePack.Unwind;
begin
  if not IsLocked then
    where := 0;
end;

function TTreePack.AddType(Value: TTreeType): Boolean;
begin
  Result := False;
  if IsLocked then
    Exit;
  FTreeType[where] := Value;
  Inc(where);
  Result := True;
end;

procedure TTreePack.Lock;   // closes DistPack Input channel
begin
  IsLocked := True;
end;

function TTreePack.GetMyPegTreeMethodCitation: AnsiString;
begin
  Result := 'Fujita_et_al_2011';
end;

procedure TTreePack.UnLock; // opens DistPack Input channel
begin
  IsLocked := False;
end;

function TTreePack.GetNext: TTreeType;
begin
  Result := ttNone;
  if not IsLocked then
    Exit;
  Result := FTreeType[where];
  if Result <> ttNone then
    Inc(where);
end;

function TTreePack.DoesContain(Value: TTreeType): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i:=0 to MaxTreePackIndex do
   if FTreeType[i]= Value then
     Exit;
  Result := False;
end;

procedure TTreePack.ReplaceType(FromType, ToType: TTreeType);
 var
  i: Integer;
begin
  for i:=0 to MaxTreePackIndex do
   if FTreeType[i]= FromType then
     FTreeType[i] := ToType;
end;

function TTreePack.GetNameAcronym: AnsiString;
begin
  case FTreeType[0] of
    ttNJ:          Result := 'NJ';
    ttUPGMA:       Result := 'UPGMA';
    ttME:          Result := 'ME';
    ttMP:          Result := 'MP';
    ttML:          Result := 'ML';
    ttFourCluster: Result := '4-C';
  else
    Result := 'None'
  end;
end;

function TTreePack.GetRandSeed: Integer;
begin
  Result := FRandSeed;
end;

function TTreePack.GetMLInitialTreeMethodStr: String;
begin
  case FMLInitialTreesMethod of
     NJInitTreeMethod: Result := 'nj';
     BioNJInitTreeMethod: Result := 'bionj';
     MEInitTreeMethod: Result := 'me';
     MPInitTreeMethod: Result := 'mp';
     MultipleMPTreesMethod: Result := 'mp';
     DefaultInitTreeMethod: Result := 'default';
     UserProvidedInitTree: Result := 'userTree';
     else
       raise Exception.Create(Format('invalid initial trees method = %d', [FMLInitialTreesMethod]));
  end;
end;

function TTreePack.GetTreeMethodName: AnsiString;
begin
  case FTreeType[0] of
    ttNJ:          Result := 'Neighbor-Joining';
    ttUPGMA:       Result := 'UPGMA';
    ttME:          Result := 'Minimum Evolution';
    ttMP:          Result := 'Maximum Parsimony';
    ttML:          Result := 'Maximum Likelihood';
    ttFourCluster: Result := 'Four-cluster analysis';
    ttUseOLS:      Result := 'Least Squares';
  else
    Result := 'None'
  end;
end;

function TTreePack.GetTreeMethodCitation: AnsiString;
begin
  case FTreeType[0] of
    ttNJ:          Result := 'Saitou_and_Nei_1987';
    ttUPGMA:       Result := 'Sneath_and_Sokal_1973';
    ttME:          Result := 'Rzhetsky_and_Nei_1992';
    ttMP:          Result := 'Eck_and_Dayhoff_1966';
    ttML:          Result := 'Nei_and_Kumar_2000';
    ttUseOLS:      Result :=  'Rzhetsky_and_Nei_1993';
    //ttFourCluster: Result := ''; { TODO : When adding four cluster be sure to fill this out }
  else
    Result := 'None'
  end;
end;

function TTreePack.GetSearchName: AnsiString;
begin
  Result := EmptyStr;
  case FTreeType[0] of
    ttME, ttMP:
    ;
  else
    Exit;
  end;

  case FTreeType[1] of
    ttBranchBound:  Result := 'Branch-&-Bound';
    ttMPMinMini:  Result := 'Min-Mini';
    ttDtSearch: Result := 'CNI';
  end;

  case FTreeType[2] of
    ttMPMinMini:  Result := 'Min-Mini';
    ttRzhetskyNei:  Result := 'CNI';
    ttLeadingTaxon: Result := 'Leading Taxon algorithm';
  end;
end;

function TTreePack.GetFullName: AnsiString;
begin
  if GetSearchName <> EmptyStr then
    Result := GetTreeMethodName+' ('+GetSearchName+')'
  else
    Result := GetTreeMethodName;
end;

function TTreePack.GetTestAttributes: AnsiString;
begin
  Result := EmptyStr;

  if DoesContain(ttBootstrap) then
    Result := 'Felsenstein''s Bootstrap'
  else if DoesContain(ttCPTest) then
    Result := 'CP Test'
  else if DoesContain(ttFourCluster) then
    Result := '4-cluster analysis';
  Result := Result + ' ('+IntToStr(FBootReps)+' replications)';
end;

function TTreePack.GetTestType: TTreeType;
begin
  Result := ttNone;
  if DoesContain(ttBootstrap) then
    Result := ttBootstrap
  else if DoesContain(ttCPTest) then
    Result := ttCPTest
  else if DoesContain(ttFourCluster) then
    Result := ttFourCluster;
end;

function TTreePack.GetMPInitialTreesMethod: AnsiString;
begin
  Result := EmptyStr;
  case FTreeType[2] of
    ttMPMinMini:  Result := 'Min-Mini';
    ttRandomAddition: Result := 'Random Addition';
    else
      Result := '???';
  end;
end;

function TTreePack.GetMPSearchName: AnsiString;
var
  i : integer;
begin
  for i :=0 to High(FTreeType) do
  case FTreeType[i] of
    ttBranchBound: begin Result := 'Branch-&-Bound'; exit; end;
    ttMPMinMini:  begin Result := 'Min-Mini';  exit; end;
    ttDtSearch: begin Result := 'CNI'; exit; end;
    ttSPR: begin Result := 'SPR'; exit; end;
    ttTBR: begin Result := 'TBR'; exit; end;
  end;
  Result := EmptyStr;
end;

procedure TTreePack.ConstructPack(FOperation: TDistTreeDlgOption; const AStringList: TStrings; IsCoding: Boolean);
var
   MyValue: AnsiString;

   function HasRow(MyRowName: AnsiString): Boolean;
   begin
     Result := AStringList.IndexOfName(MyRowName) >= 0;
   end;

   function ValueOfRow(MyRowName: AnsiString): AnsiString;
   begin
     Result := AStringList.Values[MyRowName];
   end;

begin
  Clear;

  // add the major methods
  case FOperation of
      dtdoSiteCoverage: AddType(ttUserTree);
      dtdoNJTree:                   AddType(ttNJ);
      dtdoUPGMATree:                AddType(ttUPGMA);
      dtdoMETree:                   AddType(ttME);

      dtdoOLSInteriorBranchTest,
      dtdoPhyloQ,
      dtdoRelTimeLS, dtdoRtdtLS,
      dtdoOLSComputeUserTreeBLens:  AddType(ttUseOLS);

      dtdoMCLComputePattern,
      dtdoMCLTsTvBias            :  AddType(ttMCL);

      dtdoMPTree,
      dtdoMPInferAncSeq,
      dtdoMPInferAncSeqMyPeg,
      dtdoMPComputeUserTreeBLens:   AddType(ttMP);

      dtdoMLTree,
      dtdoMLIQTree,
      dtdoMLClockTest,
      dtdoMLClockTestLocal,
      dtdoRelTimeML, dtdoRtdtML,
      dtdoCorrTestML,
      dtdoMLCodonOmega,
      dtdoMLModelTest, dtdoMLModelTamer, dtdoMLModelTestIQTree,
      dtdoMLComputePattern,
      dtdoMLTsTvBias,
      dtdoMLGammRates,
      dtdoMLInferAncSeq,
      dtdoMLInferAncSeqMyPeg,
      dtdoMLPredictLivingSeq, dtdoEpML,
      dtdoMLComputeUserTreeBLens, dtdoLbsTiming, dtdoLbsAnalyzeTree, dtdoLbsInference,
      dtdoMLInferSiteBySiteRates, dtdoBEAM:  AddType(ttML);
  else
    Exit;
  end;

  case FOperation of
      dtdoNJTree,
      dtdoUPGMATree,
      dtdoMETree,
      dtdoMPTree,
      dtdoMLIQTree,
      dtdoMLTree,
      dtdoLbsInference:              AddType(ttInferTree);

      dtdoOLSInteriorBranchTest:     AddType(ttCPTest);

      dtdoOLSComputeUserTreeBLens,
      dtdoPhyloQ,
      dtdoMPComputeUserTreeBLens,
      dtdoMLComputeUserTreeBLens:    AddType(ttBLens);

      dtdoMCLComputePattern,
      dtdoMLComputePattern:          AddType(ttPattern);

      dtdoMCLTsTvBias,
      dtdoMLTsTvBias:                 AddType(ttTsTvBias);

      dtdoMPInferAncSeq,
      dtdoMPInferAncSeqMyPeg,
      dtdoMLInferAncSeqMyPeg,
      dtdoMLInferAncSeq:             AddType(ttAncState);

      dtdoMPPredictLivingSeq,
      dtdoMLPredictLivingSeq:        begin
                                       AddType(ttAncState);
                                       AddType(ttPredictLivingSeq);
                                     end;
      dtdoLbsAnalyzeTree, dtdoLbsTiming:
        AddType(ttUserTree);
      dtdoEpML:
          begin
            AddType(ttAncState);
            AddType(ttPredictLivingSeq);
            AddType(ttUserTree);
          end;
      dtdoMLGammRates:               AddType(ttGamma);

      dtdoMLClockTest,
      dtdoRelTimeML,
      dtdoRtdtML,
      dtdoCorrTestML,
      dtdoMLClockTestLocal:        AddType(ttClockTest);

      dtdoMLCodonOmega:              AddType(ttCodonOmega);
      dtdoMLModelTest, dtdoMLModelTestIQTree:  AddType(ttModelTest);
      dtdoMLModelTamer: AddType(ttModelTamer);
      dtdoMLInferSiteBySiteRates:    AddType(ttSiteRates);
      dtdoBEAM:
        begin
          AddType(ttInferTree);
          AddType(ttML);
          AddType(ttGamma);
          AddType(ttAncState);
          AddType(ttPredictLivingSeq);
        end;
  end;

  case FOperation of
     dtdoMLInferAncSeqMyPeg, dtdoMPInferAncSeqMyPeg:
         AddType(ttUserTree); // this is here because we won't actually read in a tree file since we already have it
  end;

  //Setup the starting, initial, user trees
  MyValue := EmptyStr;
  if HasRow(opsTreeToUse2)      then begin  AddType(ttUseInitialTree);
                                            MyValue := ValueOfRow(opsTreeToUse2);      end;
  if HasRow(opsMEInitialTrees2) then begin  AddType(ttUseInitialTree);
                                            MyValue := ValueOfRow(opsMEInitialTrees2); end;
  if HasRow(opsMLInitialTrees2) then begin  AddType(ttUseInitialTree);
                                            MyValue := ValueOfRow(opsMLInitialTrees2); end;

  if HasRow(opsKeepUserTreeBLens2) then
  begin
    if ValueOfRow(opsKeepUserTreeBLens2) = YesStr then
      AddType(ttKeepUserTreeBLens);
  end;

  if MyValue = ObtainInitialByMEStr then AddType(ttME);
  if (MyValue = ObtainInitialByNJStr) or
     (MyValue = ObtainNJStr) or
     (MyValue = NJTreeStr)            then AddType(ttNJ);
  if (MyValue = UserSpecifyFromFile) or
     (MyValue = UserSpecifyManager)  then AddType(ttUserTree);

  if MyValue = InitialTreeByNJStr then AddType(ttNJ);
  if MyValue = InitialTreeByBioNJStr then AddType(ttBioNJ);

  if HasRow(opsPickUserTree2)       then FTreeFile := ValueOfRow(opsPickUserTree2); // adding rows back in, for the case where users specify the file.
  if HasRow(opsPickStartTree2)      then FTreeFile := ValueOfRow(opsPickStartTree2);

  if HasRow(opsMESearchMethod2) then  MyValue := ValueOfRow(opsMESearchMethod2);
  if HasRow(opsMPSearchMethod2) then  MyValue := ValueOfRow(opsMPSearchMethod2);
  if HasRow(opsMLSearchMethod2) then  MyValue := ValueOfRow(opsMLSearchMethod2);

  if MyValue = NNIStr then
  begin
    AddType(ttNNI);
    AddType(ttDtSearch);
  end;

  if MyValue = SPRFastStr then
  begin
    AddType(ttSPRFast);
    AddType(ttDtSearch);
  end;

  if MyValue = SPRExtensiveStr then
  begin
    AddType(ttSPRExtensive);
    AddType(ttDtSearch);
  end;

  if MyValue = SPRStr then
  begin
    AddType(ttSPR);
    AddType(ttDtSearch);
  end;

  if MyValue = TBRStr then
  begin
    AddType(ttTBR);
    AddType(ttDtSearch);
  end;

  if MyValue = MinMiniStr then              AddType(ttMPMinMini);
  if MyValue = MaxMiniStr then              AddType(ttBranchBound);

  if MyValue = CNIStr then
  begin
    AddType(ttCNI);
    AddType(ttDtSearch);
  end;

  if MyValue = RandomAdditionTreesStr then
  begin
    AddType(ttDtSearch);
    AddType(ttRandomAddition);
  end;

  if HasRow(opsMESearchLevelPanel2) then
  begin
    SearchFactor := StrToIntWithInvalid(ValueofRow(opsMESearchLevelPanel2)); //MECNISearchSE.Value
    MaxTrees     := 1;
  end;

  if HasRow(opsMPSearchLevelPanel2) then
  begin
    if (MyValue = MinMiniStr) then
      SearchFactor := StrToIntWithInvalid(ValueOfRow(opsMPSearchLevelPanel2))
    else if (MyValue = SPRStr) or (MyValue = TBRStr) or (MyValue = RandomAdditionTreesStr) then
    begin
      RandomAddReps  := StrToIntWithInvalid(ValueOfRow(opsMPRandomAddTreesPanel2));
      SearchLevel := StrToIntWithInvalid(ValueOfRow(opsMPSearchLevelPanel2));
    end;
    MaxTrees := StrToIntWithInvalid(ValueOfRow(opsMaxTrees2));
  end;

  // Bootstrap and Interior branch test
  if HasRow(opsTestPhylo2)      then MyValue := ValueOfRow(opsTestPhylo2);
  if MyValue = IntBranchTestStr then AddType(ttCPTest);
  if (MyValue = BootTestStr) or (MyValue = opsSubsampleUpsample2) or (MyValue = BootTestSlowStr) or (MyValue = BootTestAdaptiveStr) or (MyValue = BootTestAdaptiveStrFast) then AddType(ttBootstrap);

  if HasRow(opsBootReps2) then
  begin
    BootReps  := StrToIntWithInvalid(ValueOfRow(opsBootReps2));
    RandSeed  := Random(1000);
  end;

  if HasRow(opsAssumeMolClock2) and (ValueOfRow(opsAssumeMolClock2) = YesStr) then
    AddType(ttClock);
  if HasRow(opsMLBLenOptimize2) and (ValueOfRow(opsMLBLenOptimize2) = YesSlowStr) then
    AddType(ttOptimizePreBLens);

  //== for parsimony
  if (DoesContain(ttMP) or DoesContain(ttMCL)) and HasRow(opsNucSynAminoType2)then
  begin
    MyValue := ValueOfRow(opsNucSynAminoType2);
    if MyValue = NucStr   then AddType(ttNucParsim);
    if MyValue = AminoStr then AddType(ttAminoParsim);
  end;
end;

function TTreePack.ReadFromFile(var data: File):boolean;
var
  i: Integer;
  version: integer = -1;
  maxIndex: Integer = 20;
begin
  result := true;
  try
    BlockRead(data, version, SizeOf(Integer));
    if version < 3 then
      maxIndex := 10;
    for i := 0 to maxIndex do
      BlockRead(data, FTreeType[i], SizeOf(TTreeType));

    BlockRead(data, FBootReps, SizeOf(FBootReps));
    BlockRead(data, FRandSeed, SizeOf(FRandSeed));
    BlockRead(data, FRandomAddReps, SizeOf(FRandomAddReps));
    BlockRead(data, IsLocked, SizeOf(IsLocked));
    BlockRead(data, FMaxTrees, SizeOf(FMaxTrees));
    BlockRead(data, FSearchFactor, SizeOf(FSearchFactor));
    BlockRead(data, FSearchLevel, SizeOf(FSearchLevel));
    BlockRead(data, FTvWeight, SizeOf(FTvWeight));
    BlockRead(data, Where, SizeOf(Where));
    if version > 1 then
      BlockRead(data, FMLInitialTreesMethod, SizeOf(FMLInitialTreesMethod));
  except
    on E:Exception do
    begin
      Result := False;
    end;
  end;
end;

procedure TTreePack.WriteToFile(var data: File);
var
  i: integer;
begin
  i := 3;  //version
  BlockWrite(data, i, SizeOf(i));

  for i := 0 to MaxTreePackIndex do
    BlockWrite(data, FTreeType[i], SizeOf(TTreeType));

  BlockWrite(data, FBootReps, SizeOf(FBootReps));
  BlockWrite(data, FRandSeed, SizeOf(FRandSeed));
  BlockWrite(data, FRandomAddReps, SizeOf(FRandomAddReps));
  BlockWrite(data, IsLocked, SizeOf(IsLocked));
  BlockWrite(data, FMaxTrees, SizeOf(FMaxTrees));
  BlockWrite(data, FSearchFactor, SizeOf(FSearchFactor));
  BlockWrite(data, FSearchLevel, SizeOf(FSearchLevel));
  BlockWrite(data, FTvWeight, SizeOf(FTvWeight));
  BlockWrite(data, Where, SizeOf(Where));
  BlockWrite(data, FMLInitialTreesMethod, SizeOf(FMLInitialTreesMethod));
end;

procedure TTreePack.Assign(Source: TTreePack);
var
  i: integer;
begin
  for i := 0 to MaxTreePackIndex do
    FTreeType[i] := Source.FTreeType[i];
  FBootReps      := Source.FBootReps;
  FRandSeed      := Source.FRandSeed;
  FRandomAddReps := Source.FRandomAddReps;
  IsLocked       := Source.IsLocked;
  FMaxTrees      := Source.FMaxTrees;
  FSearchFactor  := Source.FSearchFactor;
  FSearchLevel   := Source.FSearchLevel;
  FTvWeight      := Source.FTvWeight;
  FTreeFile      := Source.FTreeFile;
  Where          := Source.Where;
  FGlobalClock   := Source.FGlobalClock;
  FGlobalClockLevel := Source.FGlobalClockLevel;
end;

function TTreePack.GetHeuristicMethod: AnsiString;
begin
  Result := EmptyStr;
  if doesContain(ttNNI) then
    result := NNIstr
  else if doesContain(ttCNI) then
    result := CNIStr
  else if doesContain(ttTBR) then
    result := TBRStr
  else if doesContain(ttSPR) then
    result := SPRStr;
end;

function TTreePack.GetIsInfered: Boolean;
begin
  Result := doesContain(ttInferTree);
end;

/// <summary>Set the value of FMLInitialTreesMethod</summary>
/// <note>Valid values are found in MegaConsts</note>
procedure TTreePack.SetMLInitialTreesMethod(initialMethod: Integer);
begin
 if (initialMethod = NJInitTreeMethod) or
    (initialMethod = BioNJInitTreeMethod) or
    (initialMethod = DefaultInitTreeMethod) or
    (initialMethod = UserProvidedInitTree) or
    (initialMethod = MultipleMPTreesMethod) or
    (initialMethod = MPInitTreeMethod) then
    FMLInitialTreesMethod := initialMethod
  else
    Raise Exception.Create('Invalid value given for TTreePack.FMLInitialTreesMethod.');
end;

/// <summary>Get the value of FMLInitialTreesMethod. Denotes the method used
/// for auto-generating the initial tree in Maximum Likelihood phylogeny
/// inference</summary>
function TTreePack.GetMLInitialTreesMethod: Integer;
begin
  result := FMLInitialTreesMethod;
end;

function TTreePack.GetIsAncesteral: Boolean;
begin
  result := doesContain(ttAncState);
end;

function TTreePack.GetIsDoubleOptimized: Boolean;
begin
  result := doesContain(ttOptimizePreBLens);
end;

function TTreePack.GetGlobalClock: Boolean;
begin
  result := FGlobalClock;
end;

function TTreePack.GetKeepUserTreeBLens: Boolean;
begin
  result := DoesContain(ttKeepUserTreeBLens);
end;

function TTreePack.GetGlobalClockLevel: Integer;
begin
  result := FGlobalClockLevel;
end;

function TTreePack.StateToStringList(Comment: String=''): TStringList;
var
  MyStringList: TStringList;
begin
  MyStringList := TStringList.Create;
  if Trim(Comment) <> EmptyStr then
  MyStringList.Add(Comment);
  MyStringList.Add('TTreePack=MyName');
  MyStringList.Add('IsValid=' + BoolToStr(IsValid));
  MyStringList.Add('IsInfered=' + BoolToStr(IsInfered));
  MyStringList.Add('IsAncestral=' + BoolToStr(IsAncesteral));
  MyStringList.Add('IsDoubleOptimized=' + BoolToStr(IsDoubleOptimized));
  MyStringList.Add('KeepUserTreeBLens=' + BoolToStr(KeepUserTreeBLens));
  MyStringList.Add('GlobalClock=' + BoolToStr(GlobalClock));
  MyStringList.Add('MaxTrees=' + IntToStr(MaxTrees));
  MyStringList.Add('SearchLevel=' + IntToStr(SearchLevel));
  MyStringList.Add('RandomAddReps=' + IntToStr(RandomAddReps));
  MyStringList.Add('BootReps=' + IntToStr(BootReps));
  MyStringList.Add('RandSeed=' + IntToStr(RandSeed));
  MyStringList.Add('MLInitialTreesMethod=' + IntToStr(MLInitialTreesMethod));
  MyStringList.Add('TvWeight=' + IntToStr(TvWeight));
  MyStringList.Add('GlobalClockLevel=' + IntToStr(GlobalClockLevel));
  MyStringList.Add('TreeFile=' + TreeFile);
  MyStringList.Add('NameAcronym=' + NameAcronym);
  MyStringList.Add('TreeMethodName=' + TreeMethodName);
  MyStringList.Add('TreeMethodCitation=' + TreeMethodCitation);
  MyStringList.Add('SearchName=' + SearchName);
  MyStringList.Add('HeuristicMethod=' + HeuristicMethod);
  MyStringList.Add('FullName=' + FullName);
  MyStringList.Add('TestAttributes=' + TestAttributes);
  MyStringList.Add('MPInitialTreesMethod=' + MPInitialTreesMethod);
  MyStringList.Add('MPSearchName=' + MPSearchName);
  MyStringList.Add('TestType=' + GetEnumName(TypeInfo(TTreeType), integer(TestType)));
  MyStringList.Add('SearchFactor=' + FloatToStrF(SearchFactor, ffFixed, 4, 4));

  Result := MyStringList;

end;

function TTreePack.DebugTreeTypes: String;
var
  i: Integer = 0;
  builder: TMegaStringBuilder = nil;
  t: TTreeType;
begin
  try
    builder := TMegaStringBuilder.Create;
    for i := 0 to MaxTreePackIndex do
    begin
      t := FTreeType[i];
      builder.Add(GetEnumName(TypeInfo(TTreeType), Integer(t)));
      if i < MaxTreePackIndex then
        builder.Add(', ');
    end;
    Result := builder.GenerateString;
  finally
    if Assigned(builder) then
      builder.Free;
  end;
end;

end.

