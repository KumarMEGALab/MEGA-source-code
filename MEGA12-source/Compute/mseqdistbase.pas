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

unit MSeqDistBase;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntF, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, MegaConsts, MDistPack, MRuntimeProgressDlg, DateUtils;

//-- FPairD is used for keeping pairwise distances
//- FDx, FGpDx, used for computing averages within a rep of d
//- FUserD, FUserGpD, keep averages over all reps of d
//- FUserV, FUserGpV, keep average all reps of d*d

const
  PROGRESS_UPDATE_INTERVAL = 150; { min number of milliseconds between progress updates}

type

  { EDistComputationError }

  EDistComputationError = class(Exception)
    private
      FErrorId: Integer;
    public
      constructor Create(aMsg: String; aErrorId: Integer);
      property ErrorId: Integer read FErrorId;
  end;

  { TSeqDistBase }

  TSeqDistBase = class abstract(TObject)
  protected
    FSequences  : TList;
    FNoOfSeqs   : Integer;
    FDistPack   : TDistPack;    // distance
    FNoOfGps    : Integer;      // number of groups in the data
    FGpId       : ArrayOfInteger;  // group designation for each Sequence
    FQuickExit  : Boolean;      // immediately return on an error

    FNoOfSites   : Integer;          // The sequence Length in nucs/codons/amino acids
    FFreqTable   : ArrayOfInteger; // Frequency of occurances of each site/codon/amino acid
    FFreqTableLen: Integer;          // length of the freqTable vector
    IsInternalFT:  Boolean;          // Is the frequency table internal

	 // for writing results; memory provided by the caller
    FUserD,       FUserV:       PDistanceMatrix; // For pairwise distances
    FUserGpD,     FUserGpV:     PDistanceMatrix; // For pairwise group avg. distances
    FUserMeanGpD, FUserMeanGpV: ArrayOfDouble;  // for avg. within gp D and its var
    FUserMeanD,   FUserMeanV:   PDouble;         // Overall mean
    FUserDiversity, FUserDiversityV: PDouble;    // for diversity computations

  protected
    ARP             : TRuntimeProgress;
    FCaption        : TEdit;        // hints etc.
    FStopBtn        : TSpeedButton; // to detect stop

    PercentPerAdvance : Double; // Percent adanvce upto now
    CurAdvance        : Double; // Cur Adavance found
    totalPercent      : Integer; // Current total percentage
    lastUpdateTime    : TDateTime;
    FCancelled: Boolean;
    //Calculation error
    FErrorI         : Integer;
    FErrorJ         : Integer;

    BootReps:        Integer;    // number of runs for boostrap variance
    ComputationType: TDistType;  // Pairwise; PairwiseGp; etc.
    DistModel:       TDistType;  // method: NeiGojobori; LWL; Tamura-Nei; etc.
    DistCorrection:  TDistType;  // Jukes-Cantor/K2 for NAG; Li93 or Kumar for LWL etc.
    DistComponent:   TDistType;  // ts/tv/bothTsTv/syn/nonSyn/etc.
    VarType:         TDistType;  // none, bootstrap, analytical
    HasGamma:        Boolean;    // updated with FGammapara
    IsHetero:        Boolean;    //  Tamura-Kumar (2002) modification     @KT
    GammaPara:       Double;     // Gamma parameter & inverse

    IterateLen:   Integer;  // no of sites to iterate on
    GammaInv:     Double;   // Inverse of gamma parameter when applicable

    // for simultaneous estimation
    k1, k2, k, pA, pG, pC, pT, pR, pY: Double;
    Ak1, Ak2, ApA, ApT, ApC, ApG: double;      // Averages for parameters
    Vk1, Vk2, VpA, VpT, VpC, VpG: double;      // Variances for parameters

  protected   // This is the information here
    Dist1, Var1:  Double;          // estimates of pairwise distance
    GpSz:       ArrayOfInteger;  // Allocated internally; number of Seqs in each group
    PairD:      PDistanceMatrix;   // Keeps all useful distances for sequence pairs

    // Estimtes for the current replication or computation
    CurD       : PDistanceMatrix;
    CurGpD     : PDistanceMatrix;
    CurMeanGpD : ArrayOfDouble;
    CurMeanD   : Double;
    NPairs     : array of array of Integer; // keeps number of valid reps

    // No of valid bootstrap replications
    Drep       : PDistanceMatrix;
    GpDrep     : PDistanceMatrix;
    MeanGpDrep : ArrayOfDouble;
    MeanDrep   : Double;   // used also for Diversity as that is a single number
    // To keep running x and x*x averages over all reps;
    // They are just proxy pointers
    Dx,         Dxx:       PDistanceMatrix; // For pairwise distances
    GpDx,       GpDxx:     PDistanceMatrix; // For pairwise group avg. distances
    MeanGpDx,   MeanGpDxx: ArrayOfDouble;  // for avg. within gp D and its var
    MeanDx,     MeanDxx:   PDouble;         // Overall mean; and used for diversity as well

    IDV        : Double; //short for invalid distance

    FShowInProgressBar: Boolean; // Whether progress is shown in the progress bar or as text of the ARP.

  protected
    function CheckAbort: Boolean;
    procedure CheckBasicInfo;  // raises an exception if error
    procedure CheckFreqInfo;   // -- do --
    procedure CheckGroupInfo;  // -- do --
    procedure CheckMemory;     // -- do --

    function  IsUsefulComparison(i,j: Integer): Boolean;
    function  DeltaVar(const S: array of Double): Double;

    procedure AllocateMatrices;
    procedure InitializeUserD;       // sets everything to 0 in UserD matrices
    procedure InitializeUserV;       // sets everything to 0 in UserV matrices
    procedure InitializeDreps;       // Sets everything to 0 in Drep  matrices
    procedure FillDxMatrixFromPairD;
    procedure CopyCurDToProxyD(IsBootRep: Boolean);

    procedure ComputeFastExclusivePairwiseDistances; // called strict to avoid other costs
    function  ComputeDistances: Boolean; virtual;

    procedure ComputeFastExclusivePairwiseDistancesSE; // called strict to avoid other costs     KT DO IT NOW
    function  ComputeDistancesSE: Boolean; virtual;

    procedure SummarizeBootstrapVar;  // over all replicates
    procedure DoPairwiseAnalysis(i,j: Integer); virtual; abstract;
    procedure DoSimultaneousAnalysis(D: PDistanceMatrix; IncProgress: boolean); virtual; abstract;

    procedure AddProgressIncrement;

  protected
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FProgress: Integer;
     // for Composition Distance and Disparity Index based Monte Carlo work
    Freq1:     array[0..20] of Integer;  // Avoids time spent in allocations
    Freq2:     array[0..20] of Integer;  // Avoids time spent in allocations
    procedure MonteCarloDisparityIndexTest(NDiff, NSites, NStates: LongInt; IDValue: Double; Seq1, Seq2: PAnsiChar;InvalidBase: AnsiChar);
    function  CompositionDistance(NSites, NStates: LongInt; Seq1, Seq2: PAnsiChar;InvalidBase: AnsiChar): Double;
    procedure UpdateProgress(aProgress: Integer); overload;
    procedure UpdateProgress(aProgress: Integer; aStatus: String; aInfo: String); overload;
    procedure UpdateRunStatus(aStatus: AnsiString; aInfo: AnsiString);
    procedure SetErrorStatus(errorId: Integer; errorMsg: String);
  private
    procedure SetDistPack(APack: TDistPack);
  public
    InternalErrId: Integer;
    InternalErrStr: AnsiString;
    ProgressCheckCancelFunc: TCheckCancelFunc;
    CheckAbortFunc: TCheckAbortFunc;
    RunStatusProc: TRunStatusProc;
    constructor Create;
    destructor  Destroy; override;

    procedure Assign(source: TSeqDistBase);
    procedure SetRuntimeProgress(ARPin: TRuntimeProgress);
    procedure SetProgressCaption(ProgressCaption: TEdit);
    procedure SetStopButton(StopBtn: TSpeedButton);
    property StartTime: TDateTime read FStartTime;
    property EndTime: TDateTime read FEndTime;
    property QuickExit: Boolean read FQuickExit write FQuickExit;

    property NoOfSeqs: Integer read FNoOfSeqs write  FNoOfSeqs;
    property NoOfSites: Integer read FNoOfSites write  FNoOfSites;
    property Sequences: TList read FSequences write FSequences;
    property DistPack:  TDistPack read FDistPack write SetDistPack;

    property FreqTable   : ArrayOfInteger read FFreqTable write FFreqTable;
    property FreqTableLen: Integer read FFreqTableLen write FFreqTableLen;

    property GpId:    ArrayOfInteger read FGpId write FGpId;
    property NoOfGps: Integer read FNoOfGps write FNoOfGps;

    property ErrorI: Integer read FErrorI;
    property ErrorJ: Integer read FErrorJ;

          // Pairwise between sequences
    property D: PDistanceMatrix  read FUserD write FUserD;
    property V: PDistanceMatrix read FUserV write FUserV;
          // Pairwise group Averages
    property GpD: PDistanceMatrix read FUserGpD write FUserGpD;    // Average D
    property GpV: PDistanceMatrix read FUserGpV write FUserGpV;    // Variance of Avg D.
          // for storing overall mean
    property MeanD: PDouble read FUserMeanD write FUserMeanD;
    property MeanV: PDouble read FUserMeanV  write FUserMeanV;
       // for storing avg. witin gp
    property MeanGpD:  ArrayOfDouble read FUserMeanGpD write FUserMeanGpD;
    property MeanGpV:  ArrayOfDouble read FUserMeanGpV write FUserMeanGpV;
        // for accessing MCL analysis
    property MCL_kappa1: Double read k1;
    property MCL_kappa2: Double read k2;
    property MCL_kappa1_Var: Double read Vk1;
    property MCL_kappa2_Var: Double read Vk2;
    property MCL_Freq_A: Double read pA;
    property MCL_Freq_T: Double read pT;
    property MCL_Freq_C: Double read pC;
    property MCL_Freq_G: Double read pG;

    // Progress in the visual GUI
    property showInProgressBar: Boolean read FshowInProgressBar write FshowInProgressBar;
  end;

implementation

uses
  {$IFDEF DEBUG}{$IFNDEF VISUAL_BUILD}MegaUtils_NV, MAnalysisInfo,{$ENDIF}{$ENDIF}
  MegaUtils, Math, MNucDist, ErrorMessages_HC;

{ EDistComputationError }

constructor EDistComputationError.Create(aMsg: String; aErrorId: Integer);
begin
  inherited Create(aMsg);
  FErrorId := aErrorId;
end;

//--- TSeqDistBase
constructor TSeqDistBase.Create;
begin
  InternalErrId := -1;
  FCancelled := False;
  FProgress := 0;
  ProgressCheckCancelFunc := nil;
  CheckAbortFunc := nil;
  RunStatusProc := nil;
  FSequences:= nil;
  FNoOfSeqs := 0;
  FNoOfSites:= 0;
  FDistPack := nil;

  FNoOfGps  := 0;
  FGpId     := nil;
  GpSz      := nil;

  FStopBtn := nil;
  FCaption := nil;

  FFreqTable:= nil;
  FFreqTableLen:=0;
  IsInternalFT:= False;

  FUserD        := nil;  FUserV        := nil;
  FUserMeanD    := nil;  FUserMeanV    := nil;
  FUserMeanGpD  := nil;  FUserMeanGpV  := nil;

  GammaPara  := -1;
  GammaInv   := -1;
  BootReps   := 0;
  FQuickExit := True;
  FErrorI := -1;
  FErrorJ := -1;

  PairD := nil;

  CurD       := nil;   Drep       := nil;
  CurGpD     := nil;   GpDrep     := nil;
  CurMeanGpD := nil;   MeanGpDrep := nil;

  Dx         := nil;   Dxx        := nil;
  GpDx       := nil;   GpDxx      := nil;
  MeanGpDx   := nil;   MeanGpDxx  := nil;
  MeanDx     := nil;   MeanDxx    := nil;

  IDV := InvalidDistValue;
  PercentPerAdvance:=  0;
  CurAdvance      :=  0;
  LastUpdateTime := Now;
  FShowInProgressBar := True;
end;

procedure TSeqDistBase.Assign(source: TSeqDistBase);
begin
  FSequences := source.FSequences;
  FNoOfSeqs  := source.FNoOfSeqs;
  FNoOfSites := source.FNoOfSites;
  FDistPack  := source.FDistPack;

  FNoOfGps  := source.FNoOfGps;
  FGpId     := source.FGpId;
  GpSz      := source.GpSz;

  FFreqTableLen := source.FFreqTableLen;
  setlength(FFreqTable, FFreqTableLen);
//  IsInternalFT  := source.IsInternalFT;

  BootReps        := source.BootReps;
  ComputationType := source.ComputationType;
  DistModel       := source.DistModel;
  DistCorrection  := source.DistCorrection;
  DistComponent   := source.DistComponent;
  VarType         := source.VarType;
  HasGamma        := source.HasGamma;
  IsHetero        := source.IsHetero;
  GammaPara       := source.GammaPara;
  GammaInv        := source.GammaInv;
  IterateLen      := source.IterateLen;

  k1 := source.k1; k2 := source.k2; k := source.k;
  pA := source.pA; pG := source.pG; pC := source.pC; pT := source.pT; pR := source.pR; pY := source.pY;
  Ak1 := source.Ak1; Ak2 := source.Ak2; ApA := source.ApA; ApT := source.ApT; ApC := source.ApC; ApG := source.ApG;      // Averages for parameters
  Vk1 := source.Vk1; Vk2 := source.Vk2; VpA := source.VpA; VpT := source.VpT; VpC := source.VpC; VpG := source.VpG;      // Variances for parameters

  FQuickExit := source.FQuickExit;
  FErrorI    := source.FErrorI;
  FErrorJ    := source.FErrorJ;

  IDV                := source.IDV;
  PercentPerAdvance  := source.PercentPerAdvance;
  totalPercent       := source.totalPercent;
  CurAdvance         := source.CurAdvance;
  LastUpdateTime     := source.LastUpdateTime;
  FShowInProgressBar := source.FShowInProgressBar;
end;

//---- destructor
destructor TSeqDistBase.Destroy;
begin
  ProgressCheckCancelFunc := nil;
  RunStatusProc := nil;
  FFreqTable := nil; // 2.1 as it would have been reference counted otherwise
  GpSz := nil; // fixed in 2.1
  CurMeanGpD := nil;
  MeanGpDrep := nil;
  NPairs := nil;

  if PairD <> nil      then FreeDistMatrix(PairD,FNoOfSeqs);  PairD := nil;
  if CurD <> nil       then FreeDistMatrix(CurD, FNoOfSeqs);  CurD := nil;
  if Drep <> nil       then FreeDistMatrix(Drep, FNoOfSeqs);  Drep := nil;
  if CurGpD <> nil     then FreeDistMatrix(CurGpD, FNoOfGps); CurGpD := nil;
  if GpDrep <> nil     then FreeDistMatrix(GpDrep, FNoOfGps); GpDrep := nil;
  inherited Destroy;
end;

procedure TSeqDistBase.SetDistPack(APack: TDistPack);
begin
  FDistPack := APack;

  ComputationType := APack.ComputationType;
  DistModel       := APack.DistModel;
  DistCorrection  := APack.DistCorrection;
  DistComponent   := APack.DistComponent;
  VarType         := APack.VarType;
  if VarType = gdBootstrapVar then
    BootReps := APack.BootReps;
  HasGamma        := APack.DoesContain(gdGamma);
  if HasGamma then
  begin
    GammaPara := APack.GammaParameter;
    if GammaPara = 0 then GammaPara := 5000; // Prevents div by 0;
    GammaInv := 1.0/Gammapara;
  end;
  IsHetero := APack.DoesContain(gdHetero);
end;

procedure TSeqDistBase.SetRuntimeProgress(ARPin : TRuntimeProgress);
begin
  ARP := ARPin;
  UpdateProgress(0);
end;

procedure TSeqDistBase.SetProgressCaption(ProgressCaption: TEdit);
begin
  FCaption := ProgressCaption;
  if FCaption <> nil then
    FCaption.Text := EmptyStr;
end;

procedure TSeqDistBase.SetStopButton(StopBtn: TSpeedButton);
begin
  FStopBtn := StopBtn;
  if FStopBtn <> nil then
  begin
    FStopBtn.AllowAllUp := True;
    FStopBtn.GroupIndex := High(Integer);
  end;
end;

procedure TSeqDistBase.AddProgressIncrement;
begin
  CurAdvance := CurAdvance + 1;
  if CurAdvance > PercentPerAdvance then
  begin
    if showInProgressBar then
    begin
      FProgress := Min(FProgress + Trunc(CurAdvance/PercentPerAdvance), 100);
      UpdateProgress(FProgress);
    end;
    CurAdvance := CurAdvance - PercentPerAdvance*Trunc(CurAdvance/PercentPerAdvance);
  end;
end;

// computes the Delta variance
// Format c1, P, c2, Q, xxx NSites
function TSeqDistBase.DeltaVar(const S: array of Double): Double;
var
  i: Integer;
  first, second: Double;
begin
  if (SizeOf(S) = 0) or ((High(S) mod 2) <> 0) then  // 0 - high
    SetErrorStatus(HC_Unexpected_Error, 'Delta function failed.');

  first := 0; second := 0;
  i := 0;
  while i < High(S) do
  begin
    first  := first + SQR(S[i])*S[i+1];
    second := second + S[i]*S[i+1];
    i := i+2;
  end;
  Result := (first - SQR(second))/S[High(S)];
end;

function TSeqDistBase.CheckAbort: Boolean;
begin
  Result := False;
  if Assigned(CheckAbortFunc) then
    Result := CheckAbortFunc;
  if Result then
  begin
    InternalErrId := HC_User_Stopped_Computation;
    InternalErrStr := 'User stopped the computation';
  end;
end;

procedure TSeqDistBase.CheckBasicInfo;
begin
  if (FSequences = nil) or (FNoOfSeqs < 2) or (FNoOfSites < 1) then
    SetErrorStatus(HC_Unexpected_Error, 'Invalid basic information in distance computation.');
end;

procedure TSeqDistBase.CheckFreqInfo;
begin
  CheckBasicInfo;
  if (FFreqTable <> nil) and ((FFreqTableLen>FNoOfSites) or (FFreqTableLen<1)) then
    SetErrorStatus(HC_Unexpected_Error, 'Invalid bootstrap resample/site frequency information in distance computation.');
end;

procedure TSeqDistBase.CheckGroupInfo;
var
  i, MultiTaxaGps: Integer;
begin
 if FNoOfGps <= 0 then
   Exit;

 if FGpId=nil then
    SetErrorStatus(HC_Unexpected_Error, 'Group/cluster designation missing for sequences');

  SetLength(GpSz, FNoOfGps); // 2.1

 for i:=0 to FNoOfGps-1 do
   GpSz[i] :=0;
 for i:=0 to FNoOfSeqs-1 do
   Inc(GpSz[FGpId[i]]);
 for i:=0 to FNoOfGps-1 do
   if GpSz[i] = 0 then
       SetErrorStatus(HC_Unexpected_Error, 'At least one group/cluster contains 0 sequences.');

 // check if enough
 MultiTaxaGps := 0;
 for i:=0 to FNoOfGps-1 do
   if GpSz[i] > 1 then Inc(MultiTaxaGps);

 case ComputationType of
   gdWithinGroupMean:
     if MultiTaxaGps < 1 then
       SetErrorStatus(HC_Not_Enough_Groups_Selected, 'No multi-sequence groups found.');
    gdNetGroupMean:
      if MultiTaxaGps < 2 then
       SetErrorStatus(HC_Not_Enough_Groups_Selected, 'At least two groups with two or more sequences needed.');
    gdAvgDiversityWithinSubPops,
    gdInterPopDiversity,
    gdPropOfInterPopDiversity:
      if MultiTaxaGps < 2 then
       SetErrorStatus(HC_Not_Enough_Groups_Selected, 'At least two populations with two or more sequences needed.');
 end;
end;

procedure TSeqDistBase.CheckMemory;
  procedure RaiseMemoryError;
  begin
    SetErrorStatus(HC_Unexpected_Error, 'Distance computer doesn''t have (was not provided with) enough memory.');
  end;
begin
  case ComputationType of
    gdPairwise:         if FUserD = nil       then RaiseMemoryError;
    gdBetweenGroupMean,
    gdNetGroupMean:     if FUserGpD = nil     then RaiseMemoryError;
    gdWithinGroupMean:  if FUserMeanGpD = nil then RaiseMemoryError;
    gdOverallMean:      if FUserMeanD = nil   then RaiseMemoryError;

    gdAvgDiversityWithinSubPops,
    gdAvgDiversityForEntirePop,
    gdInterPopDiversity,
    gdPropOfInterPopDiversity:  if FUserMeanD = nil   then RaiseMemoryError;
  end;
  if VarType <> gdNone then
    case ComputationType of
      gdPairwise:         if FUserV = nil       then RaiseMemoryError;
      gdBetweenGroupMean,
      gdNetGroupMean:     if FUserGpV = nil     then RaiseMemoryError;
      gdWithinGroupMean:  if FUserMeanGpV = nil then RaiseMemoryError;
      gdOverallMean:      if FUserMeanV = nil   then RaiseMemoryError;

      gdAvgDiversityWithinSubPops,
      gdAvgDiversityForEntirePop,
      gdInterPopDiversity,
      gdPropOfInterPopDiversity:  if FUserMeanV = nil  then RaiseMemoryError;
    end;
end;

function TSeqDistBase.IsUsefulComparison(i,j: Integer): Boolean;
begin
 Result := False;
  if CheckAbort then
    Exit;

  case ComputationType of
    gdPairwise,
    gdOverallMean,
    gdNetGroupMean:       Result := True;
    gdWithinGroupMean:    Result := (FGpId[i] = FGpId[j]);
    gdBetweenGroupMean:   Result := (FGpId[i] <> FGpId[j]);

    gdAvgDiversityWithinSubPops:  Result := (FGpId[i] <> FGpId[j]);
    gdAvgDiversityForEntirePop,
    gdInterPopDiversity,
    gdPropOfInterPopDiversity:  Result := True;
  else
    Result := False;
  end;
end;

function TSeqDistBase.ComputeDistances: Boolean;
var
  CurRun, i, j: Integer;
  BootRepsStr: String;
begin
  FStartTime := Now;
  Result := False;
  try
    CheckBasicInfo;  // checks if basic information is there
    CheckFreqInfo;   // checks if frequency information is given
    CheckGroupInfo;  // checks if all group information is there

    CheckMemory;     // checks if user has given memory for results
    if (ComputationType = gdPairwise) and (VarType <> gdBootstrapVar) then
    begin
      ComputeFastExclusivePairwiseDistances;
      Result := true;
    end
    else
    begin //=== Otherwise we have a more complicated computation
      AllocateMatrices; // allocate any matrices needed

      // we are finished unless the bootstrap variance is needed
      if VarType = gdBootstrapVar then
      begin
        // first allocate memory for bootstrap resampling
        if FFreqTable <> nil then
          FFreqTable := nil;
        //FFreqTable := nil;
        //GetMem(FFreqTable, FNoOfSites*sizeOf(Integer));
        SetLength(FFreqTable, FNoOfSites); // Fixed in 2.1
        FFreqTableLen := FNoOfSites;
        IsInternalFT := True;
        for i:=0 to FNoOfSites-1 do
          FFreqTable[i] := 1;
        BootReps := FDistPack.BootReps;
        MegaRandSeed(FDistPack.RandSeed);

        // setup proxies for temporary work
        Dx       := FUserD;        Dxx       := FUserV;
        GpDx     := FUserGpD;      GpDxx     := FUserGpV;
        MeanGpDx := FUserMeanGpD;  MeanGpDxx := FUserMeanGpV;
        MeanDx   := FUserMeanD;    MeanDxx   := FUserMeanV;

        // Now we do the computations;
        InitializeUserD;
        InitializeUserV;
        InitializeDreps;


        PercentPerAdvance := BootReps/100;
        CurAdvance        :=  0;
        BootRepsStr := IntToStr(BootReps);
        for CurRun:=0 to BootReps-1 do
        begin
          totalPercent := 0;
          Resample(FFreqTable, FNoOfSites);
          for i:= 1 to FNoOfSeqs-1 do
          begin
            for j:= 0 to i-1 do
            begin
              IsUsefulComparison(i,j); //Check if we need to use this
              try
                DoPairwiseAnalysis(i,j);
              except
                on E:Exception do
                begin
                  FErrorI := i;
                  FErrorJ := j;
                  if FQuickExit then
                     raise;
                  Dist1 := IDV;
                end;
              end;
              PairD[i][j] := Dist1;
            end; // end of for j
          end;
          FillDxMatrixFromPairD;
          CopyCurDToProxyD(True);
          AddProgressIncrement;
          UpdateRunStatus(BOOT_REP_PROG_STR, IntToStr(CurRun + 1) + '/' + BootRepsStr);
        end; // end of runs
        SummarizeBootstrapVar;
        FFreqTable := nil;  // 2.1
      end;

      PercentPerAdvance := FNoOfSeqs*(FNoOfSeqs-1)/2/100;
      CurAdvance        :=  0;
      // Now do the estimation for the original variables
      InitializeUserD;
      for i:= 1 to FNoOfSeqs-1 do
      begin
        for j:= 0 to i-1 do
        begin
          IsUsefulComparison(i,j); //Check if we need to use this
          try
            DoPairwiseAnalysis(i,j);
          except
            On E:Exception do
            begin
              FErrorI := i;
              FErrorJ := j;
              if FQuickExit then
                  raise;
              Dist1 := IDV;
            end;
          end;
          PairD[i][j] := Dist1; //save for use
          AddProgressIncrement;
        end; // end of for j
      end;  // end of for i
      FillDxMatrixFromPairD;
      CopyCurDToProxyD(False);
      Result:= True;
    end;
    FEndTime := Now;
  except
    On E: Exception do
    begin
      Result := False;
      raise;
    end
  end;
end;

// FOR KT
function TSeqDistBase.ComputeDistancesSE: Boolean;
var
  CurRun, i: Integer;
  StartTime: TDateTime;
  BootRepsStr: String;

  procedure InitializeParameterVariance;
  begin
    Vk1 :=  0;   Ak1 := 0;
    Vk2 :=  0;   Ak2 := 0;
    VpA :=  0;   ApA := 0;
    VpT :=  0;   ApT := 0;
    VpC :=  0;   ApC := 0;
    VpG :=  0;   ApG := 0;
  end;

  procedure ComputeParameterVariance;
  begin
    Ak1 := Ak1/BootReps;
    Ak2 := Ak2/BootReps;
    ApA := ApA/BootReps;
    ApT := ApT/BootReps;
    ApC := ApC/BootReps;
    ApG := ApG/BootReps;
    Vk1 := Vk1/BootReps -Ak1*Ak1;
    Vk2 := Vk2/BootReps -Ak2*Ak2;
    VpA := VpA/BootReps -ApA*ApA;
    VpT := VpT/BootReps -ApT*ApT;
    VpC := VpC/BootReps -ApC*ApC;
    VpG := VpG/BootReps -ApG*ApG;
  end;

begin
  Result := False;
  try
    CheckBasicInfo;  // checks if basic information is there
    CheckFreqInfo;   // checks if frequency information is given
    CheckGroupInfo;  // checks if all group information is there

    CheckMemory;     // checks if user has given memory for results

    if (ComputationType = gdPairwise) and (VarType <> gdBootstrapVar) then
    begin
      ComputeFastExclusivePairwiseDistancesSE;
      Result := True;
    end
    else
    begin //=== Otherwise we have a more complicated computation
      AllocateMatrices; // allocate any matrices needed
      PercentPerAdvance := 1;
      // we are finished unless the bootstrap variance is needed
      if VarType = gdBootstrapVar then
      begin
        // first allocate memory for bootstrap resampling
        if FFreqTable <> nil then
          FFreqTable := nil; //find all these and setlength to zero to prevent memory leak
        SetLength(FFreqTable, FNoOfSites);
        FFreqTableLen := FNoOfSites;
        IsInternalFT := True;
        for i:=0 to FNoOfSites-1 do
          FFreqTable[i] := 1;
        BootReps := FDistPack.BootReps;
        MegaRandSeed(FDistPack.RandSeed);

        // setup proxies for temporary work
        Dx       := FUserD;        Dxx       := FUserV;
        GpDx     := FUserGpD;      GpDxx     := FUserGpV;
        MeanGpDx := FUserMeanGpD;  MeanGpDxx := FUserMeanGpV;
        MeanDx   := FUserMeanD;    MeanDxx   := FUserMeanV;

        // Now we do the computations;
        InitializeUserD;
        InitializeUserV;
        InitializeDreps;

        PercentPerAdvance := BootReps/100;
        CurAdvance        :=  0;
        InitializeParameterVariance;
        StartTime := Now;
        BootRepsStr := IntToStr(BootReps);
        for CurRun:=0 to BootReps-1 do
        begin
          Resample(FFreqTable, FNoOfSites);

          DoSimultaneousAnalysis(PairD, false);
          Vk1 := Vk1 +k1*k1; Ak1 := Ak1 +k1;
          Vk2 := Vk2 +k2*k2; Ak2 := Ak2 +k2;
          VpA := VpA +pA*pA; ApA := ApA +pA;
          VpT := VpT +pT*pT; ApT := ApT +pT;
          VpC := VpC +pC*pC; ApC := ApC +pC;
          VpG := VpG +pG*pG; ApG := ApG +pG;

          FillDxMatrixFromPairD;
          CopyCurDToProxyD(True);
          if not ShowInProgressBar then
          begin
            UpdateProgress(Min(Round(CurRun / BootReps * 100), 100), 'Status', 'Computing Distances SE');
            UpdateRunStatus(BOOT_REP_PROG_STR, IntToStr(CurRun + 1) + '/' + BootRepsStr);
          end;
          AddProgressIncrement;
          if CurRun = 0 then
            if MillisecondsBetween(Now, StartTime) < PROGRESS_UPDATE_INTERVAL then
              ShowInProgressBar := False;
        end; // end of runs
        ComputeParameterVariance;
        SummarizeBootstrapVar;
        FFreqTable := nil;  // 2.1
      end;

      // Now do the estimation for the original variables
      InitializeUserD;

       DoSimultaneousAnalysis(PairD, true);

      FillDxMatrixFromPairD;
      CopyCurDToProxyD(False);
      Result:= True;
    end;
  except
    On E: Exception do
    begin
      Result := False;
      raise;
    end
  end;
end;

//=== This uses all usable pairs for every computation
procedure TSeqDistBase.FillDxMatrixFromPairD;
var
  i, j, gI, gJ: Integer;
  TempDouble : Double;
  // sub procedures

  //1. Calculate between gp means
  procedure CalculateBetweenGpMeans;
  var
    i, j, gI, gJ: Integer;
  begin
    for gI := 1 to FNoOfGps-1 do
      for gJ := 0 to gI-1 do
      begin
        CurGpD[gI][gJ] := 0;
        NPairs[gI][gJ] := 0;
      end;

    // Add pairs
    for i:= 1 to FNoOfSeqs-1 do
      for j:= 0 to i-1 do
      begin
        gI := FGpID[i];
        gJ := FGpID[j];
        if gI = gJ then  // to ensure between group
          Continue;
        if gI < gJ then
        begin
          gI := FGpID[j];
          gJ := FGpID[i];
        end;
        if PairD[i][j] > IDV then
        begin
          CurGpD[gI][gJ] := CurGpD[gI][gJ] + PairD[i][j]; // mega2b4
          Inc(NPairs[gI][gJ]);
        end;
      end;

    // Take average
    for gI := 1 to FNoOfGps-1 do
      for gJ := 0 to gI-1 do
         if NPairs[gI][gJ] > 0 then  //mega2b4
           CurGpD[gI][gJ] := CurGpD[gI][gJ]/NPairs[gI][gJ]
         else
           CurGpD[gI][gJ] := IDV;
  end;

  //2. Calculate Within gp means
  procedure CalculateWithinGpMeans;
  var
    i, j, gI, gJ: Integer;
  begin
    // initialize
    for gI := 0 to FNoOfGps-1 do  // fixed in version 2.1; index was wrongly from 1
    begin
      CurMeanGpD[gI] := 0;
      NPairs[gI][gI] := 0;
    end;

    // add all pairs
    for i:= 1 to FNoOfSeqs-1 do
      for j:= 0 to i-1 do
      begin
        gI := FGpID[i];
        gJ := FGpID[j];
        if gI <> gJ then  // to ensure within group
          Continue;
        if PairD[i][j] > IDV then
        begin
          CurMeanGpD[gI] := CurMeanGpD[gI] + PairD[i][j];
          Inc(NPairs[gI][gI]);
        end;
      end;

    // Take average
    for gI := 0 to FNoOfGps-1 do // take average
      if NPairs[gI][gI] > 0 then //mega2b4
        CurMeanGpD[gI] := CurMeanGpD[gI]/NPairs[gI][gI]
      else
        CurMeanGpD[gI] := IDV;
  end;

  //3.  Calculates overall mean
  procedure CalculateOverallMean;
  var
    i, j: Integer;
    PairCount: INteger;
  begin
    CurMeanD := 0; // set to 0
    PairCount := 0;
    for i:= 1 to FNoOfSeqs-1 do
      for j:= 0 to i-1 do
        if PairD[i][j] > IDV then
        begin
          CurMeanD := CurMeanD + PairD[i][j];
          Inc(PairCount);
        end;
    // Take average
    if PairCount > 0 then // mega2b4
      CurMeanD := CurMeanD/PairCount
    else
      CurMeanD := IDV;
  end;

  //4. Calculate Avg Diversity Within SubPops;  Pi_S
  // this is treated differently from the above 3
  function CalculateAvgDiversityWithinSubPops: Double;
  var
    gI: Integer;
  begin
    Result := 0;
    for gI := 0 to FNoOfGps-1 do
      if CurMeanGpD[gI] > IDV then
        Result := Result + CurMeanGpD[gI]
      else
      begin
        Result := IDV;
        Exit;
      end;
    Result := Result/FNoOfGps;
  end;
begin

 // initialize NPairs
 for i := 1 to FNoOfSeqs-1 do
   for j := 0 to i-1 do
     NPairs[i][j] := 0;

  if ComputationType = gdPairwise then
    for i := 1 to FNoOfSeqs-1 do
      for j := 0 to i-1 do
        CurD[i][j] := PairD[i][j];

  case ComputationType of
    gdOverallMean:       CalculateOverallMean;
    gdBetweenGroupMean:  CalculateBetweenGpMeans;
    gdWithinGroupMean :  CalculateWithinGpMeans;
    gdNetGroupMean    :
      begin
        CalculateBetweenGpMeans;
        CalculateWithinGpMeans;
        for gI := 1 to FNoOfGps-1 do
          for gJ := 0 to gI-1 do
            if CurGpD[gI][gJ] > IDV then  // so we atleast have between gp pairs
            begin
              if ((GpSz[gI] > 1) and (not (CurMeanGpD[gI] > IDV))) or
                 ((GpSz[gJ] > 1) and (not (CurMeanGpD[gJ] > IDV))) then
                   CurGpD[gI][gJ] := IDV
              else
              begin
                if GpSz[gI] > 1 then CurGpD[gI][gJ] := CurGpD[gI][gJ] - CurMeanGpD[gI]/2;
                if GpSz[gJ] > 1 then CurGpD[gI][gJ] := CurGpD[gI][gJ] - CurMeanGpD[gJ]/2;
              end;
            end;
      end;
    // now I am adding the diversity computation
    gdAvgDiversityWithinSubPops:
      begin
        CalculateWithinGpMeans;
        CurMeanD := CalculateAvgDiversityWithinSubPops;
      end;
    gdAvgDiversityForEntirePop:
      begin
        CalculateOverallMean;
      end;
    gdInterPopDiversity:
      begin
        CalculateWithinGpMeans;
        TempDouble := CalculateAvgDiversityWithinSubPops; // this update CurDiversity
        CalculateOverallMean;               // this updates CurMeanD
        CurMeanD := CurMeanD - TempDouble;
      end;
    gdPropOfInterPopDiversity:
      begin
        CalculateWithinGpMeans;
        TempDouble := CalculateAvgDiversityWithinSubPops; //
        CalculateOverallMean;               // this updates CurMeanD
        CurMeanD := (CurMeanD - TempDouble)/CurMeanD;
      end;
  end;
end;

procedure TSeqDistBase.CopyCurDToProxyD(IsBootRep: Boolean);
var
  i, j, gI, gJ: Integer;
begin
  if IsBootRep = False then
  begin
    case ComputationType of
      gdPairwise:
        for i := 1 to FNoOfSeqs-1 do
          for j := 0 to i-1 do        FUserD[i][j] := CurD[i][j];
      gdBetweenGroupMean,
      gdNetGroupMean:
        for gI := 1 to FNoOfGps-1 do
          for gJ := 0 to gI-1 do      FUserGpD[gI][gJ] := CurGpD[gI][gJ];
      gdWithinGroupMean:
        for gI := 0 to FNoOfGps-1 do  FUserMeanGpD[gI] := CurMeanGpD[gI];
      gdOverallMean:                  FUserMeanD^ := CurMeanD;

      gdAvgDiversityWithinSubPops,
      gdAvgDiversityForEntirePop,
      gdInterPopDiversity,
      gdPropOfInterPopDiversity:      FUserMeanD^ := CurMeanD;
    end;
  end
  else  // it is a bootstrap rep
  begin
    case ComputationType of
      gdPairwise:
        for i := 1 to FNoOfSeqs-1 do
          for j := 0 to i-1 do
            if CurD[i][j] > IDV then
            begin
              Dx[i][j]   := Dx[i][j]  + (CurD[i][j]/BootReps);
              Dxx[i][j]  := Dxx[i][j] + CurD[i][j]*CurD[i][j]/BootReps;
              if Dxx[i][j] < 0 then
                showmessage('Unexpected Error while calculating distance for the pair ('
                            +IntToStr(i+1) + ',' + IntToStr(j+1)+')');
              Drep[i][j] := Drep[i][j] +1;
            end;
      gdBetweenGroupMean,
      gdNetGroupMean:
        for gI := 1 to FNoOfGps-1 do
          for gJ := 0 to gI-1 do
            if CurGpD[gI][gJ] > IDV then
            begin
              GpDx[gI][gJ]   := GpDx[gI][gJ]  + (CurGpD[gI][gJ]/BootReps);
              GpDxx[gI][gJ]  := GpDxx[gI][gJ] + CurGpD[gI][gJ]*CurGpD[gI][gJ]/BootReps;
              GpDrep[gI][gJ] := GpDrep[gI][gJ]+1;
            end;
      gdWithinGroupMean:
        for gI := 0 to FNoOfGps-1 do
          if CurMeanGpD[gI] > IDV then
          begin
            MeanGpDx[gI]  := MeanGpDx[gI]  + (CurMeanGpD[gI]/BootReps);
            MeanGpDxx[gI] := MeanGpDxx[gI] + CurMeanGpD[gI]*CurMeanGpD[gI]/BootReps;
            MeanGpDrep[gI]   := MeanGpDrep[gI]+1;
          end;
      gdOverallMean:
        if CurMeanD > IDV then
        begin
          MeanDx^  := MeanDx^  + (CurMeanD/BootReps);
          MeanDxx^ := MeanDxx^ + CurMeanD*CurMeanD/BootReps;
          MeanDrep    := MeanDrep+1;
        end;
      gdAvgDiversityWithinSubPops,
      gdAvgDiversityForEntirePop,
      gdInterPopDiversity,
      gdPropOfInterPopDiversity:
        if CurMeanD > IDV then
        begin
          MeanDx^  := MeanDx^  + (CurMeanD/BootReps);
          MeanDxx^ := MeanDxx^ + CurMeanD*CurMeanD/BootReps;
          MeanDrep := MeanDrep+1;
        end;
    end;
  end;
end;

procedure TSeqDistBase.InitializeUserD; // same as initializing proxies also
var
  i, j, gI, gJ: Integer;
begin
  if FUserD <> nil then
    for i:=1 to FNoOfSeqs-1 do
      for j:=0 to i-1 do            FUserD[i][j] := 0;
  if FUserGpD <> nil then
    for gI := 1 to FNoOfGps-1 do
      for gJ := 0 to gI-1 do        FUserGpD[gI][gJ] := 0;
  if FUserMeanGpD <> nil then
    for gI := 0 to FNoOfGps-1 do    FUserMeanGpD[gI] := 0;
  if FUserMeanD <> nil then         FUserMeanD^ := 0;
end;

procedure TSeqDistBase.InitializeUserV;
var
  i, j, gI, gJ: Integer;
begin
  if FUserV <> nil then
    for i:=1 to FNoOfSeqs-1 do
      for j:=0 to i-1 do           FUserV[i][j] := 0;
  if FUserGpV <> nil then
    for gI := 1 to FNoOfGps-1 do
      for gJ := 0 to gI-1 do       FUserGpV[gI][gJ] := 0;
  if FUserMeanGpV <> nil then
    for gI := 0 to FNoOfGps-1 do   FUserMeanGpV[gI] := 0;
  if FUserMeanV <> nil then        FUserMeanV^ := 0;
end;

procedure TSeqDistBase.InitializeDreps;
var
  i, j, gI, gJ: Integer;
begin
  if Drep <> nil then
    for i:=1 to FNoOfSeqs-1 do
      for j:=0 to i-1 do           Drep[i][j] := 0;
  if GpDrep <> nil then
    for gI := 1 to FNoOfGps-1 do
      for gJ := 0 to gI-1 do       GpDrep[gI][gJ] := 0;
  if MeanGpDrep <> nil then
    for gI := 0 to FNoOfGps-1 do   MeanGpDrep[gI] := 0;
  MeanDrep := 0;
end;

//----------- Finally divides everyting by the no of valid reps
procedure TSeqDistBase.SummarizeBootstrapVar;
var
  i, j: Integer;
begin
  if VarType <> gdBootstrapVar then
    Exit;

  // I use definitions without proxies to avoid making errors
  case ComputationType of
    gdPairwise:
      begin
        for i:= 1 to FNoOfSeqs-1 do
          for j:= 0 to i-1 do
          begin
            if FUserV[i][j] < 0 then
              SetErrorStatus(HC_Unexpected_Error, 'Covariance estimated to be 0!');
            if (FUserD[i][j] = IDV) or (Drep[i][j] < 25) then
              FUserV[i][j] := IDV
            else
              FUserV[i][j] := Drep[i][j]*(FUserV[i][j]-
                              FUserD[i][j]*FUserD[i][j])/(Drep[i][j]-1);
          end;
        {$IFDEF DEBUG}
        {$IFNDEF VISUAL_BUILD}
        UserVarMatrixToFile(FUserV, NextAvailableFilenameNV(Format('_%d.variance', [TAnalysisInfo(ARP.FMAI).NumSitesPerSubSample])), FNoOfSeqs);
        {$ENDIF}
        {$ENDIF}
      end;
    gdBetweenGroupMean,
    gdNetGroupMean:     // same treatment
      for i := 1 to FNoOfGps-1 do
        for j := 0 to i-1 do
          if (FUserGpD[i][j] = IDV) or (GpDrep[i][j] < 25) then
             FUserGpV[i][j] := IDV
          else
             FUserGpV[i][j] := GpDrep[i][j]*(FUserGpV[i][j]-
                                 FUserGpD[i][j]*FUserGpD[i][j])/(GpDrep[i][j]-1);
    gdWithinGroupMean:
      for i := 0 to FNoOfGps-1 do
        if (FUserMeanGpD[i] = IDV) or (MeanGpDrep[i] < 25) then
          FUserMeanGpV[i] := IDV
        else
          FUserMeanGpV[i] := MeanGpDrep[i]*(FUserMeanGpV[i]-
                              FUserMeanGpD[i]*FUserMeanGpD[i])/(MeanGpDrep[i]-1);
    gdOverallMean:
      if (FUserMeanD^ = IDV) or (MeanDrep < 25) then
        FUserMeanV^ := IDV
      else
        FUserMeanV^ := MeanDrep*(FUserMeanV^ -
                       FUserMeanD^*FUserMeanD^)/(MeanDrep-1);

    gdAvgDiversityWithinSubPops,
    gdAvgDiversityForEntirePop,
    gdInterPopDiversity,
    gdPropOfInterPopDiversity:
      if (FUserMeanD^ = IDV) or (MeanDrep < 25) then
        FUserMeanV^ := IDV
      else
        FUserMeanV^ := MeanDrep*(FUserMeanV^ -
                       FUserMeanD^*FUserMeanD^)/(MeanDrep-1);
  end;
end;

procedure TSeqDistBase.ComputeFastExclusivePairwiseDistances;
var
  i, j: Integer;
  setHighDist: Integer;
  NumComparisons: LongInt;
begin
  totalPercent := 0;
  CheckBasicInfo;  // checks if basic information is there
  CheckFreqInfo;   // checks if frequency information is given
  CheckGroupInfo;  // checks if all group information is there

  CheckMemory;     // checks if user has given memory for results

  NumComparisons := (FNoOfSeqs * (FNoOfSeqs - 1) div 2);
  {$IFDEF DEBUG}
  if not ((ComputationType = gdPairwise) and (VarType <> gdBootstrapVar)) then
    SetErrorStatus(HC_Unexpected_Error, 'Invalid function call.');
  {$ENDIF}
  // First do the estimation of the original
  PercentPerAdvance := (FNoOfSeqs/100)*(FNoOfSeqs-1)/2;
  CurAdvance        :=  0;
  setHighDist := -1;  // not asked the developer yet
  if ShowInProgressBar then
    UpdateProgress(0, 'Status', 'Computing distances');

  for i:= 1 to FNoOfSeqs-1 do
  begin
    if ShowInProgressBar then
      if MillisecondsBetween(Time, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
        UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));

    for j:= 0 to i-1 do
    begin
      if CheckAbort then
        Exit;
      try
        DoPairwiseAnalysis(i,j);
      except
        On E:Exception do
        begin
          FErrorI := i;
          FErrorJ := j;
          Dist1 := IDV;
          Var1  := IDV;
          if FQuickExit then
          begin
            if not IsDeveloper then    //Errors will be handled in a different way when we build the non developer edition, are we sure we want this? -Nick
               raise;
            if SetHighDist < 1 then
            begin
              if MessageDlg('Do you want to set all invalid distances to 10.0?',
                       mtConfirmation, [mbYes,mbNo],0) = mrYes then
                SetHighDist := 1
              else
                SetHighDist := 0;
            end;

            if SetHighDist < 1 then
              raise;
            Dist1 := 10;
          end;
        end;
      end;
      FUserD[i][j] := Dist1;
      case VarType of
        gdAnalyticalVar:   FUserV[i][j] := Var1;
        gdMonteCarloTest:  FUserV[i][j] := Var1;
      end;
    end; // end of for j
  end;// end for i
end;

// KT DO IT NOW
procedure TSeqDistBase.ComputeFastExclusivePairwiseDistancesSE;
begin
  CheckBasicInfo;  // checks if basic information is there
  CheckFreqInfo;   // checks if frequency information is given
  CheckGroupInfo;  // checks if all group information is there

  CheckMemory;     // checks if user has given memory for results

  if not ((ComputationType = gdPairwise) and (VarType <> gdBootstrapVar)) then
    SetErrorStatus(HC_Unexpected_Error, 'Invalid function call.');

 // First do the estimation of the original
  PercentPerAdvance := 1;//no pairwise analysi
  CurAdvance        := 0;
  DoSimultaneousAnalysis(FUserD, true);
end;

procedure TSeqDistBase.AllocateMatrices;
begin
  if (ComputationType = gdPairwise) and (VarType <> gdBootstrapVar) then
      Exit;

  // First pairwise calculations are kept in here
  PairD := NewDistMatrix(FNoOfSeqs, False);
  SetLength(NPairs, FNoOfSeqs, FNoOfSeqs);

  case ComputationType of
    gdPairwise:          CurD       := NewDistMatrix(FNoOfSeqs,False);
    gdBetweenGroupMean:  CurGpD     := NewDistMatrix(FNoOfGps, False);
    gdWithinGroupMean:   SetLength(CurMeanGpD, FNoOfGps);
    gdNetGroupMean:
      begin
        CurGpD     := NewDistMatrix(FNoOfGps, False);
        SetLength(CurMeanGpD, FNoOfGps);
      end;
    gdAvgDiversityWithinSubPops,
    gdInterPopDiversity,
    gdPropOfInterPopDiversity:  SetLength(CurMeanGpD, FNoOfGps); // as just arrays are needed
  end;

  // to keep the # of good replicates
  if VarType = gdBootstrapVar then
    case ComputationType of
      gdPairwise:          Drep       := NewDistMatrix(FNoOfSeqs,False);  // keep the # of good reps
      gdNetGroupMean,
      gdBetweenGroupMean:  GpDrep     := NewDistMatrix(FNoOfGps,False);
      gdWithinGroupMean:   SetLength(MeanGpDrep, FNoOfGps);
    end;
end;

{
//==
function TSeqDistBase.CompositionDistance(NSites, NStates: LongInt; Seq1, Seq2: PChar; InvalidBase: Char): Double;
  function IsValidSite(SiteChar1, SiteChar2 : Char) : Boolean;
  begin
    Result := True;
    if (SiteChar1 = InvalidBase) or (SiteChar2 = InvalidBase) then
      Result := False;
  end;
var
  i, j: Integer;
begin
  for i:=0 to NStates-1 do
  begin
    Freq1[i] := 0;
    Freq2[i] := 0;
  end;

  i := 0;
  j := 0;
  while j < NSites do
  begin
    if IsValidSite(Seq1[i],Seq2[i]) then
    begin
      Inc(Freq1[Integer(Seq1[i])]);
      Inc(Freq2[Integer(Seq2[i])]);
      Inc(j);
    end;
    Inc(i);
  end;
  Result := 0;
  for i:=0 to NStates-1 do
    if Freq1[i] <> Freq2[i] then
      Result := Result + 0.5*(Freq1[i]-Freq2[i])*(Freq1[i]-Freq2[i]);
end;
}

//== An expanded composition distance ==
function TSeqDistBase.CompositionDistance(NSites, NStates: LongInt; Seq1, Seq2: PAnsiChar; InvalidBase: AnsiChar): Double;

  function IsValidSite(SiteChar1, SiteChar2 : AnsiChar) : Boolean;
  begin
    Result := True;
    if (SiteChar1 = InvalidBase) or (SiteChar2 = InvalidBase) then
      Result := False;
  end;

var
  i, NAlleles, Sum1, Sum2, AddInt, DiffCount: Integer;
  absDiff: Double;
begin
  Result :=0;
  for i:=0 to NStates-1 do
  begin
    Freq1[i] := 0;
    Freq2[i] := 0;
  end;

  DiffCount := 0;
  for i := 0 to IterateLen-1 do
    if IsValidSite(Seq1[i],Seq2[i]) then
    begin
      if FFreqTable = nil then
        AddInt :=1
      else
        AddInt := FFreqTable[i];
      Inc(Freq1[Integer(Seq1[i])], AddInt);
      Inc(Freq2[Integer(Seq2[i])], AddInt);
      if Seq1[i] <> Seq2[i] then
         DiffCount := DiffCount + AddInt;
    end;

  // compute distance
  NAlleles := 0;
  Sum1 := 0;
  Sum2 := 0;
  for i:=0 to NStates-1 do
  begin
    if (Freq1[i] + Freq2[i]) > 0 then
      Inc(NAlleles)
    else
      continue;

    Sum1 := Sum1 + Freq1[i];
    Sum2 := Sum2 + Freq2[i];
  end;

  for i:=0 to NStates-1 do
  begin
    if Freq1[i] = Freq2[i] then
      Continue;

    case DistComponent of
      gdBhattacharyaDist, gdManhattanDist,
      gdProvostiDist, gdRogersDist,
      gdNei1983Dist:            absDiff := abs(Freq1[i]/sum1 - Freq2[i]/sum2);
      gdKumarGadagkarDist, gdKumarGadagkarDisparityDist:
                                absDiff := abs(Freq1[i] - Freq2[i])  // for Dc
    else
                                absDiff := abs(Freq1[i] - Freq2[i]);  // for Dc
    end;

    case DistComponent of
      gdManhattanDist,
      gdProvostiDist          :  Result := Result + absDiff;
      gdBhattacharyaDist      :  Result := Result + absDiff/(Freq1[i]/sum1 + Freq2[i]/sum2);
      gdRogersDist            :  Result := Result + absDiff*absDiff;
      gdNei1983Dist           :  Result := Result + sqrt((Freq1[i]/Sum1) * (Freq2[i]/Sum2));
      gdKumarGadagkarDist, gdKumarGadagkarDisparityDist:
                                 Result := Result + absDiff*absDiff //Dc and ID
    else
                                 Result := Result + absDiff*absDiff; //Dc
    end;
  end;

  case DistComponent of
    gdBhattacharyaDist        : Result := 0.5*Result;
    gdProvostiDist            : Result := 0.5*Result;
    gdRogersDist              : Result := sqrt(0.5*Result);
    gdNei1983Dist             : Result := 1 -  Result;
    gdKumarGadagkarDist       : Result := 0.5*Result;  // just Dc
    gdKumarGadagkarDisparityDist:
                             begin
                                Result := 0.5*Result - DiffCount;
                                if Result < 0 then
                                   Result := 0;
                             end;
  else
                                Result := 0.5*Result;  // Dc
  end;

end;

procedure TSeqDistBase.UpdateProgress(aProgress: Integer);
begin
  if not ShowInProgressBar then
    Exit;
  LastUpdateTime := Now;
  if Assigned(ProgressCheckCancelFunc) then
  begin
    FCancelled := ProgressCheckCancelFunc(aProgress, EmptyStr);
    if FCancelled then
      raise EAbort.Create('distance calculation aborted');
  end
  else
    ARP.Progress := aProgress;
end;

procedure TSeqDistBase.UpdateProgress(aProgress: Integer; aStatus: String; aInfo: String);
begin
  if not ShowInProgressBar then
    Exit;
  LastUpdateTime := Now;
  if Assigned(ProgressCheckCancelFunc) then
  begin
    FCancelled := ProgressCheckCancelFunc(aProgress, aInfo);
    if FCancelled then
      raise EAbort.Create('distance calculation aborted');
  end
  else
  begin
    ARP.Progress := aProgress;
    ARP.AddRunStatusInfo(aStatus, aInfo);
  end;
end;

procedure TSeqDistBase.UpdateRunStatus(aStatus: AnsiString; aInfo: AnsiString);
begin
  if Assigned(RunStatusProc) then
    RunStatusProc(aStatus, aInfo);
end;

procedure TSeqDistBase.SetErrorStatus(errorId: Integer; errorMsg: String);
begin
  InternalErrId := errorId;
  InternalErrStr := errorMsg;
  raise EDistComputationError.Create(errorMsg, errorId);
end;

procedure TSeqDistBase.MonteCarloDisparityIndexTest(NDiff, NSites, NStates: LongInt; IDValue: Double; Seq1, Seq2: PAnsiChar; InvalidBase: AnsiChar);
  function IsValidSite(SiteChar1, SiteChar2 : AnsiChar) : Boolean;
  begin
    Result := True;
    if (SiteChar1 = InvalidBase) or (SiteChar2 = InvalidBase) then
      Result := False;
  end;
var
  S1, S2, M1, M2: array of integer;
  curNd, mutant: Integer;
  rnSite, rep, rn, i, TotalMutations: integer;
  Nd, Dce, Dco : Extended;
  WereTheSame : Boolean;
begin
  if FFreqTable <> nil then
  begin
    InternalErrId := HC_Unexpected_Error;
    InternalErrStr:= 'Disparity Index Test cannot have FreqTable non-nil.';
    Exit;
  end;

  if IDValue <= 0 then
  begin
    Dist1 := 1;  // prob
    Var1  := 0;  // stat
    Exit;
  end;

  // otherwise we can do monte-carlo to compute variance
  Var1 := IDValue/NSites;  // this is now the test statistic per site
  try
    Dco := IDValue;  // observed Dc
    Nd :=  NDiff;   // observed Nd
    SetLength(S1, NSites);
    SetLength(S2, NSites);
    SetLength(M1, NSites);
    SetLength(M2, NSites);
    Dist1 := 0;

    rnSite := 0;
    for i:=0 to IterateLen-1 do
    begin
      if IsValidSite(Seq1[i],Seq2[i]) then
        if (Integer(Seq1[i]) < NStates) and (Integer(Seq2[i]) < NStates) then
        begin
          S1[rnSite] := Integer(Seq1[i]);
          S2[rnSite] := Integer(Seq2[i]);
          Inc(rnSite);
        end;
    end;

    if rnSite <> NSites then
    begin
      InternalErrId := HC_Unexpected_Error;
      InternalErrStr:= 'Disparity Index Test has site number mismatch.';
      Exit;
    end;

    if NDiff = 0 then
    begin
      Dist1 := 1; // this is prob
      Var1  := 0;
      Exit;
    end;

    for Rep:=0 to FDistPack.BootReps-1 do
    begin
      for i:=0 to NSites-1 do
      begin
        // generate a URan sequence from the given distribution
        rn := URan(2*NSites);
        if rn < NSites then M1[i] := S1[rn]
                       else M1[i] := S2[rn-NSites];
        M2[i] := M1[i] // copy M1 to M2 at the same time
      end;

      // do mutations
      TotalMutations := Round(Nd);
      CurNd := 0;
      while CurNd < TotalMutations do
      begin
        // the mutations from original pile of nucs/aminos
        rn := URan(2*NSites);
        if rn < NSites then mutant := S1[rn]
                       else mutant := S2[rn-NSites];
        // site to mutate
        rnSite := URAN(NSites);
        WereTheSame := (M1[rnSite] = M2[rnSite]);
        // seq to mutate
        rn := URan(2);
        //now make mutation
        if rn = 0 then   M1[rnSite] := mutant
        else             M2[rnSite] := mutant;

        if M1[rnSite] = M2[rnSite] then
        begin
          if not WereTheSame then
            Dec(CurNd)
        end
        else // they are now different
        begin
          if WereTheSame then
            Inc(CurNd);
        end;
      end; // end of while CurNd < TotalMutations

      // get freq
      for i:=0 to NStates-1 do
      begin
        Freq1[i] := 0;
        Freq2[i] := 0;
      end;

      for i:=0 to NSites-1 do
      begin
        Inc(Freq1[M1[i]]);
        Inc(Freq2[M2[i]]);
      end;
      Dce := 0;
      for i:=0 to NStates-1 do
        if Freq1[i] <> Freq2[i] then
          Dce := Dce + 0.5*(Freq1[i]-Freq2[i])*(Freq1[i]-Freq2[i]);
      Dce := Dce - Nd;
      if Dco > Dce then
        Dist1 := Dist1 + 1;
    end;
    Dist1 := 1 - Dist1/FDistPack.BootReps; // this is prob
  finally
    M1 := nil;
    M2 := nil;
    S1 := nil;
    S2 := nil;
  end;
end;

end.


