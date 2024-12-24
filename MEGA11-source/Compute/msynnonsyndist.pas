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

unit MSynNonsynDist;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
{
ErrorHelp Checked 1/28/00
Warnings  Checked 1/28/00
}
uses
  MSeqDistBase, MCodons, MegaConsts;

type
  TSynNonsynDist = Class(TSeqDistBase)
  private
    FFixedRateRatio: Double; // ts/tv ratio for the Pathway method
    CodonInfo: TCodonInfo;  // Used to get all the codon information

    function ComputeIterateLen: Integer;
    function ComplexDegen(Degen: Integer): Boolean;

    procedure NeiGojoboriAnalysis(i,j: Integer);
    procedure InaAnalysis(i,j: Integer);
    procedure LiWuLouTypeAnalysis(i,j: Integer);

    function GetOnePathwayCounts(CodonI, CodonJ: TCodonMap; Pos: Integer; LxPos, LyPos: array of Integer; var sTs, nTs, sTv, nTv: array of Integer): Integer;
    function GetTwoPathwayCounts(CodonI, CodonJ: TCodonMap; DiffPos, LxPos, LyPos: array of Integer; var sTs, nTs, sTv, nTv: array of Integer): Integer;
    function GetThreePathwayCounts(CodonI, CodonJ: TCodonMap; LxPos, LyPos:array of Integer; var sTs, nTs, sTv, nTv: array of Integer): Integer;

    procedure SetCodeTable(AString: AnsiString);

    // error setting functions to reduce the work load
    function SetTooSmallSErr(value: Double): Boolean;
    function SetTooSmallNErr(value: Double): Boolean;
    function SetPSMoreThan1Err(Pvalue: Double): Boolean;
    function SetPNMoreThan1Err(Pvalue: Double): Boolean;
    function SetJCFailedErr(Pvalue: Double): Boolean;
    function SetK2FailedErr(v1, v2: Double): Boolean;

  protected
    procedure DoPairwiseAnalysis(i,j: Integer); override; // internal
  public
    constructor Create;
    destructor Destroy; override;


    function  ComputeDistances: Boolean; override;
  public
    property CodeTable: AnsiString write SetCodeTable;
    property FixedRateRatio: Double read FFixedRateRatio write FFixedRateRatio;

    property NoOfSeqs;
    property NoOfSites;
    property Sequences;
    property DistPack;

    property FreqTable;
    property FreqTableLen;

    property GpId;
    property NoOfGps;

    property D;
    property V;
    property GpD;
    property GpV;
    property MeanD;
    property MeanV;
    property MeanGpD;
    property MeanGpV;

    property QuickExit;
  end;

implementation

uses
  SysUtils, Math, MegaUtils, MDistPack, MD_InputSeqData, ErrorMessages_HC;

constructor TSynNonsynDist.Create;
begin
  inherited Create;
  FFixedRateRatio := -1.0; // shows that it needs to be computed
  CodonInfo := nil;
end;

destructor TSynNonsynDist.Destroy;
begin
  if CodonInfo <> nil then
    CodonInfo.Free;
  CodonInfo := nil;
  inherited Destroy;
end;

procedure TSynNonsynDist.SetCodeTable(AString: AnsiString);
begin
  try
    if CodonInfo <> nil then
      FreeAndNil(CodonInfo);  // Previously wasn't freeing CodonInfo if it was already allocated.
    CodonInfo := TCodonInfo.Create;
    CodonInfo.CodeTable := AString;
  except on E: Exception do
     raise Exception.Create('An error has occured when attempting to set the Code Table (SetCodeTable(' + AString + ')).' + #10 + #13 + E.Message);
  end;
end;

function TSynNonsynDist.SetTooSmallSErr(value: Double): Boolean;
begin
  Result := False;
  if value >= 1 then
    Exit;
  InternalErrId := HC_No_Common_Sites;
  InternalErrStr:= 'Less than 1 total synonymous sites';
  Result := True; // this means error
end;

function TSynNonsynDist.SetTooSmallNErr(value: Double): Boolean;
begin
  Result := False;
  if value >= 1 then
    Exit;
  InternalErrId := HC_No_Common_Sites;
  InternalErrStr:= 'Less than 1 total nonsynonymous sites';
  Result := True; // this means error
end;

function TSynNonsynDist.SetPSMoreThan1Err(Pvalue: Double): Boolean;
begin
  Result := False;
  if Pvalue <= 1.0 then
    Exit;
  InternalErrId := HC_p_distance_is_found_to_be_1;
  InternalErrStr:= 'Proportion of synonymous sites different is more than 100%.';
  Result := True; // this means error
end;

function TSynNonsynDist.SetPNMoreThan1Err(Pvalue: Double): Boolean;
begin
  Result := False;
  if Pvalue <= 1.0 then
    Exit;
  InternalErrId := HC_p_distance_is_found_to_be_1;
  InternalErrStr:= 'Proportion of nonsynonymous sites different is more than 100%.';
  Result := True; // this means error
end;

function TSynNonsynDist.SetJCFailedErr(Pvalue: Double): Boolean;
begin
  Result := False;
  if Pvalue <= 0.74 then
    Exit;
  InternalErrId := HC_Jukes_Cantor_Correction_Failed;
  InternalErrStr:= 'Jukes-Cantor (1969) correction failed.';
  Result := True; // this means error
end;

function TSynNonsynDist.SetK2FailedErr(v1, v2: Double): Boolean;
begin
  Result := False;
  if (v1 > 0.01) and (v2 > 0.01) then
    Exit;
  InternalErrId := HC_Kimura_1980_Distance_Correction_Failed;
  InternalErrStr:= 'Kimura 2-parameter (1980) correction failed.';
  Result := True; // this means error
end;

// get the length of iteration
function TSynNonsynDist.ComputeIterateLen: Integer;
begin
  if FFreqTable <> nil then
    Result := FFreqTableLen
  else
    Result := FNoOfSites;
end;

{ Index:  0: Nondegenerate
          1: Complex Two-fold sites
          2: Simple Two-fold degenerate; All Ts syn and all Tv not syn
          3: Complex Two-fold sites
          4: Four-fold degenerate}

function TSynNonsynDist.ComplexDegen(Degen: Integer): Boolean;
begin
  case Degen of
    0, 2, 4:  Result := False;
  else
    Result := True;
  end;
end;

procedure TSynNonsynDist.DoPairwiseAnalysis(i,j: Integer);
begin
  InternalErrId := 0;
  InternalErrStr := EmptyStr;
  case DistModel of
      gdNeiGojobori:        NeiGojoboriAnalysis(i,j);
      gdIna:                InaAnalysis(i,j);
      gdLWL,
      gdPBL,
      gdComeronKumar:       LiWuLouTypeAnalysis(i,j);
  end;

  if InternalErrId > 0 then
  begin
    Dist1 := InvalidDistValue;
    if FQuickExit then
      SetErrorStatus(InternalErrId, InternalErrStr);
  end;
end;

//----- Computes the pairwise between taxa
function  TSynNonsynDist.ComputeDistances: Boolean;
begin
  if Assigned(D_InputSeqData) and (D_InputSeqData.CodeTable <> EmptyStr) then
    SetCodeTable(D_InputSeqData.CodeTable);

  case DistModel of
      gdNeiGojobori:
           begin
             if (not CodonInfo.HasSynSiteTable) or (CodonInfo.TsTvRatio <> 1) then
             begin
               CodonInfo.TsTvRatio := 1;     //= 0.5 of the R = a/2b
               CodonInfo.GenerateSynSiteTable;
             end;
             if not CodonInfo.HasDsDnMatrices then
               CodonInfo.GenerateDsDnMatrices;
           end;
      gdIna:
         begin
           Assert(Assigned(CodonInfo), 'missing codoninfo');
           FFixedRateRatio := FDistPack.InaRValue;
           if (not CodonInfo.HasSynSiteTable) or (CodonInfo.TsTvRatio <> FFixedRateRatio) then
           begin
             CodonInfo.TsTvRatio := FFixedRateRatio*2;  //= alpha/beta
             CodonInfo.GenerateSynSiteTable;
           end;
           if not CodonInfo.HasDsDnMatrices then
             CodonInfo.GenerateDsDnMatrices;
         end;
  end;
  Result := inherited ComputeDistances;
end;

procedure TSynNonsynDist.NeiGojoboriAnalysis(i, j: Integer); // only for a pair
var
  Seq1, Seq2: PAnsiChar;
  CodonI, CodonJ:  TCodonMap;
  Site, MultInt: Integer;
  Sd, Nd, S, N, pS, pN: Double;

  procedure ApplyCorrection(pDiff, NSite: Double; var d, v: Double);
  begin
    case DistCorrection of
      gdNoOfDiff:   d := pDiff*NSite;
      gdPropDist:   d := pDiff;
      gdJukesCantor:  if    SetJCFailedErr(pDiff) then Exit
                      else  d := -0.75*Ln(1- 4*pDiff/3)
    end;

    if VarType = gdAnalyticalVar then
    begin
      v := -1;
      if InternalErrId > 0 then Exit;
      case DistCorrection of
        gdNoOfDiff:    v := pDiff*(1-pDiff)*NSite ;
        gdPropDist:    v := pDiff*(1-pDiff)/NSite;
        gdJukesCantor: v := pDiff*(1-pDiff)/sqr(1 - 4*pDiff/3)/NSite;
      end;
    end;
  end;
  procedure DeriveEstimate(a1, n1, a2, n2: Double; var d, v: Double);
  var
    d1: Double = 0;
    v1: Double = 0;
    d2: Double = 0;
    v2: Double = 0;
  begin
    ApplyCorrection(a1, n1, d1, v1);
    if InternalErrId > 0 then Exit;

    ApplyCorrection(a2, n2, d2, v2);
    if InternalErrId > 0 then Exit;

    case DistComponent of
      gdRatioSynNonsyn, gdRatioNonsynSyn:  d := d1/d2;
      gdDiffSynNonsyn, gdDiffNonsynSyn:
        begin
           d := d1 - d2;
           if VarType = gdAnalyticalVar then
             v := v1 + v2;
        end;
      gdPurifySelTest, gdNeutralEvolTest, gdPosSelTest:
        begin
          d := d1 - d2;
          if VarType = gdAnalyticalVar then
            v := v1 + v2;
        end;
    end;
  end;
begin
  Seq1 := FSequences[i];
  Seq2 := FSequences[j];

  // initialize
  Sd := 0; Nd := 0;
  S  := 0; N  := 0;

  IterateLen := ComputeIterateLen;
  MultInt := 1;
  for Site:=0 to IterateLen-1 do
  begin
    CodonI := Seq1[site];
    CodonJ := Seq2[site];
    with CodonInfo do
    begin
      if (CodonI > #63) or (CodonJ > #63) then
        Continue;
      if FFreqTable <> nil then
      begin
        MultInt := FFreqTable[Site];
        if MultInt < 1 then Continue;
      end;

      S := S + MultInt*(SynSites[CodonI] + SynSites[CodonJ]);
      N := N + MultInt*(NonsynSites[CodonI] + NonsynSites[CodonJ]);

      if CodonI <> CodonJ then
      begin
        Sd := Sd + MultInt*SynDiff[CodonI, CodonJ];
        Nd := Nd + MultInt*NonsynDiff[CodonI, CodonJ];

{        CurSiteS := MultInt*(SynSites[CodonI] + SynSites[CodonJ])/2;
        ASynDiffs := SynDiff[CodonI, CodonJ];
        ANonsynDiffs := NonsynDiff[CodonI, CodonJ];

        NewSd := NewSd + MultInt*ASynDiffs*(CurSiteS);
        NewNd := NewNd + MultInt*ANonsynDiffs*(3-CurSiteS);
}      end;
    end; // end of with CodonInfo
  end; // end of for every site in a pairwise comparison

  {todo -oKumar -cSynNonsyn: problem with pS, pN calculation when comparing two one-codon sequences: GAA with GAG
                 because here there is only 0.33 site but one syn difference, so pS = 3}
  {todo -oKumar -cSynNonsyn: it is rather tricky}
  pS := 10;
  pN := 10;
  //Average numbers of SynSites and NonSyn sites
  S := S/2; N := N/2;
  if S > 0 then
    pS := Sd/S;
  if N > 0 then
   pN := Nd/N;

  // now estimate appropriate parameters
  if FDistPack.SelectionTestType <> gdNone then
  begin
    if SetTooSmallSErr(S)    or SetTooSmallSErr(N) or
       SetPSMoreThan1Err(pS) or SetPNMoreThan1Err(pN) then
         Exit;
    case FDistPack.SelectionTestType of
      gdExactTest,
      gdMcdonaldKreitmanTest:
        try
          N := S+N;    // we first Fix the total no of S and N sites
          S := Ceil(S); // to make test conservative
          N := N - S;
          // Now we fix the total number of differences
          Sd := Ceil(Sd);  // to make the test conservative
          Nd := Floor(Nd); // to make the test conservative
          // only S, N, and Sd and Nd matter
          if pS > pN then
            Dist1 := 1   // this makes it a perfect one-tailed test
          else
            Dist1 := FisherExactTestOneTailed(Floor(Sd), Floor(S), Floor(Nd), Floor(N));
        except
          InternalErrId := HC_Fisher_s_Exact_Test_Has_Failed;
        end;
      gdZSelectionTest:
       case DistComponent of
         gdPurifySelTest:  DeriveEstimate(pS, S, pN, N, Dist1, Var1);
         gdNeutralEvolTest,  // n - s
         gdPosSelTest:     DeriveEstimate(pN, N, pS, S, Dist1, Var1);
       end;
    end;
    Exit;
  end;

  // compute the distances
  case DistComponent of
    gdNoOfSynSites:     Dist1 := S;

    gdNoOfNonsynSites:  Dist1 := N;
    gdSynOnly:        if    SetTooSmallSErr(S) or SetPSMoreThan1Err(pS) then Exit
                      else  ApplyCorrection(pS, S, Dist1, Var1);
    gdNonsynOnly:     if    SetTooSmallNErr(N) or SetPNMoreThan1Err(pN) then Exit
                      else  ApplyCorrection(pN, N, Dist1, Var1);
    gdRatioSynNonsyn: if    SetTooSmallSErr(S)    or SetTooSmallSErr(N) or
                            SetPSMoreThan1Err(pS) or SetPNMoreThan1Err(pN) then Exit
                      else  DeriveEstimate(pS, S, pN, N, Dist1, Var1);
    gdRatioNonsynSyn: if    SetTooSmallSErr(S)    or SetTooSmallSErr(N) or
                            SetPSMoreThan1Err(pS) or SetPNMoreThan1Err(pN) then Exit
                      else  DeriveEstimate(pN, N, pS, S, Dist1, Var1);
    gdDiffSynNonsyn:  if    SetTooSmallSErr(S) or SetTooSmallSErr(N) or
                      SetPSMoreThan1Err(pS) or SetPNMoreThan1Err(pN) then Exit
                      else  DeriveEstimate(pS, S, pN, N, Dist1, Var1);
    gdDiffNonsynSyn:  if    SetTooSmallSErr(S) or SetTooSmallSErr(N) or
                      SetPSMoreThan1Err(pS) or SetPNMoreThan1Err(pN) then Exit
                      else  DeriveEstimate(pN, N, pS, S, Dist1, Var1);
  end;
end;

//-------------- Ina's analysis
procedure TSynNonsynDist.InaAnalysis(i,j: Integer);
var
  Seq1, Seq2: PAnsiChar;
  CodonI, CodonJ:  TCodonMap;
  Site, MultInt: Integer;
  Sd, Nd, S, N, P, Q, a, b, c: Double;
  sTs, nTs, sTv, nTv: Double;

  procedure ApplyCorrection(Ts, Tv, NSite: Double; var d, v: Double);
  begin
    P := Ts/NSite;
    Q := Tv/NSite;
    case DistCorrection of
      gdNoOfDiff:    d := Ts+Tv;
      gdPropDist:    d := P+Q;
      gdJukesCantor: if    SetJCFailedErr(P+Q) then Exit
                      else
                      begin
                        d := -0.75*Ln(1- 4*(P+Q)/3);
                        if d < 0 then
                          InternalErrId := HC_Jukes_Cantor_Correction_Failed;
                      end;
      gdKimura2para: if    SetK2FailedErr(1-2*P-Q, 1-2*Q) then Exit
                     else
                     begin
                       d := -0.5*Ln(1-2*P-Q) - 0.25*Ln(1-2*Q);
                       if d < 0 then
                          InternalErrId := HC_Kimura_1980_Distance_Correction_Failed;
                     end;
    end;


    if InternalErrId > 0 then
      Exit;
    if VarType = gdAnalyticalVar then
      case DistCorrection of
        gdNoOfDiff:    v := (P+Q)*(1-P-Q)*NSite;
        gdPropDist:    v := (P+Q)*(1-P-Q)/NSite;
        gdJukesCantor: v := (P+Q)*(1-P-Q)/SQR(1- 4*(P+Q)/3)/NSite;
        gdKimura2para:
          begin
            a := 1/(1-2*P-Q); b := 1/(1-2*Q);
            c := (a+b)/2;
            v := DeltaVar([a, P, c, Q, NSite]);
          end;
      end;
  end;
  procedure DeriveEstimate(Ts1, Tv1, n1, Ts2, Tv2, n2: Double; var d, v: Double);
  var
    d1: Double = 0;
    v1: Double = 0;
    d2: Double = 0;
    v2: Double = 0;
  begin
    if InternalErrId > 0 then
      Exit;
    ApplyCorrection(Ts1, Tv1, n1, d1, v1);
    if InternalErrId > 0 then
      Exit;
    ApplyCorrection(Ts2, Tv2, n2, d2, v2);
    if InternalErrId > 0 then
      Exit;
    case DistComponent of
      gdRatioSynNonsyn, gdRatioNonsynSyn:  d := d1/d2;
      gdDiffSynNonsyn, gdDiffNonsynSyn:
        begin
          d := d1 - d2;
          if VarType = gdAnalyticalVar then
             v := v1 + v2;
        end;
      gdPurifySelTest, gdNeutralEvolTest, gdPosSelTest:
        begin
          d := d1 - d2;
          if VarType = gdAnalyticalVar then
            v := v1 + v2;
        end;
    end;
  end;
begin
  Seq1 := FSequences[i];
  Seq2 := FSequences[j];

  // initialize
  S  := 0;  N  := 0;
  sTs := 0; nTs := 0;
  sTv := 0; nTv := 0;

  IterateLen := ComputeIterateLen;
  MultInt := 1;
  for site:=0 to IterateLen-1 do
  begin
    CodonI := Seq1[site];
    CodonJ := Seq2[site];

    with CodonInfo do
    begin
      if IsAmbiguousCodon(CodonI) or IsAmbiguousCodon(CodonJ) then
        Continue;
      if (AminoAcid[CodonI] = '*') or (AminoAcid[CodonJ] = '*') then
        Continue;

      if FFreqTable <> nil then
      begin
        MultInt := FFreqTable[Site];
        if MultInt < 1 then
          Continue;
      end;

      S := S + MultInt*(SynSites[CodonI] + SynSites[CodonJ]);
      N := N + MultInt*(NonsynSites[CodonI] + NonsynSites[CodonJ]);
      if CodonI <> CodonJ then
      begin
          sTs := sTs + MultInt*TsSynDiff[CodonI, CodonJ];
          nTs := nTs + MultInt*TsNonsynDiff[CodonI, CodonJ];
          sTv := sTv + MultInt*TvSynDiff[CodonI, CodonJ];
          nTv := nTv + MultInt*TvNonsynDiff[CodonI, CodonJ];
      end;
    end; // end of with CodonInfo
  end; // end of for every site in a pairwise comparison

  //Average numbers of SynSites and NonSyn sites
  S := S/2;
  N := N/2;

  Sd := (sTs+sTv);
  Nd := (nTs+nTv);

  // now estimate appropriate parameters
  if FDistPack.SelectionTestType <> gdNone then
  begin
    if SetTooSmallSErr(S)    or SetTooSmallSErr(N) or
       SetPSMoreThan1Err(Sd/S) or SetPNMoreThan1Err(Nd/N) then
         Exit;
    case FDistPack.SelectionTestType of
      gdExactTest,
      gdMcdonaldKreitmanTest:
        try
          N :=  S+N; // first, fix the total no of S and N sits
          S := Ceil(S);  // to make test conservative
          N := N - S;
          // Now we fix the total number of substitutions
          Sd := Ceil(Sd);  // to make the test conservative
          Nd := Floor(Nd); // to make the test conservative
          // only S, N, and Sd and Nd matter
          //if (Sd/S) < (Nd/N) then
          if (Sd/S) > (Nd/N) then
            Dist1 := 1   // this makes it a perfect one-tailed test
          else
            Dist1 := FisherExactTestOneTailed(Floor(Sd), Floor(S), Floor(Nd), Floor(N));
        except
          InternalErrId := HC_Fisher_s_Exact_Test_Has_Failed;
        end;
      gdZSelectionTest:
       case DistComponent of
         gdPurifySelTest:  DeriveEstimate(sTs, sTv, S, nTs, nTv, N, Dist1, Var1);
         gdNeutralEvolTest,  // n - s
         gdPosSelTest:     DeriveEstimate(nTs, nTv, N, sTs, sTv, S, Dist1, Var1);
       end;
    end;
    Exit;
  end;

  // normal distances
  case DistComponent of
    gdNoOfSynSites:    Dist1 := S;
    gdNoOfNonsynSites: Dist1 := N;
    gdSynOnly:        if    SetTooSmallSErr(S) or SetPSMoreThan1Err((sTs+sTv)/S) then Exit
                      else  ApplyCorrection(sTs, sTv, S, Dist1, Var1);
    gdNonsynOnly:     if    SetTooSmallSErr(N) or SetPNMoreThan1Err((nTs+nTv)/N) then Exit
                      else  ApplyCorrection(nTs, nTv, N, Dist1, Var1);
    gdRatioSynNonsyn: if    SetTooSmallSErr(S) or SetTooSmallSErr(N) then Exit
                      else  DeriveEstimate(sTs, sTv, S, nTs, nTv, N, Dist1, Var1);
    gdRatioNonsynSyn: if    SetTooSmallSErr(S) or SetTooSmallSErr(N) then Exit
                      else  DeriveEstimate(nTs, nTv, N, sTs, sTv, S, Dist1, Var1);
    gdDiffSynNonsyn:  if    SetTooSmallSErr(S) or SetTooSmallSErr(N) then Exit
                      else  DeriveEstimate(sTs, sTv, S, nTs, nTv, N, Dist1, Var1);
    gdDiffNonsynSyn:  if    SetTooSmallSErr(S) or SetTooSmallSErr(N) then Exit
                      else  DeriveEstimate(nTs, nTv, N, sTs, sTv, S, Dist1, Var1);
  end;
end;

procedure TSynNonsynDist.LiWuLouTypeAnalysis(i,j: Integer);
var
  Seq1, Seq2: PAnsiChar;
  CodonI, CodonJ:  TCodonMap;
  k, Site, Pos, DegenI, DegenJ: Integer;

    // They are estimated from the CodonInformation below
  Li:            array[0..4] of Double;  // index is over all the degeneracies; use 0/2/and4.
  Pi, Qi:        array[0..4] of Double;
  Ai, Bi, Ki:    array[0..4] of Double;
  VAi, VBi, VKi: array[0..4] of Double;

    // For a given codon codon
  LxPos: array[0..2] of Integer;          // Site type at a given pos in x
  LyPos: array[0..2] of Integer;          // Site type at a given pos in y
  sTs, sTv, nTs, nTv: array[0..2] of Integer;  // needed for special handling of problematic codons
    // over all codons
  LiX: array[0..4] of Integer;  // index is over all the degeneracies; use 0/2/and4.
  LiY: array[0..4] of Integer;  // index is over all the degeneracies; use 0/2/and4.
  TsFullMat: array[0..4] of Double; // Keeps frequencies of p0  p2'N  p2  p2'S  p4
  TvFullMat: array[0..4] of Double; // Keeps frequencies of q0  q2'N  q2  q2'S  q4

    // keeps positions that are different
  DiffPos: array [0..2] of Integer;
  TvDiffFlag: Boolean;
  AlarmFlag: array[0..2] of Boolean;
  AddInt, NoOfDiff, Total, ThePos: Integer;
  a,b,c,e,TempMultiply: Double;

  i2N, i2S: Integer; // they are for Complex 2-fold sites

  procedure ComputeAandB(x: Integer; TheL: Double);
  begin
    a := (1-2*Pi[x]-Qi[x]);
    b := (1-2*Qi[x]);
    if SetK2FailedErr(a, b) then
      Exit;
    a := 1/a; b := 1/b;
    Ai[x]  := 0.5*ln(a) - 0.25*ln(b);
    Bi[x]  := 0.5*ln(b);
    Ki[x]  := Ai[x] + Bi[x];
    if VarType = gdAnalyticalVar then
    begin
      c := (a-b)/2; e:= b + c;
      VAi[x] := DeltaVar([a, Pi[x], c, Qi[x], TheL]);
      VBi[x] := b*b*Qi[x]*(1-Qi[x])/TheL;
      VKi[x] := DeltaVar([a, Pi[x], e, Qi[x], TheL]);
    end;
  end;
  procedure Apply_0_FoldCorrection(var d, v: Double);
  begin
    d := Bi[0]+Ai[0];
    if VarType = gdAnalyticalVar then
      v := VKi[0];
  end;
  procedure Apply_4_FoldCorrection(var d, v: Double);
  begin
    d := Bi[4]+Ai[4];
    if VarType = gdAnalyticalVar then
      v := VKi[4];
  end;
    // Multiple hit correction of syn subst.
  procedure ApplySynCorrection(var d, v: Double);
  begin
    case DistModel of
      gdLWL:
        d := 3*(Li[2]*Ai[2] + Li[4]*Ki[4])/(Li[2]  + 3*Li[4]);
      gdPBL, gdComeronKumar:
        d := Bi[4] + ((Li[2]+Li[3])*Ai[2] + Li[4]*Ai[4])/(Li[2]+Li[3]+Li[4]);
    end;

    if d < 0 then
    begin
     InternalErrId := HC_Kimura_1980_Distance_Correction_Failed;
     InternalErrStr:= 'Kimura 2-parameter (1980) correction failed.';
    end;

    if VarType = gdAnalyticalVar then
      case DistModel of
        gdLWL:
          begin
            v := (VAi[2]*Li[2]*Li[2] + VKi[4]*Li[4]*Li[4]);
            v := 9*v/(  Li[2] + 3*Li[4])/(  Li[2] + 3*Li[4]);
          end;
        gdPBL, gdComeronKumar:
          begin
            v := VBi[4] + (VAi[2]*(Li[2]+Li[3])*(Li[2]+Li[3]) + VAi[4]*Li[4]*Li[4])/(Li[2]+Li[3]+Li[4])/(Li[2]+Li[3]+Li[4]);
            a := 1/(1-2*Pi[4]-Qi[4]); b := 1/(1-2*Qi[4]); c := (a-b)/2;
            v := v - b*Qi[4]*(2*a*Pi[4] - c*(1-Qi[4]))/(Li[2]+Li[3]+Li[4]);
          end;
      end;
  end;
    // Multiple hit correction of nonsyn subst.
  procedure ApplyNonSynCorrection(var d, v: Double);
  begin
    case DistModel of
      gdLWL:  d := 3*(Li[2]*Bi[2] + Li[0]*Ki[0])/(2*Li[2]  + 3*Li[0]);
      gdPBL, gdComeronKumar:
          d := Ai[0] + (Li[0]*Bi[0] + (Li[2]+Li[3])*Bi[2])/(Li[0]+Li[2]+Li[3]);
    end;

    if d < 0 then
    begin
     InternalErrId := HC_Kimura_1980_Distance_Correction_Failed;
     InternalErrStr:= 'Kimura 2-parameter (1980) correction failed.';
    end;

    if VarType = gdAnalyticalVar then
      case DistModel of
        gdLWL:
          begin
            v := (VBi[2]*Li[2]*Li[2] + VKi[0]*Li[0]*Li[0]);
            v := 9*v/(2*Li[2] + 3*Li[0])/(2*Li[2] + 3*Li[0]);
          end;
        gdPBL, gdComeronKumar:
          begin
            v := VAi[0] + (VBi[0]*Li[0]*Li[0] + VBi[2]*(Li[2]+Li[3])*(Li[2]+Li[3]))/(Li[0]+Li[2])/(Li[0]+Li[2]);
            a := 1/(1-2*Pi[0]-Qi[0]); b := 1/(1-2*Qi[0]); c := (a-b)/2;
            v := v - b*Qi[0]*(2*a*Pi[0] - c*(1-Qi[0]))/(Li[0]+Li[2]+Li[3]);
          end;
      end;
  end;

  procedure DeriveEstimate(var d, v: Double);
  var
    d1: Double = 0;
    v1: Double = 0;
    d2: Double = 0;
    v2: Double = 0;
  begin
    case DistComponent of
      gdRatioSynNonsyn, gdDiffSynNonsyn,
      gdPurifySelTest:
        begin
          if InternalErrId > 0 then
            exit;
          ApplySynCorrection(d1, v1);
          if InternalErrId > 0 then
            exit;
          ApplyNonsynCorrection(d2, v2);
        end;
      gdRatioNonsynSyn, gdDiffNonsynSyn,
      gdNeutralEvolTest,
      gdPosSelTest:
        begin
          if InternalErrId > 0 then
            exit;
          ApplyNonsynCorrection(d1, v1);
          if InternalErrId > 0 then
            exit;
          ApplySynCorrection(d2, v2);
        end;
    end;
    if InternalErrId > 0 then
       exit;
    //--- now do the computation
    case DistComponent of
      gdRatioSynNonsyn, gdRatioNonsynSyn:   d := d1/d2;
      gdDiffSynNonsyn, gdDiffNonsynSyn,
      gdPurifySelTest, gdNeutralEvolTest, gdPosSelTest:
        begin
          d := d1 - d2;
          if VarType = gdAnalyticalVar then
            v := v1 + v2;
        end;
    end;
  end;
begin
  i2N := 1;
  i2S := 3;
  Seq1 := FSequences[i];
  Seq2 := FSequences[j];
  for i:=0 to 2 do
    sTs[i] := 0; nTs[i] := 0;  sTv[i] := 0;  nTv[i] := 0;

  IterateLen := ComputeIterateLen;

  // initialize
  for k:=0 to 4 do
  begin
    Li[k] := 0.0; LiX[k]:= 0;  LiY[k] := 0;
    Pi[k] := 0.0; Qi[k] := 0;  Ai[k] := 0; Bi[k] := 0;
  end;

  for i:=0 to 4 do
  begin
    TsFullMat[i] := 0; TvFullMat[i] := 0;
  end;

  AddInt := 1;

  for site:=0 to IterateLen-1 do
  begin
    CodonI := Seq1[site];
    CodonJ := Seq2[site];

    if FFreqTable <> nil then
    begin
      AddInt := FFreqTable[Site];
      if AddInt < 1 then
        Continue;
    end;

    with CodonInfo do
    begin
      if (CodonI > #63) or (CodonJ > #63) then
        Continue;
      if (AminoAcid[CodonI] = '*') or (AminoAcid[CodonJ] = '*') then
        Continue;
      for Pos:=0 to 2 do  // this is just to get the LiX and LiY
      begin
        DegenI := ExtendedRedundancy[CodonI,Pos];
        DegenJ := ExtendedRedundancy[CodonJ,Pos];
        if (DegenI < 0) or (DegenJ < 0) then
          SetErrorStatus(HC_Unexpected_Error, 'Invalid degeneracy values.');

        // remember the degeneracy for each site
        LxPos[Pos] := DegenI;
        LyPos[Pos] := DegenJ;

        // add towards the overall degeneracy
        LiX[DegenI] := LiX[DegenI] + AddInt;
        LiY[DegenJ] := LiY[DegenJ] + AddInt;
      end;  // end for al three positions

      if CodonI <> CodonJ then // they are different
      begin
        NoOfDiff := 0;
        for i:= 0 to 2 do
          if IsDifferent(CodonI, CodonJ, i) then
          begin
            DiffPos[NoOfDiff] := i;
            Inc(NoOfDiff);
          end;

        for i:=0 to 2 do   AlarmFlag[i] := False;
        for i:=0 to NoOfDiff-1 do  // Note that it is done only for polymorphic sites
          if ComplexDegen(LxPos[DiffPos[i]]) or ComplexDegen(LyPos[DiffPos[i]]) then
            AlarmFlag[i] := True;

        // so alarm flag is only ON if a change is seen at a site that is
        // not conventional 0, 2, or 4-fold degenerate
        if not(AlarmFlag[0] or AlarmFlag[1] or AlarmFlag[2]) then
        begin
          for i:=0 to NoOfDiff-1 do
          begin
            ThePos := DiffPos[i];
            DegenI := LxPos[ThePos]; DegenJ := LyPos[ThePos];
            TvDiffFlag := IsTvDiff(CodonI, CodonJ, ThePos);
            if TvDiffFlag then
            begin
              TvFullMat[DegenI] := TvFullMat[DegenI] + AddInt;
              TvFullMat[DegenJ] := TvFullMat[DegenJ] + AddInt;
            end
            else
            begin
              TsFullMat[DegenI] := TsFullMat[DegenI] + AddInt;
              TsFullMat[DegenJ] := TsFullMat[DegenJ] + AddInt;
            end;
          end;
        end
        else
        begin // special handling is needed
          case NoOfDiff of
            1: Total := GetOnePathwayCounts(CodonI, CodonJ, DiffPos[0], LxPos, LyPos, sTs, nTs, sTv, nTv);
            2: Total := GetTwoPathwayCounts(CodonI, CodonJ, DiffPos, LxPos, LyPos, sTs, nTs, sTv, nTv);
            3: Total := GetThreePathwayCounts(CodonI, CodonJ, LxPos, LyPos, sTs, nTs, sTv, nTv);
          end;

          for i:=0 to NoOfDiff-1 do
          begin
            ThePos := DiffPos[i];
            DegenI := LxPos[ThePos];  DegenJ := LyPos[ThePos];
            if not AlarmFlag[i] then  // treat the site as if it is OK
            begin
              if IsTvDiff(CodonI, CodonJ, ThePos) then
              begin
                TvFullMat[DegenI] := TvFullMat[DegenI] + AddInt;
                TvFullMat[DegenJ] := TvFullMat[DegenJ] + AddInt;
              end
              else
              begin
                TsFullMat[DegenI] := TsFullMat[DegenI] + AddInt;
                TsFullMat[DegenJ] := TsFullMat[DegenJ] + AddInt;
              end;
            end
            else if Total > 0 then  // now the problem handling system if somepathways seen
            begin
              Total := sTs[ThePos]+ nTs[ThePos] + sTv[ThePos] + nTv[ThePos];

              if Total > 0 then
                TempMultiply := AddInt/Total
              else
                SetErrorStatus(HC_Unexpected_Error, 'Internal program exception.');

              if not ComplexDegen(DegenI) then  // conventional site
              begin
                TsFullMat[DegenI] := TsFullMat[DegenI] + (sTs[ThePos]+nTs[ThePos])*TempMultiply;
                TvFullMat[DegenI] := TvFullMat[DegenI] + (sTv[ThePos]+nTv[ThePos])*TempMultiply;
              end
              else // 2' then
              begin
                TsFullMat[i2N] := TsFullMat[i2N] + nTs[ThePos]*TempMultiply; // Nonsyn Ts type
                TsFullMat[i2S] := TsFullMat[i2S] + sTs[ThePos]*TempMultiply; // Syn Ts type
                TvFullMat[i2N] := TvFullMat[i2N] + nTv[ThePos]*TempMultiply; // Nonsyn Tv type
                TvFullMat[i2S] := TvFullMat[i2S] + sTv[ThePos]*TempMultiply; // Syn Tv type
              end;

              if not ComplexDegen(DegenJ) then // conventional site
              begin
                TsFullMat[DegenJ] := TsFullMat[DegenJ] + (sTs[ThePos]+nTs[ThePos])*TempMultiply;
                TvFullMat[DegenJ] := TvFullMat[DegenJ] + (sTv[ThePos]+nTv[ThePos])*TempMultiply;
              end
              else // 2' then
              begin
                TsFullMat[i2N] := TsFullMat[i2N] + nTs[ThePos]*TempMultiply; // Nonsyn Ts type
                TsFullMat[i2S] := TsFullMat[i2S] + sTs[ThePos]*TempMultiply; // Syn Ts type
                TvFullMat[i2N] := TvFullMat[i2N] + nTv[ThePos]*TempMultiply; // Nonsyn Tv type
                TvFullMat[i2S] := TvFullMat[i2S] + sTv[ThePos]*TempMultiply; // Syn Tv type
              end;
            end;
          end; // end for all the diffs in special handling
        end; // special handling over here
      end; // end if codonI <> CodonJ
    end; // end of with CodonInfo
  end;   // end of for every site in a pairwise comparison

  Li[0] := (LiX[0]+LiY[0])/2;
  Li[1] := 0;
  Li[2] := (LiX[2]+LiY[2])/2;
  Li[3] := (LiX[i2S]+LiY[i2S]+LiX[i2N]+LiY[i2N])/2; // such that 1 and 3 are under 3
  Li[4] := (LiX[4]+LiY[4])/2;

  case DistComponent of
    gdNoOf0FoldSites: begin dist1 := Li[0]; Exit; end;
    gdNoOf4FoldSites: begin dist1 := Li[4]; Exit; end;
  end;

  // Note here that all differences and all Li's are multiplied by 2
  // it is OK for proportion but NOT OK for variances, so be careful
  for k:=0 to 4 do   //Why are we doing this multiple times?  It doesn't change with each loop, should Pi[0] be Pi[k]?? etc... -Nick
  begin
    Pi[0]  := 0;   Qi[0] := 0;
    Ai[0]  := 0;   Bi[0] := 0;
    VAi[0] := 0;   VBi[0] := 0;
  end;

  // if LWL or PBL method or no ambiguous sites then it is the same as the Li's method
  if (DistModel = gdLWL) or (DistModel = gdPBL) then
  begin
    if Li[0] > 0 then // at zerofold sites
    begin
      Pi[0] := TsFullMat[0]/2/Li[0];
      Qi[0] := TvFullMat[0]/2/Li[0];
      ComputeAandB(0,Li[0]);
    end;
    if Li[4] > 0 then // at fourfold sites
    begin
      Pi[4] := TsFullMat[4]/2/Li[4];
      Qi[4] := TvFullMat[4]/2/Li[4];
      ComputeAandB(4,Li[4]);
    end;
    Li[2] := Li[2]+Li[3];  // at 2-fold sites; it uses
    Li[3] := 0;            // force Li[3] = 0;
    if Li[2] > 0 then
    begin
      Pi[2] := (TsFullMat[2]+TsFullMat[i2S]+TvFullMat[i2S])/2/Li[2]; // assumes that all    syn=Ts
      Qi[2] := (TvFullMat[2]+TvFullMat[i2N]+TsFullMat[i2N])/2/Li[2]; // assumes that all nonsyn=Tv
      ComputeAandB(2,Li[2]);
    end;
  end
  else // Comeron-Kumar method
  begin
    if (Li[0]+Li[3]) > 0 then // at zerofold sites
    begin
      Pi[0] := (TsFullMat[0]+TsFullMat[i2N])/2/(Li[0]+Li[3]);  // p0 + p'2N
      if Li[0] > 0 then
        Qi[0] := TvFullMat[0]/2/Li[0];
      ComputeAandB(0,Li[0]);  // so we have Ai[0] and Bi[0]
    end;
    if (Li[4]+Li[3]) > 0 then // at fourfold sites
    begin
      if Li[4] > 0 then Pi[4] := TsFullMat[4]/2/Li[4];
      Qi[4] := (TvFullMat[4]+TvFullMat[i2S])/2/(Li[4]+Li[3]);  // q4 + q'2S
      ComputeAandB(4,Li[4]);  // so we have Ai[4] and Bi[4]
    end;
    if (Li[2]+Li[3]) > 0 then // at 2-fold sites;
    begin
      Pi[2] := (TsFullMat[2]+TsFullMat[i2S])/2/(Li[2]+Li[3]);  // p2 + p'2S
      Qi[2] := (TvFullMat[2]+TvFullMat[i2N])/2/(Li[2]+Li[3]);  // q2 + q'2N
      ComputeAandB(2,Li[2]+Li[3]);  // so we have Ai[2] and Bi[2]
    end;
  end;

  // now estimate appropriate parameters
  case DistComponent of
    gdSynOnly:        ApplySynCorrection(Dist1, Var1);
    gdNonsynOnly:     ApplyNonSynCorrection(Dist1, Var1);
    gdNonsyn0Fold:    Apply_0_FoldCorrection(Dist1, Var1);
    gdSyn4Fold:       Apply_4_FoldCorrection(Dist1, Var1);
    gdRatioSynNonsyn, gdRatioNonsynSyn:  DeriveEstimate(Dist1, Var1);
    gdNeutralEvolTest,  // s <> n
    gdPosSelTest,       // n > s
    gdPurifySelTest,    // n < s
    gdDiffSynNonsyn,  gdDiffNonsynSyn:   DeriveEstimate(Dist1, Var1);
  end;
end;

function TSynNonsynDist.GetOnePathwayCounts(CodonI, CodonJ: TCodonMap; Pos: Integer; LxPos, LyPos: array of Integer; var sTs, nTs, sTv, nTv: array of Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i:=0 to 2 do
  begin
    sTs[i] := 0; nTs[i] := 0;  sTv[i] := 0;  nTv[i] := 0;
  end;

  if not (ComplexDegen(LxPos[Pos]) or ComplexDegen(LyPos[Pos])) then
    Exit;

  with CodonInfo do
  begin
    if IsTsDiff(CodonI, CodonJ, Pos) then
    begin
      if AminoAcid[CodonI] = AminoAcid[CodonJ] then sTs[Pos] := 1 else nTs[Pos] := 1;
    end
    else
    begin
      if AminoAcid[CodonI] = AminoAcid[CodonJ] then sTv[Pos] := 1 else nTv[Pos] := 1;
    end;
  end;
  Result := 1;
end;

function TSynNonsynDist.GetTwoPathwayCounts(CodonI, CodonJ: TCodonMap; DiffPos, LxPos, LyPos: array of Integer; var sTs, nTs, sTv, nTv: array of Integer): Integer;
var
  i, ThePos: Integer;
  NewY, OldX, OldY: TCodonMap;
  sTs1, sTv1, nTs1, nTv1: array[0..2] of Integer;  // needed for special handling of problematic codons
begin
  Result := 0; // contains number of pathways
  for i:=0 to 2 do
  begin
    sTs[i] := 0; nTs[i] := 0;  sTv[i] := 0;  nTv[i] := 0;
    sTs1[i] := 0; nTs1[i] := 0;  sTv1[i] := 0;  nTv1[i] := 0;
  end;

  OldX := CodonI;
  OldY := CodonJ;
  with CodonInfo do
  begin
    for i:= 0 to 1 do
    begin
      ThePos := DiffPos[i];
      NewY  := MutateTo(CodonI, CodonJ, ThePos);

      // OldX -> NewY -> OldY
      if AminoAcid[AnsiChar(NewY)] = '*' then
        Continue;

      Inc(Result);
         // OldX -> NewY change
      if ComplexDegen(LxPos[ThePos]) or ComplexDegen(LyPos[ThePos]) then
      begin // this part of the pathway will produce a 2' type change
        GetOnePathwayCounts(OldX, NewY, ThePos, LxPos, LyPos, sTs1, nTs1, sTv1, nTv1);
        sTs[ThePos] := sTs[ThePos] + sTs1[ThePos];
        sTv[ThePos] := sTv[ThePos] + sTv1[ThePos];
        nTs[ThePos] := nTs[ThePos] + nTs1[ThePos];
        nTv[ThePos] := nTv[ThePos] + nTv1[ThePos];
      end;

      // NewY -> OldY
      if i = 0 then        ThePos := DiffPos[1]
      else                 ThePos := DiffPos[0];
      if ComplexDegen(LxPos[ThePos]) or ComplexDegen(LyPos[ThePos]) then
      begin // therefore this part of the pathway will produce a 2' type change
        GetOnePathwayCounts(NewY, OldY, ThePos, LxPos, LyPos, sTs1, nTs1, sTv1, nTv1);
        sTs[ThePos] := sTs[ThePos] + sTs1[ThePos];
        sTv[ThePos] := sTv[ThePos] + sTv1[ThePos];
        nTs[ThePos] := nTs[ThePos] + nTs1[ThePos];
        nTv[ThePos] := nTv[ThePos] + nTv1[ThePos];
      end;
    end;
  end;
end;

function TSynNonsynDist.GetThreePathwayCounts(CodonI, CodonJ: TCodonMap; LxPos, LyPos:array of Integer; var sTs, nTs, sTv, nTv: array of Integer): Integer;
var
  NewY, OldX, OldY: TCodonMap;
  i, NoOfBottomPaths: Integer;
  sTs1, sTv1, nTs1, nTv1: array[0..2] of Integer;  // needed for special handling of problematic codons
  sTs2, sTv2, nTs2, nTv2: array[0..2] of Integer;  // needed for special handling of problematic codons
  PosDiff: array[0..1] of Integer;
  ThePos: Integer;
begin
  Result := 0;
  for i:=0 to 2 do
  begin
    sTs[i] := 0; nTs[i] := 0;  sTv[i] := 0;  nTv[i] := 0;
    sTs1[i] := 0; nTs1[i] := 0;  sTv1[i] := 0;  nTv1[i] := 0;
    sTs2[i] := 0; nTs2[i] := 0;  sTv2[i] := 0;  nTv2[i] := 0;
  end;

  OldX := CodonI;
  OldY := CodonJ;
  with CodonInfo do
  begin
    for i:= 0 to 2 do
    begin
      ThePos := i;
      NewY  := MutateTo(CodonI, CodonJ, ThePos);

      if AminoAcid[AnsiChar(NewY)] = '*' then
        Continue;

      // OldX -> NewY -> OldY system

      // First do the bottom ; Has Two substitutions
      // NewY -> OldY
      case i of
        0: begin PosDiff[0] := 1; PosDiff[1] := 2; end;
        1: begin PosDiff[0] := 0; PosDiff[1] := 2; end;
        2: begin PosDiff[0] := 0; PosDiff[1] := 1; end;
      end;
      NoOfBottomPaths := GetTwoPathwayCounts(NewY, OldY, PosDiff, LxPos, LyPos, sTs2, nTs2, sTv2, nTv2);
      if NoOfBottomPaths = 0 then  // then no substitutions in this side possible
        Continue;

      Result := Result + NoOfBottomPaths;

      // now add the contribution of the bottom part
      ThePos := PosDiff[0];
      if ComplexDegen(LxPos[ThePos]) or ComplexDegen(LyPos[ThePos]) then
      begin
        sTs[ThePos] := sTs[ThePos] + sTs2[ThePos];
        sTv[ThePos] := sTv[ThePos] + sTv2[ThePos];
        nTs[ThePos] := nTs[ThePos] + nTs2[ThePos];
        nTv[ThePos] := nTv[ThePos] + nTv2[ThePos];
      end;

      ThePos := PosDiff[1];
      if ComplexDegen(LxPos[ThePos]) or ComplexDegen(LyPos[ThePos]) then
      begin
        sTs[ThePos] := sTs[ThePos] + sTs2[ThePos];
        sTv[ThePos] := sTv[ThePos] + sTv2[ThePos];
        nTs[ThePos] := nTs[ThePos] + nTs2[ThePos];
        nTv[ThePos] := nTv[ThePos] + nTv2[ThePos];
      end;

      // OldX -> NewY (one substitution in the first position)
      ThePos := i;
      if ComplexDegen(LxPos[ThePos]) or ComplexDegen(LyPos[ThePos]) then
      begin
        GetOnePathwayCounts(OldX, NewY, ThePos, LxPos, LyPos, sTs1, nTs1, sTv1, nTv1);
        sTs[ThePos] := sTs[ThePos] + sTs1[ThePos]*NoOfBottomPaths;
        sTv[ThePos] := sTv[ThePos] + sTv1[ThePos]*NoOfBottomPaths;
        nTs[ThePos] := nTs[ThePos] + nTs1[ThePos]*NoOfBottomPaths;
        nTv[ThePos] := nTv[ThePos] + nTv1[ThePos]*NoOfBottomPaths;
      end;
    end;
  end;
end;

end.

