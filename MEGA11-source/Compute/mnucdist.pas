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

unit MNucDist;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  MSeqDistBase, MegaConsts;

type
  TNucDist = Class(TSeqDistBase)
  private
    FMonomorphic: array [0..3] of Integer;         // sites with identical nucleotides
       // for distance computation
    SubstMat: array[0..4, 0..4] of Integer; // 4+1 nucleotides
    nucMap:   array[0..4, 0..4] of Integer; // indexial mapping for above matrices
    nucf:     array[0..4] of Integer;

     // For simultaneous analysis
    PMat, QMat: PDistanceMatrix;
    fA,fT,fC,fG: array of double;

    function  ComputeIterateLen: Integer;                   // needed in some situations
    procedure SetMonomorphic(Index, Value: Integer);
    function  GetBaseFreq(Base, NSites: Integer): Double; // uses SubstMat

    // embryo analysis
    procedure ComputeEmbryoDist(NDiff, NSites: LongInt);

    // distance estimation procedures
    procedure ComputeNoOfDiff(NDiff, NSites: LongInt);
    procedure ComputePDist(NDiff, NSites: LongInt);
    procedure ComputeTestRandom(NDiff, NSites: LongInt);


    procedure ComputeCompDist(NDiff, NSites: LongInt; Seq1, Seq2: PAnsiChar);
    procedure ComputeDisparityIndex(NDiff, NSites: LongInt; Seq1, Seq2: PAnsiChar);
    procedure ComputeDisparityIndexTest(NDiff, NSites: LongInt; Seq1, Seq2: PAnsiChar);

    procedure ComputeJCDist(NDiff, NSites: LongInt);
    procedure ComputeJCGammaDist(NDiff, NSites: LongInt);
    procedure ComputeKimuraDist(NDiff, NSites: LongInt);
    procedure ComputeKimuraGammaDist(NDiff, NSites: LongInt);

    procedure ComputeTjmNeiDist(NDiff, NSites: LongInt);
    procedure ComputeTjmNeiGammaDist(NDiff, NSites: LongInt);              // new

    procedure ComputeTamuraDist(NDiff, NSites: LongInt);
    procedure ComputeTamuraGammaDist(NDiff, NSites: LongInt);              // new

    procedure ComputeTamNeiDist(NDiff, NSites: LongInt);
    procedure ComputeTamNeiGammaDist(NDiff, NSites: LongInt);

    procedure ComputeTjmNeiHeteroDist(NDiff, NSites: LongInt);             // new
    procedure ComputeTjmNeiGammaHeteroDist(NDiff, NSites: LongInt);        // new

    procedure ComputeTamuraHeteroDist(NDiff, NSites: LongInt);             // new
    procedure ComputeTamuraGammaHeteroDist(NDiff, NSites: LongInt);        // new

    procedure ComputeTamNeiHeteroDist(NDiff, NSites: LongInt);             // new
    procedure ComputeTamNeiGammaHeteroDist(NDiff, NSites: LongInt);        // new

    procedure ComputeTamKumLogDetDist(NDiff, NSites: LongInt);             // new

    // Simultaneous Estimation
    procedure ComputeTamNeiDistSimultaneous(D: PDistanceMatrix; IncProgress: boolean);
    procedure ComputeTamNeiGammaDistSimultaneous(D: PDistanceMatrix; IncProgress: boolean);
    procedure ComputeTamNeiHeteroDistSimultaneous(D: PDistanceMatrix; IncProgress: boolean);             // new
    procedure ComputeTamNeiGammaHeteroDistSimultaneous(D: PDistanceMatrix; IncProgress: boolean);        // new

    procedure AddProgressIncrement10;

  protected

    procedure DoPairwiseAnalysis(i,j: Integer); override;
    procedure DoSimultaneousAnalysis(D: PDistanceMatrix; IncProgress: boolean); override;
    function  NucToInt(nuc : AnsiChar): integer;

  public
    constructor Create;
    destructor Destroy; override;

    function  ComputeDistances: Boolean; override;
    function  ComputeDistancesSE: Boolean; override;

    property Monomorphic[Index: Integer]: Integer write SetMonomorphic;

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
  SysUtils, Math, Forms, MegaUtils, MDistPack, ErrorMessages_HC, dateutils;

var
  T,C,A,G:Integer;

constructor TNucDist.Create;
var
  i, j, Index: Integer;
begin
  inherited Create;
  FMonomorphic[A] := 0;
  FMonomorphic[C] := 0;
  FMonomorphic[G] := 0;
  FMonomorphic[T] := 0;

  Index := 0;
  for i:=0 to 4 do
    for j:=0 to i do
    begin
       NucMap[i][j] := Index;
       NucMap[j][i] := Index;
       Inc(Index);
    end;
end;

// TNucleotideDist destructor
destructor TNucDist.Destroy;
begin
  inherited Destroy;
end;

// speed up the computation
procedure TNucDist.SetMonomorphic(Index, Value: Integer);
begin
  if (Index = A) or (Index = T) or (Index = C) or (Index = G) then
    FMonomorphic[Index] := Value;
end;

// get the length of iteration
function TNucDist.ComputeIterateLen: Integer;
begin
  if FFreqTable <> nil then
    Result := FFreqTableLen
  else
    Result := FNoOfSites - (FMonomorphic[A]-FMonomorphic[C]-FMonomorphic[G]-FMonomorphic[T]);
end;

function TNucDist.GetBaseFreq(Base, NSites: Integer): Double;
var
  i: Integer;
begin
  Result := 0;
  for i:=0 to 3 do
    if Base = i then
      Result := Result + 2*SubstMat[Base][Base]
    else
      Result := Result +   SubstMat[Base][i];
  Result := Result/2/NSites;
end;

function TNucDist.ComputeDistances: Boolean;
begin
  result := inherited ComputeDistances;
end;

function TNucDist.ComputeDistancesSE: Boolean;
begin
  result := inherited ComputeDistancesSE;
end;

//----- Computes the pairwise between taxa
procedure TNucDist.DoPairwiseAnalysis(i, j: Integer);
var
  Seq1, Seq2: PAnsiChar;
  AddInt, NSame, NDiff, Site: Integer;
  NucI, NucJ, k, l, NSites: Integer;

  procedure MakeMatSymmetrical;
  var
    mms_k, mms_l: Integer;
  begin
    for mms_k:=0 to 4 do
      for mms_l:=0 to mms_k-1 do
      begin
        SubstMat[mms_k][mms_l] := SubstMat[mms_k][mms_l] + SubstMat[mms_l][mms_k];
        SubstMat[mms_l][mms_k] := SubstMat[mms_k][mms_l];
      end;
  end;

begin
  InternalErrId := 0;
  InternalErrStr:= EmptyStr;

  Seq1 := FSequences[i];
  Seq2 := FSequences[j];

  // initialize
  IterateLen := ComputeIterateLen;

  for k:=0 to 4 do
    for l:=0 to 4 do
      SubstMat[k,l] := 0;

  for k:=0 to 3 do
    nucf[k] := 0;

  AddInt := 1;
  for Site:=0 to IterateLen-1 do
  begin
    NucI := Integer(Seq1[site]);
    NucJ := Integer(Seq2[site]);

    if (NucI > 4) or (NucI < 0) or (NucJ > 4) or (NucJ < 0) then
    begin
      InternalErrId := 1;
      InternalErrStr := 'Improperly reading in FSequences.';
      exit;
    end;

    if FFreqTable <> nil then
      AddInt := FFreqTable[Site];
    SubstMat[NucI][NucJ] := SubstMat[NucI][NucJ] + AddInt;
    if DistModel = gdTestRandom then
    begin
      Inc(nucf[NucI], 1); //AddInt);
      Inc(nucf[NucJ], 1); //AddInt);
    end;
  end;

  // update variables
  NSame := SubstMat[A][A] + SubstMat[T][T] + SubstMat[C][C] + SubstMat[G][G];
  NDiff := SubstMat[A][T] + SubstMat[T][A] +
           SubstMat[A][C] + SubstMat[C][A] +
           SubstMat[G][T] + SubstMat[T][G] +
           SubstMat[G][C] + SubstMat[C][G] +
           SubstMat[A][G] + SubstMat[G][A] +
           SubstMat[T][C] + SubstMat[C][T];
  NSites := NDiff + NSame;

  if DistComponent = gdCommonSites then
  begin
    Dist1 := NSites;
    Exit;
  end;

  if NSites < 1 then
  begin
    InternalErrId := HC_No_Common_Sites;
    InternalErrStr:= 'No common sites were found for the sequence pair ('+
                      IntToStr(i+1)+','+IntToStr(j+1)+').' + NoCommonSitesStr;
  end
  else
    case DistModel of
      gdTestRandom:
          begin
            MakeMatSymmetrical;
            ComputeTestRandom(NDiff, NSites);
          end;
       gdAlleleFreqDist:
          ComputeCompDist(NDiff, NSites, Seq1, Seq2);
      gdNoOfDiff:
          begin
            if FDistPack.DoesContain(gdDisparityIndexTest) then
               ComputeDisparityIndexTest(NDiff, NSites, Seq1, Seq2)
            else if FDistPack.DoesContain(gdDisparityIndex) then
              ComputeDisparityIndex(NDiff, NSites, Seq1, Seq2)
            else if FDistPack.DoesContain(gdCompositionDistance) then
              ComputeCompDist(NDiff, NSites, Seq1, Seq2)
            else
            begin
              MakeMatSymmetrical;
              ComputeNoOfDiff(NDiff, NSites);
            end;
          end;
      gdPropDist:
          begin
            MakeMatSymmetrical;
            ComputePDist(NDiff, NSites);
          end;
      gdJukesCantor:
           if HasGamma then ComputeJCGammaDist(NDiff, NSites)
                       else ComputeJCDist(NDiff, NSites);
      gdTajimaNei:
          begin
            if not IsHetero then
              MakeMatSymmetrical;
            if IsHetero then
            begin
              if HasGamma then ComputeTjmNeiGammaHeteroDist(NDiff, NSites)
                          else ComputeTjmNeiHeteroDist(NDiff, NSites)
            end
            else
              if HasGamma then ComputeTjmNeiGammaDist(NDiff, NSites)
                          else ComputeTjmNeiDist(NDiff, NSites);
          end;
      gdKimura2para:
          begin
            MakeMatSymmetrical;
            if HasGamma then ComputeKimuraGammaDist(NDiff, NSites)
                        else ComputeKimuraDist(NDiff, NSites);
          end;
      gdTamura:
          begin
            if not IsHetero then
              MakeMatSymmetrical;
            if IsHetero then
            begin
              if HasGamma then ComputeTamuraGammaHeteroDist(NDiff, NSites)
                          else ComputeTamuraHeteroDist(NDiff, NSites)
            end
            else
              if HasGamma then ComputeTamuraGammaDist(NDiff, NSites)
                          else ComputeTamuraDist(NDiff, NSites);
          end;
{
      gdGlobalPara:
          if not FHasParameters then
          begin
            TK02Parameter;
            if Dist1 > 0 then VTK02;
          end
          else
            TK02ML;
}
      gdTamuraNei:
          begin
            if not IsHetero then
              MakeMatSymmetrical;
            if IsHetero then
              if HasGamma then ComputeTamNeiGammaHeteroDist(NDiff, NSites)
                          else ComputeTamNeiHeteroDist(NDiff, NSites)
            else
              if HasGamma then ComputeTamNeiGammaDist(NDiff, NSites)
                          else ComputeTamNeiDist(NDiff, NSites);
          end;
      gdLogDet:
        ComputeTamKumLogDetDist(NDiff, NSites);
      gdEmbryoWhole,
      gdEmbryoExpression,
      gdEmbryoWtd: // expression distance
            ComputeEmbryoDist(NDiff, NSites);
     end;

  if InternalErrId > 0 then
  begin
    Dist1 := InvalidDistValue;
    SetErrorStatus(InternalErrId, InternalErrStr);
  end;
end;

//=========== NEW STUFF
function TNucDist.NucToInt(nuc : AnsiChar): integer;
begin
  case nuc of
    'A' : Result := 0;
    'C' : Result := 1;
    'G' : Result := 2;
    'T', 'U' : Result := 3;
  end;
end;

procedure TNucDist.DoSimultaneousAnalysis(D: PDistanceMatrix; IncProgress: boolean);
var
  Seq1, Seq2: PAnsiChar;
  AddInt, NSame, NDiff, Site: Integer;
  NucI, NucJ, i,j,k, l, NSites: Integer;
  NumComparisons: LongInt;
begin
  InternalErrId := 0;
  InternalErrStr:= EmptyStr;

  setlength(fA, FNoOfSeqs);
  setlength(fT, FNoOfSeqs);
  setlength(fC, FNoOfSeqs);
  setlength(fG, FNoOfSeqs);

  PMat := nil;
  QMat := nil;
  try
    PMat := CreateDistanceMatrix(FNoOfSeqs);
    QMat := CreateDistanceMatrix(FNoOfSeqs);
    LastUpdateTime := Now;
    if ShowInProgressBar then
      NumComparisons := (FNoOfSeqs * (FNoOfSeqs - 1) div 2);
    for i:= 1 to FNoOfSeqs-1 do
    begin
      if ShowInProgressBar then
        if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
          UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));

      for j:= 0 to i-1 do
      begin
        Seq1 := FSequences[i];
        Seq2 := FSequences[j];

        IterateLen := ComputeIterateLen;

        for k:=0 to 4 do
          for l:=0 to 4 do
            SubstMat[k,l] := 0;

        AddInt := 1;
        for Site:=0 to IterateLen-1 do
        begin
          NucI := Integer(Seq1[site]);
          NucJ := Integer(Seq2[site]);

          if FFreqTable <> nil then
            AddInt := FFreqTable[Site];
           SubstMat[NucI][NucJ] := SubstMat[NucI][NucJ] + AddInt;
        end;
{
        SubstMat[A][A] := substMat[A][A] + FMonomorphic[A];
        SubstMat[C][C] := substMat[C][C] + FMonomorphic[C];
        SubstMat[G][G] := substMat[G][G] + FMonomorphic[G];
        SubstMat[T][T] := substMat[T][T] + FMonomorphic[T];
}
        NSame := SubstMat[A][A] + SubstMat[T][T] + SubstMat[C][C] + SubstMat[G][G];
        NDiff := SubstMat[A][T] + SubstMat[T][A] +
                 SubstMat[A][C] + SubstMat[C][A] +
                 SubstMat[G][T] + SubstMat[T][G] +
                 SubstMat[G][C] + SubstMat[C][G] +
                 SubstMat[A][G] + SubstMat[G][A] +
                 SubstMat[T][C] + SubstMat[C][T];
        NSites := NDiff + NSame;

        if DistComponent = gdCommonSites then
          D[i][j] := NSites;

        if NSites < 1 then
        begin
          InternalErrId := HC_No_Common_Sites;
          InternalErrStr:= 'No common sites found for the sequence pair ('+
                      IntToStr(i+1)+','+IntToStr(j+1)+').' + NoCommonSitesStr;
          Exit;
        end
        else
        begin
          PMat[i,j] := (SubstMat[A][G] +SubstMat[G][A])/NSites;
          PMat[j,i] := (SubstMat[T][C] +SubstMat[C][T])/NSites;
          QMat[i,j] := (SubstMat[A][T] +SubstMat[T][A]
                       +SubstMat[A][C] +SubstMat[C][A]
                       +SubstMat[G][T] +SubstMat[T][G]
                       +SubstMat[G][C] +SubstMat[C][G])/NSites;

          fA[i] := (SubstMat[A][A] +SubstMat[A][T] +SubstMat[A][C] +SubstMat[A][G])/NSites;
          fT[i] := (SubstMat[T][A] +SubstMat[T][T] +SubstMat[T][C] +SubstMat[T][G])/NSites;
          fC[i] := (SubstMat[C][A] +SubstMat[C][T] +SubstMat[C][C] +SubstMat[C][G])/NSites;
          fG[i] := (SubstMat[G][A] +SubstMat[G][T] +SubstMat[G][C] +SubstMat[G][G])/NSites;
          if (i = 1) and (j = 0) then
          begin
            fA[0] := (SubstMat[A][A] +SubstMat[T][A] +SubstMat[C][A] +SubstMat[G][A])/NSites;
            fT[0] := (SubstMat[A][T] +SubstMat[T][T] +SubstMat[C][T] +SubstMat[G][T])/NSites;
            fC[0] := (SubstMat[A][C] +SubstMat[T][C] +SubstMat[C][C] +SubstMat[G][C])/NSites;
            fG[0] := (SubstMat[A][G] +SubstMat[T][G] +SubstMat[C][G] +SubstMat[G][G])/NSites;
          end;
        end;
      end;
    end;
    if DistComponent = gdCommonSites then
      Exit;

    pA := 0;
    pT := 0;
    pC := 0;
    pG := 0;
    for i:= 0 to FNoOfSeqs-1 do
    begin
      pA := pA +fA[i];
      pT := pT +fT[i];
      pC := pC +fC[i];
      pG := pG +fG[i];
    end;
    pA := pA/FNoOfSeqs;
    pT := pT/FNoOfSeqs;
    pC := pC/FNoOfSeqs;
    pG := pG/FNoOfSeqs;
    pR := pA+pG;
    pY := pT+pC;

    case DistModel of
      gdMCL, gdTamuraNei:
          begin
            if IsHetero then
              if HasGamma then ComputeTamNeiGammaHeteroDistSimultaneous(D, IncProgress)
                          else ComputeTamNeiHeteroDistSimultaneous(D, IncProgress)
            else
              if HasGamma then ComputeTamNeiGammaDistSimultaneous(D, IncProgress)
                          else ComputeTamNeiDistSimultaneous(D, IncProgress);
          end;
    end;
  finally
    if assigned(PMat) then DestroyDistanceMatrix(PMat, FNoOfSeqs);
    if assigned(QMat) then DestroyDistanceMatrix(QMat, FNoOfSeqs);
    setlength(fA, 0);
    setlength(fT, 0);
    setlength(fC, 0);
    setlength(fG, 0);

    if InternalErrId > 0 then
    begin
      Dist1 := InvalidDistValue;
      SetErrorStatus(InternalErrId, InternalErrStr);
    end;
  end;
end;

procedure TNucDist.ComputeEmbryoDist(NDiff, NSites: LongInt);
var
  //xA, yA: Integer; not used
  DW, DE: double;
begin
  //xA := SubstMat[A][A] + SubstMat[A][T];
  //yA := SubstMat[A][A] + SubstMat[T][A];

  DW := (SubstMat[A][T] + SubstMat[T][A])/
        (SubstMat[A][A]+ SubstMat[A][T] + SubstMat[T][A] + SubstMat[T][T]);
  if NSites = SubstMat[T][T] then
    DE := 0
  else
    DE := (SubstMat[A][T] + SubstMat[T][A])/(NSites-SubstMat[T][T]);

  case DistModel of
   gdEmbryoWhole:      Dist1 := DW;
   gdEmbryoExpression: Dist1 := DE;
   gdEmbryoWtd:        Dist1 := (DW + DE)/2;
  end;
end;

//--- composition distance
{
// this was used until version 4019
procedure TNucDist.ComputeCompDist(NDiff, NSites: LongInt);
var
  i, j: Integer;
begin
  Dist1 := 0;
  Var1 := 0;
  if NDiff = 0 then
    Exit;

  for i:=0 to 3 do
  begin
    Freq1[i] := 0;
    Freq2[i] := 0;
  end;

  for i:=0 to 3 do
    for j:=0 to 3 do
    begin
      Freq1[i] := Freq1[i] + SubstMat[i][j];  // rows
      Freq2[i] := Freq2[i] + SubstMat[j][i];  // columns
    end;

  Dist1 := 0;
  Var1 := 0;
  for i := 0 to 3 do
   if Freq1[i] <> Freq2[i] then
     Dist1 := Dist1 + 0.5*(Freq1[i] - Freq2[i])*(Freq1[i] - Freq2[i]);
  Dist1 := Dist1/NSites;
end;
}

//--- composition distance


procedure TNucDist.ComputeCompDist(NDiff, NSites: LongInt;Seq1, Seq2: PAnsiChar);
begin
  Dist1 := 0;
  Var1 := 0;
  if NDiff = 0 then
    Exit;
  Dist1 := CompositionDistance(NSites, 4, Seq1, Seq2,gdBaseN);
  Dist1 := Dist1/NSites;
end;

procedure TNucDist.ComputeDisparityIndex(NDiff, NSites: LongInt; Seq1, Seq2: PAnsiChar);
begin
  Dist1 := 0;
  Var1 := 0;
  if NDiff = 0 then
    Exit;
  ComputeCompDist(NDiff, NSites, Seq1, Seq2);
  Dist1 := (Dist1*NSites - NDiff)/NSites;
  if Dist1 < 0 then
    Dist1 := 0;
  Var1 := 0;
end;

procedure TNucDist.ComputeDisparityIndexTest(NDiff, NSites: LongInt; Seq1, Seq2: PAnsiChar);
begin
  if FFreqTable <> nil then
  begin
    InternalErrId := HC_Unexpected_Error;
    InternalErrStr:= 'Disparity Index Test cannot have FreqTable non-nil.';
    Exit;
  end;
  Dist1 := 0;
  Var1 := 0;
  if NDiff = 0 then
  begin
    Dist1 := 1;  // this is probabilty
    Var1 := 0;
    Exit;
  end;
  Dist1 := CompositionDistance(NSites, 4, Seq1, Seq2,gdBaseN)-NDiff; // inherited
//  ComputeDisparityIndex(NDiff, NSites);
//  Dist1 := Dist1*NSites; // so it is an integer
  MonteCarloDisparityIndexTest(NDiff, NSites, 4, Dist1, Seq1, Seq2,gdBaseN);
end;

procedure TNucDist.ComputeNoOfDiff(NDiff, NSites: LongInt);
begin
  ComputePDist(NDiff, NSites);  // calls to compute the distance
  case DistComponent of
    gdNone,
    gdNucTsOnly,
    gdNucTvOnly:    Dist1 := Dist1*NSites;
    gdNucRatioTsTv: Dist1 := Dist1;
  end;

  if VarType = gdAnalyticalVar then
    case DistComponent of
      gdNone,
      gdNucTsOnly,
      gdNucTvOnly:     Var1 := Var1*NSites*NSites;
      gdNucRatioTsTv:  Var1 := Var1;
    end;
end;

procedure TNucDist.ComputeTestRandom(NDiff, NSites: LongInt);
var
  i: Integer;
  sc: Integer;
begin
  sc := 0;
  for i:=0 to 3 do
    sc := sc + nucf[i];
  Dist1 := 0;
  for i:=0 to 3 do
    Dist1 := Dist1 + nucf[i]/sc*nucf[i]/sc;
  Dist1 := (NSites-NDiff)/NSites - Dist1;
end;

procedure TNucDist.ComputePDist(NDiff, NSites: LongInt);
var
  c1, c2, P, Q: Double;
begin
  P := (SubstMat[A][G] + SubstMat[T][C])/NSites;
  Q := (SubstMat[A][T] + SubstMat[A][C] + SubstMat[G][T] + SubstMat[G][C])/NSites;

  case DistComponent of
    gdNone:          Dist1 := P+Q;
    gdNucTsOnly:     Dist1 := P;
    gdNucTvOnly:     Dist1 := Q;
    gdNucRatioTsTv:  Dist1 := P/Q;
  end;

  if VarType = gdAnalyticalVar then
    case DistComponent of
      gdNone:      Var1 := (P+Q)*(1-P-Q)/NSites;
      gdNucTsOnly: Var1 := P*(1-P)/NSites;
      gdNucTvOnly: Var1 := Q*(1-Q)/NSites;
      gdNucRatioTsTv:
        begin
          c1 := 1/Q; c2 := -P/Q/Q;
          Var1 := DeltaVar([c1, P, c2, Q, NSites]);
        end;
    end;
end;

// Jukes-cantor distance
procedure TNucDist.ComputeJCDist(NDiff, NSites: LongInt);
var
 p: Double;
begin
  p := NDiff/NSites;
  if p > 0.74 then
  begin
    InternalErrId := HC_Jukes_Cantor_Correction_Failed;
    InternalErrStr:= 'Jukes-Cantor distance incalculable.';
    Exit;
  end;

  Dist1 := -0.75*ln(1 - p/0.75);
  if VarType = gdAnalyticalVar then
    Var1 := p*(1-p)/SQR(1- p/0.75)/NSites;
end;

// jukes-cantor gamma distance
procedure TNucDist.ComputeJCGammaDist(NDiff, NSites: LongInt);
var
 p: Double;
begin
  p     := NDiff/NSites;
  if p > 0.74 then
  begin
    InternalErrId := HC_Jukes_Cantor_Correction_Failed;
    InternalErrStr:= 'Jukes-Cantor distance incalculable.';
    Exit;
  end;
  Dist1 := 0.75*GammaPara*(power(1-p/0.75, -GammaInv)-1);
  if VarType = gdAnalyticalVar then
    Var1 := p*(1-p)*power(1-p/0.75, -2*(GammaInv+1))/NSites;
end;

// Tajima-nei distance
procedure TNucDist.ComputeTjmNeiDist(NDiff, NSites: LongInt);
var
  gA, gT, gC, gG, p, c1, b: Double;
begin
  p := NDiff/NSites;
  if NDiff = 0 then // then distances as well as the variances are 0
  begin
    Dist1 := 0;   var1  := 0;   Exit;
  end;

  // first get base frequencies
  gA  := GetBaseFreq(A, NSites);
  gT  := GetBaseFreq(T, NSites);
  gG  := GetBaseFreq(G, NSites);
  gC  := GetBaseFreq(C, NSites);

  b := 0.5*(1 - (gA*gA + gT*gT + gC*gC + gG*gG));
  c1 := -1;
  if (gA > 0.0) and (gT > 0.0) and (gC > 0.0) and (gG > 0.0) then
    c1 := SQR(SubstMat[A][T]/NSites)/2/gA/gT + SQR(SubstMat[A][G]/NSites)/2/gA/gG +
          SQR(SubstMat[A][C]/NSites)/2/gA/gC + SQR(SubstMat[G][T]/NSites)/2/gG/gT +
          SQR(SubstMat[G][C]/NSites)/2/gG/gC + SQR(SubstMat[C][T]/NSites)/2/gC/gT;
  if c1 > 0 then
    b  := b + 0.5*p*p/c1;

  if (p/b) > 0.99  then
  begin
    InternalErrId := HC_Tajima_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tajima-Nei distance incalculable.';
    Exit;
  end;

  Dist1 := -b*ln(1-p/b);
  if VarType = gdAnalyticalVar then
    Var1 := p*(1-p)/SQR(1-(p/b))/NSites;
end;

procedure TNucDist.ComputeTjmNeiGammaDist(NDiff, NSites: LongInt);
var
  gA, gT, gC, gG, p, b: Double;
begin
  p := NDiff/NSites;
  if NDiff = 0 then // then distances as well as the variances are 0
  begin
    Dist1 := 0;   var1  := 0;   Exit;
  end;

  // first get base frequencies
  gA  := GetBaseFreq(A, NSites);
  gT  := GetBaseFreq(T, NSites);
  gG  := GetBaseFreq(G, NSites);
  gC  := GetBaseFreq(C, NSites);

  b := 1 -(gA*gA + gT*gT + gC*gC + gG*gG);
  if (p/b) > 0.99  then
  begin
    InternalErrId := HC_Tajima_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tajima-Nei distance incalculable.';
    Exit;
  end;

  Dist1 := b*GammaPara*(power(1-p/b, -GammaInv)-1);
  if VarType = gdAnalyticalVar then
    Var1 := p*(1-p)*power(1-p/b, -2*(GammaInv+1))/NSites;
end;

procedure TNucDist.ComputeTjmNeiHeteroDist(NDiff, NSites: LongInt);
var
  gA1,gT1,gC1,gG1,gA2,gT2,gC2,gG2,p, f, b: Double;
begin
  p := NDiff/NSites;
  if NDiff = 0 then // then distances as well as the variances are 0
  begin
    Dist1 := 0;   var1  := 0;   Exit;
  end;

  // first get base frequencies
  gA1 := (SubstMat[A][A]+SubstMat[A][T]+SubstMat[A][C]+SubstMat[A][G])/NSites;
  gT1 := (SubstMat[T][A]+SubstMat[T][T]+SubstMat[T][C]+SubstMat[T][G])/NSites;
  gG1 := (SubstMat[C][A]+SubstMat[C][T]+SubstMat[C][C]+SubstMat[C][G])/NSites;
  gC1 := (SubstMat[G][A]+SubstMat[G][T]+SubstMat[G][C]+SubstMat[G][G])/NSites;
  gA2 := (SubstMat[A][A]+SubstMat[T][A]+SubstMat[C][A]+SubstMat[G][A])/NSites;
  gT2 := (SubstMat[A][T]+SubstMat[T][T]+SubstMat[C][T]+SubstMat[G][T])/NSites;
  gG2 := (SubstMat[A][C]+SubstMat[T][C]+SubstMat[C][C]+SubstMat[G][C])/NSites;
  gC2 := (SubstMat[A][G]+SubstMat[T][G]+SubstMat[C][G]+SubstMat[G][G])/NSites;

  b := 1 - ((gA1+gA2)*(gA1+gA2) + (gT1+gT2)*(gT1+gT2) + (gC1+gC2)*(gC1+gC2) + (gG1+gG2)*(gG1+gG2))/4;
  f := 1 - (gA1*gA2 + gT1*gT2 + gC1*gC2 + gG1*gG2);

  if (p/f) > 0.99  then
  begin
    InternalErrId := HC_Tajima_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tajima-Nei distance incalculable.';
    Exit;
  end;

  Dist1 := -b*ln(1-p/f);
end;

procedure TNucDist.ComputeTjmNeiGammaHeteroDist(NDiff, NSites: LongInt);
var
  gA1,gT1,gC1,gG1,gA2,gT2,gC2,gG2,p, f, b: Double;
begin
  p := NDiff/NSites;
  if NDiff = 0 then // then distances as well as the variances are 0
  begin
    Dist1 := 0;   var1  := 0;   Exit;
  end;

  // first get base frequencies
  gA1 := (SubstMat[A][A]+SubstMat[A][T]+SubstMat[A][C]+SubstMat[A][G])/NSites;
  gT1 := (SubstMat[T][A]+SubstMat[T][T]+SubstMat[T][C]+SubstMat[T][G])/NSites;
  gG1 := (SubstMat[C][A]+SubstMat[C][T]+SubstMat[C][C]+SubstMat[C][G])/NSites;
  gC1 := (SubstMat[G][A]+SubstMat[G][T]+SubstMat[G][C]+SubstMat[G][G])/NSites;
  gA2 := (SubstMat[A][A]+SubstMat[T][A]+SubstMat[C][A]+SubstMat[G][A])/NSites;
  gT2 := (SubstMat[A][T]+SubstMat[T][T]+SubstMat[C][T]+SubstMat[G][T])/NSites;
  gG2 := (SubstMat[A][C]+SubstMat[T][C]+SubstMat[C][C]+SubstMat[G][C])/NSites;
  gC2 := (SubstMat[A][G]+SubstMat[T][G]+SubstMat[C][G]+SubstMat[G][G])/NSites;

  b := 1 - ((gA1+gA2)*(gA1+gA2) + (gT1+gT2)*(gT1+gT2) + (gC1+gC2)*(gC1+gC2) + (gG1+gG2)*(gG1+gG2))/4;
  f := 1 - (gA1*gA2 + gT1*gT2 + gC1*gC2 + gG1*gG2);

  if (p/f) > 0.99  then
  begin
    InternalErrId := HC_Tajima_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tajima-Nei distance incalculable.';
    Exit;
  end;

  Dist1 := b*GammaPara*(power(1-p/f, -GammaInv)-1);
end;


// Kimura's 2-paramter model distance
procedure TNucDist.ComputeKimuraDist(NDiff, NSites: LongInt);
var
  c1, c2, c3, c4, c5, c6, P, Q: Double;
begin
  P := (SubstMat[A][G] + SubstMat[T][C])/NSites;
  Q := (SubstMat[A][T] + SubstMat[A][C] + SubstMat[G][T] + SubstMat[G][C])/NSites;

  c1 := 1-2*P-Q;
  c2 := 1-2*Q;

  if (c1 < 0.01) or (c2 < 0.01) then
  begin
    InternalErrId := HC_Kimura_1980_Distance_Correction_Failed;
    InternalErrStr:= 'Kimura (1980) distance incalculable.';
    Exit;
  end;

  case DistComponent of
    gdNone:         Dist1 := -0.5*ln(c1) - 0.25*ln(c2);
    gdNucTsOnly:    Dist1 := -0.5*ln(c1) + 0.25*ln(c2);
    gdNucTvOnly:    Dist1 := -0.5*ln(c2);
    gdNucRatioTsTv: Dist1 := ln(c1)/ln(c2) - 0.5;
  end;

  if VarType = gdAnalyticalVar then
    case DistComponent of
      gdNone:
        begin
          c1 := 1/c1;  c2 := 1/c2;  c3 := (c1+c2)/2;
          Var1 := DeltaVar([c1, P, c3, Q, NSites]);
        end;
      gdNucTsOnly:
        begin
          c1 := 1/c1;  c2 := 1/c2;  c4 := (c1-c2)/2;
          Var1 := DeltaVar([c1, P, c4, Q, NSites]);
        end;
      gdNucTvOnly: Var1 := Q*(1-Q)/c2/c2/NSites;
      gdNucRatioTsTv:
        begin
          c1 := 1/c1;   c2 := 1/c2;
          c5 := -2*c1/ln(1/c2);
          c6 := (c5 + 4*c2*ln(1/c1)/SQR(ln(1/c2)))/2;
          Var1 := DeltaVar([c5, P, c6, Q, NSites]);
        end;
    end;
end;

// Kimura's 2-paramter model with Gamma distance
procedure TNucDist.ComputeKimuraGammaDist(NDiff, NSites: LongInt);
var
  c1, c2, c3, c4, c5, c6, P, Q: Double;
begin
  P := (SubstMat[A][G] + SubstMat[T][C])/NSites;
  Q := (SubstMat[A][T] + SubstMat[A][C] + SubstMat[G][T] + SubstMat[G][C])/NSites;

  if ((1-2*P-Q) < 0.01) or ((1-2*Q) < 0.01) then
  begin
    InternalErrId := HC_Kimura_1980_Distance_Correction_Failed;
    InternalErrStr:= 'Kimura (1980) distance incalculable.';
    Exit;
  end;

  try
    c1 := power(1-2*P-Q, -GammaInv);
    c2 := power(1-2*Q,   -GammaInv);
  except
    on E:Exception do
    begin
      InternalErrId := HC_Kimura_1980_Distance_Correction_Failed;
      InternalErrStr:= 'Kimura (1980) distance incalculable. Please check your option parameters.';
      Exit;
    end;
  end;

  case DistComponent of
    gdNone:         Dist1 := 0.5*GammaPara*(c1 + 0.5*c2 - 1.5);
    gdNucTsOnly:    Dist1 := 0.5*GammaPara*(c1 - 0.5*c2 - 0.5);
    gdNucTvOnly:    Dist1 := 0.5*GammaPara*(c2-1);
    gdNucRatioTsTv: Dist1 := (c1-1)/(c2-1) - 0.5;
  end;

  if VarType = gdAnalyticalVar then
  begin
    c1 := power(1-2*P-Q, -GammaInv -1);
    c2 := power(1-2*Q  , -GammaInv -1);
    case DistComponent of
      gdNone:
        begin
          c3 := (c1+c2)/2;
          Var1 := DeltaVar([c1, P, c3, Q, NSites]);
        end;
      gdNucTsOnly:
        begin
          c4 := (c1-c2)/2;
          Var1 := DeltaVar([c1, P, c4, Q, NSites]);
        end;
      gdNucTvOnly:
          Var1 := c2*c2*Q*(1-Q)/NSites;
      gdNucRatioTsTv:
        begin
          c5 := 2*c1*GammaInv/(power(1-2*Q,-GammaInv) - 1);
          c6 := GammaInv/SQR( power(1-2*Q, -gammaInv) -1)*
	  	            (power(1-2*Q,-gammaInv-1)*(c1-c2) -
		            (2*c2*(power(1-2*P-Q, -gammaInv) - 0.5*power(1-2*Q,-gammaInv) - 0.5))
                          );
          Var1 := DeltaVar([c5, P, c6, Q, NSites]);
        end;
    end;
  end;
end;

// Tamura distance
procedure TNucDist.ComputeTamuraDist(NDiff, NSites: LongInt);
var
  c1, c2, c3, c4, c5, c6, P, Q: Double;
  theta, thetaTerm: Double;
begin
  theta     := GetBaseFreq(G, NSites) + GetBaseFreq(C, NSites);
  thetaTerm := 2*theta*(1 - theta);

  if thetaTerm <= 0.0 then // i.e, theta == 0 or theta == 1
  begin
    InternalErrId := HC_Tamura_1992_Correction_Could_Not_Be_Applied;
    InternalErrStr:= 'G+C content is 0% or 100% for a given pair.';
    exit;
  end;

  P := (SubstMat[A][G] + SubstMat[T][C])/NSites;
  Q := (SubstMat[A][T] + SubstMat[A][C] + SubstMat[G][T] + SubstMat[G][C])/NSites;

  c1 := 1- P/thetaTerm - Q;
  c2 := 1- 2*Q;

  if (c1 < 0.01) or (c2 < 0.01) then
  begin
    InternalErrId := HC_Tamura_1992_Correction_Could_Not_Be_Applied;
    InternalErrStr:= 'Tamura-distance incalculable for some pairs.';
    exit;
  end;

  case DistComponent of
    gdNone:         Dist1 := -thetaTerm*ln(c1) - 0.5*(1-thetaTerm)*ln(c2);
    gdNucTsOnly:    Dist1 := -thetaTerm*ln(c1) + 0.5*thetaTerm*ln(c2);
    gdNucTvOnly:    Dist1 := -0.5*ln(c2);
    gdNucRatioTsTv: Dist1 := 2*thetaTerm*ln(c1)/ln(c2) - thetaTerm;
  end;

  if VarType = gdAnalyticalVar then
    case DistComponent of
      gdNone:
        begin
          c1 := 1/c1;  c2 := 1/c2;
          c3 := thetaTerm*(c1-c2) + c2;
          Var1 := DeltaVar([c1, P, c3, Q, NSites]);
        end;
      gdNucTsOnly:
        begin
          c1 := 1/c1;  c2 := 1/c2;
          c4 := thetaTerm*(c1-c2);
          Var1 := DeltaVar([c1, P, c4, Q, NSites]);
        end;
      gdNucTvOnly: Var1 := c2*c2*Q*(1-Q)/NSites;
      gdNucRatioTsTv:
        begin
	  c1   := 1/c1;  c2 := 1/c2;
	  c5   := -2*c1/ln(1/c2);
	  c6   := thetaTerm*(c5 + 4*c2*ln(1/c1)/SQR(ln(1/c2)));
	  var1 := DeltaVar([c5, P, c6, Q, NSites]);
        end;
     end;
end;

// Tamura Gamma distance
procedure TNucDist.ComputeTamuraGammaDist(NDiff, NSites: LongInt);
var
  c1, c2, c3, c4, c5, c6, P, Q: Double;
  theta, thetaTerm, gInv: Double;
begin
  gInv := 1/GammaPara;

  theta     := GetBaseFreq(G, NSites) + GetBaseFreq(C, NSites);
  thetaTerm := 2*theta*(1 - theta);

  if thetaTerm <= 0.0 then // i.e, theta == 0 or theta == 1
  begin
    InternalErrId := HC_Tamura_1992_Correction_Could_Not_Be_Applied;
    InternalErrStr:= 'G+C content is 0% or 100% for a given pair.';
    exit;
  end;

  P := (SubstMat[A][G] + SubstMat[T][C])/NSites;
  Q := (SubstMat[A][T] + SubstMat[A][C] + SubstMat[G][T] + SubstMat[G][C])/NSites;

  c1 := 1- P/thetaTerm - Q;
  c2 := 1- 2*Q;

  if (c1 < 0.01) or (c2 < 0.01) then
  begin
    InternalErrId := HC_Tamura_1992_Correction_Could_Not_Be_Applied;
    InternalErrStr:= 'Tamura-distance incalculable for some pairs.';
    exit;
  end;

  case DistComponent of
    gdNone:         Dist1 := GammaPara/2*(2*thetaTerm*power(c1, -gInv) +(1-thetaTerm)*power(c2, -gInv) -thetaTerm -1);
    gdNucTsOnly:    Dist1 := GammaPara/2*(2*thetaTerm*power(c1, -gInv) -thetaTerm*power(c2, -gInv) -thetaTerm);
    gdNucTvOnly:    Dist1 := GammaPara/2*(power(c2, -gInv) -1);
    gdNucRatioTsTv: Dist1 := thetaTerm*(2*power(c1, -gInv) -power(c2, -gInv) -1)/(power(c2, -gInv) -1);
  end;

  if VarType = gdAnalyticalVar then
    case DistComponent of
      gdNone:
        begin
          c1 := power(c1, -(1+gInv));  c2 := power(c2, -(1+gInv));
          c3 := thetaTerm*(c1-c2) + c2;
          Var1 := DeltaVar([c1, P, c3, Q, NSites]);
        end;
      gdNucTsOnly:
        begin
          c1 := power(c1, -(1+gInv));  c2 := power(c2, -(1+gInv));
          c4 := thetaTerm*(c1-c2);
          Var1 := DeltaVar([c1, P, c4, Q, NSites]);
        end;
      gdNucTvOnly:
        begin
          c2   := power(c2, -(1+gInv));
          Var1 := c2*c2*Q*(1-Q)/NSites;
        end;
      gdNucRatioTsTv:
        begin
          c3 := power(c1, -(1+gInv));  c4 := power(c2, -(1+gInv));
          c5 := thetaTerm*(c3-c4);
          c6 := c5 -thetaTerm*(2*power(c1, -gInv) -power(c2, -gInv) -1)/(power(c2, -gInv) -1)*c4;
          c3 := c3/(GammaPara/2*(power(c2, -gInv) -1));
          c6 := c6/(GammaPara/2*(power(c2, -gInv) -1));
	  var1 := DeltaVar([c3, P, c6, Q, NSites]);
        end;
     end;
end;

//
procedure TNucDist.ComputeTamuraHeteroDist(NDiff, NSites: LongInt);
var
  c1, c2, P, Q: Double;
  theta1, theta2, thetaTerm: Double;
begin
  theta1 := (SubstMat[C][A]+SubstMat[C][T]+SubstMat[C][C]+SubstMat[C][G]
            +SubstMat[G][A]+SubstMat[G][T]+SubstMat[G][C]+SubstMat[G][G])/NSites;
  theta2 := (SubstMat[A][C]+SubstMat[T][C]+SubstMat[C][C]+SubstMat[G][C]
            +SubstMat[A][G]+SubstMat[T][G]+SubstMat[C][G]+SubstMat[G][G])/NSites;
  thetaTerm := (theta1+theta2)*(1 - (theta1+theta2)/2);

  if thetaTerm <= 0.0 then // i.e, theta == 0 or theta == 1
  begin
    InternalErrId := HC_Tamura_1992_Correction_Could_Not_Be_Applied;
    InternalErrStr:= 'G+C content is 0% or 100% for a given pair.';
    exit;
  end;

  P := (SubstMat[A][G]+SubstMat[G][A]+SubstMat[T][C]+SubstMat[C][T])/NSites;
  Q := (SubstMat[A][T]+SubstMat[A][C]+SubstMat[G][T]+SubstMat[G][C]
       +SubstMat[T][A]+SubstMat[C][A]+SubstMat[T][G]+SubstMat[C][G])/NSites;

  c1 := 1- P/(theta1*(1-theta2)+theta2*(1-theta1)) - Q;
  c2 := 1- 2*Q;

  if (c1 < 0.01) or (c2 < 0.01) then
  begin
    InternalErrId := HC_Tamura_1992_Correction_Could_Not_Be_Applied;
    InternalErrStr:= 'Tamura-distance incalculable for some pairs.';
    exit;
  end;

  case DistComponent of
    gdNone:         Dist1 := -thetaTerm*ln(c1) - 0.5*(1-thetaTerm)*ln(c2);
    gdNucTsOnly:    Dist1 := -thetaTerm*ln(c1) + 0.5*thetaTerm*ln(c2);
    gdNucTvOnly:    Dist1 := -0.5*ln(c2);
    gdNucRatioTsTv: Dist1 := 2*thetaTerm*ln(c1)/ln(c2) - thetaTerm;
  end;
end;

procedure TNucDist.ComputeTamuraGammaHeteroDist(NDiff, NSites: LongInt);
var
  c1, c2, P, Q: Double;
  theta1, theta2, thetaTerm, gInv: Double;
begin
  gInv := 1/GammaPara;

  theta1 := (SubstMat[C][A]+SubstMat[C][T]+SubstMat[C][C]+SubstMat[C][G]
            +SubstMat[G][A]+SubstMat[G][T]+SubstMat[G][C]+SubstMat[G][G])/NSites;
  theta2 := (SubstMat[A][C]+SubstMat[T][C]+SubstMat[C][C]+SubstMat[G][C]
            +SubstMat[A][G]+SubstMat[T][G]+SubstMat[C][G]+SubstMat[G][G])/NSites;
  thetaTerm := (theta1+theta2)*(1 - (theta1+theta2)/2);

  if thetaTerm <= 0.0 then // i.e, theta == 0 or theta == 1
  begin
    InternalErrId := HC_Tamura_1992_Correction_Could_Not_Be_Applied;
    InternalErrStr:= 'G+C content is 0% or 100% for a given pair.';
    exit;
  end;

  P := (SubstMat[A][G]+SubstMat[G][A]+SubstMat[T][C]+SubstMat[C][T])/NSites;
  Q := (SubstMat[A][T]+SubstMat[A][C]+SubstMat[G][T]+SubstMat[G][C]
       +SubstMat[T][A]+SubstMat[C][A]+SubstMat[T][G]+SubstMat[C][G])/NSites;

  c1 := 1- P/(theta1*(1-theta2)+theta2*(1-theta1)) - Q;
  c2 := 1- 2*Q;

  if (c1 < 0.01) or (c2 < 0.01) then
  begin
    InternalErrId := HC_Tamura_1992_Correction_Could_Not_Be_Applied;
    InternalErrStr:= 'Tamura-distance incalculable for some pairs.';
    exit;
  end;

  case DistComponent of
    gdNone:         Dist1 := GammaPara/2*(2*thetaTerm*power(c1, -gInv) +(1-thetaTerm)*power(c2, -gInv) -thetaTerm -1);
    gdNucTsOnly:    Dist1 := GammaPara/2*(2*thetaTerm*power(c1, -gInv) -thetaTerm*power(c2, -gInv) -thetaTerm);
    gdNucTvOnly:    Dist1 := GammaPara/2*(power(c2, -gInv) -1);
    gdNucRatioTsTv: Dist1 := thetaTerm*2*power(c1, -gInv)/(power(c2, -gInv) -1) -thetaTerm;
  end;
end;

// Tamura-Nei distance
procedure TNucDist.ComputeTamNeiDist(NDiff, NSites: LongInt);
var
  gA, gT, gC, gG, gR, gY: Double;
  P1, P2, Q, sx, vx: Double;
  k1, k2, k3,k5: Double;  // multipliers
  w1, w2, w3: Double; // log arguments
  c1, c2, c3, c4, c5, c6, c7, c8: Double;  // for variances
begin
  // first get base frequencies
  gA  := GetBaseFreq(A, NSites);
  gT  := GetBaseFreq(T, NSites);
  gG  := GetBaseFreq(G, NSites);
  gC  := GetBaseFreq(C, NSites);

  gR := gA + gG;
  gY := gT + gC;
  P1 := SubstMat[A][G]/NSites;
  P2 := SubstMat[T][C]/NSites;
  Q  := (SubstMat[A][T]+SubstMat[A][C]+SubstMat[G][T]+SubstMat[G][C])/NSites;

  if (gR<0) or (gY <0) then
  begin
    InternalErrId := HC_Tamura_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tamura-Nei distance could not be computed';
    exit;
  end;

  k1 := 2*gA*gG/gR;
  k2 := 2*gT*gC/gY;
  k3 := 2*(gR*gY -gA*gG*gY/gR -gT*gC*gR/gY);

  if (k1<0) or (k2<0) then
  begin
    InternalErrId := HC_Tamura_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tamura-Nei distance could not be computed';
    exit;
  end;

  w1 := 1 - P1/k1 - Q/2/gR;
  w2 := 1 - P2/k2 - Q/2/gY;
  w3 := 1 - Q/2/gR/gY;

  if (w1 < 0.01) or (w2 < 0.01) or (w3 < 0.01) then
  begin
    InternalErrId := HC_Tamura_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tamura-Nei distance could not be computed';
    exit;
  end;

  vx := 0;
  sx := 0;
  case DistComponent of
    gdNone:      Dist1 := -k1*ln(w1) -k2*ln(w2) -k3*ln(w3);
    gdNucTsOnly: Dist1 := -k1*ln(w1) -k2*ln(w2) -(k3 - 2*gR*gY)*ln(w3);
    gdNucTvOnly: Dist1 := -2*gR*gY*ln(w3);
    gdNucRatioTsTv:
      begin
        sx := -k1*ln(w1) -k2*ln(w2) -(k3 - 2*gR*gY)*ln(w3);
        vx := -2*gR*gY*ln(w3);
        if sx < 0 then sx := 0;
        if vx < 0 then vx := 0;
        if sx = 0 then
          Dist1 := 0
        else
          Dist1 := sx/vx;
      end;
  end;

  if VarType <> gdAnalyticalVar then
    Exit;

  c1 := 1/w1;
  c2 := 1/w2;
  c3 := 1/w3;
  case DistComponent of
    gdNone:
      begin
        k5 := (gA*gA + gG*gG)/2/gR/gR + (gT*gT + gC*gC)/2/gY/gY;
	c4 := k1*c1/2/gR + k2*c2/2/gY + k5*c3;
        Var1 := DeltaVar([c1, P1, c2, P2, c4, Q, NSites]);
      end;
    gdNucTsOnly:
      begin
        c5 := -2*gA*gG*(gY*(gA*gG*Q+P1*gR*gR) - gA*gG*Q)/(gR*(2*gR*gY-Q)*(gA*gG*(Q-2*gR)+P1*gR*gR))
              -2*gT*gC*(gR*(gT*gC*Q+P2*gY*gY) - gT*gC*Q)/(gY*(2*gR*gY-Q)*(gT*gC*(Q-2*gY)+P2*gY*gY));
        Var1 := DeltaVar([c1, P1, c2, P2, c5, Q, NSites]);
      end;
    gdNucTvOnly: Var1 := (c3*c3*Q*(1-Q))/NSites;
    gdNucRatioTsTv:
      begin
        c6 := c1/vx;
        c7 := c2/vx;
        c5 := -2*gA*gG*(gY*(gA*gG*Q+P1*gR*gR) - gA*gG*Q)/(gR*(2*gR*gY-Q)*(gA*gG*(Q-2*gR)+P1*gR*gR))
              -2*gT*gC*(gR*(gT*gC*Q+P2*gY*gY) - gT*gC*Q)/(gY*(2*gR*gY-Q)*(gT*gC*(Q-2*gY)+P2*gY*gY));
        c8 := (c5 - sx*c3/vx)/vx;
	Var1 := DeltaVar([c6, P1,  c7, P2, c8, Q, NSites]);
      end;
  end;
end;



// Tamura-nei gamma distance
procedure TNucDist.ComputeTamNeiGammaDist(NDiff, NSites: LongInt);
var
  gA, gT, gC, gG, gR, gY: Double;
  P1, P2, Q, sx, vx: Double;
  k1, k2, k3, k4, k5: Double;  // multipliers
  w1, w2, w3: Double; // log arguments
  c1, c2, c3, c4, c5, c6, c7, c8: Double;  // for variances
  gInv: Double;
begin
  gInv := 1/GammaPara;

  // first get base frequencies
  gA  := GetBaseFreq(A, NSites);
  gT  := GetBaseFreq(T, NSites);
  gG  := GetBaseFreq(G, NSites);
  gC  := GetBaseFreq(C, NSites);

  gR := gA + gG;
  gY := gT + gC;
  P1 := SubstMat[A][G]/NSites;
  P2 := SubstMat[T][C]/NSites;
  Q  := (SubstMat[A][T]+SubstMat[A][C]+SubstMat[G][T]+SubstMat[G][C])/NSites;

  if (gR<0) or (gY <0) then
  begin
    InternalErrId := HC_Tamura_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tamura-Nei distance could not be computed';
    exit;
  end;

  k1 := 2*gA*gG/gR;
  k2 := 2*gT*gC/gY;
  k3 := 2*(gR*gY -gA*gG*gY/gR -gT*gC*gR/gY);
  k4 := 2*(gA*gG + gT*gC + gR*gY);

  if (k1<0) or (k2<0) then
  begin
    InternalErrId := HC_Tamura_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tamura-Nei distance could not be computed';
    exit;
  end;

  w1 := 1 - P1/k1 - Q/2/gR;
  w2 := 1 - P2/k2 - Q/2/gY;
  w3 := 1 - Q/2/gR/gY;

  if (w1 < 0.01) or (w2 < 0.01) or (w3 < 0.01) then
  begin
    InternalErrId := HC_Tamura_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tamura-Nei distance could not be computed';
    exit;
  end;

  vx := 0;
  sx := 0;
  case DistComponent of
    gdNone:        Dist1 := (k1*power(w1, -gInv) +k2*power(w2, -gInv) + k3*power(w3, -gInv) - k4)*GammaPara;
    gdNucTsOnly:   Dist1 := (k1*power(w1, -gInv) +k2*power(w2, -gInv) +(k3 - 2*gR*gY)*power(w3, -gInv) - (k4 - 2*gR*gY))*GammaPara;
    gdNucTvOnly:   Dist1 := 2*gR*gY*(power(w3, -gInv) -1)*GammaPara;
    gdNucRatioTsTv:
      begin
        sx := (k1*power(w1, -gInv) +k2*power(w2, -gInv) +(k3 - 2*gR*gY)*power(w3, -gInv) - (k4 - 2*gR*gY))*GammaPara;
        vx := 2*gR*gY*(power(w3, -gInv)-1)*GammaPara;
        Dist1 := sx/vx;
      end;
  end;

  if VarType <> gdAnalyticalVar then
    Exit;

  c1 := power(w1, -gInv-1);
  c2 := power(w2, -gInv-1);
  c3 := power(w3, -gInv-1);
  case DistComponent of
    gdNone:
      begin
//        k5 := (gA*gA + gG*gG)/2/gR/gR + (gT*gT + gC*gC)/2/gY/gY;
	c4 := k1*c1/2/gR + k2*c2/2/gY + k3*c3/2/gR/gY;
        Var1 := DeltaVar([c1, P1, c2, P2, c4, Q, NSites]);
      end;
    gdNucTsOnly:
      begin
        k5 :=  gA*gG/gR/gR + gT*gC/gY/gY;
	c5 := k1*c1/2/gR + k2*c2/2/gY + k5*c3;
        Var1 := DeltaVar([c1, P1, c2, P2, c5, Q, NSites]);
      end;
    gdNucTvOnly: Var1 := (c3*c3*Q*(1-Q))/NSites;
    gdNucRatioTsTv:
      begin
        k5 := gA*gG/gR/gR + gT*gC/gY/gY;
	c5 := k1*c1/2/gR + k2*c2/2/gY + k5*c3;
        c6 := c1/vx; c7 := c2/vx;
        c8 := (c5 - sx*c3/vx)/vx;
	Var1 := DeltaVar([c6, P1,  c7, P2, c8, Q, NSites]);
      end;
  end;
end;

procedure TNucDist.ComputeTamNeiHeteroDist(NDiff, NSites: LongInt);
var
  gA1,gT1,gC1,gG1,gR1,gY1,gA2,gT2,gC2,gG2,gR2,gY2,gA,gT,gC,gG,gR,gY: Double;
  P1, P2, Q, sx, vx: Double;
  k1, k2, k3: Double;  // multipliers
  w1, w2, w3: Double; // log arguments
begin
  // first get base frequencies
  gA1 := (SubstMat[A][A]+SubstMat[A][T]+SubstMat[A][C]+SubstMat[A][G])/NSites;
  gT1 := (SubstMat[T][A]+SubstMat[T][T]+SubstMat[T][C]+SubstMat[T][G])/NSites;
  gC1 := (SubstMat[C][A]+SubstMat[C][T]+SubstMat[C][C]+SubstMat[C][G])/NSites;
  gG1 := (SubstMat[G][A]+SubstMat[G][T]+SubstMat[G][C]+SubstMat[G][G])/NSites;
  gA2 := (SubstMat[A][A]+SubstMat[T][A]+SubstMat[C][A]+SubstMat[G][A])/NSites;
  gT2 := (SubstMat[A][T]+SubstMat[T][T]+SubstMat[C][T]+SubstMat[G][T])/NSites;
  gC2 := (SubstMat[A][C]+SubstMat[T][C]+SubstMat[C][C]+SubstMat[G][C])/NSites;
  gG2 := (SubstMat[A][G]+SubstMat[T][G]+SubstMat[C][G]+SubstMat[G][G])/NSites;
  gR1 := gA1 + gG1;
  gY1 := gT1 + gC1;
  gR2 := gA2 + gG2;
  gY2 := gT2 + gC2;

  gA := (gA1+gA2)/2;
  gT := (gT1+gT2)/2;
  gC := (gC1+gC2)/2;
  gG := (gG1+gG2)/2;
  gR := (gR1+gR2)/2;
  gY := (gY1+gY2)/2;

  P1 := (SubstMat[A][G]+SubstMat[G][A])/NSites;
  P2 := (SubstMat[T][C]+SubstMat[C][T])/NSites;
  Q  := ( SubstMat[A][T]+SubstMat[A][C]+SubstMat[G][T]+SubstMat[G][C]
         +SubstMat[T][A]+SubstMat[C][A]+SubstMat[T][G]+SubstMat[C][G])/NSites;

  if (gR1<0) or (gY1 <0) or (gR2<0) or (gY2 <0) then
  begin
    InternalErrId := HC_Tamura_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tamura-Nei distance could not be computed';
    exit;
  end;

  k1 := 2*gA*gG/gR;
  k2 := 2*gT*gC/gY;
  k3 := 2*(gR*gY -gA*gG*gY/gR -gT*gC*gR/gY);

  if (k1<0) or (k2<0) then
  begin
    InternalErrId := HC_Tamura_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tamura-Nei distance could not be computed';
    exit;
  end;

  w1 := 1 - P1*gR/(gA1*gG2+gA2*gG1) - Q/2/gR;
  w2 := 1 - P2*gY/(gT1*gC2+gT2*gC1) - Q/2/gY;
  w3 := 1 - Q/(gR1*gY2+gR2*gY1);

  if (w1 < 0.01) or (w2 < 0.01) or (w3 < 0.01) then
  begin
    InternalErrId := HC_Tamura_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tamura-Nei distance could not be computed';
    exit;
  end;

  vx := 0;
  sx := 0;
  case DistComponent of
    gdNone:      Dist1 := -k1*ln(w1) -k2*ln(w2) -k3*ln(w3);
    gdNucTsOnly: Dist1 := -k1*ln(w1) -k2*ln(w2) -(k3 - 2*gR*gY)*ln(w3);
    gdNucTvOnly: Dist1 := -2*gR*gY*ln(w3);
    gdNucRatioTsTv:
      begin
        sx := -k1*ln(w1) -k2*ln(w2) -(k3 - 2*gR*gY)*ln(w3);
        vx := -2*gR*gY*ln(w3);
        Dist1 := sx/vx;
      end;
  end;
end;

procedure TNucDist.ComputeTamNeiGammaHeteroDist(NDiff, NSites: LongInt);
var
  gA1,gT1,gC1,gG1,gR1,gY1,gA2,gT2,gC2,gG2,gR2,gY2,gA,gT,gC,gG,gR,gY: Double;
  P1, P2, Q, sx, vx: Double;
  k1, k2, k3, k4: Double;  // multipliers
  w1, w2, w3: Double; // log arguments
  gInv: Double;
begin
  gInv := 1/GammaPara;

  // first get base frequencies
  gA1 := (SubstMat[A][A]+SubstMat[A][T]+SubstMat[A][C]+SubstMat[A][G])/NSites;
  gT1 := (SubstMat[T][A]+SubstMat[T][T]+SubstMat[T][C]+SubstMat[T][G])/NSites;
  gC1 := (SubstMat[C][A]+SubstMat[C][T]+SubstMat[C][C]+SubstMat[C][G])/NSites;
  gG1 := (SubstMat[G][A]+SubstMat[G][T]+SubstMat[G][C]+SubstMat[G][G])/NSites;
  gA2 := (SubstMat[A][A]+SubstMat[T][A]+SubstMat[C][A]+SubstMat[G][A])/NSites;
  gT2 := (SubstMat[A][T]+SubstMat[T][T]+SubstMat[C][T]+SubstMat[G][T])/NSites;
  gC2 := (SubstMat[A][C]+SubstMat[T][C]+SubstMat[C][C]+SubstMat[G][C])/NSites;
  gG2 := (SubstMat[A][G]+SubstMat[T][G]+SubstMat[C][G]+SubstMat[G][G])/NSites;
  gR1 := gA1 + gG1;
  gY1 := gT1 + gC1;
  gR2 := gA2 + gG2;
  gY2 := gT2 + gC2;

  gA := (gA1+gA2)/2;
  gT := (gT1+gT2)/2;
  gC := (gC1+gC2)/2;
  gG := (gG1+gG2)/2;
  gR := (gR1+gR2)/2;
  gY := (gY1+gY2)/2;

  P1 := (SubstMat[A][G]+SubstMat[G][A])/NSites;
  P2 := (SubstMat[T][C]+SubstMat[C][T])/NSites;
  Q  := ( SubstMat[A][T]+SubstMat[A][C]+SubstMat[G][T]+SubstMat[G][C]
         +SubstMat[T][A]+SubstMat[C][A]+SubstMat[T][G]+SubstMat[C][G])/NSites;

  if (gR1<0) or (gY1 <0) or (gR2<0) or (gY2 <0) then
  begin
    InternalErrId := HC_Tamura_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tamura-Nei distance could not be computed';
    exit;
  end;

  k1 := 2*gA*gG/gR;
  k2 := 2*gT*gC/gY;
  k3 := 2*(gR*gY -gA*gG*gY/gR -gT*gC*gR/gY);
  k4 := 2*(gA*gG +gT*gC + gR*gY);

  if (k1<0) or (k2<0) then
  begin
    InternalErrId := HC_Tamura_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tamura-Nei distance could not be computed';
    exit;
  end;

  w1 := 1 - P1*gR/(gA1*gG2+gA2*gG1) - Q/2/gR;
  w2 := 1 - P2*gY/(gT1*gC2+gT2*gC1) - Q/2/gY;
  w3 := 1 - Q/(gR1*gY2+gR2*gY1);

  if (w1 < 0.01) or (w2 < 0.01) or (w3 < 0.01) then
  begin
    InternalErrId := HC_Tamura_Nei_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'Tamura-Nei distance could not be computed';
    exit;
  end;

  vx := 0;
  sx := 0;
  case DistComponent of
    gdNone:        Dist1 := (k1*power(w1, -gInv) +k2*power(w2, -gInv) + k3*power(w3, -gInv) - k4)*GammaPara;
    gdNucTsOnly:   Dist1 := (k1*power(w1, -gInv) +k2*power(w2, -gInv) +(k3 - 2*gR*gY)*power(w3, -gInv) - (k4 - 2*gR*gY))*GammaPara;
    gdNucTvOnly:   Dist1 := 2*gR*gY*(power(w3, -gInv) -1)*GammaPara;
    gdNucRatioTsTv:
      begin
        sx := (k1*power(w1, -gInv) +k2*power(w2, -gInv) +(k3 - 2*gR*gY)*power(w3, -gInv) - (k4 - 2*gR*gY))*GammaPara;
        vx := 2*gR*gY*(power(w3, -gInv)-1)*GammaPara;
        Dist1 := sx/vx;
      end;
  end;
end;

procedure TNucDist.ComputeTamKumLogDetDist(NDiff, NSites: LongInt);
type
  TDNAMatrix = array[0..3,0..3] of double;

const
  tiny = 1.0e-15;

  function Det(F: TDNAMatrix):double;
  var
     k,j,imax,i: integer;
     sum,dum,big: double;
     vv: array[0..3] of double;
  begin
     result := 1.0;
     for i := 0 to 3 do
     begin
        big := 0.0;
        for j := 0 to 3 do
           if abs(F[i,j]) > big then
              big := abs(F[i,j]);
        if abs(big) < tiny then
        begin
          result := 0;
          exit;
        end;
        vv[i] := 1.0/big
     end;
     for j := 0 to 3 do
     begin
        for i := 0 to j-1 do
        begin
           sum := F[i,j];
           for k := 0 to i-1 do
              sum := sum-F[i,k]*F[k,j];
           F[i,j] := sum
        end;
        big := 0.0;
        imax := j;
        for i := j to 3 do
        begin
           sum := F[i,j];
           for k := 0 to j-1 do
              sum := sum-F[i,k]*F[k,j];
           F[i,j] := sum;
           dum := vv[i]*abs(sum);
           if dum >= big then
           begin
              big := dum;
              imax := i
           end;
        end;
        if j <> imax then
        begin
           for k := 0 to 3 do
           begin
              dum := F[imax,k];
              F[imax,k] := F[j,k];
              F[j,k] := dum
           end;
           result := -result;
           vv[imax] := vv[j]
        end;
        if F[j,j] = 0.0 then
           F[j,j] := tiny;
        if j <> 3 then
        begin
           dum := 1.0/F[j,j];
           for i := j+1 to 3 do
              F[i,j] := F[i,j]*dum;
        end;
     end;
     for i := 0 to 3 do
        result := result*F[i,i];
  end;

var
  f1,f2: array[0..3] of double;
  X,F: TDNAMatrix;
  i,j: integer;
  pA,pT,pC,pG, DetF, DetX: double;
begin
  F[A,A] := SubstMat[A][A]/NSites; F[A,T] := SubstMat[A][T]/NSites; F[A,C] := SubstMat[A][C]/NSites; F[A,G] := SubstMat[A][G]/NSites;
  F[T,A] := SubstMat[T][A]/NSites; F[T,T] := SubstMat[T][T]/NSites; F[T,C] := SubstMat[T][C]/NSites; F[T,G] := SubstMat[T][G]/NSites;
  F[C,A] := SubstMat[C][A]/NSites; F[C,T] := SubstMat[C][T]/NSites; F[C,C] := SubstMat[C][C]/NSites; F[C,G] := SubstMat[C][G]/NSites;
  F[G,A] := SubstMat[G][A]/NSites; F[G,T] := SubstMat[G][T]/NSites; F[G,C] := SubstMat[G][C]/NSites; F[G,G] := SubstMat[G][G]/NSites;
  f1[A] := (F[A,A]+F[A,T]+F[A,C]+F[A,G]);
  f1[T] := (F[T,T]+F[T,A]+F[T,C]+F[T,G]);
  f1[C] := (F[C,C]+F[C,A]+F[C,T]+F[C,G]);
  f1[G] := (F[G,G]+F[G,A]+F[G,T]+F[G,C]);
  f2[A] := (F[A,A]+F[T,A]+F[C,A]+F[G,A]);
  f2[T] := (F[T,T]+F[A,T]+F[C,T]+F[G,T]);
  f2[C] := (F[C,C]+F[A,C]+F[T,C]+F[G,C]);
  f2[G] := (F[G,G]+F[A,G]+F[T,G]+F[C,G]);

  pA := (f1[A]+f2[A])/2;
  pT := (f1[T]+f2[T])/2;
  pC := (f1[C]+f2[C])/2;
  pG := (f1[G]+f2[G])/2;

  for i := A to G do
    for j := A to G do
      if i = j then
        X[i,i] := f1[i]*f2[i]
      else
        X[i,j] := 0;

  DetF := Det(F);
  DetX := Det(X);

  if (DetF < tiny) or (DetX < tiny) then
  begin
    InternalErrId := HC_LogDet_Distance_Could_Not_Be_Computed;
    InternalErrStr:= 'LogDet distance could not be computed';
    exit;
  end;

  Dist1 := (1 -pA*pA-pT*pT-pC*pC-pG*pG)/3*(-ln(DetF)+(ln(DetX))/2);
end;

procedure TNucDist.AddProgressIncrement10;
var
  i: integer;
begin
  for i := 1 to 10 do
    AddProgressIncrement;
end;

procedure TNucDist.ComputeTamNeiDistSimultaneous(D: PDistanceMatrix; IncProgress: boolean);
var
  //LastUpdateTime: TDateTime;
  NumComparisons: LongInt;

  function P1(i,j: integer): double;
  begin
    result := PMat[i,j];
  end;

  function P2(i,j: integer): double;
  begin
    result := PMat[j,i];
  end;

  function Q(i,j: integer): double;
  begin
    result := QMat[i,j];
  end;

  function R(i,j: integer): double;
  begin
    result := 1-(PMat[i,j]+PMat[j,i]+QMat[i,j]);
  end;

  function b(i,j: integer): Double;
  begin
    result := QMat[j,i];
  end;

  function EP1(i,j: integer):double;
  begin
    if pR > 0 then
      result := 2*pA*pG/pR*(pR +pY*exp(-2*b(i,j)) -exp(-2*(pR*k1 +pY)*b(i,j)))
    else
      result := 0;
  end;

  function EP2(i,j: integer):double;
  begin
    if pY > 0 then
      result := 2*pT*pC/pY*(pY +pR*exp(-2*b(i,j)) -exp(-2*(pY*k2 +pR)*b(i,j)))
    else
      result := 0;
  end;

  function EQ(i,j: integer):double;
  begin
    result := 2*pR*pY*(1 -exp(-2*b(i,j)));
  end;

  function dP1dk1(i,j: integer):double;
  begin
    result := 4*pA*pG*exp(-2*(pR*k1 +pY)*b(i,j))*b(i,j);
  end;

  function dP1dx(i,j: integer):double;
  begin
    result := -4*pA*pG/pR*(pY*exp(-2*b(i,j)) -(pR*k1 +pY)*exp(-2*(pR*k1 +pY)*b(i,j)));
  end;

  function dP2dk2(i,j: integer):double;
  begin
    result := 4*pT*pC*exp(-2*(pY*k2 +pR)*b(i,j))*b(i,j);
  end;

  function dP2dx(i,j: integer):double;
  begin
    result := -4*pT*pC/pY*(pR*exp(-2*b(i,j)) -(pY*k2 +pR)*exp(-2*(pY*k2 +pR)*b(i,j)));
  end;

  function dQdx(i,j: integer):double;
  begin
    result := 4*pR*pY*exp(-2*b(i,j));
  end;

  function ddP1dx(i,j: integer):double;
  begin
    result := 8*pA*pG/pR*(pY*exp(-2*b(i,j)) -(pR*k1 +pY)*(pR*k1 +pY)*exp(-2*(pR*k1 +pY)*b(i,j)));
  end;

  function ddP1dk1(i,j: integer):double;
  begin
    result := -8*pR*pA*pG*exp(-2*(pR*k1 +pY)*b(i,j))*b(i,j)*b(i,j);
  end;

  function ddP2dx(i,j: integer):double;
  begin
    result := 8*pT*pC/pY*(pR*exp(-2*b(i,j)) -(pY*k2 +pR)*(pY*k2 +pR)*exp(-2*(pY*k2 +pR)*b(i,j)));
  end;

  function ddP2dk2(i,j: integer):double;
  begin
    result := -8*pY*pT*pC*EXP(-2*(pY*k2 +pR)*b(i,j))*b(i,j)*b(i,j);
  end;

  function ddQdx(i,j: integer):double;
  begin
    result := -8*pR*pY*exp(-2*b(i,j));
  end;

  function IterateDistance(i,j: integer):double;
  var
    dL,ddL,b0,se0,se1: double;

    procedure ComputeDifferentials;
    var
      dP1,dP2,dQ,dR,ddP1,ddP2,ddQ,ddR,ER,s: double;
    begin
      dL   := 0.0;
      ddL  := 0.0;
      ER   := 1;
      dP1  := 0;
      ddP1 := 0;
      dP2  := 0;
      ddP2 := 0;
      dQ   := 0;
      ddQ  := 0;

      s := EP1(i,j);
      if s > 0 then
      begin
        dP1  := dP1dx(i,j);
        ddP1 := ddP1dx(i,j);
        if p1(i,j) > 0 then
        begin
          dL  := dL  +p1(i,j)*dP1/s;
          ddL := ddL +p1(i,j)*(ddP1 -dP1*dP1/s)/s;
        end;
        ER := ER -s;
      end;
      s := EP2(i,j);
      if s > 0 then
      begin
        dP2  := dP2dx(i,j);
        ddP2 := ddP2dx(i,j);
        if p2(i,j) > 0 then
        begin
          dL  := dL  +p2(i,j)*dP2/s;
          ddL := ddL +p2(i,j)*(ddP2 -dP2*dP2/s)/s;
        end;
        ER := ER -s;
      end;
      s := EQ(i,j);
      if s > 0 then
      begin
        dQ   := dQdx(i,j);
        ddQ  := ddQdx(i,j);
        if Q(i,j) > 0 then
        begin
          dL  := dL  +Q(i,j)*dQ/s;
          ddL := ddL +Q(i,j)*(ddQ -dQ*dQ/s)/s;
        end;
        ER := ER -s;
      end;
      if ER > 0 then
      begin
        dR   := -dP1 -dP2 -dQ;
        ddR  := -ddP1 -ddP2 -ddQ;
        dL  := dL  +r(i,j)*dR/ER;
        ddL := ddL +r(i,j)*(ddR -dR*dR/ER)/ER;
      end;
    end;

  begin
    if QMat[j,i] < 0.000000000001 then
    begin
      result := 0;
      exit;
    end;

    ComputeDifferentials;

    if abs(ddL) > 0.000000000001 then
    begin
      se0 := sqrt(abs(1/ddL/NoOfSites));
      if (QMat[j,i] < dL/ddL) then
        result := 0
      else
        result := QMat[j,i] -dL/ddL;

      if 4*(pA*pG*k1 +pT*pC*k2 +pR*pY)*result > 1.0 then
      begin
        b0 := QMat[j,i];
        QMat[j,i] := result;
        ComputeDifferentials;
        if abs(ddL) > 0.000000000001 then
        begin
          se1 := sqrt(abs(1/ddL/NoOfSites));
          if (se1-se0) > abs(QMat[j,i]-b0) then
            result := b0;
        end
        else
          result := b0;
        QMat[j,i] := b0;
      end;
    end
    else
      result := QMat[j,i];
  end;

  procedure IterateKappa;
  var
    i,j: integer;
    dP1,ddP1,dP2,ddP2,ER,s,dLdk1,ddLdk1,dLdk2,ddLdk2: double;
  begin
    dLdk1  := 0;
    ddLdk1 := 0;
    dLdk2  := 0;
    ddLdk2 := 0;
    for i := 1 to FNoOfSeqs-1 do
    begin
      if ShowInProgressBar then
        if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
          UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));

      for j := 0 to i-1 do
      begin
        dP1  := dP1dk1(i,j);
        ddP1 := ddP1dk1(i,j);
        dP2  := dP2dk2(i,j);
        ddP2 := ddP2dk2(i,j);
        ER := 1;
        s := EP1(i,j);
        if s > 0 then
        begin
          dLdk1  := dLdk1  +p1(i,j)*dP1/s;
          ddLdk1 := ddLdk1 +p1(i,j)*(ddP1 -dP1*dP1/s)/s;
          ER := ER -s;
        end;
        s := EP2(i,j);
        if s > 0 then
        begin
          dLdk2  := dLdk2  +p2(i,j)*dP2/s;
          ddLdk2 := ddLdk2 +p2(i,j)*(ddP2 -dP2*dP2/s)/s;
          ER := ER -s;
        end;
        s := EQ(i,j);
        if s > 0 then
          ER := ER -s;
        if ER > 0 then
        begin
          dLdk1  := dLdk1  -r(i,j)*dP1/ER;
          ddLdk1 := ddLdk1 -r(i,j)*(ddP1 -dP1*dP1/ER)/ER;
          dLdk2  := dLdk2  -r(i,j)*dP2/ER;
          ddLdk2 := ddLdk2 -r(i,j)*(ddP2 -dP2*dP2/ER)/ER;
        end;
      end;
    end;
      if ddLdk1 <> 0 then
        if abs(dLdk1/ddLdk1) < k1 then
          k1 := k1 -dLdk1/ddLdk1;
      if ddLdk2 <> 0 then
        if abs(dLdk2/ddLdk2) < k2 then
          k2 := k2 -dLdk2/ddLdk2;
      if k1 > 1000 then
        k1 := 1000;
      if k2 > 1000 then
        k2 := 1000;
  end;

  function SL:double;
  var
    i,j: integer;
    s,ER: double;
  begin
    result := 0;
    for i := 1 to FNoOfSeqs-1 do
      for j := 0 to i-1 do
      begin
        ER := 1;
        s := EP1(i,j);
        if s > 0 then
        begin
          result := result +p1(i,j)*ln(s);
          ER := ER -s;
        end;
        s := EP2(i,j);
        if s > 0 then
        begin
          result := result +p2(i,j)*ln(s);
          ER := ER -s;
        end;
        s := EQ(i,j);
        if s > 0 then
        begin
          result := result +q(i,j)*ln(s);
          ER := ER -s;
        end;
        if ER > 0 then
          result := result +r(i,j)*ln(ER)
        else
          result := result +r(i,j);
      end;
  end;

var
  Sa1,Sa2,Sb,dL,ddL,mind: double;
  i,j,progress: integer;

begin

  Sa1 := 0;
  Sa2 := 0;
  Sb := 0;

  if ShowInProgressBar then
  begin
    UpdateProgress(0, 'Status', 'Computing TN distances');
    NumComparisons := FNoOfSeqs * (FNoOfSeqs - 1) div 2;
  end;


  for i := 1 to FNoOfSeqs-1 do
  begin
    if ShowInProgressBar then
      if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
        UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));

    for j := 0 to i-1 do
    begin
      QMat[j,i] := p1(i,j)+p2(i,j)+q(i,j);
      if QMat[j,i] > 0 then
      begin
        Sa1 := Sa1 +p1(i,j)/QMat[j,i]/QMat[j,i];
        Sa2 := Sa2 +p2(i,j)/QMat[j,i]/QMat[j,i];
        Sb  := Sb  +q(i,j)/QMat[j,i]/QMat[j,i];
      end;
    end;
  end;
  if (Sb > 0) and (pR > 0) and (pY > 0) then
  begin
    if (pA > 0) and (pG > 0) then
      k1 := (Sa1/pA/pG)/(Sb/pR/pY)
    else
      k1 := 1;
    if (pT > 0) and (pC > 0) then
      k2 := (Sa2/pT/pC)/(Sb/pR/pY)
    else
      k2 := 1;
  end
  else
  begin
    k1 := 1;
    k2 := 1;
  end;

  UpdateProgress(0, 'Status', 'Updating distance matrix');

  for i := 1 to FNoOfSeqs-1 do
  begin
    if ShowInProgressBar then
      if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
        UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));

    for j := 0 to i-1 do
      QMat[j,i] := QMat[j,i]/4/(pA*pG*k1 +pT*pC*k2 +pR*pY);
  end;

  dL := SL;
//  if IncProgress then
//    AddProgressIncrement10;

  if dL < 0 then
  begin
    progress := floor(log10(-dL));
    mind := power(10, progress-8);
    repeat
      UpdateProgress(0);
      for i := 1 to FNoOfSeqs-1 do
      begin
        if ShowInProgressBar then
          if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
            UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));
        for j := 0 to i-1 do
          if QMat[j,i] > 0.000000000001 then
            QMat[j,i] := IterateDistance(i,j);
      end;

      IterateKappa;

      ddL := SL -dL;
      dL := dL +ddL;
      if CheckAbort then
        Exit;
//      try
//        if IncProgress then
//        begin
//          while (ddL > 0) And (log10(ddL) < progress) do
//          begin
//            AddProgressIncrement10;
//            dec(progress);
//            Application.ProcessMessages;
//          end;
//        end;
//      except
//        On E:Exception do
//        begin
//          Dist1 := IDV;
//          Var1  := IDV;
//          if FQuickExit then raise;
//        end;
//      end;

    until ddL < mind;
  end;

  for i:= 1 to FNoOfSeqs-1 do
  begin
    if ShowInProgressBar then
      if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
        UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100), 'Status', 'Finalizing');

    for j:= 0 to i-1 do
    begin
      case DistComponent of
        gdNone:         D[i][j] := 4*(pA*pG*k1 +pT*pC*k2 +pR*pY)*QMat[j,i];
        gdNucTsOnly:    D[i][j] := 4*(pA*pG*k1 +pT*pC*k2)*QMat[j,i];
        gdNucTvOnly:    D[i][j] := 4*pR*pY*QMat[j,i];
        gdNucRatioTsTv: if pR*pY < 0.000000000001 then
                          D[i][j] := -1
                        else
                          // D[i][j] := (pA*pG*k1 +pT*pC*k2)/pR*pY;
                          D[i][j] := (pA*pG*k1+pT*pC*k2)/pR/pY;
      end;
{
      case VarType of
        gdAnalyticalVar:   FUserV[i][j] := Var1;
        gdMonteCarloTest:  FUserV[i][j] := Var1;
      end;
}
    end; // end of for j
  end;// end for i
end;

procedure TNucDist.ComputeTamNeiGammaDistSimultaneous(D: PDistanceMatrix; IncProgress: boolean);
var
  gamma,c1,c2: double;
  NumComparisons: LongInt;
//  LastUpdateTime: TDateTime;

  function P1(i,j: integer): double;
  begin
    result := PMat[i,j];
  end;

  function P2(i,j: integer): double;
  begin
    result := PMat[j,i];
  end;

  function Q(i,j: integer): double;
  begin
    result := QMat[i,j];
  end;

  function R(i,j: integer): double;
  begin
    result := 1-(PMat[i,j]+PMat[j,i]+QMat[i,j]);
  end;

  function b(i,j: integer): Double;
  begin
    result := Qmat[j,i];
  end;

  function f1(i,j: integer):double;
  begin
    result := gamma*ln(gamma/(gamma+c1*b(i,j)));
  end;

  function df1dx(i,j: integer):double;
  begin
    result := -gamma*c1/(gamma +c1*b(i,j));
  end;

  function ddf1dx(i,j: integer):double;
  begin
    result := gamma*c1*c1/(gamma +c1*b(i,j))/(gamma +c1*b(i,j));
  end;

  function df1dk1(i,j: integer):double;
  begin
    result := -2*gamma*pR*b(i,j)/(gamma +c1*b(i,j));
  end;

  function ddf1dk1(i,j: integer):double;
  begin
    result := 4*gamma*pR*pR*b(i,j)*b(i,j)/(gamma +c1*b(i,j))/(gamma +c1*b(i,j));
  end;

  function f2(i,j: integer):double;
  begin
    result := gamma*ln(gamma/(gamma +c2*b(i,j)));
  end;

  function df2dx(i,j: integer):double;
  begin
    result := -gamma*c2/(gamma +c2*b(i,j));
  end;

  function ddf2dx(i,j: integer):double;
  begin
    result := gamma*c2*c2/(gamma +c2*b(i,j))/(gamma +c2*b(i,j));
  end;

  function df2dk2(i,j: integer):double;
  begin
    result := -2*gamma*pY*b(i,j)/(gamma +c2*b(i,j));
  end;

  function ddf2dk2(i,j: integer):double;
  begin
    result := 4*gamma*pY*pY*b(i,j)*b(i,j)/(gamma +c2*b(i,j))/(gamma +c2*b(i,j));
  end;

  function f3(i,j: integer):double;
  begin
    result := gamma*ln(gamma/(gamma +2*b(i,j)));
  end;

  function df3dx(i,j: integer):double;
  begin
    result := -2*gamma/(gamma +2*b(i,j));
  end;

  function ddf3dx(i,j: integer):double;
  begin
    result := 4*gamma/(gamma +2*b(i,j))/(gamma +2*b(i,j));
  end;

  function EP1(i,j: integer):double;
  begin
    if pR > 0 then
      result := 2*pA*pG/pR*(pR -exp(f1(i,j)) +pY*exp(f3(i,j)))
    else
      result := 0;
  end;

  function EP2(i,j: integer):double;
  begin
    if pY > 0 then
      result := 2*pT*pC/pY*(pY -exp(f2(i,j)) +pR*exp(f3(i,j)))
    else
      result := 0;
  end;

  function EQ(i,j: integer):double;
  begin
    result := 2*pR*pY*(1 -exp(f3(i,j)));
  end;

  function dP1dx(i,j: integer):double;
  begin
    result := 2*pA*pG/pR*(-exp(f1(i,j))*df1dx(i,j) +pY*exp(f3(i,j))*df3dx(i,j));
  end;

  function dP1dk1(i,j: integer):double;
  begin
    result := -2*pA*pG/pR*exp(f1(i,j))*df1dk1(i,j);
  end;

  function dP2dx(i,j: integer):double;
  begin
    result := 2*pT*pC/pY*(-exp(f2(i,j))*df2dx(i,j) +pR*exp(f3(i,j))*df3dx(i,j));
  end;

  function dP2dk2(i,j: integer):double;
  begin
    result := -2*pT*pC/pY*exp(f2(i,j))*df2dk2(i,j);
  end;

  function dQdx(i,j: integer):double;
  begin
    result := -2*pR*pY*exp(f3(i,j))*df3dx(i,j);
  end;

  function ddP1dx(i,j: integer):double;
  begin
    result := 2*pA*pG/pR*(-exp(f1(i,j))*(df1dx(i,j)*df1dx(i,j) +ddf1dx(i,j))
                          +pY*exp(f3(i,j))*(df3dx(i,j)*df3dx(i,j) +ddf3dx(i,j)));
  end;

  function ddP1dk1(i,j: integer):double;
  begin
    result := -2*pA*pG/pR*exp(f1(i,j))*(df1dk1(i,j)*df1dk1(i,j) +ddf1dk1(i,j));
  end;

  function ddP2dx(i,j: integer):double;
  begin
    result := 2*pT*pC/pY*(-exp(f2(i,j))*(df2dx(i,j)*df2dx(i,j) +ddf2dx(i,j))
                          +pR*exp(f3(i,j))*(df3dx(i,j)*df3dx(i,j) +ddf3dx(i,j)));
  end;

  function ddP2dk2(i,j: integer):double;
  begin
    result := -2*pT*pC/pY*exp(f2(i,j))*(df2dk2(i,j)*df2dk2(i,j) +ddf2dk2(i,j));
  end;

  function ddQdx(i,j: integer):double;
  begin
    result := -2*pR*pY*exp(f3(i,j))*(df3dx(i,j)*df3dx(i,j) +ddf3dx(i,j));
  end;

  function IterateDistance(i,j: integer):double;
  var
    dL,ddL,b0,se0,se1: double;

    procedure ComputeDifferentials;
    var
      dP1,dP2,dQ,dR,ddP1,ddP2,ddQ,ddR,ER,s: double;
    begin
      ER   := 1;
      dL   := 0;
      ddL  := 0;
      dP1  := 0;
      ddP1 := 0;
      dP2  := 0;
      ddP2 := 0;
      dQ   := 0;
      ddQ  := 0;

      s := EP1(i,j);
      if s > 0 then
      begin
        dP1  := dP1dx(i,j);
        ddP1 := ddP1dx(i,j);
        if p1(i,j) > 0 then
        begin
          dL  := dL  +p1(i,j)*dP1/s;
          ddL := ddL +p1(i,j)*(ddP1 -dP1*dP1/s)/s;
        end;
        ER := ER -s;
      end;
      s := EP2(i,j);
      if s > 0 then
      begin
        dP2  := dP2dx(i,j);
        ddP2 := ddP2dx(i,j);
        if p2(i,j) > 0 then
        begin
          dL  := dL  +p2(i,j)*dP2/s;
          ddL := ddL +p2(i,j)*(ddP2 -dP2*dP2/s)/s;
        end;
        ER := ER -s;
      end;
      s := EQ(i,j);
      if s > 0 then
      begin
        dQ   := dQdx(i,j);
        ddQ  := ddQdx(i,j);
        if Q(i,j) > 0 then
        begin
          dL  := dL  +Q(i,j)*dQ/s;
          ddL := ddL +Q(i,j)*(ddQ -dQ*dQ/s)/s;
        end;
        ER := ER -s;
      end;
      if ER > 0 then
      begin
        dR   := -dP1 -dP2 -dQ;
        ddR  := -ddP1 -ddP2 -ddQ;
        dL  := dL  +r(i,j)*dR/ER;
        ddL := ddL +r(i,j)*(ddR -dR*dR/ER)/ER;
      end;
    end;

  begin
    if QMat[j,i] < 0.000000000001 then
    begin
      result := 0;
      exit;
    end;

    ComputeDifferentials;

    if abs(ddL) > 0.000000000001 then
    begin
      se0 := sqrt(abs(1/ddL/NoOfSites));
      if (QMat[j,i] < dL/ddL) then
        result := 0
      else
        result := QMat[j,i] -dL/ddL;

      if 4*(pA*pG*k1 +pT*pC*k2 +pR*pY)*result > 1.0 then
      begin
        b0 := QMat[j,i];
        QMat[j,i] := result;
        ComputeDifferentials;
        if abs(ddL) > 0.000000000001 then
        begin
          se1 := sqrt(abs(1/ddL/NoOfSites));
          if (se1-se0) > abs(QMat[j,i]-b0) then
            result := b0;
        end
        else
          result := b0;
        QMat[j,i] := b0;
      end;
    end
    else
      result := QMat[j,i];
  end;

  procedure IterateKappa;
  var
    i,j: integer;
    dP1,ddP1,dP2,ddP2,ER,s,dLdk1,ddLdk1,dLdk2,ddLdk2: double;
  begin
    dLdk1  := 0;
    ddLdk1 := 0;
    dLdk2  := 0;
    ddLdk2 := 0;
    for i := 1 to FNoOfSeqs-1 do
    begin
      if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
        UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));

      for j := 0 to i-1 do
      begin
        dP1  := 0;
        ddP1 := 0;
        dP2  := 0;
        ddP2 := 0;
        ER := 1;
        s := EP1(i,j);
        if s > 0 then
        begin
          dP1  := dP1dk1(i,j);
          ddP1 := ddP1dk1(i,j);
          if p1(i,j) > 0 then
          begin
            dLdk1  := dLdk1  +p1(i,j)*dP1/s;
            ddLdk1 := ddLdk1 +p1(i,j)*(ddP1 -dP1*dP1/s)/s;
          end;
          ER := ER -s;
        end;
        s := EP2(i,j);
        if s > 0 then
        begin
          dP2  := dP2dk2(i,j);
          ddP2 := ddP2dk2(i,j);
          if p2(i,j) > 0 then
          begin
            dLdk2  := dLdk2  +p2(i,j)*dP2/s;
            ddLdk2 := ddLdk2 +p2(i,j)*(ddP2 -dP2*dP2/s)/s;
          end;
          ER := ER -s;
        end;
        s := EQ(i,j);
        if s > 0 then
          ER := ER -s;
        if ER > 0 then
        begin
          dLdk1  := dLdk1  -r(i,j)*dP1/ER;
          ddLdk1 := ddLdk1 -r(i,j)*(ddP1 -dP1*dP1/ER)/ER;
          dLdk2  := dLdk2  -r(i,j)*dP2/ER;
          ddLdk2 := ddLdk2 -r(i,j)*(ddP2 -dP2*dP2/ER)/ER;
        end;
      end;
    end;
      if ddLdk1 <> 0 then
        if abs(dLdk1/ddLdk1) < k1 then
          k1 := k1 -dLdk1/ddLdk1;
      if ddLdk2 <> 0 then
        if abs(dLdk2/ddLdk2) < k2 then
          k2 := k2 -dLdk2/ddLdk2;

      if k1 > 1000 then
        k1 := 1000;
      if k2 > 1000 then
        k2 := 1000;

      c1 := 2*(pR*k1 +pY);
      c2 := 2*(pY*k2 +pR);
  end;

  function SL:double;
  var
    i,j: integer;
    s,ER: double;
  begin
    result := 0;
    for i := 1 to FNoOfSeqs-1 do
      for j := 0 to i-1 do
      begin
        ER := 1;
        s := EP1(i,j);
        if s > 0 then
        begin
          if p1(i,j) > 0 then
            result := result +p1(i,j)*ln(s);
          ER := ER -s;
        end;
        s := EP2(i,j);
        if s > 0 then
        begin
          if p2(i,j) > 0 then
            result := result +p2(i,j)*ln(s);
          ER := ER -s;
        end;
        s := EQ(i,j);
        if s > 0 then
        begin
          if Q(i,j) > 0 then
            result := result +q(i,j)*ln(s);
          ER := ER -s;
        end;
        if ER > 0 then
          result := result +r(i,j)*ln(ER)
        else
          result := result +r(i,j);
      end;
  end;

var
  Sa1,Sa2,Sb,dL,ddL,mind: double;
  i,j,progress: integer;

begin
  Sa1 := 0;
  Sa2 := 0;
  Sb := 0;
  UpdateProgress(0, 'Status', 'Computing TN distances');

  NumComparisons := FNoOfSeqs * (FNoOfSeqs - 1) div 2;
  LastUpdateTime := Time;
  for i := 1 to FNoOfSeqs-1 do
  begin
    if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
      UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));
    for j := 0 to i-1 do
    begin
      QMat[j,i] := p1(i,j)+p2(i,j)+q(i,j);
      if QMat[j,i] > 0 then
      begin
        Sa1 := Sa1 +p1(i,j)/QMat[j,i]/QMat[j,i];
        Sa2 := Sa2 +p2(i,j)/QMat[j,i]/QMat[j,i];
        Sb  := Sb  +q(i,j)/QMat[j,i]/QMat[j,i];
      end;
    end;
  end;
  if (Sb > 0) and (pR > 0) and (pY > 0) then
  begin
    if (pA > 0) and (pG > 0) then
      k1 := (Sa1/pA/pG)/(Sb/pR/pY)
    else
      k1 := 1;
    if (pT > 0) and (pC > 0) then
      k2 := (Sa2/pT/pC)/(Sb/pR/pY)
    else
      k2 := 1;
  end
  else
  begin
    k1 := 1;
    k2 := 1;
  end;

  gamma := GammaPara;
  c1 := 2*(pR*k1 +pY);
  c2 := 2*(pY*k2 +pR);

  UpdateProgress(0, 'Status', 'Updating distance matrix');
  for i := 1 to FNoOfSeqs-1 do
  begin
    if ShowInProgressBar then
      if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
        UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));

    for j := 0 to i-1 do
      QMat[j,i] := QMat[j,i]/4/(pA*pG*k1 +pT*pC*k2 +pR*pY);
  end;

  dL := SL;
//  if IncProgress then
//    AddProgressIncrement10;

  if dL < 0 then
  begin
    progress := floor(log10(-dL));
    mind := power(10, progress-8);
  end;

  UpdateProgress(0, 'Status', 'Iterating distances');
  repeat
    UpdateProgress(0);
    for i := 1 to FNoOfSeqs-1 do
    begin
      if ShowInProgressBar then
        if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
          UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));
      for j := 0 to i-1 do
        if QMat[j,i] > 0.000000000001 then
          QMat[j,i] := IterateDistance(i,j);
    end;

    IterateKappa;
{
    if k1 > 0.000000000001 then
    begin
      k1 := k1 -dLdk1/ddLdk1;
      c1 := 2*(pR*k1 +pY);
    end;
    if k2 > 0.000000000001 then
    begin
      k2 := k2 -dLdk2/ddLdk2;
      c2 := 2*(pY*k2 +pR);
    end;
}
    ddL := SL -dL;
    dL := dL +ddL;
    if CheckAbort then
      Exit;
//    try
//      if IncProgress then
//      begin
//        while (ddL > 0) and (log10(ddL) < progress) do
//        begin
//          AddProgressIncrement10;
//          dec(progress);
//        end;
//      end;
//    except
//      On E:Exception do
//      begin
//        Dist1 := IDV;
//        Var1  := IDV;
//        if FQuickExit then raise;
//      end;
//    end;

  until ddL < mind;

  UpdateProgress(0, 'Status', 'Finalizing');
  for i:= 1 to FNoOfSeqs-1 do
  begin
    if ShowInProgressBar then    
      if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
        UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));
    for j:= 0 to i-1 do
    begin
      case DistComponent of
        gdNone:         D[i][j] := 4*(pA*pG*k1 +pT*pC*k2 +pR*pY)*QMat[j,i];
        gdNucTsOnly:    D[i][j] := 4*(pA*pG*k1 +pT*pC*k2)*QMat[j,i];
        gdNucTvOnly:    D[i][j] := 4*pR*pY*QMat[j,i];
        gdNucRatioTsTv: if pR*pY < 0.000000000001 then
                          D[i][j] := -1
                        else
                          // D[i][j] := (pA*pG*k1 +pT*pC*k2)/pR*pY;
                          D[i][j] := (pA*pG*k1+pT*pC*k2)/pR/pY;
      end;
{
      case VarType of
        gdAnalyticalVar:   FUserV[i][j] := Var1;
        gdMonteCarloTest:  FUserV[i][j] := Var1;
      end;
}
    end; // end of for j
  end;// end for i
end;

procedure TNucDist.ComputeTamNeiHeteroDistSimultaneous(D: PDistanceMatrix; IncProgress: boolean);             // new
var
  f1,f2,f3,Sa1,Sa2,Sb,dL,ddL,mind: double;
  i,j,progress: integer;
//  LastUpdateTime: TDateTime;
  NumComparisons: LongInt;

  function P1(i,j: integer): double;
  begin
    result := PMat[i,j];
  end;

  function P2(i,j: integer): double;
  begin
    result := PMat[j,i];
  end;

  function Q(i,j: integer): double;
  begin
    result := QMat[i,j];
  end;

  function R(i,j: integer): double;
  begin
    result := 1-(PMat[i,j]+PMat[j,i]+QMat[i,j]);
  end;

  function b(i,j: integer): Double;
  begin
    result := Qmat[j,i];
  end;

  function EP1(i,j: integer):double;
  var
    f1,f3,pR,pY: double;
  begin
    pR := (fA[i]+fG[i]+fA[j]+fG[j])/2;
    if pR > 0 then
    begin
      pY := (fT[i]+fC[i]+fT[j]+fC[j])/2;
      f1 := (fA[i]*fG[j]+fA[j]*fG[i])/pR;
      f3 := ((fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]))/pR/2;
      result := f1*(1-f3 +f3*exp(-2*b(i,j)) -exp(-2*(pR*k1 +pY)*b(i,j)));
    end
    else
      result := 0;
  end;

  function EP2(i,j: integer):double;
  var
    f2,f3,pR,pY: double;
  begin
    pY := (fT[i]+fC[i]+fT[j]+fC[j])/2;
    if pY > 0 then
    begin
      pR := (fA[i]+fG[i]+fA[j]+fG[j])/2;
      f2 := (fT[i]*fC[j]+fT[j]*fC[i])/pY;
      f3 := ((fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]))/pY/2;
      result := f2*(1-f3 +f3*exp(-2*b(i,j)) -exp(-2*(pY*k2 +pR)*b(i,j)));
    end
    else
      result := 0;
  end;

  function EQ(i,j: integer):double;
  var
    f3: double;
  begin
    f3 := (fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]);
    result := f3*(1 -exp(-2*b(i,j)));
  end;

  function dP1dk1(i,j: integer):double;
  var
    f1,pR,pY: double;
  begin
    pR := (fA[i]+fG[i]+fA[j]+fG[j])/2;
    pY := (fT[i]+fC[i]+fT[j]+fC[j])/2;
    f1 := fA[i]*fG[j]+fA[j]*fG[i];
    result := 2*f1*exp(-2*(pR*k1 +pY)*b(i,j))*b(i,j);
  end;

  function dP1dx(i,j: integer):double;
  var
    f1,f3,pR,pY: double;
  begin
    pR := (fA[i]+fG[i]+fA[j]+fG[j])/2;
    pY := (fT[i]+fC[i]+fT[j]+fC[j])/2;
    f1 := (fA[i]*fG[j]+fA[j]*fG[i])/pR;
    f3 := ((fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]))/pR;
    result := -f1*(f3*exp(-2*b(i,j)) -2*(pR*k1 +pY)*exp(-2*(pR*k1 +pY)*b(i,j)));
  end;

  function dP2dk2(i,j: integer):double;
  var
    f2,pR,pY: double;
  begin
    pR := (fA[i]+fG[i]+fA[j]+fG[j])/2;
    pY := (fT[i]+fC[i]+fT[j]+fC[j])/2;
    f2 := fT[i]*fC[j]+fT[j]*fC[i];
    result := 2*f2*exp(-2*(pY*k2 +pR)*b(i,j))*b(i,j);
  end;

  function dP2dx(i,j: integer):double;
  var
    f2,f3,pR,pY: double;
  begin
    pR := (fA[i]+fG[i]+fA[j]+fG[j])/2;
    pY := (fT[i]+fC[i]+fT[j]+fC[j])/2;
    f2 := (fT[i]*fC[j]+fT[j]*fC[i])/pY;
    f3 := ((fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]))/pY;
    result := -f2*(f3*exp(-2*b(i,j)) -2*(pY*k2 +pR)*exp(-2*(pY*k2 +pR)*b(i,j)));
  end;

  function dQdx(i,j: integer):double;
  var
    f3: double;
  begin
    f3 := (fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]);
    result := 2*f3*exp(-2*b(i,j));
  end;

  function ddP1dx(i,j: integer):double;
  var
    f1,f3,pR,pY: double;
  begin
    pR := (fA[i]+fG[i]+fA[j]+fG[j])/2;
    pY := (fT[i]+fC[i]+fT[j]+fC[j])/2;
    f1 := (fA[i]*fG[j]+fA[j]*fG[i])/pR;
    f3 := ((fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]))/pR;
    result := 2*f1*(f3*exp(-2*b(i,j)) -2*(pR*k1 +pY)*(pR*k1 +pY)*exp(-2*(pR*k1 +pY)*b(i,j)));
  end;

  function ddP1dk1(i,j: integer):double;
  var
    f1,pR,pY: double;
  begin
    f1 := fA[i]*fG[j]+fA[j]*fG[i];
    pR := (fA[i]+fG[i]+fA[j]+fG[j])/2;
    pY := (fT[i]+fC[i]+fT[j]+fC[j])/2;
    result := -4*f1*pR*exp(-2*(pR*k1 +pY)*b(i,j))*b(i,j)*b(i,j);
  end;

  function ddP2dx(i,j: integer):double;
  var
    f2,f3,pR,pY: double;
  begin
    f2 := fT[i]*fC[j]+fT[j]*fC[i];
    f3 := (fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]);
    pR := (fA[i]+fG[i]+fA[j]+fG[j])/2;
    pY := (fT[i]+fC[i]+fT[j]+fC[j])/2;
    result := 2*f2/pY*(f3/pY*exp(-2*b(i,j)) -2*(pY*k2 +pR)*(pY*k2 +pR)*exp(-2*(pY*k2 +pR)*b(i,j)));
  end;

  function ddP2dk2(i,j: integer):double;
  var
    f2,pR,pY: double;
  begin
    f2 := fT[i]*fC[j]+fT[j]*fC[i];
    pR := (fA[i]+fG[i]+fA[j]+fG[j])/2;
    pY := (fT[i]+fC[i]+fT[j]+fC[j])/2;
    result := -4*f2*pY*exp(-2*(pY*k2 +pR)*b(i,j))*b(i,j)*b(i,j);
  end;

  function ddQdx(i,j: integer):double;
  var
    f3: double;
  begin
    f3 := (fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]);
    result := -4*f3*exp(-2*b(i,j));
  end;

  function IterateDistance(i,j: integer):double;
  var
    dP1,dP2,dQ,dR,ddP1,ddP2,ddQ,ddR,ER,s,dL,ddL: double;
  begin
    if QMat[j,i] < 0.000000000001 then
    begin
      result := 0;
      exit;
    end;
    dL   := 0.0;
    ddL  := 0.0;
    ER   := 1;
    dP1  := 0;
    ddP1 := 0;
    dP2  := 0;
    ddP2 := 0;
    dQ   := 0;
    ddQ  := 0;

    s := EP1(i,j);
    if s > 0 then
    begin
      dP1  := dP1dx(i,j);
      ddP1 := ddP1dx(i,j);
      if p1(i,j) > 0 then
      begin
        dL  := dL  +p1(i,j)*dP1/s;
        ddL := ddL +p1(i,j)*(ddP1 -dP1*dP1/s)/s;
      end;
      ER := ER -s;
    end;
    s := EP2(i,j);
    if s > 0 then
    begin
      dP2  := dP2dx(i,j);
      ddP2 := ddP2dx(i,j);
      if p2(i,j) > 0 then
      begin
        dL  := dL  +p2(i,j)*dP2/s;
        ddL := ddL +p2(i,j)*(ddP2 -dP2*dP2/s)/s;
      end;
      ER := ER -s;
    end;
    s := EQ(i,j);
    if s > 0 then
    begin
      dQ   := dQdx(i,j);
      ddQ  := ddQdx(i,j);
      if Q(i,j) > 0 then
      begin
        dL  := dL  +Q(i,j)*dQ/s;
        ddL := ddL +Q(i,j)*(ddQ -dQ*dQ/s)/s;
      end;
      ER := ER -s;
    end;
    if ER > 0 then
    begin
      dR   := -dP1 -dP2 -dQ;
      ddR  := -ddP1 -ddP2 -ddQ;
      dL  := dL  +r(i,j)*dR/ER;
      ddL := ddL +r(i,j)*(ddR -dR*dR/ER)/ER;
    end;
    if abs(ddL) > 0.000000000001 then
      if (QMat[j,i] > dL/ddL) then
        result := QMat[j,i] -dL/ddL
      else
        result := 0
    else
      result := QMat[j,i];
  end;

  procedure IterateKappa;
  var
    i,j: integer;
    dP1,ddP1,dP2,ddP2,ER,s,dLdk1,ddLdk1,dLdk2,ddLdk2: double;
  begin
    dLdk1  := 0;
    ddLdk1 := 0;
    dLdk2  := 0;
    ddLdk2 := 0;

    for i := 1 to FNoOfSeqs-1 do
    begin
      if ShowInProgressBar then      
        if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
          UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));
      for j := 0 to i-1 do
      begin
        dP1  := 0;
        ddP1 := 0;
        dP2  := 0;
        ddP2 := 0;
        ER := 1;
        s := EP1(i,j);
        if s > 0 then
        begin
          dP1  := dP1dk1(i,j);
          ddP1 := ddP1dk1(i,j);
          if p1(i,j) > 0 then
          begin
            dLdk1  := dLdk1  +p1(i,j)*dP1/s;
            ddLdk1 := ddLdk1 +p1(i,j)*(ddP1 -dP1*dP1/s)/s;
          end;
          ER := ER -s;
        end;
        s := EP2(i,j);
        if s > 0 then
        begin
          dP2  := dP2dk2(i,j);
          ddP2 := ddP2dk2(i,j);
          if p2(i,j) > 0 then
          begin
            dLdk2  := dLdk2  +p2(i,j)*dP2/s;
            ddLdk2 := ddLdk2 +p2(i,j)*(ddP2 -dP2*dP2/s)/s;
          end;
          ER := ER -s;
        end;
        s := EQ(i,j);
        if s > 0 then
          ER := ER -s;
        if ER > 0 then
        begin
          dLdk1  := dLdk1  -r(i,j)*dP1/ER;
          ddLdk1 := ddLdk1 -r(i,j)*(ddP1 -dP1*dP1/ER)/ER;
          dLdk2  := dLdk2  -r(i,j)*dP2/ER;
          ddLdk2 := ddLdk2 -r(i,j)*(ddP2 -dP2*dP2/ER)/ER;
        end;
      end;
    end;
    if ddLdk1 <> 0 then
      if abs(dLdk1/ddLdk1) < k1 then
        k1 := k1 -dLdk1/ddLdk1;
    if ddLdk2 <> 0 then
      if abs(dLdk2/ddLdk2) < k2 then
        k2 := k2 -dLdk2/ddLdk2;
    if k1 > 1000 then
      k1 := 1000;
    if k2 > 1000 then
      k2 := 1000;
  end;

  function SL:double;
  var
    i,j: integer;
    s,ER: double;
  begin
    result := 0;
    for i := 1 to FNoOfSeqs-1 do
      for j := 0 to i-1 do
      begin
        ER := 1;
        s := EP1(i,j);
        if s > 0 then
        begin
          result := result +p1(i,j)*ln(s);
          ER := ER -s;
        end;
        s := EP2(i,j);
        if s > 0 then
        begin
          result := result +p2(i,j)*ln(s);
          ER := ER -s;
        end;
        s := EQ(i,j);
        if s > 0 then
        begin
          result := result +q(i,j)*ln(s);
          ER := ER -s;
        end;
        if ER > 0 then
          result := result +r(i,j)*ln(ER)
        else
          result := result +r(i,j);
      end;
  end;

begin
  Sa1 := 0;
  Sa2 := 0;
  Sb := 0;
  if ShowInProgressBar then
  begin
    UpdateProgress(0, 'Status', 'Computing TN hetero distances');
    NumComparisons := FNoOfSeqs * (FNoOfSeqs - 1) div 2;
  end;

  for i := 1 to FNoOfSeqs-1 do
  begin
    if ShowInProgressBar then    
      if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
        UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));
    for j := 0 to i-1 do
    begin
      QMat[j,i] := p1(i,j)+p2(i,j)+q(i,j);
      f1 := fA[i]*fG[j]+fA[j]*fG[i];
      f2 := fT[i]*fC[j]+fT[j]*fC[i];
      f3 := (fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]);
      if (QMat[j,i] > 0) and (f1 > 0) and (f2 > 0) and (f3 > 0) then
      begin
        Sa1 := Sa1 +p1(i,j)/QMat[j,i]/QMat[j,i];
        Sa2 := Sa2 +p2(i,j)/QMat[j,i]/QMat[j,i];
        Sb  := Sb  +q(i,j)/QMat[j,i]/QMat[j,i];
      end;
    end;
  end;
  if (Sb > 0) and (pR > 0) and (pY > 0) then
  begin
    if (pA > 0) and (pG > 0) then
      k1 := (Sa1/pA/pG)/(Sb/pR/pY)
    else
      k1 := 1;
    if (pT > 0) and (pC > 0) then
      k2 := (Sa2/pT/pC)/(Sb/pR/pY)
    else
      k2 := 1;
  end
  else
  begin
    k1 := 1;
    k2 := 1;
  end;

  UpdateProgress(0, 'Status', 'Updating distance matrix');

  for i := 1 to FNoOfSeqs-1 do
  begin
    if ShowInProgressBar then    
      if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
        UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));
    for j := 0 to i-1 do
    begin
      f1 := (fA[i]+fA[j])*(fG[i]+fG[j]);
      f2 := (fT[i]+fT[j])*(fC[i]+fC[j]);
      f3 := (fA[i]+fA[j]+fG[i]+fG[j])*(fT[i]+fT[j]+fC[i]+fC[j]);
      QMat[j,i] := QMat[j,i]/(f1*k1 +f2*k2 +f3);
    end;
  end;

  dL := SL;
//  if IncProgress then
//    AddProgressIncrement10;

  UpdateProgress(0, 'Status', 'Iterating distances');
  progress := floor(log10(-dL));
  mind := power(10, progress-8);
  repeat
    UpdateProgress(0);
    for i := 1 to FNoOfSeqs-1 do
    begin
      if ShowInProgressBar then
        if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
          UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));
      for j := 0 to i-1 do
        if QMat[j,i] > 0.000000000001 then
          QMat[j,i] := IterateDistance(i,j);
    end;
    if ShowInProgressBar then
      if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
        UpdateProgress(0);
    IterateKappa;
{
    if k1 > 0.000000000001 then
      k1 := k1 -dLdk1/ddLdk1;
    if k2 > 0.000000000001 then
      k2 := k2 -dLdk2/ddLdk2;
}
    ddL := SL -dL;
    dL := dL +ddL;
    if CheckAbort then
      Exit;
//    try
//      if IncProgress then
//      begin
//        while (ddL > 0) and (log10(ddL) < progress) do
//        begin
//          AddProgressIncrement10;
//          dec(progress);
//          Application.ProcessMessages;
//        end;
//      end;
//    except
//      On E:Exception do
//      begin
//        Dist1 := IDV;
//        Var1  := IDV;
//        if FQuickExit then raise;
//      end;
//    end;

  until ddL < mind;

  UpdateProgress(0, 'Status', 'Finalizing');

  for i:= 1 to FNoOfSeqs-1 do
  begin
    if ShowInProgressBar then    
      if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
        UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));
    for j:= 0 to i-1 do
    begin
      f1 := (fA[i]+fA[j])*(fG[i]+fG[j]);
      f2 := (fT[i]+fT[j])*(fC[i]+fC[j]);
      f3 := (fA[i]+fA[j]+fG[i]+fG[j])*(fT[i]+fT[j]+fC[i]+fC[j]);
      case DistComponent of
        gdNone:         D[i][j] := (f1*k1 +f2*k2 +f3)*b(i,j);
        gdNucTsOnly:    D[i][j] := (f1*k1 +f2*k2)*b(i,j);
        gdNucTvOnly:    D[i][j] := f3*b(i,j);
        gdNucRatioTsTv: if f3 < 0.000000000001 then
                          D[i][j] := -1
                        else
                          D[i][j] := (f1*k1 +f2*k2)/f3;
      end;
{
      case VarType of
        gdAnalyticalVar:   FUserV[i][j] := Var1;
        gdMonteCarloTest:  FUserV[i][j] := Var1;
      end;
}
    end; // end of for j
  end;// end for i
end;

procedure TNucDist.ComputeTamNeiGammaHeteroDistSimultaneous(D: PDistanceMatrix; IncProgress: boolean);        // new
var
  gamma: double;
  g1,g2,g3,Sa1,Sa2,Sb,dL,ddL,mind: double;
  i,j,progress: integer;

  NumComparisons: LongInt;

  function P1(i,j: integer): double;
  begin
    result := PMat[i,j];
  end;

  function P2(i,j: integer): double;
  begin
    result := PMat[j,i];
  end;

  function Q(i,j: integer): double;
  begin
    result := QMat[i,j];
  end;

  function R(i,j: integer): double;
  begin
    result := 1-(PMat[i,j]+PMat[j,i]+QMat[i,j]);
  end;

  function b(i,j: integer): Double;
  begin
    result := Qmat[j,i];
  end;

  function c1(i,j: integer):double;
  begin
    result := (fA[i]+fG[i]+fA[j]+fG[j])*k1 +(fT[i]+fC[i]+fT[j]+fC[j]);
  end;

  function f1(i,j: integer):double;
  begin
    result := gamma*ln(gamma/(gamma+c1(i,j)*b(i,j)));
  end;

  function df1dx(i,j: integer):double;
  begin
    result := -gamma*c1(i,j)/(gamma +c1(i,j)*b(i,j));
  end;

  function ddf1dx(i,j: integer):double;
  begin
    result := gamma*c1(i,j)*c1(i,j)/(gamma +c1(i,j)*b(i,j))/(gamma +c1(i,j)*b(i,j));
  end;

  function df1dk1(i,j: integer):double;
  var
    fR: double;
  begin
    fR := (fA[i]+fG[i]+fA[j]+fG[j]);
    result := -gamma*fR*b(i,j)/(gamma +c1(i,j)*b(i,j));
  end;

  function ddf1dk1(i,j: integer):double;
  var
    fR: double;
  begin
    fR := (fA[i]+fG[i]+fA[j]+fG[j]);
    result := gamma*fR*fR*b(i,j)*b(i,j)/(gamma +c1(i,j)*b(i,j))/(gamma +c1(i,j)*b(i,j));
  end;

  function c2(i,j: integer):double;
  begin
    result := (fT[i]+fC[i]+fT[j]+fC[j])*k2 +(fA[i]+fG[i]+fA[j]+fG[j]);
  end;

  function f2(i,j: integer):double;
  begin
    result := gamma*ln(gamma/(gamma +c2(i,j)*b(i,j)));
  end;

  function df2dx(i,j: integer):double;
  begin
    result := -gamma*c2(i,j)/(gamma +c2(i,j)*b(i,j));
  end;

  function ddf2dx(i,j: integer):double;
  begin
    result := gamma*c2(i,j)*c2(i,j)/(gamma +c2(i,j)*b(i,j))/(gamma +c2(i,j)*b(i,j));
  end;

  function df2dk2(i,j: integer):double;
  var
    fY: double;
  begin
    fY := (fT[i]+fC[i]+fT[j]+fC[j]);
    result := -gamma*fY*b(i,j)/(gamma +c2(i,j)*b(i,j));
  end;

  function ddf2dk2(i,j: integer):double;
  var
    fY: double;
  begin
    fY := (fT[i]+fC[i]+fT[j]+fC[j]);
    result := gamma*fY*fY*b(i,j)*b(i,j)/(gamma +c2(i,j)*b(i,j))/(gamma +c2(i,j)*b(i,j));
  end;

  function f3(i,j: integer):double;
  begin
    result := gamma*ln(gamma/(gamma +2*b(i,j)));
  end;

  function df3dx(i,j: integer):double;
  begin
    result := -2*gamma/(gamma +2*b(i,j));
  end;

  function ddf3dx(i,j: integer):double;
  begin
    result := 4*gamma/(gamma +2*b(i,j))/(gamma +2*b(i,j));
  end;

  function EP1(i,j: integer):double;
  var
    g1,g3,fR: double;
  begin
    fR := (fA[i]+fG[i]+fA[j]+fG[j])/2;
    if fR > 0 then
    begin
      g1 := (fA[i]*fG[j]+fA[j]*fG[i])/fR;
      g3 := ((fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]))/fR/2;
      result := g1*(1-g3 -exp(f1(i,j)) +g3*exp(f3(i,j)));
    end
    else
      result := 0;
  end;

  function EP2(i,j: integer):double;
  var
    g2,g3,fY: double;
  begin
    fY := (fT[i]+fC[i]+fT[j]+fC[j])/2;
    if fY > 0 then
    begin
      g2 := (fT[i]*fC[j]+fT[j]*fC[i])/fY;
      g3 := ((fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]))/fY/2;
      result := g2*(1-g3 -exp(f2(i,j)) +g3*exp(f3(i,j)));
    end
    else
      result := 0;
  end;

  function EQ(i,j: integer):double;
  var
    g3: double;
  begin
    g3 := (fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]);
    result := g3*(1 -exp(f3(i,j)));
  end;

  function dP1dx(i,j: integer):double;
  var
    g1,g3,fR: double;
  begin
    fR := (fA[i]+fG[i]+fA[j]+fG[j])/2;
    g1 := (fA[i]*fG[j]+fA[j]*fG[i])/fR;
    g3 := ((fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]))/fR/2;
    result := g1*(-exp(f1(i,j))*df1dx(i,j) +g3*exp(f3(i,j))*df3dx(i,j));
  end;

  function dP1dk1(i,j: integer):double;
  var
    g1,fR: double;
  begin
    fR := (fA[i]+fG[i]+fA[j]+fG[j])/2;
    g1 := (fA[i]*fG[j]+fA[j]*fG[i])/fR;
    result := -g1*exp(f1(i,j))*df1dk1(i,j);
  end;

  function dP2dx(i,j: integer):double;
  var
    g2,g3,fY: double;
  begin
    fY := (fT[i]+fC[i]+fT[j]+fC[j])/2;
    g2 := (fT[i]*fC[j]+fT[j]*fC[i])/fY;
    g3 := ((fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]))/fY/2;
    result := g2*(-exp(f2(i,j))*df2dx(i,j) +g3*exp(f3(i,j))*df3dx(i,j));
  end;

  function dP2dk2(i,j: integer):double;
  var
    g2,fY: double;
  begin
    fY := (fT[i]+fC[i]+fT[j]+fC[j])/2;
    g2 := (fT[i]*fC[j]+fT[j]*fC[i])/fY;
    result := -g2*exp(f2(i,j))*df2dk2(i,j);
  end;

  function dQdx(i,j: integer):double;
  var
    g3: double;
  begin
    g3 := (fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]);
    result := -g3*exp(f3(i,j))*df3dx(i,j);
  end;

  function ddP1dx(i,j: integer):double;
  var
    g1,g3,fR: double;
  begin
    fR := (fA[i]+fG[i]+fA[j]+fG[j])/2;
    g1 := (fA[i]*fG[j]+fA[j]*fG[i])/fR;
    g3 := ((fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]))/fR/2;
    result := g1*(g3*exp(f3(i,j))*(df3dx(i,j)*df3dx(i,j) +ddf3dx(i,j))
                    -exp(f1(i,j))*(df1dx(i,j)*df1dx(i,j) +ddf1dx(i,j)));
  end;

  function ddP1dk1(i,j: integer):double;
  var
    g1,fR: double;
  begin
    fR := (fA[i]+fG[i]+fA[j]+fG[j])/2;
    g1 := (fA[i]*fG[j]+fA[j]*fG[i])/fR;
    result := -g1*exp(f1(i,j))*(df1dk1(i,j)*df1dk1(i,j) +ddf1dk1(i,j));
  end;

  function ddP2dx(i,j: integer):double;
  var
    g2,g3,fY: double;
  begin
    fY := (fT[i]+fC[i]+fT[j]+fC[j])/2;
    g2 := (fT[i]*fC[j]+fT[j]*fC[i])/fY;
    g3 := ((fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]))/fY/2;
    result := g2*(g3*exp(f3(i,j))*(df3dx(i,j)*df3dx(i,j) +ddf3dx(i,j))
                    -exp(f2(i,j))*(df2dx(i,j)*df2dx(i,j) +ddf2dx(i,j)));
  end;

  function ddP2dk2(i,j: integer):double;
  var
    g2,fY: double;
  begin
    fY := (fT[i]+fC[i]+fT[j]+fC[j])/2;
    g2 := (fT[i]*fC[j]+fT[j]*fC[i])/fY;
    result := -g2*exp(f2(i,j))*(df2dk2(i,j)*df2dk2(i,j) +ddf2dk2(i,j));
  end;

  function ddQdx(i,j: integer):double;
  var
    g3: double;
  begin
    g3 := (fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]);
    result := -g3*exp(f3(i,j))*(df3dx(i,j)*df3dx(i,j) +ddf3dx(i,j));
  end;

  function IterateDistance(i,j: integer):double;
  var
    dP1,dP2,dQ,dR,ddP1,ddP2,ddQ,ddR,ER,s,dL,ddL: double;
  begin
    if QMat[j,i] < 0.000000000001 then
    begin
      result := 0;
      exit;
    end;
    dL   := 0.0;
    ddL  := 0.0;
    ER   := 1;
    dP1  := 0;
    ddP1 := 0;
    dP2  := 0;
    ddP2 := 0;
    dQ   := 0;
    ddQ  := 0;

    s := EP1(i,j);
    if s > 0 then
    begin
      dP1  := dP1dx(i,j);
      ddP1 := ddP1dx(i,j);
      if p1(i,j) > 0 then
      begin
        dL  := dL  +p1(i,j)*dP1/s;
        ddL := ddL +p1(i,j)*(ddP1 -dP1*dP1/s)/s;
      end;
      ER := ER -s;
    end;
    s := EP2(i,j);
    if s > 0 then
    begin
      dP2  := dP2dx(i,j);
      ddP2 := ddP2dx(i,j);
      if p2(i,j) > 0 then
      begin
        dL  := dL  +p2(i,j)*dP2/s;
        ddL := ddL +p2(i,j)*(ddP2 -dP2*dP2/s)/s;
      end;
      ER := ER -s;
    end;
    s := EQ(i,j);
    if s > 0 then
    begin
      dQ   := dQdx(i,j);
      ddQ  := ddQdx(i,j);
      if Q(i,j) > 0 then
      begin
        dL  := dL  +Q(i,j)*dQ/s;
        ddL := ddL +Q(i,j)*(ddQ -dQ*dQ/s)/s;
      end;
      ER := ER -s;
    end;
    if ER > 0 then
    begin
      dR   := -dP1 -dP2 -dQ;
      ddR  := -ddP1 -ddP2 -ddQ;
      dL  := dL  +r(i,j)*dR/ER;
      ddL := ddL +r(i,j)*(ddR -dR*dR/ER)/ER;
    end;
    if abs(ddL) > 0.000000000001 then
      if QMat[j,i] > dL/ddL then
        result := QMat[j,i] -dL/ddL
      else
        result := 0
    else
      result := QMat[j,i];
  end;

  procedure IterateKappa;
  var
    i,j: integer;
    dP1,ddP1,dP2,ddP2,ER,s,dLdk1,ddLdk1,dLdk2,ddLdk2: double;
  begin
    dLdk1  := 0;
    ddLdk1 := 0;
    dLdk2  := 0;
    ddLdk2 := 0;
    for i := 1 to FNoOfSeqs-1 do
    begin
      if ShowInProgressBar then      
        if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
          UpdateProgress(Round((i * (i -1) / 2) / NumComparisons * 100));
      for j := 0 to i-1 do
      begin
        dP1  := 0;
        ddP1 := 0;
        dP2  := 0;
        ddP2 := 0;
        ER := 1;
        s := EP1(i,j);
        if s > 0 then
        begin
          dP1  := dP1dk1(i,j);
          ddP1 := ddP1dk1(i,j);
          if p1(i,j) > 0 then
          begin
            dLdk1  := dLdk1  +p1(i,j)*dP1/s;
            ddLdk1 := ddLdk1 +p1(i,j)*(ddP1 -dP1*dP1/s)/s;
          end;
          ER := ER -s;
        end;
        s := EP2(i,j);
        if s > 0 then
        begin
          dP2  := dP2dk2(i,j);
          ddP2 := ddP2dk2(i,j);
          if p2(i,j) > 0 then
          begin
            dLdk2  := dLdk2  +p2(i,j)*dP2/s;
            ddLdk2 := ddLdk2 +p2(i,j)*(ddP2 -dP2*dP2/s)/s;
          end;
          ER := ER -s;
        end;
        s := EQ(i,j);
        if s > 0 then
          ER := ER -s;
        if ER > 0 then
        begin
          dLdk1  := dLdk1  -r(i,j)*dP1/ER;
          ddLdk1 := ddLdk1 -r(i,j)*(ddP1 -dP1*dP1/ER)/ER;
          dLdk2  := dLdk2  -r(i,j)*dP2/ER;
          ddLdk2 := ddLdk2 -r(i,j)*(ddP2 -dP2*dP2/ER)/ER;
        end;
      end;
    end;
      if ddLdk1 <> 0 then
        if abs(dLdk1/ddLdk1) < k1 then
          k1 := k1 -dLdk1/ddLdk1;
      if ddLdk2 <> 0 then
        if abs(dLdk2/ddLdk2) < k2 then
          k2 := k2 -dLdk2/ddLdk2;

      if k1 > 1000 then
        k1 := 1000;
      if k2 > 1000 then
        k2 := 1000;
  end;

  function SL:double;
  var
    i,j: integer;
    s,ER: double;
  begin
    result := 0;
    for i := 1 to FNoOfSeqs-1 do
      for j := 0 to i-1 do
      begin
        ER := 1;
        s := EP1(i,j);
        if EP1(i,j) > 0 then
        begin
          result := result +p1(i,j)*ln(s);
          ER := ER -s;
        end;
        s := EP2(i,j);
        if s > 0 then
        begin
          result := result +p2(i,j)*ln(s);
          ER := ER -s;
        end;
        s := EQ(i,j);
        if s > 0 then
        begin
          result := result +q(i,j)*ln(s);
          ER := ER -s;
        end;
        if ER > 0 then
          result := result +r(i,j)*ln(ER)
        else
          result := result +r(i,j);
      end;
  end;

begin
  Sa1 := 0;
  Sa2 := 0;
  Sb := 0;
  if ShowInProgressBar then
  begin
    UpdateProgress(0, 'Status', 'Computing TN hetero distances');
    NumComparisons := FNoOfSeqs * (FNoOfSeqs - 1) div 2;
  end;

  for i := 1 to FNoOfSeqs-1 do
  begin
    if ShowInProgressBar then
      if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
        UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));
    for j := 0 to i-1 do
    begin
      QMat[j,i] := p1(i,j)+p2(i,j)+q(i,j);
      g1 := fA[i]*fG[j]+fA[j]*fG[i];
      g2 := fT[i]*fC[j]+fT[j]*fC[i];
      g3 := (fA[i]+fG[i])*(fT[j]+fC[j])+(fA[j]+fG[j])*(fT[i]+fC[i]);
      if (QMat[j,i] > 0) and (g1 > 0) and (g2 > 0) and (g3 > 0) then
      begin
        Sa1 := Sa1 +p1(i,j)/QMat[j,i]/QMat[j,i];
        Sa2 := Sa2 +p2(i,j)/QMat[j,i]/QMat[j,i];
        Sb  := Sb  +q(i,j)/QMat[j,i]/QMat[j,i];
      end;
    end;
  end;
  if (Sb > 0) and (pR > 0) and (pY > 0) then
  begin
    if (pA > 0) and (pG > 0) then
      k1 := (Sa1/pA/pG)/(Sb/pR/pY)
    else
      k1 := 1;
    if (pT > 0) and (pC > 0) then
      k2 := (Sa2/pT/pC)/(Sb/pR/pY)
    else
      k2 := 1;
  end
  else
  begin
    k1 := 1;
    k2 := 1;
  end;
  gamma := GammaPara;

  UpdateProgress(0, 'Status', 'Updating distance matrix');
  for i := 1 to FNoOfSeqs-1 do
  begin
    if ShowInProgressBar then    
      if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
        UpdateProgress(Round((i * (i -1) / 2) / NumComparisons * 100));
    for j := 0 to i-1 do
    begin
      g1 := (fA[i]+fA[j])*(fG[i]+fG[j]);
      g2 := (fT[i]+fT[j])*(fC[i]+fC[j]);
      g3 := (fA[i]+fA[j]+fG[i]+fG[j])*(fT[i]+fT[j]+fC[i]+fC[j]);
      QMat[j,i] := QMat[j,i]/(g1*k1 +g2*k2 +g3);
    end;
  end;

  dL := SL;
//  if IncProgress then
//    AddProgressIncrement10;

  UpdateProgress(0, 'Status', 'Iterating distances');
  progress := floor(log10(-dL));
  mind := power(10, progress-8);
  repeat
    for i := 1 to FNoOfSeqs-1 do
    begin
      if ShowInProgressBar then
//      left off here
      if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
      begin
        UpdateProgress(Round((i * (i -1) / 2) / NumComparisons * 100));
        LastUpdateTime := Now;
      end;
      for j := 0 to i-1 do
        if QMat[j,i] > 0.000000000001 then
          QMat[j,i] := IterateDistance(i,j);
    end;

    IterateKappa;
{
    if k1 > 0.000000000001 then
      k1 := k1 -dLdk1/ddLdk1;
    if k2 > 0.000000000001 then
      k2 := k2 -dLdk2/ddLdk2;
}
    ddL := SL -dL;
    dL := dL +ddL;

    if CheckAbort then
      Exit;
//    try
//      if IncProgress then
//      begin
//        while (ddL > 0) and (log10(ddL) < progress) do
//        begin
//          AddProgressIncrement10;
//          dec(progress);
//          Application.ProcessMessages;
//        end;
//      end;
//    except
//      On E:Exception do
//      begin
//        Dist1 := IDV;
//        Var1  := IDV;
//        if FQuickExit then raise;
//      end;
//    end;

  until ddL < mind;

  UpdateProgress(0, 'Status', 'Finalizing');
  for i:= 1 to FNoOfSeqs-1 do
  begin
    if MillisecondsBetween(Now, LastUpdateTime) > PROGRESS_UPDATE_INTERVAL then
    begin
      UpdateProgress(Round((i * (i - 1) / 2) / NumComparisons * 100));
      LastUpdateTime := Now;
    end;
    for j:= 0 to i-1 do
    begin
      g1 := (fA[i]+fA[j])*(fG[i]+fG[j]);
      g2 := (fT[i]+fT[j])*(fC[i]+fC[j]);
      g3 := (fA[i]+fA[j]+fG[i]+fG[j])*(fT[i]+fT[j]+fC[i]+fC[j]);
      case DistComponent of
        gdNone:         D[i][j] := (g1*k1 +g2*k2 +g3)*b(i,j);
        gdNucTsOnly:    D[i][j] := (g1*k1 +g2*k2)*b(i,j);
        gdNucTvOnly:    D[i][j] := g3*b(i,j);
        gdNucRatioTsTv: if g3 < 0.000000000001 then
                          D[i][j] := -1
                        else
                          D[i][j] := (g1*k1 +g2*k2)/g3;
      end;
{
      case VarType of
        gdAnalyticalVar:   FUserV[i][j] := Var1;
        gdMonteCarloTest:  FUserV[i][j] := Var1;
      end;
}
    end; // end of for j
  end;// end for i
end;


initialization
begin
  T := Integer(gdBaseT);
  C := Integer(gdBaseC);
  A := Integer(gdBaseA);
  G := Integer(gdBaseG);
end;

end.

