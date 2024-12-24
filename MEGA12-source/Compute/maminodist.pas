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

unit MAminoDist;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  MSeqDistBase, MegaConsts, MAminoMatrix;

type
  TFmatrix = array[ala..val, ala..val] of integer;  // For computing PAM and JTT distances  @KT
  AAmatrix = array[0..19,0..19] of double;          // For computing PAM and JTT distances  @KT
  AAeigen = array[0..19] of double;                 // For computing PAM and JTT distances  @KT
  pAAmatrix = ^AAmatrix;                            // For computing PAM and JTT distances  @KT
  pAAeigen = ^AAeigen;                              // For computing PAM and JTT distances  @KT

  { TAminoDist }

  TAminoDist = Class(TSeqDistBase)
  private
    FMonomorphic: Integer;
    aaf: array[#0..#19] of Integer;       // amino acid frequencies
    aaf1: array[#0..#19] of Integer;      // @KT
    aaf2: array[#0..#19] of Integer;      // @KT

    prob: pAAmatrix;                      // For computing PAM and JTT distances  @KT
    eig: pAAeigen;                        // For computing PAM and JTT distances  @KT

    function  ComputeIterateLen: Integer;

    procedure ComputeCompDist(NDiff, NSites: LongInt; Seq1, Seq2: PAnsiChar);
    procedure ComputeDisparityIndex(NDiff, NSites: LongInt; Seq1, Seq2: PAnsiChar);
    procedure ComputeDisparityIndexTest(NDiff, NSites: LongInt; Seq1, Seq2: PAnsiChar);

    function ComputeMatrixDist(var F: TFmatrix; NSites: integer):double;
    function ComputePAMDist(var F: TFmatrix; NSites: integer): double;
    function ComputePAMGammaDist(var F: TFmatrix; NSites: integer): double;
    function ComputeJTTDist(var F: TFmatrix; NSites: integer): double;
    function ComputeJTTGammaDist(var F: TFmatrix; NSites: integer): double;

  protected
    procedure DoSimultaneousAnalysis(D: PDistanceMatrix; IncProgress: boolean); override;
  public
    procedure DoPairwiseAnalysis(i,j: Integer); override;

    constructor Create;
    destructor Destroy; override;

    function ComputeDistances: Boolean; override;

    property Monomorphic: Integer write FMonomorphic;

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
  SysUtils, Math, MDistPack, ErrorMessages_HC;

const
  epsilon: double = 0.0001;        // For computing PAM and JTT distances  @KT
  fracchange: double = 0.01;       // For computing PAM and JTT distances  @KT

constructor TAminoDist.Create;
begin
  inherited Create;
  FMonomorphic := 0;
end;

destructor TAminoDist.Destroy;
begin
  inherited Destroy;
end;

// get the length of iteration
function TAminoDist.ComputeIterateLen: Integer;
begin
  if FFreqTable <> nil then
    Result := FFreqTableLen
  else
    Result := FNoOfSites - FMonomorphic;
end;

//=== Actually does the pairwise sequence analysis
procedure TAminoDist.DoPairwiseAnalysis(i, j: Integer);
var
  Seq1, Seq2: PAnsiChar;
  FMatrix: TFMatrix;                               //  @KT
  a1,a2: AASites;                                  //  @KT

  AminoI, AminoJ, x:  AnsiChar;
  AddInt, NDiff, Site, SitesCompared, ErrorId: Integer;
  pDiff: Double;

  function GetRandomSimilarity: Double;
  var
    m: AnsiChar;
    sc: Integer;
  begin
    Result := 0;
    sc := 0;
    for m:=#0 to #19 do
      sc := sc + aaf[m];

    for m:=#0 to #19 do
      Result := Result + aaf[m]/sc*aaf[m]/sc;
  end;

  function ApplyCorrection(pDiff, NSite: Double; var d, v: Double): Boolean;
  var
    b,f: double;
    x:  AnsiChar;
  begin
    Result := False;
    if NSite = 0 then
    begin
      ErrorId := HC_No_Common_Sites;
      Exit;
    end;

    d := IDV;
    case DistCorrection of
      gdNoOfDiff:
         begin
           d := pDiff*NSite;
         end;
      gdPropDist:   d := pDiff;
      gdTestRandom: d := (1-pDiff) - GetRandomSimilarity;
      gdPoisson:
        if (1 - pDiff) > 0.01 then
        begin
          if HasGamma then
            d := GammaPara*(power(1-pDiff, -GammaInv)-1)
          else
            d := -Ln(1- pDiff)
        end
        else
        begin
          if HasGamma then
            ErrorId := HC_Gamma_Correction_Failed_Because_p_0_99
          else
            ErrorId := HC_Poisson_Correction_Failed_because_p_0_99;
          Exit;
        end;
      gdEqualInput:
          begin
            b := 1;
            for x := #0 to #19 do
              b := b -(aaf1[x]+aaf2[x])*(aaf1[x]+aaf2[x])/NSite/NSite/4;

            if IsHetero then
            begin
              f := 1;
              for x := #0 to #19 do
                f := f -aaf1[x]*aaf2[x]/NSite/NSite;
            end
            else
              f := b;

            if (f > 0) and ((1 - pDiff/f) > 0.01) then
              if HasGamma then
                d := b*GammaPara*(power(1-pDiff/f, -GammaInv)-1)
              else
                d := -b*Ln(1- pDiff/f)
            else
            begin
              ErrorId := HC_Equal_Input_Correction_Failed;
              Exit;
            end;
          end;
      gdJones:
          begin
            if HasGamma then
              d := ComputeJTTGammaDist(FMatrix, SitesCompared)
            else
              d := ComputeJTTDist(FMatrix, SitesCompared);
            if d = InvalidDistValue then
            begin
              ErrorId := InternalErrId;
              exit;
            end;
          end;
      gdDayhoff:
          begin
            if HasGamma then
              d := ComputePAMGammaDist(FMatrix, SitesCompared)
            else
              d := ComputePAMDist(FMatrix, SitesCompared);
            if d = InvalidDistValue then
            begin
              ErrorId := InternalErrId;
              exit;
            end;
          end;
    end;

    if VarType = gdAnalyticalVar then
    begin
      v := IDV;
      case DistCorrection of
        gdNoOfDiff: v := pDiff*(1-pDiff)*NSite ;
        gdPropDist: v := pDiff*(1-pDiff)/NSite;
        gdPoisson:  v := pDiff/(1-pDiff)/NSite;
        gdGamma:    v := pDiff*power(1-pDiff, -2.0*gammaInv -1)/NSite;
      end;
    end;
    Result := True;
  end;
begin
  Seq1 := FSequences[i];
  Seq2 := FSequences[j];

  // initialize
  IterateLen := ComputeIterateLen;
  SitesCompared := FMonomorphic;
  NDiff := 0;

  for x:=#0 to #19 do
    aaf[x] := 0;

  for site:=0 to IterateLen-1 do
  begin
    AminoI := Seq1[site];
    AminoJ := Seq2[site];

    // Save time by mapping all amino acid code to X and the check that
    if (AminoI > #19) or (AminoJ > #19) then
        Continue;

    if FFreqTable = nil then AddInt := 1
    else                     AddInt := FFreqTable[Site];
    SitesCompared := SitesCompared + AddInt;
    if AminoI <> AminoJ then
      NDiff := NDiff + AddInt;

    if DistCorrection = gdTestRandom then
    begin
      Inc(aaf[AminoI], 1);  // to ensure exact same exp value at all times
      Inc(aaf[AminoJ], 1);
    end;
  end; // end of for every site in a pairwise comparison

  if (DistCorrection = gdEqualInput) or
     (DistCorrection = gdJones) or
     (DistCorrection = gdDayhoff) then
  begin
    for x:= #0 to #19 do
    begin
      aaf1[x] := 0;
      aaf2[x] := 0;
    end;

    for a1 := ala to val do
      for a2 := ala to val do
        FMatrix[a1,a2] := 0;

    for site := 0 to IterateLen-1 do
    begin
      if FFreqTable = nil then
        AddInt := 1
      else
        AddInt := FFreqTable[Site];

      if AddInt = 0 then Continue;

      AminoI := Seq1[site];
      AminoJ := Seq2[site];

      if (AminoI > #19) or (AminoJ > #19) then
        Continue;

      case AminoI of
        #0:  a1 := ala;
        #1:  a1 := cys;
        #2:  a1 := asp;
        #3:  a1 := glu;
        #4:  a1 := phe;
        #5:  a1 := gly;
        #6:  a1 := his;
        #7:  a1 := ileu;
        #8:  a1 := lys;
        #9:  a1 := leu;
        #10: a1 := met;
        #11: a1 := asn;
        #12: a1 := pro;
        #13: a1 := gln;
        #14: a1 := arg;
        #15: a1 := ser;
        #16: a1 := thr;
        #17: a1 := val;
        #18: a1 := trp;
        #19: a1 := tyr;
      end;
      case AminoJ of
        #0:  a2 := ala;
        #1:  a2 := cys;
        #2:  a2 := asp;
        #3:  a2 := glu;
        #4:  a2 := phe;
        #5:  a2 := gly;
        #6:  a2 := his;
        #7:  a2 := ileu;
        #8:  a2 := lys;
        #9:  a2 := leu;
        #10: a2 := met;
        #11: a2 := asn;
        #12: a2 := pro;
        #13: a2 := gln;
        #14: a2 := arg;
        #15: a2 := ser;
        #16: a2 := thr;
        #17: a2 := val;
        #18: a2 := trp;
        #19: a2 := tyr;
      end;
      Inc(FMatrix[a1,a2], AddInt);
      Inc(aaf1[AminoI], AddInt);
      Inc(aaf2[AminoJ], AddInt);
    end;
  end;

  if DistComponent = gdCommonSites then
  begin
    Dist1 := SitesCompared;
    Exit;
  end;

  if SitesCompared < 1 then
  begin
    Dist1 := InvalidDistValue;
    if FQuickExit then
      SetErrorStatus(HC_No_Common_Sites, 'No common sites were found for the sequence pair ('+
                                             IntToStr(i+1)+','+IntToStr(j+1)+').' + NoCommonSitesStr)
    else
      Exit;
  end;

  // compute p
  pDiff := NDiff/SitesCompared;
  ErrorId := 0;

  if FDistPack.DoesContain(gdDisparityIndexTest) then
     ComputeDisparityIndexTest(NDiff, SitesCompared, Seq1, Seq2)
  else if FDistPack.DoesContain(gdDisparityIndex) then
    ComputeDisparityIndex(NDiff, SitesCompared, Seq1, Seq2)
  else if FDistPack.DoesContain(gdCompositionDistance) or
          FDistPack.DoesContain(gdAlleleFreqDist) then
    ComputeCompDist(NDiff, SitesCompared, Seq1, Seq2)
  else if not ApplyCorrection(pDiff, SitesCompared, Dist1, Var1) then
  begin
    Dist1 := InvalidDistValue;
    if FQuickExit then
      SetErrorStatus(ErrorId, 'Some pairwise distances could not be estimated. For example, between sequences '
                                +IntToStr(i+1) + ' and ' + IntToStr(j+1) + '.')
    else
     Exit;
  end;
end;

//--- composition/ allele freq distance
procedure TAminoDist.ComputeCompDist(NDiff, NSites: LongInt;Seq1, Seq2: PAnsiChar);
begin
  Dist1 := 0;
  Var1 := 0;
  if NDiff = 0 then
    Exit;
  Dist1 := CompositionDistance(NSites, 20, Seq1, Seq2,gdResiX);
  Dist1 := Dist1/NSites;
end;

procedure TAminoDist.ComputeDisparityIndex(NDiff, NSites: LongInt;Seq1, Seq2: PAnsiChar);
begin
  Dist1 := 0;
  Var1 := 0;
  if NDiff = 0 then
    Exit;
  Dist1 := CompositionDistance(NSites, 20, Seq1, Seq2,gdResiX) - NDiff; // inherited
  Dist1 := Dist1/NSites;
  if Dist1 < 0 then
    Dist1 := 0;
end;

//--- For testing
procedure TAminoDist.ComputeDisparityIndexTest(NDiff, NSites: LongInt; Seq1, Seq2: PAnsiChar);
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
    Dist1 := 1; // prob
    Var1 := 0;
    Exit;
  end;
  Dist1 :=  CompositionDistance(NSites, 20, Seq1, Seq2,gdResiX)-NDiff; // inherited
  MonteCarloDisparityIndexTest(NDiff, NSites, 20, Dist1, Seq1, Seq2,gdResiX);
end;

//----- Computes pairwise distances between taxa
function TAminoDist.ComputeDistances: Boolean;
begin
  Result := inherited ComputeDistances;
end; // end of function

// Added by TK
function TAminoDist.ComputeMatrixDist(var F: TFmatrix; NSites: integer):double;
var
  itterations: integer;
  delta, lnlike, slope, curv: double;
  neginfinity, inf: boolean;
  b1, b2: AASites;
  tt, p, dp, d2p, q, elambdat, dtt: double ;

  procedure predict(b1, b2: AASites);
  var
    m: integer;
  begin //* make contribution to prediction of this aa pair */
    for m := 0 to 19 do
    begin
      if HasGamma then
        elambdat := exp(-GammaPara*ln(1.0 -tt*eig[m]/GammaPara))
      else
        elambdat := exp(tt*eig[m]);
      q := prob[m][integer(b1)]*prob[m][integer(b2)]*elambdat;
      p := p +q;
      if HasGamma then
        dp := dp +(eig[m]/(1.0 -tt*eig[m]/GammaPara))*q
      else
        dp := dp +eig[m]*q;
      if HasGamma then
        d2p := d2p +(eig[m]*eig[m]*(1.0+1.0/GammaPara)/
                    ((1.0-tt*eig[m]/GammaPara)*(1.0-tt*eig[m]/GammaPara)))*q
      else
        d2p := d2p +eig[m]*eig[m]*q;
    end;
  end;  //* predict */

begin //* compute the distances */
  tt    := 10.0;
  dtt   := tt;
  delta := tt / 2.0;
  itterations := 0;
  inf := false;
  repeat
    lnlike := 0.0;
    slope := 0.0;
    curv := 0.0;
    neginfinity := false;

    for b1 := ala to val do
      for b2 := ala to val do
      begin
        if f[b1,b2] = 0 then
          continue;

        p := 0.0;
        dp := 0.0;
        d2p := 0.0;

        predict(b1, b2);

        if p <= 0.0 then
        begin
          neginfinity := true;
        {
          InternalErrId := HC_Dayhoff_Distance_Could_Not_Be_Computed;
          InternalErrStr:= 'Dayhoff/JTT Matrix based distance could not be computed';
          result := InvalidDistValue;
          exit;
        }
        end
        else
        begin
          lnlike := lnlike +ln(p)*f[b1,b2];
          slope := slope +dp/p*f[b1,b2];
          curv := curv +(d2p/p -dp*dp/(p*p))*f[b1,b2];
        end;
      end;

    itterations := itterations +1;
    if not neginfinity then
    begin
      if curv < 0.0 then
      begin
        tt := tt -slope / curv;
        if tt > 10000.0 then
        begin
          tt := -1.0/fracchange;
          inf := true;
          itterations := 20;
        end
        else if abs(tt-dtt) < epsilon*fracchange then
          itterations := 20;
      end
      else
      begin
        if ((slope > 0.0) and (delta < 0.0)) or ((slope < 0.0) and (delta > 0.0)) then
          delta := delta/(-2);
        tt := tt +delta;
      end
    end
    else
    begin
      delta := delta /(-2);
      tt := tt +delta;
    end;
    if (tt < epsilon) and (not inf) then
      tt := epsilon;
    dtt := tt;
  until itterations = 20;
  result := fracchange * tt;
end;  //* makedists */

function TAminoDist.ComputePAMDist(var F: TFmatrix; NSites: integer): double;
begin
  prob := addr(PAMprobs);
  eig  := addr(PAMeigs);
  result := ComputeMatrixDist(F,NSites);
end;

function TAminoDist.ComputePAMGammaDist(var F: TFmatrix; NSites: integer): double;
begin
  prob := addr(PAMprobs);
  eig  := addr(PAMeigs);
  result := ComputeMatrixDist(F,NSites);
end;

function TAminoDist.ComputeJTTDist(var F: TFmatrix; NSites: integer): double;
begin
  prob := addr(JTTprobs);
  eig  := addr(JTTeigs);
  result := ComputeMatrixDist(F,NSites);
end;

function TAminoDist.ComputeJTTGammaDist(var F: TFmatrix; NSites: integer): double;
begin
  prob := addr(JTTprobs);
  eig  := addr(JTTeigs);
  result := ComputeMatrixDist(F,NSites);
end;

procedure TAminoDist.DoSimultaneousAnalysis(D: PDistanceMatrix;
  IncProgress: boolean);
begin
  Assert(False, 'DoSimultaneousAnalysis only implemented for TNucDist');
end;



end.

