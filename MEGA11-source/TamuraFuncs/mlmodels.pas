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

unit MLModels;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, MegaConsts, MLTree, MatrixConsts, MatrixFuncs;

type
  TGTRModel = class (TGammaRateVariationModel)
  private
    pA,pT,pC,pG,s: extended;
    r: array [1..6] of Extended;
    FixedIndex: integer;

    Lambda,F,Fi: PArrayOfExtended;
    A,Ai,V,AV: PMatrixOfExtended;
    FQ,FQFi: PMatrixOfExtended;
    Q: PMatrixOfExtended;

    FreqFlag: boolean;

    procedure ComputeEigenVector(Q: PMatrixOfExtended);
    procedure ChangeParams(r1,r2,r3,r4,r5,r6: extended);
    procedure ResetProb(tree: TMLTree);
  protected
    procedure ComputeProbWithGamma(P: PMatrixOfExtended; b: extended); override;
  public
    procedure Assign(source: TGammaRateVariationModel); override;

    procedure ComputeProb(P: PMatrixOfExtended; b: extended); override;
    procedure OptimizeParameters(tree: TMLTree; aCheckCancel: TCheckCancelFunc = nil); override;

    procedure SetParamsFromSeqs(seqs: TStringList); override;
    procedure SetFreqs(fA,fT,fC,fG: extended);
    procedure SetParams(r1,r2,r3,r4,r5,r6: extended);
    procedure GetParams(var r1,r2,r3,r4,r5,r6: extended);

    procedure GetInfo(ModelInfo: TModelInfo); override;

    function PatternBias: Extended; // override;
    function GetName: String; override;
    function ReadFromFile(var data: File):boolean; override;
    procedure WriteToFile(var data: File); override;
    //function StateToStringList(Comment: String=EmptyStr):TStringList;
    constructor Create(gamma: extended; inv: boolean; ncat: integer);
    destructor Destroy; override;
  end;

  TTN93Model = class (TGammaRateVariationModel)
    pA,pT,pC,pG: extended;
    k1,k2: extended;
  private
    pR,pY: extended;
    procedure ChangeProbOfNode(node: TMLTreeNode; b,k1,k2: extended);
  protected
    function CompositeLikelihood(M: PMatrixOfExtended; b: extended; usegamma: boolean): extended; override;
    procedure ComputeProbWithGamma(P: PMatrixOfExtended; b: extended); override;
  public
    procedure Assign(source: TGammaRateVariationModel); override;

    procedure ComputeProb(P: PMatrixOfExtended; b: extended); override;
    procedure OptimizeParameters(tree: TMLTree; aCheckCancel: TCheckCancelFunc = nil); override;

    procedure SetParamsFromSeqs(seqs: TStringList); override;

    procedure SetFreqs(FreqA,FreqT,FreqC,FreqG: extended);
    procedure SetParams(kappa1,kappa2: extended);
    procedure GetParams(var kappa1,kappa2: extended);

    procedure GetInfo(ModelInfo: TModelInfo); override;

    function PatternBias: Extended; // override;
    function GetName: String; override;
    function ReadFromFile(var data: File):boolean; override;
    procedure WriteToFile(var data: File); override;

    constructor Create(gamma: extended; inv: boolean; ncat: integer);
    destructor Destroy; override;
  end;

  THKYModel = class (TGammaRateVariationModel)
  private
    pA,pT,pC,pG: extended;
    k: extended;
    pR,pY: extended;
    procedure SetFreqsFromSeqs(seqs: TStringList); virtual;
    procedure ChangeProbOfNode(node: TMLTreeNode; b,k: extended);
  protected
    procedure ComputeProbWithGamma(P: PMatrixOfExtended; b: extended); override;
    function CompositeLikelihood(M: PMatrixOfExtended; b: extended; usegamma: boolean): extended; override;
  public
    procedure Assign(source: TGammaRateVariationModel); override;

    procedure ComputeProb(P: PMatrixOfExtended; b: extended); override;
    procedure OptimizeParameters(tree: TMLTree; aCheckCancel: TCheckCancelFunc = nil); override;

    procedure SetParamsFromSeqs(seqs: TStringList); override;
    procedure SetFreqs(FreqA,FreqT,FreqC,FreqG: extended);
    procedure SetParams(kappa: extended);
    procedure GetParams(var kappa: extended);

    procedure GetInfo(ModelInfo: TModelInfo); override;

    function PatternBias: Extended; // override;
    function GetName: String; override;
    function ReadFromFile(var data: File):boolean; override;
    procedure WriteToFile(var data: File); override;

    constructor Create(gamma: extended; inv: boolean; ncat: integer);
    destructor Destroy; override;
  end;

  TT3Model = class (THKYModel)
  public
    constructor create(gamma: extended; inv: boolean; ncat: integer);
    procedure SetFreqs(fGC: extended);
    procedure SetParams(kappa: extended);
    procedure SetParamsFromSeqs(seqs: TStringList); override;
    procedure GetInfo(ModelInfo: TModelInfo); override;
    function GetName: String; override;
  end;

  TK2Model = class (THKYModel)
  public
    constructor create(gamma: extended; inv: boolean; ncat: integer);
    procedure SetParams(kappa: extended);
    procedure SetParamsFromSeqs(seqs: TStringList); override;
    procedure GetInfo(ModelInfo: TModelInfo); override;
    function GetName: String; override;
  end;

  TJCModel = class (THKYModel)
  protected
    function CompositeLikelihood(M: PMatrixOfExtended; b: extended; usegamma: boolean): extended; override;
  public
    constructor create(gamma: extended; inv: boolean; ncat: integer);
    procedure OptimizeParameters(tree: TMLTree; aCheckCancel: TCheckCancelFunc = nil); override;
    procedure SetParamsFromSeqs(seqs: TStringList); override;
    procedure GetInfo(ModelInfo: TModelInfo); override;
    function GetName: String; override;
  end;

  TProteinMatrixModel = class (TGammaRateVariationModel)
  private
    D,V: PArrayOfExtended;
    A,Ai,AV: PMatrixOfExtended;

    FUseFreq: boolean;
    DefaultFreq: array [0..19] of extended;

    procedure ComputeEigenVector; virtual;
    procedure ResetProb(tree: TMLTree);
  protected
    procedure ComputeProbWithGamma(P: PMatrixOfExtended; b: extended); override;
  public
    property UseFreq: boolean read FUseFreq write FUseFreq;

    procedure ComputeProb(P: PMatrixOfExtended; b: extended); override;

    procedure Assign(source: TGammaRateVariationModel); override;

    procedure SetDefaultFreq;
    procedure SetFreqs(f: array of extended); virtual;
    procedure SetParamsFromSeqs(seqs: TStringList); override;

    procedure GetMatrix(M: PMatrixOfExtended); virtual; abstract;
    procedure GetInfo(ModelInfo: TModelInfo); override;

    function ReadFromFile(var data: File):boolean; override;
    procedure WriteToFile(var data: File); override;

    constructor Create(gamma: extended; inv: boolean; freq: boolean; ncat: integer);
    destructor Destroy; override;
  end;

  TDayhoffModel = class (TProteinMatrixModel)
  public
    constructor Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
    procedure GetMatrix(M: PMatrixOfExtended); override;
    procedure GetInfo(ModelInfo: TModelInfo); override;
    function GetName: String; override;
  end;

  TJTTModel = class (TProteinMatrixModel)
  public
    constructor Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
    procedure GetMatrix(M: PMatrixOfExtended); override;
    procedure GetInfo(ModelInfo: TModelInfo); override;
    function GetName: String; override;
  end;

  TmtREV24Model = class (TProteinMatrixModel)
  public
    constructor Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
    procedure GetMatrix(M: PMatrixOfExtended); override;
    procedure GetInfo(ModelInfo: TModelInfo); override;
    function GetName: String; override;
  end;

  TcpREVModel = class (TProteinMatrixModel)
  public
    constructor Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
    procedure GetMatrix(M: PMatrixOfExtended); override;
    procedure GetInfo(ModelInfo: TModelInfo); override;
    function GetName: String; override;
  end;

  TrtREVModel = class (TProteinMatrixModel)
  public
    constructor Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
    procedure GetMatrix(M: PMatrixOfExtended); override;
    procedure GetInfo(ModelInfo: TModelInfo); override;
    function GetName: String; override;
  end;

  TWAGModel = class (TProteinMatrixModel)
  public
    constructor Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
    procedure GetMatrix(M: PMatrixOfExtended); override;
    procedure GetInfo(ModelInfo: TModelInfo); override;
    function GetName: String; override;
  end;

  TLGModel = class (TProteinMatrixModel)
  public
    constructor Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
    procedure GetMatrix(M: PMatrixOfExtended); override;
    procedure GetInfo(ModelInfo: TModelInfo); override;
    function GetName: String; override;
  end;

  TPoissonModel = class (TProteinMatrixModel)
  public
    constructor Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
    procedure GetMatrix(M: PMatrixOfExtended); override;
    procedure GetInfo(ModelInfo: TModelInfo); override;
    function GetName: String; override;
  end;

  TProteinGTRModel = class (TProteinMatrixModel)
  private
    Lambda: PArrayOfComplex;
    r: PMatrixOfExtended;

    FixedI,FixedJ: integer;

    Q: PMatrixOfExtended;

    procedure ComputeEigenVector; override;
    procedure ResetProb(tree: TMLTree);
  public
    procedure OptimizeParameters(tree: TMLTree; aCheckCancel: TCheckCancelFunc = nil); override;
    procedure GetMatrix(M: PMatrixOfExtended); override;
    function GetName: String; override;
    constructor Create(gamma: extended; inv: boolean; ncat: integer);
    destructor Destroy; override;
  end;

  function CopyModel(source: TGammaRateVariationModel): TGammaRateVariationModel;
  function GetApproximateSizeOfDnaModelInBytes(nsites, nrates: Integer): Int64;
  function GetApproximateSizeOfProteinModelInBytes(nsites, nrates: Integer): Int64;

implementation

Uses SysUtils, MGlobalSettings;

function CopyModel(source: TGammaRateVariationModel): TGammaRateVariationModel;
begin
  Result := nil;
  if source = nil then exit;
  if source.SeqDataType = DNA then
  begin
    if source is TJCModel then
      Result := TJCModel.Create(source.Gamma, source.UseInvar, source.NoOfRates)
    else if source is TK2Model then
      Result := TK2Model.Create(source.Gamma, source.UseInvar, source.NoOfRates)
    else if source is TT3Model then
      Result := TT3Model.Create(source.Gamma, source.UseInvar, source.NoOfRates)
    else if source is THKYModel then
      Result := THKYModel.Create(source.Gamma, source.UseInvar, source.NoOfRates)
    else if source is TTN93Model then
      Result := TTN93Model.Create(source.Gamma, source.UseInvar, source.NoOfRates)
    else if source is TGTRModel then
      Result := TGTRModel.Create(source.Gamma, source.UseInvar, source.NoOfRates)
  end
  else
  begin
    if source is TDayhoffModel then
      Result := TDayhoffModel.Create(source.Gamma, source.UseInvar, TProteinMatrixModel(source).UseFreq, source.NoOfRates)
    else if source is TJTTModel then
      Result := TJTTModel.Create(source.Gamma, source.UseInvar, TProteinMatrixModel(source).UseFreq, source.NoOfRates)
    else if source is TWAGModel then
      Result := TWAGModel.Create(source.Gamma, source.UseInvar, TProteinMatrixModel(source).UseFreq, source.NoOfRates)
    else if source is TLGModel then
      Result := TLGModel.Create(source.Gamma, source.UseInvar, TProteinMatrixModel(source).UseFreq, source.NoOfRates)
    else if source is TmtREV24Model then
      Result := TmtREV24Model.Create(source.Gamma, source.UseInvar, TProteinMatrixModel(source).UseFreq, source.NoOfRates)
    else if source is TcpREVModel then
      Result := TcpREVModel.Create(source.Gamma, source.UseInvar, TProteinMatrixModel(source).UseFreq, source.NoOfRates)
    else if source is TrtREVModel then
      Result := TrtREVModel.Create(source.Gamma, source.UseInvar, TProteinMatrixModel(source).UseFreq, source.NoOfRates)
    else if source is TPoissonModel then
      Result := TPoissonModel.Create(source.Gamma, source.UseInvar, TProteinMatrixModel(source).UseFreq, source.NoOfRates);
  end;

  Result.Assign(source);

end;

function GetApproximateSizeOfDnaModelInBytes(nsites, nrates: Integer): Int64;
begin
  Result := 4*SizeOf(Extended); // IniGamma, FGamma, FInvar, deltaL: extended;
  Result := Result + 12*SizeOf(Integer); // FNoOfStates, FTotalNoOfSites, FSiteCoverage, FNoOfGapSites, FNoOfCommonSites, FNoOfInvarSites, FNofOfRates, FNoOfSites, MaxIter, FixedI, FixedJ: integer;
  Result := Result + SizeOf(Integer)*nsites; // f0: array of integer;
  Result := Result + SizeOf(Integer)*nsites; // SC: array of integer;
  Result := Result + SizeOf(Integer)*nrates; //ch: array of integer;
  Result := Result + SizeOf(Extended)*nrates; // FRate: array of extended;
  Result := Result + SizeOf(Extended) *20; // FFreq: array of extended;
  Result := Result + SizeOf(TSeqDataType); // FSeqDataType: TSeqDataType;
  Result := Result + 3*SizeOf(Boolean); // FUseFreq, FixedGamma, FUseInvar: boolean;
  //FBootTable: PArrayOfInt;
  //FBootTable := nil;
  Result := Result + SizeOf(Extended)*4; // D: PArrayOfExtended;
  Result := Result + 4*(SizeOf(Pointer)*4 + SizeOf(Extended)*4*4); // A,Ai,AV, V: PMatrixOfExtended;
  Result := Result + SizeOf(Extended)*4; // DefaultFreq: array [0..3] of extended;
  Result := Result + SizeOf(TComplex)*4; // Lambda: PArrayOfComplex;
  Result := Result + 4*(SizeOf(Pointer)*4 + SizeOf(Extended)*4*4); // r, Q, FQ, FQFI: PMatrixOfExtended;
  Result := Result*2; // pad it as it is not perfect
end;

function GetApproximateSizeOfProteinModelInBytes(nsites, nrates: Integer): Int64;
begin
  Result := 4*SizeOf(Extended); // IniGamma, FGamma, FInvar, deltaL: extended;
  Result := Result + 12*SizeOf(Integer); // FNoOfStates, FTotalNoOfSites, FSiteCoverage, FNoOfGapSites, FNoOfCommonSites, FNoOfInvarSites, FNofOfRates, FNoOfSites, MaxIter, FixedI, FixedJ: integer;
  Result := Result + SizeOf(Integer)*nsites; // f0: array of integer;
  Result := Result + SizeOf(Integer)*nsites; // SC: array of integer;
  Result := Result + SizeOf(Integer)*nrates; //ch: array of integer;
  Result := Result + SizeOf(Extended)*nrates; // FRate: array of extended;
  Result := Result + SizeOf(Extended) *20; // FFreq: array of extended;
  Result := Result + SizeOf(TSeqDataType); // FSeqDataType: TSeqDataType;
  Result := Result + 3*SizeOf(Boolean); // FUseFreq, FixedGamma, FUseInvar: boolean;
  //FBootTable: PArrayOfInt;
  //FBootTable := nil;
  Result := Result + 2*SizeOf(Extended)*20; // D,V: PArrayOfExtended;
  Result := Result + 3*(SizeOf(Pointer)*20 + SizeOf(Extended)*20*20); // A,Ai,AV: PMatrixOfExtended;
  Result := Result + SizeOf(Extended)*20; // DefaultFreq: array [0..19] of extended;
  Result := Result + SizeOf(TComplex)*20; // Lambda: PArrayOfComplex;
  Result := Result + 2*(SizeOf(Pointer)*20 + SizeOf(Extended)*20*20); // r, Q: PMatrixOfExtended;
  Result := Result + SizeOf(Extended)*100; // GenerateDiscreteGammaRates
  Result := Round(Result*2.2); // pad it as it is not perfect
end;

////////////////////////////
// GTR model
////////////////////////////


constructor TGTRModel.Create(gamma: extended; inv: boolean; ncat: integer);
var
  i: integer;
begin
  inherited Create(gamma, inv, ncat);

  FNoOfStates := 4;

  setlength(FFreq, 4);

  GetMem(Lambda, SizeOf(Extended)*4);
  GetMem(F, SizeOf(Extended)*4);
  GetMem(Fi, SizeOf(Extended)*4);
  CreateMatrix(A, 4);
  CreateMatrix(Ai, 4);
  CreateMatrix(V, 4);
  CreateMatrix(AV, 4);
  CreateMatrix(FQ, 4);
  CreateMatrix(FQFi, 4);
  CreateMatrix(Q, 4);

  pA := 0.25;
  pT := 0.25;
  pC := 0.25;
  pG := 0.25;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;

  r[1] := 1;
  r[2] := 1;
  r[3] := 1;
  r[4] := 1;
  r[5] := 1;
  r[6] := 1;

  Q[0,1] := pT*r[3];
  Q[0,2] := pC*r[4];
  Q[0,3] := pG*r[1];
  Q[1,0] := pA*r[3];
  Q[1,2] := pC*r[2];
  Q[1,3] := pG*r[5];
  Q[2,0] := pA*r[4];
  Q[2,1] := pT*r[2];
  Q[2,3] := pG*r[6];
  Q[3,0] := pA*r[1];
  Q[3,1] := pT*r[5];
  Q[3,2] := pC*r[6];
  Q[0,0] := -Q[0,1]-Q[0,2]-Q[0,3];
  Q[1,1] := -Q[1,0]-Q[1,2]-Q[1,3];
  Q[2,2] := -Q[2,0]-Q[2,1]-Q[2,3];
  Q[3,3] := -Q[3,0]-Q[3,1]-Q[3,2];

  s := 2*(pA*pG*r[1] +pT*pC*r[2] +pA*pT*r[3] +pA*pC*r[4] +pT*pG*r[5] +pC*pG*r[6]);

  for i := 0 to 3 do
  begin
    F[i]  := sqrt(FFreq[i]);
    Fi[i] := 1/sqrt(FFreq[i]);
  end;

  ComputeEigenVector(Q);

  FixedIndex := 1;
end;

destructor TGTRModel.Destroy;
begin
  setlength(FFreq, 0);

  FreeMemAndNil(Lambda);
  FreeMemAndNil(F);
  FreeMemAndNil(Fi);
  DestroyMatrix(A, 4);
  DestroyMatrix(Ai, 4);
  DestroyMatrix(V, 4);
  DestroyMatrix(AV, 4);
  DestroyMatrix(FQ, 4);
  DestroyMatrix(FQFi, 4);
  DestroyMatrix(Q, 4);

  inherited;
end;

procedure  TGTRModel.Assign(source: TGammaRateVariationModel);
var
  i,j: integer;
begin
  if not(source is TGTRModel) then
    exit;

  inherited Assign(source);

  pA := TGTRModel(source).pA;
  pT := TGTRModel(source).pT;
  pC := TGTRModel(source).pC;
  pG := TGTRModel(source).pG;
  s  := TGTRModel(source).s;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;
  for i := 1 to 6 do
    r[i] := TGTRModel(source).r[i];

  FixedIndex := TGTRModel(source).FixedIndex;

  for i := 0 to 3 do
  begin
    Lambda[i] := TGTRModel(source).Lambda[i];
    F[i]      := TGTRModel(source).F[i];
    Fi[i]     := TGTRModel(source).Fi[i];

    for j := 0 to 3 do
    begin
      A[i,j]    := TGTRModel(source).A[i,j];
      Ai[i,j]   := TGTRModel(source).Ai[i,j];
      V[i,j]    := TGTRModel(source).V[i,j];
      AV[i,j]   := TGTRModel(source).AV[i,j];
      FQ[i,j]   := TGTRModel(source).FQ[i,j];
      FQFi[i,j] := TGTRModel(source).FQFi[i,j];
      Q[i,j]    := TGTRModel(source).Q[i,j];
    end;
  end;

  FreqFlag := TGTRModel(source).FreqFlag;
end;

procedure TGTRModel.SetFreqs(fA,fT,fC,fG: extended);
var
  i: integer;
begin
  pA := fA;
  pT := fT;
  pC := fC;
  pG := fG;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;

  Q[0,1] := pT*r[3];
  Q[0,2] := pC*r[4];
  Q[0,3] := pG*r[1];
  Q[1,0] := pA*r[3];
  Q[1,2] := pC*r[2];
  Q[1,3] := pG*r[5];
  Q[2,0] := pA*r[4];
  Q[2,1] := pT*r[2];
  Q[2,3] := pG*r[6];
  Q[3,0] := pA*r[1];
  Q[3,1] := pT*r[5];
  Q[3,2] := pC*r[6];
  Q[0,0] := -Q[0,1]-Q[0,2]-Q[0,3];
  Q[1,1] := -Q[1,0]-Q[1,2]-Q[1,3];
  Q[2,2] := -Q[2,0]-Q[2,1]-Q[2,3];
  Q[3,3] := -Q[3,0]-Q[3,1]-Q[3,2];

  s := 2*(pA*pG*r[1] +pT*pC*r[2] +pA*pT*r[3] +pA*pC*r[4] +pT*pG*r[5] +pC*pG*r[6]);

  FreqFlag := true;
  for i := 0 to 3 do
    if FFreq[i] > 0 then
    begin
      F[i]  := sqrt(FFreq[i]);
      Fi[i] := 1/sqrt(FFreq[i]);
    end
    else
    begin
      F[i]  := 0;
      Fi[i] := 0;
      FreqFlag := false;
    end;

  ComputeEigenVector(Q);
end;

procedure TGTRModel.SetParams(r1,r2,r3,r4,r5,r6: extended);
var
  i,j: integer;
begin
  r[1] := r1;
  r[2] := r2;
  r[3] := r3;
  r[4] := r4;
  r[5] := r5;
  r[6] := r6;

  j := 1;
  for i := 2 to 6 do
    if r[i] > r[j] then
      j := i;
  for i := 1 to 6 do
    if i <> j then
      r[i] := r[i]/r[j];
  r[j] := 1;
  FixedIndex := j;

  Q[0,1] := pT*r[3];
  Q[0,2] := pC*r[4];
  Q[0,3] := pG*r[1];
  Q[1,0] := pA*r[3];
  Q[1,2] := pC*r[2];
  Q[1,3] := pG*r[5];
  Q[2,0] := pA*r[4];
  Q[2,1] := pT*r[2];
  Q[2,3] := pG*r[6];
  Q[3,0] := pA*r[1];
  Q[3,1] := pT*r[5];
  Q[3,2] := pC*r[6];
  Q[0,0] := -Q[0,1]-Q[0,2]-Q[0,3];
  Q[1,1] := -Q[1,0]-Q[1,2]-Q[1,3];
  Q[2,2] := -Q[2,0]-Q[2,1]-Q[2,3];
  Q[3,3] := -Q[3,0]-Q[3,1]-Q[3,2];

  s := 2*(pA*pG*r[1] +pT*pC*r[2] +pA*pT*r[3] +pA*pC*r[4] +pT*pG*r[5] +pC*pG*r[6]);

  ComputeEigenVector(Q);
end;

procedure TGTRModel.SetParamsFromSeqs(seqs: TStringList);
var
  TN93Model: TTN93Model;
  i,j: integer;
begin
  inherited;

  TN93Model := TTN93Model.Create(0, UseInvar, NoOfRates);
  TN93Model.SiteCoverage  := SiteCoverage;
  TN93Model.SetParamsFromSeqs(seqs);

  pA := TN93Model.pA;
  pT := TN93Model.pT;
  pC := TN93Model.pC;
  pG := TN93Model.pG;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;

  r[1] := TN93Model.k1;
  r[2] := TN93Model.k2;
  r[3] := 1;
  r[4] := 1;
  r[5] := 1;
  r[6] := 1;

  IniGamma := TN93Model.IniGamma;
  FGamma   := TN93Model.Gamma;
  FInvar   := TN93Model.Invar;
  for i := 0 to NoOfRates-1 do
    FRate[i] := TN93Model.FRate[i];

  TN93Model.Free;

  j := 1;
  for i := 2 to 6 do
    if r[i] > r[j] then
      j := i;
  for i := 1 to 6 do
    if i <> j then
      r[i] := r[i]/r[j];
  r[j] := 1;
  FixedIndex := j;

  Q[0,1] := pT*r[3];
  Q[0,2] := pC*r[4];
  Q[0,3] := pG*r[1];
  Q[1,0] := pA*r[3];
  Q[1,2] := pC*r[2];
  Q[1,3] := pG*r[5];
  Q[2,0] := pA*r[4];
  Q[2,1] := pT*r[2];
  Q[2,3] := pG*r[6];
  Q[3,0] := pA*r[1];
  Q[3,1] := pT*r[5];
  Q[3,2] := pC*r[6];
  Q[0,0] := -Q[0,1]-Q[0,2]-Q[0,3];
  Q[1,1] := -Q[1,0]-Q[1,2]-Q[1,3];
  Q[2,2] := -Q[2,0]-Q[2,1]-Q[2,3];
  Q[3,3] := -Q[3,0]-Q[3,1]-Q[3,2];

  s := 2*(pA*pG*r[1] +pT*pC*r[2] +pA*pT*r[3] +pA*pC*r[4] +pT*pG*r[5] +pC*pG*r[6]);

  FreqFlag := true;
  for i := 0 to 3 do
    if FFreq[i] > 0 then
    begin
      F[i]  := sqrt(FFreq[i]);
      Fi[i] := 1/sqrt(FFreq[i]);
    end
    else
    begin
      F[i]  := 0;
      Fi[i] := 0;
      FreqFlag := false;
    end;

  ComputeEigenVector(Q);
end;

procedure TGTRModel.ComputeEigenVector(Q: PMatrixOfExtended);
var
  tmpLambda: PArrayOfComplex;
  i,j: integer;
begin
  if FreqFlag then
  begin
    for i := 0 to 3 do
      for j := 0 to 3 do
        FQ[i,j] := F[i]*Q[i,j];

    for i := 0 to 3 do
      for j := 0 to 3 do
        FQFi[i,j] := FQ[i,j]*Fi[j];

    Jacobi(FQFi, 0, 3, 100, 0.0000000000001, V, Lambda);

    for i := 0 to 3 do
      for j := 0 to 3 do
        A[i,j] := Fi[i]*V[i,j];
  end
  else
  begin
    GetMem(tmpLambda, SizeOf(TComplex)*4);

    EigenVect(Q, 0, 3, tmpLambda, A);

    for i := 0 to 3 do
      Lambda[i] := tmpLambda[i].X;

    FreeMemAndNil(tmpLambda);
  end;

  InvMat(A, 0, 3, Ai);
end;

procedure TGTRModel.ComputeProb(P: PMatrixOfExtended; b: extended);
var
  i,j,k: integer;
begin
  for i := 0 to 3 do
    V[i,i] := exp(Lambda[i]*b/s);

//  MultiplyMatrix(A, V, AV, 0,3);
  for i := 0 to 3 do
    for j := 0 to 3 do
      AV[i,j] := A[i,j]*V[j,j];

//  MultiplyMatrix(AV, Ai, P, 0,3);
  for i := 0 to 3 do
    for j := 0 to 3 do
    begin
      P[i,j] := 0;
      for k := 0 to 3 do
        P[i,j] := P[i,j] +AV[i,k]*Ai[k,j];
    end;

  for i := 0 to 3 do
  begin
    P[i,i] := 1;
    for j := 0 to 3 do
      if j <> i then
        P[i,i] := P[i,i] -P[i,j];
  end;
end;

procedure TGTRModel.ComputeProbWithGamma(P: PMatrixOfExtended; b: extended);
var
  i,j,k: integer;
begin
  for i := 0 to 3 do
    V[i,i] := exp(-FGamma*ln(1 -Lambda[i]*b/s/FGamma));

//  MultiplyMatrix(A, V, AV, 0,3);
  for i := 0 to 3 do
    for j := 0 to 3 do
      AV[i,j] := A[i,j]*V[j,j];

//  MultiplyMatrix(AV, Ai, P, 0,3);
  for i := 0 to 3 do
    for j := 0 to 3 do
    begin
      P[i,j] := 0;
      for k := 0 to 3 do
        P[i,j] := P[i,j] +AV[i,k]*Ai[k,j];
      if P[i,j] < 0 then
        P[i,j] := 0;
    end;

  for i := 0 to 3 do
  begin
    P[i,i] := 1;
    for j := 0 to 3 do
      if j <> i then
        P[i,i] := P[i,i] -P[i,j];
  end;
end;

procedure TGTRModel.ChangeParams(r1,r2,r3,r4,r5,r6: extended);
begin
  Q[0,1] := pT*r3;
  Q[0,2] := pC*r4;
  Q[0,3] := pG*r1;
  Q[1,0] := pA*r3;
  Q[1,2] := pC*r2;
  Q[1,3] := pG*r5;
  Q[2,0] := pA*r4;
  Q[2,1] := pT*r2;
  Q[2,3] := pG*r6;
  Q[3,0] := pA*r1;
  Q[3,1] := pT*r5;
  Q[3,2] := pC*r6;
  Q[0,0] := -Q[0,1]-Q[0,2]-Q[0,3];
  Q[1,1] := -Q[1,0]-Q[1,2]-Q[1,3];
  Q[2,2] := -Q[2,0]-Q[2,1]-Q[2,3];
  Q[3,3] := -Q[3,0]-Q[3,1]-Q[3,2];

  s := 2*(pA*pG*r1 +pT*pC*r2 +pA*pT*r3 +pA*pC*r4 +pT*pG*r5 +pC*pG*r6);

  ComputeEigenVector(Q);
end;

procedure TGTRModel.ResetProb(tree: TMLTree);
var
  i,j: integer;
begin
  for i := 0 to  2*tree.NoOfSeqs-2 do
  begin
    if tree.Node[i] = tree.Root then
      continue;
    for j := 0 to NoOfRates-1 do
      ComputeProb(tree.Node[i].Prob[j], tree.Node[i].blen*FRate[j]/(1-FInvar));
  end;
end;

procedure TGTRModel.OptimizeParameters(tree: TMLTree; aCheckCancel: TCheckCancelFunc = nil);
var
  L0,dL0,dL1,dL2,dL,ddL,r0,dr: extended;
  p,d: array [1..6] of extended;
  iter,i,j: integer;
begin
  iter := 0;

  for i := 1 to 6 do
    if r[i] < 0.000000001 then
      d[i] := 0.000000000001
    else
      d[i] := r[i]/1000;

  repeat
    L0  := tree.LogLikelihood;

    for i := 1 to 6 do
    begin
      if i = FixedIndex then
        continue;

      dL0 := tree.LogLikelihood;

      r0 := r[i];
      for j := 1 to 6 do
        p[j] := r[j];

      p[i] := r[i]+d[i];
      ChangeParams(p[1],p[2],p[3],p[4],p[5],p[6]);
      ResetProb(tree);
      dL1 := tree.ResetL1;

      p[i] := r[i]-d[i];
      ChangeParams(p[1],p[2],p[3],p[4],p[5],p[6]);
      ResetProb(tree);
      dL2 := tree.ResetL1;

      dL  := (dL1-dL2)/d[i]/2;
      ddL := ((dL1-2*dL0+dL2))/d[i]/d[i];

      if abs(ddL) > 0 then
        dr := dL/ddL
      else
        dr := 0;
      if dr > r[i] then
        dr := r[i];
      r[i] := r[i] -dr;

      if r[i] < 0 then
        r[i] := 0;

      ChangeParams(r[1],r[2],r[3],r[4],r[5],r[6]);
      ResetProb(tree);

      tree.LogLikelihood := tree.ResetL1;
      if tree.LogLikelihood < dL0 then
      begin
        r[i] := r0;
        ChangeParams(r[1],r[2],r[3],r[4],r[5],r[6]);
        ResetProb(tree);
        tree.LogLikelihood := tree.ResetL1;
      end
      else if (abs(dr) > 0) and (abs(dr)/2 < d[i]) then
        d[i] := abs(dr)/2;
    end;
    inc(iter);
    if Assigned(aCheckCancel) then
      aCheckCancel(Round(iter/MaxIter*100), 'Optimizing parameters');
  until (abs(tree.LogLikelihood-L0) < deltaL) or (iter = MaxIter);
  if Assigned(aCheckCancel) then
    aCheckCancel(100, 'Optimizing parameters');
  inherited;
end;

procedure TGTRModel.GetParams(var r1,r2,r3,r4,r5,r6: extended);
begin
  r1 := r[1];
  r2 := r[2];
  r3 := r[3];
  r4 := r[4];
  r5 := r[5];
  r6 := r[6];
end;

function TGTRModel.PatternBias: Extended;
var
  r1,r2,r3: Extended;
begin
  result := 0;
  r3 := (r[3]+r[4]+r[5]+r[6])/4;
  if r3 < 0.000000000001 then
    exit;

  r1 := r[1]/r3;
  r2 := r[2]/r3;

  result :=  ((0.25-pA)*(0.25-pA) + (0.25-pG)*(0.25-pG))*r1
            +((0.25-pT)*(0.25-pT) + (0.25-pC)*(0.25-pC))*r2;
end;

procedure TGTRModel.GetInfo(ModelInfo: TModelInfo);
var
  i: integer;
begin
  if ModelInfo = nil then exit;

  inherited GetInfo(ModelInfo);

  for i := 0 to 3 do
    ModelInfo.Freq[i] := Freq[i];

  ModelInfo.DataType  := 'DNA';
  ModelInfo.ModelName := 'GTR' +ModelInfo.ModelName;
  ModelInfo.FullName  := 'General Time Reversible model'+ModelInfo.FullName;
  ModelInfo.NoOfParams  := 8;
  ModelInfo.NoOfSamples := TotalNoOfSites;
  ModelInfo.Matrix[0,1] := pT*r[3];
  ModelInfo.Matrix[0,2] := pC*r[4];
  ModelInfo.Matrix[0,3] := pG*r[1];
  ModelInfo.Matrix[1,0] := pA*r[3];
  ModelInfo.Matrix[1,2] := pC*r[2];
  ModelInfo.Matrix[1,3] := pG*r[5];
  ModelInfo.Matrix[2,0] := pA*r[4];
  ModelInfo.Matrix[2,1] := pT*r[2];
  ModelInfo.Matrix[2,3] := pG*r[6];
  ModelInfo.Matrix[3,0] := pA*r[1];
  ModelInfo.Matrix[3,1] := pT*r[5];
  ModelInfo.Matrix[3,2] := pC*r[6];
  ModelInfo.SVR         := (pA*pG*r[1] +pT*pC*r[2])/(pA*pT*r[3] +pA*pC*r[4] +pT*pG*r[5] +pC*pG*r[6]);
end;


function TGTRModel.GetName: String;
begin
  Result := 'GTR';
end;

function TGTRModel.ReadFromFile(var data: File):boolean;
var
  i,version: integer;
begin
  version := -1;
  result := false;
  try
    if not inherited ReadFromFile(data) then
      exit;

    BlockRead(data, version, sizeof(integer));

    for i := 1 to 6 do
      BlockRead(data, r[i], sizeof(extended));

    BlockRead(data, FixedIndex, sizeof(integer));

    pA := Freq[0];
    pT := Freq[1];
    pC := Freq[2];
    pG := Freq[3];
    Q[0,1] := pT*r[3];
    Q[0,2] := pC*r[4];
    Q[0,3] := pG*r[1];
    Q[1,0] := pA*r[3];
    Q[1,2] := pC*r[2];
    Q[1,3] := pG*r[5];
    Q[2,0] := pA*r[4];
    Q[2,1] := pT*r[2];
    Q[2,3] := pG*r[6];
    Q[3,0] := pA*r[1];
    Q[3,1] := pT*r[5];
    Q[3,2] := pC*r[6];
    Q[0,0] := -Q[0,1]-Q[0,2]-Q[0,3];
    Q[1,1] := -Q[1,0]-Q[1,2]-Q[1,3];
    Q[2,2] := -Q[2,0]-Q[2,1]-Q[2,3];
    Q[3,3] := -Q[3,0]-Q[3,1]-Q[3,2];

    s := 2*(pA*pG*r[1] +pT*pC*r[2] +pA*pT*r[3] +pA*pC*r[4] +pT*pG*r[5] +pC*pG*r[6]);

    FreqFlag := true;
    for i := 0 to 3 do
      if FFreq[i] > 0 then
      begin
        F[i]  := sqrt(FFreq[i]);
        Fi[i] := 1/sqrt(FFreq[i]);
      end
      else
      begin
        F[i]  := 0;
        Fi[i] := 0;
        FreqFlag := false;
      end;

    ComputeEigenVector(Q);

    result := true;
  except
    result := false;
  end;
end;

procedure TGTRModel.WriteToFile(var data: File);
var
  i,version: integer;
begin
  inherited WriteToFile(data);

  version := 1;

  BlockWrite(data, version, sizeof(integer));

  for i := 1 to 6 do
    BlockWrite(data, r[i], sizeof(extended));

  BlockWrite(data, FixedIndex, sizeof(integer));
end;

////////////////////////
//  TTN93 model
////////////////////////

constructor TTN93Model.Create(gamma: extended; inv: boolean; ncat: integer);
begin
  inherited Create(gamma, inv, ncat);

  FNoOfStates := 4;

  setlength(FFreq, 4);

  pA := 0.25;
  pT := 0.25;
  pC := 0.25;
  pG := 0.25;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;

  k1 := 1;
  k2 := 1;
  pR := pA+pG;
  pY := pT+pC;
end;

destructor TTN93Model.Destroy;
begin
  setlength(FFreq, 0);

  inherited;
end;

procedure TTN93Model.Assign(source: TGammaRateVariationModel);
begin
  inherited Assign(source);

  if not(source is TTN93Model) then
    exit;

  pA := TTN93Model(source).pA;
  pT := TTN93Model(source).pT;
  pC := TTN93Model(source).pC;
  pG := TTN93Model(source).pG;
  pR := TTN93Model(source).pR;
  pY := TTN93Model(source).pY;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;
  k1 := TTN93Model(source).k1;
  k2 := TTN93Model(source).k2;
end;

procedure TTN93Model.SetFreqs(FreqA,FreqT,FreqC,FreqG: extended);
begin
  pA := FreqA;
  pT := FreqT;
  pC := FreqC;
  pG := FreqG;
  pR := pA+pG;
  pY := pT+pC;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;
end;

procedure TTN93Model.SetParams(kappa1,kappa2: extended);
begin
  k1 := kappa1;
  k2 := kappa2;
end;

procedure TTN93Model.SetParamsFromSeqs(seqs: TStringList);
var
  h: array of integer;
  b: array of extended;
  m,p: array of PMatrixOfExtended;
  p1,p2,q,r,s: extended;
  d1,d2,dL,ddL,dL0,dL1,dL2,L,L0,k0,nn: extended;
  s1,s2: AnsiString;
  i,j,k,n,n1,n2,d,it: integer;
begin
  inherited;

  setlength(h,seqs.Count);
  setlength(b,seqs.Count);
  setlength(m,seqs.Count);
  for i := 0 to seqs.Count-1 do
    CreateMatrix(m[i], 4);
  setlength(p,seqs.Count);
  for i := 0 to seqs.Count-1 do
    CreateMatrix(p[i], 4);

  SortSeqs(seqs, h);

  if (NoOfRates > 1) and (FGamma < 0.00000000001) then
    InitGammaParamFromSeqs(seqs, h);

  pA := 0;
  pT := 0;
  pC := 0;
  pG := 0;
  n := length(seqs[0]);
  for k := 1 to n do
  begin
    if FBootTable = nil then
      d := 1
    else if FBootTable[k] = 0 then
      continue
    else
      d := FBootTable[k];

    for i := 0 to seqs.Count-1 do
      case upcase(seqs[i][k]) of
        'A': pA := pA+d;
        'T',
        'U': pT := pT+d;
        'C': pC := pC+d;
        'G': pG := pG+d;
      end;
  end;
  if not (FTotalNoOfSites > 0) then
    raise EDivByZero.Create('division by zero in SetParamsFromSeqs');
  nn := pA+pT+pC+pG;
  pA := pA/nn;
  pT := pT/nn;
  pC := pC/nn;
  pG := pG/nn;
  pR := pA+pG;
  pY := pT+pC;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;

  for i := 0 to seqs.Count-1 do
  begin
    for n1 := 0 to 3 do
      for n2 := 0 to 3 do
        m[i][n1,n2] := 0;

    if i = seqs.Count-1 then
      j := 0
    else
      j := i+1;

    s1 := seqs[h[i]];
    s2 := seqs[h[j]];
    for k := 1 to n do
    begin
      if FBootTable = nil then
        d := 1
      else if FBootTable[k] = 0 then
        continue
      else
        d := FBootTable[k];
      case upcase(s1[k]) of
        'A': n1 := 0;
        'T',
        'U': n1 := 1;
        'C': n1 := 2;
        'G': n1 := 3;
        else
          continue;
      end;
      case upcase(s2[k]) of
        'A': n2 := 0;
        'T',
        'U': n2 := 1;
        'C': n2 := 2;
        'G': n2 := 3;
        else
          continue;
      end;

      m[i][n1,n2] := m[i][n1,n2] +d;
    end;
  end;

  k1 := 0;
  k2 := 0;
  s  := 0;
  for i := 0 to seqs.Count-1 do
  begin
    p1 := m[i][0,3]+m[i][3,0];
    p2 := m[i][1,2]+m[i][2,1];
    q  := m[i][0,1]+m[i][1,0]+m[i][0,2]+m[i][2,0]+m[i][1,3]+m[i][3,1]+m[i][2,3]+m[i][3,2];
    r  := p1+p2+q +m[i][0,0] +m[i][1,1] +m[i][2,2] +m[i][3,3];
    if r < 0.000000000001 then
      continue;
    p1 := p1/r;
    p2 := p2/r;
    q  := q/r;
    if (pA*pG > 0) and (pT*pC > 0) then
      if ((pR/pA/pG/2*p1 +q/pR/2) < 1) and ((pY/pT/pC/2*p2 +q/pY/2) < 1) and (q/2/pR/pY < 1) then
      begin
        r := -(ln(1 -pR/pA/pG/2*p1 -q/pR/2) -pY*ln(1 -q/2/pR/pY))/pR;
        if r > 0 then
          k1 := k1 +r;
        r := -(ln(1 -pY/pT/pC/2*p2 -q/pY/2) -pR*ln(1 -q/2/pR/pY))/pY;
        if r > 0 then
          k2 := k2 +r;
        s  := s  -ln(1 -q/2/pR/pY);
      end;
  end;
  if (s > 0) then
  begin
    k1 := k1/s;
    k2 := k2/s;
  end
  else
  begin
    k1 := 1;
    k2 := 1;
  end;

  for i := 0 to seqs.Count-1 do
    b[i] := ComputeDistance(m[i], true);

  L := 0;
  for i := 0 to seqs.Count-1 do
    L := L +CompositeLikelihood(m[i],b[i], true);

  it := 0;
  repeat
    L0 := L;
    dL0 := L;

    if (pA*pG > 0) and (k1 > 0.000000000001) then
    begin
      k0 := k1;
      d1 := k1/1000;

      k1 := k1 +d1;
      dL1 := 0;
      for i := 0 to seqs.Count-1 do
        dL1 := dL1 +CompositeLikelihood(m[i],b[i], true);

      k1 := k1 -2*d1;
      dL2 := 0;
      for i := 0 to seqs.Count-1 do
        dL2 := dL2 +CompositeLikelihood(m[i],b[i], true);

      dL  := (dL1-dL2)/d1/2;
      ddL := ((dL1-2*dL0+dL2))/d1/d1;

      if abs(ddL) > 0 then
        if dL/ddL > k1/2 then
          k1 := k1/2
        else if dL/ddL < -k1 then
          k1 := k1*2
        else
          k1 := k1 -dL/ddL;

      L := 0;
      for i := 0 to seqs.Count-1 do
        L := L +CompositeLikelihood(m[i],b[i], true);
      if L < L0 then
      begin
        k1 := k0;
        L  := dL0;
      end;
    end;

    if (pT*pC > 0) and (k2 > 0.000000000001) then
    begin
      dL0 := L;

      k0 := k2;
      d2 := k2/1000;

      k2 := k2 +d2;
      dL1 := 0;
      for i := 0 to seqs.Count-1 do
        dL1 := dL1 +CompositeLikelihood(m[i],b[i], true);

      k2 := k2 -2*d2;
      dL2 := 0;
      for i := 0 to seqs.Count-1 do
        dL2 := dL2 +CompositeLikelihood(m[i],b[i], true);

      dL  := (dL1-dL2)/d2/2;
      ddL := ((dL1-2*dL0+dL2))/d2/d2;

      if abs(ddL) > 0 then
        if dL/ddL > k2/2 then
          k2 := k2/2
        else if dL/ddL < -k2 then
          k2 := k2*2
        else
          k2 := k2 -dL/ddL;

      L := 0;
      for i := 0 to seqs.Count-1 do
        L := L +CompositeLikelihood(m[i],b[i], true);
      if L < dL0 then
      begin
        k2 := k0;
        L  := dL0;
      end;
    end;

    inc(it);
  until (abs(L-L0) < deltaL) or (it = MaxIter);

  setlength(h,0);
  setlength(b,0);
  for i := 0 to seqs.Count-1 do
    DestroyMatrix(m[i], 4);
  setlength(m,0);
  for i := 0 to seqs.Count-1 do
    DestroyMatrix(p[i], 4);
  setlength(p,0);
end;

procedure TTN93Model.ComputeProb(P: PMatrixOfExtended; b: extended);
var
  P1,P2,Q,r: extended;

  function EP1: extended;
  begin
    result := (pR +pY*exp(-2*b/r) -exp(-2*(pR*k1 +pY)*b/r))/pR;
  end;

  function EP2: extended;
  begin
    result := (pY +pR*exp(-2*b/r) -exp(-2*(pY*k2 +pR)*b/r))/pY;
  end;

  function EQ: extended;
  begin
    result := 1 -exp(-2*b/r);
  end;

begin
  r := 4*(pA*pG*k1 +pT*pC*k2 +pR*pY);

  P1 := EP1;
  P2 := EP2;
  Q  := EQ;

  P[0,1] := pT*Q;
  P[0,2] := pC*Q;
  P[0,3] := pG*P1;
  P[0,0] := 1-P[0,1]-P[0,2]-P[0,3];

  P[1,0] := pA*Q;
  P[1,2] := pC*P2;
  P[1,3] := pG*Q;
  P[1,1] := 1-P[1,0]-P[1,2]-P[1,3];

  P[2,0] := pA*Q;
  P[2,1] := pT*P2;
  P[2,3] := pG*Q;
  P[2,2] := 1-P[2,0]-P[2,1]-P[2,3];

  P[3,0] := pA*P1;
  P[3,1] := pT*Q;
  P[3,2] := pC*Q;
  P[3,3] := 1-P[3,0]-P[3,1]-P[3,2];
end;

procedure TTN93Model.ComputeProbWithGamma(P: PMatrixOfExtended; b: extended);
var
  P1,P2,Q,r: extended;

  function EP1: extended;
  begin
    result := (pR +pY*exp(FGamma*ln(FGamma/(FGamma +2*b/r))) -exp(FGamma*ln(FGamma/(FGamma +2*(pR*k1 +pY)*b/r))))/pR;
  end;

  function EP2: extended;
  begin
    result := (pY +pR*exp(FGamma*ln(FGamma/(FGamma +2*b/r))) -exp(FGamma*ln(FGamma/(FGamma +2*(pY*k2 +pR)*b/r))))/pY;
  end;

  function EQ: extended;
  begin
    result := 1 -exp(FGamma*ln(FGamma/(FGamma +2*b/r)));
  end;

begin
  r := 4*(pA*pG*k1 +pT*pC*k2 +pR*pY);

  P1 := EP1;
  P2 := EP2;
  Q  := EQ;

  P[0,1] := pT*Q;
  P[0,2] := pC*Q;
  P[0,3] := pG*P1;
  P[0,0] := 1-P[0,1]-P[0,2]-P[0,3];

  P[1,0] := pA*Q;
  P[1,2] := pC*P2;
  P[1,3] := pG*Q;
  P[1,1] := 1-P[1,0]-P[1,2]-P[1,3];

  P[2,0] := pA*Q;
  P[2,1] := pT*P2;
  P[2,3] := pG*Q;
  P[2,2] := 1-P[2,0]-P[2,1]-P[2,3];

  P[3,0] := pA*P1;
  P[3,1] := pT*Q;
  P[3,2] := pC*Q;
  P[3,3] := 1-P[3,0]-P[3,1]-P[3,2];
end;



procedure TTN93Model.ChangeProbOfNode(node: TMLTreeNode; b,k1,k2: extended);
var
  P1,P2,Q,r: extended;

  function EP1: extended;
  begin
    result := (pR +pY*exp(-2*b/r) -exp(-2*(pR*k1 +pY)*b/r))/pR;
  end;

  function EP2: extended;
  begin
    result := (pY +pR*exp(-2*b/r) -exp(-2*(pY*k2 +pR)*b/r))/pY;
  end;

  function EQ: extended;
  begin
    result := 1 -exp(-2*b/r);
  end;

var
  i: integer;
begin
  with node do
    for i := 0 to NoOfRates-1 do
    begin
      if FRate[i] < 0.000000000001 then
      begin
        Prob[i][0,1] := 0;
        Prob[i][0,2] := 0;
        Prob[i][0,3] := 0;
        Prob[i][0,0] := 1;

        Prob[i][1,0] := 0;
        Prob[i][1,2] := 0;
        Prob[i][1,3] := 0;
        Prob[i][1,1] := 1;

        Prob[i][2,0] := 0;
        Prob[i][2,1] := 0;
        Prob[i][2,3] := 0;
        Prob[i][2,2] := 1;

        Prob[i][3,0] := 0;
        Prob[i][3,1] := 0;
        Prob[i][3,2] := 0;
        Prob[i][3,3] := 1;
      end
      else
      begin
        r := 4*(pA*pG*k1 +pT*pC*k2 +pR*pY)/(FRate[i]/(1-FInvar));

        P1 := EP1;
        P2 := EP2;
        Q  := EQ;

        Prob[i][0,1] := pT*Q;
        Prob[i][0,2] := pC*Q;
        Prob[i][0,3] := pG*P1;
        Prob[i][0,0] := 1-Prob[i][0,1]-Prob[i][0,2]-Prob[i][0,3];

        Prob[i][1,0] := pA*Q;
        Prob[i][1,2] := pC*P2;
        Prob[i][1,3] := pG*Q;
        Prob[i][1,1] := 1-Prob[i][1,0]-Prob[i][1,2]-Prob[i][1,3];

        Prob[i][2,0] := pA*Q;
        Prob[i][2,1] := pT*P2;
        Prob[i][2,3] := pG*Q;
        Prob[i][2,2] := 1-Prob[i][2,0]-Prob[i][2,1]-Prob[i][2,3];

        Prob[i][3,0] := pA*P1;
        Prob[i][3,1] := pT*Q;
        Prob[i][3,2] := pC*Q;
        Prob[i][3,3] := 1-Prob[i][3,0]-Prob[i][3,1]-Prob[i][3,2];
      end;
    end;
end;

procedure TTN93Model.OptimizeParameters(tree: TMLTree; aCheckCancel: TCheckCancelFunc = nil);
var
  L0, dL0,dL1,dL2,dL,ddL,d1,d2,k0,dk,ek: extended;
  iter,i: integer;
begin
  d1 := k1/1000;
  d2 := k2/1000;

  iter := 0;
  repeat
    L0  := tree.LogLikelihood;

    dL0 := tree.LogLikelihood;

    if (pA*pG > 0) and (k1 > 0.000000000001) then
    begin
      k0 := k1;
      for i := 0 to  2*tree.NoOfSeqs-2 do
      begin
        if tree.Node[i] = tree.Root then
          continue;
        ChangeProbOfNode(tree.Node[i], tree.Node[i].blen, k1+d1,k2);
      end;
      dL1 := tree.ResetL1;

      for i := 0 to  2*tree.NoOfSeqs-2 do
      begin
        if tree.Node[i] = tree.Root then
          continue;
        ChangeProbOfNode(tree.Node[i], tree.Node[i].blen, k1-d1,k2);
      end;
      dL2 := tree.ResetL1;

      dL  := (dL1-dL2)/d1/2;
      ddL := ((dL1-2*dL0+dL2))/d1/d1;

      if abs(ddL) > 0 then
      begin
        dk := dL/ddL;
        ek := sqrt(abs(1/ddL));
      end
      else
      begin
        dk := 0;
        ek := 0;
      end;

      if abs(dk) < 3*ek then
        if dk > k1/2 then
          k1 := k1/2
        else if dk < -k1 then
          k1 := k1*2
        else
          k1 := k1 -dk;

      for i := 0 to  2*tree.NoOfSeqs-2 do
      begin
        if tree.Node[i] = tree.Root then
          continue;
        ChangeProbOfNode(tree.Node[i], tree.Node[i].blen, k1,k2);
      end;
      tree.LogLikelihood := tree.ResetL1;
      if tree.LogLikelihood < dL0 then
      begin
        k1 := k0;
        for i := 0 to  2*tree.NoOfSeqs-2 do
        begin
          if tree.Node[i] = tree.Root then
            continue;
          ChangeProbOfNode(tree.Node[i], tree.Node[i].blen, k1,k2);
        end;
        tree.LogLikelihood := tree.ResetL1;
      end
      else if (abs(dk) > 0) and (abs(dk)/2 < d1) then
        d1 := abs(dk)/2;
    end;

    if (pT*pC > 0) and (k2 > 0.000000000001) then
    begin
      dL0 := tree.LogLikelihood;

      k0 := k2;
      for i := 0 to  2*tree.NoOfSeqs-2 do
      begin
        if tree.Node[i] = tree.Root then
          continue;
        ChangeProbOfNode(tree.Node[i], tree.Node[i].blen, k1,k2+d2);
      end;
      dL1 := tree.ResetL1;

      for i := 0 to  2*tree.NoOfSeqs-2 do
      begin
        if tree.Node[i] = tree.Root then
          continue;
        ChangeProbOfNode(tree.Node[i], tree.Node[i].blen, k1,k2-d2);
      end;
      dL2 := tree.ResetL1;

      dL  := (dL1-dL2)/d2/2;
      ddL := ((dL1-2*dL0+dL2))/d2/d2;

      if abs(ddL) > 0 then
      begin
        dk := dL/ddL;
        ek := sqrt(abs(1/ddL));
      end
      else
      begin
        dk := 0;
        ek := 0;
      end;

      if abs(dk) < 3*ek then
        if dk > k2/2 then
          k2 := k2/2
        else if dk < -k2 then
          k2 := k2*2
        else
          k2 := k2 -dk;

      for i := 0 to  2*tree.NoOfSeqs-2 do
      begin
        if tree.Node[i] = tree.Root then
          continue;
        ChangeProbOfNode(tree.Node[i], tree.Node[i].blen, k1,k2);
      end;
      tree.LogLikelihood := tree.ResetL1;

      if tree.LogLikelihood < dL0 then
      begin
        k2 := k0;
        for i := 0 to  2*tree.NoOfSeqs-2 do
        begin
          if tree.Node[i] = tree.Root then
            continue;
          ChangeProbOfNode(tree.Node[i], tree.Node[i].blen, k1,k2);
        end;
        tree.LogLikelihood := tree.ResetL1;
      end
      else if (abs(dk) > 0) and (abs(dk)/2 < d2) then
        d2 := abs(dk)/2;
    end;
    inc(iter);
    if Assigned(aCheckCancel) then
      aCheckCancel(Round(iter/MaxIter*100), 'Optimizing parameters');
  until (abs(tree.LogLikelihood-L0) < deltaL) or (iter = MaxIter);
  if Assigned(aCheckCancel) then
    aCheckCancel(100, 'Optimizing parameters');
  inherited;
end;

function TTN93Model.PatternBias: Extended;
begin
  result :=  ((0.25-pA)*(0.25-pA) + (0.25-pG)*(0.25-pG))*k1
            +((0.25-pT)*(0.25-pT) + (0.25-pC)*(0.25-pC))*k2;
end;

procedure TTN93Model.GetParams(var kappa1,kappa2: extended);
begin
  kappa1 := k1;
  kappa2 := k2;
end;

function TTN93Model.CompositeLikelihood(M: PMatrixOfExtended; b: extended; usegamma: boolean): extended;
var
  p: PMatrixOfExtended;
  r1,r0: extended;
begin
  result := 0;
  p := nil;
  CreateMatrix(p, NoOfStates);

  if usegamma and (Gamma > 0.001) then
    ComputeProbWithGamma(p, b/(1-FInvar))
  else
    ComputeProb(p, b/(1-FInvar));

  r0 := (Freq[0]*p[0,3]+Freq[3]*p[3,0])*(1-FInvar);
  r1 := m[0,3]+m[3,0];
  if (r0 > 0) and (r1 > 0) then
    result := result +r1*ln(r0);
  r0 := (Freq[1]*p[1,2]+Freq[2]*p[2,1])*(1-FInvar);
  r1 := m[1,2]+m[2,1];
  if (r0 > 0) and (r1 > 0) then
    result := result +r1*ln(r0);
  r0 := (Freq[0]*(p[0,1]+p[0,2])+Freq[1]*(p[1,0]+p[1,3])+Freq[2]*(p[2,0]+p[2,3])+Freq[3]*(p[3,1]+p[3,2]))*(1-FInvar);
  r1 := m[0,1]+m[1,0]+m[0,2]+m[2,0]+m[1,3]+m[3,1]+m[2,3]+m[3,2];
  if (r0 > 0) and (r1 > 0) then
    result := result +r1*ln(r0);

  r0 :=  (Freq[0]*p[0,0]+Freq[1]*p[1,1]+Freq[2]*p[2,2]+Freq[3]*p[3,3])*(1-FInvar) +FInvar;
  r1 := m[0,0]+m[1,1]+m[2,2]+m[3,3];
  if (r0 > 0) and (r1 > 0) then
    result := result +r1*ln(r0);

  DestroyMatrix(p,4);
end;

procedure TTN93Model.GetInfo(ModelInfo: TModelInfo);
var
  i: integer;
begin
  if ModelInfo = nil then exit;

  inherited GetInfo(ModelInfo);

  for i := 0 to 3 do
    ModelInfo.Freq[i] := Freq[i];

  ModelInfo.DataType  := 'DNA';
  ModelInfo.ModelName := 'TN93' +ModelInfo.ModelName;
  ModelInfo.FullName  := 'Tamura-Nei (1993) model'+ModelInfo.FullName;
  ModelInfo.NoOfParams  := 5;
  ModelInfo.NoOfSamples := TotalNoOfSites;
  ModelInfo.Matrix[0,1] := pT;
  ModelInfo.Matrix[0,2] := pC;
  ModelInfo.Matrix[0,3] := pG*k1;
  ModelInfo.Matrix[1,0] := pA;
  ModelInfo.Matrix[1,2] := pC*k2;
  ModelInfo.Matrix[1,3] := pG;
  ModelInfo.Matrix[2,0] := pA;
  ModelInfo.Matrix[2,1] := pT*k2;
  ModelInfo.Matrix[2,3] := pG;
  ModelInfo.Matrix[3,0] := pA*k1;
  ModelInfo.Matrix[3,1] := pT;
  ModelInfo.Matrix[3,2] := pC;
  ModelInfo.SVR         := (pA*pG*k1 +pT*pC*k2)/(pA*pT +pA*pC +pT*pG +pC*pG);
end;

function TTN93Model.GetName: String;
begin
  Result := 'TN93';
end;

function TTN93Model.ReadFromFile(var data: File):boolean;
var
  version: integer;
begin
  version := -1;
  result := false;
  try
    if not inherited ReadFromFile(data) then
      exit;

    BlockRead(data, version, sizeof(integer));

    BlockRead(data, k1, sizeof(extended));
    BlockRead(data, k2, sizeof(extended));
    pA := Freq[0];
    pT := Freq[1];
    pC := Freq[2];
    pG := Freq[3];
    pR := pA+pG;
    pY := pT+pC;

    result := true;
  except
    result := false;
  end;
end;

procedure TTN93Model.WriteToFile(var data: File);
var
  version: integer;
begin
  inherited WriteToFile(data);

  version := 1;

  BlockWrite(data, version, sizeof(integer));

  BlockWrite(data, k1, sizeof(extended));
  BlockWrite(data, k2, sizeof(extended));
end;

////////////////////////
//  THKY model
////////////////////////

constructor THKYModel.Create(gamma: extended; inv: boolean; ncat: integer);
begin
  inherited Create(gamma, inv, ncat);

  FNoOfStates := 4;

  setlength(FFreq, 4);

  pA := 0.25;
  pT := 0.25;
  pC := 0.25;
  pG := 0.25;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;

  k  := 1;

  pR := pA+pG;
  pY := pT+pC;
end;

destructor THKYModel.Destroy;
begin
  setlength(FFreq, 0);

  inherited;
end;

procedure THKYModel.Assign(source: TGammaRateVariationModel);
begin
  inherited Assign(source);

  if not(source is THKYModel) then
    exit;

  pA := THKYModel(source).pA;
  pT := THKYModel(source).pT;
  pC := THKYModel(source).pC;
  pG := THKYModel(source).pG;
  pR := THKYModel(source).pR;
  pY := THKYModel(source).pY;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;
  k := THKYModel(source).k;
end;

procedure THKYModel.SetFreqs(FreqA,FreqT,FreqC,FreqG: extended);
begin
  pA := FreqA;
  pT := FreqT;
  pC := FreqC;
  pG := FreqG;
  pR := pA+pG;
  pY := pT+pC;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;
end;

procedure THKYModel.SetParams(kappa: extended);
begin
  k  := kappa;
end;

procedure THKYModel.SetFreqsFromSeqs(seqs: TStringList);
var
  i,k,n,d: integer;
  nn: extended;
begin
  pA := 0;
  pT := 0;
  pC := 0;
  pG := 0;
  n := length(seqs[0]);

  for k := 1 to n do
  begin
    if FBootTable = nil then
      d := 1
    else if FBootTable[k] = 0 then
      continue
    else
      d := FBootTable[k];
    for i := 0 to seqs.Count-1 do
      case upcase(seqs[i][k]) of
        'A': pA := pA+d;
        'T',
        'U': pT := pT+d;
        'C': pC := pC+d;
        'G': pG := pG+d;
      end;
  end;
  nn := pA+pT+pC+pG;
  pA := pA/nn;
  pT := pT/nn;
  pC := pC/nn;
  pG := pG/nn;
  pR := pA+pG;
  pY := pT+pC;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;
end;

procedure THKYModel.SetParamsFromSeqs(seqs: TStringList);
var
  h: array of integer;
  b: array of extended;
  m,p: array of PMatrixOfExtended;
  p1,p2,q,r,s: extended;
  dk,dL,ddL,dL0,dL1,dL2,L,k0: extended;
  s1,s2: AnsiString;
  i,j,n,n1,n2,d,it: integer;
begin
  inherited;

  SetFreqsFromSeqs(seqs);

  setlength(h,seqs.Count);
  setlength(b,seqs.Count);
  setlength(m,seqs.Count);
  for i := 0 to seqs.Count-1 do
    CreateMatrix(m[i], 4);
  setlength(p,seqs.Count);
  for i := 0 to seqs.Count-1 do
    CreateMatrix(p[i], 4);

  SortSeqs(seqs, h);

  if (NoOfRates > 1) and (FGamma < 0.00000000001) then
    InitGammaParamFromSeqs(seqs, h);

  for i := 0 to seqs.Count-1 do
  begin
    for n1 := 0 to 3 do
      for n2 := 0 to 3 do
        m[i][n1,n2] := 0;

    if i = seqs.Count-1 then
      j := 0
    else
      j := i+1;

    s1 := seqs[h[i]];
    s2 := seqs[h[j]];
    for n := 1 to length(seqs[0]) do
    begin
      if FBootTable = nil then
        d := 1
      else if FBootTable[n] = 0 then
        continue
      else
        d := FBootTable[n];

      case upcase(s1[n]) of
        'A': n1 := 0;
        'T',
        'U': n1 := 1;
        'C': n1 := 2;
        'G': n1 := 3;
        else
          continue;
      end;
      case upcase(s2[n]) of
        'A': n2 := 0;
        'T',
        'U': n2 := 1;
        'C': n2 := 2;
        'G': n2 := 3;
        else
          continue;
      end;
      m[i][n1,n2] := m[i][n1,n2] +d;
    end;
  end;

  k := 0;
  s := 0;
  for i := 0 to seqs.Count-1 do
  begin
    p1 := m[i][0,3]+m[i][3,0];
    p2 := m[i][1,2]+m[i][2,1];
    q  := m[i][0,1]+m[i][1,0]+m[i][0,2]+m[i][2,0]+m[i][1,3]+m[i][3,1]+m[i][2,3]+m[i][3,2];
    r  := p1+p2+q +m[i][0,0] +m[i][1,1] +m[i][2,2] +m[i][3,3];
    if r = 0 then
      continue;
    p1 := p1/r;
    p2 := p2/r;
    q  := q/r;
    if (pA*pG > 0) and (pT*pC > 0) then
      if ((pR/pA/pG/2*p1 +q/pR/2) < 1) and ((pY/pT/pC/2*p2 +q/pY/2) < 1) and (q/2/pR/pY < 1) then
      begin
        r := -(ln(1 -pR/pA/pG/2*p1 -q/pR/2) -pY*ln(1 -q/2/pR/pY))/pR/2;
        if r > 0 then
          k := k +r;
        r := -(ln(1 -pY/pT/pC/2*p2 -q/pY/2) -pR*ln(1 -q/2/pR/pY))/pY/2;
        if r > 0 then
          k := k +r;
        s  := s  -ln(1 -q/2/pR/pY);
      end;
  end;
  if (s > 0) then
    k := k/s
  else
    k := 1;

  for i := 0 to seqs.Count-1 do
    b[i] := ComputeDistance(m[i], true);

  L := 0;
  for i := 0 to seqs.Count-1 do
    L := L +CompositeLikelihood(m[i],b[i], true);

  if k > 0.000000000001 then
  begin
  it := 0;
  repeat
    dL0 := L;

    k0 := k;
    dk := k/1000;

    k := k +dk;
    dL1 := 0;
    for i := 0 to seqs.Count-1 do
      dL1 := dL1 +CompositeLikelihood(m[i],b[i], true);

    k := k -2*dk;
    dL2 := 0;
    for i := 0 to seqs.Count-1 do
      dL2 := dL2 +CompositeLikelihood(m[i],b[i], true);

    dL  := (dL1-dL2)/dk/2;
    ddL := ((dL1-2*dL0+dL2))/dk/dk;

    if abs(ddL) > 0 then
      if dL/ddL > k/2 then
        k := k/2
      else if dL/ddL < -k then
        k := k*2
      else
        k := k -dL/ddL;

    L := 0;
    for i := 0 to seqs.Count-1 do
      L := L +CompositeLikelihood(m[i],b[i], true);
    if L < dL0 then
    begin
      k := k0;
      L  := dL0;
    end;

    inc(it);
  until (abs(L-dL0) < deltaL) or (it = MaxIter);
  end;

  setlength(h,0);
  setlength(b,0);
  for i := 0 to seqs.Count-1 do
    DestroyMatrix(m[i], 4);
  setlength(m,0);
  for i := 0 to seqs.Count-1 do
    DestroyMatrix(p[i], 4);
  setlength(p,0);
end;

procedure THKYModel.ComputeProb(P: PMatrixOfExtended; b: extended);
var
  P1,P2,Q,r: extended;
begin
  r := 4*((pA*pG +pT*pC)*k +pR*pY);

  P1 := (pR +pY*exp(-2*b/r) -exp(-2*(pR*k +pY)*b/r))/pR;

  P2 := (pY +pR*exp(-2*b/r) -exp(-2*(pY*k +pR)*b/r))/pY;

  Q  := 1 -exp(-2*b/r);

  P[0,1] := pT*Q;
  P[0,2] := pC*Q;
  P[0,3] := pG*P1;
  P[0,0] := 1-P[0,1]-P[0,2]-P[0,3];

  P[1,0] := pA*Q;
  P[1,2] := pC*P2;
  P[1,3] := pG*Q;
  P[1,1] := 1-P[1,0]-P[1,2]-P[1,3];

  P[2,0] := pA*Q;
  P[2,1] := pT*P2;
  P[2,3] := pG*Q;
  P[2,2] := 1-P[2,0]-P[2,1]-P[2,3];

  P[3,0] := pA*P1;
  P[3,1] := pT*Q;
  P[3,2] := pC*Q;
  P[3,3] := 1-P[3,0]-P[3,1]-P[3,2];
end;

procedure THKYModel.ComputeProbWithGamma(P: PMatrixOfExtended; b: extended);
var
  P1,P2,Q,r: extended;
begin
  r := 4*((pA*pG +pT*pC)*k +pR*pY);

  P1 := (pR +pY*exp(FGamma*ln(FGamma/(FGamma +2*b/r))) -exp(FGamma*ln(FGamma/(FGamma +2*(pR*k +pY)*b/r))))/pR;

  P2 := (pY +pR*exp(FGamma*ln(FGamma/(FGamma +2*b/r))) -exp(FGamma*ln(FGamma/(FGamma +2*(pY*k +pR)*b/r))))/pY;

  Q  :=1 -exp(FGamma*ln(FGamma/(FGamma +2*b/r)));

  P[0,1] := pT*Q;
  P[0,2] := pC*Q;
  P[0,3] := pG*P1;
  P[0,0] := 1-P[0,1]-P[0,2]-P[0,3];

  P[1,0] := pA*Q;
  P[1,2] := pC*P2;
  P[1,3] := pG*Q;
  P[1,1] := 1-P[1,0]-P[1,2]-P[1,3];

  P[2,0] := pA*Q;
  P[2,1] := pT*P2;
  P[2,3] := pG*Q;
  P[2,2] := 1-P[2,0]-P[2,1]-P[2,3];

  P[3,0] := pA*P1;
  P[3,1] := pT*Q;
  P[3,2] := pC*Q;
  P[3,3] := 1-P[3,0]-P[3,1]-P[3,2];
end;

procedure THKYModel.ChangeProbOfNode(node: TMLTreeNode; b,k: extended);
var
  P1,P2,Q,r: extended;
var
  i: integer;
begin
  with node do
    for i := 0 to NoOfRates-1 do
      if FRate[i] < 0.000000000001 then
      begin
        Prob[i][0,1] := 0;
        Prob[i][0,2] := 0;
        Prob[i][0,3] := 0;
        Prob[i][0,0] := 1;

        Prob[i][1,0] := 0;
        Prob[i][1,2] := 0;
        Prob[i][1,3] := 0;
        Prob[i][1,1] := 1;

        Prob[i][2,0] := 0;
        Prob[i][2,1] := 0;
        Prob[i][2,3] := 0;
        Prob[i][2,2] := 1;

        Prob[i][3,0] := 0;
        Prob[i][3,1] := 0;
        Prob[i][3,2] := 0;
        Prob[i][3,3] := 1;
      end
      else
      begin
        r := 4*((pA*pG +pT*pC)*k +pR*pY)/(FRate[i]/(1-FInvar));

        P1 := (pR +pY*exp(-2*b/r) -exp(-2*(pR*k +pY)*b/r))/pR;

        P2 := (pY +pR*exp(-2*b/r) -exp(-2*(pY*k +pR)*b/r))/pY;

        Q  := 1 -exp(-2*b/r);

        Prob[i][0,1] := pT*Q;
        Prob[i][0,2] := pC*Q;
        Prob[i][0,3] := pG*P1;
        Prob[i][0,0] := 1-Prob[i][0,1]-Prob[i][0,2]-Prob[i][0,3];

        Prob[i][1,0] := pA*Q;
        Prob[i][1,2] := pC*P2;
        Prob[i][1,3] := pG*Q;
        Prob[i][1,1] := 1-Prob[i][1,0]-Prob[i][1,2]-Prob[i][1,3];

        Prob[i][2,0] := pA*Q;
        Prob[i][2,1] := pT*P2;
        Prob[i][2,3] := pG*Q;
        Prob[i][2,2] := 1-Prob[i][2,0]-Prob[i][2,1]-Prob[i][2,3];

        Prob[i][3,0] := pA*P1;
        Prob[i][3,1] := pT*Q;
        Prob[i][3,2] := pC*Q;
        Prob[i][3,3] := 1-Prob[i][3,0]-Prob[i][3,1]-Prob[i][3,2];
      end;
end;

procedure THKYModel.OptimizeParameters(tree: TMLTree; aCheckCancel: TCheckCancelFunc = nil);
var
  L0,dL0,dL1,dL2,dL,ddL,d,k0,dk,ek: extended;
  iter,i: integer;
begin
  d := k/1000;

  if k > 0.000000000001 then
  begin
    iter := 0;
    repeat
      L0  := tree.LogLikelihood;

      dL0 := tree.LogLikelihood;

      k0 := k;
      for i := 0 to  2*tree.NoOfSeqs-2 do
      begin
        if tree.Node[i] = tree.Root then
          continue;
        ChangeProbOfNode(tree.Node[i], tree.Node[i].blen, k+d);
      end;
      dL1 := tree.ResetL1;

      for i := 0 to  2*tree.NoOfSeqs-2 do
      begin
        if tree.Node[i] = tree.Root then
          continue;
        ChangeProbOfNode(tree.Node[i], tree.Node[i].blen, k-d);
      end;
      dL2 := tree.ResetL1;

      dL  := (dL1-dL2)/d/2;
      ddL := ((dL1-2*dL0+dL2))/d/d;

      if abs(ddL) > 0 then
      begin
        dk := dL/ddL;
        ek := sqrt(abs(1/ddL));
      end
      else
      begin
        dk := 0;
        ek := 0;
      end;

      if abs(dk) < 3*ek then
        if dk > k/2 then
          k := k/2
        else if dk < -k then
          k := k*2
        else
          k := k -dk;

      for i := 0 to  2*tree.NoOfSeqs-2 do
      begin
        if tree.Node[i] = tree.Root then
          continue;
        ChangeProbOfNode(tree.Node[i], tree.Node[i].blen, k);
      end;
      tree.LogLikelihood := tree.ResetL1;

      if tree.LogLikelihood < dL0 then
      begin
        k := k0;
        for i := 0 to  2*tree.NoOfSeqs-2 do
        begin
          if tree.Node[i] = tree.Root then
            continue;
          ChangeProbOfNode(tree.Node[i], tree.Node[i].blen, k);
        end;
        tree.LogLikelihood := tree.ResetL1;
      end
      else if (abs(dk) > 0) and (abs(dk)/2 < d) then
        d := abs(dk)/2;

      inc(iter);
    if Assigned(aCheckCancel) then
      aCheckCancel(Round(iter/MaxIter*100), 'Optimizing parameters');
    until (abs(tree.LogLikelihood-L0) < deltaL) or (iter = MaxIter);
    if Assigned(aCheckCancel) then
      aCheckCancel(100, 'Optimizing parameters');
  end;
  inherited;
end;

procedure THKYModel.GetParams(var kappa: extended);
begin
  kappa := k;
end;

function THKYModel.CompositeLikelihood(M: PMatrixOfExtended; b: extended; usegamma: boolean): extended;
var
  p: PMatrixOfExtended;
  r0,r1: extended;
begin
  result := 0;
  p := nil;
  CreateMatrix(p, NoOfStates);

  if usegamma and (Gamma > 0.001) then
    ComputeProbWithGamma(p, b/(1-FInvar))
  else
    ComputeProb(p, b/(1-FInvar));

  r0 := (Freq[0]*p[0,3]+Freq[3]*p[3,0]+Freq[1]*p[1,2]+Freq[2]*p[2,1])*(1-FInvar);
  r1 := m[0,3]+m[3,0]+m[1,2]+m[2,1];
  if (r0 > 0) and (r1 > 0) then
    result := result +r1*ln(r0);
  r0 := (Freq[0]*(p[0,1]+p[0,2])+Freq[1]*(p[1,0]+p[1,3])+Freq[2]*(p[2,0]+p[2,3])+Freq[3]*(p[3,1]+p[3,2]))*(1-FInvar);
  r1 := m[0,1]+m[1,0]+m[0,2]+m[2,0]+m[1,3]+m[3,1]+m[2,3]+m[3,2];
  if (r0 > 0) and (r1 > 0) then
    result := result +r1*ln(r0);

  r0 := (Freq[0]*p[0,0]+Freq[1]*p[1,1]+Freq[2]*p[2,2]+Freq[3]*p[3,3])*(1-FInvar) +FInvar;
  r1 := m[0,0]+m[1,1]+m[2,2]+m[3,3];
  if (r0 > 0) and (r1 > 0) then
    result := result +r1*ln(r0);

  DestroyMatrix(p, NoOfStates);
end;

function THKYModel.PatternBias: Extended;
begin
  result := ((0.25-pA)*(0.25-pA) +(0.25-pG)*(0.25-pG) +(0.25-pT)*(0.25-pT) +(0.25-pC)*(0.25-pC))*k;
end;

procedure THKYModel.GetInfo(ModelInfo: TModelInfo);
var
  i: integer;
begin
  if ModelInfo = nil then exit;

  inherited GetInfo(ModelInfo);

  for i := 0 to 3 do
    ModelInfo.Freq[i] := Freq[i];

  ModelInfo.DataType  := 'DNA';
  ModelInfo.ModelName := 'HKY' +ModelInfo.ModelName;
  ModelInfo.FullName  := 'Hasegawa-Kishino-Yano (1985) model'+ModelInfo.FullName;
  ModelInfo.NoOfParams  := 4;
  ModelInfo.NoOfSamples := TotalNoOfSites;
  ModelInfo.Matrix[0,1] := pT;
  ModelInfo.Matrix[0,2] := pC;
  ModelInfo.Matrix[0,3] := pG*k;
  ModelInfo.Matrix[1,0] := pA;
  ModelInfo.Matrix[1,2] := pC*k;
  ModelInfo.Matrix[1,3] := pG;
  ModelInfo.Matrix[2,0] := pA;
  ModelInfo.Matrix[2,1] := pT*k;
  ModelInfo.Matrix[2,3] := pG;
  ModelInfo.Matrix[3,0] := pA*k;
  ModelInfo.Matrix[3,1] := pT;
  ModelInfo.Matrix[3,2] := pC;
  ModelInfo.SVR         := (pA*pG +pT*pC)*k/(pA*pT +pA*pC +pT*pG +pC*pG);
end;

function THKYModel.GetName: String;
begin
  Result := 'HKY';
end;

function THKYModel.ReadFromFile(var data: File):boolean;
var
  version: integer;
begin
  version := -1;
  result := false;
  try
    if not inherited ReadFromFile(data) then
      exit;

    BlockRead(data, version, sizeof(integer));

    BlockRead(data, k, sizeof(extended));
    pA := Freq[0];
    pT := Freq[1];
    pC := Freq[2];
    pG := Freq[3];
    pR := pA+pG;
    pY := pT+pC;

    result := true;
  except
    result := false;
  end;
end;

procedure THKYModel.WriteToFile(var data: File);
var
  version: integer;
begin
  inherited WriteToFile(data);

  version := 1;

  BlockWrite(data, version, sizeof(integer));

  BlockWrite(data, k, sizeof(extended));
end;

constructor TT3Model.create(gamma: extended; inv: boolean; ncat: integer);
begin
  inherited create(gamma, inv, ncat);
end;

procedure TT3Model.SetFreqs(fGC: extended);
begin
  pG := fGC/2;
  pC := pG;
  pA := (1-fGC)/2;
  pT := pA;
  pR := 0.5;
  pY := 0.5;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;
end;

procedure TT3Model.SetParams(kappa: extended);
begin
  k := kappa;
end;

{
procedure TT3Model.SetFreqsFromSeqs(seqs: TStringList);
var
  pGC: extended;
begin
  inherited;

  pGC := pC+pG;
  pG := pGC/2;
  pC := pG;
  pA := (1-pGC)/2;
  pT := pA;
  pR := 0.5;
  pY := 0.5;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;
end;
}
procedure TT3Model.SetParamsFromSeqs(seqs: TStringList);
var
  pGC: extended;
begin
  inherited;

  pGC := pC+pG;
  pG := pGC/2;
  pC := pG;
  pA := (1-pGC)/2;
  pT := pA;
  pR := 0.5;
  pY := 0.5;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;
end;

procedure TT3Model.GetInfo(ModelInfo: TModelInfo);
begin
  if ModelInfo = nil then exit;

  inherited GetInfo(ModelInfo);

  ModelInfo.NoOfParams := 2;
  
  Delete(ModelInfo.ModelName, 1, 3);
  ModelInfo.ModelName := 'T92' +ModelInfo.ModelName;
  Delete(ModelInfo.FullName, 1, 28);  //NRP 6/29/09 changed ModelInfo.ModelName to ModelInfo.FullName.  ModelInfo is already constructed at this point, but FullName still needs to be modified
  ModelInfo.FullName  := 'Tamura (1992)' +ModelInfo.FullName;
end;

function TT3Model.GetName: String;
begin
  Result := 'T3';
end;

constructor TK2Model.create(gamma: extended; inv: boolean; ncat: integer);
begin
  inherited create(gamma, inv, ncat);
end;

procedure TK2Model.SetParams(kappa: extended);
begin
  k := kappa;
end;

procedure TK2Model.SetParamsFromSeqs(seqs: TStringList);
begin
  inherited;

  pG := 0.25;
  pC := 0.25;
  pA := 0.25;
  pT := 0.25;
  pR := 0.5;
  pY := 0.5;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;
end;

procedure TK2Model.GetInfo(ModelInfo: TModelInfo);
begin
  if ModelInfo = nil then exit;

  inherited GetInfo(ModelInfo);

  ModelInfo.NoOfParams := 1;
  
  Delete(ModelInfo.ModelName, 1, 3);
  ModelInfo.ModelName := 'K2' +ModelInfo.ModelName;
  Delete(ModelInfo.FullName, 1, 28);  //NRP 6/29/09 changed ModelInfo.ModelName to ModelInfo.FullName.  ModelInfo is already constructed at this point, but FullName still needs to be modified
  ModelInfo.FullName  := 'Kimura (1980) 2-parameter' +ModelInfo.FullName;
end;

function TK2Model.GetName: String;
begin
  Result := 'K2';
end;

constructor TJCModel.create(gamma: extended; inv: boolean; ncat: integer);
begin
  inherited create(gamma, inv, ncat);
end;

procedure TJCModel.SetParamsFromSeqs(seqs: TStringList);
begin
  inherited;

  k := 1;
  pG := 0.25;
  pC := 0.25;
  pA := 0.25;
  pT := 0.25;
  pR := 0.5;
  pY := 0.5;
  FFreq[0] := pA;
  FFreq[1] := pT;
  FFreq[2] := pC;
  FFreq[3] := pG;
end;

procedure TJCModel.OptimizeParameters(tree: TMLTree; aCheckCancel: TCheckCancelFunc = nil);
var
  L0: extended;
  iter: integer;
begin
  iter := 0;
  repeat
    L0  := tree.LogLikelihood;
    IterateGammaParameter(tree);
    IterateInvarParameter(tree);
    inc(iter);
    if Assigned(aCheckCancel) then
      aCheckCancel(Round(iter/MaxIter*100), 'Optimizing parameters');
  until (abs(tree.LogLikelihood-L0) < deltaL) or (iter = MaxIter);
  if Assigned(aCheckCancel) then
    aCheckCancel(100, 'Optimizing parameters');
end;

function TJCModel.CompositeLikelihood(M: PMatrixOfExtended; b: extended; usegamma: boolean): extended;
var
  p: PMatrixOfExtended;
  r0,r1: extended;
begin
  p := nil;
  result := 0;

  CreateMatrix(p, NoOfStates);

  if usegamma and (Gamma > 0.001) then
    ComputeProbWithGamma(p, b/(1-FInvar))
  else
    ComputeProb(p, b/(1-FInvar));

  r0 := (Freq[0]*p[0,0]+Freq[1]*p[1,1]+Freq[2]*p[2,2]+Freq[3]*p[3,3])*(1-FInvar) +FInvar;
  r1 := m[0,0]+m[1,1]+m[2,2]+m[3,3];
  if (r0 > 0) and (r1 > 0) then
    result := result +r1*ln(r0);
  r0 := (Freq[0]*(p[0,1]+p[0,2]+p[0,3])+Freq[1]*(p[1,0]+p[1,2]+p[1,3])+Freq[2]*(p[2,0]+p[2,1]+p[2,3])+Freq[3]*(p[3,0]+p[3,1]+p[3,2]))*(1-FInvar);
  r1 := m[0,1]+m[0,2]+m[0,3]+m[1,0]+m[1,2]+m[1,3]+m[2,0]+m[2,1]+m[2,3]+m[3,0]+m[3,1]+m[3,2];
  if (r0 > 0) and (r1 > 0) then
    result := result +r1*ln(r0);

  DestroyMatrix(p, NoOfStates);
end;

procedure TJCModel.GetInfo(ModelInfo: TModelInfo);
begin
  if ModelInfo = nil then exit;

  inherited GetInfo(ModelInfo);

  ModelInfo.NoOfParams := 0; // Added by Dr.Tamura to fix bug #0002658
  
  Delete(ModelInfo.ModelName, 1, 3);
  ModelInfo.ModelName := 'JC' +ModelInfo.ModelName;
  Delete(ModelInfo.FullName, 1, 28);  //NRP 6/29/09 changed ModelInfo.ModelName to ModelInfo.FullName.  ModelInfo is already constructed at this point, but FullName still needs to be modified
  ModelInfo.FullName  := 'Jukes-Cantor (1969)' +ModelInfo.FullName;
end;

function TJCModel.GetName: String;
begin
  Result := 'JC';
end;

///////////////////////
// Protein models
///////////////////////

constructor TProteinMatrixModel.Create(gamma: extended; inv: boolean; freq: boolean; ncat: integer);
begin
  inherited Create(gamma, inv, ncat);

  FNoOfStates := 20;
  FSeqDataType := Protein;
  FUseFreq := freq;

  setlength(FFreq, 20);

  GetMem(D, SizeOf(Extended)*20);
  GetMem(V, SizeOf(Extended)*20);
  CreateMatrix(A, 20);
  CreateMatrix(Ai, 20);
  CreateMatrix(AV, 20);
end;

destructor TProteinMatrixModel.Destroy;
begin
  setlength(FFreq, 0);

  FreeMemAndNil(D);
  FreeMemAndNil(V);
  DestroyMatrix(A, 20);
  DestroyMatrix(Ai, 20);
  DestroyMatrix(AV, 20);

  inherited;
end;

procedure  TProteinMatrixModel.Assign(source: TGammaRateVariationModel);
var
  i,j: integer;
begin
  inherited Assign(source);

  if not(source is TProteinMatrixModel) then
    exit;

  FUseFreq := TProteinMatrixModel(source).FUseFreq;
  for i := 0 to 19 do
  begin
    FFreq[i] := TProteinMatrixModel(source).FFreq[i];
    D[i]     := TProteinMatrixModel(source).D[i];
    V[i]     := TProteinMatrixModel(source).V[i];
  end;
  for i := 0 to 19 do
    for j := 0 to 19 do
    begin
      A[i,j]  := TProteinMatrixModel(source).A[i,j];
      Ai[i,j] := TProteinMatrixModel(source).Ai[i,j];
      Av[i,j] := TProteinMatrixModel(source).Av[i,j];
    end;
end;

procedure TProteinMatrixModel.ComputeEigenVector;
var
  Q: PMatrixOfExtended;
  Lambda: PArrayOfComplex;
  i: integer;
begin
  Q := nil;
  GetMem(Lambda, SizeOf(TComplex)*20);

  CreateMatrix(Q, 20);
  GetMatrix(Q);

  EigenVect(Q, 0, 19, Lambda, A);

  for i := 0 to 19 do
    D[i] := Lambda[i].X;

  InvMat(A, 0, 19, Ai);

  DestroyMatrix(Q, 20);

  FreeMemAndNil(Lambda);
end;

procedure TProteinMatrixModel.SetDefaultFreq;
begin
  FFreq[0]  := DefaultFreq[0];
  FFreq[1]  := DefaultFreq[1];
  FFreq[2]  := DefaultFreq[2];
  FFreq[3]  := DefaultFreq[3];
  FFreq[4]  := DefaultFreq[4];
  FFreq[5]  := DefaultFreq[5];
  FFreq[6]  := DefaultFreq[6];
  FFreq[7]  := DefaultFreq[7];
  FFreq[8]  := DefaultFreq[8];
  FFreq[9]  := DefaultFreq[9];
  FFreq[10] := DefaultFreq[10];
  FFreq[11] := DefaultFreq[11];
  FFreq[12] := DefaultFreq[12];
  FFreq[13] := DefaultFreq[13];
  FFreq[14] := DefaultFreq[14];
  FFreq[15] := DefaultFreq[15];
  FFreq[16] := DefaultFreq[16];
  FFreq[17] := DefaultFreq[17];
  FFreq[18] := DefaultFreq[18];
  FFreq[19] := DefaultFreq[19];
end;

procedure TProteinMatrixModel.SetParamsFromSeqs(seqs: TStringList);
var
  f: array [0..19] of integer;
  i,j,n,d: integer;
  flag: boolean;
  s: extended;
begin
  inherited;

  for i := 0 to 19 do
    f[i] := 0;

  n := length(seqs[0]);
  for j := 1 to n do
  begin
    if FBootTable = nil then
      d := 1
    else if FBootTable[j] = 0 then
      continue
    else
      d := FBootTable[j];

    for i := 0 to seqs.Count-1 do
      case upcase(seqs[i][j]) of
        'A': inc(f[0], d);
        'R': inc(f[1], d);
        'N': inc(f[2], d);
        'D': inc(f[3], d);
        'C': inc(f[4], d);
        'Q': inc(f[5], d);
        'E': inc(f[6], d);
        'G': inc(f[7], d);
        'H': inc(f[8], d);
        'I': inc(f[9], d);
        'L': inc(f[10], d);
        'K': inc(f[11], d);
        'M': inc(f[12], d);
        'F': inc(f[13], d);
        'P': inc(f[14], d);
        'S': inc(f[15], d);
        'T': inc(f[16], d);
        'W': inc(f[17], d);
        'Y': inc(f[18], d);
        'V': inc(f[19], d);
      end;
  end;

  if UseFreq then
  begin
    flag := false;
    for i := 0 to 19 do
    begin
      FFreq[i] := f[i]/TotalNoOfSites;
      if f[i] = 0 then
      begin
        FFreq[i] := 1/TotalNoOfSites;
        flag := true;
      end;
    end;
    if flag then
    begin
      s := 0;
      for i := 0 to 19 do
        s := s +FFreq[i];
      for i := 0 to 19 do
        FFreq[i] := FFreq[i]/s;
    end;
  end;

  ComputeEigenVector;
end;

procedure TProteinMatrixModel.SetFreqs(f: array of extended);
var
  i: integer;
begin
  for i := 0 to 19 do
    FFreq[i] := f[i];

  ComputeEigenVector;
end;

procedure TProteinMatrixModel.ComputeProb(P: PMatrixOfExtended; b: extended);
var
  i,j,k: integer;
begin
  for i := 0 to 19 do
    V[i] := exp(D[i]*b);

  for i := 0 to 19 do
    for j := 0 to 19 do
      AV[i,j] := A[i,j]*V[j];

  for i := 0 to 19 do
    for j := 0 to 19 do
    begin
      P[i,j] := 0;
      for k := 0 to 19 do
        P[i,j] := P[i,j] +AV[i,k]*Ai[k,j];
      if P[i,j] < 0 then
        P[i,j] := 0;
    end;

  for i := 0 to 19 do
  begin
    P[i,i] := 1;
    for j := 0 to 19 do
      if j <> i then
        P[i,i] := P[i,i] -P[i,j];
  end;

end;

procedure TProteinMatrixModel.ComputeProbWithGamma(P: PMatrixOfExtended; b: extended);
var
  i,j,k: integer;
begin
  for i := 0 to 19 do
    V[i] := exp(-FGamma*ln(1 -D[i]*b/FGamma));

  for i := 0 to 19 do
    for j := 0 to 19 do
      AV[i,j] := A[i,j]*V[j];

  for i := 0 to 19 do
    for j := 0 to 19 do
    begin
      P[i,j] := 0;
      for k := 0 to 19 do
        P[i,j] := P[i,j] +AV[i,k]*Ai[k,j];
      if P[i,j] < 0 then
        P[i,j] := 0;
    end;

  for i := 0 to 19 do
  begin
    P[i,i] := 1;
    for j := 0 to 19 do
      if j <> i then
        P[i,i] := P[i,i] -P[i,j];
  end;
end;

procedure TProteinMatrixModel.ResetProb(tree: TMLTree);
var
  i,j: integer;
begin
  for i := 0 to  2*tree.NoOfSeqs-2 do
  begin
    if tree.Node[i] = tree.Root then
      continue;
    for j := 0 to NoOfRates-1 do
      ComputeProb(tree.Node[i].Prob[j], tree.Node[i].blen*FRate[j]/(1-FInvar));
  end;
end;

procedure TProteinMatrixModel.GetInfo(ModelInfo: TModelInfo);
var
  M: PMatrixOfExtended;
  i,j: integer;
begin
  M := nil;
  inherited GetInfo(ModelInfo);

  // IMPORTANT: Since Freq is added in some protein models we need to write the names with +F when applicable.  The inherited function knows nothign of Freq so we must do it here.
  if UseFreq then
  begin
    if NoOfRates > 1 then
      if UseInvar then
      begin
        ModelInfo.ModelName := '+G+I+F';
        ModelInfo.FullName  := ' (+Gamma +Invar +Freq)'
      end
      else
      begin
        ModelInfo.ModelName := '+G+F';
        ModelInfo.FullName  := ' (+Gamma +Freq)'
      end
    else if UseInvar then
    begin
      ModelInfo.ModelName := '+I+F';
      ModelInfo.FullName  := ' (+Invar +Freq)'
    end
    else
    begin
      ModelInfo.ModelName := '+F';
      ModelInfo.FullName  := ' (+Freq)'; 
    end;
  end;
  ModelInfo.DataType    := 'Protein';
  ModelInfo.NoOfSamples := TotalNoOfSites;

  for i := 0 to 19 do
    ModelInfo.Freq[i] := Freq[i];

  CreateMatrix(M,20);
  GetMatrix(M);
  for i := 0 to 19 do
    for j := 0 to 19 do
      ModelInfo.Matrix[i,j] := M[i,j];
  DestroyMatrix(M,20);
end;

function TProteinMatrixModel.ReadFromFile(var data: File):boolean;
var
  i,version: integer;
begin
  version := -1;
  result := false;
  try
    if not inherited ReadFromFile(data) then
      exit;

    BlockRead(data, version, sizeof(integer));

    BlockRead(data, FUseFreq, sizeof(boolean));

    for i := 0 to 19 do
      BlockRead(data, DefaultFreq[i], sizeof(extended));

    ComputeEigenVector;

    result := true;
  except
    result := false;
  end;
end;


procedure TProteinMatrixModel.WriteToFile(var data: File);
var
  i,version: integer;
begin
  inherited WriteToFile(data);

  version := 1;

  BlockWrite(data, version, sizeof(integer));

  BlockWrite(data, FUseFreq, sizeof(boolean));

  for i := 0 to 19 do
    BlockWrite(data, DefaultFreq[i], sizeof(extended));
end;

constructor TDayhoffModel.Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
begin
  inherited Create(gamma, inv, freq, ncat);

  DefaultFreq[0]  := 0.0871;  // 0.087
  DefaultFreq[1]  := 0.0409;  // 0.041
  DefaultFreq[2]  := 0.0404;  // 0.040
  DefaultFreq[3]  := 0.0469;  // 0.047
  DefaultFreq[4]  := 0.0335;  // 0.033
  DefaultFreq[5]  := 0.0383;  // 0.038
  DefaultFreq[6]  := 0.0495;  // 0.050
  DefaultFreq[7]  := 0.0886;  // 0.089
  DefaultFreq[8]  := 0.0336;  // 0.034
  DefaultFreq[9]  := 0.0369;  // 0.037
  DefaultFreq[10] := 0.0854;  // 0.085
  DefaultFreq[11] := 0.0805;  // 0.081
  DefaultFreq[12] := 0.0148;  // 0.015
  DefaultFreq[13] := 0.0398;  // 0.040
  DefaultFreq[14] := 0.0507;  // 0.051
  DefaultFreq[15] := 0.0696;  // 0.070
  DefaultFreq[16] := 0.0585;  // 0.058
  DefaultFreq[17] := 0.0105;  // 0.010
  DefaultFreq[18] := 0.0299;  // 0.030
  DefaultFreq[19] := 0.0647;  // 0.065

  SetDefaultFreq;

  ComputeEigenVector;
end;

procedure TDayhoffModel.GetMatrix(M: PMatrixOfExtended);
var
  i,j: integer;
  s: extended;
begin
  s := 0;
  for i := 0 to 19 do
    for j := 0 to 19 do
      s := s +DayhoffMatrix[i,j]/DefaultFreq[i]/DefaultFreq[j]*FFreq[i]*Freq[j];

  for i := 0 to 19 do
    for j := 0 to 19 do
      M[i,j] := DayhoffMatrix[i,j]/DefaultFreq[i]/DefaultFreq[j]*FFreq[j]/s;

  for i := 0 to 19 do
  begin
    M[i,i] := 0;
    for j := 0 to 19 do
      if j <> i then
        M[i,i] := M[i,i] -M[i,j];
  end;
end;

function TDayhoffModel.GetName: String;
begin
  Result := 'Dayhoff';
end;

procedure TDayhoffModel.GetInfo(ModelInfo: TModelInfo);
begin
  inherited GetInfo(ModelInfo);

  ModelInfo.ModelName := 'Dayhoff' +ModelInfo.ModelName;
  ModelInfo.FullName  := 'Dayhoff (1978) model'+ModelInfo.FullName;
end;

constructor TJTTModel.Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
begin
  inherited Create(gamma, inv, freq, ncat);

  DefaultFreq[0]  := 0.0769;  //  0.077
  DefaultFreq[1]  := 0.0511;  //  0.051
  DefaultFreq[2]  := 0.0425;  //  0.043
  DefaultFreq[3]  := 0.0513;  //  0.052
  DefaultFreq[4]  := 0.0203;  //  0.020
  DefaultFreq[5]  := 0.0411;  //  0.041
  DefaultFreq[6]  := 0.0618;  //  0.062
  DefaultFreq[7]  := 0.0747;  //  0.074
  DefaultFreq[8]  := 0.0230;  //  0.023
  DefaultFreq[9]  := 0.0526;  //  0.053
  DefaultFreq[10] := 0.0911;  //  0.091
  DefaultFreq[11] := 0.0595;  //  0.059
  DefaultFreq[12] := 0.0234;  //  0.024
  DefaultFreq[13] := 0.0405;  //  0.040
  DefaultFreq[14] := 0.0505;  //  0.051
  DefaultFreq[15] := 0.0682;  //  0.069
  DefaultFreq[16] := 0.0585;  //  0.059
  DefaultFreq[17] := 0.0143;  //  0.014
  DefaultFreq[18] := 0.0323;  //  0.032
  DefaultFreq[19] := 0.0664;  //  0.066

  SetDefaultFreq;

  ComputeEigenVector;
end;

procedure TJTTModel.GetMatrix(M: PMatrixOfExtended);
var
  i,j: integer;
  s: extended;
begin
  s := 0;
  for i := 0 to 19 do
    for j := 0 to 19 do
      s := s +JTTMatrix[i,j]/DefaultFreq[i]/DefaultFreq[j]*FFreq[i]*Freq[j];

  for i := 0 to 19 do
    for j := 0 to 19 do
      M[i,j] := JTTMatrix[i,j]/DefaultFreq[i]/DefaultFreq[j]*FFreq[j]/s;

  for i := 0 to 19 do
  begin
    M[i,i] := 0;
    for j := 0 to 19 do
      if j <> i then
        M[i,i] := M[i,i] -M[i,j];
  end;
end;

function TJTTModel.GetName: String;
begin
  Result := 'JTT';
end;

procedure TJTTModel.GetInfo(ModelInfo: TModelInfo);
begin
  inherited GetInfo(ModelInfo);

  ModelInfo.ModelName := 'JTT' +ModelInfo.ModelName;
  ModelInfo.FullName  := 'Jones-Taylor-Thornton (1992) model'+ModelInfo.FullName;
end;


constructor TmtREV24Model.Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
begin
  inherited Create(gamma, inv, freq, ncat);

  DefaultFreq[0]  := 0.072;
  DefaultFreq[1]  := 0.019;
  DefaultFreq[2]  := 0.039;
  DefaultFreq[3]  := 0.019;
  DefaultFreq[4]  := 0.006;
  DefaultFreq[5]  := 0.025;
  DefaultFreq[6]  := 0.024;
  DefaultFreq[7]  := 0.056;
  DefaultFreq[8]  := 0.028;
  DefaultFreq[9]  := 0.087;
  DefaultFreq[10] := 0.168;
  DefaultFreq[11] := 0.023;
  DefaultFreq[12] := 0.053;
  DefaultFreq[13] := 0.060;
  DefaultFreq[14] := 0.055;
  DefaultFreq[15] := 0.072;
  DefaultFreq[16] := 0.088;
  DefaultFreq[17] := 0.029;
  DefaultFreq[18] := 0.033;
  DefaultFreq[19] := 0.044;

  SetDefaultFreq;

  ComputeEigenVector;
end;

procedure TmtREV24Model.GetMatrix(M: PMatrixOfExtended);
var
  i,j: integer;
  s: extended;
begin
  s := 0;
  for i := 0 to 19 do
    for j := 0 to 19 do
      s := s +mtREV24Matrix[i,j]*FFreq[i]*FFreq[j];

  for i := 0 to 19 do
    for j := 0 to 19 do
      M[i,j] := mtREV24Matrix[i,j]*FFreq[j]/s;

  for i := 0 to 19 do
  begin
    M[i,i] := 0;
    for j := 0 to 19 do
      if j <> i then
        M[i,i] := M[i,i] -M[i,j];
  end;
end;

function TmtREV24Model.GetName: String;
begin
  Result := 'mtREV24';
end;

procedure TmtREV24Model.GetInfo(ModelInfo: TModelInfo);
begin
  inherited GetInfo(ModelInfo);

  ModelInfo.ModelName := 'mtREV24' +ModelInfo.ModelName;
  ModelInfo.FullName  := 'Adachi-Hasegawa (1996) mitochondrial DNA model'+ModelInfo.FullName;
end;


constructor TcpREVModel.Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
begin
  inherited Create(gamma, inv, freq, ncat);

  DefaultFreq[0]  := 0.0756;
  DefaultFreq[1]  := 0.0621;
  DefaultFreq[2]  := 0.0410;
  DefaultFreq[3]  := 0.0371;
  DefaultFreq[4]  := 0.0091;
  DefaultFreq[5]  := 0.0382;
  DefaultFreq[6]  := 0.0495;
  DefaultFreq[7]  := 0.0838;
  DefaultFreq[8]  := 0.0246;
  DefaultFreq[9]  := 0.0806;
  DefaultFreq[10] := 0.1011;
  DefaultFreq[11] := 0.0504;
  DefaultFreq[12] := 0.0220;
  DefaultFreq[13] := 0.0506;
  DefaultFreq[14] := 0.0431;
  DefaultFreq[15] := 0.0622;
  DefaultFreq[16] := 0.0543;
  DefaultFreq[17] := 0.0181;
  DefaultFreq[18] := 0.0307;
  DefaultFreq[19] := 0.0660;

  SetDefaultFreq;

  ComputeEigenVector;
end;

procedure TcpREVModel.GetMatrix(M: PMatrixOfExtended);
var
  i,j: integer;
  s: extended;
begin
  s := 0;
  for i := 0 to 19 do
    for j := 0 to 19 do
      s := s +cpREVMatrix[i,j]/DefaultFreq[j]*FFreq[i]*FFreq[j];

  for i := 0 to 19 do
    for j := 0 to 19 do
      M[i,j] := cpREVMatrix[i,j]/DefaultFreq[j]*FFreq[j]/s;

  for i := 0 to 19 do
  begin
    M[i,i] := 0;
    for j := 0 to 19 do
      if j <> i then
        M[i,i] := M[i,i] -M[i,j];
  end;
end;

function TcpREVModel.GetName: String;
begin
  Result := 'cpREV';
end;

procedure TcpREVModel.GetInfo(ModelInfo: TModelInfo);
begin
  inherited GetInfo(ModelInfo);

  ModelInfo.ModelName := 'cpREV' +ModelInfo.ModelName;
  ModelInfo.FullName  := 'Adachi et al . (2000) chloroplast DNA model'+ModelInfo.FullName;
end;

constructor TrtREVModel.Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
begin
  inherited Create(gamma, inv, freq, ncat);

  DefaultFreq[0]  := 0.0646;
  DefaultFreq[1]  := 0.0453;
  DefaultFreq[2]  := 0.0376;
  DefaultFreq[3]  := 0.0422;
  DefaultFreq[4]  := 0.0114;
  DefaultFreq[5]  := 0.0606;
  DefaultFreq[6]  := 0.0607;
  DefaultFreq[7]  := 0.0639;
  DefaultFreq[8]  := 0.0273;
  DefaultFreq[9]  := 0.0679;
  DefaultFreq[10] := 0.1018;
  DefaultFreq[11] := 0.0751;
  DefaultFreq[12] := 0.0150;
  DefaultFreq[13] := 0.0287;
  DefaultFreq[14] := 0.0681;
  DefaultFreq[15] := 0.0488;
  DefaultFreq[16] := 0.0622;
  DefaultFreq[17] := 0.0251;
  DefaultFreq[18] := 0.0318;
  DefaultFreq[19] := 0.0619;

  SetDefaultFreq;

  ComputeEigenVector;
end;

procedure TrtREVModel.GetMatrix(M: PMatrixOfExtended);
var
  i,j: integer;
  s: extended;
begin
  s := 0;
  for i := 0 to 19 do
    for j := 0 to 19 do
      s := s +rtREVMatrix[i,j]*FFreq[i]*FFreq[j];

  for i := 0 to 19 do
    for j := 0 to 19 do
      M[i,j] := rtREVMatrix[i,j]*FFreq[j]/s;

  for i := 0 to 19 do
  begin
    M[i,i] := 0;
    for j := 0 to 19 do
      if j <> i then
        M[i,i] := M[i,i] -M[i,j];
  end;
end;

function TrtREVModel.GetName: String;
begin
  Result := 'rtREV';
end;

procedure TrtREVModel.GetInfo(ModelInfo: TModelInfo);
begin
  inherited GetInfo(ModelInfo);

  ModelInfo.ModelName := 'rtREV' +ModelInfo.ModelName;
  ModelInfo.FullName  := 'Dimmic et al . (2002) reverse transcriptase model'+ModelInfo.FullName;
end;

constructor TWAGModel.Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
begin
  inherited Create(gamma, inv, freq, ncat);

  DefaultFreq[0]  := 0.0866279;
  DefaultFreq[1]  := 0.0439720;
  DefaultFreq[2]  := 0.0390894;
  DefaultFreq[3]  := 0.0570451;
  DefaultFreq[4]  := 0.0193078;
  DefaultFreq[5]  := 0.0367281;
  DefaultFreq[6]  := 0.0580589;
  DefaultFreq[7]  := 0.0832518;
  DefaultFreq[8]  := 0.0244313;
  DefaultFreq[9]  := 0.0484660;
  DefaultFreq[10] := 0.0862090;
  DefaultFreq[11] := 0.0620286;
  DefaultFreq[12] := 0.0195027;
  DefaultFreq[13] := 0.0384319;
  DefaultFreq[14] := 0.0457631;
  DefaultFreq[15] := 0.0695179;
  DefaultFreq[16] := 0.0610127;
  DefaultFreq[17] := 0.0143859;
  DefaultFreq[18] := 0.0352742;
  DefaultFreq[19] := 0.0708956;

  SetDefaultFreq;

  ComputeEigenVector;
end;

procedure TWAGModel.GetMatrix(M: PMatrixOfExtended);
var
  i,j: integer;
  s: extended;
begin
  s := 0;
  for i := 0 to 19 do
    for j := 0 to 19 do
      s := s +WAGMatrix[i,j]*FFreq[i]*FFreq[j];

  for i := 0 to 19 do
    for j := 0 to 19 do
      M[i,j] := WAGMatrix[i,j]*FFreq[j]/s;

  for i := 0 to 19 do
  begin
    M[i,i] := 0;
    for j := 0 to 19 do
      if j <> i then
        M[i,i] := M[i,i] -M[i,j];
  end;
end;

function TWAGModel.GetName: String;
begin
  Result := 'WAG';
end;

procedure TWAGModel.GetInfo(ModelInfo: TModelInfo);
begin
  inherited GetInfo(ModelInfo);

  ModelInfo.ModelName := 'WAG' +ModelInfo.ModelName;
  ModelInfo.FullName  := 'Whelan-Goldman (2001) WAG model'+ModelInfo.FullName;
end;

constructor TLGModel.Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
begin
  inherited Create(gamma, inv, freq, ncat);

  DefaultFreq[0]  := 0.079066;
  DefaultFreq[1]  := 0.055941;
  DefaultFreq[2]  := 0.041977;
  DefaultFreq[3]  := 0.053052;
  DefaultFreq[4]  := 0.012937;
  DefaultFreq[5]  := 0.040767;
  DefaultFreq[6]  := 0.071586;
  DefaultFreq[7]  := 0.057337;
  DefaultFreq[8]  := 0.022355;
  DefaultFreq[9]  := 0.062157;
  DefaultFreq[10] := 0.099081;
  DefaultFreq[11] := 0.064600;
  DefaultFreq[12] := 0.022951;
  DefaultFreq[13] := 0.042302;
  DefaultFreq[14] := 0.044040;
  DefaultFreq[15] := 0.061197;
  DefaultFreq[16] := 0.053287;
  DefaultFreq[17] := 0.012066;
  DefaultFreq[18] := 0.034155;
  DefaultFreq[19] := 0.069147;

  SetDefaultFreq;

  ComputeEigenVector;
end;

procedure TLGModel.GetMatrix(M: PMatrixOfExtended);
var
  i,j: integer;
  s: extended;
begin
  s := 0;
  for i := 0 to 19 do
    for j := 0 to 19 do
      s := s +LGMatrix[i,j]*FFreq[i]*FFreq[j];

  for i := 0 to 19 do
    for j := 0 to 19 do
      M[i,j] := LGMatrix[i,j]*FFreq[j]/s;

  for i := 0 to 19 do
  begin
    M[i,i] := 0;
    for j := 0 to 19 do
      if j <> i then
        M[i,i] := M[i,i] -M[i,j];
  end;
end;

function TLGModel.GetName: String;
begin
  Result := 'LG';
end;

procedure TLGModel.GetInfo(ModelInfo: TModelInfo);
begin
  inherited GetInfo(ModelInfo);

  ModelInfo.ModelName := 'LG' +ModelInfo.ModelName;
  ModelInfo.FullName  := 'Le-Gascuel (2008) LG model'+ModelInfo.FullName;
end;

constructor TPoissonModel.Create(gamma: extended; inv: boolean; freq: boolean;  ncat: integer);
begin
  inherited Create(gamma, inv, freq, ncat);

  DefaultFreq[0]  := 0.05;
  DefaultFreq[1]  := 0.05;
  DefaultFreq[2]  := 0.05;
  DefaultFreq[3]  := 0.05;
  DefaultFreq[4]  := 0.05;
  DefaultFreq[5]  := 0.05;
  DefaultFreq[6]  := 0.05;
  DefaultFreq[7]  := 0.05;
  DefaultFreq[8]  := 0.05;
  DefaultFreq[9]  := 0.05;
  DefaultFreq[10] := 0.05;
  DefaultFreq[11] := 0.05;
  DefaultFreq[12] := 0.05;
  DefaultFreq[13] := 0.05;
  DefaultFreq[14] := 0.05;
  DefaultFreq[15] := 0.05;
  DefaultFreq[16] := 0.05;
  DefaultFreq[17] := 0.05;
  DefaultFreq[18] := 0.05;
  DefaultFreq[19] := 0.05;

  SetDefaultFreq;

  ComputeEigenVector;
end;

procedure TPoissonModel.GetMatrix(M: PMatrixOfExtended);
var
  i,j: integer;
  s: extended;
begin
  s := 0;
  for i := 0 to 19 do
    for j := 0 to 19 do
      s := s +PoissonMatrix[i,j]*FFreq[i]*FFreq[j];

  for i := 0 to 19 do
    for j := 0 to 19 do
      M[i,j] := PoissonMatrix[i,j]*FFreq[j]/s;

  for i := 0 to 19 do
  begin
    M[i,i] := 0;
    for j := 0 to 19 do
      if j <> i then
        M[i,i] := M[i,i] -M[i,j];
  end;
end;

function TPoissonModel.GetName: String;
begin
  Result := 'Poisson';
end;

procedure TPoissonModel.GetInfo(ModelInfo: TModelInfo);
begin
  inherited GetInfo(ModelInfo);

  ModelInfo.ModelName := 'Poisson' +ModelInfo.ModelName;
  ModelInfo.FullName  := 'Poisson model'+ModelInfo.FullName;
end;



/////////////////////////////////

constructor TProteinGTRModel.Create(gamma: extended; inv: boolean; ncat: integer);
var
  i,j: integer;
  s: extended;
begin
  inherited Create(gamma, inv, true, ncat);

  //FUseFreq := true;

  GetMem(Lambda, SizeOf(TComplex)*20);
  CreateMatrix(r, 20);
  CreateMatrix(Q, 20);

  FFreq[0]  := 0.0866279;
  FFreq[1]  := 0.0439720;
  FFreq[2]  := 0.0390894;
  FFreq[3]  := 0.0570451;
  FFreq[4]  := 0.0193078;
  FFreq[5]  := 0.0367281;
  FFreq[6]  := 0.0580589;
  FFreq[7]  := 0.0832518;
  FFreq[8]  := 0.0244313;
  FFreq[9]  := 0.0484660;
  FFreq[10] := 0.0862090;
  FFreq[11] := 0.0620286;
  FFreq[12] := 0.0195027;
  FFreq[13] := 0.0384319;
  FFreq[14] := 0.0457631;
  FFreq[15] := 0.0695179;
  FFreq[16] := 0.0610127;
  FFreq[17] := 0.0143859;
  FFreq[18] := 0.0352742;
  FFreq[19] := 0.0708956;

  s := 0;
  for i := 0 to 19 do
    for j := 0 to 19 do
      if WAGMatrix[i,j] > s then
        s := WAGMatrix[i,j];

  for i := 0 to 19 do
    for j := 0 to 19 do
      r[i,j] := WAGMatrix[i,j]/s;

  s := 0;
  for i := 1 to 19 do
    for j := 0 to i-1 do
      s := s +2*FFreq[i]*FFreq[j]*r[i,j];

  ComputeEigenVector;
end;

destructor TProteinGTRModel.Destroy;
begin
  FreeMemAndNil(Lambda);
  DestroyMatrix(r, 20);
  DestroyMatrix(Q, 20);

  inherited;
end;

procedure TProteinGTRModel.ComputeEigenVector;
var
  i,j: integer;
  s: extended;
begin

  s := 0;
  for i := 1 to 19 do
    for j := 0 to i-1 do
      s := s +2*r[i,j]*FFreq[i]*FFreq[j];

  for i := 1 to 19 do
    for j := 0 to i-1 do
      Q[i,j] := FFreq[j]*r[i,j]/s;
  for i := 1 to 19 do
    for j := 0 to i-1 do
      Q[j,i] := FFreq[i]*r[i,j]/s;

  for i := 0 to 19 do
  begin
    Q[i,i] := 0;
    for j := 0 to 19 do
      if j <> i then
        Q[i,i] := Q[i,i] -Q[i,j];
  end;

  EigenVect(Q, 0, 19, Lambda, A);

  for i := 0 to 19 do
    D[i] := Lambda[i].X;

  InvMat(A, 0, 19, Ai);

end;

procedure TProteinGTRModel.ResetProb(tree: TMLTree);
var
  i,j: integer;
begin
  for i := 0 to  2*tree.NoOfSeqs-2 do
  begin
    if tree.Node[i] = tree.Root then
      continue;
    for j := 0 to NoOfRates-1 do
      ComputeProb(tree.Node[i].Prob[j], tree.Node[i].blen*FRate[j]/(1-FInvar));
  end;
end;

procedure TProteinGTRModel.OptimizeParameters(tree: TMLTree; aCheckCancel: TCheckCancelFunc = nil);
var
  L0,dL0,dL1,dL2,dL,ddL,r0,dr: extended;
  iter,i,j: integer;
begin
  for i := 1 to 19 do
    for j := 0 to i-1 do
      r[j,i] := r[i,j]/1000;

  iter := 0;
  repeat
    L0  := tree.LogLikelihood;

    for i := 1 to 19 do
      for j := 0 to i-1 do
      begin
        if (i = FixedI) and (j = FixedJ) then
          continue;

        dL0 := tree.LogLikelihood;

        r0 := r[i,j];

        r[i,j] := r[i,j] +r[j,i];
        ComputeEigenVector;
        ResetProb(tree);
        dL1 := tree.ResetL1;

        r[i,j] := r[i,j] -2*r[j,i];
        ComputeEigenVector;
        ResetProb(tree);
        dL2 := tree.ResetL1;

        r[i,j] := r[i,j] +r[j,i];

        dL  := (dL1-dL2)/r[j,i]/2;
        ddL := ((dL1-2*dL0+dL2))/r[j,i]/r[j,i];

        if abs(ddL) > 0 then
          dr := dL/ddL
        else
          dr := 0;
        if dr > r[i,j] then
          dr := r[i,j];
        r[i,j] := r[i,j] -dr;

        if r[i,j] < 0.000000000001 then
          r[i,j] := 0.000000000001;

        ComputeEigenVector;
        ResetProb(tree);

        tree.LogLikelihood := tree.ResetL1;
        if tree.LogLikelihood < dL0 then
        begin
          r[i,j] := r0;
          ComputeEigenVector;
          ResetProb(tree);
          tree.LogLikelihood := tree.ResetL1;
        end
        else if (abs(dr) > 0) and (abs(dr)/2 < r[j,i]) then
          r[j,i] := abs(dr)/2;
    end;
    inc(iter);
    if Assigned(aCheckCancel) then
      aCheckCancel(Round(iter/MaxIter*100), 'Optimizing parameters');
  until (abs(tree.LogLikelihood-L0) < deltaL) or (iter = MaxIter);
  if Assigned(aCheckCancel) then
    aCheckCancel(100, 'Optimizing parameters');
  inherited;
end;

procedure TProteinGTRModel.GetMatrix(M: PMatrixOfExtended);
var
  i,j: integer;
  s: extended;
begin
  s := 0;
  for i := 1 to 19 do
    for j := 0 to i-1 do
      s := s +2*r[i,j]*FFreq[i]*FFreq[j];

  for i := 1 to 19 do
    for j := 0 to i-1 do
      M[i,j] := r[i,j]*FFreq[j]/s;
  for i := 1 to 19 do
    for j := 0 to i-1 do
      M[j,i] := r[i,j]*FFreq[i]/s;

  for i := 0 to 19 do
  begin
    M[i,i] := 0;
    for j := 0 to 19 do
      if j <> i then
        M[i,i] := M[i,i] -M[i,j];
  end;
end;

function TProteinGTRModel.GetName: String;
begin
  Result := 'Protein GTR';
end;

end.
