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

unit mmlcalculatedvalues;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, mextendedlist, MLTree, MegaConsts;

const
  TARGET_IS_RATES = 0;
  TARGET_IS_FREQS = 1;

type

  { TMLCalculatedValues }

  TMLCalculatedValues = class(TObject)
  private
    FNumSiteConfigs: Integer;
    protected
      FIsProteinData: Boolean;
      FLogLikelihoods: TExtendedList;
      FInitialLogLikelihoods: TExtendedList;
      FAics: TExtendedList;
      FBics: TExtendedList;
      FGammas: TExtendedList;
      FInvars: TExtendedList;
      FSVRs  : TExtendedList;
      FRam: TExtendedList;
      FMatrices: T3DDoubleArray;
      FRate: T2dArrayOfExtended;
      FFreq: T2dArrayOfExtended;
    public
      constructor Create(isProteinData: Boolean);
      destructor Destroy; override;
      procedure AddModelInfo(aInfo: TModelInfo; initialLogLikelihood: Extended);
      function TryGetMeanAndVarianceForMatrices(var transitionStrings: TStringList; var aMean: ArrayOfExtended; var aVariance: ArrayOfExtended): Boolean;
      function TryGetMeanAndVarianceForArrays(const target: Integer; var baseStrings: TStringList; var aMean: ArrayOfExtended; var aVariance: ArrayOfExtended): Boolean;
      class function ExportModelInfoToCsvFile(isProteinData: Boolean; aInfo: TModelInfo; filename: String; initialLogLikelihood: Extended): Boolean; static;
      property LogLikelihoods: TExtendedList read FLogLikelihoods;
      property InitialLogLikelihoods: TExtendedList read FInitialLogLikelihoods;
      property Aics: TExtendedList read FAics;
      property Bics: TExtendedList read FBics;
      property Gammas: TExtendedList read FGammas;
      property Invars: TExtendedList read FInvars;
      property SVRs  : TExtendedList read FSVRs;
      property RAMs : TExtendedList read FRam;
      property NumSiteConfigs: Integer read FNumSiteConfigs write FNumSiteConfigs;
  end;

  { TMLCalculatedValueList }

  TMLCalculatedValueList = class(TList)
    private
      FIsGamma: Boolean;
      FIsInvar: Boolean;
      FIsProteinData: Boolean;
      function GetItem(Index: Integer): TMLCalculatedValues;
      function AppendBaseTransitionCsvStrings(var transitionStrings: TStringList; const transitionMeans: ArrayOfExtended; const transitionVariances: ArrayOfExtended): Boolean;
    protected
      function ExportSubsampleValuesToCsvFile(filename: String; numSamples: Integer = -1): Boolean;
      function ExportFullSampleValuesToCsvFile(filename: String): Boolean;
    public
      constructor Create(numElements: Integer; isProteinData, isGamma, isInvar: Boolean);
      destructor Destroy; override;
      procedure Clear; override;
      function ExportValuesToCsvFile(filename: String; numSamples: Integer = -1): Boolean;
      property Items[Index: Integer]: TMLCalculatedValues read GetItem; default;
      property IsProteinData: Boolean read FIsProteinData;
      property IsGamma: Boolean read FIsGamma;
      property IsInvar: Boolean read FIsInvar;
  end;

implementation

uses
  MegaUtils, math
  {$IFNDEF VISUAL_BUILD}
    ,MegaUtils_NV, syncobjs, mmemutils
  {$ENDIF}
;

{ TMLCalculatedValues }

constructor TMLCalculatedValues.Create(isProteinData: Boolean);
begin
  FIsProteinData := isProteinData;
  FLogLikelihoods := TExtendedList.Create;
  FInitialLogLikelihoods := TExtendedList.Create;
  FAics := TExtendedList.Create;
  FBics := TExtendedList.Create;
  FGammas := TExtendedList.Create;
  FInvars := TExtendedList.Create;
  FSVRs := TExtendedList.Create;
  FRam := TExtendedList.Create;
  SetLength(FMatrices, 0);
  SetLength(FRate, 0);
  SetLength(FFreq, 0);
  FNumSiteConfigs := -1;
end;

destructor TMLCalculatedValues.Destroy;
begin
  if Assigned(FLogLikelihoods) then
    FLogLikelihoods.Free;
  if Assigned(FInitialLogLikelihoods) then
    FInitialLogLikelihoods.Free;
  if Assigned(FAics) then
    FAics.Free;
  if Assigned(FBics) then
    FBics.Free;
  if Assigned(FGammas) then
    FGammas.Free;
  if Assigned(FInvars) then
    FInvars.Free;
  if Assigned(FSVRs) then
    FSVRs.Free;
  if Assigned(FRam) then
    FRam.Free;
  SetLength(FMatrices, 0);
  SetLength(FRate, 0);
  SetLength(FFreq, 0);
  inherited Destroy;
end;

procedure TMLCalculatedValues.AddModelInfo(aInfo: TModelInfo; initialLogLikelihood: Extended);
var
  currentMatrix: Integer;
  i,j: Integer;
  matrixDimension: Integer;
begin
  FLogLikelihoods.Add(aInfo.LogL);
  FInitialLogLikelihoods.Add(initialLogLikelihood);
  FAics.Add(aInfo.AICc);
  FBics.Add(aInfo.BIC);
  FGammas.Add(aInfo.Gamma);
  FInvars.Add(aInfo.Invar);
  FSVRs.Add(aInfo.SVR);
  {$IFDEF MSWINDOWS}
  FRam.Add(GetCurrentProcessMemoryMB/(1024*1024));
  {$ELSE}
    {$IFNDEF VISUAL_BUILD}
    try
      PeakMemoryCS.Acquire;
      FRam.Add((PeakMemoryUsedByMega + GetHeapStatus.TotalAllocated)/(1024*1024));
    finally
      PeakMemoryCS.Release;
    end;
    {$ENDIF}
  {$ENDIF}

  currentMatrix := Length(FMatrices);
  SetLength(FMatrices, currentMatrix + 1);
  if FIsProteinData then
    matrixDimension := 20
  else
    matrixDimension := 4;
  SetLength(FMatrices[currentMatrix], matrixDimension);
  for i := 0 to Length(FMatrices[currentMatrix]) - 1 do
    SetLength(FMatrices[currentMatrix][i], matrixDimension);
  for i := 0 to Length(FMatrices[currentMatrix]) - 1 do
    for j := 0 to Length(FMatrices[currentMatrix][i]) - 1 do
      FMatrices[currentMatrix][i][j] := aInfo.Matrix[i][j];
  SetLength(FRate, currentMatrix + 1);
  SetLength(FFreq, currentMatrix + 1);
  SetLength(FRate[currentMatrix], matrixDimension);
  SetLength(FFreq[currentMatrix], matrixDimension);
  for i := 0 to matrixDimension - 1 do
  begin
    FRate[currentMatrix][i] := aInfo.Rate[i];
    FFreq[currentMatrix][i] := aInfo.Freq[i];
  end;
end;

function TMLCalculatedValues.TryGetMeanAndVarianceForMatrices(var transitionStrings: TStringList; var aMean: ArrayOfExtended; var aVariance: ArrayOfExtended): Boolean;
var
  col,row, matrix: Integer;
  matrixDimension: Integer;
  temp: ArrayOfExtended;
  aAvg: Extended = 0;
  aStdDev: Extended = 0;
  index: Integer = 0;
begin
  Result := False;
  try
    transitionStrings.Clear;
    matrixDimension := Length(FMatrices[0]);
    SetLength(temp, Length(FMatrices));
    for col := 0 to matrixDimension - 1 do
      for row := 0 to matrixDimension - 1 do
      begin
        if col <> row then
        begin
          transitionStrings.Add('r(' + MapIntegerToBaseString(col, FIsProteinData) + '->' + MapIntegerToBaseString(row, FIsProteinData) + ')');
          for matrix := 0 to Length(temp) - 1 do
            temp[matrix] := FMatrices[matrix][col][row];
          meanandstddev(temp, aAvg, aStdDev);
          aMean[index] := aAvg;
          aVariance[index] := aStdDev*aStdDev;
          inc(index);
        end;
      end;
    Result := True;
  except
    on E:Exception do
    begin
      Result := False;
      {$IFNDEF VISUAL_BUILD}
      warn_nv('Failed to get mean and variance for transition matrices: ' + E.Message);
      {$ENDIF}
    end;
  end;
end;

function TMLCalculatedValues.TryGetMeanAndVarianceForArrays(const target: Integer; var baseStrings: TStringList; var aMean: ArrayOfExtended; var aVariance: ArrayOfExtended): Boolean;
var
  i, j: Integer;
  m: Extended = 0;
  sd: Extended = 0;
  targetArray: T2dArrayOfExtended;
  prefix: String;
  temp: ArrayOfExtended;
begin
  Result := False;
  case target of
    TARGET_IS_RATES: begin
      targetArray := FRate;
      prefix := 'Rate';
    end;
    TARGET_IS_FREQS:
      begin
        targetArray := FFreq;
        prefix := 'Freq';
      end
    else
      raise Exception.Create('APPLICATION ERROR: invalid target array');
  end;

  try
    SetLength(temp, Length(targetArray));
    baseStrings.Clear;
    for i := 0 to Length(aMean) - 1 do
    begin
      baseStrings.Add(Format('%s(%s)', [prefix, MapIntegerToBaseString(i, FIsProteinData)]));
      for j := 0 to Length(temp) - 1 do
        temp[j] := targetArray[j][i];
      meanandstddev(temp, m, sd);
      aMean[i] := m;
      aVariance[i] := sd*sd;
    end;
    Result := True;
  except
    on E:Exception do
    begin
      Result := False;
      {$IFNDEF VISUAL_BUILD}
      warn_nv('failed to get mean and variance: ' + E.Message);
      {$ENDIF}
    end;
  end;
end;

class function TMLCalculatedValues.ExportModelInfoToCsvFile(isProteinData: Boolean; aInfo: TModelInfo; filename: String; initialLogLikelihood: Extended): Boolean;
var
  aFile: TextFile;
  i, j: Integer;
  arrayLength: Integer;
begin
  Result := True;
  if isProteinData then
    arrayLength := 20
  else
    arrayLength := 4;
  try
    try
      AssignFile(aFile, filename);
      Rewrite(aFile);
      WriteLn(aFile, 'value,result');
      WriteLn(aFile, Format('initial_ln,%.3e', [initialLogLikelihood]));
      WriteLn(aFile, Format('final_ln,%.3e', [aInfo.LogL]));
      WriteLn(aFile, Format('gamma,%.3e', [aInfo.Gamma]));
      WriteLn(aFile, Format('invar,%.3e', [aInfo.Invar]));
      WriteLn(aFile, Format('AICc,%.3e', [aInfo.AICc]));
      WriteLn(aFile, Format('BIC,%.3e', [aInfo.BIC]));
      WriteLn(aFile, Format('SVR,%.3e', [aInfo.SVR]));
      {$IFNDEF VISUAL_BUILD}{$IFDEF MSWINDOWS}
      WriteLn(aFile, Format('RAM (MB),%.3e', [GetCurrentProcessMemoryMB/(1024*1024)]));
      {$ELSE}
      try
        PeakMemoryCS.Acquire;
        WriteLn(aFile, Format('RAM (MB),%.3e', [(PeakMemoryUsedByMega + GetHeapStatus.TotalAllocated)/(1024*1024)]));
      finally
        PeakMemoryCS.Release;
      end;
      {$ENDIF}{$ENDIF}
      for i := 0 to arrayLength - 1 do
        WriteLn(aFile, Format('Rate(%s),%.3e', [MapIntegerToBaseString(i, IsProteinData), aInfo.Rate[i]]));
      for i := 0 to arrayLength - 1 do
        WriteLn(aFile, Format('Freq(%s),%.3e', [MapIntegerToBaseString(i, IsProteinData), aInfo.Freq[i]]));

      for i := 0 to arrayLength - 1 do
      begin
        for j := 0 to arrayLength - 1 do
        begin
          if i <> j then // we don't transform from X to X.
            WriteLn(aFile, Format('(%s -> %s),%.3e', [MapIntegerToBaseString(i, IsProteinData), MapIntegerToBaseString(j, IsProteinData), aInfo.matrix[i][j]]));
        end;
      end;
    except
      on E:Exception do
      begin
        Result := False;
        {$IFNDEF VISUAL_BUILD}
        warn_NV('Failed to export model info to csv file');
        {$ENDIF}
      end;
    end;
  finally
    CloseFile(aFile);
  end;
  if Result then
    Result := FileExists(filename);
end;

{ TMLCalculatedValueList }

function TMLCalculatedValueList.GetItem(Index: Integer): TMLCalculatedValues;
var
  p: Pointer;
begin
  Result := nil;
  p := inherited Items[Index];
  if Assigned(p) then
    Result := TMLCalculatedValues(p);
end;

function TMLCalculatedValueList.AppendBaseTransitionCsvStrings(var transitionStrings: TStringList; const transitionMeans: ArrayOfExtended; const transitionVariances: ArrayOfExtended): Boolean;
var
  i: Integer;
begin
  Assert(transitionStrings.Count = Length(transitionMeans), Format('unmatched dimensions: %d vs %d', [transitionStrings.Count, Length(transitionMeans)]));
  Result := False;
  try
    for i := 0 to transitionStrings.Count - 1 do
      transitionStrings[i] := transitionStrings[i] + Format(',%.3e,%.3e', [transitionMeans[i], transitionVariances[i]]);
  except
    on E:Exception do
    begin
      Result := False;
      {$IFNDEF VISUAL_BUILD}
      warn_nv('failed to build csv strings for base transitions: ' + E.Message);
      {$ENDIF}
    end;
  end;
end;

function TMLCalculatedValueList.ExportSubsampleValuesToCsvFile(filename: String; numSamples: Integer = -1): Boolean;
var
  aFile: TextFile;
  i: Integer;
  vals: TMLCalculatedValues = nil;
  aMean: Extended = 0;
  aVariance: Extended = 0;
  tempStrings: TStringList = nil;
  csvStrings: TStringList = nil;
  tempMeans: ArrayOfExtended;
  tempVariances: ArrayOfExtended;
  limit: Integer;
begin
  Result := False;
  if numSamples <> -1 then
    limit := numSamples
  else
    limit := Count;
  try
    try
      AssignFile(aFile, filename);
      Rewrite(aFile);
      Write(aFile, 'value');
      for i := 0 to limit - 1 do
        Write(aFile, Format(',sample_%d_mean,sample_%d_variance', [i + 1, i + 1]));
      WriteLn(aFile);

      Write(aFile, 'num_configs');
      for i := 0 to limit - 1 do
      begin
        vals := GetItem(i);
        Write(aFile, Format(',%d,n/a', [vals.NumSiteConfigs]));
      end;
      WriteLn(aFile);

      Write(aFile, 'initial_ln');
      for i := 0 to limit - 1 do
      begin
        vals := GetItem(i);
        if vals.InitialLogLikelihoods.CalcMeanAndVariance(aMean, aVariance) then
          Write(aFile, Format(',%.3e,%.3e', [aMean, aVariance]))
        else
          Write(aFile, ',n/a,n/a');
      end;
      WriteLn(aFile);

      Write(aFile, 'final_ln');
      for i := 0 to limit - 1 do
      begin
        vals := GetItem(i);
        if vals.LogLikelihoods.CalcMeanAndVariance(aMean, aVariance) then
          Write(aFile, Format(',%.3e,%.3e', [aMean, aVariance]))
        else
          Write(aFile, ',n/a,n/a');
      end;
      WriteLn(aFile);

      if FIsGamma then
      begin
        Write(aFile, 'gamma');
        for i := 0 to limit - 1 do
        begin
          vals := GetItem(i);
          if vals.Gammas.CalcMeanAndVariance(aMean, aVariance) then
            Write(aFile, Format(',%.3e,%.3e', [aMean, aVariance]))
          else
            Write(aFile, ',n/a,n/a');
        end;
        WriteLn(aFile);
      end;

      if FIsInvar then
      begin
        Write(aFile, 'invar');
        for i := 0 to limit - 1 do
        begin
          vals := GetItem(i);
          if vals.Invars.CalcMeanAndVariance(aMean, aVariance) then
            Write(aFile, Format(',%.3e,%.3e', [aMean, aVariance]))
          else
            Write(aFile, ',n/a,n/a');
        end;
        WriteLn(aFile);
      end;

      Write(aFile, 'AICc');
      for i := 0 to limit - 1 do
      begin
        vals := GetItem(i);
        if vals.Aics.CalcMeanAndVariance(aMean, aVariance) then
          Write(aFile, Format(',%.3e,%.3e', [aMean, aVariance]))
        else
          Write(aFile, ',n/a,n/a');
      end;
      WriteLn(aFile);

      Write(aFile, 'BIC');
      for i := 0 to limit - 1 do
      begin
        vals := GetItem(i);
        if vals.Bics.CalcMeanAndVariance(aMean, aVariance) then
          Write(aFile, Format(',%.3e,%.3e', [aMean, aVariance]))
        else
          Write(aFile, ',n/a,n/a');
      end;
      WriteLn(aFile);

      Write(aFile, 'SVR');
      for i := 0 to limit - 1 do
      begin
        vals := GetItem(i);
        if vals.SVRs.CalcMeanAndVariance(aMean, aVariance) then
          Write(aFile, Format(',%.3e,%.3e', [aMean, aVariance]))
        else
          Write(aFile, ',n/a,n/a');
      end;
      WriteLn(aFile);

      Write(aFile, 'RAM');
      for i := 0 to limit - 1 do
      begin
        vals := GetItem(i);
        if vals.RAMs.CalcMeanAndVariance(aMean, aVariance) then
          Write(aFile, Format(',%.3e,%.3e', [aMean, aVariance]))
        else
          Write(aFile, ',n/a,n/a');
      end;
      WriteLn(aFile);

      tempStrings := TStringList.Create;
      csvStrings := TStringList.Create;
      if FIsProteinData then
      begin
        SetLength(tempMeans, 20);
        SetLength(tempVariances, 20);
      end
      else
      begin
        SetLength(tempMeans, 4);
        SetLength(tempVariances, 4);
      end;

      vals := GetItem(0);
      if not vals.TryGetMeanAndVarianceForArrays(TARGET_IS_RATES, csvStrings, tempMeans, tempVariances) then
        WriteLn(aFile, 'rates,n/a,n/a')
      else
      begin
        for i := 0 to limit - 1 do
        begin
          vals := GetItem(i);
          if vals.TryGetMeanAndVarianceForArrays(TARGET_IS_RATES, tempStrings, tempMeans, tempVariances) then
            AppendBaseTransitionCsvStrings(csvStrings, tempMeans, tempVariances)
          else
            WriteLn('rates,n/a,n/a');
        end;
        for i := 0 to csvStrings.Count - 1 do
          WriteLn(aFile, csvStrings[i]);
      end;

      csvStrings.Clear;
      tempStrings.Clear;
      vals := GetItem(0);
      if not vals.TryGetMeanAndVarianceForArrays(TARGET_IS_FREQS, csvStrings, tempMeans, tempVariances) then
        WriteLn(aFile, 'freqs,n/a,n/a')
      else
      begin
        for i := 0 to limit - 1 do
        begin
          vals := GetItem(i);
          if vals.TryGetMeanAndVarianceForArrays(TARGET_IS_FREQS, tempStrings, tempMeans, tempVariances) then
            AppendBaseTransitionCsvStrings(csvStrings, tempMeans, tempVariances)
          else
            WriteLn('freqs,n/a,n/a');
        end;
        for i := 0 to csvStrings.Count - 1 do
          WriteLn(aFile, csvStrings[i]);
      end;

      if FIsProteinData then
      begin
        SetLength(tempMeans, 20*19);
        SetLength(tempVariances, 20*19);
      end
      else
      begin
        SetLength(tempMeans, 4*3);
        SetLength(tempVariances, 4*3);
      end;
      tempStrings.Clear;
      csvStrings.Clear;
      vals := GetItem(0);
      if not vals.TryGetMeanAndVarianceForMatrices(csvStrings, tempMeans, tempVariances) then
        WriteLn(aFile, 'transitions,n/a,n/a')
      else
      begin
        for i := 0 to limit - 1 do
        begin
          vals := GetItem(i);
          if vals.TryGetMeanAndVarianceForMatrices(tempStrings, tempMeans, tempVariances) then
            AppendBaseTransitionCsvStrings(csvStrings, tempMeans, tempVariances)
          else
            WriteLn(aFile, 'transitions,n/a,n/a');
        end;
        for i := 0 to csvStrings.Count - 1 do
          WriteLn(aFile, csvStrings[i]);
      end;
      Result := FileExists(filename);
    except
      on E:Exception do
      begin
        Result := False;
        {$IFNDEF VISUAL_BUILD}
        warn_NV('failed to export ML calculated values: ' + E.Message);
        {$ENDIF}
      end;
    end;
  finally
    CloseFile(aFile);
    if Assigned(tempStrings) then
      tempStrings.Free;
    if Assigned(csvStrings) then
      csvStrings.Free;
  end;
end;

function TMLCalculatedValueList.ExportFullSampleValuesToCsvFile(filename: String): Boolean;
var
  aFile: TextFile;
  i: Integer;
  vals: TMLCalculatedValues = nil;
  aMean: Extended = 0;
  aVariance: Extended = 0;
  tempStrings: TStringList = nil;
  tempMeans, tempVariances: ArrayOfExtended;
begin
  Result := False;
  try
    try
      AssignFile(aFile, filename);
      Rewrite(aFile);
      WriteLn(aFile, 'value,mean,variance');
      vals := GetItem(0);

      WriteLn(Format('num_configs,%d,n/a', [vals.NumSiteConfigs]));

      if vals.InitialLogLikelihoods.CalcMeanAndVariance(aMean, aVariance) then
        WriteLn(aFile, Format('initial_ln,%.3e,%.3e', [aMean, aVariance]))
      else
        WriteLn(aFile, 'initial_ln,n/a,n/a');

      if vals.LogLikelihoods.CalcMeanAndVariance(aMean, aVariance) then
        WriteLn(aFile, Format('final_ln,%.3e,%.3e', [aMean, aVariance]))
      else
        WriteLn(aFile, 'final_ln,n/a,n/a');

      if FIsGamma then
      begin
        if vals.Gammas.CalcMeanAndVariance(aMean, aVariance) then
          WriteLn(aFile, Format('gamma,%.3e,%.3e', [aMean, aVariance]))
        else
          WriteLn(aFile, 'gamma,n/a,n/a');
      end;

      if FIsInvar then
      begin
        if vals.Invars.CalcMeanAndVariance(aMean, aVariance) then
          WriteLn(aFile, Format('invar,%.3e,%.3e', [aMean, aVariance]))
        else
          WriteLn(aFile, 'invar,n/a,n/a');
      end;

      if vals.Aics.CalcMeanAndVariance(aMean, aVariance) then
        WriteLn(aFile, Format('AICc,%.3e,%.3e', [aMean, aVariance]))
      else
        WriteLn(aFile, 'AICc,n/a,n/a');

      if vals.Bics.CalcMeanAndVariance(aMean, aVariance) then
        WriteLn(aFile, Format('BIC,%.3e,%.3e', [aMean, aVariance]))
      else
        WriteLn(aFile, 'BIC,n/a,n/a');

      if vals.SVRs.CalcMeanAndVariance(aMean, aVariance) then
        WriteLn(aFile, Format('SVR,%.3e,%.3e', [aMean, aVariance]))
      else
        WriteLn(aFile, 'SVR,n/a,n/a');

      if vals.RAMs.CalcMeanAndVariance(aMean, aVariance) then
        WriteLn(aFile, Format('RAM,%.3e,%.3e', [aMean, aVariance]))
      else
        WriteLn(aFile, 'RAM,n/a,n/a');

      tempStrings := TStringList.Create;

      if FIsProteinData then
      begin
        SetLength(tempMeans, 20);
        SetLength(tempVariances, 20);
      end
      else
      begin
        SetLength(tempMeans, 4);
        SetLength(tempVariances, 4);
      end;

      if FIsGamma then
      begin
        if not vals.TryGetMeanAndVarianceForArrays(TARGET_IS_RATES, tempStrings, tempMeans, tempVariances) then
          WriteLn(aFile, 'rates,n/a,n/a')
        else
        begin
          AppendBaseTransitionCsvStrings(tempStrings, tempMeans, tempVariances);
          for i := 0 to tempStrings.Count - 1 do
            WriteLn(aFile, tempStrings[i]);
        end;
      end;

      tempStrings.Clear;
      if not vals.TryGetMeanAndVarianceForArrays(TARGET_IS_FREQS, tempStrings, tempMeans, tempVariances) then
        WriteLn(aFile, 'freqs,n/a,n/a')
      else
      begin
        AppendBaseTransitionCsvStrings(tempStrings, tempMeans, tempVariances);
        for i := 0 to tempStrings.Count - 1 do
          WriteLn(aFile, tempStrings[i]);
      end;

      tempStrings.Clear;
      if FIsProteinData then
      begin
        SetLength(tempMeans, 20*19);
        SetLength(tempVariances, 20*19);
      end
      else
      begin
        SetLength(tempMeans, 4*3);
        SetLength(tempVariances, 4*3);
      end;
      if not vals.TryGetMeanAndVarianceForMatrices(tempStrings, tempMeans, tempVariances) then
        Write(aFile, 'transitions,n/a,n/a')
      else
      begin
        AppendBaseTransitionCsvStrings(tempStrings, tempMeans, tempVariances);
        for i := 0 to tempStrings.Count - 1 do
          WriteLn(aFile, tempStrings[i]);
      end;

      Result := FileExists(filename);
    except
      on E:Exception do
      begin
        Result := False;
        {$IFNDEF VISUAL_BUILD}
        warn_NV('failed to export ML calculated values: ' + E.Message);
        {$ENDIF}
      end;
    end;
  finally
    CloseFile(aFile);
    if Assigned(tempStrings) then
      tempStrings.Free;
  end;
end;

constructor TMLCalculatedValueList.Create(numElements: Integer; isProteinData, isGamma, isInvar: Boolean);
var
  i: Integer;
  v: TMLCalculatedValues = nil;
begin
  inherited Create;
  FIsProteinData := isProteinData;
  FIsGamma := isGamma;
  FIsInvar := isInvar;
  if numElements <= 0 then
    raise Exception.Create('invalid use of TMLCalculatedValueList');
  for i := 1 to numElements do
  begin
    v := TMLCalculatedValues.Create(isProteinData);
    inherited Add(v);
  end;
end;

destructor TMLCalculatedValueList.Destroy;
begin
  inherited Destroy;
end;

procedure TMLCalculatedValueList.Clear;
var
  i: Integer;
  aItem: TMLCalculatedValues = nil;
begin
  if Count > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      aItem := GetItem(i);
      aItem.Free;
    end;
  end;
end;

function TMLCalculatedValueList.ExportValuesToCsvFile(filename: String; numSamples: Integer = -1): Boolean;
begin
  if Count > 1 then
    Result := ExportSubsampleValuesToCsvFile(filename, numSamples)
  else
    Result := ExportFullSampleValuesToCsvFile(filename);
end;

end.

